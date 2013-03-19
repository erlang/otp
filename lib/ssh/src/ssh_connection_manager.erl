%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
%%----------------------------------------------------------------------
%% Purpose: Handles multiplexing to ssh channels and global connection
%% requests e.i. the SSH Connection Protocol (RFC 4254), that provides
%% interactive login sessions, remote execution of commands, forwarded
%% TCP/IP connections, and forwarded X11 connections. Details of the
%% protocol is implemented in ssh_connection.erl
%% ----------------------------------------------------------------------
-module(ssh_connection_manager).

-behaviour(gen_server).

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include("ssh_transport.hrl").

-export([start_link/1]).

-export([info/1, info/2, 
	 renegotiate/1, connection_info/2, channel_info/3,
	 peer_addr/1, send_window/3, recv_window/3, adjust_window/3,
	 close/2, stop/1, send/5,
	 send_eof/2]).

-export([open_channel/6, reply_request/3, request/6, request/7, global_request/4, event/2, event/3, cast/2]).

%% Internal application API and spawn
-export([send_msg/1, ssh_channel_info_handler/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DBG_MESSAGE, true).

-record(state,
	{
	  role,
	  client,
	  starter,
	  connection,       % pid()
	  connection_state, % #connection{} 
	  latest_channel_id = 0, 
	  opts,
	  channel_args,
	  idle_timer_ref, % timerref
	  connected 
	 }).

%%====================================================================
%% Internal application API
%%====================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

open_channel(ConnectionManager,	ChannelType, ChannelSpecificData,
	     InitialWindowSize, MaxPacketSize, Timeout) ->
    case (catch call(ConnectionManager, {open, self(), ChannelType,
					 InitialWindowSize, 
					 MaxPacketSize, ChannelSpecificData},
	      Timeout)) of
	{open, Channel} -> 
	    {ok, Channel}; 
	Error -> 
	    %% TODO: Best way?
	    Error
    end.

request(ConnectionManager, ChannelPid, ChannelId, Type, true, Data, Timeout) ->
    call(ConnectionManager, {request, ChannelPid, ChannelId, Type, Data}, Timeout);
request(ConnectionManager, ChannelPid, ChannelId, Type, false, Data, _) ->
    cast(ConnectionManager, {request, ChannelPid, ChannelId, Type, Data}).

request(ConnectionManager, ChannelId, Type, true, Data, Timeout) ->
    call(ConnectionManager, {request, ChannelId, Type, Data}, Timeout);
request(ConnectionManager, ChannelId, Type, false, Data, _) ->
    cast(ConnectionManager, {request, ChannelId, Type, Data}).

reply_request(ConnectionManager, Status, ChannelId) ->
    cast(ConnectionManager, {reply_request, Status, ChannelId}).

global_request(ConnectionManager, Type, true = Reply, Data) ->
    case call(ConnectionManager, 
	      {global_request, self(), Type, Reply, Data}) of
	{ssh_cm, ConnectionManager, {success, _}} ->
	    ok;
	{ssh_cm, ConnectionManager, {failure, _}} ->
	    error
    end;

global_request(ConnectionManager, Type, false = Reply, Data) ->
    cast(ConnectionManager, {global_request, self(), Type, Reply, Data}).

event(ConnectionManager, BinMsg, ErrorMsg) ->
    call(ConnectionManager, {ssh_msg, self(), BinMsg, ErrorMsg}).
event(ConnectionManager, BinMsg) -> 
    call(ConnectionManager, {ssh_msg, self(), BinMsg}).
info(ConnectionManager) ->
    info(ConnectionManager, {info, all}).

info(ConnectionManager, ChannelProcess) ->
    call(ConnectionManager, {info, ChannelProcess}).

%% TODO: Do we really want this function? Should not 
%% renegotiation be triggered by configurable timer
%% or amount of data sent counter! 
renegotiate(ConnectionManager) ->
    cast(ConnectionManager, renegotiate).
renegotiate_data(ConnectionManager) ->
    cast(ConnectionManager, renegotiate_data).
connection_info(ConnectionManager, Options) ->
    call(ConnectionManager, {connection_info, Options}).

channel_info(ConnectionManager, ChannelId, Options) ->
    call(ConnectionManager, {channel_info, ChannelId, Options}).

%% Replaced by option peer to connection_info/2 keep for now
%% for Backwards compatibility!
peer_addr(ConnectionManager) ->
    call(ConnectionManager, {peer_addr, self()}).

%% Backwards compatibility!
send_window(ConnectionManager, Channel, TimeOut) ->
    call(ConnectionManager, {send_window, Channel}, TimeOut).
%% Backwards compatibility!
recv_window(ConnectionManager, Channel, TimeOut) ->
    call(ConnectionManager, {recv_window, Channel}, TimeOut).

adjust_window(ConnectionManager, Channel, Bytes) ->
    cast(ConnectionManager, {adjust_window, Channel, Bytes}).

close(ConnectionManager, ChannelId) ->
    case call(ConnectionManager, {close, ChannelId}) of
	ok ->
	    ok;
	{error, channel_closed} ->
	    ok
    end.	

stop(ConnectionManager) ->
    case call(ConnectionManager, stop) of
	ok ->
	    ok;
	{error, channel_closed} ->
	    ok
    end.
			
send(ConnectionManager, ChannelId, Type, Data, Timeout) ->
    call(ConnectionManager, {data, ChannelId, Type, Data}, Timeout).

send_eof(ConnectionManager, ChannelId) ->
    call(ConnectionManager, {eof, ChannelId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([server, _Socket, Opts]) ->
    process_flag(trap_exit, true),
    ssh_bits:install_messages(ssh_connection:messages()),
    Cache = ssh_channel:cache_create(),
    {ok, #state{role = server, 
		connection_state = #connection{channel_cache = Cache,
					       channel_id_seed = 0,
					       port_bindings = [],
					       requests = []},
		opts = Opts,
		connected = false}};

init([client, Opts]) ->  
    process_flag(trap_exit, true),
    {links, [Parent]} = process_info(self(), links),
    ssh_bits:install_messages(ssh_connection:messages()),
    Cache = ssh_channel:cache_create(),
    Address =  proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    SocketOpts = proplists:get_value(socket_opts, Opts),
    Options = proplists:get_value(ssh_opts, Opts),
    ChannelPid = proplists:get_value(channel_pid, Opts),
    self() ! 
	{start_connection, client, [Parent, Address, Port, SocketOpts, Options]},
    TimerRef = get_idle_time(Options),
	
    {ok, #state{role = client, 
		client = ChannelPid,
		connection_state = #connection{channel_cache = Cache,
					       channel_id_seed = 0,
					       port_bindings = [],
					       connection_supervisor = Parent,
					       requests = []},
		opts = Opts,
		idle_timer_ref = TimerRef,
		connected = false}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({request, ChannelPid, ChannelId, Type, Data}, From, State0) ->
    {{replies, Replies}, State} = handle_request(ChannelPid, 
						 ChannelId, Type, Data, 
						 true, From, State0),
    %% Sends message to the connection handler process, reply to
    %% channel is sent later when reply arrives from the connection
    %% handler. 
    lists:foreach(fun send_msg/1, Replies),
    SshOpts = proplists:get_value(ssh_opts, State0#state.opts),
    case proplists:get_value(idle_time, SshOpts) of
	infinity ->
	    ok;
	_IdleTime ->
	    erlang:send_after(5000, self(), {check_cache, [], []})
    end,
    {noreply, State};

handle_call({request, ChannelId, Type, Data}, From, State0) ->
    {{replies, Replies}, State} = handle_request(ChannelId, Type, Data, 
						 true, From, State0),
    %% Sends message to the connection handler process, reply to
    %% channel is sent later when reply arrives from the connection
    %% handler.
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

%% Message from ssh_connection_handler
handle_call({ssh_msg, Pid, Msg}, From, 
	    #state{connection_state = Connection0,
		   role = Role, opts = Opts, connected = IsConnected,
		   client = ClientPid}
	    = State) ->
    
    %% To avoid that not all data sent by the other side is processes before
    %% possible crash in ssh_connection_handler takes down the connection.
    gen_server:reply(From, ok),
    ConnectionMsg = decode_ssh_msg(Msg),
    try ssh_connection:handle_msg(ConnectionMsg, Connection0, Pid, Role) of
	{{replies, Replies}, Connection} ->
	    lists:foreach(fun send_msg/1, Replies),
	    {noreply, State#state{connection_state = Connection}};
	{noreply, Connection} ->
	    {noreply, State#state{connection_state = Connection}};
	{disconnect, {_, Reason}, {{replies, Replies}, Connection}} 
	when Role == client andalso (not IsConnected) ->
	    lists:foreach(fun send_msg/1, Replies),
	    ClientPid ! {self(), not_connected, Reason},
	    {stop, {shutdown, normal}, State#state{connection = Connection}};
	{disconnect, Reason, {{replies, Replies}, Connection}} ->
	    lists:foreach(fun send_msg/1, Replies),
	    SSHOpts = proplists:get_value(ssh_opts, Opts),
	    disconnect_fun(Reason, SSHOpts),
	    {stop, {shutdown, normal}, State#state{connection_state = Connection}}
	catch
	    _:Error ->
		{disconnect, Reason, {{replies, Replies}, Connection}} =
		    ssh_connection:handle_msg(
		      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
					  description = "Internal error",
					  language = "en"}, Connection0, undefined,
		      Role),
		lists:foreach(fun send_msg/1, Replies),
		SSHOpts = proplists:get_value(ssh_opts, Opts),
		disconnect_fun(Reason, SSHOpts),
		{stop, {shutdown, Error}, State#state{connection_state = Connection}}
	end;
handle_call({ssh_msg, Pid, Msg, ErrorMsg}, From,
	    #state{connection_state = Connection0,
		   role = Role, opts = Opts, connected = IsConnected,
		   client = ClientPid}
	    = State) ->

    %% To avoid that not all data sent by the other side is processes before
    %% possible crash in ssh_connection_handler takes down the connection.
    gen_server:reply(From, ok),
    ConnectionMsg = decode_ssh_msg(Msg),
    try ssh_connection:handle_msg(ConnectionMsg, Connection0, Pid, Role) of
	{{replies, Replies}, Connection} ->
	    lists:foreach(fun send_msg/1, Replies),
	    {noreply, State#state{connection_state = Connection}};
	{noreply, Connection} ->
	    {noreply, State#state{connection_state = Connection}};
	{disconnect, {_, Reason}, {{replies, Replies}, Connection}}
	when Role == client andalso (not IsConnected) ->
	    lists:foreach(fun send_msg/1, Replies),
	    ClientPid ! {self(), not_connected, {Reason, ErrorMsg}},
	    {stop, {shutdown, normal}, State#state{connection = Connection}};
	{disconnect, Reason, {{replies, Replies}, Connection}} ->
	    lists:foreach(fun send_msg/1, Replies),
	    SSHOpts = proplists:get_value(ssh_opts, Opts),
	    disconnect_fun(Reason, SSHOpts),
	    {stop, {shutdown, normal}, State#state{connection_state = Connection}}
	catch
	    _:Error ->
		{disconnect, Reason, {{replies, Replies}, Connection}} =
		    ssh_connection:handle_msg(
		      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
					  description = "Internal error",
					  language = "en"}, Connection0, undefined,
		      Role),
		lists:foreach(fun send_msg/1, Replies),
		SSHOpts = proplists:get_value(ssh_opts, Opts),
		disconnect_fun(Reason, SSHOpts),
		{stop, {shutdown, Error}, State#state{connection_state = Connection}}
	end;
handle_call({global_request, Pid, _, _, _} = Request, From, 
	    #state{connection_state = 
		   #connection{channel_cache = Cache}} = State0) ->
    State1 = handle_global_request(Request, State0),
    Channel = ssh_channel:cache_find(Pid, Cache),
    State = add_request(true, Channel#channel.local_id, From, State1),
    {noreply, State};

handle_call({data, ChannelId, Type, Data}, From, 
	    #state{connection_state = #connection{channel_cache = _Cache} 
		   = Connection0, 
		   connection = ConnectionPid} = State) ->
    channel_data(ChannelId, Type, Data, Connection0, ConnectionPid, From,
		 State);

handle_call({eof, ChannelId}, _From,
			#state{connection = Pid, connection_state =
					   #connection{channel_cache = Cache}} = State) ->
	case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{remote_id = Id, sent_close = false} ->
			send_msg({connection_reply, Pid,
					  ssh_connection:channel_eof_msg(Id)}),
			{reply, ok, State};
		_ ->
			{reply, {error,closed}, State}
	end;

handle_call({connection_info, Options}, From, 
	    #state{connection = Connection} = State) ->
    ssh_connection_handler:connection_info(Connection, From, Options),
    %% Reply will be sent by the connection handler by calling
    %% ssh_connection_handler:send_msg/1.
    {noreply, State};

handle_call({channel_info, ChannelId, Options}, From, 
	    #state{connection_state = #connection{channel_cache = Cache}} = State) ->
 
    case ssh_channel:cache_lookup(Cache, ChannelId) of			 
       #channel{} = Channel ->
	    spawn(?MODULE, ssh_channel_info_handler, [Options, Channel, From]),
	    {noreply, State};
	undefined -> 
	    {reply, []}
    end;
   
handle_call({info, ChannelPid}, _From, 
	    #state{connection_state = 
		   #connection{channel_cache = Cache}} = State) ->
    Result = ssh_channel:cache_foldl(
	       fun(Channel, Acc) when ChannelPid == all; 
		  Channel#channel.user == ChannelPid ->
		       [Channel | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], Cache),
    {reply, {ok, Result}, State};

handle_call({open, ChannelPid, Type, InitialWindowSize, MaxPacketSize, Data}, 
	    From, #state{connection = Pid,
			 connection_state = 
			     #connection{channel_cache = Cache}} = State0) ->
    erlang:monitor(process, ChannelPid),
    {ChannelId, State1}  = new_channel_id(State0),
    Msg = ssh_connection:channel_open_msg(Type, ChannelId, 
					  InitialWindowSize, 
					  MaxPacketSize, Data),
    send_msg({connection_reply, Pid, Msg}),
    Channel = #channel{type = Type,
		       sys = "none",
		       user = ChannelPid,
		       local_id = ChannelId,
		       recv_window_size = InitialWindowSize,
		       recv_packet_size = MaxPacketSize},
    ssh_channel:cache_update(Cache, Channel),
    State = add_request(true, ChannelId, From, State1),
    {noreply, remove_timer_ref(State)};

handle_call({send_window, ChannelId}, _From, 
	    #state{connection_state = 
		   #connection{channel_cache = Cache}} = State) ->
     Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{send_window_size = WinSize,
			 send_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined -> 
		    {error, einval}
	    end,
    {reply, Reply, State};

handle_call({recv_window, ChannelId}, _From, 
	    #state{connection_state = #connection{channel_cache = Cache}} 
	     = State) ->
  
    Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{recv_window_size = WinSize,
			 recv_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined -> 
		    {error, einval}
	    end,
    {reply, Reply, State};

%% Replaced by option peer to connection_info/2 keep for now
%% for Backwards compatibility!
handle_call({peer_addr, _ChannelId}, _From, 
	    #state{connection = Pid} = State) ->
    Reply = ssh_connection_handler:peer_address(Pid),
    {reply, Reply, State};

handle_call(opts, _, #state{opts = Opts} = State) ->
    {reply, Opts, State};

handle_call({close, ChannelId}, _,
	    #state{connection = Pid, connection_state = 
		   #connection{channel_cache = Cache}} = State) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of			 
	#channel{remote_id = Id} = Channel ->
	    send_msg({connection_reply, Pid, 
		      ssh_connection:channel_close_msg(Id)}),
	    ssh_channel:cache_update(Cache, Channel#channel{sent_close = true}),
	    SshOpts = proplists:get_value(ssh_opts, State#state.opts),
	    case proplists:get_value(idle_time, SshOpts) of
		infinity ->
		    ok;
		_IdleTime ->
		    erlang:send_after(5000, self(), {check_cache, [], []})
	    end,
	    {reply, ok, State};
	undefined -> 
	    {reply, ok, State}
    end;

handle_call(stop, _, #state{connection_state = Connection0,
			    role = Role,
			    opts = Opts} = State) ->
    {disconnect, Reason, {{replies, Replies}, Connection}} =
	ssh_connection:handle_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
						      description = "User closed down connection",
						      language = "en"}, Connection0, undefined, 
				  Role),
    lists:foreach(fun send_msg/1, Replies),
    SSHOpts = proplists:get_value(ssh_opts, Opts),
    disconnect_fun(Reason, SSHOpts),
    {stop, normal, ok, State#state{connection_state = Connection}};

%% API violation make it the violaters problem
%% by ignoring it. The violating process will get
%% a timeout or hang. 
handle_call(_, _, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({request, ChannelPid, ChannelId, Type, Data}, State0) ->
    {{replies, Replies}, State} = handle_request(ChannelPid, ChannelId, 
						 Type, Data, 
						 false, none, State0),
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

handle_cast({request, ChannelId, Type, Data}, State0) ->
    {{replies, Replies}, State} = handle_request(ChannelId, Type, Data, 
						 false, none, State0),
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

handle_cast({reply_request, Status, ChannelId}, #state{connection_state =
        #connection{channel_cache = Cache}} = State0) ->
    State = case ssh_channel:cache_lookup(Cache, ChannelId) of
        #channel{remote_id = RemoteId} ->
            cm_message({Status, RemoteId}, State0);
        undefined ->
            State0
    end,
    {noreply, State};

handle_cast({global_request, _, _, _, _} = Request, State0) ->
    State = handle_global_request(Request, State0),
    {noreply, State};

handle_cast(renegotiate, #state{connection = Pid} = State) ->
    ssh_connection_handler:renegotiate(Pid),
    {noreply, State};
handle_cast(renegotiate_data, #state{connection = Pid} = State) ->
    ssh_connection_handler:renegotiate_data(Pid),
    {noreply, State};
handle_cast({adjust_window, ChannelId, Bytes}, 
	    #state{connection = Pid, connection_state =
		   #connection{channel_cache = Cache}} = State) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{recv_window_size = WinSize, remote_id = Id} = Channel ->
	    ssh_channel:cache_update(Cache, Channel#channel{recv_window_size = 
					       WinSize + Bytes}),
	    Msg = ssh_connection:channel_adjust_window_msg(Id, Bytes),
	    send_msg({connection_reply, Pid, Msg});
	undefined -> 
	    ignore
    end,
    {noreply, State};

handle_cast({success, ChannelId},  #state{connection = Pid} = State) ->
    Msg = ssh_connection:channel_success_msg(ChannelId),
    send_msg({connection_reply, Pid, Msg}),
    {noreply, State};

handle_cast({failure, ChannelId},  #state{connection = Pid} = State) ->
    Msg = ssh_connection:channel_failure_msg(ChannelId),
    send_msg({connection_reply, Pid, Msg}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({start_connection, server, 
	     [Address, Port, Socket, Options, SubSysSup]},
	    #state{connection_state = CState} = State) ->
    {ok, Connection} = ssh_transport:accept(Address, Port, Socket, Options),
    Shell = proplists:get_value(shell, Options),
    Exec = proplists:get_value(exec, Options),
    CliSpec = proplists:get_value(ssh_cli, Options, {ssh_cli, [Shell]}),
    ssh_connection_handler:send_event(Connection, socket_control),
    erlang:send_after(60000, self(), rekey_data),
    {noreply, State#state{connection = Connection,
			  connection_state = 
			  CState#connection{address = Address,
					    port = Port,
					    cli_spec = CliSpec,
					    options = Options,
					    exec = Exec,
					    sub_system_supervisor = SubSysSup
					   }}};
	    
handle_info({start_connection, client, 
	     [Parent, Address, Port, SocketOpts, Options]},
	    #state{client = Pid} = State) ->
    case (catch ssh_transport:connect(Parent, Address, 
				      Port, SocketOpts, Options)) of
	{ok, Connection} ->
	    erlang:send_after(60000, self(), rekey_data),
	    erlang:send_after(3600000, self(), rekey),
	    {noreply, State#state{connection = Connection}};
	Reason ->
	    Pid ! {self(), not_connected, Reason},
	    {stop, {shutdown, normal}, State}
    end;
handle_info({check_cache, _ , _}, 
	    #state{connection_state = 
		       #connection{channel_cache = Cache}} = State) ->
    {noreply, check_cache(State, Cache)};
handle_info({ssh_cm, _Sender, Msg}, State0) ->
    %% Backwards compatibility!  
    State = cm_message(Msg, State0),
    {noreply, State};

%% Nop backwards compatibility
handle_info({same_user, _}, State) ->
    {noreply, State};

handle_info(ssh_connected, #state{role = client, client = Pid} 
	    = State) ->
    Pid ! {self(), is_connected},
    {noreply, State#state{connected = true, opts = handle_password(State#state.opts)}};

handle_info(ssh_connected, #state{role = server} = State) ->
    {noreply, State#state{connected = true}};

%%% Handle that ssh channels user process goes down
handle_info({'DOWN', _Ref, process, ChannelPid, _Reason}, State) ->
    handle_down(handle_channel_down(ChannelPid, State));

%%% So that terminate will be run when supervisor is shutdown
handle_info({'EXIT', _Sup, Reason}, State) ->
    {stop, Reason, State};
handle_info(rekey, State) ->
    renegotiate(self()),
    erlang:send_after(3600000, self(), rekey),
    {noreply, State};
handle_info(rekey_data, State) ->
    renegotiate_data(self()),
    erlang:send_after(60000, self(), rekey_data),
    {noreply, State}.
handle_password(Opts) ->
    handle_rsa_password(handle_dsa_password(handle_normal_password(Opts))).
handle_normal_password(Opts) ->
    case proplists:get_value(ssh_opts, Opts, false) of
	false ->
	    Opts;
	SshOpts ->
	    case proplists:get_value(password, SshOpts, false) of
		false ->
		    Opts;
		_Password ->
		    NewOpts = [{password, undefined}|lists:keydelete(password, 1, SshOpts)],
		    [{ssh_opts, NewOpts}|lists:keydelete(ssh_opts, 1, Opts)]
	    end
    end.
handle_dsa_password(Opts) ->
    case proplists:get_value(ssh_opts, Opts, false) of
	false ->
	    Opts;
	SshOpts ->
	    case proplists:get_value(dsa_pass_phrase, SshOpts, false) of
		false ->
		    Opts;
		_Password ->
		    NewOpts = [{dsa_pass_phrase, undefined}|lists:keydelete(dsa_pass_phrase, 1, SshOpts)],
		    [{ssh_opts, NewOpts}|lists:keydelete(ssh_opts, 1, Opts)]
	    end
    end.
handle_rsa_password(Opts) ->
    case proplists:get_value(ssh_opts, Opts, false) of
	false ->
	    Opts;
	SshOpts ->
	    case proplists:get_value(rsa_pass_phrase, SshOpts, false) of
		false ->
		    Opts;
		_Password ->
		    NewOpts = [{rsa_pass_phrase, undefined}|lists:keydelete(rsa_pass_phrase, 1, SshOpts)],
		    [{ssh_opts, NewOpts}|lists:keydelete(ssh_opts, 1, Opts)]
	    end
    end.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{role = client,
			  connection_state =
			      #connection{connection_supervisor = Supervisor}}) ->
    sshc_sup:stop_child(Supervisor);
	
terminate(_Reason, #state{role = server,
			 connection_state =
			     #connection{sub_system_supervisor = SubSysSup},
			 opts = Opts}) ->
    Address =  proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    SystemSup = ssh_system_sup:system_supervisor(Address, Port),
    ssh_system_sup:stop_subsystem(SystemSup, SubSysSup).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_idle_time(SshOptions) ->
    case proplists:get_value(idle_time, SshOptions) of
	infinity ->
	    infinity;
       _IdleTime -> %% We dont want to set the timeout on first connect
	    undefined
    end.
check_cache(State, Cache) ->
    %% Check the number of entries in Cache
    case proplists:get_value(size, ets:info(Cache)) of
	0 ->
	    Opts = proplists:get_value(ssh_opts, State#state.opts),
	    case proplists:get_value(idle_time, Opts) of
		infinity ->
		    State;
		undefined ->
		    State;
		Time ->
		    case State#state.idle_timer_ref of
			undefined ->
			    TimerRef = erlang:send_after(Time, self(), {'EXIT', [], "Timeout"}),
			    State#state{idle_timer_ref=TimerRef};
			_ ->
			    State
		    end
	    end;
	_ ->
	    State
    end.
remove_timer_ref(State) ->
    case State#state.idle_timer_ref of
	infinity -> %% If the timer is not activated
	    State;
	undefined -> %% If we already has cancelled the timer
	    State;
	TimerRef -> %% Timer is active
	    erlang:cancel_timer(TimerRef),
	    State#state{idle_timer_ref = undefined}
    end.
channel_data(Id, Type, Data, Connection0, ConnectionPid, From, State) ->
    case ssh_connection:channel_data(Id, Type, Data, Connection0,
				     ConnectionPid, From) of
	{{replies, Replies}, Connection} ->
	    lists:foreach(fun send_msg/1, Replies),
	    {noreply, State#state{connection_state = Connection}};
	{noreply, Connection} ->
	    {noreply, State#state{connection_state = Connection}}
    end.

call(Pid, Msg) ->
    call(Pid, Msg, infinity).
call(Pid, Msg, Timeout) ->
    try gen_server:call(Pid, Msg, Timeout) of
	Result ->
	    Result
    catch
	exit:{timeout, _} ->
	    {error, timeout};
	exit:{normal, _} ->
	    {error, channel_closed};
	exit:{{shutdown, _}, _} ->
	    {error, channel_closed};
	exit:{noproc,_} ->
	    {error, channel_closed}
    end.

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

decode_ssh_msg(BinMsg) when is_binary(BinMsg)->
    ssh_bits:decode(BinMsg);
decode_ssh_msg(Msg) ->
    Msg.


send_msg(Msg) ->
    catch do_send_msg(Msg).
do_send_msg({channel_data, Pid, Data}) ->
    Pid ! {ssh_cm, self(), Data};
do_send_msg({channel_requst_reply, From, Data}) ->
    gen_server:reply(From, Data);
do_send_msg({connection_reply, Pid, Data}) ->
    Msg = ssh_bits:encode(Data),
    ssh_connection_handler:send(Pid, Msg);
do_send_msg({flow_control, Cache, Channel, From, Msg}) ->
    ssh_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
    gen_server:reply(From, Msg);
do_send_msg({flow_control, From, Msg}) ->
    gen_server:reply(From, Msg).

handle_request(ChannelPid, ChannelId, Type, Data, WantReply, From, 
	       #state{connection = Pid,
		      connection_state = 
		      #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    update_sys(Cache, Channel, Type, ChannelPid),
	    Msg = ssh_connection:channel_request_msg(Id, Type, 
						     WantReply, Data),
	    Replies = [{connection_reply, Pid, Msg}],
	    State = add_request(WantReply, ChannelId, From, State0),
	    {{replies, Replies}, State};
	undefined ->
	    {{replies, []}, State0}
    end.

handle_request(ChannelId, Type, Data, WantReply, From, 
	       #state{connection = Pid,
		      connection_state = 
		      #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id}  ->
	    Msg = ssh_connection:channel_request_msg(Id, Type, 
						     WantReply, Data),
	    Replies = [{connection_reply, Pid, Msg}],
	    State = add_request(WantReply, ChannelId, From, State0),
	    {{replies, Replies}, State};
	undefined ->
	    {{replies, []}, State0}
    end.

handle_down({{replies, Replies}, State}) ->
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State}.

handle_channel_down(ChannelPid, #state{connection_state = 
					   #connection{channel_cache = Cache}} =
			State) ->
    ssh_channel:cache_foldl(
	       fun(Channel, Acc) when Channel#channel.user == ChannelPid ->
		       ssh_channel:cache_delete(Cache,
						Channel#channel.local_id),
		       Acc;
		  (_,Acc) ->
		       Acc
	       end, [], Cache),
    {{replies, []}, check_cache(State, Cache)}.

update_sys(Cache, Channel, Type, ChannelPid) ->
    ssh_channel:cache_update(Cache, 
			     Channel#channel{sys = Type, user = ChannelPid}).

add_request(false, _ChannelId, _From, State) ->
    State;
add_request(true, ChannelId, From, #state{connection_state = 
					#connection{requests = Requests0} = 
					Connection} = State) ->
    Requests = [{ChannelId, From} | Requests0],
    State#state{connection_state = Connection#connection{requests = Requests}}.
 
new_channel_id(#state{connection_state = #connection{channel_id_seed = Id} =
		      Connection}
	       = State) ->
    {Id, State#state{connection_state = 
		     Connection#connection{channel_id_seed = Id + 1}}}.
   
handle_global_request({global_request, ChannelPid, 
		       "tcpip-forward" = Type, WantReply, 
		       <<?UINT32(IPLen), 
			IP:IPLen/binary, ?UINT32(Port)>> = Data}, 
		      #state{connection = ConnectionPid, 
			     connection_state = 
			     #connection{channel_cache = Cache}
			     = Connection0} = State) ->
    ssh_channel:cache_update(Cache, #channel{user = ChannelPid,
					     type = "forwarded-tcpip",
 					     sys = none}),
    Connection = ssh_connection:bind(IP, Port, ChannelPid, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg({connection_reply, ConnectionPid, Msg}),
    State#state{connection_state = Connection};

handle_global_request({global_request, _Pid, "cancel-tcpip-forward" = Type, 
		       WantReply, <<?UINT32(IPLen), 
				   IP:IPLen/binary, ?UINT32(Port)>> = Data}, 
		      #state{connection = Pid,
			     connection_state = Connection0} = State) ->
    Connection = ssh_connection:unbind(IP, Port, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg({connection_reply, Pid, Msg}),
    State#state{connection_state = Connection};

handle_global_request({global_request, _Pid, "cancel-tcpip-forward" = Type, 
		       WantReply,  Data}, #state{connection = Pid} = State) ->
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg({connection_reply, Pid, Msg}),
    State.

cm_message(Msg, State) ->
    {noreply, NewState} =  handle_cast(Msg, State),
    NewState.

disconnect_fun(Reason, Opts) ->
    case proplists:get_value(disconnectfun, Opts) of
 	undefined ->
 	    ok;
 	Fun ->
 	    catch Fun(Reason)
     end.

ssh_channel_info_handler(Options, Channel, From) ->
    Info = ssh_channel_info(Options, Channel, []),
    send_msg({channel_requst_reply, From, Info}).

ssh_channel_info([], _, Acc) ->
    Acc;

ssh_channel_info([recv_window | Rest], #channel{recv_window_size = WinSize,
						   recv_packet_size = Packsize
						  } = Channel, Acc) ->
    ssh_channel_info(Rest, Channel, [{recv_window, {{win_size, WinSize}, 
						      {packet_size, Packsize}}} | Acc]);
ssh_channel_info([send_window | Rest], #channel{send_window_size = WinSize,
						send_packet_size = Packsize
					       } = Channel, Acc) ->
    ssh_channel_info(Rest, Channel, [{send_window, {{win_size, WinSize}, 
						      {packet_size, Packsize}}} | Acc]);
ssh_channel_info([ _ | Rest], Channel, Acc) ->
    ssh_channel_info(Rest, Channel, Acc).



