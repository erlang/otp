%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
%%----------------------------------------------------------------------
%% Purpose: Handles an ssh connection, e.i. both the
%% setup SSH Transport Layer Protocol (RFC 4253), Authentication
%% Protocol (RFC 4252) and SSH connection Protocol (RFC 4255)
%% Details of the different protocols are
%% implemented in ssh_transport.erl, ssh_auth.erl and ssh_connection.erl
%% ----------------------------------------------------------------------

-module(ssh_connection_handler).

-behaviour(gen_fsm).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_auth.hrl").
-include("ssh_connect.hrl").
-compile(export_all).
-export([start_link/3]).

%% Internal application API
-export([open_channel/6, reply_request/3, request/6, request/7,
	 global_request/4, send/5, send_eof/2, info/1, info/2,
	 connection_info/2, channel_info/3,
	 adjust_window/3, close/2, stop/1, renegotiate/1, renegotiate_data/1,
	 start_connection/4,
	 get_print_info/1]).

%% gen_fsm callbacks
-export([hello/2, kexinit/2, key_exchange/2, 
	 key_exchange_dh_gex_init/2, key_exchange_dh_gex_reply/2,
	 new_keys/2,
	 service_request/2, connected/2,
	 userauth/2,
	 userauth_keyboard_interactive/2,
	 userauth_keyboard_interactive_info_response/2,
	 error/2]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, format_status/2, code_change/4]).

-record(state, {
	  role,
	  client,
	  starter,
	  auth_user,
	  connection_state,
	  latest_channel_id = 0,
	  idle_timer_ref,
	  transport_protocol,      % ex: tcp
	  transport_cb,
	  transport_close_tag,
	  ssh_params,              %  #ssh{} - from ssh.hrl
	  socket,                  %  socket()
	  decoded_data_buffer,     %  binary()
	  encoded_data_buffer,     %  binary()
	  undecoded_packet_length, %  integer()
	  key_exchange_init_msg,   %  #ssh_msg_kexinit{}
	  renegotiate = false,     %  boolean() 
	  last_size_rekey = 0,
	  event_queue = [],
	  connection_queue,
	  address,
	  port,
	  opts,
	  recbuf
	 }). 

-type state_name()           :: hello | kexinit | key_exchange | key_exchange_dh_gex_init |
				key_exchange_dh_gex_reply | new_keys | service_request | 
				userauth | userauth_keyboard_interactive |
				userauth_keyboard_interactive_info_response |
				connection.

-type gen_fsm_state_return() :: {next_state, state_name(), term()} |
				{next_state, state_name(), term(), timeout()} |
				{stop, term(), term()}.

-type gen_fsm_sync_return() :: {next_state, state_name(), term()} |
			       {next_state, state_name(), term(), timeout()} |
			       {reply, term(), state_name(), term()} |
			       {stop, term(), term(), term()}.

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec start_connection(client| server, port(), proplists:proplist(),
		       timeout()) -> {ok, pid()} | {error, term()}.
%%--------------------------------------------------------------------
start_connection(client = Role, Socket, Options, Timeout) ->
    try
	{ok, Pid} = sshc_sup:start_child([Role, Socket, Options]),
	{_, Callback, _} =
	    proplists:get_value(transport, Options, {tcp, gen_tcp, tcp_closed}),
	ok = socket_control(Socket, Pid, Callback),
	Ref = erlang:monitor(process, Pid),
	handshake(Pid, Ref, Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssh_not_started};
	_:Error ->
	    {error, Error}
    end;

start_connection(server = Role, Socket, Options, Timeout) ->
    SSH_Opts = proplists:get_value(ssh_opts, Options, []),
    try
	case proplists:get_value(parallel_login, SSH_Opts, false) of
	    true ->
		HandshakerPid = 
		    spawn_link(fun() -> 
				       receive
					   {do_handshake, Pid} ->
					       handshake(Pid, erlang:monitor(process,Pid), Timeout)
				       end
			       end),
		ChildPid = start_the_connection_child(HandshakerPid, Role, Socket, Options),
		HandshakerPid ! {do_handshake, ChildPid};
	    false ->
		ChildPid = start_the_connection_child(self(), Role, Socket, Options),
		handshake(ChildPid, erlang:monitor(process,ChildPid), Timeout)
	end
    catch
	exit:{noproc, _} ->
	    {error, ssh_not_started};
	_:Error ->
	    {error, Error}
    end.

start_the_connection_child(UserPid, Role, Socket, Options) ->
    Sups = proplists:get_value(supervisors, Options),
    ConnectionSup = proplists:get_value(connection_sup, Sups),
    Opts = [{supervisors, Sups}, {user_pid, UserPid} | proplists:get_value(ssh_opts, Options, [])],
    {ok, Pid} = ssh_connection_sup:start_child(ConnectionSup, [Role, Socket, Opts]),
    {_, Callback, _} =  proplists:get_value(transport, Options, {tcp, gen_tcp, tcp_closed}),
    socket_control(Socket, Pid, Callback),
    Pid.


start_link(Role, Socket, Options) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Role, Socket, Options]])}.

init([Role, Socket, SshOpts]) ->
    process_flag(trap_exit, true),
    {NumVsn, StrVsn} = ssh_transport:versions(Role, SshOpts),
    {Protocol, Callback, CloseTag} = 
	proplists:get_value(transport, SshOpts, {tcp, gen_tcp, tcp_closed}),
    Cache = ssh_channel:cache_create(),
    State0 = #state{
		role = Role,
		connection_state = #connection{channel_cache = Cache,
					       channel_id_seed = 0,
					       port_bindings = [],
					       requests = [],
					       options = SshOpts},
		socket = Socket,
		decoded_data_buffer = <<>>,
		encoded_data_buffer = <<>>,
		transport_protocol = Protocol,
		transport_cb = Callback,
		transport_close_tag = CloseTag,
		opts = SshOpts
	       },

    State = init_role(State0),

    try init_ssh(Role, NumVsn, StrVsn, SshOpts, Socket) of
	Ssh ->
	    gen_fsm:enter_loop(?MODULE, [], hello,
			       State#state{ssh_params = Ssh})
    catch
	_:Error ->
	    gen_fsm:enter_loop(?MODULE, [], error, {Error, State})
    end.

%% Temporary fix for the Nessus error.  SYN->   <-SYNACK  ACK->  RST-> ?
error(_Event, {Error,State=#state{}}) ->
    case Error of
	{badmatch,{error,enotconn}} ->
	    %% {error,enotconn} probably from inet:peername in
	    %% init_ssh(server,..)/5 called from init/1
	    {stop, {shutdown,"TCP connenction to server was prematurely closed by the client"}, State};
	_ ->
	    {stop, {shutdown,{init,Error}}, State}
    end;
error(Event, State) -> 
    %% State deliberately not checked beeing #state. This is a panic-clause...
    {stop, {shutdown,{init,{spurious_error,Event}}}, State}.

%%--------------------------------------------------------------------
-spec open_channel(pid(), string(), iodata(), integer(), integer(),
		   timeout()) -> {open, channel_id()} | {error, term()}.
%%--------------------------------------------------------------------
open_channel(ConnectionHandler, ChannelType, ChannelSpecificData,
					InitialWindowSize,
	     MaxPacketSize, Timeout) ->
    sync_send_all_state_event(ConnectionHandler, {open, self(), ChannelType,
						  InitialWindowSize, MaxPacketSize,
						  ChannelSpecificData,
						  Timeout}).
%%--------------------------------------------------------------------
-spec request(pid(), pid(), channel_id(), string(), boolean(), iodata(),
		   timeout()) -> success | failure | ok | {error, term()}.
%%--------------------------------------------------------------------
request(ConnectionHandler, ChannelPid, ChannelId, Type, true, Data, Timeout) ->
    sync_send_all_state_event(ConnectionHandler, {request, ChannelPid, ChannelId, Type, Data,
						  Timeout});
request(ConnectionHandler, ChannelPid, ChannelId, Type, false, Data, _) ->
    send_all_state_event(ConnectionHandler, {request, ChannelPid, ChannelId, Type, Data}).

%%--------------------------------------------------------------------
-spec request(pid(), channel_id(), string(), boolean(), iodata(),
		   timeout()) ->  success | failure | {error, timeout}.
%%--------------------------------------------------------------------
request(ConnectionHandler, ChannelId, Type, true, Data, Timeout) ->
    sync_send_all_state_event(ConnectionHandler, {request, ChannelId, Type, Data, Timeout});
request(ConnectionHandler, ChannelId, Type, false, Data, _) ->
    send_all_state_event(ConnectionHandler, {request, ChannelId, Type, Data}).

%%--------------------------------------------------------------------
-spec reply_request(pid(), success | failure, channel_id()) -> ok.
%%--------------------------------------------------------------------
reply_request(ConnectionHandler, Status, ChannelId) ->
    send_all_state_event(ConnectionHandler, {reply_request, Status, ChannelId}).

%%--------------------------------------------------------------------
-spec global_request(pid(), string(), boolean(), iolist()) -> ok | error.
%%--------------------------------------------------------------------
global_request(ConnectionHandler, Type, true = Reply, Data) ->
    case sync_send_all_state_event(ConnectionHandler,
				   {global_request, self(), Type, Reply, Data}) of
	{ssh_cm, ConnectionHandler, {success, _}} ->
	    ok;
	{ssh_cm, ConnectionHandler, {failure, _}} ->
	    error
    end;
global_request(ConnectionHandler, Type, false = Reply, Data) ->
    send_all_state_event(ConnectionHandler, {global_request, self(), Type, Reply, Data}).

%%--------------------------------------------------------------------
-spec send(pid(), channel_id(), integer(), iodata(), timeout()) ->
		  ok | {error, timeout} | {error, closed}.
%%--------------------------------------------------------------------
send(ConnectionHandler, ChannelId, Type, Data, Timeout) ->
    sync_send_all_state_event(ConnectionHandler, {data, ChannelId, Type, Data, Timeout}).

%%--------------------------------------------------------------------
-spec send_eof(pid(), channel_id()) -> ok | {error, closed}.
%%--------------------------------------------------------------------
send_eof(ConnectionHandler, ChannelId) ->
    sync_send_all_state_event(ConnectionHandler, {eof, ChannelId}).

%%--------------------------------------------------------------------
-spec connection_info(pid(), [atom()]) -> proplists:proplist().
%%--------------------------------------------------------------------
get_print_info(ConnectionHandler) ->
    sync_send_all_state_event(ConnectionHandler, get_print_info, 1000).

connection_info(ConnectionHandler, Options) ->
    sync_send_all_state_event(ConnectionHandler, {connection_info, Options}).

%%--------------------------------------------------------------------
-spec channel_info(pid(), channel_id(), [atom()]) -> proplists:proplist().
%%--------------------------------------------------------------------
channel_info(ConnectionHandler, ChannelId, Options) ->
    sync_send_all_state_event(ConnectionHandler, {channel_info, ChannelId, Options}).

%%--------------------------------------------------------------------
-spec adjust_window(pid(), channel_id(), integer()) -> ok.
%%--------------------------------------------------------------------
adjust_window(ConnectionHandler, Channel, Bytes) ->
    send_all_state_event(ConnectionHandler, {adjust_window, Channel, Bytes}).
%%--------------------------------------------------------------------
-spec renegotiate(pid()) -> ok.
%%--------------------------------------------------------------------
renegotiate(ConnectionHandler) ->
    send_all_state_event(ConnectionHandler, renegotiate).

%%--------------------------------------------------------------------
-spec renegotiate_data(pid()) -> ok.
%%--------------------------------------------------------------------
renegotiate_data(ConnectionHandler) ->
    send_all_state_event(ConnectionHandler, data_size).

%%--------------------------------------------------------------------
-spec close(pid(), channel_id()) -> ok.
%%--------------------------------------------------------------------
close(ConnectionHandler, ChannelId) ->
    case sync_send_all_state_event(ConnectionHandler, {close, ChannelId}) of
	ok ->
	    ok;
	{error, closed} -> 
	    ok
    end.	
	
%%--------------------------------------------------------------------
-spec stop(pid()) -> ok | {error, term()}.
%%--------------------------------------------------------------------
stop(ConnectionHandler)->
    case sync_send_all_state_event(ConnectionHandler, stop) of
       {error, closed} ->
	    ok;
	Other ->
	    Other
    end.

info(ConnectionHandler) ->
    info(ConnectionHandler, {info, all}).

info(ConnectionHandler, ChannelProcess) ->
    sync_send_all_state_event(ConnectionHandler, {info, ChannelProcess}).


%%====================================================================
%% gen_fsm callbacks
%%====================================================================

%%--------------------------------------------------------------------
-spec hello(socket_control | {info_line, list()} | {version_exchange, list()},
	    #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------

hello(socket_control, #state{socket = Socket, ssh_params = Ssh} = State) ->
    VsnMsg = ssh_transport:hello_version_msg(string_version(Ssh)),
    send_msg(VsnMsg, State),
    case getopt(recbuf, Socket) of
	{ok, Size} ->
	    inet:setopts(Socket, [{packet, line}, {active, once}, {recbuf, ?MAX_PROTO_VERSION}]),
	    {next_state, hello, State#state{recbuf = Size}};
	{error, Reason} ->
	    {stop, {shutdown, Reason}, State}
    end;

hello({info_line, _Line},#state{role = client, socket = Socket} = State) ->
    %% The server may send info lines before the version_exchange
    inet:setopts(Socket, [{active, once}]),
    {next_state, hello, State};

hello({info_line, _Line},#state{role = server,
				socket = Socket,
				transport_cb = Transport } = State) ->
    %% as openssh
    Transport:send(Socket, "Protocol mismatch."),
    {stop, {shutdown,"Protocol mismatch in version exchange."}, State};

hello({version_exchange, Version}, #state{ssh_params = Ssh0,
					  socket = Socket,
					  recbuf = Size} = State) ->
    {NumVsn, StrVsn} = ssh_transport:handle_hello_version(Version),
    case handle_version(NumVsn, StrVsn, Ssh0) of
	{ok, Ssh1} ->
	    inet:setopts(Socket, [{packet,0}, {mode,binary}, {active, once}, {recbuf, Size}]),
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh1),
	    send_msg(SshPacket, State),
	    {next_state, kexinit, next_packet(State#state{ssh_params = Ssh,
							  key_exchange_init_msg = 
							  KeyInitMsg})};
	not_supported ->
	    DisconnectMsg =
		#ssh_msg_disconnect{code = 
					?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,
				    description = "Protocol version " ++  StrVsn 
				    ++ " not supported",
				    language = "en"},
	    handle_disconnect(DisconnectMsg, State)
    end.

%%--------------------------------------------------------------------
-spec kexinit({#ssh_msg_kexinit{}, binary()}, #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------
kexinit({#ssh_msg_kexinit{} = Kex, Payload},
	#state{ssh_params = #ssh{role = Role} = Ssh0,
	       key_exchange_init_msg = OwnKex} =
	    State) ->
    Ssh1 = ssh_transport:key_init(opposite_role(Role), Ssh0, Payload), 
    case ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1) of
	{ok, NextKexMsg, Ssh} when Role == client ->
	    send_msg(NextKexMsg, State),
	    {next_state, key_exchange, 
	     next_packet(State#state{ssh_params = Ssh})};
	{ok, Ssh} when Role == server ->
	    {next_state, key_exchange, 
	     next_packet(State#state{ssh_params = Ssh})}
    end.

%%--------------------------------------------------------------------
-spec key_exchange(#ssh_msg_kexdh_init{} | #ssh_msg_kexdh_reply{} |
		   #ssh_msg_kex_dh_gex_group{} | #ssh_msg_kex_dh_gex_request{} |
		   #ssh_msg_kex_dh_gex_request{} | #ssh_msg_kex_dh_gex_reply{}, #state{})
		  -> gen_fsm_state_return().
%%--------------------------------------------------------------------

key_exchange(#ssh_msg_kexdh_init{} = Msg, 
	     #state{ssh_params = #ssh{role = server} = Ssh0} = State) ->
    case ssh_transport:handle_kexdh_init(Msg, Ssh0) of
	{ok, KexdhReply, Ssh1} ->
	    send_msg(KexdhReply, State),
	    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
	    send_msg(NewKeys, State),
	    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}
    end;

key_exchange(#ssh_msg_kexdh_reply{} = Msg, 
	     #state{ssh_params = #ssh{role = client} = Ssh0} = State) -> 
    {ok, NewKeys, Ssh} = ssh_transport:handle_kexdh_reply(Msg, Ssh0),
    send_msg(NewKeys, State),
    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})};

key_exchange(#ssh_msg_kex_dh_gex_request{} = Msg, 
	     #state{ssh_params = #ssh{role = server} = Ssh0} = State) ->
    {ok, GexGroup, Ssh} = ssh_transport:handle_kex_dh_gex_request(Msg, Ssh0),
    send_msg(GexGroup, State),
    {next_state, key_exchange_dh_gex_init, next_packet(State#state{ssh_params = Ssh})};

key_exchange(#ssh_msg_kex_dh_gex_request_old{} = Msg, 
	     #state{ssh_params = #ssh{role = server} = Ssh0} = State) ->
    {ok, GexGroup, Ssh} = ssh_transport:handle_kex_dh_gex_request(Msg, Ssh0),
    send_msg(GexGroup, State),
    {next_state, key_exchange_dh_gex_init, next_packet(State#state{ssh_params = Ssh})};

key_exchange(#ssh_msg_kex_dh_gex_group{} = Msg, 
	     #state{ssh_params = #ssh{role = client} = Ssh0} = State) ->
    {ok, KexGexInit, Ssh} = ssh_transport:handle_kex_dh_gex_group(Msg, Ssh0),
    send_msg(KexGexInit, State),
    {next_state, key_exchange_dh_gex_reply, next_packet(State#state{ssh_params = Ssh})};

key_exchange(#ssh_msg_kex_ecdh_init{} = Msg, 
	     #state{ssh_params = #ssh{role = server} = Ssh0} = State) ->
    {ok, KexEcdhReply, Ssh1} = ssh_transport:handle_kex_ecdh_init(Msg, Ssh0),
    send_msg(KexEcdhReply, State),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    send_msg(NewKeys, State),
    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})};

key_exchange(#ssh_msg_kex_ecdh_reply{} = Msg, 
	     #state{ssh_params = #ssh{role = client} = Ssh0} = State) ->
    {ok, NewKeys, Ssh} = ssh_transport:handle_kex_ecdh_reply(Msg, Ssh0),
    send_msg(NewKeys, State),
    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}.

%%--------------------------------------------------------------------
-spec key_exchange_dh_gex_init(#ssh_msg_kex_dh_gex_init{}, #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------
key_exchange_dh_gex_init(#ssh_msg_kex_dh_gex_init{} = Msg,
			 #state{ssh_params = #ssh{role = server} = Ssh0} = State) ->
    {ok, KexGexReply, Ssh1} =  ssh_transport:handle_kex_dh_gex_init(Msg, Ssh0),
    send_msg(KexGexReply, State),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    send_msg(NewKeys, State),
    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh})}.

%%--------------------------------------------------------------------
-spec key_exchange_dh_gex_reply(#ssh_msg_kex_dh_gex_reply{}, #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------
key_exchange_dh_gex_reply(#ssh_msg_kex_dh_gex_reply{} = Msg,
			  #state{ssh_params = #ssh{role = client} = Ssh0} = State) ->
    {ok, NewKeys, Ssh1} =  ssh_transport:handle_kex_dh_gex_reply(Msg, Ssh0),
    send_msg(NewKeys, State),
    {next_state, new_keys, next_packet(State#state{ssh_params = Ssh1})}.

%%--------------------------------------------------------------------
-spec new_keys(#ssh_msg_newkeys{}, #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------

new_keys(#ssh_msg_newkeys{} = Msg, #state{ssh_params = Ssh0} = State0) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, Ssh0),
    after_new_keys(next_packet(State0#state{ssh_params = Ssh})).

%%--------------------------------------------------------------------
-spec service_request(#ssh_msg_service_request{} | #ssh_msg_service_accept{},
		      #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------
service_request(#ssh_msg_service_request{name = "ssh-userauth"} = Msg, 
	 #state{ssh_params = #ssh{role = server, 
				  session_id = SessionId} = Ssh0} = State) ->
    {ok, {Reply, Ssh}} = ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0),
    send_msg(Reply, State),
    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})};

service_request(#ssh_msg_service_accept{name = "ssh-userauth"},  
		#state{ssh_params = #ssh{role = client,
					 service = "ssh-userauth"} = Ssh0} = 
		    State) ->
    {Msg, Ssh} = ssh_auth:init_userauth_request_msg(Ssh0),
    send_msg(Msg, State),
    {next_state, userauth, next_packet(State#state{auth_user = Ssh#ssh.user, ssh_params = Ssh})}.

%%--------------------------------------------------------------------
-spec userauth(#ssh_msg_userauth_request{} | #ssh_msg_userauth_info_request{} |
	       #ssh_msg_userauth_info_response{} | #ssh_msg_userauth_success{} |
	       #ssh_msg_userauth_failure{} | #ssh_msg_userauth_banner{},
	       #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------
userauth(#ssh_msg_userauth_request{service = "ssh-connection",
                                  method = "none"} = Msg, 
        #state{ssh_params = #ssh{session_id = SessionId, role = server, 
                                 service = "ssh-connection"} = Ssh0
              } = State) -> 
    {not_authorized, {_User, _Reason}, {Reply, Ssh}} =
	ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0),
    send_msg(Reply, State),
    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})};

userauth(#ssh_msg_userauth_request{service = "ssh-connection",
				   method = Method} = Msg, 
	 #state{ssh_params = #ssh{session_id = SessionId, role = server, 
				  service = "ssh-connection",
				  peer = {_, Address}} = Ssh0,
		opts = Opts, starter = Pid} = State) ->
    case lists:member(Method, Ssh0#ssh.userauth_methods) of
	true ->
	    case ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0) of
		{authorized, User, {Reply, Ssh}} ->
		    send_msg(Reply, State),
		    Pid ! ssh_connected,
		    connected_fun(User, Address, Method, Opts),
		    {next_state, connected, 
		     next_packet(State#state{auth_user = User, ssh_params = Ssh#ssh{authenticated = true}})};
		{not_authorized, {User, Reason}, {Reply, Ssh}} when Method == "keyboard-interactive" ->
		    retry_fun(User, Address, Reason, Opts),
		    send_msg(Reply, State),
		    {next_state, userauth_keyboard_interactive, next_packet(State#state{ssh_params = Ssh})};
		{not_authorized, {User, Reason}, {Reply, Ssh}} ->
		    retry_fun(User, Address, Reason, Opts),
		    send_msg(Reply, State),
		    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})} 
	    end;
	false ->
	    userauth(Msg#ssh_msg_userauth_request{method="none"}, State)
    end;

userauth(#ssh_msg_userauth_success{}, #state{ssh_params = #ssh{role = client} = Ssh,
					     starter = Pid} = State) ->
    Pid ! ssh_connected,
    {next_state, connected, next_packet(State#state{ssh_params =
							Ssh#ssh{authenticated = true}})};
userauth(#ssh_msg_userauth_failure{},  
	 #state{ssh_params = #ssh{role = client,
				  userauth_methods = []}} 
	 = State) ->
    Msg = #ssh_msg_disconnect{code = 
			      ?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
			      description = "Unable to connect using the available"
			      " authentication methods",
			      language = "en"},
    handle_disconnect(Msg, State); 

%% Server tells us which authentication methods that are allowed
userauth(#ssh_msg_userauth_failure{authentications = Methodes},  
	 #state{ssh_params = #ssh{role = client,
				  userauth_methods = none} = Ssh0} = State) ->
    AuthMethods = string:tokens(Methodes, ","),
    Ssh1 = Ssh0#ssh{userauth_methods = AuthMethods},
    case ssh_auth:userauth_request_msg(Ssh1) of
	{disconnect, DisconnectMsg, {Msg, Ssh}} ->
	    send_msg(Msg, State),
	    handle_disconnect(DisconnectMsg, State#state{ssh_params = Ssh});
	{"keyboard-interactive", {Msg, Ssh}} ->
	    send_msg(Msg, State),
	    {next_state, userauth_keyboard_interactive, next_packet(State#state{ssh_params = Ssh})};
	{_Method, {Msg, Ssh}} ->
	    send_msg(Msg, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    end;

%% The prefered authentication method failed try next method 
userauth(#ssh_msg_userauth_failure{},
	 #state{ssh_params = #ssh{role = client} = Ssh0} = State) ->
    case ssh_auth:userauth_request_msg(Ssh0) of
	{disconnect,  DisconnectMsg,{Msg, Ssh}} ->
	    send_msg(Msg, State),
	    handle_disconnect(DisconnectMsg, State#state{ssh_params = Ssh}); 
	{"keyboard-interactive", {Msg, Ssh}} ->
	    send_msg(Msg, State),
	    {next_state, userauth_keyboard_interactive, next_packet(State#state{ssh_params = Ssh})};
	{_Method, {Msg, Ssh}} ->
	    send_msg(Msg, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})}
    end;

userauth(#ssh_msg_userauth_banner{}, 
	 #state{ssh_params = #ssh{userauth_quiet_mode = true, 
				  role = client}} = State) ->
    {next_state, userauth, next_packet(State)};
userauth(#ssh_msg_userauth_banner{message = Msg}, 
	 #state{ssh_params = 
		#ssh{userauth_quiet_mode = false, role = client}} = State) ->
    io:format("~s", [Msg]),
    {next_state, userauth, next_packet(State)}.



userauth_keyboard_interactive(#ssh_msg_userauth_info_request{} = Msg, 
			      #state{ssh_params = #ssh{role = client, 
						       io_cb = IoCb} = Ssh0} = State) ->
    {ok, {Reply, Ssh}} = ssh_auth:handle_userauth_info_request(Msg, IoCb, Ssh0),
    send_msg(Reply, State),
    {next_state, userauth_keyboard_interactive_info_response, next_packet(State#state{ssh_params = Ssh})};

userauth_keyboard_interactive(#ssh_msg_userauth_info_response{} = Msg, 
			      #state{ssh_params = #ssh{role = server,
						       peer = {_, Address}} = Ssh0,
				     opts = Opts, starter = Pid} = State) ->
    case ssh_auth:handle_userauth_info_response(Msg, Ssh0) of
	{authorized, User, {Reply, Ssh}} ->
	    send_msg(Reply, State),
	    Pid ! ssh_connected,
	    connected_fun(User, Address, "keyboard-interactive", Opts),
	    {next_state, connected, 
	     next_packet(State#state{auth_user = User, ssh_params = Ssh#ssh{authenticated = true}})};
	{not_authorized, {User, Reason}, {Reply, Ssh}} ->
	    retry_fun(User, Address, Reason, Opts),
	    send_msg(Reply, State),
	    {next_state, userauth, next_packet(State#state{ssh_params = Ssh})} 
    end;
userauth_keyboard_interactive(Msg = #ssh_msg_userauth_failure{},
			      #state{ssh_params = Ssh0 =
					 #ssh{role = client,
					      userauth_preference = Prefs0}} 
			      = State) ->
    Prefs = [{Method,M,F,A} || {Method,M,F,A} <- Prefs0,
			       Method =/= "keyboard-interactive"],
    userauth(Msg, State#state{ssh_params = Ssh0#ssh{userauth_preference=Prefs}}).



userauth_keyboard_interactive_info_response(Msg=#ssh_msg_userauth_failure{},
					    #state{ssh_params = #ssh{role = client}} = State) ->
    userauth(Msg, State);
userauth_keyboard_interactive_info_response(Msg=#ssh_msg_userauth_success{},
					    #state{ssh_params = #ssh{role = client}} = State) ->
    userauth(Msg, State);
userauth_keyboard_interactive_info_response(Msg=#ssh_msg_userauth_info_request{},
					    #state{ssh_params = #ssh{role = client}} = State) ->
    userauth_keyboard_interactive(Msg, State).

%%--------------------------------------------------------------------
-spec connected({#ssh_msg_kexinit{}, binary()}, %%| %% #ssh_msg_kexdh_init{},
		 #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------
connected({#ssh_msg_kexinit{}, _Payload} = Event, #state{ssh_params = Ssh0} = State0) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
    State = State0#state{ssh_params = Ssh,
			 key_exchange_init_msg = KeyInitMsg,
			 renegotiate = true},
    send_msg(SshPacket, State),
    kexinit(Event, State).

%%--------------------------------------------------------------------
-spec handle_event(#ssh_msg_disconnect{} | #ssh_msg_ignore{} | #ssh_msg_debug{} |
		   #ssh_msg_unimplemented{} | {adjust_window, integer(), integer()} |
		   {reply_request, success | failure, integer()} | renegotiate |
		   data_size | {request, pid(), integer(), integer(), iolist()} |
		   {request, integer(), integer(), iolist()}, state_name(),
		   #state{}) -> gen_fsm_state_return().

%%--------------------------------------------------------------------
handle_event(#ssh_msg_disconnect{description = Desc} = DisconnectMsg, _StateName, #state{} = State) ->
    handle_disconnect(peer, DisconnectMsg, State),
    {stop, {shutdown, Desc}, State};

handle_event(#ssh_msg_ignore{}, StateName, State) ->
    {next_state, StateName, next_packet(State)};

handle_event(#ssh_msg_debug{always_display = Display, message = DbgMsg, language=Lang}, 
	     StateName, #state{opts = Opts} = State) ->
    F = proplists:get_value(ssh_msg_debug_fun, Opts, 
			    fun(_ConnRef, _AlwaysDisplay, _Msg, _Language) -> ok end
			   ),
    catch F(self(), Display, DbgMsg, Lang),
    {next_state, StateName, next_packet(State)};

handle_event(#ssh_msg_unimplemented{}, StateName, State) ->
    {next_state, StateName, next_packet(State)};

handle_event(renegotiate, connected, #state{ssh_params = Ssh0} 
	     = State) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
    send_msg(SshPacket, State),
    timer:apply_after(?REKEY_TIMOUT, gen_fsm, send_all_state_event, [self(), renegotiate]),
    {next_state, kexinit,
     next_packet(State#state{ssh_params = Ssh,
			     key_exchange_init_msg = KeyInitMsg,
			     renegotiate = true})};

handle_event(renegotiate, StateName, State) ->
    %% Already in key-exchange so safe to ignore
    {next_state, StateName, State};

%% Rekey due to sent data limit reached?
handle_event(data_size, connected, #state{ssh_params = Ssh0} = State) ->
    {ok, [{send_oct,Sent0}]} = inet:getstat(State#state.socket, [send_oct]),
    Sent = Sent0 - State#state.last_size_rekey,
    MaxSent = proplists:get_value(rekey_limit, State#state.opts, 1024000000),
    timer:apply_after(?REKEY_DATA_TIMOUT, gen_fsm, send_all_state_event, [self(), data_size]),
    case Sent >= MaxSent of
	true ->
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
	    send_msg(SshPacket, State),
	    {next_state, kexinit,
	     next_packet(State#state{ssh_params = Ssh,
				     key_exchange_init_msg = KeyInitMsg,
				     renegotiate = true,
				     last_size_rekey = Sent0})};
	_ ->
	    {next_state, connected, next_packet(State)}
    end;
handle_event(data_size, StateName, State) ->
    %% Already in key-exchange so safe to ignore
    {next_state, StateName, State};

handle_event(Event, StateName, State) when StateName /= connected ->
    Events = [{event, Event} | State#state.event_queue],
    {next_state, StateName, State#state{event_queue = Events}};

handle_event({adjust_window, ChannelId, Bytes}, StateName,
	     #state{connection_state =
			#connection{channel_cache = Cache}} = State0) ->
    State =
	case ssh_channel:cache_lookup(Cache, ChannelId) of
	    #channel{recv_window_size = WinSize,
		     recv_window_pending = Pending,
		     recv_packet_size = PktSize} = Channel
	      when (WinSize-Bytes) >= 2*PktSize ->
		%% The peer can send at least two more *full* packet, no hurry.
		ssh_channel:cache_update(Cache, 
					 Channel#channel{recv_window_pending = Pending + Bytes}),
		State0;

	    #channel{recv_window_size = WinSize,
		     recv_window_pending = Pending,
		     remote_id = Id} = Channel ->
		%% Now we have to update the window - we can't receive so many more pkts
		ssh_channel:cache_update(Cache, 
					 Channel#channel{recv_window_size =
							     WinSize + Bytes + Pending,
							 recv_window_pending = 0}),
		Msg = ssh_connection:channel_adjust_window_msg(Id, Bytes + Pending),
		send_replies([{connection_reply, Msg}], State0);

	    undefined ->
		State0
	end,
    {next_state, StateName, next_packet(State)};

handle_event({reply_request, success, ChannelId}, StateName,
		  #state{connection_state =
			     #connection{channel_cache = Cache}} = State0) ->
    State = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{remote_id = RemoteId} ->
		    Msg = ssh_connection:channel_success_msg(RemoteId),
		    send_replies([{connection_reply, Msg}], State0);
		undefined ->
		    State0
	    end,
    {next_state, StateName, State};

handle_event({request, ChannelPid, ChannelId, Type, Data}, StateName, State0) ->
    {{replies, Replies}, State1} = handle_request(ChannelPid, ChannelId,
						 Type, Data,
						 false, none, State0),
    State = send_replies(Replies, State1),
    {next_state, StateName, next_packet(State)};

handle_event({request, ChannelId, Type, Data}, StateName, State0) ->
    {{replies, Replies}, State1} = handle_request(ChannelId, Type, Data,
						 false, none, State0),
    State = send_replies(Replies, State1),
    {next_state, StateName, next_packet(State)};

handle_event({unknown, Data}, StateName, State) ->
    Msg = #ssh_msg_unimplemented{sequence = Data},
    send_msg(Msg, State),
    {next_state, StateName, next_packet(State)}.

%%--------------------------------------------------------------------
-spec handle_sync_event({request, pid(), channel_id(), integer(), binary(), timeout()} |
			{request, channel_id(), integer(), binary(), timeout()} |
			{global_request, pid(), integer(), boolean(), binary()} | {eof, integer()} |
			{open, pid(), integer(), channel_id(), integer(), binary(), _} |
			{send_window, channel_id()} | {recv_window, channel_id()} |
			{connection_info, [client_version | server_version | peer |
					   sockname]} | {channel_info, channel_id(), [recv_window |
										      send_window]} |
			{close, channel_id()} | stop, term(), state_name(), #state{})
		       -> gen_fsm_sync_return().
%%--------------------------------------------------------------------
handle_sync_event(get_print_info, _From, StateName, State) ->
    Reply =
	try
	    {inet:sockname(State#state.socket),
	     inet:peername(State#state.socket)
	    }
	of
	    {{ok,Local}, {ok,Remote}} -> {{Local,Remote},io_lib:format("statename=~p",[StateName])};
	    _ -> {{"-",0},"-"}
	catch
	    _:_ -> {{"?",0},"?"}
	end,
    {reply, Reply, StateName, State};

handle_sync_event({connection_info, Options}, _From, StateName, State) ->
    Info = ssh_info(Options, State, []),
    {reply, Info, StateName, State};

handle_sync_event({channel_info, ChannelId, Options}, _From, StateName,
		  #state{connection_state = #connection{channel_cache = Cache}} = State) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
       #channel{} = Channel ->
	    Info = ssh_channel_info(Options, Channel, []),
	    {reply, Info, StateName, State};
	undefined ->
	    {reply, [], StateName, State}
    end;

handle_sync_event({info, ChannelPid}, _From, StateName,
	    #state{connection_state =
		       #connection{channel_cache = Cache}} = State) ->
    Result = ssh_channel:cache_foldl(
	       fun(Channel, Acc) when ChannelPid == all;
				      Channel#channel.user == ChannelPid ->
		       [Channel | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], Cache),
    {reply, {ok, Result}, StateName, State};

handle_sync_event(stop, _, _StateName, #state{connection_state = Connection0,
					     role = Role} = State0) ->
    {disconnect, _Reason, {{replies, Replies}, Connection}} =
	ssh_connection:handle_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
						      description = "User closed down connection",
						      language = "en"}, Connection0, Role),
    State = send_replies(Replies, State0),
    {stop, normal, ok, State#state{connection_state = Connection}};


handle_sync_event(Event, From, StateName, State) when StateName /= connected ->
    Events = [{sync, Event, From} | State#state.event_queue],
    {next_state, StateName, State#state{event_queue = Events}};

handle_sync_event({request, ChannelPid, ChannelId, Type, Data, Timeout}, From,  StateName, State0) ->
    {{replies, Replies}, State1} = handle_request(ChannelPid,
						 ChannelId, Type, Data,
						 true, From, State0),
    %% Note reply to channel will happen later when
    %% reply is recived from peer on the socket
    State = send_replies(Replies, State1),
    start_timeout(ChannelId, From, Timeout),
    handle_idle_timeout(State),
    {next_state, StateName, next_packet(State)};

handle_sync_event({request, ChannelId, Type, Data, Timeout}, From,  StateName, State0) ->
    {{replies, Replies}, State1} = handle_request(ChannelId, Type, Data,
						 true, From, State0),
    %% Note reply to channel will happen later when
    %% reply is recived from peer on the socket
    State = send_replies(Replies, State1),
    start_timeout(ChannelId, From, Timeout),
    handle_idle_timeout(State),
    {next_state, StateName, next_packet(State)};

handle_sync_event({global_request, Pid, _, _, _} = Request, From, StateName,
		  #state{connection_state =
			     #connection{channel_cache = Cache}} = State0) ->
    State1 = handle_global_request(Request, State0),
    Channel = ssh_channel:cache_find(Pid, Cache),
    State = add_request(true, Channel#channel.local_id, From, State1),
    {next_state, StateName, next_packet(State)};

handle_sync_event({data, ChannelId, Type, Data, Timeout}, From, StateName,
		  #state{connection_state = #connection{channel_cache = _Cache}
			 = Connection0} = State0) ->

    case ssh_connection:channel_data(ChannelId, Type, Data, Connection0, From) of
	{{replies, Replies}, Connection} ->
	    State = send_replies(Replies,  State0#state{connection_state = Connection}),
	    start_timeout(ChannelId, From, Timeout),
	    {next_state, StateName, next_packet(State)};
	{noreply, Connection} ->
	    start_timeout(ChannelId, From, Timeout),
	    {next_state, StateName, next_packet(State0#state{connection_state = Connection})}
    end;

handle_sync_event({eof, ChannelId}, _From, StateName,
		  #state{connection_state =
			     #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id, sent_close = false} ->
	    State = send_replies([{connection_reply,
			  ssh_connection:channel_eof_msg(Id)}], State0),
	    {reply, ok, StateName, next_packet(State)};
	_ ->
	    {reply, {error,closed}, StateName, State0}
    end;

handle_sync_event({open, ChannelPid, Type, InitialWindowSize, MaxPacketSize, Data, Timeout},
		  From,  StateName, #state{connection_state =
					       #connection{channel_cache = Cache}} = State0) ->
    erlang:monitor(process, ChannelPid),
    {ChannelId, State1}  = new_channel_id(State0),
    Msg = ssh_connection:channel_open_msg(Type, ChannelId,
					  InitialWindowSize,
					  MaxPacketSize, Data),
    State2 = send_replies([{connection_reply, Msg}], State1),
    Channel = #channel{type = Type,
		       sys = "none",
		       user = ChannelPid,
		       local_id = ChannelId,
		       recv_window_size = InitialWindowSize,
		       recv_packet_size = MaxPacketSize,
		       send_buf = queue:new()
		      },
    ssh_channel:cache_update(Cache, Channel),
    State = add_request(true, ChannelId, From, State2),
    start_timeout(ChannelId, From, Timeout),
    {next_state, StateName, next_packet(remove_timer_ref(State))};

handle_sync_event({send_window, ChannelId}, _From, StateName,
		  #state{connection_state =
			     #connection{channel_cache = Cache}} = State) ->
    Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{send_window_size = WinSize,
			 send_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {reply, Reply, StateName, next_packet(State)};

handle_sync_event({recv_window, ChannelId}, _From, StateName,
		  #state{connection_state = #connection{channel_cache = Cache}}
	     = State) ->

    Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{recv_window_size = WinSize,
			 recv_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {reply, Reply, StateName, next_packet(State)};

handle_sync_event({close, ChannelId}, _, StateName,
		  #state{connection_state =
			     #connection{channel_cache = Cache}} = State0) ->
    State =
	case ssh_channel:cache_lookup(Cache, ChannelId) of
	    #channel{remote_id = Id} = Channel ->
		State1 = send_replies([{connection_reply,
				       ssh_connection:channel_close_msg(Id)}], State0),
		ssh_channel:cache_update(Cache, Channel#channel{sent_close = true}),
		handle_idle_timeout(State1),
		State1;
	    undefined ->
		State0
	end,
    {reply, ok, StateName, next_packet(State)}.

%%--------------------------------------------------------------------
-spec handle_info({atom(), port(), binary()} | {atom(), port()} |
		  term (), state_name(), #state{}) -> gen_fsm_state_return().
%%--------------------------------------------------------------------

handle_info({Protocol, Socket, "SSH-" ++ _ = Version}, hello, 
	    #state{socket = Socket,
		   transport_protocol = Protocol} = State ) -> 
    event({version_exchange, Version}, hello, State);

handle_info({Protocol, Socket, Info}, hello, 
	    #state{socket = Socket,
		   transport_protocol = Protocol} = State) -> 
    event({info_line, Info}, hello, State);

handle_info({Protocol, Socket, Data}, StateName, 
	    #state{socket = Socket,
		   transport_protocol = Protocol,
		   ssh_params = Ssh0,
		   decoded_data_buffer = DecData0,
		   encoded_data_buffer = EncData0,
		   undecoded_packet_length = RemainingSshPacketLen0} = State0) ->
    Encoded = <<EncData0/binary, Data/binary>>,
    try ssh_transport:handle_packet_part(DecData0, Encoded, RemainingSshPacketLen0, Ssh0) 
    of
	{get_more, DecBytes, EncDataRest, RemainingSshPacketLen, Ssh1} ->
	    {next_state, StateName,
	     next_packet(State0#state{encoded_data_buffer = EncDataRest,
				      decoded_data_buffer = DecBytes,
				      undecoded_packet_length = RemainingSshPacketLen,
				      ssh_params = Ssh1})};
	{decoded, MsgBytes, EncDataRest, Ssh1} ->
	    generate_event(MsgBytes, StateName,
			   State0#state{ssh_params = Ssh1,
					%% Important to be set for
					%% next_packet
%%% FIXME: the following three seem to always be set in generate_event!
					decoded_data_buffer = <<>>,
					undecoded_packet_length = undefined,
					encoded_data_buffer = EncDataRest},
			   EncDataRest);
	{bad_mac, Ssh1} ->
	    DisconnectMsg = 
		    #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
					description = "Bad mac",
					language = ""},
	    handle_disconnect(DisconnectMsg, State0#state{ssh_params=Ssh1});

	{error, {exceeds_max_size,PacketLen}} ->
	    DisconnectMsg = 
		#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				    description = "Bad packet length " 
				    ++ integer_to_list(PacketLen),
				    language = ""},
	    handle_disconnect(DisconnectMsg, State0)
    catch
	_:_ ->
	    DisconnectMsg = 
		#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				    description = "Bad packet",
				    language = ""},
	    handle_disconnect(DisconnectMsg, State0)
    end;
		
handle_info({CloseTag, _Socket}, _StateName, 
	    #state{transport_close_tag = CloseTag,
		   ssh_params = #ssh{role = _Role, opts = _Opts}} = State) ->
    DisconnectMsg = 
	#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
			    description = "Connection closed",
			    language = "en"},
    handle_disconnect(DisconnectMsg, State);

handle_info({timeout, {_, From} = Request}, Statename,
	    #state{connection_state = #connection{requests = Requests} = Connection} = State) ->
    case lists:member(Request, Requests) of
	true ->
	    gen_fsm:reply(From, {error, timeout}),
	    {next_state, Statename,
	     State#state{connection_state =
			     Connection#connection{requests =
						       lists:delete(Request, Requests)}}};
	false ->
	    {next_state, Statename, State}
    end;

%%% Handle that ssh channels user process goes down
handle_info({'DOWN', _Ref, process, ChannelPid, _Reason}, Statename, State0) ->
    {{replies, Replies}, State1} = handle_channel_down(ChannelPid, State0),
    State = send_replies(Replies, State1),
    {next_state, Statename, next_packet(State)};

%%% So that terminate will be run when supervisor is shutdown
handle_info({'EXIT', _Sup, Reason}, _StateName, State) ->
    {stop, {shutdown, Reason}, State};

handle_info({check_cache, _ , _},
	    StateName, #state{connection_state =
		       #connection{channel_cache = Cache}} = State) ->
    {next_state, StateName, check_cache(State, Cache)};

handle_info(UnexpectedMessage, StateName, #state{opts = Opts,
						 ssh_params = SshParams} = State) ->
    case unexpected_fun(UnexpectedMessage, Opts, SshParams) of
	report ->
	    Msg = lists:flatten(
		    io_lib:format(
		      "Unexpected message '~p' received in state '~p'\n"
		      "Role: ~p\n"
		      "Peer: ~p\n"
		      "Local Address: ~p\n", [UnexpectedMessage, StateName,
					      SshParams#ssh.role, SshParams#ssh.peer,
					      proplists:get_value(address, SshParams#ssh.opts)])),
	    error_logger:info_report(Msg);

	skip ->
	    ok;

	Other ->
	    Msg = lists:flatten(
		    io_lib:format("Call to fun in 'unexpectedfun' failed:~n"
				  "Return: ~p\n"
				  "Message: ~p\n"
				  "Role: ~p\n"
				  "Peer: ~p\n"
				  "Local Address: ~p\n", [Other, UnexpectedMessage, 
							  SshParams#ssh.role, 
							  element(2,SshParams#ssh.peer),
							  proplists:get_value(address, SshParams#ssh.opts)]
				 )),

	    error_logger:error_report(Msg)
    end,
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
-spec terminate(Reason::term(), state_name(), #state{}) -> _.
%%--------------------------------------------------------------------
terminate(normal, _, #state{transport_cb = Transport,
			    connection_state = Connection,
			    socket = Socket}) ->
    terminate_subsystem(Connection),
    (catch Transport:close(Socket)),
    ok;

terminate({shutdown,{init,Reason}}, StateName, State) ->
    error_logger:info_report(io_lib:format("Erlang ssh in connection handler init: ~p~n",[Reason])),
    terminate(normal, StateName, State);

%% Terminated by supervisor
terminate(shutdown, StateName, #state{ssh_params = Ssh0} = State) ->
    DisconnectMsg = 
	#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
			    description = "Application shutdown",
			    language = "en"},
    {SshPacket, Ssh} = ssh_transport:ssh_packet(DisconnectMsg, Ssh0),
    send_msg(SshPacket, State),
    terminate(normal, StateName, State#state{ssh_params = Ssh});

terminate({shutdown, #ssh_msg_disconnect{} = Msg}, StateName,
	  #state{ssh_params = Ssh0} = State) ->
     {SshPacket, Ssh} = ssh_transport:ssh_packet(Msg, Ssh0),
    send_msg(SshPacket, State),
     terminate(normal, StateName, State#state{ssh_params = Ssh});

terminate({shutdown, _}, StateName, State) ->
    terminate(normal, StateName, State);

terminate(Reason, StateName, #state{ssh_params = Ssh0, starter = _Pid,
				   connection_state = Connection} = State) ->
    terminate_subsystem(Connection),
    log_error(Reason),
    DisconnectMsg = 
	#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
			    description = "Internal error",
			    language = "en"},
    {SshPacket, Ssh} = ssh_transport:ssh_packet(DisconnectMsg, Ssh0),
    send_msg(SshPacket, State),
    terminate(normal, StateName, State#state{ssh_params = Ssh}).


terminate_subsystem(#connection{system_supervisor = SysSup,
			       sub_system_supervisor = SubSysSup}) when is_pid(SubSysSup) ->
    ssh_system_sup:stop_subsystem(SysSup, SubSysSup);
terminate_subsystem(_) ->
    ok.

format_status(normal, [_, State]) ->
    [{data, [{"StateData", State}]}]; 
format_status(terminate, [_, State]) ->
    SshParams0 = (State#state.ssh_params),
    SshParams = SshParams0#ssh{c_keyinit = "***",
			       s_keyinit = "***",
			       send_mac_key = "***",
			       send_mac_size =  "***",
			       recv_mac_key = "***",
			       recv_mac_size = "***",
			       encrypt_keys = "***",
			       encrypt_ctx = "***",
			       decrypt_keys = "***",
			       decrypt_ctx = "***",
			       compress_ctx = "***",
			       decompress_ctx = "***",
			       shared_secret =  "***",
			       exchanged_hash =  "***",
			       session_id =  "***", 
			       keyex_key =  "***", 
			       keyex_info = "***", 
			       available_host_keys = "***"},
    [{data, [{"StateData", State#state{decoded_data_buffer = "***",
				       encoded_data_buffer =  "***",
				       key_exchange_init_msg = "***",
				       opts =  "***",
				       recbuf =  "***",
				       ssh_params = SshParams
				      }}]}].

%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), state_name(), Oldstate::term(), Extra::term()) ->
			 {ok, state_name(), #state{}}.
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
init_role(#state{role = client, opts = Opts} = State0) ->
    Pid = proplists:get_value(user_pid, Opts),
    TimerRef = get_idle_time(Opts),
    timer:apply_after(?REKEY_TIMOUT, gen_fsm, send_all_state_event, [self(), renegotiate]),
    timer:apply_after(?REKEY_DATA_TIMOUT, gen_fsm, send_all_state_event,
		       [self(), data_size]),
    State0#state{starter = Pid,
		 idle_timer_ref = TimerRef};
init_role(#state{role = server, opts = Opts, connection_state = Connection} = State) ->
    Sups = proplists:get_value(supervisors, Opts),
    Pid = proplists:get_value(user_pid, Opts),
    SystemSup = proplists:get_value(system_sup, Sups),
    SubSystemSup = proplists:get_value(subsystem_sup, Sups),
    ConnectionSup = proplists:get_value(connection_sup, Sups),
    Shell = proplists:get_value(shell, Opts),
    Exec = proplists:get_value(exec, Opts),
    CliSpec = proplists:get_value(ssh_cli, Opts, {ssh_cli, [Shell]}),
    State#state{starter = Pid, connection_state = Connection#connection{
						    cli_spec = CliSpec,
						    exec = Exec,
						    system_supervisor = SystemSup,
						    sub_system_supervisor = SubSystemSup,
						    connection_supervisor = ConnectionSup
						   }}.

get_idle_time(SshOptions) ->
    case proplists:get_value(idle_time, SshOptions) of
	infinity ->
	    infinity;
       _IdleTime -> %% We dont want to set the timeout on first connect
	    undefined
    end.

init_ssh(client = Role, Vsn, Version, Options, Socket) ->
    IOCb = case proplists:get_value(user_interaction, Options, true) of
	       true -> 
		   ssh_io;
	       false -> 
		   ssh_no_io
	   end,

    AuthMethods = proplists:get_value(auth_methods, Options, 
				      ?SUPPORTED_AUTH_METHODS),
    {ok, PeerAddr} = inet:peername(Socket),
    
    PeerName =  proplists:get_value(host, Options),
    KeyCb =  proplists:get_value(key_cb, Options, ssh_file),

    #ssh{role = Role,
	 c_vsn = Vsn,
	 c_version = Version,
	 key_cb = KeyCb,
	 io_cb = IOCb,
	 userauth_quiet_mode = proplists:get_value(quiet_mode, Options, false),
	 opts = Options,
	 userauth_supported_methods = AuthMethods,
	 peer = {PeerName, PeerAddr},
	 available_host_keys = supported_host_keys(Role, KeyCb, Options),
	 random_length_padding = proplists:get_value(max_random_length_padding, 
						     Options, 
						     (#ssh{})#ssh.random_length_padding)
	};

init_ssh(server = Role, Vsn, Version, Options, Socket) ->
    AuthMethods = proplists:get_value(auth_methods, Options, 
				      ?SUPPORTED_AUTH_METHODS),
    AuthMethodsAsList = string:tokens(AuthMethods, ","),
    {ok, PeerAddr} = inet:peername(Socket),
    KeyCb =  proplists:get_value(key_cb, Options, ssh_file),

    #ssh{role = Role,
	 s_vsn = Vsn,
	 s_version = Version,
	 key_cb = KeyCb,
	 io_cb = proplists:get_value(io_cb, Options, ssh_io),
	 opts = Options,
	 userauth_supported_methods = AuthMethods,
	 userauth_methods = AuthMethodsAsList,
	 kb_tries_left = 3,
	 peer = {undefined, PeerAddr},
	 available_host_keys = supported_host_keys(Role, KeyCb, Options),
	 random_length_padding = proplists:get_value(max_random_length_padding, 
						     Options, 
						     (#ssh{})#ssh.random_length_padding)
	 }.

supported_host_keys(client, _, Options) ->
    try
	case proplists:get_value(public_key, 
				 proplists:get_value(preferred_algorithms,Options,[])
				) of
	    undefined -> 
		ssh_transport:default_algorithms(public_key);
	    L ->
		L -- (L--ssh_transport:default_algorithms(public_key))
	end
    of
	[] ->
	    {stop, {shutdown, "No public key algs"}};
	Algs ->
	    [atom_to_list(A) || A<-Algs]
    catch
	exit:Reason ->
	    {stop, {shutdown, Reason}}
    end;
supported_host_keys(server, KeyCb, Options) ->
    [atom_to_list(A) || A <- proplists:get_value(public_key, 
						 proplists:get_value(preferred_algorithms,Options,[]),
						 ssh_transport:default_algorithms(public_key)
						),
			available_host_key(KeyCb, A, Options)
    ].

%% Alg :: atom()
available_host_key(KeyCb, Alg, Opts) ->
    element(1, catch KeyCb:host_key(Alg, Opts)) == ok.

send_msg(Msg, #state{socket = Socket, transport_cb = Transport}) ->
    Transport:send(Socket, Msg).

handle_version({2, 0} = NumVsn, StrVsn, Ssh0) -> 
    Ssh = counterpart_versions(NumVsn, StrVsn, Ssh0),
    {ok, Ssh};
handle_version(_,_,_) ->
    not_supported.

string_version(#ssh{role = client, c_version = Vsn}) ->
    Vsn;
string_version(#ssh{role = server, s_version = Vsn}) ->
    Vsn.

send_event(FsmPid, Event) ->
    gen_fsm:send_event(FsmPid, Event).

send_all_state_event(FsmPid, Event) ->
    gen_fsm:send_all_state_event(FsmPid, Event).

sync_send_all_state_event(FsmPid, Event) ->
    sync_send_all_state_event(FsmPid, Event, infinity).

sync_send_all_state_event(FsmPid, Event, Timeout) ->
    try gen_fsm:sync_send_all_state_event(FsmPid, Event, Timeout) of
	{closed, _Channel} ->
	    {error, closed};
	Result ->
	    Result
    catch
	exit:{noproc, _} ->
	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{{shutdown, _},_} ->
	    {error, closed}
    end.

%% simulate send_all_state_event(self(), Event) 
event(#ssh_msg_disconnect{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
event(#ssh_msg_ignore{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
event(#ssh_msg_debug{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
event(#ssh_msg_unimplemented{} = Event, StateName, State) ->
    handle_event(Event, StateName, State);
%% simulate send_event(self(), Event)
event(Event, StateName, State) ->
    try
	?MODULE:StateName(Event, State)
    catch
	throw:#ssh_msg_disconnect{} = DisconnectMsg ->
	    handle_disconnect(DisconnectMsg, State);
	throw:{ErrorToDisplay, #ssh_msg_disconnect{} = DisconnectMsg}  ->
	    handle_disconnect(DisconnectMsg, State, ErrorToDisplay);
	_C:_Error ->
	    handle_disconnect(#ssh_msg_disconnect{code = error_code(StateName),
						  description = "Invalid state",
						  language = "en"}, State)
    end.
error_code(key_exchange) ->
    ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED;
error_code(new_keys) ->
    ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED;
error_code(_) ->
    ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE.

generate_event(<<?BYTE(Byte), _/binary>> = Msg, StateName,
	       #state{
		  role = Role,
		  starter = User,
		  renegotiate = Renegotiation,
		  connection_state = Connection0} = State0, EncData)
  when  Byte == ?SSH_MSG_GLOBAL_REQUEST;
	Byte == ?SSH_MSG_REQUEST_SUCCESS;
	Byte == ?SSH_MSG_REQUEST_FAILURE;
	Byte == ?SSH_MSG_CHANNEL_OPEN;
	Byte == ?SSH_MSG_CHANNEL_OPEN_CONFIRMATION;
	Byte == ?SSH_MSG_CHANNEL_OPEN_FAILURE;
	Byte == ?SSH_MSG_CHANNEL_WINDOW_ADJUST;
	Byte == ?SSH_MSG_CHANNEL_DATA;
	Byte == ?SSH_MSG_CHANNEL_EXTENDED_DATA;
	Byte == ?SSH_MSG_CHANNEL_EOF;
	Byte == ?SSH_MSG_CHANNEL_CLOSE;
	Byte == ?SSH_MSG_CHANNEL_REQUEST;
	Byte == ?SSH_MSG_CHANNEL_SUCCESS;
	Byte == ?SSH_MSG_CHANNEL_FAILURE ->
    try
	ssh_message:decode(Msg)
    of
	ConnectionMsg ->
	    State1 = generate_event_new_state(State0, EncData),
	    try ssh_connection:handle_msg(ConnectionMsg, Connection0, Role) of
		{{replies, Replies0}, Connection} ->
		    if StateName == connected ->
			    Replies = Replies0,
			    State2  = State1;
		       true ->
			    {ConnReplies, Replies} =
				lists:splitwith(fun not_connected_filter/1, Replies0),
			    Q = State1#state.event_queue ++ ConnReplies,
			    State2  = State1#state{ event_queue = Q }
		    end,
		    State = send_replies(Replies,  State2#state{connection_state = Connection}),
		    {next_state, StateName, next_packet(State)};
		{noreply, Connection} ->
		    {next_state, StateName, next_packet(State1#state{connection_state = Connection})};
		{disconnect, {_, Reason}, {{replies, Replies}, Connection}} when
		      Role == client andalso ((StateName =/= connected) and (not Renegotiation)) ->
		    State = send_replies(Replies,  State1#state{connection_state = Connection}),
		    User ! {self(), not_connected, Reason},
		    {stop, {shutdown, normal},
		     next_packet(State#state{connection_state = Connection})};
		{disconnect, _Reason, {{replies, Replies}, Connection}} ->
		    State = send_replies(Replies,  State1#state{connection_state = Connection}),
		    {stop, {shutdown, normal}, State#state{connection_state = Connection}}
	    catch
		_:Error ->
		    {disconnect, _Reason, {{replies, Replies}, Connection}} =
			ssh_connection:handle_msg(
			  #ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
					      description = "Internal error",
					      language = "en"}, Connection0, Role),
		    State = send_replies(Replies,  State1#state{connection_state = Connection}),
		    {stop, {shutdown, Error}, State#state{connection_state = Connection}}
	    end

    catch
	_:_ ->
	    handle_disconnect(
	      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				  description = "Bad packet received",
				  language = ""}, State0)
    end;

generate_event(Msg, StateName, State0, EncData) ->
    try 
	Event = ssh_message:decode(set_prefix_if_trouble(Msg,State0)),
	State = generate_event_new_state(State0, EncData),
	case Event of
	    #ssh_msg_kexinit{} ->
		%% We need payload for verification later.
		event({Event, Msg}, StateName, State);
	    _ ->
		event(Event, StateName, State)
	end
    catch 
	_C:_E  ->
	    DisconnectMsg =
		#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR, 
				    description = "Encountered unexpected input",
				    language = "en"},
	    handle_disconnect(DisconnectMsg, State0)   
    end.		
	    

set_prefix_if_trouble(Msg = <<?BYTE(Op),_/binary>>, #state{ssh_params=SshParams}) 
  when Op == 30;
       Op == 31
       ->
    case catch atom_to_list(kex(SshParams)) of
	"ecdh-sha2-" ++ _ -> 
	    <<"ecdh",Msg/binary>>;
	"diffie-hellman-group-exchange-" ++ _ ->
	    <<"dh_gex",Msg/binary>>;
	"diffie-hellman-group" ++ _ ->
	    <<"dh",Msg/binary>>;
	_ -> 
	    Msg
    end;
set_prefix_if_trouble(Msg, _) ->
    Msg.

kex(#ssh{algorithms=#alg{kex=Kex}}) -> Kex;
kex(_) -> undefined.


handle_request(ChannelPid, ChannelId, Type, Data, WantReply, From,
	       #state{connection_state =
		      #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    update_sys(Cache, Channel, Type, ChannelPid),
	    Msg = ssh_connection:channel_request_msg(Id, Type,
						     WantReply, Data),
	    Replies = [{connection_reply, Msg}],
	    State = add_request(WantReply, ChannelId, From, State0),
	    {{replies, Replies}, State};
	undefined ->
	    {{replies, []}, State0}
    end.

handle_request(ChannelId, Type, Data, WantReply, From,
	       #state{connection_state =
		      #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id}  ->
	    Msg = ssh_connection:channel_request_msg(Id, Type,
						     WantReply, Data),
	    Replies = [{connection_reply, Msg}],
	    State = add_request(WantReply, ChannelId, From, State0),
	    {{replies, Replies}, State};
	undefined ->
	    {{replies, []}, State0}
    end.

handle_global_request({global_request, ChannelPid,
		       "tcpip-forward" = Type, WantReply,
		       <<?UINT32(IPLen),
			IP:IPLen/binary, ?UINT32(Port)>> = Data},
		      #state{connection_state =
				 #connection{channel_cache = Cache}
			     = Connection0} = State) ->
    ssh_channel:cache_update(Cache, #channel{user = ChannelPid,
					     type = "forwarded-tcpip",
					     sys = none}),
    Connection = ssh_connection:bind(IP, Port, ChannelPid, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_replies([{connection_reply, Msg}],  State#state{connection_state = Connection});

handle_global_request({global_request, _Pid, "cancel-tcpip-forward" = Type,
		       WantReply, <<?UINT32(IPLen),
				   IP:IPLen/binary, ?UINT32(Port)>> = Data},
		      #state{connection_state = Connection0} = State) ->
    Connection = ssh_connection:unbind(IP, Port, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_replies([{connection_reply, Msg}], State#state{connection_state = Connection});

handle_global_request({global_request, _, "cancel-tcpip-forward" = Type,
		       WantReply, Data}, State) ->
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_replies([{connection_reply, Msg}], State).

handle_idle_timeout(#state{opts = Opts}) ->
    case proplists:get_value(idle_time, Opts, infinity) of
	infinity ->
	    ok;
	IdleTime ->
	    erlang:send_after(IdleTime, self(), {check_cache, [], []})
    end.

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

generate_event_new_state(#state{ssh_params = 
				#ssh{recv_sequence = SeqNum0} 
				= Ssh} = State, EncData) ->
    SeqNum = ssh_transport:next_seqnum(SeqNum0),
    State#state{ssh_params = Ssh#ssh{recv_sequence = SeqNum},
		decoded_data_buffer = <<>>,
		encoded_data_buffer = EncData, 
		undecoded_packet_length = undefined}.

next_packet(#state{decoded_data_buffer = <<>>,
		   encoded_data_buffer = Buff,
		   ssh_params = #ssh{decrypt_block_size = BlockSize},
		   socket = Socket,
		   transport_protocol = Protocol} = State) when Buff =/= <<>> ->
    case  size(Buff) >= erlang:max(8, BlockSize) of
	true ->
	    %% Enough data from the next packet has been received to
	    %% decode the length indicator, fake a socket-recive
	    %% message so that the data will be processed
	    self() ! {Protocol, Socket, <<>>};
	false ->
	    inet:setopts(Socket, [{active, once}])
    end,
    State;

next_packet(#state{socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    State.

after_new_keys(#state{renegotiate = true} = State) ->
    State1 = State#state{renegotiate = false, event_queue = []},
    lists:foldr(fun after_new_keys_events/2, {next_state, connected, State1}, State#state.event_queue);
after_new_keys(#state{renegotiate = false, 
		      ssh_params = #ssh{role = client} = Ssh0} = State) ->
    {Msg, Ssh} = ssh_auth:service_request_msg(Ssh0),
    send_msg(Msg, State),
    {next_state, service_request, State#state{ssh_params = Ssh}};
after_new_keys(#state{renegotiate = false,  
		      ssh_params = #ssh{role = server}} = State) ->
    {next_state, service_request, State}.

after_new_keys_events({sync, _Event, From}, {stop, _Reason, _StateData}=Terminator) ->
    gen_fsm:reply(From, {error, closed}),
    Terminator;
after_new_keys_events(_, {stop, _Reason, _StateData}=Terminator) ->
    Terminator;
after_new_keys_events({sync, Event, From}, {next_state, StateName, StateData}) ->
    case handle_sync_event(Event, From, StateName, StateData) of
	{reply, Reply, NextStateName, NewStateData} ->
	    gen_fsm:reply(From, Reply),
	    {next_state, NextStateName, NewStateData};
	{next_state, NextStateName, NewStateData}->
	    {next_state, NextStateName, NewStateData};
	{stop, Reason, Reply, NewStateData} ->
	    gen_fsm:reply(From, Reply),
	    {stop, Reason, NewStateData}
    end;
after_new_keys_events({event, Event}, {next_state, StateName, StateData}) ->
    case handle_event(Event, StateName, StateData) of
	{next_state, NextStateName, NewStateData}->
	    {next_state, NextStateName, NewStateData};
	{stop, Reason, NewStateData} ->
	    {stop, Reason, NewStateData}
    end;
after_new_keys_events({connection_reply, _Data} = Reply, {StateName, State}) ->
    NewState = send_replies([Reply], State),
    {next_state, StateName, NewState}.


handle_disconnect(DisconnectMsg, State) ->
    handle_disconnect(own, DisconnectMsg, State).

handle_disconnect(#ssh_msg_disconnect{} = DisconnectMsg, State, Error) ->
    handle_disconnect(own, DisconnectMsg, State, Error);
handle_disconnect(Type,  #ssh_msg_disconnect{description = Desc} = Msg, #state{connection_state = Connection0,									role = Role} = State0) ->
    {disconnect, _, {{replies, Replies}, Connection}} = ssh_connection:handle_msg(Msg, Connection0, Role),
    State = send_replies(disconnect_replies(Type, Msg, Replies), State0),
    disconnect_fun(Desc, State#state.opts),
    {stop, {shutdown, Desc}, State#state{connection_state = Connection}}.

handle_disconnect(Type, #ssh_msg_disconnect{description = Desc} = Msg, #state{connection_state = Connection0,
									      role = Role} = State0, ErrorMsg) ->
    {disconnect, _, {{replies, Replies}, Connection}} = ssh_connection:handle_msg(Msg, Connection0, Role),
    State = send_replies(disconnect_replies(Type, Msg, Replies), State0),
    disconnect_fun(Desc, State#state.opts),
    {stop, {shutdown, {Desc, ErrorMsg}}, State#state{connection_state = Connection}}.

disconnect_replies(own, Msg, Replies) ->
    [{connection_reply, Msg} | Replies];
disconnect_replies(peer, _, Replies) ->
    Replies.

counterpart_versions(NumVsn, StrVsn, #ssh{role = server} = Ssh) ->
    Ssh#ssh{c_vsn = NumVsn , c_version = StrVsn};
counterpart_versions(NumVsn, StrVsn, #ssh{role = client} = Ssh) ->
    Ssh#ssh{s_vsn = NumVsn , s_version = StrVsn}.

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.
connected_fun(User, PeerAddr, Method, Opts) ->
    case proplists:get_value(connectfun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(User, PeerAddr, Method)
    end.

retry_fun(_, _, undefined, _) ->
    ok;

retry_fun(User, PeerAddr, {error, Reason}, Opts) ->
    case proplists:get_value(failfun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    do_retry_fun(Fun, User, PeerAddr, Reason)
    end;

retry_fun(User, PeerAddr, Reason, Opts) ->
    case proplists:get_value(infofun, Opts) of
	undefined ->
	    ok;
	Fun  ->
	    do_retry_fun(Fun, User, PeerAddr, Reason)
    end.

do_retry_fun(Fun, User, PeerAddr, Reason) ->
    case erlang:fun_info(Fun, arity) of
	{arity, 2} -> %% Backwards compatible
	    catch Fun(User, Reason);
	{arity, 3} ->
	    catch Fun(User, PeerAddr, Reason)
    end.

ssh_info([], _State, Acc) ->
    Acc;
ssh_info([client_version | Rest], #state{ssh_params = #ssh{c_vsn = IntVsn,
							   c_version = StringVsn}} = State, Acc) ->
    ssh_info(Rest, State, [{client_version, {IntVsn, StringVsn}} | Acc]);

ssh_info([server_version | Rest], #state{ssh_params =#ssh{s_vsn = IntVsn,
							  s_version = StringVsn}} = State, Acc) ->
    ssh_info(Rest, State, [{server_version, {IntVsn, StringVsn}} | Acc]);
ssh_info([peer | Rest],  #state{ssh_params = #ssh{peer = Peer}} = State, Acc) ->
    ssh_info(Rest, State, [{peer, Peer} | Acc]);
ssh_info([sockname | Rest], #state{socket = Socket} = State, Acc) ->
    {ok, SockName} = inet:sockname(Socket),
   ssh_info(Rest, State, [{sockname, SockName}|Acc]);
ssh_info([user | Rest], #state{auth_user = User} = State, Acc) ->
    ssh_info(Rest, State, [{user, User}|Acc]);
ssh_info([ _ | Rest], State, Acc) ->
    ssh_info(Rest, State, Acc).

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

log_error(Reason) ->
    Report = io_lib:format("Erlang ssh connection handler failed with reason: "
			   "~p ~n, Stacktrace: ~p ~n",
			   [Reason,  erlang:get_stacktrace()]),
    error_logger:error_report(Report),
    "Internal error".

not_connected_filter({connection_reply, _Data}) ->
    true;
not_connected_filter(_) ->
    false.

send_replies([], State) ->
    State;
send_replies([{connection_reply, Data} | Rest], #state{ssh_params = Ssh0} = State) ->
    {Packet, Ssh} = ssh_transport:ssh_packet(Data, Ssh0),
    send_msg(Packet, State),
    send_replies(Rest, State#state{ssh_params = Ssh});
send_replies([Msg | Rest], State) ->
    catch send_reply(Msg),
    send_replies(Rest, State).

send_reply({channel_data, Pid, Data}) ->
    Pid ! {ssh_cm, self(), Data};
send_reply({channel_requst_reply, From, Data}) ->
    gen_fsm:reply(From, Data);
send_reply({flow_control, Cache, Channel, From, Msg}) ->
    ssh_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
    gen_fsm:reply(From, Msg);
send_reply({flow_control, From, Msg}) ->
    gen_fsm:reply(From, Msg).

disconnect_fun({disconnect,Msg}, Opts) ->
    disconnect_fun(Msg, Opts);
disconnect_fun(_, undefined) ->
    ok;
disconnect_fun(Reason, Opts) ->
    case proplists:get_value(disconnectfun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(Reason)
     end.

unexpected_fun(UnexpectedMessage, Opts, #ssh{peer={_,Peer}}) ->
    case proplists:get_value(unexpectedfun, Opts) of
	undefined ->
	    report;
	Fun ->
	    catch Fun(UnexpectedMessage, Peer) 
    end.


check_cache(#state{opts = Opts} = State, Cache) ->
    %% Check the number of entries in Cache
    case proplists:get_value(size, ets:info(Cache)) of
	0 ->
	    case proplists:get_value(idle_time, Opts, infinity) of
		infinity ->
		    State;
		Time ->
		    handle_idle_timer(Time, State)
	    end;
	_ ->
	    State
    end.

handle_idle_timer(Time, #state{idle_timer_ref = undefined} = State) ->
    TimerRef = erlang:send_after(Time, self(), {'EXIT', [], "Timeout"}),
    State#state{idle_timer_ref=TimerRef};
handle_idle_timer(_, State) ->
    State.

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

socket_control(Socket, Pid, Transport) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    send_event(Pid, socket_control);
	{error, Reason}	->
	    {error, Reason}
    end.

handshake(Pid, Ref, Timeout) ->
    receive
	ssh_connected ->
	    erlang:demonitor(Ref),
	    {ok, Pid};
	{Pid, not_connected, Reason} ->
	    {error, Reason};
	{Pid, user_password} ->
	    Pass = io:get_password(),
	    Pid ! Pass,
	    handshake(Pid, Ref, Timeout);
	{Pid, question} ->
	    Answer = io:get_line(""),
	    Pid ! Answer,
	    handshake(Pid, Ref, Timeout);
	{'DOWN', _, process, Pid, {shutdown, Reason}} ->
	    {error, Reason};
	{'DOWN', _, process, Pid, Reason} ->
	    {error, Reason}
    after Timeout ->
	    stop(Pid),
	    {error, timeout}
    end.

start_timeout(_,_, infinity) ->
    ok;
start_timeout(Channel, From, Time) ->
    erlang:send_after(Time, self(), {timeout, {Channel, From}}).

getopt(Opt, Socket) ->
    case inet:getopts(Socket, [Opt]) of
	{ok, [{Opt, Value}]} ->
	    {ok, Value};
	Other ->
	    {error, {unexpected_getopts_return, Other}}
    end.
		
