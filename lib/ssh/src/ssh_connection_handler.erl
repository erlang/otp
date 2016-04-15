%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-behaviour(gen_statem).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_auth.hrl").
-include("ssh_connect.hrl").

%%====================================================================
%%% Exports
%%====================================================================

%%% Start and stop
-export([start_link/3,
	 stop/1
	]).

%%% Internal application API
-export([start_connection/4,
	 open_channel/6,
	 request/6, request/7,
	 reply_request/3, 
	 global_request/4,
	 send/5,
	 send_eof/2,
	 info/1, info/2,
	 connection_info/2,
	 channel_info/3,
	 adjust_window/3, close/2,
	 disconnect/1, disconnect/2,
	 get_print_info/1
	]).

%%% Behaviour callbacks
-export([handle_event/4, terminate/3, format_status/2, code_change/4]).

%%% Exports not intended to be used :)
-export([init_connection_handler/3,	   % proc_lib:spawn needs this
	 init_ssh_record/3,		   % Export intended for low level protocol test suites
	 renegotiate/1, renegotiate_data/1 % Export intended for test cases
	]).

%%====================================================================
%% Start / stop
%%====================================================================
%%--------------------------------------------------------------------
-spec start_link(role(),
		 inet:socket(),
		 proplists:proplist()
		) -> {ok, pid()}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
start_link(Role, Socket, Options) ->
    {ok, proc_lib:spawn_link(?MODULE, init_connection_handler, [Role, Socket, Options])}.


%%--------------------------------------------------------------------
-spec stop(connection_ref()
	  ) -> ok | {error, term()}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
stop(ConnectionHandler)->
    case call(ConnectionHandler, stop) of
       {error, closed} ->
	    ok;
	Other ->
	    Other
    end.

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec start_connection(role(),
		       inet:socket(),
		       proplists:proplist(),
		       timeout()
		      ) -> {ok, connection_ref()} | {error, term()}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
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

%%--------------------------------------------------------------------
%%% Some other module has decided to disconnect.
-spec disconnect(#ssh_msg_disconnect{}) -> no_return().
-spec disconnect(#ssh_msg_disconnect{}, iodata()) -> no_return().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
disconnect(Msg = #ssh_msg_disconnect{}) ->
    throw({keep_state_and_data, 
	   [{next_event, internal, {disconnect, Msg, Msg#ssh_msg_disconnect.description}}]}).

disconnect(Msg = #ssh_msg_disconnect{}, ExtraInfo) ->
    throw({keep_state_and_data,
	   [{next_event, internal, {disconnect, Msg, {Msg#ssh_msg_disconnect.description,ExtraInfo}}}]}).


%%--------------------------------------------------------------------
-spec open_channel(connection_ref(), 
		   string(),
		   binary(),
		   pos_integer(),
		   pos_integer(),
		   timeout()
		  ) -> {ok, channel_id()} | {error, term()}.
		   
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
open_channel(ConnectionHandler, 
	     ChannelType, ChannelSpecificData, InitialWindowSize, MaxPacketSize, 
	     Timeout) ->
    call(ConnectionHandler,
	 {open, self(), 
	  ChannelType, InitialWindowSize, MaxPacketSize, ChannelSpecificData,
	  Timeout}).

%%--------------------------------------------------------------------
-spec request(connection_ref(),
	      pid(),
	      channel_id(),
	      string(),
	      boolean(),
	      iodata(),
	      timeout()
	     ) -> success | failure | ok | {error,timeout}.

-spec request(connection_ref(),
	      channel_id(),
	      string(),
	      boolean(),
	      iodata(),
	      timeout()
	     ) -> success | failure | ok | {error,timeout}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
request(ConnectionHandler, ChannelPid, ChannelId, Type, true, Data, Timeout) ->
    call(ConnectionHandler, {request, ChannelPid, ChannelId, Type, Data, Timeout});
request(ConnectionHandler, ChannelPid, ChannelId, Type, false, Data, _) ->
    cast(ConnectionHandler, {request, ChannelPid, ChannelId, Type, Data}).

request(ConnectionHandler, ChannelId, Type, true, Data, Timeout) ->
    call(ConnectionHandler, {request, ChannelId, Type, Data, Timeout});
request(ConnectionHandler, ChannelId, Type, false, Data, _) ->
    cast(ConnectionHandler, {request, ChannelId, Type, Data}).

%%--------------------------------------------------------------------
-spec reply_request(connection_ref(),
		    success | failure,
		    channel_id()
		   ) -> ok.
			   
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
reply_request(ConnectionHandler, Status, ChannelId) ->
    cast(ConnectionHandler, {reply_request, Status, ChannelId}).

%%--------------------------------------------------------------------
-spec global_request(connection_ref(),
		     string(),
		     boolean(),
		     iolist()
		    ) -> ok | error.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
global_request(ConnectionHandler, Type, true = Reply, Data) ->
    case call(ConnectionHandler, {global_request, self(), Type, Reply, Data}) of
	{ssh_cm, ConnectionHandler, {success, _}} ->
	    ok;
	{ssh_cm, ConnectionHandler, {failure, _}} ->
	    error
    end;
global_request(ConnectionHandler, Type, false = Reply, Data) ->
    cast(ConnectionHandler, {global_request, self(), Type, Reply, Data}).

%%--------------------------------------------------------------------
-spec send(connection_ref(),
	   channel_id(),
	   non_neg_integer(),
	   iodata(),
	   timeout()
	  ) -> ok | {error, timeout|closed}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
send(ConnectionHandler, ChannelId, Type, Data, Timeout) ->
    call(ConnectionHandler, {data, ChannelId, Type, Data, Timeout}).

%%--------------------------------------------------------------------
-spec send_eof(connection_ref(),
	       channel_id()
	      ) -> ok | {error,closed}.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
send_eof(ConnectionHandler, ChannelId) ->
    call(ConnectionHandler, {eof, ChannelId}).

%%--------------------------------------------------------------------
-spec info(connection_ref()
	  ) -> [ #channel{} ].

-spec info(connection_ref(),
	   pid()
	  ) -> [ #channel{} ].
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
info(ConnectionHandler) ->
    info(ConnectionHandler, {info, all}).

info(ConnectionHandler, ChannelProcess) ->
    call(ConnectionHandler, {info, ChannelProcess}).

%%--------------------------------------------------------------------
-type local_sock_info() :: {inet:ip_address(), non_neg_integer()} | string().
-type peer_sock_info()  :: {inet:ip_address(), non_neg_integer()} | string().
-type state_info() :: iolist().

-spec get_print_info(connection_ref()
		    ) -> {{local_sock_info(), peer_sock_info()},
			  state_info()
			 }.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
get_print_info(ConnectionHandler) ->
    call(ConnectionHandler, get_print_info, 1000).

%%--------------------------------------------------------------------
-spec connection_info(connection_ref(),
		      [atom()]
		     ) -> proplists:proplist().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
connection_info(ConnectionHandler, Options) ->
    call(ConnectionHandler, {connection_info, Options}).

%%--------------------------------------------------------------------
-spec channel_info(connection_ref(),
		   channel_id(),
		   [atom()]
		  ) -> proplists:proplist().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
channel_info(ConnectionHandler, ChannelId, Options) ->
    call(ConnectionHandler, {channel_info, ChannelId, Options}).

%%--------------------------------------------------------------------
-spec adjust_window(connection_ref(),
		    channel_id(),
		    integer()
		   ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
adjust_window(ConnectionHandler, Channel, Bytes) ->
    cast(ConnectionHandler, {adjust_window, Channel, Bytes}).

%%--------------------------------------------------------------------
-spec renegotiate(connection_ref()
		 ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
renegotiate(ConnectionHandler) ->
    cast(ConnectionHandler, renegotiate).

%%--------------------------------------------------------------------
-spec renegotiate_data(connection_ref()
		      ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
renegotiate_data(ConnectionHandler) ->
    cast(ConnectionHandler, data_size).

%%--------------------------------------------------------------------
-spec close(connection_ref(),
	    channel_id()
	   ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
close(ConnectionHandler, ChannelId) ->
    case call(ConnectionHandler, {close, ChannelId}) of
	ok ->
	    ok;
	{error, closed} ->
	    ok
    end.

%%====================================================================
%% Internal process state
%%====================================================================
-record(data, {
	  starter                 :: pid(),
	  auth_user               :: string(),
	  connection_state        :: #connection{},
	  latest_channel_id = 0   :: non_neg_integer(),
	  idle_timer_ref          :: infinity | reference(),
	  transport_protocol      :: atom(),	% ex: tcp
	  transport_cb            :: atom(),	% ex: gen_tcp
	  transport_close_tag     :: atom(),	% ex: tcp_closed
	  ssh_params              :: #ssh{},
	  socket                  :: inet:socket(),
	  decoded_data_buffer     :: binary(),
	  encoded_data_buffer     :: binary(),
	  undecoded_packet_length :: non_neg_integer(),
	  key_exchange_init_msg   :: #ssh_msg_kexinit{},
	  last_size_rekey = 0     :: non_neg_integer(),
	  event_queue = []        :: list(),
	  opts                    :: proplists:proplist(),
	  recbuf                  :: pos_integer()
	 }).

%%====================================================================
%% Intitialisation
%%====================================================================
%%--------------------------------------------------------------------
-spec init_connection_handler(role(),
			      inet:socket(),
			      proplists:proplist()
			     ) -> no_return().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
init_connection_handler(Role, Socket, Opts) ->
    process_flag(trap_exit, true),
    S0 = init_process_state(Role, Socket, Opts),
    try
	{Protocol, Callback, CloseTag} =
	    proplists:get_value(transport, Opts, {tcp, gen_tcp, tcp_closed}),
	S0#data{ssh_params = init_ssh_record(Role, Socket, Opts),
		 transport_protocol = Protocol,
		 transport_cb = Callback,
		 transport_close_tag = CloseTag
		}
    of
	S -> gen_statem:enter_loop(?MODULE,
				   [], %%[{debug,[trace,log,statistics,debug]} || Role==server],
				   handle_event_function,
				   {hello,Role},
				   S,
				   [])
    catch
	_:Error -> init_error(Error, S0)
    end.


init_error(Error, S) ->
    gen_statem:enter_loop(?MODULE, [], handle_event_function, {init_error,Error}, S, []).


init_process_state(Role, Socket, Opts) ->
    S = #data{connection_state =
		   C = #connection{channel_cache = ssh_channel:cache_create(),
				   channel_id_seed = 0,
				   port_bindings = [],
				   requests = [],
				   options = Opts},
	       starter = proplists:get_value(user_pid, Opts),
	       socket = Socket,
	       decoded_data_buffer = <<>>,
	       encoded_data_buffer = <<>>,
	       opts = Opts
	      },
    case Role of
	client ->
	    TimerRef = get_idle_time(Opts),
	    timer:apply_after(?REKEY_TIMOUT, gen_statem, cast, [self(), renegotiate]),
	    timer:apply_after(?REKEY_DATA_TIMOUT, gen_statem, cast, [self(), data_size]),
	    S#data{idle_timer_ref = TimerRef};

	server ->
	    S#data{connection_state = init_connection(Role, C, Opts)}
    end.


init_connection(server, C = #connection{}, Opts) ->
    Sups = proplists:get_value(supervisors, Opts),
    SystemSup = proplists:get_value(system_sup, Sups),
    SubSystemSup = proplists:get_value(subsystem_sup, Sups),
    ConnectionSup = proplists:get_value(connection_sup, Sups),
    Shell = proplists:get_value(shell, Opts),
    Exec = proplists:get_value(exec, Opts),
    CliSpec = proplists:get_value(ssh_cli, Opts, {ssh_cli, [Shell]}),
    C#connection{cli_spec = CliSpec,
		 exec = Exec,
		 system_supervisor = SystemSup,
		 sub_system_supervisor = SubSystemSup,
		 connection_supervisor = ConnectionSup
		}.


init_ssh_record(Role, Socket, Opts) ->
    {ok, PeerAddr} = inet:peername(Socket),
    KeyCb = proplists:get_value(key_cb, Opts, ssh_file),
    AuthMethods = proplists:get_value(auth_methods, Opts, ?SUPPORTED_AUTH_METHODS),
    S0 = #ssh{role = Role,
	      key_cb = KeyCb,
	      opts = Opts,
	      userauth_supported_methods = AuthMethods,
	      available_host_keys = supported_host_keys(Role, KeyCb, Opts),
	      random_length_padding = proplists:get_value(max_random_length_padding,
							  Opts,
							  (#ssh{})#ssh.random_length_padding)
	   },

    {Vsn, Version} = ssh_transport:versions(Role, Opts),
    case Role of
	client ->
	    PeerName =  proplists:get_value(host, Opts),
	    S0#ssh{c_vsn = Vsn,
		   c_version = Version,
		   io_cb = case proplists:get_value(user_interaction, Opts, true) of
			       true ->  ssh_io;
			       false -> ssh_no_io
			   end,
		   userauth_quiet_mode = proplists:get_value(quiet_mode, Opts, false),
		   peer = {PeerName, PeerAddr}
		  };

	server ->
	    S0#ssh{s_vsn = Vsn,
		   s_version = Version,
		   io_cb = proplists:get_value(io_cb, Opts, ssh_io),
		   userauth_methods = string:tokens(AuthMethods, ","),
		   kb_tries_left = 3,
		   peer = {undefined, PeerAddr}
		  }
    end.



%%====================================================================
%% gen_statem callbacks
%%====================================================================
%%--------------------------------------------------------------------

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

%%% ######## Error in the initialiasation ####

handle_event(_, _Event, {init_error,{badmatch,{error,enotconn}}}, _State) ->
    %% Handles the abnormal sequence:
    %%    SYN->
    %%            <-SYNACK
    %%    ACK->
    %%    RST->
    {stop, {shutdown,"TCP connenction to server was prematurely closed by the client"}};

handle_event(_, _Event, {init_error,OtherError}, _State) -> 
    {stop, {shutdown,{init,OtherError}}};


%%% ######## {hello, client|server} ####
handle_event(_, socket_control, StateName={hello,_}, S=#data{socket=Socket, 
							      ssh_params=Ssh}) ->
    VsnMsg = ssh_transport:hello_version_msg(string_version(Ssh)),
    send_bytes(VsnMsg, S),
    case getopt(recbuf, Socket) of
	{ok, Size} ->
	    inet:setopts(Socket, [{packet, line}, {active, once}, {recbuf, ?MAX_PROTO_VERSION}, {nodelay,true}]),
	    {next_state, StateName, S#data{recbuf=Size}};
	{error, Reason} ->
	    {stop, {shutdown,Reason}}
    end;

handle_event(_, {info_line,_Line}, StateName={hello,client}, S=#data{socket=Socket}) ->
    %% The server may send info lines before the version_exchange
    inet:setopts(Socket, [{active, once}]),
    {next_state, StateName, S};

handle_event(_, {info_line,_Line}, {hello,server}, S) ->
    %% as openssh
    send_bytes("Protocol mismatch.", S),
    {stop, {shutdown,"Protocol mismatch in version exchange."}};

handle_event(_, {version_exchange,Version}, {hello,Role}, S=#data{ssh_params = Ssh0,
								   socket = Socket,
								   recbuf = Size}) ->
    {NumVsn, StrVsn} = ssh_transport:handle_hello_version(Version),
    case handle_version(NumVsn, StrVsn, Ssh0) of
	{ok, Ssh1} ->
	    inet:setopts(Socket, [{packet,0}, {mode,binary}, {active, once}, {recbuf, Size}]),
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh1),
	    send_bytes(SshPacket, S),
	    {next_state, {kexinit,Role,init}, S#data{ssh_params = Ssh,
						      key_exchange_init_msg = KeyInitMsg}};
	not_supported ->
	    disconnect(
	      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,
				  description = ["Protocol version ",StrVsn," not supported"]},
	      {next_state, {hello,Role}, S})
    end;

		  
%%% ######## {kexinit, client|server, init|renegotiate} ####

handle_event(_, {#ssh_msg_kexinit{} = Kex, Payload}, {kexinit,client,ReNeg},
	     S = #data{ssh_params = Ssh0,
			key_exchange_init_msg = OwnKex}) ->
    Ssh1 = ssh_transport:key_init(server, Ssh0, Payload), % Yes, *server*
    {ok, NextKexMsg, Ssh} = ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1),
    send_bytes(NextKexMsg, S),
    {next_state, {key_exchange,client,ReNeg}, S#data{ssh_params = Ssh}};

handle_event(_, {#ssh_msg_kexinit{} = Kex, Payload}, {kexinit,server,ReNeg},
	     S = #data{ssh_params = Ssh0,
			key_exchange_init_msg = OwnKex}) ->
    Ssh1 = ssh_transport:key_init(client, Ssh0, Payload), % Yes, *client*
    {ok, Ssh} = ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1),
    {next_state, {key_exchange,server,ReNeg}, S#data{ssh_params = Ssh}};


%%% ######## {key_exchange, client|server, init|renegotiate} ####

handle_event(_, #ssh_msg_kexdh_init{} = Msg, {key_exchange,server,ReNeg},
	     S = #data{ssh_params = Ssh0}) ->
    {ok, KexdhReply, Ssh1} = ssh_transport:handle_kexdh_init(Msg, Ssh0),
    send_bytes(KexdhReply, S),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    send_bytes(NewKeys, S),
    {next_state, {new_keys,server,ReNeg}, S#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_kexdh_reply{} = Msg, {key_exchange,client,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, NewKeys, Ssh} = ssh_transport:handle_kexdh_reply(Msg, Ssh0),
    send_bytes(NewKeys, State),
    {next_state, {new_keys,client,ReNeg}, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_kex_dh_gex_request{} = Msg, {key_exchange,server,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, GexGroup, Ssh} = ssh_transport:handle_kex_dh_gex_request(Msg, Ssh0),
    send_bytes(GexGroup, State),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_kex_dh_gex_request_old{} = Msg, {key_exchange,server,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, GexGroup, Ssh} = ssh_transport:handle_kex_dh_gex_request(Msg, Ssh0),
    send_bytes(GexGroup, State),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_kex_dh_gex_group{} = Msg, {key_exchange,client,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, KexGexInit, Ssh} = ssh_transport:handle_kex_dh_gex_group(Msg, Ssh0),
    send_bytes(KexGexInit, State),
    {next_state, {key_exchange_dh_gex_reply,client,ReNeg}, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_kex_ecdh_init{} = Msg, {key_exchange,server,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, KexEcdhReply, Ssh1} = ssh_transport:handle_kex_ecdh_init(Msg, Ssh0),
    send_bytes(KexEcdhReply, State),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    send_bytes(NewKeys, State),
    {next_state, {new_keys,server,ReNeg}, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_kex_ecdh_reply{} = Msg, {key_exchange,client,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, NewKeys, Ssh} = ssh_transport:handle_kex_ecdh_reply(Msg, Ssh0),
    send_bytes(NewKeys, State),
    {next_state, {new_keys,client,ReNeg}, State#data{ssh_params = Ssh}};


%%% ######## {key_exchange_dh_gex_init, server, init|renegotiate} ####

handle_event(_, #ssh_msg_kex_dh_gex_init{} = Msg, {key_exchange_dh_gex_init,server,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, KexGexReply, Ssh1} =  ssh_transport:handle_kex_dh_gex_init(Msg, Ssh0),
    send_bytes(KexGexReply, State),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    send_bytes(NewKeys, State),
    {next_state, {new_keys,server,ReNeg}, State#data{ssh_params = Ssh}};


%%% ######## {key_exchange_dh_gex_reply, client, init|renegotiate} ####

handle_event(_, #ssh_msg_kex_dh_gex_reply{} = Msg, {key_exchange_dh_gex_reply,client,ReNeg},
	     #data{ssh_params=Ssh0} = State) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_dh_gex_reply(Msg, Ssh0),
    send_bytes(NewKeys, State),
    {next_state, {new_keys,client,ReNeg}, State#data{ssh_params = Ssh1}};


%%% ######## {new_keys, client|server} ####

handle_event(_, #ssh_msg_newkeys{} = Msg, {new_keys,client,init},
	     #data{ssh_params = Ssh0} = State) ->
    {ok, Ssh1} = ssh_transport:handle_new_keys(Msg, Ssh0),
    {MsgReq, Ssh} = ssh_auth:service_request_msg(Ssh1),
    send_bytes(MsgReq, State),
    {next_state, {service_request,client}, State#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_newkeys{} = Msg, {new_keys,server,init},
	     S = #data{ssh_params = Ssh0}) ->
    {ok, Ssh} = ssh_transport:handle_new_keys(Msg, Ssh0),
    {next_state, {service_request,server}, S#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_newkeys{}, {new_keys,Role,renegotiate}, S) ->
    {next_state, {connected,Role}, S};


%%% ######## {service_request, client|server}

handle_event(_, #ssh_msg_service_request{name = "ssh-userauth"} = Msg, {service_request,server},
	     #data{ssh_params = #ssh{session_id=SessionId} = Ssh0} = State) ->
    {ok, {Reply, Ssh}} = ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0),
    send_bytes(Reply, State),
    {next_state, {userauth,server}, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_service_request{}, {service_request,server}=StateName, State) ->
    Msg = #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			      description = "Unknown service"},
    disconnect(Msg, StateName, State);

handle_event(_, #ssh_msg_service_accept{name = "ssh-userauth"}, {service_request,client},
	     #data{ssh_params = #ssh{service="ssh-userauth"} = Ssh0} = State) ->
    {Msg, Ssh} = ssh_auth:init_userauth_request_msg(Ssh0),
    send_bytes(Msg, State),
    {next_state, {userauth,client}, State#data{auth_user = Ssh#ssh.user, ssh_params = Ssh}};


%%% ######## {userauth, client|server} ####

handle_event(_, #ssh_msg_userauth_request{service = "ssh-connection",
					  method = "none"} = Msg, StateName={userauth,server},
        #data{ssh_params = #ssh{session_id = SessionId,
                                 service = "ssh-connection"} = Ssh0
              } = State) ->
    {not_authorized, {_User, _Reason}, {Reply, Ssh}} =
	ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0),
    send_bytes(Reply, State),
    {next_state, StateName, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_userauth_request{service = "ssh-connection",
					  method = Method} = Msg, StateName={userauth,server},
	 #data{ssh_params = #ssh{session_id = SessionId,
				  service = "ssh-connection",
				  peer = {_, Address}} = Ssh0,
		opts = Opts, starter = Pid} = State) ->
    case lists:member(Method, Ssh0#ssh.userauth_methods) of
	true ->
	    case ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0) of
		{authorized, User, {Reply, Ssh}} ->
		    send_bytes(Reply, State),
		    Pid ! ssh_connected,
		    connected_fun(User, Address, Method, Opts),
		    {next_state, {connected,server},
		     State#data{auth_user = User, ssh_params = Ssh#ssh{authenticated = true}}};
		{not_authorized, {User, Reason}, {Reply, Ssh}} when Method == "keyboard-interactive" ->
		    retry_fun(User, Address, Reason, Opts),
		    send_bytes(Reply, State),
		    {next_state, {userauth_keyboard_interactive,server}, State#data{ssh_params = Ssh}};
		{not_authorized, {User, Reason}, {Reply, Ssh}} ->
		    retry_fun(User, Address, Reason, Opts),
		    send_bytes(Reply, State),
		    {next_state, StateName, State#data{ssh_params = Ssh}}
	    end;
	false ->
	    %% At least one non-erlang client does like this. Retry as the next event
	    {next_state, StateName, State,
	     [{next_event, internal, Msg#ssh_msg_userauth_request{method="none"}}]
	    }
	     end;

handle_event(_, #ssh_msg_userauth_request{service = Service}, {userauth,server}=StateName, State)
  when Service =/= "ssh-connection" ->
    Msg = #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			      description = "Unknown service"},
    disconnect(Msg, StateName, State);

handle_event(_, #ssh_msg_userauth_success{}, {userauth,client}, #data{ssh_params = Ssh,
								       starter = Pid} = State) ->
    Pid ! ssh_connected,
    {next_state, {connected,client}, State#data{ssh_params=Ssh#ssh{authenticated = true}}};

handle_event(_, #ssh_msg_userauth_failure{}, {userauth,client}=StateName,
	     #data{ssh_params = #ssh{userauth_methods = []}}  = State) ->
    Msg = #ssh_msg_disconnect{code = ?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
			      description = "Unable to connect using the available"
			                    " authentication methods"},
    disconnect(Msg, StateName, State);

handle_event(_, #ssh_msg_userauth_failure{authentications = Methods}, StateName={userauth,client},
	     #data{ssh_params = Ssh0 = #ssh{userauth_methods=AuthMthds}} = State) ->
    %% The prefered authentication method failed try next method
    Ssh1 = case AuthMthds of
	       none ->
		   %% Server tells us which authentication methods that are allowed
		   Ssh0#ssh{userauth_methods = string:tokens(Methods, ",")};
	       _ ->
		   %% We already know...
		   Ssh0
	   end,
    case ssh_auth:userauth_request_msg(Ssh1) of
	{disconnect, DisconnectMsg, {Msg, Ssh}} ->
	    send_bytes(Msg, State),
	    disconnect(DisconnectMsg, StateName, State#data{ssh_params = Ssh});
	{"keyboard-interactive", {Msg, Ssh}} ->
	    send_bytes(Msg, State),
	    {next_state, {userauth_keyboard_interactive,client}, State#data{ssh_params = Ssh}};
	{_Method, {Msg, Ssh}} ->
	    send_bytes(Msg, State),
	    {next_state, StateName, State#data{ssh_params = Ssh}}
    end;

handle_event(_, #ssh_msg_userauth_banner{}, StateName={userauth,client},
	 #data{ssh_params = #ssh{userauth_quiet_mode=true}} = State) ->
    {next_state, StateName, State};

handle_event(_, #ssh_msg_userauth_banner{message = Msg}, StateName={userauth,client},
	 #data{ssh_params = #ssh{userauth_quiet_mode=false}} = State) ->
    io:format("~s", [Msg]),
    {next_state, StateName, State};


%%% ######## {userauth_keyboard_interactive, client|server}

handle_event(_, #ssh_msg_userauth_info_request{} = Msg, {userauth_keyboard_interactive, client},
	     #data{ssh_params = #ssh{io_cb=IoCb} = Ssh0} = State) ->
    {ok, {Reply, Ssh}} = ssh_auth:handle_userauth_info_request(Msg, IoCb, Ssh0),
    send_bytes(Reply, State),
    {next_state, {userauth_keyboard_interactive_info_response,client}, State#data{ssh_params = Ssh}};

handle_event(_, #ssh_msg_userauth_info_response{} = Msg, {userauth_keyboard_interactive, server},
	     #data{ssh_params = #ssh{peer = {_,Address}} = Ssh0,
		    opts = Opts,
		    starter = Pid} = State) ->
    case ssh_auth:handle_userauth_info_response(Msg, Ssh0) of
	{authorized, User, {Reply, Ssh}} ->
	    send_bytes(Reply, State),
	    Pid ! ssh_connected,
	    connected_fun(User, Address, "keyboard-interactive", Opts),
	    {next_state, {connected,server}, State#data{auth_user = User,
							 ssh_params = Ssh#ssh{authenticated = true}}};
	{not_authorized, {User, Reason}, {Reply, Ssh}} ->
	    retry_fun(User, Address, Reason, Opts),
	    send_bytes(Reply, State),
	    {next_state, {userauth,server}, State#data{ssh_params = Ssh}}
    end;

handle_event(_, Msg = #ssh_msg_userauth_failure{}, {userauth_keyboard_interactive, client},
	     #data{ssh_params = Ssh0 = #ssh{userauth_preference=Prefs0}} = State) ->
    Prefs = [{Method,M,F,A} || {Method,M,F,A} <- Prefs0,
			       Method =/= "keyboard-interactive"],
    {next_state, {userauth,client},
     State#data{ssh_params = Ssh0#ssh{userauth_preference=Prefs}},
     [{next_event, internal, Msg}]};

handle_event(_, Msg=#ssh_msg_userauth_failure{}, {userauth_keyboard_interactive_info_response, client}, S) ->
    {next_state, {userauth,client}, S, [{next_event, internal, Msg}]};

handle_event(_, Msg=#ssh_msg_userauth_success{}, {userauth_keyboard_interactive_info_response, client}, S) ->
    {next_state, {userauth,client}, S, [{next_event, internal, Msg}]};

handle_event(_, Msg=#ssh_msg_userauth_info_request{}, {userauth_keyboard_interactive_info_response, client}, S) ->
    {next_state, {userauth_keyboard_interactive,client}, S, [{next_event, internal, Msg}]};


%%% ######## {connected, client|server} ####

handle_event(_, {#ssh_msg_kexinit{},_} = Event, {connected,Role},  #data{ssh_params = Ssh0} = State0) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
    State = State0#data{ssh_params = Ssh,
			 key_exchange_init_msg = KeyInitMsg},
    send_bytes(SshPacket, State),
    {next_state, {kexinit,Role,renegotiate}, State, [{next_event, internal, Event}]};

handle_event(_, #ssh_msg_disconnect{description=Desc} = Msg, StateName,
	     State0 = #data{connection_state = Connection0}) ->
    {disconnect, _, {{replies, Replies}, _Connection}} =
	ssh_connection:handle_msg(Msg, Connection0, role(StateName)),
    {Repls,State} = send_replies(Replies, State0),
    disconnect_fun(Desc, State#data.opts),
    {stop_and_reply, {shutdown,Desc}, Repls, State};

handle_event(_, #ssh_msg_ignore{}, StateName, State) ->
    {next_state, StateName, State};

handle_event(_, #ssh_msg_debug{always_display = Display,
			       message = DbgMsg,
			       language = Lang}, StateName, #data{opts = Opts} = State) ->
    F = proplists:get_value(ssh_msg_debug_fun, Opts,
			    fun(_ConnRef, _AlwaysDisplay, _Msg, _Language) -> ok end
			   ),
    catch F(self(), Display, DbgMsg, Lang),
    {next_state, StateName, State};

handle_event(_, #ssh_msg_unimplemented{}, StateName, State) ->
    {next_state, StateName, State};

handle_event(internal, Msg=#ssh_msg_global_request{},            StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_request_success{},           StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_request_failure{},           StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_open{},              StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_open_confirmation{}, StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_open_failure{},      StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_window_adjust{},     StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_data{},              StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_extended_data{},     StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_eof{},               StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_close{},             StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_request{},           StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_success{},           StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(internal, Msg=#ssh_msg_channel_failure{},           StateName, State) ->
    handle_connection_msg(Msg, StateName, State);

handle_event(cast, renegotiate, {connected,Role}, #data{ssh_params=Ssh0} = State) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
    send_bytes(SshPacket, State),
%%% FIXME: timer
    timer:apply_after(?REKEY_TIMOUT, gen_statem, cast, [self(), renegotiate]),
    {next_state, {kexinit,Role,renegotiate}, State#data{ssh_params = Ssh,
							 key_exchange_init_msg = KeyInitMsg}};

handle_event(cast, renegotiate, StateName, State) ->
    %% Already in key-exchange so safe to ignore
    {next_state, StateName, State};

%% Rekey due to sent data limit reached?
handle_event(cast, data_size, {connected,Role}, #data{ssh_params=Ssh0} = State) ->
    {ok, [{send_oct,Sent0}]} = inet:getstat(State#data.socket, [send_oct]),
    Sent = Sent0 - State#data.last_size_rekey,
    MaxSent = proplists:get_value(rekey_limit, State#data.opts, 1024000000),
%%% FIXME: timer
    timer:apply_after(?REKEY_DATA_TIMOUT, gen_statem, cast, [self(), data_size]),
    case Sent >= MaxSent of
	true ->
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh0),
	    send_bytes(SshPacket, State),
	    {next_state, {kexinit,Role,renegotiate}, State#data{ssh_params = Ssh,
								 key_exchange_init_msg = KeyInitMsg,
								 last_size_rekey = Sent0}};
	_ ->
	    {next_state, {connected,Role}, State}
    end;

handle_event(cast, data_size, StateName, State) ->
    %% Already in key-exchange so safe to ignore
    {next_state, StateName, State};

handle_event(cast, _, StateName, State) when StateName /= {connected,server},
					     StateName /= {connected,client} ->
    {next_state, StateName, State, [postpone]};

handle_event(cast, {adjust_window,ChannelId,Bytes}, StateName={connected,_Role},
	     #data{connection_state =
			#connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 recv_packet_size = PktSize} = Channel
	  when (WinSize-Bytes) >= 2*PktSize ->
	    %% The peer can send at least two more *full* packet, no hurry.
	    ssh_channel:cache_update(Cache,
				     Channel#channel{recv_window_pending = Pending + Bytes}),
	    {next_state, StateName, State0};
	
	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 remote_id = Id} = Channel ->
	    %% Now we have to update the window - we can't receive so many more pkts
	    ssh_channel:cache_update(Cache,
				     Channel#channel{recv_window_size =
							 WinSize + Bytes + Pending,
						     recv_window_pending = 0}),
	    Msg = ssh_connection:channel_adjust_window_msg(Id, Bytes + Pending),
	    {next_state, StateName, send_msg(Msg,State0)};
	
	undefined ->
	    {next_state, StateName, State0}
    end;

handle_event(cast, {reply_request,success,ChannelId}, StateName={connected,_},
	     #data{connection_state =
			#connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = RemoteId} ->
	    Msg = ssh_connection:channel_success_msg(RemoteId),
	    {next_state, StateName, send_msg(Msg,State0)};
	
	undefined ->
	    {next_state, StateName, State0}
    end;

handle_event(cast, {request,ChannelPid,ChannelId,Type,Data}, StateName={connected,_}, State0) ->
    State = handle_request(ChannelPid, ChannelId, Type, Data, false, none, State0),
    {next_state, StateName, State};

handle_event(cast, {request,ChannelId,Type,Data}, StateName={connected,_}, State0) ->
    State = handle_request(ChannelId, Type, Data, false, none, State0),
    {next_state, StateName, State};

handle_event(cast, {unknown,Data}, StateName={connected,_}, State) ->
    Msg = #ssh_msg_unimplemented{sequence = Data},
    {next_state, StateName, send_msg(Msg,State)};

%%% Previously handle_sync_event began here
handle_event({call,From}, get_print_info, StateName, State) ->
    Reply =
	try
	    {inet:sockname(State#data.socket),
	     inet:peername(State#data.socket)
	    }
	of
	    {{ok,Local}, {ok,Remote}} -> {{Local,Remote},io_lib:format("statename=~p",[StateName])};
	    _ -> {{"-",0},"-"}
	catch
	    _:_ -> {{"?",0},"?"}
	end,
    {next_state, StateName, State, [{reply,From,Reply}]};

handle_event({call,From}, {connection_info, Options}, StateName, State) ->
    Info = ssh_info(Options, State, []),
    {next_state, StateName, State, [{reply,From,Info}]};

handle_event({call,From}, {channel_info,ChannelId,Options}, StateName,
	     State=#data{connection_state = #connection{channel_cache = Cache}}) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
       #channel{} = Channel ->
	    Info = ssh_channel_info(Options, Channel, []),
	    {next_state, StateName, State, [{reply,From,Info}]};
	undefined ->
	    {next_state, StateName, State, [{reply,From,[]}]}
    end;

handle_event({call,From}, {info, ChannelPid}, StateName, State = #data{connection_state =
									    #connection{channel_cache = Cache}}) ->
    Result = ssh_channel:cache_foldl(
	       fun(Channel, Acc) when ChannelPid == all;
				      Channel#channel.user == ChannelPid ->
		       [Channel | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], Cache),
    {next_state, StateName, State, [{reply, From, {ok,Result}}]};

handle_event({call,From}, stop, StateName, #data{connection_state = Connection0} = State0) ->
    {disconnect, _Reason, {{replies, Replies}, Connection}} =
	ssh_connection:handle_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
						      description = "User closed down connection"},
				  Connection0, role(StateName)),
    {Repls,State} = send_replies(Replies, State0),
    {stop_and_reply, normal, [{reply,From,ok}|Repls], State#data{connection_state=Connection}};

handle_event({call,_}, _, StateName, State) when StateName /= {connected,server},
						 StateName /= {connected,client}  ->
    {next_state, StateName, State, [postpone]};

handle_event({call,From}, {request, ChannelPid, ChannelId, Type, Data, Timeout}, StateName={connected,_}, State0) ->
    State = handle_request(ChannelPid, ChannelId, Type, Data, true, From, State0),
    %% Note reply to channel will happen later when reply is recived from peer on the socket
    start_timeout(ChannelId, From, Timeout),
    handle_idle_timeout(State),
    {next_state, StateName, State};

handle_event({call,From}, {request, ChannelId, Type, Data, Timeout}, StateName={connected,_}, State0) ->
    State = handle_request(ChannelId, Type, Data, true, From, State0),
    %% Note reply to channel will happen later when reply is recived from peer on the socket
    start_timeout(ChannelId, From, Timeout),
    handle_idle_timeout(State),
    {next_state, StateName, State};

handle_event({call,From}, {global_request, Pid, _, _, _} = Request, StateName={connected,_},
	     #data{connection_state = #connection{channel_cache = Cache}} = State0) ->
    State1 = handle_global_request(Request, State0),
    Channel = ssh_channel:cache_find(Pid, Cache),
    State = add_request(true, Channel#channel.local_id, From, State1),
    {next_state, StateName, State};

handle_event({call,From}, {data, ChannelId, Type, Data, Timeout}, StateName={connected,_},
	     #data{connection_state = #connection{channel_cache=_Cache} = Connection0} = State0) ->
    case ssh_connection:channel_data(ChannelId, Type, Data, Connection0, From) of
	{{replies, Replies}, Connection} ->
	    {Repls,State} = send_replies(Replies, State0#data{connection_state = Connection}),
	    start_timeout(ChannelId, From, Timeout),
	    {next_state, StateName, State, Repls};
	{noreply, Connection} ->
	    start_timeout(ChannelId, From, Timeout),
	    {next_state, StateName, State0#data{connection_state = Connection}}
    end;

handle_event({call,From}, {eof, ChannelId}, StateName={connected,_},
	     #data{connection_state = #connection{channel_cache=Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id, sent_close = false} ->
	    State = send_msg(ssh_connection:channel_eof_msg(Id), State0),
	    {next_state, StateName, State, [{reply,From,ok}]};
	_ ->
	    {next_state, StateName, State0, [{reply,From,{error,closed}}]}
    end;

handle_event({call,From},
	     {open, ChannelPid, Type, InitialWindowSize, MaxPacketSize, Data, Timeout},
	     StateName = {connected,_},
	     #data{connection_state = #connection{channel_cache = Cache}} = State0) ->
    erlang:monitor(process, ChannelPid),
    {ChannelId, State1} = new_channel_id(State0),
    Msg = ssh_connection:channel_open_msg(Type, ChannelId,
					  InitialWindowSize,
					  MaxPacketSize, Data),
    State2 = send_msg(Msg, State1),
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
    {next_state, StateName, remove_timer_ref(State)};

handle_event({call,From}, {send_window, ChannelId}, StateName={connected,_},
	     #data{connection_state = #connection{channel_cache = Cache}} = State) ->
    Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{send_window_size = WinSize,
			 send_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {next_state, StateName, State, [{reply,From,Reply}]};

handle_event({call,From}, {recv_window, ChannelId}, StateName={connected,_},
	     #data{connection_state = #connection{channel_cache = Cache}} = State) ->
    Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{recv_window_size = WinSize,
			 recv_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {next_state, StateName, State, [{reply,From,Reply}]};

handle_event({call,From}, {close, ChannelId}, StateName={connected,_},
	     #data{connection_state =
			#connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    State1 = send_msg(ssh_connection:channel_close_msg(Id), State0),
	    ssh_channel:cache_update(Cache, Channel#channel{sent_close = true}),
	    handle_idle_timeout(State1),
	    {next_state, StateName, State1, [{reply,From,ok}]};
	undefined ->
	    {next_state, StateName, State0, [{reply,From,ok}]}
    end;

handle_event(info, {Protocol, Socket, "SSH-" ++ _ = Version}, StateName={hello,_},
	     State=#data{socket = Socket,
			  transport_protocol = Protocol}) ->
    {next_state, StateName, State,  [{next_event, internal, {version_exchange,Version}}]};

handle_event(info, {Protocol, Socket, Info}, StateName={hello,_},
	     State=#data{socket = Socket,
			  transport_protocol = Protocol}) ->
    {next_state, StateName, State,  [{next_event, internal, {info_line,Info}}]};

handle_event(info, {Protocol, Socket, Data}, StateName, State0 =
		 #data{socket = Socket,
			transport_protocol = Protocol,
			decoded_data_buffer = DecData0,
			encoded_data_buffer = EncData0,
			undecoded_packet_length = RemainingSshPacketLen0,
			ssh_params = Ssh0}) ->
    Encoded = <<EncData0/binary, Data/binary>>,
    try ssh_transport:handle_packet_part(DecData0, Encoded, RemainingSshPacketLen0, Ssh0)
    of
	{decoded, Bytes, EncDataRest, Ssh1} ->
	    State = State0#data{ssh_params =
				     Ssh1#ssh{recv_sequence = ssh_transport:next_seqnum(Ssh1#ssh.recv_sequence)},
				 decoded_data_buffer = <<>>,
				 undecoded_packet_length = undefined,
				 encoded_data_buffer = EncDataRest},
	    try
		ssh_message:decode(set_prefix_if_trouble(Bytes,State))
	    of
		Msg = #ssh_msg_kexinit{} ->
		    {next_state, StateName, State, [{next_event, internal, {Msg,Bytes}},
						    {next_event, internal, prepare_next_packet}
						   ]};
		Msg ->
		    {next_state, StateName, State, [{next_event, internal, Msg},
						    {next_event, internal, prepare_next_packet}
						   ]}
	    catch
		_C:_E  ->
		    DisconnectMsg =
			#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
					    description = "Encountered unexpected input"},
		    disconnect(DisconnectMsg, StateName, State)
	    end;

	{get_more, DecBytes, EncDataRest, RemainingSshPacketLen, Ssh1} ->
	    %% Here we know that there are not enough bytes in EncDataRest to use. Must wait.
	    inet:setopts(Socket, [{active, once}]),
	    {next_state, StateName, State0#data{encoded_data_buffer = EncDataRest,
						 decoded_data_buffer = DecBytes,
						 undecoded_packet_length = RemainingSshPacketLen,
						 ssh_params = Ssh1}};

	{bad_mac, Ssh1} ->
	    DisconnectMsg =
		    #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
					description = "Bad mac"},
	    disconnect(DisconnectMsg, StateName, State0#data{ssh_params=Ssh1});

	{error, {exceeds_max_size,PacketLen}} ->
	    DisconnectMsg =
		#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				    description = "Bad packet length "
				    ++ integer_to_list(PacketLen)},
	    disconnect(DisconnectMsg, StateName, State0)
    catch
	_C:_E ->
	    DisconnectMsg =
		#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				    description = "Bad packet"},
	    disconnect(DisconnectMsg, StateName, State0)
    end;

handle_event(internal, prepare_next_packet, StateName, State) ->
    Enough =  erlang:max(8, State#data.ssh_params#ssh.decrypt_block_size),
    case size(State#data.encoded_data_buffer) of
	Sz when Sz >= Enough ->
	    self() ! {State#data.transport_protocol, State#data.socket, <<>>};
	_ ->
	    inet:setopts(State#data.socket, [{active, once}])
    end,
    {next_state, StateName, State};

handle_event(info, {CloseTag,Socket}, StateName,
	     State=#data{socket = Socket,
			  transport_close_tag = CloseTag}) ->
    DisconnectMsg =
	#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
			    description = "Connection closed"},
    disconnect(DisconnectMsg, StateName, State);

handle_event(info, {timeout, {_, From} = Request}, StateName,
	    #data{connection_state = #connection{requests = Requests} = Connection} = State) ->
    case lists:member(Request, Requests) of
	true ->
	    {next_state, StateName,
	     State#data{connection_state =
			     Connection#connection{requests =
						       lists:delete(Request, Requests)}},
	     [{reply,From,{error,timeout}}]};
	false ->
	    {next_state, StateName, State}
    end;

%%% Handle that ssh channels user process goes down
handle_event(info, {'DOWN', _Ref, process, ChannelPid, _Reason}, StateName, State0) ->
    {{replies, Replies}, State1} = handle_channel_down(ChannelPid, State0),
    {Repls, State} = send_replies(Replies, State1),
    {next_state, StateName, State, Repls};

%%% So that terminate will be run when supervisor is shutdown
handle_event(info, {'EXIT', _Sup, Reason}, _, _) ->
    {stop, {shutdown, Reason}};

handle_event(info, {check_cache, _ , _}, StateName,
	     #data{connection_state = #connection{channel_cache=Cache}} = State) ->
    {next_state, StateName, check_cache(State, Cache)};

handle_event(info, UnexpectedMessage, StateName,
	     State = #data{opts = Opts,
			    ssh_params = SshParams}) ->
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
	    error_logger:info_report(Msg),
	    {next_state, StateName, State};

	skip ->
	    {next_state, StateName, State};

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
	    error_logger:error_report(Msg),
	    {next_state, StateName, State}
    end;

handle_event(internal, {disconnect,Msg,_Reason}, StateName, State) ->
    disconnect(Msg, StateName, State);

handle_event(Type, Ev, StateName, State) ->
    case catch atom_to_list(element(1,Ev)) of
	"ssh_msg_" ++_ when Type==internal ->
	    Msg = #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				      description = "Message in wrong state"},
	    disconnect(Msg, StateName, State);
	_ ->
	    Msg = #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				      description = "Internal error"},
	    disconnect(Msg, StateName, State)
    end.


%%--------------------------------------------------------------------

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

terminate(normal, StateName, State) ->
    finalize_termination(StateName, State);

terminate({shutdown,{init,Reason}}, StateName, State) ->
    error_logger:info_report(io_lib:format("Erlang ssh in connection handler init: ~p~n",[Reason])),
    finalize_termination(StateName, State);

terminate(shutdown, StateName, State0) ->
    %% Terminated by supervisor
    State = send_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
					 description = "Application shutdown"},
		     State0),
timer:sleep(400),  %% FIXME!!! gen_tcp:shutdown instead
    finalize_termination(StateName, State);

%% terminate({shutdown,Msg}, StateName, State0) when is_record(Msg,ssh_msg_disconnect)->
%%     State = send_msg(Msg, State0),
%% timer:sleep(400),  %% FIXME!!! gen_tcp:shutdown instead
%%     finalize_termination(StateName, Msg, State);

terminate({shutdown,_R}, StateName, State) ->
    finalize_termination(StateName, State);

terminate(Reason, StateName, State0) ->
    %% Others, e.g  undef, {badmatch,_}
    log_error(Reason),
    State = send_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
					   description = "Internal error"},
		     State0),
    finalize_termination(StateName, State).

%%--------------------------------------------------------------------

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

format_status(normal, [_, _StateName, State]) ->
    [{data, [{"State", State}]}];
format_status(terminate, [_, _StateName, State]) ->
    SshParams0 = (State#data.ssh_params),
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
    [{data, [{"State", State#data{decoded_data_buffer = "***",
				   encoded_data_buffer =  "***",
				   key_exchange_init_msg = "***",
				   opts =  "***",
				   recbuf =  "***",
				   ssh_params = SshParams
				  }}]}].


%%--------------------------------------------------------------------

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Starting

start_the_connection_child(UserPid, Role, Socket, Options) ->
    Sups = proplists:get_value(supervisors, Options),
    ConnectionSup = proplists:get_value(connection_sup, Sups),
    Opts = [{supervisors, Sups}, {user_pid, UserPid} | proplists:get_value(ssh_opts, Options, [])],
    {ok, Pid} = ssh_connection_sup:start_child(ConnectionSup, [Role, Socket, Opts]),
    {_, Callback, _} =  proplists:get_value(transport, Options, {tcp, gen_tcp, tcp_closed}),
    socket_control(Socket, Pid, Callback),
    Pid.

%%--------------------------------------------------------------------
%% Stopping

finalize_termination(_StateName, #data{transport_cb = Transport,
					connection_state = Connection,
					socket = Socket}) ->
    case Connection of
	#connection{system_supervisor = SysSup,
		    sub_system_supervisor = SubSysSup} when is_pid(SubSysSup) ->
	    ssh_system_sup:stop_subsystem(SysSup, SubSysSup);
	_ ->
	    do_nothing
    end,
    (catch Transport:close(Socket)),
    ok.




%% StateName to Role
role({_,Role}) -> Role;
role({_,Role,_}) -> Role.


renegotiation({_,_,ReNeg}) -> ReNeg == renegotiation;
renegotiation(_) -> false.


get_idle_time(SshOptions) ->
    case proplists:get_value(idle_time, SshOptions) of
	infinity ->
	    infinity;
       _IdleTime -> %% We dont want to set the timeout on first connect
	    undefined
    end.

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


send_msg(Msg, State=#data{ssh_params=Ssh0}) when is_tuple(Msg) ->
    {Bytes, Ssh} = ssh_transport:ssh_packet(Msg, Ssh0),
    send_bytes(Bytes, State),
    State#data{ssh_params=Ssh}.

send_bytes(Bytes, #data{socket = Socket, transport_cb = Transport}) ->
    Transport:send(Socket, Bytes).

handle_version({2, 0} = NumVsn, StrVsn, Ssh0) ->
    Ssh = counterpart_versions(NumVsn, StrVsn, Ssh0),
    {ok, Ssh};
handle_version(_,_,_) ->
    not_supported.

string_version(#ssh{role = client, c_version = Vsn}) ->
    Vsn;
string_version(#ssh{role = server, s_version = Vsn}) ->
    Vsn.


cast(FsmPid, Event) ->
    gen_statem:cast(FsmPid, Event).

call(FsmPid, Event) ->
    call(FsmPid, Event, infinity).

call(FsmPid, Event, Timeout) ->
    try gen_statem:call(FsmPid, Event, Timeout) of
	{closed, _R} ->
	    {error, closed};
	{killed, _R} ->
	    {error, closed};
	Result ->
	    Result
    catch
	exit:{noproc, _R} ->
	    {error, closed};
	exit:{normal, _R} ->
	    {error, closed};
	exit:{{shutdown, _R},_} ->
	    {error, closed}
    end.


handle_connection_msg(Msg, StateName, State0 =
			  #data{starter = User,
				 connection_state = Connection0,
				 event_queue = Qev0}) ->
    Renegotiation = renegotiation(StateName),
    Role = role(StateName),
    try ssh_connection:handle_msg(Msg, Connection0, Role) of
	{{replies, Replies}, Connection} ->
	    case StateName of
		{connected,_} ->
		    {Repls, State} = send_replies(Replies,
						  State0#data{connection_state=Connection}),
		    {next_state, StateName, State, Repls};
		_ ->
		    {ConnReplies, Replies} =
			lists:splitwith(fun not_connected_filter/1, Replies),
		    {Repls, State} = send_replies(Replies,
						  State0#data{event_queue = Qev0 ++ ConnReplies}),
		    {next_state, StateName, State, Repls}
	    end;

	{noreply, Connection} ->
	    {next_state, StateName, State0#data{connection_state = Connection}};

	{disconnect, Reason0, {{replies, Replies}, Connection}} ->
	    {Repls,State} = send_replies(Replies, State0#data{connection_state = Connection}),
	    case {Reason0,Role} of
		{{_, Reason}, client} when ((StateName =/= {connected,client}) and (not Renegotiation)) ->
		    User ! {self(), not_connected, Reason};
		_ ->
		    ok
	    end,
	    {stop, {shutdown,normal}, Repls, State#data{connection_state = Connection}}

    catch
	_:Error ->
	    {disconnect, _Reason, {{replies, Replies}, Connection}} =
		ssh_connection:handle_msg(
		  #ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
				      description = "Internal error"},
		  Connection0, Role),
	    {Repls,State} = send_replies(Replies, State0#data{connection_state = Connection}),
	    {stop, {shutdown,Error}, Repls, State#data{connection_state = Connection}}
    end.


set_prefix_if_trouble(Msg = <<?BYTE(Op),_/binary>>, #data{ssh_params=SshParams})
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

%%%----------------------------------------------------------------
handle_request(ChannelPid, ChannelId, Type, Data, WantReply, From,
	       #data{connection_state =
		      #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    update_sys(Cache, Channel, Type, ChannelPid),
	    Msg = ssh_connection:channel_request_msg(Id, Type,
						     WantReply, Data),
	    send_msg(Msg, add_request(WantReply, ChannelId, From, State0));
	undefined ->
	    State0
    end.

handle_request(ChannelId, Type, Data, WantReply, From,
	       #data{connection_state =
		      #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id}  ->
	    Msg = ssh_connection:channel_request_msg(Id, Type,
						     WantReply, Data),
	    send_msg(Msg,  add_request(WantReply, ChannelId, From, State0));
	undefined ->
	    State0
    end.

%%%----------------------------------------------------------------
handle_global_request({global_request, ChannelPid,
		       "tcpip-forward" = Type, WantReply,
		       <<?UINT32(IPLen),
			IP:IPLen/binary, ?UINT32(Port)>> = Data},
		      #data{connection_state =
				 #connection{channel_cache = Cache}
			     = Connection0} = State) ->
    ssh_channel:cache_update(Cache, #channel{user = ChannelPid,
					     type = "forwarded-tcpip",
					     sys = none}),
    Connection = ssh_connection:bind(IP, Port, ChannelPid, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg(Msg, State#data{connection_state = Connection});

handle_global_request({global_request, _Pid, "cancel-tcpip-forward" = Type,
		       WantReply, <<?UINT32(IPLen),
				   IP:IPLen/binary, ?UINT32(Port)>> = Data},
		      #data{connection_state = Connection0} = State) ->
    Connection = ssh_connection:unbind(IP, Port, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg(Msg, State#data{connection_state = Connection});

handle_global_request({global_request, _, "cancel-tcpip-forward" = Type,
		       WantReply, Data}, State) ->
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg(Msg, State).

%%%----------------------------------------------------------------
handle_idle_timeout(#data{opts = Opts}) ->
    case proplists:get_value(idle_time, Opts, infinity) of
	infinity ->
	    ok;
	IdleTime ->
	    erlang:send_after(IdleTime, self(), {check_cache, [], []})
    end.

handle_channel_down(ChannelPid, #data{connection_state =
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
add_request(true, ChannelId, From, #data{connection_state =
					#connection{requests = Requests0} =
					Connection} = State) ->
    Requests = [{ChannelId, From} | Requests0],
    State#data{connection_state = Connection#connection{requests = Requests}}.

new_channel_id(#data{connection_state = #connection{channel_id_seed = Id} =
		      Connection}
	       = State) ->
    {Id, State#data{connection_state =
		     Connection#connection{channel_id_seed = Id + 1}}}.

%%%----------------------------------------------------------------
%% %%% This server/client has decided to disconnect via the state machine:
disconnect(Msg=#ssh_msg_disconnect{description=Description}, _StateName, State0) ->
    State = send_msg(Msg, State0),
    disconnect_fun(Description, State#data.opts),
timer:sleep(400),
    {stop, {shutdown,Description}, State}.

%%%----------------------------------------------------------------
counterpart_versions(NumVsn, StrVsn, #ssh{role = server} = Ssh) ->
    Ssh#ssh{c_vsn = NumVsn , c_version = StrVsn};
counterpart_versions(NumVsn, StrVsn, #ssh{role = client} = Ssh) ->
    Ssh#ssh{s_vsn = NumVsn , s_version = StrVsn}.

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
ssh_info([client_version | Rest], #data{ssh_params = #ssh{c_vsn = IntVsn,
							   c_version = StringVsn}} = State, Acc) ->
    ssh_info(Rest, State, [{client_version, {IntVsn, StringVsn}} | Acc]);

ssh_info([server_version | Rest], #data{ssh_params =#ssh{s_vsn = IntVsn,
							  s_version = StringVsn}} = State, Acc) ->
    ssh_info(Rest, State, [{server_version, {IntVsn, StringVsn}} | Acc]);
ssh_info([peer | Rest], #data{ssh_params = #ssh{peer = Peer}} = State, Acc) ->
    ssh_info(Rest, State, [{peer, Peer} | Acc]);
ssh_info([sockname | Rest], #data{socket = Socket} = State, Acc) ->
    {ok, SockName} = inet:sockname(Socket),
   ssh_info(Rest, State, [{sockname, SockName}|Acc]);
ssh_info([user | Rest], #data{auth_user = User} = State, Acc) ->
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
    Report = io_lib:format("Erlang ssh connection handler failed with reason:~n"
			   "    ~p~n"
			   "Stacktrace:~n"
			   "    ~p~n",
			   [Reason, erlang:get_stacktrace()]),
    error_logger:error_report(Report).


%%%----------------------------------------------------------------
not_connected_filter({connection_reply, _Data}) -> true;
not_connected_filter(_) -> false.

%%%----------------------------------------------------------------
send_replies(Repls, State) ->
    lists:foldl(fun get_repl/2,
		{[],State},
		Repls).

get_repl({connection_reply,Msg}, {CallRepls,S}) ->
    {CallRepls, send_msg(Msg,S)};
get_repl({channel_data,undefined,_Data}, Acc) ->
    Acc;
get_repl({channel_data,Pid,Data}, Acc) ->
    Pid ! {ssh_cm, self(), Data},
    Acc;
get_repl({channel_request_reply,From,Data}, {CallRepls,S}) ->
    {[{reply,From,Data}|CallRepls], S};
get_repl({flow_control,Cache,Channel,From,Msg}, {CallRepls,S}) ->
    ssh_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
    {[{reply,From,Msg}|CallRepls], S};
get_repl({flow_control,From,Msg}, {CallRepls,S}) ->
    {[{reply,From,Msg}|CallRepls], S};
get_repl(noreply, Acc) ->
    Acc;
get_repl(X, Acc) ->
    exit({get_repl,X,Acc}).



%%%----------------------------------------------------------------
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


check_cache(#data{opts = Opts} = State, Cache) ->
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

handle_idle_timer(Time, #data{idle_timer_ref = undefined} = State) ->
    TimerRef = erlang:send_after(Time, self(), {'EXIT', [], "Timeout"}),
    State#data{idle_timer_ref=TimerRef};
handle_idle_timer(_, State) ->
    State.

remove_timer_ref(State) ->
    case State#data.idle_timer_ref of
	infinity -> %% If the timer is not activated
	    State;
	undefined -> %% If we already has cancelled the timer
	    State;
	TimerRef -> %% Timer is active
	    erlang:cancel_timer(TimerRef),
	    State#data{idle_timer_ref = undefined}
    end.

socket_control(Socket, Pid, Transport) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    gen_statem:cast(Pid, socket_control);
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
