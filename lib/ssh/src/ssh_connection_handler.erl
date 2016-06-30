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

%%% Exports not intended to be used :). They are used for spawning and tests
-export([init_connection_handler/3,	   % proc_lib:spawn needs this
	 init_ssh_record/3,		   % Export of this internal function
					   % intended for low-level protocol test suites
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

-define(DefaultTransport,  {tcp, gen_tcp, tcp_closed} ).

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
	ok = socket_control(Socket, Pid, Options),
	handshake(Pid, erlang:monitor(process,Pid), Timeout)
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
		   iodata(),
		   pos_integer(),
		   pos_integer(),
		   timeout()
		  ) -> {open, channel_id()} | {error, term()}.
		   
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
open_channel(ConnectionHandler, 
	     ChannelType, ChannelSpecificData, InitialWindowSize, MaxPacketSize, 
	     Timeout) ->
    call(ConnectionHandler,
	 {open, 
	  self(), 
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
	  ) -> {ok, [#channel{}]} .

-spec info(connection_ref(),
	   pid() | all
	  ) -> {ok, [#channel{}]} .
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
info(ConnectionHandler) ->
    info(ConnectionHandler, all).

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
%% Test support
%%====================================================================
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


%%====================================================================
%% Internal process state
%%====================================================================
-record(data, {
	  starter                               :: pid(),
	  auth_user                             :: string()
						 | undefined,
	  connection_state                      :: #connection{},
	  latest_channel_id         = 0         :: non_neg_integer(),
	  idle_timer_ref                        :: undefined 
						 | infinity
						 | reference(),
	  idle_timer_value          = infinity  :: infinity
						 | pos_integer(),
	  transport_protocol                    :: atom(),	% ex: tcp
	  transport_cb                          :: atom(),	% ex: gen_tcp
	  transport_close_tag                   :: atom(),	% ex: tcp_closed
	  ssh_params                            :: #ssh{}
						 | undefined,
	  socket                                :: inet:socket(),
	  decrypted_data_buffer     = <<>>      :: binary(),
	  encrypted_data_buffer     = <<>>      :: binary(),
	  undecrypted_packet_length             :: undefined | non_neg_integer(),
	  key_exchange_init_msg                 :: #ssh_msg_kexinit{}
						 | undefined,
	  last_size_rekey           = 0         :: non_neg_integer(),
	  event_queue               = []        :: list(),
	  opts                                  :: proplists:proplist(),
	  inet_initial_recbuf_size              :: pos_integer()
						 | undefined
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
	    proplists:get_value(transport, Opts, ?DefaultTransport),
	S0#data{ssh_params = init_ssh_record(Role, Socket, Opts),
		 transport_protocol = Protocol,
		 transport_cb = Callback,
		 transport_close_tag = CloseTag
		}
    of
	S ->
	    gen_statem:enter_loop(?MODULE,
				  [], %%[{debug,[trace,log,statistics,debug]} || Role==server],
				  handle_event_function,
				  {hello,Role},
				  S)
    catch
	_:Error ->
	    gen_statem:enter_loop(?MODULE,
				  [],
				  handle_event_function,
				  {init_error,Error},
				  S0)
    end.


init_process_state(Role, Socket, Opts) ->
    D = #data{connection_state =
		   C = #connection{channel_cache = ssh_channel:cache_create(),
				   channel_id_seed = 0,
				   port_bindings = [],
				   requests = [],
				   options = Opts},
	       starter = proplists:get_value(user_pid, Opts),
	       socket = Socket,
	       opts = Opts
	      },
    case Role of
	client ->
	    %% Start the renegotiation timers
	    timer:apply_after(?REKEY_TIMOUT,      gen_statem, cast, [self(), renegotiate]),
	    timer:apply_after(?REKEY_DATA_TIMOUT, gen_statem, cast, [self(), data_size]),
	    cache_init_idle_timer(D);
	server ->
	    D#data{connection_state = init_connection(Role, C, Opts)}
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
    AuthMethods = proplists:get_value(auth_methods,
				      Opts,
				      case Role of
					  server -> ?SUPPORTED_AUTH_METHODS;
					  client -> undefined
				      end),
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
-type event_content() ::  any().

-type renegotiate_flag() :: init | renegotiate.

-type state_name() :: 
	{init_error,any()}
      | {hello, role()}
      | {kexinit, role(), renegotiate_flag()}
      | {key_exchange, role(), renegotiate_flag()}
      | {key_exchange_dh_gex_init, server, renegotiate_flag()}
      | {key_exchange_dh_gex_reply, client, renegotiate_flag()}
      | {new_keys, role()}
      | {service_request, role()}
      | {userauth, role()}
      | {userauth_keyboard_interactive, role()}
      | {connected, role()}
	.

-type handle_event_result() :: gen_statem:handle_event_result().

-spec handle_event(gen_statem:event_type(),
		   event_content(),
		   state_name(),
		   #data{}
		  ) -> handle_event_result().
		   
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

%%% ######## Error in the initialisation ####

handle_event(_, _Event, {init_error,Error}, _) ->
    case Error of
	{badmatch,{error,enotconn}} ->
	    %% Handles the abnormal sequence:
	    %%    SYN->
	    %%            <-SYNACK
	    %%    ACK->
	    %%    RST->
	    {stop, {shutdown,"TCP connenction to server was prematurely closed by the client"}};

	OtherError ->
	    {stop, {shutdown,{init,OtherError}}}
    end;


%%% ######## {hello, client|server} ####
%% The very first event that is sent when the we are set as controlling process of Socket
handle_event(_, socket_control, {hello,_}, D) ->
    VsnMsg = ssh_transport:hello_version_msg(string_version(D#data.ssh_params)),
    ok = send_bytes(VsnMsg, D),
    case inet:getopts(Socket=D#data.socket, [recbuf]) of
	{ok, [{recbuf,Size}]} ->
	    %% Set the socket to the hello text line handling mode:
	    inet:setopts(Socket, [{packet, line},
				  {active, once},
				  % Expecting the version string which might
				  % be max ?MAX_PROTO_VERSION bytes:
				  {recbuf, ?MAX_PROTO_VERSION},
				  {nodelay,true}]),
	    {keep_state, D#data{inet_initial_recbuf_size=Size}};

	Other ->
	    {stop, {shutdown,{unexpected_getopts_return, Other}}}
    end;

handle_event(_, {info_line,_Line}, {hello,Role}, D) ->
    case Role of
	client ->
	    %% The server may send info lines to the client before the version_exchange
	    inet:setopts(D#data.socket, [{active, once}]),
	    keep_state_and_data;
	server ->
	    %% But the client may NOT send them to the server. Openssh answers with cleartext,
	    %% and so do we
	    ok = send_bytes("Protocol mismatch.", D),
	    {stop, {shutdown,"Protocol mismatch in version exchange. Client sent info lines."}}
    end;

handle_event(_, {version_exchange,Version}, {hello,Role}, D) ->
    {NumVsn, StrVsn} = ssh_transport:handle_hello_version(Version),
    case handle_version(NumVsn, StrVsn, D#data.ssh_params) of
	{ok, Ssh1} ->
	    %% Since the hello part is finnished correctly, we set the
	    %% socket to the packet handling mode (including recbuf size):
	    inet:setopts(D#data.socket, [{packet,0},
					 {mode,binary},
					 {active, once},
					 {recbuf, D#data.inet_initial_recbuf_size}]),
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh1),
	    ok = send_bytes(SshPacket, D),
	    {next_state, {kexinit,Role,init}, D#data{ssh_params = Ssh,
						     key_exchange_init_msg = KeyInitMsg}};
	not_supported ->
	    disconnect(
	      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,
				  description = ["Protocol version ",StrVsn," not supported"]},
	      {next_state, {hello,Role}, D})
    end;

		  
%%% ######## {kexinit, client|server, init|renegotiate} ####

handle_event(_, {#ssh_msg_kexinit{}=Kex, Payload}, {kexinit,Role,ReNeg},
	     D = #data{key_exchange_init_msg = OwnKex}) ->
    Ssh1 = ssh_transport:key_init(peer_role(Role), D#data.ssh_params, Payload),
    Ssh = case ssh_transport:handle_kexinit_msg(Kex, OwnKex, Ssh1) of
	      {ok, NextKexMsg, Ssh2} when Role==client ->
		  ok = send_bytes(NextKexMsg, D),
		  Ssh2;
	      {ok, Ssh2} when Role==server ->
		  Ssh2
	  end,
    {next_state, {key_exchange,Role,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange, client|server, init|renegotiate} ####

%%%---- diffie-hellman
handle_event(_, #ssh_msg_kexdh_init{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, KexdhReply, Ssh1} = ssh_transport:handle_kexdh_init(Msg, D#data.ssh_params),
    ok = send_bytes(KexdhReply, D),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    ok = send_bytes(NewKeys, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kexdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh} = ssh_transport:handle_kexdh_reply(Msg, D#data.ssh_params),
    ok = send_bytes(NewKeys, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- diffie-hellman group exchange
handle_event(_, #ssh_msg_kex_dh_gex_request{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, GexGroup, Ssh} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    ok = send_bytes(GexGroup, D),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kex_dh_gex_request_old{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, GexGroup, Ssh} = ssh_transport:handle_kex_dh_gex_request(Msg, D#data.ssh_params),
    ok = send_bytes(GexGroup, D),
    {next_state, {key_exchange_dh_gex_init,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kex_dh_gex_group{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, KexGexInit, Ssh} = ssh_transport:handle_kex_dh_gex_group(Msg, D#data.ssh_params),
    ok = send_bytes(KexGexInit, D),
    {next_state, {key_exchange_dh_gex_reply,client,ReNeg}, D#data{ssh_params=Ssh}};

%%%---- elliptic curve diffie-hellman
handle_event(_, #ssh_msg_kex_ecdh_init{} = Msg, {key_exchange,server,ReNeg}, D) ->
    {ok, KexEcdhReply, Ssh1} = ssh_transport:handle_kex_ecdh_init(Msg, D#data.ssh_params),
    ok = send_bytes(KexEcdhReply, D),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    ok = send_bytes(NewKeys, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};

handle_event(_, #ssh_msg_kex_ecdh_reply{} = Msg, {key_exchange,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh} = ssh_transport:handle_kex_ecdh_reply(Msg, D#data.ssh_params),
    ok = send_bytes(NewKeys, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange_dh_gex_init, server, init|renegotiate} ####

handle_event(_, #ssh_msg_kex_dh_gex_init{} = Msg, {key_exchange_dh_gex_init,server,ReNeg}, D) ->
    {ok, KexGexReply, Ssh1} =  ssh_transport:handle_kex_dh_gex_init(Msg, D#data.ssh_params),
    ok = send_bytes(KexGexReply, D),
    {ok, NewKeys, Ssh} = ssh_transport:new_keys_message(Ssh1),
    ok = send_bytes(NewKeys, D),
    {next_state, {new_keys,server,ReNeg}, D#data{ssh_params=Ssh}};


%%% ######## {key_exchange_dh_gex_reply, client, init|renegotiate} ####

handle_event(_, #ssh_msg_kex_dh_gex_reply{} = Msg, {key_exchange_dh_gex_reply,client,ReNeg}, D) ->
    {ok, NewKeys, Ssh1} = ssh_transport:handle_kex_dh_gex_reply(Msg, D#data.ssh_params),
    ok = send_bytes(NewKeys, D),
    {next_state, {new_keys,client,ReNeg}, D#data{ssh_params=Ssh1}};


%%% ######## {new_keys, client|server} ####

%% First key exchange round:
handle_event(_, #ssh_msg_newkeys{} = Msg, {new_keys,Role,init}, D) ->
    {ok, Ssh1} = ssh_transport:handle_new_keys(Msg, D#data.ssh_params),
    Ssh = case Role of
	      client ->
		  {MsgReq, Ssh2} = ssh_auth:service_request_msg(Ssh1),
		  ok = send_bytes(MsgReq, D),
		  Ssh2;
	      server ->
		  Ssh1
	  end,
    {next_state, {service_request,Role}, D#data{ssh_params=Ssh}};

%% Subsequent key exchange rounds (renegotiation):
handle_event(_, #ssh_msg_newkeys{}, {new_keys,Role,renegotiate}, D) ->
    {next_state, {connected,Role}, D};

%%% ######## {service_request, client|server}

handle_event(_, Msg = #ssh_msg_service_request{name=ServiceName}, StateName = {service_request,server}, D) ->
    case ServiceName of
	"ssh-userauth" ->
	    Ssh0 = #ssh{session_id=SessionId} = D#data.ssh_params,
	    {ok, {Reply, Ssh}} = ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0),
	    ok = send_bytes(Reply, D),
	    {next_state, {userauth,server}, D#data{ssh_params = Ssh}};

	_ ->
	    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
					   description = "Unknown service"},
		       StateName, D)
    end;

handle_event(_, #ssh_msg_service_accept{name = "ssh-userauth"}, {service_request,client},
	     #data{ssh_params = #ssh{service="ssh-userauth"} = Ssh0} = State) ->
    {Msg, Ssh} = ssh_auth:init_userauth_request_msg(Ssh0),
    ok = send_bytes(Msg, State),
    {next_state, {userauth,client}, State#data{auth_user = Ssh#ssh.user, ssh_params = Ssh}};


%%% ######## {userauth, client|server} ####

%%---- userauth request to server
handle_event(_, 
	     Msg = #ssh_msg_userauth_request{service = ServiceName, method = Method},
	     StateName = {userauth,server},
	     D = #data{ssh_params=Ssh0}) ->

    case {ServiceName, Ssh0#ssh.service, Method} of
	{"ssh-connection", "ssh-connection", "none"} ->
	    %% Probably the very first userauth_request but we deny unauthorized login
	    {not_authorized, _, {Reply,Ssh}} =
		ssh_auth:handle_userauth_request(Msg, Ssh0#ssh.session_id, Ssh0),
	    ok = send_bytes(Reply, D),
	    {keep_state, D#data{ssh_params = Ssh}};
	
	{"ssh-connection", "ssh-connection", Method} ->
	    %% Userauth request with a method like "password" or so
	    case lists:member(Method, Ssh0#ssh.userauth_methods) of
		true ->
		    %% Yepp! we support this method
		    case ssh_auth:handle_userauth_request(Msg, Ssh0#ssh.session_id, Ssh0) of
			{authorized, User, {Reply, Ssh}} ->
			    ok = send_bytes(Reply, D),
			    D#data.starter ! ssh_connected,
			    connected_fun(User, Method, D),
			    {next_state, {connected,server},
			     D#data{auth_user = User, 
				    ssh_params = Ssh#ssh{authenticated = true}}};
			{not_authorized, {User, Reason}, {Reply, Ssh}} when Method == "keyboard-interactive" ->
			    retry_fun(User, Reason, D),
			    ok = send_bytes(Reply, D),
			    {next_state, {userauth_keyboard_interactive,server}, D#data{ssh_params = Ssh}};
			{not_authorized, {User, Reason}, {Reply, Ssh}} ->
			    retry_fun(User, Reason, D),
			    ok = send_bytes(Reply, D),
			    {keep_state, D#data{ssh_params = Ssh}}
		    end;
		false ->
		    %% No we do not support this method (=/= none)
		    %% At least one non-erlang client does like this. Retry as the next event
		    {keep_state_and_data,
		     [{next_event, internal, Msg#ssh_msg_userauth_request{method="none"}}]
		    }
	    end;

	%% {"ssh-connection", Expected, Method} when Expected =/= ServiceName -> Do what?
	%% {ServiceName,      Expected, Method} when Expected =/= ServiceName -> Do what?

	{ServiceName, _, _} when ServiceName =/= "ssh-connection" ->
	    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
					   description = "Unknown service"},
		       StateName, D)
    end;

%%---- userauth success to client
handle_event(_, #ssh_msg_userauth_success{}, {userauth,client}, D=#data{ssh_params = Ssh}) ->
    D#data.starter ! ssh_connected,
    {next_state, {connected,client}, D#data{ssh_params=Ssh#ssh{authenticated = true}}};


%%---- userauth failure response to client
handle_event(_, #ssh_msg_userauth_failure{}, {userauth,client}=StateName,
	     D = #data{ssh_params = #ssh{userauth_methods = []}}) ->
    Msg = #ssh_msg_disconnect{code = ?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,
			      description = "Unable to connect using the available"
			                    " authentication methods"},
    disconnect(Msg, StateName, D);

handle_event(_, #ssh_msg_userauth_failure{authentications = Methods}, StateName={userauth,client},
	     D = #data{ssh_params = Ssh0}) ->
    %% The prefered authentication method failed try next method
    Ssh1 = case Ssh0#ssh.userauth_methods of
	       none ->
		   %% Server tells us which authentication methods that are allowed
		   Ssh0#ssh{userauth_methods = string:tokens(Methods, ",")};
	       _ ->
		   %% We already know...
		   Ssh0
	   end,
    case ssh_auth:userauth_request_msg(Ssh1) of
	{disconnect, DisconnectMsg, {Msg, Ssh}} ->
	    send_bytes(Msg, D),
	    disconnect(DisconnectMsg, StateName, D#data{ssh_params = Ssh});
	{"keyboard-interactive", {Msg, Ssh}} ->
	    send_bytes(Msg, D),
	    {next_state, {userauth_keyboard_interactive,client}, D#data{ssh_params = Ssh}};
	{_Method, {Msg, Ssh}} ->
	    send_bytes(Msg, D),
	    {keep_state, D#data{ssh_params = Ssh}}
    end;

%%---- banner to client
handle_event(_, #ssh_msg_userauth_banner{message = Msg}, {userauth,client}, D) ->
    case D#data.ssh_params#ssh.userauth_quiet_mode of
	false -> io:format("~s", [Msg]);
	true -> ok
    end,
    keep_state_and_data;


%%% ######## {userauth_keyboard_interactive, client|server}

handle_event(_, #ssh_msg_userauth_info_request{} = Msg, {userauth_keyboard_interactive, client},
	     #data{ssh_params = Ssh0} = D) ->
    case ssh_auth:handle_userauth_info_request(Msg, Ssh0) of
	{ok, {Reply, Ssh}} ->
	    send_bytes(Reply, D),
	    {next_state, {userauth_keyboard_interactive_info_response,client}, D#data{ssh_params = Ssh}};
	not_ok ->
	    {next_state, {userauth,client}, D, [{next_event, internal, Msg}]}
    end;

handle_event(_, #ssh_msg_userauth_info_response{} = Msg, {userauth_keyboard_interactive, server}, D) ->
    case ssh_auth:handle_userauth_info_response(Msg, D#data.ssh_params) of
	{authorized, User, {Reply, Ssh}} ->
	    send_bytes(Reply, D),
	    D#data.starter ! ssh_connected,
	    connected_fun(User, "keyboard-interactive", D),
	    {next_state, {connected,server}, D#data{auth_user = User,
						    ssh_params = Ssh#ssh{authenticated = true}}};
	{not_authorized, {User, Reason}, {Reply, Ssh}} ->
	    retry_fun(User, Reason, D),
	    send_bytes(Reply, D),
	    {next_state, {userauth,server}, D#data{ssh_params = Ssh}}
    end;

handle_event(_, Msg = #ssh_msg_userauth_failure{}, {userauth_keyboard_interactive, client},
	     #data{ssh_params = Ssh0} = D0) ->
    Prefs = [{Method,M,F,A} || {Method,M,F,A} <- Ssh0#ssh.userauth_preference,
			       Method =/= "keyboard-interactive"],
    D = D0#data{ssh_params = Ssh0#ssh{userauth_preference=Prefs}},
    {next_state, {userauth,client}, D, [{next_event, internal, Msg}]};

handle_event(_, Msg=#ssh_msg_userauth_failure{}, {userauth_keyboard_interactive_info_response, client},
	     #data{ssh_params = Ssh0} = D0) ->
    Opts = Ssh0#ssh.opts,
    D = case proplists:get_value(password, Opts) of
	    undefined ->
		D0;
	    _ ->
		D0#data{ssh_params =
			    Ssh0#ssh{opts =
					 lists:keyreplace(password,1,Opts,
							  {password,not_ok})}} % FIXME:intermodule dependency
	end,
    {next_state, {userauth,client}, D, [{next_event, internal, Msg}]};

handle_event(_, Msg=#ssh_msg_userauth_success{}, {userauth_keyboard_interactive_info_response, client}, D) ->
    {next_state, {userauth,client}, D, [{next_event, internal, Msg}]};

handle_event(_, Msg=#ssh_msg_userauth_info_request{}, {userauth_keyboard_interactive_info_response, client}, D) ->
    {next_state, {userauth_keyboard_interactive,client}, D, [{next_event, internal, Msg}]};


%%% ######## {connected, client|server} ####

handle_event(_, {#ssh_msg_kexinit{},_} = Event, {connected,Role}, D0) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(D0#data.ssh_params),
    D = D0#data{ssh_params = Ssh,
		key_exchange_init_msg = KeyInitMsg},
    send_bytes(SshPacket, D),
    {next_state, {kexinit,Role,renegotiate}, D, [{next_event, internal, Event}]};

handle_event(_, #ssh_msg_disconnect{description=Desc} = Msg, StateName, D0) ->
    {disconnect, _, {{replies,Replies}, _}} =
	ssh_connection:handle_msg(Msg, D0#data.connection_state, role(StateName)),
    {Actions,D} = send_replies(Replies, D0),
    disconnect_fun(Desc, D),
    {stop_and_reply, {shutdown,Desc}, Actions, D};

handle_event(_, #ssh_msg_ignore{}, _, _) ->
    keep_state_and_data;

handle_event(_, #ssh_msg_unimplemented{}, _, _) ->
    keep_state_and_data;

handle_event(_, #ssh_msg_debug{} = Msg, _, D) ->
    debug_fun(Msg, D),
    keep_state_and_data;

handle_event(internal, Msg=#ssh_msg_global_request{},            StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_request_success{},           StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_request_failure{},           StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_open{},              StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_open_confirmation{}, StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_open_failure{},      StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_window_adjust{},     StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_data{},              StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_extended_data{},     StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_eof{},               StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_close{},             StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_request{},           StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_success{},           StateName, D) ->
    handle_connection_msg(Msg, StateName, D);

handle_event(internal, Msg=#ssh_msg_channel_failure{},           StateName, D) ->
    handle_connection_msg(Msg, StateName, D);


handle_event(cast, renegotiate, {connected,Role}, D) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(D#data.ssh_params),
    send_bytes(SshPacket, D),
    timer:apply_after(?REKEY_TIMOUT, gen_statem, cast, [self(), renegotiate]),
    {next_state, {kexinit,Role,renegotiate}, D#data{ssh_params = Ssh,
						    key_exchange_init_msg = KeyInitMsg}};

handle_event(cast, renegotiate, _, _) ->
    %% Already in key-exchange so safe to ignore
    timer:apply_after(?REKEY_TIMOUT, gen_statem, cast, [self(), renegotiate]), % FIXME: not here in original
    keep_state_and_data;


%% Rekey due to sent data limit reached?
handle_event(cast, data_size, {connected,Role}, D) ->
    {ok, [{send_oct,Sent0}]} = inet:getstat(D#data.socket, [send_oct]),
    Sent = Sent0 - D#data.last_size_rekey,
    MaxSent = proplists:get_value(rekey_limit, D#data.opts, 1024000000),
    timer:apply_after(?REKEY_DATA_TIMOUT, gen_statem, cast, [self(), data_size]),
    case Sent >= MaxSent of
	true ->
	    {KeyInitMsg, SshPacket, Ssh} =
		ssh_transport:key_exchange_init_msg(D#data.ssh_params),
	    send_bytes(SshPacket, D),
	    {next_state, {kexinit,Role,renegotiate}, D#data{ssh_params = Ssh,
							    key_exchange_init_msg = KeyInitMsg,
							    last_size_rekey = Sent0}};
	_ ->
	    keep_state_and_data
    end;

handle_event(cast, data_size, _, _) ->
    %% Already in key-exchange so safe to ignore
    timer:apply_after(?REKEY_DATA_TIMOUT, gen_statem, cast, [self(), data_size]),  % FIXME: not here in original
    keep_state_and_data;



handle_event(cast, _, StateName, _) when StateName /= {connected,server},
					 StateName /= {connected,client} ->
    {keep_state_and_data, [postpone]};


handle_event(cast, {adjust_window,ChannelId,Bytes}, {connected,_}, D) ->
    case ssh_channel:cache_lookup(cache(D), ChannelId) of
	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 recv_packet_size = PktSize} = Channel
	  when (WinSize-Bytes) >= 2*PktSize ->
	    %% The peer can send at least two more *full* packet, no hurry.
	    ssh_channel:cache_update(cache(D),
				     Channel#channel{recv_window_pending = Pending + Bytes}),
	    keep_state_and_data;

	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 remote_id = Id} = Channel ->
	    %% Now we have to update the window - we can't receive so many more pkts
	    ssh_channel:cache_update(cache(D),
				     Channel#channel{recv_window_size =
							 WinSize + Bytes + Pending,
						     recv_window_pending = 0}),
	    Msg = ssh_connection:channel_adjust_window_msg(Id, Bytes + Pending),
	    {keep_state, send_msg(Msg,D)};

	undefined ->
	    keep_state_and_data
    end;

handle_event(cast, {reply_request,success,ChannelId}, {connected,_}, D) ->
    case ssh_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = RemoteId} ->
	    Msg = ssh_connection:channel_success_msg(RemoteId),
	    {keep_state, send_msg(Msg,D)};

	undefined ->
	    keep_state_and_data
    end;

handle_event(cast, {request,ChannelPid, ChannelId, Type, Data}, {connected,_}, D) ->
    {keep_state,  handle_request(ChannelPid, ChannelId, Type, Data, false, none, D)};

handle_event(cast, {request,ChannelId,Type,Data}, {connected,_}, D) ->
    {keep_state,  handle_request(ChannelId, Type, Data, false, none, D)};

handle_event(cast, {unknown,Data}, {connected,_}, D) ->
    Msg = #ssh_msg_unimplemented{sequence = Data},
    {keep_state, send_msg(Msg,D)};

%%% Previously handle_sync_event began here
handle_event({call,From}, get_print_info, StateName, D) ->
    Reply =
	try
	    {inet:sockname(D#data.socket),
	     inet:peername(D#data.socket)
	    }
	of
	    {{ok,Local}, {ok,Remote}} -> 
		{{Local,Remote},io_lib:format("statename=~p",[StateName])};
	    _ ->
		{{"-",0},"-"}
	catch
	    _:_ ->
		{{"?",0},"?"}
	end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {connection_info, Options}, _, D) ->
    Info = fold_keys(Options, fun conn_info/2, D),
    {keep_state_and_data, [{reply,From,Info}]};

handle_event({call,From}, {channel_info,ChannelId,Options}, _, D) ->
    case ssh_channel:cache_lookup(cache(D), ChannelId) of
	#channel{} = Channel ->
	    Info = fold_keys(Options, fun chann_info/2, Channel),
	    {keep_state_and_data, [{reply,From,Info}]};
	undefined ->
	    {keep_state_and_data, [{reply,From,[]}]}
    end;


handle_event({call,From}, {info, all}, _, D) ->
    Result = ssh_channel:cache_foldl(fun(Channel, Acc) ->
					     [Channel | Acc]
				     end,
				     [], cache(D)),
    {keep_state_and_data, [{reply, From, {ok,Result}}]};
    
handle_event({call,From}, {info, ChannelPid}, _, D) ->
    Result = ssh_channel:cache_foldl(
	       fun(Channel, Acc) when Channel#channel.user == ChannelPid ->
		       [Channel | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], cache(D)),
    {keep_state_and_data, [{reply, From, {ok,Result}}]};

handle_event({call,From}, stop, StateName, D0) ->
    {disconnect, _Reason, {{replies, Replies}, Connection}} =
	ssh_connection:handle_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
						      description = "User closed down connection"},
				  D0#data.connection_state,
				  role(StateName)),
    {Repls,D} = send_replies(Replies, D0),
    {stop_and_reply, normal, [{reply,From,ok}|Repls], D#data{connection_state=Connection}};

handle_event({call,_}, _, StateName, _) when StateName /= {connected,server},
					     StateName /= {connected,client}  ->
    {keep_state_and_data, [postpone]};

handle_event({call,From}, {request, ChannelPid, ChannelId, Type, Data, Timeout}, {connected,_}, D0) ->
    D = handle_request(ChannelPid, ChannelId, Type, Data, true, From, D0),
    %% Note reply to channel will happen later when reply is recived from peer on the socket
    start_channel_request_timer(ChannelId, From, Timeout),
    {keep_state, cache_request_idle_timer_check(D)};

handle_event({call,From}, {request, ChannelId, Type, Data, Timeout}, {connected,_}, D0) ->
    D = handle_request(ChannelId, Type, Data, true, From, D0),
    %% Note reply to channel will happen later when reply is recived from peer on the socket
    start_channel_request_timer(ChannelId, From, Timeout),
    {keep_state, cache_request_idle_timer_check(D)};

handle_event({call,From}, {data, ChannelId, Type, Data, Timeout}, {connected,_}, D0) ->
    {{replies, Replies}, Connection} =
	ssh_connection:channel_data(ChannelId, Type, Data, D0#data.connection_state, From),
    {Repls,D} = send_replies(Replies, D0#data{connection_state = Connection}),
    start_channel_request_timer(ChannelId, From, Timeout), % FIXME: No message exchange so why?
    {keep_state, D, Repls};

handle_event({call,From}, {eof, ChannelId}, {connected,_}, D0) ->
    case ssh_channel:cache_lookup(cache(D0), ChannelId) of
	#channel{remote_id = Id, sent_close = false} ->
	    D = send_msg(ssh_connection:channel_eof_msg(Id), D0),
	    {keep_state, D, [{reply,From,ok}]};
	_ ->
	    {keep_state, D0, [{reply,From,{error,closed}}]}
    end;

handle_event({call,From},
	     {open, ChannelPid, Type, InitialWindowSize, MaxPacketSize, Data, Timeout},
	     {connected,_},
	     D0) ->
    erlang:monitor(process, ChannelPid),
    {ChannelId, D1} = new_channel_id(D0),
    D2 = send_msg(ssh_connection:channel_open_msg(Type, ChannelId,
						  InitialWindowSize,
						  MaxPacketSize, Data),
		  D1),
    ssh_channel:cache_update(cache(D2), 
			     #channel{type = Type,
				      sys = "none",
				      user = ChannelPid,
				      local_id = ChannelId,
				      recv_window_size = InitialWindowSize,
				      recv_packet_size = MaxPacketSize,
				      send_buf = queue:new()
				     }),
    D = add_request(true, ChannelId, From, D2),
    start_channel_request_timer(ChannelId, From, Timeout),
    {keep_state, cache_cancel_idle_timer(D)};

handle_event({call,From}, {send_window, ChannelId}, {connected,_}, D) ->
    Reply = case ssh_channel:cache_lookup(cache(D), ChannelId) of
		#channel{send_window_size = WinSize,
			 send_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {recv_window, ChannelId}, {connected,_}, D) ->
    Reply = case ssh_channel:cache_lookup(cache(D), ChannelId) of
		#channel{recv_window_size = WinSize,
			 recv_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {close, ChannelId}, {connected,_}, D0) ->
    case ssh_channel:cache_lookup(cache(D0), ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    D1 = send_msg(ssh_connection:channel_close_msg(Id), D0),
	    ssh_channel:cache_update(cache(D1), Channel#channel{sent_close = true}),
	    {keep_state, cache_request_idle_timer_check(D1), [{reply,From,ok}]};
	undefined ->
	    {keep_state_and_data, [{reply,From,ok}]}
    end;


%%===== Reception of encrypted bytes, decryption and framing
handle_event(info, {Proto, Sock, Info}, {hello,_}, #data{socket = Sock,
							 transport_protocol = Proto}) ->
    case Info of
	"SSH-" ++ _ ->  
	    {keep_state_and_data, [{next_event, internal, {version_exchange,Info}}]};
	_ ->
	    {keep_state_and_data, [{next_event, internal, {info_line,Info}}]}
    end;

handle_event(info, {Proto, Sock, NewData}, StateName, D0 = #data{socket = Sock,
								 transport_protocol = Proto}) ->
    try ssh_transport:handle_packet_part(
	  D0#data.decrypted_data_buffer,
	  <<(D0#data.encrypted_data_buffer)/binary, NewData/binary>>,
	  D0#data.undecrypted_packet_length,
	  D0#data.ssh_params)
    of
	{packet_decrypted, DecryptedBytes, EncryptedDataRest, Ssh1} ->
	    D = D0#data{ssh_params =
			    Ssh1#ssh{recv_sequence = ssh_transport:next_seqnum(Ssh1#ssh.recv_sequence)},
			decrypted_data_buffer = <<>>,
			undecrypted_packet_length = undefined,
			encrypted_data_buffer = EncryptedDataRest},
	    try
		ssh_message:decode(set_kex_overload_prefix(DecryptedBytes,D))
	    of
		Msg = #ssh_msg_kexinit{} ->
		    {keep_state, D, [{next_event, internal, {Msg,DecryptedBytes}},
				     {next_event, internal, prepare_next_packet}
				    ]};
		Msg ->
		    {keep_state, D, [{next_event, internal, Msg},
				     {next_event, internal, prepare_next_packet}
				    ]}
	    catch
		_C:_E  ->
		    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
						   description = "Encountered unexpected input"},
			       StateName, D)
	    end;

	{get_more, DecryptedBytes, EncryptedDataRest, RemainingSshPacketLen, Ssh1} ->
	    %% Here we know that there are not enough bytes in
	    %% EncryptedDataRest to use. We must wait for more.
	    inet:setopts(Sock, [{active, once}]),
	    {keep_state, D0#data{encrypted_data_buffer = EncryptedDataRest,
				 decrypted_data_buffer = DecryptedBytes,
				 undecrypted_packet_length = RemainingSshPacketLen,
				 ssh_params = Ssh1}};

	{bad_mac, Ssh1} ->
	    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
					   description = "Bad mac"},
		       StateName, D0#data{ssh_params=Ssh1});

	{error, {exceeds_max_size,PacketLen}} ->
	    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
					   description = "Bad packet length "
					   ++ integer_to_list(PacketLen)},
		       StateName, D0)
    catch
	_C:_E ->
	    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
					   description = "Bad packet"},
		       StateName, D0)
    end;


%%%==== 
handle_event(internal, prepare_next_packet, _, D) ->
    Enough =  erlang:max(8, D#data.ssh_params#ssh.decrypt_block_size),
    case size(D#data.encrypted_data_buffer) of
	Sz when Sz >= Enough ->
	    self() ! {D#data.transport_protocol, D#data.socket, <<>>};
	_ ->
	    ok
    end,
    inet:setopts(D#data.socket, [{active, once}]),
    keep_state_and_data;

handle_event(info, {CloseTag,Socket}, StateName,
	     D = #data{socket = Socket,
		       transport_close_tag = CloseTag}) ->
    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
				   description = "Connection closed"},
	       StateName, D);

handle_event(info, {timeout, {_, From} = Request}, _,
	     #data{connection_state = #connection{requests = Requests} = C0} = D) ->
    case lists:member(Request, Requests) of
	true ->
	    %% A channel request is not answered in time. Answer {error,timeout}
	    %% to the caller
	    C = C0#connection{requests = lists:delete(Request, Requests)},
	    {keep_state, D#data{connection_state=C}, [{reply,From,{error,timeout}}]};
	false ->
	    %% The request is answered - just ignore the timeout
	    keep_state_and_data
    end;

%%% Handle that ssh channels user process goes down
handle_event(info, {'DOWN', _Ref, process, ChannelPid, _Reason}, _, D0) ->
    {{replies, Replies}, D1} = handle_channel_down(ChannelPid, D0),
    {Repls, D} = send_replies(Replies, D1),
    {keep_state, D, Repls};

%%% So that terminate will be run when supervisor is shutdown
handle_event(info, {'EXIT', _Sup, Reason}, _, _) ->
    {stop, {shutdown, Reason}};

handle_event(info, check_cache, _, D) ->
    {keep_state, cache_check_set_idle_timer(D)};

handle_event(info, UnexpectedMessage, StateName, D = #data{ssh_params = Ssh}) ->
    case unexpected_fun(UnexpectedMessage, D) of
	report ->
	    Msg = lists:flatten(
		    io_lib:format(
		      "Unexpected message '~p' received in state '~p'\n"
		      "Role: ~p\n"
		      "Peer: ~p\n"
		      "Local Address: ~p\n", [UnexpectedMessage,
					      StateName,
					      Ssh#ssh.role, 
					      Ssh#ssh.peer,
					      proplists:get_value(address, Ssh#ssh.opts)])),
	    error_logger:info_report(Msg),
	    keep_state_and_data;

	skip ->
	    keep_state_and_data;

	Other ->
	    Msg = lists:flatten(
		    io_lib:format("Call to fun in 'unexpectedfun' failed:~n"
				  "Return: ~p\n"
				  "Message: ~p\n"
				  "Role: ~p\n"
				  "Peer: ~p\n"
				  "Local Address: ~p\n", [Other,
							  UnexpectedMessage,
							  Ssh#ssh.role,
							  element(2,Ssh#ssh.peer),
							  proplists:get_value(address, Ssh#ssh.opts)]
				 )),
	    error_logger:error_report(Msg),
	    keep_state_and_data
    end;

handle_event(internal, {disconnect,Msg,_Reason}, StateName, D) ->
    disconnect(Msg, StateName, D);

handle_event(Type, Ev, StateName, D) ->
    Descr =
	case catch atom_to_list(element(1,Ev)) of
	    "ssh_msg_" ++_ when Type==internal ->
		"Message in wrong state";
	    _ ->
		"Internal error"
	end,
    disconnect(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				   description = Descr},
	       StateName, D).


%%--------------------------------------------------------------------
-spec terminate(any(),
		state_name(),
		#data{}
	       ) -> finalize_termination_result() .
			
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
    finalize_termination(StateName, State);

%% terminate({shutdown,Msg}, StateName, State0) when is_record(Msg,ssh_msg_disconnect)->
%%     State = send_msg(Msg, State0),
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

format_status(normal, [_, _StateName, D]) ->
    [{data, [{"State", D}]}];
format_status(terminate, [_, _StateName, D]) ->
    DataPropList0 = fmt_stat_rec(record_info(fields, data), D,
				 [decrypted_data_buffer,
				  encrypted_data_buffer,
				  key_exchange_init_msg,
				  user_passwords,
				  opts,
				  inet_initial_recbuf_size]),
    SshPropList = fmt_stat_rec(record_info(fields, ssh), D#data.ssh_params,
			       [c_keyinit,
				s_keyinit,
				send_mac_key,
				send_mac_size,
				recv_mac_key,
				recv_mac_size,
				encrypt_keys,
				encrypt_ctx,
				decrypt_keys,
				decrypt_ctx,
				compress_ctx,
				decompress_ctx,
				shared_secret,
				exchanged_hash,
				session_id,
				keyex_key,
				keyex_info,
				available_host_keys]),
    DataPropList = lists:keyreplace(ssh_params, 1, DataPropList0,
				    {ssh_params,SshPropList}),
    [{data, [{"State", DataPropList}]}].


fmt_stat_rec(FieldNames, Rec, Exclude) ->
    Values = tl(tuple_to_list(Rec)),
    [P || {K,_} = P <- lists:zip(FieldNames, Values),
	  not lists:member(K, Exclude)].

%%--------------------------------------------------------------------
-spec code_change(term() | {down,term()},
		  state_name(),
		  #data{},
		  term()
		 ) -> {gen_statem:callback_mode(), state_name(), #data{}}.

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

code_change(_OldVsn, StateName, State, _Extra) ->
    {handle_event_function, StateName, State}.


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
    ok = socket_control(Socket, Pid, Options),
    Pid.

%%--------------------------------------------------------------------
%% Stopping
-type finalize_termination_result() :: ok .

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

%%--------------------------------------------------------------------
%% "Invert" the Role
peer_role(client) -> server;
peer_role(server) -> client.

%%--------------------------------------------------------------------
%% StateName to Role
role({_,Role}) -> Role;
role({_,Role,_}) -> Role.

%%--------------------------------------------------------------------
%% Check the StateName to see if we are in the renegotiation phase
renegotiation({_,_,ReNeg}) -> ReNeg == renegotiation;
renegotiation(_) -> false.

%%--------------------------------------------------------------------
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


handle_connection_msg(Msg, StateName, D0 = #data{starter = User,
						 connection_state = Connection0,
						 event_queue = Qev0}) ->
    Renegotiation = renegotiation(StateName),
    Role = role(StateName),
    try ssh_connection:handle_msg(Msg, Connection0, Role) of
	{{replies, Replies}, Connection} ->
	    {Repls, D} = 
		case StateName of
		    {connected,_} ->
			send_replies(Replies, D0#data{connection_state=Connection});
		    _ ->
			{ConnReplies, NonConnReplies} = lists:splitwith(fun not_connected_filter/1, Replies),
			send_replies(NonConnReplies, D0#data{event_queue = Qev0 ++ ConnReplies})
		end,
	    {keep_state, D, Repls};

	{noreply, Connection} ->
	    {keep_state, D0#data{connection_state = Connection}};

	{disconnect, Reason0, {{replies, Replies}, Connection}} ->
	   {Repls, D} = send_replies(Replies, D0#data{connection_state = Connection}),
	   case {Reason0,Role} of
	       {{_, Reason}, client} when ((StateName =/= {connected,client}) and (not Renegotiation)) ->
		   User ! {self(), not_connected, Reason};
	       _ ->
		   ok
	   end,
	   {stop_and_reply, {shutdown,normal}, Repls, D#data{connection_state = Connection}}

    catch
	_:Error ->
	    {disconnect, _Reason, {{replies, Replies}, Connection}} =
		ssh_connection:handle_msg(
		  #ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
				      description = "Internal error"},
		  Connection0, Role),
	    {Repls, D} = send_replies(Replies, D0#data{connection_state = Connection}),
	    {stop_and_reply, {shutdown,Error}, Repls, D#data{connection_state = Connection}}
    end.


set_kex_overload_prefix(Msg = <<?BYTE(Op),_/binary>>, #data{ssh_params=SshParams})
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
set_kex_overload_prefix(Msg, _) ->
    Msg.

kex(#ssh{algorithms=#alg{kex=Kex}}) -> Kex;
kex(_) -> undefined.

cache(#data{connection_state=C}) -> C#connection.channel_cache.
    

%%%----------------------------------------------------------------
handle_request(ChannelPid, ChannelId, Type, Data, WantReply, From, D) ->
    case ssh_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    update_sys(cache(D), Channel, Type, ChannelPid),
	    send_msg(ssh_connection:channel_request_msg(Id, Type, WantReply, Data),
		     add_request(WantReply, ChannelId, From, D));
	undefined ->
	    D
    end.

handle_request(ChannelId, Type, Data, WantReply, From, D) ->
    case ssh_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = Id} ->
	    send_msg(ssh_connection:channel_request_msg(Id, Type, WantReply, Data),
		     add_request(WantReply, ChannelId, From, D));
	undefined ->
	    D
    end.

%%%----------------------------------------------------------------
handle_channel_down(ChannelPid, D) ->
    ssh_channel:cache_foldl(
	       fun(Channel, Acc) when Channel#channel.user == ChannelPid ->
		       ssh_channel:cache_delete(cache(D),
						Channel#channel.local_id),
		       Acc;
		  (_,Acc) ->
		       Acc
	       end, [], cache(D)),
    {{replies, []}, cache_check_set_idle_timer(D)}.


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
    disconnect_fun(Description, State),
    {stop, {shutdown,Description}, State}.

%%%----------------------------------------------------------------
counterpart_versions(NumVsn, StrVsn, #ssh{role = server} = Ssh) ->
    Ssh#ssh{c_vsn = NumVsn , c_version = StrVsn};
counterpart_versions(NumVsn, StrVsn, #ssh{role = client} = Ssh) ->
    Ssh#ssh{s_vsn = NumVsn , s_version = StrVsn}.

%%%----------------------------------------------------------------
conn_info(client_version, #data{ssh_params=S}) -> {S#ssh.c_vsn, S#ssh.c_version};
conn_info(server_version, #data{ssh_params=S}) -> {S#ssh.s_vsn, S#ssh.s_version};
conn_info(peer,           #data{ssh_params=S}) -> S#ssh.peer;
conn_info(user,                             D) -> D#data.auth_user;
conn_info(sockname, D) -> {ok, SockName} = inet:sockname(D#data.socket),
			  SockName;
%% dbg options ( = not documented):
conn_info(socket, D) -> D#data.socket;
conn_info(chan_ids, D) -> 
    ssh_channel:cache_foldl(fun(#channel{local_id=Id}, Acc) ->
				    [Id | Acc]
			    end, [], cache(D)).

%%%----------------------------------------------------------------
chann_info(recv_window, C) ->
    {{win_size,    C#channel.recv_window_size},
     {packet_size, C#channel.recv_packet_size}};
chann_info(send_window, C) ->
    {{win_size,    C#channel.send_window_size},
     {packet_size, C#channel.send_packet_size}};
%% dbg options ( = not documented):
chann_info(pid, C) ->
    C#channel.user.

%%%----------------------------------------------------------------
%% Assisting meta function for the *_info functions
fold_keys(Keys, Fun, Extra) ->
    lists:foldr(fun(Key, Acc) ->
			try Fun(Key, Extra) of
			    Value -> [{Key,Value}|Acc]
			catch
			    _:_ -> Acc
			end
		end, [], Keys).

%%%----------------------------------------------------------------
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
disconnect_fun({disconnect,Msg}, D) ->
    disconnect_fun(Msg, D);
disconnect_fun(Reason,  #data{opts=Opts}) ->
    case proplists:get_value(disconnectfun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(Reason)
     end.

unexpected_fun(UnexpectedMessage, #data{opts = Opts,
					ssh_params = #ssh{peer = {_,Peer} }
				       } ) ->
    case proplists:get_value(unexpectedfun, Opts) of
	undefined ->
	    report;
	Fun ->
	    catch Fun(UnexpectedMessage, Peer)
    end.


debug_fun(#ssh_msg_debug{always_display = Display,
			 message = DbgMsg,
			 language = Lang},
	  #data{opts = Opts}) ->
    case proplists:get_value(ssh_msg_debug_fun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(self(), Display, DbgMsg, Lang)
    end.


connected_fun(User, Method, #data{ssh_params = #ssh{peer = {_,Peer}},
				  opts = Opts}) ->
    case proplists:get_value(connectfun, Opts) of
	undefined ->
	    ok;
	Fun ->
	    catch Fun(User, Peer, Method)
    end.

retry_fun(_, undefined, _) ->
    ok;
retry_fun(User, Reason, #data{ssh_params = #ssh{opts = Opts,
						peer = {_,Peer}
					       }}) ->
    {Tag,Info} =
	case Reason of
	    {error, Error} ->
		{failfun, Error};
	    _ ->
		{infofun, Reason}
	end,
    Fun = proplists:get_value(Tag, Opts, fun(_,_)-> ok end),
    try erlang:fun_info(Fun, arity)
    of
	{arity, 2} -> %% Backwards compatible
	    catch Fun(User, Info);
	{arity, 3} ->
	    catch Fun(User, Peer, Info);
	_ ->
	    ok
    catch
	_:_ ->
	    ok
    end.

%%%----------------------------------------------------------------
%%% Cache idle timer that closes the connection if there are no
%%% channels open for a while.

cache_init_idle_timer(D) ->
    case proplists:get_value(idle_time, D#data.opts, infinity) of
	infinity ->
	    D#data{idle_timer_value = infinity,
		   idle_timer_ref = infinity	% A flag used later...
		  };
	IdleTime ->
	    %% We dont want to set the timeout on first connect
	    D#data{idle_timer_value = IdleTime}
    end.


cache_check_set_idle_timer(D = #data{idle_timer_ref = undefined,
				     idle_timer_value = IdleTime}) ->
    %% No timer set - shall we set one?
    case ssh_channel:cache_info(num_entries, cache(D)) of
	0 when IdleTime == infinity ->
	    %% No. Meaningless to set a timer that fires in an infinite time...
	    D;
	0 ->
	    %% Yes, we'll set one since the cache is empty and it should not
	    %% be that for a specified time
	    D#data{idle_timer_ref =
		       erlang:send_after(IdleTime, self(), {'EXIT',[],"Timeout"})};
	_ ->
	    %% No - there are entries in the cache
	    D
    end;
cache_check_set_idle_timer(D) ->
    %% There is already a timer set or the timeout time is infinite
    D.
    

cache_cancel_idle_timer(D) ->
    case D#data.idle_timer_ref of
	infinity -> 
	    %% The timer is not activated
	    D;
	undefined ->
	    %% The timer is already cancelled
	    D;
	TimerRef -> 
	    %% The timer is active
	    erlang:cancel_timer(TimerRef),
	    D#data{idle_timer_ref = undefined}
    end.


cache_request_idle_timer_check(D = #data{idle_timer_value = infinity}) ->
    D;
cache_request_idle_timer_check(D = #data{idle_timer_value = IdleTime}) ->
    erlang:send_after(IdleTime, self(), check_cache),
    D.

%%%----------------------------------------------------------------
start_channel_request_timer(_,_, infinity) ->
    ok;
start_channel_request_timer(Channel, From, Time) ->
    erlang:send_after(Time, self(), {timeout, {Channel, From}}).

%%%----------------------------------------------------------------
%%% Connection start and initalization helpers

socket_control(Socket, Pid, Options) ->
    {_, TransportCallback, _} =		   % For example {_,gen_tcp,_}
	proplists:get_value(transport, Options, ?DefaultTransport),
    case TransportCallback:controlling_process(Socket, Pid) of
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

