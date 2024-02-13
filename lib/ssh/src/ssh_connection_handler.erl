%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-moduledoc false.

-behaviour(gen_statem).

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_auth.hrl").
-include("ssh_connect.hrl").

-include("ssh_fsm.hrl").

%%====================================================================
%%% Exports
%%====================================================================

%%% Start and stop
-export([start_link/4, start_link/5,
         takeover/4,
	 stop/1
	]).

%%% Internal application API
-export([available_hkey_algorithms/2,
	 open_channel/6,
         start_channel/5,
         handshake/2,
         handle_direct_tcpip/6,
	 request/6, request/7,
	 reply_request/3,
         global_request/5,
         handle_ssh_msg_ext_info/2,
	 send/5,
         send_bytes/2,
         send_msg/2,
	 send_eof/2,
         send_disconnect/6,
         send_disconnect/7,
         store/3,
         retrieve/2,
	 info/1, info/2,
	 connection_info/2,
	 channel_info/3,
	 adjust_window/3, close/2,
	 disconnect/4,
	 get_print_info/1,
         set_sock_opts/2, get_sock_opts/2,
         prohibited_sock_option/1
	]).

%%% Behaviour callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3,
	 format_status/2, code_change/4]).

%%% Exports not intended to be used :). They are used for spawning and tests
-export([init_ssh_record/3,		   % Export of this internal function
					   % intended for low-level protocol test suites
	 renegotiate/1, alg/1 % Export intended for test cases
	]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1,
         ssh_dbg_on/1, ssh_dbg_off/1,
         ssh_dbg_format/2, ssh_dbg_format/3]).


-define(call_disconnectfun_and_log_cond(LogMsg, DetailedText, StateName, D),
        call_disconnectfun_and_log_cond(LogMsg, DetailedText, ?MODULE, ?LINE, StateName, D)).

%%====================================================================
%% Start / stop
%%====================================================================

start_link(Role, Address, Socket, Options) ->
    start_link(Role, Address, undefined, Socket, Options).

start_link(Role, _Address=#address{}, Id, Socket, Options) ->
    case gen_statem:start_link(?MODULE,
                               [Role, Socket, Options],
                               [{spawn_opt, [{message_queue_data,off_heap}]}]) of

        {ok, Pid} when Id =/= undefined ->
            %% Announce the ConnectionRef to the system supervisor so it could
            %%   1) initiate the socket handover, and
            %%   2) be returned to whoever called for example ssh:connect; the Pid
            %%      returned from this function is "consumed" by the subsystem
            %%      supervisor.
            ?GET_INTERNAL_OPT(user_pid,Options) ! {new_connection_ref, Id, Pid},
            {ok, Pid};

        Others ->
            Others
    end.


takeover(ConnPid, client, Socket, Options) ->
    group_leader(group_leader(), ConnPid),
    takeover(ConnPid, common, Socket, Options);

takeover(ConnPid, _, Socket, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    case Callback:controlling_process(Socket, ConnPid) of
        ok ->
            gen_statem:cast(ConnPid, socket_control),
            NegTimeout = ?GET_INTERNAL_OPT(negotiation_timeout,
                                           Options,
                                           ?GET_OPT(negotiation_timeout, Options)
                                          ),
            handshake(ConnPid, erlang:monitor(process,ConnPid), NegTimeout);
        {error, Reason}	->
            {error, Reason}
    end.

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
%%% Some other module has decided to disconnect.

-spec disconnect(Code::integer(), Details::iodata(),
                      Module::atom(), Line::integer()) -> no_return().
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

% Preferable called with the macro ?DISCONNECT

disconnect(Code, DetailedText, Module, Line) ->
    throw({keep_state_and_data,
	   [{next_event, internal, {send_disconnect, Code, DetailedText, Module, Line}}]}).

%%--------------------------------------------------------------------
%%% Open a channel in the connection to the peer, that is, do the ssh
%%% signalling with the peer.
-spec open_channel(connection_ref(), 
		   string(),
		   iodata(),
		   pos_integer() | undefined,
		   pos_integer() | undefined,
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
%%% Start a channel handling process in the superviser tree
-spec start_channel(connection_ref(), atom(), channel_id(), list(), term()) ->
                           {ok, pid()} | {error, term()}.

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
start_channel(ConnectionHandler, CallbackModule, ChannelId, Args, Exec) ->
    {ok, {SubSysSup,Role,Opts}} = call(ConnectionHandler, get_misc),
    ssh_subsystem_sup:start_channel(Role, SubSysSup,
                                    ConnectionHandler, CallbackModule, ChannelId,
                                    Args, Exec, Opts).

%%--------------------------------------------------------------------
handle_direct_tcpip(ConnectionHandler, ListenHost, ListenPort, ConnectToHost, ConnectToPort, Timeout) ->
    call(ConnectionHandler, {handle_direct_tcpip, ListenHost, ListenPort, ConnectToHost, ConnectToPort, Timeout}).

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
global_request(ConnectionHandler, Type, true, Data, Timeout) ->
    call(ConnectionHandler, {global_request, Type, Data, Timeout});
global_request(ConnectionHandler, Type, false, Data, _) ->
    cast(ConnectionHandler, {global_request, Type, Data}).

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
connection_info(ConnectionHandler, []) ->
    connection_info(ConnectionHandler, conn_info_keys());
connection_info(ConnectionHandler, Key) when is_atom(Key) ->
    case connection_info(ConnectionHandler, [Key]) of
        [{Key,Val}] -> {Key,Val};
        Other -> Other
    end;
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


%%--------------------------------------------------------------------
store(ConnectionHandler, Key, Value) ->
    cast(ConnectionHandler, {store,Key,Value}).

retrieve(#connection{options=Opts}, Key) ->
    try ?GET_INTERNAL_OPT(Key, Opts) of
        Value ->
            {ok,Value}
    catch
        error:{badkey,Key} ->
            undefined
    end;
retrieve(ConnectionHandler, Key) ->
    call(ConnectionHandler, {retrieve,Key}).

%%--------------------------------------------------------------------
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
set_sock_opts(ConnectionRef, SocketOptions) ->
    try lists:foldr(fun({Name,_Val}, Acc) ->
                            case prohibited_sock_option(Name) of
                                true -> [Name|Acc];
                                false -> Acc
                            end
                    end, [], SocketOptions)
    of
        [] ->
            call(ConnectionRef, {set_sock_opts,SocketOptions});
        Bad ->
            {error, {not_allowed,Bad}}
    catch
        _:_ ->
            {error, badarg}
    end.

prohibited_sock_option(active)    -> true;
prohibited_sock_option(deliver)   -> true;
prohibited_sock_option(mode)      -> true;
prohibited_sock_option(packet)    -> true;
prohibited_sock_option(_) -> false.

%%--------------------------------------------------------------------
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
get_sock_opts(ConnectionRef, SocketGetOptions) ->
    call(ConnectionRef, {get_sock_opts,SocketGetOptions}).

%%====================================================================
%% Test support
%%====================================================================
%%--------------------------------------------------------------------
-spec renegotiate(connection_ref()
		 ) -> ok.
%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
renegotiate(ConnectionHandler) ->
    cast(ConnectionHandler, force_renegotiate).

%%--------------------------------------------------------------------
alg(ConnectionHandler) ->
    call(ConnectionHandler, get_alg).

%%====================================================================
%% Intitialisation
%%====================================================================
init([Role, Socket, Opts]) when Role==client ; Role==server ->
    process_flag(trap_exit, true),
    %% ssh_params will be updated after receiving socket_control event
    %% in wait_for_socket state;
    D = #data{socket = Socket, ssh_params = #ssh{role = Role, opts = Opts}},
    {ok, {wait_for_socket, Role}, D}.

%%%----------------------------------------------------------------
%%% Connection start and initialization helpers
init_connection_record(Role, Socket, Opts) ->
    {WinSz, PktSz} = init_inet_buffers_window(Socket),
    C = #connection{channel_cache = ssh_client_channel:cache_create(),
                    channel_id_seed = 0,
                    suggest_window_size = WinSz,
                    suggest_packet_size = PktSz,
                    requests = [],
                    options = Opts,
                    sub_system_supervisor = ?GET_INTERNAL_OPT(subsystem_sup, Opts)
                   },
    case Role of
        server ->
            C#connection{cli_spec =
                             ?GET_OPT(ssh_cli, Opts, {ssh_cli,[?GET_OPT(shell, Opts)]}),
                         exec =
                             ?GET_OPT(exec, Opts)};
        client ->
            C
    end.

init_ssh_record(Role, Socket, Opts) ->
    %% Export of this internal function is
    %% intended for low-level protocol test suites
    {ok,PeerAddr} = inet:peername(Socket),
    init_ssh_record(Role, Socket, PeerAddr, Opts).

init_ssh_record(Role, Socket, PeerAddr, Opts) ->
    AuthMethods = ?GET_OPT(auth_methods, Opts),
    S0 = #ssh{role = Role,
	      opts = Opts,
	      userauth_supported_methods = AuthMethods,
	      available_host_keys = available_hkey_algorithms(Role, Opts),
	      random_length_padding = ?GET_OPT(max_random_length_padding, Opts)
	   },

    {Vsn, Version} = ssh_transport:versions(Role, Opts),
    LocalName = case inet:sockname(Socket) of
                    {ok,Local} -> Local;
                    _ -> undefined
                end,
    case Role of
	client ->
	    PeerName = case ?GET_INTERNAL_OPT(host, Opts, element(1,PeerAddr)) of
                           PeerIP when is_tuple(PeerIP) ->
                               inet_parse:ntoa(PeerIP);
                           PeerName0 when is_atom(PeerName0) ->
                               atom_to_list(PeerName0);
                           PeerName0 when is_list(PeerName0) ->
                               PeerName0
                       end,
            S1 =
                S0#ssh{c_vsn = Vsn,
                       c_version = Version,
                       opts = ?PUT_INTERNAL_OPT({io_cb, case ?GET_OPT(user_interaction, Opts) of
                                                            true ->  ssh_io;
                                                            false -> ssh_no_io
                                                        end},
                                                Opts),
                       userauth_quiet_mode = ?GET_OPT(quiet_mode, Opts),
                       peer = {PeerName, PeerAddr},
                       local = LocalName
                      },
            S1#ssh{userauth_pubkeys = [K || K <- ?GET_OPT(pref_public_key_algs, Opts),
                                            is_usable_user_pubkey(K, S1)
                                      ]
                  };

	server ->
	    S0#ssh{s_vsn = Vsn,
		   s_version = Version,
		   userauth_methods = string:tokens(AuthMethods, ","),
		   kb_tries_left = 3,
		   peer = {undefined, PeerAddr},
                   local = LocalName
		  }
    end.


handshake(Pid, Ref, Timeout) ->
    receive
	{Pid, ssh_connected} ->
	    erlang:demonitor(Ref, [flush]),
	    {ok, Pid};
	{Pid, {not_connected, Reason}} ->
	    erlang:demonitor(Ref, [flush]),
	    {error, Reason};
	{'DOWN', Ref, process, Pid, {shutdown, Reason}} ->
	    {error, Reason};
	{'DOWN', Ref, process, Pid, Reason} ->
	    {error, Reason};
        {'EXIT',_,Reason} ->
            stop(Pid),
            {error, {exit,Reason}}
    after Timeout ->
	    erlang:demonitor(Ref, [flush]),
	    ssh_connection_handler:stop(Pid),
	    {error, timeout}
    end.

handshake(Msg, #data{starter = User}) ->
    User ! {self(), Msg}.

%%====================================================================
%% gen_statem callbacks
%%====================================================================
%%--------------------------------------------------------------------
-type event_content() ::  any().

-type renegotiate_flag() :: init | renegotiate.

-type state_name() :: 
        {hello,                     role()                    }
      | {kexinit,                   role(), renegotiate_flag()}
      | {key_exchange,              role(), renegotiate_flag()}
      | {key_exchange_dh_gex_init,  server, renegotiate_flag()}
      | {key_exchange_dh_gex_reply, client, renegotiate_flag()}
      | {new_keys,                  role(), renegotiate_flag()}
      | {ext_info,                  role(), renegotiate_flag()}
      | {service_request,           role()                    }
      | {userauth,                  role()                    }
      | {userauth_keyboard_interactive,       role()          }
      | {userauth_keyboard_interactive_extra, server          }
      | {userauth_keyboard_interactive_info_response, client  }
      | {connected,                 role()                    }
	.

%% The state names must fulfill some rules regarding
%% where the role() and the renegotiate_flag() is placed:

-spec renegotiation(state_name()) -> boolean().
renegotiation({_,_,ReNeg}) -> ReNeg == renegotiate;
renegotiation(_) -> false.


-define(CONNECTED(StateName),
        (element(1,StateName) == connected orelse
         element(1,StateName) == ext_info ) ).

-spec handle_event(gen_statem:event_type(),
		   event_content(),
		   state_name(),
		   #data{}
		  ) -> gen_statem:event_handler_result(state_name()) .

-define(CONNECTION_MSG(Msg),
        [{next_event, internal, prepare_next_packet},
         {next_event,internal,{conn_msg,Msg}}]).

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
callback_mode() ->
    [handle_event_function,
     state_enter].

%%% ######## {hello, client|server} ####
%% The very first event that is sent when ssh_connection_handler
%% becomes owner process for Socket
handle_event(cast, socket_control, {wait_for_socket, Role},
             #data{socket = Socket, ssh_params = #ssh{opts = Opts}}) ->
    case inet:peername(Socket) of
        {ok, PeerAddr} ->
            try
                {Protocol, Callback, CloseTag} = ?GET_OPT(transport, Opts),
                D = #data{starter = ?GET_INTERNAL_OPT(user_pid, Opts),
                          socket = Socket,
                          transport_protocol = Protocol,
                          transport_cb = Callback,
                          transport_close_tag = CloseTag,
                          ssh_params = init_ssh_record(Role, Socket, PeerAddr, Opts),
                          connection_state = init_connection_record(Role, Socket, Opts)
                         },
                NextEvent = {next_event, internal, socket_ready},
                {next_state, {hello,Role}, D, NextEvent}
            catch
                _:{error,Error} -> {stop, {error,Error}};
                error:Error ->     {stop, {error,Error}}
            end;

        {error,Error} ->
            {stop, {shutdown,Error}}
    end;

handle_event(internal, socket_ready, {hello,_}=StateName, #data{ssh_params = Ssh0} = D) ->
    VsnMsg = ssh_transport:hello_version_msg(string_version(Ssh0)),
    send_bytes(VsnMsg, D),
    case inet:getopts(Socket=D#data.socket, [recbuf]) of
	{ok, [{recbuf,Size}]} ->
	    %% Set the socket to the hello text line handling mode:
	    inet:setopts(Socket, [{packet, line},
				  {active, once},
				  % Expecting the version string which might
				  % be max ?MAX_PROTO_VERSION bytes:
				  {recbuf, ?MAX_PROTO_VERSION},
				  {nodelay,true}]),
            Time = ?GET_OPT(hello_timeout, Ssh0#ssh.opts, infinity),
	    {keep_state, D#data{inet_initial_recbuf_size=Size}, [{state_timeout,Time,no_hello_received}] };

	Other ->
            ?call_disconnectfun_and_log_cond("Option return", 
                                             io_lib:format("Unexpected getopts return:~n  ~p",[Other]),
                                             StateName, D),
	    {stop, {shutdown,{unexpected_getopts_return, Other}}}
    end;

handle_event(internal, {info_line,_Line}, {hello,client}, D) ->
    %% The server may send info lines to the client before the version_exchange
    %% RFC4253/4.2
    inet:setopts(D#data.socket, [{active, once}]),
    keep_state_and_data;

handle_event(internal, {info_line,Line}, {hello,server}=StateName, D) ->
    %% But the client may NOT send them to the server. Openssh answers with cleartext,
    %% and so do we
    send_bytes("Protocol mismatch.", D),
    Msg = io_lib:format("Protocol mismatch in version exchange. Client sent info lines.~n~s",
                        [ssh_dbg:hex_dump(Line, 64)]),
    ?call_disconnectfun_and_log_cond("Protocol mismatch.", Msg, StateName, D),
    {stop, {shutdown,"Protocol mismatch in version exchange. Client sent info lines."}};

handle_event(internal, {version_exchange,Version}, {hello,Role}, D0) ->
    {NumVsn, StrVsn} = ssh_transport:handle_hello_version(Version),
    case handle_version(NumVsn, StrVsn, D0#data.ssh_params) of
	{ok, Ssh1} ->
	    %% Since the hello part is finished correctly, we set the
	    %% socket to the packet handling mode (including recbuf size):
	    inet:setopts(D0#data.socket, [{packet,0},
					 {mode,binary},
					 {active, once},
					 {recbuf, D0#data.inet_initial_recbuf_size}]),
	    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(Ssh1),
	    send_bytes(SshPacket, D0),
            D = D0#data{ssh_params = Ssh,
                        key_exchange_init_msg = KeyInitMsg},
	    {next_state, {kexinit,Role,init}, D, {change_callback_module, ssh_fsm_kexinit}};

	not_supported ->
            {Shutdown, D} =
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,
                                 io_lib:format("Offending version is ~p",[string:chomp(Version)]),
                                 {hello,Role},
                                 D0),
	    {stop, Shutdown, D}
    end;

%%% timeout after tcp:connect but then nothing arrives
handle_event(state_timeout, no_hello_received, {hello,_Role}=StateName, D0 = #data{ssh_params = Ssh0}) ->
    Time = ?GET_OPT(hello_timeout, Ssh0#ssh.opts),
    {Shutdown, D} =
        ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                         lists:concat(["No HELLO received within ",ssh_lib:format_time_ms(Time)]),
                         StateName, D0),
    {stop, Shutdown, D};


%%% ######## {service_request, client|server} ####

handle_event(internal, Msg = #ssh_msg_service_request{name=ServiceName}, StateName = {service_request,server}, D0) ->
    case ServiceName of
	"ssh-userauth" ->
	    Ssh0 = #ssh{session_id=SessionId} = D0#data.ssh_params,
	    {ok, {Reply, Ssh}} = ssh_auth:handle_userauth_request(Msg, SessionId, Ssh0),
            D = send_msg(Reply, D0#data{ssh_params = Ssh}),
	    {next_state, {userauth,server}, D, {change_callback_module,ssh_fsm_userauth_server}};

	_ ->
            {Shutdown, D} =
                ?send_disconnect(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                                 io_lib:format("Unknown service: ~p",[ServiceName]),
                                 StateName, D0),
            {stop, Shutdown, D}
    end;

handle_event(internal, #ssh_msg_service_accept{name = "ssh-userauth"}, {service_request,client},
	     #data{ssh_params = #ssh{service="ssh-userauth"} = Ssh0} = D0) ->
    {Msg, Ssh} = ssh_auth:init_userauth_request_msg(Ssh0),
    D = send_msg(Msg, D0#data{ssh_params = Ssh,
                              auth_user = Ssh#ssh.user
                             }),
    {next_state, {userauth,client}, D, {change_callback_module,ssh_fsm_userauth_client}};


%% Skip ext_info messages in connected state (for example from OpenSSH >= 7.7)
handle_event(internal, #ssh_msg_ext_info{}, {connected,_Role}, D) ->
    {keep_state, D};

handle_event(internal, {#ssh_msg_kexinit{},_}, {connected,Role}, D0) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(D0#data.ssh_params),
    D = D0#data{ssh_params = Ssh,
		key_exchange_init_msg = KeyInitMsg},
    send_bytes(SshPacket, D),
    {next_state, {kexinit,Role,renegotiate}, D, [postpone, {change_callback_module,ssh_fsm_kexinit}]};

handle_event(internal, #ssh_msg_disconnect{description=Desc} = Msg, StateName, D0) ->
    {disconnect, _, RepliesCon} =
	ssh_connection:handle_msg(Msg, D0#data.connection_state, ?role(StateName), D0#data.ssh_params),
    {Actions,D} = send_replies(RepliesCon, D0),
    disconnect_fun("Received disconnect: "++Desc, D),
    {stop_and_reply, {shutdown,Desc}, Actions, D};

handle_event(internal, #ssh_msg_ignore{}, {_StateName, _Role, init},
             #data{ssh_params = #ssh{kex_strict_negotiated = true,
                                     send_sequence = SendSeq,
                                     recv_sequence = RecvSeq}}) ->
    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                io_lib:format("strict KEX violation: unexpected SSH_MSG_IGNORE "
                              "send_sequence = ~p  recv_sequence = ~p",
                              [SendSeq, RecvSeq])
               );

handle_event(internal, #ssh_msg_ignore{}, _StateName, _) ->
    keep_state_and_data;

handle_event(internal, #ssh_msg_unimplemented{}, _StateName, _) ->
    keep_state_and_data;

%% Quick fix of failing test case (ssh_options_SUITE:ssh_msg_debug_fun_option_{client|server}/1)
handle_event(cast, #ssh_msg_debug{} = Msg, State, D) ->
    handle_event(internal, Msg, State, D);

handle_event(internal, #ssh_msg_debug{} = Msg, _StateName, D) ->
    debug_fun(Msg, D),
    keep_state_and_data;

handle_event(internal, {conn_msg,Msg}, StateName, #data{connection_state = Connection0,
                                                        event_queue = Qev0} = D0) ->
    Role = ?role(StateName),
    Rengotation = renegotiation(StateName),
    try ssh_connection:handle_msg(Msg, Connection0, Role, D0#data.ssh_params) of
	{disconnect, Reason0, RepliesConn} ->
            {Repls, D} = send_replies(RepliesConn, D0),
            case {Reason0,Role} of
                {{_, Reason}, client} when ((StateName =/= {connected,client})
                                            and (not Rengotation)) ->
                    handshake({not_connected,Reason}, D);
                _ ->
                    ok
            end,
            {stop_and_reply, {shutdown,normal}, Repls, D};

	{Replies, Connection} when is_list(Replies) ->
	    {Repls, D} = 
		case StateName of
		    {connected,_} ->
			send_replies(Replies, D0#data{connection_state=Connection});
		    _ ->
			{ConnReplies, NonConnReplies} = lists:splitwith(fun not_connected_filter/1, Replies),
			send_replies(NonConnReplies, D0#data{event_queue = Qev0 ++ ConnReplies})
		end,
            case {Msg, StateName} of
                {#ssh_msg_channel_close{}, {connected,_}} ->
                    {keep_state, D, [cond_set_idle_timer(D)|Repls]};
                {#ssh_msg_channel_success{}, _} ->
                    update_inet_buffers(D#data.socket),
                    {keep_state, D, Repls};
                _ ->
                    {keep_state, D, Repls}
            end

    catch
	Class:Error ->
            {Repls, D1} = send_replies(ssh_connection:handle_stop(Connection0), D0),
            {Shutdown, D} = ?send_disconnect(?SSH_DISCONNECT_BY_APPLICATION,
                                             io_lib:format("Internal error: ~p:~p",[Class,Error]),
                                             StateName, D1),
            {stop_and_reply, Shutdown, Repls, D}
    end;


handle_event(enter, OldState, {connected,_}=NewState, D) ->
    %% Entering the state where re-negotiation is possible
    init_renegotiate_timers(OldState, NewState, D);

handle_event(enter, OldState, {ext_info,_,renegotiate}=NewState, D) ->
    %% Could be hanging in exit_info state if nothing else arrives
    init_renegotiate_timers(OldState, NewState, D);

handle_event(enter, {connected,_}=OldState, NewState, D) ->
    %% Exiting the state where re-negotiation is possible
    pause_renegotiate_timers(OldState, NewState, D);

handle_event(cast, force_renegotiate, StateName, D) ->
    handle_event({timeout,renegotiate}, undefined, StateName, D);

handle_event({timeout,renegotiate}, _, StateName, D0) ->
    case StateName of
        {connected,Role} ->
            start_rekeying(Role, D0);
        {ext_info,Role,renegotiate} ->
            start_rekeying(Role, D0);
        _ ->
            %% Wrong state for starting a renegotiation, must be in re-negotiation
            keep_state_and_data
    end;

handle_event({timeout,check_data_size}, _, StateName, D0) ->
    %% Rekey due to sent data limit reached? (Can't be in {ext_info,...} if data is sent)
    case StateName of
        {connected,Role} ->
            check_data_rekeying(Role, D0);
        _ ->
            %% Wrong state for starting a renegotiation, must be in re-negotiation
            keep_state_and_data
    end;

handle_event({call,From}, get_alg, _, D) ->
    #ssh{algorithms=Algs} = D#data.ssh_params,
    {keep_state_and_data, [{reply,From,Algs}]};

handle_event(cast, _, StateName, _) when not ?CONNECTED(StateName) ->
    {keep_state_and_data, [postpone]};

handle_event(cast, {adjust_window,ChannelId,Bytes}, StateName, D) when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 recv_packet_size = PktSize} = Channel
	  when (WinSize-Bytes) >= 2*PktSize ->
	    %% The peer can send at least two more *full* packet, no hurry.
	    ssh_client_channel:cache_update(cache(D),
				     Channel#channel{recv_window_pending = Pending + Bytes}),
	    keep_state_and_data;

	#channel{recv_window_size = WinSize,
		 recv_window_pending = Pending,
		 remote_id = Id} = Channel ->
	    %% Now we have to update the window - we can't receive so many more pkts
	    ssh_client_channel:cache_update(cache(D),
				     Channel#channel{recv_window_size =
							 WinSize + Bytes + Pending,
						     recv_window_pending = 0}),
	    Msg = ssh_connection:channel_adjust_window_msg(Id, Bytes + Pending),
	    {keep_state, send_msg(Msg,D)};

	undefined ->
	    keep_state_and_data
    end;

handle_event(cast, {reply_request,Resp,ChannelId}, StateName, D) when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
        #channel{remote_id = RemoteId} when Resp== success ; Resp==failure ->
            Msg =
                case Resp of
                    success -> ssh_connection:channel_success_msg(RemoteId);
                    failure -> ssh_connection:channel_failure_msg(RemoteId)
                end,
            update_inet_buffers(D#data.socket),
            {keep_state, send_msg(Msg,D)};

        #channel{} ->
            Details = io_lib:format("Unhandled reply in state ~p:~n~p", [StateName,Resp]),
            {_Shutdown, D1} =
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR, Details, StateName, D),
            {keep_state, D1};

	undefined ->
	    keep_state_and_data
    end;

handle_event(cast, {request,ChannelPid, ChannelId, Type, Data}, StateName, D) when ?CONNECTED(StateName) ->
    {keep_state,  handle_request(ChannelPid, ChannelId, Type, Data, false, none, D)};

handle_event(cast, {request,ChannelId,Type,Data}, StateName, D) when ?CONNECTED(StateName) ->
    {keep_state,  handle_request(ChannelId, Type, Data, false, none, D)};

handle_event(cast, {unknown,Data}, StateName, D) when ?CONNECTED(StateName) ->
    Msg = #ssh_msg_unimplemented{sequence = Data},
    {keep_state, send_msg(Msg,D)};

handle_event(cast, {global_request, Type, Data}, StateName, D) when ?CONNECTED(StateName) ->
    {keep_state, send_msg(ssh_connection:request_global_msg(Type,false,Data), D)};


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
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{} = Channel ->
	    Info = fold_keys(Options, fun chann_info/2, Channel),
	    {keep_state_and_data, [{reply,From,Info}]};
	undefined ->
	    {keep_state_and_data, [{reply,From,[]}]}
    end;


handle_event({call,From}, {info, all}, _, D) ->
    Result = ssh_client_channel:cache_foldl(fun(Channel, Acc) ->
					     [Channel | Acc]
				     end,
				     [], cache(D)),
    {keep_state_and_data, [{reply, From, {ok,Result}}]};

handle_event({call,From}, {info, ChannelPid}, _, D) ->
    Result = ssh_client_channel:cache_foldl(
	       fun(Channel, Acc) when Channel#channel.user == ChannelPid ->
		       [Channel | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], cache(D)),
    {keep_state_and_data, [{reply, From, {ok,Result}}]};

handle_event({call,From}, {set_sock_opts,SocketOptions}, _StateName, D) ->
    Result = try inet:setopts(D#data.socket, SocketOptions)
             catch
                 _:_ -> {error, badarg}
             end,
    {keep_state_and_data, [{reply,From,Result}]};

handle_event({call,From}, {get_sock_opts,SocketGetOptions}, _StateName, D) ->
    Result = try inet:getopts(D#data.socket, SocketGetOptions)
             catch
                 _:_ -> {error, badarg}
             end,
    {keep_state_and_data, [{reply,From,Result}]};

handle_event({call,From}, stop, _StateName, D0) ->
    {Repls,D} = send_replies(ssh_connection:handle_stop(D0#data.connection_state), D0),
    {stop_and_reply, normal, [{reply,From,ok}|Repls], D};

handle_event({call,_}, _, StateName, _) when not ?CONNECTED(StateName) ->
    {keep_state_and_data, [postpone]};

handle_event({call,From}, {request, ChannelPid, ChannelId, Type, Data, Timeout}, StateName, D0)
  when ?CONNECTED(StateName) ->
    case handle_request(ChannelPid, ChannelId, Type, Data, true, From, D0) of
        {error,Error} ->
            {keep_state, D0, {reply,From,{error,Error}}};
        D ->
            %% Note reply to channel will happen later when reply is received from peer on the socket
            start_channel_request_timer(ChannelId, From, Timeout),
            {keep_state, D, cond_set_idle_timer(D)}
    end;

handle_event({call,From}, {request, ChannelId, Type, Data, Timeout}, StateName, D0)
  when ?CONNECTED(StateName) ->
    case handle_request(ChannelId, Type, Data, true, From, D0) of
        {error,Error} ->
            {keep_state, D0, {reply,From,{error,Error}}};
        D ->
            %% Note reply to channel will happen later when reply is received from peer on the socket
            start_channel_request_timer(ChannelId, From, Timeout),
            {keep_state, D, cond_set_idle_timer(D)}
    end;

handle_event({call,From}, {global_request, "tcpip-forward" = Type,
                           {ListenHost,ListenPort,ConnectToHost,ConnectToPort},
                           Timeout}, StateName, D0) when ?CONNECTED(StateName) ->
    Id = make_ref(),
    Data =  <<?STRING(ListenHost), ?Euint32(ListenPort)>>,
    Fun = fun({success, <<Port:32/unsigned-integer>>}, C) ->
                  Key = {tcpip_forward,ListenHost,Port},
                  Value = {ConnectToHost,ConnectToPort},
                  C#connection{options = ?PUT_INTERNAL_OPT({Key,Value}, C#connection.options)};
             ({success, <<>>}, C) ->
                  Key = {tcpip_forward,ListenHost,ListenPort},
                  Value = {ConnectToHost,ConnectToPort},
                  C#connection{options = ?PUT_INTERNAL_OPT({Key,Value}, C#connection.options)};
             (_, C) ->
                  C
          end,
    D = send_msg(ssh_connection:request_global_msg(Type, true, Data),
                 add_request(Fun, Id, From, D0)),
    start_channel_request_timer(Id, From, Timeout),
    {keep_state, D, cond_set_idle_timer(D)};

handle_event({call,From}, {global_request, Type, Data, Timeout}, StateName, D0) when ?CONNECTED(StateName) ->
    Id = make_ref(),
    D = send_msg(ssh_connection:request_global_msg(Type, true, Data),
                 add_request(true, Id, From, D0)),
    start_channel_request_timer(Id, From, Timeout),
    {keep_state, D, cond_set_idle_timer(D)};

handle_event({call,From}, {data, ChannelId, Type, Data, Timeout}, StateName, D0)
  when ?CONNECTED(StateName) ->
    {Repls,D} = send_replies(ssh_connection:channel_data(ChannelId, Type, Data, D0#data.connection_state, From),
                             D0),
    start_channel_request_timer(ChannelId, From, Timeout), % FIXME: No message exchange so why?
    {keep_state, D, Repls};

handle_event({call,From}, {eof, ChannelId}, StateName, D0)
  when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D0), ChannelId) of
	#channel{remote_id = Id, sent_close = false} ->
	    D = send_msg(ssh_connection:channel_eof_msg(Id), D0),
	    {keep_state, D, [{reply,From,ok}]};
	_ ->
	    {keep_state, D0, [{reply,From,{error,closed}}]}
    end;

handle_event({call,From}, get_misc, StateName,
             #data{connection_state = #connection{options = Opts}} = D) when ?CONNECTED(StateName) ->
    SubSysSup = ?GET_INTERNAL_OPT(subsystem_sup, Opts),
    Reply = {ok, {SubSysSup, ?role(StateName), Opts}},
    {keep_state, D, [{reply,From,Reply}]};

handle_event({call,From},
	     {open, ChannelPid, Type, InitialWindowSize, MaxPacketSize, Data, Timeout},
	     StateName,
	     D0 = #data{connection_state = C}) when ?CONNECTED(StateName) ->
    erlang:monitor(process, ChannelPid),
    {ChannelId, D1} = new_channel_id(D0),
    WinSz = case InitialWindowSize of
                undefined -> C#connection.suggest_window_size;
                _ -> InitialWindowSize
            end,
    PktSz = case MaxPacketSize of
                undefined -> C#connection.suggest_packet_size;
                _ -> MaxPacketSize
            end,
    D2 = send_msg(ssh_connection:channel_open_msg(Type, ChannelId, WinSz, PktSz, Data),
		  D1),
    ssh_client_channel:cache_update(cache(D2),
			     #channel{type = Type,
				      sys = "none",
				      user = ChannelPid,
				      local_id = ChannelId,
				      recv_window_size = WinSz,
				      recv_packet_size = PktSz,
				      send_buf = queue:new()
				     }),
    D = add_request(true, ChannelId, From, D2),
    start_channel_request_timer(ChannelId, From, Timeout),
    {keep_state, D, cond_set_idle_timer(D)};

handle_event({call,From}, {send_window, ChannelId}, StateName, D)
  when ?CONNECTED(StateName) ->
    Reply = case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
		#channel{send_window_size = WinSize,
			 send_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {recv_window, ChannelId}, StateName, D)
  when ?CONNECTED(StateName) ->
    Reply = case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
		#channel{recv_window_size = WinSize,
			 recv_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined ->
		    {error, einval}
	    end,
    {keep_state_and_data, [{reply,From,Reply}]};

handle_event({call,From}, {close, ChannelId}, StateName, D0)
  when ?CONNECTED(StateName) ->
    case ssh_client_channel:cache_lookup(cache(D0), ChannelId) of
	#channel{remote_id = Id} = Channel ->
	    D1 = send_msg(ssh_connection:channel_close_msg(Id), D0),
	    ssh_client_channel:cache_update(cache(D1), Channel#channel{sent_close = true}),
	    {keep_state, D1, [cond_set_idle_timer(D1), {reply,From,ok}]};
	undefined ->
	    {keep_state_and_data, [{reply,From,ok}]}
    end;

handle_event(cast, {store,Key,Value}, _StateName, #data{connection_state=C0} = D) ->
    C = C0#connection{options = ?PUT_INTERNAL_OPT({Key,Value}, C0#connection.options)},
    {keep_state, D#data{connection_state = C}};

handle_event({call,From}, {retrieve,Key}, _StateName, #data{connection_state=C}) ->
    case retrieve(C, Key) of
        {ok,Value} ->
            {keep_state_and_data, [{reply,From,{ok,Value}}]};
        _ ->
            {keep_state_and_data, [{reply,From,undefined}]}
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


handle_event(info, {Proto, Sock, NewData}, StateName,
             D0 = #data{socket = Sock,
                        transport_protocol = Proto,
                        ssh_params = SshParams}) ->
    try ssh_transport:handle_packet_part(
	  D0#data.decrypted_data_buffer,
	  <<(D0#data.encrypted_data_buffer)/binary, NewData/binary>>,
          D0#data.aead_data,
          D0#data.undecrypted_packet_length,
	  D0#data.ssh_params)
    of
	{packet_decrypted, DecryptedBytes, EncryptedDataRest, Ssh1} ->
	    D1 = D0#data{ssh_params =
			    Ssh1#ssh{recv_sequence = ssh_transport:next_seqnum(Ssh1#ssh.recv_sequence)},
			decrypted_data_buffer = <<>>,
                        undecrypted_packet_length = undefined,
                        aead_data = <<>>,
			encrypted_data_buffer = EncryptedDataRest},
	    try
		ssh_message:decode(set_kex_overload_prefix(DecryptedBytes,D1))
	    of
		#ssh_msg_kexinit{} = Msg ->
		    {keep_state, D1, [{next_event, internal, prepare_next_packet},
				     {next_event, internal, {Msg,DecryptedBytes}}
				    ]};

                #ssh_msg_global_request{}            = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_request_success{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_request_failure{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_open{}              = Msg -> {keep_state, D1,
                                                               [{{timeout, max_initial_idle_time}, cancel} |
                                                                ?CONNECTION_MSG(Msg)
                                                               ]};
                #ssh_msg_channel_open_confirmation{} = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_open_failure{}      = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_window_adjust{}     = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_data{}              = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_extended_data{}     = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_eof{}               = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_close{}             = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_request{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_failure{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};
                #ssh_msg_channel_success{}           = Msg -> {keep_state, D1, ?CONNECTION_MSG(Msg)};

		Msg ->
		    {keep_state, D1, [{next_event, internal, prepare_next_packet},
                                      {next_event, internal, Msg}
				    ]}
	    catch
		C:E:ST  ->
                    MaxLogItemLen = ?GET_OPT(max_log_item_len,SshParams#ssh.opts),
                    {Shutdown, D} =
                        ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                         io_lib:format("Bad packet: Decrypted, but can't decode~n~p:~p~n~P",
                                                       [C,E,ST,MaxLogItemLen]),
                                         StateName, D1),
                    {stop, Shutdown, D}
	    end;

	{get_more, DecryptedBytes, EncryptedDataRest, AeadData, RemainingSshPacketLen, Ssh1} ->
	    %% Here we know that there are not enough bytes in
	    %% EncryptedDataRest to use. We must wait for more.
	    inet:setopts(Sock, [{active, once}]),
	    {keep_state, D0#data{encrypted_data_buffer = EncryptedDataRest,
				 decrypted_data_buffer = DecryptedBytes,
                                 undecrypted_packet_length = RemainingSshPacketLen,
                                 aead_data = AeadData,
				 ssh_params = Ssh1}};

	{bad_mac, Ssh1} ->
            {Shutdown, D} =
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                 "Bad packet: bad mac",
                                 StateName, D0#data{ssh_params=Ssh1}),
            {stop, Shutdown, D};

	{error, {exceeds_max_size,PacketLen}} ->
            {Shutdown, D} =
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                 io_lib:format("Bad packet: Size (~p bytes) exceeds max size",
                                               [PacketLen]),
                                 StateName, D0),
            {stop, Shutdown, D}
    catch
	C:E:ST ->
            MaxLogItemLen = ?GET_OPT(max_log_item_len,SshParams#ssh.opts),
            {Shutdown, D} =
                ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR,
                                 io_lib:format("Bad packet: Couldn't decrypt~n~p:~p~n~P",
                                               [C,E,ST,MaxLogItemLen]),
                                 StateName, D0),
            {stop, Shutdown, D}
    end;


%%%====
handle_event(internal, prepare_next_packet, _StateName, D) ->
    Enough =  erlang:max(8, D#data.ssh_params#ssh.decrypt_block_size),
    case byte_size(D#data.encrypted_data_buffer) of
	Sz when Sz >= Enough ->
	    self() ! {D#data.transport_protocol, D#data.socket, <<>>};
	_ ->
	    ok
    end,
    inet:setopts(D#data.socket, [{active, once}]),
    keep_state_and_data;

handle_event(info, {CloseTag,Socket}, _StateName,
	     D0 = #data{socket = Socket,
                        transport_close_tag = CloseTag,
                        connection_state = C0}) ->
    {Repls, D} = send_replies(ssh_connection:handle_stop(C0), D0),
    disconnect_fun("Received a transport close", D),
    {stop_and_reply, {shutdown,"Connection closed"}, Repls, D};

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
handle_event(info, {'DOWN', _Ref, process, ChannelPid, _Reason}, _, D) ->
    Cache = cache(D),
    ssh_client_channel:cache_foldl(
      fun(#channel{user=U,
                   local_id=Id}, Acc) when U == ChannelPid ->
              ssh_client_channel:cache_delete(Cache, Id),
              Acc;
         (_,Acc) ->
              Acc
      end, [], Cache),
    {keep_state, D, cond_set_idle_timer(D)};

handle_event({timeout,idle_time}, _Data,  _StateName, D) ->
    case ssh_client_channel:cache_info(num_entries, cache(D)) of
        0 ->
            {stop, {shutdown, "Timeout"}};
        _ ->
            keep_state_and_data
    end;

handle_event({timeout,max_initial_idle_time}, _Data,  _StateName, _D) ->
    {stop, {shutdown, "Timeout"}};

%%% So that terminate will be run when supervisor is shutdown
handle_event(info, {'EXIT', _Sup, Reason}, StateName, _D) ->
    Role = ?role(StateName),
    if
	Role == client ->
	    %% OTP-8111 tells this function clause fixes a problem in
	    %% clients, but there were no check for that role.
	    {stop, {shutdown, Reason}};

	Reason == normal ->
	    %% An exit normal should not cause a server to crash. This has happened...
	    keep_state_and_data;

	true ->
	    {stop, {shutdown, Reason}}
    end;

handle_event(info, check_cache, _, D) ->
    {keep_state, D, cond_set_idle_timer(D)};

handle_event(info, {fwd_connect_received, Sock, ChId, ChanCB}, StateName, #data{connection_state = Connection}) ->
    #connection{options = Options,
                channel_cache = Cache,
                sub_system_supervisor = SubSysSup} = Connection,
    Channel = ssh_client_channel:cache_lookup(Cache, ChId),
    {ok,Pid} = ssh_subsystem_sup:start_channel(?role(StateName), SubSysSup, self(), ChanCB, ChId, [Sock], undefined, Options),
    ssh_client_channel:cache_update(Cache, Channel#channel{user=Pid}),
    gen_tcp:controlling_process(Sock, Pid),
    inet:setopts(Sock, [{active,once}]),
    keep_state_and_data;

handle_event({call,From},
             {handle_direct_tcpip, ListenHost, ListenPort, ConnectToHost, ConnectToPort, _Timeout},
             _StateName,
             #data{connection_state = #connection{sub_system_supervisor=SubSysSup}}) ->
    case ssh_tcpip_forward_acceptor:supervised_start(ssh_subsystem_sup:tcpip_fwd_supervisor(SubSysSup),
                                                     {ListenHost, ListenPort},
                                                     {ConnectToHost, ConnectToPort},
                                                     "direct-tcpip", ssh_tcpip_forward_client,
                                                     self()) of
        {ok,LPort} ->
            {keep_state_and_data, [{reply,From,{ok,LPort}}]};
        {error,Error} ->
            {keep_state_and_data, [{reply,From,{error,Error}}]}
    end;

handle_event(info, UnexpectedMessage, StateName, D = #data{ssh_params = Ssh}) ->
    case unexpected_fun(UnexpectedMessage, D) of
	report ->
	    Msg = lists:flatten(
		    io_lib:format(
                      "*** SSH: "
		      "Unexpected message '~p' received in state '~p'\n"
		      "Role: ~p\n"
		      "Peer: ~p\n"
		      "Local Address: ~p\n",
                      [UnexpectedMessage,
                       StateName,
                       Ssh#ssh.role,
                       Ssh#ssh.peer,
                       ?GET_INTERNAL_OPT(address, Ssh#ssh.opts, undefined)])),
	    error_logger:info_report(Msg),
	    keep_state_and_data;

	skip ->
	    keep_state_and_data;

	Other ->
	    Msg = lists:flatten(
		    io_lib:format("*** SSH: "
                                  "Call to fun in 'unexpectedfun' failed:~n"
				  "Return: ~p\n"
				  "Message: ~p\n"
				  "Role: ~p\n"
				  "Peer: ~p\n"
				  "Local Address: ~p\n",
                                  [Other,
                                   UnexpectedMessage,
                                   Ssh#ssh.role,
                                   Ssh#ssh.peer,
                                   ?GET_INTERNAL_OPT(address, Ssh#ssh.opts, undefined)]
				 )),
	    error_logger:error_report(Msg),
	    keep_state_and_data
    end;

handle_event(internal, {send_disconnect,Code,DetailedText,Module,Line}, StateName, D0) ->
    {Shutdown, D} =
        send_disconnect(Code, DetailedText, Module, Line, StateName, D0),
    {stop, Shutdown, D};


handle_event(enter, _OldState, State, D) ->
    %% Just skip
    {next_state, State, D};

handle_event(_Type, _Msg, {ext_info,Role,_ReNegFlag}, D) ->
    %% If something else arrives, goto next state and handle the event in that one
    {next_state, {connected,Role}, D, [postpone]};

handle_event(Type, Ev, StateName, D0) ->
    Details =
	case catch atom_to_list(element(1,Ev)) of
	    "ssh_msg_" ++_ when Type==internal ->
                lists:flatten(io_lib:format("Message ~p in wrong state (~p)", [element(1,Ev), StateName]));
	    _ ->
		io_lib:format("Unhandled event in state ~p and type ~p:~n~p", [StateName,Type,Ev])
	end,
    {Shutdown, D} =
        ?send_disconnect(?SSH_DISCONNECT_PROTOCOL_ERROR, Details, StateName, D0),
    {stop, Shutdown, D}.


%%--------------------------------------------------------------------
-spec terminate(any(),
		state_name(),
		#data{}
	       ) -> term().

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
terminate(_, {wait_for_socket, _}, _) ->
    %% No need to to anything - maybe we have not yet gotten
    %% control over the socket
    ok;

terminate(normal, _StateName, D) ->
    close_transport(D);

terminate({shutdown,_R}, _StateName, D) ->
    %% Internal termination, usually already reported via ?send_disconnect resulting in a log entry
    close_transport(D);

terminate(shutdown, _StateName, D0) ->
    %% Terminated by supervisor
    %% Use send_msg directly instead of ?send_disconnect to avoid filling the log
    D = send_msg(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
                                     description = "Terminated (shutdown) by supervisor"},
                 D0),
    close_transport(D);

terminate(Reason, StateName, D0) ->
    %% Others, e.g  undef, {badmatch,_}, ...
    log(error, D0, Reason),
    {_ShutdownReason, D} = ?send_disconnect(?SSH_DISCONNECT_BY_APPLICATION,
                                            "Internal error",
                                            io_lib:format("Reason: ~p",[Reason]),
                                            StateName, D0),
    close_transport(D).

%%--------------------------------------------------------------------

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

format_status(A, B) ->
    try format_status0(A, B)
    catch
        _:_ -> "????"
    end.

format_status0(normal, [_PDict, _StateName, D]) ->
    [{data, [{"State", D}]}];
format_status0(terminate, [_, _StateName, D]) ->
    [{data, [{"State", clean(D)}]}].


clean(#data{}=R) ->
    fmt_stat_rec(record_info(fields,data), R,
                 [decrypted_data_buffer,
                  encrypted_data_buffer,
                  key_exchange_init_msg,
                  user_passwords,
                  opts,
                  inet_initial_recbuf_size]);
clean(#ssh{}=R) ->
    fmt_stat_rec(record_info(fields, ssh), R,
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
                  available_host_keys]);
clean(#connection{}=R) ->
    fmt_stat_rec(record_info(fields, connection), R,
                 []);
clean(L) when is_list(L) ->
    lists:map(fun clean/1, L);
clean(T) when is_tuple(T) ->
    list_to_tuple( clean(tuple_to_list(T)));
clean(X) ->
    ssh_options:no_sensitive(filter, X).

fmt_stat_rec(FieldNames, Rec, Exclude) ->
    Values = tl(tuple_to_list(Rec)),
    list_to_tuple(
      [element(1,Rec) |
       lists:map(fun({K,V}) ->
                         case lists:member(K, Exclude) of
                             true -> '****';
                             false -> clean(V)
                         end
                 end, lists:zip(FieldNames, Values))
      ]).

%%--------------------------------------------------------------------
-spec code_change(term() | {down,term()},
		  state_name(),
		  #data{},
		  term()
		 ) -> {ok, state_name(), #data{}}.

%% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
close_transport(#data{transport_cb = Transport,
                      socket = Socket}) ->
    catch Transport:close(Socket),
    ok.

%%--------------------------------------------------------------------
available_hkey_algorithms(client, Options) ->
    case available_hkey_algos(Options) of
        [] ->
            error({shutdown, "No public key algs"});
        Algs ->
	    [atom_to_list(A) || A<-Algs]
    end;

available_hkey_algorithms(server, Options) ->
    case [A || A <- available_hkey_algos(Options),
               is_usable_host_key(A, Options)] of
        [] ->
            error({shutdown, "No host key available"});
	Algs ->
	    [atom_to_list(A) || A<-Algs]
    end.


available_hkey_algos(Options) ->
    SupAlgos = ssh_transport:supported_algorithms(public_key),
    HKeys = proplists:get_value(public_key,
                                ?GET_OPT(preferred_algorithms,Options)
                               ),
    NonSupported =  HKeys -- SupAlgos,
    AvailableAndSupported = HKeys -- NonSupported,
    AvailableAndSupported.


send_msg(Msg, State=#data{ssh_params=Ssh0}) when is_tuple(Msg) ->
    {Bytes, Ssh} = ssh_transport:ssh_packet(Msg, Ssh0),
    send_bytes(Bytes, State),
    State#data{ssh_params=Ssh}.

send_bytes("", _D) ->
    ok;
send_bytes(Bytes, #data{socket = Socket, transport_cb = Transport}) ->
    _ = Transport:send(Socket, Bytes),
    ok.

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
	    {error, closed};
	exit:{shutdown, _R} ->
	    {error, closed}
    end.


set_kex_overload_prefix(Msg = <<?BYTE(Op),_/binary>>, #data{ssh_params=SshParams})
  when Op == 30;
       Op == 31
       ->
    case catch atom_to_list(kex(SshParams)) of
	"ecdh-sha2-" ++ _ ->
	    <<"ecdh",Msg/binary>>;
        "curve25519-" ++ _ ->
	    <<"ecdh",Msg/binary>>;
        "curve448-" ++ _ ->
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
handle_ssh_msg_ext_info(#ssh_msg_ext_info{}, D=#data{ssh_params = #ssh{recv_ext_info=false}} ) ->
    % The peer sent this although we didn't allow it!
    D;

handle_ssh_msg_ext_info(#ssh_msg_ext_info{data=Data}, D0) ->
    lists:foldl(fun ext_info/2, D0, Data).


ext_info({"server-sig-algs",SigAlgsStr},
         D0 = #data{ssh_params=#ssh{role=client,
                                    userauth_pubkeys=ClientSigAlgs}=Ssh0}) ->
    %% ClientSigAlgs are the pub_key algorithms that:
    %%  1) is usable, that is, the user has such a public key and
    %%  2) is either the default list or set by the caller
    %%     with the client option 'pref_public_key_algs'
    %%
    %% The list is already checked for duplicates.

    SigAlgs = [A || Astr <- string:tokens(SigAlgsStr, ","),
                    A <- try [list_to_existing_atom(Astr)]
                              %% list_to_existing_atom will fail for unknown algorithms
                         catch _:_ -> []
                         end],

    CommonAlgs = [A || A <- SigAlgs,
                       lists:member(A, ClientSigAlgs)],

    %% Re-arrange the client supported public-key algorithms so that the server
    %% preferred ones are tried first.
    %% Trying algorithms not mentioned by the server is ok, since the server can't know
    %% if the client supports 'server-sig-algs' or not.

    D0#data{
      ssh_params =
          Ssh0#ssh{
            userauth_pubkeys =
                CommonAlgs ++ (ClientSigAlgs -- CommonAlgs)
           }};

ext_info(_, D0) ->
    %% Not implemented
    D0.

%%%----------------------------------------------------------------
is_usable_user_pubkey(Alg, Ssh) ->
    try ssh_auth:get_public_key(Alg, Ssh) of
        {ok,_} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

%%%----------------------------------------------------------------
is_usable_host_key(Alg, Opts) ->
    try ssh_transport:get_host_key(Alg, Opts)
    of
        _PrivHostKey -> true
    catch
        _:_ -> false
    end.

%%%----------------------------------------------------------------
handle_request(ChannelPid, ChannelId, Type, Data, WantReply, From, D) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = Id,
                 sent_close = false} = Channel ->
	    update_sys(cache(D), Channel, Type, ChannelPid),
	    send_msg(ssh_connection:channel_request_msg(Id, Type, WantReply, Data),
		     add_request(WantReply, ChannelId, From, D));

        _ when WantReply==true ->
            {error,closed};

        _ ->
            D
    end.

handle_request(ChannelId, Type, Data, WantReply, From, D) ->
    case ssh_client_channel:cache_lookup(cache(D), ChannelId) of
	#channel{remote_id = Id,
                 sent_close = false} ->
	    send_msg(ssh_connection:channel_request_msg(Id, Type, WantReply, Data),
		     add_request(WantReply, ChannelId, From, D));

	_ when WantReply==true ->
            {error,closed};

        _ ->
            D
    end.

%%%----------------------------------------------------------------
update_sys(Cache, Channel, Type, ChannelPid) ->
    ssh_client_channel:cache_update(Cache,
			     Channel#channel{sys = Type, user = ChannelPid}).

add_request(false, _ChannelId, _From, State) ->
    State;
add_request(true, ChannelId, From, #data{connection_state =
					     #connection{requests = Requests0} =
					     Connection} = State) ->
    Requests = [{ChannelId, From} | Requests0],
    State#data{connection_state = Connection#connection{requests = Requests}};
add_request(Fun, ChannelId, From, #data{connection_state =
                                            #connection{requests = Requests0} =
                                            Connection} = State) when is_function(Fun) ->
    Requests = [{ChannelId, From, Fun} | Requests0],
    State#data{connection_state = Connection#connection{requests = Requests}}.

new_channel_id(#data{connection_state = #connection{channel_id_seed = Id} =
			 Connection}
	       = State) ->
    {Id, State#data{connection_state =
			Connection#connection{channel_id_seed = Id + 1}}}.


%%%----------------------------------------------------------------
start_rekeying(Role, D0) ->
    {KeyInitMsg, SshPacket, Ssh} = ssh_transport:key_exchange_init_msg(D0#data.ssh_params),
    send_bytes(SshPacket, D0),
    D = D0#data{ssh_params = Ssh,
                key_exchange_init_msg = KeyInitMsg},
    {next_state, {kexinit,Role,renegotiate}, D, {change_callback_module,ssh_fsm_kexinit}}.


init_renegotiate_timers(_OldState, NewState, D) ->
    {RekeyTimeout,_MaxSent} = ?GET_OPT(rekey_limit, (D#data.ssh_params)#ssh.opts),
    {next_state, NewState, D, [{{timeout,renegotiate},     RekeyTimeout,       none},
                               {{timeout,check_data_size}, ?REKEY_DATA_TIMOUT, none} ]}.


pause_renegotiate_timers(_OldState, NewState, D) ->
    {next_state, NewState, D, [{{timeout,renegotiate},     infinity, none},
                               {{timeout,check_data_size}, infinity, none} ]}.

check_data_rekeying(Role, D) ->
    case inet:getstat(D#data.socket, [send_oct]) of
        {ok, [{send_oct,SocketSentTotal}]} ->
            SentSinceRekey = SocketSentTotal - D#data.last_size_rekey,
            {_RekeyTimeout,MaxSent} = ?GET_OPT(rekey_limit, (D#data.ssh_params)#ssh.opts),
            case check_data_rekeying_dbg(SentSinceRekey, MaxSent) of
                true ->
                    start_rekeying(Role, D#data{last_size_rekey = SocketSentTotal});
                _ ->
                    %% Not enough data sent for a re-negotiation. Restart timer.
                    {keep_state, D, {{timeout,check_data_size}, ?REKEY_DATA_TIMOUT, none}}
            end;
        {error,_} ->
            %% Socket closed, but before this module has handled that. Maybe
            %% it is in the message queue.
            %% Just go on like if there was not enough data transmitted to start re-keying:
            {keep_state, D, {{timeout,check_data_size}, ?REKEY_DATA_TIMOUT, none}}
    end.

check_data_rekeying_dbg(SentSinceRekey, MaxSent) ->
    %% This function is for the ssh_dbg to trace on. See dbg_trace/3 at the end.
    SentSinceRekey >= MaxSent.

%%%----------------------------------------------------------------
%%% This server/client has decided to disconnect via the state machine:
%%% The unused arguments are for debugging.

send_disconnect(Code, DetailedText, Module, Line, StateName, D) ->
    send_disconnect(Code, default_text(Code), DetailedText, Module, Line, StateName, D).

send_disconnect(Code, Reason, DetailedText, Module, Line, StateName, D0) ->
    Msg = #ssh_msg_disconnect{code = Code,
                              description = Reason},
    D = send_msg(Msg, D0),
    LogMsg = io_lib:format("Disconnects with code = ~p [RFC4253 11.1]: ~s",[Code,Reason]),
    call_disconnectfun_and_log_cond(LogMsg, DetailedText, Module, Line, StateName, D),
    {{shutdown,Reason}, D}.

call_disconnectfun_and_log_cond(LogMsg, DetailedText, Module, Line, StateName, D) ->
    case disconnect_fun(LogMsg, D) of
        void ->
            log(info, D,
                "~s~n"
                "State = ~p~n"
                "Module = ~p, Line = ~p.~n"
                "Details:~n  ~s~n",
                [LogMsg, StateName, Module, Line, DetailedText]);
        _ ->
            ok
    end.


default_text(?SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT) -> "Host not allowed to connect";
default_text(?SSH_DISCONNECT_PROTOCOL_ERROR) -> "Protocol error";
default_text(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED) -> "Key exchange failed";
default_text(?SSH_DISCONNECT_RESERVED) -> "Reserved";
default_text(?SSH_DISCONNECT_MAC_ERROR) -> "Mac error";
default_text(?SSH_DISCONNECT_COMPRESSION_ERROR) -> "Compression error";
default_text(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE) -> "Service not available";
default_text(?SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED) -> "Protocol version not supported";
default_text(?SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE) -> "Host key not verifiable";
default_text(?SSH_DISCONNECT_CONNECTION_LOST) -> "Connection lost";
default_text(?SSH_DISCONNECT_BY_APPLICATION) -> "By application";
default_text(?SSH_DISCONNECT_TOO_MANY_CONNECTIONS) -> "Too many connections";
default_text(?SSH_DISCONNECT_AUTH_CANCELLED_BY_USER) -> "Auth cancelled by user";
default_text(?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE) -> "Unable to connect using the available authentication methods";
default_text(?SSH_DISCONNECT_ILLEGAL_USER_NAME) -> "Illegal user name".

%%%----------------------------------------------------------------
counterpart_versions(NumVsn, StrVsn, #ssh{role = server} = Ssh) ->
    Ssh#ssh{c_vsn = NumVsn , c_version = StrVsn};
counterpart_versions(NumVsn, StrVsn, #ssh{role = client} = Ssh) ->
    Ssh#ssh{s_vsn = NumVsn , s_version = StrVsn}.

%%%----------------------------------------------------------------
conn_info_keys() ->
    [client_version,
     server_version,
     peer,
     user,
     sockname,
     options,
     algorithms,
     channels
    ].

conn_info(client_version, #data{ssh_params=S}) -> {S#ssh.c_vsn, S#ssh.c_version};
conn_info(server_version, #data{ssh_params=S}) -> {S#ssh.s_vsn, S#ssh.s_version};
conn_info(peer,           #data{ssh_params=S}) -> S#ssh.peer;
conn_info(user,                             D) -> D#data.auth_user;
conn_info(sockname,       #data{ssh_params=S}) -> S#ssh.local;
conn_info(options,        #data{ssh_params=#ssh{opts=Opts}})    -> lists:sort(
                                                                     maps:to_list(
                                                                       ssh_options:keep_set_options(
                                                                         client,
                                                                         ssh_options:keep_user_options(client,Opts))));
conn_info(algorithms,     #data{ssh_params=#ssh{algorithms=A}}) -> conn_info_alg(A);
conn_info(channels, D) -> try conn_info_chans(ets:tab2list(cache(D)))
                          catch _:_ -> undefined
                          end;
%% dbg options ( = not documented):
conn_info(socket, D) ->   D#data.socket;
conn_info(chan_ids, D) ->
    ssh_client_channel:cache_foldl(fun(#channel{local_id=Id}, Acc) ->
				    [Id | Acc]
			    end, [], cache(D)).

conn_info_chans(Chs) ->
    Fs = record_info(fields, channel),
    [lists:zip(Fs, tl(tuple_to_list(Ch))) || Ch=#channel{} <- Chs].

conn_info_alg(AlgTup) ->
    [alg|Vs] = tuple_to_list(AlgTup),
    Fs = record_info(fields, alg),
    [{K,V} || {K,V} <- lists:zip(Fs,Vs),
              lists:member(K,[kex,
                              hkey,
                              encrypt,
                              decrypt,
                              send_mac,
                              recv_mac,
                              compress,
                              decompress,
                              send_ext_info,
                              recv_ext_info])].

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
log(Tag, D, Format, Args) ->
    log(Tag, D, io_lib:format(Format,Args)).

log(Tag, D, Reason) ->
    case atom_to_list(Tag) of                   % Dialyzer-technical reasons...
        "error"   -> do_log(error_msg,   Reason, D);
        "warning" -> do_log(warning_msg, Reason, D);
        "info"    -> do_log(info_msg,    Reason, D)
    end.


do_log(F, Reason0, #data{ssh_params=S}) ->
    Reason1 = string:chomp(assure_string(Reason0)),
    Reason = limit_size(Reason1, ?GET_OPT(max_log_item_len,S#ssh.opts)),
    case S of
        #ssh{role = Role} when Role==server ;
                               Role==client ->
            {PeerRole,PeerVersion} =
                case Role of
                    server -> {"Peer client", S#ssh.c_version};
                    client -> {"Peer server", S#ssh.s_version}
                end,
            error_logger:F("Erlang SSH ~p version: ~s ~s.~n"
                           "Address: ~s~n"
                           "~s version: ~p~n"
                           "Peer address: ~s~n"
                           "~s~n",
                           [Role, ssh_log_version(), crypto_log_info(),
                            ssh_lib:format_address_port(S#ssh.local),
                            PeerRole, PeerVersion,
                            ssh_lib:format_address_port(element(2,S#ssh.peer)),
                            Reason]);
        _ ->
            error_logger:F("Erlang SSH ~s ~s.~n"
                           "~s~n",
                           [ssh_log_version(), crypto_log_info(), 
                            Reason])
    end.

assure_string(S) ->
    try io_lib:format("~s",[S])
    of Formatted -> Formatted
    catch
        _:_ -> io_lib:format("~p",[S])
    end.

limit_size(S, MaxLen) when is_integer(MaxLen) ->
    limit_size(S, lists:flatlength(S), MaxLen);
limit_size(S, _) ->
    S.

limit_size(S, Len, MaxLen) when Len =< MaxLen ->
    S;
limit_size(S, Len, MaxLen) when Len =< (MaxLen + 5) ->
    %% Looks silly with e.g "... (2 bytes skipped)"
    S;
limit_size(S, Len, MaxLen) when Len > MaxLen ->
    %% Cut
    io_lib:format("~s ... (~w bytes skipped)",
                  [string:substr(lists:flatten(S), 1, MaxLen),
                   Len-MaxLen]).

crypto_log_info() ->
    try
        [{_,_,CI}] = crypto:info_lib(),
        case crypto:info_fips() of
            enabled ->
                <<"(",CI/binary,". FIPS enabled)">>;
            not_enabled ->
                <<"(",CI/binary,". FIPS available but not enabled)">>;
            _ ->
                <<"(",CI/binary,")">>
        end
    catch
        _:_ -> ""
    end.

ssh_log_version() ->
    case application:get_key(ssh,vsn) of
        {ok,Vsn} -> Vsn;
        undefined -> ""
    end.

%%%----------------------------------------------------------------
not_connected_filter({connection_reply, _Data}) -> true;
not_connected_filter(_) -> false.

%%%----------------------------------------------------------------

send_replies({Repls,C = #connection{}}, D) when is_list(Repls) ->
    send_replies(Repls, D#data{connection_state=C});
send_replies(Repls, State) ->
    lists:foldl(fun get_repl/2, {[],State}, Repls).

get_repl({connection_reply,Msg}, {CallRepls,S}) ->
    if is_record(Msg, ssh_msg_channel_success) ->
	    update_inet_buffers(S#data.socket);
       true ->
	    ok
    end,
    {CallRepls, send_msg(Msg,S)};
get_repl({channel_data,undefined,_Data}, Acc) ->
    Acc;
get_repl({channel_data,Pid,Data}, Acc) ->
    Pid ! {ssh_cm, self(), Data},
    Acc;
get_repl({channel_request_reply,From,Data}, {CallRepls,S}) ->
    {[{reply,From,Data}|CallRepls], S};
get_repl({flow_control,Cache,Channel,From,Msg}, {CallRepls,S}) ->
    ssh_client_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
    {[{reply,From,Msg}|CallRepls], S};
get_repl({flow_control,From,Msg}, {CallRepls,S}) ->
    {[{reply,From,Msg}|CallRepls], S};
%% get_repl(noreply, Acc) ->
%%     Acc;
%% get_repl([], Acc) ->
%%     Acc;
get_repl(X, Acc) ->
    exit({get_repl,X,Acc}).

%%%----------------------------------------------------------------
%%disconnect_fun({disconnect,Msg}, D) -> ?CALL_FUN(disconnectfun,D)(Msg);
disconnect_fun(Reason, D)           -> ?CALL_FUN(disconnectfun,D)(Reason).

unexpected_fun(UnexpectedMessage, #data{ssh_params = #ssh{peer = {_,Peer} }} = D) ->
    ?CALL_FUN(unexpectedfun,D)(UnexpectedMessage, Peer).

debug_fun(#ssh_msg_debug{always_display = Display,
			 message = DbgMsg,
			 language = Lang},
	  D) ->
    ?CALL_FUN(ssh_msg_debug_fun,D)(self(), Display, DbgMsg, Lang).


%%%----------------------------------------------------------------
%%% Cache idle timer that closes the connection if there are no
%%% channels open for a while.

cond_set_idle_timer(D) ->
    case ssh_client_channel:cache_info(num_entries, cache(D)) of
        0 -> {{timeout,idle_time}, ?GET_OPT(idle_time, (D#data.ssh_params)#ssh.opts), none};
        _ -> {{timeout,idle_time}, infinity, none}
    end.

%%%----------------------------------------------------------------
start_channel_request_timer(_,_, infinity) ->
    ok;
start_channel_request_timer(Channel, From, Time) ->
    erlang:send_after(Time, self(), {timeout, {Channel, From}}).

%%%----------------------------------------------------------------

init_inet_buffers_window(Socket) ->
    %% Initialize the inet buffer handling. First try to increase the buffers:
    update_inet_buffers(Socket),
    %% then get good start values for the window handling:
    {ok,SockOpts} = inet:getopts(Socket, [buffer,recbuf]),
    WinSz = proplists:get_value(recbuf, SockOpts, ?DEFAULT_WINDOW_SIZE),
    PktSz = min(proplists:get_value(buffer, SockOpts, ?DEFAULT_PACKET_SIZE),
                ?DEFAULT_PACKET_SIZE),  % Too large packet size might cause deadlock
                                        % between sending and receiving
    {WinSz, PktSz}.

update_inet_buffers(Socket) ->
    try
        {ok, BufSzs0} = inet:getopts(Socket, [sndbuf,recbuf]),
        MinVal = 655360,
        [{Tag,MinVal} || {Tag,Val} <- BufSzs0,
                         Val < MinVal]
    of
	[] -> ok;
	NewOpts ->
            inet:setopts(Socket, NewOpts),
            %% Note that buffers might be of different size than we just requested,
            %% the OS has the last word.
            ok
    catch
        _:_ -> ok
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [terminate, disconnect, connections, connection_events, renegotiation,
                           tcp, connection_handshake].

ssh_dbg_flags(connections) -> [c | ssh_dbg_flags(terminate)];
ssh_dbg_flags(renegotiation) -> [c];
ssh_dbg_flags(connection_events) -> [c];
ssh_dbg_flags(connection_handshake) -> [c];
ssh_dbg_flags(terminate) -> [c];
ssh_dbg_flags(tcp) -> [c];
ssh_dbg_flags(disconnect) -> [c].

ssh_dbg_on(connections) -> dbg:tp(?MODULE,  init, 1, x),
                           ssh_dbg_on(terminate);
ssh_dbg_on(connection_events) -> dbg:tp(?MODULE,   handle_event, 4, x);
ssh_dbg_on(connection_handshake) -> dbg:tpl(?MODULE, handshake, 3, x);
ssh_dbg_on(renegotiation) -> dbg:tpl(?MODULE,   init_renegotiate_timers, 3, x),
                             dbg:tpl(?MODULE,   pause_renegotiate_timers, 3, x),
                             dbg:tpl(?MODULE,   check_data_rekeying_dbg, 2, x),
                             dbg:tpl(?MODULE,   start_rekeying, 2, x),
                             dbg:tp(?MODULE,   renegotiate, 1, x);
ssh_dbg_on(terminate) -> dbg:tp(?MODULE,  terminate, 3, x);
ssh_dbg_on(tcp) -> dbg:tp(?MODULE, handle_event, 4,
                          [{[info, {tcp,'_','_'},       '_', '_'], [], []},
                           {[info, {tcp_error,'_','_'}, '_', '_'], [], []},
                           {[info, {tcp_closed,'_'},    '_', '_'], [], []}
                          ]),
                   dbg:tp(?MODULE, send_bytes, 2, x),
                   dbg:tpl(?MODULE, close_transport, 1, x);

ssh_dbg_on(disconnect) -> dbg:tpl(?MODULE,  send_disconnect, 7, x).


ssh_dbg_off(disconnect) -> dbg:ctpl(?MODULE, send_disconnect, 7);
ssh_dbg_off(terminate) -> dbg:ctpg(?MODULE, terminate, 3);
ssh_dbg_off(tcp) -> dbg:ctpg(?MODULE, handle_event, 4), % How to avoid cancelling 'connection_events' ?
                    dbg:ctpl(?MODULE, send_bytes, 2),
                    dbg:ctpl(?MODULE, close_transport, 1);
ssh_dbg_off(renegotiation) -> dbg:ctpl(?MODULE,   init_renegotiate_timers, 3),
                              dbg:ctpl(?MODULE,   pause_renegotiate_timers, 3),
                              dbg:ctpl(?MODULE,   check_data_rekeying_dbg, 2),
                              dbg:ctpl(?MODULE,   start_rekeying, 2),
                              dbg:ctpg(?MODULE,   renegotiate, 1);
ssh_dbg_off(connection_events) -> dbg:ctpg(?MODULE, handle_event, 4);
ssh_dbg_off(connection_handshake) -> dbg:ctpl(?MODULE, handshake, 3);
ssh_dbg_off(connections) -> dbg:ctpg(?MODULE, init, 1),
                            ssh_dbg_off(terminate).


ssh_dbg_format(connections, {call, {?MODULE,init, [[Role, Sock, Opts]]}}) ->
    DefaultOpts = ssh_options:handle_options(Role,[]),
    ExcludedKeys = [internal_options, user_options],
    NonDefaultOpts =
        maps:filter(fun(K,V) ->
                            case lists:member(K,ExcludedKeys) of
                                true ->
                                    false;
                                false ->
                                    V =/= (catch maps:get(K,DefaultOpts))
                            end
                    end,
                    Opts),
    {ok, {IPp,Portp}} = inet:peername(Sock),
    {ok, {IPs,Ports}} = inet:sockname(Sock),
    [io_lib:format("Starting ~p connection:\n",[Role]),
     io_lib:format("Socket = ~p, Peer = ~s, Local = ~s,~n"
                   "Non-default options:~n~p",
                   [Sock,
                    ssh_lib:format_address_port(IPp,Portp),
                    ssh_lib:format_address_port(IPs,Ports),
                    NonDefaultOpts])
    ];
ssh_dbg_format(connections, F) ->
    ssh_dbg_format(terminate, F);

ssh_dbg_format(connection_events, {call, {?MODULE,handle_event, [EventType, EventContent, State, _Data]}}) ->
    ["Connection event\n",
     io_lib:format("EventType: ~p~nEventContent: ~p~nState: ~p~n", [EventType, EventContent, State])
    ];
ssh_dbg_format(connection_events, {return_from, {?MODULE,handle_event,4}, Ret}) ->
    ["Connection event result\n",
     io_lib:format("~p~n", [ssh_dbg:reduce_state(Ret, #data{})])
    ];

ssh_dbg_format(tcp, {call, {?MODULE,handle_event, [info, {tcp,Sock,TcpData}, State, _Data]}}) ->
    ["TCP stream data arrived\n",
     io_lib:format("State: ~p~n"
                   "Socket: ~p~n"
                   "TcpData:~n~s", [State, Sock, ssh_dbg:hex_dump(TcpData, [{max_bytes,48}])])
    ];
ssh_dbg_format(tcp, {call, {?MODULE,handle_event, [info, {tcp_error,Sock,Msg}, State, _Data]}}) ->
    ["TCP stream data ERROR arrived\n",
     io_lib:format("State: ~p~n"
                   "Socket: ~p~n"
                   "ErrorMsg:~p~n", [State, Sock, Msg])
    ];
ssh_dbg_format(tcp, {call, {?MODULE,handle_event, [info, {tcp_closed,Sock}, State, _Data]}}) ->
    ["TCP stream closed\n",
     io_lib:format("State: ~p~n"
                   "Socket: ~p~n", [State, Sock])
    ];
ssh_dbg_format(tcp, {return_from, {?MODULE,handle_event,4}, _Ret}) ->
    skip;

ssh_dbg_format(tcp, {call, {?MODULE, send_bytes, ["",_D]}}) ->
    skip;
ssh_dbg_format(tcp, {call, {?MODULE, send_bytes, [TcpData, #data{socket=Sock}]}}) ->
    ["TCP send stream data\n",
     io_lib:format("Socket: ~p~n"
                   "TcpData:~n~s", [Sock, ssh_dbg:hex_dump(TcpData, [{max_bytes,48}])])
    ];
ssh_dbg_format(tcp, {return_from, {?MODULE,send_bytes,2}, _R}) ->
    skip;

ssh_dbg_format(tcp, {call, {?MODULE, close_transport, [#data{socket=Sock}]}}) ->
    ["TCP close stream\n",
     io_lib:format("Socket: ~p~n", [Sock])
    ];
ssh_dbg_format(tcp, {return_from, {?MODULE,close_transport,1}, _R}) ->
    skip;

ssh_dbg_format(renegotiation, {call, {?MODULE,init_renegotiate_timers,[OldState,NewState,D]}}) ->
    ["Renegotiation: start timer (init_renegotiate_timers)\n",
     io_lib:format("State: ~p  -->  ~p~n"
                   "rekey_limit: ~p ({ms,bytes})~n"
                   "check_data_size: ~p (ms)~n",
                   [OldState, NewState,
                    ?GET_OPT(rekey_limit, (D#data.ssh_params)#ssh.opts),
                    ?REKEY_DATA_TIMOUT])
    ];
ssh_dbg_format(renegotiation, {return_from, {?MODULE,init_renegotiate_timers,3}, _Ret}) ->
    skip;

ssh_dbg_format(renegotiation, {call, {?MODULE,renegotiate,[ConnectionHandler]}}) ->
    ["Renegotiation: renegotiation forced\n",
     io_lib:format("~p:renegotiate(~p) called~n",
                   [?MODULE,ConnectionHandler])
    ];
ssh_dbg_format(renegotiation, {return_from, {?MODULE,renegotiate,1}, _Ret}) ->
    skip;

ssh_dbg_format(renegotiation, {call, {?MODULE,pause_renegotiate_timers,[OldState,NewState,_D]}}) ->
    ["Renegotiation: pause timers\n",
     io_lib:format("State: ~p  -->  ~p~n",
                   [OldState, NewState])
    ];
ssh_dbg_format(renegotiation, {return_from, {?MODULE,pause_renegotiate_timers,3}, _Ret}) ->
    skip;

ssh_dbg_format(renegotiation, {call, {?MODULE,start_rekeying,[_Role,_D]}}) ->
    ["Renegotiation: start rekeying\n"];
ssh_dbg_format(renegotiation, {return_from, {?MODULE,start_rekeying,2}, _Ret}) ->
    skip;

ssh_dbg_format(renegotiation, {call, {?MODULE,check_data_rekeying_dbg,[SentSinceRekey, MaxSent]}}) ->
    ["Renegotiation: check size of data sent\n",
     io_lib:format("TotalSentSinceRekey: ~p~nMaxBeforeRekey: ~p~nStartRekey: ~p~n",
                   [SentSinceRekey, MaxSent, SentSinceRekey >= MaxSent])
    ];
ssh_dbg_format(renegotiation, {return_from, {?MODULE,check_data_rekeying_dbg,2}, _Ret}) ->
    skip;


ssh_dbg_format(terminate, {call, {?MODULE,terminate, [Reason, StateName, D]}}) ->
    ExtraInfo =
        try
            {conn_info(peer,D),
             conn_info(user,D),
             conn_info(sockname,D)}
        of
            {{_,{IPp,Portp}}, Usr, {IPs,Ports}} when is_tuple(IPp), is_tuple(IPs),
                                                     is_integer(Portp), is_integer(Ports) ->
                io_lib:format("Peer=~s:~p, Local=~s:~p, User=~p",
                              [inet:ntoa(IPp),Portp,inet:ntoa(IPs),Ports,Usr]);
            {Peer,Usr,Sockname} ->
                io_lib:format("Peer=~p, Local=~p, User=~p",[Peer,Sockname,Usr])
        catch
            _:_ ->
                ""
        end,
    if
        Reason == normal ;
        Reason == shutdown ;
        element(1,Reason) == shutdown
        ->
            ["Connection Terminating:\n",
             io_lib:format("Reason: ~p, StateName: ~p~n~s", [Reason, StateName, ExtraInfo])
            ];

        true ->
            ["Connection Terminating:\n",
             io_lib:format("Reason: ~p, StateName: ~p~n~s~nStateData = ~p",
                           [Reason, StateName, ExtraInfo, clean(D)])
            ]
    end;
ssh_dbg_format(renegotiation, {return_from, {?MODULE,terminate,3}, _Ret}) ->
    skip;

ssh_dbg_format(disconnect, {call,{?MODULE,send_disconnect,
                                     [Code, Reason, DetailedText, Module, Line, StateName, _D]}}) ->
    ["Disconnecting:\n",
     io_lib:format(" Module = ~p, Line = ~p, StateName = ~p,~n"
                   " Code = ~p, Reason = ~p,~n"
                   " DetailedText =~n"
                   " ~p",
                   [Module, Line, StateName, Code, Reason, lists:flatten(DetailedText)])
    ];
ssh_dbg_format(renegotiation, {return_from, {?MODULE,send_disconnect,7}, _Ret}) ->
    skip.


ssh_dbg_format(connection_handshake, {call, {?MODULE,handshake,[Pid, Ref, Timeout]}}, Stack) ->
    {["Connection handshake\n",
      io_lib:format("Connection Child: ~p~nReg: ~p~nTimeout: ~p~n",
                   [Pid, Ref, Timeout])
     ],
     [Pid|Stack]
    };
ssh_dbg_format(connection_handshake, {Tag, {?MODULE,handshake,3}, Ret}, [Pid|Stack]) ->
    {[lists:flatten(io_lib:format("Connection handshake result ~p\n", [Tag])),
      io_lib:format("Connection Child: ~p~nRet: ~p~n",
                    [Pid, Ret])
     ],
     Stack
    }.
