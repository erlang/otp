%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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
%% Purpose: Provide help function to handle generic parts of TLS
%% connection fsms
%%----------------------------------------------------------------------

-module(ssl_gen_statem).
-moduledoc false.

-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_connection.hrl").
-include("ssl_alert.hrl").
-include("tls_handshake.hrl").
-include("tls_connection.hrl").

%% Initial Erlang process setup
-export([start_link/7,
         start_link/8,
         init/1]).

%% TLS connection setup
-export([opposite_role/1,
         init_ssl_config/3,
         ssl_config/3,
         connect/8,
         handshake/7,
         handshake/2,
         handshake/3,
         handshake_continue/3,
         handshake_cancel/1,
         handle_sni_extension/2,
	 socket_control/4,
         socket_control/5,
         prepare_connection/2]).

%% User Events
-export([send/2,
         recv/3,
         close/2,
         shutdown/2,
	 new_user/2,
         get_opts/2,
         set_opts/2,
	 peer_certificate/1,
         negotiated_protocol/1,
	 connection_information/2,
         ktls_handover/1
	]).

%% Erlang Distribution export
-export([dist_handshake_complete/2]).

%% Generic fsm states
-export([initial_hello/3,
         config_error/3,
         connection/3,
         downgrade/3]).

-export([call/2,
         handle_common_event/4,
         handle_call/4,
         handle_info/3
        ]).

-export([hibernate_after/3]).

%% Data handling
-export([read_application_data/2]).

%% Alert and close handling
-export([send_alert/3,
         handle_own_alert/3,
         handle_alert/3,
	 handle_normal_shutdown/3,
         handle_trusted_certs_db/1,
         maybe_invalidate_session/6,
         maybe_invalidate_session/5,
         terminate/3]).

%% Log handling
-export([format_status/2]).

%% Tracing
-export([handle_trace/3]).

-export_type([secret_printout/0]).

-type secret_printout() :: iodata().

%%--------------------------------------------------------------------
%%% Initial Erlang process setup
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec start_link(client| server, pid(), ssl:host(), inet:port_number(), port(), tuple(), pid(), tuple()) ->
    {ok, pid()} | ignore |  {error, ssl:reason()}.
%%
%% Description: Creates a process which calls Module:init/1 to
%% choose appropriat gen_statem and initialize.
%%--------------------------------------------------------------------
start_link(Role, Sender, Host, Port, Socket, {SslOpts, _, _} = Options, User, CbInfo) ->
    ReceiverOpts = maps:get(receiver_spawn_opts, SslOpts, []),
    Opts = [link | proplists:delete(link, ReceiverOpts)],
    Pid = proc_lib:spawn_opt(?MODULE, init, [[Role, Sender, Host, Port, Socket, Options, User, CbInfo]], Opts),
    {ok, Pid}.

%%--------------------------------------------------------------------
-spec start_link(atom(), ssl:host(), inet:port_number(), port(), tuple(), pid(), tuple()) ->
			{ok, pid()} | ignore |  {error, ssl:reason()}.
%%
%% Description: Creates a gen_statem process which calls Module:init/1 to
%% initialize.
%%--------------------------------------------------------------------
start_link(Role, Host, Port, Socket, {SslOpts, _, _} = Options, User, CbInfo) ->
    ReceiverOpts = maps:get(receiver_spawn_opts, SslOpts, []),
    Opts = [link | proplists:delete(link, ReceiverOpts)],
    Pid = proc_lib:spawn_opt(?MODULE, init, [[Role, Host, Port, Socket, Options, User, CbInfo]], Opts),
    {ok, Pid}.


%%--------------------------------------------------------------------
-spec init(list()) -> no_return().
%% Description: Initialization
%%--------------------------------------------------------------------
init([Role, _Sender, Host, Port, _Socket, {TLSOpts, _, _},  _User, _CbInfo] = InitArgs) ->
    process_flag(trap_exit, true),

    case maps:get(erl_dist, TLSOpts, false) of
        true ->
            process_flag(priority, max);
        _ ->
            ok
    end,    
    
    init_label(Role, Host, Port, TLSOpts),

    case Role of 
        ?CLIENT_ROLE ->
            case TLSOpts of
                #{versions := [?TLS_1_3]} ->
                    tls_client_connection_1_3:init(InitArgs);
                _  ->
                    tls_client_connection:init(InitArgs)
            end;     
        ?SERVER_ROLE ->
            case TLSOpts of
                #{versions := [?TLS_1_3]} ->
                    tls_server_connection_1_3:init(InitArgs);
                _ ->
                    tls_server_connection:init(InitArgs)
            end                                                       
    end;
init([Role, Host, Port, _Socket, {DTLSOpts,_,_}, _User, _CbInfo] = InitArgs) ->
    process_flag(trap_exit, true),

    init_label(Role, Host, Port, DTLSOpts),

    case Role of
        ?CLIENT_ROLE ->
            dtls_client_connection:init(InitArgs);
        ?SERVER_ROLE ->
            dtls_server_connection:init(InitArgs)
    end.

init_label(?CLIENT_ROLE = Role, Host, _, Options) ->
    Protocol = maps:get(protocol, Options),
    SNIStr =
        case maps:get(server_name_indication, Options, undefined) of
            undefined ->
                host_str(Host);
            SNIOpt ->
                host_str(SNIOpt)
        end,
    SNI = erlang:iolist_to_binary(SNIStr),
    proc_lib:set_label({Protocol, Role, SNI});
init_label(?SERVER_ROLE = Role, _, Port, Options) ->
    Protocol = maps:get(protocol, Options),
    proc_lib:set_label({Protocol, Role, Port}).

host_str(Host) when is_list(Host) ->
    Host;
host_str({local, File}) when is_list(File); is_binary(File) ->
    File;
host_str(IPv4) when tuple_size(IPv4) =:= 4 ->
    IPStrs = [erlang:integer_to_list(I) || I <- tuple_to_list(IPv4)],
    lists:join(".", IPStrs);
host_str(IPv6) when tuple_size(IPv6) =:= 8 ->
    IPStrs = [erlang:integer_to_list(I, 16) || I <- tuple_to_list(IPv6)],
    lists:join(":", IPStrs);
host_str(Host) when is_atom(Host) ->
    atom_to_list(Host).

%%====================================================================
%% TLS connection setup
%%====================================================================

%%--------------------------------------------------------------------
-spec opposite_role(client | server) ->  client | server.
%%--------------------------------------------------------------------
opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

%%--------------------------------------------------------------------
-spec init_ssl_config(ssl_options(), client | server, #state{}) -> #state{}.
%%--------------------------------------------------------------------
init_ssl_config(Opts, Role, #state{ssl_options = #{handshake := Handshake},
                                   handshake_env = HsEnv} = State0) ->
     ContinueStatus = case Handshake of
                          hello ->
                             %% Will pause handshake after hello message to
                             %% enable user to react to hello extensions
                             pause;
                         full ->
                             Handshake
                      end,
    ssl_config(Opts, Role,
               State0#state{handshake_env =
                                HsEnv#handshake_env{continue_status = ContinueStatus}}).

%%--------------------------------------------------------------------
-spec ssl_config(ssl_options(), client | server, #state{}) -> #state{}.
%%--------------------------------------------------------------------
ssl_config(Opts, Role, #state{static_env = InitStatEnv0,
                              handshake_env = HsEnv,
                              connection_env = CEnv} = State0) ->
    {ok, #{cert_db_ref := Ref,
           cert_db_handle := CertDbHandle,
           fileref_db_handle := FileRefHandle,
           session_cache := CacheHandle,
           crl_db_info := CRLDbHandle,
           cert_key_alts := CertKeyAlts,
           dh_params := DHParams}} =
	ssl_config:init(Opts, Role),
    TimeStamp = erlang:monotonic_time(),
    Session = State0#state.session,

    State0#state{session = Session#session{time_stamp = TimeStamp},
                 static_env = InitStatEnv0#static_env{
                                file_ref_db = FileRefHandle,
                                cert_db_ref = Ref,
                                cert_db = CertDbHandle,
                                crl_db = CRLDbHandle,
                                session_cache = CacheHandle
                               },
                 handshake_env =
                     HsEnv#handshake_env{diffie_hellman_params = DHParams},
                 connection_env = CEnv#connection_env{cert_key_alts = CertKeyAlts},
                 ssl_options = Opts}.

%%--------------------------------------------------------------------
-spec connect(tls_gen_connection | dtls_gen_connection,
	      ssl:host(), inet:port_number(),
	      port() | {tuple(), port()}, %% TLS | DTLS
	      {ssl_options(), #socket_options{},
	       %% Tracker only needed on server side
	       undefined},
	      pid(), tuple(), timeout()) ->
		     {ok, #sslsocket{}} | {error, ssl:reason()}.
%%
%% Description: Connect to an ssl server.
%%--------------------------------------------------------------------
connect(Connection, Host, Port, Socket, Options, User, CbInfo, Timeout) ->
    try Connection:start_fsm(client, Host, Port, Socket, Options, User, CbInfo,
			     Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.
%%--------------------------------------------------------------------
-spec handshake(tls_gen_connection | dtls_gen_connection,
		 inet:port_number(), port(),
		 {ssl_options(), #socket_options{}, list()},
		 pid(), tuple(), timeout()) ->
			{ok, #sslsocket{}} | {error, ssl:reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake.
%%--------------------------------------------------------------------
handshake(Connection, Port, Socket, Opts, User, CbInfo, Timeout) ->
    try Connection:start_fsm(server, "localhost", Port, Socket, Opts, User,
		  CbInfo, Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}, timeout()) ->  {ok, #sslsocket{}} |
                                             {ok,  #sslsocket{}, map()}| {error, ssl:reason()}.
%%
%% Description: Starts ssl handshake.
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = [Pid|_]} = Socket, Timeout) ->
    case call(Pid, {start, Timeout}) of
	connected ->
	    {ok, Socket};
        {ok, Ext} ->
            {ok, Socket, no_records(Ext)};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}, {SSLOpts::list(), #socket_options{}}, timeout()) ->
          {ok, #sslsocket{}} | {ok, #sslsocket{}, map()} | {error, ssl:reason()}.
%%
%% Description: Starts ssl handshake with some new options
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = [Pid|_]} = Socket, SslOptions, Timeout) ->
    case call(Pid, {start, SslOptions, Timeout}) of
	connected ->
	    {ok, Socket};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec handshake_continue(#sslsocket{}, [ssl:tls_server_option()],
                         timeout()) ->  {ok,  #sslsocket{}}| {error, ssl:reason()}.
%%
%% Description: Continues handshake with new options
%%--------------------------------------------------------------------
handshake_continue(#sslsocket{pid = [Pid|_]} = Socket, SslOptions, Timeout) ->
    case call(Pid, {handshake_continue, SslOptions, Timeout}) of
	connected ->
	    {ok, Socket};
	Error ->
	    Error
    end.
%%--------------------------------------------------------------------
-spec handshake_cancel(#sslsocket{}) ->  ok | {error, ssl:reason()}.
%%
%% Description: Cancels connection
%%--------------------------------------------------------------------
handshake_cancel(#sslsocket{pid = [Pid|_]}) ->
    case call(Pid, cancel) of
	closed ->
            ok;
        Error ->
	    Error
    end.
%--------------------------------------------------------------------
-spec socket_control(tls_gen_connection | dtls_gen_connection, port(), [pid()], atom()) ->
    {ok, #sslsocket{}} | {error, ssl:reason()}.
%%
%% Description: Set the ssl process to own the accept socket
%%--------------------------------------------------------------------
socket_control(Connection, Socket, Pid, Transport) ->
    socket_control(Connection, Socket, Pid, Transport, undefined).

%--------------------------------------------------------------------
-spec socket_control(tls_gen_connection | dtls_gen_connection, port(), [pid()], atom(), [pid()] | atom()) ->
    {ok, #sslsocket{}} | {error, ssl:reason()}.
%%--------------------------------------------------------------------
socket_control(dtls_gen_connection, Socket, Pids, Transport, udp_listener) ->
    %% dtls listener process must have the socket control
    {ok, dtls_gen_connection:socket(Pids, Transport, Socket, undefined)};

socket_control(tls_gen_connection, Socket, [Pid|_] = Pids, Transport, Trackers) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, tls_gen_connection:socket(Pids, Transport, Socket, Trackers)};
	{error, Reason}	->
	    {error, Reason}
    end;
socket_control(dtls_gen_connection, {PeerAddrPort, Socket},
               [Pid|_] = Pids, Transport, Trackers) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, dtls_gen_connection:socket(Pids, Transport, {PeerAddrPort, Socket},
                                            Trackers)};
	{error, Reason}	->
	    {error, Reason}
    end.

prepare_connection(#state{handshake_env = #handshake_env{renegotiation = Renegotiate},
                          recv = #recv{from = RecvFrom}} = State0, Connection)
  when Renegotiate =/= {false, first},
       RecvFrom =/= undefined ->
    State = Connection:reinit(State0),
    {no_record, ack_connection(State)};
prepare_connection(State0, Connection) ->
    State = Connection:reinit(State0),
    {no_record, ack_connection(State)}.

%%====================================================================
%% User events
%%====================================================================

%%--------------------------------------------------------------------
-spec send(pid(), iodata()) -> ok | {error, ssl:reason()}.
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(Pid, Data) ->
    call(Pid, {application_data,
				    %% iolist_to_iovec should really
				    %% be called iodata_to_iovec()
				    erlang:iolist_to_iovec(Data)}).

%%--------------------------------------------------------------------
-spec recv(pid(), integer(), timeout()) ->
    {ok, binary() | list()} | {error, ssl:reason()}.
%%
%% Description:  Receives data when active = false
%%--------------------------------------------------------------------
recv(Pid, Length, Timeout) ->
    call(Pid, {recv, Length, Timeout}).

%%--------------------------------------------------------------------
-spec connection_information(pid(), boolean()) -> {ok, list()} | {error, ssl:reason()}.
%%
%% Description: Get connection information
%%--------------------------------------------------------------------
connection_information(Pid, IncludeSecrityInfo) when is_pid(Pid) ->
    case call(Pid, {connection_information, IncludeSecrityInfo}) of
        {ok, Info} when IncludeSecrityInfo == true ->
            {ok, maybe_add_keylog(Info)};
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
-spec close(pid(), {close, Timeout::integer() |
				    {NewController::pid(), Timeout::integer()}}) ->
		   ok | {ok, port()} | {error, ssl:reason()}.
%%
%% Description:  Close an ssl connection
%%--------------------------------------------------------------------
close(ConnectionPid, How) ->
    case call(ConnectionPid, How) of
	{error, closed} ->
	    ok;
	Other ->
	    Other
    end.
%%--------------------------------------------------------------------
-spec shutdown(pid(), atom()) -> ok | {error, ssl:reason()}.
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(ConnectionPid, How) ->
    call(ConnectionPid, {shutdown, How}).

%%--------------------------------------------------------------------
-spec new_user(pid(), pid()) ->  ok | {error, ssl:reason()}.
%%
%% Description:  Changes process that receives the messages when active = true
%% or once.
%%--------------------------------------------------------------------
new_user(ConnectionPid, User) ->
    call(ConnectionPid, {new_user, User}).

%%--------------------------------------------------------------------
-spec get_opts(pid(), list()) -> {ok, list()} | {error, ssl:reason()}.
%%
%% Description: Same as inet:getopts/2
%%--------------------------------------------------------------------
get_opts(ConnectionPid, OptTags) ->
    call(ConnectionPid, {get_opts, OptTags}).
%%--------------------------------------------------------------------
-spec set_opts(pid(), list()) -> ok | {error, ssl:reason()}.
%%
%% Description:  Same as inet:setopts/2
%%--------------------------------------------------------------------
set_opts(ConnectionPid, Options) ->
    call(ConnectionPid, {set_opts, Options}).

%%--------------------------------------------------------------------
-spec peer_certificate(pid()) -> {ok, binary()| undefined} | {error, ssl:reason()}.
%%
%% Description: Returns the peer cert
%%--------------------------------------------------------------------
peer_certificate(ConnectionPid) ->
    call(ConnectionPid, peer_certificate).

%%--------------------------------------------------------------------
-spec negotiated_protocol(pid()) -> {ok, binary()} | {error, ssl:reason()}.
%%
%% Description:  Returns the negotiated protocol
%%--------------------------------------------------------------------
negotiated_protocol(ConnectionPid) ->
    call(ConnectionPid, negotiated_protocol).

%%--------------------------------------------------------------------
-spec ktls_handover(pid()) -> {ok, map()} | {error, ssl:reason()}.
%%
%% Description:  Returns the negotiated protocol
%%--------------------------------------------------------------------
ktls_handover(ConnectionPid) ->
    call(ConnectionPid, ktls_handover).

dist_handshake_complete(ConnectionPid, DHandle) ->
    gen_statem:cast(ConnectionPid, {dist_handshake_complete, DHandle}).

handle_sni_extension(undefined, State) ->
    {ok, State};
handle_sni_extension(#sni{hostname = Hostname}, #state{static_env = #static_env{port = Port},
                                                       ssl_options = #{protocol := Protocol}} = State0) ->
    case check_hostname(Hostname) of
        ok ->

            proc_lib:set_label({Protocol, ?SERVER_ROLE, erlang:iolist_to_binary(Hostname), Port}),
            {ok, handle_sni_hostname(Hostname, State0)};
        #alert{} = Alert ->
            {error, Alert}
    end.

%%====================================================================
%% Generic states
%%====================================================================
%%--------------------------------------------------------------------
-spec initial_hello(gen_statem:event_type(),
                    {start, timeout()} |  {start, {list(), list()}, timeout()}| term(),
                    #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
initial_hello({call, From}, {new_user, _} = Msg, State) ->
    handle_call(Msg, From, ?STATE(initial_hello), State);
initial_hello(info, {'DOWN', _, _, _, _} = Event, State) ->
    handle_info(Event, ?STATE(initial_hello), State);
initial_hello(_Type, _Event, _State) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
                   {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error({call, From}, {start, _Timeout},
      #state{protocol_specific = #{error := Error}} = State) ->
    {stop_and_reply, {shutdown, normal},
     [{reply, From, {error, Error}}], State};
config_error({call, From}, {close, _}, State) ->
    {stop_and_reply, {shutdown, normal}, {reply, From, ok}, State};
config_error({call, From}, _Msg, State) ->
    {next_state, ?STATE(config_error), State, [{reply, From, {error, closed}}]};
config_error(info, {'DOWN', _, _, _, _} = Event, State) ->
    handle_info(Event, ?STATE(config_error), State);
config_error(_Type, _Event, _State) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(), term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection({call, RecvFrom}, {recv, N, Timeout},
	   #state{recv = Recv,
                  socket_options = #socket_options{active = false}
                 } = State0) ->
    passive_receive(State0#state{recv = Recv#recv{from = RecvFrom,
                                                  bytes_to_read = N}},
                    ?STATE(connection), [{{timeout, recv}, Timeout, timeout}]);
connection({call, From}, peer_certificate,
	   #state{session = #session{peer_certificate = Cert}} = State) ->
    hibernate_after(?STATE(connection), State, [{reply, From,  {ok, Cert}}]);
connection({call, From}, {connection_information, true}, State) ->
    Info = connection_info(State) ++ security_info(State),
    hibernate_after(?STATE(connection), State, [{reply, From, {ok, Info}}]);
connection({call, From}, {connection_information, false}, State) ->
    Info = connection_info(State),
    hibernate_after(?STATE(connection), State, [{reply, From, {ok, Info}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = undefined}} = State) ->
    hibernate_after(?STATE(connection), State, [{reply, From, {error, protocol_not_negotiated}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = SelectedProtocol}} = State) ->
    hibernate_after(?STATE(connection), State,
		    [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = SelectedProtocol,
                                                 negotiated_protocol = undefined}} = State) ->
    hibernate_after(?STATE(connection), State,
		    [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, 
           {close,{_NewController, _Timeout}},
           #state{static_env = #static_env{role = Role,
                                           socket = Socket,
                                           trackers = Trackers,
                                           transport_cb = Transport,
                                           protocol_cb = Connection},
                  connection_env = #connection_env{socket_tls_closed = #alert{} = Alert}
                 } = State) ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Trackers, Socket, From, Alert, Role, ?STATE(connection), Connection),
    {stop, {shutdown, normal}, State};
connection({call, From}, 
           {close,{NewController, Timeout}},
           #state{connection_states = ConnectionStates,
                  static_env = #static_env{protocol_cb = Connection},
                  protocol_specific = #{sender := Sender} = PS,
                  connection_env = #connection_env{socket_tls_closed = PeerClosedTLS} = CEnv
                 } = State0) ->
    Action = case PeerClosedTLS of
                 true ->
                     %%Close Alert already received from peer, "replay" in downgrade state.
                     [{next_event, internal, ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY)}];
                 false ->
                     [{timeout, Timeout, downgrade}]
             end,
    case tls_sender:downgrade(Sender, Timeout) of
        {ok, Write} ->
            %% User downgrades connection
            %% When downgrading an TLS connection to a transport connection
            %% we must receive the close alert from the peer before releasing the
            %% transport socket. Also after sending our close alert nothing 
            %% more may be sent by the tls_sender process.
            State = Connection:send_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
                                          State0#state{connection_states =
                                                           ConnectionStates#{current_write => Write}}),
            {next_state, downgrade, State#state{connection_env =
                                                    CEnv#connection_env{downgrade = {NewController, From}},
                                                protocol_specific = PS#{active_n_toggle => true,
                                                                        active_n => 1}
                                               },
             Action};
        {error, timeout} ->
            {stop_and_reply, {shutdown, downgrade_fail}, [{reply, From, {error, timeout}}]}
    end;
connection({call, From}, ktls_handover, #state{
    static_env = #static_env{
        transport_cb = Transport,
        socket = Socket
    },
    connection_env = #connection_env{
        user_application = {_Mon, Pid},
        negotiated_version = TlsVersion
    },
    ssl_options = #{ktls := true},
    socket_options = SocketOpts,
    connection_states = #{
        current_write := #{
            security_parameters := #security_parameters{cipher_suite = CipherSuite},
            cipher_state := WriteState,
            sequence_number := WriteSeq
        },
        current_read := #{
            cipher_state := ReadState,
            sequence_number := ReadSeq
        }
    }
}) ->
    Reply = case Transport:controlling_process(Socket, Pid) of
        ok ->
            {ok, #{
                socket => Socket,
                tls_version => TlsVersion,
                cipher_suite => CipherSuite,
                socket_options => SocketOpts,
                write_state => WriteState,
                write_seq => WriteSeq,
                read_state => ReadState,
                read_seq => ReadSeq
            }};
        {error, Reason} ->
            {error, Reason}
    end,
    {stop_and_reply, {shutdown, ktls}, [{reply, From, Reply}]};
connection({call, From}, Msg, State) ->
    handle_call(Msg, From, ?STATE(connection), State);
connection(cast, {dist_handshake_complete, DHandle},
           #state{ssl_options = #{erl_dist := true},
                  static_env = #static_env{protocol_cb = Connection},
                  connection_env = CEnv,
                  recv = Recv,
                  socket_options = SockOpts} = State0) ->
    process_flag(priority, normal),
    State1 =
        State0#state{
          socket_options = SockOpts#socket_options{active = true},
          connection_env = CEnv#connection_env{erl_dist_handle = DHandle},
          recv = Recv#recv{bytes_to_read = undefined}
         },
    {Record, State} = read_application_data(<<>>, State1),
    Connection:next_event(?STATE(connection), Record, State);
connection(info, Msg, #state{static_env = #static_env{protocol_cb = Connection}} = State) ->
    Connection:handle_info(Msg, ?STATE(connection), State);
connection(internal, {recv, RecvFrom},
           #state{recv = #recv{from=RecvFrom}} = State) ->
    passive_receive(State, ?STATE(connection), []);
connection(Type, Msg, State) ->
    handle_common_event(Type, Msg, ?STATE(connection), State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(internal, #alert{description = ?CLOSE_NOTIFY},
	  #state{static_env = #static_env{transport_cb = Transport,
                                          socket = Socket},
		 connection_env = #connection_env{downgrade = {Pid, From}},
                 protocol_buffers = #protocol_buffers{tls_record_buffer = TlsRecordBuffer}
                } = State) ->
    tls_socket:setopts(Transport, Socket, [{active, false}, {packet, 0}, {mode, binary}]),
    Transport:controlling_process(Socket, Pid),
    ReturnValue = case TlsRecordBuffer of
                      {undefined,{[Bin] = _Front, _Size, []}} ->
                          %% Buffered non TLS data returned to downgrade caller
                          {ok, Socket, Bin};
                      _ ->
                          {ok, Socket}
                  end,
    {stop_and_reply, {shutdown, downgrade},[{reply, From, ReturnValue}], State};
downgrade(timeout, downgrade, #state{ connection_env = #connection_env{downgrade = {_, From}}} = State) ->
    {stop_and_reply, {shutdown, normal},[{reply, From, {error, timeout}}], State};
downgrade(info, {CloseTag, Socket},
          #state{static_env = #static_env{socket = Socket, 
                                          close_tag = CloseTag},
                 connection_env = #connection_env{downgrade = {_, From}}} =
              State) ->
    {stop_and_reply, {shutdown, normal},[{reply, From, {error, CloseTag}}], State};
downgrade(info, Info, State) ->
    tls_gen_connection:handle_info(Info, ?STATE(downgrade), State);
downgrade(Type, Event, State) ->
    try
        tls_dtls_gen_connection:downgrade(Type, Event, State)
    catch throw:#alert{} = Alert ->
            handle_own_alert(Alert, ?STATE(downgrade), State)
    end.

%%====================================================================
%%  Event/Msg handling
%%====================================================================
handle_common_event(internal, {handshake, {Handshake, Raw}}, StateName,
		    #state{handshake_env = #handshake_env{tls_handshake_history = Hist0} = HsEnv,
                           connection_env = #connection_env{negotiated_version = _Version}} = State0) ->
    Hist = ssl_handshake:update_handshake_history(Hist0, Raw),
    {next_state, StateName,
     State0#state{handshake_env =
                      HsEnv#handshake_env{tls_handshake_history = Hist}},
     [{next_event, internal, Handshake}]};
handle_common_event(internal, {protocol_record, TLSorDTLSRecord}, StateName,
                    #state{static_env = #static_env{protocol_cb = Connection}} = State) ->
    Connection:handle_protocol_record(TLSorDTLSRecord, StateName, State);
handle_common_event(timeout, hibernate, _, _) ->
    {keep_state_and_data, [hibernate]};
handle_common_event({timeout, handshake}, close, _StateName,
                    #state{recv = #recv{from = StartFrom} = Recv} = State) ->
    {stop_and_reply,
     {shutdown, user_timeout},
     {reply, StartFrom, {error, timeout}}, State#state{recv = Recv#recv{from = undefined}}};
handle_common_event({timeout, recv}, timeout, StateName,
                    #state{recv = #recv{from = RecvFrom} = Recv} = State) ->
    {next_state, StateName,
     State#state{recv = Recv#recv{from = undefined, bytes_to_read = undefined}},
     [{reply, RecvFrom, {error, timeout}}]};
handle_common_event(internal, {recv, RecvFrom}, StateName,
                    #state{recv = #recv{from = RecvFrom}})
  when StateName =/= connection ->
    {keep_state_and_data, [postpone]};
handle_common_event(internal, new_connection, StateName, State) ->
    {next_state, StateName, State};
handle_common_event(Type, Msg, StateName, State) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, {unexpected_msg, {Type, Msg}}),
    handle_own_alert(Alert, StateName, State).

handle_call({application_data, _Data}, _, _, _) ->
    %% In renegotiation priorities handshake, send data when handshake is finished
    {keep_state_and_data, [postpone]};
handle_call({close, _} = Close, From, StateName, #state{connection_env = CEnv} = State) ->
    %% Run terminate before returning so that the reuseaddr
    %% inet-option works properly
    Result = terminate(Close, StateName, State),
    {stop_and_reply,
     {shutdown, normal},
     {reply, From, Result}, State#state{connection_env = CEnv#connection_env{socket_terminated = true}}};
handle_call({shutdown, read_write = How}, From, StateName,
	    #state{static_env = #static_env{transport_cb = Transport,
                                            socket = Socket},
                   connection_env = CEnv} = State) ->
    try send_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
                   StateName, State) of
        _ ->
            try Transport:shutdown(Socket, How) of
                ok ->
                    {next_state, StateName, State#state{connection_env =
                                                            CEnv#connection_env{socket_terminated = true}},
                     [{reply, From, ok}]};
                Error ->
                    {stop_and_reply, {shutdown, normal}, {reply, From, Error},
                     State#state{connection_env = CEnv#connection_env{socket_terminated = true}}}
            catch error:{undef, _} ->
                    {stop_and_reply, {shutdown, normal}, {reply, From, {error, notsup}},
                     State#state{connection_env = CEnv#connection_env{socket_terminated = true}}}
            end
    catch
        throw:Return ->
            Return
    end;
handle_call({shutdown, How0}, From, StateName,
	    #state{static_env = #static_env{transport_cb = Transport,
                                            socket = Socket}} = State) ->
    case Transport:shutdown(Socket, How0) of
	ok ->
	    {next_state, StateName, State, [{reply, From, ok}]};
	Error ->
            {stop_and_reply, {shutdown, normal}, {reply, From, Error}, State}
    end;
handle_call({recv, _N, _Timeout}, From, _,
		  #state{socket_options =
			     #socket_options{active = Active}}) when Active =/= false ->
    {keep_state_and_data, [{reply, From, {error, einval}}]};
handle_call({recv, N, Timeout}, RecvFrom, StateName, #state{recv = Recv} = State) ->
    %% Doing renegotiate wait with handling request until renegotiate is
    %% finished.
    {next_state, StateName, State#state{recv = Recv#recv{from=RecvFrom, bytes_to_read = N}},
     [{next_event, internal, {recv, RecvFrom}} , {{timeout, recv}, Timeout, timeout}]};
handle_call({new_user, User}, From, StateName,
            State = #state{connection_env = #connection_env{user_application = {OldMon, _}} = CEnv}) ->
    NewMon = erlang:monitor(process, User),
    erlang:demonitor(OldMon, [flush]),
    {next_state, StateName, State#state{connection_env = CEnv#connection_env{user_application = {NewMon, User}}},
     [{reply, From, ok}]};
handle_call({get_opts, OptTags}, From, _,
            #state{static_env = #static_env{protocol_cb = Connection,
                                            socket = Socket,
                                            transport_cb = Transport},
                   socket_options = SockOpts}) ->
    OptsReply = get_socket_opts(Connection, Transport, Socket, OptTags, SockOpts, []),
    {keep_state_and_data, [{reply, From, OptsReply}]};
handle_call({set_opts, Opts0}, From, StateName,
	    #state{static_env =  #static_env{protocol_cb = Connection,
                                             socket = Socket,
                                             transport_cb = Transport,
                                             trackers = Trackers},
                   connection_env =
                       #connection_env{user_application = {_Mon, Pid}},
                   socket_options = Opts1
                  } = State0) ->
    {Reply, Opts} = set_socket_opts(Connection, Transport, Socket, Opts0, Opts1, []),
    case {proplists:lookup(active, Opts0), Opts} of
        {{_, N}, #socket_options{active=false}} when is_integer(N) ->
            send_user(
              Pid,
              format_passive(
                Connection:pids(State0), Transport, Socket, Trackers, Connection));
        _ ->
            ok
    end,
    State = State0#state{socket_options = Opts},
    handle_active_option(Opts#socket_options.active, StateName, From, Reply, State);

handle_call(renegotiate, From, StateName, _) when StateName =/= connection ->
    {keep_state_and_data, [{reply, From, {error, already_renegotiating}}]};

handle_call(_,_,_,_) ->
    {keep_state_and_data, [postpone]}.

handle_info({ErrorTag, Socket, econnaborted}, StateName,
	    #state{static_env = #static_env{role = Role,
                                            host = Host,
                                            port = Port,
                                            socket = Socket,
                                            transport_cb = Transport,
                                            error_tag = ErrorTag,
                                            trackers = Trackers,
                                            protocol_cb = Connection},
                   handshake_env = #handshake_env{renegotiation = Type},
                   connection_env = #connection_env{negotiated_version = Version},
                   session = Session,
                   recv = #recv{from = StartFrom}
                  } = State)  when StateName =/= connection ->
    maybe_invalidate_session(Version, Type, Role, Host, Port, Session),
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Trackers, Socket,
               StartFrom, ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), Role, StateName, Connection),
    {stop, {shutdown, normal}, State};

handle_info({ErrorTag, Socket, Reason}, StateName, #state{static_env = #static_env{
                                                                          role = Role,
                                                                          socket = Socket,
                                                                          error_tag = ErrorTag}
                                                         } = State)  ->
    ?SSL_LOG(info, "Socket error", [{error_tag, ErrorTag}, {description, Reason}]),
    Alert = ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, {transport_error, Reason}),
    handle_normal_shutdown(Alert#alert{role = Role}, StateName, State),
    {stop, {shutdown,normal}, State};

handle_info({'DOWN', MonitorRef, _, _, Reason}, _,
            #state{connection_env = #connection_env{user_application = {MonitorRef, _Pid}},
                   ssl_options = #{erl_dist := true}}) ->
    {stop, {shutdown, Reason}};
handle_info({'DOWN', MonitorRef, _, _, _}, _,
            #state{connection_env = #connection_env{user_application = {MonitorRef, _Pid}}}) ->
    {stop, {shutdown, normal}};
handle_info({'EXIT', Pid, _Reason}, StateName,
            #state{connection_env = #connection_env{user_application = {_MonitorRef, Pid}}} = State) ->
    %% It seems the user application has linked to us
    %% - ignore that and let the monitor handle this
    {next_state, StateName, State};
%%% So that terminate will be run when supervisor issues shutdown
handle_info({'EXIT', _Sup, shutdown}, _StateName, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', Socket, normal}, _StateName, #state{static_env = #static_env{socket = Socket}} = State) ->
    %% Handle as transport close"
    {stop,{shutdown, transport_closed}, State};
handle_info({'EXIT', Socket, Reason}, _StateName, #state{static_env = #static_env{socket = Socket}} = State) ->
    {stop,{shutdown, Reason}, State};
handle_info(allow_renegotiate, StateName, #state{handshake_env = HsEnv} = State) -> %% PRE TLS-1.3
    {next_state, StateName, State#state{handshake_env = HsEnv#handshake_env{allow_renegotiate = true}}};
handle_info(Msg, StateName, #state{static_env = #static_env{socket = Socket, error_tag = ErrorTag}} = State) ->
    ?SSL_LOG(notice, "Unexpected INFO message",
             [{message, Msg}, {socket, Socket}, {error_tag, ErrorTag}]),
    {next_state, StateName, State}.

%%====================================================================
%% Application Data
%%====================================================================
read_application_data(Data,
                      #state{user_data_buffer =
                                 {Front0,BufferSize0,Rear0},
                             connection_env =
                                 #connection_env{erl_dist_handle = DHandle}}
                      = State) ->
    Front = Front0,
    BufferSize = BufferSize0 + byte_size(Data),
    Rear = [Data|Rear0],
    case DHandle of
        undefined ->
            read_application_data(State, Front, BufferSize, Rear);
        _ ->
            try read_application_dist_data(DHandle, Front, BufferSize, Rear) of
                Buffer ->
                    {no_record, State#state{user_data_buffer = Buffer}}
            catch
                error:notsup ->
                    %% Distribution controller has shut down
                    %% so we are no longer input handler and therefore
                    %% erlang:dist_ctrl_put_data/2 raises this exception
                    {stop, {shutdown, dist_closed},
                     %% This buffers known data, but we might have delivered
                     %% some of it to the VM, which makes buffering all
                     %% incorrect, as would be wasting all.
                     %% But we are stopping the server so
                     %% user_data_buffer is not important at all...
                     State#state{
                       user_data_buffer = {Front,BufferSize,Rear}}};
                error:Reason:Stacktrace ->
                    %% Unforeseen exception in parsing application data
                    {stop,
                     {disconnect,{error,Reason,Stacktrace}},
                     State#state{
                       user_data_buffer = {Front,BufferSize,Rear}}}
            end
    end.

passive_receive(#state{static_env = #static_env{role = Role,
                                                socket = Socket,
                                                trackers = Trackers,
                                                transport_cb = Transport,
                                                protocol_cb = Connection},
                       recv = #recv{from = RecvFrom},
                       connection_env = #connection_env{socket_tls_closed = #alert{} = Alert}} = State, 
                StateName, _) ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Trackers, Socket, RecvFrom, Alert, Role, StateName, Connection),
    {stop, {shutdown, normal}, State};
passive_receive(#state{user_data_buffer = {Front,BufferSize,Rear},
                       %% Assert! Erl distribution uses active sockets
                       static_env = #static_env{protocol_cb = Connection},
                       connection_env = #connection_env{erl_dist_handle = undefined}}
                = State0, StateName, StartTimerAction) ->
    case BufferSize of
	0 ->
	    Connection:next_event(StateName, no_record, State0, StartTimerAction);
	_ ->
	    case read_application_data(State0, Front, BufferSize, Rear) of
                {stop, _, _} = ShutdownError ->
                    ShutdownError;
                {Record, #state{recv = #recv{from = undefined}} = State} ->
                    Connection:next_event(StateName, Record, State,
                                          [{{timeout, recv}, infinity, timeout}]);
                {Record, State} ->
                    Connection:next_event(StateName, Record, State, StartTimerAction)
            end
    end.

%%====================================================================
%% Hibernation
%%====================================================================

hibernate_after(connection = StateName,
		#state{ssl_options= #{hibernate_after := HibernateAfter}} = State,
                Actions) when HibernateAfter =/= infinity ->
    {next_state, StateName, State,
     [{timeout, HibernateAfter, hibernate} | Actions]};
hibernate_after(StateName, State, []) ->
    {next_state, StateName, State};
hibernate_after(StateName, State, Actions) ->
    {next_state, StateName, State, Actions}.

%%====================================================================
%% Alert and close handling
%%====================================================================
send_alert(Alert, connection, #state{static_env = #static_env{protocol_cb = Connection}} = State) ->
     Connection:send_alert_in_connection(Alert, State);
send_alert(Alert, _, #state{static_env = #static_env{protocol_cb = Connection}} = State) ->
    Connection:send_alert(Alert, State).

handle_own_alert(Alert0, StateName,
		 #state{static_env = #static_env{role = Role,
                                                 protocol_cb = Connection},
                        ssl_options = #{log_level := LogLevel}} = State) ->
    try %% Try to tell the other side
        send_alert(Alert0, StateName, State)
    catch _:_ ->  %% Can crash if we are in a uninitialized state
	    ignore
    end,
    try %% Try to tell the local user
        Alert = Alert0#alert{role = Role},
	log_alert(LogLevel, Role, Connection:protocol_name(), StateName, Alert),
	handle_normal_shutdown(Alert,StateName, State)
    catch _:_ ->
	    ok
    end,
    {stop, {shutdown, own_alert}, State}.

handle_normal_shutdown(Alert, StateName, #state{static_env = #static_env{role = Role,
                                                                         socket = Socket,
                                                                         transport_cb = Transport,
                                                                         protocol_cb = Connection,
                                                                         trackers = Trackers},
                                                handshake_env = #handshake_env{renegotiation = {false, first}},
                                                recv = #recv{from = StartFrom}
                                               } = State) ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Trackers, Socket, StartFrom, Alert, Role, StateName, Connection);

handle_normal_shutdown(Alert, StateName, #state{static_env = #static_env{role = Role,
                                                                         socket = Socket,
                                                                         transport_cb = Transport,
                                                                         protocol_cb = Connection,
                                                                         trackers = Trackers},
                                                connection_env  = #connection_env{user_application = {_Mon, Pid}},
                                                handshake_env = #handshake_env{renegotiation = Type},
                                                socket_options = Opts,
                                                recv = #recv{from = RecvFrom}
                                               } = State) ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Trackers, Socket, Type, Opts, Pid, RecvFrom, Alert, Role, StateName, Connection).

handle_alert(#alert{level = ?FATAL} = Alert, StateName, State) ->
    handle_fatal_alert(Alert, StateName, State);
handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert,
	     downgrade= StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, Alert}]};
handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert0,
             StateName, #state{static_env = #static_env{role = Role}} = State) ->
    Alert = Alert0#alert{role = opposite_role(Role)},
    handle_normal_shutdown(Alert, StateName, State),
    {stop,{shutdown, peer_close}, State};
handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName,
	     #state{static_env = #static_env{role = server = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {false, first}},
                    ssl_options = #{log_level := LogLevel}
		   } = State) when StateName == intial_hello;
                                   StateName == hello;
                                   StateName == certify;
                                   StateName == abbreviated;
                                   StateName == cipher ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    OwnAlert = ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, unexpected_renegotiate_alert_during_initial_handshake),
    handle_own_alert(OwnAlert, StateName, State);
handle_alert(#alert{} = Alert, StateName,
	     #state{static_env = #static_env{role = server = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {false, first}},
                    ssl_options = #{log_level := LogLevel}} = State) when StateName == start;
                                                                          StateName == intial_hello;
                                                                          StateName == hello ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    OwnAlert = ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, unexpected_alert),
    handle_own_alert(OwnAlert, StateName, State);
handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert0, StateName,
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {true, internal}},
                    ssl_options = #{log_level := LogLevel}} = State) ->
    Alert = Alert0#alert{role = opposite_role(Role)},
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert),
    handle_normal_shutdown(Alert, StateName, State),
    {stop,{shutdown, peer_close}, State};
handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, connection = StateName,
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {true, From}} = HsEnv,
                    ssl_options = #{log_level := LogLevel}
		   } = State0) ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    gen_statem:reply(From, {error, renegotiation_rejected}),
    State = Connection:reinit_handshake_data(State0),
    Connection:next_event(connection, no_record,
                          State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}});
handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName,
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {true, From}} = HsEnv,
                    ssl_options = #{log_level := LogLevel}
                   } = State0) ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    gen_statem:reply(From, {error, renegotiation_rejected}),
    %% Go back to connection!
    State = Connection:reinit(State0#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}}),
    Connection:next_event(connection, no_record, State);
handle_alert(#alert{level = ?WARNING, description = ?USER_CANCELED} = Alert, StateName,
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    ssl_options = #{log_level := LogLevel}} = State) when StateName =/= connection ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName,
              Alert#alert{role = opposite_role(Role)}),
    %% Wait for close alert that should follow or handshake timeout
    Connection:next_event(StateName, no_record, State);
%% Gracefully log and ignore all other warning alerts pre TLS-1.3
handle_alert(#alert{level = ?WARNING} = Alert, StateName,
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    connection_env = #connection_env{negotiated_version = Version},
                    ssl_options = #{log_level := LogLevel}} = State) when ?TLS_LT(Version, ?TLS_1_3) ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName,
              Alert#alert{role = opposite_role(Role)}),
    Connection:next_event(StateName, no_record, State);
handle_alert(Alert, StateName, State) ->
    %% In TLS-1.3 all error alerts are fatal not matter of legacy level
    %% but keep the level for the log so that users looking at what is
    %% sent and what is logged are not confused! Or if some one sends
    %% user cancel alert in connection which is inappropriate!
    handle_fatal_alert(Alert, StateName, State).

handle_fatal_alert(Alert0, StateName,
                   #state{static_env = #static_env{role = Role,
                                                   socket = Socket,
                                                   host = Host,
                                                   port = Port,
                                                   trackers = Trackers,
                                                   transport_cb = Transport,
                                                   protocol_cb = Connection},
                          connection_env  = #connection_env{user_application = {_Mon, Pid}},
                          ssl_options = #{log_level := LogLevel},
                          recv = #recv{from = From},
                          session = Session,
                          socket_options = Opts} = State) ->
    invalidate_session(Role, Host, Port, Session),
    Alert = Alert0#alert{role = opposite_role(Role)},
    log_alert(LogLevel, Role, Connection:protocol_name(),
              StateName, Alert),
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Trackers, Socket, StateName, Opts, Pid, From, Alert, Role, StateName, Connection),
    {stop, {shutdown, normal}, State}.

handle_trusted_certs_db(#state{ssl_options =#{cacerts := []} = Opts})
  when not is_map_key(cacertfile, Opts) ->
    %% No trusted certs specified
    ok;
handle_trusted_certs_db(#state{static_env = #static_env{cert_db_ref = Ref,
                                                        cert_db = CertDb},
                               ssl_options = Opts})
  when CertDb =/= undefined, not is_map_key(cacertfile, Opts) ->
    %% Certs provided as DER directly can not be shared
    %% with other connections and it is safe to delete them when the connection ends.
    ssl_pkix_db:remove_trusted_certs(Ref, CertDb);
handle_trusted_certs_db(#state{static_env = #static_env{file_ref_db = undefined}}) ->
    %% Something went wrong early (typically cacertfile does not
    %% exist) so there is nothing to handle
    ok;
handle_trusted_certs_db(#state{static_env = #static_env{cert_db_ref = Ref,
                                                        file_ref_db = RefDb},
			       ssl_options = #{cacertfile := File}}) ->
    case ssl_pkix_db:ref_count(Ref, RefDb, -1) of
	0 ->
	    ssl_manager:clean_cert_db(Ref, File);
	_ ->
	    ok
    end.

maybe_invalidate_session(?TLS_1_3,_, _, _, _, _) ->
    ok;
maybe_invalidate_session(Version, Type, Role, Host, Port, Session) when ?TLS_LT(Version, ?TLS_1_3) ->
    maybe_invalidate_session(Type, Role, Host, Port, Session).

maybe_invalidate_session({false, first}, server = Role, Host, Port, Session) ->
    invalidate_session(Role, Host, Port, Session);
maybe_invalidate_session(_, _, _, _, _) ->
    ok.

terminate({shutdown, ktls}, connection, State) ->
    %% Socket shall not be closed as it should be returned to user
    handle_trusted_certs_db(State);
terminate({shutdown, downgrade}, downgrade, State) ->
    %% Socket shall not be closed as it should be returned to user
    handle_trusted_certs_db(State);
terminate(_, _, #state{connection_env = #connection_env{socket_terminated = true}}) ->
    %% Happens when user closes the connection using ssl:close/1 e.i. terminate
    %% is called explicitly beforehand to to guarantee that Transport:close has been called
    %% when ssl:close/1 returns. Or when using ssl:shutdown/2 with read_write
    ok;
terminate({shutdown, transport_closed} = Reason,
	  _StateName, #state{static_env = #static_env{protocol_cb = Connection,
                                                      socket = Socket,
                                                      transport_cb = Transport}} = State) ->
    handle_trusted_certs_db(State),
    Connection:close(Reason, Socket, Transport, undefined);
terminate({shutdown, own_alert}, _StateName, #state{
						static_env = #static_env{protocol_cb = Connection,
                                                                         socket = Socket,
                                                                         transport_cb = Transport}} = State) ->
    handle_trusted_certs_db(State),
    case application:get_env(ssl, alert_timeout) of
	{ok, Timeout} when is_integer(Timeout) ->
	    Connection:close({close, Timeout}, Socket, Transport, undefined);
	_ ->
	    Connection:close({close, ?DEFAULT_TIMEOUT}, Socket, Transport, undefined)
    end;
terminate(Reason, connection, #state{static_env = #static_env{
                                                     protocol_cb = Connection,
                                                     transport_cb = Transport,
                                                     socket = Socket},
                                     connection_states = ConnectionStates
                                    } = State) ->

    handle_trusted_certs_db(State),
    Alert = terminate_alert(Reason),
    %% Send the termination ALERT if possible
    catch Connection:send_alert_in_connection(Alert, State),
    Connection:close({close, ?DEFAULT_TIMEOUT}, Socket, Transport, ConnectionStates);
terminate(Reason, _StateName, #state{static_env = #static_env{transport_cb = Transport,
                                                              protocol_cb = Connection,
                                                              socket = Socket}
				    } = State) ->
    handle_trusted_certs_db(State),
    Connection:close(Reason, Socket, Transport, undefined).

%%====================================================================
%% Log handling
%%====================================================================
format_status(normal, [_, StateName, State]) ->
    [{data, [{"State", {StateName, State}}]}];
format_status(terminate, [_, StateName, State]) ->
    SslOptions = (State#state.ssl_options),
    NewOptions = SslOptions#{
                             certs_keys => ?SECRET_PRINTOUT,
                             cacerts => ?SECRET_PRINTOUT,
                             dh => ?SECRET_PRINTOUT,
                             psk_identity => ?SECRET_PRINTOUT,
                             srp_identity => ?SECRET_PRINTOUT},
    [{data, [{"State", {StateName, State#state{connection_states = ?SECRET_PRINTOUT,
					       protocol_buffers =  ?SECRET_PRINTOUT,
					       user_data_buffer = ?SECRET_PRINTOUT,
					       handshake_env =  ?SECRET_PRINTOUT,
                                               connection_env = ?SECRET_PRINTOUT,
					       session =  ?SECRET_PRINTOUT,
					       ssl_options = NewOptions}
		       }}]}].
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(FsmPid, Event) ->
    try gen_statem:call(FsmPid, Event)
    catch
	exit:{noproc, _} ->
	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{shutdown,_} ->
	    {error, closed};
	exit:{{shutdown, _},_} ->
	    {error, closed}
    end.

check_hostname("") ->
    ?ALERT_REC(?FATAL, ?UNRECOGNIZED_NAME, empty_sni);
check_hostname(Hostname) ->
    case lists:reverse(Hostname) of
        [$.|_] ->
            ?ALERT_REC(?FATAL, ?UNRECOGNIZED_NAME, {sni_included_trailing_dot, Hostname});
        _ ->
            ok
    end.

handle_sni_hostname(Hostname,
                    #state{static_env = InitStatEnv0,
                           handshake_env = HsEnv,
                           connection_env = CEnv,
                           ssl_options = Opts} = State0) ->
    case update_ssl_options_from_sni(Opts, Hostname) of
        undefined ->
            %% RFC6060:  "If the server understood the ClientHello extension but
            %%  does not recognize the server name, the server SHOULD take one of two
            %%  actions: either abort the handshake by sending a fatal-level
            %%  unrecognized_name(112) alert or continue the handshake."
            State0#state{handshake_env = HsEnv#handshake_env{sni_hostname = Hostname}};
        NewOptions ->
	    {ok, #{cert_db_ref := Ref,
                   cert_db_handle := CertDbHandle,
                   fileref_db_handle := FileRefHandle,
                   session_cache := CacheHandle,
                   crl_db_info := CRLDbHandle,
                   cert_key_alts := CertKeyAlts,
                   dh_params := DHParams}} =
                ssl_config:init(NewOptions, server),
            State0#state{
              static_env = InitStatEnv0#static_env{
                             file_ref_db = FileRefHandle,
                             cert_db_ref = Ref,
                             cert_db = CertDbHandle,
                             crl_db = CRLDbHandle,
                             session_cache = CacheHandle
                            },
              connection_env = CEnv#connection_env{cert_key_alts = CertKeyAlts},
              ssl_options = NewOptions,
              handshake_env = HsEnv#handshake_env{sni_hostname = Hostname,
                                                  sni_guided_cert_selection = true,
                                                  diffie_hellman_params = DHParams}
             }
    end.

update_ssl_options_from_sni(#{sni_fun := SNIFun} = OrigSSLOptions, SNIHostname) ->
    case SNIFun(SNIHostname) of
        undefined ->
            undefined;
        SSLOptions ->
            VersionsOpt = proplists:get_value(versions, SSLOptions, []),
            FallBackOptions = filter_for_versions(VersionsOpt, OrigSSLOptions),
            ssl:update_options(SSLOptions, server, FallBackOptions)
    end.

filter_for_versions([], OrigSSLOptions) ->
    OrigSSLOptions;
filter_for_versions(['tlsv1.3'], OrigSSLOptions) ->
    Opts = ?'PRE_TLS-1_3_ONLY_OPTIONS' ++ ?'TLS-1_0_ONLY_OPTIONS',
    maps:without(Opts, OrigSSLOptions);
filter_for_versions(['tlsv1.3', 'tlsv1.2'| Rest], OrigSSLOptions) ->
    maybe_exclude_tlsv1(Rest, OrigSSLOptions);
filter_for_versions(['tlsv1.2'], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS' ++ ?'TLS-1_0_ONLY_OPTIONS',
    maps:without(Opts, OrigSSLOptions);
filter_for_versions(['tlsv1.2' | Rest], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS',
    maybe_exclude_tlsv1(Rest, maps:without(Opts, OrigSSLOptions));
filter_for_versions(['tlsv1.1'], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS' ++ ?'FROM_TLS-1_2_ONLY_OPTIONS'++ ?'TLS-1_0_ONLY_OPTIONS',
    maps:without(Opts, OrigSSLOptions);
filter_for_versions(['tlsv1.1'| Rest], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS' ++ ?'FROM_TLS-1_2_ONLY_OPTIONS',
    maybe_exclude_tlsv1(Rest, maps:without(Opts, OrigSSLOptions));
filter_for_versions(['tlsv1'], OrigSSLOptions) ->
    OrigSSLOptions.

maybe_exclude_tlsv1(Versions, Options) ->
    case lists:member('tlsv1', Versions) of
        false ->
            Opts = ?'TLS-1_0_ONLY_OPTIONS',
            maps:without(Opts, Options);
        true ->
            Options
    end.

ack_connection(#state{handshake_env = #handshake_env{renegotiation = {true, Initiater}} =
                          HsEnv} = State) when Initiater == peer;
                                               Initiater == internal ->
    State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}};
ack_connection(#state{handshake_env = #handshake_env{renegotiation = {true, From}} = HsEnv} = State) ->
    gen_statem:reply(From, ok),
    State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}};
ack_connection(#state{handshake_env = #handshake_env{renegotiation = {false, first}} = HsEnv,
                      recv = #recv{from = StartFrom} = Recv} = State)
  when StartFrom =/= undefined ->
    gen_statem:reply(StartFrom, connected),
    State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined},
                recv = Recv#recv{from = undefined}};
ack_connection(State) ->
    State.

no_records(Extensions) ->
    maps:map(fun(_, Value) ->
                     ssl_handshake:extension_value(Value)
             end, Extensions).

handle_active_option(false, connection = StateName, To, Reply, State) ->
    gen_statem:reply(To, Reply),
    hibernate_after(StateName, State, []);

handle_active_option(_, connection = StateName, To, Reply,
                     #state{static_env = #static_env{role = Role},
                            connection_env = #connection_env{socket_tls_closed = true},
                            user_data_buffer = {_,0,_}} = State) ->
    Alert = ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, all_data_delivered),
    handle_normal_shutdown(Alert#alert{role = Role}, StateName, State),
    {stop_and_reply,{shutdown, peer_close}, [{reply, To, Reply}]};
handle_active_option(_, connection = StateName, To, _Reply,
                     #state{static_env = #static_env{role = Role,
                                                     socket = Socket,
                                                     trackers = Trackers,
                                                     transport_cb = Transport,
                                                     protocol_cb = Connection},
                            connection_env =
                                #connection_env{socket_tls_closed = Alert = #alert{}},
                            user_data_buffer = {_,0,_}} = State) ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Trackers, Socket, To, Alert, Role, StateName, Connection),
    {stop, {shutdown, normal}, State};
handle_active_option(_, connection = StateName0, To, Reply,
                     #state{static_env = #static_env{protocol_cb = Connection},
                            user_data_buffer = {_,0,_}} = State0) ->
    case Connection:next_event(StateName0, no_record, State0) of
	{next_state, StateName, State} ->
            gen_statem:reply(To, Reply),
	    hibernate_after(StateName, State, []);
	{next_state, StateName, State, Actions} ->
            gen_statem:reply(To, Reply),
	    hibernate_after(StateName, State, Actions);
	{stop, _, _} = Stop ->
	    Stop
    end;
handle_active_option(_, StateName, To, Reply, #state{user_data_buffer = {_,0,_}} = State) ->
    %% Active once already set
    gen_statem:reply(To, Reply),
    {next_state, StateName, State};

%% user_data_buffer nonempty
handle_active_option(_, StateName0, To, Reply,
                     #state{static_env = #static_env{protocol_cb = Connection}} = State0) ->
    case read_application_data(<<>>, State0) of
	{stop, _, _} = Stop ->
	    Stop;
	{Record, State1} ->
	    %% Note: Renogotiation may cause StateName0 =/= StateName
	    case Connection:next_event(StateName0, Record, State1) of
		{next_state, StateName, State} ->
                    gen_statem:reply(To, Reply),
		    hibernate_after(StateName, State, []);
		{next_state, StateName, State, Actions} ->
                    gen_statem:reply(To, Reply),
		    hibernate_after(StateName, State, Actions);
		{stop, _, _} = Stop ->
		    Stop
	    end
    end.

read_application_data(#state{socket_options = SocketOpts,
                             recv = #recv{from = RecvFrom,
                                          bytes_to_read = BytesToRead}} = State,
                      Front, BufferSize, Rear) ->
    read_application_data(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead).

%% Pick binary from queue front, if empty wait for more data
read_application_data(State, [Bin|Front], BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead) ->
    read_application_data_bin(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead, Bin);
read_application_data(State, [] = Front, BufferSize, [] = Rear, SocketOpts, RecvFrom, BytesToRead) ->
    0 = BufferSize, % Assert
    {no_record, State#state{socket_options = SocketOpts,
                            recv = State#state.recv#recv{from = RecvFrom,
                                                         bytes_to_read = BytesToRead
                                                        },
                            user_data_buffer = {Front,BufferSize,Rear}}};
read_application_data(State, [], BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead) ->
    [Bin|Front] = lists:reverse(Rear),
    read_application_data_bin(State, Front, BufferSize, [], SocketOpts, RecvFrom, BytesToRead, Bin).

read_application_data_bin(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead, <<>>) ->
    %% Done with this binary - get next
    read_application_data(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead);
read_application_data_bin(State, Front0, BufferSize0, Rear0, SocketOpts0, RecvFrom, BytesToRead, Bin0) ->
    %% Decode one packet from a binary
    case get_data(SocketOpts0, BytesToRead, Bin0) of
	{ok, Data, Bin} -> % Send data
            BufferSize = BufferSize0 - (byte_size(Bin0) - byte_size(Bin)),
            read_application_data_deliver(
              State, [Bin|Front0], BufferSize, Rear0, SocketOpts0, RecvFrom, Data);
        {more, undefined} ->
            %% We need more data, do not know how much
            if
                byte_size(Bin0) < BufferSize0 ->
                    %% We have more data in the buffer besides the first binary - concatenate all and retry
                    Bin = iolist_to_binary([Bin0,Front0|lists:reverse(Rear0)]),
                    read_application_data_bin(
                      State, [], BufferSize0, [], SocketOpts0, RecvFrom, BytesToRead, Bin);
                true ->
                    %% All data is in the first binary, no use to retry - wait for more
                    {no_record, State#state{socket_options = SocketOpts0,
                                            recv = State#state.recv#recv{from = RecvFrom,
                                                                         bytes_to_read = BytesToRead},
                                            user_data_buffer = {[Bin0|Front0],BufferSize0,Rear0}}}
            end;
        {more, Size} when Size =< BufferSize0 ->
            %% We have a packet in the buffer - collect it in a binary and decode
            {Data,Front,Rear} = iovec_from_front(Size - byte_size(Bin0), Front0, Rear0, [Bin0]),
            Bin = iolist_to_binary(Data),
            read_application_data_bin(
              State, Front, BufferSize0, Rear, SocketOpts0, RecvFrom, BytesToRead, Bin);
        {more, _Size} ->
            %% We do not have a packet in the buffer - wait for more
            {no_record, State#state{socket_options = SocketOpts0,
                                    recv = State#state.recv#recv{from = RecvFrom,
                                                                bytes_to_read = BytesToRead},
                                    user_data_buffer = {[Bin0|Front0],BufferSize0,Rear0}}};
        passive ->
            {no_record, State#state{socket_options = SocketOpts0,
                                    recv = State#state.recv#recv{from = RecvFrom,
                                                                 bytes_to_read = BytesToRead},
                                    user_data_buffer = {[Bin0|Front0],BufferSize0,Rear0}}};
	{error,_Reason} ->
            %% Invalid packet in packet mode
            #state{
               static_env =
                   #static_env{
                      socket = Socket,
                      protocol_cb = Connection,
                      transport_cb = Transport,
                      trackers = Trackers},
               connection_env =
                   #connection_env{user_application = {_Mon, Pid}}} = State,
            Buffer = iolist_to_binary([Bin0,Front0|lists:reverse(Rear0)]),
	    deliver_packet_error(
              Connection:pids(State), Transport, Socket, SocketOpts0,
              Buffer, Pid, RecvFrom, Trackers, Connection),
            {stop, {shutdown, normal},
             State#state{socket_options = SocketOpts0,
                         recv = State#state.recv#recv{from = RecvFrom,
                                                      bytes_to_read = BytesToRead},
                         user_data_buffer = {[Buffer],BufferSize0,[]}}}
    end.

read_application_data_deliver(State, Front, BufferSize, Rear, SocketOpts0, RecvFrom, Data) ->
    #state{
       static_env =
           #static_env{
              socket = Socket,
              protocol_cb = Connection,
              transport_cb = Transport,
              trackers = Trackers},
       connection_env =
           #connection_env{user_application = {_Mon, Pid}}} = State,
    SocketOpts =
        deliver_app_data(
          Connection:pids(State), Transport, Socket, SocketOpts0, Data, Pid, RecvFrom, Trackers, Connection),
    if
        SocketOpts#socket_options.active =:= false ->
            %% Passive mode, wait for active once or recv
            {no_record,
             State#state{
               user_data_buffer = {Front,BufferSize,Rear},
               recv = State#state.recv#recv{from = undefined, bytes_to_read = undefined},
               socket_options = SocketOpts
              }};
        true -> %% Try to deliver more data
            %% Process early data if it is accepted.
            case (State#state.handshake_env)#handshake_env.early_data_accepted of
                false ->
                    read_application_data(State, Front, BufferSize, Rear, SocketOpts, undefined, undefined);
                true ->
                    read_application_data(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, undefined)
            end
    end.


read_application_dist_data(DHandle, [Bin|Front], BufferSize, Rear) ->
    read_application_dist_data(DHandle, Front, BufferSize, Rear, Bin);
read_application_dist_data(_DHandle, [] = Front, BufferSize, [] = Rear) ->
    BufferSize = 0,
    {Front,BufferSize,Rear};
read_application_dist_data(DHandle, [], BufferSize, Rear) ->
    [Bin|Front] = lists:reverse(Rear),
    read_application_dist_data(DHandle, Front, BufferSize, [], Bin).
%%
read_application_dist_data(DHandle, Front0, BufferSize, Rear0, Bin0) ->
    case Bin0 of
        %%
        %% START Optimization
        %% It is cheaper to match out several packets in one match operation than to loop for each
        <<SizeA:32, DataA:SizeA/binary,
          SizeB:32, DataB:SizeB/binary,
          SizeC:32, DataC:SizeC/binary,
          SizeD:32, DataD:SizeD/binary, Rest/binary>>
          when 0 < SizeA, 0 < SizeB, 0 < SizeC, 0 < SizeD ->
            %% We have 4 complete packets in the first binary
            erlang:dist_ctrl_put_data(DHandle, DataA),
            erlang:dist_ctrl_put_data(DHandle, DataB),
            erlang:dist_ctrl_put_data(DHandle, DataC),
            erlang:dist_ctrl_put_data(DHandle, DataD),
            read_application_dist_data(
              DHandle, Front0, BufferSize - (4*4+SizeA+SizeB+SizeC+SizeD), Rear0, Rest);
        <<SizeA:32, DataA:SizeA/binary,
          SizeB:32, DataB:SizeB/binary,
          SizeC:32, DataC:SizeC/binary, Rest/binary>>
          when 0 < SizeA, 0 < SizeB, 0 < SizeC ->
            %% We have 3 complete packets in the first binary
            erlang:dist_ctrl_put_data(DHandle, DataA),
            erlang:dist_ctrl_put_data(DHandle, DataB),
            erlang:dist_ctrl_put_data(DHandle, DataC),
            read_application_dist_data(
              DHandle, Front0, BufferSize - (3*4+SizeA+SizeB+SizeC), Rear0, Rest);
        <<SizeA:32, DataA:SizeA/binary,
          SizeB:32, DataB:SizeB/binary, Rest/binary>>
          when 0 < SizeA, 0 < SizeB ->
            %% We have 2 complete packets in the first binary
            erlang:dist_ctrl_put_data(DHandle, DataA),
            erlang:dist_ctrl_put_data(DHandle, DataB),
            read_application_dist_data(
              DHandle, Front0, BufferSize - (2*4+SizeA+SizeB), Rear0, Rest);
        %% END Optimization
        %%
        %% Basic one packet code path
        <<Size:32, Data:Size/binary, Rest/binary>> ->
            %% We have a complete packet in the first binary
            0 < Size andalso erlang:dist_ctrl_put_data(DHandle, Data),
            read_application_dist_data(DHandle, Front0, BufferSize - (4+Size), Rear0, Rest);
        <<Size:32, FirstData/binary>> when 4+Size =< BufferSize ->
            %% We have a complete packet in the buffer
            %% - fetch the missing content from the buffer front
            {Data,Front,Rear} = iovec_from_front(Size - byte_size(FirstData), Front0, Rear0, [FirstData]),
            0 < Size andalso erlang:dist_ctrl_put_data(DHandle, Data),
            read_application_dist_data(DHandle, Front, BufferSize - (4+Size), Rear);
        <<Bin/binary>> ->
            %% In OTP-21 the match context reuse optimization fails if we use Bin0 in recursion, so here we
            %% match out the whole binary which will trick the optimization into keeping the match context
            %% for the first binary contains complete packet code above
            case Bin of
                <<_Size:32, _InsufficientData/binary>> ->
                    %% We have a length field in the first binary but there is not enough data
                    %% in the buffer to form a complete packet - await more data
                    {[Bin|Front0],BufferSize,Rear0};
                <<IncompleteLengthField/binary>> when 4 < BufferSize ->
                    %% We do not have a length field in the first binary but the buffer
                    %% contains enough data to maybe form a packet
                    %% - fetch a tiny binary from the buffer front to complete the length field
                    {LengthField,Front,Rear} =
                        case IncompleteLengthField of
                            <<>> ->
                                iovec_from_front(4, Front0, Rear0, []);
                            _ ->
                                iovec_from_front(
                                  4 - byte_size(IncompleteLengthField), Front0, Rear0, [IncompleteLengthField])
                        end,
                    LengthBin = iolist_to_binary(LengthField),
                    read_application_dist_data(DHandle, Front, BufferSize, Rear, LengthBin);
                <<IncompleteLengthField/binary>> ->
                    %% We do not have enough data in the buffer to even form a length field - await more data
                    case IncompleteLengthField of
                        <<>> ->
                            {Front0,BufferSize,Rear0};
                        _ ->
                            {[IncompleteLengthField|Front0],BufferSize,Rear0}
                    end
            end
    end.

iovec_from_front(0, Front, Rear, Acc) ->
    {lists:reverse(Acc),Front,Rear};
iovec_from_front(Size, [], Rear, Acc) ->
    case Rear of
        %% Avoid lists:reverse/1 for simple cases.
        %% Case clause for [] to avoid infinite loop.
        [_] ->
            iovec_from_front(Size, Rear, [], Acc);
        [Bin2,Bin1] ->
            iovec_from_front(Size, [Bin1,Bin2], [], Acc);
        [Bin3,Bin2,Bin1] ->
            iovec_from_front(Size, [Bin1,Bin2,Bin3], [], Acc);
        [_,_,_|_] = Rear ->
            iovec_from_front(Size, lists:reverse(Rear), [], Acc)
    end;
iovec_from_front(Size, [Bin|Front], Rear, []) ->
    case Bin of
        <<Last:Size/binary>> -> % Just enough
            {[Last],Front,Rear};
        <<Last:Size/binary, Rest/binary>> -> % More than enough, split here
            {[Last],[Rest|Front],Rear};
        <<>> -> % Not enough, skip empty binaries
            iovec_from_front(Size, Front, Rear, []);
        <<_/binary>> -> % Not enough
            BinSize = byte_size(Bin),
            iovec_from_front(Size - BinSize, Front, Rear, [Bin])
    end;
iovec_from_front(Size, [Bin|Front], Rear, Acc) ->
    case Bin of
        <<Last:Size/binary>> -> % Just enough
            {lists:reverse(Acc, [Last]),Front,Rear};
        <<Last:Size/binary, Rest/binary>> -> % More than enough, split here
            {lists:reverse(Acc, [Last]),[Rest|Front],Rear};
        <<>> -> % Not enough, skip empty binaries
            iovec_from_front(Size, Front, Rear, Acc);
        <<_/binary>> -> % Not enough
            BinSize = byte_size(Bin),
            iovec_from_front(Size - BinSize, Front, Rear, [Bin|Acc])
    end.
%% Picks ClientData
get_data(#socket_options{active=false}, undefined, _Bin) ->
    %% Recv timed out save buffer data until next recv
    passive;
get_data(#socket_options{active=Active, packet=Raw}, BytesToRead, Bin)
  when Raw =:= raw; Raw =:= 0 ->   %% Raw Mode
    case Bin of
        <<_/binary>> when Active =/= false orelse BytesToRead =:= 0 ->
	    %% Active true or once, or passive mode recv(0)
	    {ok, Bin, <<>>};
        <<Data:BytesToRead/binary, Rest/binary>> ->
	    %% Passive Mode, recv(Bytes)
            {ok, Data, Rest};
        <<_/binary>> ->
	    %% Passive Mode not enough data
            {more, BytesToRead}
    end;
get_data(#socket_options{packet=Type, packet_size=Size}, _, Bin) ->
    PacketOpts = [{packet_size, Size}],
    decode_packet(Type, Bin, PacketOpts).

decode_packet({http, headers}, Buffer, PacketOpts) ->
    decode_packet(httph, Buffer, PacketOpts);
decode_packet({http_bin, headers}, Buffer, PacketOpts) ->
    decode_packet(httph_bin, Buffer, PacketOpts);
decode_packet(Type, Buffer, PacketOpts) ->
    erlang:decode_packet(Type, Buffer, PacketOpts).

%% Just like with gen_tcp sockets, an ssl socket that has been configured with
%% {packet, http} (or {packet, http_bin}) will automatically switch to expect
%% HTTP headers after it sees a HTTP Request or HTTP Response line. We
%% represent the current state as follows:
%%    #socket_options.packet =:= http: Expect a HTTP Request/Response line
%%    #socket_options.packet =:= {http, headers}: Expect HTTP Headers
%% Note that if the user has explicitly configured the socket to expect
%% HTTP headers using the {packet, httph} option, we don't do any automatic
%% switching of states.
deliver_app_data(CPids, Transport, Socket,
                 #socket_options{active=Active, packet=Type} = SOpts,
                 Data, Pid, From, Trackers, Connection) ->
    send_or_reply(Active, Pid, From,
                  format_reply(CPids, Transport, Socket,
                               SOpts, Data, Trackers, Connection)),
    SO =
        case Data of
            {P, _, _, _}
              when ((P =:= http_request) or (P =:= http_response)),
                   ((Type =:= http) or (Type =:= http_bin)) ->
                SOpts#socket_options{packet={Type, headers}};
            http_eoh when tuple_size(Type) =:= 2 ->
                %% End of headers - expect another Request/Response line
                {Type1, headers} = Type,
                SOpts#socket_options{packet=Type1};
            _ ->
                SOpts
        end,
    case Active of
        once ->
            SO#socket_options{active=false};
        1 ->
            send_user(Pid,
                      format_passive(CPids, Transport,
                                     Socket, Trackers, Connection)),
            SO#socket_options{active=false};
        N when is_integer(N) ->
            SO#socket_options{active=N - 1};
	_ ->
	    SO
    end.

format_reply(_, _, _,#socket_options{active = false, mode = Mode, packet = Packet,
				  header = Header}, Data, _, _) ->
    {ok, do_format_reply(Mode, Packet, Header, Data)};
format_reply(CPids, Transport, Socket, #socket_options{active = _, mode = Mode, packet = Packet,
						header = Header}, Data, Trackers, Connection) ->
    {ssl, Connection:socket(CPids, Transport, Socket, Trackers),
     do_format_reply(Mode, Packet, Header, Data)}.

deliver_packet_error(CPids, Transport, Socket,
                     SO= #socket_options{active = Active}, Data, Pid, From, Trackers, Connection) ->
    send_or_reply(Active, Pid, From, format_packet_error(CPids,
                                                         Transport, Socket, SO, Data, Trackers, Connection)).

format_packet_error(_, _, _,#socket_options{active = false, mode = Mode}, Data, _, _) ->
    {error, {invalid_packet, do_format_reply(Mode, raw, 0, Data)}};
format_packet_error(CPids, Transport, Socket, #socket_options{active = _, mode = Mode},
                    Data, Trackers, Connection) ->
    {ssl_error, Connection:socket(CPids, Transport, Socket, Trackers),
     {invalid_packet, do_format_reply(Mode, raw, 0, Data)}}.

do_format_reply(binary, _, N, Data) when N > 0 ->  % Header mode
    header(N, Data);
do_format_reply(binary, _, _, Data) ->
    Data;
do_format_reply(list, Packet, _, Data)
  when Packet == http; Packet == {http, headers};
       Packet == http_bin; Packet == {http_bin, headers};
       Packet == httph; Packet == httph_bin ->
    Data;
do_format_reply(list, _,_, Data) ->
    binary_to_list(Data).

format_passive(CPids, Transport, Socket, Trackers, Connection) ->
    {ssl_passive, Connection:socket(CPids, Transport, Socket, Trackers)}.

header(0, <<>>) ->
    <<>>;
header(_, <<>>) ->
    [];
header(0, Binary) ->
    Binary;
header(N, Binary) ->
    <<?BYTE(ByteN), NewBinary/binary>> = Binary,
    [ByteN | header(N-1, NewBinary)].

send_or_reply(false, _Pid, From, Data) when From =/= undefined ->
    gen_statem:reply(From, Data);
send_or_reply(false, Pid, undefined, _) when is_pid(Pid) ->
    ok;
send_or_reply(_, no_pid, _, _) ->
    ok;
send_or_reply(_, Pid, _, Data) ->
    send_user(Pid, Data).

send_user(Pid, Msg) ->
    Pid ! Msg,
    ok.

alert_user(Pids, Transport, Trackers, Socket, _, Opts, Pid, From, Alert, Role, connection = StateName, Connection) ->
    alert_user(Pids, Transport, Trackers, Socket, Opts#socket_options.active, Pid, From, Alert, Role, StateName, Connection);
alert_user(Pids, Transport, Trackers, Socket, {true, internal}, Opts, Pid, From, Alert, Role, StateName, Connection) ->
    alert_user(Pids, Transport, Trackers, Socket, Opts#socket_options.active, Pid, From, Alert, Role, StateName, Connection);
alert_user(Pids, Transport, Trackers, Socket, _, _, _, From, Alert, Role, StateName, Connection) ->
    alert_user(Pids, Transport, Trackers, Socket, From, Alert, Role, StateName, Connection).

alert_user(Pids, Transport, Trackers, Socket, From, Alert, Role, StateName, Connection) ->
    alert_user(Pids, Transport, Trackers, Socket, false, no_pid, From, Alert, Role, StateName, Connection).

alert_user(_, _, _, _, false = Active, Pid, From,  Alert, Role, StateName, Connection) when From =/= undefined ->
    %% If there is an outstanding handshake | recv
    %% From will be defined and send_or_reply will
    %% send the appropriate error message.
    ReasonCode = ssl_alert:reason_code(Alert, Role, Connection:protocol_name(), StateName),
    send_or_reply(Active, Pid, From, {error, ReasonCode});
alert_user(Pids, Transport, Trackers, Socket, Active, Pid, From, Alert, Role, StateName, Connection) ->
    case ssl_alert:reason_code(Alert, Role, Connection:protocol_name(), StateName) of
	closed ->
	    send_or_reply(Active, Pid, From,
			  {ssl_closed, Connection:socket(Pids, Transport, Socket, Trackers)});
	ReasonCode ->
	    send_or_reply(Active, Pid, From,
			  {ssl_error, Connection:socket(Pids, Transport, Socket, Trackers), ReasonCode})
    end.

log_alert(Level, Role, ProtocolName, StateName, #alert{role = Role} = Alert) ->
    ssl_logger:log(notice, Level, #{protocol => ProtocolName,
                                    role => Role,
                                    statename => StateName,
                                    alert => Alert,
                                    alerter => own}, Alert#alert.where);
log_alert(Level, Role, ProtocolName, StateName,  Alert) ->
    ssl_logger:log(notice, Level, #{protocol => ProtocolName,
                                    role => Role,
                                    statename => StateName,
                                    alert => Alert,
                                    alerter => peer}, Alert#alert.where).
terminate_alert(Reason) when Reason == normal;
                             Reason == shutdown;
                             Reason == close ->
    ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY);
terminate_alert({Reason, _}) ->
    terminate_alert(Reason);
terminate_alert(_) ->
    ?ALERT_REC(?FATAL, ?INTERNAL_ERROR).

invalidate_session(client, Host, Port, Session) ->
    ssl_manager:invalidate_session(Host, Port, Session);
invalidate_session(server, _, _, _) ->
    ok.

connection_info(#state{handshake_env = #handshake_env{sni_hostname = SNIHostname,
                                                      resumption = Resumption},
                       session = #session{session_id = SessionId,
                                          cipher_suite = CipherSuite,
                                          srp_username = SrpUsername,
                                          ecc = ECCCurve} = Session,
                       connection_states = ConnectionStates,
		       connection_env = #connection_env{negotiated_version =  {_,_} = Version},
		       ssl_options = #{protocol := Protocol} = Opts}) ->
    RecordCB = record_cb(Protocol),
    CipherSuiteDef = #{key_exchange := KexAlg} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    IsNamedCurveSuite = lists:member(KexAlg,
                                     [ecdh_ecdsa, ecdhe_ecdsa, ecdh_rsa, ecdhe_rsa, ecdh_anon]),
    CurveInfo = case ECCCurve of
		    {namedCurve, Curve} when IsNamedCurveSuite ->
			[{ecc, {named_curve, pubkey_cert_records:namedCurves(Curve)}}];
		    _ ->
			[]
		end,
    MFLInfo = case maps:get(max_fragment_length, ConnectionStates, undefined) of
                  MaxFragmentLength when is_integer(MaxFragmentLength) ->
                      [{max_fragment_length, MaxFragmentLength}];
                  _ ->
                      []
              end,
    [{protocol, RecordCB:protocol_version(Version)},
     {session_id, SessionId},
     {session_data, term_to_binary(Session)},
     {session_resumption, Resumption},
     {selected_cipher_suite, CipherSuiteDef},
     {sni_hostname, SNIHostname},
     {srp_username, SrpUsername} | CurveInfo] ++ MFLInfo ++ ssl_options_list(Opts).

security_info(#state{connection_states = ConnectionStates,
                     static_env = #static_env{role = Role},
                     ssl_options = Opts,
                     protocol_specific = ProtocolSpecific}) ->
    ReadState = ssl_record:current_connection_state(ConnectionStates, read),
    #{security_parameters :=
	  #security_parameters{client_random = ClientRand,
                               server_random = ServerRand,
                               master_secret = MasterSecret,
                               application_traffic_secret = AppTrafSecretRead,
                               client_early_data_secret = ServerEarlyData
                              }} = ReadState,
    BaseSecurityInfo = [{client_random, ClientRand}, {server_random, ServerRand}, {master_secret, MasterSecret}],

    KeepSecrets = maps:get(keep_secrets, Opts, false),
    if KeepSecrets =/= true ->
            BaseSecurityInfo;
       true ->
            #{security_parameters :=
                  #security_parameters{
                     application_traffic_secret = AppTrafSecretWrite0,
                     client_early_data_secret = ClientEarlyData}} =
                ssl_record:current_connection_state(ConnectionStates, write),
            Sender = maps:get(sender, ProtocolSpecific, undefined),
            AppTrafSecretWrite = {Sender, AppTrafSecretWrite0},
            if Role == server ->
                    if ServerEarlyData =/= undefined ->
                            [{server_traffic_secret_0, AppTrafSecretWrite},
                             {client_traffic_secret_0, AppTrafSecretRead},
                             {client_early_data_secret, ServerEarlyData}];
                       true ->
                            [{server_traffic_secret_0, AppTrafSecretWrite},
                             {client_traffic_secret_0, AppTrafSecretRead}]
                    end;
               true ->
                    if ClientEarlyData =/= undefined ->
                            [{client_traffic_secret_0, AppTrafSecretWrite},
                             {server_traffic_secret_0, AppTrafSecretRead},
                             {client_early_data_secret, ClientEarlyData}];
                       true ->
                            [{client_traffic_secret_0, AppTrafSecretWrite},
                             {server_traffic_secret_0, AppTrafSecretRead}]
                    end
            end ++
                case ReadState of
                    #{client_handshake_traffic_secret := ClientHSTrafficSecret,
                      server_handshake_traffic_secret := ServerHSTrafficSecret} ->
                        [{client_handshake_traffic_secret, ClientHSTrafficSecret},
                         {server_handshake_traffic_secret, ServerHSTrafficSecret}];
                   _ ->
                        []
                end ++ BaseSecurityInfo
    end.

record_cb(tls) ->
    tls_record;
record_cb(dtls) ->
    dtls_record.

get_socket_opts(_, _,_,[], _, Acc) ->
    {ok, Acc};
get_socket_opts(Connection, Transport, Socket, [mode | Tags], SockOpts, Acc) ->
    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts,
		    [{mode, SockOpts#socket_options.mode} | Acc]);
get_socket_opts(Connection, Transport, Socket, [packet | Tags], SockOpts, Acc) ->
    case SockOpts#socket_options.packet of
	{Type, headers} ->
	    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, [{packet, Type} | Acc]);
	Type ->
	    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, [{packet, Type} | Acc])
    end;
get_socket_opts(Connection, Transport, Socket, [header | Tags], SockOpts, Acc) ->
    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts,
		    [{header, SockOpts#socket_options.header} | Acc]);
get_socket_opts(Connection, Transport, Socket, [active | Tags], SockOpts, Acc) ->
    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts,
		    [{active, SockOpts#socket_options.active} | Acc]);
get_socket_opts(Connection, Transport, Socket, [packet_size | Tags], SockOpts, Acc) ->
    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts,
		    [{packet_size, SockOpts#socket_options.packet_size} | Acc]);
get_socket_opts(Connection, Transport, Socket, [Tag | Tags], SockOpts, Acc) ->
    case Connection:getopts(Transport, Socket, [Tag]) of
        {ok, [Opt]} ->
            get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, [Opt | Acc]);
        {ok, []} ->
            get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, Acc);
        {error, Reason} ->
            {error, {options, {socket_options, Tag, Reason}}}
    end;
get_socket_opts(_,_, _,Opts, _,_) ->
    {error, {options, {socket_options, Opts, function_clause}}}.

set_socket_opts(_,_,_, [], SockOpts, []) ->
    {ok, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [], SockOpts, Other) ->
    %% Set non emulated options
    try ConnectionCb:setopts(Transport, Socket, Other) of
	ok ->
	    {ok, SockOpts};
	{error, InetError} ->
	    {{error, {options, {socket_options, Other, InetError}}}, SockOpts}
    catch
	_:Error ->
	    %% So that inet behavior does not crash our process
	    {{error, {options, {socket_options, Other, Error}}}, SockOpts}
    end;

set_socket_opts(ConnectionCb, Transport, Socket, [{active, Active}| Opts], SockOpts, Other)
  when Active == once; Active == true; Active == false ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts,
		    SockOpts#socket_options{active = Active}, Other);
set_socket_opts(ConnectionCb, Transport, Socket, [{active, Active1} = Opt| Opts],
                SockOpts=#socket_options{active = Active0}, Other)
  when Active1 >= -32768, Active1 =< 32767 ->
    Active = if
                 is_integer(Active0), Active0 + Active1 < -32768 ->
                     error;
                 is_integer(Active0), Active0 + Active1 =< 0 ->
                     false;
                 is_integer(Active0), Active0 + Active1 > 32767 ->
                     error;
                 Active1 =< 0 ->
                     false;
                 is_integer(Active0) ->
                     Active0 + Active1;
                 true ->
                     Active1
             end,
    case Active of
        error ->
            {{error, {options, {socket_options, Opt}} }, SockOpts};
        _ ->
            set_socket_opts(ConnectionCb, Transport, Socket, Opts,
                            SockOpts#socket_options{active = Active}, Other)
    end;
set_socket_opts(_,_, _, [{active, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}} }, SockOpts};

set_socket_opts(ConnectionCb, Transport,Socket, [{mode, Mode}| Opts], SockOpts, Other)
  when Mode == list; Mode == binary ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts,
		    SockOpts#socket_options{mode = Mode}, Other);
set_socket_opts(_, _, _, [{mode, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(ConnectionCb, Transport,Socket, [{packet, Packet}| Opts], SockOpts, Other)
  when Packet == raw;
       Packet == 0;
       Packet == 1;
       Packet == 2;
       Packet == 4;
       Packet == asn1;
       Packet == cdr;
       Packet == sunrm;
       Packet == fcgi;
       Packet == tpkt;
       Packet == line;
       Packet == http;
       Packet == httph;
       Packet == http_bin;
       Packet == httph_bin ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts,
		    SockOpts#socket_options{packet = Packet}, Other);
set_socket_opts(_, _, _, [{packet, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [{header, Header}| Opts], SockOpts, Other)
  when is_integer(Header) ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts,
		    SockOpts#socket_options{header = Header}, Other);
set_socket_opts(_, _, _, [{header, _} = Opt| _], SockOpts, _) ->
    {{error,{options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(ConnectionCb, Transport,Socket, [{packet_size, Size}| Opts], SockOpts, Other) when is_integer(Size) -> 
      set_socket_opts(ConnectionCb, Transport, Socket, Opts,
                      SockOpts#socket_options{packet_size = Size}, Other);
set_socket_opts(_,_, _, [{packet_size, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}} }, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [Opt | Opts], SockOpts, Other) ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, SockOpts, [Opt | Other]).

ssl_options_list(SslOptions) ->
    L = maps:to_list(SslOptions),
    ssl_options_list(L, []).



ssl_options_list([], Acc) ->
    lists:reverse(Acc);
%% Skip internal options, only return user options
ssl_options_list([{protocol, _}| T], Acc) ->
    ssl_options_list(T, Acc);
ssl_options_list([{erl_dist, _}|T], Acc) ->
    ssl_options_list(T, Acc);
ssl_options_list([{renegotiate_at, _}|T], Acc) ->
    ssl_options_list(T, Acc);
ssl_options_list([{max_fragment_length, _}|T], Acc) ->
    %% skip max_fragment_length from options since it is taken above from connection_states
    ssl_options_list(T, Acc);
ssl_options_list([{ciphers = Key, Value}|T], Acc) ->
   ssl_options_list(T,
		    [{Key, lists:map(
			     fun(Suite) ->
				     ssl_cipher_format:suite_bin_to_map(Suite)
			     end, Value)}
		     | Acc]);
ssl_options_list([{Key, Value}|T], Acc) ->
   ssl_options_list(T, [{Key, Value} | Acc]).

%% Maybe add NSS keylog info according to
%% https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS/Key_Log_Format
maybe_add_keylog(Info) ->
    maybe_add_keylog(lists:keyfind(protocol, 1, Info), Info).

maybe_add_keylog({_, 'tlsv1.3'}, Info) ->
    try
        {client_random, ClientRandomBin} = lists:keyfind(client_random, 1, Info),
        %% after traffic key update current traffic secret
        %% is stored in tls_sender process state
        MaybeUpdateTrafficSecret =
            fun({Direction, {Sender, TrafficSecret0}}) ->
                    TrafficSecret =
                        case call(Sender, get_application_traffic_secret) of
                            {ok, SenderAppTrafSecretWrite} ->
                                SenderAppTrafSecretWrite;
                            _ ->
                                TrafficSecret0
                        end,
                    {Direction, TrafficSecret};
               (TrafficSecret0) ->
                    TrafficSecret0
            end,
        {client_traffic_secret_0, ClientTrafficSecret0Bin} =
            MaybeUpdateTrafficSecret(lists:keyfind(client_traffic_secret_0, 1, Info)),
        {server_traffic_secret_0, ServerTrafficSecret0Bin} =
            MaybeUpdateTrafficSecret(lists:keyfind(server_traffic_secret_0, 1, Info)),
        {client_handshake_traffic_secret, ClientHSecretBin} = lists:keyfind(client_handshake_traffic_secret, 1, Info),
        {server_handshake_traffic_secret, ServerHSecretBin} = lists:keyfind(server_handshake_traffic_secret, 1, Info),
        {selected_cipher_suite, #{prf := Prf}} = lists:keyfind(selected_cipher_suite, 1, Info),
        ClientRandom = binary:decode_unsigned(ClientRandomBin),
        ClientTrafficSecret0 = keylog_secret(ClientTrafficSecret0Bin, Prf),
        ServerTrafficSecret0 = keylog_secret(ServerTrafficSecret0Bin, Prf),
        ClientHSecret = keylog_secret(ClientHSecretBin, Prf),
        ServerHSecret = keylog_secret(ServerHSecretBin, Prf),
        Keylog0 = [io_lib:format("CLIENT_HANDSHAKE_TRAFFIC_SECRET ~64.16.0B ", [ClientRandom]) ++ ClientHSecret,
                   io_lib:format("SERVER_HANDSHAKE_TRAFFIC_SECRET ~64.16.0B ", [ClientRandom]) ++ ServerHSecret,
                   io_lib:format("CLIENT_TRAFFIC_SECRET_0 ~64.16.0B ", [ClientRandom]) ++ ClientTrafficSecret0,
                   io_lib:format("SERVER_TRAFFIC_SECRET_0 ~64.16.0B ", [ClientRandom]) ++ ServerTrafficSecret0],
        Keylog = case lists:keyfind(client_early_data_secret, 1, Info) of
                     {client_early_data_secret, EarlySecret} ->
                         ClientEarlySecret = keylog_secret(EarlySecret, Prf),
                         [io_lib:format("CLIENT_EARLY_TRAFFIC_SECRET ~64.16.0B ", [ClientRandom]) ++ ClientEarlySecret
                          | Keylog0];
                     _ ->
                         Keylog0
                 end,
        Info ++ [{keylog,Keylog}]
    catch
        _Cxx:_Exx ->
            Info
    end;
maybe_add_keylog({_, _}, Info) ->
    try
        {client_random, ClientRandomBin} = lists:keyfind(client_random, 1, Info),
        {master_secret, MasterSecretBin} = lists:keyfind(master_secret, 1, Info),
        ClientRandom = binary:decode_unsigned(ClientRandomBin),
        MasterSecret = binary:decode_unsigned(MasterSecretBin),
        Keylog = [io_lib:format("CLIENT_RANDOM ~64.16.0B ~96.16.0B", [ClientRandom, MasterSecret])],
        Info ++ [{keylog,Keylog}]
    catch
        _Cxx:_Exx ->
            Info
    end;
maybe_add_keylog(_, Info) ->
    Info.

keylog_secret(SecretBin, sha256) ->
    io_lib:format("~64.16.0B", [binary:decode_unsigned(SecretBin)]);
keylog_secret(SecretBin, sha384) ->
    io_lib:format("~96.16.0B", [binary:decode_unsigned(SecretBin)]);
keylog_secret(SecretBin, sha512) ->
    io_lib:format("~128.16.0B", [binary:decode_unsigned(SecretBin)]).


%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(api,
                 {call, {?MODULE, connect, [Connection | _]}}, Stack0) ->
    {io_lib:format("Connection = ~w", [Connection]), Stack0};
handle_trace(rle,
                 {call, {?MODULE, init, Args = [[Role | _]]}}, Stack0) ->
    {io_lib:format("(*~w) Args = ~W", [Role, Args, 3]), [{role, Role} | Stack0]};
handle_trace(hbn,
             {call, {?MODULE, hibernate_after,
                     [_StateName = connection, State, Actions]}},
             Stack) ->
    #state{ssl_options= #{hibernate_after := HibernateAfter}} = State,
    {io_lib:format("* * * maybe hibernating in ~w ms * * * Actions = ~W ",
                   [HibernateAfter, Actions, 10]), Stack};
handle_trace(hbn,
             {return_from, {?MODULE, hibernate_after, 3},
              {Cmd, Arg,_State, Actions}},
             Stack) ->
    {io_lib:format("Cmd = ~w Arg = ~w Actions = ~W", [Cmd, Arg, Actions, 10]), Stack};
handle_trace(hbn,
             {call, {?MODULE, handle_common_event, [timeout, hibernate, connection | _]}}, Stack) ->
    {io_lib:format("* * * hibernating * * *", []), Stack}.
