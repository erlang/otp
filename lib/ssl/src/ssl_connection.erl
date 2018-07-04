%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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
%% Purpose: Common handling of a TLS/SSL/DTLS connection, see also
%% tls_connection.erl and dtls_connection.erl
%%----------------------------------------------------------------------

-module(ssl_connection).

-include("ssl_api.hrl").
-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Setup

-export([connect/8, handshake/7, handshake/2, handshake/3,
         handshake_continue/3, handshake_cancel/1,
	 socket_control/4, socket_control/5, start_or_recv_cancel_timer/2]).

%% User Events 
-export([send/2, recv/3, close/2, shutdown/2,
	 new_user/2, get_opts/2, set_opts/2, 
	 peer_certificate/1, renegotiation/1, negotiated_protocol/1, prf/5,
	 connection_information/2
	]).

%% Alert and close handling
-export([handle_own_alert/4, handle_alert/3, 
	 handle_normal_shutdown/3, stop/2, stop_and_reply/3
	]).

%% Data handling
-export([write_application_data/3, read_application_data/2]).

%% Help functions for tls|dtls_connection.erl
-export([handle_session/7, ssl_config/3,
	 prepare_connection/2, hibernate_after/3, map_extensions/1]).

%% General gen_statem state functions with extra callback argument 
%% to determine if it is an SSL/TLS or DTLS gen_statem machine
-export([init/4, error/4, hello/4, user_hello/4, abbreviated/4, certify/4, cipher/4,
         connection/4, death_row/4, downgrade/4]).

%% gen_statem callbacks
-export([terminate/3, format_status/2]).

%% Erlang Distribution export
-export([get_sslsocket/1, handshake_complete/3]).

%%====================================================================
%% Setup
%%====================================================================
%%--------------------------------------------------------------------
-spec connect(tls_connection | dtls_connection,
	      host(), inet:port_number(), 
	      port() | {tuple(), port()}, %% TLS | DTLS  
	      {#ssl_options{}, #socket_options{},
	       %% Tracker only needed on server side
	       undefined},
	      pid(), tuple(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
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
-spec handshake(tls_connection | dtls_connection,
		 inet:port_number(), port(),
		 {#ssl_options{}, #socket_options{}, undefined | pid()},
		 pid(), tuple(), timeout()) ->
			{ok, #sslsocket{}} | {error, reason()}.
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
                                                 {ok,  #sslsocket{}, map()}| {error, reason()}.
%%
%% Description: Starts ssl handshake. 
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = Pid} = Socket, Timeout) ->  
    case call(Pid, {start, Timeout}) of
	connected ->
	    {ok, Socket};
        {ok, Ext} ->
            {ok, Socket, Ext};
 	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}, {#ssl_options{},#socket_options{}},
		timeout()) ->   {ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Starts ssl handshake with some new options 
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = Pid} = Socket, SslOptions, Timeout) ->  
    case call(Pid, {start, SslOptions, Timeout}) of
	connected ->
	    {ok, Socket};
 	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec handshake_continue(#sslsocket{}, [ssl_option()],
                         timeout()) ->  {ok,  #sslsocket{}}| {error, reason()}.
%%
%% Description: Continues handshake with new options
%%--------------------------------------------------------------------
handshake_continue(#sslsocket{pid = Pid} = Socket, SslOptions, Timeout) ->  
    case call(Pid, {handshake_continue, SslOptions, Timeout}) of
	connected ->
	    {ok, Socket};
 	Error ->
	    Error
    end.
%%--------------------------------------------------------------------
-spec handshake_cancel(#sslsocket{}) ->  ok | {error, reason()}.
%%
%% Description: Cancels connection
%%--------------------------------------------------------------------
handshake_cancel(#sslsocket{pid = Pid}) ->  
    case call(Pid, cancel) of
	closed ->
            ok;
        Error ->
	    Error
    end.
%--------------------------------------------------------------------
-spec socket_control(tls_connection | dtls_connection, port(), pid(), atom()) -> 
    {ok, #sslsocket{}} | {error, reason()}.  
%%
%% Description: Set the ssl process to own the accept socket
%%--------------------------------------------------------------------	    
socket_control(Connection, Socket, Pid, Transport) ->
    socket_control(Connection, Socket, Pid, Transport, undefined).

%--------------------------------------------------------------------
-spec socket_control(tls_connection | dtls_connection, port(), pid(), atom(), pid()| undefined) -> 
    {ok, #sslsocket{}} | {error, reason()}.  
%%--------------------------------------------------------------------	    
socket_control(Connection, Socket, Pid, Transport, udp_listener) ->
    %% dtls listener process must have the socket control
    {ok, Connection:socket(Pid, Transport, Socket, Connection, undefined)};

socket_control(tls_connection = Connection, Socket, Pid, Transport, ListenTracker) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, Connection:socket(Pid, Transport, Socket, Connection, ListenTracker)};
	{error, Reason}	->
	    {error, Reason}
    end;
socket_control(dtls_connection = Connection, {_, Socket}, Pid, Transport, ListenTracker) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, Connection:socket(Pid, Transport, Socket, Connection, ListenTracker)};
	{error, Reason}	->
	    {error, Reason}
    end.

start_or_recv_cancel_timer(infinity, _RecvFrom) ->
    undefined;
start_or_recv_cancel_timer(Timeout, RecvFrom) ->
    erlang:send_after(Timeout, self(), {cancel_start_or_recv, RecvFrom}).

%%====================================================================
%% User events
%%====================================================================

%%--------------------------------------------------------------------
-spec send(pid(), iodata()) -> ok | {error, reason()}.
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(Pid, Data) -> 
    call(Pid, {application_data, 
				    %% iolist_to_binary should really
				    %% be called iodata_to_binary()
				    erlang:iolist_to_binary(Data)}).

%%--------------------------------------------------------------------
-spec recv(pid(), integer(), timeout()) ->  
    {ok, binary() | list()} | {error, reason()}.
%%
%% Description:  Receives data when active = false
%%--------------------------------------------------------------------
recv(Pid, Length, Timeout) -> 
    call(Pid, {recv, Length, Timeout}).

%%--------------------------------------------------------------------
-spec connection_information(pid(), boolean()) -> {ok, list()} | {error, reason()}.
%%
%% Description: Get the SNI hostname
%%--------------------------------------------------------------------
connection_information(Pid, IncludeSecrityInfo) when is_pid(Pid) ->
    call(Pid, {connection_information, IncludeSecrityInfo}).

%%--------------------------------------------------------------------
-spec close(pid(), {close, Timeout::integer() | 
				    {NewController::pid(), Timeout::integer()}}) -> 
		   ok | {ok, port()} | {error, reason()}.  
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
-spec shutdown(pid(), atom()) -> ok | {error, reason()}.  
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(ConnectionPid, How) ->
    call(ConnectionPid, {shutdown, How}).

%%--------------------------------------------------------------------
-spec new_user(pid(), pid()) ->  ok | {error, reason()}.
%%
%% Description:  Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
new_user(ConnectionPid, User) ->
    call(ConnectionPid, {new_user, User}).

%%--------------------------------------------------------------------
-spec negotiated_protocol(pid()) -> {ok, binary()} | {error, reason()}.
%%
%% Description:  Returns the negotiated protocol
%%--------------------------------------------------------------------
negotiated_protocol(ConnectionPid) ->
    call(ConnectionPid, negotiated_protocol).

%%--------------------------------------------------------------------
-spec get_opts(pid(), list()) -> {ok, list()} | {error, reason()}.    
%%
%% Description: Same as inet:getopts/2
%%--------------------------------------------------------------------
get_opts(ConnectionPid, OptTags) ->
    call(ConnectionPid, {get_opts, OptTags}).
%%--------------------------------------------------------------------
-spec set_opts(pid(), list()) -> ok | {error, reason()}. 
%%
%% Description:  Same as inet:setopts/2
%%--------------------------------------------------------------------
set_opts(ConnectionPid, Options) ->
    call(ConnectionPid, {set_opts, Options}).

%%--------------------------------------------------------------------
-spec peer_certificate(pid()) -> {ok, binary()| undefined} | {error, reason()}.
%%
%% Description: Returns the peer cert
%%--------------------------------------------------------------------
peer_certificate(ConnectionPid) ->
    call(ConnectionPid, peer_certificate). 

%%--------------------------------------------------------------------
-spec renegotiation(pid()) -> ok | {error, reason()}.
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
renegotiation(ConnectionPid) ->
    call(ConnectionPid, renegotiate). 


get_sslsocket(ConnectionPid) ->
    call(ConnectionPid, get_sslsocket).

handshake_complete(ConnectionPid, Node, DHandle) ->
    call(ConnectionPid, {handshake_complete, Node, DHandle}).

%%--------------------------------------------------------------------
-spec prf(pid(), binary() | 'master_secret', binary(),
	  [binary() | ssl:prf_random()], non_neg_integer()) ->
		 {ok, binary()} | {error, reason()} | {'EXIT', term()}.
%%
%% Description: use a ssl sessions TLS PRF to generate key material
%%--------------------------------------------------------------------
prf(ConnectionPid, Secret, Label, Seed, WantedLength) ->
    call(ConnectionPid, {prf, Secret, Label, Seed, WantedLength}).

%%====================================================================
%% Alert and close handling
%%====================================================================
handle_own_alert(Alert, Version, StateName, 
		 #state{role = Role,
                        transport_cb = Transport,
                        socket = Socket,
                        protocol_cb = Connection,
                        connection_states = ConnectionStates,
                        ssl_options = SslOpts} = State) ->
    try %% Try to tell the other side
	{BinMsg, _} =
	Connection:encode_alert(Alert, Version, ConnectionStates),
	Connection:send(Transport, Socket, BinMsg)
    catch _:_ ->  %% Can crash if we are in a uninitialized state
	    ignore
    end,
    try %% Try to tell the local user
	log_alert(SslOpts#ssl_options.log_alert, Role, Connection:protocol_name(), StateName, Alert#alert{role = Role}),
	handle_normal_shutdown(Alert,StateName, State)
    catch _:_ ->
	    ok
    end,
    stop({shutdown, own_alert}, State).

handle_normal_shutdown(Alert, _, #state{socket = Socket,
					transport_cb = Transport,
					protocol_cb = Connection,
					start_or_recv_from = StartFrom,
					tracker = Tracker,
					role = Role, renegotiation = {false, first}}) ->
    alert_user(Transport, Tracker,Socket, StartFrom, Alert, Role, Connection);

handle_normal_shutdown(Alert, StateName, #state{socket = Socket,
						socket_options = Opts,
						transport_cb = Transport,
						protocol_cb = Connection,
						user_application = {_Mon, Pid},
						tracker = Tracker,
						start_or_recv_from = RecvFrom, role = Role}) ->
    alert_user(Transport, Tracker, Socket, StateName, Opts, Pid, RecvFrom, Alert, Role, Connection).

handle_alert(#alert{level = ?FATAL} = Alert, StateName,
	     #state{socket = Socket, transport_cb = Transport, 
		    protocol_cb = Connection,
		    ssl_options = SslOpts, start_or_recv_from = From, host = Host,
		    port = Port, session = Session, user_application = {_Mon, Pid},
		    role = Role, socket_options = Opts, tracker = Tracker} = State) ->
    invalidate_session(Role, Host, Port, Session),
    log_alert(SslOpts#ssl_options.log_alert, Role, Connection:protocol_name(), 
              StateName, Alert#alert{role = opposite_role(Role)}),
    alert_user(Transport, Tracker, Socket, StateName, Opts, Pid, From, Alert, Role, Connection),
    stop(normal, State);

handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	     StateName, State) -> 
    handle_normal_shutdown(Alert, StateName, State),
    stop({shutdown, peer_close}, State);

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{role = Role, ssl_options = SslOpts, protocol_cb = Connection, renegotiation = {true, internal}} = State) ->
    log_alert(SslOpts#ssl_options.log_alert, Role, 
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    handle_normal_shutdown(Alert, StateName, State),
    stop({shutdown, peer_close}, State);

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{role = Role,
                    ssl_options = SslOpts, renegotiation = {true, From},
		    protocol_cb = Connection} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert,  Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    gen_statem:reply(From, {error, renegotiation_rejected}),
    {Record, State1} = Connection:next_record(State0),
    %% Go back to connection!
    State = Connection:reinit_handshake_data(State1#state{renegotiation = undefined}),
    Connection:next_event(connection, Record, State);

%% Gracefully log and ignore all other warning alerts
handle_alert(#alert{level = ?WARNING} = Alert, StateName,
	     #state{ssl_options = SslOpts, protocol_cb = Connection, role = Role} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert,  Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    {Record, State} = Connection:next_record(State0),
    Connection:next_event(StateName, Record, State).

%%====================================================================
%% Data handling
%%====================================================================
write_application_data(Data0, {FromPid, _} = From,
		       #state{socket = Socket,
			      negotiated_version = Version,
			      protocol_cb = Connection,
			      transport_cb = Transport,
			      connection_states = ConnectionStates0,
			      socket_options = SockOpts,
			      ssl_options = #ssl_options{renegotiate_at = RenegotiateAt}} = State) ->
    Data = encode_packet(Data0, SockOpts),
    
    case time_to_renegotiate(Data, ConnectionStates0, RenegotiateAt) of
	true ->
	    Connection:renegotiate(State#state{renegotiation = {true, internal}}, 
			[{next_event, {call, From}, {application_data, Data0}}]);
	false ->
	    {Msgs, ConnectionStates} =
                Connection:encode_data(Data, Version, ConnectionStates0),
            NewState = State#state{connection_states = ConnectionStates},
	    case Connection:send(Transport, Socket, Msgs) of
                ok when FromPid =:= self() ->
                    hibernate_after(connection, NewState, []);
                Error when FromPid =:= self() ->
                    stop({shutdown, Error}, NewState);
                ok ->
                    hibernate_after(connection, NewState, [{reply, From, ok}]);
                Result ->
                    hibernate_after(connection, NewState, [{reply, From, Result}])
            end
    end.

read_application_data(Data, #state{user_application = {_Mon, Pid},
				   socket = Socket,
				   protocol_cb = Connection,
				   transport_cb = Transport,
				   socket_options = SOpts,
				   bytes_to_read = BytesToRead,
				   start_or_recv_from = RecvFrom,
				   timer = Timer,
				   user_data_buffer = Buffer0,
				   tracker = Tracker} = State0) ->
    Buffer1 = if 
		  Buffer0 =:= <<>> -> Data;
		  Data =:= <<>> -> Buffer0;
		  true -> <<Buffer0/binary, Data/binary>>
	      end,
    case get_data(SOpts, BytesToRead, Buffer1) of
	{ok, ClientData, Buffer} -> % Send data
            case State0 of
                #state{
                   ssl_options = #ssl_options{erl_dist = true},
                  protocol_specific = #{d_handle := DHandle}} ->
                    State =
                        State0#state{
                          user_data_buffer = Buffer,
                          bytes_to_read = undefined},
                    try erlang:dist_ctrl_put_data(DHandle, ClientData) of
                        _
                          when SOpts#socket_options.active =:= false;
                               Buffer =:= <<>> ->
                            %% Passive mode, wait for active once or recv
                            %% Active and empty, get more data
                            Connection:next_record_if_active(State);
                        _ -> %% We have more data
                            read_application_data(<<>>, State)
                    catch error:_ ->
                            death_row(State, disconnect)
                    end;
                _ ->
                    SocketOpt =
                        deliver_app_data(
                          Transport, Socket, SOpts,
                          ClientData, Pid, RecvFrom, Tracker, Connection),
                    cancel_timer(Timer),
                    State =
                        State0#state{
                          user_data_buffer = Buffer,
                          start_or_recv_from = undefined,
                          timer = undefined,
                          bytes_to_read = undefined,
                          socket_options = SocketOpt
                         },
                    if
                        SocketOpt#socket_options.active =:= false;
                        Buffer =:= <<>> ->
                            %% Passive mode, wait for active once or recv
                            %% Active and empty, get more data
                            Connection:next_record_if_active(State);
                        true -> %% We have more data
                            read_application_data(<<>>, State)
                    end
            end;
	{more, Buffer} -> % no reply, we need more data
	    Connection:next_record(State0#state{user_data_buffer = Buffer});
	{passive, Buffer} ->
	    Connection:next_record_if_active(State0#state{user_data_buffer = Buffer});
	{error,_Reason} -> %% Invalid packet in packet mode
	    deliver_packet_error(Transport, Socket, SOpts, Buffer1, Pid, RecvFrom, Tracker, Connection),
            stop(normal, State0)
    end.
%%====================================================================
%% Help functions for tls|dtls_connection.erl
%%====================================================================
%%--------------------------------------------------------------------
-spec handle_session(#server_hello{}, ssl_record:ssl_version(),
		     binary(), ssl_record:connection_states(), _,_, #state{}) ->
			    gen_statem:state_function_result().
%%--------------------------------------------------------------------
handle_session(#server_hello{cipher_suite = CipherSuite,
			     compression_method = Compression}, 
	       Version, NewId, ConnectionStates, ProtoExt, Protocol0,
	       #state{session = #session{session_id = OldId},
		      negotiated_version = ReqVersion,
		      negotiated_protocol = CurrentProtocol} = State0) ->
    #{key_exchange := KeyAlgorithm} =
	ssl_cipher:suite_definition(CipherSuite),
    
    PremasterSecret = make_premaster_secret(ReqVersion, KeyAlgorithm),

    {ExpectNPN, Protocol} = case Protocol0 of
				undefined -> 
				    {false, CurrentProtocol};
				_ -> 
				    {ProtoExt =:= npn, Protocol0}
			    end,

    State = State0#state{key_algorithm = KeyAlgorithm,
			 negotiated_version = Version,
			 connection_states = ConnectionStates,
			 premaster_secret = PremasterSecret,
			 expecting_next_protocol_negotiation = ExpectNPN,
			 negotiated_protocol = Protocol},
    
    case ssl_session:is_new(OldId, NewId) of
	true ->
	    handle_new_session(NewId, CipherSuite, Compression,
			       State#state{connection_states = ConnectionStates});
	false ->
	    handle_resumed_session(NewId,
				   State#state{connection_states = ConnectionStates})
    end.

%%--------------------------------------------------------------------
-spec ssl_config(#ssl_options{}, client | server, #state{}) -> #state{}.
%%--------------------------------------------------------------------
ssl_config(Opts, Role, State) ->
    ssl_config(Opts, Role, State, new).

ssl_config(Opts, Role, State0, Type) ->
    {ok, #{cert_db_ref := Ref, 
           cert_db_handle := CertDbHandle, 
           fileref_db_handle := FileRefHandle, 
           session_cache := CacheHandle, 
           crl_db_info := CRLDbHandle,
           private_key := Key,
           dh_params := DHParams,
           own_certificate := OwnCert}} =
	ssl_config:init(Opts, Role), 
    TimeStamp = erlang:monotonic_time(),
    Session = State0#state.session,
    State = State0#state{session = Session#session{own_certificate = OwnCert,
                                                   time_stamp = TimeStamp},
                         file_ref_db = FileRefHandle,
                         cert_db_ref = Ref,
                         cert_db = CertDbHandle,
                         crl_db = CRLDbHandle,
                         session_cache = CacheHandle,
                         private_key = Key,
                         diffie_hellman_params = DHParams,
                         ssl_options = Opts},
    case Type of
        new ->
            Handshake = ssl_handshake:init_handshake_history(),
            State#state{tls_handshake_history = Handshake};
        continue ->
            State
    end.


%%====================================================================
%% gen_statem general state functions with connection cb argument
%%====================================================================	
%%--------------------------------------------------------------------
-spec init(gen_statem:event_type(),
	   {start, timeout()} |  {start, {list(), list()}, timeout()}| term(),
	    #state{}, tls_connection | dtls_connection) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------

init({call, From}, {start, Timeout}, State0, Connection) ->
    Timer = start_or_recv_cancel_timer(Timeout, From),
    {Record, State} = Connection:next_record(State0#state{start_or_recv_from = From,
							  timer = Timer}),
    Connection:next_event(hello, Record, State);
init({call, From}, {start, {Opts, EmOpts}, Timeout}, 
     #state{role = Role, ssl_options = OrigSSLOptions,
            socket_options = SockOpts} = State0, Connection) ->
    try 
        SslOpts = ssl:handle_options(Opts, OrigSSLOptions),
        case SslOpts of
            #ssl_options{erl_dist = true} ->
                process_flag(priority, max);
            _ ->
                ok
        end,
	State = ssl_config(SslOpts, Role, State0),
	init({call, From}, {start, Timeout}, 
	     State#state{ssl_options = SslOpts, 
                         socket_options = new_emulated(EmOpts, SockOpts)}, Connection)
    catch throw:Error ->
	    stop_and_reply(normal, {reply, From, {error, Error}}, State0)
    end;
init({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
init(_Type, _Event, _State, _Connection) ->
    {keep_state_and_data, [postpone]}.
	
%%--------------------------------------------------------------------
-spec error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{},
            tls_connection | dtls_connection) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
error({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection).

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #server_hello{} | term(),
	    #state{}, tls_connection | dtls_connection) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
hello(internal, {common_client_hello, Type, ServerHelloExt}, State, Connection) ->
    do_server_hello(Type, ServerHelloExt, State, Connection);
hello(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
hello(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).

user_hello({call, From}, cancel, #state{negotiated_version = Version} = State, _) ->
    gen_statem:reply(From, ok),
    handle_own_alert(?ALERT_REC(?FATAL, ?USER_CANCELED, user_canceled),
                     Version, ?FUNCTION_NAME, State);
user_hello({call, From}, {handshake_continue, NewOptions, Timeout}, #state{hello = Hello,
                                                                           role = Role,
                                                                           start_or_recv_from = RecvFrom,
                                                                           ssl_options = Options0} = State0, _Connection) ->
    Timer = start_or_recv_cancel_timer(Timeout, RecvFrom),
    Options = ssl:handle_options(NewOptions, Options0#ssl_options{handshake = full}),
    State = ssl_config(Options, Role, State0, continue),
    {next_state, hello, State#state{start_or_recv_from = From,
                                    timer = Timer}, 
     [{next_event, internal, Hello}]};
user_hello(_, _, _, _) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(),
		  #hello_request{} | #finished{} | term(),
		  #state{}, tls_connection | dtls_connection) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{role = server,
		   negotiated_version = Version,
		   expecting_finished = true,
		   tls_handshake_history = Handshake,
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} =
		State0, Connection) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished, client,
					 get_current_prf(ConnectionStates0, write),
					 MasterSecret, Handshake) of
        verified ->
	    ConnectionStates =
		ssl_record:set_client_verify_data(current_both, Data, ConnectionStates0),
	    {Record, State} = prepare_connection(State0#state{connection_states = ConnectionStates,
							      expecting_finished = false}, Connection),
	    Connection:next_event(connection, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{role = client, tls_handshake_history = Handshake0,
		   session = #session{master_secret = MasterSecret},
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0, Connection) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished, server,
					 get_pending_prf(ConnectionStates0, write),
					 MasterSecret, Handshake0) of
        verified ->
	    ConnectionStates1 =
		ssl_record:set_server_verify_data(current_read, Data, ConnectionStates0),
	    {State1, Actions} =
		finalize_handshake(State0#state{connection_states = ConnectionStates1},
				   ?FUNCTION_NAME, Connection),
	    {Record, State} = prepare_connection(State1#state{expecting_finished = false}, Connection),
	    Connection:next_event(connection, Record, State, Actions);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
abbreviated(internal, #next_protocol{selected_protocol = SelectedProtocol},
	    #state{role = server, expecting_next_protocol_negotiation = true} = State0,
	    Connection) ->
    {Record, State} =
	Connection:next_record(State0#state{negotiated_protocol = SelectedProtocol}),
    Connection:next_event(?FUNCTION_NAME, Record, 
			  State#state{expecting_next_protocol_negotiation = false});
abbreviated(internal, 
	    #change_cipher_spec{type = <<1>>},  #state{connection_states = ConnectionStates0} =
		State0, Connection) ->
    ConnectionStates1 =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    {Record, State} = Connection:next_record(State0#state{connection_states = 
							      ConnectionStates1}),
    Connection:next_event(?FUNCTION_NAME, Record, State#state{expecting_finished = true});
abbreviated(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
abbreviated(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).
 
%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(),
	      #hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}, tls_connection | dtls_connection) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
certify(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
certify(internal, #certificate{asn1_certificates = []},
	#state{role = server, negotiated_version = Version,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = true}} =
	    State, _) ->
    Alert =  ?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE),
    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State);
certify(internal, #certificate{asn1_certificates = []},
	#state{role = server,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = false}} =
	State0, Connection) ->
    {Record, State} = 
	Connection:next_record(State0#state{client_certificate_requested = false}),
    Connection:next_event(?FUNCTION_NAME, Record, State);
certify(internal, #certificate{},
	#state{role = server,
	       negotiated_version = Version,
	       ssl_options = #ssl_options{verify = verify_none}} =
	    State, _) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, unrequested_certificate),
    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State);
certify(internal, #certificate{} = Cert,
        #state{negotiated_version = Version,
	       role = Role,
               host = Host,
	       cert_db = CertDbHandle,
	       cert_db_ref = CertDbRef,
	       crl_db = CRLDbInfo,
	       ssl_options = Opts} = State, Connection) ->
    case ssl_handshake:certify(Cert, CertDbHandle, CertDbRef, 
			       Opts, CRLDbInfo, Role, Host) of
        {PeerCert, PublicKeyInfo} ->
	    handle_peer_cert(Role, PeerCert, PublicKeyInfo,
			     State#state{client_certificate_requested = false}, Connection);
	#alert{} = Alert ->
            handle_own_alert(Alert, Version, ?FUNCTION_NAME, State)
    end;
certify(internal, #server_key_exchange{exchange_keys = Keys},
        #state{role = client, negotiated_version = Version,
	       key_algorithm = Alg,
	       public_key_info = PubKeyInfo,
	       connection_states = ConnectionStates} = State, Connection)
  when Alg == dhe_dss; Alg == dhe_rsa;
       Alg == ecdhe_rsa; Alg == ecdhe_ecdsa;
       Alg == dh_anon; Alg == ecdh_anon;
       Alg == psk; Alg == dhe_psk; Alg == ecdhe_psk; Alg == rsa_psk;
       Alg == srp_dss; Alg == srp_rsa; Alg == srp_anon ->

    Params = ssl_handshake:decode_server_key(Keys, Alg, ssl:tls_version(Version)),

    %% Use negotiated value if TLS-1.2 otherwhise return default
    HashSign = negotiated_hashsign(Params#server_key_params.hashsign, Alg, PubKeyInfo, ssl:tls_version(Version)),

    case is_anonymous(Alg) of
	true ->
	    calculate_secret(Params#server_key_params.params,
			     State#state{hashsign_algorithm = HashSign}, Connection);
	false ->
	    case  ssl_handshake:verify_server_key(Params, HashSign, 
						  ConnectionStates, ssl:tls_version(Version), PubKeyInfo) of
		true ->
		    calculate_secret(Params#server_key_params.params,
				     State#state{hashsign_algorithm = HashSign}, 
				     Connection);
		false ->
		    handle_own_alert(?ALERT_REC(?FATAL, ?DECRYPT_ERROR),
						Version, ?FUNCTION_NAME, State)
	    end
    end;
certify(internal, #certificate_request{},
	#state{role = client, negotiated_version = Version,
               key_algorithm = Alg} = State, _)
  when Alg == dh_anon; Alg == ecdh_anon;
       Alg == psk; Alg == dhe_psk; Alg == ecdhe_psk; Alg == rsa_psk;
       Alg == srp_dss; Alg == srp_rsa; Alg == srp_anon ->
    handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE),
                     Version, ?FUNCTION_NAME, State);
certify(internal, #certificate_request{},
	#state{session = #session{own_certificate = undefined},
	       role = client} = State0, Connection) ->
    %% The client does not have a certificate and will send an empty reply, the server may fail 
    %% or accept the connection by its own preference. No signature algorihms needed as there is
    %% no certificate to verify.
    {Record, State} = Connection:next_record(State0),
    Connection:next_event(?FUNCTION_NAME, Record, State#state{client_certificate_requested = true});
certify(internal, #certificate_request{} = CertRequest,
	#state{session = #session{own_certificate = Cert},
	       role = client,
	       ssl_options = #ssl_options{signature_algs = SupportedHashSigns},
	       negotiated_version = Version} = State0, Connection) ->
    case ssl_handshake:select_hashsign(CertRequest, Cert, SupportedHashSigns, ssl:tls_version(Version)) of
	#alert {} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0);
	NegotiatedHashSign -> 
	    {Record, State} = Connection:next_record(State0#state{client_certificate_requested = true}),
	    Connection:next_event(?FUNCTION_NAME, Record,
				  State#state{cert_hashsign_algorithm = NegotiatedHashSign})
    end;
%% PSK and RSA_PSK might bypass the Server-Key-Exchange
certify(internal, #server_hello_done{},
	#state{session = #session{master_secret = undefined},
	       negotiated_version = Version,
	       psk_identity = PSKIdentity,
	       ssl_options = #ssl_options{user_lookup_fun = PSKLookup},
	       premaster_secret = undefined,
	       role = client,
	       key_algorithm = Alg} = State0, Connection)
  when Alg == psk ->
    case ssl_handshake:premaster_secret({Alg, PSKIdentity}, PSKLookup) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0);
	PremasterSecret ->
	    State = master_secret(PremasterSecret,
				  State0#state{premaster_secret = PremasterSecret}),
	    client_certify_and_key_exchange(State, Connection)
    end;
certify(internal, #server_hello_done{},
	#state{session = #session{master_secret = undefined},
	       ssl_options = #ssl_options{user_lookup_fun = PSKLookup},
	       negotiated_version = {Major, Minor} = Version,
	       psk_identity = PSKIdentity,
	       premaster_secret = undefined,
	       role = client,
	       key_algorithm = Alg} = State0, Connection)
  when Alg == rsa_psk ->
    Rand = ssl_cipher:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    RSAPremasterSecret = <<?BYTE(Major), ?BYTE(Minor), Rand/binary>>,
    case ssl_handshake:premaster_secret({Alg, PSKIdentity}, PSKLookup, 
					RSAPremasterSecret) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0);
	PremasterSecret ->
	    State = master_secret(PremasterSecret, 
				  State0#state{premaster_secret = RSAPremasterSecret}),
	    client_certify_and_key_exchange(State, Connection)
    end;
%% Master secret was determined with help of server-key exchange msg
certify(internal, #server_hello_done{}, 
	#state{session = #session{master_secret = MasterSecret} = Session,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = undefined,
	       role = client} = State0, Connection) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    State = State0#state{connection_states = ConnectionStates},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
%% Master secret is calculated from premaster_secret
certify(internal, #server_hello_done{},
	#state{session = Session0,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = PremasterSecret,
	       role = client} = State0, Connection) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates,
				 session = Session},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
certify(internal = Type, #client_key_exchange{} = Msg,
	#state{role = server,
	       client_certificate_requested = true,
	       ssl_options = #ssl_options{fail_if_no_peer_cert = true}} = State, 
	Connection) ->
    %% We expect a certificate here
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection);
certify(internal, #client_key_exchange{exchange_keys = Keys},
	State = #state{key_algorithm = KeyAlg, negotiated_version = Version}, Connection) ->
    try
	certify_client_key_exchange(ssl_handshake:decode_client_key(Keys, KeyAlg, ssl:tls_version(Version)),
				    State, Connection)
    catch
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State)
    end;
certify(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).
 
%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(),
	     #hello_request{} | #certificate_verify{} | #finished{} | term(),
	     #state{}, tls_connection | dtls_connection) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
cipher(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
cipher(internal, #certificate_verify{signature = Signature, 
				     hashsign_algorithm = CertHashSign},
       #state{role = server,
	      key_algorithm = KexAlg,
	      public_key_info = PublicKeyInfo,
	      negotiated_version = Version,
	      session = #session{master_secret = MasterSecret},
	      tls_handshake_history = Handshake
	     } = State0, Connection) ->
    
    TLSVersion = ssl:tls_version(Version),
    %% Use negotiated value if TLS-1.2 otherwhise return default
    HashSign = negotiated_hashsign(CertHashSign, KexAlg, PublicKeyInfo, TLSVersion),
    case ssl_handshake:certificate_verify(Signature, PublicKeyInfo,
					  TLSVersion, HashSign, MasterSecret, Handshake) of
	valid ->
	    {Record, State} = Connection:next_record(State0),
	    Connection:next_event(?FUNCTION_NAME, Record,
				  State#state{cert_hashsign_algorithm = HashSign});
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
%% client must send a next protocol message if we are expecting it
cipher(internal, #finished{},
       #state{role = server, expecting_next_protocol_negotiation = true,
	      negotiated_protocol = undefined, negotiated_version = Version} = State0,
       _Connection) ->
    handle_own_alert(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE), Version, ?FUNCTION_NAME, State0);
cipher(internal, #finished{verify_data = Data} = Finished,
       #state{negotiated_version = Version,
	      host = Host,
	      port = Port,
	      role = Role,
	      expecting_finished = true,
	      session = #session{master_secret = MasterSecret}
	      = Session0,
              ssl_options = SslOpts,
	      connection_states = ConnectionStates0,
	      tls_handshake_history = Handshake0} = State, Connection) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished,
					 opposite_role(Role),
					 get_current_prf(ConnectionStates0, read),
					 MasterSecret, Handshake0) of
        verified ->
	    Session = register_session(Role, host_id(Role, Host, SslOpts), Port, Session0),
	    cipher_role(Role, Data, Session, 
			State#state{expecting_finished = false}, Connection);
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State)
    end;
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
cipher(internal, #next_protocol{selected_protocol = SelectedProtocol},
       #state{role = server, expecting_next_protocol_negotiation = true,
	      expecting_finished = true} = State0, Connection) ->
    {Record, State} = 
	Connection:next_record(State0#state{negotiated_protocol = SelectedProtocol}),
    Connection:next_event(?FUNCTION_NAME, Record, 
			  State#state{expecting_next_protocol_negotiation = false});
cipher(internal, #change_cipher_spec{type = <<1>>},  #state{connection_states = ConnectionStates0} =
	   State0, Connection) ->
    ConnectionStates1 =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    {Record, State} = Connection:next_record(State0#state{connection_states = 
							      ConnectionStates1}),
    Connection:next_event(?FUNCTION_NAME, Record, State#state{expecting_finished = true});
cipher(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(), term(), 
		 #state{}, tls_connection | dtls_connection) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection({call, {FromPid, _} = From}, {application_data, Data},
	   #state{protocol_cb = Connection} = State, Connection) ->
    %% We should look into having a worker process to do this to 
    %% parallize send and receive decoding and not block the receiver
    %% if sending is overloading the socket.
     try
	 write_application_data(Data, From, State)
     catch throw:Error ->
             case self() of
                 FromPid ->
                     stop({shutdown, Error}, State);
                 _ ->
                     hibernate_after(
                       ?FUNCTION_NAME, State, [{reply, From, Error}])
             end
     end;
connection({call, RecvFrom}, {recv, N, Timeout},  
	   #state{protocol_cb = Connection,  socket_options =
		      #socket_options{active = false}} = State0, Connection) ->
    Timer = start_or_recv_cancel_timer(Timeout, RecvFrom),
    Connection:passive_receive(State0#state{bytes_to_read = N,
					    start_or_recv_from = RecvFrom, 
					    timer = Timer}, ?FUNCTION_NAME);
connection({call, From}, renegotiate, #state{protocol_cb = Connection} = State, 
	   Connection) ->
    Connection:renegotiate(State#state{renegotiation = {true, From}}, []);
connection({call, From}, peer_certificate, 
	   #state{session = #session{peer_certificate = Cert}} = State, _) ->
    hibernate_after(?FUNCTION_NAME, State, [{reply, From,  {ok, Cert}}]); 
connection({call, From}, {connection_information, true}, State, _) ->
    Info = connection_info(State) ++ security_info(State),
    hibernate_after(?FUNCTION_NAME, State, [{reply, From, {ok, Info}}]);
connection({call, From}, {connection_information, false}, State, _) ->
    Info = connection_info(State),
    hibernate_after(?FUNCTION_NAME, State, [{reply, From, {ok, Info}}]);
connection({call, From}, negotiated_protocol, 
	   #state{negotiated_protocol = undefined} = State, _) ->
    hibernate_after(?FUNCTION_NAME, State, [{reply, From, {error, protocol_not_negotiated}}]);
connection({call, From}, negotiated_protocol, 
	   #state{negotiated_protocol = SelectedProtocol} = State, _) ->
    hibernate_after(?FUNCTION_NAME, State,
		    [{reply, From, {ok, SelectedProtocol}}]);
connection(
  {call, From}, {handshake_complete, _Node, DHandle},
  #state{
     ssl_options = #ssl_options{erl_dist = true},
     socket_options = SockOpts,
     protocol_specific = ProtocolSpecific} = State,
  Connection) ->
    %% From now on we execute on normal priority
    process_flag(priority, normal),
    try erlang:dist_ctrl_get_data_notification(DHandle) of
        _ ->
            NewState =
                State#state{
                  socket_options =
                      SockOpts#socket_options{active = true},
                  protocol_specific =
                      ProtocolSpecific#{d_handle => DHandle}},
            {Record, NewerState} = Connection:next_record_if_active(NewState),
            Connection:next_event(connection, Record, NewerState, [{reply, From, ok}])
    catch error:_ ->
            death_row(State, disconnect)
    end;
connection({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
connection(
  info, dist_data = Msg,
  #state{
     ssl_options = #ssl_options{erl_dist = true},
     protocol_specific = #{d_handle := DHandle}} = State,
  _) ->
    eat_msgs(Msg),
    try send_dist_data(?FUNCTION_NAME, State, DHandle, [])
    catch error:_ ->
            death_row(State, disconnect)
    end;
connection(
  info, {send, From, Ref, Data},
  #state{
     ssl_options = #ssl_options{erl_dist = true},
     protocol_specific = #{d_handle := _}},
  _) ->
    %% This is for testing only!
    %%
    %% Needed by some OTP distribution
    %% test suites...
    From ! {Ref, ok},
    {keep_state_and_data,
     [{next_event, {call, {self(), undefined}},
       {application_data, iolist_to_binary(Data)}}]};
connection(
  info, tick = Msg,
  #state{
     ssl_options = #ssl_options{erl_dist = true},
     protocol_specific = #{d_handle := _}},
  _) ->
    eat_msgs(Msg),
    {keep_state_and_data,
     [{next_event, {call, {self(), undefined}}, {application_data, <<>>}}]};
connection(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
connection(internal, {recv, _}, State, Connection) ->
    Connection:passive_receive(State, ?FUNCTION_NAME);
connection(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).

%%--------------------------------------------------------------------
-spec death_row(gen_statem:event_type(), term(),
		#state{}, tls_connection | dtls_connection) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
%% We just wait for the owner to die which triggers the monitor,
%% or the socket may die too
death_row(
  info, {'DOWN', MonitorRef, _, _, Reason},
  #state{user_application={MonitorRef,_Pid}},
  _) ->
    {stop, {shutdown, Reason}};
death_row(
  info, {'EXIT', Socket, Reason}, #state{socket = Socket}, _) ->
    {stop, {shutdown, Reason}};
death_row(state_timeout, Reason, _State, _Connection) ->
    {stop, {shutdown,Reason}};
death_row(_Type, _Msg, _State, _Connection) ->
    %% Waste all other events
    keep_state_and_data.

%% State entry function
death_row(State, Reason) ->
    {next_state, death_row, State,
     [{state_timeout, 5000, Reason}]}.

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), 
		#state{}, tls_connection | dtls_connection) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(internal, #alert{description = ?CLOSE_NOTIFY},
	  #state{transport_cb = Transport, socket = Socket,
		 downgrade = {Pid, From}} = State, _) ->
    tls_socket:setopts(Transport, Socket, [{active, false}, {packet, 0}, {mode, binary}]),
    Transport:controlling_process(Socket, Pid),
    gen_statem:reply(From, {ok, Socket}),
    stop(normal, State);
downgrade(timeout, downgrade, #state{downgrade = {_, From}} = State, _) ->
    gen_statem:reply(From, {error, timeout}),
    stop(normal, State);
downgrade(Type, Event, State, Connection) ->
    handle_common_event(Type, Event, ?FUNCTION_NAME, State, Connection).

%%--------------------------------------------------------------------
%% Event handling functions called by state functions to handle
%% common or unexpected events for the state.
%%--------------------------------------------------------------------
handle_common_event(internal, {handshake, {#hello_request{} = Handshake, _}}, connection = StateName,  
		    #state{role = client} = State, _) ->
    %% Should not be included in handshake history
    {next_state, StateName, State#state{renegotiation = {true, peer}}, [{next_event, internal, Handshake}]};
handle_common_event(internal, {handshake, {#hello_request{}, _}}, StateName, #state{role = client}, _) 
  when StateName =/= connection ->
    {keep_state_and_data};
handle_common_event(internal, {handshake, {Handshake, Raw}}, StateName,
		    #state{tls_handshake_history = Hs0} = State0,
		    Connection) ->
   
    PossibleSNI = Connection:select_sni_extension(Handshake),
    %% This function handles client SNI hello extension when Handshake is
    %% a client_hello, which needs to be determined by the connection callback.
    %% In other cases this is a noop
    State = handle_sni_extension(PossibleSNI, State0),
    HsHist = ssl_handshake:update_handshake_history(Hs0, iolist_to_binary(Raw)),
    {next_state, StateName, State#state{tls_handshake_history = HsHist}, 
     [{next_event, internal, Handshake}]};
handle_common_event(internal, {protocol_record, TLSorDTLSRecord}, StateName, State, Connection) -> 
    Connection:handle_common_event(internal, TLSorDTLSRecord, StateName, State);
handle_common_event(timeout, hibernate, _, _, _) ->
    {keep_state_and_data, [hibernate]};
handle_common_event(internal, {application_data, Data}, StateName, State0, Connection) ->
    case read_application_data(Data, State0) of
	{stop, _, _} = Stop->
            Stop;
	{Record, State} ->
   	    Connection:next_event(StateName, Record, State)
    end;
handle_common_event(internal, #change_cipher_spec{type = <<1>>}, StateName, 
		    #state{negotiated_version = Version} = State,  _) ->
    handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE), Version, 
				StateName, State);
handle_common_event(_Type, Msg, StateName, #state{negotiated_version = Version} = State, 
		    _) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, {unexpected_msg, Msg}),
    handle_own_alert(Alert, Version, StateName, State).

handle_call({application_data, _Data}, _, _, _, _) ->
    %% In renegotiation priorities handshake, send data when handshake is finished
    {keep_state_and_data, [postpone]};
handle_call({close, {Pid, Timeout}}, From, StateName, State0, Connection) when is_pid(Pid) ->
    %% terminate will send close alert to peer
    State = State0#state{downgrade = {Pid, From}},
    Connection:terminate(downgrade, StateName, State),
    %% User downgrades connection
    %% When downgrading an TLS connection to a transport connection
    %% we must recive the close alert from the peer before releasing the 
    %% transport socket.
    {next_state, downgrade, State#state{terminated = true}, [{timeout, Timeout, downgrade}]};
handle_call({close, _} = Close, From, StateName, State, Connection) ->
    %% Run terminate before returning so that the reuseaddr
    %% inet-option works properly
    Result = Connection:terminate(Close, StateName, State#state{terminated = true}),
    stop_and_reply(
      {shutdown, normal},
      {reply, From, Result}, State);
handle_call({shutdown, How0}, From, _,
	    #state{transport_cb = Transport,
		   negotiated_version = Version,
		   connection_states = ConnectionStates,
		   socket = Socket} = State, Connection) ->
    case How0 of
	How when How == write; How == both ->	    
	    Alert = ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
	    {BinMsg, _} =
		Connection:encode_alert(Alert, Version, ConnectionStates),
	    Connection:send(Transport, Socket, BinMsg);
	_ ->
	    ok
    end,

    case Transport:shutdown(Socket, How0) of
	ok ->
	    {keep_state_and_data, [{reply, From, ok}]};
	Error ->
	    gen_statem:reply(From, {error, Error}),
            stop(normal, State)
    end;
handle_call({recv, _N, _Timeout}, From, _,  
		  #state{socket_options = 
			     #socket_options{active = Active}}, _) when Active =/= false ->
    {keep_state_and_data, [{reply, From, {error, einval}}]};
handle_call({recv, N, Timeout}, RecvFrom, StateName, State, _) ->
    %% Doing renegotiate wait with handling request until renegotiate is
    %% finished. 
    Timer = start_or_recv_cancel_timer(Timeout, RecvFrom),
    {next_state, StateName, State#state{bytes_to_read = N, start_or_recv_from = RecvFrom,
					timer = Timer}, 
     [{next_event, internal, {recv, RecvFrom}}]};
handle_call({new_user, User}, From, StateName, 
		  State =#state{user_application = {OldMon, _}}, _) ->
    NewMon = erlang:monitor(process, User),
    erlang:demonitor(OldMon, [flush]),
    {next_state, StateName, State#state{user_application = {NewMon,User}},
     [{reply, From, ok}]};
handle_call({get_opts, OptTags}, From, _,
		  #state{socket = Socket,
			 transport_cb = Transport,
			 socket_options = SockOpts}, Connection) ->
    OptsReply = get_socket_opts(Connection, Transport, Socket, OptTags, SockOpts, []),
    {keep_state_and_data, [{reply, From, OptsReply}]};
handle_call({set_opts, Opts0}, From, StateName, 
	    #state{socket_options = Opts1, 
			 socket = Socket,
			 transport_cb = Transport} = State0, Connection) ->
    {Reply, Opts} = set_socket_opts(Connection, Transport, Socket, Opts0, Opts1, []),
    State = State0#state{socket_options = Opts},
    handle_active_option(Opts#socket_options.active, StateName, From, Reply, State);
    
handle_call(renegotiate, From, StateName, _, _) when StateName =/= connection ->
    {keep_state_and_data, [{reply, From, {error, already_renegotiating}}]};

handle_call(
  get_sslsocket, From, _StateName,
  #state{transport_cb = Transport, socket = Socket, tracker = Tracker},
  Connection) ->
    SslSocket =
        Connection:socket(self(), Transport, Socket, Connection, Tracker),
    {keep_state_and_data, [{reply, From, SslSocket}]};

handle_call({prf, Secret, Label, Seed, WantedLength}, From, _,
	    #state{connection_states = ConnectionStates,
		   negotiated_version = Version}, _) ->
    #{security_parameters := SecParams} =
	ssl_record:current_connection_state(ConnectionStates, read),
    #security_parameters{master_secret = MasterSecret,
			 client_random = ClientRandom,
			 server_random = ServerRandom,
			 prf_algorithm = PRFAlgorithm} = SecParams,
    Reply = try
		SecretToUse = case Secret of
				  _ when is_binary(Secret) -> Secret;
				  master_secret -> MasterSecret
			      end,
		SeedToUse = lists:reverse(
			      lists:foldl(fun(X, Acc) when is_binary(X) -> [X|Acc];
					     (client_random, Acc) -> [ClientRandom|Acc];
					     (server_random, Acc) -> [ServerRandom|Acc]
					  end, [], Seed)),
		ssl_handshake:prf(ssl:tls_version(Version), PRFAlgorithm, SecretToUse, Label, SeedToUse, WantedLength)
	    catch
		exit:_ -> {error, badarg};
		error:Reason -> {error, Reason}
	    end,
    {keep_state_and_data, [{reply, From, Reply}]};
handle_call(_,_,_,_,_) ->
    {keep_state_and_data, [postpone]}.

handle_info({ErrorTag, Socket, econnaborted}, StateName,  
	    #state{socket = Socket, transport_cb = Transport,
		   protocol_cb = Connection,
		   start_or_recv_from = StartFrom, role = Role,
		   error_tag = ErrorTag,
		   tracker = Tracker} = State)  when StateName =/= connection ->
    alert_user(Transport, Tracker,Socket, 
	       StartFrom, ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), Role, Connection),
    stop(normal, State);

handle_info({ErrorTag, Socket, Reason}, StateName, #state{socket = Socket,
							  error_tag = ErrorTag} = State)  ->
    Report = io_lib:format("SSL: Socket error: ~p ~n", [Reason]),
    error_logger:error_report(Report),
    handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
    stop(normal, State);

handle_info(
  {'DOWN', MonitorRef, _, _, Reason}, _,
  #state{
     user_application = {MonitorRef, _Pid},
     ssl_options = #ssl_options{erl_dist = true}}) ->
    {stop, {shutdown, Reason}};
handle_info(
  {'DOWN', MonitorRef, _, _, _}, _,
  #state{user_application = {MonitorRef, _Pid}}) ->
    {stop, normal};
handle_info(
  {'EXIT', Pid, _Reason}, StateName,
  #state{user_application = {_MonitorRef, Pid}} = State) ->
    %% It seems the user application has linked to us
    %% - ignore that and let the monitor handle this
    {next_state, StateName, State};

%%% So that terminate will be run when supervisor issues shutdown
handle_info({'EXIT', _Sup, shutdown}, _StateName, State) ->
    stop(shutdown, State);
handle_info({'EXIT', Socket, normal}, _StateName, #state{socket = Socket} = State) ->
    %% Handle as transport close"
    stop({shutdown, transport_closed}, State);
handle_info({'EXIT', Socket, Reason}, _StateName, #state{socket = Socket} = State) ->
    stop({shutdown, Reason}, State);

handle_info(allow_renegotiate, StateName, State) ->
    {next_state, StateName, State#state{allow_renegotiate = true}};

handle_info({cancel_start_or_recv, StartFrom}, StateName,
	    #state{renegotiation = {false, first}} = State) when StateName =/= connection ->
    stop_and_reply(
      {shutdown, user_timeout},
      {reply, StartFrom, {error, timeout}},
      State#state{timer = undefined});
handle_info({cancel_start_or_recv, RecvFrom}, StateName, 
	    #state{start_or_recv_from = RecvFrom} = State) when RecvFrom =/= undefined ->
    {next_state, StateName, State#state{start_or_recv_from = undefined,
					bytes_to_read = undefined,
					timer = undefined}, [{reply, RecvFrom, {error, timeout}}]};
handle_info({cancel_start_or_recv, _RecvFrom}, StateName, State) ->
    {next_state, StateName, State#state{timer = undefined}};

handle_info(Msg, StateName, #state{socket = Socket, error_tag = Tag} = State) ->
    Report = io_lib:format("SSL: Got unexpected info: ~p ~n", [{Msg, Tag, Socket}]),
    error_logger:info_report(Report),
    {next_state, StateName, State}.

%%====================================================================
%% general gen_statem callbacks
%%====================================================================
terminate(_, _, #state{terminated = true}) ->
    %% Happens when user closes the connection using ssl:close/1
    %% we want to guarantee that Transport:close has been called
    %% when ssl:close/1 returns unless it is a downgrade where
    %% we want to guarantee that close alert is received before
    %% returning. In both cases terminate has been run manually
    %% before run by gen_statem which will end up here
    ok;
terminate({shutdown, transport_closed} = Reason, 
	  _StateName, #state{protocol_cb = Connection,
			     socket = Socket, transport_cb = Transport} = State) ->
    handle_trusted_certs_db(State),
    Connection:close(Reason, Socket, Transport, undefined, undefined);
terminate({shutdown, own_alert}, _StateName, #state{%%send_queue = SendQueue, 
						protocol_cb = Connection,
						socket = Socket, 
						transport_cb = Transport} = State) ->
    handle_trusted_certs_db(State),
    case application:get_env(ssl, alert_timeout) of
	{ok, Timeout} when is_integer(Timeout) ->
	    Connection:close({timeout, Timeout}, Socket, Transport, undefined, undefined);
	_ ->
	    Connection:close({timeout, ?DEFAULT_TIMEOUT}, Socket, Transport, undefined, undefined)
    end;
terminate(Reason, connection, #state{negotiated_version = Version,
				     protocol_cb = Connection,
				     connection_states = ConnectionStates0, 
				     ssl_options = #ssl_options{padding_check = Check},
				     transport_cb = Transport, socket = Socket
				    } = State) ->
    handle_trusted_certs_db(State),
    {BinAlert, ConnectionStates} = terminate_alert(Reason, Version, ConnectionStates0, Connection),
    Connection:send(Transport, Socket, BinAlert),
    Connection:close(Reason, Socket, Transport, ConnectionStates, Check);
terminate(Reason, _StateName, #state{transport_cb = Transport, protocol_cb = Connection,
				     socket = Socket 
				    } = State) ->
    handle_trusted_certs_db(State),
    Connection:close(Reason, Socket, Transport, undefined, undefined).

format_status(normal, [_, StateName, State]) ->
    [{data, [{"State", {StateName, State}}]}];  
format_status(terminate, [_, StateName, State]) ->
    SslOptions = (State#state.ssl_options),
    NewOptions = SslOptions#ssl_options{password = ?SECRET_PRINTOUT,
					cert = ?SECRET_PRINTOUT,
					cacerts = ?SECRET_PRINTOUT,
					key = ?SECRET_PRINTOUT,			      
					dh = ?SECRET_PRINTOUT,
					psk_identity = ?SECRET_PRINTOUT,
					srp_identity = ?SECRET_PRINTOUT},
    [{data, [{"State", {StateName, State#state{connection_states = ?SECRET_PRINTOUT,
					       protocol_buffers =  ?SECRET_PRINTOUT,
					       user_data_buffer = ?SECRET_PRINTOUT,
					       tls_handshake_history =  ?SECRET_PRINTOUT,
					       session =  ?SECRET_PRINTOUT,
					       private_key =  ?SECRET_PRINTOUT,
					       diffie_hellman_params = ?SECRET_PRINTOUT,
					       diffie_hellman_keys =  ?SECRET_PRINTOUT,
					       srp_params = ?SECRET_PRINTOUT,
					       srp_keys =  ?SECRET_PRINTOUT,
					       premaster_secret =  ?SECRET_PRINTOUT,
					       ssl_options = NewOptions,
					       flight_buffer =  ?SECRET_PRINTOUT}
		       }}]}].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
connection_info(#state{sni_hostname = SNIHostname, 
		       session = #session{session_id = SessionId,
                                          cipher_suite = CipherSuite, ecc = ECCCurve},
		       protocol_cb = Connection,
		       negotiated_version =  {_,_} = Version, 
		       ssl_options = Opts}) ->
    RecordCB = record_cb(Connection),
    CipherSuiteDef = #{key_exchange := KexAlg} = ssl_cipher:suite_definition(CipherSuite),
    IsNamedCurveSuite = lists:member(KexAlg,
                                     [ecdh_ecdsa, ecdhe_ecdsa, ecdh_rsa, ecdh_anon]),
    CurveInfo = case ECCCurve of
		    {namedCurve, Curve} when IsNamedCurveSuite ->
			[{ecc, {named_curve, pubkey_cert_records:namedCurves(Curve)}}];
		    _ ->
			[]
		end,
    [{protocol, RecordCB:protocol_version(Version)},
     {session_id, SessionId},
     {cipher_suite, ssl_cipher:erl_suite_definition(CipherSuiteDef)},
     {selected_cipher_suite, CipherSuiteDef},
     {sni_hostname, SNIHostname} | CurveInfo] ++ ssl_options_list(Opts).

security_info(#state{connection_states = ConnectionStates}) ->
    #{security_parameters :=
	  #security_parameters{client_random = ClientRand, 
                               server_random = ServerRand,
                               master_secret = MasterSecret}} =
	ssl_record:current_connection_state(ConnectionStates, read),
    [{client_random, ClientRand}, {server_random, ServerRand}, {master_secret, MasterSecret}].

do_server_hello(Type, #hello_extensions{next_protocol_negotiation = NextProtocols} =
		    ServerHelloExt,
		#state{negotiated_version = Version,
		       session = #session{session_id = SessId},
		       connection_states = ConnectionStates0}
		= State0, Connection) when is_atom(Type) ->

    ServerHello =
	ssl_handshake:server_hello(SessId, ssl:tls_version(Version), ConnectionStates0, ServerHelloExt),
    State = server_hello(ServerHello,
			 State0#state{expecting_next_protocol_negotiation =
					  NextProtocols =/= undefined}, Connection),
    case Type of
	new ->
	    new_server_hello(ServerHello, State, Connection);
	resumed ->
	    resumed_server_hello(State, Connection)
    end.

new_server_hello(#server_hello{cipher_suite = CipherSuite,
			      compression_method = Compression,
			      session_id = SessionId},
		#state{session = Session0,
		       negotiated_version = Version} = State0, Connection) ->
    try server_certify_and_key_exchange(State0, Connection) of
        #state{} = State1 ->
            {State2, Actions} = server_hello_done(State1, Connection),
	    Session =
		Session0#session{session_id = SessionId,
				 cipher_suite = CipherSuite,
				 compression_method = Compression},
	    {Record, State} = Connection:next_record(State2#state{session = Session}),
	    Connection:next_event(certify, Record, State, Actions)
    catch
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, hello, State0)
    end.

resumed_server_hello(#state{session = Session,
			    connection_states = ConnectionStates0,
			    negotiated_version = Version} = State0, Connection) ->

    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, server) of
	{_, ConnectionStates1} ->
	    State1 = State0#state{connection_states = ConnectionStates1,
				  session = Session},
	    {State2, Actions} =
		finalize_handshake(State1, abbreviated, Connection),
	    {Record, State} = Connection:next_record(State2),
	    Connection:next_event(abbreviated, Record, State, Actions);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, hello, State0)
    end.

server_hello(ServerHello, State0, Connection) ->
    CipherSuite = ServerHello#server_hello.cipher_suite,
    #{key_exchange := KeyAlgorithm}  = ssl_cipher:suite_definition(CipherSuite),
    State = Connection:queue_handshake(ServerHello, State0),
    State#state{key_algorithm = KeyAlgorithm}.

server_hello_done(State, Connection) ->
    HelloDone = ssl_handshake:server_hello_done(),
    Connection:send_handshake(HelloDone, State).

handle_peer_cert(Role, PeerCert, PublicKeyInfo,
		 #state{session = #session{cipher_suite = CipherSuite} = Session} = State0,
		 Connection) ->
    State1 = State0#state{session =
			 Session#session{peer_certificate = PeerCert},
			 public_key_info = PublicKeyInfo},
    #{key_exchange := KeyAlgorithm} = ssl_cipher:suite_definition(CipherSuite),
    State2 = handle_peer_cert_key(Role, PeerCert, PublicKeyInfo, KeyAlgorithm, State1),

    {Record, State} = Connection:next_record(State2),
    Connection:next_event(certify, Record, State).

handle_peer_cert_key(client, _,
		     {?'id-ecPublicKey',  #'ECPoint'{point = _ECPoint} = PublicKey,
		      PublicKeyParams},
		     KeyAlg, #state{session = Session} = State)  when KeyAlg == ecdh_rsa;
                                                                      KeyAlg == ecdh_ecdsa ->
    ECDHKey = public_key:generate_key(PublicKeyParams),
    {namedCurve, Oid} = PublicKeyParams,
    Curve = pubkey_cert_records:namedCurves(Oid), %% Need API function
    PremasterSecret = ssl_handshake:premaster_secret(PublicKey, ECDHKey),
    master_secret(PremasterSecret, State#state{diffie_hellman_keys = ECDHKey,
                                               session = Session#session{ecc = {named_curve, Curve}}});
%% We do currently not support cipher suites that use fixed DH.
%% If we want to implement that the following clause can be used
%% to extract DH parameters form cert.
%% handle_peer_cert_key(client, _PeerCert, {?dhpublicnumber, PublicKey, PublicKeyParams},
%%                      {_,SignAlg},
%% 		        #state{diffie_hellman_keys = {_, MyPrivatKey}} = State) when
%%                                                                           SignAlg == dh_rsa;
%% 									     SignAlg == dh_dss ->
%%     dh_master_secret(PublicKeyParams, PublicKey, MyPrivatKey, State);
handle_peer_cert_key(_, _, _, _, State) ->
    State.

certify_client(#state{client_certificate_requested = true, role = client,
		      cert_db = CertDbHandle,
                      cert_db_ref = CertDbRef,
		      session = #session{own_certificate = OwnCert}}
	       = State, Connection) ->
    Certificate = ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, client),
    Connection:queue_handshake(Certificate, State);
certify_client(#state{client_certificate_requested = false} = State, _) ->
    State.

verify_client_cert(#state{client_certificate_requested = true, role = client,
			  negotiated_version = Version,
			  private_key = PrivateKey,
			  session = #session{master_secret = MasterSecret,
					     own_certificate = OwnCert},
			  cert_hashsign_algorithm = HashSign,
			  tls_handshake_history = Handshake0} = State, Connection) ->

    case ssl_handshake:client_certificate_verify(OwnCert, MasterSecret,
						 ssl:tls_version(Version), HashSign, PrivateKey, Handshake0) of
        #certificate_verify{} = Verified ->
           Connection:queue_handshake(Verified, State);
	ignore ->
	    State;
	#alert{} = Alert ->
	    throw(Alert)
    end;
verify_client_cert(#state{client_certificate_requested = false} = State, _) ->
    State.

client_certify_and_key_exchange(#state{negotiated_version = Version} =
				State0, Connection) ->
    try do_client_certify_and_key_exchange(State0, Connection) of
        State1 = #state{} ->
	    {State2, Actions} = finalize_handshake(State1, certify, Connection),
            State3 = State2#state{
		       %% Reinitialize
		       client_certificate_requested = false},
	    {Record, State} = Connection:next_record(State3),
	    Connection:next_event(cipher, Record, State, Actions)
    catch
        throw:#alert{} = Alert ->
	    handle_own_alert(Alert, Version, certify, State0)
    end.

do_client_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_client(State0, Connection),
    State2 = key_exchange(State1, Connection),
    verify_client_cert(State2, Connection).

server_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_server(State0, Connection),
    State2 = key_exchange(State1, Connection),
    request_client_cert(State2, Connection).

certify_client_key_exchange(#encrypted_premaster_secret{premaster_secret= EncPMS},
			    #state{private_key = Key, client_hello_version = {Major, Minor} = Version} = State, Connection) ->
    FakeSecret = make_premaster_secret(Version, rsa),
    %% Countermeasure for Bleichenbacher attack always provide some kind of premaster secret
    %% and fail handshake later.RFC 5246 section 7.4.7.1.
    PremasterSecret =
        try ssl_handshake:premaster_secret(EncPMS, Key) of
            Secret when erlang:byte_size(Secret) == ?NUM_OF_PREMASTERSECRET_BYTES ->
                case Secret of
                    <<?BYTE(Major), ?BYTE(Minor), Rest/binary>> -> %% Correct
                        <<?BYTE(Major), ?BYTE(Minor), Rest/binary>>;
                    <<?BYTE(_), ?BYTE(_), Rest/binary>> -> %% Version mismatch
                        <<?BYTE(Major), ?BYTE(Minor), Rest/binary>>
                end;
            _ -> %% erlang:byte_size(Secret) =/= ?NUM_OF_PREMASTERSECRET_BYTES
                FakeSecret
        catch 
            #alert{description = ?DECRYPT_ERROR} ->
                FakeSecret
        end,    
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);
certify_client_key_exchange(#client_diffie_hellman_public{dh_public = ClientPublicDhKey},
			    #state{diffie_hellman_params = #'DHParameter'{} = Params,
				   diffie_hellman_keys = {_, ServerDhPrivateKey}} = State,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientPublicDhKey, ServerDhPrivateKey, Params),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);

certify_client_key_exchange(#client_ec_diffie_hellman_public{dh_public = ClientPublicEcDhPoint},
			    #state{diffie_hellman_keys = ECDHKey} = State, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(#'ECPoint'{point = ClientPublicEcDhPoint}, ECDHKey),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);
certify_client_key_exchange(#client_psk_identity{} = ClientKey,
			    #state{ssl_options = 
				       #ssl_options{user_lookup_fun = PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);
certify_client_key_exchange(#client_dhe_psk_identity{} = ClientKey,
			    #state{diffie_hellman_params = #'DHParameter'{} = Params,
				   diffie_hellman_keys = {_, ServerDhPrivateKey},
				   ssl_options = 
				       #ssl_options{user_lookup_fun = PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = 
	ssl_handshake:premaster_secret(ClientKey, ServerDhPrivateKey, Params, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);
certify_client_key_exchange(#client_ecdhe_psk_identity{} = ClientKey,
			    #state{diffie_hellman_keys = ServerEcDhPrivateKey,
				   ssl_options =
				       #ssl_options{user_lookup_fun = PSKLookup}} = State,
			    Connection) ->
    PremasterSecret =
	ssl_handshake:premaster_secret(ClientKey, ServerEcDhPrivateKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);
certify_client_key_exchange(#client_rsa_psk_identity{} = ClientKey,
			    #state{private_key = Key,
				   ssl_options = 
				       #ssl_options{user_lookup_fun = PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);
certify_client_key_exchange(#client_srp_public{} = ClientKey,
			    #state{srp_params = Params,
				   srp_keys = Key
				  } = State0, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, Params),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher).

certify_server(#state{key_algorithm = Algo} = State, _) when Algo == dh_anon; 
							     Algo == ecdh_anon; 
							     Algo == psk; 
							     Algo == dhe_psk; 
							     Algo == ecdhe_psk; 
							     Algo == srp_anon  ->
    State;
certify_server(#state{cert_db = CertDbHandle,
		      cert_db_ref = CertDbRef,
		      session = #session{own_certificate = OwnCert}} = State, Connection) ->
    case ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, server) of
	Cert = #certificate{} ->
	    Connection:queue_handshake(Cert, State);
	Alert = #alert{} ->
	    throw(Alert)
    end.

key_exchange(#state{role = server, key_algorithm = rsa} = State,_) ->
    State;
key_exchange(#state{role = server, key_algorithm = Algo,
		    hashsign_algorithm = HashSignAlgo,
		    diffie_hellman_params = #'DHParameter'{} = Params,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == dhe_dss;
       Algo == dhe_rsa;
       Algo == dh_anon ->
    DHKeys = public_key:generate_key(Params),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, ssl:tls_version(Version), {dh, DHKeys, Params,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    State = Connection:queue_handshake(Msg, State0),
    State#state{diffie_hellman_keys = DHKeys};
key_exchange(#state{role = server, private_key = Key, key_algorithm = Algo} = State, _)
  when Algo == ecdh_ecdsa; Algo == ecdh_rsa ->
    State#state{diffie_hellman_keys = Key};
key_exchange(#state{role = server, key_algorithm = Algo,
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    session = #session{ecc = ECCCurve},
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == ecdhe_ecdsa; Algo == ecdhe_rsa;
       Algo == ecdh_anon ->

    ECDHKeys = public_key:generate_key(ECCCurve),
    #{security_parameters := SecParams} = 
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				      {ecdh, ECDHKeys,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    State = Connection:queue_handshake(Msg, State0),
    State#state{diffie_hellman_keys = ECDHKeys};
key_exchange(#state{role = server, key_algorithm = psk,
		    ssl_options = #ssl_options{psk_identity = undefined}} = State, _) ->
    State;
key_exchange(#state{role = server, key_algorithm = psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    #{security_parameters := SecParams} = 
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				     {psk, PskIdentityHint,
				      HashSignAlgo, ClientRandom,
				      ServerRandom,
						       PrivateKey}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{role = server, key_algorithm = dhe_psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    diffie_hellman_params = #'DHParameter'{} = Params,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    DHKeys = public_key:generate_key(Params),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				      {dhe_psk, 
				       PskIdentityHint, DHKeys, Params,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    State = Connection:queue_handshake(Msg, State0),
    State#state{diffie_hellman_keys = DHKeys};
key_exchange(#state{role = server, key_algorithm = ecdhe_psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
                    session = #session{ecc = ECCCurve},
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    ECDHKeys = public_key:generate_key(ECCCurve),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version),
				      {ecdhe_psk,
				       PskIdentityHint, ECDHKeys,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    State = Connection:queue_handshake(Msg, State0),
    State#state{diffie_hellman_keys = ECDHKeys};
key_exchange(#state{role = server, key_algorithm = rsa_psk,
		    ssl_options = #ssl_options{psk_identity = undefined}} = State, _) ->
    State;
key_exchange(#state{role = server, key_algorithm = rsa_psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				      {psk, PskIdentityHint,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{role = server, key_algorithm = Algo,
		    ssl_options = #ssl_options{user_lookup_fun = LookupFun},
		    hashsign_algorithm = HashSignAlgo,
		    session = #session{srp_username = Username},
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == srp_dss;
       Algo == srp_rsa;
       Algo == srp_anon ->
    SrpParams = handle_srp_identity(Username, LookupFun),
    Keys = case generate_srp_server_keys(SrpParams, 0) of
	       Alert = #alert{} ->
		   throw(Alert);
	       Keys0 = {_,_} ->
		   Keys0
	   end,
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version),
				      {srp, Keys, SrpParams,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    State = Connection:queue_handshake(Msg, State0),
    State#state{srp_params = SrpParams,
		srp_keys = Keys};
key_exchange(#state{role = client,
		    key_algorithm = rsa,
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    premaster_secret = PremasterSecret} = State0, Connection) ->
    Msg = rsa_key_exchange(ssl:tls_version(Version), PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    diffie_hellman_keys = {DhPubKey, _}
		   } = State0, Connection)
  when Algorithm == dhe_dss;
       Algorithm == dhe_rsa;
       Algorithm == dh_anon ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), {dh, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    diffie_hellman_keys = Keys} = State0, Connection)
  when Algorithm == ecdhe_ecdsa; Algorithm == ecdhe_rsa;
       Algorithm == ecdh_ecdsa; Algorithm == ecdh_rsa;
       Algorithm == ecdh_anon ->
    Msg = ssl_handshake:key_exchange(client, ssl:tls_version(Version), {ecdh, Keys}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = psk,
		    negotiated_version = Version} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), 
				      {psk, SslOpts#ssl_options.psk_identity}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = dhe_psk,
		    negotiated_version = Version,
		    diffie_hellman_keys = {DhPubKey, _}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {dhe_psk, 
				       SslOpts#ssl_options.psk_identity, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = ecdhe_psk,
		    negotiated_version = Version,
		    diffie_hellman_keys = ECDHKeys} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {ecdhe_psk,
				       SslOpts#ssl_options.psk_identity, ECDHKeys}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = rsa_psk,
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    premaster_secret = PremasterSecret}
	     = State0, Connection) ->
    Msg = rsa_psk_key_exchange(ssl:tls_version(Version), SslOpts#ssl_options.psk_identity,
			       PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    srp_keys = {ClientPubKey, _}}
	     = State0, Connection)
  when Algorithm == srp_dss;
       Algorithm == srp_rsa;
       Algorithm == srp_anon ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), {srp, ClientPubKey}),
    Connection:queue_handshake(Msg, State0).

rsa_key_exchange(Version, PremasterSecret, PublicKeyInfo = {Algorithm, _, _})
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption;
       Algorithm == ?sha224WithRSAEncryption;
       Algorithm == ?sha256WithRSAEncryption;
       Algorithm == ?sha384WithRSAEncryption;
       Algorithm == ?sha512WithRSAEncryption
       ->
    ssl_handshake:key_exchange(client, ssl:tls_version(Version),
			       {premaster_secret, PremasterSecret,
				PublicKeyInfo});
rsa_key_exchange(_, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, pub_key_is_not_rsa)).

rsa_psk_key_exchange(Version, PskIdentity, PremasterSecret, 
		     PublicKeyInfo = {Algorithm, _, _})
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption;
       Algorithm == ?sha224WithRSAEncryption;
       Algorithm == ?sha256WithRSAEncryption;
       Algorithm == ?sha384WithRSAEncryption;
       Algorithm == ?sha512WithRSAEncryption
       ->
    ssl_handshake:key_exchange(client, ssl:tls_version(Version),
			       {psk_premaster_secret, PskIdentity, PremasterSecret,
				PublicKeyInfo});
rsa_psk_key_exchange(_, _, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, pub_key_is_not_rsa)).

request_client_cert(#state{key_algorithm = Alg} = State, _)
  when Alg == dh_anon; Alg == ecdh_anon;
       Alg == psk; Alg == dhe_psk; Alg == ecdhe_psk; Alg == rsa_psk;
       Alg == srp_dss; Alg == srp_rsa; Alg == srp_anon ->
    State;

request_client_cert(#state{ssl_options = #ssl_options{verify = verify_peer, 
						      signature_algs = SupportedHashSigns},
			   connection_states = ConnectionStates0,
			   cert_db = CertDbHandle,
			   cert_db_ref = CertDbRef,
			   negotiated_version = Version} = State0, Connection) ->
    #{security_parameters :=
	  #security_parameters{cipher_suite = CipherSuite}} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    TLSVersion =  ssl:tls_version(Version),
    HashSigns = ssl_handshake:available_signature_algs(SupportedHashSigns, 
						       TLSVersion),
    Msg = ssl_handshake:certificate_request(CipherSuite, CertDbHandle, CertDbRef, 
					    HashSigns, TLSVersion),
    State = Connection:queue_handshake(Msg, State0),
    State#state{client_certificate_requested = true};

request_client_cert(#state{ssl_options = #ssl_options{verify = verify_none}} =
		    State, _) ->
    State.

calculate_master_secret(PremasterSecret, 
			#state{negotiated_version = Version,
			       connection_states = ConnectionStates0,
			       session = Session0} = State0, Connection,
			_Current, Next) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, server) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State1 = State0#state{connection_states = ConnectionStates,
				  session = Session},
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_event(Next, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, certify, State0)
    end.

finalize_handshake(State0, StateName, Connection) ->
    #state{connection_states = ConnectionStates0} =
	State1 = cipher_protocol(State0, Connection),

    ConnectionStates =
        ssl_record:activate_pending_connection_state(ConnectionStates0,
                                                     write, Connection),

    State2 = State1#state{connection_states = ConnectionStates},
    State = next_protocol(State2, Connection),
    finished(State, StateName, Connection).

next_protocol(#state{role = server} = State, _) ->
    State;
next_protocol(#state{negotiated_protocol = undefined} = State, _) ->
    State;
next_protocol(#state{expecting_next_protocol_negotiation = false} = State, _) ->
    State;
next_protocol(#state{negotiated_protocol = NextProtocol} = State0, Connection) ->
    NextProtocolMessage = ssl_handshake:next_protocol(NextProtocol),
    Connection:queue_handshake(NextProtocolMessage, State0).

cipher_protocol(State, Connection) ->
    Connection:queue_change_cipher(#change_cipher_spec{}, State).

finished(#state{role = Role, negotiated_version = Version,
		session = Session,
                connection_states = ConnectionStates0,
                tls_handshake_history = Handshake0} = State0, StateName, Connection) ->
    MasterSecret = Session#session.master_secret,
    Finished = ssl_handshake:finished(ssl:tls_version(Version), Role,
				       get_current_prf(ConnectionStates0, write),
				       MasterSecret, Handshake0),
    ConnectionStates = save_verify_data(Role, Finished, ConnectionStates0, StateName),
    Connection:send_handshake(Finished, State0#state{connection_states =
								 ConnectionStates}).

save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, certify) ->
    ssl_record:set_client_verify_data(current_write, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, cipher) ->
    ssl_record:set_server_verify_data(current_both, Data, ConnectionStates);
save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_client_verify_data(current_both, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_server_verify_data(current_write, Data, ConnectionStates).

calculate_secret(#server_dh_params{dh_p = Prime, dh_g = Base, 
				   dh_y = ServerPublicDhKey} = Params,
		 State, Connection) ->
    Keys = {_, PrivateDhKey} = crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret =
	ssl_handshake:premaster_secret(ServerPublicDhKey, PrivateDhKey, Params),
    calculate_master_secret(PremasterSecret,
			    State#state{diffie_hellman_keys = Keys}, 
			    Connection, certify, certify);

calculate_secret(#server_ecdh_params{curve = ECCurve, public = ECServerPubKey},
		     State=#state{session=Session}, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),
    PremasterSecret = 
	ssl_handshake:premaster_secret(#'ECPoint'{point = ECServerPubKey}, ECDHKeys),
    calculate_master_secret(PremasterSecret,
			    State#state{diffie_hellman_keys = ECDHKeys,
					session = Session#session{ecc = ECCurve}},
			    Connection, certify, certify);

calculate_secret(#server_psk_params{
		    hint = IdentityHint},
		 State0, Connection) ->
    %% store for later use
    {Record, State} = Connection:next_record(State0#state{psk_identity = IdentityHint}),
    Connection:next_event(certify, Record, State);

calculate_secret(#server_dhe_psk_params{
		    dh_params = #server_dh_params{dh_p = Prime, dh_g = Base}} = ServerKey,
		    #state{ssl_options = #ssl_options{user_lookup_fun = PSKLookup}} = 
		     State, Connection) ->
    Keys = {_, PrivateDhKey} =
	crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, PrivateDhKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State#state{diffie_hellman_keys = Keys},
			    Connection, certify, certify);

calculate_secret(#server_ecdhe_psk_params{
                    dh_params = #server_ecdh_params{curve = ECCurve}} = ServerKey,
                 #state{ssl_options = #ssl_options{user_lookup_fun = PSKLookup}} = 
		     State=#state{session=Session}, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),

    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, ECDHKeys, PSKLookup),
    calculate_master_secret(PremasterSecret,
			    State#state{diffie_hellman_keys = ECDHKeys,
					session = Session#session{ecc = ECCurve}},
			    Connection, certify, certify);

calculate_secret(#server_srp_params{srp_n = Prime, srp_g = Generator} = ServerKey,
		 #state{ssl_options = #ssl_options{srp_identity = SRPId}} = State, 
		 Connection) ->
    Keys = generate_srp_client_keys(Generator, Prime, 0),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, Keys, SRPId),
    calculate_master_secret(PremasterSecret, State#state{srp_keys = Keys}, Connection, 
			    certify, certify).

master_secret(#alert{} = Alert, _) ->
    Alert;
master_secret(PremasterSecret, #state{session = Session,
				      negotiated_version = Version, role = Role,
				      connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, Role) of
	{MasterSecret, ConnectionStates} ->
	    State#state{
	      session =
		  Session#session{master_secret = MasterSecret},
	      connection_states = ConnectionStates};
	#alert{} = Alert ->
	    Alert
    end.

generate_srp_server_keys(_SrpParams, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_server_keys(SrpParams =
			     #srp_user{generator = Generator, prime = Prime,
				       verifier = Verifier}, N) ->
    case crypto:generate_key(srp, {host, [Verifier, Generator, Prime, '6a']}) of
	error ->
	    generate_srp_server_keys(SrpParams, N+1);
	Keys ->
	    Keys
    end.

generate_srp_client_keys(_Generator, _Prime, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_client_keys(Generator, Prime, N) ->

    case crypto:generate_key(srp, {user, [Generator, Prime, '6a']}) of
	error ->
	    generate_srp_client_keys(Generator, Prime, N+1);
	Keys ->
	    Keys
    end.

handle_srp_identity(Username, {Fun, UserState}) ->
    case Fun(srp, Username, UserState) of
	{ok, {SRPParams, Salt, DerivedKey}}
	  when is_atom(SRPParams), is_binary(Salt), is_binary(DerivedKey) ->
	    {Generator, Prime} = ssl_srp_primes:get_srp_params(SRPParams),
	    Verifier = crypto:mod_pow(Generator, DerivedKey, Prime),
	    #srp_user{generator = Generator, prime = Prime,
		      salt = Salt, verifier = Verifier};
	#alert{} = Alert ->
	    throw(Alert);
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end.


cipher_role(client, Data, Session, #state{connection_states = ConnectionStates0} = State0,
	    Connection) ->
    ConnectionStates = ssl_record:set_server_verify_data(current_both, Data, 
							 ConnectionStates0),
     {Record, State} = prepare_connection(State0#state{session = Session,
						       connection_states = ConnectionStates},
					 Connection),
    Connection:next_event(connection, Record, State);
cipher_role(server, Data, Session,  #state{connection_states = ConnectionStates0} = State0,
	    Connection) ->
    ConnectionStates1 = ssl_record:set_client_verify_data(current_read, Data, 
							  ConnectionStates0),
    {State1, Actions} =
	finalize_handshake(State0#state{connection_states = ConnectionStates1,
					session = Session}, cipher, Connection),
    {Record, State} = prepare_connection(State1, Connection),
    Connection:next_event(connection, Record, State, Actions).

is_anonymous(Algo) when Algo == dh_anon;
			Algo == ecdh_anon;
			Algo == psk;
			Algo == dhe_psk;
			Algo == ecdhe_psk;
			Algo == rsa_psk;
			Algo == srp_anon ->
    true;
is_anonymous(_) ->
    false.

get_current_prf(CStates, Direction) ->
    #{security_parameters := SecParams} = ssl_record:current_connection_state(CStates, Direction),
    SecParams#security_parameters.prf_algorithm.
get_pending_prf(CStates, Direction) ->
    #{security_parameters := SecParams} = ssl_record:pending_connection_state(CStates, Direction),
    SecParams#security_parameters.prf_algorithm.

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

record_cb(tls_connection) ->
    tls_record;
record_cb(dtls_connection) ->
    dtls_record.

call(FsmPid, Event) ->
    try gen_statem:call(FsmPid, Event)
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{{shutdown, _},_} ->
	    {error, closed}
    end.

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
get_socket_opts(Connection, Transport, Socket, [Tag | Tags], SockOpts, Acc) ->
    case Connection:getopts(Transport, Socket, [Tag]) of
        {ok, [Opt]} ->
            get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, [Opt | Acc]);
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
set_socket_opts(ConnectionCb, Transport, Socket, [{active, Active}| Opts], SockOpts, Other) 
  when Active == once;
       Active == true;
       Active == false ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, 
		    SockOpts#socket_options{active = Active}, Other);
set_socket_opts(_,_, _, [{active, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}} }, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [Opt | Opts], SockOpts, Other) ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, SockOpts, [Opt | Other]).



hibernate_after(connection = StateName, 
		#state{ssl_options=#ssl_options{hibernate_after = HibernateAfter}} = State,
		Actions) ->
    {next_state, StateName, State, [{timeout, HibernateAfter, hibernate} | Actions]};
hibernate_after(StateName, State, Actions) ->
    {next_state, StateName, State, Actions}.

map_extensions(#hello_extensions{renegotiation_info = RenegotiationInfo,
                                 signature_algs = SigAlg,          
                                 alpn = Alpn,
                                 next_protocol_negotiation = Next, 
                                 srp = SRP,
                                 ec_point_formats = ECPointFmt,
                                 elliptic_curves = ECCCurves,
                                 sni = SNI}) ->
    #{renegotiation_info => ssl_handshake:extension_value(RenegotiationInfo),
      signature_algs =>  ssl_handshake:extension_value(SigAlg),
      alpn =>  ssl_handshake:extension_value(Alpn),
      srp  => ssl_handshake:extension_value(SRP),
      next_protocol => ssl_handshake:extension_value(Next),
      ec_point_formats  => ssl_handshake:extension_value(ECPointFmt),
      elliptic_curves => ssl_handshake:extension_value(ECCCurves),
      sni => ssl_handshake:extension_value(SNI)}.

terminate_alert(normal, Version, ConnectionStates, Connection)  ->
    Connection:encode_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
		     Version, ConnectionStates);
terminate_alert({Reason, _}, Version, ConnectionStates, Connection) when Reason == close;
									 Reason == shutdown ->
    Connection:encode_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
		     Version, ConnectionStates);

terminate_alert(_, Version, ConnectionStates, Connection) ->
    {BinAlert, _} = Connection:encode_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR),
					    Version, ConnectionStates),
    BinAlert.

handle_trusted_certs_db(#state{ssl_options = 
				   #ssl_options{cacertfile = <<>>, cacerts = []}}) ->
    %% No trusted certs specified
    ok;
handle_trusted_certs_db(#state{cert_db_ref = Ref,
			       cert_db = CertDb,
			       ssl_options = #ssl_options{cacertfile = <<>>}}) when CertDb =/= undefined ->
    %% Certs provided as DER directly can not be shared
    %% with other connections and it is safe to delete them when the connection ends.
    ssl_pkix_db:remove_trusted_certs(Ref, CertDb);
handle_trusted_certs_db(#state{file_ref_db = undefined}) ->
    %% Something went wrong early (typically cacertfile does not
    %% exist) so there is nothing to handle
    ok;
handle_trusted_certs_db(#state{cert_db_ref = Ref,
			       file_ref_db = RefDb,
			       ssl_options = #ssl_options{cacertfile = File}}) ->
    case ssl_pkix_db:ref_count(Ref, RefDb, -1) of
	0 ->
	    ssl_manager:clean_cert_db(Ref, File);
	_ ->
	    ok
    end.

prepare_connection(#state{renegotiation = Renegotiate, 
			  start_or_recv_from = RecvFrom} = State0, Connection) 
  when Renegotiate =/= {false, first}, 
       RecvFrom =/= undefined ->
    State1 = Connection:reinit_handshake_data(State0),   
    {Record, State} = Connection:next_record(State1),
    {Record, ack_connection(State)};
prepare_connection(State0, Connection) ->
    State = Connection:reinit_handshake_data(State0),
    {no_record, ack_connection(State)}.

ack_connection(#state{renegotiation = {true, Initiater}} = State) 
  when Initiater == internal;
       Initiater == peer ->
    State#state{renegotiation = undefined};
ack_connection(#state{renegotiation = {true, From}} = State) ->    
    gen_statem:reply(From, ok),
    State#state{renegotiation = undefined};
ack_connection(#state{renegotiation = {false, first}, 
		      start_or_recv_from = StartFrom,
		      timer = Timer} = State) when StartFrom =/= undefined ->
    gen_statem:reply(StartFrom, connected),
    cancel_timer(Timer),
    State#state{renegotiation = undefined, 
		start_or_recv_from = undefined, timer = undefined};
ack_connection(State) ->
    State.

cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    ok.

register_session(client, Host, Port, #session{is_resumable = new} = Session0) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Host, Port, Session),
    Session;
register_session(server, _, Port, #session{is_resumable = new} = Session0) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Port, Session),
    Session;
register_session(_, _, _, Session) ->
    Session. %% Already registered

host_id(client, _Host, #ssl_options{server_name_indication = Hostname}) when is_list(Hostname) ->
    Hostname;
host_id(_, Host, _) ->
    Host.

handle_new_session(NewId, CipherSuite, Compression, 
		   #state{session = Session0,
			  protocol_cb = Connection} = State0) ->
    Session = Session0#session{session_id = NewId,
			       cipher_suite = CipherSuite,
			       compression_method = Compression},
    {Record, State} = Connection:next_record(State0#state{session = Session}),
    Connection:next_event(certify, Record, State).

handle_resumed_session(SessId, #state{connection_states = ConnectionStates0,
				      negotiated_version = Version,
				      host = Host, port = Port,
				      protocol_cb = Connection,
				      session_cache = Cache,
				      session_cache_cb = CacheCb} = State0) ->
    Session = CacheCb:lookup(Cache, {{Host, Port}, SessId}),
    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, client) of
	{_, ConnectionStates} ->
	    {Record, State} =
		Connection:next_record(State0#state{
			      connection_states = ConnectionStates,
			      session = Session}),
	    Connection:next_event(abbreviated, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, hello, State0)
    end.

make_premaster_secret({MajVer, MinVer}, rsa) ->
    Rand = ssl_cipher:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>;
make_premaster_secret(_, _) ->
    undefined.

negotiated_hashsign(undefined, KexAlg, PubKeyInfo, Version) ->
    %% Not negotiated choose default 
    case is_anonymous(KexAlg) of
	true ->
	    {null, anon};
	false ->
	    {PubAlg, _, _} = PubKeyInfo,
	    ssl_handshake:select_hashsign_algs(undefined, PubAlg, Version)
    end;
negotiated_hashsign(HashSign = {_, _}, _, _, _) ->
    HashSign.

ssl_options_list(SslOptions) ->
    Fileds = record_info(fields, ssl_options),
    Values = tl(tuple_to_list(SslOptions)),
    ssl_options_list(Fileds, Values, []).

ssl_options_list([],[], Acc) ->
    lists:reverse(Acc);
%% Skip internal options, only return user options
ssl_options_list([protocol | Keys], [_ | Values], Acc) ->
    ssl_options_list(Keys, Values, Acc);
ssl_options_list([erl_dist | Keys], [_ | Values], Acc) ->
    ssl_options_list(Keys, Values, Acc);
ssl_options_list([renegotiate_at | Keys], [_ | Values], Acc) ->
    ssl_options_list(Keys, Values, Acc);
ssl_options_list([ciphers = Key | Keys], [Value | Values], Acc) ->
   ssl_options_list(Keys, Values, 
		    [{Key, lists:map(
			     fun(Suite) -> 
				     ssl_cipher:suite_definition(Suite) 
			     end, Value)} 
		     | Acc]);
ssl_options_list([Key | Keys], [Value | Values], Acc) ->
   ssl_options_list(Keys, Values, [{Key, Value} | Acc]).

handle_active_option(false, connection = StateName, To, Reply, State) ->
    hibernate_after(StateName, State, [{reply, To, Reply}]);

handle_active_option(_, connection = StateName0, To, Reply, #state{protocol_cb = Connection,
							      user_data_buffer = <<>>} = State0) ->
    %% Need data, set active once
    {Record, State1} = Connection:next_record_if_active(State0),
    %% Note: Renogotiation may cause StateName0 =/= StateName
    case Connection:next_event(StateName0, Record, State1) of
	{next_state, StateName, State} ->
	    hibernate_after(StateName, State, [{reply, To, Reply}]);
	{next_state, StateName, State, Actions} -> 
	    hibernate_after(StateName, State, [{reply, To, Reply} | Actions]);
	{stop, _, _} = Stop ->
	    Stop
    end;
handle_active_option(_, StateName, To, Reply, #state{user_data_buffer = <<>>} = State) ->
    %% Active once already set 
    {next_state, StateName, State, [{reply, To, Reply}]};

%% user_data_buffer =/= <<>>
handle_active_option(_, StateName0, To, Reply, #state{protocol_cb = Connection} = State0) -> 
    case read_application_data(<<>>, State0) of
	{stop, _, _} = Stop ->
	    Stop;
	{Record, State1} ->
	    %% Note: Renogotiation may cause StateName0 =/= StateName
	    case Connection:next_event(StateName0, Record, State1) of
		{next_state, StateName, State} ->
		    hibernate_after(StateName, State, [{reply, To, Reply}]);
		{next_state, StateName, State, Actions} -> 
		    hibernate_after(StateName, State, [{reply, To, Reply} | Actions]);
		{stop, _, _} = Stop ->
		    Stop
	    end
    end.

encode_packet(Data, #socket_options{packet=Packet}) ->
    case Packet of
	1 -> encode_size_packet(Data, 8,  (1 bsl 8) - 1);
	2 -> encode_size_packet(Data, 16, (1 bsl 16) - 1);
	4 -> encode_size_packet(Data, 32, (1 bsl 32) - 1);
	_ -> Data
    end.

encode_size_packet(Bin, Size, Max) ->
    Len = erlang:byte_size(Bin),
    case Len > Max of
	true  -> throw({error, {badarg, {packet_to_large, Len, Max}}});
	false -> <<Len:Size, Bin/binary>>
    end.

time_to_renegotiate(_Data, 
		    #{current_write := #{sequence_number := Num}}, 
		    RenegotiateAt) ->
    
    %% We could do test:
    %% is_time_to_renegotiate((erlang:byte_size(_Data) div ?MAX_PLAIN_TEXT_LENGTH) + 1, RenegotiateAt),
    %% but we chose to have a some what lower renegotiateAt and a much cheaper test 
    is_time_to_renegotiate(Num, RenegotiateAt).

is_time_to_renegotiate(N, M) when N < M->
    false;
is_time_to_renegotiate(_,_) ->
    true.


%% Picks ClientData 
get_data(_, _, <<>>) ->
    {more, <<>>};
%% Recv timed out save buffer data until next recv
get_data(#socket_options{active=false}, undefined, Buffer) ->
    {passive, Buffer};
get_data(#socket_options{active=Active, packet=Raw}, BytesToRead, Buffer) 
  when Raw =:= raw; Raw =:= 0 ->   %% Raw Mode
    if 
	Active =/= false orelse BytesToRead =:= 0  ->
	    %% Active true or once, or passive mode recv(0)  
	    {ok, Buffer, <<>>};
	byte_size(Buffer) >= BytesToRead ->  
	    %% Passive Mode, recv(Bytes) 
	    <<Data:BytesToRead/binary, Rest/binary>> = Buffer,
	    {ok, Data, Rest};
	true ->
	    %% Passive Mode not enough data
	    {more, Buffer}
    end;
get_data(#socket_options{packet=Type, packet_size=Size}, _, Buffer) ->
    PacketOpts = [{packet_size, Size}], 
    case decode_packet(Type, Buffer, PacketOpts) of
	{more, _} ->
	    {more, Buffer};
	Decoded ->
	    Decoded
    end.

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
deliver_app_data(Transport, Socket, SOpts = #socket_options{active=Active, packet=Type},
		 Data, Pid, From, Tracker, Connection) ->
    send_or_reply(Active, Pid, From, format_reply(Transport, Socket, SOpts, Data, Tracker, Connection)),
    SO = case Data of
	     {P, _, _, _} when ((P =:= http_request) or (P =:= http_response)),
			       ((Type =:= http) or (Type =:= http_bin)) ->
	         SOpts#socket_options{packet={Type, headers}};
	     http_eoh when tuple_size(Type) =:= 2 ->
                 % End of headers - expect another Request/Response line
	         {Type1, headers} = Type,
	         SOpts#socket_options{packet=Type1};
	     _ ->
	         SOpts
	 end,
    case Active of
        once ->
            SO#socket_options{active=false};
	_ ->
	    SO
    end.

format_reply(_, _,#socket_options{active = false, mode = Mode, packet = Packet,
				  header = Header}, Data, _, _) ->
    {ok, do_format_reply(Mode, Packet, Header, Data)};
format_reply(Transport, Socket, #socket_options{active = _, mode = Mode, packet = Packet,
						header = Header}, Data, Tracker, Connection) ->
    {ssl, Connection:socket(self(), Transport, Socket, Connection, Tracker), 
     do_format_reply(Mode, Packet, Header, Data)}.

deliver_packet_error(Transport, Socket, SO= #socket_options{active = Active}, Data, Pid, From, Tracker, Connection) ->
    send_or_reply(Active, Pid, From, format_packet_error(Transport, Socket, SO, Data, Tracker, Connection)).

format_packet_error(_, _,#socket_options{active = false, mode = Mode}, Data, _, _) ->
    {error, {invalid_packet, do_format_reply(Mode, raw, 0, Data)}};
format_packet_error(Transport, Socket, #socket_options{active = _, mode = Mode}, Data, Tracker, Connection) ->
    {ssl_error, Connection:socket(self(), Transport, Socket, Connection, Tracker), 
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
%% Can happen when handling own alert or tcp error/close and there is
%% no outstanding gen_fsm sync events
send_or_reply(false, no_pid, _, _) ->
    ok;
send_or_reply(_, Pid, _From, Data) ->
    send_user(Pid, Data).

send_user(Pid, Msg) ->
    Pid ! Msg,
    ok.

alert_user(Transport, Tracker, Socket, connection, Opts, Pid, From, Alert, Role, Connection) ->
    alert_user(Transport, Tracker, Socket, Opts#socket_options.active, Pid, From, Alert, Role, Connection);
alert_user(Transport, Tracker, Socket,_, _, _, From, Alert, Role, Connection) ->
    alert_user(Transport, Tracker, Socket, From, Alert, Role, Connection).

alert_user(Transport, Tracker, Socket, From, Alert, Role, Connection) ->
    alert_user(Transport, Tracker, Socket, false, no_pid, From, Alert, Role, Connection).

alert_user(_, _, _, false = Active, Pid, From,  Alert, Role, _) when From =/= undefined ->
    %% If there is an outstanding ssl_accept | recv
    %% From will be defined and send_or_reply will
    %% send the appropriate error message.
    ReasonCode = ssl_alert:reason_code(Alert, Role),
    send_or_reply(Active, Pid, From, {error, ReasonCode});
alert_user(Transport, Tracker, Socket, Active, Pid, From, Alert, Role, Connection) ->
    case ssl_alert:reason_code(Alert, Role) of
	closed ->
	    send_or_reply(Active, Pid, From,
			  {ssl_closed, Connection:socket(self(), 
							 Transport, Socket, Connection, Tracker)});
	ReasonCode ->
	    send_or_reply(Active, Pid, From,
			  {ssl_error, Connection:socket(self(), 
							Transport, Socket, Connection, Tracker), ReasonCode})
    end.

log_alert(true, Role, ProtocolName, StateName, #alert{role = Role} = Alert) ->
    Txt = ssl_alert:own_alert_txt(Alert),
    error_logger:info_report(io_lib:format("~s ~p: In state ~p ~s\n", [ProtocolName, Role, StateName, Txt]));
log_alert(true, Role, ProtocolName, StateName, Alert) ->
    Txt = ssl_alert:alert_txt(Alert),
    error_logger:info_report(io_lib:format("~s ~p: In state ~p ~s\n", [ProtocolName, Role, StateName, Txt]));
log_alert(false, _, _, _, _) ->
    ok.

invalidate_session(client, Host, Port, Session) ->
    ssl_manager:invalidate_session(Host, Port, Session);
invalidate_session(server, _, Port, Session) ->
    ssl_manager:invalidate_session(Port, Session).

handle_sni_extension(undefined, State) ->
    State;
handle_sni_extension(#sni{hostname = Hostname}, State0) ->
    NewOptions = update_ssl_options_from_sni(State0#state.ssl_options, Hostname),
    case NewOptions of
	undefined ->
	    State0;
	_ ->
	    {ok, #{cert_db_ref := Ref, 
                   cert_db_handle := CertDbHandle, 
                   fileref_db_handle := FileRefHandle, 
                   session_cache := CacheHandle, 
                   crl_db_info := CRLDbHandle,
                   private_key := Key,
                   dh_params := DHParams,
                   own_certificate := OwnCert}} =
                 ssl_config:init(NewOptions, State0#state.role),
             State0#state{
               session = State0#state.session#session{own_certificate = OwnCert},
               file_ref_db = FileRefHandle,
               cert_db_ref = Ref,
               cert_db = CertDbHandle,
               crl_db = CRLDbHandle,
               session_cache = CacheHandle,
               private_key = Key,
	      diffie_hellman_params = DHParams,
	      ssl_options = NewOptions,
	      sni_hostname = Hostname
	     }
    end.

update_ssl_options_from_sni(OrigSSLOptions, SNIHostname) ->
    SSLOption = 
	case OrigSSLOptions#ssl_options.sni_fun of
	    undefined ->
		proplists:get_value(SNIHostname, 
				    OrigSSLOptions#ssl_options.sni_hosts);
	    SNIFun ->
		SNIFun(SNIHostname)
	end,
    case SSLOption of
        undefined ->
            undefined;
        _ ->
            ssl:handle_options(SSLOption, OrigSSLOptions)
    end.

new_emulated([], EmOpts) ->
    EmOpts;
new_emulated(NewEmOpts, _) ->
    NewEmOpts.
%%---------------Erlang distribution --------------------------------------

send_dist_data(StateName, State, DHandle, Acc) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DHandle),
            hibernate_after(StateName, State, lists:reverse(Acc));
        Data ->
            send_dist_data(
              StateName, State, DHandle,
              [{next_event, {call, {self(), undefined}}, {application_data, Data}}
               |Acc])
    end.

%% Overload mitigation
eat_msgs(Msg) ->
    receive Msg -> eat_msgs(Msg)
    after 0 -> ok
    end.

%% When acting as distribution controller map the exit reason
%% to follow the documented nodedown_reason for net_kernel
stop(Reason, State) ->
    {stop, erl_dist_stop_reason(Reason, State), State}.

stop_and_reply(Reason, Replies, State) ->
    {stop_and_reply, erl_dist_stop_reason(Reason, State), Replies, State}.

erl_dist_stop_reason(
  Reason, #state{ssl_options = #ssl_options{erl_dist = true}}) ->
    case Reason of
        normal ->
            %% We can not exit with normal since that will not bring
            %% down the rest of the distribution processes
            {shutdown, normal};
        _ -> Reason
    end;
erl_dist_stop_reason(Reason, _State) ->
    Reason.
