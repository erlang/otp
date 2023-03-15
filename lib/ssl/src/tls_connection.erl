%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2023. All Rights Reserved.
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
%% Purpose: TLS-1.0-TLS-1.2 FSM (* = optional)
%% %%----------------------------------------------------------------------
%%                    TLS Handshake protocol full Handshake 
%%  Client                                               Server
%%
%%       ClientHello                  -------->                       Flight 1
%%                                                       ServerHello  \
%%                                                      Certificate*   \
%%                                                ServerKeyExchange*    Flight 2
%%                                               CertificateRequest*   / 
%%                                    <--------      ServerHelloDone  /
%%       Certificate*                                                 \
%%       ClientKeyExchange                                             \
%%       CertificateVerify*                                             Flight 3 part 1
%%       [ChangeCipherSpec]                                            / 
%%       NextProtocol*
%%       Finished                     -------->                       / Flight 3 part 2
%%                                                [ChangeCipherSpec]  
%%                                    <--------             Finished Flight 4
%%       Application Data             <------->     Application Data
%%
%%
%%                    TLS Handshake protocol abbreviated Handshake 
%%    Client                                                Server
%%
%%       ClientHello                   -------->                       Abbrev Flight 1
%%                                                        ServerHello  Abbrev Flight 2 part 1
%%                                                 [ChangeCipherSpec]
%%                                     <--------             Finished  Abbrev Flight 2 part 2
%%       [ChangeCipherSpec]
%%       NextProtocol*
%%       Finished                      -------->                       Abbrev Flight 3
%%       Application Data              <------->     Application Data
%%
%%                                            
%%                                                
%%                                       Start FSM    ---> CONFIG_ERROR          
%%                                                     Send error to user
%%                                          |          and shutdown
%%                                          |   
%%                                          V
%%                                    INITIAL_HELLO
%%
%%                                          | Send/Recv Flight 1
%%                                          |
%%                                          |
%%           USER_HELLO                     |
%%  <- Possibly let user provide            V
%%  options after looking at hello ex ->    HELLO
%%                                             | Send/Recv Flight 2 or Abbrev Flight 1 - Abbrev Flight 2 part 1 
%%                                             |
%%                                New session  | Resumed session
%%  WAIT_OCSP_STAPLING   CERTIFY  <----------------------------------> ABBREVIATED
%%  WAIT_CERT_VERIFY   
%%  <- Possibly Receive  --  |                                              |
%% OCSP Staple/CertVerify -> |  Flight 3 part 1                             |
%%                           |                                              |
%%                           V                                              |  Abbrev Flight 2 part 2 to Abbrev Flight 3
%%                         CIPHER                                           |
%%                           |                                              |
%%                           |                                              |
%%                           | Fligth 3 part 2 to Flight 4                  |   
%%                           |                                              |   
%%                           V                                              V   
%%                         ----------------------------------------------------
%%                                                    |
%%                                                    |
%%                                                    V
%%                                                 CONNECTION
%%                                                    |
%%                                                    |  Renegotiaton
%%                                                    V
%%                                               GO BACK TO HELLO
%%----------------------------------------------------------------------

-module(tls_connection).

-behaviour(gen_statem).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("ssl_alert.hrl").
-include("tls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").

%% Internal application API

%% Setup
-export([init/1]).

-export([renegotiate/2,
        choose_tls_fsm/2]).

%% gen_statem state functions
-export([initial_hello/3,
         config_error/3,
         downgrade/3,
	 hello/3,
         user_hello/3,
         wait_ocsp_stapling/3,
         certify/3,
         wait_cert_verify/3,
         cipher/3,
         abbreviated/3,
	 connection/3]).

%% gen_statem callbacks
-export([callback_mode/0,
         terminate/3,
         code_change/4,
         format_status/2]).

%% Tracing
-export([handle_trace/3]).
%%====================================================================
%% Internal application API
%%====================================================================	     
init([Role, Sender, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = initial_state(Role, Sender, Host, Port, Socket, Options, User, CbInfo),
    try
        State1 = #state{static_env = #static_env{session_cache = Cache,
                                                 session_cache_cb = CacheCb
                                                },
                        connection_env = #connection_env{cert_key_alts = CertKeyAlts},
                        ssl_options = SslOptions,
                        session = Session0} = ssl_gen_statem:init_ssl_config(State0#state.ssl_options, Role, State0),
        State = case Role of
                    client ->
                        CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts),
                        Session = ssl_session:client_select_session({Host, Port, SslOptions}, Cache, CacheCb, Session0, CertKeyPairs),
                        State1#state{session = Session};
                    server ->
                        State1
                end,
        tls_gen_connection:initialize_tls_sender(State),
        gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch throw:Error ->
            #state{protocol_specific = Map} = State0,
            EState = State0#state{protocol_specific = Map#{error => Error}},
            gen_statem:enter_loop(?MODULE, [], config_error, EState)
    end.

renegotiate(#state{static_env = #static_env{role = client},
                   handshake_env = HsEnv} = State, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_handshake_history(),
    {next_state, connection, State#state{handshake_env = HsEnv#handshake_env{tls_handshake_history = Hs0}}, 
     [{next_event, internal, #hello_request{}} | Actions]};
renegotiate(#state{static_env = #static_env{role = server,
                                            socket = Socket,
                                            transport_cb = Transport},
                   handshake_env = HsEnv,
                   connection_env = #connection_env{negotiated_version = Version},
		   connection_states = ConnectionStates0} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = tls_handshake:encode_handshake(HelloRequest, Version),
    Hs0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates} = 
	tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    tls_socket:send(Transport, Socket, BinMsg),
    State = State0#state{connection_states = 
			     ConnectionStates,
			 handshake_env = HsEnv#handshake_env{tls_handshake_history = Hs0}},
    tls_gen_connection:next_event(hello, no_record, State, Actions).

%%--------------------------------------------------------------------
%% State functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec initial_hello(gen_statem:event_type(),
                    {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
initial_hello(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).
 
%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).
 
%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, #client_hello{extensions = Extensions},
      #state{static_env = #static_env{role = server},
             handshake_env = #handshake_env{continue_status = pause},
             start_or_recv_from = From} = State) ->
    {next_state, user_hello, State#state{start_or_recv_from = undefined},
     [{postpone, true}, {reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{extensions = Extensions},
      #state{handshake_env = #handshake_env{continue_status = pause},
             static_env = #static_env{role = client},
             start_or_recv_from = From} = State) ->
    {next_state, user_hello,
     State#state{start_or_recv_from = undefined}, [{postpone, true}, {reply, From, {ok, Extensions}}]};
hello(internal, #client_hello{client_version = ClientVersion} = Hello,
      #state{static_env = #static_env{role = server}, connection_env = CEnv} = State0) ->
    try
        #state{ssl_options = SslOpts} = State1 = tls_dtls_connection:handle_sni_extension(State0, Hello),
        case choose_tls_fsm(SslOpts, Hello) of
            tls_1_3_fsm ->
                {next_state, start, State1,
                 [{change_callback_module, tls_server_connection_1_3}, {next_event, internal, Hello}]};
            tls_1_0_to_1_2_fsm ->
                {ServerHelloExt, Type, State} = handle_client_hello(Hello, State1),
                {next_state, hello, State, [{next_event, internal, {common_client_hello, Type, ServerHelloExt}}]}
        end
    catch  throw:#alert{} = Alert ->
            AlertState = State0#state{connection_env = CEnv#connection_env{negotiated_version = ClientVersion}},
            ssl_gen_statem:handle_own_alert(Alert, hello, AlertState)
    end;
hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
             connection_env = CEnv,
	     static_env = #static_env{role = client},
             handshake_env = #handshake_env{
                                ocsp_stapling_state = OcspState0,
                                renegotiation = {Renegotiation, _}} = HsEnv,
             session = #session{session_id = OldId},
	     ssl_options = SslOptions} = State) ->
    try
        case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation, OldId) of
            %% Legacy TLS 1.2 and older
            {Version, NewId, ConnectionStates, ProtoExt, Protocol, OcspState} ->
                tls_dtls_connection:handle_session(
                  Hello, Version, NewId, ConnectionStates, ProtoExt, Protocol,
                  State#state{
                    handshake_env =
                        HsEnv#handshake_env{
                          ocsp_stapling_state = maps:merge(OcspState0,OcspState)}});
            %% TLS 1.3
            {next_state, wait_sh, SelectedVersion, OcspState} ->
                %% Continue in TLS 1.3 'wait_sh' state
                {next_state, wait_sh,
                 State#state{handshake_env =
                                 HsEnv#handshake_env{ocsp_stapling_state =
                                                         maps:merge(OcspState0, OcspState)},
                             connection_env =
                                 CEnv#connection_env{negotiated_version = SelectedVersion}},
                 [{change_callback_module, tls_client_connection_1_3},
                  {next_event, internal, Hello}]}
        end
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, hello, State)
    end;
hello(info, Event, State) ->
    tls_gen_connection:handle_info(Event, ?FUNCTION_NAME, State);
hello(Type, Event, State) ->
    try tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

user_hello(Type, Event, State) ->
    try tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
abbreviated(Type, Event, State) ->
    try tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

%%--------------------------------------------------------------------
-spec wait_ocsp_stapling(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_ocsp_stapling(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
wait_ocsp_stapling(Type, Event, State) ->
    try tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
certify(Type, Event, State) ->
    try tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.


%%--------------------------------------------------------------------
-spec wait_cert_verify(gen_statem:event_type(), term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert_verify(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
wait_cert_verify(Type, Event, State) ->
    try tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
cipher(Type, Event, State) ->
    try tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),  
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
connection({call, From}, {user_renegotiate, WriteState}, 
           #state{connection_states = ConnectionStates} = State) ->
    {next_state,  ?FUNCTION_NAME, State#state{connection_states = ConnectionStates#{current_write => WriteState}}, 
     [{next_event,{call, From}, renegotiate}]};
connection(internal, #hello_request{},
	   #state{static_env = #static_env{role = client,
                                           host = Host,
                                           port = Port,
                                           cert_db = CertDbHandle,
                                           cert_db_ref = CertDbRef,
                                           session_cache = Cache,
                                           session_cache_cb = CacheCb},
                  handshake_env = #handshake_env{
                                     renegotiation = {Renegotiation, peer},
                                     ocsp_stapling_state = OcspState},
                  connection_env = #connection_env{cert_key_alts = CertKeyAlts},
		  session = Session0,
		  ssl_options = SslOpts, 
                  protocol_specific = #{sender := Pid},
		  connection_states = ConnectionStates} = State0) ->
    try tls_sender:peer_renegotiate(Pid) of
        {ok, Write} ->
            CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts),
            Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb, Session0, CertKeyPairs),
            Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                               Session#session.session_id,
                                               Renegotiation, undefined,
                                               undefined, maps:get(ocsp_nonce, OcspState, undefined),
                                               CertDbHandle, CertDbRef),
            {State, Actions} = tls_gen_connection:send_handshake(Hello, 
                                                                 State0#state{connection_states = 
                                                                                  ConnectionStates#{current_write => Write},
                                                                              session = Session}),
            tls_gen_connection:next_event(hello, no_record, State, Actions)
        catch
            _:Reason:ST ->
                ?SSL_LOG(info, internal_error, [{error, Reason}, {stacktrace, ST}]),
                {stop, {shutdown, sender_blocked}, State0}
        end;
connection(internal, #hello_request{},
	   #state{static_env = #static_env{role = client,
                                           host = Host,
                                           port = Port,
                                           cert_db = CertDbHandle,
                                           cert_db_ref = CertDbRef
                                          },
                  handshake_env = #handshake_env{
                      renegotiation = {Renegotiation, _},
                      ocsp_stapling_state = OcspState},
		  ssl_options = SslOpts, 
		  connection_states = ConnectionStates} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                       <<>>, Renegotiation, undefined,
                                       undefined, maps:get(ocsp_nonce, OcspState, undefined),
                                      CertDbHandle, CertDbRef),

    {State, Actions} = tls_gen_connection:send_handshake(Hello, State0),
    tls_gen_connection:next_event(hello, no_record, State, Actions);
connection(internal, #client_hello{} = Hello, 
	   #state{static_env = #static_env{role = server},
                  handshake_env = #handshake_env{allow_renegotiate = true}= HsEnv,
                  connection_states = CS,
                  protocol_specific = #{sender := Sender}
                 } = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {ok, Write} = tls_sender:renegotiate(Sender),
    tls_gen_connection:next_event(hello, no_record, 
                                  State#state{connection_states = CS#{current_write => Write},
                                              handshake_env = HsEnv#handshake_env{renegotiation = {true, peer},
                                                                                  allow_renegotiate = false}
                                             }, 
                                  [{next_event, internal, Hello}]);
connection(internal, #client_hello{}, 
	   #state{static_env = #static_env{role = server},
                  handshake_env = #handshake_env{allow_renegotiate = false}} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    tls_gen_connection:send_alert_in_connection(Alert, State0),
    State = tls_gen_connection:reinit_handshake_data(State0),
    tls_gen_connection:next_event(?FUNCTION_NAME, no_record, State);
connection(Type, Event, State) ->
    try tls_dtls_connection:?FUNCTION_NAME(Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
     ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

terminate({shutdown, {sender_died, Reason}}, _StateName,
          #state{static_env = #static_env{socket = Socket, 
                                          transport_cb = Transport}} 
          = State) ->
    ssl_gen_statem:handle_trusted_certs_db(State),
    tls_gen_connection:close(Reason, Socket, Transport, undefined);
terminate(Reason, StateName, State) ->
    ssl_gen_statem:terminate(Reason, StateName, State).

format_status(Type, Data) ->
    ssl_gen_statem:format_status(Type, Data).

code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_state(Role, Sender, Host, Port, Socket, {SSLOptions, SocketOptions, Trackers}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
    put(log_level, maps:get(log_level, SSLOptions)),
    %% Use highest supported version for client/server random nonce generation
    #{versions := [Version|_]} = SSLOptions,
    BeastMitigation = maps:get(beast_mitigation, SSLOptions, disabled),
    ConnectionStates = tls_record:init_connection_states(Role,
                                                         Version,
                                                         BeastMitigation),
    #{session_cb := SessionCacheCb} = ssl_config:pre_1_3_session_opts(Role),
    UserMonitor = erlang:monitor(process, User),
    InitStatEnv = #static_env{
                     role = Role,
                     transport_cb = CbModule,
                     protocol_cb = tls_gen_connection,
                     data_tag = DataTag,
                     close_tag = CloseTag,
                     error_tag = ErrorTag,
                     passive_tag = PassiveTag,
                     host = Host,
                     port = Port,
                     socket = Socket,
                     session_cache_cb = SessionCacheCb,
                     trackers = Trackers
                    },  
    #state{
       static_env = InitStatEnv,
       handshake_env = #handshake_env{
                          tls_handshake_history = ssl_handshake:init_handshake_history(),
                          renegotiation = {false, first},
                          allow_renegotiate = maps:get(client_renegotiation, SSLOptions, undefined)
                         },
       connection_env = #connection_env{user_application = {UserMonitor, User}},
       socket_options = SocketOptions,
       ssl_options = SSLOptions,
       session = #session{is_resumable = false},
       connection_states = ConnectionStates,
       protocol_buffers = #protocol_buffers{},
       user_data_buffer = {[],0,[]},
       start_or_recv_from = undefined,
       flight_buffer = [],
       protocol_specific = #{sender => Sender,
                             active_n => ssl_config:get_internal_active_n(
                                           maps:get(erl_dist, SSLOptions, false)),
                             active_n_toggle => true
                            }
      }.

handle_client_hello(#client_hello{client_version = ClientVersion} = Hello, State) ->
    #state{connection_states = ConnectionStates0,
           static_env = #static_env{trackers = Trackers},
           handshake_env = #handshake_env{
                              kex_algorithm = KeyExAlg,
                              renegotiation = {Renegotiation, _},
                              negotiated_protocol = CurrentProtocol,
                              sni_guided_cert_selection = SNICertSelection} = HsEnv,
           connection_env = #connection_env{cert_key_alts = CertKeyAlts} = CEnv,
           session = Session0,
           ssl_options = SslOpts} = State,
    SessionTracker = proplists:get_value(session_id_tracker, Trackers),
    {Version, {Type, Session},
     ConnectionStates, Protocol0, ServerHelloExt0, HashSign} =
        tls_handshake:hello(Hello,
                            SslOpts,
                            {SessionTracker, Session0,
                             ConnectionStates0, CertKeyAlts, KeyExAlg},
                            Renegotiation),
    Protocol = case Protocol0 of
                   undefined -> CurrentProtocol;
                   _ -> Protocol0
               end,
    ServerHelloExt =
        case SNICertSelection of
            true ->
                ServerHelloExt0#{sni => #sni{hostname = ""}};
            false ->
                ServerHelloExt0
        end,
    {ServerHelloExt, Type, State#state{connection_states  = ConnectionStates,
                                       connection_env = CEnv#connection_env{negotiated_version = Version},
                                       handshake_env = HsEnv#handshake_env{
                                                         hashsign_algorithm = HashSign,
                                                         client_hello_version = ClientVersion,
                                                         negotiated_protocol = Protocol},
                                       session = Session
                                      }}.


gen_info(Event, connection = StateName, State) ->
    try
        tls_gen_connection:handle_info(Event, StateName, State)
    catch
        _:Reason:ST ->
            ?SSL_LOG(info, internal_error, [{error, Reason}, {stacktrace, ST}]),
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR,
						       malformed_data),
					    StateName, State)
    end;

gen_info(Event, StateName, State) ->
    try
        tls_gen_connection:handle_info(Event, StateName, State)
    catch
        _:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE,
						       malformed_handshake_data),
					    StateName, State)
    end.

choose_tls_fsm(#{versions := Versions},
               #client_hello{
                  extensions = #{client_hello_versions :=
                                     #client_hello_versions{versions = ClientVersions}
                                }
                 }) ->
    case ssl_handshake:select_supported_version(ClientVersions, Versions) of
        ?TLS_1_3 ->
            tls_1_3_fsm;
        _Else ->
            tls_1_0_to_1_2_fsm
    end;
choose_tls_fsm(_, _) ->
    tls_1_0_to_1_2_fsm.

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(csp,
             {call, {?MODULE, wait_ocsp_stapling,
                     [Type, Event|_]}}, Stack) ->
    {io_lib:format("Type = ~w Event = ~W", [Type, Event, 10]), Stack}.
