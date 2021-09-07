%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2020. All Rights Reserved.
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
%%  WAIT_OCSP_STAPELING   CERTIFY  <----------------------------------> ABBRIVIATED
%%     
%%  <- Possibly Receive  --  |                                              |
%%     OCSP Stapel ------>   |  Flight 3 part 1                             |
%%                           |                                              |
%%                           V                                              |  Abbrev Flight 2 part 2 to Abbrev Flight 3
%%                         CIPHER                                           |
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

-export([renegotiate/2]).

%% gen_statem state functions
-export([initial_hello/3,
         config_error/3,
         downgrade/3,
	 hello/3,
         user_hello/3,
         wait_ocsp_stapling/3,
         certify/3,
         cipher/3,
         abbreviated/3,
	 connection/3]).

%% gen_statem callbacks
-export([callback_mode/0,
         terminate/3,
         code_change/4,
         format_status/2]).
 
%%====================================================================
%% Internal application API
%%====================================================================	     
init([Role, Sender, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = #state{protocol_specific = Map} = initial_state(Role, Sender,
                                                             Host, Port, Socket, Options, User, CbInfo),
    try 
	State1 = #state{static_env = #static_env{session_cache = Cache,
                                                 session_cache_cb = CacheCb
                                                },
                        ssl_options = SslOptions,
                        session = Session0} = ssl_gen_statem:ssl_config(State0#state.ssl_options, Role, State0),
        State = case Role of
                    client ->
                        Session = ssl_session:client_select_session({Host, Port, SslOptions}, Cache, CacheCb, Session0),
                        State1#state{session = Session};
                    server ->
                        State1
                end,
        tls_gen_connection:initialize_tls_sender(State),
        gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch throw:Error ->
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
hello(internal, #client_hello{extensions = Extensions} = Hello, 
      #state{ssl_options = #{handshake := hello},
             handshake_env = HsEnv,
             start_or_recv_from = From} = State) ->
    {next_state, user_hello, State#state{start_or_recv_from = undefined,
                                         handshake_env = HsEnv#handshake_env{hello = Hello}},
     [{reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{extensions = Extensions} = Hello,
      #state{ssl_options = #{
                 handshake := hello},
             handshake_env = HsEnv,
             start_or_recv_from = From} = State) ->   
    {next_state, user_hello,
     State#state{start_or_recv_from = undefined,
                 handshake_env = HsEnv#handshake_env{
                                   hello = Hello}}, [{reply, From, {ok, Extensions}}]};
hello(internal, #client_hello{client_version = ClientVersion} = Hello, #state{ssl_options = SslOpts0,
                                                                              connection_env = CEnv} = State0) ->
    case choose_tls_fsm(SslOpts0, Hello) of
        tls_1_3_fsm ->
            %% Continue in TLS 1.3 'start' state
            {next_state, start, State0, [{change_callback_module, tls_connection_1_3}, {next_event, internal, Hello}]};
        tls_1_0_to_1_2_fsm ->
            case handle_client_hello(Hello, State0) of
                {ServerHelloExt, Type, State} ->
                    {next_state, hello, State, [{next_event, internal, {common_client_hello, Type, ServerHelloExt}}]};
                Alert ->
                    ssl_gen_statem:handle_own_alert(Alert, ClientVersion, hello,
                                                        State0#state{connection_env = CEnv#connection_env{negotiated_version
                                                                                                          = ClientVersion}})
            end
    end;
hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
             connection_env = #connection_env{negotiated_version = ReqVersion} = CEnv,
	     static_env = #static_env{role = client},
             handshake_env = #handshake_env{
                                ocsp_stapling_state = OcspState0,
                                renegotiation = {Renegotiation, _}} = HsEnv,
             session = #session{session_id = OldId},
	     ssl_options = SslOptions} = State) ->   
    case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation, OldId) of
        #alert{} = Alert -> 
            ssl_gen_statem:handle_own_alert(Alert, ReqVersion, hello,
                                            State#state{connection_env =
                                                            CEnv#connection_env{negotiated_version = ReqVersion}
                                                       });
        %% Legacy TLS 1.2 and older
        {Version, NewId, ConnectionStates, ProtoExt, Protocol, OcspState} ->
            tls_dtls_connection:handle_session(Hello, 
                                          Version, NewId, ConnectionStates, ProtoExt, Protocol,
                                          State#state{
                                            handshake_env = HsEnv#handshake_env{
                                                              ocsp_stapling_state = maps:merge(OcspState0,OcspState)}});
        %% TLS 1.3
        {next_state, wait_sh, SelectedVersion, OcspState} ->
            %% Continue in TLS 1.3 'wait_sh' state
            {next_state, wait_sh,
             State#state{handshake_env = HsEnv#handshake_env{ocsp_stapling_state =  maps:merge(OcspState0,OcspState)}, 
                         connection_env = CEnv#connection_env{negotiated_version = SelectedVersion}},
             [{change_callback_module, tls_connection_1_3}, {next_event, internal, Hello}]}
    end;
hello(info, Event, State) ->
    tls_gen_connection:handle_info(Event, ?FUNCTION_NAME, State);
hello(Type, Event, State) ->
    tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

user_hello(Type, Event, State) ->
    tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
abbreviated(Type, Event, State) ->
    tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_ocsp_stapling(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_ocsp_stapling(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
wait_ocsp_stapling(Type, Event, State) ->
    tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
certify(Type, Event, State) ->
    tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
cipher(Type, Event, State) ->
     tls_dtls_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

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
                                           session_cache = Cache,
                                           session_cache_cb = CacheCb},
                  handshake_env = #handshake_env{
                      renegotiation = {Renegotiation, peer},
                      ocsp_stapling_state = OcspState},
		  session = #session{own_certificates = OwnCerts} = Session0,
		  ssl_options = SslOpts, 
                  protocol_specific = #{sender := Pid},
		  connection_states = ConnectionStates} = State0) ->
    try tls_sender:peer_renegotiate(Pid) of
        {ok, Write} ->
            Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb, Session0),
            Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                               Session#session.session_id,
                                               Renegotiation, OwnCerts, undefined,
                                               undefined, maps:get(ocsp_nonce, OcspState, undefined)),
            {State, Actions} = tls_gen_connection:send_handshake(Hello, 
                                                                 State0#state{connection_states = 
                                                                                  ConnectionStates#{current_write => Write},
                                                                              session = Session}),
            tls_gen_connection:next_event(hello, no_record, State, Actions)
        catch 
            _:_ ->
                {stop, {shutdown, sender_blocked}, State0}
        end;
connection(internal, #hello_request{},
	   #state{static_env = #static_env{role = client,
                                           host = Host,
                                           port = Port},
                  handshake_env = #handshake_env{
                      renegotiation = {Renegotiation, _},
                      ocsp_stapling_state = OcspState},
		  session = #session{own_certificates = OwnCerts},
		  ssl_options = SslOpts, 
		  connection_states = ConnectionStates} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                       <<>>, Renegotiation, OwnCerts, undefined,
                                       undefined, maps:get(ocsp_nonce, OcspState, undefined)),

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
    tls_dtls_connection:?FUNCTION_NAME(Type, Event, State).

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
    tls_gen_connection:close(Reason, Socket, Transport, undefined, undefined);
terminate(Reason, StateName, State) ->
    catch ssl_gen_statem:terminate(Reason, StateName, State),
    ensure_sender_terminate(Reason, State).

format_status(Type, Data) ->
    ssl_gen_statem:format_status(Type, Data).

code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_state(Role, Sender, Host, Port, Socket, {SSLOptions, SocketOptions, Trackers}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
    #{beast_mitigation := BeastMitigation,
      erl_dist := IsErlDist,
      client_renegotiation := ClientRenegotiation} = SSLOptions,
    ConnectionStates = tls_record:init_connection_states(Role, BeastMitigation),
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
                          allow_renegotiate = ClientRenegotiation
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
                             active_n => ssl_config:get_internal_active_n(IsErlDist),
                             active_n_toggle => true
                            }
      }.

handle_client_hello(#client_hello{client_version = ClientVersion} = Hello, State0) ->
    case tls_dtls_connection:handle_sni_extension(State0, Hello) of
        #state{connection_states = ConnectionStates0,
                    static_env = #static_env{trackers = Trackers},
                    handshake_env = #handshake_env{
                                       kex_algorithm = KeyExAlg,
                                       renegotiation = {Renegotiation, _},
                                       negotiated_protocol = CurrentProtocol,
                                       sni_guided_cert_selection = SNICertSelection} = HsEnv,
                    connection_env = CEnv,
                    session = #session{own_certificates = OwnCerts} = Session0,
               ssl_options = SslOpts} = State ->
            SessionTracker = proplists:get_value(session_id_tracker, Trackers),
            case tls_handshake:hello(Hello,
                                     SslOpts,
                                     {SessionTracker, Session0,
                                      ConnectionStates0, OwnCerts, KeyExAlg},
                                     Renegotiation) of
                #alert{} = Alert ->
                    Alert;
                {Version, {Type, Session},
                 ConnectionStates, Protocol0, ServerHelloExt0, HashSign} ->
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
                                                      }}
            end;
        #alert{} = Alert ->
            Alert
    end.


gen_info(Event, connection = StateName,  #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    try tls_gen_connection:handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch 
        _:_ ->
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR,
						       malformed_data), 
					    Version, StateName, State)  
    end;

gen_info(Event, StateName, #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    try tls_gen_connection:handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch 
        _:_ ->
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE,
						       malformed_handshake_data), 
					    Version, StateName, State)  
    end.
	    
ensure_sender_terminate(downgrade, _) ->
    ok; %% Do not terminate sender during downgrade phase 
ensure_sender_terminate(_,  #state{protocol_specific = #{sender := Sender}}) ->
    %% Make sure TLS sender dies when connection process is terminated normally
    %% This is needed if the tls_sender is blocked in prim_inet:send 
    Kill = fun() -> 
                   receive 
                   after 5000 ->
                           catch (exit(Sender, kill))
                   end
           end,
    spawn(Kill).

choose_tls_fsm(#{versions := Versions},
               #client_hello{
                  extensions = #{client_hello_versions :=
                                     #client_hello_versions{versions = ClientVersions}
                                }
                 }) ->
    case ssl_handshake:select_supported_version(ClientVersions, Versions) of
        {3,4} ->
            tls_1_3_fsm;
        _Else ->
            tls_1_0_to_1_2_fsm
    end;
choose_tls_fsm(_, _) ->
    tls_1_0_to_1_2_fsm.
