%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

-module(tls_server_connection).
-moduledoc false.

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
%%                                             | Send/Recv Flight 2 or Abbrev Flight 1
%%                                             | - Abbrev Flight 2 part 1
%%                                             |
%%                                New session  | Resumed session
%%  WAIT_CERT_VERIFY   CERTIFY  <----------------------------------> ABBREVIATED
%%
%%  <- Possibly Receive  --  |                                              |
%% CertVerify ->             |  Flight 3 part 1                             |
%%                           |                                              |
%%                           V                                              |  Abbrev Flight 2 part
%%                                                                          |  2 to Abbrev Flight 3
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

-behaviour(gen_statem).

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("ssl_alert.hrl").

%% Internal application API

%% Setup
-export([init/1]).

%% gen_statem state functions
-export([initial_hello/3,
         config_error/3,
         downgrade/3,
	 hello/3,
         user_hello/3,
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

init([Role, Sender, Tab, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = tls_dtls_gen_connection:initial_state(Role, Sender, Tab, Host, Port, Socket,
                                                   Options, User, CbInfo),
    #state{static_env = #static_env{user_socket = UserSocket}} = State0,
    User ! {self(), user_socket, UserSocket},
    put(tls_role, server),
    try ssl_gen_statem:init_ssl_config(State0#state.ssl_options, Role, State0) of
        State ->
            tls_gen_connection:initialize_tls_sender(State),
            gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch throw:Error ->
            #state{protocol_specific = Map} = State0,
            EState = State0#state{protocol_specific = Map#{error => Error}},
            gen_statem:enter_loop(?MODULE, [], config_error, EState)
    end.

%%--------------------------------------------------------------------
%% State functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec initial_hello(gen_statem:event_type(),
                    {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
initial_hello({call, From}, {start, Timeout},
              #state{static_env = #static_env{
                                     protocol_cb = Connection},
                     ssl_options = #{versions := Versions},
                     recv = Recv} = State0) ->
    case Versions of
        [?TLS_1_3] ->
            Connection:next_event(start, no_record, State0#state{recv = Recv#recv{from = From}},
                                  [{change_callback_module, tls_server_connection_1_3},
                                   {{timeout, handshake}, Timeout, close}]);
        _ ->
            Connection:next_event(hello, no_record, State0#state{recv = Recv#recv{from = From}},
                                  [{{timeout, handshake}, Timeout, close}])
    end;
initial_hello({call, From}, {start, {Opts, EmOpts}, Timeout},
     #state{static_env = #static_env{role = Role},
            handshake_env = #handshake_env{} = Env,
            ssl_options = OrigSSLOptions,
            socket_options = SockOpts} = State0) ->
    try
        SslOpts = ssl_config:update_options(Opts, Role, OrigSSLOptions),
	State = ssl_gen_statem:ssl_config(SslOpts, Role, State0),
        CountinueStatus = case maps:get(handshake, SslOpts) of
                              hello ->
                                  pause;
                              Other ->
                                  Other
                          end,
        initial_hello({call, From}, {start, Timeout},
	     State#state{ssl_options = SslOpts,
                         handshake_env = Env#handshake_env{continue_status = CountinueStatus},
                         socket_options = ssl_config:new_emulated(EmOpts, SockOpts)})
    catch throw:Error ->
	   {stop_and_reply, {shutdown, normal}, {reply, From, {error, Error}}, State0}
    end;
initial_hello(Type, Event, State) ->
    tls_dtls_server_connection:initial_hello(Type, Event, State).

%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(Type, Event, State) ->
    ssl_gen_statem:config_error(Type, Event, State).

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, #client_hello{extensions = Extensions},
      #state{handshake_env = #handshake_env{continue_status = pause},
             recv = #recv{from = From} = Recv} = State) ->
    {next_state, user_hello, State#state{recv = Recv#recv{from = undefined}},
     [{postpone, true}, {reply, From, {ok, Extensions}}]};
hello(internal, #client_hello{client_version = ClientVersion} = Hello,
      #state{connection_env = CEnv} = State0) ->
    try
        #state{ssl_options = SslOpts} = State1 =
            tls_dtls_server_connection:handle_sni_extension(State0, Hello),
        case choose_tls_fsm(SslOpts, Hello) of
            tls_1_3_fsm ->
                {next_state, start, State1,
                 [{change_callback_module, tls_server_connection_1_3},
                  {next_event, internal, Hello}]};
            tls_1_0_to_1_2_fsm ->
                {ServerHelloExt, Type, State} = handle_client_hello(Hello, State1),
                {next_state, ?STATE(hello), State,
                 [{next_event, internal, {common_client_hello, Type, ServerHelloExt}}]}
        end
    catch throw:#alert{} = Alert ->
            NewCenv = CEnv#connection_env{negotiated_version = ClientVersion},
            AlertState =
                State0#state{connection_env = NewCenv},
            ssl_gen_statem:handle_own_alert(Alert, ?STATE(hello), AlertState)
    end;
hello(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(hello), State);
hello(Type, Event, State) ->
    gen_state(?STATE(hello), Type, Event, State).

%%--------------------------------------------------------------------
-spec user_hello(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
user_hello(Type, Event, State) ->
    gen_state(?STATE(user_hello), Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(abbreviated), State);
abbreviated(Type, Event, State) ->
    gen_state(?STATE(abbreviated), Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(certify), State);
certify(Type, Event, State) ->
    gen_state(?STATE(certify), Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_cert_verify(gen_statem:event_type(), term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert_verify(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(wait_cert_verify), State);
wait_cert_verify(internal, #certificate_verify{signature = Signature,
                                               hashsign_algorithm = CertHashSign},
                 #state{static_env = #static_env{protocol_cb = Connection},
                        handshake_env = #handshake_env{tls_handshake_history = Hist,
                                                       kex_algorithm = KexAlg,
                                                       client_certificate_status = needs_verifying,
                                                       public_key_info = PubKeyInfo} = HsEnv0,
                        connection_env = #connection_env{negotiated_version = Version},
                        session = #session{master_secret = MasterSecret} = Session0
                       } = State) ->

    TLSVersion = ssl:tls_version(Version),
    %% Use negotiated value if TLS-1.2 otherwise return default
    HashSign = tls_dtls_gen_connection:negotiated_hashsign(CertHashSign, KexAlg,
                                                           PubKeyInfo, TLSVersion),
    case ssl_handshake:certificate_verify(Signature, PubKeyInfo,
					  TLSVersion, HashSign, MasterSecret, Hist) of
	valid ->
            HsEnv = HsEnv0#handshake_env{client_certificate_status = verified},
	    Connection:next_event(cipher, no_record,
				  State#state{handshake_env = HsEnv,
                                              session = Session0#session{sign_alg = HashSign}});
	#alert{} = Alert ->
            throw(Alert)
    end;
wait_cert_verify(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(wait_cert_verify), State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(cipher), State);
cipher(Type, Event, State) ->
    gen_state(?STATE(cipher), Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(connection), State);
connection(cast, {internal_renegotiate, WriteState}, #state{handshake_env = HsEnv,
                                                            connection_states = ConnectionStates}
           = State) ->
    renegotiate(State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}},
                            connection_states = ConnectionStates#{current_write => WriteState}}, []);
connection({call, From}, {user_renegotiate, WriteState},
           #state{connection_states = ConnectionStates} = State) ->
    {next_state, ?STATE(connection),
     State#state{connection_states = ConnectionStates#{current_write => WriteState}},
     [{next_event,{call, From}, renegotiate}]};
connection(internal, #client_hello{} = Hello,
	   #state{handshake_env = #handshake_env{allow_renegotiate = true}= HsEnv,
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
                                  State#state{connection_states =
                                                  CS#{current_write => Write},
                                              handshake_env =
                                                  HsEnv#handshake_env{renegotiation = {true, peer},
                                                                      allow_renegotiate = false}
                                             },
                                  [{next_event, internal, Hello}]);
connection(internal, #client_hello{},
	   #state{handshake_env = #handshake_env{allow_renegotiate = false}} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    tls_gen_connection:send_alert_in_connection(Alert, State0),
    State = tls_gen_connection:reinit_handshake_data(State0),
    tls_gen_connection:next_event(?STATE(connection), no_record, State);
connection({call, From}, renegotiate, #state{static_env = #static_env{socket = Socket,
                                            transport_cb = Transport},
                                             handshake_env = HsEnv,
                                             connection_env =
                                                 #connection_env{negotiated_version = Version},
                                             connection_states = ConnectionStates0} = State0) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = tls_handshake:encode_handshake(HelloRequest, Version),
    Hs0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates} =
	tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    tls_socket:send(Transport, Socket, BinMsg),
    State = State0#state{connection_states =
			     ConnectionStates,
			 handshake_env = HsEnv#handshake_env{tls_handshake_history = Hs0,
                                                             renegotiation = {true, From}}},
    tls_gen_connection:next_event(hello, no_record, State);
connection(Type, Event, State) ->
    gen_state(?STATE(connection), Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
     ssl_gen_statem:downgrade(Type, Event, State).

%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

terminate(Reason, StateName, State) ->
    ssl_gen_statem:terminate(Reason, StateName, State).

format_status(Type, Data) ->
    ssl_gen_statem:format_status(Type, Data).

code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

%%====================================================================
%% Internal functions
%%====================================================================
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
    {ServerHelloExt, Type,
     State#state{connection_states  = ConnectionStates,
                 connection_env = CEnv#connection_env{negotiated_version = Version},
                 handshake_env = HsEnv#handshake_env{
                                   hashsign_algorithm = HashSign,
                                   client_hello_version = ClientVersion,
                                   negotiated_protocol = Protocol},
                 session = Session
                }}.

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

gen_state(StateName, Type, Event, State) ->
    try tls_dtls_server_connection:StateName(Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, StateName, State);
          _:Reason:ST ->
            ?SSL_LOG(info, unexpected_error, [{error, Reason}, {stacktrace, ST}]),
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR,
						       unexpected_error),
					    StateName, State)
    end.

renegotiate(#state{static_env = #static_env{socket = Socket,
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
%% Tracing
%%--------------------------------------------------------------------
handle_trace(hbn,
             {call, {?MODULE, connection,
                     [_Type = info, Event, _State]}},
             Stack) ->
    {io_lib:format("Type = info Event = ~W ", [Event, 10]), Stack}.
