%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2022-2025. All Rights Reserved.
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
%% Purpose: TLS-1.3 FSM (server side)
%%----------------------------------------------------------------------
%% A.2.  Server
%%
%%                               START <-----+
%%                Recv ClientHello |         | Send HelloRetryRequest
%%                                 v         |
%%                              RECVD_CH ----+
%%                                 | Select parameters
%%                                 v
%%                              NEGOTIATED
%%                                 | Send ServerHello
%%                                 | K_send = handshake
%%                                 | Send EncryptedExtensions
%%                                 | [Send CertificateRequest]
%%  Can send                       | [Send Certificate + CertificateVerify]
%%  app data                       | Send Finished
%%  after   -->                    | K_send = application
%%  here                  +--------+--------+
%%               No 0-RTT |                 | 0-RTT
%%                        |                 |
%%    K_recv = handshake  |                 | K_recv = early data
%%  [Skip decrypt errors] |    +------> WAIT_EOED -+
%%                        |    |       Recv |      | Recv EndOfEarlyData
%%                        |    | early data |      | K_recv = handshake
%%                        |    +------------+      |
%%                        |                        |
%%                        +> WAIT_FLIGHT2 <--------+
%%                                 |
%%                        +--------+--------+
%%                No auth |                 | Client auth
%%                        |                 |
%%                        |                 v
%%                        |             WAIT_CERT
%%                        |        Recv |       | Recv Certificate
%%                        |       empty |       v
%%                        | Certificate |    WAIT_CV
%%                        |             |       | Recv
%%                        |             v       | CertificateVerify
%%                        +-> WAIT_FINISHED <---+
%%                                 | Recv Finished
%%                                 | K_recv = application
%%                                 v
%%                             CONNECTED


-module(tls_server_connection_1_3).
-moduledoc false.

-include_lib("public_key/include/public_key.hrl").

-include("ssl_alert.hrl").
-include("ssl_connection.hrl").
-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("tls_handshake_1_3.hrl").

-behaviour(gen_statem).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         terminate/3,
         code_change/4,
         format_status/2]).

%% gen_statem state functions
-export([config_error/3,
         initial_hello/3,
         user_hello/3,
         start/3,
         negotiated/3,
         wait_cert/3,
         wait_cv/3,
         wait_finished/3,
         wait_eoed/3,
         connection/3,
         downgrade/3
        ]).

%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    [state_functions, state_enter].

init([?SERVER_ROLE, Sender, Tab, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = #state{protocol_specific = Map} =
        tls_gen_connection_1_3:initial_state(?SERVER_ROLE, Sender, Tab,
                                             Host, Port, Socket, Options, User, CbInfo),
    #state{static_env = #static_env{user_socket = UserSocket}} = State0,
    User ! {self(), user_socket, UserSocket},
    put(tls_role, server),
    try
	State = ssl_gen_statem:init_ssl_config(State0#state.ssl_options, ?SERVER_ROLE, State0),
        tls_gen_connection:initialize_tls_sender(State),
        gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch throw:Error ->
            EState = State0#state{protocol_specific = Map#{error => Error}},
            gen_statem:enter_loop(?MODULE, [], config_error, EState)
    end.

terminate(Reason, StateName, State) ->
    ssl_gen_statem:terminate(Reason, StateName, State).

format_status(Type, Data) ->
    ssl_gen_statem:format_status(Type, Data).

code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

%--------------------------------------------------------------------
%% state functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec initial_hello(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
initial_hello(enter, _, State) ->
    {keep_state, State};
initial_hello(Type, Event, State) ->
    tls_server_connection:initial_hello(Type, Event, State).

%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(enter, _, State) ->
    {keep_state, State};
config_error(Type, Event, State) ->
    ssl_gen_statem:config_error(Type, Event, State).

%%--------------------------------------------------------------------
-spec user_hello(gen_statem:event_type(),
                 term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
user_hello(enter, _, State) ->
    {keep_state, State};
user_hello({call, From}, cancel, State) ->
    gen_statem:reply(From, ok),
    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?USER_CANCELED, user_canceled),
                                    user_hello, State);
user_hello({call, From}, {handshake_continue, NewOptions, Timeout},
           #state{handshake_env = #handshake_env{continue_status = {pause, ClientVersions}},
                  ssl_options = Options0} = State0) ->
    try ssl_config:update_options(NewOptions, ?SERVER_ROLE, Options0) of
        Options = #{versions := Versions} ->
            State1 = ssl_gen_statem:ssl_config(Options, ?SERVER_ROLE, State0),
            #state{handshake_env = HsEnv0} = State1,
            HsEnv = HsEnv0#handshake_env{continue_status = continue},
            State = State1#state{recv = State1#state.recv#recv{from = From}, handshake_env = HsEnv},
            case ssl_handshake:select_supported_version(ClientVersions, Versions) of
                ?TLS_1_3 ->
                    {next_state, start, State, [{{timeout, handshake}, Timeout, close}]};
                undefined ->
                    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION),
                                                    ?STATE(user_hello), State);
                _Else ->
                    {next_state, hello, State,
                     [{change_callback_module, tls_server_connection},
                      {{timeout, handshake}, Timeout, close}]}
            end
    catch
        throw:{error, Reason} ->
            gen_statem:reply(From, {error, Reason}),
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason),
                                            user_hello, State0)
    end;
user_hello(Type, Msg, State) ->
    tls_gen_connection_1_3:user_hello(Type, Msg, State).

%%--------------------------------------------------------------------
-spec start(gen_statem:event_type(),
	   #client_hello{} | #change_cipher_spec{} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
start(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?STATE(start), State,[]};
start(internal = Type, #change_cipher_spec{} = Msg,
      #state{handshake_env = #handshake_env{tls_handshake_history = Hist}} = State) ->
    case ssl_handshake:init_handshake_history() of
        Hist -> %% First message must always be client hello
            ssl_gen_statem:handle_common_event(Type, Msg, ?STATE(start), State);
        _ ->
            tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg, ?STATE(start), State)
        end;
start(internal = Type, #change_cipher_spec{} = Msg, State) ->
    tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg, ?STATE(start), State);
start(internal, #client_hello{extensions = #{client_hello_versions :=
                                                 #client_hello_versions{versions = ClientVersions}
                                            }} = Hello,
      #state{handshake_env = #handshake_env{continue_status = full}} = State) ->
    case tls_record:is_acceptable_version(?TLS_1_3, ClientVersions) of
        true ->
            handle_client_hello(Hello, State);
        false ->
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION),
                                            ?STATE(start), State)
    end;
start(internal, #client_hello{extensions = #{client_hello_versions :=
                                                  #client_hello_versions{versions = ClientVersions}
                                            }= Extensions},
      #state{recv = #recv{from = From} = Recv,
             handshake_env = #handshake_env{continue_status = pause} = HSEnv} = State) ->
    {next_state, user_hello,
     State#state{recv = Recv#recv{from = undefined},
                 handshake_env = HSEnv#handshake_env{continue_status = {pause, ClientVersions}}},
     [{postpone, true}, {reply, From, {ok, Extensions}}]};
start(internal, #client_hello{} = Hello,
      #state{handshake_env = #handshake_env{continue_status = continue}} = State) ->
    handle_client_hello(Hello, State);
start(internal, #client_hello{}, State0) -> %% Missing mandantory TLS-1.3 extensions,
    %% so it is a previous version hello.
    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?STATE(start), State0);
start(info, Msg, State) ->
    tls_gen_connection:gen_info(Msg, ?STATE(start), State);
start(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?STATE(start), State).

%%--------------------------------------------------------------------
-spec negotiated(gen_statem:event_type(),
                 {start_handshake, #pre_shared_key_server_hello{} | undefined} |
                 term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
negotiated(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?STATE(negotiated), State,[]};
negotiated(internal = Type, #change_cipher_spec{} = Msg, State) ->
    tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg, ?STATE(negotiated), State);
negotiated(internal, {start_handshake, _} = Message, State0) ->
    case send_hello_flight(Message, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?STATE(negotiated), State0);
        {State, NextState} ->
            {next_state, NextState, State, []}
    end;
negotiated(info, Msg, State) ->
    tls_gen_connection:gen_info(Msg, ?STATE(negotiated), State);
negotiated(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?STATE(negotiated), State).

%%--------------------------------------------------------------------
-spec wait_cert(gen_statem:event_type(),
                {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert(Type, Msg, State) ->
    tls_gen_connection_1_3:wait_cert(Type, Msg, State).

%%--------------------------------------------------------------------
-spec wait_cv(gen_statem:event_type(),
              {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cv(internal,
          #certificate_verify_1_3{} = CertificateVerify, State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        State1 = Maybe(tls_handshake_1_3:verify_signature_algorithm(State0,
                                                                    CertificateVerify)),
        {State, NextState} =
            Maybe(tls_handshake_1_3:verify_certificate_verify(State1, CertificateVerify)),
        tls_gen_connection:next_event(NextState, no_record, State)
    catch
        {Ref, {#alert{} = Alert, AState}} ->
            ssl_gen_statem:handle_own_alert(Alert, ?STATE(wait_cv), AState)
    end;
wait_cv(Type, Msg, State) ->
    tls_gen_connection_1_3:wait_cv(Type, Msg, State).

%%--------------------------------------------------------------------
-spec wait_finished(gen_statem:event_type(),
                    #finished{} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_finished(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?STATE(wait_finished), State,[]};
wait_finished(internal = Type, #change_cipher_spec{} = Msg, State) ->
    tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg, ?STATE(wait_finished), State);
wait_finished(internal,
              #finished{verify_data = VerifyData}, #state{static_env = #static_env{role = Role},
                                                          ssl_options = SSLOpts} = State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        Maybe(tls_handshake_1_3:validate_finished(State0, VerifyData)),

        State1 = tls_handshake_1_3:handle_secrets(State0),
        %% Configure traffic keys
        State2 = ssl_record:step_encryption_state(State1),

        State3 = maybe_send_session_ticket(State2),

        {Record, State} = ssl_gen_statem:prepare_connection(State3, tls_gen_connection),
        KeepSecrets = maps:get(keep_secrets, SSLOpts, false),
        tls_gen_connection_1_3:maybe_traffic_keylog_1_3(KeepSecrets, Role,
                                                        State#state.connection_states, 0),
        tls_gen_connection:next_event(connection, Record,
                                      tls_gen_connection_1_3:maybe_forget_hs_secrets(KeepSecrets, State),
                                      [{{timeout, handshake}, cancel}])
    catch
        {Ref, #alert{} = Alert} ->
            ssl_gen_statem:handle_own_alert(Alert, ?STATE(wait_finished), State0)
    end;
wait_finished(info, Msg, State) ->
    tls_gen_connection:gen_info(Msg, ?STATE(wait_finished), State);
wait_finished(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?STATE(wait_finished), State).

%%--------------------------------------------------------------------
-spec wait_eoed(gen_statem:event_type(),
                #end_of_early_data{} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_eoed(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?STATE(wait_eoed), State,[]};
wait_eoed(internal = Type, #change_cipher_spec{} = Msg, State) ->
    tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg, wait_eoed, State);
wait_eoed(internal, #end_of_early_data{}, #state{handshake_env = HsEnv0} = State0) ->
    try
        State = ssl_record:step_encryption_state_read(State0),
        HsEnv = HsEnv0#handshake_env{early_data_accepted = false},
        tls_gen_connection:next_event(wait_finished, no_record, State#state{handshake_env = HsEnv})
    catch
        error:Reason:ST ->
            ?SSL_LOG(info, internal_error, [{error, Reason}, {stacktrace, ST}]),
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason),
                                            wait_eoed, State0)
    end;
wait_eoed(info, Msg, State) ->
    tls_gen_connection:gen_info(Msg, ?STATE(wait_eoed), State);
wait_eoed(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?STATE(wait_eoed), State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
                 term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(info, Msg, State) ->
    tls_gen_connection:gen_info(Msg, connection, State);
connection(Type, Msg, State) ->
    tls_gen_connection_1_3:connection(Type, Msg, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(),
                term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Msg, State) ->
    tls_gen_connection_1_3:downgrade(Type, Msg, State).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
handle_client_hello(ClientHello, State0) ->
    case do_handle_client_hello(ClientHello, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, start, State0);
        {State, start} ->
            {next_state, start, State, []};
        {State, negotiated, PSK} ->  %% Session Resumption with PSK i PSK =/= undefined
            {next_state, negotiated, State, [{next_event, internal, {start_handshake, PSK}}]}
    end.

do_handle_client_hello(#client_hello{cipher_suites = ClientCiphers,
                                     random = Random,
                                     session_id = SessionId,
                                     extensions = Extensions} = Hello, State0) ->
    SNI = maps:get(sni, Extensions, undefined),
    EarlyDataIndication = maps:get(early_data, Extensions, undefined),
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        #state{connection_states = ConnectionStates0,
               session = Session0,
               ssl_options = #{ciphers := ServerCiphers,
                               signature_algs := ServerSignAlgs,
                               supported_groups := ServerGroups0,
                               alpn_preferred_protocols := ALPNPreferredProtocols,
                               honor_cipher_order := HonorCipherOrder},
               connection_env = #connection_env{cert_key_alts = CertKeyAlts}
              } = State1 =
            Maybe(ssl_gen_statem:handle_sni_extension(SNI, State0)),

        ClientGroups0 = Maybe(tls_handshake_1_3:supported_groups_from_extensions(Extensions)),
        ClientGroups = Maybe(tls_handshake_1_3:get_supported_groups(ClientGroups0)),
        ServerGroups = Maybe(tls_handshake_1_3:get_supported_groups(ServerGroups0)),

        ClientShares = maps:get(key_share, Extensions, []),
        OfferedPSKs = maps:get(pre_shared_key, Extensions, undefined),

        ClientALPN0 = maps:get(alpn, Extensions, undefined),
        ClientALPN = ssl_handshake:decode_alpn(ClientALPN0),

        ClientSignAlgs = tls_handshake_1_3:get_signature_scheme_list(
                           maps:get(signature_algs, Extensions, undefined)),
        ClientSignAlgsCert = tls_handshake_1_3:get_signature_scheme_list(
                               maps:get(signature_algs_cert, Extensions, undefined)),
               CertAuths = tls_handshake_1_3:get_certificate_authorities(maps:get(certificate_authorities,
                                                                                  Extensions, undefined)),
        Cookie = maps:get(cookie, Extensions, undefined),

        Maybe(validate_cookie(Cookie, State1)),

        %% Handle ALPN extension if ALPN is configured
        ALPNProtocol = Maybe(handle_alpn(ALPNPreferredProtocols, ClientALPN)),

        %% If the server does not select a PSK, then the server independently selects a
        %% cipher suite, an (EC)DHE group and key share for key establishment,
        %% and a signature algorithm/certificate pair to authenticate itself to
        %% the client.
        Cipher = Maybe(select_cipher_suite(HonorCipherOrder, ClientCiphers, ServerCiphers)),
        Groups = Maybe(tls_handshake_1_3:select_common_groups(ServerGroups, ClientGroups)),
        Maybe(validate_client_key_share(ClientGroups,
                                        ClientShares#key_share_client_hello.client_shares)),
        CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts, ?TLS_1_3),
        #session{own_certificates = [Cert|_]} = Session =
            Maybe(select_server_cert_key_pair(Session0, CertKeyPairs, ClientSignAlgs,
                                              ClientSignAlgsCert, CertAuths, State0,
                                              undefined)),
        {PublicKeyAlgo, _, _, RSAKeySize, Curve} = tls_handshake_1_3:get_certificate_params(Cert),

        %% Select signature algorithm (used in CertificateVerify message).
        SelectedSignAlg = Maybe(tls_handshake_1_3:select_sign_algo(PublicKeyAlgo,
                                                                   RSAKeySize, ClientSignAlgs,
                                                                   ServerSignAlgs, Curve)),

        %% Select client public key. If no public key found in ClientShares or
        %% ClientShares is empty, trigger HelloRetryRequest as we were able
        %% to find an acceptable set of parameters but the ClientHello does not
        %% contain sufficient information.
        {Group, ClientPubKey} = select_client_public_key(Groups, ClientShares),

        %% Generate server_share
        KeyShare = ssl_cipher:generate_server_share(Group),

        State2 = case maps:get(max_frag_enum, Extensions, undefined) of
                      MaxFragEnum when is_record(MaxFragEnum, max_frag_enum) ->
                         ConnectionStates1 =
                             ssl_record:set_max_fragment_length(MaxFragEnum, ConnectionStates0),
                         HsEnv1 = (State1#state.handshake_env)#handshake_env{max_frag_enum =
                                                                                 MaxFragEnum},
                         State1#state{handshake_env = HsEnv1,
                                      session = Session,
                                      connection_states = ConnectionStates1};
                     _ ->
                         State1#state{session = Session}
                 end,

        Opts = State2#state.ssl_options,
        State3 = case maps:get(keep_secrets, Opts, false) of
                     false ->
                         State2;
                     _ ->
                         tls_handshake_1_3:set_client_random(State2, Hello#client_hello.random)
                 end,

        State4 = tls_handshake_1_3:update_start_state(State3,
                                                      #{cipher => Cipher,
                                                        key_share => KeyShare,
                                                        session_id => SessionId,
                                                        group => Group,
                                                        sign_alg => SelectedSignAlg,
                                                        peer_public_key => ClientPubKey,
                                                        alpn => ALPNProtocol,
                                                        random => Random}),

        %% 4.1.4.  Hello Retry Request
        %%
        %% The server will send this message in response to a ClientHello
        %% message if it is able to find an acceptable set of parameters but the
        %% ClientHello does not contain sufficient information to proceed with
        %% the handshake.
        case Maybe(send_hello_retry_request(State4, ClientPubKey, KeyShare, SessionId)) of
            {_, start} = NextStateTuple ->
                NextStateTuple;
            {#state{ssl_options = #{early_data := EarlyDataEnabled}} = State5, negotiated} ->
                %% Determine if early data is accepted
                State = handle_early_data(State5, EarlyDataEnabled, EarlyDataIndication),
                %% Exclude any incompatible PSKs.
                PSK = Maybe(tls_handshake_1_3:handle_pre_shared_key(State, OfferedPSKs, Cipher)),
                Maybe(session_resumption({State, negotiated}, PSK))
        end
    catch
        {Ref, #alert{} = Alert} ->
            Alert;
        error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{reason, Reason}, {stacktrace, ST}]),
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, illegal_parameter_in_client_hello)
    end.

send_hello_flight({start_handshake, PSK0},
                           #state{connection_states = ConnectionStates0,
                                  handshake_env =
                                      #handshake_env{
                                         key_share = KeyShare,
                                         early_data_accepted = EarlyDataAccepted},
                                  static_env = #static_env{protocol_cb = Connection},
                                  session = #session{session_id = SessionId,
                                                     ecc = SelectedGroup,
                                                     dh_public_value = ClientPublicKey},
                                  ssl_options = #{} = SslOpts
                                 } = State0) ->
    ServerPrivateKey = select_server_private_key(KeyShare),

    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{prf_algorithm = HKDF} = SecParamsR,

    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        %% Create server_hello
        ServerHello = tls_handshake_1_3:server_hello(server_hello, SessionId,
                                                     KeyShare, PSK0, ConnectionStates0),
        State1 = Connection:queue_handshake(ServerHello, State0),
        %% D.4.  Middlebox Compatibility Mode
        State2 = tls_gen_connection_1_3:maybe_queue_change_cipher_spec(State1, last),

        PSK = tls_handshake_1_3:get_pre_shared_key(PSK0, HKDF),

        State3 =
            tls_handshake_1_3:calculate_handshake_secrets(ClientPublicKey,
                                                          ServerPrivateKey, SelectedGroup,
                                                          PSK, State2),
        %% Step only write state if early_data is accepted
        State4 =
            case EarlyDataAccepted of
                true ->
                    ssl_record:step_encryption_state_write(State3);
                false ->
                    %% Read state is overwritten when handshake secrets are set.
                    %% Trial_decryption and early_data_accepted must be set here!
                    update_current_read(
                      ssl_record:step_encryption_state(State3),
                      true,   %% trial_decryption
                      false   %% early_data_accepted
                    )

            end,

        %% Create EncryptedExtensions
        EncryptedExtensions = tls_handshake_1_3:encrypted_extensions(State4),

        %% Encode EncryptedExtensions
        State5 = Connection:queue_handshake(EncryptedExtensions, State4),

        %% Create and send CertificateRequest ({verify, verify_peer})
        {State6, NextState} = maybe_send_certificate_request(State5, SslOpts, PSK0),

        %% Create and send Certificate (if PSK is undefined)
        State7 = Maybe(maybe_send_certificate(State6, PSK0)),

        %% Create and send CertificateVerify (if PSK is undefined)
        State8 = Maybe(maybe_send_certificate_verify(State7, PSK0)),

        %% Create Finished
        Finished = tls_handshake_1_3:finished(State8),

        %% Encode Finished
        State9 = Connection:queue_handshake(Finished, State8),

        %% Send first flight
        {State, _} = Connection:send_handshake_flight(State9),

        {State, NextState}

    catch
        {Ref, #alert{} = Alert} ->
            Alert;
        error:Reason:ST ->
            ?SSL_LOG(info, crypto_error, [{reason, Reason}, {stacktrace, ST}]),
            ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, illegal_parameter_to_compute_key)
    end.

validate_cookie(_Cookie, #state{ssl_options = #{cookie := false}}) ->
    ok;
validate_cookie(undefined, #state{ssl_options = #{cookie := true}}) ->
    ok;
validate_cookie(#cookie{cookie = Cookie0}, #state{ssl_options = #{cookie := true},
                                                  handshake_env =
                                                      #handshake_env{
                                                        tls_handshake_history =
                                                             {[_CH2,_HRR,MessageHash|_], _},
                                                         cookie_iv_shard = {IV, Shard}}}) ->
    Cookie = ssl_cipher:decrypt_data(<<"cookie">>, Cookie0, Shard, IV),
    case Cookie =:= iolist_to_binary(MessageHash) of
        true ->
            ok;
        false ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)}
    end;
validate_cookie(_,_) ->
    {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)}.

session_resumption({#state{ssl_options = #{session_tickets := disabled}} = State,
                    negotiated}, _) ->
    {ok, {State, negotiated, undefined}}; % Resumption prohibited
session_resumption({#state{ssl_options = #{session_tickets := Tickets}} = State,
                    negotiated}, undefined = PSK)
  when Tickets =/= disabled ->
    {ok, {State, negotiated, PSK}}; % No resumption
session_resumption({#state{ssl_options = #{session_tickets := Tickets},
                           handshake_env = #handshake_env{
                                              early_data_accepted = false}} = State0,
                    negotiated}, PSKInfo)
  when Tickets =/= disabled -> % Resumption but early data prohibited
    State1 = tls_gen_connection_1_3:handle_resumption(State0, ok),
    {Index, PSK, PeerCert} = PSKInfo,
    State = maybe_store_peer_cert(State1, PeerCert),
    {ok, {State, negotiated, {Index, PSK}}};
session_resumption({#state{ssl_options = #{session_tickets := Tickets},
                           handshake_env = #handshake_env{
                                              early_data_accepted = true}} = State0,
                    negotiated}, PSKInfo)
  when Tickets =/= disabled -> % Resumption with early data allowed
    State1 = tls_gen_connection_1_3:handle_resumption(State0, ok),
    %% TODO Refactor PSK-tuple {Index, PSK}, index might not be needed.
    {Index, PSK, PeerCert} = PSKInfo,
    State2 = tls_handshake_1_3:calculate_client_early_traffic_secret(State1, PSK),
    %% Set 0-RTT traffic keys for reading early_data
    State3 = ssl_record:step_encryption_state_read(State2),
    State4 = maybe_store_peer_cert(State3, PeerCert),
    State = update_current_read(State4, true, true),
    {ok, {State, negotiated, {Index, PSK}}}.

maybe_store_peer_cert(State, undefined) ->
    State;
maybe_store_peer_cert(#state{session = Session} = State, PeerCert) ->
    State#state{session = Session#session{peer_certificate = PeerCert}}.

maybe_send_session_ticket(State) ->
    Number = case application:get_env(ssl, server_session_tickets_amount) of
                 {ok, Size} when is_integer(Size) andalso
                                 Size > 0 ->
                     Size;
                 _  ->
                     3
             end,
    maybe_send_session_ticket(State, Number).

maybe_send_session_ticket(#state{ssl_options = #{session_tickets := disabled}} = State, _) ->
    %% Do nothing!
    State;
maybe_send_session_ticket(State, 0) ->
    State;
maybe_send_session_ticket(#state{connection_states = ConnectionStates,
                                 static_env = #static_env{trackers = Trackers,
                                                          protocol_cb = Connection}
                                } = State0, N) ->
    Tracker = proplists:get_value(session_tickets_tracker, Trackers),
    #{security_parameters := SecParamsR} =
        ssl_record:current_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDF,
                         resumption_master_secret = RMS} = SecParamsR,
    Ticket = new_session_ticket(Tracker, HKDF, RMS, State0),
    {State, _} = Connection:send_handshake(Ticket, State0),
    maybe_send_session_ticket(State, N - 1).

new_session_ticket(Tracker, HKDF, RMS,
                   #state{ssl_options = #{session_tickets := stateful_with_cert},
                          session = #session{peer_certificate = PeerCert}}) ->
    tls_server_session_ticket:new(Tracker, HKDF, RMS, PeerCert);
new_session_ticket(Tracker, HKDF, RMS,
                   #state{ssl_options = #{session_tickets := stateless_with_cert},
                          session = #session{peer_certificate = PeerCert}}) ->
    tls_server_session_ticket:new(Tracker, HKDF, RMS, PeerCert);
new_session_ticket(Tracker, HKDF, RMS, _) ->
    tls_server_session_ticket:new(Tracker, HKDF, RMS, undefined).

select_server_cert_key_pair(_,[], _,_,_,_, #session{}=Session) ->
    %% Conformant Cert-Key pair with advertised signature algorithm is
    %% selected.
    {ok, Session};
select_server_cert_key_pair(_,[], _,_,_,_, {fallback, #session{}=Session}) ->
    %% Use fallback Cert-Key pair as no conformant pair to the advertised
    %% signature algorithms was found.
    {ok, Session};
select_server_cert_key_pair(_,[], _,_,_,_, undefined) ->
    {error, ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, unable_to_supply_acceptable_cert)};
select_server_cert_key_pair(Session, [#{private_key := Key, certs := [Cert| _] = Certs} | Rest],
                            ClientSignAlgs, ClientSignAlgsCert, CertAuths,
                            #state{static_env = #static_env{cert_db = CertDbHandle,
                                                            cert_db_ref = CertDbRef}} = State,
                            Default0) ->
    {_, SignAlgo, SignHash, _, _} = tls_handshake_1_3:get_certificate_params(Cert),
    %% TODO: We do validate the signature algorithm and signature hash
    %% but we could also check if the signing cert has a key on a
    %% curve supported by the client for ECDSA/EDDSA certs
    case tls_handshake_1_3:check_cert_sign_algo(SignAlgo, SignHash,
                                                ClientSignAlgs, ClientSignAlgsCert) of
        ok ->
            case ssl_certificate:handle_cert_auths(Certs, CertAuths, CertDbHandle, CertDbRef) of
                {ok, EncodeChain} -> %% Chain fullfills certificate_authorities extension
                    {ok, Session#session{own_certificates = EncodeChain, private_key = Key}};
                {error, EncodeChain, not_in_auth_domain} ->
                    %% If this is the first chain to fulfill the
                    %% signing requirement, use it as default, if not
                    %% later alternative also fulfills
                    %% certificate_authorities extension
                    Default = Session#session{own_certificates = EncodeChain, private_key = Key},
                    select_server_cert_key_pair(Session, Rest, ClientSignAlgs, ClientSignAlgsCert,
                                                CertAuths, State,
                                                default_or_fallback(Default0, Default))
            end;
        _ ->
            %% If the server cannot produce a certificate chain that
            %% is signed only via the indicated supported algorithms,
            %% then it SHOULD continue the handshake by sending the
            %% client a certificate chain of its choice
            case SignHash of
                sha ->
                    %%  According to "Server Certificate Selection -
                    %%  RFC 8446" Never send cert using sha1 unless
                    %%  client allows it
                    select_server_cert_key_pair(Session, Rest, ClientSignAlgs, ClientSignAlgsCert,
                                                CertAuths, State, Default0);
                _ ->
                    %% If there does not exist a default or fallback
                    %% from previous alternatives use this alternative
                    %% as fallback.
                    Fallback = {fallback,
                                Session#session{own_certificates = Certs, private_key = Key}},
                    select_server_cert_key_pair(Session, Rest, ClientSignAlgs, ClientSignAlgsCert,
                                                CertAuths, State,
                                                default_or_fallback(Default0, Fallback))
            end
    end.

default_or_fallback(undefined, DefaultOrFallback) ->
    DefaultOrFallback;
default_or_fallback({fallback, _}, #session{} = Default) ->
    Default;
default_or_fallback(Default, _) ->
    Default.

select_server_private_key(#key_share_server_hello{server_share = ServerShare}) ->
    select_private_key(ServerShare).

select_private_key(#key_share_entry{
                   key_exchange = #'ECPrivateKey'{} = PrivateKey}) ->
    PrivateKey;
select_private_key(#key_share_entry{
                      key_exchange =
                          {_, PrivateKey}}) ->
    PrivateKey.


select_client_public_key([Group|_] = Groups, ClientShares) ->
    select_client_public_key(Groups, ClientShares, Group).

select_client_public_key([], _, PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
select_client_public_key([Group|Groups],
                         #key_share_client_hello{client_shares = ClientShares} = KeyS,
                         PreferredGroup) ->
     case lists:keysearch(Group, 2, ClientShares) of
         {value, #key_share_entry{key_exchange = ClientPublicKey}} ->
             {Group, ClientPublicKey};
         false ->
             select_client_public_key(Groups, KeyS, PreferredGroup)
     end.

send_hello_retry_request(#state{connection_states = ConnectionStates0,
                                static_env = #static_env{protocol_cb = Connection}} = State0,
                         no_suitable_key, KeyShare, SessionId) ->
    ServerHello0 = tls_handshake_1_3:server_hello(hello_retry_request, SessionId,
                                                  KeyShare, undefined, ConnectionStates0),
    {State1, ServerHello} = tls_handshake_1_3:maybe_add_cookie_extension(State0, ServerHello0),

    State2 = Connection:queue_handshake(ServerHello, State1),
    %% D.4.  Middlebox Compatibility Mode
    State3 = tls_gen_connection_1_3:maybe_queue_change_cipher_spec(State2, last),
    {State4, _} = Connection:send_handshake_flight(State3),

    %% Update handshake history
    State5 = tls_handshake_1_3:replace_ch1_with_message_hash(State4),

    {ok, {State5, start}};
send_hello_retry_request(State0, _, _, _) ->
    %% Suitable key found.
    {ok, {State0, negotiated}}.

update_current_read(#state{connection_states = CS} = State, TrialDecryption, EarlyDataExpected) ->
    #{early_data := EarlyData0} = Read0 = ssl_record:current_connection_state(CS, read),
    EarlyData = EarlyData0#{trial_decryption => TrialDecryption,
                            early_data_accepted => EarlyDataExpected},
    Read = Read0#{early_data := EarlyData},
    State#state{connection_states = CS#{current_read => Read}}.

handle_early_data(State, enabled, #early_data_indication{}) ->
    %% Accept early data
    HsEnv = (State#state.handshake_env)#handshake_env{early_data_accepted = true},
    State#state{handshake_env = HsEnv};
handle_early_data(State, _, _) ->
    State.

%% Session resumption with early_data
maybe_send_certificate_request(#state{
                                  handshake_env =
                                      #handshake_env{
                                         early_data_accepted = true}} = State,
                               _, PSK) when PSK =/= undefined ->
    %% Go wait for End of Early Data
    {State, wait_eoed};
%% Do not send CR during session resumption
maybe_send_certificate_request(State, _, PSK) when PSK =/= undefined ->
    {State, wait_finished};
maybe_send_certificate_request(State, #{verify := verify_none}, _) ->
    {State, wait_finished};
maybe_send_certificate_request(#state{static_env = #static_env{protocol_cb = Connection,
                                                               cert_db = CertDbHandle,
                                                               cert_db_ref = CertDbRef}} = State,
                               #{verify := verify_peer,
                                 signature_algs := SignAlgs,
                                 signature_algs_cert := SignAlgsCert} = Opts, _) ->
    AddCertAuth = maps:get(certificate_authorities, Opts, true),
    CertificateRequest = tls_handshake_1_3:certificate_request(SignAlgs, SignAlgsCert,
                                                               CertDbHandle,
                                                               CertDbRef, AddCertAuth),
    {Connection:queue_handshake(CertificateRequest, State), wait_cert}.

maybe_send_certificate(State, PSK) when  PSK =/= undefined ->
    {ok, State};
maybe_send_certificate(#state{session = #session{own_certificates = OwnCerts},
                              static_env = #static_env{
                                              protocol_cb = Connection,
                                              cert_db = CertDbHandle,
                                              cert_db_ref = CertDbRef}} = State, _) ->
    case tls_handshake_1_3:certificate(OwnCerts, CertDbHandle, CertDbRef, <<>>, server) of
        {ok, Certificate} ->
            {ok, Connection:queue_handshake(Certificate, State)};
        Error ->
            Error
    end.

maybe_send_certificate_verify(State, PSK) when  PSK =/= undefined ->
    {ok, State};
maybe_send_certificate_verify(#state{session = #session{sign_alg = SignatureScheme,
                                                        private_key = CertPrivateKey},
                                     static_env = #static_env{protocol_cb = Connection}
                                    } = State, _) ->
    case tls_handshake_1_3:certificate_verify(CertPrivateKey, SignatureScheme, State, server) of
        {ok, CertificateVerify} ->
            {ok, Connection:queue_handshake(CertificateVerify, State)};
        Error ->
            Error
    end.

%% RFC 8446 - 4.2.8.  Key Share
%% This vector MAY be empty if the client is requesting a
%% HelloRetryRequest.  Each KeyShareEntry value MUST correspond to a
%% group offered in the "supported_groups" extension and MUST appear in
%% the same order.  However, the values MAY be a non-contiguous subset
%% of the "supported_groups" extension and MAY omit the most preferred
%% groups.
%%
%% Clients can offer as many KeyShareEntry values as the number of
%% supported groups it is offering, each representing a single set of
%% key exchange parameters.
%%
%% Clients MUST NOT offer multiple KeyShareEntry values
%% for the same group.  Clients MUST NOT offer any KeyShareEntry values
%% for groups not listed in the client's "supported_groups" extension.
%% Servers MAY check for violations of these rules and abort the
%% handshake with an "illegal_parameter" alert if one is violated.
validate_client_key_share(_ ,[]) ->
    ok;
validate_client_key_share([], _) ->
    {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)};
validate_client_key_share([Group | ClientGroups],
                          [#key_share_entry{group = Group} | ClientShares]) ->
    validate_client_key_share(ClientGroups, ClientShares);
validate_client_key_share([_|ClientGroups], [_|_] = ClientShares) ->
    validate_client_key_share(ClientGroups, ClientShares).

select_cipher_suite(_, [], _) ->
    {error, ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_cipher)};
%% If honor_cipher_order is set to true, use the server's preference for
%% cipher suite selection.
select_cipher_suite(true, ClientCiphers, ServerCiphers) ->
    select_cipher_suite(false, ServerCiphers, ClientCiphers);
select_cipher_suite(false, [Cipher|ClientCiphers], ServerCiphers) ->
    case lists:member(Cipher, tls_v1:exclusive_suites(?TLS_1_3)) andalso
        lists:member(Cipher, ServerCiphers) of
        true ->
            {ok, Cipher};
        false ->
            select_cipher_suite(false, ClientCiphers, ServerCiphers)
    end.

%% RFC 7301 - Application-Layer Protocol Negotiation Extension
%% It is expected that a server will have a list of protocols that it
%% supports, in preference order, and will only select a protocol if the
%% client supports it.  In that case, the server SHOULD select the most
%% highly preferred protocol that it supports and that is also
%% advertised by the client.  In the event that the server supports no
%% protocols that the client advertises, then the server SHALL respond
%% with a fatal "no_application_protocol" alert.
handle_alpn(undefined, _) ->
    {ok, undefined};
handle_alpn([], _) ->
    {error,  ?ALERT_REC(?FATAL, ?NO_APPLICATION_PROTOCOL)};
handle_alpn([_|_], undefined) ->
    {ok, undefined};
handle_alpn([ServerProtocol|T], ClientProtocols) ->
    case lists:member(ServerProtocol, ClientProtocols) of
        true ->
            {ok, ServerProtocol};
        false ->
            handle_alpn(T, ClientProtocols)
    end.
