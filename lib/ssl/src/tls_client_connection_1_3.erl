%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022-2023. All Rights Reserved.
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
%% Purpose: TLS-1.3 FSM (client side)
%%----------------------------------------------------------------------
%%                                 INITIAL_HELLO
%%                                  Client send
%%                                  first ClientHello
%%                                  |                     ---> CONFIG_ERROR
%%                                  |                     Send error to user
%%                                  |                     and shutdown
%%                                  |
%%                                  V
%% RFC 8446
%% A.1.  Client
%%
%%                               START <----+
%%                Send ClientHello |        | Recv HelloRetryRequest
%%           [K_send = early data] |        |
%%                                 v        |
%%            /                 WAIT_SH ----+
%%            |                    | Recv ServerHello
%%            |                    | K_recv = handshake
%%        Can |                    V
%%       send |                 WAIT_EE
%%      early |                    | Recv EncryptedExtensions
%%       data |           +--------+--------+
%%            |     Using |                 | Using certificate
%%            |       PSK |                 v
%%            |           |            WAIT_CERT_CR
%%            |           |        Recv |       | Recv CertificateRequest
%%            |           | Certificate |       v
%%            |           |             |    WAIT_CERT
%%            |           |             |       | Recv Certificate
%%            |           |             v       v
%%            |           |              WAIT_CV
%%            |           |                 | Recv CertificateVerify
%%            |           +> WAIT_FINISHED <+
%%            |                  | Recv Finished
%%            \                  | [Send EndOfEarlyData]
%%                               | K_send = handshake
%%                               | [Send Certificate [+ CertificateVerify]]
%%     Can send                  | Send Finished
%%     app data   -->            | K_send = K_recv = application
%%     after here                v
%%                           CONNECTED
%%

-module(tls_client_connection_1_3).

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
         wait_sh/3,
         hello_middlebox_assert/3,
         hello_retry_middlebox_assert/3,
         wait_ee/3,
         wait_cert_cr/3,
         wait_cert/3,
         wait_cv/3,
         wait_finished/3,
         connection/3,
         downgrade/3
        ]).

%% Internal API
-export([maybe_send_early_data/1,
         maybe_automatic_session_resumption/1
        ]).

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    [state_functions, state_enter].

init([?CLIENT_ROLE, Sender, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = #state{protocol_specific = Map} =
        tls_gen_connection_1_3:initial_state(?CLIENT_ROLE, Sender,
                                             Host, Port, Socket,
                                             Options, User, CbInfo),
    try
	State = ssl_gen_statem:init_ssl_config(State0#state.ssl_options,
                                          ?CLIENT_ROLE, State0),
        tls_gen_connection:initialize_tls_sender(State),
        gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch throw:Error ->
            EState = State0#state{protocol_specific = Map#{error => Error}},
            gen_statem:enter_loop(?MODULE, [], config_error, EState)
    end.

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
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(enter, _, State) ->
    {keep_state, State};
config_error(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%%--------------------------------------------------------------------
-spec user_hello(gen_statem:event_type(),
                 term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
user_hello(enter, _, State) ->
    {keep_state, State};
user_hello({call, From}, cancel, State) ->
    gen_statem:reply(From, ok),
    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?USER_CANCELED,
                                               user_canceled),
                                    ?FUNCTION_NAME, State);
user_hello({call, From}, {handshake_continue, NewOptions, Timeout},
           #state{handshake_env =  #handshake_env{continue_status = pause} = HSEnv,
                  ssl_options = Options0} = State0) ->
    try ssl:update_options(NewOptions, ?CLIENT_ROLE, Options0) of
        Options ->
            State = ssl_gen_statem:ssl_config(Options, ?CLIENT_ROLE, State0),
            {next_state, wait_sh, State#state{start_or_recv_from = From,
                                              handshake_env =
                                                  HSEnv#handshake_env{continue_status
                                                                      = continue}
                                             },
             [{{timeout, handshake}, Timeout, close}]}
    catch
        throw:{error, Reason} ->
            gen_statem:reply(From, {error, Reason}),
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason), ?FUNCTION_NAME, State0)
    end;
user_hello(Type, Msg, State) ->
    tls_gen_connection_1_3:user_hello(Type, Msg, State).

%%--------------------------------------------------------------------
-spec start(gen_statem:event_type(),
                #server_hello{} | #change_cipher_spec{} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
start(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
start(internal = Type, #change_cipher_spec{} = Msg, State) ->
   tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg,
                                                    ?FUNCTION_NAME, State);
start(internal,
      #server_hello{extensions =
                        #{server_hello_selected_version :=
                              #server_hello_selected_version{selected_version
                                                             = Version}}}
      = ServerHello,
      #state{ssl_options = #{handshake := full,
                             versions := SupportedVersions}} = State) ->
    case tls_record:is_acceptable_version(Version, SupportedVersions) of
        true ->
            handle_exlusive_1_3_hello_or_hello_retry_request(ServerHello, State);
        false ->
            ssl_gen_statem:handle_own_alert(
              ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State)
    end;
start(internal, #server_hello{extensions =
                                  #{server_hello_selected_version :=
                                        #server_hello_selected_version{
                                           selected_version = Version}}
                              = Extensions},
      #state{ssl_options = #{versions := SupportedVersions},
             start_or_recv_from = From,
             handshake_env = #handshake_env{continue_status = pause}}
      = State) ->
    case tls_record:is_acceptable_version(Version, SupportedVersions) of
        true ->
            {next_state, user_hello,
             State#state{start_or_recv_from = undefined},
             [{postpone, true},
              {reply, From, {ok, Extensions}}]};
        false ->
            ssl_gen_statem:handle_own_alert(
              ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State)
    end;
start(internal, #server_hello{} = ServerHello,
      #state{handshake_env =
                 #handshake_env{continue_status = continue}} = State) ->
    handle_exlusive_1_3_hello_or_hello_retry_request(ServerHello, State);
start(internal, #server_hello{}, State0) ->
    %% Missing mandantory TLS-1.3 extensions,
    %%so it is a previous version hello.
    ssl_gen_statem:handle_own_alert(
      ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State0);
start(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
start(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec wait_sh(gen_statem:event_type(),
                 #server_hello{} | #change_cipher_spec{} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_sh(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_sh(internal = Type, #change_cipher_spec{} = Msg, State)->
   tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg,
                                                    ?FUNCTION_NAME, State);
wait_sh(internal, #server_hello{extensions = Extensions},
        #state{handshake_env = #handshake_env{continue_status = pause},
               start_or_recv_from = From} = State) ->
    {next_state, user_hello,
     State#state{start_or_recv_from = undefined},
     [{postpone, true},{reply, From, {ok, Extensions}}]};
wait_sh(internal, #server_hello{session_id = ?EMPTY_ID} = Hello,
        #state{session = #session{session_id = ?EMPTY_ID},
               ssl_options = #{middlebox_comp_mode := false}} = State0) ->
    case handle_server_hello(Hello, State0) of
         #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, wait_sh, State0);
        {State1, start, ServerHello} ->
            %% hello_retry_request: go to start
             {next_state, start, State1, [{next_event, internal, ServerHello}]};
        {State1, wait_ee} ->
             tls_gen_connection:next_event(wait_ee, no_record, State1)
    end;
wait_sh(internal, #server_hello{} = Hello,
        #state{protocol_specific = PS,
               ssl_options = SSLOpts} = State0)
  when not is_map_key(middlebox_comp_mode, SSLOpts) ->
    IsRetry = maps:get(hello_retry, PS, false),
    case handle_server_hello(Hello, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, wait_sh, State0);
        {State1 = #state{}, start, ServerHello} ->
            %% hello_retry_request
            {next_state, start, State1, [{next_event, internal, ServerHello}]};
        {State1, wait_ee} when IsRetry == true ->
            tls_gen_connection:next_event(wait_ee, no_record, State1);
        {State1, wait_ee} when IsRetry == false ->
            tls_gen_connection:next_event(hello_middlebox_assert,
                                          no_record, State1)
    end;
wait_sh(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_sh(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec hello_middlebox_assert(gen_statem:event_type(),
                             #change_cipher_spec{} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello_middlebox_assert(enter, _, State) ->
    {keep_state, State};
hello_middlebox_assert(internal, #change_cipher_spec{}, State) ->
    tls_gen_connection:next_event(wait_ee, no_record, State);
hello_middlebox_assert(internal = Type, #encrypted_extensions{} = Msg,  #state{ssl_options = #{log_level := Level}} = State) ->
    ssl_logger:log(warning, Level, #{description => "Failed to assert middlebox server message",
                                     reason => [{missing, #change_cipher_spec{}}]}, ?LOCATION),
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State);
hello_middlebox_assert(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
hello_middlebox_assert(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec hello_retry_middlebox_assert(gen_statem:event_type(),
                                   #server_hello{} | #change_cipher_spec{}
                                  | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello_retry_middlebox_assert(enter, _, State) ->
    {keep_state, State};
hello_retry_middlebox_assert(internal, #change_cipher_spec{}, State) ->
    tls_gen_connection:next_event(wait_sh, no_record, State);
hello_retry_middlebox_assert(internal = Type, #server_hello{} = Msg, #state{ssl_options = #{log_level := Level}} = State) ->
    ssl_logger:log(warning, Level, #{description => "Failed to assert middlebox server message",
                                     reason => [{missing, #change_cipher_spec{}}]}, ?LOCATION),
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State);
hello_retry_middlebox_assert(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
hello_retry_middlebox_assert(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec wait_ee(gen_statem:event_type(),
              #encrypted_extensions{} | #change_cipher_spec{}
             | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_ee(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_ee(internal = Type, #change_cipher_spec{} = Msg, State) ->
   tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg,
                                                    ?FUNCTION_NAME, State);
wait_ee(internal, #encrypted_extensions{extensions = Extensions}, State0) ->
    case handle_encrypted_extensions(Extensions, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State0);
        {State, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State)
    end;
wait_ee(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_ee(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec wait_cert_cr(gen_statem:event_type(),
                   term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert_cr(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_cert_cr(internal = Type, #change_cipher_spec{} = Msg, State) ->
    tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg,
                                                     ?FUNCTION_NAME, State);
wait_cert_cr(internal, #certificate_1_3{} = Certificate, State0) ->
    case handle_certificate(Certificate, State0) of
        {#alert{} = Alert, State} ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cert_cr, State);
        {State1, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State1)
    end;
wait_cert_cr(internal, #certificate_request_1_3{} = CertificateRequest,
             State0) ->
    case handle_certificate_request(CertificateRequest, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cert_cr, State0);
        {State1, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State1)
   end;
wait_cert_cr(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_cert_cr(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec wait_cert(gen_statem:event_type(),
                term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert(Type, Msg, State) ->
    tls_gen_connection_1_3:wait_cert(Type, Msg, State).

%%--------------------------------------------------------------------
-spec wait_cv(gen_statem:event_type(),
              term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cv(internal,
          #certificate_verify_1_3{} = CertificateVerify, State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        {State, NextState}
            = Maybe(tls_handshake_1_3:verify_certificate_verify(State0,
                                                                CertificateVerify)),
        tls_gen_connection:next_event(NextState, no_record, State)
    catch
        {Ref, {#alert{} = Alert, AState}} ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cv, AState)
    end;
wait_cv(Type, Msg, State) ->
    tls_gen_connection_1_3:wait_cv(Type, Msg, State).

%%--------------------------------------------------------------------
-spec wait_finished(gen_statem:event_type(),
                  #finished{} | #change_cipher_spec{} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_finished(enter, _, State0) ->
    State = tls_gen_connection_1_3:handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_finished(internal = Type, #change_cipher_spec{} = Msg, State) ->
    tls_gen_connection_1_3:handle_change_cipher_spec(Type, Msg,
                                                     ?FUNCTION_NAME, State);
wait_finished(internal,
              #finished{verify_data = VerifyData},
              #state{static_env = #static_env{protocol_cb = Connection}}
              = State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        Maybe(tls_handshake_1_3:validate_finished(State0, VerifyData)),
        %% D.4.  Middlebox Compatibility Mode
        State1 = tls_gen_connection_1_3:maybe_queue_change_cipher_spec(State0,
                                                                       first),
        %% Signal change of cipher
        State2 = maybe_send_end_of_early_data(State1),
        %% Maybe send Certificate + CertificateVerify
        State3 = Maybe(maybe_queue_cert_cert_cv(State2)),
        Finished = tls_handshake_1_3:finished(State3),
        %% Encode Finished
        State4 = Connection:queue_handshake(Finished, State3),
        %% Send first flight
        {State5, _} = Connection:send_handshake_flight(State4),
        State6 = tls_handshake_1_3:calculate_traffic_secrets(State5),
        State7 =
            tls_handshake_1_3:maybe_calculate_resumption_master_secret(State6),
        ExporterMasterSecret = tls_handshake_1_3:calculate_exporter_master_secret(State7),
        State8 = tls_handshake_1_3:forget_master_secret(State7),
        %% Configure traffic keys
        State9 = ssl_record:step_encryption_state(State8),
        {Record, #state{protocol_specific = PS} = State} =
            ssl_gen_statem:prepare_connection(State9, tls_gen_connection),

        tls_gen_connection:next_event(connection, Record,
                                      State#state{protocol_specific =
                                                      PS#{exporter_master_secret =>
                                                              ExporterMasterSecret}},
                                      [{{timeout, handshake}, cancel}])
    catch
        {Ref, #alert{} = Alert} ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State0)
    end;
wait_finished(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_finished(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).
%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
                 term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
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
handle_exlusive_1_3_hello_or_hello_retry_request(ServerHello, State0) ->
    case do_handle_exlusive_1_3_hello_or_hello_retry_request(ServerHello,
                                                             State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, start, State0);
        {State, NextState} ->
            {next_state, NextState, State, []}
    end.

do_handle_exlusive_1_3_hello_or_hello_retry_request(
  #server_hello{cipher_suite = SelectedCipherSuite,
                random = Random,
                session_id = SessionId,
                extensions = Extensions},
  #state{static_env = #static_env{host = Host,
                                  port = Port,
                                  cert_db = CertDbHandle,
                                  cert_db_ref = CertDbRef,
                                  protocol_cb = Connection,
                                  transport_cb = Transport,
                                  socket = Socket},
         handshake_env = #handshake_env{renegotiation = {Renegotiation, _},
                                        ocsp_stapling_state = OcspState},
         connection_env = #connection_env{negotiated_version =
                                              NegotiatedVersion},
         protocol_specific = PS,
         ssl_options = #{ciphers := ClientCiphers,
                         supported_groups := ClientGroups0,
                         use_ticket := UseTicket,
                         session_tickets := SessionTickets,
                         log_level := LogLevel} = SslOpts,
                              session = Session0,
         connection_states = ConnectionStates0
        } = State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
                                              try
        ClientGroups =
            Maybe(tls_handshake_1_3:get_supported_groups(ClientGroups0)),
        Cookie = maps:get(cookie, Extensions, undefined),

        KeyShare = maps:get(key_share, Extensions, undefined),
        SelectedGroup = server_group(KeyShare),

        %% Upon receipt of this extension in a HelloRetryRequest, the client
        %% MUST verify that (1) the selected_group field corresponds to a group
        %% which was provided in the "supported_groups" extension in the
        %% original ClientHello and (2) the selected_group field does not
        %% correspond to a group which was provided in the "key_share" extension
        %% in the original ClientHello.  If either of these checks fails, then
        %% the client MUST abort the handshake with an "illegal_parameter"
        %% alert.
        case KeyShare of
            #key_share_hello_retry_request{} ->
                Maybe(validate_selected_group(SelectedGroup, ClientGroups));
            _ ->
                ok
        end,
        Maybe(validate_cipher_suite(SelectedCipherSuite, ClientCiphers)),

        %% Otherwise, when sending the new ClientHello, the client MUST
        %% replace the original "key_share" extension with one containing only a
        %% new KeyShareEntry for the group indicated in the selected_group field
        %% of the triggering HelloRetryRequest.
        ClientKeyShare = ssl_cipher:generate_client_shares([SelectedGroup]),
        TicketData =
            tls_handshake_1_3:get_ticket_data(self(), SessionTickets, UseTicket),
        OcspNonce = maps:get(ocsp_nonce, OcspState, undefined),
        Hello0 = tls_handshake:client_hello(Host, Port,
                                            ConnectionStates0, SslOpts,
                                            SessionId, Renegotiation,
                                            ClientKeyShare,
                                            TicketData, OcspNonce,
                                            CertDbHandle, CertDbRef),
        %% Echo cookie received in HelloRetryrequest
        Hello1 = tls_handshake_1_3:maybe_add_cookie_extension(Cookie, Hello0),

        %% Update state
        State1 =
            tls_handshake_1_3:update_start_state(State0,
                                                 #{cipher => SelectedCipherSuite,
                                                   key_share => ClientKeyShare,
                                                   session_id => SessionId,
                                                   group => SelectedGroup,
                                                   random => Random}),

        %% Replace ClientHello1 with a special synthetic handshake message
        State2 = tls_handshake_1_3:replace_ch1_with_message_hash(State1),
        #state{handshake_env =
                   #handshake_env{tls_handshake_history = HHistory0}} = State2,

        %% Update pre_shared_key extension with binders (TLS 1.3)
        Hello =
            tls_handshake_1_3:maybe_add_binders(Hello1, HHistory0,
                                                TicketData, NegotiatedVersion),

        {BinMsg0, ConnectionStates, HHistory} =
            Connection:encode_handshake(Hello,
                                        NegotiatedVersion,
                                        ConnectionStates0,
                                        HHistory0),

        %% D.4.  Middlebox Compatibility Mode
        {#state{handshake_env = HsEnv} = State3, BinMsg} =
            tls_gen_connection_1_3:maybe_prepend_change_cipher_spec(State2,
                                                                    BinMsg0),

        tls_socket:send(Transport, Socket, BinMsg),
        ssl_logger:debug(LogLevel, outbound, 'handshake', Hello),
        ssl_logger:debug(LogLevel, outbound, 'record', BinMsg),

        State = State3#state{
                  connection_states = ConnectionStates,
                  session = Session0#session{session_id =
                                                 Hello#client_hello.session_id},
                  handshake_env =
                      HsEnv#handshake_env{tls_handshake_history = HHistory},
                  key_share = ClientKeyShare},

        %% If it is a hello_retry and middlebox mode is
        %% used assert the change_cipher_spec  message
        %% that the server should send next
        case (maps:get(hello_retry, PS, false)) andalso
            (maps:get(middlebox_comp_mode, SslOpts, true))
        of
            true ->
                {State, hello_retry_middlebox_assert};
            false ->
                {State, wait_sh}
        end
    catch
        {Ref, #alert{} = Alert} ->
            Alert
    end.

handle_server_hello(#server_hello{cipher_suite = SelectedCipherSuite,
                                  random = Random,
                                  session_id = SessionId,
                                  extensions = Extensions} = ServerHello,
           #state{key_share = ClientKeyShare,
                  ssl_options = #{ciphers := ClientCiphers,
                                  supported_groups := ClientGroups0,
                                  session_tickets := SessionTickets,
                                  use_ticket := UseTicket}} = State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        ClientGroups =
            Maybe(tls_handshake_1_3:get_supported_groups(ClientGroups0)),
        ServerKeyShare = server_share(maps:get(key_share, Extensions)),
        ServerPreSharedKey = maps:get(pre_shared_key, Extensions, undefined),

        %% Go to state 'start' if server replies with 'HelloRetryRequest'.
        Maybe(tls_handshake_1_3:maybe_hello_retry_request(ServerHello, State0)),

        %% Resumption and PSK
        State1 = tls_gen_connection_1_3:handle_resumption(State0,
                                                          ServerPreSharedKey),

        Maybe(validate_cipher_suite(SelectedCipherSuite, ClientCiphers)),
        Maybe(validate_server_key_share(ClientGroups, ServerKeyShare)),

        %% Get server public key
        #key_share_entry{group = SelectedGroup,
                         key_exchange = ServerPublicKey} = ServerKeyShare,

        ClientPrivateKey =
            client_private_key(SelectedGroup,
                               ClientKeyShare#key_share_client_hello.client_shares),
        %% Update state
        State2 = tls_handshake_1_3:update_start_state(State1,
                                    #{cipher => SelectedCipherSuite,
                                     key_share => ClientKeyShare,
                                     session_id => SessionId,
                                     group => SelectedGroup,
                                     peer_public_key => ServerPublicKey,
                                     random => Random}),

        #state{connection_states = ConnectionStates} = State2,
        #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
        #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

        PSK = Maybe(tls_handshake_1_3:get_pre_shared_key(SessionTickets,
                                                         UseTicket,
                                                         HKDFAlgo,
                                                         ServerPreSharedKey)),
        State3 =
            tls_handshake_1_3:calculate_handshake_secrets(ServerPublicKey,
                                                          ClientPrivateKey,
                                                          SelectedGroup,
                                                          PSK, State2),
        State4 = ssl_record:step_encryption_state_read(State3),
        {State4, wait_ee}
    catch
        {Ref, {State, StateName, ServerHello}} ->
            {State, StateName, ServerHello};
        {Ref, #alert{} = Alert} ->
            Alert
    end.

handle_encrypted_extensions(Extensions, State0) ->
    {Ref, Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        ALPNProtocol0 = maps:get(alpn, Extensions, undefined),
        ALPNProtocol = decode_alpn(ALPNProtocol0),
        EarlyDataIndication = maps:get(early_data, Extensions, undefined),

        %% RFC 6066: handle received/expected maximum fragment length
        Maybe(maybe_max_fragment_length(Extensions, State0)),

        %% Check if early_data is accepted/rejected
        State1 = maybe_check_early_data_indication(EarlyDataIndication, State0),

        %% Go to state 'wait_finished' if using PSK.
        Maybe(maybe_resumption(State1)),

        %% Update state
        #state{handshake_env = HsEnv} = State1,
        State2 = State1#state{handshake_env =
                                  HsEnv#handshake_env{alpn = ALPNProtocol}},
        {State2, wait_cert_cr}
    catch
        {Ref, #alert{} = Alert} ->
            Alert;
        {Ref, {_, _} = Next} ->
            Next
    end.

handle_certificate(#certificate_1_3{} = Certificate, State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        Maybe(tls_handshake_1_3:process_certificate(Certificate, State0))
    catch
        {Ref, #alert{} = Alert} ->
            {Alert, State0};
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State}
    end.
handle_certificate_request(#certificate_request_1_3{} =
                               CertificateRequest, State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        Maybe(tls_handshake_1_3:process_certificate_request(
                CertificateRequest, State0))
    catch
        {Ref, #alert{} = Alert} ->
            {Alert, State0}
    end.

maybe_send_early_data(#state{
                         handshake_env =
                             #handshake_env{tls_handshake_history = {Hist, _}},
                         protocol_specific = #{sender := _Sender},
                         ssl_options = #{versions := [?TLS_1_3|_],
                                         use_ticket := UseTicket,
                                         session_tickets := SessionTickets,
                                         early_data := EarlyData} = _SslOpts0
                        } = State0) when UseTicket =/= [undefined] andalso
                                         EarlyData =/= undefined ->
    %% D.4.  Middlebox Compatibility Mode
    State1 = tls_gen_connection_1_3:maybe_queue_change_cipher_spec(State0, last),
    %% Early traffic secret
    EarlyDataSize = tls_handshake_1_3:early_data_size(EarlyData),
    case tls_handshake_1_3:get_pre_shared_key_early_data(SessionTickets,
                                                         UseTicket) of
        {ok, {PSK, Cipher, HKDF, MaxSize}} when EarlyDataSize =< MaxSize ->
            State2 =
                tls_handshake_1_3:calculate_client_early_traffic_secret(Hist,
                                                                        PSK,
                                                                        Cipher,
                                                                        HKDF,
                                                                        State1),
            %% Set 0-RTT traffic keys for sending early_data and EndOfEarlyData
            State3 = ssl_record:step_encryption_state_write(State2),
            {ok, tls_handshake_1_3:encode_early_data(Cipher, State3)};
        {ok, {_, _, _, MaxSize}} ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER,
                               {too_much_early_data, {max, MaxSize}})};
        {error, Alert} ->
            {error, Alert}
    end;
maybe_send_early_data(State) ->
    {ok, State}.

maybe_send_end_of_early_data(
  #state{
     handshake_env = #handshake_env{early_data_accepted = true},
     protocol_specific = #{sender := _Sender},
     ssl_options = #{versions := [?TLS_1_3|_],
                     use_ticket := UseTicket,
                     early_data := EarlyData},
     static_env = #static_env{protocol_cb = Connection}
    } = State0) when UseTicket =/= [undefined] andalso
                     EarlyData =/= undefined ->
    %% EndOfEarlydata is encrypted with the 0-RTT traffic keys
    State1 = Connection:queue_handshake(#end_of_early_data{}, State0),
    %% Use handshake keys after EndOfEarlyData is sent
    ssl_record:step_encryption_state_write(State1);
maybe_send_end_of_early_data(State) ->
    State.

%% Configure a suitable session ticket
maybe_automatic_session_resumption(#state{ssl_options =
                                              #{versions := [Version|_],
                                                ciphers := UserSuites,
                                                early_data := EarlyData,
                                                session_tickets :=
                                                    SessionTickets,
                                                server_name_indication := SNI}
                                          = SslOpts0
                                         } = State0)
  when ?TLS_GTE(Version, ?TLS_1_3) andalso
       SessionTickets =:= auto ->
    AvailableCipherSuites = ssl_handshake:available_suites(UserSuites, Version),
    HashAlgos = cipher_hash_algos(AvailableCipherSuites),
    Ciphers = tls_handshake_1_3:ciphers_for_early_data(AvailableCipherSuites),
    %% Find a pair of tickets KeyPair = {Ticket0, Ticket2} where
    %% Ticket0 satisfies requirements for early_data and session
    %% resumption while Ticket2 can only be used for session
    %% resumption.
    EarlyDataSize = tls_handshake_1_3:early_data_size(EarlyData),
    KeyPair =
        tls_client_ticket_store:find_ticket(self(), Ciphers, HashAlgos,
                                            SNI, EarlyDataSize),
    UseTicket = tls_handshake_1_3:choose_ticket(KeyPair, EarlyData),
    tls_client_ticket_store:lock_tickets(self(), [UseTicket]),
    State = State0#state{ssl_options = SslOpts0#{use_ticket => [UseTicket]}},
    {[UseTicket], State};
maybe_automatic_session_resumption(#state{
                                      ssl_options = #{use_ticket := UseTicket}
                                     } = State) ->
    {UseTicket, State}.

maybe_resumption(#state{handshake_env =
                            #handshake_env{resumption = true}} = State) ->
    {error, {State, wait_finished}};
maybe_resumption(_) ->
    ok.

server_group(undefined) ->
    undefined;
server_group(#key_share_server_hello{server_share = #key_share_entry{group = Group}}) ->
    Group;
server_group(#key_share_hello_retry_request{selected_group = Group}) ->
    Group.

server_share(#key_share_server_hello{server_share = Share}) ->
    Share;
server_share(#key_share_hello_retry_request{selected_group = Share}) ->
    Share.

client_private_key(Group, ClientShares) ->
    case lists:keysearch(Group, 2, ClientShares) of
        {value, #key_share_entry{key_exchange =
                                     ClientPrivateKey = #'ECPrivateKey'{}}} ->
            ClientPrivateKey;
        {value, #key_share_entry{key_exchange = {_, ClientPrivateKey}}} ->
                ClientPrivateKey;
        false ->
            no_suitable_key
    end.

maybe_check_early_data_indication(EarlyDataIndication,
                                  #state{
                                     handshake_env = HsEnv,
                                     ssl_options = #{versions := [?TLS_1_3|_],
                                                     use_ticket := UseTicket,
                                                     early_data := EarlyData}
                                    } = State)
  when UseTicket =/= [undefined] andalso
       EarlyData =/= undefined andalso
       EarlyDataIndication =/= undefined ->
    signal_user_early_data(State, accepted),
    State#state{handshake_env = HsEnv#handshake_env{early_data_accepted = true}};
maybe_check_early_data_indication(EarlyDataIndication,
                                  #state{
                                     protocol_specific = #{sender := _Sender},
                                     ssl_options = #{versions := [?TLS_1_3|_],
                                                     use_ticket := UseTicket,
                                                     early_data := EarlyData}
                                     = _SslOpts0
                                    } = State)
  when UseTicket =/= [undefined] andalso
       EarlyData =/= undefined andalso
       EarlyDataIndication =:= undefined ->
    signal_user_early_data(State, rejected),
    %% Use handshake keys if early_data is rejected.
    ssl_record:step_encryption_state_write(State);
maybe_check_early_data_indication(_, State) ->
    %% Use handshake keys if there is no early_data.
    ssl_record:step_encryption_state_write(State).

signal_user_early_data(#state{
                          connection_env =
                              #connection_env{
                                 user_application = {_, User}},
                          static_env =
                              #static_env{
                                 socket = Socket,
                                 protocol_cb = Connection,
                                 transport_cb = Transport,
                                 trackers = Trackers}} = State,
                       Result) ->
    CPids = Connection:pids(State),
    SslSocket = Connection:socket(CPids, Transport, Socket, Trackers),
    User ! {ssl, SslSocket, {early_data, Result}}.

maybe_max_fragment_length(Extensions, State) ->
    ServerMaxFragEnum = maps:get(max_frag_enum, Extensions, undefined),
    ClientMaxFragEnum = ssl_handshake:max_frag_enum(
                          maps:get(max_fragment_length,
                                   State#state.ssl_options, undefined)),
    if ServerMaxFragEnum == ClientMaxFragEnum ->
            ok;
       true ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)}
    end.

cipher_hash_algos(Ciphers) ->
    Fun = fun(Cipher) ->
                  #{prf := Hash} = ssl_cipher_format:suite_bin_to_map(Cipher),
                  Hash
          end,
    lists:map(Fun, Ciphers).

maybe_queue_cert_cert_cv(#state{client_certificate_status = not_requested}
                         = State) ->
    {ok, State};
maybe_queue_cert_cert_cv(#state{connection_states = _ConnectionStates0,
                                session = #session{session_id = _SessionId,
                                                   own_certificates = OwnCerts},
                                ssl_options = #{} = _SslOpts,
                                key_share = _KeyShare,
                                handshake_env =
                                    #handshake_env{tls_handshake_history =
                                                       _HHistory0},
                                static_env = #static_env{
                                                protocol_cb = Connection,
                                                cert_db = CertDbHandle,
                                                cert_db_ref = CertDbRef,
                                                socket = _Socket,
                                                transport_cb = _Transport}
                               } = State0) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        %% Create Certificate
        Certificate = Maybe(tls_handshake_1_3:certificate(OwnCerts,
                                                          CertDbHandle,
                                                          CertDbRef, <<>>,
                                                          client)),

        %% Encode Certificate
        State1 = Connection:queue_handshake(Certificate, State0),
        %% Maybe create and queue CertificateVerify
        State = Maybe(maybe_queue_cert_verify(Certificate, State1)),
        {ok, State}
    catch
        {Ref, #alert{} = Alert} ->
            {error, Alert}
    end.

%% Clients MUST send this message whenever authenticating via a certificate
%% (i.e., when the Certificate message is non-empty).
maybe_queue_cert_verify(#certificate_1_3{certificate_list = []}, State) ->
    {ok, State};
maybe_queue_cert_verify(_Certificate,
                        #state{connection_states = _ConnectionStates0,
                               session = #session{sign_alg = SignatureScheme,
                                                  private_key = CertPrivateKey},
                               static_env = #static_env{protocol_cb = Connection}
                              } = State) ->
    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        CertificateVerify =
            Maybe(tls_handshake_1_3:certificate_verify(CertPrivateKey,
                                                       SignatureScheme,
                                                       State, client)),
        {ok, Connection:queue_handshake(CertificateVerify, State)}
    catch
        {Ref, #alert{} = Alert} ->
            {error, Alert}
    end.

decode_alpn(undefined) ->
    undefined;
decode_alpn(Encoded) ->
    [Decoded] = ssl_handshake:decode_alpn(Encoded),
    Decoded.

%% Verify that selected group is offered by the client.
validate_server_key_share([], _) ->
    {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)};
validate_server_key_share([Group |_ClientGroups], #key_share_entry{group = Group}) ->
    ok;
validate_server_key_share([_|ClientGroups], #key_share_entry{} = ServerKeyShare) ->
    validate_server_key_share(ClientGroups, ServerKeyShare).


validate_selected_group(SelectedGroup, [SelectedGroup|_]) ->
    {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER,
                       "Selected group sent by the server shall not correspond to a group"
                       " which was provided in the key_share extension")};
validate_selected_group(SelectedGroup, ClientGroups) ->
    case lists:member(SelectedGroup, ClientGroups) of
        true ->
            ok;
        false ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER,
                               "Selected group sent by the server shall correspond to a group"
                               " which was provided in the supported_groups extension")}
    end.

%% RFC 8446 4.1.3 ServerHello
%% A client which receives a cipher suite that was not offered MUST abort the
%% handshake with an "illegal_parameter" alert.
validate_cipher_suite(Cipher, ClientCiphers) ->
    case lists:member(Cipher, ClientCiphers) of
        true ->
            ok;
        false ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)}
    end.
