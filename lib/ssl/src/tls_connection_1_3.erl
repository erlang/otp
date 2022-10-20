%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2022. All Rights Reserved.
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
%% Purpose: TLS-1.3 FSM
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

-module(tls_connection_1_3).

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
-export([initial_hello/3,
         config_error/3,
         user_hello/3,
         start/3,
         hello_middlebox_assert/3,
         hello_retry_middlebox_assert/3,
         negotiated/3,
         wait_cert/3,
         wait_cv/3,
         wait_finished/3,
         wait_sh/3,
         wait_ee/3,
         wait_cert_cr/3,
         wait_eoed/3,
         connection/3,
         downgrade/3
        ]).

%% Internal API
-export([setopts/3,
         getopts/3,
         maybe/0,
         send_key_update/2,
         update_cipher_key/2,
         maybe_send_early_data/1,
         maybe_automatic_session_resumption/1]).

%% Tracing
-export([handle_trace/3]).
%%====================================================================
%% Internal API
%%====================================================================

setopts(Transport, Socket, Other) ->
    tls_socket:setopts(Transport, Socket, Other).

getopts(Transport, Socket, Tag) ->
    tls_socket:getopts(Transport, Socket, Tag).

send_key_update(Sender, Type) ->
    KeyUpdate = tls_handshake_1_3:key_update(Type),
    tls_sender:send_post_handshake(Sender, KeyUpdate).

update_cipher_key(ConnStateName, #state{connection_states = CS0} = State0) ->
    CS = update_cipher_key(ConnStateName, CS0),
    State0#state{connection_states = CS};
update_cipher_key(ConnStateName, CS0) ->
    #{security_parameters := SecParams0,
      cipher_state := CipherState0} = ConnState0 = maps:get(ConnStateName, CS0),
    HKDF = SecParams0#security_parameters.prf_algorithm,
    CipherSuite = SecParams0#security_parameters.cipher_suite,
    ApplicationTrafficSecret0 = SecParams0#security_parameters.application_traffic_secret,
    ApplicationTrafficSecret = tls_v1:update_traffic_secret(HKDF, ApplicationTrafficSecret0),

    %% Calculate traffic keys
    KeyLength = tls_v1:key_length(CipherSuite),
    {Key, IV} = tls_v1:calculate_traffic_keys(HKDF, KeyLength, ApplicationTrafficSecret),

    SecParams = SecParams0#security_parameters{application_traffic_secret = ApplicationTrafficSecret},
    CipherState = CipherState0#cipher_state{key = Key, iv = IV},
    ConnState = ConnState0#{security_parameters => SecParams,
                            cipher_state => CipherState,
                            sequence_number => 0},
    CS0#{ConnStateName => ConnState}.

%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    [state_functions, state_enter].

init([Role, Sender, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = #state{protocol_specific = Map} = initial_state(Role, Sender,
                                                             Host, Port, Socket, Options, User, CbInfo),
    try
	State = ssl_gen_statem:ssl_config(State0#state.ssl_options, Role, State0),
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

user_hello(enter, _, State) ->
    {keep_state, State};
user_hello({call, From}, cancel, State) ->
    gen_statem:reply(From, ok),
    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?USER_CANCELED, user_canceled),
                                    ?FUNCTION_NAME, State);
user_hello({call, From}, {handshake_continue, NewOptions, Timeout},
           #state{static_env = #static_env{role = client = Role},
                  handshake_env = HSEnv,
                  ssl_options = Options0} = State0) ->
    Options = ssl:update_options(NewOptions, Role, Options0),
    State = ssl_gen_statem:ssl_config(Options, Role, State0),
    {next_state, wait_sh, State#state{start_or_recv_from = From,
                                      handshake_env = HSEnv#handshake_env{continue_status = continue}},
     [{{timeout, handshake}, Timeout, close}]};
user_hello({call, From}, {handshake_continue, NewOptions, Timeout},
           #state{static_env = #static_env{role = server = Role},
                  handshake_env = #handshake_env{continue_status = {pause, ClientVersions}} = HSEnv,
                  ssl_options = Options0} = State0) ->
    Options = #{versions := Versions} = ssl:update_options(NewOptions, Role, Options0),
    State = ssl_gen_statem:ssl_config(Options, Role, State0),
    case ssl_handshake:select_supported_version(ClientVersions, Versions) of
        {3,4} ->
            {next_state, start, State#state{start_or_recv_from = From,
                                            handshake_env = HSEnv#handshake_env{continue_status = continue}},
             [{{timeout, handshake}, Timeout, close}]};
        undefined ->
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State);
        _Else ->
            {next_state, hello, State#state{start_or_recv_from = From,
                                            handshake_env = HSEnv#handshake_env{continue_status = continue}},
             [{change_callback_module, tls_connection},
              {{timeout, handshake}, Timeout, close}]}
    end;
user_hello(info, {'DOWN', _, _, _, _} = Event, State) ->
    ssl_gen_statem:handle_info(Event, ?FUNCTION_NAME, State);
user_hello(_, _, _) ->
    {keep_state_and_data, [postpone]}.

start(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
start(internal = Type, #change_cipher_spec{} = Msg,
      #state{static_env = #static_env{role = server},
             handshake_env = #handshake_env{tls_handshake_history = Hist}} = State) ->
    case ssl_handshake:init_handshake_history() of
        Hist -> %% First message must always be client hello
            ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State);
        _ ->
            handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State)
        end;
start(internal = Type, #change_cipher_spec{} = Msg,
      #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
start(internal, #client_hello{extensions = #{client_hello_versions :=
                                                 #client_hello_versions{versions = ClientVersions}
                                            }} = Hello,
      #state{ssl_options = #{handshake := full}} = State) ->
    case tls_record:is_acceptable_version({3,4}, ClientVersions) of
        true ->
            do_server_start(Hello, State);
        false ->
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State)
    end;
start(internal, #client_hello{extensions = #{client_hello_versions :=
                                                  #client_hello_versions{versions = ClientVersions}
                                            }= Extensions},
      #state{start_or_recv_from = From,
             handshake_env = #handshake_env{continue_status = pause} = HSEnv} = State) ->
    {next_state, user_hello,
     State#state{start_or_recv_from = undefined, handshake_env = HSEnv#handshake_env{continue_status = {pause, ClientVersions}}},
     [{postpone, true}, {reply, From, {ok, Extensions}}]};
start(internal, #client_hello{} = Hello,
      #state{handshake_env = #handshake_env{continue_status = continue}} = State) ->
    do_server_start(Hello, State);
start(internal, #client_hello{}, State0) -> %% Missing mandantory TLS-1.3 extensions, so it is a previous version hello.
    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State0);
start(internal, #server_hello{extensions = #{server_hello_selected_version :=
                                                 #server_hello_selected_version{selected_version = Version}}} = ServerHello,
      #state{ssl_options = #{handshake := full,
                             versions := SupportedVersions}} = State) ->
    case tls_record:is_acceptable_version(Version, SupportedVersions) of
        true ->
            do_client_start(ServerHello, State);
        false ->
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State)
    end;
start(internal, #server_hello{extensions = #{server_hello_selected_version :=
                                                 #server_hello_selected_version{selected_version = Version}}
                              = Extensions},
      #state{ssl_options = #{versions := SupportedVersions},
             start_or_recv_from = From,
             handshake_env = #handshake_env{continue_status = pause}}
      = State) ->
    case tls_record:is_acceptable_version(Version, SupportedVersions) of
        true ->
            {next_state, user_hello,
             State#state{start_or_recv_from = undefined}, [{postpone, true}, {reply, From, {ok, Extensions}}]};
        false ->
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State)
    end;
start(internal, #server_hello{} = ServerHello,
      #state{handshake_env = #handshake_env{continue_status = continue}} = State) ->
    do_client_start(ServerHello, State);
start(internal, #server_hello{}, State0) -> %% Missing mandantory TLS-1.3 extensions, so it is a previous version hello.
    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?PROTOCOL_VERSION), ?FUNCTION_NAME, State0);
start(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
start(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

negotiated(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
negotiated(internal = Type, #change_cipher_spec{} = Msg,
           #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
negotiated(internal, Message, State0) ->
    case do_negotiated(Message, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, negotiated, State0);
        {State, NextState} ->
            {next_state, NextState, State, []}
    end;
negotiated(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State).

wait_cert(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_cert(internal = Type, #change_cipher_spec{} = Msg,
          #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_cert(internal,
          #certificate_1_3{} = Certificate, State0) ->
    case do_wait_cert(Certificate, State0) of
        {#alert{} = Alert, State} ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cert, State);
        {State, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State)
    end;
wait_cert(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_cert(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

wait_cv(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_cv(internal = Type, #change_cipher_spec{} = Msg,
        #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_cv(internal,
          #certificate_verify_1_3{} = CertificateVerify, State0) ->
    case do_wait_cv(CertificateVerify, State0) of
        {#alert{} = Alert, State} ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cv, State);
        {State, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State)
    end;
wait_cv(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_cv(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

wait_finished(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_finished(internal = Type, #change_cipher_spec{} = Msg,
              #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_finished(internal,
             #finished{} = Finished, State0) ->
    case do_wait_finished(Finished, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, finished, State0);
        State1 ->
            {Record, State} = ssl_gen_statem:prepare_connection(State1, tls_gen_connection),
            tls_gen_connection:next_event(connection, Record, State,
                                      [{{timeout, handshake}, cancel}])
    end;
wait_finished(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_finished(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

wait_sh(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_sh(internal = Type, #change_cipher_spec{} = Msg,
        #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_sh(internal, #server_hello{extensions = Extensions},
        #state{handshake_env = #handshake_env{continue_status = pause},
               start_or_recv_from = From} = State) ->
    {next_state, user_hello,
     State#state{start_or_recv_from = undefined}, [{postpone, true},{reply, From, {ok, Extensions}}]};
wait_sh(internal, #server_hello{session_id = ?EMPTY_ID} = Hello, #state{session = #session{session_id = ?EMPTY_ID},
                                                                        ssl_options = #{middlebox_comp_mode := false}} = State0) ->
    case do_wait_sh(Hello, State0) of
         #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, wait_sh, State0);
        {State1, start, ServerHello} ->
            %% hello_retry_request: go to start
             {next_state, start, State1, [{next_event, internal, ServerHello}]};
        {State1, wait_ee} ->
             tls_gen_connection:next_event(wait_ee, no_record, State1)
    end;
wait_sh(internal, #server_hello{} = Hello,
        #state{protocol_specific = PS, ssl_options = #{middlebox_comp_mode := true}} = State0) ->
    IsRetry = maps:get(hello_retry, PS, false),
    case do_wait_sh(Hello, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, wait_sh, State0);
        {State1 = #state{}, start, ServerHello} ->
            %% hello_retry_request : assert middlebox before going back to start
            {next_state, hello_retry_middlebox_assert, State1, [{next_event, internal, ServerHello}]};
        {State1, wait_ee} when IsRetry == true ->
            tls_gen_connection:next_event(wait_ee, no_record, State1);
        {State1, wait_ee} when IsRetry == false ->
            tls_gen_connection:next_event(hello_middlebox_assert, no_record, State1)
    end;
wait_sh(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_sh(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

hello_middlebox_assert(enter, _, State) ->
    {keep_state, State};
hello_middlebox_assert(internal, #change_cipher_spec{}, State) ->
    tls_gen_connection:next_event(wait_ee, no_record, State);
hello_middlebox_assert(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
hello_middlebox_assert(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

hello_retry_middlebox_assert(enter, _, State) ->
    {keep_state, State};
hello_retry_middlebox_assert(internal, #change_cipher_spec{}, State) ->
    tls_gen_connection:next_event(start, no_record, State);
hello_retry_middlebox_assert(internal, #server_hello{}, State) ->
    tls_gen_connection:next_event(?FUNCTION_NAME, no_record, State, [postpone]);
hello_retry_middlebox_assert(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
hello_retry_middlebox_assert(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

wait_ee(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_ee(internal = Type, #change_cipher_spec{} = Msg,
        #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_ee(internal, #encrypted_extensions{} = EE, State0) ->
    case do_wait_ee(EE, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, wait_ee, State0);
        {State1, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State1)
    end;
wait_ee(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_ee(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

wait_cert_cr(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_cert_cr(internal = Type, #change_cipher_spec{} = Msg,
             #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_cert_cr(internal, #certificate_1_3{} = Certificate, State0) ->
    case do_wait_cert_cr(Certificate, State0) of
        {#alert{} = Alert, State} ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cert_cr, State);
        {State1, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State1)
    end;
wait_cert_cr(internal, #certificate_request_1_3{} = CertificateRequest, State0) ->
    case do_wait_cert_cr(CertificateRequest, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cert_cr, State0);
        {State1, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State1)
    end;
wait_cert_cr(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_cert_cr(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

wait_eoed(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_eoed(internal = Type, #change_cipher_spec{} = Msg,
          #state{session = #session{session_id = Id}} = State) when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_eoed(internal, #end_of_early_data{} = EOED, State0) ->
    case do_wait_eoed(EOED, State0) of
        {#alert{} = Alert, State} ->
            ssl_gen_statem:handle_own_alert(Alert, wait_eoed, State);
        {State1, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State1)
    end;
wait_eoed(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_eoed(Type, Msg, State) ->
    ssl_gen_statem:handle_common_event(Type, Msg, ?FUNCTION_NAME, State).

connection(enter, _, State) ->
    {keep_state, State};
connection(internal, #new_session_ticket{} = NewSessionTicket, State) ->
    handle_new_session_ticket(NewSessionTicket, State),
    tls_gen_connection:next_event(?FUNCTION_NAME, no_record, State);

connection(internal, #key_update{} = KeyUpdate, State0) ->
    case handle_key_update(KeyUpdate, State0) of
        {ok, State} ->
            tls_gen_connection:next_event(?FUNCTION_NAME, no_record, State);
        {error, State, Alert} ->
            ssl_gen_statem:handle_own_alert(Alert, connection, State),
            tls_gen_connection:next_event(?FUNCTION_NAME, no_record, State)
    end;
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = undefined}} = State) ->
    ssl_gen_statem:hibernate_after(?FUNCTION_NAME, State, [{reply, From, {error, protocol_not_negotiated}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = SelectedProtocol,
                                                 negotiated_protocol = undefined}} = State) ->
    ssl_gen_statem:hibernate_after(?FUNCTION_NAME, State,
                    [{reply, From, {ok, SelectedProtocol}}]);
connection(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

downgrade(enter, _, State) ->
    {keep_state, State};
downgrade(internal, #new_session_ticket{} = NewSessionTicket, State) ->
    _ = handle_new_session_ticket(NewSessionTicket, State),
    {next_state, ?FUNCTION_NAME, State};
downgrade(Type, Event, State) ->
     ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
initial_state(Role, Sender, Host, Port, Socket, {SSLOptions, SocketOptions, Trackers}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
    put(log_level, maps:get(log_level, SSLOptions)),
    %% Use highest supported version for client/server random nonce generation
    #{versions := [Version|_]} = SSLOptions,
    MaxEarlyDataSize = init_max_early_data_size(Role),
    ConnectionStates = tls_record:init_connection_states(Role,
                                                         Version,
                                                         disabled,
                                                         MaxEarlyDataSize),
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
                     trackers = Trackers
                    },
    #state{
       static_env = InitStatEnv,
       handshake_env = #handshake_env{
                          tls_handshake_history = ssl_handshake:init_handshake_history(),
                          renegotiation = {false, first}
                         },
       connection_env = #connection_env{user_application = {UserMonitor, User}},
       socket_options = SocketOptions,
       ssl_options = SSLOptions,
       session = #session{is_resumable = false,
                          session_id = ssl_session:legacy_session_id(SSLOptions)},
       connection_states = ConnectionStates,
       protocol_buffers = #protocol_buffers{},
       user_data_buffer = {[],0,[]},
       start_or_recv_from = undefined,
       flight_buffer = [],
       protocol_specific = #{sender => Sender,
                             active_n => internal_active_n(SSLOptions, Socket),
                             active_n_toggle => true
                            }
      }.

internal_active_n(#{ktls := true}, Socket) ->
    inet:setopts(Socket, [{packet, ssl_tls}]),
    1;
internal_active_n(#{erl_dist := true}, _) ->
    %% Start with a random number between 1 and ?INTERNAL_ACTIVE_N
    %% In most cases distribution connections are established all at
    %%  the same time, and flow control engages with ?INTERNAL_ACTIVE_N for
    %%  all connections. Which creates a wave of "passive" messages, leading
    %%  to significant bump of memory & scheduler utilisation. Starting with
    %%  a random number between 1 and ?INTERNAL_ACTIVE_N helps to spread the
    %%  spike.
    erlang:system_time() rem ?INTERNAL_ACTIVE_N + 1;
internal_active_n(#{erl_dist := false}, _) ->
    case application:get_env(ssl, internal_active_n) of
        {ok, N} when is_integer(N) ->
            N;
        _  ->
            ?INTERNAL_ACTIVE_N
    end.

do_server_start(ClientHello, State0) ->
    case do_start(ClientHello, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, start, State0);
        {State, start} ->
            {next_state, start, State, []};
        {State, negotiated} ->
            {next_state, negotiated, State, [{next_event, internal, {start_handshake, undefined}}]};
        {State, negotiated, PSK} ->  %% Session Resumption with PSK
            {next_state, negotiated, State, [{next_event, internal, {start_handshake, PSK}}]}
    end.

do_client_start(ServerHello, State0) ->
    case do_start(ServerHello, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, start, State0);
        {State, NextState} ->
            {next_state, NextState, State, []}
    end.

do_start(#client_hello{cipher_suites = ClientCiphers,
                       session_id = SessionId,
                       extensions = Extensions} = Hello,
         #state{ssl_options = #{ciphers := ServerCiphers,
                                signature_algs := ServerSignAlgs,
                                supported_groups := ServerGroups0,
                                alpn_preferred_protocols := ALPNPreferredProtocols,
                                keep_secrets := KeepSecrets,
                                honor_cipher_order := HonorCipherOrder,
                                early_data := EarlyDataEnabled}} = State0) ->
    SNI = maps:get(sni, Extensions, undefined),
    EarlyDataIndication = maps:get(early_data, Extensions, undefined),
    {Ref,Maybe} = maybe(),
    try
        ClientGroups0 = Maybe(tls_handshake_1_3:supported_groups_from_extensions(Extensions)),
        ClientGroups = Maybe(get_supported_groups(ClientGroups0)),
        ServerGroups = Maybe(get_supported_groups(ServerGroups0)),

        ClientShares0 = maps:get(key_share, Extensions, undefined),
        ClientShares = get_key_shares(ClientShares0),

        OfferedPSKs = get_offered_psks(Extensions),

        ClientALPN0 = maps:get(alpn, Extensions, undefined),
        ClientALPN = decode_alpn(ClientALPN0),

        ClientSignAlgs = tls_handshake_1_3:get_signature_scheme_list(
                           maps:get(signature_algs, Extensions, undefined)),
        ClientSignAlgsCert = tls_handshake_1_3:get_signature_scheme_list(
                               maps:get(signature_algs_cert, Extensions, undefined)),
        CertAuths = tls_handshake_1_3:get_certificate_authorities(maps:get(certificate_authorities, Extensions, undefined)),
        Cookie = maps:get(cookie, Extensions, undefined),

        #state{connection_states = ConnectionStates0,
               session = Session0,
               connection_env = #connection_env{cert_key_alts = CertKeyAlts}} = State1 =
            Maybe(ssl_gen_statem:handle_sni_extension(SNI, State0)),

        Maybe(validate_cookie(Cookie, State1)),

        %% Handle ALPN extension if ALPN is configured
        ALPNProtocol = Maybe(tls_handshake_1_3:handle_alpn(ALPNPreferredProtocols, ClientALPN)),

        %% If the server does not select a PSK, then the server independently selects a
        %% cipher suite, an (EC)DHE group and key share for key establishment,
        %% and a signature algorithm/certificate pair to authenticate itself to
        %% the client.
        Cipher = Maybe(tls_handshake_1_3:select_cipher_suite(HonorCipherOrder, ClientCiphers, ServerCiphers)),
        Groups = Maybe(tls_handshake_1_3:select_common_groups(ServerGroups, ClientGroups)),
        Maybe(tls_handshake_1_3:validate_client_key_share(ClientGroups, ClientShares)),
        CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts, {3,4}),
        #session{own_certificates = [Cert|_]} = Session =
            Maybe(select_server_cert_key_pair(Session0, CertKeyPairs, ClientSignAlgs,
                                              ClientSignAlgsCert, CertAuths, State0,
                                              undefined)),
        {PublicKeyAlgo, _, _, RSAKeySize, Curve} = tls_handshake_1_3:get_certificate_params(Cert),

        %% Select signature algorithm (used in CertificateVerify message).
        SelectedSignAlg = Maybe(tls_handshake_1_3:select_sign_algo(PublicKeyAlgo, RSAKeySize, ClientSignAlgs, ServerSignAlgs, Curve)),

        %% Select client public key. If no public key found in ClientShares or
        %% ClientShares is empty, trigger HelloRetryRequest as we were able
        %% to find an acceptable set of parameters but the ClientHello does not
        %% contain sufficient information.
        {Group, ClientPubKey} = get_client_public_key(Groups, ClientShares),

        %% Generate server_share
        KeyShare = ssl_cipher:generate_server_share(Group),

        State2 = case maps:get(max_frag_enum, Extensions, undefined) of
                      MaxFragEnum when is_record(MaxFragEnum, max_frag_enum) ->
                         ConnectionStates1 = ssl_record:set_max_fragment_length(MaxFragEnum, ConnectionStates0),
                         HsEnv1 = (State1#state.handshake_env)#handshake_env{max_frag_enum = MaxFragEnum},
                         State1#state{handshake_env = HsEnv1,
                                      session = Session,
                                      connection_states = ConnectionStates1};
                     _ ->
                         State1#state{session = Session}
                 end,

        State3 = if KeepSecrets =:= true ->
                         tls_handshake_1_3:set_client_random(State2, Hello#client_hello.random);
                    true ->
                         State2
                 end,

        State4 = tls_handshake_1_3:update_start_state(State3,
                                                      #{cipher => Cipher,
                                                        key_share => KeyShare,
                                                        session_id => SessionId,
                                                        group => Group,
                                                        sign_alg => SelectedSignAlg,
                                                        peer_public_key => ClientPubKey,
                                                        alpn => ALPNProtocol}),

        %% 4.1.4.  Hello Retry Request
        %%
        %% The server will send this message in response to a ClientHello
        %% message if it is able to find an acceptable set of parameters but the
        %% ClientHello does not contain sufficient information to proceed with
        %% the handshake.
        case Maybe(send_hello_retry_request(State4, ClientPubKey, KeyShare, SessionId)) of
            {_, start} = NextStateTuple ->
                NextStateTuple;
            {State5, negotiated} ->
                %% Determine if early data is accepted
                State = handle_early_data(State5, EarlyDataEnabled, EarlyDataIndication),
                %% Exclude any incompatible PSKs.
                PSK = Maybe(tls_handshake_1_3:handle_pre_shared_key(State, OfferedPSKs, Cipher)),
                Maybe(session_resumption({State, negotiated}, PSK))
        end
    catch
        {Ref, #alert{} = Alert} ->
            Alert
    end;
%% TLS Client
do_start(#server_hello{cipher_suite = SelectedCipherSuite,
                       session_id = SessionId,
                       extensions = Extensions},
         #state{static_env = #static_env{role = client,
                                         host = Host,
                                         port = Port,
                                         cert_db = CertDbHandle,
                                         cert_db_ref = CertDbRef,
                                         protocol_cb = Connection,
                                         transport_cb = Transport,
                                         socket = Socket},
                handshake_env = #handshake_env{renegotiation = {Renegotiation, _},
                                               ocsp_stapling_state = OcspState},
                connection_env = #connection_env{negotiated_version = NegotiatedVersion},
                ssl_options = #{ciphers := ClientCiphers,
                                supported_groups := ClientGroups0,
                                use_ticket := UseTicket,
                                session_tickets := SessionTickets,
                                log_level := LogLevel} = SslOpts,
                session = Session0,
                connection_states = ConnectionStates0
               } = State0) ->
    {Ref,Maybe} = maybe(),
    try
        ClientGroups = Maybe(get_supported_groups(ClientGroups0)),
        Cookie = maps:get(cookie, Extensions, undefined),

        ServerKeyShare = maps:get(key_share, Extensions, undefined),
        #key_share_hello_retry_request{selected_group = SelectedGroup} = ServerKeyShare,

        %% Upon receipt of this extension in a HelloRetryRequest, the client
        %% MUST verify that (1) the selected_group field corresponds to a group
        %% which was provided in the "supported_groups" extension in the
        %% original ClientHello and (2) the selected_group field does not
        %% correspond to a group which was provided in the "key_share" extension
        %% in the original ClientHello.  If either of these checks fails, then
        %% the client MUST abort the handshake with an "illegal_parameter"
        %% alert.
        Maybe(tls_handshake_1_3:validate_selected_group(SelectedGroup, ClientGroups)),

        Maybe(tls_handshake_1_3:validate_cipher_suite(SelectedCipherSuite, ClientCiphers)),

        %% Otherwise, when sending the new ClientHello, the client MUST
        %% replace the original "key_share" extension with one containing only a
        %% new KeyShareEntry for the group indicated in the selected_group field
        %% of the triggering HelloRetryRequest.
        ClientKeyShare = ssl_cipher:generate_client_shares([SelectedGroup]),
        TicketData = tls_handshake_1_3:get_ticket_data(self(), SessionTickets, UseTicket),
        OcspNonce = maps:get(ocsp_nonce, OcspState, undefined),
        Hello0 = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
                                           SessionId, Renegotiation, ClientKeyShare,
                                           TicketData, OcspNonce, CertDbHandle, CertDbRef),
        %% Echo cookie received in HelloRetryrequest
        Hello1 = tls_handshake_1_3:maybe_add_cookie_extension(Cookie, Hello0),

        %% Update state
        State1 = tls_handshake_1_3:update_start_state(State0,
                                                      #{cipher => SelectedCipherSuite,
                                                        key_share => ClientKeyShare,
                                                        session_id => SessionId,
                                                        group => SelectedGroup}),

        %% Replace ClientHello1 with a special synthetic handshake message
        State2 = tls_handshake_1_3:replace_ch1_with_message_hash(State1),
        #state{handshake_env = #handshake_env{tls_handshake_history = HHistory0}} = State2,

        %% Update pre_shared_key extension with binders (TLS 1.3)
        Hello = tls_handshake_1_3:maybe_add_binders(Hello1, HHistory0, TicketData, NegotiatedVersion),

        {BinMsg0, ConnectionStates, HHistory} =
            Connection:encode_handshake(Hello,  NegotiatedVersion, ConnectionStates0, HHistory0),

        %% D.4.  Middlebox Compatibility Mode
        {#state{handshake_env = HsEnv} = State3, BinMsg} =
            maybe_prepend_change_cipher_spec(State2, BinMsg0),

        tls_socket:send(Transport, Socket, BinMsg),
        ssl_logger:debug(LogLevel, outbound, 'handshake', Hello),
        ssl_logger:debug(LogLevel, outbound, 'record', BinMsg),

        State = State3#state{
                  connection_states = ConnectionStates,
                  session = Session0#session{session_id = Hello#client_hello.session_id},
                  handshake_env = HsEnv#handshake_env{tls_handshake_history = HHistory},
                  key_share = ClientKeyShare},

        {State, wait_sh}

    catch
        {Ref, #alert{} = Alert} ->
            Alert
    end.

do_negotiated({start_handshake, PSK0},
              #state{connection_states = ConnectionStates0,
                     handshake_env =
                         #handshake_env{
                            early_data_accepted = EarlyDataAccepted},
                     static_env = #static_env{protocol_cb = Connection},
                     session = #session{session_id = SessionId,
                                        ecc = SelectedGroup,
                                        dh_public_value = ClientPublicKey},
                     ssl_options = #{} = SslOpts,
                     key_share = KeyShare} = State0) ->
    ServerPrivateKey = get_server_private_key(KeyShare),

    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{prf_algorithm = HKDF} = SecParamsR,

    {Ref,Maybe} = maybe(),
    try
        %% Create server_hello
        ServerHello = tls_handshake_1_3:server_hello(server_hello, SessionId, KeyShare, PSK0, ConnectionStates0),
        State1 = Connection:queue_handshake(ServerHello, State0),
        %% D.4.  Middlebox Compatibility Mode
        State2 = maybe_queue_change_cipher_spec(State1, last),

        PSK = tls_handshake_1_3:get_pre_shared_key(PSK0, HKDF),

        State3 =
            tls_handshake_1_3:calculate_handshake_secrets(ClientPublicKey, ServerPrivateKey, SelectedGroup,
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
        error:badarg ->
            ?ALERT_REC(?ILLEGAL_PARAMETER, illegal_parameter_to_compute_key)
    end.


do_wait_cert(#certificate_1_3{} = Certificate, State0) ->
    {Ref,Maybe} = maybe(),
    try
        Maybe(tls_handshake_1_3:process_certificate(Certificate, State0))
    catch
        {Ref, #alert{} = Alert} ->
            {Alert, State0};
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State}
    end.


do_wait_cv(#certificate_verify_1_3{} = CertificateVerify, #state{static_env = #static_env{role = Role}} = State0) ->
    {Ref,Maybe} = maybe(),
    try
        State1 = case Role of
                     server ->
                         Maybe(tls_handshake_1_3:verify_signature_algorithm(State0, CertificateVerify));
                     client ->
                         State0
                 end,
        Maybe(tls_handshake_1_3:verify_certificate_verify(State1, CertificateVerify))
    catch
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State}
    end.

%% TLS Server
do_wait_finished(#finished{verify_data = VerifyData},
                 #state{static_env = #static_env{role = server}} = State0) ->
    {Ref,Maybe} = maybe(),

    try
        Maybe(tls_handshake_1_3:validate_finished(State0, VerifyData)),

        State1 = tls_handshake_1_3:calculate_traffic_secrets(State0),
        State2 = tls_handshake_1_3:maybe_calculate_resumption_master_secret(State1),
        State3 = tls_handshake_1_3:forget_master_secret(State2),

        %% Configure traffic keys
        State4 = ssl_record:step_encryption_state(State3),

        %% Send session ticket
        maybe_send_session_ticket(State4)

    catch
        {Ref, #alert{} = Alert} ->
            Alert
    end;
%% TLS Client
do_wait_finished(#finished{verify_data = VerifyData},
                 #state{static_env = #static_env{role = client,
                                                 protocol_cb = Connection}} = State0) ->
    {Ref,Maybe} = maybe(),

    try
        Maybe(tls_handshake_1_3:validate_finished(State0, VerifyData)),
        %% D.4.  Middlebox Compatibility Mode
        State1 = maybe_queue_change_cipher_spec(State0, first),
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
        State7 = tls_handshake_1_3:maybe_calculate_resumption_master_secret(State6),
        State8 = tls_handshake_1_3:forget_master_secret(State7),
        %% Configure traffic keys
        ssl_record:step_encryption_state(State8)
    catch
        {Ref, #alert{} = Alert} ->
            Alert
    end.


do_wait_sh(#server_hello{cipher_suite = SelectedCipherSuite,
                         session_id = SessionId,
                         extensions = Extensions} = ServerHello,
           #state{key_share = ClientKeyShare0,
                  ssl_options = #{ciphers := ClientCiphers,
                                  supported_groups := ClientGroups0,
                                  session_tickets := SessionTickets,
                                  use_ticket := UseTicket}} = State0) ->

    {Ref,Maybe} = maybe(),
    try
        ClientGroups = Maybe(get_supported_groups(ClientGroups0)),
        ServerKeyShare0 = maps:get(key_share, Extensions, undefined),
        ServerPreSharedKey = maps:get(pre_shared_key, Extensions, undefined),
        SelectedIdentity = get_selected_identity(ServerPreSharedKey),
        ClientKeyShare = get_key_shares(ClientKeyShare0),

        %% Go to state 'start' if server replies with 'HelloRetryRequest'.
        Maybe(tls_handshake_1_3:maybe_hello_retry_request(ServerHello, State0)),

        %% Resumption and PSK
        State1 = handle_resumption(State0, SelectedIdentity),
        ServerKeyShare = get_key_shares(ServerKeyShare0),

        Maybe(tls_handshake_1_3:validate_cipher_suite(SelectedCipherSuite, ClientCiphers)),
        Maybe(tls_handshake_1_3:validate_server_key_share(ClientGroups, ServerKeyShare)),

        %% Get server public key
        {SelectedGroup, ServerPublicKey} = get_server_public_key(ServerKeyShare),

        {_, ClientPrivateKey} = get_client_private_key([SelectedGroup], ClientKeyShare),

        %% Update state
        State2 = tls_handshake_1_3:update_start_state(State1,
                                    #{cipher => SelectedCipherSuite,
                                     key_share => ClientKeyShare0,
                                     session_id => SessionId,
                                     group => SelectedGroup,
                                     peer_public_key => ServerPublicKey}),

        #state{connection_states = ConnectionStates} = State2,
        #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
        #security_parameters{prf_algorithm = HKDFAlgo} = SecParamsR,

        PSK = Maybe(tls_handshake_1_3:get_pre_shared_key(SessionTickets, UseTicket, HKDFAlgo, SelectedIdentity)),
        State3 = tls_handshake_1_3:calculate_handshake_secrets(ServerPublicKey, ClientPrivateKey, SelectedGroup,
                                             PSK, State2),
        %% State4 = ssl_record:step_encryption_state(State3),
        State4 = ssl_record:step_encryption_state_read(State3),
        {State4, wait_ee}

    catch
        {Ref, {State, StateName, ServerHello}} ->
            {State, StateName, ServerHello};
        {Ref, #alert{} = Alert} ->
            Alert
    end.


do_wait_ee(#encrypted_extensions{extensions = Extensions}, State0) ->

    ALPNProtocol0 = maps:get(alpn, Extensions, undefined),
    ALPNProtocol = decode_alpn(ALPNProtocol0),
    EarlyDataIndication = maps:get(early_data, Extensions, undefined),

    {Ref, Maybe} = maybe(),

    try
        %% RFC 6066: handle received/expected maximum fragment length
        Maybe(maybe_max_fragment_length(Extensions, State0)),

        %% Check if early_data is accepted/rejected
        State1 = maybe_check_early_data_indication(EarlyDataIndication, State0),

        %% Go to state 'wait_finished' if using PSK.
        Maybe(maybe_resumption(State1)),

        %% Update state
        #state{handshake_env = HsEnv} = State1,
        State2 = State1#state{handshake_env = HsEnv#handshake_env{alpn = ALPNProtocol}},

        {State2, wait_cert_cr}
    catch
        {Ref, {State, StateName}} ->
            {State, StateName};
        {Ref, #alert{} = Alert} ->
            Alert
    end.


do_wait_cert_cr(#certificate_1_3{} = Certificate, State0) ->
    {Ref,Maybe} = maybe(),
    try
        Maybe(tls_handshake_1_3:process_certificate(Certificate, State0))
    catch
        {Ref, #alert{} = Alert} ->
            {Alert, State0};
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State}
    end;
do_wait_cert_cr(#certificate_request_1_3{} = CertificateRequest, State0) ->
    {Ref,Maybe} = maybe(),
    try
        Maybe(tls_handshake_1_3:process_certificate_request(CertificateRequest, State0))
    catch
        {Ref, #alert{} = Alert} ->
            {Alert, State0}
    end.


do_wait_eoed(#end_of_early_data{}, State0) ->
    {Ref,_Maybe} = maybe(),
    try
        %% Step read state to enable reading handshake messages from the client.
        %% Write state is already stepped in state 'negotiated'.
        State1 = ssl_record:step_encryption_state_read(State0),

        %% Early data has been received, no more early data is expected.
        HsEnv = (State1#state.handshake_env)#handshake_env{early_data_accepted = false},
        State2 = State1#state{handshake_env = HsEnv},
        {State2, wait_finished}
    catch
        {Ref, #alert{} = Alert} ->
            {Alert, State0};
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State}
    end.


handle_new_session_ticket(_, #state{ssl_options = #{session_tickets := disabled}}) ->
    ok;
handle_new_session_ticket(#new_session_ticket{ticket_nonce = Nonce} = NewSessionTicket,
                          #state{connection_states = ConnectionStates,
                                 ssl_options = #{session_tickets := SessionTickets,
                                                 server_name_indication := SNI},
                                 connection_env = #connection_env{user_application = {_, User}}})
  when SessionTickets =:= manual ->
    #{security_parameters := SecParams} =
	ssl_record:current_connection_state(ConnectionStates, read),
    CipherSuite = SecParams#security_parameters.cipher_suite,
    #{cipher := Cipher} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    HKDF = SecParams#security_parameters.prf_algorithm,
    RMS = SecParams#security_parameters.resumption_master_secret,
    PSK = tls_v1:pre_shared_key(RMS, Nonce, HKDF),
    send_ticket_data(User, NewSessionTicket, {Cipher, HKDF}, SNI, PSK);
handle_new_session_ticket(#new_session_ticket{ticket_nonce = Nonce} = NewSessionTicket,
                          #state{connection_states = ConnectionStates,
                                 ssl_options = #{session_tickets := SessionTickets,
                                                 server_name_indication := SNI}})
  when SessionTickets =:= auto ->
    #{security_parameters := SecParams} =
	ssl_record:current_connection_state(ConnectionStates, read),
    CipherSuite = SecParams#security_parameters.cipher_suite,
    #{cipher := Cipher} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    HKDF = SecParams#security_parameters.prf_algorithm,
    RMS = SecParams#security_parameters.resumption_master_secret,
    PSK = tls_v1:pre_shared_key(RMS, Nonce, HKDF),
    tls_client_ticket_store:store_ticket(NewSessionTicket, {Cipher, HKDF}, SNI, PSK).

send_ticket_data(User, NewSessionTicket, CipherSuite, SNI, PSK) ->
    Timestamp = erlang:system_time(millisecond),
    TicketData = #{cipher_suite => CipherSuite,
                   sni => SNI,
                   psk => PSK,
                   timestamp => Timestamp,
                   ticket => NewSessionTicket},
    User ! {ssl, session_ticket, TicketData}.

handle_key_update(#key_update{request_update = update_not_requested}, State0) ->
    %% Update read key in connection
    {ok, update_cipher_key(current_read, State0)};
handle_key_update(#key_update{request_update = update_requested},
                  #state{protocol_specific = #{sender := Sender}} = State0) ->
    %% Update read key in connection
    State1 = update_cipher_key(current_read, State0),
    %% Send key_update and update sender's write key
    case send_key_update(Sender, update_not_requested) of
        ok ->
            {ok, State1};
        {error, Reason} ->
            {error, State1, ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason)}
    end.

init_max_early_data_size(client) ->
    %% Disable trial decryption on the client side
    %% Servers do trial decryption of max_early_data bytes of plain text.
    %% Setting it to 0 means that a decryption error will result in an Alert.
    0;
init_max_early_data_size(server) ->
    ssl_config:get_max_early_data_size().

handle_middlebox(#state{session = #session{session_id = Id},
                        protocol_specific = PS} = State0)  when Id =/= ?EMPTY_ID ->
    State0#state{protocol_specific = PS#{change_cipher_spec => ignore}};
handle_middlebox(State) ->
    State.

handle_change_cipher_spec(Type, Msg, StateName, #state{protocol_specific = PS0} = State) ->
    case maps:get(change_cipher_spec, PS0) of
        ignore ->
            PS = PS0#{change_cipher_spec => fail},
            tls_gen_connection:next_event(StateName, no_record,
                                          State#state{protocol_specific = PS});
        fail ->
            ssl_gen_statem:handle_common_event(Type, Msg, StateName, State)
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

maybe_resumption(#state{handshake_env = #handshake_env{resumption = true}} = State) ->
    {error, {State, wait_finished}};
maybe_resumption(_) ->
    ok.

handle_resumption(State, undefined) ->
    State;
handle_resumption(#state{handshake_env = HSEnv0} = State, _) ->
    HSEnv = HSEnv0#handshake_env{resumption = true},
    State#state{handshake_env = HSEnv}.

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
    Ticket = tls_server_session_ticket:new(Tracker, HKDF, RMS),
    {State, _} = Connection:send_handshake(Ticket, State0),
    maybe_send_session_ticket(State, N - 1).

%% @doc Enqueues a change_cipher_spec record as the first/last message of
%%      the current flight buffer
%% @end
maybe_queue_change_cipher_spec(#state{flight_buffer = FlightBuffer0} = State0, first) ->
    {State, FlightBuffer} = maybe_prepend_change_cipher_spec(State0, FlightBuffer0),
    State#state{flight_buffer = FlightBuffer};
maybe_queue_change_cipher_spec(#state{flight_buffer = FlightBuffer0} = State0, last) ->
    {State, FlightBuffer} = maybe_append_change_cipher_spec(State0, FlightBuffer0),
    State#state{flight_buffer = FlightBuffer}.

%% @doc Prepends a change_cipher_spec record to the input binary
%%
%%      It can only prepend the change_cipher_spec record only once in
%%      order to accurately emulate a legacy TLS 1.2 connection.
%%
%%      D.4.  Middlebox Compatibility Mode
%%      If not offering early data, the client sends a dummy
%%      change_cipher_spec record (see the third paragraph of Section 5)
%%      immediately before its second flight.  This may either be before
%%      its second ClientHello or before its encrypted handshake flight.
%%      If offering early data, the record is placed immediately after the
%%      first ClientHello.
%% @end
maybe_prepend_change_cipher_spec(#state{
                                    session = #session{session_id = Id},
                                    handshake_env =
                                        #handshake_env{
                                           change_cipher_spec_sent = false} = HSEnv} = State, Bin) when Id =/= ?EMPTY_ID ->
    CCSBin = tls_handshake_1_3:create_change_cipher_spec(State),
    {State#state{handshake_env =
                     HSEnv#handshake_env{change_cipher_spec_sent = true}},
     [CCSBin|Bin]};
maybe_prepend_change_cipher_spec(State, Bin) ->
    {State, Bin}.

%% @doc Appends a change_cipher_spec record to the input binary
%% @end
maybe_append_change_cipher_spec(#state{
                                   session = #session{session_id = Id},
                                    handshake_env =
                                        #handshake_env{
                                           change_cipher_spec_sent = false} = HSEnv} = State, Bin) when Id =/= ?EMPTY_ID  ->
    CCSBin = tls_handshake_1_3:create_change_cipher_spec(State),
    {State#state{handshake_env =
                     HSEnv#handshake_env{change_cipher_spec_sent = true}},
     Bin ++ [CCSBin]};
maybe_append_change_cipher_spec(State, Bin) ->
    {State, Bin}.

maybe_queue_cert_cert_cv(#state{client_certificate_status = not_requested} = State) ->
    {ok, State};
maybe_queue_cert_cert_cv(#state{connection_states = _ConnectionStates0,
                                session = #session{session_id = _SessionId,
                                                   own_certificates = OwnCerts},
                                ssl_options = #{} = _SslOpts,
                                key_share = _KeyShare,
                                handshake_env = #handshake_env{tls_handshake_history = _HHistory0},
                                static_env = #static_env{
                                                role = client,
                                                protocol_cb = Connection,
                                                cert_db = CertDbHandle,
                                                cert_db_ref = CertDbRef,
                                                socket = _Socket,
                                                transport_cb = _Transport}
                               } = State0) ->
    {Ref,Maybe} = maybe(),
    try
        %% Create Certificate
        Certificate = Maybe(tls_handshake_1_3:certificate(OwnCerts, CertDbHandle, CertDbRef, <<>>, client)),

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
                               static_env = #static_env{role = client,
                                                        protocol_cb = Connection}
                              } = State) ->
    {Ref,Maybe} = maybe(),
    try
        CertificateVerify = Maybe(tls_handshake_1_3:certificate_verify(CertPrivateKey, SignatureScheme, State, client)),
        {ok, Connection:queue_handshake(CertificateVerify, State)}
    catch
        {Ref, #alert{} = Alert} ->
            {error, Alert}
    end.

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
                                 signature_algs_cert := SignAlgsCert,
                                 certificate_authorities := CertAuthBool}, _) ->
    CertificateRequest = tls_handshake_1_3:certificate_request(SignAlgs, SignAlgsCert, CertDbHandle,
                                                               CertDbRef, CertAuthBool),
    {Connection:queue_handshake(CertificateRequest, State), wait_cert}.

maybe_max_fragment_length(Extensions, State) ->
    ServerMaxFragEnum = maps:get(max_frag_enum, Extensions, undefined),
    ClientMaxFragEnum = ssl_handshake:max_frag_enum(
                          maps:get(max_fragment_length, State#state.ssl_options, undefined)),
    if ServerMaxFragEnum == ClientMaxFragEnum ->
            ok;
       true ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)}
    end.

maybe_send_early_data(#state{
                         handshake_env = #handshake_env{tls_handshake_history = {Hist, _}},
                         protocol_specific = #{sender := _Sender},
                         ssl_options = #{versions := [Version|_],
                                         use_ticket := UseTicket,
                                         session_tickets := SessionTickets,
                                         early_data := EarlyData} = _SslOpts0
                        } = State0) when Version =:= {3,4} andalso
                                         UseTicket =/= [undefined] andalso
                                         EarlyData =/= undefined ->
    %% D.4.  Middlebox Compatibility Mode
    State1 = maybe_queue_change_cipher_spec(State0, last),
    %% Early traffic secret
    EarlyDataSize = tls_handshake_1_3:early_data_size(EarlyData),
    case tls_handshake_1_3:get_pre_shared_key_early_data(SessionTickets, UseTicket) of
        {ok, {PSK, Cipher, HKDF, MaxSize}} when EarlyDataSize =< MaxSize ->
            State2 = tls_handshake_1_3:calculate_client_early_traffic_secret(Hist, PSK, Cipher, HKDF, State1),
            %% Set 0-RTT traffic keys for sending early_data and EndOfEarlyData
            State3 = ssl_record:step_encryption_state_write(State2),
            {ok, tls_handshake_1_3:encode_early_data(Cipher, State3)};
        {ok, {_, _, _, MaxSize}} ->
            {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, {too_much_early_data, {max, MaxSize}})};
        {error, Alert} ->
            {error, Alert}
    end;
maybe_send_early_data(State) ->
    {ok, State}.

maybe_send_end_of_early_data(
  #state{
     handshake_env = #handshake_env{early_data_accepted = true},
     protocol_specific = #{sender := _Sender},
     ssl_options = #{versions := [Version|_],
                     use_ticket := UseTicket,
                     early_data := EarlyData},
     static_env = #static_env{protocol_cb = Connection}
    } = State0) when Version =:= {3,4} andalso
                     UseTicket =/= [undefined] andalso
                     EarlyData =/= undefined ->
    %% EndOfEarlydata is encrypted with the 0-RTT traffic keys
    State1 = Connection:queue_handshake(#end_of_early_data{}, State0),
    %% Use handshake keys after EndOfEarlyData is sent
    ssl_record:step_encryption_state_write(State1);
maybe_send_end_of_early_data(State) ->
    State.
%% Configure a suitable session ticket
maybe_automatic_session_resumption(#state{
                                      ssl_options = #{versions := [Version|_],
                                                      ciphers := UserSuites,
                                                      early_data := EarlyData,
                                                      session_tickets := SessionTickets,
                                                      server_name_indication := SNI} = SslOpts0
                                     } = State0)
  when Version >= {3,4} andalso
       SessionTickets =:= auto ->
    AvailableCipherSuites = ssl_handshake:available_suites(UserSuites, Version),
    HashAlgos = cipher_hash_algos(AvailableCipherSuites),
    Ciphers = tls_handshake_1_3:ciphers_for_early_data(AvailableCipherSuites),
    %% Find a pair of tickets KeyPair = {Ticket0, Ticket2} where Ticket0 satisfies
    %% requirements for early_data and session resumption while Ticket2 can only
    %% be used for session resumption.
    EarlyDataSize = tls_handshake_1_3:early_data_size(EarlyData),
    KeyPair = tls_client_ticket_store:find_ticket(self(), Ciphers, HashAlgos, SNI, EarlyDataSize),
    UseTicket = tls_handshake_1_3:choose_ticket(KeyPair, EarlyData),
    tls_client_ticket_store:lock_tickets(self(), [UseTicket]),
    State = State0#state{ssl_options = SslOpts0#{use_ticket => [UseTicket]}},
    {[UseTicket], State};
maybe_automatic_session_resumption(#state{
                                      ssl_options = #{use_ticket := UseTicket}
                                     } = State) ->
    {UseTicket, State}.


session_resumption({#state{ssl_options = #{session_tickets := disabled}} = State, negotiated}, _) ->
    {ok, {State, negotiated}};
session_resumption({#state{ssl_options = #{session_tickets := Tickets}} = State, negotiated}, undefined)
  when Tickets =/= disabled ->
    {ok, {State, negotiated}};
session_resumption({#state{ssl_options = #{session_tickets := Tickets},
                           handshake_env = #handshake_env{
                                              early_data_accepted = false}} = State0, negotiated}, PSK)
  when Tickets =/= disabled ->
    State = handle_resumption(State0, ok),
    {ok, {State, negotiated, PSK}};
session_resumption({#state{ssl_options = #{session_tickets := Tickets},
                           handshake_env = #handshake_env{
                                              early_data_accepted = true}} = State0, negotiated}, PSK0)
  when Tickets =/= disabled ->
    State1 = handle_resumption(State0, ok),
    %% TODO Refactor PSK-tuple {Index, PSK}, index might not be needed.
    {_ , PSK} = PSK0,
    State2 = tls_handshake_1_3:calculate_client_early_traffic_secret(State1, PSK),
    %% Set 0-RTT traffic keys for reading early_data
    State3 = ssl_record:step_encryption_state_read(State2),
    State = update_current_read(State3, true, true),
    {ok, {State, negotiated, PSK0}}.

send_hello_retry_request(#state{connection_states = ConnectionStates0,
                                static_env = #static_env{protocol_cb = Connection}} = State0,
                         no_suitable_key, KeyShare, SessionId) ->
    ServerHello0 = tls_handshake_1_3:server_hello(hello_retry_request, SessionId, KeyShare, undefined, ConnectionStates0),
    {State1, ServerHello} = tls_handshake_1_3:maybe_add_cookie_extension(State0, ServerHello0),

    State2 = Connection:queue_handshake(ServerHello, State1),
    %% D.4.  Middlebox Compatibility Mode
    State3 = maybe_queue_change_cipher_spec(State2, last),
    {State4, _} = Connection:send_handshake_flight(State3),

    %% Update handshake history
    State5 = tls_handshake_1_3:replace_ch1_with_message_hash(State4),

    {ok, {State5, start}};
send_hello_retry_request(State0, _, _, _) ->
    %% Suitable key found.
    {ok, {State0, negotiated}}.

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
                                                            cert_db_ref = CertDbRef} = State},
                            Default0) ->
    {_, SignAlgo, SignHash, _, _} = tls_handshake_1_3:get_certificate_params(Cert),
    %% TODO: We do validate the signature algorithm and signature hash but we could also check
    %% if the signing cert has a key on a curve supported by the client for ECDSA/EDDSA certs
    case tls_handshake_1_3:check_cert_sign_algo(SignAlgo, SignHash, ClientSignAlgs, ClientSignAlgsCert) of
        ok ->
            case ssl_certificate:handle_cert_auths(Certs, CertAuths, CertDbHandle, CertDbRef) of
                {ok, EncodeChain} -> %% Chain fullfills certificate_authorities extension
                    {ok, Session#session{own_certificates = EncodeChain, private_key = Key}};
                {error, EncodeChain, not_in_auth_domain} ->
                    %% If this is the first chain to fulfill the signing requirement, use it as default,
                    %% if not later alternative also fulfills certificate_authorities extension
                    Default = Session#session{own_certificates = EncodeChain, private_key = Key},
                    select_server_cert_key_pair(Session, Rest, ClientSignAlgs, ClientSignAlgsCert,
                                                CertAuths, State, default_or_fallback(Default0, Default))
            end;
        _ ->
            %% If the server cannot produce a certificate chain that is signed only
            %% via the indicated supported algorithms, then it SHOULD continue the
            %% handshake by sending the client a certificate chain of its choice
            case SignHash of
                sha1 ->
                    %%  According to "Server Certificate Selection - RFC 8446"
                    %%  Never send cert using sha1 unless client allows it
                    select_server_cert_key_pair(Session, Rest, ClientSignAlgs, ClientSignAlgsCert,
                                                CertAuths, State, Default0);
                _ ->
                    %% If there does not exist a default or fallback from previous alternatives
                    %% use this alternative as fallback.
                    Fallback = {fallback, Session#session{own_certificates = Certs, private_key = Key}},
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

get_server_private_key(#key_share_server_hello{server_share = ServerShare}) ->
    get_private_key(ServerShare).

get_private_key(#key_share_entry{
                   key_exchange = #'ECPrivateKey'{} = PrivateKey}) ->
    PrivateKey;
get_private_key(#key_share_entry{
                      key_exchange =
                          {_, PrivateKey}}) ->
    PrivateKey.
maybe_check_early_data_indication(EarlyDataIndication,
                                  #state{
                                     handshake_env = HsEnv,
                                     ssl_options = #{versions := [Version|_],
                                                     use_ticket := UseTicket,
                                                     early_data := EarlyData}
                                    } = State) when Version =:= {3,4} andalso
                                                    UseTicket =/= [undefined] andalso
                                                    EarlyData =/= undefined andalso
                                                    EarlyDataIndication =/= undefined ->
    signal_user_early_data(State, accepted),
    State#state{handshake_env = HsEnv#handshake_env{early_data_accepted = true}};
maybe_check_early_data_indication(EarlyDataIndication,
                                  #state{
                                     protocol_specific = #{sender := _Sender},
                                     ssl_options = #{versions := [Version|_],
                                                     use_ticket := UseTicket,
                                                     early_data := EarlyData} = _SslOpts0
                                    } = State) when Version =:= {3,4} andalso
                                                    UseTicket =/= [undefined] andalso
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

handle_early_data(State, enabled, #early_data_indication{}) ->
    %% Accept early data
    HsEnv = (State#state.handshake_env)#handshake_env{early_data_accepted = true},
    State#state{handshake_env = HsEnv};
handle_early_data(State, _, _) ->
    State.

cipher_hash_algos(Ciphers) ->
    Fun = fun(Cipher) ->
                  #{prf := Hash} = ssl_cipher_format:suite_bin_to_map(Cipher),
                  Hash
          end,
    lists:map(Fun, Ciphers).

get_supported_groups(undefined = Groups) ->
    {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, {supported_groups, Groups})};
get_supported_groups(#supported_groups{supported_groups = Groups}) ->
    {ok, Groups}.

get_key_shares(undefined) ->
    [];
get_key_shares(#key_share_client_hello{client_shares = ClientShares}) ->
    ClientShares;
get_key_shares(#key_share_server_hello{server_share = ServerShare}) ->
    ServerShare.

%% get_cookie(undefined) ->
%%     undefined;
%% get_cookie(#cookie{cookie = Cookie}) ->
%%     Cookie.

get_selected_identity(undefined) ->
    undefined;
get_selected_identity(#pre_shared_key_server_hello{selected_identity = SelectedIdentity}) ->
    SelectedIdentity.

update_current_read(#state{connection_states = CS} = State, TrialDecryption, EarlyDataExpected) ->
    Read0 = ssl_record:current_connection_state(CS, read),
    Read = Read0#{trial_decryption => TrialDecryption,
                  early_data_accepted => EarlyDataExpected},
    State#state{connection_states = CS#{current_read => Read}}.


get_client_public_key([Group|_] = Groups, ClientShares) ->
    get_client_public_key(Groups, ClientShares, Group).
%%
get_client_public_key(_, [], PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_public_key([], _, PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_public_key([Group|Groups], ClientShares, PreferredGroup) ->
     case lists:keysearch(Group, 2, ClientShares) of
         {value, {_, _, ClientPublicKey}} ->
             {Group, ClientPublicKey};
         false ->
             get_client_public_key(Groups, ClientShares, PreferredGroup)
     end.

get_client_private_key([Group|_] = Groups, ClientShares) ->
    get_client_private_key(Groups, ClientShares, Group).
%%
get_client_private_key(_, [], PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_private_key([], _, PreferredGroup) ->
    {PreferredGroup, no_suitable_key};
get_client_private_key([Group|Groups], ClientShares, PreferredGroup) ->
     case lists:keysearch(Group, 2, ClientShares) of
         {value, {_, _, {_, ClientPrivateKey}}} ->
             {Group, ClientPrivateKey};
         {value, {_, _, #'ECPrivateKey'{} = ClientPrivateKey}} ->
             {Group, ClientPrivateKey};
         false ->
             get_client_private_key(Groups, ClientShares, PreferredGroup)
     end.


get_server_public_key({key_share_entry, Group, PublicKey}) ->
                             {Group, PublicKey}.

get_offered_psks(Extensions) ->
    PSK = maps:get(pre_shared_key, Extensions, undefined),
    case PSK of
        undefined ->
            undefined;
        #pre_shared_key_client_hello{offered_psks = OfferedPSKs} ->
            OfferedPSKs
    end.

decode_alpn(ALPNProtocol0) ->
    case ssl_handshake:decode_alpn(ALPNProtocol0) of
        undefined ->
            undefined;
        [ALPNProtocol] ->
            ALPNProtocol
    end.

maybe() ->
    Ref = erlang:make_ref(),
    Ok = fun(ok) -> ok;
            ({ok,R}) -> R;
            ({error,Reason}) ->
                 throw({Ref,Reason})
         end,
    {Ref,Ok}.


%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(kdt,
             {call, {?MODULE, handle_key_update,
                     [#key_update{request_update = update_requested}, _State]}},
             Stack) ->
    {io_lib:format("KeyUpdate procedure 2/4 - ~w received", [update_requested]), Stack};
handle_trace(kdt,
             {call, {?MODULE, handle_key_update,
                     [#key_update{request_update = update_not_requested}, _State]}},
             Stack) ->
    {io_lib:format("KeyUpdate procedure 4/4 - ~w received", [update_not_requested]), Stack}.
