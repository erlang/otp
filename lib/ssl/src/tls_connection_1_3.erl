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

-include("ssl_alert.hrl").
-include("ssl_connection.hrl").
-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("tls_handshake_1_3.hrl").

-behaviour(gen_statem).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4, format_status/2]).

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
         send_key_update/2,
         update_cipher_key/2]).

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
%% state callbacks
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
    Options = ssl:handle_options(NewOptions, Role, Options0),
    State = ssl_gen_statem:ssl_config(Options, Role, State0),
    {next_state, wait_sh, State#state{start_or_recv_from = From,
                                      handshake_env = HSEnv#handshake_env{continue_status = continue}},
     [{{timeout, handshake}, Timeout, close}]};
user_hello({call, From}, {handshake_continue, NewOptions, Timeout},
           #state{static_env = #static_env{role = server = Role},
                  handshake_env = #handshake_env{continue_status = {pause, ClientVersions}} = HSEnv,
                  ssl_options = Options0} = State0) ->
    Options = #{versions := Versions} = ssl:handle_options(NewOptions, Role, Options0),
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
    case tls_handshake_1_3:do_negotiated(Message, State0) of
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
    case tls_handshake_1_3:do_wait_cert(Certificate, State0) of
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
    case tls_handshake_1_3:do_wait_cv(CertificateVerify, State0) of
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
    case tls_handshake_1_3:do_wait_finished(Finished, State0) of
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
    case tls_handshake_1_3:do_wait_sh(Hello, State0) of
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
    case tls_handshake_1_3:do_wait_sh(Hello, State0) of
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
    case tls_handshake_1_3:do_wait_ee(EE, State0) of
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
    case tls_handshake_1_3:do_wait_cert_cr(Certificate, State0) of
        {#alert{} = Alert, State} ->
            ssl_gen_statem:handle_own_alert(Alert, wait_cert_cr, State);
        {State1, NextState} ->
            tls_gen_connection:next_event(NextState, no_record, State1)
    end;
wait_cert_cr(internal, #certificate_request_1_3{} = CertificateRequest, State0) ->
    case tls_handshake_1_3:do_wait_cert_cr(CertificateRequest, State0) of
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
    case tls_handshake_1_3:do_wait_eoed(EOED, State0) of
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

do_server_start(ClientHello, State0) ->
    case tls_handshake_1_3:do_start(ClientHello, State0) of
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
    case tls_handshake_1_3:do_start(ServerHello, State0) of
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, start, State0);
        {State, NextState} ->
            {next_state, NextState, State, []}
    end.

initial_state(Role, Sender, Host, Port, Socket, {SSLOptions, SocketOptions, Trackers}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
    #{erl_dist := IsErlDist,
      %% Use highest supported version for client/server random nonce generation
      versions := [Version|_],
      client_renegotiation := ClientRenegotiation} = SSLOptions,
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
                          renegotiation = {false, first},
                          allow_renegotiate = ClientRenegotiation
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
                             active_n => internal_active_n(IsErlDist),
                             active_n_toggle => true
                            }
      }.

internal_active_n(true) ->
    %% Start with a random number between 1 and ?INTERNAL_ACTIVE_N
    %% In most cases distribution connections are established all at
    %%  the same time, and flow control engages with ?INTERNAL_ACTIVE_N for
    %%  all connections. Which creates a wave of "passive" messages, leading
    %%  to significant bump of memory & scheduler utilisation. Starting with
    %%  a random number between 1 and ?INTERNAL_ACTIVE_N helps to spread the
    %%  spike.
    erlang:system_time() rem ?INTERNAL_ACTIVE_N + 1;
internal_active_n(false) ->
    case application:get_env(ssl, internal_active_n) of
        {ok, N} when is_integer(N) ->
            N;
        _  ->
            ?INTERNAL_ACTIVE_N
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
