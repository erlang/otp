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

-module(tls_gen_connection_1_3).

-include("ssl_alert.hrl").
-include("ssl_connection.hrl").
-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("tls_handshake_1_3.hrl").

%% Internal API


% gen_statem state help functions
-export([initial_state/8,
         user_hello/3,
         wait_cert/3,
         wait_cv/3,
         connection/3,
         downgrade/3
        ]).

-export([maybe_queue_change_cipher_spec/2,
         maybe_prepend_change_cipher_spec/2,
         maybe_append_change_cipher_spec/2,
         handle_change_cipher_spec/4,
         handle_middlebox/1,
         handle_resumption/2,
         send_key_update/2,
         update_cipher_key/2,
         do_maybe/0]).

%%--------------------------------------------------------------------
%% Internal API
%%--------------------------------------------------------------------
initial_state(Role, Sender, Host, Port, Socket,
              {SSLOptions, SocketOptions, Trackers}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
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
                          tls_handshake_history =
                              ssl_handshake:init_handshake_history(),
                          renegotiation = {false, first}
                         },
       connection_env = #connection_env{user_application = {UserMonitor, User}},
       socket_options = SocketOptions,
       ssl_options = SSLOptions,
       session = #session{is_resumable = false,
                          session_id =
                              ssl_session:legacy_session_id(SSLOptions)},
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
user_hello(info, {'DOWN', _, _, _, _} = Event, State) ->
    ssl_gen_statem:handle_info(Event, ?FUNCTION_NAME, State);
user_hello(_, _, _) ->
    {keep_state_and_data, [postpone]}.

wait_cert(enter, _, State0) ->
    State = handle_middlebox(State0),
    {next_state, ?FUNCTION_NAME, State,[]};
wait_cert(internal = Type, #change_cipher_spec{} = Msg,
          #state{session = #session{session_id = Id}} = State)
  when Id =/= ?EMPTY_ID ->
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
        #state{session = #session{session_id = Id}} = State)
  when Id =/= ?EMPTY_ID ->
    handle_change_cipher_spec(Type, Msg, ?FUNCTION_NAME, State);
wait_cv(info, Msg, State) ->
    tls_gen_connection:handle_info(Msg, ?FUNCTION_NAME, State);
wait_cv(Type, Msg, State) ->
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
	   #state{handshake_env =
                      #handshake_env{alpn = SelectedProtocol,
                                     negotiated_protocol = undefined}} =
               State) ->
    ssl_gen_statem:hibernate_after(?FUNCTION_NAME, State,
                    [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, {export_key_materials, Labels, Contexts, WantedLengths, Last},
           #state{connection_states = ConnectionStates,
                  protocol_specific = PS} = State0) ->
    #{security_parameters := #security_parameters{prf_algorithm = PRFAlgorithm}} =
	ssl_record:current_connection_state(ConnectionStates, read),
    case maps:get(exporter_master_secret, PS, undefined) of
        undefined ->
            {next_state, ?FUNCTION_NAME, State0, [{reply, From, {error, exporter_master_secret_already_consumed}}]};
        ExporterMasterSecret ->
            ExpSecrets = exporter_secrets(ExporterMasterSecret, Labels, PRFAlgorithm),
            State = case Last of
                        true  ->
                            State0#state{protocol_specific = maps:without([exporter_master_secret], PS)};
                        false ->
                            State0
                    end,
            ExportKeyMaterials = export_key_materials(ExpSecrets, Contexts, WantedLengths, PRFAlgorithm),
            {next_state, ?FUNCTION_NAME, State, [{reply, From, ExportKeyMaterials}]}
    end;
connection(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

downgrade(enter, _, State) ->
    {keep_state, State};
downgrade(internal, #new_session_ticket{} = NewSessionTicket, State) ->
    _ = handle_new_session_ticket(NewSessionTicket, State),
    {next_state, ?FUNCTION_NAME, State};
downgrade(Type, Event, State) ->
     ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%% Description: Enqueues a change_cipher_spec record as the first/last
%% message of the current flight buffer
maybe_queue_change_cipher_spec(#state{flight_buffer = FlightBuffer0} = State0,
                               first) ->
    {State, FlightBuffer} =
        maybe_prepend_change_cipher_spec(State0, FlightBuffer0),
    State#state{flight_buffer = FlightBuffer};
maybe_queue_change_cipher_spec(#state{flight_buffer = FlightBuffer0} = State0,
                               last) ->
    {State, FlightBuffer} = maybe_append_change_cipher_spec(State0,
                                                            FlightBuffer0),
    State#state{flight_buffer = FlightBuffer}.

handle_change_cipher_spec(Type, Msg, StateName,
                          #state{protocol_specific = PS0} = State) ->
    case maps:get(change_cipher_spec, PS0) of
        ignore ->
            PS = PS0#{change_cipher_spec => fail},
            tls_gen_connection:next_event(StateName, no_record,
                                          State#state{protocol_specific = PS});
        fail ->
            ssl_gen_statem:handle_common_event(Type, Msg, StateName, State)
    end.

handle_middlebox(#state{protocol_specific = PS} = State0) ->
    %% Always be prepared to ignore one change cipher spec
    %% for maximum interopablility, even if middlebox mode
    %% is not enabled.
    State0#state{protocol_specific = PS#{change_cipher_spec => ignore}}.

handle_resumption(State, undefined) ->
    State;
handle_resumption(#state{handshake_env = HSEnv0} = State, _) ->
    HSEnv = HSEnv0#handshake_env{resumption = true},
    State#state{handshake_env = HSEnv}.

do_maybe() ->
    Ref = erlang:make_ref(),
    Ok = fun(ok) -> ok;
            ({ok,R}) -> R;
            ({error,Reason}) ->
                 throw({Ref,Reason})
         end,
    {Ref,Ok}.

%%      Take care of including a change_cipher_spec message in the
%%      correct place if middlebox mod is used.  From RFC: 8446 "D.4.
%%      Middlebox Compatibility Mode If not offering early data, the
%%      client sends a dummy change_cipher_spec record (see the third
%%      paragraph of Section 5) immediately before its second flight.
%%      This may either be before its second ClientHello or before its
%%      encrypted handshake flight.  If offering early data, the
%%      record is placed immediately after the first ClientHello."
maybe_prepend_change_cipher_spec(#state{
                                    session = #session{session_id = Id},
                                    handshake_env =
                                        #handshake_env{
                                           change_cipher_spec_sent = false}
                                    = HSEnv}
                                 = State, Bin) when Id =/= ?EMPTY_ID ->
    CCSBin = tls_handshake_1_3:create_change_cipher_spec(State),
    {State#state{handshake_env =
                     HSEnv#handshake_env{change_cipher_spec_sent = true}},
     [CCSBin|Bin]};
maybe_prepend_change_cipher_spec(State, Bin) ->
    {State, Bin}.

maybe_append_change_cipher_spec(#state{
                                   session = #session{session_id = Id},
                                   handshake_env =
                                       #handshake_env{
                                          change_cipher_spec_sent = false}
                                   = HSEnv}
                                = State, Bin) when Id =/= ?EMPTY_ID  ->
    CCSBin = tls_handshake_1_3:create_change_cipher_spec(State),
    {State#state{handshake_env =
                     HSEnv#handshake_env{change_cipher_spec_sent = true}},
     Bin ++ [CCSBin]};
maybe_append_change_cipher_spec(State, Bin) ->
    {State, Bin}.

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
    ApplicationTrafficSecret0 =
        SecParams0#security_parameters.application_traffic_secret,
    ApplicationTrafficSecret =
        tls_v1:update_traffic_secret(HKDF,
                                     ApplicationTrafficSecret0),

    %% Calculate traffic keys
    KeyLength = tls_v1:key_length(CipherSuite),
    {Key, IV} = tls_v1:calculate_traffic_keys(HKDF, KeyLength,
                                              ApplicationTrafficSecret),

    SecParams = SecParams0#security_parameters{application_traffic_secret = ApplicationTrafficSecret},
    CipherState = CipherState0#cipher_state{key = Key, iv = IV},
    ConnState = ConnState0#{security_parameters => SecParams,
                            cipher_state => CipherState,
                            sequence_number => 0},
    CS0#{ConnStateName => ConnState}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

do_wait_cert(#certificate_1_3{} = Certificate, State0) ->
    {Ref,Maybe} = do_maybe(),
    try
        Maybe(tls_handshake_1_3:process_certificate(Certificate, State0))
    catch
        {Ref, #alert{} = Alert} ->
            {Alert, State0};
        {Ref, {#alert{} = Alert, State}} ->
            {Alert, State}
    end.

handle_new_session_ticket(_, #state{ssl_options = #{session_tickets := disabled}}) ->
    ok;
handle_new_session_ticket(#new_session_ticket{ticket_nonce = Nonce}
                          = NewSessionTicket,
                          #state{connection_states = ConnectionStates,
                                 ssl_options = #{session_tickets := SessionTickets} = SslOpts,
                                 connection_env = #connection_env{user_application = {_, User}}})
  when SessionTickets =:= manual ->
    #{security_parameters := SecParams} =
	ssl_record:current_connection_state(ConnectionStates, read),
    CipherSuite = SecParams#security_parameters.cipher_suite,
    #{cipher := Cipher} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    HKDF = SecParams#security_parameters.prf_algorithm,
    RMS = SecParams#security_parameters.resumption_master_secret,
    PSK = tls_v1:pre_shared_key(RMS, Nonce, HKDF),
    SNI = maps:get(server_name_indication, SslOpts, undefined),
    send_ticket_data(User, NewSessionTicket, {Cipher, HKDF}, SNI, PSK);
handle_new_session_ticket(#new_session_ticket{ticket_nonce = Nonce}
                          = NewSessionTicket,
                          #state{connection_states = ConnectionStates,
                                 ssl_options = #{session_tickets := SessionTickets} = SslOpts})
  when SessionTickets =:= auto ->
    #{security_parameters := SecParams} =
	ssl_record:current_connection_state(ConnectionStates, read),
    CipherSuite = SecParams#security_parameters.cipher_suite,
    #{cipher := Cipher} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    HKDF = SecParams#security_parameters.prf_algorithm,
    RMS = SecParams#security_parameters.resumption_master_secret,
    PSK = tls_v1:pre_shared_key(RMS, Nonce, HKDF),
    SNI = maps:get(server_name_indication, SslOpts, undefined),
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
internal_active_n(_,_) ->
    case application:get_env(ssl, internal_active_n) of
        {ok, N} when is_integer(N) ->
            N;
        _  ->
            ?INTERNAL_ACTIVE_N
    end.

exporter_secrets(ExporterMasterSecret, Labels, PRFAlgorithm) ->
    DeriveSecret =
        fun(Label) ->
                tls_v1:derive_secret(ExporterMasterSecret, Label, <<>>, PRFAlgorithm)
        end,
    [DeriveSecret(Label) || Label <- Labels].

export_key_materials(Secrets, Contexts, Lengths, PRFAlgorithm) ->
    try export_key_materials(Secrets, Contexts, Lengths, PRFAlgorithm, []) of
        ExportKeyMaterials ->
            {ok, ExportKeyMaterials}
    catch _:_ ->
            {error, bad_input}
    end.

export_key_materials([],[],[],_, Acc) ->
    lists:reverse(Acc);
export_key_materials([ExporterSecret | ExS], [Context0 | Contexts],
          [WantedLength| Lengths], PRFAlgorithm, Acc) ->
    ExporterKeyMaterial = exporter(ExporterSecret, Context0, WantedLength, PRFAlgorithm),
    export_key_materials(ExS, Contexts, Lengths, PRFAlgorithm, [ExporterKeyMaterial | Acc]).

exporter(ExporterSecret, Context0, WantedLength, PRFAlgorithm) ->
    Context = case Context0 of
                  no_context ->
                      <<>>;
                  _ ->
                      Context0
              end,
    HashContext = tls_v1:transcript_hash(Context, PRFAlgorithm),
    tls_v1:hkdf_expand_label(ExporterSecret, <<"exporter">>, HashContext,
                             WantedLength, PRFAlgorithm).
