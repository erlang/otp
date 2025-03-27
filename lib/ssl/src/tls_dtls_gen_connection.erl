%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
%%
%% NOTE: All alerts are thrown out of this module
%%----------------------------------------------------------------------

-module(tls_dtls_gen_connection).
-moduledoc false.

-include_lib("public_key/include/public_key.hrl").

-include("tls_connection.hrl").
-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").

%% TLS-1.0 to TLS-1.2 Specific User Events
-export([internal_renegotiation/2,
         renegotiation/1,
         renegotiation/2,
         is_anonymous/1]).

%% Help functions for tls|dtls*_connection.erl
-export([initial_state/9,
         negotiated_hashsign/4,
         finalize_handshake/3,
         make_premaster_secret/2,
         handle_peer_cert/6,
         calculate_master_secret/2,
         calculate_master_secret/5
        ]).

%% General state handling for TLS-1.0 to TLS-1.2
-export([hello/3,
         user_hello/3,
         abbreviated/3,
         certify/3,
         cipher/3,
         connection/3,
         downgrade/3]).

%%--------------------------------------------------------------------
-spec internal_renegotiation(pid(), ssl_record:connection_states()) ->
                                    ok.
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
internal_renegotiation(ConnectionPid, #{current_write := WriteState}) ->
    gen_statem:cast(ConnectionPid, {internal_renegotiate, WriteState}).

%%====================================================================
%% User events
%%====================================================================

%%--------------------------------------------------------------------
-spec renegotiation(pid()) -> ok | {error, ssl:reason()}.
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
renegotiation(ConnectionPid) ->
    ssl_gen_statem:call(ConnectionPid, renegotiate).

renegotiation(Pid, WriteState) ->
    ssl_gen_statem:call(Pid, {user_renegotiate, WriteState}).


is_anonymous(KexAlg) when KexAlg == dh_anon;
                          KexAlg == ecdh_anon;
                          KexAlg == psk;
                          KexAlg == dhe_psk;
                          KexAlg == ecdhe_psk;
                          KexAlg == rsa_psk;
                          KexAlg == srp_anon ->
    true;
is_anonymous(_) ->
    false.

make_premaster_secret(Version, rsa) ->
    Rand = ssl_cipher:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    {MajVer,MinVer} = Version,
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>;
make_premaster_secret(_, _) ->
    undefined.

handle_peer_cert(Role, PeerCert, PublicKeyInfo,
		 #state{handshake_env = HsEnv,
                        static_env = #static_env{protocol_cb = Connection},
                        session = #session{cipher_suite = CipherSuite} = Session} = State0,
		 Connection, Actions) ->
    State1 = State0#state{handshake_env = HsEnv#handshake_env{public_key_info = PublicKeyInfo},
                          session = Session#session{peer_certificate = PeerCert#cert.der}},
    #{key_exchange := KeyAlgorithm} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    State = handle_peer_cert_key(Role, PublicKeyInfo, KeyAlgorithm, State1),
    Connection:next_event(certify, no_record, State, Actions).

handle_peer_cert_key(client,
		     {?'id-ecPublicKey',  #'ECPoint'{point = _ECPoint} = PublicKey,
		      PublicKeyParams},
		     KeyAlg, #state{handshake_env = HsEnv,
                                    session = Session} = State)  when KeyAlg == ecdh_rsa;
                                                                      KeyAlg == ecdh_ecdsa ->
    ECDHKey = public_key:generate_key(PublicKeyParams),
    PremasterSecret = ssl_handshake:premaster_secret(PublicKey, ECDHKey),
    master_secret(PremasterSecret, State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKey},
                                               session = Session#session{ecc = PublicKeyParams}});
handle_peer_cert_key(_, _, _, State) ->
    State.

%%====================================================================
%% Help functions for tls|dtls_connection.erl
%%====================================================================


%% Only called by TLS tls_gen_server and tls_gen_client (no dtls)
initial_state(Role, Sender, Tab, Host, Port, Socket, {SSLOptions, SocketOptions, Trackers}, User,
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

    SslSocket = tls_socket:socket([self(),Sender], CbModule, Socket, tls_gen_connection, Tab, Trackers),

    true = ets:insert(Tab, {{socket_options, packet}, SocketOptions#socket_options.packet}),

    InitStatEnv = #static_env{
                     role = Role,
                     user_socket = SslSocket,
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
       tab = Tab,
       static_env = InitStatEnv,
       handshake_env = #handshake_env{
                          tls_handshake_history = ssl_handshake:init_handshake_history(),
                          renegotiation = {false, first},
                          allow_renegotiate = maps:get(client_renegotiation, SSLOptions, undefined),
                          flight_buffer = []
                         },
       connection_env = #connection_env{user_application = {UserMonitor, User}},
       socket_options = SocketOptions,
       ssl_options = SSLOptions,
       session = #session{is_resumable = false},
       connection_states = ConnectionStates,
       protocol_buffers = #protocol_buffers{},
       user_data_buffer = {[],0,[]},
       recv = #recv{},
       protocol_specific = #{sender => Sender,
                             active_n => ssl_config:get_internal_active_n(
                                           maps:get(erl_dist, SSLOptions, false)),
                             active_n_toggle => true,
                             socket_active => 0
                            }
      }.

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

finalize_handshake(State0, StateName, Connection) ->
    #state{connection_states = ConnectionStates0,
           ssl_options = SslOpts} =
	State1 = cipher_protocol(State0, Connection),

    ConnectionStates =
        ssl_record:activate_pending_connection_state(ConnectionStates0,
                                                     write, Connection),
    State2 = State1#state{connection_states = ConnectionStates},
    State = next_protocol(State2, Connection),
    KeepSecrets = maps:get(keep_secrets, SslOpts, false),
    maybe_keylog_pre_1_3(KeepSecrets, ConnectionStates),
    finished(State, StateName, Connection).

calculate_master_secret(PremasterSecret,
                        #state{static_env = #static_env{role = Role},
                               connection_env =
                                   #connection_env{negotiated_version = Version},
                               session = Session,
                               connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
                                    ConnectionStates0, Role) of
       {MasterSecret, ConnectionStates} ->
           State#state{
             session =
                 Session#session{master_secret = MasterSecret},
             connection_states = ConnectionStates};
       #alert{} = Alert ->
           throw(Alert)
    end.

calculate_master_secret(PremasterSecret, State0, Connection,
			_Current, Next) ->
    State = calculate_master_secret(PremasterSecret, State0),
    Connection:next_event(Next, no_record, State).

%%====================================================================
%% gen_statem general state functions
%%====================================================================

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #server_hello{} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello({call, From}, Msg, State) ->
    handle_call(Msg, From, ?STATE(hello), State);
hello(internal,  #hello_request{}, _) ->
    keep_state_and_data;
hello(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(hello), State).

%%--------------------------------------------------------------------
-spec user_hello(gen_statem:event_type(),
                 #hello_request{} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
user_hello({call, From}, cancel, _State) ->
    gen_statem:reply(From, ok),
    throw(?ALERT_REC(?FATAL, ?USER_CANCELED, user_canceled));
user_hello({call, From}, {handshake_continue, NewOptions, Timeout},
           #state{static_env = #static_env{role = Role},
                  handshake_env = HSEnv,
                  ssl_options = Options0} = State0) ->
    try ssl_config:update_options(NewOptions, Role, Options0) of
        Options ->
            State = ssl_gen_statem:ssl_config(Options, Role, State0),
            {next_state, hello,
             State#state{recv = State#state.recv#recv{from = From},
                         handshake_env =
                             HSEnv#handshake_env{continue_status = continue}
                                           },
             [{{timeout, handshake}, Timeout, close}]}
    catch
        throw:{error, Reason} ->
            gen_statem:reply(From, {error, Reason}),
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason),
                                            ?STATE(user_hello), State0)
    end;
user_hello(info, {'DOWN', _, _, _, _} = Event, State) ->
    ssl_gen_statem:handle_info(Event, ?STATE(user_hello), State);
user_hello(_, _, _) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(),
		  #hello_request{} | #finished{} | term(),
		  #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated({call, From}, Msg, State) ->
    handle_call(Msg, From, ?STATE(abbreviated), State);
abbreviated(internal,
	    #change_cipher_spec{type = <<1>>},
            #state{static_env = #static_env{protocol_cb = Connection},
                   connection_states = ConnectionStates0,
                   handshake_env = HsEnv} = State) ->
    ConnectionStates1 =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    Connection:next_event(?STATE(abbreviated), no_record,
                          State#state{connection_states =
                                          ConnectionStates1,
                                      handshake_env =
                                          HsEnv#handshake_env{expecting_finished = true}});
abbreviated(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(abbreviated), State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(),
	      #hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify({call, From}, Msg, State) ->
    handle_call(Msg, From, certify, State);
certify(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, certify, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(),
	     #hello_request{} | #finished{} | term(),
	     #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher({call, From}, Msg, State) ->
    handle_call(Msg, From, ?STATE(cipher), State);
cipher(internal, #change_cipher_spec{type = <<1>>},
       #state{handshake_env = HsEnv,
              static_env = #static_env{protocol_cb = Connection},
              connection_states = ConnectionStates0} = State) ->
    ConnectionStates =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    Connection:next_event(?STATE(cipher), no_record,
                          State#state{handshake_env = HsEnv#handshake_env{expecting_finished = true},
                                      connection_states = ConnectionStates});
cipher(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(cipher), State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(), term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = undefined}} = State) ->
    ssl_gen_statem:hibernate_after(connection, State, [{reply, From, {error, protocol_not_negotiated}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = SelectedProtocol}} = State) ->
    ssl_gen_statem:hibernate_after(connection, State,
                                       [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = SelectedProtocol,
                                                 negotiated_protocol = undefined}} = State) ->
    ssl_gen_statem:hibernate_after(connection, State,
                                   [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, Msg, State) when element(1, Msg) =:= export_key_materials ->
    handle_call(Msg, From, ?STATE(connection), State);
connection(internal, {handshake, {#hello_request{} = Handshake, _}},
           #state{handshake_env = HsEnv} = State) ->
    %% Should not be included in handshake history
    {next_state, connection,
     State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, peer}}},
     [{next_event, internal, Handshake}]};
connection(Type, Event, State) ->
    ssl_gen_statem:connection(Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(downgrade), State).

%%--------------------------------------------------------------------
%% Event handling functions called by state functions to handle
%% common or unexpected events for the state.
%%--------------------------------------------------------------------
handle_call(renegotiate, From, StateName, _) when StateName =/= connection ->
    {keep_state_and_data, [{reply, From, {error, already_renegotiating}}]};
handle_call({export_key_materials, [Label], [Context0], [WantedLength], _}, From, _,
	    #state{connection_states = ConnectionStates}) ->
    #{security_parameters := SecParams} =
	ssl_record:current_connection_state(ConnectionStates, read),
    #security_parameters{master_secret = MasterSecret,
			 client_random = ClientRandom,
			 server_random = ServerRandom,
			 prf_algorithm = PRFAlgorithm} = SecParams,
    Seed = case Context0 of
               no_context ->
                   <<ClientRandom/binary, ServerRandom/binary>>;
               _ ->
                   Size = erlang:byte_size(Context0),
                   <<ClientRandom/binary, ServerRandom/binary,
                     ?UINT16(Size), Context0/binary>>
           end,

    Reply = try
                {ok, tls_v1:prf(PRFAlgorithm, MasterSecret, Label, Seed, WantedLength)}
	    catch
		exit:Reason:ST ->
                    ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
                    {error, badarg};
		error:Reason:ST ->
                    ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
                    {error, Reason}
	    end,
    {keep_state_and_data, [{reply, From, Reply}]};
handle_call(Msg, From, StateName, State) ->
   ssl_gen_statem:handle_call(Msg, From, StateName, State).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
next_protocol(#state{static_env = #static_env{role = server}} = State, _) ->
    State;
next_protocol(#state{handshake_env = #handshake_env{negotiated_protocol = undefined}} = State, _) ->
    State;
next_protocol(#state{handshake_env = #handshake_env{expecting_next_protocol_negotiation = false}} = State, _) ->
    State;
next_protocol(#state{handshake_env = #handshake_env{negotiated_protocol = NextProtocol}} = State0, Connection) ->
    NextProtocolMessage = ssl_handshake:next_protocol(NextProtocol),
    Connection:queue_handshake(NextProtocolMessage, State0).

cipher_protocol(State, Connection) ->
    Connection:queue_change_cipher(#change_cipher_spec{}, State).

finished(#state{static_env = #static_env{role = Role},
                handshake_env = #handshake_env{tls_handshake_history = Hist},
                connection_env = #connection_env{negotiated_version = Version},
		session = Session,
                connection_states = ConnectionStates0} = State0,
         StateName, Connection) ->
    MasterSecret = Session#session.master_secret,
    #{security_parameters := SecParams} = ssl_record:current_connection_state(ConnectionStates0, write),
    Finished = ssl_handshake:finished(ssl:tls_version(Version), Role,
                                      SecParams#security_parameters.prf_algorithm,
                                      MasterSecret, Hist),
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

master_secret(PremasterSecret, #state{static_env = #static_env{role = Role},
                                      connection_env = #connection_env{negotiated_version = Version},
                                      session = Session,
				      connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, Role) of
	{MasterSecret, ConnectionStates} ->
	    State#state{
	      session =
		  Session#session{master_secret = MasterSecret},
	      connection_states = ConnectionStates};
	#alert{} = Alert ->
	    throw(Alert)
    end.

maybe_keylog_pre_1_3({keylog, Fun}, ConnectionStates) ->
    #{security_parameters := #security_parameters{client_random = ClientRandom,
                                                   master_secret = MasterSecret}}
        = ssl_record:current_connection_state(ConnectionStates, write),
    KeyLog = ssl_logger:keylog_traffic_pre_1_3(ClientRandom, MasterSecret),
    ssl_logger:keylog(KeyLog, ClientRandom, Fun);
maybe_keylog_pre_1_3(_,_) ->
    ok.
