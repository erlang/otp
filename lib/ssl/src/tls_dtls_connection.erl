%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2024. All Rights Reserved.
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

-module(tls_dtls_connection).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("ssl_api.hrl").
-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").

%% TLS-1.0 to TLS-1.2 Specific User Events
-export([renegotiation/1, renegotiation/2]).

%% Data handling. Note renegotiation is replaced by sesion key update mechanism in TLS-1.3
-export([internal_renegotiation/2]).

%% Help functions for tls|dtls_connection.erl
-export([handle_session/7,
         handle_sni_extension/2,
         handle_call/4]).

%% General state handlingfor TLS-1.0 to TLS-1.2 and gen_handshake that wraps
%% handling of common state handling for handshake messages for error handling
-export([hello/3,
         user_hello/3,
         abbreviated/3,
         certify/3,
         wait_cert_verify/3,
         wait_ocsp_stapling/3,
         cipher/3,
         connection/3,
         downgrade/3,
         gen_handshake/4]).

%% Tracing
-export([handle_trace/3]).

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
-spec renegotiation(pid()) -> ok | {error, reason()}.
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
renegotiation(ConnectionPid) ->
    ssl_gen_statem:call(ConnectionPid, renegotiate).

renegotiation(Pid, WriteState) ->
    ssl_gen_statem:call(Pid, {user_renegotiate, WriteState}).

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
	       #state{session = Session,
		      handshake_env = #handshake_env{negotiated_protocol = CurrentProtocol} = HsEnv,
                      connection_env = #connection_env{negotiated_version = ReqVersion} = CEnv} = State0) ->
    #{key_exchange := KeyAlgorithm} =
	ssl_cipher_format:suite_bin_to_map(CipherSuite),
    
    PremasterSecret = make_premaster_secret(ReqVersion, KeyAlgorithm),

    {ExpectNPN, Protocol} =
        case Protocol0 of
            undefined -> {false, CurrentProtocol};
            _ -> {ProtoExt =:= npn, Protocol0}
        end,

    State = State0#state{connection_states = ConnectionStates,
			 handshake_env = HsEnv#handshake_env{kex_algorithm = KeyAlgorithm,
                                                             premaster_secret = PremasterSecret,
                                                             expecting_next_protocol_negotiation = ExpectNPN,
                                                             negotiated_protocol = Protocol},
                         connection_env = CEnv#connection_env{negotiated_version = Version}},
    
    case ssl_session:is_new(Session, NewId) of
	true ->
	    handle_new_session(NewId, CipherSuite, Compression,
			       State#state{connection_states = ConnectionStates});
	false ->
	    handle_resumed_session(NewId,
				   State#state{connection_states = ConnectionStates})
    end.


%%====================================================================
%% gen_statem general state functions with connection cb argument
%%====================================================================	

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #server_hello{} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello({call, From}, Msg, State) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State);
hello(internal, {common_client_hello, Type, ServerHelloExt}, State) ->
    do_server_hello(Type, ServerHelloExt, State);
hello(info, Msg, State) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
hello(internal,  #hello_request{}, _) ->
    keep_state_and_data;
hello(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?FUNCTION_NAME, State).

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
    try ssl:update_options(NewOptions, Role, Options0) of
        Options ->
            State = ssl_gen_statem:ssl_config(Options, Role, State0),
            {next_state, hello, State#state{start_or_recv_from = From,
                                            handshake_env = HSEnv#handshake_env{continue_status = continue}
                                           },
             [{{timeout, handshake}, Timeout, close}]}
    catch
        throw:{error, Reason} ->
            gen_statem:reply(From, {error, Reason}),
            ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, Reason), ?FUNCTION_NAME, State0)
    end;
user_hello(info, {'DOWN', _, _, _, _} = Event, State) ->
    ssl_gen_statem:handle_info(Event, ?FUNCTION_NAME, State);
user_hello(_, _, _) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(),
		  #hello_request{} | #finished{} | term(),
		  #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated({call, From}, Msg, State) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State);
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{static_env = #static_env{role = server,
                                            protocol_cb = Connection},
                   handshake_env = #handshake_env{tls_handshake_history = Hist,
                                                  expecting_finished = true} = HsEnv,
		   connection_env = #connection_env{negotiated_version = Version},
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} =
		State0) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished, client,
					 get_current_prf(ConnectionStates0, write),
					 MasterSecret, Hist) of
        verified ->
	    ConnectionStates =
		ssl_record:set_client_verify_data(current_both, Data, ConnectionStates0),
	    {Record, State} =
                ssl_gen_statem:prepare_connection(State0#state{connection_states = ConnectionStates,
                                                                   handshake_env = HsEnv#handshake_env{expecting_finished = false}},
                                                      Connection),
	    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close}]);
	#alert{} = Alert ->
            throw(Alert)
    end;
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{static_env = #static_env{role = client,
                                            protocol_cb = Connection},
                   handshake_env = #handshake_env{tls_handshake_history = Hist0},
                   connection_env = #connection_env{negotiated_version = Version},
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} = State0) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished, server,
					 get_pending_prf(ConnectionStates0, write),
					 MasterSecret, Hist0) of
        verified ->
	    ConnectionStates1 =
		ssl_record:set_server_verify_data(current_read, Data, ConnectionStates0),
	    {#state{handshake_env = HsEnv} = State1, Actions} =
		finalize_handshake(State0#state{connection_states = ConnectionStates1},
				   ?FUNCTION_NAME, Connection),
	    {Record, State} =
                ssl_gen_statem:prepare_connection(State1#state{handshake_env = HsEnv#handshake_env{expecting_finished = false}},
                                                      Connection),
	    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close} | Actions]);
	#alert{} = Alert ->
            throw(Alert)
    end;
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
abbreviated(internal, #next_protocol{selected_protocol = SelectedProtocol},
	    #state{static_env = #static_env{role = server,
                                            protocol_cb = Connection},
                   handshake_env = #handshake_env{expecting_next_protocol_negotiation = true} = HsEnv} = State) ->
    Connection:next_event(?FUNCTION_NAME, no_record, 
			  State#state{handshake_env = HsEnv#handshake_env{negotiated_protocol = SelectedProtocol,
                                                                         expecting_next_protocol_negotiation = false}});
abbreviated(internal, 
	    #change_cipher_spec{type = <<1>>},  
            #state{static_env = #static_env{protocol_cb = Connection},
                   connection_states = ConnectionStates0,
                   handshake_env = HsEnv} = State) ->
    ConnectionStates1 =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    Connection:next_event(?FUNCTION_NAME, no_record,
                          State#state{connection_states =
                                          ConnectionStates1,
                                      handshake_env = HsEnv#handshake_env{expecting_finished = true}});
abbreviated(info, Msg, State) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
abbreviated(internal, #hello_request{}, _) ->
    keep_state_and_data;
abbreviated(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec wait_ocsp_stapling(gen_statem:event_type(),
                         #certificate{} |  #certificate_status{} | term(),
	                     #state{}) ->
		gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_ocsp_stapling(internal, #certificate{},
                   #state{static_env = #static_env{protocol_cb = _Connection}} = State) ->
    %% Postpone message, should be handled in certify after receiving staple message
    {next_state, ?FUNCTION_NAME, State, [{postpone, true}]};
%% Receive OCSP staple message
wait_ocsp_stapling(internal, #certificate_status{} = CertStatus,
                   #state{static_env = #static_env{protocol_cb = _Connection},
                          handshake_env =
                              #handshake_env{ocsp_stapling_state = OcspState} = HsEnv} = State) ->
    {next_state, certify,
     State#state{handshake_env =
                     HsEnv#handshake_env{ocsp_stapling_state =
                                             OcspState#{ocsp_expect => stapled,
                                                        ocsp_response => CertStatus}}}};
%% Server did not send OCSP staple message
wait_ocsp_stapling(internal, Msg,
                   #state{static_env = #static_env{protocol_cb = _Connection},
                          handshake_env = #handshake_env{
                                             ocsp_stapling_state = OcspState} = HsEnv} = State)
  when is_record(Msg, server_key_exchange) orelse
       is_record(Msg, hello_request) orelse
       is_record(Msg, certificate_request) orelse
       is_record(Msg, server_hello_done) orelse
       is_record(Msg, client_key_exchange) ->
    {next_state, certify,
     State#state{handshake_env =
                     HsEnv#handshake_env{ocsp_stapling_state =
                                             OcspState#{ocsp_expect => undetermined}}},
     [{postpone, true}]};
wait_ocsp_stapling(internal, #hello_request{}, _) ->
    keep_state_and_data;
wait_ocsp_stapling(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(),
	      #hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify({call, From}, Msg, State) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State);
certify(info, Msg, State) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
certify(internal, #certificate{asn1_certificates = []},
	#state{static_env = #static_env{role = server},
	       ssl_options = #{verify := verify_peer, fail_if_no_peer_cert := true}}) ->
    throw(?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, no_client_certificate_provided));
certify(internal, #certificate{asn1_certificates = []},
	#state{static_env = #static_env{role = server,
                                        protocol_cb = Connection},
	       ssl_options = #{verify := verify_peer,
                               fail_if_no_peer_cert := false}} =
	State0) ->
    Connection:next_event(?FUNCTION_NAME, no_record, State0#state{client_certificate_status = empty});
certify(internal, #certificate{},
	#state{static_env = #static_env{role = server},
	       ssl_options = #{verify := verify_none}}) ->
    throw(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, unrequested_certificate));
certify(internal, #certificate{},
        #state{static_env = #static_env{protocol_cb = Connection},
               handshake_env = #handshake_env{
                              ocsp_stapling_state = #{ocsp_expect := staple}}} = State) ->
    Connection:next_event(wait_ocsp_stapling, no_record, State, [{postpone, true}]);
certify(internal, #certificate{asn1_certificates = [Peer|_]} = Cert,
        #state{static_env = #static_env{
                               role = Role,
                               host = Host,
                               protocol_cb = Connection,
                               cert_db = CertDbHandle,
                               cert_db_ref = CertDbRef,
                               crl_db = CRLDbInfo},
               handshake_env = #handshake_env{
                                  ocsp_stapling_state = #{ocsp_expect := Status} = OcspState},
               connection_env = #connection_env{
                                   negotiated_version = Version},
               ssl_options = Opts} = State0) when Status =/= staple ->
    OcspInfo = ocsp_info(OcspState, Opts, Peer),
    case ssl_handshake:certify(Cert, CertDbHandle, CertDbRef,
                               Opts, CRLDbInfo, Role, Host,
                               ensure_tls(Version), OcspInfo) of
        {PeerCert, PublicKeyInfo} ->
            State = case Role of
                        server ->
                            State0#state{client_certificate_status = needs_verifying};
                        client ->
                            State0
                    end,
            handle_peer_cert(Role, PeerCert, PublicKeyInfo, State, Connection, []);
        #alert{} = Alert ->
            throw(Alert)
    end;
certify(internal, #server_key_exchange{exchange_keys = Keys},
        #state{static_env = #static_env{role = client,
                                        protocol_cb = Connection},
               handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                              public_key_info = PubKeyInfo} = HsEnv,
               connection_env = #connection_env{negotiated_version = Version},
               session = Session,
	       connection_states = ConnectionStates} = State)
  when KexAlg == dhe_dss; 
       KexAlg == dhe_rsa;
       KexAlg == ecdhe_rsa; 
       KexAlg == ecdhe_ecdsa;
       KexAlg == dh_anon; 
       KexAlg == ecdh_anon;
       KexAlg == psk; 
       KexAlg == dhe_psk; 
       KexAlg == ecdhe_psk; 
       KexAlg == rsa_psk;
       KexAlg == srp_dss; 
       KexAlg == srp_rsa; 
       KexAlg == srp_anon ->
    
    Params = ssl_handshake:decode_server_key(Keys, KexAlg, ssl:tls_version(Version)),

    %% Use negotiated value if TLS-1.2 otherwise return default
    HashSign = negotiated_hashsign(Params#server_key_params.hashsign, KexAlg, PubKeyInfo, ssl:tls_version(Version)),

    case is_anonymous(KexAlg) of
	true ->
	    calculate_secret(Params#server_key_params.params,
			     State#state{handshake_env = HsEnv#handshake_env{hashsign_algorithm = HashSign}}, Connection);
	false ->
	    case  ssl_handshake:verify_server_key(Params, HashSign, 
						  ConnectionStates, ssl:tls_version(Version), PubKeyInfo) of
		true ->
		    calculate_secret(Params#server_key_params.params,
				     State#state{handshake_env = HsEnv#handshake_env{hashsign_algorithm = HashSign},
                                                 session = session_handle_params(Params#server_key_params.params, Session)},
                    Connection);
		false ->
                    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
	    end
    end;
certify(internal, #certificate_request{},
	#state{static_env = #static_env{role = client},
               handshake_env = #handshake_env{kex_algorithm = KexAlg}})
  when KexAlg == dh_anon; 
       KexAlg == ecdh_anon;
       KexAlg == psk; 
       KexAlg == dhe_psk; 
       KexAlg == ecdhe_psk; 
       KexAlg == rsa_psk;
       KexAlg == srp_dss; 
       KexAlg == srp_rsa; 
       KexAlg == srp_anon ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE));
certify(internal, #certificate_request{},
	#state{static_env = #static_env{role = client,
                                        protocol_cb = Connection},
               session = Session0,
               connection_env = #connection_env{cert_key_alts = [#{certs := [[]]}]}} = State) ->
    %% The client does not have a certificate and will send an empty reply, the server may fail 
    %% or accept the connection by its own preference. No signature algorithms needed as there is
    %% no certificate to verify.
    Connection:next_event(?FUNCTION_NAME, no_record, State#state{client_certificate_status = requested,
                                                                 session = Session0#session{own_certificates = [[]],
                                                                                            private_key = #{}}});
certify(internal, #certificate_request{} = CertRequest,
	#state{static_env = #static_env{role = client,
                                        protocol_cb = Connection,
                                        cert_db = CertDbHandle,
                                        cert_db_ref = CertDbRef},
               connection_env = #connection_env{negotiated_version = Version,
                                                cert_key_alts = CertKeyAlts
                                               },
               session = Session0,
               ssl_options = #{signature_algs := SupportedHashSigns}} = State) ->
    TLSVersion = ssl:tls_version(Version),
    CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts, ssl:tls_version(Version)),
    Session = select_client_cert_key_pair(Session0, CertRequest, CertKeyPairs,
                                          SupportedHashSigns, TLSVersion,
                                          CertDbHandle, CertDbRef),
    Connection:next_event(?FUNCTION_NAME, no_record,
                          State#state{client_certificate_status = requested,
                                      session = Session});
%% PSK and RSA_PSK might bypass the Server-Key-Exchange
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{role = client,
                                        protocol_cb = Connection},
               session = #session{master_secret = undefined},
               handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                              premaster_secret = undefined,
                                              server_psk_identity = PSKIdentity} = HsEnv,
	       ssl_options = #{user_lookup_fun := PSKLookup}} = State0)
  when KexAlg == psk ->
    case ssl_handshake:premaster_secret({KexAlg, PSKIdentity}, PSKLookup) of
	#alert{} = Alert ->
            throw(Alert);
	PremasterSecret ->
	    State = master_secret(PremasterSecret,
				  State0#state{handshake_env =
                                                   HsEnv#handshake_env{premaster_secret = PremasterSecret}}),
            client_certify_and_key_exchange(State, Connection)
    end;
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{role = client,
                                       protocol_cb = Connection},
               connection_env = #connection_env{negotiated_version = {Major, Minor}},
               handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                              premaster_secret = undefined,
                                              server_psk_identity = PSKIdentity} = HsEnv,
               session = #session{master_secret = undefined},
	       ssl_options = #{user_lookup_fun := PSKLookup}} = State0)
  when KexAlg == rsa_psk ->
    Rand = ssl_cipher:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    RSAPremasterSecret = <<?BYTE(Major), ?BYTE(Minor), Rand/binary>>,
    case ssl_handshake:premaster_secret({KexAlg, PSKIdentity}, PSKLookup, 
					RSAPremasterSecret) of
	#alert{} = Alert ->
            throw(Alert);
	PremasterSecret ->
	    State = master_secret(PremasterSecret, 
				  State0#state{handshake_env = 
                                                   HsEnv#handshake_env{premaster_secret = RSAPremasterSecret}}),
	    client_certify_and_key_exchange(State, Connection)
    end;
%% Master secret was determined with help of server-key exchange msg
certify(internal, #server_hello_done{}, 
	#state{static_env = #static_env{role = client,
                                        protocol_cb = Connection},
               connection_env = #connection_env{negotiated_version = Version},
               handshake_env = #handshake_env{premaster_secret = undefined},
               session = #session{master_secret = MasterSecret} = Session,
	       connection_states = ConnectionStates0} = State0) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    State = State0#state{connection_states = ConnectionStates},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
            throw(Alert)
    end;
%% Master secret is calculated from premaster_secret
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{role = client,
                                       protocol_cb = Connection},
               connection_env = #connection_env{negotiated_version = Version},
               handshake_env = #handshake_env{premaster_secret = PremasterSecret},
               session = Session0,
	       connection_states = ConnectionStates0} = State0) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates,
				 session = Session},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
            throw(Alert)
    end;
certify(internal = Type, #client_key_exchange{} = Msg,
	#state{static_env = #static_env{role = server},
	       client_certificate_status = requested,
	       ssl_options = #{fail_if_no_peer_cert := true}}) ->
    %% We expect a certificate here
    throw(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, {unexpected_msg, {Type, Msg}}));
certify(internal, #client_key_exchange{exchange_keys = Keys},
	State = #state{handshake_env = #handshake_env{kex_algorithm = KeyAlg}, 
                       static_env = #static_env{protocol_cb = Connection},
                       connection_env = #connection_env{negotiated_version = Version}}) ->
    try
	certify_client_key_exchange(ssl_handshake:decode_client_key(Keys, KeyAlg, ssl:tls_version(Version)),
				    State, Connection)
    catch
	#alert{} = Alert ->
            throw(Alert)
    end;
certify(internal, #hello_request{}, _) ->
    keep_state_and_data;
certify(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?FUNCTION_NAME, State).
 
%%--------------------------------------------------------------------
-spec wait_cert_verify(gen_statem:event_type(),
	      #hello_request{} | #certificate_verify{} | term(),
	     #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert_verify(internal, #certificate_verify{signature = Signature, 
                                               hashsign_algorithm = CertHashSign},
                 #state{static_env = #static_env{role = server,
                                                 protocol_cb = Connection},
                        client_certificate_status = needs_verifying,
                        handshake_env = #handshake_env{tls_handshake_history = Hist,
                                                       kex_algorithm = KexAlg,
                                             public_key_info = PubKeyInfo},
                        connection_env = #connection_env{negotiated_version = Version},
                        session = #session{master_secret = MasterSecret} = Session0
                       } = State) ->
    
    TLSVersion = ssl:tls_version(Version),
    %% Use negotiated value if TLS-1.2 otherwise return default
    HashSign = negotiated_hashsign(CertHashSign, KexAlg, PubKeyInfo, TLSVersion),
    case ssl_handshake:certificate_verify(Signature, PubKeyInfo,
					  TLSVersion, HashSign, MasterSecret, Hist) of
	valid ->
	    Connection:next_event(cipher, no_record,
				  State#state{client_certificate_status = verified,
                                              session = Session0#session{sign_alg = HashSign}});
	#alert{} = Alert ->
            throw(Alert)
    end;
wait_cert_verify(internal, #hello_request{}, _) ->
    keep_state_and_data;
wait_cert_verify(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?FUNCTION_NAME, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(),
	     #hello_request{} | #finished{} | term(),
	     #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher({call, From}, Msg, State) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State);
cipher(info, Msg, State) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
cipher(internal, #finished{verify_data = Data} = Finished,
       #state{static_env = #static_env{role = Role,
                                       host = Host,
                                       port = Port,
                                       trackers = Trackers},
              handshake_env = #handshake_env{tls_handshake_history = Hist,
                                             expecting_finished = true} = HsEnv,
              connection_env = #connection_env{negotiated_version = Version},
	      session = #session{master_secret = MasterSecret}
	      = Session0,
              ssl_options = SslOpts,
	      connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished,
					 opposite_role(Role),
					 get_current_prf(ConnectionStates0, read),
					 MasterSecret, Hist) of
        verified ->
	    Session = handle_session(Role, SslOpts, Host, Port, Trackers, Session0),
	    cipher_role(Role, Data, Session,
			State#state{handshake_env = HsEnv#handshake_env{expecting_finished = false}});
        #alert{} = Alert ->
            throw(Alert)
    end;
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
cipher(internal, #next_protocol{selected_protocol = SelectedProtocol},
       #state{static_env = #static_env{role = server, protocol_cb = Connection},
              handshake_env = #handshake_env{expecting_finished = true,
                                             expecting_next_protocol_negotiation = true} = HsEnv} = State) ->
    Connection:next_event(?FUNCTION_NAME, no_record,
			  State#state{handshake_env = HsEnv#handshake_env{negotiated_protocol = SelectedProtocol,
                                                                          expecting_next_protocol_negotiation = false}});
cipher(internal, #change_cipher_spec{type = <<1>>},
       #state{handshake_env = HsEnv,
              static_env = #static_env{protocol_cb = Connection},
              connection_states = ConnectionStates0} = State) ->
    ConnectionStates =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    Connection:next_event(?FUNCTION_NAME, no_record, State#state{handshake_env = HsEnv#handshake_env{expecting_finished = true},
                                                                 connection_states = ConnectionStates});
cipher(internal, #hello_request{}, _) ->
    keep_state_and_data;
cipher(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?FUNCTION_NAME, State).
 
%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(), term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection({call, From}, renegotiate, #state{static_env = #static_env{protocol_cb = tls_gen_connection},
                                             handshake_env = HsEnv} = State) -> 
    tls_connection:renegotiate(State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, From}}}, []);
connection({call, From}, renegotiate, #state{static_env = #static_env{protocol_cb = dtls_gen_connection},
                                             handshake_env = HsEnv} = State) -> 
    dtls_connection:renegotiate(State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, From}}}, []);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = undefined}} = State) ->
    ssl_gen_statem:hibernate_after(?FUNCTION_NAME, State, [{reply, From, {error, protocol_not_negotiated}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = SelectedProtocol}} = State) ->
    ssl_gen_statem:hibernate_after(?FUNCTION_NAME, State,
                                       [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = SelectedProtocol,
                                                 negotiated_protocol = undefined}} = State) ->
    ssl_gen_statem:hibernate_after(?FUNCTION_NAME, State,
                                       [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, Msg, State) when element(1, Msg) =:= export_key_materials ->
    handle_call(Msg, From, ?FUNCTION_NAME, State);
connection(cast, {internal_renegotiate, WriteState}, #state{static_env = #static_env{protocol_cb = tls_gen_connection},
                                                            handshake_env = HsEnv,
                                                            connection_states = ConnectionStates}
           = State) ->
    tls_connection:renegotiate(State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}},
                            connection_states = ConnectionStates#{current_write => WriteState}}, []);
connection(cast, {internal_renegotiate, WriteState}, #state{static_env = #static_env{protocol_cb = dtls_gen_connection},
                                                            handshake_env = HsEnv,
                                                            connection_states = ConnectionStates}
           = State) ->
    dtls_connection:renegotiate(State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}},
                            connection_states = ConnectionStates#{current_write => WriteState}}, []);

connection(internal, {handshake, {#hello_request{} = Handshake, _}},
           #state{handshake_env = HsEnv} = State) ->
    %% Should not be included in handshake history
    {next_state, ?FUNCTION_NAME, State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, peer}}},
     [{next_event, internal, Handshake}]};
connection(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?FUNCTION_NAME, State).

gen_handshake(StateName, Type, Event, State) ->
    try
        tls_dtls_connection:StateName(Type, Event, State)
    catch error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, malformed_handshake_data))
    end.

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

handle_info(Msg, StateName, State) ->
    ssl_gen_statem:handle_info(Msg, StateName, State).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_server_hello(Type, #{next_protocol_negotiation := NextProtocols} =
		    ServerHelloExt,
		#state{connection_env = #connection_env{negotiated_version = Version},
                       static_env = #static_env{protocol_cb = Connection},
                       handshake_env = HsEnv,
		       session = #session{session_id = SessId},
		       connection_states = ConnectionStates0,
                       ssl_options = #{versions := [HighestVersion|_]}}
		= State0) when is_atom(Type) ->
    %% TLS 1.3 - Section 4.1.3
    %% Override server random values for TLS 1.3 downgrade protection mechanism.
    ConnectionStates1 = update_server_random(ConnectionStates0, Version, HighestVersion),
    State1 = State0#state{connection_states = ConnectionStates1},
    ServerHello =
	ssl_handshake:server_hello(SessId, ssl:tls_version(Version),
                                   ConnectionStates1, ServerHelloExt),
    
    State = server_hello(ServerHello,
			 State1#state{handshake_env = HsEnv#handshake_env{expecting_next_protocol_negotiation =
                                                                              NextProtocols =/= undefined}}, Connection),
    case Type of
	new ->
	    new_server_hello(ServerHello, State, Connection);
	resumed ->
	    resumed_server_hello(State, Connection)
    end.

update_server_random(#{pending_read := #{security_parameters := ReadSecParams0} =
                           ReadState0,
                       pending_write := #{security_parameters := WriteSecParams0} =
                           WriteState0} = ConnectionStates,
                     Version, HighestVersion) ->
    ReadRandom = override_server_random(
                   ReadSecParams0#security_parameters.server_random,
                   Version,
                   HighestVersion),
    WriteRandom = override_server_random(
                    WriteSecParams0#security_parameters.server_random,
                    Version,
                    HighestVersion),
    ReadSecParams = ReadSecParams0#security_parameters{server_random = ReadRandom},
    WriteSecParams = WriteSecParams0#security_parameters{server_random = WriteRandom},
    ReadState = ReadState0#{security_parameters => ReadSecParams},
    WriteState = WriteState0#{security_parameters => WriteSecParams},

    ConnectionStates#{pending_read => ReadState, pending_write => WriteState}.

%% TLS 1.3 - Section 4.1.3
%%
%% If negotiating TLS 1.2, TLS 1.3 servers MUST set the last eight bytes
%% of their Random value to the bytes:
%%
%%   44 4F 57 4E 47 52 44 01
%%
%% If negotiating TLS 1.1 or below, TLS 1.3 servers MUST and TLS 1.2
%% servers SHOULD set the last eight bytes of their Random value to the
%% bytes:
%%
%%   44 4F 57 4E 47 52 44 00
override_server_random(<<Random0:24/binary,_:8/binary>> = Random, {M,N}, {Major,Minor})
  when Major > 3 orelse Major =:= 3 andalso Minor >= 4 -> %% TLS 1.3 or above
    if M =:= 3 andalso N =:= 3 ->                         %% Negotiating TLS 1.2
            Down = ?RANDOM_OVERRIDE_TLS12,
            <<Random0/binary,Down/binary>>;
       M =:= 3 andalso N < 3 ->                           %% Negotiating TLS 1.1 or prior
            Down = ?RANDOM_OVERRIDE_TLS11,
            <<Random0/binary,Down/binary>>;
       true ->
            Random
    end;
override_server_random(<<Random0:24/binary,_:8/binary>> = Random, {M,N}, {Major,Minor})
  when Major =:= 3 andalso Minor =:= 3 ->   %% TLS 1.2
    if M =:= 3 andalso N < 3 ->             %% Negotiating TLS 1.1 or prior
            Down = ?RANDOM_OVERRIDE_TLS11,
            <<Random0/binary,Down/binary>>;
       true ->
            Random
    end;
override_server_random(Random, _, _) ->
    Random.

new_server_hello(#server_hello{cipher_suite = CipherSuite,
			      compression_method = Compression,
			      session_id = SessionId},
                 #state{session = Session0,
                        static_env = #static_env{protocol_cb = Connection}} = State0, Connection) ->
    #state{} = State1 = server_certify_and_key_exchange(State0, Connection),
    {State, Actions} = server_hello_done(State1, Connection),
    Session = Session0#session{session_id = SessionId,
                               cipher_suite = CipherSuite,
                               compression_method = Compression},
    Connection:next_event(certify, no_record, State#state{session = Session}, Actions).

resumed_server_hello(#state{session = Session,
			    connection_states = ConnectionStates0,
                            static_env = #static_env{protocol_cb = Connection},
			    connection_env = #connection_env{negotiated_version = Version}} = State0, Connection) ->

    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, server) of
	{_, ConnectionStates1} ->
	    State1 = State0#state{connection_states = ConnectionStates1,
				  session = Session},
	    {State, Actions} =
		finalize_handshake(State1, abbreviated, Connection),
	    Connection:next_event(abbreviated, no_record, State, Actions);
	#alert{} = Alert ->
            throw(Alert)
    end.

server_hello(ServerHello, State0, Connection) ->
    CipherSuite = ServerHello#server_hello.cipher_suite,
    #{key_exchange := KeyAlgorithm}  = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(ServerHello, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_algorithm = KeyAlgorithm}}.

server_hello_done(State, Connection) ->
    HelloDone = ssl_handshake:server_hello_done(),
    Connection:send_handshake(HelloDone, State).

handle_peer_cert(Role, PeerCert, PublicKeyInfo,
		 #state{handshake_env = HsEnv,
                        static_env = #static_env{protocol_cb = Connection},
                        session = #session{cipher_suite = CipherSuite} = Session} = State0,
		 Connection, Actions) ->
    State1 = State0#state{handshake_env = HsEnv#handshake_env{public_key_info = PublicKeyInfo},
                          session =
                              Session#session{peer_certificate = PeerCert}},
    #{key_exchange := KeyAlgorithm} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    State = handle_peer_cert_key(Role, PeerCert, PublicKeyInfo, KeyAlgorithm, State1),
    Connection:next_event(certify, no_record, State, Actions).

handle_peer_cert_key(client, _,
		     {?'id-ecPublicKey',  #'ECPoint'{point = _ECPoint} = PublicKey,
		      PublicKeyParams},
		     KeyAlg, #state{handshake_env = HsEnv,
                                    session = Session} = State)  when KeyAlg == ecdh_rsa;
                                                                      KeyAlg == ecdh_ecdsa ->
    ECDHKey = public_key:generate_key(PublicKeyParams),
    PremasterSecret = ssl_handshake:premaster_secret(PublicKey, ECDHKey),
    master_secret(PremasterSecret, State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKey},
                                               session = Session#session{ecc = PublicKeyParams}});
handle_peer_cert_key(_, _, _, _, State) ->
    State.

certify_client(#state{static_env = #static_env{role = client,
                                               cert_db = CertDbHandle,
                                               cert_db_ref = CertDbRef},
                      client_certificate_status = requested,
		      session = #session{own_certificates = OwnCerts}}
	       = State, Connection) ->
    Certificate = ssl_handshake:certificate(OwnCerts, CertDbHandle, CertDbRef, client),
    Connection:queue_handshake(Certificate, State);
certify_client(#state{client_certificate_status = not_requested} = State, _) ->
    State.

verify_client_cert(#state{static_env = #static_env{role = client},
                          handshake_env = #handshake_env{tls_handshake_history = Hist},
                          connection_env = #connection_env{negotiated_version = Version},
                          client_certificate_status = requested,
			  session = #session{sign_alg = HashSign,
                                             master_secret = MasterSecret,
                                             private_key = PrivateKey,
                                             own_certificates = OwnCerts}} = State, Connection) ->
    case ssl_handshake:client_certificate_verify(OwnCerts, MasterSecret,
						 ssl:tls_version(Version), HashSign, PrivateKey, Hist) of
        #certificate_verify{} = Verified ->
           Connection:queue_handshake(Verified, State);
	ignore ->
	    State;
	#alert{} = Alert ->
	    throw(Alert)
    end;
verify_client_cert(#state{client_certificate_status = not_requested} = State, _) ->
    State.

client_certify_and_key_exchange(State0, Connection) ->
    State1 = do_client_certify_and_key_exchange(State0, Connection),
    {State2, Actions} = finalize_handshake(State1, certify, Connection),
    State = State2#state{client_certificate_status = not_requested},     %% Reinitialize
    Connection:next_event(cipher, no_record, State, Actions).

do_client_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_client(State0, Connection),
    State2 = key_exchange(State1, Connection),
    verify_client_cert(State2, Connection).

server_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_server(State0, Connection),
    State2 = key_exchange(State1, Connection),
    request_client_cert(State2, Connection).

certify_client_key_exchange(#encrypted_premaster_secret{premaster_secret= EncPMS},
			    #state{session = #session{private_key = PrivateKey},
                                   handshake_env = #handshake_env{client_hello_version = {Major, Minor} = Version},
                                   client_certificate_status = CCStatus}
                            = State, Connection) ->
    FakeSecret = make_premaster_secret(Version, rsa),
    %% Countermeasure for Bleichenbacher attack always provide some kind of premaster secret
    %% and fail handshake later.RFC 5246 section 7.4.7.1.
    PremasterSecret =
        try ssl_handshake:premaster_secret(EncPMS, PrivateKey) of
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
    calculate_master_secret(PremasterSecret, State, Connection, certify, client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_diffie_hellman_public{dh_public = ClientPublicDhKey},
			    #state{handshake_env = #handshake_env{diffie_hellman_params = #'DHParameter'{} = Params,
                                                                  kex_keys = {_, ServerDhPrivateKey}},
                                   client_certificate_status = CCStatus
				  } = State,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientPublicDhKey, ServerDhPrivateKey, Params),
    calculate_master_secret(PremasterSecret, State, Connection, certify, client_kex_next_state(CCStatus));

certify_client_key_exchange(#client_ec_diffie_hellman_public{dh_public = ClientPublicEcDhPoint},
			    #state{handshake_env = #handshake_env{kex_keys = ECDHKey},
                                   client_certificate_status = CCStatus
                                  } = State, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(#'ECPoint'{point = ClientPublicEcDhPoint}, ECDHKey),
    calculate_master_secret(PremasterSecret, State, Connection, certify, client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_psk_identity{} = ClientKey,
			    #state{ssl_options = 
				       #{user_lookup_fun := PSKLookup},
                                   client_certificate_status = CCStatus
                                  } = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_dhe_psk_identity{} = ClientKey,
			    #state{handshake_env = #handshake_env{diffie_hellman_params = #'DHParameter'{} = Params,
                                                                  kex_keys = {_, ServerDhPrivateKey}},
				   ssl_options = 
				       #{user_lookup_fun := PSKLookup},
                                   client_certificate_status = CCStatus
                                  } = State0,
			    Connection) ->
    PremasterSecret = 
	ssl_handshake:premaster_secret(ClientKey, ServerDhPrivateKey, Params, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_ecdhe_psk_identity{} = ClientKey,
			    #state{handshake_env = #handshake_env{kex_keys = ServerEcDhPrivateKey},
				   ssl_options =
				       #{user_lookup_fun := PSKLookup},
                                   client_certificate_status = CCStatus
                                  } = State,
			    Connection) ->
    PremasterSecret =
	ssl_handshake:premaster_secret(ClientKey, ServerEcDhPrivateKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State, Connection, certify, client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_rsa_psk_identity{} = ClientKey,
			    #state{session = #session{private_key = PrivateKey},
				   ssl_options = 
				       #{user_lookup_fun := PSKLookup},
                                   client_certificate_status = CCStatus
                                  } = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, PrivateKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_srp_public{} = ClientKey,
			    #state{handshake_env = #handshake_env{srp_params = Params,
                                                                  kex_keys = Key},
                                   client_certificate_status = CCStatus
				  } = State0, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, Params),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, client_kex_next_state(CCStatus)).

client_kex_next_state(needs_verifying) ->
    wait_cert_verify;
client_kex_next_state(empty) ->
    cipher;
client_kex_next_state(not_requested) ->
    cipher.

certify_server(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg}} = 
                   State, _) when KexAlg == dh_anon; 
                                  KexAlg == ecdh_anon; 
                                  KexAlg == psk; 
                                  KexAlg == dhe_psk; 
                                  KexAlg == ecdhe_psk; 
                                  KexAlg == srp_anon  ->
    State;
certify_server(#state{static_env = #static_env{cert_db = CertDbHandle,
                                               cert_db_ref = CertDbRef},
		      session = #session{own_certificates = OwnCerts}} = State, Connection) ->
    Cert = ssl_handshake:certificate(OwnCerts, CertDbHandle, CertDbRef, server),
    #certificate{} = Cert,  %% Assert
    Connection:queue_handshake(Cert, State).

key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = rsa}} = State,_) ->
    State;
key_exchange(#state{static_env = #static_env{role = server}, 
		    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   diffie_hellman_params = #'DHParameter'{} = Params,
                                                   hashsign_algorithm = HashSignAlgo},
                    connection_env = #connection_env{negotiated_version = Version},
                    session = #session{private_key = PrivateKey},
		    connection_states = ConnectionStates0} = State0, Connection)
  when KexAlg == dhe_dss;
       KexAlg == dhe_rsa;
       KexAlg == dh_anon ->
    DHKeys = public_key:generate_key(Params),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, ssl:tls_version(Version), {dh, DHKeys, Params,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = DHKeys}};
key_exchange(#state{static_env = #static_env{role = server},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg} = HsEnv,
                    session = #session{private_key = #'ECPrivateKey'{parameters = ECCurve} = Key} = Session} = State, _)
  when KexAlg == ecdh_ecdsa; 
       KexAlg == ecdh_rsa ->
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = Key},
                session = Session#session{ecc = ECCurve}};
key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   hashsign_algorithm = HashSignAlgo},
                    connection_env = #connection_env{negotiated_version = Version},
		    session = #session{ecc = ECCCurve, private_key = PrivateKey},
		    connection_states = ConnectionStates0} = State0, Connection)
  when KexAlg == ecdhe_ecdsa; 
       KexAlg == ecdhe_rsa;
       KexAlg == ecdh_anon ->
    assert_curve(ECCCurve),
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
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys}};
key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = psk},
		    ssl_options = #{psk_identity := undefined}} = State, _) ->
    State;
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{psk_identity := PskIdentityHint},
		    handshake_env = #handshake_env{kex_algorithm = psk,
                                                   hashsign_algorithm = HashSignAlgo},     
                    connection_env = #connection_env{negotiated_version = Version},
                    session = #session{private_key = PrivateKey},
                    connection_states = ConnectionStates0} = State0, Connection) ->
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
key_exchange(#state{static_env = #static_env{role = server},                    
		    ssl_options = #{psk_identity := PskIdentityHint},
		    handshake_env = #handshake_env{kex_algorithm = dhe_psk,
                                                   diffie_hellman_params = #'DHParameter'{} = Params,
                                                   hashsign_algorithm = HashSignAlgo},                                        
                    connection_env = #connection_env{negotiated_version = Version},
                    session = #session{private_key = PrivateKey},
		    connection_states = ConnectionStates0
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
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = DHKeys}};
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{psk_identity := PskIdentityHint},
                    handshake_env = #handshake_env{kex_algorithm = ecdhe_psk,
                                                   hashsign_algorithm = HashSignAlgo},
                    connection_env = #connection_env{negotiated_version = Version},
                    session = #session{ecc = ECCCurve,  private_key = PrivateKey},
		    connection_states = ConnectionStates0
		   } = State0, Connection) ->
    assert_curve(ECCCurve),
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
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys}};
key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = rsa_psk},
		    ssl_options = #{psk_identity := undefined}} = State, _) ->
    State;
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{psk_identity := PskIdentityHint},
                    handshake_env = #handshake_env{kex_algorithm = rsa_psk,
                                                   hashsign_algorithm = HashSignAlgo}, 
                    connection_env = #connection_env{negotiated_version = Version},
                    session = #session{private_key = PrivateKey},
		    connection_states = ConnectionStates0
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
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{user_lookup_fun := LookupFun},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   hashsign_algorithm = HashSignAlgo}, 
                    connection_env = #connection_env{negotiated_version = Version},
		    session = #session{srp_username = Username, private_key = PrivateKey},
		    connection_states = ConnectionStates0
		   } = State0, Connection)
  when KexAlg == srp_dss;
       KexAlg == srp_rsa;
       KexAlg == srp_anon ->
    SrpParams = handle_srp_identity(Username, LookupFun),
    Keys = generate_srp_server_keys(SrpParams, 0),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version),
				      {srp, Keys, SrpParams,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{srp_params = SrpParams,
                                                    kex_keys = Keys}};
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = rsa,
                                                   public_key_info = PublicKeyInfo,
                                                   premaster_secret = PremasterSecret},
                    connection_env = #connection_env{negotiated_version = Version}
		   } = State0, Connection) ->
    Msg = rsa_key_exchange(ssl:tls_version(Version), PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   kex_keys = {DhPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version}
                   } = State0, Connection)
  when KexAlg == dhe_dss;
       KexAlg == dhe_rsa;
       KexAlg == dh_anon ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), {dh, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   kex_keys = #'ECPrivateKey'{parameters = ECCurve} = Key},
                    connection_env = #connection_env{negotiated_version = Version},
                    session = Session
		   } = State0, Connection)
  when KexAlg == ecdhe_ecdsa; 
       KexAlg == ecdhe_rsa;
       KexAlg == ecdh_ecdsa; 
       KexAlg == ecdh_rsa;
       KexAlg == ecdh_anon ->
    Msg = ssl_handshake:key_exchange(client, ssl:tls_version(Version), {ecdh, Key}),
    Connection:queue_handshake(Msg, State0#state{session = Session#session{ecc = ECCurve}});
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = psk},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), 
				      {psk, PSKIdentity}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = dhe_psk,
                                                   kex_keys = {DhPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {dhe_psk, 
				       PSKIdentity, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = ecdhe_psk,
                                                   kex_keys = ECDHKeys},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {ecdhe_psk,
				       PSKIdentity, ECDHKeys}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = rsa_psk,
                                                   public_key_info = PublicKeyInfo,
                                                   premaster_secret = PremasterSecret},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}}
	     = State0, Connection) ->
    Msg = rsa_psk_key_exchange(ssl:tls_version(Version), PSKIdentity,
			       PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   kex_keys = {ClientPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version}}
	     = State0, Connection)
  when KexAlg == srp_dss;
       KexAlg == srp_rsa;
       KexAlg == srp_anon ->
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
    throw(?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, pub_key_is_not_rsa)).

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
    throw(?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, pub_key_is_not_rsa)).

request_client_cert(#state{handshake_env = #handshake_env{kex_algorithm = Alg}} = State, _)
  when Alg == dh_anon; 
       Alg == ecdh_anon;
       Alg == psk; 
       Alg == dhe_psk; 
       Alg == ecdhe_psk; 
       Alg == rsa_psk;
       Alg == srp_dss; 
       Alg == srp_rsa; 
       Alg == srp_anon ->
    State;

request_client_cert(#state{static_env = #static_env{cert_db = CertDbHandle,
                                                    cert_db_ref = CertDbRef},
                           connection_env = #connection_env{negotiated_version = Version},
                           ssl_options = #{verify := verify_peer} = Opts} = State0, Connection) ->
    SupportedHashSigns = maps:get(signature_algs, Opts, undefined),
    TLSVersion =  ssl:tls_version(Version),
    HashSigns = ssl_handshake:available_signature_algs(SupportedHashSigns, 
						       TLSVersion),
    Msg = ssl_handshake:certificate_request(CertDbHandle, CertDbRef, 
					    HashSigns, TLSVersion),
    State = Connection:queue_handshake(Msg, State0),
    State#state{client_certificate_status = requested};

request_client_cert(#state{ssl_options = #{verify := verify_none}} =
		    State, _) ->
    State.

calculate_master_secret(PremasterSecret, 
			#state{connection_env = #connection_env{negotiated_version = Version},
			       connection_states = ConnectionStates0,
			       session = Session0} = State0, Connection,
			_Current, Next) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, server) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates,
				  session = Session},
	    Connection:next_event(Next, no_record, State);
	#alert{} = Alert ->
            throw(Alert)
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
    Finished = ssl_handshake:finished(ssl:tls_version(Version), Role,
				       get_current_prf(ConnectionStates0, write),
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

calculate_secret(#server_dh_params{dh_p = Prime, dh_g = Base, 
				   dh_y = ServerPublicDhKey} = Params,
		 #state{handshake_env = HsEnv} = State, Connection) ->
    Keys = {_, PrivateDhKey} = crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret =
	ssl_handshake:premaster_secret(ServerPublicDhKey, PrivateDhKey, Params),
    calculate_master_secret(PremasterSecret,
			    State#state{handshake_env = HsEnv#handshake_env{kex_keys = Keys}}, 
			    Connection, certify, certify);

calculate_secret(#server_ecdh_params{curve = ECCurve, public = ECServerPubKey},
		     #state{handshake_env = HsEnv,
                            session = Session} = State, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),
    PremasterSecret = 
	ssl_handshake:premaster_secret(#'ECPoint'{point = ECServerPubKey}, ECDHKeys),
    calculate_master_secret(PremasterSecret,
			    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys},
					session = Session#session{ecc = ECCurve}},
			    Connection, certify, certify);

calculate_secret(#server_psk_params{
		    hint = IdentityHint},
		 #state{handshake_env = HsEnv} = State, Connection) ->
    %% store for later use
    Connection:next_event(certify, no_record, 
                          State#state{handshake_env = 
                                          HsEnv#handshake_env{server_psk_identity = IdentityHint}});

calculate_secret(#server_dhe_psk_params{
		    dh_params = #server_dh_params{dh_p = Prime, dh_g = Base}} = ServerKey,
		    #state{handshake_env = HsEnv,
                           ssl_options = #{user_lookup_fun := PSKLookup}} =
		     State, Connection) ->
    Keys = {_, PrivateDhKey} =
	crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, PrivateDhKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State#state{handshake_env = HsEnv#handshake_env{kex_keys = Keys}},
			    Connection, certify, certify);

calculate_secret(#server_ecdhe_psk_params{
                    dh_params = #server_ecdh_params{curve = ECCurve}} = ServerKey,
                 #state{ssl_options = #{user_lookup_fun := PSKLookup}} =
		     #state{handshake_env = HsEnv,
                            session = Session} = State, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),

    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, ECDHKeys, PSKLookup),
    calculate_master_secret(PremasterSecret,
			    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys},
					session = Session#session{ecc = ECCurve}},
			    Connection, certify, certify);

calculate_secret(#server_srp_params{srp_n = Prime, srp_g = Generator} = ServerKey,
		 #state{handshake_env = HsEnv,
                        ssl_options = #{srp_identity := SRPId}} = State,
		 Connection) ->
    Keys = generate_srp_client_keys(Generator, Prime, 0),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, Keys, SRPId),
    calculate_master_secret(PremasterSecret, State#state{handshake_env = HsEnv#handshake_env{kex_keys = Keys}}, Connection, 
			    certify, certify).

master_secret(#alert{} = Alert, _) ->
    throw(Alert);
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

generate_srp_server_keys(_SrpParams, 10) ->
    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));
generate_srp_server_keys(SrpParams =
			     #srp_user{generator = Generator, prime = Prime,
				       verifier = Verifier}, N) ->
    try crypto:generate_key(srp, {host, [Verifier, Generator, Prime, '6a']})
    catch
	error:Reason:ST ->
            ?SSL_LOG(debug, crypto_error, [{error, Reason}, {stacktrace, ST}]),
	    generate_srp_server_keys(SrpParams, N+1)
    end.

generate_srp_client_keys(_Generator, _Prime, 10) ->
    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));
generate_srp_client_keys(Generator, Prime, N) ->
    try crypto:generate_key(srp, {user, [Generator, Prime, '6a']})
    catch
	error:Reason:ST ->
            ?SSL_LOG(debug, crypto_error, [{error, Reason}, {stacktrace, ST}]),
	    generate_srp_client_keys(Generator, Prime, N+1)
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


cipher_role(client, Data, Session, #state{static_env = #static_env{protocol_cb = Connection},
                                          connection_states = ConnectionStates0} = State0) ->
    ConnectionStates = ssl_record:set_server_verify_data(current_both, Data, 
							 ConnectionStates0),
    {Record, State} = ssl_gen_statem:prepare_connection(State0#state{session = Session,
                                                                         connection_states = ConnectionStates},
                                                            Connection),
    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close}]);
cipher_role(server, Data, Session,  #state{static_env = #static_env{protocol_cb = Connection},
                                       connection_states = ConnectionStates0} = State0) ->
    ConnectionStates1 = ssl_record:set_client_verify_data(current_read, Data, 
							  ConnectionStates0),
    {State1, Actions} =
	finalize_handshake(State0#state{connection_states = ConnectionStates1,
					session = Session}, cipher, Connection),
    {Record, State} = ssl_gen_statem:prepare_connection(State1, Connection),
    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close} | Actions]).

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



session_handle_params(#server_ecdh_params{curve = ECCurve}, Session) ->
    Session#session{ecc = ECCurve};
session_handle_params(_, Session) ->
    Session.

handle_session(server, #{reuse_sessions := true}, 
               _Host, _Port, Trackers, #session{is_resumable = false} = Session) ->
    Tracker = proplists:get_value(session_id_tracker, Trackers),
    server_register_session(Tracker, Session#session{is_resumable = true});
handle_session(Role = client, #{verify := verify_peer,
                                reuse_sessions := Reuse} = SslOpts,
               Host, Port, _, #session{is_resumable = false} = Session) when Reuse =/= false ->
    client_register_session(host_id(Role, Host, SslOpts), Port, Session#session{is_resumable = true}, 
                            reg_type(Reuse));
handle_session(_,_,_,_,_, Session) ->
    Session.

reg_type(save) ->
    true;
reg_type(true) ->
    unique.

client_register_session(Host, Port, Session, Save) ->
    ssl_manager:register_session(Host, Port, Session, Save),
    Session.
server_register_session(Tracker, Session) ->
    ssl_server_session_cache:register_session(Tracker, Session),
    Session.

host_id(client, _Host, #{server_name_indication := Hostname}) when is_list(Hostname) ->
    Hostname;
host_id(_, Host, _) ->
    Host.

handle_new_session(NewId, CipherSuite, Compression, 
		   #state{static_env = #static_env{protocol_cb = Connection},
                          session = Session0
			 } = State0) ->
    Session = Session0#session{session_id = NewId,
			       cipher_suite = CipherSuite,
			       compression_method = Compression},
    Connection:next_event(certify, no_record, State0#state{session = Session}).

handle_resumed_session(SessId, #state{static_env = #static_env{host = Host,
                                                               port = Port,
                                                               protocol_cb = Connection,
                                                               session_cache = Cache,
                                                               session_cache_cb = CacheCb},
                                      connection_env = #connection_env{negotiated_version = Version},
                                      connection_states = ConnectionStates0,
                                      ssl_options = Opts} = State) ->

    Session = case maps:get(reuse_session, Opts, undefined) of
        {SessId,SessionData} when is_binary(SessId), is_binary(SessionData) ->
             binary_to_term(SessionData, [safe]);
        _Else ->
             CacheCb:lookup(Cache, {{Host, Port}, SessId})
    end,

    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, client) of
	{_, ConnectionStates} ->
	    Connection:next_event(abbreviated, no_record, State#state{
                                                            connection_states = ConnectionStates,
                                                            session = Session});
	#alert{} = Alert ->
            throw(Alert)
    end.

make_premaster_secret(Version, rsa) ->
    Rand = ssl_cipher:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    {MajVer,MinVer} = Version,
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

%% Handle SNI extension in pre-TLS 1.3 and DTLS
handle_sni_extension(#state{static_env =
                                #static_env{protocol_cb = Connection}} = State0,
                     Hello) ->
    PossibleSNI = Connection:select_sni_extension(Hello),
    case ssl_gen_statem:handle_sni_extension(PossibleSNI, State0) of
        {ok, State} ->
            State;
        {error, #alert{}=Alert} ->
            throw(Alert)
    end.

ensure_tls(Version) when ?DTLS_1_X(Version) ->
    dtls_v1:corresponding_tls_version(Version);
ensure_tls(Version) -> 
    Version.

ocsp_info(#{ocsp_expect := stapled, ocsp_response := CertStatus} = OcspState,
          #{ocsp_stapling := OcspStapling} = _SslOpts, PeerCert) ->
    #{ocsp_responder_certs := OcspResponderCerts} = OcspStapling,
    #{cert_ext => #{public_key:pkix_subject_id(PeerCert) => [CertStatus]},
      ocsp_responder_certs => OcspResponderCerts,
      ocsp_state => OcspState};
ocsp_info(#{ocsp_expect := no_staple} = OcspState, _, PeerCert) ->
    #{cert_ext => #{public_key:pkix_subject_id(PeerCert) => []},
      ocsp_responder_certs => [],
      ocsp_state => OcspState}.

select_client_cert_key_pair(Session0,_,
                            [#{private_key := NoKey, certs := [[]] = NoCerts}],
                            _,_,_,_) ->
    %% No certificate supplied : empty certificate will be sent
    Session0#session{own_certificates = NoCerts,
                     private_key = NoKey};
select_client_cert_key_pair(Session0, CertRequest, CertKeyPairs, SupportedHashSigns, TLSVersion, CertDbHandle, CertDbRef) ->
    select_client_cert_key_pair(Session0, CertRequest, CertKeyPairs, SupportedHashSigns, TLSVersion, CertDbHandle, CertDbRef, undefined).

select_client_cert_key_pair(Session0,_,[], _, _,_,_, undefined) ->
    %% No certificate compliant with supported algorithms: empty certificate will be sent
    Session0#session{own_certificates = [[]],
                     private_key = #{}};
select_client_cert_key_pair(_,_,[], _, _,_,_,#session{}=Session) ->
    %% No certificate compliant with guide lines send default
    Session;
select_client_cert_key_pair(Session0, #certificate_request{certificate_authorities = CertAuths} = CertRequest,
                            [#{private_key := PrivateKey, certs := [Cert| _] = Certs} | Rest],
                            SupportedHashSigns, TLSVersion, CertDbHandle, CertDbRef, Default) ->
    case ssl_handshake:select_hashsign(CertRequest, Cert, SupportedHashSigns, TLSVersion) of
        #alert{} ->
            select_client_cert_key_pair(Session0, CertRequest, Rest, SupportedHashSigns, TLSVersion, CertDbHandle, CertDbRef, Default);
        SelectedHashSign ->
            case ssl_certificate:handle_cert_auths(Certs, CertAuths, CertDbHandle, CertDbRef) of
                {ok, EncodedChain} ->
                    Session0#session{sign_alg = SelectedHashSign,
                                     own_certificates = EncodedChain,
                                     private_key = PrivateKey
                                    };
                {error, EncodedChain, not_in_auth_domain} ->
                    Session = Session0#session{sign_alg = SelectedHashSign,
                                               own_certificates = EncodedChain,
                                               private_key = PrivateKey
                                              },
                    select_client_cert_key_pair(Session0, CertRequest, Rest, SupportedHashSigns, TLSVersion,
                                                CertDbHandle, CertDbRef, default_cert_key_pair_return(Default, Session))
            end
    end.

default_cert_key_pair_return(undefined, Session) ->
    Session;
default_cert_key_pair_return(Default, _) ->
    Default.

assert_curve(ECCCurve) ->
    case ECCCurve of
        no_curve ->
            throw(?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_elliptic_curve));
        _ ->
            ok
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(csp,
             {call, {?MODULE, wait_ocsp_stapling, [Type, Msg | _]}}, Stack) ->
    {io_lib:format("Type = ~w Msg = ~W", [Type, Msg, 10]), Stack}.
