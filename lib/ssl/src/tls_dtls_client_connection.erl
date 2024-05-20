%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2024. All Rights Reserved.
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

-module(tls_dtls_client_connection).
-moduledoc false.

-include_lib("public_key/include/public_key.hrl").

-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").

%% General state handling for TLS-1.0 to TLS-1.2
-export([hello/3,
         user_hello/3,
         abbreviated/3,
         certify/3,
         wait_stapling/3,
         cipher/3,
         connection/3,
         downgrade/3]).

%% Help functions for tls_client_connection.erl and dtls_client_connection.erl
-export([handle_session/7]).

%%====================================================================
%% gen_statem general state functions
%%====================================================================

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #server_hello{} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal,  #hello_request{}, _) ->
    keep_state_and_data;
hello(Type, Event, State) ->
    tls_dtls_gen_connection:hello(Type, Event, State).

%--------------------------------------------------------------------
-spec user_hello(gen_statem:event_type(),
                 term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
user_hello(Type, Event, State) ->
    tls_dtls_gen_connection:user_hello(Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(),
		  #finished{} | term(),
		  #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(internal, #hello_request{}, _) ->
    keep_state_and_data;
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{static_env = #static_env{protocol_cb = Connection},
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
		tls_dtls_gen_connection:finalize_handshake(
                  State0#state{connection_states = ConnectionStates1},
                  ?STATE(abbreviated), Connection),
	    {Record, State} =
                ssl_gen_statem:prepare_connection(
                  State1#state{handshake_env = HsEnv#handshake_env{expecting_finished = false}},
                  Connection),
	    Connection:next_event(connection, Record, State,
                                  [{{timeout, handshake}, infinity, close} | Actions]);
	#alert{} = Alert ->
            throw(Alert)
    end;
abbreviated(Type, Event, State) ->
    tls_dtls_gen_connection:abbreviated(Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_stapling(gen_statem:event_type(),
                         #certificate{} |  #certificate_status{} | term(),
	                     #state{}) ->
		gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_stapling(internal, #certificate{},
                   #state{static_env = #static_env{protocol_cb = _Connection}} = State) ->
    %% Postpone message, should be handled in certify after receiving staple message
    {next_state, ?STATE(wait_stapling), State, [{postpone, true}]};
%% Receive OCSP staple message
wait_stapling(internal, #certificate_status{} = CertStatus,
                   #state{static_env = #static_env{protocol_cb = _Connection},
                          handshake_env =
                              #handshake_env{stapling_state = StaplingState} = HsEnv} = State) ->
    {next_state, certify,
     State#state{handshake_env =
                     HsEnv#handshake_env{
                       stapling_state =
                           StaplingState#{status => received_staple,
                                          staple => CertStatus}}}};
%% Server did not send OCSP staple message
wait_stapling(internal, Msg,
                   #state{static_env = #static_env{protocol_cb = _Connection},
                          handshake_env = #handshake_env{
                                             stapling_state = StaplingState} = HsEnv} = State)
  when is_record(Msg, server_key_exchange) orelse
       is_record(Msg, hello_request) orelse
       is_record(Msg, certificate_request) orelse
       is_record(Msg, server_hello_done) orelse
       is_record(Msg, client_key_exchange) ->
    {next_state, certify,
     State#state{handshake_env =
                     HsEnv#handshake_env{stapling_state =
                                             StaplingState#{status => not_received}}},
     [{postpone, true}]};
wait_stapling(internal, #hello_request{}, _) ->
    keep_state_and_data;
wait_stapling(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(wait_stapling), State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(),
	      #hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(internal, #certificate{},
        #state{static_env = #static_env{protocol_cb = Connection},
               handshake_env = #handshake_env{
                                  stapling_state = #{status := negotiated}}} = State) ->
    Connection:next_event(wait_stapling, no_record, State, [{postpone, true}]);
certify(internal, #certificate{asn1_certificates = DerCerts},
        #state{static_env = #static_env{
                               role = Role,
                               host = Host,
                               protocol_cb = Connection,
                               cert_db = CertDbHandle,
                               cert_db_ref = CertDbRef,
                               crl_db = CRLDbInfo},
               handshake_env = #handshake_env{
                                  stapling_state = #{status := StaplingStatus} =
                                      StaplingState},
               connection_env = #connection_env{
                                   negotiated_version = Version},
               ssl_options = Opts} = State)
  when StaplingStatus == not_negotiated; StaplingStatus == received_staple ->
    %% this clause handles also scenario with stapling disabled, so
    %% 'not_negotiated' appears in guard
    Certs = try [#cert{der=DerCert, otp=public_key:pkix_decode_cert(DerCert, otp)}
                 || DerCert <- DerCerts]
            catch
                error:{_,{error, {asn1, Asn1Reason}}}=Reason:ST ->
                    %% ASN-1 decode of certificate somehow failed
                    ?SSL_LOG(info, asn1_decode, [{error, Reason}, {stacktrace, ST}]),
                    throw(?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN, {failed_to_decode_certificate, Asn1Reason}))
            end,
    ExtInfo = ext_info(StaplingState, hd(Certs)),
    case ssl_handshake:certify(Certs, CertDbHandle, CertDbRef,
                               Opts, CRLDbInfo, Role, Host,
                               ensure_tls(Version), ExtInfo) of
        {PeerCert, PublicKeyInfo} ->
            handle_peer_cert(Role, PeerCert, PublicKeyInfo, State, Connection, []);
        #alert{} = Alert ->
            throw(Alert)
    end;
certify(internal, #server_key_exchange{exchange_keys = Keys},
        #state{static_env = #static_env{protocol_cb = Connection},
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
    HashSign = tls_dtls_gen_connection:negotiated_hashsign(Params#server_key_params.hashsign,
                                                           KexAlg, PubKeyInfo,
                                                           ssl:tls_version(Version)),

    case tls_dtls_gen_connection:is_anonymous(KexAlg) of
	true ->
	    calculate_secret(Params#server_key_params.params,
			     State#state{handshake_env =
                                             HsEnv#handshake_env{hashsign_algorithm = HashSign}},
                             Connection);
	false ->
	    case ssl_handshake:verify_server_key(Params, HashSign,
						  ConnectionStates, ssl:tls_version(Version),
                                                 PubKeyInfo) of
		true ->
		    calculate_secret(Params#server_key_params.params,
				     State#state{handshake_env =
                                                     HsEnv#handshake_env{hashsign_algorithm
                                                                         = HashSign},
                                                 session =
                                                     session_handle_params(
                                                       Params#server_key_params.params, Session)},
                                     Connection);
		false ->
                    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
	    end
    end;
certify(internal, #certificate_request{},
	#state{handshake_env = #handshake_env{kex_algorithm = KexAlg}})
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
	#state{static_env = #static_env{protocol_cb = Connection},
               session = Session0,
               handshake_env = HsEnv0,
               connection_env = #connection_env{cert_key_alts = [#{certs := [[]]}]}} = State) ->
    %% The client does not have a certificate and will send an empty reply, the server may fail
    %% or accept the connection by its own preference. No signature algorithms needed as there is
    %% no certificate to verify.
    HsEnv = HsEnv0#handshake_env{client_certificate_status = requested},
    Connection:next_event(?STATE(certify), no_record,
                          State#state{handshake_env = HsEnv,
                                      session = Session0#session{own_certificates = [[]],
                                                                 private_key = #{}}});
certify(internal, #certificate_request{} = CertRequest,
	#state{static_env = #static_env{protocol_cb = Connection,
                                        cert_db = CertDbHandle,
                                        cert_db_ref = CertDbRef},
               connection_env = #connection_env{negotiated_version = Version,
                                                cert_key_alts = CertKeyAlts
                                               },
               handshake_env = HsEnv0,
               session = Session0,
               ssl_options = #{signature_algs := SupportedHashSigns}} = State) ->
    TLSVersion = ssl:tls_version(Version),
    CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts, ssl:tls_version(Version)),
    Session = select_client_cert_key_pair(Session0, CertRequest, CertKeyPairs,
                                          SupportedHashSigns, TLSVersion,
                                          CertDbHandle, CertDbRef),
    HsEnv = HsEnv0#handshake_env{client_certificate_status = requested},
    Connection:next_event(?STATE(certify), no_record,
                          State#state{handshake_env = HsEnv, session = Session});
%% PSK and RSA_PSK might bypass the Server-Key-Exchange
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{protocol_cb = Connection},
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
	    State = tls_dtls_gen_connection:calculate_master_secret(PremasterSecret,
				  State0#state{handshake_env =
                                                   HsEnv#handshake_env{premaster_secret = PremasterSecret}}),
            client_certify_and_key_exchange(State, Connection)
    end;
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{protocol_cb = Connection},
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
            State1 = State0#state{handshake_env =
                                      HsEnv#handshake_env{premaster_secret
                                                          = RSAPremasterSecret}},
            State =
                tls_dtls_gen_connection:calculate_master_secret(PremasterSecret,
                                                                State1),
	    client_certify_and_key_exchange(State, Connection)
    end;
%% Master secret was determined with help of server-key exchange msg
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{protocol_cb = Connection},
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
	#state{static_env = #static_env{protocol_cb = Connection},
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
certify(internal, #hello_request{}, _) ->
    keep_state_and_data;
certify(Type, Event, State) ->
    tls_dtls_gen_connection:certify(Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(),
	     #hello_request{} | #finished{} | term(),
	     #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(internal, #hello_request{}, _) ->
    keep_state_and_data;
cipher(internal, #finished{verify_data = Data} = Finished,
       #state{static_env = #static_env{protocol_cb = Connection,
                                       role = Role,
                                       host = Host,
                                       port = Port,
                                       trackers = Trackers},
              handshake_env = #handshake_env{tls_handshake_history = Hist,
                                             expecting_finished = true},
              connection_env = #connection_env{negotiated_version = Version},
	      session = #session{master_secret = MasterSecret}
	      = Session0,
              ssl_options = SslOpts,
	      connection_states = ConnectionStates0} = State0) ->
    #{security_parameters := SecParams} = ssl_record:current_connection_state(ConnectionStates0, read),
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished,
					 ssl_gen_statem:opposite_role(Role),
                                         SecParams#security_parameters.prf_algorithm,
					 MasterSecret, Hist) of
        verified ->
	    Session = maybe_register_session(SslOpts, Host, Port, Trackers, Session0),
            ConnectionStates = ssl_record:set_server_verify_data(current_both, Data,
                                                                 ConnectionStates0),
            {Record, State} =
                ssl_gen_statem:prepare_connection(State0#state{session = Session,
                                                               connection_states = ConnectionStates},
                                                                Connection),
            Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close}]);
        #alert{} = Alert ->
            throw(Alert)
    end;
cipher(Type, Event, State) ->
    tls_dtls_gen_connection:cipher(Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(), term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection({call, From}, renegotiate, #state{static_env = #static_env{protocol_cb = Connection},
                                             handshake_env = HsEnv} = State0) ->
    State = Connection:reinit_handshake_data(State0#state{handshake_env =
                                                              HsEnv#handshake_env{renegotiation = {true, From}}}),
    %% Handle same way as if server requested the renegotiation
    {next_state, connection, State, [{next_event, internal, #hello_request{}}]};
connection(cast, {internal_renegotiate, WriteState}, #state{static_env = #static_env{protocol_cb = Connection},
                                                            handshake_env = HsEnv,
                                                            connection_states = ConnectionStates}
           = State0) ->
    State1 = State0#state{handshake_env =
                              HsEnv#handshake_env{renegotiation = {true, internal}},
                                                  connection_states =
                              ConnectionStates#{current_write => WriteState}},
    State = Connection:reinit_handshake_data(State1),
    %% Handle same way as if server requested the renegotiation
    {next_state, connection, State, [{next_event, internal, #hello_request{}}]};
connection(internal, {handshake, {#hello_request{} = Handshake, _}},
           #state{handshake_env = HsEnv} = State) ->
    %% Should not be included in handshake history
    {next_state, connection, State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, peer}}},
     [{next_event, internal, Handshake}]};
connection(Type, Event, State) ->
    tls_dtls_gen_connection:connection(Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(downgrade), State).

%%====================================================================
%% Help functions for tls|dtls_client_connection.erl
%%====================================================================

%%--------------------------------------------------------------------
-spec handle_session(#server_hello{}, ssl_record:ssl_version(),
		     binary(), ssl_record:connection_states(), _,_, #state{}) ->
			    gen_statem:state_function_result().
%%--------------------------------------------------------------------
handle_session(#server_hello{cipher_suite = CipherSuite},
	       Version, NewId, ConnectionStates, ProtoExt, Protocol0,
	       #state{session = Session,
		      handshake_env = #handshake_env{negotiated_protocol = CurrentProtocol} = HsEnv,
                      connection_env = #connection_env{negotiated_version = ReqVersion} = CEnv} = State0) ->
    #{key_exchange := KeyAlgorithm} =
	ssl_cipher_format:suite_bin_to_map(CipherSuite),

    PremasterSecret = tls_dtls_gen_connection:make_premaster_secret(ReqVersion, KeyAlgorithm),

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
	    handle_new_session(NewId, CipherSuite, State);
	false ->
	    handle_resumed_session(NewId, State)
    end.


%%====================================================================
%% Internal functions
%%====================================================================
select_client_cert_key_pair(Session0,_,
                            [#{private_key := NoKey, certs := [[]] = NoCerts}],
                            _,_,_,_) ->
    %% No certificate supplied : empty certificate will be sent
    Session0#session{own_certificates = NoCerts,
                     private_key = NoKey};
select_client_cert_key_pair(Session0, CertRequest, CertKeyPairs, SupportedHashSigns,
                            TLSVersion, CertDbHandle, CertDbRef) ->
    select_client_cert_key_pair(Session0, CertRequest, CertKeyPairs, SupportedHashSigns,
                                TLSVersion, CertDbHandle, CertDbRef, undefined).

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
            select_client_cert_key_pair(Session0, CertRequest, Rest, SupportedHashSigns,
                                        TLSVersion, CertDbHandle, CertDbRef, Default);
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
                                                CertDbHandle, CertDbRef,
                                                default_cert_key_pair_return(Default, Session))
            end
    end.

default_cert_key_pair_return(undefined, Session) ->
    Session;
default_cert_key_pair_return(Default, _) ->
    Default.

handle_new_session(NewId, CipherSuite,
		   #state{static_env = #static_env{protocol_cb = Connection},
                          session = Session0
			 } = State0) ->
    Session = Session0#session{session_id = NewId,
			       cipher_suite = CipherSuite},
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

calculate_secret(#server_dh_params{dh_p = Prime, dh_g = Base,
				   dh_y = ServerPublicDhKey} = Params,
		 #state{handshake_env = HsEnv} = State, Connection) ->
    Keys = {_, PrivateDhKey} = crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret =
	ssl_handshake:premaster_secret(ServerPublicDhKey, PrivateDhKey, Params),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret,
			    State#state{handshake_env = HsEnv#handshake_env{kex_keys = Keys}},
			    Connection, certify, certify);
calculate_secret(#server_ecdh_params{curve = ECCurve, public = ECServerPubKey},
		     #state{handshake_env = HsEnv,
                            session = Session} = State, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),
    PremasterSecret =
	ssl_handshake:premaster_secret(#'ECPoint'{point = ECServerPubKey}, ECDHKeys),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret,
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
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret,
                                                    State#state{handshake_env =
                                                                    HsEnv#handshake_env{kex_keys = Keys}},
			    Connection, certify, certify);
calculate_secret(#server_ecdhe_psk_params{
                    dh_params = #server_ecdh_params{curve = ECCurve}} = ServerKey,
                 #state{ssl_options = #{user_lookup_fun := PSKLookup}} =
		     #state{handshake_env = HsEnv,
                            session = Session} = State, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),

    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, ECDHKeys, PSKLookup),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret,
                                                    State#state{handshake_env =
                                                                    HsEnv#handshake_env{kex_keys = ECDHKeys},
                                                                session = Session#session{ecc = ECCurve}},
                                                    Connection, certify, certify);
calculate_secret(#server_srp_params{srp_n = Prime, srp_g = Generator} = ServerKey,
		 #state{handshake_env = HsEnv,
                        ssl_options = #{srp_identity := SRPId}} = State,
		 Connection) ->
    Keys = generate_srp_client_keys(Generator, Prime, 0),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, Keys, SRPId),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret,
                                                    State#state{handshake_env =
                                                                    HsEnv#handshake_env{kex_keys = Keys}},
                                                    Connection, certify, certify).

client_certify_and_key_exchange(State0, Connection) ->
    State1 = do_client_certify_and_key_exchange(State0, Connection),
    {State2, Actions} = tls_dtls_gen_connection:finalize_handshake(State1, certify, Connection),
    #state{handshake_env = HsEnv0} = State2,
    HsEnv = HsEnv0#handshake_env{client_certificate_status = not_requested},
    State = State2#state{handshake_env = HsEnv},     %% Reinitialize
    Connection:next_event(cipher, no_record, State, Actions).

do_client_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_client(State0, Connection),
    State2 = key_exchange(State1, Connection),
    verify_client_cert(State2, Connection).

certify_client(#state{static_env = #static_env{cert_db = CertDbHandle,
                                               cert_db_ref = CertDbRef},
                      handshake_env = #handshake_env{client_certificate_status = requested},
		      session = #session{own_certificates = OwnCerts}}
	       = State, Connection) ->
    Certificate = ssl_handshake:certificate(OwnCerts, CertDbHandle, CertDbRef, client),
    Connection:queue_handshake(Certificate, State);
certify_client(#state{handshake_env = #handshake_env{client_certificate_status = not_requested}
                     } = State, _) ->
    State.

key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = rsa,
                                                   public_key_info = PublicKeyInfo,
                                                   premaster_secret = PremasterSecret},
                    connection_env = #connection_env{negotiated_version = Version}
		   } = State0, Connection) ->
    Msg = rsa_key_exchange(ssl:tls_version(Version), PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   kex_keys = {DhPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version}
                   } = State0, Connection)
  when KexAlg == dhe_dss;
       KexAlg == dhe_rsa;
       KexAlg == dh_anon ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), {dh, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg,
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
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = psk},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {psk, PSKIdentity}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = dhe_psk,
                                                   kex_keys = {DhPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {dhe_psk,
				       PSKIdentity, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = ecdhe_psk,
                                                   kex_keys = ECDHKeys},
                    connection_env = #connection_env{negotiated_version = Version},
                    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {ecdhe_psk,
				       PSKIdentity, ECDHKeys}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = rsa_psk,
                                                   public_key_info = PublicKeyInfo,
                                                   premaster_secret = PremasterSecret},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}}
	     = State0, Connection) ->
    Msg = rsa_psk_key_exchange(ssl:tls_version(Version), PSKIdentity,
			       PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg,
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

generate_srp_client_keys(_Generator, _Prime, 10) ->
    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));
generate_srp_client_keys(Generator, Prime, N) ->
    try crypto:generate_key(srp, {user, [Generator, Prime, '6a']})
    catch
	error:Reason:ST ->
            ?SSL_LOG(debug, crypto_error, [{error, Reason}, {stacktrace, ST}]),
	    generate_srp_client_keys(Generator, Prime, N+1)
    end.

verify_client_cert(#state{handshake_env = #handshake_env{tls_handshake_history = Hist,
                                                         client_certificate_status = requested},
                          connection_env = #connection_env{negotiated_version = Version},
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
verify_client_cert(#state{handshake_env = #handshake_env{client_certificate_status = not_requested}
                         } = State, _) ->
    State.

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
                                    connection_env =
                                        #connection_env{negotiated_version = Version},
                                    session = Session,
                                    connection_states =
                                        ConnectionStates0} = State) when KeyAlg == ecdh_rsa;
                                                                         KeyAlg == ecdh_ecdsa ->
    ECDHKey = public_key:generate_key(PublicKeyParams),
    PremasterSecret = ssl_handshake:premaster_secret(PublicKey, ECDHKey),
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
                                     ConnectionStates0, ?CLIENT_ROLE) of
        {MasterSecret, ConnectionStates} ->
	    State#state{
              handshake_env = HsEnv#handshake_env{kex_keys = ECDHKey},
              session = Session#session{ecc = PublicKeyParams,
                                        master_secret = MasterSecret},
	      connection_states = ConnectionStates};
	#alert{} = Alert ->
	    throw(Alert)
    end;
handle_peer_cert_key(_, _, _, State) ->
    State.

get_pending_prf(CStates, Direction) ->
    #{security_parameters := SecParams} = ssl_record:pending_connection_state(CStates, Direction),
    SecParams#security_parameters.prf_algorithm.

session_handle_params(#server_ecdh_params{curve = ECCurve}, Session) ->
    Session#session{ecc = ECCurve};
session_handle_params(_, Session) ->
    Session.

maybe_register_session(#{verify := verify_peer,
                         reuse_sessions := Reuse} = SslOpts,
                       Host, Port, _, #session{is_resumable = false} = Session0) when Reuse =/= false ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(host_id(Host, SslOpts), Port, Session, reg_type(Reuse)),
    Session;
maybe_register_session(_,_,_,_, Session) ->
    Session.

host_id(_Host, #{server_name_indication := Hostname}) when is_list(Hostname) ->
    Hostname;
host_id(Host, _) ->
    Host.

reg_type(save) ->
    true;
reg_type(true) ->
    unique.

ensure_tls(Version) when ?DTLS_1_X(Version) ->
    dtls_v1:corresponding_tls_version(Version);
ensure_tls(Version) ->
    Version.

ext_info(#{status := received_staple, staple := CertStatus} = StaplingState,
         #cert{otp = PeerCert}) ->
    #{cert_ext => #{public_key:pkix_subject_id(PeerCert) => [CertStatus]},
      stapling_state => StaplingState};
ext_info(#{status := not_negotiated} = StaplingState, #cert{otp = PeerCert}) ->
    #{cert_ext => #{public_key:pkix_subject_id(PeerCert) => []},
      stapling_state => StaplingState}.
