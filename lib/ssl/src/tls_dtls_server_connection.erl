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
%%----------------------------------------------------------------------
%% Purpose: Common handling of a TLS/SSL/DTLS connection, see also
%% tls_connection.erl and dtls_connection.erl
%%
%% NOTE: All alerts are thrown out of this module
%%----------------------------------------------------------------------

-module(tls_dtls_server_connection).
-moduledoc false.

-include_lib("public_key/include/public_key.hrl").

-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").

%% General state handlingfor TLS-1.0 to TLS-1.2
-export([initial_hello/3,
         hello/3,
         user_hello/3,
         abbreviated/3,
         certify/3,
         wait_cert_verify/3,
         cipher/3,
         connection/3,
         downgrade/3]).

%% Help functions for tls|dtls*_connection.erl
-export([handle_sni_extension/2]).

%%====================================================================
%% gen_statem general state functions with connection cb argument
%%====================================================================

%%--------------------------------------------------------------------
-spec initial_hello(gen_statem:event_type(),
                    {start, timeout()} |  {start, {list(), list()}, timeout()}| term(),
                    #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
initial_hello({call, _From} = Type, {new_user, _} = Msg, State) ->
    ssl_gen_statem:initial_hello(Type, Msg, State);
initial_hello({call, From}, _Msg, _State) ->
    {keep_state_and_data, [{reply, From, {error, notsup_on_transport_accept_socket}}]};
initial_hello(Type, Msg, State) ->
    ssl_gen_statem:initial_hello(Type, Msg, State).

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(), term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, {common_client_hello, Type, ServerHelloExt}, State) ->
    do_server_hello(Type, ServerHelloExt, State);
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
		  #finished{} | #next_protocol{} | term(),
		  #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{static_env = #static_env{protocol_cb = Connection},
                   handshake_env = #handshake_env{tls_handshake_history = Hist,
                                                  expecting_finished = true} = HsEnv,
		   connection_env = #connection_env{negotiated_version = Version},
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} =
		State0) ->
    #{security_parameters := SecParams} =
        ssl_record:current_connection_state(ConnectionStates0, write),
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished, client,
                                         SecParams#security_parameters.prf_algorithm,
					 MasterSecret, Hist) of
        verified ->
	    ConnectionStates =
		ssl_record:set_client_verify_data(current_both, Data, ConnectionStates0),
	    {Record, State} =
                ssl_gen_statem:prepare_connection(
                  State0#state{connection_states = ConnectionStates,
                               handshake_env = HsEnv#handshake_env{expecting_finished = false}},
                  Connection),
	    Connection:next_event(connection, Record, State,
                                  [{{timeout, handshake}, infinity, close}]);
	#alert{} = Alert ->
            throw(Alert)
    end;
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
abbreviated(internal, #next_protocol{selected_protocol = SelectedProtocol},
	    #state{static_env = #static_env{protocol_cb = Connection},
                   handshake_env =
                       #handshake_env{expecting_next_protocol_negotiation = true} = HsEnv}
            = State) ->
    NewHSEnv = HsEnv#handshake_env{negotiated_protocol = SelectedProtocol,
                                   expecting_next_protocol_negotiation = false},
    Connection:next_event(?STATE(abbreviated), no_record,
			  State#state{handshake_env = NewHSEnv});
abbreviated(Type, Event, State) ->
    tls_dtls_gen_connection:abbreviated(Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(),
	      #hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(internal, #certificate{asn1_certificates = []},
	#state{ssl_options = #{verify := verify_peer, fail_if_no_peer_cert := true}}) ->
    throw(?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, no_client_certificate_provided));
certify(internal, #certificate{asn1_certificates = []},
	#state{static_env = #static_env{role = server,
                                        protocol_cb = Connection},
               handshake_env = HsEnv0,
	       ssl_options = #{verify := verify_peer,
                               fail_if_no_peer_cert := false}} =
	State0) ->
    HsEnv = HsEnv0#handshake_env{client_certificate_status = empty},
    Connection:next_event(?STATE(certify), no_record, State0#state{handshake_env = HsEnv});
certify(internal, #certificate{},
	#state{ssl_options = #{verify := verify_none}}) ->
    throw(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, unrequested_certificate));
certify(internal, #certificate{asn1_certificates = DerCerts},
        #state{static_env = #static_env{
                               role = Role,
                               host = Host,
                               protocol_cb = Connection,
                               cert_db = CertDbHandle,
                               cert_db_ref = CertDbRef,
                               crl_db = CRLDbInfo},
               handshake_env = #handshake_env{stapling_state = StaplingState} = HsEnv0,
               connection_env = #connection_env{
                                   negotiated_version = Version},
               ssl_options = Opts} = State0) ->
    %% Dummy ext info
    Certs = try [#cert{der=DerCert, otp=public_key:pkix_decode_cert(DerCert, otp)}
                 || DerCert <- DerCerts]
            catch
                error:{_,{error, {asn1, Asn1Reason}}}=Reason:ST ->
                    %% ASN-1 decode of certificate somehow failed
                    ?SSL_LOG(info, asn1_decode, [{error, Reason}, {stacktrace, ST}]),
                    throw(?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN,
                                     {failed_to_decode_certificate, Asn1Reason}))
            end,
    ExtInfo = ext_info(StaplingState, Opts, hd(Certs)),
    case ssl_handshake:certify(Certs, CertDbHandle, CertDbRef,
                               Opts, CRLDbInfo, Role, Host,
                               ssl:tls_version(Version),
                               ExtInfo) of
        {PeerCert, PublicKeyInfo} ->
            HsEnv = HsEnv0#handshake_env{client_certificate_status = needs_verifying},
            State = State0#state{handshake_env = HsEnv},
            tls_dtls_gen_connection:handle_peer_cert(Role, PeerCert, PublicKeyInfo, State,
                                                     Connection, []);
        #alert{} = Alert ->
            throw(Alert)
    end;
certify(internal = Type, #client_key_exchange{} = Msg,
	#state{handshake_env = #handshake_env{client_certificate_status = requested},
               ssl_options = #{fail_if_no_peer_cert := true}}) ->
    %% We expect a certificate here
    throw(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, {unexpected_msg, {Type, Msg}}));
certify(internal, #client_key_exchange{exchange_keys = Keys},
	State = #state{handshake_env = #handshake_env{kex_algorithm = KeyAlg},
                       static_env = #static_env{protocol_cb = Connection},
                       connection_env = #connection_env{negotiated_version = Version}}) ->
    try
	certify_client_key_exchange(ssl_handshake:decode_client_key(Keys, KeyAlg,
                                                                    ssl:tls_version(Version)),
				    State, Connection)
    catch
	#alert{} = Alert ->
            throw(Alert)
    end;
certify(Type, Event, State) ->
    tls_dtls_gen_connection:certify(Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_cert_verify(gen_statem:event_type(),
	      #hello_request{} | #certificate_verify{} | term(),
	     #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
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
wait_cert_verify({call, From}, Msg, State) ->
    ssl_gen_statem:handle_call(Msg, From, ?STATE(wait_cert_verify), State);
wait_cert_verify(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(wait_cert_verify), State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(),
	     #hello_request{} | #finished{} | term(),
	     #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
cipher(internal, #next_protocol{selected_protocol = SelectedProtocol},
       #state{static_env = #static_env{protocol_cb = Connection},
              handshake_env = #handshake_env{expecting_finished = true,
                                             expecting_next_protocol_negotiation = true} = HsEnv}
       = State) ->
    NewHSEnv =  HsEnv#handshake_env{negotiated_protocol = SelectedProtocol,
                                    expecting_next_protocol_negotiation = false},
    Connection:next_event(?STATE(cipher), no_record,
			  State#state{handshake_env = NewHSEnv});
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
    #{security_parameters := SecParams} =
        ssl_record:current_connection_state(ConnectionStates0, read),
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished,
					 ssl_gen_statem:opposite_role(Role),
					 SecParams#security_parameters.prf_algorithm,
					 MasterSecret, Hist) of
        verified ->
	    Session = maybe_register_session(SslOpts, Host, Port, Trackers, Session0),
            ConnectionStates1 = ssl_record:set_client_verify_data(current_read, Data,
                                                                  ConnectionStates0),
            {State1, Actions} =
                tls_dtls_gen_connection:finalize_handshake(State0#state{connection_states =
                                                                            ConnectionStates1,
                                                                        session = Session},
                                                           cipher, Connection),
            {Record, State} = ssl_gen_statem:prepare_connection(State1, Connection),
            Connection:next_event(connection, Record, State,
                                  [{{timeout, handshake}, infinity, close} | Actions]);
        #alert{} = Alert ->
            throw(Alert)
    end;
cipher(Type, Event, State) ->
    tls_dtls_gen_connection:cipher(Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(), term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(cast, {internal_renegotiate, WriteState},
           #state{static_env = #static_env{protocol_cb = Connection},
                  handshake_env = HsEnv,
                  connection_states = ConnectionStates}
           = State0) ->
    State = State0#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}},
                         connection_states = ConnectionStates#{current_write => WriteState}},
    Connection:renegotiate(State, []);
connection(Type, Event, State) ->
    tls_dtls_gen_connection:connection(Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
    ssl_gen_statem:handle_common_event(Type, Event, ?STATE(downgrade), State).


%%====================================================================
%% Help functions for tls|dtls_server_connection.erl
%%====================================================================

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
			 State1#state{handshake_env =
                                          HsEnv#handshake_env{expecting_next_protocol_negotiation =
                                                                  NextProtocols =/= undefined}},
                         Connection),
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
                               session_id = SessionId},
                 #state{session = Session0,
                        static_env = #static_env{protocol_cb = Connection}} = State0, Connection) ->
    #state{} = State1 = server_certify_and_key_exchange(State0, Connection),
    {State, Actions} = server_hello_done(State1, Connection),
    Session = Session0#session{session_id = SessionId,
                               cipher_suite = CipherSuite},
    Connection:next_event(certify, no_record, State#state{session = Session}, Actions).

resumed_server_hello(#state{session = Session,
			    connection_states = ConnectionStates0,
                            static_env = #static_env{protocol_cb = Connection},
			    connection_env = #connection_env{negotiated_version = Version}} =
                         State0, Connection) ->

    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, server) of
	{_, ConnectionStates1} ->
	    State1 = State0#state{connection_states = ConnectionStates1,
				  session = Session},
	    {State, Actions} =
		tls_dtls_gen_connection:finalize_handshake(State1, abbreviated, Connection),
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

server_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_server(State0, Connection),
    State2 = key_exchange(State1, Connection),
    request_client_cert(State2, Connection).

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

key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = rsa}} = State,_) ->
    State;
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   diffie_hellman_params =
                                                       #'DHParameter'{} = Params,
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
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg} = HsEnv,
                    session = #session{private_key = #'ECPrivateKey'{parameters = ECCurve} = Key}
                    = Session} = State, _)
  when KexAlg == ecdh_ecdsa;
       KexAlg == ecdh_rsa ->
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = Key},
                session = Session#session{ecc = ECCurve}};
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg,
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
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = psk},
		    ssl_options = #{psk_identity := undefined}} = State, _) ->
    State;
key_exchange(#state{ssl_options = #{psk_identity := PskIdentityHint},
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
key_exchange(#state{ssl_options = #{psk_identity := PskIdentityHint},
		    handshake_env = #handshake_env{kex_algorithm = dhe_psk,
                                                   diffie_hellman_params =
                                                       #'DHParameter'{} = Params,
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
key_exchange(#state{ssl_options = #{psk_identity := PskIdentityHint},
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
key_exchange(#state{handshake_env = #handshake_env{kex_algorithm = rsa_psk},
		    ssl_options = #{psk_identity := undefined}} = State, _) ->
    State;
key_exchange(#state{ssl_options = #{psk_identity := PskIdentityHint},
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
key_exchange(#state{ssl_options = #{user_lookup_fun := LookupFun},
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
                                                    kex_keys = Keys}}.

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
    IncludeCertAuths = maps:get(certificate_authorities, Opts, true),
    Msg = ssl_handshake:certificate_request(CertDbHandle, CertDbRef,
					    HashSigns, TLSVersion, IncludeCertAuths),
    #state{handshake_env = HsEnv0} = State = Connection:queue_handshake(Msg, State0),
    HsEnv = HsEnv0#handshake_env{client_certificate_status = requested},
    State#state{handshake_env = HsEnv};

request_client_cert(#state{ssl_options = #{verify := verify_none}} =
		    State, _) ->
    State.

certify_client_key_exchange(#encrypted_premaster_secret{premaster_secret= EncPMS},
			    #state{session = #session{private_key = PrivateKey},
                                   handshake_env = #handshake_env{
                                                      client_hello_version = Version,
                                                      client_certificate_status = CCStatus
                                                     }
                                  } = State, Connection) ->
    {Major, Minor} = Version,
    FakeSecret = tls_dtls_gen_connection:make_premaster_secret(Version, rsa),
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
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State, Connection,
                                                    certify, client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_diffie_hellman_public{dh_public = ClientPublicDhKey},
			    #state{handshake_env =
                                       #handshake_env{diffie_hellman_params =
                                                          #'DHParameter'{} = Params,
                                                      kex_keys = {_, ServerDhPrivateKey},
                                                      client_certificate_status = CCStatus}
				  } = State,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientPublicDhKey,
                                                     ServerDhPrivateKey, Params),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State,
                                                    Connection, certify,
                                                    client_kex_next_state(CCStatus));

certify_client_key_exchange(#client_ec_diffie_hellman_public{dh_public = ClientPublicEcDhPoint},
			    #state{handshake_env =
                                       #handshake_env{kex_keys = ECDHKey,
                                                      client_certificate_status = CCStatus}
                                  } = State, Connection) ->
    PremasterSecret =
        ssl_handshake:premaster_secret(#'ECPoint'{point = ClientPublicEcDhPoint}, ECDHKey),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State,
                                                    Connection, certify,
                                                    client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_psk_identity{} = ClientKey,
			    #state{ssl_options =
				       #{user_lookup_fun := PSKLookup},
                                   handshake_env =
                                       #handshake_env{client_certificate_status = CCStatus}
                                  } = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, PSKLookup),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State0,
                                                    Connection, certify,
                                                    client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_dhe_psk_identity{} = ClientKey,
			    #state{handshake_env =
                                       #handshake_env{diffie_hellman_params =
                                                          #'DHParameter'{} = Params,
                                                      kex_keys = {_, ServerDhPrivateKey},
                                                      client_certificate_status = CCStatus
                                                     },
				   ssl_options =
				       #{user_lookup_fun := PSKLookup}
                                  } = State0,
			    Connection) ->
    PremasterSecret =
	ssl_handshake:premaster_secret(ClientKey, ServerDhPrivateKey, Params, PSKLookup),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State0,
                                                    Connection, certify,
                                                    client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_ecdhe_psk_identity{} = ClientKey,
			    #state{handshake_env =
                                       #handshake_env{kex_keys = ServerEcDhPrivateKey,
                                                      client_certificate_status = CCStatus},
				   ssl_options = #{user_lookup_fun := PSKLookup}
                                  } = State,
			    Connection) ->
    PremasterSecret =
	ssl_handshake:premaster_secret(ClientKey, ServerEcDhPrivateKey, PSKLookup),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State,
                                                    Connection, certify,
                                                    client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_rsa_psk_identity{} = ClientKey,
			    #state{session = #session{private_key = PrivateKey},
				   ssl_options =
				       #{user_lookup_fun := PSKLookup},
                                   handshake_env =
                                       #handshake_env{client_certificate_status = CCStatus}
                                  } = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, PrivateKey, PSKLookup),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State0,
                                                    Connection, certify,
                                                    client_kex_next_state(CCStatus));
certify_client_key_exchange(#client_srp_public{} = ClientKey,
			    #state{handshake_env =
                                       #handshake_env{srp_params = Params,
                                                      kex_keys = Key,
                                                      client_certificate_status = CCStatus}
				  } = State0, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, Params),
    tls_dtls_gen_connection:calculate_master_secret(PremasterSecret, State0,
                                                    Connection, certify,
                                                    client_kex_next_state(CCStatus)).

client_kex_next_state(needs_verifying) ->
    wait_cert_verify;
client_kex_next_state(empty) ->
    cipher;
client_kex_next_state(not_requested) ->
    cipher.

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

maybe_register_session(#{reuse_sessions := true},
                       _Host, _Port, Trackers, #session{is_resumable = false} = Session0) ->
    Tracker = proplists:get_value(session_id_tracker, Trackers),
    Session = Session0#session{is_resumable = true},
    ssl_server_session_cache:register_session(Tracker, Session),
    Session;
maybe_register_session(_,_,_,_, Session) ->
    Session.

ext_info(OcspState, _, #cert{otp=PeerCert}) ->
    #{cert_ext => #{public_key:pkix_subject_id(PeerCert) => []},
      stapling_state => OcspState}.

assert_curve(ECCCurve) ->
    case ECCCurve of
        no_curve ->
            throw(?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_elliptic_curve));
        _ ->
            ok
    end.
