%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% Purpose: Common handling of a TLS/SSL/DTLS connection, see also
%% tls_connection.erl and dtls_connection.erl
%%----------------------------------------------------------------------

-module(ssl_connection).

-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([hello/3, abbreviated/3, certify/3, cipher/3, connection/3]).

%%--------------------------------------------------------------------
-spec hello(start | #hello_request{} | #server_hello{} | term(),
	    #state{}, tls_connection | dtls_connection) ->
		   gen_fsm_state_return().
%%--------------------------------------------------------------------
hello(start, #state{role = server} = State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(hello, hello, Record, State);

hello(#hello_request{}, #state{role = client} = State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(hello, hello, Record, State);

hello({common_client_hello, Type, ServerHelloExt, HashSign},
      #state{session = #session{cipher_suite = CipherSuite},
	     negotiated_version = Version} = State, Connection) ->
    {KeyAlg, _, _, _} = ssl_cipher:suite_definition(CipherSuite),
    NegotiatedHashSign = negotiated_hashsign(HashSign, KeyAlg, Version),
    do_server_hello(Type, ServerHelloExt,
		    State#state{hashsign_algorithm = NegotiatedHashSign}, Connection);

hello(timeout, State, _) ->
    { next_state, hello, State, hibernate };

hello(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, hello, State).

%%--------------------------------------------------------------------
-spec abbreviated(#hello_request{} | #finished{} | term(),
		  #state{}, tls_connection | dtls_connection) ->
			 gen_fsm_state_return().
%%--------------------------------------------------------------------
abbreviated(#hello_request{}, State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(abbreviated, hello, Record, State);

abbreviated(#finished{verify_data = Data} = Finished,
	    #state{role = server,
		   negotiated_version = Version,
		   tls_handshake_history = Handshake,
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} =
		State, Connection) ->
    case ssl_handshake:verify_connection(Version, Finished, client,
					 get_current_prf(ConnectionStates0, write),
					 MasterSecret, Handshake) of
        verified ->
	    ConnectionStates =
		ssl_record:set_client_verify_data(current_both, Data, ConnectionStates0),
	    Connection:next_state_connection(abbreviated,
					     Connection:ack_connection(
					       State#state{connection_states = ConnectionStates}));
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, abbreviated, State)
    end;

abbreviated(#finished{verify_data = Data} = Finished,
	    #state{role = client, tls_handshake_history = Handshake0,
		   session = #session{master_secret = MasterSecret},
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0, Connection) ->
    case ssl_handshake:verify_connection(Version, Finished, server,
					 get_pending_prf(ConnectionStates0, write),
					 MasterSecret, Handshake0) of
        verified ->
	    ConnectionStates1 =
		ssl_record:set_server_verify_data(current_read, Data, ConnectionStates0),
	    State =
		finalize_handshake(State0#state{connection_states = ConnectionStates1},
				   abbreviated, Connection),
	    Connection:next_state_connection(abbreviated,
					     Connection:ack_connection(State));
        #alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, abbreviated, State0)
    end;

%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
abbreviated(#next_protocol{selected_protocol = SelectedProtocol},
	    #state{role = server, expecting_next_protocol_negotiation = true} = State0,
	    Connection) ->
    {Record, State} = Connection:next_record(State0#state{next_protocol = SelectedProtocol}),
    Connection:next_state(abbreviated, abbreviated, Record, State);

abbreviated(timeout, State, _) ->
    {next_state, abbreviated, State, hibernate };

abbreviated(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, abbreviated, State).

%%--------------------------------------------------------------------
-spec certify(#hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}, tls_connection | dtls_connection) ->
		     gen_fsm_state_return().
%%--------------------------------------------------------------------
certify(#hello_request{}, State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(certify, hello, Record, State);

certify(#certificate{asn1_certificates = []},
	#state{role = server, negotiated_version = Version,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = true}} =
	    State, Connection) ->
    Alert =  ?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE),
    Connection:handle_own_alert(Alert, Version, certify, State);

certify(#certificate{asn1_certificates = []},
	#state{role = server,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = false}} =
	State0, Connection) ->
    {Record, State} = Connection:next_record(State0#state{client_certificate_requested = false}),
    Connection:next_state(certify, certify, Record, State);

certify(#certificate{} = Cert,
        #state{negotiated_version = Version,
	       role = Role,
	       cert_db = CertDbHandle,
	       cert_db_ref = CertDbRef,
	       ssl_options = Opts} = State, Connection) ->
    case ssl_handshake:certify(Cert, CertDbHandle, CertDbRef, Opts#ssl_options.depth,
			       Opts#ssl_options.verify,
			       Opts#ssl_options.verify_fun, Role) of
        {PeerCert, PublicKeyInfo} ->
	    handle_peer_cert(Role, PeerCert, PublicKeyInfo,
			     State#state{client_certificate_requested = false}, Connection);
	#alert{} = Alert ->
            Connection:handle_own_alert(Alert, Version, certify, State)
    end;

certify(#server_key_exchange{} = KeyExchangeMsg,
        #state{role = client, negotiated_version = Version,
	       key_algorithm = Alg} = State0, Connection)
  when Alg == dhe_dss; Alg == dhe_rsa;
       Alg == ecdhe_rsa; Alg == ecdhe_ecdsa;
       Alg == dh_anon; Alg == ecdh_anon;
       Alg == psk; Alg == dhe_psk; Alg == rsa_psk;
       Alg == srp_dss; Alg == srp_rsa; Alg == srp_anon ->
    case handle_server_key(KeyExchangeMsg, State0) of
	#state{} = State1 ->
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, certify, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify(#server_key_exchange{} = Msg,
        #state{role = client, key_algorithm = rsa} = State, Connection) ->
    Connection:handle_unexpected_message(Msg, certify_server_keyexchange, State);

certify(#certificate_request{hashsign_algorithms = HashSigns},
	#state{session = #session{own_certificate = Cert}} = State0, Connection) ->
    HashSign = ssl_handshake:select_hashsign(HashSigns, Cert),
    {Record, State} = Connection:next_record(State0#state{client_certificate_requested = true}),
    Connection:next_state(certify, certify, Record,
			  State#state{cert_hashsign_algorithm = HashSign});

%% PSK and RSA_PSK might bypass the Server-Key-Exchange
certify(#server_hello_done{},
	#state{session = #session{master_secret = undefined},
	       negotiated_version = Version,
	       psk_identity = PSKIdentity,
	       premaster_secret = undefined,
	       role = client,
	       key_algorithm = Alg} = State0, Connection)
  when Alg == psk ->
    case server_psk_master_secret(PSKIdentity, State0) of
	#state{} = State ->
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify(#server_hello_done{},
	#state{session = #session{master_secret = undefined},
	       ssl_options = SslOpts,
	       negotiated_version = Version,
	       psk_identity = PSKIdentity,
	       premaster_secret = undefined,
	       role = client,
	       key_algorithm = Alg} = State0, Connection)
  when Alg == rsa_psk ->
    case handle_psk_identity(PSKIdentity, SslOpts#ssl_options.user_lookup_fun) of
	{ok, PSK} when is_binary(PSK) ->
	    PremasterSecret = make_premaster_secret(Version, rsa),
	    Len = byte_size(PSK),
	    RealPMS = <<?UINT16(48), PremasterSecret/binary, ?UINT16(Len), PSK/binary>>,
	    State1 = State0#state{premaster_secret = PremasterSecret},
	    State = master_from_premaster_secret(RealPMS, State1),
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    Alert;
	_ ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
    end;

%% Master secret was determined with help of server-key exchange msg
certify(#server_hello_done{},
	#state{session = #session{master_secret = MasterSecret} = Session,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = undefined,
	       role = client} = State0, Connection) ->
    case ssl_handshake:master_secret(record_cb(Connection), Version, Session,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    State = State0#state{connection_states = ConnectionStates},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

%% Master secret is calculated from premaster_secret
certify(#server_hello_done{},
	#state{session = Session0,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = PremasterSecret,
	       role = client} = State0, Connection) ->
    case ssl_handshake:master_secret(record_cb(Connection), Version, PremasterSecret,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates,
				 session = Session},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify(#client_key_exchange{} = Msg,
	#state{role = server,
	       client_certificate_requested = true,
	       ssl_options = #ssl_options{fail_if_no_peer_cert = true}} = State, Connection) ->
    %% We expect a certificate here
    Connection:handle_unexpected_message(Msg, certify_client_key_exchange, State);

certify(#client_key_exchange{exchange_keys = Keys},
	State = #state{key_algorithm = KeyAlg, negotiated_version = Version}, Connection) ->
    try
	certify_client_key_exchange(ssl_handshake:decode_client_key(Keys, KeyAlg, Version),
				    State, Connection)
    catch
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State)
    end;

certify(timeout, State, _) ->
    {next_state, certify, State, hibernate};

certify(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, certify, State).

%%--------------------------------------------------------------------
-spec cipher(#hello_request{} | #certificate_verify{} | #finished{} | term(),
	     #state{}, tls_connection | dtls_connection) ->
		    gen_fsm_state_return().
%%--------------------------------------------------------------------
cipher(#hello_request{}, State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(cipher, hello, Record, State);

cipher(#certificate_verify{signature = Signature, hashsign_algorithm = CertHashSign},
       #state{role = server,
	      public_key_info = {Algo, _, _} =PublicKeyInfo,
	      negotiated_version = Version,
	      session = #session{master_secret = MasterSecret},
	      tls_handshake_history = Handshake
	     } = State0, Connection) ->

    HashSign = ssl_handshake:select_cert_hashsign(CertHashSign, Algo, Version),
    case ssl_handshake:certificate_verify(Signature, PublicKeyInfo,
					  Version, HashSign, MasterSecret, Handshake) of
	valid ->
	    {Record, State} = Connection:next_record(State0),
	    Connection:next_state(cipher, cipher, Record,
				  State#state{cert_hashsign_algorithm = HashSign});
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, cipher, State0)
    end;

%% client must send a next protocol message if we are expecting it
cipher(#finished{}, #state{role = server, expecting_next_protocol_negotiation = true,
			   next_protocol = undefined, negotiated_version = Version} = State0,
       Connection) ->
    Connection:handle_own_alert(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE), Version, cipher, State0);

cipher(#finished{verify_data = Data} = Finished,
       #state{negotiated_version = Version,
	      host = Host,
	      port = Port,
	      role = Role,
	      session = #session{master_secret = MasterSecret}
	      = Session0,
	      connection_states = ConnectionStates0,
	      tls_handshake_history = Handshake0} = State, Connection) ->
    case ssl_handshake:verify_connection(Version, Finished,
					 opposite_role(Role),
					 get_current_prf(ConnectionStates0, read),
					 MasterSecret, Handshake0) of
        verified ->
	    Session = Connection:register_session(Role, Host, Port, Session0),
	    cipher_role(Role, Data, Session, State, Connection);
        #alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, cipher, State)
    end;

%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
cipher(#next_protocol{selected_protocol = SelectedProtocol},
       #state{role = server, expecting_next_protocol_negotiation = true} = State0, Connection) ->
    {Record, State} = Connection:next_record(State0#state{next_protocol = SelectedProtocol}),
    Connection:next_state(cipher, cipher, Record, State);

cipher(timeout, State, _) ->
    {next_state, cipher, State, hibernate};

cipher(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, cipher, State).

%%--------------------------------------------------------------------
-spec connection(term(), #state{}, tls_connection | dtls_connection) ->
			gen_fsm_state_return().
%%--------------------------------------------------------------------
connection(timeout, State,  _) ->
    {next_state, connection, State, hibernate};

connection(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, connection, State).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_server_hello(Type, #hello_extensions{next_protocol_negotiation = NextProtocols} =
		    ServerHelloExt,
		#state{negotiated_version = Version,
		       session = #session{session_id = SessId},
		       connection_states = ConnectionStates0}
		= State0, Connection) when is_atom(Type) ->

    ServerHello =
	ssl_handshake:server_hello(SessId, Version, ConnectionStates0, ServerHelloExt),
    State = server_hello(ServerHello,
			 State0#state{expecting_next_protocol_negotiation =
					  NextProtocols =/= undefined}, Connection),
    case Type of
	new ->
	    new_server_hello(ServerHello, State, Connection);
	resumed ->
	    resumed_server_hello(State, Connection)
    end.

new_server_hello(#server_hello{cipher_suite = CipherSuite,
			      compression_method = Compression,
			      session_id = SessionId},
		#state{session = Session0,
		       negotiated_version = Version} = State0, Connection) ->
    try server_certify_and_key_exchange(State0, Connection) of
        #state{} = State1 ->
            State2 = server_hello_done(State1, Connection),
	    Session =
		Session0#session{session_id = SessionId,
				 cipher_suite = CipherSuite,
				 compression_method = Compression},
	    {Record, State} = Connection:next_record(State2#state{session = Session}),
	    Connection:next_state(hello, certify, Record, State)
    catch
        #alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, hello, State0)
    end.

resumed_server_hello(#state{session = Session,
			    connection_states = ConnectionStates0,
			    negotiated_version = Version} = State0, Connection) ->

    case ssl_handshake:master_secret(record_cb(Connection), Version, Session,
				     ConnectionStates0, server) of
	{_, ConnectionStates1} ->
	    State1 = State0#state{connection_states = ConnectionStates1,
				  session = Session},
	    State2 =
		finalize_handshake(State1, abbreviated, Connection),
	    {Record, State} = Connection:next_record(State2),
	    Connection:next_state(hello, abbreviated, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, hello, State0)
    end.

server_hello(ServerHello, State0, Connection) ->
    CipherSuite = ServerHello#server_hello.cipher_suite,
    {KeyAlgorithm, _, _, _} = ssl_cipher:suite_definition(CipherSuite),
    State = Connection:send_handshake(ServerHello, State0),
    State#state{key_algorithm = KeyAlgorithm}.

server_hello_done(State, Connection) ->
    HelloDone = ssl_handshake:server_hello_done(),
    Connection:send_handshake(HelloDone, State).

handle_peer_cert(Role, PeerCert, PublicKeyInfo,
		 #state{session = #session{cipher_suite = CipherSuite} = Session} = State0,
		 Connection) ->
    State1 = State0#state{session =
			 Session#session{peer_certificate = PeerCert},
			 public_key_info = PublicKeyInfo},
    {KeyAlg,_,_,_} = ssl_cipher:suite_definition(CipherSuite),
    State2 = handle_peer_cert_key(Role, PeerCert, PublicKeyInfo, KeyAlg, State1),

    {Record, State} = Connection:next_record(State2),
    Connection:next_state(certify, certify, Record, State).

handle_peer_cert_key(client, _,
		     {?'id-ecPublicKey',  #'ECPoint'{point = _ECPoint} = PublicKey,
		      PublicKeyParams},
		     KeyAlg, State)  when KeyAlg == ecdh_rsa;
					  KeyAlg == ecdh_ecdsa ->
    ECDHKey = public_key:generate_key(PublicKeyParams),
    ec_dh_master_secret(ECDHKey, PublicKey, State#state{diffie_hellman_keys = ECDHKey});

%% We do currently not support cipher suites that use fixed DH.
%% If we want to implement that the following clause can be used
%% to extract DH parameters form cert.
%% handle_peer_cert_key(client, _PeerCert, {?dhpublicnumber, PublicKey, PublicKeyParams},
%%                      {_,SignAlg},
%% 		        #state{diffie_hellman_keys = {_, MyPrivatKey}} = State) when
%%                                                                           SignAlg == dh_rsa;
%% 									     SignAlg == dh_dss ->
%%     dh_master_secret(PublicKeyParams, PublicKey, MyPrivatKey, State);
handle_peer_cert_key(_, _, _, _, State) ->
    State.

certify_client(#state{client_certificate_requested = true, role = client,
		      cert_db = CertDbHandle,
                      cert_db_ref = CertDbRef,
		      session = #session{own_certificate = OwnCert}}
	       = State, Connection) ->
    Certificate = ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, client),
    Connection:send_handshake(Certificate, State);

certify_client(#state{client_certificate_requested = false} = State, _) ->
    State.

verify_client_cert(#state{client_certificate_requested = true, role = client,
			  negotiated_version = Version,
			  private_key = PrivateKey,
			  session = #session{master_secret = MasterSecret,
					     own_certificate = OwnCert},
			  cert_hashsign_algorithm = HashSign,
			  tls_handshake_history = Handshake0} = State, Connection) ->

    case ssl_handshake:client_certificate_verify(OwnCert, MasterSecret,
						 Version, HashSign, PrivateKey, Handshake0) of
        #certificate_verify{} = Verified ->
           Connection:send_handshake(Verified, State);
	ignore ->
	    State;
	#alert{} = Alert ->
	    throw(Alert)
    end;
verify_client_cert(#state{client_certificate_requested = false} = State, _) ->
    State.

client_certify_and_key_exchange(#state{negotiated_version = Version} =
				State0, Connection) ->
    try do_client_certify_and_key_exchange(State0, Connection) of
        State1 = #state{} ->
	    State2 = finalize_handshake(State1, certify, Connection),
            State3 = State2#state{
		       %% Reinitialize
		       client_certificate_requested = false},
	    {Record, State} = Connection:next_record(State3),
	    Connection:next_state(certify, cipher, Record, State)
    catch
        throw:#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end.

do_client_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_client(State0, Connection),
    State2 = key_exchange(State1, Connection),
    verify_client_cert(State2, Connection).

server_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_server(State0, Connection),
    State2 = key_exchange(State1, Connection),
    request_client_cert(State2, Connection).

certify_client_key_exchange(#encrypted_premaster_secret{premaster_secret= EncPMS},
			    #state{negotiated_version = Version,
				   connection_states = ConnectionStates0,
				   session = Session0,
				   private_key = Key} = State0, Connection) ->
    PremasterSecret = ssl_handshake:decrypt_premaster_secret(EncPMS, Key),
    case ssl_handshake:master_secret(record_cb(Connection), Version, PremasterSecret,
				     ConnectionStates0, server) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State1 = State0#state{connection_states = ConnectionStates,
				  session = Session},
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, cipher, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify_client_key_exchange(#client_diffie_hellman_public{dh_public = ClientPublicDhKey},
			    #state{negotiated_version = Version,
				   diffie_hellman_params = #'DHParameter'{} = Params,
				   diffie_hellman_keys = {_, ServerDhPrivateKey}} = State0,
			    Connection) ->
    case dh_master_secret(Params, ClientPublicDhKey, ServerDhPrivateKey, State0) of
	#state{} = State1 ->
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, cipher, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify_client_key_exchange(#client_ec_diffie_hellman_public{dh_public = ClientPublicEcDhPoint},
			    #state{negotiated_version = Version,
				   diffie_hellman_keys = ECDHKey} = State0, Connection) ->
    case ec_dh_master_secret(ECDHKey, #'ECPoint'{point = ClientPublicEcDhPoint}, State0) of
	#state{} = State1 ->
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, cipher, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify_client_key_exchange(#client_psk_identity{identity = ClientPSKIdentity},
			    #state{negotiated_version = Version} = State0, Connection) ->
    case server_psk_master_secret(ClientPSKIdentity, State0) of
	#state{} = State1 ->
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, cipher, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify_client_key_exchange(#client_dhe_psk_identity{
			       identity =  ClientPSKIdentity,
			       dh_public = ClientPublicDhKey},
			    #state{negotiated_version = Version,
				   diffie_hellman_params = #'DHParameter'{prime = P,
									  base = G},
				   diffie_hellman_keys = {_, ServerDhPrivateKey}} = State0,
			    Connection) ->
    case dhe_psk_master_secret(ClientPSKIdentity, P, G, ClientPublicDhKey,
			       ServerDhPrivateKey, State0) of
	#state{} = State1 ->
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, cipher, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify_client_key_exchange(#client_rsa_psk_identity{
			       identity = PskIdentity,
			       exchange_keys =
				   #encrypted_premaster_secret{premaster_secret= EncPMS}},
			    #state{negotiated_version = Version,
				   private_key = Key} = State0, Connection) ->
    PremasterSecret = ssl_handshake:decrypt_premaster_secret(EncPMS, Key),
    case server_rsa_psk_master_secret(PskIdentity, PremasterSecret, State0) of
	#state{} = State1 ->
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, cipher, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify_client_key_exchange(#client_srp_public{srp_a = ClientPublicKey},
			    #state{negotiated_version = Version,
				   srp_params =
				       #srp_user{prime = Prime,
						 verifier = Verifier}
				  } = State0, Connection) ->
    case server_srp_master_secret(Verifier, Prime, ClientPublicKey, State0) of
	#state{} = State1 ->
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(certify, cipher, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end.

certify_server(#state{key_algorithm = Algo} = State, _)
  when Algo == dh_anon; Algo == ecdh_anon; Algo == psk; Algo == dhe_psk; Algo == srp_anon  ->
    State;

certify_server(#state{cert_db = CertDbHandle,
		      cert_db_ref = CertDbRef,
		      session = #session{own_certificate = OwnCert}} = State, Connection) ->
    case ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, server) of
	Cert = #certificate{} ->
	    Connection:send_handshake(Cert, State);
	Alert = #alert{} ->
	    throw(Alert)
    end.

key_exchange(#state{role = server, key_algorithm = rsa} = State,_) ->
    State;
key_exchange(#state{role = server, key_algorithm = Algo,
		    hashsign_algorithm = HashSignAlgo,
		    diffie_hellman_params = #'DHParameter'{} = Params,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == dhe_dss;
       Algo == dhe_rsa;
       Algo == dh_anon ->
    DHKeys = public_key:generate_key(Params),
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, Version, {dh, DHKeys, Params,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{diffie_hellman_keys = DHKeys};

key_exchange(#state{role = server, private_key = Key, key_algorithm = Algo} = State, _)
  when Algo == ecdh_ecdsa; Algo == ecdh_rsa ->
    State#state{diffie_hellman_keys = Key};
key_exchange(#state{role = server, key_algorithm = Algo,
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == ecdhe_ecdsa; Algo == ecdhe_rsa;
       Algo == ecdh_anon ->

    ECDHKeys = public_key:generate_key(select_curve(State0)),
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {ecdh, ECDHKeys,
							HashSignAlgo, ClientRandom,
							ServerRandom,
							PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{diffie_hellman_keys = ECDHKeys};

key_exchange(#state{role = server, key_algorithm = psk,
		    ssl_options = #ssl_options{psk_identity = undefined}} = State, _) ->
    State;
key_exchange(#state{role = server, key_algorithm = psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, Version, {psk, PskIdentityHint,
						       HashSignAlgo, ClientRandom,
						       ServerRandom,
						       PrivateKey}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = server, key_algorithm = dhe_psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    diffie_hellman_params = #'DHParameter'{} = Params,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    DHKeys = public_key:generate_key(Params),
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {dhe_psk, PskIdentityHint, DHKeys, Params,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{diffie_hellman_keys = DHKeys};

key_exchange(#state{role = server, key_algorithm = rsa_psk,
		    ssl_options = #ssl_options{psk_identity = undefined}} = State, _) ->
    State;
key_exchange(#state{role = server, key_algorithm = rsa_psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {psk, PskIdentityHint,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = server, key_algorithm = Algo,
		    ssl_options = #ssl_options{user_lookup_fun = LookupFun},
		    hashsign_algorithm = HashSignAlgo,
		    session = #session{srp_username = Username},
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == srp_dss;
       Algo == srp_rsa;
       Algo == srp_anon ->
    SrpParams = handle_srp_identity(Username, LookupFun),
    Keys = case generate_srp_server_keys(SrpParams, 0) of
	       Alert = #alert{} ->
		   throw(Alert);
	       Keys0 = {_,_} ->
		   Keys0
	   end,
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {srp, Keys, SrpParams,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{srp_params = SrpParams,
		srp_keys = Keys};

key_exchange(#state{role = client,
		    key_algorithm = rsa,
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    premaster_secret = PremasterSecret} = State0, Connection) ->
    Msg = rsa_key_exchange(Version, PremasterSecret, PublicKeyInfo),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    diffie_hellman_keys = {DhPubKey, _}
		   } = State0, Connection)
  when Algorithm == dhe_dss;
       Algorithm == dhe_rsa;
       Algorithm == dh_anon ->
    Msg =  ssl_handshake:key_exchange(client, Version, {dh, DhPubKey}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    diffie_hellman_keys = Keys} = State0, Connection)
  when Algorithm == ecdhe_ecdsa; Algorithm == ecdhe_rsa;
       Algorithm == ecdh_ecdsa; Algorithm == ecdh_rsa;
       Algorithm == ecdh_anon ->
    Msg = ssl_handshake:key_exchange(client, Version, {ecdh, Keys}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = psk,
		    negotiated_version = Version} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, Version, {psk, SslOpts#ssl_options.psk_identity}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = dhe_psk,
		    negotiated_version = Version,
		    diffie_hellman_keys = {DhPubKey, _}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, Version,
				      {dhe_psk, SslOpts#ssl_options.psk_identity, DhPubKey}),
    Connection:send_handshake(Msg, State0);
key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = rsa_psk,
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    premaster_secret = PremasterSecret}
	     = State0, Connection) ->
    Msg = rsa_psk_key_exchange(Version, SslOpts#ssl_options.psk_identity,
			       PremasterSecret, PublicKeyInfo),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    srp_keys = {ClientPubKey, _}}
	     = State0, Connection)
  when Algorithm == srp_dss;
       Algorithm == srp_rsa;
       Algorithm == srp_anon ->
    Msg =  ssl_handshake:key_exchange(client, Version, {srp, ClientPubKey}),
    Connection:send_handshake(Msg, State0).

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
    ssl_handshake:key_exchange(client, Version,
			       {premaster_secret, PremasterSecret,
				PublicKeyInfo});
rsa_key_exchange(_, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE)).

rsa_psk_key_exchange(Version, PskIdentity, PremasterSecret, PublicKeyInfo = {Algorithm, _, _})
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption;
       Algorithm == ?sha224WithRSAEncryption;
       Algorithm == ?sha256WithRSAEncryption;
       Algorithm == ?sha384WithRSAEncryption;
       Algorithm == ?sha512WithRSAEncryption
       ->
    ssl_handshake:key_exchange(client, Version,
			       {psk_premaster_secret, PskIdentity, PremasterSecret,
				PublicKeyInfo});
rsa_psk_key_exchange(_, _, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE)).

request_client_cert(#state{ssl_options = #ssl_options{verify = verify_peer},
			   connection_states = ConnectionStates0,
			   cert_db = CertDbHandle,
			   cert_db_ref = CertDbRef,
			   negotiated_version = Version} = State0, Connection) ->
    #connection_state{security_parameters =
			  #security_parameters{cipher_suite = CipherSuite}} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    Msg = ssl_handshake:certificate_request(CipherSuite, CertDbHandle, CertDbRef, Version),
    State = Connection:send_handshake(Msg, State0),
    State#state{client_certificate_requested = true};

request_client_cert(#state{ssl_options = #ssl_options{verify = verify_none}} =
		    State, _) ->
    State.

finalize_handshake(State0, StateName, Connection) ->
    #state{connection_states = ConnectionStates0} =
	State1 = cipher_protocol(State0, Connection),

    ConnectionStates =
        ssl_record:activate_pending_connection_state(ConnectionStates0,
                                                     write),

    State2 = State1#state{connection_states = ConnectionStates},
    State = next_protocol(State2, Connection),
    finished(State, StateName, Connection).

next_protocol(#state{role = server} = State, _) ->
    State;
next_protocol(#state{next_protocol = undefined} = State, _) ->
    State;
next_protocol(#state{expecting_next_protocol_negotiation = false} = State, _) ->
    State;
next_protocol(#state{next_protocol = NextProtocol} = State0, Connection) ->
    NextProtocolMessage = ssl_handshake:next_protocol(NextProtocol),
    Connection:send_handshake(NextProtocolMessage, State0).

cipher_protocol(State, Connection) ->
    Connection:send_change_cipher(#change_cipher_spec{}, State).

finished(#state{role = Role, negotiated_version = Version,
		session = Session,
                connection_states = ConnectionStates0,
                tls_handshake_history = Handshake0} = State0, StateName, Connection) ->
    MasterSecret = Session#session.master_secret,
    Finished = ssl_handshake:finished(Version, Role,
				       get_current_prf(ConnectionStates0, write),
				       MasterSecret, Handshake0),
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

handle_server_key(#server_key_exchange{exchange_keys = Keys},
		  #state{key_algorithm = KeyAlg,
			 negotiated_version = Version} = State) ->

    Params = ssl_handshake:decode_server_key(Keys, KeyAlg, Version),
    HashSign = negotiated_hashsign(Params#server_key_params.hashsign, KeyAlg, Version),
    case is_anonymous(KeyAlg) of
	true ->
	    server_master_secret(Params#server_key_params.params,
				 State#state{hashsign_algorithm = HashSign});
	false ->
	    verify_server_key(Params, HashSign, State#state{hashsign_algorithm = HashSign})
    end.

verify_server_key(#server_key_params{params = Params,
				     params_bin = EncParams,
				     signature = Signature},
		  HashSign = {HashAlgo, _},
		  #state{negotiated_version = Version,
			 public_key_info = PubKeyInfo,
			 connection_states = ConnectionStates} = State) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Hash = ssl_handshake:server_key_exchange_hash(HashAlgo,
						  <<ClientRandom/binary,
						    ServerRandom/binary,
						    EncParams/binary>>),
    case ssl_handshake:verify_signature(Version, Hash, HashSign, Signature, PubKeyInfo) of
	true ->
	    server_master_secret(Params, State);
	false ->
	    ?ALERT_REC(?FATAL, ?DECRYPT_ERROR)
    end.

make_premaster_secret({MajVer, MinVer}, rsa) ->
    Rand = ssl:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>;
make_premaster_secret(_, _) ->
    undefined.

server_master_secret(#server_dh_params{dh_p = P, dh_g = G, dh_y = ServerPublicDhKey},
		     State) ->
    dh_master_secret(P, G, ServerPublicDhKey, undefined, State);

server_master_secret(#server_ecdh_params{curve = ECCurve, public = ECServerPubKey},
		     State) ->
    ECDHKeys = public_key:generate_key(ECCurve),
    ec_dh_master_secret(ECDHKeys, #'ECPoint'{point = ECServerPubKey},
			State#state{diffie_hellman_keys = ECDHKeys});

server_master_secret(#server_psk_params{
			hint = IdentityHint},
		     State) ->
    %% store for later use
    State#state{psk_identity = IdentityHint};

server_master_secret(#server_dhe_psk_params{
			hint = IdentityHint,
			dh_params = #server_dh_params{dh_p = P, dh_g = G, dh_y = ServerPublicDhKey}},
		     State) ->
    dhe_psk_master_secret(IdentityHint, P, G, ServerPublicDhKey, undefined, State);

server_master_secret(#server_srp_params{srp_n = N, srp_g = G, srp_s = S, srp_b = B},
		     State) ->
    client_srp_master_secret(G, N, S, B, undefined, State).

master_from_premaster_secret(PremasterSecret,
			     #state{session = Session,
				    negotiated_version = Version, role = Role,
				    connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:master_secret(tls_record, Version, PremasterSecret,
				     ConnectionStates0, Role) of
	{MasterSecret, ConnectionStates} ->
	    State#state{
	      session =
		  Session#session{master_secret = MasterSecret},
	      connection_states = ConnectionStates};
	#alert{} = Alert ->
	    Alert
    end.

dh_master_secret(#'DHParameter'{} = Params, OtherPublicDhKey, MyPrivateKey, State) ->
    PremasterSecret =
	public_key:compute_key(OtherPublicDhKey, MyPrivateKey, Params),
    master_from_premaster_secret(PremasterSecret, State).

dh_master_secret(Prime, Base, PublicDhKey, undefined, State) ->
    Keys = {_, PrivateDhKey} = crypto:generate_key(dh, [Prime, Base]),
    dh_master_secret(Prime, Base, PublicDhKey, PrivateDhKey, State#state{diffie_hellman_keys = Keys});

dh_master_secret(Prime, Base, PublicDhKey, PrivateDhKey, State) ->
    PremasterSecret =
	crypto:compute_key(dh, PublicDhKey, PrivateDhKey, [Prime, Base]),
    master_from_premaster_secret(PremasterSecret, State).

ec_dh_master_secret(ECDHKeys, ECPoint, State) ->
    PremasterSecret =
	public_key:compute_key(ECPoint, ECDHKeys),
    master_from_premaster_secret(PremasterSecret, State).

handle_psk_identity(_PSKIdentity, LookupFun)
  when LookupFun == undefined ->
    error;
handle_psk_identity(PSKIdentity, {Fun, UserState}) ->
    Fun(psk, PSKIdentity, UserState).

server_psk_master_secret(ClientPSKIdentity,
			 #state{ssl_options = SslOpts} = State) ->
    case handle_psk_identity(ClientPSKIdentity, SslOpts#ssl_options.user_lookup_fun) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = byte_size(PSK),
	    PremasterSecret = <<?UINT16(Len), 0:(Len*8), ?UINT16(Len), PSK/binary>>,
	    master_from_premaster_secret(PremasterSecret, State);
	#alert{} = Alert ->
	    Alert;
	_ ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
    end.

dhe_psk_master_secret(PSKIdentity, Prime, Base, PublicDhKey, undefined, State) ->
    Keys = {_, PrivateDhKey} =
	crypto:generate_key(dh, [Prime, Base]),
    dhe_psk_master_secret(PSKIdentity, Prime, Base, PublicDhKey, PrivateDhKey,
			  State#state{diffie_hellman_keys = Keys});

dhe_psk_master_secret(PSKIdentity, Prime, Base, PublicDhKey, PrivateDhKey,
			     #state{ssl_options = SslOpts} = State) ->
    case handle_psk_identity(PSKIdentity, SslOpts#ssl_options.user_lookup_fun) of
	{ok, PSK} when is_binary(PSK) ->
	    DHSecret =
		crypto:compute_key(dh, PublicDhKey, PrivateDhKey,
				   [Prime, Base]),
	    DHLen = erlang:byte_size(DHSecret),
	    Len = erlang:byte_size(PSK),
	    PremasterSecret = <<?UINT16(DHLen), DHSecret/binary, ?UINT16(Len), PSK/binary>>,
	    master_from_premaster_secret(PremasterSecret, State);
	#alert{} = Alert ->
	    Alert;
	_ ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
    end.

server_rsa_psk_master_secret(PskIdentity, PremasterSecret,
			     #state{ssl_options = SslOpts} = State) ->
    case handle_psk_identity(PskIdentity, SslOpts#ssl_options.user_lookup_fun) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = byte_size(PSK),
	    RealPMS = <<?UINT16(48), PremasterSecret/binary, ?UINT16(Len), PSK/binary>>,
	    master_from_premaster_secret(RealPMS, State);
	#alert{} = Alert ->
	    Alert;
	_ ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
    end.

generate_srp_server_keys(_SrpParams, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_server_keys(SrpParams =
			     #srp_user{generator = Generator, prime = Prime,
				       verifier = Verifier}, N) ->
    case crypto:generate_key(srp, {host, [Verifier, Generator, Prime, '6a']}) of
	error ->
	    generate_srp_server_keys(SrpParams, N+1);
	Keys ->
	    Keys
    end.

generate_srp_client_keys(_Generator, _Prime, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_client_keys(Generator, Prime, N) ->

    case crypto:generate_key(srp, {user, [Generator, Prime, '6a']}) of
	error ->
	    generate_srp_client_keys(Generator, Prime, N+1);
	Keys ->
	    Keys
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

server_srp_master_secret(Verifier, Prime, ClientPub, State = #state{srp_keys = ServerKeys}) ->
    case crypto:compute_key(srp, ClientPub, ServerKeys, {host, [Verifier, Prime, '6a']}) of
	error ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
	PremasterSecret ->
	    master_from_premaster_secret(PremasterSecret, State)
    end.

client_srp_master_secret(_Generator, _Prime, _Salt, _ServerPub, #alert{} = Alert, _State) ->
    Alert;
client_srp_master_secret(Generator, Prime, Salt, ServerPub, undefined, State) ->
    Keys = generate_srp_client_keys(Generator, Prime, 0),
    client_srp_master_secret(Generator, Prime, Salt, ServerPub, Keys, State#state{srp_keys = Keys});

client_srp_master_secret(Generator, Prime, Salt, ServerPub, ClientKeys,
			 #state{ssl_options = SslOpts} = State) ->
    case ssl_srp_primes:check_srp_params(Generator, Prime) of
	ok ->
	    {Username, Password} = SslOpts#ssl_options.srp_identity,
	    DerivedKey = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
	    case crypto:compute_key(srp, ServerPub, ClientKeys, {user, [DerivedKey, Prime, Generator, '6a']}) of
		error ->
		    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
		PremasterSecret ->
		    master_from_premaster_secret(PremasterSecret, State)
	    end;
	_ ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
    end.

cipher_role(client, Data, Session, #state{connection_states = ConnectionStates0} = State,
	    Connection) ->
    ConnectionStates = ssl_record:set_server_verify_data(current_both, Data, ConnectionStates0),
    Connection:next_state_connection(cipher,
				     Connection:ack_connection(
				       State#state{session = Session,
						   connection_states = ConnectionStates}));

cipher_role(server, Data, Session,  #state{connection_states = ConnectionStates0} = State0,
	    Connection) ->
    ConnectionStates1 = ssl_record:set_client_verify_data(current_read, Data, ConnectionStates0),
    State =
	finalize_handshake(State0#state{connection_states = ConnectionStates1,
					session = Session}, cipher, Connection),
    Connection:next_state_connection(cipher, Connection:ack_connection(State#state{session = Session})).

negotiated_hashsign(undefined, Algo, Version) ->
    default_hashsign(Version, Algo);
negotiated_hashsign(HashSign = {_, _}, _, _) ->
    HashSign.

%% RFC 5246, Sect. 7.4.1.4.1.  Signature Algorithms
%% If the client does not send the signature_algorithms extension, the
%% server MUST do the following:
%%
%% -  If the negotiated key exchange algorithm is one of (RSA, DHE_RSA,
%%    DH_RSA, RSA_PSK, ECDH_RSA, ECDHE_RSA), behave as if client had
%%    sent the value {sha1,rsa}.
%%
%% -  If the negotiated key exchange algorithm is one of (DHE_DSS,
%%    DH_DSS), behave as if the client had sent the value {sha1,dsa}.
%%
%% -  If the negotiated key exchange algorithm is one of (ECDH_ECDSA,
%%    ECDHE_ECDSA), behave as if the client had sent value {sha1,ecdsa}.

default_hashsign(_Version = {Major, Minor}, KeyExchange)
  when Major >= 3 andalso Minor >= 3 andalso
       (KeyExchange == rsa orelse
	KeyExchange == dhe_rsa orelse
	KeyExchange == dh_rsa orelse
	KeyExchange == ecdhe_rsa orelse
	KeyExchange == ecdh_rsa orelse
	KeyExchange == srp_rsa) ->
    {sha, rsa};
default_hashsign(_Version, KeyExchange)
  when KeyExchange == rsa;
       KeyExchange == dhe_rsa;
       KeyExchange == dh_rsa;
       KeyExchange == ecdhe_rsa;
       KeyExchange == ecdh_rsa;
       KeyExchange == srp_rsa ->
    {md5sha, rsa};
default_hashsign(_Version, KeyExchange)
  when KeyExchange == ecdhe_ecdsa;
       KeyExchange == ecdh_ecdsa ->
    {sha, ecdsa};
default_hashsign(_Version, KeyExchange)
  when KeyExchange == dhe_dss;
       KeyExchange == dh_dss;
       KeyExchange == srp_dss ->
    {sha, dsa};
default_hashsign(_Version, KeyExchange)
  when KeyExchange == dh_anon;
       KeyExchange == ecdh_anon;
       KeyExchange == psk;
       KeyExchange == dhe_psk;
       KeyExchange == rsa_psk;
       KeyExchange == srp_anon ->
    {null, anon}.

select_curve(#state{client_ecc = {[Curve|_], _}}) ->
    {namedCurve, Curve};
select_curve(_) ->
    {namedCurve, ?secp256k1}.

is_anonymous(Algo) when Algo == dh_anon;
			Algo == ecdh_anon;
			Algo == psk;
			Algo == dhe_psk;
			Algo == rsa_psk;
			Algo == srp_anon ->
    true;
is_anonymous(_) ->
    false.

get_current_prf(CStates, Direction) ->
	CS = ssl_record:current_connection_state(CStates, Direction),
	CS#connection_state.security_parameters#security_parameters.prf_algorithm.
get_pending_prf(CStates, Direction) ->
	CS = ssl_record:pending_connection_state(CStates, Direction),
	CS#connection_state.security_parameters#security_parameters.prf_algorithm.

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

record_cb(tls_connection) ->
    tls_record;
record_cb(dtls_connection) ->
    dtls_record.
