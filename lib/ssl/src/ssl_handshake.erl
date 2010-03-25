%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling the SSL-handshake protocol
%%----------------------------------------------------------------------

-module(ssl_handshake).

-include("ssl_handshake.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_debug.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([master_secret/4, client_hello/4, server_hello/3, hello/2,
	 hello_request/0, certify/5, certificate/3, 
	 client_certificate_verify/6, 
	 certificate_verify/6, certificate_request/2,
	 key_exchange/2, finished/4,
	 verify_connection/5, 
	 get_tls_handshake/4,
	 server_hello_done/0, sig_alg/1,
         encode_handshake/3, init_hashes/0, 
         update_hashes/2, decrypt_premaster_secret/2]).

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: client_hello(Host, Port, ConnectionStates, SslOpts) -> 
%%                                                  #client_hello{} 
%%      Host
%%      Port
%%      ConnectionStates = #connection_states{}
%%      SslOpts = #ssl_options{}
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(Host, Port, ConnectionStates, #ssl_options{versions = Versions,
							ciphers = Ciphers} 
	     = SslOpts) ->
    
    Fun = fun(Version) ->
		  ssl_record:protocol_version(Version)
	  end,
    Version = ssl_record:highest_protocol_version(lists:map(Fun, Versions)),
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
   
    Id = ssl_manager:client_session_id(Host, Port, SslOpts),

    #client_hello{session_id = Id, 
		  client_version = Version,
		  cipher_suites = Ciphers,
		  compression_methods = ssl_record:compressions(),
		  random = SecParams#security_parameters.client_random
		 }.

%%--------------------------------------------------------------------
%% Function: server_hello(Host, Port, SessionId, 
%%                        Version, ConnectionStates) -> #server_hello{} 
%%      SessionId
%%      Version
%%      ConnectionStates 
%%	
%%
%% Description: Creates a server hello message.
%%--------------------------------------------------------------------
server_hello(SessionId, Version, ConnectionStates) ->
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    #server_hello{server_version = Version,
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method = 
		  SecParams#security_parameters.compression_algorithm,
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId
		 }.

%%--------------------------------------------------------------------
%% Function: hello_request() -> #hello_request{} 
%%
%% Description: Creates a hello request message sent by server to 
%% trigger renegotiation.
%%--------------------------------------------------------------------
hello_request() ->
    #hello_request{}.

%%--------------------------------------------------------------------
%% Function: hello(Hello, Info) -> 
%%                                   {Version, Id, NewConnectionStates} |
%%                                   #alert{}
%%
%%      Hello = #client_hello{} | #server_hello{}
%%      Info = ConnectionStates | {Port, Session, ConnectionStates}
%%      ConnectionStates = #connection_states{}
%%
%% Description: Handles a recieved hello message
%%--------------------------------------------------------------------
hello(#server_hello{cipher_suite = CipherSuite, server_version = Version,
		    compression_method = Compression, random = Random,
		    session_id = SessionId}, ConnectionStates) ->
    NewConnectionStates =
	hello_pending_connection_states(client, CipherSuite, Random, 
					Compression, ConnectionStates),
    {Version, SessionId, NewConnectionStates};

hello(#client_hello{client_version = ClientVersion, random = Random} = Hello,
      {Port, #ssl_options{versions = Versions} = SslOpts,
       Session0, Cache, CacheCb, ConnectionStates0}) ->
    Version = select_version(ClientVersion, Versions),
    case ssl_record:is_acceptable_version(Version) of
	true ->
	    {Type, #session{cipher_suite = CipherSuite,
			    compression_method = Compression} = Session} 
		= select_session(Hello, Port, Session0, Version, 
				 SslOpts, Cache, CacheCb),
	    case CipherSuite of 
		no_suite ->
		    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY);
		_ ->
		    ConnectionStates =
			hello_pending_connection_states(server, 
							CipherSuite,
						        Random, 
							Compression,
							ConnectionStates0),
		    {Version, {Type, Session}, ConnectionStates}
	    end;
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end.

%%--------------------------------------------------------------------
%% Function: certify(Certs, CertDbRef, MaxPathLen) ->
%%                                 {PeerCert, PublicKeyInfo}  | #alert{}
%%
%%      Certs = #certificate{}
%%	CertDbRef = reference()
%%      MaxPathLen = integer() | nolimit
%%
%% Description: Handles a certificate handshake message
%%--------------------------------------------------------------------
certify(#certificate{asn1_certificates = ASN1Certs}, CertDbRef, 
	MaxPathLen, Verify, VerifyFun) -> 
    [PeerCert | _] = ASN1Certs,
    VerifyBool =  verify_bool(Verify),
  
    try
	%% Allow missing root_cert and check that with VerifyFun
	ssl_certificate:trusted_cert_and_path(ASN1Certs, CertDbRef, false) of
	{TrustedErlCert, CertPath, VerifyErrors} ->
	    Result = public_key:pkix_path_validation(TrustedErlCert, 
						     CertPath, 
						     [{max_path_length, 
						       MaxPathLen},
						      {verify, VerifyBool},
						      {acc_errors, 
						       VerifyErrors}]),
	    case Result of
		{error, Reason} ->
		    path_validation_alert(Reason, Verify);
		{ok, {PublicKeyInfo,_, []}} ->
		    {PeerCert, PublicKeyInfo};
		{ok, {PublicKeyInfo,_, AccErrors = [Error | _]}} ->
		    case VerifyFun(AccErrors) of
			true ->
			    {PeerCert, PublicKeyInfo};
			false ->
			    path_validation_alert(Error, Verify)
		    end
	    end
    catch 
	throw:Alert ->
	    Alert
    end.
	    
%%--------------------------------------------------------------------
%% Function: certificate(OwnCert, CertDbRef, Role) -> #certificate{}
%%
%%      OwnCert = binary()
%%      CertDbRef = term() as returned by ssl_certificate_db:create()
%%
%% Description: Creates a certificate message.
%%--------------------------------------------------------------------
certificate(OwnCert, CertDbRef, client) ->
    Chain =
	case ssl_certificate:certificate_chain(OwnCert, CertDbRef) of
	    {ok, CertChain} ->
		CertChain;
	    {error, _} -> 
		%% If no suitable certificate is available, the client
		%% SHOULD send a certificate message containing no
		%% certificates. (chapter 7.4.6. rfc 4346) 
		[]	 
	end,
    #certificate{asn1_certificates = Chain};

certificate(OwnCert, CertDbRef, server) -> 
    case ssl_certificate:certificate_chain(OwnCert, CertDbRef) of
	{ok, Chain} ->
	    #certificate{asn1_certificates = Chain};
	{error, _} ->
	    ?ALERT_REC(?FATAL, ?INTERNAL_ERROR)
    end.

%%--------------------------------------------------------------------
%% Function: client_certificate_verify(Cert, ConnectionStates) -> 
%%                                          #certificate_verify{} | ignore
%% Cert             = #'OTPcertificate'{}
%% ConnectionStates = #connection_states{}
%%
%% Description: Creates a certificate_verify message, called by the client.
%%--------------------------------------------------------------------
client_certificate_verify(undefined, _, _, _, _, _) ->
    ignore;
client_certificate_verify(_, _, _, _, undefined, _) ->
    ignore;
client_certificate_verify(OwnCert, MasterSecret, Version, Algorithm,
			  PrivateKey, {Hashes0, _}) ->
    case public_key:pkix_is_fixed_dh_cert(OwnCert) of
	true ->
	    ignore;
	false ->	    
	    Hashes = 
		calc_certificate_verify(Version, MasterSecret,
					Algorithm, Hashes0), 
	    Signed = digitally_signed(Hashes, PrivateKey),
	    #certificate_verify{signature = Signed}
    end.

%%--------------------------------------------------------------------
%% Function: certificate_verify(Signature, PublicKeyInfo) -> valid | #alert{}
%%
%% Signature     = binary()
%% PublicKeyInfo = {Algorithm, PublicKey, PublicKeyParams}
%%
%% Description: Checks that the certificate_verify message is valid.
%%--------------------------------------------------------------------
certificate_verify(Signature, {_, PublicKey, _}, Version, 
		   MasterSecret, Algorithm, {_, Hashes0})
  when Algorithm =:= rsa; Algorithm =:= dh_rsa; Algorithm =:= dhe_rsa ->
    Hashes = calc_certificate_verify(Version, MasterSecret,
					   Algorithm, Hashes0),
    case public_key:decrypt_public(Signature, PublicKey, 
				   [{rsa_pad, rsa_pkcs1_padding}]) of
	Hashes ->
	    valid;
	_ ->
	    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE)
    end.
%% TODO dsa clause

%%--------------------------------------------------------------------
%% Function: certificate_request(ConnectionStates, CertDbRef) -> 
%%                                                #certificate_request{}
%%
%% Description: Creates a certificate_request message, called by the server.
%%--------------------------------------------------------------------
certificate_request(ConnectionStates, CertDbRef) ->
    #connection_state{security_parameters = 
		      #security_parameters{cipher_suite = CipherSuite}} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    Types = certificate_types(CipherSuite),
    Authorities = certificate_authorities(CertDbRef),
    #certificate_request{
		    certificate_types = Types,
		    certificate_authorities = Authorities
		   }.

%%--------------------------------------------------------------------
%% Function: key_exchange(Role, Secret, Params) -> 
%%                         #client_key_exchange{} | #server_key_exchange{}
%%
%%      Secret -
%%      Params - 
%%
%% Description: Creates a keyexchange message.
%%--------------------------------------------------------------------
key_exchange(client, {premaster_secret, Secret, {_, PublicKey, _}}) ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{exchange_keys = EncPremasterSecret};
key_exchange(client, fixed_diffie_hellman) -> 
    #client_key_exchange{exchange_keys = 
			 #client_diffie_hellman_public{
			   dh_public = <<>>
			  }};
key_exchange(client, {dh, PublicKey}) ->
    Len = byte_size(PublicKey), 
    #client_key_exchange{
		exchange_keys = #client_diffie_hellman_public{
		  dh_public = <<?UINT16(Len), PublicKey/binary>>}
	       };

%% key_exchange(server, {{?'dhpublicnumber', _PublicKey, 
%% 		       #'DomainParameters'{p = P, g = G, y = Y},
%% 		       SignAlgorithm, ClientRandom, ServerRandom}})  ->
%%     ServerDHParams = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y},
%%     PLen = byte_size(P),
%%     GLen = byte_size(G),
%%     YLen = byte_size(Y),
%%     Hash = server_key_exchange_hash(SignAlgorithm, <<ClientRandom/binary, 
%% 						    ServerRandom/binary, 
%% 						    ?UINT16(PLen), P/binary, 
%% 						    ?UINT16(GLen), G/binary,
%% 						    ?UINT16(YLen), Y/binary>>),
%%     Signed = digitally_signed(Hash, PrivateKey),
%%     #server_key_exchange{
%% 		  params = ServerDHParams,
%% 		  signed_params = Signed
%% 		 };
key_exchange(_, _) ->
    %%TODO : Real imp
    #server_key_exchange{}.

%%--------------------------------------------------------------------
%% Function: master_secret(Version, Session/PremasterSecret, 
%%                         ConnectionStates, Role) -> 
%%                          {MasterSecret, NewConnectionStates} | #alert{}
%%      Version = #protocol_version{}
%%      Session = #session{} (session contains master secret)
%%      PremasterSecret = binary()  
%%      ConnectionStates = #connection_states{}
%%      Role = client | server
%%
%% Description: Sets or calculates the master secret and calculate keys,
%% updating the pending connection states. The Mastersecret and the update
%% connection states are returned or an alert if the calculation fails.
%%-------------------------------------------------------------------
master_secret(Version, #session{master_secret = Mastersecret}, 
	      ConnectionStates, Role) ->
    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    try master_secret(Version, Mastersecret, SecParams, 
		      ConnectionStates, Role)
    catch
	exit:Reason ->
	    error_logger:error_report("Key calculation failed due to ~p",
				      [Reason]),
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end;

master_secret(Version, PremasterSecret, ConnectionStates, Role) ->
    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams, 
    try master_secret(Version, 
		      calc_master_secret(Version,PremasterSecret,
				       ClientRandom, ServerRandom),
		      SecParams, ConnectionStates, Role) 
    catch
	exit:Reason ->
	    error_logger:error_report("Master secret calculation failed"
				      " due to ~p", [Reason]),
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end.

%%--------------------------------------------------------------------
%% Function: finished(Version, Role, MacSecret, Hashes) -> #finished{}
%%
%%      ConnectionStates = #connection_states{}
%%
%% Description: Creates a handshake finished message
%%-------------------------------------------------------------------
finished(Version, Role, MasterSecret, {Hashes, _}) -> % use the current hashes
    #finished{verify_data = 
	      calc_finished(Version, Role, MasterSecret, Hashes)}.

%%--------------------------------------------------------------------
%% Function: verify_connection(Finished, Role, 
%%                             MasterSecret, Hashes) -> verified | #alert{}
%% 
%% Finished = #finished{}
%% Role = client | server - the role of the process that sent the finished
%% message.
%% MasterSecret = binary()
%% Hashes = binary() -  {md5_hash, sha_hash} 
%%
%%
%% Description: Checks the ssl handshake finished message to verify
%%              the connection.
%%-------------------------------------------------------------------
verify_connection(Version, #finished{verify_data = Data}, 
		  Role, MasterSecret, {_, {MD5, SHA}}) -> 
    %% use the previous hashes
    ?DBG_HEX(crypto:md5_final(MD5)),
    ?DBG_HEX(crypto:sha_final(SHA)),
    case calc_finished(Version, Role, MasterSecret, {MD5, SHA}) of
	Data ->
	    verified;
	_E ->
 	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end.
	    
server_hello_done() ->
    #server_hello_done{}.

%%--------------------------------------------------------------------
%% Function: encode_handshake(HandshakeRec) -> BinHandshake
%% HandshakeRec = #client_hello | #server_hello{} | server_hello_done |
%%              #certificate{} | #client_key_exchange{} | #finished{} |
%%              #client_certify_request{}
%%     
%% encode a handshake packet to binary
%%--------------------------------------------------------------------
encode_handshake(Package, Version, SigAlg) ->
    {MsgType, Bin} = enc_hs(Package, Version, SigAlg),
    Len = byte_size(Bin),
    [MsgType, ?uint24(Len), Bin].

%%--------------------------------------------------------------------
%% Function: get_tls_handshake(Data, Buffer) -> Result
%%      Result = {[#handshake{}], [Raw], NewBuffer}
%%      Data = Buffer = NewBuffer = Raw = binary()
%%
%% Description: Given buffered and new data from ssl_record, collects
%% and returns it as a list of #handshake, also returns leftover
%% data.
%%--------------------------------------------------------------------
get_tls_handshake(Data, <<>>, KeyAlg, Version) ->
    get_tls_handshake_aux(Data, KeyAlg, Version, []);
get_tls_handshake(Data, Buffer, KeyAlg, Version) ->
    get_tls_handshake_aux(list_to_binary([Buffer, Data]), 
			  KeyAlg, Version, []).

get_tls_handshake_aux(<<?BYTE(Type), ?UINT24(Length), Body:Length/binary,Rest/binary>>, 
		      KeyAlg, Version, Acc) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), Body/binary>>,
    H = dec_hs(Type, Body, KeyAlg, Version),
    get_tls_handshake_aux(Rest, KeyAlg, Version, [{H,Raw} | Acc]);
get_tls_handshake_aux(Data, _KeyAlg, _Version, Acc) ->
    {lists:reverse(Acc), Data}.

%%--------------------------------------------------------------------
%% Function: sig_alg(atom()) -> integer()
%%
%% Description: Convert from key exchange as atom to signature
%% algorithm as a ?SIGNATURE_... constant
%%--------------------------------------------------------------------

sig_alg(dh_anon) ->
    ?SIGNATURE_ANONYMOUS;
sig_alg(Alg) when Alg == dhe_rsa; Alg == rsa; Alg == dh_rsa ->
    ?SIGNATURE_RSA;
sig_alg(Alg) when Alg == dh_dss; Alg == dhe_dss ->
    ?SIGNATURE_DSA;
sig_alg(_) ->
    ?NULL.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
verify_bool(verify_peer) ->
    true;
verify_bool(verify_none) ->
    false.

path_validation_alert({bad_cert, cert_expired}, _) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_EXPIRED);
path_validation_alert({bad_cert, invalid_issuer}, _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, invalid_signature} , _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, name_not_permitted}, _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, unknown_critical_extension}, _) ->
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE);
path_validation_alert({bad_cert, cert_revoked}, _) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_REVOKED);
path_validation_alert(_, _) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE).

select_session(Hello, Port, Session, Version, 
	       #ssl_options{ciphers = UserSuites} = SslOpts, Cache, CacheCb) ->
    SuggestedSessionId = Hello#client_hello.session_id,
    SessionId = ssl_manager:server_session_id(Port, SuggestedSessionId, 
					      SslOpts),
    
    Suites = case UserSuites of
		 [] ->
		     ssl_cipher:suites(Version);
		 _ ->
		   UserSuites
	     end,

    case ssl_session:is_new(SuggestedSessionId, SessionId) of
        true ->
	    CipherSuite = 
		select_cipher_suite(Hello#client_hello.cipher_suites, Suites),
	    Compressions = Hello#client_hello.compression_methods,
	    Compression = select_compression(Compressions),
	    {new, Session#session{session_id = SessionId,
				  cipher_suite = CipherSuite,
				  compression_method = Compression}};
	false ->	    
	    {resumed, CacheCb:lookup(Cache, {Port, SessionId})}
    end.
	    
%% Update pending connection states with parameters exchanged via 
%% hello messages
%% NOTE : Role is the role of the receiver of the hello message
%%        currently being processed.
hello_pending_connection_states(Role, CipherSuite, Random, Compression,
				 ConnectionStates) ->    
    ReadState =  
	ssl_record:pending_connection_state(ConnectionStates, read),
    WriteState = 
	ssl_record:pending_connection_state(ConnectionStates, write),
    
    NewReadSecParams = 
	hello_security_parameters(Role, ReadState, CipherSuite, 
			    Random, Compression),
    
    NewWriteSecParams =
	hello_security_parameters(Role, WriteState, CipherSuite,
			    Random, Compression),
 
    ssl_record:update_security_params(NewReadSecParams,
				    NewWriteSecParams,
				    ConnectionStates).

hello_security_parameters(client, ConnectionState, CipherSuite, Random,
			  Compression) ->   
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(CipherSuite, SecParams),
    NewSecParams#security_parameters{
      server_random = Random,
      compression_algorithm = Compression
     };

hello_security_parameters(server, ConnectionState, CipherSuite, Random, 
			  Compression) ->
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(CipherSuite, SecParams),
    NewSecParams#security_parameters{
      client_random = Random,
      compression_algorithm = Compression
     }.

select_version(ClientVersion, Versions) ->   
    Fun = fun(Version) ->
		  ssl_record:protocol_version(Version)
	  end,
    ServerVersion = ssl_record:highest_protocol_version(lists:map(Fun,
								  Versions)),
    ssl_record:lowest_protocol_version(ClientVersion, ServerVersion).

select_cipher_suite([], _) ->
   no_suite;
select_cipher_suite([Suite | ClientSuites], SupportedSuites) ->
    case is_member(Suite, SupportedSuites) of
	true ->
	    Suite;
        false ->
	    select_cipher_suite(ClientSuites, SupportedSuites)
    end.

is_member(Suite, SupportedSuites) ->
    lists:member(Suite, SupportedSuites).

select_compression(_CompressionMetodes) ->
    ?NULL.

master_secret(Version, MasterSecret, #security_parameters{
			 client_random = ClientRandom,
			 server_random = ServerRandom,
			 hash_size = HashSize,
			 key_material_length = KML,
			 expanded_key_material_length = EKML,
			 iv_size = IVS,
			 exportable = Exportable},
	      ConnectionStates, Role) ->
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV} =
	setup_keys(Version, Exportable, MasterSecret, ServerRandom, 
		   ClientRandom, HashSize, KML, EKML, IVS),
    ?DBG_HEX(ClientWriteKey),
    ?DBG_HEX(ClientIV),
    ConnStates1 = ssl_record:set_master_secret(MasterSecret, ConnectionStates),
    ConnStates2 =
	ssl_record:set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret,
				  Role, ConnStates1),

    ClientCipherState = #cipher_state{iv = ClientIV, key = ClientWriteKey},
    ServerCipherState = #cipher_state{iv = ServerIV, key = ServerWriteKey}, 
    {MasterSecret, 
     ssl_record:set_pending_cipher_state(ConnStates2, ClientCipherState, 
					 ServerCipherState, Role)}.


dec_hs(?HELLO_REQUEST, <<>>, _, _) ->
    #hello_request{};

%% Client hello v2.
%% The server must be able to receive such messages, from clients that
%% are willing to use ssl v3 or higher, but have ssl v2 compatibility.
dec_hs(?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor),
		       ?UINT16(CSLength), ?UINT16(0),
		       ?UINT16(CDLength), 
		       CipherSuites:CSLength/binary, 
		       ChallengeData:CDLength/binary>>,
       _, _) ->
    ?DBG_HEX(CipherSuites),
    ?DBG_HEX(CipherSuites),
    #client_hello{client_version = {Major, Minor},
		  random = ssl_ssl2:client_random(ChallengeData, CDLength),
		  session_id = 0,
		  cipher_suites = from_3bytes(CipherSuites),
		  compression_methods = [?NULL]
		 };
dec_hs(?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
		       ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
		       _FutureCompatData/binary>>,
       _, _) ->
    #client_hello{
	client_version = {Major,Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suites = from_2bytes(CipherSuites),
	compression_methods = Comp_methods
       };
dec_hs(?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method)>>, _, _) ->
    #server_hello{
	server_version = {Major,Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method
       };
dec_hs(?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>, _, _) ->
    #certificate{asn1_certificates = certs_to_list(ASN1Certs)};
dec_hs(?SERVER_KEY_EXCHANGE, <<?UINT16(ModLen), Mod:ModLen/binary,
			      ?UINT16(ExpLen), Exp:ExpLen/binary,
			      Sig/binary>>,
       ?KEY_EXCHANGE_RSA, _) ->
    #server_key_exchange{params = #server_rsa_params{rsa_modulus = Mod, 
						     rsa_exponent = Exp}, 
			 signed_params = Sig}; 	
dec_hs(?SERVER_KEY_EXCHANGE, <<?UINT16(PLen), P:PLen/binary,
			      ?UINT16(GLen), G:GLen/binary,
			      ?UINT16(YLen), Y:YLen/binary,
			      Sig/binary>>,
       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #server_key_exchange{params = #server_dh_params{dh_p = P,dh_g = G, dh_y = Y},
			 signed_params = Sig};
dec_hs(?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>, _, _) ->
    %% TODO: maybe we should chop up CertAuths into a list?
    #certificate_request{certificate_types = CertTypes,
			 certificate_authorities = CertAuths};
dec_hs(?SERVER_HELLO_DONE, <<>>, _, _) ->
    #server_hello_done{};
dec_hs(?CERTIFICATE_VERIFY,<<?UINT16(_), Signature/binary>>, _, _)->
    #certificate_verify{signature = Signature};
dec_hs(?CLIENT_KEY_EXCHANGE, PKEPMS, rsa, {3, 0}) ->
    PreSecret = #encrypted_premaster_secret{premaster_secret = PKEPMS},
    #client_key_exchange{exchange_keys = PreSecret};
dec_hs(?CLIENT_KEY_EXCHANGE, <<?UINT16(_), PKEPMS/binary>>, rsa, _) ->
    PreSecret = #encrypted_premaster_secret{premaster_secret = PKEPMS},
    #client_key_exchange{exchange_keys = PreSecret};
dec_hs(?CLIENT_KEY_EXCHANGE, <<>>, ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) -> 
    %% TODO: Should check whether the cert already contains a suitable DH-key (7.4.7.2)
    throw(?ALERT_REC(?FATAL, implicit_public_value_encoding));
dec_hs(?CLIENT_KEY_EXCHANGE, <<?UINT16(DH_YCLen), DH_YC:DH_YCLen/binary>>,
       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #client_diffie_hellman_public{dh_public = DH_YC};
dec_hs(?FINISHED, VerifyData, _, _) ->
    #finished{verify_data = VerifyData};
dec_hs(_, _, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

encrypted_premaster_secret(Secret, RSAPublicKey) -> 
    try 
	PreMasterSecret = public_key:encrypt_public(Secret, RSAPublicKey, 
						    [{rsa_pad, 
						      rsa_pkcs1_padding}]),
	#encrypted_premaster_secret{premaster_secret = PreMasterSecret}
    catch
	_:_->
	    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE))
    end.

decrypt_premaster_secret(Secret, RSAPrivateKey) ->
    try public_key:decrypt_private(Secret, RSAPrivateKey,  
				   [{rsa_pad, rsa_pkcs1_padding}])
    catch
	_:_ ->
	    throw(?ALERT_REC(?FATAL, ?DECRYPTION_FAILED))
    end.
    
%% encode/decode stream of certificate data to/from list of certificate data 
certs_to_list(ASN1Certs) ->
    certs_to_list(ASN1Certs, []).

certs_to_list(<<?UINT24(CertLen), Cert:CertLen/binary, Rest/binary>>, Acc) ->
    certs_to_list(Rest, [Cert | Acc]);
certs_to_list(<<>>, Acc) ->
    lists:reverse(Acc, []).

certs_from_list(ACList) ->
    list_to_binary([begin
			CertLen = byte_size(Cert),
                        <<?UINT24(CertLen), Cert/binary>>
		    end || Cert <- ACList]).

enc_hs(#hello_request{}, _Version, _) ->
    {?HELLO_REQUEST, <<>>};
enc_hs(#client_hello{
	client_version = {Major, Minor},
	random = Random,
	session_id = SessionID,
	cipher_suites = CipherSuites,
	compression_methods = CompMethods}, _Version, _) ->
    SIDLength = byte_size(SessionID),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SIDLength), SessionID/binary,
		     ?UINT16(CsLength), BinCipherSuites/binary,
		     ?BYTE(CmLength), BinCompMethods/binary>>};
enc_hs(#server_hello{
	server_version = {Major, Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method}, _Version, _) ->
    SID_length = byte_size(Session_ID),
    {?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SID_length), Session_ID/binary,
                     Cipher_suite/binary, ?BYTE(Comp_method)>>};
enc_hs(#certificate{asn1_certificates = ASN1CertList}, _Version, _) ->
    ASN1Certs = certs_from_list(ASN1CertList),
    ACLen = erlang:iolist_size(ASN1Certs),
    {?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>};
enc_hs(#server_key_exchange{params = #server_rsa_params{rsa_modulus = Mod,
							rsa_exponent = Exp},
	signed_params = SignedParams}, _Version, _) ->
    ModLen = byte_size(Mod),
    ExpLen = byte_size(Exp),
    {?SERVER_KEY_EXCHANGE, <<?UINT16(ModLen), Mod/binary,
			    ?UINT16(ExpLen), Exp/binary,
			    SignedParams/binary>>
    };
enc_hs(#server_key_exchange{params = #server_dh_params{
			      dh_p = P, dh_g = G, dh_y = Y},
	signed_params = SignedParams}, _Version, _) ->
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    {?SERVER_KEY_EXCHANGE, <<?UINT16(PLen), P:PLen/binary,
			    ?UINT16(GLen), G:GLen/binary,
			    ?UINT16(YLen), Y:YLen/binary,
			    SignedParams/binary>>
    };
enc_hs(#certificate_request{certificate_types = CertTypes,
			    certificate_authorities = CertAuths}, 
       _Version, _) ->
    CertTypesLen = byte_size(CertTypes),
    CertAuthsLen = byte_size(CertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(CertAuthsLen), CertAuths/binary>>
    };
enc_hs(#server_hello_done{}, _Version, _) ->
    {?SERVER_HELLO_DONE, <<>>};
enc_hs(#client_key_exchange{exchange_keys = ExchangeKeys}, Version, _) ->
    {?CLIENT_KEY_EXCHANGE, enc_cke(ExchangeKeys, Version)};
enc_hs(#certificate_verify{signature = BinSig}, _, _) ->
    EncSig = enc_bin_sig(BinSig),
    {?CERTIFICATE_VERIFY, EncSig};
enc_hs(#finished{verify_data = VerifyData}, _Version, _) ->
    {?FINISHED, VerifyData}.

enc_cke(#encrypted_premaster_secret{premaster_secret = PKEPMS},{3, 0}) ->
    PKEPMS;
enc_cke(#encrypted_premaster_secret{premaster_secret = PKEPMS}, _) ->
    PKEPMSLen = byte_size(PKEPMS),
    <<?UINT16(PKEPMSLen), PKEPMS/binary>>;
enc_cke(#client_diffie_hellman_public{dh_public = DHPublic}, _) ->
    Len = byte_size(DHPublic),
    <<?UINT16(Len), DHPublic/binary>>.

enc_bin_sig(BinSig) ->
    Size = byte_size(BinSig),
    <<?UINT16(Size), BinSig/binary>>.

init_hashes() ->
    T = {crypto:md5_init(), crypto:sha_init()},
    {T, T}.

update_hashes(Hashes, % special-case SSL2 client hello
	      <<?CLIENT_HELLO, ?UINT24(_), ?BYTE(Major), ?BYTE(Minor),
	       ?UINT16(CSLength), ?UINT16(0),
	       ?UINT16(CDLength), 
	       CipherSuites:CSLength/binary, 
	       ChallengeData:CDLength/binary>>) ->
    update_hashes(Hashes,
		  <<?CLIENT_HELLO, ?BYTE(Major), ?BYTE(Minor),
		   ?UINT16(CSLength), ?UINT16(0),
		   ?UINT16(CDLength), 
		   CipherSuites:CSLength/binary, 
		   ChallengeData:CDLength/binary>>);
update_hashes({{MD50, SHA0}, _Prev}, Data) ->
    ?DBG_HEX(Data),
    {MD51, SHA1} = {crypto:md5_update(MD50, Data),
		    crypto:sha_update(SHA0, Data)},
    ?DBG_HEX(crypto:md5_final(MD51)),
    ?DBG_HEX(crypto:sha_final(SHA1)),
    {{MD51, SHA1}, {MD50, SHA0}}.

from_3bytes(Bin3) ->
    from_3bytes(Bin3, []).

from_3bytes(<<>>, Acc) ->
    lists:reverse(Acc);
from_3bytes(<<?UINT24(N), Rest/binary>>, Acc) ->
    from_3bytes(Rest, [?uint16(N) | Acc]).

from_2bytes(Bin2) ->
    from_2bytes(Bin2, []).

from_2bytes(<<>>, Acc) ->
    lists:reverse(Acc);
from_2bytes(<<?UINT16(N), Rest/binary>>, Acc) ->
    from_2bytes(Rest, [?uint16(N) | Acc]).

certificate_types({KeyExchange, _, _, _})  
  when KeyExchange == rsa;
       KeyExchange == dh_dss;
       KeyExchange == dh_rsa;
       KeyExchange == dhe_dss;
       KeyExchange == dhe_rsa ->
    <<?BYTE(?RSA_SIGN), ?BYTE(?DSS_SIGN)>>;

certificate_types(_) ->
    %%TODO: Is this a good default,
    %% is there a case where we like to request
    %% a RSA_FIXED_DH or DSS_FIXED_DH
    <<?BYTE(?RSA_SIGN)>>.

certificate_authorities(CertDbRef) ->
    Authorities = certificate_authorities_from_db(CertDbRef),
    Enc = fun(#'OTPCertificate'{tbsCertificate=TBSCert}) ->
		  OTPSubj = TBSCert#'OTPTBSCertificate'.subject,
		  Subj = public_key:pkix_transform(OTPSubj, encode),
		  {ok, DNEncoded} = 'OTP-PUB-KEY':encode('Name', Subj),
		  DNEncodedBin = iolist_to_binary(DNEncoded),
		  DNEncodedLen = byte_size(DNEncodedBin),
		  <<?UINT16(DNEncodedLen), DNEncodedBin/binary>>
	  end,
    list_to_binary([Enc(Cert) || {_, Cert} <- Authorities]).

certificate_authorities_from_db(CertDbRef) ->
    certificate_authorities_from_db(CertDbRef, no_candidate, []).

certificate_authorities_from_db(CertDbRef, PrevKey, Acc) ->
    case ssl_certificate_db:issuer_candidate(PrevKey) of
	no_more_candidates ->
	    lists:reverse(Acc);
	{{CertDbRef, _, _} = Key, Cert} ->
	    certificate_authorities_from_db(CertDbRef, Key, [Cert|Acc]);
	{Key, _Cert} ->
	    %% skip certs not from this ssl connection
	    certificate_authorities_from_db(CertDbRef, Key, Acc)
    end.

digitally_signed(Hashes, #'RSAPrivateKey'{} = Key) ->
    public_key:encrypt_private(Hashes, Key,
			       [{rsa_pad, rsa_pkcs1_padding}]);
digitally_signed(Hashes, #'DSAPrivateKey'{} = Key) ->
    public_key:sign(Hashes, Key).


calc_master_secret({3,0}, PremasterSecret, ClientRandom, ServerRandom) ->
    ssl_ssl3:master_secret(PremasterSecret, ClientRandom, ServerRandom);

calc_master_secret({3,N},PremasterSecret, ClientRandom, ServerRandom) 
  when N == 1; N == 2 ->
    ssl_tls1:master_secret(PremasterSecret, ClientRandom, ServerRandom).

setup_keys({3,0}, Exportable, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, EKML, IVS) ->
    ssl_ssl3:setup_keys(Exportable, MasterSecret, ServerRandom, 
			ClientRandom, HashSize, KML, EKML, IVS);

setup_keys({3,1}, _Exportable, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, _EKML, IVS) ->
    ssl_tls1:setup_keys(MasterSecret, ServerRandom, ClientRandom, HashSize, 
			KML, IVS);

setup_keys({3,2}, _Exportable, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, _EKML, _IVS) ->
    ssl_tls1:setup_keys(MasterSecret, ServerRandom, 
			ClientRandom, HashSize, KML).

calc_finished({3, 0}, Role, MasterSecret, Hashes) ->
    ssl_ssl3:finished(Role, MasterSecret, Hashes);
calc_finished({3, N}, Role, MasterSecret, Hashes) 
  when  N == 1; N == 2 ->
    ssl_tls1:finished(Role, MasterSecret, Hashes).

calc_certificate_verify({3, 0}, MasterSecret, Algorithm, Hashes) ->
    ssl_ssl3:certificate_verify(Algorithm, MasterSecret, Hashes);
calc_certificate_verify({3, N}, _, Algorithm, Hashes) 
  when  N == 1; N == 2 ->
    ssl_tls1:certificate_verify(Algorithm, Hashes).

%% server_key_exchange_hash(Algorithm, Value) when Algorithm == rsa;
%% 						Algorithm == dh_rsa;
%% 						Algorithm == dhe_rsa ->
%%     MD5 = crypto:md5_final(Value),
%%     SHA =  crypto:sha_final(Value),
%%     <<MD5/binary, SHA/binary>>;

%% server_key_exchange_hash(Algorithm, Value) when Algorithm == dh_dss;
%% 					   Algorithm == dhe_dss ->
%%     crypto:sha_final(Value).
