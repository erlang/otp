%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

%----------------------------------------------------------------------
%% Purpose: Help funtions for handling the SSL-handshake protocol (common
%% to SSL/TLS and DTLS
%%----------------------------------------------------------------------

-module(ssl_handshake).

-include("ssl_handshake.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl").

-export_type([ssl_handshake/0, ssl_handshake_history/0,
	      public_key_info/0, oid/0]).

-type oid()               :: tuple().
-type public_key_params() :: #'Dss-Parms'{} |  {namedCurve, oid()} | #'ECParameters'{} | term().
-type public_key_info()   :: {oid(), #'RSAPublicKey'{} | integer() | #'ECPoint'{}, public_key_params()}.
-type ssl_handshake_history() :: {[binary()], [binary()]}.

-type ssl_handshake() :: #server_hello{} | #server_hello_done{} | #certificate{} | #certificate_request{} |
			 #client_key_exchange{} | #finished{} | #certificate_verify{} |
			 #hello_request{} | #next_protocol{}.

%% Handshake messages
-export([hello_request/0, server_hello/4, server_hello_done/0,
	 certificate/4, certificate_request/5, key_exchange/3,
	 finished/5,  next_protocol/1]).

%% Handle handshake messages
-export([certify/10, client_certificate_verify/6, certificate_verify/6, verify_signature/5,
	 master_secret/5, server_key_exchange_hash/2, verify_connection/6,
	 init_handshake_history/0, update_handshake_history/2, verify_server_key/5
	]).

%% Encode/Decode
-export([encode_handshake/2, encode_hello_extensions/1,
	 encode_client_protocol_negotiation/2, encode_protocols_advertised_on_server/1,
	 decode_handshake/3, decode_hello_extensions/1,
	 decode_server_key/3, decode_client_key/3,
	 decode_suites/2
	]).

%% Cipher suites handling
-export([available_suites/2, available_signature_algs/3, cipher_suites/2,
	 select_session/11, supported_ecc/1, available_signature_algs/4]).

%% Extensions handling
-export([client_hello_extensions/6,
	 handle_client_hello_extensions/9, %% Returns server hello extensions
	 handle_server_hello_extensions/9, select_curve/2
	]).

%% MISC
-export([select_version/3, prf/6,  select_hashsign/4, select_hashsign/5,
	 select_hashsign_algs/3,
	 premaster_secret/2, premaster_secret/3, premaster_secret/4]).

%%====================================================================
%% Internal application API
%%====================================================================

%% ---------- Create handshake messages  ----------

%%--------------------------------------------------------------------
-spec hello_request() -> #hello_request{}.
%%
%% Description: Creates a hello request message sent by server to
%% trigger renegotiation.
%%--------------------------------------------------------------------
hello_request() ->
    #hello_request{}.

%%--------------------------------------------------------------------
-spec server_hello(#session{}, ssl_record:ssl_version(), #connection_states{},
		   #hello_extensions{}) -> #server_hello{}.
%%
%% Description: Creates a server hello message.
%%--------------------------------------------------------------------
server_hello(SessionId, Version, ConnectionStates, Extensions) ->
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,

    #server_hello{server_version = Version,
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method =
		  SecParams#security_parameters.compression_algorithm,
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId,
		  extensions = Extensions
		 }.

%%--------------------------------------------------------------------
-spec server_hello_done() ->  #server_hello_done{}.
%%
%% Description: Creates a server hello done message.
%%--------------------------------------------------------------------
server_hello_done() ->
    #server_hello_done{}.

client_hello_extensions(Host, Version, CipherSuites, 
			#ssl_options{signature_algs = SupportedHashSigns, versions = AllVersions} = SslOpts, ConnectionStates, Renegotiation) ->
    {EcPointFormats, EllipticCurves} =
	case advertises_ec_ciphers(lists:map(fun ssl_cipher:suite_definition/1, CipherSuites)) of
	    true ->
		client_ecc_extensions(tls_v1, Version);
	    false ->
		{undefined, undefined}
	end,
    SRP = srp_user(SslOpts),

    #hello_extensions{
       renegotiation_info = renegotiation_info(tls_record, client,
					       ConnectionStates, Renegotiation),
       srp = SRP,
       signature_algs = available_signature_algs(SupportedHashSigns, Version, AllVersions),
       ec_point_formats = EcPointFormats,
       elliptic_curves = EllipticCurves,
       alpn = encode_alpn(SslOpts#ssl_options.alpn_advertised_protocols, Renegotiation),
       next_protocol_negotiation =
	   encode_client_protocol_negotiation(SslOpts#ssl_options.next_protocol_selector,
					      Renegotiation),
       sni = sni(Host, SslOpts#ssl_options.server_name_indication)}.

%%--------------------------------------------------------------------
-spec certificate(der_cert(), db_handle(), certdb_ref(), client | server) -> #certificate{} | #alert{}.
%%
%% Description: Creates a certificate message.
%%--------------------------------------------------------------------
certificate(OwnCert, CertDbHandle, CertDbRef, client) ->
    Chain =
	case ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef) of
	    {ok, _,  CertChain} ->
		CertChain;
	    {error, _} ->
		%% If no suitable certificate is available, the client
		%% SHOULD send a certificate message containing no
		%% certificates. (chapter 7.4.6. RFC 4346)
		[]
	end,
    #certificate{asn1_certificates = Chain};

certificate(OwnCert, CertDbHandle, CertDbRef, server) ->
    case ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef) of
	{ok, _, Chain} ->
	    #certificate{asn1_certificates = Chain};
	{error, _} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, server_has_no_suitable_certificates)
    end.

%%--------------------------------------------------------------------
-spec next_protocol(binary()) -> #next_protocol{}.
%%
%% Description: Creates a next protocol message
%%-------------------------------------------------------------------
next_protocol(SelectedProtocol) ->
  #next_protocol{selected_protocol = SelectedProtocol}.

%%--------------------------------------------------------------------
-spec client_certificate_verify(undefined | der_cert(), binary(),
				ssl_record:ssl_version(), term(), public_key:private_key(),
				ssl_handshake_history()) ->
    #certificate_verify{} | ignore | #alert{}.
%%
%% Description: Creates a certificate_verify message, called by the client.
%%--------------------------------------------------------------------
client_certificate_verify(undefined, _, _, _, _, _) ->
    ignore;
client_certificate_verify(_, _, _, _, undefined, _) ->
    ignore;
client_certificate_verify(OwnCert, MasterSecret, Version,
			  {HashAlgo, SignAlgo},
			  PrivateKey, {Handshake, _}) ->
    case public_key:pkix_is_fixed_dh_cert(OwnCert) of
	true ->
            ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, fixed_diffie_hellman_prohibited);
	false ->
	    Hashes =
		calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
	    Signed = digitally_signed(Version, Hashes, HashAlgo, PrivateKey),
	    #certificate_verify{signature = Signed, hashsign_algorithm = {HashAlgo, SignAlgo}}
    end.

%%--------------------------------------------------------------------
-spec certificate_request(ssl_cipher:cipher_suite(), db_handle(), 
			  certdb_ref(),  #hash_sign_algos{}, ssl_record:ssl_version()) ->
				 #certificate_request{}.
%%
%% Description: Creates a certificate_request message, called by the server.
%%--------------------------------------------------------------------
certificate_request(CipherSuite, CertDbHandle, CertDbRef, HashSigns, Version) ->
    Types = certificate_types(ssl_cipher:suite_definition(CipherSuite), Version),
    Authorities = certificate_authorities(CertDbHandle, CertDbRef),
    #certificate_request{
		    certificate_types = Types,
		    hashsign_algorithms = HashSigns,
		    certificate_authorities = Authorities
		   }.
%%--------------------------------------------------------------------
-spec key_exchange(client | server, ssl_record:ssl_version(),
		   {premaster_secret, binary(), public_key_info()} |
		   {dh, binary()} |
		   {dh, {binary(), binary()}, #'DHParameter'{}, {HashAlgo::atom(), SignAlgo::atom()},
		   binary(), binary(), public_key:private_key()} |
		   {ecdh, #'ECPrivateKey'{}} |
		   {psk, binary()} |
		   {dhe_psk, binary(), binary()} |
		   {srp, {binary(), binary()}, #srp_user{}, {HashAlgo::atom(), SignAlgo::atom()},
		   binary(), binary(), public_key:private_key()}) ->
    #client_key_exchange{} | #server_key_exchange{}.

%%
%% Description: Creates a keyexchange message.
%%--------------------------------------------------------------------
key_exchange(client, _Version, {premaster_secret, Secret, {_, PublicKey, _}}) ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{exchange_keys = EncPremasterSecret};

key_exchange(client, _Version, {dh, PublicKey}) ->
    #client_key_exchange{
	      exchange_keys = #client_diffie_hellman_public{
		dh_public = PublicKey}
	       };

key_exchange(client, _Version, {ecdh, #'ECPrivateKey'{publicKey =  ECPublicKey}}) ->
    #client_key_exchange{
	      exchange_keys = #client_ec_diffie_hellman_public{
		dh_public = ECPublicKey}
	       };

key_exchange(client, _Version, {psk, Identity}) ->
    #client_key_exchange{
       exchange_keys = #client_psk_identity{
			  identity = Identity}
      };

key_exchange(client, _Version, {dhe_psk, Identity, PublicKey}) ->
    #client_key_exchange{
	      exchange_keys = #client_dhe_psk_identity{
		identity = Identity,
		dh_public = PublicKey}
	       };

key_exchange(client, _Version, {psk_premaster_secret, PskIdentity, Secret, {_, PublicKey, _}}) ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{
       exchange_keys = #client_rsa_psk_identity{
			  identity = PskIdentity,
			  exchange_keys = EncPremasterSecret}};

key_exchange(client, _Version, {srp, PublicKey}) ->
    #client_key_exchange{
       exchange_keys = #client_srp_public{
			  srp_a = PublicKey}
      };

key_exchange(server, Version, {dh, {PublicKey, _},
			       #'DHParameter'{prime = P, base = G},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerDHParams = #server_dh_params{dh_p = int_to_bin(P),
				       dh_g = int_to_bin(G), dh_y = PublicKey},
    enc_server_key_exchange(Version, ServerDHParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {ecdh,  #'ECPrivateKey'{publicKey =  ECPublicKey,
						      parameters = ECCurve}, HashSign,
			       ClientRandom, ServerRandom, PrivateKey}) ->
    ServerECParams = #server_ecdh_params{curve = ECCurve, public = ECPublicKey},
    enc_server_key_exchange(Version, ServerECParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {psk, PskIdentityHint,
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerPSKParams = #server_psk_params{hint = PskIdentityHint},
    enc_server_key_exchange(Version, ServerPSKParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {dhe_psk, PskIdentityHint, {PublicKey, _},
			       #'DHParameter'{prime = P, base = G},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerEDHPSKParams = #server_dhe_psk_params{
      hint = PskIdentityHint,
      dh_params = #server_dh_params{dh_p = int_to_bin(P),
				    dh_g = int_to_bin(G), dh_y = PublicKey}
     },
    enc_server_key_exchange(Version, ServerEDHPSKParams,
			    HashSign, ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {srp, {PublicKey, _},
			       #srp_user{generator = Generator, prime = Prime,
					 salt = Salt},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerSRPParams = #server_srp_params{srp_n = Prime, srp_g = Generator,
					 srp_s = Salt, srp_b = PublicKey},
    enc_server_key_exchange(Version, ServerSRPParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey).

%%--------------------------------------------------------------------
-spec finished(ssl_record:ssl_version(), client | server, integer(), binary(), ssl_handshake_history()) ->
    #finished{}.
%%
%% Description: Creates a handshake finished message
%%-------------------------------------------------------------------
finished(Version, Role, PrfAlgo, MasterSecret, {Handshake, _}) -> % use the current handshake
    #finished{verify_data =
	      calc_finished(Version, Role, PrfAlgo, MasterSecret, Handshake)}.

%% ---------- Handle handshake messages  ----------

verify_server_key(#server_key_params{params_bin = EncParams,
				     signature = Signature},
		  HashSign = {HashAlgo, _},
		  ConnectionStates, Version, PubKeyInfo) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Hash = server_key_exchange_hash(HashAlgo,
				    <<ClientRandom/binary,
				      ServerRandom/binary,
				      EncParams/binary>>),
    verify_signature(Version, Hash, HashSign, Signature, PubKeyInfo).

%%--------------------------------------------------------------------
-spec certificate_verify(binary(), public_key_info(), ssl_record:ssl_version(), term(),
			 binary(), ssl_handshake_history()) -> valid | #alert{}.
%%
%% Description: Checks that the certificate_verify message is valid.
%%--------------------------------------------------------------------
certificate_verify(_, _, _, undefined, _, _) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, invalid_certificate_verify_message);

certificate_verify(Signature, PublicKeyInfo, Version,
		   HashSign = {HashAlgo, _}, MasterSecret, {_, Handshake}) ->
    Hash = calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
    case verify_signature(Version, Hash, HashSign, Signature, PublicKeyInfo) of
	true ->
	    valid;
	_ ->
	    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE)
    end.
%%--------------------------------------------------------------------
-spec verify_signature(ssl_record:ssl_version(), binary(), {term(), term()}, binary(),
				   public_key_info()) -> true | false.
%%
%% Description: Checks that a public_key signature is valid.
%%--------------------------------------------------------------------
verify_signature(_Version, _Hash, {_HashAlgo, anon}, _Signature, _) ->
    true;
verify_signature({3, Minor}, Hash, {HashAlgo, rsa}, Signature, {?rsaEncryption, PubKey, _PubKeyParams})
  when Minor >= 3 ->
    public_key:verify({digest, Hash}, HashAlgo, Signature, PubKey);
verify_signature(_Version, Hash, _HashAlgo, Signature, {?rsaEncryption, PubKey, _PubKeyParams}) ->
    case public_key:decrypt_public(Signature, PubKey,
				   [{rsa_pad, rsa_pkcs1_padding}]) of
	Hash -> true;
	_    -> false
    end;
verify_signature(_Version, Hash, {HashAlgo, dsa}, Signature, {?'id-dsa', PublicKey, PublicKeyParams}) ->
    public_key:verify({digest, Hash}, HashAlgo, Signature, {PublicKey, PublicKeyParams});
verify_signature(_, Hash, {HashAlgo, _SignAlg}, Signature,
		 {?'id-ecPublicKey', PublicKey, PublicKeyParams}) ->
    public_key:verify({digest, Hash}, HashAlgo, Signature, {PublicKey, PublicKeyParams}).


%%--------------------------------------------------------------------
-spec certify(#certificate{}, db_handle(), certdb_ref(), integer() | nolimit,
	      verify_peer | verify_none, {fun(), term}, fun(), term(), term(),
	      client | server) ->  {der_cert(), public_key_info()} | #alert{}.
%%
%% Description: Handles a certificate handshake message
%%--------------------------------------------------------------------
certify(#certificate{asn1_certificates = ASN1Certs}, CertDbHandle, CertDbRef,
	MaxPathLen, _Verify, ValidationFunAndState0, PartialChain, CRLCheck, CRLDbHandle, Role) ->
    [PeerCert | _] = ASN1Certs,
        
    ValidationFunAndState = validation_fun_and_state(ValidationFunAndState0, Role, 
						     CertDbHandle, CertDbRef,  CRLCheck, CRLDbHandle),

    try
	{TrustedCert, CertPath}  =
	    ssl_certificate:trusted_cert_and_path(ASN1Certs, CertDbHandle, CertDbRef, PartialChain),
	case public_key:pkix_path_validation(TrustedCert,
					     CertPath,
					     [{max_path_length, MaxPathLen},
					      {verify_fun, ValidationFunAndState}]) of
	    {ok, {PublicKeyInfo,_}} ->
		{PeerCert, PublicKeyInfo};
	    {error, Reason} ->
		path_validation_alert(Reason)
	end
    catch
	error:_ ->
	    %% ASN-1 decode of certificate somehow failed
            ?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN, failed_to_decode_certificate)
    end.

%%--------------------------------------------------------------------
-spec verify_connection(ssl_record:ssl_version(), #finished{}, client | server, integer(), binary(),
			ssl_handshake_history()) -> verified | #alert{}.
%%
%% Description: Checks the ssl handshake finished message to verify
%%              the connection.
%%-------------------------------------------------------------------
verify_connection(Version, #finished{verify_data = Data},
		  Role, PrfAlgo, MasterSecret, {_, Handshake}) ->
    %% use the previous hashes
    case calc_finished(Version, Role, PrfAlgo, MasterSecret, Handshake) of
	Data ->
	    verified;
	_ ->
	    ?ALERT_REC(?FATAL, ?DECRYPT_ERROR)
    end.

%%--------------------------------------------------------------------
-spec init_handshake_history() -> ssl_handshake_history().

%%
%% Description: Initialize the empty handshake history buffer.
%%--------------------------------------------------------------------
init_handshake_history() ->
    {[], []}.

%%--------------------------------------------------------------------
-spec update_handshake_history(ssl_handshake:ssl_handshake_history(), Data ::term()) ->
				      ssl_handshake:ssl_handshake_history().
%%
%% Description: Update the handshake history buffer with Data.
%%--------------------------------------------------------------------
update_handshake_history(Handshake, % special-case SSL2 client hello
			 <<?CLIENT_HELLO, ?UINT24(_), ?BYTE(Major), ?BYTE(Minor),
			   ?UINT16(CSLength), ?UINT16(0),
			   ?UINT16(CDLength),
			   CipherSuites:CSLength/binary,
			   ChallengeData:CDLength/binary>>) ->
    update_handshake_history(Handshake,
			     <<?CLIENT_HELLO, ?BYTE(Major), ?BYTE(Minor),
			       ?UINT16(CSLength), ?UINT16(0),
			       ?UINT16(CDLength),
			       CipherSuites:CSLength/binary,
			       ChallengeData:CDLength/binary>>);
update_handshake_history({Handshake0, _Prev}, Data) ->
    {[Data|Handshake0], Handshake0}.

%% %%--------------------------------------------------------------------
%% -spec decrypt_premaster_secret(binary(), #'RSAPrivateKey'{}) -> binary().

%% %%
%% %% Description: Public key decryption using the private key.
%% %%--------------------------------------------------------------------
%% decrypt_premaster_secret(Secret, RSAPrivateKey) ->
%%     try public_key:decrypt_private(Secret, RSAPrivateKey,
%% 				   [{rsa_pad, rsa_pkcs1_padding}])
%%     catch
%% 	_:_ ->
%% 	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
%%     end.

premaster_secret(OtherPublicDhKey, MyPrivateKey, #'DHParameter'{} = Params) ->
    try 
	public_key:compute_key(OtherPublicDhKey, MyPrivateKey, Params)  
    catch 
	error:computation_failed -> 
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;	   
premaster_secret(PublicDhKey, PrivateDhKey, #server_dh_params{dh_p = Prime, dh_g = Base}) ->
    try 
	crypto:compute_key(dh, PublicDhKey, PrivateDhKey, [Prime, Base])
    catch 
	error:computation_failed -> 
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(#client_srp_public{srp_a = ClientPublicKey}, ServerKey, #srp_user{prime = Prime,
										   verifier = Verifier}) ->
    case crypto:compute_key(srp, ClientPublicKey, ServerKey, {host, [Verifier, Prime, '6a']}) of
	error ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));
	PremasterSecret ->
	    PremasterSecret
    end;
premaster_secret(#server_srp_params{srp_n = Prime, srp_g = Generator, srp_s = Salt, srp_b = Public},
		 ClientKeys, {Username, Password}) ->
    case ssl_srp_primes:check_srp_params(Generator, Prime) of
	ok ->
	    DerivedKey = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
	    case crypto:compute_key(srp, Public, ClientKeys, {user, [DerivedKey, Prime, Generator, '6a']}) of
		error ->
		    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));
		PremasterSecret ->
		    PremasterSecret
	    end;
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(#client_rsa_psk_identity{
		    identity = PSKIdentity,
		    exchange_keys = #encrypted_premaster_secret{premaster_secret = EncPMS}
		   }, #'RSAPrivateKey'{} = Key, PSKLookup) ->
    PremasterSecret = premaster_secret(EncPMS, Key),
    psk_secret(PSKIdentity, PSKLookup, PremasterSecret);
premaster_secret(#server_dhe_psk_params{
		    hint = IdentityHint,
		    dh_params =  #server_dh_params{dh_y = PublicDhKey} = Params},
		    PrivateDhKey,
		    LookupFun) ->
    PremasterSecret = premaster_secret(PublicDhKey, PrivateDhKey, Params),
    psk_secret(IdentityHint, LookupFun, PremasterSecret);
premaster_secret({rsa_psk, PSKIdentity}, PSKLookup, RSAPremasterSecret) ->
    psk_secret(PSKIdentity, PSKLookup, RSAPremasterSecret).

premaster_secret(#client_dhe_psk_identity{
		    identity =  PSKIdentity,
		    dh_public = PublicDhKey}, PrivateKey, #'DHParameter'{} = Params, PSKLookup) ->
    PremasterSecret = premaster_secret(PublicDhKey, PrivateKey, Params),
    psk_secret(PSKIdentity, PSKLookup, PremasterSecret).
premaster_secret(#client_psk_identity{identity = PSKIdentity}, PSKLookup) ->
    psk_secret(PSKIdentity, PSKLookup);
premaster_secret({psk, PSKIdentity}, PSKLookup) ->
    psk_secret(PSKIdentity, PSKLookup);
premaster_secret(#'ECPoint'{} = ECPoint, #'ECPrivateKey'{} = ECDHKeys) ->
    public_key:compute_key(ECPoint, ECDHKeys);
premaster_secret(EncSecret, #'RSAPrivateKey'{} = RSAPrivateKey) ->
    try public_key:decrypt_private(EncSecret, RSAPrivateKey,
				   [{rsa_pad, rsa_pkcs1_padding}])
    catch
	_:_ ->
	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
    end.
%%--------------------------------------------------------------------
-spec server_key_exchange_hash(md5sha | md5 | sha | sha224 |sha256 | sha384 | sha512, binary()) -> binary().
%%
%% Description: Calculate server key exchange hash
%%--------------------------------------------------------------------
server_key_exchange_hash(md5sha, Value) ->
    MD5 = crypto:hash(md5, Value),
    SHA = crypto:hash(sha, Value),
    <<MD5/binary, SHA/binary>>;

server_key_exchange_hash(Hash, Value) ->
    crypto:hash(Hash, Value).
%%--------------------------------------------------------------------
-spec prf(ssl_record:ssl_version(), non_neg_integer(), binary(), binary(), [binary()], non_neg_integer()) ->
		 {ok, binary()} | {error, undefined}.
%%
%% Description: use the TLS PRF to generate key material
%%--------------------------------------------------------------------
prf({3,0}, _, _, _, _, _) ->
    {error, undefined};
prf({3,_N}, PRFAlgo, Secret, Label, Seed, WantedLength) ->
    {ok, tls_v1:prf(PRFAlgo, Secret, Label, Seed, WantedLength)}.


%%--------------------------------------------------------------------
-spec select_hashsign(#hash_sign_algos{} | undefined,  undefined | binary(), 
		      atom(), [atom()], ssl_record:ssl_version()) ->
			     {atom(), atom()} | undefined  | #alert{}.

%%
%% Description: Handles signature_algorithms hello extension (server)
%%--------------------------------------------------------------------
select_hashsign(_, undefined, _,  _, _Version) ->
    {null, anon};
%% The signature_algorithms extension was introduced with TLS 1.2. Ignore it if we have
%% negotiated a lower version.
select_hashsign(HashSigns, Cert, KeyExAlgo, 
		undefined, {Major, Minor} = Version)  when Major >= 3 andalso Minor >= 3->
    select_hashsign(HashSigns, Cert, KeyExAlgo, tls_v1:default_signature_algs(Version), Version);
select_hashsign(#hash_sign_algos{hash_sign_algos = HashSigns}, Cert, KeyExAlgo, SupportedHashSigns,
		{Major, Minor}) when Major >= 3 andalso Minor >= 3 ->
    #'OTPCertificate'{tbsCertificate = TBSCert} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPCertificate'{tbsCertificate = TBSCert,
		      signatureAlgorithm =  {_,SignAlgo, _}} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_, SubjAlgo, _}} = 
     	TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,

    Sign = sign_algo(SignAlgo),
    SubSing = sign_algo(SubjAlgo),

    case lists:filter(fun({_, S} = Algos) when S == Sign ->
			      is_acceptable_hash_sign(Algos, Sign,
						      SubSing, KeyExAlgo, SupportedHashSigns);
			 (_)  ->
			      false
		      end, HashSigns) of
	[] ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm);
	[HashSign | _] ->
	    HashSign
    end;
select_hashsign(_, Cert, _, _, Version) ->
    #'OTPCertificate'{tbsCertificate = TBSCert} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_,Algo, _}} = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    select_hashsign_algs(undefined, Algo, Version).
%%--------------------------------------------------------------------
-spec select_hashsign(#certificate_request{},  binary(), 
		      [atom()], ssl_record:ssl_version()) ->
			     {atom(), atom()} | #alert{}.

%%
%% Description: Handles signature algorithms selection for certificate requests (client) 
%%--------------------------------------------------------------------
select_hashsign(#certificate_request{}, undefined, _, {Major, Minor})  when Major >= 3 andalso Minor >= 3->
    %% There client does not have a certificate and will send an empty reply, the server may fail 
    %% or accept the connection by its own preference. No signature algorihms needed as there is
    %% no certificate to verify.
    {undefined, undefined}; 
 
select_hashsign(#certificate_request{hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSigns}, 
				     certificate_types = Types}, Cert, SupportedHashSigns, 
		{Major, Minor})  when Major >= 3 andalso Minor >= 3->
    #'OTPCertificate'{tbsCertificate = TBSCert} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPCertificate'{tbsCertificate = TBSCert,
		      signatureAlgorithm =  {_,SignAlgo, _}} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_, SubjAlgo, _}} = 
     	TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,

    Sign = sign_algo(SignAlgo),
    SubSign = sign_algo(SubjAlgo),
    
    case is_acceptable_cert_type(SubSign, HashSigns, Types) andalso is_supported_sign(Sign, HashSigns) of
	true ->
	    case lists:filter(fun({_, S} = Algos) when S == SubSign ->
				 is_acceptable_hash_sign(Algos, SupportedHashSigns);
			    (_)  ->
				      false
			      end, HashSigns) of
		[] ->
		    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm);
		[HashSign | _] ->
		    HashSign
	    end;
	false ->
	    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)
    end;
select_hashsign(#certificate_request{}, Cert, _, Version) ->
    select_hashsign(undefined, Cert, undefined, undefined, Version).

%%--------------------------------------------------------------------
-spec select_hashsign_algs({atom(), atom()}| undefined, oid(), ssl_record:ssl_version()) ->
				  {atom(), atom()}.

%% Description: For TLS 1.2 hash function and signature algorithm pairs can be
%% negotiated with the signature_algorithms extension,
%% for previous versions always use appropriate defaults.
%% RFC 5246, Sect. 7.4.1.4.1.  Signature Algorithms
%% If the client does not send the signature_algorithms extension, the
%% server MUST do the following: (e.i defaults for TLS 1.2)
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

%%--------------------------------------------------------------------
select_hashsign_algs(HashSign, _, {Major, Minor}) when HashSign =/= undefined andalso
						       Major >= 3 andalso Minor >= 3 ->
    HashSign;
select_hashsign_algs(undefined, ?rsaEncryption, {Major, Minor}) when Major >= 3 andalso Minor >= 3 ->
    {sha, rsa};
select_hashsign_algs(undefined,?'id-ecPublicKey', _) ->
    {sha, ecdsa};
select_hashsign_algs(undefined, ?rsaEncryption, _) -> 
    {md5sha, rsa};
select_hashsign_algs(undefined, ?'id-dsa', _) ->
    {sha, dsa}.


%%--------------------------------------------------------------------
-spec master_secret(atom(), ssl_record:ssl_version(), #session{} | binary(), #connection_states{},
		   client | server) -> {binary(), #connection_states{}} | #alert{}.
%%
%% Description: Sets or calculates the master secret and calculate keys,
%% updating the pending connection states. The Mastersecret and the update
%% connection states are returned or an alert if the calculation fails.
%%-------------------------------------------------------------------
master_secret(RecordCB, Version, #session{master_secret = Mastersecret},
	      ConnectionStates, Role) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    try master_secret(RecordCB, Version, Mastersecret, SecParams,
		      ConnectionStates, Role)
    catch
	exit:_ ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, key_calculation_failure)
    end;

master_secret(RecordCB, Version, PremasterSecret, ConnectionStates, Role) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{prf_algorithm = PrfAlgo,
			 client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    try master_secret(RecordCB, Version,
		      calc_master_secret(Version,PrfAlgo,PremasterSecret,
					 ClientRandom, ServerRandom),
		      SecParams, ConnectionStates, Role)
    catch
	exit:_ ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, master_secret_calculation_failure)
    end.

%%-------------Encode/Decode --------------------------------
encode_handshake(#next_protocol{selected_protocol = SelectedProtocol}, _Version) ->
    PaddingLength = 32 - ((byte_size(SelectedProtocol) + 2) rem 32),
    {?NEXT_PROTOCOL, <<?BYTE((byte_size(SelectedProtocol))), SelectedProtocol/binary,
                         ?BYTE(PaddingLength), 0:(PaddingLength * 8)>>};

encode_handshake(#server_hello{server_version = {Major, Minor},
			       random = Random,
			       session_id = Session_ID,
			       cipher_suite = CipherSuite,
			       compression_method = Comp_method,
			       extensions = #hello_extensions{} = Extensions}, _Version) ->
			SID_length = byte_size(Session_ID),
    ExtensionsBin = encode_hello_extensions(Extensions),
    {?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SID_length), Session_ID/binary,
                     CipherSuite/binary, ?BYTE(Comp_method), ExtensionsBin/binary>>};
encode_handshake(#certificate{asn1_certificates = ASN1CertList}, _Version) ->
    ASN1Certs = certs_from_list(ASN1CertList),
    ACLen = erlang:iolist_size(ASN1Certs),
    {?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>};
encode_handshake(#server_key_exchange{exchange_keys = Keys}, _Version) ->
    {?SERVER_KEY_EXCHANGE, Keys};
encode_handshake(#server_key_params{params_bin = Keys, hashsign = HashSign,
			  signature = Signature}, Version) ->
    EncSign = enc_sign(HashSign, Signature, Version),
    {?SERVER_KEY_EXCHANGE, <<Keys/binary, EncSign/binary>>};
encode_handshake(#certificate_request{certificate_types = CertTypes,
			    hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSignAlgos},
			    certificate_authorities = CertAuths},
       {Major, Minor})
  when Major == 3, Minor >= 3 ->
    HashSigns= << <<(ssl_cipher:hash_algorithm(Hash)):8, (ssl_cipher:sign_algorithm(Sign)):8>> ||
		   {Hash, Sign} <- HashSignAlgos >>,
    CertTypesLen = byte_size(CertTypes),
    HashSignsLen = byte_size(HashSigns),
    CertAuthsLen = byte_size(CertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(HashSignsLen), HashSigns/binary,
	?UINT16(CertAuthsLen), CertAuths/binary>>
    };
encode_handshake(#certificate_request{certificate_types = CertTypes,
			    certificate_authorities = CertAuths},
       _Version) ->
    CertTypesLen = byte_size(CertTypes),
    CertAuthsLen = byte_size(CertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(CertAuthsLen), CertAuths/binary>>
    };
encode_handshake(#server_hello_done{}, _Version) ->
    {?SERVER_HELLO_DONE, <<>>};
encode_handshake(#client_key_exchange{exchange_keys = ExchangeKeys}, Version) ->
    {?CLIENT_KEY_EXCHANGE, encode_client_key(ExchangeKeys, Version)};
encode_handshake(#certificate_verify{signature = BinSig, hashsign_algorithm = HashSign}, Version) ->
    EncSig = enc_sign(HashSign, BinSig, Version),
    {?CERTIFICATE_VERIFY, EncSig};
encode_handshake(#finished{verify_data = VerifyData}, _Version) ->
    {?FINISHED, VerifyData}.

encode_hello_extensions(#hello_extensions{} = Extensions) ->
    encode_hello_extensions(hello_extensions_list(Extensions), <<>>).
encode_hello_extensions([], <<>>) ->
    <<>>;
encode_hello_extensions([], Acc) ->
    Size = byte_size(Acc),
    <<?UINT16(Size), Acc/binary>>;

encode_hello_extensions([#alpn{extension_data = ExtensionData} | Rest], Acc) ->
	Len = byte_size(ExtensionData),
    ExtLen = Len + 2,
	encode_hello_extensions(Rest, <<?UINT16(?ALPN_EXT), ?UINT16(ExtLen), ?UINT16(Len),
					ExtensionData/binary, Acc/binary>>);
encode_hello_extensions([#next_protocol_negotiation{extension_data = ExtensionData} | Rest], Acc) ->
    Len = byte_size(ExtensionData),
    encode_hello_extensions(Rest, <<?UINT16(?NEXTPROTONEG_EXT), ?UINT16(Len),
				    ExtensionData/binary, Acc/binary>>);
encode_hello_extensions([#renegotiation_info{renegotiated_connection = undefined} | Rest], Acc) ->
    encode_hello_extensions(Rest, Acc);
encode_hello_extensions([#renegotiation_info{renegotiated_connection = ?byte(0) = Info} | Rest], Acc) ->
    Len = byte_size(Info),
    encode_hello_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), Info/binary, Acc/binary>>);

encode_hello_extensions([#renegotiation_info{renegotiated_connection = Info} | Rest], Acc) ->
    InfoLen = byte_size(Info),
    Len = InfoLen +1,
    encode_hello_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), ?BYTE(InfoLen),
				    Info/binary, Acc/binary>>);
encode_hello_extensions([#elliptic_curves{elliptic_curve_list = EllipticCurves} | Rest], Acc) ->

    EllipticCurveList = << <<(tls_v1:oid_to_enum(X)):16>> || X <- EllipticCurves>>,
    ListLen = byte_size(EllipticCurveList),
    Len = ListLen + 2,
    encode_hello_extensions(Rest, <<?UINT16(?ELLIPTIC_CURVES_EXT),
				 ?UINT16(Len), ?UINT16(ListLen), EllipticCurveList/binary, Acc/binary>>);
encode_hello_extensions([#ec_point_formats{ec_point_format_list = ECPointFormats} | Rest], Acc) ->
    ECPointFormatList = list_to_binary(ECPointFormats),
    ListLen = byte_size(ECPointFormatList),
    Len = ListLen + 1,
    encode_hello_extensions(Rest, <<?UINT16(?EC_POINT_FORMATS_EXT),
				 ?UINT16(Len), ?BYTE(ListLen), ECPointFormatList/binary, Acc/binary>>);
encode_hello_extensions([#srp{username = UserName} | Rest], Acc) ->
    SRPLen = byte_size(UserName),
    Len = SRPLen + 2,
    encode_hello_extensions(Rest, <<?UINT16(?SRP_EXT), ?UINT16(Len), ?BYTE(SRPLen),
				    UserName/binary, Acc/binary>>);
encode_hello_extensions([#hash_sign_algos{hash_sign_algos = HashSignAlgos} | Rest], Acc) ->
    SignAlgoList = << <<(ssl_cipher:hash_algorithm(Hash)):8, (ssl_cipher:sign_algorithm(Sign)):8>> ||
		       {Hash, Sign} <- HashSignAlgos >>,
    ListLen = byte_size(SignAlgoList),
    Len = ListLen + 2,
    encode_hello_extensions(Rest, <<?UINT16(?SIGNATURE_ALGORITHMS_EXT),
				 ?UINT16(Len), ?UINT16(ListLen), SignAlgoList/binary, Acc/binary>>);
encode_hello_extensions([#sni{hostname = Hostname} | Rest], Acc) ->
    HostLen = length(Hostname),
    HostnameBin = list_to_binary(Hostname),
    % Hostname type (1 byte) + Hostname length (2 bytes) + Hostname (HostLen bytes)
    ServerNameLength = 1 + 2 + HostLen,
    % ServerNameListSize (2 bytes) + ServerNameLength
    ExtLength = 2 + ServerNameLength,
    encode_hello_extensions(Rest, <<?UINT16(?SNI_EXT), ?UINT16(ExtLength),
				    ?UINT16(ServerNameLength),
				    ?BYTE(?SNI_NAMETYPE_HOST_NAME),
				    ?UINT16(HostLen), HostnameBin/binary,
				    Acc/binary>>).

enc_server_key_exchange(Version, Params, {HashAlgo, SignAlgo},
			ClientRandom, ServerRandom, PrivateKey) ->
    EncParams = encode_server_key(Params),
    case HashAlgo of
	null ->
	    #server_key_params{params = Params,
			       params_bin = EncParams,
			       hashsign = {null, anon},
			       signature = <<>>};
	_ ->
	    Hash =
		server_key_exchange_hash(HashAlgo, <<ClientRandom/binary,
						     ServerRandom/binary,
						     EncParams/binary>>),
	    Signature = digitally_signed(Version, Hash, HashAlgo, PrivateKey),
	    #server_key_params{params = Params,
			       params_bin = EncParams,
			       hashsign = {HashAlgo, SignAlgo},
			       signature = Signature}
    end.

%%--------------------------------------------------------------------
-spec decode_client_key(binary(), ssl_cipher:key_algo(), ssl_record:ssl_version()) ->
			    #encrypted_premaster_secret{}
			    | #client_diffie_hellman_public{}
			    | #client_ec_diffie_hellman_public{}
			    | #client_psk_identity{}
			    | #client_dhe_psk_identity{}
			    | #client_rsa_psk_identity{}
			    | #client_srp_public{}.
%%
%% Description: Decode client_key data and return appropriate type
%%--------------------------------------------------------------------
decode_client_key(ClientKey, Type, Version) ->
    dec_client_key(ClientKey, key_exchange_alg(Type), Version).

%%--------------------------------------------------------------------
-spec decode_server_key(binary(), ssl_cipher:key_algo(), ssl_record:ssl_version()) ->
			       #server_key_params{}.
%%
%% Description: Decode server_key data and return appropriate type
%%--------------------------------------------------------------------
decode_server_key(ServerKey, Type, Version) ->
    dec_server_key(ServerKey, key_exchange_alg(Type), Version).

%%
%% Description: Encode and decode functions for ALPN extension data.
%%--------------------------------------------------------------------

%% While the RFC opens the door to allow ALPN during renegotiation, in practice
%% this does not work and it is recommended to ignore any ALPN extension during
%% renegotiation, as done here.
encode_alpn(_, true) ->
    undefined;
encode_alpn(undefined, _) ->
    undefined;
encode_alpn(Protocols, _) ->
    #alpn{extension_data = lists:foldl(fun encode_protocol/2, <<>>, Protocols)}.

decode_alpn(undefined) ->
    undefined;
decode_alpn(#alpn{extension_data=Data}) ->
    decode_protocols(Data, []).

encode_client_protocol_negotiation(undefined, _) ->
    undefined;
encode_client_protocol_negotiation(_, false) ->
	#next_protocol_negotiation{extension_data = <<>>};
encode_client_protocol_negotiation(_, _) ->
	undefined.

encode_protocols_advertised_on_server(undefined) ->
	undefined;

encode_protocols_advertised_on_server(Protocols) ->
	#next_protocol_negotiation{extension_data = lists:foldl(fun encode_protocol/2, <<>>, Protocols)}.

decode_handshake(_, ?HELLO_REQUEST, <<>>) ->
    #hello_request{};
decode_handshake(_, ?NEXT_PROTOCOL, <<?BYTE(SelectedProtocolLength),
				      SelectedProtocol:SelectedProtocolLength/binary,
				      ?BYTE(PaddingLength), _Padding:PaddingLength/binary>>) ->
    #next_protocol{selected_protocol = SelectedProtocol};

decode_handshake(_Version, ?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
					    ?BYTE(SID_length), Session_ID:SID_length/binary,
					    Cipher_suite:2/binary, ?BYTE(Comp_method)>>) ->
    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       compression_method = Comp_method,
       extensions = #hello_extensions{}};

decode_handshake(_Version, ?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method),
		       ?UINT16(ExtLen), Extensions:ExtLen/binary>>) ->

    HelloExtensions = decode_hello_extensions(Extensions),

    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       compression_method = Comp_method,
       extensions = HelloExtensions};

decode_handshake(_Version, ?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>) ->
    #certificate{asn1_certificates = certs_to_list(ASN1Certs)};
decode_handshake(_Version, ?SERVER_KEY_EXCHANGE, Keys) ->
    #server_key_exchange{exchange_keys = Keys};
decode_handshake({Major, Minor}, ?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(HashSignsLen), HashSigns:HashSignsLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>)
  when Major >= 3, Minor >= 3 ->
    HashSignAlgos = [{ssl_cipher:hash_algorithm(Hash), ssl_cipher:sign_algorithm(Sign)} ||
			<<?BYTE(Hash), ?BYTE(Sign)>> <= HashSigns],
    #certificate_request{certificate_types = CertTypes,
			 hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSignAlgos},
			 certificate_authorities = CertAuths};
decode_handshake(_Version, ?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>) ->
    #certificate_request{certificate_types = CertTypes,
			 certificate_authorities = CertAuths};
decode_handshake(_Version, ?SERVER_HELLO_DONE, <<>>) ->
    #server_hello_done{};
decode_handshake({Major, Minor}, ?CERTIFICATE_VERIFY,<<HashSign:2/binary, ?UINT16(SignLen),
						       Signature:SignLen/binary>>)
  when Major == 3, Minor >= 3 ->
    #certificate_verify{hashsign_algorithm = dec_hashsign(HashSign), signature = Signature};
decode_handshake(_Version, ?CERTIFICATE_VERIFY,<<?UINT16(SignLen), Signature:SignLen/binary>>)->
    #certificate_verify{signature = Signature};
decode_handshake(_Version, ?CLIENT_KEY_EXCHANGE, PKEPMS) ->
    #client_key_exchange{exchange_keys = PKEPMS};
decode_handshake(_Version, ?FINISHED, VerifyData) ->
    #finished{verify_data = VerifyData};
decode_handshake(_, Message, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {unknown_or_malformed_handshake, Message})).

%%--------------------------------------------------------------------
-spec decode_hello_extensions({client, binary()} | binary()) -> #hello_extensions{}.
%%
%% Description: Decodes TLS hello extensions
%%--------------------------------------------------------------------
decode_hello_extensions({client, <<>>}) ->
    #hello_extensions{};
decode_hello_extensions({client, <<?UINT16(ExtLen), Extensions:ExtLen/binary>>}) ->
    decode_hello_extensions(Extensions);
decode_hello_extensions(Extensions) ->
    dec_hello_extensions(Extensions, #hello_extensions{}).

dec_server_key(<<?UINT16(PLen), P:PLen/binary,
		 ?UINT16(GLen), G:GLen/binary,
		 ?UINT16(YLen), Y:YLen/binary, _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, Version) ->
    Params = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y},
    {BinMsg, HashSign, Signature} = dec_server_key_params(PLen + GLen + YLen + 6, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
%% ECParameters with named_curve
%% TODO: explicit curve
dec_server_key(<<?BYTE(?NAMED_CURVE), ?UINT16(CurveID),
		 ?BYTE(PointLen), ECPoint:PointLen/binary,
		 _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN, Version) ->
    Params = #server_ecdh_params{curve = {namedCurve, tls_v1:enum_to_oid(CurveID)},
				 public = ECPoint},
    {BinMsg, HashSign, Signature} = dec_server_key_params(PointLen + 4, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(Len), PskIdentityHint:Len/binary, _/binary>> = KeyStruct,
	       KeyExchange, Version)
  when KeyExchange == ?KEY_EXCHANGE_PSK; KeyExchange == ?KEY_EXCHANGE_RSA_PSK ->
    Params = #server_psk_params{
		hint = PskIdentityHint},
    {BinMsg, HashSign, Signature} = dec_server_key_params(Len + 2, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(Len), IdentityHint:Len/binary,
		 ?UINT16(PLen), P:PLen/binary,
		 ?UINT16(GLen), G:GLen/binary,
		 ?UINT16(YLen), Y:YLen/binary, _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_DHE_PSK, Version) ->
    DHParams = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y},
    Params = #server_dhe_psk_params{
      hint = IdentityHint,
      dh_params = DHParams},
    {BinMsg, HashSign, Signature} = dec_server_key_params(Len + PLen + GLen + YLen + 8, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(NLen), N:NLen/binary,
		 ?UINT16(GLen), G:GLen/binary,
		 ?BYTE(SLen), S:SLen/binary,
		 ?UINT16(BLen), B:BLen/binary, _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_SRP, Version) ->
    Params = #server_srp_params{srp_n = N, srp_g = G, srp_s = S, srp_b = B},
    {BinMsg, HashSign, Signature} = dec_server_key_params(NLen + GLen + SLen + BLen + 7, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(_, KeyExchange, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {unknown_or_malformed_key_exchange, KeyExchange})).

%%--------------------------------------------------------------------
-spec decode_suites('2_bytes'|'3_bytes', binary()) -> list().
%%
%% Description:
%%--------------------------------------------------------------------
decode_suites('2_bytes', Dec) ->
    from_2bytes(Dec);
decode_suites('3_bytes', Dec) ->
    from_3bytes(Dec).

%%-------------Cipeher suite handling --------------------------------

available_suites(UserSuites, Version) ->
    lists:filtermap(fun(Suite) ->
			    lists:member(Suite, ssl_cipher:all_suites(Version))
		    end, UserSuites).

available_suites(ServerCert, UserSuites, Version, undefined, Curve) ->
    ssl_cipher:filter(ServerCert, available_suites(UserSuites, Version))
	-- unavailable_ecc_suites(Curve);
available_suites(ServerCert, UserSuites, Version, HashSigns, Curve) ->
    Suites = available_suites(ServerCert, UserSuites, Version, undefined, Curve),
    filter_hashsigns(Suites, [ssl_cipher:suite_definition(Suite) || Suite <- Suites], HashSigns, []).
filter_hashsigns([], [], _, Acc) ->
    lists:reverse(Acc);
filter_hashsigns([Suite | Suites], [{KeyExchange,_,_,_} | Algos], HashSigns,
		 Acc) when KeyExchange == dhe_ecdsa;
			   KeyExchange == ecdhe_ecdsa ->
    do_filter_hashsigns(ecdsa, Suite, Suites, Algos, HashSigns, Acc);

filter_hashsigns([Suite | Suites], [{KeyExchange,_,_,_} | Algos], HashSigns,
		 Acc) when KeyExchange == rsa;
			   KeyExchange == dhe_rsa;
			   KeyExchange == ecdhe_rsa;
			   KeyExchange == srp_rsa;
			   KeyExchange == rsa_psk ->
    do_filter_hashsigns(rsa, Suite, Suites, Algos, HashSigns, Acc);
filter_hashsigns([Suite | Suites], [{KeyExchange,_,_,_} | Algos], HashSigns, Acc) when 
      KeyExchange == dhe_dss;
      KeyExchange == srp_dss ->							       
    do_filter_hashsigns(dsa, Suite, Suites, Algos, HashSigns, Acc);
filter_hashsigns([Suite | Suites], [{KeyExchange,_,_,_} | Algos], HashSigns, Acc) when 
      KeyExchange == dh_dss; 
      KeyExchange == dh_rsa; 
      KeyExchange == dh_ecdsa;
      KeyExchange == ecdh_rsa;    
      KeyExchange == ecdh_ecdsa ->
      %%  Fixed DH certificates MAY be signed with any hash/signature
      %%  algorithm pair appearing in the hash_sign extension.  The names
    %%  DH_DSS, DH_RSA, ECDH_ECDSA, and ECDH_RSA are historical.
    filter_hashsigns(Suites, Algos, HashSigns, [Suite| Acc]);
filter_hashsigns([Suite | Suites], [{KeyExchange,_,_,_} | Algos], HashSigns, Acc) when 
      KeyExchange == dh_anon;
      KeyExchange == ecdh_anon;
      KeyExchange == srp_anon;
      KeyExchange == psk;
      KeyExchange == dhe_psk ->
    %% In this case hashsigns is not used as the kexchange is anonaymous
    filter_hashsigns(Suites, Algos, HashSigns, [Suite| Acc]).

do_filter_hashsigns(SignAlgo, Suite, Suites, Algos, HashSigns, Acc) ->
    case lists:keymember(SignAlgo, 2, HashSigns) of
	true ->
	    filter_hashsigns(Suites, Algos, HashSigns, [Suite| Acc]);
	false ->
	    filter_hashsigns(Suites, Algos, HashSigns, Acc)
    end.

unavailable_ecc_suites(no_curve) ->
    ssl_cipher:ec_keyed_suites();
unavailable_ecc_suites(_) ->
    [].

cipher_suites(Suites, false) ->
    [?TLS_EMPTY_RENEGOTIATION_INFO_SCSV | Suites];
cipher_suites(Suites, true) ->
    Suites.

select_session(SuggestedSessionId, CipherSuites, HashSigns, Compressions, Port, #session{ecc = ECCCurve} = 
		   Session, Version,
	       #ssl_options{ciphers = UserSuites, honor_cipher_order = HonorCipherOrder} = SslOpts,
	       Cache, CacheCb, Cert) ->
    {SessionId, Resumed} = ssl_session:server_id(Port, SuggestedSessionId,
						 SslOpts, Cert,
						 Cache, CacheCb),
    case Resumed of
        undefined ->
	    Suites = available_suites(Cert, UserSuites, Version, HashSigns, ECCCurve),
	    CipherSuite = select_cipher_suite(CipherSuites, Suites, HonorCipherOrder),
	    Compression = select_compression(Compressions),
	    {new, Session#session{session_id = SessionId,
				  cipher_suite = CipherSuite,
				  compression_method = Compression}};
	_ ->
	    {resumed, Resumed}
    end.

supported_ecc({Major, Minor} = Version) when ((Major == 3) and (Minor >= 1)) orelse (Major > 3) ->
    Curves = tls_v1:ecc_curves(Version),
    #elliptic_curves{elliptic_curve_list = Curves};
supported_ecc(_) ->
    #elliptic_curves{elliptic_curve_list = []}.

%%-------------certificate handling --------------------------------

certificate_types(_, {N, M}) when N >= 3 andalso M >= 3 ->
    case proplists:get_bool(ecdsa,  
			    proplists:get_value(public_keys, crypto:supports())) of
	true ->
	    <<?BYTE(?ECDSA_SIGN), ?BYTE(?RSA_SIGN), ?BYTE(?DSS_SIGN)>>;
	false ->
	    <<?BYTE(?RSA_SIGN), ?BYTE(?DSS_SIGN)>>
    end;

certificate_types({KeyExchange, _, _, _}, _) when KeyExchange == rsa;
						  KeyExchange == dh_rsa;
						  KeyExchange == dhe_rsa;
						  KeyExchange == ecdhe_rsa ->
    <<?BYTE(?RSA_SIGN)>>;

certificate_types({KeyExchange, _, _, _}, _)  when KeyExchange == dh_dss; 
						   KeyExchange == dhe_dss;
						   KeyExchange == srp_dss ->
    <<?BYTE(?DSS_SIGN)>>;

certificate_types({KeyExchange, _, _, _}, _) when KeyExchange == dh_ecdsa;
						  KeyExchange == dhe_ecdsa;
						  KeyExchange == ecdh_ecdsa;
						  KeyExchange == ecdhe_ecdsa ->
    <<?BYTE(?ECDSA_SIGN)>>;

certificate_types(_, _) ->
    <<?BYTE(?RSA_SIGN)>>.

certificate_authorities(CertDbHandle, CertDbRef) ->
    Authorities = certificate_authorities_from_db(CertDbHandle, CertDbRef),
    Enc = fun(#'OTPCertificate'{tbsCertificate=TBSCert}) ->
		  OTPSubj = TBSCert#'OTPTBSCertificate'.subject,
		  DNEncodedBin = public_key:pkix_encode('Name', OTPSubj, otp),
		  DNEncodedLen = byte_size(DNEncodedBin),
		  <<?UINT16(DNEncodedLen), DNEncodedBin/binary>>
	  end,
    list_to_binary([Enc(Cert) || {_, Cert} <- Authorities]).

certificate_authorities_from_db(CertDbHandle, CertDbRef) ->
    ConnectionCerts = fun({{Ref, _, _}, Cert}, Acc) when Ref  == CertDbRef ->
			      [Cert | Acc];
			 (_, Acc) ->
			      Acc
		      end,
    ssl_pkix_db:foldl(ConnectionCerts, [], CertDbHandle).

%%-------------Extension handling --------------------------------

handle_client_hello_extensions(RecordCB, Random, ClientCipherSuites,
			       #hello_extensions{renegotiation_info = Info,
						 srp = SRP,
						 ec_point_formats = ECCFormat,
						 alpn = ALPN,
						 next_protocol_negotiation = NextProtocolNegotiation}, Version,
			       #ssl_options{secure_renegotiate = SecureRenegotation,
                                            alpn_preferred_protocols = ALPNPreferredProtocols} = Opts,
			       #session{cipher_suite = NegotiatedCipherSuite,
					compression_method = Compression} = Session0,
			       ConnectionStates0, Renegotiation) ->
    Session = handle_srp_extension(SRP, Session0),
    ConnectionStates = handle_renegotiation_extension(server, RecordCB, Version, Info,
						      Random, NegotiatedCipherSuite, 
						      ClientCipherSuites, Compression,
						      ConnectionStates0, Renegotiation, SecureRenegotation),

    ServerHelloExtensions =  #hello_extensions{
				renegotiation_info = renegotiation_info(RecordCB, server,
									ConnectionStates, Renegotiation),
				ec_point_formats = server_ecc_extension(Version, ECCFormat)
			       },

    %% If we receive an ALPN extension and have ALPN configured for this connection,
    %% we handle it. Otherwise we check for the NPN extension.
    if
        ALPN =/= undefined, ALPNPreferredProtocols =/= undefined ->
			case handle_alpn_extension(ALPNPreferredProtocols, decode_alpn(ALPN)) of
                #alert{} = Alert ->
                    Alert;
                Protocol ->
                    {Session, ConnectionStates, Protocol,
                        ServerHelloExtensions#hello_extensions{alpn=encode_alpn([Protocol], Renegotiation)}}
            end;
        true ->
            ProtocolsToAdvertise = handle_next_protocol_extension(NextProtocolNegotiation, Renegotiation, Opts),
            {Session, ConnectionStates, undefined,
				ServerHelloExtensions#hello_extensions{next_protocol_negotiation=
                	encode_protocols_advertised_on_server(ProtocolsToAdvertise)}}
    end.

handle_server_hello_extensions(RecordCB, Random, CipherSuite, Compression,
			       #hello_extensions{renegotiation_info = Info,
                                                 alpn = ALPN,
						 next_protocol_negotiation = NextProtocolNegotiation}, Version,
			       #ssl_options{secure_renegotiate = SecureRenegotation,
					    next_protocol_selector = NextProtoSelector},
			       ConnectionStates0, Renegotiation) ->
    ConnectionStates = handle_renegotiation_extension(client, RecordCB, Version, Info, Random, 
						      CipherSuite, undefined,
						      Compression, ConnectionStates0,
						      Renegotiation, SecureRenegotation),

    %% If we receive an ALPN extension then this is the protocol selected,
    %% otherwise handle the NPN extension.
    case decode_alpn(ALPN) of
        %% ServerHello contains exactly one protocol: the one selected.
        %% We also ignore the ALPN extension during renegotiation (see encode_alpn/2).
        [Protocol] when not Renegotiation ->
            {ConnectionStates, alpn, Protocol};
        undefined ->
            case handle_next_protocol(NextProtocolNegotiation, NextProtoSelector, Renegotiation) of
                #alert{} = Alert ->
                    Alert;
                Protocol ->
                    {ConnectionStates, npn, Protocol}
            end;
        {error, Reason} ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason);
        [] ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, no_protocols_in_server_hello);
        [_|_] ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, too_many_protocols_in_server_hello)
    end.

select_version(RecordCB, ClientVersion, Versions) ->
    do_select_version(RecordCB, ClientVersion, Versions).

do_select_version(_, ClientVersion, []) ->
    ClientVersion;
do_select_version(RecordCB, ClientVersion, [Version | Versions]) ->
    case RecordCB:is_higher(Version, ClientVersion) of
	true ->
	    %% Version too high for client - keep looking
	    do_select_version(RecordCB, ClientVersion, Versions);
	false ->
	    %% Version ok for client - look for a higher
	    do_select_version(RecordCB, ClientVersion, Versions, Version)
    end.
%%
do_select_version(_, _, [], GoodVersion) ->
    GoodVersion;
do_select_version(
  RecordCB, ClientVersion, [Version | Versions], GoodVersion) ->
    BetterVersion =
	case RecordCB:is_higher(Version, ClientVersion) of
	    true ->
		%% Version too high for client
		GoodVersion;
	    false ->
		%% Version ok for client
		case RecordCB:is_higher(Version, GoodVersion) of
		    true ->
			%% Use higher version
			Version;
		    false ->
			GoodVersion
		end
	end,
    do_select_version(RecordCB, ClientVersion, Versions, BetterVersion).

renegotiation_info(_, client, _, false) ->
    #renegotiation_info{renegotiated_connection = undefined};
renegotiation_info(_RecordCB, server, ConnectionStates, false) ->
    CS  = ssl_record:current_connection_state(ConnectionStates, read),
    case CS#connection_state.secure_renegotiation of
	true ->
	    #renegotiation_info{renegotiated_connection = ?byte(0)};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;
renegotiation_info(_RecordCB, client, ConnectionStates, true) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    case CS#connection_state.secure_renegotiation of
	true ->
	    Data = CS#connection_state.client_verify_data,
	    #renegotiation_info{renegotiated_connection = Data};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;

renegotiation_info(_RecordCB, server, ConnectionStates, true) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    case CS#connection_state.secure_renegotiation of
	true ->
	    CData = CS#connection_state.client_verify_data,
	    SData  =CS#connection_state.server_verify_data,
	    #renegotiation_info{renegotiated_connection = <<CData/binary, SData/binary>>};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end.

handle_renegotiation_info(_RecordCB, _, #renegotiation_info{renegotiated_connection = ?byte(0)},
			  ConnectionStates, false, _, _) ->
    {ok, ssl_record:set_renegotiation_flag(true, ConnectionStates)};

handle_renegotiation_info(_RecordCB, server, undefined, ConnectionStates, _, _, CipherSuites) ->
    case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	true ->
	    {ok, ssl_record:set_renegotiation_flag(true, ConnectionStates)};
	false ->
	    {ok, ssl_record:set_renegotiation_flag(false, ConnectionStates)}
    end;

handle_renegotiation_info(_RecordCB, _, undefined, ConnectionStates, false, _, _) ->
    {ok, ssl_record:set_renegotiation_flag(false, ConnectionStates)};

handle_renegotiation_info(_RecordCB, client, #renegotiation_info{renegotiated_connection = ClientServerVerify},
			  ConnectionStates, true, _, _) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    CData = CS#connection_state.client_verify_data,
    SData = CS#connection_state.server_verify_data,
    case <<CData/binary, SData/binary>> == ClientServerVerify of
	true ->
	    {ok, ConnectionStates};
	false ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, client_renegotiation)
    end;
handle_renegotiation_info(_RecordCB, server, #renegotiation_info{renegotiated_connection = ClientVerify},
			  ConnectionStates, true, _, CipherSuites) ->

      case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
              ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {server_renegotiation, empty_renegotiation_info_scsv});
	  false ->
	      CS = ssl_record:current_connection_state(ConnectionStates, read),
	      Data = CS#connection_state.client_verify_data,
	      case Data == ClientVerify of
		  true ->
		      {ok, ConnectionStates};
		  false ->
                      ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, server_renegotiation)
	      end
      end;

handle_renegotiation_info(RecordCB, client, undefined, ConnectionStates, true, SecureRenegotation, _) ->
    handle_renegotiation_info(RecordCB, ConnectionStates, SecureRenegotation);

handle_renegotiation_info(RecordCB, server, undefined, ConnectionStates, true, SecureRenegotation, CipherSuites) ->
     case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
             ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {server_renegotiation, empty_renegotiation_info_scsv});
	 false ->
	     handle_renegotiation_info(RecordCB, ConnectionStates, SecureRenegotation)
     end.

handle_renegotiation_info(_RecordCB, ConnectionStates, SecureRenegotation) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    case {SecureRenegotation, CS#connection_state.secure_renegotiation} of
	{_, true} ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, already_secure);
	{true, false} ->
	    ?ALERT_REC(?FATAL, ?NO_RENEGOTIATION);
	{false, false} ->
	    {ok, ConnectionStates}
    end.

hello_extensions_list(#hello_extensions{renegotiation_info = RenegotiationInfo,
					srp = SRP,
					signature_algs = HashSigns,
					ec_point_formats = EcPointFormats,
					elliptic_curves = EllipticCurves,
                                        alpn = ALPN,
					next_protocol_negotiation = NextProtocolNegotiation,
					sni = Sni}) ->
    [Ext || Ext <- [RenegotiationInfo, SRP, HashSigns,
		    EcPointFormats, EllipticCurves, ALPN, NextProtocolNegotiation, Sni], Ext =/= undefined].

srp_user(#ssl_options{srp_identity = {UserName, _}}) ->
    #srp{username = UserName};
srp_user(_) ->
    undefined.

client_ecc_extensions(Module, Version) ->
    CryptoSupport = proplists:get_value(public_keys, crypto:supports()),
    case proplists:get_bool(ecdh, CryptoSupport) of
	true ->
	    EcPointFormats = #ec_point_formats{ec_point_format_list = [?ECPOINT_UNCOMPRESSED]},
	    EllipticCurves = #elliptic_curves{elliptic_curve_list = Module:ecc_curves(Version)},
	    {EcPointFormats, EllipticCurves};
	_ ->
	    {undefined, undefined}
    end.

server_ecc_extension(_Version, EcPointFormats) ->
    CryptoSupport = proplists:get_value(public_keys, crypto:supports()),
    case proplists:get_bool(ecdh, CryptoSupport) of
	true ->
	    handle_ecc_point_fmt_extension(EcPointFormats);
	false ->
	    undefined
    end.

handle_ecc_point_fmt_extension(undefined) ->
    undefined;
handle_ecc_point_fmt_extension(_) ->
    #ec_point_formats{ec_point_format_list = [?ECPOINT_UNCOMPRESSED]}.

advertises_ec_ciphers([]) ->
    false;
advertises_ec_ciphers([{ecdh_ecdsa, _,_,_} | _]) ->
    true;
advertises_ec_ciphers([{ecdhe_ecdsa, _,_,_} | _]) ->
    true;
advertises_ec_ciphers([{ecdh_rsa, _,_,_} | _]) ->
    true;
advertises_ec_ciphers([{ecdhe_rsa, _,_,_} | _]) ->
    true;
advertises_ec_ciphers([{ecdh_anon, _,_,_} | _]) ->
    true;
advertises_ec_ciphers([_| Rest]) ->
    advertises_ec_ciphers(Rest).
select_curve(#elliptic_curves{elliptic_curve_list = ClientCurves}, 
	     #elliptic_curves{elliptic_curve_list = ServerCurves}) -> 
    select_curve(ClientCurves, ServerCurves);
select_curve(undefined, _) ->
    %% Client did not send ECC extension use default curve if 
    %% ECC cipher is negotiated
    {namedCurve, ?secp256r1};
select_curve(_, []) ->
    no_curve;
select_curve(Curves, [Curve| Rest]) ->
    case lists:member(Curve, Curves) of
	true ->
	    {namedCurve, Curve};
	false ->
	    select_curve(Curves, Rest)
    end.
%% RFC 6066, Section 3: Currently, the only server names supported are
%% DNS hostnames
sni(_, disable) ->
    undefined;
sni(Host, undefined) ->
    sni1(Host);
sni(_Host, SNIOption) ->
    sni1(SNIOption).

sni1(Hostname) ->
    case inet_parse:domain(Hostname) of
        false -> undefined;
        true -> #sni{hostname = Hostname}
    end.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
validation_fun_and_state({Fun, UserState0}, Role,  CertDbHandle, CertDbRef, CRLCheck, CRLDbHandle) ->
    {fun(OtpCert, {extension, _} = Extension, {SslState, UserState}) ->
	     case ssl_certificate:validate(OtpCert,
					   Extension,
					   SslState) of
		 {valid, NewSslState} ->
		     {valid, {NewSslState, UserState}};
		 {fail, Reason} ->
		     apply_user_fun(Fun, OtpCert, Reason, UserState,
				    SslState);
		 {unknown, _} ->
		     apply_user_fun(Fun, OtpCert,
				    Extension, UserState, SslState)
	     end;
	(OtpCert, VerifyResult, {SslState, UserState}) ->
	     apply_user_fun(Fun, OtpCert, VerifyResult, UserState,
			    SslState)
     end, {{Role, CertDbHandle, CertDbRef, CRLCheck, CRLDbHandle}, UserState0}};
validation_fun_and_state(undefined, Role, CertDbHandle, CertDbRef, CRLCheck, CRLDbHandle) ->
    {fun(OtpCert, {extension, _} = Extension, SslState) ->
	     ssl_certificate:validate(OtpCert,
				      Extension,
				      SslState);
	(OtpCert, VerifyResult, SslState) when (VerifyResult == valid) or (VerifyResult == valid_peer) -> 
	     case crl_check(OtpCert, CRLCheck, CertDbHandle, CertDbRef, CRLDbHandle, VerifyResult) of
		 valid ->
		     {VerifyResult, SslState};
		 Reason ->
		     {fail, Reason}
	     end;
	(OtpCert, VerifyResult, SslState) ->
	     ssl_certificate:validate(OtpCert,
				      VerifyResult,
				      SslState)
     end, {Role, CertDbHandle, CertDbRef, CRLCheck, CRLDbHandle}}.

apply_user_fun(Fun, OtpCert, VerifyResult, UserState0, 
	       {_, CertDbHandle, CertDbRef, CRLCheck, CRLDbHandle} = SslState) when
      (VerifyResult == valid) or (VerifyResult == valid_peer) ->
    case Fun(OtpCert, VerifyResult, UserState0) of
	{Valid, UserState} when (Valid == valid) or (Valid == valid_peer) ->
	    case crl_check(OtpCert, CRLCheck, CertDbHandle, CertDbRef, CRLDbHandle, VerifyResult) of
		valid ->
		    {Valid, {SslState, UserState}};
		Result ->
		    apply_user_fun(Fun, OtpCert, Result, UserState, SslState)
	    end;
	{fail, _} = Fail ->
	    Fail
    end;
apply_user_fun(Fun, OtpCert, ExtensionOrError, UserState0, SslState) ->
    case Fun(OtpCert, ExtensionOrError, UserState0) of
	{Valid, UserState} when (Valid == valid) or (Valid == valid_peer)->
	    {Valid, {SslState, UserState}};
	{fail, _} = Fail ->
	    Fail;
	{unknown, UserState} ->
	    {unknown, {SslState, UserState}}
    end.

path_validation_alert({bad_cert, cert_expired}) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_EXPIRED);
path_validation_alert({bad_cert, invalid_issuer}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, invalid_signature}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, name_not_permitted}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, unknown_critical_extension}) ->
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE);
path_validation_alert({bad_cert, {revoked, _}}) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_REVOKED);
path_validation_alert({bad_cert, revocation_status_undetermined}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, selfsigned_peer}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, unknown_ca}) ->
     ?ALERT_REC(?FATAL, ?UNKNOWN_CA);
path_validation_alert(Reason) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason).

encrypted_premaster_secret(Secret, RSAPublicKey) ->
    try
	PreMasterSecret = public_key:encrypt_public(Secret, RSAPublicKey,
						    [{rsa_pad,
						      rsa_pkcs1_padding}]),
	#encrypted_premaster_secret{premaster_secret = PreMasterSecret}
    catch
        _:_->
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, premaster_encryption_failed))
    end.

digitally_signed(Version, Hashes, HashAlgo, PrivateKey) ->
    try do_digitally_signed(Version, Hashes, HashAlgo, PrivateKey) of
	Signature ->
	    Signature
    catch
	error:badkey->
	    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, bad_key(PrivateKey)))
    end.

do_digitally_signed({3, Minor}, Hash, HashAlgo, Key) when Minor >= 3 ->
    public_key:sign({digest, Hash}, HashAlgo, Key);
do_digitally_signed(_Version, Hash, HashAlgo, #'DSAPrivateKey'{} = Key) ->
    public_key:sign({digest, Hash}, HashAlgo, Key);
do_digitally_signed(_Version, Hash, _HashAlgo, #'RSAPrivateKey'{} = Key) ->
    public_key:encrypt_private(Hash, Key,
			       [{rsa_pad, rsa_pkcs1_padding}]);
do_digitally_signed(_Version, Hash, HashAlgo, Key) ->
    public_key:sign({digest, Hash}, HashAlgo, Key).

calc_certificate_verify({3, 0}, HashAlgo, MasterSecret, Handshake) ->
    ssl_v3:certificate_verify(HashAlgo, MasterSecret, lists:reverse(Handshake));
calc_certificate_verify({3, N}, HashAlgo, _MasterSecret, Handshake) ->
    tls_v1:certificate_verify(HashAlgo, N, lists:reverse(Handshake)).

calc_finished({3, 0}, Role, _PrfAlgo, MasterSecret, Handshake) ->
    ssl_v3:finished(Role, MasterSecret, lists:reverse(Handshake));
calc_finished({3, N}, Role, PrfAlgo, MasterSecret, Handshake) ->
    tls_v1:finished(Role, N, PrfAlgo, MasterSecret, lists:reverse(Handshake)).

master_secret(_RecordCB, Version, MasterSecret,
	      #security_parameters{
		 bulk_cipher_algorithm = BCA,
		 client_random = ClientRandom,
		 server_random = ServerRandom,
		 hash_size = HashSize,
		 prf_algorithm = PrfAlgo,
		 key_material_length = KML,
		 expanded_key_material_length = EKML,
		 iv_size = IVS},
	      ConnectionStates, Role) ->
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV} =
	setup_keys(Version, PrfAlgo, MasterSecret, ServerRandom,
		   ClientRandom, HashSize, KML, EKML, IVS),

    ConnStates1 = ssl_record:set_master_secret(MasterSecret, ConnectionStates),
    ConnStates2 =
	ssl_record:set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret,
				  Role, ConnStates1),

    ClientCipherState = ssl_cipher:cipher_init(BCA, ClientIV, ClientWriteKey),
    ServerCipherState = ssl_cipher:cipher_init(BCA, ServerIV, ServerWriteKey),
    {MasterSecret,
     ssl_record:set_pending_cipher_state(ConnStates2, ClientCipherState,
					 ServerCipherState, Role)}.

setup_keys({3,0}, _PrfAlgo, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, EKML, IVS) ->
    ssl_v3:setup_keys(MasterSecret, ServerRandom,
			ClientRandom, HashSize, KML, EKML, IVS);

setup_keys({3,N}, PrfAlgo, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, _EKML, IVS) ->
    tls_v1:setup_keys(N, PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
			KML, IVS).

calc_master_secret({3,0}, _PrfAlgo, PremasterSecret, ClientRandom, ServerRandom) ->
    ssl_v3:master_secret(PremasterSecret, ClientRandom, ServerRandom);

calc_master_secret({3,_}, PrfAlgo, PremasterSecret, ClientRandom, ServerRandom) ->
    tls_v1:master_secret(PrfAlgo, PremasterSecret, ClientRandom, ServerRandom).

handle_renegotiation_extension(Role, RecordCB, Version, Info, Random, NegotiatedCipherSuite, 
			       ClientCipherSuites, Compression,
			       ConnectionStates0, Renegotiation, SecureRenegotation) ->
    case handle_renegotiation_info(RecordCB, Role, Info, ConnectionStates0,
				   Renegotiation, SecureRenegotation,
				   ClientCipherSuites) of
	{ok, ConnectionStates} ->
	    hello_pending_connection_states(RecordCB, Role,
					    Version,
					    NegotiatedCipherSuite,
					    Random,
					    Compression,
					    ConnectionStates);
	#alert{} = Alert ->
	    throw(Alert)
    end.

%% Update pending connection states with parameters exchanged via
%% hello messages
%% NOTE : Role is the role of the receiver of the hello message
%%        currently being processed.
hello_pending_connection_states(_RecordCB, Role, Version, CipherSuite, Random, Compression,
				 ConnectionStates) ->
    ReadState =
	ssl_record:pending_connection_state(ConnectionStates, read),
    WriteState =
	ssl_record:pending_connection_state(ConnectionStates, write),

    NewReadSecParams =
	hello_security_parameters(Role, Version, ReadState, CipherSuite,
			    Random, Compression),

    NewWriteSecParams =
	hello_security_parameters(Role, Version, WriteState, CipherSuite,
			    Random, Compression),

    ssl_record:set_security_params(NewReadSecParams,
				    NewWriteSecParams,
				    ConnectionStates).

hello_security_parameters(client, Version, ConnectionState, CipherSuite, Random,
			  Compression) ->
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
    NewSecParams#security_parameters{
      server_random = Random,
      compression_algorithm = Compression
     };

hello_security_parameters(server, Version, ConnectionState, CipherSuite, Random,
			  Compression) ->
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
    NewSecParams#security_parameters{
      client_random = Random,
      compression_algorithm = Compression
     }.

%%-------------Encode/Decode --------------------------------

encode_server_key(#server_dh_params{dh_p = P, dh_g = G, dh_y = Y}) ->
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    <<?UINT16(PLen), P/binary, ?UINT16(GLen), G/binary, ?UINT16(YLen), Y/binary>>;
encode_server_key(#server_ecdh_params{curve = {namedCurve, ECCurve}, public = ECPubKey}) ->
    %%TODO: support arbitrary keys
    KLen = size(ECPubKey),
    <<?BYTE(?NAMED_CURVE), ?UINT16((tls_v1:oid_to_enum(ECCurve))),
      ?BYTE(KLen), ECPubKey/binary>>;
encode_server_key(#server_psk_params{hint = PskIdentityHint}) ->
    Len = byte_size(PskIdentityHint),
    <<?UINT16(Len), PskIdentityHint/binary>>;
encode_server_key(Params = #server_dhe_psk_params{hint = undefined}) ->
    encode_server_key(Params#server_dhe_psk_params{hint = <<>>});
encode_server_key(#server_dhe_psk_params{
		  hint = PskIdentityHint,
		  dh_params = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y}}) ->
    Len = byte_size(PskIdentityHint),
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    <<?UINT16(Len), PskIdentityHint/binary,
      ?UINT16(PLen), P/binary, ?UINT16(GLen), G/binary, ?UINT16(YLen), Y/binary>>;
encode_server_key(#server_srp_params{srp_n = N, srp_g = G,	srp_s = S, srp_b = B}) ->
    NLen = byte_size(N),
    GLen = byte_size(G),
    SLen = byte_size(S),
    BLen = byte_size(B),
    <<?UINT16(NLen), N/binary, ?UINT16(GLen), G/binary,
      ?BYTE(SLen), S/binary, ?UINT16(BLen), B/binary>>.

encode_client_key(#encrypted_premaster_secret{premaster_secret = PKEPMS},{3, 0}) ->
    PKEPMS;
encode_client_key(#encrypted_premaster_secret{premaster_secret = PKEPMS}, _) ->
    PKEPMSLen = byte_size(PKEPMS),
    <<?UINT16(PKEPMSLen), PKEPMS/binary>>;
encode_client_key(#client_diffie_hellman_public{dh_public = DHPublic}, _) ->
    Len = byte_size(DHPublic),
    <<?UINT16(Len), DHPublic/binary>>;
encode_client_key(#client_ec_diffie_hellman_public{dh_public = DHPublic}, _) ->
    Len = byte_size(DHPublic),
    <<?BYTE(Len), DHPublic/binary>>;
encode_client_key(#client_psk_identity{identity = undefined}, _) ->
    Id = <<"psk_identity">>,
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
encode_client_key(#client_psk_identity{identity = Id}, _) ->
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
encode_client_key(Identity = #client_dhe_psk_identity{identity = undefined}, Version) ->
    encode_client_key(Identity#client_dhe_psk_identity{identity = <<"psk_identity">>}, Version);
encode_client_key(#client_dhe_psk_identity{identity = Id, dh_public = DHPublic}, _) ->
    Len = byte_size(Id),
    DHLen = byte_size(DHPublic),
    <<?UINT16(Len), Id/binary, ?UINT16(DHLen), DHPublic/binary>>;
encode_client_key(Identity = #client_rsa_psk_identity{identity = undefined}, Version) ->
    encode_client_key(Identity#client_rsa_psk_identity{identity = <<"psk_identity">>}, Version);
encode_client_key(#client_rsa_psk_identity{identity = Id, exchange_keys = ExchangeKeys}, Version) ->
    EncPMS = encode_client_key(ExchangeKeys, Version),
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary, EncPMS/binary>>;
encode_client_key(#client_srp_public{srp_a = A}, _) ->
    Len = byte_size(A),
    <<?UINT16(Len), A/binary>>.

enc_sign({_, anon}, _Sign, _Version) ->
    <<>>;
enc_sign({HashAlg, SignAlg}, Signature, _Version = {Major, Minor})
  when Major == 3, Minor >= 3->
	SignLen = byte_size(Signature),
	HashSign = enc_hashsign(HashAlg, SignAlg),
	<<HashSign/binary, ?UINT16(SignLen), Signature/binary>>;
enc_sign(_HashSign, Sign, _Version) ->
	SignLen = byte_size(Sign),
	<<?UINT16(SignLen), Sign/binary>>.

enc_hashsign(HashAlgo, SignAlgo) ->
    Hash = ssl_cipher:hash_algorithm(HashAlgo),
    Sign = ssl_cipher:sign_algorithm(SignAlgo),
    <<?BYTE(Hash), ?BYTE(Sign)>>.

encode_protocol(Protocol, Acc) ->
	Len = byte_size(Protocol),
	<<Acc/binary, ?BYTE(Len), Protocol/binary>>.

dec_client_key(PKEPMS, ?KEY_EXCHANGE_RSA, {3, 0}) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
dec_client_key(<<?UINT16(_), PKEPMS/binary>>, ?KEY_EXCHANGE_RSA, _) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
dec_client_key(<<>>, ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, empty_dh_public));
dec_client_key(<<?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #client_diffie_hellman_public{dh_public = DH_Y};
dec_client_key(<<>>, ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN, _) ->
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, empty_dh_public));
dec_client_key(<<?BYTE(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN, _) ->
    #client_ec_diffie_hellman_public{dh_public = DH_Y};
dec_client_key(<<?UINT16(Len), Id:Len/binary>>,
	       ?KEY_EXCHANGE_PSK, _) ->
    #client_psk_identity{identity = Id};
dec_client_key(<<?UINT16(Len), Id:Len/binary,
		 ?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DHE_PSK, _) ->
    #client_dhe_psk_identity{identity = Id, dh_public = DH_Y};
dec_client_key(<<?UINT16(Len), Id:Len/binary, PKEPMS/binary>>,
	       ?KEY_EXCHANGE_RSA_PSK, {3, 0}) ->
    #client_rsa_psk_identity{identity = Id,
			     exchange_keys = #encrypted_premaster_secret{premaster_secret = PKEPMS}};
dec_client_key(<<?UINT16(Len), Id:Len/binary, ?UINT16(_), PKEPMS/binary>>,
	       ?KEY_EXCHANGE_RSA_PSK, _) ->
    #client_rsa_psk_identity{identity = Id,
			     exchange_keys = #encrypted_premaster_secret{premaster_secret = PKEPMS}};
dec_client_key(<<?UINT16(ALen), A:ALen/binary>>,
	       ?KEY_EXCHANGE_SRP, _) ->
    #client_srp_public{srp_a = A}.

dec_server_key_params(Len, Keys, Version) ->
    <<Params:Len/bytes, Signature/binary>> = Keys,
    dec_server_key_signature(Params, Signature, Version).

dec_server_key_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(0)>>, {Major, Minor})
  when Major == 3, Minor >= 3 ->
    HashSign = {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)},
    {Params, HashSign, <<>>};
dec_server_key_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(Len), Signature:Len/binary>>, {Major, Minor})
  when Major == 3, Minor >= 3 ->
    HashSign = {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)},
    {Params, HashSign, Signature};
dec_server_key_signature(Params, <<>>, _) ->
    {Params, {null, anon}, <<>>};
dec_server_key_signature(Params, <<?UINT16(0)>>, _) ->
    {Params, {null, anon}, <<>>};
dec_server_key_signature(Params, <<?UINT16(Len), Signature:Len/binary>>, _) ->
    {Params, undefined, Signature};
dec_server_key_signature(_, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, failed_to_decrypt_server_key_sign)).

dec_hello_extensions(<<>>, Acc) ->
    Acc;
dec_hello_extensions(<<?UINT16(?ALPN_EXT), ?UINT16(ExtLen), ?UINT16(Len), ExtensionData:Len/binary, Rest/binary>>, Acc)
        when Len + 2 =:= ExtLen ->
    ALPN = #alpn{extension_data = ExtensionData},
    dec_hello_extensions(Rest, Acc#hello_extensions{alpn = ALPN});
dec_hello_extensions(<<?UINT16(?NEXTPROTONEG_EXT), ?UINT16(Len), ExtensionData:Len/binary, Rest/binary>>, Acc) ->
    NextP = #next_protocol_negotiation{extension_data = ExtensionData},
    dec_hello_extensions(Rest, Acc#hello_extensions{next_protocol_negotiation = NextP});
dec_hello_extensions(<<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), Info:Len/binary, Rest/binary>>, Acc) ->
    RenegotiateInfo = case Len of
			  1 ->  % Initial handshake
			      Info; % should be <<0>> will be matched in handle_renegotiation_info
			  _ ->
			      VerifyLen = Len - 1,
			      <<?BYTE(VerifyLen), VerifyInfo/binary>> = Info,
			      VerifyInfo
		      end,
    dec_hello_extensions(Rest, Acc#hello_extensions{renegotiation_info =
							#renegotiation_info{renegotiated_connection =
										RenegotiateInfo}});

dec_hello_extensions(<<?UINT16(?SRP_EXT), ?UINT16(Len), ?BYTE(SRPLen), SRP:SRPLen/binary, Rest/binary>>, Acc)
  when Len == SRPLen + 2 ->
    dec_hello_extensions(Rest,  Acc#hello_extensions{srp = #srp{username = SRP}});

dec_hello_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Acc) ->
    SignAlgoListLen = Len - 2,
    <<?UINT16(SignAlgoListLen), SignAlgoList/binary>> = ExtData,
    HashSignAlgos = [{ssl_cipher:hash_algorithm(Hash), ssl_cipher:sign_algorithm(Sign)} ||
			<<?BYTE(Hash), ?BYTE(Sign)>> <= SignAlgoList],
    dec_hello_extensions(Rest, Acc#hello_extensions{signature_algs =
						    #hash_sign_algos{hash_sign_algos = HashSignAlgos}});

dec_hello_extensions(<<?UINT16(?ELLIPTIC_CURVES_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Acc) ->
    <<?UINT16(_), EllipticCurveList/binary>> = ExtData,
    %% Ignore unknown curves
    Pick = fun(Enum) ->
		   case tls_v1:enum_to_oid(Enum) of
		       undefined ->
			   false;
		       Oid ->
			   {true, Oid}
		   end
	   end,
    EllipticCurves = lists:filtermap(Pick, [ECC || <<ECC:16>> <= EllipticCurveList]),
    dec_hello_extensions(Rest, Acc#hello_extensions{elliptic_curves =
							#elliptic_curves{elliptic_curve_list =
									     EllipticCurves}});
dec_hello_extensions(<<?UINT16(?EC_POINT_FORMATS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Acc) ->
    <<?BYTE(_), ECPointFormatList/binary>> = ExtData,
    ECPointFormats = binary_to_list(ECPointFormatList),
    dec_hello_extensions(Rest, Acc#hello_extensions{ec_point_formats =
							#ec_point_formats{ec_point_format_list =
									      ECPointFormats}});

dec_hello_extensions(<<?UINT16(?SNI_EXT), ?UINT16(Len), Rest/binary>>, Acc) when Len == 0 ->
    dec_hello_extensions(Rest, Acc#hello_extensions{sni = ""}); %% Server may send an empy SNI

dec_hello_extensions(<<?UINT16(?SNI_EXT), ?UINT16(Len),
                ExtData:Len/binary, Rest/binary>>, Acc) ->
    <<?UINT16(_), NameList/binary>> = ExtData,
    dec_hello_extensions(Rest, Acc#hello_extensions{sni = dec_sni(NameList)});
%% Ignore data following the ClientHello (i.e.,
%% extensions) if not understood.

dec_hello_extensions(<<?UINT16(_), ?UINT16(Len), _Unknown:Len/binary, Rest/binary>>, Acc) ->
    dec_hello_extensions(Rest, Acc);
%% This theoretically should not happen if the protocol is followed, but if it does it is ignored.
dec_hello_extensions(_, Acc) ->
    Acc.

dec_hashsign(<<?BYTE(HashAlgo), ?BYTE(SignAlgo)>>) ->
    {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)}.

%% Ignore unknown names (only host_name is supported)
dec_sni(<<?BYTE(?SNI_NAMETYPE_HOST_NAME), ?UINT16(Len),
                HostName:Len/binary, _/binary>>) ->
    #sni{hostname = binary_to_list(HostName)};
dec_sni(<<?BYTE(_), ?UINT16(Len), _:Len, Rest/binary>>) -> dec_sni(Rest);
dec_sni(_) -> undefined.

decode_next_protocols({next_protocol_negotiation, Protocols}) ->
    decode_protocols(Protocols, []).

decode_protocols(<<>>, Acc) ->
    lists:reverse(Acc);
decode_protocols(<<?BYTE(Len), Protocol:Len/binary, Rest/binary>>, Acc) ->
    case Len of
        0 ->
            {error, invalid_protocols};
        _ ->
            decode_protocols(Rest, [Protocol|Acc])
    end;
decode_protocols(_Bytes, _Acc) ->
    {error, invalid_protocols}.

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
key_exchange_alg(rsa) ->
    ?KEY_EXCHANGE_RSA;
key_exchange_alg(Alg) when Alg == dhe_rsa; Alg == dhe_dss;
			   Alg == dh_dss; Alg == dh_rsa; Alg == dh_anon ->
    ?KEY_EXCHANGE_DIFFIE_HELLMAN;
key_exchange_alg(Alg) when Alg == ecdhe_rsa; Alg == ecdh_rsa;
			   Alg == ecdhe_ecdsa; Alg == ecdh_ecdsa;
			   Alg == ecdh_anon ->
    ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN;
key_exchange_alg(psk) ->
    ?KEY_EXCHANGE_PSK;
key_exchange_alg(dhe_psk) ->
    ?KEY_EXCHANGE_DHE_PSK;
key_exchange_alg(rsa_psk) ->
    ?KEY_EXCHANGE_RSA_PSK;
key_exchange_alg(Alg)
  when Alg == srp_rsa; Alg == srp_dss; Alg == srp_anon ->
    ?KEY_EXCHANGE_SRP;
key_exchange_alg(_) ->
    ?NULL.

%%-------------Extension handling --------------------------------

%% Receive protocols, choose one from the list, return it.
handle_alpn_extension(_, {error, Reason}) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason);
handle_alpn_extension([], _) ->
	?ALERT_REC(?FATAL, ?NO_APPLICATION_PROTOCOL);
handle_alpn_extension([ServerProtocol|Tail], ClientProtocols) ->
	case lists:member(ServerProtocol, ClientProtocols) of
		true -> ServerProtocol;
		false -> handle_alpn_extension(Tail, ClientProtocols)
	end.

handle_next_protocol(undefined,
		     _NextProtocolSelector, _Renegotiating) ->
    undefined;

handle_next_protocol(#next_protocol_negotiation{} = NextProtocols,
    NextProtocolSelector, Renegotiating) ->

    case next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) of
        true ->
            select_next_protocol(decode_next_protocols(NextProtocols), NextProtocolSelector);
        false ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, unexpected_next_protocol_extension)
    end.


handle_next_protocol_extension(NextProtocolNegotiation, Renegotiation, SslOpts)->
    case handle_next_protocol_on_server(NextProtocolNegotiation, Renegotiation, SslOpts) of
	#alert{} = Alert ->
	    Alert;
	ProtocolsToAdvertise ->
	    ProtocolsToAdvertise
    end.

handle_next_protocol_on_server(undefined, _Renegotiation, _SslOpts) ->
    undefined;

handle_next_protocol_on_server(#next_protocol_negotiation{extension_data = <<>>},
			       false, #ssl_options{next_protocols_advertised = Protocols}) ->
    Protocols;

handle_next_protocol_on_server(_Hello, _Renegotiation, _SSLOpts) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, unexpected_next_protocol_extension).

next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) ->
    NextProtocolSelector =/= undefined andalso not Renegotiating.

select_next_protocol({error, Reason}, _NextProtocolSelector) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason);
select_next_protocol(Protocols, NextProtocolSelector) ->
    case NextProtocolSelector(Protocols) of
	?NO_PROTOCOL ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, no_next_protocol);
	Protocol when is_binary(Protocol)  ->
	    Protocol
    end.

handle_srp_extension(undefined, Session) ->
    Session;
handle_srp_extension(#srp{username = Username}, Session) ->
    Session#session{srp_username = Username}.

%%-------------Misc --------------------------------

select_cipher_suite(CipherSuites, Suites, false) ->
    select_cipher_suite(CipherSuites, Suites);
select_cipher_suite(CipherSuites, Suites, true) ->
    select_cipher_suite(Suites, CipherSuites).

select_cipher_suite([], _) ->
   no_suite;
select_cipher_suite([Suite | ClientSuites], SupportedSuites) ->
    case is_member(Suite, SupportedSuites) of
	true ->
	    Suite;
        false ->
	    select_cipher_suite(ClientSuites, SupportedSuites)
    end.

int_to_bin(I) ->
    L = (length(integer_to_list(I, 16)) + 1) div 2,
    <<I:(L*8)>>.

is_member(Suite, SupportedSuites) ->
    lists:member(Suite, SupportedSuites).

select_compression(_CompressionMetodes) ->
    ?NULL.

available_signature_algs(undefined, _, _)  ->
    undefined;
available_signature_algs(SupportedHashSigns, {Major, Minor}, AllVersions) when Major >= 3 andalso Minor >= 3 ->
    case tls_record:lowest_protocol_version(AllVersions) of
	{3, 3} ->
	    #hash_sign_algos{hash_sign_algos = SupportedHashSigns};
	_ ->
	    undefined
    end;	
available_signature_algs(_, _, _) ->
    undefined.

psk_secret(PSKIdentity, PSKLookup) ->
    case handle_psk_identity(PSKIdentity, PSKLookup) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = erlang:byte_size(PSK),
	    <<?UINT16(Len), 0:(Len*8), ?UINT16(Len), PSK/binary>>;
	#alert{} = Alert ->
	    Alert;
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end.

psk_secret(PSKIdentity, PSKLookup, PremasterSecret) ->
    case handle_psk_identity(PSKIdentity, PSKLookup) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = erlang:byte_size(PremasterSecret),
	    PSKLen = erlang:byte_size(PSK),
	    <<?UINT16(Len), PremasterSecret/binary, ?UINT16(PSKLen), PSK/binary>>;
	#alert{} = Alert ->
	    Alert;
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end.

handle_psk_identity(_PSKIdentity, LookupFun)
  when LookupFun == undefined ->
    error;
handle_psk_identity(PSKIdentity, {Fun, UserState}) ->
    Fun(psk, PSKIdentity, UserState).

crl_check(_, false, _,_,_, _) ->
    valid;
crl_check(_, peer, _, _,_, valid) -> %% Do not check CAs with this option.
    valid;
crl_check(OtpCert, Check, CertDbHandle, CertDbRef, {Callback, CRLDbHandle}, _) ->
    Options = [{issuer_fun, {fun(_DP, CRL, Issuer, DBInfo) ->
				     ssl_crl:trusted_cert_and_path(CRL, Issuer, DBInfo)
			     end, {CertDbHandle, CertDbRef}}}, 
	       {update_crl, fun(DP, CRL) -> Callback:fresh_crl(DP, CRL) end}
	      ],
    case dps_and_crls(OtpCert, Callback, CRLDbHandle, ext) of
	no_dps ->
	    crl_check_same_issuer(OtpCert, Check,
				  dps_and_crls(OtpCert, Callback, CRLDbHandle, same_issuer),
				  Options);
	DpsAndCRLs ->  %% This DP list may be empty if relevant CRLs existed 
	    %% but could not be retrived, will result in {bad_cert, revocation_status_undetermined}
	    case public_key:pkix_crls_validate(OtpCert, DpsAndCRLs, Options) of
		{bad_cert, revocation_status_undetermined} ->
		    crl_check_same_issuer(OtpCert, Check, dps_and_crls(OtpCert, Callback, 
								       CRLDbHandle, same_issuer), Options);
		Other ->
		    Other
	    end
    end.

crl_check_same_issuer(OtpCert, best_effort, Dps, Options) ->		
    case public_key:pkix_crls_validate(OtpCert, Dps, Options) of 
	{bad_cert, revocation_status_undetermined}  ->
	    valid;
	Other ->
	    Other
    end;
crl_check_same_issuer(OtpCert, _, Dps, Options) ->    
    public_key:pkix_crls_validate(OtpCert, Dps, Options).

dps_and_crls(OtpCert, Callback, CRLDbHandle, ext) ->
    case public_key:pkix_dist_points(OtpCert) of
	[] ->
	    no_dps;
	DistPoints ->
	    Issuer = OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer,
	    distpoints_lookup(DistPoints, Issuer, Callback, CRLDbHandle)
    end;

dps_and_crls(OtpCert, Callback, CRLDbHandle, same_issuer) ->    
    DP = #'DistributionPoint'{distributionPoint = {fullName, GenNames}} = 
	public_key:pkix_dist_point(OtpCert),
    CRLs = lists:flatmap(fun({directoryName, Issuer}) -> 
				 Callback:select(Issuer, CRLDbHandle);
			    (_) ->
				 []
			 end, GenNames),
    [{DP, {CRL, public_key:der_decode('CertificateList', CRL)}} ||  CRL <- CRLs].

distpoints_lookup([], _, _, _) ->
    [];
distpoints_lookup([DistPoint | Rest], Issuer, Callback, CRLDbHandle) ->
    Result =
	try Callback:lookup(DistPoint, Issuer, CRLDbHandle)
	catch
	    error:undef ->
		%% The callback module still uses the 2-argument
		%% version of the lookup function.
		Callback:lookup(DistPoint, CRLDbHandle)
	end,
    case Result of
	not_available ->
	    distpoints_lookup(Rest, Issuer, Callback, CRLDbHandle);
	CRLs ->
	    [{DistPoint, {CRL, public_key:der_decode('CertificateList', CRL)}} ||  CRL <- CRLs]
    end.	

sign_algo(?rsaEncryption) ->
    rsa;
sign_algo(?'id-ecPublicKey') ->
    ecdsa;
sign_algo(?'id-dsa') ->
    dsa;
sign_algo(Alg) ->
    {_, Sign} =public_key:pkix_sign_types(Alg),
    Sign.

is_acceptable_hash_sign(Algos, _, _, KeyExAlgo, SupportedHashSigns) when 
      KeyExAlgo == dh_dss;
      KeyExAlgo == dh_rsa;
      KeyExAlgo == dh_ecdsa ->
    %% dh_* could be called only dh in TLS-1.2
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign(Algos, rsa, ecdsa, ecdh_rsa, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, rsa} = Algos, rsa, _, dhe_rsa, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, rsa} = Algos, rsa, rsa, ecdhe_rsa, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, rsa} = Algos, rsa, rsa, rsa, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, rsa} = Algos, rsa, _, srp_rsa, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, rsa} = Algos, rsa, _, rsa_psk, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, dsa} = Algos, dsa, _, dhe_dss, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, dsa} = Algos, dsa, _, srp_dss, SupportedHashSigns) ->  
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, ecdsa} = Algos, ecdsa, _, dhe_ecdsa, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign({_, ecdsa} = Algos, ecdsa, ecdsa, ecdhe_ecdsa, SupportedHashSigns) ->
    is_acceptable_hash_sign(Algos, SupportedHashSigns); 
is_acceptable_hash_sign(_, _, _, KeyExAlgo, _) when 
      KeyExAlgo == psk;
      KeyExAlgo == dhe_psk;
      KeyExAlgo == srp_anon;
      KeyExAlgo == dh_anon;
      KeyExAlgo == ecdhe_anon     
      ->
    true; 
is_acceptable_hash_sign(_,_, _,_,_) ->
    false.					

is_acceptable_hash_sign(Algos, SupportedHashSigns) ->
    lists:member(Algos, SupportedHashSigns).

is_acceptable_cert_type(Sign, _HashSigns, Types) ->
    lists:member(sign_type(Sign), binary_to_list(Types)).

is_supported_sign(Sign, HashSigns) ->
     [] =/=  lists:dropwhile(fun({_, S}) when S =/= Sign -> 
				     true;
				(_)-> 
				     false 
			     end, HashSigns).
sign_type(rsa) ->
    ?RSA_SIGN;
sign_type(dsa) ->
    ?DSS_SIGN;
sign_type(ecdsa) ->
    ?ECDSA_SIGN.


bad_key(#'DSAPrivateKey'{}) ->
    unacceptable_dsa_key;
bad_key(#'RSAPrivateKey'{}) ->
    unacceptable_rsa_key;
bad_key(#'ECPrivateKey'{}) ->
    unacceptable_ecdsa_key.

available_signature_algs(undefined, SupportedHashSigns, _, {Major, Minor}) when 
      (Major >= 3) andalso (Minor >= 3) ->
    SupportedHashSigns;
available_signature_algs(#hash_sign_algos{hash_sign_algos = ClientHashSigns}, SupportedHashSigns, 
		     _, {Major, Minor}) when (Major >= 3) andalso (Minor >= 3) ->
    sets:to_list(sets:intersection(sets:from_list(ClientHashSigns), 
				   sets:from_list(SupportedHashSigns)));
available_signature_algs(_, _, _, _) -> 
    undefined.

