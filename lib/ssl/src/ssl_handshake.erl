%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2014. All Rights Reserved.
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

%% Handshake messages
-export([hello_request/0, server_hello/4, server_hello_done/0,
	 certificate/4, certificate_request/4, key_exchange/3,
	 finished/5,  next_protocol/1]).

%% Handle handshake messages
-export([certify/7, client_certificate_verify/6, certificate_verify/6, verify_signature/5,
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
-export([available_suites/2, cipher_suites/2,
	 select_session/10, supported_ecc/1]).

%% Extensions handling
-export([client_hello_extensions/6,
	 handle_client_hello_extensions/9, %% Returns server hello extensions
	 handle_server_hello_extensions/9, select_curve/2
	]).

%% MISC
-export([select_version/3, prf/5, select_hashsign/2, select_cert_hashsign/3,
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
-spec server_hello(#session{}, tls_version(), #connection_states{},
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

client_hello_extensions(Host, Version, CipherSuites, SslOpts, ConnectionStates, Renegotiation) ->
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
       hash_signs = advertised_hash_signs(Version),
       ec_point_formats = EcPointFormats,
       elliptic_curves = EllipticCurves,
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
	    {ok, CertChain} ->
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
	{ok, Chain} ->
	    #certificate{asn1_certificates = Chain};
	{error, _} ->
	    ?ALERT_REC(?FATAL, ?INTERNAL_ERROR)
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
				tls_version(), term(), private_key(),
				tls_handshake_history()) ->
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
	    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE);
	false ->
	    Hashes =
		calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
	    Signed = digitally_signed(Version, Hashes, HashAlgo, PrivateKey),
	    #certificate_verify{signature = Signed, hashsign_algorithm = {HashAlgo, SignAlgo}}
    end.

%%--------------------------------------------------------------------
-spec certificate_request(erl_cipher_suite(), db_handle(), certdb_ref(), tls_version()) ->
    #certificate_request{}.
%%
%% Description: Creates a certificate_request message, called by the server.
%%--------------------------------------------------------------------
certificate_request(CipherSuite, CertDbHandle, CertDbRef, Version) ->
    Types = certificate_types(CipherSuite),
    HashSigns = advertised_hash_signs(Version),
    Authorities = certificate_authorities(CertDbHandle, CertDbRef),
    #certificate_request{
		    certificate_types = Types,
		    hashsign_algorithms = HashSigns,
		    certificate_authorities = Authorities
		   }.
%%--------------------------------------------------------------------
-spec key_exchange(client | server, tls_version(),
		   {premaster_secret, binary(), public_key_info()} |
		   {dh, binary()} |
		   {dh, {binary(), binary()}, #'DHParameter'{}, {HashAlgo::atom(), SignAlgo::atom()},
		   binary(), binary(), private_key()} |
		   {ecdh, #'ECPrivateKey'{}} |
		   {psk, binary()} |
		   {dhe_psk, binary(), binary()} |
		   {srp, {binary(), binary()}, #srp_user{}, {HashAlgo::atom(), SignAlgo::atom()},
		   binary(), binary(), private_key()}) ->
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

key_exchange(client, _Version, {ecdh, #'ECPrivateKey'{publicKey = {0, ECPublicKey}}}) ->
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

key_exchange(server, Version, {ecdh,  #'ECPrivateKey'{publicKey =  {0, ECPublicKey},
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
-spec finished(tls_version(), client | server, integer(), binary(), tls_handshake_history()) ->
    #finished{}.
%%
%% Description: Creates a handshake finished message
%%-------------------------------------------------------------------
finished(Version, Role, PrfAlgo, MasterSecret, {Handshake, _}) -> % use the current handshake
    #finished{verify_data =
	      calc_finished(Version, Role, PrfAlgo, MasterSecret, Handshake)}.

%% ---------- Handle handshake messages  ----------

verify_server_key(#server_key_params{params = Params,
				     params_bin = EncParams,
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
-spec certificate_verify(binary(), public_key_info(), tls_version(), term(),
			 binary(), tls_handshake_history()) -> valid | #alert{}.
%%
%% Description: Checks that the certificate_verify message is valid.
%%--------------------------------------------------------------------
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
-spec verify_signature(tls_version(), binary(), {term(), term()}, binary(),
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
verify_signature(_Version, Hash, {HashAlgo, ecdsa}, Signature,
		 {?'id-ecPublicKey', PublicKey, PublicKeyParams}) ->
    public_key:verify({digest, Hash}, HashAlgo, Signature, {PublicKey, PublicKeyParams}).

%%--------------------------------------------------------------------
-spec certify(#certificate{}, db_handle(), certdb_ref(), integer() | nolimit,
	      verify_peer | verify_none, {fun(), term},
	      client | server) ->  {der_cert(), public_key_info()} | #alert{}.
%%
%% Description: Handles a certificate handshake message
%%--------------------------------------------------------------------
certify(#certificate{asn1_certificates = ASN1Certs}, CertDbHandle, CertDbRef,
	MaxPathLen, _Verify, VerifyFunAndState, Role) ->
    [PeerCert | _] = ASN1Certs,

    ValidationFunAndState =
	case VerifyFunAndState of
	    undefined ->
		{fun(OtpCert, ExtensionOrVerifyResult, SslState) ->
			 ssl_certificate:validate_extension(OtpCert,
							    ExtensionOrVerifyResult, SslState)
		 end, Role};
	    {Fun, UserState0} ->
		{fun(OtpCert, {extension, _} = Extension, {SslState, UserState}) ->
			 case ssl_certificate:validate_extension(OtpCert,
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
		 end, {Role, UserState0}}
	end,

    try
	{TrustedErlCert, CertPath}  =
	    ssl_certificate:trusted_cert_and_path(ASN1Certs, CertDbHandle, CertDbRef),
	case public_key:pkix_path_validation(TrustedErlCert,
					      CertPath,
					     [{max_path_length,
					       MaxPathLen},
					      {verify_fun, ValidationFunAndState}]) of
	    {ok, {PublicKeyInfo,_}} ->
		{PeerCert, PublicKeyInfo};
	    {error, Reason} ->
		path_validation_alert(Reason)
	end
    catch
	error:_ ->
	    %% ASN-1 decode of certificate somehow failed
	    ?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN)
    end.

%%--------------------------------------------------------------------
-spec verify_connection(tls_version(), #finished{}, client | server, integer(), binary(),
			tls_handshake_history()) -> verified | #alert{}.
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
-spec init_handshake_history() -> tls_handshake_history().

%%
%% Description: Initialize the empty handshake history buffer.
%%--------------------------------------------------------------------
init_handshake_history() ->
    {[], []}.

%%--------------------------------------------------------------------
-spec update_handshake_history(tls_handshake_history(), Data ::term()) ->
				      tls_handshake_history().
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
    public_key:compute_key(OtherPublicDhKey, MyPrivateKey, Params);

premaster_secret(PublicDhKey, PrivateDhKey, #server_dh_params{dh_p = Prime, dh_g = Base}) ->
    crypto:compute_key(dh, PublicDhKey, PrivateDhKey, [Prime, Base]);
premaster_secret(#client_srp_public{srp_a = ClientPublicKey}, ServerKey, #srp_user{prime = Prime,
										   verifier = Verifier}) ->
    case crypto:compute_key(srp, ClientPublicKey, ServerKey, {host, [Verifier, Prime, '6a']}) of
	error ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
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
		    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
		PremasterSecret ->
		    PremasterSecret
	    end;
	_ ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
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
-spec prf(tls_version(), binary(), binary(), [binary()], non_neg_integer()) ->
		 {ok, binary()} | {error, undefined}.
%%
%% Description: use the TLS PRF to generate key material
%%--------------------------------------------------------------------
prf({3,0}, _, _, _, _) ->
    {error, undefined};
prf({3,1}, Secret, Label, Seed, WantedLength) ->
    {ok, tls_v1:prf(?MD5SHA, Secret, Label, Seed, WantedLength)};
prf({3,_N}, Secret, Label, Seed, WantedLength) ->
    {ok, tls_v1:prf(?SHA256, Secret, Label, Seed, WantedLength)}.
%%--------------------------------------------------------------------
-spec select_hashsign(#hash_sign_algos{}| undefined,  undefined | binary()) ->
			      [{atom(), atom()}] | undefined.

%%
%% Description:
%%--------------------------------------------------------------------
select_hashsign(_, undefined) ->
    {null, anon};
select_hashsign(undefined,  Cert) ->
    #'OTPCertificate'{tbsCertificate = TBSCert} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_,Algo, _}} = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    select_cert_hashsign(undefined, Algo, {undefined, undefined});
select_hashsign(#hash_sign_algos{hash_sign_algos = HashSigns}, Cert) ->
    #'OTPCertificate'{tbsCertificate = TBSCert} =public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_,Algo, _}} = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    DefaultHashSign = {_, Sign} = select_cert_hashsign(undefined, Algo, {undefined, undefined}),
    case lists:filter(fun({sha, dsa}) ->
			      true;
			 ({_, dsa}) ->
			      false;
			 ({Hash, S}) when  S == Sign ->
			      ssl_cipher:is_acceptable_hash(Hash,
							    proplists:get_value(hashs, crypto:supports()));
			 (_)  ->
			      false
		      end, HashSigns) of
	[] ->
	    DefaultHashSign;
	[HashSign| _] ->
	    HashSign
    end.
%%--------------------------------------------------------------------
-spec select_cert_hashsign(#hash_sign_algos{}| undefined, oid(), tls_version() | {undefined, undefined}) ->
				  {atom(), atom()}.

%%
%% Description: For TLS 1.2 selected cert_hash_sign will be recived
%% in the handshake message, for previous versions use appropriate defaults.
%% This function is also used by select_hashsign to extract
%% the alogrithm of the server cert key.
%%--------------------------------------------------------------------
select_cert_hashsign(HashSign, _, {Major, Minor}) when HashSign =/= undefined andalso
						       Major >= 3 andalso Minor >= 3 ->
    HashSign;
select_cert_hashsign(undefined,?'id-ecPublicKey', _) ->
    {sha, ecdsa};
select_cert_hashsign(undefined, ?rsaEncryption, _) ->
    {md5sha, rsa};
select_cert_hashsign(undefined, ?'id-dsa', _) ->
    {sha, dsa}.

%%--------------------------------------------------------------------
-spec master_secret(atom(), tls_version(), #session{} | binary(), #connection_states{},
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
	exit:Reason ->
	    Report = io_lib:format("Key calculation failed due to ~p",
				   [Reason]),
	    error_logger:error_report(Report),
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
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
	exit:Reason ->
	    Report = io_lib:format("Master secret calculation failed"
				   " due to ~p", [Reason]),
	    error_logger:error_report(Report),
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
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
-spec decode_client_key(binary(), key_algo(), tls_version()) ->
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
-spec decode_server_key(binary(), key_algo(), tls_version()) ->
			       #server_key_params{}.
%%
%% Description: Decode server_key data and return appropriate type
%%--------------------------------------------------------------------
decode_server_key(ServerKey, Type, Version) ->
    dec_server_key(ServerKey, key_exchange_alg(Type), Version).

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
decode_handshake(_, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

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
dec_server_key(_, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

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
    case UserSuites of
	[] ->
	    ssl_cipher:suites(Version);
	_ ->
	    UserSuites
    end.

available_suites(ServerCert, UserSuites, Version, Curve) ->
    ssl_cipher:filter(ServerCert, available_suites(UserSuites, Version))
	-- unavailable_ecc_suites(Curve).

unavailable_ecc_suites(no_curve) ->
    ssl_cipher:ec_keyed_suites();
unavailable_ecc_suites(_) ->
    [].

cipher_suites(Suites, false) ->
    [?TLS_EMPTY_RENEGOTIATION_INFO_SCSV | Suites];
cipher_suites(Suites, true) ->
    Suites.

select_session(SuggestedSessionId, CipherSuites, Compressions, Port, #session{ecc = ECCCurve} = 
		   Session, Version,
	       #ssl_options{ciphers = UserSuites} = SslOpts, Cache, CacheCb, Cert) ->
    {SessionId, Resumed} = ssl_session:server_id(Port, SuggestedSessionId,
						 SslOpts, Cert,
						 Cache, CacheCb),
    case Resumed of
        undefined ->
	    Suites = available_suites(Cert, UserSuites, Version, ECCCurve),
	    CipherSuite = select_cipher_suite(CipherSuites, Suites),
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

certificate_types({KeyExchange, _, _, _})
  when KeyExchange == rsa;
       KeyExchange == dhe_dss;
       KeyExchange == dhe_rsa;
       KeyExchange == ecdhe_rsa ->
    <<?BYTE(?RSA_SIGN), ?BYTE(?DSS_SIGN)>>;

certificate_types({KeyExchange, _, _, _})
  when KeyExchange == dh_ecdsa;
       KeyExchange == dhe_ecdsa ->
    <<?BYTE(?ECDSA_SIGN)>>;

certificate_types(_) ->
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
						 next_protocol_negotiation = NextProtocolNegotiation}, Version,
			       #ssl_options{secure_renegotiate = SecureRenegotation} = Opts,
			       #session{cipher_suite = NegotiatedCipherSuite,
					compression_method = Compression} = Session0,
			       ConnectionStates0, Renegotiation) ->
    Session = handle_srp_extension(SRP, Session0),
    ConnectionStates = handle_renegotiation_extension(server, RecordCB, Version, Info,
						      Random, NegotiatedCipherSuite, 
						      ClientCipherSuites, Compression,
						      ConnectionStates0, Renegotiation, SecureRenegotation),
    ProtocolsToAdvertise = handle_next_protocol_extension(NextProtocolNegotiation, Renegotiation, Opts),
   
    ServerHelloExtensions =  #hello_extensions{
				renegotiation_info = renegotiation_info(RecordCB, server,
									ConnectionStates, Renegotiation),
				ec_point_formats = server_ecc_extension(Version, ECCFormat),
				next_protocol_negotiation =
				    encode_protocols_advertised_on_server(ProtocolsToAdvertise)
			       },
    {Session, ConnectionStates, ServerHelloExtensions}.

handle_server_hello_extensions(RecordCB, Random, CipherSuite, Compression,
			       #hello_extensions{renegotiation_info = Info,
						 next_protocol_negotiation = NextProtocolNegotiation}, Version,
			       #ssl_options{secure_renegotiate = SecureRenegotation,
					    next_protocol_selector = NextProtoSelector},
			       ConnectionStates0, Renegotiation) ->
    ConnectionStates = handle_renegotiation_extension(client, RecordCB, Version, Info, Random, 
						      CipherSuite, undefined,
						      Compression, ConnectionStates0,
						      Renegotiation, SecureRenegotation),
    case handle_next_protocol(NextProtocolNegotiation, NextProtoSelector, Renegotiation) of
	#alert{} = Alert ->
	    Alert;
	Protocol ->
	    {ConnectionStates, Protocol}
    end.

select_version(RecordCB, ClientVersion, Versions) ->
    ServerVersion = RecordCB:highest_protocol_version(Versions),
    RecordCB:lowest_protocol_version(ClientVersion, ServerVersion).

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
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end;
handle_renegotiation_info(_RecordCB, server, #renegotiation_info{renegotiated_connection = ClientVerify},
			  ConnectionStates, true, _, CipherSuites) ->

      case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
	      ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	  false ->
	      CS = ssl_record:current_connection_state(ConnectionStates, read),
	      Data = CS#connection_state.client_verify_data,
	      case Data == ClientVerify of
		  true ->
		      {ok, ConnectionStates};
		  false ->
		      ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
	      end
      end;

handle_renegotiation_info(RecordCB, client, undefined, ConnectionStates, true, SecureRenegotation, _) ->
    handle_renegotiation_info(RecordCB, ConnectionStates, SecureRenegotation);

handle_renegotiation_info(RecordCB, server, undefined, ConnectionStates, true, SecureRenegotation, CipherSuites) ->
     case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
	     ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	 false ->
	     handle_renegotiation_info(RecordCB, ConnectionStates, SecureRenegotation)
     end.

handle_renegotiation_info(_RecordCB, ConnectionStates, SecureRenegotation) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    case {SecureRenegotation, CS#connection_state.secure_renegotiation} of
	{_, true} ->
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	{true, false} ->
	    ?ALERT_REC(?FATAL, ?NO_RENEGOTIATION);
	{false, false} ->
	    {ok, ConnectionStates}
    end.

hello_extensions_list(#hello_extensions{renegotiation_info = RenegotiationInfo,
					srp = SRP,
					hash_signs = HashSigns,
					ec_point_formats = EcPointFormats,
					elliptic_curves = EllipticCurves,
					next_protocol_negotiation = NextProtocolNegotiation,
					sni = Sni}) ->
    [Ext || Ext <- [RenegotiationInfo, SRP, HashSigns,
		    EcPointFormats, EllipticCurves, NextProtocolNegotiation, Sni], Ext =/= undefined].

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
apply_user_fun(Fun, OtpCert, ExtensionOrError, UserState0, SslState) ->
    case Fun(OtpCert, ExtensionOrError, UserState0) of
	{valid, UserState} ->
	    {valid, {SslState, UserState}};
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
path_validation_alert({bad_cert, cert_revoked}) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_REVOKED);
path_validation_alert({bad_cert, selfsigned_peer}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, unknown_ca}) ->
     ?ALERT_REC(?FATAL, ?UNKNOWN_CA);
path_validation_alert(_) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE).

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

digitally_signed({3, Minor}, Hash, HashAlgo, Key) when Minor >= 3 ->
    public_key:sign({digest, Hash}, HashAlgo, Key);
digitally_signed(_Version, Hash, HashAlgo, #'DSAPrivateKey'{} = Key) ->
    public_key:sign({digest, Hash}, HashAlgo, Key);
digitally_signed(_Version, Hash, _HashAlgo, #'RSAPrivateKey'{} = Key) ->
    public_key:encrypt_private(Hash, Key,
			       [{rsa_pad, rsa_pkcs1_padding}]);
digitally_signed(_Version, Hash, HashAlgo, Key) ->
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

    ClientCipherState = #cipher_state{iv = ClientIV, key = ClientWriteKey},
    ServerCipherState = #cipher_state{iv = ServerIV, key = ServerWriteKey},
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
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE));
dec_client_key(<<?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #client_diffie_hellman_public{dh_public = DH_Y};
dec_client_key(<<>>, ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN, _) ->
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE));
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
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

dec_hello_extensions(<<>>, Acc) ->
    Acc;
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
    dec_hello_extensions(Rest, Acc#hello_extensions{hash_signs =
						    #hash_sign_algos{hash_sign_algos = HashSignAlgos}});

dec_hello_extensions(<<?UINT16(?ELLIPTIC_CURVES_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Acc) ->
    <<?UINT16(_), EllipticCurveList/binary>> = ExtData,
    EllipticCurves = [tls_v1:enum_to_oid(X) || <<X:16>> <= EllipticCurveList],
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
%% Ignore data following the ClientHello (i.e.,
%% extensions) if not understood.

dec_hello_extensions(<<?UINT16(_), ?UINT16(Len), _Unknown:Len/binary, Rest/binary>>, Acc) ->
    dec_hello_extensions(Rest, Acc);
%% This theoretically should not happen if the protocol is followed, but if it does it is ignored.
dec_hello_extensions(_, Acc) ->
    Acc.

dec_hashsign(<<?BYTE(HashAlgo), ?BYTE(SignAlgo)>>) ->
    {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)}.

decode_next_protocols({next_protocol_negotiation, Protocols}) ->
    decode_next_protocols(Protocols, []).
decode_next_protocols(<<>>, Acc) ->
    lists:reverse(Acc);
decode_next_protocols(<<?BYTE(Len), Protocol:Len/binary, Rest/binary>>, Acc) ->
    case Len of
        0 ->
            {error, invalid_next_protocols};
        _ ->
            decode_next_protocols(Rest, [Protocol|Acc])
    end;
decode_next_protocols(_Bytes, _Acc) ->
    {error, invalid_next_protocols}.

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

handle_next_protocol(undefined,
		     _NextProtocolSelector, _Renegotiating) ->
    undefined;

handle_next_protocol(#next_protocol_negotiation{} = NextProtocols,
    NextProtocolSelector, Renegotiating) ->

    case next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) of
        true ->
            select_next_protocol(decode_next_protocols(NextProtocols), NextProtocolSelector);
        false ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE) % unexpected next protocol extension
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
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE). % unexpected next protocol extension

next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) ->
    NextProtocolSelector =/= undefined andalso not Renegotiating.

select_next_protocol({error, _Reason}, _NextProtocolSelector) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
select_next_protocol(Protocols, NextProtocolSelector) ->
    case NextProtocolSelector(Protocols) of
	?NO_PROTOCOL ->
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	Protocol when is_binary(Protocol)  ->
	    Protocol
    end.

handle_srp_extension(undefined, Session) ->
    Session;
handle_srp_extension(#srp{username = Username}, Session) ->
    Session#session{srp_username = Username}.

%%-------------Misc --------------------------------

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

-define(TLSEXT_SIGALG_RSA(MD), {MD, rsa}).
-define(TLSEXT_SIGALG_DSA(MD), {MD, dsa}).
-define(TLSEXT_SIGALG_ECDSA(MD), {MD, ecdsa}).

-define(TLSEXT_SIGALG(MD), ?TLSEXT_SIGALG_ECDSA(MD), ?TLSEXT_SIGALG_RSA(MD)).

advertised_hash_signs({Major, Minor}) when Major >= 3 andalso Minor >= 3 ->
    HashSigns = [?TLSEXT_SIGALG(sha512),
		 ?TLSEXT_SIGALG(sha384),
		 ?TLSEXT_SIGALG(sha256),
		 ?TLSEXT_SIGALG(sha224),
		 ?TLSEXT_SIGALG(sha),
		 ?TLSEXT_SIGALG_DSA(sha),
		 ?TLSEXT_SIGALG_RSA(md5)],
    CryptoSupport = crypto:supports(),
    HasECC = proplists:get_bool(ecdsa,  proplists:get_value(public_keys, CryptoSupport)),
    Hashs = proplists:get_value(hashs, CryptoSupport),
    #hash_sign_algos{hash_sign_algos =
			 lists:filter(fun({Hash, ecdsa}) -> HasECC andalso proplists:get_bool(Hash, Hashs);
					 ({Hash, _}) -> proplists:get_bool(Hash, Hashs) end, HashSigns)};
advertised_hash_signs(_) ->
    undefined.

psk_secret(PSKIdentity, PSKLookup) ->
    case handle_psk_identity(PSKIdentity, PSKLookup) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = erlang:byte_size(PSK),
	    <<?UINT16(Len), 0:(Len*8), ?UINT16(Len), PSK/binary>>;
	#alert{} = Alert ->
	    Alert;
	_ ->
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
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
	    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER)
    end.

handle_psk_identity(_PSKIdentity, LookupFun)
  when LookupFun == undefined ->
    error;
handle_psk_identity(PSKIdentity, {Fun, UserState}) ->
    Fun(psk, PSKIdentity, UserState).
