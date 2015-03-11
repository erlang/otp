%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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

-module(public_key).

-include("public_key.hrl").

-export([pem_decode/1, pem_encode/1, 
	 der_decode/2, der_encode/2,
	 pem_entry_decode/1,
	 pem_entry_decode/2,
	 pem_entry_encode/2,
	 pem_entry_encode/3,
	 pkix_decode_cert/2, pkix_encode/3,
	 encrypt_private/2, encrypt_private/3,
	 decrypt_private/2, decrypt_private/3, 
	 encrypt_public/2, encrypt_public/3, 
	 decrypt_public/2, decrypt_public/3,
	 sign/3, verify/4,
	 generate_key/1,
	 compute_key/2, compute_key/3,
	 pkix_sign/2, pkix_verify/2,	 
	 pkix_sign_types/1,
	 pkix_is_self_signed/1, 
	 pkix_is_fixed_dh_cert/1,
	 pkix_is_issuer/2,
	 pkix_issuer_id/2,
	 pkix_normalize_name/1,
	 pkix_path_validation/3,
	 ssh_decode/2, ssh_encode/2,
	 pkix_crls_validate/3,
	 pkix_dist_point/1,
	 pkix_dist_points/1,
	 pkix_crl_verify/2,
	 pkix_crl_issuer/1
	]).

-export_type([public_key/0, private_key/0, pem_entry/0,
	      pki_asn1_type/0, asn1_type/0, ssh_file/0, der_encoded/0]).

-type public_key()           ::  rsa_public_key() | dsa_public_key() | ec_public_key().
-type private_key()          ::  rsa_private_key() | dsa_private_key() | ec_private_key().

-type rsa_public_key()       ::  #'RSAPublicKey'{}.
-type rsa_private_key()      ::  #'RSAPrivateKey'{}.
-type dsa_private_key()      ::  #'DSAPrivateKey'{}.
-type dsa_public_key()       :: {integer(), #'Dss-Parms'{}}.
-type ec_public_key()        :: {#'ECPoint'{},{namedCurve, Oid::tuple()} | #'ECParameters'{}}.
-type ec_private_key()       :: #'ECPrivateKey'{}.
-type der_encoded()          :: binary().
-type pki_asn1_type()        ::  'Certificate' | 'RSAPrivateKey' | 'RSAPublicKey'
			       | 'DSAPrivateKey' | 'DSAPublicKey' | 'DHParameter'
                               | 'SubjectPublicKeyInfo' | 'PrivateKeyInfo' | 
				 'CertificationRequest' | 'CertificateList' |
				 'ECPrivateKey' | 'EcpkParameters'.
-type pem_entry()            :: {pki_asn1_type(), 
				 binary(), %% DER or Encrypted DER
				 not_encrypted | {Cipher :: string(), Salt :: binary()} |
				 {Cipher :: string(), #'PBES2-params'{}} | 
				 {Cipher :: string(), {#'PBEParameter'{}, atom()}} %% hash type
				}.
-type asn1_type()            :: atom(). %% see "OTP-PUB-KEY.hrl
-type ssh_file()             :: openssh_public_key | rfc4716_public_key | known_hosts |
				auth_keys.
-type rsa_padding()          :: 'rsa_pkcs1_padding' | 'rsa_pkcs1_oaep_padding' 
			      | 'rsa_no_padding'.
-type public_crypt_options() :: [{rsa_pad, rsa_padding()}].
-type rsa_digest_type()      :: 'md5' | 'sha'| 'sha224' | 'sha256' | 'sha384' | 'sha512'.
-type dss_digest_type()      :: 'none' | 'sha'. %% None is for backwards compatibility
-type ecdsa_digest_type()       :: 'sha'| 'sha224' | 'sha256' | 'sha384' | 'sha512'.
-type crl_reason()           ::  unspecified | keyCompromise | cACompromise | affiliationChanged | superseded
			       | cessationOfOperation | certificateHold | privilegeWithdrawn |  aACompromise.
-type oid()                  :: tuple().

-define(UINT32(X), X:32/unsigned-big-integer).
-define(DER_NULL, <<5, 0>>).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
-spec pem_decode(binary()) -> [pem_entry()].
%%
%% Description: Decode PEM binary data and return
%% entries as asn1 der encoded entities. 
%%--------------------------------------------------------------------
pem_decode(PemBin) when is_binary(PemBin) ->
    pubkey_pem:decode(PemBin).

%%--------------------------------------------------------------------
-spec pem_encode([pem_entry()]) -> binary().
%%
%% Description: Creates a PEM binary.
%%--------------------------------------------------------------------
pem_encode(PemEntries) when is_list(PemEntries) ->
    iolist_to_binary(pubkey_pem:encode(PemEntries)).

%%--------------------------------------------------------------------
-spec pem_entry_decode(pem_entry(), string()) -> term().
%
%% Description: Decodes a pem entry. pem_decode/1 returns a list of
%% pem entries.
%%--------------------------------------------------------------------
pem_entry_decode({'SubjectPublicKeyInfo', Der, _}) ->
    {_, {'AlgorithmIdentifier', AlgId, Params}, {0, Key0}}
        = der_decode('SubjectPublicKeyInfo', Der),
    KeyType = pubkey_cert_records:supportedPublicKeyAlgorithms(AlgId),
    case KeyType of
        'RSAPublicKey' ->
            der_decode(KeyType, Key0);
        'DSAPublicKey' ->
            {params, DssParams} = der_decode('DSAParams', Params),
            {der_decode(KeyType, Key0), DssParams};
        'ECPoint' ->
            der_decode(KeyType, Key0)
    end;
pem_entry_decode({Asn1Type, Der, not_encrypted}) when is_atom(Asn1Type),
						      is_binary(Der) ->
    der_decode(Asn1Type, Der).
pem_entry_decode({Asn1Type, Der, not_encrypted}, _) when is_atom(Asn1Type),
							 is_binary(Der) ->
    der_decode(Asn1Type, Der);
pem_entry_decode({Asn1Type, CryptDer, {Cipher, #'PBES2-params'{}}} = PemEntry, 
		 Password) when is_atom(Asn1Type) andalso
				is_binary(CryptDer) andalso
				is_list(Cipher) ->
    do_pem_entry_decode(PemEntry, Password);
pem_entry_decode({Asn1Type, CryptDer, {Cipher, {#'PBEParameter'{},_}}} = PemEntry, 
 		 Password) when is_atom(Asn1Type) andalso
 				is_binary(CryptDer) andalso
 				is_list(Cipher) andalso
				is_list(Password) ->
    do_pem_entry_decode(PemEntry, Password);
pem_entry_decode({Asn1Type, CryptDer, {Cipher, Salt}} = PemEntry, 
		 Password) when is_atom(Asn1Type) andalso
				is_binary(CryptDer) andalso
				is_list(Cipher) andalso
				is_binary(Salt) andalso
				((erlang:byte_size(Salt) == 8) or (erlang:byte_size(Salt) == 16)) andalso
				is_list(Password) ->
    do_pem_entry_decode(PemEntry, Password).	


%%--------------------------------------------------------------------
-spec pem_entry_encode(pki_asn1_type(), term()) -> pem_entry().
-spec pem_entry_encode(pki_asn1_type(), term(), term()) -> pem_entry().
%%
%% Description: Creates a pem entry that can be feed to pem_encode/1.
%%--------------------------------------------------------------------
pem_entry_encode('SubjectPublicKeyInfo', Entity=#'RSAPublicKey'{}) ->
    Der = der_encode('RSAPublicKey', Entity),
    Spki = {'SubjectPublicKeyInfo',
            {'AlgorithmIdentifier', ?'rsaEncryption', ?DER_NULL}, {0, Der}},
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode('SubjectPublicKeyInfo',
                 {DsaInt, Params=#'Dss-Parms'{}}) when is_integer(DsaInt) ->
    KeyDer = der_encode('DSAPublicKey', DsaInt),
    ParamDer = der_encode('DSAParams', {params, Params}),
    Spki = {'SubjectPublicKeyInfo',
            {'AlgorithmIdentifier', ?'id-dsa', ParamDer}, {0, KeyDer}},
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode(Asn1Type, Entity)  when is_atom(Asn1Type) ->
    Der = der_encode(Asn1Type, Entity),
    {Asn1Type, Der, not_encrypted}.
pem_entry_encode(Asn1Type, Entity, {{Cipher, #'PBES2-params'{}} = CipherInfo, 
				    Password}) when is_atom(Asn1Type) andalso
						    is_list(Password) andalso
						    is_list(Cipher) ->
    do_pem_entry_encode(Asn1Type, Entity, CipherInfo, Password);
pem_entry_encode(Asn1Type, Entity, {{Cipher,
 				     {#'PBEParameter'{}, _}} = CipherInfo,
 				    Password}) when is_atom(Asn1Type) andalso
 						    is_list(Password) andalso
 						    is_list(Cipher) ->
    do_pem_entry_encode(Asn1Type, Entity, CipherInfo, Password);
pem_entry_encode(Asn1Type, Entity, {{Cipher, Salt} = CipherInfo, 
				    Password}) when is_atom(Asn1Type) andalso
						    is_list(Password) andalso
						    is_list(Cipher) andalso
						    is_binary(Salt) andalso
						    ((erlang:byte_size(Salt) == 8) or
						     (erlang:byte_size(Salt) == 16)) ->
    do_pem_entry_encode(Asn1Type, Entity, CipherInfo, Password).
    
%%--------------------------------------------------------------------
-spec der_decode(asn1_type(), Der::binary()) -> term().
%%
%% Description: Decodes a public key asn1 der encoded entity.
%%--------------------------------------------------------------------
der_decode(Asn1Type, Der) when (Asn1Type == 'PrivateKeyInfo') or 
			       (Asn1Type == 'EncryptedPrivateKeyInfo')
			       andalso is_binary(Der) ->
    try
	{ok, Decoded} = 'PKCS-FRAME':decode(Asn1Type, Der),
	Decoded
    catch
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end;

der_decode(Asn1Type, Der) when is_atom(Asn1Type), is_binary(Der) ->
    try 
	{ok, Decoded} = 'OTP-PUB-KEY':decode(Asn1Type, Der),
	Decoded
    catch	    
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end.

%%--------------------------------------------------------------------
-spec der_encode(asn1_type(), term()) -> Der::binary().
%%
%% Description: Encodes a public key entity with asn1 DER encoding.
%%--------------------------------------------------------------------
der_encode(Asn1Type, Entity) when (Asn1Type == 'PrivateKeyInfo') or 
				  (Asn1Type == 'EncryptedPrivateKeyInfo') ->
     try
	{ok, Encoded} = 'PKCS-FRAME':encode(Asn1Type, Entity),
	iolist_to_binary(Encoded)
    catch
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end;

der_encode(Asn1Type, Entity) when is_atom(Asn1Type) ->
    try 
	{ok, Encoded} = 'OTP-PUB-KEY':encode(Asn1Type, Entity),
	iolist_to_binary(Encoded)
    catch	    
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end.

%%--------------------------------------------------------------------
-spec pkix_decode_cert(Cert::binary(), plain | otp) ->
			      #'Certificate'{} | #'OTPCertificate'{}.
%%
%% Description: Decodes an asn1 der encoded pkix certificate. The otp
%% option will use the customized asn1 specification OTP-PKIX.asn1 for
%% decoding and also recursively decode most of the standard
%% extensions.
%% --------------------------------------------------------------------
pkix_decode_cert(DerCert, plain)  when is_binary(DerCert) ->
    der_decode('Certificate', DerCert);
pkix_decode_cert(DerCert, otp) when is_binary(DerCert) ->
    try 
	{ok, #'OTPCertificate'{}= Cert} = 
	    pubkey_cert_records:decode_cert(DerCert),
	Cert
    catch
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end.

%%--------------------------------------------------------------------
-spec pkix_encode(asn1_type(), term(), otp | plain) -> Der::binary().
%%
%% Description: Der encodes a certificate or part of a certificate.
%% This function must be used for encoding certificates or parts of certificates
%% that are decoded with the otp format, whereas for the plain format this
%% function will only call der_encode/2.   
%%--------------------------------------------------------------------
pkix_encode(Asn1Type, Term, plain) when is_atom(Asn1Type) ->
    der_encode(Asn1Type, Term);

pkix_encode(Asn1Type, Term0, otp) when is_atom(Asn1Type) ->
    Term = pubkey_cert_records:transform(Term0, encode),
    der_encode(Asn1Type, Term).

%%--------------------------------------------------------------------
-spec decrypt_private(CipherText :: binary(), rsa_private_key()) -> 
			     PlainText :: binary().
-spec decrypt_private(CipherText :: binary(), rsa_private_key(), 
		      public_crypt_options()) -> PlainText :: binary().
%%
%% Description: Public key decryption using the private key.
%%--------------------------------------------------------------------
decrypt_private(CipherText, Key) ->
    decrypt_private(CipherText, Key, []).

decrypt_private(CipherText,
		#'RSAPrivateKey'{} = Key,
		Options)
  when is_binary(CipherText),
       is_list(Options) ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:private_decrypt(rsa, CipherText, format_rsa_private_key(Key), Padding).

%%--------------------------------------------------------------------
-spec decrypt_public(CipherText :: binary(), rsa_public_key() | rsa_private_key()) ->
			    PlainText :: binary().
-spec decrypt_public(CipherText :: binary(), rsa_public_key() | rsa_private_key(),
		     public_crypt_options()) -> PlainText :: binary().
%% NOTE: The rsa_private_key() is not part of the documented API it is
%% here for testing purposes, in a real situation this is not a relevant
%% thing to do.
%%
%% Description: Public key decryption using the public key.
%%--------------------------------------------------------------------
decrypt_public(CipherText, Key) ->
    decrypt_public(CipherText, Key, []).

decrypt_public(CipherText, #'RSAPublicKey'{modulus = N, publicExponent = E}, 
	       Options) when is_binary(CipherText), is_list(Options)  ->
    decrypt_public(CipherText, N,E, Options);

decrypt_public(CipherText,#'RSAPrivateKey'{modulus = N, publicExponent = E}, 
	       Options) when is_binary(CipherText), is_list(Options) ->
    decrypt_public(CipherText, N,E, Options).

%%--------------------------------------------------------------------
-spec encrypt_public(PlainText :: binary(), rsa_public_key() | rsa_private_key()) ->
			    CipherText :: binary().
-spec encrypt_public(PlainText :: binary(), rsa_public_key() | rsa_private_key(),
		     public_crypt_options()) ->  CipherText :: binary().

%% NOTE: The rsa_private_key() is not part of the documented API it is
%% here for testing purposes, in a real situation this is not a relevant
%% thing to do.
%%
%% Description: Public key encryption using the public key.
%%--------------------------------------------------------------------
encrypt_public(PlainText, Key) ->
    encrypt_public(PlainText, Key, []).

encrypt_public(PlainText, #'RSAPublicKey'{modulus=N,publicExponent=E}, 
	       Options) when is_binary(PlainText), is_list(Options) ->
    encrypt_public(PlainText, N,E, Options);

encrypt_public(PlainText, #'RSAPrivateKey'{modulus=N,publicExponent=E}, 
	       Options) when is_binary(PlainText), is_list(Options) ->
    encrypt_public(PlainText, N,E, Options).

%%--------------------------------------------------------------------
-spec encrypt_private(PlainText :: binary(), rsa_private_key()) -> 
			     CipherText :: binary().
-spec encrypt_private(PlainText :: binary(), rsa_private_key(), 
		      public_crypt_options()) -> CipherText :: binary().
%%
%% Description: Public key encryption using the private key.
%%--------------------------------------------------------------------
encrypt_private(PlainText, Key) ->
    encrypt_private(PlainText, Key, []).

encrypt_private(PlainText,
		#'RSAPrivateKey'{modulus = N, publicExponent = E,
				 privateExponent = D} = Key,
		Options)
  when is_binary(PlainText),
       is_integer(N), is_integer(E), is_integer(D),
       is_list(Options) ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:private_encrypt(rsa, PlainText, format_rsa_private_key(Key), Padding).

%%--------------------------------------------------------------------
-spec generate_key(#'DHParameter'{} | {namedCurve, Name ::oid()} |
		   #'ECParameters'{}) -> {Public::binary(), Private::binary()} |
					    #'ECPrivateKey'{}.
%% Description: Generates a new keypair
%%--------------------------------------------------------------------
generate_key(#'DHParameter'{prime = P, base = G}) ->
    crypto:generate_key(dh, [P, G]);
generate_key({namedCurve, _} = Params) ->
    ec_generate_key(Params);
generate_key(#'ECParameters'{} = Params) ->
    ec_generate_key(Params).

%%--------------------------------------------------------------------
-spec compute_key(#'ECPoint'{} , #'ECPrivateKey'{}) -> binary().
-spec compute_key(OthersKey ::binary(), MyKey::binary(), #'DHParameter'{}) -> binary().
%% Description: Compute shared secret
%%--------------------------------------------------------------------
compute_key(#'ECPoint'{point = Point}, #'ECPrivateKey'{privateKey = PrivKey,
						       parameters = Param}) ->
    ECCurve = ec_curve_spec(Param),
    crypto:compute_key(ecdh, Point, list_to_binary(PrivKey), ECCurve).

compute_key(PubKey, PrivKey, #'DHParameter'{prime = P, base = G}) ->
    crypto:compute_key(dh, PubKey, PrivKey, [P, G]).

%%--------------------------------------------------------------------
-spec pkix_sign_types(SignatureAlg::oid()) ->
			     %% Relevant dsa digest type is subpart of rsa digest type
			     { DigestType :: rsa_digest_type(),
			       SignatureType :: rsa | dsa | ecdsa
			     }.
%% Description:
%%--------------------------------------------------------------------
pkix_sign_types(?sha1WithRSAEncryption) ->
    {sha, rsa};
pkix_sign_types(?'sha-1WithRSAEncryption') ->
    {sha, rsa};
pkix_sign_types(?sha224WithRSAEncryption) ->
    {sha224, rsa};
pkix_sign_types(?sha256WithRSAEncryption) ->
    {sha256, rsa};
pkix_sign_types(?sha384WithRSAEncryption) ->
    {sha384, rsa};
pkix_sign_types(?sha512WithRSAEncryption) ->
    {sha512, rsa};
pkix_sign_types(?md5WithRSAEncryption) ->
    {md5, rsa};
pkix_sign_types(?'id-dsa-with-sha1') ->
    {sha, dsa};
pkix_sign_types(?'id-dsaWithSHA1') ->
    {sha, dsa};
pkix_sign_types(?'ecdsa-with-SHA1') ->
    {sha, ecdsa};
pkix_sign_types(?'ecdsa-with-SHA256') ->
    {sha256, ecdsa};
pkix_sign_types(?'ecdsa-with-SHA384') ->
    {sha384, ecdsa};
pkix_sign_types(?'ecdsa-with-SHA512') ->
    {sha512, ecdsa}.

%%--------------------------------------------------------------------
-spec sign(binary() | {digest, binary()},  rsa_digest_type() | dss_digest_type() | ecdsa_digest_type(),
	   rsa_private_key() |
	   dsa_private_key() | ec_private_key()) -> Signature :: binary().
%% Description: Create digital signature.
%%--------------------------------------------------------------------
sign(DigestOrPlainText, DigestType, Key = #'RSAPrivateKey'{}) ->
    crypto:sign(rsa, DigestType, DigestOrPlainText, format_rsa_private_key(Key));

sign(DigestOrPlainText, sha, #'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) ->
    crypto:sign(dss, sha, DigestOrPlainText, [P, Q, G, X]);

sign(DigestOrPlainText, DigestType, #'ECPrivateKey'{privateKey = PrivKey,
						    parameters = Param}) ->
    ECCurve = ec_curve_spec(Param),
    crypto:sign(ecdsa, DigestType, DigestOrPlainText, [list_to_binary(PrivKey), ECCurve]);

%% Backwards compatible
sign(Digest, none, #'DSAPrivateKey'{} = Key) ->
    sign({digest,Digest}, sha, Key).

%%--------------------------------------------------------------------
-spec verify(binary() | {digest, binary()}, rsa_digest_type() | dss_digest_type() | ecdsa_digest_type(),
	     Signature :: binary(), rsa_public_key()
	     | dsa_public_key() | ec_public_key()) -> boolean().
%% Description: Verifies a digital signature.
%%--------------------------------------------------------------------
verify(DigestOrPlainText, DigestType, Signature,
       #'RSAPublicKey'{modulus = Mod, publicExponent = Exp}) ->
    crypto:verify(rsa, DigestType, DigestOrPlainText, Signature,
		  [Exp, Mod]);

verify(DigestOrPlaintext, DigestType, Signature, {#'ECPoint'{point = Point}, Param}) ->
    ECCurve = ec_curve_spec(Param),
    crypto:verify(ecdsa, DigestType, DigestOrPlaintext, Signature, [Point, ECCurve]);

%% Backwards compatibility
verify(Digest, none, Signature, {_,  #'Dss-Parms'{}} = Key ) ->
    verify({digest,Digest}, sha, Signature, Key);

verify(DigestOrPlainText, sha = DigestType, Signature, {Key,  #'Dss-Parms'{p = P, q = Q, g = G}})
  when is_integer(Key), is_binary(Signature) ->
    crypto:verify(dss, DigestType, DigestOrPlainText, Signature, [P, Q, G, Key]).

%%--------------------------------------------------------------------
-spec pkix_dist_point(der_encoded() | #'OTPCertificate'{}) -> 
			      #'DistributionPoint'{}.  
%% Description:  Creates a distribution point for CRLs issued by the same issuer as <c>Cert</c>.
%%--------------------------------------------------------------------
pkix_dist_point(OtpCert) when is_binary(OtpCert) ->
    pkix_dist_point(pkix_decode_cert(OtpCert, otp));
pkix_dist_point(OtpCert) ->
    Issuer = public_key:pkix_normalize_name(
	       pubkey_cert_records:transform(
		 OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer, encode)),
    
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    Extensions = pubkey_cert:extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    AltNames = case pubkey_cert:select_extension(?'id-ce-issuerAltName', Extensions) of 
		   undefined ->
		       [];
		   #'Extension'{extnValue = Value} ->
		       Value
	       end,
    Point = {fullName, [{directoryName, Issuer} | AltNames]},
    #'DistributionPoint'{cRLIssuer = asn1_NOVALUE,
			 reasons = asn1_NOVALUE,
			 distributionPoint =  Point}.	
%%--------------------------------------------------------------------
-spec pkix_dist_points(der_encoded() | #'OTPCertificate'{}) -> 
			      [#'DistributionPoint'{}].  
%% Description:  Extracts distributionpoints specified in the certificates extensions.
%%--------------------------------------------------------------------
pkix_dist_points(OtpCert) when is_binary(OtpCert) ->
    pkix_dist_points(pkix_decode_cert(OtpCert, otp));
pkix_dist_points(OtpCert) ->
    Value = pubkey_cert:distribution_points(OtpCert),
    lists:foldl(fun(Point, Acc0) ->
			DistPoint = pubkey_cert_records:transform(Point, decode),
			[DistPoint | Acc0]
		end, 
		[], Value).

%%--------------------------------------------------------------------
-spec pkix_sign(#'OTPTBSCertificate'{},
		rsa_private_key() | dsa_private_key()) -> Der::binary().
%%
%% Description: Sign a pkix x.509 certificate. Returns the corresponding
%% der encoded 'Certificate'{}
%%--------------------------------------------------------------------
pkix_sign(#'OTPTBSCertificate'{signature = 
				   #'SignatureAlgorithm'{algorithm = Alg} 
			       = SigAlg} = TBSCert, Key) ->

    Msg = pkix_encode('OTPTBSCertificate', TBSCert, otp),
    {DigestType, _} = pkix_sign_types(Alg),
    Signature = sign(Msg, DigestType, Key),
    Cert = #'OTPCertificate'{tbsCertificate= TBSCert,
			     signatureAlgorithm = SigAlg,
			     signature = {0, Signature}
			    },
    pkix_encode('OTPCertificate', Cert, otp).

%%--------------------------------------------------------------------
-spec pkix_verify(Cert::binary(), rsa_public_key()|
		  dsa_public_key() | ec_public_key()) -> boolean().
%%
%% Description: Verify pkix x.509 certificate signature.
%%--------------------------------------------------------------------
pkix_verify(DerCert, {Key, #'Dss-Parms'{}} = DSAKey) 
  when is_binary(DerCert), is_integer(Key) ->
    {DigestType, PlainText, Signature} = pubkey_cert:verify_data(DerCert),
    verify(PlainText, DigestType, Signature, DSAKey);

pkix_verify(DerCert,  #'RSAPublicKey'{} = RSAKey) 
  when is_binary(DerCert) ->
    {DigestType, PlainText, Signature} = pubkey_cert:verify_data(DerCert),
    verify(PlainText, DigestType, Signature, RSAKey);

pkix_verify(DerCert, Key = {#'ECPoint'{}, _})
  when is_binary(DerCert) ->
    {DigestType, PlainText, Signature} = pubkey_cert:verify_data(DerCert),
    verify(PlainText, DigestType, Signature,  Key).

%%--------------------------------------------------------------------
-spec pkix_crl_verify(CRL::binary() | #'CertificateList'{}, Cert::binary() | #'OTPCertificate'{}) -> boolean().
%%
%% Description: Verify that Cert is the CRL signer.
%%--------------------------------------------------------------------
pkix_crl_verify(CRL, Cert) when is_binary(CRL) ->
    pkix_crl_verify(der_decode('CertificateList', CRL), Cert);
pkix_crl_verify(CRL, Cert) when is_binary(Cert) ->
    pkix_crl_verify(CRL, pkix_decode_cert(Cert, otp));
pkix_crl_verify(#'CertificateList'{} = CRL, #'OTPCertificate'{} = Cert) ->
    TBSCert = Cert#'OTPCertificate'.tbsCertificate, 
    PublicKeyInfo = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PublicKey = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    AlgInfo = PublicKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    PublicKeyParams = AlgInfo#'PublicKeyAlgorithm'.parameters,
    pubkey_crl:verify_crl_signature(CRL, 
				    der_encode('CertificateList', CRL), 
				    PublicKey, PublicKeyParams).

%%--------------------------------------------------------------------
-spec pkix_is_issuer(Cert :: der_encoded()| #'OTPCertificate'{} | #'CertificateList'{},
		     IssuerCert :: der_encoded()|
				   #'OTPCertificate'{}) -> boolean().
%%
%% Description: Checks if <IssuerCert> issued <Cert>.
%%--------------------------------------------------------------------
pkix_is_issuer(Cert, IssuerCert)  when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_issuer(OtpCert, IssuerCert);
pkix_is_issuer(Cert, IssuerCert) when is_binary(IssuerCert) ->
    OtpIssuerCert = pkix_decode_cert(IssuerCert, otp),
    pkix_is_issuer(Cert, OtpIssuerCert);
pkix_is_issuer(#'OTPCertificate'{tbsCertificate = TBSCert}, 
	       #'OTPCertificate'{tbsCertificate = Candidate}) ->
    pubkey_cert:is_issuer(TBSCert#'OTPTBSCertificate'.issuer,
			  Candidate#'OTPTBSCertificate'.subject);
pkix_is_issuer(#'CertificateList'{tbsCertList = TBSCRL},
	       #'OTPCertificate'{tbsCertificate = Candidate}) ->
    pubkey_cert:is_issuer(Candidate#'OTPTBSCertificate'.subject,
			  pubkey_cert_records:transform(TBSCRL#'TBSCertList'.issuer, decode)).

%%--------------------------------------------------------------------
-spec pkix_is_self_signed(Cert::binary()| #'OTPCertificate'{}) -> boolean().
%%
%% Description: Checks if a Certificate is self signed. 
%%--------------------------------------------------------------------
pkix_is_self_signed(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_self_signed(OTPCert);
pkix_is_self_signed(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_self_signed(OtpCert).
  
%%--------------------------------------------------------------------
-spec pkix_is_fixed_dh_cert(Cert::binary()| #'OTPCertificate'{}) -> boolean().
%%
%% Description: Checks if a Certificate is a fixed Diffie-Hellman Cert.
%%--------------------------------------------------------------------
pkix_is_fixed_dh_cert(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_fixed_dh_cert(OTPCert);
pkix_is_fixed_dh_cert(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_fixed_dh_cert(OtpCert).

%%--------------------------------------------------------------------
-spec pkix_issuer_id(Cert::binary()| #'OTPCertificate'{},
		     IssuedBy :: self | other) ->
			    {ok, {SerialNr :: integer(),
				  Issuer :: {rdnSequence,
					     [#'AttributeTypeAndValue'{}]}}}
				| {error, Reason :: term()}.
%
%% Description: Returns the issuer id.
%%--------------------------------------------------------------------
pkix_issuer_id(#'OTPCertificate'{} = OtpCert, Signed) when (Signed == self) or 
							   (Signed == other) ->
    pubkey_cert:issuer_id(OtpCert, Signed);
pkix_issuer_id(Cert, Signed) when is_binary(Cert) -> 
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_issuer_id(OtpCert, Signed).

%%--------------------------------------------------------------------
-spec pkix_crl_issuer(CRL::binary()| #'CertificateList'{}) -> 
			     {rdnSequence,
			      [#'AttributeTypeAndValue'{}]}.
%
%% Description: Returns the issuer.
%%--------------------------------------------------------------------
pkix_crl_issuer(CRL) when is_binary(CRL) ->
    pkix_crl_issuer(der_decode('CertificateList', CRL));
pkix_crl_issuer(#'CertificateList'{} = CRL) ->
    pubkey_cert_records:transform(
      CRL#'CertificateList'.tbsCertList#'TBSCertList'.issuer, decode).

%%--------------------------------------------------------------------
-spec pkix_normalize_name({rdnSequence,
				   [#'AttributeTypeAndValue'{}]}) ->
					 {rdnSequence, 
					  [#'AttributeTypeAndValue'{}]}.
%%
%% Description: Normalizes a issuer name so that it can be easily
%%              compared to another issuer name. 
%%--------------------------------------------------------------------
pkix_normalize_name(Issuer) -> 
    pubkey_cert:normalize_general_name(Issuer).

%%-------------------------------------------------------------------- 
-spec pkix_path_validation(Cert::binary()| #'OTPCertificate'{} | atom(),
			   CertChain :: [binary()] ,
			   Options :: proplists:proplist()) ->
				  {ok, {PublicKeyInfo :: term(), 
					PolicyTree :: term()}} |
				  {error, {bad_cert, Reason :: term()}}.
%% Description: Performs a basic path validation according to RFC 5280.
%%--------------------------------------------------------------------
pkix_path_validation(PathErr, [Cert | Chain], Options0) when is_atom(PathErr)->
    {VerifyFun, Userstat0} =
	proplists:get_value(verify_fun, Options0, ?DEFAULT_VERIFYFUN),
    Otpcert = otp_cert(Cert),
    Reason = {bad_cert, PathErr},
    try VerifyFun(Otpcert, Reason, Userstat0) of
	{valid, Userstate} ->
	    Options = proplists:delete(verify_fun, Options0),
	    pkix_path_validation(Otpcert, Chain, [{verify_fun,
						   {VerifyFun, Userstate}}| Options]);
	{fail, _} ->
	    {error, Reason}
    catch
	_:_ ->
	    {error, Reason}
    end;
pkix_path_validation(TrustedCert, CertChain, Options)
  when is_binary(TrustedCert) ->
    OtpCert = pkix_decode_cert(TrustedCert, otp),
    pkix_path_validation(OtpCert, CertChain, Options);

pkix_path_validation(#'OTPCertificate'{} = TrustedCert, CertChain, Options)
  when is_list(CertChain), is_list(Options) ->
    MaxPathDefault = length(CertChain),
    ValidationState = pubkey_cert:init_validation_state(TrustedCert, 
							MaxPathDefault, 
							Options),
    path_validation(CertChain, ValidationState).

%--------------------------------------------------------------------
-spec pkix_crls_validate(#'OTPCertificate'{},
			 [{DP::#'DistributionPoint'{}, {DerCRL::binary(), CRL::#'CertificateList'{}}}],
			 Options :: proplists:proplist()) -> valid | {bad_cert, revocation_status_undetermined}
								| {bad_cert, {revoked, crl_reason()}}.

%% Description: Performs a CRL validation according to RFC 5280.
%%--------------------------------------------------------------------
pkix_crls_validate(OtpCert, [{_,_,_} |_] = DPAndCRLs, Options) ->
    pkix_crls_validate(OtpCert, DPAndCRLs, DPAndCRLs,
		       Options, pubkey_crl:init_revokation_state());

pkix_crls_validate(OtpCert, DPAndCRLs0, Options) ->
    CallBack = proplists:get_value(update_crl, Options, fun(_, CurrCRL) ->
							       CurrCRL
						       end),
    DPAndCRLs = sort_dp_crls(DPAndCRLs0, CallBack),
    pkix_crls_validate(OtpCert, DPAndCRLs, DPAndCRLs,
		       Options, pubkey_crl:init_revokation_state()).


%%--------------------------------------------------------------------
-spec ssh_decode(binary(), public_key | ssh_file()) -> [{public_key(), Attributes::list()}].
%%
%% Description: Decodes a ssh file-binary. In the case of know_hosts
%% or auth_keys the binary may include one or more lines of the
%% file. Returns a list of public keys and their attributes, possible
%% attribute values depends on the file type represented by the
%% binary.
%%--------------------------------------------------------------------
ssh_decode(SshBin, Type) when is_binary(SshBin),
			      Type == public_key;
			      Type == rfc4716_public_key;
			      Type == openssh_public_key;
			      Type == auth_keys;
			      Type == known_hosts ->
    pubkey_ssh:decode(SshBin, Type).

%%--------------------------------------------------------------------
-spec ssh_encode([{public_key(), Attributes::list()}], ssh_file()) ->
			binary().
%% Description: Encodes a list of ssh file entries (public keys and
%% attributes) to a binary. Possible attributes depends on the file
%% type.
%%--------------------------------------------------------------------
ssh_encode(Entries, Type) when is_list(Entries),
			       Type == rfc4716_public_key;
			       Type == openssh_public_key;
			       Type == auth_keys;
			       Type == known_hosts ->
    pubkey_ssh:encode(Entries, Type).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_pem_entry_encode(Asn1Type, Entity, CipherInfo, Password) ->
    Der = der_encode(Asn1Type, Entity),
    DecryptDer = pubkey_pem:cipher(Der, CipherInfo, Password),
    {Asn1Type, DecryptDer, CipherInfo}.
  
do_pem_entry_decode({Asn1Type,_, _} = PemEntry, Password) ->
    Der = pubkey_pem:decipher(PemEntry, Password),
    der_decode(Asn1Type, Der).

encrypt_public(PlainText, N, E, Options)->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:public_encrypt(rsa, PlainText, [E,N], Padding).

decrypt_public(CipherText, N,E, Options) ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:public_decrypt(rsa, CipherText,[E, N], Padding).

path_validation([], #path_validation_state{working_public_key_algorithm
					   = Algorithm,
					   working_public_key =
					   PublicKey,
					   working_public_key_parameters 
					   = PublicKeyParams,
					   valid_policy_tree = Tree
					  }) ->
    {ok, {{Algorithm, PublicKey, PublicKeyParams}, Tree}};

path_validation([DerCert | Rest], ValidationState = #path_validation_state{
				    max_path_length = Len}) when Len >= 0 ->
    try validate(DerCert,
		 ValidationState#path_validation_state{last_cert=Rest=:=[]}) of
	#path_validation_state{} = NewValidationState ->
	    path_validation(Rest, NewValidationState)
    catch   
	throw:Reason ->
	    {error, Reason}
    end;

path_validation([Cert | _] = Path,
		#path_validation_state{user_state = UserState0,
				       verify_fun = VerifyFun} =
		    ValidationState) ->
    Reason = {bad_cert, max_path_length_reached},
    OtpCert = otp_cert(Cert),

    try VerifyFun(OtpCert,  Reason, UserState0) of
	{valid, UserState} ->
	    path_validation(Path,
			    ValidationState#path_validation_state{
			      max_path_length = 0,
			      user_state = UserState});
	{fail, _} ->
	    {error, Reason}
    catch
	_:_ ->
	    {error, Reason}
    end.

validate(Cert, #path_validation_state{working_issuer_name = Issuer,
					 working_public_key = Key,
					 working_public_key_parameters = 
					 KeyParams, 
					 permitted_subtrees = Permit,
					 excluded_subtrees = Exclude,
					 last_cert = Last,
					 user_state = UserState0,
					 verify_fun = VerifyFun} =
	     ValidationState0) ->

    OtpCert = otp_cert(Cert),

    {ValidationState1, UserState1} =
	pubkey_cert:validate_extensions(OtpCert, ValidationState0, UserState0,
					VerifyFun),

    %% We want the key_usage extension to be checked before we validate
    %% other things so that CRL validation errors will comply to standard
    %% test suite description

    UserState2 = pubkey_cert:validate_time(OtpCert, UserState1, VerifyFun),

    UserState3 = pubkey_cert:validate_issuer(OtpCert, Issuer, UserState2, VerifyFun),

    UserState4 = pubkey_cert:validate_names(OtpCert, Permit, Exclude, Last,
					    UserState3, VerifyFun),

    UserState5 = pubkey_cert:validate_signature(OtpCert, der_cert(Cert),
						Key, KeyParams, UserState4, VerifyFun),
    UserState = case Last of
		    false ->
			pubkey_cert:verify_fun(OtpCert, valid, UserState5, VerifyFun);
		    true ->
			pubkey_cert:verify_fun(OtpCert, valid_peer,
					       UserState5, VerifyFun)
		end,

    ValidationState  = 
	ValidationState1#path_validation_state{user_state = UserState},

    pubkey_cert:prepare_for_next_cert(OtpCert, ValidationState).

otp_cert(Der) when is_binary(Der) ->
    pkix_decode_cert(Der, otp);
otp_cert(#'OTPCertificate'{} =Cert) ->
    Cert.

der_cert(#'OTPCertificate'{} = Cert) ->
    pkix_encode('OTPCertificate', Cert, otp);
der_cert(Der) when is_binary(Der) ->
    Der.

pkix_crls_validate(_, [],_, _, _) ->
    {bad_cert, revocation_status_undetermined};
pkix_crls_validate(OtpCert, [{DP, CRL, DeltaCRL} | Rest],  All, Options, RevokedState0) ->
    CallBack = proplists:get_value(update_crl, Options, fun(_, CurrCRL) ->
							       CurrCRL
						       end),
    case pubkey_crl:fresh_crl(DP, CRL, CallBack) of
	{fresh, CRL} ->
	    do_pkix_crls_validate(OtpCert, [{DP, CRL, DeltaCRL} | Rest],
				  All, Options, RevokedState0);
	{fresh, NewCRL} ->
	    NewAll = [{DP, NewCRL, DeltaCRL} | All -- [{DP, CRL, DeltaCRL}]],
	    do_pkix_crls_validate(OtpCert, [{DP, NewCRL, DeltaCRL} | Rest],
				  NewAll, Options, RevokedState0);
	no_fresh_crl ->
	    pkix_crls_validate(OtpCert, Rest, All, Options, RevokedState0)
    end.

do_pkix_crls_validate(OtpCert, [{DP, CRL, DeltaCRL} | Rest],  All, Options, RevokedState0) ->
    OtherDPCRLs = All -- [{DP, CRL, DeltaCRL}],
    case pubkey_crl:validate(OtpCert, OtherDPCRLs, DP, CRL, DeltaCRL, Options, RevokedState0) of
	{undetermined, _, _} when Rest == []->
	    {bad_cert, revocation_status_undetermined};
	{undetermined, _, RevokedState} when Rest =/= []->
	    pkix_crls_validate(OtpCert, Rest, All, Options, RevokedState);
	{finished, unrevoked} ->
	    valid;
	{finished, Status} ->
	    {bad_cert, Status}
    end.

sort_dp_crls(DpsAndCrls, FreshCB) ->
    Sorted = do_sort_dp_crls(DpsAndCrls, dict:new()),
    sort_crls(Sorted, FreshCB, []).

do_sort_dp_crls([], Dict) ->
    dict:to_list(Dict);
do_sort_dp_crls([{DP, CRL} | Rest], Dict0) ->
    Dict = try dict:fetch(DP, Dict0) of
	       _ ->
		   dict:append(DP, CRL, Dict0)
	   catch _:_ ->
		   dict:store(DP, [CRL], Dict0)
	   end,
    do_sort_dp_crls(Rest, Dict).

sort_crls([], _, Acc) ->
    Acc;

sort_crls([{DP, AllCRLs} | Rest], FreshCB, Acc)->
    {DeltaCRLs, CRLs} = do_sort_crls(AllCRLs),
    DpsAndCRLs = combine(CRLs, DeltaCRLs, DP, FreshCB, []),
    sort_crls(Rest, FreshCB, DpsAndCRLs ++ Acc).

do_sort_crls(CRLs) ->
    lists:partition(fun({_, CRL}) ->
			    pubkey_crl:is_delta_crl(CRL)
		    end, CRLs).

combine([], _,_,_,Acc) ->
    Acc;
combine([{_, CRL} = Entry | CRLs], DeltaCRLs, DP, FreshCB, Acc) ->
    DeltaCRL = combine(CRL, DeltaCRLs),
    case pubkey_crl:fresh_crl(DP, DeltaCRL, FreshCB) of
	no_fresh_crl ->
	    combine(CRLs, DeltaCRLs, DP, FreshCB, [{DP, Entry, {undefined, undefined}} | Acc]);
	{fresh, NewDeltaCRL} ->
	    combine(CRLs, DeltaCRLs, DP, FreshCB, [{DP, Entry, NewDeltaCRL} | Acc])
    end.

combine(CRL, DeltaCRLs) ->
    Deltas = lists:filter(fun({_,DeltaCRL}) ->
				  pubkey_crl:combines(CRL, DeltaCRL)
			  end, DeltaCRLs),
    case Deltas of
	[] ->
	    {undefined, undefined};
	[Delta] ->
	    Delta;
	[_,_|_] ->
	    Fun =
		fun({_, #'CertificateList'{tbsCertList = FirstTBSCRL}} = CRL1,
		    {_, #'CertificateList'{tbsCertList = SecondTBSCRL}} = CRL2) ->
			Time1 = pubkey_cert:time_str_2_gregorian_sec(
				  FirstTBSCRL#'TBSCertList'.thisUpdate),
			Time2 = pubkey_cert:time_str_2_gregorian_sec(
				  SecondTBSCRL#'TBSCertList'.thisUpdate),
			case Time1 > Time2 of
			      true ->
				CRL1;
			    false ->
				CRL2
			end
		end,
	    lists:foldl(Fun,  hd(Deltas), tl(Deltas))
    end.

format_rsa_private_key(#'RSAPrivateKey'{modulus = N, publicExponent = E,
					privateExponent = D,
					prime1 = P1, prime2 = P2,
					exponent1 = E1, exponent2 = E2,
					coefficient = C})
  when is_integer(N), is_integer(E), is_integer(D),
       is_integer(P1), is_integer(P2),
       is_integer(E1), is_integer(E2), is_integer(C) ->
   [E, N, D, P1, P2, E1, E2, C];

format_rsa_private_key(#'RSAPrivateKey'{modulus = N, publicExponent = E,
					privateExponent = D}) when is_integer(N),
								   is_integer(E),
								   is_integer(D) ->
   [E, N, D].

ec_generate_key(Params) ->
    Curve = ec_curve_spec(Params),
    Term = crypto:generate_key(ecdh, Curve),
    ec_key(Term, Params).

ec_curve_spec( #'ECParameters'{fieldID = FieldId, curve = PCurve, base = Base, order = Order, cofactor = CoFactor }) ->
    Field = {pubkey_cert_records:supportedCurvesTypes(FieldId#'FieldID'.fieldType),
	     FieldId#'FieldID'.parameters},
    Curve = {erlang:list_to_binary(PCurve#'Curve'.a), erlang:list_to_binary(PCurve#'Curve'.b), none},
    {Field, Curve, erlang:list_to_binary(Base), Order, CoFactor};
ec_curve_spec({namedCurve, OID}) ->
    pubkey_cert_records:namedCurves(OID).

ec_key({PubKey, PrivateKey}, Params) ->
    #'ECPrivateKey'{version = 1,
		    privateKey = binary_to_list(PrivateKey),
		    parameters = Params,
		    publicKey = {0, PubKey}}.

