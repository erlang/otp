%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
	 dh_gex_group/4,
	 dh_gex_group_sizes/0,
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
	 pkix_verify_hostname/2, pkix_verify_hostname/3,
	 ssh_decode/2, ssh_encode/2,
	 ssh_hostkey_fingerprint/1, ssh_hostkey_fingerprint/2,
	 ssh_curvename2oid/1, oid2ssh_curvename/1,
	 pkix_crls_validate/3,
	 pkix_dist_point/1,
	 pkix_dist_points/1,
	 pkix_match_dist_point/2,
	 pkix_crl_verify/2,
	 pkix_crl_issuer/1,
	 short_name_hash/1
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
-type ecdsa_digest_type()    :: 'sha'| 'sha224' | 'sha256' | 'sha384' | 'sha512'.
-type digest_type()          :: rsa_digest_type() |  dss_digest_type() | ecdsa_digest_type().
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
    {_, {'AlgorithmIdentifier', AlgId, Params}, Key0}
        = der_decode('SubjectPublicKeyInfo', Der),
    KeyType = pubkey_cert_records:supportedPublicKeyAlgorithms(AlgId),
    case KeyType of
        'RSAPublicKey' ->
            der_decode(KeyType, Key0);
        'DSAPublicKey' ->
            {params, DssParams} = der_decode('DSAParams', Params),
            {der_decode(KeyType, Key0), DssParams};
        'ECPoint' ->
	    ECCParams = der_decode('EcpkParameters', Params),
            {#'ECPoint'{point = Key0}, ECCParams}
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
            {'AlgorithmIdentifier', ?'rsaEncryption', ?DER_NULL}, Der},
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode('SubjectPublicKeyInfo',
                 {DsaInt, Params=#'Dss-Parms'{}}) when is_integer(DsaInt) ->
    KeyDer = der_encode('DSAPublicKey', DsaInt),
    ParamDer = der_encode('DSAParams', {params, Params}),
    Spki = {'SubjectPublicKeyInfo',
            {'AlgorithmIdentifier', ?'id-dsa', ParamDer}, KeyDer},
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode('SubjectPublicKeyInfo',
		 {#'ECPoint'{point = Key}, ECParam}) when is_binary(Key)->
    Params = der_encode('EcpkParameters',ECParam),
    Spki = {'SubjectPublicKeyInfo',
	    {'AlgorithmIdentifier', ?'id-ecPublicKey', Params},
	    Key},
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
	Encoded
    catch
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end;

der_encode(Asn1Type, Entity) when is_atom(Asn1Type) ->
    try 
	{ok, Encoded} = 'OTP-PUB-KEY':encode(Asn1Type, Entity),
	Encoded
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
dh_gex_group_sizes() ->
    pubkey_ssh:dh_gex_group_sizes().

dh_gex_group(Min, N, Max, Groups) ->
    pubkey_ssh:dh_gex_group(Min, N, Max, Groups).

%%--------------------------------------------------------------------
-spec generate_key(#'DHParameter'{}) ->
                          {Public::binary(), Private::binary()};
                  ({namedCurve, Name ::oid()}) ->
                          #'ECPrivateKey'{};
                  (#'ECParameters'{}) ->
                          #'ECPrivateKey'{};
                  ({rsa, Size::pos_integer(), PubExp::pos_integer()}) ->
                          #'RSAPrivateKey'{}.

%% Description: Generates a new keypair
%%--------------------------------------------------------------------
generate_key(#'DHParameter'{prime = P, base = G}) ->
    crypto:generate_key(dh, [P, G]);
generate_key({namedCurve, _} = Params) ->
    ec_generate_key(Params);
generate_key(#'ECParameters'{} = Params) ->
    ec_generate_key(Params);
generate_key({rsa, ModulusSize, PublicExponent}) ->
    case crypto:generate_key(rsa, {ModulusSize,PublicExponent}) of
        {[E, N], [E, N, D, P, Q, D_mod_P_1, D_mod_Q_1, InvQ_mod_P]} ->
            Nint = crypto:bytes_to_integer(N),
            Eint = crypto:bytes_to_integer(E),
            #'RSAPrivateKey'{version = 0, % Two-factor (I guess since otherPrimeInfos is not given)
                             modulus = Nint,
                             publicExponent = Eint,
                             privateExponent = crypto:bytes_to_integer(D),
                             prime1 = crypto:bytes_to_integer(P),
                             prime2 = crypto:bytes_to_integer(Q),
                             exponent1 = crypto:bytes_to_integer(D_mod_P_1),
                             exponent2 = crypto:bytes_to_integer(D_mod_Q_1),
                             coefficient = crypto:bytes_to_integer(InvQ_mod_P)};

        {[E, N], [E, N, D]} -> % FIXME: what to set the other fields in #'RSAPrivateKey'?
                               % Answer: Miller [Mil76]
                               %   G.L. Miller. Riemann's hypothesis and tests for primality.
                               %   Journal of Computer and Systems Sciences,
                               %   13(3):300-307,
                               %   1976.
            Nint = crypto:bytes_to_integer(N),
            Eint = crypto:bytes_to_integer(E),
            #'RSAPrivateKey'{version = 0, % Two-factor (I guess since otherPrimeInfos is not given)
                              modulus = Nint,
                              publicExponent = Eint,
                              privateExponent = crypto:bytes_to_integer(D),
                              prime1 = '?',
                              prime2 = '?',
                              exponent1 = '?',
                              exponent2 = '?',
                              coefficient = '?'};
        
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
-spec compute_key(#'ECPoint'{} , #'ECPrivateKey'{}) -> binary().
-spec compute_key(OthersKey ::binary(), MyKey::binary(), #'DHParameter'{}) -> binary().
%% Description: Compute shared secret
%%--------------------------------------------------------------------
compute_key(#'ECPoint'{point = Point}, #'ECPrivateKey'{privateKey = PrivKey,
						       parameters = Param}) ->
    ECCurve = ec_curve_spec(Param),
    crypto:compute_key(ecdh, Point, PrivKey, ECCurve).

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
    crypto:sign(ecdsa, DigestType, DigestOrPlainText, [PrivKey, ECCurve]);

%% Backwards compatible
sign(Digest, none, #'DSAPrivateKey'{} = Key) ->
    sign({digest,Digest}, sha, Key).

%%--------------------------------------------------------------------
-spec verify(binary() | {digest, binary()}, rsa_digest_type() | dss_digest_type() | ecdsa_digest_type(),
	     Signature :: binary(), rsa_public_key()
	     | dsa_public_key() | ec_public_key()) -> boolean().
%% Description: Verifies a digital signature.
%%--------------------------------------------------------------------
verify(DigestOrPlainText, DigestType, Signature, Key) when is_binary(Signature) ->
    do_verify(DigestOrPlainText, DigestType, Signature, Key);
verify(_,_,_,_) -> 
    %% If Signature is a bitstring and not a binary we know already at this
    %% point that the signature is invalid.
    false.

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
-spec pkix_match_dist_point(der_encoded() | #'CertificateList'{},
			    #'DistributionPoint'{}) -> boolean().
%% Description: Check whether the given distribution point matches
%% the "issuing distribution point" of the CRL.
%%--------------------------------------------------------------------
pkix_match_dist_point(CRL, DistPoint) when is_binary(CRL) ->
    pkix_match_dist_point(der_decode('CertificateList', CRL), DistPoint);
pkix_match_dist_point(#'CertificateList'{},
		      #'DistributionPoint'{distributionPoint = asn1_NOVALUE}) ->
    %% No distribution point name specified - that's considered a match.
    true;
pkix_match_dist_point(#'CertificateList'{
			 tbsCertList =
			     #'TBSCertList'{
				crlExtensions = Extensions}},
		      #'DistributionPoint'{
			 distributionPoint = {fullName, DPs}}) ->
    case pubkey_cert:select_extension(?'id-ce-issuingDistributionPoint', Extensions) of
	undefined ->
	    %% If the CRL doesn't have an IDP extension, it
	    %% automatically qualifies.
	    true;
	#'Extension'{extnValue = IDPValue} ->
	    %% If the CRL does have an IDP extension, it must match
	    %% the given DistributionPoint to be considered a match.
	    IDPEncoded = der_decode('IssuingDistributionPoint', IDPValue),
	    #'IssuingDistributionPoint'{distributionPoint = {fullName, IDPs}} =
		pubkey_cert_records:transform(IDPEncoded, decode),
	    pubkey_crl:match_one(IDPs, DPs)
    end.

%%--------------------------------------------------------------------
-spec pkix_sign(#'OTPTBSCertificate'{},
		rsa_private_key() | dsa_private_key() | ec_private_key()) -> Der::binary().
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
			     signature = Signature
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

%--------------------------------------------------------------------
-spec pkix_verify_hostname(Cert :: #'OTPCertificate'{} | binary(),
			   ReferenceIDs :: [{uri_id | dns_id | oid(),  string()}]) -> boolean().

-spec pkix_verify_hostname(Cert :: #'OTPCertificate'{} | binary(),
			   ReferenceIDs :: [{uri_id | dns_id | oid(),  string()}],
			   Options :: proplists:proplist()) -> boolean().

%% Description: Validates a hostname to RFC 6125
%%--------------------------------------------------------------------
pkix_verify_hostname(Cert, ReferenceIDs) ->
    pkix_verify_hostname(Cert, ReferenceIDs, []).

pkix_verify_hostname(BinCert, ReferenceIDs, Options)  when is_binary(BinCert) ->
    pkix_verify_hostname(pkix_decode_cert(BinCert,otp), ReferenceIDs, Options);

pkix_verify_hostname(Cert = #'OTPCertificate'{tbsCertificate = TbsCert}, ReferenceIDs0, Opts) ->
    MatchFun = proplists:get_value(match_fun,     Opts, undefined),
    FailCB   = proplists:get_value(fail_callback, Opts, fun(_Cert) -> false end),
    FqdnFun  = proplists:get_value(fqdn_fun,      Opts, fun verify_hostname_extract_fqdn_default/1),

    ReferenceIDs = [{T,to_string(V)} || {T,V} <- ReferenceIDs0],
    PresentedIDs =
	try lists:keyfind(?'id-ce-subjectAltName',
			  #'Extension'.extnID,
			  TbsCert#'OTPTBSCertificate'.extensions)
	of
	    #'Extension'{extnValue = ExtVals} ->
		[{T,to_string(V)} || {T,V} <- ExtVals];
	    false ->
		[]
	catch
	    _:_ -> []
	end,
    %% PresentedIDs example: [{dNSName,"ewstest.ericsson.com"}, {dNSName,"www.ericsson.com"}]}
    case PresentedIDs of
	[] ->
	    %% Fallback to CN-ids [rfc6125, ch6]
	    case TbsCert#'OTPTBSCertificate'.subject of
		{rdnSequence,RDNseq} ->
		    PresentedCNs =
			[{cn, to_string(V)}
			 || ATVs <- RDNseq, % RDNseq is list-of-lists
			    #'AttributeTypeAndValue'{type = ?'id-at-commonName',
						     value = {_T,V}} <- ATVs
						% _T = kind of string (teletexString etc)
			],
		    %% Example of PresentedCNs:  [{cn,"www.ericsson.se"}]
		    %% match ReferenceIDs to PresentedCNs
		    verify_hostname_match_loop(verify_hostname_fqnds(ReferenceIDs, FqdnFun),
					       PresentedCNs,
					       MatchFun, FailCB, Cert);
		
		_ ->
		    false
	    end;
	_ ->
	    %% match ReferenceIDs to PresentedIDs
	    case verify_hostname_match_loop(ReferenceIDs, PresentedIDs,
					    MatchFun, FailCB, Cert) of
		false ->
		    %% Try to extract DNS-IDs from URIs etc
		    DNS_ReferenceIDs =
			[{dns_is,X} || X <- verify_hostname_fqnds(ReferenceIDs, FqdnFun)],
		    verify_hostname_match_loop(DNS_ReferenceIDs, PresentedIDs,
					       MatchFun, FailCB, Cert);
		true ->
		    true
	    end
    end.

%%--------------------------------------------------------------------
-spec ssh_decode(binary(), public_key | ssh_file()) -> [{public_key(), Attributes::list()}]
	      ; (binary(), ssh2_pubkey) ->  public_key()
	      .
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
			      Type == known_hosts;
			      Type == ssh2_pubkey ->
    pubkey_ssh:decode(SshBin, Type).

%%--------------------------------------------------------------------
-spec ssh_encode([{public_key(), Attributes::list()}], ssh_file()) -> binary()
	      ; (public_key(), ssh2_pubkey) -> binary()
	      ; ({public_key(),atom()}, ssh2_pubkey) -> binary()
	      .
%%
%% Description: Encodes a list of ssh file entries (public keys and
%% attributes) to a binary. Possible attributes depends on the file
%% type.
%%--------------------------------------------------------------------
ssh_encode(Entries, Type) when is_list(Entries),
			       Type == rfc4716_public_key;
			       Type == openssh_public_key;
			       Type == auth_keys;
			       Type == known_hosts;
			       Type == ssh2_pubkey ->
    pubkey_ssh:encode(Entries, Type).

%%--------------------------------------------------------------------
-spec ssh_curvename2oid(binary()) -> oid().

%% Description: Converts from the ssh name of elliptic curves to
%% the OIDs.
%%--------------------------------------------------------------------
ssh_curvename2oid(<<"nistp256">>) ->  ?'secp256r1';
ssh_curvename2oid(<<"nistp384">>) ->  ?'secp384r1';
ssh_curvename2oid(<<"nistp521">>) ->  ?'secp521r1'.

%%--------------------------------------------------------------------
-spec oid2ssh_curvename(oid()) -> binary().

%% Description: Converts from elliptic curve OIDs to the ssh name.
%%--------------------------------------------------------------------
oid2ssh_curvename(?'secp256r1') -> <<"nistp256">>;
oid2ssh_curvename(?'secp384r1') -> <<"nistp384">>;
oid2ssh_curvename(?'secp521r1') -> <<"nistp521">>.

%%--------------------------------------------------------------------
-spec ssh_hostkey_fingerprint(public_key()) -> string().
-spec ssh_hostkey_fingerprint( digest_type(),  public_key()) ->  string()
                           ; ([digest_type()], public_key())   -> [string()]
                           .

ssh_hostkey_fingerprint(Key) ->
    sshfp_string(md5, public_key:ssh_encode(Key,ssh2_pubkey) ).

ssh_hostkey_fingerprint(HashAlgs, Key) when is_list(HashAlgs) ->
    EncKey = public_key:ssh_encode(Key, ssh2_pubkey),
    [sshfp_full_string(HashAlg,EncKey) || HashAlg <- HashAlgs];
ssh_hostkey_fingerprint(HashAlg, Key) when is_atom(HashAlg) ->
    EncKey = public_key:ssh_encode(Key, ssh2_pubkey),
    sshfp_full_string(HashAlg, EncKey).


sshfp_string(HashAlg, EncodedKey) ->
    %% Other HashAlgs than md5 will be printed with
    %% other formats than hextstr by
    %%    ssh-keygen -E <alg> -lf <file>
    fp_fmt(sshfp_fmt(HashAlg), crypto:hash(HashAlg, EncodedKey)).

sshfp_full_string(HashAlg, EncKey) ->
    lists:concat([sshfp_alg_name(HashAlg),
		  [$: | sshfp_string(HashAlg, EncKey)]
		 ]).

sshfp_alg_name(sha) -> "SHA1";
sshfp_alg_name(Alg) -> string:to_upper(atom_to_list(Alg)).

sshfp_fmt(md5) -> hexstr;
sshfp_fmt(_) -> b64.

fp_fmt(hexstr, Bin) ->
    lists:flatten(string:join([io_lib:format("~2.16.0b",[C1]) || <<C1>> <= Bin], ":"));
fp_fmt(b64, Bin) ->
    %% This function clause *seems* to be
    %%    [C || C<-base64:encode_to_string(Bin), C =/= $=]
    %% but I am not sure. Must be checked.
    B64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    BitsInLast = 8*size(Bin) rem 6,
    Padding = (6-BitsInLast) rem 6, % Want BitsInLast = [1:5] to map to padding [5:1] and 0 -> 0
    [lists:nth(C+1,B64Chars) || <<C:6>> <= <<Bin/binary,0:Padding>> ].

%%--------------------------------------------------------------------
-spec short_name_hash({rdnSequence, [#'AttributeTypeAndValue'{}]}) ->
			     string().

%% Description: Generates OpenSSL-style hash of a name.
%%--------------------------------------------------------------------
short_name_hash({rdnSequence, _Attributes} = Name) ->
    HashThis = encode_name_for_short_hash(Name),
    <<HashValue:32/little, _/binary>> = crypto:hash(sha, HashThis),
    string:to_lower(string:right(integer_to_list(HashValue, 16), 8, $0)).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_verify(DigestOrPlainText, DigestType, Signature,
       #'RSAPublicKey'{modulus = Mod, publicExponent = Exp}) ->
    crypto:verify(rsa, DigestType, DigestOrPlainText, Signature,
		  [Exp, Mod]);

do_verify(DigestOrPlaintext, DigestType, Signature, {#'ECPoint'{point = Point}, Param}) ->
    ECCurve = ec_curve_spec(Param),
    crypto:verify(ecdsa, DigestType, DigestOrPlaintext, Signature, [Point, ECCurve]);

%% Backwards compatibility
do_verify(Digest, none, Signature, {_,  #'Dss-Parms'{}} = Key ) ->
    verify({digest,Digest}, sha, Signature, Key);

do_verify(DigestOrPlainText, sha = DigestType, Signature, {Key,  #'Dss-Parms'{p = P, q = Q, g = G}})
  when is_integer(Key), is_binary(Signature) ->
    crypto:verify(dss, DigestType, DigestOrPlainText, Signature, [P, Q, G, Key]).

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
    sort_crls(maps:to_list(lists:foldl(fun group_dp_crls/2,
                                       #{},
                                       DpsAndCrls)),
              FreshCB, []).

group_dp_crls({DP,CRL}, M) ->
    case M of
        #{DP := CRLs} -> M#{DP := [CRL|CRLs]};
        _ -> M#{DP => [CRL]}
    end.

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
    Curve = {PCurve#'Curve'.a, PCurve#'Curve'.b, none},
    {Field, Curve, Base, Order, CoFactor};
ec_curve_spec({namedCurve, OID}) when is_tuple(OID), is_integer(element(1,OID)) ->
    ec_curve_spec({namedCurve,  pubkey_cert_records:namedCurves(OID)});
ec_curve_spec({namedCurve, Name}) when is_atom(Name) ->
    crypto:ec_curve(Name).


ec_key({PubKey, PrivateKey}, Params) ->
    #'ECPrivateKey'{version = 1,
		    privateKey = PrivateKey,
		    parameters = Params,
		    publicKey = PubKey}.

encode_name_for_short_hash({rdnSequence, Attributes0}) ->
    Attributes = lists:map(fun normalise_attribute/1, Attributes0),
    {Encoded, _} = 'OTP-PUB-KEY':'enc_RDNSequence'(Attributes, []),
    Encoded.

%% Normalise attribute for "short hash".  If the attribute value
%% hasn't been decoded yet, decode it so we can normalise it.
normalise_attribute([#'AttributeTypeAndValue'{
                        type = _Type,
                        value = Binary} = ATV]) when is_binary(Binary) ->
    case pubkey_cert_records:transform(ATV, decode) of
	#'AttributeTypeAndValue'{value = Binary} ->
	    %% Cannot decode attribute; return original.
	    [ATV];
	DecodedATV = #'AttributeTypeAndValue'{} ->
	    %% The new value will either be String or {Encoding,String}.
	    normalise_attribute([DecodedATV])
    end;
normalise_attribute([#'AttributeTypeAndValue'{
                        type = _Type,
                        value = {Encoding, String}} = ATV])
  when
      Encoding =:= utf8String;
      Encoding =:= printableString;
      Encoding =:= teletexString;
      Encoding =:= ia5String ->
    %% These string types all give us something that the unicode
    %% module understands.
    NewValue = normalise_attribute_value(String),
    [ATV#'AttributeTypeAndValue'{value = NewValue}];
normalise_attribute([#'AttributeTypeAndValue'{
                        type = _Type,
                        value = String} = ATV]) when is_list(String) ->
    %% A string returned by pubkey_cert_records:transform/2, for
    %% certain attributes that commonly have incorrect value types.
    NewValue = normalise_attribute_value(String),
    [ATV#'AttributeTypeAndValue'{value = NewValue}].

normalise_attribute_value(String) ->
    Converted = unicode:characters_to_binary(String),
    NormalisedString = normalise_string(Converted),
    %% We can't use the encoding function for the actual type of the
    %% attribute, since some of them don't allow utf8Strings, which is
    %% the required encoding when creating the hash.
    {NewBinary, _} = 'OTP-PUB-KEY':'enc_X520CommonName'({utf8String, NormalisedString}, []),
    NewBinary.

normalise_string(String) ->
    %% Normalise attribute values as required for "short hashes", as
    %% implemented by OpenSSL.

    %% Remove ASCII whitespace from beginning and end.
    TrimmedLeft = re:replace(String, "^[\s\f\n\r\t\v]+", "", [unicode, global]),
    TrimmedRight = re:replace(TrimmedLeft, "[\s\f\n\r\t\v]+$", "", [unicode, global]),
    %% Convert multiple whitespace characters to a single space.
    Collapsed = re:replace(TrimmedRight, "[\s\f\n\r\t\v]+", "\s", [unicode, global]),
    %% Convert ASCII characters to lowercase
    Lower = ascii_to_lower(Collapsed),
    %% And we're done!
    Lower.

ascii_to_lower(String) ->
    %% Can't use string:to_lower/1, because that changes Latin-1
    %% characters as well.
    << <<(if $A =< C, C =< $Z ->
                  C + ($a - $A);
             true ->
                  C
          end)>>
       ||
        <<C>> <= iolist_to_binary(String) >>.

%%%----------------------------------------------------------------
%%% pkix_verify_hostname help functions
verify_hostname_extract_fqdn_default({dns_id,S}) ->
    S;
verify_hostname_extract_fqdn_default({uri_id,URI}) ->
    {ok,{https,_,Host,_,_,_}} = http_uri:parse(URI),
    Host.


verify_hostname_fqnds(L, FqdnFun) ->
    [E || E0 <- L,
	  E <- [try case FqdnFun(E0) of
			default -> verify_hostname_extract_fqdn_default(E0);
                        undefined -> undefined; % will make the "is_list(E)" test fail
			Other -> Other
		    end
		catch _:_-> undefined % will make the "is_list(E)" test fail
		end],
	  is_list(E),
	  E =/= "",
	  {error,einval} == inet:parse_address(E)
    ].


-define(srvName_OID, {1,3,6,1,4,1,434,2,2,1,37,0}).

verify_hostname_match_default(Ref, Pres) ->
    verify_hostname_match_default0(to_lower_ascii(Ref), to_lower_ascii(Pres)).

verify_hostname_match_default0(FQDN=[_|_], {cn,FQDN}) -> 
    not lists:member($*, FQDN);
verify_hostname_match_default0(FQDN=[_|_], {cn,Name=[_|_]}) -> 
    [F1|Fs] = string:tokens(FQDN, "."),
    [N1|Ns] = string:tokens(Name, "."),
    match_wild(F1,N1) andalso Fs==Ns;
verify_hostname_match_default0({dns_id,R}, {dNSName,P}) ->
    R==P;
verify_hostname_match_default0({uri_id,R}, {uniformResourceIdentifier,P}) ->
    R==P;
verify_hostname_match_default0({srv_id,R}, {T,P}) when T == srvName ;
                                                       T == ?srvName_OID ->
    R==P;
verify_hostname_match_default0(_, _) ->
    false.


match_wild(A,     [$*|B]) -> match_wild_suffixes(A, B);
match_wild([C|A], [ C|B]) -> match_wild(A, B);
match_wild([],        []) -> true;
match_wild(_,          _) -> false.

%% Match the parts after the only wildcard by comparing them from the end
match_wild_suffixes(A, B) -> match_wild_sfx(lists:reverse(A), lists:reverse(B)).

match_wild_sfx([$*|_],      _) -> false; % Bad name (no wildcards alowed)
match_wild_sfx(_,      [$*|_]) -> false; % Bad pattern (no more wildcards alowed)
match_wild_sfx([A|Ar], [A|Br]) -> match_wild_sfx(Ar, Br);
match_wild_sfx(Ar,         []) -> not lists:member($*, Ar); % Chk for bad name (= wildcards)
match_wild_sfx(_,           _) -> false.
    

verify_hostname_match_loop(Refs0, Pres0, undefined, FailCB, Cert) ->
    Pres = lists:map(fun to_lower_ascii/1, Pres0),
    Refs = lists:map(fun to_lower_ascii/1, Refs0),
    lists:any(
      fun(R) ->
	      lists:any(fun(P) ->
                                verify_hostname_match_default(R,P) orelse FailCB(Cert)
			end, Pres)
      end, Refs);
verify_hostname_match_loop(Refs, Pres, MatchFun, FailCB, Cert) ->
    lists:any(
      fun(R) ->
	      lists:any(fun(P) ->
				(case MatchFun(R,P) of
				     default -> verify_hostname_match_default(R,P);
				     Bool -> Bool
				 end) orelse FailCB(Cert)
			end,
			Pres)
      end,
      Refs).


to_lower_ascii(S) when is_list(S) -> lists:map(fun to_lower_ascii/1, S);
to_lower_ascii({T,S}) -> {T, to_lower_ascii(S)};
to_lower_ascii(C) when $A =< C,C =< $Z -> C + ($a-$A);
to_lower_ascii(C) -> C.

to_string(S) when is_list(S) -> S;
to_string(B) when is_binary(B) -> binary_to_list(B).

