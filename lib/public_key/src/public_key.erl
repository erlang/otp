%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
	 sign/3, sign/4, verify/4, verify/5,
	 generate_key/1,
	 compute_key/2, compute_key/3,
	 pkix_sign/2, pkix_verify/2,
	 pkix_hash_type/1,
	 pkix_sign_types/1,
	 pkix_is_self_signed/1, 
	 pkix_is_fixed_dh_cert/1,
	 pkix_is_issuer/2,
	 pkix_issuer_id/2,
	 pkix_subject_id/1,
         pkix_normalize_name/1,
	 pkix_path_validation/3,
	 pkix_verify_hostname/2, pkix_verify_hostname/3,
         pkix_verify_hostname_match_fun/1,
	 ssh_decode/2, ssh_encode/2,
	 ssh_hostkey_fingerprint/1, ssh_hostkey_fingerprint/2,
	 ssh_curvename2oid/1, oid2ssh_curvename/1,
	 pkix_crls_validate/3,
	 pkix_dist_point/1,
	 pkix_dist_points/1,
	 pkix_match_dist_point/2,
	 pkix_crl_verify/2,
	 pkix_crl_issuer/1,
	 short_name_hash/1,
         pkix_test_data/1,
         pkix_test_root_cert/2,
     ocsp_status/3,
     ocsp_responses/3,
     ocsp_nonce/0,
     ocsp_responder_id/1,
     ocsp_extensions/1
	]).

-export_type([public_key/0, private_key/0, pem_entry/0,
	      pki_asn1_type/0, asn1_type/0, ssh_file/0, der_encoded/0,
              key_params/0, digest_type/0, issuer_name/0, cert_id/0, oid/0]).

-type public_key()           ::  rsa_public_key() | rsa_pss_public_key() | dsa_public_key() | ec_public_key() | ed_public_key() .
-type private_key()          ::  rsa_private_key() | rsa_pss_private_key() | dsa_private_key() | ec_private_key() | ed_private_key() .
-type rsa_public_key()       ::  #'RSAPublicKey'{}.
-type rsa_private_key()      ::  #'RSAPrivateKey'{}. 
-type rsa_pss_public_key()   ::  {#'RSAPublicKey'{}, #'RSASSA-PSS-params'{}}.
-type rsa_pss_private_key()  ::  { #'RSAPrivateKey'{}, #'RSASSA-PSS-params'{}}.
-type dsa_private_key()      ::  #'DSAPrivateKey'{}.
-type dsa_public_key()       :: {integer(), #'Dss-Parms'{}}.
-type ecpk_parameters() :: {ecParameters, #'ECParameters'{}} | {namedCurve, Oid::tuple()}.
-type ecpk_parameters_api() :: ecpk_parameters() | #'ECParameters'{} | {namedCurve, Name::crypto:ec_named_curve()}.
-type ec_public_key()        :: {#'ECPoint'{}, ecpk_parameters_api()}.
-type ec_private_key()       :: #'ECPrivateKey'{}.
-type ed_public_key()        :: {ed_pub, ed25519|ed448, Key::binary()}.
-type ed_private_key()       :: {ed_pri, ed25519|ed448, Pub::binary(), Priv::binary()}.

-type key_params()           :: #'DHParameter'{} | {namedCurve, oid()} | #'ECParameters'{} | 
                                {rsa, Size::integer(), PubExp::integer()}. 
-type der_encoded()          :: binary().
-type pki_asn1_type()        ::  'Certificate' | 'RSAPrivateKey' | 'RSAPublicKey'
			       | 'DSAPrivateKey' | 'DSAPublicKey' | 'DHParameter'
                               | 'SubjectPublicKeyInfo' | 'PrivateKeyInfo' | 
				 'CertificationRequest' | 'CertificateList' |
				 'ECPrivateKey' | 'EcpkParameters'.
-type pem_entry()            :: {pki_asn1_type(), 
				 der_or_encrypted_der(),
				 not_encrypted | cipher_info()
				}.
-type der_or_encrypted_der() :: binary().
-type cipher_info()          :: {cipher(),
                                 cipher_info_params()} .
-type cipher()               :: string() . % "RC2-CBC" | "DES-CBC" | "DES-EDE3-CBC", 
-type cipher_info_params()   :: salt()
                              | {#'PBEParameter'{}, digest_type()}
                              | #'PBES2-params'{} .

-type salt()                 :: binary(). % crypto:strong_rand_bytes(8)
%% -type cipher_info()          :: {Cipher :: string(), Salt :: binary()} |
%%                                 {Cipher :: string(), #'PBES2-params'{}} | 
%%                                 {Cipher :: string(), {#'PBEParameter'{}, atom()}} %% hash type
%%                                 .

-type asn1_type()            :: atom(). %% see "OTP-PUB-KEY.hrl
-type ssh_file()             :: openssh_public_key | rfc4716_public_key | known_hosts |
				auth_keys.
-type digest_type()          :: none % None is for backwards compatibility
                              | sha1 % Backwards compatibility
                              | crypto:rsa_digest_type()
                              | crypto:dss_digest_type()
                              | crypto:ecdsa_digest_type().
-type crl_reason()           ::  unspecified | keyCompromise | cACompromise | affiliationChanged | superseded
			       | cessationOfOperation | certificateHold | privilegeWithdrawn |  aACompromise.
-type oid()                  :: tuple().
-type chain_type()           :: server_chain | client_chain.

-type cert_id()              :: {SerialNr::integer(), issuer_name()} .
-type issuer_name()          :: {rdnSequence,[#'AttributeTypeAndValue'{}]} .



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
%% Description: Decodes a pem entry. pem_decode/1 returns a list of
%% pem entries.
%%--------------------------------------------------------------------
-spec pem_entry_decode(PemEntry) -> term() when PemEntry :: pem_entry() .

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
pem_entry_decode({{no_asn1,new_openssh}, Special, not_encrypted}) ->
    ssh_decode(Special, new_openssh);
pem_entry_decode({Asn1Type, Der, not_encrypted}) when is_atom(Asn1Type),
						      is_binary(Der) ->
    der_decode(Asn1Type, Der).

-spec pem_entry_decode(PemEntry, Password) -> term() when PemEntry :: pem_entry(),
                                                          Password :: string() .
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
%%
%% Description: Creates a pem entry that can be feed to pem_encode/1.
%%--------------------------------------------------------------------
-spec pem_entry_encode(Asn1Type, Entity) -> pem_entry() when Asn1Type :: pki_asn1_type(),
                                                             Entity :: term() .

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

-spec pem_entry_encode(Asn1Type, Entity, InfoPwd) ->
                              pem_entry() when Asn1Type :: pki_asn1_type(),
                                               Entity :: term(),
                                               InfoPwd :: {CipherInfo,Password},
                                               CipherInfo :: cipher_info(),
                                               Password :: string() .
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
-spec der_decode(Asn1Type, Der) -> Entity when Asn1Type :: asn1_type(),
                                               Der :: binary(),
                                               Entity :: term().
%%
%% Description: Decodes a public key asn1 der encoded entity.
%%--------------------------------------------------------------------
der_decode(Asn1Type, Der) when (Asn1Type == 'PrivateKeyInfo') or 
			       (Asn1Type == 'EncryptedPrivateKeyInfo')
			       andalso is_binary(Der) ->
    try
	{ok, Decoded} = 'PKCS-FRAME':decode(Asn1Type, Der),
	der_priv_key_decode(Decoded)
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

der_priv_key_decode({'PrivateKeyInfo', v1,
	{'PrivateKeyInfo_privateKeyAlgorithm', ?'id-ecPublicKey', {asn1_OPENTYPE, Parameters}}, PrivKey, _}) ->
	EcPrivKey = der_decode('ECPrivateKey', PrivKey),
	EcPrivKey#'ECPrivateKey'{parameters = der_decode('EcpkParameters', Parameters)};
der_priv_key_decode({'PrivateKeyInfo', v1,
	{'PrivateKeyInfo_privateKeyAlgorithm', ?'rsaEncryption', _}, PrivKey, _}) ->
	der_decode('RSAPrivateKey', PrivKey);
der_priv_key_decode({'PrivateKeyInfo', v1,
                     {'PrivateKeyInfo_privateKeyAlgorithm', ?'id-RSASSA-PSS', 
                      {asn1_OPENTYPE, Parameters}}, PrivKey, _}) ->
    Key = der_decode('RSAPrivateKey', PrivKey),
    Params = der_decode('RSASSA-PSS-params', Parameters),
    {Key, Params};
der_priv_key_decode({'PrivateKeyInfo', v1,
	{'PrivateKeyInfo_privateKeyAlgorithm', ?'id-dsa', {asn1_OPENTYPE, Parameters}}, PrivKey, _}) ->
	{params, #'Dss-Parms'{p=P, q=Q, g=G}} = der_decode('DSAParams', Parameters),
	X = der_decode('Prime-p', PrivKey),
	#'DSAPrivateKey'{p=P, q=Q, g=G, x=X};
der_priv_key_decode(PKCS8Key) ->
	PKCS8Key.

%%--------------------------------------------------------------------
-spec der_encode(Asn1Type, Entity) -> Der when Asn1Type :: asn1_type(),
                                               Entity :: term(),
                                               Der :: binary() .
%%
%% Description: Encodes a public key entity with asn1 DER encoding.
%%--------------------------------------------------------------------
der_encode('PrivateKeyInfo', #'DSAPrivateKey'{p=P, q=Q, g=G, x=X}) ->
    der_encode('PrivateKeyInfo',
               {'PrivateKeyInfo', v1,
                {'PrivateKeyInfo_privateKeyAlgorithm', ?'id-dsa',
                 {asn1_OPENTYPE, der_encode('Dss-Parms', #'Dss-Parms'{p=P, q=Q, g=G})}},
		der_encode('Prime-p', X), asn1_NOVALUE});
der_encode('PrivateKeyInfo', #'RSAPrivateKey'{} = PrivKey) ->
    der_encode('PrivateKeyInfo',
               {'PrivateKeyInfo', v1,
                {'PrivateKeyInfo_privateKeyAlgorithm', ?'rsaEncryption', 
                 {asn1_OPENTYPE, ?DER_NULL}},
                der_encode('RSAPrivateKey', PrivKey), asn1_NOVALUE});
der_encode('PrivateKeyInfo', {#'RSAPrivateKey'{} = PrivKey, Parameters}) ->
    der_encode('PrivateKeyInfo',
               {'PrivateKeyInfo', v1,
                {'PrivateKeyInfo_privateKeyAlgorithm', ?'id-RSASSA-PSS', 
                 {asn1_OPENTYPE, der_encode('RSASSA-PSS-params', Parameters)}},
                der_encode('RSAPrivateKey', PrivKey), asn1_NOVALUE});
der_encode('PrivateKeyInfo', #'ECPrivateKey'{parameters = Parameters} = PrivKey) ->
    der_encode('PrivateKeyInfo',
               {'PrivateKeyInfo', v1,
		{'PrivateKeyInfo_privateKeyAlgorithm', ?'id-ecPublicKey',
                 {asn1_OPENTYPE, der_encode('EcpkParameters', Parameters)}},
                der_encode('ECPrivateKey', PrivKey#'ECPrivateKey'{parameters = asn1_NOVALUE}), 
                asn1_NOVALUE});
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
-spec pkix_decode_cert(Cert, Type) ->
			      #'Certificate'{} | #'OTPCertificate'{} 
                                  when Cert :: der_encoded(),
                                       Type :: plain | otp .
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
-spec pkix_encode(Asn1Type, Entity, Type) -> Der
                                                 when Asn1Type :: asn1_type(),
                                                      Entity :: term(),
                                                      Type :: otp | plain,
                                                      Der :: der_encoded() .
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
%%
%% Description: Public key decryption using the private key.
%%--------------------------------------------------------------------
-spec decrypt_private(CipherText, Key) ->
                             PlainText when CipherText :: binary(),
                                            Key :: rsa_private_key(),
                                            PlainText ::  binary() .
decrypt_private(CipherText, Key) ->
    decrypt_private(CipherText, Key, []).

-spec decrypt_private(CipherText, Key, Options) ->
                             PlainText when CipherText :: binary(),
                                            Key :: rsa_private_key(),
                                            Options :: crypto:pk_encrypt_decrypt_opts(),
                                            PlainText ::  binary() .
decrypt_private(CipherText,
		#'RSAPrivateKey'{} = Key,
		Options)
  when is_binary(CipherText),
       is_list(Options) ->
    crypto:private_decrypt(rsa, CipherText, format_rsa_private_key(Key), default_options(Options)).

%%--------------------------------------------------------------------
%% Description: Public key decryption using the public key.
%%--------------------------------------------------------------------
-spec decrypt_public(CipherText, Key) ->
			    PlainText
                                when CipherText :: binary(),
                                     Key :: rsa_public_key(),
                                     PlainText :: binary() .
decrypt_public(CipherText, Key) ->
    decrypt_public(CipherText, Key, []).

-spec decrypt_public(CipherText, Key, Options) ->
			    PlainText
                                when CipherText :: binary(),
                                     Key :: rsa_public_key(),
                                     Options :: crypto:pk_encrypt_decrypt_opts(),
                                     PlainText :: binary() .
decrypt_public(CipherText, #'RSAPublicKey'{modulus = N, publicExponent = E}, 
	       Options) when is_binary(CipherText), is_list(Options)  ->
    crypto:public_decrypt(rsa, CipherText,[E, N], default_options(Options)).

%%--------------------------------------------------------------------
%% Description: Public key encryption using the public key.
%%--------------------------------------------------------------------
-spec encrypt_public(PlainText, Key) ->
			     CipherText
                                 when  PlainText :: binary(),
                                       Key :: rsa_public_key(),
                                       CipherText :: binary() .
encrypt_public(PlainText, Key) ->
    encrypt_public(PlainText, Key, []).


-spec encrypt_public(PlainText, Key, Options) ->
			     CipherText
                                 when  PlainText :: binary(),
                                       Key :: rsa_public_key(),
                                       Options :: crypto:pk_encrypt_decrypt_opts(),
                                       CipherText :: binary() .
encrypt_public(PlainText, #'RSAPublicKey'{modulus=N,publicExponent=E}, 
	       Options) when is_binary(PlainText), is_list(Options) ->
    crypto:public_encrypt(rsa, PlainText, [E,N], default_options(Options)).

%%--------------------------------------------------------------------
%%
%% Description: Public key encryption using the private key.
%%--------------------------------------------------------------------
-spec encrypt_private(PlainText, Key) ->
			     CipherText
                                 when  PlainText :: binary(),
                                       Key :: rsa_private_key(),
                                       CipherText :: binary() .
encrypt_private(PlainText, Key) ->
    encrypt_private(PlainText, Key, []).


-spec encrypt_private(PlainText, Key, Options) ->
			     CipherText
                                 when  PlainText :: binary(),
                                       Key :: rsa_private_key(),
                                       Options :: crypto:pk_encrypt_decrypt_opts(),
                                       CipherText :: binary() .
encrypt_private(PlainText,
		#'RSAPrivateKey'{modulus = N, publicExponent = E,
				 privateExponent = D} = Key,
		Options)
  when is_binary(PlainText),
       is_integer(N), is_integer(E), is_integer(D),
       is_list(Options) ->
    crypto:private_encrypt(rsa, PlainText, format_rsa_private_key(Key), default_options(Options)).

%%--------------------------------------------------------------------
%% Description: List available group sizes among the pre-computed dh groups
%%--------------------------------------------------------------------
-spec dh_gex_group_sizes() -> [pos_integer()].
dh_gex_group_sizes() ->
    pubkey_ssh:dh_gex_group_sizes().

%%--------------------------------------------------------------------
%% Description: Select a precomputed group
%%--------------------------------------------------------------------
-spec dh_gex_group(MinSize, SuggestedSize, MaxSize, Groups) ->
                          {ok,{Size,Group}} | {error,term()}
                              when MinSize :: pos_integer(),
                                   SuggestedSize :: pos_integer(),
                                   MaxSize :: pos_integer(),
                                   Groups :: undefined | [{Size,[Group]}],
                                   Size :: pos_integer(),
                                   Group :: {G,P},
                                   G :: pos_integer(),
                                   P :: pos_integer() .
dh_gex_group(Min, N, Max, Groups) ->
    pubkey_ssh:dh_gex_group(Min, N, Max, Groups).

%%--------------------------------------------------------------------
%% Description: Generate a new key pair
%%--------------------------------------------------------------------
-spec generate_key(DHparams | ECparams | RSAparams) ->
                          DHkeys | ECkey | RSAkey
                              when DHparams :: #'DHParameter'{},
                                   DHkeys :: {PublicDH::binary(), PrivateDH::binary()},
                                   ECparams :: ecpk_parameters_api(),
                                   ECkey :: #'ECPrivateKey'{},
                                   RSAparams :: {rsa, Size, PubExp},
                                   Size::pos_integer(),
                                   PubExp::pos_integer(),
                                   RSAkey :: #'RSAPrivateKey'{} .

generate_key(#'DHParameter'{prime = P, base = G}) ->
    crypto:generate_key(dh, [P, G]);
generate_key({namedCurve, _} = Params) ->
    ec_generate_key(Params);
generate_key({ecParameters, _} = Params) ->
    ec_generate_key(Params);
generate_key(#'ECParameters'{} = Params) ->
    ec_generate_key(Params);
generate_key({rsa, ModulusSize, PublicExponent}) ->
    case crypto:generate_key(rsa, {ModulusSize,PublicExponent}) of
        {[E, N], [E, N, D, P, Q, D_mod_P_1, D_mod_Q_1, InvQ_mod_P]} ->
            Nint = crypto:bytes_to_integer(N),
            Eint = crypto:bytes_to_integer(E),
            #'RSAPrivateKey'{version = 'two-prime', % Two-factor (I guess since otherPrimeInfos is not given)
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
            #'RSAPrivateKey'{version = 'two-prime', % Two-factor (I guess since otherPrimeInfos is not given)
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
%% Description: Compute shared secret
%%--------------------------------------------------------------------
-spec compute_key(OthersECDHkey, MyECDHkey) -> 
                         SharedSecret
                             when OthersECDHkey :: #'ECPoint'{},
                                  MyECDHkey :: #'ECPrivateKey'{},
                                  SharedSecret :: binary().
compute_key(#'ECPoint'{point = Point}, #'ECPrivateKey'{privateKey = PrivKey,
						       parameters = Param}) ->
    ECCurve = ec_curve_spec(Param),
    crypto:compute_key(ecdh, Point, PrivKey, ECCurve).

-spec compute_key(OthersDHkey, MyDHkey, DHparms) -> 
                         SharedSecret
                             when OthersDHkey :: crypto:dh_public(), % Was: binary(),
                                  MyDHkey :: crypto:dh_private(), % Was: binary(),
                                  DHparms ::  #'DHParameter'{},
                                  SharedSecret :: binary().
compute_key(PubKey, PrivKey, #'DHParameter'{prime = P, base = G}) ->
    crypto:compute_key(dh, PubKey, PrivKey, [P, G]).

%%--------------------------------------------------------------------
-spec pkix_sign_types(AlgorithmId) -> 
                             {DigestType, SignatureType}
                                 when AlgorithmId :: oid(),
                                      %% Relevant dsa digest type is a subset of rsa_digest_type()
                                      DigestType :: crypto:rsa_digest_type(),
                                      SignatureType :: rsa | dsa | ecdsa .
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
pkix_sign_types(?'id-dsa-with-sha224') ->
    {sha224, dsa};
pkix_sign_types(?'id-dsa-with-sha256') ->
    {sha256, dsa};
pkix_sign_types(?'ecdsa-with-SHA1') ->
    {sha, ecdsa};
pkix_sign_types(?'ecdsa-with-SHA256') ->
    {sha256, ecdsa};
pkix_sign_types(?'ecdsa-with-SHA384') ->
    {sha384, ecdsa};
pkix_sign_types(?'ecdsa-with-SHA512') ->
    {sha512, ecdsa}.

%%--------------------------------------------------------------------
-spec pkix_hash_type(HashOid::oid()) -> DigestType:: md5 | crypto:sha1() | crypto:sha2().
          
pkix_hash_type(?'id-sha1') ->
    sha;
pkix_hash_type(?'id-sha512') ->
    sha512;
pkix_hash_type(?'id-sha384') ->
    sha384;
pkix_hash_type(?'id-sha256') ->
    sha256;
pkix_hash_type('id-sha224') ->
    sha224;
pkix_hash_type('id-md5') ->
    md5.

%%--------------------------------------------------------------------
%% Description: Create digital signature.
%%--------------------------------------------------------------------
-spec sign(Msg, DigestType, Key) ->
                  Signature when Msg ::  binary() | {digest,binary()},
                                 DigestType :: digest_type(),
                                 Key :: private_key(),
                                 Signature :: binary() .
sign(DigestOrPlainText, DigestType, Key) ->
    sign(DigestOrPlainText, DigestType, Key, []).

-spec sign(Msg, DigestType, Key, Options) ->
                  Signature when Msg ::  binary() | {digest,binary()},
                                 DigestType :: digest_type(),
                                 Key :: private_key(),
                                 Options :: crypto:pk_sign_verify_opts(),
                                 Signature :: binary() .
sign(Digest, none, Key = #'DSAPrivateKey'{}, Options) when is_binary(Digest) ->
    %% Backwards compatible
    sign({digest, Digest}, sha, Key, Options);
sign(DigestOrPlainText, DigestType, Key, Options) ->
    case format_sign_key(Key) of
	badarg ->
	    erlang:error(badarg, [DigestOrPlainText, DigestType, Key, Options]);
	{Algorithm, CryptoKey} ->
	    crypto:sign(Algorithm, DigestType, DigestOrPlainText, CryptoKey, Options)
    end.

%%--------------------------------------------------------------------
%% Description: Verifies a digital signature.
%%--------------------------------------------------------------------
-spec verify(Msg, DigestType, Signature, Key) ->
                    boolean() when Msg :: binary() | {digest, binary()},
                                   DigestType :: digest_type(),
                                   Signature :: binary(),
                                   Key :: public_key() .

verify(DigestOrPlainText, DigestType, Signature, Key) ->
    verify(DigestOrPlainText, DigestType, Signature, Key, []).

-spec verify(Msg, DigestType, Signature, Key, Options) ->
                    boolean() when Msg :: binary() | {digest, binary()},
                                   DigestType :: digest_type(),
                                   Signature :: binary(),
                                   Key :: public_key(),
                                   Options :: crypto:pk_sign_verify_opts().

verify(Digest, none, Signature, Key = {_, #'Dss-Parms'{}}, Options) when is_binary(Digest) ->
    %% Backwards compatible
    verify({digest, Digest}, sha, Signature, Key, Options);
verify(DigestOrPlainText, DigestType, Signature, Key, Options) when is_binary(Signature) ->
    case format_verify_key(Key) of
	badarg ->
	    erlang:error(badarg, [DigestOrPlainText, DigestType, Signature, Key, Options]);
	{Algorithm, CryptoKey} ->
	    crypto:verify(Algorithm, DigestType, DigestOrPlainText, Signature, CryptoKey, Options)
    end;
verify(_,_,_,_,_) ->
    %% If Signature is a bitstring and not a binary we know already at this
    %% point that the signature is invalid.
    false.

%%--------------------------------------------------------------------
-spec pkix_dist_point(Cert) -> DistPoint when Cert :: der_encoded() | #'OTPCertificate'{},
                                              DistPoint :: #'DistributionPoint'{}.
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
-spec pkix_dist_points(Cert) -> DistPoints when Cert :: der_encoded() | #'OTPCertificate'{},
                                                DistPoints :: [ #'DistributionPoint'{} ].
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
-spec pkix_match_dist_point(CRL, DistPoint) ->
                                   boolean()
                                       when CRL :: der_encoded() | #'CertificateList'{},
                                            DistPoint :: #'DistributionPoint'{}.
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
-spec pkix_sign(Cert, Key) -> Der when Cert :: #'OTPTBSCertificate'{}, 
                                       Key :: private_key(),
                                       Der :: der_encoded() .
%%
%% Description: Sign a pkix x.509 certificate. Returns the corresponding
%% der encoded 'Certificate'{}
%%--------------------------------------------------------------------
pkix_sign(#'OTPTBSCertificate'{signature = 
				   #'SignatureAlgorithm'{} 
			       = SigAlg} = TBSCert, Key) ->
    Msg = pkix_encode('OTPTBSCertificate', TBSCert, otp),
    {DigestType, _, Opts} = pubkey_cert:x509_pkix_sign_types(SigAlg),
    Signature = sign(Msg, DigestType, format_pkix_sign_key(Key), Opts),
    Cert = #'OTPCertificate'{tbsCertificate= TBSCert,
			     signatureAlgorithm = SigAlg,
			     signature = Signature
			    },
    pkix_encode('OTPCertificate', Cert, otp).

%%--------------------------------------------------------------------
-spec pkix_verify(Cert, Key) -> boolean() when Cert :: der_encoded(),
                                               Key :: public_key() .
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

pkix_verify(DerCert,  {#'RSAPublicKey'{} = RSAKey, #'RSASSA-PSS-params'{} = Params}) 
  when is_binary(DerCert) ->
    {DigestType, PlainText, Signature} = pubkey_cert:verify_data(DerCert),
    verify(PlainText, DigestType, Signature, RSAKey, rsa_opts(Params));

pkix_verify(DerCert, Key = {#'ECPoint'{}, _})
  when is_binary(DerCert) ->
    {DigestType, PlainText, Signature} = pubkey_cert:verify_data(DerCert),
    verify(PlainText, DigestType, Signature,  Key).

%%--------------------------------------------------------------------
-spec pkix_crl_verify(CRL, Cert) -> boolean()
                                        when CRL  :: der_encoded() | #'CertificateList'{},
                                             Cert :: der_encoded() | #'OTPCertificate'{} .
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
-spec pkix_is_issuer(Cert, IssuerCert) -> 
                            boolean() when Cert :: der_encoded()
                                                 | #'OTPCertificate'{}
                                                 | #'CertificateList'{},
                                           IssuerCert :: der_encoded()
                                                       | #'OTPCertificate'{} .
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
-spec pkix_is_self_signed(Cert) -> boolean() when Cert::der_encoded()| #'OTPCertificate'{}.
%%
%% Description: Checks if a Certificate is self signed. 
%%--------------------------------------------------------------------
pkix_is_self_signed(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_self_signed(OTPCert);
pkix_is_self_signed(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_self_signed(OtpCert).
  
%%--------------------------------------------------------------------
-spec pkix_is_fixed_dh_cert(Cert) -> boolean() when Cert::der_encoded()| #'OTPCertificate'{}.
%%
%% Description: Checks if a Certificate is a fixed Diffie-Hellman Cert.
%%--------------------------------------------------------------------
pkix_is_fixed_dh_cert(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_fixed_dh_cert(OTPCert);
pkix_is_fixed_dh_cert(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_fixed_dh_cert(OtpCert).

%%--------------------------------------------------------------------
-spec pkix_issuer_id(Cert, IssuedBy) ->
			    {ok, ID::cert_id()} | {error, Reason}
                                when Cert::der_encoded()| #'OTPCertificate'{},
                                     IssuedBy :: self | other,
                                     Reason :: term() .

%% Description: Returns the issuer id.
%%--------------------------------------------------------------------
pkix_issuer_id(#'OTPCertificate'{} = OtpCert, Signed) when (Signed == self) or 
							   (Signed == other) ->
    pubkey_cert:issuer_id(OtpCert, Signed);
pkix_issuer_id(Cert, Signed) when is_binary(Cert) -> 
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_issuer_id(OtpCert, Signed).

%%--------------------------------------------------------------------
-spec pkix_subject_id(Cert) -> ID
              when Cert::der_encoded()| #'OTPCertificate'{},
                   ID::cert_id() .

%% Description: Returns the subject id.
%%--------------------------------------------------------------------
pkix_subject_id(#'OTPCertificate'{} = OtpCert) ->
    pubkey_cert:subject_id(OtpCert);
pkix_subject_id(Cert) when is_binary(Cert) -> 
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_subject_id(OtpCert).

%%--------------------------------------------------------------------
-spec pkix_crl_issuer(CRL| #'CertificateList'{}) -> 
                             Issuer when CRL :: der_encoded(),
                                         Issuer :: issuer_name() .
%
%% Description: Returns the issuer.
%%--------------------------------------------------------------------
pkix_crl_issuer(CRL) when is_binary(CRL) ->
    pkix_crl_issuer(der_decode('CertificateList', CRL));
pkix_crl_issuer(#'CertificateList'{} = CRL) ->
    pubkey_cert_records:transform(
      CRL#'CertificateList'.tbsCertList#'TBSCertList'.issuer, decode).

%%--------------------------------------------------------------------
-spec pkix_normalize_name(Issuer) -> Normalized 
                                         when Issuer :: issuer_name(),
                                              Normalized :: issuer_name() .
%%
%% Description: Normalizes a issuer name so that it can be easily
%%              compared to another issuer name. 
%%--------------------------------------------------------------------
pkix_normalize_name(Issuer) -> 
    pubkey_cert:normalize_general_name(Issuer).

%%-------------------------------------------------------------------- 
-spec pkix_path_validation(Cert::binary()| #'OTPCertificate'{} | atom(),
			   CertChain :: [binary()] ,
			   Options :: [{atom(),term()}]) ->
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
	{fail, UserReason} ->
	    {error, UserReason}
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
-spec pkix_crls_validate(OTPcertificate, DPandCRLs, Options) ->
                                CRLstatus when OTPcertificate :: #'OTPCertificate'{},
                                               DPandCRLs :: [DPandCRL],
                                               DPandCRL  :: {DP, {DerCRL, CRL}},
                                               DP :: #'DistributionPoint'{},
                                               DerCRL :: der_encoded(),
                                               CRL :: #'CertificateList'{},
                                               Options :: [{atom(),term()}],
                                               CRLstatus :: valid
                                                          | {bad_cert, BadCertReason},
                                               BadCertReason :: revocation_status_undetermined
                                                              | {revocation_status_undetermined, Reason::term()}
                                                              | {revoked, crl_reason()}.

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
-type referenceIDs() :: [referenceID()] .
-type referenceID() :: {uri_id | dns_id | ip | srv_id | oid(),  string()} 
                     | {ip, inet:ip_address()} .

-type high_level_alg() :: https .
-type match_fun() ::  fun((ReferenceID::referenceID() | string(),
                           PresentedID::{atom()|oid(),string()}) -> match_fun_result() ) .
-type match_fun_result() :: boolean() | default .

%% Description: Validates a hostname to RFC 6125
%%--------------------------------------------------------------------
-spec pkix_verify_hostname(Cert, ReferenceIDs) -> boolean()
                                                      when Cert :: der_encoded() 
                                                                 | #'OTPCertificate'{},
                                                           ReferenceIDs :: referenceIDs() .
pkix_verify_hostname(Cert, ReferenceIDs) ->
    pkix_verify_hostname(Cert, ReferenceIDs, []).

-spec pkix_verify_hostname(Cert, ReferenceIDs, Options) ->
                                  boolean()
                                      when Cert :: der_encoded() 
                                                 | #'OTPCertificate'{},
                                           ReferenceIDs :: referenceIDs(),
                                           Options :: [{atom(),term()}] .

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
			[{dns_id,X} || X <- verify_hostname_fqnds(ReferenceIDs, FqdnFun)],
		    verify_hostname_match_loop(DNS_ReferenceIDs, PresentedIDs,
					       MatchFun, FailCB, Cert);
		true ->
		    true
	    end
    end.


-spec pkix_verify_hostname_match_fun(high_level_alg()) -> match_fun() .

pkix_verify_hostname_match_fun(https) ->
    fun({dns_id,FQDN=[_|_]}, {dNSName,Name=[_|_]}) -> verify_hostname_match_wildcard(FQDN, Name);
       (_, _) -> default
    end.

%%--------------------------------------------------------------------
-spec ssh_decode(SshBin, Type) ->
                        Decoded
                            when SshBin :: binary(),
                                 Type :: ssh2_pubkey | OtherType | InternalType,
                                 OtherType :: public_key | ssh_file(),
                                 InternalType :: new_openssh,
                                 Decoded :: Decoded_ssh2_pubkey
                                          | Decoded_OtherType,
                                 Decoded_ssh2_pubkey :: public_key(),
                                 Decoded_OtherType :: [{public_key(), Attributes}],
                                 Attributes :: [{atom(),term()}] .
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
			      Type == ssh2_pubkey;
                              Type == new_openssh ->
    pubkey_ssh:decode(SshBin, Type).

%%--------------------------------------------------------------------
-spec ssh_encode(InData, Type) ->
                        binary()
                            when Type :: ssh2_pubkey | OtherType,
                                 OtherType :: public_key | ssh_file(),
                                 InData :: InData_ssh2_pubkey | OtherInData,
                                 InData_ssh2_pubkey :: public_key(),
                                 OtherInData :: [{Key,Attributes}],
                                 Key :: public_key(),
                                 Attributes :: [{atom(),term()}] .
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

ssh_hostkey_fingerprint(Key) ->
    sshfp_string(md5, public_key:ssh_encode(Key,ssh2_pubkey) ).


-spec ssh_hostkey_fingerprint( digest_type(),  public_key()) ->  string()
                           ; ([digest_type()], public_key())   -> [string()]
                           .
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
-spec short_name_hash(Name) -> string() when Name :: issuer_name() .

%% Description: Generates OpenSSL-style hash of a name.
%%--------------------------------------------------------------------
short_name_hash({rdnSequence, _Attributes} = Name) ->
    HashThis = encode_name_for_short_hash(Name),
    <<HashValue:32/little, _/binary>> = crypto:hash(sha, HashThis),
    string:to_lower(string:right(integer_to_list(HashValue, 16), 8, $0)).


%%--------------------------------------------------------------------
-spec pkix_test_data(#{chain_type() := pubkey_cert:chain_opts()} |
                     pubkey_cert:chain_opts()) ->
                            pubkey_cert:test_config() |
                            [pubkey_cert:conf_opt()].

%% Description: Generates cert(s) and ssl configuration
%%--------------------------------------------------------------------

pkix_test_data(#{client_chain := ClientChain0,
                 server_chain := ServerChain0}) ->
    Default = #{intermediates => []},
    ClientChain = maps:merge(Default, ClientChain0),
    ServerChain = maps:merge(Default, ServerChain0),
    pubkey_cert:gen_test_certs(#{client_chain => ClientChain,
                                 server_chain => ServerChain});
pkix_test_data(#{} = Chain) ->
    Default = #{intermediates => []},
    pubkey_cert:gen_test_certs(maps:merge(Default, Chain)).

%%--------------------------------------------------------------------
-spec pkix_test_root_cert(Name, Options) ->
                                 RootCert
                                     when Name :: string(),
                                          Options :: [{atom(),term()}], %[cert_opt()],
                                          RootCert :: pubkey_cert:test_root_cert().
%% Description: Generates a root cert suitable for pkix_test_data/1
%%--------------------------------------------------------------------

pkix_test_root_cert(Name, Opts) ->
    pubkey_cert:root_cert(Name, Opts).

%%--------------------------------------------------------------------
-spec ocsp_status(binary | #'Certificate'{} | #'OTPCertificate'{},
                  list(), list()) ->
    {good, term()} | {unknown, term()} | {revoked, term()} | no_matched_response.
%%
%% Description: Get Certificate status
%%--------------------------------------------------------------------
ocsp_status(Cert, CertPath, Responses) ->
    case pubkey_ocsp:find_single_response(Cert, CertPath, Responses) of
        {ok, #'SingleResponse'{certStatus = CertStatus}} ->
            CertStatus;
        {error, no_matched_response} ->
            no_matched_response
    end.

%%--------------------------------------------------------------------
-spec ocsp_responses(binary(), list(), undefined | binary()) ->
    {ok, [#'SingleResponse'{}]} | {error, Reason::term()}.
%%
%% Description: Get verified OCSP responses
%%--------------------------------------------------------------------
ocsp_responses(OCSPResponseDer, ResponderCerts, Nonce) ->
    catch pubkey_ocsp:verify_ocsp_response(
        OCSPResponseDer, ResponderCerts, Nonce).

%%--------------------------------------------------------------------
-spec ocsp_extensions(undefined | binary()) -> list().
%% Description: Get OCSP stapling extensions for request
%%--------------------------------------------------------------------
ocsp_extensions(Nonce) ->
    [Extn || Extn <- [pubkey_ocsp:get_nonce_extn(Nonce),
                      pubkey_ocsp:get_acceptable_response_types_extn()],
             erlang:is_record(Extn, 'Extension')].

%%--------------------------------------------------------------------
-spec ocsp_responder_id(#'Certificate'{}) -> binary().
%%
%% Description: Get the OCSP responder ID der
%%--------------------------------------------------------------------
ocsp_responder_id(Cert) ->
    pubkey_ocsp:get_ocsp_responder_id(Cert).

%%--------------------------------------------------------------------
-spec ocsp_nonce() -> binary().
%%
%% Description: Get an OCSP nonce
%%--------------------------------------------------------------------
ocsp_nonce() ->
    pubkey_ocsp:get_nonce().

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
default_options([]) ->
    [{rsa_padding, rsa_pkcs1_padding}];
default_options(Opts) ->
    case proplists:get_value(rsa_pad, Opts) of
        undefined ->
            case proplists:get_value(rsa_padding, Opts) of
                undefined ->
                    case lists:dropwhile(fun erlang:is_tuple/1, Opts) of
                        [Pad|_] ->
                            set_padding(Pad, Opts);
                        [] ->
                            set_padding(rsa_pkcs1_padding, Opts)
                    end;
                Pad ->
                    set_padding(Pad, Opts)
            end;
        Pad ->
            set_padding(Pad, Opts)
    end.

set_padding(Pad, Opts) ->
    [{rsa_padding,Pad} | [{T,V} || {T,V} <- Opts,
                                   T =/= rsa_padding,
                                   T =/= rsa_pad]
    ].

format_pkix_sign_key({#'RSAPrivateKey'{} = Key, _}) ->
    %% Params are handled in option arg
    Key;
format_pkix_sign_key(Key) ->
    Key.
format_sign_key(Key = #'RSAPrivateKey'{}) ->
    {rsa, format_rsa_private_key(Key)};
format_sign_key(#'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) ->
    {dss, [P, Q, G, X]};
format_sign_key(#'ECPrivateKey'{privateKey = PrivKey, parameters = Param}) ->
    {ecdsa, [PrivKey, ec_curve_spec(Param)]};
format_sign_key({ed_pri, Curve, _Pub, Priv}) ->
    {eddsa, [Priv,Curve]};
format_sign_key(_) ->
    badarg.

format_verify_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Exp}) ->
    {rsa, [Exp, Mod]};
format_verify_key({#'ECPoint'{point = Point}, Param}) ->
    {ecdsa, [Point, ec_curve_spec(Param)]};
format_verify_key({Key,  #'Dss-Parms'{p = P, q = Q, g = G}}) ->
    {dss, [P, Q, G, Key]};
format_verify_key({ed_pub, Curve, Key}) ->
    {eddsa, [Key,Curve]};
%% Convert private keys to public keys
format_verify_key(#'RSAPrivateKey'{modulus = Mod, publicExponent = Exp}) ->
    format_verify_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Exp});
format_verify_key(#'ECPrivateKey'{parameters = Param, publicKey = {_, Point}}) ->
    format_verify_key({#'ECPoint'{point = Point}, Param});
format_verify_key(#'ECPrivateKey'{parameters = Param, publicKey = Point}) ->
    format_verify_key({#'ECPoint'{point = Point}, Param});
format_verify_key(#'DSAPrivateKey'{y=Y, p=P, q=Q, g=G}) ->
    format_verify_key({Y, #'Dss-Parms'{p=P, q=Q, g=G}});
format_verify_key(_) ->
    badarg.

rsa_opts(#'RSASSA-PSS-params'{maskGenAlgorithm = 
                                  #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                      parameters = #'HashAlgorithm'{algorithm = HashAlgoOid}
                                                     }}) ->
    HashAlgo = pkix_hash_type(HashAlgoOid),
    [{rsa_padding, rsa_pkcs1_pss_padding},
     {rsa_pss_saltlen, -1},
     {rsa_mgf1_md, HashAlgo}].

do_pem_entry_encode(Asn1Type, Entity, CipherInfo, Password) ->
    Der = der_encode(Asn1Type, Entity),
    DecryptDer = pubkey_pem:cipher(Der, CipherInfo, Password),
    {Asn1Type, DecryptDer, CipherInfo}.
  
do_pem_entry_decode({Asn1Type,_, _} = PemEntry, Password) ->
    Der = pubkey_pem:decipher(PemEntry, Password),
    der_decode(Asn1Type, Der).

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
otp_cert(#'OTPCertificate'{} = Cert) ->
    Cert.

der_cert(#'OTPCertificate'{} = Cert) ->
    pkix_encode('OTPCertificate', Cert, otp);
der_cert(Der) when is_binary(Der) ->
    Der.

pkix_crls_validate(_, [],_, Options, #revoke_state{details = Details}) ->
     case proplists:get_value(undetermined_details, Options, false) of
         false ->
             {bad_cert, revocation_status_undetermined};
         true ->
             {bad_cert, {revocation_status_undetermined, {bad_crls, format_details(Details)}}}
     end;
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
	{undetermined, unrevoked, #revoke_state{details = Details}} when Rest == []->
            case proplists:get_value(undetermined_details, Options, false) of
                false ->
                    {bad_cert, revocation_status_undetermined};
                true ->
                    {bad_cert, {revocation_status_undetermined, {bad_crls, Details}}}
            end;
	{undetermined, unrevoked, RevokedState} when Rest =/= []->
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

-spec ec_generate_key(ecpk_parameters_api()) -> #'ECPrivateKey'{}.
ec_generate_key(Params) ->
    Curve = ec_curve_spec(Params),
    Term = crypto:generate_key(ecdh, Curve),
    NormParams = ec_normalize_params(Params),
    ec_key(Term, NormParams).

-spec ec_normalize_params(ecpk_parameters_api()) -> ecpk_parameters().
ec_normalize_params({namedCurve, Name}) when is_atom(Name) ->
	{namedCurve, pubkey_cert_records:namedCurves(Name)};
ec_normalize_params(#'ECParameters'{} = ECParams) ->
	{ecParameters, ECParams};
ec_normalize_params(Other) -> Other.

-spec ec_curve_spec(ecpk_parameters_api()) -> term().
ec_curve_spec( #'ECParameters'{fieldID = #'FieldID'{fieldType = Type,
                                                    parameters = Params}, curve = PCurve, base = Base, order = Order, cofactor = CoFactor }) ->
    Field = format_field(pubkey_cert_records:supportedCurvesTypes(Type), Params),
    Curve = {PCurve#'Curve'.a, PCurve#'Curve'.b, none},
    {Field, Curve, Base, Order, CoFactor};
ec_curve_spec({ecParameters, ECParams}) ->
	ec_curve_spec(ECParams);
ec_curve_spec({namedCurve, OID}) when is_tuple(OID), is_integer(element(1,OID)) ->
    ec_curve_spec({namedCurve,  pubkey_cert_records:namedCurves(OID)});
ec_curve_spec({namedCurve, x25519 = Name}) ->
    Name;
ec_curve_spec({namedCurve, x448 = Name}) ->
    Name;
ec_curve_spec({namedCurve, Name}) when is_atom(Name) ->
    crypto:ec_curve(Name).

format_field(characteristic_two_field = Type, Params0) ->
    #'Characteristic-two'{
       m = M,
       basis = BasisOid,
       parameters = Params} = der_decode('Characteristic-two', Params0),
    {Type, M, field_param_decode(BasisOid, Params)};
format_field(prime_field, Params0) ->
    Prime = der_decode('Prime-p', Params0),
    {prime_field, Prime}.

field_param_decode(?'ppBasis', Params) ->
    #'Pentanomial'{k1 = K1, k2 = K2, k3 = K3} =
        der_decode('Pentanomial', Params),
    {ppbasis, K1, K2, K3};
field_param_decode(?'tpBasis', Params) ->
    K = der_decode('Trinomial', Params),
    {tpbasis, K};
field_param_decode(?'gnBasis', _) ->
    onbasis.
        
-spec ec_key({PubKey::term(), PrivateKey::term()}, Params::ecpk_parameters()) -> #'ECPrivateKey'{}.
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
    #{scheme := "https", host := Host} = uri_string:normalize(URI, [return_map]),
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
    verify_hostname_match_wildcard(FQDN, Name);
verify_hostname_match_default0({dns_id,R}, {dNSName,P}) ->
    R==P;
verify_hostname_match_default0({uri_id,R}, {uniformResourceIdentifier,P}) ->
    R==P;
verify_hostname_match_default0({ip,R}, {iPAddress,P}) when length(P) == 4 ->
    %% IPv4
    try
        list_to_tuple(P)
            == if is_tuple(R), size(R)==4 -> R;
                  is_list(R) -> ok(inet:parse_ipv4strict_address(R))
               end
    catch
        _:_ ->
            false
    end;

verify_hostname_match_default0({ip,R}, {iPAddress,P}) when length(P) == 16 ->
    %% IPv6. The length 16 is due to the certificate specification.
    try
        l16_to_tup(P)
            == if is_tuple(R), size(R)==8 -> R;
                  is_list(R) -> ok(inet:parse_ipv6strict_address(R))
               end
    catch
        _:_ ->
            false
    end;
verify_hostname_match_default0({srv_id,R}, {srvName,P}) ->
    R==P;
verify_hostname_match_default0({srv_id,R}, {?srvName_OID,P}) ->
    R==P;
verify_hostname_match_default0(_, _) ->
    false.


verify_hostname_match_wildcard(FQDN, Name) ->
    [F1|Fs] = string:tokens(FQDN, "."),
    [N1|Ns] = string:tokens(Name, "."),
    match_wild(F1,N1) andalso Fs==Ns.


ok({ok,X}) -> X.

l16_to_tup(L) -> list_to_tuple(l16_to_tup(L, [])).
%%
l16_to_tup([A,B|T], Acc) -> l16_to_tup(T, [(A bsl 8) bor B | Acc]);
l16_to_tup([], Acc) -> lists:reverse(Acc).
    
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


to_lower_ascii({ip,_}=X) -> X;
to_lower_ascii({iPAddress,_}=X) -> X;
to_lower_ascii(S) when is_list(S) -> lists:map(fun to_lower_ascii/1, S);
to_lower_ascii({T,S}) -> {T, to_lower_ascii(S)};
to_lower_ascii(C) when $A =< C,C =< $Z -> C + ($a-$A);
to_lower_ascii(C) -> C.

to_string(S) when is_list(S) -> S;
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(X) -> X.

format_details([]) ->
    no_relevant_crls;
format_details(Details) ->
    Details.
  
