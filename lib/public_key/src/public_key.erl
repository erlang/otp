%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
-moduledoc """
API module for public-key infrastructure.

Provides functions to handle public-key infrastructure, for details see
[public_key application](public_key_app.md).

> #### Note {: .info }
>
> All records used in this Reference Manual are generated from ASN.1
> specifications and are documented in the User's Guide. See
> [Public-key Records](public_key_records.md).

Use the following include directive to get access to the records and constant
macros described here and in the User's Guide:

```text
 -include_lib("public_key/include/public_key.hrl").
```
""".


-feature(maybe_expr,enable).
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
	 pkix_crls_validate/3,
	 pkix_dist_point/1,
	 pkix_dist_points/1,
	 pkix_match_dist_point/2,
	 pkix_crl_verify/2,
	 pkix_crl_issuer/1,
	 short_name_hash/1,
         pkix_test_data/1,
         pkix_test_root_cert/2,
         pkix_ocsp_validate/5,
         ocsp_extensions/1,
         cacerts_get/0,
         cacerts_load/0,
         cacerts_load/1,
         cacerts_clear/0
	]).
%% Tracing
-export([handle_trace/3]).

%%----------------
%% Moved to ssh
-removed([{ssh_decode,2, "use ssh_file:decode/2 instead"},
          {ssh_encode,2, "use ssh_file:encode/2 instead"},
          {ssh_hostkey_fingerprint,1, "use ssh:hostkey_fingerprint/1 instead"},
          {ssh_hostkey_fingerprint,2, "use ssh:hostkey_fingerprint/2 instead"}
         ]).
-export([ssh_curvename2oid/1, oid2ssh_curvename/1]).
%% When removing for OTP-25.0, remember to also remove
%%   - most of pubkey_ssh.erl except
%%       + dh_gex_group/4
%%       + dh_gex_group_sizes/0
%%   - pubkey_pem:pem_start({no_asn1, new_openssh})
%%   - pubkey_pem:pem_end(<<"-----BEGIN OPENSSH PRIVATE KEY-----">>)
%%   - pubkey_pem:asn1_type(<<"-----BEGIN OPENSSH PRIVATE KEY-----">>) -> {no_asn1, new_openssh}

%%----------------------------------------------------------------
%% Types
-export_type([asn1_type/0,
              bad_cert_reason/0,
              cert/0,
              cert_id/0,
              cert_opt/0,
              chain_opts/0,
              combined_cert/0,
              conf_opt/0,
              der_encoded/0,
              digest_type/0,
              issuer_name/0,
              key_params/0,
              oid/0,
              pem_entry/0,
              pki_asn1_type/0,
              policy_node/0,
              private_key/0,
              public_key/0,
              rsa_public_key/0,
              rsa_private_key/0,
              rsa_pss_public_key/0,
              rsa_pss_private_key/0,
              dsa_public_key/0,
              dsa_private_key/0,
              ecdsa_public_key/0,
              ecdsa_private_key/0,
              eddsa_public_key/0,
              eddsa_private_key/0,
              custom_key_opts/0,
              public_key_info/0,
              %% Internal exports beneath do not document
              test_config/0,
              test_root_cert/0
             ]).

%% Needed for legacy TLS-1.0 and TLS-1.1 functionality
-compile({nowarn_deprecated_function, [{crypto, private_encrypt, 4},
                                       {crypto, private_decrypt, 4},
                                       {crypto, public_encrypt, 4},
                                       {crypto, public_decrypt, 4}
                                      ]}).

-doc(#{group => <<"Keys">>}).
-doc "Supported public keys".
-type public_key()           ::  rsa_public_key() |
                                 rsa_pss_public_key() |
                                 dsa_public_key() |
                                 ecdsa_public_key() |
                                 eddsa_public_key() .
-doc(#{group => <<"Keys">>}).
-doc "Supported private keys".
-type private_key()          ::  rsa_private_key() |
                                 rsa_pss_private_key() |
                                 dsa_private_key() |
                                 ecdsa_private_key() |
                                 eddsa_private_key() |
                                 #{algorithm := eddsa | rsa_pss_pss | ecdsa | rsa | dsa,
                                   sign_fun => fun()} .
-doc(#{group => <<"Keys">>}).
-doc """
Can be provided together with a custom private key, that specifies a key fun, to
provide additional options understood by the fun.
""".
-type custom_key_opts()      :: [term()].

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined public key format for plain RSA algorithm.".
-type rsa_public_key()       ::  #'RSAPublicKey'{}.

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined private key format plain RSA algorithm or customization fun.".
-type rsa_private_key()      ::  #'RSAPrivateKey'{} | #{algorithm := rsa,
                                                        encrypt_fun => fun()}.
-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined public key format for the RSSASSA-PSS algorithm.".
-type rsa_pss_public_key()   ::  {rsa_public_key(), #'RSASSA-PSS-params'{}}.

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined private key format the RSSASSA-PSS algorithm or customization fun.".
-type rsa_pss_private_key()  ::  { #'RSAPrivateKey'{}, #'RSASSA-PSS-params'{}}.

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined private key format for the DSA algorithm.".
-type dsa_private_key()      ::  #'DSAPrivateKey'{}.

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined public key format for the DSA algorithm.".
-type dsa_public_key()       :: {dss_public_key(), #'Dss-Parms'{}}.

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined public key format for the DSS algorithm (part of DSA key).".
-type dss_public_key()       :: pos_integer().

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined public key format for the ECDSA algorithm.".
-type ecdsa_public_key()        :: {#'ECPoint'{},{namedCurve, oid()} | #'ECParameters'{}}.

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined private key format for the ECDSA algorithm.".
-type ecdsa_private_key()       :: #'ECPrivateKey'{}.

-doc(#{group => <<"Keys">>}).
-doc """
ASN.1 defined public key format for the EDDSA algorithm, possible oids: ?'id-Ed25519' | ?'id-Ed448'
""".
-type eddsa_public_key()        :: {#'ECPoint'{}, {namedCurve, oid()}}.

-doc(#{group => <<"Keys">>}).
-doc """
ASN.1 defined private key format for the EDDSA algorithm, possible oids: ?'id-Ed25519' | ?'id-Ed448'
""".
-type eddsa_private_key()       :: #'ECPrivateKey'{parameters :: {namedCurve, oid()}}.

-doc(#{group => <<"Keys">>}).
-doc "ASN.1 defined parameters for public key algorithms.".
-type key_params()    :: 'NULL' | #'RSASSA-PSS-params'{} |  {namedCurve, oid()} | #'ECParameters'{} | #'Dss-Parms'{}.

-doc(#{group => <<"Common">>}).
-doc """
ASN.1 DER encoded entity.
""".
-type der_encoded()          :: binary().

-doc(#{group => <<"PEM files">>}).
-doc """
ASN.1 type that can be found in PEM files that can be decode by the public_key application.
""".
-type pki_asn1_type()        ::  'Certificate' | 'RSAPrivateKey' | 'RSAPublicKey'
			       | 'SubjectPublicKeyInfo' | 'DSAPrivateKey'
                               | 'DHParameter' | 'PrivateKeyInfo' |
				 'CertificationRequest' | 'ContentInfo' | 'CertificateList' |
				 'ECPrivateKey' | 'OneAsymmetricKey'| 'EcpkParameters'.

-doc(#{group => <<"PEM files">>}).
-doc """
Possible `Ciphers` are "RC2-CBC" | "DES-CBC" | "DES-EDE3-CBC" `Salt` could be generated with
[`crypto:strong_rand_bytes(8)`](`crypto:strong_rand_bytes/1`).
""".
-type pem_entry()            :: {pki_asn1_type(),
				 DerOrDerEncrypted::binary(),
				 not_encrypted | {Cipher::iodata(), Salt::binary()
                                | {#'PBEParameter'{}, digest_type()}
                                | #'PBES2-params'{}}
				}.

-doc(#{group => <<"Common">>}).
-doc "ASN.1 type present in the Public Key applications ASN.1 specifications.".
-type asn1_type()            :: atom(). %% see "OTP-PUB-KEY.hrl

-doc(#{group => <<"Common">>}).
-doc "Hash function used to create a message digest".
-type digest_type()          ::  crypto:sha2() | crypto:sha1() | md5 | none.

-doc(#{group => <<"Certificate Revocation">>}).
-doc """
The reason that a certifcate has been revoked as define by RFC 5280.
""".
-type crl_reason()           ::  unspecified | keyCompromise | cACompromise | affiliationChanged | superseded
			       | cessationOfOperation | certificateHold | privilegeWithdrawn |  aACompromise.

-doc(#{group => <<"Common">>}).
-doc "Object identifier, a tuple of integers as generated by the `ASN.1` compiler.".
-type oid()                  :: tuple().

-doc(#{group => <<"Certificates">>}).
-doc """
A certificate is identified by its serial-number and Issuer Name.
""".
-type cert_id()              :: {SerialNr::integer(), issuer_name()} .

-doc(#{group => <<"Certificates">>}).
-doc """
The value of the issuer part of a certificate.
""".
-type issuer_name()          :: {rdnSequence,[[#'AttributeTypeAndValue'{}]]} .

-doc(#{group => <<"Certificates">>}).
-doc """
The reason that a certifcate gets rejected by the certificate path validation.
""".
-type bad_cert_reason()      :: cert_expired | invalid_issuer | invalid_signature | name_not_permitted |
                                missing_basic_constraint | invalid_key_usage | duplicate_cert_in_path |
                                {key_usage_mismatch, term()} |
                                {'policy_requirement_not_met', term()} | {'invalid_policy_mapping', term()} |
                                {revoked, crl_reason()} | invalid_validity_dates |
                                {revocation_status_undetermined, term()} | atom().

-doc(#{group => <<"Certificates">>}).
-doc """
A record that can be used to provide the certificate on both the DER encoded and the OTP decode format.

Such a construct can be useful to avoid conversions and problems that can arise due to relaxed decoding rules.
""".
-type combined_cert()        :: #cert{}.

-doc(#{group => <<"Certificates">>}).
-doc "An encoded or decode certificate.".
-type cert()                 :: der_encoded() | #'OTPCertificate'{}.

-doc(#{group => <<"Certificates">>}).
-doc "Certificate policy information.".
-type policy_node() ::
        #{valid_policy := oid(),
          qualifier_set := [#'UserNotice'{}| {uri, string()}],
          expected_policy_set := [oid()]}.

-doc(#{group => <<"Certificates">>}).
-doc """
Information a certificates public key.

Possible oids: ?'rsaEncryption' | ?'id-RSASSA-PSS' | ?'id-ecPublicKey' | ?'id-Ed25519' | ?'id-Ed448' | ?'id-dsa'
""".
-type public_key_info()      :: {oid(),  rsa_public_key() | #'ECPoint'{} | dss_public_key(),  key_params()}.

-doc(#{group => <<"Test Data">>}).
-doc """
Options to customize generated test certificates
""".
-type cert_opt()  :: {digest, digest_type()} |
                     {key,  {namedCurve, oid()} | #'ECParameters'{} | {rsa, Size::pos_integer(), Prime::pos_integer()}  | private_key()} |
                     {validity, {From::erlang:timestamp(), To::erlang:timestamp()}} |
                     {extensions, [#'Extension'{}]}.

-doc(#{group => <<"Test Data">>}).
-doc """
Certificate customize options for diffrent parts of the certificate test chain.
""".
-type chain_opts()  :: #{root :=  [cert_opt()],  intermediates =>  [[cert_opt()]],
                         peer :=  [cert_opt()]
                        }.

-doc(#{group => <<"Test Data">>}).
-doc """
Configuration options for the generated certificate test chain.
""".
-type conf_opt()    :: {cert, der_encoded()} |
                       {key,  private_key()} |
                       {cacerts, [der_encoded()]}.

-doc false.
-type test_config() ::
        #{server_config := [conf_opt()],  client_config :=  [conf_opt()]}.

-doc false.
-type test_root_cert() ::
        #{cert := der_encoded(), key := private_key()}.

-define(UINT32(X), X:32/unsigned-big-integer).
-define(DER_NULL, <<5, 0>>).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------

-doc(#{group => <<"PEM API">>}).
-doc """
Decodes PEM binary data and returns entries as ASN.1 DER encoded entities.

Example
`{ok, PemBin} = file:read_file("cert.pem"). PemEntries = public_key:pem_decode(PemBin).`
""".
-doc(#{since => <<"OTP R14B">>}).
-spec pem_decode(binary()) -> [pem_entry()].
%%
%%--------------------------------------------------------------------
pem_decode(PemBin) when is_binary(PemBin) ->
    pubkey_pem:decode(PemBin).


%%--------------------------------------------------------------------
-doc(#{group => <<"PEM API">>,
      since => <<"OTP R14B">>}).
-doc "Creates a PEM binary.".
-spec pem_encode([pem_entry()]) -> binary().
%%--------------------------------------------------------------------
pem_encode(PemEntries) when is_list(PemEntries) ->
    iolist_to_binary(pubkey_pem:encode(PemEntries)).

%%--------------------------------------------------------------------
-doc(#{group => <<"PEM API">>,
       equiv => pem_entry_decode(PemEntry, ""),
       since => <<"OTP R14B">>}).
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
            ECCParams = ec_decode_params(AlgId, Params),
            {#'ECPoint'{point = Key0}, ECCParams}
    end;
pem_entry_decode({Asn1Type, Der, not_encrypted}) when is_atom(Asn1Type),
						      is_binary(Der) ->
    der_decode(Asn1Type, Der).

-doc(#{group => <<"PEM API">>,
       since => <<"OTP R14B">>}).
-doc """
Decodes a PEM entry. [`pem_decode/1`](`pem_decode/1`) returns a list of PEM
entries. Notice that if the PEM entry is of type 'SubjectPublickeyInfo', it is
further decoded to an `t:rsa_public_key/0` or `t:dsa_public_key/0`.

Password can be either an octet string or function which returns same type.
""".
-spec pem_entry_decode(PemEntry, Password) -> term() when
      PemEntry :: pem_entry(),
      Password :: iodata() | fun(() -> iodata()).
pem_entry_decode(PemEntry, PasswordFun) when is_function(PasswordFun) ->
     pem_entry_decode(PemEntry, PasswordFun());
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
-doc(#{group => <<"PEM API">>,
       since => <<"OTP R14B">>,
       equiv => pem_entry_encode/3}).
-spec pem_entry_encode(Asn1Type, Entity) -> pem_entry() when Asn1Type :: pki_asn1_type(),
                                                             Entity :: term() .

pem_entry_encode('SubjectPublicKeyInfo', Entity=#'RSAPublicKey'{}) ->
    KeyDer = der_encode('RSAPublicKey', Entity),
    Spki = subject_public_key_info(#'AlgorithmIdentifier'{algorithm = ?'rsaEncryption',
                                                          parameters =?DER_NULL}, KeyDer),
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode('SubjectPublicKeyInfo',
                 {DsaInt, Params=#'Dss-Parms'{}}) when is_integer(DsaInt) ->
    KeyDer = der_encode('DSAPublicKey', DsaInt),
    ParamDer = der_encode('DSAParams', {params, Params}),
    Spki = subject_public_key_info(#'AlgorithmIdentifier'{algorithm =?'id-dsa',
                                                          parameters = ParamDer},
                                   KeyDer),
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode('SubjectPublicKeyInfo',
		 {#'ECPoint'{point = Key}, {namedCurve, ?'id-Ed25519' = ID}}) when is_binary(Key)->
    Spki = subject_public_key_info(#'AlgorithmIdentifier'{algorithm = ID}, Key),
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode('SubjectPublicKeyInfo',
		 {#'ECPoint'{point = Key}, {namedCurve, ?'id-Ed448' = ID}}) when is_binary(Key)->
    Spki = subject_public_key_info(#'AlgorithmIdentifier'{algorithm = ID}, Key),
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode('SubjectPublicKeyInfo',
		 {#'ECPoint'{point = Key}, ECParam}) when is_binary(Key)->
    Params = der_encode('EcpkParameters',ECParam),
    Spki = subject_public_key_info(#'AlgorithmIdentifier'{algorithm =?'id-ecPublicKey',
                                                          parameters = Params},
                                   Key),
    pem_entry_encode('SubjectPublicKeyInfo', Spki);
pem_entry_encode(Asn1Type, Entity)  when is_atom(Asn1Type) ->
    Der = der_encode(Asn1Type, Entity),
    {Asn1Type, Der, not_encrypted}.

-doc(#{group => <<"PEM API">>,
       since => <<"OTP R14B">>}).
-doc """
Creates a PEM entry that can be feed to [`pem_encode/1`](`pem_encode/1`).

If `Asn1Type` is `'SubjectPublicKeyInfo'`, `Entity` must be either an
`t:rsa_public_key/0`, `t:dsa_public_key/0` or an `t:ecdsa_public_key/0` and this
function creates the appropriate `'SubjectPublicKeyInfo'` entry.
""".
-spec pem_entry_encode(Asn1Type, Entity, InfoPwd) ->
                              pem_entry() when Asn1Type :: pki_asn1_type(),
                                               Entity :: term(),
                                               InfoPwd :: {CipherInfo,Password},
                                               CipherInfo :: {Cipher::iodata(), Salt::binary()
                                                                                     | {#'PBEParameter'{}, digest_type()}
                                                                                     | #'PBES2-params'{}},
                                               Password :: iodata() .
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
-doc(#{group => <<"ASN.1 Encoding API">>,
       since => <<"OTP R14B">>}).
-doc "Decodes a public-key ASN.1 DER encoded entity.".

-spec der_decode(Asn1Type, Der) -> Entity when Asn1Type :: asn1_type(),
                                               Der :: der_encoded(),
                                               Entity :: term().
%%
%% Description: Decodes a public key asn1 der encoded entity.
%%--------------------------------------------------------------------
der_decode(Asn1Type, Der) when (((Asn1Type == 'PrivateKeyInfo')
                                 orelse
                                   (Asn1Type == 'EncryptedPrivateKeyInfo'))
                                andalso is_binary(Der)) ->
    try
	{ok, Decoded} = 'PKCS-FRAME':decode(Asn1Type, Der),
	der_priv_key_decode(Decoded)
    catch
	error:{badmatch, {error, _}} = Error ->
            handle_pkcs_frame_error(Asn1Type, Der, Error)
    end;

der_decode(Asn1Type, Der) when is_atom(Asn1Type), is_binary(Der) ->
    try 
	{ok, Decoded} = 'OTP-PUB-KEY':decode(Asn1Type, Der),
	Decoded
    catch	    
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end.

handle_pkcs_frame_error('PrivateKeyInfo', Der, _) ->
    try
	{ok, Decoded} = 'PKCS-FRAME':decode('OneAsymmetricKey', Der),
	der_priv_key_decode(Decoded)
    catch
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end;
handle_pkcs_frame_error(_, _, Error) ->
    erlang:error(Error).

der_priv_key_decode(#'PrivateKeyInfo'{version = v1,
                                      privateKeyAlgorithm =
                                          #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-ecPublicKey',
                                                                                parameters = {asn1_OPENTYPE, Parameters}},
                                      privateKey = PrivKey}) ->
    EcPrivKey = der_decode('ECPrivateKey', PrivKey),
    EcPrivKey#'ECPrivateKey'{parameters = der_decode('EcpkParameters', Parameters)};
der_priv_key_decode(#'PrivateKeyInfo'{version = v1,
                                      privateKeyAlgorithm =#'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = CurveOId},
                                      privateKey = CurvePrivKey}) when
      CurveOId == ?'id-Ed25519'orelse
      CurveOId == ?'id-Ed448' ->
    PrivKey = der_decode('CurvePrivateKey', CurvePrivKey),
    #'ECPrivateKey'{version = 1, parameters = {namedCurve, CurveOId}, privateKey = PrivKey};
der_priv_key_decode(#'OneAsymmetricKey'{
                       privateKeyAlgorithm = #'OneAsymmetricKey_privateKeyAlgorithm'{algorithm = CurveOId},
                       privateKey = CurvePrivKey,
                       attributes = Attr,
                       publicKey = PubKey}) when
      CurveOId == ?'id-Ed25519'orelse
      CurveOId == ?'id-Ed448' ->
    PrivKey = der_decode('CurvePrivateKey', CurvePrivKey),
    #'ECPrivateKey'{version = 2, parameters = {namedCurve, CurveOId}, privateKey = PrivKey,
                    attributes = Attr,
                    publicKey = PubKey};
der_priv_key_decode(#'PrivateKeyInfo'{version = v1,
                                      privateKeyAlgorithm =
                                          #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'rsaEncryption'},
                                      privateKey = PrivKey}) ->
    der_decode('RSAPrivateKey', PrivKey);
der_priv_key_decode(#'PrivateKeyInfo'{version = v1,
                                      privateKeyAlgorithm =
                                          #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                                                                parameters = {asn1_OPENTYPE, Parameters}},
                                      privateKey = PrivKey}) ->
    Key = der_decode('RSAPrivateKey', PrivKey),
    Params = der_decode('RSASSA-PSS-params', Parameters),
    {Key, Params};
der_priv_key_decode(#'PrivateKeyInfo'{version = v1,
                                      privateKeyAlgorithm =
                                          #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                                                                parameters = asn1_NOVALUE},
                                      privateKey = PrivKey}) ->
    Key = der_decode('RSAPrivateKey', PrivKey),
    #'RSASSA-AlgorithmIdentifier'{parameters = Params} = ?'rSASSA-PSS-Default-Identifier',
    {Key, Params};
der_priv_key_decode(#'PrivateKeyInfo'{version = v1,
                                      privateKeyAlgorithm =
                                          #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-dsa',
                                                                                parameters =
                                                                                    {asn1_OPENTYPE, Parameters}},
                                      privateKey = PrivKey}) ->
    {params, #'Dss-Parms'{p=P, q=Q, g=G}} = der_decode('DSAParams', Parameters),
    X = der_decode('Prime-p', PrivKey),
    #'DSAPrivateKey'{p=P, q=Q, g=G, x=X};
der_priv_key_decode(PKCS8Key) ->
    PKCS8Key.

%%--------------------------------------------------------------------
-doc(#{group => <<"ASN.1 Encoding API">>,
        since => <<"OTP R14B">>}).
-doc "Encodes a public-key entity with ASN.1 DER encoding.".
-spec der_encode(Asn1Type, Entity) -> Der when Asn1Type :: asn1_type(),
                                               Entity :: term(),
                                               Der :: binary() .
%%--------------------------------------------------------------------
der_encode('PrivateKeyInfo', #'DSAPrivateKey'{p=P, q=Q, g=G, x=X}) ->
    Params = der_encode('Dss-Parms', #'Dss-Parms'{p=P, q=Q, g=G}),
    Alg =  #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-dsa',
                                                 parameters =
                                                     {asn1_OPENTYPE, Params}},
    Key = der_encode('Prime-p', X),
    der_encode('PrivateKeyInfo',
               #'PrivateKeyInfo'{version = v1,
                                 privateKeyAlgorithm = Alg,
                                 privateKey = Key});
der_encode('PrivateKeyInfo', #'RSAPrivateKey'{} = PrivKey) ->
    Parms = ?DER_NULL,
    Alg = #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'rsaEncryption',
                                                parameters = {asn1_OPENTYPE, Parms}},
    Key = der_encode('RSAPrivateKey', PrivKey),
    der_encode('PrivateKeyInfo',
               #'PrivateKeyInfo'{version = v1,
                                 privateKeyAlgorithm = Alg,
                                 privateKey = Key});
der_encode('PrivateKeyInfo', {#'RSAPrivateKey'{} = PrivKey, Parameters}) ->
    #'RSASSA-AlgorithmIdentifier'{parameters = DefaultParams} = ?'rSASSA-PSS-Default-Identifier',
    Params = case Parameters of
                 DefaultParams ->
                     asn1_NOVALUE;
                 _ ->
                     {asn1_OPENTYPE, der_encode('RSASSA-PSS-params', Parameters)}
             end,
    Alg = #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                                parameters = Params},
    Key = der_encode('RSAPrivateKey', PrivKey),
    der_encode('PrivateKeyInfo', #'PrivateKeyInfo'{version = v1,
                                                   privateKeyAlgorithm = Alg,
                                                   privateKey = Key});
der_encode('PrivateKeyInfo', #'ECPrivateKey'{parameters = {namedCurve, CurveOId},
                                             privateKey = Key}) when
      CurveOId == ?'id-Ed25519' orelse
      CurveOId == ?'id-Ed448' ->
    CurvePrivKey = der_encode('CurvePrivateKey', Key),
    Alg = #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = CurveOId},
    der_encode('PrivateKeyInfo', #'PrivateKeyInfo'{version = v1,
                                                   privateKeyAlgorithm = Alg,
                                                   privateKey = CurvePrivKey});
der_encode('PrivateKeyInfo', #'ECPrivateKey'{parameters = Parameters} = PrivKey) ->
    Params = der_encode('EcpkParameters', Parameters),
    Alg = #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-ecPublicKey',
                                                parameters = {asn1_OPENTYPE, Params}},
    Key = der_encode('ECPrivateKey', PrivKey#'ECPrivateKey'{parameters = asn1_NOVALUE}),
    der_encode('PrivateKeyInfo',
               #'PrivateKeyInfo'{version = v1,
                                 privateKeyAlgorithm = Alg,
                                 privateKey = Key});
der_encode('OneAsymmetricKey', #'ECPrivateKey'{parameters = {namedCurve, CurveOId},
                                               privateKey = Key,
                                               attributes = Attr,
                                               publicKey = PubKey}) when
      CurveOId == ?'id-Ed25519' orelse
      CurveOId == ?'id-Ed448' ->
    CurvePrivKey = der_encode('CurvePrivateKey', Key),
    Alg = #'OneAsymmetricKey_privateKeyAlgorithm'{algorithm = CurveOId},
    der_encode('OneAsymmetricKey',
               #'OneAsymmetricKey'{version = 1,
                                   privateKeyAlgorithm = Alg,
                                   privateKey = CurvePrivKey,
                                   attributes = Attr,
                                   publicKey = PubKey});
der_encode('OneAsymmetricKey', #'ECPrivateKey'{parameters = {namedCurve, CurveOId},
                                               privateKey = Key,
                                               attributes = Attr,
                                               publicKey = PubKey}) ->
    Alg = #'OneAsymmetricKey_privateKeyAlgorithm'{algorithm = CurveOId},
    der_encode('OneAsymmetricKey',
               #'OneAsymmetricKey'{version = 1,
                                   privateKeyAlgorithm = Alg,
                                   privateKey = Key,
                                   attributes = Attr,
                                   publicKey = PubKey});
der_encode(Asn1Type, Entity) when (Asn1Type == 'PrivateKeyInfo') orelse
                                  (Asn1Type == 'OneAsymmetricKey') orelse
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
-doc(#{group => <<"Certificate API">>}).
-doc """
Decodes an ASN.1 DER-encoded PKIX certificate.

Option `otp` uses the customized ASN.1 specification OTP-PKIX.asn1 for
decoding and also recursively decode most of the standard parts.
""".

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
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R14B">>}).
-doc """
DER encodes a PKIX x509 certificate or part of such a certificate.

This function must be used for encoding certificates or parts of
certificates that are decoded/created in the `otp` format, whereas for
the plain format this function directly calls
[`der_encode/2`](`der_encode/2`).

> #### Note {: .info }
>
> Subtle ASN-1 encoding errors in certificates may be worked around when
> decoding, this may have the affect that the encoding a certificate back to DER
> may generate different bytes then the supplied original.
""".
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
-doc(#{equiv => decrypt_private(CipherText, Key, []),
       group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R14B">>}).
-spec decrypt_private(CipherText, Key) ->
                             PlainText when CipherText :: binary(),
                                            Key :: rsa_private_key(),
                                            PlainText ::  binary() .
decrypt_private(CipherText, Key) ->
    decrypt_private(CipherText, Key, []).

-doc(#{group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R14B">>}).
-doc """
Public-key decryption using the private key. See also `crypto:private_decrypt/4`

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.
""".
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
-doc(#{equiv => decrypt_public(CipherText, Key, []),
       group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R14B">>}).
-spec decrypt_public(CipherText, Key) ->
			    PlainText
                                when CipherText :: binary(),
                                     Key :: rsa_public_key(),
                                     PlainText :: binary() .
decrypt_public(CipherText, Key) ->
    decrypt_public(CipherText, Key, []).

-doc(#{group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R14B">>}).
-doc """
Public-key decryption using the public key. See also `crypto:public_decrypt/4`

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use  with rsa_pkcs1_padding.
> For digital signatures the use of [`verify/4`](`verify/4`) together
> with [`sign/3`](`sign/3`) is a prefered solution.
""".
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
-doc(#{equiv => encrypt_public(PlainText, Key, []),
       group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R14B">>}).
-spec encrypt_public(PlainText, Key) ->
			     CipherText
                                 when  PlainText :: binary(),
                                       Key :: rsa_public_key(),
                                       CipherText :: binary() .
encrypt_public(PlainText, Key) ->
    encrypt_public(PlainText, Key, []).

-doc(#{group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP 21.1">>}).
-doc """
Public-key encryption using the public key. See also `crypto:public_encrypt/4`.

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.
""".
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
-doc(#{equiv => encrypt_private(PlainText, Key, []),
       group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R14B">>}).
-spec encrypt_private(PlainText, Key) ->
			     CipherText
                                 when  PlainText :: binary(),
                                       Key :: rsa_private_key(),
                                       CipherText :: binary() .
encrypt_private(PlainText, Key) ->
    encrypt_private(PlainText, Key, []).

-doc(#{group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP 21.1">>}).
-doc """
Public-key encryption using the private key.

See also `crypto:private_encrypt/4`. The key, can besides a standard
RSA key, be a map specifing the key algorithm `rsa` and a fun to
handle the encryption operation.  This may be used for customized the
encryption operation with for instance hardware security modules (HSM)
or trusted platform modules (TPM).

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.
> For digital signatures use of [`sign/3`](`sign/3`) together with [`verify/4`](`verify/4`)  is
> the prefered solution.
""".
-spec encrypt_private(PlainText, Key, Options) ->
			     CipherText
                                 when  PlainText :: binary(),
                                       Key :: rsa_private_key(),
                                       Options :: crypto:pk_encrypt_decrypt_opts() | custom_key_opts(),
                                       CipherText :: binary() .
encrypt_private(PlainText, Key, Options)
  when is_binary(PlainText),
       is_list(Options) ->
    Opts = default_options(Options),
    case format_sign_key(Key) of
        {extern, Fun} -> Fun(PlainText, Opts);
        {rsa, CryptoKey} -> crypto:private_encrypt(rsa, PlainText, CryptoKey, Opts)
    end.

%%--------------------------------------------------------------------
%% Description: List available group sizes among the pre-computed dh groups
%%--------------------------------------------------------------------
-doc false.
-spec dh_gex_group_sizes() -> [pos_integer()].
dh_gex_group_sizes() ->
    pubkey_ssh:dh_gex_group_sizes().

%%--------------------------------------------------------------------
%% Description: Select a precomputed group
%%--------------------------------------------------------------------
-doc """
Selects a group for Diffie-Hellman key exchange with the key size in the range
`MinSize...MaxSize` and as close to `SuggestedSize` as possible. If
`Groups == undefined` a default set will be used, otherwise the group is
selected from `Groups`.

First a size, as close as possible to SuggestedSize, is selected. Then one group
with that key size is randomly selected from the specified set of groups. If no
size within the limits of `MinSize` and `MaxSize` is available,
`{error,no_group_found}` is returned.

The default set of groups is listed in `lib/public_key/priv/moduli`. This file
may be regenerated like this:

```text
	$> cd $ERL_TOP/lib/public_key/priv/
	$> generate
         ---- wait until all background jobs has finished. It may take several days !
	$> cat moduli-* > moduli
	$> cd ..; make
```
""".
-doc(#{group => <<"Key API">>,
       since => <<"OTP 18.2">>}).
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
-doc """
Generates a new key pair. Note that except for Diffie-Hellman the public key is
included in the private key structure. See also `crypto:generate_key/2`
""".
-doc(#{group => <<"Key API">>,
       since => <<"OTP R16B01">>}).
-spec generate_key(DHparams | ECparams | RSAparams) ->
                          DHkeys | ECkey | RSAkey
                              when DHparams :: #'DHParameter'{},
                                   DHkeys :: {PublicDH::binary(), PrivateDH::binary()},
                                   ECparams :: {namedCurve, oid() | atom()} | #'ECParameters'{},
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
-doc(#{group => <<"Key API">>,
       since => <<"OTP R16B01">>}).
-doc "Computes shared secret.".
-spec compute_key(OthersECDHkey, MyECDHkey) -> 
                         SharedSecret
                             when OthersECDHkey :: #'ECPoint'{},
                                  MyECDHkey :: #'ECPrivateKey'{},
                                  SharedSecret :: binary().
compute_key(#'ECPoint'{point = Point}, #'ECPrivateKey'{privateKey = PrivKey,
						       parameters = {namedCurve, Curve} = Param})
  when (Curve == ?'id-X25519') orelse
       (Curve == ?'id-X448') ->
    ECCurve = ec_curve_spec(Param),
    crypto:compute_key(eddh, Point, PrivKey, ECCurve);
compute_key(#'ECPoint'{point = Point}, #'ECPrivateKey'{privateKey = PrivKey,
						       parameters = Param}) ->
    ECCurve = ec_curve_spec(Param),
    crypto:compute_key(ecdh, Point, PrivKey, ECCurve).

-doc(#{group => <<"Key API">>,
       since => <<"OTP R16B01">>}).
-doc "Computes shared secret.".

-spec compute_key(OthersDHkey, MyDHkey, DHparms) -> 
                         SharedSecret
                             when OthersDHkey :: crypto:dh_public(), % Was: binary(),
                                  MyDHkey :: crypto:dh_private(), % Was: binary(),
                                  DHparms ::  #'DHParameter'{},
                                  SharedSecret :: binary().
compute_key(PubKey, PrivKey, #'DHParameter'{prime = P, base = G}) ->
    crypto:compute_key(dh, PubKey, PrivKey, [P, G]).

%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R16B01">>}).
-doc """
Translates signature algorithm OID to Erlang digest and signature types.

The `AlgorithmId` is the signature OID from a certificate or a certificate
revocation list.
""".
-spec pkix_sign_types(AlgorithmId) -> 
                             {DigestType, SignatureType}
                                 when AlgorithmId :: oid(),
                                      DigestType ::  digest_type(),
                                      SignatureType :: rsa | dsa | ecdsa | eddsa.
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
    {sha512, ecdsa};
pkix_sign_types(?'id-Ed25519') ->
    {none, eddsa};
pkix_sign_types(?'id-Ed448') ->
    {none, eddsa}.

%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 23.0">>}).
-doc "Translates OID to Erlang digest type".
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
-doc(#{equiv => sign(Msg, DigestType, Key, []),
       group => <<"Sign/Verify API">>}).

-spec sign(Msg, DigestType, Key) ->
                  Signature when Msg ::  binary() | {digest,binary()},
                                 DigestType :: digest_type(),
                                 Key :: private_key(),
                                 Signature :: binary() .
sign(DigestOrPlainText, DigestType, Key) ->
    sign(DigestOrPlainText, DigestType, Key, []).

-doc """
Creates a digital signature.

The `Msg` is either the binary "plain text" data to be signed or it is the
hashed value of "plain text", that is, the digest. The key, can besides a
standard key, be a map specifing a key algorithm and a fun that should handle
the signing. This may be used for customized signing with for instance hardware
security modules (HSM) or trusted platform modules (TPM).
""".
-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP 20.1">>}).
-spec sign(Msg, DigestType, Key, Options) ->
                  Signature when Msg ::  binary() | {digest,binary()},
                                 DigestType :: digest_type(),
                                 Key :: private_key(),
                                 Options :: crypto:pk_sign_verify_opts() | custom_key_opts(),
                                 Signature :: binary() .
sign(Digest, none, Key = #'DSAPrivateKey'{}, Options) when is_binary(Digest) ->
    %% Backwards compatible
    sign({digest, Digest}, sha, Key, Options);
sign(DigestOrPlainText, DigestType, Key, Options) ->
    case format_sign_key(Key) of
	badarg ->
	    erlang:error(badarg, [DigestOrPlainText, DigestType, Key, Options]);
        {extern, Fun} when is_function(Fun) ->
            Fun(DigestOrPlainText, DigestType, Options);
	{Algorithm, CryptoKey} ->
	    crypto:sign(Algorithm, DigestType, DigestOrPlainText, CryptoKey, Options)
    end.

%%--------------------------------------------------------------------
-doc(#{equiv => verify(Msg, DigestType, Signature, Key, []),
       group => <<"Sign/Verify API">>,
       since => <<"OTP R14B">>}).
-spec verify(Msg, DigestType, Signature, Key) ->
                    boolean() when Msg :: binary() | {digest, binary()},
                                   DigestType :: digest_type(),
                                   Signature :: binary(),
                                   Key :: public_key().

verify(DigestOrPlainText, DigestType, Signature, Key) ->
    verify(DigestOrPlainText, DigestType, Signature, Key, []).

-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP 20.1">>}).
-doc """
Verifies a digital signature.

The `Msg` is either the binary "plain text" data or it is the hashed value of
"plain text", that is, the digest.
""".
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
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP 17.5">>}).
-doc """
Creates a distribution point for CRLs issued by the same issuer as `Cert`. Can
be used as input to `pkix_crls_validate/3`
""".

-spec pkix_dist_point(Cert) -> DistPoint when Cert :: cert(),
                                              DistPoint :: #'DistributionPoint'{}.
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
-doc "Extracts distribution points from the certificates extensions.".
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP 17.5">>}).
-spec pkix_dist_points(Cert) -> DistPoints when Cert :: cert(),
                                                DistPoints :: [ #'DistributionPoint'{} ].
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
-doc """
Checks whether the given distribution point matches the Issuing Distribution
Point of the CRL, as described in RFC 5280.

If the CRL doesn't have an Issuing
Distribution Point extension, the distribution point always matches.
""".
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP 19.0">>}).
-spec pkix_match_dist_point(CRL, DistPoint) ->
                                   boolean()
                                       when CRL :: der_encoded() | #'CertificateList'{},
                                            DistPoint :: #'DistributionPoint'{}.
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
-doc "Signs an 'OTPTBSCertificate'. Returns the corresponding DER-encoded certificate.".
-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP R14B">>}).
-spec pkix_sign(Cert, Key) -> Der when Cert :: #'OTPTBSCertificate'{}, 
                                       Key :: private_key(),
                                       Der :: der_encoded().
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
-doc "Verifies PKIX x.509 certificate signature.".
-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP R14B">>}).
-spec pkix_verify(Cert, Key) -> boolean() when Cert :: der_encoded(),
                                               Key :: public_key() .

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

pkix_verify(DerCert, Key = {#'ECPoint'{}, {namedCurve, Curve}}) when (Curve == ?'id-Ed25519'orelse
                                                                      Curve == ?'id-Ed448') andalso is_binary(DerCert) ->
    case pubkey_cert:verify_data(DerCert) of
        {none = DigestType, PlainText, Signature} ->
            verify(PlainText, DigestType, Signature, Key);
        _ ->
            false
    end;
pkix_verify(DerCert, Key = {#'ECPoint'{}, _}) when is_binary(DerCert) ->
    case pubkey_cert:verify_data(DerCert) of
        {none, _, _} ->
            false;
        {DigestType, PlainText, Signature} ->
            verify(PlainText, DigestType, Signature, Key)
    end.

%%--------------------------------------------------------------------
-doc "Verify that `Cert` is the `CRL` signer.".
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP 17.5">>}).
-spec pkix_crl_verify(CRL, Cert) -> boolean()
                                        when CRL  :: der_encoded() | #'CertificateList'{},
                                             Cert :: cert().
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
-doc "Checks if `IssuerCert` issued `Cert`.".
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R14B">>}).
-spec pkix_is_issuer(CertorCRL, IssuerCert) ->
          boolean() when CertorCRL :: cert() | #'CertificateList'{},
                         IssuerCert :: cert().
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
-doc "Checks if a certificate is self-signed.".
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R14B">>}).
-spec pkix_is_self_signed(Cert) -> boolean() when Cert::cert().
%%--------------------------------------------------------------------
pkix_is_self_signed(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_self_signed(OTPCert);
pkix_is_self_signed(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_self_signed(OtpCert).
  
%%--------------------------------------------------------------------
-doc "Checks if a certificate is a fixed Diffie-Hellman certificate.".
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R14B">>}).
-spec pkix_is_fixed_dh_cert(Cert) -> boolean() when Cert::cert().
%%--------------------------------------------------------------------
pkix_is_fixed_dh_cert(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_fixed_dh_cert(OTPCert);
pkix_is_fixed_dh_cert(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_fixed_dh_cert(OtpCert).

%%--------------------------------------------------------------------
-doc "Returns the x509 certificate issuer id, if it can be determined.".
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R14B">>}).
-spec pkix_issuer_id(Cert, IssuedBy) ->
			    {ok, ID::cert_id()} | {error, Reason}
                                when Cert::cert(),
                                     IssuedBy :: self | other,
                                     Reason :: term() .

%%--------------------------------------------------------------------
pkix_issuer_id(#'OTPCertificate'{} = OtpCert, Signed) when (Signed == self) or 
							   (Signed == other) ->
    pubkey_cert:issuer_id(OtpCert, Signed);
pkix_issuer_id(Cert, Signed) when is_binary(Cert) -> 
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_issuer_id(OtpCert, Signed).

%%--------------------------------------------------------------------
-doc "Returns the X509 certificate subject id.".
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 23.1">>}).
-spec pkix_subject_id(Cert) -> ID
              when Cert::cert(),
                   ID::cert_id() .

%%--------------------------------------------------------------------
pkix_subject_id(#'OTPCertificate'{} = OtpCert) ->
    pubkey_cert:subject_id(OtpCert);
pkix_subject_id(Cert) when is_binary(Cert) -> 
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_subject_id(OtpCert).

%%--------------------------------------------------------------------
-doc "Returns the issuer of the `CRL`.".
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP 17.5">>}).
-spec pkix_crl_issuer(CRL) -> Issuer
               when CRL :: der_encoded() | #'CertificateList'{},
                    Issuer :: issuer_name() .
%%--------------------------------------------------------------------
pkix_crl_issuer(CRL) when is_binary(CRL) ->
    pkix_crl_issuer(der_decode('CertificateList', CRL));
pkix_crl_issuer(#'CertificateList'{} = CRL) ->
    pubkey_cert_records:transform(
      CRL#'CertificateList'.tbsCertList#'TBSCertList'.issuer, decode).

%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R14B">>}).
-doc """
Normalizes an issuer name so that it can be easily compared to another issuer
name.
""".
-spec pkix_normalize_name(Issuer) -> Normalized 
                                         when Issuer :: issuer_name() | der_encoded(),
                                              Normalized :: issuer_name() .

%%--------------------------------------------------------------------
pkix_normalize_name(Issuer) when is_binary(Issuer) -> 
    PlainGenName = der_decode('Name', Issuer),
    GenName = pubkey_cert_records:transform(PlainGenName, decode),
    pkix_normalize_name(GenName);
pkix_normalize_name(Issuer) -> 
    pubkey_cert:normalize_general_name(Issuer).

%%-------------------------------------------------------------------- 
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP R16B">>}).
-doc """
Performs a basic path validation according to
[RFC 5280.](http://www.ietf.org/rfc/rfc5280.txt)

However, CRL validation is done separately by [pkix_crls_validate/3
](`pkix_crls_validate/3`)and is to be called from the supplied
`verify_fun`. The policy tree check was added in OTP-26.2 and if the
certificates include policies the constrained policy set with
potential qualifiers will be returned, these values are derived from
the policy tree created as part of the path validation algorithm. The
constrained set can be constrained only by the Certificate Authorities
or also by the user when the option `policy_set` is provided to this
function. The qualifiers convey information about the valid policy and
is intended as information to end users.

Available options:

- **\{verify_fun, \{fun(), UserState::term()\}** - The fun must be
  defined as:

  ```erlang
  fun(OtpCert :: #'OTPCertificate'{},
      Event :: {bad_cert, Reason :: bad_cert_reason() | {revoked, atom()}} |
               {extension, #'Extension'{}},
      UserState :: term()) ->
  	{valid, UserState :: term()} |
  	{valid_peer, UserState :: term()} |
  	{fail, Reason :: term()} |
  	{unknown, UserState :: term()}.
  ```

  or as:

  ```erlang
  fun(OtpCert :: #'OTPCertificate'{},
      DerCert :: der_encoded(),
      Event :: {bad_cert, Reason :: bad_cert_reason() | {revoked, atom()}} |
               {extension, #'Extension'{}},
      UserState :: term()) ->
	{valid, UserState :: term()} |
	{valid_peer, UserState :: term()} |
	{fail, Reason :: term()} |
	{unknown, UserState :: term()}.
  ```

  The verify callback can have 3 or 4 arguments in case the DER encoded
  version is needed by the callback.

  If the verify callback fun returns `{fail, Reason}`, the verification process
  is immediately stopped. If the verify callback fun returns
  `{valid, UserState}`, the verification process is continued. This can be used
  to accept specific path validation errors, such as `selfsigned_peer`, as well
  as verifying application-specific extensions. If called with an extension
  unknown to the user application, the return value `{unknown, UserState}` is to
  be used.

  > #### Note {: .note }
  > If you need the DER encoded version of the certificate and have
  > the OTP decoded version encoding it back can fail to give the correct result,
  > due to work arounds for common misbehaving encoders. So it is recommended
  > to call `pkix_path_validation` with `Cert` and `CertChain` arguments as
  >  `der_encoded() | #cert{}` and `[der_encoded() | #cert{}]`. Also note
  > that the path validation itself needs both the encoded and the
  > decoded version of the certificate.

  > #### Warning {: .warning }
  >
  > Note that user defined custom `verify_fun` may alter original path
  > validation error (e.g `selfsigned_peer`). Use with caution.

- **\{max_path_length, integer()\}** - The `max_path_length` is the maximum
  number of non-self-issued intermediate certificates that can follow the peer
  certificate in a valid certification path. So, if `max_path_length` is 0, the
  PEER must be signed by the trusted ROOT-CA directly, if it is 1, the path can
  be PEER, CA, ROOT-CA, if it is 2, the path can be PEER, CA, CA, ROOT-CA, and
  so on.

- **\{policy_set, \[oid()]\}**(Since OTP 26.2)  
  The set of policies that will be accepted, defaults to the special value
  `[?anyPolicy]` that will accept all policies.

- **\{explicit_policy, boolean()\}**(Since OTP 26.2)  
  Explicitly require that each certificate in the path must include at least one
  of the certificate policies in the `policy_set`.

- **\{inhibit_policy_mapping, boolean()\}**(Since OTP 26.2)  
  Prevent policies to be mapped to other policies.

- **\{inhibit_any_policy, boolean()\}**(Since OTP 26.2)  
  Prevent the special policy `?anyPolicy` from being accepted.

Explanations of reasons for a bad certificate:

- **cert_expired** - Certificate is no longer valid as its expiration date has
  passed.

- **invalid_issuer** - Certificate issuer name does not match the name of the
  issuer certificate in the chain.

- **invalid_signature** - Certificate was not signed by its issuer certificate
  in the chain.

- **name_not_permitted** - Invalid Subject Alternative Name extension.

- **missing_basic_constraint** - Certificate, required to have the basic
  constraints extension, does not have a basic constraints extension.

- **invalid_key_usage** - Certificate key is used in an invalid way according to
  the key-usage extension.

- **\{revoked, crl_reason()\}** - Certificate has been revoked.

- **invalid_validity_dates** - The validity section of the X.509 certificate(s)
  contains invalid date formats not matching the RFC.

- **atom()** - Application-specific error reason that is to be checked by the
  `verify_fun`.
""".
-spec pkix_path_validation(Cert, CertChain, Options) ->
          {ok, {PublicKeyInfo, ConstrainedPolicyNodes}} |
          {error, {bad_cert, Reason :: bad_cert_reason()}}
              when
      Cert :: cert() | combined_cert() | atom(),
      CertChain :: [cert() | combined_cert()],
      Options  :: [{max_path_length, integer()} | {verify_fun, {fun(), term()}}],
      PublicKeyInfo :: public_key_info(),
      ConstrainedPolicyNodes :: [policy_node()].

%%--------------------------------------------------------------------
pkix_path_validation(Cert, CertChain, Options)
  when (not is_atom(Cert)), is_list(CertChain), is_list(Options) ->
    TrustedCert = combined_cert(Cert),
    MaxPathDefault = length(CertChain),
    {VerifyFun, UserState0} =
	proplists:get_value(verify_fun, Options, ?DEFAULT_VERIFYFUN),
    try pubkey_cert:validate_time(TrustedCert, UserState0, VerifyFun) of
        UserState1 -> 
            ValidationState = pubkey_cert:init_validation_state(TrustedCert, 
                                                                MaxPathDefault, 
                                                                [{verify_fun, {VerifyFun, UserState1}} | 
                                                                 proplists:delete(verify_fun, Options)]),
            case exists_duplicate_cert(CertChain) of
                true ->
                    {error, {bad_cert, duplicate_cert_in_path}};
                false ->
                    path_validation(CertChain, ValidationState)
            end
    catch
        throw:{bad_cert, _} = Result ->
            {error, Result}
    end;
pkix_path_validation(PathErr, [Cert0 | Chain], Options0) when is_atom(PathErr)->
    {VerifyFun, Userstat0} =
	proplists:get_value(verify_fun, Options0, ?DEFAULT_VERIFYFUN),
    Cert = combined_cert(Cert0),
    Reason = {bad_cert, PathErr},
    try pubkey_cert:apply_fun(VerifyFun, Cert#cert.otp, Cert#cert.der, Reason, Userstat0) of
	{valid, Userstate} ->
	    Options = proplists:delete(verify_fun, Options0),
	    pkix_path_validation(Cert, Chain, [{verify_fun,
						   {VerifyFun, Userstate}}| Options]);
	{fail, UserReason} ->
	    {error, UserReason}
    catch
	_:_ ->
	    {error, Reason}
    end.
%--------------------------------------------------------------------
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP R16B">>}).
-doc """
Performs CRL validation. It is intended to be called from the verify fun of
[pkix_path_validation/3 ](`pkix_path_validation/3`).

Available options:

- **\{update_crl, fun()\}** - The fun has the following type specification:

  ```erlang
   fun(#'DistributionPoint'{}, #'CertificateList'{}) ->
          #'CertificateList'{}
  ```

  The fun uses the information in the distribution point to access the latest
  possible version of the CRL. If this fun is not specified, Public Key uses the
  default implementation:

  ```text
   fun(_DP, CRL) -> CRL end
  ```

- **\{issuer_fun, \{fun(), UserState::term()\}\}** - The fun has the following type
  specification:

  ```erlang
  fun(#'DistributionPoint'{}, #'CertificateList'{},
      {rdnSequence,[#'AttributeTypeAndValue'{}]}, UserState::term()) ->
  	{ok, #'OTPCertificate'{}, [der_encoded]}
  ```

  The fun returns the root certificate and certificate chain that has signed the
  CRL.

  ```erlang
   fun(DP, CRL, Issuer, UserState) -> {ok, RootCert, CertChain}
  ```

- **\{undetermined_details, boolean()\}** - Defaults to false. When revocation
  status cannot be determined, and this option is set to true, details of why no
  CRLs where accepted are included in the return value.
""".
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
%% Description: Validates a hostname to RFC 6125
%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       equiv => pkix_verify_hostname(Cert, ReferenceIDs, []),
       since => <<"OTP 19.3">>}).
-spec pkix_verify_hostname(Cert, ReferenceIDs) -> boolean()
                                                      when Cert :: cert(),
                                                           ReferenceIDs :: [{uri_id | dns_id | ip | srv_id | atom() | oid(),  string()}
                                                                           | {ip, inet:ip_address() | string()}] .
pkix_verify_hostname(Cert, ReferenceIDs) ->
    pkix_verify_hostname(Cert, ReferenceIDs, []).

-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 19.3">>}).
-doc """
This function checks that the _Presented Identifier_ (e.g hostname) in a peer
certificate is in agreement with at least one of the _Reference Identifier_ that
the client expects to be connected to.

The function is intended to be added as an extra client check of the
peer certificate when performing
[public_key:pkix_path_validation/3](`pkix_path_validation/3`)

See [RFC 6125](https://tools.ietf.org/html/rfc6125) for detailed information
about hostname verification. The
[User's Guide](using_public_key.md#verify_hostname) and
[code examples](using_public_key.md#verify_hostname_examples) describes this
function more detailed.

The option funs are described here:

- **`match_fun`**

  ```erlang
  fun(ReferenceId::ReferenceId() | FQDN::string(),
      PresentedId::{dNSName,string()} | {uniformResourceIdentifier,string() |
                   {iPAddress,list(byte())} | {OtherId::atom()|oid(),term()}})
  ```

  This function replaces the default host name matching rules. The fun should
  return a boolean to tell if the Reference ID and Presented ID matches or not.
  The match fun can also return a third value, value, the atom `default`, if the
  default matching rules shall apply. This makes it possible to augment the
  tests with a special case:

  ```text
  fun(....) -> true;   % My special case
     (_, _) -> default % all others falls back to the inherit tests
  end
  ```

  See `pkix_verify_hostname_match_fun/1` for a function that takes a protocol
  name as argument and returns a `fun/2` suitable for this option and
  [Re-defining the match operation](using_public_key.md#redefining_match_op) in
  the User's Guide for an example.

  > #### Note {: .info }
  >
  > Reference Id values given as binaries will be converted to strings, and ip
  > references may be given in string format that is "10.0.1.1" or
  > "1234::5678:9012" as well as on the format `t:inet:ip_address/0`

- **`fail_callback`** - If a matching fails, there could be circumstances when
  the certificate should be accepted anyway. Think for example of a web browser
  where you choose to accept an outdated certificate. This option enables
  implementation of such an exception but for hostnames. This `fun/1` is called
  when no `ReferenceID` matches. The return value of the fun (a `t:boolean/0`)
  decides the outcome. If `true` the the certificate is accepted otherwise it is
  rejected. See
  ["Pinning" a Certificate](using_public_key.md#pinning-a-certificate) in the
  User's Guide.

- **`fqdn_fun`** - This option augments the host name extraction from URIs and
  other Reference IDs. It could for example be a very special URI that is not
  standardised. The fun takes a Reference ID as argument and returns one of:

  - the hostname
  - the atom `default`: the default host name extract function will be used
  - the atom `undefined`: a host name could not be extracted. The
    pkix_verify_hostname/3 will return `false`.

  For an example, see
  [Hostname extraction](using_public_key.md#hostname_extraction) in the User's
  Guide.
""".
-spec pkix_verify_hostname(Cert, ReferenceIDs, Options) ->
                                  boolean()
                                      when Cert :: cert(),
                                           ReferenceIDs :: [{uri_id | dns_id | ip | srv_id | atom() | oid(),  string()}
                                                                           | {ip, inet:ip_address() | string()}],
                                           Options :: [{match_fun | fail_callback | fqdn_fun, fun()}] .

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

-doc """
The return value of calling this function is intended to be used in the
`match_fun` option in `pkix_verify_hostname/3`.

The returned fun augments the verify hostname matching according to the specific
rules for the protocol in the argument.

> #### Note {: .info }
>
> Currently supported https fun will allow wildcard certificate matching as
> specified by the HTTP standard. Note that for instance LDAP have a different
> set of wildcard matching rules. If you do not want to allow wildcard
> certificates (recommended from a security perspective) or otherwise customize
> the hostname match the default match function used by ssl application will be
> sufficient.
""".
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 21.0">>}).
-spec pkix_verify_hostname_match_fun(Protocol) ->  Result when
      Protocol :: https,
      Result :: fun().


pkix_verify_hostname_match_fun(https) ->
    fun({dns_id,FQDN=[_|_]}, {dNSName,Name=[_|_]}) -> verify_hostname_match_wildcard(FQDN, Name);
       (_, _) -> default
    end.

%%--------------------------------------------------------------------
-doc false.
-spec ssh_curvename2oid(binary()) -> oid().

%% Description: Converts from the ssh name of elliptic curves to
%% the OIDs.
%%--------------------------------------------------------------------
ssh_curvename2oid(<<"nistp256">>) ->  ?'secp256r1';
ssh_curvename2oid(<<"nistp384">>) ->  ?'secp384r1';
ssh_curvename2oid(<<"nistp521">>) ->  ?'secp521r1'.

%%--------------------------------------------------------------------
-doc false.
-spec oid2ssh_curvename(oid()) -> binary().

%% Description: Converts from elliptic curve OIDs to the ssh name.
%%--------------------------------------------------------------------
oid2ssh_curvename(?'secp256r1') -> <<"nistp256">>;
oid2ssh_curvename(?'secp384r1') -> <<"nistp384">>;
oid2ssh_curvename(?'secp521r1') -> <<"nistp521">>.


%%--------------------------------------------------------------------
-doc """
Generates a short hash of an issuer name. The hash is returned as a string
containing eight hexadecimal digits.

The return value of this function is the same as the result of the commands
`openssl crl -hash` and `openssl x509 -issuer_hash`, when passed the issuer name
of a CRL or a certificate, respectively. This hash is used by the `c_rehash`
tool to maintain a directory of symlinks to CRL files, in order to facilitate
looking up a CRL by its issuer name.
""".
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP 19.0">>}).
-spec short_name_hash(Name) -> string() when Name :: issuer_name() .

%% Description: Generates OpenSSL-style hash of a name.
%%--------------------------------------------------------------------
short_name_hash({rdnSequence, _Attributes} = Name) ->
    HashThis = encode_name_for_short_hash(Name),
    <<HashValue:32/little, _/binary>> = crypto:hash(sha, HashThis),
    string:to_lower(string:right(integer_to_list(HashValue, 16), 8, $0)).


%%--------------------------------------------------------------------
-doc """
Creates certificate configuration(s) consisting of certificate and its private
key plus CA certificate bundle, for a client and a server, intended to
facilitate automated testing of applications using X509-certificates, often
through SSL/TLS. The test data can be used when you have control over both the
client and the server in a test scenario.

When this function is called with a map containing client and server chain
specifications; it generates both a client and a server certificate chain where
the `cacerts` returned for the server contains the root cert the server should
trust and the intermediate certificates the server should present to connecting
clients. The root cert the server should trust is the one used as root of the
client certificate chain. Vice versa applies to the `cacerts` returned for the
client. The root cert(s) can either be pre-generated with
[pkix_test_root_cert/2 ](`pkix_test_root_cert/2`), or if options are specified;
it is (they are) generated.

When this function is called with a list of certificate options; it generates a
configuration with just one node certificate where `cacerts` contains the root
cert and the intermediate certs that should be presented to a peer. In this case
the same root cert must be used for all peers. This is useful in for example an
Erlang distributed cluster where any node, towards another node, acts either as
a server or as a client depending on who connects to whom. The generated
certificate contains a subject altname, which is not needed in a client
certificate, but makes the certificate useful for both roles.

Explanation of the options used to customize certificates in the generated
chains:

- **\{digest, digest_type()\}** - Hash algorithm to be used for signing the
  certificate together with the key option. Defaults to sha that is sha1.

- **\{key,  ec_params()| {rsa, Size:pos_integer(), Prime::pos_integer()} | private_key()\}** - Parameters to be used to call
  public_key:generate_key/1, to generate a key, or an existing key. Defaults to
  generating an ECDSA key. Note this could fail if Erlang/OTP is compiled with a
  very old cryptolib.

- **\{validity, \{From::erlang:timestamp(), To::erlang:timestamp()\}\}** - The
  validity period of the certificate.

- **\{extensions, \[#'Extension'\{\}]\}** - Extensions to include in the
  certificate.

  Default extensions included in CA certificates if not otherwise specified are:

  ```erlang
  [#'Extension'{extnID = ?'id-ce-keyUsage',
                extnValue = [keyCertSign, cRLSign],
                critical = false},
  #'Extension'{extnID = ?'id-ce-basicConstraints',
               extnValue = #'BasicConstraints'{cA = true},
               critical = true}]
  ```

  Default extensions included in the server peer cert if not otherwise specified
  are:

  ```erlang
  [#'Extension'{extnID = ?'id-ce-keyUsage',
                extnValue = [digitalSignature, keyAgreement],
                critical = false},
  #'Extension'{extnID = ?'id-ce-subjectAltName',
               extnValue = [{dNSName, Hostname}],
               critical = false}]
  ```

  Hostname is the result of calling net_adm:localhost() in the Erlang node where
  this function is called.

> #### Note {: .info }
>
> Note that the generated certificates and keys does not provide a formally
> correct PKIX-trust-chain and they cannot be used to achieve real security.
> This function is provided for testing purposes only.
""".

-doc(#{group => <<"Test Data API">>,
       since => <<"OTP 20.1">>}).
-spec pkix_test_data(ChainConf) -> TestConf when
      ChainConf :: #{server_chain:= chain_opts(),
                     client_chain:= chain_opts()} |
                   chain_opts(),
      TestConf ::  #{server_config := [conf_opt()],  client_config :=  [conf_opt()]} | [conf_opt()].

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
-doc """
Generates a root certificate that can be used in multiple calls to
`pkix_test_data/1` when you want the same root certificate for several generated
certificates.
""".
-doc(#{group => <<"Test Data API">>,
       since => <<"OTP 20.2">>}).
-spec pkix_test_root_cert(Name, Options) ->
                                 RootCertAndKey
                                     when Name :: string(),
                                          Options :: [cert_opt()],
                                          RootCertAndKey :: #{cert := der_encoded(), key := private_key()}.
%% Description: Generates a root cert suitable for pkix_test_data/1
%%--------------------------------------------------------------------

pkix_test_root_cert(Name, Opts) ->
    pubkey_cert:root_cert(Name, Opts).

%%--------------------------------------------------------------------
-doc """
Perform OCSP response validation according to RFC 6960. Returns {'ok', Details} when OCSP
response is successfully validated and \{error, \{bad_cert, Reason\}\}
otherwise.

Available options:

- **\{is_trusted_responder_fun, fun()\}** - The fun has the following type
  specification:

  ```text
   fun(#cert{}) ->
  	  boolean()
  ```

  The fun returns the `true` if certificate in the argument is trusted. If this
  fun is not specified, Public Key uses the default implementation:

  ```text
   fun(_) -> false end
  ```

> #### Note {: .info }
>
> OCSP response can be provided without a nonce value - even if it was requested
> by the client. In such cases {missing, ocsp_nonce} will be returned
> in Details list.
""".
-doc(#{group => <<"Certificate Revocation API">>,
       since => <<"OTP 27.0">>}).
-spec pkix_ocsp_validate(Cert, IssuerCert, OcspRespDer, NonceExt, Options) ->
          {ok, Details} | {error, {bad_cert, Reason}}
              when Cert::cert(),
                   IssuerCert::cert(),
                   OcspRespDer::der_encoded(),
                   NonceExt::undefined | binary(),
                   Options::[{is_trusted_responder_fun,
                              fun((combined_cert()) -> boolean)}],
                   Details::list(),
                   Reason::bad_cert_reason().
%% Description: Validate OCSP response
%%--------------------------------------------------------------------
pkix_ocsp_validate(DerCert, IssuerCert, OcspRespDer, NonceExt, Options)
  when is_binary(DerCert) ->
    pkix_ocsp_validate(
      pkix_decode_cert(DerCert, otp), IssuerCert, OcspRespDer, NonceExt, Options);
pkix_ocsp_validate(Cert, DerIssuerCert, OcspRespDer, NonceExt, Options)
  when is_binary(DerIssuerCert) ->
    pkix_ocsp_validate(
      Cert, pkix_decode_cert(DerIssuerCert, otp), OcspRespDer, NonceExt, Options);
pkix_ocsp_validate(Cert, IssuerCert, OcspRespDer, NonceExt, Options)
  when is_record(Cert, 'OTPCertificate'),
       is_record(IssuerCert, 'OTPCertificate') ->
    IsTrustedResponderFun =
	proplists:get_value(is_trusted_responder_fun, Options,
                            fun(_) -> false end),
    maybe
        {ok, BasicOcspResponse = #'BasicOCSPResponse'{certs = Certs0}} ?=
            pubkey_ocsp:decode_response(OcspRespDer),
        Certs = case Certs0 of
                    asn1_NOVALUE -> []; % case when certs field is empty
                    _ -> Certs0
                end,
        OcspResponseCerts = [combined_cert(C) || C <- Certs] ++
            [#cert{der = <<>>, otp = IssuerCert}],
        {ok, Responses, Details} ?=
            pubkey_ocsp:verify_response(
              BasicOcspResponse, OcspResponseCerts, NonceExt, IssuerCert,
              IsTrustedResponderFun),
        {ok, #'SingleResponse'{certStatus = CertStatus}} ?=
            pubkey_ocsp:find_single_response(Cert, IssuerCert, Responses),
        pubkey_ocsp:status(CertStatus, Details)
    else
        {error, Reason} ->
            {error, {bad_cert, {revocation_status_undetermined, Reason}}}
    end.

%%--------------------------------------------------------------------
-doc false.
-spec ocsp_extensions(undefined | binary()) -> list().
%% Description: Get OCSP stapling extensions for request
%%--------------------------------------------------------------------
ocsp_extensions(Nonce) ->
    [Extn || Extn <- [pubkey_ocsp:get_nonce_extn(Nonce)],
             erlang:is_record(Extn, 'Extension')].

%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 25.0">>}).
-doc """
Returns the trusted CA certificates if any are loaded, otherwise uses
`cacerts_load/0` to load them. The function fails if no `cacerts` could be
loaded.
""".

-spec cacerts_get() -> [combined_cert()].
%%--------------------------------------------------------------------
cacerts_get() ->
    pubkey_os_cacerts:get().

%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 25.0">>}).
-doc """
Loads the OS supplied trusted CA certificates.

This can be overridden by setting the `cacerts_path`
environment key of the `public_key` application with
the location of an alternative certificate.
You can set it via the command line as:

    erl -public_key cacerts_path '"/path/to/certs.pem"'

Use it with care. It is your responsibility to ensure
that the certificates found in this alternative path
can be trusted by the running system.
""".

-spec cacerts_load() -> ok | {error, Reason::term()}.
%%--------------------------------------------------------------------
cacerts_load() ->
    pubkey_os_cacerts:load().


%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 25.0">>}).
-doc "Loads the trusted CA certificates from a file.".
-spec cacerts_load(File::file:filename_all()) -> ok | {error, Reason::term()}.
%%--------------------------------------------------------------------
cacerts_load(File) ->
    pubkey_os_cacerts:load([File]).

%%--------------------------------------------------------------------
-doc(#{group => <<"Certificate API">>,
       since => <<"OTP 25.0">>}).
-doc "Clears any loaded CA certificates, returns true if any was loaded.".
-spec cacerts_clear() -> boolean().
%%--------------------------------------------------------------------
cacerts_clear() ->
    pubkey_os_cacerts:clear().

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
ec_decode_params(AlgId, _) when AlgId == ?'id-Ed25519';
                                AlgId == ?'id-Ed448' ->
    {namedCurve, AlgId};
ec_decode_params(_, Params) ->
    der_decode('EcpkParameters', Params).

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

format_sign_key(#{encrypt_fun := KeyFun}) ->
    {extern, KeyFun};
format_sign_key(#{sign_fun := KeyFun}) ->
    {extern, KeyFun};
format_sign_key(Key = #'RSAPrivateKey'{}) ->
    {rsa, format_rsa_private_key(Key)};
format_sign_key({#'RSAPrivateKey'{} = Key, _}) ->
    %% Params are handled in options arg
    %% provided by caller.
    {rsa, format_rsa_private_key(Key)};
format_sign_key(#'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) ->
    {dss, [P, Q, G, X]};
format_sign_key(#'ECPrivateKey'{privateKey = PrivKey, parameters = {namedCurve, Curve} = Param})
  when (Curve == ?'id-Ed25519') orelse (Curve == ?'id-Ed448')->
    ECCurve = ec_curve_spec(Param),
    {eddsa, [PrivKey, ECCurve]};
format_sign_key(#'ECPrivateKey'{privateKey = PrivKey, parameters = Param}) ->
    ECCurve = ec_curve_spec(Param),
    {ecdsa, [PrivKey, ECCurve]};
format_sign_key({ed_pri, Curve, _Pub, Priv}) ->
    {eddsa, [Priv,Curve]};
format_sign_key(_) ->
    badarg.

format_verify_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Exp}) ->
    {rsa, [Exp, Mod]};
format_verify_key({#'ECPoint'{point = Point}, {namedCurve, Curve} = Param}) when (Curve == ?'id-Ed25519') orelse
                                                                                 (Curve == ?'id-Ed448') ->
    ECCurve = ec_curve_spec(Param),
    {eddsa, [Point, ECCurve]};
format_verify_key({#'ECPoint'{point = Point}, Param}) ->
    ECCurve = ec_curve_spec(Param),
    {ecdsa, [Point, ECCurve]};
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

rsa_opts(#'RSASSA-PSS-params'{saltLength = SaltLen, 
                              maskGenAlgorithm = 
                                  #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                      parameters = #'HashAlgorithm'{algorithm = HashAlgoOid}
                                                     }}) ->
    HashAlgo = pkix_hash_type(HashAlgoOid),
    [{rsa_padding, rsa_pkcs1_pss_padding},
     {rsa_pss_saltlen, SaltLen},
     {rsa_mgf1_md, HashAlgo}].

do_pem_entry_encode(Asn1Type, Entity, CipherInfo, Password) ->
    Der = der_encode(Asn1Type, Entity),
    DecryptDer = pubkey_pem:cipher(Der, CipherInfo, Password),
    {Asn1Type, DecryptDer, CipherInfo}.
  
do_pem_entry_decode({Asn1Type,_, _} = PemEntry, Password) ->
    Der = pubkey_pem:decipher(PemEntry, Password),
    der_decode(Asn1Type, Der).

%% The only way a path with duplicates could be somehow wrongly
%% passed is if the certs are located together and also are
%% self-signed. This is what we need to possible protect against. We
%% only check for togetherness here as it helps with the case not
%% otherwise caught. It can result in a different error message for
%% cases already failing before but that is not important, the
%% important thing is that it will be rejected.
exists_duplicate_cert([]) ->
    false;
exists_duplicate_cert([Cert, Cert | _]) ->
    true;
exists_duplicate_cert([_ | Rest]) ->
    exists_duplicate_cert(Rest).

path_validation([], #path_validation_state{working_public_key_algorithm
					   = Algorithm,
					   working_public_key =
					   PublicKey,
					   working_public_key_parameters 
					   = PublicKeyParams,
					   valid_policy_tree = Tree
					  }) ->
    ValidPolicyNodeSet0 = pubkey_policy_tree:constrained_policy_node_set(Tree),
    CollectQualifiers = fun(#{expected_policy_set := PolicySet} = Node) ->
                                QF = fun(Policy) ->
                                             pubkey_policy_tree:collect_qualifiers(Tree, Policy)
                                     end,
                                Qualifiers = lists:flatmap(QF, PolicySet),
                                Node#{qualifier_set => Qualifiers}
                        end,
    ValidPolicyNodeSet =  lists:map(CollectQualifiers, ValidPolicyNodeSet0),
    {ok, {{Algorithm, PublicKey, PublicKeyParams}, ValidPolicyNodeSet}};

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
    DerCert = der_cert(Cert),

    try pubkey_cert:apply_fun(VerifyFun, OtpCert, DerCert, Reason, UserState0) of
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

validate(Cert0, #path_validation_state{working_issuer_name = Issuer,
                                      working_public_key = Key,
                                      working_public_key_parameters = 
                                          KeyParams, 
                                      permitted_subtrees = Permit,
                                      excluded_subtrees = Exclude,
                                      last_cert = Last,
                                      user_state = UserState0,
                                      verify_fun = VerifyFun} =
	     ValidationState0) ->
    
    Cert = combined_cert(Cert0),

    {ValidationState1, UserState1} =
	pubkey_cert:validate_extensions(Cert, ValidationState0, UserState0,
					VerifyFun),

    %% We want the key_usage extension to be checked before we validate
    %% other things so that CRL validation errors will comply to standard
    %% test suite description

    UserState2 = pubkey_cert:validate_time(Cert, UserState1, VerifyFun),

    UserState3 = pubkey_cert:validate_issuer(Cert, Issuer, UserState2, VerifyFun),

    UserState4 = pubkey_cert:validate_names(Cert, Permit, Exclude, Last,
					    UserState3, VerifyFun),

    UserState5 = pubkey_cert:validate_signature(Cert, der_cert(Cert),
						Key, KeyParams, UserState4, VerifyFun),
    UserState = case Last of
		    false ->
			pubkey_cert:verify_fun(Cert, valid, UserState5, VerifyFun);
		    true ->
			pubkey_cert:verify_fun(Cert, valid_peer,
					       UserState5, VerifyFun)
		end,

    ValidationState  = 
	ValidationState1#path_validation_state{user_state = UserState},

    pubkey_cert:prepare_for_next_cert(Cert, ValidationState).

otp_cert(Der) when is_binary(Der) ->
    pkix_decode_cert(Der, otp);
otp_cert(#'OTPCertificate'{} = Cert) ->
    Cert;
otp_cert(#cert{otp = OtpCert}) ->
    OtpCert.

combined_cert(#'Certificate'{} = Cert) ->
    Der = der_encode('Certificate', Cert),
    Otp = pkix_decode_cert(Der, otp),
    #cert{der = Der, otp = Otp};
combined_cert(#cert{} = CombinedCert) ->
    CombinedCert;
combined_cert(Der) when is_binary(Der) ->
    Otp = pkix_decode_cert(Der, otp),
    #cert{der = Der, otp = Otp};
combined_cert(#'OTPCertificate'{} = Otp) ->
    %% Note that this conversion is not
    %% safe due to encodeing work arounds.
    Der = der_cert(Otp),
    #cert{der = Der, otp = Otp}.

der_cert(#'OTPCertificate'{} = Cert) ->
    pkix_encode('OTPCertificate', Cert, otp);
der_cert(Der) when is_binary(Der) ->
    Der;
der_cert(#cert{der = DerCert}) ->
    DerCert.

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

ec_generate_key(Params) ->
    Curve = ec_curve_spec(Params),
    CurveType = ec_curve_type(Curve),
    Term = crypto:generate_key(CurveType, Curve),
    NormParams = ec_normalize_params(Params),
    ec_key(Term, NormParams).

ec_normalize_params({namedCurve, Name}) when is_atom(Name) ->
	{namedCurve, pubkey_cert_records:namedCurves(Name)};
ec_normalize_params(#'ECParameters'{} = ECParams) ->
	{ecParameters, ECParams};
ec_normalize_params(Other) -> Other.

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
ec_curve_spec({namedCurve, ed25519 = Name}) ->
    Name;
ec_curve_spec({namedCurve, ed448 = Name}) ->
    Name;
ec_curve_spec({namedCurve, Name}) when is_atom(Name) ->
   (Name).

ec_curve_type(ed25519) ->
    eddsa;
ec_curve_type(ed448) ->
    eddsa;
ec_curve_type(x25519) ->
    eddh;
ec_curve_type(x448) ->
    eddh;
ec_curve_type(_) ->
    ecdh.

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
            == if tuple_size(R)==4 -> R;
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
            == if tuple_size(R)==8 -> R;
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
    [F1|Fs] = string:split(to_lower_ascii(FQDN), "."),
    [N1|Ns] = string:split(to_lower_ascii(Name), "."),
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

match_wild_sfx([$*|_],      _) -> false; % Bad name (no wildcards allowed)
match_wild_sfx(_,      [$*|_]) -> false; % Bad pattern (no more wildcards allowed)
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

subject_public_key_info(Alg, PubKey) ->
    #'OTPSubjectPublicKeyInfo'{algorithm = Alg, subjectPublicKey = PubKey}.

%%%################################################################
%%%#
%%%# Tracing
%%%#
-doc false.
handle_trace(crt,
             {call, {?MODULE, pkix_decode_cert, [Cert, _Type]}}, Stack) ->
    {io_lib:format("Cert = ~W", [Cert, 5]), Stack};
    %% {io_lib:format("Cert = ~s", [ssl_test_lib:format_cert(Cert)]), Stack};
handle_trace(csp,
             {call, {?MODULE, pkix_ocsp_validate, [Cert, IssuerCert | _]}}, Stack) ->
    {io_lib:format("#2 OCSP validation started~nCert = ~W IssuerCert = ~W",
                   [Cert, 7, IssuerCert, 7]), Stack};
    %% {io_lib:format("#2 OCSP validation started~nCert = ~s IssuerCert = ~s",
    %%                [ssl_test_lib:format_cert(Cert),
    %%                 ssl_test_lib:format_cert(IssuerCert)]), Stack};
handle_trace(csp,
             {call, {?MODULE, otp_cert, [Cert]}}, Stack) ->
    {io_lib:format("Cert = ~W", [Cert, 5]), Stack};
    %% {io_lib:format("Cert = ~s", [ssl_test_lib:format_cert(otp_cert(Cert))]), Stack};
handle_trace(csp,
             {return_from, {?MODULE, pkix_ocsp_validate, 5}, Return},
             Stack) ->
    {io_lib:format("#2 OCSP validation result = ~p", [Return]), Stack}.
