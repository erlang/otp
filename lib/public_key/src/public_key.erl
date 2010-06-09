%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-export([decode_private_key/1, decode_private_key/2, decode_dhparams/1, 
	 decrypt_private/2, decrypt_private/3, encrypt_public/2, 
	 encrypt_public/3, decrypt_public/2, decrypt_public/3, 
	 encrypt_private/2, encrypt_private/3, gen_key/1, sign/2, sign/3,
	 verify_signature/3, verify_signature/4, verify_signature/5,
	 pem_to_der/1, pem_to_der/2, der_to_pem/2,
	 pkix_decode_cert/2, pkix_encode_cert/1, pkix_transform/2,
	 pkix_is_self_signed/1, pkix_is_fixed_dh_cert/1,
	 pkix_issuer_id/2,
	 pkix_is_issuer/2, pkix_normalize_general_name/1,
	 pkix_path_validation/3
	]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: decode_private_key(KeyInfo [,Password]) -> 
%%                                     {ok, PrivateKey} | {error, Reason}
%%
%%	KeyInfo = {Type, der_bin(), ChipherInfo} - as returned from
%%	pem_to_der/[1,2] for private keys
%%      Type = rsa_private_key | dsa_private_key
%%	ChipherInfo = opaque() | no_encryption
%%
%% Description: Decodes an asn1 der encoded private key.
%%--------------------------------------------------------------------
decode_private_key(KeyInfo) ->
    decode_private_key(KeyInfo, no_passwd).

decode_private_key(KeyInfo = {rsa_private_key, _, _}, Password) ->
    DerEncoded = pubkey_pem:decode_key(KeyInfo, Password),
    'OTP-PUB-KEY':decode('RSAPrivateKey', DerEncoded);
decode_private_key(KeyInfo = {dsa_private_key, _, _}, Password) ->
    DerEncoded = pubkey_pem:decode_key(KeyInfo, Password),
    'OTP-PUB-KEY':decode('DSAPrivateKey', DerEncoded).


%%--------------------------------------------------------------------
%% Function: decode_dhparams(DhParamInfo) -> 
%%                                     {ok, DhParams} | {error, Reason}
%%
%%	DhParamsInfo = {Type, der_bin(), ChipherInfo} - as returned from
%%	pem_to_der/[1,2] for DH parameters.
%%      Type = dh_params
%%	ChipherInfo = opaque() | no_encryption
%%
%% Description: Decodes an asn1 der encoded DH parameters.
%%--------------------------------------------------------------------
decode_dhparams({dh_params, DerEncoded, not_encrypted}) ->
    'OTP-PUB-KEY':decode('DHParameter', DerEncoded).

%%--------------------------------------------------------------------
%% Function: decrypt_private(CipherText, Key) -> 
%%           decrypt_private(CipherText, Key, Options) -> PlainTex
%%           decrypt_public(CipherText, Key) -> 
%%           decrypt_public(CipherText, Key, Options) -> PlainTex
%%
%%	CipherText = binary()
%%      Key = rsa_key()
%%      PlainText = binary()
%%
%% Description: Decrypts <CipherText>.
%%--------------------------------------------------------------------
decrypt_private(CipherText, Key) ->
    decrypt_private(CipherText, Key, []).
decrypt_private(CipherText, Key, Options)  ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    pubkey_crypto:decrypt_private(CipherText, Key, Padding).

decrypt_public(CipherText, Key) ->
    decrypt_public(CipherText, Key, []).
decrypt_public(CipherText, Key, Options)  ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    pubkey_crypto:decrypt_public(CipherText, Key, Padding).

%%--------------------------------------------------------------------
%% Function: encrypt_public(PlainText, Key, Options) -> CipherText
%%           encrypt_private(PlainText, Key, Options) -> CipherText
%%
%%      PlainText = iolist()
%%      Key = rsa_private_key()
%%      CipherText = binary()
%%
%% Description: Encrypts <Plain>
%%--------------------------------------------------------------------
encrypt_public(PlainText, Key) ->
    encrypt_public(PlainText, Key, []).
encrypt_public(PlainText, Key, Options)  ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    pubkey_crypto:encrypt_public(PlainText, Key, Padding).

encrypt_private(PlainText, Key) ->
    encrypt_private(PlainText, Key, []).
encrypt_private(PlainText, Key, Options)  ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    pubkey_crypto:encrypt_private(PlainText, Key, Padding).

%%--------------------------------------------------------------------
%% Function: gen_key(Params) -> Keys
%%
%%	Params = #'DomainParameters'{} - Currently only supported option
%%	Keys = {PublicDHKey = integer(), PrivateDHKey = integer()}
%%
%% Description: Generates keys. Currently supports Diffie-Hellman keys.
%%--------------------------------------------------------------------
gen_key(#'DHParameter'{prime = P, base = G}) when is_integer(P), 
						  is_integer(G) ->
    pubkey_crypto:gen_key(diffie_hellman, [P, G]).

%%--------------------------------------------------------------------
%% Function: pem_to_der(CertSource) ->
%%           pem_to_der(CertSource, Password) -> {ok, [Entry]} |
%%                                               {error, Reason}
%%
%%      CertSource = File | CertData
%%      CertData = binary()
%%	File = path()
%%	Entry = {entry_type(), der_bin(), ChipherInfo}
%%      ChipherInfo = opague() | no_encryption
%%      der_bin() = binary()
%%	entry_type() = cert | cert_req | rsa_private_key | dsa_private_key
%%      dh_params
%%
%% Description: decode PEM binary data or a PEM file and return
%% entries as asn1 der encoded entities. Currently supported entry
%% types are certificates, certificate requests, rsa private keys and
%% dsa private keys. In the case of a key entry ChipherInfo will be
%% private keys and Diffie Hellam parameters .In the case of a key
%% entry ChipherInfo will be used by decode_private_key/2 if the key
%% is protected by a password.
%%--------------------------------------------------------------------
pem_to_der(CertSource) ->
    pem_to_der(CertSource, no_passwd).

pem_to_der(File, Password) when is_list(File) ->
    pubkey_pem:read_file(File, Password);
pem_to_der(PemBin, Password) when is_binary(PemBin) ->
    pubkey_pem:decode(PemBin, Password).

der_to_pem(File, TypeDerList) ->
    pubkey_pem:write_file(File, TypeDerList).

%%--------------------------------------------------------------------
%% Function: pkix_decode_cert(BerCert, Type) -> {ok, Cert} | {error, Reason}
%%
%%	BerCert = binary()
%%      Type = plain | otp
%%      Cert = certificate()
%%
%% Description:  Decodes an asn1 ber encoded pkix certificate.
%% otp - Uses OTP-PKIX.asn1 to decode known extensions and
%% enhance the signature field in #'Certificate'{} and '#TBSCertificate'{}. 
%%--------------------------------------------------------------------
pkix_decode_cert(BinCert, Type) ->
    pubkey_cert_records:decode_cert(BinCert, Type).

%%--------------------------------------------------------------------
%% Function: pkix_encode_cert(Cert) -> {ok, binary()} | {error, Reason}
%%
%%	Cert = #'Certificate'{} 
%%
%% Description: Encodes a certificate record using asn1.
%%--------------------------------------------------------------------
pkix_encode_cert(Cert) ->
    pubkey_cert_records:encode_cert(Cert).
    
%%--------------------------------------------------------------------
%% Function: pkix_transform(CertPart, Op) -> TransformedCertPart
%%
%%	CertPart = pkix part data
%%      Op = encode | decode
%%
%% Description: Transform parts of a pkix certificate between 'plain' format
%% and the internal 'otp' format, see pkix_decode_cert/2.
%% Decode transforms from 'plain' to 'otp' and encode from 'otp' to 'plain'
%% format.
%%--------------------------------------------------------------------
pkix_transform(CertPart, Op) ->
    pubkey_cert_records:transform(CertPart, Op).

%%--------------------------------------------------------------------
%% Function: pkix_path_validation(TrustedCert, CertChain, Options) -> 
%%   {ok, {{algorithm(), public_key(), public_key_params()} policy_tree()}} |
%%   {error, Reason}
%%
%% Description: Performs a bacis path validation according to RFC 3280.
%%--------------------------------------------------------------------
pkix_path_validation(TrustedCert, CertChain, Options)
  when is_binary(TrustedCert) ->
    {ok, OtpCert} = pkix_decode_cert(TrustedCert, otp),
    pkix_path_validation(OtpCert, CertChain, Options);

pkix_path_validation(#'OTPCertificate'{} = TrustedCert, CertChain, Options) 
  when is_list(CertChain), is_list(Options) ->
    MaxPathDefault = length(CertChain),
    ValidationState = pubkey_cert:init_validation_state(TrustedCert, 
							MaxPathDefault, 
							Options),
    Fun = proplists:get_value(validate_extensions_fun, Options,
			      fun(Extensions, State, _, AccError) ->
				      {Extensions, State, AccError}
			      end),
    Verify = proplists:get_value(verify, Options, true),
    path_validation(CertChain, ValidationState, Fun, Verify).
%%--------------------------------------------------------------------
%% Function: pkix_is_fixed_dh_cert(Cert) -> true | false
%%
%% Description: Checks if a Certificate is a fixed Diffie-Hellman Cert
%%--------------------------------------------------------------------
pkix_is_fixed_dh_cert(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_fixed_dh_cert(OTPCert);
pkix_is_fixed_dh_cert(Cert) when is_binary(Cert) ->
    {ok, OtpCert} = pkix_decode_cert(Cert, otp),
    pkix_is_fixed_dh_cert(OtpCert).

%%--------------------------------------------------------------------
%% Function: pkix_is_self_signed(Cert) -> true | false
%%
%% Description: Checks if a Certificate is self signed. 
%%--------------------------------------------------------------------
pkix_is_self_signed(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_self_signed(OTPCert);
pkix_is_self_signed(Cert) when is_binary(Cert) ->
    {ok, OtpCert} = pkix_decode_cert(Cert, otp),
    pkix_is_self_signed(OtpCert).

%%--------------------------------------------------------------------
%% Function: pkix_issuer_id(Cert) -> {ok, {SerialNr, Issuer}} | {error, Reason}
%%                                     
%%	Cert = asn1_der_encoded() | 'OTPCertificate'{}
%%
%% Description: Returns the issuer id.  
%%--------------------------------------------------------------------
pkix_issuer_id(#'OTPCertificate'{} = OtpCert, self) ->
    pubkey_cert:issuer_id(OtpCert, self);

pkix_issuer_id(#'OTPCertificate'{} = OtpCert, other) ->
    pubkey_cert:issuer_id(OtpCert, other);

pkix_issuer_id(Cert, Signed) when is_binary(Cert) ->
    {ok, OtpCert} = pkix_decode_cert(Cert, otp),
    pkix_issuer_id(OtpCert, Signed).

%%--------------------------------------------------------------------
%% Function: pkix_is_issuer(Cert, IssuerCert) -> true | false
%%
%%	Cert = asn1_der_encoded() | 'OTPCertificate'{}
%%	IssuerCert = asn1_der_encoded() | 'OTPCertificate'{}
%%
%% Description: Checks if <IssuerCert> issued <Cert>.
%%--------------------------------------------------------------------
pkix_is_issuer(Cert, IssuerCert)  when is_binary(Cert) ->
    {ok, OtpCert} = pkix_decode_cert(Cert, otp),
    pkix_is_issuer(OtpCert, IssuerCert);

pkix_is_issuer(Cert, IssuerCert) when is_binary(IssuerCert) ->
    {ok, OtpIssuerCert} = pkix_decode_cert(IssuerCert, otp),
    pkix_is_issuer(Cert, OtpIssuerCert);

pkix_is_issuer(#'OTPCertificate'{tbsCertificate = TBSCert}, 
	       #'OTPCertificate'{tbsCertificate = Candidate}) ->
    pubkey_cert:is_issuer(TBSCert#'OTPTBSCertificate'.issuer,
			  Candidate#'OTPTBSCertificate'.subject).
    
%%--------------------------------------------------------------------
%% Function: pkix_normalize_general_name(Issuer) -> 
%%
%%	Issuer = general_name() - see PKIX
%%   
%% Description: Normalizes a general name so that it can be easily
%%              compared to another genral name. 
%%--------------------------------------------------------------------
pkix_normalize_general_name(Issuer) -> 
    pubkey_cert:normalize_general_name(Issuer).

%%--------------------------------------------------------------------
%% Function:sign(Msg, Key) -> {ok, Signature} 
%%          sign(Msg, Key, KeyParams) -> {ok, Signature}
%%
%%	Msg = binary() | #'TBSCertificate'{}
%%      Key = private_key()
%%	KeyParams = key_params()
%%      Signature = binary()
%%
%% Description: Signs plaintext Msg or #TBSCertificate{}, in the later
%%              case a der encoded "#Certificate{}" will be returned. 
%%--------------------------------------------------------------------
sign(Msg, #'RSAPrivateKey'{} = Key) when is_binary(Msg) -> 
    pubkey_crypto:sign(Msg, Key);

sign(Msg, #'DSAPrivateKey'{} = Key) when is_binary(Msg) ->
    pubkey_crypto:sign(Msg, Key);

sign(#'OTPTBSCertificate'{signature = #'SignatureAlgorithm'{algorithm = Alg} 
			  = SigAlg} = TBSCert, Key) ->
    Msg = pubkey_cert_records:encode_tbs_cert(TBSCert),
    DigestType = pubkey_cert:digest_type(Alg),
    Signature = pubkey_crypto:sign(DigestType, Msg, Key),
    Cert = #'OTPCertificate'{tbsCertificate= TBSCert,
			     signatureAlgorithm = SigAlg,
			     signature = {0, Signature}
			    },
    pkix_encode_cert(Cert).

sign(DigestType, Msg, Key) ->
    pubkey_crypto:sign(DigestType, Msg, Key).

%%--------------------------------------------------------------------
%% Function: verify_signature(PlainText, DigestType, Signature, Key) ->
%%           verify_signature(PlainText, DigestType,
%%                                       Signature, Key, KeyParams) -> 
%%           verify_signature(DerCert, Key, KeyParams) ->
%%
%%      PlainText = binary()
%%      DigestType = md5 | sha
%%      DerCert = asn1_der_encoded()
%%      Signature = binary()
%%      Key = public_key()
%%      KeyParams = key_params()
%%      Verified = boolean()
%%
%% Description: Verifies the signature <Signature>.
%%--------------------------------------------------------------------
verify_signature(PlainText, DigestType, Signature, #'RSAPublicKey'{} = Key)
  when is_binary(PlainText), is_binary(Signature), DigestType == sha;
       DigestType == md5 ->
    pubkey_crypto:verify(DigestType, PlainText, Signature, Key, undefined).

verify_signature(PlainText, DigestType, Signature, #'RSAPublicKey'{} = Key,
		 KeyParams) 
  when is_binary(PlainText), is_binary(Signature), DigestType == sha;
       DigestType == md5 ->
    pubkey_crypto:verify(DigestType, PlainText, Signature, Key, KeyParams);
verify_signature(PlainText, sha, Signature, Key, #'Dss-Parms'{} = KeyParams) 
  when is_binary(PlainText), is_binary(Signature), is_integer(Key) ->
    pubkey_crypto:verify(sha, PlainText, Signature, Key, KeyParams);
verify_signature(Hash, none, Signature, Key, KeyParams) ->
    pubkey_crypto:verify(none, Hash, Signature, Key, KeyParams).

verify_signature(DerCert, Key, #'Dss-Parms'{} = KeyParams) 
  when is_binary(DerCert), is_integer(Key) ->
    pubkey_cert:verify_signature(DerCert, Key, KeyParams);
verify_signature(DerCert,  #'RSAPublicKey'{} = Key, KeyParams) 
  when is_binary(DerCert) ->
    pubkey_cert:verify_signature(DerCert, Key, KeyParams).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
path_validation([], #path_validation_state{working_public_key_algorithm
					   = Algorithm,
					   working_public_key =
					   PublicKey,
					   working_public_key_parameters 
					   = PublicKeyParams,
					   valid_policy_tree = Tree,
					   acc_errors = AccErrors
					  }, _, _) ->
    {ok, {{Algorithm, PublicKey, PublicKeyParams}, Tree, AccErrors}};

path_validation([DerCert | Rest], ValidationState = #path_validation_state{
				    max_path_length = Len}, 
		Fun, Verify) when Len >= 0 ->    
    try validate(DerCert, 
		 ValidationState#path_validation_state{last_cert=Rest=:=[]}, 
		 Fun, Verify) of 
	#path_validation_state{} = NewValidationState ->
	    path_validation(Rest, NewValidationState, Fun, Verify)
    catch   
	throw:Reason ->
	    {error, Reason}
    end;

path_validation(_, _, _, true) ->
    {error, {bad_cert, max_path_length_reached}};

path_validation(_, #path_validation_state{working_public_key_algorithm
					   = Algorithm,
					   working_public_key =
					   PublicKey,
					   working_public_key_parameters 
					   = PublicKeyParams,
					  valid_policy_tree = Tree,
					  acc_errors = AccErrors
					 }, _, false) ->
    {ok, {{Algorithm, PublicKey, PublicKeyParams}, Tree, 
	  [{bad_cert, max_path_length_reached}|AccErrors]}}.

validate(DerCert, #path_validation_state{working_issuer_name = Issuer,
					 working_public_key = Key,
					 working_public_key_parameters = 
					 KeyParams, 
					 permitted_subtrees = Permit,
					 excluded_subtrees = Exclude,
					 last_cert = Last,
					 user_state = UserState0,
					 acc_errors = AccErr0} = 
	 ValidationState0, ValidateExtensionFun, Verify) -> 
    {ok, OtpCert} = pkix_decode_cert(DerCert, otp),
    %% All validate functions will throw {bad_cert, Reason} if they 
    %% fail and Verify = true if Verify = false errors
    %% will be accumulated in the validationstate 
    AccErr1 = pubkey_cert:validate_time(OtpCert, AccErr0, Verify),

    AccErr2 = pubkey_cert:validate_issuer(OtpCert, Issuer, AccErr1, Verify),

    AccErr3 = pubkey_cert:validate_names(OtpCert, Permit, Exclude, Last,
					 AccErr2, Verify),
    AccErr4 = 
	pubkey_cert:validate_revoked_status(OtpCert, Verify, AccErr3),
    
    {ValidationState1, UnknownExtensions0, AccErr5} = 
	pubkey_cert:validate_extensions(OtpCert, ValidationState0, Verify,
					AccErr4),
    %% We want the key_usage extension to be checked before we validate 
    %% the signature. 
    AccErr6 = 
	pubkey_cert:validate_signature(OtpCert, DerCert, Key, KeyParams,
				       AccErr5, Verify),

    {UnknownExtensions, UserState, AccErr7} = 
	ValidateExtensionFun(UnknownExtensions0, UserState0, Verify, AccErr6),
   
    %% Check that all critical extensions have been handled 
    AccErr = 
	pubkey_cert:validate_unknown_extensions(UnknownExtensions, AccErr7, 
						Verify),
    ValidationState  = 
	ValidationState1#path_validation_state{user_state = UserState,
					       acc_errors = AccErr},
    pubkey_cert:prepare_for_next_cert(OtpCert, ValidationState).
