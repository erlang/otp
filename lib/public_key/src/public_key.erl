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
	 pkix_sign/2, pkix_verify/2,	 
	 pkix_is_self_signed/1, 
	 pkix_is_fixed_dh_cert/1,
	 pkix_is_issuer/2,
	 pkix_issuer_id/2,
	 pkix_normalize_name/1,
	 pkix_path_validation/3
	]).

%% Deprecated
-export([decode_private_key/1, decode_private_key/2, pem_to_der/1]).

-deprecated({pem_to_der, 1, next_major_release}).
-deprecated({decode_private_key, 1, next_major_release}).
-deprecated({decode_private_key, 2, next_major_release}).

-type rsa_public_key()       ::  #'RSAPublicKey'{}.
-type rsa_private_key()      ::  #'RSAPrivateKey'{}.
-type dsa_private_key()      ::  #'DSAPrivateKey'{}.
-type dsa_public_key()       :: {integer(), #'Dss-Parms'{}}.
-type rsa_padding()          :: 'rsa_pkcs1_padding' | 'rsa_pkcs1_oaep_padding' 
			      | 'rsa_no_padding'.
-type public_crypt_options() :: [{rsa_pad, rsa_padding()}].
-type rsa_digest_type()      :: 'md5' | 'sha'.
-type dss_digest_type()      :: 'none' | 'sha'.

-define(UINT32(X), X:32/unsigned-big-integer).

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
-spec pem_entry_decode(pem_entry(), [string()]) -> term().
%
%% Description: Decodes a pem entry. pem_decode/1 returns a list of
%% pem entries.
%%--------------------------------------------------------------------
pem_entry_decode({Asn1Type, Der, not_encrypted}) when is_atom(Asn1Type),
						      is_binary(Der) ->
    der_decode(Asn1Type, Der).
pem_entry_decode({Asn1Type, Der, not_encrypted}, _) when is_atom(Asn1Type),
							 is_binary(Der) ->
    der_decode(Asn1Type, Der);
pem_entry_decode({Asn1Type, CryptDer, {Cipher, Salt}} = PemEntry, 
		 Password) when is_atom(Asn1Type),
				is_binary(CryptDer),
				is_list(Cipher),
				is_binary(Salt),
				erlang:byte_size(Salt) == 8
				->
    Der = pubkey_pem:decipher(PemEntry, Password),
    der_decode(Asn1Type, Der).

%%--------------------------------------------------------------------
-spec pem_entry_encode(pki_asn1_type(), term()) -> pem_entry().
-spec pem_entry_encode(pki_asn1_type(), term(),
		       {{Cipher :: string(), Salt :: binary()}, string()}) ->
			      pem_entry().
%
%% Description: Creates a pem entry that can be feed to pem_encode/1.
%%--------------------------------------------------------------------
pem_entry_encode(Asn1Type, Entity)  when is_atom(Asn1Type) ->
    Der = der_encode(Asn1Type, Entity),
    {Asn1Type, Der, not_encrypted}.
pem_entry_encode(Asn1Type, Entity, 
		 {{Cipher, Salt}= CipherInfo, Password}) when is_atom(Asn1Type),
							      is_list(Cipher),
							      is_binary(Salt),
							      erlang:byte_size(Salt) == 8,
							      is_list(Password)->
    Der = der_encode(Asn1Type, Entity),
    DecryptDer = pubkey_pem:cipher(Der, CipherInfo, Password),
    {Asn1Type, DecryptDer, CipherInfo}.

%%--------------------------------------------------------------------
-spec der_decode(asn1_type(), der_encoded()) -> term().
%%
%% Description: Decodes a public key asn1 der encoded entity.
%%--------------------------------------------------------------------
der_decode(Asn1Type, Der) when is_atom(Asn1Type), is_binary(Der) ->
    try 
	{ok, Decoded} = 'OTP-PUB-KEY':decode(Asn1Type, Der),
	Decoded
    catch	    
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end.

%%--------------------------------------------------------------------
-spec der_encode(asn1_type(), term()) -> der_encoded().
%%
%% Description: Encodes a public key entity with asn1 DER encoding.
%%--------------------------------------------------------------------
der_encode(Asn1Type, Entity) when is_atom(Asn1Type) ->
    try 
	{ok, Encoded} = 'OTP-PUB-KEY':encode(Asn1Type, Entity),
	iolist_to_binary(Encoded)
    catch	    
	error:{badmatch, {error, _}} = Error ->
	    erlang:error(Error)
    end.

%%--------------------------------------------------------------------
-spec pkix_decode_cert(der_encoded(), plain | otp) -> 
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
-spec pkix_encode(asn1_type(), term(), otp | plain) -> der_encoded().
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
		#'RSAPrivateKey'{modulus = N,publicExponent = E,
				 privateExponent = D}, 
		Options)  when is_binary(CipherText), 
			       is_list(Options) ->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:rsa_private_decrypt(CipherText, 
			       [crypto:mpint(E), crypto:mpint(N),
				crypto:mpint(D)], Padding).

%%--------------------------------------------------------------------
-spec decrypt_public(CipherText :: binary(), rsa_public_key()) -> 
			    PlainText :: binary().
-spec decrypt_public(CipherText :: binary(), rsa_public_key(), 
		     public_crypt_options()) -> PlainText :: binary().
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
-spec encrypt_public(PlainText :: binary(), rsa_public_key()) ->  
			    CipherText :: binary().
-spec encrypt_public(PlainText :: binary(), rsa_public_key(), 
		     public_crypt_options()) ->  CipherText :: binary().
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

encrypt_private(PlainText, #'RSAPrivateKey'{modulus = N,
					    publicExponent = E, 
					    privateExponent = D}, 
		Options) when is_binary(PlainText), is_list(Options) ->		
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:rsa_private_encrypt(PlainText, [crypto:mpint(E), 
					   crypto:mpint(N), 
					   crypto:mpint(D)], Padding).

%%--------------------------------------------------------------------
-spec sign(PlainTextOrDigest :: binary(), rsa_digest_type() | dss_digest_type(), 
	   rsa_private_key() | 
	   dsa_private_key()) -> Signature :: binary().
%%
%% Description: Create digital signature.
%%--------------------------------------------------------------------
sign(PlainText, DigestType,  #'RSAPrivateKey'{modulus = N,  publicExponent = E,
					      privateExponent = D}) 
  when is_binary(PlainText),
       DigestType == md5;
       DigestType == sha ->
    
    crypto:rsa_sign(DigestType, sized_binary(PlainText), [crypto:mpint(E),
							  crypto:mpint(N),
							  crypto:mpint(D)]);

sign(Digest, none, #'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) 
  when is_binary(Digest)->
    crypto:dss_sign(none, Digest, 
		    [crypto:mpint(P), crypto:mpint(Q), 
		     crypto:mpint(G), crypto:mpint(X)]);
  
sign(PlainText, sha, #'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) 
  when is_binary(PlainText) ->
    crypto:dss_sign(sized_binary(PlainText), 
		    [crypto:mpint(P), crypto:mpint(Q), 
		     crypto:mpint(G), crypto:mpint(X)]).

%%--------------------------------------------------------------------
-spec verify(PlainTextOrDigest :: binary(), rsa_digest_type() | dss_digest_type(), 
	     Signature :: binary(), rsa_public_key() 
	     | dsa_public_key()) -> boolean().
%%
%% Description: Verifies a digital signature.
%%--------------------------------------------------------------------
verify(PlainText, DigestType, Signature, 
       #'RSAPublicKey'{modulus = Mod, publicExponent = Exp}) 
  when is_binary (PlainText), DigestType == sha; DigestType == md5 ->
    crypto:rsa_verify(DigestType,
		      sized_binary(PlainText), 
		      sized_binary(Signature), 
		      [crypto:mpint(Exp), crypto:mpint(Mod)]);

verify(Digest, none, Signature, {Key,  #'Dss-Parms'{p = P, q = Q, g = G}}) 
  when is_integer(Key),  is_binary(Digest), is_binary(Signature) ->
    crypto:dss_verify(none, 
		      Digest, 
		      sized_binary(Signature), 
		      [crypto:mpint(P), crypto:mpint(Q), 
		       crypto:mpint(G), crypto:mpint(Key)]);
    
verify(PlainText, sha, Signature, {Key,  #'Dss-Parms'{p = P, q = Q, g = G}}) 
  when is_integer(Key),  is_binary(PlainText), is_binary(Signature) ->
    crypto:dss_verify(sized_binary(PlainText), 
		      sized_binary(Signature), 
		      [crypto:mpint(P), crypto:mpint(Q), 
		       crypto:mpint(G), crypto:mpint(Key)]).
%%--------------------------------------------------------------------
-spec pkix_sign(#'OTPTBSCertificate'{},
		rsa_private_key() | dsa_private_key()) -> der_encoded().
%%
%% Description: Sign a pkix x.509 certificate. Returns the corresponding
%% der encoded 'Certificate'{}
%%--------------------------------------------------------------------
pkix_sign(#'OTPTBSCertificate'{signature = 
				   #'SignatureAlgorithm'{algorithm = Alg} 
			       = SigAlg} = TBSCert, Key) ->

    Msg = pkix_encode('OTPTBSCertificate', TBSCert, otp),    
    DigestType = pubkey_cert:digest_type(Alg),
    Signature = sign(Msg, DigestType, Key),
    Cert = #'OTPCertificate'{tbsCertificate= TBSCert,
			     signatureAlgorithm = SigAlg,
			     signature = {0, Signature}
			    },
    pkix_encode('OTPCertificate', Cert, otp).

%%--------------------------------------------------------------------
-spec pkix_verify(der_encoded(), rsa_public_key()| 
		  dsa_public_key()) -> boolean().
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
    verify(PlainText, DigestType, Signature, RSAKey).

%%--------------------------------------------------------------------
-spec pkix_is_issuer(Cert :: der_encoded()| #'OTPCertificate'{},
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
			  Candidate#'OTPTBSCertificate'.subject).

%%--------------------------------------------------------------------
-spec pkix_is_self_signed(der_encoded()| #'OTPCertificate'{}) -> boolean().
%%
%% Description: Checks if a Certificate is self signed. 
%%--------------------------------------------------------------------
pkix_is_self_signed(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_self_signed(OTPCert);
pkix_is_self_signed(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_self_signed(OtpCert).
  
%%--------------------------------------------------------------------
-spec pkix_is_fixed_dh_cert(der_encoded()| #'OTPCertificate'{}) -> boolean().
%%
%% Description: Checks if a Certificate is a fixed Diffie-Hellman Cert.
%%--------------------------------------------------------------------
pkix_is_fixed_dh_cert(#'OTPCertificate'{} = OTPCert) ->
    pubkey_cert:is_fixed_dh_cert(OTPCert);
pkix_is_fixed_dh_cert(Cert) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_is_fixed_dh_cert(OtpCert).

%%--------------------------------------------------------------------
-spec pkix_issuer_id(der_encoded()| #'OTPCertificate'{}, 
		     IssuedBy :: self | other) -> 
			    {ok, {SerialNr :: integer(), 
				  Issuer :: {rdnSequence, 
					     [#'AttributeTypeAndValue'{}]}}} 
				| {error, Reason :: term()}.
%
%% Description: Returns the issuer id.  
%%--------------------------------------------------------------------
pkix_issuer_id(#'OTPCertificate'{} = OtpCert, self) ->
    pubkey_cert:issuer_id(OtpCert, self);

pkix_issuer_id(#'OTPCertificate'{} = OtpCert, other) ->
    pubkey_cert:issuer_id(OtpCert, other);

pkix_issuer_id(Cert, Signed) when is_binary(Cert) ->
    OtpCert = pkix_decode_cert(Cert, otp),
    pkix_issuer_id(OtpCert, Signed).

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
-spec pkix_path_validation(der_encoded()| #'OTPCertificate'{} | unknown_ca,
			   CertChain :: [der_encoded()] , 
			   Options :: list()) ->  
				  {ok, {PublicKeyInfo :: term(), 
					PolicyTree :: term()}} |
				  {error, {bad_cert, Reason :: term()}}.
%% Description: Performs a basic path validation according to RFC 5280.
%%--------------------------------------------------------------------
pkix_path_validation(unknown_ca, [Cert | Chain], Options0) ->
    {VerifyFun, Userstat0} =
	proplists:get_value(verify_fun, Options0, ?DEFAULT_VERIFYFUN),
    Otpcert = pkix_decode_cert(Cert, otp),
    Reason = {bad_cert, unknown_ca},
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
pkix_path_validation(TrustedCert, CertChain, Options) when
  is_binary(TrustedCert) -> OtpCert = pkix_decode_cert(TrustedCert,
  otp), pkix_path_validation(OtpCert, CertChain, Options);

pkix_path_validation(#'OTPCertificate'{} = TrustedCert, CertChain, Options) 
  when is_list(CertChain), is_list(Options) ->
    MaxPathDefault = length(CertChain),
    ValidationState = pubkey_cert:init_validation_state(TrustedCert, 
							MaxPathDefault, 
							Options),
    path_validation(CertChain, ValidationState).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

encrypt_public(PlainText, N, E, Options)->
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:rsa_public_encrypt(PlainText, [crypto:mpint(E),crypto:mpint(N)],
			      Padding).

decrypt_public(CipherText, N,E, Options) ->  
    Padding = proplists:get_value(rsa_pad, Options, rsa_pkcs1_padding),
    crypto:rsa_public_decrypt(CipherText,[crypto:mpint(E), crypto:mpint(N)], 
			      Padding).


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

path_validation([DerCert | _] = Path,
		#path_validation_state{user_state = UserState0,
				       verify_fun = VerifyFun} =
		    ValidationState) ->
    Reason = {bad_cert, max_path_length_reached},
    OtpCert = pkix_decode_cert(DerCert, otp),
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


validate(DerCert, #path_validation_state{working_issuer_name = Issuer,
					 working_public_key = Key,
					 working_public_key_parameters = 
					 KeyParams, 
					 permitted_subtrees = Permit,
					 excluded_subtrees = Exclude,
					 last_cert = Last,
					 user_state = UserState0,
					 verify_fun = VerifyFun} =
	     ValidationState0) ->

    OtpCert = pkix_decode_cert(DerCert, otp),

    UserState1 = pubkey_cert:validate_time(OtpCert, UserState0, VerifyFun),

    UserState2 = pubkey_cert:validate_issuer(OtpCert, Issuer, UserState1, VerifyFun),

    UserState3 = pubkey_cert:validate_names(OtpCert, Permit, Exclude, Last,
					    UserState2,VerifyFun),

    UserState4 = pubkey_cert:validate_revoked_status(OtpCert, UserState3, VerifyFun),
    
    {ValidationState1, UserState5} =
	pubkey_cert:validate_extensions(OtpCert, ValidationState0, UserState4,
					VerifyFun),

    %% We want the key_usage extension to be checked before we validate
    %% the signature. 
    UserState0 = pubkey_cert:validate_signature(OtpCert, DerCert,
						Key, KeyParams, UserState5, VerifyFun),
    UserState = pubkey_cert:verify_fun(OtpCert, valid, UserState0, VerifyFun),
    ValidationState  = 
	ValidationState1#path_validation_state{user_state = UserState},

    pubkey_cert:prepare_for_next_cert(OtpCert, ValidationState).

sized_binary(Binary) when is_binary(Binary) ->
    Size = size(Binary),
    <<?UINT32(Size), Binary/binary>>;
sized_binary(List) ->
    sized_binary(list_to_binary(List)).

%%--------------------------------------------------------------------
%%% Deprecated functions
%%--------------------------------------------------------------------
pem_to_der(CertSource) ->
    {ok, Bin} = file:read_file(CertSource),
    pubkey_pem:decode(Bin).

decode_private_key(KeyInfo) ->
    decode_private_key(KeyInfo, no_passwd).

decode_private_key(KeyInfo = {'RSAPrivateKey', _, _}, Password) ->
    DerEncoded = pubkey_pem:decode_key(KeyInfo, Password),
    'OTP-PUB-KEY':decode('RSAPrivateKey', DerEncoded);
decode_private_key(KeyInfo = {'DSAPrivateKey', _, _}, Password) ->
    DerEncoded = pubkey_pem:decode_key(KeyInfo, Password),
    'OTP-PUB-KEY':decode('DSAPrivateKey', DerEncoded).
