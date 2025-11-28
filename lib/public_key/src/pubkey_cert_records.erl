%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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

-module(pubkey_cert_records).
-moduledoc false.

-include("public_key_internal.hrl").

-export([decode_cert/1, decode_cert/2,
         transform/2,
         supportedPublicKeyAlgorithms/1,
	 supportedCurvesTypes/1,
         namedCurves/1,
         encode_extensions/1,
         decode_extensions/1,
         ext_oid/1,
         oid_to_ml_dsa_algo/1,
         oid_to_slh_dsa_algo/1,
         mldsa_algo_to_oid/1,
         slh_dsa_algo_to_oid/1
        ]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec decode_cert(DerCert::binary()) -> {ok, #'OTPCertificate'{}}.
%%
%% Description: Recursively decodes a Certificate. 
%%-------------------------------------------------------------------- 
decode_cert(DerCert) ->
    decode_cert(DerCert, otp).

%%--------------------------------------------------------------------
-spec decode_cert(DerCert :: binary(), Type :: 'otp' | 'relaxed') -> {ok, #'OTPCertificate'{}}.
%%
%% Description: Recursively decodes a Certificate with given decoder Type. 
%%-------------------------------------------------------------------- 
decode_cert(DerCert, otp) ->
    decode_cert1(DerCert, 'OTP-PKIX');
decode_cert(DerCert, relaxed) ->
    decode_cert1(DerCert, 'OTP-PKIX-Relaxed').

decode_cert1(DerCert, Mod) ->
    {ok, Cert0} = Mod:decode('OTPCertificate', DerCert),
    Cert = dec_transform(Cert0),
    {ok, Cert}.

%%--------------------------------------------------------------------
-spec transform(term(), encode | decode) ->term().
%%
%% Description: Transforms between encoded and decode otp formatted
%% certificate parts.
%%
%% Note that this function operates on raw data that has not gone
%% through the pubkey_translation module. Thus does the same
%% backwards compatibility translation done in pubkey_translation.
%%--------------------------------------------------------------------

transform(Term, encode) -> enc_transform(Term);
transform(Term, decode) -> dec_transform(Term).

enc_transform(#'OTPCertificate'{tbsCertificate = TBS, signatureAlgorithm=SA} = Cert) ->
    Cert#'OTPCertificate'{tbsCertificate=enc_transform(TBS),
                          signatureAlgorithm=enc_transform(SA)};
enc_transform(#'OTPTBSCertificate'{signature=Signature0,
                                   issuer=Issuer0,
                                   subject=Subject0,
                                   subjectPublicKeyInfo=Spki0,
                                   extensions=Exts0}=TBS) ->
    Signature = enc_transform(Signature0),
    Issuer = enc_transform(Issuer0),
    Subject = enc_transform(Subject0),
    Spki = encode_supportedPublicKey(Spki0),
    Exts = encode_extensions(Exts0),
    TBS#'OTPTBSCertificate'{signature = Signature,
                            issuer=Issuer,
                            subject=Subject,
                            subjectPublicKeyInfo=Spki,
                            extensions=Exts};
enc_transform(#'SignatureAlgorithm'{algorithm=Algo,parameters=Params}) ->
    #'OTPTBSCertificate_signature'{algorithm=Algo,parameters=enc_transform(Params)};
enc_transform({params, #'Dss-Parms'{p=P,q=Q,g=G}}) ->
    {present,#'DSA-Params'{p=P,q=Q,g=G}};
enc_transform(#'AttributeTypeAndValue'{type=Id, value=Value0}) ->
    case Id of
        ?'id-at-countryName' ->
            #'SingleAttribute'{type=Id, value={correct, Value0}};
        ?'id-emailAddress' ->
            #'SingleAttribute'{type=Id, value={correct, Value0}};
        _ ->
            #'SingleAttribute'{type=Id,value=Value0}
    end;
enc_transform(#'AuthorityKeyIdentifier'{authorityCertIssuer=ACI}=AKI) ->
    AKI#'AuthorityKeyIdentifier'{authorityCertIssuer=enc_transform(ACI)};
enc_transform([{directoryName, _}]=List) ->
    [{directoryName, enc_transform(Value)} || {directoryName, Value} <- List];
enc_transform({directoryName, Value}) ->
    {directoryName, enc_transform(Value)};
enc_transform({rdnSequence, SeqList}) when is_list(SeqList) ->
    {rdnSequence,
     lists:map(fun(Seq) ->
		       lists:map(fun(Element) -> enc_transform(Element) end, Seq)
	       end, SeqList)};
enc_transform(#'NameConstraints'{permittedSubtrees=Permitted, excludedSubtrees=Excluded}) ->
    #'NameConstraints'{permittedSubtrees=enc_transform_sub_tree(Permitted),
		       excludedSubtrees=enc_transform_sub_tree(Excluded)};
enc_transform(Other) ->
    Other.

dec_transform(#'OTPCertificate'{tbsCertificate = TBS, signatureAlgorithm=SA}=Cert) ->
    Cert#'OTPCertificate'{tbsCertificate=dec_transform(TBS),
                          signatureAlgorithm=dec_transform(SA)};
dec_transform(#'OTPCertificate_signatureAlgorithm'{algorithm=Algo,parameters=Params}) ->
    #'SignatureAlgorithm'{algorithm=Algo,parameters=dec_transform(Params)};
dec_transform(#'OTPTBSCertificate'{signature=Signature0,
                                   issuer=Issuer0,
                                   subject=Subject0,
                                   subjectPublicKeyInfo=Spki0,
                                   extensions=Exts0}=TBS) ->
    Signature = dec_transform(Signature0),
    Issuer  = dec_transform(Issuer0),
    Subject = dec_transform(Subject0),
    Spki = decode_supportedPublicKey(Spki0),
    Exts = decode_extensions(Exts0, crl_later),
    TBS#'OTPTBSCertificate'{issuer=Issuer, subject=Subject,
                            signature=setelement(1, Signature, 'SignatureAlgorithm'),
			    subjectPublicKeyInfo=Spki,extensions=Exts};
dec_transform(#'OTPTBSCertificate_signature'{algorithm=Algo,parameters=Params}) ->
    #'SignatureAlgorithm'{algorithm=Algo,parameters=dec_transform(Params)};
dec_transform({present,#'DSA-Params'{p=P,q=Q,g=G}}) ->
    {params, #'Dss-Parms'{p=P,q=Q,g=G}};
dec_transform({absent,'NULL'}) ->
    'NULL';
dec_transform(#'SingleAttribute'{type=Id,value=Value0}) ->
    case {Id, Value0} of
        {?'id-at-countryName', {_,String}} ->
            #'AttributeTypeAndValue'{type=Id, value=String};
        {?'id-emailAddress', {_,String}} ->
            #'AttributeTypeAndValue'{type=Id, value=String};
        {_, _} ->
            #'AttributeTypeAndValue'{type=Id, value=Value0}
    end;
dec_transform(#'AuthorityKeyIdentifier'{authorityCertIssuer=ACI}=AKI) ->
    AKI#'AuthorityKeyIdentifier'{authorityCertIssuer=dec_transform(ACI)};
dec_transform([{directoryName, _}]=List) ->
    [{directoryName, dec_transform(Value)} || {directoryName, Value} <- List];
dec_transform({directoryName, Value}) ->
    {directoryName, dec_transform(Value)};
dec_transform({rdnSequence, SeqList}) when is_list(SeqList) ->
    {rdnSequence,
     lists:map(fun(Seq) ->
		       lists:map(fun(Element) -> dec_transform(Element) end, Seq)
	       end, SeqList)};
dec_transform(#'NameConstraints'{permittedSubtrees=Permitted, excludedSubtrees=Excluded}) ->
    #'NameConstraints'{permittedSubtrees=dec_transform_sub_tree(Permitted),
		       excludedSubtrees=dec_transform_sub_tree(Excluded)};
dec_transform(Other) ->
    Other.


enc_transform_sub_tree(asn1_NOVALUE) ->
    asn1_NOVALUE;
enc_transform_sub_tree(TreeList) ->
    [Tree#'GeneralSubtree'{base=enc_transform(Name)} ||
	#'GeneralSubtree'{base=Name}=Tree <- TreeList].

dec_transform_sub_tree(asn1_NOVALUE) ->
    asn1_NOVALUE;
dec_transform_sub_tree(TreeList) ->
    [Tree#'GeneralSubtree'{base=dec_transform(Name)} ||
	#'GeneralSubtree'{base=Name}=Tree <- TreeList].

%%--------------------------------------------------------------------
-spec supportedPublicKeyAlgorithms(Oid::tuple()) -> public_key:asn1_type().
%%
%% Description: Returns the public key type for an algorithm
%% identifier tuple as found in SubjectPublicKeyInfo.
%%
%%--------------------------------------------------------------------
supportedPublicKeyAlgorithms(?'rsaEncryption') -> 'RSAPublicKey';
supportedPublicKeyAlgorithms(?'id-dsa') -> 'DSAPublicKey';
supportedPublicKeyAlgorithms(?'dhpublicnumber') -> 'DHPublicKey';
supportedPublicKeyAlgorithms(?'id-keyExchangeAlgorithm') -> 'KEA-PublicKey';
supportedPublicKeyAlgorithms(?'id-ecPublicKey') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-RSASSA-PSS') -> 'RSAPublicKey';
supportedPublicKeyAlgorithms(?'id-Ed25519') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-Ed448') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-X25519') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-X448') -> 'ECPoint';
supportedPublicKeyAlgorithms(?'id-ml-dsa-44') -> 'ML-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-ml-dsa-65') -> 'ML-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-ml-dsa-87') -> 'ML-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-sha2-128f') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-sha2-128s') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-sha2-192f') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-sha2-192s') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-sha2-256f') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-sha2-256s') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-shake-128f') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-shake-128s') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-shake-192f') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-shake-192s') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-shake-256f') -> 'SLH-DSAPublicKey';
supportedPublicKeyAlgorithms(?'id-slh-dsa-shake-256s') -> 'SLH-DSAPublicKey'.

supportedCurvesTypes(?'characteristic-two-field') -> characteristic_two_field;
supportedCurvesTypes(?'prime-field') -> prime_field;
supportedCurvesTypes(?'id-edwards-curve-algs') -> edwards_curve.

namedCurves(?'sect571r1') -> sect571r1;
namedCurves(?'sect571k1') -> sect571k1;
namedCurves(?'sect409r1') -> sect409r1;
namedCurves(?'sect409k1') -> sect409k1;
namedCurves(?'secp521r1') -> secp521r1;
namedCurves(?'secp384r1') -> secp384r1;
namedCurves(?'secp224r1') -> secp224r1;
namedCurves(?'secp224k1') -> secp224k1;
namedCurves(?'secp192k1') -> secp192k1;
namedCurves(?'secp160r2') -> secp160r2;
namedCurves(?'secp128r2') -> secp128r2;
namedCurves(?'secp128r1') -> secp128r1;
namedCurves(?'sect233r1') -> sect233r1;
namedCurves(?'sect233k1') -> sect233k1;
namedCurves(?'sect193r2') -> sect193r2;
namedCurves(?'sect193r1') -> sect193r1;
namedCurves(?'sect131r2') -> sect131r2;
namedCurves(?'sect131r1') -> sect131r1;
namedCurves(?'sect283r1') -> sect283r1;
namedCurves(?'sect283k1') -> sect283k1;
namedCurves(?'sect163r2') -> sect163r2;
namedCurves(?'secp256k1') -> secp256k1;
namedCurves(?'secp160k1') -> secp160k1;
namedCurves(?'secp160r1') -> secp160r1;
namedCurves(?'secp112r2') -> secp112r2;
namedCurves(?'secp112r1') -> secp112r1;
namedCurves(?'sect113r2') -> sect113r2;
namedCurves(?'sect113r1') -> sect113r1;
namedCurves(?'sect239k1') -> sect239k1;
namedCurves(?'sect163r1') -> sect163r1;
namedCurves(?'sect163k1') -> sect163k1;
namedCurves(?'secp256r1') -> secp256r1;
namedCurves(?'secp192r1') -> secp192r1;
namedCurves(?'id-X25519') -> x25519;
namedCurves(?'id-X448') -> x448;
namedCurves(?'id-Ed25519') -> ed25519;
namedCurves(?'id-Ed448') -> ed448;
namedCurves(?'brainpoolP160r1') -> brainpoolP160r1;
namedCurves(?'brainpoolP160t1') -> brainpoolP160t1;
namedCurves(?'brainpoolP192r1') -> brainpoolP192r1;
namedCurves(?'brainpoolP192t1') -> brainpoolP192t1;
namedCurves(?'brainpoolP224r1') -> brainpoolP224r1;
namedCurves(?'brainpoolP224t1') -> brainpoolP224t1;
namedCurves(?'brainpoolP256r1') -> brainpoolP256r1;
namedCurves(?'brainpoolP256t1') -> brainpoolP256t1;
namedCurves(?'brainpoolP320r1') -> brainpoolP320r1;
namedCurves(?'brainpoolP320t1') -> brainpoolP320t1;
namedCurves(?'brainpoolP384r1') -> brainpoolP384r1;
namedCurves(?'brainpoolP384t1') -> brainpoolP384t1;
namedCurves(?'brainpoolP512r1') -> brainpoolP512r1;
namedCurves(?'brainpoolP512t1') -> brainpoolP512t1;
namedCurves(sect571r1) -> ?'sect571r1';
namedCurves(sect571k1) -> ?'sect571k1';
namedCurves(sect409r1) -> ?'sect409r1';
namedCurves(sect409k1) -> ?'sect409k1';
namedCurves(secp521r1) -> ?'secp521r1';
namedCurves(secp384r1) -> ?'secp384r1';
namedCurves(secp224r1) -> ?'secp224r1';
namedCurves(secp224k1) -> ?'secp224k1';
namedCurves(secp192k1) -> ?'secp192k1';
namedCurves(secp160r2) -> ?'secp160r2';
namedCurves(secp128r2) -> ?'secp128r2';
namedCurves(secp128r1) -> ?'secp128r1';
namedCurves(sect233r1) -> ?'sect233r1';
namedCurves(sect233k1) -> ?'sect233k1';
namedCurves(sect193r2) -> ?'sect193r2';
namedCurves(sect193r1) -> ?'sect193r1';
namedCurves(sect131r2) -> ?'sect131r2';
namedCurves(sect131r1) -> ?'sect131r1';
namedCurves(sect283r1) -> ?'sect283r1';
namedCurves(sect283k1) -> ?'sect283k1';
namedCurves(sect163r2) -> ?'sect163r2';
namedCurves(secp256k1) -> ?'secp256k1';
namedCurves(secp160k1) -> ?'secp160k1';
namedCurves(secp160r1) -> ?'secp160r1';
namedCurves(secp112r2) -> ?'secp112r2';
namedCurves(secp112r1) -> ?'secp112r1';
namedCurves(sect113r2) -> ?'sect113r2';
namedCurves(sect113r1) -> ?'sect113r1';
namedCurves(sect239k1) -> ?'sect239k1';
namedCurves(sect163r1) -> ?'sect163r1';
namedCurves(sect163k1) -> ?'sect163k1';
namedCurves(secp256r1) -> ?'secp256r1';
namedCurves(secp192r1) -> ?'secp192r1';
namedCurves(x25519)    -> ?'id-X25519';
namedCurves(x448)      -> ?'id-X448';
namedCurves(ed25519)    -> ?'id-Ed25519';
namedCurves(ed448)      -> ?'id-Ed448';
namedCurves(brainpoolP160r1) -> ?'brainpoolP160r1';
namedCurves(brainpoolP160t1) -> ?'brainpoolP160t1';
namedCurves(brainpoolP192r1) -> ?'brainpoolP192r1';
namedCurves(brainpoolP192t1) -> ?'brainpoolP192t1';
namedCurves(brainpoolP224r1) -> ?'brainpoolP224r1';
namedCurves(brainpoolP224t1) -> ?'brainpoolP224t1';
namedCurves(brainpoolP256r1) -> ?'brainpoolP256r1';
namedCurves(brainpoolP256t1) -> ?'brainpoolP256t1';
namedCurves(brainpoolP320r1) -> ?'brainpoolP320r1';
namedCurves(brainpoolP320t1) -> ?'brainpoolP320t1';
namedCurves(brainpoolP384r1) -> ?'brainpoolP384r1';
namedCurves(brainpoolP384t1) -> ?'brainpoolP384t1';
namedCurves(brainpoolP512r1) -> ?'brainpoolP512r1';
namedCurves(brainpoolP512t1) -> ?'brainpoolP512t1'.


oid_to_ml_dsa_algo(?'id-ml-dsa-44') ->
    mldsa44;
oid_to_ml_dsa_algo(?'id-ml-dsa-65') ->
    mldsa65;
oid_to_ml_dsa_algo(?'id-ml-dsa-87') ->
    mldsa87.

oid_to_slh_dsa_algo(?'id-slh-dsa-sha2-128s') ->
    slh_dsa_sha2_128s;
oid_to_slh_dsa_algo(?'id-slh-dsa-sha2-128f') ->
    slh_dsa_sha2_128f;
oid_to_slh_dsa_algo(?'id-slh-dsa-sha2-192s') ->
    slh_dsa_sha2_192s;
oid_to_slh_dsa_algo(?'id-slh-dsa-sha2-192f') ->
    slh_dsa_sha2_192f;
oid_to_slh_dsa_algo(?'id-slh-dsa-sha2-256s') ->
    slh_dsa_sha2_256s;
oid_to_slh_dsa_algo(?'id-slh-dsa-sha2-256f') ->
    slh_dsa_sha2_256f;
oid_to_slh_dsa_algo(?'id-slh-dsa-shake-128s') ->
    slh_dsa_shake_128s;
oid_to_slh_dsa_algo(?'id-slh-dsa-shake-128f') ->
    slh_dsa_shake_128f;
oid_to_slh_dsa_algo(?'id-slh-dsa-shake-192s') ->
    slh_dsa_shake_192s;
oid_to_slh_dsa_algo(?'id-slh-dsa-shake-192f') ->
    slh_dsa_shake_192f;
oid_to_slh_dsa_algo(?'id-slh-dsa-shake-256s') ->
    slh_dsa_shake_256s;
oid_to_slh_dsa_algo(?'id-slh-dsa-shake-256f') ->
    slh_dsa_shake_256f.

mldsa_algo_to_oid(mldsa44) ->
    ?'id-ml-dsa-44';
mldsa_algo_to_oid(mldsa65) ->
    ?'id-ml-dsa-65';
mldsa_algo_to_oid(mldsa87) ->
    ?'id-ml-dsa-87'.

slh_dsa_algo_to_oid(slh_dsa_sha2_128s) ->
    ?'id-slh-dsa-sha2-128s';
slh_dsa_algo_to_oid(slh_dsa_sha2_128f) ->
    ?'id-slh-dsa-sha2-128f';
slh_dsa_algo_to_oid(slh_dsa_sha2_192s) ->
    ?'id-slh-dsa-sha2-192s';
slh_dsa_algo_to_oid(slh_dsa_sha2_192f) ->
    ?'id-slh-dsa-sha2-192f';
slh_dsa_algo_to_oid(slh_dsa_sha2_256s) ->
    ?'id-slh-dsa-sha2-256s';
slh_dsa_algo_to_oid(slh_dsa_sha2_256f) ->
    ?'id-slh-dsa-sha2-256f';
slh_dsa_algo_to_oid(slh_dsa_shake_128s) ->
    ?'id-slh-dsa-shake-128s';
slh_dsa_algo_to_oid(slh_dsa_shake_128f) ->
    ?'id-slh-dsa-shake-128f';
slh_dsa_algo_to_oid(slh_dsa_shake_192s) ->
    ?'id-slh-dsa-shake-192s';
slh_dsa_algo_to_oid(slh_dsa_shake_192f) ->
    ?'id-slh-dsa-shake-192f';
slh_dsa_algo_to_oid(slh_dsa_shake_256s) ->
    ?'id-slh-dsa-shake-256s';
slh_dsa_algo_to_oid(slh_dsa_shake_256f) ->
    ?'id-slh-dsa-shake-256f'.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%% SubjectPublicKey
decode_supportedPublicKey(#'SubjectPublicKeyInfo'{algorithm=PA,
                                                  subjectPublicKey=SPK0}) ->
    #'SubjectPublicKeyInfo_algorithm'{algorithm=Algo,parameters=Params0} = PA,
    Type = supportedPublicKeyAlgorithms(Algo),
    SPK = case Type of
              'ECPoint' ->
                  #'ECPoint'{point = SPK0};
              'ML-DSAPublicKey' ->
                  #'ML-DSAPublicKey'{algorithm = oid_to_ml_dsa_algo(Algo),
                                     key = SPK0};
              'SLH-DSAPublicKey' ->
                  #'SLH-DSAPublicKey'{algorithm = oid_to_slh_dsa_algo(Algo),
                                      key = SPK0};
              _ ->
                  public_key:der_decode(Type, SPK0)
          end,
    Params = case Params0 of
                 #'DSA-Params'{p=P,q=Q,g=G} -> {params, #'Dss-Parms'{p=P,q=Q,g=G}};
                 _ -> Params0
             end,
    #'OTPSubjectPublicKeyInfo'{subjectPublicKey = SPK,
                               algorithm=#'PublicKeyAlgorithm'{algorithm=Algo,
                                                               parameters=Params}}.
encode_supportedPublicKey(#'OTPSubjectPublicKeyInfo'{
                             algorithm =
                                 #'PublicKeyAlgorithm'{algorithm=Algo,parameters = Params0},
                             subjectPublicKey = SPK0}) ->
    Type = supportedPublicKeyAlgorithms(Algo),
    SPK = case Type of
              'ECPoint' ->
                  SPK0#'ECPoint'.point;
              'ML-DSAPublicKey' ->
                  #'ML-DSAPublicKey'{key = SPK1} = SPK0,
                   SPK1;
              'SLH-DSAPublicKey' ->
                  #'SLH-DSAPublicKey'{key = SPK1} = SPK0,
                  SPK1;
              _ ->
                  public_key:der_encode(Type, SPK0)
          end,
    Params = case Params0 of
                 {params, #'Dss-Parms'{p=P,q=Q,g=G}} -> #'DSA-Params'{p=P,q=Q,g=G};
                 _ -> Params0
             end,
    PA = #'SubjectPublicKeyInfo_algorithm'{algorithm=Algo,parameters=Params},
    #'SubjectPublicKeyInfo'{subjectPublicKey = SPK, algorithm=PA}.

%%% Extensions

extension_id(?'id-ce-authorityKeyIdentifier') ->
    {'PKIX1Implicit-2009', getdec_CrlExtensions, getenc_CrlExtensions, 'AuthorityKeyIdentifier'};
extension_id(?'id-ce-subjectKeyIdentifier') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'SubjectKeyIdentifier'};
extension_id(?'id-ce-keyUsage') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'KeyUsage'};
extension_id(?'id-ce-privateKeyUsagePeriod') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'PrivateKeyUsagePeriod'};
extension_id(?'id-ce-certificatePolicies') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'CertificatePolicies'};
extension_id(?'id-ce-policyMappings') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'PolicyMappings'};
extension_id(?'id-ce-subjectAltName') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'SubjectAltName'};
extension_id(?'id-ce-issuerAltName') ->
    {'PKIX1Implicit-2009', getdec_CrlExtensions, getenc_CrlExtensions, 'IssuerAltName'};
extension_id(?'id-ce-subjectDirectoryAttributes') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'SubjectDirectoryAttributes'};
extension_id(?'id-ce-basicConstraints' ) ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'BasicConstraints'};
extension_id(?'id-ce-nameConstraints') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'NameConstraints'};
extension_id(?'id-ce-policyConstraints') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'PolicyConstraints'};
extension_id(?'id-ce-extKeyUsage') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'ExtKeyUsageSyntax'};
extension_id(?'id-ce-inhibitAnyPolicy') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'InhibitAnyPolicy'};
extension_id(?'id-ce-freshestCRL') ->
    {'PKIX1Implicit-2009', getdec_CrlExtensions, getenc_CrlExtensions, 'FreshestCRL'};
extension_id(?'id-ce-issuingDistributionPoint') ->
    {'PKIX1Implicit-2009', getdec_CrlExtensions, getenc_CrlExtensions, 'IssuingDistributionPoint'};
extension_id(?'id-pe-authorityInfoAccess') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'AuthorityInfoAccessSyntax'};
extension_id(?'id-pe-subjectInfoAccess') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'SubjectInfoAccessSyntax'};
extension_id(?'id-ce-cRLNumber') ->
    {'PKIX1Implicit-2009', getdec_CrlExtensions, getenc_CrlExtensions, 'CRLNumber'};
extension_id(?'id-ce-deltaCRLIndicator') ->
    {'PKIX1Implicit-2009', getdec_CrlExtensions, getenc_CrlExtensions, 'BaseCRLNumber'};
extension_id(?'id-ce-cRLReasons') ->
    {'PKIX1Implicit-2009', getdec_CrlEntryExtensions, getenc_CrlEntryExtensions, 'CRLReason'};
extension_id(?'id-ce-certificateIssuer') ->
    {'PKIX1Implicit-2009', getdec_CrlEntryExtensions, getenc_CrlEntryExtensions, 'CertificateIssuer'};
extension_id(?'id-ce-holdInstructionCode') ->
    {'PKIX1Implicit-2009', getdec_CrlEntryExtensions, getenc_CrlEntryExtensions, 'HoldInstructionCode'};
extension_id(?'id-ce-invalidityDate') ->
    {'PKIX1Implicit-2009', getdec_CrlEntryExtensions, getenc_CrlEntryExtensions, 'InvalidityDate'};
extension_id(?'id-ce-cRLDistributionPoints') ->
    {'PKIX1Implicit-2009', getdec_CertExtensions, getenc_CertExtensions, 'CRLDistributionPoints'};
extension_id(_) ->
    {undefined, undefined, undefined, undefined}.

ext_oid('AuthorityKeyIdentifier') ->     ?'id-ce-authorityKeyIdentifier';
ext_oid('SubjectKeyIdentifier') ->       ?'id-ce-subjectKeyIdentifier';
ext_oid('KeyUsage') ->                   ?'id-ce-keyUsage';
ext_oid('PrivateKeyUsagePeriod') ->      ?'id-ce-privateKeyUsagePeriod';
ext_oid('CertificatePolicies') ->        ?'id-ce-certificatePolicies';
ext_oid('PolicyMappings') -> 	         ?'id-ce-policyMappings';
ext_oid('SubjectAltName') -> 	         ?'id-ce-subjectAltName';
ext_oid('IssuerAltName') -> 	         ?'id-ce-issuerAltName';
ext_oid('SubjectDirectoryAttributes') -> ?'id-ce-subjectDirectoryAttributes';
ext_oid('BasicConstraints') -> 	         ?'id-ce-basicConstraints';
ext_oid('NameConstraints') -> 	         ?'id-ce-nameConstraints';
ext_oid('PolicyConstraints') -> 	 ?'id-ce-policyConstraints';
ext_oid('ExtKeyUsageSyntax') -> 	 ?'id-ce-extKeyUsage';
ext_oid('InhibitAnyPolicy') -> 	         ?'id-ce-inhibitAnyPolicy';
ext_oid('FreshestCRL') -> 	         ?'id-ce-freshestCRL';
ext_oid('IssuingDistributionPoint') ->   ?'id-ce-issuingDistributionPoint';
ext_oid('AuthorityInfoAccessSyntax') ->  ?'id-pe-authorityInfoAccess';
ext_oid('SubjectInfoAccessSyntax') -> 	 ?'id-pe-subjectInfoAccess';
ext_oid('CRLNumber') -> 	         ?'id-ce-cRLNumber';
ext_oid('BaseCRLNumber') -> 	         ?'id-ce-deltaCRLIndicator';
ext_oid('CRLReason') -> 	         ?'id-ce-cRLReasons';
ext_oid('CertificateIssuer') -> 	 ?'id-ce-certificateIssuer';
ext_oid('HoldInstructionCode') -> 	 ?'id-ce-holdInstructionCode';
ext_oid('InvalidityDate') -> 	         ?'id-ce-invalidityDate';
ext_oid('CRLDistributionPoints') -> 	 ?'id-ce-cRLDistributionPoints';
ext_oid(_) ->
    undefined.

decode_extensions(Exts) ->
    decode_extensions(Exts, crl_now).

decode_extensions(asn1_NOVALUE, _) ->
    asn1_NOVALUE;

decode_extensions(Exts, WhenCRL) ->
    lists:map(fun(Ext = #'Extension'{extnID=Id, extnValue=Value0}) ->
                      %% Some Extensions only has special decoding functions
                      %% with other naming-convention
                      {Mod, DecLookup, _Enc, ExtId} = extension_id(Id),
		      case ExtId =/= undefined andalso Mod:DecLookup(Id) of
			  false ->
                              Ext;
                          DecodeExt when ExtId =:= 'CertificatePolicies',
                                         is_function(DecodeExt, 3) ->
                              %% Might need workaround to gracefully handle long user notices
                              try
                                  Value = DecodeExt('ExtnType', iolist_to_binary(Value0), dummy),
                                  Ext#'Extension'{extnValue=transform(Value,decode)}
                              catch exit:{_, {error,{asn1,bad_range}}} ->
                                      decode_otp_cert_polices(Ext, iolist_to_binary(Value0))
                              end;
			  DecodeExt when is_function(DecodeExt, 3) ->
                              case (ExtId == 'CRLDistributionPoints') andalso (WhenCRL == crl_later) of
                                  true ->
                                      %% Work around for certs that do not use CRL's but
                                      %% wrongly decode the extension as NULL
                                      Ext;
                                  false ->
                                      Value = DecodeExt('ExtnType', iolist_to_binary(Value0), dummy),
                                      Ext#'Extension'{extnValue=transform(Value,decode)}
                              end
                      end
	      end, Exts).

decode_otp_cert_polices(Ext, Value) ->
    %% RFC 3280 states that certificate users SHOULD gracefully handle
    %% explicitText with more than 200 characters.
    {ok, CPs} = 'OTP-PKIX':decode('OTPCertificatePolicies', Value),
    Ext#'Extension'{extnValue=[translate_cert_polices(CP) || CP <- CPs]}.

translate_cert_polices(#'OTPPolicyInformation'{policyIdentifier = Id, policyQualifiers = Qs0}) ->
    Qs = [translate_cert_polices(Q) || Q <- Qs0],
    #'PolicyInformation'{policyIdentifier = Id, policyQualifiers = Qs};
translate_cert_polices(#'OTPPolicyQualifierInfo'{policyQualifierId = Id, qualifier = Q0}) ->
    Q = case Q0 of
            #'OTPUserNotice'{noticeRef = Ref, explicitText = {Type, Text0}} ->
                Text = string:slice(Text0, 0, 350),
                #'UserNotice'{noticeRef = Ref, explicitText = {Type, Text}};
            Other ->
                Other
        end,
    #'PolicyQualifierInfo'{policyQualifierId = Id, qualifier = Q}.

encode_extensions(asn1_NOVALUE) ->
    asn1_NOVALUE;

encode_extensions(Exts) ->
    %% Some Extensions only has special decoding functions
    %% with other naming-convention
    lists:map(fun(Ext = #'Extension'{extnID=Id, extnValue=Value0}) ->
                      {Mod, _Dec, EncLookup, ExtId} = extension_id(Id),
		      case ExtId =/= undefined andalso Mod:EncLookup(Id) of
			  false ->
                              Ext;
			  EncodeExt when is_function(EncodeExt, 3) ->
                              case (ExtId == 'CRLDistributionPoints') andalso is_binary(Value0) of
                                  true ->
                                      Ext; %% Already encoded
                                  false ->
                                      Value1 = pubkey_translation:encode(Value0),
                                      Value = element(1,EncodeExt('ExtnType', Value1, dummy)),
                                      Ext#'Extension'{extnValue= iolist_to_binary(Value)}
                              end
		      end
	      end, Exts).

