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

-module(pubkey_ocsp).

-include("public_key.hrl").

-export([otp_cert/1,
         get_ocsp_responder_id/1,
         get_nonce_extn/1,
         verify_ocsp_response/3,
         get_acceptable_response_types_extn/0,
         find_single_response/3,
         ocsp_status/1]).

%% Test related exports
-export([decode_ocsp_response/1]).

otp_cert(#'OTPCertificate'{} = Cert) ->
    Cert;
otp_cert(#'Certificate'{} = Cert) ->
    public_key:pkix_decode_cert(
        public_key:der_encode('Certificate', Cert), otp);
otp_cert(CertDer) when is_binary(CertDer) ->
    public_key:pkix_decode_cert(CertDer, otp).

-spec get_ocsp_responder_id(#'Certificate'{}) -> binary().
get_ocsp_responder_id(#'Certificate'{tbsCertificate = TbsCert}) ->
    public_key:der_encode(
        'ResponderID', {byName, TbsCert#'TBSCertificate'.subject}).

-spec get_nonce_extn(undefined | binary()) -> undefined | #'Extension'{}.
get_nonce_extn(undefined) ->
    undefined;
get_nonce_extn(Nonce) when is_binary(Nonce) ->
    #'Extension'{
        extnID    = ?'id-pkix-ocsp-nonce',
        extnValue = Nonce
    }.

-spec verify_ocsp_response(binary(), list(), undefined | binary()) ->
    {ok, term()} | {error, term()}.
verify_ocsp_response(OCSPResponseDer, ResponderCerts, Nonce) ->
    do_verify_ocsp_response(
      decode_ocsp_response(OCSPResponseDer), ResponderCerts, Nonce).

-spec get_acceptable_response_types_extn() -> #'Extension'{}.
get_acceptable_response_types_extn() ->
    #'Extension'{
        extnID    = ?'id-pkix-ocsp-response',
        extnValue = public_key:der_encode(
            'AcceptableResponses', [?'id-pkix-ocsp-basic'])
    }.

-spec find_single_response(#'OTPCertificate'{}, #'OTPCertificate'{},
                           [#'SingleResponse'{}]) ->
          {ok, #'SingleResponse'{}} | {error, no_matched_response}.
find_single_response(Cert, IssuerCert, SingleResponseList) ->
    IssuerName = get_subject_name(IssuerCert),
    IssuerKey = get_public_key(IssuerCert),
    SerialNum = get_serial_num(Cert),
    match_single_response(IssuerName, IssuerKey, SerialNum, SingleResponseList).

-spec ocsp_status({atom(), term()}) -> atom() | {atom(), {atom(), term()}}.
ocsp_status({good, _}) ->
    valid;
ocsp_status({unknown, Reason}) ->
    {bad_cert, {revocation_status_undetermined, Reason}};
ocsp_status({revoked, Reason}) ->
    {bad_cert, {revoked, Reason}}.

decode_ocsp_response(ResponseDer) ->
    Resp = public_key:der_decode('OCSPResponse', ResponseDer),
    case Resp#'OCSPResponse'.responseStatus of
        successful ->
             decode_response_bytes(
                 Resp#'OCSPResponse'.responseBytes
             );
        Error ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
match_single_response(_IssuerName, _IssuerKey, _SerialNum, []) ->
    {error, no_matched_response};
match_single_response(IssuerName, IssuerKey, SerialNum,
                      [#'SingleResponse'{
                          certID = #'CertID'{hashAlgorithm = Algo} = CertID} =
                           Response | Responses]) ->
    HashType = public_key:pkix_hash_type(Algo#'AlgorithmIdentifier'.algorithm),
    case (SerialNum == CertID#'CertID'.serialNumber) andalso
        (crypto:hash(HashType, IssuerName) == CertID#'CertID'.issuerNameHash) andalso
        (crypto:hash(HashType, IssuerKey) == CertID#'CertID'.issuerKeyHash) of
        true ->
            {ok, Response};
        false ->
            match_single_response(IssuerName, IssuerKey, SerialNum, Responses)
    end.

get_serial_num(Cert) ->
    #'OTPCertificate'{tbsCertificate = TbsCert} = otp_cert(Cert),
    TbsCert#'OTPTBSCertificate'.serialNumber.

decode_response_bytes(#'ResponseBytes'{
                          responseType = ?'id-pkix-ocsp-basic',
                          response = Data}) ->
    {ok, public_key:der_decode('BasicOCSPResponse', Data)};
decode_response_bytes(#'ResponseBytes'{responseType = RespType}) ->
    {error, {ocsp_response_type_not_supported, RespType}}.

do_verify_ocsp_response({ok, #'BasicOCSPResponse'{
                                tbsResponseData = ResponseData,
                                signatureAlgorithm = SignatureAlgo,
                                signature = Signature,
                                certs = Certs}},
                        ResponderCerts, Nonce) ->
    #'ResponseData'{responderID = ResponderID} = ResponseData,
    case verify_ocsp_signature(
           public_key:der_encode('ResponseData', ResponseData),
           SignatureAlgo#'AlgorithmIdentifier'.algorithm,
           Signature, Certs ++ ResponderCerts,
           ResponderID) of
        ok ->
            verify_ocsp_nonce(ResponseData, Nonce);
        {error, Reason} ->
            {error, Reason}
    end;
do_verify_ocsp_response({error, Reason}, _ResponderCerts, _Nonce) ->
    {error, Reason}.

verify_ocsp_nonce(ResponseData, Nonce) ->
    #'ResponseData'{responses = Responses, responseExtensions = ResponseExtns} =
        ResponseData,
    case get_nonce_value(ResponseExtns) of
        Nonce ->
            {ok, Responses};
        _Other ->
            {error, nonce_mismatch}
    end.

%% no extensions present in response
get_nonce_value(asn1_NOVALUE) ->
    undefined;
%% extensions exist, but no oscp nonce
get_nonce_value([]) ->
    undefined;
get_nonce_value([#'Extension'{
                     extnID = ?'id-pkix-ocsp-nonce',
                     extnValue = Value} | _Extns]) ->
    Value;
get_nonce_value([_Extn | Rest]) ->
    get_nonce_value(Rest).

verify_ocsp_signature(ResponseDataDer, SignatureAlgo, Signature,
                      Certs, ResponderID) ->
    case find_responder_cert(ResponderID, Certs) of
        {ok, Cert} ->
            do_verify_ocsp_signature(
                ResponseDataDer, Signature, SignatureAlgo, Cert);
        {error, Reason} ->
            {error, Reason}
    end.

find_responder_cert(_ResponderID, []) ->
    {error, ocsp_responder_cert_not_found};
find_responder_cert(ResponderID, [Cert | TCerts]) ->
    case is_responder(ResponderID, Cert) of
        true ->
            {ok, Cert};
        false ->
            find_responder_cert(ResponderID, TCerts)
    end.

is_responder({byName, Name}, Cert) ->
    public_key:der_encode('Name', Name) == get_subject_name(Cert);
is_responder({byKey, Key}, Cert) ->
    Key == crypto:hash(sha, get_public_key(Cert)).

get_subject_name(#'Certificate'{} = Cert) ->
    get_subject_name(otp_cert(Cert));
get_subject_name(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    public_key:pkix_encode('Name', TbsCert#'OTPTBSCertificate'.subject, otp).

get_public_key(#'Certificate'{} = Cert) ->
    get_public_key(otp_cert(Cert));
get_public_key(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    PKInfo = TbsCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    enc_pub_key(PKInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey).

enc_pub_key(Key = #'RSAPublicKey'{}) ->
    public_key:der_encode('RSAPublicKey', Key);
enc_pub_key({DsaInt, #'Dss-Parms'{}}) when is_integer(DsaInt) ->
    public_key:der_encode('DSAPublicKey', DsaInt);
enc_pub_key({#'ECPoint'{point = Key}, _ECParam}) ->
    Key.

do_verify_ocsp_signature(ResponseDataDer, Signature, AlgorithmID, Cert) ->
    {DigestType, _SignatureType} = public_key:pkix_sign_types(AlgorithmID),
    case public_key:verify(
           ResponseDataDer, DigestType, Signature,
           get_public_key_rec(Cert)) of
        true ->
            ok;
        false ->
            {error, ocsp_response_bad_signature}
    end.

get_public_key_rec(#'Certificate'{} = Cert) ->
    get_public_key_rec(otp_cert(Cert));
get_public_key_rec(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    PKInfo = TbsCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PKInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey.

