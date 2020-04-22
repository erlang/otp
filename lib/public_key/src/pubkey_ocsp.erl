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

-export([otp_cert/1, get_ocsp_responder_id/1, get_nonce/1, get_nonce_extn/1,
         decode_ocsp_response/1, verify_ocsp_response/3,
         get_acceptable_response_types_extn/0]).

%%--------------------------------------------------------------------
-spec verify_ocsp_response(binary(), list(), undefined | binary()) ->
    {ok, term()} | {error, term()}.
%%
%% Description: Verify the OCSP response to get the certificate status
%%--------------------------------------------------------------------
verify_ocsp_response(OCSPResponseDer, ResponderCerts, Nonce) ->
    do_verify_ocsp_response(
        decode_ocsp_response(OCSPResponseDer), ResponderCerts, Nonce
    ).

%%--------------------------------------------------------------------
-spec do_verify_ocsp_response({ok, #'BasicOCSPResponse'{}} | {error, term()},
                              list(), undefined | binary()) ->
    {ok, term()} | {error, term()}.
%%
%% Description: Verify the OCSP response to get the certificate status
%%--------------------------------------------------------------------
do_verify_ocsp_response(
    {ok, #'BasicOCSPResponse'{
        tbsResponseData = ResponseData,
        signatureAlgorithm = SignatureAlgo,
        signature = Signature,
        certs = Certs
    }}, ResponderCerts, Nonce) ->

    #'ResponseData'{
        responderID = ResponderID
    } = ResponseData,

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

%%--------------------------------------------------------------------
-spec verify_ocsp_nonce(#'ResponseData'{}, undefined | binary()) ->
    {ok, term()} | {error, nonce_mismatch}.
%%
%% Description: Check if the nonces matches in OCSP response
%%--------------------------------------------------------------------
verify_ocsp_nonce(ResponseData, Nonce) ->
    #'ResponseData'{
        responses = Responses,
        responseExtensions = ResponseExtns
    } = ResponseData,

    case get_nonce_value(ResponseExtns) of
        Nonce ->
            {ok, Responses};
        _Other ->
            {error, nonce_mismatch}
    end.

%%--------------------------------------------------------------------
-spec get_nonce_value(asn1_NOVALUE | list()) ->
    undefined | binary().
%%
%% Description: Get the nonce value from extensions
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
-spec decode_ocsp_response(binary()) ->
    {ok, #'BasicOCSPResponse'{}} | {error, term()}.
%%
%% Description: Decode the OCSP response der
%%--------------------------------------------------------------------
decode_ocsp_response(Response) ->
    Resp = public_key:der_decode('OCSPResponse', Response),
    case Resp#'OCSPResponse'.responseStatus of
        successful ->
             decode_response_bytes(
                 Resp#'OCSPResponse'.responseBytes
             );
        Error ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
-spec decode_response_bytes(#'ResponseBytes'{}) ->
    {ok, #'BasicOCSPResponse'{}} | {error, term()}.
%%
%% Description: Get basic ocsp response field
%%--------------------------------------------------------------------
decode_response_bytes(#'ResponseBytes'{
                          responseType = ?'id-pkix-ocsp-basic',
                          response = Data}) ->
    {ok, public_key:der_decode('BasicOCSPResponse', Data)};
decode_response_bytes(#'ResponseBytes'{responseType = RespType}) ->
    {error, {ocsp_response_type_not_supported, RespType}}.

%%--------------------------------------------------------------------
-spec verify_ocsp_signature(binary(), term(), term(), list(), term()) ->
    ok | {error, term()}.
%%
%% Description: Verify the signature of OCSP response
%%--------------------------------------------------------------------
verify_ocsp_signature(
    ResponseDataDer, SignatureAlgo, Signature, Certs, ResponderID) ->
    case find_responder_cert(ResponderID, Certs) of
        {ok, Cert} ->
            do_verify_ocsp_signature(
                ResponseDataDer, Signature, SignatureAlgo, Cert);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
-spec find_responder_cert(term(), list()) ->
    {ok, #'Certificate'{} | #'OTPCertificate'{}} | {error, term()}.
%%
%% Description: Find the OCSP responder's cert in input list
%%--------------------------------------------------------------------
find_responder_cert(_ResponderID, []) ->
    {error, ocsp_responder_cert_not_found};
find_responder_cert(ResponderID, [Cert | TCerts]) ->
    case is_responder(ResponderID, Cert) of
        true ->
            {ok, Cert};
        false ->
            find_responder_cert(ResponderID, TCerts)
    end.

%%--------------------------------------------------------------------
-spec do_verify_ocsp_signature(
        binary(), term(), term(), #'Certificate'{} | #'OTPCertificate'{}) ->
    ok | {error, term()}.
%%
%% Description: Verify the signature of OCSP response
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
-spec get_public_key_rec(#'Certificate'{} | #'OTPCertificate'{}) ->
    term().
%%
%% Description: Get the subject public key field
%%--------------------------------------------------------------------
get_public_key_rec(#'Certificate'{} = Cert) ->
    get_public_key_rec(otp_cert(Cert));
get_public_key_rec(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    PKInfo = TbsCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PKInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey.

%%--------------------------------------------------------------------
-spec is_responder(tuple(), #'Certificate'{} | #'OTPCertificate'{}) ->
    boolean().
%%
%% Description: Check if is OCSP responder's cert
%%--------------------------------------------------------------------
is_responder({byName, Name}, Cert) ->
    public_key:der_encode('Name', Name) == get_subject_name(Cert);
is_responder({byKey, Key}, Cert) ->
    Key == crypto:hash(sha, get_public_key(Cert)).

%%--------------------------------------------------------------------
-spec otp_cert(#'Certificate'{} | #'OTPCertificate'{} | binary()) ->
    #'OTPCertificate'{}.
%%
%% Description: Convert to #'OTPCertificate'{}
%%--------------------------------------------------------------------
otp_cert(#'OTPCertificate'{} = Cert) ->
    Cert;
otp_cert(#'Certificate'{} = Cert) ->
    public_key:pkix_decode_cert(
        public_key:der_encode('Certificate', Cert), otp);
otp_cert(CertDer) when is_binary(CertDer) ->
    public_key:pkix_decode_cert(CertDer, otp).

%%--------------------------------------------------------------------
-spec get_ocsp_responder_id(#'Certificate'{}) -> binary().
%%
%% Description: Get the OCSP responder ID der
%%--------------------------------------------------------------------
get_ocsp_responder_id(#'Certificate'{tbsCertificate = TbsCert}) ->
    public_key:der_encode(
        'ResponderID', {byName, TbsCert#'TBSCertificate'.subject}).

%%--------------------------------------------------------------------
-spec get_subject_name(#'Certificate'{} | #'OTPCertificate'{}) -> binary().
%%
%% Description: Get the subject der from cert
%%--------------------------------------------------------------------
get_subject_name(#'Certificate'{} = Cert) ->
    get_subject_name(otp_cert(Cert));
get_subject_name(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    public_key:pkix_encode('Name', TbsCert#'OTPTBSCertificate'.subject, otp).

%%--------------------------------------------------------------------
-spec get_public_key(#'Certificate'{} | #'OTPCertificate'{}) -> binary().
%%
%% Description: Get the public key der from cert
%%--------------------------------------------------------------------
get_public_key(#'Certificate'{} = Cert) ->
    get_public_key(otp_cert(Cert));
get_public_key(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    PKInfo = TbsCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    #'OTPSubjectPublicKeyInfo'{
		subjectPublicKey = SubPubKey} = PKInfo,
    SubPubKey.

%%--------------------------------------------------------------------
-spec get_nonce(boolean()) -> binary() | undefined.
%%
%% Description: Get an OCSP nonce
%%--------------------------------------------------------------------
get_nonce(false) ->
    undefined;
get_nonce(true) ->
    crypto:strong_rand_bytes(8).

%%--------------------------------------------------------------------
-spec get_nonce_extn(undefined | binary()) -> undefined | #'Extension'{}.
%%
%% Description: Get an OCSP nonce der
%%--------------------------------------------------------------------
get_nonce_extn(undefined) ->
    undefined;
get_nonce_extn(Nonce) when is_binary(Nonce) ->
    #'Extension'{
        extnID    = ?'id-pkix-ocsp-nonce',
        extnValue = Nonce
    }.

%%--------------------------------------------------------------------
-spec get_acceptable_response_types_extn() -> #'Extension'{}.
%%
%% Description: Get an acceptable response types der
%%--------------------------------------------------------------------
get_acceptable_response_types_extn() ->
    #'Extension'{
        extnID    = ?'id-pkix-ocsp-response',
        extnValue = public_key:der_encode(
            'AcceptableResponses', [?'id-pkix-ocsp-basic'])
    }.