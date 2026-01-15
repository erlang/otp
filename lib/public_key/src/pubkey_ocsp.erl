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

-module(pubkey_ocsp).
-moduledoc false.

-include("public_key_internal.hrl").

-export([find_single_response/3,
         get_acceptable_response_types_extn/0,
         get_nonce_extn/1,
         status/2,
         verify_response/5,
         decode_response/1]).
%% Tracing
-export([handle_trace/3]).

-spec get_nonce_extn(undefined | binary()) -> undefined | #'Extension'{}.
get_nonce_extn(undefined) ->
    undefined;
get_nonce_extn(Nonce) when is_binary(Nonce) ->
    #'Extension'{
        extnID    = ?'id-pkix-ocsp-nonce',
        extnValue = Nonce
    }.

-spec verify_response(#'BasicOCSPResponse'{}, list(), undefined | binary(),
                           public_key:cert(), fun()) ->
    {ok, term(), list()} | {error, term()}.
verify_response(#'BasicOCSPResponse'{
                   tbsResponseData = ResponseData,
                   signatureAlgorithm = SignatureAlgo,
                   signature = Signature},
                ResponderCerts, Nonce, IssuerCert,
                IsTrustedResponderFun) ->
    #'ResponseData'{responderID = ResponderID,
                    producedAt = ProducedAt} = ResponseData,
    maybe
        ok ?= verify_past_timestamp(ProducedAt),
        ok ?= verify_signature(
                public_key:der_encode('ResponseData', ResponseData),
                SignatureAlgo#'BasicOCSPResponse_signatureAlgorithm'.algorithm,
                Signature, ResponderCerts,
                ResponderID, IssuerCert, IsTrustedResponderFun),
        verify_nonce(ResponseData, Nonce)
    else
        {error, Reason} ->
            {error, Reason}
    end.

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

-spec status({atom(), term()}, list()) -> {ok, list()} | {error, {bad_cert, term()}}.
status({good, _}, Details) ->
    {ok, Details};
status({unknown, Reason}, _) ->
    {error, {bad_cert, {revocation_status_undetermined, Reason}}};
status({revoked, Reason}, _) ->
    {error, {bad_cert, {revoked, Reason}}}.

decode_response(ResponseDer) ->
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
                           SingleResponse | Tail]) ->
    #'SingleResponse'{thisUpdate = ThisUpdate,
                      nextUpdate = NextUpdate} = SingleResponse,
    HashType = public_key:pkix_hash_type(Algo#'CertID_hashAlgorithm'.algorithm),
    case (SerialNum == CertID#'CertID'.serialNumber) andalso
        (crypto:hash(HashType, IssuerName) == CertID#'CertID'.issuerNameHash) andalso
        (crypto:hash(HashType, IssuerKey) == CertID#'CertID'.issuerKeyHash) andalso
        verify_past_timestamp(ThisUpdate) == ok andalso
        verify_next_update(NextUpdate) == ok of
        true ->
            {ok, SingleResponse};
        false ->
            match_single_response(IssuerName, IssuerKey, SerialNum, Tail)
    end;
match_single_response(IssuerName, IssuerKey, SerialNum,
                      [_BadSingleResponse | Tail]) ->
    match_single_response(IssuerName, IssuerKey, SerialNum, Tail).

get_serial_num(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    TbsCert#'OTPTBSCertificate'.serialNumber.

decode_response_bytes(#'ResponseBytes'{
                          responseType = ?'id-pkix-ocsp-basic',
                          response = Data}) ->
    {ok, public_key:der_decode('BasicOCSPResponse', Data)};
decode_response_bytes(#'ResponseBytes'{responseType = RespType}) ->
    {error, {ocsp_response_type_not_supported, RespType}}.

verify_nonce(ResponseData, NonceSent) ->
    #'ResponseData'{responses = Responses, responseExtensions = ResponseExtns} =
        ResponseData,
    NonceReceived = get_nonce_value(ResponseExtns),
    case {NonceSent, NonceReceived} of
        {undefined, _} -> % disabled
            {ok, Responses, []};
        {NonceSent, undefined} when is_binary(NonceSent) -> % enabled but not received
            %% As specified in RFC8954 3.1, RFC6960 4.4, RFC5019 2.2.1
            %% lack of nonce in response should not stop processing
            {ok, Responses, [{missing, ocsp_nonce}]};
        {NonceSent, NonceSent} -> % enabled, sent and received the same value
            {ok, Responses, []};
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

verify_signature(_, _, _, [], _, _, _) ->
    {error, ocsp_responder_cert_not_found};
verify_signature(ResponseDataDer, SignatureAlgo, Signature,
                 [ResponderCert | RCs], ResponderID, IssuerCert,
                 IsTrustedResponderFun) ->
    maybe
        true ?= is_responder_cert(ResponderID, ResponderCert),
        true ?= is_authorized_responder(ResponderCert, IssuerCert,
                                        IsTrustedResponderFun),
        ok ?= do_verify_signature(ResponseDataDer, Signature, SignatureAlgo,
                                  ResponderCert)
    else
        _->
            verify_signature(ResponseDataDer, SignatureAlgo, Signature,
                             RCs, ResponderID, IssuerCert,
                             IsTrustedResponderFun)
    end.

verify_past_timestamp(Timestamp) ->
    {Now, TimestampSec} = get_time_in_sec(Timestamp),
    verify_timestamp(Now, TimestampSec, past_timestamp).

verify_future_timestamp(Timestamp) ->
    {Now, TimestampSec} = get_time_in_sec(Timestamp),
    verify_timestamp(Now, TimestampSec, future_timestamp).

verify_timestamp(Now, Timestamp, past_timestamp) when Timestamp =< Now ->
    ok;
verify_timestamp(Now, Timestamp, future_timestamp) when Now =< Timestamp ->
    ok;
verify_timestamp(_, _, _) ->
    {error, ocsp_stale_response}.

get_time_in_sec(Timestamp) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    TimestampSec = pubkey_cert:time_str_2_gregorian_sec(
                     {generalTime, Timestamp}),
    {Now, TimestampSec}.

verify_next_update(asn1_NOVALUE) ->
    ok;
verify_next_update(NextUpdate) ->
    verify_future_timestamp(NextUpdate).

is_responder_cert({byName, Name}, #cert{otp = Cert}) ->
    public_key:der_encode('Name', Name) == get_subject_name(Cert);
is_responder_cert({byKey, Key}, #cert{otp = Cert}) ->
    Key == crypto:hash(sha, get_public_key(Cert)).

is_authorized_responder(CombinedResponderCert = #cert{otp = ResponderCert},
                        IssuerCert, IsTrustedResponderFun) ->
    Case1 =
        %% the CA who issued the certificate in question signed the
        %% response
        fun() ->
                ResponderCert == IssuerCert
        end,
    Case2 =
        %% a CA Designated Responder (Authorized Responder, defined in
        %%      Section 4.2.2.2) who holds a specially marked certificate
        %%      issued directly by the CA, indicating that the responder may
        %%      issue OCSP responses for that CA (id-kp-OCSPSigning)
        fun() ->
                public_key:pkix_is_issuer(ResponderCert, IssuerCert) andalso
                                 designated_for_ocsp_signing(ResponderCert)
        end,
    Case3 =
        %% a Trusted Responder whose public key is trusted by the requestor
        fun() ->
                IsTrustedResponderFun(CombinedResponderCert)
        end,

    case lists:any(fun(E) -> E() end, [Case1, Case2, Case3]) of
        true ->
            true;
        false ->
            not_authorized_responder
    end.

do_verify_signature(ResponseDataDer, Signature, AlgorithmID,
                    #cert{otp = ResponderCert}) ->
    {DigestType, _SignatureType} = public_key:pkix_sign_types(AlgorithmID),
    case public_key:verify(
           ResponseDataDer, DigestType, Signature,
           get_public_key_rec(ResponderCert)) of
        true ->
            ok;
        false ->
            {error, ocsp_response_bad_signature}
    end.

get_public_key_rec(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    PKInfo = TbsCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    Params = PKInfo#'OTPSubjectPublicKeyInfo'.algorithm#'PublicKeyAlgorithm'.parameters,
    SubjectPublicKey = PKInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    case {SubjectPublicKey, Params} of
        {#'RSAPublicKey'{}, 'NULL'} ->
            SubjectPublicKey;
        {_, _} ->
            {SubjectPublicKey, Params}
    end.

get_subject_name(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    public_key:pkix_encode('Name', TbsCert#'OTPTBSCertificate'.subject, otp).

get_public_key(OtpCert) ->
    enc_pub_key(get_public_key_rec(OtpCert)).

enc_pub_key(Key = #'RSAPublicKey'{}) ->
    public_key:der_encode('RSAPublicKey', Key);
enc_pub_key({DsaInt, #'Dss-Parms'{}}) when is_integer(DsaInt) ->
    public_key:der_encode('DSAPublicKey', DsaInt);
enc_pub_key({#'ECPoint'{point = Key}, _ECParam}) ->
    Key.

designated_for_ocsp_signing(OtpCert) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
    TBSExtensions = TBSCert#'OTPTBSCertificate'.extensions,
    Extensions = pubkey_cert:extensions_list(TBSExtensions),
    case pubkey_cert:select_extension(?'id-ce-extKeyUsage', Extensions) of
	undefined ->
	    false;
	#'Extension'{extnValue = KeyUses} ->
            lists:member(?'id-kp-OCSPSigning', KeyUses)
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(csp,
             {call, {?MODULE, match_single_response,
                     [_IssuerName, _IssuerKey, _SerialNum,
                     [#'SingleResponse'{thisUpdate = ThisUpdate,
                                        nextUpdate = NextUpdate} | _]]}}, Stack) ->
    {io_lib:format("ThisUpdate = ~p NextUpdate = ~p", [ThisUpdate, NextUpdate]), Stack};
handle_trace(csp,
             {call, {?MODULE, is_responder, [Id, Cert]}}, Stack) ->
    {io_lib:format("~nId = ~P~nCert = ~P", [Id, 10, Cert, 10]), Stack};
handle_trace(csp,
             {call, {?MODULE, find_single_response, [Cert, IssuerCert | _]}}, Stack) ->
    {io_lib:format("#2 OCSP validation started~nCert = ~W IssuerCert = ~W",
                   [Cert, 7, IssuerCert, 7]), Stack};
    %% {io_lib:format("#2 OCSP validation started~nCert = ~s IssuerCert = ~s",
    %%                [ssl_test_lib:format_cert(Cert),
    %%                 ssl_test_lib:format_cert(IssuerCert)]), Stack};

handle_trace(csp,
             {return_from, {?MODULE, is_responder, 2}, Return},
             Stack) ->
    {io_lib:format("Return = ~p", [Return]), Stack}.
