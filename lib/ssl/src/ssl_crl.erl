%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2021. All Rights Reserved.
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

%----------------------------------------------------------------------
%% Purpose: CRL handling 
%%----------------------------------------------------------------------

-module(ssl_crl).

-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-export([trusted_cert_and_path/4]).

trusted_cert_and_path(CRL, {SerialNumber, Issuer}, CertPath, {Db, DbRef}) ->
    %% CRL issuer cert ID is known
    case ssl_pkix_db:lookup_trusted_cert(Db, DbRef, SerialNumber, Issuer) of
	undefined ->
            %% But not found in our database
	    search_certpath(CRL, CertPath, Db, DbRef);
	{ok, #cert{otp=OtpCert}}  ->
	    {ok, Root, Chain} = ssl_certificate:certificate_chain(OtpCert, Db, DbRef),
	    {ok, Root,  lists:reverse(Chain)}
    end;
trusted_cert_and_path(CRL, issuer_not_found, CertPath, {Db, DbRef}) ->
    case search_certpath(CRL, CertPath, Db, DbRef) of
	{error, unknown_ca} ->
            Issuer = public_key:pkix_normalize_name(public_key:pkix_crl_issuer(CRL)),
            IsIssuerFun =
                fun({_Key, CertCandidate}, Acc) ->
                        verify_crl_issuer(CRL, CertCandidate, Issuer, Acc);
                   (_, Acc) ->
                        Acc
                end,
            case search_db(IsIssuerFun, Db, DbRef) of
                {ok, OtpCert} ->
                    {ok, Root, Chain} = ssl_certificate:certificate_chain(OtpCert, Db, DbRef),
                    {ok, Root, lists:reverse(Chain)};
                {error, issuer_not_found} ->
                    {error, unknown_ca}
            end;
        Result ->
            Result
    end.

search_certpath(CRL, CertPath, Db, DbRef) ->
    Issuer = public_key:pkix_normalize_name(public_key:pkix_crl_issuer(CRL)),
    IsIssuerFun =
	fun(CertCandidate, Acc) ->
		verify_crl_issuer(CRL, CertCandidate, Issuer, Acc)
	end,
    case find_issuer(IsIssuerFun, certpath, CertPath) of
	{ok, OtpCert} ->
	    {ok, Root, Chain} = ssl_certificate:certificate_chain(OtpCert, Db, DbRef),
	    {ok, Root, lists:reverse(Chain)};
	{error, issuer_not_found} ->
            {error, unknown_ca}
    end.

search_db(IsIssuerFun, _, {extracted, ExtractedCerts})->
    find_issuer(IsIssuerFun, extracted, ExtractedCerts);
search_db(IsIssuerFun, Db, DbRef) ->
    find_issuer(IsIssuerFun, Db, DbRef).

find_issuer(IsIssuerFun, certpath, Certs) ->
    try lists:foldl(IsIssuerFun, issuer_not_found, Certs) of
        issuer_not_found ->
            {error, issuer_not_found}
    catch
        {ok, _} = Result ->
            Result
    end;
find_issuer(IsIssuerFun, extracted, CertsData) ->
    Certs = [Entry || {decoded, Entry} <- CertsData],
    try lists:foldl(IsIssuerFun, issuer_not_found, Certs) of
        issuer_not_found ->
            {error, issuer_not_found}
    catch
        {ok, _} = Result ->
            Result
    end;
find_issuer(IsIssuerFun, Db, _) ->  
    try ssl_pkix_db:foldl(IsIssuerFun, issuer_not_found, Db) of
        issuer_not_found ->
            {error, issuer_not_found}
    catch
        {ok, _} = Result ->
            Result
    end.

verify_crl_issuer(CRL, #cert{otp = OTPCertCandidate}, Issuer, NotIssuer) ->
    TBSCert = OTPCertCandidate#'OTPCertificate'.tbsCertificate,
    case public_key:pkix_normalize_name(TBSCert#'OTPTBSCertificate'.subject) of
	Issuer ->
	    case public_key:pkix_crl_verify(CRL, OTPCertCandidate) of
		true ->
		    throw({ok, OTPCertCandidate});
		false ->
		    NotIssuer
	    end;
	_ ->
	    NotIssuer
    end.
