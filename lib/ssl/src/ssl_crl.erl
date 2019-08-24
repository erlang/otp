%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2017. All Rights Reserved.
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

-export([trusted_cert_and_path/3]).

trusted_cert_and_path(CRL, {SerialNumber, Issuer},{_, {Db, DbRef}} = DbHandle) -> 
    case ssl_pkix_db:lookup_trusted_cert(Db, DbRef, SerialNumber, Issuer) of
	undefined ->
	    trusted_cert_and_path(CRL, issuer_not_found, DbHandle);
	{ok, {_, OtpCert}}  ->
	    {ok, Root, Chain} = ssl_certificate:certificate_chain(OtpCert, Db, DbRef),
	    {ok, Root,  lists:reverse(Chain)}
    end;
trusted_cert_and_path(CRL, issuer_not_found, {CertPath, {Db, DbRef}}) -> 
    case find_issuer(CRL, {certpath, 
                           [{Der, public_key:pkix_decode_cert(Der,otp)} || Der <-  CertPath]}) of
	{ok, OtpCert} ->
	    {ok, Root, Chain} = ssl_certificate:certificate_chain(OtpCert, Db, DbRef),
	    {ok, Root, lists:reverse(Chain)};
	{error, issuer_not_found} ->
	    trusted_cert_and_path(CRL, issuer_not_found, {Db, DbRef}) 
    end;
trusted_cert_and_path(CRL, issuer_not_found, {Db, DbRef} = DbInfo) -> 
    case find_issuer(CRL, DbInfo) of
	{ok, OtpCert} ->
            {ok, Root, Chain} = ssl_certificate:certificate_chain(OtpCert, Db, DbRef),
	    {ok, Root, lists:reverse(Chain)};
         {error, issuer_not_found} ->
             {error, unknown_ca}
     end.

find_issuer(CRL, {certpath = Db, DbRef}) ->
    Issuer = public_key:pkix_normalize_name(public_key:pkix_crl_issuer(CRL)),
    IsIssuerFun =
	fun({_Der,ErlCertCandidate}, Acc) ->
		verify_crl_issuer(CRL, ErlCertCandidate, Issuer, Acc);
	   (_, Acc) ->
		Acc
	end,
    find_issuer(IsIssuerFun, Db, DbRef);
find_issuer(CRL, {Db, DbRef}) ->
    Issuer = public_key:pkix_normalize_name(public_key:pkix_crl_issuer(CRL)),
    IsIssuerFun =
	fun({_Key, {_Der,ErlCertCandidate}}, Acc) ->
		verify_crl_issuer(CRL, ErlCertCandidate, Issuer, Acc);
	   (_, Acc) ->
		Acc
	end,
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

verify_crl_issuer(CRL, ErlCertCandidate, Issuer, NotIssuer) ->
    TBSCert =  ErlCertCandidate#'OTPCertificate'.tbsCertificate,
    case public_key:pkix_normalize_name(TBSCert#'OTPTBSCertificate'.subject) of
	Issuer ->
	    case public_key:pkix_crl_verify(CRL, ErlCertCandidate) of
		true ->
		    throw({ok, ErlCertCandidate});
		false ->
		    NotIssuer
	    end;
	_ ->
	    NotIssuer
    end.
