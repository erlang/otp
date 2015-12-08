%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2016. All Rights Reserved.
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

-module(ssl_crl_hash_dir).

-include_lib("public_key/include/public_key.hrl").

-behaviour(ssl_crl_cache_api).

-export([lookup/3, select/2, fresh_crl/2]).

lookup(#'DistributionPoint'{cRLIssuer = CRLIssuer} = DP, CertIssuer, CRLDbInfo) ->
    Issuer =
	case CRLIssuer of
	    asn1_NOVALUE ->
		%% If the distribution point extension doesn't
		%% indicate a CRL issuer, use the certificate issuer.
		CertIssuer;
	    _ ->
		CRLIssuer
	end,
    %% Find all CRLs for this issuer, and return those that match the
    %% given distribution point.
    AllCRLs = select(Issuer, CRLDbInfo),
    lists:filter(fun(DER) -> match_dp(DER, DP) end, AllCRLs).

fresh_crl(#'DistributionPoint'{}, CurrentCRL) ->
    CurrentCRL.

select(Issuer, {_DbHandle, [{dir, Dir}]}) ->
    case find_crls(Issuer, Dir) of
        [_|_] = DERs ->
	    DERs;
        [] ->
            %% That's okay, just report that we didn't find any CRL.
            %% If the crl_check setting is best_effort, ssl_handshake
            %% is happy with that, but if it's true, this is an error.
            [];
        {error, Error} ->
            error_logger:error_report(
              [{cannot_find_crl, Error},
               {dir, Dir},
               {module, ?MODULE},
               {line, ?LINE}]),
            []
    end.

find_crls(Issuer, Dir) ->
    case filelib:is_dir(Dir) of
        true ->
	    Hash = public_key:openssl_name_hash(Issuer),
	    find_crls(Issuer, Hash, Dir, 0, []);
        false ->
            {error, not_a_directory}
    end.

find_crls(Issuer, Hash, Dir, N, Acc) ->
    Filename = filename:join(Dir, Hash ++ ".r" ++ integer_to_list(N)),
    case file:read_file(Filename) of
	{error, enoent} ->
	    Acc;
	{ok, Bin} ->
	    try maybe_parse_pem(Bin) of
		DER when is_binary(DER) ->
		    %% Found one file.  Let's see if there are more.
		    find_crls(Issuer, Hash, Dir, N + 1, [DER] ++ Acc)
	    catch
		error:Error ->
		    %% Something is wrong with the file.  Report
		    %% it, and try the next one.
		    error_logger:error_report(
		      [{crl_parse_error, Error},
		       {filename, Filename},
		       {module, ?MODULE},
		       {line, ?LINE}]),
		    find_crls(Issuer, Hash, Dir, N + 1, Acc)
	    end
    end.

match_dp(_DER, #'DistributionPoint'{distributionPoint = asn1_NOVALUE}) ->
    %% No distribution point name specified - that's considered a match.
    true;
match_dp(DER, #'DistributionPoint'{distributionPoint = {fullName, DPs}}) ->
    #'CertificateList'{
       tbsCertList =
	   #'TBSCertList'{
	      crlExtensions = Extensions}} = parse_crl(DER),
    case pubkey_cert:select_extension(?'id-ce-issuingDistributionPoint', Extensions) of
	undefined ->
	    %% If the CRL doesn't have an IDP extension, it
	    %% automatically qualifies.
	    true;
	#'Extension'{extnValue = IDPValue} ->
	    %% If the CRL does have an IDP extension, it must match
	    %% the given DistributionPoint to be considered a match.
	    IDPEncoded = public_key:der_decode('IssuingDistributionPoint', IDPValue),
	    #'IssuingDistributionPoint'{distributionPoint = {fullName, IDPs}} =
		pubkey_cert_records:transform(IDPEncoded, decode),
	    pubkey_crl:match_one(IDPs, DPs)
    end.


maybe_parse_pem(<<"-----BEGIN", _/binary>> = PEM) ->
    %% It's a PEM encoded file.  Need to extract the DER
    %% encoded data.
    [{'CertificateList', DER, not_encrypted}] = public_key:pem_decode(PEM),
    DER;
maybe_parse_pem(DER) when is_binary(DER) ->
    %% Let's assume it's DER-encoded.
    DER.

parse_crl(DER) when is_binary(DER) ->
    CRL = #'CertificateList'{} = public_key:der_decode('CertificateList', DER),
    CRL.

