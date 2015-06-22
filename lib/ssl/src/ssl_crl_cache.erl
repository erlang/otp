%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2015. All Rights Reserved.
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
%% Purpose: Simple default CRL cache 
%%----------------------------------------------------------------------

-module(ssl_crl_cache).

-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-behaviour(ssl_crl_cache_api).

-export([lookup/2, select/2, fresh_crl/2]).
-export([insert/1, insert/2, delete/1]).

%%====================================================================
%% Cache callback API
%%====================================================================

lookup(#'DistributionPoint'{distributionPoint = {fullName, Names}},
       CRLDbInfo) ->
    get_crls(Names, CRLDbInfo);
lookup(_,_) ->
    not_available.

select(Issuer, {{_Cache, Mapping},_}) ->
    case ssl_pkix_db:lookup(Issuer, Mapping) of
	undefined ->
	    [];
	CRLs ->
	    CRLs
    end.

fresh_crl(#'DistributionPoint'{distributionPoint = {fullName, Names}}, CRL) ->
    case get_crls(Names, undefined) of
	not_available ->
	    CRL;
	[NewCRL] ->
	    NewCRL
    end.

%%====================================================================
%% API 
%%====================================================================

insert(CRLs) ->
    insert(?NO_DIST_POINT, CRLs).

insert(URI, {file, File}) when is_list(URI) ->				     
    case file:read_file(File) of
	{ok, PemBin} ->
	    PemEntries = public_key:pem_decode(PemBin),
	    CRLs = [ CRL || {'CertificateList', CRL, not_encrypted} 
				<- PemEntries],
	    do_insert(URI, CRLs);
	Error ->
	    Error
    end;
insert(URI, {der, CRLs}) ->	
    do_insert(URI, CRLs).

delete({file, File}) ->
    case file:read_file(File) of
	{ok, PemBin} ->
	    PemEntries = public_key:pem_decode(PemBin),
	    CRLs = [ CRL || {'CertificateList', CRL, not_encrypted} 
				<- PemEntries],
	    ssl_manager:delete_crls({?NO_DIST_POINT, CRLs});
	Error ->
	    Error
    end;
delete({der, CRLs}) ->	
    ssl_manager:delete_crls({?NO_DIST_POINT, CRLs});

delete(URI) ->
    case http_uri:parse(URI) of
	{ok, {http, _, _ , _, Path,_}} -> 
	    ssl_manager:delete_crls(string:strip(Path, left, $/));
	_ ->
	    {error, {only_http_distribution_points_supported, URI}}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_insert(URI, CRLs) ->
    case http_uri:parse(URI) of
	{ok, {http, _, _ , _, Path,_}} -> 
	    ssl_manager:insert_crls(string:strip(Path, left, $/), CRLs);
	_ ->
	    {error, {only_http_distribution_points_supported, URI}}
    end.

get_crls([], _) ->
    not_available;
get_crls([{uniformResourceIdentifier, "http"++_ = URL} | Rest], 
	 CRLDbInfo) ->
    case cache_lookup(URL, CRLDbInfo) of
	[] ->
	   handle_http(URL, Rest, CRLDbInfo);
	CRLs ->
	    CRLs
    end;
get_crls([ _| Rest], CRLDbInfo) ->
    %% unsupported CRL location
    get_crls(Rest, CRLDbInfo).

http_lookup(URL, Rest, CRLDbInfo, Timeout) ->
    case application:ensure_started(inets) of
	ok ->
	    http_get(URL, Rest, CRLDbInfo, Timeout);  
	_ ->
	    get_crls(Rest, CRLDbInfo)
    end.

http_get(URL, Rest, CRLDbInfo, Timeout) ->
    case httpc:request(get, {URL, [{"connection", "close"}]}, 
		       [{timeout, Timeout}], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} ->
            case Body of
                <<"-----BEGIN", _/binary>> ->
                    Pem = public_key:pem_decode(Body),
		    lists:filtermap(fun({'CertificateList', 
					 CRL, not_encrypted}) ->
					    {true, CRL};
				       (_) ->
					    false
				    end, Pem);
		_ ->
		    try public_key:der_decode('CertificateList', Body) of
			_ ->
			    [Body]
		    catch
			_:_ ->
			    get_crls(Rest, CRLDbInfo)
		    end   
	    end;
        {error, _Reason} ->
            get_crls(Rest, CRLDbInfo)
    end.

cache_lookup(_, undefined) ->
    [];
cache_lookup(URL, {{Cache, _}, _}) ->
    {ok, {_, _, _ , _, Path,_}} = http_uri:parse(URL), 
    case ssl_pkix_db:lookup(string:strip(Path, left, $/), Cache) of
	undefined ->
	    [];
	CRLs ->
	    CRLs
    end.

handle_http(URI, Rest, {_,  [{http, Timeout}]} = CRLDbInfo) ->
    CRLs = http_lookup(URI, Rest, CRLDbInfo, Timeout),
    %% Uncomment to improve performance, but need to 
    %% implement cache limit and or cleaning to prevent 
    %% DoS attack possibilities
    %%insert(URI, {der, CRLs}),
    CRLs;
handle_http(_, Rest, CRLDbInfo) ->
    get_crls(Rest, CRLDbInfo).

