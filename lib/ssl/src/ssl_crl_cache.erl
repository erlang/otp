%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2024. All Rights Reserved.
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
-moduledoc """
CRL cache

Implements an internal CRL (Certificate Revocation List) cache. In addition to
implementing the `m:ssl_crl_cache_api` behaviour the following functions are
available.
""".
-moduledoc(#{since => "OTP 18.0"}).

-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-behaviour(ssl_crl_cache_api).

-export_type([crl_src/0]).
-doc """
A source to input CRLs
""".
-type crl_src() :: {file, file:filename()} | {der,  public_key:der_encoded()}.

-export([lookup/3, select/2, fresh_crl/2]).
-export([insert/1, insert/2, delete/1]).

%%====================================================================
%% Cache callback API
%%====================================================================

-doc false.
lookup(#'DistributionPoint'{distributionPoint = {fullName, Names}},
       _Issuer,
       CRLDbInfo) ->
    get_crls(Names, CRLDbInfo);
lookup(_,_,_) ->
    not_available.

-doc false.
select(GenNames, CRLDbHandle) when is_list(GenNames) ->
    lists:flatmap(fun({directoryName, Issuer}) ->
                          select(Issuer, CRLDbHandle);
                     (_) ->
                          []
                  end, GenNames);
select(Issuer, {{_Cache, Mapping},_}) ->
    case ssl_pkix_db:lookup(Issuer, Mapping) of
	undefined ->
	    [];
	CRLs ->
	    CRLs
    end.

-doc false.
fresh_crl(#'DistributionPoint'{distributionPoint = {fullName, Names}}, CRL) ->
    case get_crls(Names, undefined) of
	not_available ->
	    CRL;
	NewCRL ->
	    NewCRL
    end.

%%====================================================================
%% API 
%%====================================================================

%%--------------------------------------------------------------------
-doc(#{equiv => insert/2}).
-doc(#{since => <<"OTP 18.0">>}).
-spec insert(CRLSrc) -> ok | {error, Reason} when
      CRLSrc :: crl_src(),
      Reason :: ssl:reason().
%%--------------------------------------------------------------------
insert(CRLSrc) ->
    insert(?NO_DIST_POINT, CRLSrc).

%%--------------------------------------------------------------------
-doc """
Insert CRLs into the ssl applications local cache, with or without a
distribution point reference URI
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec insert(DistPointURI, CRLSrc) -> ok | {error, Reason} when
      DistPointURI :: uri_string:uri_string(),
      CRLSrc :: crl_src(),
      Reason :: ssl:reason().
%%--------------------------------------------------------------------
insert(DistPointURI, {file, File}) when is_list(DistPointURI) ->
    case file:read_file(File) of
	{ok, PemBin} ->
	    PemEntries = public_key:pem_decode(PemBin),
	    CRLs = [ CRL || {'CertificateList', CRL, not_encrypted} 
				<- PemEntries],
	    do_insert(DistPointURI, CRLs);
	Error ->
	    Error
    end;
insert(DistPointURI, {der, CRLs}) ->	
    do_insert(DistPointURI, CRLs).

%%--------------------------------------------------------------------
-doc """
Delete CRLs from the ssl applications local cache.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec delete(Entries) -> ok | {error, Reason} when
      Entries :: crl_src() | uri_string:uri_string(),
      Reason :: ssl:reason().
%%--------------------------------------------------------------------
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
    case uri_string:normalize(URI, [return_map]) of
	#{scheme := "http", path := Path} ->
	    ssl_manager:delete_crls(string:trim(Path, leading, "/"));
	_ ->
	    {error, {only_http_distribution_points_supported, URI}}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_insert(URI, CRLs) ->
    case uri_string:normalize(URI, [return_map]) of
	#{scheme := "http", path := Path} ->
	    ssl_manager:insert_crls(string:trim(Path, leading, "/"), CRLs);
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
    #{path :=  Path} = uri_string:normalize(URL, [return_map]),
    case ssl_pkix_db:lookup(string:trim(Path, leading, "/"), Cache) of
	undefined ->
	    [];
	[CRLs] ->
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

