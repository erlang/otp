%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2026. All Rights Reserved.
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

%%----------------------------------------------------------------------
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

%% Exported for testing (ssl_crl_SUITE)
-export([is_internal_ip/1, validate_host/4, is_allowed/3]).

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
%% Might want to add possibility to provide <CRLDbInfo> argument
%% Future improvement, needs public_key feature too.
fresh_crl(#'DistributionPoint'{distributionPoint = {fullName, Names}}, CRL) ->
    case get_crls(Names, {undefined, [{http, 1000}]}) of
	not_available ->
	    CRL;
	NewCRL ->
	    NewCRL
    end;
fresh_crl(#'DistributionPoint'{}, CRL) ->
    %% nameRelativeToCRLIssuer or asn1_NOVALUE — cannot fetch via HTTP,
    %% return the current CRL unchanged.
    CRL.

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
	#{scheme := "http",
          host := Host,
          path := Path} = Map ->
            Port = maps:get(port, Map, 80),
            Key = make_key(Host, Port, Path),
	    ssl_manager:delete_crls(Key);
	_ ->
	    {error, {only_http_distribution_points_supported, URI}}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_insert(URI, CRLs) ->
    case uri_string:normalize(URI, [return_map]) of
	#{scheme := "http", 
          host := Host,
          path := Path} = Map ->
            Port = maps:get(port, Map, 80),
            Key = make_key(Host, Port, Path),
	    ssl_manager:insert_crls(Key, CRLs);
	_ ->
	    {error, {only_http_distribution_points_supported, URI}}
    end.

get_crls([], _) ->
    not_available;
get_crls([{uniformResourceIdentifier, "http"++_ = URL} | Rest], 
	 CRLDbInfo) ->
    URI = #{scheme := Scheme} = uri_string:normalize(URL, [return_map]),
    case cache_lookup(URI, CRLDbInfo) of
        [] when Scheme == "http" ->
            handle_http(URL, URI, Rest, CRLDbInfo);
        [] ->
            get_crls(Rest, CRLDbInfo);
	CRLs ->
	    CRLs
    end;
get_crls([ _| Rest], CRLDbInfo) ->
    %% unsupported CRL location
    get_crls(Rest, CRLDbInfo).

http_lookup(URL, #{host := Host} = URI, Rest,  {_, Args} = CRLDbInfo, Timeout) ->
    case application:ensure_started(inets) of
	ok ->
            Allowed = proplists:get_value(allowed_hosts, Args, []),
            {IP, Family} = host_family(Host),
            case validate_host(URI, Allowed, IP, Family) of
                ok ->
                    http_get(URL, IP, Family, Rest, CRLDbInfo, Timeout);
                {disallowed, Reason} ->
                    ?LOG_WARNING("CRL fetch ignored, host disallowed: ~p ~n", [Reason]),
                    get_crls(Rest, CRLDbInfo)
            end;
	_ ->
	    get_crls(Rest, CRLDbInfo)
    end.

-doc false.
-spec validate_host(map(), [string()], inet:ip_address(), inet | inet6| unknown) ->
          ok | {disallowed, term()}.
validate_host(_, _, _, unknown) ->
    {disallowed, host_not_found};
validate_host(#{host := Host} = URI, Allowed, IP, _) ->
    Port = maps:get(port, URI, 80),
    case Allowed of
        [] when Port == 80;
                Port == 8080 ->
            allow_external_ip(IP);
        [_ | _] ->
            case is_allowed(Host, Port, Allowed) of
                true ->
                    ok;
                false ->
                    {disallowed, not_in_allowed_list}
            end;
        _ ->
            {disallowed, not_standard_port}
    end.

-doc false.
-spec is_allowed(string(), non_neg_integer(), [string()]) -> boolean().

is_allowed(_, _, []) ->
    false;
is_allowed(Host, Port, [Allowed| Rest] = List) ->
    try string:tokens(Allowed, [$:]) of
        [Host] when Port == 80 ->
            true;
        [Host, StrPort]->
            case length(StrPort) of
                N when N =< 5 ->
                    try Port == list_to_integer(StrPort) of
                        true = Result->
                            Result;
                        false ->
                            is_allowed(Host, Port, Rest)
                    catch _:_ ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            is_allowed(Host, Port, Rest)
    catch _:_ ->
            ?LOG_WARNING("CRL fetch ignored, invalid host allow list ~p ~n", [List]),
            false
    end.

host_family(Host) ->
    case inet:getaddr(Host, inet) of
        {ok, IP}  ->
            {IP, inet};
        {error, _} ->
            case inet:getaddr(Host, inet6) of
                {ok, IP} ->
                    {IP, inet6};
                {error, _} ->
                    unknown
            end
    end.

allow_external_ip(IP) ->
    case is_external_ip(IP) of
        true ->
            ok;
        false ->
            {disallowed, {local_ip_for_host, IP}}
    end.

is_external_ip(IP) ->
    not is_internal_ip(IP).

http_get(URL, IP, Family, Rest, CRLDbInfo, Timeout) ->
    case httpc:request(get, {URL, [{"connection", "close"}]},
                       [{autoredirect, false},{timeout, Timeout}],
                       [{socket_opts, [Family, {ip, IP}]},{body_format, binary}]) of
        {ok, {{_,200,_}, _Headers, Body}} when byte_size(Body) =< (?MAX_CRL_SIZE)->
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
        {ok, {Status, Headers, Body}} ->
            ?LOG_WARNING("CRL fetch ignored: ~n"
                         "HTTP status: ~p ~n"
                         "HTTP headers: ~p ~n"
                         "HTTP Body size ~p ~n",
                         [Status, Headers, byte_size(Body)]),
            get_crls(Rest, CRLDbInfo);
        {error, _Reason} ->
            get_crls(Rest, CRLDbInfo)
    end.

cache_lookup(_, undefined) ->
    [];
cache_lookup(#{path :=  Path,
               host := Host} = URI, {{Cache, _}, _}) ->
    Port = maps:get(port, URI, 80),
    Key = make_key(Host, Port, Path),
    case ssl_pkix_db:lookup(Key, Cache) of
	undefined ->
	    [];
	[CRLs] ->
	    CRLs
    end.

handle_http(URL, #{path :=  Path,
                   host := Host} = URI, Rest, {_,  Args} = CRLDbInfo) ->
    case proplists:get_value(http, Args, undefined) of
        undefined ->
            get_crls(Rest, CRLDbInfo);
        Timeout ->
            case http_lookup(URL, URI, Rest, CRLDbInfo, Timeout)  of
                not_available ->
                    not_available;
                CRLs ->
                    case proplists:get_value(owner, Args, undefined) of
                        undefined ->
                            CRLs;
                        CacheOwner ->
                            Port = maps:get(port, URI, 80),
                            Key = make_key(Host, Port, Path),
                            ssl_manager:async_insert_crls(Key, CRLs, CacheOwner),
                            CRLs
                    end
            end
    end.

make_key(Host, Port, Path) -> 
    Host ++ ":" ++ integer_to_list(Port) ++ Path.

-doc false.
-spec is_internal_ip(inet:ip_address()) -> boolean().

%% IPv4
is_internal_ip({127, _, _, _}) -> true;           %% loopback
is_internal_ip({10, _, _, _}) -> true;            %% RFC 1918
is_internal_ip({172, B, _, _}) when B >= 16, B =< 31 -> true; %% RFC 1918
is_internal_ip({192, 168, _, _}) -> true;         %% RFC 1918
is_internal_ip({169, 254, _, _}) -> true;         %% link-local
is_internal_ip({0, 0, 0, 0}) -> true;             %% unspecified
%% IPv6
is_internal_ip({0,0,0,0,0,0,0,1}) -> true;       %% ::1 loopback
is_internal_ip({0,0,0,0,0,0,0,0}) -> true;       %% :: unspecified
is_internal_ip({16#fe80,_,_,_,_,_,_,_}) -> true;  %% link-local fe80::/10
is_internal_ip({W,_,_,_,_,_,_,_}) when W >= 16#fc00, W =< 16#fdff -> true; %% unique-local
is_internal_ip(_) -> false.
