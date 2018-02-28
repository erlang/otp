%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-module(uri_string_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,
         normalize/1, normalize_map/1, normalize_return_map/1, normalize_negative/1,
         parse_binary_fragment/1, parse_binary_host/1, parse_binary_host_ipv4/1,
         parse_binary_host_ipv6/1,
         parse_binary_path/1, parse_binary_pct_encoded_fragment/1, parse_binary_pct_encoded_query/1,
         parse_binary_pct_encoded_userinfo/1, parse_binary_port/1,
         parse_binary_query/1, parse_binary_scheme/1, parse_binary_userinfo/1,
         parse_fragment/1, parse_host/1, parse_host_ipv4/1, parse_host_ipv6/1,
         parse_path/1, parse_pct_encoded_fragment/1, parse_pct_encoded_query/1,
         parse_pct_encoded_userinfo/1, parse_port/1,
         parse_query/1, parse_scheme/1, parse_userinfo/1,
	 parse_list/1, parse_binary/1, parse_mixed/1, parse_relative/1,
         parse_special/1, parse_special2/1, parse_negative/1,
         recompose_fragment/1, recompose_parse_fragment/1,
         recompose_query/1, recompose_parse_query/1,
         recompose_path/1, recompose_parse_path/1,
         recompose_autogen/1, parse_recompose_autogen/1,
         transcode_basic/1, transcode_options/1, transcode_mixed/1, transcode_negative/1,
         compose_query/1, compose_query_latin1/1, compose_query_negative/1,
         dissect_query/1, dissect_query_negative/1,
         interop_query_latin1/1, interop_query_utf8/1
        ]).


-define(SCHEME, "foo").
-define(USERINFO, "åsa").
-define(USERINFO_ENC, "%C3%A5sa").
-define(HOST, "älvsjö").
-define(HOST_ENC, "%C3%A4lvsj%C3%B6").
-define(IPV6, "::127.0.0.1").
-define(IPV6_ENC, "[::127.0.0.1]").
-define(PORT, 8042).
-define(PORT_ENC, ":8042").
-define(PATH, "/där").
-define(PATH_ENC, "/d%C3%A4r").
-define(QUERY, "name=örn").
-define(QUERY_ENC, "?name=%C3%B6rn").
-define(FRAGMENT, "näsa").
-define(FRAGMENT_ENC, "#n%C3%A4sa").


suite() ->
    [{timetrap,{minutes,1}}].

all() ->
    [
     normalize,
     normalize_map,
     normalize_return_map,
     normalize_negative,
     parse_binary_scheme,
     parse_binary_userinfo,
     parse_binary_pct_encoded_userinfo,
     parse_binary_host,
     parse_binary_host_ipv4,
     parse_binary_host_ipv6,
     parse_binary_port,
     parse_binary_path,
     parse_binary_query,
     parse_binary_pct_encoded_query,
     parse_binary_fragment,
     parse_binary_pct_encoded_fragment,
     parse_scheme,
     parse_userinfo,
     parse_pct_encoded_userinfo,
     parse_host,
     parse_host_ipv4,
     parse_host_ipv6,
     parse_port,
     parse_path,
     parse_query,
     parse_pct_encoded_query,
     parse_fragment,
     parse_pct_encoded_fragment,
     parse_list,
     parse_binary,
     parse_mixed,
     parse_relative,
     parse_special,
     parse_special2,
     parse_negative,
     recompose_fragment,
     recompose_parse_fragment,
     recompose_query,
     recompose_parse_query,
     recompose_path,
     recompose_parse_path,
     recompose_autogen,
     parse_recompose_autogen,
     transcode_basic,
     transcode_options,
     transcode_mixed,
     transcode_negative,
     compose_query,
     compose_query_latin1,
     compose_query_negative,
     dissect_query,
     dissect_query_negative,
     interop_query_latin1,
     interop_query_utf8
    ].

groups() ->
    [].


%%-------------------------------------------------------------------------
%% Helper functions
%%-------------------------------------------------------------------------
uri_combinations() ->
    [[Sch,Usr,Hst,Prt,Pat,Qry,Frg] ||
        Sch <- [fun update_scheme/1, fun update_scheme_binary/1, none],
        Usr <- [fun update_userinfo/1, fun update_userinfo_binary/1, none],
        Hst <- [fun update_host/1, fun update_host_binary/1,
                fun update_ipv6/1, fun update_ipv6_binary/1, none],
        Prt <- [fun update_port/1, none],
        Pat <- [fun update_path/1, fun update_path_binary/1],
        Qry <- [fun update_query/1,fun update_query_binary/1, none],
        Frg <- [fun update_fragment/1, fun update_fragment_binary/1, none],
        not (Usr =:= none andalso Hst =:= none andalso Prt =/= none),
        not (Usr =/= none andalso Hst =:= none andalso Prt =:= none),
        not (Usr =/= none andalso Hst =:= none andalso Prt =/= none)].


generate_test_vector(Comb) ->
    Fun = fun (F, {Map, URI}) when is_function(F) -> F({Map, URI});
              (_, Map) -> Map
          end,
    lists:foldl(Fun, {#{}, empty}, Comb).

generate_test_vectors(L) ->
    lists:map(fun generate_test_vector/1, L).

update_fragment({In, empty}) ->
    {In#{fragment => ?FRAGMENT}, ?FRAGMENT_ENC};
update_fragment({In, Out}) when is_list(Out) ->
    {In#{fragment => ?FRAGMENT}, Out ++ ?FRAGMENT_ENC};
update_fragment({In, Out}) when is_binary(Out) ->
    {In#{fragment => ?FRAGMENT}, binary_to_list(Out) ++ ?FRAGMENT_ENC}.

update_fragment_binary({In, empty}) ->
    {In#{fragment => <<?FRAGMENT/utf8>>}, <<?FRAGMENT_ENC>>};
update_fragment_binary({In, Out}) when is_list(Out) ->
    {In#{fragment => <<?FRAGMENT/utf8>>}, Out ++ ?FRAGMENT_ENC};
update_fragment_binary({In, Out}) when is_binary(Out) ->
    {In#{fragment => <<?FRAGMENT/utf8>>}, <<Out/binary,?FRAGMENT_ENC>>}.


update_query({In, empty}) ->
    {In#{query => ?QUERY}, ?QUERY_ENC};
update_query({In, Out}) when is_list(Out) ->
    {In#{query => ?QUERY}, Out ++ ?QUERY_ENC};
update_query({In, Out}) when is_binary(Out) ->
    {In#{query => ?QUERY}, binary_to_list(Out) ++ ?QUERY_ENC}.

update_query_binary({In, empty}) ->
    {In#{query => <<?QUERY/utf8>>}, <<?QUERY_ENC>>};
update_query_binary({In, Out}) when is_list(Out) ->
    {In#{query => <<?QUERY/utf8>>}, Out ++ ?QUERY_ENC};
update_query_binary({In, Out}) when is_binary(Out) ->
    {In#{query => <<?QUERY/utf8>>}, <<Out/binary,?QUERY_ENC>>}.

update_path({In, empty}) ->
    {In#{path => ?PATH}, ?PATH_ENC};
update_path({In, Out}) when is_list(Out) ->
    {In#{path => ?PATH}, Out ++ ?PATH_ENC};
update_path({In, Out}) when is_binary(Out) ->
    {In#{path => ?PATH}, binary_to_list(Out) ++ ?PATH_ENC}.

update_path_binary({In, empty}) ->
    {In#{path => <<?PATH/utf8>>}, <<?PATH_ENC>>};
update_path_binary({In, Out}) when is_list(Out) ->
    {In#{path => <<?PATH/utf8>>}, Out ++ ?PATH_ENC};
update_path_binary({In, Out}) when is_binary(Out) ->
    {In#{path => <<?PATH/utf8>>}, <<Out/binary,?PATH_ENC>>}.

update_port({In, Out}) when is_list(Out) ->
    {In#{port => ?PORT}, Out ++ ?PORT_ENC};
update_port({In, Out}) when is_binary(Out) ->
    {In#{port => ?PORT}, <<Out/binary,?PORT_ENC>>}.

update_host({In, empty}) ->
    {In#{host => ?HOST}, "//" ++ ?HOST_ENC};
update_host({In, Out}) when is_list(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => ?HOST}, Out ++ [$@|?HOST_ENC]};
        false -> {In#{host => ?HOST}, Out ++ [$/,$/|?HOST_ENC]}
    end;
update_host({In, Out}) when is_binary(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => ?HOST}, binary_to_list(Out) ++ [$@|?HOST_ENC]};
        false -> {In#{host => ?HOST}, binary_to_list(Out) ++ [$/,$/|?HOST_ENC]}
    end.

update_host_binary({In, empty}) ->
    {In#{host => <<?HOST/utf8>>}, <<"//",?HOST_ENC>>};
update_host_binary({In, Out}) when is_list(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => <<?HOST/utf8>>}, Out ++ [$@|?HOST_ENC]};
        false -> {In#{host => <<?HOST/utf8>>}, Out ++ [$/,$/|?HOST_ENC]}
    end;
update_host_binary({In, Out}) when is_binary(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => <<?HOST/utf8>>}, <<Out/binary,$@,?HOST_ENC>>};
        false-> {In#{host => <<?HOST/utf8>>}, <<Out/binary,"//",?HOST_ENC>>}
    end.

update_ipv6({In, empty}) ->
    {In#{host => ?IPV6}, "//" ++ ?IPV6_ENC};
update_ipv6({In, Out}) when is_list(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => ?IPV6}, Out ++ [$@|?IPV6_ENC]};
        false -> {In#{host => ?IPV6}, Out ++ [$/,$/|?IPV6_ENC]}
    end;
update_ipv6({In, Out}) when is_binary(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => ?IPV6}, binary_to_list(Out) ++ [$@|?IPV6_ENC]};
        false -> {In#{host => ?IPV6}, binary_to_list(Out) ++ [$/,$/|?IPV6_ENC]}
    end.

update_ipv6_binary({In, empty}) ->
    {In#{host => <<?IPV6/utf8>>}, <<"//",?IPV6_ENC>>};
update_ipv6_binary({In, Out}) when is_list(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => <<?IPV6/utf8>>}, Out ++ [$@|?IPV6_ENC]};
        false -> {In#{host => <<?IPV6/utf8>>}, Out ++ [$/,$/|?IPV6_ENC]}
    end;
update_ipv6_binary({In, Out}) when is_binary(Out) ->
    case maps:is_key(userinfo, In) of
        true -> {In#{host => <<?IPV6/utf8>>}, <<Out/binary,$@,?IPV6_ENC>>};
        false-> {In#{host => <<?IPV6/utf8>>}, <<Out/binary,"//",?IPV6_ENC>>}
    end.

update_userinfo({In, empty}) ->
    {In#{userinfo => ?USERINFO}, "//" ++ ?USERINFO_ENC};
update_userinfo({In, Out}) when is_list(Out) ->
    {In#{userinfo => ?USERINFO}, Out ++ "//" ++ ?USERINFO_ENC};
update_userinfo({In, Out}) when is_binary(Out) ->
    {In#{userinfo => ?USERINFO}, binary_to_list(Out) ++ "//" ++ ?USERINFO_ENC}.

update_userinfo_binary({In, empty}) ->
    {In#{userinfo => <<?USERINFO/utf8>>}, <<"//",?USERINFO_ENC>>};
update_userinfo_binary({In, Out}) when is_list(Out) ->
    {In#{userinfo => <<?USERINFO/utf8>>}, Out ++ "//" ++ ?USERINFO_ENC};
update_userinfo_binary({In, Out}) when is_binary(Out) ->
    {In#{userinfo => <<?USERINFO/utf8>>}, <<Out/binary,"//",?USERINFO_ENC>>}.

update_scheme({In, empty}) ->
    {In#{scheme => ?SCHEME}, ?SCHEME ++ ":"}.

update_scheme_binary({In, empty}) ->
    {In#{scheme => <<?SCHEME/utf8>>}, <<?SCHEME,$:>>}.


%% Test recompose on a generated test vector
run_test_recompose({#{}, empty}) ->
    try "" = uri_string:recompose(#{}) of
        _ -> ok
    catch
        _:_ -> error({test_failed, #{}, ""})
    end;
run_test_recompose({Map, URI}) ->
    try URI = uri_string:recompose(Map) of
        URI -> ok
    catch
        _:_ -> error({test_failed, Map, URI})
    end.

%% Test parse - recompose on a generated test vector
run_test_parse_recompose({#{}, empty}) ->
    try "" = uri_string:recompose(uri_string:parse("")) of
        _ -> ok
    catch
        _:_ -> error({test_failed, #{}, ""})
    end;
run_test_parse_recompose({Map, URI}) ->
    try URI = uri_string:recompose(uri_string:parse(URI)) of
        URI -> ok
    catch
        _:_ -> error({test_failed, Map, URI})
    end.


%%-------------------------------------------------------------------------
%% Parse tests
%%-------------------------------------------------------------------------

parse_binary_scheme(_Config) ->
    #{} = uri_string:parse(<<>>),
    #{path := <<"foo">>} = uri_string:parse(<<"foo">>),
    #{scheme := <<"foo">>} = uri_string:parse(<<"foo:">>),
    #{scheme := <<"foo">>, path := <<"bar:nisse">>} = uri_string:parse(<<"foo:bar:nisse">>),
    #{scheme := <<"foo">>, host := <<"">>} = uri_string:parse(<<"foo://">>),
    #{scheme := <<"foo">>, host := <<"">>, path := <<"/">>} = uri_string:parse(<<"foo:///">>),
    #{scheme := <<"foo">>, host := <<"">>, path := <<"//">>} = uri_string:parse(<<"foo:////">>),

    #{path := <<"/">>} = uri_string:parse(<<"/">>),
    #{host := <<>>} = uri_string:parse(<<"//">>),
    #{host := <<>>, path := <<"/">>} = uri_string:parse(<<"///">>).

parse_binary_userinfo(_Config) ->
    #{scheme := <<"user">>, path := <<"password@localhost">>} =
        uri_string:parse(<<"user:password@localhost">>),
    #{path := <<"user@">>} = uri_string:parse(<<"user@">>),
    #{path := <<"/user@">>} = uri_string:parse(<<"/user@">>),
    #{path := <<"user@localhost">>} = uri_string:parse(<<"user@localhost">>),
    #{userinfo := <<"user">>, host := <<"localhost">>} = uri_string:parse(<<"//user@localhost">>),
    #{userinfo := <<"user:password">>, host := <<"localhost">>} =
        uri_string:parse(<<"//user:password@localhost">>),
    #{scheme := <<"foo">>, path := <<"/user@">>} =
        uri_string:parse(<<"foo:/user@">>),
    #{scheme := <<"foo">>, userinfo := <<"user">>, host := <<"localhost">>} =
        uri_string:parse(<<"foo://user@localhost">>),
    #{scheme := <<"foo">>, userinfo := <<"user:password">>, host := <<"localhost">>} =
        uri_string:parse(<<"foo://user:password@localhost">>).

parse_binary_pct_encoded_userinfo(_Config) ->
    #{scheme := <<"user">>, path := <<"合@気道"/utf8>>} =
        uri_string:parse(<<"user:%E5%90%88@%E6%B0%97%E9%81%93">>),
    #{path := <<"合気道@"/utf8>>} = uri_string:parse(<<"%E5%90%88%E6%B0%97%E9%81%93@">>),
    #{path := <<"/合気道@"/utf8>>} = uri_string:parse(<<"/%E5%90%88%E6%B0%97%E9%81%93@">>),
    #{path := <<"合@気道"/utf8>>} = uri_string:parse(<<"%E5%90%88@%E6%B0%97%E9%81%93">>),
    #{userinfo := <<"合"/utf8>>, host := <<"気道"/utf8>>} =
        uri_string:parse(<<"//%E5%90%88@%E6%B0%97%E9%81%93">>),
    #{userinfo := <<"合:気"/utf8>>, host := <<"道"/utf8>>} =
        uri_string:parse(<<"//%E5%90%88:%E6%B0%97@%E9%81%93">>),
    #{scheme := <<"foo">>, path := <<"/合気道@"/utf8>>} =
        uri_string:parse(<<"foo:/%E5%90%88%E6%B0%97%E9%81%93@">>),
    #{scheme := <<"foo">>, userinfo := <<"合"/utf8>>, host := <<"気道"/utf8>>} =
        uri_string:parse(<<"foo://%E5%90%88@%E6%B0%97%E9%81%93">>),
    #{scheme := <<"foo">>, userinfo := <<"合:気"/utf8>>, host := <<"道"/utf8>>} =
        uri_string:parse(<<"foo://%E5%90%88:%E6%B0%97@%E9%81%93">>),
    {error,invalid_uri,"@"} = uri_string:parse(<<"//%E5%90%88@%E6%B0%97%E9%81%93@">>),
    {error,invalid_uri,":"} = uri_string:parse(<<"foo://%E5%90%88@%E6%B0%97%E9%81%93@">>).

parse_binary_host(_Config) ->
    #{host := <<"hostname">>} = uri_string:parse(<<"//hostname">>),
    #{host := <<"hostname">>,scheme := <<"foo">>} = uri_string:parse(<<"foo://hostname">>),
    #{host := <<"hostname">>,scheme := <<"foo">>, userinfo := <<"user">>} =
        uri_string:parse(<<"foo://user@hostname">>).

parse_binary_host_ipv4(_Config) ->
    #{host := <<"127.0.0.1">>} = uri_string:parse(<<"//127.0.0.1">>),
    #{host := <<"127.0.0.1">>, path := <<"/over/there">>} =
        uri_string:parse(<<"//127.0.0.1/over/there">>),
    #{host := <<"127.0.0.1">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"//127.0.0.1?name=ferret">>),
    #{host := <<"127.0.0.1">>, fragment := <<"nose">>} = uri_string:parse(<<"//127.0.0.1#nose">>),
    {error,invalid_uri,"x"} = uri_string:parse(<<"//127.0.0.x">>),
    {error,invalid_uri,"1227.0.0.1"} = uri_string:parse(<<"//1227.0.0.1">>).

parse_binary_host_ipv6(_Config) ->
    #{host := <<"::127.0.0.1">>} = uri_string:parse(<<"//[::127.0.0.1]">>),
    #{host := <<"2001:0db8:0000:0000:0000:0000:1428:07ab">>} =
        uri_string:parse(<<"//[2001:0db8:0000:0000:0000:0000:1428:07ab]">>),
    #{host := <<"::127.0.0.1">>, path := <<"/over/there">>} =
        uri_string:parse(<<"//[::127.0.0.1]/over/there">>),
    #{host := <<"::127.0.0.1">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"//[::127.0.0.1]?name=ferret">>),
    #{host := <<"::127.0.0.1">>, fragment := <<"nose">>} =
        uri_string:parse(<<"//[::127.0.0.1]#nose">>),
    {error,invalid_uri,"x"} = uri_string:parse(<<"//[::127.0.0.x]">>),
    {error,invalid_uri,"::1227.0.0.1"} = uri_string:parse(<<"//[::1227.0.0.1]">>),
    {error,invalid_uri,"G"} = uri_string:parse(<<"//[2001:0db8:0000:0000:0000:0000:1428:G7ab]">>).

parse_binary_port(_Config) ->
    #{path:= <<"/:8042">>} =
        uri_string:parse(<<"/:8042">>),
    #{host:= <<>>, port := 8042} =
        uri_string:parse(<<"//:8042">>),
    #{host := <<"example.com">>, port:= 8042} =
        uri_string:parse(<<"//example.com:8042">>),
    #{scheme := <<"foo">>, path := <<"/:8042">>} =
        uri_string:parse(<<"foo:/:8042">>),
    #{scheme := <<"foo">>, host := <<>>, port := 8042} =
        uri_string:parse(<<"foo://:8042">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, port := 8042} =
        uri_string:parse(<<"foo://example.com:8042">>),
    {error,invalid_uri,":"} = uri_string:parse(":600"),
    {error,invalid_uri,"x"} = uri_string:parse("//:8042x").

parse_binary_path(_Config) ->
    #{path := <<"over/there">>} = uri_string:parse(<<"over/there">>),
    #{path := <<"/over/there">>} = uri_string:parse(<<"/over/there">>),
    #{scheme := <<"foo">>, path := <<"/over/there">>} =
        uri_string:parse(<<"foo:/over/there">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/over/there">>} =
        uri_string:parse(<<"foo://example.com/over/there">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/over/there">>, port := 8042} =
        uri_string:parse(<<"foo://example.com:8042/over/there">>).

parse_binary_query(_Config) ->
    #{scheme := <<"foo">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"foo:?name=ferret">>),
    #{scheme := <<"foo">>, path:= <<"over/there">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"foo:over/there?name=ferret">>),
    #{scheme := <<"foo">>, path:= <<"/over/there">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"foo:/over/there?name=ferret">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"foo://example.com?name=ferret">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"foo://example.com/?name=ferret">>),

    #{path := <<>>, query := <<"name=ferret">>} =
        uri_string:parse(<<"?name=ferret">>),
    #{path := <<"over/there">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"over/there?name=ferret">>),
    #{path := <<"/">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"/?name=ferret">>),
    #{path := <<"/over/there">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"/over/there?name=ferret">>),
    #{host := <<"example.com">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"//example.com?name=ferret">>),
    #{host := <<"example.com">>, path := <<"/">>, query := <<"name=ferret">>} =
        uri_string:parse(<<"//example.com/?name=ferret">>).

parse_binary_pct_encoded_query(_Config) ->
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/">>,
      query := <<"name=合気道"/utf8>>} =
        uri_string:parse(<<"foo://example.com/?name=%E5%90%88%E6%B0%97%E9%81%93">>),
    #{host := <<"example.com">>, path := <<"/">>, query := <<"name=合気道"/utf8>>} =
        uri_string:parse(<<"//example.com/?name=%E5%90%88%E6%B0%97%E9%81%93">>).

parse_binary_fragment(_Config) ->
    #{scheme := <<"foo">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo:#nose">>),
    #{scheme := <<"foo">>, path:= <<"over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo:over/there#nose">>),
    #{scheme := <<"foo">>, path:= <<"/over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo:/over/there#nose">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com#nose">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, path := <<"/">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com/#nose">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com#nose">>),

    #{fragment := <<"nose">>} =
        uri_string:parse(<<"#nose">>),
    #{path := <<"over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"over/there#nose">>),
    #{path := <<"/">>, fragment := <<"nose">>} =
        uri_string:parse(<<"/#nose">>),
    #{path := <<"/over/there">>, fragment := <<"nose">>} =
        uri_string:parse(<<"/over/there#nose">>),
    #{host := <<"example.com">>, fragment := <<"nose">>} =
        uri_string:parse(<<"//example.com#nose">>),
    #{host := <<"example.com">>, path := <<"/">>, fragment := <<"nose">>} =
        uri_string:parse(<<"//example.com/#nose">>).

parse_binary_pct_encoded_fragment(_Config) ->
    #{scheme := <<"foo">>, host := <<"example.com">>, fragment := <<"合気道"/utf8>>} =
        uri_string:parse(<<"foo://example.com#%E5%90%88%E6%B0%97%E9%81%93">>),
    #{host := <<"example.com">>, path := <<"/">>, fragment := <<"合気道"/utf8>>} =
        uri_string:parse(<<"//example.com/#%E5%90%88%E6%B0%97%E9%81%93">>).

parse_scheme(_Config) ->
    #{} = uri_string:parse(""),
    #{path := "foo"} = uri_string:parse("foo"),
    #{scheme := "foo"} = uri_string:parse("foo:"),
    #{scheme := "foo", path := "bar:nisse"} = uri_string:parse("foo:bar:nisse"),
    #{scheme := "foo", host := ""} = uri_string:parse("foo://"),
    #{scheme := "foo", host := "", path := "/"} = uri_string:parse("foo:///"),
    #{scheme := "foo", host := "", path := "//"} = uri_string:parse("foo:////"),

    #{path := "/"} = uri_string:parse("/"),
    #{host := ""} = uri_string:parse("//"),
    #{host := "", path := "/"} = uri_string:parse("///").

parse_userinfo(_Config) ->
    #{scheme := "user", path := "password@localhost"} = uri_string:parse("user:password@localhost"),
    #{path := "user@"} = uri_string:parse("user@"),
    #{path := "/user@"} = uri_string:parse("/user@"),
    #{path := "user@localhost"} = uri_string:parse("user@localhost"),
    #{userinfo := "user", host := "localhost"} = uri_string:parse("//user@localhost"),
    #{userinfo := "user:password", host := "localhost"} =
        uri_string:parse("//user:password@localhost"),
    #{scheme := "foo", path := "/user@"} =
        uri_string:parse("foo:/user@"),
    #{scheme := "foo", userinfo := "user", host := "localhost"} =
        uri_string:parse("foo://user@localhost"),
    #{scheme := "foo", userinfo := "user:password", host := "localhost"} =
        uri_string:parse("foo://user:password@localhost").

parse_pct_encoded_userinfo(_Config) ->
    #{scheme := "user", path := "合@気道"} =
        uri_string:parse("user:%E5%90%88@%E6%B0%97%E9%81%93"),
    #{path := "合気道@"} = uri_string:parse("%E5%90%88%E6%B0%97%E9%81%93@"),
    #{path := "/合気道@"} = uri_string:parse("/%E5%90%88%E6%B0%97%E9%81%93@"),
    #{path := "合@気道"} = uri_string:parse("%E5%90%88@%E6%B0%97%E9%81%93"),
    #{userinfo := "合", host := "気道"} =
        uri_string:parse("//%E5%90%88@%E6%B0%97%E9%81%93"),
    #{userinfo := "合:気", host := "道"} =
        uri_string:parse("//%E5%90%88:%E6%B0%97@%E9%81%93"),
    #{scheme := "foo", path := "/合気道@"} =
        uri_string:parse("foo:/%E5%90%88%E6%B0%97%E9%81%93@"),
    #{scheme := "foo", userinfo := "合", host := "気道"} =
        uri_string:parse("foo://%E5%90%88@%E6%B0%97%E9%81%93"),
    #{scheme := "foo", userinfo := "合:気", host := "道"} =
        uri_string:parse("foo://%E5%90%88:%E6%B0%97@%E9%81%93"),
    {error,invalid_uri,"@"} = uri_string:parse("//%E5%90%88@%E6%B0%97%E9%81%93@"),
    {error,invalid_uri,":"} = uri_string:parse("foo://%E5%90%88@%E6%B0%97%E9%81%93@").


parse_host(_Config) ->
    #{host := "hostname"} = uri_string:parse("//hostname"),
    #{host := "hostname",scheme := "foo"} = uri_string:parse("foo://hostname"),
    #{host := "hostname",scheme := "foo", userinfo := "user"} =
        uri_string:parse("foo://user@hostname").

parse_host_ipv4(_Config) ->
    #{host := "127.0.0.1"} = uri_string:parse("//127.0.0.1"),
    #{host := "2001:0db8:0000:0000:0000:0000:1428:07ab"} =
        uri_string:parse("//[2001:0db8:0000:0000:0000:0000:1428:07ab]"),
    #{host := "127.0.0.1", path := "/over/there"} = uri_string:parse("//127.0.0.1/over/there"),
    #{host := "127.0.0.1", query := "name=ferret"} = uri_string:parse("//127.0.0.1?name=ferret"),
    #{host := "127.0.0.1", fragment := "nose"} = uri_string:parse("//127.0.0.1#nose"),
    {error,invalid_uri,"x"} = uri_string:parse("//127.0.0.x"),
    {error,invalid_uri,"1227.0.0.1"} = uri_string:parse("//1227.0.0.1").

parse_host_ipv6(_Config) ->
    #{host := "::127.0.0.1"} = uri_string:parse("//[::127.0.0.1]"),
    #{host := "::127.0.0.1", path := "/over/there"} = uri_string:parse("//[::127.0.0.1]/over/there"),
    #{host := "::127.0.0.1", query := "name=ferret"} =
        uri_string:parse("//[::127.0.0.1]?name=ferret"),
    #{host := "::127.0.0.1", fragment := "nose"} = uri_string:parse("//[::127.0.0.1]#nose"),
    {error,invalid_uri,"x"} = uri_string:parse("//[::127.0.0.x]"),
    {error,invalid_uri,"::1227.0.0.1"} = uri_string:parse("//[::1227.0.0.1]"),
    {error,invalid_uri,"G"} = uri_string:parse("//[2001:0db8:0000:0000:0000:0000:1428:G7ab]").

parse_port(_Config) ->
    #{path:= "/:8042"} =
        uri_string:parse("/:8042"),
    #{host:= "", port := 8042} =
        uri_string:parse("//:8042"),
    #{host := "example.com", port:= 8042} =
        uri_string:parse("//example.com:8042"),
    #{scheme := "foo", path := "/:8042"} =
        uri_string:parse("foo:/:8042"),
    #{scheme := "foo", host := "", port := 8042} =
        uri_string:parse("foo://:8042"),
    #{scheme := "foo", host := "example.com", port := 8042} =
        uri_string:parse("foo://example.com:8042").

parse_path(_Config) ->
    #{path := "over/there"} = uri_string:parse("over/there"),
    #{path := "/over/there"} = uri_string:parse("/over/there"),
    #{scheme := "foo", path := "/over/there"} =
        uri_string:parse("foo:/over/there"),
    #{scheme := "foo", host := "example.com", path := "/over/there"} =
        uri_string:parse("foo://example.com/over/there"),
    #{scheme := "foo", host := "example.com", path := "/over/there", port := 8042} =
        uri_string:parse("foo://example.com:8042/over/there").

parse_query(_Config) ->
    #{scheme := "foo", query := "name=ferret"} =
        uri_string:parse("foo:?name=ferret"),
    #{scheme := "foo", path:= "over/there", query := "name=ferret"} =
        uri_string:parse("foo:over/there?name=ferret"),
    #{scheme := "foo", path:= "/over/there", query := "name=ferret"} =
        uri_string:parse("foo:/over/there?name=ferret"),
    #{scheme := "foo", host := "example.com", query := "name=ferret"} =
        uri_string:parse("foo://example.com?name=ferret"),
    #{scheme := "foo", host := "example.com", path := "/", query := "name=ferret"} =
        uri_string:parse("foo://example.com/?name=ferret"),

    #{path := "", query := "name=ferret"} =
        uri_string:parse("?name=ferret"),
    #{path := "over/there", query := "name=ferret"} =
        uri_string:parse("over/there?name=ferret"),
    #{path := "/", query := "name=ferret"} =
        uri_string:parse("/?name=ferret"),
    #{path := "/over/there", query := "name=ferret"} =
        uri_string:parse("/over/there?name=ferret"),
    #{host := "example.com", query := "name=ferret"} =
        uri_string:parse("//example.com?name=ferret"),
    #{host := "example.com", path := "/", query := "name=ferret"} =
        uri_string:parse("//example.com/?name=ferret").

parse_pct_encoded_query(_Config) ->
    #{scheme := "foo", host := "example.com", path := "/",
      query := "name=合気道"} =
        uri_string:parse("foo://example.com/?name=%E5%90%88%E6%B0%97%E9%81%93"),
    #{host := "example.com", path := "/", query := "name=合気道"} =
        uri_string:parse("//example.com/?name=%E5%90%88%E6%B0%97%E9%81%93").

parse_fragment(_Config) ->
    #{scheme := "foo", fragment := "nose"} =
        uri_string:parse("foo:#nose"),
    #{scheme := "foo", path:= "over/there", fragment := "nose"} =
        uri_string:parse("foo:over/there#nose"),
    #{scheme := "foo", path:= "/over/there", fragment := "nose"} =
        uri_string:parse("foo:/over/there#nose"),
    #{scheme := "foo", host := "example.com", fragment := "nose"} =
        uri_string:parse("foo://example.com#nose"),
    #{scheme := "foo", host := "example.com", path := "/", fragment := "nose"} =
        uri_string:parse("foo://example.com/#nose"),
    #{scheme := "foo", host := "example.com", fragment := "nose"} =
        uri_string:parse("foo://example.com#nose"),

    #{fragment := "nose"} =
        uri_string:parse("#nose"),
    #{path := "over/there", fragment := "nose"} =
        uri_string:parse("over/there#nose"),
    #{path := "/", fragment := "nose"} =
        uri_string:parse("/#nose"),
    #{path := "/over/there", fragment := "nose"} =
        uri_string:parse("/over/there#nose"),
    #{host := "example.com", fragment := "nose"} =
        uri_string:parse("//example.com#nose"),
    #{host := "example.com", path := "/", fragment := "nose"} =
        uri_string:parse("//example.com/#nose").

parse_pct_encoded_fragment(_Config) ->
    #{scheme := "foo", host := "example.com", fragment := "合気道"} =
        uri_string:parse("foo://example.com#%E5%90%88%E6%B0%97%E9%81%93"),
    #{host := "example.com", path := "/", fragment := "合気道"} =
        uri_string:parse("//example.com/#%E5%90%88%E6%B0%97%E9%81%93").

parse_list(_Config) ->
    #{scheme := "foo", path := "bar:nisse"} = uri_string:parse("foo:bar:nisse"),
    #{scheme := "foo", host := "example.com", port := 8042,
      path := "/over/there", query := "name=ferret", fragment := "nose"} =
        uri_string:parse("foo://example.com:8042/over/there?name=ferret#nose"),
    #{scheme := "foo", userinfo := "admin:admin", host := "example.com", port := 8042,
      path := "/over/there", query := "name=ferret", fragment := "nose"} =
        uri_string:parse("foo://admin:admin@example.com:8042/over/there?name=ferret#nose").

parse_binary(_Config) ->
    #{scheme := <<"foo">>, path := <<"bar:nisse">>} = uri_string:parse(<<"foo:bar:nisse">>),
    #{scheme := <<"foo">>, host := <<"example.com">>, port := 8042,
      path := <<"/over/there">>, query := <<"name=ferret">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://example.com:8042/over/there?name=ferret#nose">>),
    #{scheme := <<"foo">>, userinfo := <<"admin:admin">>, host := <<"example.com">>, port := 8042,
      path := <<"/over/there">>, query := <<"name=ferret">>, fragment := <<"nose">>} =
        uri_string:parse(<<"foo://admin:admin@example.com:8042/over/there?name=ferret#nose">>).


parse_mixed(_Config) ->
    #{scheme := "foo", path := "bar"} =
        uri_string:parse(lists:append("fo",<<"o:bar">>)),
    #{scheme := "foo", path := "bar"} =
        uri_string:parse(lists:append("foo:b",<<"ar">>)),
    #{scheme := "foo", path := "bar:bar"} =
        uri_string:parse([[102],[111,111],<<":bar">>,58,98,97,114]).

parse_relative(_Config) ->
    #{path := "/path"} =
        uri_string:parse(lists:append("/pa",<<"th">>)),
    #{path := "foo"} =
        uri_string:parse(lists:append("fo",<<"o">>)).

parse_special(_Config) ->
    #{host := [],query := []} = uri_string:parse("//?"),
    #{fragment := [],host := []} = uri_string:parse("//#"),
    #{host := [],query := [],scheme := "foo"} = uri_string:parse("foo://?"),
    #{fragment := [],host := [],scheme := "foo"} = uri_string:parse("foo://#"),
    #{host := <<>>, path := <<"/">>} = uri_string:parse(<<"///">>),
    #{host := <<"hostname">>} = uri_string:parse(<<"//hostname">>),
    #{host := <<>>, path := <<"/hostname">>} = uri_string:parse(<<"///hostname">>),
    #{host :=  [],path := "/",query := []} = uri_string:parse("///?"),
    #{fragment := [],host := [],path := "/"} = uri_string:parse("///#"),
    #{host := "foo",query := []} = uri_string:parse("//foo?"),
    #{fragment := [],host := "foo"} = uri_string:parse("//foo#"),
    #{host := "foo",path := "/"} = uri_string:parse("//foo/"),
    #{host := "foo",query := [],scheme := "http"} = uri_string:parse("http://foo?"),
    #{fragment := [],host := "foo",scheme := "http"} = uri_string:parse("http://foo#"),
    #{host := "foo",path := "/",scheme := "http"} = uri_string:parse("http://foo/"),
    #{fragment := [],host := "host",port := 80,scheme := "http"} = uri_string:parse("http://host:80#"),
    #{host := "host",port := 80,query := [],scheme := "http"} = uri_string:parse("http://host:80?"),
    #{path := [],query := []} = uri_string:parse("?"),
    #{path := [],query := "?"} = uri_string:parse("??"),
    #{path := [],query := "??"} = uri_string:parse("???").

parse_special2(_Config) ->
    #{host := [],path := "/",port := 1,scheme := "a"} = uri_string:parse("a://:1/"),
    #{path := "/a/",scheme := "a"} = uri_string:parse("a:/a/"),
    #{host := [],path := [],userinfo := []} = uri_string:parse("//@"),
    #{host := [],path := [],scheme := "foo",userinfo := []} = uri_string:parse("foo://@"),
    #{host := [],path := "/",userinfo := []} = uri_string:parse("//@/"),
    #{host := [],path := "/",scheme := "foo",userinfo := []} = uri_string:parse("foo://@/"),
    #{host := "localhost",path := "/",port := undefined} = uri_string:parse("//localhost:/"),
    #{host := [],path := [],port := undefined} = uri_string:parse("//:").

parse_negative(_Config) ->
    {error,invalid_uri,"å"} = uri_string:parse("å"),
    {error,invalid_uri,"å"} = uri_string:parse("aå:/foo"),
    {error,invalid_uri,":"} = uri_string:parse("foo://usär@host"),
    {error,invalid_uri,"ö"} = uri_string:parse("//host/path?foö=bar"),
    {error,invalid_uri,"ö"} = uri_string:parse("//host/path#foö"),
    {error,invalid_uri,"127.256.0.1"} = uri_string:parse("//127.256.0.1"),
    {error,invalid_uri,":::127.0.0.1"} = uri_string:parse("//[:::127.0.0.1]"),
    {error,invalid_utf8,<<0,0,0,246>>} = uri_string:parse("//%00%00%00%F6"),
    {error,invalid_uri,"A"} = uri_string:parse("//localhost:A8").


%%-------------------------------------------------------------------------
%% Recompose tests
%%-------------------------------------------------------------------------
recompose_fragment(_Config) ->
    <<?FRAGMENT_ENC>> = uri_string:recompose(#{fragment => <<?FRAGMENT/utf8>>, path => <<>>}),
    ?FRAGMENT_ENC = uri_string:recompose(#{fragment => ?FRAGMENT, path => ""}).

recompose_parse_fragment(_Config) ->
    <<?FRAGMENT_ENC>> = uri_string:recompose(uri_string:parse(<<?FRAGMENT_ENC>>)),
    ?FRAGMENT_ENC = uri_string:recompose(uri_string:parse(?FRAGMENT_ENC)).

recompose_query(_Config) ->
    <<?QUERY_ENC>> =
        uri_string:recompose(#{query => <<?QUERY/utf8>>, path => <<>>}),
    <<?QUERY_ENC?FRAGMENT_ENC>> =
        uri_string:recompose(#{query => <<?QUERY/utf8>>,
                               fragment => <<?FRAGMENT/utf8>>,
                               path => <<>>}),
    "?name=%C3%B6rn" =
        uri_string:recompose(#{query => "name=örn", path => ""}),
    "?name=%C3%B6rn#n%C3%A4sa" =
        uri_string:recompose(#{query => "name=örn",
                               fragment => "näsa",
                               path => ""}).

recompose_parse_query(_Config) ->
    <<"?name=%C3%B6rn">> = uri_string:recompose(uri_string:parse(<<"?name=%C3%B6rn">>)),
    <<"?name=%C3%B6rn#n%C3%A4sa">> =
        uri_string:recompose(uri_string:parse(<<"?name=%C3%B6rn#n%C3%A4sa">>)),
    "?name=%C3%B6rn" = uri_string:recompose(uri_string:parse("?name=%C3%B6rn")),
    "?name=%C3%B6rn#n%C3%A4sa" = uri_string:recompose(uri_string:parse("?name=%C3%B6rn#n%C3%A4sa")).

recompose_path(_Config) ->
    <<"/d%C3%A4r">> =
        uri_string:recompose(#{path => <<"/där"/utf8>>}),
    <<"/d%C3%A4r#n%C3%A4sa">> =
        uri_string:recompose(#{path => <<"/där"/utf8>>,
                               fragment => <<"näsa"/utf8>>}),
    <<"/d%C3%A4r?name=%C3%B6rn">> =
        uri_string:recompose(#{path => <<"/där"/utf8>>,
                               query => <<"name=örn"/utf8>>}),
    <<"/d%C3%A4r?name=%C3%B6rn#n%C3%A4sa">> =
        uri_string:recompose(#{path => <<"/där"/utf8>>,
                               query => <<"name=örn"/utf8>>,
                               fragment => <<"näsa"/utf8>>}),


    "/d%C3%A4r" =
        uri_string:recompose(#{path => "/där"}),
    "/d%C3%A4r#n%C3%A4sa" =
        uri_string:recompose(#{path => "/där",
                               fragment => "näsa"}),
    "/d%C3%A4r?name=%C3%B6rn" =
        uri_string:recompose(#{path => "/där",
                               query => "name=örn"}),
    "/d%C3%A4r?name=%C3%B6rn#n%C3%A4sa" =
        uri_string:recompose(#{path => "/där",
                               query => "name=örn",
                               fragment => "näsa"}).


recompose_parse_path(_Config) ->
    <<"/d%C3%A4r">> =
        uri_string:recompose(uri_string:parse(<<"/d%C3%A4r">>)),
    <<"/d%C3%A4r#n%C3%A4sa">> =
        uri_string:recompose(uri_string:parse(<<"/d%C3%A4r#n%C3%A4sa">>)),
    <<"/d%C3%A4r?name=%C3%B6rn">> =
        uri_string:recompose(uri_string:parse(<<"/d%C3%A4r?name=%C3%B6rn">>)),

    "/d%C3%A4r" =
        uri_string:recompose(uri_string:parse("/d%C3%A4r")),
    "/d%C3%A4r#n%C3%A4sa" =
        uri_string:recompose(uri_string:parse("/d%C3%A4r#n%C3%A4sa")),
    "/d%C3%A4r?name=%C3%B6rn" =
        uri_string:recompose(uri_string:parse("/d%C3%A4r?name=%C3%B6rn")).


recompose_autogen(_Config) ->
    Tests = generate_test_vectors(uri_combinations()),
    lists:map(fun run_test_recompose/1, Tests).

parse_recompose_autogen(_Config) ->
    Tests = generate_test_vectors(uri_combinations()),
    lists:map(fun run_test_parse_recompose/1, Tests).

transcode_basic(_Config) ->
    <<"foo%C3%B6bar"/utf8>> =
        uri_string:transcode(<<"foo%00%00%00%F6bar"/utf32>>, [{in_encoding, utf32},{out_encoding, utf8}]),
    "foo%C3%B6bar" =
        uri_string:transcode("foo%00%00%00%F6bar", [{in_encoding, utf32},{out_encoding, utf8}]),
    <<"foo%00%00%00%F6bar"/utf32>> =
        uri_string:transcode(<<"foo%C3%B6bar"/utf8>>, [{in_encoding, utf8},{out_encoding, utf32}]),
    "foo%00%00%00%F6bar" =
        uri_string:transcode("foo%C3%B6bar", [{in_encoding, utf8},{out_encoding, utf32}]),
    "foo%C3%B6bar" =
        uri_string:transcode("foo%F6bar", [{in_encoding, latin1},{out_encoding, utf8}]).

transcode_options(_Config) ->
    <<"foo%C3%B6bar"/utf8>> =
        uri_string:transcode(<<"foo%C3%B6bar"/utf8>>, []),
    <<"foo%C3%B6bar"/utf8>> =
        uri_string:transcode(<<"foo%00%00%00%F6bar"/utf32>>, [{in_encoding, utf32}]),
    <<"foo%00%00%00%F6bar"/utf32>> =
        uri_string:transcode(<<"foo%C3%B6bar"/utf8>>, [{out_encoding, utf32}]).

transcode_mixed(_Config) ->
    "foo%00%00%00%F6bar" =
        uri_string:transcode(["foo",<<"%C3%B6"/utf8>>,<<"ba"/utf8>>,"r"], [{out_encoding, utf32}]),
    "foo%00%00%00%F6bar" =
        uri_string:transcode(["foo",<<"%C3%"/utf8>>,<<"B6ba"/utf8>>,"r"], [{out_encoding, utf32}]),
    "foo%C3%B6bar" =
        uri_string:transcode(["foo%00", <<"%00%0"/utf32>>,<<"0%F"/utf32>>,"6bar"], [{in_encoding, utf32},{out_encoding, utf8}]).

transcode_negative(_Config) ->
    {error,invalid_percent_encoding,"%BXbar"} =
        uri_string:transcode(<<"foo%C3%BXbar"/utf8>>, [{in_encoding, utf8},{out_encoding, utf32}]),
    {error,invalid_input,<<"ö">>} =
        uri_string:transcode("foo%F6bar", [{in_encoding, utf8},{out_encoding, utf8}]).

compose_query(_Config) ->
    [] = uri_string:compose_query([]),
    "foo=1&bar=2" = uri_string:compose_query([{<<"foo">>,"1"}, {"bar", "2"}]),
    "foo=1&b%C3%A4r=2" = uri_string:compose_query([{"foo","1"}, {"bär", "2"}],[{encoding,utf8}]),
    "foo=1&b%C3%A4r=2" = uri_string:compose_query([{"foo","1"}, {"bär", "2"}],[{encoding,unicode}]),
    "foo=1&b%E4r=2" = uri_string:compose_query([{"foo","1"}, {"bär", "2"}],[{encoding,latin1}]),
    "foo+bar=1&%E5%90%88=2" = uri_string:compose_query([{"foo bar","1"}, {"合", "2"}]),
    "foo+bar=1&%26%2321512%3B=2" =
        uri_string:compose_query([{"foo bar","1"}, {"合", "2"}],[{encoding,latin1}]),
    "foo+bar=1&%C3%B6=2" = uri_string:compose_query([{<<"foo bar">>,<<"1">>}, {"ö", <<"2">>}]),
    <<"foo+bar=1&%C3%B6=2">> =
        uri_string:compose_query([{<<"foo bar">>,<<"1">>}, {<<"ö"/utf8>>, <<"2">>}]).

compose_query_latin1(_Config) ->
    Q = uri_string:compose_query([{"合foö bar","1"}, {"合", "合"}],[{encoding,latin1}]),
    Q1 = uri_string:transcode(Q, [{in_encoding, latin1}]),
    [{"合foö bar","1"}, {"合", "合"}] = uri_string:dissect_query(Q1),
    Q2 = uri_string:compose_query([{<<"合foö bar"/utf8>>,<<"1">>}, {<<"合"/utf8>>, <<"合"/utf8>>}],
                                  [{encoding,latin1}]),
    Q3 = uri_string:transcode(Q2, [{in_encoding, latin1}]),
    [{<<"合foö bar"/utf8>>,<<"1">>}, {<<"合"/utf8>>, <<"合"/utf8>>}] =
        uri_string:dissect_query(Q3).

compose_query_negative(_Config) ->
    {error,invalid_input,4} = uri_string:compose_query([{"",4}]),
    {error,invalid_input,5} = uri_string:compose_query([{5,""}]),
    {error,invalid_encoding,utf16} =
        uri_string:compose_query([{"foo bar","1"}, {<<"ö">>, "2"}],[{encoding,utf16}]).

dissect_query(_Config) ->
    [] = uri_string:dissect_query(""),
    [{"foo","1"}, {"amp;bar", "2"}] = uri_string:dissect_query("foo=1&amp;bar=2"),
    [{"foo","1"}, {"bar", "2"}] = uri_string:dissect_query("foo=1&bar=2"),
    [{"foo","1;bar=2"}] = uri_string:dissect_query("foo=1;bar=2"),
    [{"foo","1"}, {"bar", "222"}] = uri_string:dissect_query([<<"foo=1&bar=2">>,"22"]),
    [{"foo","ö"}, {"bar", "2"}] = uri_string:dissect_query("foo=%C3%B6&bar=2"),
    [{<<"foo">>,<<"ö"/utf8>>}, {<<"bar">>, <<"2">>}] =
        uri_string:dissect_query(<<"foo=%C3%B6&bar=2">>),
    [{"foo bar","1"},{"ö","2"}] =
        uri_string:dissect_query([<<"foo+bar=1&">>,<<"%C3%B6=2">>]),
    [{"foo bar","1"},{[21512],"2"}] =
        uri_string:dissect_query("foo+bar=1&%26%2321512%3B=2"),
    [{<<"foo bar">>,<<"1">>},{<<"合"/utf8>>,<<"2">>}] =
        uri_string:dissect_query(<<"foo+bar=1&%26%2321512%3B=2">>),
    [{"föo bar","1"},{"ö","2"}] =
        uri_string:dissect_query("föo+bar=1&%C3%B6=2"),
    [{<<"föo bar"/utf8>>,<<"1">>},{<<"ö"/utf8>>,<<"2">>}] =
        uri_string:dissect_query(<<"föo+bar=1&%C3%B6=2"/utf8>>).

dissect_query_negative(_Config) ->
    {error,missing_value,"&"} =
        uri_string:dissect_query("foo1&bar=2"),
    {error,invalid_percent_encoding,"%XX%B6"} = uri_string:dissect_query("foo=%XX%B6&amp;bar=2"),
    {error,invalid_input,[153]} =
        uri_string:dissect_query("foo=%99%B6&amp;bar=2"),
    {error,invalid_character,"ö"} = uri_string:dissect_query(<<"föo+bar=1&%C3%B6=2">>),
    {error,invalid_input,<<"ö">>} =
        uri_string:dissect_query([<<"foo+bar=1&amp;">>,<<"%C3%B6=2ö">>]).

normalize(_Config) ->
    "/a/g" = uri_string:normalize("/a/b/c/./../../g"),
    <<"mid/6">> = uri_string:normalize(<<"mid/content=5/../6">>),
    "http://localhost-%C3%B6rebro/a/g" =
        uri_string:normalize("http://localhos%74-%c3%b6rebro:80/a/b/c/./../../g"),
    <<"http://localhost-%C3%B6rebro/a/g">> =
        uri_string:normalize(<<"http://localhos%74-%c3%b6rebro:80/a/b/c/./../../g">>),
    <<"https://localhost/">> =
        uri_string:normalize(<<"https://localhost:443">>),
    <<"https://localhost:445/">> =
        uri_string:normalize(<<"https://localhost:445">>),
    <<"ftp://localhost">> =
        uri_string:normalize(<<"ftp://localhost:21">>),
    <<"ssh://localhost">> =
        uri_string:normalize(<<"ssh://localhost:22">>),
    <<"sftp://localhost">> =
        uri_string:normalize(<<"sftp://localhost:22">>),
    <<"tftp://localhost">> =
        uri_string:normalize(<<"tftp://localhost:69">>).

normalize_map(_Config) ->
    "/a/g" = uri_string:normalize(#{path => "/a/b/c/./../../g"}),
    <<"mid/6">> = uri_string:normalize(#{path => <<"mid/content=5/../6">>}),
    "http://localhost-%C3%B6rebro/a/g" =
        uri_string:normalize(#{scheme => "http",port => 80,path => "/a/b/c/./../../g",
                               host => "localhost-örebro"}),
    <<"http://localhost-%C3%B6rebro/a/g">> =
        uri_string:normalize(#{scheme => <<"http">>,port => 80,
                               path => <<"/a/b/c/./../../g">>,
                               host => <<"localhost-örebro"/utf8>>}),
    <<"https://localhost/">> =
        uri_string:normalize(#{scheme => <<"https">>,port => 443,path => <<>>,
                               host => <<"localhost">>}),
    <<"https://localhost:445/">> =
        uri_string:normalize(#{scheme => <<"https">>,port => 445,path => <<>>,
                               host => <<"localhost">>}),
    <<"ftp://localhost">> =
        uri_string:normalize(#{scheme => <<"ftp">>,port => 21,path => <<>>,
                               host => <<"localhost">>}),
    <<"ssh://localhost">> =
        uri_string:normalize(#{scheme => <<"ssh">>,port => 22,path => <<>>,
                               host => <<"localhost">>}),
    <<"sftp://localhost">> =
        uri_string:normalize(#{scheme => <<"sftp">>,port => 22,path => <<>>,
                               host => <<"localhost">>}),
    <<"tftp://localhost">> =
        uri_string:normalize(#{scheme => <<"tftp">>,port => 69,path => <<>>,
                               host => <<"localhost">>}).

normalize_return_map(_Config) ->
    #{scheme := "http",path := "/a/g",host := "localhost-örebro"} =
        uri_string:normalize("http://localhos%74-%c3%b6rebro:80/a/b/c/./../../g",
                                   [return_map]),
    #{scheme := <<"http">>,path := <<"/a/g">>, host := <<"localhost-örebro"/utf8>>} =
        uri_string:normalize(<<"http://localhos%74-%c3%b6rebro:80/a/b/c/./../../g">>,
                                   [return_map]),
    #{scheme := <<"https">>,path := <<"/">>, host := <<"localhost">>} =
        uri_string:normalize(#{scheme => <<"https">>,port => 443,path => <<>>,
                               host => <<"localhost">>}, [return_map]).

normalize_negative(_Config) ->
    {error,invalid_uri,":"} =
        uri_string:normalize("http://local>host"),
    {error,invalid_uri,":"} =
        uri_string:normalize(<<"http://local>host">>),
    {error,invalid_uri,":"} =
        uri_string:normalize("http://[192.168.0.1]", [return_map]),
    {error,invalid_uri,":"} =
        uri_string:normalize(<<"http://[192.168.0.1]">>, [return_map]).

interop_query_utf8(_Config) ->
    Q = uri_string:compose_query([{"foo bar","1"}, {"合", "2"}]),
    Uri = uri_string:recompose(#{path => "/", query => Q}),
    #{query := Q1} = uri_string:parse(Uri),
    [{"foo bar","1"}, {"合", "2"}] = uri_string:dissect_query(Q1).

interop_query_latin1(_Config) ->
    Q = uri_string:compose_query([{"foo bar","1"}, {"合", "2"}], [{encoding,latin1}]),
    Uri = uri_string:recompose(#{path => "/", query => Q}),
    Uri1 = uri_string:transcode(Uri, [{in_encoding, latin1}]),
    #{query := Q1} = uri_string:parse(Uri1),
    [{"foo bar","1"}, {"合", "2"}] = uri_string:dissect_query(Q1).
