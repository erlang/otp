%%
%% %CopyrightBegin%
%%
%% Copyright Sergey Prokhorov 2021. All Rights Reserved.
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
%% @doc Map comprehension / map generators tests
-module(mc_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 basic/1, shadow/1]).

-include_lib("common_test/include/ct.hrl").

-define(assertEqual(Pattern, Expr), (Pattern == Expr) orelse error({not_equal, Pattern, Expr})).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [basic,
       shadow
      ]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Config.

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    ok.

basic(Config) when is_list(Config) ->
    ?assertEqual(#{1 => 2, 3 => 4}, map_comprehension_map_generator(#{1 => 2, 3 => 4})),
    ?assertEqual(#{3 => 4}, map_comprehension_map_generator_filter(#{1 => 2, 3 => 4})),
    ?assertEqual(#{1 => 1,2 => 2,3 => 3,4 => 4},
                 map_comprehension_list_generator([1, 2, 3, 4])),
    ?assertEqual([3, 7], list_comprehension_map_generator(#{1 => 2, 3 => 4})),
    ?assertEqual(#{1 => 2, 3 => 4}, map_comprehension_binary_generator(<<1, 2, 3, 4>>)),
    ?assertEqual(<<1, 2 ,3, 4>>, binary_comprehension_map_generator(#{1 => 2, 3 => 4})),
    ?assertEqual(
       #{1 => 2,3 => 2},
       map_comprehension_map_generator_pattern_exprs(
         #{ {1, 1} => 2,
            {2, 2} => 1,
            {1, 2} => 2,
            {2, 3} => 1,
            {3, 3} => 2
          })),
    ?assertEqual(
       #{2 => {4,ok},4 => {6,ok}},
       map_comprehension_map_generator_kv_exprs(#{1 => 2, 3 => 4})),
    ?assertEqual(
       #{{a,1} => 2,{a,3} => 4,{b,5} => 6,{b,7} => 8},
       map_comprehension_nested_map_generator(
         #{
           a => #{1 => 2, 3 => 4},
           b => #{5 => 6, 7 => 8}
          })),
    %% map generator over non-map expression
    {error, {badmap, not_map}} =
        try map_comprehension_map_generator(not_map) catch T0:R0 -> {T0, R0} end,
    %% error in map comprehension
    {error, badarith} =
        try map_comprehension_map_generator_kv_exprs(#{a => b}) catch T1:R1 -> {T1, R1} end,
    %% nested map generator over non-map
    {error, {badmap, [1, 2, 3]}} =
        try map_comprehension_nested_map_generator(#{a => [1, 2, 3]}) catch T2:R2 -> {T2, R2} end.

shadow(Config) when is_list(Config) ->
    Shadowed = nomatch,
    _ = id(Shadowed),
    Map = #{1 => 1, 2 => 2, 3 => 3},
    M = #{Shadowed => Shadowed + 1 || Shadowed := Shadowed <- Map},
    ?assertEqual(#{1 => 2, 2 => 3, 3 => 4}, id(M)),
    ?assertEqual(#{3 => 4}, #{Shadowed => Shadowed + 1
                              || Shadowed := Shadowed <- Map,
                                 Shadowed > 2}).

map_comprehension_map_generator(Map) ->
   #{K => V || K := V <- Map}.

map_comprehension_map_generator_filter(Map) ->
   #{K => V || K := V <- Map, K > 1}.

map_comprehension_list_generator(List) ->
    #{K => K || K <- List}.

list_comprehension_map_generator(Map) ->
   [K + V || K := V <- Map].

map_comprehension_binary_generator(Bin) ->
    #{K => V || <<K, V>> <= Bin}.

binary_comprehension_map_generator(Map) ->
    << <<K, V>> || K := V <- Map >>.

map_comprehension_map_generator_pattern_exprs(Map) ->
    #{K => V || {K, K} := 2 = V <- id(Map)}.

map_comprehension_map_generator_kv_exprs(Map) ->
    #{K + 1 => {V + 2, ok} || K := V <- id(Map)}.

map_comprehension_nested_map_generator(Map) ->
    #{{K0, K} => V || K0 := V0 <- id(Map), K := V <- id(V0)}.

id(I) -> I.
