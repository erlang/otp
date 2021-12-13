%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2021. All Rights Reserved.
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

-module(proplists_SUITE).

-export([all/0, suite/0,groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2,
         examples/1, map_conversion/1, map_conversion_normalize/1,
         pm_fold_test/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() ->
    [examples, map_conversion, map_conversion_normalize, pm_fold_test].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Test all examples in the documentation.

examples(_Config) ->
    [1,2,3,4] = proplists:append_values(a, [{a, [1,2]}, {b, 0}, {a, 3}, {c, -1}, {a, [4]}]),

    ExpandRes = [fie, bar, baz, fum],
    ExpandRes = proplists:expand([{foo, [bar, baz]}], [fie, foo, fum]),
    ExpandRes = proplists:expand([{{foo, true}, [bar, baz]}], [fie, foo, fum]),
    ExpandRes = proplists:expand([{{foo, false}, [bar, baz]}], [fie, {foo, false}, fum]),

    [{foo, false}, fie, foo, fum] = proplists:expand([{{foo, true}, [bar, baz]}],
                                                     [{foo, false}, fie, foo, fum]),

    {[[a], [{b, 5}, b],[{c, 2}, {c, 3, 4}]], [{e, 1}, d]} =
        proplists:split([{c, 2}, {e, 1}, a, {c, 3, 4}, d, {b, 5}, b], [a, b, c]),

    ColorList = [{color, red}, {colour, green}, color, colour],
    ColorListRes = [{colour, red}, {colour, green}, colour, colour],
    ColorListRes = proplists:substitute_aliases([{color, colour}], ColorList),

    NegList = [no_foo, {no_foo, true}, {no_foo, false}, {no_foo, any}, foo],
    NegListRes = [{foo, false}, {foo, false}, foo, foo, foo],
    NegListRes = proplists:substitute_negations([{no_foo, foo}], NegList),

    true = #{a => true, b => 1, c => 2} =:= proplists:to_map([a, {b, 1}, {c, 2}, {c, 3}]),

    ok.

map_conversion(_Config) ->
    %% Simple tests.
    true = #{} =:= proplists:to_map([]),
    true = #{a => true, b => true} =:= proplists:to_map([a, b]),
    true = #{a => true, b => true} =:= proplists:to_map([b, a]),
    true = #{a => 1, b => true} =:= proplists:to_map([{a, 1}, b]),
    true = #{a => 1, b => true} =:= proplists:to_map([b, {a, 1}]),
    true = #{a => 1, b => 2} =:= proplists:to_map([{a, 1}, {b, 2}]),
    true = #{a => 1, b => 2} =:= proplists:to_map([{b, 2}, {a, 1}]),
    true = #{b => true} =:= proplists:to_map(["a", b]),
    true = #{b => true} =:= proplists:to_map([b, "a"]),
    true = #{b => true} =:= proplists:to_map([{a}, b]),
    true = #{b => true} =:= proplists:to_map([b, {a}]),
    true = #{b => true} =:= proplists:to_map([{a, 1, 2}, b]),
    true = #{b => true} =:= proplists:to_map([b, {a, 1, 2}]),

    %% Ensure that maps:get/3 using the created map yields the same
    %% results as proplists:get_value/3 on the original proplist does,
    %% and that proplists:get_value/3 on a proplist created from the
    %% map yields the same results as proplists:get_value/3 on the
    %% original proplist, ie they either all return the same `Value',
    %% or they all return the `Default' given as respective third argument.
    Default = make_ref(),
    InList=[a, b, {a, 1}, {}, {a}, {a, 1, 2}, {c, 1, 2}, "foo"],
    Fun = fun (L1, Acc) ->
        LKs = proplists:get_keys(L1),
        M = proplists:to_map(L1),
        L2 = proplists:from_map(M),
        true = lists:sort(maps:keys(M)) =:= lists:sort(proplists:get_keys(L2)),
        lists:foreach(
            fun (K) ->
                case
                    {
                        maps:get(K, M, Default),
                        proplists:get_value(K, L1, Default),
                        proplists:get_value(K, L2, Default)
                    }
                of
                    {Default, Default, Default} -> ok;
                    {V, V, V} -> ok
                end
            end,
            LKs
        ),
        Acc
    end,
    _ = pm_fold(Fun, undefined, InList),
    ok.

map_conversion_normalize(_Config) ->
    Stages = [
        {aliases, [{a, alias_a}]},
        {negations, [{no_b, b}]},
        {expand, [{c, [d]}]}
    ],

    M1 = proplists:to_map([], Stages),
    true = M1 =:= #{},
    true = M1 =:= proplists:to_map(proplists:normalize([], Stages)),

    List = [a, no_b, c],
    M2 = proplists:to_map(List, Stages),
    true = M2 =:= #{alias_a => true, b => false, d => true},
    true = M2 =:= proplists:to_map(proplists:normalize(List, Stages)),

    ok.

pm_fold(_, _, []) ->
    [];
pm_fold(Fun, Acc0, L) ->
    pm_fold(Fun, Acc0, L, []).

pm_fold(Fun, Acc, [], Mut) ->
    Fun(Mut, Acc);
pm_fold(Fun, Acc, L, Mut) ->
    lists:foldl(
        fun
            (X, AccIn) -> pm_fold(Fun, AccIn, lists:delete(X, L), [X|Mut])
        end,
        Acc,
        L
    ).

pm_fold_test(_Config) ->
    Fun = fun (M, A) -> [M|A] end,

    [] = pm_fold(Fun, [], []),

    [[1]] = lists:sort(pm_fold(Fun, [], [1])),

    Exp1 = lists:sort([[1, 2], [2, 1]]),
    Exp1 = lists:sort(pm_fold(Fun, [], [1, 2])),

    Exp2 = lists:sort([[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]),
    Exp2 = lists:sort(pm_fold(Fun, [], [1, 2, 3])),

    ok.
