%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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
%% Originally based on Per Gustafsson's test suite.
%%

-module(mc_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         basic/1,duplicate_keys/1,mixed/1,
         shadow/1,bad_generators/1,multi/1,from_keys_optimization/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [basic,
       duplicate_keys,
       mixed,
       shadow,
       bad_generators,
       multi,
       from_keys_optimization]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-record(foo, {a,b}).

basic(_Config) ->
    mc_double(0),
    mc_double(1),
    mc_double(2),
    mc_double(3),
    mc_double(4),
    mc_double(5),

    mc_double(17),
    mc_double(18),
    mc_double(19),

    mc_double(30),
    mc_double(31),
    mc_double(32),
    mc_double(33),
    mc_double(34),

    mc_double(63),
    mc_double(64),
    mc_double(65),

    mc_double(77),
    mc_double(127),
    mc_double(128),
    mc_double(255),
    mc_double(333),
    mc_double(444),
    mc_double(7777),
    mc_double(8765),

    %% Patterns that cannot possibly match.
    #{} = #{K => V || K := [V]={V} <- id(#{a => [b]})},
    #{} = #{K => V || [K] = 42 := V <- id(#{42 => whatever})},

    %% Filtering.
    Map = #{{a,1} => {a,b,c}, {b,42} => [1,2,3], c => [4,5,6], d => {x,y}},
    [c, d] = lists:sort([K || K := _ <- Map, is_atom(K)]),
    [{a,b,c}, [1,2,3]] = lists:sort([V || {_,_} := V <- Map]),
    [1] = [H || {_,_} := [H|_] <- Map],
    [c, {b,42}] = lists:sort([K || K := [_|_] <- Map]),

    %% Filtering using literal patterns.
    [] = [0 || a := b <- #{}],
    [] = [0 || a := b <- #{x => y}],
    [0] = [0 || a := b <- #{a => b}],

    <<>> = << <<0>> || a := b <- #{} >>,
    <<>> = << <<0>> || a := b <- #{x => y} >>,
    <<0>> = << <<0>> || a := b <- #{a => b} >>,

    %% Matching partial records.
    RecordMap = id(#{#foo{a=I,b=I*I} => I*I*I || I <- [1,2,3,4]}),

    EvenMap = maps:from_list([{K,V} ||
                                 {#foo{a=N}=K,V} <- maps:to_list(RecordMap),
                                 N rem 2 =:= 0]),
    EvenMap = #{K => V ||
                  #foo{a=N}=K := V <- RecordMap,
                  N rem 2 =:= 0},

    Odd = lists:sort([V || {#foo{a=N}, V} <- maps:to_list(RecordMap),
                           N rem 2 =:= 1]),
    Odd = lists:sort([V || #foo{a=N} := V <- RecordMap, N rem 2 =:= 1]),

    %% Strict generators (each generator type)
    #{1 := 2, 2 := 3, 3 := 4} = #{X => X+1 || X <:- [1,2,3]},
    #{1 := 2, 2 := 3, 3 := 4} = #{X => X+1 || <<X>> <:= <<1,2,3>>},
    #{2 := 4, 4 := 8} = #{X+1 => Y*2 || X := Y <:- #{1 => 2, 3 => 4}},

    %% A failing guard following a strict generator is ok
    #{2 := 3, 3 := 4} = #{X => X+1 || X <:- [1,2,3], X > 1},
    #{2 := 3, 3 := 4} = #{X => X+1 || <<X>> <:= <<1,2,3>>, X > 1},
    #{4 := 8} = #{X+1 => Y*2 || X := Y <:- #{1 => 2, 3 => 4}, X > 1},

    %% Non-matching elements cause a badmatch error for strict generators
    {'EXIT',{{badmatch,2},_}} = (catch #{X => X+1 || {ok, X} <:- [{ok,1},2,{ok,3}]}),
    {'EXIT',{{badmatch,<<128,2>>},_}} = (catch #{X => X+1 || <<0:1, X:7>> <:= <<1,128,2>>}),
    {'EXIT',{{badmatch,{2,error}},_}} = (catch #{X => X+1 || X := ok <:-#{1 => ok, 2 => error, 3 => ok}}),

    ok.

mc_double(Size) ->
    Seq = lists:seq(1, Size),
    Map = #{{key,I} => I || I <- Seq},

    MapDouble = #{K => 2 * V || K := V <- id(Map)},
    MapDouble = maps:from_list([{{key,I}, 2 * I} || I <- Seq]),

    OddKeys = lists:seq(1, Size, 2),
    OddKeys = lists:sort([I || {key,I} := I <- Map,
                               I rem 2 =/= 0]),

    OddMap = #{I => [] || {key,I} := I <- Map,
                          I rem 2 =/= 0},
    OddMap = #{I => [] || {key,I} := I <- Map,
                          id(I) rem 2 =/= 0},
    OddKeys = lists:sort(maps:keys(OddMap)),

    %% Test that map comprehensions works on iterators.
    test_iterator(Map, 0),
    test_iterator(Map, map_size(Map) div 3),
    test_iterator(Map, map_size(Map) div 2),
    test_iterator(Map, map_size(Map)),

    ok.

test_iterator(Map, N) ->
    Iter0 = maps:iterator(Map),
    {First,Iter} = grab(N, Iter0, []),
    All = [{K,V} || K := V <- Iter] ++ First,
    Map = maps:from_list(All),
    ok.

grab(0, Iter, Acc) ->
    {Acc,Iter};
grab(N, Iter0, Acc) ->
    case maps:next(Iter0) of
        none ->
            {Acc,Iter0};
        {K,V,Iter} ->
            grab(N - 1, Iter, [{K,V}|Acc])
    end.

duplicate_keys(_Config) ->
    #{x := b} = #{V => K || {K,V} <- [{a, x}, {b, x}]},

    #{a := 4, b := 4} =
        #{K => V || K <- [a,b],
                    <<V>> <= <<1,2,3,4>>},
    ok.

mixed(_Config) ->
    Map = id(#{1 => 10, 2 => 5, 3 => 88, 4 => 99, 5 => 36}),
    Bin = << <<K:8,V:24>> || K := V <- Map >>,
    Map = maps:from_list([{K,V} || <<K:8,V:24>> <= Bin]),

    Atoms = [list_to_atom([C]) || C <- lists:seq($a, $z)],
    Integers = lists:seq(1, 64),

    mixed_1(Atoms, Integers),
    mixed_2(Atoms, Integers),
    mixed_3(Atoms, Integers),

    sum_of_triangular_numbers(7),
    sum_of_triangular_numbers(10),

    ok.

mixed_1(Atoms, Integers) ->
    IntegerMap = #{N => [] || N <- Integers},
    IntegerKeys = [N || N := [] <- IntegerMap],
    Integers = lists:sort(IntegerKeys),
    Combined = [{C,N} || C <- Atoms, N := [] <- IntegerMap],
    Combined = [{C,N} || C <- Atoms, N := [] <- maps:iterator(IntegerMap)],
    Combined = [{C,N} || C <- Atoms, N <- IntegerKeys],

    ok.

mixed_2(Atoms, Integers) ->
    IntegerMap = #{N => [] || N <- Integers},
    IntegerKeys = [N || N := [] <- IntegerMap],
    Bin = << <<N:16>> || N := [] <- IntegerMap >>,
    Integers = lists:sort(IntegerKeys),

    Combined = [{C,N} || N := [] <- IntegerMap, C <- Atoms],
    Combined = [{C,N} || N := [] <- maps:iterator(IntegerMap), C <- Atoms],
    Combined = [{C,N} || <<N:16>> <= Bin, C <- Atoms],
    Combined = [{C,N} || N <- IntegerKeys, C <- Atoms],

    ok.

mixed_3(Atoms, Integers) ->
    Map = #{K => V || {K,V} <- lists:zip(Atoms, Integers, trim)},
    Bin = << <<N:16>> || _ := N <- Map >>,
    {TrimmedAtoms,TrimmedIntegers} = lists:unzip([{K,V} || K := V <- Map]),

    Combined = lists:sort([{K,V} || K := _ <- Map, _ := V <- Map]),
    Combined = lists:sort([{K,V} || K <- TrimmedAtoms, <<V:16>> <= Bin]),
    Combined = lists:sort([{K,V} || K <- TrimmedAtoms, V <- TrimmedIntegers]),

    ok.

sum_of_triangular_numbers(N) ->
    Sum = N * (N + 1) * (N + 2) div 6,
    Maps = [#{I => I || I <- lists:seq(0, I)} || I <- lists:seq(0, N)],
    Numbers = [I || M <- Maps, I := I <- M],
    Numbers = lists:flatten([[I || I := I <- M] || M <- Maps]),
    Sum = lists:sum([lists:sum([I || I := I <- M]) || M <- Maps]),
    Sum = lists:sum(Numbers),
    ok.

shadow(_Config)->
    Shadowed = nomatch,
    _ = id(Shadowed),				%Eliminate warning.
    Map = #{Shadowed => Shadowed+1 || Shadowed <- lists:seq(7, 9)},
    #{7 := 8, 8 := 9, 9 := 10} = id(Map),
    [8,9] = lists:sort([Shadowed || _ := Shadowed <- id(Map),
                                    Shadowed < 10]),
    ok.

bad_generators(_Config) ->
    %% Make sure that line numbers point out the generator.
    case ?MODULE of
        mc_inline_SUITE ->
            ok;
        _ ->
            {'EXIT',{{bad_generator,a},
                     [{?MODULE,_,_,
                       [{file,"bad_mc.erl"},{line,4}]}|_]}} =
                catch id(bad_generator(a)),

            {'EXIT',{{bad_generator,a},
                     [{?MODULE,_,_,
                       [{file,"bad_mc.erl"},{line,7}]}|_]}} =
                catch id(bad_generator_bc(a)),

            {'EXIT',{{bad_generator,a},
                     [{?MODULE,_,_,
                       [{file,"bad_mc.erl"},{line,10}]}|_]}} =
                catch id(bad_generator_mc(a)),

            BadIterator = [16#ffff|#{}],

            {'EXIT',{{bad_generator,BadIterator},
                     [{?MODULE,_,_,
                       [{file,"bad_mc.erl"},{line,4}]}|_]}} =
                catch id(bad_generator(BadIterator)),

            {'EXIT',{{bad_generator,BadIterator},
                     [{?MODULE,_,_,
                       [{file,"bad_mc.erl"},{line,7}]}|_]}} =
                catch id(bad_generator_bc(BadIterator)),

            {'EXIT',{{bad_generator,BadIterator},
                     [{?MODULE,_,_,
                       [{file,"bad_mc.erl"},{line,10}]}|_]}} =
                catch id(bad_generator_mc(BadIterator))
    end,
    ok.

multi(_Config) ->
    Exp = #{true => 1, false => 2},
    Exp = #{true => 1, false => 2 || true},
    Exp2 = #{1 => 1, 2 => 2, 5 => 5, 6 => 6},
    Exp2 = #{X => X, X + 1 => X + 1 || X <- [1, 5]},
    Exp3 = #{1 => 4, 5 => 8},
    Exp3 = #{X => X+1, X => X+3 || X <- [1, 5]},
    ok.

from_keys_optimization(_Config) ->
    %% Literal values - should use from_keys
    #{a := 42, b := 42} = #{K => 42 || K <- [a, b]},
    #{a := foo, b := foo} = #{K => foo || K <- [a, b]},

    %% Outer variable - should use from_keys
    Value = id(make_ref()),
    #{a := Value, b := Value} = #{K => Value || K <- [a, b]},

    %% Safe expression on outer vars (tuple) - should use from_keys
    X = id(1), Y = id(2),
    #{a := {1, 2}, b := {1, 2}} = #{K => {X, Y} || K <- [a, b]},

    %% With filter - should still use from_keys
    #{2 := ok, 4 := ok} = #{K => ok || K <- [1,2,3,4], K rem 2 =:= 0},

    %% Multiple expressions with same value - should use from_keys
    #{a := 42, b := 42, 1 := 42, 2 := 42} =
        #{K => 42, K2 => 42 || K <- [a, b], K2 <- [1, 2]},

    %% Multiple expressions with outer var as value - should use from_keys
    Z = id(outer),
    #{a := outer, 1 := outer} = #{K => Z, K2 => Z || K <- [a], K2 <- [1]},

    %% Multiple expressions with DIFFERENT values - should NOT use from_keys
    #{a := 1, b := 1, 1 := 2, 2 := 2} =
        #{K => 1, K2 => 2 || K <- [a, b], K2 <- [1, 2]},
    #{2 := [Value], 3 := [Value], 4 := [Value], 5 := [Value]} =
        #{2*K => [Value], 2*K+1 => [Value] || K <- [1, 2]},
    #{2 := {val, Value}, 3 := {val, Value},
      4 := {val, Value}, 5 := {val, Value}} =
        #{2*K => {val, Value}, 2*K+1 => {val, Value} || K <- [1, 2]},
    #{2 := [Value], 3 := [42], 4 := [Value], 5 := [42]} =
        #{2*K => [Value], 2*K+1 => [42] || K <- [1, 2]},

    %% Value from generator - should NOT use from_keys
    #{a := 1, b := 2} = #{K => V || {K, V} <- [{a, 1}, {b, 2}]},

    %% Value depends on key - should NOT use from_keys
    #{1 := 2, 2 := 4} = #{K => K * 2 || K <- [1, 2]},

    %% Value depends on filter - should NOT use from_keys
    #{1 := 2, 2 := 4} = #{K => V || K <- [1, 2], is_integer(V = K * 2)},

    %% Failable expression on outer vars - should NOT use from_keys
    %% (if list is empty, the division would never execute)
    A = id(1), B = id(0),
    #{} = #{K => A div B || K <- [], K > 0},

    ok.

id(I) -> I.

-file("bad_mc.erl", 1).
bad_generator(Map) ->                           %Line 2
    [{K,V} ||                                   %Line 3
        K := V <- Map].                         %Line 4
bad_generator_bc(Map) ->                        %Line 5
    << <<K:8,V:24>> ||                          %Line 6
        K := V <- Map>>.                        %Line 7
bad_generator_mc(Map) ->                        %Line 8
    #{V => K ||                                 %Line 9
        K := V <- Map}.                         %Line 10
