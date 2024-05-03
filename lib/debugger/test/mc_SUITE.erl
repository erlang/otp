%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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

-export([all/0,suite/0,init_per_suite/1,end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
	 init_per_group/2,end_per_group/2,
         basic/1,duplicate_keys/1,mixed/1,
         shadow/1,bad_generators/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [basic,
     duplicate_keys,
     mixed,
     shadow,
     bad_generators].

init_per_suite(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
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
                     [{?MODULE,_,_,_}|_]}} =
                catch id(bad_generator(a)),

            {'EXIT',{{bad_generator,a},
                     [{?MODULE,_,_,_}|_]}} =
                catch id(bad_generator_bc(a)),

            {'EXIT',{{bad_generator,a},
                     [{?MODULE,_,_,_}|_]}} =
                catch id(bad_generator_mc(a)),

            BadIterator = [16#ffff|#{}],

            {'EXIT',{{bad_generator,BadIterator},
                     [{?MODULE,_,_,_}|_]}} =
                catch id(bad_generator(BadIterator)),

            {'EXIT',{{bad_generator,BadIterator},
                     [{?MODULE,_,_,_}|_]}} =
                catch id(bad_generator_bc(BadIterator)),

            {'EXIT',{{bad_generator,BadIterator},
                     [{?MODULE,_,_,_}|_]}} =
                catch id(bad_generator_mc(BadIterator))
    end,
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
