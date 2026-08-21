%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(array_prop).

-include_lib("common_test/include/ct_property_test.hrl").

-export([prop_new/0, prop_is_array/0, prop_set_get/0, prop_size/0,
         prop_sparse_size/0,
         prop_default/0, prop_fix_relax/0,
         prop_resize/0, prop_resize_pruned/0,
         prop_reset/0, prop_to_list/0, prop_from_list/0,
         prop_to_orddict/0, prop_from_orddict/0, prop_map/0,
         prop_foldl/0, prop_foldr/0, prop_shift/0, prop_slice/0,
         prop_append_prepend/0, prop_concat/0, prop_mapfoldl/0, prop_mapfoldr/0,
         prop_sparse_mapfoldl/0, prop_sparse_mapfoldr/0]).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% Generate a small non-negative integer for array indices
small_nat() ->
    ?LET(N, nat(), N rem 520).

safe_any() ->  %% Just integers for now
    int().

%% Generate array with list model

array_with_list() ->
    ?LET(TypeGen, fun() -> safe_any() end,
         ?LET(Def, TypeGen(),
              ?SIZED(Size, array_with_list(Size, TypeGen, [], array:new({default, Def}))))).

array_with_list(TypeGen) ->
    ?LET(Def, TypeGen(),
         ?SIZED(Size, array_with_list(Size, TypeGen, [], array:new({default, Def})))).

array_with_list(0, _Type, List, A) ->
    {List, A};
array_with_list(N, Type, ListAcc, ArrAcc) ->
    RC = fun({L, A}) -> array_with_list(N-1, Type, L, A) end,
    oneof([ %% Set/append/prepend many at end/beginning
            ?LET(List, list(Type()),
                 RC(array_list_append(List, ListAcc, ArrAcc))),
            ?LET(List, list(Type()),
                 RC(array_list_set(List, ListAcc, ArrAcc))),
            ?LET(List, list(Type()),
                 RC(array_list_prepend(List, ListAcc, ArrAcc))),
            ?LET(List, list(Type()),
                 RC(array_list_concat(List, ListAcc, ArrAcc))),
            %% Set and reset random position single
            ?LET({I, V}, {small_nat(), Type()},
                 RC(array_list_set(I,V,ListAcc, ArrAcc))),
            ?LET(I, small_nat(),
                 RC(array_list_reset(I, ListAcc, ArrAcc))),
            %% Resize, shift
            ?LET(I, small_nat(),
                 RC(array_list_resize(I, ListAcc, ArrAcc))),
            ?LET(I, Type(),
                 RC(array_list_shift(I, ListAcc, ArrAcc)))
          ]).

array_list_append(List, ListAcc, ArrAcc) ->
    try
        ListAcc = array:to_list(ArrAcc), %% assert
        L = ListAcc ++ List,
        A = lists:foldl(fun array:append/2, ArrAcc, List),
        L = array:to_list(A), %% assert
        {L, A}
    catch Class:Err:St ->
            io:format("~w~n  ~w~n ~w~n", [List,ListAcc,ArrAcc]),
            erlang:raise(Class, Err, St)
    end.

array_list_set(List, ListAcc, ArrAcc) ->
    try
        ListAcc = array:to_list(ArrAcc), %% assert
        ArraySet = fun(V, A) -> array:set(array:size(A), V, A) end,
        L = ListAcc ++ List,
        A = lists:foldl(ArraySet, ArrAcc, List),
        L = array:to_list(A), %% assert
        {L, A}
    catch Class:Err:St ->
            io:format("~w~n  ~w~n ~w~n", [List,ListAcc,ArrAcc]),
            erlang:raise(Class, Err, St)
    end.

array_list_prepend(List, ListAcc, ArrAcc) ->
    try
        ListAcc = array:to_list(ArrAcc), %% assert
        L = List ++ ListAcc,
        A = lists:foldl(fun array:prepend/2, ArrAcc, lists:reverse(List)),
        L = array:to_list(A), %% assert
        {L, A}
    catch Class:Err:St ->
            io:format("~w~n  ~w~n ~w~n", [List,ListAcc,ArrAcc]),
            erlang:raise(Class, Err, St)
    end.

array_list_concat(List, ListAcc, ArrAcc) ->
    try
        ListAcc = array:to_list(ArrAcc), %% assert
        L = List ++ ListAcc,
        A = array:concat(array:from_list(List, array:default(ArrAcc)), ArrAcc),
        L = array:to_list(A), %% assert
        {L, A}
    catch Class:Err:St ->
            io:format("~w~n  ~w~n ~w~n", [List,ListAcc,ArrAcc]),
            erlang:raise(Class, Err, St)
    end.

array_list_set(I, V, L0, A0) ->
    try
        L0 = array:to_list(A0), %% assert
        Size = array:size(A0),
        {L, A} =
            if I < Size ->
                    {L1, [_|L2]} = lists:split(I, L0),
                    {L1 ++ [V|L2], array:set(I, V, A0)};
               I == Size ->
                    {L0 ++ [V], array:set(I, V, A0)};
               true ->
                    Def = array:default(A0),
                    Idx = Size + (I rem (Size+17)),  %% Allow up to next bucket
                    L2 = lists:duplicate(Idx-Size, Def) ++ [V],
                    {L0++L2, array:set(Idx, V, A0)}
            end,
        L = array:to_list(A), %% assert
        {L, A}
    catch Class:Err:St ->
            io:format("~w ~w~n  ~w~n ~w~n", [I,V,L0,A0]),
            erlang:raise(Class, Err, St)
    end.

array_list_reset(I, L0, A0) ->
    try
        L0 = array:to_list(A0), %% assert
        Size = array:size(A0),
        {L,A} = if I < Size ->
                        Def = array:default(A0),
                        {L1, [_|L2]} = lists:split(I, L0),
                        {L1 ++ [Def|L2], array:reset(I, A0)};
                   true ->
                        {L0, array:reset(I, A0)}
                end,
        L = array:to_list(A), %% assert
        {L, A}
    catch Class:Err:St ->
            io:format("~w~n  ~w~n ~w~n", [I,L0,A0]),
            erlang:raise(Class, Err, St)
    end.

array_list_resize(NewSz0, L0, A0) ->
    try
        L0 = array:to_list(A0), %% assert
        Size = array:size(A0),
        {L, A} =
            if NewSz0 =< Size ->
                    {L1, _L2} = lists:split(NewSz0, L0),
                    {L1, array:resize(NewSz0, A0)};
               true ->
                    %% Limit NewSize
                    Extend = (NewSz0 rem (Size+17)),
                    Def = array:default(A0),
                    L1 = lists:duplicate(Extend, Def),
                    {L0 ++ L1, array:resize(Size+Extend, A0)}
            end,
        L = array:to_list(A), %% assert
        {L, A}
    catch Class:Err:St ->
            io:format("~w~n  ~w~n ~w~n", [NewSz0,L0,A0]),
            erlang:raise(Class, Err, St)
    end.

array_list_shift(I0, L0, A0) ->
    try
        L0 = array:to_list(A0), %% assert
        Size = array:size(A0),
        Steps = if Size =< 1 ->
                        -abs(I0 rem 10);
                   true ->
                        I1 = I0 rem (Size-1),
                        I1 - Size div 2
                end,
        Atemp = array:shift(Steps, A0),  %% We need to reset entries in the array
        {L1,A1} = case Steps >= 0 of     %% to be equal to a list after another shift
                      true ->
                          {lists:sublist(L0, Steps+1, Size-Steps),
                           lists:foldl(fun array:reset/2, Atemp, lists:seq(Size-Steps, Size))};
                      false ->
                          {lists:duplicate(abs(Steps), array:default(A0)) ++ L0,
                           lists:foldl(fun array:reset/2, Atemp, lists:seq(0, -Steps-1))}
                  end,
        {L1,Steps} = {array:to_list(A1), Steps}, %% Assert
        {L1, A1}
    catch Class:Err:St ->
            io:format("~w~n  ~w~n ~w~n", [I0,L0,A0]),
            erlang:raise(Class, Err, St)
    end.

%% Generate a simple array
array_gen() ->
    array_gen(safe_any()).

array_gen(Type) ->
    ?LET({List, Default},
         {list(Type), Type},
         array:from_list(List, Default)).

%% Generate non-array term
non_array() ->
    ?SUCHTHAT(T, safe_any(), not array:is_array(T)).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_new() ->
    array:is_array(array:new()).

prop_is_array() ->
    ?FORALL({IsArray, Term},
            oneof([{true, array_gen()},
                   {false, non_array()}]),
            IsArray =:= array:is_array(Term)).

prop_set_get() ->
    ?FORALL({{List, A}, I, V},
            {array_with_list(), small_nat(), safe_any()},
            begin
                case I < length(List) of
                    true ->
                        A1 = array:set(I, V, A),
                        array:get(I, A1) =:= V;
                    false ->
                        %% Setting beyond size grows array
                        A1 = array:relax(A),
                        A2 = array:set(I, V, A1),
                        array:get(I, A2) =:= V
                end
            end).

prop_size() ->
    ?FORALL({List, A}, array_with_list(),
            array:size(A) =:= length(List)).

prop_sparse_size() ->
    ?FORALL({List, A}, array_with_list(),
            begin
                SparseSize = array:sparse_size(A),
                Size = array:size(A),
                Strip = fun(X) -> X == array:default(A) end,
                Expected = lists:reverse(lists:dropwhile(Strip, lists:reverse(List))),
                SparseSize >= 0 andalso SparseSize =< Size
                    andalso SparseSize =:= length(Expected)
            end).

prop_default() ->
    ?FORALL({Default, Size},
            {safe_any(), small_nat()},
            begin
                A = array:new([{size, Size}, {default, Default}]),
                array:default(A) =:= Default
            end).

prop_fix_relax() ->
    ?FORALL(A, array_gen(),
            begin
                A1 = array:fix(A),
                I = array:size(A),
                try array:set(I, x, A1), false catch _:_ -> true end
                    andalso array:is_fix(A1)
                    andalso begin
                                A2 = array:relax(A1),
                                try array:set(I, x, A), true catch _:_ -> false end
                                    andalso not array:is_fix(A2)
                            end
            end).

prop_resize() ->
    ?FORALL({A, NewSize},
            {array_gen(), small_nat()},
            ?TIMEOUT(60_000,
                     begin
                         A1 = array:resize(NewSize, A),
                         array:size(A1) =:= NewSize andalso
                             length(array:to_list(A1)) == NewSize
                     end)).

prop_resize_pruned() ->
    ?FORALL({{L0, A0}, ShiftBy0}, {array_with_list(), small_nat()},
            begin
                {L1, A1} = array_list_append([foo], L0, A0),
                ShiftBy = min(ShiftBy0, array:size(A1)),
                A2 = array:shift(ShiftBy, A1),
                L2 = lists:sublist(L1, ShiftBy+1, length(L1)-ShiftBy),
                {L3, A3} = array_list_resize(array:size(A2), L2, A2),
                L4 = lists:duplicate(ShiftBy, array:default(A0)) ++ L3,
                A4 = array:shift(-ShiftBy, A3),
                L4 == array:to_list(A4)
            end).

prop_reset() ->
    ?FORALL({{List, A}, I},
            {array_with_list(), small_nat()},
            begin
                case I < length(List) orelse not array:is_fix(A) of
                    true ->
                        Default = array:default(A),
                        A1 = array:set(I, some_value, A),
                        A2 = array:reset(I, A1),
                        array:get(I, A2) =:= Default;
                    false ->
                        try array:reset(I, A), false catch _:_ -> true end
                end
            end).

prop_to_list() ->
    ?FORALL({List, A}, array_with_list(),
            array:to_list(A) =:= List).

prop_from_list() ->
    ?FORALL(L, list(safe_any()),
            begin
                A = array:from_list(L),
                array:is_array(A) andalso
                    array:size(A) =:= length(L) andalso
                    array:to_list(A) =:= L
            end).

prop_to_orddict() ->
    ?FORALL({List, A}, array_with_list(),
            begin
                OD = array:to_orddict(A),
                Expected = [{I, V} || I <:- lists:seq(0, length(List)-1) && V <:- List],
                OD =:= Expected
            end).

prop_from_orddict() ->
    ?FORALL(L, list({small_nat(), safe_any()}),
            begin
                OD = lists:ukeysort(1, L),
                A = array:from_orddict(OD),
                array:is_array(A) andalso
                    array:sparse_to_orddict(A) =:= OD
            end).

prop_map() ->
    ?FORALL({List, A}, array_with_list(),
            begin
                FL = fun(X) -> {mapped, X} end,
                FA = fun(_I, X) -> {mapped, X} end,
                A1 = array:map(FA, A),
                Expected = lists:map(FL, List),
                array:to_list(A1) =:= Expected
            end).

prop_foldl() ->
    ?FORALL({{List, A}, I0, I1}, {array_with_list(fun() -> int() end), small_nat(), small_nat()},
            case array:size(A) of
                0 ->
                    true;
                Size ->
                    Stop  = min(Size-1, max(I0, I1)),
                    Start = min(Stop, min(I0, I1)),
                    Sum1 = array:foldl(Start, Stop, fun(_I, V, Acc) -> Acc + V end, 0, A),
                    Split = lists:sublist(List, Start+1, 1+Stop-Start),
                    Sum2 = lists:foldl(fun(V, Acc) -> Acc + V end, 0, Split),

                    Sum3 = array:foldl(fun(_I, V, Acc) -> Acc + V end, 0, A),
                    Sum4 = lists:foldl(fun(V, Acc) -> Acc + V end, 0, List),
                    Sum1 =:= Sum2 andalso Sum3 =:= Sum4
            end).

prop_foldr() ->
    ?FORALL({{List, A}, I0, I1}, {array_with_list(fun() -> int() end), small_nat(), small_nat()},
            case array:size(A) of
                0 ->
                    true;
                Size ->
                    Stop  = min(Size-1, max(I0, I1)),
                    Start = min(Stop, min(I0, I1)),
                    Sum1 = array:foldr(Start, Stop, fun(_I, V, Acc) -> Acc + V end, 0, A),
                    Split = lists:sublist(List, Start+1, 1+Stop-Start),
                    Sum2 = lists:foldr(fun(V, Acc) -> Acc + V end, 0, Split),

                    Sum3 = array:foldr(fun(_I, V, Acc) -> Acc + V end, 0, A),
                    Sum4 = lists:foldr(fun(V, Acc) -> Acc + V end, 0, List),
                    Sum1 =:= Sum2 andalso Sum3 =:= Sum4
            end).

prop_shift() ->
    ?FORALL({{List, A}, Steps},
            {array_with_list(), ?LET(N, int(), N rem 20)},
            begin
                Size = length(List),
                case Steps =< Size of
                    true ->
                        Expected =
                            case Steps > 0 of
                                true ->
                                    lists:sublist(List, Steps+1, Size-Steps);
                                false ->
                                    lists:duplicate(abs(Steps), array:default(A)) ++ List
                            end,
                        A1 = case Steps < 0 of
                                 true ->
                                     %% We need to reset it here it to prune (history) shifted values.
                                     array:shift(Steps, array:resize(array:size(A), A));
                                 false ->
                                     array:shift(Steps, A)
                             end,
                        array:is_array(A1) andalso
                            array:size(A1) =:= max(0, Size - Steps) andalso
                            array:to_list(A1) =:= Expected;
                    false ->
                        true
                end
            end).

prop_slice() ->
    ?FORALL({{List, A}, I, Len},
            {array_with_list(), small_nat(), small_nat()},
            begin
                Size = length(List),
                case I < Size of
                    true ->
                        ActualLen = min(Len, Size - I),
                        A1 = array:slice(I, ActualLen, A),
                        Expected = lists:sublist(List, I+1, ActualLen),
                        array:to_list(A1) =:= Expected;
                    false ->
                        true
                end
            end).

prop_append_prepend() ->
    ?FORALL({{List, A}, V},
            {array_with_list(), safe_any()},
            begin
                A1 = array:append(V, A),
                array:to_list(A1) =:= List ++ [V] andalso
                    begin
                        A2 = array:prepend(V, A),
                        array:to_list(A2) =:= [V | List]
                    end
            end).

prop_concat() ->
    ?FORALL({{List1, A1}, {List2, A2}},
            {array_with_list(), array_with_list()},
            begin
                A3 = array:concat(A1, A2),
                array:to_list(A3) =:= List1 ++ List2
            end).

prop_mapfoldl() ->
    ?FORALL({List, A}, array_with_list(fun() -> int() end),
            begin
                F = fun(_I, V, Acc) -> {{mapped, V}, Acc + V} end,
                {A1, Sum1} = array:mapfoldl(F, 0, A),
                Sum2 = lists:sum(List),
                Expected = lists:map(fun(V) -> {mapped, V} end, List),
                array:to_list(A1) =:= Expected andalso Sum1 =:= Sum2
            end).

prop_mapfoldr() ->
    ?FORALL({List, A}, array_with_list(fun() -> int() end),
            begin
                F = fun(_I, V, Acc) -> {{mapped, V}, Acc + V} end,
                {A1, Sum1} = array:mapfoldr(F, 0, A),
                Sum2 = lists:sum(List),
                Expected = lists:map(fun(V) -> {mapped, V} end, List),
                array:to_list(A1) =:= Expected andalso Sum1 =:= Sum2
            end).

prop_sparse_mapfoldl() ->
    ?FORALL({{List, A}, I0, I1},
            {array_with_list(fun() -> int() end), small_nat(), small_nat()},
            case array:size(A) of
                0 ->
                    true;
                Size ->
                    High = min(Size-1, max(I0, I1)),
                    Low = min(High, min(I0, I1)),

                    F = fun(_I, V, Acc) -> {{mapped, V}, Acc + V} end,
                    {A1, Sum1} = array:sparse_mapfoldl(Low, High, F, 0, A),
                    SubList = lists:sublist(List, Low+1, 1+High-Low),
                    Def = array:default(A1),
                    F2 = fun(V, Acc) -> case V == Def of
                                            false -> {{mapped, V}, Acc + V};
                                            true  -> {V, Acc}
                                        end
                         end,
                    {ExpectedSub, Sum2} = lists:mapfoldl(F2, 0, SubList),
                    ResultSub = lists:sublist(array:to_list(A1), Low+1, High-Low+1),
                    ResultSub =:= ExpectedSub andalso Sum1 =:= Sum2
            end).

prop_sparse_mapfoldr() ->
    ?FORALL({{List, A}, I0, I1},
            {array_with_list(fun() -> int() end), small_nat(), small_nat()},
            case array:size(A) of
                0 ->
                    true;
                Size ->
                    High = min(Size-1, max(I0, I1)),
                    Low = min(High, min(I0, I1)),
                    F = fun(_I, V, Acc) -> {{mapped, V}, Acc + V} end,
                    {A1, Sum1} = array:sparse_mapfoldr(Low, High, F, 0, A),
                    SubList = lists:sublist(List, Low+1, High-Low+1),
                    Def = array:default(A1),
                    F2 = fun(V, Acc) -> case V == Def of
                                            false -> {{mapped, V}, Acc + V};
                                            true  -> {V, Acc}
                                        end
                         end,
                    {ExpectedSub, Sum2} = lists:mapfoldr(F2, 0, SubList),
                    ResultSub = lists:sublist(array:to_list(A1), Low+1, High-Low+1),
                    ResultSub =:= ExpectedSub andalso Sum1 =:= Sum2
            end).
