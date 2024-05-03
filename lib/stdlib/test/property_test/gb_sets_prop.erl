%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2024. All Rights Reserved.
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
-module(gb_sets_prop).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% --- add/2 ----------------------------------------------------------
%%
%% add_element/2 is an alias for add/2
%% Since add_element/2 is tested in sets_prop, this property only tests if
%% the result of add/2 is the same as the result of add_element/2.
prop_add() ->
    ?FORALL(
        {S, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), L1 ++ L2}
        ),
        lists:all(fun(E) -> gb_sets:add(E, S) =:= gb_sets:add_element(E, S) end, L)
    ).

%% --- balance/1 ------------------------------------------------------
prop_balance() ->
    ?FORALL(
        S,
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            lists:foldl(
                fun gb_sets:del_element/2,
                gb_sets:from_list(L1 ++ L2),
                L1
            )
        ),
        gb_sets:is_equal(S, gb_sets:balance(S))
    ).

%% --- ceiling/2 -------------------------------------------------------
prop_ceiling() ->
    ?FORALL(
        {S, O, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), lists:usort(L1), L1 ++ L2}
        ),
        lists:all(
            fun(E) ->
                gb_sets:ceiling(E, S) =:= do_ceiling(E, O)
            end,
            L
         )
    ).

do_ceiling(_, []) ->
    none;
do_ceiling(E, [X | _]) when X >= E ->
    {found, X};
do_ceiling(E, [X | R]) when X < E ->
    do_ceiling(E, R).

%% --- delete/2 -------------------------------------------------------
prop_delete() ->
    ?FORALL(
        {S, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), L1 ++ L2}
        ),
        lists:all(
            fun(E) ->
                try
                    gb_sets:delete(E, S) =:= gb_sets:del_element(E, S)
                of
                    _ -> gb_sets:is_element(E, S)
                catch
                    error:_ -> not gb_sets:is_element(E, S)
                end
            end,
            L
         )
    ).

%% --- delete_any/2 ---------------------------------------------------
%%
%% del_element/2 is an alias for delete_any/2
%% Since del_element/2 is tested in sets_prop, this property only tests if
%% the result of delete_any/2 is the same as the result of del_element/2.
prop_delete_any() ->
    ?FORALL(
        {S, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), L1 ++ L2}
        ),
        lists:all(fun(E) -> gb_sets:delete_any(E, S) =:= gb_sets:del_element(E, S) end, L)
    ).

%% --- difference/2 ---------------------------------------------------
%%
%% subtract/2 is an alias for difference/2
%% Since subtract/2 is tested in sets_prop, this property only tests if
%% the result of difference/2 is the same as the result of subtract/2.
prop_difference() ->
    ?FORALL(
        {S1, S2},
        ?LET(
            {L1, L2, Both},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {gb_sets:from_list(L1 ++ Both), gb_sets:from_list(L2 ++ Both)}
        ),
        gb_sets:difference(S1, S2) =:= gb_sets:subtract(S1, S2)
    ).

%% --- floor/2 -------------------------------------------------------
prop_floor() ->
    ?FORALL(
        {S, O, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), lists:reverse(lists:usort(L1)), L1 ++ L2}
        ),
        lists:all(
            fun(E) ->
                gb_sets:floor(E, S) =:= do_floor(E, O)
            end,
            L
         )
    ).

do_floor(_, []) ->
    none;
do_floor(E, [X | _]) when X =< E ->
    {found, X};
do_floor(E, [X | R]) when X > E ->
    do_floor(E, R).

%% --- from_ordset/1 --------------------------------------------------
prop_from_ordset() ->
    ?FORALL(
        L,
        ct_proper_ext:safe_list(),
        gb_sets:is_equal(gb_sets:from_list(L),
                         gb_sets:from_ordset(ordsets:from_list(L)))
    ).

%% --- insert/2 -------------------------------------------------------
prop_insert() ->
    ?FORALL(
        {S, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), L1 ++ L2}
        ),
        lists:all(
            fun(E) ->
                try
                    gb_sets:insert(E, S) =:= gb_sets:add_element(E, S)
                of
                    _ -> not gb_sets:is_element(E, S)
                catch
                    error:_ -> gb_sets:is_element(E, S)
                end
            end,
            L
         )
    ).

%% --- is_member/2 ----------------------------------------------------
%%
%% is_element/2 is an alias for is_member/2
%% Since is_element/2 is tested in sets_prop, this property only tests if
%% the result of is_member/2 is the same as the result of is_element/2.
prop_is_member() ->
    ?FORALL(
        {S, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), L1 ++ L2}
        ),
        lists:all(fun(E) -> gb_sets:is_member(E, S) =:= gb_sets:is_element(E, S) end, L)
    ).

%% --- iterator/1 -----------------------------------------------------
%%
%% This property implicitly tests next/1
prop_iterator() ->
    ?FORALL(
        {S, L},
        ?LET(
            L,
            ct_proper_ext:safe_list(),
            begin
                L1 = lists:usort(L),
                {gb_sets:from_list(L1), L1}
            end
        ),
        do_iterate(gb_sets:iterator(S), L, ordered)
    ),
    ?FORALL(
        {S, L},
        ?LET(
            L,
            ct_proper_ext:safe_list(),
            begin
                L1 = lists:usort(L),
                {gb_sets:from_list(L1), lists:reverse(L1)}
            end
        ),
        do_iterate(gb_sets:iterator(S, reversed), L, reversed)
    ).

do_iterate(none, L, _) ->
    L =:= [];
do_iterate(I, [], _) ->
    none =:= gb_sets:next(I);
do_iterate(I0, L0, Order) ->
    {E, I1} = gb_sets:next(I0),
    lists:member(E, L0) andalso
    do_iterate_from(E, I1, lists:delete(E, L0), Order).

%% --- iterator_from/2 ------------------------------------------------
%%
%% This property implicitly tests next/1
prop_iterator_from() ->
    ?FORALL(
        {S, L, From},
        ?LET(
            {L, E},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_any()},
            begin
                L1 = lists:usort(L),
                L2 = lists:dropwhile(fun(X) -> X < E end, L1),
                F = case L2 of
                        [] -> E;
                        _ -> oneof([E, hd(L2)])
                    end,
                {gb_sets:from_list(L1), L2, F}
            end
        ),
        do_iterate_from(From, gb_sets:iterator_from(From, S), L, ordered)
    ),
    ?FORALL(
        {S, L, From},
        ?LET(
            {L, E},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_any()},
            begin
                L1 = lists:usort(L),
                L2 = lists:dropwhile(fun(X) -> X > E end, lists:reverse(L1)),
                F = case L2 of
                        [] -> E;
                        _ -> oneof([E, hd(L2)])
                    end,
                {gb_sets:from_list(L1), L2, F}
            end
        ),
        do_iterate_from(From, gb_sets:iterator_from(From, S, reversed), L, reversed)
    ).

do_iterate_from(_From, none, L, _) ->
    L =:= [];
do_iterate_from(_From, I, [], _) ->
    none =:= gb_sets:next(I);
do_iterate_from(From, I0, L0, ordered) ->
    {E, I1} = gb_sets:next(I0),
    lists:member(E, L0) andalso
    From =< E andalso
    do_iterate_from(E, I1, lists:delete(E, L0), ordered);
do_iterate_from(From, I0, L0, reversed) ->
    {E, I1} = gb_sets:next(I0),
    lists:member(E, L0) andalso
    From >= E andalso
    do_iterate_from(E, I1, lists:delete(E, L0), reversed).

%% --- larger/2 -------------------------------------------------------
prop_larger() ->
    ?FORALL(
        {S, O, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), lists:usort(L1), L1 ++ L2}
        ),
        lists:all(
            fun(E) ->
                gb_sets:larger(E, S) =:= do_larger(E, O)
            end,
            L
         )
    ).

do_larger(_, []) ->
    none;
do_larger(E, [X | _]) when X > E ->
    {found, X};
do_larger(E, [X | R]) when X =< E ->
    do_larger(E, R).

%% --- largest/1 ------------------------------------------------------
prop_largest() ->
    ?FORALL(
        {Set, Largest},
        ?LET(
            L,
            non_empty(ct_proper_ext:safe_list()),
            begin
                L1 = lists:usort(L),
                {gb_sets:from_list(L1), lists:last(L1)}
            end
        ),
        Largest =:= gb_sets:largest(Set)
    ).

%% --- singleton/1 ----------------------------------------------------
prop_singleton() ->
    ?FORALL(
        E,
        ct_proper_ext:safe_any(),
        [E] =:= gb_sets:to_list(gb_sets:singleton(E))
    ).

%% --- smaller/2 -------------------------------------------------------
prop_smaller() ->
    ?FORALL(
        {S, O, L},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(),
             non_empty(ct_proper_ext:safe_list())},
            {gb_sets:from_list(L1), lists:reverse(lists:usort(L1)), L1 ++ L2}
        ),
        lists:all(
            fun(E) ->
                gb_sets:smaller(E, S) =:= do_smaller(E, O)
            end,
            L
         )
    ).

do_smaller(_, []) ->
    none;
do_smaller(E, [X | _]) when X < E ->
    {found, X};
do_smaller(E, [X | R]) when X >= E ->
    do_smaller(E, R).

%% --- smallest/1 -----------------------------------------------------
prop_smallest() ->
    ?FORALL(
        {Set, Smallest},
        ?LET(
            L,
            non_empty(ct_proper_ext:safe_list()),
            begin
                L1 = lists:usort(L),
                {gb_sets:from_list(L1), hd(L1)}
            end
        ),
        Smallest =:= gb_sets:smallest(Set)
    ).

%% --- take_largest/1 -------------------------------------------------
prop_take_largest() ->
    ?FORALL(
        {S, Largest},
        ?LET(
            L,
            non_empty(ct_proper_ext:safe_list()),
            begin
                L1 = lists:usort(L),
                {gb_sets:from_list(L1), lists:last(L1)}
            end
        ),
        begin
            {Largest1, S1} = gb_sets:take_largest(S),
            Largest1 =:= Largest andalso
            gb_sets:is_equal(S1, gb_sets:del_element(Largest, S))
        end
    ).

%% --- take_smallest/1 ------------------------------------------------
prop_take_smallest() ->
    ?FORALL(
        {S, Smallest},
        ?LET(
            L,
            non_empty(ct_proper_ext:safe_list()),
            begin
                L1 = lists:usort(L),
                {gb_sets:from_list(L1), hd(L1)}
            end
        ),
        begin
            {Smallest1, S1} = gb_sets:take_smallest(S),
            Smallest1 =:= Smallest andalso
            gb_sets:is_equal(S1, gb_sets:del_element(Smallest, S))
        end
    ).
