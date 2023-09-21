%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2023. All Rights Reserved.
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
-module(lists_prop).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% all/2
prop_all_true() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        lists:all(fun(_) -> true end, InList)
    ).

prop_all_false() ->
    ?FORALL(
        {InList, Elem},
        ?LET(
            {F, R, E},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             make_ref()},
            {F ++ [E|R], E}
        ),
        not lists:all(fun(T) -> T =/= Elem end, InList)
    ).

%% any/2
prop_any_true() ->
    ?FORALL(
        {InList, Elem},
        ?LET(
            {F, R, E},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             make_ref()},
            {F ++ [E|R], E}
        ),
        lists:any(fun(T) -> T =:= Elem end, InList)
    ).

prop_any_false() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        not lists:any(fun(_) -> false end, InList)
    ).

%% append/1
prop_append_1() ->
    ?FORALL(
        InLists,
        list(ct_proper_ext:safe_list()),
        check_appended(InLists, lists:append(InLists))
    ).

%% append/2
prop_append_2() ->
    ?FORALL(
        {InList1, InList2},
        {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
        lists:append(InList1, InList2) =:= InList1 ++ InList2
    ).

%% concat/1
prop_concat() ->
    ?FORALL(
        {InList, ExpString},
        gen_list_fold(
            oneof([ct_proper_ext:safe_atom(), number(), string()]),
            fun
                (A, Acc) when is_atom(A) -> Acc ++ atom_to_list(A);
                (I, Acc) when is_integer(I) -> Acc ++ integer_to_list(I);
                (F, Acc) when is_float(F) -> Acc ++ float_to_list(F);
                (L, Acc) when is_list(L) -> Acc ++ L
            end,
            []
        ),
        lists:concat(InList) =:= ExpString
    ).

%% delete/2
prop_delete() ->
    ?FORALL(
        {InList, DelElem},
        ?LET(
            {F, R, E},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_any()},
            {F ++ [E|R], E}
        ),
        begin
            DeletedList = lists:delete(DelElem, InList),
            length(DeletedList) =:= length(InList) - 1 andalso
            check_deleted(DelElem, InList, DeletedList)
        end
    ).

prop_delete_absent() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        lists:delete(make_ref(), InList) =:= InList
    ).

%% droplast/1
prop_droplast() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        try
            lists:droplast(InList) =:= lists:reverse(tl(lists:reverse(InList)))
        catch
            error:_ ->
                InList =:= []
        end
    ).

%% dropwhile/2
prop_dropwhile() ->
    ?FORALL(
        {Pred, InList, ExpList},
        ?LET(
            Fn,
            function1(boolean()),
            ?LET(
                {L, {_, DL}},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(E, {Drop, Acc}) ->
                        case Drop andalso Fn(E) of
                            true -> {true, Acc};
                            false -> {false, Acc ++ [E]}
                        end
                    end,
                    {true, []}
                ),
                {Fn, L, DL}
            )
        ),
        lists:dropwhile(Pred, InList) =:= ExpList
    ).

%% duplicate/2
prop_duplicate() ->
    ?FORALL(
        {N, Term, ExpList},
        ?LET(
            T,
            ct_proper_ext:safe_any(),
            ?LET(L, list(T), {length(L), T, L})
        ),
        lists:duplicate(N, Term) =:= ExpList
    ).

%% enumerate/1
prop_enumerate_1() ->
    ?FORALL(
        {InList, ExpList},
        ?LET(
            {L, {_, EL}},
            gen_list_fold(
                ct_proper_ext:safe_any(),
                fun(T, {I, Acc}) ->
                    {I + 1, Acc ++ [{I, T}]}
                end,
                {1, []}
            ),
            {L, EL}
        ),
        lists:enumerate(InList) =:= ExpList
    ).

%% enumerate/2
prop_enumerate_2() ->
    ?FORALL(
        {StartIndex, InList, ExpList},
        ?LET(
            N,
            integer(),
            ?LET(
                {L, {_, EL}},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, {I, Acc}) ->
                        {I + 1, Acc ++ [{I, T}]}
                    end,
                    {N, []}
                ),
                {N, L, EL}
            )
        ),
        lists:enumerate(StartIndex, InList) =:= ExpList
    ).

%% enumerate/3
prop_enumerate_3() ->
    ?FORALL(
        {StartIndex, Step, InList, ExpList},
        ?LET(
	    {N, S},
	    {integer(), integer()},
            ?LET(
                {L, {_, EL}},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, {I, Acc}) ->
                        {I + S, Acc ++ [{I, T}]}
                    end,
                    {N, []}
                ),
                {N, S, L, EL}
            )
        ),
        lists:enumerate(StartIndex, Step, InList) =:= ExpList
    ).

%% filter/2
prop_filter() ->
    ?FORALL(
        {Pred, InList, ExpList},
        ?LET(
            P,
            function1(boolean()),
            ?LET(
                {L, F},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, Acc) ->
                        case P(T) of
                            true -> Acc ++ [T];
                            false -> Acc
                        end
                    end,
                    []
                ),
                {P, L, F}
            )
        ),
        lists:filter(Pred, InList) =:= ExpList
    ).

%% filtermap/2
prop_filtermap() ->
    ?FORALL(
        {FilterMapFn, InList, ExpList},
        ?LET(
            Fn,
            function1(oneof([true, false, {true, ct_proper_ext:safe_any()}])),
            ?LET(
                {L, FM},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, Acc) ->
                        case Fn(T) of
                            false -> Acc;
                            true -> Acc ++ [T];
                            {true, T1} -> Acc ++ [T1]
                        end
                    end,
                    []
                ),
                {Fn, L, FM}
            )
        ),
        lists:filtermap(FilterMapFn, InList) =:= ExpList
    ).

%% flatlength/1
prop_flatlength() ->
    ?FORALL(
        {DeepList, Len},
        gen_list_deepfold(fun(_, _, Cnt) -> Cnt + 1 end, 0),
        lists:flatlength(DeepList) =:= Len
    ).

%% flatmap/2
prop_flatmap() ->
    ?FORALL(
        {MapFn, InList, ExpList},
        ?LET(
            Fn,
            function1(ct_proper_ext:safe_list()),
            ?LET(
                {L, FlatMapped},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, Acc) ->
                        Acc ++ Fn(T)
                    end,
                    []
                ),
                {Fn, L, FlatMapped}
            )
        ),
        lists:flatmap(MapFn, InList) =:= ExpList
    ).

%% flatten/1
prop_flatten_1() ->
    ?FORALL(
        {DeepList, FlatList},
        gen_list_deepfold(fun(_, E, Acc) -> Acc ++ [E] end, []),
        lists:flatten(DeepList) =:= FlatList
    ).

%% flatten/2
prop_flatten_2() ->
    ?FORALL(
        {{DeepList, FlatList}, Tail},
        {gen_list_deepfold(fun(_, E, Acc) -> Acc ++ [E] end, []),
         ct_proper_ext:safe_list()},
        lists:flatten(DeepList, Tail) =:= FlatList ++ Tail
    ).

%% foldl/3
prop_foldl() ->
    ?FORALL(
        {FoldFn, InList, Acc0, Exp},
        ?LET(
            {Fn, Acc0},
            {function2(ct_proper_ext:safe_any()), ct_proper_ext:safe_any()},
            ?LET(
                {L, V},
                gen_list_fold(ct_proper_ext:safe_any(), Fn, Acc0),
                {Fn, L, Acc0, V}
            )
        ),
        lists:foldl(FoldFn, Acc0, InList) =:= Exp
    ).

%% foldr/3
prop_foldr() ->
    ?FORALL(
        {FoldFn, InList, Acc0, Exp},
        ?LET(
            {Fn, Acc0},
            {function2(ct_proper_ext:safe_any()), ct_proper_ext:safe_any()},
            ?LET(
                {L, V},
                gen_list_fold(ct_proper_ext:safe_any(), Fn, Acc0),
                {Fn, lists:reverse(L), Acc0, V}
            )
        ),
        lists:foldr(FoldFn, Acc0, InList) =:= Exp
    ).

%% foreach/2
prop_foreach() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        begin
            Tag = make_ref(),
            lists:foreach(fun(E) -> self() ! {Tag, E} end, InList),
            [receive {Tag, T} -> T after 100 -> error(timeout) end || _ <- InList] =:= InList
        end
    ).

%% join/2
prop_join() ->
    ?FORALL(
        {Sep, InList},
        {ct_proper_ext:safe_any(), ct_proper_ext:safe_list()},
        check_joined(Sep, InList, lists:join(Sep, InList))
    ).

%% keydelete/3
prop_keydelete() ->
    ?FORALL(
        {Key, N, InList},
        ?LET(
            {K, N},
            {ct_proper_ext:safe_any(), range(1, 5)},
            ?LET(
                {F, R, E},
                {ct_proper_ext:safe_list(),
                 ct_proper_ext:safe_list(),
                 gen_keytuple(K, N, N + 3)},
                {K, N, F ++ [E|R]}
            )
        ),
        begin
            DeletedL = lists:keydelete(Key, N, InList),
            length(DeletedL) =:= length(InList) - 1 andalso
            check_keydeleted(Key, N, InList, DeletedL)
        end
    ).

prop_keydelete_absent() ->
    ?FORALL(
        {N, InList},
        {pos_integer(), ct_proper_ext:safe_list()},
        lists:keydelete(make_ref(), N, InList) =:= InList
    ).

%% keyfind/3
prop_keyfind() ->
    ?FORALL(
        {Key, N, InList},
        ?LET(
            {K, N},
            {ct_proper_ext:safe_any(), range(1, 5)},
            ?LET(
                {F, R, E},
                {ct_proper_ext:safe_list(),
                 ct_proper_ext:safe_list(),
                 gen_keytuple(K, N, N + 3)},
                {K, N, F ++ [E|R]}
            )
        ),
        begin
            Found = lists:keyfind(Key, N, InList),
            is_tuple(Found) andalso
            tuple_size(Found) >= N andalso
            element(N, Found) == Key
        end
    ).

prop_keyfind_absent() ->
    ?FORALL(
        {N, InList},
        {pos_integer(), ct_proper_ext:safe_list()},
        not lists:keyfind(make_ref(), N, InList)
    ).

%% keymap/3
prop_keymap() ->
    ?FORALL(
        {MapFn, N, InList, ExpList},
        ?LET(
            Fn,
            function([ct_proper_ext:safe_any()], ct_proper_ext:safe_any()),
            ?LET(
                N,
                range(1, 5),
                ?LET(
                    {L, M},
                    gen_list_fold(
                        gen_tuple(N, N + 3),
                        fun(T, Acc) ->
                            Acc ++ [setelement(N, T, Fn(element(N, T)))]
                        end,
                        []
                    ),
                    {Fn, N, L, M}
                )
            )
        ),
        lists:keymap(MapFn, N, InList) =:= ExpList
    ).

%% keymember/3
prop_keymember() ->
    ?FORALL(
        {Key, N, InList},
        ?LET(
            {K, N},
            {ct_proper_ext:safe_any(), range(1, 5)},
            ?LET(
                {F, R, E},
                {ct_proper_ext:safe_list(),
                 ct_proper_ext:safe_list(),
                 gen_keytuple(K, N, N + 3)},
                {K, N, F ++ [E|R]}
            )
        ),
        lists:keymember(Key, N, InList)
    ).

prop_keymember_absent() ->
    ?FORALL(
        {N, InList},
        {pos_integer(), ct_proper_ext:safe_list()},
        not lists:keymember(make_ref(), N, InList)
    ).

%% keymerge/3
prop_keymerge() ->
    ?FORALL(
        {N, InList1, InList2},
        ?LET(
            N,
            range(1, 5),
            ?LET(
                {L1, L2},
                {list(gen_tuple(N, N+3)), list(gen_tuple(N, N+3))},
                {N, lists:sort(L1), lists:sort(L2)}
            )
        ),
        check_merged(
            fun (E1, E2) -> element(N, E1) =< element(N, E2) end,
            [InList1, InList2],
            lists:keymerge(N, InList1, InList2)
        )
    ).

prop_keymerge_invalid() ->
    ?FORALL(
        {N, InList, X, Y},
        ?LET(
            N,
            range(1, 5),
            ?LET(
                {L, X, Y},
                {list(gen_tuple(N, N+3)), non_list(), non_list()},
                {N, L, X, Y}
            )
        ),
        expect_error(fun lists:keymerge/3, [N, InList, Y]) andalso
        expect_error(fun lists:keymerge/3, [N, X, InList]) andalso
        expect_error(fun lists:keymerge/3, [N, X, Y])
    ).

%% keyreplace/4
prop_keyreplace() ->
    ?FORALL(
        {Key, N, InList, Replacement},
        ?LET(
            {K, N},
            {ct_proper_ext:safe_any(), range(1, 5)},
            ?LET(
                {F, R, E0, E1},
                {ct_proper_ext:safe_list(),
                 ct_proper_ext:safe_list(),
                 gen_keytuple(K, N, N + 3),
                 gen_tuple()},
                {K, N, F ++ [E0|R], E1}
            )
        ),
        check_keyreplaced(Key, N, Replacement, InList, lists:keyreplace(Key, N, InList, Replacement))
    ).

prop_keyreplace_absent() ->
    ?FORALL(
        {N, InList, Replacement},
        {pos_integer(), ct_proper_ext:safe_list(), gen_tuple()},
        lists:keyreplace(make_ref(), N, InList, Replacement) =:= InList
    ).

%% keysearch/3
prop_keysearch() ->
    ?FORALL(
        {Key, N, InList},
        ?LET(
            {K, N},
            {ct_proper_ext:safe_any(), range(1, 5)},
            ?LET(
                {F, R, E},
                {ct_proper_ext:safe_list(),
                 ct_proper_ext:safe_list(),
                 gen_keytuple(K, N, N + 3)},
                {K, N, F ++ [E|R]}
            )
        ),
        begin
            {value, Found} = lists:keysearch(Key, N, InList),
            is_tuple(Found) andalso
            tuple_size(Found) >= N andalso
            element(N, Found) == Key
        end
    ).

prop_keysearch_absent() ->
    ?FORALL(
        {N, InList},
        {pos_integer(), ct_proper_ext:safe_list()},
        not lists:keysearch(make_ref(), N, InList)
    ).

%% keysort/2
prop_keysort() ->
    ?FORALL(
        {N, InList},
        ?LET(
            N,
            range(1, 5),
            {N, list(gen_tuple(N, N + 3))}
        ),
        begin
            Sorted = lists:keysort(N, InList),
            length(Sorted) =:= length(InList) andalso
            check_sorted(fun(E1, E2) -> element(N, E1) =< element(N, E2) end, InList, Sorted)
        end
    ).

%% keystore/4
prop_keystore() ->
    ?FORALL(
        {Key, N, InList, ToStore},
        ?LET(
            {K, N},
            {ct_proper_ext:safe_any(), range(1, 5)},
            ?LET(
                {F, R, E0, E1},
                {ct_proper_ext:safe_list(),
                 ct_proper_ext:safe_list(),
                 gen_keytuple(K, N, N + 3),
                 gen_tuple()},
                {K, N, F ++ [E0|R], E1}
            )
        ),
        check_keyreplaced(Key, N, ToStore, InList, lists:keystore(Key, N, InList, ToStore))
    ).

prop_keystore_absent() ->
    ?FORALL(
        {N, InList, ToStore},
        {pos_integer(), ct_proper_ext:safe_list(), gen_tuple()},
        lists:keystore(make_ref(), N, InList, ToStore) =:= InList ++ [ToStore]
    ).

%% keytake/3
prop_keytake() ->
    ?FORALL(
        {Key, N, InList, ExpList, ExpElem},
        ?LET(
           {K, N},
           {make_ref(), range(1, 5)},
           ?LET(
               {F, R, E},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             gen_keytuple(K, N, N + 3)},
            {K, N, F ++ [E|R], F ++ R, E}
           )
        ),
        lists:keytake(Key, N, InList) =:= {value, ExpElem, ExpList}
    ).

prop_keytake_absent() ->
    ?FORALL(
        {N, InList},
        {pos_integer(), ct_proper_ext:safe_list()},
        lists:keytake(make_ref(), N, InList) =:= false
    ).

%% last/1
prop_last() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        try
            lists:last(InList) =:= hd(lists:reverse(InList))
        catch
            error:_ ->
                InList =:= []
        end
    ).

%% map/2
prop_map() ->
    ?FORALL(
        {MapFn, InList, ExpList},
        ?LET(
            Fn,
            function1(ct_proper_ext:safe_any()),
            ?LET(
                {L, M},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, Acc) ->
                        Acc ++ [Fn(T)]
                    end,
                    []
                ),
                {Fn, L, M}
            )
        ),
        lists:map(MapFn, InList) =:= ExpList
    ).

%% mapfoldl/3
prop_mapfoldl() ->
    ?FORALL(
        {MapFoldFn, InList, Acc0, Exp},
        ?LET(
            {MapFn, FoldFn, Acc0},
            {function1(ct_proper_ext:safe_any()),
             function2(ct_proper_ext:safe_any()),
             ct_proper_ext:safe_any()},
            ?LET(
                {L, MV},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, {AccM, AccF}) ->
                        {AccM ++ [MapFn(T)], FoldFn(T, AccF)}
                    end,
                    {[], Acc0}
                ),
                {fun(T, Acc) -> {MapFn(T), FoldFn(T, Acc)} end, L, Acc0, MV}
            )
        ),
        lists:mapfoldl(MapFoldFn, Acc0, InList) =:= Exp
    ).

%% mapfoldr/3
prop_mapfoldr() ->
    ?FORALL(
        {MapFoldFn, InList, Acc0, Exp},
        ?LET(
            {MapFn, FoldFn, Acc0},
            {function1(ct_proper_ext:safe_any()),
             function2(ct_proper_ext:safe_any()),
             ct_proper_ext:safe_any()},
            ?LET(
                {L, MV},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(T, {AccM, AccF}) ->
                        {[MapFn(T)|AccM], FoldFn(T, AccF)}
                    end,
                    {[], Acc0}
                ),
                {fun(T, Acc) -> {MapFn(T), FoldFn(T, Acc)} end, lists:reverse(L), Acc0, MV}
            )
        ),
        lists:mapfoldr(MapFoldFn, Acc0, InList) =:= Exp
    ).

%% max/1
prop_max() ->
    ?FORALL(
        {InList, ExpMax},
        gen_list_fold(ct_proper_ext:safe_any(), fun erlang:max/2),
        try
            lists:max(InList) == ExpMax
        catch
            error:_ ->
                InList =:= []
        end
    ).

%% member/2
prop_member() ->
    ?FORALL(
        {InList, Member},
        ?LET(
            {F, R, E},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_any()},
            {F ++ [E|R], E}
        ),
        lists:member(Member, InList)
    ).

prop_member_absent() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        not lists:member(make_ref(), InList)
    ).

%% merge/1
prop_merge_1() ->
    ?FORALL(
        InLists,
        list(?LET(L, ct_proper_ext:safe_list(), lists:sort(L))),
        check_merged(fun erlang:'=<'/2, InLists, lists:merge(InLists))
    ).

prop_merge_1_invalid() ->
    ?FORALL(
        InLists,
        ?LET(
            {L1, X, L2},
            {list(oneof([non_list(), ct_proper_ext:safe_list()])),
             non_list(),
             list(oneof([non_list(), ct_proper_ext:safe_list()]))},
            L1 ++ [X|L2]
        ),
        expect_error(fun lists:merge/1, [InLists])
    ).

%% merge/2
prop_merge_2() ->
    ?FORALL(
        {InList1, InList2},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {lists:sort(L1), lists:sort(L2)}
        ),
        check_merged(fun erlang:'=<'/2, [InList1, InList2], lists:merge(InList1, InList2))
    ).

prop_merge_2_invalid() ->
    ?FORALL(
        {InList, X, Y},
        {ct_proper_ext:safe_list(), non_list(), non_list()},
        expect_error(fun lists:merge/2, [InList, X]) andalso
        expect_error(fun lists:merge/2, [X, InList]) andalso
        expect_error(fun lists:merge/2, [X, Y])
    ).

%% merge/3
prop_merge_3() ->
    ?FORALL(
        {SortFn, InList1, InList2},
        ?LET(
            {Fn, L1, L2},
            {gen_ordering_fun(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {Fn, lists:sort(Fn, L1), lists:sort(Fn, L2)}
        ),
        check_merged(SortFn, [InList1, InList2], lists:merge(SortFn, InList1, InList2))
    ).

prop_merge_3_invalid() ->
    ?FORALL(
        {SortFn, InList, X, Y},
        {gen_ordering_fun(),
         ct_proper_ext:safe_list(),
         non_list(),
         non_list()},
        expect_error(fun lists:merge/3, [SortFn, InList, Y]) andalso
        expect_error(fun lists:merge/3, [SortFn, X, InList]) andalso
        expect_error(fun lists:merge/3, [SortFn, X, Y])
    ).

%% merge3/3
prop_merge3() ->
    ?FORALL(
        {InList1, InList2, InList3},
        ?LET(
            {L1, L2, L3},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {lists:sort(L1), lists:sort(L2), lists:sort(L3)}
        ),
        check_merged(fun erlang:'=<'/2, [InList1, InList2, InList3], lists:merge3(InList1, InList2, InList3))
    ).

prop_merge3_invalid() ->
    ?FORALL(
        {InList, X, Y, Z},
        {ct_proper_ext:safe_list(), non_list(), non_list(), non_list()},
        expect_error(fun lists:merge/3, [InList, InList, Z]) andalso
        expect_error(fun lists:merge/3, [InList, Y, InList]) andalso
        expect_error(fun lists:merge/3, [InList, Y, Z]) andalso
        expect_error(fun lists:merge/3, [X, InList, Z]) andalso
        expect_error(fun lists:merge/3, [X, Y, InList]) andalso
        expect_error(fun lists:merge/3, [X, Y, Z])
    ).

%% min/1
prop_min() ->
    ?FORALL(
        {InList, ExpMin},
        gen_list_fold(ct_proper_ext:safe_any(), fun erlang:min/2),
        try
            lists:min(InList) == ExpMin
        catch
            error:_ ->
                InList =:= []
        end
    ).

%% nth/2
prop_nth() ->
    ?FORALL(
        {InList, N, ExpElem},
        ?LET(
            {F, R, E},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_any()},
            {F ++ [E|R], length(F)+1, E}
        ),
        lists:nth(N, InList) =:= ExpElem
    ).

prop_nth_outofrange() ->
    ?FORALL(
        {N, InList},
        ?LET(
            {L, Offset},
            {ct_proper_ext:safe_list(), pos_integer()},
            {length(L) + Offset, L}
        ),
        try
            lists:nth(N, InList)
        of
            _ ->
                false
        catch
            error:_ ->
                true
        end
    ).

%% nthtail/2
prop_nthtail() ->
    ?FORALL(
        {InList, N, ExpTail},
        ?LET(
            {F, R},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {F ++ R, length(F), R}
        ),
        lists:nthtail(N, InList) =:= ExpTail
    ).

prop_nthtail_outofrange() ->
    ?FORALL(
        {N, InList},
        ?LET(
            {L, Offset},
            {ct_proper_ext:safe_list(), pos_integer()},
            {length(L) + Offset, L}
        ),
        try
            lists:nthtail(N, InList)
        of
            _ ->
                false
        catch
            error:_ ->
                true
        end
    ).

%% partition/2
prop_partition() ->
    ?FORALL(
        {Pred, InList},
        {function1(boolean()), ct_proper_ext:safe_list()},
        begin
            {Group1, Group2} = lists:partition(Pred, InList),
            check_partitioned(Pred, InList, Group1, Group2)
        end
    ).

%% prefix/2
prop_prefix() ->
    ?FORALL(
        {InList, Prefix},
        ?LET(
            {F, R},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {F ++ R, F}
        ),
        lists:prefix(Prefix, InList) andalso
        not lists:prefix([make_ref()|Prefix], InList) andalso
        not lists:prefix(Prefix ++ [make_ref()], InList) andalso
        (not lists:prefix(Prefix, [make_ref()|InList]) orelse Prefix =:= [])
    ).

%% reverse/1
prop_reverse_1() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        check_reversed(InList, lists:reverse(InList)) andalso
        lists:reverse(lists:reverse(InList)) =:= InList
    ).

%% reverse/2
prop_reverse_2() ->
    ?FORALL(
        {InList, InTail},
        {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
        check_reversed(InList, lists:reverse(InList, InTail), InTail)
    ).

%% search/2
prop_search() ->
    ?FORALL(
        {Pred, InList, ExpElem},
        ?LET(
            {F, R, E},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             make_ref()},
            {fun(T) -> T =:= E end, F ++ [E|R], E}
        ),
        lists:search(Pred, InList) =:= {value, ExpElem}
    ).

prop_search_absent() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        lists:search(fun(_) -> false end, InList) =:= false
    ).

%% seq/2
prop_seq2() ->
    ?FORALL(
        {From, To},
        {integer(), integer()},
        try
            lists:seq(From, To)
        of
            Seq ->
                To >= From - 1 andalso
                check_seq(Seq, From, To, 1)
        catch
            error:_ ->
                To < From - 1
        end
    ).

%% seq/3
prop_seq3() ->
    ?FORALL(
        {From, To, Step},
        {integer(), integer(), integer()},
        try
            lists:seq(From, To, Step)
        of
            Seq when Step > 0 ->
                To >= From - Step andalso
                check_seq(Seq, From, To, Step);
            Seq when Step < 0 ->
                To =< From - Step andalso
                check_seq(Seq, From, To, Step);
            Seq when Step =:= 0 ->
                From =:= To andalso
                check_seq(Seq, From, To, Step)
        catch
            error:_ when Step > 0 ->
                To < From - Step;
            error:_ when Step < 0 ->
                To > From - Step;
            error:_ when Step =:= 0 ->
                From =/= To
        end
    ).

%% sort/1
prop_sort_1() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        begin
            Sorted = lists:sort(InList),
            length(Sorted) =:= length(InList) andalso
            check_sorted(InList, Sorted)
        end
    ).

%% sort/2
prop_sort_2() ->
    ?FORALL(
        {SortFn, InList},
        {gen_ordering_fun(), ct_proper_ext:safe_list()},
        begin
            Sorted = lists:sort(SortFn, InList),
            length(Sorted) =:= length(InList) andalso
            check_sorted(SortFn, InList, Sorted)
        end
    ).

%% split/2
prop_split() ->
    ?FORALL(
        {N, InList, ExpList1, ExpList2},
        ?LET(
            {F, R},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {length(F), F ++ R, F, R}
        ),
        lists:split(N, InList) =:= {ExpList1, ExpList2}
    ).

prop_split_outofrange() ->
    ?FORALL(
        {N, InList},
        ?LET(
            {L, Offset},
            {ct_proper_ext:safe_list(), pos_integer()},
            {length(L) + Offset, L}
        ),
        try
            lists:split(N, InList)
        of
            _ ->
                false
        catch
            error:_ ->
                true
        end
    ).

%% splitwith/2
prop_splitwith() ->
    ?FORALL(
        {Pred, InList},
        {function1(boolean()), ct_proper_ext:safe_list()},
        begin
            {Part1, Part2} = lists:splitwith(Pred, InList),
            check_splitwithed(Pred, InList, Part1, Part2)
        end
    ).

%% sublist/2
prop_sublist_2() ->
    ?FORALL(
        {Len, InList, ExpList},
        ?LET(
            {F, R},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {length(F), F ++ R, F}
        ),
        lists:sublist(InList, Len) =:= ExpList
    ).

%% sublist/3
prop_sublist_3() ->
    ?FORALL(
        {Start, Len, InList, ExpList},
        ?LET(
            {F, M, R},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {length(F)+1, length(M), F ++ M ++ R, M}
        ),
        lists:sublist(InList, Start, Len) =:= ExpList
    ).

%% subtract/2
prop_subtract() ->
    ?FORALL(
        {InList, SubtractList},
        ?LET(
            {L, B, S},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {L ++ B, S ++ B}
        ),
        lists:subtract(InList, SubtractList) =:= InList -- SubtractList
    ).

%% suffix/2
prop_suffix() ->
    ?FORALL(
        {InList, Suffix},
        ?LET(
            {F, R},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {F ++ R, R}
        ),
        lists:suffix(Suffix, InList) andalso
        not lists:suffix([make_ref()|Suffix], InList) andalso
        not lists:suffix(Suffix ++ [make_ref()], InList) andalso
        (not lists:suffix(Suffix, InList ++ [make_ref()]) orelse Suffix =:= [])
    ).

%% sum/1
prop_sum() ->
    ?FORALL(
        {InList, ExpSum},
        gen_list_fold(number(), fun erlang:'+'/2, 0),
        lists:sum(InList) =:= ExpSum
    ).

%% takewhile/2
prop_takewhile() ->
    ?FORALL(
        {Pred, InList, ExpList},
        ?LET(
            Fn,
            function1(boolean()),
            ?LET(
                {L, {_, TL}},
                gen_list_fold(
                    ct_proper_ext:safe_any(),
                    fun(E, {Take, Acc}) ->
                        case Take andalso Fn(E) of
                            true -> {true, Acc ++ [E]};
                            false -> {false, Acc}
                        end
                    end,
                    {true, []}
                ),
                {Fn, L, TL}
            )
        ),
        lists:takewhile(Pred, InList) =:= ExpList
    ).

%% ukeymerge/3
prop_ukeymerge() ->
    ?FORALL(
        {N, InList1, InList2},
        ?LET(
            N,
            range(1, 5),
            ?LET(
                {L1, L2},
                {list(gen_tuple(N, N+3)), list(gen_tuple(N, N+3))},
                {N, lists:ukeysort(N, L1), lists:ukeysort(N, L2)}
            )
        ),
        check_umerged(
            fun(E1, E2) -> element(N, E1) =< element(N, E2) end,
            [InList1, InList2],
            lists:ukeymerge(N, InList1, InList2)
        )
    ).

prop_ukeymerge_invalid() ->
    ?FORALL(
        {N, InList, X, Y},
        ?LET(
            N,
            range(1, 5),
            ?LET(
                {L, X, Y},
                {list(gen_tuple(N, N+3)), non_list(), non_list()},
                {N, L, X, Y}
            )
        ),
        expect_error(fun lists:ukeymerge/3, [N, InList, Y]) andalso
        expect_error(fun lists:ukeymerge/3, [N, X, InList]) andalso
        expect_error(fun lists:ukeymerge/3, [N, X, Y])
    ).

%% ukeysort/2
prop_ukeysort() ->
    ?FORALL(
        {N, InList},
        ?LET(
            N,
            range(1, 5),
            {N, list(gen_tuple(N, N + 3))}
        ),
        begin
            Sorted = lists:ukeysort(N, InList),
            length(Sorted) =< length(InList) andalso
            check_usorted(fun(E1, E2) -> element(N, E1) =< element(N, E2) end, InList, Sorted)
        end
    ).

%% umerge/1
prop_umerge_1() ->
    ?FORALL(
        InLists,
        list(?LET(L, ct_proper_ext:safe_list(), lists:usort(L))),
        check_umerged(InLists, lists:umerge(InLists))
    ).

prop_umerge_1_invalid() ->
    ?FORALL(
        InList,
        ?LET(
            {L1, X, L2},
	    {list(oneof([non_list(), ct_proper_ext:safe_list()])),
             non_list(),
             list(oneof([non_list(), ct_proper_ext:safe_list()]))},
	    L1 ++ [X|L2]
        ),
	expect_error(fun lists:umerge/1, [InList])
    ).

%% umerge/2
prop_umerge_2() ->
    ?FORALL(
        {InList1, InList2},
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {lists:usort(L1), lists:usort(L2)}
        ),
        check_umerged([InList1, InList2], lists:umerge(InList1, InList2))
    ).

prop_umerge_2_invalid() ->
    ?FORALL(
        {InList, X, Y},
	{ct_proper_ext:safe_list(), non_list(), non_list()},
	expect_error(fun lists:umerge/2, [InList, Y]) andalso
	expect_error(fun lists:umerge/2, [X, InList]) andalso
	expect_error(fun lists:umerge/2, [X, Y])
    ).

%% umerge/3
prop_umerge_3() ->
    ?FORALL(
        {SortFn, InList1, InList2},
        ?LET(
            {Fn, L1, L2},
            {gen_ordering_fun(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {Fn, lists:usort(Fn, L1), lists:usort(Fn, L2)}
        ),
        check_umerged(SortFn, [InList1, InList2], lists:umerge(SortFn, InList1, InList2))
    ).

prop_umerge_3_invalid() ->
    ?FORALL(
        {SortFn, InList, X, Y},
	{gen_ordering_fun(),
         ct_proper_ext:safe_list(),
         non_list(),
         non_list()},
	expect_error(fun lists:umerge/3, [SortFn, InList, Y]) andalso
	expect_error(fun lists:umerge/3, [SortFn, X, InList]) andalso
	expect_error(fun lists:umerge/3, [SortFn, X, Y])
    ).

%% umerge3/3
prop_umerge3() ->
    ?FORALL(
        {InList1, InList2, InList3},
        ?LET(
            {L1, L2, L3},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {lists:usort(L1), lists:usort(L2), lists:usort(L3)}
        ),
        check_umerged([InList1, InList2, InList3], lists:umerge3(InList1, InList2, InList3))
    ).

prop_umerge3_invalid() ->
    ?FORALL(
        {InList, X, Y, Z},
	{ct_proper_ext:safe_list(), non_list(), non_list(), non_list()},
	expect_error(fun lists:umerge3/3, [InList, InList, Z]) andalso
	expect_error(fun lists:umerge3/3, [InList, Y, InList]) andalso
	expect_error(fun lists:umerge3/3, [InList, Y, Z]) andalso
	expect_error(fun lists:umerge3/3, [X, InList, InList]) andalso
	expect_error(fun lists:umerge3/3, [X, InList, Z]) andalso
	expect_error(fun lists:umerge3/3, [X, Y, InList]) andalso
	expect_error(fun lists:umerge3/3, [X, Y, Z])
    ).

%% uniq/1
prop_uniq_1() ->
    ?FORALL(
        InList,
        ?LET(
            {L, M},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
	    ?LET(
	        S,
		vector(length(L) + 2 * length(M), integer()),
	        [E || {_, E} <- lists:sort(lists:zip(S, L ++ M ++ M))]
	    )
        ),
        check_uniqed(InList, lists:uniq(InList))
    ).

%% uniq/2
prop_uniq_2() ->
    ?FORALL(
        {UniqFn, InList},
        {function1(oneof([a, b, c])), ct_proper_ext:safe_list()},
        check_uniqed(UniqFn, InList, lists:uniq(UniqFn, InList))
    ).

%% unzip/1
prop_unzip() ->
    ?FORALL(
        {InList, {ExpList1, ExpList2}},
        gen_list_fold(
            {ct_proper_ext:safe_any(), ct_proper_ext:safe_any()},
            fun({T1, T2}, {L1, L2}) ->
                {L1 ++ [T1], L2 ++ [T2]}
            end,
            {[], []}
        ),
        lists:unzip(InList) =:= {ExpList1, ExpList2}
    ).

%% unzip3/1
prop_unzip3() ->
    ?FORALL(
        {InList, {ExpList1, ExpList2, ExpList3}},
        gen_list_fold(
            {ct_proper_ext:safe_any(),
             ct_proper_ext:safe_any(),
             ct_proper_ext:safe_any()},
            fun({T1, T2, T3}, {L1, L2, L3}) ->
                {L1 ++ [T1], L2 ++ [T2], L3 ++ [T3]}
            end,
            {[], [], []}
        ),
        lists:unzip3(InList) =:= {ExpList1, ExpList2, ExpList3}
    ).

%% usort/1
prop_usort_1() ->
    ?FORALL(
        InList,
        ct_proper_ext:safe_list(),
        begin
            Sorted = lists:usort(InList),
            length(Sorted) =< length(InList) andalso
            check_usorted(InList, Sorted)
        end
    ).

%% usort/2
prop_usort_2() ->
    ?FORALL(
        {SortFn, InList},
        {gen_ordering_fun(), ct_proper_ext:safe_list()},
        begin
            Sorted = lists:usort(SortFn, InList),
            length(Sorted) =< length(InList) andalso
            check_usorted(SortFn, InList, Sorted)
        end
    ).

%% zip/2
prop_zip_2() ->
    ?FORALL(
        {ExpList, {InList1, InList2}},
        gen_list_fold(
            {ct_proper_ext:safe_any(), ct_proper_ext:safe_any()},
            fun({T1, T2}, {L1, L2}) ->
                {L1 ++ [T1], L2 ++ [T2]}
            end,
            {[], []}
        ),
        lists:zip(InList1, InList2) =:= ExpList
    ).

%% zip/3
prop_zip_3() ->
	?FORALL(
		{{ExpList, {InList1, InList2}}, ExtraList},
		{
			gen_list_fold(
				{ct_proper_ext:safe_any(), ct_proper_ext:safe_any()},
				fun({T1, T2}, {L1, L2}) ->
					{L1 ++ [T1], L2 ++ [T2]}
				end,
				{[], []}
			),
			non_empty(ct_proper_ext:safe_list())
		},
		begin
			Tag = make_ref(),

			Res1 = ExpList =:= lists:zip(InList1, InList2, fail) andalso
			       ExpList =:= lists:zip(InList1, InList2, trim) andalso
			       ExpList =:= lists:zip(InList1, InList2, {pad, {Tag, Tag}}),

			Res2 = try lists:zip(InList1, InList2 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zip(InList1 ++ ExtraList, InList2, fail) of _ -> false catch error:_ -> true end,

			Res3 = ExpList =:= lists:zip(InList1, InList2 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zip(InList1 ++ ExtraList, InList2, trim),

			Padded1 = lists:zip(InList1, InList2 ++ ExtraList, {pad, {Tag, Tag}}),
			Padded2 = lists:zip(InList1 ++ ExtraList, InList2, {pad, {Tag, Tag}}),
			Res4 = Padded1 =:= ExpList ++ [{Tag, X} || X <- ExtraList] andalso
			       Padded2 =:= ExpList ++ [{X, Tag} || X <- ExtraList],

			Res1 andalso Res2 andalso Res3 andalso Res4
		end
	).

%% zip3/3
prop_zip3_3() ->
    ?FORALL(
        {ExpList, {InList1, InList2, InList3}},
        gen_list_fold(
            {ct_proper_ext:safe_any(),
             ct_proper_ext:safe_any(),
             ct_proper_ext:safe_any()},
            fun({T1, T2, T3}, {L1, L2, L3}) ->
                {L1 ++ [T1], L2 ++ [T2], L3 ++ [T3]}
            end,
            {[], [], []}
        ),
        lists:zip3(InList1, InList2, InList3) =:= ExpList
    ).

%% zip3/4
prop_zip3_4() ->
	?FORALL(
		{{ExpList, {InList1, InList2, InList3}}, ExtraList},
		{
			gen_list_fold(
				{ct_proper_ext:safe_any(),
				 ct_proper_ext:safe_any(),
				 ct_proper_ext:safe_any()},
				fun({T1, T2, T3}, {L1, L2, L3}) ->
					{L1 ++ [T1], L2 ++ [T2], L3 ++ [T3]}
				end,
				{[], [], []}
			),
			non_empty(ct_proper_ext:safe_list())
		},
		begin
			Tag = make_ref(),

			Res1 = ExpList =:= lists:zip3(InList1, InList2, InList3, fail) andalso
			       ExpList =:= lists:zip3(InList1, InList2, InList3, trim) andalso
			       ExpList =:= lists:zip3(InList1, InList2, InList3, {pad, {Tag, Tag, Tag}}),

			Res2 = try lists:zip3(InList1, InList2, InList3 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zip3(InList1, InList2 ++ ExtraList, InList3, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zip3(InList1, InList2 ++ ExtraList, InList3 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zip3(InList1 ++ ExtraList, InList2, InList3, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zip3(InList1 ++ ExtraList, InList2, InList3 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zip3(InList1 ++ ExtraList, InList2 ++ ExtraList, InList3, fail) of _ -> false catch error:_ -> true end,

			Res3 = ExpList =:= lists:zip3(InList1, InList2, InList3 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zip3(InList1, InList2 ++ ExtraList, InList3, trim) andalso
			       ExpList =:= lists:zip3(InList1, InList2 ++ ExtraList, InList3 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zip3(InList1 ++ ExtraList, InList2, InList3, trim) andalso
			       ExpList =:= lists:zip3(InList1 ++ ExtraList, InList2, InList3 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zip3(InList1 ++ ExtraList, InList2 ++ ExtraList, InList3, trim),

			Padded1 = lists:zip3(InList1, InList2, InList3 ++ ExtraList, {pad, {Tag, Tag, Tag}}),
			Padded2 = lists:zip3(InList1, InList2 ++ ExtraList, InList3, {pad, {Tag, Tag, Tag}}),
			Padded3 = lists:zip3(InList1, InList2 ++ ExtraList, InList3 ++ ExtraList, {pad, {Tag, Tag, Tag}}),
			Padded4 = lists:zip3(InList1 ++ ExtraList, InList2, InList3, {pad, {Tag, Tag, Tag}}),
			Padded5 = lists:zip3(InList1 ++ ExtraList, InList2, InList3 ++ ExtraList, {pad, {Tag, Tag, Tag}}),
			Padded6 = lists:zip3(InList1 ++ ExtraList, InList2 ++ ExtraList, InList3, {pad, {Tag, Tag, Tag}}),
			Res4 = Padded1 =:= ExpList ++ [{Tag, Tag, X} || X <- ExtraList] andalso
			       Padded2 =:= ExpList ++ [{Tag, X, Tag} || X <- ExtraList] andalso
			       Padded3 =:= ExpList ++ [{Tag, X, X} || X <- ExtraList] andalso
			       Padded4 =:= ExpList ++ [{X, Tag, Tag} || X <- ExtraList] andalso
			       Padded5 =:= ExpList ++ [{X, Tag, X} || X <- ExtraList] andalso
			       Padded6 =:= ExpList ++ [{X, X, Tag} || X <- ExtraList],

			Res1 andalso Res2 andalso Res3 andalso Res4
		end
	).

%% zipwith/3
prop_zipwith_3() ->
    ?FORALL(
        {ZipFn, InList1, InList2, ExpList},
        ?LET(
            Fn,
            function2(ct_proper_ext:safe_any()),
            ?LET(
                {_, {L1, L2, Z}},
                gen_list_fold(
                    {ct_proper_ext:safe_any(), ct_proper_ext:safe_any()},
                    fun({T1, T2}, {L1, L2, Z}) ->
                        {L1 ++ [T1], L2 ++ [T2], Z ++ [Fn(T1, T2)]}
                    end,
                    {[], [], []}
                ),
                {Fn, L1, L2, Z}
            )
        ),
        lists:zipwith(ZipFn, InList1, InList2) =:= ExpList
    ).

%% zipwith/4
prop_zipwith_4() ->
	?FORALL(
		{ZipFn, InList1, InList2, ExpList, ExtraList},
		?LET(
			{Extra, Fn},
			{non_empty(ct_proper_ext:safe_list()),
			 function2(ct_proper_ext:safe_any())},
			?LET(
				{_, {L1, L2, Z}},
				gen_list_fold(
					{ct_proper_ext:safe_any(), ct_proper_ext:safe_any()},
					fun({T1, T2}, {L1, L2, Z}) ->
						{L1 ++ [T1], L2 ++ [T2], Z ++ [Fn(T1, T2)]}
					end,
					{[], [], []}
				),
				{Fn, L1, L2, Z, Extra}
			)
		),
		begin
			Tag = make_ref(),

			Res1 = ExpList =:= lists:zipwith(ZipFn, InList1, InList2, fail) andalso
			       ExpList =:= lists:zipwith(ZipFn, InList1, InList2, trim) andalso
			       ExpList =:= lists:zipwith(ZipFn, InList1, InList2, {pad, {Tag, Tag}}),

			Res2 = try lists:zipwith(ZipFn, InList1, InList2 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zipwith(ZipFn, InList1 ++ ExtraList, InList2, fail) of _ -> false catch error:_ -> true end,

			Res3 = ExpList =:= lists:zipwith(ZipFn, InList1, InList2 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zipwith(ZipFn, InList1 ++ ExtraList, InList2, trim),

			Padded1 = lists:zipwith(ZipFn, InList1, InList2 ++ ExtraList, {pad, {Tag, Tag}}),
			Padded2 = lists:zipwith(ZipFn, InList1 ++ ExtraList, InList2, {pad, {Tag, Tag}}),
			Res4 = Padded1 =:= ExpList ++ [ZipFn(Tag, X) || X <- ExtraList] andalso
			       Padded2 =:= ExpList ++ [ZipFn(X, Tag) || X <- ExtraList],

			Res1 andalso Res2 andalso Res3 andalso Res4
		end
	).

%% zipwith3/4
prop_zipwith3_4() ->
    ?FORALL(
        {ZipFn, InList1, InList2, InList3, ExpList},
        ?LET(
            Fn,
            function3(ct_proper_ext:safe_any()),
            ?LET(
                {_, {L1, L2, L3, Z}},
                gen_list_fold(
                    {ct_proper_ext:safe_any(),
                     ct_proper_ext:safe_any(),
                     ct_proper_ext:safe_any()},
                    fun({T1, T2, T3}, {L1, L2, L3, Z}) ->
                        {L1 ++ [T1], L2 ++ [T2], L3 ++ [T3], Z ++ [Fn(T1, T2, T3)]}
                    end,
                    {[], [], [], []}
                ),
                {Fn, L1, L2, L3, Z}
            )
        ),
        lists:zipwith3(ZipFn, InList1, InList2, InList3) =:= ExpList
    ).

%% zipwith3/5
prop_zipwith3_5() ->
	?FORALL(
		{ZipFn, InList1, InList2, InList3, ExpList, ExtraList},
		?LET(
			{Extra, Fn},
			{non_empty(ct_proper_ext:safe_list()),
			 function3(ct_proper_ext:safe_any())},
			?LET(
				{_, {L1, L2, L3, Z}},
				gen_list_fold(
					{ct_proper_ext:safe_any(),
					 ct_proper_ext:safe_any(),
					 ct_proper_ext:safe_any()},
					fun({T1, T2, T3}, {L1, L2, L3, Z}) ->
						{L1 ++ [T1], L2 ++ [T2], L3 ++ [T3], Z ++ [Fn(T1, T2, T3)]}
					end,
					{[], [], [], []}
				),
				{Fn, L1, L2, L3, Z, Extra}
			)
		),
		begin
			Tag = make_ref(),

			Res1 = ExpList =:= lists:zipwith3(ZipFn, InList1, InList2, InList3, fail) andalso
			       ExpList =:= lists:zipwith3(ZipFn, InList1, InList2, InList3, trim) andalso
			       ExpList =:= lists:zipwith3(ZipFn, InList1, InList2, InList3, {pad, {Tag, Tag, Tag}}),

			Res2 = try lists:zipwith3(ZipFn, InList1, InList2, InList3 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zipwith3(ZipFn, InList1, InList2 ++ ExtraList, InList3, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zipwith3(ZipFn, InList1, InList2 ++ ExtraList, InList3 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2, InList3, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2, InList3 ++ ExtraList, fail) of _ -> false catch error:_ -> true end andalso
			       try lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2 ++ ExtraList, InList3, fail) of _ -> false catch error:_ -> true end,

			Res3 = ExpList =:= lists:zipwith3(ZipFn, InList1, InList2, InList3 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zipwith3(ZipFn, InList1, InList2 ++ ExtraList, InList3, trim) andalso
			       ExpList =:= lists:zipwith3(ZipFn, InList1, InList2 ++ ExtraList, InList3 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2, InList3, trim) andalso
			       ExpList =:= lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2, InList3 ++ ExtraList, trim) andalso
			       ExpList =:= lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2 ++ ExtraList, InList3, trim),

			Padded1 = lists:zipwith3(ZipFn, InList1, InList2, InList3 ++ ExtraList, {pad, {Tag, Tag, Tag}}),
			Padded2 = lists:zipwith3(ZipFn, InList1, InList2 ++ ExtraList, InList3, {pad, {Tag, Tag, Tag}}),
			Padded3 = lists:zipwith3(ZipFn, InList1, InList2 ++ ExtraList, InList3 ++ ExtraList, {pad, {Tag, Tag, Tag}}),
			Padded4 = lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2, InList3, {pad, {Tag, Tag, Tag}}),
			Padded5 = lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2, InList3 ++ ExtraList, {pad, {Tag, Tag, Tag}}),
			Padded6 = lists:zipwith3(ZipFn, InList1 ++ ExtraList, InList2 ++ ExtraList, InList3, {pad, {Tag, Tag, Tag}}),
			Res4 = Padded1 =:= ExpList ++ [ZipFn(Tag, Tag, X) || X <- ExtraList] andalso
			       Padded2 =:= ExpList ++ [ZipFn(Tag, X, Tag) || X <- ExtraList] andalso
			       Padded3 =:= ExpList ++ [ZipFn(Tag, X, X) || X <- ExtraList] andalso
			       Padded4 =:= ExpList ++ [ZipFn(X, Tag, Tag) || X <- ExtraList] andalso
			       Padded5 =:= ExpList ++ [ZipFn(X, Tag, X) || X <- ExtraList] andalso
			       Padded6 =:= ExpList ++ [ZipFn(X, X, Tag) || X <- ExtraList],

			Res1 andalso Res2 andalso Res3 andalso Res4
		end
	).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

non_list() ->
    ?SUCHTHAT(NonList, ct_proper_ext:safe_any(), not is_list(NonList)).

%% Generator for lists of the given type, folding the given function
%% over values on the top level as they are generated. The first generated
%% value serves as the initial accumulator.
gen_list_fold(Gen, FoldFn) ->
    ?SIZED(
        Size,
        ?LET(
            T,
            Gen,
            if
                Size =< 1 ->
                    {[], T};
                true ->
                    gen_list_fold(max(0, Size - 1), Gen, [T], FoldFn, T)
            end
        )
    ).

%% Generator for lists of the given type, folding the given function
%% over values on the top level as they are generated.
gen_list_fold(Gen, FoldFn, Acc0) ->
    ?SIZED(
        Size,
        gen_list_fold(max(0, Size - 1), Gen, [], FoldFn, Acc0)
    ).

gen_list_fold(0, _Gen, L, _FoldFn, Acc) ->
    {L, Acc};
gen_list_fold(N, Gen, L, FoldFn, Acc) ->
    ?LET(
        E,
        Gen,
        gen_list_fold(N - 1, Gen, L ++ [E], FoldFn, FoldFn(E, Acc))
    ).

%% Generator for key tuples of the given size,
%% with the given key in the given (ie, last) position.
gen_keytuple(Key, Size) ->
    gen_keytuple(Key, Size, Size).

%% Generator for key tuples of the given minimum and maximum
%% sizes, with the given key in the given minimum position.
gen_keytuple(Key, MinSize, MaxSize) ->
    ?LET(
        Tuple,
        gen_tuple(MinSize, MaxSize),
        setelement(MinSize, Tuple, Key)
    ).

%% Generator for tuples of random size.
gen_tuple() ->
    ct_proper_ext:safe_tuple().

%% Generator for tuples of the given size.
gen_tuple(Size) ->
    ?LET(
        V,
        vector(Size, ct_proper_ext:safe_any()),
        list_to_tuple(V)
    ).

%% Generator for tuples of the given minimum and
%% maximum sizes.
gen_tuple(MinSize, MaxSize) ->
    ?LET(
        N,
        range(MinSize, MaxSize),
        ?LET(
            V,
            vector(N, ct_proper_ext:safe_any()),
            list_to_tuple(V)
        )
    ).

%% Generator for lists of anything, folding the given function
%% over values on all levels of list-nesting as they are generated.
gen_list_deepfold(FoldFn, Acc0) ->
    ?SIZED(
        Size,
        ?LET(
            {_, L, Acc},
            gen_list_deepfold(max(0, Size - 1), 0, [], FoldFn, Acc0),
            {L, Acc}
        )
    ).

gen_list_deepfold(N, _Level, L, _FoldFn, Acc) when N =< 0 ->
    {N, lists:reverse(L), Acc};
gen_list_deepfold(N, Level, L, FoldFn, Acc) ->
    ?LET(
        X,
        frequency([
            {5, {term, oneof([ct_proper_ext:safe_atom(),
                              ct_proper_ext:safe_tuple(),
                              integer(),
                              float(),
                              bitstring()])}},
            {1, deeplist},
            {2, stop}
        ]),
        case X of
            deeplist ->
                ?LET(
                    {N1, L1, Acc1},
                    gen_list_deepfold(N, Level + 1, [], FoldFn, Acc),
                    gen_list_deepfold(N1, Level, [L1|L], FoldFn, Acc1)
                );
            stop ->
                {N, lists:reverse(L), Acc};
            {term, E} ->
                gen_list_deepfold(N - 1, Level, [E|L], FoldFn, FoldFn(Level, E, Acc))
        end
    ).

%% Generator for ordering functions, to be used for sorting and merging.
%% The generated ordering functions are designed to fulfill the requirements given
%% at the top of the `lists' documentation, namely to be antisymmetric, transitive,
%% and total. Further, the chances that two terms compare equal, less or greater
%% are equal.
gen_ordering_fun() ->
    ?LET(
        F,
        function1(range(1, 3)),
        fun(T1, T2) ->
            F(T1) =< F(T2)
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% --------------------------------------------------------------------
expect_error(Fn, Args) when is_function(Fn, length(Args))->
    try
        erlang:apply(Fn, Args)
    of
        _ -> false
    catch
        error:_ -> true;
        _:_ -> false
    end.

%% --------------------------------------------------------------------
check_appended([], []) ->
    true;
check_appended([[]|Ls], AL) ->
    check_appended(Ls, AL);
check_appended([L], AL) ->
    L =:= AL;
check_appended([[E1|L]|Ls], [E2|AL]) ->
    E1 =:= E2 andalso
    check_appended([L|Ls], AL);
check_appended(_Ls, _AL) ->
    false.

%% --------------------------------------------------------------------
check_deleted(E, [E|L], DL) ->
    L =:= DL;
check_deleted(E, [_|L], [_|DL]) ->
    check_deleted(E, L, DL);
check_deleted(_E, [], []) ->
    true;
check_deleted(_E, _L, _DL) ->
    false.

%% --------------------------------------------------------------------
check_joined(Sep, [E|L], [E, Sep|JL]) ->
    check_joined(Sep, L, JL);
check_joined(_Sep, [E], [E]) ->
    true;
check_joined(_Sep, [], []) ->
    true;
check_joined(_Sep, _L, _JL) ->
    false.

%% --------------------------------------------------------------------
check_keydeleted(K, N, [E|L], KDL) when element(N, E) == K ->
    L =:= KDL;
check_keydeleted(K, N, [_|L], [_|KDL]) ->
    check_keydeleted(K, N, L, KDL);
check_keydeleted(_K, _N, _L, _KDL) ->
    false.

%% --------------------------------------------------------------------
check_keyreplaced(K, N, R, [E1|L], [E2|KRL]) when element(N, E1) == K ->
    E2 =:= R andalso L =:= KRL;
check_keyreplaced(K, N, R, [_|L], [_|KRL]) ->
    check_keyreplaced(K, N, R, L, KRL);
check_keyreplaced(_K, _N, _R, _L, _KRL) ->
    false.

%% --------------------------------------------------------------------
check_merged(Ls, ML) ->
    check_merged(fun erlang:'=<'/2, Ls, ML).

check_merged(Fn, [[]|Ls], ML) ->
    check_merged(Fn, Ls, ML);
check_merged(_Fn, [], ML) ->
    ML =:= [];
check_merged(_Fn, [L], ML) ->
    ML =:= L;
check_merged(Fn, Ls, [E|ML]) ->
    case find_in_heads(Fn, E, Ls) of
        {true, Ls1} ->
            check_merged(Fn, Ls1, ML);
        false ->
            false
    end;
check_merged(_Fn, _Ls, _ML) ->
    false.

find_in_heads(Fn, E, Ls) ->
    find_in_heads(Fn, E, Ls, []).

find_in_heads(Fn, E, [[]|Ls], Seen) ->
    find_in_heads(Fn, E, Ls, Seen);
find_in_heads(Fn, E, [[E1|LRest]=L|Ls], Seen) ->
    case Fn(E, E1) andalso Fn(E1, E) of
        true ->
            {true, lists:reverse(Seen, [LRest|Ls])};
        false ->
            find_in_heads(Fn, E, Ls, [L|Seen])
    end;
find_in_heads(_Fn, _E, _Ls, _Seen) ->
    false.

%% --------------------------------------------------------------------
check_partitioned(Pred, [E|L], P1, P2) ->
    case {Pred(E), P1, P2} of
        {true, [E|Rest], _} ->
            check_partitioned(Pred, L, Rest, P2);
        {false, _, [E|Rest]}  ->
            check_partitioned(Pred, L, P1, Rest);
        _ ->
            false
    end;
check_partitioned(_Pred, [], [], []) ->
    true;
check_partitioned(_Pred, _L, _P1, _P2) ->
    false.

%% --------------------------------------------------------------------
check_reversed(L1, L2) ->
    check_reversed(L1, L2, []).

check_reversed(L1, L2, Tail) ->
    check_reversed1(L1, L2) =:= Tail.

check_reversed1([], L2) ->
    L2;
check_reversed1([E|L1], L2) ->
    case check_reversed1(L1, L2) of
        [E|L2Rest] -> L2Rest;
        _ -> false
    end.

%% --------------------------------------------------------------------
check_seq([F|Seq], F, T, S) ->
    check_seq(Seq, F + S, T, S);
check_seq([], F, T, S) when S >= 0 ->
    F >= T;
check_seq([], F, T, S) when S < 0 ->
    F =< T;
check_seq(_Seq, _F, _T, _S) ->
    false.

%% --------------------------------------------------------------------
check_sorted(L, Sorted) ->
    check_sorted(fun erlang:'=<'/2, L, Sorted).

check_sorted(SortFun, L, Sorted) ->
    ExpElems = count_elems(L),
    check_sorted(SortFun, Sorted, ExpElems, #{}).

check_sorted(_SortFun, [], ExpElems, FoundElems) ->
    ExpElems =:= FoundElems;
check_sorted(SortFun, [E], ExpElems, FoundElems) ->
    maps:is_key(E, ExpElems) andalso
    check_sorted(SortFun, [], ExpElems, maps:update_with(E, fun(Cnt) -> Cnt + 1 end, 1, FoundElems));
check_sorted(SortFun, [E1|[E2|_]=L], ExpElems, FoundElems) ->
    SortFun(E1, E2) andalso
    maps:is_key(E1, ExpElems) andalso
    check_sorted(SortFun, L, ExpElems, maps:update_with(E1, fun(Cnt) -> Cnt + 1 end, 1, FoundElems));
check_sorted(_SortFun, _L, _ExpElems, _FoundElems) ->
    false.

count_elems(L) ->
    count_elems(L, #{}).

count_elems([E|Es], Acc) ->
    count_elems(Es, maps:update_with(E, fun(Cnt) -> Cnt + 1 end, 1, Acc));
count_elems([], Acc) ->
    Acc.

%% --------------------------------------------------------------------
check_splitwithed(Pred, [E|L], [E|P1], P2) ->
    Pred(E) andalso
    check_splitwithed(Pred, L, P1, P2);
check_splitwithed(Pred, [E|_]=L, [], P2) ->
    not Pred(E) andalso L =:= P2;
check_splitwithed(_Pred, [], [], []) ->
    true;
check_splitwithed(_Pred, _L, _P1, _P2) ->
    false.

%% --------------------------------------------------------------------
check_umerged(Ls, ML) ->
    check_umerged(fun erlang:'=<'/2, Ls, ML).

check_umerged(Fn, [[]|Ls], ML) ->
    check_umerged(Fn, Ls, ML);
check_umerged(_Fn, [L], ML) ->
    ML =:= L;
check_umerged(_Fn, [], ML) ->
    ML =:= [];
check_umerged(Fn, Ls, [E|ML]) ->
    case find_and_remove_from_heads(Fn, E, Ls) of
        {true, Ls1} ->
            check_umerged(Fn, Ls1, ML);
        false ->
            false
    end;
check_umerged(_Fn, _Ls, _ML) ->
    false.

find_and_remove_from_heads(Fn, E, Ls) ->
    find_and_remove_from_heads(false, Fn, E, Ls, []).

find_and_remove_from_heads(Found, Fn, E, [[]|Ls], Seen) ->
    find_and_remove_from_heads(Found, Fn, E, Ls, Seen);
find_and_remove_from_heads(false, _Fn, _E, [], _Seen) ->
    false;
find_and_remove_from_heads(true, _Fn, _E, [], Seen) ->
    {true, lists:reverse(Seen)};
find_and_remove_from_heads(Found, Fn, E, [[E1|LRest]=L|Ls], Seen) ->
    case Fn(E, E1) andalso Fn(E1, E) of
        true ->
            find_and_remove_from_heads(true, Fn, E, Ls, [LRest|Seen]);
        false ->
            find_and_remove_from_heads(Found, Fn, E, Ls, [L|Seen])
    end.

%% --------------------------------------------------------------------
check_uniqed(L, UL) ->
    check_uniqed(fun(X) -> X end, L, UL).

check_uniqed(Fn, L, UL) ->
    check_uniqed1(Fn, L, UL, sets:new([{version, 2}])).

check_uniqed1(Fn, [E|L], [], Seen) ->
    sets:is_element(Fn(E), Seen) andalso
    check_uniqed1(Fn, L, [], Seen);
check_uniqed1(Fn, [E1|L], [E2|URest]=U, Seen) ->
    X1 = Fn(E1),
    X2 = Fn(E2),
    case sets:is_element(X1, Seen) of
        true ->
            X1 =/= X2 andalso
            check_uniqed1(Fn, L, U, Seen);
        false ->
            X1 =:= X2 andalso
            check_uniqed1(Fn, L, URest, sets:add_element(X1, Seen))
    end;
check_uniqed1(_Fn, [], [], _Seen) ->
    true;
check_uniqed1(_Fn, _L, _UL, _Seen) ->
    false.

%% --------------------------------------------------------------------
check_usorted(L, Sorted) ->
    check_usorted(fun erlang:'=<'/2, L, Sorted).

check_usorted(SortFun, L, Sorted) ->
    ExpElems = ucount_elems(SortFun, L),
    check_sorted(SortFun, Sorted, ExpElems, #{}).

ucount_elems(SortFun, L) ->
    ucount_elems(SortFun, L, #{}).

ucount_elems(SortFun, [E|Es], Acc) ->
    K = ufind_key(SortFun, E, maps:keys(Acc)),
    ucount_elems(SortFun, Es, maps:put(K, 1, Acc));
ucount_elems(_SortFun, [], Acc) ->
    Acc.

ufind_key(SortFun, E, [K|Keys]) ->
    case SortFun(E, K) andalso SortFun(K, E) of
        true ->
            K;
        false ->
            ufind_key(SortFun, E, Keys)
    end;
ufind_key(_SortFun, E, []) ->
    E.
