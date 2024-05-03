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
-module(sets_prop).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% --- add_element/2 --------------------------------------------------
prop_add_element() ->
    test_all(fun subprop_add_element/1).

subprop_add_element(Mod) ->
    ?FORALL(
        {{S0, M0}, Es},
        ?LET(
            {L1, L2, B},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {gen_set(Mod, L1 ++ B), L2 ++ B}
        ),
        begin
            {S1, M1} = lists:foldl(fun(E, {SAcc, MAcc}) ->
                                       {Mod:add_element(E, SAcc),
                                        model_add_element(E, MAcc)}
                                   end,
                                   {S0, M0},
                                   Es),
            is_equal(S1, M1)
        end
    ).


%% --- del_element/2 --------------------------------------------------
prop_del_element() ->
    test_all(fun subprop_del_element/1).

subprop_del_element(Mod) ->
    ?FORALL(
        {{S0, M0}, Es},
        ?LET(
            {L1, L2, B},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {gen_set(Mod, L1 ++ B), L2 ++ B}
        ),
        begin
            {S1, M1} = lists:foldl(fun(E, {SAcc, MAcc}) ->
                                       {Mod:del_element(E, SAcc),
                                        model_del_element(E, MAcc)}
                                   end,
                                   {S0, M0},
                                   Es),
            is_equal(S1, M1)
        end
    ).


%% --- filter/2 -------------------------------------------------------
prop_filter() ->
    test_all(fun subprop_filter/1).

subprop_filter(Mod) ->
    ?FORALL(
        {{S0, M0}, Fun},
        {gen_set(Mod), function1(boolean())},
        is_equal(Mod:filter(Fun, S0),
                 model_filter(Fun, M0))
    ).


%% --- filtermap/2 ----------------------------------------------------
prop_filtermap() ->
    test_all(fun subprop_filtermap/1).

subprop_filtermap(Mod) ->
    ?FORALL(
        {{S0, M0}, Fun},
	{gen_set(Mod),
         function1(oneof([true, false, {true, ct_proper_ext:safe_any()}]))},
	is_equal(Mod:filtermap(Fun, S0),
                 model_filtermap(Fun, M0))
    ).


%% --- fold/3 ---------------------------------------------------------
prop_fold() ->
    test_all(fun subprop_fold/1).

subprop_fold(Mod) ->
    ?FORALL(
        {S, M},
        gen_set(Mod),
        begin
            Fun = fun(E, Acc) -> Acc + erlang:phash2(E) end,
            Mod:fold(Fun, 0, S) =:= model_fold(Fun, 0, M)
        end
    ).


%% --- from_list/1,2 --------------------------------------------------
prop_from_list() ->
    test_all(fun subprop_from_list/1).

subprop_from_list(sets) ->
    ?FORALL(
        {L, V},
        {ct_proper_ext:safe_list(), gen_version()},
        is_equal(sets:from_list(L, [{version, V}]),
                 model_from_list(sets, L))
    );
subprop_from_list(Mod) ->
    ?FORALL(
        L,
        ct_proper_ext:safe_list(),
        is_equal(Mod:from_list(L),
                 model_from_list(Mod, L))
    ).


%% --- intersection/1 -------------------------------------------------
prop_intersection_1() ->
    test_all(fun subprop_intersection_1/1).

subprop_intersection_1(Mod) ->
    ?FORALL(
        SMs,
        ?LET(
            {Ls, A},
            {non_empty(list(ct_proper_ext:safe_list())),
             ct_proper_ext:safe_list()},
            [gen_set(Mod, L ++ A) || L <- Ls]
        ),
        begin
            {Ss, Ms} = lists:unzip(SMs),
            is_equal(Mod:intersection(Ss),
                     model_intersection(Ms))
        end
    ).


%% --- intersection/2 -------------------------------------------------
prop_intersection_2() ->
    test_all(fun subprop_intersection_2/1).

subprop_intersection_2(Mod) ->
    ?FORALL(
        {{S1, M1}, {S2, M2}},
        ?LET(
            {L1, L2, B},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {gen_set(Mod, L1 ++ B), gen_set(Mod, L2 ++ B)}
        ),
        is_equal(Mod:intersection(S1, S2),
                 model_intersection(M1, M2))
    ).


%% --- is_disjoint/2 --------------------------------------------------
prop_is_disjoint() ->
    test_all(fun subprop_is_disjoint/1).

subprop_is_disjoint(Mod) ->
    ?FORALL(
        {{S1, M1}, {S2, M2}},
        ?LET(
            {L1, L2, B},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            begin
                {gen_set(Mod, L1 ++ B), gen_set(Mod, L2 ++ B)}
            end
        ),
        Mod:is_disjoint(S1, S2) =:= model_is_disjoint(M1, M2)
    ).


%% --- is_element/2 ---------------------------------------------------
prop_is_element() ->
    test_all(fun subprop_is_element/1).

subprop_is_element(Mod) ->
    ?FORALL(
        {{S, M}, Es},
        ?LET(
            {L, Extra},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            {gen_set(Mod, L), L ++ Extra}
        ),
        lists:all(fun(E) ->
                      Mod:is_element(E, S) =:= model_is_element(E, M)
                  end,
                  Es)
    ).


%% --- is_empty/1 -----------------------------------------------------
prop_is_empty() ->
    test_all(fun subprop_is_empty/1).

subprop_is_empty(Mod) ->
    ?FORALL(
        {S, M},
        gen_set(Mod),
        Mod:is_empty(S) =:= model_is_empty(M)
    ).


%% --- is_equal/1 -----------------------------------------------------
prop_is_equal() ->
    test_all(fun subprop_is_equal/1).

subprop_is_equal(Mod) ->
    ?FORALL(
        {{S1, M1}, {S2, M2}},
	{gen_set(Mod), gen_set(Mod)},
	Mod:is_equal(S1, S2)=:=is_equal(S1, M2) andalso
	Mod:is_equal(S2, S1)=:=is_equal(S2, M1)
    ).


%% --- is_set/1 -------------------------------------------------------
prop_is_set() ->
    test_all(fun subprop_is_set/1).

subprop_is_set(sets) ->
    ?FORALL(
        {Exp, {S, _M}},
        oneof([{true, gen_set(sets)},
               {false, {?SUCHTHAT(T,
                                  ct_proper_ext:safe_any(),
                                  not (is_map(T) orelse
                                       is_tuple(T) andalso
                                       tuple_size(T)=:=9 andalso
                                       element(1, T)=:=set)),
                        undefined}}]),
        Exp =:= sets:is_set(S)
    );
subprop_is_set(ordsets) ->
    ?FORALL(
        {Exp, {S, _M}},
        oneof([{true, gen_set(ordsets)},
               {false, {?SUCHTHAT(T,
                                  ct_proper_ext:safe_any(),
                                  not is_list(T)),
                        undefined}}]),
        Exp =:= ordsets:is_set(S)
    );
subprop_is_set(gb_sets) ->
    ?FORALL(
        {Exp, {S, _M}},
        oneof([{true, gen_set(gb_sets)},
               {false, {?SUCHTHAT(T,
                                  ct_proper_ext:safe_any(),
                                  not (is_tuple(T) andalso
                                       tuple_size(T) =:= 2 andalso
                                       is_integer(element(1, T)) andalso
                                       element(1, T) >= 0 andalso
                                       (element(2, T) =:= nil orelse
                                        is_tuple(element(2, T)) andalso
                                        tuple_size(element(2, T)) =:= 3))),
                        undefined}}]),
        Exp =:= gb_sets:is_set(S)
    ).


%% --- subset/2 -------------------------------------------------------
%%
%% +-----------------------------+
%% | S0 +----------------------+ |
%% |    | S1   +-------------+ | |
%% |    |      | S2          | | |
%% |    | +----+-----------+ | | |
%% |    | | S3 | +-------+ | | | |
%% |    | |    | | Empty | | | | |
%% |    | |    | +-------+ | | | |
%% |    | |    +-----------+-+ | |
%% |    | +----------------+   | |
%% |    +----------------------+ |
%% +-----------------------------+
%% * Empty is a subset of S2 and S3
%%
%% * S2 is a subset of S1 but not of S3
%% * S3 is a subset of S1 but not of S2
%% --> Empty is a subset of S1
%%
%% * S1 is a subset of S0
%% --> S2, S3 and Empty are subsets of S0
prop_is_subset() ->
    test_all(fun subprop_is_subset/1).

subprop_is_subset(Mod) ->
    ?FORALL(
        SMs,
        ?LET(
            {L1, L2},
            {ct_proper_ext:safe_list(), ct_proper_ext:safe_list()},
            begin
                L3Extra = [make_ref()|L2],
                L2Extra = [make_ref()|L1],
                L1Extra = [make_ref()|L2Extra ++ L3Extra],
                L0Extra = [make_ref()|L1Extra],
                [gen_set(Mod, L0Extra),
                 gen_set(Mod, L1Extra),
                 gen_set(Mod, L2Extra),
                 gen_set(Mod, L3Extra),
                 gen_set(Mod, [])]
            end
        ),
        lists:all(fun({{S1, M1}, {S2, M2}}) ->
                      Mod:is_subset(S1, S2) =:= model_is_subset(M1, M2)
                  end,
                  [{SM1, SM2} || SM1 <- SMs, SM2 <- SMs])
    ).


%% --- map/2 ----------------------------------------------------------
prop_map() ->
    test_all(fun subprop_map/1).

subprop_map(Mod) ->
    ?FORALL(
        {{S0, M0}, Fun},
        {gen_set(Mod), function1(ct_proper_ext:safe_any())},
        is_equal(Mod:map(Fun, S0),
                 model_map(Fun, M0))
    ).


%% --- size/1 ---------------------------------------------------------
prop_size() ->
    test_all(fun subprop_size/1).

subprop_size(Mod) ->
    ?FORALL(
        {S, M},
        gen_set(Mod),
        Mod:size(S) =:= model_size(M)
    ).


%% --- subtract/2 -----------------------------------------------------
prop_subtract() ->
    test_all(fun subprop_subtract/1).

subprop_subtract(Mod) ->
    ?FORALL(
        {{S1, M1}, {S2, M2}},
        ?LET(
            {L1, L2, B},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {gen_set(Mod, L1 ++ B), gen_set(Mod, L2 ++ B)}
        ),
        is_equal(Mod:subtract(S1, S2),
                 model_subtract(M1, M2)) andalso
        is_equal(Mod:subtract(S2, S1),
                 model_subtract(M2, M1))
    ).


%% --- to_list/1 ------------------------------------------------------
prop_to_list() ->
    test_all(fun subprop_to_list/1).

subprop_to_list(Mod) ->
    ?FORALL(
        {S, M},
        gen_set(Mod),
        list_matchsort(Mod:to_list(S)) =:= list_matchsort(model_to_list(M))
    ).


%% --- union/1 --------------------------------------------------------
prop_union_1() ->
    test_all(fun subprop_union_1/1).

subprop_union_1(Mod) ->
    ?FORALL(
        SMs,
        ?LET(
            {Ls, A},
            {list(ct_proper_ext:safe_list()), ct_proper_ext:safe_list()},
            [gen_set(Mod, L ++ A) || L <- Ls]
        ),
        begin
            {Ss, Ms} = lists:unzip(SMs),
            is_equal(Mod:union(Ss),
                     model_union(Mod, Ms))
        end
    ).


%% --- union/2 --------------------------------------------------------
prop_union_2() ->
    test_all(fun subprop_union_2/1).

subprop_union_2(Mod) ->
    ?FORALL(
        {{S1, M1}, {S2, M2}},
        ?LET(
            {L1, L2, B},
            {ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list(),
             ct_proper_ext:safe_list()},
            {gen_set(Mod, L1 ++ B), gen_set(Mod, L2 ++ B)}
        ),
        is_equal(Mod:union(S1, S2),
                 model_union(Mod, M1, M2))
    ).

%% --- sequence of modifying operations -------------------------------
prop_operations() ->
    test_all(fun subprop_operations/1).

subprop_operations(Mod) ->
    ?FORALL(
        {SM0, Ops},
        {gen_set(Mod),
         list(oneof([{add_element, ct_proper_ext:safe_any()},
                     {del_element, ct_proper_ext:safe_any()},
                     {filter, function1(boolean())},
                     {filtermap, function1(oneof([true,
                                                  false,
                                                  {true, ct_proper_ext:safe_any()}]))},
                     {intersection, gen_set(Mod)},
                     {map, function1(ct_proper_ext:safe_any())},
                     {subtract, gen_set(Mod)},
                     {union, gen_set(Mod)}]))},
        begin
            {S1, M1} = lists:foldl(fun
                                       ({add_element, E}, {SAcc, MAcc}) ->
                                           {Mod:add_element(E, SAcc),
                                            model_add_element(E, MAcc)};
                                       ({del_element, E}, {SAcc, MAcc}) ->
                                           {Mod:del_element(E, SAcc),
                                            model_del_element(E, MAcc)};
                                       ({filter, Fun}, {SAcc, MAcc}) ->
                                           {Mod:filter(Fun, SAcc),
                                            model_filter(Fun, MAcc)};
                                       ({filtermap, Fun}, {SAcc, MAcc}) ->
                                           {Mod:filtermap(Fun, SAcc),
                                            model_filtermap(Fun, MAcc)};
                                       ({intersection, {S, M}}, {SAcc, MAcc}) ->
                                           {Mod:intersection(SAcc, S),
                                            model_intersection(MAcc, M)};
                                       ({map, Fun}, {SAcc, MAcc}) ->
                                           {Mod:map(Fun, SAcc),
                                            model_map(Fun, MAcc)};
                                       ({subtract, {S, M}}, {SAcc, MAcc}) ->
                                           {Mod:subtract(SAcc, S),
                                            model_subtract(MAcc, M)};
                                       ({union, {S, M}}, {SAcc, MAcc}) ->
                                           {Mod:union(SAcc, S),
                                            model_union(Mod, MAcc, M)}
                                   end,
                                   SM0,
                                   Ops),
            is_equal(S1, M1)
        end
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

gen_version() ->
    oneof([1, 2]).

gen_set(sets) ->
    ?LET(
        {L, V},
        {ct_proper_ext:safe_list(), gen_version()},
        gen_set(sets, L, V)
    );
gen_set(Mod) ->
    ?LET(
        L,
        ct_proper_ext:safe_list(),
        gen_set(Mod, L, 0)
    ).

gen_set(sets, List) when is_list(List) ->
    ?LET(
        V,
        gen_version(),
        gen_set(sets, List, V)
    );
gen_set(Mod, List) when is_list(List) ->
    gen_set(Mod, List, 0);
gen_set(Mod, Version) when is_integer(Version) ->
    ?LET(
        L,
        ct_proper_ext:safe_list(),
        gen_set(Mod, L, Version)
    ).

gen_set(sets, List, Version) ->
    {sets:from_list(List, [{version, Version}]),
     model_from_list(sets, List)};
gen_set(Mod, List, _Version) ->
    {Mod:from_list(List),
     model_from_list(Mod, List)}.


%%%%%%%%%%%%%
%%% Model %%%
%%%%%%%%%%%%%

-record(model, {type, module, content=#{}}).

model_new(sets) ->
    #model{type=match, module=sets};
model_new(ordsets) ->
    #model{type=equal, module=ordsets};
model_new(gb_sets) ->
    #model{type=equal, module=gb_sets}.

model_add_element(E, #model{type=equal, content=C}=M) when is_float(E), trunc(E) == E ->
    M#model{content=C#{trunc(E) => E}};
model_add_element(E, #model{content=C}=M) ->
    M#model{content=C#{E => E}}.

model_del_element(E, #model{type=equal, content=C}=M) when is_float(E), trunc(E) == E ->
    M#model{content=maps:remove(trunc(E), C)};
model_del_element(E, #model{content=C}=M) ->
    M#model{content=maps:remove(E, C)}.

model_from_list(Mod, L) ->
    lists:foldl(fun model_add_element/2, model_new(Mod), L).

model_to_list(#model{content=C}) ->
    maps:values(C).

model_is_element(E, #model{type=equal, content=C}) when is_float(E), trunc(E) == E ->
    maps:is_key(trunc(E), C);
model_is_element(E, #model{content=C}) ->
    maps:is_key(E, C).

model_size(#model{content=C}) ->
    maps:size(C).

model_filter(Fun, #model{content=C}=M) ->
    M#model{content=maps:filter(fun(_K, V) -> Fun(V) end, C)}.

model_map(Fun, #model{module=Mod, content=C}) ->
    maps:fold(fun(_K, V, Acc) -> model_add_element(Fun(V), Acc) end, model_new(Mod), C).

model_filtermap(Fun, #model{module=Mod, content=C}) ->
    maps:fold(fun(_K, V0, Acc) ->
                  case Fun(V0) of
                      true ->
                          model_add_element(V0, Acc);
                      {true, V1} ->
                          model_add_element(V1, Acc);
                      false ->
                          Acc
                  end
              end,
              model_new(Mod),
              C).

model_fold(Fun, Acc0, #model{content=C}) ->
    maps:fold(fun(_K, V, Acc1) -> Fun(V, Acc1) end, Acc0, C).

model_subtract(#model{module=Mod, content=C1}=M1, #model{module=Mod, content=C2}) ->
    M1#model{content=maps:without(maps:keys(C2), C1)}.

model_intersection([M|Ms]) ->
    model_intersection_1(Ms, M).

model_intersection(M1, M2) ->
    model_intersection_1([M1], M2).

model_intersection_1([], Acc) ->
    Acc;
model_intersection_1([#model{module=Mod, content=C1}|Ms], #model{module=Mod, content=C2}=Acc) ->
    model_intersection_1(Ms, Acc#model{content=maps:with(maps:keys(C2), maps:with(maps:keys(C1), C2))}).

model_union(Mod, []) ->
    model_new(Mod);
model_union(_Mod, [M|Ms]) ->
    model_union_1(Ms, M).

model_union(_Mod, M1, M2) ->
    model_union_1([M1], M2).

model_union_1([], Acc) ->
    Acc;
model_union_1([#model{module=Mod, content=C1}|Ms], #model{module=Mod, content=C2}=Acc) ->
    model_union_1(Ms, Acc#model{content=maps:merge(C2, C1)}).

model_is_subset(#model{module=Mod, content=C1}, #model{module=Mod, content=C2}) ->
    [] =:= maps:keys(C1) -- maps:keys(C2).

model_is_empty(M) ->
    0 =:= model_size(M).

model_is_disjoint(M1, M2) ->
    0 =:= model_size(model_intersection(M1, M2)).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

test_all(Fun) ->
    conjunction([{T, Fun(T)} || T <- [sets, ordsets, gb_sets]]).

list_matchsort(L) ->
    lists:sort(fun
                   (A, B) when is_float(A), is_integer(B) ->
                       true;
                   (A, B) when is_integer(A), is_float(B) ->
                       false;
                   (A, B) ->
                       A =< B
               end,
               L).

is_equal(S, #model{type=T, module=Mod, content=C}) ->
    L1 = list_matchsort(Mod:to_list(S)),
    L2 = list_matchsort(maps:keys(C)),
    case T of
        match -> L1 =:= L2;
        equal -> L1 == L2
    end.

