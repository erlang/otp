%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2022. All Rights Reserved.
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

-module(erl_types_prop).
-compile([export_all, nowarn_export_all]).

%% This module only supports proper, as we use features of PropEr's
%% module erlang_abstract_code not available in quickcheck.

-proptest([proper]).

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").

-define(REPETITIONS, 5000).

absorption() ->
    numtests(?REPETITIONS, absorption_1()).

absorption_1() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            ?WHENFAIL(
               begin
                   io:format("TypeA = ~p,\n", [TypeA]),
                   io:format("TypeB = ~p,\n", [TypeB]),
                   io:format("~p:absorption_check(TypeA, TypeB).\n",
                             [?MODULE])
               end,
               absorption_check(TypeA, TypeB))).

absorption_check(A, B) ->
    %% a ∨ (a ∧ b) = a
    R1 = sup(A, inf(A, B)),
    %% a ∧ (a ∨ b) = a
    R2 = inf(A, sup(A, B)),
    equal(A, R1) andalso equal(A, R2).

associativity() ->
    numtests(?REPETITIONS, associativity_1()).

associativity_1() ->
    ?FORALL({TypeA, TypeB, TypeC},
            ?LET(TypeA, type_without_no_return(),
                 ?LET(TypeB, type_without_no_return(),
                      ?LET(TypeC, type_without_no_return(), {TypeA, TypeB, TypeC}))),
            ?WHENFAIL(
               begin
                   io:format("TypeA = ~p,\n", [TypeA]),
                   io:format("TypeB = ~p,\n", [TypeB]),
                   io:format("TypeC = ~p,\n", [TypeC]),
                   io:format("~p:associativity_check(TypeA, TypeB, TypeC).\n",
                             [?MODULE])
               end,
               associativity_check(TypeA, TypeB, TypeC))).

associativity_check(A, B, C) ->
    %% a ∨ (b ∨ c) = (a ∨ b) ∨ c
    LHS_Sup = sup(A, sup(B, C)),
    RHS_Sup = sup(sup(A, B), C),
    Test1 = equal(LHS_Sup, RHS_Sup),
    %% a ∧ (b ∧ c) = (a ∧ b) ∧ c
    LHS_Inf = inf(A, inf(B, C)),
    RHS_Inf = inf(inf(A, B), C),
    Test2 = equal(LHS_Inf, RHS_Inf),
    Test1 and Test2.

commutativity() ->
    numtests(?REPETITIONS, commutativity_1()).

commutativity_1() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            ?WHENFAIL(
               begin
                   io:format("TypeA = ~p,\n", [TypeA]),
                   io:format("TypeB = ~p,\n", [TypeB]),
                   io:format("~p:commutativity_check(TypeA, TypeB).\n",
                             [?MODULE])
               end,
               commutativity_check(TypeA, TypeB))).

commutativity_check(A, B) ->
    %% a ∨ b = b ∨ a
    Sup_AB = sup(A, B),
    Sup_BA = sup(B, A),
    true = sup(A, B) =:= sup(B, A),
    Test1 = equal(Sup_AB, Sup_BA),
    %% a ∧ b = b ∧ a
    Inf_AB = inf(A, B),
    Inf_BA = inf(B, A),
    Test2 = equal(Inf_AB, Inf_BA),
    Test1 and Test2.

idempotence() ->
    numtests(?REPETITIONS, idempotence_1()).

idempotence_1() ->
    ?FORALL(Type,
            type(),
            ?WHENFAIL(
               begin
                   io:format("Type = ~p,\n", [Type]),
                   io:format("~p:idempotence_check(Type).\n", [?MODULE])
               end,
               idempotence_check(Type))).

idempotence_check(Type) ->
    %% a ∨ a = a
    Sup = sup(Type, Type),
    Test1 = equal(Sup, Type),
    %% a ∧ a = a
    Inf = inf(Type, Type),
    Test2 = equal(Inf, Type),
    Test1 and Test2.

identity() ->
    numtests(?REPETITIONS, identity_1()).

identity_1() ->
    ?FORALL(Type, type(), identity_check(Type)).

identity_check(Type) ->
    %% a ∨ [bottom element] = a
    Sup = sup(Type, none),
    Test1 = equal(Sup, Type),
    %% a ∧ [top element] = a
    Inf = inf(Type, any),
    Test2 = equal(Inf, Type),
    Test1 and Test2.

limit() ->
    numtests(?REPETITIONS, limit_1()).

limit_1() ->
    ?FORALL(Type,
            type(),
            ?WHENFAIL(
               begin
                   io:format("Type = ~p,\n", [Type]),
                   io:format("~p:limit_check(Type).\n", [?MODULE])
               end,
               limit_check(Type))).

limit_check(Type1) ->
    equal(erl_types:t_limit(Type1, 1000), Type1).

inf(A, B) ->
    erl_types:t_inf(A, B).

sup(A, B) ->
    erl_types:t_sup(A, B).

equal(A, B) ->
    erl_types:t_is_equal(A, B).

type_without_no_return() ->
    ?SUCHTHAT(Type, type(), not has_no_return(Type)).

has_no_return(unit) ->
    true;
has_no_return(Tuple) when is_tuple(Tuple) ->
    has_no_return(tuple_to_list(Tuple));
has_no_return([T|Ts]) ->
    has_no_return(T) orelse has_no_return(Ts);
has_no_return(_) ->
    false.

type() ->
    ?LET(Forms,
         abstr(),
         begin
             [F | _] = [T || {attribute, _, K, {_, T, []}} <- Forms,
                             K =:= type orelse K =:= opaque],
             form_to_type(F)
         end).

abstr() ->
    Opts = [{types, [{Type, 0} || Type <- types()]},
            {weight, {atom, 10}},
            {weight, {type_decl, 1}},
            {weight, {record_decl, 0}},
            {weight, {function_spec, 0}},
            {weight, {function_decl, 0}},
            {weight, {type_variable, 0}}],
    proper_erlang_abstract_code:module(Opts).

form_to_type(Form) ->
    Types = [SiteType | _] = types(),
    Anno = erl_anno:new(0),
    TableTypes = [{{type, Type, 0},
                   {{m, {"file", Anno}, {type, 0, any, []}, []}, any}} ||
                     Type <- Types],
    TypeTable = maps:from_list(TableTypes),
    CodeTable = ets:new(table, [set]),
    try
        true = ets:insert(CodeTable, {m, TypeTable}),
        Site = {type, {m, SiteType, 0}, ""},
        C = erl_types:cache__new(),
        ETypes = sets:from_list([]),
        {T, _} = erl_types:t_from_form(Form, ETypes, Site, CodeTable, #{}, C),
        T
    after
        true = ets:delete(CodeTable)
    end.

types() ->
    [t, tt].

-endif.
