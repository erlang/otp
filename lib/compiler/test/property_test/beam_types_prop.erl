%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-module(beam_types_prop).

-compile([export_all, nowarn_export_all]).

%% This module only supports proper, as we don't have an eqc license to test
%% with.

-proptest([proper]).

-ifdef(PROPER).

-include_lib("compiler/src/beam_types.hrl").

-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-import(lists, [foldl/3]).

%% The default repetitions of 100 is a bit too low to reliably cover all type
%% combinations, so we crank it up a bit.
-define(REPETITIONS, 1000).

absorption() ->
    numtests(?REPETITIONS, absorption_1()).

absorption_1() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            absorption_check(TypeA, TypeB)).

absorption_check(A, B) ->
    %% a ∨ (a ∧ b) = a,
    A = join(A, meet(A, B)),

    %% a ∧ (a ∨ b) = a.
    A = meet(A, join(A, B)),

    true.

associativity() ->
    numtests(?REPETITIONS, associativity_1()).

associativity_1() ->
    ?FORALL({TypeA, TypeB, TypeC},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(),
                      ?LET(TypeC, type(), {TypeA, TypeB, TypeC}))),
            associativity_check(TypeA, TypeB, TypeC)).

associativity_check(A, B, C) ->
    %% a ∨ (b ∨ c) = (a ∨ b) ∨ c,
    LHS_Join = join(A, join(B, C)),
    RHS_Join = join(join(A, B), C),
    LHS_Join = RHS_Join,

    %% a ∧ (b ∧ c) = (a ∧ b) ∧ c.
    LHS_Meet = meet(A, meet(B, C)),
    RHS_Meet = meet(meet(A, B), C),
    LHS_Meet = RHS_Meet,

    true.

commutativity() ->
    numtests(?REPETITIONS, commutativity_1()).

commutativity_1() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            commutativity_check(TypeA, TypeB)).

commutativity_check(A, B) ->
    %% a ∨ b = b ∨ a,
    true = join(A, B) =:= join(B, A),

    %% a ∧ b = b ∧ a.
    true = meet(A, B) =:= meet(B, A),

    true.

idempotence() ->
    numtests(?REPETITIONS, idempotence_1()).

idempotence_1() ->
    ?FORALL(Type, type(), idempotence_check(Type)).

idempotence_check(Type) ->
    %% a ∨ a = a,
    Type = join(Type, Type),

    %% a ∧ a = a.
    Type = meet(Type, Type),

    true.

identity() ->
    ?FORALL(Type, type(), identity_check(Type)).

identity_check(Type) ->
    %% a ∨ [bottom element] = a,
    Type = join(Type, none),

    %% a ∧ [top element] = a.
    Type = meet(Type, any),

    true.

subtraction() ->
    numtests(?REPETITIONS, subtraction_1()).

subtraction_1() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            subtraction_check(TypeA, TypeB)).

subtraction_check(A, B) ->
    %% Subtraction can be thought of as `a ∧ ¬b`, so the result must be at
    %% least as specific as `a`.
    Res = subtract(A, B),
    Res = meet(A, Res),

    true.

meet(A, B) -> beam_types:meet(A, B).
join(A, B) -> beam_types:join(A, B).
subtract(A, B) -> beam_types:subtract(A, B).

%%%
%%% Generators
%%%

type() ->
    type(0).

type(Depth) ->
    oneof(nested_types(Depth) ++
              numerical_types() ++
              other_types()).

other_types() ->
    [any,
     gen_atom(),
     gen_bs_matchable(),
     none].

numerical_types() ->
    [gen_integer(), gen_float(), number].

nested_types(Depth) when Depth >= 3 -> [none];
nested_types(Depth) -> list_types(Depth + 1) ++
                           [#t_map{},
                            gen_fun(Depth + 1),
                            gen_union(Depth + 1),
                            gen_tuple(Depth + 1)].

list_types(Depth) when Depth >= 3 ->
    [nil];
list_types(Depth) ->
    [gen_list(Depth), gen_cons(Depth), nil].

gen_atom() ->
    ?LET(Size, range(0, ?ATOM_SET_SIZE),
         case Size of
             0 ->
                 #t_atom{};
             _ ->
                 ?LET(Set, sized_list(Size, gen_atom_val()),
                      begin
                          #t_atom{elements=ordsets:from_list(Set)}
                      end)
         end).

gen_atom_val() ->
    ?LET(N, range($0, $~), list_to_atom([N])).

gen_bs_matchable() ->
    oneof([?LET(Unit, range(1, 128), #t_bs_matchable{tail_unit=Unit}),
           ?LET(Unit, range(1, 128), #t_bs_context{tail_unit=Unit}),
           ?LET(Unit, range(1, 128), #t_bitstring{size_unit=Unit})]).

gen_fun(Depth) ->
    oneof([?LET(Arity, range(1, 8),
                #t_fun{type=type(Depth),arity=Arity}),
                #t_fun{type=type(Depth),arity=any}]).

gen_integer() ->
    oneof([gen_integer_bounded(), #t_integer{}]).

gen_integer_bounded() ->
    ?LET({A, B}, {integer(), integer()},
         begin
             #t_integer{elements={min(A,B), max(A,B)}}
         end).

gen_cons(Depth) ->
    ?LET({Type, Term}, {gen_element(Depth), gen_element(Depth)},
         #t_cons{type=Type,terminator=Term}).

gen_list(Depth) ->
    ?LET({Type, Term}, {gen_element(Depth), gen_element(Depth)},
         #t_list{type=Type,terminator=Term}).

gen_float() ->
    oneof([gen_float_bounded(), #t_float{}]).

gen_float_bounded() ->
    ?LET({A, B}, {integer(), integer()},
         begin
             Min = float(min(A,B)),
             Max = float(max(A,B)),
             #t_float{elements={Min,Max}}
         end).

gen_tuple(Depth) ->
    ?SIZED(Size,
           ?LET({Exact, Elements}, {boolean(), gen_tuple_elements(Size, Depth)},
                begin
                    #t_tuple{exact=Exact,
                             size=Size,
                             elements=Elements}
                end)).

gen_union(Depth) ->
    ?LAZY(oneof([gen_wide_union(Depth), gen_tuple_union(Depth)])).

gen_wide_union(Depth) ->
    ?LET({A, B, C, D}, {oneof(nested_types(Depth)),
                        oneof(numerical_types()),
                        oneof(list_types(Depth)),
                        oneof(other_types())},
         begin
             T0 = join(A, B),
             T1 = join(T0, C),
             join(T1, D)
         end).

gen_tuple_union(Depth) ->
    ?SIZED(Size,
           ?LET(Tuples, sized_list(Size, gen_tuple(Depth)),
                foldl(fun join/2, none, Tuples))).

gen_tuple_elements(Size, Depth) ->
    ?LET(Types, sized_list(rand:uniform(Size div 4 + 1), gen_element(Depth)),
         foldl(fun(Type, Acc) ->
                       Index = rand:uniform(Size),
                       beam_types:set_tuple_element(Index, Type, Acc)
               end, #{}, Types)).

gen_element(Depth) ->
    ?LAZY(?SUCHTHAT(Type, type(Depth),
                    case Type of
                        any -> false;
                        none -> false;
                        _ -> true
                    end)).

sized_list(0, _Gen) -> [];
sized_list(N, Gen) -> [Gen | sized_list(N - 1, Gen)].

-endif.
