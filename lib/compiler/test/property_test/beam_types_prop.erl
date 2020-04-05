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

-define(BEAM_TYPES_INTERNAL, true).
-include_lib("compiler/src/beam_types.hrl").

-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-import(lists, [duplicate/2,foldl/3]).

%% The default repetitions of 100 is a bit too low to reliably cover all type
%% combinations, so we crank it up a bit.
-define(REPETITIONS, 5000).

absorption() ->
    numtests(?REPETITIONS, absorption_1()).

absorption_1() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            absorption_check(TypeA, TypeB)).

absorption_check(A, B) ->
    verified_type(A),
    verified_type(B),

    %% a ∨ (a ∧ b) = a
    A = join(A, meet(A, B)),

    %% a ∧ (a ∨ b) = a
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
    verified_type(A),
    verified_type(B),
    verified_type(C),

    %% a ∨ (b ∨ c) = (a ∨ b) ∨ c
    LHS_Join = join(A, join(B, C)),
    RHS_Join = join(join(A, B), C),
    LHS_Join = RHS_Join,

    %% a ∧ (b ∧ c) = (a ∧ b) ∧ c
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
    verified_type(A),
    verified_type(B),

    %% a ∨ b = b ∨ a
    true = join(A, B) =:= join(B, A),

    %% a ∧ b = b ∧ a
    true = meet(A, B) =:= meet(B, A),

    true.

idempotence() ->
    numtests(?REPETITIONS, idempotence_1()).

idempotence_1() ->
    ?FORALL(Type, type(), idempotence_check(Type)).

idempotence_check(Type) ->
    verified_type(Type),

    %% a ∨ a = a
    Type = join(Type, Type),

    %% a ∧ a = a
    Type = meet(Type, Type),

    true.

identity() ->
    ?FORALL(Type, type(), identity_check(Type)).

identity_check(Type) ->
    verified_type(Type),

    %% a ∨ [bottom element] = a
    Type = join(Type, none),

    %% a ∧ [top element] = a
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
    verified_type(A),
    verified_type(B),

    %% Subtraction can be thought of as `a ∧ ¬b`, so the result must be at
    %% least as specific as `a`.
    Res = subtract(A, B),
    Res = meet(A, Res),

    true.

meet(A, B) -> beam_types:meet(A, B).
join(A, B) -> beam_types:join(A, B).
subtract(A, B) -> beam_types:subtract(A, B).
verified_type(T) -> beam_types:verified_type(T).

%%%
%%% Generators
%%%

type() ->
    type(?MAX_TYPE_DEPTH).

type(Depth) ->
    ?SHRINK(?LAZY(oneof([any, none] ++ term_types(Depth))),
            [nil, any, none]).

term_type(Depth) ->
    ?SHRINK(?LAZY(oneof([any | term_types(Depth)])),
            [nil, any]).

term_types(Depth) ->
    nested_generators(Depth) ++
        numerical_generators() ++
        [gen_atom(), gen_bs_matchable()].

numerical_generators() ->
    [gen_integer(), gen_float(), number].

nested_generators(Depth) when Depth =< 0 ->
    [nil];
nested_generators(Depth) ->
    [gen_list(Depth - 1),
     gen_fun(Depth - 1),
     gen_map(Depth - 1),
     ?LAZY(gen_tuple(Depth - 1)),
     ?LAZY(gen_union(Depth - 1))].

%% Proper's atom generator is far too wide, generating strings like 'û\2144Bò}'
%% which are both hard to read and fill up the atom table really fast.
readable_atom() ->
    ?LET(Atom, range($0, $~), list_to_atom([Atom])).

%%

gen_atom() ->
    ?LET(Size, range(0, ?ATOM_SET_SIZE),
         ?LET(Set, duplicate(Size, readable_atom()),
              case ordsets:from_list(Set) of
                  [_|_]=Vs -> #t_atom{elements=ordsets:from_list(Vs)};
                  [] -> #t_atom{}
              end)).

gen_bs_matchable() ->
    oneof([?LET(Unit, range(1, 16), #t_bs_matchable{tail_unit=Unit}),
           ?LET(Unit, range(1, 16), #t_bs_context{tail_unit=Unit}),
           ?LET(Unit, range(1, 16), #t_bitstring{size_unit=Unit})]).

gen_float() ->
    oneof([?LET({A, B}, {integer(), integer()},
                begin
                    Min = float(min(A,B)),
                    Max = float(max(A,B)),
                    #t_float{elements={Min,Max}}
                end),
           #t_float{}]).

gen_fun(Depth) ->
    ?SHRINK(?LET({Type, Arity}, {type(Depth), oneof([any, range(1, 4)])},
                 #t_fun{type=Type,arity=Arity}),
            [#t_fun{}]).

gen_integer() ->
    oneof([?LET({A, B}, {integer(), integer()},
                #t_integer{elements={min(A,B), max(A,B)}}),
           #t_integer{}]).

gen_list(Depth) ->
    ?SHRINK(oneof([?LET({Type, Term}, {term_type(Depth), term_type(Depth)},
                        #t_list{type=Type,terminator=Term}),
                   ?LET({Type, Term}, {term_type(Depth), term_type(Depth)},
                        #t_cons{type=Type,terminator=Term}),
                   nil]),
            [nil]).

gen_map(Depth) ->
    ?SHRINK(?LET({SKey, SValue}, {term_type(Depth), term_type(Depth)},
                 #t_map{super_key=SKey,super_value=SValue}),
            [#t_map{}]).

gen_tuple(Depth) ->
    ?SHRINK(oneof([gen_tuple_plain(Depth), gen_tuple_record(Depth)]),
            [#t_tuple{}]).

gen_tuple_record(Depth) ->
    ?LET({Start, Size}, {range(2, ?TUPLE_ELEMENT_LIMIT),
                         range(1, ?TUPLE_ELEMENT_LIMIT * 2)},
         ?LET({Tag, Es0}, {readable_atom(),
                           gen_tuple_elements(Start, Size, Depth)},
              begin
                  Es = Es0#{ 1 => #t_atom{elements=[Tag]} },
                  #t_tuple{exact=true,size=Size,elements=Es}
              end)).

gen_tuple_plain(Depth) ->
    ?LET({Start, Size}, {range(1, ?TUPLE_ELEMENT_LIMIT),
                         range(0, ?TUPLE_ELEMENT_LIMIT * 2)},
         ?LET({Exact, Es}, {boolean(), gen_tuple_elements(Start, Size, Depth)},
              #t_tuple{exact=Exact,size=Size,elements=Es})).

gen_tuple_elements(Start, Size, Depth) ->
    End = min(Size, ?TUPLE_ELEMENT_LIMIT),
    ?SHRINK(?LET(Types, gen_tuple_elements_1(Start, End, term_type(Depth)),
                 foldl(fun({Index, Type}, Acc) ->
                               beam_types:set_tuple_element(Index, Type, Acc)
                       end, #{}, Types)),
            [#{}]).

gen_tuple_elements_1(Index, End, _Gen) when Index > End ->
    [];
gen_tuple_elements_1(Index, End, Gen) ->
    case rand:uniform(2) of
        1 -> [{Index, Gen} | gen_tuple_elements_1(Index + 1, End, Gen)];
        2 -> gen_tuple_elements_1(Index + 1, End, Gen)
    end.

gen_union(Depth) ->
    ?SHRINK(oneof([gen_union_wide(Depth), gen_union_record(Depth)]),
            [gen_union_record(?MAX_TYPE_DEPTH)]).

%% Creates a union with most (if not all) slots filled.
gen_union_wide(Depth) ->
    ?LET({A, B, C, D, E, F}, {gen_atom(),
                              gen_bs_matchable(),
                              gen_list(Depth),
                              gen_tuple(Depth),
                              oneof(nested_generators(Depth)),
                              oneof(numerical_generators())},
         begin
             T0 = join(A, B),
             T1 = join(T0, C),
             T2 = join(T1, D),
             T3 = join(T2, E),
             join(T3, F)
         end).

%% Creates a union consisting solely of records
gen_union_record(Depth) ->
    ?LET(Size, range(2, ?TUPLE_SET_LIMIT),
         ?LET(Tuples, duplicate(Size, gen_tuple_record(Depth)),
              foldl(fun join/2, none, Tuples))).

-endif.
