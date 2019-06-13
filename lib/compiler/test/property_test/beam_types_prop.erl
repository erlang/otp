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

%% This module has only supports proper, as we don't have an eqc license to
%% test with.

-proptest([proper]).

-ifdef(PROPER).

-include_lib("compiler/src/beam_types.hrl").

-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

consistency() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            consistency_check(TypeA, TypeB)).

consistency_check(A, B) ->
    consistency_check_1(A, B),
    consistency_check_1(B, A),
    true.

consistency_check_1(A, B) ->
    A = beam_types:meet(A, beam_types:join(A, B)),
    A = beam_types:join(A, beam_types:meet(A, B)),
    ok.

commutativity() ->
    ?FORALL({TypeA, TypeB},
            ?LET(TypeA, type(),
                 ?LET(TypeB, type(), {TypeA, TypeB})),
            commutativity_check(TypeA, TypeB)).

commutativity_check(A, B) ->
    true = beam_types:meet(A, B) =:= beam_types:meet(B, A),
    true = beam_types:join(A, B) =:= beam_types:join(B, A),
    true.

%%%
%%% Generators
%%%

type() ->
    type(0).

type(Depth) ->
    oneof(nested_types(Depth) ++
              numerical_types() ++
              list_types() ++
              other_types()).

other_types() ->
    [any, gen_atom(), gen_binary(), none].

list_types() ->
    [cons, list, nil].

numerical_types() ->
    [gen_integer(), float, number].

nested_types(Depth) when Depth >= 3 -> [];
nested_types(Depth) -> [#t_map{}, gen_tuple(Depth + 1)].

gen_atom() ->
    ?LET(Size, range(0, ?ATOM_SET_SIZE),
         case Size of
             0 ->
                 #t_atom{};
             _ ->
                 ?LET(Set, sized_list(Size, atom()),
                      begin
                          #t_atom{elements=ordsets:from_list(Set)}
                      end)
         end).

gen_binary() ->
    ?SHRINK(#t_bitstring{unit=range(1, 128)},
            [#t_bitstring{unit=[1, 2, 3, 5, 7, 8, 16, 32, 64]}]).

gen_integer() ->
    oneof([gen_integer_bounded(), #t_integer{}]).

gen_integer_bounded() ->
    ?LET(A, integer(),
         ?LET(B, integer(),
              begin
                  #t_integer{elements={min(A,B), max(A,B)}}
              end)).

gen_tuple(Depth) ->
    ?SIZED(Size,
           ?LET(Exact, oneof([true, false]),
                ?LET(Elements, gen_tuple_elements(Size, Depth),
                     begin
                         #t_tuple{exact=Exact,
                                  size=Size,
                                  elements=Elements}
                     end))).

gen_tuple_elements(Size, Depth) ->
    ?LET(Types, sized_list(rand:uniform(Size div 4 + 1), gen_element(Depth)),
         maps:from_list([{rand:uniform(Size), T} || T <- Types])).

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
