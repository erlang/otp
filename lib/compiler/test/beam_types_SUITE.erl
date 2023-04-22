%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

-module(beam_types_SUITE).

-define(BEAM_TYPES_INTERNAL, true).
-include_lib("compiler/src/beam_types.hrl").

-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1]).

-export([absorption/1,
         associativity/1,
         commutativity/1,
         idempotence/1,
         identity/1,
         subtraction/1]).

-export([binary_absorption/1,
         integer_absorption/1,
         integer_associativity/1,
         tuple_absorption/1,
         tuple_set_limit/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,property_tests},
     binary_absorption,
     integer_absorption,
     integer_associativity,
     tuple_absorption,
     tuple_set_limit].

groups() ->
    [{property_tests,[parallel],
      [absorption,
       associativity,
       commutativity,
       idempotence,
       identity,
       subtraction]}].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

absorption(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:absorption()).
    true = ct_property_test:quickcheck(beam_types_prop:absorption(), Config).

associativity(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:associativity()).
    true = ct_property_test:quickcheck(beam_types_prop:associativity(), Config).

commutativity(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:commutativity()).
    true = ct_property_test:quickcheck(beam_types_prop:commutativity(), Config).

idempotence(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:idempotence()).
    true = ct_property_test:quickcheck(beam_types_prop:idempotence(), Config).

identity(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:identity()).
    true = ct_property_test:quickcheck(beam_types_prop:identity(), Config).

subtraction(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:subtraction()).
    true = ct_property_test:quickcheck(beam_types_prop:subtraction(), Config).

binary_absorption(Config) when is_list(Config) ->
    %% These binaries should meet into {binary,12} as that's the best common
    %% unit for both types.
    A = #t_bitstring{size_unit=4},
    B = #t_bitstring{size_unit=6},

    #t_bitstring{size_unit=12} = beam_types:meet(A, B),
    #t_bitstring{size_unit=2} = beam_types:join(A, B),

    A = beam_types:meet(A, beam_types:join(A, B)),
    A = beam_types:join(A, beam_types:meet(A, B)),

    %% Tests that the appendable flag behaves as intended.
    C = #t_bitstring{size_unit=4,appendable=true},
    D = #t_bitstring{size_unit=6},

    #t_bitstring{size_unit=12,appendable=true} = beam_types:meet(C, D),
    #t_bitstring{size_unit=2,appendable=false} = beam_types:join(C, D),

    C = beam_types:meet(C, beam_types:join(C, D)),
    C = beam_types:join(C, beam_types:meet(C, D)),

    ok.

integer_absorption(Config) when is_list(Config) ->
    %% Integers that don't overlap at all should never meet.
    A = #t_integer{elements={2,3}},
    B = #t_integer{elements={4,5}},

    none = beam_types:meet(A, B),
    #t_integer{elements={2,5}} = beam_types:join(A, B),

    A = beam_types:meet(A, beam_types:join(A, B)),
    A = beam_types:join(A, beam_types:meet(A, B)),

    ok.

integer_associativity(Config) when is_list(Config) ->
    A = #t_integer{elements={3,5}},
    B = #t_integer{elements={4,6}},
    C = #t_integer{elements={5,5}},

    %% a ∨ (b ∨ c) = (a ∨ b) ∨ c,
    LHS_Join = beam_types:join(A, beam_types:join(B, C)),
    RHS_Join = beam_types:join(beam_types:join(A, B), C),
    #t_integer{elements={3,6}} = LHS_Join = RHS_Join,

    %% a ∧ (b ∧ c) = (a ∧ b) ∧ c.
    LHS_Meet = beam_types:meet(A, beam_types:meet(B, C)),
    RHS_Meet = beam_types:meet(beam_types:meet(A, B), C),
    #t_integer{elements={5,5}} = LHS_Meet = RHS_Meet,

    ok.

tuple_absorption(Config) when is_list(Config) ->
    %% An inexact tuple can't meet an exact one that's smaller

    A = #t_tuple{size=3,exact=true,
                 elements=#{1 => #t_atom{elements=[gurka]}}},
    B = #t_tuple{size=5,exact=false,
                 elements=#{3 => #t_atom{elements=[gaffel]}}},

    A = beam_types:meet(A, beam_types:join(A, B)),
    A = beam_types:join(A, beam_types:meet(A, B)),

    ok.

tuple_set_limit(Config) when is_list(Config) ->
    %% When joining two tuple sets of differing sizes, the resulting set could
    %% become larger than ?TUPLE_SET_LIMIT.

    As = [#t_tuple{size=N,exact=true,
                   elements=#{ 1 => #t_integer{elements={N,N}} }} ||
             N <- lists:seq(1, ?TUPLE_SET_LIMIT)],

    Bs = [#t_tuple{size=1,exact=true,
                   elements=#{ 1 => #t_integer{elements={N,N}} }} ||
             N <- lists:seq(1, ?TUPLE_SET_LIMIT)],

    A = beam_types:join(As),
    B = beam_types:join(Bs),

    beam_types:verified_type(beam_types:join(A, B)),

    ok.
