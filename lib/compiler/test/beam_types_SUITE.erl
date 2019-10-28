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

-module(beam_types_SUITE).

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
         integer_associativity/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,property_tests},
     binary_absorption,
     integer_absorption,
     integer_associativity].

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

