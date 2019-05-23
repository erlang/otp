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

-export([consistency/1, commutativity/1,
         binary_consistency/1, integer_consistency/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,property_tests}].

groups() ->
    [{property_tests,[], [consistency, commutativity,
                          binary_consistency, integer_consistency]}].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

consistency(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:consistency()).
    true = ct_property_test:quickcheck(beam_types_prop:consistency(), Config).

commutativity(Config) when is_list(Config) ->
    %% manual test: proper:quickcheck(beam_types_prop:commutativity()).
    true = ct_property_test:quickcheck(beam_types_prop:commutativity(), Config).

binary_consistency(Config) when is_list(Config) ->
    %% These binaries should meet into {binary,12} as that's the best common
    %% unit for both types.
    A = {binary, 4},
    B = {binary, 6},

    {binary, 12} = beam_types:meet(A, B),
    {binary, 2} = beam_types:join(A, B),

    A = beam_types:meet(A, beam_types:join(A, B)),
    A = beam_types:join(A, beam_types:meet(A, B)),

    ok.

integer_consistency(Config) when is_list(Config) ->
    %% Integers that don't overlap fully should never meet.
    A = #t_integer{elements={3,5}},
    B = #t_integer{elements={4,6}},

    none = beam_types:meet(A, B),
    #t_integer{elements={3,6}} = beam_types:join(A, B),

    A = beam_types:meet(A, beam_types:join(A, B)),
    A = beam_types:join(A, beam_types:meet(A, B)),

    ok.
