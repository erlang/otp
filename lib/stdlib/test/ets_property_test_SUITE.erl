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
-module(ets_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() -> [
          first_case,
          next_case,
          last_case,
          prev_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

first_case(Config) ->
    do_proptest(prop_first, Config).

next_case(Config) ->
    do_proptest(prop_next, Config).

last_case(Config) ->
    do_proptest(prop_last, Config).

prev_case(Config) ->
    do_proptest(prop_prev, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(
        ets_prop:Prop(),
        Config).
