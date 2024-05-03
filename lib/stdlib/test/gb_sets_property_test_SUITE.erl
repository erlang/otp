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
-module(gb_sets_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() -> [
          add_case,
          balance_case,
          delete_case, delete_any_case,
          difference_case,
          from_ordset_case,
          insert_case,
          is_member_case,
          iterator_case, iterator_from_case,
          larger_case,
          largest_case,
          singleton_case,
          smaller_case,
          smallest_case,
          take_largest_case,
          take_smallest_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

add_case(Config) ->
    do_proptest(prop_add, Config).

balance_case(Config) ->
    do_proptest(prop_balance, Config).

delete_case(Config) ->
    do_proptest(prop_delete, Config).

delete_any_case(Config) ->
    do_proptest(prop_delete_any, Config).

difference_case(Config) ->
    do_proptest(prop_difference, Config).

from_ordset_case(Config) ->
    do_proptest(prop_from_ordset, Config).

insert_case(Config) ->
    do_proptest(prop_insert, Config).

is_member_case(Config) ->
    do_proptest(prop_is_member, Config).

iterator_case(Config) ->
    do_proptest(prop_iterator, Config).

iterator_from_case(Config) ->
    do_proptest(prop_iterator_from, Config).

larger_case(Config) ->
    do_proptest(prop_larger, Config).

largest_case(Config) ->
    do_proptest(prop_largest, Config).

singleton_case(Config) ->
    do_proptest(prop_singleton, Config).

smaller_case(Config) ->
    do_proptest(prop_smaller, Config).

smallest_case(Config) ->
    do_proptest(prop_smallest, Config).

take_largest_case(Config) ->
    do_proptest(prop_take_largest, Config).

take_smallest_case(Config) ->
    do_proptest(prop_take_smallest, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(
        gb_sets_prop:Prop(),
        Config).
