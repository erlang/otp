%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(maps_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [
        size_case,
        keys_case,
        values_case,
        from_list_case,
        from_keys_case,
        to_list_case,
        iteration_case,
        is_key_case,
        get_2_case, get_3_case,
        find_case,
        put_case,
        update_case,
        update_with_3_case, update_with_4_case,
        remove_case,
        take_case,
        with_case,
        without_case,
        foreach_case,
        fold_case,
        map_case,
        filter_case,
        filtermap_case,
        merge_case,
        merge_with_case,
        intersect_case,
        intersect_with_case,
        groups_from_list_2_case, groups_from_list_3_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(maps_prop:Prop(), Config).

size_case(Config) ->
    do_proptest(prop_size, Config).

keys_case(Config) ->
    do_proptest(prop_keys, Config).

values_case(Config) ->
    do_proptest(prop_values, Config).

from_list_case(Config) ->
    do_proptest(prop_from_list, Config).

from_keys_case(Config) ->
    do_proptest(prop_from_keys, Config).

to_list_case(Config) ->
    do_proptest(prop_to_list, Config).

iteration_case(Config) ->
    do_proptest(prop_iteration, Config).

is_key_case(Config) ->
    do_proptest(prop_is_key, Config).

get_2_case(Config) ->
    do_proptest(prop_get_2, Config).

get_3_case(Config) ->
    do_proptest(prop_get_3, Config).

find_case(Config) ->
    do_proptest(prop_find, Config).

put_case(Config) ->
    do_proptest(prop_put, Config).

update_case(Config) ->
    do_proptest(prop_update, Config).

update_with_3_case(Config) ->
    do_proptest(prop_update_with_3, Config).

update_with_4_case(Config) ->
    do_proptest(prop_update_with_4, Config).

remove_case(Config) ->
    do_proptest(prop_remove, Config).

take_case(Config) ->
    do_proptest(prop_take, Config).

with_case(Config) ->
    do_proptest(prop_with, Config).

without_case(Config) ->
    do_proptest(prop_without, Config).

foreach_case(Config) ->
    do_proptest(prop_foreach, Config).

fold_case(Config) ->
    do_proptest(prop_fold, Config).

map_case(Config) ->
    do_proptest(prop_map, Config).

filter_case(Config) ->
    do_proptest(prop_filter, Config).

filtermap_case(Config) ->
    do_proptest(prop_filtermap, Config).

merge_case(Config) ->
    do_proptest(prop_merge, Config).

merge_with_case(Config) ->
    do_proptest(prop_merge_with, Config).

intersect_case(Config) ->
    do_proptest(prop_intersect, Config).

intersect_with_case(Config) ->
    do_proptest(prop_intersect_with, Config).

groups_from_list_2_case(Config) ->
    do_proptest(prop_groups_from_list_2, Config).

groups_from_list_3_case(Config) ->
    do_proptest(prop_groups_from_list_3, Config).
