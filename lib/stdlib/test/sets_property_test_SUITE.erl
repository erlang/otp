%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2023. All Rights Reserved.
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
-module(sets_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [
        add_element_case,
        del_element_case,
        filter_case,
        filtermap_case,
        fold_case,
	from_list_case,
        intersection_1_case,
        intersection_2_case,
        is_disjoint_case,
        is_element_case,
        is_empty_case,
        is_equal_case,
        is_set_case,
        is_subset_case,
	map_case,
        size_case,
        subtract_case,
        to_list_case,
        union_1_case,
        union_2_case,
	operations_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(sets_prop:Prop(), Config).

add_element_case(Config) ->
    do_proptest(prop_add_element, Config).

del_element_case(Config) ->
    do_proptest(prop_del_element, Config).

filter_case(Config) ->
    do_proptest(prop_filter, Config).

filtermap_case(Config) ->
    do_proptest(prop_filtermap, Config).

fold_case(Config) ->
    do_proptest(prop_fold, Config).

from_list_case(Config) ->
    do_proptest(prop_from_list, Config).

intersection_1_case(Config) ->
    do_proptest(prop_intersection_1, Config).

intersection_2_case(Config) ->
    do_proptest(prop_intersection_2, Config).

is_disjoint_case(Config) ->
    do_proptest(prop_is_disjoint, Config).

is_element_case(Config) ->
    do_proptest(prop_is_element, Config).

is_empty_case(Config) ->
    do_proptest(prop_is_empty, Config).

is_equal_case(Config) ->
    do_proptest(prop_is_equal, Config).

is_set_case(Config) ->
    do_proptest(prop_is_set, Config).

is_subset_case(Config) ->
    do_proptest(prop_is_subset, Config).

map_case(Config) ->
    do_proptest(prop_map, Config).

size_case(Config) ->
    do_proptest(prop_size, Config).

subtract_case(Config) ->
    do_proptest(prop_subtract, Config).

to_list_case(Config) ->
    do_proptest(prop_to_list, Config).

union_1_case(Config) ->
    do_proptest(prop_union_1, Config).

union_2_case(Config) ->
    do_proptest(prop_union_2, Config).

operations_case(Config) ->
    do_proptest(prop_operations, Config).
