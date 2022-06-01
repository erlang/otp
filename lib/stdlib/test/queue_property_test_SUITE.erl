%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2022. All Rights Reserved.
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
-module(queue_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [
        new_case,
        is_queue_case,
        list_conversion_case,
        from_list_invalid_case,
        to_list_invalid_case,

        all_case,
        all_invalid_case,
        any_case,
        any_invalid_case,
        cons_case,
        cons_invalid_case,
        daeh_case,
        daeh_invalid_case,
        delete_case,
        delete_invalid_case,
        delete_r_case,
        delete_r_invalid_case,
        delete_with_case,
        delete_with_invalid_case,
        delete_with_r_case,
        delete_with_r_invalid_case,
        drop_case,
        drop_invalid_case,
        drop_r_case,
        drop_r_invalid_case,
        filter_case,
        filter_invalid_case,
        filtermap_case,
        filtermap_invalid_case,
        fold_case,
        fold_invalid_case,
        get_case,
        get_invalid_case,
        get_r_case,
        get_r_invalid_case,
        head_case,
        head_invalid_case,
        in_case,
        in_invalid_case,
        in_r_case,
        in_r_invalid_case,
        init_case,
        init_invalid_case,
        is_empty_case,
        is_empty_invalid_case,
        join_case,
        join_invalid_case,
        last_case,
        last_invalid_case,
        len_case,
        len_invalid_case,
        liat_case,
        liat_invalid_case,
        member_case,
        member_invalid_case,
        out_case,
        out_invalid_case,
        out_r_case,
        out_r_invalid_case,
        peek_case,
        peek_invalid_case,
        peek_r_case,
        peek_r_invalid_case,
        reverse_case,
        reverse_invalid_case,
        snoc_case,
        snoc_invalid_case,
        split_case,
        split_invalid_case,
        tail_case,
        tail_invalid_case,

        ops_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

new_case(Config) ->
    do_proptest(prop_new, Config).

is_queue_case(Config) ->
    do_proptest(prop_is_queue, Config).

list_conversion_case(Config) ->
    do_proptest(prop_list_conversion, Config).

from_list_invalid_case(Config) ->
    do_proptest(prop_from_list_invalid, Config).

to_list_invalid_case(Config) ->
    do_proptest(prop_to_list_invalid, Config).

all_case(Config) ->
    do_proptest(prop_all, Config).

all_invalid_case(Config) ->
    do_proptest(prop_all_invalid, Config).

any_case(Config) ->
    do_proptest(prop_any, Config).

any_invalid_case(Config) ->
    do_proptest(prop_any_invalid, Config).

cons_case(Config) ->
    do_proptest(prop_cons, Config).

cons_invalid_case(Config) ->
    do_proptest(prop_cons_invalid, Config).

daeh_case(Config) ->
    do_proptest(prop_daeh, Config).

daeh_invalid_case(Config) ->
    do_proptest(prop_daeh_invalid, Config).

delete_case(Config) ->
    do_proptest(prop_delete, Config).

delete_invalid_case(Config) ->
    do_proptest(prop_delete_invalid, Config).

delete_r_case(Config) ->
    do_proptest(prop_delete_r, Config).

delete_r_invalid_case(Config) ->
    do_proptest(prop_delete_r_invalid, Config).

delete_with_case(Config) ->
    do_proptest(prop_delete_with, Config).

delete_with_invalid_case(Config) ->
    do_proptest(prop_delete_with_invalid, Config).

delete_with_r_case(Config) ->
    do_proptest(prop_delete_with_r, Config).

delete_with_r_invalid_case(Config) ->
    do_proptest(prop_delete_with_r_invalid, Config).

drop_case(Config) ->
    do_proptest(prop_drop, Config).

drop_invalid_case(Config) ->
    do_proptest(prop_drop_invalid, Config).

drop_r_case(Config) ->
    do_proptest(prop_drop_r, Config).

drop_r_invalid_case(Config) ->
    do_proptest(prop_drop_r_invalid, Config).

filter_case(Config) ->
    do_proptest(prop_filter, Config).

filter_invalid_case(Config) ->
    do_proptest(prop_filter_invalid, Config).

filtermap_case(Config) ->
    do_proptest(prop_filtermap, Config).

filtermap_invalid_case(Config) ->
    do_proptest(prop_filtermap_invalid, Config).

fold_case(Config) ->
    do_proptest(prop_fold, Config).

fold_invalid_case(Config) ->
    do_proptest(prop_fold_invalid, Config).

get_case(Config) ->
    do_proptest(prop_get, Config).

get_invalid_case(Config) ->
    do_proptest(prop_get_invalid, Config).

get_r_case(Config) ->
    do_proptest(prop_get_r, Config).

get_r_invalid_case(Config) ->
    do_proptest(prop_get_r_invalid, Config).

head_case(Config) ->
    do_proptest(prop_head, Config).

head_invalid_case(Config) ->
    do_proptest(prop_head_invalid, Config).

in_case(Config) ->
    do_proptest(prop_in, Config).

in_invalid_case(Config) ->
    do_proptest(prop_in_invalid, Config).

in_r_case(Config) ->
    do_proptest(prop_in_r, Config).

in_r_invalid_case(Config) ->
    do_proptest(prop_in_r_invalid, Config).

init_case(Config) ->
    do_proptest(prop_init, Config).

init_invalid_case(Config) ->
    do_proptest(prop_init_invalid, Config).

is_empty_case(Config) ->
    do_proptest(prop_is_empty, Config).

is_empty_invalid_case(Config) ->
    do_proptest(prop_is_empty_invalid, Config).

join_case(Config) ->
    do_proptest(prop_join, Config).

join_invalid_case(Config) ->
    do_proptest(prop_join_invalid, Config).

last_case(Config) ->
    do_proptest(prop_last, Config).

last_invalid_case(Config) ->
    do_proptest(prop_last_invalid, Config).

len_case(Config) ->
    do_proptest(prop_len, Config).

len_invalid_case(Config) ->
    do_proptest(prop_len_invalid, Config).

liat_case(Config) ->
    do_proptest(prop_liat, Config).

liat_invalid_case(Config) ->
    do_proptest(prop_liat_invalid, Config).

member_case(Config) ->
    do_proptest(prop_member, Config).

member_invalid_case(Config) ->
    do_proptest(prop_member_invalid, Config).

out_case(Config) ->
    do_proptest(prop_out, Config).

out_invalid_case(Config) ->
    do_proptest(prop_out_invalid, Config).

out_r_case(Config) ->
    do_proptest(prop_out_r, Config).

out_r_invalid_case(Config) ->
    do_proptest(prop_out_r_invalid, Config).

peek_case(Config) ->
    do_proptest(prop_peek, Config).

peek_invalid_case(Config) ->
    do_proptest(prop_peek_invalid, Config).

peek_r_case(Config) ->
    do_proptest(prop_peek_r, Config).

peek_r_invalid_case(Config) ->
    do_proptest(prop_peek_r_invalid, Config).

reverse_case(Config) ->
    do_proptest(prop_reverse, Config).

reverse_invalid_case(Config) ->
    do_proptest(prop_reverse_invalid, Config).

snoc_case(Config) ->
    do_proptest(prop_snoc, Config).

snoc_invalid_case(Config) ->
    do_proptest(prop_snoc_invalid, Config).

split_case(Config) ->
    do_proptest(prop_split, Config).

split_invalid_case(Config) ->
    do_proptest(prop_split_invalid, Config).

tail_case(Config) ->
    do_proptest(prop_tail, Config).

tail_invalid_case(Config) ->
    do_proptest(prop_tail_invalid, Config).

ops_case(Config) ->
    do_proptest(prop_ops, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(
        queue_prop:Prop(),
        Config).
