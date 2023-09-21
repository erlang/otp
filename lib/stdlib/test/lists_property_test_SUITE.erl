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
-module(lists_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [
        all_true_case, all_false_case,
        any_true_case, any_false_case,
        append_1_case,
        append_2_case,
        concat_case,
        delete_case, delete_absent_case,
        droplast_case,
        dropwhile_case,
        duplicate_case,
        enumerate_1_case,
        enumerate_2_case,
        enumerate_3_case,
        filter_case,
        filtermap_case,
        flatlength_case,
        flatmap_case,
        flatten_1_case,
        flatten_2_case,
        foldl_case,
        foldr_case,
        foreach_case,
        join_case,
        keydelete_case, keydelete_absent_case,
        keyfind_case, keyfind_absent_case,
        keymap_case,
        keymember_case, keymember_absent_case,
        keymerge_case, keymerge_invalid_case,
        keyreplace_case, keyreplace_absent_case,
        keysearch_case, keysearch_absent_case,
        keysort_case,
        keystore_case, keystore_absent_case,
        keytake_case, keytake_absent_case,
        last_case,
        map_case,
        mapfoldl_case,
        mapfoldr_case,
        max_case,
        member_case, member_absent_case,
        merge_1_case, merge_1_invalid_case,
        merge_2_case, merge_2_invalid_case,
        merge_3_case, merge_3_invalid_case,
        merge3_case, merge3_invalid_case,
        min_case,
        nth_case, nth_outofrange_case,
        nthtail_case, nthtail_outofrange_case,
        partition_case,
        prefix_case,
        reverse_1_case,
        reverse_2_case,
        search_case, search_absent_case,
        seq2_case,
        seq3_case,
        sort_1_case,
        sort_2_case,
        split_case, split_outofrange_case,
        splitwith_case,
        sublist_2_case,
        sublist_3_case,
        subtract_case,
        suffix_case,
        sum_case,
        takewhile_case,
        ukeymerge_case, ukeymerge_invalid_case,
        ukeysort_case,
        umerge_1_case, umerge_1_invalid_case,
        umerge_2_case, umerge_2_invalid_case,
        umerge_3_case, umerge_3_invalid_case,
        umerge3_case, umerge3_invalid_case,
        uniq_1_case,
        uniq_2_case,
        unzip_case,
        unzip3_case,
        usort_1_case,
        usort_2_case,
        zip_2_case,
        zip_3_case,
        zip3_3_case,
        zip3_4_case,
        zipwith_3_case,
        zipwith_4_case,
        zipwith3_4_case,
        zipwith3_5_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(lists_prop:Prop(), Config).

all_true_case(Config) ->
    do_proptest(prop_all_true, Config).

all_false_case(Config) ->
    do_proptest(prop_all_false, Config).

any_true_case(Config) ->
    do_proptest(prop_any_true, Config).

any_false_case(Config) ->
    do_proptest(prop_any_false, Config).

append_1_case(Config) ->
    do_proptest(prop_append_1, Config).

append_2_case(Config) ->
    do_proptest(prop_append_2, Config).

concat_case(Config) ->
    do_proptest(prop_concat, Config).

delete_case(Config) ->
    do_proptest(prop_delete, Config).

delete_absent_case(Config) ->
    do_proptest(prop_delete_absent, Config).

droplast_case(Config) ->
    do_proptest(prop_droplast, Config).

dropwhile_case(Config) ->
    do_proptest(prop_dropwhile, Config).

duplicate_case(Config) ->
    do_proptest(prop_duplicate, Config).

enumerate_1_case(Config) ->
    do_proptest(prop_enumerate_1, Config).

enumerate_2_case(Config) ->
    do_proptest(prop_enumerate_2, Config).

enumerate_3_case(Config) ->
    do_proptest(prop_enumerate_3, Config).

filter_case(Config) ->
    do_proptest(prop_filter, Config).

filtermap_case(Config) ->
    do_proptest(prop_filtermap, Config).

flatlength_case(Config) ->
    do_proptest(prop_flatlength, Config).

flatmap_case(Config) ->
    do_proptest(prop_flatmap, Config).

flatten_1_case(Config) ->
    do_proptest(prop_flatten_1, Config).

flatten_2_case(Config) ->
    do_proptest(prop_flatten_2, Config).

foldl_case(Config) ->
    do_proptest(prop_foldl, Config).

foldr_case(Config) ->
    do_proptest(prop_foldr, Config).

foreach_case(Config) ->
    do_proptest(prop_foreach, Config).

join_case(Config) ->
    do_proptest(prop_join, Config).

keydelete_case(Config) ->
    do_proptest(prop_keydelete, Config).

keydelete_absent_case(Config) ->
    do_proptest(prop_keydelete_absent, Config).

keyfind_case(Config) ->
    do_proptest(prop_keyfind, Config).

keyfind_absent_case(Config) ->
    do_proptest(prop_keyfind_absent, Config).

keymap_case(Config) ->
    do_proptest(prop_keymap, Config).

keymember_case(Config) ->
    do_proptest(prop_keymember, Config).

keymember_absent_case(Config) ->
    do_proptest(prop_keymember_absent, Config).

keymerge_case(Config) ->
    do_proptest(prop_keymerge, Config).

keymerge_invalid_case(Config) ->
    do_proptest(prop_keymerge_invalid, Config).

keyreplace_case(Config) ->
    do_proptest(prop_keyreplace, Config).

keyreplace_absent_case(Config) ->
    do_proptest(prop_keyreplace_absent, Config).

keysearch_case(Config) ->
    do_proptest(prop_keysearch, Config).

keysearch_absent_case(Config) ->
    do_proptest(prop_keysearch_absent, Config).

keysort_case(Config) ->
    do_proptest(prop_keysort, Config).

keystore_case(Config) ->
    do_proptest(prop_keystore, Config).

keystore_absent_case(Config) ->
    do_proptest(prop_keystore_absent, Config).

keytake_case(Config) ->
    do_proptest(prop_keytake, Config).

keytake_absent_case(Config) ->
    do_proptest(prop_keytake_absent, Config).

last_case(Config) ->
    do_proptest(prop_last, Config).

map_case(Config) ->
    do_proptest(prop_map, Config).

mapfoldl_case(Config) ->
    do_proptest(prop_mapfoldl, Config).

mapfoldr_case(Config) ->
    do_proptest(prop_mapfoldr, Config).

max_case(Config) ->
    do_proptest(prop_max, Config).

member_case(Config) ->
    do_proptest(prop_member, Config).

member_absent_case(Config) ->
    do_proptest(prop_member_absent, Config).

merge_1_case(Config) ->
    do_proptest(prop_merge_1, Config).

merge_1_invalid_case(Config) ->
    do_proptest(prop_merge_1_invalid, Config).

merge_2_case(Config) ->
    do_proptest(prop_merge_2, Config).

merge_2_invalid_case(Config) ->
    do_proptest(prop_merge_2_invalid, Config).

merge_3_case(Config) ->
    do_proptest(prop_merge_3, Config).

merge_3_invalid_case(Config) ->
    do_proptest(prop_merge_3_invalid, Config).

merge3_case(Config) ->
    do_proptest(prop_merge3, Config).

merge3_invalid_case(Config) ->
    do_proptest(prop_merge3_invalid, Config).

min_case(Config) ->
    do_proptest(prop_min, Config).

nth_case(Config) ->
    do_proptest(prop_nth, Config).

nth_outofrange_case(Config) ->
    do_proptest(prop_nth_outofrange, Config).

nthtail_case(Config) ->
    do_proptest(prop_nthtail, Config).

nthtail_outofrange_case(Config) ->
    do_proptest(prop_nthtail_outofrange, Config).

partition_case(Config) ->
    do_proptest(prop_partition, Config).

prefix_case(Config) ->
    do_proptest(prop_prefix, Config).

reverse_1_case(Config) ->
    do_proptest(prop_reverse_1, Config).

reverse_2_case(Config) ->
    do_proptest(prop_reverse_2, Config).

search_case(Config) ->
    do_proptest(prop_search, Config).

search_absent_case(Config) ->
    do_proptest(prop_search_absent, Config).

seq2_case(Config) ->
    do_proptest(prop_seq2, Config).

seq3_case(Config) ->
    do_proptest(prop_seq3, Config).

sort_1_case(Config) ->
    do_proptest(prop_sort_1, Config).

sort_2_case(Config) ->
    do_proptest(prop_sort_2, Config).

split_case(Config) ->
    do_proptest(prop_split, Config).

split_outofrange_case(Config) ->
    do_proptest(prop_split_outofrange, Config).

splitwith_case(Config) ->
    do_proptest(prop_splitwith, Config).

sublist_2_case(Config) ->
    do_proptest(prop_sublist_2, Config).

sublist_3_case(Config) ->
    do_proptest(prop_sublist_3, Config).

subtract_case(Config) ->
    do_proptest(prop_subtract, Config).

suffix_case(Config) ->
    do_proptest(prop_suffix, Config).

sum_case(Config) ->
    do_proptest(prop_sum, Config).

takewhile_case(Config) ->
    do_proptest(prop_takewhile, Config).

ukeymerge_case(Config) ->
    do_proptest(prop_ukeymerge, Config).

ukeymerge_invalid_case(Config) ->
    do_proptest(prop_ukeymerge_invalid, Config).

ukeysort_case(Config) ->
    do_proptest(prop_ukeysort, Config).

umerge_1_case(Config) ->
    do_proptest(prop_umerge_1, Config).

umerge_1_invalid_case(Config) ->
    do_proptest(prop_umerge_1_invalid, Config).

umerge_2_case(Config) ->
    do_proptest(prop_umerge_2, Config).

umerge_2_invalid_case(Config) ->
    do_proptest(prop_umerge_2_invalid, Config).

umerge_3_case(Config) ->
    do_proptest(prop_umerge_3, Config).

umerge_3_invalid_case(Config) ->
    do_proptest(prop_umerge_3_invalid, Config).

umerge3_case(Config) ->
    do_proptest(prop_umerge3, Config).

umerge3_invalid_case(Config) ->
    do_proptest(prop_umerge3_invalid, Config).

uniq_1_case(Config) ->
    do_proptest(prop_uniq_1, Config).

uniq_2_case(Config) ->
    do_proptest(prop_uniq_2, Config).

unzip_case(Config) ->
    do_proptest(prop_unzip, Config).

unzip3_case(Config) ->
    do_proptest(prop_unzip3, Config).

usort_1_case(Config) ->
    do_proptest(prop_usort_1, Config).

usort_2_case(Config) ->
    do_proptest(prop_usort_2, Config).

zip_2_case(Config) ->
    do_proptest(prop_zip_2, Config).

zip_3_case(Config) ->
    do_proptest(prop_zip_3, Config).

zip3_3_case(Config) ->
    do_proptest(prop_zip3_3, Config).

zip3_4_case(Config) ->
    do_proptest(prop_zip3_4, Config).

zipwith_3_case(Config) ->
    do_proptest(prop_zipwith_3, Config).

zipwith_4_case(Config) ->
    do_proptest(prop_zipwith_4, Config).

zipwith3_4_case(Config) ->
    do_proptest(prop_zipwith3_4, Config).

zipwith3_5_case(Config) ->
    do_proptest(prop_zipwith3_5, Config).

