%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022. All Rights Reserved.
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
-module(binary_property_test_SUITE).

-compile([export_all, nowarn_export_all]).

all() ->
    [at_case, at_invalid_case,
     bin_to_list_1_case,
     bin_to_list_2_3_case, bin_to_list_2_3_invalid_case,
     compile_pattern_case, compile_pattern_invalid_case,
     copy_case, copy_2_invalid_case,
     decode_hex_case, decode_hex_invalid_case,
     decode_unsigned_case,
     encode_hex_case,
     encode_unsigned_case, encode_unsigned_invalid_case,
     first_case,
     last_case,
     list_to_bin_case, list_to_bin_invalid_case,
     longest_common_prefix_case,
     longest_common_suffix_case,
     match_2_case,
     match_3_case, match_3_invalid_case,
     matches_2_case,
     matches_3_case, matches_3_invalid_case,
     part_case, part_invalid_case,
     replace_3_case,
     replace_4_case, replace_4_invalid1_case, replace_4_invalid2_case,
     split_2_case,
     split_3_case, split_3_invalid_case].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(binary_prop:Prop(), Config).

at_case(Config) ->
    do_proptest(prop_at, Config).

at_invalid_case(Config) ->
    do_proptest(prop_at_invalid, Config).

bin_to_list_1_case(Config) ->
    do_proptest(prop_bin_to_list_1, Config).

bin_to_list_2_3_case(Config) ->
    do_proptest(prop_bin_to_list_2_3, Config).

bin_to_list_2_3_invalid_case(Config) ->
    do_proptest(prop_bin_to_list_2_3_invalid, Config).

compile_pattern_case(Config) ->
    do_proptest(prop_compile_pattern, Config).

compile_pattern_invalid_case(Config) ->
    do_proptest(prop_compile_pattern_invalid, Config).

copy_case(Config) ->
    do_proptest(prop_copy, Config).

copy_2_invalid_case(Config) ->
    do_proptest(prop_copy_2_invalid, Config).

decode_hex_case(Config) ->
    do_proptest(prop_decode_hex, Config).

decode_hex_invalid_case(Config) ->
    do_proptest(prop_decode_hex_invalid, Config).

decode_unsigned_case(Config) ->
    do_proptest(prop_decode_unsigned, Config).

encode_hex_case(Config) ->
    do_proptest(prop_encode_hex, Config).

encode_unsigned_case(Config) ->
    do_proptest(prop_encode_unsigned, Config).

encode_unsigned_invalid_case(Config) ->
    do_proptest(prop_encode_unsigned_invalid, Config).

first_case(Config) ->
    do_proptest(prop_first, Config).

last_case(Config) ->
    do_proptest(prop_last, Config).

list_to_bin_case(Config) ->
    do_proptest(prop_list_to_bin, Config).

list_to_bin_invalid_case(Config) ->
    do_proptest(prop_list_to_bin_invalid, Config).

longest_common_prefix_case(Config) ->
    do_proptest(prop_longest_common_prefix, Config).

longest_common_suffix_case(Config) ->
    do_proptest(prop_longest_common_suffix, Config).

match_2_case(Config) ->
    do_proptest(prop_match_2, Config).

match_3_case(Config) ->
    do_proptest(prop_match_3, Config).

match_3_invalid_case(Config) ->
    do_proptest(prop_match_3_invalid, Config).

matches_2_case(Config) ->
    do_proptest(prop_matches_2, Config).

matches_3_case(Config) ->
    do_proptest(prop_matches_3, Config).

matches_3_invalid_case(Config) ->
    do_proptest(prop_matches_3_invalid, Config).

part_case(Config) ->
    do_proptest(prop_part, Config).

part_invalid_case(Config) ->
    do_proptest(prop_part_invalid, Config).

replace_3_case(Config) ->
    do_proptest(prop_replace_3, Config).

replace_4_case(Config) ->
    do_proptest(prop_replace_4, Config).

replace_4_invalid1_case(Config) ->
    do_proptest(prop_replace_4_invalid1, Config).

replace_4_invalid2_case(Config) ->
    do_proptest(prop_replace_4_invalid2, Config).

split_2_case(Config) ->
    do_proptest(prop_split_2, Config).

split_3_case(Config) ->
    do_proptest(prop_split_3, Config).

split_3_invalid_case(Config) ->
    do_proptest(prop_split_3_invalid, Config).

