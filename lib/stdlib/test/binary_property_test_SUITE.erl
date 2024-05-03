%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022-2024. All Rights Reserved.
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
    [{group, valid_input}, {group, invalid_input}].

valid_input_tests() ->
    [at_case,
     bin_to_list_1_case, bin_to_list_2_3_case,
     compile_pattern_case,
     copy_case,
     decode_hex_case,
     decode_unsigned_case,
     encode_hex_case,
     encode_unsigned_case,
     first_case,
     last_case,
     list_to_bin_case,
     longest_common_prefix_case,
     longest_common_suffix_case,
     match_case,
     matches_case,
     part_case,
     replace_case,
     split_case].

out_of_binary_tests() ->
    [at_invalid_index_case,
     bin_to_list_2_3_invalid_range_case,
     match_3_invalid_scope_case,
     matches_3_invalid_scope_case,
     part_invalid_range_case,
     replace_4_invalid_scope_case, replace_4_invalid_insert_replaced_case,
     split_3_invalid_scope_case].

invalid_subject_tests() ->
    [at_invalid_subject_case,
     bin_to_list_invalid_subject_case,
     copy_invalid_subject_case,
     decode_hex_invalid_subject_case,
     decode_unsigned_invalid_subject_case,
     encode_hex_invalid_subject_case,
     first_invalid_subject_case,
     last_invalid_subject_case,
     longest_common_prefix_invalid_subject_case,
     longest_common_suffix_invalid_subject_case,
     match_invalid_subject_case,
     matches_invalid_subject_case,
     part_invalid_subject_case,
     replace_invalid_subject_case,
     split_invalid_subject_case].

invalid_pattern_tests() ->
    [compile_pattern_invalid_pattern_case,
     match_invalid_pattern_case,
     matches_invalid_pattern_case,
     replace_invalid_pattern_case,
     split_invalid_pattern_case].

misc_invalid_input_tests() ->
    [copy_2_invalid_n_case,
     decode_hex_invalid_chars_case,
     decode_unsigned_2_invalid_endianness_case,
     encode_hex_2_invalid_case_case,
     encode_unsigned_invalid_integer_case,
     encode_unsigned_2_invalid_endianness_case,
     list_to_bin_invalid_bytes_case,
     replace_invalid_replacement_case].

groups() ->
    [{valid_input, [], valid_input_tests()},
     {invalid_input, [], [{out_of_binary, [], out_of_binary_tests()},
                          {invalid_subjects, [], invalid_subject_tests()},
                          {invalid_patterns, [], invalid_pattern_tests()},
                          {misc_invalid, [], misc_invalid_input_tests()}]}].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(binary_prop:Prop(), Config).

at_case(Config) ->
    do_proptest(prop_at, Config).

at_invalid_index_case(Config) ->
    do_proptest(prop_at_invalid_index, Config).

at_invalid_subject_case(Config) ->
    do_proptest(prop_at_invalid_subject, Config).

bin_to_list_1_case(Config) ->
    do_proptest(prop_bin_to_list_1, Config).

bin_to_list_2_3_case(Config) ->
    do_proptest(prop_bin_to_list_2_3, Config).

bin_to_list_2_3_invalid_range_case(Config) ->
    do_proptest(prop_bin_to_list_2_3_invalid_range, Config).

bin_to_list_invalid_subject_case(Config) ->
    do_proptest(prop_bin_to_list_invalid_subject, Config).

compile_pattern_case(Config) ->
    do_proptest(prop_compile_pattern, Config).

compile_pattern_invalid_pattern_case(Config) ->
    do_proptest(prop_compile_pattern_invalid_pattern, Config).

copy_case(Config) ->
    do_proptest(prop_copy, Config).

copy_2_invalid_n_case(Config) ->
    do_proptest(prop_copy_2_invalid_n, Config).

copy_invalid_subject_case(Config) ->
    do_proptest(prop_copy_invalid_subject, Config).

decode_hex_case(Config) ->
    do_proptest(prop_decode_hex, Config).

decode_hex_invalid_chars_case(Config) ->
    do_proptest(prop_decode_hex_invalid_chars, Config).

decode_hex_invalid_subject_case(Config) ->
    do_proptest(prop_decode_hex_invalid_subject, Config).

decode_unsigned_case(Config) ->
    do_proptest(prop_decode_unsigned, Config).

decode_unsigned_2_invalid_endianness_case(Config) ->
    do_proptest(prop_decode_unsigned_2_invalid_endianness, Config).

decode_unsigned_invalid_subject_case(Config) ->
    do_proptest(prop_decode_unsigned_invalid_subject, Config).

encode_hex_case(Config) ->
    do_proptest(prop_encode_hex, Config).

encode_hex_2_invalid_case_case(Config) ->
    do_proptest(prop_encode_hex_2_invalid_case, Config).

encode_hex_invalid_subject_case(Config) ->
    do_proptest(prop_encode_hex_invalid_subject, Config).

encode_unsigned_case(Config) ->
    do_proptest(prop_encode_unsigned, Config).

encode_unsigned_invalid_integer_case(Config) ->
    do_proptest(prop_encode_unsigned_invalid_integer, Config).

encode_unsigned_2_invalid_endianness_case(Config) ->
    do_proptest(prop_encode_unsigned_2_invalid_endianness, Config).

first_case(Config) ->
    do_proptest(prop_first, Config).

first_invalid_subject_case(Config) ->
    do_proptest(prop_first_invalid_subject, Config).

last_case(Config) ->
    do_proptest(prop_last, Config).

last_invalid_subject_case(Config) ->
    do_proptest(prop_last_invalid_subject, Config).

list_to_bin_case(Config) ->
    do_proptest(prop_list_to_bin, Config).

list_to_bin_invalid_bytes_case(Config) ->
    do_proptest(prop_list_to_bin_invalid_bytes, Config).

longest_common_prefix_case(Config) ->
    do_proptest(prop_longest_common_prefix, Config).

longest_common_prefix_invalid_subject_case(Config) ->
    do_proptest(prop_longest_common_prefix_invalid_subject, Config).

longest_common_suffix_case(Config) ->
    do_proptest(prop_longest_common_suffix, Config).

longest_common_suffix_invalid_subject_case(Config) ->
    do_proptest(prop_longest_common_suffix_invalid_subject, Config).

match_case(Config) ->
    do_proptest(prop_match, Config).

match_3_invalid_scope_case(Config) ->
    do_proptest(prop_match_3_invalid_scope, Config).

match_invalid_pattern_case(Config) ->
    do_proptest(prop_match_invalid_pattern, Config).

match_invalid_subject_case(Config) ->
    do_proptest(prop_match_invalid_subject, Config).

matches_case(Config) ->
    do_proptest(prop_matches, Config).

matches_3_invalid_scope_case(Config) ->
    do_proptest(prop_matches_3_invalid_scope, Config).

matches_invalid_pattern_case(Config) ->
    do_proptest(prop_matches_invalid_pattern, Config).

matches_invalid_subject_case(Config) ->
    do_proptest(prop_matches_invalid_subject, Config).

part_case(Config) ->
    do_proptest(prop_part, Config).

part_invalid_range_case(Config) ->
    do_proptest(prop_part_invalid_range, Config).

part_invalid_subject_case(Config) ->
    do_proptest(prop_part_invalid_subject, Config).

replace_case(Config) ->
    do_proptest(prop_replace, Config).

replace_4_invalid_scope_case(Config) ->
    do_proptest(prop_replace_4_invalid_scope, Config).

replace_4_invalid_insert_replaced_case(Config) ->
    do_proptest(prop_replace_4_invalid_insert_replaced, Config).

replace_invalid_pattern_case(Config) ->
    do_proptest(prop_replace_invalid_pattern, Config).

replace_invalid_subject_case(Config) ->
    do_proptest(prop_replace_invalid_subject, Config).

replace_invalid_replacement_case(Config) ->
    do_proptest(prop_replace_invalid_replacement, Config).

split_case(Config) ->
    do_proptest(prop_split, Config).

split_3_invalid_scope_case(Config) ->
    do_proptest(prop_split_3_invalid_scope, Config).

split_invalid_pattern_case(Config) ->
    do_proptest(prop_split_invalid_pattern, Config).

split_invalid_subject_case(Config) ->
    do_proptest(prop_split_invalid_subject, Config).

