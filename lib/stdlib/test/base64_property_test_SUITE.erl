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
-module(base64_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [
     encode_1_case, encode_2_case,
     encode_to_string_1_case, encode_to_string_2_case,
     decode_1_case, decode_2_case,
     decode_1_malformed_case, decode_2_malformed_case,
     decode_1_noisy_case, decode_2_noisy_case,
     decode_to_string_1_case, decode_to_string_2_case,
     decode_to_string_1_malformed_case, decode_to_string_2_malformed_case,
     decode_to_string_1_noisy_case, decode_to_string_2_noisy_case,
     mime_decode_1_case, mime_decode_2_case,
     mime_decode_1_malformed_case, mime_decode_2_malformed_case,
     mime_decode_to_string_1_case, mime_decode_to_string_2_case,
     mime_decode_to_string_1_malformed_case, mime_decode_to_string_2_malformed_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

encode_1_case(Config) ->
    do_proptest(prop_encode_1, Config).

encode_2_case(Config) ->
    do_proptest(prop_encode_2, Config).

encode_to_string_1_case(Config) ->
    do_proptest(prop_encode_to_string_1, Config).

encode_to_string_2_case(Config) ->
    do_proptest(prop_encode_to_string_2, Config).

decode_1_case(Config) ->
    do_proptest(prop_decode_1, Config).

decode_2_case(Config) ->
    do_proptest(prop_decode_2, Config).

decode_1_malformed_case(Config) ->
    do_proptest(prop_decode_1_malformed, Config).

decode_2_malformed_case(Config) ->
    do_proptest(prop_decode_2_malformed, Config).

decode_1_noisy_case(Config) ->
    do_proptest(prop_decode_1_noisy, Config).

decode_2_noisy_case(Config) ->
    do_proptest(prop_decode_2_noisy, Config).

decode_to_string_1_case(Config) ->
    do_proptest(prop_decode_to_string_1, Config).

decode_to_string_2_case(Config) ->
    do_proptest(prop_decode_to_string_2, Config).

decode_to_string_1_malformed_case(Config) ->
    do_proptest(prop_decode_to_string_1_malformed, Config).

decode_to_string_2_malformed_case(Config) ->
    do_proptest(prop_decode_to_string_2_malformed, Config).

decode_to_string_1_noisy_case(Config) ->
    do_proptest(prop_decode_to_string_1_noisy, Config).

decode_to_string_2_noisy_case(Config) ->
    do_proptest(prop_decode_to_string_2_noisy, Config).

mime_decode_1_case(Config) ->
    do_proptest(prop_mime_decode_1, Config).

mime_decode_2_case(Config) ->
    do_proptest(prop_mime_decode_2, Config).

mime_decode_1_malformed_case(Config) ->
    do_proptest(prop_mime_decode_1_malformed, Config).

mime_decode_2_malformed_case(Config) ->
    do_proptest(prop_mime_decode_2_malformed, Config).

mime_decode_to_string_1_case(Config) ->
    do_proptest(prop_mime_decode_to_string_1, Config).

mime_decode_to_string_2_case(Config) ->
    do_proptest(prop_mime_decode_to_string_2, Config).

mime_decode_to_string_1_malformed_case(Config) ->
    do_proptest(prop_mime_decode_to_string_1_malformed, Config).

mime_decode_to_string_2_malformed_case(Config) ->
    do_proptest(prop_mime_decode_to_string_2_malformed, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(
        base64_prop:Prop(),
        Config).
