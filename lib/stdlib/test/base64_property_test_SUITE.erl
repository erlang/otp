%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
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
     encode_case,
     encode_to_string_case,
     decode_case,
     decode_malformed_case,
     decode_noisy_case,
     decode_to_string_case,
     decode_to_string_malformed_case,
     decode_to_string_noisy_case,
     mime_decode_case,
     mime_decode_malformed_case,
     mime_decode_to_string_case,
     mime_decode_to_string_malformed_case
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

encode_case(Config) ->
    do_proptest(prop_encode, Config).

encode_to_string_case(Config) ->
    do_proptest(prop_encode_to_string, Config).

decode_case(Config) ->
    do_proptest(prop_decode, Config).

decode_malformed_case(Config) ->
    do_proptest(prop_decode_malformed, Config).

decode_noisy_case(Config) ->
    do_proptest(prop_decode_noisy, Config).

decode_to_string_case(Config) ->
    do_proptest(prop_decode_to_string, Config).

decode_to_string_malformed_case(Config) ->
    do_proptest(prop_decode_to_string_malformed, Config).

decode_to_string_noisy_case(Config) ->
    do_proptest(prop_decode_to_string_noisy, Config).

mime_decode_case(Config) ->
    do_proptest(prop_mime_decode, Config).

mime_decode_malformed_case(Config) ->
    do_proptest(prop_mime_decode_malformed, Config).

mime_decode_to_string_case(Config) ->
    do_proptest(prop_mime_decode_to_string, Config).

mime_decode_to_string_malformed_case(Config) ->
    do_proptest(prop_mime_decode_to_string_malformed, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(
        base64_prop:Prop(),
        Config).
