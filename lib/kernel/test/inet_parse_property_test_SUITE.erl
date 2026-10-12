%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2026. All Rights Reserved.
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
-module(inet_parse_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [ipv4_address_case,
     ipv4strict_address_case,
     ipv6strict_address_case,
     ipv6mapped_ipv4_address_case,
     ntoa_case].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

ipv4_address_case(Config) ->
    do_proptest(prop_ipv4_address, Config).

ipv4strict_address_case(Config) ->
    do_proptest(prop_ipv4strict_address, Config).

ipv6strict_address_case(Config) ->
    do_proptest(prop_ipv6strict_address, Config).

ipv6mapped_ipv4_address_case(Config) ->
    do_proptest(prop_ipv6mapped_ipv4_address, Config).

ntoa_case(Config) ->
    do_proptest(prop_ntoa, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(inet_parse_prop:Prop(), Config).
