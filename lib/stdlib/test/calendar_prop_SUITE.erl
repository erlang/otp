%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
-module(calendar_prop_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
         rfc3339_lists_binaries/1,
         universal_time_system_time_symmetry/1,
         local_time_system_time_symmetry/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [rfc3339_lists_binaries,
    universal_time_system_time_symmetry,
    local_time_system_time_symmetry].

groups() ->
    [].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

rfc3339_lists_binaries(Config) when is_list(Config) ->
    ct_property_test:quickcheck(
        calendar_prop:rfc3339_lists_binaries(),
        Config).

universal_time_system_time_symmetry(Config) when is_list(Config) ->
    ct_property_test:quickcheck(
        calendar_prop:universal_time_system_time_symmetry(),
        Config).

local_time_system_time_symmetry(Config) when is_list(Config) ->
    ct_property_test:quickcheck(
        calendar_prop:local_time_system_time_symmetry(),
        Config).
