%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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

-module(ct_update_config_SUITE2).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile([export_all, nowarn_export_all]).
-include("ct.hrl").

-define(now, ct_test_support:unique_timestamp()).

%% Test server callback functions
init_per_suite(Config) ->
    [{init_per_suite,?now}|Config].

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    [{init_per_group,?now}|Config].

end_per_group(_GroupName, _Config) ->
    ok.

all() ->
    [{group,group1}].

groups() ->
    [{group1,[],[test_case,
                 test_case_fail,
                 test_case_timetrap]}].

test_case(Config) when is_list(Config) ->
    ok.

test_case_fail(Config) when is_list(Config) ->
    ct:fail(because_i_want_failure).

test_case_timetrap() ->
    [{timetrap, {seconds, 1}}].

test_case_timetrap(_) ->
    ct:sleep(infinity).
