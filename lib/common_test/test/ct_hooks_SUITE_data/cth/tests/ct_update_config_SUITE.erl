%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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

-module(ct_update_config_SUITE).

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

init_per_testcase(_TestCase, Config) ->
    [{init_per_testcase,?now}|Config].

end_per_testcase(test_case_timetrap_end_per_testcase_crash, _Config) ->
    1/0, % invoke code crash
    ok;
end_per_testcase(_TestCase, _Config) ->
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
                 test_case_timetrap,
                 test_case_timetrap_end_per_testcase_crash,
                 test_case_badmatch,
                 test_case_spawn_crash]}].

test_case(Config) when is_list(Config) ->
    ok.

test_case_fail(Config) when is_list(Config) ->
    ct:fail(because_i_want_failure).

test_case_timetrap() ->
    [{timetrap, {seconds, 1}}].

test_case_timetrap(_) ->
    ct:sleep(infinity).

test_case_badmatch(_) ->
    1 = 2.

%% test case is a slightly different flow, but at the moment behavior should be
%% the same as for test_case_timetrap, as crash in end_per_testcase is catched
%% in test_server module
test_case_timetrap_end_per_testcase_crash() ->
    [{timetrap, {seconds, 1}}].

test_case_timetrap_end_per_testcase_crash(Config) when is_list(Config) ->
    ct:sleep(infinity).

test_case_spawn_crash(_Config) ->
    spawn_link(fun() -> error(bam) end),
    ct:sleep(infinity).
