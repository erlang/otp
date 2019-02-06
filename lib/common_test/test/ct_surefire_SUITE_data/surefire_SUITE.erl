%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File: surefire_SUITE.erl
%%
%% Description:
%%    This file contains the test cases for cth_surefire.
%%
%% Test  of surefire support in common_test
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
-module(surefire_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

all() ->
    testcases() ++ [{group,g},{group,g_fail}].

groups() ->
    [{g,testcases()},
     {g_fail,[tc_ok]}].

testcases() ->
    [tc_ok,
     tc_fail,
     tc_skip,
     tc_autoskip_require].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(g_fail, _Config) ->
    exit(all_cases_should_be_skipped);
init_per_group(_, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%%-----------------------------------------------------------------
%%% Test cases
break(_Config) ->
    test_server:break(""),
    ok.

tc_ok(_Config) ->
    ok.

tc_fail(_Config) ->
    ct:fail("this test should fail").

tc_skip(_Config) ->
    {skip,"this test is skipped"}.

tc_autoskip_require() ->
    [{require,whatever}].
tc_autoskip_require(Config) ->
    ct:fail("this test should never be executed - it should be autoskipped").
