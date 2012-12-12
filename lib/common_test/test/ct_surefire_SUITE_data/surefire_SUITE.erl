%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%----------------------------------------------------------------------
%% File: surefire_SUITE.erl
%%
%% Description:
%%    This file contains the test cases for cth_surefire.
%%
%% @author Support
%% @doc Test  of surefire support in common_test
%% @end
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
