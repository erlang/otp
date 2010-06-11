%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
-module(repeat_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% COMMON TEST CALLBACK FUNCTIONS
%%====================================================================

suite() ->
    [{timetrap,{minutes,1}}].

groups() ->
    [
      {test_group_1, [{repeat,2}], [testcase_1a,testcase_1b]},
      {test_group_2, [{repeat,1}], [testcase_2a,testcase_2b]},

      {test_group_3, [{repeat_until_all_fail,1}],
       [testcase_3a,
	{test_group_4, [{repeat_until_any_fail,1}],
	 [testcase_4a, testcase_4b]},
	testcase_3b]}
     ].

all() ->
    [
     {group, test_group_1},
     {group, test_group_2},
     {group, test_group_3}
    ].

%%--------------------------------------------------------------------
%% Suite Configuration
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

%%--------------------------------------------------------------------
%% Group Configuration
%%--------------------------------------------------------------------

init_per_group(Group, Config) ->
    Group = proplists:get_value(name,?config(tc_group_properties,Config)),
    ct:comment(Group),
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Testcase Configuration
%%--------------------------------------------------------------------

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Testcases
%%--------------------------------------------------------------------

testcase_1a(_) ->
    ok.
testcase_1b(_) ->
    ok.

testcase_2a(_) ->
    ok.
testcase_2b(_) ->
    ok.

testcase_3a(_) ->
    ok.
testcase_3b(_) ->
    ok.

testcase_4a(_) ->
    ok.
testcase_4b(_) ->
    ok.
