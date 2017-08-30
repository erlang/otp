%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(groups_21_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% COMMON TEST CALLBACK FUNCTIONS
%%====================================================================

suite() ->
    [{timetrap,{minutes,1}}].

groups() ->
    [
      {test_group_1a, [testcase_1a,testcase_1b]},

      {test_group_1b, [], [testcase_1a,testcase_1b]},

      {test_group_2, [], [testcase_2a,

			  {test_group_3, [], [testcase_3a,
					      testcase_3b]},
			  testcase_2b]},

      {test_group_4, [{test_group_5, [], [testcase_5a,

					  {group, test_group_6},

					  testcase_5b]}]},

      {test_group_6, [{group, test_group_7}]},

      {test_group_7, [testcase_7a,testcase_7b]}
     ].

all() ->
    [testcase_1,
     {group, test_group_1a},
     {group, test_group_1b},
     testcase_2,
     {group, test_group_2},
     testcase_3,
     {group, test_group_4}].

%% this func only for internal test purposes
grs_and_tcs() ->
    {[
      test_group_1a, test_group_1b,
      test_group_2, test_group_3,
      test_group_4, test_group_5,
      test_group_6, test_group_7
     ],
     [
      testcase_1,
      testcase_1a, testcase_1b,
      testcase_2,
      testcase_2a, testcase_2b,
      testcase_3a, testcase_3b,
      testcase_3,
      testcase_5a, testcase_5b,
      testcase_7a, testcase_7b
     ]}.

%%--------------------------------------------------------------------
%% Suite Configuration
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    [{suite,init}|Config].

end_per_suite(Config) ->
    init = ?config(suite,Config).

%%--------------------------------------------------------------------
%% Group Configuration
%%--------------------------------------------------------------------

init_per_group(Group, Config) ->
    [{name,Group}] = ?config(tc_group_properties,Config),
    {Grs,_} = grs_and_tcs(),
    case lists:member(Group, Grs) of
	true ->
	    ct:comment(io_lib:format("~w", [Group])),
	    init = ?config(suite,Config),
	    [{Group,Group} | Config];
	false ->
	    ct:fail({bad_group,Group})
    end.

end_per_group(Group, Config) ->
    {Grs,_} = grs_and_tcs(),
    case lists:member(Group, Grs) of
	true ->
	    init = ?config(suite,Config),
	    Group = ?config(Group,Config),
	    ok;
	false ->
	    ct:fail({bad_group,Group})
    end.

%%--------------------------------------------------------------------
%% Testcase Configuration
%%--------------------------------------------------------------------

init_per_testcase(TestCase, Config) ->
    {_,TCs} = grs_and_tcs(),
    case lists:member(TestCase, TCs) of
	true ->
	    init = ?config(suite,Config),
	    [{TestCase,TestCase} | Config];
	false ->
	    ct:fail({unknown_testcase,TestCase})
    end.

end_per_testcase(TestCase, Config) ->
    {_,TCs} = grs_and_tcs(),
    case lists:member(TestCase, TCs) of
	true ->
	    init = ?config(suite,Config),
	    TestCase = ?config(TestCase,Config),
	    ok;
	false ->
	    ct:fail({unknown_testcase,TestCase})
    end.


%%--------------------------------------------------------------------
%% Testcases
%%--------------------------------------------------------------------

testcase_1() ->
    [].
testcase_1(Config) ->
    init = ?config(suite,Config),
    testcase_1 = ?config(testcase_1,Config),
    ok.

testcase_1a() ->
    [].
testcase_1a(Config) ->
    init = ?config(suite,Config),
    case ?config(test_group_1a,Config) of
	test_group_1a -> ok;
	_ ->
	    case ?config(test_group_1b,Config) of
		test_group_1b -> ok;
		_ -> ct:fail(no_group_data)
	    end
    end,
    testcase_1a = ?config(testcase_1a,Config),
    ok.
testcase_1b() ->
    [].
testcase_1b(Config) ->
    init = ?config(suite,Config),
    case ?config(test_group_1a,Config) of
	test_group_1a -> ok;
	_ ->
	    case ?config(test_group_1b,Config) of
		test_group_1b -> ok;
		_ -> ct:fail(no_group_data)
	    end
    end,
    undefined = ?config(testcase_1a,Config),
    testcase_1b = ?config(testcase_1b,Config),
    ok.

testcase_2() ->
    [].
testcase_2(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(test_group_1a,Config),
    undefined = ?config(test_group_1b,Config),
    testcase_2 = ?config(testcase_2,Config),
    ok.

testcase_2a() ->
    [].
testcase_2a(Config) ->
    init = ?config(suite,Config),
    test_group_2 = ?config(test_group_2,Config),
    testcase_2a = ?config(testcase_2a,Config),
    ok.
testcase_2b() ->
    [].
testcase_2b(Config) ->
    init = ?config(suite,Config),
    test_group_2 = ?config(test_group_2,Config),
    undefined = ?config(testcase_2a,Config),
    testcase_2b = ?config(testcase_2b,Config),
    ok.

testcase_3a() ->
    [].
testcase_3a(Config) ->
    init = ?config(suite,Config),
    test_group_2 = ?config(test_group_2,Config),
    test_group_3 = ?config(test_group_3,Config),
    undefined = ?config(testcase_2b,Config),
    testcase_3a = ?config(testcase_3a,Config),
    ok.
testcase_3b() ->
    [].
testcase_3b(Config) ->
    init = ?config(suite,Config),
    test_group_2 = ?config(test_group_2,Config),
    test_group_3 = ?config(test_group_3,Config),
    undefined = ?config(testcase_3a,Config),
    testcase_3b = ?config(testcase_3b,Config),
    ok.

testcase_3() ->
    [].
testcase_3(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(test_group_2,Config),
    undefined = ?config(test_group_3,Config),
    testcase_3 = ?config(testcase_3,Config),
    ok.

testcase_5a() ->
    [].
testcase_5a(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(test_group_3,Config),
    test_group_4 = ?config(test_group_4,Config),
    test_group_5 = ?config(test_group_5,Config),
    undefined = ?config(testcase_3,Config),
    testcase_5a = ?config(testcase_5a,Config),
    ok.
testcase_5b() ->
    [].
testcase_5b(Config) ->
    init = ?config(suite,Config),
    test_group_4 = ?config(test_group_4,Config),
    test_group_5 = ?config(test_group_5,Config),
    undefined = ?config(testcase_5a,Config),
    testcase_5b = ?config(testcase_5b,Config),
    ok.

testcase_7a() ->
    [].
testcase_7a(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(test_group_3,Config),
    test_group_4 = ?config(test_group_4,Config),
    test_group_5 = ?config(test_group_5,Config),
    test_group_6 = ?config(test_group_6,Config),
    test_group_7 = ?config(test_group_7,Config),
    testcase_7a = ?config(testcase_7a,Config),
    ok.
testcase_7b() ->
    [].
testcase_7b(Config) ->
    init = ?config(suite,Config),
    test_group_4 = ?config(test_group_4,Config),
    test_group_5 = ?config(test_group_5,Config),
    test_group_6 = ?config(test_group_6,Config),
    test_group_7 = ?config(test_group_7,Config),
    undefined = ?config(testcase_7a,Config),
    testcase_7b = ?config(testcase_7b,Config),
    ok.
