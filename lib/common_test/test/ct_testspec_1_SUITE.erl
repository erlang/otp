%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_testspec_1_SUITE
%%%
%%% Description:
%%% Test test specifications
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_testspec_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config1 = ct_test_support:init_per_suite(Config),
    Config1.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [all_suites, skip_all_suites, suite, skip_suite,
     all_testcases, skip_all_testcases, testcase,
     skip_testcase, all_groups, skip_all_groups, group,
     skip_group, group_all_testcases,
     skip_group_all_testcases, group_testcase,
     skip_group_testcase, topgroup, subgroup, skip_subgroup,
     subgroup_all_testcases, skip_subgroup_all_testcases,
     subgroup_testcase, skip_subgroup_testcase,
     sub_skipped_by_top, testcase_many_groups,
     order_of_tests_many_dirs_no_merge_tests,
     order_of_tests_many_suites_no_merge_tests,
     order_of_suites_many_dirs_no_merge_tests,
     order_of_groups_many_dirs_no_merge_tests,
     order_of_groups_many_suites_no_merge_tests,
     order_of_tests_many_dirs,
     order_of_tests_many_suites,
     order_of_suites_many_dirs,
     order_of_groups_many_dirs,
     order_of_groups_many_suites,
     order_of_tests_many_suites_with_skip_no_merge_tests,
     order_of_tests_many_suites_with_skip,
     all_plus_one_tc_no_merge_tests,
     all_plus_one_tc].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%

all_suites(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{label,"all_suites"},
		{suites,TestDir,all}],

    setup_and_execute(all_suites, TestSpec, Config).

skip_all_suites(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{label,skip_all_suites},
		{suites,TestDir,all},
		{skip_suites,TestDir,all,"SKIPPED!"}],

    setup_and_execute(skip_all_suites, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

suite(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{label,undefined},
		{suites,TestDir,simple_1_SUITE}],

    setup_and_execute(suite, TestSpec, Config).

skip_suite(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{suites,TestDir,[simple_1_SUITE,simple_2_SUITE]},
		{skip_suites,TestDir,simple_1_SUITE,"SKIPPED!"}],

    setup_and_execute(skip_suite, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

all_testcases(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{cases,TestDir,simple_1_SUITE,all}],

    setup_and_execute(all_testcases, TestSpec, Config).

skip_all_testcases(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{suites,TestDir,[simple_1_SUITE]},
		{skip_cases,TestDir,simple_1_SUITE,all,"SKIPPED!"}],

    setup_and_execute(skip_all_testcases, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

testcase(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{cases,TestDir,simple_1_SUITE,tc1}],

    setup_and_execute(testcase, TestSpec, Config).

skip_testcase(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{cases,TestDir,simple_1_SUITE,[tc1,tc2]},
		{cases,TestDir,simple_2_SUITE,[tc2,tc1]},
		{skip_cases,TestDir,simple_1_SUITE,[tc1],"SKIPPED!"},
		{skip_cases,TestDir,simple_2_SUITE,tc2,"SKIPPED!"}],

    setup_and_execute(skip_testcase, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

all_groups(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,all}],

    setup_and_execute(all_groups, TestSpec, Config).

skip_all_groups(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,all},
		{skip_groups,TestDir,groups_11_SUITE,all,"SKIPPED!"}],

    setup_and_execute(skip_all_groups, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

group(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,test_group_1a}],

    setup_and_execute(group, TestSpec, Config).

skip_group(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,[test_group_1a,
						 test_group_1b]},
		{skip_groups,TestDir,groups_11_SUITE,
		 [test_group_1b,test_group_2,test_group_7],"SKIPPED!"}],

    setup_and_execute(skip_group, TestSpec, Config).


%%%-----------------------------------------------------------------
%%%

group_all_testcases(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,test_group_1a,{cases,all}}],

    setup_and_execute(group_all_testcases, TestSpec, Config).

skip_group_all_testcases(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,[test_group_1a,
						 test_group_1b]},
		{skip_groups,TestDir,groups_11_SUITE,
		 test_group_1b,{cases,all},"SKIPPED!"},
		{skip_groups,TestDir,groups_11_SUITE,
		 test_group_1a,{cases,all},"SKIPPED!"}],

    setup_and_execute(skip_group_all_testcases, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

group_testcase(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,test_group_1a,{cases,testcase_1a}}],

    setup_and_execute(group_testcase, TestSpec, Config).

skip_group_testcase(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_11_SUITE,test_group_1a,
		 {cases,[testcase_1a,testcase_1b]}},
		{groups,TestDir,groups_11_SUITE,test_group_1b,
		 {cases,[testcase_1b,testcase_1a]}},
		{skip_groups,TestDir,groups_11_SUITE,
		 test_group_1a,{cases,testcase_1b},"SKIPPED!"},
		{skip_groups,TestDir,groups_11_SUITE,
		 test_group_1b,{cases,[testcase_1a]},"SKIPPED!"}],

    setup_and_execute(skip_group_testcase, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

topgroup(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,test_group_2},
		{groups,TestDir,groups_12_SUITE,test_group_4}],

    setup_and_execute(topgroup, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

subgroup(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,test_group_3}],

    setup_and_execute(subgroup, TestSpec, Config).

skip_subgroup(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,[test_group_4]},
		{skip_groups,TestDir,groups_12_SUITE,
		 test_group_8,"SKIPPED!"}],

    setup_and_execute(skip_subgroup, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

subgroup_all_testcases(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,
		 test_group_5,{cases,all}},
		{groups,TestDir,groups_12_SUITE,
		 test_group_3,{cases,all}}],

    setup_and_execute(subgroup_all_testcases, TestSpec, Config).

skip_subgroup_all_testcases(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,test_group_4},
		{skip_groups,TestDir,groups_12_SUITE,
		 test_group_5,{cases,all},"SKIPPED!"}],

    setup_and_execute(skip_subgroup_all_testcases, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

subgroup_testcase(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,
		 test_group_7,{cases,testcase_7a}},
		{groups,TestDir,groups_12_SUITE,
		 test_group_3,{cases,testcase_3b}}],

    setup_and_execute(subgroup_testcase, TestSpec, Config).

skip_subgroup_testcase(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,test_group_5},
		{skip_groups,TestDir,groups_12_SUITE,
		 test_group_7,{cases,[testcase_7a,testcase_7b]},"SKIPPED!"}],

    setup_and_execute(skip_subgroup_testcase, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

%%!
%%! Somewhat weird result from this one:
%%!
sub_skipped_by_top(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir,groups_12_SUITE,test_group_5},
		{skip_groups,TestDir,groups_12_SUITE,test_group_4,"SKIPPED!"}],

    setup_and_execute(sub_skipped_by_top, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

testcase_many_groups(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{cases,TestDir,groups_12_SUITE,[testcase_1a,testcase_1b]},
		{skip_cases,TestDir,groups_12_SUITE,[testcase_1b],"SKIPPED!"}],

    setup_and_execute(testcase_many_groups, TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_tests_many_dirs_no_merge_tests(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestDir2 = filename:join(DataDir, "groups_2"),
    TestSpec = [{merge_tests, false},
		{cases,TestDir1,groups_12_SUITE,[testcase_1a]},
		{cases,TestDir2,groups_22_SUITE,[testcase_1]},
		{cases,TestDir1,groups_12_SUITE,[testcase_1b]}],

    setup_and_execute(order_of_tests_many_dirs_no_merge_tests, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_tests_many_suites_no_merge_tests(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{merge_tests, false},
		{cases,TestDir1,groups_12_SUITE,[testcase_1a]},
		{cases,TestDir1,groups_11_SUITE,[testcase_1]},
		{cases,TestDir1,groups_12_SUITE,[testcase_1b]}],

    setup_and_execute(order_of_tests_many_suites_no_merge_tests, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_suites_many_dirs_no_merge_tests(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestDir2 = filename:join(DataDir, "groups_2"),
    TestSpec = [{merge_tests, false},
		{suites,TestDir1,groups_12_SUITE},
		{suites,TestDir2,groups_22_SUITE},
		{suites,TestDir1,groups_11_SUITE}],

    setup_and_execute(order_of_suites_many_dirs_no_merge_tests, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_groups_many_dirs_no_merge_tests(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestDir2 = filename:join(DataDir, "groups_2"),
    TestSpec = [{merge_tests, false},
		{groups,TestDir1,groups_12_SUITE,test_group_1a},
		{groups,TestDir2,groups_22_SUITE,test_group_1a},
		{groups,TestDir1,groups_12_SUITE,test_group_1b}],

    setup_and_execute(order_of_groups_many_dirs_no_merge_tests, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_groups_many_suites_no_merge_tests(Config) 
  when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{merge_tests, false},
		{groups,TestDir1,groups_12_SUITE,test_group_1a},
		{groups,TestDir1,groups_11_SUITE,test_group_1a},
		{groups,TestDir1,groups_12_SUITE,test_group_1b}],

    setup_and_execute(order_of_groups_many_suites_no_merge_tests, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_tests_many_suites_with_skip_no_merge_tests(Config) 
  when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{merge_tests, false},
		{cases,TestDir1,groups_12_SUITE,[testcase_1a]},
		{cases,TestDir1,groups_11_SUITE,[testcase_1]},
		{cases,TestDir1,groups_12_SUITE,[testcase_1b]},
		{cases,TestDir1,groups_11_SUITE,[testcase_2]},
		{skip_cases,TestDir1,groups_12_SUITE,[testcase_1b],"Skip it"}],

    setup_and_execute(
      order_of_tests_many_suites_with_skip_no_merge_tests,
      TestSpec, Config).
     

%%%-----------------------------------------------------------------
%%%

order_of_tests_many_dirs(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestDir2 = filename:join(DataDir, "groups_2"),
    TestSpec = [{cases,TestDir1,groups_12_SUITE,[testcase_1a]},
		{cases,TestDir2,groups_22_SUITE,[testcase_1]},
		{cases,TestDir1,groups_12_SUITE,[testcase_1b]}],

    setup_and_execute(order_of_tests_many_dirs, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_tests_many_suites(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{cases,TestDir1,groups_12_SUITE,[testcase_1a]},
		{cases,TestDir1,groups_11_SUITE,[testcase_1]},
		{cases,TestDir1,groups_12_SUITE,[testcase_1b]}],

    setup_and_execute(order_of_tests_many_suites, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_suites_many_dirs(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestDir2 = filename:join(DataDir, "groups_2"),
    TestSpec = [{suites,TestDir1,groups_12_SUITE},
		{suites,TestDir2,groups_22_SUITE},
		{suites,TestDir1,groups_11_SUITE}],

    setup_and_execute(order_of_suites_many_dirs, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_groups_many_dirs(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestDir2 = filename:join(DataDir, "groups_2"),
    TestSpec = [{groups,TestDir1,groups_12_SUITE,test_group_1a},
		{groups,TestDir2,groups_22_SUITE,test_group_1a},
		{groups,TestDir1,groups_12_SUITE,test_group_1b}],

    setup_and_execute(order_of_groups_many_dirs, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_groups_many_suites(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{groups,TestDir1,groups_12_SUITE,test_group_1a},
		{groups,TestDir1,groups_11_SUITE,test_group_1a},
		{groups,TestDir1,groups_12_SUITE,test_group_1b}],

    setup_and_execute(order_of_groups_many_suites, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

order_of_tests_many_suites_with_skip(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{cases,TestDir1,groups_12_SUITE,[testcase_1a]},
		{cases,TestDir1,groups_11_SUITE,[testcase_1]},
		{cases,TestDir1,groups_12_SUITE,[testcase_1b]},
		{cases,TestDir1,groups_11_SUITE,[testcase_2]},
		{skip_cases,TestDir1,groups_12_SUITE,[testcase_1b],"Skip it!"}],

    setup_and_execute(order_of_tests_many_suites_with_skip, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

all_plus_one_tc_no_merge_tests(Config) when is_list(Config) ->
    
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{merge_tests,false},
		{suites,TestDir1,groups_12_SUITE},
		{cases,TestDir1,groups_12_SUITE,[testcase_1a]}],

    setup_and_execute(all_plus_one_tc_no_merge_tests, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%%

all_plus_one_tc(Config) when is_list(Config) ->
    
    DataDir = ?config(data_dir, Config),

    TestDir1 = filename:join(DataDir, "groups_1"),
    TestSpec = [{suites,TestDir1,groups_12_SUITE},
		{cases,TestDir1,groups_12_SUITE,[testcase_1a]}],

    setup_and_execute(all_plus_one_tc, 
		      TestSpec, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

setup_and_execute(TCName, TestSpec, Config) ->
    SpecFile = create_spec_file(?config(priv_dir, Config),
				TCName, TestSpec),
    TestTerms =
	case lists:keymember(label, 1, TestSpec) of
	    true -> [{spec,SpecFile}];
	    false -> [{spec,SpecFile},{label,TCName}]
	end,
    {Opts,ERPid} = setup(TestTerms, Config),

    FullSpecFile = ct_test_support:join_abs_dirs(?config(net_dir, Opts),
						 SpecFile),
    io:format("~nTest spec created here~n~n<a href=\"file://~s\">~s</a>~n",
	      [FullSpecFile,FullSpecFile]),

    ok = ct_test_support:run(Opts, Config),
    TestSpec1 = [{logdir,proplists:get_value(logdir,Opts)},
		 {label,proplists:get_value(label,TestTerms)} | TestSpec],
    {_Ok,_Failed,{_USkipped,_ASkipped}} = 
	ct_test_support:run(ct, run_testspec, [TestSpec1], Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(TCName,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(TCName),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

create_spec_file(SpecDir, TCName, TestSpec) ->
    FileName = filename:join(SpecDir,
			     atom_to_list(TCName)++".spec"),
    {ok,Dev} = file:open(FileName, [write]),
    [io:format(Dev, "~p.~n", [Term]) || Term <- TestSpec],
    file:close(Dev),
    FileName.

setup(Test, Config) when is_tuple(Test) ->
    setup([Test], Config);
setup(Tests, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ Tests ++ [{event_handler,{?eh,EvHArgs}}],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).
%reformat(Events, _EH) ->
%    Events.

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
events_to_check(Test) ->
    %% 2 tests (ct:run_test + script_start) is default
    events_to_check(Test, 2).

events_to_check(_, 0) ->
    [];
events_to_check(Test, N) ->
    test_events(Test) ++ events_to_check(Test, N-1).

test_events(all_suites) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{simple_1_SUITE,init_per_suite}},
     {?eh,tc_done,{simple_1_SUITE,end_per_suite,'_'}},
     {?eh,tc_start,{simple_2_SUITE,init_per_suite}},
     {?eh,test_stats,{4,0,{0,0}}},
     {?eh,tc_done,{simple_2_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(skip_all_suites) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_user_skip,{simple_1_SUITE,all,"SKIPPED!"}},
     {?eh,tc_user_skip,{simple_2_SUITE,all,"SKIPPED!"}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(suite) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{simple_1_SUITE,init_per_suite}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_done,{simple_1_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_suite) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_user_skip,{simple_1_SUITE,all,"SKIPPED!"}},
     {?eh,tc_done,{simple_2_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(all_testcases) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{simple_1_SUITE,init_per_suite}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_done,{simple_1_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_all_testcases) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_user_skip,{simple_1_SUITE,all,"SKIPPED!"}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(testcase) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{simple_1_SUITE,init_per_suite}},
     {?eh,test_stats,{1,0,{0,0}}},
     {negative,{?eh,test_stats,{2,0,{0,0}}},
      {?eh,tc_done,{simple_1_SUITE,end_per_suite,'_'}}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_testcase) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{simple_1_SUITE,init_per_suite}},
     {?eh,tc_user_skip,{simple_1_SUITE,tc1,"SKIPPED!"}},
     {?eh,tc_start,{simple_1_SUITE,tc2}},
     {?eh,tc_start,{simple_1_SUITE,end_per_suite}},

     {?eh,tc_start,{simple_2_SUITE,init_per_suite}},
     {?eh,tc_user_skip,{simple_2_SUITE,tc2,"SKIPPED!"}},
     {?eh,tc_start,{simple_2_SUITE,tc1}},
     {?eh,test_stats,{2,0,{2,0}}},
     {?eh,tc_start,{simple_2_SUITE,end_per_suite}},
     
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(all_groups) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,test_stats,{12,0,{0,0}}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_all_groups) ->
    [
     {?eh,start_logging,'_'},
     {?eh,start_info,{1,1,12}},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_1a},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1a,test_group_1a},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{1,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1b,test_group_1a},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{2,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_1a},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_1b},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1a,test_group_1b},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{3,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1b,test_group_1b},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{4,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_1b},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_2},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_2a,test_group_2},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{5,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_3a,test_group_3},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{6,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_3b,test_group_3},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{7,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_2b,test_group_2},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{8,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_2},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_4},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_5a,test_group_5},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{9,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_7a,test_group_7},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{10,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_7b,test_group_7},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{11,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_5b,test_group_5},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{12,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_4},"SKIPPED!"}},
     {?eh,tc_start,{groups_11_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,ok}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(group) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},'_'}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_group) ->
    [
      {?eh,start_logging,'_'},
      {?eh,start_info,{1,1,8}},
      {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
      [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1a,[]},ok}},
       {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
       {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
       {?eh,test_stats,{2,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1a,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},ok}}],
      {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_1b},
			 "SKIPPED!"}},
      {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1a,test_group_1b},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1b,test_group_1b},"SKIPPED!"}},
      {?eh,test_stats,{2,0,{2,0}}},
      {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_1b},
			 "SKIPPED!"}},
      {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_2},
			 "SKIPPED!"}},
      {?eh,tc_user_skip,{groups_11_SUITE,{testcase_2a,test_group_2},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_11_SUITE,{testcase_3a,test_group_3},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_11_SUITE,{testcase_3b,test_group_3},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_11_SUITE,{testcase_2b,test_group_2},"SKIPPED!"}},
      {?eh,test_stats,{2,0,{6,0}}},
      {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_2},
			 "SKIPPED!"}},
      {?eh,tc_done,{groups_11_SUITE,end_per_suite,ok}},
      {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
     ];

test_events(group_all_testcases) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},'_'}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_group_all_testcases) ->
    [
     {?eh,start_logging,'_'},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_1a},
			"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1a,test_group_1a},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1b,test_group_1a},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{2,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_1a},
			"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{init_per_group,test_group_1b},
			"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1a,test_group_1b},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1b,test_group_1b},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{4,0}}},
     {?eh,tc_user_skip,{groups_11_SUITE,{end_per_group,test_group_1b},
			"SKIPPED!"}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,ok}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(group_testcase) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
     {?eh,test_stats,{1,0,{0,0}}},
     {negative,{?eh,test_stats,{2,0,{0,0}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},'_'}}},

     {?eh,tc_done,{groups_11_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_group_testcase) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     
     {?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1b,test_group_1a},"SKIPPED!"}},
     {?eh,test_stats,{1,0,{1,0}}},
     {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},'_'}},
     
     {?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1b,[]}}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
     {?eh,tc_user_skip,{groups_11_SUITE,{testcase_1a,test_group_1b},"SKIPPED!"}},
     {?eh,test_stats,{2,0,{2,0}}},
     {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1b,[]},'_'}},

     {negative,{?eh,tc_user_skip,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(topgroup) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     
     {parallel, 
      [{?eh,tc_start,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}}
       ],
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[]}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[]}}}
       ],
       {?eh,test_stats,{6,0,{0,0}}},
       {?eh,tc_start,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},

     [{?eh,tc_start,
       {groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {parallel,
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
	{parallel,
	 [{?eh,tc_start,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
	  [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,'_'}}},
	   {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,'_'}}}],
	  {shuffle,
	   [{?eh,tc_start,
	     {groups_12_SUITE,{init_per_group,test_group_8,
			       [{shuffle,'_'},sequence]}}},
	    {?eh,tc_done,
	     {groups_12_SUITE,{init_per_group,test_group_8,
			       [{shuffle,'_'},sequence]},ok}},
	    {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_8,
					    [shuffle,sequence]}}},
	    {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_8,
					    [shuffle,sequence]},ok}}
	   ]},
	  {?eh,tc_start,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}
	 ]},
	{?eh,test_stats,{12,0,{0,0}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}}],

     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(subgroup) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     
     {parallel, 
      [{?eh,tc_start,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}}
       ],
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[]}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[]}}}
       ],
       {?eh,test_stats,{4,0,{0,0}}},
       {?eh,tc_start,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_subgroup) ->
    [
     {?eh,start_logging,'_'},
     {?eh,start_info,{1,1,6}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},

     [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_4,[]},ok}},

      {parallel,
       [{?eh,tc_start,{groups_12_SUITE,
		       {init_per_group,test_group_5,[parallel]}}},
       	{?eh,tc_done,{groups_12_SUITE,
       		      {init_per_group,test_group_5,[parallel]},ok}},

      	{parallel,
      	 [{?eh,tc_start,{groups_12_SUITE,
      			 {init_per_group,test_group_6,[parallel]}}},
      	  {?eh,tc_done,{groups_12_SUITE,
      			{init_per_group,test_group_6,[parallel]},ok}},

      	  [{?eh,tc_start,{groups_12_SUITE,
      			  {init_per_group,test_group_7,[sequence]}}},
      	   {?eh,tc_done,{groups_12_SUITE,
      			 {init_per_group,test_group_7,[sequence]},ok}},
           {?eh,tc_done,{groups_12_SUITE,testcase_7a,ok}},
           {?eh,tc_done,{groups_12_SUITE,testcase_7b,ok}},
      	   {?eh,tc_start,{groups_12_SUITE,
      			  {end_per_group,test_group_7,[sequence]}}},
      	   {?eh,tc_done,{groups_12_SUITE,
      			 {end_per_group,test_group_7,[sequence]},ok}}],

      	  {?eh,tc_user_skip,{groups_12_SUITE,
      			     {init_per_group,test_group_8},"SKIPPED!"}},
      	  {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8a,test_group_8},"SKIPPED!"}},
      	  {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8b,test_group_8},"SKIPPED!"}},
      	  {?eh,tc_user_skip,{groups_12_SUITE,
      			     {end_per_group,test_group_8},"SKIPPED!"}},

      	  {?eh,tc_start,{groups_12_SUITE,
      			 {end_per_group,test_group_6,[parallel]}}},
      	  {?eh,tc_done,{groups_12_SUITE,
      			{end_per_group,test_group_6,[parallel]},ok}}]},

       	{?eh,test_stats,{4,0,{2,0}}},
       	{?eh,tc_start,{groups_12_SUITE,
       		       {end_per_group,test_group_5,[parallel]}}},
       	{?eh,tc_done,{groups_12_SUITE,
       		      {end_per_group,test_group_5,[parallel]},ok}}]},

      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,ok}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(subgroup_all_testcases) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},

     [{?eh,tc_start,
       {groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {parallel,
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
	{parallel,
	 [{?eh,tc_start,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
	  [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,'_'}}},
	   {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,'_'}}}],
	  {shuffle,
	   [{?eh,tc_start,
	     {groups_12_SUITE,{init_per_group,test_group_8,
			       [{shuffle,'_'},sequence]}}},
	    {?eh,tc_done,
	     {groups_12_SUITE,{init_per_group,test_group_8,
			       [{shuffle,'_'},sequence]},ok}},
	    {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_8,
					    [shuffle,sequence]}}},
	    {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_8,
					    [shuffle,sequence]},ok}}
	   ]},
	  {?eh,tc_start,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}
	 ]},
	{?eh,test_stats,{6,0,{0,0}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}}],

     {parallel, 
      [{?eh,tc_start,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}}
       ],
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[]}}},
	{?eh,test_stats,{10,0,{0,0}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[]}}}
       ],
       {?eh,tc_start,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},

     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_subgroup_all_testcases) ->
    [
     {?eh,start_logging,'_'},
     {?eh,start_info,{1,1,6}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_4,[]},ok}},
      {?eh,tc_user_skip,{groups_12_SUITE,
			 {init_per_group,test_group_5},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_12_SUITE,{testcase_5a,test_group_5},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_12_SUITE,{testcase_7a,test_group_7},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_12_SUITE,{testcase_7b,test_group_7},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8a,test_group_8},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8b,test_group_8},"SKIPPED!"}},
      {?eh,tc_user_skip,{groups_12_SUITE,{testcase_5b,test_group_5},"SKIPPED!"}},
      {?eh,test_stats,{0,0,{6,0}}},
      {?eh,tc_user_skip,{groups_12_SUITE,
			 {end_per_group,test_group_5},"SKIPPED!"}},
      {?eh,tc_start,{groups_12_SUITE,
		     {end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,
		    {end_per_group,test_group_4,[]},ok}}],
     {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(subgroup_testcase) ->
    [
     {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},

     [{?eh,tc_start,
       {groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {parallel,
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
	{parallel,
	 [{?eh,tc_start,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
	  [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,'_'}}},
	   {?eh,test_stats,{1,0,{0,0}}},
	   {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,'_'}}}],
	  {?eh,tc_start,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}
	 ]},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}}],

     {parallel, 
      [{?eh,tc_start,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
	{?eh,test_stats,{2,0,{0,0}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}}
       ],
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_3,[]}}},
	{?eh,test_stats,{3,0,{0,0}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_3,[]}}}
       ],
       {?eh,tc_start,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},

     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];
 
test_events(skip_subgroup_testcase) ->
    [

    {?eh,start_logging,'_'},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},

     [{?eh,tc_start,
       {groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {parallel,
       [{?eh,tc_start,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
	{parallel,
	 [{?eh,tc_start,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
	  [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,'_'}}},
	   {?eh,tc_user_skip,{groups_12_SUITE,{testcase_7a,test_group_7},"SKIPPED!"}},
           {?eh,test_stats,{1,0,{1,0}}},
           {?eh,tc_user_skip, {groups_12_SUITE,{testcase_7b,test_group_7},"SKIPPED!"}},
           {?eh,test_stats,{1,0,{2,0}}},
	   {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,'_'}}}],
	  {shuffle,
	   [{?eh,tc_start,
	     {groups_12_SUITE,{init_per_group,test_group_8,
			       [{shuffle,'_'},sequence]}}},
	    {?eh,tc_done,
	     {groups_12_SUITE,{init_per_group,test_group_8,
			       [{shuffle,'_'},sequence]},ok}},
	    {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_8,
					    [shuffle,sequence]}}},
	    {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_8,
					    [shuffle,sequence]},ok}}
	   ]},
	  {?eh,tc_start,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}
	 ]},
	{?eh,test_stats,{4,0,{2,0}}},
	{?eh,tc_start,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
	 {groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}}],

     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
     
    ];

test_events(sub_skipped_by_top) ->
    [
     {?eh,start_logging,'_'},
     {?eh,start_info,{1,1,12}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_user_skip,{groups_12_SUITE,{init_per_group,test_group_4},
			"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_5a,test_group_5},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_7a,test_group_7},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_7b,test_group_7},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8a,test_group_8},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8b,test_group_8},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_5b,test_group_5},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,
			{end_per_group,test_group_4},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,
			{init_per_group,test_group_4},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_5a,test_group_5},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_7a,test_group_7},"SKIPPED!"}},     
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_7b,test_group_7},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8a,test_group_8},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_8b,test_group_8},"SKIPPED!"}},
     {?eh,tc_user_skip,{groups_12_SUITE,{testcase_5b,test_group_5},"SKIPPED!"}},
     {?eh,test_stats,{0,0,{12,0}}},
     {?eh,tc_user_skip,{groups_12_SUITE,
			{end_per_group,test_group_4},"SKIPPED!"}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,'_'},{?eh,stop_logging,'_'}}
    ];

test_events(testcase_many_groups) ->
    [];

test_events(order_of_tests_many_dirs_no_merge_tests) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
     {?eh,tc_done, {groups_12_SUITE,testcase_1a,
		    {failed,{error,{test_case_failed,no_group_data}}}}},
     {?eh,tc_start,{groups_22_SUITE,testcase_1}},
     {?eh,tc_done,{groups_22_SUITE,testcase_1,ok}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
     {?eh,tc_done, {groups_12_SUITE,testcase_1b,
		    {failed,{error,{test_case_failed,no_group_data}}}}},
     {?eh,stop_logging,[]}
    ];
test_events(order_of_tests_many_suites_no_merge_tests) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1a,'_'}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1b,'_'}},
     {?eh,stop_logging,[]}
    ];
test_events(order_of_suites_many_dirs_no_merge_tests) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,init_per_suite,'_'}},
     {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {?eh,tc_start,{groups_22_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,init_per_suite,'_'}},
     {?eh,tc_start,{groups_22_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,end_per_suite,'_'}},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,init_per_suite,'_'}},
     {?eh,tc_start,{groups_11_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,'_'}},
     {?eh,stop_logging,[]}];
test_events(order_of_groups_many_dirs_no_merge_tests) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     
     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1a,'_'},'_'}},

     {?eh,tc_start, {groups_22_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_22_SUITE,{end_per_group,test_group_1a,'_'},'_'}},
      
     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1b,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1b,'_'},'_'}},

     {?eh,stop_logging,[]}];
test_events(order_of_groups_many_suites_no_merge_tests) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     
     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1a,'_'},'_'}},

     {?eh,tc_start, {groups_11_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_11_SUITE,{end_per_group,test_group_1a,'_'},'_'}},
      
     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1b,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1b,'_'},'_'}},

     {?eh,stop_logging,[]}];
test_events(order_of_tests_many_suites_with_skip_no_merge_tests) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1a,'_'}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,tc_user_skip,{groups_12_SUITE,testcase_1b,'_'}},
     {?eh,tc_start,{groups_11_SUITE,testcase_2}},
     {?eh,tc_done,{groups_11_SUITE,testcase_2,ok}},
     {?eh,stop_logging,[]}
    ];

test_events(order_of_tests_many_dirs) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
     {?eh,tc_done,
      {groups_12_SUITE,testcase_1a,
       {failed,{error,{test_case_failed,no_group_data}}}}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
     {?eh,tc_done,
      {groups_12_SUITE,testcase_1b,
       {failed,{error,{test_case_failed,no_group_data}}}}},
     {?eh,tc_start,{groups_22_SUITE,testcase_1}},
     {?eh,tc_done,{groups_22_SUITE,testcase_1,ok}},
     {?eh,stop_logging,[]}
    ];
test_events(order_of_tests_many_suites) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1a,'_'}},

     {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1b,'_'}},

     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,stop_logging,[]}
    ];
test_events(order_of_suites_many_dirs) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,init_per_suite,'_'}},
     {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},

     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,init_per_suite,'_'}},
     {?eh,tc_start,{groups_11_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,'_'}},

     {?eh,tc_start,{groups_22_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,init_per_suite,'_'}},
     {?eh,tc_start,{groups_22_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,end_per_suite,'_'}},
     {?eh,stop_logging,[]}];
test_events(order_of_groups_many_dirs) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     
     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1a,'_'},'_'}},

     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1b,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1b,'_'},'_'}},

     {?eh,tc_start, {groups_22_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_22_SUITE,{end_per_group,test_group_1a,'_'},'_'}},

     {?eh,stop_logging,[]}];
test_events(order_of_groups_many_suites) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     
     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1a,'_'},'_'}},

     {?eh,tc_start, {groups_12_SUITE,{init_per_group,test_group_1b,'_'}}},
     {?eh,tc_done, {groups_12_SUITE,{end_per_group,test_group_1b,'_'},'_'}},

     {?eh,tc_start, {groups_11_SUITE,{init_per_group,test_group_1a,'_'}}},
     {?eh,tc_done, {groups_11_SUITE,{end_per_group,test_group_1a,'_'},'_'}},

     {?eh,stop_logging,[]}];

test_events(order_of_tests_many_suites_with_skip) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1a,'_'}},
     {?eh,tc_user_skip,{groups_12_SUITE,testcase_1b,'_'}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,tc_start,{groups_11_SUITE,testcase_2}},
     {?eh,tc_done,{groups_11_SUITE,testcase_2,ok}},
     {?eh,stop_logging,[]}
    ];

test_events(all_plus_one_tc_no_merge_tests) ->

    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {?eh,stop_logging,[]}
    ];

test_events(all_plus_one_tc) ->

    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,'_'}},
     {negative,{?eh,tc_start,{groups_12_SUITE,init_per_suite}},
      {?eh,stop_logging,[]}}
    ];

test_events(_) ->
    [
    ].
