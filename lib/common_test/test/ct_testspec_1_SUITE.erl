%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").
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

all(doc) ->
    ["Run smoke tests of Common Test."];

all(suite) ->
    [all_suites, skip_all_suites,
     suite, skip_suite,
     all_testcases, skip_all_testcases,
     testcase, skip_testcase,
     all_groups, skip_all_groups,
     group, skip_group,
     group_all_testcases, skip_group_all_testcases,
     group_testcase, skip_group_testcase,
     topgroup,
     subgroup, skip_subgroup,
     subgroup_all_testcases, skip_subgroup_all_testcases,
     subgroup_testcase, skip_subgroup_testcase,
     sub_skipped_by_top,
     testcase_in_multiple_groups].

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

testcase_in_multiple_groups(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "groups_1"),
    TestSpec = [{cases,TestDir,groups_12_SUITE,[testcase_1a,testcase_1b]},
		{skip_cases,TestDir,groups_12_SUITE,[testcase_1b],"SKIPPED!"}],

    setup_and_execute(testcase_in_multiple_groups, TestSpec, Config).

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
    ok = ct_test_support:run(Opts, Config),
    TestSpec1 = [{logdir,proplists:get_value(logdir,Opts)},
		 {label,proplists:get_value(label,TestTerms)} | TestSpec],
    ok = ct_test_support:run(ct, run_testspec, [TestSpec1], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(TCName,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(TCName),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

create_spec_file(SpecDir, TCName, TestSpec) ->
    FileName = filename:join(SpecDir,
			     atom_to_list(TCName)++".spec"),
    {ok,Dev} = file:open(FileName, [write]),
    [io:format(Dev, "~p.~n", [Term]) || Term <- TestSpec],
    file:close(Dev),
    io:format("~nTest spec created here~n~n<a href=\"file://~s\">~s</a>~n",
	      [FileName,FileName]),
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

test_events(_) ->
    [
    ].
