%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%%% File: ct_html_validation_SUITE
%%%
%%% Description:
%%% Test HTML validation
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_html_validation_SUITE).

-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         tests1_spec/1, tests2_spec/1,
         tests1_multi_specs/1, tests2_multi_specs/1,
         tests1_log_cache/1, tests2_log_cache/1,
         check_parameter/1, read_config/1]).

-include("ct_html_validation_SUITE.hrl").
-include("ct_results_parser.hrl").
-include_lib("common_test/include/ct.hrl").

-record(test_run, {tc_name,
                   specs,
                   test_opts = [],
                   expected = []
                  }).

-record(expected, {test_name,
                   label,
                   suites,
                   group_paths,
                   names,
                   input
                  }).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    DataDir = ?config(data_dir, Config0),
    Config = ct_test_support:init_per_suite(Config0),
    SpecsDir = filename:join(DataDir, "specs"),
    [{specs_dir, SpecsDir} | Config].

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config0) ->
    PrivDir0 = ?config(priv_dir, Config0),
    PrivDir = filename:join(PrivDir0, TestCase),
    ok = file:make_dir(PrivDir),
    Config = set_priv_dir(PrivDir, Config0),
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> [tests1_spec,
          tests2_spec,
          tests1_multi_specs,
          tests2_multi_specs,
          tests1_log_cache,
          tests2_log_cache].

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

tests1_spec(Config) ->
    Expected = [#expected{test_name = get_test_name(Config, ".tests1"),
                          label = ?FUNCTION_NAME,
                          suites = ["t1_SUITE", "t2_SUITE", "t3_SUITE", "t4_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES}],
    TestRun = #test_run{tc_name = ?FUNCTION_NAME,
                        specs = [?FUNCTION_NAME],
                        expected = Expected},
    run_test(TestRun, Config).

tests2_spec(Config) ->
    Expected = [#expected{test_name = get_test_name(Config, ".tests2"),
                          label = ?FUNCTION_NAME,
                          suites = ["t5_SUITE", "t6_SUITE", "t7_SUITE", "t8_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES}],
    TestRun = #test_run{tc_name = ?FUNCTION_NAME,
                        specs = [?FUNCTION_NAME],
                        expected = Expected},
    run_test(TestRun, Config).

tests1_multi_specs(Config) ->
    Expected = [#expected{test_name = get_test_name(Config, ".tests1.t1_SUITE"),
                          label = ?FUNCTION_NAME,
                          suites = ["t1_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE.[test1a, test1b]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t4_SUITE"],
                          group_paths = [],
                          names = [test1a, test1b],
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE.group2"),
                          label = ?FUNCTION_NAME,
                          suites = ["t4_SUITE"],
                          group_paths = [["group2", "subgroup2a"], ["group2", "subgroup2b"]],
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests1.t3_SUITE"),
                          label = ?FUNCTION_NAME,
                          suites = ["t3_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE"),
                          label = ?FUNCTION_NAME,
                          suites = ["t4_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE.group1.subgroup1a"),
                          label = ?FUNCTION_NAME,
                          suites = ["t4_SUITE"],
                          group_paths = [["group1", "subgroup1a"]],
                          names = all,
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS},
                #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE.group2.[test2c, test2d]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t4_SUITE"],
                          group_paths = [["group2", "subgroup2b"]],
                          names = [test2c, test2d],
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS},
                #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE.[group1.subgroup1a, group1.subgroup1b]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t4_SUITE"],
                          group_paths = [["group1", "subgroup1a"], ["group1", "subgroup1b"]],
                          names = all,
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS},
                #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE.[group1.subgroup1a.[test1a, test1c], group1.subgroup1b.[test1a, test1c]]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t4_SUITE"],
                          group_paths = [["group1", "subgroup1a"], ["group1", "subgroup1b"]],
                          names = [test1a, test1c],
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS}],
    TestRun = #test_run{tc_name = ?FUNCTION_NAME,
                        specs = [tests1_single_suite_spec,
                                 tests1_cases_spec,
                                 tests1_group_spec,
                                 tests1_multi_suites_spec,
                                 tests1_group_path_spec,
                                 tests1_group_cases_spec,
                                 tests1_multi_group_paths_spec,
                                 tests1_multi_group_paths_cases_spec],
                        expected = Expected},
    run_test(TestRun, Config).

tests2_multi_specs(Config) ->
    Expected = [#expected{test_name = get_test_name(Config, ".tests2.t6_SUITE"),
                          label = ?FUNCTION_NAME,
                          suites = ["t6_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE.[test1a, test1b]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t8_SUITE"],
                          group_paths = [],
                          names = [test1a, test1b],
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE.group2"),
                          label = ?FUNCTION_NAME,
                          suites = ["t8_SUITE"],
                          group_paths = [["group2", "subgroup2a"], ["group2", "subgroup2b"]],
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests2.t7_SUITE"),
                          label = ?FUNCTION_NAME,
                          suites = ["t7_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE"),
                          label = ?FUNCTION_NAME,
                          suites = ["t8_SUITE"],
                          group_paths = all,
                          names = all,
                          input = ?ALL_TEST_CASES},
                #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE.group1.subgroup1a"),
                          label = ?FUNCTION_NAME,
                          suites = ["t8_SUITE"],
                          group_paths = [["group1", "subgroup1a"]],
                          names = all,
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS},
                #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE.group2.[test2c, test2d]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t8_SUITE"],
                          group_paths = [["group2", "subgroup2b"]],
                          names = [test2c, test2d],
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS},
                #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE.[group1.subgroup1a, group1.subgroup1b]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t8_SUITE"],
                          group_paths = [["group1", "subgroup1a"], ["group1", "subgroup1b"]],
                          names = all,
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS},
                #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE.[group1.subgroup1a.[test1a, test1c], group1.subgroup1b.[test1a, test1c]]"),
                          label = ?FUNCTION_NAME,
                          suites = ["t8_SUITE"],
                          group_paths = [["group1", "subgroup1a"], ["group1", "subgroup1b"]],
                          names = [test1a, test1c],
                          input = ?ALL_TEST_CASES_SPLIT_GROUPS}],
    TestRun = #test_run{tc_name = ?FUNCTION_NAME,
                        specs = [tests2_single_suite_spec,
                                 tests2_cases_spec,
                                 tests2_group_spec,
                                 tests2_multi_suites_spec,
                                 tests2_group_path_spec,
                                 tests2_group_cases_spec,
                                 tests2_multi_group_paths_spec,
                                 tests2_multi_group_paths_cases_spec],
                        expected = Expected},
    run_test(TestRun, Config).

tests1_log_cache(Config) ->
    Expected1 = [#expected{test_name = get_test_name(Config, ".tests1"),
                           label = ?FUNCTION_NAME,
                           suites = ["t1_SUITE", "t2_SUITE", "t3_SUITE", "t4_SUITE"],
                           group_paths = all,
                           names = all,
                           input = ?ALL_TEST_CASES},
                 #expected{test_name = get_test_name(Config, ".tests1.t4_SUITE.group2"),
                           label = ?FUNCTION_NAME,
                           suites = ["t4_SUITE"],
                           group_paths = [["group2", "subgroup2a"], ["group2", "subgroup2b"]],
                           names = all,
                           input = ?ALL_TEST_CASES}],
    TestRun1 = #test_run{tc_name = ?FUNCTION_NAME,
                         specs = [tests1_spec, tests1_group_spec],
                         expected = Expected1},
    run_test(TestRun1, Config),
    Expected2 = Expected1 ++
        [#expected{test_name = get_test_name(Config, ".tests1.t1_SUITE"),
                   label = ?FUNCTION_NAME,
                   suites = ["t1_SUITE"],
                   group_paths = all,
                   names = all,
                   input = ?ALL_TEST_CASES}],
    TestRun2 = #test_run{tc_name = ?FUNCTION_NAME,
                         specs = [tests1_single_suite_spec],
                         expected = Expected2},
    run_test(TestRun2, Config).

tests2_log_cache(Config) ->
    Expected1 = [#expected{test_name = get_test_name(Config, ".tests2"),
                           label = ?FUNCTION_NAME,
                           suites = ["t5_SUITE", "t6_SUITE", "t7_SUITE", "t8_SUITE"],
                           group_paths = all,
                           names = all,
                           input = ?ALL_TEST_CASES},
                 #expected{test_name = get_test_name(Config, ".tests2.t8_SUITE.group2"),
                           label = ?FUNCTION_NAME,
                           suites = ["t8_SUITE"],
                           group_paths = [["group2", "subgroup2a"], ["group2", "subgroup2b"]],
                           names = all,
                           input = ?ALL_TEST_CASES}],
    TestRun1 = #test_run{tc_name = ?FUNCTION_NAME,
                         specs = [tests2_spec, tests2_group_spec],
                         expected = Expected1},
    run_test(TestRun1, Config),
    Expected2 = Expected1 ++
        [#expected{test_name = get_test_name(Config, ".tests2.t6_SUITE"),
                   label = ?FUNCTION_NAME,
                   suites = ["t6_SUITE"],
                   group_paths = all,
                   names = all,
                   input = ?ALL_TEST_CASES}],
    TestRun2 = #test_run{tc_name = ?FUNCTION_NAME,
                         specs = [tests2_single_suite_spec],
                         expected = Expected2},
    run_test(TestRun2, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

check_parameter(TCID) ->
    {ok, {config, TCID}}.

%%%-----------------------------------------------------------------

read_config(TCID) ->
    {ok, [{tcname, list_to_atom(TCID)}]}.

%%%-----------------------------------------------------------------

fname(Tag, File, Config) ->
    filename:join(?config(Tag, Config), File).

%%%-----------------------------------------------------------------

run_test(#test_run{tc_name = TCName,
                   specs = Specs0,
                   test_opts = TestOpts,
                   expected = Expected}, Config) ->

    ExpectedTestsAndCases = expected_tests(Expected, Config),
    OldRun = convert_to_old_run(ExpectedTestsAndCases),

    Specs = [fname(specs_dir, Spec, Config) || Spec <- Specs0],

    setup_and_execute(TCName, Specs, TestOpts, Config),
    validate_html_files(Config, ExpectedTestsAndCases, [OldRun]).

%%%-----------------------------------------------------------------

setup_and_execute(TCName, Specs, TestOpts, Config) ->
    TestID = {userconfig, {?MODULE, atom_to_list(TCName)}},
    TestTerms = [TestID, {spec, Specs}, {label, TCName}] ++ TestOpts,
    Opts0 = ct_test_support:get_opts(Config),
    Opts = ct_test_support:get_overwritten_opts(Opts0 ++ TestTerms),
    ct_test_support:run_ct_run_test(Opts, Config).

%%%-----------------------------------------------------------------

set_priv_dir(PrivDir, Config0) ->
    Config = proplists:delete(priv_dir, Config0),
    [{priv_dir, PrivDir} | Config].

%%%-----------------------------------------------------------------

validate_html_files(Config, ExpectedTestsAndCases, ExpectedOldRuns) ->
    validate_index_html_file(Config, ExpectedTestsAndCases, ExpectedOldRuns).

%%%-----------------------------------------------------------------

validate_index_html_file(Config, ExpectedTestsAndCases, ExpectedOldRuns) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, "index.html"),
    Tests = ct_results_parser:parse_index_html_file(Path),
    ok = ct_results_validator:validate_total(Tests),
    SortedTests = lists:sort(fun compare_tests/2, Tests),
    Expected = [E || {E, _} <- maps:values(ExpectedTestsAndCases)],
    SortedExpected = lists:sort(fun compare_tests/2, Expected),
    ok = ct_results_validator:validate_tests(SortedExpected, SortedTests, false),
    F = fun(#total{}) -> ok;
           (#test{test_name = TestName, suite_log_link = SuiteLogLink,
                  ct_log_link = CtLogLink, old_runs_link = OldRunsLink}) ->
                {ExpectedTest, ExpectedCases} = maps:get(TestName, ExpectedTestsAndCases),
                validate_suite_log_file(Config, SuiteLogLink, ExpectedTest, ExpectedCases),
                IndexLink = string:replace(CtLogLink, "ctlog.html", "index.html"),
                validate_specific_index_html_file(Config, IndexLink, [ExpectedTest]),
                validate_old_runs_file(Config, OldRunsLink, ExpectedOldRuns)
        end,
    lists:foreach(F, SortedTests).

%%%-----------------------------------------------------------------

validate_suite_log_file(Config, Link, #test{test_name = TestName}, ExpectedCases) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, Link),
    [#suite_test_name{test_name = TestName} | Cases] = ct_results_parser:parse_suite_log_file(Path),
    ok = ct_results_validator:validate_test_cases_total(Cases),
    ok = ct_results_validator:validate_test_cases(ExpectedCases, Cases).

%%%-----------------------------------------------------------------

validate_specific_index_html_file(Config, Link, [#test{test_name = TestName}] = ExpectedTests) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, Link),
    Tests = ct_results_parser:parse_index_html_file(Path),
    ok = ct_results_validator:validate_total(Tests),
    F = fun(#total{}) -> true;
           (#test{test_name = Name}) -> TestName =:= Name end,
    ok = ct_results_validator:validate_tests(ExpectedTests, lists:filter(F, Tests), true).

%%%-----------------------------------------------------------------

validate_old_runs_file(_Config, undefined, _ExpectedOldRuns) ->
    ok;
validate_old_runs_file(Config, Link, ExpectedOldRuns) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, Link),
    OldRuns = ct_results_parser:parse_old_runs_file(Path),
    ok = ct_results_validator:validate_old_runs(ExpectedOldRuns, OldRuns).

%%%-----------------------------------------------------------------

default_test(TestName, Label, Config) ->
    #test{test_name = TestName,
          label = Label,
          start_date = calendar:local_time(),
          node = ?config(ct_node, Config),
          missing_suites = 0}.

%%%-----------------------------------------------------------------

expected_tests(Expected, Config) ->
    expected_tests(Expected, Config, maps:new()).

%%%-----------------------------------------------------------------

expected_tests([], _Config, Acc) ->
    Acc;
expected_tests([#expected{test_name = TestName,
                          label = Label,
                          suites = Suites,
                          group_paths = GroupPaths,
                          names = Names,
                          input = Input} | Expected], Config, Acc) ->
    ExpectedTestCases = expected_test_cases(Input, Suites, GroupPaths, Names),
    ExpectedNumbers = expected_numbers(ExpectedTestCases),
    ExpectedTest0 = default_test(TestName, Label, Config),
    ExpectedTest = fill_numbers(ExpectedTest0, ExpectedNumbers),
    expected_tests(Expected, Config, maps:put(TestName, {ExpectedTest, ExpectedTestCases}, Acc)).

%%%-----------------------------------------------------------------

expected_test_cases(Input, ExpectedSuites, ExpectedGroupPaths0, ExpectedNames)
  when is_list(ExpectedGroupPaths0) ->
    ExpectedGroupPaths = [lists:reverse(GroupPath) || GroupPath <- ExpectedGroupPaths0],
    expected_test_cases(Input, [], ExpectedSuites, ExpectedGroupPaths,
                        ExpectedNames, 1, false, []);
expected_test_cases(Input, ExpectedSuites, ExpectedGroupPaths, ExpectedNames) ->
    expected_test_cases(Input, [], ExpectedSuites, ExpectedGroupPaths,
                        ExpectedNames, 1, false, []).

%%%-----------------------------------------------------------------

expected_numbers(Cases) ->
    expected_numbers(Cases, {0, 0, 0, 0, 0}).

%%%-----------------------------------------------------------------

convert_to_old_run(Tests) when is_map(Tests) ->
    convert_to_old_run([T || {T, _} <- maps:values(Tests)]);

convert_to_old_run(Tests) ->
    convert_to_old_run(Tests, #old_run{}).

%%%-----------------------------------------------------------------

expected_test_cases([], _GroupPath, _ExpectedSuites, _ExpectedGroupPaths,
                    _ExpectedNames, _Index, _InitFail, Acc) ->
    lists:reverse(Acc);
expected_test_cases([#test_case{module = Suite, tc = end_per_suite} = Case | Cases],
                    GroupPath, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, true, Acc0) ->
    case ExpectedSuites =:= all orelse lists:member(Suite, ExpectedSuites) of
        true ->
            Acc = [Case | Acc0],
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, false, Acc);
        false ->
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, false, Acc0)
    end;
expected_test_cases([#test_case{tc = Name, result = failed} = Case | Cases],
                    GroupPath, all, ExpectedGroupPaths, ExpectedNames, Index, _InitFail, Acc0)
  when Name =:= init_per_suite ->
    Acc = [Case | Acc0],
    expected_test_cases(Cases, GroupPath, all, ExpectedGroupPaths,
                        ExpectedNames, Index, true, Acc);
expected_test_cases([#test_case{tc = Name} = Case | Cases],
                    GroupPath, all, ExpectedGroupPaths, ExpectedNames, Index, InitFail, Acc0)
  when Name =:= init_per_suite; Name =:= end_per_suite ->
    Acc = [Case | Acc0],
    expected_test_cases(Cases, GroupPath, all, ExpectedGroupPaths,
                        ExpectedNames, Index, InitFail, Acc);
expected_test_cases([#test_case{module = Suite, tc = Name, result = failed} = Case | Cases],
                    GroupPath, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, _InitFail, Acc0)
  when Name =:= init_per_suite ->
    case lists:member(Suite, ExpectedSuites) of
        true ->
            Acc = [Case | Acc0],
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, true, Acc);
        false ->
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, true, Acc0)
    end;
expected_test_cases([#test_case{module = Suite, tc = Name} = Case | Cases],
                    GroupPath, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, InitFail, Acc0)
  when Name =:= init_per_suite; Name =:= end_per_suite ->
    case lists:member(Suite, ExpectedSuites) of
        true ->
            Acc = [Case | Acc0],
            expected_test_cases(Cases, GroupPath, ExpectedSuites,
                                ExpectedGroupPaths, ExpectedNames, Index, InitFail, Acc);
        false ->
            expected_test_cases(Cases, GroupPath, ExpectedSuites,
                                ExpectedGroupPaths, ExpectedNames, Index, InitFail, Acc0)
    end;
expected_test_cases([#test_case{module = Suite, tc = init_per_group, group = Group} = Case | Cases],
                    GroupPath0, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, InitFail, Acc0) ->
    GroupPath = [Group | GroupPath0],
    case (ExpectedSuites =:= all orelse lists:member(Suite, ExpectedSuites)) andalso
        (ExpectedGroupPaths =:= all orelse
         lists:any(fun(ExpectedGroupPath) -> lists:suffix(GroupPath, ExpectedGroupPath) end,
                   ExpectedGroupPaths)) of
        true ->
            Acc = [Case | Acc0],
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, InitFail, Acc);
        false ->
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, InitFail, Acc0)
    end;
expected_test_cases([#test_case{module = Suite, tc = end_per_group, group = Group} | Cases],
                    GroupPath0, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, InitFail,
                    [#test_case{module = Suite, tc = init_per_group, group = Group} | Acc]) ->
    %% Empty group, we have to skip it
    [Group | GroupPath] = GroupPath0,
    expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                        ExpectedNames, Index, InitFail, Acc);
expected_test_cases([#test_case{module = Suite, tc = end_per_group, group = Group} = Case | Cases],
                    GroupPath0, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, InitFail, Acc0) ->
    [Group | GroupPath] = GroupPath0,
    case (ExpectedSuites =:= all orelse lists:member(Suite, ExpectedSuites)) andalso
        (ExpectedGroupPaths =:= all orelse
         lists:any(fun(ExpectedGroupPath) -> lists:suffix(GroupPath0, ExpectedGroupPath) end,
                   ExpectedGroupPaths)) of
        true ->
            Acc = [Case | Acc0],
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, InitFail, Acc);
        false ->
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, InitFail, Acc0)
    end;
expected_test_cases([#test_case{module = Suite, tc = Name, group = undefined} = Case | Cases],
                    [], ExpectedSuites, ExpectedGroupPaths, ExpectedNames, Index, InitFail, Acc0)
  when ExpectedGroupPaths =:= all; ExpectedGroupPaths =:= [] ->
    case (ExpectedSuites =:= all orelse lists:member(Suite, ExpectedSuites)) andalso
        (ExpectedNames =:= all orelse lists:member(Name, ExpectedNames)) of
        true ->
            Acc = [Case#test_case{num = Index} | Acc0],
            expected_test_cases(Cases, [], ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index + 1, InitFail, Acc);
        false ->
            expected_test_cases(Cases, [], ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, InitFail, Acc0)
    end;
expected_test_cases([#test_case{group = undefined} | Cases],
                    [], ExpectedSuites, ExpectedGroupPaths, ExpectedNames, Index, InitFail, Acc)
  when is_list(ExpectedGroupPaths) ->
    expected_test_cases(Cases, [], ExpectedSuites, ExpectedGroupPaths,
                        ExpectedNames, Index, InitFail, Acc);
expected_test_cases([#test_case{module = Suite, tc = Name, group = Group} = Case | Cases],
                    [Group | _] = GroupPath, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, InitFail, Acc0) ->
    case (ExpectedSuites =:= all orelse lists:member(Suite, ExpectedSuites)) andalso
        (ExpectedGroupPaths =:= all orelse lists:member(GroupPath, ExpectedGroupPaths)) andalso
        (ExpectedNames =:= all orelse lists:member(Name, ExpectedNames)) of
        true ->
            Acc = [Case#test_case{num = Index} | Acc0],
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index + 1, InitFail, Acc);
        false ->
            expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, InitFail, Acc0)
    end;
expected_test_cases([#test_case{module = Suite, tc = Name, group = Group} = Case | Cases],
                    [], ExpectedSuites, ExpectedGroupPaths, ExpectedNames, Index, true, Acc0)
  when Group /= undefined ->
    %% If init_per_suite failed, common_test prints skipped test cases to html log, but it's
    %% init_per_group is not printed, is this a bug?
    case (ExpectedSuites =:= all orelse lists:member(Suite, ExpectedSuites)) andalso
        (ExpectedNames =:= all orelse lists:member(Name, ExpectedNames)) of
        true ->
            Acc = [Case#test_case{num = Index} | Acc0],
            expected_test_cases(Cases, [], ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index + 1, true, Acc);
        false ->
            expected_test_cases(Cases, [], ExpectedSuites, ExpectedGroupPaths,
                                ExpectedNames, Index, true, Acc0)
    end;
expected_test_cases([#test_case{group = Group} | Cases],
                    GroupPath, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, InitFail, Acc0)
  when Group /= undefined ->
    expected_test_cases(Cases, GroupPath, ExpectedSuites, ExpectedGroupPaths,
                        ExpectedNames, Index, InitFail, Acc0);
expected_test_cases([Case | _Cases], GroupPath, ExpectedSuites, ExpectedGroupPaths,
                    ExpectedNames, Index, InitFail, Acc0) ->
    ct:pal("expected_test_cases: UNMATCHED CLAUSE~n"
           "  Case: ~p~n"
           "  GroupPath: ~p~n"
           "  ExpectedSuites: ~p~n"
           "  ExpectedGroupPaths: ~p~n"
           "  ExpectedNames: ~p~n"
           "  Index: ~p~n"
           "  InitFail: ~p~n"
           "  Acc: ~p",
           [Case, GroupPath, ExpectedSuites, ExpectedGroupPaths,
            ExpectedNames, Index, InitFail, Acc0]),
    error({unmatched_expected_test_case, Case, GroupPath, ExpectedSuites,
           ExpectedGroupPaths, ExpectedNames, Index, InitFail}).

%%%-----------------------------------------------------------------

expected_numbers([], Acc) ->
    Acc;
expected_numbers([#test_case{tc = Name} | Cases], Numbers)
  when Name =:= init_per_suite; Name =:= init_per_group;
       Name =:= end_per_group; Name =:= end_per_suite ->
    expected_numbers(Cases, Numbers);
expected_numbers([#test_case{result = ok} | Cases],
                 {Ok, Failed, Skipped, UserSkipped, AutoSkipped}) ->
    expected_numbers(Cases, {Ok + 1, Failed, Skipped, UserSkipped, AutoSkipped});
expected_numbers([#test_case{result = failed} | Cases],
                 {Ok, Failed, Skipped, UserSkipped, AutoSkipped}) ->
    expected_numbers(Cases, {Ok, Failed + 1, Skipped, UserSkipped, AutoSkipped});
expected_numbers([#test_case{result = user_skipped} | Cases],
                 {Ok, Failed, Skipped, UserSkipped, AutoSkipped}) ->
    expected_numbers(Cases, {Ok, Failed, Skipped + 1, UserSkipped + 1, AutoSkipped});
expected_numbers([#test_case{result = auto_skipped} | Cases],
                 {Ok, Failed, Skipped, UserSkipped, AutoSkipped}) ->
    expected_numbers(Cases, {Ok, Failed, Skipped + 1, UserSkipped, AutoSkipped + 1}).

%%%-----------------------------------------------------------------

fill_numbers(#test{} = Test, {Ok, Failed, Skipped, UserSkipped, AutoSkipped}) ->
    Test#test{ok = Ok,
              failed = Failed,
              skipped = Skipped,
              user_skipped = UserSkipped,
              auto_skipped = AutoSkipped}.

%%%-----------------------------------------------------------------

convert_to_old_run([], OldRun) ->
    OldRun;
convert_to_old_run([#test{test_name = TestName,
                          label = Label,
                          start_date = StartDate,
                          ok = Ok,
                          failed = Failed,
                          skipped = Skipped,
                          user_skipped = UserSkipped,
                          auto_skipped = AutoSkipped,
                          missing_suites = MissingSuites,
                          node = Node} | Tests],
                   #old_run{tests = Tests0,
                            test_names = TestNames0,
                            total = Total0,
                            ok = Ok0,
                            failed = Failed0,
                            skipped = Skipped0,
                            user_skipped = UserSkipped0,
                            auto_skipped = AutoSkipped0,
                            missing_suites = MissingSuites0} = OldRun0) ->
    OldRun = OldRun0#old_run{start_date = StartDate,
                             node = Node,
                             label = Label,
                             tests = add_integer(Tests0, 1),
                             test_names = add_test_name(TestNames0, TestName),
                             total = add_integer(Total0, Ok + Failed + Skipped),
                             ok = add_integer(Ok0, Ok),
                             failed = add_integer(Failed0, Failed),
                             skipped = add_integer(Skipped0, Skipped),
                             user_skipped = add_integer(UserSkipped0, UserSkipped),
                             auto_skipped = add_integer(AutoSkipped0, AutoSkipped),
                             missing_suites = add_integer(MissingSuites0, MissingSuites)},
    convert_to_old_run(Tests, OldRun).

%%%-----------------------------------------------------------------

add_test_name(undefined, TestName) ->
    TestName;
add_test_name(TestNames, TestName) ->
    TestNames ++ ", " ++ TestName.

%%%-----------------------------------------------------------------

add_integer(undefined, I) ->
    I;
add_integer(I1, I2) ->
    I1 + I2.

%%%-----------------------------------------------------------------

get_test_name(Config, Suffix) ->
    DataDir = ?config(data_dir, Config),
    Parts = filename:split(DataDir),
    Result = lists:last(Parts) ++ Suffix,
    ct_util:get_display_name(Result).

%%%-----------------------------------------------------------------

compare_tests(#test{}, #total{}) ->
    true;
compare_tests(#total{}, #test{}) ->
    false;
compare_tests(#test{test_name = Name1}, #test{test_name = Name2}) ->
    Name1 =< Name2.
