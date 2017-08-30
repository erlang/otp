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
%%% File: ct_groups_test_2_SUITE
%%%
%%% Description: 
%%% Test some simple test case group scenarios.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_groups_test_2_SUITE).

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
    [missing_conf, repeat_1, empty_group].

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

missing_conf(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Suite = filename:join(DataDir, "groups_1/missing_conf_SUITE"),

    {Opts,ERPid} = setup([{suite,Suite}], Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(missing_conf_SUITE, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(missing_conf),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%%

repeat_1(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    Suite = filename:join(DataDir, "groups_1/repeat_1_SUITE"),

    {Opts,ERPid} = setup([{suite,Suite}], Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(repeat_1,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(repeat_1),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%%

empty_group(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    Suite = filename:join(DataDir, "groups_2/groups_22_SUITE"),

    {Opts,ERPid} = setup([{suite,Suite},
			  {group,[test_group_8,test_group_9,test_group_10]}],
			 Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(empty_group,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(empty_group),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}} | Test],
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

test_events(missing_conf) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,2}},
     {?eh,tc_start,{ct_framework,{init_per_group,group1,
				  [{suite,missing_conf_SUITE}]}}},
     {?eh,tc_done,{ct_framework,{init_per_group,group1,
				 [{suite,missing_conf_SUITE}]},ok}},
     {?eh,tc_start,{missing_conf_SUITE,tc1}},
     {?eh,tc_done,{missing_conf_SUITE,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{missing_conf_SUITE,tc2}},
     {?eh,tc_done,{missing_conf_SUITE,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_start,{ct_framework,{end_per_group,group1,
				  [{suite,missing_conf_SUITE}]}}},
     {?eh,tc_done,{ct_framework,{end_per_group,group1,
				 [{suite,missing_conf_SUITE}]},ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(repeat_1) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,unknown}},
     {?eh,tc_start,{repeat_1_SUITE,init_per_suite}},
     {?eh,tc_done,{repeat_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,
       {repeat_1_SUITE,{init_per_group,test_group_1,[{repeat,2}]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,{init_per_group,test_group_1,[{repeat,2}]},ok}},
      {?eh,tc_start,{repeat_1_SUITE,testcase_1a}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{1,0,{0,0}}},
      {?eh,tc_start,{repeat_1_SUITE,testcase_1b}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{2,0,{0,0}}},
      {?eh,tc_start,
       {repeat_1_SUITE,{end_per_group,test_group_1,[{repeat,2}]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,{end_per_group,test_group_1,[{repeat,2}]},ok}}],
     [{?eh,tc_start,
       {repeat_1_SUITE,{init_per_group,test_group_1,[]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,{init_per_group,test_group_1,[]},ok}},
      {?eh,tc_start,{repeat_1_SUITE,testcase_1a}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
      {?eh,tc_start,{repeat_1_SUITE,testcase_1b}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,
       {repeat_1_SUITE,{end_per_group,test_group_1,[]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,{end_per_group,test_group_1,[]},ok}}],
     [{?eh,tc_start,
       {repeat_1_SUITE,{init_per_group,test_group_2,[]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,{init_per_group,test_group_2,[]},ok}},
      {?eh,tc_start,{repeat_1_SUITE,testcase_2a}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_2a,ok}},
      {?eh,test_stats,{5,0,{0,0}}},
      {?eh,tc_start,{repeat_1_SUITE,testcase_2b}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_2b,ok}},
      {?eh,test_stats,{6,0,{0,0}}},
      {?eh,tc_start,
       {repeat_1_SUITE,{end_per_group,test_group_2,[]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,{end_per_group,test_group_2,[]},ok}}],
     [{?eh,tc_start,
       {repeat_1_SUITE,
	{init_per_group,test_group_3,[]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,
	{init_per_group,test_group_3,[]},
	ok}},
      {?eh,tc_start,{repeat_1_SUITE,testcase_3a}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_3a,ok}},
      {?eh,test_stats,{7,0,{0,0}}},
      [{?eh,tc_start,
        {repeat_1_SUITE,
	 {init_per_group,test_group_4,[]}}},
       {?eh,tc_done,
        {repeat_1_SUITE,
	 {init_per_group,test_group_4,[]},
	 ok}},
       {?eh,tc_start,{repeat_1_SUITE,testcase_4a}},
       {?eh,tc_done,{repeat_1_SUITE,testcase_4a,ok}},
       {?eh,test_stats,{8,0,{0,0}}},
       {?eh,tc_start,{repeat_1_SUITE,testcase_4b}},
       {?eh,tc_done,{repeat_1_SUITE,testcase_4b,ok}},
       {?eh,test_stats,{9,0,{0,0}}},
       {?eh,tc_start,
        {repeat_1_SUITE,
	 {end_per_group,test_group_4,[]}}},
       {?eh,tc_done,
        {repeat_1_SUITE,
	 {end_per_group,test_group_4,[]},
	 ok}}],
      {?eh,tc_start,{repeat_1_SUITE,testcase_3b}},
      {?eh,tc_done,{repeat_1_SUITE,testcase_3b,ok}},
      {?eh,test_stats,{10,0,{0,0}}},
      {?eh,tc_start,
       {repeat_1_SUITE,
	{end_per_group,test_group_3,[]}}},
      {?eh,tc_done,
       {repeat_1_SUITE,
	{end_per_group,test_group_3,[]},
	ok}}],
     {?eh,tc_start,{repeat_1_SUITE,end_per_suite}},
     {?eh,tc_done,{repeat_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(empty_group) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{groups_22_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,
       {groups_22_SUITE,{init_per_group,test_group_8,[]}}},
      {?eh,tc_done,
       {groups_22_SUITE,{init_per_group,test_group_8,[]},ok}},
      {?eh,tc_start,{groups_22_SUITE,testcase_8}},
      {?eh,tc_done,{groups_22_SUITE,testcase_8,ok}},
      {?eh,test_stats,{1,0,{0,0}}},
      {?eh,tc_start,
       {groups_22_SUITE,{end_per_group,test_group_8,[]}}},
      {?eh,tc_done,
       {groups_22_SUITE,{end_per_group,test_group_8,[]},ok}}],
     {?eh,tc_start,{groups_22_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
