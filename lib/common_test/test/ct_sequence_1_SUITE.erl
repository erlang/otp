%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2011. All Rights Reserved.
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
%%% File: ct_sequence_1_SUITE
%%%
%%% Description:
%%% Test sequences
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_sequence_1_SUITE).

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
    [subgroup_return_fail, subgroup_init_fail,
     subgroup_after_failed_case,
     case_after_subgroup_return_fail,
     case_after_subgroup_fail_init].

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

subgroup_return_fail(Config) when is_list(Config) ->
    execute(subgroup_return_fail,
	    "subgroups_1_SUITE", subgroup_return_fail,
	    Config).

%%%-----------------------------------------------------------------
%%%

subgroup_init_fail(Config) when is_list(Config) ->
    execute(subgroup_init_fail,
	    "subgroups_1_SUITE", subgroup_init_fail,
	    Config).

%%%-----------------------------------------------------------------
%%%

subgroup_after_failed_case(Config) when is_list(Config) ->
    execute(subgroup_after_failed_case,
	    "subgroups_1_SUITE", subgroup_after_failed_case,
	    Config).

%%%-----------------------------------------------------------------
%%%

case_after_subgroup_return_fail(Config) when is_list(Config) ->
    execute(case_after_subgroup_return_fail,
	    "subgroups_1_SUITE", case_after_subgroup_return_fail,
	    Config).

%%%-----------------------------------------------------------------
%%%

case_after_subgroup_fail_init(Config) when is_list(Config) ->
    execute(case_after_subgroup_fail_init,
	    "subgroups_1_SUITE", case_after_subgroup_fail_init,
	    Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

execute(TestCase, SuiteName, Group, Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, SuiteName),

    {Opts,ERPid} = setup([{suite,Suite},{group,Group},{label,TestCase}], Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(TestCase,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(TestCase),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

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

test_events(subgroup_return_fail) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,2}},
     [{?eh,tc_start,
       {subgroups_1_SUITE,{init_per_group,subgroup_return_fail,[sequence]}}},
      {?eh,tc_done,
       {subgroups_1_SUITE,{init_per_group,subgroup_return_fail,[sequence]},ok}},
      [{?eh,tc_start,
	{subgroups_1_SUITE,{init_per_group,return_fail,[]}}},
       {?eh,tc_done,
        {subgroups_1_SUITE,{init_per_group,return_fail,[]},ok}},
       {?eh,tc_start,{subgroups_1_SUITE,failing_tc}},
       {?eh,tc_done,
	{subgroups_1_SUITE,failing_tc,{failed,{error,{{badmatch,3},'_'}}}}},
       {?eh,test_stats,{0,1,{0,0}}},
       {?eh,tc_start,
	{subgroups_1_SUITE,{end_per_group,return_fail,[]}}},
       {?eh,tc_done,{subgroups_1_SUITE,{end_per_group,return_fail,[]},
		     {return_group_result,failed}}}],
      {?eh,tc_auto_skip,
       {subgroups_1_SUITE,ok_tc,{group_result,return_fail,failed}}},
      {?eh,test_stats,{0,1,{0,1}}},
      {?eh,tc_start,
       {subgroups_1_SUITE,{end_per_group,subgroup_return_fail,[sequence]}}},
      {?eh,tc_done,
       {subgroups_1_SUITE,{end_per_group,subgroup_return_fail,[sequence]},
	{return_group_result,failed}}}],
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(subgroup_init_fail) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,2}},
     [{?eh,tc_start,
       {subgroups_1_SUITE,{init_per_group,subgroup_init_fail,[sequence]}}},
      {?eh,tc_done,
       {subgroups_1_SUITE,{init_per_group,subgroup_init_fail,[sequence]},ok}},
      [{?eh,tc_start,{subgroups_1_SUITE,{init_per_group,fail_init,[]}}},
       {?eh,tc_done,{subgroups_1_SUITE,{init_per_group,fail_init,[]},
		     {failed,{error,init_per_group_fails_on_purpose}}}},
       {?eh,tc_auto_skip,{subgroups_1_SUITE,ok_tc,
			  {failed,{subgroups_1_SUITE,init_per_group,
				   {'EXIT',init_per_group_fails_on_purpose}}}}},
       {?eh,test_stats,{0,0,{0,1}}},
       {?eh,tc_auto_skip,{subgroups_1_SUITE,{end_per_group,fail_init},
             {failed,{subgroups_1_SUITE,init_per_group,
		      {'EXIT',init_per_group_fails_on_purpose}}}}}],
      {?eh,tc_auto_skip,{subgroups_1_SUITE,ok_tc,{group_result,fail_init,failed}}},
      {?eh,test_stats,{0,0,{0,2}}},
      {?eh,tc_start,{subgroups_1_SUITE,{end_per_group,subgroup_init_fail,[sequence]}}},
      {?eh,tc_done,{subgroups_1_SUITE,
		    {end_per_group,subgroup_init_fail,[sequence]},
		    {return_group_result,failed}}}],
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(subgroup_after_failed_case) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,2}},
     [{?eh,tc_start,{subgroups_1_SUITE,
		     {init_per_group,subgroup_after_failed_case,[sequence]}}},
      {?eh,tc_done,{subgroups_1_SUITE,
		    {init_per_group,subgroup_after_failed_case,[sequence]},ok}},
      {?eh,tc_start,{subgroups_1_SUITE,failing_tc}},
      {?eh,tc_done,{subgroups_1_SUITE,failing_tc,{failed,{error,{{badmatch,3},'_'}}}}},
      {?eh,test_stats,{0,1,{0,0}}},
      {?eh,tc_auto_skip,{subgroups_1_SUITE,ok_tc,{failed,{subgroups_1_SUITE,failing_tc}}}},
      {?eh,test_stats,{0,1,{0,1}}},
      {?eh,tc_start,{subgroups_1_SUITE,
		     {end_per_group,subgroup_after_failed_case,[sequence]}}},
      {?eh,tc_done,{subgroups_1_SUITE,
		    {end_per_group,subgroup_after_failed_case,[sequence]},
		    {return_group_result,failed}}}],
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
];

test_events(case_after_subgroup_return_fail) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,2}},
     [{?eh,tc_start,{subgroups_1_SUITE,
		     {init_per_group,case_after_subgroup_return_fail,[sequence]}}},
      {?eh,tc_done,{subgroups_1_SUITE,
		    {init_per_group,case_after_subgroup_return_fail,[sequence]},ok}},
      [{?eh,tc_start,{subgroups_1_SUITE,{init_per_group,return_fail,[]}}},
       {?eh,tc_done,{subgroups_1_SUITE,{init_per_group,return_fail,[]},ok}},
       {?eh,tc_start,{subgroups_1_SUITE,failing_tc}},
       {?eh,tc_done,{subgroups_1_SUITE,failing_tc,{failed,{error,{{badmatch,3},'_'}}}}},
       {?eh,test_stats,{0,1,{0,0}}},
       {?eh,tc_start,{subgroups_1_SUITE,{end_per_group,return_fail,[]}}},
       {?eh,tc_done,{subgroups_1_SUITE,{end_per_group,return_fail,[]},
		     {return_group_result,failed}}}],
      {?eh,tc_auto_skip,{subgroups_1_SUITE,ok_tc,{group_result,return_fail,failed}}},
      {?eh,test_stats,{0,1,{0,1}}},
      {?eh,tc_start,{subgroups_1_SUITE,
		     {end_per_group,case_after_subgroup_return_fail,[sequence]}}},
      {?eh,tc_done,{subgroups_1_SUITE,
		    {end_per_group,case_after_subgroup_return_fail,[sequence]},
		    {return_group_result,failed}}}],
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(case_after_subgroup_fail_init) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,2}},
     [{?eh,tc_start,{subgroups_1_SUITE,
		     {init_per_group,case_after_subgroup_fail_init,[sequence]}}},
      {?eh,tc_done,{subgroups_1_SUITE,
		    {init_per_group,case_after_subgroup_fail_init,[sequence]},ok}},
      [{?eh,tc_start,{subgroups_1_SUITE,{init_per_group,fail_init,[]}}},
       {?eh,tc_done,{subgroups_1_SUITE,
		     {init_per_group,fail_init,[]},
		     {failed,{error,init_per_group_fails_on_purpose}}}},
       {?eh,tc_auto_skip,{subgroups_1_SUITE,ok_tc,
			  {failed,
			   {subgroups_1_SUITE,init_per_group,
			    {'EXIT',init_per_group_fails_on_purpose}}}}},
       {?eh,test_stats,{0,0,{0,1}}},
       {?eh,tc_auto_skip,{subgroups_1_SUITE,{end_per_group,fail_init},
			  {failed,
			   {subgroups_1_SUITE,init_per_group,
			    {'EXIT',init_per_group_fails_on_purpose}}}}}],

      {?eh,tc_auto_skip,
       {subgroups_1_SUITE,ok_tc,{group_result,fail_init,failed}}},
      {?eh,test_stats,{0,0,{0,2}}},
      {?eh,tc_start,{subgroups_1_SUITE,
		     {end_per_group,case_after_subgroup_fail_init,[sequence]}}},
      {?eh,tc_done,{subgroups_1_SUITE,
		    {end_per_group,case_after_subgroup_fail_init,[sequence]},
		    {return_group_result,failed}}}],
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
