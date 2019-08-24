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
%%% File: ct_group_info_SUITE
%%%
%%% Description: 
%%% Test that the group info function works as expected with regards
%%% to timetraps and require (and default config values).
%%%
%%%-------------------------------------------------------------------
-module(ct_group_info_SUITE).

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
    [
     timetrap_all,
     timetrap_group,
     timetrap_group_case,
     timetrap_all_no_ips,
     timetrap_all_no_ipg,
     require,
     require_default,
     require_no_ips,
     require_no_ipg
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
timetrap_all(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_timetrap_1_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},
			  {label,timetrap_all}], Config),
    ok = execute(timetrap_all, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
timetrap_group(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_timetrap_1_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{group,[g1,g3,g7]},
			  {label,timetrap_group}], Config),
    ok = execute(timetrap_group, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
timetrap_group_case(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_timetrap_1_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{group,g4},{testcase,t41},
			  {label,timetrap_group_case}], Config),
    ok = execute(timetrap_group_case, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
timetrap_all_no_ips(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_timetrap_2_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},
			  {label,timetrap_all_no_ips}], Config),
    ok = execute(timetrap_all_no_ips, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
timetrap_all_no_ipg(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_timetrap_3_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},
			  {label,timetrap_all_no_ipg}], Config),
    ok = execute(timetrap_all_no_ipg, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
require(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_require_1_SUITE"),
    CfgFile = filename:join(DataDir, "vars.cfg"),
    {Opts,ERPid} = setup([{suite,Suite},{config,CfgFile},
			  {label,require}], Config),
    ok = execute(require, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
require_default(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_require_1_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},
			  {label,require_default}], Config),
    ok = execute(require_default, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
require_no_ips(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_require_2_SUITE"),
    CfgFile = filename:join(DataDir, "vars.cfg"),
    {Opts,ERPid} = setup([{suite,Suite},{config,CfgFile},
			  {label,require_no_ips}], Config),
    ok = execute(require_no_ips, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
require_no_ipg(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_require_3_SUITE"),
    CfgFile = filename:join(DataDir, "vars.cfg"),
    {Opts,ERPid} = setup([{suite,Suite},{config,CfgFile},
			  {label,require_no_ipg}], Config),
    ok = execute(require_no_ipg, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}}|Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

execute(Name, Opts, ERPid, Config) ->
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Name, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(Name),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

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


test_events(timetrap_all) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,14}},
     {?eh,tc_done,{group_timetrap_1_SUITE,init_per_suite,ok}},

     {?eh,tc_done,{group_timetrap_1_SUITE,t1,{failed,{timetrap_timeout,1000}}}},

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{group_timetrap_1_SUITE,t11,{failed,{timetrap_timeout,500}}}},
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{group_timetrap_1_SUITE,t21,{failed,{timetrap_timeout,1500}}}},
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g2,[]},ok}}],

     {?eh,tc_done,{group_timetrap_1_SUITE,t2,{failed,{timetrap_timeout,1000}}}},

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g3,[]},ok}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g4,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t41,{failed,{timetrap_timeout,250}}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g4,[]},ok}}],
      {?eh,tc_done,{group_timetrap_1_SUITE,t31,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g5,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g5,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t51,{failed,{timetrap_timeout,1500}}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g5,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g5,[]},ok}}],
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g3,[]},ok}}],

     {?eh,tc_done,{group_timetrap_1_SUITE,t3,{failed,{timetrap_timeout,250}}}},

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g6,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g6,[]},ok}},
      {?eh,tc_done,{group_timetrap_1_SUITE,t61,{failed,{timetrap_timeout,500}}}},
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g6,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g6,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g7,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g7,[]},ok}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g8,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g8,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t81,{failed,{timetrap_timeout,750}}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g8,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g8,[]},ok}}],
      {?eh,tc_done,{group_timetrap_1_SUITE,t71,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g9,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g9,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t91,{failed,{timetrap_timeout,250}}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g9,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g9,[]},ok}}],
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g7,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g7,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g10,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g10,[]},ok}},
      {?eh,tc_done,{group_timetrap_1_SUITE,t101,{failed,{timetrap_timeout,1000}}}},
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g10,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g10,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g11,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,
		    {init_per_group,g11,[]},
		    {auto_skipped,{group0_failed,bad_return_value}}}},
      {?eh,tc_auto_skip,
       {group_timetrap_1_SUITE,{t111,g11},{group0_failed,bad_return_value}}},
      {?eh,test_stats,{0,13,{0,1}}},
      {?eh,tc_auto_skip,{group_timetrap_1_SUITE,
			 {end_per_group,g11},
			 {group0_failed,bad_return_value}}}],

     {?eh,tc_start,{group_timetrap_1_SUITE,end_per_suite}},
     {?eh,tc_done,{group_timetrap_1_SUITE,end_per_suite,ok}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(timetrap_group) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,7}},
     {?eh,tc_done,{group_timetrap_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{group_timetrap_1_SUITE,t11,{failed,{timetrap_timeout,500}}}},
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g3,[]},ok}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g4,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t41,{failed,{timetrap_timeout,250}}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g4,[]},ok}}],
      {?eh,tc_done,{group_timetrap_1_SUITE,t31,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g5,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g5,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t51,{failed,{timetrap_timeout,1500}}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g5,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g5,[]},ok}}],
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g3,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g7,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g7,[]},ok}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g8,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g8,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t81,{failed,{timetrap_timeout,750}}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g8,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g8,[]},ok}}],
      {?eh,tc_done,{group_timetrap_1_SUITE,t71,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g9,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g9,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t91,{failed,{timetrap_timeout,250}}}},
       {?eh,test_stats,{0,7,{0,0}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g9,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g9,[]},ok}}],
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g7,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g7,[]},ok}}],

     {?eh,tc_done,{group_timetrap_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(timetrap_group_case) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_done,{group_timetrap_1_SUITE,init_per_suite,ok}},

     [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g3,[]},ok}},
      [{?eh,tc_start,{group_timetrap_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{init_per_group,g4,[]},ok}},
       {?eh,tc_done,{group_timetrap_1_SUITE,t41,{failed,{timetrap_timeout,250}}}},
       {?eh,test_stats,{0,1,{0,0}}},
       {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g4,[]},ok}}],
      {?eh,tc_start,{group_timetrap_1_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_1_SUITE,{end_per_group,g3,[]},ok}}],

     {?eh,tc_done,{group_timetrap_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(timetrap_all_no_ips) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,14}},

     {?eh,tc_done,{group_timetrap_2_SUITE,t1,{failed,{timetrap_timeout,1000}}}},

     [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{group_timetrap_2_SUITE,t11,{failed,{timetrap_timeout,500}}}},
      {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{group_timetrap_2_SUITE,t21,{failed,{timetrap_timeout,1500}}}},
      {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g2,[]},ok}}],

     {?eh,tc_done,{group_timetrap_2_SUITE,t2,{failed,{timetrap_timeout,1000}}}},

     [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g3,[]},ok}},
      [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g4,[]},ok}},
       {?eh,tc_done,{group_timetrap_2_SUITE,t41,{failed,{timetrap_timeout,250}}}},
       {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g4,[]},ok}}],
      {?eh,tc_done,{group_timetrap_2_SUITE,t31,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g5,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g5,[]},ok}},
       {?eh,tc_done,{group_timetrap_2_SUITE,t51,{failed,{timetrap_timeout,1500}}}},
       {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g5,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g5,[]},ok}}],
      {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g3,[]},ok}}],

     {?eh,tc_done,{group_timetrap_2_SUITE,t3,{failed,{timetrap_timeout,250}}}},

     [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g6,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g6,[]},ok}},
      {?eh,tc_done,{group_timetrap_2_SUITE,t61,{failed,{timetrap_timeout,500}}}},
      {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g6,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g6,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g7,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g7,[]},ok}},
      [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g8,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g8,[]},ok}},
       {?eh,tc_done,{group_timetrap_2_SUITE,t81,{failed,{timetrap_timeout,750}}}},
       {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g8,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g8,[]},ok}}],
      {?eh,tc_done,{group_timetrap_2_SUITE,t71,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g9,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g9,[]},ok}},
       {?eh,tc_done,{group_timetrap_2_SUITE,t91,{failed,{timetrap_timeout,250}}}},
       {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g9,[]}}},
       {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g9,[]},ok}}],
      {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g7,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g7,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g10,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{init_per_group,g10,[]},ok}},
      {?eh,tc_done,{group_timetrap_2_SUITE,t101,{failed,{timetrap_timeout,1000}}}},
      {?eh,tc_start,{group_timetrap_2_SUITE,{end_per_group,g10,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,{end_per_group,g10,[]},ok}}],

     [{?eh,tc_start,{group_timetrap_2_SUITE,{init_per_group,g11,[]}}},
      {?eh,tc_done,{group_timetrap_2_SUITE,
		    {init_per_group,g11,[]},
		    {auto_skipped,{group0_failed,bad_return_value}}}},
      {?eh,tc_auto_skip,{group_timetrap_2_SUITE,{t111,g11},
			 {group0_failed,bad_return_value}}},
      {?eh,test_stats,{0,13,{0,1}}},
      {?eh,tc_auto_skip,{group_timetrap_2_SUITE,
			 {end_per_group,g11},
			 {group0_failed,bad_return_value}}}],
     {?eh,stop_logging,[]}
    ];

test_events(timetrap_all_no_ipg) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,14}},

     {?eh,tc_done,{group_timetrap_3_SUITE,t1,{failed,{timetrap_timeout,1000}}}},

     [{?eh,tc_start,{ct_framework,{init_per_group,g1,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g1,[{suite,group_timetrap_3_SUITE}]},ok}},
      {?eh,tc_done,{group_timetrap_3_SUITE,t11,{failed,{timetrap_timeout,500}}}},
      {?eh,tc_start,{ct_framework,{end_per_group,g1,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g1,[{suite,group_timetrap_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g2,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g2,[{suite,group_timetrap_3_SUITE}]},ok}},
      {?eh,tc_done,{group_timetrap_3_SUITE,t21,{failed,{timetrap_timeout,1500}}}},
      {?eh,tc_start,{ct_framework,{end_per_group,g2,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g2,[{suite,group_timetrap_3_SUITE}]},ok}}],

     {?eh,tc_done,{group_timetrap_3_SUITE,t2,{failed,{timetrap_timeout,1000}}}},

     [{?eh,tc_start,{ct_framework,{init_per_group,g3,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g3,[{suite,group_timetrap_3_SUITE}]},ok}},
      [{?eh,tc_start,{ct_framework,{init_per_group,g4,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{init_per_group,g4,[{suite,group_timetrap_3_SUITE}]},ok}},
       {?eh,tc_done,{group_timetrap_3_SUITE,t41,{failed,{timetrap_timeout,250}}}},
       {?eh,tc_start,{ct_framework,{end_per_group,g4,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{end_per_group,g4,[{suite,group_timetrap_3_SUITE}]},ok}}],
      {?eh,tc_done,{group_timetrap_3_SUITE,t31,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{ct_framework,{init_per_group,g5,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{init_per_group,g5,[{suite,group_timetrap_3_SUITE}]},ok}},
       {?eh,tc_done,{group_timetrap_3_SUITE,t51,{failed,{timetrap_timeout,1500}}}},
       {?eh,tc_start,{ct_framework,{end_per_group,g5,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{end_per_group,g5,[{suite,group_timetrap_3_SUITE}]},ok}}],
      {?eh,tc_start,{ct_framework,{end_per_group,g3,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g3,[{suite,group_timetrap_3_SUITE}]},ok}}],

     {?eh,tc_done,{group_timetrap_3_SUITE,t3,{failed,{timetrap_timeout,250}}}},

     [{?eh,tc_start,{ct_framework,{init_per_group,g6,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g6,[{suite,group_timetrap_3_SUITE}]},ok}},
      {?eh,tc_done,{group_timetrap_3_SUITE,t61,{failed,{timetrap_timeout,500}}}},
      {?eh,tc_start,{ct_framework,{end_per_group,g6,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g6,[{suite,group_timetrap_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g7,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g7,[{suite,group_timetrap_3_SUITE}]},ok}},
      [{?eh,tc_start,{ct_framework,{init_per_group,g8,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{init_per_group,g8,[{suite,group_timetrap_3_SUITE}]},ok}},
       {?eh,tc_done,{group_timetrap_3_SUITE,t81,{failed,{timetrap_timeout,750}}}},
       {?eh,tc_start,{ct_framework,{end_per_group,g8,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{end_per_group,g8,[{suite,group_timetrap_3_SUITE}]},ok}}],
      {?eh,tc_done,{group_timetrap_3_SUITE,t71,{failed,{timetrap_timeout,500}}}},
      [{?eh,tc_start,{ct_framework,{init_per_group,g9,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{init_per_group,g9,[{suite,group_timetrap_3_SUITE}]},ok}},
       {?eh,tc_done,{group_timetrap_3_SUITE,t91,{failed,{timetrap_timeout,250}}}},
       {?eh,tc_start,{ct_framework,{end_per_group,g9,[{suite,group_timetrap_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{end_per_group,g9,[{suite,group_timetrap_3_SUITE}]},ok}}],
      {?eh,tc_start,{ct_framework,{end_per_group,g7,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g7,[{suite,group_timetrap_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g10,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g10,[{suite,group_timetrap_3_SUITE}]},ok}},
      {?eh,tc_done,{group_timetrap_3_SUITE,t101,{failed,{timetrap_timeout,1000}}}},
      {?eh,tc_start,{ct_framework,{end_per_group,g10,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g10,[{suite,group_timetrap_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g11,[{suite,group_timetrap_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,
		    {init_per_group,g11,[{suite,group_timetrap_3_SUITE}]},
		    {auto_skipped,{group0_failed,bad_return_value}}}},
      {?eh,tc_auto_skip,{group_timetrap_3_SUITE,{t111,g11},{group0_failed,bad_return_value}}},
      {?eh,test_stats,{0,13,{0,1}}},
      {?eh,tc_auto_skip,{ct_framework,{end_per_group,g11},
			 {group0_failed,bad_return_value}}}],

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(require) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,13}},
     {?eh,tc_done,{group_require_1_SUITE,init_per_suite,ok}},
     {?eh,tc_done,{group_require_1_SUITE,t1,ok}},

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t11,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t21,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g2,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g3,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t31,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g3,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g4,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g4,[]},
		    {auto_skipped,{require_failed,
				   {name_in_use,common2_alias,common2}}}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{t41,g4},
			  {require_failed,
			   {name_in_use,common2_alias,common2}}}},
      {?eh,test_stats,{4,0,{0,1}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{end_per_group,g4},
	{require_failed,{name_in_use,common2_alias,common2}}}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g5,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g5,[]},ok}},
      [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g6,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g6,[]},ok}},
       {?eh,tc_done,{group_require_1_SUITE,t61,ok}},
       {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g6,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g6,[]},ok}}],
      {?eh,tc_done,{group_require_1_SUITE,t51,ok}},
      [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g7,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g7,[]},ok}},
       {?eh,tc_done,{group_require_1_SUITE,t71,ok}},
       {?eh,tc_done,{group_require_1_SUITE,t72,ok}},
       {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g7,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g7,[]},ok}}],
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g5,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g5,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g8,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,
		    {init_per_group,g8,[]},
		    {auto_skipped,{require_failed,
				   {not_available,non_existing}}}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{t81,g8},
			 {require_failed,{not_available,non_existing}}}},
      {?eh,test_stats,{8,0,{0,2}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{end_per_group,g8},
			 {require_failed,{not_available,non_existing}}}}],
     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g9,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g9,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t91,
		    {auto_skipped,{require_failed,
				   {not_available,non_existing}}}}},
      {?eh,test_stats,{8,0,{0,3}}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g9,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g9,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g10,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g10,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t101,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g10,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g10,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g11,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,
		    {init_per_group,g11,[]},
		    {auto_skipped,{group0_failed,bad_return_value}}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{t111,g11},
			 {group0_failed,bad_return_value}}},
      {?eh,test_stats,{9,0,{0,4}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,
			 {end_per_group,g11},
			 {group0_failed,bad_return_value}}}],
     
     {?eh,tc_done,{group_require_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(require_default) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,13}},
     {?eh,tc_done,{group_require_1_SUITE,init_per_suite,ok}},
     {?eh,tc_done,{group_require_1_SUITE,t1,ok}},

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t11,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t21,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g2,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g3,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t31,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g3,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g4,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,
		    {init_per_group,g4,[]},
		    {auto_skipped,{require_failed,{not_available,common3}}}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{t41,g4},
			 {require_failed,{not_available,common3}}}},
      {?eh,test_stats,{4,0,{0,1}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{end_per_group,g4},
			 {require_failed,{not_available,common3}}}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g5,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g5,[]},ok}},
      [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g6,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g6,[]},ok}},
       {?eh,tc_done,{group_require_1_SUITE,t61,ok}},
       {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g6,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g6,[]},ok}}],
      {?eh,tc_done,{group_require_1_SUITE,t51,ok}},
      [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g7,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g7,[]},ok}},
       {?eh,tc_done,{group_require_1_SUITE,t71,ok}},
       {?eh,tc_done,{group_require_1_SUITE,t72,ok}},
       {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g7,[]}}},
       {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g7,[]},ok}}],
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g5,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g5,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g8,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,
		    {init_per_group,g8,[]},
		    {auto_skipped,{require_failed,
				   {not_available,non_existing}}}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{t81,g8},
			 {require_failed,{not_available,non_existing}}}},
      {?eh,test_stats,{8,0,{0,2}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{end_per_group,g8},
			 {require_failed,{not_available,non_existing}}}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g9,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g9,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t91,
		    {auto_skipped,{require_failed,
				   {not_available,non_existing}}}}},
      {?eh,test_stats,{8,0,{0,3}}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g9,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g9,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g10,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{init_per_group,g10,[]},ok}},
      {?eh,tc_done,{group_require_1_SUITE,t101,ok}},
      {?eh,tc_start,{group_require_1_SUITE,{end_per_group,g10,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,{end_per_group,g10,[]},ok}}],

     [{?eh,tc_start,{group_require_1_SUITE,{init_per_group,g11,[]}}},
      {?eh,tc_done,{group_require_1_SUITE,
		    {init_per_group,g11,[]},
		    {auto_skipped,{group0_failed,bad_return_value}}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,{t111,g11},
			 {group0_failed,bad_return_value}}},
      {?eh,test_stats,{9,0,{0,4}}},
      {?eh,tc_auto_skip,{group_require_1_SUITE,
			 {end_per_group,g11},
			 {group0_failed,bad_return_value}}}],

     {?eh,tc_done,{group_require_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(require_no_ips) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,13}},
     {?eh,tc_done,{group_require_2_SUITE,t1,ok}},

     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{group_require_2_SUITE,t11,ok}},
      {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{group_require_2_SUITE,t21,ok}},
      {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g2,[]},ok}}],

     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g3,[]},ok}},
      {?eh,tc_done,{group_require_2_SUITE,t31,ok}},
      {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g3,[]},ok}}],

     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g4,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g4,[]},
		    {auto_skipped,{require_failed,
				   {name_in_use,common2_alias,common2}}}}},
      {?eh,tc_auto_skip,{group_require_2_SUITE,{t41,g4},
			  {require_failed,{name_in_use,common2_alias,common2}}}},
      {?eh,test_stats,{4,0,{0,1}}},
      {?eh,tc_auto_skip,{group_require_2_SUITE,{end_per_group,g4},
			 {require_failed,{name_in_use,common2_alias,common2}}}}],

     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g5,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g5,[]},ok}},
      [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g6,[]}}},
       {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g6,[]},ok}},
       {?eh,tc_done,{group_require_2_SUITE,t61,ok}},
       {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g6,[]}}},
       {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g6,[]},ok}}],
      {?eh,tc_done,{group_require_2_SUITE,t51,ok}},
      [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g7,[]}}},
       {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g7,[]},ok}},
       {?eh,tc_done,{group_require_2_SUITE,t71,ok}},
       {?eh,tc_done,{group_require_2_SUITE,t72,ok}},
       {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g7,[]}}},
       {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g7,[]},ok}}],
      {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g5,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g5,[]},ok}}],

     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g8,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,
		    {init_per_group,g8,[]},
		    {auto_skipped,{require_failed,
				   {not_available,non_existing}}}}},
      {?eh,tc_auto_skip,{group_require_2_SUITE,{t81,g8},
			 {require_failed,{not_available,non_existing}}}},
      {?eh,test_stats,{8,0,{0,2}}},
      {?eh,tc_auto_skip,{group_require_2_SUITE,{end_per_group,g8},
			 {require_failed,{not_available,non_existing}}}}],
     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g9,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g9,[]},ok}},
      {?eh,tc_done,{group_require_2_SUITE,t91,
		    {auto_skipped,{require_failed,
				   {not_available,non_existing}}}}},
      {?eh,test_stats,{8,0,{0,3}}},
      {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g9,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g9,[]},ok}}],

     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g10,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{init_per_group,g10,[]},ok}},
      {?eh,tc_done,{group_require_2_SUITE,t101,ok}},
      {?eh,tc_start,{group_require_2_SUITE,{end_per_group,g10,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,{end_per_group,g10,[]},ok}}],
     
     [{?eh,tc_start,{group_require_2_SUITE,{init_per_group,g11,[]}}},
      {?eh,tc_done,{group_require_2_SUITE,
		    {init_per_group,g11,[]},
		    {auto_skipped,{group0_failed,bad_return_value}}}},
      {?eh,tc_auto_skip,{group_require_2_SUITE,{t111,g11},
			 {group0_failed,bad_return_value}}},
      {?eh,test_stats,{9,0,{0,4}}},
      {?eh,tc_auto_skip,{group_require_2_SUITE,
			 {end_per_group,g11},
			 {group0_failed,bad_return_value}}}],

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(require_no_ipg) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,13}},
     {?eh,tc_done,{group_require_3_SUITE,t1,ok}},

     [{?eh,tc_start,{ct_framework,{init_per_group,g1,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g1,[{suite,group_require_3_SUITE}]},ok}},
      {?eh,tc_done,{group_require_3_SUITE,t11,ok}},
      {?eh,tc_start,{ct_framework,{end_per_group,g1,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g1,[{suite,group_require_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g2,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g2,[{suite,group_require_3_SUITE}]},ok}},
      {?eh,tc_done,{group_require_3_SUITE,t21,ok}},
      {?eh,tc_start,{ct_framework,{end_per_group,g2,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g2,[{suite,group_require_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g3,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g3,[{suite,group_require_3_SUITE}]},ok}},
      {?eh,tc_done,{group_require_3_SUITE,t31,ok}},
      {?eh,tc_start,{ct_framework,{end_per_group,g3,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g3,[{suite,group_require_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g4,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g4,[{suite,group_require_3_SUITE}]},
		    {auto_skipped,{require_failed,{name_in_use,common2_alias,common2}}}}},
      {?eh,tc_auto_skip,{group_require_3_SUITE,{t41,g4},
			 {require_failed,{name_in_use,common2_alias,common2}}}},
      {?eh,test_stats,{4,0,{0,1}}},
      {?eh,tc_auto_skip,{ct_framework,{end_per_group,g4},
			 {require_failed,{name_in_use,common2_alias,common2}}}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g5,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g5,[{suite,group_require_3_SUITE}]},ok}},
      [{?eh,tc_start,{ct_framework,{init_per_group,g6,[{suite,group_require_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{init_per_group,g6,[{suite,group_require_3_SUITE}]},ok}},
       {?eh,tc_done,{group_require_3_SUITE,t61,ok}},
       {?eh,tc_start,{ct_framework,{end_per_group,g6,[{suite,group_require_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{end_per_group,g6,[{suite,group_require_3_SUITE}]},ok}}],
      {?eh,tc_done,{group_require_3_SUITE,t51,ok}},
      [{?eh,tc_start,{ct_framework,{init_per_group,g7,[{suite,group_require_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{init_per_group,g7,[{suite,group_require_3_SUITE}]},ok}},
       {?eh,tc_done,{group_require_3_SUITE,t71,ok}},
       {?eh,tc_done,{group_require_3_SUITE,t72,ok}},
       {?eh,tc_start,{ct_framework,{end_per_group,g7,[{suite,group_require_3_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{end_per_group,g7,[{suite,group_require_3_SUITE}]},ok}}],
      {?eh,tc_start,{ct_framework,{end_per_group,g5,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g5,[{suite,group_require_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g8,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g8,[{suite,group_require_3_SUITE}]},
		    {auto_skipped,{require_failed,{not_available,non_existing}}}}},
      {?eh,tc_auto_skip,{group_require_3_SUITE,{t81,g8},
			 {require_failed,{not_available,non_existing}}}},
      {?eh,test_stats,{8,0,{0,2}}},
      {?eh,tc_auto_skip,{ct_framework,{end_per_group,g8},
			 {require_failed,{not_available,non_existing}}}}],
     [{?eh,tc_start,{ct_framework,{init_per_group,g9,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g9,[{suite,group_require_3_SUITE}]},ok}},
      {?eh,tc_done,{group_require_3_SUITE,t91,
		    {auto_skipped,{require_failed,{not_available,non_existing}}}}},
      {?eh,test_stats,{8,0,{0,3}}},
      {?eh,tc_start,{ct_framework,{end_per_group,g9,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g9,[{suite,group_require_3_SUITE}]},ok}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g10,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g10,[{suite,group_require_3_SUITE}]},ok}},
      {?eh,tc_done,{group_require_3_SUITE,t101,ok}},
      {?eh,tc_start,{ct_framework,{end_per_group,g10,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g10,[{suite,group_require_3_SUITE}]},ok}}],
     
     [{?eh,tc_start,{ct_framework,{init_per_group,g11,[{suite,group_require_3_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g11,[{suite,group_require_3_SUITE}]},
		    {auto_skipped,{group0_failed,bad_return_value}}}},
      {?eh,tc_auto_skip,{group_require_3_SUITE,{t111,g11},{group0_failed,bad_return_value}}},
      {?eh,test_stats,{9,0,{0,4}}},
      {?eh,tc_auto_skip,{ct_framework,{end_per_group,g11},
			 {group0_failed,bad_return_value}}}],

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].


