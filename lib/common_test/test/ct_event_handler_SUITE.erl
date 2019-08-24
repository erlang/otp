%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%% File: ct_event_handler_SUITE.erl
%%%
%%% Description: This suite will install event handlers and run
%%% some simple tests to check that events are generated according
%%% to the specification (see Event Handling in CT User's Guide).
%%%-------------------------------------------------------------------
-module(ct_event_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/src/ct_util.hrl").

%-include_lib("common_test/include/ct_event.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    EH = filename:join(DataDir, "eh_A.erl"),
    CResult = compile:file(EH, [verbose,report,{outdir,PrivDir}]),
    test_server:format("~s compilation result: ~p~n", [EH,CResult]),

    Config1 = ct_test_support:init_per_suite(Config, 0),
    Config1.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_stop, results, event_mgrs].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
	Config.



%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_stop(doc) -> 
    [];

start_stop(suite) -> 
    [];

start_stop(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_11_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    Opts = Opts0 ++ [{suite,Suite1},{testcase,tc1},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),
    
    Events = ct_test_support:get_events(ERPid, Config),    

    ct_test_support:log_events(start_stop, 
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),
    
    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,{1,1,1}},
	 {eh_A,tc_start,{eh_11_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,init_per_suite,ok}},
	 {eh_A,tc_start,{eh_11_SUITE,tc1}},
	 {eh_A,tc_done,{eh_11_SUITE,tc1,ok}},
	 {eh_A,test_stats,{1,0,{0,0}}},
	 {eh_A,tc_start,{eh_11_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


results(doc) -> 
    [];

results(suite) -> 
    [];

results(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_11_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    Opts = Opts0 ++ [{suite,Suite1},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),
    
    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(results, 
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,{1,1,5}},
	 {eh_A,tc_start,{eh_11_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,init_per_suite,ok}},
	 [{eh_A,tc_start,{eh_11_SUITE,{init_per_group,g1,[]}}},
	  {eh_A,tc_done,{eh_11_SUITE,{init_per_group,g1,[]},ok}},
	  {eh_A,tc_start,{eh_11_SUITE,tc1}},
	  {eh_A,tc_done,{eh_11_SUITE,tc1,ok}},
	  {eh_A,test_stats,{1,0,{0,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc2}},
	  {eh_A,tc_done,{eh_11_SUITE,tc2,ok}},
	  {eh_A,test_stats,{2,0,{0,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc3}},
	  {eh_A,tc_done,{eh_11_SUITE,tc3,{skipped,"Skip"}}},
	  {eh_A,test_stats,{2,0,{1,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc4}},
	  {eh_A,tc_done,{eh_11_SUITE,tc4,{skipped,"Skipped"}}},
	  {eh_A,test_stats,{2,0,{2,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc5}},
	  {eh_A,tc_done,{eh_11_SUITE,tc5,{failed,{error,'Failing'}}}},
	  {eh_A,test_stats,{2,1,{2,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,{end_per_group,g1,[]}}},
	  {eh_A,tc_done,{eh_11_SUITE,{end_per_group,g1,[]},ok}}],
	 {eh_A,tc_start,{eh_11_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


event_mgrs(_) ->
    ?CT_EVMGR_REF = ct:get_event_mgr_ref(),
    ?CT_MEVMGR_REF = ct_master:get_event_mgr_ref().


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
