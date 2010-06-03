%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%%% File: ct_event_handler_SUITE.erl
%%%
%%% Description: This suite will install event handlers and run
%%% some simple tests to check that events are generated according
%%% to the specification (see Event Handling in CT User's Guide).
%%%-------------------------------------------------------------------
-module(ct_event_handler_SUITE).

-compile(export_all).

-include_lib("test_server/include/test_server.hrl").

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

all(doc) -> 
    [];

all(suite) -> 
    [start_stop, results].


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
			       ?config(priv_dir, Config)),
    
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
			       ?config(priv_dir, Config)),

    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,{1,1,3}},
	 {eh_A,tc_start,{eh_11_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,init_per_suite,ok}},
	 {eh_A,tc_start,{eh_11_SUITE,tc1}},
	 {eh_A,tc_done,{eh_11_SUITE,tc1,ok}},
	 {eh_A,test_stats,{1,0,{0,0}}},
	 {eh_A,tc_start,{eh_11_SUITE,tc2}},
	 {eh_A,tc_done,{eh_11_SUITE,tc2,{skipped,"Skipped"}}},
	 {eh_A,test_stats,{1,0,{1,0}}},
	 {eh_A,tc_start,{eh_11_SUITE,tc3}},
	 {eh_A,tc_done,{eh_11_SUITE,tc3,{failed,{error,'Failing'}}}},
	 {eh_A,test_stats,{1,1,{1,0}}},
	 {eh_A,tc_start,{eh_11_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
