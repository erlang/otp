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

%%%-------------------------------------------------------------------
%%% File: ct_gen_conn_SUITE
%%%
%%% Description:
%%% Test that the generic connection handling in CT works as expected.
%%%
%%% The suite used for the test is located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_gen_conn_SUITE).

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
    ct_test_support:init_per_suite(Config).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [handles_to_multi_conn_pids, handles_to_single_conn_pids,
     names_to_multi_conn_pids, names_to_single_conn_pids].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
handles_to_multi_conn_pids(Config) ->
    run_test(handles_to_multi_conn_pids, Config).

handles_to_single_conn_pids(Config) ->
    run_test(handles_to_single_conn_pids, Config).

names_to_multi_conn_pids(Config) ->
    run_test(names_to_multi_conn_pids, Config).

names_to_single_conn_pids(Config) ->
    run_test(names_to_single_conn_pids, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
run_test(TestCase, Config) ->
    DataDir = ?config(data_dir, Config),
    {Opts,ERPid} = setup_env([{dir,DataDir},
			      {suite,conn_SUITE},
			      {testcase,TestCase},
			      {config,filename:join(DataDir,"conn.conf")}],
			     Config),
    ok = ct_test_support:run(Opts, Config),
    TestEvents = ct_test_support:get_events(ERPid, Config),
    ct_test_support:log_events(TestCase,
			       reformat_events(TestEvents, ?eh),
			       ?config(priv_dir, Config),
			       Opts),
    ExpEvents = events_to_check(TestCase),
    ok = ct_test_support:verify_events(ExpEvents, TestEvents, Config).

setup_env(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}} | Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

reformat_events(Events, EH) ->
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

test_events(Name) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{conn_SUITE,init_per_suite}},
     {?eh,tc_done,{conn_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{conn_SUITE,Name}},
     {?eh,tc_done,{conn_SUITE,Name,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{conn_SUITE,end_per_suite}},
     {?eh,tc_done,{conn_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
