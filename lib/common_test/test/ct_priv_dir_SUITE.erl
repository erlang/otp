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
%%% File: ct_priv_dir_SUITE
%%%
%%% Description: 
%%% Test that it works to use the create_priv_dir option.
%%%
%%%-------------------------------------------------------------------
-module(ct_priv_dir_SUITE).

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
     default,
     auto_per_run,
     auto_per_tc,
     manual_per_tc,
     spec_default,
     spec_auto_per_run,
     spec_auto_per_run,
     spec_manual_per_tc
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
default(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "priv_dir_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,default},
			  {label,default}], Config),
    ok = execute(default, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
auto_per_run(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "priv_dir_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,default},
			  {label,auto_per_run},
			  {create_priv_dir,auto_per_run}], Config),
    ok = execute(auto_per_run, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
auto_per_tc(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "priv_dir_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,auto_per_tc},
			  {label,auto_per_tc},
			  {create_priv_dir,auto_per_tc}], Config),
    ok = execute(auto_per_tc, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
manual_per_tc(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "priv_dir_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,manual_per_tc},
			  {label,manual_per_tc},
			  {create_priv_dir,manual_per_tc}], Config),
    ok = execute(manual_per_tc, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
spec_default(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "default.spec"),
    {Opts,ERPid} = setup([{spec,Spec},
			  {label,spec_default}], Config),
    ok = execute(spec_default, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
spec_auto_per_run(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "auto_per_run.spec"),
    {Opts,ERPid} = setup([{spec,Spec},
			  {label,spec_auto_per_run}], Config),
    ok = execute(spec_auto_per_run, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
spec_auto_per_tc(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "auto_per_tc.spec"),
    {Opts,ERPid} = setup([{spec,Spec},
			  {label,spec_auto_per_tc}], Config),
    ok = execute(spec_auto_per_tc, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
spec_manual_per_tc(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "manual_per_tc.spec"),
    {Opts,ERPid} = setup([{spec,Spec},
			  {label,spec_manual_per_tc}], Config),
    ok = execute(spec_manual_per_tc, Opts, ERPid, Config).


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


test_events(DEF) when DEF == default ; DEF == auto_per_run ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,default}},
     {?eh,tc_done,{priv_dir_SUITE,default,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(auto_per_tc) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,auto_per_tc}},
     {?eh,tc_done,{priv_dir_SUITE,auto_per_tc,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(manual_per_tc) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,manual_per_tc}},
     {?eh,tc_done,{priv_dir_SUITE,manual_per_tc,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(SPECDEF) when SPECDEF == spec_default ;
			  SPECDEF == spec_auto_per_run ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,default}},
     {?eh,tc_done,{priv_dir_SUITE,default,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(spec_auto_per_tc) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,auto_per_tc}},
     {?eh,tc_done,{priv_dir_SUITE,auto_per_tc,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(spec_manual_per_tc) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,manual_per_tc}},
     {?eh,tc_done,{priv_dir_SUITE,manual_per_tc,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}].

