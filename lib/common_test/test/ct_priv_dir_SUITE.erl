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
%%% File: ct_priv_dir_SUITE
%%%
%%% Description: 
%%% Test that it works to use the unique_priv_dir option.
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
     unique_auto,
     unique_manual,
     spec_default,
     spec_unique_auto,
     spec_unique_manual
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
unique_auto(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "priv_dir_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,unique_auto},
			  {label,unique_auto},
			  {unique_priv_dir,auto}], Config),
    ok = execute(unique_auto, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
unique_manual(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "priv_dir_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,unique_manual},
			  {label,unique_manual},
			  {unique_priv_dir,manual}], Config),
    ok = execute(unique_manual, Opts, ERPid, Config).

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
spec_unique_auto(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "unique_auto.spec"),
    {Opts,ERPid} = setup([{spec,Spec},
			  {label,spec_unique_auto}], Config),
    ok = execute(spec_unique_auto, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
spec_unique_manual(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "unique_manual.spec"),
    {Opts,ERPid} = setup([{spec,Spec},
			  {label,spec_unique_manual}], Config),
    ok = execute(spec_unique_manual, Opts, ERPid, Config).


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


test_events(default) ->
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

test_events(unique_auto) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,unique_auto}},
     {?eh,tc_done,{priv_dir_SUITE,unique_auto,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(unique_manual) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,unique_manual}},
     {?eh,tc_done,{priv_dir_SUITE,unique_manual,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(spec_default) ->
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

test_events(spec_unique_auto) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,unique_auto}},
     {?eh,tc_done,{priv_dir_SUITE,unique_auto,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(spec_unique_manual) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{priv_dir_SUITE,init_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{priv_dir_SUITE,unique_manual}},
     {?eh,tc_done,{priv_dir_SUITE,unique_manual,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{priv_dir_SUITE,end_per_suite}},
     {?eh,tc_done,{priv_dir_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}].

