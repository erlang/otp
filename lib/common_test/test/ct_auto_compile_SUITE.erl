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
%%% File: ct_auto_compile_SUITE
%%%
%%% Description: 
%%% 
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_auto_compile_SUITE).
-warning("Ignore me -- testing that the debugger can handle warnings").

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
    [ac_flag, ac_spec].

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
ac_flag(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    file:copy(filename:join(DataDir, "bad_SUITE.erl"),
	      filename:join(PrivDir, "bad_SUITE.erl")), 
    Suite = filename:join(DataDir, "dummy_SUITE"),
    compile:file(Suite, [{outdir,PrivDir}]),
    {Opts,ERPid} = setup([{dir,PrivDir},
			  {auto_compile,false},
			  {label,"ac_flag"}],
			 Config),

    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(ac_flag, 
			       reformat(Events, ?eh),
			       PrivDir,
			       Opts),

    TestEvents = events_to_check(ac_flag),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 
ac_spec(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    file:copy(filename:join(DataDir, "bad_SUITE.erl"),
	      filename:join(PrivDir, "bad_SUITE.erl")),
    Suite = filename:join(DataDir, "dummy_SUITE"),
    compile:file(Suite, [{outdir,PrivDir}]),
    TestSpec = [{label,ac_spec},
		{auto_compile,false},
		{suites,PrivDir,all}],
    FileName = filename:join(?config(priv_dir, Config),"ac_spec.spec"),
    {ok,Dev} = file:open(FileName, [write]),
    [io:format(Dev, "~p.~n", [Term]) || Term <- TestSpec],
    file:close(Dev),

    {Opts,ERPid} = setup([{spec,FileName}], Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(ac_spec, 
			       reformat(Events, ?eh),
			       PrivDir,
			       Opts),

    TestEvents = events_to_check(ac_spec),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).


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

test_events(ac_flag) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,3}},
     {?eh,tc_start,{ct_framework,error_in_suite}},
     {?eh,tc_done,{ct_framework,error_in_suite,
       {failed,{error,'bad_SUITE cannot be compiled or loaded'}}}},
     {?eh,tc_start,{dummy_SUITE,init_per_suite}},
     {?eh,tc_done,{dummy_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,1,{1,0}}},
     {?eh,tc_start,{dummy_SUITE,end_per_suite}},
     {?eh,tc_done,{dummy_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(ac_spec) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,3}},
     {?eh,tc_start,{ct_framework,error_in_suite}},
     {?eh,tc_done,{ct_framework,error_in_suite,
       {failed,{error,'bad_SUITE cannot be compiled or loaded'}}}},
     {?eh,tc_start,{dummy_SUITE,init_per_suite}},
     {?eh,tc_done,{dummy_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,1,{1,0}}},
     {?eh,tc_start,{dummy_SUITE,end_per_suite}},
     {?eh,tc_done,{dummy_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
