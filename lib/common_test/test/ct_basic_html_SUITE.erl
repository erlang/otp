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
%%% File: ct_basic_html_SUITE
%%%
%%% Description: 
%%% 
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_basic_html_SUITE).

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
    [basic_flag, basic_spec].

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
basic_flag(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suites = [filename:join(DataDir, "babbling_SUITE")],
    {Opts,ERPid} = setup([{suite,Suites},
			  {basic_html,true},
			  {label,"basic_flag"}],
			 Config),

    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(basic_flag, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(basic_flag),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 
basic_spec(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    TestSpec = [{label,basic_spec},
		{basic_html,true},
		{suites,DataDir,babbling_SUITE}],
    FileName = filename:join(?config(priv_dir, Config),"basic_spec.spec"),
    {ok,Dev} = file:open(FileName, [write]),
    [io:format(Dev, "~p.~n", [Term]) || Term <- TestSpec],
    file:close(Dev),

    {Opts,ERPid} = setup([{spec,FileName}], Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(basic_spec, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(basic_spec),
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

test_events(basic_flag) ->
    [
     {ct_test_support_eh,start_logging,{'DEF','RUNDIR'}},
     {ct_test_support_eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {ct_test_support_eh,start_info,{1,1,3}},
     {ct_test_support_eh,tc_start,{babbling_SUITE,init_per_suite}},
     {ct_test_support_eh,tc_done,{babbling_SUITE,init_per_suite,ok}},
     {ct_test_support_eh,test_stats,{1,1,{1,0}}},
     {ct_test_support_eh,tc_start,{babbling_SUITE,end_per_suite}},
     {ct_test_support_eh,tc_done,{babbling_SUITE,end_per_suite,ok}},
     {ct_test_support_eh,test_done,{'DEF','STOP_TIME'}},
     {ct_test_support_eh,stop_logging,[]}
    ];

test_events(basic_spec) ->
    [
     {ct_test_support_eh,start_logging,{'DEF','RUNDIR'}},
     {ct_test_support_eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {ct_test_support_eh,start_info,{1,1,3}},
     {ct_test_support_eh,tc_start,{babbling_SUITE,init_per_suite}},
     {ct_test_support_eh,tc_done,{babbling_SUITE,init_per_suite,ok}},
     {ct_test_support_eh,test_stats,{1,1,{1,0}}},
     {ct_test_support_eh,tc_start,{babbling_SUITE,end_per_suite}},
     {ct_test_support_eh,tc_done,{babbling_SUITE,end_per_suite,ok}},
     {ct_test_support_eh,test_done,{'DEF','STOP_TIME'}},
     {ct_test_support_eh,stop_logging,[]}
    ].
