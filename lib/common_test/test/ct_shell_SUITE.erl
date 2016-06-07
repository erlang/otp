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
%%% File: ct_shell_SUITE
%%%
%%% Description:
%%% Test that the interactive mode starts properly
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_shell_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
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
    [start_interactive].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
start_interactive(Config) ->
    DataDir = ?config(data_dir, Config),
    CfgFile = filename:join(DataDir, "cfgdata"),

    {Opts,ERPid} = setup([{interactive_mode,true},{config,CfgFile}],
			 Config),
    CTNode = proplists:get_value(ct_node, Config),
    Level = proplists:get_value(trace_level, Config),
    test_server:format(Level, "Saving start opts on ~p: ~p~n",
		       [CTNode, Opts]),
    rpc:call(CTNode, application, set_env,
	     [common_test, run_test_start_opts, Opts]),
    test_server:format(Level, "Calling ct_run:script_start() on ~p~n",
		       [CTNode]),

    interactive_mode = rpc:call(CTNode, ct_run, script_start, []),

    ok = rpc:call(CTNode, ct, require, [key1]),
    value1 = rpc:call(CTNode, ct, get_config, [key1]),
    ok = rpc:call(CTNode, ct, require, [x,key2]),
    value2 = rpc:call(CTNode, ct, get_config, [x]),

    ok = rpc:call(CTNode, ct, stop_interactive, []),

    case rpc:call(CTNode, erlang, whereis, [ct_util_server]) of
	undefined ->
	    ok;
	_ ->
	    test_server:format(Level,
			       "ct_util_server not stopped on ~p yet, waiting 5 s...~n",
			       [CTNode]),
	    ct:sleep(5000),
	    undefined = rpc:call(CTNode, erlang, whereis, [ct_util_server])
    end,
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(start_interactive,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),
    TestEvents = test_events(start_interactive),
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

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------

test_events(start_interactive) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
