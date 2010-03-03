%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
%%% File: ct_config_SUITE
%%%
%%% Description:
%%% Test configuration handling in Common Test suites.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_config_SUITE).

-compile(export_all).

-include_lib("test_server/include/test_server.hrl").
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

all(doc) ->
    [""];

all(suite) ->
    [
	require,
	nested_keys
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
require(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Join = fun(D, S) -> filename:join(D, "config/test/"++S) end,
    Suites = [Join(DataDir, "config_1_SUITE")],
    CTConfig = {config, filename:join(DataDir, "config/cfg.cfg")},
    {Opts,ERPid} = setup({suite,Suites}, Config, CTConfig),
    ok = ct_test_support:run(ct, run_test, [Opts], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(require,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = test_events(require),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

nested_keys(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Join = fun(D, S) -> filename:join(D, "config/test/"++S) end,
    Suites = [Join(DataDir, "config_2_SUITE")],
    CTConfig = {config, filename:join(DataDir, "config/cfg.cfg")},
    {Opts,ERPid} = setup({suite,Suites}, Config, CTConfig),
    ok = ct_test_support:run(ct, run_test, [Opts], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(nested_keys,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = test_events(nested_keys),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

setup(Test, Config, CTConfig) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [Test,{event_handler,{?eh,EvHArgs}}, CTConfig],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).
%reformat(Events, _EH) ->
%    Events.

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
test_events(require) ->
[
 {?eh,start_logging,{'DEF','RUNDIR'}},
 {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
 {?eh,start_info,{1,1,8}},
 {?eh,tc_start,{config_1_SUITE,init_per_suite}},
 {?eh,tc_done,{config_1_SUITE,init_per_suite,ok}},
 {?eh,tc_start,{config_1_SUITE,test1}},
 {?eh,tc_done,{config_1_SUITE,test1,ok}},
 {?eh,test_stats,{1,0,{0,0}}},
 {?eh,tc_start,{config_1_SUITE,test2}},
 {?eh,tc_done,
     {config_1_SUITE,test2,{skipped,{config_name_already_in_use,[x1]}}}},
 {?eh,test_stats,{1,0,{1,0}}},
 {?eh,tc_start,{config_1_SUITE,test3}},
 {?eh,tc_done,{config_1_SUITE,test3,ok}},
 {?eh,test_stats,{2,0,{1,0}}},
 {?eh,tc_start,{config_1_SUITE,test4}},
 {?eh,tc_done,
     {config_1_SUITE,test4,{skipped,{config_name_already_in_use,[x1,alias]}}}},
 {?eh,test_stats,{2,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,test5}},
 {?eh,tc_done,{config_1_SUITE,test5,ok}},
 {?eh,test_stats,{3,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,test6}},
 {?eh,tc_done,{config_1_SUITE,test6,ok}},
 {?eh,test_stats,{4,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,test7}},
 {?eh,tc_done,{config_1_SUITE,test7,ok}},
 {?eh,test_stats,{5,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,test8}},
 {?eh,tc_done,{config_1_SUITE,test8,ok}},
 {?eh,test_stats,{6,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,end_per_suite}},
 {?eh,tc_done,{config_1_SUITE,end_per_suite,ok}},
 {?eh,test_done,{'DEF','STOP_TIME'}},
 {?eh,stop_logging,[]}
];

test_events(nested_keys)->
[].
