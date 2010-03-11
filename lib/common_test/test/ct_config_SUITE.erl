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
	userconfig_static,
	userconfig_dynamic,
	testspec_legacy,
	testspec_static,
	testspec_dynamic
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
require(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    run_test(require,
	     Config,
	     {config, filename:join(DataDir, "config/config.txt")},
             ["config_1_SUITE"]).

userconfig_static(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    run_test(userconfig_static,
	     Config,
	     {userconfig, {ct_config_xml, filename:join(DataDir, "config/config.xml")}},
             ["config_1_SUITE"]).

userconfig_dynamic(Config) when is_list(Config) ->
    run_test(userconfig_dynamic,
	     Config,
	     {userconfig, {config_driver, "config_server"}},
             ["config_2_SUITE"]).

testspec_legacy(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    make_spec(DataDir,
		"config/spec_legacy.spec",
		[config_1_SUITE],
		[{config, filename:join(DataDir, "config/config.txt")}]),
    run_test(testspec_legacy,
	     Config,
	     {spec, filename:join(DataDir, "config/spec_legacy.spec")},
             []),
    file:delete(filename:join(DataDir, "config/spec_legacy.spec")).

testspec_static(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    make_spec(DataDir,
		"config/spec_static.spec",
		[config_1_SUITE],
		[{userconfig, {ct_config_xml, filename:join(DataDir, "config/config.xml")}}]),
    run_test(testspec_static,
	     Config,
	     {spec, filename:join(DataDir, "config/spec_static.spec")},
             []),
    file:delete(filename:join(DataDir, "config/spec_static.spec")).

testspec_dynamic(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    make_spec(DataDir, "config/spec_dynamic.spec",
		[config_2_SUITE],
		[{userconfig, {config_driver, "config_server"}}]),
    run_test(testspec_dynamic,
	     Config,
	     {spec, filename:join(DataDir, "config/spec_dynamic.spec")},
             []),
    file:delete(filename:join(DataDir, "config/spec_dynamic.spec")).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
% {suites, "ct_config_SUITE_data/config/test", config_2_SUITE}.
make_spec(DataDir, Filename, Suites, Config)->
    {ok, Fd} = file:open(filename:join(DataDir, Filename), [write]),
    ok = file:write(Fd,
		io_lib:format("{suites, \"~sconfig/test/\", ~p}.~n", [DataDir, Suites])),
    lists:foreach(fun(C)-> ok=file:write(Fd, io_lib:format("~p.~n", [C])) end, Config),
    ok = file:close(Fd).

run_test(Name, Config, CTConfig, SuiteNames)->
    DataDir = ?config(data_dir, Config),
    Joiner = fun(Suite) -> filename:join(DataDir, "config/test/"++Suite) end,
    Suites = lists:map(Joiner, SuiteNames),
    {Opts,ERPid} = setup_env({suite,Suites}, Config, CTConfig),
    ok = ct_test_support:run(ct, run_test, [Opts], Config),
    TestEvents = ct_test_support:get_events(ERPid, Config),
    ct_test_support:log_events(Name,
			       reformat_events(TestEvents, ?eh),
			       ?config(priv_dir, Config)),
    ExpEvents = expected_events(Name),
    ok = ct_test_support:verify_events(ExpEvents, TestEvents, Config).

setup_env(Test, Config, CTConfig) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [Test,{event_handler,{?eh,EvHArgs}}, CTConfig],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

reformat_events(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
expected_events(Static) when
	Static == require; Static == testspec_legacy;
	Static == userconfig_static; Static == testspec_static->
[
 {?eh,start_logging,{'DEF','RUNDIR'}},
 {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
 {?eh,start_info,{1,1,8}},
 {?eh,tc_start,{config_1_SUITE,init_per_suite}},
 {?eh,tc_done,{config_1_SUITE,init_per_suite,ok}},
 {?eh,tc_start,{config_1_SUITE,test_get_config_simple}},
 {?eh,tc_done,{config_1_SUITE,test_get_config_simple,ok}},
 {?eh,test_stats,{1,0,{0,0}}},
 {?eh,tc_start,{config_1_SUITE,test_get_config_nested}},
 {?eh,tc_done,{config_1_SUITE,test_get_config_nested,ok}},
 {?eh,test_stats,{2,0,{0,0}}},
 {?eh,tc_start,{config_1_SUITE,test_default_suitewide}},
 {?eh,tc_done,{config_1_SUITE,test_default_suitewide,ok}},
 {?eh,test_stats,{3,0,{0,0}}},
 {?eh,tc_start,{config_1_SUITE,test_config_name_already_in_use1}},
 {?eh,tc_done,
     {config_1_SUITE,test_config_name_already_in_use1,{skipped,{config_name_already_in_use,[x1]}}}},
 {?eh,test_stats,{3,0,{1,0}}},
 {?eh,tc_start,{config_1_SUITE,test_default_tclocal}},
 {?eh,tc_done,{config_1_SUITE,test_default_tclocal,ok}},
 {?eh,test_stats,{4,0,{1,0}}},
 {?eh,tc_start,{config_1_SUITE,test_config_name_already_in_use2}},
 {?eh,tc_done,
     {config_1_SUITE,test_config_name_already_in_use2,
         {skipped,{config_name_already_in_use,[x1,alias]}}}},
 {?eh,test_stats,{4,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,test_alias_tclocal}},
 {?eh,tc_done,{config_1_SUITE,test_alias_tclocal,ok}},
 {?eh,test_stats,{5,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,test_get_config_undefined}},
 {?eh,tc_done,{config_1_SUITE,test_get_config_undefined,ok}},
 {?eh,test_stats,{6,0,{2,0}}},
 {?eh,tc_start,{config_1_SUITE,end_per_suite}},
 {?eh,tc_done,{config_1_SUITE,end_per_suite,ok}},
 {?eh,test_done,{'DEF','STOP_TIME'}},
 {?eh,stop_logging,[]}
];

expected_events(Dynamic) when Dynamic == testspec_dynamic; Dynamic == userconfig_dynamic->
[
 {ct_test_support_eh,start_logging,{'DEF','RUNDIR'}},
 {ct_test_support_eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
 {ct_test_support_eh,start_info,{1,1,3}},
 {ct_test_support_eh,tc_start,{config_2_SUITE,init_per_suite}},
 {ct_test_support_eh,tc_done,{config_2_SUITE,init_per_suite,ok}},
 {ct_test_support_eh,tc_start,{config_2_SUITE,test_get_known_variable}},
 {ct_test_support_eh,tc_done,{config_2_SUITE,test_get_known_variable,ok}},
 {ct_test_support_eh,test_stats,{1,0,{0,0}}},
 {ct_test_support_eh,tc_start,{config_2_SUITE,test_localtime_update}},
 {ct_test_support_eh,tc_done,{config_2_SUITE,test_localtime_update,ok}},
 {ct_test_support_eh,test_stats,{2,0,{0,0}}},
 {ct_test_support_eh,tc_start,{config_2_SUITE,test_server_pid}},
 {ct_test_support_eh,tc_done,{config_2_SUITE,test_server_pid,ok}},
 {ct_test_support_eh,test_stats,{3,0,{0,0}}},
 {ct_test_support_eh,tc_start,{config_2_SUITE,end_per_suite}},
 {ct_test_support_eh,tc_done,{config_2_SUITE,end_per_suite,ok}},
 {ct_test_support_eh,test_done,{'DEF','STOP_TIME'}},
 {ct_test_support_eh,stop_logging,[]}
].