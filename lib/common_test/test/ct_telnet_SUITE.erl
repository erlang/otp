%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2013. All Rights Reserved.
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
%%% File: ct_telnet_SUITE
%%%
%%% Description:
%%% Edit your ts.unix.config or ts.win32.config before runnings these tests
%%% Test ct_telnet_SUITE module
%%%
%%%-------------------------------------------------------------------
-module(ct_telnet_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

-define(erl_telnet_server_port,1234).
-define(erl_telnet_server_user,"telnuser").
-define(erl_telnet_server_pwd,"telnpwd").

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

init_per_testcase(TestCase, Config) when TestCase=/=unix_telnet->
    TS = telnet_server:start([{port,?erl_telnet_server_port},
			      {users,[{?erl_telnet_server_user,
				       ?erl_telnet_server_pwd}]}]),
    ct_test_support:init_per_testcase(TestCase, [{telnet_server,TS}|Config]);
init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    case ?config(telnet_server,Config) of
	undefined -> ok;
	TS -> telnet_server:stop(TS)
    end,
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     unix_telnet,
     own_server,
     timetrap
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
unix_telnet(Config) when is_list(Config) ->
    all_tests_in_suite(unix_telnet,"ct_telnet_basic_SUITE","telnet.cfg",Config).

own_server(Config) ->
    all_tests_in_suite(own_server,"ct_telnet_own_server_SUITE",
		       "telnet2.cfg",Config).

timetrap(Config) ->
    all_tests_in_suite(timetrap,"ct_telnet_timetrap_SUITE",
		       "telnet3.cfg",Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

all_tests_in_suite(TestCase, SuiteName, CfgFileName, Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, SuiteName),
    CfgFile = filename:join(DataDir, CfgFileName),
    Cfg = telnet_config(TestCase),
    ok = file:write_file(CfgFile, io_lib:write(Cfg) ++ "."),
    {Opts,ERPid} = setup([{suite,Suite},
			  {label,TestCase},
			  {config,CfgFile}],
			 Config),
    ok = execute(TestCase, Opts, ERPid, Config).

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

    TestEvents = events_to_check(Name,Config),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).


telnet_config(unix_telnet) ->
    {unix, ct:get_config(unix)};
telnet_config(_) ->
    {unix,[{telnet,"localhost"},
	   {port, ?erl_telnet_server_port},
	   {username,?erl_telnet_server_user},
	   {password,?erl_telnet_server_pwd},
	   {keep_alive,true}]}.

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
events_to_check(unix_telnet,Config) ->
    all_cases(ct_telnet_basic_SUITE,Config);
events_to_check(own_server,Config) ->
    all_cases(ct_telnet_own_server_SUITE,Config);
events_to_check(timetrap,_Config) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_done,{ct_telnet_timetrap_SUITE,expect_timetrap,
		   {failed,{timetrap_timeout,7000}}}},
     {?eh,tc_done,{ct_telnet_timetrap_SUITE,expect_success,ok}},
     {?eh,stop_logging,[]}].

all_cases(Suite,Config) ->
    {module,_} = code:load_abs(filename:join(?config(data_dir,Config),
					     Suite)),
    TCs = Suite:all(),
    code:purge(Suite),
    code:delete(Suite),

    OneTest =
	[{?eh,start_logging,{'DEF','RUNDIR'}}] ++
	[{?eh,tc_done,{Suite,TC,ok}} || TC <- TCs] ++
	[{?eh,stop_logging,[]}],

    %% 2 tests (ct:run_test + script_start) is default
    OneTest ++ OneTest.
