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
%%% File: ct_release_test_SUITE
%%%
%%% Description:
%%% Test ct_release_test module
%%%
%%%-------------------------------------------------------------------
-module(ct_release_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).
-define(suite, release_test_SUITE).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case os:type() of
	{win32,_} ->
	    {skipped, "Upgrade tests do currently not work on windows"};
	_ ->
	    ct_test_support:init_per_suite(Config)
    end.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     minor,
     major,
     major_fail_init,
     major_fail_upgraded,
     major_fail_downgraded,
     major_fail_no_init
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
minor(Config) when is_list(Config) ->
    {Suite,Cfg} = setup1(Config),
    {Opts,ERPid} = setup([{suite,Suite},
			  {testcase,minor},
			  {label,minor}|Cfg], Config),
    execute(minor, Opts, ERPid, Config).

major(Config) when is_list(Config) ->
    {Suite,Cfg} = setup1(Config),
    {Opts,ERPid} = setup([{suite,Suite},
			  {testcase,major},
			  {label,major}|Cfg], Config),
    execute(major, Opts, ERPid, Config).

major_fail_init(Config) when is_list(Config) ->
    {Suite,Cfg} = setup1(Config),
    {Opts,ERPid} = setup([{suite,Suite},
			  {testcase,major_fail_init},
			  {label,major_fail_init}|Cfg], Config),
    execute(major_fail_init, Opts, ERPid, Config).

major_fail_upgraded(Config) when is_list(Config) ->
    {Suite,Cfg} = setup1(Config),
    {Opts,ERPid} = setup([{suite,Suite},
			  {testcase,major_fail_upgraded},
			  {label,major_fail_upgraded}|Cfg], Config),
    execute(major_fail_upgraded, Opts, ERPid, Config).

major_fail_downgraded(Config) when is_list(Config) ->
    {Suite,Cfg} = setup1(Config),
    {Opts,ERPid} = setup([{suite,Suite},
			  {testcase,major_fail_downgraded},
			  {label,major_fail_downgraded}|Cfg], Config),
    execute(major_fail_downgraded, Opts, ERPid, Config).

major_fail_no_init(Config) when is_list(Config) ->
    {Suite,Cfg} = setup1(Config),
    {Opts,ERPid} = setup([{suite,Suite},
			  {testcase,major_fail_no_init},
			  {label,major_fail_no_init}|Cfg], Config),
    execute(major_fail_no_init, Opts, ERPid, Config).


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
setup1(Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, atom_to_list(?suite)),
    Cfg = case ct:get_config(otp_releases) of
	      undefined ->
		  [];
	      Rels ->
		  CfgFile = filename:join(DataDir, "release_test.cfg"),
		  file:write_file(CfgFile,
				  io_lib:format("{otp_releases,~p}.",[Rels])),
		  [{config,CfgFile}]
	  end,
    {Suite,Cfg}.

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

    verify_events(Name,Events,Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
verify_events(TC,Events,Config) ->
    Ok = expected_events(TC,ok),
    case ct_test_support:verify_events(Ok, Events, Config) of
	ok ->
	    ok;
	{event_not_found,{?eh,tc_done,{_Suite,TC,ok}}}=R1 ->
	    ct:log("Did not find 'ok', checking if skipped...",[]),
	    Skipped = expected_events(TC,{skipped,"Old release not available"}),
	    case ct_test_support:verify_events(Skipped, Events, Config) of
		ok ->
		    {skipped,"Old release not available"};
		R2 ->
		    ct:log("Did not find skipped case either: ~n~p",[R2]),
		    exit(R1)
	    end
    end.

expected_events(TC,Result) ->
    OneTest =
	[{?eh,start_logging,{'DEF','RUNDIR'}},
	 {?eh,tc_done,{?suite,TC,Result}},
	 {?eh,stop_logging,[]}],
    %% 2 tests (ct:run_test + script_start) is default
    OneTest ++ OneTest.
