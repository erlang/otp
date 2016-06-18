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
%%% File: ct_netconfc_SUITE
%%%
%%% Description:
%%% Test ct_netconfc module
%%%
%%%-------------------------------------------------------------------
-module(ct_netconfc_SUITE).

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
    case check_crypto_and_ssh() of
	ok ->
	    ct_test_support:init_per_suite(Config);
	Skip ->
	    Skip
    end.

check_crypto_and_ssh() ->
    (catch code:load_file(crypto)),
    case code:is_loaded(crypto) of
	{file,_} ->
	    case catch ssh:start() of
		Ok when Ok==ok; Ok=={error,{already_started,ssh}} ->
		    ct:log("ssh started",[]),
		    ok;
		Other ->
		    ct:log("could not start ssh: ~p",[Other]),
		    {skip, "SSH could not be started!"}
	    end;
	Other ->
	    ct:log("could not load crypto: ~p",[Other]),
	    {skip, "crypto could not be loaded!"}
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
     netconfc1_SUITE,
     netconfc_remote_SUITE
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
netconfc1_SUITE(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "netconfc1_SUITE"),
    CfgFile = filename:join(DataDir, "netconfc1.cfg"),
    {Opts,ERPid} = setup([{suite,Suite},{config,CfgFile},
			  {label,netconfc1_SUITE}], Config),

    ok = execute(netconfc1_SUITE, Opts, ERPid, Config).

netconfc_remote_SUITE(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "netconfc_remote_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,netconfc_remote_SUITE}], Config),

    ok = execute(netconfc_remote_SUITE, Opts, ERPid, Config).


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

    TestEvents = events_to_check(Name,Config),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
events_to_check(Suite,Config) ->
    {module,_} = code:load_abs(filename:join(?config(data_dir,Config),Suite)),
    TCs = Suite:all(),
    code:purge(Suite),
    code:delete(Suite),

    OneTest =
	[{?eh,start_logging,{'DEF','RUNDIR'}}] ++
	[{?eh,tc_done,{Suite,TC,ok}} || TC <- TCs] ++
	[{?eh,stop_logging,[]}],

    %% 2 tests (ct:run_test + script_start) is default
    OneTest ++ OneTest.
