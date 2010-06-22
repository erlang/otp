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
%%% File: ct_testspec_1_SUITE
%%%
%%% Description:
%%% Test test specifications
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_testspec_1_SUITE).

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
    ["Run smoke tests of Common Test."];

all(suite) ->
    [all_suites, skip_all_suites,
     suite, skip_suite].
%     cases_1, skip_cases_1,
%     groups_1, skip_groups_1].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%

all_suites(Config) when is_list(Config) ->
    Self = all_suites,
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{suites,TestDir,all}],
    SpecFile = create_spec_file(?config(priv_dir, Config),
				Self, TestSpec),

    {Opts,ERPid} = setup({spec,SpecFile}, Config),
    ok = ct_test_support:run(Opts, Config),
    ok = ct_test_support:run(ct, run_testspec, [TestSpec], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Self,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(Self),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

skip_all_suites(Config) when is_list(Config) ->
    Self = skip_all_suites,
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{suites,TestDir,all},
		{skip_suites,TestDir,all,"SKIPPED!"}],
    SpecFile = create_spec_file(?config(priv_dir, Config),
				Self, TestSpec),

    {Opts,ERPid} = setup({spec,SpecFile}, Config),
    ok = ct_test_support:run(Opts, Config),
    ok = ct_test_support:run(ct, run_testspec, [TestSpec], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Self,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(Self),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%%

suite(Config) when is_list(Config) ->
    Self = suite,
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{suites,TestDir,simple_1_SUITE}],
    SpecFile = create_spec_file(?config(priv_dir, Config),
				Self, TestSpec),

    {Opts,ERPid} = setup({spec,SpecFile}, Config),
    ok = ct_test_support:run(Opts, Config),
    ok = ct_test_support:run(ct, run_testspec, [TestSpec], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Self,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(Self),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

skip_suite(Config) when is_list(Config) ->
    Self = skip_suite,
    DataDir = ?config(data_dir, Config),

    TestDir = filename:join(DataDir, "suites_1"),
    TestSpec = [{suites,TestDir,[simple_1_SUITE,simple_2_SUITE]},
		{skip_suites,TestDir,simple_1_SUITE,"SKIPPED!"}],
    SpecFile = create_spec_file(?config(priv_dir, Config),
				Self, TestSpec),

    {Opts,ERPid} = setup({spec,SpecFile}, Config),
    ok = ct_test_support:run(Opts, Config),
    ok = ct_test_support:run(ct, run_testspec, [TestSpec], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Self,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(Self),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).


%%%-----------------------------------------------------------------
%%%

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

create_spec_file(SpecDir, TCName, TestSpec) ->
    FileName = filename:join(SpecDir,
			     atom_to_list(TCName)++".spec"),
    {ok,Dev} = file:open(FileName, [write]),
    [io:format(Dev, "~p.~n", [Term]) || Term <- TestSpec],
    file:close(Dev),
    io:format("~nTest spec created here~n~n<a href=\"file://~s\">~s</a>~n",
	      [FileName,FileName]),
    FileName.

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [Test,{event_handler,{?eh,EvHArgs}}],
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

test_events(_) ->
    [
    ].
