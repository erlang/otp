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
%%% File: ct_repeat_1_SUITE.erl
%%%
%%% Description:
%%% Test some simple test case group scenarios with repeat.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_repeat_1_SUITE).

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
    [];

all(suite) ->
    [repeat_cs,
     repeat_cs_and_grs,
     repeat_seq_1,
     repeat_seq_2,
     repeat_cs_until_any_ok,
     repeat_cs_until_any_fail,
     repeat_cs_until_all_ok,
     repeat_cs_until_all_fail
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

repeat_cs(Config) when is_list(Config) ->
    execute(repeat_cs,
	    "repeat_1_SUITE", repeat_cs,
	    Config).
%%%-------------------------------------------------------------------
repeat_cs_and_grs(Config) when is_list(Config) ->
    execute(repeat_cs_and_grs,
	    "repeat_1_SUITE", repeat_cs_and_grs,
	    Config).
%%%-------------------------------------------------------------------
repeat_seq(Config) when is_list(Config) ->
    execute(repeat_seq,
	    "repeat_1_SUITE", repeat_seq,
	    Config).
%%%-------------------------------------------------------------------
repeat_cs_until_any_ok(Config) when is_list(Config) ->
    execute(repeat_cs_until_any_ok,
	    "repeat_1_SUITE", repeat_cs_until_any_ok,
	    Config).
%%%-------------------------------------------------------------------
repeat_gr_until_any_ok(Config) when is_list(Config) ->
    execute(repeat_gr_until_any_ok,
	    "repeat_1_SUITE", repeat_gr_until_any_ok,
	    Config).
%%%-------------------------------------------------------------------
repeat_cs_until_any_fail(Config) when is_list(Config) ->
    execute(repeat_cs_until_any_fail,
	    "repeat_1_SUITE", repeat_cs_until_any_fail,
	    Config).
%%%-------------------------------------------------------------------
repeat_gr_until_any_fail(Config) when is_list(Config) ->
    execute(repeat_gr_until_any_fail,
	    "repeat_1_SUITE", repeat_gr_until_any_fail,
	    Config).
%%%-------------------------------------------------------------------
repeat_cs_until_all_ok(Config) when is_list(Config) ->
    execute(repeat_cs_until_all_ok,
	    "repeat_1_SUITE", repeat_cs_until_all_ok,
	    Config).
%%%-------------------------------------------------------------------
repeat_gr_until_all_ok(Config) when is_list(Config) ->
    execute(repeat_gr_until_all_ok,
	    "repeat_1_SUITE", repeat_gr_until_all_ok,
	    Config).
%%%-------------------------------------------------------------------
repeat_cs_until_all_fail(Config) when is_list(Config) ->
    execute(repeat_cs_until_all_fail,
	    "repeat_1_SUITE", repeat_cs_until_all_fail,
	    Config).
%%%-------------------------------------------------------------------
repeat_gr_until_all_fail(Config) when is_list(Config) ->
    execute(repeat_gr_until_all_fail,
	    "repeat_1_SUITE", repeat_gr_until_all_fail,
	    Config).
%%%-------------------------------------------------------------------
repeat_seq_until_any_fail(Config) when is_list(Config) ->
    execute(repeat_seq_until_any_fail,
	    "repeat_1_SUITE", repeat_seq_until_any_fail,
	    Config).
%%%-------------------------------------------------------------------
repeat_shuffled_seq_until_any_fail(Config) when is_list(Config) ->
    execute(repeat_shuffled_seq_until_any_fail,
	    "repeat_1_SUITE", repeat_shuffled_seq_until_any_fail,
	    Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
execute(TestCase, SuiteName, Group, Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, SuiteName),

    {Opts,ERPid} = setup([{suite,Suite},{group,Group},{label,TestCase}], Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(TestCase,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(TestCase),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}} | Test],
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

test_events(repeat_cs) ->
    [];

test_events(repeat_cs_and_grs) ->
    [];

test_events(repeat_seq_1) ->
    [];

test_events(repeat_seq_2) ->
    [];

test_events(repeat_cs_until_any_ok) ->
    [];

test_events(repeat_cs_until_any_fail) ->
    [];

test_events(repeat_cs_until_all_ok) ->
    [];

test_events(repeat_cs_until_all_fail) ->
    [].
