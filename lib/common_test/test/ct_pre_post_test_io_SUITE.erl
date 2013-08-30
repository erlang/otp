%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%%% File: ct_pre_post_test_io_SUITE
%%%
%%% Description:
%%%
%%% Test that ct:log/2 printouts and error/progress reports that happen
%%% before or after the test run are saved in the pre/post test IO log.
%%%-------------------------------------------------------------------
-module(ct_pre_post_test_io_SUITE).

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
    DataDir = ?config(data_dir, Config),
    CTH = filename:join(DataDir, "cth_ctrl.erl"),
    ct:pal("Compiling ~p: ~p",
	   [CTH,compile:file(CTH,[{outdir,DataDir},debug_info])]),
    ct_test_support:init_per_suite([{path_dirs,[DataDir]} | Config]).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     pre_post_io
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
pre_post_io(Config) ->
    TC = pre_post_io,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "dummy_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC},{ct_hooks,cth_ctrl}], Config),
    ct_test_support:run(Opts, Config),
    ct:sleep(2000),
    ct_test_support:ct_rpc({cth_ctrl,proceed,[]}, Config),
    ct:sleep(4000),
    ct_test_support:ct_rpc({cth_ctrl,proceed,[]}, Config),
    ct:sleep(2000),
    Events = ct_test_support:get_events(ERPid, Config),
    ct_test_support:log_events(TC,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),
    TestEvents = events_to_check(TC),
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

events_to_check(_Test) ->
    [].
