%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2012. All Rights Reserved.
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
%%% File: ct_verbosity_SUITE
%%%
%%% Description: 
%%% Test that verbosity levels vs the importance parameter works as
%%% expected.
%%%
%%%-------------------------------------------------------------------
-module(ct_verbosity_SUITE).

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
    [
     no_levels,
     general_level_low,
     general_level_std,
     general_level_hi,
     change_default,
     combine_categories,
     testspec_only,
     merge_with_testspec
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
no_levels(Config) ->
    TC = no_levels,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "io_test_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC}], Config),
    ok = execute(TC, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%%
general_level_low(Config) ->
    TC = general_level_low,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "io_test_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC},
			  {verbosity,0}], Config),
    ok = execute(TC, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%%
general_level_std(Config) ->
    TC = general_level_std,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "io_test_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC},
			  {verbosity,50}], Config),
    ok = execute(TC, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%%
general_level_hi(Config) ->
    TC = general_level_high,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "io_test_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC},
			  {verbosity,100}], Config),
    ok = execute(TC, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%%
change_default(Config) ->
    TC = change_default,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "io_test_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC},
			  {verbosity,[{default,49}]}], Config),
    ok = execute(TC, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%%
combine_categories(Config) ->
    TC = combine_categories,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "io_test_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC},
			  {verbosity,[{error,?HI_VERBOSITY},
				      {default,?LOW_VERBOSITY}]}], Config),
    ok = execute(TC, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%%
testspec_only(Config) ->
    TC = testspec_only,
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    TestSpec = [{verbosity,[{default,1},{error,75},100]},
		{suites,DataDir,[io_test_SUITE]},
		{label,TC}],

    TestSpecName = ct_test_support:write_testspec(TestSpec, PrivDir,
						  "verbosity_1_spec"),
    {Opts,ERPid} = setup([{spec,TestSpecName}], Config),

    ok = execute(TC, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%%
merge_with_testspec(Config) ->
    TC = merge_with_testspec,
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    TestSpec = [{verbosity,[{default,100},{error,100}]},
		{suites,DataDir,[io_test_SUITE]},
		{label,TC}],

    TestSpecName = ct_test_support:write_testspec(TestSpec, PrivDir,
						  "verbosity_2_spec"),

    %% below should override verbosity categories in testspec
    {Opts,ERPid} = setup([{spec,TestSpecName},
			  {verbosity,[{default,0},0]}],
			 Config),

    ok = execute(TC, Opts, ERPid, Config).

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

    TestEvents = events_to_check(Name),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

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
     {?eh,tc_done,{io_test_SUITE,tc1,ok}},
     {?eh,tc_done,{io_test_SUITE,tc2,ok}},
     {?eh,tc_done,{io_test_SUITE,tc3,ok}},

     {parallel,
      [
       {?eh,tc_start,{io_test_SUITE,tc1}},
       {?eh,tc_start,{io_test_SUITE,tc2}},
       {?eh,tc_start,{io_test_SUITE,tc3}},
       {?eh,tc_done,{io_test_SUITE,tc1,ok}},
       {?eh,tc_done,{io_test_SUITE,tc2,ok}},
       {?eh,tc_done,{io_test_SUITE,tc3,ok}},
       {parallel,
	[
	 {?eh,tc_start,{io_test_SUITE,tc1}},
	 {?eh,tc_start,{io_test_SUITE,tc2}},
	 {?eh,tc_start,{io_test_SUITE,tc3}},
	 {?eh,tc_done,{io_test_SUITE,tc1,ok}},
	 {?eh,tc_done,{io_test_SUITE,tc2,ok}},
	 {?eh,tc_done,{io_test_SUITE,tc3,ok}},
	 {?eh,test_stats,{9,0,{0,0}}}
	]}
       ]},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].

