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
%%% File: ct_error_SUITE
%%%
%%% Description: 
%%% Test various errors in Common Test suites.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_suite_callback_SUITE).

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
    DataDir = ?config(data_dir, Config),
    TestDir = filename:join(DataDir,"scb/tests/"),
    SCBs = filelib:wildcard(filename:join(TestDir,"*_scb.erl")),
    io:format("SCBs: ~p",[SCBs]),
    [io:format("Compiling ~p: ~p",
	    [FileName,compile:file(FileName,[{outdir,TestDir},debug_info])]) ||
	FileName <- SCBs],
    ct_test_support:init_per_suite([{path_dirs,[TestDir]} | Config]).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).


suite() ->
    [{timetrap,{minutes,1}}].

all() ->
    all(suite).

all(suite) -> 
    [
     empty
    ].
     

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
empty(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suites = [filename:join(DataDir,"scb/tests/ct_scb_empty_SUITE.erl")],
    {Opts,ERPid} = setup([{suite,Suites},
			  {suite_callbacks,[empty_scb]}], Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(empty_scb, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(empty),
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

test_events(empty) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{empty_scb,init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{empty_scb,pre_init_per_suite,[ct_scb_empty_SUITE,[]]}},
     {?eh,scb,{empty_scb,post_init_per_suite,[ct_scb_empty_SUITE,[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_scb_empty_SUITE,test_case}},
     {?eh,scb,{empty_scb,pre_init_per_testcase,[test_case,[]]}},
     {?eh,scb,{empty_scb,post_end_per_testcase,[test_case,[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,test_case,ok}},
     
     {?eh,tc_start,{ct_scb_empty_SUITE,end_per_suite}},
     {?eh,scb,{empty_scb,pre_end_per_suite,[ct_scb_empty_SUITE,[]]}},
     {?eh,scb,{empty_scb,post_end_per_suite,[ct_scb_empty_SUITE,[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{empty_scb,terminate,[[]]}},
     {?eh,stop_logging,[]}
    ].
