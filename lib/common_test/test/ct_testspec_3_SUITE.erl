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
%%% File: ct_testspec_1_SUITE
%%%
%%% Description:
%%% Test test specifications
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_testspec_3_SUITE).

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
    Config1 = ct_test_support:init_per_suite(Config),
    SpecsDir1 = filename:join(DataDir, "specs1"),
    SpecsDir2 = filename:join(DataDir, "specs2"),
    [{specs_dir1,SpecsDir1},{specs_dir2,SpecsDir2} | Config1].

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [start_separate,
     start_join,
     incl_separate1,
     incl_separate2,
     incl_join1,
     incl_join2,
     incl_both1,
     incl_both2,
     incl_both_and_join1,
     incl_both_and_join2,
     rec_incl_separate1,
     rec_incl_separate2,
     rec_incl_join1,
     rec_incl_join2,
     rec_incl_separate_join1,
     rec_incl_separate_join2,
     rec_incl_join_separate1,
     rec_incl_join_separate2
    ].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%

start_separate(Config) ->
    Specs = [fname(specs_dir1, "flat_spec1", Config),
	     fname(specs_dir2, "flat_spec2", Config)],
    setup_and_execute(start_separate, Specs, [], Config).

%%%-----------------------------------------------------------------
%%% 

start_join(Config) ->
    Specs = [fname(specs_dir1, "flat_spec1", Config),
	     fname(specs_dir2, "flat_spec2", Config)],
    setup_and_execute(start_join, Specs, [{join_specs,true}], Config).

%%%-----------------------------------------------------------------
%%% 

incl_separate1(Config) ->
    Specs = [fname(specs_dir1, "spec_sep1", Config),
	     fname(specs_dir2, "spec_sep2", Config)],
    setup_and_execute(incl_separate1, Specs, [], Config).

incl_separate2(Config) ->
    Specs = [fname(specs_dir1, "spec_sep1", Config),
	     fname(specs_dir2, "spec_sep2", Config)],
    setup_and_execute(incl_separate2, Specs, [{join_specs,true}], Config).

%%%-----------------------------------------------------------------
%%% 

incl_join1(Config) ->
    Specs = [fname(specs_dir1, "spec_join1", Config),
	     fname(specs_dir2, "spec_join2", Config)],
    setup_and_execute(incl_join1, Specs, [], Config).

incl_join2(Config) ->
    Specs = [fname(specs_dir1, "spec_join1", Config),
	     fname(specs_dir2, "spec_join2", Config)],
    setup_and_execute(incl_join2, Specs, [{join_specs,true}], Config).

%%%-----------------------------------------------------------------
%%% 

incl_both1(Config) ->
    Specs = [fname(specs_dir1, "spec_both1", Config),
	     fname(specs_dir2, "spec_both2", Config)],
    setup_and_execute(incl_both1, Specs, [], Config).

incl_both2(Config) ->
    Specs = [fname(specs_dir1, "spec_both1", Config),
	     fname(specs_dir2, "spec_both2", Config)],
    setup_and_execute(incl_both2, Specs, [{join_specs,true}], Config).

%%%-----------------------------------------------------------------
%%% 

incl_both_and_join1(Config) ->
    Specs = [fname(specs_dir1, "spec_both_join1", Config),
	     fname(specs_dir2, "spec_both_join2", Config)],
    setup_and_execute(incl_both_and_join1, Specs, [], Config).

incl_both_and_join2(Config) ->
    Specs = [fname(specs_dir1, "spec_both_join1", Config),
	     fname(specs_dir2, "spec_both_join2", Config)],
    setup_and_execute(incl_both_and_join2, Specs, [{join_specs,true}], Config).

%%%-----------------------------------------------------------------
%%% 

rec_incl_separate1(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_sep1", Config),
	     fname(specs_dir2, "rec_spec_sep2", Config)],
    setup_and_execute(rec_incl_separate1, Specs, [], Config).

rec_incl_separate2(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_sep1", Config),
	     fname(specs_dir2, "rec_spec_sep2", Config)],
    setup_and_execute(rec_incl_separate2, Specs, [{join_specs,true}], Config).

%%%-----------------------------------------------------------------
%%% 

rec_incl_join1(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_join1", Config),
	     fname(specs_dir2, "rec_spec_join2", Config)],
    setup_and_execute(rec_incl_join1, Specs, [], Config).

rec_incl_join2(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_join1", Config),
	     fname(specs_dir2, "rec_spec_join2", Config)],
    setup_and_execute(rec_incl_join2, Specs, [{join_specs,true}], Config).


%%%-----------------------------------------------------------------
%%%

rec_incl_separate_join1(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_sep_join1", Config),
	     fname(specs_dir2, "rec_spec_sep_join2", Config)],
    setup_and_execute(rec_incl_separate_join1, Specs, [], Config).

rec_incl_separate_join2(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_sep_join1", Config),
	     fname(specs_dir2, "rec_spec_sep_join2", Config)],
    setup_and_execute(rec_incl_separate_join2, Specs, 
		      [{join_specs,true}], Config).

%%%-----------------------------------------------------------------
%%%

rec_incl_join_separate1(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_join_sep1", Config),
	     fname(specs_dir2, "rec_spec_join_sep2", Config)],
    setup_and_execute(rec_incl_join_separate1, Specs, [], Config).

rec_incl_join_separate2(Config) ->
    Specs = [fname(specs_dir1, "rec_spec_join_sep1", Config),
	     fname(specs_dir2, "rec_spec_join_sep2", Config)],
    setup_and_execute(rec_incl_join_separate2, Specs, 
		      [{join_specs,true}], Config).


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

fname(Tag, File, Config) ->
    filename:join(?config(Tag, Config), File).

check_parameter(TCID) ->
    {ok,{config,TCID}}.

read_config(TCID) ->
    {ok,[{tcname,list_to_atom(TCID)}]}.

setup_and_execute(TCName, Specs, TestOpts, Config) ->

    TestID = {userconfig,{?MODULE,atom_to_list(TCName)}},
    TestTerms = [TestID,{spec,Specs},{label,TCName}] ++ TestOpts,

    {Opts,ERPid} = setup(TestTerms, Config),

    case ct_test_support:run(Opts, Config) of
	ok ->
	    ok;
	Error ->
	    ct:pal("Error executing with opts: ~p", [Opts]),
	    exit(Error)
    end,

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(TCName,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(TCName),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

setup(Test, Config) when is_tuple(Test) ->
    setup([Test], Config);
setup(Tests, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ Tests ++ [{event_handler,{?eh,EvHArgs}}],
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


%%%!
%%%! IMPORTANT NOTE ABOUT THE TEST ORDER:
%%%!
%%%! When merging testspec terms, CT will group the tests by TestDir and
%%%! Suite, before term order (in testspec). That means that if tests
%%%! are ordered like e.g:
%%%!   {Dir1,Suite11}, {Dir2,Suite21}, {Dir1,Suite12},
%%%! the execution order after merge (even if no merge takes place),
%%%! will be:
%%%!   {Dir1,[Suite11,Suite12]}, {Dir2,Suite21}
%%%!
%%%! Also, tests in a tree of included testspecs are always collected
%%%! and merged in depth-first manner, meaning even if a particular test is
%%%! on a higher level in the tree, it may be executed later than a test on a
%%%! lower level.
%%%!

test_events(start_separate) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(start_join) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_separate1) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_separate2) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_join1) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_join2) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_both1) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_both2) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,ok_tc}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_both_and_join1) -> 
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{5,3,25}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{5,10,{5,5}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(incl_both_and_join2) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_separate1) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}] 
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},
     
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_separate2) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++	
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},
	 
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},
	
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_join1) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{5,5,25}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{5,10,{5,5}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_join2) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{5,5,25}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{5,10,{5,5}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_separate_join1) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_separate_join2) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,5}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},

     {?eh,stop_logging,[]},
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{4,4,20}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{4,8,{4,4}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_join_separate1) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++

    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(rec_incl_join_separate2) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t23_SUITE,init_per_suite}},
     {?eh,tc_done,{t23_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t23_SUITE,end_per_suite}},
     {?eh,tc_done,{t23_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec2_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}}]
	++ flat_spec1_events() ++
    [{?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}].

%%%-----------------------------------------------------------------

flat_spec1_events() ->
    [
     {?eh,start_info,{2,2,10}},
     {?eh,tc_start,{t11_SUITE,init_per_suite}},
     {?eh,tc_done,{t11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t11_SUITE,ok_tc}},
     {?eh,tc_done,{t11_SUITE,ok_tc,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{t11_SUITE,exit_tc}},
     {?eh,tc_done,{t11_SUITE,exit_tc,{failed,{error,kaboom}}}},
     {?eh,test_stats,{1,1,{0,0}}},
     {?eh,tc_start,{t11_SUITE,to_tc}},
     {?eh,tc_done,{t11_SUITE,to_tc,{failed,{timetrap_timeout,1}}}},
     {?eh,test_stats,{1,2,{0,0}}},
     {?eh,tc_start,{t11_SUITE,autoskip_tc}},
     {?eh,tc_done,
      {t11_SUITE,autoskip_tc,{auto_skipped,
			      {failed,
			       {t11_SUITE,init_per_testcase,
				{kaboom,'_'}}}}}},
     {?eh,test_stats,{1,2,{0,1}}},
     {?eh,tc_start,{t11_SUITE,userskip_tc}},
     {?eh,tc_done,{t11_SUITE,userskip_tc,{skipped,"user skipped"}}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t11_SUITE,end_per_suite}},
     {?eh,tc_done,{t11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,init_per_suite}},
     {?eh,tc_done,{t21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t21_SUITE,ok_tc}},
     {?eh,tc_done,{t21_SUITE,ok_tc,ok}},
     {?eh,test_stats,{2,2,{1,1}}},
     {?eh,tc_start,{t21_SUITE,exit_tc}},
     {?eh,tc_done,{t21_SUITE,exit_tc,{failed,{error,kaboom}}}},
     {?eh,test_stats,{2,3,{1,1}}},
     {?eh,tc_start,{t21_SUITE,to_tc}},
     {?eh,tc_done,{t21_SUITE,to_tc,{failed,{timetrap_timeout,1}}}},
     {?eh,test_stats,{2,4,{1,1}}},
     {?eh,tc_start,{t21_SUITE,autoskip_tc}},
     {?eh,tc_done,
      {t21_SUITE,autoskip_tc,{auto_skipped,
			      {failed,
			       {t21_SUITE,init_per_testcase,
				{kaboom,'_'}}}}}},
     {?eh,test_stats,{2,4,{1,2}}},
     {?eh,tc_start,{t21_SUITE,userskip_tc}},
     {?eh,tc_done,{t21_SUITE,userskip_tc,{skipped,"user skipped"}}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t21_SUITE,end_per_suite}},
     {?eh,tc_done,{t21_SUITE,end_per_suite,ok}}].

flat_spec2_events() ->
    [
     {?eh,start_info,{3,2,15}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,ok_tc}},
     {?eh,tc_done,{t12_SUITE,ok_tc,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{t12_SUITE,exit_tc}},
     {?eh,tc_done,{t12_SUITE,exit_tc,{failed,{error,kaboom}}}},
     {?eh,test_stats,{1,1,{0,0}}},
     {?eh,tc_start,{t12_SUITE,to_tc}},
     {?eh,tc_done,{t12_SUITE,to_tc,{failed,{timetrap_timeout,1}}}},
     {?eh,test_stats,{1,2,{0,0}}},
     {?eh,tc_start,{t12_SUITE,autoskip_tc}},
     {?eh,tc_done,
      {t12_SUITE,autoskip_tc,{auto_skipped,
			      {failed,
			       {t12_SUITE,init_per_testcase,
				{kaboom,'_'}}}}}},
     {?eh,test_stats,{1,2,{0,1}}},
     {?eh,tc_start,{t12_SUITE,userskip_tc}},
     {?eh,tc_done,{t12_SUITE,userskip_tc,{skipped,"user skipped"}}},
     {?eh,test_stats,{1,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,init_per_suite}},
     {?eh,tc_done,{t12_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t12_SUITE,ok_tc}},
     {?eh,tc_done,{t12_SUITE,ok_tc,ok}},
     {?eh,test_stats,{2,2,{1,1}}},
     {?eh,tc_start,{t12_SUITE,exit_tc}},
     {?eh,tc_done,{t12_SUITE,exit_tc,{failed,{error,kaboom}}}},
     {?eh,test_stats,{2,3,{1,1}}},
     {?eh,tc_start,{t12_SUITE,to_tc}},
     {?eh,tc_done,{t12_SUITE,to_tc,{failed,{timetrap_timeout,1}}}},
     {?eh,test_stats,{2,4,{1,1}}},
     {?eh,tc_start,{t12_SUITE,autoskip_tc}},
     {?eh,tc_done,
      {t12_SUITE,autoskip_tc,{auto_skipped,
			      {failed,
			       {t12_SUITE,init_per_testcase,
				{kaboom,'_'}}}}}},
     {?eh,test_stats,{2,4,{1,2}}},
     {?eh,tc_start,{t12_SUITE,userskip_tc}},
     {?eh,tc_done,{t12_SUITE,userskip_tc,{skipped,"user skipped"}}},
     {?eh,test_stats,{2,4,{2,2}}},
     {?eh,tc_start,{t12_SUITE,end_per_suite}},
     {?eh,tc_done,{t12_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,init_per_suite}},
     {?eh,tc_done,{t22_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{t22_SUITE,ok_tc}},
     {?eh,tc_done,{t22_SUITE,ok_tc,ok}},
     {?eh,test_stats,{3,4,{2,2}}},
     {?eh,tc_start,{t22_SUITE,exit_tc}},
     {?eh,tc_done,{t22_SUITE,exit_tc,{failed,{error,kaboom}}}},
     {?eh,test_stats,{3,5,{2,2}}},
     {?eh,tc_start,{t22_SUITE,to_tc}},
     {?eh,tc_done,{t22_SUITE,to_tc,{failed,{timetrap_timeout,1}}}},
     {?eh,test_stats,{3,6,{2,2}}},
     {?eh,tc_start,{t22_SUITE,autoskip_tc}},
     {?eh,tc_done,
      {t22_SUITE,autoskip_tc,{auto_skipped,
			      {failed,
			       {t22_SUITE,init_per_testcase,
				{kaboom,'_'}}}}}},
     {?eh,test_stats,{3,6,{2,3}}},
     {?eh,tc_start,{t22_SUITE,userskip_tc}},
     {?eh,tc_done,{t22_SUITE,userskip_tc,{skipped,"user skipped"}}},
     {?eh,test_stats,{3,6,{3,3}}},
     {?eh,tc_start,{t22_SUITE,end_per_suite}},
     {?eh,tc_done,{t22_SUITE,end_per_suite,ok}}].
