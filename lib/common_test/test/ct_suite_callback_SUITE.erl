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
    [{timetrap,{seconds,15}}].

all() ->
    all(suite).

all(suite) -> 
    %%    lists:reverse(
      [
       one_scb, two_scb, faulty_scb_no_init, faulty_scb_exit_in_init,
       faulty_scb_exit_in_init_scope_suite, minimal_scb, 
       minimal_and_maximal_scb, faulty_scb_undef, scope_per_suite_scb,
       scope_per_group_scb, scope_suite_scb,
       fail_pre_suite_scb, fail_post_suite_scb, skip_pre_suite_scb,
       skip_post_suite_scb, recover_post_suite_scb, update_config_scb,
       state_update_scb
      ]
    %%)
	.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
one_scb(Config) when is_list(Config) -> 
    do_test(one_empty_scb, "ct_scb_empty_SUITE.erl",[empty_scb], Config).

two_scb(Config) when is_list(Config) -> 
    do_test(two_empty_scb, "ct_scb_empty_SUITE.erl",[empty_scb,empty_scb],
	    Config).

faulty_scb_no_init(Config) when is_list(Config) ->
    do_test(faulty_scb_no_init, "ct_scb_empty_SUITE.erl",[askjhdkljashdkaj],
	    Config,{error,"Failed to start SCB, see the "
		   "CT Log for details"}).

minimal_scb(Config) when is_list(Config) ->
    do_test(minimal_scb, "ct_scb_empty_SUITE.erl",[minimal_scb],Config).

minimal_and_maximal_scb(Config) when is_list(Config) ->
    do_test(minimal_and_maximal_scb, "ct_scb_empty_SUITE.erl",
	    [minimal_scb, empty_scb],Config).
    
faulty_scb_undef(Config) when is_list(Config) ->
    do_test(faulty_scb_undef, "ct_scb_empty_SUITE.erl",
	    [undef_scb],Config).

faulty_scb_exit_in_init_scope_suite(Config) when is_list(Config) ->
    do_test(faulty_scb_exit_in_init_scope_suite, 
	    "ct_exit_in_init_scope_suite_scb_SUITE.erl",
	    [],Config).

faulty_scb_exit_in_init(Config) when is_list(Config) ->
    do_test(faulty_scb_exit_in_init, "ct_scb_empty_SUITE.erl",
	    [crash_init_scb], Config,
	    {error,"Failed to start SCB, see the "
	     "CT Log for details"}).

scope_per_suite_scb(Config) when is_list(Config) ->
    do_test(scope_per_suite_scb, "ct_scope_per_suite_scb_SUITE.erl",
	    [],Config).

scope_suite_scb(Config) when is_list(Config) ->
    do_test(scope_suite_scb, "ct_scope_suite_scb_SUITE.erl",
	    [],Config).

scope_per_group_scb(Config) when is_list(Config) ->
    do_test(scope_per_group_scb, "ct_scope_per_group_scb_SUITE.erl",
	    [],Config).

fail_pre_suite_scb(Config) ->
    do_test(fail_pre_suite_scb, "ct_scb_empty_SUITE.erl",
	    [fail_pre_suite_scb],Config).

fail_post_suite_scb(Config) ->
    do_test(fail_post_suite_scb, "ct_scb_empty_SUITE.erl",
	    [fail_post_suite_scb],Config).

skip_pre_suite_scb(Config) ->
    do_test(skip_pre_suite_scb, "ct_scb_empty_SUITE.erl",
	    [skip_pre_suite_scb],Config).

skip_post_suite_scb(Config) ->
    do_test(skip_post_suite_scb, "ct_scb_empty_SUITE.erl",
	    [skip_post_suite_scb],Config).

recover_post_suite_scb(Config) ->
    do_test(recover_post_suite_scb, "ct_scb_fail_per_suite_SUITE.erl",
	    [recover_post_suite_scb],Config).

update_config_scb(Config) ->
    do_test(update_config_scb, "ct_update_config_SUITE.erl",
	    [update_config_scb],Config).

state_update_scb(Config) ->
    do_test(state_update_scb, "ct_scb_fail_one_skip_one_SUITE.erl",
	    [state_update_scb,state_update_scb],Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

do_test(Tag, SWC, SCBs, Config) ->
    do_test(Tag, SWC, SCBs, Config, ok).
do_test(Tag, SWC, SCBs, Config, {error,_} = Res) ->
    do_test(Tag, SWC, SCBs, Config, Res, 1);
do_test(Tag, SWC, SCBs, Config, Res) ->
    do_test(Tag, SWC, SCBs, Config, Res, 2).

do_test(Tag, SuiteWildCard, SCBs, Config, Res, EC) ->
    
    DataDir = ?config(data_dir, Config),
    Suites = filelib:wildcard(
	       filename:join([DataDir,"scb/tests",SuiteWildCard])),
    {Opts,ERPid} = setup([{suite,Suites},
			  {suite_callbacks,SCBs},{label,Tag}], Config),
    Res = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Tag, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(Tag, EC),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

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

test_events(one_empty_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{empty_scb,init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{empty_scb,pre_init_per_suite,
	       [ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{empty_scb,post_init_per_suite,
	       [ct_scb_empty_SUITE,'$proplist','$proplist',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_scb_empty_SUITE,test_case}},
     {?eh,scb,{empty_scb,pre_init_per_testcase,[test_case,'$proplist',[]]}},
     {?eh,scb,{empty_scb,post_end_per_testcase,[test_case,'$proplist','_',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,test_case,ok}},
     
     {?eh,tc_start,{ct_scb_empty_SUITE,end_per_suite}},
     {?eh,scb,{empty_scb,pre_end_per_suite,
	       [ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{empty_scb,post_end_per_suite,[ct_scb_empty_SUITE,'$proplist','_',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{empty_scb,terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(two_empty_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scb_empty_SUITE,'$proplist','$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scb_empty_SUITE,'$proplist','$proplist',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_scb_empty_SUITE,test_case}},
     {?eh,scb,{'_',pre_init_per_testcase,[test_case,'$proplist',[]]}},
     {?eh,scb,{'_',pre_init_per_testcase,[test_case,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_testcase,[test_case,'$proplist',ok,[]]}},
     {?eh,scb,{'_',post_end_per_testcase,[test_case,'$proplist',ok,[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,test_case,ok}},
     
     {?eh,tc_start,{ct_scb_empty_SUITE,end_per_suite}},
     {?eh,scb,{'_',pre_end_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',pre_end_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_suite,[ct_scb_empty_SUITE,'$proplist','_',[]]}},
     {?eh,scb,{'_',post_end_per_suite,[ct_scb_empty_SUITE,'$proplist','_',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{'_',terminate,[[]]}},
     {?eh,scb,{'_',terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(faulty_scb_no_init) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(minimal_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_scb_empty_SUITE,test_case}},
     {?eh,tc_done,{ct_scb_empty_SUITE,test_case,ok}},
     
     {?eh,tc_start,{ct_scb_empty_SUITE,end_per_suite}},
     {?eh,tc_done,{ct_scb_empty_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(minimal_and_maximal_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scb_empty_SUITE,'$proplist','$proplist',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_scb_empty_SUITE,test_case}},
     {?eh,scb,{'_',pre_init_per_testcase,[test_case,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_testcase,[test_case,'$proplist',ok,[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,test_case,ok}},
     
     {?eh,tc_start,{ct_scb_empty_SUITE,end_per_suite}},
     {?eh,scb,{'_',pre_end_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_suite,[ct_scb_empty_SUITE,'$proplist','_',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{'_',terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(faulty_scb_undef) ->
    FailReasonStr = "undef_scb:pre_init_per_suite/3 SCB call failed",
    FailReason = {ct_scb_empty_SUITE,init_per_suite,
		  {failed,FailReasonStr}},
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,
		  {failed, {error,FailReasonStr}}}},
     {?eh,scb,{'_',on_tc_fail,'_'}},

     {?eh,tc_auto_skip,{ct_scb_empty_SUITE,test_case,
			{failed, FailReason}}},
     {?eh,scb,{'_',on_tc_skip,'_'}},
     
     {?eh,tc_auto_skip,{ct_scb_empty_SUITE,end_per_suite,
			{failed, FailReason}}},
     {?eh,scb,{'_',on_tc_skip,'_'}},
     
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(faulty_scb_exit_in_init_scope_suite) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',init_per_suite}},
     {?eh,scb,{empty_scb,init,[[]]}},
     {?eh,tc_done,
      {ct_exit_in_init_scope_suite_scb_SUITE,init_per_suite,
       {failed,
	{error,
	 "Failed to start SCB, see the CT Log for details"}}}},
     {?eh,tc_auto_skip,
      {ct_exit_in_init_scope_suite_scb_SUITE,test_case,
       {failed,
	{ct_exit_in_init_scope_suite_scb_SUITE,init_per_suite,
	 {failed,
	  "Failed to start SCB, see the CT Log for details"}}}}},
     {?eh,tc_auto_skip,
      {ct_exit_in_init_scope_suite_scb_SUITE,end_per_suite,
       {failed,
	{ct_exit_in_init_scope_suite_scb_SUITE,init_per_suite,
	 {failed,
	  "Failed to start SCB, see the CT Log for details"}}}}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(faulty_scb_exit_in_init) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{empty_scb,init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(scope_per_suite_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scope_per_suite_scb_SUITE,init_per_suite}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scope_per_suite_scb_SUITE,'$proplist','$proplist',[]]}},
     {?eh,tc_done,{ct_scope_per_suite_scb_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_scope_per_suite_scb_SUITE,test_case}},
     {?eh,scb,{'_',pre_init_per_testcase,[test_case,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_testcase,[test_case,'$proplist',ok,[]]}},
     {?eh,tc_done,{ct_scope_per_suite_scb_SUITE,test_case,ok}},
     
     {?eh,tc_start,{ct_scope_per_suite_scb_SUITE,end_per_suite}},
     {?eh,scb,{'_',pre_end_per_suite,
	       [ct_scope_per_suite_scb_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_suite,[ct_scope_per_suite_scb_SUITE,'$proplist','_',[]]}},
     {?eh,scb,{'_',terminate,[[]]}},
     {?eh,tc_done,{ct_scope_per_suite_scb_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(scope_suite_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scope_suite_scb_SUITE,init_per_suite}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scope_suite_scb_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scope_suite_scb_SUITE,'$proplist','$proplist',[]]}},
     {?eh,tc_done,{ct_scope_suite_scb_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_scope_suite_scb_SUITE,test_case}},
     {?eh,scb,{'_',pre_init_per_testcase,[test_case,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_testcase,[test_case,'$proplist',ok,[]]}},
     {?eh,tc_done,{ct_scope_suite_scb_SUITE,test_case,ok}},
     
     {?eh,tc_start,{ct_scope_suite_scb_SUITE,end_per_suite}},
     {?eh,scb,{'_',pre_end_per_suite,[ct_scope_suite_scb_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_end_per_suite,[ct_scope_suite_scb_SUITE,'$proplist','_',[]]}},
     {?eh,scb,{'_',terminate,[[]]}},
     {?eh,tc_done,{ct_scope_suite_scb_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(scope_per_group_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scope_per_group_scb_SUITE,init_per_suite}},
     {?eh,tc_done,{ct_scope_per_group_scb_SUITE,init_per_suite,ok}},

     [{?eh,tc_start,{ct_scope_per_group_scb_SUITE,{init_per_group,group1,[]}}},
      {?eh,scb,{'_',init,[[]]}},
      {?eh,scb,{'_',post_init_per_group,[group1,'$proplist','$proplist',[]]}},
      {?eh,tc_done,{ct_scope_per_group_scb_SUITE,{init_per_group,group1,[]},ok}},
      
      {?eh,tc_start,{ct_scope_per_group_scb_SUITE,test_case}},
      {?eh,scb,{'_',pre_init_per_testcase,[test_case,'$proplist',[]]}},
      {?eh,scb,{'_',post_end_per_testcase,[test_case,'$proplist',ok,[]]}},
      {?eh,tc_done,{ct_scope_per_group_scb_SUITE,test_case,ok}},
      
      {?eh,tc_start,{ct_scope_per_group_scb_SUITE,{end_per_group,group1,[]}}},
      {?eh,scb,{'_',pre_end_per_group,[group1,'$proplist',[]]}},
      {?eh,scb,{'_',post_end_per_group,[group1,'$proplist','_',[]]}},
      {?eh,scb,{'_',terminate,[[]]}},
      {?eh,tc_done,{ct_scope_per_group_scb_SUITE,{end_per_group,group1,[]},ok}}],
     
     {?eh,tc_start,{ct_scope_per_group_scb_SUITE,end_per_suite}},
     {?eh,tc_done,{ct_scope_per_group_scb_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(fail_pre_suite_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},

     
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scb_empty_SUITE,'$proplist',
					{fail,"Test failure"},[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,
                   {failed, {error,"Test failure"}}}},
     {?eh,scb,{'_',on_tc_fail,
	       [init_per_suite,{failed,"Test failure"},[]]}},

     
     {?eh,tc_auto_skip,{ct_scb_empty_SUITE,test_case,
                        {failed,{ct_scb_empty_SUITE,init_per_suite,
				 {failed,"Test failure"}}}}},
     {?eh,scb,{'_',on_tc_skip,
	       [test_case, {tc_auto_skip,
			    {failed, {ct_scb_empty_SUITE, init_per_suite,
				     {failed, "Test failure"}}}},[]]}},

     
     {?eh,tc_auto_skip, {ct_scb_empty_SUITE, end_per_suite,
                         {failed, {ct_scb_empty_SUITE, init_per_suite,
				   {failed, "Test failure"}}}}},
     {?eh,scb,{'_',on_tc_skip,
	       [end_per_suite, {tc_auto_skip,
				{failed, {ct_scb_empty_SUITE, init_per_suite,
					  {failed, "Test failure"}}}},[]]}},

     
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb, {'_',terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(fail_post_suite_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scb_empty_SUITE,'$proplist','$proplist',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,
		   {failed,{error,"Test failure"}}}},
     {?eh,scb,{'_',on_tc_fail,[init_per_suite, {failed,"Test failure"}, []]}},

     {?eh,tc_auto_skip,{ct_scb_empty_SUITE,test_case,
                        {failed,{ct_scb_empty_SUITE,init_per_suite,
				 {failed,"Test failure"}}}}},
     {?eh,scb,{'_',on_tc_skip,[test_case,{tc_auto_skip,'_'},[]]}},
     
     {?eh,tc_auto_skip, {ct_scb_empty_SUITE, end_per_suite,
                         {failed, {ct_scb_empty_SUITE, init_per_suite,
				   {failed, "Test failure"}}}}},
     {?eh,scb,{'_',on_tc_skip,[end_per_suite,{tc_auto_skip,'_'},[]]}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb, {'_',terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(skip_pre_suite_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scb_empty_SUITE,'$proplist',{skip,"Test skip"},[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,{skipped,"Test skip"}}},
     {?eh,scb,{'_',on_tc_skip,
	       [init_per_suite,{tc_user_skip,{skipped,"Test skip"}},[]]}},

     {?eh,tc_auto_skip,{ct_scb_empty_SUITE,test_case,"Test skip"}},
     {?eh,scb,{'_',on_tc_skip,[test_case,{tc_auto_skip,"Test skip"},[]]}},
     
     {?eh,tc_auto_skip, {ct_scb_empty_SUITE, end_per_suite,"Test skip"}},
     {?eh,scb,{'_',on_tc_skip,[end_per_suite,{tc_auto_skip,"Test skip"},[]]}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb, {'_',terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(skip_post_suite_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     
     {?eh,tc_start,{ct_scb_empty_SUITE,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,[ct_scb_empty_SUITE,'$proplist',[]]}},
     {?eh,scb,{'_',post_init_per_suite,[ct_scb_empty_SUITE,'$proplist','$proplist',[]]}},
     {?eh,tc_done,{ct_scb_empty_SUITE,init_per_suite,{skipped,"Test skip"}}},
     {?eh,scb,{'_',on_tc_skip,
	       [init_per_suite,{tc_user_skip,{skipped,"Test skip"}},[]]}},

     {?eh,tc_auto_skip,{ct_scb_empty_SUITE,test_case,"Test skip"}},
     {?eh,scb,{'_',on_tc_skip,[test_case,{tc_auto_skip,"Test skip"},[]]}},
     
     {?eh,tc_auto_skip, {ct_scb_empty_SUITE, end_per_suite,"Test skip"}},
     {?eh,scb,{'_',on_tc_skip,[end_per_suite,{tc_auto_skip,"Test skip"},[]]}},
     
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{'_',terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(recover_post_suite_scb) ->
    Suite = ct_scb_fail_per_suite_SUITE,
    [
     {?eh,start_logging,'_'},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{Suite,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,[Suite,'$proplist','$proplist']}},
     {?eh,scb,{'_',post_init_per_suite,[Suite,contains([tc_status]),
					{'EXIT',{'_','_'}},[]]}},
     {?eh,tc_done,{Suite,init_per_suite,ok}},

     {?eh,tc_start,{Suite,test_case}},
     {?eh,scb,{'_',pre_init_per_testcase,
	       [test_case, not_contains([tc_status]),[]]}},
     {?eh,scb,{'_',post_end_per_testcase,
	       [test_case, contains([tc_status]),'_',[]]}},
     {?eh,tc_done,{Suite,test_case,ok}},
     
     {?eh,tc_start,{Suite,end_per_suite}},
     {?eh,scb,{'_',pre_end_per_suite,
	       [Suite,not_contains([tc_status]),[]]}},
     {?eh,scb,{'_',post_end_per_suite,
	       [Suite,not_contains([tc_status]),'_',[]]}},
     {?eh,tc_done,{Suite,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{'_',terminate,[[]]}},
     {?eh,stop_logging,[]}
    ];

test_events(update_config_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     
     {?eh,tc_start,{ct_update_config_SUITE,init_per_suite}},
     {?eh,scb,{'_',pre_init_per_suite,
	       [ct_update_config_SUITE,contains([]),[]]}},
     {?eh,scb,{'_',post_init_per_suite,
	       [ct_update_config_SUITE,
		'$proplist',
		contains(
			  [init_per_suite,
			   pre_init_per_suite]),
		[]]}},
     {?eh,tc_done,{ct_update_config_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ct_update_config_SUITE, {init_per_group,group1,[]}}},
     {?eh,scb,{'_',pre_init_per_group,
	       [group1,contains(
			 [post_init_per_suite,
			  init_per_suite,
			  pre_init_per_suite]),
		[]]}},
     {?eh,scb,{'_',post_init_per_group,
	       [group1,
		contains(
		  [post_init_per_suite,
		   init_per_suite,
		   pre_init_per_suite]),
		contains(
		  [init_per_group,
		   pre_init_per_group,
		   post_init_per_suite,
		   init_per_suite,
		   pre_init_per_suite]),
	       []]}},
     {?eh,tc_done,{ct_update_config_SUITE,{init_per_group,group1,[]},ok}},

     {?eh,tc_start,{ct_update_config_SUITE,test_case}},
     {?eh,scb,{'_',pre_init_per_testcase,
	       [test_case,contains(
			    [post_init_per_group,
			     init_per_group,
			     pre_init_per_group,
			     post_init_per_suite,
			     init_per_suite,
			     pre_init_per_suite]),
		[]]}},
     {?eh,scb,{'_',post_end_per_testcase,
	       [test_case,contains(
			    [init_per_testcase,
			     pre_init_per_testcase,
			     post_init_per_group,
			     init_per_group,
			     pre_init_per_group,
			     post_init_per_suite,
			     init_per_suite,
			     pre_init_per_suite]),
		ok,[]]}},
     {?eh,tc_done,{ct_update_config_SUITE,test_case,ok}},

     {?eh,tc_start,{ct_update_config_SUITE, {end_per_group,group1,[]}}},
     {?eh,scb,{'_',pre_end_per_group,
	       [group1,contains(
			 [post_init_per_group,
			  init_per_group,
			  pre_init_per_group,
			  post_init_per_suite,
			  init_per_suite,
			  pre_init_per_suite]),
		[]]}},
     {?eh,scb,{'_',post_end_per_group,
	       [group1,
		contains(
		  [pre_end_per_group,
		   post_init_per_group,
		   init_per_group,
		   pre_init_per_group,
		   post_init_per_suite,
		   init_per_suite,
		   pre_init_per_suite]),
	       ok,[]]}},
     {?eh,tc_done,{ct_update_config_SUITE,{end_per_group,group1,[]},ok}},
     
     {?eh,tc_start,{ct_update_config_SUITE,end_per_suite}},
     {?eh,scb,{'_',pre_end_per_suite,
	       [ct_update_config_SUITE,contains(
					 [post_init_per_suite,
					  init_per_suite,
					  pre_init_per_suite]),
		[]]}},
     {?eh,scb,{'_',post_end_per_suite,
	       [ct_update_config_SUITE,contains(
					 [pre_end_per_suite,
					  post_init_per_suite,
					  init_per_suite,
					  pre_init_per_suite]),
	       '_',[]]}},
     {?eh,tc_done,{ct_update_config_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{'_',terminate,[contains(
				[post_end_per_suite,
				 pre_end_per_suite,
				 post_init_per_suite,
				 init_per_suite,
				 pre_init_per_suite])]}},
     {?eh,stop_logging,[]}
    ];

test_events(state_update_scb) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,scb,{'_',init,[[]]}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',init_per_suite}},
     
     {?eh,tc_done,{'_',end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,scb,{'_',terminate,[contains(
				[post_end_per_suite,pre_end_per_suite,
				 post_end_per_group,pre_end_per_group,
				 {not_in_order,
				  [post_end_per_testcase,pre_init_per_testcase,
				   on_tc_skip,post_end_per_testcase,
				   pre_init_per_testcase,on_tc_fail,
				   post_end_per_testcase,pre_init_per_testcase]
				 },
				 post_init_per_group,pre_init_per_group,
				 post_init_per_suite,pre_init_per_suite,
				 init])]}},
     {?eh,scb,{'_',terminate,[contains(
				[post_end_per_suite,pre_end_per_suite,
				 post_end_per_group,pre_end_per_group,
				 {not_in_order,
				  [post_end_per_testcase,pre_init_per_testcase,
				   on_tc_skip,post_end_per_testcase,
				   pre_init_per_testcase,on_tc_fail,
				   post_end_per_testcase,pre_init_per_testcase]
				 },
				 post_init_per_group,pre_init_per_group,
				 post_init_per_suite,pre_init_per_suite,
				 init]
			       )]}},
     {?eh,stop_logging,[]}
    ];

test_events(ok) ->
    ok.


%% test events help functions
contains(List) ->
    fun(Proplist) when is_list(Proplist) ->
	    contains(List,Proplist)
    end.

contains([{not_in_order,List}|T],Rest) ->
    contains_parallel(List,Rest),
    contains(T,Rest);
contains([{Ele,Pos}|T] = L,[H|T2]) ->
    case element(Pos,H) of
	Ele ->
	    contains(T,T2);
	_ ->
	    contains(L,T2)
    end;
contains([Ele|T],[{Ele,_}|T2])->
    contains(T,T2);
contains([Ele|T],[Ele|T2])->
    contains(T,T2);
contains(List,[_|T]) ->
    contains(List,T);
contains([],_) ->
    match.

contains_parallel([Key | T], Elems) ->
    contains([Key],Elems),
    contains_parallel(T,Elems);
contains_parallel([],Elems) ->
    match.

not_contains(List) ->
    fun(Proplist) when is_list(Proplist) ->
	    [] = [Ele || {Ele,_} <- Proplist,
			 Test <- List,
			 Test =:= Ele]
    end.
