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
-module(ct_error_SUITE).

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
    [""];

all(suite) -> 
    [
     cfg_error,
     lib_error,
     no_compile
    ].
     

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
cfg_error(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Join = fun(D, S) -> filename:join(D, "error/test/"++S) end,
    Suites = [Join(DataDir, "cfg_error_1_SUITE"),
	      Join(DataDir, "cfg_error_2_SUITE"),
	      Join(DataDir, "cfg_error_3_SUITE"),
	      Join(DataDir, "cfg_error_4_SUITE"),
	      Join(DataDir, "cfg_error_5_SUITE"),
	      Join(DataDir, "cfg_error_6_SUITE"),
	      Join(DataDir, "cfg_error_7_SUITE"),
	      Join(DataDir, "cfg_error_8_SUITE"),
	      Join(DataDir, "cfg_error_9_SUITE")
	     ],
    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(cfg_error, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(cfg_error),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).


%%%-----------------------------------------------------------------
%%% 
lib_error(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Join = fun(D, S) -> filename:join(D, "error/test/"++S) end,
    Suites = [Join(DataDir, "lib_error_1_SUITE")],
    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(lib_error, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(lib_error),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).
    

%%%-----------------------------------------------------------------
%%% 
no_compile(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Join = fun(D, S) -> filename:join(D, "error/test/"++S) end,
    Suites = [Join(DataDir, "no_compile_SUITE")],
    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(no_compile, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(no_compile),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).
    

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

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

test_events(cfg_error) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{9,9,33}},

     {?eh,tc_start,{cfg_error_1_SUITE,init_per_suite}},
     {?eh,tc_done,
      {cfg_error_1_SUITE,init_per_suite,{failed,{error,init_per_suite_fails}}}},
     {?eh,tc_auto_skip,
      {cfg_error_1_SUITE,tc1,{failed,{cfg_error_1_SUITE,init_per_suite,
				      {'EXIT',init_per_suite_fails}}}}},
     {?eh,test_stats,{0,0,{0,1}}},
     {?eh,tc_auto_skip,
      {cfg_error_1_SUITE,tc2,{failed,{cfg_error_1_SUITE,init_per_suite,
				      {'EXIT',init_per_suite_fails}}}}},
     {?eh,test_stats,{0,0,{0,2}}},
     {?eh,tc_auto_skip,
      {cfg_error_1_SUITE,end_per_suite,{failed,{cfg_error_1_SUITE,init_per_suite,
				      {'EXIT',init_per_suite_fails}}}}},

     {?eh,tc_start,{cfg_error_2_SUITE,init_per_suite}},
     {?eh,tc_done,
      {cfg_error_2_SUITE,init_per_suite,
       {failed,{error,{{badmatch,[1,2]},
		       [{cfg_error_2_SUITE,init_per_suite,1},
			{test_server,my_apply,3},
			{test_server,ts_tc,3},
			{test_server,run_test_case_eval1,6},
			{test_server,run_test_case_eval,8}]}}}}},
     {?eh,tc_auto_skip,
      {cfg_error_2_SUITE,tc1,
       {failed,{cfg_error_2_SUITE,init_per_suite,      
		{'EXIT',{{badmatch,[1,2]},
			 [{cfg_error_2_SUITE,init_per_suite,1},
			  {test_server,my_apply,3},
			  {test_server,ts_tc,3},
			  {test_server,run_test_case_eval1,6},
			  {test_server,run_test_case_eval,8}]}}}}}},
     {?eh,test_stats,{0,0,{0,3}}},
     {?eh,tc_auto_skip,
      {cfg_error_2_SUITE,tc2,
       {failed,{cfg_error_2_SUITE,init_per_suite,      
		{'EXIT',{{badmatch,[1,2]},
			 [{cfg_error_2_SUITE,init_per_suite,1},
			  {test_server,my_apply,3},
			  {test_server,ts_tc,3},
			  {test_server,run_test_case_eval1,6},
			  {test_server,run_test_case_eval,8}]}}}}}},
     {?eh,test_stats,{0,0,{0,4}}},
     {?eh,tc_auto_skip,
      {cfg_error_2_SUITE,end_per_suite,
       {failed,{cfg_error_2_SUITE,init_per_suite,      
		{'EXIT',{{badmatch,[1,2]},
			 [{cfg_error_2_SUITE,init_per_suite,1},
			  {test_server,my_apply,3},
			  {test_server,ts_tc,3},
			  {test_server,run_test_case_eval1,6},
			  {test_server,run_test_case_eval,8}]}}}}}},

     {?eh,tc_start,{cfg_error_3_SUITE,init_per_suite}},
     {?eh,tc_done,
      {cfg_error_3_SUITE,init_per_suite,{failed,{timetrap_timeout,2000}}}},
     {?eh,tc_auto_skip,
      {cfg_error_3_SUITE,tc1,
       {failed,{cfg_error_3_SUITE,init_per_suite,{timetrap_timeout,2000}}}}},
     {?eh,test_stats,{0,0,{0,5}}},
     {?eh,tc_auto_skip,
      {cfg_error_3_SUITE,tc2,
       {failed,{cfg_error_3_SUITE,init_per_suite,{timetrap_timeout,2000}}}}},
     {?eh,test_stats,{0,0,{0,6}}},
     {?eh,tc_auto_skip,
      {cfg_error_3_SUITE,end_per_suite,
       {failed,{cfg_error_3_SUITE,init_per_suite,{timetrap_timeout,2000}}}}},

     {?eh,tc_start,{cfg_error_4_SUITE,init_per_suite}},
     {?eh,tc_done,{cfg_error_4_SUITE,init_per_suite,ok}},
     {?eh,tc_auto_skip,
      {cfg_error_4_SUITE,tc1,
       {failed,{cfg_error_4_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{0,0,{0,7}}},
     {?eh,tc_auto_skip,
      {cfg_error_4_SUITE,tc2,
       {failed,{cfg_error_4_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{0,0,{0,8}}},
     {?eh,tc_auto_skip,
      {cfg_error_4_SUITE,end_per_suite,
       {failed,{cfg_error_4_SUITE,init_per_suite,bad_return}}}},

     {?eh,tc_start,{cfg_error_5_SUITE,init_per_suite}},
     {?eh,tc_done,{cfg_error_5_SUITE,init_per_suite,ok}},
     {?eh,tc_auto_skip,
      {cfg_error_5_SUITE,tc1,
       {failed,{cfg_error_5_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{0,0,{0,9}}},
     {?eh,tc_auto_skip,
      {cfg_error_5_SUITE,tc2,
       {failed,{cfg_error_5_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{0,0,{0,10}}},
     {?eh,tc_auto_skip,
      {cfg_error_5_SUITE,end_per_suite,
       {failed,{cfg_error_5_SUITE,init_per_suite,bad_return}}}},

     {?eh,tc_start,{cfg_error_6_SUITE,init_per_suite}},
     {?eh,tc_done,{cfg_error_6_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{cfg_error_6_SUITE,tc1}},
     {?eh,tc_done,{cfg_error_6_SUITE,tc1,ok}},
     {?eh,test_stats,{1,0,{0,10}}},
     [{?eh,tc_start,{cfg_error_6_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{cfg_error_6_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_start,{cfg_error_6_SUITE,tc2}},
      {?eh,tc_done,{cfg_error_6_SUITE,tc2,ok}},
      {?eh,test_stats,{2,0,{0,10}}},
      {?eh,tc_start,{cfg_error_6_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{cfg_error_6_SUITE,{end_per_group,g1,[]},ok}}],
     {?eh,tc_start,{cfg_error_6_SUITE,end_per_suite}},
     {?eh,tc_done,{cfg_error_6_SUITE,end_per_suite,
		   {failed,{error,{{badmatch,[1,2]},
				   [{cfg_error_6_SUITE,end_per_suite,1},
				    {test_server,my_apply,3},
				    {test_server,ts_tc,3},
				    {test_server,run_test_case_eval1,6},
				    {test_server,run_test_case_eval,8}]}}}}},

     {?eh,tc_start,{cfg_error_7_SUITE,init_per_suite}},
     {?eh,tc_done,{cfg_error_7_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{cfg_error_7_SUITE,tc1}},
     {?eh,tc_done,{cfg_error_7_SUITE,tc1,ok}},
     {?eh,test_stats,{3,0,{0,10}}},
     [{?eh,tc_start,{cfg_error_7_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{cfg_error_7_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_start,{cfg_error_7_SUITE,tc2}},
      {?eh,tc_done,{cfg_error_7_SUITE,tc2,ok}},
      {?eh,test_stats,{4,0,{0,10}}},
      {?eh,tc_start,{cfg_error_7_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{cfg_error_7_SUITE,{end_per_group,g1,[]},ok}}],
     {?eh,tc_start,{cfg_error_7_SUITE,end_per_suite}},
     {?eh,tc_done,
      {cfg_error_7_SUITE,end_per_suite,{failed,{timetrap_timeout,2000}}}},

     {?eh,tc_start,{cfg_error_8_SUITE,init_per_suite}},
     {?eh,tc_done,{cfg_error_8_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,
       {cfg_error_8_SUITE,{init_per_group,g1,[]},
	{failed,{error,{init_per_group_fails,g1}}}}},
      {?eh,tc_auto_skip,
       {cfg_error_8_SUITE,tc1,
	{failed,{cfg_error_8_SUITE,init_per_group,
	  {'EXIT',{init_per_group_fails,g1}}}}}},
      {?eh,test_stats,{4,0,{0,11}}},
      {?eh,tc_auto_skip,
       {cfg_error_8_SUITE,end_per_group,
	{failed,{cfg_error_8_SUITE,init_per_group,
	  {'EXIT',{init_per_group_fails,g1}}}}}}],

     [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,
		    {init_per_group,unknown,[]},
		    {failed,{timetrap_timeout,2000}}}},
      {?eh,tc_auto_skip,{cfg_error_8_SUITE,tc1,
			 {failed,{cfg_error_8_SUITE,init_per_group,
				  {timetrap_timeout,2000}}}}},
      {?eh,test_stats,{4,0,{0,12}}},
      {?eh,tc_auto_skip,{cfg_error_8_SUITE,end_per_group,
			 {failed,{cfg_error_8_SUITE,init_per_group,
				  {timetrap_timeout,2000}}}}}],
     
     [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,
       {cfg_error_8_SUITE,{init_per_group,g3,[]},
	{failed,{error,{{badmatch,42},
			[{cfg_error_8_SUITE,init_per_group,2},
			 {cfg_error_8_SUITE,init_per_group,2},
			 {test_server,my_apply,3},
			 {test_server,ts_tc,3},
			 {test_server,run_test_case_eval1,6},
			 {test_server,run_test_case_eval,8}]}}}}},
      {?eh,tc_auto_skip,
       {cfg_error_8_SUITE,tc1,
	{failed,{cfg_error_8_SUITE,init_per_group,
		 {'EXIT',{{badmatch,42},
			  [{cfg_error_8_SUITE,init_per_group,2},
			   {cfg_error_8_SUITE,init_per_group,2},
			   {test_server,my_apply,3},
			   {test_server,ts_tc,3},
			   {test_server,run_test_case_eval1,6},
			   {test_server,run_test_case_eval,8}]}}}}}},
      {?eh,test_stats,{4,0,{0,13}}},
      {?eh,tc_auto_skip,
       {cfg_error_8_SUITE,end_per_group,
	{failed,{cfg_error_8_SUITE,init_per_group,
		 {'EXIT',{{badmatch,42},
			  [{cfg_error_8_SUITE,init_per_group,2},
			   {cfg_error_8_SUITE,init_per_group,2},
			   {test_server,my_apply,3},
			   {test_server,ts_tc,3},
			   {test_server,run_test_case_eval1,6},
			   {test_server,run_test_case_eval,8}]}}}}}}],

     [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g4,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{init_per_group,g4,[]},ok}},
      {?eh,tc_start,{cfg_error_8_SUITE,tc1}},
      {?eh,tc_done,{cfg_error_8_SUITE,tc1,ok}},
      {?eh,test_stats,{5,0,{0,13}}},
      {?eh,tc_start,{cfg_error_8_SUITE,{end_per_group,g4,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{end_per_group,g4,[]},ok}}],
     
     [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g5,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{init_per_group,g5,[]},ok}},
      {?eh,tc_start,{cfg_error_8_SUITE,tc1}},
      {?eh,tc_done,{cfg_error_8_SUITE,tc1,ok}},
      {?eh,test_stats,{6,0,{0,13}}},
      [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g6,[]}}},
       {?eh,tc_done,{cfg_error_8_SUITE,{init_per_group,g6,[]},
		     {failed,{error,{sub_group_failed,g6}}}}},
       {?eh,tc_auto_skip,
	{cfg_error_8_SUITE,tc2,
	 {failed,{cfg_error_8_SUITE,init_per_group,
		  {'EXIT',{sub_group_failed,g6}}}}}},
       {?eh,test_stats,{6,0,{0,14}}},
       {?eh,tc_auto_skip,
	{cfg_error_8_SUITE,end_per_group,
	 {failed,{cfg_error_8_SUITE,init_per_group,
		  {'EXIT',{sub_group_failed,g6}}}}}}],
      {?eh,tc_start,{cfg_error_8_SUITE,tc3}},
      {?eh,tc_done,{cfg_error_8_SUITE,tc3,ok}},
      {?eh,test_stats,{7,0,{0,14}}},
      {?eh,tc_start,{cfg_error_8_SUITE,{end_per_group,g5,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{end_per_group,g5,[]},ok}}],

     [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g11,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{init_per_group,g11,[]},ok}},
      {?eh,tc_start,{cfg_error_8_SUITE,tc1}},
      {?eh,tc_done,{cfg_error_8_SUITE,tc1,ok}},
      {?eh,test_stats,{8,0,{0,14}}},
      {?eh,tc_start,{cfg_error_8_SUITE,{end_per_group,g11,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{end_per_group,g11,[]},
		    {failed,{error,{end_per_group_fails,g5}}}}}],

     [{?eh,tc_start,{cfg_error_8_SUITE,{init_per_group,g12,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{init_per_group,g12,[]},ok}},
      {?eh,tc_start,{cfg_error_8_SUITE,tc1}},
      {?eh,tc_done,{cfg_error_8_SUITE,tc1,ok}},
      {?eh,test_stats,{9,0,{0,14}}},
      {?eh,tc_start,{cfg_error_8_SUITE,{end_per_group,g12,[]}}},
      {?eh,tc_done,{cfg_error_8_SUITE,{end_per_group,unknown,[]},
		    {failed,{timetrap_timeout,2000}}}}],

     {?eh,tc_start,{cfg_error_8_SUITE,end_per_suite}},
     {?eh,tc_done,{cfg_error_8_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{cfg_error_9_SUITE,init_per_suite}},
     {?eh,tc_done,{cfg_error_9_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc1}},
     {?eh,tc_done,{cfg_error_9_SUITE,tc1,
		   {skipped,{failed,{cfg_error_9_SUITE,init_per_testcase,
				     tc1_should_be_skipped}}}}},
     {?eh,test_stats,{9,0,{0,15}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc2}},
     {?eh,tc_done,{cfg_error_9_SUITE,tc2,
		   {skipped,{failed,{cfg_error_9_SUITE,init_per_testcase,
				     {timetrap_timeout,2000}}}}}},
     {?eh,test_stats,{9,0,{0,16}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc3}},
     {?eh,tc_done,{cfg_error_9_SUITE,tc3,
		   {skipped,{failed,{cfg_error_9_SUITE,init_per_testcase,
				     {{badmatch,undefined},
				      [{cfg_error_9_SUITE,init_per_testcase,2},
				       {test_server,my_apply,3},
				       {test_server,init_per_testcase,3},
				       {test_server,run_test_case_eval1,6},
				       {test_server,run_test_case_eval,8}]}}}}}},
     {?eh,test_stats,{9,0,{0,17}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc4}},
     {?eh,tc_done,
      {cfg_error_9_SUITE,tc4,
       {skipped,{failed,{cfg_error_9_SUITE,init_per_testcase,bad_return}}}}},
     {?eh,test_stats,{9,0,{0,18}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc5}},
     {?eh,tc_done,
      {cfg_error_9_SUITE,tc5,{failed,{error,fail_this_testcase}}}},
     {?eh,test_stats,{9,1,{0,18}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc6}},
     %%! we get ok with tc_done since it's only afterwards
     %%! end_tc failes the testcase
     {?eh,tc_done,{cfg_error_9_SUITE,tc6,ok}},
     {?eh,test_stats,{9,2,{0,18}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc11}},
     {?eh,tc_done,{cfg_error_9_SUITE,tc11,
		   {failed,{cfg_error_9_SUITE,end_per_testcase,
			    {'EXIT',warning_should_be_printed}}}}},
     {?eh,test_stats,{10,2,{0,18}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc12}},
     {?eh,tc_done,{cfg_error_9_SUITE,tc12,
		   {failed,{cfg_error_9_SUITE,end_per_testcase,
			    {timetrap_timeout,2000}}}}},
     {?eh,test_stats,{11,2,{0,18}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc13}},
     {?eh,tc_done,{cfg_error_9_SUITE,tc13,
		   {failed,{cfg_error_9_SUITE,end_per_testcase,
			    {'EXIT',{{badmatch,undefined},
				     [{cfg_error_9_SUITE,end_per_testcase,2},
				      {test_server,my_apply,3},
				      {test_server,do_end_per_testcase,4},
				      {test_server,run_test_case_eval1,6},
				      {test_server,run_test_case_eval,8}]}}}}}},
     {?eh,test_stats,{12,2,{0,18}}},
     {?eh,tc_start,{cfg_error_9_SUITE,tc14}},
     {?eh,tc_done,
      {cfg_error_9_SUITE,tc14,{failed,{error,tc14_should_be_failed}}}},
     {?eh,test_stats,{12,3,{0,18}}},

     {?eh,tc_start,{cfg_error_9_SUITE,end_per_suite}},
     {?eh,tc_done,{cfg_error_9_SUITE,end_per_suite,ok}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(lib_error) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,14}},
     {?eh,tc_start,{lib_error_1_SUITE,init_per_suite}},
     {?eh,tc_done,{lib_error_1_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{lib_error_1_SUITE,lines_error}},
     {?eh,tc_done,
      {lib_error_1_SUITE,lines_error,{failed,
				      {error,
				       {{badmatch,[1,2]},
					[{lib_lines,do_error,0},
					 {lib_error_1_SUITE,lines_error,1},
					 {test_server,my_apply,3},
					 {test_server,ts_tc,3},
					 {test_server,run_test_case_eval1,6},
					 {test_server,run_test_case_eval,8}]}}}}},
     {?eh,test_stats,{0,1,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,lines_exit}},
     {?eh,tc_done,
      {lib_error_1_SUITE,lines_exit,{failed,{error,byebye}}}},
     {?eh,test_stats,{0,2,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,lines_hang}},
     {?eh,tc_done,
      {lib_lines,do_hang,{failed,{timetrap_timeout,3000}}}},
     {?eh,test_stats,{0,3,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,lines_throw}},
     {?eh,tc_done,
      {lib_error_1_SUITE,lines_throw,
       {failed,{error,{thrown,catch_me_if_u_can}}}}},
     {?eh,test_stats,{0,4,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,no_lines_error}},
     {?eh,tc_done,
      {lib_error_1_SUITE,no_lines_error,{failed,
					 {error,
					  {{badmatch,[1,2]},
					   [{lib_no_lines,do_error,0},
					    {lib_error_1_SUITE,no_lines_error,1},
					    {test_server,my_apply,3},
					    {test_server,ts_tc,3},
					    {test_server,run_test_case_eval1,6},
					    {test_server,run_test_case_eval,8}]}}}}},
     {?eh,test_stats,{0,5,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,no_lines_exit}},
     {?eh,tc_done,
      {lib_error_1_SUITE,no_lines_exit,{failed,{error,byebye}}}},
     {?eh,test_stats,{0,6,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,no_lines_hang}},
     {?eh,tc_done,
      {lib_error_1_SUITE,no_lines_hang,{failed,{timetrap_timeout,3000}}}},
     {?eh,test_stats,{0,7,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,no_lines_throw}},
     {?eh,tc_done,
      {lib_error_1_SUITE,no_lines_throw,{failed,{error,{thrown,catch_me_if_u_can}}}}},
     {?eh,test_stats,{0,8,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,init_tc_error}},
     {?eh,tc_done,{lib_error_1_SUITE,init_tc_error,ok}},
     {?eh,test_stats,{1,8,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,init_tc_exit}},
     {?eh,tc_done,{lib_error_1_SUITE,init_tc_exit,ok}},
     {?eh,test_stats,{2,8,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,init_tc_throw}},
     {?eh,tc_done,{lib_error_1_SUITE,init_tc_throw,ok}},
     {?eh,test_stats,{3,8,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,end_tc_error}},
     {?eh,tc_done,{lib_error_1_SUITE,end_tc_error,ok}},
     {?eh,test_stats,{3,9,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,end_tc_exit}},
     {?eh,tc_done,{lib_error_1_SUITE,end_tc_exit,ok}},
     {?eh,test_stats,{3,10,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,end_tc_throw}},
     {?eh,tc_done,{lib_error_1_SUITE,end_tc_throw,ok}},
     {?eh,test_stats,{3,11,{0,0}}},
     {?eh,tc_start,{lib_error_1_SUITE,end_per_suite}},
     {?eh,tc_done,{lib_error_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(no_compile) ->
    [].
