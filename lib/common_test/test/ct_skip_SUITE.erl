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
%%% File: ct_skip_SUITE
%%%
%%% Description: 
%%% Test auto- and user-skip functionality
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_skip_SUITE).

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
     auto_skip,
     user_skip
    ].
     

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
auto_skip(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Join = fun(D, S) -> filename:join(D, "skip/test/"++S) end,
    Suites = [Join(DataDir, "auto_skip_1_SUITE"),
	      Join(DataDir, "auto_skip_2_SUITE"),
	      Join(DataDir, "auto_skip_3_SUITE"),
	      Join(DataDir, "auto_skip_4_SUITE"),
	      Join(DataDir, "auto_skip_5_SUITE"),
	      Join(DataDir, "auto_skip_6_SUITE"),
	      Join(DataDir, "auto_skip_7_SUITE"),
	      Join(DataDir, "auto_skip_8_SUITE"),
	      Join(DataDir, "auto_skip_9_SUITE"),
	      Join(DataDir, "auto_skip_10_SUITE"),
	      Join(DataDir, "auto_skip_11_SUITE")
	     ],

    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(auto_skip, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(auto_skip),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).
    

%%%-----------------------------------------------------------------
%%% 
user_skip(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Join = fun(D, S) -> filename:join(D, "skip/test/"++S) end,
    Suites = [Join(DataDir, "user_skip_1_SUITE"),
	      Join(DataDir, "user_skip_2_SUITE"),	      
	      Join(DataDir, "user_skip_3_SUITE"),
	      Join(DataDir, "user_skip_4_SUITE"),
	      Join(DataDir, "user_skip_5_SUITE")],

    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(user_skip, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),

    TestEvents = events_to_check(user_skip),
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

test_events(auto_skip) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{11,11,34}},

     {?eh,tc_start,{auto_skip_1_SUITE,init_per_suite}},
     {?eh,tc_done,
      {auto_skip_1_SUITE,init_per_suite,{failed,{error,init_per_suite_failed}}}},
     {?eh,tc_auto_skip,
      {auto_skip_1_SUITE,tc1,{failed,{auto_skip_1_SUITE,init_per_suite,
				      {'EXIT',init_per_suite_failed}}}}},
     {?eh,test_stats,{0,0,{0,1}}},
     {?eh,tc_auto_skip,
      {auto_skip_1_SUITE,tc2,{failed,{auto_skip_1_SUITE,init_per_suite,
				      {'EXIT',init_per_suite_failed}}}}},
     {?eh,test_stats,{0,0,{0,2}}},
     {?eh,tc_auto_skip,
      {auto_skip_1_SUITE,end_per_suite,{failed,{auto_skip_1_SUITE,init_per_suite,
						{'EXIT',init_per_suite_failed}}}}},

     {?eh,tc_start,{auto_skip_2_SUITE,init_per_suite}},
     {?eh,tc_done,
      {auto_skip_2_SUITE,init_per_suite,{failed,{error,init_per_suite_failed}}}},
     {?eh,tc_auto_skip,      
      {auto_skip_2_SUITE,tc1,{failed,{auto_skip_2_SUITE,init_per_suite,
				      {'EXIT',init_per_suite_failed}}}}},
     {?eh,test_stats,{0,0,{0,3}}},
     {?eh,tc_auto_skip,
      {auto_skip_2_SUITE,end_per_suite,{failed,{auto_skip_2_SUITE,init_per_suite,
						{'EXIT',init_per_suite_failed}}}}},

     {?eh,tc_start,{auto_skip_3_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_3_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{auto_skip_3_SUITE,tc1}},
     {?eh,tc_done,
      {auto_skip_3_SUITE,tc1,
       {skipped,{failed,{auto_skip_3_SUITE,init_per_testcase,
			 {init_per_testcase,tc1,failed}}}}}},
     {?eh,test_stats,{0,0,{0,4}}},
     {?eh,tc_start,{auto_skip_3_SUITE,tc2}},
     {?eh,tc_done,{auto_skip_3_SUITE,tc2,ok}},
     {?eh,test_stats,{1,0,{0,4}}},
     {?eh,tc_start,{auto_skip_3_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_3_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{auto_skip_4_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_4_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{auto_skip_4_SUITE,tc1}},
     {?eh,tc_done,{auto_skip_4_SUITE,tc1,
		   {skipped,{failed,{auto_skip_4_SUITE,init_per_testcase,
				     {timetrap_timeout,1000}}}}}},
     {?eh,test_stats,{1,0,{0,5}}},     
     {?eh,tc_start,{auto_skip_4_SUITE,tc2}},
     {?eh,tc_done,{auto_skip_4_SUITE,tc2,ok}},
     {?eh,test_stats,{2,0,{0,5}}},    
     {?eh,tc_start,{auto_skip_4_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_4_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{auto_skip_5_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_5_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{auto_skip_5_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,
       {auto_skip_5_SUITE,{init_per_group,g1,[]},{failed,{error,{group,g1,failed}}}}},
      {?eh,tc_auto_skip,
       {auto_skip_5_SUITE,tc1,{failed,{auto_skip_5_SUITE,init_per_group,
				       {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,6}}},    
      {?eh,tc_auto_skip,
       {auto_skip_5_SUITE,tc2,{failed,{auto_skip_5_SUITE,init_per_group,
				       {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,7}}},
      {?eh,tc_auto_skip,
       {auto_skip_5_SUITE,end_per_group,{failed,{auto_skip_5_SUITE,init_per_group,
						 {'EXIT',{group,g1,failed}}}}}}],

     {?eh,tc_start,{auto_skip_5_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_5_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{auto_skip_6_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_6_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{auto_skip_6_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,
       {auto_skip_6_SUITE,{init_per_group,g1,[]},{failed,{error,{group,g1,failed}}}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,tc1,{failed,{auto_skip_6_SUITE,init_per_group,
				       {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,8}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,tc3,{failed,{auto_skip_6_SUITE,init_per_group,
				       {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,9}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,tc4,{failed,{auto_skip_6_SUITE,init_per_group,
				       {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,10}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,tc2,{failed,{auto_skip_6_SUITE,init_per_group,
				       {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,11}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,end_per_group,{failed,{auto_skip_6_SUITE,init_per_group,
						 {'EXIT',{group,g1,failed}}}}}}],

     [{?eh,tc_start,{auto_skip_6_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{auto_skip_6_SUITE,{init_per_group,g3,[]},ok}},
      {?eh,tc_start,{auto_skip_6_SUITE,tc1}},
      {?eh,tc_done,{auto_skip_6_SUITE,tc1,ok}},
      {?eh,test_stats,{3,0,{0,11}}},
      [{?eh,tc_start,{auto_skip_6_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{auto_skip_6_SUITE,{init_per_group,g4,[]},
		     {failed,{error,{group,g4,failed}}}}},
       {?eh,tc_auto_skip,
	{auto_skip_6_SUITE,tc3,{failed,{auto_skip_6_SUITE,init_per_group,
					{'EXIT',{group,g4,failed}}}}}},
       {?eh,test_stats,{3,0,{0,12}}},
       {?eh,tc_auto_skip,
	{auto_skip_6_SUITE,tc4,{failed,{auto_skip_6_SUITE,init_per_group,
					{'EXIT',{group,g4,failed}}}}}},
       {?eh,test_stats,{3,0,{0,13}}},
       {?eh,tc_auto_skip,
	{auto_skip_6_SUITE,end_per_group,{failed,{auto_skip_6_SUITE,init_per_group,
						  {'EXIT',{group,g4,failed}}}}}}],
      {?eh,tc_start,{auto_skip_6_SUITE,tc2}},
      {?eh,tc_done,{auto_skip_6_SUITE,tc2,ok}},
      {?eh,test_stats,{4,0,{0,13}}},
      {?eh,tc_start,{auto_skip_6_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{auto_skip_6_SUITE,{end_per_group,g3,[]},ok}}],

     {?eh,tc_start,{auto_skip_6_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_6_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{auto_skip_7_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_7_SUITE,init_per_suite,ok}},
     {?eh,tc_auto_skip,
      {auto_skip_7_SUITE,tc1,{failed,{auto_skip_7_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{4,0,{0,14}}},
     {?eh,tc_auto_skip,
      {auto_skip_7_SUITE,tc2,{failed,{auto_skip_7_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{4,0,{0,15}}},
     {?eh,tc_auto_skip,
      {auto_skip_7_SUITE,end_per_suite,{failed,{auto_skip_7_SUITE,init_per_suite,bad_return}}}},

     {?eh,tc_start,{auto_skip_8_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_8_SUITE,init_per_suite,ok}},
     {?eh,tc_auto_skip,
      {auto_skip_8_SUITE,tc1,{failed,{auto_skip_8_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{4,0,{0,16}}},
     {?eh,tc_auto_skip,
      {auto_skip_8_SUITE,tc2,{failed,{auto_skip_8_SUITE,init_per_suite,bad_return}}}},
     {?eh,test_stats,{4,0,{0,17}}},
     {?eh,tc_auto_skip,
      {auto_skip_8_SUITE,end_per_suite,{failed,{auto_skip_8_SUITE,init_per_suite,bad_return}}}},

     {?eh,tc_start,{auto_skip_9_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_9_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{auto_skip_9_SUITE,tc1}},
     {?eh,tc_done,{auto_skip_9_SUITE,tc1,ok}},
     {?eh,test_stats,{5,0,{0,17}}},
     {?eh,tc_start,{auto_skip_9_SUITE,tc2}},
     {?eh,tc_done,{auto_skip_9_SUITE,tc2,
		   {skipped,{failed,{auto_skip_9_SUITE,init_per_testcase,bad_return}}}}},
     {?eh,test_stats,{5,0,{0,18}}},

     [{?eh,tc_start,{auto_skip_9_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{auto_skip_9_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_start,{auto_skip_9_SUITE,tc3}},
      {?eh,tc_done,{auto_skip_9_SUITE,tc3,ok}},
      {?eh,test_stats,{6,0,{0,18}}},
      {?eh,tc_start,{auto_skip_9_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{auto_skip_9_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{auto_skip_9_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{auto_skip_9_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_start,{auto_skip_9_SUITE,tc4}},
      {?eh,tc_done,{auto_skip_9_SUITE,tc4,ok}},
      {?eh,test_stats,{7,0,{0,18}}},
      {?eh,tc_start,{auto_skip_9_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{auto_skip_9_SUITE,{end_per_group,g2,[]},ok}}],

     [{?eh,tc_start,{auto_skip_9_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{auto_skip_9_SUITE,{init_per_group,g3,[]},ok}},
      {?eh,tc_start,{auto_skip_9_SUITE,tc5}},
      {?eh,tc_done,{auto_skip_9_SUITE,tc5,
		    {skipped,{failed,{auto_skip_9_SUITE,init_per_testcase,bad_return}}}}},
      {?eh,test_stats,{7,0,{0,19}}},
      {?eh,tc_start,{auto_skip_9_SUITE,{end_per_group,g3,[]}}},
      {?eh,tc_done,{auto_skip_9_SUITE,{end_per_group,g3,[]},ok}}],

     {parallel,
      [{?eh,tc_start,
	{auto_skip_9_SUITE,{init_per_group,g4,[parallel]}}},
       {?eh,tc_done,
	{auto_skip_9_SUITE,{init_per_group,g4,[parallel]},ok}},
       {?eh,tc_start,{auto_skip_9_SUITE,tc6}},
       {?eh,tc_done,{auto_skip_9_SUITE,tc6,ok}},
       {parallel,
	[{?eh,tc_start,
	  {auto_skip_9_SUITE,{init_per_group,g5,[parallel]}}},
	 {?eh,tc_done,
	  {auto_skip_9_SUITE,{init_per_group,g5,[parallel]},ok}},
	 {?eh,tc_start,{auto_skip_9_SUITE,tc8}},
	 {?eh,tc_done,
	  {auto_skip_9_SUITE,tc8,
	   {skipped,{failed,{auto_skip_9_SUITE,init_per_testcase,
			     {{badmatch,undefined},
			      [{auto_skip_9_SUITE,init_per_testcase,2},
			       {test_server,my_apply,3},
			       {test_server,init_per_testcase,3},
			       {test_server,run_test_case_eval1,6},
			       {test_server,run_test_case_eval,8}]}}}}}},
	 {?eh,tc_start,
	  {auto_skip_9_SUITE,{end_per_group,g5,[parallel]}}},
	 {?eh,tc_done,
	  {auto_skip_9_SUITE,{end_per_group,g5,[parallel]},ok}}]},

       {?eh,tc_start,{auto_skip_9_SUITE,tc7}},
       {?eh,tc_done,{auto_skip_9_SUITE,tc7,ok}},
       {?eh,test_stats,{9,0,{0,20}}},
       {?eh,tc_start,
	{auto_skip_9_SUITE,{end_per_group,g4,[parallel]}}},
       {?eh,tc_done,
	{auto_skip_9_SUITE,{end_per_group,g4,[parallel]},ok}}]},

     {?eh,tc_start,{auto_skip_9_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_9_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{auto_skip_10_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_10_SUITE,init_per_suite,
		   {skipped,
		    {require_failed_in_suite0,
		     {not_available,undefined_config_variable}}}}},
     {?eh,tc_auto_skip,
      {auto_skip_10_SUITE,tc1,
       {require_failed_in_suite0,{not_available,undefined_config_variable}}}},
     {?eh,test_stats,{9,0,{0,21}}},
     {?eh,tc_auto_skip,
      {auto_skip_10_SUITE,tc2,
       {require_failed_in_suite0,{not_available,undefined_config_variable}}}},
     {?eh,test_stats,{9,0,{0,22}}},
     {?eh,tc_auto_skip,
      {auto_skip_10_SUITE,end_per_suite,
       {require_failed_in_suite0,{not_available,undefined_config_variable}}}},

     {?eh,tc_start,{auto_skip_11_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{auto_skip_11_SUITE,tc1}},
     {?eh,tc_done,{auto_skip_11_SUITE,tc1,
		   {skipped,{require_failed,{not_available,undefined_config_variable}}}}},
     {?eh,test_stats,{9,0,{0,23}}},
     {?eh,tc_start,{auto_skip_11_SUITE,tc2}},
     {?eh,tc_done,{auto_skip_11_SUITE,tc2,ok}},
     {?eh,test_stats,{10,0,{0,23}}},

     {parallel,
      [{?eh,tc_start,
	{auto_skip_11_SUITE,{init_per_group,g1,[parallel]}}},
       {?eh,tc_done,
	{auto_skip_11_SUITE,{init_per_group,g1,[parallel]},ok}},
       {parallel,
	[{?eh,tc_start,
	  {auto_skip_11_SUITE,{init_per_group,g2,[parallel]}}},
	 {?eh,tc_done,
	  {auto_skip_11_SUITE,{init_per_group,g2,[parallel]},ok}},
	 {?eh,tc_start,{auto_skip_11_SUITE,tc3}},
	 {?eh,tc_done,
	  {auto_skip_11_SUITE,tc3,
	   {skipped,{require_failed,{not_available,undefined_config_variable}}}}},
	 {?eh,test_stats,{10,0,{0,24}}},
	 {?eh,tc_start,
	  {auto_skip_11_SUITE,{end_per_group,g2,[parallel]}}},
	 {?eh,tc_done,
	  {auto_skip_11_SUITE,{end_per_group,g2,[parallel]},ok}}]},
       {?eh,tc_start,
	{auto_skip_11_SUITE,{end_per_group,g1,[parallel]}}},
       {?eh,tc_done,
	{auto_skip_11_SUITE,{end_per_group,g1,[parallel]},ok}}]},

     {?eh,tc_start,{auto_skip_11_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_11_SUITE,end_per_suite,ok}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(user_skip) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{5,5,27}},

     {?eh,tc_start,{user_skip_1_SUITE,init_per_suite}},
     {?eh,tc_done,
      {user_skip_1_SUITE,init_per_suite,{skipped,"Whole suite skipped"}}},
     {?eh,tc_auto_skip,
      {user_skip_1_SUITE,tc1,"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{0,1}}},
     {?eh,tc_auto_skip,
      {user_skip_1_SUITE,tc2,"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{0,2}}},
     {?eh,tc_auto_skip,
      {user_skip_1_SUITE,tc3,"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{0,3}}},
     {?eh,tc_auto_skip,
      {user_skip_1_SUITE,tc4,"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{0,4}}},
     {?eh,tc_auto_skip,
      {user_skip_1_SUITE,end_per_suite,"Whole suite skipped"}},

     {?eh,tc_start,{user_skip_2_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_2_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{user_skip_2_SUITE,tc1}},
     {?eh,tc_done,{user_skip_2_SUITE,tc1,{skipped,{tc1,skipped}}}},
     {?eh,test_stats,{0,0,{1,4}}},

     [{?eh,tc_start,{user_skip_2_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_2_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_start,{user_skip_2_SUITE,tc2}},
      {?eh,tc_done,{user_skip_2_SUITE,tc2,ok}},
      {?eh,test_stats,{1,0,{1,4}}},
      {?eh,tc_start,{user_skip_2_SUITE,tc3}},
      {?eh,tc_done,{user_skip_2_SUITE,tc3,{skipped,{tc3,skipped}}}},
      {?eh,test_stats,{1,0,{2,4}}},
      {?eh,tc_start,{user_skip_2_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_2_SUITE,{end_per_group,g1,[]},ok}}],

     {?eh,tc_start,{user_skip_2_SUITE,tc4}},
     {?eh,tc_done,{user_skip_2_SUITE,tc4,ok}},
     {?eh,test_stats,{2,0,{2,4}}},
     {?eh,tc_start,{user_skip_2_SUITE,end_per_suite}},
     {?eh,tc_done,{user_skip_2_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{user_skip_3_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_3_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{user_skip_3_SUITE,tc1}},
     {?eh,tc_done,{user_skip_3_SUITE,tc1,{skipped,"Test case skipped"}}},
     {?eh,test_stats,{2,0,{3,4}}},

     [{?eh,tc_start,{user_skip_3_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_3_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_start,{user_skip_3_SUITE,tc2}},
      {?eh,tc_done,{user_skip_3_SUITE,tc2,ok}},
      {?eh,test_stats,{3,0,{3,4}}},
      {?eh,tc_start,{user_skip_3_SUITE,tc3}},
      {?eh,tc_done,{user_skip_3_SUITE,tc3,{skipped,"Test case skipped"}}},
      {?eh,test_stats,{3,0,{4,4}}},
      {?eh,tc_start,{user_skip_3_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_3_SUITE,{end_per_group,g1,[]},ok}}],

     {?eh,tc_start,{user_skip_3_SUITE,tc4}},
     {?eh,tc_done,{user_skip_3_SUITE,tc4,
		   {skipped,{proc_info,{{current_function,{user_skip_3_SUITE,tc4,1}},
					{initial_call,{erlang,apply,2}}}}}}},
     {?eh,test_stats,{3,0,{5,4}}},
     {?eh,tc_start,{user_skip_3_SUITE,end_per_suite}},
     {?eh,tc_done,{user_skip_3_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{user_skip_4_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_4_SUITE,init_per_suite,ok}},

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g1,[]},{skipped,"Group skipped"}}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,tc1,"Group skipped"}},   
      {?eh,test_stats,{3,0,{5,5}}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,tc2,"Group skipped"}},
      {?eh,test_stats,{3,0,{5,6}}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,end_per_group,"Group skipped"}}],

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_start,{user_skip_4_SUITE,tc3}},
      {?eh,tc_done,{user_skip_4_SUITE,tc3,ok}},
      {?eh,test_stats,{4,0,{5,6}}},
      {?eh,tc_start,{user_skip_4_SUITE,tc4}},
      {?eh,tc_done,{user_skip_4_SUITE,tc4,ok}},
      {?eh,test_stats,{5,0,{5,6}}},
      {?eh,tc_start,{user_skip_4_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{end_per_group,g2,[]},ok}}],

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g3,[]},{skipped,"Group skipped"}}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,tc5,"Group skipped"}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,tc6,"Group skipped"}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,tc7,"Group skipped"}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,tc8,"Group skipped"}},
      {?eh,test_stats,{5,0,{5,10}}},
      {?eh,tc_auto_skip,{user_skip_4_SUITE,end_per_group,"Group skipped"}}],

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g5,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g5,[]},ok}},
      {?eh,tc_start,{user_skip_4_SUITE,tc9}},
      {?eh,tc_done,{user_skip_4_SUITE,tc9,ok}},
      {?eh,test_stats,{6,0,{5,10}}},
      [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g6,[]}}},
       {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g6,[]},{skipped,"Group skipped"}}},
       {?eh,tc_auto_skip,{user_skip_4_SUITE,tc10,"Group skipped"}},
       {?eh,test_stats,{6,0,{5,11}}},
       {?eh,tc_auto_skip,{user_skip_4_SUITE,tc11,"Group skipped"}},
       {?eh,test_stats,{6,0,{5,12}}},
       {?eh,tc_auto_skip,{user_skip_4_SUITE,end_per_group,"Group skipped"}}],
       {?eh,tc_start,{user_skip_4_SUITE,{end_per_group,g5,[]}}},
       {?eh,tc_done,{user_skip_4_SUITE,{end_per_group,g5,[]},ok}}],

     {?eh,tc_start,{user_skip_4_SUITE,end_per_suite}},
     {?eh,tc_done,{user_skip_4_SUITE,end_per_suite,ok}},

         {ct_test_support_eh,tc_start,{user_skip_5_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_5_SUITE,init_per_suite,
		   {skipped,{bad,'Whole suite skipped'}}}},
     {?eh,tc_auto_skip,{user_skip_5_SUITE,tc1,{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{5,13}}},
     {?eh,tc_auto_skip,{user_skip_5_SUITE,tc2,{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{5,14}}},
     {?eh,tc_auto_skip,{user_skip_5_SUITE,tc3,{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{5,15}}},
     {?eh,tc_auto_skip,{user_skip_5_SUITE,tc4,{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{5,16}}},
     {?eh,tc_auto_skip,{user_skip_5_SUITE,end_per_suite,{bad,'Whole suite skipped'}}},
     
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
