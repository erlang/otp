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
%%% File: ct_skip_SUITE
%%%
%%% Description: 
%%% Test auto- and user-skip functionality
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_skip_SUITE).

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
    [auto_skip, user_skip, testspec_skip].

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
	      Join(DataDir, "auto_skip_11_SUITE"),
	      Join(DataDir, "auto_skip_12_SUITE")
	     ],

    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(auto_skip, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

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
	      Join(DataDir, "user_skip_5_SUITE"),
	      Join(DataDir, "user_skip_6_SUITE")],

    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(user_skip, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(user_skip),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 
testspec_skip(Config) when is_list(Config) ->
    TestDir = filename:join(?config(data_dir, Config),
			    filename:join("skip", "test")),
    TestSpec1 = [{suites, TestDir, user_skip_7_SUITE},
		 {skip_cases, TestDir, user_skip_7_SUITE, [tc1,tc3], "SKIPPED"}],

    TestSpec2 = [{suites, TestDir, user_skip_7_SUITE},
		 {skip_groups, TestDir, user_skip_7_SUITE, ptop1, "SKIPPED"}],

    TestSpec3 = [{suites, TestDir, user_skip_7_SUITE},
		 {skip_groups, TestDir, user_skip_7_SUITE, psub1, "SKIPPED"}],

    TestSpec4 = [{suites, TestDir, user_skip_7_SUITE},
		 {skip_suites, TestDir, user_skip_7_SUITE, "SKIPPED"}],

    TestSpec5 = [{groups, TestDir, user_skip_6_SUITE, ptop1},
		 {skip_groups, TestDir, user_skip_6_SUITE, psub1, "SKIPPED"}],

    {Opts,ERPid} = setup_testspec([{ts1,TestSpec1},
    				   {ts2,TestSpec2},
    				   {ts3,TestSpec3},
    				   {ts4,TestSpec4},
    				   {ts5,TestSpec5}], Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(testspec_skip, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(testspec_skip),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
setup_testspec(TestSpecs, Config) ->
    SpecFiles =
	[begin SpecFile = filename:join(?config(priv_dir, Config),
					atom_to_list(SpecName)++".spec"),
	       {ok,Dev} = file:open(SpecFile, [write]),
	       [io:format(Dev, "~p.~n", [Term]) || Term <- TestSpec],
	       file:close(Dev),
	       SpecFile
	 end || {SpecName,TestSpec} <- TestSpecs],
    setup({spec,SpecFiles}, Config).

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
     {?eh,start_info,{12,12,43}},

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
      {auto_skip_2_SUITE,{tc1,g1},{failed,{auto_skip_2_SUITE,init_per_suite,
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
       {auto_skipped,{failed,{auto_skip_3_SUITE,init_per_testcase,
			      {'init_per_testcase for tc1 failed','_'}}}}}},
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
		   {auto_skipped,{failed,{auto_skip_4_SUITE,init_per_testcase,
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
       {auto_skip_5_SUITE,{tc1,g1},{failed,{auto_skip_5_SUITE,init_per_group,
					    {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,6}}},    
      {?eh,tc_auto_skip,
       {auto_skip_5_SUITE,{tc2,g1},{failed,{auto_skip_5_SUITE,init_per_group,
					    {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,7}}},
      {?eh,tc_auto_skip,
       {auto_skip_5_SUITE,{end_per_group,g1},
	{failed,{auto_skip_5_SUITE,init_per_group,
		 {'EXIT',{group,g1,failed}}}}}}],

     {?eh,tc_start,{auto_skip_5_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_5_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{auto_skip_6_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_6_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{auto_skip_6_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,
       {auto_skip_6_SUITE,{init_per_group,g1,[]},{failed,{error,{group,g1,failed}}}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,{tc1,g1},{failed,{auto_skip_6_SUITE,init_per_group,
					    {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,8}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,{tc3,g2},{failed,{auto_skip_6_SUITE,init_per_group,
					    {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,9}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,{tc4,g2},{failed,{auto_skip_6_SUITE,init_per_group,
					    {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,10}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,{tc2,g1},{failed,{auto_skip_6_SUITE,init_per_group,
					    {'EXIT',{group,g1,failed}}}}}},
      {?eh,test_stats,{2,0,{0,11}}},
      {?eh,tc_auto_skip,
       {auto_skip_6_SUITE,{end_per_group,g1},
	{failed,{auto_skip_6_SUITE,init_per_group,
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
	{auto_skip_6_SUITE,{tc3,g4},{failed,{auto_skip_6_SUITE,init_per_group,
					     {'EXIT',{group,g4,failed}}}}}},
       {?eh,test_stats,{3,0,{0,12}}},
       {?eh,tc_auto_skip,
	{auto_skip_6_SUITE,{tc4,g4},{failed,{auto_skip_6_SUITE,init_per_group,
					     {'EXIT',{group,g4,failed}}}}}},
       {?eh,test_stats,{3,0,{0,13}}},
       {?eh,tc_auto_skip,
	{auto_skip_6_SUITE,{end_per_group,g4},
	 {failed,{auto_skip_6_SUITE,init_per_group,
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
		   {auto_skipped,{failed,{auto_skip_9_SUITE,init_per_testcase,bad_return}}}}},
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
		    {auto_skipped,{failed,{auto_skip_9_SUITE,init_per_testcase,bad_return}}}}},
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
	   {auto_skipped,{failed,{auto_skip_9_SUITE,init_per_testcase,
			     {{badmatch,undefined},'_'}}}}}},
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
		   {auto_skipped,{require_failed_in_suite0,
				  {not_available,undefined_config_variable}}}}},
     {?eh,tc_auto_skip,{auto_skip_10_SUITE,tc1,
			{require_failed_in_suite0,
			 {not_available,undefined_config_variable}}}},
     {?eh,test_stats,{9,0,{0,21}}},
     {?eh,tc_auto_skip,{auto_skip_10_SUITE,tc2,
			{require_failed_in_suite0,
			 {not_available,undefined_config_variable}}}},
     {?eh,test_stats,{9,0,{0,22}}},
     {?eh,tc_auto_skip,{auto_skip_10_SUITE,end_per_suite,
			{require_failed_in_suite0,
			 {not_available,undefined_config_variable}}}},

     {?eh,tc_start,{auto_skip_11_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{auto_skip_11_SUITE,tc1}},
     {?eh,tc_done,{auto_skip_11_SUITE,tc1,
		   {auto_skipped,{require_failed,
				  {not_available,undefined_config_variable}}}}},
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
	   {auto_skipped,{require_failed,
			  {not_available,undefined_config_variable}}}}},
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
     
     {?eh,tc_start,{auto_skip_12_SUITE,init_per_suite}},
     {?eh,tc_done,{auto_skip_12_SUITE,init_per_suite,ok}},

     [{?eh,tc_start,{ct_framework,{init_per_group,g1,
				   [{suite,auto_skip_12_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g1,
				  [{suite,auto_skip_12_SUITE}]},
		    {auto_skipped,
		     {require_failed,{not_available,unknown_variable_g1}}}}},
      {?eh,tc_auto_skip,{auto_skip_12_SUITE,{tc1,g1},
			 {require_failed,{not_available,unknown_variable_g1}}}},
      {?eh,test_stats,{10,0,{0,25}}},
      {?eh,tc_auto_skip,{auto_skip_12_SUITE,{tc2,g1},
			 {require_failed,{not_available,unknown_variable_g1}}}},
      {?eh,test_stats,{10,0,{0,26}}},
      {?eh,tc_auto_skip,{auto_skip_12_SUITE,{tc3,g2},
			 {require_failed,{not_available,unknown_variable_g1}}}},
      {?eh,test_stats,{10,0,{0,27}}},
      {?eh,tc_auto_skip,{ct_framework,{end_per_group,g1},
			 {require_failed,{not_available,unknown_variable_g1}}}}],
     
     [{?eh,tc_start,{ct_framework,{init_per_group,g1,
				   [{suite,auto_skip_12_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g1,
				  [{suite,auto_skip_12_SUITE}]},
		    {auto_skipped,
		     {require_failed,{not_available,unknown_variable_g1}}}}},
      {?eh,tc_auto_skip,{auto_skip_12_SUITE,{tc1,g1},
			 {require_failed,{not_available,unknown_variable_g1}}}},
      {?eh,test_stats,{10,0,{0,28}}},
      {?eh,tc_auto_skip,{auto_skip_12_SUITE,{tc2,g1},
			 {require_failed,{not_available,unknown_variable_g1}}}},
      {?eh,test_stats,{10,0,{0,29}}},
      {?eh,tc_auto_skip,{auto_skip_12_SUITE,{tc3,g2},
			 {require_failed,{not_available,unknown_variable_g1}}}},
      {?eh,test_stats,{10,0,{0,30}}},
      {?eh,tc_auto_skip,{ct_framework,{end_per_group,g1},
			 {require_failed,{not_available,unknown_variable_g1}}}}],

     [{?eh,tc_start,{ct_framework,{init_per_group,g3,
				   [{suite,auto_skip_12_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{init_per_group,g3,
				  [{suite,auto_skip_12_SUITE}]},ok}},
      {?eh,tc_start,{auto_skip_12_SUITE,tc1}},
      {?eh,tc_done,{auto_skip_12_SUITE,tc1,ok}},
      {?eh,test_stats,{11,0,{0,30}}},
      {?eh,tc_start,{auto_skip_12_SUITE,tc2}},
      {?eh,tc_done,{auto_skip_12_SUITE,tc2,ok}},
      {?eh,test_stats,{12,0,{0,30}}},
      [{?eh,tc_start,{ct_framework,{init_per_group,g4,
				    [{suite,auto_skip_12_SUITE}]}}},
       {?eh,tc_done,{ct_framework,{init_per_group,g4,
				   [{suite,auto_skip_12_SUITE}]},
		     {auto_skipped,
		      {require_failed,{not_available,unknown_variable_g4}}}}},
       {?eh,tc_auto_skip,{auto_skip_12_SUITE,{tc3,g4},
			  {require_failed,{not_available,unknown_variable_g4}}}},
       {?eh,test_stats,{12,0,{0,31}}},
       {?eh,tc_auto_skip,{ct_framework,{end_per_group,g4},
			  {require_failed,{not_available,unknown_variable_g4}}}}],

      {?eh,tc_start,{ct_framework,{end_per_group,g3,
				   [{suite,auto_skip_12_SUITE}]}}},
      {?eh,tc_done,{ct_framework,{end_per_group,g3,
				  [{suite,auto_skip_12_SUITE}]},ok}}],

     {?eh,tc_start,{auto_skip_12_SUITE,end_per_suite}},
     {?eh,tc_done,{auto_skip_12_SUITE,end_per_suite,ok}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(user_skip) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{6,6,35}},

     {?eh,tc_start,{user_skip_1_SUITE,init_per_suite}},
     {?eh,tc_done,
      {user_skip_1_SUITE,init_per_suite,{skipped,"Whole suite skipped"}}},
     {?eh,tc_user_skip,
      {user_skip_1_SUITE,tc1,"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{1,0}}},
     {?eh,tc_user_skip,
      {user_skip_1_SUITE,{tc2,g1},"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{2,0}}},
     {?eh,tc_user_skip,
      {user_skip_1_SUITE,{tc3,g1},"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{3,0}}},
     {?eh,tc_user_skip,
      {user_skip_1_SUITE,tc4,"Whole suite skipped"}},
     {?eh,test_stats,{0,0,{4,0}}},
     {?eh,tc_user_skip,
      {user_skip_1_SUITE,end_per_suite,"Whole suite skipped"}},

     {?eh,tc_start,{user_skip_2_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_2_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{user_skip_2_SUITE,tc1}},
     {?eh,tc_done,{user_skip_2_SUITE,tc1,{skipped,{tc1,skipped}}}},
     {?eh,test_stats,{0,0,{5,0}}},

     [{?eh,tc_start,{user_skip_2_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_2_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_start,{user_skip_2_SUITE,tc2}},
      {?eh,tc_done,{user_skip_2_SUITE,tc2,ok}},
      {?eh,test_stats,{1,0,{5,0}}},
      {?eh,tc_start,{user_skip_2_SUITE,tc3}},
      {?eh,tc_done,{user_skip_2_SUITE,tc3,{skipped,{tc3,skipped}}}},
      {?eh,test_stats,{1,0,{6,0}}},
      {?eh,tc_start,{user_skip_2_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_2_SUITE,{end_per_group,g1,[]},ok}}],

     {?eh,tc_start,{user_skip_2_SUITE,tc4}},
     {?eh,tc_done,{user_skip_2_SUITE,tc4,ok}},
     {?eh,test_stats,{2,0,{6,0}}},
     {?eh,tc_start,{user_skip_2_SUITE,end_per_suite}},
     {?eh,tc_done,{user_skip_2_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{user_skip_3_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_3_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{user_skip_3_SUITE,tc1}},
     {?eh,tc_done,{user_skip_3_SUITE,tc1,{skipped,"Test case skipped"}}},
     {?eh,test_stats,{2,0,{7,0}}},

     [{?eh,tc_start,{user_skip_3_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_3_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_start,{user_skip_3_SUITE,tc2}},
      {?eh,tc_done,{user_skip_3_SUITE,tc2,ok}},
      {?eh,test_stats,{3,0,{7,0}}},
      {?eh,tc_start,{user_skip_3_SUITE,tc3}},
      {?eh,tc_done,{user_skip_3_SUITE,tc3,{skipped,"Test case skipped"}}},
      {?eh,test_stats,{3,0,{8,0}}},
      {?eh,tc_start,{user_skip_3_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_3_SUITE,{end_per_group,g1,[]},ok}}],

     {?eh,tc_start,{user_skip_3_SUITE,tc4}},
     {?eh,tc_done,{user_skip_3_SUITE,tc4,
		   {skipped,{proc_info,{{current_function,{user_skip_3_SUITE,tc4,1}},
					{initial_call,{erlang,apply,2}}}}}}},
     {?eh,test_stats,{3,0,{9,0}}},
     {?eh,tc_start,{user_skip_3_SUITE,end_per_suite}},
     {?eh,tc_done,{user_skip_3_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{user_skip_4_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_4_SUITE,init_per_suite,ok}},

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g1,[]},{skipped,"Group skipped"}}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{tc1,g1},"Group skipped"}},   
      {?eh,test_stats,{3,0,{10,0}}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{tc2,g1},"Group skipped"}},
      {?eh,test_stats,{3,0,{11,0}}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{end_per_group,g1},"Group skipped"}}],

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_start,{user_skip_4_SUITE,tc3}},
      {?eh,tc_done,{user_skip_4_SUITE,tc3,ok}},
      {?eh,test_stats,{4,0,{11,0}}},
      {?eh,tc_start,{user_skip_4_SUITE,tc4}},
      {?eh,tc_done,{user_skip_4_SUITE,tc4,ok}},
      {?eh,test_stats,{5,0,{11,0}}},
      {?eh,tc_start,{user_skip_4_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{end_per_group,g2,[]},ok}}],

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g3,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g3,[]},{skipped,"Group skipped"}}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{tc5,g3},"Group skipped"}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{tc6,g4},"Group skipped"}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{tc7,g4},"Group skipped"}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{tc8,g3},"Group skipped"}},
      {?eh,test_stats,{5,0,{15,0}}},
      {?eh,tc_user_skip,{user_skip_4_SUITE,{end_per_group,g3},"Group skipped"}}],

     [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g5,[]}}},
      {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g5,[]},ok}},
      {?eh,tc_start,{user_skip_4_SUITE,tc9}},
      {?eh,tc_done,{user_skip_4_SUITE,tc9,ok}},
      {?eh,test_stats,{6,0,{15,0}}},
      [{?eh,tc_start,{user_skip_4_SUITE,{init_per_group,g6,[]}}},
       {?eh,tc_done,{user_skip_4_SUITE,{init_per_group,g6,[]},{skipped,"Group skipped"}}},
       {?eh,tc_user_skip,{user_skip_4_SUITE,{tc10,g6},"Group skipped"}},
       {?eh,test_stats,{6,0,{16,0}}},
       {?eh,tc_user_skip,{user_skip_4_SUITE,{tc11,g6},"Group skipped"}},
       {?eh,test_stats,{6,0,{17,0}}},
       {?eh,tc_user_skip,{user_skip_4_SUITE,{end_per_group,g6},"Group skipped"}}],
       {?eh,tc_start,{user_skip_4_SUITE,{end_per_group,g5,[]}}},
       {?eh,tc_done,{user_skip_4_SUITE,{end_per_group,g5,[]},ok}}],

     {?eh,tc_start,{user_skip_4_SUITE,end_per_suite}},
     {?eh,tc_done,{user_skip_4_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{user_skip_5_SUITE,init_per_suite}},
     {?eh,tc_done,{user_skip_5_SUITE,init_per_suite,
		   {skipped,{bad,'Whole suite skipped'}}}},
     {?eh,tc_user_skip,{user_skip_5_SUITE,tc1,{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{18,0}}},
     {?eh,tc_user_skip,{user_skip_5_SUITE,{tc2,g1},{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{19,0}}},
     {?eh,tc_user_skip,{user_skip_5_SUITE,{tc3,g1},{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{20,0}}},
     {?eh,tc_user_skip,{user_skip_5_SUITE,tc4,{bad,'Whole suite skipped'}}},
     {?eh,test_stats,{6,0,{21,0}}},
     {?eh,tc_user_skip,{user_skip_5_SUITE,end_per_suite,{bad,'Whole suite skipped'}}},
     
     {parallel,
      [{?eh,tc_start,{user_skip_6_SUITE,{init_per_group,ptop1,[parallel]}}},
       {?eh,tc_done,{user_skip_6_SUITE,
		     {init_per_group,ptop1,[parallel]},
		     {skipped,"Top group skipped"}}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc1,ptop1},"Top group skipped"}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc3,psub1},"Top group skipped"}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc4,psub1},"Top group skipped"}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc2,ptop1},"Top group skipped"}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{end_per_group,ptop1},
			  "Top group skipped"}}]},
      
     {parallel,
      [{?eh,tc_start,{user_skip_6_SUITE,{init_per_group,ptop2,[parallel]}}},
       {?eh,tc_done,{user_skip_6_SUITE,{init_per_group,ptop2,[parallel]},ok}},
       {?eh,tc_start,{user_skip_6_SUITE,tc1}},
       {?eh,tc_done,{user_skip_6_SUITE,tc1,ok}},
       
       {parallel,
	[{?eh,tc_start,{user_skip_6_SUITE,{init_per_group,psub2,[parallel]}}},
	 {?eh,tc_done,{user_skip_6_SUITE,
		       {init_per_group,psub2,[parallel]},
		       {skipped,"Sub group skipped"}}},
	 {?eh,tc_user_skip,{user_skip_6_SUITE,{tc3,psub2},"Sub group skipped"}},
	 {?eh,tc_user_skip,{user_skip_6_SUITE,{tc4,psub2},"Sub group skipped"}},
	 {?eh,tc_user_skip,{user_skip_6_SUITE,{end_per_group,psub2},
			    "Sub group skipped"}}]},
       
       {?eh,tc_start,{user_skip_6_SUITE,tc2}},
       {?eh,tc_done,{user_skip_6_SUITE,tc2,ok}},
       {?eh,test_stats,{8,0,{27,0}}},
       {?eh,tc_start,{user_skip_6_SUITE,{end_per_group,ptop2,[parallel]}}},
       {?eh,tc_done,{user_skip_6_SUITE,{end_per_group,ptop2,[parallel]},ok}}]},
       
       {?eh,test_done,{'DEF','STOP_TIME'}},
       {?eh,stop_logging,[]}
    ];

test_events(testspec_skip) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{ct_framework,init_per_suite}},
     {?eh,tc_done,{ct_framework,init_per_suite,ok}},
     {parallel,
      [{?eh,tc_start,
	{user_skip_7_SUITE,{init_per_group,ptop1,[parallel]}}},
       {?eh,tc_done,
	{user_skip_7_SUITE,{init_per_group,ptop1,[parallel]},ok}},
       {?eh,tc_user_skip,{user_skip_7_SUITE,{tc1,ptop1},"SKIPPED"}},
       {?eh,test_stats,{0,0,{1,0}}},
       {parallel,
	[{?eh,tc_start,
          {user_skip_7_SUITE,{init_per_group,psub1,[parallel]}}},
	 {?eh,tc_done,
          {user_skip_7_SUITE,{init_per_group,psub1,[parallel]},ok}},
	 {?eh,tc_user_skip,{user_skip_7_SUITE,{tc3,psub1},"SKIPPED"}},
	 {?eh,tc_start,{user_skip_7_SUITE,tc4}},
	 {?eh,tc_done,{user_skip_7_SUITE,tc4,ok}},
	 {?eh,test_stats,{1,0,{2,0}}},
	 {?eh,tc_start,
	  {user_skip_7_SUITE,{end_per_group,psub1,[parallel]}}},
	 {?eh,tc_done,
          {user_skip_7_SUITE,{end_per_group,psub1,[parallel]},ok}}]},
       {?eh,tc_start,{user_skip_7_SUITE,tc2}},
       {?eh,tc_done,{user_skip_7_SUITE,tc2,ok}},
       {?eh,test_stats,{2,0,{2,0}}},
       {?eh,tc_start,
	{user_skip_7_SUITE,{end_per_group,ptop1,[parallel]}}},
       {?eh,tc_done,
	{user_skip_7_SUITE,{end_per_group,ptop1,[parallel]},ok}}]},
     {?eh,tc_start,{ct_framework,end_per_suite}},
     {?eh,tc_done,{ct_framework,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},
     
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{ct_framework,init_per_suite}},
     {?eh,tc_done,{ct_framework,init_per_suite,ok}},
     {?eh,tc_user_skip,{user_skip_7_SUITE,{init_per_group,ptop1},"SKIPPED"}},
     {?eh,tc_user_skip,{user_skip_7_SUITE,{tc1,ptop1},"SKIPPED"}},
     {?eh,test_stats,{0,0,{1,0}}},
     {?eh,tc_user_skip,{user_skip_7_SUITE,{tc3,psub1},"SKIPPED"}},
     {?eh,test_stats,{0,0,{2,0}}},
     {?eh,tc_user_skip,{user_skip_7_SUITE,{tc4,psub1},"SKIPPED"}},
     {?eh,test_stats,{0,0,{3,0}}},
     {?eh,tc_user_skip,{user_skip_7_SUITE,{tc2,ptop1},"SKIPPED"}},
     {?eh,test_stats,{0,0,{4,0}}},
     {?eh,tc_user_skip,{user_skip_7_SUITE,{end_per_group,ptop1},"SKIPPED"}},
     {?eh,tc_start,{ct_framework,end_per_suite}},
     {?eh,tc_done,{ct_framework,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},
     
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{ct_framework,init_per_suite}},
     {?eh,tc_done,{ct_framework,init_per_suite,ok}},
     {parallel,
      [{?eh,tc_start,
	{user_skip_7_SUITE,{init_per_group,ptop1,[parallel]}}},
       {?eh,tc_done,
	{user_skip_7_SUITE,{init_per_group,ptop1,[parallel]},ok}},
       {?eh,tc_user_skip,
	{user_skip_7_SUITE,{init_per_group,psub1},"SKIPPED"}},
       {?eh,tc_user_skip,{user_skip_7_SUITE,{tc3,psub1},"SKIPPED"}},
       {?eh,tc_user_skip,{user_skip_7_SUITE,{tc4,psub1},"SKIPPED"}},
       {?eh,test_stats,{0,0,{2,0}}},
       {?eh,tc_user_skip,{user_skip_7_SUITE,{end_per_group,psub1},"SKIPPED"}},
       {?eh,tc_start,{user_skip_7_SUITE,tc1}},
       {?eh,tc_done,{user_skip_7_SUITE,tc1,ok}},
       {?eh,tc_start,{user_skip_7_SUITE,tc2}},
       {?eh,tc_done,{user_skip_7_SUITE,tc2,ok}},
       {?eh,test_stats,{2,0,{2,0}}},
       {?eh,tc_start,{user_skip_7_SUITE,{end_per_group,ptop1,[parallel]}}},
       {?eh,tc_done,{user_skip_7_SUITE,{end_per_group,ptop1,[parallel]},ok}}]},
     {?eh,tc_start,{ct_framework,end_per_suite}},
     {?eh,tc_done,{ct_framework,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,0}},
     {?eh,tc_user_skip,{user_skip_7_SUITE,all,"SKIPPED"}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]},

     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{ct_framework,init_per_suite}},
     {?eh,tc_done,{ct_framework,init_per_suite,ok}},
     {parallel,
      [{?eh,tc_start,{user_skip_6_SUITE,{init_per_group,ptop1,[parallel]}}},
       {?eh,tc_done,{user_skip_6_SUITE,
		     {init_per_group,ptop1,[parallel]},
		     {skipped,"Top group skipped"}}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc1,ptop1},"Top group skipped"}},
       {?eh,test_stats,{0,0,{1,0}}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc3,psub1},"SKIPPED"}},
       {?eh,test_stats,{0,0,{2,0}}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc4,psub1},"SKIPPED"}},
       {?eh,test_stats,{0,0,{3,0}}},
       {?eh,tc_user_skip,{user_skip_6_SUITE,{tc2,ptop1},"Top group skipped"}},
       {?eh,test_stats,{0,0,{4,0}}},
       {?eh,tc_user_skip,
	{user_skip_6_SUITE,{end_per_group,ptop1},"Top group skipped"}}]},
     {?eh,tc_start,{ct_framework,end_per_suite}},
     {?eh,tc_done,{ct_framework,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].


