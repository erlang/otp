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
%%% File: ct_groups_test_1_SUITE
%%%
%%% Description: 
%%% Test some simple test case group scenarios.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_groups_test_1_SUITE).

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
    [groups_suite_1, groups_suite_2, groups_suites_1,
     groups_dir_1, groups_dirs_1].

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

groups_suite_1(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Suite = filename:join(DataDir, "groups_1/test/groups_11_SUITE"),

    {Opts,ERPid} = setup({suite,Suite}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(groups_suite_1, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(groups_suite_1),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).
    

%%%-----------------------------------------------------------------
%%% 

groups_suite_2(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Suite = filename:join(DataDir, "groups_1/test/groups_12_SUITE"),

    {Opts,ERPid} = setup({suite,Suite}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(groups_suite_2, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(groups_suite_2),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).  
    

%%%-----------------------------------------------------------------
%%% 

groups_suites_1(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Suites = [filename:join(DataDir, "groups_1/test/groups_11_SUITE"),
	      filename:join(DataDir, "groups_1/test/groups_12_SUITE")],

    {Opts,ERPid} = setup({suite,Suites}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(groups_suites_1, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(groups_suites_1),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).  


%%%-----------------------------------------------------------------
%%% 

groups_dir_1(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Dir = filename:join(DataDir, "groups_1"),

    {Opts,ERPid} = setup({dir,Dir}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(groups_dir_1, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(groups_dir_1),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).  

%%%-----------------------------------------------------------------
%%% 

groups_dirs_1(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Dirs = [filename:join(DataDir, "groups_1"),
	    filename:join(DataDir, "groups_2")],

    {Opts,ERPid} = setup({dir,Dirs}, Config),
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(groups_dirs_1, 
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(groups_dirs_1),
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

test_events(groups_suite_1) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,15}},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1a,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{2,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
     {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1a,[]}}},
     {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},ok}}],

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1b,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{5,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1b,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,testcase_2}},
     {?eh,tc_done,{groups_11_SUITE,testcase_2,ok}},
     {?eh,test_stats,{6,0,{0,0}}},
     
     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_2,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_2a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2a,ok}},
      {?eh,test_stats,{7,0,{0,0}}},
       [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_3,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_3,[]},ok}},
	{?eh,tc_start,{groups_11_SUITE,testcase_3a}},
	{?eh,tc_done,{groups_11_SUITE,testcase_3a,ok}},
	{?eh,test_stats,{8,0,{0,0}}},
	{?eh,tc_start,{groups_11_SUITE,testcase_3b}},
	{?eh,tc_done,{groups_11_SUITE,testcase_3b,ok}},
	{?eh,test_stats,{9,0,{0,0}}},
	{?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_3,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_3,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,testcase_2b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2b,ok}},
      {?eh,test_stats,{10,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_2,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,testcase_3}},
     {?eh,tc_done,{groups_11_SUITE,testcase_3,ok}},
     {?eh,test_stats,{11,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_4,[]},ok}},
       [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_5,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_5,[]},ok}},
	{?eh,tc_start,{groups_11_SUITE,testcase_5a}},
	{?eh,tc_done,{groups_11_SUITE,testcase_5a,ok}},
	{?eh,test_stats,{12,0,{0,0}}},
	 [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_6,[]}}},
	  {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_6,[]},ok}},
	   [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_7,[]}}},
	    {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_7,[]},ok}},
	    {?eh,tc_start,{groups_11_SUITE,testcase_7a}},
	    {?eh,tc_done,{groups_11_SUITE,testcase_7a,ok}},
	    {?eh,test_stats,{13,0,{0,0}}},
	    {?eh,tc_start,{groups_11_SUITE,testcase_7b}},
	    {?eh,tc_done,{groups_11_SUITE,testcase_7b,ok}},
	    {?eh,test_stats,{14,0,{0,0}}},
	    {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_7,[]}}},
	    {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_7,[]},ok}}],
	  {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_6,[]}}},
	  {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_6,[]},ok}}],
	 {?eh,tc_start,{groups_11_SUITE,testcase_5b}},
	 {?eh,tc_done,{groups_11_SUITE,testcase_5b,ok}},
	{?eh,test_stats,{15,0,{0,0}}},
	{?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_5,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_5,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(groups_suite_2) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,unknown}},
     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,init_per_suite,ok}},

     {shuffle,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]}}},
	       {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]},ok}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
	       {?eh,test_stats,{1,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},
	       {?eh,test_stats,{2,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1c}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1c,ok}},
	       {?eh,test_stats,{3,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]}}},
	       {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]},ok}}]},

     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]},ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1a}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},		
		{?eh,test_stats,{5,0,{0,0}}},
		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]},ok}}]},

     {?eh,tc_start,{groups_12_SUITE,testcase_1}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1,ok}},
     {?eh,test_stats,{6,0,{0,0}}},
     {?eh,tc_start,{groups_12_SUITE,testcase_2}},
     {?eh,tc_done,{groups_12_SUITE,testcase_2,ok}},
     {?eh,test_stats,{7,0,{0,0}}},
     
     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},		 
 		 {?eh,tc_start,{groups_12_SUITE,testcase_2a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_2a,ok}},

		 [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
		  {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]},ok}},
		   {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		   {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		   {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		   {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		  {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}},
		  {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]},ok}}],

		 [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[]}}},
		  {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[]},ok}},
		   {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		   {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		   {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		   {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		  {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[]}}},
		  {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[]},ok}}],

		{?eh,tc_start,{groups_12_SUITE,testcase_2b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_2b,ok}},
		{?eh,test_stats,{13,0,{0,0}}},

		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},

     {?eh,tc_start,{groups_12_SUITE,testcase_3}},
     {?eh,tc_done,{groups_12_SUITE,testcase_3,ok}},
     {?eh,test_stats,{14,0,{0,0}}},

     [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_4,[]},ok}},
      
      {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_5a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_5a,ok}},
		 {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
			    {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
		  
			    [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]}}},
			     {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]},ok}},
			      {?eh,tc_start,{groups_12_SUITE,testcase_7a}},
			      {?eh,tc_done,{groups_12_SUITE,testcase_7a,ok}},
			      {?eh,tc_start,{groups_12_SUITE,testcase_7b}},
			      {?eh,tc_done,{groups_12_SUITE,testcase_7b,ok}},
			     {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]}}},
			     {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]},ok}}],

			    {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
			    {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}]},

		  {?eh,tc_start,{groups_12_SUITE,testcase_5b}},
		  {?eh,tc_done,{groups_12_SUITE,testcase_5b,ok}},
		 {?eh,test_stats,{18,0,{0,0}}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},

      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_4,[]},ok}}],

      {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
      {?eh,tc_done,{groups_12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(groups_suites_1) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,unknown}},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1a,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{2,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1a,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},ok}}],

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1b,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{5,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1b,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,testcase_2}},
     {?eh,tc_done,{groups_11_SUITE,testcase_2,ok}},
     {?eh,test_stats,{6,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_2,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_2a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2a,ok}},
      {?eh,test_stats,{7,0,{0,0}}},
      [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_3,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_3,[]},ok}},
       {?eh,tc_start,{groups_11_SUITE,testcase_3a}},
       {?eh,tc_done,{groups_11_SUITE,testcase_3a,ok}},
       {?eh,test_stats,{8,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,testcase_3b}},
       {?eh,tc_done,{groups_11_SUITE,testcase_3b,ok}},
       {?eh,test_stats,{9,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_3,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_3,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,testcase_2b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2b,ok}},
      {?eh,test_stats,{10,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_2,[]},ok}}],
     
     {?eh,tc_start,{groups_11_SUITE,testcase_3}},
     {?eh,tc_done,{groups_11_SUITE,testcase_3,ok}},
     {?eh,test_stats,{11,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_4,[]},ok}},
       [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_5,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_5,[]},ok}},
	{?eh,tc_start,{groups_11_SUITE,testcase_5a}},
	{?eh,tc_done,{groups_11_SUITE,testcase_5a,ok}},
	{?eh,test_stats,{12,0,{0,0}}},
	[{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_6,[]}}},
	 {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_6,[]},ok}},
	 [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_7,[]}}},
	  {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_7,[]},ok}},
	  {?eh,tc_start,{groups_11_SUITE,testcase_7a}},
	  {?eh,tc_done,{groups_11_SUITE,testcase_7a,ok}},
	  {?eh,test_stats,{13,0,{0,0}}},
	  {?eh,tc_start,{groups_11_SUITE,testcase_7b}},
	  {?eh,tc_done,{groups_11_SUITE,testcase_7b,ok}},
	  {?eh,test_stats,{14,0,{0,0}}},
	  {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_7,[]}}},
	  {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_7,[]},ok}}],
	 {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_6,[]}}},
	 {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_6,[]},ok}}],
	{?eh,tc_start,{groups_11_SUITE,testcase_5b}},
	{?eh,tc_done,{groups_11_SUITE,testcase_5b,ok}},
	{?eh,test_stats,{15,0,{0,0}}},
	{?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_5,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_5,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,init_per_suite,ok}},

     {shuffle,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]}}},
	       {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]},ok}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1c}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1c,ok}},
	       {?eh,test_stats,{16,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},
	       {?eh,test_stats,{17,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
	       {?eh,test_stats,{18,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]}}},
	       {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]},ok}}]},
     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]},ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1a}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},
		{?eh,test_stats,{20,0,{0,0}}},
		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]},ok}}]},
     {?eh,tc_start,{groups_12_SUITE,testcase_1}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1,ok}},
     {?eh,test_stats,{21,0,{0,0}}},
     {?eh,tc_start,{groups_12_SUITE,testcase_2}},
     {?eh,tc_done,{groups_12_SUITE,testcase_2,ok}},
     {?eh,test_stats,{22,0,{0,0}}},

     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_2a}},
		{?eh,tc_done,{groups_12_SUITE,testcase_2a,ok}},
		[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]},ok}}],
		[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[]},ok}}],
		{?eh,tc_start,{groups_12_SUITE,testcase_2b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_2b,ok}},
		{?eh,test_stats,{28,0,{0,0}}},
		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},
     
     {?eh,tc_start,{groups_12_SUITE,testcase_3}},
     {?eh,tc_done,{groups_12_SUITE,testcase_3,ok}},
     {?eh,test_stats,{29,0,{0,0}}},
     
     [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_4,[]},ok}},
       {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
		  {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
		  {?eh,tc_start,{groups_12_SUITE,testcase_5a}},
		  {?eh,tc_done,{groups_12_SUITE,testcase_5a,ok}},
		   {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
			      {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
			       [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]}}},
				{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]},ok}},
				 {?eh,tc_start,{groups_12_SUITE,testcase_7a}},
				 {?eh,tc_done,{groups_12_SUITE,testcase_7a,ok}},
				 {?eh,tc_start,{groups_12_SUITE,testcase_7b}},
				 {?eh,tc_done,{groups_12_SUITE,testcase_7b,ok}},
				{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]}}},
				{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]},ok}}],
			      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
			      {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}]},
		  {?eh,tc_start,{groups_12_SUITE,testcase_5b}},
		  {?eh,tc_done,{groups_12_SUITE,testcase_5b,ok}},
		  {?eh,test_stats,{33,0,{0,0}}},
		  {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
		  {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(groups_dir_1) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,2,unknown}},
     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1a,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{2,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1a,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},ok}}],

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1b,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{5,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1b,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,testcase_2}},
     {?eh,tc_done,{groups_11_SUITE,testcase_2,ok}},
     {?eh,test_stats,{6,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_2,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_2a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2a,ok}},
      {?eh,test_stats,{7,0,{0,0}}},
      [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_3,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_3,[]},ok}},
       {?eh,tc_start,{groups_11_SUITE,testcase_3a}},
       {?eh,tc_done,{groups_11_SUITE,testcase_3a,ok}},
       {?eh,test_stats,{8,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,testcase_3b}},
       {?eh,tc_done,{groups_11_SUITE,testcase_3b,ok}},
       {?eh,test_stats,{9,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_3,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_3,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,testcase_2b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2b,ok}},
      {?eh,test_stats,{10,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_2,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,testcase_3}},
     {?eh,tc_done,{groups_11_SUITE,testcase_3,ok}},
     {?eh,test_stats,{11,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_4,[]},ok}},
      [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_5,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_5,[]},ok}},
       {?eh,tc_start,{groups_11_SUITE,testcase_5a}},
       {?eh,tc_done,{groups_11_SUITE,testcase_5a,ok}},
       {?eh,test_stats,{12,0,{0,0}}},
       [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_6,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_6,[]},ok}},
	[{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_7,[]}}},
	 {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_7,[]},ok}},
         {?eh,tc_start,{groups_11_SUITE,testcase_7a}},
         {?eh,tc_done,{groups_11_SUITE,testcase_7a,ok}},
         {?eh,test_stats,{13,0,{0,0}}},
         {?eh,tc_start,{groups_11_SUITE,testcase_7b}},
         {?eh,tc_done,{groups_11_SUITE,testcase_7b,ok}},
         {?eh,test_stats,{14,0,{0,0}}},
	 {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_7,[]}}},
	 {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_7,[]},ok}}],
	{?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_6,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_6,[]},ok}}],
       {?eh,tc_start,{groups_11_SUITE,testcase_5b}},
       {?eh,tc_done,{groups_11_SUITE,testcase_5b,ok}},
       {?eh,test_stats,{15,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_5,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_5,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,init_per_suite,ok}},

     {shuffle,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]}}},
	       {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]},ok}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},
	       {?eh,test_stats,{16,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1c}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1c,ok}},
	       {?eh,test_stats,{17,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
	       {?eh,test_stats,{18,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]}}},
	       {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]},ok}}]},
     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]},ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1a}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},
		{?eh,test_stats,{20,0,{0,0}}},
		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]},ok}}]},
     {?eh,tc_start,{groups_12_SUITE,testcase_1}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1,ok}},
     {?eh,test_stats,{21,0,{0,0}}},
     {?eh,tc_start,{groups_12_SUITE,testcase_2}},
     {?eh,tc_done,{groups_12_SUITE,testcase_2,ok}},
     {?eh,test_stats,{22,0,{0,0}}},

     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_2a}},
		{?eh,tc_done,{groups_12_SUITE,testcase_2a,ok}},
		[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]},ok}}],
		[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[]},ok}}],
		{?eh,tc_start,{groups_12_SUITE,testcase_2b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_2b,ok}},
		{?eh,test_stats,{28,0,{0,0}}},
		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},

     {?eh,tc_start,{groups_12_SUITE,testcase_3}},
     {?eh,tc_done,{groups_12_SUITE,testcase_3,ok}},
     {?eh,test_stats,{29,0,{0,0}}},

     [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_4,[]},ok}},
      {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_5a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_5a,ok}},
		 {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
			    {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
			    [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]}}},
			     {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]},ok}},
			     {?eh,tc_start,{groups_12_SUITE,testcase_7a}},
			     {?eh,tc_done,{groups_12_SUITE,testcase_7a,ok}},
			     {?eh,tc_start,{groups_12_SUITE,testcase_7b}},
			     {?eh,tc_done,{groups_12_SUITE,testcase_7b,ok}},
			     {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]}}},
			     {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]},ok}}],
			    {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
			    {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}]},
		 {?eh,tc_start,{groups_12_SUITE,testcase_5b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_5b,ok}},
		 {?eh,test_stats,{33,0,{0,0}}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}];

test_events(groups_dirs_1) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,4,unknown}},

     {?eh,tc_start,{groups_11_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{groups_11_SUITE,testcase_1}},
     {?eh,tc_done,{groups_11_SUITE,testcase_1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1a,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1a,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{2,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1a,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1a,[]},ok}}],

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_1b,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{5,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_1b,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_1b,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,testcase_2}},
     {?eh,tc_done,{groups_11_SUITE,testcase_2,ok}},
     {?eh,test_stats,{6,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_2,[]},ok}},
      {?eh,tc_start,{groups_11_SUITE,testcase_2a}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2a,ok}},
      {?eh,test_stats,{7,0,{0,0}}},
      [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_3,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_3,[]},ok}},
       {?eh,tc_start,{groups_11_SUITE,testcase_3a}},
       {?eh,tc_done,{groups_11_SUITE,testcase_3a,ok}},
       {?eh,test_stats,{8,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,testcase_3b}},
       {?eh,tc_done,{groups_11_SUITE,testcase_3b,ok}},
       {?eh,test_stats,{9,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_3,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_3,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,testcase_2b}},
      {?eh,tc_done,{groups_11_SUITE,testcase_2b,ok}},
      {?eh,test_stats,{10,0,{0,0}}},
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_2,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_2,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,testcase_3}},
     {?eh,tc_done,{groups_11_SUITE,testcase_3,ok}},
     {?eh,test_stats,{11,0,{0,0}}},

     [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_4,[]},ok}},
      [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_5,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_5,[]},ok}},
       {?eh,tc_start,{groups_11_SUITE,testcase_5a}},
       {?eh,tc_done,{groups_11_SUITE,testcase_5a,ok}},
       {?eh,test_stats,{12,0,{0,0}}},
       [{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_6,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_6,[]},ok}},
	[{?eh,tc_start,{groups_11_SUITE,{init_per_group,test_group_7,[]}}},
	 {?eh,tc_done,{groups_11_SUITE,{init_per_group,test_group_7,[]},ok}},
         {?eh,tc_start,{groups_11_SUITE,testcase_7a}},
         {?eh,tc_done,{groups_11_SUITE,testcase_7a,ok}},
         {?eh,test_stats,{13,0,{0,0}}},
         {?eh,tc_start,{groups_11_SUITE,testcase_7b}},
         {?eh,tc_done,{groups_11_SUITE,testcase_7b,ok}},
         {?eh,test_stats,{14,0,{0,0}}},
	 {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_7,[]}}},
	 {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_7,[]},ok}}],
	{?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_6,[]}}},
	{?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_6,[]},ok}}],
       {?eh,tc_start,{groups_11_SUITE,testcase_5b}},
       {?eh,tc_done,{groups_11_SUITE,testcase_5b,ok}},
       {?eh,test_stats,{15,0,{0,0}}},
       {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_5,[]}}},
       {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_5,[]},ok}}],
      {?eh,tc_start,{groups_11_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_11_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_11_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_11_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{groups_12_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,init_per_suite,ok}},

     {shuffle,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]}}},
	       {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1a,[{shuffle,'_'}]},ok}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1b}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},
	       {?eh,test_stats,{16,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1c}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1c,ok}},
	       {?eh,test_stats,{17,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,testcase_1a}},
	       {?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
	       {?eh,test_stats,{18,0,{0,0}}},
	       {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]}}},
	       {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1a,[shuffle]},ok}}]},
     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_1b,[parallel]},ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1a}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1a,ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_1b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_1b,ok}},
		{?eh,test_stats,{20,0,{0,0}}},
		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_1b,[parallel]},ok}}]},
     {?eh,tc_start,{groups_12_SUITE,testcase_1}},
     {?eh,tc_done,{groups_12_SUITE,testcase_1,ok}},
     {?eh,test_stats,{21,0,{0,0}}},
     {?eh,tc_start,{groups_12_SUITE,testcase_2}},
     {?eh,tc_done,{groups_12_SUITE,testcase_2,ok}},
     {?eh,test_stats,{22,0,{0,0}}},

     {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
		{?eh,tc_start,{groups_12_SUITE,testcase_2a}},
		{?eh,tc_done,{groups_12_SUITE,testcase_2a,ok}},
		[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[{repeat,2}]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[{repeat,2}]},ok}}],
		[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_3,[]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_3,[]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3a,ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_3b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_3b,ok}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_3,[]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_3,[]},ok}}],
		{?eh,tc_start,{groups_12_SUITE,testcase_2b}},
		{?eh,tc_done,{groups_12_SUITE,testcase_2b,ok}},
		{?eh,test_stats,{28,0,{0,0}}},
		{?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]}}},
		{?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},

     {?eh,tc_start,{groups_12_SUITE,testcase_3}},
     {?eh,tc_done,{groups_12_SUITE,testcase_3,ok}},
     {?eh,test_stats,{29,0,{0,0}}},

     [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_4,[]},ok}},
      {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]}}},
		 {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
		 {?eh,tc_start,{groups_12_SUITE,testcase_5a}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_5a,ok}},
		 {parallel,[{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]}}},
			    {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
			    [{?eh,tc_start,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]}}},
			     {?eh,tc_done,{groups_12_SUITE,{init_per_group,test_group_7,[sequence]},ok}},
			     {?eh,tc_start,{groups_12_SUITE,testcase_7a}},
			     {?eh,tc_done,{groups_12_SUITE,testcase_7a,ok}},
			     {?eh,tc_start,{groups_12_SUITE,testcase_7b}},
			     {?eh,tc_done,{groups_12_SUITE,testcase_7b,ok}},
			     {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]}}},
			     {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_7,[sequence]},ok}}],
			    {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]}}},
			    {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_6,[parallel]},ok}}]},
		 {?eh,tc_start,{groups_12_SUITE,testcase_5b}},
		 {?eh,tc_done,{groups_12_SUITE,testcase_5b,ok}},
		 {?eh,test_stats,{33,0,{0,0}}},
		 {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]}}},
		 {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,{groups_12_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,{groups_12_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_12_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_12_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{groups_21_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{groups_21_SUITE,testcase_1}},
     {?eh,tc_done,{groups_21_SUITE,testcase_1,ok}},
     {?eh,test_stats,{34,0,{0,0}}},

     [{?eh,tc_start,
       {groups_21_SUITE,{init_per_group,test_group_1a,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{init_per_group,test_group_1a,[]},ok}},
      {?eh,tc_start,{groups_21_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_21_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{35,0,{0,0}}},
      {?eh,tc_start,{groups_21_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_21_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{36,0,{0,0}}},
      {?eh,tc_start,
       {groups_21_SUITE,{end_per_group,test_group_1a,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{end_per_group,test_group_1a,[]},ok}}],

     [{?eh,tc_start,
       {groups_21_SUITE,{init_per_group,test_group_1b,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{init_per_group,test_group_1b,[]},ok}},
      {?eh,tc_start,{groups_21_SUITE,testcase_1a}},
      {?eh,tc_done,{groups_21_SUITE,testcase_1a,ok}},
      {?eh,test_stats,{37,0,{0,0}}},
      {?eh,tc_start,{groups_21_SUITE,testcase_1b}},
      {?eh,tc_done,{groups_21_SUITE,testcase_1b,ok}},
      {?eh,test_stats,{38,0,{0,0}}},
      {?eh,tc_start,
       {groups_21_SUITE,{end_per_group,test_group_1b,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{end_per_group,test_group_1b,[]},ok}}],
     {?eh,tc_start,{groups_21_SUITE,testcase_2}},
     {?eh,tc_done,{groups_21_SUITE,testcase_2,ok}},
     {?eh,test_stats,{39,0,{0,0}}},

     [{?eh,tc_start,
       {groups_21_SUITE,{init_per_group,test_group_2,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{init_per_group,test_group_2,[]},ok}},
      {?eh,tc_start,{groups_21_SUITE,testcase_2a}},
      {?eh,tc_done,{groups_21_SUITE,testcase_2a,ok}},
      {?eh,test_stats,{40,0,{0,0}}},
      [{?eh,tc_start,
	{groups_21_SUITE,{init_per_group,test_group_3,[]}}},
       {?eh,tc_done,
	{groups_21_SUITE,{init_per_group,test_group_3,[]},ok}},
       {?eh,tc_start,{groups_21_SUITE,testcase_3a}},
       {?eh,tc_done,{groups_21_SUITE,testcase_3a,ok}},
       {?eh,test_stats,{41,0,{0,0}}},
       {?eh,tc_start,{groups_21_SUITE,testcase_3b}},
       {?eh,tc_done,{groups_21_SUITE,testcase_3b,ok}},
       {?eh,test_stats,{42,0,{0,0}}},
       {?eh,tc_start,
	{groups_21_SUITE,{end_per_group,test_group_3,[]}}},
       {?eh,tc_done,
	{groups_21_SUITE,{end_per_group,test_group_3,[]},ok}}],
      {?eh,tc_start,{groups_21_SUITE,testcase_2b}},
      {?eh,tc_done,{groups_21_SUITE,testcase_2b,ok}},
      {?eh,test_stats,{43,0,{0,0}}},
      {?eh,tc_start,
       {groups_21_SUITE,{end_per_group,test_group_2,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{end_per_group,test_group_2,[]},ok}}],

     {?eh,tc_start,{groups_21_SUITE,testcase_3}},
     {?eh,tc_done,{groups_21_SUITE,testcase_3,ok}},
     {?eh,test_stats,{44,0,{0,0}}},

     [{?eh,tc_start,
       {groups_21_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{init_per_group,test_group_4,[]},ok}},
      [{?eh,tc_start,
	{groups_21_SUITE,{init_per_group,test_group_5,[]}}},
       {?eh,tc_done,
	{groups_21_SUITE,{init_per_group,test_group_5,[]},ok}},
       {?eh,tc_start,{groups_21_SUITE,testcase_5a}},
       {?eh,tc_done,{groups_21_SUITE,testcase_5a,ok}},
       {?eh,test_stats,{45,0,{0,0}}},
       [{?eh,tc_start,
	 {groups_21_SUITE,{init_per_group,test_group_6,[]}}},
	{?eh,tc_done,
	 {groups_21_SUITE,{init_per_group,test_group_6,[]},ok}},
	[{?eh,tc_start,
	  {groups_21_SUITE,{init_per_group,test_group_7,[]}}},
	 {?eh,tc_done,
	  {groups_21_SUITE,{init_per_group,test_group_7,[]},ok}},
         {?eh,tc_start,{groups_21_SUITE,testcase_7a}},
         {?eh,tc_done,{groups_21_SUITE,testcase_7a,ok}},
         {?eh,test_stats,{46,0,{0,0}}},
         {?eh,tc_start,{groups_21_SUITE,testcase_7b}},
         {?eh,tc_done,{groups_21_SUITE,testcase_7b,ok}},
         {?eh,test_stats,{47,0,{0,0}}},
	 {?eh,tc_start,
	  {groups_21_SUITE,{end_per_group,test_group_7,[]}}},
	 {?eh,tc_done,
	  {groups_21_SUITE,{end_per_group,test_group_7,[]},ok}}],
	{?eh,tc_start,
	 {groups_21_SUITE,{end_per_group,test_group_6,[]}}},
	{?eh,tc_done,
	 {groups_21_SUITE,{end_per_group,test_group_6,[]},ok}}],
       {?eh,tc_start,{groups_21_SUITE,testcase_5b}},
       {?eh,tc_done,{groups_21_SUITE,testcase_5b,ok}},
       {?eh,test_stats,{48,0,{0,0}}},
       {?eh,tc_start,
	{groups_21_SUITE,{end_per_group,test_group_5,[]}}},
       {?eh,tc_done,
	{groups_21_SUITE,{end_per_group,test_group_5,[]},ok}}],
      {?eh,tc_start,
       {groups_21_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,
       {groups_21_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_21_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_21_SUITE,end_per_suite,ok}},

     {?eh,tc_start,{groups_22_SUITE,init_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,init_per_suite,ok}},

     {shuffle,
      [{?eh,tc_start,
	{groups_22_SUITE,
	 {init_per_group,test_group_1a,[{shuffle,'_'}]}}},
       {?eh,tc_done,
	{groups_22_SUITE,
	 {init_per_group,test_group_1a,[{shuffle,'_'}]},
	 ok}},
       {?eh,tc_start,{groups_22_SUITE,testcase_1a}},
       {?eh,tc_done,{groups_22_SUITE,testcase_1a,ok}},
       {?eh,tc_start,{groups_22_SUITE,testcase_1b}},
       {?eh,tc_done,{groups_22_SUITE,testcase_1b,ok}},
       {?eh,tc_start,{groups_22_SUITE,testcase_1c}},
       {?eh,tc_done,{groups_22_SUITE,testcase_1c,ok}},
       {?eh,test_stats,{51,0,{0,0}}},
       {?eh,tc_start,
	{groups_22_SUITE,{end_per_group,test_group_1a,[shuffle]}}},
       {?eh,tc_done,
	{groups_22_SUITE,{end_per_group,test_group_1a,[shuffle]},ok}}]},

     {parallel,
      [{?eh,tc_start,
	{groups_22_SUITE,{init_per_group,test_group_1b,[parallel]}}},
       {?eh,tc_done,
	{groups_22_SUITE,{init_per_group,test_group_1b,[parallel]},ok}},
       {?eh,tc_start,{groups_22_SUITE,testcase_1a}},
       {?eh,tc_done,{groups_22_SUITE,testcase_1a,ok}},
       {?eh,tc_start,{groups_22_SUITE,testcase_1b}},
       {?eh,tc_done,{groups_22_SUITE,testcase_1b,ok}},
       {?eh,test_stats,{53,0,{0,0}}},
       {?eh,tc_start,
	{groups_22_SUITE,{end_per_group,test_group_1b,[parallel]}}},
       {?eh,tc_done,
	{groups_22_SUITE,{end_per_group,test_group_1b,[parallel]},ok}}]},

     {?eh,tc_start,{groups_22_SUITE,testcase_1}},
     {?eh,tc_done,{groups_22_SUITE,testcase_1,ok}},
     {?eh,test_stats,{54,0,{0,0}}},
     {?eh,tc_start,{groups_22_SUITE,testcase_2}},
     {?eh,tc_done,{groups_22_SUITE,testcase_2,ok}},
     {?eh,test_stats,{55,0,{0,0}}},

     {parallel,
      [{?eh,tc_start,
	{groups_22_SUITE,{init_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_22_SUITE,{init_per_group,test_group_2,[parallel]},ok}},
       {?eh,tc_start,{groups_22_SUITE,testcase_2a}},
       {?eh,tc_done,{groups_22_SUITE,testcase_2a,ok}},
       [{?eh,tc_start,
         {groups_22_SUITE,{init_per_group,test_group_3,[{repeat,2}]}}},
	{?eh,tc_done,
         {groups_22_SUITE,{init_per_group,test_group_3,[{repeat,2}]},ok}},
	{?eh,tc_start,{groups_22_SUITE,testcase_3a}},
	{?eh,tc_done,{groups_22_SUITE,testcase_3a,ok}},
	{?eh,tc_start,{groups_22_SUITE,testcase_3b}},
	{?eh,tc_done,{groups_22_SUITE,testcase_3b,ok}},
	{?eh,tc_start,
         {groups_22_SUITE,{end_per_group,test_group_3,[{repeat,2}]}}},
	{?eh,tc_done,
         {groups_22_SUITE,{end_per_group,test_group_3,[{repeat,2}]},ok}}],
       [{?eh,tc_start,
	 {groups_22_SUITE,{init_per_group,test_group_3,[]}}},
	{?eh,tc_done,
         {groups_22_SUITE,{init_per_group,test_group_3,[]},ok}},
	{?eh,tc_start,{groups_22_SUITE,testcase_3a}},
	{?eh,tc_done,{groups_22_SUITE,testcase_3a,ok}},
	{?eh,tc_start,{groups_22_SUITE,testcase_3b}},
	{?eh,tc_done,{groups_22_SUITE,testcase_3b,ok}},
	{?eh,tc_start,
	 {groups_22_SUITE,{end_per_group,test_group_3,[]}}},
	{?eh,tc_done,
	 {groups_22_SUITE,{end_per_group,test_group_3,[]},ok}}],
       {?eh,tc_start,{groups_22_SUITE,testcase_2b}},
       {?eh,tc_done,{groups_22_SUITE,testcase_2b,ok}},
       {?eh,test_stats,{61,0,{0,0}}},
       {?eh,tc_start,
	{groups_22_SUITE,{end_per_group,test_group_2,[parallel]}}},
       {?eh,tc_done,
	{groups_22_SUITE,{end_per_group,test_group_2,[parallel]},ok}}]},
     {?eh,tc_start,{groups_22_SUITE,testcase_3}},
     {?eh,tc_done,{groups_22_SUITE,testcase_3,ok}},
     {?eh,test_stats,{62,0,{0,0}}},
     [{?eh,tc_start,
       {groups_22_SUITE,{init_per_group,test_group_4,[]}}},
      {?eh,tc_done,
       {groups_22_SUITE,{init_per_group,test_group_4,[]},ok}},

      {parallel,
       [{?eh,tc_start,
         {groups_22_SUITE,{init_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
         {groups_22_SUITE,{init_per_group,test_group_5,[parallel]},ok}},
	{?eh,tc_start,{groups_22_SUITE,testcase_5a}},
	{?eh,tc_done,{groups_22_SUITE,testcase_5a,ok}},
	{parallel,
	 [{?eh,tc_start,
	   {groups_22_SUITE,{init_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_22_SUITE,{init_per_group,test_group_6,[parallel]},ok}},
	  [{?eh,tc_start,
	    {groups_22_SUITE,{init_per_group,test_group_7,[sequence]}}},
	   {?eh,tc_done,
	    {groups_22_SUITE,{init_per_group,test_group_7,[sequence]},ok}},
           {?eh,tc_start,{groups_22_SUITE,testcase_7a}},
           {?eh,tc_done,{groups_22_SUITE,testcase_7a,ok}},
           {?eh,tc_start,{groups_22_SUITE,testcase_7b}},
           {?eh,tc_done,{groups_22_SUITE,testcase_7b,ok}},
	   {?eh,tc_start,
	    {groups_22_SUITE,{end_per_group,test_group_7,[sequence]}}},
	   {?eh,tc_done,
	    {groups_22_SUITE,{end_per_group,test_group_7,[sequence]},ok}}],
	  {?eh,tc_start,
	   {groups_22_SUITE,{end_per_group,test_group_6,[parallel]}}},
	  {?eh,tc_done,
	   {groups_22_SUITE,{end_per_group,test_group_6,[parallel]},ok}}]},
	{?eh,tc_start,{groups_22_SUITE,testcase_5b}},
	{?eh,tc_done,{groups_22_SUITE,testcase_5b,ok}},
	{?eh,test_stats,{66,0,{0,0}}},
	{?eh,tc_start,
         {groups_22_SUITE,{end_per_group,test_group_5,[parallel]}}},
	{?eh,tc_done,
         {groups_22_SUITE,{end_per_group,test_group_5,[parallel]},ok}}]},
      {?eh,tc_start,
       {groups_22_SUITE,{end_per_group,test_group_4,[]}}},
      {?eh,tc_done,
       {groups_22_SUITE,{end_per_group,test_group_4,[]},ok}}],

     {?eh,tc_start,{groups_22_SUITE,end_per_suite}},
     {?eh,tc_done,{groups_22_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}].
