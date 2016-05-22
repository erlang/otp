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
%%% File: ct_groups_spec_SUITE
%%%
%%% Description: 
%%% Test that overriding default group properties with group terms
%%% in all/0 and in test specifications works as expected.
%%%
%%%-------------------------------------------------------------------
-module(ct_groups_spec_SUITE).

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
     simple_group_opt,
     simple_group_case_opt,
     override_with_all,
     override_with_spec
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
simple_group_opt(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "groups_spec_1_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{group,[g1,g5]},
			  {label,simple_group_opt}], Config),
    ok = execute(simple_group_opt, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
simple_group_case_opt(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "groups_spec_1_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{group,g5},{testcase,[t52,t54]},
			  {label,simple_group_case_opt}], Config),
    ok = execute(simple_group_case_opt, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
override_with_all(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "groups_spec_1_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,override_with_all}], Config),
    ok = execute(override_with_all, Opts, ERPid, Config).

%%%-----------------------------------------------------------------
%%% 
override_with_spec(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "override.spec"),
    {Opts,ERPid} = setup([{spec,Spec},{label,override_with_spec}], Config),
    ok = execute(override_with_spec, Opts, ERPid, Config).

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


test_events(simple_group_opt) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,start_info,{1,1,7}},
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},

     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t11,ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
      {?eh,tc_done,{groups_spec_1_SUITE,t13,ok}},
      {?eh,test_stats,{2,1,{0,0}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[]},ok}}],

     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]},ok}},

      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g4,[]},ok}},

       {parallel,
	[{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]},ok}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t51}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t51,ok}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t52}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t52,{failed,{timetrap_timeout,2000}}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t53}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t53,{failed,{error,crashes}}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t54}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t54,ok}},
	 {?eh,test_stats,{4,3,{0,0}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]},ok}}]},

       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g4,[]},ok}}],

      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]},ok}}],

     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(simple_group_case_opt) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,start_info,{1,1,2}},
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},

     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]},ok}},

      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g4,[]},ok}},

       {parallel,
	[{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]},ok}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t52}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t52,{failed,{timetrap_timeout,2000}}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t54}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t54,ok}},
	 {?eh,test_stats,{1,1,{0,0}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]},ok}}]},

       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g4,[]},ok}}],

      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]},ok}}],

     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(override_with_all) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,start_info,{1,1,45}},
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},

     %% TEST: {group,g1,default}
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t11,ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
      {?eh,tc_done,{groups_spec_1_SUITE,t13,ok}},
      {?eh,test_stats,{2,1,{0,0}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[]},ok}}],

     %% TEST: {group,g1,[sequence]}
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[sequence]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t11,ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t13,g1},
			 {failed,{groups_spec_1_SUITE,t12}}}},
      {?eh,test_stats,{3,2,{0,1}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[sequence]},ok}}],

     %% TEST: {group,g1,[parallel],[]}
     {parallel,
      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[parallel]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[parallel]},ok}},
       {?eh,tc_start,{groups_spec_1_SUITE,t11}},
       {?eh,tc_done,{groups_spec_1_SUITE,t11,ok}},
       {?eh,tc_start,{groups_spec_1_SUITE,t12}},
       {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
       {?eh,tc_start,{groups_spec_1_SUITE,t13}},
       {?eh,tc_done,{groups_spec_1_SUITE,t13,ok}},
       {?eh,test_stats,{5,3,{0,1}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[parallel]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[parallel]},ok}}]},

     %% TEST: {group,g2,[],[]}
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t21,ok}},
      {?eh,test_stats,{6,3,{0,1}}},

      {parallel,
       [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g3,[parallel]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g3,[parallel]},ok}},
	{?eh,tc_start,{groups_spec_1_SUITE,t31}},
	{?eh,tc_done,{groups_spec_1_SUITE,t31,ok}},
	{?eh,tc_start,{groups_spec_1_SUITE,t32}},
	{?eh,tc_done,{groups_spec_1_SUITE,t32,{failed,{error,crashes}}}},
	{?eh,tc_start,{groups_spec_1_SUITE,t33}},
	{?eh,tc_done,{groups_spec_1_SUITE,t33,ok}},
	{?eh,test_stats,{8,4,{0,1}}},
	{?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g3,[parallel]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g3,[parallel]},ok}}]},

      {?eh,tc_done,{groups_spec_1_SUITE,t22,{failed,{error,crashes}}}},
      {?eh,test_stats,{8,5,{0,1}}},

      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g4,[]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t41,ok}},
       {?eh,test_stats,{9,5,{0,1}}},

       {parallel,
	[{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]},ok}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t51}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t51,ok}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t52}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t52,{failed,{timetrap_timeout,2000}}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t53}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t53,{failed,{error,crashes}}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t54}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t54,ok}},
	 {?eh,test_stats,{11,7,{0,1}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]},ok}}]},

       {?eh,tc_done,{groups_spec_1_SUITE,t42,{failed,{error,crashes}}}},
       {?eh,test_stats,{11,8,{0,1}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g4,[]},ok}}],

      {?eh,tc_done,{groups_spec_1_SUITE,t23,ok}},
      {?eh,test_stats,{12,8,{0,1}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[]},ok}}],

     %% TEST: {group,g2,default,[{g3,[sequence]}]}
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t21,ok}},
      {?eh,test_stats,{13,8,{0,1}}},

      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t31,ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t32,{failed,{error,crashes}}}},
       {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t33,g3},
			  {failed,{groups_spec_1_SUITE,t32}}}},
       {?eh,test_stats,{14,9,{0,2}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]},ok}}],

      {?eh,tc_done,{groups_spec_1_SUITE,t22,{failed,{error,crashes}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t41,g4},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t51,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t52,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t53,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t54,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t42,g4},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t23,g2},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,test_stats,{14,10,{0,9}}},

      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]},ok}}],

     %% TEST: {group,g2,[],[{g4,[sequence],[{g5,[sequence]}]},{g3,[sequence]}]}
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t21,ok}},
      {?eh,test_stats,{15,10,{0,9}}},

      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t31,ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t32,{failed,{error,crashes}}}},
       {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t33,g3},
			  {failed,{groups_spec_1_SUITE,t32}}}},
       {?eh,test_stats,{16,11,{0,10}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]},ok}}],

      {?eh,tc_done,{groups_spec_1_SUITE,t22,{failed,{error,crashes}}}},
      {?eh,test_stats,{16,12,{0,10}}},

      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g4,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g4,[sequence]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t41,ok}},
       {?eh,test_stats,{17,12,{0,10}}},

       [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g5,[sequence]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g5,[sequence]},ok}},
	{?eh,tc_done,{groups_spec_1_SUITE,t51,ok}},
	{?eh,tc_done,{groups_spec_1_SUITE,t52,{failed,{timetrap_timeout,2000}}}},
	{?eh,tc_auto_skip,{groups_spec_1_SUITE,{t53,g5},
			   {failed,{groups_spec_1_SUITE,t52}}}},
	{?eh,tc_auto_skip,{groups_spec_1_SUITE,{t54,g5},
			   {failed,{groups_spec_1_SUITE,t52}}}},
	{?eh,test_stats,{18,13,{0,12}}},
	{?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g5,[sequence]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g5,[sequence]},ok}}],

       {?eh,tc_done,{groups_spec_1_SUITE,t42,{failed,{error,crashes}}}},
       {?eh,test_stats,{18,14,{0,12}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g4,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g4,[sequence]},ok}}],

      {?eh,tc_done,{groups_spec_1_SUITE,t23,ok}},
      {?eh,test_stats,{19,14,{0,12}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[]},ok}}],

     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(override_with_spec) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,start_info,{7,4,49}},

     %% TEST: {groups, dir, groups_spec_1_SUITE, {g1,default}}.
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t11,ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
      {?eh,tc_done,{groups_spec_1_SUITE,t13,ok}},
      {?eh,test_stats,{2,1,{0,0}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[]},ok}}],
     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},

     %% TEST: {groups, dir, groups_spec_1_SUITE, [{g1,[sequence]},
     %%                                           {g1,[parallel],[]}]}.
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[sequence]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t11,ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t13,g1},
			 {failed,{groups_spec_1_SUITE,t12}}}},
      {?eh,test_stats,{3,2,{0,1}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[sequence]},ok}}],
     {parallel,
      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[parallel]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[parallel]},ok}},
       {?eh,tc_start,{groups_spec_1_SUITE,t11}},
       {?eh,tc_done,{groups_spec_1_SUITE,t11,ok}},
       {?eh,tc_start,{groups_spec_1_SUITE,t12}},
       {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
       {?eh,tc_start,{groups_spec_1_SUITE,t13}},
       {?eh,tc_done,{groups_spec_1_SUITE,t13,ok}},
       {?eh,test_stats,{5,3,{0,1}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[parallel]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[parallel]},ok}}]},
     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},

     %% TEST: {groups, dir, groups_spec_1_SUITE, {g2,[],[]}}.
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_start,{groups_spec_1_SUITE,t21}},
      {?eh,test_stats,{6,3,{0,1}}},
      {parallel,
       [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g3,[parallel]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g3,[parallel]},ok}},
	{?eh,tc_start,{groups_spec_1_SUITE,t31}},
	{?eh,tc_done,{groups_spec_1_SUITE,t31,ok}},
	{?eh,tc_start,{groups_spec_1_SUITE,t32}},
	{?eh,tc_done,{groups_spec_1_SUITE,t32,{failed,{error,crashes}}}},
	{?eh,tc_start,{groups_spec_1_SUITE,t33}},
	{?eh,tc_done,{groups_spec_1_SUITE,t33,ok}},
	{?eh,test_stats,{8,4,{0,1}}},
	{?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g3,[parallel]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g3,[parallel]},ok}}]},
      {?eh,tc_done,{groups_spec_1_SUITE,t22,{failed,{error,crashes}}}},
      {?eh,test_stats,{8,5,{0,1}}},
      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g4,[]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t41,ok}},
       {?eh,test_stats,{9,5,{0,1}}},
       {parallel,
	[{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g5,[parallel]},ok}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t51}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t51,ok}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t52}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t52,{failed,{timetrap_timeout,2000}}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t53}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t53,{failed,{error,crashes}}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,t54}},
	 {?eh,tc_done,{groups_spec_1_SUITE,t54,ok}},
	 {?eh,test_stats,{11,7,{0,1}}},
	 {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]}}},
	 {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g5,[parallel]},ok}}]},
       {?eh,tc_done,{groups_spec_1_SUITE,t42,{failed,{error,crashes}}}},
       {?eh,test_stats,{11,8,{0,1}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g4,[]},ok}}],
      {?eh,tc_done,{groups_spec_1_SUITE,t23,ok}},
      {?eh,test_stats,{12,8,{0,1}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[]},ok}}],
     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},

     %% TEST: {groups, dir, groups_spec_1_SUITE, {g2,default,[{g3,[sequence]}]}}
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t21,ok}},
      {?eh,test_stats,{13,8,{0,1}}},
      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t31,ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t32,{failed,{error,crashes}}}},
       {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t33,g3},
			  {failed,{groups_spec_1_SUITE,t32}}}},
       {?eh,test_stats,{14,9,{0,2}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]},ok}}],
      {?eh,tc_done,{groups_spec_1_SUITE,t22,{failed,{error,crashes}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t41,g4},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t51,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t52,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t53,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t54,g5},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t42,g4},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t23,g2},
			 {failed,{groups_spec_1_SUITE,t22}}}},
      {?eh,test_stats,{14,10,{0,9}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]},ok}}],
     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},

     %% TEST: {groups, dir, groups_spec_1_SUITE,
     %%        {g2,[],[{g4,[sequence],[{g5,[sequence]}]},{g3,[sequence]}]}}.
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t21,ok}},
      {?eh,test_stats,{15,10,{0,9}}},
      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g3,[sequence]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t31,ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t32,{failed,{error,crashes}}}},
       {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t33,g3},
			  {failed,{groups_spec_1_SUITE,t32}}}},
       {?eh,test_stats,{16,11,{0,10}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g3,[sequence]},ok}}],
      {?eh,tc_done,{groups_spec_1_SUITE,t22,{failed,{error,crashes}}}},
      {?eh,test_stats,{16,12,{0,10}}},
      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g4,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g4,[sequence]},ok}},
       {?eh,tc_done,{groups_spec_1_SUITE,t41,ok}},
       {?eh,test_stats,{17,12,{0,10}}},
       [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g5,[sequence]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g5,[sequence]},ok}},
	{?eh,tc_done,{groups_spec_1_SUITE,t51,ok}},
	{?eh,tc_done,{groups_spec_1_SUITE,t52,{failed,{timetrap_timeout,2000}}}},
	{?eh,tc_auto_skip,{groups_spec_1_SUITE,{t53,g5},
			   {failed,{groups_spec_1_SUITE,t52}}}},
	{?eh,tc_auto_skip,{groups_spec_1_SUITE,{t54,g5},
			   {failed,{groups_spec_1_SUITE,t52}}}},
	{?eh,test_stats,{18,13,{0,12}}},
	{?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g5,[sequence]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g5,[sequence]},ok}}],
       {?eh,tc_done,{groups_spec_1_SUITE,t42,{failed,{error,crashes}}}},
       {?eh,test_stats,{18,14,{0,12}}},
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g4,[sequence]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g4,[sequence]},ok}}],
      {?eh,tc_done,{groups_spec_1_SUITE,t23,ok}},
      {?eh,test_stats,{19,14,{0,12}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[]},ok}}],
     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},

     %% TEST: {groups, dir, groups_spec_1_SUITE, {g1,[sequence]}, {cases,[t12,t13]}}
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g1,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g1,[sequence]},ok}},
      {?eh,tc_done,{groups_spec_1_SUITE,t12,{failed,{error,crashes}}}},
      {?eh,tc_auto_skip,{groups_spec_1_SUITE,{t13,g1},
			 {failed,{groups_spec_1_SUITE,t12}}}},
      {?eh,test_stats,{19,15,{0,13}}},
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g1,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g1,[sequence]},ok}}],
     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},

     %% TEST: {groups, dir, groups_spec_1_SUITE, {g5,[]}, {cases,[t53,t54]}}
     {?eh,tc_done,{groups_spec_1_SUITE,init_per_suite,ok}},
     [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g2,[sequence]},ok}},
      [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g4,[]},ok}},
       [{?eh,tc_start,{groups_spec_1_SUITE,{init_per_group,g5,[]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{init_per_group,g5,[]},ok}},
	{?eh,tc_done,{groups_spec_1_SUITE,t53,{failed,{error,crashes}}}},
	{?eh,tc_done,{groups_spec_1_SUITE,t54,ok}},
	{?eh,test_stats,{20,16,{0,13}}},
	{?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g5,[]}}},
	{?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g5,[]},ok}}],
       {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g4,[]}}},
       {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g4,[]},ok}}],
      {?eh,tc_start,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]}}},
      {?eh,tc_done,{groups_spec_1_SUITE,{end_per_group,g2,[sequence]},ok}}],
     {?eh,tc_done,{groups_spec_1_SUITE,end_per_suite,ok}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].

