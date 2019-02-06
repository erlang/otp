%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
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
%%% File: ct_repeat_test_SUITE
%%%
%%% Description:
%%% Test different options for repeating test runs:
%%%   -repeat N
%%%   -duration T [-force_stop [skip_rest]]
%%%   -until T [-force_stop [skip_rest]]
%%%
%%%-------------------------------------------------------------------
-module(ct_repeat_testrun_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).
-define(skip_reason, "Repeated test stopped by force_stop option").
-define(skipped, {auto_skipped, ?skip_reason}).


%% Timers used in this test.
%% Each test suite consists of
%%
%% [tc1,tc2,{group,g,[tc1,tc2]},tc2]
%%
%% In r1_SUITE tc1 has a sleep of 10 sec - all other test cases just
%% return ok.
%%
%% => One complete test run of two suites r1_SUITE + r2_SUITE is at
%% least 20 seconds (10 sec for each r1_SUITE:tc1)
%%
-define(t1,30). % time shall expire during second run of r1_SUITE
-define(t2,9).  % time shall expire during first run of tc1
-define(t3,19).  % time shall expire during second run of tc1


%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    TTInfo = {_T,{_Scaled,ScaleVal}} = ct:get_timetrap_info(),
    ct:pal("Timetrap info = ~w", [TTInfo]),
    if ScaleVal > 1 ->
	    {skip,"Skip on systems running e.g. cover or debug!"};
       ScaleVal =< 1 ->
	    Config = ct_test_support:init_per_suite(Config0),
	    DataDir = ?config(data_dir, Config),
	    Suite1 = filename:join([DataDir,"a_test","r1_SUITE"]),
	    Suite2 = filename:join([DataDir,"b_test","r2_SUITE"]),
	    Opts0 = ct_test_support:get_opts(Config),
	    Opts1 = Opts0 ++ [{suite,Suite1},{testcase,tc2},{label,timing1}],
	    Opts2 = Opts0 ++ [{suite,Suite2},{testcase,tc2},{label,timing2}],

	    %% Make sure both suites are compiled
	    {1,0,{0,0}} = ct_test_support:run(ct,run_test,[Opts1],Config),
	    {1,0,{0,0}} = ct_test_support:run(ct,run_test,[Opts2],Config),
	    
	    %% Check if file i/o is too slow for correct measurements
	    Opts3 = Opts0 ++ [{suite,Suite1},{testcase,tc1},{label,timing3}],
	    {T,_} = 
		timer:tc(
		  fun() ->
			  {1,0,{0,0}} = ct_test_support:run(ct,run_test,
							    [Opts3],Config),
			  {1,0,{0,0}} = ct_test_support:run(ct,run_test,
							    [Opts3],Config)
		  end),
	    %% The time to compare with here must match the timeout value
	    %% in the test suite. Accept 30% logging overhead (26 sec total).
	    if T > 26000000 ->
		    ct:pal("Timing test took ~w sec (< 27 sec expected). "
			   "Skipping the suite!",
			   [trunc(T/1000000)]),
		    ct_test_support:end_per_suite(Config),
		    {skip,"File I/O too slow for this suite"};
	       true ->
		    ct:pal("Timing test took ~w sec. Proceeding...",
			   [trunc(T/1000000)]),
		    [{offset,0}|Config]
	    end
    end.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     repeat_n,
     duration,
     duration_force_stop,
     duration_force_stop_skip_rest,
     duration_force_stop_skip_rest_group,
     until,
     until_force_stop,
     until_force_stop_skip_rest,
     until_force_stop_skip_rest_group
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
repeat_n(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,repeat_n},
			  {repeat,2}],
			 Config),
    ok = execute(repeat_n, Opts, ERPid, Config).

duration(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,duration},
			  {duration,duration_str(?t1,2,Config)}],
			 Config),
    ok = execute(duration, Opts, ERPid, Config).

duration_force_stop(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,duration_force_stop},
			  {duration,duration_str(?t1,2,Config)},
			  {force_stop,true}],
			 Config),
    ok = execute(duration_force_stop, Opts, ERPid, Config).

duration_force_stop_skip_rest(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,duration_force_stop_skip_rest},
			  {duration,duration_str(?t2,1,Config)},
			  {force_stop,skip_rest}],
			 Config),
    ok = execute(duration_force_stop_skip_rest, Opts, ERPid, Config).

duration_force_stop_skip_rest_group(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,duration_force_stop_skip_rest_group},
			  {duration,duration_str(?t3,1,Config)},
			  {force_stop,skip_rest}],
			 Config),
    ok = execute(duration_force_stop_skip_rest_group, Opts, ERPid, Config).

until(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,until}],
			 Config),
    ExecuteFun =
	fun() ->
		[_,_] = ct_test_support:run_ct_run_test(
			Opts++[{until,until_str(?t1,2,Config)}],Config),
		0 = ct_test_support:run_ct_script_start(
		      Opts++[{until,until_str(?t1,2,Config)}],Config)
	end,
    ok = execute(ExecuteFun, until, Opts, ERPid, Config).

until_force_stop(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,until_force_stop},
			  {force_stop,true}],
			 Config),
    ExecuteFun =
	fun() ->
		[_,_] = ct_test_support:run_ct_run_test(
			  Opts++[{until,until_str(?t1,2,Config)}],Config),
		0 = ct_test_support:run_ct_script_start(
		      Opts++[{until,until_str(?t1,2,Config)}],Config)
	end,
    ok = execute(ExecuteFun, until_force_stop, Opts, ERPid, Config).

until_force_stop_skip_rest(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,until_force_stop_skip_rest},
			  {force_stop,skip_rest}],
			 Config),
    ExecuteFun =
	fun() ->
		[_] = ct_test_support:run_ct_run_test(
			Opts++[{until,until_str(?t2,1,Config)}],Config),
		1 = ct_test_support:run_ct_script_start(
		      Opts++[{until,until_str(?t2,1,Config)}],Config)
	end,
    ok = execute(ExecuteFun, until_force_stop_skip_rest,
		 Opts, ERPid, Config).

until_force_stop_skip_rest_group(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Dirs = filelib:wildcard(filename:join(DataDir,"*")),
    {Opts,ERPid} = setup([{dir,Dirs},
			  {label,until_force_stop_skip_rest_group},
			  {force_stop,skip_rest}],
			 Config),
    ExecuteFun =
	fun() ->
		[_] = ct_test_support:run_ct_run_test(
			Opts++[{until,until_str(?t3,1,Config)}],Config),
		1 = ct_test_support:run_ct_script_start(
		      Opts++[{until,until_str(?t3,1,Config)}],Config)
	end,
    ok = execute(ExecuteFun,
		 until_force_stop_skip_rest_group,
		 Opts, ERPid, Config).


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

%% Execute test, first with ct:run_test, then with ct:script_start
execute(Name, Opts, ERPid, Config) ->
    ExecuteFun = fun() -> ok = ct_test_support:run(Opts, Config) end,
    execute(ExecuteFun, Name, Opts, ERPid, Config).

execute(ExecuteFun, Name, Opts, ERPid, Config) ->
    ExecuteFun(),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Name,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(Name),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%% N is the expected number of repeats
until_str(Secs0,N,Config) ->
    Offset = ?config(offset,Config),
    Secs = Secs0 + N*Offset,
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    {{Y,Mo,D},{H,M,S}} = calendar:gregorian_seconds_to_datetime(Now+Secs),
    lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w",
				[Y rem 100, Mo, D, H, M, S])).

%% N is the expected number of repeats
duration_str(Secs0,N,Config) ->
    Offset = ?config(offset,Config),
    Secs = Secs0 + N*Offset,
    "0000" ++ lists:flatten(io_lib:format("~2..0w",[Secs])).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
%% 2 tests (ct:run_test + script_start) is default
events_to_check(C) when C==repeat_n; C==duration; C==until ->
    dupl(4, start_logging() ++ all_succ() ++ stop_logging());
events_to_check(C) when C==duration_force_stop; C==until_force_stop ->
    dupl(2, start_logging() ++
	     all_succ() ++
	     stop_logging() ++
	     start_logging() ++
	     all_succ(r1_SUITE) ++
	     stop_logging());
events_to_check(C) when C==duration_force_stop_skip_rest;
			C==until_force_stop_skip_rest ->
    dupl(2, start_logging() ++ skip_first_tc1(r1_SUITE) ++ stop_logging());
events_to_check(C) when C==duration_force_stop_skip_rest_group;
			C==until_force_stop_skip_rest_group ->
    dupl(2, start_logging() ++ skip_tc1_in_group(r1_SUITE) ++ stop_logging()).

dupl(N,List) ->
    lists:flatten(lists:duplicate(N,List)).

start_logging() ->
    [{?eh,start_logging,{'DEF','RUNDIR'}}].
stop_logging() ->
    [{?eh,stop_logging,[]}].


all_succ() ->
    all_succ(r1_SUITE) ++ all_succ(r2_SUITE).

all_succ(Suite) ->
    [{?eh,tc_start,{Suite,init_per_suite}},
     {?eh,tc_done,{Suite,init_per_suite,ok}},
     {?eh,tc_start,{Suite,tc1}},
     {?eh,tc_done,{Suite,tc1,ok}},
     {?eh,test_stats,{'_',0,{0,0}}},
     {?eh,tc_start,{Suite,tc2}},
     {?eh,tc_done,{Suite,tc2,ok}},
     {?eh,test_stats,{'_',0,{0,0}}},
     [{?eh,tc_start,{Suite,{init_per_group,g,[]}}},
      {?eh,tc_done,{Suite,{init_per_group,g,[]},ok}},
      {?eh,tc_start,{Suite,tc1}},
      {?eh,tc_done,{Suite,tc1,ok}},
      {?eh,test_stats,{'_',0,{0,0}}},
      {?eh,tc_start,{Suite,tc2}},
      {?eh,tc_done,{Suite,tc2,ok}},
      {?eh,test_stats,{'_',0,{0,0}}},
      {?eh,tc_start,{Suite,{end_per_group,g,[]}}},
      {?eh,tc_done,{Suite,{end_per_group,g,[]},ok}}],
     {?eh,tc_start,{Suite,tc2}},
     {?eh,tc_done,{Suite,tc2,ok}},
     {?eh,test_stats,{'_',0,{0,0}}},
     {?eh,tc_start,{Suite,end_per_suite}},
     {?eh,tc_done,{Suite,end_per_suite,ok}}].

skip_first_tc1(Suite) ->
    [{?eh,tc_start,{Suite,init_per_suite}},
     {?eh,tc_done,{Suite,init_per_suite,ok}},
     {?eh,tc_start,{Suite,tc1}},
     {?eh,tc_done,{Suite,tc1,ok}},
     {?eh,test_stats,{'_',0,{0,0}}},
     {?eh,tc_start,{Suite,tc2}},
     {?eh,tc_done,{Suite,tc2,?skipped}},
     {?eh,test_stats,{'_',0,{0,1}}},
     {?eh,tc_start,{Suite,{init_per_group,g,[]}}},
     {?eh,tc_done,{Suite,{init_per_group,g,[]},?skipped}},
     {?eh,tc_auto_skip,{Suite,{tc1,g},?skip_reason}},
     {?eh,test_stats,{'_',0,{0,2}}},
     {?eh,tc_auto_skip,{Suite,{tc2,g},?skip_reason}},
     {?eh,test_stats,{'_',0,{0,3}}},
     {?eh,tc_auto_skip,{Suite,{end_per_group,g},?skip_reason}},
     {?eh,tc_start,{Suite,tc2}},
     {?eh,tc_done,{Suite,tc2,?skipped}},
     {?eh,test_stats,{'_',0,{0,4}}},
     {?eh,tc_start,{Suite,end_per_suite}},
     {?eh,tc_done,{Suite,end_per_suite,ok}}].

skip_tc1_in_group(Suite) ->
    [{?eh,tc_start,{Suite,init_per_suite}},
     {?eh,tc_done,{Suite,init_per_suite,ok}},
     {?eh,tc_start,{Suite,tc1}},
     {?eh,tc_done,{Suite,tc1,ok}},
     {?eh,test_stats,{'_',0,{0,0}}},
     {?eh,tc_start,{Suite,tc2}},
     {?eh,tc_done,{Suite,tc2,ok}},
     {?eh,test_stats,{'_',0,{0,0}}},
     [{?eh,tc_start,{Suite,{init_per_group,g,[]}}},
      {?eh,tc_done,{Suite,{init_per_group,g,[]},ok}},
      {?eh,tc_start,{Suite,tc1}},
      {?eh,tc_done,{Suite,tc1,ok}},
      {?eh,test_stats,{'_',0,{0,0}}},
      {?eh,tc_start,{Suite,tc2}},
      {?eh,tc_done,{Suite,tc2,?skipped}},
      {?eh,test_stats,{'_',0,{0,1}}},
      {?eh,tc_start,{Suite,{end_per_group,g,[]}}},
      {?eh,tc_done,{Suite,{end_per_group,g,[]},ok}}],
     {?eh,tc_start,{Suite,tc2}},
     {?eh,tc_done,{Suite,tc2,?skipped}},
     {?eh,test_stats,{'_',0,{0,2}}},
     {?eh,tc_start,{Suite,end_per_suite}},
     {?eh,tc_done,{Suite,end_per_suite,ok}}].
