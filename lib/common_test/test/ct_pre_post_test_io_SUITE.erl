%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
%%% File: ct_pre_post_test_io_SUITE
%%%
%%% Description:
%%%
%%% Test that ct:log/2 printouts and error/progress reports that happen
%%% before or after the test run are saved in the pre/post test IO log.
%%%-------------------------------------------------------------------
-module(ct_pre_post_test_io_SUITE).

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
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,120}}].

all() ->
    [
     pre_post_io
    ].

init_per_suite(Config) ->
    TTInfo = {_T,{_Scaled,ScaleVal}} = ct:get_timetrap_info(),
    ct:pal("Timetrap info = ~w", [TTInfo]),
    if ScaleVal > 1 ->
	    {skip,"Skip on systems running e.g. cover or debug!"};
       ScaleVal =< 1 ->
	    DataDir = ?config(data_dir, Config),
	    CTH = filename:join(DataDir, "cth_ctrl.erl"),
	    ct:pal("Compiling ~p: ~p",
		   [CTH,compile:file(CTH,[{outdir,DataDir},
					  debug_info])]),
	    ct_test_support:init_per_suite([{path_dirs,[DataDir]},
					    {start_sasl,true} | Config])
    end.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
pre_post_io(Config) ->
    TC = pre_post_io,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "dummy_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC},{ct_hooks,[cth_ctrl]}],
			 Config),
    
    %%!--------------------------------------------------------------------
    %%! Note that error reports will not start showing up in the pre-test
    %%! io log until handle_remote_events has been set to true (see below).
    %%! The reason is that the error logger has its group leader on the
    %%! test_server node (not the ct node) and cth_log_redirect ignores
    %%! events with remote destination until told otherwise.
    %%!--------------------------------------------------------------------

    spawn(fun() ->
		  ct:pal("CONTROLLER: Starting test run #1...", []),
		  %% --- test run 1 ---
		  try_loop(ct_test_support, ct_rpc, [{cth_log_redirect,
						      handle_remote_events,
						      [true]}, Config], 3000),
		  CTLoggerPid1 = ct_test_support:ct_rpc({erlang,whereis,
							[ct_logs]}, Config),
		  ct:pal("CONTROLLER: Logger = ~w~nHandle remote events = true",
			 [CTLoggerPid1]),
		  ct:sleep(5000),
		  ct:pal("CONTROLLER: Proceeding with test run #1...", []),
		  ok = ct_test_support:ct_rpc({cth_ctrl,proceed,[]}, Config),
		  ct:sleep(6000),
		  ct:pal("CONTROLLER: Proceeding with shutdown #1...", []),
		  ok = ct_test_support:ct_rpc({cth_ctrl,proceed,[]}, Config),
		  try_loop(fun() ->
				   false = ct_test_support:ct_rpc({erlang,
								    is_process_alive,
								    [CTLoggerPid1]},
								   Config)
			   end, 3000),
		  ct:pal("CONTROLLER: Shutdown #1 complete!", []),
		  ct:pal("CONTROLLER: Starting test run #2...", []),
		  %% --- test run 2 ---
		  try_loop(ct_test_support, ct_rpc, [{cth_log_redirect,
						      handle_remote_events,
						      [true]}, Config], 3000),
		  CTLoggerPid2 = ct_test_support:ct_rpc({erlang,whereis,
							[ct_logs]}, Config),
		  ct:pal("CONTROLLER: Logger = ~w~nHandle remote events = true",
			 [CTLoggerPid2]),
		  ct:sleep(5000),
		  ct:pal("CONTROLLER: Proceeding with test run #2...", []),
		  ok = ct_test_support:ct_rpc({cth_ctrl,proceed,[]}, Config),
		  ct:sleep(6000),
		  ct:pal("CONTROLLER: Proceeding with shutdown #2...", []),
		  ok = ct_test_support:ct_rpc({cth_ctrl,proceed,[]}, Config),
		  try_loop(fun() ->
				   false = ct_test_support:ct_rpc({erlang,
								   is_process_alive,
								   [CTLoggerPid2]},
								  Config)
			   end, 3000),
		  ct:pal("CONTROLLER: Shutdown #2 complete!", [])
	  end),
    ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),
    ct_test_support:log_events(TC,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),
    TestEvents = events_to_check(TC),
    ok = ct_test_support:verify_events(TestEvents, Events, Config),
    
    LogDirs = lists:flatmap(fun({_EH,#event{name=start_logging,data=Dir}}) ->
				    [Dir];
			       (_) ->
				    []
			    end, Events),
    PrePostIoFiles =
	[filename:join(LogDir, "misc_io.log.html") || LogDir <- LogDirs],
    lists:foreach(
      fun(PrePostIoFile) ->
	      ct:log("Reading Pre/Post Test IO Log file: ~ts", [PrePostIoFile]),
	      {ok,Bin} = file:read_file(PrePostIoFile),
	      Ts = string:lexemes(binary_to_list(Bin),[$\n]),
	      PrePostIOEntries =
		  lists:foldl(fun([$L,$o,$g,$g,$e,$r|_],
				  {pre,PreLogN,PreErrN,0,0}) ->
				      {pre,PreLogN+1,PreErrN,0,0};
				 ([$=,$E,$R,$R,$O,$R|_],
				  {pre,PreLogN,PreErrN,0,0}) ->
				      {pre,PreLogN,PreErrN+1,0,0};
				 ([_,_,_,_,$P,$O,$S,$T,$-,$T,$E,$S,$T|_],
				  {pre,PreLogN,PreErrN,0,0}) ->
				      {post,PreLogN,PreErrN,0,0};
				 ([$L,$o,$g,$g,$e,$r|_],
				  {post,PreLogN,PreErrN,PostLogN,PostErrN}) ->
				      {post,PreLogN,PreErrN,PostLogN+1,PostErrN};
				 ([$=,$E,$R,$R,$O,$R|_],
				  {post,PreLogN,PreErrN,PostLogN,PostErrN}) ->
				      {post,PreLogN,PreErrN,PostLogN,PostErrN+1};
				 (_, Counters) ->
				      Counters
			      end, {pre,0,0,0,0}, Ts),
	      [_|Counters] = tuple_to_list(PrePostIOEntries),
	      ct:pal("Entries in the Pre/Post Test IO Log: ~w", [Counters]),
	      case [C || C <- Counters, C < 2] of
		  [] ->
		      ok;
		  _ ->
		      exit("Not enough entries in the Pre/Post Test IO Log!")
	      end
      end, PrePostIoFiles), 

    UnexpIoFiles =
	[filelib:wildcard(
	   filename:join(LogDir,
			 "*dummy_SUITE.logs/run.*/"
			 "unexpected_io.log.html")) || LogDir <- LogDirs],
    lists:foreach(
      fun(UnexpIoFile) ->
	      ct:log("Reading Unexpected IO Log file: ~ts", [UnexpIoFile]),
	      {ok,Bin} = file:read_file(UnexpIoFile),
	      Ts = string:lexemes(binary_to_list(Bin),[$\n]),
	      UnexpIOEntries =
		  lists:foldl(fun([$L,$o,$g,$g,$e,$r|_], [LogN,ErrN]) ->
				      [LogN+1,ErrN];
				 ([$=,$E,$R,$R,$O,$R|_], [LogN,ErrN]) ->
				      [LogN,ErrN+1];
				 (_, Counters) -> Counters
			      end, [0,0], Ts),
	      ct:log("Entries in the Unexpected IO Log: ~w", [UnexpIOEntries]),
	      case [N || N <- UnexpIOEntries, N < 2] of
		  [] ->
		      ok;
		  _ ->
		      exit("Not enough entries in the Unexpected IO Log!")
	      end
      end, UnexpIoFiles),
    ok.

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

try_loop(_Fun, 0) ->
    ct:pal("WARNING! Fun never succeeded!", []),
    gave_up;
try_loop(Fun, N) ->
    try Fun() of
	{Error,_} when Error==error; Error==badrpc ->
	    timer:sleep(10),
	    try_loop(Fun, N-1);
	Result ->
	    Result
    catch
	_:_What ->
	    timer:sleep(10),
	    try_loop(Fun, N-1)
    end.

try_loop(M, F, _A, 0) ->
    ct:pal("WARNING! ~w:~w never succeeded!", [M,F]),
    gave_up;
try_loop(M, F, A, N) ->
    try apply(M, F, A) of
	{Error,_Reason} when Error==error; Error==badrpc ->
	    timer:sleep(10),
	    try_loop(M, F, A, N-1);
	Result ->
	    Result
    catch
	_:_ ->
	    timer:sleep(10),
	    try_loop(M, F, A, N-1)
    end.

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------

events_to_check(pre_post_io) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,7}},
     {?eh,tc_start,{dummy_SUITE,init_per_suite}},
     {?eh,tc_done,{dummy_SUITE,init_per_suite,ok}},
     {parallel,
      [{?eh,tc_start,{dummy_SUITE,{init_per_group,g1,[parallel]}}},
       {?eh,tc_done,
	{dummy_SUITE,{init_per_group,g1,[parallel]},ok}},
       {?eh,tc_start,{dummy_SUITE,tc1}},
       {?eh,tc_start,{dummy_SUITE,tc2}},
       {?eh,tc_start,{dummy_SUITE,tc3}},
       {?eh,tc_done,{dummy_SUITE,tc2,ok}},
       {?eh,tc_done,{dummy_SUITE,tc1,ok}},
       {?eh,tc_done,{dummy_SUITE,tc3,ok}},
       {?eh,test_stats,{1,0,{0,0}}},
       {?eh,test_stats,{2,0,{0,0}}},
       {?eh,test_stats,{3,0,{0,0}}},
       {?eh,tc_start,{dummy_SUITE,{end_per_group,g1,[parallel]}}},
       {?eh,tc_done,{dummy_SUITE,{end_per_group,g1,[parallel]},ok}}]},
     {?eh,tc_start,{dummy_SUITE,tc1}},
     {?eh,tc_done,{dummy_SUITE,tc1,ok}},
     {?eh,test_stats,{4,0,{0,0}}},
     {?eh,tc_start,{dummy_SUITE,tc2}},
     {?eh,tc_done,{dummy_SUITE,tc2,ok}},
     {?eh,test_stats,{5,0,{0,0}}},
     [{?eh,tc_start,{dummy_SUITE,{init_per_group,g2,[]}}},
      {?eh,tc_done,{dummy_SUITE,{init_per_group,g2,[]},ok}},
      {?eh,tc_start,{dummy_SUITE,tc4}},
      {?eh,tc_done,{dummy_SUITE,tc4,ok}},
      {?eh,test_stats,{6,0,{0,0}}},
      {?eh,tc_start,{dummy_SUITE,tc5}},
      {?eh,tc_done,{dummy_SUITE,tc5,ok}},
      {?eh,test_stats,{7,0,{0,0}}},
      {?eh,tc_start,{dummy_SUITE,{end_per_group,g2,[]}}},
      {?eh,tc_done,{dummy_SUITE,{end_per_group,g2,[]},ok}}],
     {?eh,tc_start,{dummy_SUITE,end_per_suite}},
     {?eh,tc_done,{dummy_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}].
