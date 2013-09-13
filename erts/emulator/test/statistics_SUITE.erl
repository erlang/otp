%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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

-module(statistics_SUITE).

%% Tests the statistics/1 bif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 wall_clock_zero_diff/1, wall_clock_update/1,
	 runtime_zero_diff/1,
	 runtime_update/1, runtime_diff/1,
	 run_queue_one/1,
	 scheduler_wall_time/1,
	 reductions/1, reductions_big/1, garbage_collection/1, io/1,
	 badarg/1]).

%% Internal exports.

-export([hog/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(_, Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(300)),
    [{watchdog, Dog}|Config].

end_per_testcase(_, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, wall_clock}, {group, runtime}, reductions,
     reductions_big, {group, run_queue}, scheduler_wall_time,
     garbage_collection, io, badarg].

groups() -> 
    [{wall_clock, [],
      [wall_clock_zero_diff, wall_clock_update]},
     {runtime, [],
      [runtime_zero_diff, runtime_update, runtime_diff]},
     {run_queue, [], [run_queue_one]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%% Testing statistics(wall_clock).



wall_clock_zero_diff(doc) ->
    "Tests that the 'Wall clock since last call' element of the result "
    "is zero when statistics(runtime) is called twice in succession.";
wall_clock_zero_diff(Config) when is_list(Config) ->
    wall_clock_zero_diff1(16).

wall_clock_zero_diff1(N) when N > 0 ->
    ?line {Time, _} = statistics(wall_clock),
    ?line case statistics(wall_clock) of
	      {Time, 0} -> ok;
	      _ -> wall_clock_zero_diff1(N-1)
    end;
wall_clock_zero_diff1(0) ->
    ?line test_server:fail("Difference never zero.").

wall_clock_update(doc) ->
    "Test that the time differences returned by two calls to "
    "statistics(wall_clock) are compatible, and are within a small number "
    "of ms of the amount of real time we waited for.";
wall_clock_update(Config) when is_list(Config) ->
    wall_clock_update1(6).

wall_clock_update1(N) when N > 0 ->
    ?line {T1_wc_time, _} = statistics(wall_clock),
    ?line receive after 1000 -> ok end,
    ?line {T2_wc_time, Wc_Diff} = statistics(wall_clock),

    ?line Wc_Diff = T2_wc_time - T1_wc_time,
    ?line test_server:format("Wall clock diff = ~p; should be  = 1000..1040~n",
			     [Wc_Diff]),
    case ?t:is_debug() of
	false ->
	    ?line true = Wc_Diff =< 1040;
	true ->
	    ?line true = Wc_Diff =< 2000	%Be more tolerant in debug-compiled emulator.
    end,
    ?line true = Wc_Diff >= 1000,
    wall_clock_update1(N-1);
wall_clock_update1(0) ->
    ok.


%%% Test statistics(runtime).


runtime_zero_diff(doc) ->
    "Tests that the difference between the times returned from two consectuitive "
	"calls to statistics(runtime) is zero.";
runtime_zero_diff(Config) when is_list(Config) ->
    ?line runtime_zero_diff1(16).

runtime_zero_diff1(N) when N > 0 ->
    ?line {T1, _} = statistics(runtime),
    ?line case statistics(runtime) of
	      {T1, 0} -> ok;
	      _ -> runtime_zero_diff1(N-1)
	  end;
runtime_zero_diff1(0) ->
    ?line test_server:fail("statistics(runtime) never returned zero difference").

runtime_update(doc) ->
    "Test that the statistics(runtime) returns a substanstially "
	"updated difference after running a process that takes all CPU "
	" power of the Erlang process for a second.";
runtime_update(Config) when is_list(Config) ->
    case ?t:is_cover() of
	false ->
	    ?line process_flag(priority, high),
	    do_runtime_update(10);
	true ->
	    {skip,"Cover-compiled"}
    end.

do_runtime_update(0) ->
    {comment,"Never close enough"};
do_runtime_update(N) ->
    ?line {T1,Diff0} = statistics(runtime),
    ?line spawn_link(fun cpu_heavy/0),
    receive after 1000 -> ok end,
    ?line {T2,Diff} = statistics(runtime),
    ?line true = is_integer(T1+T2+Diff0+Diff),
    ?line test_server:format("T1 = ~p, T2 = ~p, Diff = ~p, T2-T1 = ~p",
			     [T1,T2,Diff,T2-T1]),
    ?line if
	      T2 - T1 =:= Diff, 900 =< Diff, Diff =< 1500 -> ok;
	      true -> do_runtime_update(N-1)
	  end.
    
cpu_heavy() ->
    cpu_heavy().

runtime_diff(doc) ->
    "Test that the difference between two consecutive absolute runtimes is "
    "equal to the last relative runtime. The loop runs a lot of times since "
    "the bug which this test case tests for showed up only rarely.";
runtime_diff(Config) when is_list(Config) ->
    runtime_diff1(1000).

runtime_diff1(N) when N > 0 ->
    ?line {T1_wc_time, _} = statistics(runtime),
    ?line do_much(),
    ?line {T2_wc_time, Wc_Diff} = statistics(runtime),
    ?line Wc_Diff = T2_wc_time - T1_wc_time,
    runtime_diff1(N-1);
runtime_diff1(0) ->
    ok.

%%% do_much(100000) takes about 760 ms on boromir.
%%% do_much(1000) takes about 8 ms on boromir.

do_much() ->
    do_much(1000).

do_much(0) ->
    ok;
do_much(N) ->
    _ = 4784728478274827 * 72874284728472,
    do_much(N-1).


reductions(doc) ->
    "Test that statistics(reductions) is callable, and that "
	"Total_Reductions and Reductions_Since_Last_Call make sense. "
	"(This to fail on pre-R3A version of JAM.";
reductions(Config) when is_list(Config) ->
    {Reductions, _} = statistics(reductions),

    %% Each loop of reductions/2 takes 4 reductions + that the garbage built
    %% outside the heap in the BIF calls will bump the reductions.
    %% 300 * 4 is more than CONTEXT_REDS (1000).  Thus, there will be one or
    %% more context switches.

    Mask = (1 bsl erlang:system_info(wordsize)*8) - 1,
    reductions(300, Reductions, Mask).

reductions(N, Previous, Mask) when N > 0 ->
    ?line {Reductions, Diff} = statistics(reductions),
    ?line build_some_garbage(),
    ?line if Reductions > 0 -> ok end,
    ?line if Diff >= 0 -> ok end,
    io:format("Previous = ~p, Reductions = ~p, Diff = ~p, DiffShouldBe = ~p",
	      [Previous, Reductions, Diff, (Reductions-Previous) band Mask]),
    ?line if Reductions == ((Previous+Diff) band Mask) -> reductions(N-1, Reductions, Mask) end;
reductions(0, _, _) ->
    ok.

build_some_garbage() ->
    %% This will build garbage outside the process heap, which will cause
    %% a garbage collection in the scheduler.
    processes().

reductions_big(doc) ->
    "Test that the number of reductions can be returned as a big number.";
reductions_big(Config) when is_list(Config) ->
    ?line reductions_big_loop(),
    ok.

reductions_big_loop() ->
    erlang:yield(),
    case statistics(reductions) of
	{Red, Diff} when Red >= 16#7ffFFFF ->
	    ok = io:format("Reductions = ~w, Diff = ~w", [Red, Diff]);
	_ ->
	    reductions_big_loop()
    end.


%%% Tests of statistics(run_queue).


run_queue_one(doc) ->
    "Tests that statistics(run_queue) returns 1 if we start a "
    "CPU-bound process.";
run_queue_one(Config) when is_list(Config) ->
    ?line MS = erlang:system_flag(multi_scheduling, block),
    ?line run_queue_one_test(Config),
    ?line erlang:system_flag(multi_scheduling, unblock),
    case MS of
	blocked ->
	    {comment,
	     "Multi-scheduling blocked during test. This test-case "
	     "was not written to work with multiple schedulers."};
	_ -> ok
    end.
    

run_queue_one_test(Config) when is_list(Config) ->
    ?line _Hog = spawn_link(?MODULE, hog, [self()]),
    ?line receive
	      hog_started -> ok
	  end,
    ?line receive after 100 -> ok end,		% Give hog a head start.
    ?line case statistics(run_queue) of
	      N when N >= 1 -> ok;
	      Other -> ?line ?t:fail({unexpected,Other})
	  end,
    ok.

%% CPU-bound process, going at low priority.  It will always be ready
%% to run.

hog(Pid) ->
    ?line process_flag(priority, low),
    ?line Pid ! hog_started,
    ?line Mon = erlang:monitor(process, Pid),
    ?line hog_iter(0, Mon).

hog_iter(N, Mon) when N > 0 ->
    receive
	{'DOWN', Mon, _, _, _} ->  ok
    after 0 ->
	    ?line hog_iter(N-1, Mon)
    end;
hog_iter(0, Mon) ->
    ?line hog_iter(10000, Mon).

%%% Tests of statistics(scheduler_wall_time).

scheduler_wall_time(doc) ->
    "Tests that statistics(scheduler_wall_time) works as intended";
scheduler_wall_time(Config) when is_list(Config) ->
    %% Should return undefined if system_flag is not turned on yet
    undefined = statistics(scheduler_wall_time),
    %% Turn on statistics
    false = erlang:system_flag(scheduler_wall_time, true),
    try
	Schedulers = erlang:system_info(schedulers_online),
	%% Let testserver and everyone else finish their work
	timer:sleep(500),
	%% Empty load
	EmptyLoad = get_load(),
	{false, _} = {lists:any(fun(Load) -> Load > 50 end, EmptyLoad),EmptyLoad},
	MeMySelfAndI = self(),
	StartHog = fun() ->
			   Pid = spawn(?MODULE, hog, [self()]),
			   receive hog_started -> MeMySelfAndI ! go end,
			   Pid
		   end,
	P1 = StartHog(),
	%% Max on one, the other schedulers empty (hopefully)
	%% Be generous the process can jump between schedulers
	%% which is ok and we don't want the test to fail for wrong reasons
	_L1 = [S1Load|EmptyScheds1] = get_load(),
	{true,_}  = {S1Load > 50,S1Load},
	{false,_} = {lists:any(fun(Load) -> Load > 50 end, EmptyScheds1),EmptyScheds1},
	{true,_}  = {lists:sum(EmptyScheds1) < 60,EmptyScheds1},

	%% 50% load
	HalfHogs = [StartHog() || _ <- lists:seq(1, (Schedulers-1) div 2)],
	HalfLoad = lists:sum(get_load()) div Schedulers,
	if Schedulers < 2, HalfLoad > 80 -> ok; %% Ok only one scheduler online and one hog
	   %% We want roughly 50% load
	   HalfLoad > 40, HalfLoad < 60 -> ok;
	   true -> exit({halfload, HalfLoad})
	end,

	%% 100% load
	LastHogs = [StartHog() || _ <- lists:seq(1, Schedulers div 2)],
	FullScheds = get_load(),
	{false,_} = {lists:any(fun(Load) -> Load < 80 end, FullScheds),FullScheds},
	FullLoad = lists:sum(FullScheds) div Schedulers,
	if FullLoad > 90 -> ok;
	   true -> exit({fullload, FullLoad})
	end,

	[exit(Pid, kill) || Pid <- [P1|HalfHogs++LastHogs]],
	AfterLoad = get_load(),
	{false,_} = {lists:any(fun(Load) -> Load > 5 end, AfterLoad),AfterLoad},
	true = erlang:system_flag(scheduler_wall_time, false)
    after
	erlang:system_flag(scheduler_wall_time, false)
    end.

get_load() ->
    Start = erlang:statistics(scheduler_wall_time),
    timer:sleep(500),
    End = erlang:statistics(scheduler_wall_time),
    lists:reverse(lists:sort(load_percentage(lists:sort(Start),lists:sort(End)))).

load_percentage([{Id, WN, TN}|Ss], [{Id, WP, TP}|Ps]) ->
    [100*(WN-WP) div (TN-TP)|load_percentage(Ss, Ps)];
load_percentage([], []) -> [].


garbage_collection(doc) ->
    "Tests that statistics(garbage_collection) is callable. "
    "It is not clear how to test anything more.";
garbage_collection(Config) when is_list(Config) ->
    ?line Bin = list_to_binary(lists:duplicate(19999, 42)),
    ?line case statistics(garbage_collection) of
	      {Gcs0,R,0} when is_integer(Gcs0), is_integer(R) ->
		  ?line io:format("Reclaimed: ~p", [R]),
		  ?line Gcs = garbage_collection_1(Gcs0, Bin),
		  ?line io:format("Reclaimed: ~p",
				  [element(2, statistics(garbage_collection))]),
		  {comment,integer_to_list(Gcs-Gcs0)++" GCs"}
	  end.

garbage_collection_1(Gcs0, Bin) ->
    case statistics(garbage_collection) of
	{Gcs,Reclaimed,0} when Gcs >= Gcs0 ->
	    if
		Reclaimed > 16#7ffffff ->
		    Gcs;
		true ->
		    _ = binary_to_list(Bin),
		    erlang:garbage_collect(),
		    garbage_collection_1(Gcs, Bin)
	    end
    end.

io(doc) ->
    "Tests that statistics(io) is callable. "
    "This could be improved to test something more.";
io(Config) when is_list(Config) ->
    ?line case statistics(io) of
	      {{input,In},{output,Out}} when is_integer(In), is_integer(Out) -> ok
	  end.

badarg(doc) ->
    "Tests that some illegal arguments to statistics fails.";
badarg(Config) when is_list(Config) ->
    ?line case catch statistics(1) of
	      {'EXIT', {badarg, _}} -> ok
	  end,
    ?line case catch statistics(bad_atom) of
	      {'EXIT', {badarg, _}} -> ok
	  end.
