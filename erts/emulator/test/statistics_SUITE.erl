%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

-module(statistics_SUITE).

%% Tests the statistics/1 bif.

-export([all/0, suite/0, groups/0,
         wall_clock_sanity/1,
	 wall_clock_zero_diff/1, wall_clock_update/1,
         runtime_sanity/1,
         runtime_zero_diff/1,
	 runtime_update/1, runtime_diff/1,
	 run_queue_one/1,
	 scheduler_wall_time/1,
         scheduler_wall_time_all/1,
         msb_scheduler_wall_time/1,
	 reductions/1, reductions_big/1, garbage_collection/1, io/1,
	 badarg/1, run_queues_lengths_active_tasks/1, msacc/1]).

%% Internal exports.

-export([hog/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() -> 
    [{group, wall_clock}, {group, runtime}, reductions,
     reductions_big, {group, run_queue},
     scheduler_wall_time, scheduler_wall_time_all,
     msb_scheduler_wall_time,
     garbage_collection, io, badarg,
     run_queues_lengths_active_tasks,
     msacc].

groups() -> 
    [{wall_clock, [],
      [wall_clock_sanity, wall_clock_zero_diff, wall_clock_update]},
     {runtime, [],
      [runtime_sanity, runtime_zero_diff, runtime_update, runtime_diff]},
     {run_queue, [], [run_queue_one]}].

wall_clock_sanity(Config) when is_list(Config) ->
    erlang:yield(),
    {WallClock, _} = statistics(wall_clock),
    MT = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(MT - erlang:system_info(start_time),
                                    native, millisecond),
    io:format("Time=~p WallClock=~p~n",
              [Time, WallClock]),
    true = WallClock =< Time,
    true = Time - 100 =< WallClock,
    ok.

%%% Testing statistics(wall_clock).

%% Tests that the 'Wall clock since last call' element of the result
%% is zero when statistics(runtime) is called twice in succession.
wall_clock_zero_diff(Config) when is_list(Config) ->
    wall_clock_zero_diff1(16).

wall_clock_zero_diff1(N) when N > 0 ->
    {Time, _} = statistics(wall_clock),
    case statistics(wall_clock) of
        {Time, 0} -> ok;
        _ -> wall_clock_zero_diff1(N-1)
    end;
wall_clock_zero_diff1(0) ->
    ct:fail("Difference never zero.").

%% Test that the time differences returned by two calls to
%% statistics(wall_clock) are compatible, and are within a small number
%% of ms of the amount of real time we waited for.
wall_clock_update(Config) when is_list(Config) ->
    N = 10,
    Inc = 200,
    TotalTime = wall_clock_update1(N, Inc, 0),
    Overhead = TotalTime - N * Inc,
    IsDebug = test_server:is_debug(),

    %% Check that the average overhead is reasonable.
    if
        Overhead < N * 100 ->
            ok;
        IsDebug, Overhead < N * 1000 ->
            ok;
        true ->
            io:format("There was an overhead of ~p ms during ~p rounds.",
                      [Overhead,N]),
            ct:fail(too_much_overhead)
    end.

wall_clock_update1(N, Inc, Total) when N > 0 ->
    {Time1, _} = statistics(wall_clock),
    receive after Inc -> ok end,
    {Time2, WcDiff} = statistics(wall_clock),
    WcDiff = Time2 - Time1,
    io:format("Wall clock diff = ~p (expected at least ~p)\n", [WcDiff,Inc]),
    true = WcDiff >= Inc,
    wall_clock_update1(N-1, Inc, Total + WcDiff);
wall_clock_update1(0, _, Total) ->
    Total.


%%% Test statistics(runtime).

runtime_sanity(Config) when is_list(Config) ->
    case erlang:system_info(logical_processors_available) of
        unknown ->
            {skipped, "Don't know available logical processors"};
        LP when is_integer(LP) ->
            erlang:yield(),
            {RunTime, _} = statistics(runtime),
            MT = erlang:monotonic_time(),
            Time = erlang:convert_time_unit(MT - erlang:system_info(start_time),
                                            native, millisecond),
            io:format("Time=~p RunTime=~p~n",
                      [Time, RunTime]),
            true = RunTime =< Time*LP
    end.

%% Tests that the difference between the times returned from two consectuitive
%% calls to statistics(runtime) is zero.
runtime_zero_diff(Config) when is_list(Config) ->
    runtime_zero_diff1(16).

runtime_zero_diff1(N) when N > 0 ->
    {T1, _} = statistics(runtime),
    case statistics(runtime) of
        {T1, 0} -> ok;
        _ -> runtime_zero_diff1(N-1)
    end;
runtime_zero_diff1(0) ->
    ct:fail("statistics(runtime) never returned zero difference").

%% Test that the statistics(runtime) returns a substanstially
%% updated difference after running a process that takes all CPU
%% power of the Erlang process for a second.
runtime_update(Config) when is_list(Config) ->
    case test_server:is_cover() of
        false ->
            process_flag(priority, high),
            do_runtime_update(10);
        true ->
            {skip,"Cover-compiled"}
    end.

do_runtime_update(0) ->
    {comment,"Never close enough"};
do_runtime_update(N) ->
    {T1,Diff0} = statistics(runtime),
    {CPUHog, CPUHogMon} = spawn_opt(fun cpu_heavy/0,[link,monitor]),
    receive after 1000 -> ok end,
    {T2,Diff} = statistics(runtime),
    unlink(CPUHog),
    exit(CPUHog, kill),
    
    true = is_integer(T1+T2+Diff0+Diff),
    io:format("T1 = ~p, T2 = ~p, Diff = ~p, T2-T1 = ~p", [T1,T2,Diff,T2-T1]),
    receive {'DOWN',CPUHogMon,process,CPUHog,_} -> ok end,
    if
        T2 - T1 =:= Diff, 900 =< Diff, Diff =< 1500 -> ok;
        true -> do_runtime_update(N-1)
    end.

cpu_heavy() ->
    cpu_heavy().

%% Test that the difference between two consecutive absolute runtimes is
%% equal to the last relative runtime. The loop runs a lot of times since
%% the bug which this test case tests for showed up only rarely.
runtime_diff(Config) when is_list(Config) ->
    runtime_diff1(1000).

runtime_diff1(N) when N > 0 ->
    {T1_wc_time, _} = statistics(runtime),
    do_much(),
    {T2_wc_time, Wc_Diff} = statistics(runtime),
    Wc_Diff = T2_wc_time - T1_wc_time,
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


%% Test that statistics(reductions) is callable, and that
%% Total_Reductions and Reductions_Since_Last_Call make sense.
%% This to fail on pre-R3A version of JAM.
reductions(Config) when is_list(Config) ->
    {Reductions, _} = statistics(reductions),

    %% Each loop of reductions/2 takes 4 reductions + that the garbage built
    %% outside the heap in the BIF calls will bump the reductions.
    %% 300 * 4 is more than CONTEXT_REDS (1000).  Thus, there will be one or
    %% more context switches.

    Mask = (1 bsl erlang:system_info(wordsize)*8) - 1,
    reductions(300, Reductions, Mask).

reductions(N, Previous, Mask) when N > 0 ->
    {Reductions, Diff} = statistics(reductions),
    build_some_garbage(),
    if Reductions > 0 -> ok end,
    if Diff >= 0 -> ok end,
    io:format("Previous = ~p, Reductions = ~p, Diff = ~p, DiffShouldBe = ~p",
              [Previous, Reductions, Diff, (Reductions-Previous) band Mask]),
    if Reductions == ((Previous+Diff) band Mask) -> reductions(N-1, Reductions, Mask) end;
reductions(0, _, _) ->
    ok.

build_some_garbage() ->
    %% This will build garbage outside the process heap, which will cause
    %% a garbage collection in the scheduler.
    processes().

%% Test that the number of reductions can be returned as a big number.
reductions_big(Config) when is_list(Config) ->
    reductions_big_loop(),
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


%% Tests that statistics(run_queue) returns 1 if we start a
%% CPU-bound process.
run_queue_one(Config) when is_list(Config) ->
    MS = erlang:system_flag(multi_scheduling, block),
    run_queue_one_test(Config),
    erlang:system_flag(multi_scheduling, unblock),
    case MS of
        blocked ->
            {comment,
             "Multi-scheduling blocked during test. This test-case "
             "was not written to work with multiple schedulers."};
        _ -> ok
    end.


run_queue_one_test(Config) when is_list(Config) ->
    _Hog = spawn_link(?MODULE, hog, [self()]),
    receive
        hog_started -> ok
    end,
    receive after 100 -> ok end,		% Give hog a head start.
    case statistics(run_queue) of
        N when N >= 1 -> ok;
        Other -> ct:fail({unexpected,Other})
    end,
    ok.

%% CPU-bound process, going at low priority.  It will always be ready
%% to run.

hog(Pid) ->
    process_flag(priority, low),
    Pid ! hog_started,
    Mon = erlang:monitor(process, Pid),
    hog_iter(0, Mon).

hog_iter(N, Mon) when N > 0 ->
    receive
        {'DOWN', Mon, _, _, _} ->  ok
    after 0 ->
              hog_iter(N-1, Mon)
    end;
hog_iter(0, Mon) ->
    hog_iter(10000, Mon).

%%% Tests of statistics(scheduler_wall_time).

%% Tests that statistics(scheduler_wall_time) works as intended
scheduler_wall_time(Config) when is_list(Config) ->
    scheduler_wall_time_test(scheduler_wall_time).

%% Tests that statistics(scheduler_wall_time_all) works as intended
scheduler_wall_time_all(Config) when is_list(Config) ->
    scheduler_wall_time_test(scheduler_wall_time_all).

scheduler_wall_time_test(Type) ->
    case string:find(erlang:system_info(system_version),
                     "dirty-schedulers-TEST") == nomatch of
        true -> run_scheduler_wall_time_test(Type);
        false -> {skip, "Cannot be run with dirty-schedulers-TEST build"}
    end.

run_scheduler_wall_time_test(Type) ->
    %% Should return undefined if system_flag is not turned on yet
    undefined = statistics(Type),
    %% Turn on statistics
    false = erlang:system_flag(scheduler_wall_time, true),
    try
        Schedulers = erlang:system_info(schedulers_online),
        DirtyCPUSchedulers = erlang:system_info(dirty_cpu_schedulers_online),
        DirtyIOSchedulers = erlang:system_info(dirty_io_schedulers),
        TotLoadSchedulers = case Type of
                                scheduler_wall_time_all ->
                                    Schedulers + DirtyCPUSchedulers + DirtyIOSchedulers;
                                scheduler_wall_time ->
                                    Schedulers + DirtyCPUSchedulers
                        end,
                            
        %% Let testserver and everyone else finish their work
        timer:sleep(1500),
        %% Empty load
        EmptyLoad = get_load(Type),
        {false, _} = {lists:any(fun(Load) -> Load > 50 end, EmptyLoad),EmptyLoad},
        MeMySelfAndI = self(),
        StartHog = fun() ->
                           Pid = spawn_link(?MODULE, hog, [self()]),
                           receive hog_started -> MeMySelfAndI ! go end,
                           Pid
                   end,
        StartDirtyHog = fun(Func) ->
                                F = fun () ->
                                            erts_debug:Func(alive_waitexiting,
                                                            MeMySelfAndI)
                                    end,
                                Pid = spawn_link(F),
                                receive {alive, Pid} -> ok end,
                                Pid
                        end,
        P1 = StartHog(),
        %% Max on one, the other schedulers empty (hopefully)
        %% Be generous the process can jump between schedulers
        %% which is ok and we don't want the test to fail for wrong reasons
        _L1 = [S1Load|EmptyScheds1] = get_load(Type),
        {true,_}  = {S1Load > 50,S1Load},
        {false,_} = {lists:any(fun(Load) -> Load > 50 end, EmptyScheds1),EmptyScheds1},
        {true,_}  = {lists:sum(EmptyScheds1) < 60,EmptyScheds1},

        %% 50% load
        HalfHogs = [StartHog() || _ <- lists:seq(1, (Schedulers-1) div 2)],
        HalfDirtyCPUHogs = [StartDirtyHog(dirty_cpu)
                            || _ <- lists:seq(1, lists:max([1,DirtyCPUSchedulers div 2]))],
        HalfDirtyIOHogs = [StartDirtyHog(dirty_io)
                           || _ <- lists:seq(1, lists:max([1,DirtyIOSchedulers div 2]))],
        HalfLoad = lists:sum(get_load(Type)) div TotLoadSchedulers,
        if Schedulers < 2, HalfLoad > 80 -> ok; %% Ok only one scheduler online and one hog
           %% We want roughly 50% load
           HalfLoad > 40, HalfLoad < 60 -> ok;
           true -> exit({halfload, HalfLoad})
        end,

        %% 100% load
        LastHogs = [StartHog() || _ <- lists:seq(1, Schedulers div 2)],
        LastDirtyCPUHogs = [StartDirtyHog(dirty_cpu)
                            || _ <- lists:seq(1, DirtyCPUSchedulers div 2)],
        LastDirtyIOHogs = [StartDirtyHog(dirty_io)
                           || _ <- lists:seq(1, DirtyIOSchedulers div 2)],
        FullScheds = get_load(Type),
        {false,_} = {lists:any(fun(Load) -> Load < 80 end, FullScheds),FullScheds},
        FullLoad = lists:sum(FullScheds) div TotLoadSchedulers,
        if FullLoad > 90 -> ok;
           true -> exit({fullload, FullLoad})
        end,

	KillHog = fun (HP) ->
			  HPM = erlang:monitor(process, HP),
                          unlink(HP),
			  exit(HP, kill),
			  receive
			      {'DOWN', HPM, process, HP, killed} ->
				  ok
			  end
		  end,
        [KillHog(Pid) || Pid <- [P1|HalfHogs++HalfDirtyCPUHogs++HalfDirtyIOHogs
                                 ++LastHogs++LastDirtyCPUHogs++LastDirtyIOHogs]],
        receive after 2000 -> ok end, %% Give dirty schedulers time to complete...
        AfterLoad = get_load(Type),
	io:format("AfterLoad=~p~n", [AfterLoad]),
        {false,_} = {lists:any(fun(Load) -> Load > 25 end, AfterLoad),AfterLoad},
        true = erlang:system_flag(scheduler_wall_time, false)
    after
        erlang:system_flag(scheduler_wall_time, false)
    end.

get_load(Type) ->
    Start = erlang:statistics(Type),
    timer:sleep(1500),
    End = erlang:statistics(Type),
    lists:reverse(lists:sort(load_percentage(lists:sort(Start),lists:sort(End)))).

load_percentage([{Id, WN, TN}|Ss], [{Id, WP, TP}|Ps]) ->
    [100*(WN-WP) div (TN-TP)|load_percentage(Ss, Ps)];
load_percentage([], []) -> [].

count(0) ->
    ok;
count(N) ->
    count(N-1).

msb_swt_hog(true) ->
    count(1000000),
    erts_debug:dirty_cpu(wait, 10),
    erts_debug:dirty_io(wait, 10),
    msb_swt_hog(true);
msb_swt_hog(false) ->
    count(1000000),
    msb_swt_hog(false).

msb_scheduler_wall_time(_Config) ->
    erlang:system_flag(scheduler_wall_time, true),
    Dirty = erlang:system_info(dirty_cpu_schedulers) /= 0,
    Hogs = lists:map(fun (_) ->
                             spawn_opt(fun () ->
                                               msb_swt_hog(Dirty)
                                       end, [{priority,low}, link, monitor])
                     end, lists:seq(1,10)),
    erlang:system_flag(multi_scheduling, block),
    try
        SWT1 = lists:sort(statistics(scheduler_wall_time_all)),
        %% io:format("SWT1 = ~p~n", [SWT1]),
        receive after 4000 -> ok end,
        SWT2 = lists:sort(statistics(scheduler_wall_time_all)),
        %% io:format("SWT2 = ~p~n", [SWT2]),
        SWT = lists:zip(SWT1, SWT2),
        io:format("SU = ~p~n", [lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
                                                  {I, (A1 - A0)/(T1 - T0)} end,
                                         SWT)]),
        {A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
                                     {Ai + (A1 - A0), Ti + (T1 - T0)}
                             end,
                             {0, 0},
                             SWT),
        TSU = A/T,
        WSU = ((TSU * (erlang:system_info(schedulers)
                       + erlang:system_info(dirty_cpu_schedulers)
                       + erlang:system_info(dirty_io_schedulers)))
               / 1),
        %% Weighted scheduler utilization should be
        %% very close to 1.0, i.e., we execute the
        %% same time as one thread executing all
        %% the time...
        io:format("WSU = ~p~n", [WSU]),
        true = 0.9 < WSU andalso WSU < 1.1,
        ok
    after
        erlang:system_flag(multi_scheduling, unblock),
        erlang:system_flag(scheduler_wall_time, false),
        lists:foreach(fun ({HP, _HM}) ->
                              unlink(HP),
                              exit(HP, kill)
                      end, Hogs),
        lists:foreach(fun ({HP, HM}) ->
                              receive
                                  {'DOWN', HM, process, HP, _} ->
                                      ok
                              end
                      end, Hogs),
        ok
    end.

%% Tests that statistics(garbage_collection) is callable.
%% It is not clear how to test anything more.
garbage_collection(Config) when is_list(Config) ->
    Bin = list_to_binary(lists:duplicate(19999, 42)),
    case statistics(garbage_collection) of
        {Gcs0,R,0} when is_integer(Gcs0), is_integer(R) ->
            io:format("Reclaimed: ~p", [R]),
            Gcs = garbage_collection_1(Gcs0, Bin),
            io:format("Reclaimed: ~p",
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

%% Tests that statistics(io) is callable.
%% This could be improved to test something more.
io(Config) when is_list(Config) ->
    case statistics(io) of
        {{input,In},{output,Out}} when is_integer(In), is_integer(Out) -> ok
    end.

%% Tests that some illegal arguments to statistics fails.
badarg(Config) when is_list(Config) ->
    case catch statistics(1) of
        {'EXIT', {badarg, _}} -> ok
    end,
    case catch statistics(bad_atom) of
        {'EXIT', {badarg, _}} -> ok
    end.

tok_loop() ->
    tok_loop().

run_queues_lengths_active_tasks(_Config) ->
    TokLoops = lists:map(fun (_) ->
                                 spawn_opt(fun () ->
                                                   tok_loop()
                                           end,
                                           [link, {priority, low}])
                         end,
                         lists:seq(1,10)),

                   

    TRQLs0 = statistics(total_run_queue_lengths),
    TRQLAs0 = statistics(total_run_queue_lengths_all),
    TATs0 = statistics(total_active_tasks),
    TATAs0 = statistics(total_active_tasks_all),
    true = is_integer(TRQLs0),
    true = is_integer(TATs0),
    true = TRQLs0 >= 0,
    true = TRQLAs0 >= 0,
    true = TATs0 >= 11,
    true = TATAs0 >= 11,

    NoScheds = erlang:system_info(schedulers),
    {DefRqs,
     AllRqs} = case erlang:system_info(dirty_cpu_schedulers) of
                   0 -> {NoScheds, NoScheds};
                   _ -> {NoScheds+1, NoScheds+2}
               end,
    RQLs0 = statistics(run_queue_lengths),
    RQLAs0 = statistics(run_queue_lengths_all),
    ATs0 = statistics(active_tasks),
    ATAs0 = statistics(active_tasks_all),
    DefRqs = length(RQLs0),
    AllRqs = length(RQLAs0),
    DefRqs = length(ATs0),
    AllRqs = length(ATAs0),
    true = lists:sum(RQLs0) >= 0,
    true = lists:sum(RQLAs0) >= 0,
    true = lists:sum(ATs0) >= 11,
    true = lists:sum(ATAs0) >= 11,

    SO = erlang:system_flag(schedulers_online, 1),

    %% Give newly suspended schedulers some time to
    %% migrate away work from their run queues...
    receive after 1000 -> ok end,

    TRQLs1 = statistics(total_run_queue_lengths),
    TATs1 = statistics(total_active_tasks),
    true = TRQLs1 >= 10,
    true = TATs1 >= 11,
    NoScheds = erlang:system_info(schedulers),

    RQLs1 = statistics(run_queue_lengths),
    ATs1 = statistics(active_tasks),
    DefRqs = length(RQLs1),
    DefRqs = length(ATs1),
    TRQLs2 = lists:sum(RQLs1),
    TATs2 = lists:sum(ATs1),
    true = TRQLs2 >= 10,
    true = TATs2 >= 11,
    [TRQLs2|_] = RQLs1,
    [TATs2|_] = ATs1,

    erlang:system_flag(schedulers_online, SO),

    lists:foreach(fun (P) ->
                          unlink(P),
                          exit(P, bang)
                  end,
                  TokLoops),

    ok.

%% Tests that statistics(microstate_statistics) works.
msacc(Config) ->

    %% Test if crypto nif is available
    Niff = try crypto:strong_rand_bytes(1), ok catch _:_ -> nok end,
    TmpFile = filename:join(proplists:get_value(priv_dir,Config),"file.tmp"),

    false = erlang:system_flag(microstate_accounting, true),

    msacc_test(TmpFile),

    true = erlang:system_flag(microstate_accounting, false),

    MsaccStats = erlang:statistics(microstate_accounting),

    case os:type() of
        {win32, _} ->
            %% Some windows have a very poor accuracy on their
            %% timing primitives, so we just make sure that
            %% some state besides sleep has been triggered.
            Sum = lists:sum(
                    lists:map(fun({sleep, _V}) -> 0;
                                 ({_, V}) -> V
                              end, maps:to_list(msacc_sum_states()))
                   ),
            if Sum > 0 ->
                   ok;
               true ->
                   ct:fail({no_states_triggered, MsaccStats})
            end;
        _ ->

            %% Make sure that all states were triggered at least once
            maps:map(fun(nif, 0) ->
                             case Niff of
                                 ok ->
                                     ct:fail({zero_state,nif});
                                 nok ->
                                     ok
                             end;
                        (aux, 0) ->
                             %% aux will be zero if we do not have smp support
                             %% or no async threads
                             case erlang:system_info(thread_pool_size) > 0 of
                                 false ->
                                     ok;
                                 true ->
                                     ct:log("msacc: ~p",[MsaccStats]),
                                     ct:fail({zero_state,aux})
                             end;
                        (Key, 0) ->
                             ct:log("msacc: ~p",[MsaccStats]),
                             ct:fail({zero_state,Key});
                        (_,_) -> ok
                     end, msacc_sum_states())
    end,

    erlang:system_flag(microstate_accounting, reset),

    msacc_test(TmpFile),

    %% Make sure all counters are zero after stopping and resetting
    maps:map(fun(_Key, 0) -> ok;
                (Key,_) ->
                     ct:log("msacc: ~p",[erlang:statistics(microstate_accounting)]),
                     ct:fail({non_zero_state,Key})
             end,msacc_sum_states()).

%% This test tries to make sure to trigger all of the different available states
msacc_test(TmpFile) ->

    %% We write some data
    [file:write_file(TmpFile,<<0:(1024*1024*8)>>,[raw]) || _ <- lists:seq(1,100)],

    %% Do some ETS operations
    Tid = ets:new(table, []),
    ets:insert(Tid, {1, hello}),
    ets:delete(Tid),

    %% Check some IO
    {ok, L} = gen_tcp:listen(0, [{active, true},{reuseaddr,true}]),
    {ok, Port} = inet:port(L),
    Pid = spawn(fun() ->
                        {ok, S} = gen_tcp:accept(L),
                        (fun F() -> receive M -> F() end end)()
                end),
    {ok, C} = gen_tcp:connect("localhost", Port, []),
    [begin gen_tcp:send(C,"hello"),timer:sleep(1) end || _ <- lists:seq(1,100)],

    %% Collect some garbage
    [erlang:garbage_collect() || _ <- lists:seq(1,100)],

    %% Send some messages
    [begin self() ! {hello},receive _ -> ok end end ||  _ <- lists:seq(1,100)],

    %% Setup some timers
    Refs = [erlang:send_after(10000,self(),ok) ||  _ <- lists:seq(1,100)],

    %% Do some nif work
    catch [crypto:strong_rand_bytes(128) || _ <- lists:seq(1,100)],

    %% Cancel some timers
    [erlang:cancel_timer(R) ||  R <- Refs],

    %% Wait for a while
    timer:sleep(100).

msacc_sum_states() ->
    Stats = erlang:statistics(microstate_accounting),
    [#{ counters := C }|_] = Stats,
    InitialCounters = maps:map(fun(_,_) -> 0 end,C),
    lists:foldl(fun(#{ counters := Counters }, Cnt) ->
                        maps:fold(fun(Key, Value, Acc) ->
                                          NewValue = Value+maps:get(Key,Acc),
                                          maps:update(Key, NewValue, Acc)
                                  end, Cnt, Counters)
                end,InitialCounters,Stats).
