%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

%%
%% Stress tests of rwmutex implementation.
%%
%% Author: Rickard Green
%%
-module(mtx_SUITE).

%%-define(line_trace,true).

-include_lib("common_test/include/ct.hrl").

-export([all/0,suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([long_rwlock/1,
	 hammer_ets_rwlock/1,
	 hammer_rwlock/1,
	 hammer_rwlock_check/1,
	 hammer_tryrwlock/1,
	 hammer_tryrwlock_check/1,
	 hammer_sched_long_rwlock/1,
	 hammer_sched_long_rwlock_check/1,
	 hammer_sched_long_freqread_rwlock/1,
	 hammer_sched_long_freqread_rwlock_check/1,
	 hammer_sched_long_tryrwlock/1,
	 hammer_sched_long_tryrwlock_check/1,
	 hammer_sched_long_freqread_tryrwlock/1,
	 hammer_sched_long_freqread_tryrwlock_check/1,
	 hammer_sched_rwlock/1,
	 hammer_sched_rwlock_check/1,
	 hammer_sched_freqread_rwlock/1,
	 hammer_sched_freqread_rwlock_check/1,
	 hammer_sched_tryrwlock/1,
	 hammer_sched_tryrwlock_check/1,
	 hammer_sched_freqread_tryrwlock/1,
	 hammer_sched_freqread_tryrwlock_check/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 15}}].

all() ->
    [long_rwlock, hammer_rwlock_check, hammer_rwlock,
     hammer_tryrwlock_check, hammer_tryrwlock,
     hammer_ets_rwlock, hammer_sched_long_rwlock_check,
     hammer_sched_long_rwlock,
     hammer_sched_long_freqread_rwlock_check,
     hammer_sched_long_freqread_rwlock,
     hammer_sched_long_tryrwlock_check,
     hammer_sched_long_tryrwlock,
     hammer_sched_long_freqread_tryrwlock_check,
     hammer_sched_long_freqread_tryrwlock,
     hammer_sched_rwlock_check, hammer_sched_rwlock,
     hammer_sched_freqread_rwlock_check,
     hammer_sched_freqread_rwlock,
     hammer_sched_tryrwlock_check, hammer_sched_tryrwlock,
     hammer_sched_freqread_tryrwlock_check,
     hammer_sched_freqread_tryrwlock].

init_per_suite(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Lib = filename:join([DataDir, atom_to_list(?MODULE)]),
    case {erlang:load_nif(Lib, none),erlang:system_info(threads)} of
	{{error,_},false} ->
	    {skip, "No thread support"};
	_ ->
	    Config
    end.

end_per_suite(Config) when is_list(Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false),
    Config.

init_per_testcase(_Case, Config) ->
    %% Wait for deallocations to complete since we measure
    %% runtime in test cases.
    wait_deallocations(),
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

wait_deallocations() ->
    try
	erts_debug:set_internal_state(wait, deallocations)
    catch
	error:undef ->
	    erts_debug:set_internal_state(available_internal_state, true),
	    wait_deallocations()
    end.

long_rwlock(Config) when is_list(Config) ->
    statistics(runtime),
    LLRes = long_rw_test(),
    {_, RunTime} = statistics(runtime),
    %% A very short run time is expected, since
    %% threads in the test mostly wait
    io:format("RunTime=~p~n", [RunTime]),
    true = RunTime < 400,
    RunTimeStr = "Run-time during test was "++integer_to_list(RunTime)++" ms.",
    case LLRes of
	ok ->
	    {comment, RunTimeStr};
	{comment, Comment} ->
	    {comment, Comment ++ " " ++ RunTimeStr}
    end.

hammer_rwlock(Config) when is_list(Config) ->
    hammer_rw_test(false).

hammer_rwlock_check(Config) when is_list(Config) ->
    hammer_rw_test(true).

hammer_tryrwlock(Config) when is_list(Config) ->
    hammer_tryrw_test(false).

hammer_tryrwlock_check(Config) when is_list(Config) ->
    hammer_tryrw_test(true).

hammer_sched_rwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, false, true, 0, 0).

hammer_sched_rwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, true, true, 0, 0).

hammer_sched_freqread_rwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, false, true, 0, 0).

hammer_sched_freqread_rwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, true, true, 0, 0).

hammer_sched_tryrwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, false, false, 0, 100).

hammer_sched_tryrwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, true, false, 0, 100).

hammer_sched_freqread_tryrwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, false, false, 0, 100).

hammer_sched_freqread_tryrwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, true, false, 0, 100).

hammer_sched_long_rwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, false, true, 100, 0).

hammer_sched_long_rwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, true, true, 100, 0).

hammer_sched_long_freqread_rwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, false, true, 100, 0).

hammer_sched_long_freqread_rwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, true, true, 100, 0).

hammer_sched_long_tryrwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, false, false, 100, 100).

hammer_sched_long_tryrwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(false, true, false, 100, 100).

hammer_sched_long_freqread_tryrwlock(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, false, false, 100, 100).

hammer_sched_long_freqread_tryrwlock_check(Config) when is_list(Config) ->
    hammer_sched_rwlock_test(true, true, false, 100, 100).

hammer_sched_rwlock_test(FreqRead, LockCheck, Blocking, WaitLocked, WaitUnlocked) ->
    case create_rwlock(FreqRead, LockCheck) of
        enotsup ->
            {skipped, "Not supported."};
        RWLock ->
            Onln = erlang:system_info(schedulers_online),
            NWPs = case Onln div 3 of
                       1 -> case Onln < 4 of
                                true -> 1;
                                false -> 2
                            end;
                       X -> X
                   end,
            NRPs = Onln - NWPs,
            NoLockOps = ((((50000000 div Onln)
                           div case {Blocking, WaitLocked} of
                                   {false, 0} -> 1;
                                   _ -> 10
                               end)
                          div (case WaitLocked == 0 of
                                   true -> 1;
                                   false -> WaitLocked*250
                               end))
                         div handicap()),
            io:format("NoLockOps=~p~n", [NoLockOps]),
            Sleep = case Blocking of
                        true -> NoLockOps;
                        false -> NoLockOps div 10
                    end,
            WPs = lists:map(
                    fun (Sched) ->
                            spawn_opt(
                              fun () ->
                                      io:format("Writer on scheduler ~p.~n",
                                                [Sched]),
                                      Sched = erlang:system_info(scheduler_id),
                                      receive go -> gone end,
                                      hammer_sched_rwlock_proc(RWLock,
                                                               Blocking,
                                                               true,
                                                               WaitLocked,
                                                               WaitUnlocked,
                                                               NoLockOps,
                                                               Sleep),
                                      Sched = erlang:system_info(scheduler_id)
                              end,
                              [link, {scheduler, Sched}])
                    end,
                    lists:seq(1, NWPs)),
            RPs = lists:map(
                    fun (Sched) ->
                            spawn_opt(
                              fun () ->
                                      io:format("Reader on scheduler ~p.~n",
                                                [Sched]),
                                      Sched = erlang:system_info(scheduler_id),
                                      receive go -> gone end,
                                      hammer_sched_rwlock_proc(RWLock,
                                                               Blocking,
                                                               false,
                                                               WaitLocked,
                                                               WaitUnlocked,
                                                               NoLockOps,
                                                               Sleep),
                                      Sched = erlang:system_info(scheduler_id)
                              end,
                              [link, {scheduler, Sched}])
                    end,
                    lists:seq(NWPs + 1, NWPs + NRPs)),
            Procs = WPs ++ RPs,
            case {Blocking, WaitLocked} of
                {_, 0} -> ok;
                {false, _} -> ok;
                _ -> statistics(runtime)
            end,
            lists:foreach(fun (P) -> P ! go end, Procs),
            lists:foreach(fun (P) ->
                                  M = erlang:monitor(process, P),
                                  receive
                                      {'DOWN', M, process, P, _} ->
                                          ok
                                  end
                          end,
                          Procs),
            case {Blocking, WaitLocked} of
                {_, 0} -> ok;
                {false, _} -> ok;
                _ ->
                    {_, RunTime} = statistics(runtime),
                    io:format("RunTime=~p~n", [RunTime]),
                    true = RunTime < 700,
                    {comment,
                     "Run-time during test was "
                     ++ integer_to_list(RunTime)
                     ++ " ms."}
            end
    end.

hammer_sched_rwlock_proc(_RWLock,
			 _Blocking,
			 _WriteOp,
			 _WaitLocked,
			 _WaitUnlocked,
			 0,
			 _Sleep) ->
    ok;
hammer_sched_rwlock_proc(RWLock,
			 Blocking,
			 WriteOp,
			 WaitLocked,
			 WaitUnlocked,
			 Times,
			 Sleep) when Times rem Sleep == 0 ->
    rwlock_op(RWLock, Blocking, WriteOp, WaitLocked, WaitUnlocked),
    hammer_sched_rwlock_proc(RWLock,
			     Blocking,
			     WriteOp,
			     WaitLocked,
			     WaitUnlocked,
			     Times - 1,
			     Sleep);
hammer_sched_rwlock_proc(RWLock,
			 Blocking,
			 WriteOp,
			 WaitLocked,
			 WaitUnlocked,
			 Times,
			 Sleep) ->
    rwlock_op(RWLock, Blocking, WriteOp, WaitLocked, 0),
    hammer_sched_rwlock_proc(RWLock,
			     Blocking,
			     WriteOp,
			     WaitLocked,
			     WaitUnlocked,
			     Times - 1,
			     Sleep).

-define(HAMMER_ETS_RWLOCK_REPEAT_TIMES, 1).
-define(HAMMER_ETS_RWLOCK_TSIZE, 500).

hammer_ets_rwlock(Config) when is_list(Config) ->
    {Ops, Procs} = case handicap() of
		       1 -> {20000, 500};
		       2 -> {20000, 50};
		       3 -> {2000, 50};
		       _ -> {200, 50}
		   end,
    io:format("Procs=~p~nOps=~p~n", [Procs, Ops]),
    lists:foreach(fun (XOpts) ->
			  io:format("Running with extra opts: ~p", [XOpts]),
			  hammer_ets_rwlock_test(XOpts, true, 2, Ops,
						 Procs, false)
		  end,
		  [[],
		   [{read_concurrency, true}],
		   [{write_concurrency, true}],
		   [{read_concurrency, true},{write_concurrency, true}]]),
    ok.

%% Aux funcs

long_rw_test() ->
    exit(no_nif_implementation).

hammer_rw_test(_Arg) ->
    exit(no_nif_implementation).

hammer_tryrw_test(_Arg) ->
    exit(no_nif_implementation).

create_rwlock(_FreqRead, _LockCheck) ->
    exit(no_nif_implementation).

rwlock_op(_RWLock, _Blocking, _WriteOp, _WaitLocked, _WaitUnlocked) ->
    exit(no_nif_implementation).

hammer_ets_rwlock_put_data() ->
    put(?MODULE, {"here are some", data, "to store", make_ref()}).

hammer_ets_rwlock_get_data() ->
    get(?MODULE).

hammer_ets_rwlock_ops(_T, _UW, _N, _C, _SC, 0) ->
    ok;
hammer_ets_rwlock_ops(T, UW, N, C, SC, Tot) when N >= ?HAMMER_ETS_RWLOCK_TSIZE ->
    hammer_ets_rwlock_ops(T, UW, 0, C, SC, Tot);
hammer_ets_rwlock_ops(T, UW, N, 0, SC, Tot) ->
    case UW of
	true ->
	    true = ets:insert(T, {N, Tot, hammer_ets_rwlock_get_data()});
	false ->
	    [{N, _, _}] = ets:lookup(T, N)
    end,
    hammer_ets_rwlock_ops(T, UW, N+1, SC, SC, Tot-1);
hammer_ets_rwlock_ops(T, UW, N, C, SC, Tot) ->
    case UW of
	false ->
	    true = ets:insert(T, {N, Tot, hammer_ets_rwlock_get_data()});
	true ->
	    [{N, _, _}] = ets:lookup(T, N)
    end,
    hammer_ets_rwlock_ops(T, UW, N+1, C-1, SC, Tot-1).

hammer_ets_rwlock_init(T, N) when N < ?HAMMER_ETS_RWLOCK_TSIZE ->
    ets:insert(T, {N, N, N}),
    hammer_ets_rwlock_init(T, N+1);
hammer_ets_rwlock_init(_T, _N) ->
    ok.

hammer_ets_rwlock_test(XOpts, UW, C, N, NP, SC) ->
    receive after 100 -> ok end,
    {TP, TM} = spawn_monitor(
                 fun () ->
                         _L = repeat_list(
                                fun () ->
                                        Caller = self(),
                                        T = fun () ->
                                                    Parent = self(),
                                                    hammer_ets_rwlock_put_data(),
                                                    T=ets:new(x, [public | XOpts]),
                                                    hammer_ets_rwlock_init(T, 0),
                                                    Ps0 = repeat_list(
                                                            fun () ->
                                                                    spawn_link(
                                                                      fun () ->
                                                                              hammer_ets_rwlock_put_data(),
                                                                              receive go -> ok end,
                                                                              hammer_ets_rwlock_ops(T, UW, N, C, C, N),
                                                                              Parent ! {done, self()},
                                                                              receive after infinity -> ok end
                                                                      end)
                                                            end,
                                                            NP - case SC of
                                                                     false -> 0;
                                                                     _ -> 1
                                                                 end),
                                                    Ps = case SC of
                                                             false -> Ps0;
                                                             _ -> [spawn_link(fun () ->
                                                                                      hammer_ets_rwlock_put_data(),
                                                                                      receive go -> ok end,
                                                                                      hammer_ets_rwlock_ops(T, UW, N, SC, SC, N),
                                                                                      Parent ! {done, self()},
                                                                                      receive after infinity -> ok end
                                                                              end) | Ps0]
                                                         end,
                                                    Start = erlang:monotonic_time(),
                                                    lists:foreach(fun (P) -> P ! go end, Ps),
                                                    lists:foreach(fun (P) -> receive {done, P} -> ok end end, Ps),
                                                    Stop = erlang:monotonic_time(),
                                                    lists:foreach(fun (P) ->
                                                                          unlink(P),
                                                                          exit(P, bang),
                                                                          M = erlang:monitor(process, P),
                                                                          receive
                                                                              {'DOWN', M, process, P, _} -> ok
                                                                          end
                                                                  end, Ps),
                                                    Res = (Stop-Start)/erlang:convert_time_unit(1,second,native),
                                                    Caller ! {?MODULE, self(), Res}
                                            end,
                                        TP = spawn_link(T),
                                        receive
                                            {?MODULE, TP, Res} ->
                                                Res
                                        end
                                end,
                                ?HAMMER_ETS_RWLOCK_REPEAT_TIMES)
                 end),
    receive
        {'DOWN', TM, process, TP, _} -> ok
    end.

repeat_list(Fun, N) ->
    repeat_list(Fun, N, []).

repeat_list(_Fun, 0, Acc) ->
    Acc;
repeat_list(Fun, N, Acc) ->
    repeat_list(Fun, N-1, [Fun()|Acc]).


handicap() ->
    X0 = case catch (erlang:system_info(logical_processors_available) >=
                     erlang:system_info(schedulers_online)) of
             true -> 1;
             _ -> 2
         end,
    case erlang:system_info(build_type) of
        opt ->
            X0;
        ReallySlow when ReallySlow == debug;
                        ReallySlow == valgrind;
                        ReallySlow == purify ->
            X0*3;
        _Slow ->
            X0*2
    end.
