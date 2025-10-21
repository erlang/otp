%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2014-2025. All Rights Reserved.
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
%% @doc Microstate accounting utility function
%%
%%	This module provides a user interface for analysing
%%      erlang:statistics(microstate_accounting) data.
%%

-module(msacc).
-moduledoc """
Convenience functions for microstate accounting

This module implements some convenience functions for analyzing microstate
accounting data. For details about how to use the basic API and what the
different states represent, see
[`erlang:statistics(microstate_accounting)`](`m:erlang#statistics_microstate_accounting`).

[](){: #msacc_print_example }

_Basic Scenario_

```erlang
1> msacc:start(1000).
ok
2> msacc:print().
Average thread real-time    : 1000513 us
Accumulated system run-time :    2213 us
Average scheduler run-time  :    1076 us

        Thread      aux check_io emulator       gc    other     port    sleep

Stats per thread:
     async( 0)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
     async( 1)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
       aux( 1)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%   99.99%
 scheduler( 1)    0.00%    0.03%    0.13%    0.00%    0.01%    0.00%   99.82%
 scheduler( 2)    0.00%    0.00%    0.00%    0.00%    0.03%    0.00%   99.97%

Stats per type:
         async    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
           aux    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%   99.99%
     scheduler    0.00%    0.02%    0.06%    0.00%    0.02%    0.00%   99.89%
ok
```

This first command enables microstate accounting for 1000 milliseconds. See
`start/0`, `stop/0`, `reset/0`, and `start/1` for more details. The second
command prints the statistics gathered during that time. First three general
statistics are printed.

- **Average real-time** - The average time spent collecting data in the threads.
  This should be close to the time which data was collected.

- **System run-time** - The total run-time of all threads in the system. This is
  what you get if you call `msacc:stats(total_runtime,Stats).`

- **Average scheduler run-time** - The average run-time for the schedulers. This
  is the average amount of time the schedulers did not sleep.

Then one column per state is printed with a the percentage of time this thread
spent in the state out of it's own real-time. After the thread specific time,
the accumulated time for each type of thread is printed in a similar format.

Since we have the average real-time and the percentage spent in each state we
can easily calculate the time spent in each state by multiplying
`Average thread real-time` with `Thread state %`, that is, to get the time Scheduler
1 spent in the emulator state we do `1000513us * 0.13% = 1300us`.
""".
-moduledoc(#{since => "OTP 19.0"}).
-export([available/0, start/0, start/1, stop/0, reset/0, to_file/1,
         from_file/1, stats/0, stats/2, print/0, print/1, print/2,
         print/3]).

-type msacc_data() :: [msacc_data_thread()].

-type msacc_data_thread() :: #{ '$type' := msacc_data,
                                type := msacc_type(), id := msacc_id(),
                                counters := msacc_data_counters() }.
-doc """
A map containing the different microstate accounting states and the number of
microseconds spent in it.
""".
-type msacc_data_counters() :: #{ msacc_state() => non_neg_integer()}.

-type msacc_stats() :: [msacc_stats_thread()].
-doc """
A map containing information about a specific thread. The percentages in the map
can be either run-time or real-time depending on if `runtime` or `realtime` was
requested from `stats/2`. `system` is the percentage of total system time for
this specific thread.
""".
-type msacc_stats_thread() :: #{ '$type' := msacc_stats,
                                 type := msacc_type(), id := msacc_id(),
                                 system := float(),
                                 counters := msacc_stats_counters()}.
-doc """
A map containing the different microstate accounting states. Each value in the
map contains another map with the percentage of time that this thread has spent
in the specific state. Both the percentage of `system` time and the time for
that specific `thread` is part of the map.
""".
-type msacc_stats_counters() :: #{ msacc_state() => #{ thread := float(),
                                                       system := float()}}.


-type msacc_type() :: aux | async | dirty_cpu_scheduler
                    | dirty_io_scheduler | poll | scheduler.
-type msacc_id() :: non_neg_integer().
-doc """
The different states that a thread can be in. See
[erlang:statistics(microstate_accounting)](`m:erlang#statistics_microstate_accounting`)
for details.
""".
-type msacc_state() :: alloc | aux | bif | busy_wait | check_io |
                       emulator | ets | gc | gc_fullsweep | nif |
                       other | port | send | sleep | timers.

-doc "The different options that can be given to `print/2`.".
-type msacc_print_options() :: #{ system => boolean() }.

-doc "This function checks whether microstate accounting is available or not.".
-doc(#{since => <<"OTP 19.0">>}).
-spec available() -> boolean().
available() ->
    try
        [_|_] = erlang:statistics(microstate_accounting),
        true
    catch _:_ ->
            false
    end.

-doc """
Start microstate accounting. Returns whether it was previously enabled or
disabled.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec start() -> boolean().
start() ->
    erlang:system_flag(microstate_accounting, true).

-doc """
Stop microstate accounting. Returns whether is was previously enabled or
disabled.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec stop() -> boolean().
stop() ->
    erlang:system_flag(microstate_accounting, false).

-doc """
Reset microstate accounting counters. Returns whether is was enabled or
disabled.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec reset() -> boolean().
reset() ->
    erlang:system_flag(microstate_accounting, reset).

-doc """
Resets all counters and then starts microstate accounting for the given
milliseconds.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec start(Time) -> true when
      Time :: timeout().
start(Tmo) ->
    stop(), reset(), start(),
    timer:sleep(Tmo),
    stop().

-doc """
Dumps the current microstate statistics counters to a file that can be parsed
with `file:consult/1`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec to_file(Filename) -> ok | {error, file:posix()} when
      Filename :: file:name_all().
to_file(Filename) ->
    file:write_file(Filename, io_lib:format("~p.~n",[stats()])).

-doc "Read a file dump produced by [to_file(Filename)](`to_file/1`).".
-doc(#{since => <<"OTP 19.0">>}).
-spec from_file(Filename) -> msacc_data() when
      Filename :: file:name_all().
from_file(Filename) ->
    {ok, [Stats]} = file:consult(Filename),
    Stats.

-doc """
Prints the current microstate accounting to standard out. Equivalent to
[`msacc:print(msacc:stats(), #{}).`](`print/1`)
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec print() -> ok.
print() ->
    print(stats()).

-doc #{equiv => print(DataOrStats, #{})}.
-doc(#{since => <<"OTP 19.0">>}).
-spec print(DataOrStats) -> ok when
      DataOrStats :: msacc_data() | msacc_stats().
print(Stats) ->
    print(Stats, #{}).

-doc """
Print the given microstate statistics values to standard out. With many states
this can be verbose. See the top of this reference manual for a brief
description of what the fields mean.

It is possible to print more specific types of statistics by first manipulating
the `DataOrStats` using `stats/2`. For instance if you want to print the
percentage of run-time for each thread you can do:

```erlang
msacc:print(msacc:stats(runtime, msacc:stats())).
```

If you want to only print run-time per thread type you can do:

```erlang
msacc:print(msacc:stats(type, msacc:stats(runtime, msacc:stats()))).
```

_Options_

- **`system`** - Print percentage of time spent in each state out of system time
  as well as thread time. Default: false.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec print(DataOrStats, Options) -> ok when
      DataOrStats :: msacc_data() | msacc_stats(),
      Options :: msacc_print_options().
print(Stats, Options) ->
    print(group_leader(), Stats, Options).

-doc """
Print the given microstate statistics values to the given file or device. The
other arguments behave the same way as for `print/2`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec print(FileOrDevice, DataOrStats, Options) -> ok when
      FileOrDevice :: file:filename() | io:device(),
      DataOrStats :: msacc_data() | msacc_stats(),
      Options :: msacc_print_options().
print(Filename, Stats, Options) when is_list(Filename) ->
    case file:open(Filename,[write]) of
        {ok, D} -> print(D, Stats, Options),file:close(D);
        Error -> Error
    end;
print(Device, Stats, Options) ->
    DefaultOpts = #{ system => false },
    print_int(Device, Stats, maps:merge(DefaultOpts, Options)).
print_int(Device, [#{ '$type' := msacc_data, id := _Id }|_] = Stats, Options) ->
    TypeStats = stats(type, Stats),
    io:format(Device, "~s", [print_stats_overview(Stats, Options)]),
    io:format(Device, "~s", [print_stats_header(Stats, Options)]),
    io:format(Device, "~s", [print_stats_threads(
                               stats(realtime, Stats), Options)]),
    io:format(Device, "~s", [print_stats_type(
                               stats(realtime, TypeStats), Options)]);
print_int(Device, [#{ '$type' := msacc_data }|_] = Stats, Options) ->
    io:format(Device, "~s", [print_stats_header(Stats, Options)]),
    io:format(Device, "~s", [print_stats_type(
                               stats(realtime, Stats), Options)]);
print_int(Device, [#{ '$type' := msacc_stats, id := _Id }|_] = Stats,Options) ->
    io:format(Device, "~s", [print_stats_header(Stats, Options)]),
    io:format(Device, "~s", [print_stats_threads(Stats, Options)]),
    io:format(Device, "~s", [print_stats_type(
                               msacc:stats(type, Stats), Options)]);
print_int(Device, [#{ '$type' := msacc_stats }|_] = Stats, Options) ->
    io:format(Device, "~s", [print_stats_header(Stats, Options)]),
    io:format(Device, "~s", [print_stats_type(Stats, Options)]).


-doc """
Returns a runtime system independent version of the microstate statistics data
presented by
[`erlang:statistics(microstate_accounting)`](`m:erlang#statistics_microstate_accounting`).
All counters have been normalized to be in microsecond resolution.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec stats() -> msacc_data().
stats() ->
    Fun = fun F(K,{PerfCount,StateCount}) ->
                %% Need to handle ERTS_MSACC_STATE_COUNTERS
                {F(K,PerfCount),StateCount};
              F(_K,PerfCount) ->
                erlang:convert_time_unit(PerfCount, perf_counter, 1000000)
        end,
    UsStats = lists:map(
                fun(#{ counters := Cnt } = M) ->
                        UsCnt = maps:map(Fun,Cnt),
                        M#{ '$type' => msacc_data, counters := UsCnt }
                end, erlang:statistics(microstate_accounting)),
    statssort(UsStats).

-doc """
Returns the system time for the given microstate statistics values. System time
is the accumulated time of all threads.

- **`realtime`** - Returns all time recorded for all threads.

- **`runtime`** - Returns all time spent doing work for all threads, i.e. all
  time not spent in the `sleep` state.

Returns fractions of real-time or run-time spent in the various threads from the
given microstate statistics values.

Returns a list of microstate statistics values where the values for all threads
of the same type has been merged.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec stats(Analysis, Stats) -> non_neg_integer() when
      Analysis :: system_realtime | system_runtime,
      Stats :: msacc_data();
           (Analysis, Stats) -> msacc_stats() when
      Analysis :: realtime | runtime,
      Stats :: msacc_data();
           (Analysis, StatsOrData) -> msacc_data() | msacc_stats() when
      Analysis :: type,
      StatsOrData :: msacc_data() | msacc_stats().
stats(system_realtime, Stats) ->
    lists:foldl(fun(#{ counters := Cnt }, Acc) ->
                        get_total(Cnt, Acc)
                end, 0, Stats);
stats(system_runtime, Stats) ->
    lists:foldl(fun(#{ counters := Cnt }, Acc) ->
                        get_total(maps:remove(sleep, Cnt), Acc)
                end, 0, Stats);
stats(realtime, Stats) ->
    RealTime = stats(system_realtime, Stats),
    statssort([get_thread_perc(Thread, RealTime) || Thread <- Stats]);
stats(runtime, Stats) ->
    RunTime = stats(system_runtime, Stats),
    statssort([get_thread_perc(T#{ counters := maps:remove(sleep,Cnt)}, RunTime)
                               || T = #{ counters := Cnt } <- Stats]);
stats(type, Stats) ->
    statssort(merge_threads(Stats, [])).

print_stats_overview(Stats, _Options) ->
    RunTime = stats(system_runtime, Stats),
    RealTime = stats(system_realtime, Stats) div length(Stats),
    SchedStats = [S || #{ type := scheduler } = S <- Stats],
    AvgSchedRunTime = stats(system_runtime, SchedStats) div length(SchedStats),
    NumSize = if
                  RealTime > RunTime -> length(integer_to_list(RealTime));
                  true -> length(integer_to_list(RunTime))
              end,
    [io_lib:format("Average thread real-time    : ~*B us~n",
                   [NumSize, RealTime]),
     io_lib:format("Accumulated system run-time : ~*B us~n",
                   [NumSize, RunTime]),
     io_lib:format("Average scheduler run-time  : ~*B us~n",
                   [NumSize, AvgSchedRunTime]),
     io_lib:format("~n",[])].

print_stats_threads(Stats, Options) ->
    [io_lib:format("~nStats per thread:~n", []),
     [print_thread_info(Thread, Options) || Thread <- Stats]].

print_stats_type(Stats, Options) ->
    [io_lib:format("~nStats per type:~n", []),
     [print_thread_info(Thread, Options) || Thread <- Stats]].


print_stats_header([#{ counters := Cnt }|_], #{ system := PrintSys }) ->
    [io_lib:format("~14s", ["Thread"]),
     map(fun(Counter, _) when PrintSys->
                 io_lib:format("~9s     ", [atom_to_list(Counter)]);
            (Counter, _) ->
                 io_lib:format("~9s", [atom_to_list(Counter)])
         end, Cnt),
     io_lib:format("~n",[])].

print_thread_info(#{ '$type' := msacc_stats,
                     counters := Cnt } = Thread, #{ system := PrintSys }) ->
    [case maps:find(id, Thread) of
        error ->
            io_lib:format("~14s", [atom_to_list(maps:get(type, Thread))]);
        {ok, Id} ->
            io_lib:format("~10s(~2B)", [atom_to_list(maps:get(type,Thread)),Id])
    end,
    map(fun(_Key, #{ thread := ThreadPerc, system := SystemPerc }) when PrintSys ->
                io_lib:format("~6.2f%(~4.1f%)", [ThreadPerc, SystemPerc]);
           (_Key, #{ thread := ThreadPerc }) ->
                io_lib:format("~8.2f%", [ThreadPerc])
             end, Cnt),
    io_lib:format("~n",[])].

get_total(Cnt, Base) ->
    maps:fold(fun(_, {Val,_}, Time) ->
                      %% Have to handle ERTS_MSACC_STATE_COUNTERS
                      Time + Val;
                 (_, Val, Time) -> Time + Val
              end, Base, Cnt).

get_thread_perc(#{ '$type' := msacc_data, counters := Cnt } = Thread,
                SystemTime) ->
    ThreadTime = get_total(Cnt, 0),
    Thread#{ '$type' := msacc_stats,
             system => percentage(ThreadTime,SystemTime),
             counters => get_thread_perc(Cnt, ThreadTime, SystemTime)}.
get_thread_perc(Cnt, ThreadTime, SystemTime) ->
    maps:map(fun F(Key, {Val, C}) ->
                    M = F(Key, Val),
                    M#{ cnt => C };
                 F(_Key, Val) ->
                    #{ thread => percentage(Val, ThreadTime),
                       system => percentage(Val, SystemTime) }
            end, Cnt).

%% This code is a little bit messy as it has to be able to deal with
%% both [msacc_data()] and [msacc_stats()].
merge_threads([#{ '$type' := msacc_stats,
                  type := Type,
                  counters := Cnt } = M0|R], Acc) ->
    case keyfind(type, Type, Acc) of
        false ->
            merge_threads(R, [maps:remove(id,M0#{ threads => 1 })|Acc]);
        #{ '$type' := msacc_stats, counters := Cnt0,
           threads := Threads, system := System } = M ->
            NewMap = M#{ counters := add_counters(Cnt, Cnt0),
                         system := System + maps:get(system, M0),
                         threads := Threads + 1},
            NewAcc = keyreplace(type, Type, NewMap, Acc),
            merge_threads(R, NewAcc)
    end;
merge_threads([], [#{ '$type' := msacc_stats,
                      system := System,
                      threads := Threads,
                      counters := Cnt} = M0|R]) ->
    Counters = maps:map(fun(_,#{ thread := Thr } = Map) ->
                                Map#{ thread := Thr / Threads }
                        end, Cnt),
    M = maps:remove(threads, M0),
    [M#{ system := System, counters := Counters} | merge_threads([],R)];
merge_threads([], []) ->
    [];
%% The clauses below deal with msacc_data()
merge_threads([#{ '$type' := msacc_data,
                  type := Type,
                  counters := Cnt } = M0|R], Acc) ->
    case keyfind(type, Type, Acc) of
        false ->
            merge_threads(R, [maps:remove(id,M0)|Acc]);
        #{ '$type' := msacc_data, counters := Cnt0 } = M ->
            NewMap = M#{ counters := add_counters(Cnt, Cnt0) },
            NewAcc = keyreplace(type, Type, NewMap, Acc),
            merge_threads(R, NewAcc)
    end;
merge_threads([], Acc) ->
    Acc.

add_counters(M1, M2) ->
    maps:map(
      fun(Key, #{ thread := Thr1, system := Sys1, cnt := Cnt1}) ->
              %% Have to handle ERTS_MSACC_STATE_COUNTERS
              #{ thread := Thr2, system := Sys2, cnt := Cnt2} = maps:get(Key, M2),
              #{ thread => Thr1 + Thr2, system => Sys1 + Sys2,
                 cnt => Cnt1 + Cnt2 };
         (Key, #{ thread := Thr1, system := Sys1}) ->
              #{ thread := Thr2, system := Sys2} = maps:get(Key, M2),
              #{ thread => Thr1 + Thr2, system => Sys1 + Sys2};
         (Key, {V1,C1}) ->
              %% Have to handle ERTS_MSACC_STATE_COUNTERS
              {V2,C2} = maps:get(Key, M2),{V1+V2,C1+C2};
         (Key, V1) -> maps:get(Key, M2) + V1
      end, M1).

percentage(Divident, Divisor) ->
    if Divisor == 0 andalso Divident /= 0 ->
            100.0;
        Divisor == 0 ->
            0.0;
       true ->
            Divident / Divisor * 100
    end.

keyfind(Key, Value, [H|T]) ->
    case maps:find(Key, H) of
        {ok, Value} ->
            H;
        _ ->
            keyfind(Key, Value, T)
    end;
keyfind(_, _, []) ->
    false.

keyreplace(Key, Value, NewMap, [H|T]) ->
    case maps:find(Key, H) of
        {ok, Value} ->
            [NewMap|T];
        _ ->
            [H|keyreplace(Key, Value, NewMap, T)]
    end;
keyreplace(_, _, _, []) ->
    [].

statssort(Stats) ->
    lists:sort(fun(#{ type := Type1, id := Id1},
                   #{ type := Type2, id := Id2}) ->
                       {Type1, Id1} < {Type2, Id2};
                  (#{ type := Type1}, #{ type := Type2}) ->
                       Type1 < Type2
               end, Stats).

map(Fun,Map) ->
    [ Fun(K,V) || {K,V} <- maps:to_list(Map) ].
