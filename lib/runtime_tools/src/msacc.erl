%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
-export([available/0, start/0, start/1, stop/0, reset/0, to_file/1,
         from_file/1, stats/0, stats/2, print/0, print/1, print/2,
         print/3]).

-type msacc_data() :: [msacc_data_thread()].

-type msacc_data_thread() :: #{ '$type' := msacc_data,
                                type := msacc_type(), id := msacc_id(),
                                counters := msacc_data_counters() }.
-type msacc_data_counters() :: #{ msacc_state() => non_neg_integer()}.

-type msacc_stats() :: [msacc_stats_thread()].
-type msacc_stats_thread() :: #{ '$type' := msacc_stats,
                                 type := msacc_type(), id := msacc_id(),
                                 system := float(),
                                 counters := msacc_stats_counters()}.
-type msacc_stats_counters() :: #{ msacc_state() => #{ thread := float(),
                                                       system := float()}}.


-type msacc_type() :: scheduler | aux | async.
-type msacc_id() :: non_neg_integer().
-type msacc_state() :: alloc | aux | bif | busy_wait | check_io |
                       emulator | ets | gc | gc_fullsweep | nif |
                       other | port | send | sleep | timers.

-type msacc_print_options() :: #{ system => boolean() }.

-spec available() -> boolean().
available() ->
    try
        [_|_] = erlang:statistics(microstate_accounting),
        true
    catch _:_ ->
            false
    end.

-spec start() -> boolean().
start() ->
    erlang:system_flag(microstate_accounting, true).

-spec stop() -> boolean().
stop() ->
    erlang:system_flag(microstate_accounting, false).

-spec reset() -> boolean().
reset() ->
    erlang:system_flag(microstate_accounting, reset).

-spec start(Time) -> true when
      Time :: timeout().
start(Tmo) ->
    stop(), reset(), start(),
    timer:sleep(Tmo),
    stop().

-spec to_file(Filename) -> ok | {error, file:posix()} when
      Filename :: file:name_all().
to_file(Filename) ->
    file:write_file(Filename, io_lib:format("~p.~n",[stats()])).

-spec from_file(Filename) -> msacc_data() when
      Filename :: file:name_all().
from_file(Filename) ->
    {ok, [Stats]} = file:consult(Filename),
    Stats.

-spec print() -> ok.
print() ->
    print(stats()).

-spec print(DataOrStats) -> ok when
      DataOrStats :: msacc_data() | msacc_stats().
print(Stats) ->
    print(Stats, #{}).

-spec print(DataOrStats, Options) -> ok when
      DataOrStats :: msacc_data() | msacc_stats(),
      Options :: msacc_print_options().
print(Stats, Options) ->
    print(group_leader(), Stats, Options).

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
