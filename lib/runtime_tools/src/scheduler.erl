%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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

%% @doc Utility functions for easier measurement of scheduler utilization
%%      using erlang:statistics(scheduler_wall_time).

-module(scheduler).

-export([sample/0,
         sample_all/0,
         utilization/1,
         utilization/2]).

-export_type([sched_sample/0]).


-opaque sched_sample() ::
          {scheduler_wall_time | scheduler_wall_time_all,
           [{sched_type(), sched_id(), ActiveTime::integer(), TotalTime::integer()}]}.

-type sched_type() :: normal | cpu | io.

-type sched_id() :: integer().

-spec sample() -> sched_sample().
sample() ->
    sample(scheduler_wall_time).

-spec sample_all() -> sched_sample().
sample_all() ->
    sample(scheduler_wall_time_all).

sample(Stats) ->
    case erlang:statistics(Stats) of
        undefined ->
            erlang:system_flag(scheduler_wall_time, true),
            sample(Stats);
        
        List ->
            Sorted = lists:sort(List),
            Tagged = lists:map(fun({I, A, T}) -> {sched_tag(I), I, A, T} end,
                               Sorted),
            {Stats, Tagged}
    end.

-type sched_util_result() ::
        [{sched_type(), sched_id(), float(), string()} |
         {total, float(), string()} |
         {weighted, float(), string()}].

-spec utilization(Seconds) -> sched_util_result() when
      Seconds :: pos_integer();
                 (Sample) -> sched_util_result() when
      Sample :: sched_sample().
utilization(Seconds) when is_integer(Seconds), Seconds > 0 ->
    OldFlag = erlang:system_flag(scheduler_wall_time, true),
    T0 = sample(),
    receive after Seconds*1000 -> ok end,
    T1 = sample(),
    case OldFlag of
        false ->
            erlang:system_flag(scheduler_wall_time, OldFlag);
        true ->
            ok
    end,
    utilization(T0,T1);

utilization({Stats, _}=T0) when Stats =:= scheduler_wall_time;
                                Stats =:= scheduler_wall_time_all ->
    utilization(T0, sample(Stats)).

-spec utilization(Sample1, Sample2) -> sched_util_result() when
      Sample1 :: sched_sample(),
      Sample2 :: sched_sample().
utilization({Stats, Ts0}, {Stats, Ts1}) ->
    Diffs = lists:map(fun({{Tag, I, A0, T0}, {Tag, I, A1, T1}}) ->
                              {Tag, I, (A1 - A0), (T1 - T0)}
                      end,
                      lists:zip(Ts0,Ts1)),

    {Lst0, {A, T, N}} = lists:foldl(fun({Tag, I, Adiff, Tdiff}, {Lst, Acc}) ->
                                            R = safe_div(Adiff, Tdiff),
                                            {[{Tag, I, R, percent(R)} | Lst],
                                             acc(Tag, Adiff, Tdiff, Acc)}
                                    end,
                                    {[], {0, 0, 0}},
                               Diffs),

    Total = safe_div(A, T),
    Lst1 = lists:reverse(Lst0),
    Lst2 = case erlang:system_info(logical_processors_available) of
               unknown -> Lst1;
               LPA ->
                   Weighted = Total * (N / LPA),
                   [{weighted, Weighted, percent(Weighted)} | Lst1]
           end,
    [{total, Total, percent(Total)} | Lst2];

utilization({scheduler_wall_time, _}=T0,
            {scheduler_wall_time_all, Ts1}) ->
    utilization(T0, {scheduler_wall_time, remove_io(Ts1)});

utilization({scheduler_wall_time_all, Ts0},
            {scheduler_wall_time, _}=T1) ->
    utilization({scheduler_wall_time, remove_io(Ts0)}, T1).

%% Do not include dirty-io in totals
acc(io, _, _, Acc) ->
    Acc;
acc(Tag, Adiff, Tdiff, {Asum, Tsum, N}) when Tag =:= normal; Tag =:= cpu ->
    {Adiff+Asum, Tdiff+Tsum, N+1}.


remove_io(Ts) ->
    lists:filter(fun({io,_,_,_}) -> false;
                    (_) -> true end,
                 Ts).

safe_div(A, B) ->
    if B == 0.0 -> 0.0;
       true -> A / B
    end.            

sched_tag(Nr) ->
    Normal = erlang:system_info(schedulers),
    Cpu = Normal + erlang:system_info(dirty_cpu_schedulers),
    case Nr of
        _ when Nr =< Normal -> normal;
        _ when Nr =< Cpu -> cpu;
        _ -> io
    end.


percent(F) ->
    float_to_list(F*100, [{decimals,1}]) ++ [$%].
