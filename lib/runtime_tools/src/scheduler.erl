%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
-moduledoc """
Measure scheduler utilization

This module contains utility functions for easy measurement and calculation of
scheduler utilization. It act as a wrapper around the more primitive API
[`erlang:statistics(scheduler_wall_time)`](`m:erlang#statistics_scheduler_wall_time`).

The simplest usage is to call the blocking
[`scheduler:utilization(Seconds)`](`utilization/1`).

For non blocking and/or continuous calculation of scheduler utilization, the
recommended usage is:

- First call
  [`erlang:system_flag(scheduler_wall_time,true)`](`m:erlang#system_flag_scheduler_wall_time`)
  to enable scheduler wall time measurements.
- Call `get_sample/0` to collect samples with some time in between.
- Call `utilization/2` to calculate the scheduler utilization in the interval
  between two samples.
- When done call
  [`erlang:system_flag(scheduler_wall_time,false)`](`m:erlang#system_flag_scheduler_wall_time`)
  to disable scheduler wall time measurements and avoid unecessary cpu overhead.

To get correct values from `utilization/2`, it is important that
`scheduler_wall_time` is kept enabled during the entire interval between the two
samples. To ensure this, the process that called
[`erlang:system_flag(scheduler_wall_time,true)`](`m:erlang#system_flag_scheduler_wall_time`)
must be kept alive, as `scheduler_wall_time` will automatically be disabled if
it terminates.
""".
-moduledoc(#{since => "OTP 21.0"}).

-export([sample/0, get_sample/0,
         sample_all/0, get_sample_all/0,
         utilization/1,
         utilization/2]).

-export_type([sched_sample/0]).


-opaque sched_sample() ::
          {scheduler_wall_time | scheduler_wall_time_all,
           [{sched_type(), sched_id(), ActiveTime::integer(), TotalTime::integer()}]}.

-type sched_type() :: normal | cpu | io.

-type sched_id() :: integer().

-doc """
Return a scheduler utilization sample for normal and dirty-cpu schedulers. Will
call
[`erlang:system_flag(scheduler_wall_time,true)`](`m:erlang#system_flag_scheduler_wall_time`)
first if not already already enabled.

> #### Note {: .info }
>
> This function is _not recommended_ as there is no way to detect if
> `scheduler_wall_time` already was enabled or not. If `scheduler_wall_time` has
> been disabled between two samples, passing them to
> [`utilization/2`](`utilization/1`) will yield invalid results.
>
> Instead use `get_sample/0` together with
> [`erlang:system_flag(scheduler_wall_time,_)`](`m:erlang#system_flag_scheduler_wall_time`).
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec sample() -> sched_sample().
sample() ->
    sample(scheduler_wall_time).

-doc """
Return a scheduler utilization sample for all schedulers, including dirty-io
schedulers. Will call
[`erlang:system_flag(scheduler_wall_time,true)`](`m:erlang#system_flag_scheduler_wall_time`)
first if not already already enabled.

> #### Note {: .info }
>
> This function is _not recommended_ for same reason as `sample/0`. Instead use
> `get_sample_all/0` together with
> [`erlang:system_flag(scheduler_wall_time,_)`](`m:erlang#system_flag_scheduler_wall_time`).
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec sample_all() -> sched_sample().
sample_all() ->
    sample(scheduler_wall_time_all).

sample(Stats) ->
    case erlang:statistics(Stats) of
        undefined ->
            erlang:system_flag(scheduler_wall_time, true),
            sample(Stats);
        
        List ->
            create_sample(Stats, List)
    end.

-doc """
Returns a scheduler utilization sample for normal and dirty-cpu schedulers.
Returns `undefined` if system flag
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`) has not been
enabled.
""".
-doc(#{since => <<"OTP 24.3">>}).
-spec get_sample() -> sched_sample() | undefined.
get_sample() ->
    get_sample(scheduler_wall_time).

-doc """
Return a scheduler utilization sample for all schedulers, including dirty-io
schedulers. Returns `undefined` if system flag
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`) has not been
enabled.
""".
-doc(#{since => <<"OTP 24.3">>}).
-spec get_sample_all() -> sched_sample() | undefined.
get_sample_all() ->
    get_sample(scheduler_wall_time_all).

get_sample(Stats) ->
    case erlang:statistics(Stats) of
        undefined ->
            undefined;
        List ->
            create_sample(Stats, List)
    end.

create_sample(Stats, List) ->
    Sorted = lists:sort(List),
    Tagged = lists:map(fun({I, A, T}) -> {sched_tag(I), I, A, T} end,
                       Sorted),
    {Stats, Tagged}.


-doc """
A list of tuples containing results for individual schedulers as well as
aggregated averages. `Util` is the scheduler utilization as a floating point
value between 0.0 and 1.0. `Percent` is the same utilization as a more human
readable string expressed in percent.

- **`{normal, SchedulerId, Util, Percent}`** - Scheduler utilization of a normal
  scheduler with number `SchedulerId`. Schedulers that are not online will also
  be included. [Online schedulers](`m:erlang#system_info_schedulers_online`)
  have the lowest `SchedulerId`.

- **`{cpu, SchedulerId, Util, Percent}`** - Scheduler utilization of a dirty-cpu
  scheduler with number `SchedulerId`.

- **`{io, SchedulerId, Util, Percent}`** - Scheduler utilization of a dirty-io
  scheduler with number `SchedulerId`. This tuple will only exist if both
  samples were taken with `sample_all/0`.

- **`{total, Util, Percent}`** - Total utilization of all normal and dirty-cpu
  schedulers.

- **`{weighted, Util, Percent}`** - Total utilization of all normal and
  dirty-cpu schedulers, weighted against maximum amount of available CPU time.
""".
-type sched_util_result() ::
        [{sched_type(), sched_id(), float(), string()} |
         {total, float(), string()} |
         {weighted, float(), string()}].

-doc """
Measure utilization for normal and dirty-cpu schedulers during `Seconds`
seconds, and then return the result.

Will automatically first enable and then disable
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`).

Calculate scheduler utilizations for the time interval from when `Sample` was
taken and "now". The same as calling
`scheduler:utilization(Sample, scheduler:sample_all())`.

> #### Note {: .info }
>
> This function is _not recommended_ as it's so easy to get invalid results
> without noticing. In particular do not do this:
>
> ```erlang
> scheduler:utilization(scheduler:sample()). % DO NOT DO THIS!
> ```
>
> The above example takes two samples in rapid succession and calculates the
> scheduler utilization between them. The resulting values will probably be more
> misleading than informative.
>
> Instead use [`scheduler:utilization/2`](`utilization/2`) and call
> `get_sample/0` to get samples with some time in between.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec utilization(Seconds) -> sched_util_result() when
      Seconds :: pos_integer();
                 (Sample) -> sched_util_result() when
      Sample :: sched_sample().
utilization(Seconds) when is_integer(Seconds), Seconds > 0 ->
    _ = erlang:system_flag(scheduler_wall_time, true),
    T0 = sample(),
    receive after Seconds*1000 -> ok end,
    T1 = sample(),
    _ = erlang:system_flag(scheduler_wall_time, false),
    utilization(T0,T1);

utilization({Stats, _}=T0) when Stats =:= scheduler_wall_time;
                                Stats =:= scheduler_wall_time_all ->
    utilization(T0, sample(Stats)).

-doc """
Calculates scheduler utilizations for the time interval between the two samples
obtained from calling [`get_sample/0`](`sample/0`) or
[`get_sample_all/0`](`sample_all/0`).

This function itself, does not need
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`) to be
enabled. However, for a correct result, `scheduler_wall_time` must have been
enabled during the entire interval between the two samples.
""".
-doc(#{since => <<"OTP 21.0">>}).
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
