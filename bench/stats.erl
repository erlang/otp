-module(stats).

-export([test_avg/3]).

-include("stats.hrl").

%%================================================================
%% Timing functions
%%================================================================

%% @doc Use timer:tc/2
regression_timer_1(Fun, Args) ->
    {Time, Result} = timer:tc(Fun, Args),
    {round(Time/1000), Result}. % Return millisecs.

%% @doc Use erlang:now/0
regression_timer_2(Fun, Args) ->
    Start = now(),
    Result = apply(Fun, Args),
    Stop = now(),
    Time = timer:now_diff(Stop, Start),
    {round(Time/1000), Result}. % Return millisecs.

%% @doc Use erlang:statistics(wall_clock)
regression_timer_3(Fun, Args) ->
    {_Start, _} = erlang:statistics(wall_clock),
    Result = apply(Fun, Args),
    {_Stop, Time_SLC_2} = erlang:statistics(wall_clock),
    {Time_SLC_2, Result}.

%% @doc Use os:timestamp/0
regression_timer_4(Fun, Args) ->
    {_, _, Start} = os:timestamp(),
    Result = apply(Fun, Args),
    {_, _, Stop} = os:timestamp(),
    {Stop - Start, Result}.

%% @doc Use erlang:statistics(runtime)
regression_timer_5(Fun, Args) ->
    {_Start, _} = erlang:statistics(runtime),
    Result = apply(Fun, Args),
    {_Stop, Time_SLC_2} = erlang:statistics(runtime),
    {Time_SLC_2, Result}.

%% @doc Driver function
regression(Timer, Fun, Args) ->
    Myself = self(),
    Opts = [], %[{min_heap_size, 100000000}],
    %%XXX: Spawns a new timing process (=> clean heap)!
    spawn_opt(fun () -> Myself ! Timer(Fun, Args) end, Opts),
    receive
        Result -> Result
    end.

%%================================================================
%% Testers
%%================================================================

%% @doc A function that measures execution time more elaborately!
test_avg(Fun, Args, N) when N > 0 ->
    Timer = fun regression_timer_1/2,
    L = test_loop(Fun, Args, Timer, N, []),
    Length = length(L),
    S = #stat{range = #range{min = lists:min(L),
                             max = lists:max(L)},
              median = lists:nth(round(Length / 2), lists:sort(L)),
              average = avg(L),
              stddev = std_dev(L)},
    S.

%% @doc Execute test N times and collect results (execution times)
test_loop(_Fun, _Args, _Timer, 0, List) ->
    List;
test_loop(Fun, Args, Timer, N, List) ->
    {T, _Result} = regression(Timer, Fun, Args),
    test_loop(Fun, Args, Timer, N - 1, [T/1000|List]). %XXX: In milliseconds.

%%================================================================
%% Misc. functions
%%================================================================

pp_stat(#stat{range   = #range{min=Min,max=Max},
              median  = Med,
              average = Avg,
              stddev  = Stddev}) ->
    io:format("Range: ~b - ~b msecs~n"
              "Median: ~b msecs~n"
              "Average: ~b msecs~n"
              "Standard deviation: ~b msecs~n",
              [Min, Max, Med, Avg, Stddev]).

avg(L) ->
    lists:sum(L) / length(L).

std_dev(Values) ->
    L = length(Values),
    case L =:= 1 of
        true -> 0.0; % Executed only once (no deviation).
        false ->
            Avg = avg(Values),
            Sums = lists:foldl(
                     fun(V, Acc) -> D = V - Avg, Acc + (D * D) end, 0, Values),
            math:sqrt(Sums / (L - 1))
    end.
