%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(old_scheduler_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2]).
-export([equal/1, many_low/1, few_low/1, max/1, high/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 11}}].

all() -> 
    case catch erlang:system_info(modified_timing_level) of
        Level when is_integer(Level) ->
            {skipped,
             "Modified timing (level " ++
             integer_to_list(Level) ++
             ") is enabled. Testcases gets messed "
             "up by modfied timing."};
        _ -> [equal, many_low, few_low, max, high]
    end.


%%-----------------------------------------------------------------------------------
%% TEST SUITE DESCRIPTION
%%
%% The test case function spawns two controlling processes: Starter and Receiver.
%% Starter spawns a number of prio A and a number of prio B test processes. Each
%% test process loops for a number of times, sends a report to the Receiver, then
%% loops again. For each report, the Receiver increases a counter that corresponds
%% to the priority of the sender. After a certain amount of time, the Receiver
%% sends the collected data to the main test process and waits for the test case
%% to terminate. From this data, it's possible to calculate the average run time
%% relationship between the prio A and B test processes.
%%
%% Note that in order to be able to run tests with high or max prio test processes, 
%% the main test process and the Receiver needs to run at max prio, or they will
%% be starved by the test processes. The controlling processes must not wait for
%% messages from a normal (or low) prio process while max or high prio test processes
%% are running (which happens e.g. if an io function is called).
%%-----------------------------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    %% main test process needs max prio
    Prio = process_flag(priority, max),
    MS = erlang:system_flag(multi_scheduling, block),
    [{prio,Prio},{multi_scheduling, MS}|Config].

end_per_testcase(_Case, Config) ->
    erlang:system_flag(multi_scheduling, unblock),
    Prio=proplists:get_value(prio, Config),
    process_flag(priority, Prio),
    ok.

ok(Config) when is_list(Config) ->
    case proplists:get_value(multi_scheduling, Config) of
        blocked ->
            {comment,
             "Multi-scheduling blocked during test. This testcase was not "
             "written to work with multiple schedulers."};
        _ -> ok
    end.

%% Run equal number of low and normal prio processes.

equal(Config) when is_list(Config) ->
    Self = self(),

    %% specify number of test processes to run
    Normal = {normal,500},
    Low    = {low,500},

    %% specify time of test (in seconds)
    Time = 30,

    %% start controllers
    Receiver = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, Normal, Low) end),
    Starter =
    spawn(fun() -> starter(Normal, Low, Receiver) end),

    %% receive test data from Receiver
    {NRs,NAvg,LRs,LAvg,Ratio} = 
    receive
        {Receiver,Res} -> Res
    end,

    %% stop controllers and test processes
    exit(Starter, kill),
    exit(Receiver, kill),

    io:format("Reports: ~w normal (~w/proc), ~w low (~w/proc). Ratio: ~w~n", 
              [NRs,NAvg,LRs,LAvg,Ratio]),

    %% runtime ratio between normal and low should be ~8
    if Ratio < 7.5 ; Ratio > 8.5 ->	
           ct:fail({bad_ratio,Ratio});
       true ->
           ok(Config)
    end.


%% Run many low and few normal prio processes.

many_low(Config) when is_list(Config) ->
    Self = self(),
    Normal = {normal,1},
    Low    = {low,1000},

    %% specify time of test (in seconds)
    Time = 30,

    Receiver = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, Normal, Low) end),
    Starter =
    spawn(fun() -> starter(Normal, Low, Receiver) end),
    {NRs,NAvg,LRs,LAvg,Ratio} = 
    receive
        {Receiver,Res} -> Res
    end,
    exit(Starter, kill),
    exit(Receiver, kill),
    io:format("Reports: ~w normal (~w/proc), ~w low (~w/proc). Ratio: ~w~n", 
              [NRs,NAvg,LRs,LAvg,Ratio]),
    if Ratio < 7.5 ; Ratio > 8.5 ->
           ct:fail({bad_ratio,Ratio});
       true ->
           ok(Config)
    end.


%% Run few low and many normal prio processes.

few_low(Config) when is_list(Config) ->
    Self = self(),
    Normal = {normal,1000},
    Low    = {low,1},

    %% specify time of test (in seconds)
    Time = 30,

    Receiver = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, Normal, Low) end),
    Starter =
    spawn(fun() -> starter(Normal, Low, Receiver) end),
    {NRs,NAvg,LRs,LAvg,Ratio} = 
    receive
        {Receiver,Res} -> Res
    end,
    exit(Starter, kill),
    exit(Receiver, kill),
    io:format("Reports: ~w normal (~w/proc), ~w low (~w/proc). Ratio: ~w~n", 
              [NRs,NAvg,LRs,LAvg,Ratio]),
    if Ratio < 7.0 ; Ratio > 8.5 ->
           ct:fail({bad_ratio,Ratio});
       true ->
           ok(Config)
    end.


%% Run max prio processes and verify they get at least as much 
%% runtime as high, normal and low.

max(Config) when is_list(Config) ->
    max = process_flag(priority, max),		% should already be max (init_per_tc)
    Self = self(),
    Max    = {max,2},
    High   = {high,2},
    Normal = {normal,100},
    Low    = {low,100},

    %% specify time of test (in seconds)
    Time = 30,

    Receiver1 = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, Max, High) end),
    Starter1 =
    spawn(fun() -> starter(Max, High, Receiver1) end),
    {M1Rs,M1Avg,HRs,HAvg,Ratio1} = 
    receive
        {Receiver1,Res1} -> Res1
    end,
    exit(Starter1, kill),
    exit(Receiver1, kill),
    io:format("Reports: ~w max (~w/proc), ~w high (~w/proc). Ratio: ~w~n", 
              [M1Rs,M1Avg,HRs,HAvg,Ratio1]),
    if Ratio1 < 1.0 ->
           ct:fail({bad_ratio,Ratio1});
       true ->
           ok(Config)
    end,

    Receiver2 = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, Max, Normal) end),
    Starter2 =
    spawn(fun() -> starter(Max, Normal, Receiver2) end),
    {M2Rs,M2Avg,NRs,NAvg,Ratio2} = 
    receive
        {Receiver2,Res2} -> Res2
    end,
    exit(Starter2, kill),
    exit(Receiver2, kill),
    io:format("Reports: ~w max (~w/proc), ~w normal (~w/proc). Ratio: ~w~n", 
              [M2Rs,M2Avg,NRs,NAvg,Ratio2]),
    if Ratio2 < 1.0 ->
           ct:fail({bad_ratio,Ratio2});
       true ->
           ok
    end,

    Receiver3 = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, Max, Low) end),
    Starter3 =
    spawn(fun() -> starter(Max, Low, Receiver3) end),
    {M3Rs,M3Avg,LRs,LAvg,Ratio3} = 
    receive
        {Receiver3,Res3} -> Res3
    end,
    exit(Starter3, kill),
    exit(Receiver3, kill),
    io:format("Reports: ~w max (~w/proc), ~w low (~w/proc). Ratio: ~w~n", 
              [M3Rs,M3Avg,LRs,LAvg,Ratio3]),
    if Ratio3 < 1.0 ->
           ct:fail({bad_ratio,Ratio3});
       true ->
           ok(Config)
    end.


%% Run high prio processes and verify they get at least as much 
%% runtime as normal and low.

high(Config) when is_list(Config) ->
    max = process_flag(priority, max),		% should already be max (init_per_tc)
    Self = self(),
    High   = {high,2},
    Normal = {normal,100},
    Low    = {low,100},

    %% specify time of test (in seconds)
    Time = 30,

    Receiver1 = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, High, Normal) end),
    Starter1 =
    spawn(fun() -> starter(High, Normal, Receiver1) end),
    {H1Rs,H1Avg,NRs,NAvg,Ratio1} = 
    receive
        {Receiver1,Res1} -> Res1
    end,
    exit(Starter1, kill),
    exit(Receiver1, kill),
    io:format("Reports: ~w high (~w/proc), ~w normal (~w/proc). Ratio: ~w~n", 
              [H1Rs,H1Avg,NRs,NAvg,Ratio1]),
    if Ratio1 < 1.0 ->
           ct:fail({bad_ratio,Ratio1});
       true ->
           ok
    end,

    Receiver2 = 
    spawn(fun() -> receiver(erlang:monotonic_time(), Time, Self, High, Low) end),
    Starter2 =
    spawn(fun() -> starter(High, Low, Receiver2) end),
    {H2Rs,H2Avg,LRs,LAvg,Ratio2} = 
    receive
        {Receiver2,Res2} -> Res2
    end,
    exit(Starter2, kill),
    exit(Receiver2, kill),
    io:format("Reports: ~w high (~w/proc), ~w low (~w/proc). Ratio: ~w~n", 
              [H2Rs,H2Avg,LRs,LAvg,Ratio2]),
    if Ratio2 < 1.0 ->
           ct:fail({bad_ratio,Ratio2});
       true ->
           ok(Config)
    end.


%%-----------------------------------------------------------------------------------
%% Controller processes and help functions
%%-----------------------------------------------------------------------------------

receiver(T0, TimeSec, Main, {P1,P1N}, {P2,P2N}) ->
    %% prio should be max so that mailbox doesn't overflow
    process_flag(priority, max),
    receiver(T0, TimeSec*1000, Main, P1,P1N,0, P2,P2N,0, 100000).

%% uncomment lines below to get life sign (debug)
receiver(T0, Time, Main, P1,P1N,P1Rs, P2,P2N,P2Rs, 0) ->
    %    T = erlang:convert_time_unit(erlang:monotonic_time() - T0, native, milli_seconds),
    %    erlang:display({round(T/1000),P1Rs,P2Rs}),
    receiver(T0, Time, Main, P1,P1N,P1Rs, P2,P2N,P2Rs, 100000);

receiver(T0, Time, Main, P1,P1N,P1Rs, P2,P2N,P2Rs, C) ->
    Remain = Time - erlang:convert_time_unit(erlang:monotonic_time() - T0,
                                             native, milli_seconds), % test time remaining
    Remain1 = if Remain < 0 ->
                     0;
                 true ->
                     Remain
              end,
    {P1Rs1,P2Rs1} = 
    receive
        {_Pid,P1} ->			% report from a P1 process
            {P1Rs+1,P2Rs};
        {_Pid,P2} ->			% report from a P2 process
            {P1Rs,P2Rs+1}
    after Remain1 ->
              {P1Rs,P2Rs}
    end,
    if Remain > 0 ->				% keep going
           receiver(T0, Time, Main, P1,P1N,P1Rs1, P2,P2N,P2Rs1, C-1);
       true ->					% finish
           %% calculate results and send to main test process
           P1Avg = P1Rs1/P1N,
           P2Avg = P2Rs1/P2N,
           Ratio = if P2Avg < 1.0 -> P1Avg;
                      true -> P1Avg/P2Avg
                   end,
           Main ! {self(),{P1Rs1,round(P1Avg),P2Rs1,round(P2Avg),Ratio}},
           flush_loop()
    end.

starter({P1,P1N}, {P2,P2N}, Receiver) ->
    %% start N1 processes with prio P1
    start_p(P1, P1N, Receiver),
    %% start N2 processes with prio P2
    start_p(P2, P2N, Receiver),
    erlang:display({started,P1N+P2N}),
    flush_loop().

start_p(_, 0, _) ->
    ok;
start_p(Prio, N, Receiver) ->
    spawn_link(fun() -> p(Prio, Receiver) end),
    start_p(Prio, N-1, Receiver).

p(Prio, Receiver) ->
    %% set process priority
    process_flag(priority, Prio),
    p_loop(0, Prio, Receiver).

p_loop(100, Prio, Receiver) ->
    receive after 0 -> ok end,
    %% if Receiver gone, we're done
    case is_process_alive(Receiver) of
        false -> exit(bye);
        true -> ok
    end,
    %% send report
    Receiver ! {self(),Prio},
    p_loop(0, Prio, Receiver);

p_loop(N, Prio, Receiver) ->
    p_loop(N+1, Prio, Receiver).


flush_loop() ->
    receive _ ->
                ok
    end,
    flush_loop().
