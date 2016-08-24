%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(timer_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, init_per_group/2,end_per_group/2]).
-export([do_big_test/1]).
-export([big_test/1, collect/3, i_t/3, a_t/2]).
-export([do_nrev/1, internal_watchdog/2]).

-include_lib("common_test/include/ct.hrl").

%% Random test of the timer module. This is a really nasty test, as it
%% runs a lot of timeouts and then checks in the end if any of them
%% was triggered too early or if any late timeouts was much too late.
%%
%% Running time on average is about 90 seconds.

%% The main test case in this module is "do_big_test", which
%% orders a large number of timeouts and measures how
%% exact the timeouts arrives. To simulate a system under load there is
%% also a number of other concurrent processes running "nrev" at the same
%% time. The result is analyzed afterwards by trying to check if the
%% measured values are reasonable. It is hard to determine what is
%% reasonable on different machines; therefore the test can sometimes
%% fail, even though the timer module is ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,20}}].

all() -> 
    [do_big_test].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% ------------------------------------------------------- %%

do_big_test(TConfig) when is_list(TConfig) ->
    Save = process_flag(trap_exit, true),
    Result = big_test(200),
    process_flag(trap_exit, Save),
    report_result(Result).

report_result(ok) -> ok;
report_result(Error) -> ct:fail(Error).

%% ------------------------------------------------------- %%

big_test(N) ->
    C = start_collect(),
    system_time(), system_time(), system_time(),

    big_loop(C, N, []),

    %%C ! print_report,
    C ! {self(), get_report},
    Report = receive
		 {report, R} ->
		     R
	     end,
    C ! stop,
    receive
	{'EXIT', C, normal} ->
	    ok
    end,
    print_report(Report),
    Result = analyze_report(Report),
    %%io:format("big_test is done: ~w~n", [Result]),
    Result.

big_loop(_C, 0, []) ->
    %%io:format("All processes are done!~n", []),
    ok;
big_loop(C, 0, Pids) ->
    %%ok = io:format("Loop done, ~w processes remaining~n", [length(Pids)]),
    %% wait for remaining processes
    receive
	{'EXIT', Pid, done} ->
	    big_loop(C, 0, lists:delete(Pid, Pids));
	{'EXIT', Pid, Error} ->
	    ok = io:format("XXX Pid ~w died with reason ~p~n",
			   [Pid, Error]),
	    big_loop(C, 0, lists:delete(Pid, Pids))
    end;
big_loop(C, N, Pids) ->
    %% First reap any processes that are done.
    receive
	{'EXIT', Pid, done} ->
	    big_loop(C, N, lists:delete(Pid, Pids));
	{'EXIT', Pid, Error} ->
	    ok =io:format("XXX Internal error: Pid ~w died, reason ~p~n",
			  [Pid, Error]),
	    big_loop(C, N, lists:delete(Pid, Pids))
    after 0 ->

	    %% maybe start an interval timer test
	    Pids1 = maybe_start_i_test(Pids, C, rand:uniform(4)),

	    %% start 1-4 "after" tests
	    Pids2 = start_after_test(Pids1, C, rand:uniform(4)),
	    %%Pids2=Pids1,

	    %% wait a little while
	    timer:sleep(rand:uniform(200)*3),

	    %% spawn zero, one or two nrev to get some load ;-/
	    Pids3 = start_nrev(Pids2, rand:uniform(100)),

	    big_loop(C, N-1, Pids3)
    end.


start_nrev(Pids, N) when N < 25 ->
    Pids;
start_nrev(Pids, N) when N < 75 ->
    [spawn_link(timer_SUITE, do_nrev, [1])|Pids];
start_nrev(Pids, _N) ->
    NrevPid1 = spawn_link(timer_SUITE, do_nrev, [rand:uniform(1000)*10]),
    NrevPid2 = spawn_link(timer_SUITE, do_nrev, [1]),
    [NrevPid1,NrevPid2|Pids].


start_after_test(Pids, C, 1) ->
    TO1 = rand:uniform(100)*47,
    [s_a_t(C, TO1)|Pids];
start_after_test(Pids, C, 2) ->
    TO1 = rand:uniform(100)*47,
    TO2 = TO1 div rand:uniform(3) + 101,
    [s_a_t(C, TO1),s_a_t(C, TO2)|Pids];
start_after_test(Pids, C, N) ->
    TO1 = rand:uniform(100)*47,
    start_after_test([s_a_t(C, TO1)|Pids], C, N-1).

s_a_t(C, TimeOut) ->
    spawn_link(timer_SUITE, a_t, [C, TimeOut]).

a_t(C, TimeOut) ->
    start_watchdog(self(), TimeOut),
    Start = system_time(),
    timer:send_after(TimeOut, self(), now),
    receive
	now ->
	    Stop = system_time(),
	    report(C, Start,Stop,TimeOut),
	    exit(done);
	watchdog ->
	    Stop = system_time(),
	    report(C, Start,Stop,TimeOut),
	    ok = io:format("Internal watchdog timeout (a), not good!!~n",
			   []),
	    exit(done)
    end.


maybe_start_i_test(Pids, C, 1) ->
    %% ok do it
    TOI = rand:uniform(53)*49,
    CountI = rand:uniform(10) + 3,		% at least 4 times
    [spawn_link(timer_SUITE, i_t, [C, TOI, CountI])|Pids];
maybe_start_i_test(Pids, _C, _) ->
    Pids.

i_t(C, TimeOut, Times) ->
    start_watchdog(self(), TimeOut*Times),
    Start = system_time(),
    {ok, Ref} = timer:send_interval(TimeOut, interval),
    i_wait(Start, Start, 1, TimeOut, Times, Ref, C).

i_wait(Start, Prev, Times, TimeOut, Times, Ref, C) ->
    receive
	interval ->
	    Now = system_time(),
	    report_interval(C, {final,Times}, Start, Prev, Now, TimeOut),
	    timer:cancel(Ref),
	    exit(done);
	watchdog ->
	    Now = system_time(),
	    report_interval(C, {final,Times}, Start, Prev, Now, TimeOut),
	    timer:cancel(Ref),
	    ok = io:format("Internal watchdog timeout (i), not good!!~n",
			   []),
	    exit(done)
    end;
i_wait(Start, Prev, Count, TimeOut, Times, Ref, C) ->
    receive
	interval ->
	    Now = system_time(),
	    report_interval(C, Count, Start, Prev, Now, TimeOut),
	    i_wait(Start, Now, Count+1, TimeOut, Times, Ref, C);
	watchdog ->
	    Now = system_time(),
	    report_interval(C, {final,Count}, Start, Prev, Now, TimeOut),
	    ok = io:format("Internal watchdog timeout (j), not good!!~n",
			   []),
	    exit(done)
    end.

report(C, Start, Stop, Time) ->
    C ! {a_sample, Start, Stop, Time}.
report_interval(C, Count, Start, Prev, Now, TimeOut) ->
    C ! {i_sample, Count, Start, Prev, Now, TimeOut}.

%% ------------------------------------------------------- %%

%% internal watchdog
start_watchdog(Pid, TimeOut) ->
    spawn_link(timer_SUITE, internal_watchdog, [Pid, 3*TimeOut+1000]).

internal_watchdog(Pid, TimeOut) ->
    receive
    after TimeOut ->
	    Pid ! watchdog,
	    exit(normal)
    end.

%% ------------------------------------------------------- %%

-record(stat, {n=0,max=0,min=min,avg=0}).

start_collect() ->
    spawn_link(timer_SUITE, collect, [0,{0,new_update(),new_update()},[]]).

collect(N, {E,A,B}, I) ->
    receive
	{a_sample, Start, Stop, Time} when Stop - Start > Time ->
	    collect(N+1, {E,update(Stop-Start-Time,A),B}, I);
	{a_sample, Start, Stop, Time} when Stop - Start < Time ->
	    collect(N+1, {E,A,update(Time-Stop+Start,B)}, I);
	{a_sample, _Start, _Stop, _Time} ->
	    collect(N+1, {E+1,A,B}, I);
	{i_sample, {final,Count}, Start, Prev, Now, TimeOut} ->
	    IntervDiff = Now - Prev - TimeOut,
	    Drift = Now - (Count*TimeOut) - Start,
	    collect(N, {E,A,B}, [{{final,Count},IntervDiff,Drift}|I]);
	{i_sample, Count, Start, Prev, Now, TimeOut} ->
	    IntervDiff = Now - Prev - TimeOut,
	    Drift = Now - (Count*TimeOut) - Start,
	    collect(N, {E,A,B}, [{Count,IntervDiff,Drift}|I]);
	print_report ->
	    print_report({E,A,B,I}),
	    collect(N,{E,A,B}, I);
	{Pid, get_report} when is_pid(Pid) ->
	    Pid ! {report, {E, A, B, I}},
	    collect(N,{E,A,B}, I);
	reset ->
	    collect(0, {0,new_update(),new_update()}, []);
	stop ->
	    exit(normal);
	_Other ->
	    collect(N, {E,A,B}, I)
    end.

new_update() -> #stat{}.
update(New, Stat) when New > Stat#stat.max ->
    Stat#stat{n=Stat#stat.n + 1, max=New, avg=(New+Stat#stat.avg) div 2};
update(New, Stat) when New < Stat#stat.min ->
    Stat#stat{n=Stat#stat.n + 1, min=New, avg=(New+Stat#stat.avg) div 2};
update(New, Stat) ->
    Stat#stat{n=Stat#stat.n + 1, avg=(New+Stat#stat.avg) div 2}.

print_report({E,LateS,EarlyS,I}) ->
    Early = EarlyS#stat.n, Late = LateS#stat.n,
    Total = E + Early + Late,
    io:format("~nOn total of ~w timeouts, there were ~w exact, ~w "
	      "late and ~w early.~n", [Total, E, Late, Early]),
    io:format("Late stats (N,Max,Min,Avg): ~w~nEarly stats: ~w~n",
	      [LateS, EarlyS]),
    IntervS = collect_interval_final_stats(I),
    io:format("Interval stats (Max,Min,Avg): ~w~n", [IntervS]),
    ok.

collect_interval_final_stats(I) ->
    collect_interval_final_stats(I, 0, min, 0).
collect_interval_final_stats([], Max, Min, Avg) ->
    {Max, Min, Avg};
collect_interval_final_stats([{{final,_Count},_,Dev}|T], Max, Min, Avg) ->
    NMax = if Dev>Max -> Dev; true -> Max end,
    NMin = if Dev<Min -> Dev; true -> Min end,
    collect_interval_final_stats(T, NMax, NMin, (Dev+Avg) div 2);
collect_interval_final_stats([_|T], Max, Min, Avg) ->
    collect_interval_final_stats(T, Max, Min, Avg).

analyze_report({E,LateS,EarlyS,I}) ->
    Early = EarlyS#stat.n, Late = LateS#stat.n,
    IntervS = collect_interval_final_stats(I),
    Res1 = min_and_early_check(E, Early, Late, element(2,IntervS)),
    Res2 = abnormal_max_check(LateS#stat.max, element(1,IntervS)),
    res_combine(ok, [Res1, Res2]).

-define(ok_i_min, -100).
-define(ok_max, 8000).
-define(ok_i_max, 4000).

%% ok as long as Early == 0 and IntervMin >= ok_interv_min
min_and_early_check(_Exact, 0, _Late, IntervMin) when IntervMin >= ?ok_i_min ->
    ok;
min_and_early_check(_Exact, Early, _Late, IntervMin) when IntervMin >= ?ok_i_min ->
    {error, {early_timeouts, Early}};
min_and_early_check(_Exact, 0, _Late, _IntervMin) ->
    {error, early_interval_timeout};
min_and_early_check(_Exact, Early, _Late, _IntervMin) ->
    {error, [{early_timeouts, Early},{error, early_interval_timeout}]}.

abnormal_max_check(LateMax, IntMax) when LateMax < ?ok_max,
                                         IntMax < ?ok_i_max ->
    ok;
abnormal_max_check(LateMax, IntMax) when IntMax < ?ok_i_max ->
    {error, {big_late_max, LateMax}};
abnormal_max_check(LateMax, IntMax) when LateMax < ?ok_max ->
    {error, {big_interval_max, IntMax}};
abnormal_max_check(LateMax, IntMax) ->
    {error, [{big_late_max, LateMax},{big_interval_max, IntMax}]}.

res_combine(Res, []) ->
    Res;
res_combine(Res, [ok|T]) ->
    res_combine(Res, T);
res_combine(ok, [{error,What}|T]) ->
    res_combine({error,What}, T);
res_combine({error,Es}, [{error,E}|T]) ->
    res_combine({error,lists:flatten([E,Es])}, T).


system_time() ->
    erlang:monotonic_time(millisecond).

%% ------------------------------------------------------- %%

do_nrev(Sleep) ->
    timer:sleep(Sleep),
    test(1000,"abcdefghijklmnopqrstuvxyz1234"),
    exit(done).

test(0,_) ->
    true;
test(N,L) ->
    nrev(L),
    test(N - 1, L).

nrev([]) ->
    [];
nrev([H|T]) ->
    append(nrev(T), [H]).

append([H|T],Z) ->
    [H|append(T,Z)];
append([],X) ->
    X.

%% ------------------------------------------------------- %%
