%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%% Purpose : Test the timer module a simpler/faster test than timer_SUITE

-module(timer_simple_SUITE).

%% external
-export([all/1, 
	 init_per_testcase/2,
	 apply_after/1,
	 send_after1/1,
	 send_after2/1,
	 send_after3/1,
	 exit_after1/1,
	 exit_after2/1,
	 kill_after1/1,
	 kill_after2/1,
	 apply_interval/1,
	 send_interval1/1,
	 send_interval2/1,
	 send_interval3/1,
	 send_interval4/1,
	 cancel1/1,
	 cancel2/1,
	 tc/1,
	 unique_refs/1,
	 timer_perf/1]).

%% internal
-export([forever/0,
	 do_nrev/2,
	 send/2,
	 timer/4,
	 timer/5]).

-include("test_server.hrl").

-define(MAXREF, (1 bsl 18)).
-define(REFMARG, 30).

all(doc) -> "Test of the timer module.";
all(suite) -> 
    [apply_after,
     send_after1,
     send_after2,
     send_after3,
     exit_after1,
     exit_after2,
     kill_after1,
     kill_after2,
     apply_interval,
     send_interval1,
     send_interval2,
     send_interval3,
     send_interval4,
     cancel1,
     cancel2,
     tc,
     unique_refs,
     timer_perf].

init_per_testcase(_, Config) when is_list(Config) ->
    timer:start(),
    Config.

%% Testing timer interface!!

apply_after(doc) -> "Test of apply_after, with sending of message.";
apply_after(suite) -> [];
apply_after(Config) when is_list(Config) ->
    ?line timer:apply_after(500, ?MODULE, send, [self(), ok_apply]),
    ?line ok = get_mess(1000, ok_apply).

send_after1(doc) -> "Test of send_after with time = 0.";
send_after1(suite) -> [];
send_after1(Config) when is_list(Config) ->
    ?line timer:send_after(0, ok_send1),
    ?line ok = get_mess(1000, ok_send1).

send_after2(doc) -> "Test of send_after with time = 500.";
send_after2(suite) -> [];
send_after2(Config) when is_list(Config) ->
    ?line timer:send_after(500, self(), ok_send2),
    ?line ok = get_mess(2000, ok_send2).

send_after3(doc) -> "Test of send_after with time = 500, with receiver "
			"a registered process. [OTP-2735]";
send_after3(suite) -> [];
send_after3(Config) when is_list(Config) ->
    ?line Name = list_to_atom(pid_to_list(self())),
    ?line register(Name, self()),
    ?line timer:send_after(500, Name, ok_send3),
    ?line ok = get_mess(2000, ok_send3),
    ?line unregister(Name).

exit_after1(doc) -> "Test of exit_after with time = 1000.";
exit_after1(suite) -> [];
exit_after1(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Pid = spawn_link(?MODULE, forever, []),
    ?line timer:exit_after(1000, Pid, exit_test1),
    ?line ok = get_mess(5000, {'EXIT', Pid, exit_test1}).

exit_after2(doc) -> "Test of exit_after with time = 1000. The process to "
			"exit is the name of a registered process. "
			"[OTP-2735]";
exit_after2(suite) -> [];
exit_after2(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Pid = spawn_link(?MODULE, forever, []),
    ?line Name = list_to_atom(pid_to_list(Pid)),
    ?line register(Name, Pid),
    ?line timer:exit_after(1000, Name, exit_test2),
    ?line ok = get_mess(2000, {'EXIT', Pid, exit_test2}).

kill_after1(doc) -> "Test of kill_after with time = 1000.";
kill_after1(suite) -> [];
kill_after1(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Pid = spawn_link(?MODULE, forever, []),
    ?line timer:kill_after(1000, Pid),
    ?line ok = get_mess(2000, {'EXIT', Pid, killed}).

kill_after2(doc) -> "Test of kill_after with time = 1000. The process to "
			"exit is the name of a registered process. "
			"[OTP-2735]";
kill_after2(suite) -> [];
kill_after2(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Pid = spawn_link(?MODULE, forever, []),
    ?line Name = list_to_atom(pid_to_list(Pid)),
    ?line register(Name, Pid),
    ?line timer:kill_after(1000, Name),
    ?line ok = get_mess(2000, {'EXIT', Pid, killed}).

apply_interval(doc) -> "Test of apply_interval by sending messages. Receive "
                       "3 messages, cancel the timer, and check that we do "
		       "not get any more messages.";
apply_interval(suite) -> [];
apply_interval(Config) when is_list(Config) ->
    ?line {ok, Ref} = timer:apply_interval(1000, ?MODULE, send, 
				     [self(), apply_int]),
    ?line ok = get_mess(1500, apply_int, 3),
    ?line timer:cancel(Ref),
    ?line nor = get_mess(1000, apply_int).

send_interval1(doc) -> "Test of send_interval/2. Receive 5 messages, cancel "
		      "the timer, and check that we do not get any more "
                      "messages.";
send_interval1(suite) -> [];
send_interval1(Config) when is_list(Config) ->
    {ok, Ref} = timer:send_interval(1000, send_int),
    ?line ok = get_mess(1500, send_int, 5),
    timer:cancel(Ref),
    ?line nor = get_mess(1000, send_int). % We should receive only five

send_interval2(doc) -> "Test of send_interval/3. Receive 2 messages, cancel "
		      "the timer, and check that we do not get any more "
                      "messages.";
send_interval2(suite) -> [];
send_interval2(Config) when is_list(Config) ->
    {ok, Ref} = timer:send_interval(1000, self(), send_int2),
    ?line ok = get_mess(1500, send_int2, 2),
    timer:cancel(Ref),
    ?line nor = get_mess(1000, send_int2).  % We should receive only two

send_interval3(doc) -> "Test of send_interval/3. Receive 2 messages, cancel "
		      "the timer, and check that we do not get any more "
                      "messages. The receiver is the name of a registered "
			   "process. [OTP-2735]";
send_interval3(suite) -> [];
send_interval3(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Name = list_to_atom(pid_to_list(self())),
    ?line register(Name, self()),
    ?line {ok, Ref} = timer:send_interval(1000, Name, send_int3),
    ?line ok = get_mess(1500, send_int3, 2),
    timer:cancel(Ref),
    ?line nor = get_mess(1000, send_int3),  % We should receive only two
    ?line unregister(Name).

send_interval4(doc) -> "Test that send interval stops sending msg when the "
			   "receiving process terminates.";
send_interval4(suite) -> [];
send_interval4(Config) when is_list(Config) ->
    ?line timer:send_interval(500, one_time_only),
    receive 
	one_time_only -> ok
    end,
    ?line timer_server ! {'EXIT', self(), normal}, % Should remove the timer
    ?line timer:send_after(600, send_intv_ok),
    ?line send_intv_ok = receive 
			     Msg -> Msg
			 end.

cancel1(doc) -> "Test that we can cancel a timer.";
cancel1(suite) -> [];
cancel1(Config) when is_list(Config) ->
    ?line {ok, Ref} = timer:send_after(1000, this_should_be_canceled),
    ?line timer:cancel(Ref),
    ?line nor = get_mess(2000, this_should_be_canceled). % We should rec 0 msgs

cancel2(doc) -> "Test cancel/1 with bad argument.";
cancel2(suite) -> [];
cancel2(Config) when is_list(Config) ->
    ?line {error, badarg} = timer:cancel(no_reference). 

tc(doc) -> "Test sleep/1 and tc/3.";
tc(suite) -> [];
tc(Config) when is_list(Config) ->
    % This should both sleep and tc/3
    ?line {Res1, ok} = timer:tc(timer, sleep, [500]),
    ?line ok = 	if
		    Res1 < 500*1000 -> {too_early, Res1}; % Too early
		    Res1 > 800*1000 -> {too_late, Res1};  % Too much time
		    true -> ok
		end,

    % This should both sleep and tc/2
    ?line {Res2, ok} = timer:tc(fun(T) -> timer:sleep(T) end, [500]),
    ?line ok = 	if
		    Res2 < 500*1000 -> {too_early, Res2}; % Too early
		    Res2 > 800*1000 -> {too_late, Res2};  % Too much time
		    true -> ok
		end,

    ?line Sec = timer:seconds(4),
    ?line Min = timer:minutes(4),
    ?line Hour = timer:hours(4),
    ?line MyRes = 4*1000 + 4*60*1000 + 4*60*60*1000,
    ?line if  MyRes == Sec + Min + Hour -> ok end,
    ?line TimerRes = timer:hms(4,4,4),
    ?line if MyRes == TimerRes -> ok end,
    ok.

unique_refs(doc) ->
    "Tests that cancellations of one-shot timers do not accidentally "
	"cancel interval timers [OTP-2771].";
unique_refs(suite) ->
    [];
unique_refs(Config) when is_list(Config) ->
    ?line ITimers = repeat_send_interval(10),		% 10 interval timers
    ?line eat_refs(?MAXREF - ?REFMARG),
    ?line set_and_cancel_one_shots(?REFMARG),
    ?line NumLeft = num_timers(),
    ?line io:format("~w timers left, should be 10\n", [NumLeft]),
    ?line cancel(ITimers),
    ?line receive_nisse(),
    ?line 10 = NumLeft.


repeat_send_interval(0) ->
    [];
repeat_send_interval(M) ->
    ?line {ok, Ref} = timer:send_interval(6000,self(), nisse),
    ?line [Ref| repeat_send_interval(M - 1)].

eat_refs(0) ->
    0;
eat_refs(N) ->
    _ = make_ref(),
    eat_refs(N-1).

set_and_cancel_one_shots(0) ->
    0;
set_and_cancel_one_shots(N) ->
    {ok, Ref} = timer:send_after(7000, self(), kalle),
    %% Cancel twice
    timer:cancel(Ref),			
    timer:cancel(Ref),
    set_and_cancel_one_shots(N-1).

cancel([T| Ts]) ->
    ?line timer:cancel(T),
    ?line cancel(Ts);
cancel([]) ->
    ok.

num_timers() ->
    {{_, TotalTimers},{_, _IntervalTimers}} = timer:get_status(),
    TotalTimers.

receive_nisse() ->    
    receive
	nisse ->
	    receive_nisse()
    after 0 ->
	    ok
    end.


get_mess(Time, Mess) -> get_mess(Time, Mess, 1).
get_mess(_, _, 0) -> ok;  % Received
get_mess(Time, Mess, N) ->
    receive 
	Mess -> get_mess(Time, Mess, N-1)
    after Time
	  -> nor   % Not Received
    end.

forever() ->
    timer:sleep(1000),
    forever().


%
% Testing for performance (on different implementations) of timers 
% 

timer_perf(suite) -> [];
timer_perf(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(10)),
    Res = performance(timer),
    ?t:timetrap_cancel(Dog),
    Res.

performance(Mod) ->
    process_flag(trap_exit, true),    
    {Y,Mo,D} = date(),
    {H,M,S} = time(),
    io:format("Testing module '~p' Date: ~w/~w/~w ~w:~w:~w~n", 
	      [Mod,Y,Mo,D,H,M,S]),
    Result = big_test(Mod),
    report_result(Result).

big_test(M) ->
    Load_Pids = start_nrev(20, M),   % Increase if more load wanted :)
 
    apply(M, sleep, [9000]),
    LPids = spawn_timers(5, M, 10000, 5),

    apply(M, sleep, [4000]),
    MPids = spawn_timers(10, M, 1000, 6),

    apply(M, sleep, [3500]),
    SPids = spawn_timers(15, M, 100, 3),

    Res = wait(SPids ++ MPids ++ LPids, [], 0, M),
    
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Load_Pids),
    Res.

wait([], Res, N, _) ->
    {Res, N};
wait(Pids, ResList, N, M) ->
    receive
	{Pid, ok, Res, T} ->
	    wait(lists:delete(Pid, Pids), [{T, Res} | ResList], N, M);
	{Pid, Error}->
	    ?line test_server:fail(Error),
	    wait(lists:delete(Pid, Pids), ResList, N+1, M);
	{'EXIT', Pid, normal} ->
	    wait(lists:delete(Pid, Pids), ResList, N, M);
	{'EXIT', Pid, Reason} ->
    	    ?line test_server:fail({Pid,Reason})
    end.

spawn_timers(0, _, _, _) ->
    [];
spawn_timers(N, M, T, NumIter) ->
    apply(M, sleep, [120*N]),
    Pid1 = spawn_link(?MODULE, timer, [apply, M, T, self()]),
    Pid2 = spawn_link(?MODULE, timer, [interval, M, T, self(), NumIter]),
    [Pid1, Pid2 | spawn_timers(N-1, M, T, NumIter)].

timer(apply, Mod, T, Pid) ->
    Before = system_time(),
    {ok, Ref} = apply(Mod, apply_after, [T, ?MODULE, send, [self(), done]]),
    receive 
	done ->
	    After = system_time(),
	    Pid ! {self(), ok, (After-Before) div 1000, T}
    after T*3 + 300 ->   % Watch dog
	    io:format("WARNING TIMER WATCHDOG timed out: ~w ~n", [T]),
	    timer:cancel(Ref),
	    Pid ! {self(), watch_dog_timed_out}
    end.

timer(interval, Mod, T, Pid, NumIter) ->
    Before = system_time(),
    {ok, Ref} = apply(Mod, apply_interval, [T, ?MODULE, send, [self(), done]]),
    timer_irec(Before, T, {0, NumIter}, [], {Pid, Mod, Ref}).

timer_irec(_Start, T, {N, N}, Res, {Pid, Mod, Ref}) ->
    apply(Mod, cancel, [Ref]),
    Min = lists:min(Res),
    Max = lists:max(Res),
    Tot = lists:sum(Res),
    Pid ! {self(), ok, {N, Tot, Tot div N, Min, Max}, T};
timer_irec(Start, T, {N, Max}, Res, {Pid, Mod, Ref}) ->
    receive
	done ->
	    Now = system_time(),
	    Elapsed = (Now - (Start + (N*T*1000))) div 1000,
%	    io:format("~w Now ~w Started ~w Elap ~w~n", [T,Now,Start,Elapsed]),
	    timer_irec(Start, T,
		       {N+1, Max},
		       [Elapsed | Res],
		       {Pid, Mod, Ref})
    after T*3 + 300 ->
	    apply(Mod, cancel, [Ref]),
	    io:format("WARNING: TIMER WATCHDOG timed out <Interval>~w~n",[T]),
	    Pid ! {self(), timer_watchdog_timed_out_in_interlval_test}  
    end.

%% ------------------------------------------------------- %%
%%  Small last generator

start_nrev(0, _) ->
    [];

start_nrev(N, M) ->
    Pid = spawn_link(?MODULE, do_nrev, [N, M]),
    [Pid | start_nrev(N-1, M)].

do_nrev(Sleep, Mod) ->
    apply(Mod, sleep, [50 * Sleep]),
    test(1000,"abcdefghijklmnopqrstuvxyz1234"),
    ok.

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

system_time() ->    
    {M,S,U} = erlang:now(),
    1000000*(M*1000000 + S) + U.

%% ------------------------------------------------------- %%

report_result({Res, 0}) ->
%    io:format("DEBUG0 all ~p ~n", [Res]),
    {A_List, I_List} = split_list(Res, [], []),
    A_val = calc_a_val(A_List),
    I_val = calc_i_val(I_List),
    print_report(A_val, I_val),
    ok;

report_result({Head, N}) ->
    io:format("Test Failed: Number of internal tmo ~w~n", [N]),
    ?line test_server:fail({Head, N}).

split_list([], AL, IL) ->
    {AL, IL};
split_list([{T, {N, Tot, A, Min, Max}} | Rest], AL, IL) ->
    split_list(Rest, AL, [{T, {N, Tot, A, Min, Max}} | IL]);
split_list([Head | Rest], AL, IL) ->
    split_list(Rest, [Head | AL], IL).

split([{T, Res} | R]) ->
    split(R, {{T,[Res]}, {T*10,[]}, {T*100,[]}}).

split([{T, Res} | R], {{T,S}, M, L}) ->
    split(R, {{T,[Res|S]}, M, L});

split([{T, Res} | R], {S, {T,M}, L}) ->
    split(R, {S, {T, [Res|M]}, L});

split([{T, Res} | R], {S, M, {T,L}}) ->
    split(R, {S, M, {T, [Res|L]}});

split(_Done, Vals) ->
    Vals.

calc_a_val(List) ->
    New = lists:sort(List),
    {{T1, S}, {T2, M}, {T3, L}} = split(New),
    S2 = {length(S), lists:max(S), lists:min(S), 
	  lists:sum(S) div length(S)},
    M2 = {length(M), lists:max(M), lists:min(M), 
	  lists:sum(M) div length(M)},
    L2 = {length(L), lists:max(L), lists:min(L), 
	  lists:sum(L) div length(L)},
    [{T1, S2}, {T2, M2}, {T3, L2}].

calc_i_val(List) ->
    New =  lists:sort(List),
    {{T1, S}, {T2, M}, {T3, L}} = split(New),
    S2 = get_ivals(S),
    M2 = get_ivals(M),
    L2 = get_ivals(L),
    [{T1, S2}, {T2, M2}, {T3, L2}].

get_ivals(List) ->
    Len = length(List),
    Num = element(1, hd(List)), % Number of iterations

    LTot = lists:map(fun(X) -> element(2, X) end, List),
    LMin = lists:map(fun(X) -> element(4, X) end, List),
    LMax = lists:map(fun(X) -> element(5, X) end, List),
    
    MaxTot  = lists:max(LTot),
    MinTot  = lists:min(LTot),
    AverTot = lists:sum(LTot) div Len,
    
    IterMax = lists:max(LMax),
    IterMin = lists:min(LMin),
    IterAver= AverTot div Num,

    {Len, Num,
     {MaxTot, MinTot, AverTot}, 
     {IterMax, IterMin, IterAver}}.


print_report(A_L, I_L) ->
    io:format("~nRESULTS from timer test~n~n",[]),
    io:format("Time out times for send_after~n~n", []),
    io:format("Time No of tests  Max    Min  Average~n",[]),
    print_aval(A_L),
    io:format("Time out times for send_interval~n~n", []),
    io:format("Time No.tests  No.intvals TotMax TotMin TotAver  MaxI   MinI  AverI~n", []),
    print_ival(I_L).

print_aval([]) ->
    io:format("~n~n", []);
print_aval([{T, {L, Max, Min, Aver}}|R]) ->
    io:format("~5w ~8w ~6w ~6w ~8w ~n", 
	      [T,L,Max,Min,Aver]),
    print_aval(R).

print_ival([]) ->
    io:format("~n", []);
print_ival([{T, {Len, Num, 
		 {MaxT, MinT, AverT},
		 {MaxI, MinI, AverI}}}|R]) ->
    io:format("~5w ~6w ~10w ~8w ~6w ~6w ~6w ~6w ~6w~n", 
	      [T,Len,Num,MaxT,MinT,AverT, MaxI, MinI, AverI]),
    print_ival(R).

send(Pid, Msg) ->
    Pid ! Msg.
