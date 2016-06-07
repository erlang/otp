%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

-module(time_SUITE).
-compile({nowarn_deprecated_function, {erlang,now,0}}).

%% "Time is on my side." -- The Rolling Stones

%% Tests the BIFs:
%%	erlang:localtime_to_universaltime/1
%%	erlang:universaltime_to_localtime/1
%%	date/0
%%	time/0
%%	now/0
%%

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, univ_to_local/1, local_to_univ/1,
	 bad_univ_to_local/1, bad_local_to_univ/1,
	 univ_to_seconds/1, seconds_to_univ/1,
	 consistency/1,
	 now_unique/1, now_update/1, timestamp/1,
	 time_warp_modes/1,
	 monotonic_time_monotonicity/1,
	 monotonic_time_monotonicity_parallel/1,
	 time_unit_conversion/1,
	 signed_time_unit_conversion/1,
	 erlang_timestamp/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([local_to_univ_utc/1]).

-include_lib("common_test/include/ct.hrl").

-export([linear_time/1]).

%% The following defines the timezone in which the test is run.
%% It is interpreted as the number of hours to be added to UTC
%% to obtain the local time.  The number will be positive east
%% of Greenwhich, negative west of Greenwhich.
%%
%% Allowable range is -12 through 11.

-define(timezone, 1).

%% Similarly to timezone, but the difference when Daylight Saving Time
%% is in use.  [Same range.]

-define(dst_timezone, 2).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(_Func, _Config) ->
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [univ_to_local, local_to_univ, local_to_univ_utc,
     bad_univ_to_local, bad_local_to_univ, 
     univ_to_seconds, seconds_to_univ,
     consistency,
     {group, now}, timestamp,
     time_warp_modes,
     monotonic_time_monotonicity,
     monotonic_time_monotonicity_parallel,
     time_unit_conversion,
     signed_time_unit_conversion,
     erlang_timestamp].

groups() -> 
    [{now, [], [now_unique, now_update]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Test that DST = true on timezones without DST is ignored
local_to_univ_utc(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} ->
	    %% TZ variable has a meaning
	    {ok, Node} =
		test_server:start_node(local_univ_utc,peer,
				       [{args, "-env TZ UTC"}]),
	    {{2008,8,1},{0,0,0}} =
		rpc:call(Node,
			 erlang,localtime_to_universaltime,
			 [{{2008, 8, 1}, {0, 0, 0}},
			  false]),
	    {{2008,8,1},{0,0,0}} =
		rpc:call(Node,
			 erlang,localtime_to_universaltime,
			 [{{2008, 8, 1}, {0, 0, 0}},
			  true]),
	    [{{2008,8,1},{0,0,0}}] =
		rpc:call(Node,
			 calendar,local_time_to_universal_time_dst,
			 [{{2008, 8, 1}, {0, 0, 0}}]),
	    test_server:stop_node(Node),
	    ok;
	_ ->
	    {skip,"Only valid on Unix"}
    end.


%% Tests conversion from univeral to local time.

univ_to_local(Config) when is_list(Config) ->
    test_univ_to_local(test_data()).

test_univ_to_local([{Utc, Local}|Rest]) ->
    io:format("Testing ~p => ~p~n", [Local, Utc]),
    Local = erlang:universaltime_to_localtime(Utc),
    test_univ_to_local(Rest);
test_univ_to_local([]) ->
    ok.

%% Tests conversion from local to universal time.

local_to_univ(Config) when is_list(Config) ->
    test_local_to_univ(test_data()).

test_local_to_univ([{Utc, Local}|Rest]) ->
    io:format("Testing ~p => ~p~n", [Utc, Local]),
    Utc = erlang:localtime_to_universaltime(Local),
    test_local_to_univ(Rest);
test_local_to_univ([]) ->
    ok.

%% Test bad arguments to erlang:universaltime_to_localtime; should
%% generate a badarg.

bad_univ_to_local(Config) when is_list(Config) ->
    bad_test_univ_to_local(bad_dates()).

bad_test_univ_to_local([Utc|Rest]) ->
    io:format("Testing ~p~n", [Utc]),
    case catch erlang:universaltime_to_localtime(Utc) of
	      {'EXIT', {badarg, _}} -> bad_test_univ_to_local(Rest)
	  end;
bad_test_univ_to_local([]) ->
    ok.

%% Test bad arguments to erlang:localtime_to_universaltime/1; should
%% generate a badarg.

bad_local_to_univ(Config) when is_list(Config) ->
    bad_test_local_to_univ(bad_dates()).

bad_test_local_to_univ([Local|Rest]) ->
    io:format("Testing ~p~n", [Local]),
    case catch erlang:localtime_to_universaltime(Local) of
	      {'EXIT', {badarg, _}} -> bad_test_local_to_univ(Rest)
	  end;
bad_test_local_to_univ([]) ->
    ok.


%% Test universaltime to seconds conversions
univ_to_seconds(Config) when is_list(Config) ->
    test_univ_to_seconds(ok_utc_seconds()).

test_univ_to_seconds([{Datetime, Seconds}|DSs]) ->
    io:format("universaltime = ~p -> seconds = ~p", [Datetime, Seconds]),
    Seconds = erlang:universaltime_to_posixtime(Datetime),
    test_univ_to_seconds(DSs);
test_univ_to_seconds([]) -> 
    ok.

%% Test seconds to universaltime conversions
seconds_to_univ(Config) when is_list(Config) ->
    test_seconds_to_univ(ok_utc_seconds()).

test_seconds_to_univ([{Datetime, Seconds}|DSs]) ->
    io:format("universaltime = ~p <- seconds = ~p", [Datetime, Seconds]),
    Datetime = erlang:posixtime_to_universaltime(Seconds),
    test_seconds_to_univ(DSs);
test_seconds_to_univ([]) -> 
    ok.


%% Test that the the different time functions return
%% consistent results.
consistency(_Config) ->
    %% Test that:
    %% 	 * date() & time() gives the same time as erlang:localtime()
    %%
    %%   * the difference between erlang:universaltime() and
    %%     erlang:localtime() is reasonable (with assuming any
    %%     particular timezone)

    ok = compare_date_time_and_localtime(16),
    compare_local_and_universal(16).

compare_date_time_and_localtime(Times) when Times > 0 ->
    {Year, Mon, Day} = date(),
    {Hour, Min, Sec} = time(),
    case erlang:localtime() of
	{{Year, Mon, Day}, {Hour, Min, Sec}} -> ok;
	_ -> compare_date_time_and_localtime(Times-1)
    end;
compare_date_time_and_localtime(0) ->
    error.

compare_local_and_universal(Times) when Times > 0 ->
    Utc = erlang:universaltime(),
    Local = erlang:localtime(),
    io:format("local = ~p, utc = ~p", [Local,Utc]),

    AcceptableDiff = 14*3600,
    case linear_time(Utc) - linear_time(Local) of
	Diff when abs(Diff) < AcceptableDiff ->
	    ok;
	Diff ->
	    io:format("More than ~p seconds difference betwen "
		      "local and universal time", [Diff]),
	    ct:fail(huge_diff)
    end.

%% This function converts a date and time to a linear time.
%% Two linear times can be subtracted to give their difference
%% in seconds.
%%
%% XXX Limitations: Simplified leap year calc will fail for 2100 :-)

linear_time({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    86400*(year_to_days(Year) + month_to_days(Year,Mon) + (Day-1)) +
	3600*Hour + 60*Min + Sec.

year_to_days(Year) ->
    Year * 365 + (Year-1) div 4.

month_to_days(Year, Mon) ->
    DoM = [31,days_in_february(Year),31,30,31,30,31,31,30,31,30,31],
    {PastMonths,_} = lists:split(Mon-1, DoM),
    lists:sum(PastMonths).

days_in_february(Year) ->
    case (Year rem 4) of
	0 -> 29;
	_ -> 28
    end.

%% Test (the bif) os:timestamp/0, which is something quite like, but not
%% similar to erlang:now...

%% Test that os:timestamp works.
timestamp(Config) when is_list(Config) ->
    try
	repeating_timestamp_check(100000)
    catch
	throw : {fail, Failure} ->
	    %%
	    %% Our time warping test machines currently warps
	    %% time every 6:th second. If we get a warp during
	    %% 10 seconds, assume this is a time warping test
	    %% and ignore the failure.
	    %%
	    case had_time_warp(10) of
		true ->
		    {skip, "Seems to be time warp test run..."};
		false ->
		    ct:fail(Failure)
	    end
    end.

os_system_time_offset() ->
    erlang:convert_time_unit(os:system_time() - erlang:monotonic_time(),
			     native, micro_seconds).

had_time_warp(Secs) ->
    had_time_warp(os_system_time_offset(), Secs).

had_time_warp(OrigOffs, 0) ->
    false;
had_time_warp(OrigOffs, N) ->
    receive after 1000 -> ok end,
    case OrigOffs - os_system_time_offset() of
	Diff when Diff > 500000; Diff < -500000 ->
	    true;
	_Diff ->
	    had_time_warp(OrigOffs, N-1)
    end.

repeating_timestamp_check(0) ->
    ok;
repeating_timestamp_check(N) ->
    {A,B,C} = TS = os:timestamp(),
    if
	is_integer(A),
	is_integer(B),
	is_integer(C),
	B < 1000000,
	C < 1000000 ->
	    ok;
	true ->
	    ct:fail("Strange return from os:timestamp/0 ~w~n",[TS])
    end,
    %% I assume the now and timestamp should not differ more than 1 hour,
    %% which is safe assuming the system has not had a large time-warp
    %% during the testrun...
    Secs = A*1000000+B+round(C/1000000),
    {NA,NB,NC} = erlang:now(),
    NSecs = NA*1000000+NB+round(NC/1000000),
    case Secs - NSecs of
	TooLarge when TooLarge > 3600 ->
	    throw({fail,
	       lists:flatten(
		io_lib:format("os:timestamp/0 is ~w s more than erlang:now/0",
			     [TooLarge]))});
	TooSmall when TooSmall < -3600 ->
	    throw({fail,
	      lists:flatten(
		io_lib:format("os:timestamp/0 is ~w s less than erlang:now/0",
			     [-TooSmall]))});
	_ ->
	    ok
    end,
    repeating_timestamp_check(N-1).
	    

%% Test now/0.


%% Tests that successive calls to now/0 returns different values.
%% Also returns a comment string with the median difference between
%% times (in microseconds).

now_unique(Config) when is_list(Config) ->
    now_unique(1000, now(), []),
    fast_now_unique(100000, now()).

now_unique(N, Previous, Result) when N > 0 ->
    case now() of
	      Previous ->
		  ct:fail("now/0 returned the same value twice");
	      New ->
		  now_unique(N-1, New, [New|Result])
	  end;
now_unique(0, _, [Then|Rest]) ->
    now_calc_increment(Rest, microsecs(Then), []).

now_calc_increment([Then|Rest], Previous, _Result) ->
    This = microsecs(Then),
    now_calc_increment(Rest, This, [Previous-This]);
now_calc_increment([], _, Differences) ->
    {comment, "Median increment: " ++ integer_to_list(median(Differences))}.

fast_now_unique(0, _) -> ok;
fast_now_unique(N, Then) ->
    case now() of
	Then ->
	    ct:fail("now/0 returned the same value twice");
	Now ->
	    fast_now_unique(N-1, Now)
    end.

median(Unsorted_List) ->
    Length = length(Unsorted_List),
    List = lists:sort(Unsorted_List),
    case Length rem 2 of
	0 ->					% Even length.
	    [A, B] = lists:nthtail((Length div 2)-1, List),
	    (A+B)/2;
	1 ->					% Odd list length.
	    lists:nth((Length div 2)+1, List)
    end.

microsecs({Mega_Secs, Secs, Microsecs}) ->
    (Mega_Secs*1000000+Secs)*1000000+Microsecs.

%% Test that the time differences returned by two calls to
%% now/0 one second apart is comparable to the difference of two
%% calls to erlang:localtime().

now_update(Config) when is_list(Config) ->
    case test_server:is_debug() of
	false -> now_update1(10);
	true -> {skip,"Unreliable in DEBUG build"}
    end.


now_update1(N) when N > 0 ->
    T1_linear = linear_time(erlang:localtime()),
    T1_now = microsecs(now()),

    receive after 1008 -> ok end,

    T2_linear = linear_time(erlang:localtime()),
    T2_now = microsecs(now()),

    Linear_Diff = (T2_linear-T1_linear)*1000000,
    Now_Diff = T2_now-T1_now,
    io:format("Localtime diff = ~p; now() diff = ~p", [Linear_Diff, Now_Diff]),
    case abs(Linear_Diff - Now_Diff) of
	      Abs_Delta when Abs_Delta =< 40000 -> ok;
	      _ -> now_update1(N-1)
	  end;
now_update1(0) ->
    ct:fail("now_update zero").

time_warp_modes(Config) when is_list(Config) ->
    %% All time warp modes always supported in
    %% combination with no time correction...
    check_time_warp_mode(Config, false, no_time_warp),
    check_time_warp_mode(Config, false, single_time_warp),
    check_time_warp_mode(Config, false, multi_time_warp),

    erts_debug:set_internal_state(available_internal_state, true),
    try
	case erts_debug:get_internal_state({check_time_config,
					    true, no_time_warp}) of
	    false -> ok;
	    true -> check_time_warp_mode(Config, true, no_time_warp)
	end,
	case erts_debug:get_internal_state({check_time_config,
					    true, single_time_warp}) of
	    false -> ok;
	    true -> check_time_warp_mode(Config, true, single_time_warp)
	end,
	case erts_debug:get_internal_state({check_time_config,
					    true, multi_time_warp}) of
	    false -> ok;
	    true -> check_time_warp_mode(Config, true, multi_time_warp)
	end
    after
	erts_debug:set_internal_state(available_internal_state, false)
    end.

check_time_warp_mode(Config, TimeCorrection, TimeWarpMode) ->
    io:format("~n~n~n***** Testing TimeCorrection=~p TimeWarpMode=~p *****~n",
	      [TimeCorrection, TimeWarpMode]),
    Mon = erlang:monitor(time_offset, clock_service),
    _ = erlang:time_offset(),
    Start = erlang:monotonic_time(1000),
    MonotonicityTimeout = 2000,
    {ok, Node} = start_node(Config,
			    "+c " ++ atom_to_list(TimeCorrection)
			    ++ " +C " ++ atom_to_list(TimeWarpMode)),
    StartTime = rpc:call(Node, erlang, system_info, [start_time]),
    Me = self(),
    MonotincityTestStarted = make_ref(),
    MonotincityTestDone = make_ref(),
    spawn_link(Node,
	       fun () ->
		       Me ! MonotincityTestStarted,
		       cmp_times(erlang:start_timer(MonotonicityTimeout,
						    self(),
						    timeout),
				 erlang:monotonic_time()),
		       Me ! MonotincityTestDone
	       end),
    receive MonotincityTestStarted -> ok end,
    check_time_offset(Node, TimeWarpMode),
    TimeWarpMode = rpc:call(Node, erlang, system_info, [time_warp_mode]),
    TimeCorrection = rpc:call(Node, erlang, system_info, [time_correction]),
    receive MonotincityTestDone -> ok end,
    MonotonicTime = rpc:call(Node, erlang, monotonic_time, []),
    MonotonicTimeUnit = rpc:call(Node,
				       erlang,
				       convert_time_unit,
				       [1, seconds, native]),
    UpMilliSeconds = erlang:convert_time_unit(MonotonicTime - StartTime,
					      MonotonicTimeUnit,
					      milli_seconds),
    io:format("UpMilliSeconds=~p~n", [UpMilliSeconds]),
    End = erlang:monotonic_time(milli_seconds),
    stop_node(Node),
    try
	true = (UpMilliSeconds > (98*MonotonicityTimeout) div 100),
	true = (UpMilliSeconds < (102*(End-Start)) div 100)
    catch
	error:_ ->
	    io:format("Uptime inconsistency", []),
	    case {TimeCorrection, erlang:system_info(time_correction)} of
		{true, true} ->
		    ct:fail(uptime_inconsistency);
		{true, false} ->
		    _ = erlang:time_offset(),
		    receive
			{'CHANGE', Mon, time_offset, clock_service, _} ->
			    ignore
		    after 1000 ->
			    ct:fail(uptime_inconsistency)
		    end;
		_ ->
		    ignore
	    end
    end,
    erlang:demonitor(Mon, [flush]),
    ok.

check_time_offset(Node, no_time_warp) ->
    final = rpc:call(Node, erlang, system_info, [time_offset]),
    final = rpc:call(Node, erlang, system_flag, [time_offset, finalize]),
    final = rpc:call(Node, erlang, system_info, [time_offset]);
check_time_offset(Node, single_time_warp) ->
    preliminary = rpc:call(Node, erlang, system_info, [time_offset]),
    preliminary = rpc:call(Node, erlang, system_flag, [time_offset, finalize]),
    final = rpc:call(Node, erlang, system_info, [time_offset]),
    final = rpc:call(Node, erlang, system_flag, [time_offset, finalize]);
check_time_offset(Node, multi_time_warp) ->
    volatile = rpc:call(Node, erlang, system_info, [time_offset]),
    volatile = rpc:call(Node, erlang, system_flag, [time_offset, finalize]),
    volatile = rpc:call(Node, erlang, system_info, [time_offset]).

monotonic_time_monotonicity(Config) when is_list(Config) ->
    Done = erlang:start_timer(10000,self(),timeout),
    cmp_times(Done, erlang:monotonic_time()).

cmp_times(Done, X0) ->
    X1 = erlang:monotonic_time(),
    X2 = erlang:monotonic_time(),
    X3 = erlang:monotonic_time(),
    X4 = erlang:monotonic_time(),
    X5 = erlang:monotonic_time(),
    true = (X0 =< X1),
    true = (X1 =< X2),
    true = (X2 =< X3),
    true = (X3 =< X4),
    true = (X4 =< X5),
    receive
	{timeout, Done, timeout} ->
	    ok
    after 0 ->
	    cmp_times(Done, X5)
    end.

-define(NR_OF_MONOTONIC_CALLS, 100000).

monotonic_time_monotonicity_parallel(Config) when is_list(Config) ->
    Me = self(),
    Result = make_ref(),
    Go = make_ref(),
    UpAndRunning = make_ref(),
    NoOnlnScheds = erlang:system_info(schedulers_online),
    OffsetUI = erlang:unique_integer([monotonic]),
    OffsetMT = erlang:monotonic_time(),
    MinHSz = ?NR_OF_MONOTONIC_CALLS*(2
				     + 3
				     + erts_debug:flat_size(OffsetUI)
				     + erts_debug:flat_size(OffsetMT)),
    Ps = lists:map(
	   fun (Sched) ->
		   spawn_opt(
		     fun () ->
			     Me ! {self(), UpAndRunning},
			     receive Go -> ok end,
			     Res = fetch_monotonic(?NR_OF_MONOTONIC_CALLS, []),
			     Me ! {self(), Result, Sched, Res}
		     end,
		     [{scheduler, Sched},
		      {priority, max},
		      {min_heap_size, MinHSz}])
	   end,
	   lists:seq(1, NoOnlnScheds)),
    lists:foreach(fun (P) -> receive {P, UpAndRunning} -> ok end end, Ps),
    lists:foreach(fun (P) -> P ! Go end, Ps),
    TMs = recv_monotonics(Result, OffsetMT, OffsetUI, NoOnlnScheds, []),
    true = check_monotonic_result(TMs, OffsetMT, OffsetUI, true).

check_monotonic_result([{_Sched, _PrevUI, _MT, _PostUI}],
		       _OffsetMT, _OffsetUI, Res) ->
    Res;
check_monotonic_result([{_ASched, _APrevUI, AMT, APostUI} = A,
			{_BSched, BPrevUI, BMT, _BPostUI} = B | _] = L,
		       OffsetMT, OffsetUI, Res) ->
    NewRes = case (AMT =< BMT) orelse (BPrevUI < APostUI) of
		 true ->
		     Res;
		 false ->
		     io:format("INCONSISTENCY: ~p ~p~n", [A, B]),
		     false
	     end,
    check_monotonic_result(tl(L), OffsetMT, OffsetUI, NewRes).

recv_monotonics(_Result, _OffsetMT, _OffsetUI, 0, Acc) ->
    lists:keysort(2, Acc);
recv_monotonics(Result, OffsetMT, OffsetUI, N, Acc) ->
    receive
	{_, Result, Sched, Res} ->
	    CRes = convert_monotonic(Sched, OffsetMT, OffsetUI, Res, []),
	    recv_monotonics(Result, OffsetMT, OffsetUI, N-1, CRes ++ Acc)
    end.

convert_monotonic(_Sched, _OffsetMT, _OffsetUI, [{_MT, _UI}], Acc) ->
    Acc;
convert_monotonic(Sched, OffsetMT, OffsetUI,
		  [{MT, UI}, {_PrevMT, PrevUI} | _] = L, Acc) ->
    convert_monotonic(Sched, OffsetMT, OffsetUI, tl(L),
		      [{Sched, PrevUI-OffsetUI, MT-OffsetMT, UI-OffsetUI}
		       | Acc]).

fetch_monotonic(0, Acc) ->
    Acc;
fetch_monotonic(N, Acc) ->
    MT = erlang:monotonic_time(),
    UI = erlang:unique_integer([monotonic]),
    fetch_monotonic(N-1, [{MT, UI} | Acc]).

-define(CHK_RES_CONVS_TIMEOUT, 400).

time_unit_conversion(Config) when is_list(Config) ->
    Mon = erlang:monitor(time_offset, clock_service),
    start_check_res_convs(Mon, 1000000000000),
    start_check_res_convs(Mon, 2333333333333),
    start_check_res_convs(Mon, 5732678356789),
    erlang:demonitor(Mon, [flush]).
    
start_check_res_convs(Mon, Res) ->
    io:format("Checking ~p time_unit~n", [Res]),
    check_res_convs(Mon,
		    erlang:start_timer(?CHK_RES_CONVS_TIMEOUT,
				       self(),
				       timeout),
		    Res).
    

check_res_convs(Mon, Done, Res) ->
    receive
	{timeout, Done, timeout} ->
	    case Res div 10 of
		0 ->
		    ok;
		NewRes ->
		    start_check_res_convs(Mon, NewRes)
	    end
    after 0 ->
	    do_check_res_convs(Mon, Done, Res)
    end.

do_check_res_convs(Mon, Done, Res) ->
    TStart = erlang:monotonic_time(),
    T = erlang:monotonic_time(Res),
    TEnd = erlang:monotonic_time(),
    TMin = erlang:convert_time_unit(TStart, native, Res),
    TMax = erlang:convert_time_unit(TEnd, native, Res),
    %io:format("~p =< ~p =< ~p~n", [TMin, T, TEnd]),
    true = (TMin =< T),
    true = (TMax >= T),
    check_time_offset_res_conv(Mon, Res),
    check_res_convs(Mon, Done, Res).


check_time_offset_res_conv(Mon, Res) ->
    TORes = erlang:time_offset(Res),
    TO = erlang:time_offset(),
    case erlang:convert_time_unit(TO, native, Res) of
	TORes ->
	    ok;
	TORes2 ->
	    case check_time_offset_change(Mon, TO, 1000) of
		{TO, false} ->
		    ct:fail({time_unit_conversion_inconsistency,
			     TO, TORes, TORes2});
		{_NewTO, true} ->
		    io:format("time_offset changed", []),
		    check_time_offset_res_conv(Mon, Res)
	    end
    end.

signed_time_unit_conversion(Config) when is_list(Config) ->
    chk_strc(1000000000, 1000000),
    chk_strc(1000000000, 1000),
    chk_strc(1000000000, 1),
    chk_strc(1000000, 1000),
    chk_strc(1000000, 1),
    chk_strc(1000, 1),
    chk_strc(4711, 17),
    chk_strc(1 bsl 10, 1),
    chk_strc(1 bsl 16, 10),
    chk_strc(1 bsl 17, 1 bsl 8),
    chk_strc((1 bsl 17) + 1, (1 bsl 8) - 1),
    chk_strc(1 bsl 17, 11),
    ok.

chk_strc(Res0, Res1) ->
    case (Res0 /= Res1) andalso (Res0 =< 1000000) andalso (Res1 =< 1000000) of
	true ->
	    {FromRes, ToRes} = case Res0 > Res1 of
				   true -> {Res0, Res1};
				   false -> {Res1, Res0}
			       end,
	    MinFromValuesPerToValue = FromRes div ToRes,
	    MaxFromValuesPerToValue = ((FromRes-1) div ToRes)+1,
	    io:format("~p -> ~p [~p, ~p]~n",
		      [FromRes, ToRes,
		       MinFromValuesPerToValue, MaxFromValuesPerToValue]),
	    chk_values_per_value(FromRes, ToRes,
				 -10*FromRes, 10*FromRes,
				 MinFromValuesPerToValue,
				 MaxFromValuesPerToValue,
				 undefined, MinFromValuesPerToValue);
	_ ->
	    ok
    end,
    chk_random_values(Res0, Res1),
    chk_random_values(Res1, Res0),
    ok.

chk_random_values(FR, TR) ->
    io:format("rand values ~p -> ~p~n", [FR, TR]),
    rand:seed(exsplus, {268438039,268440479,268439161}),
    Values = lists:map(fun (_) -> rand:uniform(1 bsl 65) - (1 bsl 64) end,
		       lists:seq(1, 100000)),
    CheckFun = fun (V) ->
                       CV = erlang:convert_time_unit(V, FR, TR),
                       case {(FR*CV) div TR =< V,
                             (FR*(CV+1)) div TR >= V} of
                           {true, true} ->
                               ok;
                           Failure ->
                               ct:fail({Failure, CV, V, FR, TR})
                       end
               end,
    lists:foreach(CheckFun, Values).
		       

chk_values_per_value(_FromRes, _ToRes,
	 EndValue, EndValue,
	 MinFromValuesPerToValue, MaxFromValuesPerToValue,
	 _ToValue, FromValueCount) ->
%    io:format("~p [~p]~n", [EndValue, FromValueCount]),
    case ((MinFromValuesPerToValue =< FromValueCount)
	  andalso (FromValueCount =< MaxFromValuesPerToValue)) of
	false ->
	    ct:fail({MinFromValuesPerToValue,
		     FromValueCount,
		     MaxFromValuesPerToValue});
	true ->
	    ok
    end;
chk_values_per_value(FromRes, ToRes, Value, EndValue,
		     MinFromValuesPerToValue, MaxFromValuesPerToValue,
		     ToValue, FromValueCount) ->
    case erlang:convert_time_unit(Value, FromRes, ToRes) of
        ToValue ->
            chk_values_per_value(FromRes, ToRes,
                                 Value+1, EndValue,
                                 MinFromValuesPerToValue,
                                 MaxFromValuesPerToValue,
                                 ToValue, FromValueCount+1);
        NewToValue ->
            case ((MinFromValuesPerToValue =< FromValueCount)
                  andalso (FromValueCount =< MaxFromValuesPerToValue)) of
                false ->
                    ct:fail({MinFromValuesPerToValue,
                             FromValueCount,
                             MaxFromValuesPerToValue});
                true ->
                    %		    io:format("~p -> ~p [~p]~n",
                    %			      [Value, NewToValue, FromValueCount]),
                    chk_values_per_value(FromRes, ToRes,
                                         Value+1, EndValue,
                                         MinFromValuesPerToValue,
                                         MaxFromValuesPerToValue,
                                         NewToValue, 1)
            end
    end.

erlang_timestamp(Config) when is_list(Config) ->
    Mon = erlang:monitor(time_offset, clock_service),
    {TO, _} = check_time_offset_change(Mon,
				       erlang:time_offset(),
				       0),
    Done = erlang:start_timer(10000,self(),timeout),
    ok = check_erlang_timestamp(Done, Mon, TO).

check_erlang_timestamp(Done, Mon, TO) ->
    receive
        {timeout, Done, timeout} ->
            erlang:demonitor(Mon, [flush]),
            ok
    after 0 ->
              do_check_erlang_timestamp(Done, Mon, TO)
    end.

do_check_erlang_timestamp(Done, Mon, TO) ->
    MinMon = erlang:monotonic_time(),
    {MegaSec, Sec, MicroSec} = erlang:timestamp(),
    MaxMon = erlang:monotonic_time(),
    TsMin = erlang:convert_time_unit(MinMon+TO,
				     native,
				     micro_seconds),
    TsMax = erlang:convert_time_unit(MaxMon+TO,
				     native,
				     micro_seconds),
    TsTime = (MegaSec*1000000+Sec)*1000000+MicroSec,
    case (TsMin =< TsTime) andalso (TsTime =< TsMax) of
	true ->
	    NewTO = case erlang:time_offset() of
			TO ->
			    TO;
			_ ->
			    check_time_offset_change(Mon, TO, 0)
		    end,
	    check_erlang_timestamp(Done, Mon, NewTO);
	false ->
	    io:format("TsMin=~p TsTime=~p TsMax=~p~n", [TsMin, TsTime, TsMax]),
	    io:format("Detected inconsistency; "
		      "checking for time_offset change...", []),
	    case check_time_offset_change(Mon, TO, 1000) of
		{TO, false} ->
		    ct:fail(timestamp_inconsistency);
		{NewTO, true} ->
		    io:format("time_offset changed", []),
		    check_erlang_timestamp(Done, Mon, NewTO)
	    end
    end.

check_time_offset_change(Mon, TO, Wait) ->
    process_changed_time_offset(Mon, TO, false, Wait).

process_changed_time_offset(Mon, TO, Changed, Wait) ->
    receive
	{'CHANGE', Mon, time_offset, clock_service, NewTO} ->
	    process_changed_time_offset(Mon, NewTO, true, Wait)
    after Wait ->
	    case erlang:time_offset() of
		TO ->
		    {TO, Changed};
		_OtherTO ->
		    receive
			{'CHANGE', Mon, time_offset, clock_service, NewTO} ->
			    process_changed_time_offset(Mon, NewTO, true, Wait)
		    end
	    end
    end.
    


%% Returns the test data: a list of {Utc, Local} tuples.

test_data() ->
    {TZ,DSTTZ} = 
	case os:type() of
	    {unix,_} ->
		case os:cmd("date '+%Z'") of
		    "SAST"++_ ->
			{2,2};
		    _ ->
			{?timezone,?dst_timezone}
		end;
	    _ ->
		{?timezone,?dst_timezone}
	end,
    test_data(nondst_dates(), TZ) ++
	test_data(dst_dates(), DSTTZ) ++
	crossover_test_data(crossover_dates(), TZ).    


%% test_data1() ->
%%     test_data(nondst_dates(), ?timezone) ++
%% 	test_data(dst_dates(), ?dst_timezone) ++
%% 	crossover_test_data(crossover_dates(), ?timezone).

crossover_test_data([{Year, Month, Day}|Rest], TimeZone) when TimeZone > 0 ->
    Hour = 23,
    Min = 35,
    Sec = 55,
    Utc = {{Year, Month, Day}, {Hour, Min, Sec}},
    Local = {{Year, Month, Day+1}, {Hour+TimeZone-24, Min, Sec}},
    [{Utc, Local}|crossover_test_data(Rest, TimeZone)];
crossover_test_data([{Year, Month, Day}|Rest], TimeZone) when TimeZone < 0 ->
    Hour = 0,
    Min = 23,
    Sec = 12,
    Utc = {{Year, Month, Day}, {Hour, Min, Sec}},
    Local = {{Year, Month, Day-1}, {Hour+TimeZone+24, Min, Sec}},
    [{Utc, Local}|crossover_test_data(Rest, TimeZone)];
crossover_test_data([], _) ->
    [].

test_data([Date|Rest], TimeZone) ->
    Hour = 12,
    Min = 45,
    Sec = 7,
    Utc = {Date, {Hour, Min, Sec}},
    Local = {Date, {Hour+TimeZone, Min, Sec}},
    [{Utc, Local}|test_data(Rest, TimeZone)];
test_data([], _) ->
    [].
    
nondst_dates() ->
    [{1996, 01, 30},
     {1997, 01, 30},
     {1998, 01, 30},
     {1999, 01, 30},
     {1996, 02, 29},
     {1997, 02, 28},
     {1998, 02, 28},
     {1999, 02, 28},
     {1996, 03, 2},
     {1997, 03, 2},
     {1998, 03, 2},
     {1999, 03, 2}].

dst_dates() ->
    [{1996, 06, 1},
     {1997, 06, 2},
     {1998, 06, 3},
     {1999, 06, 4}].

%% exakt utc {date(), time()} which corresponds to the same seconds since 1 jan 1970 
%% negative seconds are ok
%% generated with date --date='1979-05-28 12:30:35 UTC' +%s
ok_utc_seconds() -> [
	{ {{1970, 1, 1},{ 0, 0, 0}},            0 },
	{ {{1970, 1, 1},{ 0, 0, 1}},            1 },
	{ {{1969,12,31},{23,59,59}},           -1 },
	{ {{1920,12,31},{23,59,59}},  -1546300801 },
	{ {{1600,02,19},{15,14,08}}, -11671807552 },
	{ {{1979,05,28},{12,30,35}},    296742635 },
	{ {{1999,12,31},{23,59,59}},    946684799 },
	{ {{2000, 1, 1},{ 0, 0, 0}},    946684800 },
	{ {{2000, 1, 1},{ 0, 0, 1}},    946684801 },

	{ {{2038, 1,19},{03,14,07}},   2147483647 }, % Sint32 full - 1
	{ {{2038, 1,19},{03,14,08}},   2147483648 }, % Sint32 full
	{ {{2038, 1,19},{03,14,09}},   2147483649 }, % Sint32 full + 1

	{ {{2106, 2, 7},{ 6,28,14}},   4294967294 }, % Uint32 full  0xFFFFFFFF - 1
	{ {{2106, 2, 7},{ 6,28,15}},   4294967295 }, % Uint32 full  0xFFFFFFFF
	{ {{2106, 2, 7},{ 6,28,16}},   4294967296 }, % Uint32 full  0xFFFFFFFF + 1
	{ {{2012,12, 6},{16,28,08}},   1354811288 },
	{ {{2412,12, 6},{16,28,08}},  13977592088 }
    ].


%% The following dates should not be near the end or beginning of
%% a month, because they will be used to test when the dates are
%% different in UTC and local time.

crossover_dates() ->
    [{1996, 01, 25},
     {1997, 01, 25},
     {1998, 01, 25},
     {1999, 01, 25},
     {1996, 02, 27},
     {1997, 02, 27},
     {1998, 02, 27},
     {1999, 02, 27}].

bad_dates() ->
    [{{1900, 7, 1}, {12, 0, 0}},		% Year

     {{1996, 0, 20}, {12, 0, 0}},		% Month
     {{1996, 13, 20}, {12, 0, 0}},

     {{1996, 1, 0}, {12, 0, 0}},		% Date
     {{1996, 1, 32}, {12, 0, 0}},
     {{1996, 2, 30}, {12, 0, 0}},
     {{1997, 2, 29}, {12, 0, 0}},
     {{1998, 2, 29}, {12, 0, 0}},
     {{1999, 2, 29}, {12, 0, 0}},
     {{1996, 4, 31}, {12, 0, 0}},

     {{1996, 4, 30}, {-1, 0, 0}},		% Hour
     {{1996, 4, 30}, {25, 0, 0}},

     {{1996, 4, 30}, {12,-1, 0}},		% Minute
     {{1996, 4, 30}, {12, 60, 0}},

     {{1996, 4, 30}, {12, 0, -1}},		% Sec
     {{1996, 4, 30}, {12, 0, 60}}].

start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) ->
    TestCase = proplists:get_value(testcase, Config),
    PA = filename:dirname(code:which(?MODULE)),
    ESTime = erlang:monotonic_time(1) + erlang:time_offset(1),
    Unique = erlang:unique_integer([positive]),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(TestCase)
			++ "-"
			++ integer_to_list(ESTime)
			++ "-"
			++ integer_to_list(Unique)),
    test_server:start_node(Name,
			   slave,
			   [{args, "-pa " ++ PA ++ " " ++ Args}]).

stop_node(Node) ->
    test_server:stop_node(Node).
