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

-module(time_SUITE).

%% "Time is on my side." -- The Rolling Stones

%% Tests the BIFs:
%%	erlang:localtime_to_universaltime/1
%%	erlang:universaltime_to_localtime/1
%%	date/0
%%	time/0
%%	now/0
%%

-export([all/1, univ_to_local/1, local_to_univ/1,
	 bad_univ_to_local/1, bad_local_to_univ/1,
	 consistency/1,
	 now/1, now_unique/1, now_update/1, timestamp/1]).

-export([local_to_univ_utc/1]).

-include("test_server.hrl").

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

all(suite) -> [univ_to_local, local_to_univ,
	       local_to_univ_utc,
	       bad_univ_to_local, bad_local_to_univ,
	       consistency, now, timestamp].

local_to_univ_utc(suite) ->
    [];
local_to_univ_utc(doc) ->
    ["Test that DST = true on timezones without DST is ignored"];
local_to_univ_utc(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} ->
	    %% TZ variable has a meaning
	    ?line {ok, Node} =
		test_server:start_node(local_univ_utc,peer,
				       [{args, "-env TZ UTC"}]),
	    ?line {{2008,8,1},{0,0,0}} =
		rpc:call(Node,
			 erlang,localtime_to_universaltime,
			 [{{2008, 8, 1}, {0, 0, 0}},
			  false]),
	    ?line {{2008,8,1},{0,0,0}} =
		rpc:call(Node,
			 erlang,localtime_to_universaltime,
			 [{{2008, 8, 1}, {0, 0, 0}},
			  true]),
	    ?line [{{2008,8,1},{0,0,0}}] =
		rpc:call(Node,
			 calendar,local_time_to_universal_time_dst,
			 [{{2008, 8, 1}, {0, 0, 0}}]),
	    ?line test_server:stop_node(Node),
	    ok;
	_ ->
	    {skip,"Only valid on Unix"}
    end.


%% Tests conversion from univeral to local time.

univ_to_local(Config) when is_list(Config) ->
    ?line test_univ_to_local(test_data()).

test_univ_to_local([{Utc, Local}|Rest]) ->
    ?line io:format("Testing ~p => ~p~n", [Local, Utc]),
    ?line Local = erlang:universaltime_to_localtime(Utc),
    ?line test_univ_to_local(Rest);
test_univ_to_local([]) ->
    ok.

%% Tests conversion from local to universal time.

local_to_univ(Config) when is_list(Config) ->
    ?line test_local_to_univ(test_data()).

test_local_to_univ([{Utc, Local}|Rest]) ->
    ?line io:format("Testing ~p => ~p~n", [Utc, Local]),
    ?line Utc = erlang:localtime_to_universaltime(Local),
    ?line test_local_to_univ(Rest);
test_local_to_univ([]) ->
    ok.

%% Test bad arguments to erlang:universaltime_to_localtime; should
%% generate a badarg.

bad_univ_to_local(Config) when is_list(Config) ->
    ?line bad_test_univ_to_local(bad_dates()).

bad_test_univ_to_local([Utc|Rest]) ->
    ?line io:format("Testing ~p~n", [Utc]),
    ?line case catch erlang:universaltime_to_localtime(Utc) of
	      {'EXIT', {badarg, _}} -> bad_test_univ_to_local(Rest)
	  end;
bad_test_univ_to_local([]) ->
    ok.

%% Test bad arguments to erlang:localtime_to_universaltime/1; should
%% generate a badarg.

bad_local_to_univ(Config) when is_list(Config) ->
    ?line bad_test_local_to_univ(bad_dates()).

bad_test_local_to_univ([Local|Rest]) ->
    ?line io:format("Testing ~p~n", [Local]),
    ?line case catch erlang:localtime_to_universaltime(Local) of
	      {'EXIT', {badarg, _}} -> bad_test_local_to_univ(Rest)
	  end;
bad_test_local_to_univ([]) ->
    ok.

%% Test that the the different time functions return
%% consistent results. (See the test case for assumptions
%% and limitations.)
consistency(Config) when is_list(Config) ->
    %% Test the following equations:
    %% 		date() & time() == erlang:localtime()
    %% 		erlang:universaltime() + timezone == erlang:localtime()
    %%
    %% Assumptions:
    %% 		Middle-European time zone, EU rules for daylight-saving time.
    %%
    %% Limitations:
    %% 		Localtime and universaltime must be in the same	month.
    %%	        Daylight-saving calculations are incorrect from the last
    %%		Sunday of March and October to the end of the month.

    ?line ok = compare_date_time_and_localtime(16),
    ?line ok = compare_local_and_universal(16).

compare_date_time_and_localtime(Times) when Times > 0 ->
    ?line {Year, Mon, Day} = date(),
    ?line {Hour, Min, Sec} = time(),
    ?line case erlang:localtime() of
	{{Year, Mon, Day}, {Hour, Min, Sec}} -> ok;
	_ -> compare_date_time_and_localtime(Times-1)
    end;
compare_date_time_and_localtime(0) ->
    error.

compare_local_and_universal(Times) when Times > 0 ->
    case compare(erlang:universaltime(), erlang:localtime()) of
	true -> ok;
	false -> compare_local_and_universal(Times-1)
    end;
compare_local_and_universal(0) ->
    error.

compare(Utc0, Local) ->
    io:format("local = ~p, utc = ~p", [Local, Utc0]),
    Utc = linear_time(Utc0)+effective_timezone(Utc0)*3600,
    case linear_time(Local) of
	Utc -> true;
	Other ->
	    io:format("Failed: local = ~p, utc = ~p~n",
		      [Other, Utc]),
	    false
    end.

%% This function converts a date and time to a linear time.
%% Two linear times can be subtracted to give their difference
%% in seconds.
%%
%% XXX Limitations: The length of months and leap years are not
%% taken into account; thus a comparision of dates is only
%% valid if they are in the SAME month.

linear_time({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    86400*(366*Year + 31*(Mon-1) + (Day-1)) +
	3600*Hour + 60*Min + Sec.

%% This functions returns either the normal timezone or the
%% the DST timezone, depending on the given UTC time.
%%
%% XXX This function uses an approximation of the EU rule for
%% daylight saving time.  This function will fail in the
%% following intervals: After the last Sunday in March upto
%% the end of March, and after the last Sunday in October
%% upto the end of October.

effective_timezone(Time) ->
    case os:type() of
	{unix,_} ->
	    case os:cmd("date '+%Z'") of
		"SAST"++_ ->
		    2;
		_ ->
		    effective_timezone1(Time)
	    end;
	_ ->
	    effective_timezone1(Time)
    end.

effective_timezone1({{_Year,Mon,_Day}, _}) when Mon < 4 ->
    ?timezone;
effective_timezone1({{_Year,Mon,_Day}, _}) when Mon > 10 ->
    ?timezone;
effective_timezone1(_) ->
    ?dst_timezone.

%% Test (the bif) os:timestamp/0, which is something quite like, but not
%% similar to erlang:now...

timestamp(suite) ->
    [];
timestamp(doc) ->
    ["Test that os:timestamp works."];
timestamp(Config) when is_list(Config) ->
    repeating_timestamp_check(100000).

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
	    test_server:fail(
	      lists:flatten(
		io_lib:format("Strange return from os:timestamp/0 ~w~n",[TS])))
    end,
    %% I assume the now and timestamp should not differ more than 1 hour,
    %% which is safe assuming the system has not had a large time-warp
    %% during the testrun...
    Secs = A*1000000+B+round(C/1000000),
    {NA,NB,NC} = erlang:now(),
    NSecs = NA*1000000+NB+round(NC/1000000),
    case Secs - NSecs of
	TooLarge when TooLarge > 3600 ->
	    test_server:fail(
	      lists:flatten(
		io_lib:format("os:timestamp/0 is ~w s more than erlang:now/0",
			     [TooLarge])));
	TooSmall when TooSmall < -3600 ->
	     test_server:fail(
	      lists:flatten(
		io_lib:format("os:timestamp/0 is ~w s less than erlang:now/0",
			     [-TooSmall])));
	_ ->
	    ok
    end,
    repeating_timestamp_check(N-1).
	    

%% Test now/0.

now(suite) -> [now_unique, now_update].

%% Tests that successive calls to now/0 returns different values.
%% Also returns a comment string with the median difference between
%% times (in microseconds).

now_unique(Config) when is_list(Config) ->
    ?line now_unique(1000, now(), []),
    ?line fast_now_unique(100000, now()).

now_unique(N, Previous, Result) when N > 0 ->
    ?line case now() of
	      Previous ->
		  test_server:fail("now/0 returned the same value twice");
	      New ->
		  now_unique(N-1, New, [New|Result])
	  end;
now_unique(0, _, [Then|Rest]) ->
    ?line now_calc_increment(Rest, microsecs(Then), []).

now_calc_increment([Then|Rest], Previous, _Result) ->
    ?line This = microsecs(Then),
    ?line now_calc_increment(Rest, This, [Previous-This]);
now_calc_increment([], _, Differences) ->
    {comment, "Median increment: " ++ integer_to_list(median(Differences))}.

fast_now_unique(0, _) -> ok;
fast_now_unique(N, Then) ->
    case now() of
	Then ->
	    ?line ?t:fail("now/0 returned the same value twice");
	Now ->
	    fast_now_unique(N-1, Now)
    end.

median(Unsorted_List) ->
    ?line Length = length(Unsorted_List),
    ?line List = lists:sort(Unsorted_List),
    ?line case Length rem 2 of
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
    case ?t:is_debug() of
	false -> ?line now_update1(10);
	true -> {skip,"Unreliable in DEBUG build"}
    end.


now_update1(N) when N > 0 ->
    ?line T1_linear = linear_time(erlang:localtime()),
    ?line T1_now = microsecs(now()),

    ?line receive after 1008 -> ok end,

    ?line T2_linear = linear_time(erlang:localtime()),
    ?line T2_now = microsecs(now()),

    ?line Linear_Diff = (T2_linear-T1_linear)*1000000,
    ?line Now_Diff = T2_now-T1_now,
    test_server:format("Localtime diff = ~p; now() diff = ~p",
		       [Linear_Diff, Now_Diff]),
    ?line case abs(Linear_Diff - Now_Diff) of
	      Abs_Delta when Abs_Delta =< 40000 -> ok;
	      _ -> now_update1(N-1)
	  end;
now_update1(0) ->
    ?line test_server:fail().

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
    ?line test_data(nondst_dates(), TZ) ++
	test_data(dst_dates(), DSTTZ) ++
	crossover_test_data(crossover_dates(), TZ).    


%% test_data1() ->
%%     ?line test_data(nondst_dates(), ?timezone) ++
%% 	test_data(dst_dates(), ?dst_timezone) ++
%% 	crossover_test_data(crossover_dates(), ?timezone).

crossover_test_data([{Year, Month, Day}|Rest], TimeZone) when TimeZone > 0 ->
    Hour = 23,
    Min = 35,
    Sec = 55,
    ?line Utc = {{Year, Month, Day}, {Hour, Min, Sec}},
    ?line Local = {{Year, Month, Day+1}, {Hour+TimeZone-24, Min, Sec}},
    ?line [{Utc, Local}|crossover_test_data(Rest, TimeZone)];
crossover_test_data([{Year, Month, Day}|Rest], TimeZone) when TimeZone < 0 ->
    Hour = 0,
    Min = 23,
    Sec = 12,
    ?line Utc = {{Year, Month, Day}, {Hour, Min, Sec}},
    ?line Local = {{Year, Month, Day-1}, {Hour+TimeZone+24, Min, Sec}},
    ?line [{Utc, Local}|crossover_test_data(Rest, TimeZone)];
crossover_test_data([], _) ->
    [].

test_data([Date|Rest], TimeZone) ->
    Hour = 12,
    Min = 45,
    Sec = 7,
    ?line Utc = {Date, {Hour, Min, Sec}},
    ?line Local = {Date, {Hour+TimeZone, Min, Sec}},
    ?line [{Utc, Local}|test_data(Rest, TimeZone)];
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
     
