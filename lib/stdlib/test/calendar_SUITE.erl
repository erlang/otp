%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2019. All Rights Reserved.
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
-module(calendar_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 gregorian_days/1,
	 big_gregorian_days/1,
	 gregorian_seconds/1,
	 day_of_the_week/1,
	 day_of_the_week_calibrate/1,
	 leap_years/1,
	 last_day_of_the_month/1,
	 local_time_to_universal_time_dst/1,
	 iso_week_number/1,
         system_time/1, rfc3339/1]).

-define(START_YEAR, 1947).			
-define(END_YEAR, 2012).

-define(BIG_START_YEAR, 20000000).
-define(BIG_END_YEAR, 20000020).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [gregorian_days, gregorian_seconds, day_of_the_week,
     day_of_the_week_calibrate, leap_years,
     last_day_of_the_month, local_time_to_universal_time_dst,
     iso_week_number, system_time, rfc3339, big_gregorian_days].

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

%% Tests that date_to_gregorian_days and gregorian_days_to_date
%% are each others inverses from ?START_YEAR-01-01 up to ?END_YEAR-01-01.
%% At the same time valid_date is tested.
gregorian_days(Config) when is_list(Config) ->
    Days = calendar:date_to_gregorian_days({?START_YEAR, 1, 1}),
    MaxDays = calendar:date_to_gregorian_days({?END_YEAR, 1, 1}),
    check_gregorian_days(Days, MaxDays).

%% Tests that date_to_gregorian_days and gregorian_days_to_date
%% are each others inverses from ?BIG_START_YEAR-01-01 up to ?BIG_END_YEAR-01-01.
%% At the same time valid_date is tested.
big_gregorian_days(Config) when is_list(Config) ->
    Days = calendar:date_to_gregorian_days({?BIG_START_YEAR, 1, 1}),
    MaxDays = calendar:date_to_gregorian_days({?BIG_END_YEAR, 1, 1}),
    check_gregorian_days(Days, MaxDays).

%% Tests that datetime_to_gregorian_seconds and
%% gregorian_seconds_to_date are each others inverses for a sampled
%% number of seconds from ?START_YEAR-01-01 up to ?END_YEAR-01-01: We check
%% every 2 days + 1 second.
gregorian_seconds(Config) when is_list(Config) ->
    Secs = calendar:datetime_to_gregorian_seconds({{?START_YEAR, 1, 1},
						   {0, 0, 0}}),
    MaxSecs = calendar:datetime_to_gregorian_seconds({{?END_YEAR, 1, 1},
						      {0, 0, 0}}),
    check_gregorian_seconds(Secs, MaxSecs).

%% Tests that day_of_the_week reports correctly the day of the week from
%% year ?START_YEAR up to ?END_YEAR.
day_of_the_week(Config) when is_list(Config) ->
    Days = calendar:date_to_gregorian_days({?START_YEAR, 1, 1}),
    MaxDays = calendar:date_to_gregorian_days({?END_YEAR, 1, 1}),
    DayNumber = calendar:day_of_the_week({?START_YEAR, 1, 1}),
    check_day_of_the_week(Days, MaxDays, DayNumber).

%% Tests that day_of_the_week for 1997-11-11 is Tuesday (2).
day_of_the_week_calibrate(Config) when is_list(Config) ->
    2 = calendar:day_of_the_week({1997, 11, 11}).

%% Tests that is_leap_year reports correctly the leap years from
%% year ?START_YEAR up to ?END_YEAR.
leap_years(Config) when is_list(Config) ->
    check_leap_years(?START_YEAR, ?END_YEAR).

%% Tests that last_day_of_the_month reports correctly from
%% year ?START_YEAR up to ?END_YEAR.
last_day_of_the_month(Config) when is_list(Config) ->
    check_last_day_of_the_month({?START_YEAR, 1}, {?END_YEAR, 1}).

%% Tests local_time_to_universal_time_dst for MET.
local_time_to_universal_time_dst(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} ->
	    case os:cmd("date '+%Z'") of
		"SAST"++_ ->
		    {comment, "Spoky time zone with zero-set DST, skipped"};
		_ ->
		    local_time_to_universal_time_dst_x(Config)
	    end;
	_ ->
	    local_time_to_universal_time_dst_x(Config)
    end.
local_time_to_universal_time_dst_x(Config) when is_list(Config) ->
    %% Assumes MET (UTC+1 / UTC+2(dst)
    LtW   = {{2003,01,15},{14,00,00}}, % Winter
    UtW   = {{2003,01,15},{13,00,00}}, %
    UtWd  = {{2003,01,15},{12,00,00}}, % dst
    LtS   = {{2003,07,15},{14,00,00}}, % Summer
    UtS   = {{2003,07,15},{13,00,00}}, %
    UtSd  = {{2003,07,15},{12,00,00}}, % dst
    LtWS  = {{2003,03,30},{02,30,00}}, % Winter->Summer
    UtWS  = {{2003,03,30},{01,30,00}}, %
    UtWSd = {{2003,03,30},{00,30,00}}, % dst
    LtSW  = {{2003,10,26},{02,30,00}}, % Summer->Winter
    UtSW  = {{2003,10,26},{01,30,00}}, %
    UtSWd = {{2003,10,26},{00,30,00}}, % dst
    %%
    UtW   = calendar:local_time_to_universal_time(LtW, false),
    UtWd  = calendar:local_time_to_universal_time(LtW, true),
    UtW   = calendar:local_time_to_universal_time(LtW, undefined),
    %%
    UtS   = calendar:local_time_to_universal_time(LtS, false),
    UtSd  = calendar:local_time_to_universal_time(LtS, true),
    UtSd  = calendar:local_time_to_universal_time(LtS, undefined),
    %%
    case calendar:local_time_to_universal_time(LtWS, false) of
	UtWS ->
	    UtWSd = calendar:local_time_to_universal_time(LtWS, true),
	    []    = calendar:local_time_to_universal_time_dst(LtWS),
	    %%
	    UtSW  = calendar:local_time_to_universal_time(LtSW, false),
	    UtSWd = calendar:local_time_to_universal_time(LtSW, true),
	    [UtSWd, UtSW] = calendar:local_time_to_universal_time_dst(LtSW),
	    ok;
	{{1969,12,31},{23,59,59}} ->
	    %% It seems that Apple has no intention of fixing this bug in
	    %% Mac OS 10.3.9, and we have no intention of implementing a
	    %% workaround. 
	    {comment,"Bug in mktime() in this OS"}
    end.

%% Test the iso week number calculation for all three possibilities:
%%  When the date falls on the last week of the previous year,
%%  when the date falls on a week within the given year and finally,
%%  when the date falls on the first week of the next year.
iso_week_number(Config) when is_list(Config) ->
    check_iso_week_number().

system_time(Config) when is_list(Config) ->
    EpochDate = {{1970,1,1}, {0,0,0}},
    Epoch = calendar:datetime_to_gregorian_seconds(EpochDate),
    Y0 = {{0,1,1},{0,0,0}},

    EpochDate = calendar:system_time_to_universal_time(0, second),
    0 = calendar:datetime_to_gregorian_seconds(Y0),
    Y0 = calendar:system_time_to_universal_time(-Epoch, second),

    T = erlang:system_time(second),
    UDate = calendar:system_time_to_universal_time(T, second),
    LDate = erlang:universaltime_to_localtime(UDate),
    LDate = calendar:system_time_to_local_time(T, second),

    ok.

rfc3339(Config) when is_list(Config) ->
    Ms = [{unit, millisecond}],
    Mys = [{unit, microsecond}],
    Ns = [{unit, nanosecond}],
    S = [{unit, second}],
    D = [{time_designator, $\s}],
    Z = [{offset, "Z"}],

    "1985-04-12T23:20:50.520Z" = test_parse("1985-04-12T23:20:50.52Z", Ms),
    "1985-04-12T23:20:50.520Z" = test_parse("1985-04-12t23:20:50.52z", Ms),
    "1985-04-12T21:20:50.520Z" =
        test_parse("1985-04-12T23:20:50.52+02:00", Ms),
    "1985-04-12T23:20:50Z" = test_parse("1985-04-12T23:20:50.52Z", S),
    "1985-04-12T23:20:50.520Z" = test_parse("1985-04-12T23:20:50.52Z", Ms),
    "1985-04-12T23:20:50.520000Z" =
        test_parse("1985-04-12t23:20:50.52z", Mys),
    "1985-04-12 21:20:50.520000000Z" =
        test_parse("1985-04-12 23:20:50.52+02:00", Ns++D),
    "1985-04-12T23:20:50Z" = test_parse("1985-04-12T23:20:50.52Z"),
    "1996-12-20T00:39:57Z" = test_parse("1996-12-19T16:39:57-08:00"),
    "1991-01-01T00:00:00Z" = test_parse("1990-12-31T23:59:60Z"),
    "1991-01-01T08:00:00Z" = test_parse("1990-12-31T23:59:60-08:00"),

    "1996-12-20T00:39:57Z" = test_parse("1996-12-19T16:39:57-08:00"),
    %% The leap second is not handled:
    "1991-01-01T00:00:00Z" = test_parse("1990-12-31T23:59:60Z"),

    "9999-12-31T23:59:59Z" = do_format_z(253402300799, []),
    "9999-12-31T23:59:59.999Z" = do_format_z(253402300799*1000+999, Ms),
    "9999-12-31T23:59:59.999999Z" =
        do_format_z(253402300799*1000000+999999, Mys),
    "9999-12-31T23:59:59.999999999Z" =
        do_format_z(253402300799*1000000000+999999999, Ns),
    {'EXIT', _} = (catch do_format_z(253402300799+1, [])),
    {'EXIT', _} = (catch do_parse("9999-12-31T23:59:60Z", [])),
    {'EXIT', _} = (catch do_format_z(253402300799*1000000000+999999999+1, Ns)),
    253402300799 = do_parse("9999-12-31T23:59:59Z", []),

    "0000-01-01T00:00:00Z" = test_parse("0000-01-01T00:00:00.0+00:00"),
    "9999-12-31T00:00:00Z" = test_parse("9999-12-31T00:00:00.0+00:00"),
    "1584-03-04T00:00:00Z" = test_parse("1584-03-04T00:00:00.0+00:00"),
    "1900-01-01T00:00:00Z" = test_parse("1900-01-01T00:00:00.0+00:00"),
    "2016-01-24T00:00:00Z" = test_parse("2016-01-24T00:00:00.0+00:00"),
    "1970-01-01T00:00:00Z" = test_parse("1970-01-01T00:00:00Z"),
    "1970-01-02T00:00:00Z" = test_parse("1970-01-01T23:59:60Z"),
    "1970-01-02T00:00:00Z" = test_parse("1970-01-01T23:59:60.5Z"),
    "1970-01-02T00:00:00Z" = test_parse("1970-01-01T23:59:60.55Z"),
    "1970-01-02T00:00:00.550Z" = test_parse("1970-01-01T23:59:60.55Z", Ms),
    "1970-01-02T00:00:00.550000Z" =
        test_parse("1970-01-01T23:59:60.55Z", Mys),
    "1970-01-02T00:00:00.550000000Z" =
        test_parse("1970-01-01T23:59:60.55Z", Ns),
    "1970-01-02T00:00:00.999999Z" =
        test_parse("1970-01-01T23:59:60.999999Z", Mys),
    "1970-01-02T00:00:01.000Z" =
        test_parse("1970-01-01T23:59:60.999999Z", Ms),
    "1970-01-01T00:00:00Z" = test_parse("1970-01-01T00:00:00+00:00"),
    "1970-01-01T00:00:00Z" = test_parse("1970-01-01T00:00:00-00:00"),
    "1969-12-31T00:01:00Z" = test_parse("1970-01-01T00:00:00+23:59"),
    "1918-11-11T09:00:00.000000Z" =
        test_parse("1918-11-11T11:00:00+02:00", Mys),
    "1970-01-01T00:00:00.000001Z" =
        test_parse("1970-01-01T00:00:00.000001Z", Mys),

    test_time(erlang:system_time(second), []),
    test_time(erlang:system_time(second), Z),
    test_time(erlang:system_time(second), Z ++ S),
    test_time(erlang:system_time(second), [{offset, "+02:20"}]),
    test_time(erlang:system_time(millisecond), Ms),
    test_time(erlang:system_time(microsecond), Mys++[{offset, "-02:20"}]),

    946720800 = TO = do_parse("2000-01-01 10:00:00Z", []),
    Str = "2000-01-01T10:02:00+00:02",
    Str = do_format(TO, [{offset, 120}]),
    "2000-01-01T10:02:00.000+00:02" =
        do_format(TO * 1000, [{offset, 120 * 1000}]++Ms),
    "2000-01-01T10:02:00.000000+00:02" =
        do_format(TO * 1000 * 1000, [{offset, 120 * 1000 * 1000}]++Mys),
    "2000-01-01T10:02:00.000000000+00:02" =
        do_format(TO * 1000 * 1000 * 1000,
                  [{offset, 120 * 1000 * 1000 * 1000}]++Ns),

    NStr = "2000-01-01T09:58:00-00:02",
    NStr = do_format(TO, [{offset, -120}]),
    "2000-01-01T09:58:00.000-00:02" =
        do_format(TO * 1000, [{offset, -120 * 1000}]++Ms),
    "2000-01-01T09:58:00.000000-00:02" =
        do_format(TO * 1000 * 1000, [{offset, -120 * 1000 * 1000}]++Mys),
    "2000-01-01T09:58:00.000000000-00:02" =
        do_format(TO * 1000 * 1000 * 1000,
                  [{offset, -120 * 1000 * 1000 * 1000}]++Ns),

    543210000 = do_parse("1970-01-01T00:00:00.54321Z", Ns),
    54321000 = do_parse("1970-01-01T00:00:00.054321Z", Ns),
    543210 = do_parse("1970-01-01T00:00:00.54321Z", Mys),
    543 = do_parse("1970-01-01T00:00:00.54321Z", Ms),
    0 = do_parse("1970-01-01T00:00:00.000001Z", Ms),
    1 = do_parse("1970-01-01T00:00:00.000001Z", Mys),
    1000 = do_parse("1970-01-01T00:00:00.000001Z", Ns),
    0 = do_parse("1970-01-01Q00:00:00.00049Z", Ms),
    1 = do_parse("1970-01-01Q00:00:00.0005Z", Ms),
    6543210 = do_parse("1970-01-01T00:00:06.54321Z", Mys),
    298815132000000 = do_parse("1979-06-21T12:12:12Z", Mys),
    -1613826000000000 = do_parse("1918-11-11T11:00:00Z", Mys),
    -1613833200000000 = do_parse("1918-11-11T11:00:00+02:00", Mys),
    -1613833200000000 = do_parse("1918-11-11T09:00:00Z", Mys),

    "1970-01-01T00:00:00.000000Z" = do_format_z(0, Mys),
    "1970-01-01T00:00:01Z" = do_format_z(1, S),
    "1970-01-01T00:00:00.001Z" = do_format_z(1, Ms),
    "1970-01-01T00:00:00.000001Z" = do_format_z(1, Mys),
    "1970-01-01T00:00:00.000000001Z" = do_format_z(1, Ns),
    "1970-01-01T00:00:01.000000Z" = do_format_z(1000000, Mys),
    "1970-01-01T00:00:00.543210Z" = do_format_z(543210, Mys),
    "1970-01-01T00:00:00.543Z" = do_format_z(543, Ms),
    "1970-01-01T00:00:00.543210000Z" = do_format_z(543210000, Ns),
    "1970-01-01T00:00:06.543210Z" = do_format_z(6543210, Mys),
    "1979-06-21T12:12:12.000000Z" = do_format_z(298815132000000, Mys),
    "1918-11-11T13:00:00.000000Z" = do_format_z(-1613818800000000, Mys),
    ok.

%%
%% LOCAL FUNCTIONS
%%

test_parse(String) ->
    test_parse(String, []).

test_parse(String, Options) ->
    T = do_parse(String, Options),
    calendar:system_time_to_rfc3339(T, [{offset, "Z"} | Options]).

do_parse(String, Options) ->
    calendar:rfc3339_to_system_time(String, Options).

test_time(Time, Options) ->
    F = calendar:system_time_to_rfc3339(Time, Options),
    Time = calendar:rfc3339_to_system_time(F, Options).

do_format_z(Time, Options) ->
    do_format(Time, [{offset, "Z"}|Options]).

do_format(Time, Options) ->
    calendar:system_time_to_rfc3339(Time, Options).

%% check_gregorian_days
%% 
check_gregorian_days(Days, MaxDays) when Days < MaxDays ->
    Date = calendar:gregorian_days_to_date(Days),
    true = calendar:valid_date(Date),
    Days = calendar:date_to_gregorian_days(Date),
    check_gregorian_days(Days + 1, MaxDays);
check_gregorian_days(_Days, _MaxDays) ->
    ok.

%% check_gregorian_seconds
%% 
%% We increment with something prime (172801 = 2 days + 1 second).
%%
check_gregorian_seconds(Secs, MaxSecs) when Secs < MaxSecs ->
    DateTime = calendar:gregorian_seconds_to_datetime(Secs),
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    check_gregorian_seconds(Secs + 172801, MaxSecs);
check_gregorian_seconds(_Secs, _MaxSecs) ->
    ok.


%% check_day_of_the_week
%%
check_day_of_the_week(Days, MaxDays, DayNumber) when Days < MaxDays ->
    Date = calendar:gregorian_days_to_date(Days),
    DayNumber = calendar:day_of_the_week(Date),
    check_day_of_the_week(Days + 1, MaxDays,
			  ((DayNumber rem 7) + 1));
check_day_of_the_week(_Days, _MaxDays, _DayNumber) ->
    ok.

%% check_leap_years
%%
%% SYr must be larger than 1800, and EYr must be less than ?END_YEAR.
%%
check_leap_years(SYr, EYr) when SYr < EYr ->
    Rem = SYr rem 4,
    case Rem of
	0 ->
	    case SYr of
		1900 ->
		    false = calendar:is_leap_year(SYr);
		2000 ->
		    true = calendar:is_leap_year(SYr);
		_  ->
		    true = calendar:is_leap_year(SYr)
	    end;
	_ ->
	    false = calendar:is_leap_year(SYr)
    end,
    check_leap_years(SYr + 1, EYr);
check_leap_years(_SYr, _EYr) ->
    ok.

check_last_day_of_the_month({SYr, SMon}, {EYr, EMon}) when SYr < EYr ->
    LastDay = calendar:last_day_of_the_month(SYr, SMon),
    LastDay = case SMon of
		  1 -> 31;
		  2 ->
		      case calendar:is_leap_year(SYr) of
			  true -> 29;
			  false  -> 28
		      end;
		  3 -> 31;
		  4 -> 30;
		  5 -> 31;
		  6 -> 30;
		  7 -> 31;
		  8 -> 31;
		  9 -> 30;
		  10 -> 31;
		  11 -> 30;
		  12 -> 31
	      end,
    NYr = case SMon of
	      12 -> SYr + 1;
	      _ -> SYr
	  end,
    check_last_day_of_the_month({NYr, (SMon rem 12) + 1},
				{EYr, EMon});
check_last_day_of_the_month(_, _) ->
    ok.

%% check_iso_week_number
%%
check_iso_week_number() ->
    {2004, 53} = calendar:iso_week_number({2005, 1, 1}),
    {2007, 1} = calendar:iso_week_number({2007, 1, 1}),
    {2009, 1} = calendar:iso_week_number({2008, 12, 29}).
