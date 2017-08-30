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
-module(calendar_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 gregorian_days/1,
	 gregorian_seconds/1,
	 day_of_the_week/1,
	 day_of_the_week_calibrate/1,
	 leap_years/1,
	 last_day_of_the_month/1,
	 local_time_to_universal_time_dst/1,
	 iso_week_number/1]).

-define(START_YEAR, 1947).			
-define(END_YEAR, 2012).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [gregorian_days, gregorian_seconds, day_of_the_week,
     day_of_the_week_calibrate, leap_years,
     last_day_of_the_month, local_time_to_universal_time_dst, iso_week_number].

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

%%
%% LOCAL FUNCTIONS
%%

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
