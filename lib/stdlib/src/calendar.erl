%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(calendar).
-moduledoc """
Local and universal time, day of the week, date and time conversions.

This module provides computation of local and universal time, day of the week,
and many time conversion functions.

Time is local when it is adjusted in accordance with the current time zone and
daylight saving. Time is universal when it reflects the time at longitude zero,
without any adjustment for daylight saving. Universal Coordinated Time (UTC)
time is also called Greenwich Mean Time (GMT).

The time functions `local_time/0` and `universal_time/0` in this module both
return date and time. This is because separate functions for date and time can
result in a date/time combination that is displaced by 24 hours. This occurs if
one of the functions is called before midnight, and the other after midnight.
This problem also applies to the Erlang BIFs `date/0` and `time/0`, and their
use is strongly discouraged if a reliable date/time stamp is required.

All dates conform to the Gregorian calendar. This calendar was introduced by
Pope Gregory XIII in 1582 and was used in all Catholic countries from this year.
Protestant parts of Germany and the Netherlands adopted it in 1698, England
followed in 1752, and Russia in 1918 (the October revolution of 1917 took place
in November according to the Gregorian calendar).

The Gregorian calendar in this module is extended back to year 0. For a given
date, the _gregorian days_ is the number of days up to and including the date
specified. Similarly, the _gregorian seconds_ for a specified date and time is
the number of seconds up to and including the specified date and time.

For computing differences between epochs in time, use the functions counting
gregorian days or seconds. If epochs are specified as local time, they must be
converted to universal time to get the correct value of the elapsed time between
epochs. Use of function [`time_difference/2`](`time_difference/2`) is
discouraged.

Different definitions exist for the week of the year. This module contains a
week of the year implementation conforming to the ISO 8601 standard. As the week
number for a specified date can fall on the previous, the current, or on the
next year, it is important to specify both the year and the week number.
Functions `iso_week_number/0` and [`iso_week_number/1`](`iso_week_number/1`)
return a tuple of the year and the week number.

## Leap Years

The notion that every fourth year is a leap year is not completely true. By the
Gregorian rule, a year Y is a leap year if one of the following rules is valid:

- Y is divisible by 4, but not by 100.
- Y is divisible by 400.

Hence, 1996 is a leap year, 1900 is not, but 2000 is.

## Date and Time Source

Local time is obtained from the Erlang BIF `localtime/0`. Universal time is
computed from the BIF `universaltime/0`.

The following apply:

- There are 86400 seconds in a day.
- There are 365 days in an ordinary year.
- There are 366 days in a leap year.
- There are 1461 days in a 4 year period.
- There are 36524 days in a 100 year period.
- There are 146097 days in a 400 year period.
- There are 719528 days between Jan 1, 0 and Jan 1, 1970.
""".

%% local and universal time, time conversions

-export([date_to_gregorian_days/1, 
	 date_to_gregorian_days/3,
	 datetime_to_gregorian_seconds/1,
	 day_of_the_week/1,
	 day_of_the_week/3,
	 gregorian_days_to_date/1,
	 gregorian_seconds_to_datetime/1,
	 is_leap_year/1,
	 iso_week_number/0,
	 iso_week_number/1,
	 last_day_of_the_month/2,
	 local_time/0,
         local_time_to_system_time/1,
         local_time_to_system_time/2,
	 local_time_to_universal_time/1, 
	 local_time_to_universal_time/2, 
	 local_time_to_universal_time_dst/1, 
	 now_to_datetime/1,			% = now_to_universal_time/1
	 now_to_local_time/1,
	 now_to_universal_time/1,
         rfc3339_to_system_time/1,
         rfc3339_to_system_time/2,
	 seconds_to_daystime/1,
	 seconds_to_time/1,
         system_time_to_local_time/2,
         system_time_to_universal_time/2,
         system_time_to_rfc3339/1,
         system_time_to_rfc3339/2,
	 time_difference/2,
	 time_to_seconds/1,
	 universal_time/0,
	 universal_time_to_local_time/1,
         universal_time_to_system_time/1,
         universal_time_to_system_time/2,
	 valid_date/1,
	 valid_date/3]).

-deprecated([{local_time_to_universal_time,1,
              "use calendar:local_time_to_universal_time_dst/1 instead"}]).

-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(DAYS_PER_YEAR, 365).
-define(DAYS_PER_LEAP_YEAR, 366).
%% -define(DAYS_PER_4YEARS, 1461).
%% -define(DAYS_PER_100YEARS, 36524).
%% -define(DAYS_PER_400YEARS, 146097).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(DAYS_FROM_0_TO_10000, 2932897).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY)).
-define(SECONDS_FROM_0_TO_10000, (?DAYS_FROM_0_TO_10000*?SECONDS_PER_DAY)).

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-export_type([date/0, time/0, datetime/0, datetime1970/0]).

-doc """
The year using the Gregorian calendar.

Year cannot be abbreviated. For example, 93 denotes year 93, not 1993. The valid
range depends on the underlying operating system.
""".
-type year()     :: non_neg_integer().
-type year1970() :: 1970..10000.	% should probably be 1970..
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type daynum()   :: 1..7.
-doc "The last day of the month.".
-type ldom()     :: 28 | 29 | 30 | 31. % last day of month
-type weeknum()  :: 1..53.

-doc """
A date using the Gregorian calendar.

All APIs expect this to be a valid date. If the source of the date
is unknown, then verify that is it valid by calling `valid_date/1`
before using it.
""".
-type date()           :: {year(),month(),day()}.
-type time()           :: {hour(),minute(),second()}.
-type datetime()       :: {date(),time()}.
-type datetime1970()   :: {{year1970(),month(),day()},time()}.
-type yearweeknum()    :: {year(),weeknum()}.

-type rfc3339_string() :: [byte(), ...] | binary().
-doc """
The time unit used by the rfc3339 conversion functions.

> #### Note {: .info }
>
> The `native` time unit was added to `t:rfc3339_time_unit/0` in OTP 25.0.
""".
-type rfc3339_time_unit() :: 'microsecond'
                           | 'millisecond'
                           | 'nanosecond'
                           | 'second'
                           | 'native'.

%%----------------------------------------------------------------------

%% All dates are according the the Gregorian calendar. In this module
%% the Gregorian calendar is extended back to year 0 for convenience.
%%
%% A year Y is a leap year if and only if either
%%
%%        (1)    Y is divisible by 4, but not by 100, or
%%        (2)    Y is divisible by 400.
%%
%% Hence, e.g. 1996 is a leap year, 1900 is not, but 2000 is.
%%

%%
%% EXPORTS
%%

%% date_to_gregorian_days(Year, Month, Day) = Integer
%% date_to_gregorian_days({Year, Month, Day}) = Integer
%%
%% Computes the total number of days starting from year 0,
%% January 1st.
%%
%% df/2 catches the case Year<0
-doc(#{equiv => date_to_gregorian_days({Year, Month, Day})}).
-spec date_to_gregorian_days(Year, Month, Day) -> Days when
      Year :: year(),
      Month :: month(),
      Day :: day(),
      Days :: non_neg_integer().
date_to_gregorian_days(Year, Month, Day) when is_integer(Day), Day > 0 ->
    Last = last_day_of_the_month(Year, Month),
    if
	Day =< Last ->
	    dy(Year) + dm(Month) + df(Year, Month) + Day - 1
    end.

-doc """
Computes the number of gregorian days starting with year 0 and ending at the
specified date.
""".
-spec date_to_gregorian_days(Date) -> Days when
      Date :: date(),
      Days :: non_neg_integer().
date_to_gregorian_days({Year, Month, Day}) ->
    date_to_gregorian_days(Year, Month, Day).


%% datetime_to_gregorian_seconds(DateTime) = Integer
%%
%% Computes the total number of seconds starting from year 0,
%% January 1st.
%%
-doc """
Computes the number of gregorian seconds starting with year 0 and ending at the
specified date and time.
""".
-spec datetime_to_gregorian_seconds(DateTime) -> Seconds when
      DateTime :: datetime(),
      Seconds :: non_neg_integer().
datetime_to_gregorian_seconds({Date, Time}) ->
    ?SECONDS_PER_DAY*date_to_gregorian_days(Date) +
	time_to_seconds(Time).


%% day_of_the_week(Year, Month, Day)
%% day_of_the_week({Year, Month, Day})
%%
%% Returns: 1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7.
%%
-doc(#{equiv => day_of_the_week({Year, Month, Day})}).
-spec day_of_the_week(Year, Month, Day) -> daynum() when
      Year :: year(),
      Month :: month(),
      Day :: day().
day_of_the_week(Year, Month, Day) ->
    (date_to_gregorian_days(Year, Month, Day) + 5) rem 7 + 1.

-doc """
Computes the day of the week from the specified `Year`, `Month`, and `Day`.
Returns the day of the week as `1`: Monday, `2`: Tuesday, and so on.
""".
-spec day_of_the_week(Date) -> daynum() when
      Date:: date().
day_of_the_week({Year, Month, Day}) ->
    day_of_the_week(Year, Month, Day).


%% gregorian_days_to_date(Days) = {Year, Month, Day}
%%
-doc "Computes the date from the specified number of gregorian days.".
-spec gregorian_days_to_date(Days) -> date() when
      Days :: non_neg_integer().
gregorian_days_to_date(Days) ->
    {Year, DayOfYear} = day_to_year(Days),
    {Month, DayOfMonth} = year_day_to_date(Year, DayOfYear),
    {Year, Month, DayOfMonth}.


%% gregorian_seconds_to_datetime(Secs)
%%
-doc "Computes the date and time from the specified number of gregorian seconds.".
-spec gregorian_seconds_to_datetime(Seconds) -> datetime() when
      Seconds :: non_neg_integer().
gregorian_seconds_to_datetime(Secs) when Secs >= 0 ->
    Days = Secs div ?SECONDS_PER_DAY,
    Rest = Secs rem ?SECONDS_PER_DAY,
    {gregorian_days_to_date(Days), seconds_to_time(Rest)}.


%% is_leap_year(Year) = true | false
%%
-doc "Checks if the specified year is a leap year.".
-spec is_leap_year(Year) -> boolean() when
      Year :: year().
is_leap_year(Y) when is_integer(Y), Y >= 0 ->
    is_leap_year1(Y).

-spec is_leap_year1(year()) -> boolean().
is_leap_year1(Year) when Year rem 4 =:= 0, Year rem 100 > 0 ->
    true;
is_leap_year1(Year) when Year rem 400 =:= 0 ->
    true;
is_leap_year1(_) -> false.


%%
%% Calculates the iso week number for the current date.
%%
-doc """
Returns tuple `{Year, WeekNum}` representing the ISO week number for the actual
date. To determine the actual date, use function `local_time/0`.
""".
-doc(#{since => <<"OTP R14B02">>}).
-spec iso_week_number() -> yearweeknum().
iso_week_number() ->
    {Date, _} = local_time(),
    iso_week_number(Date).


%%
%% Calculates the iso week number for the given date.
%%
-doc """
Returns tuple `{Year, WeekNum}` representing the ISO week number for the
specified date.
""".
-doc(#{since => <<"OTP R14B02">>}).
-spec iso_week_number(Date) -> yearweeknum() when
      Date :: date().
iso_week_number({Year, Month, Day}) ->
    D = date_to_gregorian_days({Year, Month, Day}),
    W01_1_Year = gregorian_days_of_iso_w01_1(Year),
    W01_1_NextYear = gregorian_days_of_iso_w01_1(Year + 1),
    if W01_1_Year =< D andalso D < W01_1_NextYear ->
	    % Current Year Week 01..52(,53)
	    {Year, (D - W01_1_Year) div 7 + 1};
	D < W01_1_Year ->
	    % Previous Year 52 or 53
	    PWN = case day_of_the_week(Year - 1, 1, 1) of
		4 -> 53;
		_ -> case day_of_the_week(Year - 1, 12, 31) of
			4 -> 53;
			_ -> 52
		     end
		end,
	    {Year - 1, PWN};
	W01_1_NextYear =< D ->
	    % Next Year, Week 01
	    {Year + 1, 1}
    end.


%% last_day_of_the_month(Year, Month)
%%
%% Returns the number of days in a month.
%%
-doc "Computes the number of days in a month.".
-spec last_day_of_the_month(Year, Month) -> LastDay when
      Year :: year(),
      Month :: month(),
      LastDay :: ldom().
last_day_of_the_month(Y, M) when is_integer(Y), Y >= 0 ->
    last_day_of_the_month1(Y, M).

-spec last_day_of_the_month1(year(),month()) -> ldom().
last_day_of_the_month1(_, 4) -> 30;
last_day_of_the_month1(_, 6) -> 30;
last_day_of_the_month1(_, 9) -> 30;
last_day_of_the_month1(_,11) -> 30;
last_day_of_the_month1(Y, 2) ->
   case is_leap_year(Y) of
      true -> 29;
      _    -> 28
   end;
last_day_of_the_month1(_, M) when is_integer(M), M > 0, M < 13 ->
    31.


%% local_time()
%%
%% Returns: {date(), time()}, date() = {Y, M, D}, time() = {H, M, S}.
-doc "Returns the local time reported by the underlying operating system.".
-spec local_time() -> datetime().
local_time() ->
    erlang:localtime().

-doc(#{equiv => local_time_to_system_time(LocalTime, [])}).
-doc(#{since => <<"OTP 28.0">>}).
-spec local_time_to_system_time(datetime1970()) -> pos_integer().
local_time_to_system_time(LocalTime) ->
    local_time_to_system_time(LocalTime, []).

-doc(#{since => <<"OTP 28.0">>}).
-doc """
Converts local time into system time.
Error will occur if the local time is non existing or ambiguous due to DST,
see [`calendar:local_time_to_universal_time_dst/1`](`local_time_to_universal_time_dst/1`).
""".
-spec local_time_to_system_time(datetime1970(), Options) -> pos_integer() when
      Options :: [Option],
      Option :: {unit, erlang:time_unit()}.
local_time_to_system_time(LocalTime, Options) ->
    case local_time_to_universal_time_dst(LocalTime) of
        [UniversalTime] ->
            universal_time_to_system_time(UniversalTime, Options);
        [] ->
            error({non_existing_local_time, LocalTime});
        [_, _] ->
            error({ambiguous_local_time, LocalTime})
    end.
        

%% local_time_to_universal_time(DateTime)
%%
-doc """
Converts from local time to Universal Coordinated Time (UTC). `DateTime1` must
refer to a local date after Jan 1, 1970.

> #### Warning {: .warning }
>
> This function is deprecated. Use `local_time_to_universal_time_dst/1` instead,
> as it gives a more correct and complete result. Especially for the period that
> does not exist, as it is skipped during the switch _to_ daylight saving time,
> this function still returns a result.
""".
-spec local_time_to_universal_time(DateTime1) -> DateTime2 when
      DateTime1 :: datetime1970(),
      DateTime2 :: datetime1970().
local_time_to_universal_time(DateTime) ->
    erlang:localtime_to_universaltime(DateTime).

-doc false.
-spec local_time_to_universal_time(datetime1970(),
				   'true' | 'false' | 'undefined') ->
                                          datetime1970().
local_time_to_universal_time(DateTime, IsDst) ->
    erlang:localtime_to_universaltime(DateTime, IsDst).

-doc """
Converts from local time to Universal Coordinated Time (UTC). `DateTime1` must
refer to a local date after Jan 1, 1970.

The return value is a list of 0, 1, or 2 possible UTC times:

- **`[]`** - For a local `{Date1, Time1}` during the period that is skipped when
  switching _to_ daylight saving time, there is no corresponding UTC, as the
  local time is illegal (it has never occured).

- **`[DstDateTimeUTC, DateTimeUTC]`** - For a local `{Date1, Time1}` during the
  period that is repeated when switching _from_ daylight saving time, two
  corresponding UTCs exist; one for the first instance of the period when
  daylight saving time is still active, and one for the second instance.

- **`[DateTimeUTC]`** - For all other local times only one corresponding UTC
  exists.
""".
-spec local_time_to_universal_time_dst(DateTime1) -> [DateTime] when
      DateTime1 :: datetime1970(),
      DateTime :: datetime1970().
local_time_to_universal_time_dst(DateTime) ->
    UtDst = erlang:localtime_to_universaltime(DateTime, true),
    Ut    = erlang:localtime_to_universaltime(DateTime, false),
    %% Reverse check the universal times
    LtDst = erlang:universaltime_to_localtime(UtDst),
    Lt    = erlang:universaltime_to_localtime(Ut),
    %% Return the valid universal times
    case {LtDst,Lt} of
	{DateTime,DateTime} when UtDst =/= Ut ->
	    [UtDst,Ut];
	{DateTime,_} ->
	    [UtDst];
	{_,DateTime} ->
	    [Ut];
	{_,_} ->
	    []
    end.

%% now_to_universal_time(Now)
%% now_to_datetime(Now)
%%
%% Convert from erlang:timestamp() to UTC.
%%
%% Args: Now = now(); now() = {MegaSec, Sec, MilliSec}, MegaSec = Sec
%% = MilliSec = integer() 
%% Returns: {date(), time()}, date() = {Y, M, D}, time() = {H, M, S}.
%% 
-doc """
Returns Universal Coordinated Time (UTC) converted from the return value from
`erlang:timestamp/0`.
""".
-spec now_to_datetime(Now) -> datetime1970() when
      Now :: erlang:timestamp().
now_to_datetime({MSec, Sec, _uSec}) ->
    system_time_to_datetime(MSec*1_000_000 + Sec).

-doc """
Returns Universal Coordinated Time (UTC) converted from the return value from
`erlang:timestamp/0`.
""".
-spec now_to_universal_time(Now) -> datetime1970() when
      Now :: erlang:timestamp().
now_to_universal_time(Now) ->
    now_to_datetime(Now).


%% now_to_local_time(Now)
%%
%% Args: Now = now()
%%
-doc """
Returns local date and time converted from the return value from
`erlang:timestamp/0`.
""".
-spec now_to_local_time(Now) -> datetime1970() when
      Now :: erlang:timestamp().
now_to_local_time({MSec, Sec, _uSec}) ->
    erlang:universaltime_to_localtime(
      now_to_universal_time({MSec, Sec, _uSec})).

-doc(#{equiv => rfc3339_to_system_time(DateTimeString, [])}).
-doc(#{since => <<"OTP 21.0">>}).
-spec rfc3339_to_system_time(DateTimeString) -> integer() when
      DateTimeString :: rfc3339_string().

rfc3339_to_system_time(DateTimeString) ->
    rfc3339_to_system_time(DateTimeString, []).

-doc """
Converts an RFC 3339 timestamp into system time. The data format of RFC 3339
timestamps is described by [RFC 3339](https://www.ietf.org/rfc/rfc3339.txt).
Starting from OTP 25.1, the minutes part of the time zone is optional.

Valid option:

- **`{unit, Unit}`** - The time unit of the return value. The default is
  `second`.

```erlang
1> calendar:rfc3339_to_system_time("2018-02-01T16:17:58+01:00").
1517498278
2> calendar:rfc3339_to_system_time("2018-02-01 15:18:02.088Z",
   [{unit, nanosecond}]).
1517498282088000000
3> calendar:rfc3339_to_system_time(<<"2018-02-01 15:18:02.088Z">>,
   [{unit, nanosecond}]).
1517498282088000000
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec rfc3339_to_system_time(DateTimeString, Options) -> integer() when
      DateTimeString :: rfc3339_string(),
      Options :: [Option],
      Option :: {'unit', rfc3339_time_unit()}.

rfc3339_to_system_time(Bin, Options) when is_binary(Bin) ->
    rfc3339_to_system_time_bin(Bin, Options);
rfc3339_to_system_time(List, Options) when is_list(List) ->
    rfc3339_to_system_time_list(List, Options).

%% _T is the character separating the date and the time:
rfc3339_to_system_time_bin(
    <<Year0:4/binary, $-, Month0:2/binary, $-, Day0:2/binary, _T,
      Hour0:2/binary, $:, Min0:2/binary, $:, Sec0:2/binary, TimeStr/binary>> = DateTimeBin, Options) ->
    Hour = binary_to_integer(Hour0),
    Min = binary_to_integer(Min0),
    Sec = binary_to_integer(Sec0),
    Year = binary_to_integer(Year0),
    Month = binary_to_integer(Month0),
    Day = binary_to_integer(Day0),
    rfc3339_to_system_time_1(DateTimeBin, Options, Year, Month, Day, Hour, Min, Sec, binary_to_list(TimeStr)).

%% _T is the character separating the date and the time:
rfc3339_to_system_time_list(
    [Y1, Y2, Y3, Y4, $-, Mon1, Mon2, $-, D1, D2, _T,
     H1, H2, $:, Min1, Min2, $:, S1, S2 | TimeStr] = DateTimeString, Options) ->
    Hour = list_to_integer([H1, H2]),
    Min = list_to_integer([Min1, Min2]),
    Sec = list_to_integer([S1, S2]),
    Year = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([Mon1, Mon2]),
    Day = list_to_integer([D1, D2]),
    rfc3339_to_system_time_1(DateTimeString, Options, Year, Month, Day, Hour, Min, Sec, TimeStr).

rfc3339_to_system_time_1(DateTimeIn, Options, Year, Month, Day, Hour, Min, Sec, TimeStr) ->
    Unit = proplists:get_value(unit, Options, second),
    DateTime = {{Year, Month, Day}, {Hour, Min, Sec}},
    IsFractionChar = fun(C) -> C >= $0 andalso C =< $9 orelse C =:= $. end,
    {FractionStr, UtcOffset} = lists:splitwith(IsFractionChar, TimeStr),
    Time = datetime_to_system_time(DateTime),
    Secs = Time - offset_string_adjustment(Time, second, UtcOffset),
    check(DateTimeIn, Options, Secs),
    ScaledEpoch = erlang:convert_time_unit(Secs, second, Unit),
    ScaledEpoch + fraction(Unit, FractionStr).



%% seconds_to_daystime(Secs) = {Days, {Hour, Minute, Second}}
%%
-doc """
Converts a specified number of seconds into days, hours, minutes, and seconds.
`Time` is always non-negative, but `Days` is negative if argument `Seconds` is.
""".
-spec seconds_to_daystime(Seconds) -> {Days, Time} when
      Seconds :: integer(),
      Days :: integer(),
      Time :: time().
seconds_to_daystime(Secs) ->
    Days0 = Secs div ?SECONDS_PER_DAY,
    Secs0 = Secs rem ?SECONDS_PER_DAY,
    if 
	Secs0 < 0 ->
	    {Days0 - 1, seconds_to_time(Secs0 + ?SECONDS_PER_DAY)};
	true ->
	    {Days0, seconds_to_time(Secs0)}
    end.


%%
%% seconds_to_time(Secs)
%%
%% Wraps.
%%
-type secs_per_day() :: 0..86399.
-doc """
Computes the time from the specified number of seconds. `Seconds` must be less
than the number of seconds per day (86400).
""".
-spec seconds_to_time(Seconds) -> time() when
      Seconds :: secs_per_day().
seconds_to_time(Secs) when Secs >= 0, Secs < ?SECONDS_PER_DAY ->
    Secs0 = Secs rem ?SECONDS_PER_DAY,
    Hour = Secs0 div ?SECONDS_PER_HOUR,
    Secs1 = Secs0 rem ?SECONDS_PER_HOUR,
    Minute =  Secs1 div ?SECONDS_PER_MINUTE,
    Second =  Secs1 rem ?SECONDS_PER_MINUTE,
    {Hour, Minute, Second}.

-doc "Converts a specified system time into local date and time.".
-doc(#{since => <<"OTP 21.0">>}).
-spec system_time_to_local_time(Time, TimeUnit) -> datetime() when
      Time :: integer(),
      TimeUnit :: erlang:time_unit().

system_time_to_local_time(Time, TimeUnit) ->
    UniversalDate = system_time_to_universal_time(Time, TimeUnit),
    erlang:universaltime_to_localtime(UniversalDate).

-doc "Converts a specified system time into universal date and time.".
-doc(#{since => <<"OTP 21.0">>}).
-spec system_time_to_universal_time(Time, TimeUnit) -> datetime() when
      Time :: integer(),
      TimeUnit :: erlang:time_unit().

system_time_to_universal_time(Time, TimeUnit) ->
    Secs = erlang:convert_time_unit(Time, TimeUnit, second),
    system_time_to_datetime(Secs).

-doc(#{equiv => system_time_to_rfc3339(Time, [])}).
-doc(#{since => <<"OTP 21.0">>}).
-spec system_time_to_rfc3339(Time) -> DateTimeString when
      Time :: integer(),
      DateTimeString :: rfc3339_string().

system_time_to_rfc3339(Time) ->
    system_time_to_rfc3339(Time, []).

-type offset() :: [byte()] | (Time :: integer()).
-doc """
Converts a system time into an RFC 3339 timestamp.

The data format of RFC 3339 timestamps is described by [RFC 3339].
The data format of offsets is also described by [RFC 3339].

Valid options:

- **`{offset, Offset}`** - The offset, either a string or an integer, to be
  included in the formatted string. An empty string, which is the default, is
  interpreted as local time. A non-empty string is included as is. The time unit
  of the integer is the same as the one of `Time`.

- **`{time_designator, Character}`** - The character used as time designator,
  that is, the date and time separator. The default is `$T`.

- **`{unit, Unit}`** - The time unit of `Time`. The default is `second`. If some
  other unit is given (`millisecond`, `microsecond`, `nanosecond`, or `native`),
  the formatted string includes a fraction of a second. The number of fractional
  second digits is three, six, or nine depending on what time unit is chosen.
  For `native` three fractional digits are included. Notice that trailing zeros
  are not removed from the fraction.

- **`{return, Return}`** - The desired encoding type for the output,
  whether a string or a binary is desired. Defaults to string.

```erlang
1> calendar:system_time_to_rfc3339(erlang:system_time(second)).
"2018-04-23T14:56:28+02:00"
2> calendar:system_time_to_rfc3339(erlang:system_time(second),
   [{offset, "-02:00"}]).
"2018-04-23T10:56:52-02:00"
3> calendar:system_time_to_rfc3339(erlang:system_time(second),
   [{offset, -7200}]).
"2018-04-23T10:57:05-02:00"
4> calendar:system_time_to_rfc3339(erlang:system_time(millisecond),
   [{unit, millisecond}, {time_designator, $\s}, {offset, "Z"}]).
"2018-04-23 12:57:20.482Z"
5> calendar:system_time_to_rfc3339(erlang:system_time(millisecond),
   [{unit, millisecond}, {time_designator, $\s}, {offset, "Z"}, {return, binary}]).
<<"2018-04-23 12:57:20.482Z">>
```
[RFC 3339]: https://www.ietf.org/rfc/rfc3339.txt
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec system_time_to_rfc3339(Time, Options) -> DateTimeString when
      Time :: integer(), % Since Epoch
      Options :: [Option],
      Option :: {'offset', offset()}
              | {'time_designator', byte()}
              | {'unit', rfc3339_time_unit()}
              | {'return', 'string' | 'binary'},
      DateTimeString :: rfc3339_string().

system_time_to_rfc3339(Time, Options) ->
    Unit = proplists:get_value(unit, Options, second),
    OffsetOpt0 = proplists:get_value(offset, Options, ""),
    case Unit of
        native ->
            TimeMS = erlang:convert_time_unit(Time, native, millisecond),
            OffsetOpt1 =
                case is_integer(OffsetOpt0) of
                    true ->
                        erlang:convert_time_unit(OffsetOpt0, native, millisecond);
                    false ->
                        OffsetOpt0
                end,
            system_time_to_rfc3339_do(TimeMS, Options, millisecond, OffsetOpt1);
        _ ->
            system_time_to_rfc3339_do(Time, Options, Unit, OffsetOpt0)
    end.

system_time_to_rfc3339_do(Time, Options, Unit, OffsetOption) ->
    T = proplists:get_value(time_designator, Options, $T),
    AdjustmentSecs = offset_adjustment(Time, Unit, OffsetOption),
    Offset = offset(OffsetOption, AdjustmentSecs),
    Adjustment = erlang:convert_time_unit(AdjustmentSecs, second, Unit),
    AdjustedTime = Time + Adjustment,
    Factor = factor(Unit),
    Secs0 = AdjustedTime div Factor,
    Secs = if
	       AdjustedTime rem Factor < 0 -> Secs0 - 1;
	       true -> Secs0
	   end,
    check(Time, Options, Secs),
    DateTime = system_time_to_datetime(Secs),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    FractionStr = fraction_str(Factor, AdjustedTime),
    L = [pad4(Year), "-", pad2(Month), "-", pad2(Day), [T],
         pad2(Hour), ":", pad2(Min), ":", pad2(Sec), FractionStr, Offset],
    case proplists:get_value(return, Options, string) of
        string ->
            lists:append(L);
        binary ->
            iolist_to_binary(L)
    end.

%% time_difference(T1, T2) = Tdiff
%%
%% Returns the difference between two {Date, Time} structures.
%%
%% T1 = T2 = {Date, Time}, Tdiff = {Day, {Hour, Min, Sec}}, 
%% Date = {Year, Month, Day}, Time = {Hour, Minute, Sec},
%% Year = Month = Day = Hour = Minute = Sec = integer()
%%
-doc """
Returns the difference between two `{Date, Time}` tuples. `T2` is to refer to an
epoch later than `T1`.

> #### Warning {: .warning }
>
> This function is obsolete. Use the conversion functions for gregorian days and
> seconds instead.
""".
-spec time_difference(T1, T2) -> {Days, Time} when
      T1 :: datetime(),
      T2 :: datetime(),
      Days :: integer(),
      Time :: time().
time_difference({{Y1, Mo1, D1}, {H1, Mi1, S1}}, 
		{{Y2, Mo2, D2}, {H2, Mi2, S2}}) ->
    Secs = datetime_to_gregorian_seconds({{Y2, Mo2, D2}, {H2, Mi2, S2}}) -
	datetime_to_gregorian_seconds({{Y1, Mo1, D1}, {H1, Mi1, S1}}),
    seconds_to_daystime(Secs).


%%
%% time_to_seconds(Time)
%%
-doc "Returns the number of seconds since midnight up to the specified time.".
-spec time_to_seconds(Time) -> secs_per_day() when
      Time :: time().
time_to_seconds({H, M, S}) when is_integer(H), is_integer(M), is_integer(S) ->
    H * ?SECONDS_PER_HOUR +
	M * ?SECONDS_PER_MINUTE + S.
      

%% universal_time()
%%
%% Returns: {date(), time()}, date() = {Y, M, D}, time() = {H, M, S}.
-doc """
Returns the Universal Coordinated Time (UTC) reported by the underlying
operating system. Returns local time if universal time is unavailable.
""".
-spec universal_time() -> datetime().
universal_time() ->
    erlang:universaltime().

-doc(#{equiv => universal_time_to_system_time(LocalTime, [])}).
-doc(#{since => <<"OTP 28.0">>}).
-spec universal_time_to_system_time(datetime()) -> integer().
universal_time_to_system_time(UniversalTime) ->
    universal_time_to_system_time(UniversalTime, []).

-doc(#{since => <<"OTP 28.0">>}).
-doc "Converts universal time into system time.".
-spec universal_time_to_system_time(datetime(), Options) -> integer() when
      Options :: [Option],
      Option :: {unit, erlang:time_unit()}.
universal_time_to_system_time(DateTime, Options) ->
    Unit = proplists:get_value(unit, Options, second),
    Factor = factor(Unit),
    Time = datetime_to_system_time(DateTime),
    Time * Factor.

%% universal_time_to_local_time(DateTime)
%%
-doc """
Converts from Universal Coordinated Time (UTC) to local time. `DateTime` must
refer to a date after Jan 1, 1970.
""".
-spec universal_time_to_local_time(DateTime) -> datetime() when
      DateTime :: datetime1970().
universal_time_to_local_time(DateTime) ->
    erlang:universaltime_to_localtime(DateTime).


%% valid_date(Year, Month, Day) = true | false
%% valid_date({Year, Month, Day}) = true | false
%%
-doc(#{equiv => valid_date({Year, Month, Day})}).
-spec valid_date(Year, Month, Day) -> boolean() when
      Year :: integer(),
      Month :: integer(),
      Day :: integer().
valid_date(Y, M, D) when is_integer(Y), is_integer(M), is_integer(D) ->
    valid_date1(Y, M, D).

-spec valid_date1(integer(), integer(), integer()) -> boolean().
valid_date1(Y, M, D) when Y >= 0, M > 0, M < 13, D > 0 ->
    D =< last_day_of_the_month(Y, M);
valid_date1(_, _, _) ->
    false.

-doc "This function checks if a date is a valid.".
-spec valid_date(Date) -> boolean() when
      Date :: date().
valid_date({Y, M, D}) ->
    valid_date(Y, M, D).


%%
%%  LOCAL FUNCTIONS
%%
-type day_of_year() :: 0..365.

%% day_to_year(DayOfEpoch) = {Year, DayOfYear}
%%
%% The idea here is to first set the upper and lower bounds for a year,
%% and then adjust a range by interpolation search. Although complexity
%% of the algorithm is log(log(n)), at most 1 or 2 recursive steps
%% are taken.
%%
-spec day_to_year(non_neg_integer()) -> {year(), day_of_year()}.
day_to_year(DayOfEpoch) when DayOfEpoch >= 0 ->
    YMax = DayOfEpoch div ?DAYS_PER_YEAR,
    YMin = DayOfEpoch div ?DAYS_PER_LEAP_YEAR,
    {Y1, D1} = dty(YMin, YMax, DayOfEpoch, dy(YMin), dy(YMax)),
    {Y1, DayOfEpoch - D1}.

-spec dty(year(), year(), non_neg_integer(), non_neg_integer(),
    non_neg_integer()) ->
		{year(), non_neg_integer()}.
dty(Min, Max, _D1, DMin, _DMax) when Min == Max ->
    {Min, DMin};
dty(Min, Max, D1, DMin, DMax) ->
    Diff = Max - Min,
    Mid = Min + (Diff * (D1 - DMin)) div (DMax - DMin),
    MidLength =
        case is_leap_year(Mid) of
            true -> ?DAYS_PER_LEAP_YEAR;
            false -> ?DAYS_PER_YEAR
        end,
    case dy(Mid) of
        D2 when D1 < D2 ->
            NewMax = Mid - 1,
            dty(Min, NewMax, D1, DMin, dy(NewMax));
        D2 when D1 - D2 >= MidLength ->
            NewMin = Mid + 1,
            dty(NewMin, Max, D1, dy(NewMin), DMax);
        D2 ->
            {Mid, D2}
    end.

%%
%% The Gregorian days of the iso week 01 day 1 for a given year.
%%
-spec gregorian_days_of_iso_w01_1(year()) -> non_neg_integer().
gregorian_days_of_iso_w01_1(Year) ->
    D0101 = date_to_gregorian_days(Year, 1, 1),
    DOW = day_of_the_week(Year, 1, 1),
    if DOW =< 4 ->
	D0101 - DOW + 1;
    true ->
	D0101 + 7 - DOW + 1
    end.

%% year_day_to_date(Year, DayOfYear)  = {Month,  DayOfMonth}
%%
%% Note: 1 is the first day of the month. 
%%
-spec year_day_to_date(year(), day_of_year()) -> {month(), day()}.
year_day_to_date(Year, DayOfYear) ->
    ExtraDay = case is_leap_year(Year) of
		   true ->
		       1;
		   false ->
		       0
	       end,
    {Month, Day} = year_day_to_date2(ExtraDay, DayOfYear),
    {Month, Day + 1}.
  

%% Note: 0 is the first day of the month 
%% 
-spec year_day_to_date2(0 | 1, day_of_year()) -> {month(), 0..30}.
year_day_to_date2(_, Day) when Day < 31 ->
    {1, Day}; 
year_day_to_date2(E, Day) when 31 =< Day, Day < 59 + E ->
    {2, Day - 31}; 
year_day_to_date2(E, Day) when 59 + E =< Day, Day < 90 + E -> 
    {3, Day - (59 + E)}; 
year_day_to_date2(E, Day) when 90 + E =< Day, Day < 120 + E -> 
    {4, Day - (90 + E)};
year_day_to_date2(E, Day) when 120 + E =< Day, Day < 151 + E -> 
    {5, Day - (120 + E)}; 
year_day_to_date2(E, Day) when 151 + E =< Day, Day < 181 + E ->
    {6, Day - (151 + E)};
year_day_to_date2(E, Day) when 181 + E =< Day, Day < 212 + E -> 
    {7, Day - (181 + E)}; 
year_day_to_date2(E, Day) when 212 + E =< Day, Day < 243 + E ->
    {8, Day - (212 + E)};
year_day_to_date2(E, Day) when 243 + E =< Day, Day < 273 + E ->
    {9, Day - (243 + E)};
year_day_to_date2(E, Day) when 273 + E =< Day, Day < 304 + E ->
    {10, Day - (273 + E)};
year_day_to_date2(E, Day) when 304 + E =< Day, Day < 334 + E ->
    {11, Day - (304 + E)};
year_day_to_date2(E, Day) when 334 + E =< Day -> 
    {12, Day - (334 + E)}.

%% dy(Year)
%%
%% Days in previous years.
%%
-spec dy(integer()) -> non_neg_integer().
dy(Y) when Y =< 0 -> 
    0; 
dy(Y) -> 
    X = Y - 1, 
    (X div 4) - (X div 100) + (X div 400) + 
	X*?DAYS_PER_YEAR + ?DAYS_PER_LEAP_YEAR.

%%  dm(Month)
%%
%%  Returns the total number of days in all months
%%  preceeding Month, for an ordinary year.
%%
-spec dm(month()) ->
	     0 | 31 | 59 | 90 | 120 | 151 | 181 | 212 | 243 | 273 | 304 | 334.
dm(1) -> 0;    dm(2) ->  31;   dm(3) ->   59;   dm(4) ->  90;
dm(5) -> 120;  dm(6) ->  151;  dm(7) ->  181;   dm(8) -> 212;
dm(9) -> 243;  dm(10) -> 273;  dm(11) -> 304;  dm(12) -> 334.
 
%%  df(Year, Month)
%%
%%  Accounts for an extra day in February if Year is
%%  a leap year, and if Month > 2.
%%
-spec df(year(), month()) -> 0 | 1.
df(_, Month) when Month < 3 ->
    0;
df(Year, _) ->
    case is_leap_year(Year) of
	true -> 1;
	false  -> 0
    end.

check(_Arg, _Options, Secs) when Secs >= - ?SECONDS_FROM_0_TO_1970,
                                 Secs < ?SECONDS_FROM_0_TO_10000 ->
    ok;
check(Arg, Options, _Secs) ->
    erlang:error({badarg, [Arg, Options]}).

datetime_to_system_time(DateTime) ->
    datetime_to_gregorian_seconds(DateTime) - ?SECONDS_FROM_0_TO_1970.

system_time_to_datetime(Seconds) ->
    gregorian_seconds_to_datetime(Seconds + ?SECONDS_FROM_0_TO_1970).

offset(OffsetOption, Secs0) when OffsetOption =:= "";
                                 is_integer(OffsetOption) ->
    Sign = case Secs0 < 0 of
               true -> $-;
               false -> $+
           end,
    Secs = abs(Secs0),
    Hour = Secs div 3600,
    Min = (Secs rem 3600) div 60,
    [Sign | lists:append([pad2(Hour), ":", pad2(Min)])];
offset(OffsetOption, _Secs) ->
    OffsetOption.

offset_adjustment(Time, Unit, "") ->
    local_offset(Time, Unit);
offset_adjustment(Time, Unit, OffsetString) when is_list(OffsetString) ->
    offset_string_adjustment(Time, Unit, OffsetString);
offset_adjustment(_Time, Unit, Offset) when is_integer(Offset) ->
    erlang:convert_time_unit(Offset, Unit, second).

offset_string_adjustment(_Time, _Unit, "Z") ->
    0;
offset_string_adjustment(_Time, _Unit, "z") ->
    0;
offset_string_adjustment(_Time, _Unit, Tz) ->
    [Sign, H1, H2 | MinutesDiff] = Tz,
    Hour = list_to_integer([H1, H2]),
    Min = case MinutesDiff of
              [$:, M1, M2] ->
                  list_to_integer([M1, M2]);
              [] ->
                  0
          end,
    Adjustment = 3600 * Hour + 60 * Min,
    case Sign of
        $- -> -Adjustment;
        $+ -> Adjustment
    end.

local_offset(SystemTime, Unit) ->
    %% Not optimized for special cases.
    UniversalTime = system_time_to_universal_time(SystemTime, Unit),
    LocalTime = erlang:universaltime_to_localtime(UniversalTime),
    LocalSecs = datetime_to_gregorian_seconds(LocalTime),
    UniversalSecs = datetime_to_gregorian_seconds(UniversalTime),
    LocalSecs - UniversalSecs.

mod(N, D) ->
    case N rem D of
	R when R < 0 -> mod(R + D, D);
	R -> R
    end.

fraction_str(1, _Time) ->
    "";
fraction_str(Factor, Time) ->
    Fraction = mod(Time, Factor),
    S = integer_to_list(Fraction),
    [$. | pad(log10(Factor) - length(S), S)].

fraction(second, _) ->
    0;
fraction(_, "") ->
    0;
fraction(Unit, FractionStr) ->
    round(factor(Unit) * list_to_float([$0|FractionStr])).

factor(second)      -> 1;
factor(millisecond) -> 1000;
factor(microsecond) -> 1_000_000;
factor(nanosecond)  -> 1_000_000_000;
factor(native)      -> erlang:convert_time_unit(1, second, native).

log10(1000) -> 3;
log10(1_000_000) -> 6;
log10(1_000_000_000) -> 9.

pad(0, S) ->
    S;
pad(I, S) ->
    [$0 | pad(I - 1, S)].

pad2(N) when N < 10 ->
    [$0 | integer_to_list(N)];
pad2(N) ->
    integer_to_list(N).

pad4(N) when N < 10 ->
    [$0, $0, $0 | integer_to_list(N)];
pad4(N) when N < 100 ->
    [$0, $0 | integer_to_list(N)];
pad4(N) when N < 1000 ->
    [$0 | integer_to_list(N)];
pad4(N) ->
    integer_to_list(N).
