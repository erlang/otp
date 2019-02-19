%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
	 valid_date/1,
	 valid_date/3]).

-deprecated([{local_time_to_universal_time,1}]).

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

-type year()     :: non_neg_integer().
-type year1970() :: 1970..10000.	% should probably be 1970..
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type daynum()   :: 1..7.
-type ldom()     :: 28 | 29 | 30 | 31. % last day of month
-type weeknum()  :: 1..53.

-type date()           :: {year(),month(),day()}.
-type time()           :: {hour(),minute(),second()}.
-type datetime()       :: {date(),time()}.
-type datetime1970()   :: {{year1970(),month(),day()},time()}.
-type yearweeknum()    :: {year(),weeknum()}.

-type rfc3339_string() :: [byte(), ...].
%% By design 'native' is not supported:
-type rfc3339_time_unit() :: 'microsecond'
                           | 'millisecond'
                           | 'nanosecond'
                           | 'second'.

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
-spec day_of_the_week(Year, Month, Day) -> daynum() when
      Year :: year(),
      Month :: month(),
      Day :: day().
day_of_the_week(Year, Month, Day) ->
    (date_to_gregorian_days(Year, Month, Day) + 5) rem 7 + 1.

-spec day_of_the_week(Date) -> daynum() when
      Date:: date().
day_of_the_week({Year, Month, Day}) ->
    day_of_the_week(Year, Month, Day).


%% gregorian_days_to_date(Days) = {Year, Month, Day}
%%
-spec gregorian_days_to_date(Days) -> date() when
      Days :: non_neg_integer().
gregorian_days_to_date(Days) ->
    {Year, DayOfYear} = day_to_year(Days),
    {Month, DayOfMonth} = year_day_to_date(Year, DayOfYear),
    {Year, Month, DayOfMonth}.


%% gregorian_seconds_to_datetime(Secs)
%%
-spec gregorian_seconds_to_datetime(Seconds) -> datetime() when
      Seconds :: non_neg_integer().
gregorian_seconds_to_datetime(Secs) when Secs >= 0 ->
    Days = Secs div ?SECONDS_PER_DAY,
    Rest = Secs rem ?SECONDS_PER_DAY,
    {gregorian_days_to_date(Days), seconds_to_time(Rest)}.


%% is_leap_year(Year) = true | false
%%
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
-spec iso_week_number() -> yearweeknum().
iso_week_number() ->
    {Date, _} = local_time(),
    iso_week_number(Date).


%%
%% Calculates the iso week number for the given date.
%%
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
-spec local_time() -> datetime().
local_time() ->
    erlang:localtime().


%% local_time_to_universal_time(DateTime)
%%
-spec local_time_to_universal_time(DateTime1) -> DateTime2 when
      DateTime1 :: datetime1970(),
      DateTime2 :: datetime1970().
local_time_to_universal_time(DateTime) ->
    erlang:localtime_to_universaltime(DateTime).

-spec local_time_to_universal_time(datetime1970(),
				   'true' | 'false' | 'undefined') ->
                                          datetime1970().
local_time_to_universal_time(DateTime, IsDst) ->
    erlang:localtime_to_universaltime(DateTime, IsDst).

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
-spec now_to_datetime(Now) -> datetime1970() when
      Now :: erlang:timestamp().
now_to_datetime({MSec, Sec, _uSec}) ->
    system_time_to_datetime(MSec*1000000 + Sec).

-spec now_to_universal_time(Now) -> datetime1970() when
      Now :: erlang:timestamp().
now_to_universal_time(Now) ->
    now_to_datetime(Now).


%% now_to_local_time(Now)
%%
%% Args: Now = now()
%%
-spec now_to_local_time(Now) -> datetime1970() when
      Now :: erlang:timestamp().
now_to_local_time({MSec, Sec, _uSec}) ->
    erlang:universaltime_to_localtime(
      now_to_universal_time({MSec, Sec, _uSec})).

-spec rfc3339_to_system_time(DateTimeString) -> integer() when
      DateTimeString :: rfc3339_string().

rfc3339_to_system_time(DateTimeString) ->
    rfc3339_to_system_time(DateTimeString, []).

-spec rfc3339_to_system_time(DateTimeString, Options) -> integer() when
      DateTimeString :: rfc3339_string(),
      Options :: [Option],
      Option :: {'unit', rfc3339_time_unit()}.

rfc3339_to_system_time(DateTimeString, Options) ->
    Unit = proplists:get_value(unit, Options, second),
    %% _T is the character separating the date and the time:
    [Y1, Y2, Y3, Y4, $-, Mon1, Mon2, $-, D1, D2, _T,
     H1, H2, $:, Min1, Min2, $:, S1, S2 | TimeStr] = DateTimeString,
    Hour = list_to_integer([H1, H2]),
    Min = list_to_integer([Min1, Min2]),
    Sec = list_to_integer([S1, S2]),
    Year = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([Mon1, Mon2]),
    Day = list_to_integer([D1, D2]),
    DateTime = {{Year, Month, Day}, {Hour, Min, Sec}},
    IsFractionChar = fun(C) -> C >= $0 andalso C =< $9 orelse C =:= $. end,
    {FractionStr, UtcOffset} = lists:splitwith(IsFractionChar, TimeStr),
    Time = datetime_to_system_time(DateTime),
    Secs = Time - offset_adjustment(Time, second, UtcOffset),
    check(DateTimeString, Options, Secs),
    ScaledEpoch = erlang:convert_time_unit(Secs, second, Unit),
    ScaledEpoch + copy_sign(fraction(Unit, FractionStr), ScaledEpoch).



%% seconds_to_daystime(Secs) = {Days, {Hour, Minute, Second}}
%%
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
-type secs_per_day() :: 0..?SECONDS_PER_DAY.
-spec seconds_to_time(Seconds) -> time() when
      Seconds :: secs_per_day().
seconds_to_time(Secs) when Secs >= 0, Secs < ?SECONDS_PER_DAY ->
    Secs0 = Secs rem ?SECONDS_PER_DAY,
    Hour = Secs0 div ?SECONDS_PER_HOUR,
    Secs1 = Secs0 rem ?SECONDS_PER_HOUR,
    Minute =  Secs1 div ?SECONDS_PER_MINUTE,
    Second =  Secs1 rem ?SECONDS_PER_MINUTE,
    {Hour, Minute, Second}.

-spec system_time_to_local_time(Time, TimeUnit) -> datetime() when
      Time :: integer(),
      TimeUnit :: erlang:time_unit().

system_time_to_local_time(Time, TimeUnit) ->
    UniversalDate = system_time_to_universal_time(Time, TimeUnit),
    erlang:universaltime_to_localtime(UniversalDate).

-spec system_time_to_universal_time(Time, TimeUnit) -> datetime() when
      Time :: integer(),
      TimeUnit :: erlang:time_unit().

system_time_to_universal_time(Time, TimeUnit) ->
    Secs = erlang:convert_time_unit(Time, TimeUnit, second),
    system_time_to_datetime(Secs).

-spec system_time_to_rfc3339(Time) -> DateTimeString when
      Time :: integer(),
      DateTimeString :: rfc3339_string().

system_time_to_rfc3339(Time) ->
    system_time_to_rfc3339(Time, []).

-type offset() :: [byte()] | (Time :: integer()).
-spec system_time_to_rfc3339(Time, Options) -> DateTimeString when
      Time :: integer(), % Since Epoch
      Options :: [Option],
      Option :: {'offset', offset()}
              | {'time_designator', byte()}
              | {'unit', rfc3339_time_unit()},
      DateTimeString :: rfc3339_string().

system_time_to_rfc3339(Time, Options) ->
    Unit = proplists:get_value(unit, Options, second),
    OffsetOption = proplists:get_value(offset, Options, ""),
    T = proplists:get_value(time_designator, Options, $T),
    AdjustmentSecs = offset_adjustment(Time, Unit, OffsetOption),
    Offset = offset(OffsetOption, AdjustmentSecs),
    Adjustment = erlang:convert_time_unit(AdjustmentSecs, second, Unit),
    AdjustedTime = Time + Adjustment,
    Factor = factor(Unit),
    Secs = AdjustedTime div Factor,
    check(Time, Options, Secs),
    DateTime = system_time_to_datetime(Secs),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    FractionStr = fraction_str(Factor, AdjustedTime),
    L = [pad4(Year), "-", pad2(Month), "-", pad2(Day), [T],
         pad2(Hour), ":", pad2(Min), ":", pad2(Sec), FractionStr, Offset],
    lists:append(L).

%% time_difference(T1, T2) = Tdiff
%%
%% Returns the difference between two {Date, Time} structures.
%%
%% T1 = T2 = {Date, Time}, Tdiff = {Day, {Hour, Min, Sec}}, 
%% Date = {Year, Month, Day}, Time = {Hour, Minute, Sec},
%% Year = Month = Day = Hour = Minute = Sec = integer()
%%
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
-spec time_to_seconds(Time) -> secs_per_day() when
      Time :: time().
time_to_seconds({H, M, S}) when is_integer(H), is_integer(M), is_integer(S) ->
    H * ?SECONDS_PER_HOUR +
	M * ?SECONDS_PER_MINUTE + S.
      

%% universal_time()
%%
%% Returns: {date(), time()}, date() = {Y, M, D}, time() = {H, M, S}.
-spec universal_time() -> datetime().
universal_time() ->
    erlang:universaltime().
 

%% universal_time_to_local_time(DateTime)
%%
-spec universal_time_to_local_time(DateTime) -> datetime() when
      DateTime :: datetime1970().
universal_time_to_local_time(DateTime) ->
    erlang:universaltime_to_localtime(DateTime).


%% valid_date(Year, Month, Day) = true | false
%% valid_date({Year, Month, Day}) = true | false
%%
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

offset_adjustment(Time, Unit, OffsetString) when is_list(OffsetString) ->
    offset_string_adjustment(Time, Unit, OffsetString);
offset_adjustment(_Time, Unit, Offset) when is_integer(Offset) ->
    erlang:convert_time_unit(Offset, Unit, second).

offset_string_adjustment(Time, Unit, "") ->
    local_offset(Time, Unit);
offset_string_adjustment(_Time, _Unit, "Z") ->
    0;
offset_string_adjustment(_Time, _Unit, "z") ->
    0;
offset_string_adjustment(_Time, _Unit, Tz) ->
    [Sign, H1, H2, $:, M1, M2] = Tz,
    Hour = list_to_integer([H1, H2]),
    Min = list_to_integer([M1, M2]),
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

fraction_str(1, _Time) ->
    "";
fraction_str(Factor, Time) ->
    Fraction = Time rem Factor,
    S = integer_to_list(abs(Fraction)),
    [$. | pad(log10(Factor) - length(S), S)].

fraction(second, _) ->
    0;
fraction(_, "") ->
    0;
fraction(Unit, FractionStr) ->
    round(factor(Unit) * list_to_float([$0|FractionStr])).

copy_sign(N1, N2) when N2 < 0 -> -N1;
copy_sign(N1, _N2) -> N1.

factor(second)      -> 1;
factor(millisecond) -> 1000;
factor(microsecond) -> 1000000;
factor(nanosecond)  -> 1000000000.

log10(1000) -> 3;
log10(1000000) -> 6;
log10(1000000000) -> 9.

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
