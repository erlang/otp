%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
	 last_day_of_the_month/2,
	 local_time/0, 
	 local_time_to_universal_time/1, 
	 local_time_to_universal_time/2, 
	 local_time_to_universal_time_dst/1, 
	 now_to_datetime/1,			% = now_to_universal_time/1
	 now_to_local_time/1,
	 now_to_universal_time/1,
	 seconds_to_daystime/1,
	 seconds_to_time/1,
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
-define(DAYS_PER_4YEARS, 1461).
-define(DAYS_PER_100YEARS, 36524).
-define(DAYS_PER_400YEARS, 146097).
-define(DAYS_FROM_0_TO_1970, 719528).

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-type year()     :: non_neg_integer().
-type year1970() :: 1970..10000.	% should probably be 1970..
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type daynum()   :: 1..7.
-type ldom()     :: 28 | 29 | 30 | 31. % last day of month

-type t_now()    :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.

-type t_date()         :: {year(),month(),day()}.
-type t_time()         :: {hour(),minute(),second()}.
-type t_datetime()     :: {t_date(),t_time()}.
-type t_datetime1970() :: {{year1970(),month(),day()},t_time()}.

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
-spec date_to_gregorian_days(year(),month(),day()) -> non_neg_integer().
date_to_gregorian_days(Year, Month, Day) when is_integer(Day), Day > 0 ->
    Last = last_day_of_the_month(Year, Month),
    if
	Day =< Last ->
	    dy(Year) + dm(Month) + df(Year, Month) + Day - 1
    end.

-spec date_to_gregorian_days(t_date()) -> non_neg_integer().
date_to_gregorian_days({Year, Month, Day}) ->
    date_to_gregorian_days(Year, Month, Day).


%% datetime_to_gregorian_seconds(DateTime) = Integer
%%
%% Computes the total number of seconds starting from year 0,
%% January 1st.
%%
-spec datetime_to_gregorian_seconds(t_datetime()) -> non_neg_integer().
datetime_to_gregorian_seconds({Date, Time}) ->
    ?SECONDS_PER_DAY*date_to_gregorian_days(Date) +
	time_to_seconds(Time).


%% day_of_the_week(Year, Month, Day)
%% day_of_the_week({Year, Month, Day})
%%
%% Returns: 1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7.
%%
-spec day_of_the_week(year(), month(), day()) -> daynum().
day_of_the_week(Year, Month, Day) ->
    (date_to_gregorian_days(Year, Month, Day) + 5) rem 7 + 1.

-spec day_of_the_week(t_date()) -> daynum().
day_of_the_week({Year, Month, Day}) ->
    day_of_the_week(Year, Month, Day).


%% gregorian_days_to_date(Days) = {Year, Month, Day}
%%
-spec gregorian_days_to_date(non_neg_integer()) -> t_date().
gregorian_days_to_date(Days) ->
    {Year, DayOfYear} = day_to_year(Days),
    {Month, DayOfMonth} = year_day_to_date(Year, DayOfYear),
    {Year, Month, DayOfMonth}.


%% gregorian_seconds_to_datetime(Secs)
%%
-spec gregorian_seconds_to_datetime(non_neg_integer()) -> t_datetime().
gregorian_seconds_to_datetime(Secs) when Secs >= 0 ->
    Days = Secs div ?SECONDS_PER_DAY,
    Rest = Secs rem ?SECONDS_PER_DAY,
    {gregorian_days_to_date(Days), seconds_to_time(Rest)}.


%% is_leap_year(Year) = true | false
%%
-spec is_leap_year(year()) -> boolean().
is_leap_year(Y) when is_integer(Y), Y >= 0 ->
    is_leap_year1(Y).

-spec is_leap_year1(year()) -> boolean().
is_leap_year1(Year) when Year rem 4 =:= 0, Year rem 100 > 0 ->
    true;
is_leap_year1(Year) when Year rem 400 =:= 0 ->
    true;
is_leap_year1(_) -> false.


%% last_day_of_the_month(Year, Month)
%%
%% Returns the number of days in a month.
%%
-spec last_day_of_the_month(year(), month()) -> ldom().
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
-spec local_time() -> t_datetime().
local_time() ->
    erlang:localtime().


%% local_time_to_universal_time(DateTime)
%%
-spec local_time_to_universal_time(t_datetime1970()) -> t_datetime1970().
local_time_to_universal_time(DateTime) ->
    erlang:localtime_to_universaltime(DateTime).

-spec local_time_to_universal_time(t_datetime1970(),
				   'true' | 'false' | 'undefined') ->
       					t_datetime1970().
local_time_to_universal_time(DateTime, IsDst) ->
    erlang:localtime_to_universaltime(DateTime, IsDst).

-spec local_time_to_universal_time_dst(t_datetime1970()) -> [t_datetime1970()].
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
%% Convert from now() to UTC.
%%
%% Args: Now = now(); now() = {MegaSec, Sec, MilliSec}, MegaSec = Sec
%% = MilliSec = integer() 
%% Returns: {date(), time()}, date() = {Y, M, D}, time() = {H, M, S}.
%% 
-spec now_to_datetime(t_now()) -> t_datetime1970().
now_to_datetime({MSec, Sec, _uSec}) ->
    Sec0 = MSec*1000000 + Sec + ?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY,
    gregorian_seconds_to_datetime(Sec0).

-spec now_to_universal_time(t_now()) -> t_datetime1970().
now_to_universal_time(Now) ->
    now_to_datetime(Now).


%% now_to_local_time(Now)
%%
%% Args: Now = now()
%%
-spec now_to_local_time(t_now()) -> t_datetime1970().
now_to_local_time({MSec, Sec, _uSec}) ->
    erlang:universaltime_to_localtime(
      now_to_universal_time({MSec, Sec, _uSec})).



%% seconds_to_daystime(Secs) = {Days, {Hour, Minute, Second}}
%%
-spec seconds_to_daystime(integer()) -> {integer(), t_time()}.
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
-spec seconds_to_time(secs_per_day()) -> t_time().
seconds_to_time(Secs) when Secs >= 0, Secs < ?SECONDS_PER_DAY ->
    Secs0 = Secs rem ?SECONDS_PER_DAY,
    Hour = Secs0 div ?SECONDS_PER_HOUR,
    Secs1 = Secs0 rem ?SECONDS_PER_HOUR,
    Minute =  Secs1 div ?SECONDS_PER_MINUTE,
    Second =  Secs1 rem ?SECONDS_PER_MINUTE,
    {Hour, Minute, Second}.

%% time_difference(T1, T2) = Tdiff
%%
%% Returns the difference between two {Date, Time} structures.
%%
%% T1 = T2 = {Date, Time}, Tdiff = {Day, {Hour, Min, Sec}}, 
%% Date = {Year, Month, Day}, Time = {Hour, Minute, Sec},
%% Year = Month = Day = Hour = Minute = Sec = integer()
%%
-type timediff() :: {integer(), t_time()}.
-spec time_difference(t_datetime(), t_datetime()) -> timediff().
time_difference({{Y1, Mo1, D1}, {H1, Mi1, S1}}, 
		{{Y2, Mo2, D2}, {H2, Mi2, S2}}) ->
    Secs = datetime_to_gregorian_seconds({{Y2, Mo2, D2}, {H2, Mi2, S2}}) -
	datetime_to_gregorian_seconds({{Y1, Mo1, D1}, {H1, Mi1, S1}}),
    seconds_to_daystime(Secs).


%%
%% time_to_seconds(Time)
%%
-spec time_to_seconds(t_time()) -> secs_per_day().
time_to_seconds({H, M, S}) when is_integer(H), is_integer(M), is_integer(S) ->
    H * ?SECONDS_PER_HOUR +
	M * ?SECONDS_PER_MINUTE + S.
      

%% universal_time()
%%
%% Returns: {date(), time()}, date() = {Y, M, D}, time() = {H, M, S}.
-spec universal_time() -> t_datetime().
universal_time() ->
    erlang:universaltime().
 

%% universal_time_to_local_time(DateTime)
%%
-spec universal_time_to_local_time(t_datetime()) -> t_datetime().
universal_time_to_local_time(DateTime) ->
    erlang:universaltime_to_localtime(DateTime).


%% valid_date(Year, Month, Day) = true | false
%% valid_date({Year, Month, Day}) = true | false
%%
-spec valid_date(integer(), integer(), integer()) -> boolean().
valid_date(Y, M, D) when is_integer(Y), is_integer(M), is_integer(D) ->
    valid_date1(Y, M, D).

-spec valid_date1(integer(), integer(), integer()) -> boolean().
valid_date1(Y, M, D) when Y >= 0, M > 0, M < 13, D > 0 ->
    D =< last_day_of_the_month(Y, M);
valid_date1(_, _, _) ->
    false.

-spec valid_date({integer(),integer(),integer()}) -> boolean().
valid_date({Y, M, D}) ->
    valid_date(Y, M, D).


%%
%%  LOCAL FUNCTIONS
%%
-type day_of_year() :: 0..365.

%% day_to_year(DayOfEpoch) = {Year, DayOfYear}
%%
%% The idea here is to first guess a year, and then adjust. Although
%% the implementation is recursive, at most 1 or 2 recursive steps
%% are taken.
%% If DayOfEpoch is very large, we need far more than 1 or 2 iterations,
%% since we just subtract a yearful of days at a time until we're there.
%%
-spec day_to_year(non_neg_integer()) -> {year(), day_of_year()}.
day_to_year(DayOfEpoch) when DayOfEpoch >= 0 ->
    Y0 = DayOfEpoch div ?DAYS_PER_YEAR,
    {Y1, D1} = dty(Y0, DayOfEpoch, dy(Y0)),
    {Y1, DayOfEpoch - D1}.

-spec dty(year(), non_neg_integer(), non_neg_integer()) ->
		{year(), non_neg_integer()}.
dty(Y, D1, D2) when D1 < D2 -> 
    dty(Y-1, D1, dy(Y-1));
dty(Y, _D1, D2) ->
    {Y, D2}.

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
