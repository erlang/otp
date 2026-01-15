%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
-module(calendar_prop).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

between_40_years_ago_and_in_40_years(Unit) ->
    integer(erlang:system_time(Unit) - erlang:convert_time_unit(40*60*60*24*365, second, Unit),
            erlang:system_time(Unit) + erlang:convert_time_unit(40*60*60*24*365, second, Unit)).

unit() ->
    proper_types:oneof([second,
                        millisecond,
                        microsecond,
                        nanosecond,
                        native]).

rfc3339_lists_binaries() ->
    Unit = millisecond,
    Ms = [{unit, Unit}],
    ?FORALL(
        TS,
        between_40_years_ago_and_in_40_years(Unit),
        begin
            DateTimeString = calendar:system_time_to_rfc3339(TS, Ms),
            DateTimeBin = calendar:system_time_to_rfc3339(TS, [{return, binary} | Ms]),
            ListToBinary = erlang:list_to_binary(DateTimeString),
            FromStr = calendar:rfc3339_to_system_time(DateTimeString, Ms),
            FromBin = calendar:rfc3339_to_system_time(DateTimeBin, Ms),
            DateTimeBin =:= ListToBinary andalso FromStr =:= FromBin
        end
    ).

universal_time_system_time_symmetry() ->
    ?FORALL(
        {SystemTime0, Unit},
        ?LET(Unit,
             unit(),
             {between_40_years_ago_and_in_40_years(Unit), Unit}),
        begin
            Options = [{unit, Unit}],
            UTime = calendar:system_time_to_universal_time(SystemTime0, Unit),
            SystemTime = calendar:universal_time_to_system_time(UTime, Options),
            loss(SystemTime0, Unit) =:= (SystemTime0 - SystemTime)
        end
    ).

local_time_system_time_symmetry() ->
    ?FORALL(
        {SystemTime0, Unit},
        ?LET(Unit,
             unit(),
             {between_40_years_ago_and_in_40_years(Unit), Unit}),
        begin
            Options = [{unit, Unit}],
            UTime = calendar:system_time_to_local_time(SystemTime0, Unit),
            SystemTime = calendar:local_time_to_system_time(UTime, Options),
            loss(SystemTime0, Unit) =:= (SystemTime0 - SystemTime)
        end
    ).

loss(_SystemTime, second) -> 0;
loss(SystemTime, millisecond) -> SystemTime rem 1_000;
loss(SystemTime, microsecond) -> SystemTime rem 1_000_000;
loss(SystemTime, nanosecond) -> SystemTime rem 1_000_000_000;
loss(SystemTime, native) -> loss(erlang:convert_time_unit(SystemTime, native, nanosecond), nanosecond).

%% Property: date_to_gregorian_days and gregorian_days_to_date are inverses
%% Includes negative days (dates before year 0)
gregorian_days_roundtrip() ->
    ?FORALL(
        Days,
        integer(-1_000_000, 4_000_000),  % Covers year ~-2738 to ~10950
        begin
            Date = calendar:gregorian_days_to_date(Days),
            Days =:= calendar:date_to_gregorian_days(Date)
        end
    ).

%% Property: date_to_gregorian_days produces strictly increasing values
gregorian_days_monotonic() ->
    ?FORALL(
        {Year, Month, Day},
        valid_date(),
        begin
            Days1 = calendar:date_to_gregorian_days(Year, Month, Day),
            %% Next day should be Days1 + 1
            {Y2, M2, D2} = next_day(Year, Month, Day),
            Days2 = calendar:date_to_gregorian_days(Y2, M2, D2),
            Days2 =:= Days1 + 1
        end
    ).

%% Property: day_of_the_week cycles correctly (1-7, Monday-Sunday)
%% Includes negative days (dates before year 0)
day_of_week_cycle() ->
    ?FORALL(
        Days,
        integer(-500_000, 1_000_000),
        begin
            DOW1 = calendar:day_of_the_week(calendar:gregorian_days_to_date(Days)),
            DOW2 = calendar:day_of_the_week(calendar:gregorian_days_to_date(Days + 7)),
            DOW1 =:= DOW2 andalso DOW1 >= 1 andalso DOW1 =< 7
        end
    ).

%% Property: leap years have 366 days, non-leap years have 365 days
%% Includes negative years
year_length() ->
    ?FORALL(
        Year,
        integer(-2000, 10000),
        begin
            Jan1 = calendar:date_to_gregorian_days(Year, 1, 1),
            Dec31 = calendar:date_to_gregorian_days(Year, 12, 31),
            YearLength = Dec31 - Jan1 + 1,
            ExpectedLength = case calendar:is_leap_year(Year) of
                                 true -> 366;
                                 false -> 365
                             end,
            YearLength =:= ExpectedLength
        end
    ).

%% Generator for valid dates (including negative years)
valid_date() ->
    ?LET(Year, integer(-2000, 9999),
         ?LET(Month, integer(1, 12),
              ?LET(Day, integer(1, calendar:last_day_of_the_month(Year, Month)),
                   {Year, Month, Day}))).

%% Helper: compute next day
next_day(Year, Month, Day) ->
    LastDay = calendar:last_day_of_the_month(Year, Month),
    if
        Day < LastDay ->
            {Year, Month, Day + 1};
        Month < 12 ->
            {Year, Month + 1, 1};
        true ->
            {Year + 1, 1, 1}
    end.

%% Property: leap year rules work correctly for negative years
negative_leap_year() ->
    ?FORALL(
        Year,
        integer(-10000, -1),
        begin
            IsLeap = calendar:is_leap_year(Year),
            Expected = (Year rem 4 =:= 0) andalso
                       ((Year rem 100 =/= 0) orelse (Year rem 400 =:= 0)),
            IsLeap =:= Expected
        end
    ).

%% Property: gregorian_seconds roundtrip works for negative seconds
gregorian_seconds_roundtrip() ->
    ?FORALL(
        Secs,
        integer(-100_000_000, 100_000_000),
        begin
            DateTime = calendar:gregorian_seconds_to_datetime(Secs),
            Secs =:= calendar:datetime_to_gregorian_seconds(DateTime)
        end
    ).
