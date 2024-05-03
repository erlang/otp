%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2023. All Rights Reserved.
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

-module(pubkey_cert_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0,
         groups/0]).

%% Test cases
-export([time_str_2_gregorian_utc_post2000/0,
         time_str_2_gregorian_utc_post2000/1,
         time_str_2_gregorian_utc_limit_50_years_before_current_time/0,
         time_str_2_gregorian_utc_limit_50_years_before_current_time/1,
         time_str_2_gregorian_utc_limit_51_years_before_current_time/0,
         time_str_2_gregorian_utc_limit_51_years_before_current_time/1,
         time_str_2_gregorian_utc_limit_50_years_from_current_time/0,
         time_str_2_gregorian_utc_limit_50_years_from_current_time/1,
         time_str_2_gregorian_utc_limit_49_years_from_current_time/0,
         time_str_2_gregorian_utc_limit_49_years_from_current_time/1,
         time_str_2_gregorian_generaltime_50_years_before_current_time/0,
         time_str_2_gregorian_generaltime_50_years_before_current_time/1,
         time_str_2_gregorian_generaltime_50_years_from_current_time/0,
         time_str_2_gregorian_generaltime_50_years_from_current_time/1
        ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, time_str_2_gregorian_sec}].

groups() ->
    [{time_str_2_gregorian_sec, [], time_str_two_gregorian()}].

time_str_two_gregorian() ->
    [time_str_2_gregorian_utc_post2000,
     time_str_2_gregorian_utc_limit_50_years_before_current_time,
     time_str_2_gregorian_utc_limit_51_years_before_current_time,
     time_str_2_gregorian_utc_limit_50_years_from_current_time,
     time_str_2_gregorian_utc_limit_49_years_from_current_time,
     time_str_2_gregorian_generaltime_50_years_before_current_time,
     time_str_2_gregorian_generaltime_50_years_from_current_time
    ].
%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
time_str_2_gregorian_utc_post2000() ->
    [{doc, "Tests a valid gregorian Utc time"}].
time_str_2_gregorian_utc_post2000(_) ->
    YYMMDD = "450101",
    HHMMSSZ = "000000Z",
    ExpectedYear = 2045,
    UtcTime = {utcTime, YYMMDD ++ HHMMSSZ},
    {ExpectedDate, _} = convert_to_datetime_format(UtcTime, ExpectedYear),

    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    {ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(Result).


time_str_2_gregorian_utc_limit_50_years_before_current_time() ->
    [{doc, "Tests limit of gregorian Utc time 50 years before current time"}].
time_str_2_gregorian_utc_limit_50_years_before_current_time(_) ->
    {ExpectedDate, UtcTime} = get_date(utcTime, -50),
    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    {ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(Result).

time_str_2_gregorian_utc_limit_51_years_before_current_time() ->
    [{doc, "Tests limit of gregorian Utc time 51 years before current time"}].
time_str_2_gregorian_utc_limit_51_years_before_current_time(_) ->
    {{Y, M, D}, UtcTime} = get_date(utcTime, -51),
    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    %% the sliding window method from pubkey_cert reaches its limit and
    %% reverses the year from 19XX to 20XX. Because of this, the expected
    %% year is current_year + 50, or ExpectedYear + 100 (they are equivalent)
    ExpectedYear = Y + 100,
    {{ExpectedYear, M, D}, _} = calendar:gregorian_seconds_to_datetime(Result).

time_str_2_gregorian_utc_limit_50_years_from_current_time() ->
    [{doc, "Tests a valid gregorian Utc time 50 years from now"}].
time_str_2_gregorian_utc_limit_50_years_from_current_time(_) ->
    {{Y, M, D}, UtcTime} = get_date(utcTime, 50),
    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    %% the sliding window method from pubkey_cert reaches its limit and
    %% reverses the year from 20XX to 19XX. Because of this, the expected
    %% year is current_year - 50, or ExpectedYear - 100 (they are equivalent)
    ExpectedYear = Y - 100,
    {{ExpectedYear, M, D}, _} = calendar:gregorian_seconds_to_datetime(Result).

time_str_2_gregorian_utc_limit_49_years_from_current_time() ->
    [{doc, "Tests a valid gregorian Utc time 49 years from now"}].
time_str_2_gregorian_utc_limit_49_years_from_current_time(_) ->
    {ExpectedDate, UtcTime} = get_date(utcTime, 49),
    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    {ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(Result).


time_str_2_gregorian_generaltime_50_years_before_current_time() ->
    [{doc, "Tests a valid general time 50 years before current time"}].
time_str_2_gregorian_generaltime_50_years_before_current_time(_) ->
    {ExpectedDate, UtcTime} = get_date(generalTime, -50),
    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    {ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(Result).


time_str_2_gregorian_generaltime_50_years_from_current_time() ->
    [{doc, "Tests a valid general time 50 years from now"}].
time_str_2_gregorian_generaltime_50_years_from_current_time(_) ->
    {ExpectedDate, UtcTime} = get_date(generalTime, 50),
    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    {ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Helper functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec convert_to_datetime_format({Format, Date}, ExpectedYear) -> Result when
      Format :: generalTime | utcTime,
      Date :: YYMMDDHHMMSS | YYYYMMDDHHMMSS,
      YYMMDDHHMMSS :: string(),
      YYYYMMDDHHMMSS :: string(),
      ExpectedYear :: non_neg_integer(),
      Result :: {{non_neg_integer(), 1..12, 1..31}, {0, 0, 0}}.
convert_to_datetime_format({Format, Date}, ExpectedYear) ->
    YYMMDD = group_year(Format, Date),
    [Y, M, D] = lists:map(fun (Str) -> erlang:list_to_integer(Str) end, YYMMDD),
    %% assertions to test that the result is the expected one
    case Format of
        utcTime -> true = (ExpectedYear rem 100) == Y;
        generalTime -> true = ExpectedYear == Y
    end,
    {{ExpectedYear, M, D}, {0, 0, 0}}.

-spec get_date(Format, YearsFromNow) -> {{Year, Month, Day}, UtcDate} when
      Format :: utcTime | generalTime,
      YearsFromNow :: integer(),
      Year :: non_neg_integer(),
      Month :: 1..12,
      Day :: 1..31,
      UtcDate :: string().
get_date(Format, Years) ->
    {YYYY, MM0, DD0} = date(),
    MM = io_lib:format("~2..0w", [MM0]),
    DD = io_lib:format("~2..0w", [DD0]),
    ExpectedYear = YYYY + Years,
    YYMMDD = format_year(Format, ExpectedYear, {MM, DD}),
    HHMMSSZ = "000000Z",
    FormattedTime = {Format, YYMMDD ++ HHMMSSZ},
    {ExpectedDate, _} = convert_to_datetime_format(FormattedTime, ExpectedYear),
    {ExpectedDate, FormattedTime}.

-spec format_year(Format, ExpectedYear, {Month, Day}) -> YYMMDD | YYYYMMDD when
      Format :: utcTime | generalTime,
      ExpectedYear :: non_neg_integer(),
      Month :: string(),
      Day :: string(),
      YYMMDD :: string(),
      YYYYMMDD :: string().
format_year(utcTime, ExpectedYear, {MM, DD}) ->
    YY = erlang:integer_to_list((ExpectedYear) rem 100),
    lists:flatten(YY++MM++DD);
format_year(generalTime, ExpectedYear, {MM, DD}) ->
    YY = erlang:integer_to_list(ExpectedYear),
    lists:flatten(YY++MM++DD).

-spec group_year(Format, Date) -> [list()] when
      Format :: utcTime | generalTime,
      Date :: [non_neg_integer()].
group_year(utcTime, [Y1, Y2, M1, M2, D1, D2 | _]) ->
    [[Y1, Y2], [M1, M2], [D1, D2]];
group_year(generalTime, [Y1, Y2, Y3, Y4, M1, M2, D1, D2 | _]) ->
    [[Y1, Y2, Y3, Y4], [M1, M2], [D1, D2]].
