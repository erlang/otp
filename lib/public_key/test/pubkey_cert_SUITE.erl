%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2022. All Rights Reserved.
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
%% -include_lib("public_key/include/public_key.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, time_str_2_gregorian_sec}].

groups() ->
    [{time_str_2_gregorian_sec, [], time_str_two_gregorian()}].

time_str_two_gregorian() ->
    [ time_str_2_gregorian_utc_post2000
    , time_str_2_gregorian_utc_limit_pre2000
    , time_str_2_gregorian_utc_limit_post2000
    , time_str_2_gregorian_generaltime_pre2000
    , time_str_2_gregorian_generaltime_post2000
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


time_str_2_gregorian_utc_limit_pre2000() ->
    [{doc, "Tests a valid gregorian Utc time"}].
time_str_2_gregorian_utc_limit_pre2000(_) ->
    YYMMDD = "720101",
    HHMMSSZ = "000000Z",
    ExpectedYear = 1972,
    UtcTime = {utcTime, YYMMDD ++ HHMMSSZ},
    {ExpectedDate, _} = convert_to_datetime_format(UtcTime, ExpectedYear),

    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    {ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(Result).


time_str_2_gregorian_utc_limit_post2000() ->
    [{doc, "Tests a valid gregorian Utc time"}].
time_str_2_gregorian_utc_limit_post2000(_) ->
    YYMMDD = "710101",
    HHMMSSZ = "000000Z",
    ExpectedYear = 2071,
    UtcTime = {utcTime, YYMMDD ++ HHMMSSZ},
    {ExpectedDate, _} = convert_to_datetime_format(UtcTime, ExpectedYear),

    Result = pubkey_cert:time_str_2_gregorian_sec(UtcTime),
    {ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(Result).


time_str_2_gregorian_generaltime_pre2000() ->
    [{doc, "Tests a valid gregorian Utc time"}].
time_str_2_gregorian_generaltime_pre2000(_) ->
    [Year | _]=YYMMDD = ["1972", "01", "01"],
    HHMMSSZ = "000000Z",
    GeneralTime = {generalTime, lists:flatten(YYMMDD) ++ HHMMSSZ},
    {ExpectedYear, _} = convert_to_datetime_format(GeneralTime
                                                  , erlang:list_to_integer(Year)),

    Result = pubkey_cert:time_str_2_gregorian_sec(GeneralTime),
    {ExpectedYear, _} = calendar:gregorian_seconds_to_datetime(Result).


time_str_2_gregorian_generaltime_post2000() ->
    [{doc, "Tests a valid gregorian Utc time"}].
time_str_2_gregorian_generaltime_post2000(_) ->
    [Year | _]=YYMMDD = ["2045", "01", "01"],
    HHMMSSZ = "000000Z",
    GeneralTime = {generalTime, lists:flatten(YYMMDD) ++ HHMMSSZ},
    {ExpectedYear, _} = convert_to_datetime_format(GeneralTime
                                                  , erlang:list_to_integer(Year)),

    Result = pubkey_cert:time_str_2_gregorian_sec(GeneralTime),
    {ExpectedYear, _} = calendar:gregorian_seconds_to_datetime(Result).


convert_to_datetime_format({utcTime, [Y1, Y2, M1, M2, D1, D2 | _]}, ExpectedYear) ->
    YYMMDD = [[Y1, Y2], [M1, M2], [D1, D2]],
    [Y, M, D] = lists:map(fun (Str) -> erlang:list_to_integer(Str) end
                         , YYMMDD),
    case (ExpectedYear rem 100) =:= Y of
        true -> {{ExpectedYear, M, D}, {0, 0, 0}};
        false -> error
    end;

convert_to_datetime_format({generalTime, [Y1, Y2, Y3, Y4, M1, M2, D1, D2 | _]}, ExpectedYear) ->
    YYMMDD = [[Y1, Y2, Y3, Y4], [M1, M2], [D1, D2]],
    [ExpectedYear, M, D] = lists:map(fun (Str) -> erlang:list_to_integer(Str) end
                                    , YYMMDD),
    {{ExpectedYear, M, D}, {0, 0, 0}}.
