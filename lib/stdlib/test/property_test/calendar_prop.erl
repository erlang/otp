%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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

between_40_years_ago_and_in_40_years() ->
    integer(erlang:system_time(millisecond) - 40*1000*60*60*24*365,
            erlang:system_time(millisecond) + 40*1000*60*60*24*365).

rfc3339_lists_binaries() ->
    Ms = [{unit, millisecond}],
    ?FORALL(
        TS,
        between_40_years_ago_and_in_40_years(),
        begin
            DateTimeString = calendar:system_time_to_rfc3339(TS, Ms),
            DateTimeBin = calendar:system_time_to_rfc3339(TS, [{return, binary} | Ms]),
            ListToBinary = erlang:list_to_binary(DateTimeString),
            FromStr = calendar:rfc3339_to_system_time(DateTimeString, Ms),
            FromBin = calendar:rfc3339_to_system_time(DateTimeBin, Ms),
            DateTimeBin =:= ListToBinary andalso FromStr =:= FromBin
        end
    ).
