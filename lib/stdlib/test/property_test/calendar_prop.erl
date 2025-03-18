%%
%% %CopyrightBegin%
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
