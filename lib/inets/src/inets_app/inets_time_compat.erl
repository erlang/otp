%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2015. All Rights Reserved.
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

%% This module is created to be able to execute on ERTS versions both
%% earlier and later than 7.0.

-module(inets_time_compat).

%% We don't want warnings about the use of erlang:now/0 in
%% this module.
-compile(nowarn_deprecated_function).

-export([monotonic_time/0,
         timestamp/0,
         unique_integer/0,
         unique_integer/1]).

monotonic_time() ->
    try
	erlang:monotonic_time()
    catch
	error:undef ->
	    %% Use Erlang system time as monotonic time
	    erlang_system_time_fallback()
    end.

timestamp() ->
    try
	erlang:timestamp()
    catch
	error:undef ->
	    erlang:now()
    end.

unique_integer() ->
    try
	erlang:unique_integer()
    catch
	error:undef ->
            erlang_system_time_fallback()
    end.

unique_integer(Modifiers) ->
    try
	erlang:unique_integer(Modifiers)
    catch
	error:badarg ->
	    erlang:error(badarg, [Modifiers]);
	error:undef ->
            erlang_system_time_fallback()
    end.

erlang_system_time_fallback() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.
