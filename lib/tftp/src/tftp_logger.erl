%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%%
-module(tftp_logger).
-moduledoc """
Trivial FTP logger.

A `tftp_logger` callback module is to be implemented as a `tftp_logger` behavior
and export the following functions:
""".
-moduledoc(#{since => "OTP 18.1"}).

%%-------------------------------------------------------------------
%% Interface
%%-------------------------------------------------------------------

%% public functions
-export([
	 error_msg/2,
	 warning_msg/2,
	 info_msg/2
	]).

-doc "Logs a warning message. See `logger:warning/2` for details.".
-doc(#{since => <<"OTP 18.1">>}).
-callback warning_msg(Format :: io:format(), Args :: [term()]) -> ok.
-doc "Logs an info message. See `logger:info/2` for details.".
-doc(#{since => <<"OTP 18.1">>}).
-callback info_msg(Format :: io:format(), Args :: [term()]) -> ok.
-doc "Logs an error message. See `logger:error/2` for details.".
-doc(#{since => <<"OTP 18.1">>}).
-callback error_msg(Format :: io:format(), Args :: [term()]) -> ok.

-optional_callbacks([warning_msg/2, error_msg/2, info_msg/2]).

%%-------------------------------------------------------------------
%% error_msg(Format, Data) -> ok | exit(Reason)
%% 
%% Format = string()
%% Data = [term()]
%% Reason = term()
%%
%% Log an error message
%%-------------------------------------------------------------------

-doc false.
error_msg(Format, Data) ->
    {Format2, Data2} = add_timestamp(Format, Data),
    logger:error(Format2, Data2).

%%-------------------------------------------------------------------
%% warning_msg(Format, Data) -> ok | exit(Reason)
%% 
%% Format = string()
%% Data = [term()]
%% Reason = term()
%%
%% Log a warning message
%%-------------------------------------------------------------------

-doc false.
warning_msg(Format, Data) ->
    {Format2, Data2} = add_timestamp(Format, Data),
    logger:warning(Format2, Data2).

%%-------------------------------------------------------------------
%% info_msg(Format, Data) -> ok | exit(Reason)
%% 
%% Format = string()
%% Data = [term()]
%% Reason = term()
%%
%% Log an info message
%%-------------------------------------------------------------------

-doc false.
info_msg(Format, Data) ->
    {Format2, Data2} = add_timestamp(Format, Data),
    logger:info(Format2, Data2).

%%-------------------------------------------------------------------
%% Add timestamp to log message
%%-------------------------------------------------------------------

add_timestamp(Format, Data) ->
    Time = erlang:timestamp(),
    {{_Y, _Mo, _D}, {H, Mi, S}} = calendar:now_to_universal_time(Time),
    %% {"~p-~s-~sT~s:~s:~sZ,~6.6.0w tftp: " ++ Format ++ "\n", 
    %%  [Y, t(Mo), t(D), t(H), t(Mi), t(S), MicroSecs | Data]}.
    {"~s:~s:~s tftp: " ++ Format, [t(H), t(Mi), t(S) | Data]}.

%% Convert 9 to "09".
t(Int) ->
    case integer_to_list(Int) of
	[Single] -> [$0, Single];
        Multi    -> Multi
    end.
