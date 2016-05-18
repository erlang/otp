%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%%-------------------------------------------------------------------
%% Interface
%%-------------------------------------------------------------------

%% public functions
-export([
	 error_msg/2,
	 warning_msg/2,
	 info_msg/2
	]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{error_msg, 2}, {warning_msg, 2}, {info_msg, 2}];
behaviour_info(_) ->
    undefined.

%%-------------------------------------------------------------------
%% error_msg(Format, Data) -> ok | exit(Reason)
%% 
%% Format = string()
%% Data = [term()]
%% Reason = term()
%%
%% Log an error message
%%-------------------------------------------------------------------

error_msg(Format, Data) ->
    {Format2, Data2} = add_timestamp(Format, Data),
    error_logger:error_msg(Format2, Data2).

%%-------------------------------------------------------------------
%% warning_msg(Format, Data) -> ok | exit(Reason)
%% 
%% Format = string()
%% Data = [term()]
%% Reason = term()
%%
%% Log a warning message
%%-------------------------------------------------------------------

warning_msg(Format, Data) ->
    {Format2, Data2} = add_timestamp(Format, Data),
    error_logger:warning_msg(Format2, Data2).

%%-------------------------------------------------------------------
%% info_msg(Format, Data) -> ok | exit(Reason)
%% 
%% Format = string()
%% Data = [term()]
%% Reason = term()
%%
%% Log an info message
%%-------------------------------------------------------------------

info_msg(Format, Data) ->
    {Format2, Data2} = add_timestamp(Format, Data),
    io:format(Format2, Data2).

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
