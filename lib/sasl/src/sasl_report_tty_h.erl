%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(sasl_report_tty_h).

%%%
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all sasl_* events formatted to stdout.
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

init(Type) ->
% should link to user (or group_leader???)
    {ok, Type}.
    
handle_event({Type, GL, _Msg}, Type) when node(GL) /= node() ->
    {ok, Type};
handle_event(Event, Type) ->
    _ = sasl_report:write_report(standard_io, Type, tag_event(Event)),
    {ok, Type}.

handle_info(_, Type) -> {ok, Type}.

handle_call(_Query, _Type) -> {error, bad_query}.

terminate(_Reason, _Type) ->
    [].

tag_event(Event) ->    
    {calendar:local_time(), Event}.

