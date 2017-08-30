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
-module(alarm_handler).

%%%-----------------------------------------------------------------
%%% This is the SASL alarm handler process.
%%% It is a gen_event process.  When it is started, a simple
%%% event handler which logs all alarms is installed.
%%%-----------------------------------------------------------------
-export([start_link/0, set_alarm/1, clear_alarm/1, get_alarms/0,
	 add_alarm_handler/1, add_alarm_handler/2,
	 delete_alarm_handler/1]).

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

start_link() ->
    case gen_event:start_link({local, alarm_handler}) of
	{ok, Pid} ->
	    gen_event:add_handler(alarm_handler, alarm_handler, []),
	    {ok, Pid};
	Error -> Error
    end.

%%-----------------------------------------------------------------
%% Func: set_alarm/1
%% Args: Alarm ::= {AlarmId, term()}
%%       where AlarmId ::= term()
%%-----------------------------------------------------------------
set_alarm(Alarm) ->
    gen_event:notify(alarm_handler, {set_alarm, Alarm}).

%%-----------------------------------------------------------------
%% Func: clear_alarm/1
%% Args: AlarmId ::= term()
%%-----------------------------------------------------------------
clear_alarm(AlarmId) ->
    gen_event:notify(alarm_handler, {clear_alarm, AlarmId}).

%%-----------------------------------------------------------------
%% Func: get_alarms/0
%% Returns: [{AlarmId, AlarmDesc}]
%%-----------------------------------------------------------------
get_alarms() ->
    gen_event:call(alarm_handler, alarm_handler, get_alarms).

add_alarm_handler(Module) when is_atom(Module) ->
    gen_event:add_handler(alarm_handler, Module, []).

add_alarm_handler(Module, Args) when is_atom(Module) ->
    gen_event:add_handler(alarm_handler, Module, Args).

delete_alarm_handler(Module) when is_atom(Module) ->
    gen_event:delete_handler(alarm_handler, Module, []).

%%-----------------------------------------------------------------
%% Default Alarm handler
%%-----------------------------------------------------------------
init(_) -> {ok, []}.
    
handle_event({set_alarm, Alarm}, Alarms)->
    error_logger:info_report([{alarm_handler, {set, Alarm}}]),
    {ok, [Alarm | Alarms]};
handle_event({clear_alarm, AlarmId}, Alarms)->
    error_logger:info_report([{alarm_handler, {clear, AlarmId}}]),
    {ok, lists:keydelete(AlarmId, 1, Alarms)};
handle_event(_, Alarms)->
    {ok, Alarms}.

handle_info(_, Alarms) -> {ok, Alarms}.

handle_call(get_alarms, Alarms) -> {ok, Alarms, Alarms};
handle_call(_Query, Alarms)     -> {ok, {error, bad_query}, Alarms}.

terminate(swap, Alarms) ->
    {alarm_handler, Alarms};
terminate(_, _) ->
    ok.
