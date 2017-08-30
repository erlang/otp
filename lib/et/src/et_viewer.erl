%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Displays a sequence chart for trace events (messages/actions)
%%----------------------------------------------------------------------

-module(et_viewer).


%% External exports
-export([file/1,
         start/0,
         start/1,
         start/2,
	 start_link/1,
	 start_link/2,
	 open_event/2,
	 stop/1, 
         get_collector_pid/1]).

-include("../include/et.hrl").
-include("et_internal.hrl").

-define(unknown, "UNKNOWN").


%%%----------------------------------------------------------------------
%%% Client side
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% file(FileName) -> {ok, ViewerPid} | {error, Reason}
%%
%% Start a new event viewer and a corresponding collector
%% and load them with trace events from a trace file.
%%
%% FileName() = string()
%% ViewerPid = pid()
%% Reason = term()
%%----------------------------------------------------------------------

file(FileName) ->
    start_link([{trace_client, {file, FileName}}], default).

%%----------------------------------------------------------------------
%% start() -> ok
%% 
%% Simplified start of a sequence chart viewer with
%% global tracing activated.
%%
%% Convenient to be used from the command line
%% (erl -s et_viewer) as both the viewer and collector
%% processes are unlinked from the calling process.
%%----------------------------------------------------------------------

start() ->
    start([{trace_global, true}], default).

%%----------------------------------------------------------------------
%% start(Options) -> {ok, ViewerPid} | {error, Reason}
%%----------------------------------------------------------------------

start(GUI) when GUI =:= wx; GUI =:= default ->
    start_link([{trace_global, true}], GUI);
start(Options) ->
    start_link([{parent_pid, undefined} | Options], default).

start(Options, GUI) ->
    start_link([{parent_pid, undefined} | Options], GUI).

%%----------------------------------------------------------------------
%% start_link(Options) -> {ok, ViewerPid} | {error, Reason}
%%
%% Start a sequence chart viewer for trace events (messages/actions)
%% 
%% Options = [option() | collector_option()]
%%
%% option() =
%%   {parent_pid, extended_pid()} |
%%   {title, term()} |
%%   {detail_level, detail_level()} |
%%   {is_suspended, boolean()} |
%%   {scale, integer()} |
%%   {width, integer()} |
%%   {height, integer()} |
%%   {collector_pid, extended_pid()} |
%%   {event_order, event_order()} |
%%   {active_filter, atom()} |
%%   {max_events, extended_integer()} |
%%   {max_actors, extended_integer()} |
%%   {trace_global, et_collector_trace_global()} |
%%   {trace_pattern, et_collector_trace_pattern()} |
%%   {trace_port, et_collector_trace_port()} |
%%   {trace_max_queue, et_collector_trace_max_queue()} |
%%   {trace_client, et_collector_trace_client()} |
%%   {dict_insert, {filter, filter_name()}, event_filter_fun()} |
%%   {dict_insert, et_collector_dict_key(), et_collector_dict_val()} |
%%   {dict_delete, {filter, filter_name()}} |
%%   {dict_delete, et_collector_dict_key()} |
%%   {actors, actors()} |
%%   {first_event, first_key()} |
%%   {hide_unknown, boolean()} |
%%   {hide_actions, boolean()} |
%%   {display_mode, display_mode()}
%%   
%% extended_pid() = pid() | undefined
%% detail_level() = min | max | integer(X) when X >=0, X =< 100
%% event_order() = trace_ts | event_ts
%% extended_integer() = integer() | infinity
%% display_mode() = all | {search_actors, direction(), first_key(), actors()}
%% direction() = forward | reverse
%% first_key() = event_key()
%% actors() = [term()]
%% 
%% filter_name() = atom()
%% filter_fun() =  fun(Event) -> false | true | {true, NewEvent}
%% Event = NewEvent = record(event)
%%
%% ViewerPid = pid()
%% Reason = term()
%%
%% A filter_fun() takes an event record as sole argument
%% and returns false | true | {true, NewEvent}.
%%----------------------------------------------------------------------

start_link(GUI) when GUI =:= wx; GUI =:= default ->
    start_link([{trace_global, true}], GUI);
start_link(Options) ->
    start_link(Options, default).

start_link(Options, GUI) -> 
    case GUI of
	wx ->
	    et_wx_viewer:start_link(Options);
	default ->
	    start_link(Options, which_gui())
    end.

which_gui() -> wx.

get_collector_pid(ViewerPid) ->
    call(ViewerPid, get_collector_pid).

%%----------------------------------------------------------------------
%% stop(ViewerPid) -> ok
%% 
%% Stops a viewer
%% 
%% ViewerPid = pid()
%%----------------------------------------------------------------------

stop(ViewerPid) ->
    call(ViewerPid, stop).


%%----------------------------------------------------------------------

open_event(ViewerPid, N) ->
    call(ViewerPid, {open_event, N}).

%%----------------------------------------------------------------------

call(ViewerPid, Request) ->
    gen_server:call(ViewerPid, Request, infinity).

