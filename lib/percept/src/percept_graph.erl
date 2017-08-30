%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

%% @doc Interface for CGI request on graphs used by percept. The module exports two functions that are implementations for ESI callbacks used by the httpd server. See http://www.erlang.org//doc/apps/inets/index.html.

-module(percept_graph).
-export([proc_lifetime/3, graph/3, scheduler_graph/3, activity/3, percentage/3]).

-include("percept.hrl").
-include_lib("kernel/include/file.hrl").

%% API

%% graph
%% @spec graph(SessionID, Env, Input) -> term()
%% @doc An ESI callback implementation used by the httpd server. 
%% 

graph(SessionID, Env, Input) ->
    ok = mod_esi:deliver(SessionID, header()),
    ok = mod_esi:deliver(SessionID, binary_to_list(graph(Env, Input))).
 
%% activity
%% @spec activity(SessionID, Env, Input) -> term() 
%% @doc An ESI callback implementation used by the httpd server.

activity(SessionID, Env, Input) ->
    ok = mod_esi:deliver(SessionID, header()),
    ok = mod_esi:deliver(SessionID, binary_to_list(activity_bar(Env, Input))).

proc_lifetime(SessionID, Env, Input) ->
    ok = mod_esi:deliver(SessionID, header()),
    ok = mod_esi:deliver(SessionID, binary_to_list(proc_lifetime(Env, Input))).

percentage(SessionID, Env, Input) ->
    ok = mod_esi:deliver(SessionID, header()),
    ok = mod_esi:deliver(SessionID, binary_to_list(percentage(Env,Input))).

scheduler_graph(SessionID, Env, Input) ->
    ok = mod_esi:deliver(SessionID, header()),
    ok = mod_esi:deliver(SessionID, binary_to_list(scheduler_graph(Env, Input))).

graph(_Env, Input) ->
    Query    = httpd:parse_query(Input),
    RangeMin = percept_html:get_option_value("range_min", Query),
    RangeMax = percept_html:get_option_value("range_max", Query),
    Pids     = percept_html:get_option_value("pids", Query),
    Width    = percept_html:get_option_value("width", Query),
    Height   = percept_html:get_option_value("height", Query),
    
    % Convert Pids to id option list
    IDs      = [ {id, ID} || ID <- Pids],

    % seconds2ts
    StartTs  = percept_db:select({system, start_ts}),
    TsMin    = percept_analyzer:seconds2ts(RangeMin, StartTs),
    TsMax    = percept_analyzer:seconds2ts(RangeMax, StartTs),
    
    Options  = [{ts_min, TsMin},{ts_max, TsMax} | IDs],
    
    Acts     = percept_db:select({activity, Options}),
    Counts   = case IDs of
	[] -> percept_analyzer:activities2count(Acts, StartTs);
	_  -> percept_analyzer:activities2count2(Acts, StartTs)
    end,

    percept_image:graph(Width, Height,Counts).

scheduler_graph(_Env, Input) -> 
    Query    = httpd:parse_query(Input),
    RangeMin = percept_html:get_option_value("range_min", Query),
    RangeMax = percept_html:get_option_value("range_max", Query),
    Width    = percept_html:get_option_value("width", Query),
    Height   = percept_html:get_option_value("height", Query),
    
    StartTs  = percept_db:select({system, start_ts}),
    TsMin    = percept_analyzer:seconds2ts(RangeMin, StartTs),
    TsMax    = percept_analyzer:seconds2ts(RangeMax, StartTs),
    

    Acts     = percept_db:select({scheduler, [{ts_min, TsMin}, {ts_max,TsMax}]}),
    
    Counts   = [{?seconds(Ts, StartTs), Scheds, 0} || #activity{where = Scheds, timestamp = Ts} <- Acts],

    percept_image:graph(Width, Height, Counts).

activity_bar(_Env, Input) ->
    Query  = httpd:parse_query(Input),
    Pid    = percept_html:get_option_value("pid", Query),
    Min    = percept_html:get_option_value("range_min", Query),
    Max    = percept_html:get_option_value("range_max", Query),
    Width  = percept_html:get_option_value("width", Query),
    Height = percept_html:get_option_value("height", Query),
    
    Data    = percept_db:select({activity, [{id, Pid}]}),
    StartTs = percept_db:select({system, start_ts}),
    Activities = [{?seconds(Ts, StartTs), State} || #activity{timestamp = Ts, state = State} <- Data],
    
    percept_image:activities(Width, Height, {Min,Max},Activities).

proc_lifetime(_Env, Input) ->
    Query = httpd:parse_query(Input),
    ProfileTime = percept_html:get_option_value("profiletime", Query),
    Start = percept_html:get_option_value("start", Query),
    End = percept_html:get_option_value("end", Query),
    Width = percept_html:get_option_value("width", Query),
    Height = percept_html:get_option_value("height", Query),
    percept_image:proc_lifetime(round(Width), round(Height), float(Start), float(End), float(ProfileTime)).

percentage(_Env, Input) ->
    Query = httpd:parse_query(Input),
    Width = percept_html:get_option_value("width", Query),
    Height = percept_html:get_option_value("height", Query),
    Percentage = percept_html:get_option_value("percentage", Query),
    percept_image:percentage(round(Width), round(Height), float(Percentage)).

header() ->
    "Content-Type: image/png\r\n\r\n".
