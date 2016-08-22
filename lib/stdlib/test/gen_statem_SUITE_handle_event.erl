%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
-module(gen_statem_SUITE_handle_event).
-compile(export_all).
-behaviour(gen_statem).
-define(CB, gen_statem_SUITE).

init(Args) ->
    ?CB:init(Args).
handle_event(EventType, EventContent, State, Data) ->
    ?CB:handle_event_function(EventType, EventContent, State, Data).
terminate(Reason, State, Data) ->
    ?CB:terminate(Reason, State, Data).
code_change(OldVsn, OldState, OldData, Extra) ->
    ?CB:code_change(OldVsn, OldState, OldData, Extra).
format_status(StatusOption, Status) ->
    ?CB:format_status(StatusOption, Status).
