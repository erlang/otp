%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2024. All Rights Reserved.
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

-module(ext_records).
-export([local/2,server/0]).

-export_record([vector,quad]).
-record #vector{x=10, y=1, z=5}.
-record #quad{a, b, c, d}.

%% Local records.
-record #local{x, y}.
-record #state{counter=0}.

local(X, Y) ->
    #local{x=X, y=Y}.

server() ->
    spawn_link(fun() ->
                       State = #state{},
                       server(State)
               end).

server(State0) ->
    receive
        {From, done} when is_pid(From) ->
            From ! {self(), done};
        {From, Request} when is_pid(From) ->
            {Reply,State} = handle_request(Request, State0),
            From ! {self(), Reply},
            server(State)
    end.

handle_request(bump, State0) ->
    Counter0 = State0#state.counter,
    Counter = Counter0 + 1,

    %% Creating a new record should always work, even if this code has
    %% became old.
    State = #state{counter=Counter},
    {Counter, State}.
