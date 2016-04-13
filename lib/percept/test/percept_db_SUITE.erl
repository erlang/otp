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
%%

-module(percept_db_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).

%% Test cases
-export([start/1]).

%% Default timetrap timeout (set in init_per_testcase)
-define(restarts, 10).
-define(alive_timeout, 500).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [start].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%% Percept_db start and restart test.
start(Config) when is_list(Config) ->
    ok = restart(?restarts),
    {stopped, _DB} = percept_db:stop(),
    ok.

restart(0)-> ok;
restart(N)->
    {_, DB} = percept_db:start(),
    timer:sleep(?alive_timeout),
    true = erlang:is_process_alive(DB),
    restart(N-1).
