%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(percept_db_SUITE).
-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
	start/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(2)).
-define(restarts, 10).
-define(alive_timeout, 500).

init_per_suite(Config) when is_list(Config) ->
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{max_size, 300}, {watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) ->
    % Test cases
    [start].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

start(suite) ->
    [];
start(doc) ->
    ["Percept_db start and restart test."];
start(Config) when is_list(Config) ->
    ok = restart(?restarts),
    {stopped, _DB} = percept_db:stop(),
    ok.

restart(0)->
    ok;
restart(N)->
    {_, DB} = percept_db:start(),
    timer:sleep(?alive_timeout),
    true = erlang:is_process_alive(DB),
    restart(N-1).
