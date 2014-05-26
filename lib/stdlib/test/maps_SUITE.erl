%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
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
%%%----------------------------------------------------------------
%%% Purpose: Test suite for the 'maps' module.
%%%-----------------------------------------------------------------

-module(maps_SUITE).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
% This should be set relatively high (10-15 times the expected
% max testcasetime).
-define(default_timeout, ?t:minutes(4)).

% Test server specific exports
-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get3/1]).

suite() ->
    [{ct_hooks, [ts_install_cth]}].

all() ->
    [get3].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

get3(Config) when is_list(Config) ->
    Map = #{ key1 => value1, key2 => value2 },
    DefaultValue = "Default value",
    ?line value1 = maps:get(key1, Map, DefaultValue),
    ?line value2 = maps:get(key2, Map, DefaultValue),
    ?line DefaultValue = maps:get(key3, Map, DefaultValue),
    ok.
