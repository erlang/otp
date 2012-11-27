%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
-module(test_server_cover_SUITE).

-export([all/1, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([tc1/1, tc2/1]).

-include_lib("test_server/include/test_server.hrl").

all(suite) ->
    [tc1,tc2].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case,Config) ->
    Dog = ?t:timetrap({minutes,10}),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case,Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.


%%%-----------------------------------------------------------------
%%% Test cases
tc1(Config) when is_list(Config) ->
    cover_helper:foo(),
    ok.

tc2(Config) when is_list(Config) ->
    cover_helper:bar(),
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions

