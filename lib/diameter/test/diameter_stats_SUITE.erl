%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

%%
%% Tests of the server implemented by diameter_stats.erl.
%%

-module(diameter_stats_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([an/1,
         twa/1]).

-define(stat, diameter_stats).
-define(util, diameter_util).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [{group, all} | tc()].

groups() ->
    [{all, [parallel], tc()}].

tc() ->
    [an,
     twa].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _) ->
    ok.

init_per_suite(Config) ->
    ok = diameter:start(),
    Config.

end_per_suite(_Config) ->
    ok = diameter:stop().

%% ===========================================================================

an(_) ->
    Ref = {'_', make_ref()},
    true = ?stat:reg(Ref),
    true = ?stat:reg(Ref),  %% duplicate
    ok = ?stat:incr(x),
    ok = ?stat:incr(x, Ref),
    ok = ?stat:incr(y, 2),
    ok = ?stat:incr(y, Ref),
    %% Flushing a pid flushes even stats on the registered reference.
    [{x,2},{y,3}] = lists:sort(?stat:flush()),
    [] = ?stat:flush(Ref),
    [] = ?stat:flush().

twa(_) ->
    Ref = make_ref(),
    ok = ?stat:incr(x, 8),
    ok = ?stat:incr(x, Ref, 7),
    %% Flushing a reference doesn't affect registered pids.
    [{x,7}] = ?stat:flush(Ref),
    [] = ?stat:flush(Ref),
    [{x,8}] = ?stat:flush(),
    [] = ?stat:flush().
