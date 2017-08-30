%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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

%%
%% Tests of the server implemented by diameter_stats.erl.
%%

-module(diameter_stats_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([reg/1,
         incr/1,
         read/1,
         sum/1,
         flush/1]).

-define(stat, diameter_stats).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, all},
     {group, all, [parallel]}].

groups() ->
    [{all, [], tc()}].

tc() ->
    [reg,
     incr,
     read,
     sum,
     flush].

init_per_suite(Config) ->
    ok = diameter:start(),
    Config.

end_per_suite(_Config) ->
    ok = diameter:stop().

%% ===========================================================================

reg(_) ->
    Ref = '$1',
    true = ?stat:reg(Ref),
    false = ?stat:reg(Ref).  %% duplicate

incr(_) ->
    Ref = '_',
    Ctr = x,
    false = ?stat:incr(Ctr),      %% not registered,
    1 = ?stat:incr(Ctr, Ref, 1),  %% only pids need register
    true = ?stat:reg(Ref),
    spawn(fun() ->
                  true = ?stat:reg(Ref),
                  2 = ?stat:incr(Ctr, self(), 2)
          end),
    ok = fold(Ctr, Ref, 3),  %% folded
    ?stat:flush([self(), Ref]).

read(_) ->
    Ref = make_ref(),
    C1 = {a,b},
    C2 = {b,a},
    true = ?stat:reg(Ref),
    1 = ?stat:incr(C1),
    1 = ?stat:incr(C2),
    2 = ?stat:incr(C1),
    7 = ?stat:incr(C1, Ref, 7),
    Self = self(),
    [{Ref,  [{C1,7}]}, {Self, [{C1,2}, {C2,1}]}]
        = ?stat:read([self(), Ref, make_ref()]),
    [] = ?stat:read([]),
    [] = ?stat:read([make_ref()]),
    ?stat:flush([self(), Ref, make_ref()]).

sum(_) ->
    Ref = make_ref(),
    C1 = {a,b},
    C2 = {b,a},
    true = ?stat:reg(Ref),
    1 = ?stat:incr(C1),
    1 = ?stat:incr(C2),
    2 = ?stat:incr(C2),
    7 = ?stat:incr(C1, Ref, 7),
    [{Ref,  [{C1,8}, {C2,2}]}]
        = ?stat:sum([Ref, make_ref()]),
    Self = self(),
    [{Self,  [{C1,1}, {C2,2}]}]
        = ?stat:sum([self()]),
    [{Ref, [{C1,7}]}, {Self, [{C1,1}, {C2,2}]}]
        = ?stat:flush([self(), Ref]).

flush(_) ->
    Ref = make_ref(),
    Ctr = '_',
    true = ?stat:reg(Ref),
    1 = ?stat:incr(Ctr),
    3 = ?stat:incr(Ctr, self(), 2),
    2 = ?stat:incr(Ctr, Ref, 2),
    Self = self(),
    [{Self, [{Ctr, 3}]}] = ?stat:flush([self()]),
    1 = ?stat:incr(Ctr),
    [{Ref,  [{Ctr, 2}]}] = ?stat:flush([Ref]),
    [{Self, [{Ctr, 1}]}] = ?stat:flush([self()]),
    [] = ?stat:flush([self(), Ref]).

%% ===========================================================================

%% Keep incremented until a fold results in the specified value.
fold(Ctr, Ref, N) ->
    case ?stat:incr(Ctr, Ref, 0) of
        N ->
            ok;
        M when M < N ->
            erlang:yield(),
            fold(Ctr, Ref, N)
    end.
