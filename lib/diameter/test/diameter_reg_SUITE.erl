%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
%% Tests of the server implemented by diameter_reg.erl.
%%

-module(diameter_reg_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([add/1,
         add_new/1,
         remove/1,
         down/1,
         terms/1,
         pids/1]).

-define(reg,  diameter_reg).
-define(util, diameter_util).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, all},
     {group, all, [parallel]}].

groups() ->
    [{all, [], tc()}].

tc() ->
    [add,
     add_new,
     remove,
     down,
     terms,
     pids].

init_per_suite(Config) ->
    ok = diameter:start(),
    Config.

end_per_suite(_Config) ->
    ok = diameter:stop().

%% ===========================================================================

add(_) ->
    Ref = make_ref(),
    true = ?reg:add(Ref),
    true = ?reg:add(Ref),
    [{Ref, Pid}] = ?reg:match(Ref),
    Pid = self().

add_new(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    false = ?reg:add_new(Ref).

remove(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    true = ?reg:add_new({Ref}),
    true = ?reg:remove({Ref}),
    [{Ref, Pid}] = ?reg:match(Ref),
    Pid = self().

down(_) ->
    Ref = make_ref(),
    {_, MRef} = spawn_monitor(fun() -> ?reg:add_new(Ref), timer:sleep(1000) end),
    receive {'DOWN', MRef, process, _, _} -> ok end,
    timer:sleep(1000),
    [] = ?reg:match(Ref).

terms(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    [[Pid]] = [L || {T,L} <- ?reg:terms(), T == Ref],
    Pid = self().

pids(_) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    %% Don't match [[Ref]] since this will only necessarily be the
    %% case when the test is run in its own process.
    [_|_] = [L || {P,L} <- ?reg:pids(), P == self()].
