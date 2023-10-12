%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2022. All Rights Reserved.
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

%% testcases, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([all/0,
         parallel/1]).

-define(reg, diameter_reg).

%% ===========================================================================

all() ->
    [parallel].

parallel(_Config) ->
    run().

%% ===========================================================================

%% run/0

run() ->
    run([add, add_new, remove, down, terms, pids]).

%% run/1

run(List)
  when is_list(List) ->
    ok = diameter:start(),
    try
        diameter_util:run([{[fun run/1, T], 10000} || T <- List])
    after
        ok = diameter:stop()
    end;

run(add) ->
    Ref = make_ref(),
    true = ?reg:add(Ref),
    true = ?reg:add(Ref),
    [{Ref, Pid}] = ?reg:match(Ref),
    Pid = self();

run(add_new) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    false = ?reg:add_new(Ref);

run(remove) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    true = ?reg:add_new({Ref}),
    true = ?reg:remove({Ref}),
    [{Ref, Pid}] = ?reg:match(Ref),
    Pid = self();

run(down) ->
    Ref = make_ref(),
    {_, MRef} = spawn_monitor(fun() -> ?reg:add_new(Ref), timer:sleep(1000) end),
    receive {'DOWN', MRef, process, _, _} -> ok end,
    timer:sleep(1000),
    [] = ?reg:match(Ref);

run(terms) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    [[Pid]] = [L || {T,L} <- ?reg:terms(), T == Ref],
    Pid = self();

run(pids) ->
    Ref = make_ref(),
    true = ?reg:add_new(Ref),
    %% Don't match [[Ref]] since this will only necessarily be the
    %% case when the test is run in its own process.
    [_|_] = [L || {P,L} <- ?reg:pids(), P == self()].
