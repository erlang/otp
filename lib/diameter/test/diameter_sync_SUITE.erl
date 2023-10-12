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
%% Tests of the server implemented by diameter_sync.erl.
%%

-module(diameter_sync_SUITE).

%% testcases, no common_test depedency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([suite/0,
         all/0,
         parallel/1]).

-define(sync, diameter_sync).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 15}}].

all() ->
    [parallel].

parallel(_Config) ->
    run().

%% ===========================================================================

%% run/0

run() ->
    run([call, cast, timeout, flush]).

%% run/1

run(List)
  when is_list(List) ->
    ok = diameter:start(),
    try
        diameter_util:run([{[fun run/1, T], 10000} || T <- List])
    after
        ok = diameter:stop()
    end;

run(call) ->
    Ref = make_ref(),
    Q = {q, Ref},
    F = fun() -> Ref end,
    Ref = ?sync:call(Q, F, infinity, infinity),
    Ref = ?sync:call(Q, F, 0, infinity),
    Ref = call(Q, F),
    Ref = call(Q, {fun(_) -> Ref end, x}),
    timeout = call(Q, fun() -> exit(unexpected) end),
    {_,_,_} = call(Q, {erlang, now, []}),
    {_,_,_} = call(Q, [fun erlang:now/0]);

run(cast) ->
    Ref = make_ref(),
    Q = {q, Ref},
    false = ?sync:carp(Q),
    [] = ?sync:pids(Q),
    %% Queue a request that blocks until we send it Ref and another
    %% that exits with Ref.
    ok = cast(Q, fun() -> receive Ref -> ok end end),
    ok = cast(Q, fun() -> exit(Ref) end),
    [_,Pid] = ?sync:pids(Q),
    %% Ensure some expected truths ...
    2 = ?sync:pending(Q),
    true = 2 =< ?sync:pending(),
    true = lists:member(Q, ?sync:queues()),
    %% ... and that the max number of requests is respected.
    rejected = ?sync:call(Q, {erlang, now, []}, 1, infinity),
    rejected = ?sync:cast(Q, {erlang, now, []}, 1, infinity),
    %% Monitor on the identifiable request and see that exits when we
    %% let the blocking request finish.
    MRef = erlang:monitor(process, Pid),
    {value, P} = ?sync:carp(Q),
    P ! Ref,
    Ref = receive {'DOWN', MRef, process, _, Reason} -> Reason end;

run(timeout) ->
    Q = make_ref(),
    ok = ?sync:cast(Q, {timer, sleep, [2000]}, infinity, 2000),
    timeout = ?sync:call(Q, fun() -> ok end, infinity, 1000);

run(flush) ->
    Q = make_ref(),
    F = {timer, sleep, [2000]},
    ok = cast(Q, F),
    ok = cast(Q, F),
    1 = ?sync:flush(Q).

%% ===========================================================================

call(Q, Req) ->
    sync(call, Q, Req).

cast(Q, Req) ->
    sync(cast, Q, Req).

sync(F, Q, Req) ->
    ?sync:F(Q, Req, infinity, infinity).
