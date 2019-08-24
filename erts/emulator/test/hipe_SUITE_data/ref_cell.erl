%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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

-module(ref_cell).

-export([start_link/0, start_link_deep/0, call/2]).

-compile(native).

-define(DEPTH, 100).
-define(ALLOCS, 500).

start_link() ->
    spawn_link(fun() -> loop(undefined) end).

start_link_deep() ->
    spawn_link(fun() -> go_deep(?DEPTH) end).

%% Create a stack large enough to get a graylimit trap placed next time there's
%% a minor gc.
go_deep(0) ->
    alloc_some(?ALLOCS),
    loop(undefined),
    0;
go_deep(Depth) ->
    go_deep(Depth-1)+1.

%% Do some allocation to trigger a minor gc
alloc_some(Amount) ->
    Check = (Amount * (Amount + 1)) div 2,
    Check = lists:sum(lists:seq(1, Amount)).

call(Pid, Call) ->
    Pid ! {Call, self()},
    receive {Pid, Res} -> Res end.

loop(Thing) ->
    receive
	{done, Pid} -> Pid ! {self(), done};
	{{put_res_of, Fun}, Pid} ->
	    NewThing = Fun(),
	    Pid ! {self(), put},
	    loop(NewThing);
	{get, Pid} ->
	    Pid ! {self(), Thing},
	    loop(Thing)
    end.
