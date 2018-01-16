%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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
-module(generic_statem).

-export([start/1, reply/2, transit/2, stop/1]).

-export([callback_mode/0, init/1]).
-export([state1/3, state2/3]).

-behaviour(gen_statem).

%% API

start(Data) ->
    {ok, Pid} = gen_statem:start(?MODULE, Data, []),
    Pid.

stop(P) ->
    ok = gen_statem:stop(P).

reply(S, M) ->
    gen_statem:call(S, {reply, M}, infinity).

transit(S, M) ->
    gen_statem:call(S, {transit, M}, infinity).

%% Implementation

callback_mode() ->
    state_functions.

init(Data) ->
    {ok, state1, Data}.

state1({call, From}, {reply, M}, Data) ->
    {keep_state, Data, {reply, From, M}};
state1({call, From}, {transit, M}, Data) ->
    {next_state, state2, Data, {reply, From, M}}.

state2({call, From}, {transit, M}, Data) ->
    {next_state, state1, Data, {reply, From, M}}.
