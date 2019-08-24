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
-module(generic_fsm).

-export([start/1, reply/2, transit/2, stop/1]).

-export([init/1, terminate/3]).
-export([state1/3, state2/3]).

-behaviour(gen_fsm).


%% API

start(Data) ->
    {ok, Pid} = gen_fsm:start(?MODULE, Data, []),
    Pid.

stop(P) ->
    ok = gen_fsm:stop(P).

reply(S, M) ->
    gen_fsm:sync_send_event(S, {reply, M}, infinity).

transit(S, M) ->
    gen_fsm:sync_send_event(S, {transit, M}, infinity).

%% Implementation

init(Data) ->
    {ok, state1, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

state1({reply, M}, _From, Data) ->
    {reply, M, ?FUNCTION_NAME, Data};
state1({transit, M}, _From, Data) ->
    {reply, M, state2, Data}.

state2({transit, M}, _From, Data) ->
    {reply, M, state1, Data}.
