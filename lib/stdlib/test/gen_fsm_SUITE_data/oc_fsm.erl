%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(oc_fsm).

-behaviour(gen_fsm).

%% API
-export([start/0]).

%% gen_fsm callbacks
-export([init/1,
         state_name/2,
         state_name/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start() ->
    gen_fsm:start({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, state_name, #state{}}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

