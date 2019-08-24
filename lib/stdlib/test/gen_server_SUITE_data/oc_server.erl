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
-module(oc_server).

-behaviour(gen_server).

%% API
-export([start/0, start/1]).

%% gen_server callbacks
-export([init/1]).

-record(state, {}).

start() ->
    gen_server:start(?MODULE, ok, []).

start(continue) ->
    gen_server:start(?MODULE, continue, []).

init(ok) ->
    {ok, #state{}};
init(continue) ->
    {ok, #state{}, {continue, continue}}.
