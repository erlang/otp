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
-module(oc_statem).

-behaviour(gen_statem).

%% API
-export([start/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).

start() ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, state_name, #{}}.

callback_mode() ->
    handle_event_function.
