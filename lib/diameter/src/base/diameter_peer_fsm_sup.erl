%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% The supervisor of peer_fsm processes.
%%

-module(diameter_peer_fsm_sup).

-behaviour(supervisor).

-define(NAME, ?MODULE).  %% supervisor name

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------

-export([start_link/0,       %% supervisor start
	 start_child/1]).    %% peer fsm start

-export([init/1]).

%% start_link/0

start_link() ->
    SupName = {local, ?NAME},
    supervisor:start_link(SupName, ?MODULE, []).

%% start_child/1
%%
%% Start a peer_fsm process.

start_child(T) ->
    supervisor:start_child(?NAME, [T]).

%% init/1

init([]) ->
    Mod = diameter_peer_fsm,
    Flags = {simple_one_for_one, 0, 1},
    ChildSpec = {Mod,
                 {Mod, start_link, []},
                 temporary,
                 1000,
                 worker,
                 [Mod]},
    {ok, {Flags, [ChildSpec]}}.
