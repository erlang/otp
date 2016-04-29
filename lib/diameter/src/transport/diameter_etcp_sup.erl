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

-module(diameter_etcp_sup).

-behaviour(supervisor).

%% interface
-export([start/0,
         start_child/1]).

%% internal exports
-export([start_link/1,
         init/1]).

%% start/0
%%
%% Start the etcp top supervisor.

start() ->
    diameter_transport_sup:start_child(?MODULE, ?MODULE).

%% start_child/1
%%
%% Start a worker under the etcp supervisor.

start_child(T) ->
    supervisor:start_child(?MODULE, [T]).

%% start_link/1
%%
%% Callback from diameter_transport_sup as a result of start/0.
%% Starts a child supervisor under the transport supervisor.

start_link(?MODULE) ->
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

init([]) ->
    Mod = diameter_etcp,
    Flags = {simple_one_for_one, 0, 1},
    ChildSpec = {Mod,
                 {Mod, start_link, []},
                 temporary,
                 1000,
                 worker,
                 [Mod]},
    {ok, {Flags, [ChildSpec]}}.
