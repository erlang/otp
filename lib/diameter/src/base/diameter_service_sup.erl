%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
%% The supervisor of service processes.
%%

-module(diameter_service_sup).

-behaviour(supervisor).

-export([start_link/0,     %% supervisor start
         start_child/1]).  %% service start

%% supervisor callback
-export([init/1]).

-define(NAME, ?MODULE).  %% supervisor name

%% start_link/0

start_link() ->
    SupName = {local, ?NAME},
    supervisor:start_link(SupName, ?MODULE, []).

%% start_child/1
%%
%% A service and its related processes (transport, peer_fwm and
%% watchdog) are all temporary since they're all restarted in
%% application code. A Transport and peer_fsm is restarted by a
%% watchdog as required by the RFC 3539 state machine, a watchdog is
%% restarted by service, and services are restarted by diameter_config.

start_child(ServiceName) ->
    supervisor:start_child(?NAME, [ServiceName]).

%% init/1

init([]) ->
    Mod = diameter_service,
    Flags = {simple_one_for_one, 0, 1},
    ChildSpec = {Mod,
                 {Mod, start_link, []},
                 temporary,
                 5000,
                 worker,
                 [Mod]},
    {ok, {Flags, [ChildSpec]}}.
