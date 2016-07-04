%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
%% Supervisor for config processes.
%%

-module(diameter_config_sup).

-behaviour(supervisor).

%% interface
-export([start_link/0,        %% supervisor start
	 start_child/1]).     %% config start

-export([init/1]).

-define(NAME, ?MODULE).  %% supervisor name

%% start_link/0

start_link() ->
    SupName = {local, ?NAME},
    supervisor:start_link(SupName, ?MODULE, []).

%% start_child/1

start_child(T) ->
    supervisor:start_child(?NAME, [T]).

%% init/1

init([]) ->
    Mod = diameter_config,
    Flags = {simple_one_for_one, 0, 1},
    ChildSpec = {Mod,
                 {Mod, start_link, []},
                 temporary,
                 1000,
                 worker,
                 [Mod]},
    {ok, {Flags, [ChildSpec]}}.
