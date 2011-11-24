%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% The supervisor of the static server processes.
%%

-module(diameter_misc_sup).

-behaviour(supervisor).

-export([start_link/0]).  %% supervisor start

%% supervisor callback
-export([init/1]).

-define(CHILDREN, [diameter_sync,      %% serialization
                   diameter_stats,     %% statistics counter management
                   diameter_reg,       %% service/property publishing
                   diameter_peer,      %% remote peer manager
                   diameter_config]).  %% configuration/restart

%% start_link/0

start_link() ->
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

%% init/1

init([]) ->
    Flags = {one_for_one, 1, 5},
    Workers  = lists:map(fun spec/1, ?CHILDREN),
    {ok, {Flags, Workers}}.

spec(Mod) ->
    {Mod,
     {Mod, start_link, []},
     permanent,
     1000,
     worker,
     [Mod]}.
