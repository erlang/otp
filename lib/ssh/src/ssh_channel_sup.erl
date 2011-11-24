%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Ssh channel supervisor.
%%----------------------------------------------------------------------
-module(ssh_channel_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).

start_child(Sup, ChildSpec) ->
    supervisor:start_child(Sup, ChildSpec).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_Args) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = [],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
