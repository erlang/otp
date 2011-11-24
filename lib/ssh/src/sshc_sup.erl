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
%% Purpose: The ssh client subsystem supervisor 
%%----------------------------------------------------------------------

-module(sshc_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(Args) ->
    RestartStrategy = simple_one_for_one,
    MaxR = 10,
    MaxT = 3600,
    {ok, {{RestartStrategy, MaxR, MaxT}, [child_spec(Args)]}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(_) ->
    Name = undefined, % As simple_one_for_one is used.
    StartFunc = {ssh_connection_controler, start_link, []},
    Restart = temporary, 
%    Shutdown = infinity,
    Shutdown = 5000,
    Modules = [ssh_connection_controler],
%    Type = supervisor,
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

