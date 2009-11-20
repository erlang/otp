%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
%% Purpose: The top supervisor for the http client hangs under 
%%          inets_sup.
%%----------------------------------------------------------------------

-module(httpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(HttpcServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HttpcServices]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([HttpcServices]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = child_specs(HttpcServices),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(HttpcServices) ->
    [httpc_profile_sup(HttpcServices), httpc_handler_sup()].

httpc_profile_sup(HttpcServices) ->
    Name = httpc_profile_sup,
    StartFunc = {httpc_profile_sup, start_link, [HttpcServices]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpc_profile_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

httpc_handler_sup() ->
    Name = httpc_handler_sup,
    StartFunc = {httpc_handler_sup, start_link, []},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpc_handler_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.


