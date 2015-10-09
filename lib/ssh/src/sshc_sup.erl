%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: The ssh client subsystem supervisor 
%%----------------------------------------------------------------------

-module(sshc_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/1, stop_child/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

stop_child(Client) ->
    spawn(fun() -> 
		  ClientSup = whereis(?MODULE),
		  supervisor:terminate_child(ClientSup, Client)
	  end),
    ok.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(Args) ->
    RestartStrategy = simple_one_for_one,
    MaxR = 0,
    MaxT = 3600,
    {ok, {{RestartStrategy, MaxR, MaxT}, [child_spec(Args)]}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(_) ->
    Name = undefined, % As simple_one_for_one is used.
    StartFunc = {ssh_connection_handler, start_link, []},
    Restart = temporary,
    Shutdown = infinity,
    Modules = [ssh_connection_handler],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
