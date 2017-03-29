%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% Purpose: The ssh subsystem supervisor 
%%----------------------------------------------------------------------

-module(ssh_subsystem_sup).

-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/1,
	 connection_supervisor/1,
	 channel_supervisor/1
	]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Options) ->
    supervisor:start_link(?MODULE, [Options]).

connection_supervisor(SupPid) ->
    Children = supervisor:which_children(SupPid),
    ssh_connection_sup(Children).

channel_supervisor(SupPid) ->    
    Children = supervisor:which_children(SupPid),
    ssh_channel_sup(Children).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
-spec init( [term()] ) -> {ok,{supervisor:sup_flags(),[supervisor:child_spec()]}} | ignore .

init([Options]) ->
    RestartStrategy = one_for_all,
    MaxR = 0,
    MaxT = 3600,
    Children = child_specs(Options),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(Options) ->
    case ?GET_INTERNAL_OPT(role, Options) of
	client ->		
	    [];
	server ->
	    [ssh_channel_child_spec(Options), ssh_connectinon_child_spec(Options)]
    end.
  
ssh_connectinon_child_spec(Options) ->
    Address = ?GET_INTERNAL_OPT(address, Options),
    Port = ?GET_INTERNAL_OPT(port, Options),
    Role = ?GET_INTERNAL_OPT(role, Options),
    Name = id(Role, ssh_connection_sup, Address, Port),
    StartFunc = {ssh_connection_sup, start_link, [Options]},
    Restart = temporary,
    Shutdown = 5000,
     Modules = [ssh_connection_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

ssh_channel_child_spec(Options) ->
    Address = ?GET_INTERNAL_OPT(address, Options),
    Port = ?GET_INTERNAL_OPT(port, Options),
    Role = ?GET_INTERNAL_OPT(role, Options),
    Name = id(Role, ssh_channel_sup, Address, Port),
    StartFunc = {ssh_channel_sup, start_link, [Options]},
    Restart = temporary,
    Shutdown = infinity,
    Modules = [ssh_channel_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Role, Sup, Address, Port) ->
    {Role, Sup, Address, Port}.

ssh_connection_sup([{_, Child, _, [ssh_connection_sup]} | _]) ->
    Child;
ssh_connection_sup([_ | Rest]) ->
    ssh_connection_sup(Rest).

ssh_channel_sup([{_, Child, _, [ssh_channel_sup]} | _]) ->
    Child;
ssh_channel_sup([_ | Rest]) ->
    ssh_channel_sup(Rest).



