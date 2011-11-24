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
%% Purpose: The ssh subsystem supervisor 
%%----------------------------------------------------------------------

-module(ssh_subsystem_sup).

-behaviour(supervisor).

-export([start_link/1, connection_supervisor/1, channel_supervisor/1
	]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Opts) ->
    supervisor:start_link(?MODULE, [Opts]).

connection_supervisor(SupPid) ->
    Children = supervisor:which_children(SupPid),
    ssh_connection_sup(Children).

channel_supervisor(SupPid) ->    
    Children = supervisor:which_children(SupPid),
    ssh_channel_sup(Children).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Opts]) ->
    RestartStrategy = one_for_all,
    MaxR = 0,
    MaxT = 3600,
    Children = child_specs(Opts),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(Opts) ->
    case proplists:get_value(role, Opts) of
	client ->		
	    [ssh_connectinon_child_spec(Opts)];
	server ->
	    [ssh_connectinon_child_spec(Opts), ssh_channel_child_spec(Opts)]
    end.
  
ssh_connectinon_child_spec(Opts) ->
    Address = proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    Role = proplists:get_value(role, Opts),
    Name = id(Role, ssh_connection_controler, Address, Port),
    StartFunc = {ssh_connection_controler, start_link, [Opts]},
    Restart = transient, 
%    Restart = permanent, 
    Shutdown = 5000,
    Modules = [ssh_connection_controler],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

ssh_channel_child_spec(Opts) ->
    Address = proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    Role = proplists:get_value(role, Opts),
    Name = id(Role, ssh_channel_sup, Address, Port),
    StartFunc = {ssh_channel_sup, start_link, [Opts]},
    Restart = transient, 
%    Restart = permanent, 
    Shutdown = infinity,
    Modules = [ssh_channel_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Role, Sup, Address, Port) ->
    {Role, Sup, Address, Port}.

ssh_connection_sup([{_, Child, _, [ssh_connection_controler]} | _]) ->
    Child;
ssh_connection_sup([_ | Rest]) ->
    ssh_connection_sup(Rest).

ssh_channel_sup([{_, Child, _, [ssh_channel_sup]} | _]) ->
    Child;
ssh_channel_sup([_ | Rest]) ->
    ssh_channel_sup(Rest).



