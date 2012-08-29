%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2012. All Rights Reserved.
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
%% Purpose: Ssh connection supervisor.
%%----------------------------------------------------------------------

-module(ssh_connection_sup).

-behaviour(supervisor).

-export([start_link/1, start_handler_child/2, start_manager_child/2,
	 connection_manager/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).

%% Will be called from the manager child process
start_handler_child(Sup, Args) ->
    [Spec] = child_specs(handler, Args),
    supervisor:start_child(Sup, Spec).

%% Will be called from the acceptor process
start_manager_child(Sup, Args) ->
    [Spec] = child_specs(manager, Args),    
    supervisor:start_child(Sup, Spec).

connection_manager(SupPid) -> 
    try supervisor:which_children(SupPid) of
	Children ->
	    {ok, ssh_connection_manager(Children)}
    catch exit:{noproc,_} ->
	    {ok, undefined}
    end.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Args]) ->  
    RestartStrategy = one_for_all,
    MaxR = 0,
    MaxT = 3600,
    Children = child_specs(Args),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(Opts) ->
    case proplists:get_value(role, Opts) of
	client ->		
	    child_specs(manager, [client | Opts]);
	server ->
	    %% Children started by acceptor process
	    []
    end.
 
% The manager process starts the handler process
child_specs(manager, Opts) ->
    [manager_spec(Opts)];
child_specs(handler, Opts) ->
    [handler_spec(Opts)].

manager_spec([server = Role, Socket, Opts]) ->
    Name = make_ref(), 
    StartFunc = {ssh_connection_manager, start_link, [[Role, Socket, Opts]]},
    Restart = temporary,
    Shutdown = 3600,
    Modules = [ssh_connection_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules};

manager_spec([client = Role | Opts]) ->
    Name = make_ref(), 
    StartFunc = {ssh_connection_manager, start_link, [[Role, Opts]]},
    Restart = temporary,
    Shutdown = 3600,
    Modules = [ssh_connection_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

handler_spec([Role, Socket, Opts]) ->
    Name = make_ref(), 
    StartFunc = {ssh_connection_handler, 
		 start_link, [Role, self(), Socket, Opts]},
    Restart = temporary,
    Shutdown = 3600,
    Modules = [ssh_connection_handler],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

ssh_connection_manager([]) ->
    undefined;
ssh_connection_manager([{_, Child, _, [ssh_connection_manager]} | _]) ->
    Child;
ssh_connection_manager([_ | Rest]) ->
    ssh_connection_manager(Rest).
