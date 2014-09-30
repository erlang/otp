%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2014. All Rights Reserved.
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
%% Purpose: The ssh server instance supervisor, an instans of this supervisor
%%          exists for every ip-address and port combination, hangs under  
%%          sshd_sup.
%%----------------------------------------------------------------------

-module(ssh_system_sup).

-behaviour(supervisor).

-export([start_link/1, stop_listener/1,
	 stop_listener/2, stop_system/1,
	 stop_system/2, system_supervisor/2,
	 subsystem_supervisor/1, channel_supervisor/1, 
	 connection_supervisor/1, 
	 acceptor_supervisor/1, start_subsystem/2, restart_subsystem/2,
	 restart_acceptor/2, stop_subsystem/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%% Internal  API
%%%=========================================================================
start_link(ServerOpts) ->
    Address = proplists:get_value(address, ServerOpts),
    Port = proplists:get_value(port, ServerOpts),
    Name = make_name(Address, Port),
    supervisor:start_link({local, Name}, ?MODULE, [ServerOpts]).

stop_listener(SysSup) ->
    stop_acceptor(SysSup). 

stop_listener(Address, Port) ->
    Name = make_name(Address, Port),
    stop_acceptor(whereis(Name)). 
 
stop_system(SysSup) ->
    Name = sshd_sup:system_name(SysSup),
    spawn(fun() -> sshd_sup:stop_child(Name) end),
    ok.

stop_system(Address, Port) -> 
    spawn(fun() -> sshd_sup:stop_child(Address, Port) end),
    ok.

system_supervisor(Address, Port) ->
    Name = make_name(Address, Port),
    whereis(Name).

subsystem_supervisor(SystemSup) ->
    ssh_subsystem_sup(supervisor:which_children(SystemSup)).

channel_supervisor(SystemSup) ->
    SubSysSup = ssh_subsystem_sup(supervisor:which_children(SystemSup)),
    ssh_subsystem_sup:channel_supervisor(SubSysSup).

connection_supervisor(SystemSup) ->
    SubSysSup = ssh_subsystem_sup(supervisor:which_children(SystemSup)),
    ssh_subsystem_sup:connection_supervisor(SubSysSup).

acceptor_supervisor(SystemSup) ->
    ssh_acceptor_sup(supervisor:which_children(SystemSup)).

start_subsystem(SystemSup, Options) ->
    Spec = ssh_subsystem_child_spec(Options),
    supervisor:start_child(SystemSup, Spec).

stop_subsystem(SystemSup, SubSys) ->
    case catch lists:keyfind(SubSys, 2, supervisor:which_children(SystemSup)) of
	false ->
	    {error, not_found};
	{Id, _, _, _} ->
	    spawn(fun() -> supervisor:terminate_child(SystemSup, Id),
			   supervisor:delete_child(SystemSup, Id) end),
	    ok;
	{'EXIT', {noproc, _}} ->
	    %% Already terminated; probably shutting down.
	    ok;
	{'EXIT', {shutdown, _}} ->
	    %% Already shutting down.
	    ok
    end.


restart_subsystem(Address, Port) ->
    SysSupName = make_name(Address, Port),
    SubSysName = id(ssh_subsystem_sup, Address, Port),
    case supervisor:terminate_child(SysSupName, SubSysName) of
	ok ->
	    supervisor:restart_child(SysSupName, SubSysName);
	Error  ->
	    Error
    end.

restart_acceptor(Address, Port) ->
    SysSupName = make_name(Address, Port),
    AcceptorName = id(ssh_acceptor_sup, Address, Port),
    supervisor:restart_child(SysSupName, AcceptorName).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([ServerOpts]) ->
    RestartStrategy = one_for_one,
    MaxR = 0,
    MaxT = 3600,
    Children = child_specs(ServerOpts),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(ServerOpts) ->
    [ssh_acceptor_child_spec(ServerOpts)]. 
  
ssh_acceptor_child_spec(ServerOpts) ->
    Address = proplists:get_value(address, ServerOpts),
    Port = proplists:get_value(port, ServerOpts),
    Name = id(ssh_acceptor_sup, Address, Port),
    StartFunc = {ssh_acceptor_sup, start_link, [ServerOpts]},
    Restart = transient, 
    Shutdown = infinity,
    Modules = [ssh_acceptor_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

ssh_subsystem_child_spec(ServerOpts) ->
    Name = make_ref(),
    StartFunc = {ssh_subsystem_sup, start_link, [ServerOpts]},
    Restart = temporary,
    Shutdown = infinity,
    Modules = [ssh_subsystem_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.


id(Sup, Address, Port) ->
    {Sup, Address, Port}.

make_name(Address, Port) ->
    list_to_atom(lists:flatten(io_lib:format("ssh_system_~p_~p_sup", 
					     [Address, Port]))).

ssh_subsystem_sup([{_, Child, _, [ssh_subsystem_sup]} | _]) ->
    Child;
ssh_subsystem_sup([_ | Rest]) ->
    ssh_subsystem_sup(Rest).

ssh_acceptor_sup([{_, Child, _, [ssh_acceptor_sup]} | _]) ->
    Child;
ssh_acceptor_sup([_ | Rest]) ->
    ssh_acceptor_sup(Rest).

stop_acceptor(Sup) ->
    [{Name, AcceptorSup}] =
	[{SupName, ASup} || {SupName, ASup, _, [ssh_acceptor_sup]} <- 
			  supervisor:which_children(Sup)],
    supervisor:terminate_child(AcceptorSup, Name).

