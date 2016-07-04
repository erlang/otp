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
%% Purpose: The ssh server instance supervisor, an instans of this supervisor
%%          exists for every ip-address and port combination, hangs under  
%%          sshd_sup.
%%----------------------------------------------------------------------

-module(ssh_system_sup).

-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/1, stop_listener/1,
	 stop_listener/3, stop_system/1,
	 stop_system/3, system_supervisor/3,
	 subsystem_supervisor/1, channel_supervisor/1, 
	 connection_supervisor/1, 
	 acceptor_supervisor/1, start_subsystem/2, restart_subsystem/3,
	 restart_acceptor/3, stop_subsystem/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%% Internal  API
%%%=========================================================================
start_link(ServerOpts) ->
    Address = proplists:get_value(address, ServerOpts),
    Port = proplists:get_value(port, ServerOpts),
    Profile = proplists:get_value(profile,  proplists:get_value(ssh_opts, ServerOpts), ?DEFAULT_PROFILE),
    Name = make_name(Address, Port, Profile),
    supervisor:start_link({local, Name}, ?MODULE, [ServerOpts]).

stop_listener(SysSup) ->
    stop_acceptor(SysSup). 

stop_listener(Address, Port, Profile) ->
    Name = make_name(Address, Port, Profile),
    stop_acceptor(whereis(Name)). 
 
stop_system(SysSup) ->
    Name = sshd_sup:system_name(SysSup),
    spawn(fun() -> sshd_sup:stop_child(Name) end),
    ok.

stop_system(Address, Port, Profile) -> 
    spawn(fun() -> sshd_sup:stop_child(Address, Port, Profile) end),
    ok.

system_supervisor(Address, Port, Profile) ->
    Name = make_name(Address, Port, Profile),
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


restart_subsystem(Address, Port, Profile) ->
    SysSupName = make_name(Address, Port, Profile),
    SubSysName = id(ssh_subsystem_sup, Address, Port, Profile),
    case supervisor:terminate_child(SysSupName, SubSysName) of
	ok ->
	    supervisor:restart_child(SysSupName, SubSysName);
	Error  ->
	    Error
    end.

restart_acceptor(Address, Port, Profile) ->
    SysSupName = make_name(Address, Port, Profile),
    AcceptorName = id(ssh_acceptor_sup, Address, Port, Profile),
    supervisor:restart_child(SysSupName, AcceptorName).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
-spec init( [term()] ) -> {ok,{supervisor:sup_flags(),[supervisor:child_spec()]}} | ignore .

init([ServerOpts]) ->
    RestartStrategy = one_for_one,
    MaxR = 0,
    MaxT = 3600,
    Children = case proplists:get_value(asocket,ServerOpts) of
		   undefined -> child_specs(ServerOpts);
		   _ -> []
	       end,
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(ServerOpts) ->
    [ssh_acceptor_child_spec(ServerOpts)]. 
  
ssh_acceptor_child_spec(ServerOpts) ->
    Address = proplists:get_value(address, ServerOpts),
    Port = proplists:get_value(port, ServerOpts),
    Profile = proplists:get_value(profile,  proplists:get_value(ssh_opts, ServerOpts), ?DEFAULT_PROFILE),
    Name = id(ssh_acceptor_sup, Address, Port, Profile),
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


id(Sup, Address, Port, Profile) ->
    case is_list(Address) of	
	true ->
	    {Sup, any, Port, Profile};
	false ->
	    {Sup, Address, Port, Profile}
	end.

make_name(Address, Port, Profile) ->
    case is_list(Address) of
	true  ->
	    list_to_atom(lists:flatten(io_lib:format("ssh_system_~p_~p_~p_sup", 
						     [any, Port, Profile])));
	false  ->
	    list_to_atom(lists:flatten(io_lib:format("ssh_system_~p_~p_~p_sup", 
						     [Address, Port, Profile])))
    end.

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
    case supervisor:terminate_child(AcceptorSup, Name) of
        ok ->
            supervisor:delete_child(AcceptorSup, Name);
        Error ->
            Error
    end.
