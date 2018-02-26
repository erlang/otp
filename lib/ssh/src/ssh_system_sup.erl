%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

-export([start_link/4, stop_listener/1,
	 stop_listener/3, stop_system/1,
	 stop_system/3, system_supervisor/3,
	 subsystem_supervisor/1, channel_supervisor/1,
	 connection_supervisor/1,
	 acceptor_supervisor/1, start_subsystem/6,
	 stop_subsystem/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%% API
%%%=========================================================================
start_link(Address, Port, Profile, Options) ->
    Name = make_name(Address, Port, Profile),
    supervisor:start_link({local, Name}, ?MODULE, [Address, Port, Profile, Options]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Address, Port, Profile, Options]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs =
        case ?GET_INTERNAL_OPT(connected_socket,Options,undefined) of
            undefined ->
                [#{id       => id(ssh_acceptor_sup, Address, Port, Profile),
                   start    => {ssh_acceptor_sup, start_link, [Address, Port, Profile, Options]},
                   restart  => transient,
                   type     => supervisor
                  }];
            _ ->
                []
        end,
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%% Service API
%%%=========================================================================
stop_listener(SystemSup) ->
    {Name, AcceptorSup, _, _} = lookup(ssh_acceptor_sup, SystemSup),
    case supervisor:terminate_child(AcceptorSup, Name) of
        ok ->
            supervisor:delete_child(AcceptorSup, Name);
        Error ->
            Error
    end.

stop_listener(Address, Port, Profile) ->
    stop_listener(
      system_supervisor(Address, Port, Profile)).


stop_system(SysSup) ->
    catch sshd_sup:stop_child(SysSup),
    ok.

stop_system(Address, Port, Profile) ->
    catch sshd_sup:stop_child(Address, Port, Profile),
    ok.


system_supervisor(Address, Port, Profile) ->
    Name = make_name(Address, Port, Profile),
    whereis(Name).

subsystem_supervisor(SystemSup) ->
    {_, Child, _, _} = lookup(ssh_subsystem_sup, SystemSup),
    Child.

channel_supervisor(SystemSup) ->
    ssh_subsystem_sup:channel_supervisor(
      subsystem_supervisor(SystemSup)).

connection_supervisor(SystemSup) ->
    ssh_subsystem_sup:connection_supervisor(
      subsystem_supervisor(SystemSup)).

acceptor_supervisor(SystemSup) ->
    {_, Child, _, _} = lookup(ssh_acceptor_sup, SystemSup),
    Child.


start_subsystem(SystemSup, Role, Address, Port, Profile, Options) ->
    SubsystemSpec =
        #{id       => make_ref(),
          start    => {ssh_subsystem_sup, start_link, [Role, Address, Port, Profile, Options]},
          restart  => temporary,
          type     => supervisor
         },
    supervisor:start_child(SystemSup, SubsystemSpec).

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

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
id(Sup, Address, Port, Profile) ->
    {Sup, Address, Port, Profile}.

make_name(Address, Port, Profile) ->
    list_to_atom(lists:flatten(io_lib:format("ssh_system_~s_~p_~p_sup", [fmt_host(Address), Port, Profile]))).

fmt_host(IP) when is_tuple(IP) -> inet:ntoa(IP);
fmt_host(A)  when is_atom(A)   -> A;
fmt_host(S)  when is_list(S)   -> S.


lookup(SupModule, SystemSup) ->
    lists:keyfind([SupModule], 4,
                  supervisor:which_children(SystemSup)).

