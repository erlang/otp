%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-export([start_link/0, start_child/3, stop_subsystem/2, start_connection/4]).

%% Internal exports
-export([start_link/1]).

%% Supervisor callback
-export([init/1]).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-define(SSHC_SUP, ?MODULE).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local,?SSHC_SUP}, ?MODULE, []).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

start_child(Role, Socket, Options) ->
    supervisor:start_child(?MODULE, [{Role, Socket, Options}]).

stop_subsystem(SystemSup, SubSys) ->
    case catch lists:keyfind(SubSys, 2, supervisor:which_children(SystemSup)) of
	false ->
	    ok;
	{Id, _, _, _} ->
	    spawn(fun() -> 
                          supervisor:terminate_child(SystemSup, Id),
                          supervisor:delete_child(SystemSup, Id),
                          supervisor:terminate_child(?MODULE, SystemSup)
                  end),
	    ok;
	{'EXIT', {noproc, _}} ->
	    %% Already terminated; probably shutting down.
	    ok;
	{'EXIT', {shutdown, _}} ->
	    %% Already shutting down.
	    ok
    end.

start_connection(SupPid, Role, Socket, Options) ->
    SubSysSup = ssh_subsystem_sup(SupPid),
    Options1 =  ?PUT_INTERNAL_OPT({supervisors, [{system_sup, SupPid},
                                                 {subsystem_sup, SubSysSup}
                                                ]},
                                  Options),
    supervisor:start_child(SupPid, connection_spec(Role, Socket, Options1)).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([]) ->
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs = [#{id       => undefined, % As simple_one_for_one is used.
                    start    => {?MODULE, start_link, []},
                    restart  => temporary, % because there is no way to restart a crashed connection
                    type     => supervisor
                   }
                 ],
    {ok, {SupFlags,ChildSpecs}};
init({Role, Socket, Options}) ->
    SupFlags = #{strategy  => one_for_all, 
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs = [subsystem_spec(Role, Socket, Options)],
    {ok, {SupFlags,ChildSpecs}}.


subsystem_spec(Role, Socket, Options) ->
    {ok, {Address, Port}} = inet:peername(Socket),
    Profile =  ?GET_OPT(profile, Options),
    #{id       => id(Role, ssh_subsystem_sup, Address, Port),
      start    => {ssh_subsystem_sup, start_link, [Role, Address, Port, Profile, Options]},
      restart  => transient,
      type     => supervisor
     }.

connection_spec(Role, Socket, Options) ->
    {ok, {Address, Port}} = inet:peername(Socket),
    #{id       => id(Role, ssh_connection_handler, Address, Port),
      start    => {ssh_connection_handler, start_link, [Role, Socket, Options]},
      restart  => transient,
      type     => worker
     }.

id(Role, Mod, Address, Port) ->
    {Role, Mod, Address, Port}.

ssh_subsystem_sup(SupPid) ->
    find_child(SupPid, ssh_subsystem_sup).

find_child(SupPid, Mod) ->
    [Pid] = [Child || {_, Child, _, [M]} <- supervisor:which_children(SupPid),
                      M == Mod],
    Pid.
