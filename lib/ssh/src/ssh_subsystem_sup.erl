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
%% Purpose: The ssh subsystem supervisor 
%%----------------------------------------------------------------------

-module(ssh_subsystem_sup).

-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/5,
	 connection_supervisor/1,
	 channel_supervisor/1
	]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Role, Address, Port, Profile, Options) ->
    supervisor:start_link(?MODULE, [Role, Address, Port, Profile, Options]).

connection_supervisor(SupPid) ->
    Children = supervisor:which_children(SupPid),
    ssh_connection_sup(Children).

channel_supervisor(SupPid) ->    
    Children = supervisor:which_children(SupPid),
    ssh_channel_sup(Children).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Role, Address, Port, Profile, Options]) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs = child_specs(Role, Address, Port, Profile, Options),
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_specs(client, _Address, _Port, _Profile, _Options) ->
    [];
child_specs(server, Address, Port, Profile, Options) ->
    [ssh_channel_child_spec(server, Address, Port, Profile, Options), 
     ssh_connection_child_spec(server, Address, Port, Profile, Options)].
  
ssh_connection_child_spec(Role, Address, Port, _Profile, Options) ->
    #{id       => id(Role, ssh_connection_sup, Address, Port),
      start    => {ssh_connection_sup, start_link, [Options]},
      restart  => temporary,
      type     => supervisor
     }.

ssh_channel_child_spec(Role, Address, Port, _Profile, Options) ->
    #{id       => id(Role, ssh_channel_sup, Address, Port),
      start    => {ssh_channel_sup, start_link, [Options]},
      restart  => temporary,
      type     => supervisor
     }.

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



