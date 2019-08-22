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

-export([start_link/0, start_child/4]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Address, Port, Profile, Options) ->
    %% Here we a new connction on a new Host/EFERMERAL Port/Profile
    Spec = child_spec(Address, Port, Profile, Options),
    supervisor:start_child(?MODULE, Spec).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy  => one_for_one, 
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs = [],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(Address, Port, Profile, Options) ->
    #{id       => id(Address, Port, Profile),
      start    => {ssh_system_sup, start_link, [client, Address, Port, Profile, Options]},
      restart  => temporary,
      type     => supervisor
     }.

id(Address, Port, Profile) ->
    {client, ssh_system_sup, Address, Port, Profile}.
