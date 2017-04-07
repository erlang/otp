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
%% Purpose: The top supervisor for ssh servers hangs under
%%          ssh_sup.
%%----------------------------------------------------------------------

-module(sshd_sup).

-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/0,
         start_child/4,
         stop_child/1,
	 stop_child/3
]).

%% Supervisor callback
-export([init/1]).

-define(SSHD_SUP, ?MODULE).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    %% No children are start now. We wait until the user calls ssh:daemon
    %% and uses start_child/4 to create the children
    supervisor:start_link({local,?SSHD_SUP}, ?MODULE, []).

start_child(Address, Port, Profile, Options) ->
    case ssh_system_sup:system_supervisor(Address, Port, Profile) of
       undefined ->
            %% Here we start listening on a new Host/Port/Profile
	    Spec = child_spec(Address, Port, Profile, Options),
            supervisor:start_child(?SSHD_SUP, Spec);
	Pid ->
            %% Here we resume listening on a new Host/Port/Profile after
            %% haveing stopped listening to he same with ssh:stop_listen(Pid)
	    AccPid = ssh_system_sup:acceptor_supervisor(Pid),
            ssh_acceptor_sup:start_child(AccPid, Address, Port, Profile, Options),
            {ok,Pid}
    end.

stop_child(ChildId) when is_tuple(ChildId) ->
    supervisor:terminate_child(?SSHD_SUP, ChildId);
stop_child(ChildPid) when is_pid(ChildPid)->
    stop_child(system_name(ChildPid)).


stop_child(Address, Port, Profile) ->
    Id = id(Address, Port, Profile),
    stop_child(Id).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [
                 ],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(Address, Port, Profile, Options) ->
    #{id       => id(Address, Port, Profile),
      start    => {ssh_system_sup, start_link, [Address, Port, Profile, Options]},
      restart  => temporary,
      shutdown => infinity,
      type     => supervisor,
      modules  => [ssh_system_sup]
     }.

id(Address, Port, Profile) ->
    {server, ssh_system_sup, Address, Port, Profile}.

system_name(SysSup) ->
    case lists:keyfind(SysSup, 2, supervisor:which_children(?SSHD_SUP)) of
        {Name, SysSup, _, _} -> Name;
        false -> undefind
    end.

