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
	 stop_child/3,
         system_name/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Address, Port, Profile, Options) ->
io:format("~p:~p ~p:~p~n",[?MODULE,?LINE,Address, Port]),
    case ssh_system_sup:system_supervisor(Address, Port, Profile) of
       undefined ->
io:format("~p:~p undefined~n",[?MODULE,?LINE]),
	    Spec = child_spec(Address, Port, Profile, Options),
            Reply = supervisor:start_child(?MODULE, Spec),
io:format("~p:~p Reply=~p~n",[?MODULE,?LINE,Reply]),
            Reply;
	Pid ->
io:format("~p:~p Pid=~p~n",[?MODULE,?LINE,Pid]),
	    AccPid = ssh_system_sup:acceptor_supervisor(Pid),
            ssh_acceptor_sup:start_child(AccPid, Address, Port, Profile, Options),
            {ok,Pid}
    end.

stop_child(Name) ->
    supervisor:terminate_child(?MODULE, Name).

stop_child(Address, Port, Profile) ->
    Name = id(Address, Port, Profile),
    stop_child(Name).

system_name(SysSup) ->
    Children = supervisor:which_children(sshd_sup),
    system_name(SysSup, Children).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    {ok, {{one_for_one, 10, 3600}, []}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(Address, Port, Profile, Options) ->
    Name = id(Address, Port,Profile),
    StartFunc = {ssh_system_sup, start_link, [Address, Port, Profile, Options]},
    Restart = temporary, 
    Shutdown = infinity,
    Modules = [ssh_system_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Address, Port, Profile) ->
    {server, ssh_system_sup, Address, Port, Profile}.

system_name([], _ ) ->
    undefined;
system_name(SysSup, [{Name, SysSup, _, _} | _]) ->
    Name;
system_name(SysSup, [_ | Rest]) ->
    system_name(SysSup, Rest).
