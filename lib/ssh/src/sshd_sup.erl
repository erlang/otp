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

-export([start_link/1, start_child/1, stop_child/1,
	 stop_child/3, system_name/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Servers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers]).

start_child(ServerOpts) ->
    Address = proplists:get_value(address, ServerOpts),
    Port = proplists:get_value(port, ServerOpts),    
    Profile = proplists:get_value(profile,  proplists:get_value(ssh_opts, ServerOpts), ?DEFAULT_PROFILE),
    case ssh_system_sup:system_supervisor(Address, Port, Profile) of
       undefined ->
	    Spec =  child_spec(Address, Port, ServerOpts),    
	    case supervisor:start_child(?MODULE, Spec) of
		{error, already_present} ->
		    Name = id(Address, Port, Profile),
		    supervisor:delete_child(?MODULE, Name),
		    supervisor:start_child(?MODULE, Spec);
		Reply ->
		    Reply
	    end;
	Pid ->
	    AccPid = ssh_system_sup:acceptor_supervisor(Pid),
	    ssh_acceptor_sup:start_child(AccPid, ServerOpts)
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
-spec init( [term()] ) -> {ok,{supervisor:sup_flags(),[supervisor:child_spec()]}} | ignore .

init([Servers]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Fun = fun(ServerOpts) -> 
		  Address = proplists:get_value(address, ServerOpts),
		  Port = proplists:get_value(port, ServerOpts),
		  child_spec(Address, Port, ServerOpts) 
	  end,
    Children = lists:map(Fun, Servers),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(Address, Port, ServerOpts) ->
    Profile = proplists:get_value(profile,  proplists:get_value(ssh_opts, ServerOpts), ?DEFAULT_PROFILE),
    Name = id(Address, Port,Profile),
    StartFunc = {ssh_system_sup, start_link, [ServerOpts]},
    Restart = temporary, 
    Shutdown = infinity,
    Modules = [ssh_system_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Address, Port, Profile) ->
    case is_list(Address) of	
	true ->
	    {server, ssh_system_sup, any, Port, Profile};
	false ->
	    {server, ssh_system_sup, Address, Port, Profile}
    end.

system_name([], _ ) ->
    undefined;
system_name(SysSup, [{Name, SysSup, _, _} | _]) ->
    Name;
system_name(SysSup, [_ | Rest]) ->
    system_name(SysSup, Rest).
