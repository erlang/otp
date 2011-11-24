%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% Purpose: The top supervisor for ssh servers hangs under 
%%          ssh_sup.
%%----------------------------------------------------------------------

-module(sshd_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/1, stop_child/1,
	 stop_child/2, system_name/1]).

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
    case ssh_system_sup:system_supervisor(Address, Port) of
       undefined ->
	    Spec =  child_spec(Address, Port, ServerOpts),    
	    case supervisor:start_child(?MODULE, Spec) of
		{error, already_present} ->
		    Name = id(Address, Port),
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
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.

stop_child(Address, Port) ->
    Name = id(Address, Port),
    stop_child(Name).

system_name(SysSup) ->
    Children = supervisor:which_children(sshd_sup),
    system_name(SysSup, Children).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
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
    Name = id(Address, Port),
    StartFunc = {ssh_system_sup, start_link, [ServerOpts]},
    Restart = transient, 
    Shutdown = infinity,
    Modules = [ssh_system_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Address, Port) ->
    {server, ssh_system_sup, Address, Port}.

system_name([], _ ) ->
    undefined;
system_name(SysSup, [{Name, SysSup, _, _} | _]) ->
    Name;
system_name(SysSup, [_ | Rest]) ->
    system_name(SysSup, Rest).
