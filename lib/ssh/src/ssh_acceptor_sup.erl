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
%% Purpose: The acceptor supervisor for ssh servers hangs under 
%%          ssh_system_sup.
%%----------------------------------------------------------------------

-module(ssh_acceptor_sup).
-behaviour(supervisor).

-export([start_link/1, start_child/2, stop_child/2]).

%% Supervisor callback
-export([init/1]).

-define(DEFAULT_TIMEOUT, 50000).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Servers) ->
    supervisor:start_link(?MODULE, [Servers]).

start_child(AccSup, ServerOpts) ->
    Spec = child_spec(ServerOpts),    
    case supervisor:start_child(AccSup, Spec) of
	{error, already_present} ->
	    Address = proplists:get_value(address, ServerOpts),
	    Port = proplists:get_value(port, ServerOpts),
	    Name = id(Address, Port),
	    supervisor:delete_child(?MODULE, Name),
	    supervisor:start_child(AccSup, Spec);
	Reply ->
	    Reply
    end.

stop_child(Address, Port) ->
    Name = id(Address, Port),
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([ServerOpts]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = [child_spec(ServerOpts)],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(ServerOpts) ->
    Address = proplists:get_value(address, ServerOpts),
    Port = proplists:get_value(port, ServerOpts),
    Timeout = proplists:get_value(timeout, ServerOpts, ?DEFAULT_TIMEOUT),
    Name = id(Address, Port),
    SocketOpts = proplists:get_value(socket_opts, ServerOpts),
    StartFunc = {ssh_acceptor, start_link, [Port, Address, 
					    [{active, false},
					     {reuseaddr, true}] ++ SocketOpts, 
					    ServerOpts, Timeout]},
    Restart = permanent, 
    Shutdown = 3600,
    Modules = [ssh_acceptor],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Address, Port) ->
    {ssh_acceptor_sup, Address, Port}.

