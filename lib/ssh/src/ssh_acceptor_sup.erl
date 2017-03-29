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
%% Purpose: The acceptor supervisor for ssh servers hangs under 
%%          ssh_system_sup.
%%----------------------------------------------------------------------

-module(ssh_acceptor_sup).
-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/1, start_child/2, stop_child/4]).

%% Supervisor callback
-export([init/1]).

-define(DEFAULT_TIMEOUT, 50000).

-spec init( [term()] ) -> {ok,{supervisor:sup_flags(),[supervisor:child_spec()]}} | ignore .

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Servers) ->
    supervisor:start_link(?MODULE, [Servers]).

start_child(AccSup, Options) ->
    Spec = child_spec(Options),
    case supervisor:start_child(AccSup, Spec) of
	{error, already_present} ->
	    Address = ?GET_INTERNAL_OPT(address, Options),
	    Port = ?GET_INTERNAL_OPT(port, Options),
	    Profile = ?GET_OPT(profile, Options),  
	    stop_child(AccSup, Address, Port, Profile),
	    supervisor:start_child(AccSup, Spec);
	Reply ->
	    Reply
    end.

stop_child(AccSup, Address, Port, Profile) ->
    Name = id(Address, Port, Profile),
    case supervisor:terminate_child(AccSup, Name) of
        ok ->
            supervisor:delete_child(AccSup, Name);
        Error ->
            Error
    end.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Options]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = [child_spec(Options)],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(Options) ->
    Address = ?GET_INTERNAL_OPT(address, Options),
    Port = ?GET_INTERNAL_OPT(port, Options),
    Timeout = ?GET_INTERNAL_OPT(timeout, Options, ?DEFAULT_TIMEOUT),
    Profile = ?GET_OPT(profile, Options),
    Name = id(Address, Port, Profile),
    StartFunc = {ssh_acceptor, start_link, [Port, Address, Options, Timeout]},
    Restart = transient, 
    Shutdown = brutal_kill,
    Modules = [ssh_acceptor],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Address, Port, Profile) ->
    case is_list(Address) of
	true ->
	    {ssh_acceptor_sup, any, Port, Profile};
	false ->
	    {ssh_acceptor_sup, Address, Port, Profile}
    end.

