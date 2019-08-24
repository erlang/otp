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
%% Purpose: The acceptor supervisor for ssh servers hangs under 
%%          ssh_system_sup.
%%----------------------------------------------------------------------

-module(ssh_acceptor_sup).
-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/4, start_child/5, stop_child/4]).

%% Supervisor callback
-export([init/1]).

-define(DEFAULT_TIMEOUT, 50000).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Address, Port, Profile, Options) ->
    supervisor:start_link(?MODULE, [Address, Port, Profile, Options]).

start_child(AccSup, Address, Port, Profile, Options) ->
    Spec = child_spec(Address, Port, Profile, Options),
    case supervisor:start_child(AccSup, Spec) of
	{error, already_present} ->
            %% Is this ever called?
	    stop_child(AccSup, Address, Port, Profile),
	    supervisor:start_child(AccSup, Spec);
	Reply ->
            %% Reply = {ok,SystemSupPid} when the user calls ssh:daemon
            %% after having called ssh:stop_listening
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
init([Address, Port, Profile, Options]) ->
    %% Initial start of ssh_acceptor_sup for this port or new start after
    %% ssh:stop_daemon
    SupFlags = #{strategy  => one_for_one, 
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [child_spec(Address, Port, Profile, Options)],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec(Address, Port, Profile, Options) ->
    Timeout = ?GET_INTERNAL_OPT(timeout, Options, ?DEFAULT_TIMEOUT),
    #{id       => id(Address, Port, Profile),
      start    => {ssh_acceptor, start_link, [Port, Address, Options, Timeout]},
      restart  => transient % because a crashed listener could be replaced by a new one
     }.

id(Address, Port, Profile) ->
    {ssh_acceptor_sup, Address, Port, Profile}.

