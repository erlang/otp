%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
%% This module implements a supervisor for external (plug-in) processes

-module(mnesia_ext_sup).
-behaviour(supervisor).

-export([start/0,
	 init/1,
	 start_proc/4, start_proc/5,
	 stop_proc/1]).

-define(SHUTDOWN, 120000).  % 2 minutes

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_proc(Name, M, F, A) ->
    start_proc(Name, M, F, A, []).

start_proc(Name, M, F, A, Opts) ->
    [Restart, Shutdown, Type, Modules] =
	[proplists:get_value(K, Opts, Default)
	 || {K, Default} <- [{restart, transient},
			     {shutdown, ?SHUTDOWN},
			     {type, worker},
			     {modules, [M]}]],
    case supervisor:start_child(
	   ?MODULE, {Name, {M,F,A}, Restart, Shutdown, Type, Modules}) of
	{error, already_present} ->
	    supervisor:restart_child(?MODULE, Name);
	Other ->
	    Other
    end.

stop_proc(Name) ->
    supervisor:terminate_child(?MODULE, Name).

init(_) ->
    {ok, {{one_for_all, 10, 60}, []}}.
