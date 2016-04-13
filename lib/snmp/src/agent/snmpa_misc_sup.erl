%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(snmpa_misc_sup).

-include("snmp_debug.hrl").

-behaviour(supervisor).

%% External exports
-export([
	 start_link/0, 
	 start_mib_server/4, stop_mib_server/1,
	 start_net_if/6, stop_net_if/1,
	 start_note_store/3, stop_note_store/1
	]).

%% Internal exports
-export([init/1]).

-define(SERVER, ?MODULE).


%%%-----------------------------------------------------------------
%%% This is a supervisor for the mib and net_ifprocesses.
%%% Each agent has one mib process.
%%%-----------------------------------------------------------------

start_link() ->
    ?d("start_link -> entry", []),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%-----------------------------------------------------------------
%% When the agent starts, it calls this function. If there already
%% exist a mib process for the agent, this one is used. Otherwise
%% a new one is started.
%%-----------------------------------------------------------------
start_mib_server(Prio, Ref, Mibs, Opts) ->
    ?d("start_mib_server -> entry with"
	"~n   Prio:    ~p"
	"~n   Ref:     ~p"
	"~n   Mibs:    ~p"
	"~n   Opts:    ~p", [Prio, Ref, Mibs, Opts]),
    SupName = ?SERVER,
    start_mibserver(SupName, Ref, [Prio, Mibs, Opts]).

start_mibserver(SupName, Ref, Args) ->
    Children = supervisor:which_children(SupName),
    case lists:keysearch({mib, Ref}, 1, Children) of
	{value, {_, Pid, _, _}} -> {ok, Pid};
	_ ->
	    Mib = {{mib, Ref}, 
		   {snmpa_mib, start_link, Args},
		   transient, 10000, worker, [snmpa_mib]},
	    supervisor:start_child(SupName, Mib)
    end.

stop_mib_server(Ref) ->
    SupName = ?SERVER,
    case whereis(SupName) of
	undefined ->
	    ok;
	_ ->
	    supervisor:terminate_child(SupName, {mib, Ref}),
	    supervisor:delete_child(SupName, {mib, Ref})
    end.


start_net_if(Prio, NoteStore, Ref, Master, Mod, Opts) ->
    ?d("start_mib -> entry with"
	"~n   Prio:      ~p"
	"~n   NoteStore: ~p"
	"~n   Ref:       ~p"
	"~n   Master:    ~p"
	"~n   Mod:       ~p"
	"~n   Opts:      ~p", 
       [Prio, NoteStore, Ref, Master, Mod, Opts]),
    SupName = ?SERVER,
    start_netif(SupName, Ref, Mod, [Prio, NoteStore, Master, Opts]).

start_netif(SupName, Ref, Mod, Args) ->
    %% make sure we start from scratch...
    Children = supervisor:which_children(SupName),
    case lists:keysearch({net_if, Ref}, 1, Children) of
	{value, {_, _Pid, _, _}} ->
	    stop_net_if(Ref);
	_ ->
	    ok
    end,
    NetIf = {{net_if, Ref}, 
	     {Mod, start_link, Args},
	     permanent, 2000, worker, [Mod]},
    supervisor:start_child(SupName, NetIf).

stop_net_if(Ref) ->
    SupName = ?SERVER,
    case whereis(SupName) of
	undefined ->
	    ok;
	_ ->
	    supervisor:terminate_child(SupName, {net_if, Ref}),
	    supervisor:delete_child(SupName, {net_if, Ref})
    end.


start_note_store(Prio, Ref, Opts) ->
    ?d("start_note_store -> entry with"
	"~n   Prio:    ~p"
	"~n   Ref:     ~p"
	"~n   Opts:    ~p", [Prio, Ref, Opts]),
    SupName = ?SERVER,
    start_notestore(SupName, Ref, [Prio, snmpa, Opts]).

start_notestore(SupName, Ref, Args) ->
    %% make sure we start from scratch...
    Children = supervisor:which_children(SupName),
    case lists:keysearch({note_store, Ref}, 1, Children) of
	{value, {_, _Pid, _, _}} ->
	    stop_note_store(Ref);
	_ ->
	    ok
    end,
    Mod = snmp_note_store,
    Note = {{note_store, Ref}, 
	    {Mod, start_link, Args},
	    permanent, 2000, worker, [Mod]},
    supervisor:start_child(SupName, Note).

stop_note_store(Ref) ->
    SupName = ?SERVER,
    case whereis(SupName) of
	undefined ->
	    ok;
	_ ->
	    supervisor:terminate_child(SupName, {note_store, Ref}),
	    supervisor:delete_child(SupName, {note_store, Ref})
    end.


init([]) ->
    SupFlags = {one_for_all, 0, 3600},
    {ok, {SupFlags, []}}.
