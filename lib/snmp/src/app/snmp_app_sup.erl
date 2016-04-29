%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
-module(snmp_app_sup).

-include("snmp_debug.hrl").

-behaviour(supervisor).


%% External exports
-export([start_link/0, start_agent/2, start_manager/2]).

%% Internal exports
-export([stop/0]).


%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link() ->
    ?d("start_link -> entry",[]),
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

stop() ->
    ?d("stop -> entry", []),
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    ?d("stop -> Pid: ~p", [Pid]),
	    exit(Pid, shutdown),
	    ?d("stop -> stopped", []),
	    ok;
	_ ->
	    ?d("stop -> not running", []),
	    not_running
    end.
    

start_agent(Type, Opts) ->
    ?d("start_agent -> entry with"
	"~n   Type: ~p"
	"~n   Opts: ~p", [Type, Opts]),
    Restart = get_restart(Opts, permanent), 
    start_sup_child(snmpa_supervisor, Restart, [Type, Opts]).


start_manager(Type, Opts) ->
    ?d("start_manager -> entry with"
	"~n   Type: ~p"
	"~n   Opts: ~p", [Type, Opts]),
    Restart = get_restart(Opts, transient), 
    start_sup_child(snmpm_supervisor, Restart, [Type, Opts]).


%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init(_Args) ->
    ?d("init -> entry", []),
    Flags = {one_for_one, 0, 1},
    Sups  = [],
    {ok, {Flags, Sups}}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

get_restart(Opts, Def) ->
    get_opt(Opts, restart_type, Def).

get_opt(Opts, Key, Def) ->
    snmp_misc:get_option(Key, Opts, Def).

start_sup_child(Mod, Type, Args) ->
    Spec = sup_spec(Mod, Type, Args), 
    supervisor:start_child(?MODULE, Spec).

sup_spec(Name, Type, Args) ->
    {Name, 
     {Name, start_link, Args}, 
     Type, 2000, supervisor, [Name, supervisor]}.


% i(F) ->
%     i(F, []).

% i(F, A) ->
%     io:format("~p: " ++ F ++ "~n", [?MODULE|A]).

