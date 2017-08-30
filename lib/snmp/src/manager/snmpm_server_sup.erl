%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

-module(snmpm_server_sup).

-behaviour(supervisor).


%% External exports
-export([start_link/2, stop/0]).

%% supervisor callbacks
-export([init/1]).


-define(SERVER, ?MODULE).

-include("snmp_debug.hrl").


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(_Type, Opts) ->
    ?d("start_link -> entry with"
       "~n   Opts: ~p", [Opts]),
    SupName = {local, ?MODULE}, 
    supervisor:start_link(SupName, ?MODULE, [Opts]).

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


%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([Opts]) ->
    ?d("init -> entry with"
       "~n   Opts: ~p", [Opts]),
    Restart = get_restart(Opts), 
    Flags   = {one_for_all, 5, 500},
    Server  = worker_spec(snmpm_server, [], Restart, [gen_server]),
    Sups    = [Server],
    {ok, {Flags, Sups}}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

get_restart(Opts) ->
    get_opt(Opts, restart_type, transient).

get_opt(Opts, Key, Def) ->
    snmp_misc:get_option(Key, Opts, Def).
	
%% sup_spec(Name, Args, Restart) ->
%%     ?d("sup_spec -> entry with"
%%        "~n   Name:    ~p"
%%        "~n   Args:    ~p"
%%        "~n   Restart: ~p", [Name, Args, Restart]),
%%     {Name, 
%%      {Name, start_link, Args}, 
%%      Restart, 2000, supervisor, [Name,supervisor]}.

worker_spec(Name, Args, Restart, Modules) ->
    ?d("worker_spec -> entry with"
       "~n   Name:    ~p"
       "~n   Args:    ~p"
       "~n   Restart: ~p"
       "~n   Modules: ~p", [Name, Args, Restart, Modules]),
    {Name, 
     {Name, start_link, Args}, 
     Restart, 2000, worker, [Name] ++ Modules}.



