%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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

-module(snmpm_supervisor).

-behaviour(supervisor).


%% External exports
-export([start_link/2, stop/0, stop/1]).

%% supervisor callbacks
-export([init/1]).


-define(SERVER, ?MODULE).

-include("snmp_debug.hrl").


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link(Type, Opts) ->
    ?d("start_link -> entry with"
       "~n   Opts: ~p", [Opts]),
    SupName = {local, ?MODULE}, 
    supervisor:start_link(SupName, ?MODULE, [Type, Opts]).


stop() ->
    stop(0).

stop(Timeout) ->
    ?d("stop -> entry", []),
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
            stop(Pid, Timeout);
	_ ->
	    ?d("stop -> not running", []),
	    not_running
    end.

%% For some unfathomable reason there is no "nice" way to stop
%% a supervisor. The "normal" way to do it is:
%% 1) exit(Pid, kill) (kaboom)
%% 2) If the caller is the *parent*: exit(Pid, shutdown)
%% So, here we do it the really ugly way...but since this function is 
%% intended for testing (mostly)...
stop(Pid, Timeout) when (Timeout =:= 0) ->
    ?d("stop -> Pid: ~p", [Pid]),
    sys:terminate(Pid, shutdown),
    ?d("stop -> stopped", []),
    ok;
stop(Pid, Timeout) ->
    ?d("stop -> Pid: ~p", [Pid]),
    MRef = erlang:monitor(process, Pid),
    sys:terminate(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ?d("stop -> stopped", []),
            ok
    after Timeout ->
            ?d("stop -> timeout", []),
            erlang:demonitor(MRef, [flush]),
            {error, timeout}
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
init([Opts]) when is_list(Opts) ->    %% OTP-5963: Due to the addition
    init([normal, Opts]);             %% OTP-5963: of server_sup
init([Type, Opts]) ->
    ?d("init -> entry with"
       "~n   Type: ~p"
       "~n   Opts: ~p", [Type, Opts]),
    Restart   = get_restart(Opts), 
    Flags     = {one_for_all, 0, 3600},
    Config    = worker_spec(snmpm_config, [Opts], Restart, [gen_server]),
    MiscSup   = sup_spec(snmpm_misc_sup, [], Restart),
    ServerSup = sup_spec(snmpm_server_sup, [Type, Opts], Restart),
    Sups      = [Config, MiscSup, ServerSup],
    {ok, {Flags, Sups}}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

get_restart(Opts) ->
    get_opt(Opts, restart_type, transient).

get_opt(Opts, Key, Def) ->
    snmp_misc:get_option(Key, Opts, Def).
	
sup_spec(Name, Args, Restart) ->
    ?d("sup_spec -> entry with"
       "~n   Name:    ~p"
       "~n   Args:    ~p"
       "~n   Restart: ~p", [Name, Args, Restart]),
    {Name, 
     {Name, start_link, Args}, 
     Restart, 2000, supervisor, [Name,supervisor]}.

worker_spec(Name, Args, Restart, Modules) ->
    ?d("worker_spec -> entry with"
       "~n   Name:    ~p"
       "~n   Args:    ~p"
       "~n   Restart: ~p"
       "~n   Modules: ~p", [Name, Args, Restart, Modules]),
    {Name, 
     {Name, start_link, Args}, 
     Restart, 2000, worker, [Name] ++ Modules}.



