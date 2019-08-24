%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% Supervisor for the entire Mnesia application

-module(mnesia_sup).

-behaviour(supervisor).

-export([start_link/1, init/1, start_event/0, kill/0]).

start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [Args]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callback functions

init([[]]) ->
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.

init() ->
    Flags = {one_for_all, 0, 3600}, % Should be rest_for_one policy

    Event = event_procs(),
    Ext = ext_procs(),
    Kernel = kernel_procs(),

    {ok, {Flags, Event ++ Ext ++ Kernel}}.

event_procs() ->
    KillAfter = timer:seconds(30),
    KA = mnesia_kernel_sup:supervisor_timeout(KillAfter),
    E = mnesia_event,
    [{E, {?MODULE, start_event, []}, permanent, KA, worker, [E, gen_event]}].

kernel_procs() ->
    K = mnesia_kernel_sup,
    KA = infinity,
    [{K, {K, start, []}, permanent, KA, supervisor, [K, supervisor]}].

ext_procs() ->
    K = mnesia_ext_sup,
    KA = infinity,
    [{K, {K, start, []}, permanent, KA, supervisor, [K, supervisor]}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% event handler

start_event() ->
    case gen_event:start_link({local, mnesia_event}) of
	{ok, Pid} ->
	    case add_event_handler() of
		ok -> 
		    {ok, Pid};
		Error ->
		    Error
	    end;
	Error  ->
	    Error
    end.

add_event_handler() ->
    Handler = mnesia_monitor:get_env(event_module),
    gen_event:add_handler(mnesia_event, Handler, []).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% debug functions

kill() ->
    Mnesia = [mnesia_fallback | mnesia:ms()],
    Kill = fun(Name) -> catch exit(whereis(Name), kill) end,
    lists:foreach(Kill, Mnesia),
    lists:foreach(fun ensure_dead/1, Mnesia),
    timer:sleep(10),
    case lists:keymember(mnesia, 1, application:which_applications()) of
	true -> kill();
	false -> ok
    end.

ensure_dead(Name) ->
    case whereis(Name) of
	undefined ->
	    ok;
	Pid when is_pid(Pid) ->
	    exit(Pid, kill),
	    timer:sleep(10),
	    ensure_dead(Name)
    end.
