%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_sup.erl,v 1.1 2008/12/17 09:53:39 mikpe Exp $
%%
%% Supervisor for the entire Mnesia application

-module(mnesia_sup).

-behaviour(application).
-behaviour(supervisor).

-export([start/0, start/2, init/1, stop/1, start_event/0, kill/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application and suprvisor callback functions

start(normal, Args) ->
    SupName = {local,?MODULE},
    case supervisor:start_link(SupName, ?MODULE, [Args]) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error ->
	    Error
    end;
start(_, _) ->
    {error, badarg}.

start() ->
    SupName = {local,?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

stop(_StartArgs) ->
    ok.

init([]) -> % Supervisor
    init();
init([[]]) -> % Application
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.

init() ->
    Flags = {one_for_all, 0, 3600}, % Should be rest_for_one policy

    Event = event_procs(),
    Kernel = kernel_procs(),

    {ok, {Flags, Event ++ Kernel}}.

event_procs() ->
    KillAfter = timer:seconds(30),
    KA = mnesia_kernel_sup:supervisor_timeout(KillAfter),
    E = mnesia_event,
    [{E, {?MODULE, start_event, []}, permanent, KA, worker, [E, gen_event]}].

kernel_procs() ->
    K = mnesia_kernel_sup,
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
	Pid when pid(Pid) ->
	    exit(Pid, kill),
	    timer:sleep(10),
	    ensure_dead(Name)
    end.
