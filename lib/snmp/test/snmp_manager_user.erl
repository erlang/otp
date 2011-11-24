%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Utility functions for the (snmp manager) user test(s).
%%----------------------------------------------------------------------

-module(snmp_manager_user).

-behaviour(snmpm_user).
%% -behaviour(snmpm_user_old).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("test_server.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
         start_link/0, start_link/1, start_link/2, 
	 start/0, start/1, start/2, 
	 stop/0,
	 info/0, 
	 system_info/0, 
	 simulate_crash/1,
	 register_agent/2, register_agent/3, 
	 unregister_agent/1, unregister_agent/2, 
	 agent_info/2, agent_info/3, 
	 update_agent_info/3, update_agent_info/4, 
	 which_all_agents/0, which_own_agents/0, 
	 load_mib/1, unload_mib/1, 
	 sync_get/1,       sync_get/2,       sync_get/3,       sync_get2/3, 
	 async_get/1,      async_get/2,      async_get/3,      async_get2/3,
	 sync_get_next/1,  sync_get_next/2,  sync_get_next/3,  sync_get_next2/3,
	 async_get_next/1, async_get_next/2, async_get_next/3, async_get_next2/3,
	 sync_set/1,       sync_set/2,       sync_set/3,       sync_set2/3, 
	 async_set/1,      async_set/2,      async_set/3,      async_set2/3, 
	 sync_get_bulk/3,  sync_get_bulk/4,  sync_get_bulk/5,  sync_get_bulk2/5,
	 async_get_bulk/3, async_get_bulk/4, async_get_bulk/5, async_get_bulk2/5,
	 name_to_oid/1, oid_to_name/1, 
	 purify_oid/1	 
        ]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

-export([
	 main/4
        ]).

-export([
	 handle_error/3,
         handle_agent/4,
         handle_pdu/4,
         handle_trap/3,
         handle_inform/3,
         handle_report/3, 
         handle_pdu/5,    % For backwards compatibillity 
         handle_trap/4,   % For backwards compatibillity 
         handle_inform/4, % For backwards compatibillity 
         handle_report/4  % For backwards compatibillity 
	]).


-define(SERVER, ?MODULE).

-record(state, {parent, id, reqs = []}).
%% -record(request, {from, ref, tmr, req_id, type}).


%%----------------------------------------------------------------------
%% The user API
%%----------------------------------------------------------------------

start() ->
    start(self()).

start(Parent) ->
    start(Parent, test_user).

start(Parent, Id) ->
    proc_lib:start(?MODULE, main, [true, Parent, self(), Id]).

start_link() ->
    start_link(self()).
start_link(Parent) ->
    start_link(Parent, test_user).

start_link(Parent, Id) ->
    proc_lib:start_link(?MODULE, main, [true, Parent, self(), Id]).

stop() ->
    cast(stop).

info() ->
    call(info).

system_info() ->
    call(system_info).

simulate_crash(Reason) ->
    call({simulate_crash, Reason}).

register_agent(TargetName, Config) 
  when is_list(TargetName) andalso is_list(Config) ->
    call({register_agent, TargetName, Config});
register_agent(Addr, Port) ->
    register_agent(Addr, Port, []).

register_agent(Addr, Port, Conf) ->
    call({register_agent, Addr, Port, Conf}).

unregister_agent(TargetName) ->
    call({unregister_agent, TargetName}).
unregister_agent(Addr, Port) ->
    call({unregister_agent, Addr, Port}).

agent_info(TargetName, Item) ->
    call({agent_info, TargetName, Item}).
agent_info(Addr, Port, Item) ->
    call({agent_info, Addr, Port, Item}).

update_agent_info(TargetName, Item, Val) ->
    call({update_agent_info, TargetName, Item, Val}).
update_agent_info(Addr, Port, Item, Val) ->
    call({update_agent_info, Addr, Port, Item, Val}).

which_all_agents() ->
    call(which_all_agents).

which_own_agents() ->
    call(which_own_agents).

load_mib(Mib) ->
    call({load_mib, Mib}).

unload_mib(Mib) ->
    call({unload_mib, Mib}).

%% -- 

sync_get(Oids) ->
    call({sync_get, Oids}).

sync_get(Addr_or_TargetName, Oids) ->
    call({sync_get, Addr_or_TargetName, Oids}).

sync_get(Addr, Port, Oids) ->
    call({sync_get, Addr, Port, Oids}).

sync_get2(TargetName, Oids, SendOpts) ->
    call({sync_get2, TargetName, Oids, SendOpts}).


%% --

async_get(Oids) ->
    call({async_get, Oids}).

async_get(Addr_or_TargetName, Oids) ->
    call({async_get, Addr_or_TargetName, Oids}).

async_get(Addr, Port, Oids) ->
    call({async_get, Addr, Port, Oids}).

async_get2(TargetName, Oids, SendOpts) ->
    call({async_get2, TargetName, Oids, SendOpts}).

%% --

sync_get_next(Oids) ->
    call({sync_get_next, Oids}).

sync_get_next(Addr_or_TargetName, Oids) ->
    call({sync_get_next, Addr_or_TargetName, Oids}).

sync_get_next(Addr, Port, Oids) ->
    call({sync_get_next, Addr, Port, Oids}).

sync_get_next2(TargetName, Oids, SendOpts) ->
    call({sync_get_next2, TargetName, Oids, SendOpts}).

%% --

async_get_next(Oids) ->
    call({async_get_next, Oids}).

async_get_next(Addr_or_TargetName, Oids) ->
    call({async_get_next, Addr_or_TargetName, Oids}).

async_get_next(Addr, Port, Oids) ->
    call({async_get_next, Addr, Port, Oids}).

async_get_next2(TargetName, Oids, SendOpts) ->
    call({async_get_next2, TargetName, Oids, SendOpts}).

%% --

sync_set(VAV) ->
    call({sync_set, VAV}).

sync_set(Addr_or_TargetName, VAV) ->
    call({sync_set, Addr_or_TargetName, VAV}).

sync_set(Addr, Port, VAV) ->
    call({sync_set, Addr, Port, VAV}).

sync_set2(TargetName, VAV, SendOpts) ->
    call({sync_set2, TargetName, VAV, SendOpts}).

%% --

async_set(VAV) ->
    call({async_set, VAV}).

async_set(Addr_or_TargetName, VAV) ->
    call({async_set, Addr_or_TargetName, VAV}).

async_set(Addr, Port, VAV) ->
    call({async_set, Addr, Port, VAV}).

async_set2(TargetName, VAV, SendOpts) ->
    call({async_set2, TargetName, VAV, SendOpts}).

%% --

sync_get_bulk(NonRep, MaxRep, Oids) ->
    call({sync_get_bulk, NonRep, MaxRep, Oids}).

sync_get_bulk(Addr_or_TargetName, NonRep, MaxRep, Oids) ->
    call({sync_get_bulk, Addr_or_TargetName, NonRep, MaxRep, Oids}).

sync_get_bulk(Addr, Port, NonRep, MaxRep, Oids) ->
    call({sync_get_bulk, Addr, Port, NonRep, MaxRep, Oids}).

sync_get_bulk2(TargetName, NonRep, MaxRep, Oids, SendOpts) ->
    call({sync_get_bulk2, TargetName, NonRep, MaxRep, Oids, SendOpts}).

%% --

async_get_bulk(NonRep, MaxRep, Oids) ->
    call({async_get_bulk, NonRep, MaxRep, Oids}).

async_get_bulk(Addr_or_TargetName, NonRep, MaxRep, Oids) ->
    call({async_get_bulk, Addr_or_TargetName, NonRep, MaxRep, Oids}).

async_get_bulk(Addr, Port, NonRep, MaxRep, Oids) ->
    call({async_get_bulk, Addr, Port, NonRep, MaxRep, Oids}).

async_get_bulk2(TargetName, NonRep, MaxRep, Oids, SendOpts) ->
    call({async_get_bulk2, TargetName, NonRep, MaxRep, Oids, SendOpts}).

%% -- 

name_to_oid(Name) ->
    call({name_to_oid, Name}).

oid_to_name(Oid) ->
    call({oid_to_name, Oid}).

purify_oid(Oid) ->
    call({purify_oid, Oid}).


%%----------------------------------------------------------------------

main(Debug, Parent, Starter, Id) ->
    put(debug, Debug),
    d("main -> entry with"
      "~n   Parent:  ~p"
      "~n   Starter: ~p"
      "~n   Id:      ~p", [Parent, Starter, Id]),
    case (catch do_init(Id)) of
	ok ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(#state{parent = Parent, id = Id});
	Error ->
	    d("main -> error: "
	      "~p", [Error]),
	    proc_lib:init_ack(Starter, Error)
    end.

do_init(Id) ->
    erlang:register(?SERVER, self()),
    snmpm:register_user(Id, ?MODULE, self()).


%%----------------------------------------------------------------------

loop(#state{parent = Parent, id = Id} = S) ->
    d("loop -> entry"),
    receive
	{stop, _From} ->
	    d("loop -> received stop request"),
	    exit(normal);

	{{simulate_crash, Reason}, From, Ref} ->
	    d("loop -> received simulate_crash request"),
	    reply(From, ok, Ref),
	    exit(Reason);

	{info, From, Ref} ->
	    d("loop -> received info request"),
	    Res = snmpm:info(),
	    reply(From, Res, Ref),
	    loop(S);

	{system_info, From, Ref} ->
	    d("loop -> received system_info request"),
	    Res = snmpm_config:system_info(),
	    reply(From, Res, Ref),
	    loop(S);

	{{register_agent, TargetName, Conf}, From, Ref} ->
	    d("loop -> received register_agent request"),
	    Res = snmpm:register_agent(Id, TargetName, Conf),
	    reply(From, Res, Ref),
	    loop(S);

	{{register_agent, Addr, Port, Conf}, From, Ref} ->
	    d("loop -> received register_agent request"),
	    Res = snmpm:register_agent(Id, Addr, Port, Conf),
	    reply(From, Res, Ref),
	    loop(S);

	{{unregister_agent, TargetName}, From, Ref} ->
	    d("loop -> received unregister_agent request"),
	    Res = snmpm:unregister_agent(Id, TargetName),
	    reply(From, Res, Ref),
	    loop(S);

	{{unregister_agent, Addr, Port}, From, Ref} ->
	    d("loop -> received unregister_agent request"),
	    Res = snmpm:unregister_agent(Id, Addr, Port),
	    reply(From, Res, Ref),
	    loop(S);

	{{agent_info, TargetName, Item}, From, Ref} ->
	   d("loop -> received agent_info request with"
	     "~n   TargetName: ~p"
	     "~n   Item:       ~p", [TargetName, Item]), 
	    Res = snmpm:agent_info(TargetName, Item),
	   d("loop -> agent_info for ~p"
	     "~n   Res: ~p", [Item, Res]), 
	    reply(From, Res, Ref),
	    loop(S);

	{{agent_info, Addr, Port, Item}, From, Ref} ->
	   d("loop -> received agent_info request with"
	     "~n   Addr: ~p"
	     "~n   Port: ~p"
	     "~n   Item: ~p", [Addr, Port, Item]), 
	    Res = snmpm:agent_info(Addr, Port, Item),
	    reply(From, Res, Ref),
	    loop(S);

	{{update_agent_info, TargetName, Item, Val}, From, Ref} ->
	   d("loop -> received update_agent_info request with"
	     "~n   TargetName: ~p"
	     "~n   Item:       ~p"
	     "~n   Val:        ~p", [TargetName, Item, Val]), 
	    Res = snmpm:update_agent_info(Id, TargetName, Item, Val),
	    reply(From, Res, Ref),
	    loop(S);

	{{update_agent_info, Addr, Port, Item, Val}, From, Ref} ->
	   d("loop -> received update_agent_info request with"
	     "~n   Addr: ~p"
	     "~n   Port: ~p"
	     "~n   Item: ~p"
	     "~n   Val: ~p", [Addr, Port, Item, Val]), 
	    Res = snmpm:update_agent_info(Id, Addr, Port, Item, Val),
	    reply(From, Res, Ref),
	    loop(S);

	{which_all_agents, From, Ref} ->
	    d("loop -> received which_all_agents request"),
	    Res = snmpm:which_agents(),
	    reply(From, Res, Ref),
	    loop(S);

	{which_own_agents, From, Ref} ->
	    d("loop -> received which_own_agents request"),
	    Res = snmpm:which_agents(Id),
	    reply(From, Res, Ref),
	    loop(S);

	{{load_mib, Mib}, From, Ref} ->
	    d("loop -> received load_mib request"),
	    Res = snmpm:load_mib(Mib),
	    reply(From, Res, Ref),
	    loop(S);

	{{unload_mib, Mib}, From, Ref} ->
	    d("loop -> received unload_mib request"),
	    Res = snmpm:unload_mib(Mib),
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (sync) get-request --
	%% 

	{{sync_get2, TargetName, Oids, SendOpts}, From, Ref} 
	  when is_list(TargetName) ->
	    d("loop -> received sync_get2 request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p"
	      "~n   SendOpts:   ~p", [TargetName, Oids, SendOpts]),
	    Res = snmpm:sync_get2(Id, TargetName, Oids, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	%% No agent specified, so send it to all of them
	{{sync_get, Oids}, From, Ref} ->
	    d("loop -> received sync_get request "
	      "(for every agent of this user)"),
	    Res = [snmpm:sync_get(Id, TargetName, Oids) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get, TargetName, Oids}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received sync_get request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p", [TargetName, Oids]),
	    Res = snmpm:sync_get(Id, TargetName, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get, Addr, Oids}, From, Ref} ->
	    d("loop -> received sync_get request with"
	      "~n   Addr: ~p"
	      "~n   Oids: ~p", [Addr, Oids]),
	    Res = snmpm:g(Id, Addr, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get, Addr, Port, Oids}, From, Ref} ->
	    d("loop -> received sync_get request with"
	      "~n   Addr: ~p"
	      "~n   Port: ~p"
	      "~n   Oids: ~p", [Addr, Port, Oids]),
	    Res = snmpm:g(Id, Addr, Port, Oids), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (async) get-request --
	%% 

	{{async_get2, TargetName, Oids, SendOpts}, From, Ref} 
	  when is_list(TargetName) ->
	    d("loop -> received async_get2 request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p"
	      "~n   SendOpts:   ~p", [TargetName, Oids, SendOpts]),
	    Res = snmpm:async_get2(Id, TargetName, Oids, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	%% No agent specified, so send it to all of them
	{{async_get, Oids}, From, Ref} ->
	    d("loop -> received async_get request"),
	    Res = [snmpm:async_get(Id, TargetName, Oids) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get, TargetName, Oids}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received async_get request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p", [TargetName, Oids]),
	    Res = snmpm:async_get(Id, TargetName, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get, Addr, Oids}, From, Ref} ->
	    d("loop -> received async_get request"),
	    Res = snmpm:ag(Id, Addr, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get, Addr, Port, Oids}, From, Ref} ->
	    d("loop -> received async_get request"),
	    Res = snmpm:ag(Id, Addr, Port, Oids), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (sync) get_next-request --
	%% 

	{{sync_get_next2, TargetName, Oids, SendOpts}, From, Ref} 
	  when is_list(TargetName) ->
	    d("loop -> received sync_get_next2 request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p"
	      "~n   SendOpts:   ~p", [TargetName, Oids, SendOpts]),
	    Res = snmpm:sync_get_next2(Id, TargetName, Oids, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	%% No agent specified, so send it to all of them
	{{sync_get_next, Oids}, From, Ref} ->
	    d("loop -> received sync_get_next request"),
	    Res = [snmpm:sync_get_next(Id, TargetName, Oids) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get_next, TargetName, Oids}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received sync_get_next request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p", [TargetName, Oids]),
	    Res = snmpm:sync_get_next(Id, TargetName, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get_next, Addr, Oids}, From, Ref} ->
	    d("loop -> received sync_get_next request"),
	    Res = snmpm:gn(Id, Addr, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get_next, Addr, Port, Oids}, From, Ref} ->
	    d("loop -> received sync_get_next request"),
	    Res = snmpm:gn(Id, Addr, Port, Oids), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (async) get_next-request --
	%% 

	{{async_get_next2, TargetName, Oids, SendOpts}, From, Ref} 
	  when is_list(TargetName) ->
	    d("loop -> received async_get_next2 request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p"
	      "~n   SendOpts:   ~p", [TargetName, Oids, SendOpts]),
	    Res = snmpm:async_get_next2(Id, TargetName, Oids, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	%% No agent specified, so send it to all of them
	{{async_get_next, Oids}, From, Ref} ->
	    d("loop -> received async_get_next request"),
	    Res = [snmpm:async_get_next(Id, TargetName, Oids) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get_next, TargetName, Oids}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received async_get_next request with"
	      "~n   TargetName: ~p"
	      "~n   Oids:       ~p", [TargetName, Oids]),
	    Res = snmpm:async_get_next(Id, TargetName, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get_next, Addr, Oids}, From, Ref} ->
	    d("loop -> received async_get_next request"),
	    Res = snmpm:agn(Id, Addr, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get_next, Addr, Port, Oids}, From, Ref} ->
	    d("loop -> received async_get_next request"),
	    Res = snmpm:agn(Id, Addr, Port, Oids), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (sync) set-request --
	%% 

	{{sync_set2, TargetName, VAV, SendOpts}, From, Ref} 
	  when is_list(TargetName) ->
	    d("loop -> received sync_set2 request with"
	      "~n   TargetName: ~p"
	      "~n   VAV:        ~p"
	      "~n   SendOpts:   ~p", [TargetName, VAV, SendOpts]),
	    Res = snmpm:sync_set2(Id, TargetName, VAV, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_set, VAV}, From, Ref} ->
	    d("loop -> received sync_set request"),
	    Res = [snmpm:sync_set(Id, TargetName, VAV) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_set, TargetName, VAV}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received sync_set request"),
	    Res = snmpm:sync_set(Id, TargetName, VAV), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_set, Addr, VAV}, From, Ref} ->
	    d("loop -> received sync_set request"),
	    Res = snmpm:s(Id, Addr, VAV), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_set, Addr, Port, VAV}, From, Ref} ->
	    d("loop -> received sync_set request"),
	    Res = snmpm:s(Id, Addr, Port, VAV), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (async) set-request --
	%% 

	{{async_set2, TargetName, VAV, SendOpts}, From, Ref} 
	  when is_list(TargetName) ->
	    d("loop -> received async_set2 request with"
	      "~n   TargetName: ~p"
	      "~n   VAV:        ~p"
	      "~n   SendOpts:   ~p", [TargetName, VAV, SendOpts]),
	    Res = snmpm:async_set2(Id, TargetName, VAV, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_set, VAV}, From, Ref} ->
	    d("loop -> received async_set request"),
	    Res = [snmpm:async_set(Id, TargetName, VAV) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{async_set, TargetName, VAV}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received async_set request"),
	    Res = snmpm:async_set(Id, TargetName, VAV), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_set, Addr, VAV}, From, Ref} ->
	    d("loop -> received async_set request"),
	    Res = snmpm:as(Id, Addr, VAV), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_set, Addr, Port, VAV}, From, Ref} ->
	    d("loop -> received async_set request"),
	    Res = snmpm:as(Id, Addr, Port, VAV), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (sync) get-bulk-request --
	%% 

	{{sync_get_bulk2, TargetName, NonRep, MaxRep, Oids, SendOpts}, From, Ref} 
	  when is_list(TargetName) ->
	    d("loop -> received sync_get_bulk request with"
	      "~n   TargetName: ~p"
	      "~n   NonRep:     ~w"
	      "~n   MaxRep:     ~w"
	      "~n   Oids:       ~p"
	      "~n   SendOpts:   ~p", 
	      [TargetName, NonRep, MaxRep, Oids, SendOpts]),
	    Res = snmpm:sync_get_bulk2(Id, TargetName, 
				       NonRep, MaxRep, Oids, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	%% No agent specified, so send it to all of them
	{{sync_get_bulk, NonRep, MaxRep, Oids}, From, Ref} ->
	    d("loop -> received sync_get_bulk request with"
	      "~n   NonRep: ~w"
	      "~n   MaxRep: ~w"
	      "~n   Oids:   ~p", [NonRep, MaxRep, Oids]),
	    Res = [snmpm:sync_get_bulk(Id, TargetName, NonRep, MaxRep, Oids) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get_bulk, TargetName, NonRep, MaxRep, Oids}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received sync_get_bulk request with"
	      "~n   TargetName: ~p"
	      "~n   NonRep:     ~w"
	      "~n   MaxRep:     ~w"
	      "~n   Oids:       ~p", [TargetName, NonRep, MaxRep, Oids]),
	    Res = snmpm:sync_get_bulk(Id, TargetName, NonRep, MaxRep, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get_bulk, Addr, NonRep, MaxRep, Oids}, From, Ref} ->
	    d("loop -> received sync_get_bulk request with"
	      "~n   Addr:   ~p"
	      "~n   NonRep: ~w"
	      "~n   MaxRep: ~w"
	      "~n   Oids:   ~p", [Addr, NonRep, MaxRep, Oids]),
	    Res = snmpm:gb(Id, Addr, NonRep, MaxRep, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{sync_get_bulk, Addr, Port, NonRep, MaxRep, Oids}, From, Ref} ->
	    d("loop -> received sync_get_bulk request with"
	      "~n   Addr:   ~p"
	      "~n   Port:   ~w"
	      "~n   NonRep: ~w"
	      "~n   MaxRep: ~w"
	      "~n   Oids:   ~p", [Addr, Port, NonRep, MaxRep, Oids]),
	    Res = snmpm:gb(Id, Addr, Port, NonRep, MaxRep, Oids), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- (async) get-bulk-request --
	%% 

	{{async_get_bulk2, TargetName, NonRep, MaxRep, Oids, SendOpts}, 
	 From, Ref} when is_list(TargetName) ->
	    d("loop -> received async_get_bulk2 request with"
	      "~n   TargetName: ~p"
	      "~n   NonRep:     ~w"
	      "~n   MaxRep:     ~w"
	      "~n   Oids:       ~p"
	      "~n   SendOpts:   ~p", 
	      [TargetName, NonRep, MaxRep, Oids, SendOpts]),
	    Res = snmpm:async_get_bulk2(Id, TargetName, 
					NonRep, MaxRep, Oids, SendOpts), 
	    reply(From, Res, Ref),
	    loop(S);

	%% No agent specified, so send it to all of them
	{{async_get_bulk, NonRep, MaxRep, Oids}, From, Ref} ->
	    d("loop -> received async_get_bulk request with"
	      "~n   NonRep: ~w"
	      "~n   MaxRep: ~w"
	      "~n   Oids:   ~p", [NonRep, MaxRep, Oids]),
	    Res = [snmpm:async_get_bulk(Id, TargetName, NonRep, MaxRep, Oids) ||
		      TargetName <- snmpm:which_agents(Id)],
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get_bulk, TargetName, NonRep, MaxRep, Oids}, From, Ref} when is_list(TargetName) ->
	    d("loop -> received async_get_bulk request with"
	      "~n   TargetName: ~p"
	      "~n   NonRep:     ~w"
	      "~n   MaxRep:     ~w"
	      "~n   Oids:       ~p", [TargetName, NonRep, MaxRep, Oids]),
	    Res = snmpm:async_get_bulk(Id, TargetName, NonRep, MaxRep, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get_bulk, Addr, NonRep, MaxRep, Oids}, From, Ref} ->
	    d("loop -> received async_get_bulk request with"
	      "~n   Addr:   ~p"
	      "~n   NonRep: ~w"
	      "~n   MaxRep: ~w"
	      "~n   Oids:   ~p", [Addr, NonRep, MaxRep, Oids]),
	    Res = snmpm:agb(Id, Addr, NonRep, MaxRep, Oids), 
	    reply(From, Res, Ref),
	    loop(S);

	{{async_get_bulk, Addr, Port, NonRep, MaxRep, Oids}, From, Ref} ->
	    d("loop -> received async_get_bulk request with"
	      "~n   Addr:   ~p"
	      "~n   Port:   ~w"
	      "~n   NonRep: ~w"
	      "~n   MaxRep: ~w"
	      "~n   Oids:   ~p", [Addr, Port, NonRep, MaxRep, Oids]),
	    Res = snmpm:agb(Id, Addr, Port, NonRep, MaxRep, Oids), 
	    reply(From, Res, Ref),
	    loop(S);


	%% 
	%% -- logical name translation --
	%% 

	{{name_to_oid, Name}, From, Ref} ->
	    d("loop -> received name_to_oid request for"
	      "~n   Name: ~p", [Name]),
	    Res = snmpm:name_to_oid(Name), 
	    reply(From, Res, Ref),
	    loop(S);

	{{oid_to_name, Oid}, From, Ref} ->
	    d("loop -> received oid_to_name request for"
	      "~n   Oid: ~p", [Oid]),
	    Res = snmpm:oid_to_name(Oid), 
	    reply(From, Res, Ref),
	    loop(S);

	{{purify_oid, Oid}, From, Ref} ->
	    d("loop -> received purify_oid request for"
	      "~n   Oid: ~p", [Oid]),
	    Res = do_purify_oid(Oid),
	    reply(From, Res, Ref),
	    loop(S);


	%% SNMP manager callback messages (from our callback API):

	{handle_error, _Pid, ReqId, Reason} ->
	    d("loop -> received error callback from manager for ~w:"
	      "~n   ~p", [ReqId, Reason]),
	    Parent ! {async_event, ReqId, {error, Reason}},
	    loop(S);

	{handle_agent, _Pid, Addr, Port, SnmpInfo} ->
	    d("loop -> received agent callback from manager for ~n   ~p:~w", 
	      [Addr, Port]),
	    Parent ! {async_event, {Addr, Port}, {agent, SnmpInfo}},
	    loop(S);

	{handle_pdu, _Pid, _TargetName, ReqId, SnmpResponse} ->
	    d("loop -> received pdu callback from manager for ~w", [ReqId]),
	    Parent ! {async_event, ReqId, {pdu, SnmpResponse}},
	    loop(S);

	%% For backwards compatibillity 
	{handle_pdu, _Pid, _Addr, _Port, ReqId, SnmpResponse} ->
	    d("loop -> received pdu callback from manager for ~w", [ReqId]),
	    Parent ! {async_event, ReqId, {pdu, SnmpResponse}},
	    loop(S);

	{handle_trap, _Pid, TargetName, SnmpTrap} ->
	    d("loop -> received trap callback from manager for "
	      "~n   ~p", 
	      "~n   ~p", 
	      [TargetName, SnmpTrap]),
	    Parent ! {async_event, TargetName, {trap, SnmpTrap}}, 
	    loop(S);

	%% For backwards compatibillity 
	{handle_trap, _Pid, Addr, Port, SnmpTrap} ->
	    d("loop -> received trap callback from manager for "
	      "~n   ~p:~w", 
	      "~n   ~p", 
	      [Addr, Port, SnmpTrap]),
	    Parent ! {async_event, {Addr, Port}, {trap, SnmpTrap}}, 
	    loop(S);

	{handle_inform, Pid, TargetName, SnmpInform} ->
	    d("loop -> received inform callback from manager for "
	      "~n   ~p", 
	      "~n   ~p", 
	      [TargetName, SnmpInform]),
	    Parent ! {async_event, TargetName, {inform, Pid, SnmpInform}}, 
	    loop(S);

	%% For backwards compatibillity 
	{handle_inform, Pid, Addr, Port, SnmpInform} ->
	    d("loop -> received inform callback from manager for "
	      "~n   ~p:~w", 
	      "~n   ~p", 
	      [Addr, Port, SnmpInform]),
	    Parent ! {async_event, {Addr, Port}, {inform, Pid, SnmpInform}}, 
	    loop(S);

	{handle_report, _Pid, TargetName, SnmpReport} ->
	    d("loop -> received report callback from manager for "
	      "~n   ~p", 
	      "~n   ~p", 
	      [TargetName, SnmpReport]),
	    Parent ! {async_event, TargetName, {report, SnmpReport}}, 
	    loop(S);

	%% For backwards compatibillity 
	{handle_report, _Pid, Addr, Port, SnmpReport} ->
	    d("loop -> received report callback from manager for "
	      "~n   ~p:~w", 
	      "~n   ~p", 
	      [Addr, Port, SnmpReport]),
	    Parent ! {async_event, {Addr, Port}, {report, SnmpReport}}, 
	    loop(S);

	{'EXIT', Parent, Reason} ->
	    d("received exit signal from parent: ~n~p", [Reason]),
	    info("received exit signal from parent: ~n~p", [Reason]),
	    exit(Reason);

	Unknown ->
	    d("received unknown message: ~n~p", [Unknown]),
	    info("received unknown message: ~n~p", [Unknown]),
	    loop(S)
    end.
	    

%% -------------

do_purify_oid([A|T]) when is_atom(A) ->
    case snmpm:name_to_oid(A) of
	{ok, [Oid|_]} ->
	    verify_pure_oid(lists:flatten([Oid|T]));
	{error, not_found} ->
	    {error, {not_found, A}};
	{error, _} = Error ->
	    Error
    end;
do_purify_oid(L) when is_list(L) ->
    verify_pure_oid(lists:flatten(L));
do_purify_oid(X) ->
    {error, {unpure_oid, X}}.

verify_pure_oid([]) ->
    [];
verify_pure_oid([H | T]) when is_integer(H) andalso (H >= 0) ->
    [H | verify_pure_oid(T)];
verify_pure_oid([H | _]) ->
    throw({error, {not_pure_oid, H}}).


%% -------------

info(F, A) ->
    error_logger:info_msg("TEST MGR USER " ++ F ++ "~n", A).


%% -------------

call(Req) ->
    call(Req, 5000).

call(Req, To) when is_integer(To) ->
    Ref = make_ref(),
    ?SERVER ! {Req, self(), Ref},
    receive
	{Reply, Ref} ->
	    Reply
    after To ->
	    {error, timeout}
    end.

reply(Pid, Reply, Ref) -> 
    d("reply -> entry with"
      "~n   Pid:   ~p"
      "~n   Reply: ~p"
      "~n   Ref:   ~p", [Pid, Reply, Ref]),
    Pid ! {Reply, Ref}.

cast(Msg) ->
    ?SERVER ! {Msg, self()},
    ok.


%%----------------------------------------------------------------------
%% User callback functions:
%%----------------------------------------------------------------------

handle_error(ReqId, Reason, UserPid) ->
    UserPid ! {handle_error, self(), ReqId, Reason},
    ignore.
 
 
handle_agent(Addr, Port, SnmpInfo, UserPid) ->
    UserPid ! {handle_agent, self(), Addr, Port, SnmpInfo},
    ignore.
 
 
handle_pdu(TargetName, ReqId, SnmpResponse, UserPid) ->
    UserPid ! {handle_pdu, self(), TargetName, ReqId, SnmpResponse},
    ignore.
 
%% For backwards compatibillity 
handle_pdu(Addr, Port, ReqId, SnmpResponse, UserPid) ->
    UserPid ! {handle_pdu, self(), Addr, Port, ReqId, SnmpResponse},
    ignore.
 
 
handle_trap(TargetName, SnmpTrap, UserPid) ->
    UserPid ! {handle_trap, self(), TargetName, SnmpTrap},
    ok.
 
%% For backwards compatibillity 
handle_trap(Addr, Port, SnmpTrap, UserPid) ->
    UserPid ! {handle_trap, self(), Addr, Port, SnmpTrap},
    ok.
 
 
handle_inform(TargetName, SnmpInform, UserPid) ->
    UserPid ! {handle_inform, self(), TargetName, SnmpInform},
    receive
	{handle_inform_no_response, TargetName} ->
	    no_reply;
	{handle_inform_response, TargetName} ->
	    ok
    end.

%% For backwards compatibillity 
handle_inform(Addr, Port, SnmpInform, UserPid) ->
    UserPid ! {handle_inform, self(), Addr, Port, SnmpInform},
    receive
	{handle_inform_no_response, {Addr, Port}} ->
	    no_reply;
	{handle_inform_response, {Addr, Port}} ->
	    ok
    end.


handle_report(TargetName, SnmpReport, UserPid) ->
    UserPid ! {handle_report, self(), TargetName, SnmpReport},
    ok.

%% For backwards compatibillity 
handle_report(Addr, Port, SnmpReport, UserPid) ->
    UserPid ! {handle_report, self(), Addr, Port, SnmpReport},
    ok.


%%----------------------------------------------------------------------
%% Debug
%%----------------------------------------------------------------------

d(F) ->
    d(F, []).

d(F, A) ->
    d(get(debug), F, A).

d(true, F, A) ->
    io:format("~w:" ++ F ++ "~n", [?SERVER|A]);
d(_, _, _) ->
    ok.
