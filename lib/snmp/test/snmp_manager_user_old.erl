%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Utility functions for the (snmp manager) user test(s).
%%----------------------------------------------------------------------

-module(snmp_manager_user_old).

-behaviour(snmpm_user_old).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 start/0, start/1, 
	 stop/0,
	 register_agent/1, register_agent/2, register_agent/3, 
	 unregister_agent/1, unregister_agent/2, 
	 agent_info/3, 
	 update_agent_info/4, 
	 which_agents/0, which_users/0, 
	 load_mib/1, unload_mib/1, 
	 sync_get/2,       sync_get/3,
	 async_get/2,      async_get/3,
	 sync_get_next/2,  sync_get_next/3,
	 async_get_next/2, async_get_next/3,
	 sync_set/2,       sync_set/3, 
	 async_set/2,      async_set/3, 
	 sync_get_bulk/4,  sync_get_bulk/5,
	 async_get_bulk/4, async_get_bulk/5,
	 name_to_oid/1, oid_to_name/1, 
	 purify_oid/1	 
        ]).

-export([
	 handle_error/3,
         handle_agent/4,
         handle_pdu/5,    
         handle_trap/4,   
         handle_inform/4, 
         handle_report/4  
	]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

%% -define(SERVER, ?MODULE).
-define(USER_ID, ?MODULE).

%% -record(state, {parent, id, reqs = []}).
%% -record(request, {from, ref, tmr, req_id, type}).


%%----------------------------------------------------------------------
%% The user API
%%----------------------------------------------------------------------

start() ->
    start([]).

start(DefaultAgentConfig) when is_list(DefaultAgentConfig) ->
    snmpm:register_user(?USER_ID, ?MODULE, self(), DefaultAgentConfig).

stop() ->
    snmpm:unregister_user(?USER_ID).

register_agent(Addr) ->
    snmpm:register_agent(?USER_ID, Addr).

register_agent(Addr, PortOrConfig) ->
    snmpm:register_agent(?USER_ID, Addr, PortOrConfig).

register_agent(Addr, Port, Config) ->
    snmpm:register_agent(?USER_ID, Addr, Port, Config).

unregister_agent(Addr) ->
    snmpm:unregister_agent(?USER_ID, Addr).

unregister_agent(Addr, Port) ->
    snmpm:unregister_agent(?USER_ID, Addr, Port).

agent_info(Addr, Port, Item) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:agent_info(TargetName, Item);
	Error ->
	    Error
    end.

update_agent_info(Addr, Port, Item, Val) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:update_agent_info(?USER_ID, TargetName, Item, Val);
	Error ->
	    Error
    end.

which_agents() ->
    snmpm:which_agents().

which_users() ->
    snmpm:which_users().

load_mib(Mib) ->
    snmpm:load_mib(?USER_ID, Mib).

unload_mib(Mib) ->
    snmpm:unload_mib(?USER_ID, Mib).


%% -- 

sync_get(Addr, Oids) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:sync_get2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.

sync_get(Addr, Port, Oids) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:sync_get2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.


%% --

async_get(Addr, Oids) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:async_get2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.

async_get(Addr, Port, Oids) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:async_get2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.


%% --

sync_get_next(Addr, Oids) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:sync_get_next2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.

sync_get_next(Addr, Port, Oids) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:sync_get_next2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.


%% --

async_get_next(Addr, Oids) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:async_get_next2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.

async_get_next(Addr, Port, Oids) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:async_get_next2(?USER_ID, TargetName, Oids);
	Error ->
	    Error
    end.


%% --

sync_set(Addr, VAV) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:sync_set2(?USER_ID, TargetName, VAV);
	Error ->
	    Error
    end.

sync_set(Addr, Port, VAV) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:sync_set2(?USER_ID, TargetName, VAV);
	Error ->
	    Error
    end.


%% --

async_set(Addr, VAV) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:async_set2(?USER_ID, TargetName, VAV);
	Error ->
	    Error
    end.

async_set(Addr, Port, VAV) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:async_set2(?USER_ID, TargetName, VAV);
	Error ->
	    Error
    end.


%% --

sync_get_bulk(Addr, NonRep, MaxRep, Oids) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:sync_get_bulk2(?USER_ID, TargetName, NonRep, MaxRep, Oids);
	Error ->
	    Error
    end.

sync_get_bulk(Addr, Port, NonRep, MaxRep, Oids) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:sync_get_bulk2(?USER_ID, TargetName, NonRep, MaxRep, Oids);
	Error ->
	    Error
    end.


%% --

async_get_bulk(Addr, NonRep, MaxRep, Oids) ->
    case snmpm:target_name(Addr) of
	{ok, TargetName} ->
	    snmpm:async_get_bulk2(?USER_ID, TargetName, NonRep, MaxRep, Oids);
	Error ->
	    Error
    end.

async_get_bulk(Addr, Port, NonRep, MaxRep, Oids) ->
    case snmpm:target_name(Addr, Port) of
	{ok, TargetName} ->
	    snmpm:async_get_bulk2(?USER_ID, TargetName, NonRep, MaxRep, Oids);
	Error ->
	    Error
    end.


%% -- 

name_to_oid(Name) ->
    snmpm:name_to_oid(Name).

oid_to_name(Oid) ->
    snmpm:oid_to_name(Oid).

purify_oid(Oid) ->
    snmpm:purify_oid(Oid).


%%----------------------------------------------------------------------
%% User callback functions:
%%----------------------------------------------------------------------

handle_error(ReqId, Reason, UserPid) ->
    UserPid ! {handle_error, self(), ReqId, Reason},
    ignore.
 
 
handle_agent(Addr, Port, SnmpInfo, UserPid) ->
    UserPid ! {handle_agent, self(), Addr, Port, SnmpInfo},
    ignore.
 
 
handle_pdu(Addr, Port, ReqId, SnmpResponse, UserPid) ->
    UserPid ! {handle_pdu, self(), Addr, Port, ReqId, SnmpResponse},
    ignore.
 

handle_trap(Addr, Port, SnmpTrap, UserPid) ->
    UserPid ! {handle_trap, self(), Addr, Port, SnmpTrap},
    ok.
 
 
handle_inform(Addr, Port, SnmpInform, UserPid) ->
    UserPid ! {handle_inform, self(), Addr, Port, SnmpInform},
    receive
	{handle_inform_no_response, {Addr, Port}} ->
	    no_reply;
	{handle_inform_response, {Addr, Port}} ->
	    ok
    end.


handle_report(Addr, Port, SnmpReport, UserPid) ->
    UserPid ! {handle_report, self(), Addr, Port, SnmpReport},
    ok.


%%----------------------------------------------------------------------
%% Debug
%%----------------------------------------------------------------------

%% d(F) ->
%%     d(F, []).

%% d(F, A) ->
%%     d(get(debug), F, A).

%% d(true, F, A) ->
%%     io:format("~w:" ++ F ++ "~n", [?SERVER|A]);
%% d(_, _, _) ->
%%     ok.
