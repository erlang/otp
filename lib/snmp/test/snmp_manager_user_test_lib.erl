%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(snmp_manager_user_test_lib).

-behaviour(snmpm_user).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
         start_link/0, stop/1,
	 simulate_crash/2,
	 register/2, register/3, 
	 register_monitor/2, register_monitor/3, 
	 unregister/1, unregister/2,
	 register_agent/4
        ]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
         user/1
        ]).

-export([
	 handle_error/3,
         handle_agent/4,
         handle_pdu/4,
         handle_pdu/5,     % For backwards compatibillity 
         handle_trap/3,
         handle_trap/4,    % For backwards compatibillity 
         handle_inform/3,
         handle_inform/4,  % For backwards compatibillity 
         handle_report/3, 
         handle_report/4   % For backwards compatibillity 
	]).


-record(state, {parent, ids = []}).


%%----------------------------------------------------------------------
%% The user simulation API
%%----------------------------------------------------------------------

start_link() ->
    S = #state{parent = self()},
    proc_lib:start_link(?MODULE, user, [S]).

stop(Pid) ->
    cast(Pid, stop).

simulate_crash(Pid, Reason) ->
    call(Pid, {simulate_crash, Reason}).

%% For backwards compatibillity 
register(Pid, Id) ->
    call(Pid, {register, Id}).

register(Pid, Id, DefaultAgentConfig) ->
    call(Pid, {register, Id, DefaultAgentConfig}).

%% For backwards compatibillity 
register_monitor(Pid, Id) ->
    call(Pid, {register_monitor, Id}).

register_monitor(Pid, Id, DefaultAgentConfig) ->
    call(Pid, {register_monitor, Id, DefaultAgentConfig}).

unregister(Pid) ->
    call(Pid, unregister).

unregister(Pid, Id) ->
    call(Pid, {unregister, Id}).

register_agent(Pid, Id, TargetName, AgentConfig) ->
    call(Pid, {register_agent, Id, TargetName, AgentConfig}).

user(#state{parent = Pid} = S) ->
    proc_lib:init_ack(Pid, {ok, self()}),
    user_loop(S).

user_loop(#state{parent = Parent} = S) ->
    receive
	{stop, Parent} ->
	    exit(normal);

	{{simulate_crash, Reason}, Parent, Ref} ->
	    reply(Parent, ok, Ref),
	    exit(Reason);
	
	%% For backwards compatibillity 
	{{register, Id}, Parent, Ref} ->
	    IDs = S#state.ids,
	    case lists:member(Id, IDs) of
		false ->
		    Res = snmpm:register_user(Id, ?MODULE, self()),
		    reply(Parent, Res, Ref),
		    user_loop(S#state{ids = [Id|IDs]});
		true ->
		    reply(Parent, {error, already_registered}, Ref),
		    user_loop(S)
	    end;
	
	{{register, Id, DefaultAgentConf}, Parent, Ref} ->
	    IDs = S#state.ids,
	    case lists:member(Id, IDs) of
		false ->
		    Res = snmpm:register_user(Id, ?MODULE, self(), 
					      DefaultAgentConf),
		    reply(Parent, Res, Ref),
		    user_loop(S#state{ids = [Id|IDs]});
		true ->
		    reply(Parent, {error, already_registered}, Ref),
		    user_loop(S)
	    end;
	
	%% For backwards compatibillity 
	{{register_monitor, Id}, Parent, Ref} ->
	    IDs = S#state.ids,
	    case lists:member(Id, IDs) of
		false ->
		    Res = snmpm:register_user_monitor(Id, ?MODULE, self()),
		    reply(Parent, Res, Ref),
		    user_loop(S#state{ids = [Id|IDs]});
		true ->
		    reply(Parent, {error, already_registered}, Ref),
		    user_loop(S)
	    end;
	
	{{register_monitor, Id, DefaultAgentConf}, Parent, Ref} ->
	    IDs = S#state.ids,
	    case lists:member(Id, IDs) of
		false ->
		    Res = snmpm:register_user_monitor(Id, ?MODULE, self(), 
						      DefaultAgentConf),
		    reply(Parent, Res, Ref),
		    user_loop(S#state{ids = [Id|IDs]});
		true ->
		    reply(Parent, {error, already_registered}, Ref),
		    user_loop(S)
	    end;
	
	{unregister, Parent, Ref} ->
	    Res = [snmpm:unregister_user(Id) || Id <- S#state.ids],
	    reply(Parent, {ok, Res}, Ref),
	    user_loop(S);
	
	{{unregister, Id}, Parent, Ref} ->
	    IDs = S#state.ids,
	    IDs2 = 
		case lists:member(Id, IDs) of
		    true ->
			Res = snmpm:unregister_user(Id),
			reply(Parent, Res, Ref),
			lists:delete(Id, IDs);
		    false ->
			reply(Parent, {error, not_registered}, Ref),
			IDs
		end,
	    user_loop(S#state{ids = IDs2});
	
	{{register_agent, Id, TargetName, Config}, Parent, Ref} ->
	    IDs = S#state.ids,
	    case lists:member(Id, IDs) of
		true ->
		    Res = snmpm:register_agent(Id, TargetName, Config),
		    reply(Parent, Res, Ref),
		    user_loop(S);
		false ->
		    reply(Parent, {error, {unknown_user, Id}}, Ref),
		    user_loop(S)
	    end;
	


	%% SNMP manager callback messages (from our callback API):

	{handle_error, Pid, ReqId, Reason} ->
	    do_handle_error(Pid, ReqId, Reason),
	    user_loop(S);

	{handle_agent, Pid, Addr, Port, SnmpInfo} ->
	    do_handle_agent(Pid, Addr, Port, SnmpInfo),
	    user_loop(S);

	{handle_pdu, Pid, TargetName, ReqId, SnmpResponse} ->
	    do_handle_pdu(Pid, TargetName, ReqId, SnmpResponse),
	    user_loop(S);

	{handle_pdu, Pid, Addr, Port, ReqId, SnmpResponse} ->
	    do_handle_pdu(Pid, Addr, Port, ReqId, SnmpResponse),
	    user_loop(S);

	{handle_trap, Pid, TargetName, SnmpTrap} ->
	    do_handle_trap(Pid, TargetName, SnmpTrap),
	    user_loop(S);

	{handle_trap, Pid, Addr, Port, SnmpTrap} ->
	    do_handle_trap(Pid, Addr, Port, SnmpTrap),
	    user_loop(S);

	{handle_inform, Pid, TargetName, SnmpInform} ->
	    do_handle_inform(Pid, TargetName, SnmpInform),
	    user_loop(S);

	{handle_inform, Pid, Addr, Port, SnmpInform} ->
	    do_handle_inform(Pid, Addr, Port, SnmpInform),
	    user_loop(S);

	{handle_report, Pid, TargetName, SnmpReport} ->
	    do_handle_report(Pid, TargetName, SnmpReport),
	    user_loop(S);

	{handle_report, Pid, Addr, Port, SnmpReport} ->
	    do_handle_report(Pid, Addr, Port, SnmpReport),
	    user_loop(S);

	Unknown ->
	    info("received unknown message: ~n~p", [Unknown]),
	    user_loop(S)
    end.
	    

%% -------------

do_handle_error(Pid, ReqId, Reason) ->
    info("received error callback:"
         "~n   ReqId:    ~p"
         "~n   Reason:   ~p", [ReqId, Reason]),
    Pid ! {ignore, self()},
    ok.


do_handle_agent(Pid, Addr, Port, SnmpInfo) ->
    info("received agent callback:"
         "~n   Addr:     ~p"
         "~n   Port:     ~p"
         "~n   SnmpInfo: ~p", [Addr, Port, SnmpInfo]),
    Pid ! {ignore, self()},
    ok.


do_handle_pdu(Pid, TargetName, ReqId, SnmpResponse) ->
    info("received pdu callback:"
         "~n   TargetName:   ~p"
         "~n   ReqId:        ~p"
         "~n   SnmpResponse: ~p", [TargetName, ReqId, SnmpResponse]),
    Pid ! {ignore, self()},
    ok.

%% For backwards compatibillity 
do_handle_pdu(Pid, Addr, Port, ReqId, SnmpResponse) ->
    info("received pdu callback:"
         "~n   Addr:         ~p"
         "~n   Port:         ~p"
         "~n   ReqId:        ~p"
         "~n   SnmpResponse: ~p", [Addr, Port, ReqId, SnmpResponse]),
    Pid ! {ignore, self()},
    ok.


do_handle_trap(Pid, TargetName, SnmpTrap) ->
    info("received trap callback:"
         "~n   TargetName: ~p"
         "~n   SnmpTrap:   ~p", [TargetName, SnmpTrap]),
    Pid ! {ignore, self()},
    ok.

%% For backwards compatibillity 
do_handle_trap(Pid, Addr, Port, SnmpTrap) ->
    info("received trap callback:"
         "~n   Addr:     ~p"
         "~n   Port:     ~p"
         "~n   SnmpTrap: ~p", [Addr, Port, SnmpTrap]),
    Pid ! {ignore, self()},
    ok.


do_handle_inform(Pid, TargetName, SnmpInform) ->
    info("received inform callback:"
         "~n   TargetName: ~p"
         "~n   SnmpInform: ~p", [TargetName, SnmpInform]),
    Pid ! {ignore, self()},
    ok.

%% For backwards compatibillity 
do_handle_inform(Pid, Addr, Port, SnmpInform) ->
    info("received inform callback:"
         "~n   Addr:       ~p"
         "~n   Port:       ~p"
         "~n   SnmpInform: ~p", [Addr, Port, SnmpInform]),
    Pid ! {ignore, self()},
    ok.


do_handle_report(Pid, TargetName, SnmpReport) ->
    info("received report callback:"
         "~n   TargetName: ~p"
         "~n   SnmpReport: ~p", [TargetName, SnmpReport]),
    Pid ! {ignore, self()},
    ok.

%% For backwards compatibillity 
do_handle_report(Pid, Addr, Port, SnmpReport) ->
    info("received report callback:"
         "~n   Addr:       ~p"
         "~n   Port:       ~p"
         "~n   SnmpReport: ~p", [Addr, Port, SnmpReport]),
    Pid ! {ignore, self()},
    ok.


info(F, A) ->
    error_logger:info_msg("USER SIMULATOR " ++ F ++ "~n", A).


%% -------------

call(UserPid, Req) ->
    call(UserPid, Req, 5000).

call(UserPid, Req, To) ->
    Ref = make_ref(),
    UserPid ! {Req, self(), Ref},
    receive
	{Reply, UserPid, Ref} ->
	    Reply
    after To ->
	    {error, timeout}
    end.

reply(Pid, Reply, Ref) ->    
    Pid ! {Reply, self(), Ref}.

cast(UserPid, Msg) ->
    UserPid ! {Msg, self()},
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
    ok.

%% For backwards compatibillity 
handle_inform(Addr, Port, SnmpInform, UserPid) ->
    UserPid ! {handle_inform, self(), Addr, Port, SnmpInform},
    ok.


handle_report(TargetName, SnmpReport, UserPid) ->
    UserPid ! {handle_report, self(), TargetName, SnmpReport},
    ok.

%% For backwards compatibillity 
handle_report(Addr, Port, SnmpReport, UserPid) ->
    UserPid ! {handle_report, self(), Addr, Port, SnmpReport},
    ok.
