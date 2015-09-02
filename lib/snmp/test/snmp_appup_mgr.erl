%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2015. All Rights Reserved.
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
%% Purpose: Simple (snmp) manager used when performing appup tests.
%%----------------------------------------------------------------------
-module(snmp_appup_mgr).

-behaviour(snmpm_user).

-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([start/0, start/1, start/2]).
-export([handle_error/3, 
	 handle_agent/4,
	 handle_pdu/5,
	 handle_trap/4,
	 handle_inform/4,
	 handle_report/4]).
-export([main/2]).

-record(agent, {host, port, conf}).
-record(state, {timer, reqs, ids, agent}).

-define(USER_ID,      ?MODULE).
-define(REQ_TIMEOUT,  10000).
-define(POLL_TIMEOUT, 5000).
-define(DEFAULT_PORT, 4000).
%% -define(DEFAULT_PORT, 161).

-define(v1_2(V1,V2),
	case get(vsn) of
	    v1 -> V1;
	    _  -> V2
	end).


start() ->
    {ok, AgentHost} = inet:gethostname(),
    AgentPort = ?DEFAULT_PORT,
    start(AgentHost, AgentPort).

start(AgentPort) when is_integer(AgentPort) ->
    {ok, AgentHost} = inet:gethostname(),
    start(AgentHost, AgentPort);
start(AgentHost) when is_list(AgentHost) ->
    AgentPort = 161,
    start(AgentHost, AgentPort).

start(AgentHost, AgentPort) 
  when is_list(AgentHost) and is_integer(AgentPort) ->
    ensure_started(snmp),
    Pid = erlang:spawn_link(?MODULE, main, [AgentHost, AgentPort]),
    receive
	{'EXIT', Pid, normal} ->
	    ok;
	{'EXIT', Pid, Reason} ->
	    {error, {unexpected_exit, Reason}}
    end.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, _}} ->
	    ok;
	{error, Reason} ->
	    exit(Reason)
    end.

poll_timer() ->
    poll_timer(first).

poll_timer(How) ->
    erlang:send_after(?POLL_TIMEOUT, self(), {poll_timeout, How}).

next_poll_type(first) ->
    all;
next_poll_type(all) ->
    first.

main(AgentHost, AgentPort) ->
    ok = snmpm:register_user_monitor(?USER_ID, ?MODULE, self()),
    AgentConf = [{community, "all-rights"},
		 {engine_id, "agentEngine"},
		 {sec_level, noAuthNoPriv},
		 {version,   v1}],
    ok = snmpm:register_agent(?USER_ID, AgentHost, AgentPort, AgentConf),
    Reqs = [{"sysDescr",    get, ?sysDescr_instance},
	    {"sysObjectID", get, ?sysObjectID_instance},
	    {"sysUpTime",   get, ?sysUpTime_instance}],
    Agent = #agent{host = AgentHost, port = AgentPort, conf = AgentConf},
    State = #state{timer = poll_timer(), reqs = Reqs, agent = Agent},
    loop(State).

loop(State) ->
    receive
	{poll_timeout, How} ->
	    NewState = handle_poll_timeout(State, How),
	    loop(NewState#state{timer = poll_timer(next_poll_type(How))});
	
	{req_timeout, ReqId} ->
	    NewState = handle_req_timeout(State, ReqId),
	    loop(NewState);

	{snmp_callback, Info} ->
	    NewState = handle_snmp(State, Info),
	    loop(NewState)
    end.


handle_poll_timeout(#state{agent = Agent, reqs = [Req|Reqs], ids = IDs} = S, 
	       first) ->
    ReqId = handle_req(Agent, [Req]),
    S#state{reqs = Reqs ++ [Req], ids = [ReqId|IDs]};
handle_poll_timeout(#state{agent = Agent, reqs = Reqs, ids = IDs} = S, all) ->
    ReqId = handle_req(Agent, Reqs),
    S#state{ids = [ReqId|IDs]}.

handle_req(#agent{host = Host, port = Port}, Reqs) ->
    Oids  = [Oid  || {_Desc, Op, Oid} <- Reqs, Op == get],
    Descs = [Desc || {Desc, Op, _Oid} <- Reqs, Op == get],
    {ok, ReqId} = snmpm:ag(?USER_ID, Host, Port, Oids),
    p("issued get-request (~w) for: ~s", [ReqId, oid_descs(Descs)]),
    ReqTimer = erlang:send_after(?REQ_TIMEOUT, self(), {req_timeout, ReqId}),
    {ReqId, erlang:monotonic_time(micro_seconds), ReqTimer}.

oid_descs([]) ->
    [];
oid_descs([Desc]) ->
    lists:flatten(io_lib:format("~s", [Desc]));
oid_descs([Desc|Descs]) ->
    lists:flatten(io_lib:format("~s, ", [Desc])) ++ oid_descs(Descs).

handle_req_timeout(#state{ids = IDs0} = State, ReqId) ->
    case lists:keysearch(ReqId, 1, IDs0) of
	{value, {ReqId, _T, _Ref}} ->
	    e("Request timeout for request ~w", [ReqId]),
	    IDs = lists:keydelete(ReqId, 1, IDs0),
	    State#state{ids = IDs};
	false ->
	    w("Did not find request corresponding to id ~w", [ReqId]),
	    State
    end.
    
handle_snmp(#state{ids = IDs0} = S, {error, ReqId, Reason}) ->
    case lists:keysearch(ReqId, 1, IDs0) of
	{value, {ReqId, T, Ref}} ->
	    Diff = erlang:monotonic_time(micro_seconds) - T,
	    p("SNMP error regarding outstanding request after ~w microsec:"
	      "~n   ReqId:  ~w"
	      "~n   Reason: ~w", [Diff, ReqId, Reason]),
	    IDs = lists:keydelete(ReqId, 1, IDs0),
	    erlang:cancel_timer(Ref),
	    S#state{ids = IDs};
	false ->
	    w("SNMP error regarding unknown request:"
	      "~n   ReqId:  ~w"
	      "~n   Reason: ~w", [ReqId, Reason]),
	    S
    end;

handle_snmp(State, {agent, Addr, Port, SnmpInfo}) ->
    p("Received agent info:"
      "~n   Addr:     ~w"
      "~n   Port:     ~w"
      "~n   SnmpInfo: ~w", [Addr, Port, SnmpInfo]),
    State;

handle_snmp(#state{ids = IDs0} = S, {pdu, Addr, Port, ReqId, SnmpResponse}) ->
    case lists:keysearch(ReqId, 1, IDs0) of
	{value, {ReqId, T, Ref}} ->
	    Diff = erlang:monotonic_time(micro_seconds) - T,
	    p("SNMP pdu regarding outstanding request after ~w microsec:"
	      "~n   ReqId:        ~w"
	      "~n   Addr:         ~w"
	      "~n   Port:         ~w"
	      "~n   SnmpResponse: ~w", 
	      [Diff, ReqId, Addr, Port, SnmpResponse]),
	    IDs = lists:keydelete(ReqId, 1, IDs0),
	    erlang:cancel_timer(Ref),
	    S#state{ids = IDs};
	false ->
	    w("SNMP pdu regarding unknown request:"
	      "~n   ReqId:        ~w"
	      "~n   Addr:         ~w"
	      "~n   Port:         ~w"
	      "~n   SnmpResponse: ~w", [ReqId, Addr, Port, SnmpResponse]),
	    S
    end;

handle_snmp(State, {trap, Addr, Port, SnmpTrapInfo}) ->
    p("Received trap:"
      "~n   Addr:         ~w"
      "~n   Port:         ~w"
      "~n   SnmpTrapInfo: ~w", [Addr, Port, SnmpTrapInfo]),
    State;

handle_snmp(State, {inform, Addr, Port, SnmpInform}) ->
    p("Received inform:"
      "~n   Addr:       ~w"
      "~n   Port:       ~w"
      "~n   SnmpInform: ~w", [Addr, Port, SnmpInform]),
    State;

handle_snmp(State, {report, Addr, Port, SnmpReport}) ->
    p("Received report:"
      "~n   Addr:       ~w"
      "~n   Port:       ~w"
      "~n   SnmpReport: ~w", [Addr, Port, SnmpReport]),
    State;

handle_snmp(State, Unknown) ->
    p("Received unknown snmp info:"
      "~n   Unknown: ~w", [Unknown]),
    State.


%% -----------------------------------------------------------------------
%% 
%% Manager user callback API
%% 
%% -----------------------------------------------------------------------


handle_error(ReqId, Reason, Pid) ->
    Pid ! {snmp_callback, {error, ReqId, Reason}},
    ignore.

handle_agent(Addr, Port, SnmpInfo, Pid) ->
    Pid ! {snmp_callback, {agent, Addr, Port, SnmpInfo}},
    ignore.

handle_pdu(Addr, Port, ReqId, SnmpResponse, Pid) ->
    Pid ! {snmp_callback, {pdu, Addr, Port, ReqId, SnmpResponse}},
    ignore.

handle_trap(Addr, Port, SnmpTrapInfo, Pid) ->
    Pid ! {snmp_callback, {trap, Addr, Port, SnmpTrapInfo}},
    ignore.

handle_inform(Addr, Port, SnmpInform, Pid) ->
    Pid ! {snmp_callback, {inform, Addr, Port, SnmpInform}},
    ignore.

handle_report(Addr, Port, SnmpReport, Pid) ->
    Pid ! {snmp_callback, {report, Addr, Port, SnmpReport}},
    ignore.


%% -----------------------------------------------------------------------

e(F, A) ->
    p("*** ERROR ***", F, A).

w(F, A) ->
    p("*** WARNING ***", F, A).

p(F, A) ->
    p("*** INFO ***", F, A).

p(P, F, A) ->
    io:format("~s~nMGR: " ++ F ++ "~n~n", [P|A]).
