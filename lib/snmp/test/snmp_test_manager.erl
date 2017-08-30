%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%% This module implements an SNMP manager used in the test suite
%%----------------------------------------------------------------------
%%

-module(snmp_test_manager).

-behaviour(gen_server).
-behaviour(snmpm_user).


%% External exports
-export([
	 start_link/0, start_link/1,
	 stop/0, 

	 sync_get/1,      sync_get/2, 
	 sync_get_next/1, sync_get_next/2, 
	 sync_get_bulk/3, 
	 sync_set/1,      sync_set/2
	]).


%% Manager callback API:
-export([
	 handle_error/3,
         handle_agent/5,
         handle_pdu/4,
         handle_trap/3,
         handle_inform/3,
         handle_report/3, 
	 handle_invalid_result/3
	]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-record(state, {parent, req, agent_target_name}).

-define(SERVER, ?MODULE).
-define(USER,   ?MODULE).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    start_link([]).

start_link(Opts) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).

stop() ->
    call(stop).


sync_get(Oids) ->
    sync_get(Oids, fun(X) -> {ok, X} end).

sync_get(Oids, Verify) when is_list(Oids) and is_function(Verify) ->
    Verify(call({sync_get, Oids})).


sync_get_next(Oids) ->
    sync_get_next(Oids, fun(X) -> {ok, X} end).

sync_get_next(Oids, Verify) when is_list(Oids) and is_function(Verify) ->
    Verify(call({sync_get_next, Oids})).


sync_get_bulk(NR, MR, Oids) ->
    sync_get_bulk(NR, MR, Oids, fun(X) -> {ok, X} end).

sync_get_bulk(NR, MR, Oids, Verify) 
  when is_integer(NR) and is_integer(MR) and 
       is_list(Oids) and is_function(Verify) ->
    Verify(call({sync_get_bulk, NR, MR, Oids})).


sync_set(VarsAndVals) ->
    sync_set(VarsAndVals, fun(X) -> {ok, X} end).

sync_set(VarsAndVals, Verify) 
  when is_list(VarsAndVals) and is_function(Verify) ->
    Verify(call({sync_set, VarsAndVals})).


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Parent, Opts]) ->
    process_flag(trap_exit, true),
    case (catch do_init(Opts)) of
	{ok, State} ->
	    {ok, State#state{parent = Parent}};
	{error, Reason} ->
	    {stop, Reason}
    end.

do_init(Opts) ->
    {MgrDir, MgrConf, MgrOpts, AgentTargetName, AgentConf} = parse_opts(Opts),
    ok = snmp_config:write_manager_config(MgrDir, "", MgrConf),
    ok = snmpm:start_link(MgrOpts),
    ok = snmpm:register_user(?USER, ?MODULE, self()),
    ok = snmpm:register_agent(?USER, AgentTargetName, AgentConf),
    {ok, #state{agent_target_name = AgentTargetName}}.


parse_opts(Opts) ->
    %% Manager config (written to the manager.conf file)
    %% Addr     = get_opt(addr,      Opts, ?HOSTNAME()),
    Port     = get_opt(port,      Opts, 5000),
    EngineId = get_opt(engine_id, Opts, "mgrEngine"),
    MMS      = get_opt(max_message_size, Opts, 484),

    MgrConf = [%% {address,          Addr},
	       {port,             Port},
	       {engine_id,        EngineId},
	       {max_message_size, MMS}],

    
    %% Manager options
    MgrOpts = get_opt(options, Opts),
    MgrDir  = get_opt(dir, get_opt(config,  MgrOpts, [])),
    
    
    %% Retreive the agent configuration
    AgentConf   = get_opt(agent_config, Opts),
    AgentTarget = get_opt(agent_target, Opts),
    {MgrDir, MgrConf, MgrOpts, AgentTarget, AgentConf}.


get_opt(Key, Opts) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    throw({error, {missing_mandatory, Key}})
    end.

get_opt(Key, Opts, Def) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    Def
    end.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(stop, _From, S) ->
    (catch snmpm:stop()),
    {stop, normal, S};

handle_call({sync_get, Oids}, _From, 
	    #state{agent_target_name = TargetName} = S) ->
    Reply = (catch snmpm:sync_get(?USER, TargetName, Oids)),
    {reply, Reply, S};

handle_call({sync_get_next, Oids}, _From, 
	    #state{agent_target_name = TargetName} = S) ->
    Reply = (catch snmpm:sync_get_next(?USER, TargetName, Oids)),
    {reply, Reply, S};

handle_call({sync_get_bulk, NR, MR, Oids}, _From, 
	    #state{agent_target_name = TargetName} = S) ->
    Reply = (catch snmpm:sync_get_bulk(?USER, TargetName, NR, MR, Oids)),
    {reply, Reply, S};

handle_call({sync_set, VarsAndVals}, _From, 
	    #state{agent_target_name = TargetName} = S) ->
    Reply = (catch snmpm:sync_set(?USER, TargetName, VarsAndVals)),
    {reply, Reply, S};

handle_call(Req, From, State) ->
    error_msg("received unknown request ~n~p~nFrom ~p", [Req, From]),
    {reply, {error, unknown_request}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_msg("received unknown message ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({snmp_error, ReqId, Reason}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp error: "
	      "~n   ReqId:  ~w"
	      "~n   Reason: ~p", [ReqId, Reason]),
    P ! {snmp_error, ReqId, Reason}, 
    {noreply, State};

handle_info({snmp_agent, Addr, Port, Info, Pid, _UserData}, 
	    #state{parent = P} = State) ->
    error_msg("detected new agent: "
	      "~n   Addr: ~w"
	      "~n   Port: ~p"
	      "~n   Info: ~p", [Addr, Port, Info]),
    Pid ! {snmp_agent_reply, ignore, self()},
    P ! {snmp_agent, Addr, Port, Info},
    {noreply, State};

handle_info({snmp_pdu, TargetName, ReqId, Resp}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp pdu: "
	      "~n   TargetName: ~p"
	      "~n   ReqId:      ~w"
	      "~n   Resp:       ~p", [TargetName, ReqId, Resp]),
    P ! {snmp_pdu, TargetName, ReqId, Resp}, 
    {noreply, State};

handle_info({snmp_trap, TargetName, Info, Pid}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp trap: "
	      "~n   TargetName: ~p"
	      "~n   Info:       ~p", [TargetName, Info]),
    Pid ! {snmp_trap_reply, ignore, self()},
    P ! {snmp_trap, TargetName, Info}, 
    {noreply, State};

handle_info({snmp_inform, TargetName, Info, Pid}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp inform: "
	      "~n   TargetName: ~p"
	      "~n   Info:       ~p", [TargetName, Info]),
    Pid ! {snmp_inform_reply, ignore, self()},
    P ! {snmp_inform, TargetName, Info}, 
    {noreply, State};

handle_info({snmp_report, TargetName, Info, Pid}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp report: "
	     "~n   TargetName: ~p"
	     "~n   Info:       ~p", [TargetName, Info]),
    Pid ! {snmp_report_reply, ignore, self()},
    P ! {snmp_report, TargetName, Info}, 
    {noreply, State};

handle_info({snmp_invalid_result, In, Out}, State) ->
    error_msg("Callback failure: "
	      "~n   In:  ~p"
	      "~n   Out: ~p", [In, Out]),
    {noreply, State};

handle_info(Info, State) ->
    error_msg("received unknown info: "
	      "~n   Info: ~p", [Info]),
    {noreply, State}.
    

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};
  
% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------




%% --------------------------------------------------------------------------
%% 
%%                   SNMP manager callback functions 	   
%% 
%% --------------------------------------------------------------------------

handle_error(ReqId, Reason, Pid) ->
    Pid ! {snmp_error, ReqId, Reason},
    ignore.


handle_agent(Addr, Port, SnmpInfo, Pid, UserData) ->
    Pid ! {snmp_agent, Addr, Port, SnmpInfo, self(), UserData},
    receive
	{snmp_agent_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.


handle_pdu(TargetName, ReqId, SnmpResponse, Pid) ->
    Pid ! {snmp_pdu, TargetName, ReqId, SnmpResponse},
    ignore.


handle_trap(TargetName, SnmpTrapInfo, Pid) ->
    Pid ! {snmp_trap, TargetName, SnmpTrapInfo, self()},
    receive
	{snmp_trap_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.


handle_inform(TargetName, SnmpInfo, Pid) ->
    Pid ! {snmp_inform, TargetName, SnmpInfo, self()},
    receive
	{snmp_inform_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.


handle_report(TargetName, SnmpInfo, Pid) ->
    Pid ! {snmp_report, TargetName, SnmpInfo, self()},
    receive
	{snmp_report_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.


handle_invalid_result(In, Out, Pid) ->
    Pid ! {snmp_invalid_result, In, Out},
    ignore.


%%----------------------------------------------------------------------
         
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
 
% cast(Msg) ->
%     gen_server:cast(?SERVER, Msg).
 
info_msg(F, A) ->
    catch error_logger:info_msg("*** TEST-MANAGER: " ++ F ++ "~n", A).
  
error_msg(F, A) ->
    catch error_logger:error_msg("*** TEST-MANAGER: " ++ F ++ "~n", A).
  
 
