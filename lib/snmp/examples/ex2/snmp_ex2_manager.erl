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
%%----------------------------------------------------------------------
%% This module examplifies how to write test suites for your SNMP agent.
%%----------------------------------------------------------------------

-module(snmp_ex2_manager).

-behaviour(gen_server).
-behaviour(snmpm_user).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([start_link/0, start_link/1, stop/0,
	 agent/2, 
         sync_get/2, 
         sync_get_next/2, 
         sync_get_bulk/4, 
         sync_set/2, 

	 oid_to_name/1
	]).

%% Manager callback API:
-export([handle_error/3,
         handle_agent/5,
         handle_pdu/4,
         handle_trap/3,
         handle_inform/3,
         handle_report/3, 
	 handle_invalid_result/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include_lib("snmp/include/snmp_types.hrl").


-define(SERVER,   ?MODULE).
-define(USER,     ?MODULE).
-define(USER_MOD, ?MODULE).

-record(state, {parent}).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).

stop() ->
    cast(stop).


%% --- Instruct manager to handle an agent ---

agent(TargetName, Conf) ->
    call({agent, TargetName, Conf}).


%% --- Various SNMP operations ----

sync_get(TargetName, Oids) ->
    call({sync_get, TargetName, Oids}).

sync_get_next(TargetName, Oids) ->
    call({sync_get_next, TargetName, Oids}).

sync_get_bulk(TargetName, NR, MR, Oids) ->
    call({sync_get_bulk, TargetName, NR, MR, Oids}).

sync_set(TargetName, VarsAndVals) ->
    call({sync_set, TargetName, VarsAndVals}).


%% --- Misc utility functions ---

oid_to_name(Oid) ->
    call({oid_to_name, Oid}).


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

init([Parent, Opts]) ->
    process_flag(trap_exit, true),
    case (catch do_init(Opts)) of
        {ok, State} ->
            {ok, State#state{parent = Parent}};
        {error, Reason} ->
            {stop, Reason};
	Crap ->
	    {stop, Crap}
    end.

do_init(Opts) ->
    {Dir, MgrConf, MgrOpts} = parse_opts(Opts),
    write_config(Dir, MgrConf),
    start_manager(MgrOpts),
    register_user(),
    {ok, #state{}}.

write_config(Dir, Conf) ->
    case snmp_config:write_manager_config(Dir, "", Conf) of
	ok ->
	    ok;
	Error ->
	    error({failed_writing_config, Error})
    end.

start_manager(Opts) ->
    case snmpm:start_link(Opts) of
	ok ->
	    ok; 
	Error ->
	    error({failed_starting_manager, Error})
    end.

register_user() ->
    case snmpm:register_user(?USER, ?USER_MOD, self()) of
	ok ->
	    ok;
	Error ->
	    error({failed_register_user, Error})
    end.

parse_opts(Opts) ->
    Port     = get_opt(port,             Opts, 5000),
    EngineId = get_opt(engine_id,        Opts, "mgrEngine"),
    MMS      = get_opt(max_message_size, Opts, 484),

    MgrConf = [{port,             Port},
               {engine_id,        EngineId},
               {max_message_size, MMS}],

    %% Manager options
    Mibs      = get_opt(mibs,     Opts, []),
    Vsns      = get_opt(versions, Opts, [v1, v2, v3]),
    {ok, Cwd} = file:get_cwd(),
    Dir       = get_opt(dir, Opts, Cwd),
    MgrOpts   = [{mibs,     Mibs},
		 {versions, Vsns}, 
		 %% {server,   [{verbosity, trace}]}, 
		 {config,   [% {verbosity, trace}, 
			     {dir, Dir}, {db_dir, Dir}]}],
    
    {Dir, MgrConf, MgrOpts}.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call({agent, TargetName, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(?USER, TargetName, Conf)),
    {reply, Reply, S};

handle_call({oid_to_name, Oid}, _From, S) ->
    Reply = (catch snmpm:oid_to_name(Oid)),
    {reply, Reply, S};

handle_call({sync_get, TargetName, Oids}, _From, S) ->
    Reply = (catch snmpm:sync_get(?USER, TargetName, Oids)),
    {reply, Reply, S};

handle_call({sync_get_next, TargetName, Oids}, _From, S) ->
    Reply = (catch snmpm:sync_get_next(?USER, TargetName, Oids)),
    {reply, Reply, S};

handle_call({sync_get_bulk, TargetName, NR, MR, Oids}, _From, S) ->
    Reply = (catch snmpm:sync_get_bulk(?USER, TargetName, NR, MR, Oids)),
    {reply, Reply, S};

handle_call({sync_set, TargetName, VarsAndVals}, _From, S) ->
    Reply = (catch snmpm:sync_set(?USER, TargetName, VarsAndVals)),
    {reply, Reply, S};

handle_call(Req, From, State) ->
    error_msg("received unknown request ~n~p~nFrom ~p", [Req, From]),
    {reply, {error, {unknown_request, Req}}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, S) ->
    (catch snmpm:stop()),
    {stop, normal, S};

handle_cast(Msg, State) ->
    error_msg("received unknown message ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({snmp_callback, Tag, Info}, State) ->
    handle_snmp_callback(Tag, Info),
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


%% ========================================================================
%% ========================================================================

handle_snmp_callback(handle_error, {ReqId, Reason}) ->
    io:format("*** FAILURE ***"
	      "~n   Request Id: ~p"
	      "~n   Reason:     ~p"
	      "~n", [ReqId, Reason]),
    ok;
handle_snmp_callback(handle_agent, {Addr, Port, Type, SnmpInfo}) ->
    {ES, EI, VBs} = SnmpInfo, 
    io:format("*** UNKNOWN AGENT ***"
	      "~n   Address:   ~p"
	      "~n   Port:      ~p"
	      "~n   Type:      ~p"
	      "~n   SNMP Info: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, Type, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_pdu, {TargetName, ReqId, SnmpResponse}) ->
    {ES, EI, VBs} = SnmpResponse, 
    io:format("*** Received PDU ***"
	      "~n   TargetName: ~p"
	      "~n   Request Id: ~p"
	      "~n   SNMP response:"
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [TargetName, ReqId, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_trap, {TargetName, SnmpTrap}) ->
    TrapStr = 
	case SnmpTrap of
	    {Enteprise, Generic, Spec, Timestamp, Varbinds} ->
		io_lib:format("~n     Generic:    ~w"
			      "~n     Exterprise: ~w"
			      "~n     Specific:   ~w"
			      "~n     Timestamp:  ~w"
			      "~n     Varbinds:   ~p", 
			      [Generic, Enteprise, Spec, Timestamp, Varbinds]);
	    {ErrorStatus, ErrorIndex, Varbinds} ->
		io_lib:format("~n     Error Status: ~w"
			      "~n     Error Index:  ~w"
			      "~n     Varbinds:     ~p"
			      "~n", [ErrorStatus, ErrorIndex, Varbinds])
	end,
    io:format("*** Received TRAP ***"
	      "~n   TargetName: ~p"
	      "~n   SNMP trap:  ~s"
	      "~n", [TargetName, lists:flatten(TrapStr)]),
    ok;
handle_snmp_callback(handle_inform, {TargetName, SnmpInform}) ->
    {ES, EI, VBs} = SnmpInform, 
    io:format("*** Received INFORM ***"
	      "~n   TargetName: ~p"
	      "~n   SNMP inform: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [TargetName, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_report, {TargetName, SnmpReport}) ->
    {ES, EI, VBs} = SnmpReport, 
    io:format("*** Received REPORT ***"
	      "~n   TargetName: ~p"
	      "~n   SNMP report: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [TargetName, ES, EI, VBs]),
    ok;
handle_snmp_callback(BadTag, Crap) ->
    io:format("*** Received crap ***"
	      "~n   ~p"
	      "~n   ~p"
	      "~n", [BadTag, Crap]),
    ok.
    


error(Reason) ->
    throw({error, Reason}).


error_msg(F, A) ->
    catch error_logger:error_msg("*** TEST-MANAGER: " ++ F ++ "~n", A).


call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).


%% ========================================================================
%% Misc internal utility functions
%% ========================================================================

%% get_opt(Key, Opts) ->
%%     case lists:keysearch(Key, 1, Opts) of
%%         {value, {Key, Val}} ->
%%             Val;
%%         false ->
%%             throw({error, {missing_mandatory, Key}})
%%     end.

get_opt(Key, Opts, Def) ->
    case lists:keysearch(Key, 1, Opts) of
        {value, {Key, Val}} ->
            Val;
        false ->
            Def
    end.


%% ========================================================================
%% SNMPM user callback functions
%% ========================================================================

handle_error(ReqId, Reason, Server) when is_pid(Server) ->
    report_callback(Server, handle_error, {ReqId, Reason}),
    ignore.


handle_agent(Addr, Port, Type, SnmpInfo, Server) when is_pid(Server) ->
    report_callback(Server, handle_agent, {Addr, Port, Type, SnmpInfo}),
    ignore.


handle_pdu(TargetName, ReqId, SnmpResponse, Server) when is_pid(Server) ->
    report_callback(Server, handle_pdu, {TargetName, ReqId, SnmpResponse}),
    ignore.


handle_trap(TargetName, SnmpTrap, Server) when is_pid(Server) ->
    report_callback(Server, handle_trap, {TargetName, SnmpTrap}),
    ok.

handle_inform(TargetName, SnmpInform, Server) when is_pid(Server) ->
    report_callback(Server, handle_inform, {TargetName, SnmpInform}),
    ok.


handle_report(TargetName, SnmpReport, Server) when is_pid(Server) ->
    report_callback(Server, handle_inform, {TargetName, SnmpReport}),
    ok.

handle_invalid_result(In, Out, Server) when is_pid(Server) ->
    report_callback(Server, handle_invalid_result, {In, Out}),
    ok.

report_callback(Pid, Tag, Info) ->
    Pid ! {snmp_callback, Tag, Info}.
