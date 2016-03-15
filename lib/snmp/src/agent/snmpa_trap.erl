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
-module(snmpa_trap).

%%%-----------------------------------------------------------------
%%% This module takes care of all trap(notification handling.
%%%-----------------------------------------------------------------
%% External exports
-export([construct_trap/2, 
	 try_initialise_vars/2, 
	 send_trap/6, send_trap/7, send_trap/8]).
-export([send_discovery/6]).

%% Internal exports
-export([init_v2_inform/9, init_v2_inform/10, 
	 init_v3_inform/9, init_v3_inform/10, init_v3_inform/11, 
	 send_inform/6]).
-export([init_discovery_inform/13, send_discovery_inform/5]).

%% <BACKWARD-COMPAT>
-export([send_discovery/5, 
	 init_discovery_inform/12]).
%% </BACKWARD-COMPAT>

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/src/agent/snmpa_internal.hrl").
-include_lib("snmp/include/SNMPv2-MIB.hrl").
-include_lib("snmp/include/SNMPv2-TM.hrl").
-include_lib("snmp/include/SNMPv2-TC.hrl").
-include_lib("snmp/include/SNMP-FRAMEWORK-MIB.hrl").
-include_lib("snmp/include/SNMP-TARGET-MIB.hrl").
-include_lib("snmp/include/TRANSPORT-ADDRESS-MIB.hrl").
-define(enterpriseSpecific, 6).


-define(VMODULE,"TRAP").
-include("snmp_verbosity.hrl").
-include("snmpa_internal.hrl").


%%-----------------------------------------------------------------
%% Trap mechanism
%% ==============
%% Distributed subagent (dSA) case
%%   The MIB with the TRAP-TYPE macro is loaded in dSA.  This means
%%   that dSA has info on all variables defined in the TRAP-TYPE,
%%   even though some variables may be located in other SA:s (or
%%   in the MA). Other variables that may be sent in the trap, 
%%   must be known by either the dSA, or some of its parent agents
%%   (e.g. the master agent), if the variable should be referred
%%   to by symbolic name. It is however possible to send other
%%   variables as well, but then the entire OID must be provided.
%%   The dSA locates the asn1 type, oid and value for as many
%%   variables as possible. This information, together with the
%%   variables for which the type, value or oid isn't known, is
%%   sent to the dSA's parent. This agent performs the same
%%   operation, and so on, until eventually the MA will receive the
%%   info. The MA then fills in the gaps, and at this point all
%%   oids and types must be known, otherwise an error is signalled,
%%   and the opertaion is aborted. For the unknown values for some 
%%   oids, a get-operation is performed by the MA. This will
%%   retreive the missing values.
%%   At this point, all oid, types and values are known, so the MA
%%   can distribute the traps according to the information in the
%%   internal tables.
%% 
%% Local subagent (lSA) case
%%   This case is similar to the case above.
%%
%% Master agent (MA) case
%%   This case is similar to the case above.
%%
%% NOTE: All trap forwarding between agents is made asynchronously.
%%
%% dSA: Distributed SA  (the #trap is loaded here)
%% nSA: [many] SAs between dSA and MA
%% MA:  Master Agent. (all trap info (destiniations is here))
%% 1) application decides to send a trap.
%% 2) dSA calls send_trap which initialises vars
%% 3) dSA sends all to nSA
%% 4) nSA tries to map symbolic names to oids and find the types
%%    of all variableoids with a value (and no type).
%% 5) nSA sends all to (n-1)SA
%% 6) MA tries to initialise vars
%% 7) MA makes a trappdu, and sends it to all destination.
%%
%% Problems with this implementation
%% =================================
%% It's ok to send {Oid, Value} but not just Oid. (it should be for
%%   any Oid)
%% It's ok to send {Name, Value} but not just Name. (it should be
%%   for Names in the hierarchy)
%% This approach might be too flexible; will people use it?
%% *NOTE*
%% Therefore, in this version we *do not* allow extra variables
%% in traps.
%% *YES* In _this_ version we do.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: construct_trap/2
%% Args: Trap is an atom
%%       Varbinds is a list of 
%%               {Variable, Value} | {SymbolicTableCol, RowIndex, Value} 
%%         where Variable is an atom or an OID, 
%%         where RowIndex is the indexes for the row.
%%         We don't check the RowIndex.
%% Purpose: This is the initially-called function. It is called
%%          by the agent that found out that a trap should be
%%          sent.
%%          Initialize as many variables as possible.
%% Returns: {ok, TrapRecord, <list of Var>} | error
%%          where Var is returned from initiate_vars.
%% NOTE: Executed at the inital SA
%%-----------------------------------------------------------------
construct_trap(Trap, Varbinds) ->
    ?vdebug("construct_trap -> entry with"
	"~n   Trap: ~p", [Trap]),
    case snmpa_symbolic_store:get_notification(Trap) of
	undefined -> 
	    user_err("construct_trap got undef Trap: ~w" , [Trap]),
	    error;

	{value, #trap{oidobjects = ListOfVars} = TRec} ->
	    ?vdebug("construct_trap -> trap"
		    "~n   ~p", [TRec]),
	    OidVbs        = [alias_to_oid(Vb) || Vb <- Varbinds],
	    LV            = initiate_vars(ListOfVars, OidVbs),
	    InitiatedVars = try_initialise_vars(get(mibserver), LV),
	    {ok, TRec, InitiatedVars};

	{value, #notification{oidobjects = ListOfVars} = NRec} ->
	    ?vdebug("construct_trap -> notification"
		    "~n   ~p", [NRec]),
	    OidVbs        = [alias_to_oid(Vb) || Vb <- Varbinds],
	    LV            = initiate_vars(ListOfVars, OidVbs),
	    InitiatedVars = try_initialise_vars(get(mibserver), LV), 
	    {ok, NRec, InitiatedVars}
    end.

alias_to_oid({Alias, Val}) when is_atom(Alias) ->
    case snmpa_symbolic_store:aliasname_to_oid(Alias) of
	{value, Oid} -> {lists:append(Oid, [0]), {value, Val}};
	_ ->   	     {Alias, {value, Val}}
    end;
alias_to_oid({Alias, RowIndex, Val}) when is_atom(Alias) ->
    case snmpa_symbolic_store:aliasname_to_oid(Alias) of
	{value, Oid} -> {lists:append(Oid, RowIndex), {value, Val}};
	_ ->   	     {Alias, RowIndex, {value, Val}}
    end;
alias_to_oid({Oid, Val}) -> {Oid, {value, Val}}.


%%-----------------------------------------------------------------
%% Func: initiate_vars/2
%% Args: ListOfVars is a list of {Oid, #asn1_type}
%%       Varbinds is a list of 
%%          {VariableOid, Value} | 
%%          {VariableAtom, Value} |
%%          {TableColAtom, RowIndex, Value}
%% Purpose: For each variable specified in the TRAP-TYPE macro
%%          (each in ListOfVars), check if it's got a value given
%%          in the Varbinds list.
%%          For each Oid:
%%            1) It has corresponding VariableOid. Use Value.
%%            2) No corresponding VariableOid. No value.
%% Returns: A list of
%%            {VariableOid, #asn1_type, Value} |
%%            {VariableOid, #asn1_type} |
%%            {VariableOid, Value} |
%%            {VariableAtom, Value} |
%%            {TableColAtom, RowIndex, Value}
%% NOTE: Executed at the inital SA
%%-----------------------------------------------------------------
initiate_vars([{Oid, Asn1Type} | T], Varbinds) ->
    case delete_oid_from_varbinds(Oid, Varbinds) of
	{undefined, _, _} ->
	    [{Oid, Asn1Type} | initiate_vars(T, Varbinds)];
	{Value, VarOid, RestOfVarbinds} ->
	    [{VarOid, Asn1Type, Value} | initiate_vars(T, RestOfVarbinds)]
    end;
initiate_vars([], Varbinds) ->
    Varbinds.
    
delete_oid_from_varbinds(Oid, [{VarOid, Value} | T]) ->
    case lists:prefix(Oid, VarOid) of
	true -> 
	    {Value, VarOid, T};
	_ -> 
	    {Value2, VarOid2, T2} = delete_oid_from_varbinds(Oid, T),
	    {Value2, VarOid2, [{VarOid, Value} | T2]}
    end;
delete_oid_from_varbinds(Oid, [H | T]) ->
    {Value, VarOid, T2} = delete_oid_from_varbinds(Oid, T),
    {Value, VarOid, [H | T2]};
delete_oid_from_varbinds(_Oid, []) -> {undefined, undefined, []}.

%%-----------------------------------------------------------------
%% Func: try_initialise_vars(Mib, Varbinds)
%% Args: Mib is the local mib process
%%       Varbinds is a list returned from initiate_vars.
%% Purpose: Try to initialise uninitialised vars.
%% Returns: see initiate_vars
%% NOTE: Executed at the intermediate SAs
%%-----------------------------------------------------------------
try_initialise_vars(Mib, Varbinds) ->
    V = try_map_symbolic(Varbinds),
    try_find_type(V, Mib).

%%-----------------------------------------------------------------
%% Func: try_map_symbolic/1
%% Args: Varbinds is a list returned from initiate_vars.
%% Purpose: Try to map symbolic name to oid for the 
%%          symbolic names left in the Varbinds list.
%% Returns: see initiate_vars.
%% NOTE: Executed at the intermediate SAs
%%-----------------------------------------------------------------
try_map_symbolic([Varbind | Varbinds]) ->
    [localise_oid(Varbind) | try_map_symbolic(Varbinds)];
try_map_symbolic([]) -> [].

localise_oid({VariableName, Value}) when is_atom(VariableName) ->
    alias_to_oid({VariableName, Value});
localise_oid({VariableName, RowIndex, Value}) when is_atom(VariableName) ->
    alias_to_oid({VariableName, RowIndex, Value});
localise_oid(X) -> X.

%%-----------------------------------------------------------------
%% Func: try_find_type/2
%% Args: Varbinds is a list returned from initiate_vars.
%%       Mib is a ref to the Mib process corresponding to
%%         this agent.
%% Purpose: Try to find the type for each variableoid with a value
%%          but no type.
%% Returns: see initiate_vars.
%% NOTE: Executed at the intermediate SAs
%%-----------------------------------------------------------------
try_find_type([Varbind | Varbinds], Mib) ->
    [localise_type(Varbind, Mib) | try_find_type(Varbinds, Mib)];
try_find_type([], _) -> [].

localise_type({VariableOid, Type}, _Mib) 
  when is_list(VariableOid) andalso is_record(Type, asn1_type) ->
    {VariableOid, Type};
localise_type({VariableOid, Value}, Mib) when is_list(VariableOid) ->
    case snmpa_mib:lookup(Mib, VariableOid) of
	{variable, ME} ->
	    {VariableOid, ME#me.asn1_type, Value};
	{table_column, ME, _} ->
	    {VariableOid, ME#me.asn1_type, Value};
	_ ->
	    {VariableOid, Value}
    end;
localise_type(X, _) -> X.

%%-----------------------------------------------------------------
%% Func: make_v1_trap_pdu/5
%% Args: Enterprise = oid()
%%       Specific = integer()
%%       Varbinds is as returned from initiate_vars
%%         (but only {Oid, Type[, Value} permitted)
%%       SysUpTime = integer()
%%       AgentIp = {A, B, C, D}
%% Purpose: Make a #trappdu
%%          Checks the Varbinds to see that no symbolic names are
%%          present, and that each var has a type. Performs a get
%%          to find any missing value.
%% Returns: {#trappdu, [byte()] | error
%% Fails: yes
%% NOTE: Executed at the MA
%%-----------------------------------------------------------------
make_v1_trap_pdu(Enterprise, Specific, VarbindList, SysUpTime, AgentIp) ->
    {Enterp,Generic,Spec} = 
	case Enterprise of
	    ?snmp ->
		{sys_object_id(),Specific,0};
	    _ ->
		{Enterprise,?enterpriseSpecific,Specific}
    end,
    #trappdu{enterprise    = Enterp,
	     agent_addr    = AgentIp,
	     generic_trap  = Generic,
	     specific_trap = Spec,
	     time_stamp    = SysUpTime,
	     varbinds      = VarbindList}.

make_discovery_pdu(Vbs) ->
    #pdu{type         = 'inform-request',
	 request_id   = snmpa_mpd:generate_req_id(),
	 error_status = noError,
	 error_index  = 0,
	 varbinds     = Vbs}.

make_v2_notif_pdu(Vbs, Type) ->
    #pdu{type         = Type,
	 request_id   = snmpa_mpd:generate_req_id(),
	 error_status = noError,
	 error_index  = 0,
	 varbinds     = Vbs}.

make_varbind_list(Varbinds) ->
    {VariablesWithValueAndType, VariablesWithType} =
	split_variables( order(Varbinds) ),
    V    = get_values(VariablesWithType),
    Vars = lists:append([V, VariablesWithValueAndType]),
    [make_varbind(Var) || Var <- unorder(lists:keysort(1, Vars))].


%%-----------------------------------------------------------------
%% Func: send_trap/6
%% Args: TrapRec = #trap | #notification
%%       NotifyName = string()
%%       ContextName = string()
%%       Recv = no_receiver | {Ref, Receiver}
%%       Receiver = pid() | atom() | {M,F,A}
%%       Vbs = [varbind()]
%%       NetIf = pid()
%% Purpose: Default trap sending function.
%%          Sends the trap to the targets pointed out by NotifyName.
%%          If NotifyName is ""; the normal procedure defined in 
%%          SNMP-NOTIFICATION-MIB is used, i.e. the trap is sent to
%%          all managers.
%%          Otherwise, the NotifyName is used to find an entry in the
%%          SnmpNotifyTable which define how to send the notification
%%          (as an Inform or a Trap), and to select targets from
%%          SnmpTargetAddrTable (using the Tag).
%%-----------------------------------------------------------------
send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, NetIf) ->
    ExtraInfo     = ?DEFAULT_NOTIF_EXTRA_INFO, 
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
	      LocalEngineID, ExtraInfo, NetIf).

send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, ExtraInfo, NetIf) ->
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
	      LocalEngineID, ExtraInfo, NetIf).

%% The agent normally does not care about the result, 
%% but since it can be usefull when debugging, add 
%% some info when we fail to send the trap(s).
send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, LocalEngineID, 
	  ExtraInfo, NetIf) ->
    try 
	begin
	    do_send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
			 LocalEngineID, ExtraInfo, NetIf)
	end
    catch
	T:E ->
	    Info = [{args, [TrapRec, NotifyName, ContextName, 
			    Recv, Vbs, LocalEngineID, ExtraInfo, NetIf]},
		    {tag,  T},
		    {err,  E},
		    {stacktrace, erlang:get_stacktrace()}],
	    ?vlog("snmpa_trap:send_trap exception: ~p", [Info]),
	    {error, {failed_sending_trap, Info}}
    end.
     
do_send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
	     LocalEngineID, ExtraInfo, NetIf) ->
    VarbindList = make_varbind_list(Vbs),
    Dests       = find_dests(NotifyName),
    send_trap_pdus(Dests, ContextName, {TrapRec, VarbindList}, [], [], [],
		   Recv, LocalEngineID, ExtraInfo, NetIf).

send_discovery(TargetName, Record, ContextName, Vbs, NetIf) ->
    ExtraInfo = ?DEFAULT_NOTIF_EXTRA_INFO, 
    send_discovery(TargetName, Record, ContextName, Vbs, NetIf, ExtraInfo).
send_discovery(TargetName, Record, ContextName, Vbs, NetIf, ExtraInfo) ->
    case find_dest(TargetName) of
	{ok, Dest} ->
	    send_discovery_pdu(Dest, Record, ContextName, Vbs, NetIf, 
			       ExtraInfo);
	Error ->
	    Error
    end.


get_values(VariablesWithType) ->
    {Order, Varbinds} = extract_order(VariablesWithType, 1),
    ?vtrace("get_values -> "
	    "~n   Order:    ~p"
	    "~n   Varbinds: ~p", [Order, Varbinds]),
    case snmpa_agent:do_get(snmpa_acm:get_root_mib_view(), Varbinds, true) of
	{noError, _, NewVarbinds} ->
	    ?vtrace("get_values -> values retrieved"
		    "~n   NewVarbinds: ~p", [NewVarbinds]),
	    %% NewVarbinds is the result of:
	    %% first a reverse, then a sort on the oid field and finally 
	    %% a reverse during the get-processing so we need to re-sort 
	    %% on the org_index field again before contract-order
 	    NewVarbinds1 = lists:keysort(#varbind.org_index, NewVarbinds),
 	    contract_order(Order, NewVarbinds1);
	{ErrorStatus, ErrorIndex, _} ->
	    user_err("snmpa_trap: get operation failed: ~w"
		     "~n    at ~w"
		     "~n    in ~w",
		     [ErrorStatus, ErrorIndex, Varbinds]),
	    throw(error)
    end.
    
make_varbind(Varbind) when is_record(Varbind, varbind) ->
    Varbind;
make_varbind({VarOid, ASN1Type, Value}) ->
    case snmpa_agent:make_value_a_correct_value(Value, ASN1Type, undef) of
	{value, Type, Val} ->
	    #varbind{oid = VarOid, variabletype = Type, value = Val};
	{error, Reason} -> 
	    user_err("snmpa_trap: Invalid value: ~w"
		     "~n   Oid:  ~w"
		     "~n   Val:  ~w"
		     "~n   Type: ~w",
		     [Reason, VarOid, Value, ASN1Type]),
	    throw(error)
    end.

order(Varbinds) -> 
    order(Varbinds, 1).

order([H | T], No) -> [{No, H} | order(T, No + 1)];
order([], _) -> [].

unorder([{_No, H} | T]) -> [H | unorder(T)];
unorder([]) -> [].

extract_order([{No, {VarOid, _Type}} | T], Index) ->
    {Order, V} = extract_order(T, Index+1),
    {[No | Order], [#varbind{oid = VarOid, org_index = Index} | V]};
extract_order([], _) -> {[], []}.

contract_order([No | Order], [Varbind | T]) ->
    [{No, Varbind} | contract_order(Order, T)];
contract_order([], []) -> 
    [].

split_variables([{No, {VarOid, Type, Val}} | T]) when is_list(VarOid) ->
    {A, B} = split_variables(T),
    {[{No, {VarOid, Type, Val}} | A], B};
split_variables([{No, {VarOid, Type}} | T]) 
  when is_list(VarOid) andalso is_record(Type, asn1_type) ->
    {A, B} = split_variables(T),
    {A, [{No, {VarOid, Type}} | B]};
split_variables([{_No, {VarName, Value}} | _T]) ->
    user_err("snmpa_trap: Undefined variable ~w (~w)", [VarName, Value]),
    throw(error);
split_variables([{_No, {VarName, RowIndex, Value}} | _T]) ->
    user_err("snmpa_trap: Undefined variable ~w ~w (~w)",
	     [VarName, RowIndex, Value]),
    throw(error);
split_variables([]) -> {[], []}.


%%-----------------------------------------------------------------
%% Func: find_dests(NotifyName) -> 
%%          [{DestAddr, TargetName, TargetParams, NotifyType}]
%% Types: NotifyType = string()
%%        DestAddr = {TDomain, TAddr}
%%        TargetName = string()
%%        TargetParams = {MpModel, SecModel, SecName, SecLevel}
%%        NotifyType = trap | {inform, Timeout, Retry}
%% Returns: A list of all Destination addresses for this community.
%% NOTE: This function is executed in the master agent's context
%%-----------------------------------------------------------------
find_dests("") ->
    ?vtrace("find destinations", []),
    snmp_notification_mib:get_targets();
find_dests(NotifyName) ->
    ?vtrace("find destinations for ~p", [NotifyName]),
    case snmp_notification_mib:get_targets(NotifyName) of
	[] ->
	    ?vlog("No dests found for NotifyName: ~p", [NotifyName]),
	    [];
	Dests ->
	    Dests
    end.

find_dest(TargetName) ->
    AddrCols = [?snmpTargetAddrTDomain,
		?snmpTargetAddrTAddress,
		?snmpTargetAddrTimeout,
		?snmpTargetAddrRetryCount,
		?snmpTargetAddrParams,
		?snmpTargetAddrRowStatus],
    case snmp_target_mib:snmpTargetAddrTable(get, TargetName, AddrCols) of
	[{value, TDomain},
	 {value, TAddress},
	 {value, Timeout},
	 {value, RetryCount},
	 {value, Params},
	 {value, ?'RowStatus_active'}] ->
	    ?vtrace("find_dest -> found snmpTargetAddrTable info:"
		    "~n   TDomain:    ~p"
		    "~n   TAddress:   ~p"
		    "~n   Timeout:    ~p"
		    "~n   RetryCount: ~p"
		    "~n   Params:     ~p", 
		    [TDomain, TAddress, Timeout, RetryCount, Params]),
	    ParmCols = [?snmpTargetParamsMPModel,
			?snmpTargetParamsSecurityModel,
			?snmpTargetParamsSecurityName,
			?snmpTargetParamsSecurityLevel,
			?snmpTargetParamsRowStatus],
	    case snmp_target_mib:snmpTargetParamsTable(get, Params, ParmCols) of
		[{value, ?MP_V3}, 
		 {value, SecModel}, 
		 {value, SecName}, 
		 {value, SecLevel},
		 {value, ?'RowStatus_active'}] ->
		    ?vtrace("find_dest -> found snmpTargetParamsTable info:"
			    "~n   SecModel: ~p"
			    "~n   SecName:  ~p"
			    "~n   SecLevel: ~p", 
			    [SecModel, SecName, SecLevel]),
		    DestAddr     = {TDomain, TAddress},
		    TargetParams = {SecModel, SecName, SecLevel},
		    Val = {DestAddr, TargetName, TargetParams, Timeout, RetryCount},
		    {ok, Val};
		[{value, ?MP_V3}, 
		 {value, _SecModel}, 
		 {value, _SecName}, 
		 {value, _SecLevel},
		 {value, RowStatus}] ->
		    {error, {invalid_RowStatus, RowStatus, snmpTargetParamsTable}};
		[{value, MpModel}, 
		 {value, _SecModel}, 
		 {value, _SecName}, 
		 {value, _SecLevel},
		 {value, ?'RowStatus_active'}] ->
		    {error, {invalid_MpModel, MpModel, snmpTargetParamsTable}};
		[{value, _MpModel}, 
		 {value, _SecModel}, 
		 {value, _SecName}, 
		 {value, _SecLevel},
		 {value, RowStatus}] ->
		    {error, {invalid_RowStatus, RowStatus, snmpTargetParamsTable}};
		Bad ->
		    ?vlog("find_dest -> "
			  "could not find snmpTargetParamsTable info: "
			  "~n   Bad: ~p", [Bad]),
		    {error, {not_found, snmpTargetParamsTable}}
	    end;
	
	[{value, _TDomain},
	 {value, _TAddress},
	 {value, _Timeout},
	 {value, _RetryCount},
	 {value, _Params},
	 {value, RowStatus}] ->
	    {error, {invalid_RowStatus, RowStatus, snmpTargetAddrTable}};
	_ ->
	    {error, {not_found, snmpTargetAddrTable}}
    end.

send_discovery_pdu({Dest, TargetName, {SecModel, SecName, SecLevel}, 
		    Timeout, Retry}, 
		   Record, ContextName, Vbs, NetIf, 
		   ExtraInfo) ->
    ?vdebug("send_discovery_pdu -> entry with "
	    "~n   Destination address: ~p"
	    "~n   Target name:         ~p"
	    "~n   Sec model:           ~p"
	    "~n   Sec name:            ~p"
	    "~n   Sec level:           ~p"
	    "~n   Timeout:             ~p"
	    "~n   Retry:               ~p"
	    "~n   Record:              ~p"
	    "~n   ContextName:         ~p"
	    "~n   ExtraInfo:           ~p",
	    [Dest, TargetName, SecModel, SecName, SecLevel, 
	     Timeout, Retry, Record, ContextName, ExtraInfo]),
    case get_mib_view(SecModel, SecName, SecLevel, ContextName) of
	{ok, MibView} ->
	    case check_all_varbinds(Record, Vbs, MibView) of
		true ->
		    SysUpTime = snmp_standard_mib:sys_up_time(), 
		    send_discovery_pdu(Record, Dest, Vbs, 
				       SecModel, SecName, SecLevel, 
				       TargetName, ContextName, 
				       Timeout, Retry, 
				       SysUpTime, NetIf, ExtraInfo);
		false ->
		    {error, {mibview_validation_failed, Vbs, MibView}}
	    end;
	{discarded, Reason} ->
	    {error, {failed_get_mibview, Reason}}
    end.

send_discovery_pdu(Record, Dest, Vbs, 
		   SecModel, SecName, SecLevel, TargetName, 
		   ContextName, Timeout, Retry, SysUpTime, NetIf, ExtraInfo) ->
    {_Oid, IVbs} = mk_v2_trap(Record, Vbs, SysUpTime), % v2 refers to SMIv2;
    Sender = proc_lib:spawn_link(?MODULE, init_discovery_inform,
				 [self(), 
				  Dest, 
				  SecModel, SecName, SecLevel, TargetName,
				  ContextName, 
				  Timeout, Retry, 
				  IVbs, NetIf, 
				  get(verbosity), 
				  ExtraInfo]),
    {ok, Sender, SecLevel}.

init_discovery_inform(Parent, 
		      Dest, 
		      SecModel, SecName, SecLevel, TargetName, 
		      ContextName, Timeout, Retry, Vbs, NetIf, Verbosity) ->
    ExtraInfo = ?DEFAULT_NOTIF_EXTRA_INFO, 
    init_discovery_inform(Parent, 
			  Dest, 
			  SecModel, SecName, SecLevel, TargetName, 
			  ContextName, Timeout, Retry, Vbs, NetIf, 
			  Verbosity, ExtraInfo).
init_discovery_inform(Parent, 
		      Dest, 
		      SecModel, SecName, SecLevel, TargetName, 
		      ContextName, Timeout, Retry, Vbs, NetIf, 
		      Verbosity, ExtraInfo) ->
    put(verbosity, Verbosity),
    put(sname, madis),
    Pdu = make_discovery_pdu(Vbs), 
    ContextEngineId = snmp_framework_mib:get_engine_id(),
    SecLevelFlag = mk_flag(SecLevel), 
    SecData      = {SecModel, SecName, SecLevelFlag, TargetName}, 
    MsgData      = {SecData, ContextEngineId, ContextName}, 
    Msg          = {send_discovery, Pdu, MsgData, Dest, self(), ExtraInfo},
    ?MODULE:send_discovery_inform(Parent, Timeout*10, Retry, Msg, NetIf).

%% note_timeout(Timeout, Retry) 
%%   when ((is_integer(Timeout) andalso (Timeout > 0)) andalso 
%% 	(is_integer(Retry) andalso (Retry > 0)))
%%     note_timeout(Timeout*10, Retry, 0);
%% note_timeout(Timeout, Retry) 
%%   when (is_integer(Timeout) andalso (Timeout > 0)) ->
%%     Timeout*10.

%% note_timeout(_Timeout, -1, NoteTimeout) ->
%%     NoteTimeout;
%% note_timeout(Timeout, Retry, NoteTimeout) when ->
%%     note_timeout(Timeout*2, Retry-1, NoteTimeout+Timeout).

send_discovery_inform(Parent, _Timeout, -1, _Msg, _NetIf) ->
    Parent ! {discovery_response, {error, timeout}};
send_discovery_inform(Parent, Timeout, Retry, Msg, NetIf) ->
    NetIf ! Msg,
    receive
	{snmp_discovery_response_received, Pdu, undefined} ->
	    ?vtrace("received stage 2 discovery response: "
		    "~n   Pdu: ~p", [Pdu]),
	    Parent ! {discovery_response, {ok, Pdu}};
	{snmp_discovery_response_received, Pdu, ManagerEngineId} ->
	    ?vtrace("received stage 1 discovery response: "
		    "~n   Pdu:             ~p"
		    "~n   ManagerEngineId: ~p", [Pdu, ManagerEngineId]),
	    Parent ! {discovery_response, {ok, Pdu, ManagerEngineId}}
    after
	Timeout ->
	    ?MODULE:send_discovery_inform(Parent, 
					  Timeout*2, Retry-1, Msg, NetIf)
    end.

    
%%-----------------------------------------------------------------
%% NOTE: This function is executed in the master agent's context
%% For each target, check if it has access to the objects in the
%% notification, determine which message version (v1, v2c or v3)
%% should be used for the target, and determine the message
%% specific parameters to be used.
%%-----------------------------------------------------------------
send_trap_pdus([{DestAddr, TargetName, 
		 {MpModel, SecModel, SecName, SecLevel}, Type} | T],
	       ContextName, 
	       {TrapRec, Vbs}, V1Res, V2Res, V3Res, Recv, 
	       LocalEngineID, ExtraInfo, NetIf) ->
    ?vdebug("send trap pdus: "
	    "~n   Destination address: ~p"
	    "~n   Target name:         ~p"
	    "~n   MP model:            ~p"
	    "~n   Type:                ~p"
	    "~n   V1Res:               ~p"
	    "~n   V2Res:               ~p"
	    "~n   V3Res:               ~p",
	    [DestAddr, TargetName, MpModel, Type, V1Res, V2Res, V3Res]),
    case get_mib_view(SecModel, SecName, SecLevel, ContextName) of
	{ok, MibView} ->
	    case check_all_varbinds(TrapRec, Vbs, MibView) of
		true when MpModel =:= ?MP_V1 ->
		    ?vtrace("send_trap_pdus -> v1 mp model",[]),
		    ContextEngineId = LocalEngineID, 
		    case snmp_community_mib:vacm2community({SecName,
							    ContextEngineId,
							    ContextName},
							   DestAddr) of
			{ok, Community} ->
			    ?vdebug("community found  for v1 dest: ~p",
				    [element(2, DestAddr)]),
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   [{DestAddr, Community} | V1Res],
					   V2Res, V3Res, Recv, 
					   LocalEngineID, ExtraInfo, NetIf);
			undefined ->
			    ?vdebug("No community found for v1 dest: ~p", 
				    [element(2, DestAddr)]),
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   V1Res, V2Res, V3Res, Recv, 
					   LocalEngineID, ExtraInfo, NetIf)
		    end;
		true when MpModel =:= ?MP_V2C ->
		    ?vtrace("send_trap_pdus -> v2c mp model",[]),
		    ContextEngineId = LocalEngineID, 
		    case snmp_community_mib:vacm2community({SecName,
							    ContextEngineId,
							    ContextName},
							   DestAddr) of
			{ok, Community} ->
			    ?vdebug("community found for v2c dest: ~p", 
				    [element(2, DestAddr)]),
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   V1Res,
					   [{DestAddr, Community, Type}|V2Res],
					   V3Res, Recv, 
					   LocalEngineID, ExtraInfo, NetIf);
			undefined ->
			    ?vdebug("No community found for v2c dest: ~p", 
				    [element(2, DestAddr)]),
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   V1Res, V2Res, V3Res, Recv, 
					   LocalEngineID, ExtraInfo, NetIf)
		    end;
		true when MpModel =:= ?MP_V3 ->
		    ?vtrace("send_trap_pdus -> v3 mp model",[]),
		    SecLevelF = mk_flag(SecLevel),
		    MsgData = {SecModel, SecName, SecLevelF, TargetName},
		    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
				   V1Res, V2Res,
				   [{DestAddr, MsgData, Type} | V3Res],
				   Recv, LocalEngineID, ExtraInfo, NetIf);
		true ->
		    ?vlog("bad MpModel ~p for dest ~p",
			  [MpModel, element(2, DestAddr)]),
		    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
				   V1Res, V2Res, V3Res, Recv, 
				   LocalEngineID, ExtraInfo, NetIf);
		_ ->
		    ?vlog("no access for dest: "
			  "~n   ~p in target ~p",
			  [element(2, DestAddr), TargetName]),
		    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
				   V1Res, V2Res, V3Res, Recv, 
				   LocalEngineID, ExtraInfo, NetIf)
	    end;
	{discarded, Reason} ->
	    ?vlog("mib view error ~p for"
		  "~n   dest:    ~p"
		  "~n   SecName: ~w", 
		  [Reason, element(2, DestAddr), SecName]),
	    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
			   V1Res, V2Res, V3Res, Recv, 
			   LocalEngineID, ExtraInfo, NetIf)
    end;
send_trap_pdus([], ContextName, {TrapRec, Vbs}, 
	       V1Res, V2Res, V3Res, Recv, 
	       LocalEngineID, ExtraInfo, 
	       NetIf) ->
    SysUpTime = snmp_standard_mib:sys_up_time(),
    ?vdebug("send trap pdus with sysUpTime ~p", [SysUpTime]),
    InformRecvs   = get_inform_recvs(V2Res ++ V3Res),
    InformTargets = [Addr || {Addr, _, _, _} <- InformRecvs],
    deliver_recv(Recv, snmp_targets, InformTargets),
    send_v1_trap(TrapRec, V1Res, Vbs, ExtraInfo, NetIf, SysUpTime),
    send_v2_trap(TrapRec, V2Res, Vbs, Recv, ExtraInfo, NetIf, SysUpTime),
    send_v3_trap(TrapRec, V3Res, Vbs, Recv, LocalEngineID, ExtraInfo, NetIf, 
		 SysUpTime, ContextName).

send_v1_trap(_TrapRec, [], _Vbs, _ExtraInfo, _NetIf, _SysUpTime) ->
    ok;
send_v1_trap(
  #trap{enterpriseoid = Enter, specificcode = Spec},
  V1Res, Vbs, ExtraInfo, NetIf, SysUpTime) ->
    ?vdebug("prepare to send v1 trap "
	    "~n   '~p'"
	    "~n   with"
	    "~n   ~p"
	    "~n   to"
	    "~n   ~p", [Enter, Spec, V1Res]),
    do_send_v1_trap(Enter, Spec, V1Res, Vbs, ExtraInfo, NetIf, SysUpTime);
send_v1_trap(
  #notification{oid = Oid},
  V1Res, Vbs, ExtraInfo, NetIf, SysUpTime) ->
    %% Use alg. in rfc2089 to map a v2 trap to a v1 trap
    % delete Counter64 objects from vbs
    ?vdebug("prepare to send v1 trap '~p'",[Oid]),
    NVbs = [Vb || Vb <- Vbs, Vb#varbind.variabletype =/= 'Counter64'],
    {Enter,Spec} = 
	case Oid of
	    [1,3,6,1,6,3,1,1,5,Specific] ->
		{?snmp,Specific - 1};
	    _ ->
		case lists:reverse(Oid) of
		    [Last, 0 | First] ->
			{lists:reverse(First),Last};
		    [Last | First] ->
			{lists:reverse(First),Last}
		end
	end,
    do_send_v1_trap(Enter, Spec, V1Res, NVbs, ExtraInfo, NetIf, SysUpTime).

do_send_v1_trap(Enter, Spec, V1Res, NVbs, ExtraInfo, NetIf, SysUpTime) ->
    {value, Transports} = snmp_framework_mib:intAgentTransports(get),
    {_Domain, {AgentIp, _AgentPort}} =
	case lists:keyfind(snmpUDPDomain, 1, Transports) of
	    false ->
		case lists:keyfind(transportDomainUdpIpv4, 1, Transports) of
		    false ->
			?vtrace(
			   "snmpa_trap: can not send v1 trap "
			   "without IPv4 domain: ~p",
			   [Transports]),
			user_err(
			   "snmpa_trap: can not send v1 trap "
			   "without IPv4 domain: ~p",
			   [Transports]);
		    DomainAddr ->
			DomainAddr
		end;
	    DomainAddr ->
		DomainAddr
	end,
    TrapPdu = make_v1_trap_pdu(Enter, Spec, NVbs, SysUpTime, AgentIp),
    AddrCommunities = mk_addr_communities(V1Res),
    lists:foreach(
      fun ({Community, Addrs}) ->
	      ?vtrace("send v1 trap to ~p",[Addrs]),
	      NetIf ! {send_pdu, 'version-1', TrapPdu,
		       {community, Community}, Addrs, ExtraInfo}
      end, AddrCommunities).

send_v2_trap(_TrapRec, [], _Vbs, _Recv, _ExtraInfo, _NetIf, _SysUpTime) ->
    ok;
send_v2_trap(TrapRec, V2Res, Vbs, Recv, ExtraInfo, NetIf, SysUpTime) ->
    ?vdebug("prepare to send v2 trap",[]),
    {_Oid, IVbs} = mk_v2_trap(TrapRec, Vbs, SysUpTime),
    TrapRecvs    = get_trap_recvs(V2Res),
    InformRecvs  = get_inform_recvs(V2Res),
    do_send_v2_trap(TrapRecvs, IVbs, ExtraInfo, NetIf),
    do_send_v2_inform(InformRecvs, IVbs, Recv, ExtraInfo, NetIf).
    
send_v3_trap(_TrapRec, [], _Vbs, _Recv, _LocalEngineID, _ExtraInfo, 
	     _NetIf, _SysUpTime, _ContextName) ->
    ok;
send_v3_trap(TrapRec, V3Res, Vbs, Recv, LocalEngineID, ExtraInfo, 
	     NetIf, SysUpTime, ContextName) ->
    ?vdebug("prepare to send v3 trap",[]),
    {_Oid, IVbs} = mk_v2_trap(TrapRec, Vbs, SysUpTime), % v2 refers to SMIv2;
    TrapRecvs = get_trap_recvs(V3Res),                  % same SMI for v3
    InformRecvs = get_inform_recvs(V3Res),
    do_send_v3_trap(TrapRecvs, ContextName, IVbs, ExtraInfo, NetIf),
    do_send_v3_inform(InformRecvs, ContextName, IVbs, Recv, 
		      LocalEngineID, ExtraInfo, NetIf).
    

mk_v2_trap(#notification{oid = Oid}, Vbs, SysUpTime) ->
    ?vtrace("make v2 notification '~p'",[Oid]),
    mk_v2_notif(Oid, Vbs, SysUpTime);
mk_v2_trap(#trap{enterpriseoid = Enter, specificcode = Spec}, 
	   Vbs, SysUpTime) ->
    %% Use alg. in rfc1908 to map a v1 trap to a v2 trap
    ?vtrace("make v2 trap for '~p' with ~p",[Enter,Spec]),
    {Oid,Enterp} = 
	case Enter of
	    ?snmp ->
		{?snmpTraps ++ [Spec + 1],sys_object_id()};
	    _ ->
		{Enter ++ [0, Spec],Enter}
	end,
    ExtraVb = #varbind{oid = ?snmpTrapEnterprise_instance,
		       variabletype = 'OBJECT IDENTIFIER',
		       value = Enterp},
    mk_v2_notif(Oid, Vbs ++ [ExtraVb], SysUpTime).
    
mk_v2_notif(Oid, Vbs, SysUpTime) ->
    IVbs = [#varbind{oid = ?sysUpTime_instance,
		     variabletype = 'TimeTicks',
		     value = SysUpTime},
	    #varbind{oid = ?snmpTrapOID_instance,
		     variabletype = 'OBJECT IDENTIFIER',
		     value = Oid} | Vbs],
    {Oid, IVbs}.

get_trap_recvs(TrapRecvs) ->
    [{Addr, MsgData} || {Addr, MsgData, trap} <- TrapRecvs].

get_inform_recvs(InformRecvs) ->
    [{Addr, MsgData, Timeout, Retry} || 
	{Addr, MsgData, {inform, Timeout, Retry}} <- InformRecvs].

do_send_v2_trap([], _Vbs, _ExtraInfo, _NetIf) ->
    ok;
do_send_v2_trap(Recvs, Vbs, ExtraInfo, NetIf) ->
    TrapPdu = make_v2_notif_pdu(Vbs, 'snmpv2-trap'),
    AddrCommunities = mk_addr_communities(Recvs),
    lists:foreach(fun({Community, Addrs}) ->
			  ?vtrace("~n   send v2 trap to ~p",[Addrs]),
			  NetIf ! {send_pdu, 'version-2', TrapPdu,
				   {community, Community}, Addrs, ExtraInfo}
		  end, AddrCommunities),
    ok.

do_send_v2_inform([], _Vbs, _Recv, _ExtraInfo, _NetIf) ->
    ok;
do_send_v2_inform(Recvs, Vbs, Recv, ExtraInfo, NetIf) ->
    lists:foreach(
      fun({Addr, Community, Timeout, Retry}) ->
	      ?vtrace("~n   start inform sender to send v2 inform to ~p",
		      [Addr]),
	      proc_lib:spawn_link(?MODULE, init_v2_inform,
				  [Addr, Timeout, Retry, Vbs,
				   Recv, ExtraInfo, NetIf, Community,
				   get(verbosity), get(sname)])
      end, 
      Recvs).

do_send_v3_trap([], _ContextName, _Vbs, _ExtraInfo, _NetIf) ->
    ok;
do_send_v3_trap(Recvs, ContextName, Vbs, ExtraInfo, NetIf) ->
    TrapPdu = make_v2_notif_pdu(Vbs, 'snmpv2-trap'), % Yes, v2
    ContextEngineId = snmp_framework_mib:get_engine_id(),
    lists:foreach(fun(Recv) ->
			  ?vtrace("~n   send v3 notif to ~p",[Recv]),
			  NetIf ! {send_pdu, 'version-3', TrapPdu,
				   {v3, ContextEngineId, ContextName}, 
				   [Recv], ExtraInfo}
		  end, Recvs),
    ok.

do_send_v3_inform([], _ContextName, _Vbs, _Recv, 
		  _LocalEngineID, _ExtraInfo, _NetIf) ->
    ok;
do_send_v3_inform(Recvs, ContextName, Vbs, Recv, 
		  LocalEngineID, ExtraInfo, NetIf) ->
    lists:foreach(
      fun({Addr, MsgData, Timeout, Retry}) ->
	      ?vtrace("~n   start inform sender to send v3 inform to ~p",
		      [Addr]),
	      proc_lib:spawn_link(?MODULE, init_v3_inform,
				  [{Addr, MsgData}, Timeout, Retry, Vbs,
				   Recv, LocalEngineID, ExtraInfo, 
				   NetIf, ContextName,
				   get(verbosity), get(sname)])
      end, 
      Recvs).

%% New process
init_v2_inform(Addr, Timeout, Retry, Vbs, Recv, NetIf, Community, V, S) ->
    ExtraInfo = ?DEFAULT_NOTIF_EXTRA_INFO, 
    init_v2_inform(Addr, Timeout, Retry, Vbs, Recv, ExtraInfo, NetIf, 
		   Community, V, S). 

init_v2_inform(Addr, Timeout, Retry, Vbs, Recv, ExtraInfo, NetIf, 
	       Community, V, S) ->
    %% Make a new Inform for each recipient; they need unique
    %% request-ids!
    put(verbosity,V),
    put(sname,inform_sender_short_name(S)),
    ?vdebug("~n   starting with timeout = ~p and retry = ~p",
	    [Timeout,Retry]),
    InformPdu = make_v2_notif_pdu(Vbs, 'inform-request'),
    Msg = {send_pdu_req, 'version-2', InformPdu, {community, Community},
	   [Addr], self(), ExtraInfo},
    ?MODULE:send_inform(Addr, Timeout*10, Retry, Msg, Recv, NetIf).
    

%% New process
init_v3_inform(Addr, Timeout, Retry, Vbs, Recv, NetIf, ContextName, V, S) ->
    ExtraInfo     = ?DEFAULT_NOTIF_EXTRA_INFO, 
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    init_v3_inform(Addr, Timeout, Retry, Vbs, Recv, 
		   LocalEngineID, ExtraInfo, 
		   NetIf, ContextName, V, S).

init_v3_inform(Addr, Timeout, Retry, Vbs, Recv, LocalEngineID, NetIf, 
	       ContextName, V, S) ->
    ExtraInfo = ?DEFAULT_NOTIF_EXTRA_INFO, 
    init_v3_inform(Addr, Timeout, Retry, Vbs, Recv, 
		   LocalEngineID, ExtraInfo, 
		   NetIf, ContextName, V, S).

init_v3_inform(Addr, Timeout, Retry, Vbs, Recv, LocalEngineID, ExtraInfo, 
	       NetIf, ContextName, V, S) ->
    %% Make a new Inform for each recipient; they need unique
    %% request-ids!
    put(verbosity,V),
    put(sname,inform_sender_short_name(S)),
    ?vdebug("~n   starting with timeout = ~p and retry = ~p",
	    [Timeout,Retry]),
    InformPdu = make_v2_notif_pdu(Vbs, 'inform-request'), % Yes, v2
    ContextEngineId = LocalEngineID, 
    Msg = {send_pdu_req, 'version-3', InformPdu,
	   {v3, ContextEngineId, ContextName}, [Addr], self(), ExtraInfo},
    ?MODULE:send_inform(Addr, Timeout*10, Retry, Msg, Recv, NetIf).

send_inform(Addr, _Timeout, -1, _Msg,  Recv, _NetIf) ->
    ?vinfo("~n   Delivery of send-pdu-request to net-if failed: reply timeout",
	   []),
    deliver_recv(Recv, snmp_notification, {no_response, Addr});
send_inform(Addr, Timeout, Retry, Msg, Recv, NetIf) ->
    ?vtrace("deliver send-pdu-request to net-if when"
	    "~n   Timeout: ~p"
	    "~n   Retry:   ~p",[Timeout, Retry]),
    NetIf ! Msg,
    receive
	{snmp_response_received, _Vsn, _Pdu, _From} ->
	    ?vtrace("received response for ~p (when Retry = ~p)", 
		    [Recv, Retry]),
	    deliver_recv(Recv, snmp_notification, {got_response, Addr})
    after
	Timeout ->
	    ?MODULE:send_inform(Addr, Timeout*2, Retry-1, Msg, Recv, NetIf)
    end.

% A nasty bit of verbosity setup...    
inform_sender_short_name(ma)   -> mais;
inform_sender_short_name(maw)  -> mais;
inform_sender_short_name(mats) -> mais;
inform_sender_short_name(_)    -> sais.

deliver_recv(no_receiver, _MsgId, _Result) ->
    ?vtrace("deliver_recv -> no receiver", []),
    ok;
deliver_recv(#snmpa_notification_delivery_info{tag   = Tag,
					       mod   = Mod,
					       extra = Extra},
	     snmp_targets, TAddrs) when is_list(TAddrs) ->
    ?vtrace("deliver_recv(snmp_targets) -> entry with"
	"~n   Tag:    ~p"
	"~n   Mod:    ~p"
	"~n   Extra:  ~p"
	"~n   TAddrs: ~p"
	"", [Tag, Mod, Extra, TAddrs]),
    Addrs = transform_taddrs(TAddrs),
    (catch Mod:delivery_targets(Tag, Addrs, Extra));
deliver_recv(#snmpa_notification_delivery_info{tag   = Tag,
					       mod   = Mod,
					       extra = Extra},
	     snmp_notification, {DeliveryResult, TAddr}) ->
    ?vtrace("deliver_recv -> entry with"
	"~n   Tag:            ~p"
	"~n   Mod:            ~p"
	"~n   Extra:          ~p"
	"~n   DeliveryResult: ~p"
	"~n   TAddr:          ~p"
	"", [Tag, Mod, Extra, DeliveryResult, TAddr]),
    [Addr] = transform_taddrs([TAddr]),
    (catch Mod:delivery_info(Tag, Addr, DeliveryResult, Extra));
deliver_recv({Tag, Receiver}, MsgId, Result) ->
    ?vtrace("deliver_recv -> entry with"
	"~n   Tag:      ~p"
	"~n   Receiver: ~p"
	"~n   MsgId:    ~p"
	"~n   Result:   ~p"
	"", [Tag, Receiver, MsgId, Result]),
    Msg = {MsgId, Tag, Result},
    case Receiver of
	Pid when is_pid(Pid) ->
	    Pid ! Msg;
	Name when is_atom(Name) ->
	    catch Name ! Msg;
	{M, F, A} ->
	    catch M:F([Msg | A]);
	Else ->
	    ?vinfo("~n   Cannot deliver acknowledgment: bad receiver = '~p'",
		   [Else]),
	    user_err("snmpa: bad receiver, ~w\n", [Else])
    end;
deliver_recv(Else, _MsgId, _Result) ->
    ?vinfo("~n   Cannot deliver acknowledgment: bad receiver = '~p'",
	   [Else]),
    user_err("snmpa: bad receiver, ~w\n", [Else]).

transform_taddrs(TAddrs) ->
    UseTDomain =
	case snmp_framework_mib:intAgentTransportDomain(get) of
	    {value,snmpUDPDomain} ->
		false;
	    {value,_} ->
		true;
	    genErr ->
		false
	end,
    DomAddrs = [transform_taddr(TAddr) || TAddr <- TAddrs],
    case UseTDomain of
	true ->
	    DomAddrs;
	false ->
	    [Addr || {_Domain, Addr} <- DomAddrs]
    end.

%% v2
transform_taddr({?snmpUDPDomain, Addr}) ->
    transform_taddr(transportDomainIdpIpv4, Addr);
transform_taddr({?transportDomainUdpIpv4, Addr}) ->
    transform_taddr(transportDomainUdpIpv4, Addr);
transform_taddr({?transportDomainUdpIpv6, Addr}) ->
    transform_taddr(transportDomainUdpIpv6, Addr);
%% v3
transform_taddr({{?snmpUDPDomain, Addr}, _MsgData}) ->
    transform_taddr(transportDomainUdpIpv4, Addr);
transform_taddr({{?transportDomainUdpIpv4, Addr}, _MsgData}) ->
    transform_taddr(transportDomainUdpIpv4, Addr);
transform_taddr({{?transportDomainUdpIpv6, Addr}, _MsgData}) ->
    transform_taddr(transportDomainUdpIpv6, Addr).

transform_taddr(
  transportDomainUdpIpv4 = Domain,
  [A1,A2,A3,A4,P1,P2]) ->
    Ip = {A1, A2, A3, A4},
    Port = P1 bsl 8 + P2,
    {Domain, {Ip, Port}};
transform_taddr(
  transportDomainUdpIpv6 = Domain,
  [A1, A2, A3, A4, A5, A6, A7, A8, P1, P2]) ->
    Ip = {A1, A2, A3, A4, A5, A6, A7, A8},
    Port = P1 bsl 8 + P2,
    {Domain, {Ip, Port}};
transform_taddr(
  transportDomainUdpIpv6 = Domain,
  [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16,
   P1, P2]) ->
    Ip =
	{(A1 bsl 8) bor A2, (A3 bsl 8) bor A4,
	 (A5 bsl 8) bor A6, (A7 bsl 8) bor A8,
	 (A9 bsl 8) bor A10, (A11 bsl 8) bor A12,
	 (A13 bsl 8) bor A14, (A15 bsl 8) bor A16},
    Port = P1 bsl 8 + P2,
    {Domain, {Ip, Port}}.

%% transform_taddr({?snmpUDPDomain, [A1, A2, A3, A4, P1, P2]}) -> % v2
%%     Addr = {A1, A2, A3, A4},
%%     Port = P1 bsl 8 + P2,
%%     {Addr, Port};
%% transform_taddr({?transportDomainUdpIpv4, [A1, A2, A3, A4, P1, P2]}) -> % v2
%%     Addr = {A1, A2, A3, A4},
%%     Port = P1 bsl 8 + P2,
%%     {Addr, Port};
%% transform_taddr({?transportDomainUdpIpv6,
%% 		 [A1, A2, A3, A4, A5, A6, A7, A8, P1, P2]}) -> % v2
%%     Addr = {A1, A2, A3, A4, A5, A6, A7, A8},
%%     Port = P1 bsl 8 + P2,
%%     {Addr, Port};
%% transform_taddr({{?snmpUDPDomain, [A1, A2, A3, A4, P1, P2]}, _MsgData}) -> % v3
%%     Addr = {A1, A2, A3, A4},
%%     Port = P1 bsl 8 + P2,
%%     {Addr, Port};
%% transform_taddr({{?transportDomainUdpIpv4, [A1, A2, A3, A4, P1, P2]}, _MsgData}) -> % v3
%%     Addr = {A1, A2, A3, A4},
%%     Port = P1 bsl 8 + P2,
%%     {Addr, Port};
%% transform_taddr({{?transportDomainUdpIpv6,
%% 		  [A1, A2, A3, A4, A5, A6, A7, A8, P1, P2]}, _MsgData}) -> % v3
%%     Addr = {A1, A2, A3, A4, A5, A6, A7, A8},
%%     Port = P1 bsl 8 + P2,
%%     {Addr, Port}.



check_all_varbinds(#notification{oid = Oid}, Vbs, MibView) ->
    case snmpa_acm:validate_mib_view(Oid, MibView) of
	true  -> check_all_varbinds(Vbs, MibView);
	false -> false
    end;
check_all_varbinds(#trap{enterpriseoid = Enter, specificcode = Spec},
		   Vbs, MibView) ->
    %% Use alg. in rfc1908 to map a v1 trap to a v2 trap
    Oid = case Enter of
	      ?snmp -> ?snmpTraps ++ [Spec + 1];
	      _ -> Enter ++ [0, Spec]
	  end,
    case snmpa_acm:validate_mib_view(Oid, MibView) of
	true  -> check_all_varbinds(Vbs, MibView);
	false -> false
    end.

check_all_varbinds([#varbind{oid = Oid} | Vbs], MibView) ->
    case snmpa_acm:validate_mib_view(Oid, MibView) of
	true -> check_all_varbinds(Vbs, MibView);
	false -> false
    end;
check_all_varbinds([], _MibView) -> 
    true.


%%--------------------------------------------------
%% Functions to access the local mib.
%%--------------------------------------------------
sys_object_id() ->
    case snmpa_agent:do_get(snmpa_acm:get_root_mib_view(),
			    [#varbind{oid = ?sysObjectID_instance}],
			    true) of
	{noError, _, [#varbind{value = Value}]} ->
	    Value;
	X ->
	    user_err("sysObjectID bad return value ~w", [X])
    end.

%% Collect all ADDRs for each community together.
%% In: [{Addr, Community}]
%% Out: [{Community, [Addr]}]
mk_addr_communities(Recvs) ->
    [{Addr, Comm} | T] = lists:keysort(2, Recvs),
    mic(T, Comm, [Addr], []).

mic([{Addr, Comm} | T], CurComm, AddrList, Res) when Comm =:= CurComm ->
    mic(T, CurComm, [Addr | AddrList], Res);
mic([{Addr, Comm} | T], CurComm, AddrList, Res) ->
    mic(T, Comm, [Addr], [{CurComm, AddrList} | Res]);
mic([], CurComm, AddrList, Res) ->
    [{CurComm, AddrList} | Res].


%%-----------------------------------------------------------------
%% Convert the SecurityLevel into a flag value used by snmpa_mpd
%%-----------------------------------------------------------------
mk_flag(?'SnmpSecurityLevel_noAuthNoPriv') -> 0;
mk_flag(?'SnmpSecurityLevel_authNoPriv') -> 1;
mk_flag(?'SnmpSecurityLevel_authPriv') -> 3.
     

%%--------------------------------------------------
%% Mib view wrapper
%%--------------------------------------------------
get_mib_view(SecModel, SecName, SecLevel, ContextName) ->
    snmpa_vacm:get_mib_view(notify, 
			    SecModel, SecName, SecLevel, ContextName).


user_err(F, A) ->
    snmpa_error:user_err(F, A).
