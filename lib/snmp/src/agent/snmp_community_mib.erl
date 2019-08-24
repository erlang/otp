%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2019. All Rights Reserved.
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
-module(snmp_community_mib).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([configure/1, reconfigure/1,
	 snmpCommunityTable/1, snmpCommunityTable/3,
	 snmpTargetAddrExtTable/3,
	 community2vacm/2, vacm2community/2,
	 get_target_addr_ext_mms/2]).
-export([add_community/5, add_community/6, delete_community/1]).
-export([check_community/1]).

-include("snmpa_internal.hrl").
-include("SNMP-COMMUNITY-MIB.hrl").
-include("SNMP-TARGET-MIB.hrl").
-include("SNMPv2-TM.hrl").
-include("SNMPv2-TC.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"COMMUNITY-MIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%%-----------------------------------------------------------------
%%% Implements the instrumentation functions and additional 
%%% functions for the SNMP-COMMUNITY-MIB.
%%% This MIB contains objects that makes it possible to use VACM
%%% with SNMPv1 and SNMPv2c.
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the community mib tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case db(snmpCommunityTable) of
        {_, mnesia} ->
            ?vlog("community table in mnesia: cleanup",[]),
            gc_tabs();
	TabDb ->
	    case snmpa_local_db:table_exists(TabDb) of
		true ->
		    ?vlog("community table exist: cleanup",[]),
		    gc_tabs();
		false ->
		    ?vlog("community table does not exist: reconfigure",[]),
		    reconfigure(Dir)
	    end
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the community mib tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    set_sname(),
    case (catch do_reconfigure(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    ?vinfo("reconfigure error: ~p", [Reason]),
	    config_err("reconfigure failed: ~p", [Reason]),
	    exit(configuration_error);
	Error ->
	    ?vinfo("reconfigure failed: ~p", [Error]),
	    config_err("reconfigure failed: ~p", [Error]),
	    exit(configuration_error)
    end.

do_reconfigure(Dir) ->
    ?vdebug("read community config files",[]),
    Comms = read_community_config_files(Dir),
    ?vdebug("initiate tables",[]),
    init_tabs(Comms),
    ok.

init_tabs(Comms) ->
    ?vdebug("create community table",[]),
    snmpa_local_db:table_delete(db(snmpCommunityTable)),
    snmpa_local_db:table_create(db(snmpCommunityTable)),
    ?vdebug("invalidate cache",[]),
    invalidate_cache(),
    ?vdebug("initiate community table",[]),
    init_comm_table(Comms).
    

read_community_config_files(Dir) ->
    ?vdebug("read community config file",[]),
    FileName = "community.conf", 
    Gen      = fun(D, Reason) -> 
		       warning_msg("failed reading config file ~s"
				   "~n   Config Dir: ~s"
				   "~n   Reason:     ~p", 
				   [FileName, D, Reason]),
		       ok 
	     end,
    Order    = fun snmp_conf:no_order/2,
    Filter   = fun snmp_conf:no_filter/1,
    Check    = fun(Entry, State) -> {check_community(Entry), State} end,
    [Comms]  = 
	snmp_conf:read_files(Dir, [{FileName, Gen, Order, Check, Filter}]),
    Comms.

check_community({Index, CommunityName, SecName, CtxName, TransportTag}) ->
    EngineID = get_engine_id(),
    check_community({Index, CommunityName, SecName, 
		     EngineID, CtxName, TransportTag});
check_community({Index, CommunityName, SecName, 
		 EngineID, CtxName, TransportTag}) ->
    snmp_conf:check_string(Index,{gt,0}),
    snmp_conf:check_string(CommunityName),
    snmp_conf:check_string(SecName),
    snmp_conf:check_string(CtxName),
    snmp_conf:check_string(TransportTag),
    Comm = {Index, CommunityName, SecName, EngineID, CtxName, TransportTag,
	    ?'StorageType_nonVolatile', ?'RowStatus_active'},
    {ok, Comm};
check_community(X) ->
    error({invalid_community, X}).

%% This is for the case when check_community is called from the 
%% snmp_config module (to generate community config file) and
%% the agent is not started. The actual return value is not 
%% checked, as long as it is '{ok, _}'.
get_engine_id() ->
    case (catch snmp_framework_mib:get_engine_id()) of
	{'EXIT', _} ->
	    "agentEngine";
	EngineID ->
	    EngineID
    end.

init_comm_table([Row | T]) ->
    ?vtrace("init_comm_table -> entry with"
	    "~n   Row: ~p", [Row]),
    Key = element(1, Row),
    snmpa_local_db:table_create_row(db(snmpCommunityTable), Key, Row),
    update_cache(Key),
    init_comm_table(T);
init_comm_table([]) -> 
    ?vtrace("init_comm_table -> entry when done", []),
    true.

table_cre_row(Tab, Key, Row) ->
    snmpa_mib_lib:table_cre_row(db(Tab), Key, Row).

table_del_row(Tab, Key) ->
    snmpa_mib_lib:table_del_row(db(Tab), Key).


%% FIXME: does not work with mnesia
add_community(Idx, CommName, SecName, CtxName, TransportTag) ->
    Community = {Idx, CommName, SecName, CtxName, TransportTag},
    do_add_community(Community).

add_community(Idx, CommName, SecName, EngineId, CtxName, TransportTag) ->
    Community = {Idx, CommName, SecName, EngineId, CtxName, TransportTag},
    do_add_community(Community).

do_add_community(Community) ->
    try check_community(Community) of
	{ok, Row} ->
	    Key = element(1, Row),
	    case table_cre_row(snmpCommunityTable, Key, Row) of
		true ->
		    update_cache(Key),
		    {ok, Key};
		false ->
		    {error, create_failed}
	    end
    catch
	throw:{error, _} = ERROR ->
	    ERROR;
	C:E:S ->
	    {error, {C, E, S}}
    end.

%% FIXME: does not work with mnesia
delete_community(Key) ->
    invalidate_cache(Key),
    case table_del_row(snmpCommunityTable, Key) of
	true ->
	    ok;
	false ->
	    {error, delete_failed}
    end.


gc_tabs() ->
    DB  = db(snmpCommunityTable),
    STC = stc(snmpCommunityTable),
    FOI = foi(snmpCommunityTable),
    IR  = fun(RowIndex) -> invalidate_cache(RowIndex) end,
    UR  = fun(RowIndex) -> update_cache(RowIndex) end,
    snmpa_mib_lib:gc_tab(DB, STC, FOI, IR, UR),
    ok.


%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% We keep two caches for mapping;
%% one that maps CommunityName to CommunityIndex, and
%% one that maps 
%%     {SecName, ContextEngineId, ContextName} to {CommunityName, Tag}
%% Name -> Index instead of Name -> Vacm allows us to save a 
%% few bytes of memory, although it introduces an extra level of
%% indirection.
%%-----------------------------------------------------------------
community2vacm(Community, Addr) ->
    Idxs = ets:lookup(snmp_community_cache, Community),
    ?vtrace("community2vacm ->~n"
	    "  Community: ~p~n"
	    "  Addr:      ~p~n"
	    "  Idxs:      ~p", [Community, Addr, Idxs]),
    loop_c2v_rows(lists:keysort(2, Idxs), Addr).

loop_c2v_rows([{_, CommunityIndex} | T], Addr) ->
    ?vtrace("loop_c2v_rows -> entry with"
	    "~n   CommunityIndex: ~p", [CommunityIndex]),
    case get_row(CommunityIndex) of
	{_Community, VacmParams, Tag} ->
	    ?vtrace("loop_c2v_rows ->~n"
		    "  VacmParams: ~p~n"
		    "  Tag:        ~p", [VacmParams, Tag]),
	    {TDomain, TAddr} = Addr,
	    case snmp_target_mib:is_valid_tag(Tag, TDomain, TAddr) of
		true ->
		    ?vdebug("loop_c2v_rows -> "
			    "~p valid tag for community index ~p", 
			    [Tag, CommunityIndex]),
		    VacmParams;
		false ->
		    ?vtrace("loop_c2v_rows -> "
			    "~p not valid tag community index ~p", 
			    [Tag, CommunityIndex]),
		    loop_c2v_rows(T, Addr)
	    end;
	undefined ->
	    loop_c2v_rows(T, Addr)
    end;
loop_c2v_rows([], _Addr) ->
    undefined.


%%-----------------------------------------------------------------
%% Func: vacm2community(Vacm, {TDomain, TAddr}) -> 
%%         {ok, Community} | undefined
%% Types: Vacm = {SecName, ContextEngineId, ContextName}
%% Purpose: Follows the steps in RFC 2576 section
%%          5.2.3 in order to find a community string to be used
%%          in a notification.
%%-----------------------------------------------------------------
vacm2community(Vacm, Addr) ->
    Names = ets:lookup(snmp_community_cache, Vacm),
    loop_v2c_rows(lists:keysort(2, Names), Addr).
    
loop_v2c_rows([{_, {CommunityName, Tag}} | T], Addr) ->
    ?vtrace("loop_v2c_rows -> entry with"
	    "~n   CommunityName: ~p"
	    "~n   Tag:           ~p", [CommunityName, Tag]),
    {TDomain, TAddr} = Addr,
    case snmp_target_mib:is_valid_tag(Tag, TDomain, TAddr) of
	true ->
	    ?vdebug("loop_v2c_rows -> "
		    "~p valid tag for community name ~p", 
		    [Tag, CommunityName]),
	    {ok, CommunityName};
	false ->
	    loop_v2c_rows(T, Addr)
    end;
loop_v2c_rows([], _Addr) ->
    undefined.



get_row(RowIndex) ->
    case snmp_generic:table_get_row(db(snmpCommunityTable), RowIndex,
				    foi(snmpCommunityTable)) of
	{_, CommunityName, SecName, ContextEngineId, ContextName,
	 TransportTag, _StorageType, ?'RowStatus_active'} ->
	    {CommunityName, {SecName, ContextEngineId, ContextName}, 
	     TransportTag};
	_ ->
	    undefined
    end.

invalidate_cache(RowIndex) ->
    case get_row(RowIndex) of
	{CommunityName, VacmParams, TransportTag} ->
	    ets:match_delete(snmp_community_cache,
			     {CommunityName, RowIndex}),
	    ets:match_delete(snmp_community_cache,
			     {VacmParams, {CommunityName, TransportTag}});
	undefined ->
	    ok
    end.

update_cache(RowIndex) ->
    case get_row(RowIndex) of
	{CommunityName, VacmParams, TransportTag} ->
	    ets:insert(snmp_community_cache, 
		       {CommunityName, RowIndex}), % CommunityIndex
	    ets:insert(snmp_community_cache, 
		       {VacmParams, {CommunityName, TransportTag}});
	undefined ->
	    ok
    end.

invalidate_cache() ->
    ets:match_delete(snmp_community_cache, {'_', '_'}).


get_target_addr_ext_mms(TDomain, TAddress) ->
    get_target_addr_ext_mms(TDomain, TAddress, []).
get_target_addr_ext_mms(TDomain, TAddress, Key) ->
    case snmp_target_mib:table_next(snmpTargetAddrTable, Key) of
	endOfTable -> 
	    false;
	NextKey -> 
	    case snmp_target_mib:get(
		   snmpTargetAddrTable, NextKey, [?snmpTargetAddrTDomain,
						  ?snmpTargetAddrTAddress,
						  12]) of
		[{value, TDomain}, {value, TAddress}, {value, MMS}] ->
		    {ok, MMS};
		_ ->
		    get_target_addr_ext_mms(TDomain, TAddress, NextKey)
	    end
    end.


%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------
%% Op = print - Used for debugging purposes
snmpCommunityTable(print) ->
    Table = snmpCommunityTable, 
    DB    = db(Table),
    FOI   = foi(Table),
    PrintRow = 
	fun(Prefix, Row) ->
		lists:flatten(
		  io_lib:format("~sIndex:           ~p"
				"~n~sName:            ~p"
				"~n~sSecurityName:    ~p"
				"~n~sContextEngineID: ~p"
				"~n~sContextName:     ~p"
				"~n~sTransportTag:    ~p"
				"~n~sStorageType:     ~p (~w)"
				"~n~sStatus:          ~p (~w)", 
				[Prefix, element(?snmpCommunityIndex, Row),
				 Prefix, element(?snmpCommunityName, Row),
				 Prefix, element(?snmpCommunitySecurityName, Row),
				 Prefix, element(?snmpCommunityContextEngineID, Row),
				 Prefix, element(?snmpCommunityContextName, Row),
				 Prefix, element(?snmpCommunityTransportTag, Row),
				 Prefix, element(?snmpCommunityStorageType, Row),
				 case element(?snmpCommunityStorageType, Row) of
				     ?'snmpCommunityStorageType_readOnly' -> readOnly;
				     ?'snmpCommunityStorageType_permanent' -> permanent;
				     ?'snmpCommunityStorageType_nonVolatile' -> nonVolatile;
				     ?'snmpCommunityStorageType_volatile' -> volatile;
				     ?'snmpCommunityStorageType_other' -> other;
				     _ -> undefined
				 end,
				 Prefix, element(?snmpCommunityStatus, Row),
				 case element(?snmpCommunityStatus, Row) of
				     ?'snmpCommunityStatus_destroy' -> destroy;
				     ?'snmpCommunityStatus_createAndWait' -> createAndWait;
				     ?'snmpCommunityStatus_createAndGo' -> createAndGo;
				     ?'snmpCommunityStatus_notReady' -> notReady;
				     ?'snmpCommunityStatus_notInService' -> notInService;
				     ?'snmpCommunityStatus_active' -> active;
				     _ -> undefined
				 end]))
	end,
    snmpa_mib_lib:print_table(Table, DB, FOI, PrintRow);
%% Op == new | delete
snmpCommunityTable(Op) ->
    snmp_generic:table_func(Op, db(snmpCommunityTable)).

%% Op == get | is_set_ok | set | get_next
snmpCommunityTable(get, RowIndex, Cols) ->
    get(snmpCommunityTable, RowIndex, Cols);
snmpCommunityTable(get_next, RowIndex, Cols) ->
    next(snmpCommunityTable, RowIndex, Cols);
snmpCommunityTable(is_set_ok, RowIndex, Cols0) ->
    case (catch verify_snmpCommunityTable_is_set_ok(Cols0)) of
	{ok, Cols1} ->
	    case (catch verify_snmpCommunityTable_cols(Cols1, [])) of
		{ok, Cols} ->
		    Db = db(snmpCommunityTable),
		    snmp_generic:table_func(is_set_ok, RowIndex, Cols, Db);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end;
snmpCommunityTable(set, RowIndex, Cols0) ->
    case (catch verify_snmpCommunityTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    invalidate_cache(RowIndex),
	    Db  = db(snmpCommunityTable),
	    Res = snmp_generic:table_func(set, RowIndex, Cols, Db),
            snmpa_agent:invalidate_ca_cache(),
	    update_cache(RowIndex),
	    Res;
	Error ->
	    Error
    end;
snmpCommunityTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpCommunityTable)).


verify_snmpCommunityTable_is_set_ok(Cols) ->
    LocalEngineID = snmp_framework_mib:get_engine_id(),
    case lists:keysearch(?snmpCommunityContextEngineID, 1, Cols) of
	{value, {_, LocalEngineID}} -> 
	    {ok, Cols};
	{value, _} -> 
	    {inconsistentValue, ?snmpCommunityContextEngineID};
	false -> 
	    {ok, kinsert(Cols, LocalEngineID)}
    end.

verify_snmpCommunityTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_snmpCommunityTable_cols([{Col, Val0}|Cols], Acc) ->
    Val = verify_snmpCommunityTable_col(Col, Val0),
    verify_snmpCommunityTable_cols(Cols, [{Col, Val}|Acc]).

verify_snmpCommunityTable_col(?snmpCommunityIndex, Index) ->
    case (catch snmp_conf:check_string(Index,{gt,0})) of
	ok ->
	    Index;
	_ ->
	    wrongValue(?snmpCommunityIndex)
    end;
verify_snmpCommunityTable_col(?snmpCommunityName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?snmpCommunityName)
    end;
verify_snmpCommunityTable_col(?snmpCommunitySecurityName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?snmpCommunitySecurityName)
    end;
verify_snmpCommunityTable_col(?snmpCommunityContextName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?snmpCommunityContextName)
    end;
verify_snmpCommunityTable_col(?snmpCommunityTransportTag, Tag) ->
    case (catch snmp_conf:check_string(Tag)) of
	ok ->
	    Tag;
	_ ->
	    wrongValue(?snmpCommunityTransportTag)
    end;
verify_snmpCommunityTable_col(_, Val) ->
    Val.



%% Op == get | is_set_ok | set | get_next
snmpTargetAddrExtTable(get, RowIndex, Cols) ->
    NCols = conv1(Cols),
    get(snmpTargetAddrExtTable, RowIndex, NCols);
snmpTargetAddrExtTable(get_next, RowIndex, Cols) ->
    NCols = conv1(Cols),
    conv2(next(snmpTargetAddrExtTable, RowIndex, NCols));
snmpTargetAddrExtTable(set, RowIndex, Cols0) ->
    case
	(catch verify_snmpTargetAddrExtTable_cols(
		 Cols0,
		 get_snmpTargetAddrTDomain(RowIndex, Cols0),
		 []))
    of
	{ok, Cols} ->
	    NCols = conv3(Cols),
	    snmp_generic:table_func(set, RowIndex, NCols, 
				    db(snmpTargetAddrExtTable));
	Error ->
	    Error
    end;
snmpTargetAddrExtTable(is_set_ok, RowIndex, Cols0) ->
    case (catch verify_snmpTargetAddrExtTable_cols(
		  Cols0,
		  get_snmpTargetAddrTDomain(RowIndex, Cols0),
		  []))
    of
	{ok, Cols} ->
	    NCols = conv3(Cols),
	    snmp_generic:table_func(is_set_ok, RowIndex, NCols, 
				    db(snmpTargetAddrExtTable));
	Error ->
	    Error
    end.


get_snmpTargetAddrTDomain(RowIndex, Col) ->
    Cols = [?snmpTargetAddrRowStatus,?snmpTargetAddrTDomain],
    case snmp_target_mib:snmpTargetAddrTable(get, RowIndex, Cols) of
	[{value, ?snmpTargetAddrRowStatus_active}, {value, TDomain}] ->
            TDomain;
	[{value, ?snmpTargetAddrRowStatus_active}, _] ->
            ?snmpUDPDomain;
        _ ->
	    wrongValue(Col)
    end.


verify_snmpTargetAddrExtTable_cols([], _TDomain, Cols) ->
    {ok, lists:reverse(Cols)};
verify_snmpTargetAddrExtTable_cols([{Col, Val0}|Cols], TDomain, Acc) ->
    Val = verify_snmpTargetAddrExtTable_col(Col, TDomain, Val0),
    verify_snmpTargetAddrExtTable_cols(Cols, TDomain, [{Col, Val}|Acc]).

verify_snmpTargetAddrExtTable_col(?snmpTargetAddrTMask, _TDomain, []) ->
    [];
verify_snmpTargetAddrExtTable_col(?snmpTargetAddrTMask, TDomain, TMask) ->
    case (catch snmp_conf:check_taddress(TDomain, TMask)) of
	ok ->
	    TMask; 
	_ ->
	    wrongValue(?snmpTargetAddrTMask)
    end;
verify_snmpTargetAddrExtTable_col(?snmpTargetAddrMMS, _TDomain, MMS) ->
    case (catch snmp_conf:check_packet_size(MMS)) of
	ok ->
	    MMS;
	_ ->
	    wrongValue(?snmpTargetAddrMMS)
    end;
verify_snmpTargetAddrExtTable_col(_, _TDomain, Val) ->
    Val.

db(snmpTargetAddrExtTable) -> db(snmpTargetAddrTable);
db(X) -> snmpa_agent:db(X).

fa(snmpCommunityTable) -> ?snmpCommunityName;
fa(snmpTargetAddrExtTable) -> 11.
 
foi(snmpCommunityTable) -> ?snmpCommunityIndex;
foi(snmpTargetAddrExtTable) -> 11.
 
noc(snmpCommunityTable) -> 8;
noc(snmpTargetAddrExtTable) -> 12.

stc(snmpCommunityTable) -> ?snmpCommunityStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

conv1([Col | T]) -> [Col + 10 | conv1(T)];
conv1([]) -> [].
     

conv2([{[Col | Oid], Val} | T]) ->
    [{[Col - 10 | Oid], Val} | conv2(T)];
conv2([X | T]) ->
    [X | conv2(T)];
conv2(X) -> X.


conv3([{Idx, Val}|T]) -> [{Idx+10, Val} | conv3(T)];
conv3([]) -> [].



get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

kinsert([H | T], EngineID) when element(1, H) < ?snmpCommunityContextEngineID ->
    [H | kinsert(T, EngineID)];
kinsert(Cols, EngineID) ->
    [{?snmpCommunityContextEngineID, EngineID} | Cols].


wrongValue(V) -> throw({wrongValue, V}).


%% -----

set_sname() ->
    set_sname(get(sname)).

set_sname(undefined) ->
    put(sname,conf);
set_sname(_) -> %% Keep it, if already set.
    ok.


error(Reason) ->
    throw({error, Reason}).

warning_msg(F, A) ->
    ?snmpa_warning("[COMMUNITY-MIB]: " ++ F, A).

config_err(F, A) ->
    snmpa_error:config_err("[COMMUNITY-MIB]: " ++ F, A).
