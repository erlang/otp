%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2019. All Rights Reserved.
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
-module(snmp_target_mib).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([configure/1, reconfigure/1,
	 snmpTargetSpinLock/1, snmpTargetSpinLock/2,
	 snmpTargetAddrTable/1, snmpTargetAddrTable/3,
	 snmpTargetParamsTable/1, snmpTargetParamsTable/3,
	 get_target_addrs/0, get_target_engine_id/1, set_target_engine_id/2,
	 is_valid_tag/3, get/3, table_next/2]).
-export([add_addr/10, add_addr/11, delete_addr/1,
	 add_params/5, delete_params/1]).
-export([check_target_addr/1, check_target_params/1]).
-export([default_domain/0]).

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/include/snmp_tables.hrl").
-include_lib("snmp/include/SNMP-TARGET-MIB.hrl").
-include_lib("snmp/include/SNMPv2-TC.hrl").
-include_lib("snmp/include/SNMPv2-TM.hrl").
-include_lib("snmp/include/SNMP-FRAMEWORK-MIB.hrl").
-include_lib("snmp/include/TRANSPORT-ADDRESS-MIB.hrl").

-define(VMODULE,"TARGET-MIB").
-include("snmp_verbosity.hrl").
-include("snmpa_internal.hrl").


%% Column not accessible via SNMP - needed when the agent sends informs
-define(snmpTargetAddrEngineId, 10).
%% Extra comlumns for the augmented table snmpTargetAddrExtTable
-define(snmpTargetAddrTMask,    11).
-define(snmpTargetAddrMMS,      12).

-ifdef(snmp_extended_verbosity).
-define(vt(F,A), ?vtrace(F, A)).
-else.
-define(vt(_F, _A), ok).
-endif.


%%-----------------------------------------------------------------

default_domain() ->
    snmpUDPDomain.


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the target tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case db(snmpTargetParamsTable) of
        {_, mnesia} ->
            ?vdebug("tables in mnesia: init vars & cleanup",[]),
            init_vars(),
            gc_tabs();
	TabDb ->
	    case snmpa_local_db:table_exists(TabDb) of
		true ->
		    ?vdebug("tables already exist: init vars & cleanup",[]),
		    init_vars(),
		    gc_tabs();
		false ->
		    ?vdebug("no tables: reconfigure",[]),
		    reconfigure(Dir)
	    end
    end.


%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the target tables, and
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
    ?vdebug("read target configuration files",[]),
    {Addrs, Params} = read_target_config_files(Dir),
    ?vdebug("initiate tables",[]),
    init_tabs(Addrs, Params),
    ?vdebug("initiate vars",[]),
    init_vars(),
    ?vdebug("invalidate cache for notification mib",[]),
    snmp_notification_mib:invalidate_cache(),
    ok.


read_target_config_files(Dir) ->
    ?vdebug("check target address and parameter config file(s)",[]),

    TAName   = "target_addr.conf",
    TACheck  = fun (Entry, State) -> {check_target_addr(Entry), State} end,

    TPName   = "target_params.conf",
    TPCheck  = fun (Entry, State) -> {check_target_params(Entry), State} end,

    NoGen    = fun snmp_conf:no_gen/2,
    NoOrder  = fun snmp_conf:no_order/2,
    NoFilter = fun snmp_conf:no_filter/1,

    [Addrs, Params] = 
        snmp_conf:read_files(
	  Dir,
	  [{TAName, NoGen, NoOrder, TACheck, NoFilter},
	   {TPName, NoGen, NoOrder, TPCheck, NoFilter}]),
    {Addrs, Params}.


%%-----------------------------------------------------------------
%%  TargetAddr
%%  {Name, Ip, Udp, Timeout, RetryCount, TagList, Params, EngineId,
%%   TMask, MMS}
%%-----------------------------------------------------------------

check_target_addr(
  {Name, Domain, Ip, Udp, Timeout, RetryCount, TagList, Params,
   EngineId, TMask, MMS}) -> % Arity 11
    Address = {Ip, Udp},
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      EngineId, TMask, MMS);
check_target_addr(
  {Name, Domain, Address, Timeout, RetryCount, TagList, Params,
   EngineId, TMask, MMS}) % Arity 10
  when is_atom(Domain) ->
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      EngineId, TMask, MMS);
check_target_addr(
  {Name, Ip, Udp, Timeout, RetryCount, TagList, Params,
   EngineId, TMask, MMS})
  when is_integer(Udp) -> % Arity 10
    Domain = default_domain(),
    Address = {Ip, Udp},
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      EngineId, TMask, MMS);
check_target_addr(
  {_Name, Domain, Address, _Timeout, _RetryCount, _TagList, _Params,
   _EngineId, _TMask, _MMS}) -> % Arity 10
    error({bad_address, {Domain, Address}});
check_target_addr(
  {Name, Domain, Address, Timeout, RetryCount, TagList, Params,
   EngineId}) % Arity 8
  when is_atom(Domain) ->
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      EngineId);
check_target_addr(
  {Name, Ip, Udp, Timeout, RetryCount, TagList, Params,
   EngineId}) % Arity 8
  when is_integer(Udp) ->
    Domain = default_domain(),
    Address = {Ip, Udp},
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      EngineId);
check_target_addr(
  {_Name, Domain, Address, _Timeout, _RetryCount, _TagList, _Params,
   _EngineId}) ->% Arity 8
    error({bad_address, {Domain, Address}});
%% Use dummy engine id if the old style is found
check_target_addr(
  {Name, Domain, Address, Timeout, RetryCount, TagList, Params}) % Arity 7
  when is_atom(Domain) ->
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params);
check_target_addr(
  {Name, Ip, Udp, Timeout, RetryCount, TagList, Params})  % Arity 7
  when is_integer(Udp) ->
    Domain = default_domain(),
    Address = {Ip, Udp},
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params);
check_target_addr(
  {_Name, Domain, Address, _Timeout, _RetryCount, _TagList, _Params}) -> % Arity 7
    error({bad_address, {Domain, Address}});
%% Use dummy engine id if the old style is found
check_target_addr(
  {Name, Domain, Address, Timeout, RetryCount, TagList, Params,
   TMask, MMS}) % Arity 9
  when is_atom(Domain) ->
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params, TMask, MMS);
check_target_addr(
  {Name, Ip, Udp, Timeout, RetryCount, TagList, Params,
   TMask, MMS}) % Arity 9
  when is_integer(Udp) ->
    Domain = default_domain(),
    Address = {Ip, Udp},
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params, TMask, MMS);
check_target_addr(
  {_Name, Domain, Address, _Timeout, _RetryCount, _TagList, _Params,
   _TMask, _MMS}) -> % Arity 9
    error({bad_address, {Domain, Address}});
check_target_addr(X) ->
    error({invalid_target_addr, X}).

check_target_addr(
  Name, Domain, Address, Timeout, RetryCount, TagList, Params) -> % Arity 7
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      "dummy").
%%
check_target_addr(
  Name, Domain, Address, Timeout, RetryCount, TagList, Params,
  EngineId) -> % Arity 8
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      EngineId, [], 2048).
%%
check_target_addr(
  Name, Domain, Address, Timeout, RetryCount, TagList, Params,
  TMask, MMS) -> % Arity 9
    check_target_addr(
      Name, Domain, Address, Timeout, RetryCount, TagList, Params,
      "dummy", TMask, MMS).
%%
check_target_addr(
  Name, Domain, Address, Timeout, RetryCount, TagList, Params,
  EngineId, Mask, MMS) -> % Arity 10
    ?vtrace("check target address with:"
	    "~n   Name:       ~s"
	    "~n   Domain:     ~p"
	    "~n   Address:    ~p"
	    "~n   Timeout:    ~p"
	    "~n   RetryCount: ~p"
	    "~n   TagList:    ~p"
	    "~n   Params:     ~p"
	    "~n   EngineId:   ~p"
	    "~n   Mask:      ~p"
	    "~n   MMS:        ~p",
	    [Name, Domain, Address,
	     Timeout, RetryCount,
	     TagList, Params, EngineId, Mask, MMS]),
    snmp_conf:check_string(Name,{gt,0}),
    snmp_conf:check_domain(Domain),
    NAddress = check_address(Domain, Address),
    snmp_conf:check_integer(Timeout, {gte, 0}),
    snmp_conf:check_integer(RetryCount, {gte,0}),
    snmp_conf:check_string(TagList),
    snmp_conf:check_string(Params),
    check_engine_id(EngineId),
    NMask = check_mask(Domain, Mask),
    TDomain  = snmp_conf:mk_tdomain(Domain), 
    TAddress = snmp_conf:mk_taddress(Domain, NAddress),
    TMask    = snmp_conf:mk_taddress(Domain, NMask),
    snmp_conf:check_packet_size(MMS),
    ?vtrace("check target address done",[]),
    Addr = {Name, TDomain, TAddress, Timeout,
	    RetryCount, TagList, Params,
	    ?'StorageType_nonVolatile', ?'RowStatus_active', EngineId,
            TMask, MMS}, % Values for Augmenting table in SNMP-COMMUNITY-MIB
    {ok, Addr}.

check_engine_id(discovery) ->
    ok;
check_engine_id(EngineId) ->
    snmp_conf:check_string(EngineId).

check_address(Domain, Address) ->
    case snmp_conf:check_address(Domain, Address, 162) of
	ok ->
	    Address;
	{ok, NAddress} ->
	    NAddress
    end.

check_mask(_Domain, [] = Mask) ->
    Mask;
check_mask(Domain, Mask) ->
    try snmp_conf:check_address(Domain, Mask) of
	ok ->
	    Mask;
	{ok, NMask} ->
	    NMask
    catch
	{error, {bad_address, Info}} ->
	    error({bad_mask, Info})
    end.


%%-----------------------------------------------------------------
%%  TargetParams
%%  {Name, MPModel, SecurityModel, SecurityName, SecurityLevel}
%%-----------------------------------------------------------------
check_target_params({Name, MPModel, SecModel, SecName, SecLevel}) ->
    snmp_conf:check_string(Name,{gt,0}),
    {ok, MP} = snmp_conf:check_atom(MPModel, [{v1,  ?MP_V1},
					      {v2c, ?MP_V2C},
					      {v3,  ?MP_V3}]),
    {ok, SecM} = snmp_conf:check_sec_model(SecModel, [any]),
    snmp_conf:check_string(SecName),
    {ok, SecL} = snmp_conf:check_sec_level(SecLevel),
    Params = {Name, MP, SecM, SecName, SecL, 
	      ?'StorageType_nonVolatile', ?'RowStatus_active'},
    {ok, Params};
check_target_params(X) ->
    error({invalid_target_params, X}).


init_tabs(Addrs, Params) ->
    ?vdebug("create target address table",[]),
    AddrDB = db(snmpTargetAddrTable),
    snmpa_local_db:table_delete(AddrDB),
    snmpa_local_db:table_create(AddrDB),
    init_addr_table(Addrs),
    ?vdebug("create target params table",[]),
    ParmDB = db(snmpTargetParamsTable),
    snmpa_local_db:table_delete(ParmDB),
    snmpa_local_db:table_create(ParmDB),
    init_params_table(Params).

init_addr_table([Row | T]) ->
    Key = element(1, Row),
    snmpa_local_db:table_create_row(db(snmpTargetAddrTable), Key, Row),
    init_addr_table(T);
init_addr_table([]) -> true.

init_params_table([Row | T]) ->
    Key = element(1, Row),
    snmpa_local_db:table_create_row(db(snmpTargetParamsTable), Key, Row),
    init_params_table(T);
init_params_table([]) -> true.

table_cre_row(Tab, Key, Row) ->
    snmpa_mib_lib:table_cre_row(db(Tab), Key, Row).

table_del_row(Tab, Key) ->
    snmpa_mib_lib:table_del_row(db(Tab), Key).


add_addr(
  Name, Domain_or_Ip, Addr_or_Port, Timeout, Retry, TagList, Params,
  EngineId, TMask, MMS) ->
    add_addr(
      {Name, Domain_or_Ip, Addr_or_Port, Timeout, Retry, TagList, Params,
       EngineId, TMask, MMS}).
%%
add_addr(
  Name, Domain, Ip, Port, Timeout, Retry, TagList, Params,
  EngineId, TMask, MMS) ->
    add_addr(
      {Name, Domain, Ip, Port, Timeout, Retry, TagList, Params,
       EngineId, TMask, MMS}).
%%
add_addr(Addr) ->
    case (catch check_target_addr(Addr)) of
	{ok, Row} ->
	    Key = element(1, Row),
	    case table_cre_row(snmpTargetAddrTable, Key, Row) of
		true ->
		    {ok, Key};
		false ->
		    {error, create_failed}
	    end;
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    {error, Error}
    end.

delete_addr(Key) ->
    case table_del_row(snmpTargetAddrTable, Key) of
	true ->
	    ok;
	false ->
	    {error, delete_failed}
    end.


add_params(Name, MPModel, SecModel, SecName, SecLevel) ->
    Params = {Name, MPModel, SecModel, SecName, SecLevel},
    case (catch check_target_params(Params)) of
	{ok, Row} ->
	    Key = element(1, Row),
	    case table_cre_row(snmpTargetParamsTable, Key, Row) of
		true ->
		    {ok, Key};
		false ->
		    {create_failed}
	    end;
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    {error, Error}
    end.

delete_params(Key) ->
    case table_del_row(snmpTargetParamsTable, Key) of
	true ->
	    ok;
	false ->
	    {error, delete_failed}
    end.

    
gc_tabs() ->
    AddrDB  = db(snmpTargetAddrTable),
    AddrSTC = stc(snmpTargetAddrTable),
    AddrFOI = foi(snmpTargetAddrTable),
    snmpa_mib_lib:gc_tab(AddrDB, AddrSTC, AddrFOI),
    ParmDB  = db(snmpTargetParamsTable),
    ParmSTC = stc(snmpTargetParamsTable),
    ParmFOI = foi(snmpTargetParamsTable),
    snmpa_mib_lib:gc_tab(ParmDB, ParmSTC, ParmFOI),
    ok.


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_vars() -> 
    ?vtrace("initiate vars",[]),
    lists:map(fun maybe_create_var/1, vars()).

maybe_create_var(Var) ->
    case ets:lookup(snmp_agent_table, Var) of
	[_] -> ok;
	_ -> init_var(Var)
    end.

init_var(Var) -> ets:insert(snmp_agent_table, {Var, 0}).

vars() ->
    [
     snmpUnavailableContexts,
     snmpUnknownContexts
    ].


%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
is_valid_tag("", _TDomain, _TAddress) ->
    true;
is_valid_tag(Tag, TDomain, TAddress) ->
    is_valid_tag(TDomain, TAddress, Tag, []).

is_valid_tag(TDomain, TAddress, Tag, Key) ->
    ?vtrace("is_valid_tag -> entry with"
	    "~n   TDomain:  ~p"
	    "~n   TAddress: ~p"
	    "~n   Tag:      ~p"
	    "~n   Key:      ~p", [TDomain, TAddress, Tag, Key]),
    case table_next(snmpTargetAddrTable, Key) of
	endOfTable -> 
	    ?vtrace("is_valid_tag -> endOfTable", []),
	    false;
	NextKey -> 
	    ?vtrace("is_valid_tag -> next key found"
		    "~n   NextKey: ~p", [NextKey]),
	    case get(snmpTargetAddrTable, NextKey, [?snmpTargetAddrTDomain,
						    ?snmpTargetAddrTAddress,
						    ?snmpTargetAddrTagList,
						    ?snmpTargetAddrTMask]) of
		[{value, TDomain},  % must match exactly
		 {value, TAddress}, % RFC2576: chapters 5.2.1 & 5.3
		 {value, TagList}, 
		 {value, []}] ->
		    ?vtrace("is_valid_tag -> found with exact match"
			    "~n   TagList: ~p", [TagList]),
		    case snmp_misc:is_tag_member(Tag, TagList) of
			true ->
			    ?vtrace("is_valid_tag -> exact: "
				    "tag IS member of taglist", []),
			    true;
			false ->
			    ?vtrace("is_valid_tag -> exact: "
				    "tag is NOT member of taglist", []),
			    is_valid_tag(TDomain, TAddress,
					 Tag, NextKey)
		    end;
		[{value, TDomain},   % must match exactly
		 {value, TAddress2},
		 {value, TagList}, 
		 {value, TMask}] when TMask =/= [] ->
		    ?vtrace("is_valid_tag -> found with exact match"
			    "~n   TagList: ~p"
			    "~n   TMask:   ~p", [TagList, TMask]),
		    case snmp_misc:is_tmask_match(TAddress, TAddress2, 
						  TMask) of
			true ->
			    ?vtrace("is_valid_tag -> "
				    "tmask match - now check tag member", []),
			    case snmp_misc:is_tag_member(Tag, TagList) of
				true ->
				    ?vtrace("is_valid_tag -> masked: "
					    "tag IS member of taglist", []),
				    true;
				false ->
				    ?vtrace("is_valid_tag -> masked: "
					    "tag is NOT member of taglist",[]),
				    is_valid_tag(TDomain, TAddress,
						 Tag, NextKey)
			    end;
			false ->
			    ?vtrace("is_valid_tag -> tmask NO match", []),
			    is_valid_tag(TDomain, TAddress,
					 Tag, NextKey)
		    end;
		_ ->
		    ?vtrace("is_valid_tag -> not found - try next", []),
		    is_valid_tag(TDomain, TAddress, Tag, NextKey)
	    end
    end.
    

%% TargAddrs =
%%     [{TagList, TargetAddr, TargetAddrName, TargetParams, Timeout, Retry}]
%%   TargetAddr = {TDomain, TAddr}; e.g. {?snmpUDPDomain, IpAndUdpPortAsList}
get_target_addrs() ->
    get_target_addrs([], db(snmpTargetAddrTable), []).


get_target_addrs(Key, {Tab, _} = TabDb, Acc) ->
    ?vt("get_target_addrs -> entry with"
	"~n   Key: ~p", [Key]),
    case table_next(Tab, Key) of
	endOfTable -> 
	    ?vt("get_target_addrs -> endOfTable when"
		"~n   Acc: ~p", [Acc]),
	    Acc;
	NextKey ->
	    ?vt("get_target_addrs -> next ok: "
		"~n   NextKey: ~p", [NextKey]),
	    case get_target_addr(TabDb, NextKey) of
		{ok, Targ} ->
		    get_target_addrs(NextKey, TabDb, [Targ| Acc]);
		{error, Reason} ->
		    ?vlog("failed getting target address: "
			  "~n   Reason: ~p", [Reason]),
		    get_target_addrs(NextKey, TabDb, Acc)
	    end
    end.

get_target_addr({Tab, mnesia}, Key) ->
    case mnesia:snmp_get_row(Tab, Key) of
	{ok, #snmpTargetAddrTable{
	   snmpTargetAddrTDomain    = TDomain,
	   snmpTargetAddrTAddress   = TAddress,
	   snmpTargetAddrTimeout    = Timeout,
	   snmpTargetAddrRetryCount = RetryCount,
	   snmpTargetAddrTagList    = TagList,
	   snmpTargetAddrParams     = Params,
	   snmpTargetAddrRowStatus  = ?'RowStatus_active'}} ->
	    case get_target_params(Params) of
		undefined ->
		    config_err("Failed retreiving target params [~p]"
			       "~n   for ~p [~p]", [Params, Key, TAddress]),
		    {error, {not_found, {target_params, Key, Params}}};
		TargetParams ->
		    Targ = {TagList, {TDomain, TAddress}, Key,
			    TargetParams, Timeout, RetryCount},
		    {ok, Targ}
	    end;
	_ ->
	    {error, {not_found, {target_addr, Key}}}
    end;
get_target_addr(TabDb, Key) ->
    case snmpa_local_db:table_get_row(TabDb, Key) of
	{_Key, TDomain, TAddress, Timeout, RetryCount, TagList, Params,
	 _Storage, ?'RowStatus_active', _TargetEngineId,_TMask,_MMS} ->
	    case get_target_params(Params) of
		undefined ->
		    config_err("Failed retreiving target params [~p]"
			       "~n   for target ~p [~p]", 
			       [Params, Key, TAddress]),
		    {error, {not_found, {target_params, Key, Params}}};
		TargetParams ->
		    Targ = {TagList, {TDomain, TAddress}, Key,
			    TargetParams, Timeout, RetryCount},
		    {ok, Targ}
	    end;
	_ ->
	    {error, {not_found, {target_addr, Key}}}
     end.
    

get_target_params(Params) ->
    case snmpTargetParamsTable(get, Params, [?snmpTargetParamsMPModel,
					     ?snmpTargetParamsSecurityModel,
					     ?snmpTargetParamsSecurityName,
					     ?snmpTargetParamsSecurityLevel,
					     ?snmpTargetParamsRowStatus]) of
	[{value, MpModel}, 
	 {value, SecModel}, {value, SecName}, {value, SecLevel},
	 {value, ?'RowStatus_active'}] ->
	    {MpModel, SecModel, SecName, SecLevel};
	_ ->
	    undefined
    end.

get_target_engine_id(TargetAddrName) ->
    case db(snmpTargetAddrTable) of
        {_, mnesia} ->
            case mnesia:snmp_get_row(snmpTargetAddrTable, TargetAddrName) of
                {ok, T} -> 
		    {ok, T#snmpTargetAddrTable.snmpTargetAddrEngineId};
                _ -> 
		    undefined
            end;
        TabDb ->
	    case snmpa_local_db:table_get_element(TabDb,
						  TargetAddrName,
						  ?snmpTargetAddrEngineId) of
		{value, Val} -> 
		    {ok, Val};
		_ ->
		    undefined
	    end
    end.
				    
set_target_engine_id(TargetAddrName, EngineId) ->
    snmp_generic:table_set_elements(db(snmpTargetAddrTable),
				    TargetAddrName,
				    [{?snmpTargetAddrEngineId, EngineId}]).

%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------
snmpTargetSpinLock(print) ->
    VarAndValue = [{snmpTargetSpinLock, snmpTargetSpinLock(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
    
snmpTargetSpinLock(new) ->
    snmp_generic:variable_func(new, {snmpTargetSpinLock, volatile}),
    ?SNMP_RAND_SEED(),
    %% rand:seed(exrop,
    %%           {erlang:phash2([node()]),
    %%            erlang:monotonic_time(),
    %%            erlang:unique_integer()}),
    Val = rand:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {snmpTargetSpinLock, volatile});

snmpTargetSpinLock(delete) ->
    ok;

snmpTargetSpinLock(get) ->
    snmp_generic:variable_func(get, {snmpTargetSpinLock, volatile}).

snmpTargetSpinLock(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {snmpTargetSpinLock, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
snmpTargetSpinLock(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {snmpTargetSpinLock, volatile}).


%% Op = print - Used for debugging purposes
snmpTargetAddrTable(print) ->
    Table = snmpTargetAddrTable, 
    DB    = db(Table),
    FOI   = foi(Table),
    PrintRow = 
	fun(Prefix, Row) ->
		TDomain = element(?snmpTargetAddrTDomain, Row),
		Domain =
		    try snmp_conf:tdomain_to_domain(TDomain)
		    catch
			{error, {bad_tdomain, _}} ->
			    undefined
		    end,
		TAddress = element(?snmpTargetAddrTAddress, Row),
		AddrString =
		    try snmp_conf:mk_addr_string({Domain, TAddress})
		    catch
			{error, {bad_address, _}} ->
			    "-"
		    end,
		lists:flatten(
		  io_lib:format("~sName:              ~p"
				"~n~sTDomain:           ~p (~w)"
				"~n~sTAddress:          ~p (~s)"
				"~n~sTimeout:           ~p"
				"~n~sRetryCount:        ~p"
				"~n~sTagList:           ~p"
				"~n~sParams:            ~p"
				"~n~sStorageType:       ~p (~w)"
				"~n~sStatus:            ~p (~w)"
				"~n~s[NonAcc] EngineID: ~p"
				"~n~s[Ext] TMask:       ~p"
				"~n~s[Ext] MMS:         ~p", 
				[Prefix, element(?snmpTargetAddrName, Row),
				 Prefix, TDomain, Domain,
				 Prefix, TAddress, AddrString,
				 Prefix, element(?snmpTargetAddrTimeout, Row),
				 Prefix, element(?snmpTargetAddrRetryCount, Row),
				 Prefix, element(?snmpTargetAddrTagList, Row),
				 Prefix, element(?snmpTargetAddrParams, Row),
				 Prefix, element(?snmpTargetAddrStorageType, Row),
				 case element(?snmpTargetAddrStorageType, Row) of
				     ?'snmpTargetAddrStorageType_readOnly' -> readOnly;
				     ?'snmpTargetAddrStorageType_permanent' -> permanent;
				     ?'snmpTargetAddrStorageType_nonVolatile' -> nonVolatile;
				     ?'snmpTargetAddrStorageType_volatile' -> volatile;
				     ?'snmpTargetAddrStorageType_other' -> other;
				     _ -> undefined
				 end,
				 Prefix, element(?snmpTargetAddrRowStatus, Row),
				 case element(?snmpTargetAddrRowStatus, Row) of
				     ?'snmpTargetAddrRowStatus_destroy' -> destroy;
				     ?'snmpTargetAddrRowStatus_createAndWait' -> createAndWait;
				     ?'snmpTargetAddrRowStatus_createAndGo' -> createAndGo;
				     ?'snmpTargetAddrRowStatus_notReady' -> notReady;
				     ?'snmpTargetAddrRowStatus_notInService' -> notInService;
				     ?'snmpTargetAddrRowStatus_active' -> active;
				     _ -> undefined
				 end,
				 Prefix, element(?snmpTargetAddrEngineId, Row),
				 Prefix, element(?snmpTargetAddrTMask, Row),
				 Prefix, element(?snmpTargetAddrMMS, Row)]))
	end,
    snmpa_mib_lib:print_table(Table, DB, FOI, PrintRow);
%% Op == new | delete
snmpTargetAddrTable(Op) ->
    snmp_generic:table_func(Op, db(snmpTargetAddrTable)).

%% Op == get | is_set_ok | set | get_next
snmpTargetAddrTable(get, RowIndex, Cols) ->
    get(snmpTargetAddrTable, RowIndex, Cols);
snmpTargetAddrTable(get_next, RowIndex, Cols) ->
    next(snmpTargetAddrTable, RowIndex, Cols);
snmpTargetAddrTable(set, RowIndex, Cols0) ->
    %% BMK BMK
    case (catch verify_targetAddrTable_cols(Cols0)) of
	{ok, Cols} ->
	    snmp_notification_mib:invalidate_cache(),
	    %% Add columns for augmenting table snmpTargetAddrExtTable and for
	    %% target engine ID. Target engine ID is set to "". The function
	    %% get_target_engine_id will return "" unless a value is set using
	    %% set_target_engine_id. If it is "" Informs can't be sent to the
	    %% target.
	    NCols = Cols ++ [{?snmpTargetAddrEngineId, ""},
			     {?snmpTargetAddrTMask, []},
			     {?snmpTargetAddrMMS, 2048}],
	    Db = db(snmpTargetAddrTable), 
	    snmp_generic:table_func(set, RowIndex, NCols, Db);
	Error ->
	    Error
    end;
snmpTargetAddrTable(is_set_ok, RowIndex, Cols0) ->
    case (catch verify_targetAddrTable_cols(Cols0)) of
	{ok, Cols} ->
	    %% Add columns for augmenting table snmpTargetAddrExtTable and for
	    %% target engine ID. Target engine ID is set to "". The function
	    %% get_target_engine_id will return "" unless a value is set using
	    %% set_target_engine_id. If it is "" Informs can't be sent to the
	    %% target.
	    NCols = Cols ++ [{?snmpTargetAddrEngineId, ""},
			     {?snmpTargetAddrTMask, []},
			     {?snmpTargetAddrMMS, 2048}],
	    Db = db(snmpTargetAddrTable), 
	    snmp_generic:table_func(is_set_ok, RowIndex, NCols, Db);
	Error ->
	    Error
    end;
snmpTargetAddrTable(Op, Arg1, Arg2) ->
    Db = db(snmpTargetAddrTable), 
    snmp_generic:table_func(Op, Arg1, Arg2, Db).

verify_targetAddrTable_cols(Cols) ->
    ValidCols0 = verify_targetAddrTable_cols(Cols, []),
    %% Make a last pass to verify TDomain and TAddress.
    ValidCols0.

verify_targetAddrTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_targetAddrTable_cols([{Col, Val0}|Cols], ValidCols) ->
    Val = verify_targetAddrTable_col(Col, Val0, ValidCols),
    verify_targetAddrTable_cols(Cols, [{Col, Val}|ValidCols]).

verify_targetAddrTable_col(?snmpTargetAddrName, Name, _) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?snmpTargetAddrName)
    end;
verify_targetAddrTable_col(?snmpTargetAddrTDomain, TDomain, _) ->
    case (catch snmp_conf:check_tdomain(TDomain)) of
	ok ->
	    TDomain;
	_ ->
	    wrongValue(?snmpTargetAddrTDomain)
    end;
%% In order to (properly) validate the TAddress, 
%% the TDomain must already have been validated 
%% (the format of TAddress depends on TDomain).
verify_targetAddrTable_col(?snmpTargetAddrTAddress, TAddress, ValidCols) ->
    case lists:keysearch(?snmpTargetAddrTDomain, 1, ValidCols) of
	{value, {?snmpTargetAddrTDomain, TDomain}} ->
	    case (catch snmp_conf:check_taddress(TDomain, TAddress)) of
		ok ->
		    TAddress;
		_ ->
		    wrongValue(?snmpTargetAddrTAddress)
	    end;
	false ->
	    %% The user did not provide us with a TDomain, which 
	    %% must mean that he/she intends to use the old domain. 
	    TDomain = snmp_conf:mk_tdomain(default_domain()),
	    case (catch snmp_conf:check_taddress(TDomain, TAddress)) of
		ok ->
		    TAddress;
		_ ->
		    wrongValue(?snmpTargetAddrTAddress)
	    end	    
    end;
verify_targetAddrTable_col(?snmpTargetAddrTimeout, Timeout, _) ->
    case (catch snmp_conf:check_integer(Timeout)) of
	ok when Timeout >= 0 ->
	    Timeout;
	_ ->
	    wrongValue(?snmpTargetAddrTimeout)
    end;
verify_targetAddrTable_col(?snmpTargetAddrRetryCount, Retry, _) ->
    case (catch snmp_conf:check_integer(Retry)) of
	ok when Retry >= 0 ->
	    Retry;
	_ ->
	    wrongValue(?snmpTargetAddrRetryCount)
    end;
verify_targetAddrTable_col(?snmpTargetAddrTagList, TagList, _) ->
    case (catch snmp_conf:check_string(TagList)) of
	ok ->
	    TagList;
	_ ->
	    wrongValue(?snmpTargetAddrTagList)
    end;
verify_targetAddrTable_col(?snmpTargetAddrParams, Params, _) ->
    case (catch snmp_conf:check_string(Params)) of
	ok ->
	    Params;	
	_ ->
	    wrongValue(?snmpTargetAddrParams)
    end;
verify_targetAddrTable_col(_, Val, _) ->
    Val.
    

%% Op = print - Used for debugging purposes
snmpTargetParamsTable(print) ->
    Table = snmpTargetParamsTable, 
    DB    = db(Table),
    FOI   = foi(Table),
    PrintRow = 
	fun(Prefix, Row) ->
		lists:flatten(
		  io_lib:format("~sName:          ~p"
				"~n~sMPModel:       ~p (~w)"
				"~n~sSecurityModel: ~p (~w)"
				"~n~sSecurityName:  ~p"
				"~n~sSecurityLevel: ~p (~w)"
				"~n~sStorageType:   ~p (~w)"
				"~n~sStatus:        ~p (~w)", 
				[Prefix, 
				 element(?snmpTargetParamsName, Row),
				 Prefix, 
				 element(?snmpTargetParamsMPModel, Row),
				 case element(?snmpTargetParamsMPModel, Row) of
				     ?MP_V1 -> v1;
				     ?MP_V2C -> v2c;
				     ?MP_V3 -> v3;
				     _ -> undefined
				 end,
				 Prefix, 
				 element(?snmpTargetParamsSecurityModel, Row),
				 case element(?snmpTargetParamsSecurityModel, Row) of
				     ?SEC_ANY -> any;
				     ?SEC_V1 -> v1;
				     ?SEC_V2C -> v2c;
				     ?SEC_USM -> usm;
				     _ -> undefined
				 end,
				 Prefix, 
				 element(?snmpTargetParamsSecurityName, Row),
				 Prefix, 
				 element(?snmpTargetParamsSecurityLevel, Row),
				 case element(?snmpTargetParamsSecurityLevel, Row) of
				     ?'SnmpSecurityLevel_noAuthNoPriv' -> noAuthNoPriv;
				     ?'SnmpSecurityLevel_authNoPriv' -> authNoPriv;
				     ?'SnmpSecurityLevel_authPriv' -> authPriv;
				     _ -> undefined
				 end,
				 Prefix, 
				 element(?snmpTargetParamsStorageType, Row),
				 case element(?snmpTargetParamsStorageType, Row) of
				     ?'snmpTargetParamsStorageType_readOnly' -> readOnly;
				     ?'snmpTargetParamsStorageType_permanent' -> permanent;
				     ?'snmpTargetParamsStorageType_nonVolatile' -> nonVolatile;
				     ?'snmpTargetParamsStorageType_volatile' -> volatile;
				     ?'snmpTargetParamsStorageType_other' -> other;
				     _ -> undefined
				 end,
				 Prefix, 
				 element(?snmpTargetParamsRowStatus, Row),
				 case element(?snmpTargetParamsRowStatus, Row) of
				     ?'snmpTargetParamsRowStatus_destroy' -> destroy;
				     ?'snmpTargetParamsRowStatus_createAndWait' -> createAndWait;
				     ?'snmpTargetParamsRowStatus_createAndGo' -> createAndGo;
				     ?'snmpTargetParamsRowStatus_notReady' -> notReady;
				     ?'snmpTargetParamsRowStatus_notInService' -> notInService;
				     ?'snmpTargetParamsRowStatus_active' -> active;
				     _ -> undefined
				 end]))
	end,
    snmpa_mib_lib:print_table(Table, DB, FOI, PrintRow);
%% Op == new | delete
snmpTargetParamsTable(Op) ->
    snmp_generic:table_func(Op, db(snmpTargetParamsTable)).

%% Op == get | is_set_ok | set | get_next
snmpTargetParamsTable(get, RowIndex, Cols) ->
    %% BMK BMK BMK
    get(snmpTargetParamsTable, RowIndex, Cols);
snmpTargetParamsTable(get_next, RowIndex, Cols) ->
    %% BMK BMK BMK
    next(snmpTargetParamsTable, RowIndex, Cols);
snmpTargetParamsTable(set, RowIndex, Cols0) ->
    %% BMK BMK BMK
    case (catch verify_snmpTargetParamsTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    snmp_notification_mib:invalidate_cache(),
	    snmp_generic:table_func(set, RowIndex, Cols, 
				    db(snmpTargetParamsTable));
	Error ->
	    Error
    end;
snmpTargetParamsTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpTargetParamsTable)).

verify_snmpTargetParamsTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_snmpTargetParamsTable_cols([{Col, Val0}|Cols], Acc) ->
    Val = verify_snmpTargetParamsTable_col(Col, Val0),
    verify_snmpTargetParamsTable_cols(Cols, [{Col, Val}|Acc]).

verify_snmpTargetParamsTable_col(?snmpTargetParamsName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?snmpTargetParamsName)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsMPModel, Model) ->
    case Model of
	v1      -> ?MP_V1;
	v2c     -> ?MP_V2C;
	v3      -> ?MP_V3;
	?MP_V1  -> ?MP_V1;
	?MP_V2C -> ?MP_V2C;
	?MP_V3  -> ?MP_V3;
	_       -> wrongValue(?snmpTargetParamsMPModel)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsSecurityModel, Model) ->
    case Model of
	v1       -> ?SEC_V1;
	v2c      -> ?SEC_V2C;
	usm      -> ?SEC_USM;
	?SEC_V1  -> ?SEC_V1;
	?SEC_V2C -> ?SEC_V2C;
	?SEC_USM -> ?SEC_USM;
	_        -> wrongValue(?snmpTargetParamsSecurityModel)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsSecurityName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?snmpTargetParamsSecurityName)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsSecurityLevel, Level) ->
    case Level of
	noAuthNoPriv -> 1;
	authNoPriv   -> 2;
	authPriv     -> 3;
	1            -> 1;
	2            -> 2;
	3            -> 3;
	_            -> wrongValue(?snmpTargetParamsSecurityLevel)
    end;
verify_snmpTargetParamsTable_col(_, Val) ->
    Val.

db(X) -> snmpa_agent:db(X).

fa(snmpTargetAddrTable) -> ?snmpTargetAddrTDomain;
fa(snmpTargetParamsTable) -> ?snmpTargetParamsMPModel.
 
foi(snmpTargetAddrTable) -> ?snmpTargetAddrName;
foi(snmpTargetParamsTable) -> ?snmpTargetParamsName.
 
noc(snmpTargetAddrTable) -> 9;
noc(snmpTargetParamsTable) -> 7.

stc(snmpTargetAddrTable) -> ?snmpTargetAddrStorageType;
stc(snmpTargetParamsTable) -> ?snmpTargetParamsStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).

 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).


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

config_err(F, A) ->
    snmpa_error:config_err("[TARGET-MIB]: " ++ F, A).
