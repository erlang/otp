%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(snmp_notification_mib).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([configure/1, reconfigure/1, invalidate_cache/0,
	 snmpNotifyTable/1, snmpNotifyTable/3,
	 snmpNotifyFilterTable/3, snmpNotifyFilterProfileTable/3,
	 get_targets/0, get_targets/1]).
-export([add_notify/3, delete_notify/1]).
-export([check_notify/1]).

-include("snmpa_internal.hrl").
-include("SNMP-NOTIFICATION-MIB.hrl").
-include("SNMPv2-TC.hrl").
-include("snmp_tables.hrl").

-define(VMODULE,"NOTIFICATION-MIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the notify tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case db(snmpNotifyTable) of
        {_, mnesia} ->
            ?vlog("notification table in mnesia: cleanup",[]),
            gc_tabs();
        TabDb ->
	    case snmpa_local_db:table_exists(TabDb) of
		true ->
		    ?vlog("notification table exist: cleanup",[]),
		    gc_tabs();
		false ->
		    ?vlog("notification table does not exist: reconfigure",[]),
		    reconfigure(Dir)
	    end
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the notify tables, and
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
    ?vdebug("read notify config files",[]),
    Notifs = read_notify_config_files(Dir),
    init_tabs(Notifs),
    ?vdebug("invalidate cache",[]),
    invalidate_cache(),
    ok.


read_notify_config_files(Dir) ->
    ?vdebug("read notify config file",[]),
    FileName = "notify.conf", 
    Gen =
	fun (D, Reason) ->
		info_msg("failed reading config file ~s"
			 "~n   Config Dir: ~s"
			 "~n   Reason:     ~p",
			 [FileName, D, Reason]),
		ok
	end,
    Order  = fun snmp_conf:no_order/2,
    Filter = fun snmp_conf:no_filter/1,
    Check  = fun (Entry, State) -> {check_notify(Entry), State} end,
    [Notifs] = 
	snmp_conf:read_files(Dir, [{FileName, Gen, Order, Check, Filter}]),
    Notifs.

check_notify({Name, Tag, Type}) ->
    snmp_conf:check_string(Name, {gt, 0}),
    snmp_conf:check_string(Tag),
    {ok, Val} = snmp_conf:check_atom(Type, [{trap, 1}, {inform, 2}]),
    Notify = {Name, Tag, Val, 
	      ?'StorageType_nonVolatile', ?'RowStatus_active'},
    {ok, Notify};
check_notify(X) ->
    error({invalid_notify, X}).


init_tabs(Notifs) ->
    ?vdebug("create notify table",[]),
    snmpa_local_db:table_delete(db(snmpNotifyTable)),
    snmpa_local_db:table_create(db(snmpNotifyTable)),
    ?vdebug("initiate notify table",[]),
    init_notify_table(Notifs).
    
init_notify_table([Row | T]) ->
    Key = element(1, Row),
    snmpa_local_db:table_create_row(db(snmpNotifyTable), Key, Row),
    init_notify_table(T);
init_notify_table([]) -> true.

table_cre_row(Tab, Key, Row) ->
    snmpa_mib_lib:table_cre_row(db(Tab), Key, Row).

table_del_row(Tab, Key) ->
    snmpa_mib_lib:table_del_row(db(Tab), Key).


%% FIXME: does not work with mnesia
add_notify(Name, Tag, Type) ->
    Notif = {Name, Tag, Type},
    case (catch check_notify(Notif)) of
	{ok, Row} ->
	    Key = element(1, Row),
	    case table_cre_row(snmpNotifyTable, Key, Row) of
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

%% FIXME: does not work with mnesia
delete_notify(Key) ->
    case table_del_row(snmpNotifyTable, Key) of
	true ->
	    ok;
	false ->
	    {error, delete_failed}
    end.
    
gc_tabs() ->
    DB  = db(snmpNotifyTable), 
    STC = stc(snmpNotifyTable),
    FOI = foi(snmpNotifyTable),
    snmpa_mib_lib:gc_tab(DB, STC, FOI),
    ok.

			    
%%-----------------------------------------------------------------
%% Func: get_targets()
%%       get_targets(NotifyName) -> [Target]
%% Types: Target = {DestAddr, TargetName, TargetParams, NotifyType}
%%        NotifyName = string()  - the INDEX
%%        DestAddr = {TDomain, TAddr}
%%        TagrgetName = string()
%%        TargetParams = {MpModel, SecModel, SecName, SecLevel}
%%        NotifyType = trap | {inform, Timeout, Retry}
%% Purpose: Returns a list of all targets.  Called by snmpa_trap
%%          when a trap should be sent.
%%          If a NotifyName is specified, the targets for that
%%          name is returned.
%%-----------------------------------------------------------------
get_targets() ->
    TargetsFun = fun find_targets/0,
    snmpa_target_cache:targets(TargetsFun).

get_targets(NotifyName) ->
    TargetsFun = fun find_targets/0,
    snmpa_target_cache:targets(TargetsFun, NotifyName).


%%-----------------------------------------------------------------
%% We use a cache of targets to avoid searching the tables each
%% time a trap is sent.  When some of the 3 tables (notify,
%% targetAddr, targetParams) is modified, the cache is invalidated.
%%-----------------------------------------------------------------

invalidate_cache() ->
    snmpa_target_cache:invalidate().
    

%% Ret: [{NotifyName, {DestAddr, TargetName, TargetParams, NotifyType}}]
%%   NotifyType = trap | {inform, Timeout. Retry}
%%   DestAddr = {Domain, Addr} ; e.g. {snmpUDPDomain, {IPasList, UdpPort}}

find_targets() ->
    TargAddrs = snmp_target_mib:get_target_addrs(),
    %% TargAddrs = [{TagList,DestAddr,TargetName,TargetParams,Timeout,Retry}]
    {_, Db} = db(snmpNotifyTable),
    find_targets([], TargAddrs, Db, []).
find_targets(Key, TargAddrs, Db, Res) ->
    case table_next(snmpNotifyTable, Key) of
	endOfTable -> 
	    Res;
        NextKey when Db == mnesia ->
            case mnesia:snmp_get_row(snmpNotifyTable, NextKey) of
                {ok, #snmpNotifyTable{
                   snmpNotifyTag = Tag,
                   snmpNotifyType = Type,
                   snmpNotifyRowStatus = ?'RowStatus_active'}} ->
                    ?vtrace("found notify entry for ~w"
                            "~n   Tag:     ~w"
                            "~n   Type:    ~w", [NextKey, Tag, Type]),
                    Targs = get_targets(TargAddrs, Tag, Type, NextKey),
                    find_targets(NextKey, TargAddrs, Db, Targs ++ Res);
                {ok, #snmpNotifyTable{
                   snmpNotifyTag       = Tag,
                   snmpNotifyType      = Type,
                   snmpNotifyRowStatus = RowStatus}} ->
                    ?vtrace("found invalid notify entry for ~w"
                            "~n   Tag:       ~w"
                            "~n   Type:      ~w"
			    "~n   RowStatus: ~p", 
			    [NextKey, Tag, Type, RowStatus]), 
                    find_targets(NextKey, TargAddrs, Db, Res);
                _ ->
                    ?vtrace("notify entry not found for ~w", [NextKey]),
                    find_targets(NextKey, TargAddrs, Db, Res)
            end;
 	NextKey -> 
	    Elements = [?snmpNotifyTag, ?snmpNotifyType, ?snmpNotifyRowStatus],
	    case snmpNotifyTable(get, NextKey, Elements) of
		[{value, Tag}, {value, Type}, {value, ?'RowStatus_active'}] ->
		    ?vtrace("found notify entry for ~w"
			    "~n   Tag:     ~w"
			    "~n   Type:    ~w", [NextKey, Tag, Type]),
		    Targs = get_targets(TargAddrs, Tag, Type, NextKey),
		    find_targets(NextKey, TargAddrs, Db, Targs ++ Res);
		[{value, Tag1}, {value, Type1}, {value, RowStatus}] ->
		    ?vtrace("found invalid notify entry for ~w"
			    "~n   Tag:       ~w"
			    "~n   Type:      ~w"
			    "~n   RowStatus: ~w", 
			    [NextKey, Tag1, Type1, RowStatus]),
		    find_targets(NextKey, TargAddrs, Db, Res);
		_ ->
                    ?vtrace("notify entry not found for ~w", [NextKey]),
		    find_targets(NextKey, TargAddrs, Db, Res)
	    end
    end.

get_targets([{TagList, Addr, TargetName, Params, Timeout, Retry}|T],
	    Tag, Type, Name) ->
    case snmp_misc:is_tag_member(Tag, TagList) of
	true -> 
	    ?vtrace("tag ~w *is* member", [Tag]),
	    [{Name, {Addr, TargetName, Params, type(Type, Timeout, Retry)}}|
	     get_targets(T, Tag, Type, Name)];
	false ->
	    ?vtrace("tag ~w is *not* member", [Tag]),
	    get_targets(T, Tag, Type, Name)
    end;
get_targets([], _Tag, _Type, _Name) ->
    [].

type(trap, _, _) -> trap;
type(1,    _, _) -> trap;                                  %% OTP-4329
type(inform, Timeout, Retry) -> {inform, Timeout, Retry};
type(2,      Timeout, Retry) -> {inform, Timeout, Retry}.  %% OTP-4329


%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------
%% Op = print - Used for debugging purposes
snmpNotifyTable(print) ->
    Table = snmpNotifyTable, 
    DB    = db(Table),
    FOI   = foi(Table),
    PrintRow = 
	fun(Prefix, Row) ->
		lists:flatten(
		  io_lib:format("~sName:        ~p"
				"~n~sTag:         ~p"
				"~n~sType:        ~p (~w)"
				"~n~sStorageType: ~p (~w)"
				"~n~sStatus:      ~p (~w)", 
				[Prefix, element(?snmpNotifyName, Row),
				 Prefix, element(?snmpNotifyTag, Row),
				 Prefix, element(?snmpNotifyType, Row),
				 case element(?snmpNotifyType, Row) of
				     ?snmpNotifyType_inform -> inform;
				     ?snmpNotifyType_trap -> trap;
				     _ -> undefined
				 end,
				 Prefix, element(?snmpNotifyStorageType, Row),
				 case element(?snmpNotifyStorageType, Row) of
				     ?'snmpNotifyStorageType_readOnly' -> readOnly;
				     ?'snmpNotifyStorageType_permanent' -> permanent;
				     ?'snmpNotifyStorageType_nonVolatile' -> nonVolatile;
				     ?'snmpNotifyStorageType_volatile' -> volatile;
				     ?'snmpNotifyStorageType_other' -> other;
				     _ -> undefined
				 end,
				 Prefix, element(?snmpNotifyRowStatus, Row),
				 case element(?snmpNotifyRowStatus, Row) of
				     ?'snmpNotifyRowStatus_destroy' -> destroy;
				     ?'snmpNotifyRowStatus_createAndWait' -> createAndWait;
				     ?'snmpNotifyRowStatus_createAndGo' -> createAndGo;
				     ?'snmpNotifyRowStatus_notReady' -> notReady;
				     ?'snmpNotifyRowStatus_notInService' -> notInService;
				     ?'snmpNotifyRowStatus_active' -> active;
				     _ -> undefined
				 end]))
	end,
    snmpa_mib_lib:print_table(Table, DB, FOI, PrintRow);
%% Op == new | delete
snmpNotifyTable(Op) ->
    snmp_generic:table_func(Op, db(snmpNotifyTable)).

%% Op == get | is_set_ok | set | get_next
snmpNotifyTable(get, RowIndex, Cols) ->
    %% BMK BMK BMK BMK
    get(snmpNotifyTable, RowIndex, Cols);
snmpNotifyTable(get_next, RowIndex, Cols) ->
    %% BMK BMK BMK BMK
    next(snmpNotifyTable, RowIndex, Cols);
snmpNotifyTable(set, RowIndex, Cols0) ->
    %% BMK BMK BMK BMK
    case (catch verify_snmpNotifyTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    invalidate_cache(),
	    %% invalidate_cache(RowIndex),
	    Db = db(snmpNotifyTable),
	    snmp_generic:table_func(set, RowIndex, Cols, Db);
	Error ->
	    Error
    end;
snmpNotifyTable(is_set_ok, RowIndex, Cols0) ->
    case (catch verify_snmpNotifyTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    Db = db(snmpNotifyTable),
	    snmp_generic:table_func(is_set_ok, RowIndex, Cols, Db);
	Error ->
	    Error
    end;
snmpNotifyTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpNotifyTable)).


verify_snmpNotifyTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_snmpNotifyTable_cols([{Col, Val0}|Cols], Acc) ->
    Val = verify_snmpNotifyTable_col(Col, Val0),
    verify_snmpNotifyTable_cols(Cols, [{Col, Val}|Acc]).

verify_snmpNotifyTable_col(?snmpNotifyName, Name) ->
    case (catch snmp_conf:check_string(Name, {gt, 0})) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?snmpNotifyName)
    end;
verify_snmpNotifyTable_col(?snmpNotifyTag, Tag) ->
    case (catch snmp_conf:check_string(Tag)) of
	ok ->
	    Tag;
	_ ->
	    wrongValue(?snmpNotifyTag)
    end;
verify_snmpNotifyTable_col(?snmpNotifyType, Type) ->
    case Type of
	trap   -> 1;
	inform -> 2;
	1      -> 1;
	2      -> 2;
	_      -> wrongValue(?snmpNotifyType)
    end;
verify_snmpNotifyTable_col(_, Val) ->
    Val.


%%-----------------------------------------------------------------
%% In this version of the agent, we don't support notification
%% filters.
%%-----------------------------------------------------------------
snmpNotifyFilterTable(get, _RowIndex, Cols) ->
    lists:map(fun(_Col) -> {noValue, noSuchObject} end, Cols);
snmpNotifyFilterTable(get_next, _RowIndex, Cols) ->
    lists:map(fun(_Col) -> endOfTable end, Cols);
snmpNotifyFilterTable(is_set_ok, _RowIndex, Cols) ->
    {notWritable, element(1, hd(Cols))}.

snmpNotifyFilterProfileTable(get, _RowIndex, Cols) ->
    lists:map(fun(_Col) -> {noValue, noSuchObject} end, Cols);
snmpNotifyFilterProfileTable(get_next, _RowIndex, Cols) ->
    lists:map(fun(_Col) -> endOfTable end, Cols);
snmpNotifyFilterProfileTable(is_set_ok, _RowIndex, Cols) ->
    {notWritable, element(1, hd(Cols))}.


db(X) -> snmpa_agent:db(X).

fa(snmpNotifyTable) -> ?snmpNotifyTag.
 
foi(snmpNotifyTable) -> ?snmpNotifyName.
 
noc(snmpNotifyTable) -> 5.

stc(snmpNotifyTable) -> ?snmpNotifyStorageType.
 
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
    put(sname, conf);
set_sname(_) -> %% Keep it, if already set.
    ok.

error(Reason) ->
    throw({error, Reason}).

info_msg(F, A) -> 
    ?snmpa_info("[NOTIFICATION-MIB]: " ++ F, A).

config_err(F, A) ->
    snmpa_error:config_err("[NOTIFICATION-MIB]: " ++ F, A).
