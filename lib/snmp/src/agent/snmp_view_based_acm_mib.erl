%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
-module(snmp_view_based_acm_mib).

-export([configure/1, reconfigure/1, table_next/2, get/3]).

-export([vacmAccessTable/1, vacmAccessTable/3,
	 vacmContextTable/1, vacmContextTable/3,
	 vacmSecurityToGroupTable/1, vacmSecurityToGroupTable/3,
	 vacmViewSpinLock/1, vacmViewSpinLock/2,
	 vacmViewTreeFamilyTable/1, vacmViewTreeFamilyTable/3]).
-export([add_sec2group/3,     delete_sec2group/1,
	 add_access/8,        delete_access/1,
	 add_view_tree_fam/4, delete_view_tree_fam/1]).

%% Internal exports
-export([check_vacm/1]).


-include("snmp_types.hrl").
-include("SNMPv2-TC.hrl").
-include("SNMP-VIEW-BASED-ACM-MIB.hrl").
-include("snmpa_vacm.hrl").


-define(VMODULE,"VACM-MIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the VACM tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case db(vacmSecurityToGroupTable) of
        {_, mnesia} ->
            ?vdebug("vacm security-to-group table in mnesia: cleanup",[]),
            gc_tabs(),
	    init_vacm_mnesia();
        TabDb ->
	    case snmpa_local_db:table_exists(TabDb) of
		true ->
		    ?vdebug("vacm security-to-group table already exist: "
			    "cleanup",[]),
		    gc_tabs();
		false ->
		    ?vdebug("vacm security-to-group table does not exist: "
			    "reconfigure",[]),
		    reconfigure(Dir)
	    end
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the VACM tables, and
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
    ?vdebug("read vacm configuration files",[]),
    {Sec2Group, Access, View} = read_vacm_config_files(Dir),
    ?vdebug("initiate tables",[]),
    init_tabs(Sec2Group, Access, View),
    ok.

read_vacm_config_files(Dir) ->
    ?vdebug("read vacm config file",[]),
    Gen    = fun(_) -> ok end,
    Filter = fun(Vacms) -> 
                     Sec2Group = [X || {vacmSecurityToGroup, X} <- Vacms],
                     Access = [X || {vacmAccess, X} <- Vacms],
                     View = [X || {vacmViewTreeFamily, X} <- Vacms],
                     {Sec2Group, Access, View}
             end,
    Check  = fun(Entry) -> check_vacm(Entry) end,
    [Vacms] = snmp_conf:read_files(Dir, [{Gen, Filter, Check, "vacm.conf"}]),
    Vacms.

%%-----------------------------------------------------------------
%% VACM tables
%%-----------------------------------------------------------------
check_vacm({vacmSecurityToGroup, SecModel, SecName, GroupName}) ->
    {ok, SecM} = snmp_conf:check_sec_model(SecModel, []),
    snmp_conf:check_string(SecName),
    snmp_conf:check_string(GroupName),

    Vacm = {SecM, SecName, GroupName,
	    ?'StorageType_nonVolatile', ?'RowStatus_active'},
    {ok, {vacmSecurityToGroup, Vacm}};
check_vacm({vacmAccess, GroupName, Prefix, SecModel, SecLevel,
            Match, RV, WV, NV}) ->
    snmp_conf:check_string(GroupName),
    snmp_conf:check_string(Prefix),
    {ok, SecM} = snmp_conf:check_sec_model(SecModel, []),
    {ok, SecL} = snmp_conf:check_sec_level(SecLevel),
    MatchAlt = [{exact, ?vacmAccessContextMatch_exact},
		{prefix, ?vacmAccessContextMatch_prefix}],
    {ok, M} = snmp_conf:check_atom(Match, MatchAlt),
    snmp_conf:check_string(RV),
    snmp_conf:check_string(WV),
    snmp_conf:check_string(NV),

    %% GN, Prefix, Model, Level, Row
    Vacm = {GroupName, Prefix, SecM, SecL, 
	    {M, RV, WV, NV,
	     ?'StorageType_nonVolatile', ?'RowStatus_active'}},
    {ok, {vacmAccess, Vacm}};
check_vacm({vacmViewTreeFamily, ViewName, Tree, Type, Mask}) ->
    snmp_conf:check_string(ViewName),
    snmp_conf:check_oid(Tree),
    {ok, TypeVal} =
        snmp_conf:check_atom(Type, [{included, ?view_included},
				    {excluded, ?view_excluded}]),
    MaskVal = 
        case (catch snmp_conf:check_atom(Mask, [{null, []}])) of
            {error, _}  -> 
                snmp_conf:check_oid(Mask),
                Mask;
	    {ok, X} ->
		X
	end,
    Vacm = {ViewName, Tree, MaskVal, TypeVal, 
	    ?'StorageType_nonVolatile', ?'RowStatus_active'},
    {ok, {vacmViewTreeFamily, Vacm}};
check_vacm(X) ->
    error({invalid_vacm, X}).


init_tabs(Sec2Group, Access, View) ->
    ?vdebug("create vacm security-to-group table",[]),
    snmpa_local_db:table_delete(db(vacmSecurityToGroupTable)),
    snmpa_local_db:table_create(db(vacmSecurityToGroupTable)),
    init_sec2group_table(Sec2Group),
    init_access_table(Access),
    ?vdebug("create vacm view-tree-family table",[]),
    snmpa_local_db:table_delete(db(vacmViewTreeFamilyTable)),
    snmpa_local_db:table_create(db(vacmViewTreeFamilyTable)),
    init_view_table(View).
    
init_sec2group_table([Row | T]) ->
%     ?vtrace("init security-to-group table: "
% 	    "~n   Row: ~p",[Row]),    
    Key1 = element(1, Row),
    Key2 = element(2, Row),
    Key = [Key1, length(Key2) | Key2],
    snmpa_local_db:table_create_row(db(vacmSecurityToGroupTable), Key, Row),
    init_sec2group_table(T);
init_sec2group_table([]) -> true.

init_access_table([{GN, Prefix, Model, Level, Row} | T]) ->
%     ?vtrace("init access table: "
% 	    "~n   GN:     ~p"
% 	    "~n   Prefix: ~p"
% 	    "~n   Model:  ~p"
% 	    "~n   Level:  ~p"
% 	    "~n   Row:    ~p",[GN, Prefix, Model, Level, Row]),    
    Key = [length(GN) | GN] ++ [length(Prefix) | Prefix] ++ [Model, Level],
    snmpa_vacm:insert([{Key, Row}], false),
    init_access_table(T);
init_access_table([]) ->
    snmpa_vacm:dump_table().

init_view_table([Row | T]) ->
%     ?vtrace("init view table: "
% 	    "~n   Row: ~p",[Row]),    
    Key1 = element(1, Row),
    Key2 = element(2, Row),
    Key = [length(Key1) | Key1] ++ [length(Key2) | Key2],
    snmpa_local_db:table_create_row(db(vacmViewTreeFamilyTable), Key, Row),
    init_view_table(T);
init_view_table([]) -> true.


table_cre_row(Tab, Key, Row) ->
    snmpa_mib_lib:table_cre_row(db(Tab), Key, Row).

table_del_row(Tab, Key) ->
    snmpa_mib_lib:table_del_row(db(Tab), Key).


%% add_sec2group(SecModel, SecName, GroupName) -> Result
%% Result -> {ok, Key} | {error, Reason}
%% Key -> term()
%% Reason -> term()
add_sec2group(SecModel, SecName, GroupName) ->
    Sec2Grp = {vacmSecurityToGroup, SecModel, SecName, GroupName},
    case (catch check_vacm(Sec2Grp)) of
	{ok, {vacmSecurityToGroup, Row}} ->
	    Key1 = element(1, Row),
	    Key2 = element(2, Row),
	    Key = [Key1, length(Key2) | Key2],
	    case table_cre_row(vacmSecurityToGroupTable, Key, Row) of
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

delete_sec2group(Key) ->
    case table_del_row(vacmSecurityToGroupTable, Key) of
	true ->
	    ok;
	false ->
	    {error, delete_failed}
    end.
    
%% NOTE: This function must be used in conjuction with
%%       snmpa_vacm:dump_table.
%%       That is, when all access has been added, call
%%       snmpa_vacm:dump_table/0
add_access(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV) ->
    Access = {vacmAccess, GroupName, Prefix, SecModel, SecLevel, 
	      Match, RV, WV, NV},
    case (catch check_vacm(Access)) of
	{ok, {vacmAccess, {GN, Pref, SM, SL, Row}}} ->
	    Key1 = [length(GN) | GN],
	    Key2 = [length(Pref) | Pref],
	    Key3 = [SM, SL],
	    Key  = Key1 ++ Key2 ++ Key3, 
	    snmpa_vacm:insert([{Key, Row}], false),
	    {ok, Key};
	{error, Reason} ->
	    {error, Reason};
        Error ->
            {error, Error}
    end.

delete_access(Key) ->
    snmpa_vacm:delete(Key).


add_view_tree_fam(ViewIndex, SubTree, Status, Mask) ->
    VTF = {vacmViewTreeFamily, ViewIndex, SubTree, Status, Mask},
    case (catch check_vacm(VTF)) of
	{ok, {vacmViewTreeFamily, Row}} ->
	    Key1 = element(1, Row),
	    Key2 = element(2, Row),
	    Key  = [length(Key1) | Key1] ++ [length(Key2) | Key2],
	    case table_cre_row(vacmViewTreeFamilyTable, Key, Row) of
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

delete_view_tree_fam(Key) ->
    case table_del_row(vacmViewTreeFamilyTable, Key) of
	true ->
	    ok;
	false ->
	    {error, delete_failed}
    end.

    
gc_tabs() ->
    SecDB  = db(vacmSecurityToGroupTable),
    SecSTC = stc(vacmSecurityToGroupTable),
    SecFOI = foi(vacmSecurityToGroupTable),
    snmpa_mib_lib:gc_tab(SecDB, SecSTC, SecFOI),
    ViewDB  = db(vacmViewTreeFamilyTable),
    ViewSTC = stc(vacmViewTreeFamilyTable),
    ViewFOI = foi(vacmViewTreeFamilyTable),
    snmpa_mib_lib:gc_tab(ViewDB, ViewSTC, ViewFOI),
    ok.

init_vacm_mnesia() ->
    F = fun(RowIndex, Row) ->
                snmpa_vacm:insert([{RowIndex, Row}], false)
        end,
    
    %% The 5 is intentional: It is a trick to get a tuple with the
    %% columns needed by the vacm ets-table (corresponding to the 
    %% tuple read from the config files). Therefor, 5 since it it
    %% is not a real foi...
    snmp_generic:table_foreach({vacmAccessTable, mnesia}, F, 5).


%%-----------------------------------------------------------------
%% The context table is actually implemented in an internal,
%% non-snmp visible table intContextTable.
%%-----------------------------------------------------------------
vacmContextTable(_Op) ->
    ok.
vacmContextTable(set = Op, Arg1, Arg2) ->
    snmpa_agent:invalidate_ca_cache(),
    snmp_framework_mib:intContextTable(Op, Arg1, Arg2);
vacmContextTable(Op, Arg1, Arg2) ->
    snmp_framework_mib:intContextTable(Op, Arg1, Arg2).


vacmSecurityToGroupTable(Op) ->
    snmp_generic:table_func(Op, db(vacmSecurityToGroupTable)).

vacmSecurityToGroupTable(get_next, RowIndex, Cols) ->
    next(vacmSecurityToGroupTable, RowIndex, Cols);
vacmSecurityToGroupTable(get, RowIndex, Cols) ->
    get(vacmSecurityToGroupTable, RowIndex, Cols);
vacmSecurityToGroupTable(set, RowIndex, Cols0) ->
    ?vtrace("vacmSecurityToGroupTable(set) -> entry with"
	    "~n   RowIndex: ~p"
	    "~n   Cols0:    ~p", [RowIndex, Cols0]),
    case (catch verify_vacmSecurityToGroupTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    ?vtrace("vacmSecurityToGroupTable(set) -> verified: "
		    "~n   Cols: ~p", [Cols]),
            snmpa_agent:invalidate_ca_cache(),
	    snmp_generic:table_func(set, RowIndex, Cols, 
				    db(vacmSecurityToGroupTable));
	Error ->
	    Error
    end;
vacmSecurityToGroupTable(is_set_ok, RowIndex, Cols0) ->
    ?vtrace("vacmSecurityToGroupTable(is_set_ok) -> entry with"
	    "~n   RowIndex: ~p"
	    "~n   Cols0:    ~p", [RowIndex, Cols0]),
    case (catch verify_vacmSecurityToGroupTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    ?vtrace("vacmSecurityToGroupTable(is_set_ok) -> verified: "
		    "~n   Cols: ~p", [Cols]),
	    snmp_generic:table_func(is_set_ok, RowIndex, Cols, 
				    db(vacmSecurityToGroupTable));
	Error ->
	    Error
    end;
vacmSecurityToGroupTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(vacmSecurityToGroupTable)).


verify_vacmSecurityToGroupTable_cols([], Cols) ->
    ?vtrace("verify_vacmSecurityToGroupTable_cols -> entry when done with"
	    "~n   Cols: ~p", [Cols]),
    {ok, lists:reverse(Cols)};
verify_vacmSecurityToGroupTable_cols([{Col, Val0}|Cols], Acc) ->
    ?vtrace("verify_vacmSecurityToGroupTable_cols -> entry with"
	    "~n   Col:  ~p"
	    "~n   Val0: ~p", [Col, Val0]),
    Val = verify_vacmSecurityToGroupTable_col(Col, Val0),
    ?vtrace("verify_vacmSecurityToGroupTable_cols -> verified: "
	    "~n   Val: ~p", [Val]),
    verify_vacmSecurityToGroupTable_cols(Cols, [{Col, Val}|Acc]).

verify_vacmSecurityToGroupTable_col(?vacmSecurityModel, Model) ->
    case Model of
	any      -> ?SEC_ANY;
	v1       -> ?SEC_ANY;
	v2c      -> ?SEC_ANY;
	usm      -> ?SEC_ANY;
	?SEC_ANY -> ?SEC_ANY;
	?SEC_V1  -> ?SEC_ANY;
	?SEC_V2C -> ?SEC_ANY;
	?SEC_USM -> ?SEC_ANY;
	_ ->
	    ?vlog("verification of vacmSecurityModel(~w) ~p failed", 
		  [?vacmSecurityModel, Model]),
	    wrongValue(?vacmSecurityModel)
    end;
verify_vacmSecurityToGroupTable_col(?vacmSecurityName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	Reason ->
	    ?vlog("verification of vacmSecurityName(~w) ~p failed: "
		  "~n   Reason: ~p", [?vacmSecurityName, Name, Reason]),
	    wrongValue(?vacmSecurityName)
    end;
verify_vacmSecurityToGroupTable_col(?vacmGroupName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	Reason ->
	    ?vlog("verification of vacmGroupName(~w) ~p failed: "
		  "~n   Reason: ~p", [?vacmGroupName, Name, Reason]),
	    wrongValue(?vacmGroupName)
    end;
verify_vacmSecurityToGroupTable_col(_, Val) ->
    Val.

    
%%-----------------------------------------------------------------
%% The vacmAccesTable is implemented as a bplus_tree in the
%% snmpa_vacm_access_server process.  That means that we'll have
%% to implement everything by ourselves, most notably is_set_ok
%% and set.
%% Each row is stored in the bplus_tree as
%%    {RowIndex, {Col4, Col5, ..., Col9}}
%%
%%-----------------------------------------------------------------
vacmAccessTable(_Op) ->
    ok.
vacmAccessTable(get, RowIndex, Cols) ->
    %% For GET, Cols are guaranteed to be accessible columns.
    case snmpa_vacm:get_row(RowIndex) of
	{ok, Row} ->
	    lists:map(fun(Col) -> {value, element(Col-3, Row)} end, Cols);
	false ->
	    {noValue, noSuchInstance}
    end;
vacmAccessTable(get_next, RowIndex, Cols) ->
    %% For GET-NEXT, Cols can be anything, but they are sorted.
    %% Thus, we translate each 
    %% Example: GET-NEXT  -1.3.4.5
    %%                     4.3.4.5
    %%                    10.3.4.5
    %% Table: Idx= 1.2.3 Col4= 1
    %%        Idx= 4.5.6. Col4= 2
    %% Returns:  4.1.2.3 = 1, 4.4.5.6 = 2, endOfTable
    {PreCols, ValidCols} = split_cols(Cols, []),
    do_get_next([], PreCols) ++ do_get_next(RowIndex, ValidCols);
%% vacmAccessContextMatch does not have a default value => we'll have
%% to treat that col specially
vacmAccessTable(is_set_ok, RowIndex, Cols0) ->
    case (catch verify_vacmAccessTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    IsValidKey = is_valid_key(RowIndex),
	    case lists:keysearch(?vacmAccessStatus, 1, Cols) of
		%% Ok, if contextMatch is init
		{value, {Col, ?'RowStatus_active'}} -> 
	            {ok, Row} = snmpa_vacm:get_row(RowIndex),
	            case element(?vacmAContextMatch, Row) of
			noinit -> {inconsistentValue, Col};
			_ -> {noError, 0}
		    end;
		{value, {Col, ?'RowStatus_notInService'}} -> % Ok, if not notReady
		    {ok, Row} = snmpa_vacm:get_row(RowIndex),
		    case element(?vacmAStatus, Row) of
			?'RowStatus_notReady' -> {inconsistentValue, Col};
			_ -> {noError, 0}
		    end;
		{value, {Col, ?'RowStatus_notReady'}} -> % never ok!
		    {inconsistentValue, Col};
		{value, {Col, ?'RowStatus_createAndGo'}} -> % ok, if it doesn't exist
		    Res = lists:keysearch(?vacmAccessContextMatch, 1, Cols),
		    case snmpa_vacm:get_row(RowIndex) of
			false when (IsValidKey =:= true) andalso 
				   is_tuple(Res) -> {noError, 0};
			false -> {noCreation, Col}; % Bad RowIndex
			_ -> {inconsistentValue, Col}
		    end;
		{value, {Col, ?'RowStatus_createAndWait'}} -> % ok, if it doesn't exist
		    case snmpa_vacm:get_row(RowIndex) of
			false when (IsValidKey =:= true) -> {noError, 0};
			false -> {noCreation, Col}; % Bad RowIndex
			_ -> {inconsistentValue, Col}
		    end;
		{value, {_Col, ?'RowStatus_destroy'}} -> % always ok!
		    {noError, 0};
		_ -> % otherwise, it's a change; it must exist
		    case snmpa_vacm:get_row(RowIndex) of
			{ok, _} ->
			    {noError, 0};
			false ->
			    {inconsistentName, element(1, hd(Cols))}
		    end
	    end;
	Error ->
	    Error
    end;
vacmAccessTable(set, RowIndex, Cols0) ->
    case (catch verify_vacmAccessTable_cols(Cols0, [])) of
	{ok, Cols} ->
            snmpa_agent:invalidate_ca_cache(),
	    do_vacmAccessTable_set(RowIndex, Cols);
	Error ->
	    Error
    end.

verify_vacmAccessTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_vacmAccessTable_cols([{Col, Val0}|Cols], Acc) ->
    Val = verify_vacmAccessTable_col(Col, Val0),
    verify_vacmAccessTable_cols(Cols, [{Col, Val}|Acc]).

verify_vacmAccessTable_col(?vacmAccessContextPrefix, Pref) ->
    case (catch snmp_conf:check_string(Pref)) of
	ok ->
	    Pref;
	_ ->
	    wrongValue(?vacmAccessContextPrefix)
    end;
verify_vacmAccessTable_col(?vacmAccessSecurityModel, Model) ->
    case Model of
	any      -> ?SEC_ANY;
	v1       -> ?SEC_ANY;
	v2c      -> ?SEC_ANY;
	usm      -> ?SEC_ANY;
	?SEC_ANY -> ?SEC_ANY;
	?SEC_V1  -> ?SEC_ANY;
	?SEC_V2C -> ?SEC_ANY;
	?SEC_USM -> ?SEC_ANY;
	_ ->
	    wrongValue(?vacmAccessSecurityModel)
    end;
verify_vacmAccessTable_col(?vacmAccessSecurityLevel, Level) ->
    case Level of
        noAuthNoPriv -> 1;
        authNoPriv   -> 2;
        authPriv     -> 3;
        1            -> 1;
        2            -> 2;
        3            -> 3;
        _            -> wrongValue(?vacmAccessSecurityLevel)
    end;
verify_vacmAccessTable_col(?vacmAccessContextMatch, Match) ->
    case Match of
	exact                          -> ?vacmAccessContextMatch_exact;
	prefix                         -> ?vacmAccessContextMatch_prefix;
	?vacmAccessContextMatch_exact  -> ?vacmAccessContextMatch_exact;
	?vacmAccessContextMatch_prefix -> ?vacmAccessContextMatch_prefix;
	_ ->
	    wrongValue(?vacmAccessContextMatch)
    end;
verify_vacmAccessTable_col(?vacmAccessReadViewName, RVN) ->
    case (catch snmp_conf:check_string(RVN)) of
	ok ->
	    RVN;
	_ ->
	    wrongValue(?vacmAccessReadViewName)
    end;
verify_vacmAccessTable_col(?vacmAccessWriteViewName, WVN) ->
    case (catch snmp_conf:check_string(WVN)) of
	ok ->
	    WVN;
	_ ->
	    wrongValue(?vacmAccessWriteViewName)
    end;
verify_vacmAccessTable_col(?vacmAccessNotifyViewName, NVN) ->
    case (catch snmp_conf:check_string(NVN)) of
	ok ->
	    NVN;
	_ ->
	    wrongValue(?vacmAccessNotifyViewName)
    end;
verify_vacmAccessTable_col(_, Val) ->
    Val.

do_vacmAccessTable_set(RowIndex, Cols) ->
    case lists:keysearch(?vacmAccessStatus, 1, Cols) of
	{value, {_Col, ?'RowStatus_createAndGo'}} ->
	    Row = mk_row(Cols),
	    Row2 = setelement(?vacmAStatus, Row, ?'RowStatus_active'),
	    snmpa_vacm:insert([{RowIndex, Row2}]),
	    {noError, 0};
	{value, {_Col, ?'RowStatus_createAndWait'}} ->
	    Row = mk_row(Cols),
	    Row2 = case element(?vacmAContextMatch, Row) of
		       noinit -> setelement(?vacmAStatus, Row,
					    ?'RowStatus_notReady');
		       _      -> setelement(?vacmAStatus, Row,
					    ?'RowStatus_notInService')
		   end,
	    snmpa_vacm:insert([{RowIndex, Row2}]),
	    {noError, 0};
	{value, {_Col, ?'RowStatus_destroy'}} ->
	    snmpa_vacm:delete(RowIndex),
	    {noError, 0};
	{value, {_Col, ?'RowStatus_active'}}  ->
	    {ok, Row} = snmpa_vacm:get_row(RowIndex),
	    NRow = ch_row(Cols, Row),
	    NRow2 =
		case element(?vacmAContextMatch, NRow) of
		    noinit -> setelement(?vacmAStatus, NRow,
					 ?'RowStatus_notReady');
		    _      -> setelement(?vacmAStatus, NRow,
					 ?'RowStatus_active')
		end,
	    snmpa_vacm:insert([{RowIndex, NRow2}]),
	    {noError, 0};
	_ ->
	    {ok, Row} = snmpa_vacm:get_row(RowIndex),
	    NRow = ch_row(Cols, Row),
	    NRow2 =
		case element(?vacmAContextMatch, NRow) of
		    noinit -> setelement(?vacmAStatus, NRow,
					 ?'RowStatus_notReady');
		    _      -> setelement(?vacmAStatus, NRow,
					 ?'RowStatus_notInService')
		end,
	    snmpa_vacm:insert([{RowIndex, NRow2}]),
	    {noError, 0}
    end.
	    
	    
%% Cols are sorted, and all columns are > 3.
do_get_next(RowIndex, Cols) ->
    case snmpa_vacm:get_next_row(RowIndex) of
	{NextIndex, Row} ->
	    F1 = fun(Col) when Col < ?vacmAccessStatus -> 
			 {[Col | NextIndex], element(Col-3, Row)};
		    (_) -> 
			 endOfTable
		 end,
	    lists:map(F1, Cols);
	false ->
	    case snmpa_vacm:get_next_row([]) of
		{_NextIndex, Row} ->
		    F2 = fun(Col) when Col < ?vacmAccessStatus -> 
				 {[Col+1 | RowIndex], element(Col-2, Row)};
			    (_) ->
				 endOfTable
			 end,
		    lists:map(F2, Cols);
		false ->
		    lists:map(fun(_Col) -> endOfTable end, Cols)
	    end
    end.

%%-----------------------------------------------------------------
%% Functions to manipulate vacmAccessRows.
%%-----------------------------------------------------------------
is_valid_key(RowIndex) ->
    case catch mk_key(RowIndex) of
	true -> true;
	_ -> false
    end.

mk_key([L1 | T1]) ->
    [L2 | T2] = spx(L1, T1),
    [_SM, _SL] = spx(L2, T2),
    true.

spx(N, L) -> spx(N, [], L).
spx(0, _L1, L2) -> L2;
spx(N, L1, [H | L2]) -> spx(N-1, [H | L1], L2).

mk_row(Cols) -> 
    ch_row(Cols, {noinit, "", "", "", ?'StorageType_nonVolatile', noinit}).

ch_row([], Row) -> Row;
ch_row([{Col, Val} | T], Row) -> ch_row(T, setelement(Col-3, Row, Val)).
   

%% Split a list of columns in 2 lists - the first is all columns
%% that are =< 3.  For these, use the first accessible column number: 4.
split_cols([Col | Cols], PreCols) when Col =< 3 ->
    split_cols(Cols, [4 | PreCols]);
split_cols(Cols, PreCols) ->
    {PreCols, Cols}.

vacmViewSpinLock(new) ->
    snmp_generic:variable_func(new, {vacmViewSpinLock, volatile}),
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    Val = random:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {vacmViewSpinLock, volatile});

vacmViewSpinLock(delete) ->
    ok;

vacmViewSpinLock(get) ->
    snmp_generic:variable_func(get, {vacmViewSpinLock, volatile}).

vacmViewSpinLock(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {vacmViewSpinLock, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
vacmViewSpinLock(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {vacmViewSpinLock, volatile}).


vacmViewTreeFamilyTable(Op) ->
    snmp_generic:table_func(Op, db(vacmViewTreeFamilyTable)).
vacmViewTreeFamilyTable(get_next, RowIndex, Cols) ->
    next(vacmViewTreeFamilyTable, RowIndex, Cols);
vacmViewTreeFamilyTable(get, RowIndex, Cols) ->
    get(vacmViewTreeFamilyTable, RowIndex, Cols);
vacmViewTreeFamilyTable(set, RowIndex, Cols0) ->
    case (catch verify_vacmViewTreeFamilyTable_cols(Cols0, [])) of
	{ok, Cols} ->
            snmpa_agent:invalidate_ca_cache(),
	    snmp_generic:table_func(set, RowIndex, Cols, 
				    db(vacmViewTreeFamilyTable));
	Error ->
	    Error
    end;
vacmViewTreeFamilyTable(is_set_ok, RowIndex, Cols0) ->
    case (catch verify_vacmViewTreeFamilyTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    snmp_generic:table_func(is_set_ok, RowIndex, Cols, 
				    db(vacmViewTreeFamilyTable));
	Error ->
	    Error
    end;
vacmViewTreeFamilyTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(vacmViewTreeFamilyTable)).


verify_vacmViewTreeFamilyTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_vacmViewTreeFamilyTable_cols([{Col, Val0}|Cols], Acc) ->
    Val = verify_vacmViewTreeFamilyTable_col(Col, Val0),
    verify_vacmViewTreeFamilyTable_cols(Cols, [{Col, Val}|Acc]).

verify_vacmViewTreeFamilyTable_col(?vacmViewTreeFamilyViewName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?vacmViewTreeFamilyViewName)
    end;
verify_vacmViewTreeFamilyTable_col(?vacmViewTreeFamilySubtree, Tree) ->
    case (catch snmp_conf:check_oid(Tree)) of
	ok ->
	    Tree;
	_ ->
	    wrongValue(?vacmViewTreeFamilySubtree)
    end;
verify_vacmViewTreeFamilyTable_col(?vacmViewTreeFamilyMask, Mask) ->
    case Mask of
	null -> [];
	[]   -> [];
	_ ->
	    case (catch snmp_conf:check_oid(Mask)) of
		ok ->
		    Mask;
	        _ ->
		    wrongValue(?vacmViewTreeFamilyMask)
	    end
    end;
verify_vacmViewTreeFamilyTable_col(?vacmViewTreeFamilyType, Type) ->
    case Type of
	included       -> ?view_included;
	excluded       -> ?view_excluded;
	?view_included -> ?view_included;
	?view_excluded -> ?view_excluded;
	 _ ->
	    wrongValue(?vacmViewTreeFamilyType)
    end;
verify_vacmViewTreeFamilyTable_col(_, Val) ->
    Val.
	    

table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).


db(X) -> snmpa_agent:db(X).

fa(vacmSecurityToGroupTable) -> ?vacmGroupName;
fa(vacmViewTreeFamilyTable) -> ?vacmViewTreeFamilyMask.
 
foi(vacmSecurityToGroupTable) -> ?vacmSecurityModel;
foi(vacmViewTreeFamilyTable) -> ?vacmViewTreeFamilyViewName.

noc(vacmSecurityToGroupTable) -> 5;
noc(vacmViewTreeFamilyTable) -> 6.
 
stc(vacmSecurityToGroupTable) -> ?vacmSecurityToGroupStorageType;
stc(vacmViewTreeFamilyTable) -> ?vacmViewTreeFamilyStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).
 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

wrongValue(V) -> throw({wrongValue, V}).

set_sname() ->
    set_sname(get(sname)).

set_sname(undefined) ->
    put(sname,conf);
set_sname(_) -> %% Keep it, if already set.
    ok.

error(Reason) ->
    throw({error, Reason}).

config_err(F, A) ->
    snmpa_error:config_err("[VIEW-BASED-ACM-MIB]: " ++ F, A).

