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
-module(snmp_user_based_sm_mib).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([configure/1, reconfigure/1,
	 usmUserSpinLock/1, usmUserSpinLock/2,
	 usmUserTable/1, usmUserTable/3,
	 table_next/2,
	 is_engine_id_known/1, get_user/2, get_user_from_security_name/2,
	 mk_key_change/3, mk_key_change/5, extract_new_key/3, mk_random/1]).
-export([usmStatsUnsupportedSecLevels/1,
	 usmStatsNotInTimeWindows/1,
	 usmStatsUnknownUserNames/1,
	 usmStatsUnknownEngineIDs/1,
	 usmStatsWrongDigests/1,
	 usmStatsDecryptionErrors/1]).
-export([add_user/1, add_user/13, delete_user/1]).

%% Internal
-export([check_usm/1]).

-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMP-USM-AES-MIB.hrl").
-include("SNMPv2-TC.hrl").
-include("snmpa_internal.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"USM_MIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%% Columns not accessible via SNMP
-define(usmUserAuthKey, 14).
-define(usmUserPrivKey, 15).
-define(is_cloning, 16).


%%%-----------------------------------------------------------------
%%% Utility functions
%%%-----------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% Implements the instrumentation functions and additional 
%%% functions for the SNMP-USER-BASED-SM-MIB.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the USM tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case db(usmUserTable) of
        {_, mnesia} ->
            ?vdebug("usm user table in mnesia: init vars & cleanup",[]),
            init_vars(),
            gc_tabs();
	TabDb ->
	    case snmpa_local_db:table_exists(TabDb) of
		true ->
		    ?vdebug("usm user table exists: init vars & cleanup",[]),
		    init_vars(),
		    gc_tabs();
		false ->
		    ?vdebug("usm user table does not exist: reconfigure",[]),
		    reconfigure(Dir)
	    end
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the USM tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error) |
%%        exit({unsupported_crypto, Function})
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
    ?vdebug("read usm configuration files",[]),
    Users = read_usm_config_files(Dir),
    ?vdebug("check users",[]),
    check_users(Users),
    ?vdebug("initiate tables",[]),
    init_tabs(Users),
    ?vdebug("initiate vars",[]),
    init_vars(),
    ok.


read_usm_config_files(Dir) ->
    ?vdebug("read usm config file",[]),
    Gen    = fun (D, Reason) -> generate_usm(D, Reason) end,
    Order  = fun snmp_conf:no_order/2,
    Check  = fun (Entry, State) -> {check_usm(Entry), State} end,
    Filter = fun snmp_conf:no_filter/1,
    [Usms] = 
	snmp_conf:read_files(Dir, [{"usm.conf", Gen, Order, Check, Filter}]),
    Usms.


generate_usm(Dir, _Reason) ->
    info_msg("Incomplete configuration. Generating empty usm.conf.", []),
    USMFile = filename:join(Dir, "usm.conf"),
    ok = file:write_file(USMFile, list_to_binary([])),
    [].


check_usm({EngineID, Name, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,
           PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey}) ->
    snmp_conf:check_string(EngineID),
    snmp_conf:check_string(Name),
    snmp_conf:check_string(SecName),
    CloneVal =
        case catch snmp_conf:check_atom(Clone,[{zeroDotZero,?zeroDotZero}]) of
	    {error, _} ->
                snmp_conf:check_oid(Clone),
                Clone;
	    {ok, X} ->
                X
        end,
    AuthProtoAlt = [{usmNoAuthProtocol,      ?usmNoAuthProtocol},
		    {usmHMACSHAAuthProtocol, ?usmHMACSHAAuthProtocol},
		    {usmHMACMD5AuthProtocol, ?usmHMACMD5AuthProtocol}],
    {ok, AuthProto} = snmp_conf:check_atom(AuthP, AuthProtoAlt),
    snmp_conf:check_string(AuthKeyC),
    snmp_conf:check_string(OwnAuthKeyC),
    PrivProtoAlt = [{usmNoPrivProtocol,    ?usmNoPrivProtocol},
		    {usmDESPrivProtocol,   ?usmDESPrivProtocol},
		    {usmAesCfb128Protocol, ?usmAesCfb128Protocol}],
    {ok, PrivProto} = snmp_conf:check_atom(PrivP, PrivProtoAlt),
    snmp_conf:check_string(PrivKeyC),
    snmp_conf:check_string(OwnPrivKeyC),
    snmp_conf:check_string(Public),
    snmp_conf:check_string(AuthKey, alen(AuthP)),
    snmp_conf:check_string(PrivKey, plen(PrivP)),

    User = {EngineID, Name, SecName, 
	    CloneVal, AuthProto, AuthKeyC, OwnAuthKeyC,
	    PrivProto, PrivKeyC, OwnPrivKeyC, Public,
	    ?'StorageType_nonVolatile', ?'RowStatus_active', AuthKey, PrivKey},
    {ok, User};
check_usm(X) ->
    error({invalid_user, X}).

alen(usmNoAuthProtocol) -> any;
alen(usmHMACMD5AuthProtocol) -> 16;
alen(usmHMACSHAAuthProtocol) -> 20.

plen(usmNoPrivProtocol) -> any;
plen(usmDESPrivProtocol) -> 16;
plen(usmAesCfb128Protocol) -> 16.


%%-----------------------------------------------------------------
%% This function loops through all users, and check that the
%% definition is constistent with the support for crypto on
%% the system.  Thus, it is not possible to define a user that
%% uses authentication if 'crypto' is not started, or a user that
%% uses DES if 'crypto' doesn't support DES.
%%-----------------------------------------------------------------
check_users([User | Users]) ->
    check_user(User),
    check_users(Users);
check_users([]) ->
    ok.

check_user(User) ->
    case element(?usmUserAuthProtocol, User) of
	?usmNoAuthProtocol -> ok;
	?usmHMACMD5AuthProtocol ->
	    case is_crypto_supported(md5) of
		true -> ok;
		false -> exit({unsupported_crypto, md5_mac_96})
	    end;
	?usmHMACSHAAuthProtocol ->
	    case is_crypto_supported(sha) of
		true -> ok;
		false -> exit({unsupported_crypto, sha_mac_96})
	    end
    end,
    case element(?usmUserPrivProtocol, User) of
	?usmNoPrivProtocol -> ok;
	?usmDESPrivProtocol ->
	    case is_crypto_supported(des_cbc) of
		true -> ok;
		false -> exit({unsupported_crypto, des_cbc})
	    end;
	?usmAesCfb128Protocol ->
	    case is_crypto_supported(aes_cfb128) of
		true -> ok;
		false -> exit({unsupported_crypto, aes_cfb128})
	    end
    end.
    

init_tabs(Users) ->
    ?vdebug("create usm user table",[]),
    snmpa_local_db:table_delete(db(usmUserTable)),
    snmpa_local_db:table_create(db(usmUserTable)),
    init_user_table(Users).
    
init_user_table([Row | T]) ->
    Key = mk_user_table_key(Row),
    snmpa_local_db:table_create_row(db(usmUserTable), Key, Row),
    init_user_table(T);
init_user_table([]) -> true.

mk_user_table_key(Row) ->
    Key1 = element(1, Row),
    Key2 = element(2, Row),
    [length(Key1) | Key1] ++ [length(Key2) | Key2].

table_cre_row(Tab, Key, Row) ->
    snmpa_mib_lib:table_cre_row(db(Tab), Key, Row).

table_del_row(Tab, Key) ->
    snmpa_mib_lib:table_del_row(db(Tab), Key).


add_user(EngineID, Name, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,
	 PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey) ->
    User = {EngineID, Name, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,
	    PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey},
    add_user(User).

add_user(User) ->
    case (catch check_usm(User)) of
	{ok, Row} ->
	    case (catch check_user(Row)) of
		{'EXIT', Reason} ->
		    {error, Reason};
		_ ->
		    Key = mk_user_table_key(Row),
		    case table_cre_row(usmUserTable, Key, Row) of
			true ->
			    {ok, Key};
			false ->
			    {error, create_failed}
		    end
		end;
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    {error, Error}
    end.

delete_user(Key) ->
    case table_del_row(usmUserTable, Key) of
	true ->
	    ok;
	false ->
	    {error, delete_failed}
    end.


gc_tabs() ->
    DB  = db(usmUserTable),
    STC = stc(usmUserTable),
    FOI = foi(usmUserTable),
    snmpa_mib_lib:gc_tab(DB, STC, FOI),
    ok.


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------

usmStatsUnsupportedSecLevels(print) ->
    VarAndValue = [{usmStatsUnsupportedSecLevels, 
		    usmStatsUnsupportedSecLevels(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
usmStatsUnsupportedSecLevels(get) ->
    get_counter(usmStatsUnsupportedSecLevels).

usmStatsNotInTimeWindows(print) ->
    VarAndValue = [{usmStatsNotInTimeWindows, usmStatsNotInTimeWindows(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
usmStatsNotInTimeWindows(get) ->
    get_counter(usmStatsNotInTimeWindows).

usmStatsUnknownUserNames(print) ->
    VarAndValue = [{usmStatsUnknownUserNames, usmStatsUnknownUserNames(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
usmStatsUnknownUserNames(get) ->
    get_counter(usmStatsUnknownUserNames).

usmStatsUnknownEngineIDs(print) ->
    VarAndValue = [{usmStatsUnknownEngineIDs, usmStatsUnknownEngineIDs(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
usmStatsUnknownEngineIDs(get) ->
    get_counter(usmStatsUnknownEngineIDs).

usmStatsWrongDigests(print) ->
    VarAndValue = [{usmStatsWrongDigests, usmStatsWrongDigests(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
usmStatsWrongDigests(get) ->
    get_counter(usmStatsWrongDigests).

usmStatsDecryptionErrors(print) ->
    VarAndValue = [{usmStatsDecryptionErrors, usmStatsDecryptionErrors(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
usmStatsDecryptionErrors(get) ->
    get_counter(usmStatsDecryptionErrors).


get_counter(Name) ->
    case (catch ets:lookup(snmp_agent_table, Name)) of
	[{_, Val}] ->
	    {value, Val};
	_ ->
	    genErr
    end.


init_vars() -> lists:map(fun maybe_create_var/1, vars()).

maybe_create_var(Var) ->
    case ets:lookup(snmp_agent_table, Var) of
	[_] -> ok;
	_ -> init_var(Var)
    end.

init_var(Var) -> ets:insert(snmp_agent_table, {Var, 0}).

vars() ->
    [
     usmStatsUnsupportedSecLevels,
     usmStatsNotInTimeWindows,
     usmStatsUnknownUserNames,
     usmStatsUnknownEngineIDs,
     usmStatsWrongDigests,
     usmStatsDecryptionErrors
    ].


%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
is_engine_id_known(EngineID) ->
    EngineKey = [length(EngineID) | EngineID],
    ?vtrace("is_engine_id_known -> EngineKey: ~w", [EngineKey]),
    case table_next(usmUserTable, EngineKey) of
	endOfTable -> false;
	Key -> 
	    ?vtrace("is_engine_id_known -> Key: ~w", [Key]),
	    lists:prefix(EngineKey, Key)
    end.

get_user(EngineID, UserName) ->
    Key = [length(EngineID) | EngineID] ++ [length(UserName) | UserName],
    snmp_generic:table_get_row(db(usmUserTable), Key, foi(usmUserTable)).

get_user_from_security_name(EngineID, SecName) ->
    %% Since the normal mapping between UserName and SecName is the
    %% identityfunction, we first try to use the SecName as UserName,
    %% and check the resulting row.  If it doesn't match, we'll have to
    %% loop through the entire table.
    Key = [length(EngineID) | EngineID] ++ [length(SecName) | SecName],
    case snmp_generic:table_get_row(db(usmUserTable), Key, 
				    foi(usmUserTable)) of
	Row when is_tuple(Row) ->
	    ?vtrace("get_user_from_security_name -> "
		    "found user using the identity function", []),
	    Row;
	undefined ->
	    ?vtrace("get_user_from_security_name -> "
		    "user *not* found using the identity function", []),
	    F = fun(_, Row) when (
			      (element(?usmUserEngineID,Row) =:= EngineID) andalso 
			      (element(?usmUserSecurityName,Row) =:= SecName)) ->
			throw({ok, Row});
		   (_, _) ->
			ok
		end,
	    case catch snmp_generic:table_foreach(db(usmUserTable), F) of
		{ok, Row} ->
		    Row;
		_Else ->
		    undefined
	    end
    end.
		

%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------

usmUserSpinLock(print) ->
    VarAndValue = [{usmUserSpinLock, usmUserSpinLock(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
    
usmUserSpinLock(new) ->
    snmp_generic:variable_func(new, {usmUserSpinLock, volatile}),
    ?SNMP_RAND_SEED(),
    %% rand:seed(exrop,
    %%           {erlang:phash2([node()]),
    %%            erlang:monotonic_time(),
    %%            erlang:unique_integer()}),
    Val = rand:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {usmUserSpinLock, volatile});

usmUserSpinLock(delete) ->
    ok;

usmUserSpinLock(get) ->
    snmp_generic:variable_func(get, {usmUserSpinLock, volatile}).

usmUserSpinLock(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {usmUserSpinLock, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
usmUserSpinLock(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {usmUserSpinLock, volatile}).


%% Op = print - Used for debugging purposes
usmUserTable(print) ->
    Table = usmUserTable, 
    DB    = db(Table),
    FOI   = foi(Table),
    %% TableInfo = snmpa_mib_lib:get_table(db(Table), foi(Table)),
    PrintRow = 
	fun(Prefix, Row) ->
		lists:flatten(
		  io_lib:format("~sEngineID:         ~p"
				"~n~sName:             ~p"
				"~n~sSecurityName:     ~p"
				"~n~sCloneFrom:        ~p"
				"~n~sAuthProtocol:     ~p (~w)"
				"~n~sAuthKeyChange:    ~p"
				"~n~sOwnAuthKeyChange: ~p"
				"~n~sPrivProtocol:     ~p (~w)"
				"~n~sPrivKeyChange:    ~p"
				"~n~sOwnPrivKeyChange: ~p"
				"~n~sPublic:           ~p"
				"~n~sStorageType:      ~p (~w)"
				"~n~sStatus:           ~p (~w)", 
				[Prefix, element(?usmUserEngineID, Row),
				 Prefix, element(?usmUserName, Row),
				 Prefix, element(?usmUserSecurityName, Row),
				 Prefix, element(?usmUserCloneFrom, Row),
				 Prefix, element(?usmUserAuthProtocol, Row),
				 case element(?usmUserAuthProtocol, Row) of
				     ?usmNoAuthProtocol      -> none;
				     ?usmHMACMD5AuthProtocol -> md5;
				     ?usmHMACSHAAuthProtocol -> sha;
				     md5 -> md5;
				     sha -> sha;
				     _ -> undefined
				 end,
				 Prefix, element(?usmUserAuthKeyChange, Row),
				 Prefix, element(?usmUserOwnAuthKeyChange, Row),
				 Prefix, element(?usmUserPrivProtocol, Row),
				 case element(?usmUserPrivProtocol, Row) of
				     ?usmNoPrivProtocol    -> none;
				     ?usmDESPrivProtocol   -> des;
				     ?usmAesCfb128Protocol -> aes;
				     des -> des;
				     aes -> aes;
				     _ -> undefined
				 end,
				 Prefix, element(?usmUserPrivKeyChange, Row),
				 Prefix, element(?usmUserOwnPrivKeyChange, Row),
				 Prefix, element(?usmUserPublic, Row),
				 Prefix, element(?usmUserStorageType, Row),
				 case element(?usmUserStorageType, Row) of
				     ?'usmUserStorageType_readOnly' -> readOnly;
				     ?'usmUserStorageType_permanent' -> permanent;
				     ?'usmUserStorageType_nonVolatile' -> nonVolatile;
				     ?'usmUserStorageType_volatile' -> volatile;
				     ?'usmUserStorageType_other' -> other;
				     _ -> undefined
				 end,
				 Prefix, element(?usmUserStatus, Row),
				 case element(?usmUserStatus, Row) of
				     ?'usmUserStatus_destroy' -> destroy;
				     ?'usmUserStatus_createAndWait' -> createAndWait;
				     ?'usmUserStatus_createAndGo' -> createAndGo;
				     ?'usmUserStatus_notReady' -> notReady;
				     ?'usmUserStatus_notInService' -> notInService;
				     ?'usmUserStatus_active' -> active;
				     _ -> undefined
				 end]))
	end,
    snmpa_mib_lib:print_table(Table, DB, FOI, PrintRow);
%% Op == new | delete
usmUserTable(Op) ->
    snmp_generic:table_func(Op, db(usmUserTable)).

%% Op == get | is_set_ok | set | get_next
usmUserTable(get, RowIndex, Cols) ->
    get_patch(Cols, get(usmUserTable, RowIndex, Cols));
usmUserTable(get_next, RowIndex, Cols) ->
    next_patch(next(usmUserTable, RowIndex, Cols));
usmUserTable(is_set_ok, RowIndex, Cols0) ->
    ?vtrace("usmUserTable(is_set_ok) -> entry with"
	    "~n   RowIndex: ~p"
	    "~n   Cols0:    ~p", [RowIndex, Cols0]),
    case (catch verify_usmUserTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    ?vtrace("usmUserTable(is_set_ok) -> verified: "
		    "~n   Cols: ~p", [Cols]),
	    %% Add a dummy value for securityName; otherwise snmp_generic will
	    %% think that a value is missing, so the row can't be created.
	    %% Note: this value is only added for is_set_ok, not for set!
	    NCols = [{?usmUserSecurityName, ""} | Cols],
	    IsSetOkRes = snmp_generic:table_func(is_set_ok, RowIndex,
						 NCols, db(usmUserTable)),
	    ?vtrace("usmUserTable(is_set_ok) -> tested: "
		    "~n   IsSetOkRes: ~p", [IsSetOkRes]),
	    validate_is_set_ok(IsSetOkRes, RowIndex, Cols);
	Error ->
	    Error
    end;
usmUserTable(set, RowIndex, Cols0) ->
    ?vtrace("usmUserTable(set) -> entry with"
	    "~n   RowIndex: ~p"
	    "~n   Cols0:    ~p", [RowIndex, Cols0]),
    case (catch verify_usmUserTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    ?vtrace("usmUserTable(set) -> verified"
		    "~n   Cols: ~p", [Cols]),
        % check whether we're cloning. if so, get cloned params and add a few
        % defaults that might be needed.
	    NCols = pre_set(RowIndex, validate_clone_from(RowIndex, Cols)),
	    ?vtrace("usmUserTable(set) -> pre-set: "
		    "~n   NCols: ~p", [NCols]),
	    %% NOTE: The NCols parameter is sent to snmp_generic, but not to
	    %% validate_set!  The reason is that the columns from pre_set are
	    %% set in snmp_generic, but not used by validate_set.
	    validate_set(snmp_generic:table_func(set, RowIndex,
						 NCols, db(usmUserTable)),
			 RowIndex, Cols);
	Error ->
	    Error
    end;
usmUserTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(usmUserTable)).


verify_usmUserTable_cols([], Cols) ->
    ?vtrace("verify_usmUserTable_cols -> entry when done with"
	    "~n   Cols: ~p", [Cols]),
    {ok, lists:reverse(Cols)};
verify_usmUserTable_cols([{Col, Val0}|Cols], Acc) ->
    ?vtrace("verify_usmUserTable_cols -> entry with"
	    "~n   Col:  ~p"
	    "~n   Val0: ~p", [Col, Val0]),
    Val = verify_usmUserTable_col(Col, Val0),
    ?vtrace("verify_usmUserTable_cols -> verified: "
	    "~n   Val: ~p", [Val]),
    verify_usmUserTable_cols(Cols, [{Col, Val}|Acc]).

verify_usmUserTable_col(?usmUserEngineID, EngineID) ->
    case (catch snmp_conf:check_string(EngineID)) of
	ok ->
	    EngineID;
	_ ->
	    wrongValue(?usmUserEngineID)
    end;
verify_usmUserTable_col(?usmUserName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?usmUserName)
    end;
verify_usmUserTable_col(?usmUserSecurityName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    Name;
	_ ->
	    wrongValue(?usmUserSecurityName)
    end;
verify_usmUserTable_col(?usmUserCloneFrom, Clone) ->
    case Clone of
	zeroDotZero  -> ?zeroDotZero;
	?zeroDotZero -> ?zeroDotZero;
	_ ->
	    case (catch snmp_conf:check_oid(Clone)) of
		ok ->
		    Clone;
		_ ->
		    wrongValue(?usmUserCloneFrom)
	    end
    end;
verify_usmUserTable_col(?usmUserAuthProtocol, AuthP) ->
    case AuthP of
	usmNoAuthProtocol       -> ?usmNoAuthProtocol;
	usmHMACSHAAuthProtocol  -> ?usmHMACSHAAuthProtocol;
	usmHMACMD5AuthProtocol  -> ?usmHMACMD5AuthProtocol;
	?usmNoAuthProtocol      -> ?usmNoAuthProtocol;
	?usmHMACSHAAuthProtocol -> ?usmHMACSHAAuthProtocol;
	?usmHMACMD5AuthProtocol -> ?usmHMACMD5AuthProtocol;
	_ ->
	    wrongValue(?usmUserAuthProtocol)
    end;
verify_usmUserTable_col(?usmUserAuthKeyChange, AKC) ->
    case (catch snmp_conf:check_string(AKC)) of
	ok ->
	    AKC;
	_ ->
	    wrongValue(?usmUserAuthKeyChange)
    end;
verify_usmUserTable_col(?usmUserOwnAuthKeyChange, OAKC) ->
    case (catch snmp_conf:check_string(OAKC)) of
	ok ->
	    OAKC;
	_ ->
	    wrongValue(?usmUserOwnAuthKeyChange)
    end;
verify_usmUserTable_col(?usmUserPrivProtocol, PrivP) ->
    case PrivP of
	usmNoPrivProtocol     -> ?usmNoPrivProtocol;
	usmDESPrivProtocol    -> ?usmDESPrivProtocol;
	usmAesCfb128Protocol  -> ?usmAesCfb128Protocol;
	?usmNoPrivProtocol    -> ?usmNoPrivProtocol;
	?usmDESPrivProtocol   -> ?usmDESPrivProtocol;
	?usmAesCfb128Protocol -> ?usmAesCfb128Protocol;
	_ ->
	    wrongValue(?usmUserPrivProtocol)
    end;
verify_usmUserTable_col(?usmUserPrivKeyChange, PKC) ->
    case (catch snmp_conf:check_string(PKC)) of
	ok ->
	    PKC;
	_ ->
	    wrongValue(?usmUserPrivKeyChange)
    end;
verify_usmUserTable_col(?usmUserOwnPrivKeyChange, OPKC) ->
    case (catch snmp_conf:check_string(OPKC)) of
	ok ->
	    OPKC;
	_ ->
	    wrongValue(?usmUserOwnPrivKeyChange)
    end;
verify_usmUserTable_col(?usmUserPublic, Public) ->
    case (catch snmp_conf:check_string(Public)) of
	ok ->
	    Public;
	_ ->
	    wrongValue(?usmUserPublic)
    end;
verify_usmUserTable_col(_, Val) ->
    Val.
				      

%% Patch the values stored in the DB with other values for some
%% objects.
get_patch([?usmUserCloneFrom | Cols], [{value, _Val} | Vals]) ->
    [{value, ?zeroDotZero} | get_patch(Cols, Vals)];
get_patch([?usmUserAuthKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([?usmUserOwnAuthKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([?usmUserPrivKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([?usmUserOwnPrivKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([_Col | Cols], [Val | Vals]) ->
    [Val | get_patch(Cols, Vals)];
get_patch(_Cols, Result) ->
    Result.

next_patch([{[?usmUserCloneFrom | Idx], _Val} | Vals]) ->
    [{[?usmUserCloneFrom | Idx], ?zeroDotZero} | next_patch(Vals)];
next_patch([{[?usmUserAuthKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserAuthKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([{[?usmUserOwnAuthKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserOwnAuthKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([{[?usmUserPrivKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserPrivKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([{[?usmUserOwnPrivKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserOwnPrivKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([Val | Vals]) ->
    [Val | next_patch(Vals)];
next_patch(Result) -> Result.


validate_is_set_ok({noError, 0}, RowIndex, Cols) ->
    case (catch do_validate_is_set_ok(RowIndex, Cols)) of
	ok ->
	    {noError, 0};
	Error ->
	    Error
    end;
validate_is_set_ok(Error, _RowIndex, _Cols) ->
    Error.

do_validate_is_set_ok(RowIndex, Cols) ->
    NCols = validate_clone_from(RowIndex, Cols),
    validate_auth_protocol(RowIndex, NCols),
    validate_auth_key_change(RowIndex, NCols),
    validate_own_auth_key_change(RowIndex, NCols),
    validate_priv_protocol(RowIndex, NCols),
    validate_priv_key_change(RowIndex, NCols),
    validate_own_priv_key_change(RowIndex, NCols),
    ok.
    
pre_set(RowIndex, Cols) ->
    %% Remove the ?is_cloning member again; it must no longer be
    %% present.
    Cols0 = key1delete(?is_cloning, Cols),
    %% Possibly initialize the usmUserSecurityName and privacy keys
    case snmp_generic:table_row_exists(db(usmUserTable), RowIndex) of
	true -> Cols0;
	false ->
	    SecName = get_user_name(RowIndex),
	    Cols1 = [{?usmUserSecurityName, SecName} | Cols0],
        case proplists:get_value(?is_cloning, Cols) of
            true ->
                % the row is just being cloned. the cloned user's
                % passwords are already present in Cols and must
                % not be overwritten.
                Cols1;
            _ ->
                Cols1 ++ [{?usmUserAuthKey, ""},
                    {?usmUserPrivKey, ""}]
        end
    end.

validate_set({noError, 0}, RowIndex, Cols) ->
    %% Now, all is_set_ok validation steps have been executed.  So
    %% everything is ready for the set.
    set_auth_key_change(RowIndex, Cols),
    set_own_auth_key_change(RowIndex, Cols),
    set_priv_key_change(RowIndex, Cols),
    set_own_priv_key_change(RowIndex, Cols),
    {noError, 0};
validate_set(Error, _RowIndex, _Cols) ->
    Error.

%%-----------------------------------------------------------------
%% Here's the alg: If this is the first time the CloneFrom is written,
%% we must check that the CloneFrom row exists, so we can invoke the
%% clone process in the set phase.  Otherwise, the set succed, with
%% no further checks.
%%-----------------------------------------------------------------
validate_clone_from(RowIndex, Cols) ->
    case key1search(?usmUserCloneFrom, Cols) of
	{value, {_Col, RowPointer}} ->
	    RowIndex2 = extract_row(RowPointer),
	    OldCloneFrom = snmp_generic:table_get_element(db(usmUserTable),
							  RowIndex,
							  ?usmUserCloneFrom),
	    case OldCloneFrom of
		{value, Val} when Val /= noinit ->
		    %% This means that the cloning is already done...
		    no_cloning(Cols);
		_ ->
		    %% Otherwise, we must check the CloneFrom value. It
            %% must relate to a usmUserEntry that exists and is active.
            case snmp_generic:table_get_row(db(usmUserTable), RowIndex2) of
                CloneFromRow when is_tuple(CloneFromRow) ->
                    case element(?usmUserStatus, CloneFromRow) of
                        ?'RowStatus_active' ->
                            get_cloned_cols(CloneFromRow, Cols);
                        _ ->
                            inconsistentName(?usmUserCloneFrom)
                    end;
                undefined ->
                    inconsistentName(?usmUserCloneFrom)
            end
	    end;
	false ->
        % no ?usmUserCloneFrom specified, don't modify columns
        no_cloning(Cols)
    end.

get_cloned_cols(CloneFromRow, Cols) ->
    % initialize cloned columns with data from CloneFromRow
    % and overwrite that again with data found in Cols
    AuthP = element(?usmUserAuthProtocol, CloneFromRow),
    PrivP = element(?usmUserPrivProtocol, CloneFromRow),
    AuthK = element(?usmUserAuthKey, CloneFromRow),
    PrivK = element(?usmUserPrivKey, CloneFromRow),
    ClonedCols = [{?usmUserAuthProtocol, AuthP},
        {?usmUserPrivProtocol, PrivP},
        {?usmUserAuthKey, AuthK},
        {?usmUserPrivKey, PrivK},
        {?is_cloning, true}
    ],
    Func = fun({Col, _} = Item, NCols) ->
            key1store(Col, NCols, Item)
    end,
    Cols1 = lists:foldl(Func, ClonedCols, Cols),
    key1sort(Cols1).

no_cloning(Cols0) ->
    Cols1 = key1delete(?usmUserCloneFrom, Cols0),
    key1delete(?is_cloning, Cols1).


validate_auth_protocol(RowIndex, Cols) ->
    case key1search(?usmUserAuthProtocol, Cols) of
	{value, {_Col, AuthProtocol}} ->
	    %% Check if the row is being cloned; we can't check the
	    %% old value of authProtocol, because if the row was
	    %% createAndWaited, the default value would have been
	    %% written (usmNoAuthProtocol).
	    IsCloning = proplists:get_value(?is_cloning, Cols, false),
	    if
		not IsCloning ->
		    %% This means that the row is not being cloned right
            %% now; set is ok if new protocol is usmNoAuthProtocol
		    case AuthProtocol of
			?usmNoAuthProtocol ->
			    %% Check that the Priv protocl is noPriv
			    case get_priv_proto(RowIndex, Cols) of
				?usmNoPrivProtocol -> ok;
				_ -> inconsistentValue(?usmUserAuthProtocol)
			    end;
			?usmHMACMD5AuthProtocol ->
			    inconsistentValue(?usmUserAuthProtocol);
			?usmHMACSHAAuthProtocol ->
			    inconsistentValue(?usmUserAuthProtocol);
			_ ->
			    wrongValue(?usmUserAuthProtocol)
		    end;
		true ->
		    %% Otherwise, check that the new protocol is known,
		    %% and that the system we're running supports the
		    %% hash function.
		    case AuthProtocol of
			?usmNoAuthProtocol ->
			    %% Check that the Priv protocl is noPriv
			    case get_priv_proto(RowIndex, Cols) of
				?usmNoPrivProtocol -> ok;
				_ -> inconsistentValue(?usmUserAuthProtocol)
			    end;
			?usmHMACMD5AuthProtocol ->
			    case is_crypto_supported(md5) of
				true -> ok;
				false ->
				    wrongValue(?usmUserAuthProtocol)
			    end;
			?usmHMACSHAAuthProtocol ->
			    case is_crypto_supported(sha) of
				true -> ok;
				false ->
				    wrongValue(?usmUserAuthProtocol)
			    end;
			_ -> wrongValue(?usmUserAuthProtocol)
		    end
	    end;
	false ->
	    ok
    end.

validate_auth_key_change(RowIndex, Cols) ->
    validate_key_change(RowIndex, Cols, ?usmUserAuthKeyChange, auth).

validate_own_auth_key_change(RowIndex, Cols) ->
    validate_requester(RowIndex, Cols, ?usmUserOwnAuthKeyChange),
    validate_key_change(RowIndex, Cols, ?usmUserOwnAuthKeyChange, auth).

validate_priv_key_change(RowIndex, Cols) ->
    validate_key_change(RowIndex, Cols, ?usmUserPrivKeyChange, priv).

validate_own_priv_key_change(RowIndex, Cols) ->
    validate_requester(RowIndex, Cols, ?usmUserOwnPrivKeyChange),
    validate_key_change(RowIndex, Cols, ?usmUserOwnPrivKeyChange, priv).

%% Check that the requesting user is the same as the modified user
validate_requester(RowIndex, Cols, KeyChangeCol) ->
    case key1search(KeyChangeCol, Cols) of
	{value, _} ->
	    case get(sec_model) of % Check the securityModel in the request
		?SEC_USM -> ok;
		_ -> noAccess(KeyChangeCol)
	    end,
	    %% The SecurityName may not be set yet.  First, check if it is set.
	    SecNameForUser = 
		case snmp_generic:table_get_element(db(usmUserTable),
						    RowIndex,
						    ?usmUserSecurityName) of
		    {value, Val} when Val /= noinit -> Val;
		    _ -> get_user_name(RowIndex)
		end,
	    case get(sec_name) of % Check the securityName in the request
		SecNameForUser -> ok;
		_ -> noAccess(KeyChangeCol)
	    end;
	false ->
	    ok
    end.

validate_key_change(RowIndex, Cols, KeyChangeCol, Type) ->
    case key1search(KeyChangeCol, Cols) of
	{value, {_Col, KeyC}} ->
	    %% Check if the row has been cloned; or if it is cloned in
	    %% this set-operation.
	    OldCloneFrom = snmp_generic:table_get_element(db(usmUserTable),
							  RowIndex,
							  ?usmUserCloneFrom),
	    IsClonePresent = proplists:get_value(?is_cloning, Cols, false),
	    %% Set is ok if 1) the user already is created, 2) this is
	    %% a new user, which has been cloned, or is about to be
	    %% cloned.
	    case {OldCloneFrom, IsClonePresent} of
		{{value, Val}, _} when Val /= noinit ->
		    %% The user exists, or has been cloned
		    ok;
		{_, true} ->
		    %% The user is cloned in this operation
		    ok;
		_ ->
		    %% The user doesn't exist, or hasn't been cloned,
		    %% and is not cloned in this operation.
		    inconsistentName(KeyChangeCol)
	    end,
	    %% Check that the length makes sense
	    Len = length(KeyC),
	    case Type of
		auth ->
		    case get_auth_proto(RowIndex, Cols) of
			?usmNoAuthProtocol -> ok;
			?usmHMACMD5AuthProtocol when Len =:= 32 -> ok;
			?usmHMACSHAAuthProtocol when Len =:= 40 -> ok;
			_ -> wrongValue(KeyChangeCol)
		    end;
		priv ->
		    case get_priv_proto(RowIndex, Cols) of
			?usmNoPrivProtocol -> ok;
			?usmDESPrivProtocol when Len == 32 -> ok;
			?usmAesCfb128Protocol when Len == 32 -> ok;
			_ -> wrongValue(KeyChangeCol)
		    end
	    end;
	false ->
	    ok
    end.

validate_priv_protocol(RowIndex, Cols) ->
    case key1search(?usmUserPrivProtocol, Cols) of
	{value, {_Col, PrivProtocol}} ->
	    %% Check if the row has been cloned; we can't check the
	    %% old value of privhProtocol, because if the row was
	    %% createAndWaited, the default value would have been
	    %% written (usmNoPrivProtocol).
        IsCloning = proplists:get_value(?is_cloning, Cols, false),
	    if
        not IsCloning ->
		    %% This means that the cloning is already done; set is ok
		    %% if new protocol is usmNoPrivProtocol
		    case PrivProtocol of
			?usmNoPrivProtocol ->
			    ok;
			?usmDESPrivProtocol ->
			    inconsistentValue(?usmUserPrivProtocol);
			?usmAesCfb128Protocol ->
			    inconsistentValue(?usmUserPrivProtocol);
			_ ->
			    wrongValue(?usmUserPrivProtocol)
		    end;
		true ->
		    %% Otherwise, check that the new protocol is known,
		    %% and that the system we're running supports the
		    %% crypto function.
		    case PrivProtocol of
			?usmNoPrivProtocol ->
			    ok;
			?usmDESPrivProtocol ->
			    %% The 'catch' handles the case when 'crypto' is
			    %% not present in the system.
			    case is_crypto_supported(des_cbc) of
				true ->
				    case get_auth_proto(RowIndex, Cols) of
					?usmNoAuthProtocol ->
					    inconsistentValue(?usmUserPrivProtocol);
					_ ->
					    ok
				    end;
				false -> 
				    wrongValue(?usmUserPrivProtocol)
			    end;
			?usmAesCfb128Protocol ->
			    %% The 'catch' handles the case when 'crypto' is
			    %% not present in the system.
			    case is_crypto_supported(aes_cfb128) of
				true ->
				    case get_auth_proto(RowIndex, Cols) of
					?usmNoAuthProtocol ->
					    inconsistentValue(?usmUserPrivProtocol);
					_ ->
					    ok
				    end;
				false -> 
				    wrongValue(?usmUserPrivProtocol)
			    end;
			_ -> wrongValue(?usmUserPrivProtocol)
		    end
	    end;
	false ->
	    ok
    end.


set_auth_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserAuthKeyChange, auth).

set_own_auth_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserOwnAuthKeyChange, auth).

set_priv_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserPrivKeyChange, priv).

set_own_priv_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserOwnPrivKeyChange, priv).

set_key_change(RowIndex, Cols, KeyChangeCol, Type) ->
    case key1search(KeyChangeCol, Cols) of
	{value, {_Col, KeyChange}} ->
	    KeyCol = case Type of
			 auth -> ?usmUserAuthKey;
			 priv -> ?usmUserPrivKey
		     end,
	    [AuthP, Key] =
		snmp_generic:table_get_elements(db(usmUserTable),
						RowIndex,
						[?usmUserAuthProtocol,
						 KeyCol]),
	    NewKey = extract_new_key(AuthP, Key, KeyChange),
	    snmp_generic:table_set_element(db(usmUserTable), RowIndex,
					   KeyCol, NewKey);
	false ->
	    ok
    end.

%% Extract the UserName part from a RowIndex.
get_user_name([L1 | Rest])         -> get_user_name(L1, Rest).
get_user_name(0, [_L2 | UserName]) -> UserName;
get_user_name(N, [_H | T])          -> get_user_name(N-1, T).

extract_row(RowPtr)                     -> extract_row(?usmUserEntry, RowPtr).
extract_row([H | T], [H | T2])          -> extract_row(T, T2);
extract_row([], [?usmUserSecurityName | T]) -> T;
extract_row(_, _) -> wrongValue(?usmUserCloneFrom).

%% Pre: the user exists or is being cloned in this operation
get_auth_proto(RowIndex, Cols) ->
    %% The protocol can be changed by the request too, otherwise,
    %% check the stored protocol.
    case key1search(?usmUserAuthProtocol, Cols) of
	{value, {_, Protocol}} ->
	    Protocol;
	false ->
	    %% OTP-3596
	    case snmp_generic:table_get_element(db(usmUserTable),
						RowIndex,
						?usmUserAuthProtocol) of
		{value, Protocol} ->
		    Protocol;
		_ ->
		    undefined
	    end
    end.

%% Pre: the user exists or is being cloned in this operation
get_priv_proto(RowIndex, Cols) ->
    %% The protocol can be changed by the request too, otherwise,
    %% check the stored protocol.
    case key1search(?usmUserPrivProtocol, Cols) of
	{value, {_, Protocol}} ->
	    Protocol;
	false ->
	    %% OTP-3596
	    case snmp_generic:table_get_element(db(usmUserTable),
						RowIndex,
						?usmUserPrivProtocol) of
		{value, Protocol} ->
		    Protocol;
		_ ->
		    undefined
	    end
    end.


db(X) -> snmpa_agent:db(X).

fa(usmUserTable) -> ?usmUserSecurityName.
 
foi(usmUserTable) -> ?usmUserEngineID.
 
noc(usmUserTable) -> 13.

stc(usmUserTable) -> ?usmUserStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).

 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

%%-----------------------------------------------------------------
%% Key change functions.  The KeyChange Texual-Convention is
%% defined in the SNMP-USER-BASED-SM-MIB.
%% Note that this implementation supports md5 and sha, which
%% both have fixed length requirements on the length of the key;
%% thus the implementation can be (and is) simplified.
%%-----------------------------------------------------------------
mk_key_change(Hash, OldKey, NewKey) ->
    KeyLen = length(NewKey),
    Alg = case Hash of
	      ?usmHMACMD5AuthProtocol -> md5;
	      ?usmHMACSHAAuthProtocol -> sha;
	      md5 -> md5;
	      sha -> sha
	  end,
    Random = mk_random(KeyLen),
    mk_key_change(Alg, OldKey, NewKey, KeyLen, Random).

%% This function is only exported for test purposes.  There is a test
%% case in the standard where Random is pre-defined.
mk_key_change(Alg, OldKey, NewKey, KeyLen, Random) ->
    %% OldKey and Random is of length KeyLen...
    Digest = lists:sublist(binary_to_list(crypto:hash(Alg, OldKey++Random)), KeyLen),
    %% ... and so is Digest
    Delta = snmp_misc:str_xor(Digest, NewKey),
    Random ++ Delta.

%% Extracts a new Key from a KeyChange value, sent by a manager.
extract_new_key(?usmNoAuthProtocol, OldKey, _KeyChange) ->
    OldKey;
extract_new_key(Hash, OldKey, KeyChange) ->
    KeyLen = length(OldKey),
    Alg = case Hash of
	      ?usmHMACMD5AuthProtocol -> md5;
	      ?usmHMACSHAAuthProtocol -> sha;
	      md5 -> md5;
	      sha -> sha
	  end,
    {Random, Delta} = split(KeyLen, KeyChange, []),
    Digest = lists:sublist(binary_to_list(crypto:hash(Alg, OldKey++Random)), KeyLen),
    NewKey = snmp_misc:str_xor(Digest, Delta),
    NewKey.

-define(i16(Int), (Int bsr 8) band 255, Int band 255).
-define(i8(Int), Int band 255).

mk_random(Len) when Len =< 20 ->
    binary_to_list(crypto:strong_rand_bytes(Len)).
    
split(0, Rest, FirstRev) ->
    {lists:reverse(FirstRev), Rest};
split(N, [H | T], FirstRev) when N > 0 ->
    split(N-1, T, [H | FirstRev]).


-compile({inline, [{is_crypto_supported,1}]}).
is_crypto_supported(Func) ->
    snmp_misc:is_crypto_supported(Func). 


inconsistentValue(V) -> throw({inconsistentValue, V}).
inconsistentName(N)  -> throw({inconsistentName,  N}).
wrongValue(V)        -> throw({wrongValue,        V}).
noAccess(C)          -> throw({noAccess,          C}).
     
set_sname() ->
    set_sname(get(sname)).

set_sname(undefined) ->
    put(sname,conf);
set_sname(_) -> %% Keep it, if already set.
    ok.

error(Reason) ->
    throw({error, Reason}).


%%-----------------------------------------------------------------
%%     lists key-function(s) wrappers 

-compile({inline,key1delete/2}).
key1delete(Key, List) ->
    lists:keydelete(Key, 1, List).

-compile({inline,key1search/2}).
key1search(Key, List) ->
    lists:keysearch(Key, 1, List).

-compile({inline,key1store/3}).
key1store(Key, List, Elem) ->
    lists:keystore(Key, 1, List, Elem).

-compile({inline,key1sort/1}).
key1sort(List) ->
    lists:keysort(1, List).


%%-----------------------------------------------------------------

info_msg(F, A) ->
    ?snmpa_info("USM: " ++ F, A).

%% --- 

config_err(F, A) ->
    snmpa_error:config_err("[USER-BASED-SM-MIB]: " ++ F, A).

