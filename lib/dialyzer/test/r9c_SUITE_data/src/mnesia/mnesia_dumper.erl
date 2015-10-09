%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_dumper.erl,v 1.1 2008/12/17 09:53:38 mikpe Exp $
%%
-module(mnesia_dumper).

%% The InitBy arg may be one of the following:
%% scan_decisions     Initial scan for decisions
%% startup	      Initial dump during startup
%% schema_prepare     Dump initiated during schema transaction preparation
%% schema_update      Dump initiated during schema transaction commit
%% fast_schema_update A schema_update, but ignores the log file
%% user		      Dump initiated by user
%% write_threshold    Automatic dump caused by too many log writes
%% time_threshold     Automatic dump caused by timeout

%% Public interface
-export([
	 get_log_writes/0,
	 incr_log_writes/0,
	 raw_dump_table/2,
	 raw_named_dump_table/2,
	 start_regulator/0,
	 opt_dump_log/1,
	 update/3
	]).

 %% Internal stuff
-export([regulator_init/1]).

-include("mnesia.hrl").
-include_lib("kernel/include/file.hrl").

-import(mnesia_lib, [fatal/2, dbg_out/2]).

-define(REGULATOR_NAME, mnesia_dumper_load_regulator).
-define(DumpToEtsMultiplier, 4).

-record(state, {initiated_by = nobody,
		dumper = nopid,
		regulator_pid,
		supervisor_pid,
		queue = [],
		timeout}).

get_log_writes() ->
    Max = mnesia_monitor:get_env(dump_log_write_threshold),
    Prev = mnesia_lib:read_counter(trans_log_writes),
    Left = mnesia_lib:read_counter(trans_log_writes_left),
    Diff = Max - Left,
    Prev + Diff.

incr_log_writes() ->
    Left = mnesia_lib:incr_counter(trans_log_writes_left, -1),
    if
	Left > 0 ->
	    ignore;
	true ->
	    adjust_log_writes(true)
    end.

adjust_log_writes(DoCast) ->
    Token = {mnesia_adjust_log_writes, self()},
    case global:set_lock(Token, [node()], 1) of
	false ->
	    ignore; %% Somebody else is sending a dump request
	true ->
	    case DoCast of
		false ->
		    ignore;
		true ->
		    mnesia_controller:async_dump_log(write_threshold)
	    end,
	    Max = mnesia_monitor:get_env(dump_log_write_threshold),
	    Left = mnesia_lib:read_counter(trans_log_writes_left),
	    %% Don't care if we lost a few writes
	    mnesia_lib:set_counter(trans_log_writes_left, Max),
	    Diff = Max - Left,
	    mnesia_lib:incr_counter(trans_log_writes, Diff),
	    global:del_lock(Token, [node()])
    end.

%% Returns 'ok' or exits
opt_dump_log(InitBy) ->
    Reg = case whereis(?REGULATOR_NAME) of
	      undefined ->
		  nopid;
	      Pid when pid(Pid) ->
		  Pid
	  end,
    perform_dump(InitBy, Reg).

%% Scan for decisions
perform_dump(InitBy, Regulator) when InitBy == scan_decisions ->
    ?eval_debug_fun({?MODULE, perform_dump}, [InitBy]),

    dbg_out("Transaction log dump initiated by ~w~n", [InitBy]),
    scan_decisions(mnesia_log:previous_log_file(), InitBy, Regulator),
    scan_decisions(mnesia_log:latest_log_file(), InitBy, Regulator);

%% Propagate the log into the DAT-files
perform_dump(InitBy, Regulator) ->
    ?eval_debug_fun({?MODULE, perform_dump}, [InitBy]),
    LogState = mnesia_log:prepare_log_dump(InitBy),
    dbg_out("Transaction log dump initiated by ~w: ~w~n",
	    [InitBy, LogState]),
    adjust_log_writes(false),
    mnesia_recover:allow_garb(),
    case LogState of
	already_dumped ->
	    dumped;
	{needs_dump, Diff} ->
	    U = mnesia_monitor:get_env(dump_log_update_in_place),
	    Cont = mnesia_log:init_log_dump(),
	    case catch do_perform_dump(Cont, U, InitBy, Regulator, undefined) of
		ok ->
		    ?eval_debug_fun({?MODULE, post_dump}, [InitBy]),
		    case mnesia_monitor:use_dir() of
			true ->
			    mnesia_recover:dump_decision_tab();
			false ->
			    mnesia_log:purge_some_logs()
		    end,
		    %% And now to the crucial point...
		    mnesia_log:confirm_log_dump(Diff);
		{error, Reason} ->
		    {error, Reason};
		{'EXIT', {Desc, Reason}} ->
		    case mnesia_monitor:get_env(auto_repair) of
			true ->
			    mnesia_lib:important(Desc, Reason),
			    %% Ignore rest of the log
			    mnesia_log:confirm_log_dump(Diff);
			false ->
			    fatal(Desc, Reason)
		    end
	    end;
	{error, Reason} ->
	    {error, {"Cannot prepare log dump", Reason}}
    end.

scan_decisions(Fname, InitBy, Regulator) ->
    Exists = mnesia_lib:exists(Fname),
    case Exists of
	false ->
	    ok;
	true ->
	    Header = mnesia_log:trans_log_header(),
	    Name = previous_log,
	    mnesia_log:open_log(Name, Header, Fname, Exists,
				mnesia_monitor:get_env(auto_repair), read_only),
	    Cont = start,
	    Res = (catch do_perform_dump(Cont, false, InitBy, Regulator, undefined)),
	    mnesia_log:close_log(Name),
	    case Res of
		ok -> ok;
		{'EXIT', Reason} -> {error, Reason}
	    end
    end.

do_perform_dump(Cont, InPlace, InitBy, Regulator, OldVersion) ->
    case mnesia_log:chunk_log(Cont) of
	{C2, Recs} ->
	    case catch insert_recs(Recs, InPlace, InitBy, Regulator, OldVersion) of
		{'EXIT', R} ->
		    Reason = {"Transaction log dump error: ~p~n", [R]},
		    close_files(InPlace, {error, Reason}, InitBy),
		    exit(Reason);
		Version ->
		    do_perform_dump(C2, InPlace, InitBy, Regulator, Version)
	    end;
	eof ->
	    close_files(InPlace, ok, InitBy),
	    ok
    end.

insert_recs([Rec | Recs], InPlace, InitBy, Regulator, LogV) ->
    regulate(Regulator),
    case insert_rec(Rec, InPlace, InitBy, LogV) of
	LogH when record(LogH, log_header) ->
	    insert_recs(Recs, InPlace, InitBy, Regulator, LogH#log_header.log_version);
	_ ->
	    insert_recs(Recs, InPlace, InitBy, Regulator, LogV)
    end;

insert_recs([], _InPlace, _InitBy, _Regulator, Version) ->
    Version.

insert_rec(Rec, _InPlace, scan_decisions, _LogV) ->
    if
	record(Rec, commit) ->
	    ignore;
	record(Rec, log_header) ->
	    ignore;
	true ->
	    mnesia_recover:note_log_decision(Rec, scan_decisions)
    end;
insert_rec(Rec, InPlace, InitBy, LogV) when record(Rec, commit) ->
    %% Determine the Outcome of the transaction and recover it
    D = Rec#commit.decision,
    case mnesia_recover:wait_for_decision(D, InitBy) of
	{Tid, committed} ->
	    do_insert_rec(Tid, Rec, InPlace, InitBy, LogV);
	{Tid, aborted} ->
	    mnesia_schema:undo_prepare_commit(Tid, Rec)
    end;
insert_rec(H, _InPlace, _InitBy, _LogV) when record(H, log_header) ->
    CurrentVersion = mnesia_log:version(),
    if
        H#log_header.log_kind /= trans_log ->
	    exit({"Bad kind of transaction log", H});
	H#log_header.log_version == CurrentVersion ->
	    ok;
	H#log_header.log_version == "4.2" ->
	    ok;
	H#log_header.log_version == "4.1" ->
	    ok;
	H#log_header.log_version == "4.0" ->
	    ok;
	true ->
	    fatal("Bad version of transaction log: ~p~n", [H])
    end,
    H;

insert_rec(_Rec, _InPlace, _InitBy, _LogV) ->
    ok.

do_insert_rec(Tid, Rec, InPlace, InitBy, LogV) ->
    case Rec#commit.schema_ops of
	[] ->
	    ignore;
	SchemaOps ->
	    case val({schema, storage_type}) of
		ram_copies ->
		    insert_ops(Tid, schema_ops, SchemaOps, InPlace, InitBy, LogV);
	        Storage ->
		    true = open_files(schema, Storage, InPlace, InitBy),
		    insert_ops(Tid, schema_ops, SchemaOps, InPlace, InitBy, LogV)
	    end
    end,
    D = Rec#commit.disc_copies,
    insert_ops(Tid, disc_copies, D, InPlace, InitBy, LogV),
    case InitBy of
	startup ->
	    DO = Rec#commit.disc_only_copies,
	    insert_ops(Tid, disc_only_copies, DO, InPlace, InitBy, LogV);
	_ ->
	    ignore
    end.


update(_Tid, [], _DumperMode) ->
    dumped;
update(Tid, SchemaOps, DumperMode) ->
    UseDir = mnesia_monitor:use_dir(),
    Res = perform_update(Tid, SchemaOps, DumperMode, UseDir),
    mnesia_controller:release_schema_commit_lock(),
    Res.

perform_update(_Tid, _SchemaOps, mandatory, true) ->
    %% Force a dump of the transaction log in order to let the
    %% dumper perform needed updates

    InitBy = schema_update,
    ?eval_debug_fun({?MODULE, dump_schema_op}, [InitBy]),
    opt_dump_log(InitBy);
perform_update(Tid, SchemaOps, _DumperMode, _UseDir) ->
    %% No need for a full transaction log dump.
    %% Ignore the log file and perform only perform
    %% the corresponding updates.

    InitBy = fast_schema_update,
    InPlace = mnesia_monitor:get_env(dump_log_update_in_place),
    ?eval_debug_fun({?MODULE, dump_schema_op}, [InitBy]),
    case catch insert_ops(Tid, schema_ops, SchemaOps, InPlace, InitBy,
			  mnesia_log:version()) of
	{'EXIT', Reason} ->
	    Error = {error, {"Schema update error", Reason}},
	    close_files(InPlace, Error, InitBy),
            fatal("Schema update error ~p ~p", [Reason, SchemaOps]);
	_ ->
	    ?eval_debug_fun({?MODULE, post_dump}, [InitBy]),
	    close_files(InPlace, ok, InitBy),
	    ok
    end.

insert_ops(_Tid, _Storage, [], _InPlace, _InitBy, _) ->    ok;
insert_ops(Tid, Storage, [Op], InPlace, InitBy, Ver)  when Ver >= "4.3"->
    insert_op(Tid, Storage, Op, InPlace, InitBy),
    ok;
insert_ops(Tid, Storage, [Op | Ops], InPlace, InitBy, Ver)  when Ver >= "4.3"->
    insert_op(Tid, Storage, Op, InPlace, InitBy),
    insert_ops(Tid, Storage, Ops, InPlace, InitBy, Ver);
insert_ops(Tid, Storage, [Op | Ops], InPlace, InitBy, Ver) when Ver < "4.3" ->
    insert_ops(Tid, Storage, Ops, InPlace, InitBy, Ver),
    insert_op(Tid, Storage, Op, InPlace, InitBy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Normal ops

disc_insert(_Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy) ->
    case open_files(Tab, Storage, InPlace, InitBy) of
	true ->
	    case Storage of
		disc_copies when Tab /= schema ->
		    mnesia_log:append({?MODULE,Tab}, {{Tab, Key}, Val, Op}),
		    ok;
		_ ->
		    case Op of
			write ->
			    ok = dets:insert(Tab, Val);
			delete ->
			    ok = dets:delete(Tab, Key);
			update_counter ->
			    {RecName, Incr} = Val,
			    case catch dets:update_counter(Tab, Key, Incr) of
				CounterVal when integer(CounterVal) ->
				    ok;
				_ ->
				    Zero = {RecName, Key, 0},
				    ok = dets:insert(Tab, Zero)
			    end;
			delete_object ->
			    ok = dets:delete_object(Tab, Val);
			clear_table ->
			    ok = dets:match_delete(Tab, '_')
		    end
	    end;
	false ->
	    ignore
    end.

insert(Tid, Storage, Tab, Key, [Val | Tail], Op, InPlace, InitBy) ->
    insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy),
    insert(Tid, Storage, Tab, Key, Tail, Op, InPlace, InitBy);

insert(_Tid, _Storage, _Tab, _Key, [], _Op, _InPlace, _InitBy) ->
    ok;

insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy) ->
    Item = {{Tab, Key}, Val, Op},
    case InitBy of
	startup ->
	    disc_insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy);

	_ when Storage == ram_copies ->
	    mnesia_tm:do_update_op(Tid, Storage, Item),
	    Snmp = mnesia_tm:prepare_snmp(Tab, Key, [Item]),
	    mnesia_tm:do_snmp(Tid, Snmp);

	_ when Storage == disc_copies ->
	    disc_insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy),
	    mnesia_tm:do_update_op(Tid, Storage, Item),
	    Snmp = mnesia_tm:prepare_snmp(Tab, Key, [Item]),
	    mnesia_tm:do_snmp(Tid, Snmp);

	_ when Storage == disc_only_copies ->
	    mnesia_tm:do_update_op(Tid, Storage, Item),
	    Snmp = mnesia_tm:prepare_snmp(Tab, Key, [Item]),
	    mnesia_tm:do_snmp(Tid, Snmp);

	_ when Storage == unknown ->
	    ignore
    end.

disc_delete_table(Tab, Storage) ->
    case mnesia_monitor:use_dir() of
	true ->
	    if
		Storage == disc_only_copies; Tab == schema ->
		    mnesia_monitor:unsafe_close_dets(Tab),
		    Dat = mnesia_lib:tab2dat(Tab),
		    file:delete(Dat);
		true ->
		    DclFile = mnesia_lib:tab2dcl(Tab),
		    case get({?MODULE,Tab}) of
			{opened_dumper, dcl} ->
			    del_opened_tab(Tab),
			    mnesia_log:unsafe_close_log(Tab);
			_ ->
			    ok
		    end,
		    file:delete(DclFile),
		    DcdFile = mnesia_lib:tab2dcd(Tab),
		    file:delete(DcdFile),
		    ok
	    end,
	    erase({?MODULE, Tab});
	false ->
	    ignore
    end.

disc_delete_indecies(_Tab, _Cs, Storage) when Storage /= disc_only_copies ->
    ignore;
disc_delete_indecies(Tab, Cs, disc_only_copies) ->
    Indecies = Cs#cstruct.index,
    mnesia_index:del_transient(Tab, Indecies, disc_only_copies).

insert_op(Tid, Storage, {{Tab, Key}, Val, Op}, InPlace, InitBy) ->
    %% Propagate to disc only
    disc_insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NOTE that all operations below will only
%% be performed if the dump is initiated by
%% startup or fast_schema_update
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_op(_Tid, schema_ops, _OP, _InPlace, Initby)
  when Initby /= startup,
       Initby /= fast_schema_update,
       Initby /= schema_update ->
    ignore;

insert_op(Tid, _, {op, rec, Storage, Item}, InPlace, InitBy) ->
    {{Tab, Key}, ValList, Op} = Item,
    insert(Tid, Storage, Tab, Key, ValList, Op, InPlace, InitBy);

insert_op(Tid, _, {op, change_table_copy_type, N, FromS, ToS, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Val = mnesia_schema:insert_cstruct(Tid, Cs, true), % Update ram only
    {schema, Tab, _} = Val,
    if
	InitBy /= startup ->
	    mnesia_controller:add_active_replica(Tab, N, Cs);
	true ->
	    ignore
    end,
    if
	N == node() ->
	    Dmp  = mnesia_lib:tab2dmp(Tab),
	    Dat  = mnesia_lib:tab2dat(Tab),
	    Dcd  = mnesia_lib:tab2dcd(Tab),
	    Dcl  = mnesia_lib:tab2dcl(Tab),
	    case {FromS, ToS} of
		{ram_copies, disc_copies} when Tab == schema ->
		    ok = ensure_rename(Dmp, Dat);
		{ram_copies, disc_copies} ->
		    file:delete(Dcl),
		    ok = ensure_rename(Dmp, Dcd);
		{disc_copies, ram_copies} when Tab == schema ->
		    mnesia_lib:set(use_dir, false),
		    mnesia_monitor:unsafe_close_dets(Tab),
		    file:delete(Dat);
		{disc_copies, ram_copies} ->
		    file:delete(Dcl),
		    file:delete(Dcd);
		{ram_copies, disc_only_copies} ->
		    ok = ensure_rename(Dmp, Dat),
		    true = open_files(Tab, disc_only_copies, InPlace, InitBy),
		    %% ram_delete_table must be done before init_indecies,
		    %% it uses info which is reset in init_indecies,
		    %% it doesn't matter, because init_indecies don't use
		    %% the ram replica of the table when creating the disc
		    %% index; Could be improved :)
		    mnesia_schema:ram_delete_table(Tab, FromS),
		    PosList = Cs#cstruct.index,
		    mnesia_index:init_indecies(Tab, disc_only_copies, PosList);
		{disc_only_copies, ram_copies} ->
		    mnesia_monitor:unsafe_close_dets(Tab),
		    disc_delete_indecies(Tab, Cs, disc_only_copies),
		    case InitBy of
			startup ->
			    ignore;
			_ ->
			    mnesia_controller:get_disc_copy(Tab)
		    end,
		    disc_delete_table(Tab, disc_only_copies);
		{disc_copies, disc_only_copies} ->
		    ok = ensure_rename(Dmp, Dat),
		    true = open_files(Tab, disc_only_copies, InPlace, InitBy),
		    mnesia_schema:ram_delete_table(Tab, FromS),
		    PosList = Cs#cstruct.index,
		    mnesia_index:init_indecies(Tab, disc_only_copies, PosList),
		    file:delete(Dcl),
		    file:delete(Dcd);
		{disc_only_copies, disc_copies} ->
		    mnesia_monitor:unsafe_close_dets(Tab),
		    disc_delete_indecies(Tab, Cs, disc_only_copies),
		    case InitBy of
			startup ->
			    ignore;
			_ ->
			    mnesia_log:ets2dcd(Tab),
			    mnesia_controller:get_disc_copy(Tab),
			    disc_delete_table(Tab, disc_only_copies)
		    end
	    end;
	true ->
	    ignore
    end,
    S = val({schema, storage_type}),
    disc_insert(Tid, S, schema, Tab, Val, write, InPlace, InitBy);

insert_op(Tid, _, {op, transform, _Fun, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
	disc_copies ->
	    open_dcl(Cs#cstruct.name);
	_ ->
	    ignore
    end,
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

%%%  Operations below this are handled without using the logg.

insert_op(Tid, _, {op, restore_recreate, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    Type = Cs#cstruct.type,
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    %% Delete all possbibly existing files and tables
    disc_delete_table(Tab, Storage),
    disc_delete_indecies(Tab, Cs, Storage),
    case InitBy of
	startup ->
	    ignore;
	_ ->
	    mnesia_schema:ram_delete_table(Tab, Storage),
	    mnesia_checkpoint:tm_del_copy(Tab, node())
    end,
    %%    delete_cstruct(Tid, Cs, InPlace, InitBy),
    %% And create new ones..
    if
	(InitBy == startup) or (Storage == unknown) ->
	    ignore;
	Storage == ram_copies ->
	    Args = [{keypos, 2}, public, named_table, Type],
	    mnesia_monitor:mktab(Tab, Args);
	Storage == disc_copies ->
	    Args = [{keypos, 2}, public, named_table, Type],
	    mnesia_monitor:mktab(Tab, Args),
	    File = mnesia_lib:tab2dcd(Tab),
	    FArg = [{file, File}, {name, {mnesia,create}},
		    {repair, false}, {mode, read_write}],
	    {ok, Log} = mnesia_monitor:open_log(FArg),
	    mnesia_monitor:unsafe_close_log(Log);
	Storage == disc_only_copies ->
	    File = mnesia_lib:tab2dat(Tab),
	    file:delete(File),
	    Args = [{file, mnesia_lib:tab2dat(Tab)},
		    {type, mnesia_lib:disk_type(Tab, Type)},
		    {keypos, 2},
		    {repair, mnesia_monitor:get_env(auto_repair)}],
	    mnesia_monitor:open_dets(Tab, Args)
    end,
    insert_op(Tid, ignore, {op, create_table, TabDef}, InPlace, InitBy);

insert_op(Tid, _, {op, create_table, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, false, InPlace, InitBy),
    Tab = Cs#cstruct.name,
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    case InitBy of
	startup ->
	    case Storage of
		unknown ->
		    ignore;
		ram_copies ->
		    ignore;
		disc_copies ->
		    Dcd = mnesia_lib:tab2dcd(Tab),
		    case mnesia_lib:exists(Dcd) of
			true -> ignore;
			false ->
			    mnesia_log:open_log(temp,
						mnesia_log:dcl_log_header(),
						Dcd,
						false,
						false,
						read_write),
			    mnesia_log:unsafe_close_log(temp)
		    end;
		_ ->
		    Args = [{file, mnesia_lib:tab2dat(Tab)},
			    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)},
			    {keypos, 2},
			    {repair, mnesia_monitor:get_env(auto_repair)}],
		    case mnesia_monitor:open_dets(Tab, Args) of
			{ok, _} ->
			    mnesia_monitor:unsafe_close_dets(Tab);
			{error, Error} ->
			    exit({"Failed to create dets table", Error})
		    end
	    end;
	_ ->
	    Copies = mnesia_lib:copy_holders(Cs),
	    Active = mnesia_lib:intersect(Copies, val({current, db_nodes})),
	    [mnesia_controller:add_active_replica(Tab, N, Cs) || N <- Active],

	    case Storage of
		unknown ->
		    case Cs#cstruct.local_content of
			true ->
			    ignore;
			false ->
			    mnesia_lib:set_remote_where_to_read(Tab)
		    end;
		_ ->
		    case Cs#cstruct.local_content of
			true ->
			    mnesia_lib:set_local_content_whereabouts(Tab);
			false ->
			    mnesia_lib:set({Tab, where_to_read}, node())
		    end,
		    case Storage of
			ram_copies ->
			    ignore;
			_ ->
			    %% Indecies are still created by loader
			    disc_delete_indecies(Tab, Cs, Storage)
			    %% disc_delete_table(Tab, Storage)
		    end,

		    %% Update whereabouts and create table
		    mnesia_controller:create_table(Tab)
	    end
    end;

insert_op(_Tid, _, {op, dump_table, Size, TabDef}, _InPlace, _InitBy) ->
    case Size of
	unknown ->
	    ignore;
	_ ->
	    Cs = mnesia_schema:list2cs(TabDef),
	    Tab = Cs#cstruct.name,
	    Dmp = mnesia_lib:tab2dmp(Tab),
	    Dat = mnesia_lib:tab2dcd(Tab),
	    case Size of
		0 ->
		    %% Assume that table files already are closed
		    file:delete(Dmp),
		    file:delete(Dat);
		_ ->
		    ok = ensure_rename(Dmp, Dat)
	    end
    end;

insert_op(Tid, _, {op, delete_table, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
	unknown ->
	    ignore;
	Storage ->
	    disc_delete_table(Tab, Storage),
	    disc_delete_indecies(Tab, Cs, Storage),
	    case InitBy of
		startup ->
		    ignore;
		_ ->
		    mnesia_schema:ram_delete_table(Tab, Storage),
		    mnesia_checkpoint:tm_del_copy(Tab, node())
	    end
    end,
    delete_cstruct(Tid, Cs, InPlace, InitBy);

insert_op(Tid, _, {op, clear_table, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
	unknown ->
	    ignore;
	Storage ->
	    Oid = '_', %%val({Tab, wild_pattern}),
	    if Storage == disc_copies ->
		    open_dcl(Cs#cstruct.name);
	       true ->
		    ignore
	    end,
	    insert(Tid, Storage, Tab, '_', Oid, clear_table, InPlace, InitBy)
    end;

insert_op(Tid, _, {op, merge_schema, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, false, InPlace, InitBy);

insert_op(Tid, _, {op, del_table_copy, Storage, Node, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    if
	Tab == schema, Storage == ram_copies ->
	    insert_cstruct(Tid, Cs, true, InPlace, InitBy);
        Tab /= schema ->
	    mnesia_controller:del_active_replica(Tab, Node),
	    mnesia_lib:del({Tab, Storage}, Node),
	    if
		Node == node() ->
		    case Cs#cstruct.local_content of
			true -> mnesia_lib:set({Tab, where_to_read}, nowhere);
			false -> mnesia_lib:set_remote_where_to_read(Tab)
		    end,
		    mnesia_lib:del({schema, local_tables}, Tab),
		    mnesia_lib:set({Tab, storage_type}, unknown),
		    insert_cstruct(Tid, Cs, true, InPlace, InitBy),
		    disc_delete_table(Tab, Storage),
		    disc_delete_indecies(Tab, Cs, Storage),
		    mnesia_schema:ram_delete_table(Tab, Storage),
		    mnesia_checkpoint:tm_del_copy(Tab, Node);
		true ->
		    case val({Tab, where_to_read}) of
			Node ->
			    mnesia_lib:set_remote_where_to_read(Tab);
			_  ->
			    ignore
		    end,
		    insert_cstruct(Tid, Cs, true, InPlace, InitBy)
	    end
    end;

insert_op(Tid, _, {op, add_table_copy, _Storage, _Node, TabDef}, InPlace, InitBy) ->
    %% During prepare commit, the files was created
    %% and the replica was announced
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, add_snmp, _Us, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, del_snmp, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    if
	InitBy /= startup,
	Storage /= unknown ->
	    case ?catch_val({Tab, {index, snmp}}) of
		{'EXIT', _} ->
		    ignore;
		Stab ->
		    mnesia_snmp_hook:delete_table(Tab, Stab),
		    mnesia_lib:unset({Tab, {index, snmp}})
	    end;
	true ->
	    ignore
    end,
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, add_index, Pos, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = insert_cstruct(Tid, Cs, true, InPlace, InitBy),
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    case InitBy of
	startup when Storage == disc_only_copies ->
	    mnesia_index:init_indecies(Tab, Storage, [Pos]);
	startup ->
	    ignore;
	_  ->
	    mnesia_index:init_indecies(Tab, Storage, [Pos])
    end;

insert_op(Tid, _, {op, del_index, Pos, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    case InitBy of
	startup when Storage == disc_only_copies ->
	    mnesia_index:del_index_table(Tab, Storage, Pos);
	startup ->
	    ignore;
	_ ->
	    mnesia_index:del_index_table(Tab, Storage, Pos)
    end,
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, change_table_access_mode,TabDef, _OldAccess, _Access}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    case InitBy of
	startup -> ignore;
	_ -> mnesia_controller:change_table_access_mode(Cs)
    end,
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, change_table_load_order, TabDef, _OldLevel, _Level}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, delete_property, TabDef, PropKey}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    mnesia_lib:unset({Tab, user_property, PropKey}),
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, write_property, TabDef, _Prop}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, true, InPlace, InitBy);

insert_op(Tid, _, {op, change_table_frag, _Change, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, true, InPlace, InitBy).

open_files(Tab, Storage, UpdateInPlace, InitBy)
  when Storage /= unknown, Storage /= ram_copies ->
    case get({?MODULE, Tab}) of
	undefined ->
	    case ?catch_val({Tab, setorbag}) of
		{'EXIT', _} ->
		    false;
		Type ->
		    case Storage of
			disc_copies when Tab /= schema ->
			    Bool = open_disc_copies(Tab, InitBy),
			    Bool;
			_ ->
			    Fname = prepare_open(Tab, UpdateInPlace),
			    Args = [{file, Fname},
				    {keypos, 2},
				    {repair, mnesia_monitor:get_env(auto_repair)},
				    {type, mnesia_lib:disk_type(Tab, Type)}],
			    {ok, _} = mnesia_monitor:open_dets(Tab, Args),
			    put({?MODULE, Tab}, {opened_dumper, dat}),
			    true
		    end
	    end;
	already_dumped ->
	    false;
	{opened_dumper, _} ->
	    true
    end;
open_files(_Tab, _Storage, _UpdateInPlace, _InitBy) ->
    false.

open_disc_copies(Tab, InitBy) ->
    DclF = mnesia_lib:tab2dcl(Tab),
    DumpEts =
	case file:read_file_info(DclF) of
	    {error, enoent} ->
		false;
	    {ok, DclInfo} ->
		DcdF =  mnesia_lib:tab2dcd(Tab),
		case file:read_file_info(DcdF) of
		    {error, Reason} ->
			mnesia_lib:dbg_out("File ~p info_error ~p ~n",
					   [DcdF, Reason]),
			true;
		    {ok, DcdInfo} ->
			DcdInfo#file_info.size =<
			    (DclInfo#file_info.size *
			     ?DumpToEtsMultiplier)
		end
	end,
    if
	DumpEts == false; InitBy == startup ->
	    mnesia_log:open_log({?MODULE,Tab},
				mnesia_log:dcl_log_header(),
				DclF,
				mnesia_lib:exists(DclF),
				mnesia_monitor:get_env(auto_repair),
				read_write),
	    put({?MODULE, Tab}, {opened_dumper, dcl}),
	    true;
	true ->
	    mnesia_log:ets2dcd(Tab),
	    put({?MODULE, Tab}, already_dumped),
	    false
    end.

%% Always opens the dcl file for writing overriding already_dumped
%% mechanismen, used for schema transactions.
open_dcl(Tab) ->
    case get({?MODULE, Tab}) of
	{opened_dumper, _} ->
	    true;
	_ -> %% undefined or already_dumped
	    DclF = mnesia_lib:tab2dcl(Tab),
	    mnesia_log:open_log({?MODULE,Tab},
				mnesia_log:dcl_log_header(),
				DclF,
				mnesia_lib:exists(DclF),
				mnesia_monitor:get_env(auto_repair),
				read_write),
	    put({?MODULE, Tab}, {opened_dumper, dcl}),
	    true
    end.

prepare_open(Tab, UpdateInPlace) ->
    Dat =  mnesia_lib:tab2dat(Tab),
    case UpdateInPlace of
	true ->
	    Dat;
	false ->
	    Tmp = mnesia_lib:tab2tmp(Tab),
	    case catch mnesia_lib:copy_file(Dat, Tmp) of
		ok ->
		    Tmp;
		Error ->
		    fatal("Cannot copy dets file ~p to ~p: ~p~n",
			  [Dat, Tmp, Error])
	    end
	end.

del_opened_tab(Tab) ->
    erase({?MODULE, Tab}).

close_files(UpdateInPlace, Outcome, InitBy) -> % Update in place
    close_files(UpdateInPlace, Outcome, InitBy, get()).

close_files(InPlace, Outcome, InitBy, [{{?MODULE, Tab}, already_dumped} | Tail]) ->
    erase({?MODULE, Tab}),
    close_files(InPlace, Outcome, InitBy, Tail);
close_files(InPlace, Outcome, InitBy, [{{?MODULE, Tab}, {opened_dumper, Type}} | Tail]) ->
    erase({?MODULE, Tab}),
    case val({Tab, storage_type}) of
	disc_only_copies when InitBy /= startup ->
	    ignore;
	disc_copies when Tab /= schema ->
	    mnesia_log:close_log({?MODULE,Tab});
	Storage ->
	    do_close(InPlace, Outcome, Tab, Type, Storage)
    end,
    close_files(InPlace, Outcome, InitBy, Tail);

close_files(InPlace, Outcome, InitBy, [_ | Tail]) ->
    close_files(InPlace, Outcome, InitBy, Tail);
close_files(_, _, _InitBy, []) ->
    ok.

%% If storage is unknown during close clean up files, this can happen if timing
%% is right and dirty_write conflicts with schema operations.
do_close(_, _, Tab, dcl, unknown) ->
    mnesia_log:close_log({?MODULE,Tab}),
    file:delete(mnesia_lib:tab2dcl(Tab));
do_close(_, _, Tab, dcl, _) ->  %% To be safe, can it happen?
    mnesia_log:close_log({?MODULE,Tab});

do_close(InPlace, Outcome, Tab, dat, Storage) ->
    mnesia_monitor:close_dets(Tab),
    if
	Storage == unknown, InPlace == true  ->
	    file:delete(mnesia_lib:tab2dat(Tab));
	InPlace == true ->
	    %% Update in place
	    ok;
	Outcome == ok, Storage /= unknown ->
	    %% Success: swap tmp files with dat files
	    TabDat = mnesia_lib:tab2dat(Tab),
	    ok = file:rename(mnesia_lib:tab2tmp(Tab), TabDat);
	true ->
	    file:delete(mnesia_lib:tab2tmp(Tab))
    end.


ensure_rename(From, To) ->
    case mnesia_lib:exists(From) of
	true ->
	    file:rename(From, To);
	false ->
	    case mnesia_lib:exists(To) of
		true ->
		    ok;
		false ->
		    {error, {rename_failed, From, To}}
	    end
    end.

insert_cstruct(Tid, Cs, KeepWhereabouts, InPlace, InitBy) ->
    Val = mnesia_schema:insert_cstruct(Tid, Cs, KeepWhereabouts),
    {schema, Tab, _} = Val,
    S = val({schema, storage_type}),
    disc_insert(Tid, S, schema, Tab, Val, write, InPlace, InitBy),
    Tab.

delete_cstruct(Tid, Cs, InPlace, InitBy) ->
    Val = mnesia_schema:delete_cstruct(Tid, Cs),
    {schema, Tab, _} = Val,
    S = val({schema, storage_type}),
    disc_insert(Tid, S, schema, Tab, Val, delete, InPlace, InitBy),
    Tab.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Raw dump of table. Dumper must have unique access to the ets table.

raw_named_dump_table(Tab, Ftype) ->
    case mnesia_monitor:use_dir() of
	true ->
	    mnesia_lib:lock_table(Tab),
	    TmpFname = mnesia_lib:tab2tmp(Tab),
	    Fname =
		case Ftype of
		    dat -> mnesia_lib:tab2dat(Tab);
		    dmp -> mnesia_lib:tab2dmp(Tab)
		end,
	    file:delete(TmpFname),
	    file:delete(Fname),
	    TabSize = ?ets_info(Tab, size),
	    TabRef = Tab,
	    DiskType = mnesia_lib:disk_type(Tab),
	    Args = [{file, TmpFname},
		    {keypos, 2},
		    %%		    {ram_file, true},
		    {estimated_no_objects, TabSize + 256},
		    {repair, mnesia_monitor:get_env(auto_repair)},
		    {type, DiskType}],
	    case mnesia_lib:dets_sync_open(TabRef, Args) of
		{ok, TabRef} ->
		    Storage = ram_copies,
		    mnesia_lib:db_fixtable(Storage, Tab, true),

		    case catch raw_dump_table(TabRef, Tab) of
			{'EXIT', Reason} ->
			    mnesia_lib:db_fixtable(Storage, Tab, false),
			    mnesia_lib:dets_sync_close(Tab),
			    file:delete(TmpFname),
			    mnesia_lib:unlock_table(Tab),
			    exit({"Dump of table to disc failed", Reason});
			ok ->
			    mnesia_lib:db_fixtable(Storage, Tab, false),
			    mnesia_lib:dets_sync_close(Tab),
			    mnesia_lib:unlock_table(Tab),
			    ok = file:rename(TmpFname, Fname)
		    end;
		{error, Reason} ->
		    mnesia_lib:unlock_table(Tab),
		    exit({"Open of file before dump to disc failed", Reason})
	    end;
	false ->
	    exit({has_no_disc, node()})
    end.

raw_dump_table(DetsRef, EtsRef) ->
    dets:from_ets(DetsRef, EtsRef).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load regulator
%%
%% This is a poor mans substitute for a fair scheduler algorithm
%% in the Erlang emulator. The mnesia_dumper process performs many
%% costly BIF invokations and must pay for this. But since the
%% Emulator does not handle this properly we must compensate for
%% this with some form of load regulation of ourselves in order to
%% not steal all computation power in the Erlang Emulator ans make
%% other processes starve. Hopefully this is a temporary solution.

start_regulator() ->
    case mnesia_monitor:get_env(dump_log_load_regulation) of
	false ->
	    nopid;
	true ->
	    N = ?REGULATOR_NAME,
	    case mnesia_monitor:start_proc(N, ?MODULE, regulator_init, [self()]) of
		{ok, Pid} ->
		    Pid;
		{error, Reason} ->
		    fatal("Failed to start ~n: ~p~n", [N, Reason])
	    end
    end.

regulator_init(Parent) ->
    %% No need for trapping exits.
    %% Using low priority causes the regulation
    process_flag(priority, low),
    register(?REGULATOR_NAME, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    regulator_loop().

regulator_loop() ->
    receive
	{regulate, From} ->
	    From ! {regulated, self()},
	    regulator_loop();
	{stop, From} ->
	    From ! {stopped, self()},
	    exit(normal)
    end.

regulate(nopid) ->
    ok;
regulate(RegulatorPid) ->
    RegulatorPid ! {regulate, self()},
    receive
	{regulated, RegulatorPid} -> ok
    end.

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', Reason} -> mnesia_lib:other_val(Var, Reason);
	Value -> Value
    end.
