%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
         needs_dump_ets/1,
	 raw_dump_table/2,
	 raw_named_dump_table/2,
	 dump_to_logfile/2,
	 load_from_logfile/3,
	 start_regulator/0,
	 opt_dump_log/1,
	 update/3,
	 snapshot_dcd/1
	]).

 %% Internal stuff
-export([regulator_init/1]).

-include("mnesia.hrl").
-include_lib("kernel/include/file.hrl").

-import(mnesia_lib, [fatal/2, dbg_out/2]).

-define(REGULATOR_NAME, mnesia_dumper_load_regulator).
-define(DumpToEtsMultiplier, 4).

get_log_writes() ->
    Max = mnesia_monitor:get_env(dump_log_write_threshold),
    Prev = mnesia_lib:read_counter(trans_log_writes),
    Left = mnesia_lib:read_counter(trans_log_writes_left),
    Diff = Max - Left,
    Prev + Diff.

incr_log_writes() ->
    Left = mnesia_lib:incr_counter(trans_log_writes_left, -1),
    if
	Left =:= 0 ->
	    adjust_log_writes(true);
	true ->
	    ignore
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
	    _ = mnesia_lib:incr_counter(trans_log_writes, Diff),
	    global:del_lock(Token, [node()])
    end.

%% Returns 'ok' or exits
opt_dump_log(InitBy) ->
    Reg = case whereis(?REGULATOR_NAME) of
	      undefined ->
		  nopid;
	      Pid when is_pid(Pid) ->
		  Pid
	  end,
    perform_dump(InitBy, Reg).

snapshot_dcd(Tables) ->
    lists:foreach(
      fun(Tab) ->
	      case mnesia_lib:storage_type_at_node(node(), Tab) of
		  disc_copies ->
		      mnesia_log:ets2dcd(Tab);
		  _ ->
		      %% Storage type was checked before queueing the op, though
		      skip
	      end
      end, Tables),
    dumped.

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
    case LogState of
	already_dumped ->
	    mnesia_recover:allow_garb(),
	    dumped;
	{needs_dump, Diff} ->
	    U = mnesia_monitor:get_env(dump_log_update_in_place),
	    Cont = mnesia_log:init_log_dump(),
	    mnesia_recover:sync(),
	    try do_perform_dump(Cont, U, InitBy, Regulator, undefined) of
		ok ->
		    ?eval_debug_fun({?MODULE, post_dump}, [InitBy]),
		    case mnesia_monitor:use_dir() of
			true ->
			    mnesia_recover:dump_decision_tab();
			false ->
			    mnesia_log:purge_some_logs()
		    end,
		    mnesia_recover:allow_garb(),
		    %% And now to the crucial point...
		    mnesia_log:confirm_log_dump(Diff)
	    catch exit:Reason when Reason =/= fatal ->
		    case mnesia_monitor:get_env(auto_repair) of
			true ->
			    mnesia_lib:important(error, Reason),
			    %% Ignore rest of the log
			    mnesia_log:confirm_log_dump(Diff);
			false ->
			    fatal(error, Reason)
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
	    try
		do_perform_dump(Cont, false, InitBy, Regulator, undefined)
	    catch exit:Reason when Reason =/= fatal ->
		    {error, Reason}
	    after mnesia_log:close_log(Name)
	    end
    end.

do_perform_dump(Cont, InPlace, InitBy, Regulator, OldVersion) ->
    case mnesia_log:chunk_log(Cont) of
	{C2, Recs} ->
	    try insert_recs(Recs, InPlace, InitBy, Regulator, OldVersion) of
		Version ->
		    do_perform_dump(C2, InPlace, InitBy, Regulator, Version)
	    catch _:R:ST when R =/= fatal ->
		    Reason = {"Transaction log dump error: ~tp~n", [{R, ST}]},
		    close_files(InPlace, {error, Reason}, InitBy),
		    exit(Reason)
	    end;
	eof ->
	    close_files(InPlace, ok, InitBy),
	    erase(mnesia_dumper_dets),
	    ok
    end.

insert_recs([Rec | Recs], InPlace, InitBy, Regulator, LogV) ->
    regulate(Regulator),
    case insert_rec(Rec, InPlace, InitBy, LogV) of
	LogH when is_record(LogH, log_header) ->
	    insert_recs(Recs, InPlace, InitBy, Regulator, LogH#log_header.log_version);
	_ ->
	    insert_recs(Recs, InPlace, InitBy, Regulator, LogV)
    end;

insert_recs([], _InPlace, _InitBy, _Regulator, Version) ->
    Version.

insert_rec(Rec, _InPlace, scan_decisions, _LogV) ->
    if
	is_record(Rec, commit) ->
	    ignore;
	is_record(Rec, log_header) ->
	    ignore;
	true ->
	    mnesia_recover:note_log_decision(Rec, scan_decisions)
    end;
insert_rec(Rec, InPlace, InitBy, LogV) when is_record(Rec, commit) ->
    %% Determine the Outcome of the transaction and recover it
    D = Rec#commit.decision,
    case mnesia_recover:wait_for_decision(D, InitBy) of
	{Tid, committed} ->
	    do_insert_rec(Tid, mnesia_tm:new_cr_format(Rec), InPlace, InitBy, LogV);
	{Tid, aborted} ->
	    case InitBy of
		startup ->
		    mnesia_schema:undo_prepare_commit(Tid, mnesia_tm:new_cr_format(Rec));
		_ ->
		    ok
	    end
    end;
insert_rec(H, _InPlace, _InitBy, _LogV) when is_record(H, log_header) ->
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
    insert_ext_ops(Tid, commit_ext(Rec), InPlace, InitBy),
    case InitBy of
	startup ->
	    DO = Rec#commit.disc_only_copies,
	    insert_ops(Tid, disc_only_copies, DO, InPlace, InitBy, LogV);
	_ ->
	    ignore
    end.

commit_ext(#commit{ext = []}) -> [];
commit_ext(#commit{ext = Ext}) ->
    case lists:keyfind(ext_copies, 1, Ext) of
        {_, C} -> C;
        false -> []
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
    try insert_ops(Tid, schema_ops, SchemaOps, InPlace, InitBy,
		   mnesia_log:version()),
	 ?eval_debug_fun({?MODULE, post_dump}, [InitBy]),
	 close_files(InPlace, ok, InitBy),
	 ok
    catch _:Reason:ST when Reason =/= fatal ->
	    Error = {error, {"Schema update error", {Reason, ST}}},
	    close_files(InPlace, Error, InitBy),
            fatal("Schema update error ~tp ~tp", [{Reason,ST}, SchemaOps])
    end.

insert_ext_ops(Tid, ExtOps, InPlace, InitBy) ->
  %% Note: ext ops cannot be part of pre-4.3 logs, so there's no need
  %% to support the old operation order, as in `insert_ops'
  lists:foreach(
    fun ({Ext, Op}) ->
        case storage_semantics(Ext) of
          Semantics when Semantics == disc_copies;
                         Semantics == disc_only_copies, InitBy == startup ->
            insert_op(Tid, Ext, Op, InPlace, InitBy);
          _Other ->
            ok
        end
    end,
    ExtOps).

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
    Semantics = storage_semantics(Storage),
    case open_files(Tab, Semantics, Storage, InPlace, InitBy) of
	true ->
	    case Semantics of
		disc_copies when Tab /= schema ->
		    mnesia_log:append({?MODULE,Tab}, {{Tab, Key}, Val, Op}),
		    ok;
		_ ->
		    dets_insert(Op,Tab,Key,Val,Storage)
	    end;
	false ->
	    ignore
    end.

%% To fix update_counter so that it behaves better.
%% i.e. if nothing have changed in tab except update_counter
%% trust that the value in the dets file is correct.
%% Otherwise we will get a double increment.
%% This is perfect but update_counter is a dirty op.

dets_insert(Op,Tab,Key,Val, Storage0) ->
    Storage = if Tab == schema -> disc_only_copies;
		 true -> Storage0
	      end,
    case Op of
	write ->
	    dets_updated(Tab,Key),
	    ok = mnesia_lib:db_put(Storage, Tab, Val);
	delete ->
	    dets_updated(Tab,Key),
	    ok = mnesia_lib:db_erase(Storage, Tab, Key);
	update_counter ->
	    case dets_incr_counter(Tab,Key) of
		true ->
		    {RecName, Incr} = Val,
		    try _ = mnesia_lib:db_update_counter(Storage, Tab, Key, Incr)
		    catch error:_ when Incr < 0 ->
			    Zero = {RecName, Key, 0},
			    ok = mnesia_lib:db_put(Storage, Tab, Zero);
			  error:_ ->
			    Init = {RecName, Key, Incr},
			    ok = mnesia_lib:db_put(Storage, Tab, Init)
		    end;
		false ->  ok
	    end;
	delete_object ->
	    dets_updated(Tab,Key),
	    mnesia_lib:db_match_erase(Storage, Tab, Val);
	clear_table ->
	    dets_cleared(Tab),
	    ok = mnesia_lib:db_match_erase(Storage, Tab, '_')
    end.

dets_updated(Tab,Key) ->
    case get(mnesia_dumper_dets) of
	undefined ->
	    Empty = gb_trees:empty(),
	    Tree = gb_trees:insert(Tab, gb_sets:singleton(Key), Empty),
	    put(mnesia_dumper_dets, Tree);
	Tree ->
	    case gb_trees:lookup(Tab,Tree) of
		{value, cleared} -> ignore;
		{value, Set} ->
		    T = gb_trees:update(Tab, gb_sets:add(Key, Set), Tree),
		    put(mnesia_dumper_dets, T);
		none ->
		    T = gb_trees:insert(Tab, gb_sets:singleton(Key), Tree),
		    put(mnesia_dumper_dets, T)
	    end
    end.

dets_incr_counter(Tab,Key) ->
    case get(mnesia_dumper_dets) of
	undefined -> false;
	Tree ->
	    case gb_trees:lookup(Tab,Tree) of
		{value, cleared} -> true;
		{value, Set} -> gb_sets:is_member(Key, Set);
		none -> false
	    end
    end.

dets_cleared(Tab) ->
    case get(mnesia_dumper_dets) of
	undefined ->
	    Empty = gb_trees:empty(),
	    Tree = gb_trees:insert(Tab, cleared, Empty),
	    put(mnesia_dumper_dets, Tree);
	Tree ->
	    case gb_trees:lookup(Tab,Tree) of
		{value, cleared} -> ignore;
		_ ->
		    T = gb_trees:enter(Tab, cleared, Tree),
		    put(mnesia_dumper_dets, T)
	    end
    end.

insert(Tid, Storage, Tab, Key, [Val | Tail], Op, InPlace, InitBy) ->
    insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy),
    insert(Tid, Storage, Tab, Key, Tail, Op, InPlace, InitBy);

insert(_Tid, _Storage, _Tab, _Key, [], _Op, _InPlace, _InitBy) ->
    ok;

insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy) ->
    Semantics = storage_semantics(Storage),
    Item = {{Tab, Key}, Val, Op},
    case InitBy of
	startup ->
	    disc_insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy);

	_ when Semantics == ram_copies ->
	    mnesia_tm:do_update_op(Tid, Storage, Item),
	    Snmp = mnesia_tm:prepare_snmp(Tab, Key, [Item]),
	    mnesia_tm:do_snmp(Tid, Snmp);

	_ when Semantics == disc_copies ->
	    disc_insert(Tid, Storage, Tab, Key, Val, Op, InPlace, InitBy),
	    mnesia_tm:do_update_op(Tid, Storage, Item),
	    Snmp = mnesia_tm:prepare_snmp(Tab, Key, [Item]),
	    mnesia_tm:do_snmp(Tid, Snmp);

	_ when Semantics == disc_only_copies ->
	    mnesia_tm:do_update_op(Tid, Storage, Item),
	    Snmp = mnesia_tm:prepare_snmp(Tab, Key, [Item]),
	    mnesia_tm:do_snmp(Tid, Snmp);

	_ when element(1, Storage) == ext ->
	    mnesia_tm:do_update_op(Tid, Storage, Item),
	    Snmp = mnesia_tm:prepare_snmp(Tab, Key, [Item]),
	    mnesia_tm:do_snmp(Tid, Snmp);

	_ when Storage == unknown ->
	    ignore
    end.

disc_delete_table(Tab, {ext, Alias, Mod}) ->
    Mod:close_table(Alias, Tab),
    Mod:delete_table(Alias, Tab);
disc_delete_table(Tab, Storage) ->
    case mnesia_monitor:use_dir() of
	true ->
	    if
		Storage == disc_only_copies; Tab == schema ->
		    mnesia_monitor:unsafe_close_dets(Tab),
		    Dat = mnesia_lib:tab2dat(Tab),
		    file:delete(Dat),
		    ok;
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
	    erase({?MODULE, Tab}),
	    ok;
	false ->
	    ok
    end.

disc_delete_indecies(Tab, Cs, Storage) ->
    case storage_semantics(Storage) of
	disc_only_copies ->
	    Indecies = Cs#cstruct.index,
	    mnesia_index:del_transient(Tab, Indecies, Storage);
	_ ->
	    ok
    end.

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
    FromSem = storage_semantics(FromS),
    ToSem = storage_semantics(ToS),
    case lists:member(N, val({current, db_nodes})) of
	true when InitBy /= startup  ->
	    mnesia_controller:add_active_replica(Tab, N, Cs);
	_ ->
	    ignore
    end,
    if
	N == node() ->
	    Dmp  = mnesia_lib:tab2dmp(Tab),
	    Dat  = mnesia_lib:tab2dat(Tab),
	    Dcd  = mnesia_lib:tab2dcd(Tab),
	    Dcl  = mnesia_lib:tab2dcl(Tab),
	    Logtmp  = mnesia_lib:tab2logtmp(Tab),
	    case {FromS, ToS} of
		{{ext,_FromAlias,_FromMod},{ext,ToAlias,ToMod}} ->
		    disc_delete_table(Tab, FromS),
                    ok = ToMod:delete_table(ToAlias, Tab),
		    ok = mnesia_monitor:unsafe_create_external(Tab, ToAlias, ToMod, TabDef),
		    ok = ToMod:load_table(ToAlias, Tab, {dumper,change_table_copy_type}, TabDef),
		    ok = load_from_logfile(ToS, Tab, Logtmp),
		    file:delete(Logtmp),
		    restore_indexes(Tab, ToS, Cs);

		{_,{ext,ToAlias,ToMod}} ->
		    case FromSem of
			ram_copies ->
			    mnesia_schema:ram_delete_table(Tab, FromS);
			_ ->
			    if FromSem == disc_copies ->
				    mnesia_schema:ram_delete_table(
				      Tab, FromS);
			       true -> ok
			    end,
			    disc_delete_table(Tab, FromS)
		    end,

                    ok = ToMod:delete_table(ToAlias, Tab),
		    ok = mnesia_monitor:unsafe_create_external(Tab, ToAlias, ToMod, TabDef),
		    ok = ToMod:load_table(ToAlias, Tab, {dumper,change_table_copy_type}, TabDef),
		    ok = load_from_logfile(ToS, Tab, Logtmp),
		    file:delete(Logtmp),
		    restore_indexes(Tab, ToS, Cs);

		{{ext,_FromAlias,_FromMod} = FromS, ToS} ->
		    disc_delete_table(Tab, FromS),
		    case ToS of
			ram_copies ->
			    change_disc_to_ram(
			      Tab, Cs, FromS, ToS, Logtmp, InitBy);
			disc_copies ->
			    Args = [{keypos, 2}, public, named_table,
				    Cs#cstruct.type],
			    mnesia_monitor:mktab(Tab, Args),
			    ok = load_from_logfile(ToS, Tab, Logtmp),
			    file:delete(Logtmp);
			disc_only_copies ->
			    %% ok = ensure_rename(Dmp, Dat),
			    true = open_files(Tab, ToS, InPlace, InitBy),
			    ok = load_from_logfile(ToS, Tab, Logtmp),
			    file:delete(Logtmp)
		    end,
		    restore_indexes(Tab, ToS, Cs);

		_NoneAreExt ->

                    case {FromSem, ToSem} of
                        {ram_copies, disc_copies} when Tab == schema ->
                            ok = ensure_rename(Dmp, Dat);
                        {ram_copies, disc_copies} ->
                            file:delete(Dcl),
                            ok = ensure_rename(Dmp, Dcd);
                        {disc_copies, ram_copies} when Tab == schema ->
                            mnesia_lib:set(use_dir, false),
                            mnesia_monitor:unsafe_close_dets(Tab),
                            ok = file:delete(Dat);
                        {disc_copies, ram_copies} ->
                            _ = file:delete(Dcl),
                            _ = file:delete(Dcd),
                            ok;
                        {ram_copies, disc_only_copies} ->
                            ok = ensure_rename(Dmp, Dat),
                            true = open_files(Tab, ToS, InPlace, InitBy),
                            %% ram_delete_table must be done before
                            %% init_indecies, it uses info which is reset
                            %% in init_indecies, it doesn't matter, because
                            %% init_indecies don't use the ram replica of
                            %% the table when creating the disc index;
                            %% Could be improved :)
                            mnesia_schema:ram_delete_table(Tab, FromS),
			    restore_indexes(Tab, ToS, Cs);
                        {disc_only_copies, ram_copies} when FromS == disc_only_copies ->
                            mnesia_monitor:unsafe_close_dets(Tab),
                            disc_delete_indecies(Tab, Cs, FromS),
                            case InitBy of
                                startup ->
                                    ignore;
                                _ ->
                                    mnesia_controller:get_disc_copy(Tab),
                                    ok
                            end,
                            disc_delete_table(Tab, FromS);
                        {disc_only_copies, ram_copies} when element(1, FromS) == ext ->
			    change_disc_to_ram(
			      Tab, Cs, FromS, ToS, Logtmp, InitBy);
                        {disc_copies, disc_only_copies} ->
                            ok = ensure_rename(Dmp, Dat),
                            true = open_files(Tab, ToS, InPlace, InitBy),
                            mnesia_schema:ram_delete_table(Tab, FromS),
			    restore_indexes(Tab, ToS, Cs),
			    _ = file:delete(Dcl),
                            _ = file:delete(Dcd),
                            ok;
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
    Semantics = if Storage==unknown -> unknown;
		   true -> storage_semantics(Storage)
		end,
    %% Delete all possibly existing files and tables
    disc_delete_table(Tab, Storage),
    disc_delete_indecies(Tab, Cs, Storage),
    case InitBy of
	startup ->
	    ignore;
	_ ->
	    case ?catch_val({Tab, cstruct}) of
		{'EXIT', _} -> ignore;
		_ ->
		    mnesia_schema:ram_delete_table(Tab, Storage),
		    mnesia_checkpoint:tm_del_copy(Tab, node())
	    end
    end,
    StorageProps = Cs#cstruct.storage_properties,
    
    %% And create new ones..
    if
	(InitBy == startup) or (Semantics == unknown) ->
	    ignore;
	Semantics == ram_copies ->
	    EtsProps = proplists:get_value(ets, StorageProps, []),
	    Args = [{keypos, 2}, public, named_table, Type | EtsProps],
	    mnesia_monitor:mktab(Tab, Args);
	Semantics == disc_copies ->
	    EtsProps = proplists:get_value(ets, StorageProps, []),
	    Args = [{keypos, 2}, public, named_table, Type | EtsProps],
	    mnesia_monitor:mktab(Tab, Args),
	    File = mnesia_lib:tab2dcd(Tab),
	    FArg = [{file, File}, {name, {mnesia,create}},
		    {repair, false}, {mode, read_write}],
	    {ok, Log} = mnesia_monitor:open_log(FArg),
	    mnesia_monitor:unsafe_close_log(Log);
	Storage == disc_only_copies ->  % note: Storage, not Semantics
	    File = mnesia_lib:tab2dat(Tab),
	    file:delete(File),
	    DetsProps = proplists:get_value(dets, StorageProps, []),
	    Args = [{file, mnesia_lib:tab2dat(Tab)},
		    {type, mnesia_lib:disk_type(Tab, Type)},
		    {keypos, 2},
		    {repair, mnesia_monitor:get_env(auto_repair)} 
		    | DetsProps ],
	    mnesia_monitor:open_dets(Tab, Args);
	element(1,Storage) == ext ->
	    {ext, Alias, Mod} = Storage,
	    Mod:create_table(Alias, Tab, [])
    end,
    insert_op(Tid, ignore, {op, create_table, TabDef}, InPlace, InitBy);

insert_op(Tid, _, {op, create_table, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    insert_cstruct(Tid, Cs, false, InPlace, InitBy),
    Tab = Cs#cstruct.name,
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    StorageProps = Cs#cstruct.storage_properties,
    Semantics = storage_semantics(Storage),
    case InitBy of
	startup ->
	    case Semantics of
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
						mnesia_log:dcd_log_header(),
						Dcd,
						false,
						false,
						read_write),
			    mnesia_log:unsafe_close_log(temp)
		    end;
		disc_only_copies ->
		    DetsProps = proplists:get_value(dets, StorageProps, []),

		    try_create_disc_only_copy(Storage, Tab, Cs, DetsProps)
	    end;
	_ ->
	    Copies = mnesia_lib:copy_holders(Cs),
	    Active = mnesia_lib:intersect(Copies, val({current, db_nodes})),
	    [mnesia_controller:add_active_replica(Tab, N, Cs) || N <- Active],
	    
	    case Storage of
		unknown ->
		    mnesia_lib:unset({Tab, create_table}),
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
		    case Semantics of
			ram_copies ->
			    ignore;
			_ ->
			    %% Indecies are still created by loader
			    disc_delete_indecies(Tab, Cs, Storage)
			    %% disc_delete_table(Tab, Storage)
		    end,

		    %% Update whereabouts and create table
		    mnesia_controller:create_table(Tab),
		    mnesia_lib:unset({Tab, create_table})
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
	    %% Need to catch this, it crashes on ram_copies if
	    %% the op comes before table is loaded at startup.
	    ?CATCH(insert(Tid, Storage, Tab, '_', Oid, clear_table, InPlace, InitBy))
    end;

insert_op(Tid, _, {op, merge_schema, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    case Cs#cstruct.name of
	schema ->
	    %% If we bootstrap an empty (diskless) mnesia from another node
	    %% we might have changed the storage_type of schema.
	    %% I think this is a good place to do it.
	    Update = fun(NS = {Node,Storage}) ->
			     case mnesia_lib:cs_to_storage_type(Node, Cs) of
				 Storage -> NS;
				 disc_copies when Node == node() ->
				     Dir = mnesia_lib:dir(),
				     ok = mnesia_schema:opt_create_dir(true, Dir),
				     mnesia_schema:purge_dir(Dir, []),
				     mnesia_log:purge_all_logs(),

				     mnesia_lib:set(use_dir, true),
				     mnesia_log:init(),
				     Ns = val({current, db_nodes}),
				     F = fun(U) -> mnesia_recover:log_mnesia_up(U) end,
				     lists:foreach(F, Ns),
				     raw_named_dump_table(schema, dat),
				     temp_set_master_nodes(),
				     {Node,disc_copies};
				 CSstorage ->
				     {Node,CSstorage}
			     end
		     end,

	    W2C0 = val({schema, where_to_commit}),
	    W2C = case W2C0 of
		      {blocked, List} ->
			  {blocked,lists:map(Update,List)};
		      List ->
			  lists:map(Update,List)
		  end,
	    if W2C == W2C0 -> ignore;
	       true -> mnesia_lib:set({schema, where_to_commit}, W2C)
	    end;
	_ ->
	    ignore
    end,
    insert_cstruct(Tid, Cs, false, InPlace, InitBy);

insert_op(Tid, _, {op, del_table_copy, Storage, Node, TabDef}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    Tab = Cs#cstruct.name,
    if
	Tab == schema, Storage == ram_copies ->
	    insert_cstruct(Tid, Cs, true, InPlace, InitBy);
        Tab /= schema ->
	    mnesia_controller:del_active_replica(Tab, Node),
	    mnesia_lib:del({Tab, storage_alias(Storage)}, Node),
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
    Semantics = storage_semantics(Storage),
    case InitBy of
	startup when Semantics == disc_only_copies ->
	    true = open_files(Tab, Semantics, Storage, InPlace, InitBy),
	    mnesia_index:init_indecies(Tab, Storage, [Pos]);
	startup ->
	    ignore;
	_  ->
	    case val({Tab,where_to_read}) of
		nowhere -> ignore;
		_ ->
		    mnesia_index:init_indecies(Tab, Storage, [Pos])
	    end
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
        _ when element(1, Storage) == ext ->
	    mnesia_index:del_index_table(Tab, Storage, Pos);
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

insert_op(Tid, _, {op, change_table_majority,TabDef, _OldAccess, _Access}, InPlace, InitBy) ->
    Cs = mnesia_schema:list2cs(TabDef),
    case InitBy of
	startup -> ignore;
	_ -> mnesia_controller:change_table_majority(Cs)
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


storage_semantics({ext, Alias, Mod}) ->
    Mod:semantics(Alias, storage);
storage_semantics(Storage) when is_atom(Storage) ->
    Storage.

storage_alias({ext, Alias, _}) ->
    Alias;
storage_alias(Storage) when is_atom(Storage) ->
    Storage.

change_disc_to_ram(Tab, Cs, FromS, ToS, Logtmp, InitBy) ->
    disc_delete_indecies(Tab, Cs, FromS),
    case InitBy of
	startup ->
	    ignore;
	_ ->
	    %% ram table will already have been created
	    Tab = ets:info(Tab, name),  %% assertion
	    load_from_logfile(ToS, Tab, Logtmp),
	    PosList = Cs#cstruct.index,
	    mnesia_index:init_indecies(Tab, ToS, PosList)
    end,
    disc_delete_table(Tab, FromS).


try_create_disc_only_copy({ext,Alias,Mod}, Tab, Cs, _) ->
    Mod:create_table(Alias, Tab, mnesia_schema:cs2list(Cs));
try_create_disc_only_copy(disc_only_copies, Tab, Cs, DetsProps) ->
    Args = [{file, mnesia_lib:tab2dat(Tab)},
	    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)},
	    {keypos, 2},
	    {repair, mnesia_monitor:get_env(auto_repair)}
	    | DetsProps],
    case mnesia_monitor:open_dets(Tab, Args) of
	{ok, _} ->
	    mnesia_monitor:unsafe_close_dets(Tab);
	{error, Error} ->
	    exit({"Failed to create dets table", Error})
    end.

restore_indexes(Tab, ToS, Cs) ->
    PosList = Cs#cstruct.index,
    mnesia_index:init_indecies(Tab, ToS, PosList).


open_files(Tab, Storage, UpdateInPlace, InitBy) ->
    open_files(Tab, storage_semantics(Storage), Storage, UpdateInPlace, InitBy).

open_files(Tab, Semantics, Storage, UpdateInPlace, InitBy)
  when Storage /= unknown, Semantics /= ram_copies ->
    case get({?MODULE, Tab}) of
	undefined ->
	    case ?catch_val({Tab, setorbag}) of
		{'EXIT', _} ->
		    false;
		Type ->
		    Cs = val({Tab, cstruct}),
		    if Semantics  == disc_copies, Tab /= schema ->
			    Bool = open_disc_copies(Tab, InitBy),
			    Bool;
		       Storage == disc_only_copies; Tab == schema ->
			    Props = val({Tab, storage_properties}),
			    DetsProps = proplists:get_value(dets, Props, []),
			    Fname = prepare_open(Tab, UpdateInPlace),
			    Args = [{file, Fname},
				    {keypos, 2},
				    {repair, mnesia_monitor:get_env(auto_repair)},
				    {type, mnesia_lib:disk_type(Tab, Type)} 
				    | DetsProps],
			    {ok, _} = mnesia_monitor:open_dets(Tab, Args),
			    put({?MODULE, Tab}, {opened_dumper, dat}),
			    true;
		       element(1, Storage) == ext ->
			    {ext, Alias, Mod} = Storage,
			    Mod:load_table(Alias, Tab, InitBy,
					   mnesia_schema:cs2list(Cs)),
			    put({?MODULE, Tab}, {opened_dumper, ext}),
			    true
		    end
	    end;
	already_dumped ->
	    false;
	{opened_dumper, _} ->
	    true
    end;
open_files(_Tab, _Semantics, _Storage, _UpdateInPlace, _InitBy) ->
    false.

open_disc_copies(Tab, InitBy) ->
    DumpEts = needs_dump_ets(Tab),
    if
	DumpEts == false; InitBy == startup ->
            DclF = mnesia_lib:tab2dcl(Tab),
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

needs_dump_ets(Tab) ->
    DclF = mnesia_lib:tab2dcl(Tab),
    case file:read_file_info(DclF) of
        {error, enoent} ->
            false;
        {ok, DclInfo} ->
            DcdF =  mnesia_lib:tab2dcd(Tab),
            case file:read_file_info(DcdF) of
                {error, Reason} ->
                    mnesia_lib:dbg_out("File ~tp info_error ~tp ~n",
                                       [DcdF, Reason]),
                    true;
                {ok, DcdInfo} ->
                    Mul = case ?catch_val(dc_dump_limit) of
                              {'EXIT', _} -> ?DumpToEtsMultiplier;
                              Val -> Val
                          end,
                    DcdInfo#file_info.size =< (DclInfo#file_info.size * Mul)
            end
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
	    try ok = mnesia_lib:copy_file(Dat, Tmp)
	    catch error:Error ->
		    fatal("Cannot copy dets file ~tp to ~tp: ~tp~n",
			  [Dat, Tmp, Error])
	    end,
	    Tmp
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
    Storage = val({Tab, storage_type}),
    case storage_semantics(Storage) of
	disc_only_copies when InitBy /= startup ->
	    ignore;
	disc_copies when Storage /= unknown, Tab /= schema ->
	    mnesia_log:close_log({?MODULE,Tab});
	_ ->
	    do_close(InPlace, Outcome, Tab, Type, Storage)
    end,
    close_files(InPlace, Outcome, InitBy, Tail);

close_files(InPlace, Outcome, InitBy, [_ | Tail]) ->
    close_files(InPlace, Outcome, InitBy, Tail);
close_files(_, _, _InitBy, []) ->
    ok.

%% If storage is unknown during close clean up files, this can happen if timing
%% is right and dirty_write conflicts with schema operations.
do_close(_, _, Tab, ext, {ext,Alias,Mod}) ->
    Mod:close_table(Alias, Tab);
do_close(_, _, Tab, dcl, unknown) ->
    mnesia_log:close_log({?MODULE,Tab}),
    file:delete(mnesia_lib:tab2dcl(Tab));
do_close(_, _, Tab, dcl, _) ->  %% To be safe, can it happen?
    mnesia_log:close_log({?MODULE,Tab});
do_close(_, _, _Tab, ext, unknown) ->
    %% Not sure what to do here, but let's not crash
    ok;

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


temp_set_master_nodes() ->
    Tabs = val({schema, local_tables}),
    Masters = [{Tab, (val({Tab, disc_copies}) ++
		      val({Tab, ram_copies}) ++
		      val({Tab, disc_only_copies}) ++
		      external_copies(Tab)) -- [node()]}
	       || Tab <- Tabs],
    %% UseDir = false since we don't want to remember these
    %% masternodes and we are running (really soon anyway) since we want this
    %% to be known during table loading.
    mnesia_recover:log_master_nodes(Masters, false, yes),
    ok.

external_copies(Tab) ->
    case ?catch_val({Tab, external_copies}) of
	{'EXIT',_} -> [];
	Ext ->
	    lists:concat([Ns || {_, Ns} <- Ext])
    end.

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

		    try
			ok = raw_dump_table(TabRef, Tab),
			ok = file:rename(TmpFname, Fname)
		    catch _:Reason ->
			    ?SAFE(file:delete(TmpFname)),
			    exit({"Dump of table to disc failed", Reason})
		    after
			mnesia_lib:db_fixtable(Storage, Tab, false),
			mnesia_lib:dets_sync_close(Tab),
			mnesia_lib:unlock_table(Tab)
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

dump_to_logfile(Storage, Tab) ->
    case mnesia_monitor:use_dir() of
	true ->
	    Logtmp = mnesia_lib:tab2logtmp(Tab),
	    file:delete(Logtmp),
	    case disk_log:open([{name, make_ref()},
				{file, Logtmp},
				{repair, false},
				{linkto, self()}]) of
		{ok, Fd} ->
		    mnesia_lib:db_fixtable(Storage, Tab, true),
		    try do_dump_to_logfile(Storage, Tab, Fd)
		    after
			mnesia_lib:db_fixtable(Storage, Tab, false)
		    end;
		{error, _} = Error ->
		    Error
	    end;
	false ->
	    {error, {has_no_disc, node()}}
    end.

do_dump_to_logfile(Storage, Tab, Fd) ->
    Pat = [{'_',[],['$_']}],
    log_terms(mnesia_lib:db_select_init(Storage, Tab, Pat, 100), Storage, Tab, Pat, Fd).

log_terms({Objs, Cont}, Storage, Tab, Pat, Fd) ->
    ok = disk_log:alog_terms(Fd, Objs),
    log_terms(mnesia_lib:db_select_cont(Storage, Cont, '_'), Storage, Tab, Pat, Fd);
log_terms('$end_of_table', _, _, _, Fd) ->
    disk_log:close(Fd).

load_from_logfile(Storage, Tab, F) ->
    case disk_log:open([{name, make_ref()},
			{file, F},
			{repair, true},
			{linkto, self()}]) of
	{ok, Fd} ->
	    chunk_from_log(disk_log:chunk(Fd, start), Fd, Storage, Tab);
	{repaired, Fd, _, _} ->
	    chunk_from_log(disk_log:chunk(Fd, start), Fd, Storage, Tab);
	{error, _} = E ->
	    E
    end.

chunk_from_log({Cont, Terms}, Fd, Storage, Tab) ->
    _ = [mnesia_lib:db_put(Storage, Tab, T) || T <- Terms],
    chunk_from_log(disk_log:chunk(Fd, Cont), Fd, Storage, Tab);
chunk_from_log(eof, _, _, _) ->
    ok.



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
		    fatal("Failed to start ~n: ~tp~n", [N, Reason])
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

%% Local function in order to avoid external function call
val(Var) ->
    case ?catch_val_and_stack(Var) of
	{'EXIT', Stacktrace} -> mnesia_lib:other_val(Var, Stacktrace);
	Value -> Value
    end.
