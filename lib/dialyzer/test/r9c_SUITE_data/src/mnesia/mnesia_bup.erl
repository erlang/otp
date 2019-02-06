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
%%     $Id: mnesia_bup.erl,v 1.1 2008/12/17 09:53:37 mikpe Exp $
-module(mnesia_bup).
-export([
	 %% Public interface
	 iterate/4,
	 read_schema/2,
	 fallback_bup/0,
	 fallback_exists/0,
	 tm_fallback_start/1,
	 create_schema/1,
	 install_fallback/1,
	 install_fallback/2,
	 uninstall_fallback/0,
	 uninstall_fallback/1,
	 traverse_backup/4,
	 traverse_backup/6,
	 make_initial_backup/3,
	 fallback_to_schema/0,
	 lookup_schema/2,
	 schema2bup/1,
	 refresh_cookie/2,

	 %% Internal
	 fallback_receiver/2,
	 install_fallback_master/2,
	 uninstall_fallback_master/2,
	 local_uninstall_fallback/2,
	 do_traverse_backup/7,
	 trav_apply/4
	]).

-include("mnesia.hrl").
-import(mnesia_lib, [verbose/2, dbg_out/2]).

-record(restore, {mode, bup_module, bup_data}).

-record(fallback_args, {opaque,
			scope = global,
			module = mnesia_monitor:get_env(backup_module),
			use_default_dir = true,
			mnesia_dir,
			fallback_bup,
			fallback_tmp,
			skip_tables = [],
			keep_tables = [],
			default_op = keep_tables
		       }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Backup iterator

%% Reads schema section and iterates over all records in a backup.
%%
%% Fun(BunchOfRecords, Header, Schema, Acc) is applied when a suitable amount
%% of records has been collected.
%%
%% BunchOfRecords will be [] when the iteration is done.
iterate(Mod, Fun, Opaque, Acc) ->
    R = #restore{bup_module = Mod, bup_data = Opaque},
    case catch read_schema_section(R) of
	{error, Reason} ->
	    {error, Reason};
	{R2, {Header, Schema, Rest}} ->
	    case catch iter(R2, Header, Schema, Fun, Acc, Rest) of
		{ok, R3, Res} ->
		    catch safe_apply(R3, close_read, [R3#restore.bup_data]),
		    {ok, Res};
		{error, Reason} ->
		    catch safe_apply(R2, close_read, [R2#restore.bup_data]),
		    {error, Reason};
		{'EXIT', Pid, Reason} ->
		    catch safe_apply(R2, close_read, [R2#restore.bup_data]),
		    {error, {'EXIT', Pid, Reason}};
		{'EXIT', Reason} ->
		    catch safe_apply(R2, close_read, [R2#restore.bup_data]),
		    {error, {'EXIT', Reason}}
	    end
    end.

iter(R, Header, Schema, Fun, Acc, []) ->
    case safe_apply(R, read, [R#restore.bup_data]) of
	{R2, []} ->
	    Res = Fun([], Header, Schema, Acc),
	    {ok, R2, Res};
	{R2, BupItems} ->
	    iter(R2, Header, Schema, Fun, Acc, BupItems)
    end;
iter(R, Header, Schema, Fun, Acc, BupItems) ->
    Acc2 = Fun(BupItems, Header, Schema, Acc),
    iter(R, Header, Schema, Fun, Acc2, []).

safe_apply(R, write, [_, Items]) when Items == [] ->
    R;
safe_apply(R, What, Args) ->
    Abort = fun(Re) -> abort_restore(R, What, Args, Re) end,
    receive
	{'EXIT', Pid, Re} -> Abort({'EXIT', Pid, Re})
    after 0 ->
	    Mod = R#restore.bup_module,
	    case catch apply(Mod, What, Args) of
		{ok, Opaque, Items} when What == read ->
		    {R#restore{bup_data = Opaque}, Items};
		{ok, Opaque}  when What /= read->
		    R#restore{bup_data = Opaque};
		{error, Re} ->
		    Abort(Re);
		Re ->
		    Abort(Re)
	    end
    end.

abort_restore(R, What, Args, Reason) ->
    Mod = R#restore.bup_module,
    Opaque = R#restore.bup_data,
    dbg_out("Restore aborted. ~p:~p~p -> ~p~n",
	    [Mod, What, Args, Reason]),
    catch apply(Mod, close_read, [Opaque]),
    throw({error, Reason}).

fallback_to_schema() ->
    Fname = fallback_bup(),
    fallback_to_schema(Fname).

fallback_to_schema(Fname) ->
    Mod = mnesia_backup,
    case read_schema(Mod, Fname) of
	{error, Reason} ->
	    {error, Reason};
	Schema ->
	    case catch lookup_schema(schema, Schema) of
		{error, _} ->
		    {error, "No schema in fallback"};
		List ->
		    {ok, fallback, List}
	    end
    end.

%% Opens Opaque reads schema and then close
read_schema(Mod, Opaque) ->
    R = #restore{bup_module = Mod, bup_data = Opaque},
    case catch read_schema_section(R) of
	{error, Reason} ->
	    {error, Reason};
	{R2, {_Header, Schema, _}} ->
	    catch safe_apply(R2, close_read, [R2#restore.bup_data]),
	    Schema
    end.

%% Open backup media and extract schema
%% rewind backup media and leave it open
%% Returns {R, {Header, Schema}}
read_schema_section(R) ->
    case catch do_read_schema_section(R) of
	{'EXIT', Reason} ->
	    catch safe_apply(R, close_read, [R#restore.bup_data]),
	    {error, {'EXIT', Reason}};
	{error, Reason} ->
	    catch safe_apply(R, close_read, [R#restore.bup_data]),
	    {error, Reason};
	{R2, {H, Schema, Rest}} ->
	    Schema2 = convert_schema(H#log_header.log_version, Schema),
	    {R2, {H, Schema2, Rest}}
    end.

do_read_schema_section(R) ->
    R2 = safe_apply(R, open_read, [R#restore.bup_data]),
    {R3, RawSchema} = safe_apply(R2, read, [R2#restore.bup_data]),
    do_read_schema_section(R3, verify_header(RawSchema), []).

do_read_schema_section(R, {ok, B, C, []}, Acc) ->
    case safe_apply(R, read, [R#restore.bup_data]) of
	{R2, []} ->
	    {R2, {B, Acc, []}};
	{R2, RawSchema} ->
	    do_read_schema_section(R2, {ok, B, C, RawSchema}, Acc)
    end;

do_read_schema_section(R, {ok, B, C, [Head | Tail]}, Acc)
        when element(1, Head) == schema ->
    do_read_schema_section(R, {ok, B, C, Tail}, Acc ++ [Head]);

do_read_schema_section(R, {ok, B, _C, Rest}, Acc) ->
    {R, {B, Acc, Rest}};

do_read_schema_section(_R, {error, Reason}, _Acc) ->
    {error, Reason}.

verify_header([H | RawSchema])  when record(H, log_header) ->
    Current = mnesia_log:backup_log_header(),
    if
	H#log_header.log_kind == Current#log_header.log_kind ->
	    Versions = ["0.1", "1.1", Current#log_header.log_version],
	    case lists:member(H#log_header.log_version, Versions) of
		true ->
		    {ok, H, Current, RawSchema};
		false ->
		    {error, {"Bad header version. Cannot be used as backup.", H}}
	    end;
	true ->
	    {error, {"Bad kind of header. Cannot be used as backup.", H}}
    end;
verify_header(RawSchema) ->
    {error, {"Missing header. Cannot be used as backup.", catch hd(RawSchema)}}.

refresh_cookie(Schema, NewCookie) ->
    case lists:keysearch(schema, 2, Schema) of
	{value, {schema, schema, List}} ->
	    Cs = mnesia_schema:list2cs(List),
	    Cs2 = Cs#cstruct{cookie = NewCookie},
	    Item = {schema, schema, mnesia_schema:cs2list(Cs2)},
	    lists:keyreplace(schema, 2, Schema, Item);

	false ->
	    Reason = "No schema found. Cannot be used as backup.",
	    throw({error, {Reason, Schema}})
    end.

%% Convert schema items from an external backup
%% If backup format is the latest, no conversion is needed
%% All supported backup formats should have their converters
%% here as separate function clauses.
convert_schema("0.1", Schema) ->
    convert_0_1(Schema);
convert_schema("1.1", Schema) ->
    %% The new backup format is a pure extension of the old one
    Current = mnesia_log:backup_log_header(),
    convert_schema(Current#log_header.log_version, Schema);
convert_schema(Latest, Schema) ->
    H = mnesia_log:backup_log_header(),
    if
	H#log_header.log_version == Latest ->
	    Schema;
	true ->
	    Reason = "Bad backup header version. Cannot convert schema.",
	    throw({error, {Reason, H}})
    end.

%% Backward compatibility for 0.1
convert_0_1(Schema) ->
    case lists:keysearch(schema, 2, Schema) of
	{value, {schema, schema, List}} ->
	    Schema2 = lists:keydelete(schema, 2, Schema),
	    Cs = mnesia_schema:list2cs(List),
	    convert_0_1(Schema2, [], Cs);
	false ->
	    List = mnesia_schema:get_initial_schema(disc_copies, [node()]),
	    Cs = mnesia_schema:list2cs(List),
	    convert_0_1(Schema, [], Cs)
    end.

convert_0_1([{schema, cookie, Cookie} | Schema], Acc, Cs) ->
    convert_0_1(Schema, Acc, Cs#cstruct{cookie = Cookie});
convert_0_1([{schema, db_nodes, DbNodes} | Schema], Acc, Cs) ->
    convert_0_1(Schema, Acc, Cs#cstruct{disc_copies = DbNodes});
convert_0_1([{schema, version, Version} | Schema], Acc, Cs) ->
    convert_0_1(Schema, Acc, Cs#cstruct{version = Version});
convert_0_1([{schema, Tab, Def} | Schema], Acc, Cs) ->
    Head =
	case lists:keysearch(index, 1, Def) of
	    {value, {index, PosList}} ->
		%% Remove the snmp "index"
		P = PosList -- [snmp],
		Def2 = lists:keyreplace(index, 1, Def, {index, P}),
		{schema, Tab, Def2};
	    false ->
		{schema, Tab, Def}
	end,
    convert_0_1(Schema, [Head | Acc], Cs);
convert_0_1([Head | Schema], Acc, Cs) ->
    convert_0_1(Schema, [Head | Acc], Cs);
convert_0_1([], Acc, Cs) ->
    [schema2bup({schema, schema, Cs}) | Acc].

%% Returns Val or throw error
lookup_schema(Key, Schema) ->
    case lists:keysearch(Key, 2, Schema) of
	{value, {schema, Key, Val}} -> Val;
	false -> throw({error, {"Cannot lookup", Key}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Backup compatibility

%% Convert internal schema items to backup dito
schema2bup({schema, Tab}) ->
    {schema, Tab};
schema2bup({schema, Tab, TableDef}) ->
    {schema, Tab, mnesia_schema:cs2list(TableDef)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create schema on the given nodes
%% Requires that old schemas has been deleted
%% Returns ok | {error, Reason}
create_schema([]) ->
    create_schema([node()]);
create_schema(Ns) when list(Ns) ->
    case is_set(Ns) of
	true ->
	    create_schema(Ns, mnesia_schema:ensure_no_schema(Ns));
	false ->
	    {error, {combine_error, Ns}}
    end;
create_schema(Ns) ->
    {error, {badarg, Ns}}.

is_set(List) when list(List) ->
    ordsets:is_set(lists:sort(List));
is_set(_) ->
    false.

create_schema(Ns, ok) ->
    %% Ensure that we access the intended Mnesia
    %% directory. This function may not be called
    %% during startup since it will cause the
    %% application_controller to get into deadlock
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    case mnesia_monitor:get_env(schema_location) of
		ram ->
		    {error, {has_no_disc, node()}};
		_ ->
		    case mnesia_schema:opt_create_dir(true, mnesia_lib:dir()) of
			{error, What} ->
			    {error, What};
			ok ->
			    Mod = mnesia_backup,
			    Str = mk_str(),
			    File = mnesia_lib:dir(Str),
			    file:delete(File),
			    case catch make_initial_backup(Ns, File, Mod) of
				{ok, _Res} ->
				    case do_install_fallback(File, Mod) of
					ok ->
					    file:delete(File),
					    ok;
					{error, Reason} ->
					    {error, Reason}
				    end;
				{error, Reason} ->
				    {error, Reason}
			    end
		    end
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
create_schema(_Ns, {error, Reason}) ->
    {error, Reason};
create_schema(_Ns, Reason) ->
    {error, Reason}.

mk_str() ->
    Now = [integer_to_list(I) || I <- tuple_to_list(now())],
    lists:concat([node()] ++ Now ++ ".TMP").

make_initial_backup(Ns, Opaque, Mod) ->
    Schema = [{schema, schema, mnesia_schema:get_initial_schema(disc_copies, Ns)}],
    O2 = do_apply(Mod, open_write, [Opaque], Opaque),
    O3 = do_apply(Mod, write, [O2, [mnesia_log:backup_log_header()]], O2),
    O4 = do_apply(Mod, write, [O3, Schema], O3),
    O5 = do_apply(Mod, commit_write, [O4], O4),
    {ok, O5}.

do_apply(_, write, [_, Items], Opaque) when Items == [] ->
    Opaque;
do_apply(Mod, What, Args, _Opaque) ->
    case catch apply(Mod, What, Args) of
	{ok, Opaque2} ->  Opaque2;
	{error, Reason} -> throw({error, Reason});
	{'EXIT', Reason} -> throw({error, {'EXIT', Reason}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Restore

%% Restore schema and possibly other tables from a backup
%% and replicate them to the necessary nodes
%% Requires that old schemas has been deleted
%% Returns ok | {error, Reason}
install_fallback(Opaque) ->
    install_fallback(Opaque, []).

install_fallback(Opaque, Args) ->
    %% Ensure that we access the intended Mnesia
    %% directory. This function may not be called
    %% during startup since it will cause the
    %% application_controller to get into deadlock
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    do_install_fallback(Opaque, Args);
	{error, Reason} ->
	    {error, Reason}
    end.

do_install_fallback(Opaque,  Mod) when atom(Mod) ->
    do_install_fallback(Opaque, [{module, Mod}]);
do_install_fallback(Opaque, Args) when list(Args) ->
    case check_fallback_args(Args, #fallback_args{opaque = Opaque}) of
	{ok, FA} ->
	    do_install_fallback(FA);
	{error, Reason} ->
	    {error, Reason}
    end;
do_install_fallback(_Opaque, Args) ->
    {error, {badarg, Args}}.

check_fallback_args([Arg | Tail], FA) ->
    case catch check_fallback_arg_type(Arg, FA) of
	{'EXIT', _Reason} ->
	    {error, {badarg, Arg}};
	FA2 ->
	    check_fallback_args(Tail, FA2)
    end;
check_fallback_args([], FA) ->
    {ok, FA}.

check_fallback_arg_type(Arg, FA) ->
    case Arg of
	{scope, global} ->
	    FA#fallback_args{scope = global};
	{scope, local} ->
	    FA#fallback_args{scope = local};
	{module, Mod} ->
	    Mod2 = mnesia_monitor:do_check_type(backup_module, Mod),
	    FA#fallback_args{module = Mod2};
	{mnesia_dir, Dir} ->
	    FA#fallback_args{mnesia_dir = Dir,
			     use_default_dir = false};
	{keep_tables, Tabs} ->
	    atom_list(Tabs),
	    FA#fallback_args{keep_tables = Tabs};
	{skip_tables, Tabs} ->
	    atom_list(Tabs),
	    FA#fallback_args{skip_tables = Tabs};
	{default_op, keep_tables} ->
	    FA#fallback_args{default_op = keep_tables};
	{default_op, skip_tables} ->
	    FA#fallback_args{default_op = skip_tables}
    end.

atom_list([H | T]) when atom(H) ->
    atom_list(T);
atom_list([]) ->
    ok.

do_install_fallback(FA) ->
    Pid = spawn_link(?MODULE, install_fallback_master, [self(), FA]),
    Res =
	receive
	    {'EXIT', Pid, Reason} -> % if appl has trapped exit
		{error, {'EXIT', Reason}};
	    {Pid, Res2} ->
		case Res2 of
		    {ok, _} ->
			ok;
		    {error, Reason} ->
			{error, {"Cannot install fallback", Reason}}
		end
	end,
    Res.

install_fallback_master(ClientPid, FA) ->
    process_flag(trap_exit, true),
    State = {start, FA},
    Opaque = FA#fallback_args.opaque,
    Mod = FA#fallback_args.module,
    Res = (catch iterate(Mod, fun restore_recs/4, Opaque, State)),
    unlink(ClientPid),
    ClientPid ! {self(), Res},
    exit(shutdown).

restore_recs(_, _, _, stop) ->
    throw({error, "restore_recs already stopped"});

restore_recs(Recs, Header, Schema, {start, FA}) ->
    %% No records in backup
    Schema2 = convert_schema(Header#log_header.log_version, Schema),
    CreateList = lookup_schema(schema, Schema2),
    case catch mnesia_schema:list2cs(CreateList) of
	{'EXIT', Reason} ->
	    throw({error, {"Bad schema in restore_recs", Reason}});
	Cs ->
	    Ns = get_fallback_nodes(FA, Cs#cstruct.disc_copies),
	    global:set_lock({{mnesia_table_lock, schema}, self()}, Ns, infinity),
	    Args = [self(), FA],
	    Pids = [spawn_link(N, ?MODULE, fallback_receiver, Args) || N <- Ns],
	    send_fallback(Pids, {start, Header, Schema2}),
	    Res = restore_recs(Recs, Header, Schema2, Pids),
	    global:del_lock({{mnesia_table_lock, schema}, self()}, Ns),
	    Res
    end;

restore_recs([], _Header, _Schema, Pids) ->
    send_fallback(Pids, swap),
    send_fallback(Pids, stop),
    stop;

restore_recs(Recs, _, _, Pids) ->
    send_fallback(Pids, {records, Recs}),
    Pids.

get_fallback_nodes(FA, Ns) ->
    This = node(),
    case lists:member(This, Ns) of
	true ->
	    case FA#fallback_args.scope of
		global ->  Ns;
		local -> [This]
	    end;
	false ->
	    throw({error, {"No disc resident schema on local node", Ns}})
    end.

send_fallback(Pids, Msg) when list(Pids), Pids /= [] ->
    lists:foreach(fun(Pid) -> Pid ! {self(), Msg} end, Pids),
    rec_answers(Pids, []).

rec_answers([], Acc) ->
    case {lists:keysearch(error, 1, Acc), mnesia_lib:uniq(Acc)} of
	{{value, {error, Val}}, _} -> throw({error, Val});
	{_, [SameAnswer]} -> SameAnswer;
	{_, Other} -> throw({error, {"Different answers", Other}})
    end;
rec_answers(Pids, Acc) ->
    receive
	{'EXIT', Pid, stopped} ->
	    Pids2 = lists:delete(Pid, Pids),
	    rec_answers(Pids2, [stopped|Acc]);
	{'EXIT', Pid, Reason} ->
	    Pids2 = lists:delete(Pid, Pids),
	    rec_answers(Pids2, [{error, {'EXIT', Pid, Reason}}|Acc]);
	{Pid, Reply} ->
	    Pids2 = lists:delete(Pid, Pids),
	    rec_answers(Pids2, [Reply|Acc])
    end.

fallback_exists() ->
    Fname = fallback_bup(),
    fallback_exists(Fname).

fallback_exists(Fname) ->
    case mnesia_monitor:use_dir() of
	true ->
	    mnesia_lib:exists(Fname);
	false ->
	    case ?catch_val(active_fallback) of
		{'EXIT', _} -> false;
		Bool -> Bool
	    end
    end.

fallback_name() -> "FALLBACK.BUP".
fallback_bup() -> mnesia_lib:dir(fallback_name()).

fallback_tmp_name() -> "FALLBACK.TMP".
%% fallback_full_tmp_name() -> mnesia_lib:dir(fallback_tmp_name()).

fallback_receiver(Master, FA) ->
    process_flag(trap_exit, true),

    case catch register(mnesia_fallback, self()) of
	{'EXIT', _} ->
	    Reason = {already_exists, node()},
	    local_fallback_error(Master, Reason);
	true ->
	    FA2 = check_fallback_dir(Master, FA),
	    Bup = FA2#fallback_args.fallback_bup,
	    case mnesia_lib:exists(Bup) of
		true ->
		    Reason2 = {already_exists, node()},
		    local_fallback_error(Master, Reason2);
		false ->
		    Mod = mnesia_backup,
		    Tmp = FA2#fallback_args.fallback_tmp,
		    R = #restore{mode = replace,
				 bup_module = Mod,
				 bup_data = Tmp},
		    file:delete(Tmp),
		    case catch fallback_receiver_loop(Master, R, FA2, schema) of
			{error, Reason} ->
			    local_fallback_error(Master, Reason);
			Other ->
			    exit(Other)
		    end
	    end
    end.

local_fallback_error(Master, Reason) ->
    Master ! {self(), {error, Reason}},
    unlink(Master),
    exit(Reason).

check_fallback_dir(Master, FA) ->
    case mnesia:system_info(schema_location) of
	ram ->
	    Reason = {has_no_disc, node()},
	    local_fallback_error(Master, Reason);
	_ ->
	    Dir = check_fallback_dir_arg(Master, FA),
	    Bup = filename:join([Dir, fallback_name()]),
	    Tmp = filename:join([Dir, fallback_tmp_name()]),
	    FA#fallback_args{fallback_bup = Bup,
			     fallback_tmp = Tmp,
			     mnesia_dir = Dir}
    end.

check_fallback_dir_arg(Master, FA) ->
    case FA#fallback_args.use_default_dir of
	true ->
	    mnesia_lib:dir();
	false when FA#fallback_args.scope == local ->
	    Dir = FA#fallback_args.mnesia_dir,
	    case catch mnesia_monitor:do_check_type(dir, Dir) of
		{'EXIT', _R} ->
		    Reason = {badarg, {dir, Dir}, node()},
		    local_fallback_error(Master, Reason);
		AbsDir->
		    AbsDir
	    end;
	false when FA#fallback_args.scope == global ->
	    Reason = {combine_error, global, dir, node()},
	    local_fallback_error(Master, Reason)
    end.

fallback_receiver_loop(Master, R, FA, State) ->
    receive
	{Master, {start, Header, Schema}} when State == schema ->
	    Dir = FA#fallback_args.mnesia_dir,
	    throw_bad_res(ok, mnesia_schema:opt_create_dir(true, Dir)),
	    R2 = safe_apply(R, open_write, [R#restore.bup_data]),
	    R3 = safe_apply(R2, write, [R2#restore.bup_data, [Header]]),
	    BupSchema = [schema2bup(S) || S <- Schema],
	    R4 = safe_apply(R3, write, [R3#restore.bup_data, BupSchema]),
	    Master ! {self(), ok},
	    fallback_receiver_loop(Master, R4, FA, records);

	{Master, {records, Recs}} when State == records ->
	    R2 = safe_apply(R, write, [R#restore.bup_data, Recs]),
	    Master ! {self(), ok},
	    fallback_receiver_loop(Master, R2, FA, records);

	{Master, swap} when State /= schema ->
	    ?eval_debug_fun({?MODULE, fallback_receiver_loop, pre_swap}, []),
	    safe_apply(R, commit_write, [R#restore.bup_data]),
	    Bup = FA#fallback_args.fallback_bup,
	    Tmp = FA#fallback_args.fallback_tmp,
	    throw_bad_res(ok, file:rename(Tmp, Bup)),
	    catch mnesia_lib:set(active_fallback, true),
	    ?eval_debug_fun({?MODULE, fallback_receiver_loop, post_swap}, []),
	    Master ! {self(), ok},
	    fallback_receiver_loop(Master, R, FA, stop);

	{Master, stop} when State == stop ->
	    stopped;

	Msg ->
	    safe_apply(R, abort_write, [R#restore.bup_data]),
	    Tmp = FA#fallback_args.fallback_tmp,
	    file:delete(Tmp),
	    throw({error, "Unexpected msg fallback_receiver_loop", Msg})
    end.

throw_bad_res(Expected, Expected) -> Expected;
throw_bad_res(_Expected, {error, Actual}) -> throw({error, Actual});
throw_bad_res(_Expected, Actual) -> throw({error, Actual}).

-record(local_tab, {name, storage_type, dets_args, open, close, add, record_name}).

tm_fallback_start(IgnoreFallback) ->
    mnesia_schema:lock_schema(),
    Res = do_fallback_start(fallback_exists(), IgnoreFallback),
    mnesia_schema: unlock_schema(),
    case Res of
	ok -> ok;
	{error, Reason} -> exit(Reason)
    end.

do_fallback_start(false, _IgnoreFallback) ->
    ok;
do_fallback_start(true, true) ->
    verbose("Ignoring fallback at startup, but leaving it active...~n", []),
    mnesia_lib:set(active_fallback, true),
    ok;
do_fallback_start(true, false) ->
    verbose("Starting from fallback...~n", []),

    Fname = fallback_bup(),
    Mod = mnesia_backup,
    Ets = ?ets_new_table(mnesia_local_tables, [set, public, {keypos, 2}]),
    case catch iterate(Mod, fun restore_tables/4, Fname, {start, Ets}) of
	{ok, Res} ->
	    case Res of
		{local, _, LT} ->  %% Close the last file
		    (LT#local_tab.close)(LT);
		_ ->
		    ignore
	    end,
	    List = ?ets_match_object(Ets, '_'),
	    Tabs = [L#local_tab.name || L <- List, L#local_tab.name /= schema],
	    ?ets_delete_table(Ets),
	    mnesia_lib:swap_tmp_files(Tabs),
	    catch dets:close(schema),
	    Tmp = mnesia_lib:tab2tmp(schema),
	    Dat = mnesia_lib:tab2dat(schema),
	    case file:rename(Tmp, Dat) of
		ok ->
		    file:delete(Fname),
		    ok;
		{error, Reason} ->
		    file:delete(Tmp),
		    {error, {"Cannot start from fallback. Rename error.", Reason}}
	    end;
	{error, Reason} ->
	    {error, {"Cannot start from fallback", Reason}};
	{'EXIT', Reason} ->
	    {error, {"Cannot start from fallback", Reason}}
    end.

restore_tables(Recs, Header, Schema, {start, LocalTabs}) ->
    Dir = mnesia_lib:dir(),
    OldDir = filename:join([Dir, "OLD_DIR"]),
    mnesia_schema:purge_dir(OldDir, []),
    mnesia_schema:purge_dir(Dir, [fallback_name()]),
    init_dat_files(Schema, LocalTabs),
    State = {new, LocalTabs},
    restore_tables(Recs, Header, Schema, State);
restore_tables([Rec | Recs], Header, Schema, {new, LocalTabs}) ->
    Tab = element(1, Rec),
    case ?ets_lookup(LocalTabs, Tab) of
	[] ->
	    State = {not_local, LocalTabs, Tab},
	    restore_tables(Recs, Header, Schema, State);
	[L] when record(L, local_tab) ->
	    (L#local_tab.open)(Tab, L),
	    State = {local, LocalTabs, L},
	    restore_tables([Rec | Recs], Header, Schema, State)
    end;
restore_tables([Rec | Recs], Header, Schema, S = {not_local, LocalTabs, PrevTab}) ->
    Tab = element(1, Rec),
    if
	Tab == PrevTab ->
	    restore_tables(Recs, Header, Schema, S);
	true ->
	    State = {new, LocalTabs},
	    restore_tables([Rec | Recs], Header, Schema, State)
    end;
restore_tables([Rec | Recs], Header, Schema, State = {local, LocalTabs, L}) ->
    Tab = element(1, Rec),
    if
	Tab == L#local_tab.name ->
	    Key = element(2, Rec),
	    (L#local_tab.add)(Tab, Key, Rec, L),
	    restore_tables(Recs, Header, Schema, State);
	true ->
	    (L#local_tab.close)(L),
	    NState = {new, LocalTabs},
	    restore_tables([Rec | Recs], Header, Schema, NState)
    end;
restore_tables([], _Header, _Schema, State) ->
    State.

%% Creates all necessary dat files and inserts
%% the table definitions in the schema table
%%
%% Returns a list of local_tab tuples for all local tables
init_dat_files(Schema, LocalTabs) ->
    Fname = mnesia_lib:tab2tmp(schema),
    Args = [{file, Fname}, {keypos, 2}, {type, set}],
    case dets:open_file(schema, Args) of % Assume schema lock
	{ok, _} ->
	    create_dat_files(Schema, LocalTabs),
	    dets:close(schema),
	    LocalTab = #local_tab{name = schema,
				  storage_type = disc_copies,
				  dets_args = Args,
				  open   = fun open_media/2,
				  close  = fun close_media/1,
				  add    = fun add_to_media/4,
				  record_name = schema},
	    ?ets_insert(LocalTabs, LocalTab);
	{error, Reason} ->
	    throw({error, {"Cannot open file", schema, Args, Reason}})
    end.

create_dat_files([{schema, schema, TabDef} | Tail], LocalTabs) ->
    ok = dets:insert(schema, {schema, schema, TabDef}),
    create_dat_files(Tail, LocalTabs);
create_dat_files([{schema, Tab, TabDef} | Tail], LocalTabs) ->
    Cs =  mnesia_schema:list2cs(TabDef),
    ok = dets:insert(schema, {schema, Tab, TabDef}),
    RecName = Cs#cstruct.record_name,
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
	unknown ->
	    cleanup_dat_file(Tab),
	    create_dat_files(Tail, LocalTabs);
	disc_only_copies ->
	    Fname = mnesia_lib:tab2tmp(Tab),
	    Args = [{file, Fname}, {keypos, 2},
		    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)}],
	    case mnesia_lib:dets_sync_open(Tab, Args) of
		{ok, _} ->
		    mnesia_lib:dets_sync_close(Tab),
		    LocalTab = #local_tab{name = Tab,
					  storage_type = disc_only_copies,
					  dets_args = Args,
					  open   = fun open_media/2,
					  close  = fun close_media/1,
					  add    = fun add_to_media/4,
					  record_name = RecName},
		    ?ets_insert(LocalTabs, LocalTab),
		    create_dat_files(Tail, LocalTabs);
		{error, Reason} ->
		    throw({error, {"Cannot open file", Tab, Args, Reason}})
	    end;
	ram_copies ->
	    %% Create .DCD if needed in open_media in case any ram_copies
	    %% are backed up.
	    LocalTab = #local_tab{name = Tab,
				  storage_type = ram_copies,
				  dets_args = ignore,
				  open   = fun open_media/2,
				  close  = fun close_media/1,
				  add    = fun add_to_media/4,
				  record_name = RecName},
	    ?ets_insert(LocalTabs, LocalTab),
	    create_dat_files(Tail, LocalTabs);
	Storage ->
	    %% Create DCD
	    Fname = mnesia_lib:tab2dcd(Tab),
	    file:delete(Fname),
	    Log = mnesia_log:open_log(fallback_tab, mnesia_log:dcd_log_header(),
				      Fname, false),
	    LocalTab = #local_tab{name = Tab,
				  storage_type = Storage,
				  dets_args = ignore,
				  open   = fun open_media/2,
				  close  = fun close_media/1,
				  add    = fun add_to_media/4,
				  record_name = RecName},
	    mnesia_log:close_log(Log),
	    ?ets_insert(LocalTabs, LocalTab),
	    create_dat_files(Tail, LocalTabs)
    end;
create_dat_files([{schema, Tab} | Tail], LocalTabs) ->
    cleanup_dat_file(Tab),
    create_dat_files(Tail, LocalTabs);
create_dat_files([], _LocalTabs) ->
    ok.

cleanup_dat_file(Tab) ->
    ok = dets:delete(schema, {schema, Tab}),
    mnesia_lib:cleanup_tmp_files([Tab]).

open_media(Tab, LT) ->
    case LT#local_tab.storage_type of
	disc_only_copies ->
	    Args = LT#local_tab.dets_args,
	    case mnesia_lib:dets_sync_open(Tab, Args) of
		{ok, _} -> ok;
		{error, Reason} ->
		    throw({error, {"Cannot open file", Tab, Args, Reason}})
	    end;
	ram_copies ->
	    %% Create .DCD as ram_copies backed up.
	    FnameDCD = mnesia_lib:tab2dcd(Tab),
	    file:delete(FnameDCD),
	    Log = mnesia_log:open_log(fallback_tab,
				      mnesia_log:dcd_log_header(),
				      FnameDCD, false),
	    mnesia_log:close_log(Log),

	    %% Create .DCL
	    Fname = mnesia_lib:tab2dcl(Tab),
	    file:delete(Fname),
	    mnesia_log:open_log({?MODULE,Tab},
				mnesia_log:dcl_log_header(),
				Fname, false, false,
				read_write);
	_ ->
	    Fname = mnesia_lib:tab2dcl(Tab),
	    file:delete(Fname),
	    mnesia_log:open_log({?MODULE,Tab},
				mnesia_log:dcl_log_header(),
				Fname, false, false,
				read_write)
    end.
close_media(L) ->
    Tab = L#local_tab.name,
    case L#local_tab.storage_type of
	disc_only_copies ->
	    mnesia_lib:dets_sync_close(Tab);
	_ ->
	    mnesia_log:close_log({?MODULE,Tab})
    end.

add_to_media(Tab, Key, Rec, L) ->
    RecName = L#local_tab.record_name,
    case L#local_tab.storage_type of
	disc_only_copies ->
	    case Rec of
		{Tab, Key} ->
		    ok = dets:delete(Tab, Key);
		(Rec) when Tab == RecName ->
		    ok = dets:insert(Tab, Rec);
		(Rec) ->
		    Rec2 = setelement(1, Rec, RecName),
		    ok = dets:insert(Tab, Rec2)
	    end;
	_ ->
	    Log = {?MODULE, Tab},
	    case Rec of
		{Tab, Key} ->
		    mnesia_log:append(Log, {{Tab, Key}, {Tab, Key}, delete});
		(Rec) when Tab == RecName ->
		    mnesia_log:append(Log, {{Tab, Key}, Rec, write});
		(Rec) ->
		    Rec2 = setelement(1, Rec, RecName),
		    mnesia_log:append(Log, {{Tab, Key}, Rec2, write})
	    end
    end.

uninstall_fallback() ->
    uninstall_fallback([{scope, global}]).

uninstall_fallback(Args) ->
    case check_fallback_args(Args, #fallback_args{}) of
	{ok, FA} ->
	    do_uninstall_fallback(FA);
	{error, Reason} ->
	    {error, Reason}
    end.

do_uninstall_fallback(FA) ->
    %% Ensure that we access the intended Mnesia
    %% directory. This function may not be called
    %% during startup since it will cause the
    %% application_controller to get into deadlock
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    Pid = spawn_link(?MODULE, uninstall_fallback_master, [self(), FA]),
	    receive
		{'EXIT', Pid, Reason} -> % if appl has trapped exit
		    {error, {'EXIT', Reason}};
		{Pid, Res} ->
		    Res
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

uninstall_fallback_master(ClientPid, FA) ->
    process_flag(trap_exit, true),

    FA2 = check_fallback_dir(ClientPid, FA), % May exit
    Bup = FA2#fallback_args.fallback_bup,
    case fallback_to_schema(Bup) of
	{ok, fallback, List} ->
	    Cs = mnesia_schema:list2cs(List),
	    case catch get_fallback_nodes(FA, Cs#cstruct.disc_copies) of
		Ns when list(Ns) ->
		    do_uninstall(ClientPid, Ns, FA);
		{error, Reason} ->
		    local_fallback_error(ClientPid, Reason)
	    end;
	{error, Reason} ->
	    local_fallback_error(ClientPid, Reason)
    end.

do_uninstall(ClientPid, Ns, FA) ->
    Args = [self(), FA],
    global:set_lock({{mnesia_table_lock, schema}, self()}, Ns, infinity),
    Pids = [spawn_link(N, ?MODULE, local_uninstall_fallback, Args) || N <- Ns],
    Res = do_uninstall(ClientPid, Pids, [], [], ok),
    global:del_lock({{mnesia_table_lock, schema}, self()}, Ns),
    ClientPid ! {self(), Res},
    unlink(ClientPid),
    exit(shutdown).

do_uninstall(ClientPid, [Pid | Pids], GoodPids, BadNodes, Res) ->
    receive
	%% {'EXIT', ClientPid, _} ->
	%% client_exit;
	{'EXIT', Pid, Reason} ->
	    BadNode = node(Pid),
	    BadRes = {error, {"Uninstall fallback", BadNode, Reason}},
	    do_uninstall(ClientPid, Pids, GoodPids, [BadNode | BadNodes], BadRes);
	{Pid, {error, Reason}} ->
	    BadNode = node(Pid),
	    BadRes = {error, {"Uninstall fallback", BadNode, Reason}},
	    do_uninstall(ClientPid, Pids, GoodPids, [BadNode | BadNodes], BadRes);
	{Pid, started} ->
	    do_uninstall(ClientPid, Pids, [Pid | GoodPids], BadNodes, Res)
    end;
do_uninstall(ClientPid, [], GoodPids, [], ok) ->
    lists:foreach(fun(Pid) -> Pid ! {self(), do_uninstall} end, GoodPids),
    rec_uninstall(ClientPid, GoodPids, ok);
do_uninstall(_ClientPid, [], GoodPids, BadNodes, BadRes) ->
    lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, GoodPids),
    {error, {node_not_running, BadNodes, BadRes}}.

local_uninstall_fallback(Master, FA) ->
    %% Don't trap exit

    register(mnesia_fallback, self()),        % May exit
    FA2 = check_fallback_dir(Master, FA), % May exit
    Master ! {self(), started},

    receive
	{Master, do_uninstall} ->
	    ?eval_debug_fun({?MODULE, uninstall_fallback2, pre_delete}, []),
	    catch mnesia_lib:set(active_fallback, false),
	    Tmp = FA2#fallback_args.fallback_tmp,
	    Bup = FA2#fallback_args.fallback_bup,
	    file:delete(Tmp),
	    Res =
		case fallback_exists(Bup) of
		    true -> file:delete(Bup);
		    false -> ok
		end,
	    ?eval_debug_fun({?MODULE, uninstall_fallback2, post_delete}, []),
	    Master ! {self(), Res},
	    unlink(Master),
	    exit(normal)
    end.

rec_uninstall(ClientPid, [Pid | Pids], AccRes) ->
    receive
	%% {'EXIT', ClientPid, _} ->
	%% exit(shutdown);
	{'EXIT', Pid, R} ->
	    Reason = {node_not_running, {node(Pid), R}},
	    rec_uninstall(ClientPid, Pids, {error, Reason});
	{Pid, ok} ->
	    rec_uninstall(ClientPid, Pids, AccRes);
	{Pid, BadRes} ->
	    rec_uninstall(ClientPid, Pids, BadRes)
    end;
rec_uninstall(ClientPid, [], Res) ->
    ClientPid ! {self(), Res},
    unlink(ClientPid),
    exit(normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Backup traversal

%% Iterate over a backup and produce a new backup.
%% Fun(BackupItem, Acc) is applied for each BackupItem.
%%
%% Valid BackupItems are:
%%
%%   {schema, Tab}		Table to be deleted
%%   {schema, Tab, CreateList}	Table to be created, CreateList may be empty
%%   {schema, db_nodes, DbNodes}List of nodes, defaults to [node()] OLD
%%   {schema, version, Version} Schema version                      OLD
%%   {schema, cookie, Cookie}	Unique schema cookie                OLD
%%   {Tab, Key}			Oid for record to be deleted
%%   Record			Record to be inserted.
%%
%% The Fun must return a tuple {BackupItems, NewAcc}
%% where BackupItems is a list of valid BackupItems and
%% NewAcc is a new accumulator value. Once BackupItems
%% that not are schema  related has been returned, no more schema
%% items may be  returned. The schema related items must always be
%% first in the backup.
%%
%% If TargetMod == read_only, no new backup will be created.
%%
%% Opening of the source media will be performed by
%% to SourceMod:open_read(Source)
%%
%% Opening of the target media will be performed by
%% to TargetMod:open_write(Target)
traverse_backup(Source, Target, Fun, Acc) ->
    Mod = mnesia_monitor:get_env(backup_module),
    traverse_backup(Source, Mod, Target, Mod, Fun, Acc).

traverse_backup(Source, SourceMod, Target, TargetMod, Fun, Acc) ->
    Args = [self(), Source, SourceMod, Target, TargetMod, Fun, Acc],
    Pid = spawn_link(?MODULE, do_traverse_backup, Args),
    receive
	{'EXIT', Pid, Reason} ->
	    {error, {"Backup traversal crashed", Reason}};
	{iter_done, Pid, Res} ->
	    Res
    end.

do_traverse_backup(ClientPid, Source, SourceMod, Target, TargetMod, Fun, Acc) ->
    process_flag(trap_exit, true),
    Iter =
	if
	    TargetMod /= read_only ->
		case catch do_apply(TargetMod, open_write, [Target], Target) of
		    {error, Error} ->
			unlink(ClientPid),
			ClientPid ! {iter_done, self(), {error, Error}},
			exit(Error);
		    Else -> Else
		end;
	    true ->
		ignore
	end,
    A = {start, Fun, Acc, TargetMod, Iter},
    Res =
	case iterate(SourceMod, fun trav_apply/4, Source, A) of
	    {ok, {iter, _, Acc2, _, Iter2}} when TargetMod /= read_only ->
		case catch do_apply(TargetMod, commit_write, [Iter2], Iter2) of
		    {error, Reason} ->
			{error, Reason};
		    _ ->
			{ok, Acc2}
		end;
	    {ok, {iter, _, Acc2, _, _}} ->
		{ok, Acc2};
	    {error, Reason} when TargetMod /= read_only->
		catch do_apply(TargetMod, abort_write, [Iter], Iter),
		{error, {"Backup traversal failed", Reason}};
	    {error, Reason} ->
		{error, {"Backup traversal failed", Reason}}
	end,
    unlink(ClientPid),
    ClientPid ! {iter_done, self(), Res}.

trav_apply(Recs, _Header, _Schema, {iter, Fun, Acc, Mod, Iter}) ->
    {NewRecs, Acc2} = filter_foldl(Fun, Acc, Recs),
    if
	Mod /= read_only, NewRecs /= [] ->
	    Iter2 = do_apply(Mod, write, [Iter, NewRecs], Iter),
	    {iter, Fun, Acc2, Mod, Iter2};
	true ->
	    {iter, Fun, Acc2, Mod, Iter}
    end;
trav_apply(Recs, Header, Schema, {start, Fun, Acc, Mod, Iter}) ->
    Iter2 =
	if
	    Mod /= read_only ->
		do_apply(Mod, write, [Iter, [Header]], Iter);
	    true ->
		Iter
	end,
    TravAcc = trav_apply(Schema, Header, Schema, {iter, Fun, Acc, Mod, Iter2}),
    trav_apply(Recs, Header, Schema, TravAcc).

filter_foldl(Fun, Acc, [Head|Tail]) ->
    case Fun(Head, Acc) of
	{HeadItems, HeadAcc} when list(HeadItems) ->
	    {TailItems, TailAcc} = filter_foldl(Fun, HeadAcc, Tail),
	    {HeadItems ++ TailItems, TailAcc};
	Other ->
	    throw({error, {"Fun must return a list", Other}})
    end;
filter_foldl(_Fun, Acc, []) ->
    {[], Acc}.
