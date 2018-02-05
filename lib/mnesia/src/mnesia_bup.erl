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

%%
-module(mnesia_bup).
-export([
         %% Public interface
         iterate/4,
         read_schema/2,
         fallback_bup/0,
         fallback_exists/0,
         tm_fallback_start/1,
         create_schema/1,
	 create_schema/2,
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
         trav_apply/5
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

-type fallback_args() :: #fallback_args{}.

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
    try read_schema_section(R) of
	{R2, {Header, Schema, Rest}} ->
            Ext = get_ext_types(Schema),
	    try iter(R2, Header, Schema, Ext, Fun, Acc, Rest) of
		{ok, R3, Res} ->
		    close_read(R3),
		    {ok, Res}
	    catch throw:Err ->
		    close_read(R2),
		    Err;
		  _:Reason:Stacktrace ->
		    close_read(R2),
		    {error, {Reason, Stacktrace}}
	    end
    catch throw:{error,_} = Err ->
	    Err
    end.

get_ext_types(Schema) ->
    try
        List = lookup_schema(schema, Schema),
        case lists:keyfind(user_properties, 1, List) of
            {_, Props} ->
                proplists:get_value(
                  mnesia_backend_types, Props, []);
            false ->
                []
        end
    catch
        throw:{error, {"Cannot lookup",_}} ->
            []
    end.

iter(R, Header, Schema, Ext, Fun, Acc, []) ->
    case safe_apply(R, read, [R#restore.bup_data]) of
        {R2, []} ->
            Res = Fun([], Header, Schema, Ext, Acc),
            {ok, R2, Res};
        {R2, BupItems} ->
            iter(R2, Header, Schema, Ext, Fun, Acc, BupItems)
    end;
iter(R, Header, Schema, Ext, Fun, Acc, BupItems) ->
    Acc2 = Fun(BupItems, Header, Schema, Ext, Acc),
    iter(R, Header, Schema, Ext, Fun, Acc2, []).

-spec safe_apply(#restore{}, atom(), list()) -> tuple().
safe_apply(R, write, [_, Items]) when Items =:= [] ->
    R;
safe_apply(R, What, Args) ->
    Abort = abort_restore_fun(R, What, Args),
    Mod = R#restore.bup_module,
    try apply(Mod, What, Args) of
	{ok, Opaque, Items} when What =:= read ->
	    {R#restore{bup_data = Opaque}, Items};
	{ok, Opaque}  when What =/= read->
	    R#restore{bup_data = Opaque};
	{error, Re} ->
	    Abort(Re);
	Re ->
	    Abort(Re)
    catch _:Re ->
	    Abort(Re)
    end.

-spec abort_restore_fun(_, _, _) -> fun((_) -> no_return()).
abort_restore_fun(R, What, Args) ->
    fun(Re) -> abort_restore(R, What, Args, Re) end.

abort_restore(R = #restore{bup_module=Mod}, What, Args, Reason) ->
    dbg_out("Restore aborted. ~p:~p~p -> ~p~n",
            [Mod, What, Args, Reason]),
    close_read(R),
    throw({error, Reason}).

close_read(#restore{bup_module=Mod, bup_data=Opaque}) ->
    ?SAFE(Mod:close_read(Opaque)).

fallback_to_schema() ->
    Fname = fallback_bup(),
    fallback_to_schema(Fname).

fallback_to_schema(Fname) ->
    Mod = mnesia_backup,
    case read_schema(Mod, Fname) of
        {error, Reason} ->
            {error, Reason};
        Schema ->
            try lookup_schema(schema, Schema) of
		List -> {ok, fallback, List}
	    catch throw:_ ->
		    {error, "No schema in fallback"}
            end
    end.

%% Opens Opaque reads schema and then close
read_schema(Mod, Opaque) ->
    R = #restore{bup_module = Mod, bup_data = Opaque},
    try read_schema_section(R) of
        {R2, {_Header, Schema, _}} ->
	    close_read(R2),
	    Schema
    catch throw:{error,_} = Error ->
	    Error
    end.

%% Open backup media and extract schema
%% rewind backup media and leave it open
%% Returns {R, {Header, Schema}}
read_schema_section(R) ->
    {R2, {H, Schema, Rest}} = do_read_schema_section(R),
    Schema2 = convert_schema(H#log_header.log_version, Schema),
    {R2, {H, Schema2, Rest}}.

do_read_schema_section(R) ->
    R2 = safe_apply(R, open_read, [R#restore.bup_data]),
    try
	{R3, RawSchema} = safe_apply(R2, read, [R2#restore.bup_data]),
	do_read_schema_section(R3, verify_header(RawSchema), [])
    catch T:E:S ->
	    close_read(R2),
	    erlang:raise(T,E,S)
    end.

do_read_schema_section(R, {ok, B, C, []}, Acc) ->
    case safe_apply(R, read, [R#restore.bup_data]) of
        {R2, []} ->
            {R2, {B, Acc, []}};
        {R2, RawSchema} ->
            do_read_schema_section(R2, {ok, B, C, RawSchema}, Acc)
    end;

do_read_schema_section(R, {ok, B, C, [Head | Tail]}, Acc)
        when element(1, Head) =:= schema ->
    do_read_schema_section(R, {ok, B, C, Tail}, Acc ++ [Head]);

do_read_schema_section(R, {ok, B, _C, Rest}, Acc) ->
    {R, {B, Acc, Rest}};

do_read_schema_section(_R, {error, Reason}, _Acc) ->
    throw({error, Reason}).

verify_header([H | RawSchema]) when is_record(H, log_header) ->
    Current = mnesia_log:backup_log_header(),
    if
        H#log_header.log_kind =:= Current#log_header.log_kind ->
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
    {error, {"Missing header. Cannot be used as backup.", ?CATCH(hd(RawSchema))}}.

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
        H#log_header.log_version =:= Latest ->
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
            List = mnesia_schema:get_initial_schema(disc_copies, [node()], []),
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
create_schema(Nodes) ->
    create_schema(Nodes, []).

create_schema([], Props) ->
    create_schema([node()], Props);
create_schema(Ns, Props) when is_list(Ns), is_list(Props) ->
    case is_set(Ns) of
        true ->
            create_schema(Ns, mnesia_schema:ensure_no_schema(Ns), Props);
        false ->
            {error, {combine_error, Ns}}
    end;
create_schema(Ns, _Props) ->
    {error, {badarg, Ns}}.

is_set(List) when is_list(List) ->
    ordsets:is_set(lists:sort(List));
is_set(_) ->
    false.

create_schema(Ns, ok, Props) ->
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
                            try make_initial_backup(Ns, File, Mod, Props) of
                                {ok, _Res} ->
                                    case do_install_fallback(File, Mod) of
                                        ok ->
                                            file:delete(File),
                                            ok;
                                        {error, Reason} ->
                                            {error, Reason}
                                    end
			    catch throw:{error, Reason} ->
                                    {error, Reason}
                            end
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end;
create_schema(_Ns, {error, Reason}, _) ->
    {error, Reason};
create_schema(_Ns, Reason, _) ->
    {error, Reason}.

mk_str() ->
    Now = integer_to_list(erlang:unique_integer([positive])),
    lists:concat([node()] ++ Now ++ ".TMP").

make_initial_backup(Ns, Opaque, Mod) ->
    make_initial_backup(Ns, Opaque, Mod, []).

make_initial_backup(Ns, Opaque, Mod, Props) ->
    Orig = mnesia_schema:get_initial_schema(disc_copies, Ns, Props),
    Modded = proplists:delete(storage_properties, proplists:delete(majority, Orig)),
    Schema = [{schema, schema, Modded}],
    O2 = do_apply(Mod, open_write, [Opaque], Opaque),
    O3 = do_apply(Mod, write, [O2, [mnesia_log:backup_log_header()]], O2),
    O4 = do_apply(Mod, write, [O3, Schema], O3),
    O5 = do_apply(Mod, commit_write, [O4], O4),
    {ok, O5}.

do_apply(_, write, [_, Items], Opaque) when Items =:= [] ->
    Opaque;
do_apply(Mod, What, Args, _Opaque) ->
    try apply(Mod, What, Args) of
        {ok, Opaque2} ->  Opaque2;
        {error, Reason} -> throw({error, Reason})
    catch _:Reason ->
	    throw({error, {'EXIT', Reason}})
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

do_install_fallback(Opaque,  Mod) when is_atom(Mod) ->
    do_install_fallback(Opaque, [{module, Mod}]);
do_install_fallback(Opaque, Args) when is_list(Args) ->
    case check_fallback_args(Args, #fallback_args{opaque = Opaque}) of
        {ok, FA} ->
            do_install_fallback(FA);
        {error, Reason} ->
            {error, Reason}
    end;
do_install_fallback(_Opaque, Args) ->
    {error, {badarg, Args}}.

check_fallback_args([Arg | Tail], FA) ->
    try check_fallback_arg_type(Arg, FA) of
        FA2 ->
            check_fallback_args(Tail, FA2)
    catch error:_ ->
	    {error, {badarg, Arg}}
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

atom_list([H | T]) when is_atom(H) ->
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
    Res = iterate(Mod, fun restore_recs/5, Opaque, State),
    unlink(ClientPid),
    ClientPid ! {self(), Res},
    exit(shutdown).

restore_recs(_, _, _, _, stop) ->
    throw({error, "restore_recs already stopped"});

restore_recs(Recs, Header, Schema, Ext, {start, FA}) ->
    %% No records in backup
    Schema2 = convert_schema(Header#log_header.log_version, Schema),
    CreateList = lookup_schema(schema, Schema2),
    try mnesia_schema:list2cs(CreateList) of
        Cs ->
            Ns = get_fallback_nodes(FA, Cs#cstruct.disc_copies),
            global:set_lock({{mnesia_table_lock, schema}, self()}, Ns, infinity),
            Args = [self(), FA],
            Pids = [spawn_link(N, ?MODULE, fallback_receiver, Args) || N <- Ns],
            send_fallback(Pids, {start, Header, Schema2}),
            Res = restore_recs(Recs, Header, Schema2, Ext, Pids),
            global:del_lock({{mnesia_table_lock, schema}, self()}, Ns),
            Res
    catch _:Reason ->
            throw({error, {"Bad schema in restore_recs", Reason}})
    end;

restore_recs([], _Header, _Schema, _Ext, Pids) ->
    send_fallback(Pids, swap),
    send_fallback(Pids, stop),
    stop;

restore_recs(Recs, _, _, _, Pids) ->
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

send_fallback(Pids, Msg) when is_list(Pids), Pids =/= [] ->
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

-spec fallback_receiver(pid(), fallback_args()) -> no_return().
fallback_receiver(Master, FA) ->
    process_flag(trap_exit, true),

    Res = try
	      register(mnesia_fallback, self()),
	      FA2 = check_fallback_dir(FA),
	      Bup = FA2#fallback_args.fallback_bup,
	      false = mnesia_lib:exists(Bup),
	      Mod = mnesia_backup,
	      Tmp = FA2#fallback_args.fallback_tmp,
	      R = #restore{mode = replace,
			   bup_module = Mod,
			   bup_data = Tmp},
	      file:delete(Tmp),
	      fallback_receiver_loop(Master, R, FA2, schema)
	  catch
	      error:_ ->
		  Reason = {already_exists, node()},
		  local_fallback_error(Master, Reason);
	      throw:{error, Reason} ->
		  local_fallback_error(Master, Reason)
	  end,
    exit(Res).

local_fallback_error(Master, Reason) ->
    Master ! {self(), {error, Reason}},
    unlink(Master),
    exit(Reason).


check_fallback_dir(Master, FA) ->
    try check_fallback_dir(FA)
    catch throw:{error,Reason} ->
	    local_fallback_error(Master, Reason)
    end.

check_fallback_dir(FA) ->
    case mnesia:system_info(schema_location) of
        ram ->
            Reason = {has_no_disc, node()},
            throw({error, Reason});
        _ ->
            Dir = check_fallback_dir_arg(FA),
            Bup = filename:join([Dir, fallback_name()]),
            Tmp = filename:join([Dir, fallback_tmp_name()]),
            FA#fallback_args{fallback_bup = Bup,
                             fallback_tmp = Tmp,
                             mnesia_dir = Dir}
    end.

check_fallback_dir_arg(FA) ->
    case FA#fallback_args.use_default_dir of
        true ->
            mnesia_lib:dir();
        false when FA#fallback_args.scope =:= local ->
            Dir = FA#fallback_args.mnesia_dir,
            try mnesia_monitor:do_check_type(dir, Dir)
	    catch _:_ ->
                    Reason = {badarg, {dir, Dir}, node()},
		    throw({error, Reason})
	    end;
        false when FA#fallback_args.scope =:= global ->
            Reason = {combine_error, global, dir, node()},
            throw({error, Reason})
    end.

fallback_receiver_loop(Master, R, FA, State) ->
    receive
        {Master, {start, Header, Schema}} when State =:= schema ->
            Dir = FA#fallback_args.mnesia_dir,
            throw_bad_res(ok, mnesia_schema:opt_create_dir(true, Dir)),
            R2 = safe_apply(R, open_write, [R#restore.bup_data]),
            R3 = safe_apply(R2, write, [R2#restore.bup_data, [Header]]),
            BupSchema = [schema2bup(S) || S <- Schema],
            R4 = safe_apply(R3, write, [R3#restore.bup_data, BupSchema]),
            Master ! {self(), ok},
            fallback_receiver_loop(Master, R4, FA, records);

        {Master, {records, Recs}} when State =:= records ->
            R2 = safe_apply(R, write, [R#restore.bup_data, Recs]),
            Master ! {self(), ok},
            fallback_receiver_loop(Master, R2, FA, records);

        {Master, swap} when State =/= schema ->
            ?eval_debug_fun({?MODULE, fallback_receiver_loop, pre_swap}, []),
            safe_apply(R, commit_write, [R#restore.bup_data]),
            Bup = FA#fallback_args.fallback_bup,
            Tmp = FA#fallback_args.fallback_tmp,
            throw_bad_res(ok, file:rename(Tmp, Bup)),
            ?SAFE(mnesia_lib:set(active_fallback, true)),
            ?eval_debug_fun({?MODULE, fallback_receiver_loop, post_swap}, []),
            Master ! {self(), ok},
            fallback_receiver_loop(Master, R, FA, stop);

        {Master, stop} when State =:= stop ->
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

-record(local_tab, {name,
                    storage_type,
                    open,
                    add,
                    close,
		    swap,
                    record_name,
                    opened}).

tm_fallback_start(IgnoreFallback) ->
    mnesia_schema:lock_schema(),
    Res = do_fallback_start(fallback_exists(), IgnoreFallback),
    mnesia_schema:unlock_schema(),
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

    BupFile = fallback_bup(),
    Mod = mnesia_backup,
    LocalTabs = ?ets_new_table(mnesia_local_tables, [set, public, {keypos, 2}]),
    case iterate(Mod, fun restore_tables/5, BupFile, {start, LocalTabs}) of
        {ok, _Res} ->
            ?SAFE(dets:close(schema)),
            TmpSchema = mnesia_lib:tab2tmp(schema),
            DatSchema = mnesia_lib:tab2dat(schema),
	    AllLT  = ?ets_match_object(LocalTabs, '_'),
	    ?ets_delete_table(LocalTabs),
            case file:rename(TmpSchema, DatSchema) of
                ok ->
		    [(LT#local_tab.swap)(LT#local_tab.name, LT) ||
			LT <- AllLT, LT#local_tab.name =/= schema],
                    file:delete(BupFile),
                    ok;
                {error, Reason} ->
                    file:delete(TmpSchema),
                    {error, {"Cannot start from fallback. Rename error.", Reason}}
            end;
        {error, Reason} ->
            {error, {"Cannot start from fallback", Reason}}
    end.

restore_tables(All=[Rec | Recs], Header, Schema, Ext,
               State={local, LocalTabs, LT}) ->
    Tab = element(1, Rec),
    if
        Tab =:= LT#local_tab.name ->
            Key = element(2, Rec),
            (LT#local_tab.add)(Tab, Key, Rec, LT),
            restore_tables(Recs, Header, Schema, Ext, State);
        true ->
            NewState = {new, LocalTabs},
            restore_tables(All, Header, Schema, Ext, NewState)
    end;
restore_tables(All=[Rec | Recs], Header, Schema, Ext, {new, LocalTabs}) ->
    Tab = element(1, Rec),
    case ?ets_lookup(LocalTabs, Tab) of
        [] ->
            State = {not_local, LocalTabs, Tab},
            restore_tables(Recs, Header, Schema, Ext, State);
        [LT] when is_record(LT, local_tab) ->
	    State = {local, LocalTabs, LT},
	    case LT#local_tab.opened of
		true ->  ignore;
		false ->
		    (LT#local_tab.open)(Tab, LT),
		    ?ets_insert(LocalTabs,LT#local_tab{opened=true})
	    end,
            restore_tables(All, Header, Schema, Ext, State)
    end;
restore_tables(All=[Rec | Recs], Header, Schema, Ext,
               S = {not_local, LocalTabs, PrevTab}) ->
    Tab = element(1, Rec),
    if
        Tab =:= PrevTab ->
            restore_tables(Recs, Header, Schema, Ext, S);
        true ->
            State = {new, LocalTabs},
            restore_tables(All, Header, Schema, Ext, State)
    end;
restore_tables(Recs, Header, Schema, Ext, {start, LocalTabs}) ->
    Dir = mnesia_lib:dir(),
    OldDir = filename:join([Dir, "OLD_DIR"]),
    mnesia_schema:purge_dir(OldDir, []),
    mnesia_schema:purge_dir(Dir, [fallback_name()]),
    init_dat_files(Schema, Ext, LocalTabs),
    State = {new, LocalTabs},
    restore_tables(Recs, Header, Schema, Ext, State);
restore_tables([], _Header, _Schema, _Ext, State) ->
    State.

%% Creates all neccessary dat files and inserts
%% the table definitions in the schema table
%%
%% Returns a list of local_tab tuples for all local tables
init_dat_files(Schema, Ext, LocalTabs) ->
    TmpFile = mnesia_lib:tab2tmp(schema),
    Args = [{file, TmpFile}, {keypos, 2}, {type, set}],
    case dets:open_file(schema, Args) of % Assume schema lock
        {ok, _} ->
            create_dat_files(Schema, Ext, LocalTabs),
            ok = dets:close(schema),
            LocalTab = #local_tab{name         = schema,
                                  storage_type = disc_copies,
                                  open         = undefined,
                                  add          = undefined,
                                  close        = undefined,
				  swap         = undefined,
                                  record_name  = schema,
                                  opened = false},
            ?ets_insert(LocalTabs, LocalTab);
        {error, Reason} ->
            throw({error, {"Cannot open file", schema, Args, Reason}})
    end.

create_dat_files([{schema, schema, TabDef} | Tail], Ext, LocalTabs) ->
    ok = dets:insert(schema, {schema, schema, TabDef}),
    create_dat_files(Tail, Ext, LocalTabs);
create_dat_files([{schema, Tab, TabDef} | Tail], Ext, LocalTabs) ->
    TmpFile = mnesia_lib:tab2tmp(Tab),
    DatFile = mnesia_lib:tab2dat(Tab),
    DclFile = mnesia_lib:tab2dcl(Tab),
    DcdFile = mnesia_lib:tab2dcd(Tab),
    Expunge = fun() ->
		      file:delete(DatFile),
		      file:delete(DclFile),
		      file:delete(DcdFile)
	      end,

    mnesia_lib:dets_sync_close(Tab),
    file:delete(TmpFile),
    Cs = mnesia_schema:list2cs(TabDef, Ext),
    ok = dets:insert(schema, {schema, Tab, TabDef}),
    RecName = Cs#cstruct.record_name,
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    delete_ext(Storage, Tab),
    Semantics = mnesia_lib:semantics(Storage, storage),
    if
	Semantics =:= undefined ->
            ok = dets:delete(schema, {schema, Tab}),
            create_dat_files(Tail, Ext, LocalTabs);
        Semantics =:= disc_only_copies ->
	    Open = disc_only_open_fun(Storage, Cs),
	    Add = disc_only_add_fun(Storage, Cs),
	    Close = disc_only_close_fun(Storage),
	    Swap = disc_only_swap_fun(Storage, Expunge, Open, Close),
            LocalTab = #local_tab{name         = Tab,
                                  storage_type = Storage,
                                  open         = Open,
                                  add          = Add,
                                  close        = Close,
				  swap         = Swap,
                                  record_name  = RecName,
                                  opened       = false},
            ?ets_insert(LocalTabs, LocalTab),
	    create_dat_files(Tail, Ext, LocalTabs);
        Semantics =:= ram_copies; Storage =:= disc_copies ->
	    Open = fun(T, LT) when T =:= LT#local_tab.name ->
			   mnesia_log:open_log({?MODULE, T},
					       mnesia_log:dcl_log_header(),
					       TmpFile,
					       false,
					       false,
					       read_write)
		   end,
            Add = fun(T, Key, Rec, LT) when T =:= LT#local_tab.name ->
			  Log = {?MODULE, T},
			  case Rec of
			      {_T, Key} ->
				  mnesia_log:append(Log, {{T, Key}, {T, Key}, delete});
			      (Rec) when T =:= RecName ->
				  mnesia_log:append(Log, {{T, Key}, Rec, write});
			      (Rec) ->
				  Rec2 = setelement(1, Rec, RecName),
				  mnesia_log:append(Log, {{T, Key}, Rec2, write})
			  end
		  end,
            Close = fun(T, LT) when T =:= LT#local_tab.name ->
			    mnesia_log:close_log({?MODULE, T})
                    end,
	    Swap = fun(T, LT) when T =:= LT#local_tab.name ->
			   Expunge(),
			   if
			       Storage =:= ram_copies, LT#local_tab.opened =:= false ->
				   ok;
			       true ->
				   Log = mnesia_log:open_log(fallback_tab,
							     mnesia_log:dcd_log_header(),
							     DcdFile,
							     false),
				   mnesia_log:close_log(Log),
				   case LT#local_tab.opened of
				       true ->
					   Close(T,LT);
				       false ->
					   Open(T,LT),
					   Close(T,LT)
				   end,
				   case file:rename(TmpFile, DclFile) of
				       ok ->
					   ok;
				       {error, Reason} ->
					   mnesia_lib:fatal("Cannot rename file ~tp -> ~tp: ~tp~n",
							    [TmpFile, DclFile, Reason])
				   end
			   end
		   end,
            LocalTab = #local_tab{name         = Tab,
                                  storage_type = Storage,
                                  open         = Open,
                                  add          = Add,
                                  close        = Close,
				  swap         = Swap,
                                  record_name  = RecName,
				  opened       = false
				 },
            ?ets_insert(LocalTabs, LocalTab),
            create_dat_files(Tail, Ext, LocalTabs);
	true ->
	    error({unknown_semantics, [{semantics, Semantics},
				       {tabdef, TabDef},
				       {ext, Ext}]})
    end;
create_dat_files([{schema, Tab} | Tail], Ext, LocalTabs) ->
    ?ets_delete(LocalTabs, Tab),
    ok = dets:delete(schema, {schema, Tab}),
    TmpFile = mnesia_lib:tab2tmp(Tab),
    mnesia_lib:dets_sync_close(Tab),
    file:delete(TmpFile),
    create_dat_files(Tail, Ext, LocalTabs);
create_dat_files([], _Ext, _LocalTabs) ->
    ok.

delete_ext({ext, Alias, Mod}, Tab) ->
    Mod:close_table(Alias, Tab),
    Mod:delete_table(Alias, Tab),
    ok;
delete_ext(_, _) ->
    ok.


disc_only_open_fun(disc_only_copies, #cstruct{name = Tab} =Cs) ->
    TmpFile = mnesia_lib:tab2tmp(Tab),
    Args = [{file, TmpFile}, {keypos, 2},
	    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)}],
    fun(T, LT) when T =:= LT#local_tab.name ->
	    case mnesia_lib:dets_sync_open(T, Args) of
		{ok, _} ->
		    ok;
		{error, Reason} ->
		    throw({error, {"Cannot open file", T, Args, Reason}})
	    end
    end;
disc_only_open_fun({ext,Alias,Mod}, Cs) ->
    fun(T, LT) when T =:= LT#local_tab.name ->
	    ok = Mod:load_table(Alias, T, restore, mnesia_schema:cs2list(Cs))
    end.

disc_only_add_fun(Storage, #cstruct{name = Tab,
				    record_name = RecName}) ->
    fun(T, Key, Rec, #local_tab{name = T}) when T =:= Tab->
	    case Rec of
		{_T, Key} ->
		    ok = mnesia_lib:db_erase(Storage, T, Key);
		(Rec) when T =:= RecName ->
		    ok = mnesia_lib:db_put(Storage, T, Rec);
		(Rec) ->
		    ok = mnesia_lib:db_put(Storage, T,
					   setelement(1, Rec, RecName))
	    end
    end.

disc_only_close_fun(disc_only_copies) ->
    fun(T, LT) when T =:= LT#local_tab.name ->
	    mnesia_lib:dets_sync_close(T)
    end;
disc_only_close_fun({ext, Alias, Mod}) ->
    fun(T, _LT) ->
	    Mod:sync_close_table(Alias, T)
    end.


disc_only_swap_fun(disc_only_copies, Expunge, Open, Close) ->
    fun(T, LT) when T =:= LT#local_tab.name ->
            TmpFile = mnesia_lib:tab2tmp(T),
            DatFile = mnesia_lib:tab2dat(T),
	    Expunge(),
	    case LT#local_tab.opened of
		true ->
		    Close(T,LT);
		false ->
		    Open(T,LT),
		    Close(T,LT)
	    end,
	    case file:rename(TmpFile, DatFile) of
		ok ->
		    ok;
		{error, Reason} ->
		    mnesia_lib:fatal("Cannot rename file ~tp -> ~tp: ~tp~n",
				     [TmpFile, DatFile, Reason])
	    end
    end;
disc_only_swap_fun({ext, _Alias, _Mod}, _Expunge, _Open, Close) ->
    fun(T, #local_tab{name = T} = LT) ->
	    Close(T, LT)
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

-spec uninstall_fallback_master(pid(), fallback_args()) -> no_return().
uninstall_fallback_master(ClientPid, FA) ->
    process_flag(trap_exit, true),

    FA2 = check_fallback_dir(ClientPid, FA), % May exit
    Bup = FA2#fallback_args.fallback_bup,
    case fallback_to_schema(Bup) of
        {ok, fallback, List} ->
            Cs = mnesia_schema:list2cs(List),
            try get_fallback_nodes(FA, Cs#cstruct.disc_copies) of
                Ns when is_list(Ns) ->
                    do_uninstall(ClientPid, Ns, FA)
	    catch throw:{error, Reason} ->
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
    FA2 = check_fallback_dir(Master, FA),  % May exit
    Master ! {self(), started},

    receive
        {Master, do_uninstall} ->
            ?eval_debug_fun({?MODULE, uninstall_fallback2, pre_delete}, []),
            ?SAFE(mnesia_lib:set(active_fallback, false)),
            Tmp = FA2#fallback_args.fallback_tmp,
            Bup = FA2#fallback_args.fallback_bup,
            file:delete(Tmp),
            Res = file:delete(Bup),
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
rec_uninstall(_, [], Res) ->
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Backup traversal

%% Iterate over a backup and produce a new backup.
%% Fun(BackupItem, Acc) is applied for each BackupItem.
%%
%% Valid BackupItems are:
%%
%%   {schema, Tab}              Table to be deleted
%%   {schema, Tab, CreateList}  Table to be created, CreateList may be empty
%%   {schema, db_nodes, DbNodes}List of nodes, defaults to [node()] OLD
%%   {schema, version, Version} Schema version                      OLD
%%   {schema, cookie, Cookie}   Unique schema cookie                OLD
%%   {Tab, Key}                 Oid for record to be deleted
%%   Record                     Record to be inserted.
%%
%% The Fun must return a tuple {BackupItems, NewAcc}
%% where BackupItems is a list of valid BackupItems and
%% NewAcc is a new accumulator value. Once BackupItems
%% that not are schema  related has been returned, no more schema
%% items may be  returned. The schema related items must always be
%% first in the backup.
%%
%% If TargetMod =:= read_only, no new backup will be created.
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
            TargetMod =/= read_only ->
                try do_apply(TargetMod, open_write, [Target], Target)
		catch throw:{error, Error} ->
                        unlink(ClientPid),
                        ClientPid ! {iter_done, self(), {error, Error}},
                        exit(Error)
                end;
            true ->
                ignore
        end,
    A = {start, Fun, Acc, TargetMod, Iter},
    Res =
        case iterate(SourceMod, fun trav_apply/5, Source, A) of
            {ok, {iter, _, Acc2, _, Iter2}} when TargetMod =/= read_only ->
                try
		    do_apply(TargetMod, commit_write, [Iter2], Iter2),
		    {ok, Acc2}
		catch throw:{error, Reason} ->
                        {error, Reason}
                end;
            {ok, {iter, _, Acc2, _, _}} ->
                {ok, Acc2};
            {error, Reason} when TargetMod =/= read_only->
                ?CATCH(do_apply(TargetMod, abort_write, [Iter], Iter)),
                {error, {"Backup traversal failed", Reason}};
            {error, Reason} ->
                {error, {"Backup traversal failed", Reason}}
        end,
    unlink(ClientPid),
    ClientPid ! {iter_done, self(), Res}.

trav_apply(Recs, _Header, _Schema, _Ext, {iter, Fun, Acc, Mod, Iter}) ->
    {NewRecs, Acc2} = filter_foldl(Fun, Acc, Recs),
    if
        Mod =/= read_only, NewRecs =/= [] ->
            Iter2 = do_apply(Mod, write, [Iter, NewRecs], Iter),
            {iter, Fun, Acc2, Mod, Iter2};
        true ->
            {iter, Fun, Acc2, Mod, Iter}
    end;
trav_apply(Recs, Header, Schema, Ext, {start, Fun, Acc, Mod, Iter}) ->
    Iter2 =
        if
            Mod =/= read_only ->
                do_apply(Mod, write, [Iter, [Header]], Iter);
            true ->
                Iter
        end,
    TravAcc = trav_apply(Schema, Header, Schema, Ext,
                         {iter, Fun, Acc, Mod, Iter2}),
    trav_apply(Recs, Header, Schema, Ext, TravAcc).

filter_foldl(Fun, Acc, [Head|Tail]) ->
    case Fun(Head, Acc) of
        {HeadItems, HeadAcc} when is_list(HeadItems) ->
            {TailItems, TailAcc} = filter_foldl(Fun, HeadAcc, Tail),
            {HeadItems ++ TailItems, TailAcc};
        Other ->
            throw({error, {"Fun must return a list", Other}})
    end;
filter_foldl(_Fun, Acc, []) ->
    {[], Acc}.

