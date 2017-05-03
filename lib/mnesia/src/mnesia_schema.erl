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
%% In this module we provide a number of explicit functions
%% to maninpulate the schema. All these functions are called
%% within a special schema transaction.
%%
%% We also have an init/1 function defined here, this func is
%% used by mnesia:start() to initialize the entire schema.

-module(mnesia_schema).

-export([
         add_backend_type/2,
	 do_add_backend_type/2,
	 delete_backend_type/1,
	 do_delete_backend_type/1,
	 backend_types/0,
	 add_index_plugin/3,
	 do_add_index_plugin/3,
	 delete_index_plugin/1,
	 do_delete_index_plugin/1,
	 index_plugins/0,
         add_snmp/2,
         add_table_copy/3,
         add_table_index/2,
	 arrange_restore/3,
         attr_tab_to_pos/2,
         attr_to_pos/2,
         change_table_copy_type/3,
         change_table_access_mode/2,
         change_table_load_order/2,
	 change_table_majority/2,
	 change_table_frag/2,
%%	 clear_table/1,  %% removed since it is not a schema op anymore
         create_table/1,
	 cs2list/1,
	 vsn_cs2list/1,
         del_snmp/1,
         del_table_copy/2,
         del_table_index/2,
         delete_cstruct/2,
         delete_schema/1,
         delete_schema2/0,
         delete_table/1,
         delete_table_property/2,
         dump_tables/1,
         ensure_no_schema/1,
	 get_create_list/1,
         get_initial_schema/3,
	 get_table_properties/1,
         info/0,
         info/1,
         init/1,
	 init_backends/0,
         insert_cstruct/3,
	 is_remote_member/1,
         list2cs/1,
         list2cs/2,
         lock_schema/0,
         merge_schema/0,
         merge_schema/1,
         move_table/3,
	 normalize_cs/2,
         opt_create_dir/2,
         prepare_commit/3,
         purge_dir/2,
         purge_tmp_files/0,
         ram_delete_table/2,
%         ram_delete_table/3,
	 read_cstructs_from_disc/0,
         read_nodes/0,
         remote_read_schema/0,
	 restore/1,
         restore/2,
         restore/3,
	 schema_coordinator/3,
	 set_where_to_read/3,
         transform_table/4,
         undo_prepare_commit/2,
         unlock_schema/0,
         version/0,
         write_table_property/2
        ]).

%% Exports for mnesia_frag
-export([
	 get_tid_ts_and_lock/2,
	 make_create_table/1,
         ensure_active/1,
	 pick/4,
	 verify/3,
	 incr_version/1,
	 check_keys/3,
	 check_duplicates/2,
	 make_delete_table/2
	]).

%% Needed outside to be able to use/set table_properties
%% from user (not supported)
-export([schema_transaction/1,
	 insert_schema_ops/2,
	 do_create_table/1,
	 do_delete_table/1,
	 do_read_table_property/2,
	 do_delete_table_property/2,
         do_write_table_property/2,
         do_change_table_copy_type/3]).

-include("mnesia.hrl").
-include_lib("kernel/include/file.hrl").

-import(mnesia_lib, [set/2, del/2, verbose/2, dbg_out/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Here comes the init function which also resides in
%% this module, it is called upon by the trans server
%% at startup of the system
%%
%% We have a meta table which looks like
%% {table, schema,
%%    {type, set},
%%    {disc_copies, all},
%%    {arity, 2}
%%    {attributes, [key, val]}
%%
%% This means that we have a series of {schema, Name, Cs} tuples
%% in a table called schema !!

init(IgnoreFallback) ->
    Res = read_schema(true, IgnoreFallback),
    {ok, Source, _CreateList} = exit_on_error(Res),
    verbose("Schema initiated from: ~p~n", [Source]),
    set({schema, tables}, []),
    set({schema, local_tables}, []),
    do_set_schema(schema),
    Tabs = set_schema(?ets_first(schema)),
    lists:foreach(fun(Tab) -> clear_whereabouts(Tab) end, Tabs),
    set({schema, where_to_read}, node()),
    set({schema, load_node}, node()),
    set({schema, load_reason}, initial),
    mnesia_controller:add_active_replica(schema, node()),
    init_backends().


init_backends() ->
    Backends = lists:foldl(fun({Alias, Mod}, Acc) ->
				   orddict:append(Mod, Alias, Acc)
			   end, orddict:new(), get_ext_types()),
    [init_backend(Mod, Aliases) || {Mod, Aliases} <- Backends],
    ok.

init_backend(Mod, [_|_] = Aliases) ->
    case Mod:init_backend() of
	ok ->
	    Mod:add_aliases(Aliases);
	Error ->
	    mnesia:abort({backend_init_error, Error})
    end.

exit_on_error({error, Reason}) ->
    exit(Reason);
exit_on_error(GoodRes) ->
    GoodRes.

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _} -> mnesia_lib:other_val(Var);
	Value -> Value
    end.

%% This function traverses all cstructs in the schema and
%% sets all values in mnesia_gvar accordingly for each table/cstruct

set_schema('$end_of_table') ->
    [];
set_schema(Tab) ->
    do_set_schema(Tab),
    [Tab | set_schema(?ets_next(schema, Tab))].

get_create_list(Tab) ->
    ?ets_lookup_element(schema, Tab, 3).

do_set_schema(Tab) ->
    List = get_create_list(Tab),
    Cs = list2cs(List),
    do_set_schema(Tab, Cs).

do_set_schema(Tab, Cs) ->
    Type = Cs#cstruct.type,
    set({Tab, setorbag}, Type),
    set({Tab, local_content}, Cs#cstruct.local_content),
    set({Tab, ram_copies}, Cs#cstruct.ram_copies),
    set({Tab, disc_copies}, Cs#cstruct.disc_copies),
    set({Tab, disc_only_copies}, Cs#cstruct.disc_only_copies),
    set({Tab, external_copies}, Cs#cstruct.external_copies),
    set({Tab, load_order}, Cs#cstruct.load_order),
    set({Tab, access_mode}, Cs#cstruct.access_mode),
    set({Tab, majority}, Cs#cstruct.majority),
    set({Tab, all_nodes}, mnesia_lib:cs_to_nodes(Cs)),
    set({Tab, snmp}, Cs#cstruct.snmp),
    set({Tab, user_properties}, Cs#cstruct.user_properties),
    [set({Tab, user_property, element(1, P)}, P) || P <- Cs#cstruct.user_properties],
    set({Tab, frag_properties}, Cs#cstruct.frag_properties),
    mnesia_frag:set_frag_hash(Tab, Cs#cstruct.frag_properties),
    set({Tab, storage_properties}, Cs#cstruct.storage_properties),
    set({Tab, attributes}, Cs#cstruct.attributes),
    Arity = length(Cs#cstruct.attributes) + 1,
    set({Tab, arity}, Arity),
    RecName =  Cs#cstruct.record_name,
    set({Tab, record_name}, RecName),
    set({Tab, wild_pattern}, wild(RecName, Arity)),
    set({Tab, index}, [P || {P,_} <- Cs#cstruct.index]),
    case Cs#cstruct.index of
        [] ->
            set({Tab, index_info}, mnesia_index:index_info(Type, []));
        _ ->
            ignore
    end,
    %% create actual index tabs later
    set({Tab, cookie}, Cs#cstruct.cookie),
    set({Tab, version}, Cs#cstruct.version),
    set({Tab, cstruct}, Cs),
    Storage = mnesia_lib:schema_cs_to_storage_type(node(), Cs),
    set({Tab, storage_type}, Storage),
    set_record_validation(Tab, Storage, RecName, Arity, Type),
    mnesia_lib:add({schema, tables}, Tab),
    Ns = mnesia_lib:cs_to_nodes(Cs),
    case lists:member(node(), Ns) of
        true ->
            mnesia_lib:add({schema, local_tables}, Tab);
        false when Tab == schema ->
            mnesia_lib:add({schema, local_tables}, Tab);
        false ->
            ignore
    end,
    set_ext_types(Tab, get_ext_types(), Cs#cstruct.external_copies).

set_record_validation(Tab, {ext,Alias,Mod}, RecName, Arity, Type) ->
    set({Tab, record_validation}, {RecName, Arity, Type, Alias, Mod});
set_record_validation(Tab, _, RecName, Arity, Type) ->
    set({Tab, record_validation}, {RecName, Arity, Type}).

set_ext_types(Tab, ExtTypes, ExtCopies) ->
    lists:foreach(
      fun({Type, _} = Key) ->
	      Nodes = case lists:keyfind(Key, 1, ExtCopies) of
			  {_, Ns} -> Ns;
			  false   -> []
		      end,
	      set({Tab, Type}, Nodes)
      end, ExtTypes).


wild(RecName, Arity) ->
    Wp0 = list_to_tuple(lists:duplicate(Arity, '_')),
    setelement(1, Wp0, RecName).

%% Temporarily read the local schema and return a list
%% of all nodes mentioned in the schema.DAT file
read_nodes() ->
    %% Ensure that we access the intended Mnesia
    %% directory. This function may not be called
    %% during startup since it will cause the
    %% application_controller to get into deadlock
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    case read_schema(false) of
		{ok, _Source, CreateList} ->
		    Cs = list2cs(CreateList),
		    {ok, Cs#cstruct.disc_copies ++ Cs#cstruct.ram_copies};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%% Returns Version from the tuple {Version,MasterNodes}
version() ->
    case read_schema(false) of
        {ok, Source, CreateList} when Source /= default ->
	    Cs = list2cs(CreateList),
            {Version, _Details} = Cs#cstruct.version,
            Version;
        _ ->
            case dir_exists(mnesia_lib:dir()) of
                true -> {1,0};
                false -> {0,0}
            end
    end.

%% Calculate next table version from old cstruct
incr_version(Cs) ->
    {{Major, Minor}, _} = Cs#cstruct.version,
    Nodes = mnesia_lib:intersect(val({schema, disc_copies}),
                                 mnesia_lib:cs_to_nodes(Cs)),
    V =
        case Nodes -- val({Cs#cstruct.name, active_replicas}) of
            [] -> {Major + 1, 0};    % All replicas are active
            _ -> {Major, Minor + 1}  % Some replicas are inactive
        end,
    Cs#cstruct{version = {V, {node(), erlang:timestamp()}}}.

%% Returns table name
insert_cstruct(Tid, Cs, KeepWhereabouts) ->
    Tab = Cs#cstruct.name,
    TabDef = cs2list(Cs),
    Val = {schema, Tab, TabDef},
    mnesia_checkpoint:tm_retain(Tid, schema, Tab, write),
    mnesia_subscr:report_table_event(schema, Tid, Val, write),
    Active = val({Tab, active_replicas}),

    case KeepWhereabouts of
        true ->
            ignore;
        false when Active == [] ->
            clear_whereabouts(Tab);
        false ->
            %% Someone else has initiated table
            ignore
    end,
    set({Tab, cstruct}, Cs),
    ?ets_insert(schema, Val),
    do_set_schema(Tab, Cs),
    Val.

clear_whereabouts(Tab) ->
    set({Tab, checkpoints}, []),
    set({Tab, subscribers}, []),
    set({Tab, where_to_read}, nowhere),
    set({Tab, active_replicas}, []),
    set({Tab, commit_work}, []),
    set({Tab, where_to_write}, []),
    set({Tab, where_to_commit}, []),
    set({Tab, load_by_force}, false),
    set({Tab, load_node}, unknown),
    set({Tab, load_reason}, unknown).

%% Returns table name
delete_cstruct(Tid, Cs) ->
    Tab = Cs#cstruct.name,
    TabDef = cs2list(Cs),
    Val = {schema, Tab, TabDef},
    mnesia_checkpoint:tm_retain(Tid, schema, Tab, delete),
    mnesia_subscr:report_table_event(schema, Tid, Val, delete),
    mnesia_controller:update(
      fun() ->
	      ?ets_match_delete(mnesia_gvar, {{Tab, '_'}, '_'}),
	      ?ets_match_delete(mnesia_gvar, {{Tab, '_', '_'}, '_'}),
	      del({schema, local_tables}, Tab),
	      del({schema, tables}, Tab),
	      ?ets_delete(schema, Tab)
      end),
    Val.

%% Delete the Mnesia directory on all given nodes
%% Requires that Mnesia is not running anywhere
%% Returns ok | {error,Reason}
delete_schema(Ns) when is_list(Ns), Ns /= [] ->
    RunningNs = mnesia_lib:running_nodes(Ns),
    Reason = "Cannot delete schema on all nodes",
    if
        RunningNs == [] ->
	    case rpc:multicall(Ns, ?MODULE, delete_schema2, []) of
		{Replies, []} ->
		    case [R || R <- Replies, R /= ok]  of
			[] ->
			    ok;
			BadReplies ->
			    verbose("~s: ~p~n", [Reason, BadReplies]),
			    {error, {"All nodes not running", BadReplies}}
		    end;
		{_Replies, BadNs} ->
                    verbose("~s: ~p~n", [Reason, BadNs]),
                    {error, {"All nodes not running", BadNs}}
            end;
        true ->
            verbose("~s: ~p~n", [Reason, RunningNs]),
            {error, {"Mnesia is not stopped everywhere", RunningNs}}
    end;
delete_schema(Ns) ->
    {error, {badarg, Ns}}.

delete_schema2() ->
    %% Ensure that we access the intended Mnesia
    %% directory. This function may not be called
    %% during startup since it will cause the
    %% application_controller to get into deadlock
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    case mnesia_lib:is_running() of
		no ->
		    Dir = mnesia_lib:dir(),
		    purge_dir(Dir, []),
		    ok;
		_ ->
		    {error, {"Mnesia still running", node()}}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

ensure_no_schema([H|T]) when is_atom(H) ->
    case rpc:call(H, ?MODULE, remote_read_schema, []) of
        {badrpc, Reason} ->
            {H, {"All nodes not running", H, Reason}};
        {ok,Source, _} when Source /= default ->
            {H, {already_exists, H}};
        _ ->
            ensure_no_schema(T)
    end;
ensure_no_schema([H|_]) ->
    {error,{badarg, H}};
ensure_no_schema([]) ->
    ok.

remote_read_schema() ->
    %% Ensure that we access the intended Mnesia
    %% directory. This function may not be called
    %% during startup since it will cause the
    %% application_controller to get into deadlock
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    case mnesia_monitor:get_env(schema_location) of
		opt_disc ->
		    read_schema(false);
		_ ->
		    read_schema(false)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

dir_exists(Dir) ->
    dir_exists(Dir, mnesia_monitor:use_dir()).
dir_exists(Dir, true) ->
    case file:read_file_info(Dir) of
        {ok, _} -> true;
        _ -> false
    end;
dir_exists(_Dir, false) ->
    false.

opt_create_dir(UseDir, Dir) when UseDir == true->
    case dir_exists(Dir, UseDir) of
        true ->
            check_can_write(Dir);
        false ->
            case file:make_dir(Dir) of
                ok ->
                    verbose("Create Directory ~p~n", [Dir]),
                    ok;
                {error, Reason} ->
                    verbose("Cannot create mnesia dir ~p~n", [Reason]),
                    {error, {"Cannot create Mnesia dir", Dir, Reason}}
            end
    end;
opt_create_dir(false, _) ->
    {error, {has_no_disc, node()}}.

check_can_write(Dir) ->
    case file:read_file_info(Dir) of
        {ok, FI} when FI#file_info.type == directory,
		      FI#file_info.access == read_write ->
            ok;
        {ok, _} ->
            {error, "Not allowed to write in Mnesia dir", Dir};
        _ ->
            {error, "Non existent Mnesia dir", Dir}
    end.

lock_schema() ->
    mnesia_lib:lock_table(schema).

unlock_schema() ->
    mnesia_lib:unlock_table(schema).

read_schema(Keep) ->
    read_schema(Keep, false).

%% The schema may be read for several reasons.
%% If Mnesia is not already started the read intention
%% we normally do not want the ets table named schema
%% be left around.
%% If Keep == true, the ets table schema is kept
%% If Keep == false, the ets table schema is removed
%%
%% Returns {ok, Source, SchemaCstruct} or {error, Reason}
%% Source may be: default | ram | disc | fallback

read_schema(Keep, IgnoreFallback) ->
    lock_schema(),
    Res =
        case mnesia:system_info(is_running) of
            yes ->
                {ok, ram, get_create_list(schema)};
            _IsRunning ->
                    case mnesia_monitor:use_dir() of
                        true ->
                            read_disc_schema(Keep, IgnoreFallback);
                        false when Keep == true ->
                            Args = [{keypos, 2}, public, named_table, set],
                            mnesia_monitor:mktab(schema, Args),
                            CreateList = get_initial_schema(ram_copies, []),
                            ?ets_insert(schema,{schema, schema, CreateList}),
                            {ok, default, CreateList};
                        false when Keep == false ->
			    CreateList = get_initial_schema(ram_copies, []),
                            {ok, default, CreateList}
                    end
        end,
    unlock_schema(),
    Res.

read_disc_schema(Keep, IgnoreFallback) ->
    Running = mnesia:system_info(is_running),
    case mnesia_bup:fallback_exists() of
        true when IgnoreFallback == false, Running /= yes ->
             mnesia_bup:fallback_to_schema();
        _ ->
            %% If we're running, we read the schema file even
            %% if fallback exists
            Dat = mnesia_lib:tab2dat(schema),
            case mnesia_lib:exists(Dat) of
                true ->
                    do_read_disc_schema(Dat, Keep);
                false ->
		    Dmp = mnesia_lib:tab2dmp(schema),
		    case mnesia_lib:exists(Dmp) of
			true ->
			    %% May only happen when toggling of
			    %% schema storage type has been
			    %% interrupted
			    do_read_disc_schema(Dmp, Keep);
			false ->
			    {error, "No schema file exists"}
		    end
            end
    end.

do_read_disc_schema(Fname, Keep) ->
    T =
        case Keep of
            false ->
                Args = [{keypos, 2}, public, set],
                ?ets_new_table(schema, Args);
            true ->
                Args = [{keypos, 2}, public, named_table, set],
                mnesia_monitor:mktab(schema, Args)
        end,
    Repair = mnesia_monitor:get_env(auto_repair),
    Res =  % BUGBUG Fixa till dcl!
        case mnesia_lib:dets_to_ets(schema, T, Fname, set, Repair, no) of
            loaded -> {ok, disc, ?ets_lookup_element(T, schema, 3)};
            Other -> {error, {"Cannot read schema", Fname, Other}}
        end,
    case Keep of
        true -> ignore;
        false -> ?ets_delete_table(T)
    end,
    Res.

get_initial_schema(SchemaStorage, Nodes) ->
    get_initial_schema(SchemaStorage, Nodes, []).

get_initial_schema(SchemaStorage, Nodes, Properties) ->	%
    UserProps = initial_schema_properties(Properties),
    Cs = #cstruct{name = schema,
		  record_name = schema,
		  attributes = [table, cstruct],
		  user_properties = UserProps},
    Cs2 =
	case SchemaStorage of
        ram_copies -> Cs#cstruct{ram_copies = Nodes};
        disc_copies -> Cs#cstruct{disc_copies = Nodes}
    end,
    cs2list(Cs2).

initial_schema_properties(Props0) ->
    DefaultProps = remove_duplicates(mnesia_monitor:get_env(schema)),
    Props = lists:foldl(
	      fun({K,V}, Acc) ->
		      lists:keystore(K, 1, Acc, {K,V})
	      end, DefaultProps, remove_duplicates(Props0)),
    initial_schema_properties_(Props).

initial_schema_properties_([{backend_types, Types}|Props]) ->
    lists:foreach(fun({Name, Module}) ->
			  verify_backend_type(Name, Module)
		  end, Types),
    [{mnesia_backend_types, Types}|initial_schema_properties_(Props)];
initial_schema_properties_([{index_plugins, Plugins}|Props]) ->
    lists:foreach(fun({Name, Module, Function}) ->
			  verify_index_plugin(Name, Module, Function)
		  end, Plugins),
    [{mnesia_index_plugins, Plugins}|initial_schema_properties_(Props)];
initial_schema_properties_([P|_Props]) ->
    mnesia:abort({bad_schema_property, P});
initial_schema_properties_([]) ->
    [].

remove_duplicates([{K,_} = H|T]) ->
    [H | remove_duplicates([X || {K1,_} = X <- T,
				 K1 =/= K])];
remove_duplicates([]) ->
    [].

read_cstructs_from_disc() ->
    %% Assumptions:
    %% - local schema lock in global
    %% - use_dir is true
    %% - Mnesia is not running
    %% - Ignore fallback

    Fname = mnesia_lib:tab2dat(schema),
    case mnesia_lib:exists(Fname) of
	true ->
	    Args = [{file, Fname},
		    {keypos, 2},
		    {repair, mnesia_monitor:get_env(auto_repair)},
		    {type, set}],
	    case dets:open_file(make_ref(), Args) of
		{ok, Tab} ->
                    ExtTypes = get_ext_types_disc(),
		    Fun = fun({_, _, List}) ->
				  {continue, list2cs(List, ExtTypes)}
			  end,
		    Cstructs = dets:traverse(Tab, Fun),
		    dets:close(Tab),
		    {ok, Cstructs};
		{error, Reason} ->
		    {error, Reason}
	    end;
	false ->
	    {error, "No schema file exists"}
    end.

%% We run a very special type of transactions when we
%% we want to manipulate the schema.

get_tid_ts_and_lock(Tab, Intent) ->
    TidTs = get(mnesia_activity_state),
    case TidTs of
	{_Mod, Tid, Ts} when is_record(Ts, tidstore)->
	    Store = Ts#tidstore.store,
	    case Intent of
		read -> mnesia_locker:rlock_table(Tid, Store, Tab);
		write -> mnesia_locker:wlock_table(Tid, Store, Tab);
		none -> ignore
	    end,
	    TidTs;
	_ ->
	    mnesia:abort(no_transaction)
    end.

schema_transaction(Fun) ->
    case get(mnesia_activity_state) of
	undefined ->
	    Args = [self(), Fun, whereis(mnesia_controller)],
	    Pid = spawn_link(?MODULE, schema_coordinator, Args),
	    receive
		{transaction_done, Res, Pid} -> Res;
		{'EXIT', Pid, R} -> {aborted, {transaction_crashed, R}}
	    end;
	_ ->
            {aborted, nested_transaction}
    end.

%% This process may dump the transaction log, and should
%% therefore not be run in an application process
%%
schema_coordinator(Client, _Fun, undefined) ->
    Res = {aborted, {node_not_running, node()}},
    Client ! {transaction_done, Res, self()},
    unlink(Client);

schema_coordinator(Client, Fun, Controller) when is_pid(Controller) ->
    %% Do not trap exit in order to automatically die
    %% when the controller dies

    link(Controller),
    unlink(Client),

    %% Fulfull the transaction even if the client dies
    Res = mnesia:transaction(Fun),
    Client ! {transaction_done, Res, self()},
    unlink(Controller),         % Avoids spurious exit message
    unlink(whereis(mnesia_tm)), % Avoids spurious exit message
    exit(normal).

%% The make* rotines return a list of ops, this function
%% inserts em all in the Store and maintains the local order
%% of ops.

insert_schema_ops({_Mod, _Tid, Ts}, SchemaIOps) ->
    do_insert_schema_ops(Ts#tidstore.store, SchemaIOps).

do_insert_schema_ops(Store, [Head | Tail]) ->
    ?ets_insert(Store, Head),
    do_insert_schema_ops(Store, Tail);
do_insert_schema_ops(_Store, []) ->
    ok.

api_list2cs(List) when is_list(List) ->
    Name = pick(unknown, name, List, must),
    Keys = check_keys(Name, List),
    check_duplicates(Name, Keys),
    list2cs(List);
api_list2cs(Other) ->
    mnesia:abort({badarg, Other}).

vsn_cs2list(Cs) ->
    cs2list(need_old_cstructs(), Cs).

cs2list(Cs) when is_record(Cs, cstruct) ->
    Tags = record_info(fields, cstruct),
    rec2list(Tags, Tags, 2, Cs);
cs2list(CreateList) when is_list(CreateList) ->
    CreateList;

cs2list(Cs) when element(1, Cs) == cstruct, tuple_size(Cs) == 20 ->
    Tags = [name,type,
	    ram_copies,disc_copies,disc_only_copies,external_copies,
	    load_order,access_mode,majority,index,snmp,local_content,
	    record_name,attributes,
	    user_properties,frag_properties,storage_properties,
	    cookie,version],
    rec2list(Tags, Tags, 2, Cs);
%% since vsn-4.6 (protocol 8.2 or older)
cs2list(Cs) when element(1, Cs) == cstruct, tuple_size(Cs) == 19 ->
    Tags = [name,type,ram_copies,disc_copies,disc_only_copies,
	    load_order,access_mode,majority,index,snmp,local_content,
	    record_name,attributes,
	    user_properties,frag_properties,storage_properties,
	    cookie,version],
    rec2list(Tags, Tags, 2, Cs).

cs2list(false, Cs) ->
    cs2list(Cs);
cs2list({8,3}, Cs) ->
    cs2list(Cs);
cs2list({8,Minor}, Cs) when Minor =:= 2; Minor =:= 1 ->
    Orig = record_info(fields, cstruct),
    Tags = [name,type,ram_copies,disc_copies,disc_only_copies,
	    load_order,access_mode,majority,index,snmp,local_content,
	    record_name,attributes,
	    user_properties,frag_properties,storage_properties,
	    cookie,version],
    CsList = rec2list(Tags, Orig, 2, Cs),
    case proplists:get_value(index, CsList, []) of
	[] -> CsList;
	NewFormat ->
	    OldFormat = [Pos || {Pos, _Pref} <- NewFormat],
	    lists:keyreplace(index, 1, CsList, {index, OldFormat})
    end.

rec2list([index | Tags], [index|Orig], Pos, Rec) ->
    Val = element(Pos, Rec),
    [{index, lists:map(
	       fun({_, _Type}=P) -> P;
		  (P) when is_integer(P); is_atom(P) -> {P, ordered}
	       end, Val)} | rec2list(Tags, Orig, Pos + 1, Rec)];
rec2list([external_copies | Tags], Orig0, Pos, Rec) ->
    Orig = case Orig0 of
	       [external_copies|Rest] -> Rest;
	       _ -> Orig0
	   end,
    Val = element(Pos, Rec),
    [{Alias, Ns} || {{Alias,_}, Ns} <- Val]
	++ rec2list(Tags, Orig, Pos+1, Rec);
rec2list([Tag | Tags], [Tag | Orig], Pos, Rec) ->
    Val = element(Pos, Rec),
    [{Tag, Val} | rec2list(Tags, Orig, Pos + 1, Rec)];
rec2list([], _, _Pos, _Rec) ->
    [];
rec2list(Tags, [_|Orig], Pos, Rec) ->
    rec2list(Tags, Orig, Pos+1, Rec).

normalize_cs(Cstructs, Node) ->
    %% backward-compatibility hack; normalize before returning
    case need_old_cstructs([Node]) of
	false ->
	    Cstructs;
	Version ->
	    %% some other format
	    [convert_cs(Version, Cs) || Cs <- Cstructs]
    end.

convert_cs(Version, Cs) ->
    Fields = [Value || {_, Value} <- cs2list(Version, Cs)],
    list_to_tuple([cstruct|Fields]).

list2cs(List) ->
    list2cs(List, get_ext_types()).

list2cs(List, ExtTypes) when is_list(List) ->
    Name = pick(unknown, name, List, must),
    Type = pick(Name, type, List, set),
    Rc0 = pick(Name, ram_copies, List, []),
    Dc = pick(Name, disc_copies, List, []),
    Doc = pick(Name, disc_only_copies, List, []),

    Ext = pick_external_copies(List, ExtTypes),
    Rc = case {Rc0, Dc, Doc, Ext} of
             {[], [], [], []} -> [node()];
             _ -> Rc0
         end,
    LC = pick(Name, local_content, List, false),
    RecName = pick(Name, record_name, List, Name),
    Attrs = pick(Name, attributes, List, [key, val]),
    Snmp = pick(Name, snmp, List, []),
    LoadOrder = pick(Name, load_order, List, 0),
    AccessMode = pick(Name, access_mode, List, read_write),
    Majority = pick(Name, majority, List, false),
    UserProps = pick(Name, user_properties, List, []),
    verify({alt, [nil, list]}, mnesia_lib:etype(UserProps),
	   {bad_type, Name, {user_properties, UserProps}}),
    Cookie = pick(Name, cookie, List, ?unique_cookie),
    Version = pick(Name, version, List, {{2, 0}, []}),
    Ix = pick(Name, index, List, []),
    verify({alt, [nil, list]}, mnesia_lib:etype(Ix),
	   {bad_type, Name, {index, [Ix]}}),
    Frag = pick(Name, frag_properties, List, []),
    verify({alt, [nil, list]}, mnesia_lib:etype(Frag),
	   {badarg, Name, {frag_properties, Frag}}),

    BEProps = pick(Name, storage_properties, List, []),
    verify({alt, [nil, list]}, mnesia_lib:etype(Ix),
	   {badarg, Name, {storage_properties, BEProps}}),
    CheckProp = fun(Opt, Opts) when is_atom(Opt) ->
			lists:member(Opt, Opts)
			    andalso mnesia:abort({badarg, Name, Opt});
		   (Tuple, Opts) when is_tuple(Tuple) ->
			lists:member(element(1,Tuple), Opts)
			    andalso mnesia:abort({badarg, Name, Tuple});
		   (What,_) ->
			mnesia:abort({badarg, Name, What})
		end,
    BadEtsOpts = [set, ordered_set, bag, duplicate_bag,
		  public, private, protected,
		  keypos, named_table],
    EtsOpts = proplists:get_value(ets, BEProps, []),
    is_list(EtsOpts) orelse mnesia:abort({badarg, Name, {ets, EtsOpts}}),
    [CheckProp(Prop, BadEtsOpts) || Prop <- EtsOpts],
    BadDetsOpts = [type, keypos, repair, access, file],
    DetsOpts = proplists:get_value(dets, BEProps, []),
    is_list(DetsOpts) orelse mnesia:abort({badarg, Name, {dets, DetsOpts}}),
    [CheckProp(Prop, BadDetsOpts) || Prop <- DetsOpts],

    case whereis(mnesia_controller) of
        undefined ->
            %% check_keys/2 cannot be executed when mnesia is not
            %% running, due to it not being possible to read what ext
            %% backends are loaded.
            %% this doesn't work - disabled for now:
            %%Keys = check_keys(Name, List, record_info(fields, cstruct)),
            %%check_duplicates(Name, Keys)
            ignore;
        Pid when is_pid(Pid) ->
            Keys = check_keys(Name, List),
            check_duplicates(Name, Keys)
    end,

    Cs0 = #cstruct{name = Name,
		   ram_copies = Rc,
		   disc_copies = Dc,
		   disc_only_copies = Doc,
		   external_copies = Ext,
		   type = Type,
		   index = Ix,
		   snmp = Snmp,
		   load_order = LoadOrder,
		   access_mode = AccessMode,
		   majority = Majority,
		   local_content = LC,
		   record_name = RecName,
		   attributes = Attrs,
		   user_properties = lists:sort(UserProps),
		   frag_properties = lists:sort(Frag),
                   storage_properties = lists:sort(BEProps),
		   cookie = Cookie,
		   version = Version},
    case Ix of
	[] -> Cs0;
	[_|_] ->
	    Ix2 = expand_index_attrs(Cs0),
	    Cs0#cstruct{index = Ix2}
    end;
list2cs(Other, _ExtTypes) ->
    mnesia:abort({badarg, Other}).

pick(Tab, Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        false  when Default == must ->
            mnesia:abort({badarg, Tab, "Missing key", Key, List});
        false ->
            Default;
        {value, {Key, Value}} ->
            Value;
	{value, BadArg} ->
	    mnesia:abort({bad_type, Tab, BadArg})
    end.

pick_external_copies(_List, []) ->
    [];
pick_external_copies(List, ExtTypes) ->
    lists:foldr(
      fun({K, Val}, Acc) ->
	      case lists:keyfind(K, 1, ExtTypes) of
		  false ->
		      Acc;
		  {_, Mod} ->
		      [{{K,Mod}, Val}|Acc]
	      end
      end, [], List).

expand_storage_type(S) when S==ram_copies;
			    S==disc_copies;
			    S==disc_only_copies ->
    S;
expand_storage_type(S) ->
    case lists:keyfind(S, 1, get_ext_types()) of
	false ->
	    mnesia:abort({bad_type, {storage_type, S}});
	{Alias, Mod} ->
	    {ext, Alias, Mod}
    end.

get_ext_types() ->
    get_schema_user_property(mnesia_backend_types).

get_index_plugins() ->
    get_schema_user_property(mnesia_index_plugins).

get_schema_user_property(Key) ->
    Tab = schema,
    %% Must work reliably both within transactions and outside of transactions
    Res = case get(mnesia_activity_state) of
	      undefined ->
		  dirty_read_table_property(Tab, Key);
	      _ ->
		  do_read_table_property(Tab, Key)
	  end,
    case Res of
	undefined ->
	    [];
	{_, Types} ->
	    Types
    end.

get_ext_types_disc() ->
    try get_ext_types_disc_()
    catch
	error:_ ->[]
    end.

get_ext_types_disc_() ->
    case mnesia_schema:remote_read_schema() of
        {ok, _, Prop} ->
            K1 = user_properties,
            case lists:keyfind(K1, 1, Prop) of
                {K1, UserProp} ->
                    K2 = mnesia_backend_types,
                    case lists:keyfind(K2, 1, UserProp) of
                        {K2, Types} ->
                            Types;
                        _ ->
                            []
                    end;
                _ ->
                    []
            end;
        _ ->
            []
    end.

%% Convert attribute name to integer if neccessary
attr_tab_to_pos(_Tab, Pos) when is_integer(Pos) ->
    Pos;
attr_tab_to_pos(Tab, Attr) ->
    attr_to_pos(Attr, val({Tab, attributes})).

%% Convert attribute name to integer if neccessary
attr_to_pos({_} = P, _) -> P;
attr_to_pos(Pos, _Attrs) when is_integer(Pos) ->
    Pos;
attr_to_pos(Attr, Attrs) when is_atom(Attr) ->
    attr_to_pos(Attr, Attrs, 2);
attr_to_pos(Attr, _) ->
    mnesia:abort({bad_type, Attr}).

attr_to_pos(Attr, [Attr | _Attrs], Pos) ->
    Pos;
attr_to_pos(Attr, [_ | Attrs], Pos) ->
    attr_to_pos(Attr, Attrs, Pos + 1);
attr_to_pos(Attr, _, _) ->
    mnesia:abort({bad_type, Attr}).

check_keys(Tab, Attrs) ->
    Types = [T || {T,_} <- get_ext_types()],
    check_keys(Tab, Attrs, Types ++ record_info(fields, cstruct)).

check_keys(Tab, [{Key, _Val} | Tail], Items) ->
    Key1 = if
               is_tuple(Key) ->
                   element(1, Key);
               true ->
                   Key
           end,
    case lists:member(Key1, Items) of
        true ->  [Key | check_keys(Tab, Tail, Items)];
        false -> mnesia:abort({badarg, Tab, Key})
    end;
check_keys(_, [], _) ->
    [];
check_keys(Tab, Arg, _) ->
    mnesia:abort({badarg, Tab, Arg}).

check_duplicates(Tab, Keys) ->
    case has_duplicates(Keys) of
        false -> ok;
        true -> mnesia:abort({badarg, Tab, "Duplicate keys", Keys})
    end.

has_duplicates([H | T]) ->
    case lists:member(H, T) of
        true -> true;
        false -> has_duplicates(T)
    end;
has_duplicates([]) ->
    false.

%% This is the only place where we check the validity of data

verify_cstruct(#cstruct{} = Cs) ->
    assert_correct_cstruct(Cs),
    Cs1 = verify_external_copies(
	    Cs#cstruct{index = expand_index_attrs(Cs)}),
    assert_correct_cstruct(Cs1),
    Cs1.

expand_index_attrs(#cstruct{index = Ix, attributes = Attrs,
			    name = Tab} = Cs) ->
    Prefered = prefered_index_types(Cs),
    expand_index_attrs(Ix, Tab, Attrs, Prefered).

expand_index_attrs(Ix, Tab, Attrs, Prefered) ->
    lists:map(fun(P) when is_integer(P); is_atom(P) ->
		      {attr_to_pos(P, Attrs), Prefered};
		 ({A} = P) when is_atom(A) ->
		      {P, Prefered};
		 ({P, Type}) ->
		      {attr_to_pos(P, Attrs), Type};
		 (_Other) ->
		      mnesia:abort({bad_type, Tab, {index, Ix}})
	      end, Ix).

prefered_index_types(#cstruct{external_copies = Ext}) ->
    ExtTypes = [mnesia_lib:semantics(S, index_types) ||
		   {S,Ns} <- Ext, Ns =/= []],
    case intersect_types(ExtTypes) of
	[] -> ordered;
	[Pref|_] -> Pref
    end.

intersect_types([]) ->
    [];
intersect_types([S1, S2|Rest]) ->
    intersect_types([S1 -- (S1 -- S2)|Rest]);
intersect_types([S]) ->
    S.

verify_external_copies(#cstruct{external_copies = []} = Cs) ->
    Cs;
verify_external_copies(#cstruct{name = Tab, external_copies = EC} = Cs) ->
    Bad = {bad_type, Tab, {external_copies, EC}},
    AllECNodes = lists:concat([Ns || {_, Ns} <- EC,
                                     is_list(Ns)]),
    verify(true, length(lists:usort(AllECNodes)) == length(AllECNodes), Bad),
    CsL = cs2list(Cs),
    CsL1 = lists:foldl(
	     fun({{Alias, Mod}, Ns} = _X, CsLx) ->
		     BadTab = fun(Why) ->
				      {Why, Tab, {{ext, Alias, Mod},Ns}}
			      end,
		     verify(atom, mnesia_lib:etype(Mod), BadTab),
		     verify(true, fun() ->
					  lists:all(fun is_atom/1, Ns)
				  end, BadTab),
		     check_semantics(Mod, Alias, BadTab, Cs),
		     try Mod:check_definition(Alias, Tab, Ns, CsLx) of
			 ok ->
			     CsLx;
			 {ok, CsLx1} ->
			     CsLx1;
			 {error, Reason} ->
			     mnesia:abort(BadTab(Reason))
		     catch
			 error:E ->
			     mnesia:abort(BadTab(E))
		     end;
		(_, CsLx) ->
		     CsLx
	     end, CsL, EC),
    list2cs(CsL1).

check_semantics(Mod, Alias, BadTab, #cstruct{type = Type}) ->
    Ext = {ext, Alias, Mod},
    case lists:member(mnesia_lib:semantics(Ext, storage), [ram_copies, disc_copies,
							   disc_only_copies]) of
	false -> mnesia:abort(BadTab(invalid_storage));
	true  -> ok
    end,
    case lists:member(Type, mnesia_lib:semantics(Ext, types)) of
	false -> mnesia:abort(BadTab(bad_type));
	true  -> ok
    end.

assert_correct_cstruct(Cs) when is_record(Cs, cstruct) ->
    verify_nodes(Cs),

    Tab = Cs#cstruct.name,
    verify(atom, mnesia_lib:etype(Tab), {bad_type, Tab}),
    Type = Cs#cstruct.type,
    verify(true, lists:member(Type, [set, bag, ordered_set]),
	   {bad_type, Tab, {type, Type}}),

    %% Currently ordered_set is not supported for disk_only_copies.
    if
 	Type == ordered_set, Cs#cstruct.disc_only_copies /= [] ->
	    mnesia:abort({bad_type, Tab, {not_supported, Type, disc_only_copies}});
	true ->
	    ok
    end,

    RecName = Cs#cstruct.record_name,
    verify(atom, mnesia_lib:etype(RecName),
	   {bad_type, Tab, {record_name, RecName}}),

    Attrs = Cs#cstruct.attributes,
    verify(list, mnesia_lib:etype(Attrs),
	   {bad_type, Tab, {attributes, Attrs}}),

    Arity = length(Attrs) + 1,
    verify(true, Arity > 2, {bad_type, Tab, {attributes, Attrs}}),

    lists:foldl(fun(Attr,_Other) when Attr == snmp ->
                        mnesia:abort({bad_type, Tab, {attributes, [Attr]}});
                   (Attr,Other) ->
                        verify(atom, mnesia_lib:etype(Attr),
                               {bad_type, Tab, {attributes, [Attr]}}),
                        verify(false, lists:member(Attr, Other),
                               {combine_error, Tab, {attributes, [Attr | Other]}}),
                        [Attr | Other]
                end,
                [],
                Attrs),

    Index = Cs#cstruct.index,

    verify({alt, [nil, list]}, mnesia_lib:etype(Index),
	   {bad_type, Tab, {index, Index}}),
    IxPlugins = get_index_plugins(),

    AllowIndexOnKey = check_if_allow_index_on_key(),
    IxFun =
	fun(Pos) ->
		verify(
		  true, fun() ->
				I = index_pos(Pos),
				case Pos of
				    {_, T} ->
					(T==bag orelse T==ordered)
					    andalso good_ix_pos(
						      I, AllowIndexOnKey,
						      Arity, IxPlugins);
				    _ ->
					good_ix_pos(Pos, AllowIndexOnKey,
						    Arity, IxPlugins)
				end
			end,
		  {bad_type, Tab, {index, [Pos]}})
	end,
    lists:foreach(IxFun, Index),

    LC = Cs#cstruct.local_content,
    verify({alt, [true, false]}, LC,
	   {bad_type, Tab, {local_content, LC}}),
    Access = Cs#cstruct.access_mode,
    verify({alt, [read_write, read_only]}, Access,
	   {bad_type, Tab, {access_mode, Access}}),
    Majority = Cs#cstruct.majority,
    verify({alt, [true, false]}, Majority,
	   {bad_type, Tab, {majority, Majority}}),
    case Majority of
	true ->
	    verify(false, LC,
		   {combine_error, Tab, [{local_content,true},{majority,true}]});
	false ->
	    ok
    end,
    Snmp = Cs#cstruct.snmp,
    verify(true, mnesia_snmp_hook:check_ustruct(Snmp),
	   {badarg, Tab, {snmp, Snmp}}),

    CheckProp = fun(Prop) when is_tuple(Prop), size(Prop) >= 1 -> ok;
		   (Prop) ->
			mnesia:abort({bad_type, Tab,
				      {user_properties, [Prop]}})
		end,
    lists:foreach(CheckProp, Cs#cstruct.user_properties),

    case Cs#cstruct.cookie of
	{{MegaSecs, Secs, MicroSecs}, _Node}
	when is_integer(MegaSecs), is_integer(Secs),
	     is_integer(MicroSecs), is_atom(node) ->
            ok;
        Cookie ->
            mnesia:abort({bad_type, Tab, {cookie, Cookie}})
    end,
    case Cs#cstruct.version of
        {{Major, Minor}, _Detail}
                when is_integer(Major), is_integer(Minor) ->
            ok;
        Version ->
            mnesia:abort({bad_type, Tab, {version, Version}})
    end.

good_ix_pos({_} = P, _, _, Plugins) ->
    lists:keymember(P, 1, Plugins);
good_ix_pos(I, true, Arity, _) when is_integer(I) ->
    I >= 0 andalso I =< Arity;
good_ix_pos(I, false, Arity, _) when is_integer(I) ->
    I > 2 andalso I =< Arity;
good_ix_pos(_, _, _, _) ->
    false.


check_if_allow_index_on_key() ->
    case mnesia_monitor:get_env(allow_index_on_key) of
	true ->
	    true;
	_ ->
	    false
    end.

verify_nodes(Cs) ->
    Tab = Cs#cstruct.name,
    Ram = Cs#cstruct.ram_copies,
    Disc = Cs#cstruct.disc_copies,
    DiscOnly = Cs#cstruct.disc_only_copies,
    Ext = lists:append([Ns || {_,Ns} <- Cs#cstruct.external_copies]),
    LoadOrder = Cs#cstruct.load_order,

    verify({alt, [nil, list]}, mnesia_lib:etype(Ram),
	   {bad_type, Tab, {ram_copies, Ram}}),
    verify({alt, [nil, list]}, mnesia_lib:etype(Disc),
	   {bad_type, Tab, {disc_copies, Disc}}),
    lists:foreach(
      fun({BE, Ns}) ->
	      verify({alt, [nil, list]}, mnesia_lib:etype(Ns),
		     {bad_type, Tab, {BE, Ns}}),
	      lists:foreach(fun(N) ->
				    verify(atom, mnesia_lib:etype(N),
					   {bad_type, Tab, {BE, Ns}})
			    end, Ns)
      end, Cs#cstruct.external_copies),
    case Tab of
	schema ->
	    verify([], DiscOnly, {bad_type, Tab, {disc_only_copies, DiscOnly}});
	_ ->
	    verify({alt, [nil, list]},
		   mnesia_lib:etype(DiscOnly),
		   {bad_type, Tab, {disc_only_copies, DiscOnly}})
    end,
    verify(integer, mnesia_lib:etype(LoadOrder),
	   {bad_type, Tab, {load_order, LoadOrder}}),

    Nodes = Ram ++ Disc ++ DiscOnly ++ Ext,
    verify(list, mnesia_lib:etype(Nodes),
	   {combine_error, Tab,
	    [{ram_copies, []}, {disc_copies, []},
	     {disc_only_copies, []}, {external_copies, []}]}),
    verify(false, has_duplicates(Nodes), {combine_error, Tab, Nodes}),
    AtomCheck = fun(N) ->
			verify(atom, mnesia_lib:etype(N), {bad_type, Tab, N})
		end,
    lists:foreach(AtomCheck, Nodes).

verify(Expected, Fun, Error) when is_function(Fun) ->
    do_verify(Expected, catch Fun(), Error);
verify(Expected, Actual, Error) ->
    do_verify(Expected, Actual, Error).

do_verify({alt, Values}, Value, Error) ->
    case lists:member(Value, Values) of
        true -> ok;
        false -> mnesia:abort(Error)
    end;
do_verify(Value, Value, _) ->
    ok;
do_verify(_Value, _, Error) ->
     mnesia:abort(Error).

ensure_writable(Tab) ->
    case val({Tab, where_to_write}) of
        [] -> mnesia:abort({read_only, Tab});
        _ -> ok
    end.

%% Ensure that all replicas on disk full nodes are active
ensure_active(Cs) ->
    ensure_active(Cs, active_replicas).

ensure_active(Cs, What) ->
    Tab = Cs#cstruct.name,
    W = {Tab, What},
    ensure_non_empty(W),
    Nodes = mnesia_lib:intersect(val({schema, disc_copies}),
                                 mnesia_lib:cs_to_nodes(Cs)),
    case Nodes -- val(W) of
        [] ->
            ok;
        Ns ->
            Expl = "All replicas on diskfull nodes are not active yet",
            case val({Tab, local_content}) of
                true ->
		    case rpc:multicall(Ns, ?MODULE, is_remote_member, [W]) of
			{Replies, []} ->
			    check_active(Replies, Expl, Tab);
			{_Replies, BadNs} ->
			    mnesia:abort({not_active, Expl, Tab, BadNs})
                    end;
                false ->
                    mnesia:abort({not_active, Expl, Tab, Ns})
            end
    end.

ensure_non_empty({Tab, Vhat}) ->
       case val({Tab, Vhat}) of
        [] -> mnesia:abort({no_exists, Tab});
        _ -> ok
    end.

ensure_not_active(Tab = schema, Node) ->
    Active = val({Tab, active_replicas}),
    case lists:member(Node, Active) of
	false when Active =/= [] ->
	    ok;
	false ->
	    mnesia:abort({no_exists, Tab});
	true ->
	    Expl = "Mnesia is running",
	    mnesia:abort({active, Expl, Node})
    end.

is_remote_member(Key) ->
    IsActive = lists:member(node(), val(Key)),
    {IsActive, node()}.

check_active([{true, _Node} | Replies], Expl, Tab) ->
    check_active(Replies, Expl, Tab);
check_active([{false, Node} | _Replies], Expl, Tab) ->
    mnesia:abort({not_active, Expl, Tab, [Node]});
check_active([{badrpc, Reason} | _Replies], Expl, Tab) ->
    mnesia:abort({not_active, Expl, Tab, Reason});
check_active([], _Expl, _Tab) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function for definining an external backend type

add_backend_type(Name, Module) ->
    case schema_transaction(fun() -> do_add_backend_type(Name, Module) end) of
	{atomic, NeedsInit} ->
	    case NeedsInit of
		true ->
		    Module:init_backend();
		false ->
		    ignore
	    end,
	    Module:add_aliases([Name]),
	    {atomic, ok};
	Other ->
	    Other
    end.

do_add_backend_type(Name, Module) ->
    verify_backend_type(Name, Module),
    Types = case do_read_table_property(schema, mnesia_backend_types) of
		undefined ->
		    [];
		{_, Ts} ->
		    case lists:keymember(Name, 1, Ts) of
			true ->
			    mnesia:abort({backend_type_already_exists, Name});
			false ->
			    Ts
		    end
	    end,
    ModuleRegistered = lists:keymember(Module, 2, Types),
    do_write_table_property(schema, {mnesia_backend_types,
				     [{Name, Module}|Types]}),
    ModuleRegistered.

delete_backend_type(Name) ->
    schema_transaction(fun() -> do_delete_backend_type(Name) end).

do_delete_backend_type(Name) ->
    case do_read_table_property(schema, mnesia_backend_types) of
	undefined ->
	    [];
	{_, Ts} ->
	    case lists:keyfind(Name, 1, Ts) of
		{_, Mod} ->
		    case using_backend_type(Name, Mod) of
			[_|_] = Tabs ->
			    mnesia:abort({backend_in_use, {Name, Tabs}});
			[] ->
			    do_write_table_property(
			      schema, {mnesia_backend_types,
				       lists:keydelete(Name, 1, Ts)})
		    end;
		false ->
		    mnesia:abort({no_such_backend, Name})
	    end
    end.

using_backend_type(Name, Mod) ->
    Ext = ets:select(mnesia_gvar,
		     [{ {{'$1',external_copies},'$2'}, [], [{{'$1','$2'}}] }]),
    Entry = {Name, Mod},
    [T || {T,C} <- Ext,
	  lists:keymember(Entry, 1, C)].

verify_backend_type(Name, Module) ->
    case legal_backend_name(Name) of
	false ->
	    mnesia:abort({bad_type, {backend_type,Name,Module}});
	true ->
	    ok
    end,
    ExpectedExports = mnesia_backend_type:behaviour_info(callbacks),
    Exports = try Module:module_info(exports)
              catch
                  error:_ ->
                      mnesia:abort({undef_backend, Module})
              end,
    case ExpectedExports -- Exports of
        [] ->
	    ok;
        _Other ->
	    io:fwrite(user, "Missing backend_type exports: ~p~n", [_Other]),
            mnesia:abort({bad_type, {backend_type,Name,Module}})
    end.

legal_backend_name(Name) ->
    is_atom(Name) andalso
                    (not lists:member(Name, record_info(fields, cstruct))).

%% Used e.g. by mnesia:system_info(backend_types).
backend_types() ->
    [ram_copies, disc_copies, disc_only_copies |
     [T || {T,_} <- get_ext_types()]].

add_index_plugin(Name, Module, Function) ->
    schema_transaction(
      fun() -> do_add_index_plugin(Name, Module, Function) end).

do_add_index_plugin(Name, Module, Function) ->
    verify_index_plugin(Name, Module, Function),
    Plugins = case do_read_table_property(schema, mnesia_index_plugins) of
		  undefined ->
		      [];
		  {_, Ps} ->
		      case lists:keymember(Name, 1, Ps) of
			  true ->
			      mnesia:abort({index_plugin_already_exists, Name});
			  false ->
			      Ps
		      end
	      end,
    do_write_table_property(schema, {mnesia_index_plugins,
				     [{Name, Module, Function}|Plugins]}).

delete_index_plugin(P) ->
    schema_transaction(
      fun() -> do_delete_index_plugin(P) end).

do_delete_index_plugin({A} = P) when is_atom(A) ->
    Plugins = get_index_plugins(),
    case lists:keyfind(P, 1, Plugins) of
	false ->
	    mnesia:abort({no_exists, {index_plugin, P}});
	_Found ->
	    case ets:select(mnesia_gvar,
			    [{ {{'$1',{index,{P,'_'}}},'_'},[],['$1']},
			     { {{'$1',{index,P}},'_'},[],['$1']}], 1) of
		{[_], _} ->
		    mnesia:abort({plugin_in_use, P});
		'$end_of_table' ->
		    do_write_table_property(
		      schema, {mnesia_index_plugins,
			       lists:keydelete(P, 1, Plugins)})
	    end
    end.

verify_index_plugin({A} = Name, Module, Function)
  when is_atom(A), is_atom(Module), is_atom(Function) ->
    case code:ensure_loaded(Module) of
	{error, nofile} ->
	    mnesia:abort({bad_type, {index_plugin,Name,Module,Function}});
	{module,_} ->
	    %% Index plugins are called as Module:Function(Tab, Pos, Obj)
	    case erlang:function_exported(Module, Function, 3) of
		true ->
		    ok;
		false ->
		    mnesia:abort(
		      {bad_type, {index_plugin,Name,Module,Function}})
	    end
    end;
verify_index_plugin(Name, Module, Function) ->
    mnesia:abort({bad_type, {index_plugin,Name,Module,Function}}).


%% Used e.g. by mnesia:system_info(backend_types).
index_plugins() ->
    get_index_plugins().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Here's the real interface function to create a table

create_table([_|_] = TabDef) ->
    schema_transaction(fun() -> do_multi_create_table(TabDef) end);
create_table(Arg) -> {aborted, {badarg, Arg}}.

%% And the corresponding do routines ....

do_multi_create_table(TabDef) ->
    get_tid_ts_and_lock(schema, write),
    ensure_writable(schema),
    do_create_table(TabDef),
    ok.

do_create_table(TabDef) when is_list(TabDef) ->
    Cs = api_list2cs(TabDef),
    case Cs#cstruct.frag_properties of
	[] ->
	    do_create_table_1(Cs);
	_Props ->
	    CsList = mnesia_frag:expand_cstruct(Cs),
	    lists:foreach(fun do_create_table_1/1, CsList)
    end.

do_create_table_1(Cs) ->
    {_Mod, _Tid, Ts} =  get_tid_ts_and_lock(schema, none),
    Store = Ts#tidstore.store,
    do_insert_schema_ops(Store, make_create_table(Cs)).

make_create_table(Cs) ->
    Tab = Cs#cstruct.name,
    verify(false, check_if_exists(Tab), {already_exists, Tab}),
    unsafe_make_create_table(Cs).

unsafe_make_create_table(Cs0) ->
    {_Mod, Tid, Ts} =  get_tid_ts_and_lock(schema, none),
    Cs = verify_cstruct(Cs0),
    Tab = Cs#cstruct.name,

    %% Check that we have all disc replica nodes running
    DiscNodes = Cs#cstruct.disc_copies ++ Cs#cstruct.disc_only_copies,
    RunningNodes = val({current, db_nodes}),
    CheckDisc = fun(N) ->
			verify(true, lists:member(N, RunningNodes),
			       {not_active, Tab, N})
		end,
    lists:foreach(CheckDisc, DiscNodes),

    Nodes = mnesia_lib:intersect(mnesia_lib:cs_to_nodes(Cs), RunningNodes),
    Store = Ts#tidstore.store,
    mnesia_locker:wlock_no_exist(Tid, Store, Tab, Nodes),
    [{op, create_table, vsn_cs2list(Cs)}].

check_if_exists(Tab) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    {_, _, Ts} = TidTs,
    Store = Ts#tidstore.store,
    ets:foldl(
      fun({op, create_table, [{name, T}|_]}, _Acc) when T==Tab ->
	      true;
	 ({op, delete_table, [{name,T}|_]}, _Acc) when T==Tab ->
	      false;
	 (_Other, Acc) ->
	      Acc
      end, existed_before(Tab), Store).

existed_before(Tab) ->
    ('EXIT' =/= element(1, ?catch_val({Tab,cstruct}))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delete a table entirely on all nodes.

delete_table(Tab) ->
    schema_transaction(fun() -> do_delete_table(Tab) end).

do_delete_table(schema) ->
    mnesia:abort({bad_type, schema});
do_delete_table(Tab) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    ensure_writable(schema),
    insert_schema_ops(TidTs, make_delete_table(Tab, whole_table)).

-dialyzer({no_improper_lists, make_delete_table/2}).
make_delete_table(Tab, Mode) ->
    case existed_before(Tab) of
	false ->
	    %% Deleting a table that was created in this very
	    %% schema transaction. Delete all ops in the Store
	    %% that operate on this table. We cannot run a normal
	    %% delete operation, since that involves checking live
	    %% nodes etc.
	    TidTs = get_tid_ts_and_lock(schema, write),
	    {_, _, Ts} = TidTs,
	    Store = Ts#tidstore.store,
	    Deleted = ets:select_delete(
			Store, [{{op,'$1',[{name,Tab}|'_']},
				 [{'or',
				   {'==','$1',create_table},
				   {'==','$1',delete_table}}], [true]}]),
	    ets:select_delete(
	      Store, [{{op,'$1',[{name,Tab}|'_'],'_'},
		       [{'or',
			 {'==','$1',write_table_property},
			 {'==','$1',delete_table_property}}],
		       [true]}]),
	    case Deleted of
		0 -> mnesia:abort({no_exists, Tab});
		_ -> []
	    end;
	true ->
	    case Mode of
		whole_table ->
		    case val({Tab, frag_properties}) of
			[] ->
			    [make_delete_table2(Tab)];
			_Props ->
			    %% Check if it is a base table
			    mnesia_frag:lookup_frag_hash(Tab),

			    %% Check for foreigners
			    F = mnesia_frag:lookup_foreigners(Tab),
			    verify([], F, {combine_error,
					   Tab, "Too many foreigners", F}),
			    [make_delete_table2(T) ||
				T <- mnesia_frag:frag_names(Tab)]
		    end;
		single_frag ->
		    [make_delete_table2(Tab)]
	    end
    end.

make_delete_table2(Tab) ->
    get_tid_ts_and_lock(Tab, write),
    Cs = val({Tab, cstruct}),
    ensure_active(Cs),
    ensure_writable(Tab),
    {op, delete_table, vsn_cs2list(Cs)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Change fragmentation of a table

change_table_frag(Tab, Change) ->
    schema_transaction(fun() -> do_change_table_frag(Tab, Change) end).

do_change_table_frag(Tab, Change) when is_atom(Tab), Tab /= schema ->
    TidTs = get_tid_ts_and_lock(schema, write),
    Ops = mnesia_frag:change_table_frag(Tab, Change),
    [insert_schema_ops(TidTs, Op) || Op <- Ops],
    ok;
do_change_table_frag(Tab, _Change) ->
    mnesia:abort({bad_type, Tab}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Clear a table

do_clear_table(schema) ->
    mnesia:abort({bad_type, schema});
do_clear_table(Tab) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, write),
    insert_schema_ops(TidTs, make_clear_table(Tab)).

make_clear_table(Tab) ->
    Cs = val({Tab, cstruct}),
    ensure_writable(Tab),
    [{op, clear_table, vsn_cs2list(Cs)}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_table_copy(Tab, Node, Storage) ->
    schema_transaction(fun() -> do_add_table_copy(Tab, Node, Storage) end).

do_add_table_copy(Tab, Node, Storage) when is_atom(Tab), is_atom(Node) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    insert_schema_ops(TidTs, make_add_table_copy(Tab, Node, Storage));
do_add_table_copy(Tab,Node,_) ->
    mnesia:abort({badarg, Tab, Node}).

make_add_table_copy(Tab, Node, Storage) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    Ns = mnesia_lib:cs_to_nodes(Cs),
    verify(false, lists:member(Node, Ns), {already_exists, Tab, Node}),
    Cs2 = verify_cstruct(new_cs(Cs, Node, Storage, add)),

    %% Check storage and if node is running
    IsRunning = lists:member(Node, val({current, db_nodes})),
    if
	Tab == schema ->
	    if
		Storage /= ram_copies ->
		    mnesia:abort({badarg, Tab, Storage});
		IsRunning == true ->
		    mnesia:abort({already_exists, Tab, Node});
		true ->
		    ignore
	    end;
	Storage == ram_copies ->
	    ignore;
	IsRunning == true ->
	    ignore;
	IsRunning == false ->
	    mnesia:abort({not_active, schema, Node})
    end,
    [{op, add_table_copy, Storage, Node, vsn_cs2list(Cs2)}].

del_table_copy(Tab, Node) ->
    schema_transaction(fun() -> do_del_table_copy(Tab, Node) end).

do_del_table_copy(Tab, Node) when is_atom(Node)  ->
    TidTs = get_tid_ts_and_lock(schema, write),
%%    get_tid_ts_and_lock(Tab, write),
    insert_schema_ops(TidTs, make_del_table_copy(Tab, Node));
do_del_table_copy(Tab, Node) ->
    mnesia:abort({badarg, Tab, Node}).

make_del_table_copy(Tab, Node) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    Storage = mnesia_lib:schema_cs_to_storage_type(Node, Cs),
    Cs2 = new_cs(Cs, Node, Storage, del),
    case mnesia_lib:cs_to_nodes(Cs2) of
        [] when Tab == schema ->
            mnesia:abort({combine_error, Tab, "Last replica"});
        [] ->
	    ensure_active(Cs),
            dbg_out("Last replica deleted in table ~p~n",  [Tab]),
            make_delete_table(Tab,  whole_table);
        _ when Tab == schema ->
	    %% ensure_active(Cs2),
	    ensure_not_active(Tab, Node),
            Cs3 = verify_cstruct(Cs2),
	    Ops = remove_node_from_tabs(val({schema, tables}), Node),
	    [{op, del_table_copy, ram_copies, Node, vsn_cs2list(Cs3)} | Ops];
        _ ->
	    ensure_active(Cs),
            Cs3 = verify_cstruct(Cs2),
            get_tid_ts_and_lock(Tab, write),
            [{op, del_table_copy, Storage, Node, vsn_cs2list(Cs3)}]
    end.

remove_node_from_tabs([], _Node) ->
    [];
remove_node_from_tabs([schema|Rest], Node) ->
    remove_node_from_tabs(Rest, Node);
remove_node_from_tabs([Tab|Rest], Node) ->
    {Cs, IsFragModified} =
	mnesia_frag:remove_node(Node, incr_version(val({Tab, cstruct}))),
    case mnesia_lib:schema_cs_to_storage_type(Node, Cs)  of
	unknown ->
	    case IsFragModified of
		true ->
		    [{op, change_table_frag, {del_node, Node}, vsn_cs2list(Cs)} |
		     remove_node_from_tabs(Rest, Node)];
		false ->
		    remove_node_from_tabs(Rest, Node)
	    end;
	Storage ->
	    Cs2 = new_cs(Cs, Node, Storage, del),
	    case mnesia_lib:cs_to_nodes(Cs2) of
		[] ->
		    [{op, delete_table, vsn_cs2list(Cs)} |
		     remove_node_from_tabs(Rest, Node)];
		_Ns ->
		    Cs3 = verify_cstruct(Cs2),
		    get_tid_ts_and_lock(Tab, write),
		    [{op, del_table_copy, ram_copies, Node, vsn_cs2list(Cs3)}|
		     remove_node_from_tabs(Rest, Node)]
	    end
    end.

new_cs(Cs, Node, ram_copies, add) ->
    Cs#cstruct{ram_copies = opt_add(Node, Cs#cstruct.ram_copies)};
new_cs(Cs, Node, disc_copies, add) ->
    Cs#cstruct{disc_copies = opt_add(Node, Cs#cstruct.disc_copies)};
new_cs(Cs, Node, disc_only_copies, add) ->
    Cs#cstruct{disc_only_copies = opt_add(Node, Cs#cstruct.disc_only_copies)};
new_cs(Cs, Node, ram_copies, del) ->
    Cs#cstruct{ram_copies = lists:delete(Node , Cs#cstruct.ram_copies)};
new_cs(Cs, Node, disc_copies, del) ->
    Cs#cstruct{disc_copies = lists:delete(Node , Cs#cstruct.disc_copies)};
new_cs(Cs, Node, disc_only_copies, del) ->
    Cs#cstruct{disc_only_copies =
               lists:delete(Node , Cs#cstruct.disc_only_copies)};
new_cs(#cstruct{external_copies = ExtCps} = Cs, Node, Storage0, Op) ->
    Storage = case Storage0 of
		  {ext, Alias, _} -> Alias;
		  Alias -> Alias
	      end,
    ExtTypes = get_ext_types(),
    case lists:keyfind(Storage, 1, ExtTypes) of
	false ->
	    mnesia:abort({badarg, Cs#cstruct.name, Storage});
	{_, Mod} ->
	    Key = {Storage, Mod},
	    case {lists:keymember(Key, 1, ExtCps), Op} of
		{false, del} ->
		    Cs;
		{false, add} ->
		    Cs#cstruct{external_copies = [{Key, [Node]}|ExtCps]};
		{true, _} ->
		    F = fun({K, Ns}) when K == Key ->
				case Op of
				    del -> {K, lists:delete(Node, Ns)};
				    add -> {K, opt_add(Node, Ns)}
				end;
			   (X) ->
				X
			end,
		    Cs#cstruct{external_copies = lists:map(F, ExtCps)}
	    end
    end.


opt_add(N, L) -> [N | lists:delete(N, L)].

move_table(Tab, FromNode, ToNode) ->
    schema_transaction(fun() -> do_move_table(Tab, FromNode, ToNode) end).

do_move_table(schema, _FromNode, _ToNode) ->
    mnesia:abort({bad_type, schema});
do_move_table(Tab, FromNode, ToNode) when is_atom(FromNode), is_atom(ToNode) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    AnyOld = lists:any(fun(Node) -> mnesia_monitor:needs_protocol_conversion(Node) end,
		       [ToNode|val({Tab, where_to_write})]),
    if AnyOld -> ignore;  %% Leads to deadlock on old nodes
       true -> get_tid_ts_and_lock(Tab, write)
    end,
    insert_schema_ops(TidTs, make_move_table(Tab, FromNode, ToNode));
do_move_table(Tab, FromNode, ToNode) ->
    mnesia:abort({badarg, Tab, FromNode, ToNode}).

make_move_table(Tab, FromNode, ToNode) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    Ns = mnesia_lib:cs_to_nodes(Cs),
    verify(false, lists:member(ToNode, Ns), {already_exists, Tab, ToNode}),
    verify(true, lists:member(FromNode, val({Tab, where_to_write})),
           {not_active, Tab, FromNode}),
    verify(false, val({Tab,local_content}),
           {"Cannot move table with local content", Tab}),
    ensure_active(Cs),
    Running = val({current, db_nodes}),
    Storage = mnesia_lib:schema_cs_to_storage_type(FromNode, Cs),
    verify(true, lists:member(ToNode, Running), {not_active, schema, ToNode}),

    Cs2 = new_cs(Cs, ToNode, Storage, add),
    Cs3 = verify_cstruct(new_cs(Cs2, FromNode, Storage, del)),
    [{op, add_table_copy, Storage, ToNode, vsn_cs2list(Cs2)},
     {op, sync_trans},
     {op, del_table_copy, Storage, FromNode, vsn_cs2list(Cs3)}].

%% end of functions to add and delete nodes to tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%

change_table_copy_type(Tab, Node, ToS) ->
    schema_transaction(fun() -> do_change_table_copy_type(Tab, Node, ToS) end).

do_change_table_copy_type(Tab, Node, ToS) when is_atom(Node) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, write), % ensure global sync
    %% get_tid_ts_and_lock(Tab, read),
    insert_schema_ops(TidTs, make_change_table_copy_type(Tab, Node, ToS));
do_change_table_copy_type(Tab, Node, _ToS) ->
    mnesia:abort({badarg, Tab, Node}).

make_change_table_copy_type(Tab, Node, unknown) ->
    make_del_table_copy(Tab, Node);
make_change_table_copy_type(Tab, Node, ToS) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    FromS = mnesia_lib:storage_type_at_node(Node, Tab),

    ToSExp = expand_storage_type(ToS),

    case compare_storage_type(false, FromS, ToSExp) of
	{same, _} ->
	    mnesia:abort({already_exists, Tab, Node, ToSExp});
	{diff, _} ->
	    ignore;
	incompatible ->
	    ensure_active(Cs)
    end,

    Cs2 = new_cs(Cs, Node, FromS, del),
    Cs3 = verify_cstruct(new_cs(Cs2, Node, ToS, add)),
    [{op, change_table_copy_type, Node, FromS, ToSExp, vsn_cs2list(Cs3)}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% change index functions ....
%% Pos is already added by 1 in both of these functions

add_table_index(Tab, Pos) ->
    schema_transaction(fun() -> do_add_table_index(Tab, Pos) end).

do_add_table_index(schema, _Attr) ->
    mnesia:abort({bad_type, schema});
do_add_table_index(Tab, Attr) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, read),
    Pos = attr_tab_to_pos(Tab, Attr),
    insert_schema_ops(TidTs, make_add_table_index(Tab, Pos)).

make_add_table_index(Tab, Pos) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    Ix = Cs#cstruct.index,
    verify(false, lists:keymember(index_pos(Pos), 1, Ix),
	   {already_exists, Tab, Pos}),
    Ix2 = lists:sort([Pos | Ix]),
    Cs2 = verify_cstruct(Cs#cstruct{index = Ix2}),
    NewPosInfo = lists:keyfind(Pos, 1, Cs2#cstruct.index),
    [{op, add_index, NewPosInfo, vsn_cs2list(Cs2)}].

del_table_index(Tab, Pos) ->
    schema_transaction(fun() -> do_del_table_index(Tab, Pos) end).

do_del_table_index(schema, _Attr) ->
    mnesia:abort({bad_type, schema});
do_del_table_index(Tab, Attr) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, read),
    Pos = attr_tab_to_pos(Tab, Attr),
    insert_schema_ops(TidTs, make_del_table_index(Tab, Pos)).

make_del_table_index(Tab, Pos) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    Ix = Cs#cstruct.index,
    verify(true, lists:keymember(Pos, 1, Ix), {no_exists, Tab, Pos}),
    Cs2 = verify_cstruct(Cs#cstruct{index = lists:keydelete(Pos, 1, Ix)}),
    [{op, del_index, Pos, vsn_cs2list(Cs2)}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_snmp(Tab, Ustruct) ->
    schema_transaction(fun() -> do_add_snmp(Tab, Ustruct) end).

do_add_snmp(schema, _Ustruct) ->
    mnesia:abort({bad_type, schema});
do_add_snmp(Tab, Ustruct) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, read),
    insert_schema_ops(TidTs, make_add_snmp(Tab, Ustruct)).

make_add_snmp(Tab, Ustruct) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    verify([], Cs#cstruct.snmp, {already_exists, Tab, snmp}),
    Error = {badarg, Tab, snmp, Ustruct},
    verify(true, mnesia_snmp_hook:check_ustruct(Ustruct), Error),
    Cs2 = verify_cstruct(Cs#cstruct{snmp = Ustruct}),
    [{op, add_snmp, Ustruct, vsn_cs2list(Cs2)}].

del_snmp(Tab) ->
    schema_transaction(fun() -> do_del_snmp(Tab) end).

do_del_snmp(schema) ->
    mnesia:abort({bad_type, schema});
do_del_snmp(Tab) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, read),
    insert_schema_ops(TidTs, make_del_snmp(Tab)).

make_del_snmp(Tab) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    Cs2 = verify_cstruct(Cs#cstruct{snmp = []}),
    [{op, del_snmp, vsn_cs2list(Cs2)}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%

transform_table(Tab, Fun, NewAttrs, NewRecName)
  when is_function(Fun), is_list(NewAttrs), is_atom(NewRecName) ->
    schema_transaction(fun() -> do_transform_table(Tab, Fun, NewAttrs, NewRecName) end);

transform_table(Tab, ignore, NewAttrs, NewRecName)
  when is_list(NewAttrs), is_atom(NewRecName) ->
    schema_transaction(fun() -> do_transform_table(Tab, ignore, NewAttrs, NewRecName) end);

transform_table(Tab, Fun, NewAttrs, NewRecName) ->
    {aborted,{bad_type, Tab, Fun, NewAttrs, NewRecName}}.

do_transform_table(schema, _Fun, _NewAttrs, _NewRecName) ->
    mnesia:abort({bad_type, schema});
do_transform_table(Tab, Fun, NewAttrs, NewRecName) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, write),
    insert_schema_ops(TidTs, make_transform(Tab, Fun, NewAttrs, NewRecName)).

make_transform(Tab, Fun, NewAttrs, NewRecName) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    ensure_writable(Tab),
    case Cs#cstruct.index of
	[] ->
	    Cs2 = verify_cstruct(
                    Cs#cstruct{attributes = NewAttrs,
                               record_name = NewRecName}),
	    [{op, transform, Fun, vsn_cs2list(Cs2)}];
	PosList ->
	    DelIdx = fun({Pos,_}, Ncs) ->
			     Ix = Ncs#cstruct.index,
			     Ix2 = lists:keydelete(Pos, 1, Ix),
			     Ncs1 = Ncs#cstruct{index = Ix2},
			     Op = {op, del_index, Pos, vsn_cs2list(Ncs1)},
			     {Op, Ncs1}
		     end,
	    AddIdx = fun({_,_} = Pos, Ncs) ->
			     Ix = Ncs#cstruct.index,
			     Ix2 = lists:sort([Pos | Ix]),
			     Ncs1 = Ncs#cstruct{index = Ix2},
			     Op = {op, add_index, Pos, vsn_cs2list(Ncs1)},
			     {Op, Ncs1}
		     end,
            {DelOps, Cs1} = lists:mapfoldl(DelIdx, Cs, PosList),
	    Cs2 = Cs1#cstruct{attributes = NewAttrs, record_name = NewRecName},
            {AddOps, Cs3} = lists:mapfoldl(AddIdx, Cs2, PosList),
	    _ = verify_cstruct(Cs3), % just a sanity check
	    lists:flatten([DelOps, {op, transform, Fun, vsn_cs2list(Cs2)},
			   AddOps])
    end.

index_pos({Pos,_}) -> Pos;
index_pos(Pos) when is_integer(Pos) -> Pos;
index_pos({P} = Pos) when is_atom(P) -> Pos.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%

change_table_access_mode(Tab, Mode) ->
    schema_transaction(fun() -> do_change_table_access_mode(Tab, Mode) end).

do_change_table_access_mode(Tab, Mode) ->
    {_Mod, Tid, Ts} = get_tid_ts_and_lock(schema, write),
    Store = Ts#tidstore.store,
    mnesia_locker:wlock_no_exist(Tid, Store, schema, val({schema, active_replicas})),
    mnesia_locker:wlock_no_exist(Tid, Store, Tab, val({Tab, active_replicas})),
    do_insert_schema_ops(Store, make_change_table_access_mode(Tab, Mode)).

make_change_table_access_mode(Tab, Mode) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    OldMode = Cs#cstruct.access_mode,
    verify(false, OldMode ==  Mode, {already_exists, Tab, Mode}),
    Cs2 = verify_cstruct(Cs#cstruct{access_mode = Mode}),
    [{op, change_table_access_mode, vsn_cs2list(Cs2), OldMode, Mode}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_table_load_order(Tab, LoadOrder) ->
    schema_transaction(fun() -> do_change_table_load_order(Tab, LoadOrder) end).

do_change_table_load_order(schema, _LoadOrder) ->
    mnesia:abort({bad_type, schema});
do_change_table_load_order(Tab, LoadOrder) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, none),
    insert_schema_ops(TidTs, make_change_table_load_order(Tab, LoadOrder)).

make_change_table_load_order(Tab, LoadOrder) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    OldLoadOrder = Cs#cstruct.load_order,
    Cs2 = verify_cstruct(Cs#cstruct{load_order = LoadOrder}),
    [{op, change_table_load_order, vsn_cs2list(Cs2), OldLoadOrder, LoadOrder}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_table_majority(Tab, Majority) when is_boolean(Majority) ->
    schema_transaction(fun() -> do_change_table_majority(Tab, Majority) end).

do_change_table_majority(schema, _Majority) ->
    mnesia:abort({bad_type, schema});
do_change_table_majority(Tab, Majority) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    get_tid_ts_and_lock(Tab, none),
    insert_schema_ops(TidTs, make_change_table_majority(Tab, Majority)).

make_change_table_majority(Tab, Majority) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    OldMajority = Cs#cstruct.majority,
    Cs2 = Cs#cstruct{majority = Majority},
    FragOps = case lists:keyfind(base_table, 1, Cs#cstruct.frag_properties) of
		  {_, Tab} ->
		      FragNames = mnesia_frag:frag_names(Tab) -- [Tab],
		      lists:map(
			fun(T) ->
				get_tid_ts_and_lock(Tab, none),
				CsT = incr_version(val({T, cstruct})),
				ensure_active(CsT),
				CsT2 = CsT#cstruct{majority = Majority},
				verify_cstruct(CsT2),
				{op, change_table_majority, vsn_cs2list(CsT2),
				 OldMajority, Majority}
			end, FragNames);
		  false    -> [];
		  {_, _}   -> mnesia:abort({bad_type, Tab})
	      end,
    verify_cstruct(Cs2),
    [{op, change_table_majority, vsn_cs2list(Cs2), OldMajority, Majority} | FragOps].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_table_property(Tab, Prop) when is_tuple(Prop), size(Prop) >= 1 ->
    schema_transaction(fun() -> do_write_table_property(Tab, Prop) end);
write_table_property(Tab, Prop) ->
    {aborted, {bad_type, Tab, Prop}}.

do_write_table_property(Tab, Prop) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    {_, _, Ts} = TidTs,
    Store = Ts#tidstore.store,
    case change_prop_in_existing_op(Tab, Prop, write_property, Store) of
	true ->
	    dbg_out("change_prop_in_existing_op"
		    "(~p,~p,write_property,Store) -> true~n",
		    [Tab,Prop]),
	    %% we have merged the table prop into the create_table op
	    ok;
	false ->
	    dbg_out("change_prop_in_existing_op"
		    "(~p,~p,write_property,Store) -> false~n",
		    [Tab,Prop]),
	    %% this must be an existing table
	    get_tid_ts_and_lock(Tab, none),
	    insert_schema_ops(TidTs, make_write_table_properties(Tab, [Prop]))
    end.

make_write_table_properties(Tab, Props) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    make_write_table_properties(Tab, Props, Cs).

make_write_table_properties(Tab, [Prop | Props], Cs) ->
    OldProps = Cs#cstruct.user_properties,
    PropKey = element(1, Prop),
    DelProps = lists:keydelete(PropKey, 1, OldProps),
    MergedProps = lists:merge(DelProps, [Prop]),
    Cs2 = verify_cstruct(Cs#cstruct{user_properties = MergedProps}),
    [{op, write_property, vsn_cs2list(Cs2), Prop} |
     make_write_table_properties(Tab, Props, Cs2)];
make_write_table_properties(_Tab, [], _Cs) ->
    [].

change_prop_in_existing_op(Tab, Prop, How, Store) ->
    Ops = ets:match_object(Store, '_'),
    case update_existing_op(Ops, Tab, Prop, How, []) of
	{true, Ops1} ->
	    ets:match_delete(Store, '_'),
	    [ets:insert(Store, Op) || Op <- Ops1],
	    true;
	false ->
	    false
    end.

update_existing_op([{op, Op, L = [{name,Tab}|_], _OldProp}|Ops],
		   Tab, Prop, How, Acc) when Op == write_property;
					     Op == delete_property ->
    %% Apparently, mnesia_dumper doesn't care about OldProp here -- just L,
    %% so we will throw away OldProp (not that it matters...) and insert Prop.
    %% as element 3.
    L1 = insert_prop(Prop, L, How),
    NewOp = {op, How, L1, Prop},
    {true, lists:reverse(Acc) ++ [NewOp|Ops]};
update_existing_op([Op = {op, create_table, L}|Ops], Tab, Prop, How, Acc) ->
    case lists:keysearch(name, 1, L) of
	{value, {_, Tab}} ->
	    %% Tab is being created here -- insert Prop into L
	    L1 = insert_prop(Prop, L, How),
	    {true, lists:reverse(Acc) ++ [{op, create_table, L1}|Ops]};
	_ ->
	    update_existing_op(Ops, Tab, Prop, How, [Op|Acc])
    end;
update_existing_op([Op|Ops], Tab, Prop, How, Acc) ->
    update_existing_op(Ops, Tab, Prop, How, [Op|Acc]);
update_existing_op([], _, _, _, _) ->
    false.

do_read_table_property(Tab, Key) ->
    TidTs = get_tid_ts_and_lock(schema, read),
    {_, _, Ts} = TidTs,
    Store = Ts#tidstore.store,
    Props = ets:foldl(
	      fun({op, announce_im_running,_,Opts,_,_}, _Acc) when Tab==schema ->
		      find_props(Opts);
		 ({op, create_table, [{name, T}|Opts]}, _Acc)
		    when T==Tab ->
		      find_props(Opts);
		 ({op, Op, [{name,T}|Opts], _Prop}, _Acc)
		 when T==Tab, Op==write_property;
		      T==Tab, Op==delete_property ->
		      find_props(Opts);
		 ({op, delete_table, [{name,T}|_]}, _Acc)
		 when T==Tab ->
		      [];
		 (_Other, Acc) ->
		      Acc
	      end, undefined, Store),
    case Props of
        undefined ->
            get_tid_ts_and_lock(Tab, read),
	    dirty_read_table_property(Tab, Key);
        _ when is_list(Props) ->
            case lists:keyfind(Key, 1, Props) of
		false ->
		    undefined;
		Other ->
		    Other
            end
    end.

dirty_read_table_property(Tab, Key) ->
    try ets:lookup_element(mnesia_gvar, {Tab,user_property,Key}, 2)
    catch
	error:_ ->
	    undefined
    end.


%% perhaps a misnomer. How could also be delete_property... never mind.
%% Returns the modified L.
insert_prop(Prop, L, How) ->
    Prev = find_props(L),
    MergedProps = merge_with_previous(How, Prop, Prev),
    replace_props(L, MergedProps).

find_props([{user_properties, P}|_]) -> P;
find_props([_H|T]) -> find_props(T).
%% we shouldn't reach []

replace_props([{user_properties, _}|T], P) -> [{user_properties, P}|T];
replace_props([H|T], P) -> [H|replace_props(T, P)].
%% again, we shouldn't reach []

merge_with_previous(write_property, Prop, Prev) ->
    Key = element(1, Prop),
    Prev1 = lists:keydelete(Key, 1, Prev),
    lists:sort([Prop|Prev1]);
merge_with_previous(delete_property, PropKey, Prev) ->
    lists:keydelete(PropKey, 1, Prev).

delete_table_property(Tab, PropKey) ->
    schema_transaction(fun() -> do_delete_table_property(Tab, PropKey) end).

do_delete_table_property(Tab, PropKey) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    {_, _, Ts} = TidTs,
    Store = Ts#tidstore.store,
    case change_prop_in_existing_op(Tab, PropKey, delete_property, Store) of
	true ->
	    dbg_out("change_prop_in_existing_op"
		    "(~p,~p,delete_property,Store) -> true~n",
		    [Tab,PropKey]),
	    %% we have merged the table prop into the create_table op
	    ok;
	false ->
	    dbg_out("change_prop_in_existing_op"
		    "(~p,~p,delete_property,Store) -> false~n",
		    [Tab,PropKey]),
	    %% this must be an existing table
	    get_tid_ts_and_lock(Tab, none),
	    insert_schema_ops(TidTs,
			      make_delete_table_properties(Tab, [PropKey]))
    end.

make_delete_table_properties(Tab, PropKeys) ->
    ensure_writable(schema),
    Cs = incr_version(val({Tab, cstruct})),
    ensure_active(Cs),
    make_delete_table_properties(Tab, PropKeys, Cs).

make_delete_table_properties(Tab, [PropKey | PropKeys], Cs) ->
    OldProps = Cs#cstruct.user_properties,
    Props = lists:keydelete(PropKey, 1, OldProps),
    Cs2 = verify_cstruct(Cs#cstruct{user_properties = Props}),
    [{op, delete_property, vsn_cs2list(Cs2), PropKey} |
     make_delete_table_properties(Tab, PropKeys, Cs2)];
make_delete_table_properties(_Tab, [], _Cs) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ensure that the transaction can be committed even
%% if the node crashes and Mnesia is restarted
prepare_commit(Tid, Commit, WaitFor) ->
    case Commit#commit.schema_ops of
	[] ->
	    {false, Commit, optional};
	OrigOps ->
	    {Modified, Ops, DumperMode} =
		prepare_ops(Tid, OrigOps, WaitFor, false, [], optional),
	    InitBy = schema_prepare,
	    GoodRes = {Modified,
		       Commit#commit{schema_ops = lists:reverse(Ops)},
		       DumperMode},
	    case DumperMode of
		optional ->
		    dbg_out("Transaction log dump skipped (~p): ~w~n",
			    [DumperMode, InitBy]);
		mandatory ->
		    case mnesia_controller:sync_dump_log(InitBy) of
			dumped ->
			    GoodRes;
			{error, Reason} ->
			    mnesia:abort(Reason)
		    end
	    end,
	    case Ops of
		[] ->
		    ignore;
		_ ->
		    %% We need to grab a dumper lock here, the log may not
		    %% be dumped by others, during the schema commit phase.
		    mnesia_controller:wait_for_schema_commit_lock()
	    end,
	    GoodRes
    end.

prepare_ops(Tid, [Op | Ops], WaitFor, Changed, Acc, DumperMode) ->
    case prepare_op(Tid, Op, WaitFor) of
        {true, mandatory} ->
	    prepare_ops(Tid, Ops, WaitFor, Changed, [Op | Acc], mandatory);
        {true, optional} ->
	    prepare_ops(Tid, Ops, WaitFor, Changed, [Op | Acc], DumperMode);
        {true, Ops2, mandatory} ->
	    prepare_ops(Tid, Ops, WaitFor, true, Ops2 ++ Acc, mandatory);
        {true, Ops2, optional} ->
	    prepare_ops(Tid, Ops, WaitFor, true, Ops2 ++ Acc, DumperMode);
	{false, optional} ->
	    prepare_ops(Tid, Ops, WaitFor, true, Acc, DumperMode)
    end;
prepare_ops(_Tid, [], _WaitFor, Changed, Acc, DumperMode) ->
    {Changed, Acc, DumperMode}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prepare for commit
%% returns true if Op should be included, i.e. unmodified
%%         {true, Operation} if NewRecs should be included, i.e. modified
%%         false if Op should NOT be included, i.e. modified
%%
prepare_op(_Tid, {op, rec, unknown, Rec}, _WaitFor) ->
    {{Tab, Key}, Items, _Op} = Rec,
    case val({Tab, storage_type}) of
        unknown ->
            {false, optional};
        Storage ->
            mnesia_tm:prepare_snmp(Tab, Key, Items), % May exit
            {true, [{op, rec, Storage, Rec}], optional}
    end;

prepare_op(_Tid, {op, announce_im_running, Node, SchemaDef, Running, RemoteRunning}, _WaitFor) ->
    SchemaCs = list2cs(SchemaDef),
    if
	Node == node() -> %% Announce has already run on local node
	    ignore;       %% from do_merge_schema
	true ->
	    %% If a node has restarted it may still linger in db_nodes,
	    %% but have been removed from recover_nodes
	    Current  = mnesia_lib:intersect(val({current,db_nodes}), [node()|val(recover_nodes)]),
	    NewNodes = mnesia_lib:uniq(Running++RemoteRunning) -- Current,
	    mnesia_lib:set(prepare_op, {announce_im_running,NewNodes}),
	    announce_im_running(NewNodes, SchemaCs)
    end,
    {false, optional};

prepare_op(_Tid, {op, sync_trans}, {part, CoordPid}) ->
    CoordPid ! {sync_trans, self()},
    receive
	{sync_trans, CoordPid} ->
	    {false, optional};
	{mnesia_down, _Node} = Else ->
	    mnesia_lib:verbose("sync_op terminated due to ~p~n", [Else]),
	    mnesia:abort(Else);
	{'EXIT', _, _} = Else ->
	    mnesia_lib:verbose("sync_op terminated due to ~p~n", [Else]),
	    mnesia:abort(Else)
    end;

prepare_op(_Tid, {op, sync_trans}, {coord, Nodes}) ->
    case receive_sync(Nodes, []) of
	{abort, Reason} ->
	    mnesia_lib:verbose("sync_op terminated due to ~p~n", [Reason]),
	    mnesia:abort(Reason);
	Pids ->
	    [Pid ! {sync_trans, self()} || Pid <- Pids],
	    {false, optional}
    end;
prepare_op(Tid, {op, create_table, TabDef}, _WaitFor) ->
    Cs = list2cs(TabDef),
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    UseDir = mnesia_monitor:use_dir(),
    Tab = Cs#cstruct.name,
    case Storage of
        disc_copies when UseDir == false ->
	    UseDirReason = {bad_type, Tab, Storage, node()},
            mnesia:abort(UseDirReason);
        disc_only_copies when UseDir == false ->
	    UseDirReason = {bad_type, Tab, Storage, node()},
            mnesia:abort(UseDirReason);
	ram_copies ->
	    mnesia_lib:set({Tab, create_table},true),
	    create_ram_table(Tab, Cs),
	    insert_cstruct(Tid, Cs, false),
	    {true, optional};
	disc_copies ->
	    mnesia_lib:set({Tab, create_table},true),
	    create_ram_table(Tab, Cs),
	    create_disc_table(Tab),
	    insert_cstruct(Tid, Cs, false),
	    {true, optional};
	disc_only_copies ->
	    mnesia_lib:set({Tab, create_table},true),
	    create_disc_only_table(Tab,Cs),
	    insert_cstruct(Tid, Cs, false),
	    {true, optional};
	{ext, Alias, Mod} ->
	    mnesia_lib:set({Tab, create_table},true),
            create_external_table(Alias, Tab, Mod, Cs),
	    insert_cstruct(Tid, Cs, false),
	    {true, optional};
        unknown -> %% No replica on this node
	    mnesia_lib:set({Tab, create_table},true),
	    insert_cstruct(Tid, Cs, false),
            {true, optional}
    end;

prepare_op(Tid, {op, add_table_copy, Storage, Node, TabDef}, _WaitFor) ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,

    if
	Tab == schema ->
	    {true, optional};

	Node == node() ->
	    case mnesia_lib:val({schema, storage_type}) of
		ram_copies when Storage /= ram_copies ->
		    Error = {combine_error, Tab, "has no disc", Node},
		    mnesia:abort(Error);
		_  ->
		    ok
	    end,
	    %% Tables are created by mnesia_loader get_network code
	    insert_cstruct(Tid, Cs, true),
	    case mnesia_controller:get_network_copy(Tid, Tab, Cs) of
		{loaded, ok} ->
		    {true, optional};
		{not_loaded, ErrReason} ->
		    Reason = {system_limit, Tab, {Node, ErrReason}},
		    mnesia:abort(Reason)
	    end;
	Node /= node() ->
	    %% Verify that ram table not has been dumped to disc
	    if
		Storage /= ram_copies ->
		    case mnesia_lib:schema_cs_to_storage_type(node(), Cs) of
			ram_copies ->
			    Dat = mnesia_lib:tab2dcd(Tab),
			    case mnesia_lib:exists(Dat) of
				true ->
				    mnesia:abort({combine_error, Tab, Storage,
						  "Table dumped to disc", node()});
				false ->
				    ok
			    end;
			_ ->
			    ok
		    end;
		true ->
		    ok
	    end,
	    insert_cstruct(Tid, Cs, true),
	    {true, optional}
    end;

prepare_op(_Tid, {op, del_table_copy, _Storage, Node, TabDef}, _WaitFor) ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,
    set_where_to_read(Tab, Node, Cs),
    {true, optional};

prepare_op(_Tid, {op, change_table_copy_type,  N, FromS, ToS, TabDef}, _WaitFor)
  when N == node() ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,

    NotActive = mnesia_lib:not_active_here(Tab),

    if Tab =/= schema ->
	    check_if_disc_required(FromS, ToS);
       true ->
	    ok
    end,

    if
	NotActive == true ->
	    mnesia:abort({not_active, Tab, node()});

	Tab == schema ->
	    case {FromS, ToS} of
		{ram_copies, disc_copies} ->
		    case mnesia:system_info(schema_location) of
			opt_disc ->
			    ignore;
			_ ->
			    mnesia:abort({combine_error,  Tab, node(),
					  "schema_location must be opt_disc"})
		    end,
		    Dir = mnesia_lib:dir(),
		    case opt_create_dir(true, Dir) of
			ok ->
			    purge_dir(Dir, []),
			    mnesia_log:purge_all_logs(),
			    set(use_dir, true),
			    mnesia_log:init(),
			    Ns = val({current, db_nodes}), %mnesia_lib:running_nodes(),
			    F = fun(U) -> mnesia_recover:log_mnesia_up(U) end,
			    lists:foreach(F, Ns),

			    mnesia_dumper:raw_named_dump_table(Tab, dmp),
			    mnesia_checkpoint:tm_change_table_copy_type(Tab, FromS, ToS);
			{error, Reason} ->
			    mnesia:abort(Reason)
		    end;
		{disc_copies, ram_copies} ->
		    Ltabs = val({schema, local_tables}) -- [schema],
		    Dtabs = [L || L <- Ltabs,
				  val({L, storage_type}) /= ram_copies],
		    verify([], Dtabs, {"Disc resident tables", Dtabs, N});
		_ ->
		    mnesia:abort({combine_error, Tab, ToS})
	    end;

	element(1,FromS) == ext; element(1,ToS) == ext ->
	    if ToS == ram_copies ->
		    create_ram_table(Tab, Cs);
	       true ->
		    ok
	    end,
            mnesia_dumper:dump_to_logfile(FromS, Tab),
            mnesia_checkpoint:tm_change_table_copy_type(Tab, FromS, ToS);

	FromS == ram_copies ->
	    case mnesia_monitor:use_dir() of
		true ->
		    Dat = mnesia_lib:tab2dcd(Tab),
		    case mnesia_lib:exists(Dat) of
			true ->
			    mnesia:abort({combine_error, Tab, node(),
					  "Table dump exists"});
			false ->
			    case ToS of
				disc_copies ->
				    mnesia_log:ets2dcd(Tab, dmp);
				disc_only_copies ->
				    mnesia_dumper:raw_named_dump_table(Tab, dmp)
			    end,
			    mnesia_checkpoint:tm_change_table_copy_type(Tab,
                                                                        FromS,
                                                                        ToS)
		    end;
		false ->
		    mnesia:abort({has_no_disc, node()})
	    end;

	FromS == disc_copies, ToS == disc_only_copies ->
	    mnesia_dumper:raw_named_dump_table(Tab, dmp);

	FromS == disc_only_copies ->
	    Type = Cs#cstruct.type,
	    create_ram_table(Tab, Cs),
	    Datname = mnesia_lib:tab2dat(Tab),
	    Repair = mnesia_monitor:get_env(auto_repair),
	    case mnesia_lib:dets_to_ets(Tab, Tab, Datname, Type, Repair, no) of
		loaded -> ok;
		Reason ->
		    Err = "Failed to copy disc data to ram",
		    mnesia:abort({system_limit, Tab, {Err,Reason}})
	    end;

	true ->
	    ignore
    end,
    {true, mandatory};

prepare_op(_Tid, {op, change_table_copy_type,  N, _FromS, _ToS, _TabDef}, _WaitFor)
  when N /= node() ->
    {true, mandatory};

prepare_op(_Tid, {op, delete_table, _TabDef}, _WaitFor) ->
    {true, mandatory};

prepare_op(_Tid, {op, dump_table, unknown, TabDef}, _WaitFor) ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,
    case lists:member(node(), Cs#cstruct.ram_copies) of
        true ->
	    case mnesia_monitor:use_dir() of
		true ->
		    mnesia_log:ets2dcd(Tab, dmp),
		    Size = mnesia:table_info(Tab, size),
		    {true, [{op, dump_table, Size, TabDef}], optional};
		false ->
		    mnesia:abort({has_no_disc, node()})
	    end;
        false ->
            {false, optional}
    end;

prepare_op(_Tid, {op, add_snmp, Ustruct, TabDef}, _WaitFor) ->
    Cs = list2cs(TabDef),
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
        unknown ->
            {true, optional};
        Storage ->
            Tab = Cs#cstruct.name,
            Stab = mnesia_snmp_hook:create_table(Ustruct, Tab, Storage),
            mnesia_lib:set({Tab, {index, snmp}}, Stab),
            {true, optional}
    end;

prepare_op(_Tid, {op, transform, ignore, _TabDef}, _WaitFor) ->
    {true, mandatory};   %% Apply schema changes only.
prepare_op(_Tid, {op, transform, Fun, TabDef}, _WaitFor) ->
    Cs = list2cs(TabDef),
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
        unknown ->
            {true, mandatory};
        Storage ->
            Tab = Cs#cstruct.name,
            RecName = Cs#cstruct.record_name,
	    Type = Cs#cstruct.type,
            NewArity = length(Cs#cstruct.attributes) + 1,
	    mnesia_lib:db_fixtable(Storage, Tab, true),
            Key = mnesia_lib:db_first(Tab),
	    Op = {op, transform, Fun, TabDef},
            try transform_objs(Fun, Tab, RecName, Key,
			       NewArity, Storage, Type, [Op]) of
                Objs ->
		    mnesia_lib:db_fixtable(Storage, Tab, false),
                    {true, Objs, mandatory}
	    catch _:Reason ->
		    mnesia_lib:db_fixtable(Storage, Tab, false),
		    mnesia_lib:important("Transform function failed: '~p' in '~p'",
					 [Reason, erlang:get_stacktrace()]),
                    exit({"Bad transform function", Tab, Fun, node(), Reason})
            end
    end;

prepare_op(_Tid, {op, merge_schema, TabDef}, _WaitFor) ->
    Cs = list2cs(TabDef),
    case verify_merge(Cs) of
	ok  ->
	    {true, optional};
	Error ->
	    verbose("Merge_Schema ~p failed on ~p: ~p~n", [_Tid,node(),Error]),
	    mnesia:abort({bad_commit, Error})
    end;
prepare_op(_Tid, _Op, _WaitFor) ->
    {true, optional}.

check_if_disc_required(FromS, ToS) ->
    FromSem = mnesia_lib:semantics(FromS, storage),
    ToSem = mnesia_lib:semantics(ToS, storage),
    case {FromSem, ToSem} of
	{ram_copies, _} when ToSem == disc_copies;
			     ToSem == disc_only_copies ->
	    case mnesia_monitor:use_dir() of
		true ->
		    ok;
		false ->
		    mnesia:abort({has_no_disc, node()})
	    end;
	_ ->
	    ok
    end.

create_ram_table(Tab, #cstruct{type=Type, storage_properties=Props}) ->
    EtsOpts = proplists:get_value(ets, Props, []),
    Args = [{keypos, 2}, public, named_table, Type | EtsOpts],
    case mnesia_monitor:unsafe_mktab(Tab, Args) of
	Tab ->
	    ok;
	{error,Reason} ->
	    Err = "Failed to create ets table",
	    mnesia:abort({system_limit, Tab, {Err,Reason}})
    end.

create_disc_table(Tab) ->
    File = mnesia_lib:tab2dcd(Tab),
    file:delete(File),
    FArg = [{file, File}, {name, {mnesia,create}},
	    {repair, false}, {mode, read_write}],
    case mnesia_monitor:open_log(FArg) of
	{ok,Log} ->
	    mnesia_monitor:unsafe_close_log(Log),
	    ok;
	{error,Reason} ->
	    Err = "Failed to create disc table",
	    mnesia:abort({system_limit, Tab, {Err,Reason}})
    end.
create_disc_only_table(Tab, #cstruct{type=Type, storage_properties=Props}) ->
    File = mnesia_lib:tab2dat(Tab),
    file:delete(File),
    DetsOpts = proplists:get_value(dets, Props, []),
    Args = [{file, mnesia_lib:tab2dat(Tab)},
	    {type, mnesia_lib:disk_type(Tab, Type)},
	    {keypos, 2},
	    {repair, mnesia_monitor:get_env(auto_repair)}
	    | DetsOpts],
    case mnesia_monitor:unsafe_open_dets(Tab, Args) of
	{ok, _} ->
	    ok;
	{error,Reason} ->
	    Err = "Failed to create disc table",
	    mnesia:abort({system_limit, Tab, {Err,Reason}})
    end.

create_external_table(Alias, Tab, Mod, Cs) ->
    case mnesia_monitor:unsafe_create_external(Tab, Alias, Mod, Cs) of
	ok ->
	    ok;
	{error,Reason} ->
	    Err = "Failed to create external table",
	    mnesia:abort({system_limit, Tab, {Err,Reason}})
    end.

receive_sync([], Pids) ->
    Pids;
receive_sync(Nodes, Pids) ->
    receive
	{sync_trans, Pid} ->
	    Node = node(Pid),
	    receive_sync(lists:delete(Node, Nodes), [Pid | Pids]);
	Else ->
	    {abort, Else}
    end.

set_where_to_read(Tab, Node, Cs) ->
    case mnesia_lib:val({Tab, where_to_read}) of
	Node ->
	    case Cs#cstruct.local_content of
		true ->
		    ok;
		false ->
		    mnesia_lib:set_remote_where_to_read(Tab, [Node]),
		    ok
	    end;
	_ ->
	    ok
    end.

%% Build up the list in reverse order.
transform_objs(_Fun, _Tab, _RT, '$end_of_table', _NewArity, _Storage, _Type, Acc) ->
    Acc;
transform_objs(Fun, Tab, RecName, Key, A, Storage, Type, Acc) ->
    Objs = mnesia_lib:db_get(Tab, Key),
    NextKey = mnesia_lib:db_next_key(Tab, Key),
    Oid = {Tab, Key},
    NewObjs = {Ws, Ds} = transform_obj(Tab, RecName, Key, Fun, Objs, A, Type, [], []),
    if
	NewObjs == {[], []} ->
	    transform_objs(Fun, Tab, RecName, NextKey, A, Storage, Type, Acc);
	Type == bag ->
	    transform_objs(Fun, Tab, RecName, NextKey, A, Storage, Type,
			   [{op, rec, Storage, {Oid, Ws, write}},
			    {op, rec, Storage, {Oid, [Oid], delete}} | Acc]);
	Ds == [] ->
	    %% Type is set or ordered_set, no need to delete the record first
	    transform_objs(Fun, Tab, RecName, NextKey, A, Storage, Type,
			   [{op, rec, Storage, {Oid, Ws, write}} | Acc]);
	Ws == [] ->
	    transform_objs(Fun, Tab, RecName, NextKey, A, Storage, Type,
			   [{op, rec, Storage, {Oid, Ds, write}} | Acc]);
	true ->
	    transform_objs(Fun, Tab, RecName, NextKey, A, Storage, Type,
			   [{op, rec, Storage, {Oid, Ws, write}},
			    {op, rec, Storage, {Oid, Ds, delete}} | Acc])
    end.

transform_obj(Tab, RecName, Key, Fun, [Obj|Rest], NewArity, Type, Ws, Ds) ->
    NewObj = Fun(Obj),
    if
        size(NewObj) /= NewArity ->
            exit({"Bad arity", Obj, NewObj});
	NewObj == Obj ->
	    transform_obj(Tab, RecName, Key, Fun, Rest, NewArity, Type, Ws, Ds);
        RecName == element(1, NewObj), Key == element(2, NewObj) ->
            transform_obj(Tab, RecName, Key, Fun, Rest, NewArity,
			  Type, [NewObj | Ws], Ds);
	NewObj == delete ->
	    case Type of
		bag -> %% Just don't write that object
		   transform_obj(Tab, RecName, Key, Fun, Rest,
				 NewArity, Type, Ws, Ds);
		_ ->
		    transform_obj(Tab, RecName, Key, Fun, Rest, NewArity,
				  Type, Ws, [NewObj | Ds])
	    end;
        true ->
            exit({"Bad key or Record Name", Obj, NewObj})
    end;
transform_obj(_Tab, _RecName, _Key, _Fun, [], _NewArity, _Type, Ws, Ds) ->
    {lists:reverse(Ws), lists:reverse(Ds)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Undo prepare of commit
undo_prepare_commit(Tid, Commit) ->
    case Commit#commit.schema_ops of
	[] ->
	    ignore;
	Ops ->
	    %% Catch to allow failure mnesia_controller may not be started
	    ?SAFE(mnesia_controller:release_schema_commit_lock()),
	    undo_prepare_ops(Tid, Ops)
    end,
    Commit.

%% Undo in reverse order
undo_prepare_ops(Tid, [Op | Ops]) ->
    case element(1, Op) of
	TheOp when TheOp /= op, TheOp /= restore_op ->
	    undo_prepare_ops(Tid, Ops);
	_ ->
	    undo_prepare_ops(Tid, Ops),
	    undo_prepare_op(Tid, Op)
    end;
undo_prepare_ops(_Tid, []) ->
    [].

undo_prepare_op(_Tid, {op, announce_im_running, _Node, _, _Running, _RemoteRunning}) ->
    case ?catch_val(prepare_op) of
	{announce_im_running, New} ->
            unannounce_im_running(New);
	_Else ->
	    ok
    end;

undo_prepare_op(_Tid, {op, sync_trans}) ->
    ok;

undo_prepare_op(Tid, {op, create_table, TabDef}) ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,
    mnesia_lib:unset({Tab, create_table}),
    delete_cstruct(Tid, Cs),
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
	unknown ->
	    ok;
	ram_copies ->
	    ram_delete_table(Tab, ram_copies);
	disc_copies ->
	    ram_delete_table(Tab, disc_copies),
	    DcdFile = mnesia_lib:tab2dcd(Tab),
	    %%	    disc_delete_table(Tab, Storage),
	    file:delete(DcdFile);
	disc_only_copies ->
	    mnesia_monitor:unsafe_close_dets(Tab),
	    Dat = mnesia_lib:tab2dat(Tab),
	    %%	    disc_delete_table(Tab, Storage),
	    file:delete(Dat);
        {ext, Alias, Mod} ->
	    Mod:close_table(Alias, Tab),
            Mod:delete_table(Alias, Tab)
    end;

undo_prepare_op(Tid, {op, add_table_copy, Storage, Node, TabDef}) ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,
    if
	Tab == schema ->
	    true; % Nothing to prepare
	Node == node() ->
	    mnesia_checkpoint:tm_del_copy(Tab, Node),
	    mnesia_controller:unannounce_add_table_copy(Tab, Node),
	    if
		Storage == disc_only_copies; Tab == schema ->
		    mnesia_monitor:close_dets(Tab),
		    file:delete(mnesia_lib:tab2dat(Tab));
		true ->
		    file:delete(mnesia_lib:tab2dcd(Tab))
	    end,
	    ram_delete_table(Tab, Storage),
	    Cs2 = new_cs(Cs, Node, Storage, del),
	    insert_cstruct(Tid, Cs2, true); % Don't care about the version
	Node /= node() ->
	    mnesia_controller:unannounce_add_table_copy(Tab, Node),
	    Cs2 = new_cs(Cs, Node, Storage, del),
	    insert_cstruct(Tid, Cs2, true) % Don't care about the version
    end;

undo_prepare_op(_Tid, {op, del_table_copy, _, Node, TabDef}) ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,
    if node() =:= Node ->
	    mnesia_lib:set({Tab, where_to_read}, Node);
       true ->
	    case mnesia_lib:val({Tab, where_to_read}) of
		nowhere ->
		    mnesia_lib:set_remote_where_to_read(Tab);
		_ ->
		    ignore
	    end
    end;

undo_prepare_op(_Tid, {op, change_table_copy_type, N, FromS, ToS, TabDef})
        when N == node() ->
    Cs = list2cs(TabDef),
    Tab = Cs#cstruct.name,
    mnesia_checkpoint:tm_change_table_copy_type(Tab, ToS, FromS),
    Dmp = mnesia_lib:tab2dmp(Tab),

    case {FromS, ToS} of
        {ram_copies, disc_copies} when Tab == schema ->
            file:delete(Dmp),
            mnesia_log:purge_some_logs(),
	    set(use_dir, false);
	{ram_copies, disc_copies} ->
	    file:delete(Dmp);
	{ram_copies, disc_only_copies} ->
	    file:delete(Dmp);
	{disc_only_copies, _} ->
	    ram_delete_table(Tab, ram_copies);
	_ ->
	    ignore
    end;

undo_prepare_op(_Tid, {op, dump_table, _Size, TabDef}) ->
    Cs = list2cs(TabDef),
    case lists:member(node(), Cs#cstruct.ram_copies) of
	true ->
	    Tab = Cs#cstruct.name,
	    Dmp = mnesia_lib:tab2dmp(Tab),
	    file:delete(Dmp);
	false ->
	    ignore
    end;

undo_prepare_op(_Tid, {op, add_snmp, _Ustruct, TabDef}) ->
    Cs = list2cs(TabDef),
    case mnesia_lib:cs_to_storage_type(node(), Cs) of
	unknown ->
	    true;
	_Storage ->
	    Tab = Cs#cstruct.name,
	    case ?catch_val({Tab, {index, snmp}}) of
		{'EXIT',_} ->
		    ignore;
		Stab ->
		    mnesia_snmp_hook:delete_table(Tab, Stab),
		    mnesia_lib:unset({Tab, {index, snmp}})
	    end
    end;

undo_prepare_op(_Tid, _Op) ->
    ignore.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ram_delete_table(Tab, Storage) ->
    case Storage of
	unknown ->
	    ignore;
        {ext, _, _} ->
            ignore;
	disc_only_copies ->
	    ignore;
	_Else ->
	    %% delete possible index files and data .....
	    %% Got to catch this since if no info has been set in the
	    %% mnesia_gvar it will crash
	    ?CATCH(mnesia_index:del_transient(Tab, Storage)),
	    case ?catch_val({Tab, {index, snmp}}) of
		{'EXIT', _} ->
		    ignore;
		Etab ->
		    ?SAFE(mnesia_snmp_hook:delete_table(Tab, Etab))
	    end,
	    ?SAFE(?ets_delete_table(Tab))
    end.

purge_dir(Dir, KeepFiles) ->
    Suffixes = known_suffixes(),
    purge_dir(Dir, KeepFiles, Suffixes).

purge_dir(Dir, KeepFiles, Suffixes) ->
    case dir_exists(Dir) of
	true ->
	    {ok, AllFiles} = file:list_dir(Dir),
	    purge_known_files(AllFiles, KeepFiles, Dir, Suffixes);
	false ->
	    ok
    end.

purge_tmp_files() ->
    case mnesia_monitor:use_dir() of
	true ->
	    Dir = mnesia_lib:dir(),
	    KeepFiles = [],
	    Exists = mnesia_lib:exists(mnesia_lib:tab2dat(schema)),
	    case Exists of
		true ->
		    Suffixes = tmp_suffixes(),
		    purge_dir(Dir, KeepFiles, Suffixes);
		false ->
		    %% Interrupted change of storage type
		    %% for schema table
		    Suffixes = known_suffixes(),
		    purge_dir(Dir, KeepFiles, Suffixes),
		    mnesia_lib:set(use_dir, false)
		end;

	false ->
	    ok
    end.

purge_known_files([File | Tail], KeepFiles, Dir, Suffixes) ->
    case lists:member(File, KeepFiles) of
	true ->
	    ignore;
	false ->
	    case has_known_suffix(File, Suffixes, false) of
		false ->
		    ignore;
		true ->
		    AbsFile = filename:join([Dir, File]),
                    delete_recursive(AbsFile)
            end
    end,
    purge_known_files(Tail, KeepFiles, Dir, Suffixes);
purge_known_files([], _KeepFiles, _Dir, _Suffixes) ->
    ok.

%% Removes a directory or file recursively
delete_recursive(Path) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, Names} = file:list_dir(Path),
            lists:foreach(fun(Name) ->
                                  delete_recursive(filename:join(Path, Name))
                          end,
                          Names),
            file:del_dir(Path);
        false ->
            file:delete(Path)
    end.

has_known_suffix(_File, _Suffixes, true) ->
    true;
has_known_suffix(File, [Suffix | Tail], false) ->
    has_known_suffix(File, Tail, lists:suffix(Suffix, File));
has_known_suffix(_File, [], Bool) ->
    Bool.

known_suffixes() -> known_suffixes(get_ext_types_disc()).

known_suffixes(Ext) -> real_suffixes(Ext) ++ tmp_suffixes(Ext).

real_suffixes(Ext) ->  [".DAT", ".LOG", ".BUP", ".DCL", ".DCD"] ++ ext_real_suffixes(Ext).

tmp_suffixes() -> tmp_suffixes(get_ext_types_disc()).

tmp_suffixes(Ext) -> [".TMP", ".BUPTMP", ".RET", ".DMP", "."] ++ ext_tmp_suffixes(Ext).

ext_real_suffixes(Ext) ->
    try lists:foldl(fun(Mod, Acc) -> Acc++Mod:real_suffixes() end, [],
		    [M || {_,M} <- Ext])
    catch
        error:E ->
            verbose("Cant find real ext suffixes (~p)~n", [E]),
            []
    end.

ext_tmp_suffixes(Ext) ->
    try lists:foldl(fun(Mod, Acc) -> Acc++Mod:tmp_suffixes() end, [],
		    [M || {_,M} <- Ext])
    catch
        error:E ->
            verbose("Cant find tmp ext suffixes (~p)~n", [E]),
            []
    end.

info() ->
    Tabs = lists:sort(val({schema, tables})),
    lists:foreach(fun(T) -> info(T) end, Tabs),
    ok.

info(Tab) ->
    Props = get_table_properties(Tab),
    io:format("-- Properties for ~w table --- ~n",[Tab]),
    info2(Tab, Props).
info2(Tab, [{cstruct, _V} | Tail]) -> % Ignore cstruct
    info2(Tab, Tail);
info2(Tab, [{frag_hash, _V} | Tail]) -> % Ignore frag_hash
    info2(Tab, Tail);
info2(Tab, [{P, V} | Tail]) ->
    io:format("~-20w -> ~p~n",[P,V]),
    info2(Tab, Tail);
info2(_, []) ->
    io:format("~n", []).

get_table_properties(Tab) ->
    try mnesia_lib:db_match_object(ram_copies, mnesia_gvar, {{Tab, '_'}, '_'}) of
	RawGvar ->
	    case [{Item, Val} || {{_Tab, Item}, Val} <- RawGvar] of
		[] ->
		    [];
		Gvar ->
		    Size = {size, mnesia:table_info(Tab, size)},
		    Memory = {memory, mnesia:table_info(Tab, memory)},
		    Master = {master_nodes, mnesia:table_info(Tab, master_nodes)},
		    lists:sort([Size, Memory, Master | Gvar])
	    end
    catch error:_ ->
	    mnesia:abort({no_exists, Tab, all})
    end.

%%%%%%%%%%% RESTORE %%%%%%%%%%%

-record(r, {iter = schema,
	    module,
	    table_options = [],
	    default_op = clear_tables,
	    tables = [],
	    opaque,
	    insert_op = error_fun,
	    recs = error_recs
	   }).

restore(Opaque) ->
    restore(Opaque, [], mnesia_monitor:get_env(backup_module)).
restore(Opaque, Args) when is_list(Args) ->
    restore(Opaque, Args, mnesia_monitor:get_env(backup_module));
restore(_Opaque, BadArg) ->
    {aborted, {badarg, BadArg}}.
restore(Opaque, Args, Module) when is_list(Args), is_atom(Module) ->
    InitR = #r{opaque = Opaque, module = Module},
    try lists:foldl(fun check_restore_arg/2, InitR, Args) of
	R when is_record(R, r) ->
	    case mnesia_bup:read_schema(R#r.module, Opaque) of
		{error, Reason} ->
		    {aborted, Reason};
		BupSchema ->
		    schema_transaction(fun() -> do_restore(R, BupSchema) end)
	    end
    catch exit:Reason ->
	    {aborted, Reason}
    end;
restore(_Opaque, Args, Module) ->
    {aborted, {badarg, Args, Module}}.

check_restore_arg({module, Mod}, R) when is_atom(Mod) ->
    R#r{module = Mod};

check_restore_arg({clear_tables, List}, R) when is_list(List) ->
    case lists:member(schema, List) of
	false ->
	    TableList = [{Tab, clear_tables} || Tab <- List],
	    R#r{table_options = R#r.table_options ++ TableList};
	true ->
	    exit({badarg, {clear_tables, schema}})
    end;
check_restore_arg({recreate_tables, List}, R) when is_list(List) ->
    case lists:member(schema, List) of
	false ->
	    TableList = [{Tab, recreate_tables} || Tab <- List],
	    R#r{table_options = R#r.table_options ++ TableList};
	true ->
	    exit({badarg, {recreate_tables, schema}})
    end;
check_restore_arg({keep_tables, List}, R) when is_list(List) ->
    TableList = [{Tab, keep_tables} || Tab <- List],
    R#r{table_options = R#r.table_options ++ TableList};
check_restore_arg({skip_tables, List}, R) when is_list(List) ->
    TableList = [{Tab, skip_tables} || Tab <- List],
    R#r{table_options = R#r.table_options ++ TableList};
check_restore_arg({default_op, Op}, R) ->
    case Op of
	clear_tables -> ok;
	recreate_tables -> ok;
	keep_tables -> ok;
	skip_tables -> ok;
	Else ->
	    exit({badarg, {bad_default_op, Else}})
    end,
    R#r{default_op = Op};

check_restore_arg(BadArg,_) ->
    exit({badarg, BadArg}).

do_restore(R, BupSchema) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    R2 = restore_schema(BupSchema, R),
    insert_schema_ops(TidTs, [{restore_op, R2}]),
    [element(1, TabStruct) || TabStruct <- R2#r.tables].

arrange_restore(R, Fun, Recs) ->
    R2 = R#r{insert_op = Fun, recs = Recs},
    case mnesia_bup:iterate(R#r.module, fun restore_items/5, R#r.opaque, R2) of
	{ok, R3} -> R3#r.recs;
	{error, Reason} -> mnesia:abort(Reason)
    end.

restore_items([Rec | Recs], Header, Schema, Ext, R) ->
    Tab = element(1, Rec),
    case lists:keysearch(Tab, 1, R#r.tables) of
	{value, {Tab, Where0, Snmp, RecName}} ->
	    Where = case Where0 of
			undefined ->
			    val({Tab, where_to_commit});
			_ ->
			    Where0
		    end,
	    {Rest, NRecs} = restore_tab_items([Rec | Recs], Tab,
					      RecName, Where, Snmp,
					      R#r.recs, R#r.insert_op),
	    restore_items(Rest, Header, Schema, Ext, R#r{recs = NRecs});
	false ->
	    Rest = skip_tab_items(Recs, Tab),
	    restore_items(Rest, Header, Schema, Ext, R)
    end;

restore_items([], _Header, _Schema, _Ext, R) ->
    R.

restore_func(Tab, R) ->
    case lists:keysearch(Tab, 1, R#r.table_options) of
	{value, {Tab, OP}} ->
	    OP;
	false ->
	    R#r.default_op
    end.

where_to_commit(Tab, CsList) ->
    Ram =   [{N, ram_copies} || N <- pick(Tab, ram_copies, CsList, [])],
    Disc =  [{N, disc_copies} || N <- pick(Tab, disc_copies, CsList, [])],
    DiscO = [{N, disc_only_copies} ||
		N <- pick(Tab, disc_only_copies, CsList, [])],
    ExtNodes = [{Alias, Mod, pick(Tab, Alias, CsList, [])} ||
		   {Alias, Mod} <- get_ext_types()],
    Ext = lists:foldl(fun({Alias, Mod, Ns}, Acc) ->
			      [{N, {ext, Alias, Mod}} || N <- Ns] ++ Acc
		      end, [], ExtNodes),
    Ram ++ Disc ++ DiscO ++ Ext.

%% Changes of the Meta info of schema itself is not allowed
restore_schema([{schema, schema, _List} | Schema], R) ->
    restore_schema(Schema, R);
restore_schema([{schema, Tab, List} | Schema], R) ->
    case restore_func(Tab, R) of
	clear_tables ->
	    do_clear_table(Tab),
	    Snmp = val({Tab, snmp}),
	    RecName = val({Tab, record_name}),
	    R2 = R#r{tables = [{Tab, undefined, Snmp, RecName} | R#r.tables]},
	    restore_schema(Schema, R2);
	recreate_tables ->
	    TidTs = case ?catch_val({Tab, cstruct}) of
			{'EXIT', _} ->
			    TTs = {_Mod, Tid, Ts} = get(mnesia_activity_state),
			    RunningNodes = val({current, db_nodes}),
			    Nodes = mnesia_lib:intersect(mnesia_lib:cs_to_nodes(list2cs(List)),
							 RunningNodes),
			    mnesia_locker:wlock_no_exist(Tid, Ts#tidstore.store, Tab, Nodes),
			    TTs;
			_ ->
			    get_tid_ts_and_lock(Tab, write)
		    end,
	    NC    = {cookie, ?unique_cookie},
	    List2 = lists:keyreplace(cookie, 1, List, NC),
	    Where = where_to_commit(Tab, List2),
	    Snmp  = pick(Tab, snmp, List2, []),
	    RecName = pick(Tab, record_name, List2, Tab),
	    insert_schema_ops(TidTs, [{op, restore_recreate, List2}]),
	    R2 = R#r{tables = [{Tab, Where, Snmp, RecName} | R#r.tables]},
	    restore_schema(Schema, R2);
	keep_tables ->
	    get_tid_ts_and_lock(Tab, write),
	    Snmp = val({Tab, snmp}),
	    RecName = val({Tab, record_name}),
	    R2 = R#r{tables = [{Tab, undefined, Snmp, RecName} | R#r.tables]},
	    restore_schema(Schema, R2);
	skip_tables ->
	    restore_schema(Schema, R)
    end;

restore_schema([{schema, Tab} | Schema], R) ->
    do_delete_table(Tab),
    Tabs = lists:delete(Tab,R#r.tables),
    restore_schema(Schema, R#r{tables = Tabs});
restore_schema([], R) ->
    R.

restore_tab_items([Rec | Rest], Tab, RecName, Where, Snmp, Recs, Op)
  when element(1, Rec) == Tab ->
    NewRecs = Op(Rec, Recs, RecName, Where, Snmp),
    restore_tab_items(Rest, Tab, RecName, Where, Snmp, NewRecs, Op);

restore_tab_items(Rest, _Tab, _RecName, _Where, _Snmp, Recs, _Op) ->
    {Rest, Recs}.

skip_tab_items([Rec| Rest], Tab)
  when element(1, Rec) == Tab ->
    skip_tab_items(Rest, Tab);
skip_tab_items(Recs, _) ->
    Recs.

%%%%%%%%% Dump tables %%%%%%%%%%%%%
dump_tables(Tabs) when is_list(Tabs) ->
    schema_transaction(fun() -> do_dump_tables(Tabs) end);
dump_tables(Tabs) ->
    {aborted, {bad_type, Tabs}}.

do_dump_tables(Tabs) ->
    TidTs = get_tid_ts_and_lock(schema, write),
    insert_schema_ops(TidTs, make_dump_tables(Tabs)).

make_dump_tables([schema | _Tabs]) ->
    mnesia:abort({bad_type, schema});
make_dump_tables([Tab | Tabs]) ->
    get_tid_ts_and_lock(Tab, read),
    TabDef = get_create_list(Tab),
    DiscResident =
        val({Tab, disc_copies}) ++
        val({Tab, disc_only_copies}) ++
        lists:concat([Ns || {{A,M},Ns} <- val({Tab, external_copies}),
			    lists:member(
			      mnesia_lib:semantics({ext,A,M},storage),
			      [disc_copies, disc_only_copies])]),
    verify([], DiscResident,
	   {"Only allowed on ram_copies", Tab, DiscResident}),
    [{op, dump_table, unknown, TabDef} | make_dump_tables(Tabs)];
make_dump_tables([]) ->
    [].

%% Merge the local schema with the schema on other nodes
merge_schema() ->
    schema_transaction(fun() -> do_merge_schema([]) end).

merge_schema(UserFun) ->
    schema_transaction(fun() ->
                               UserFun(fun(Arg) -> do_merge_schema(Arg) end)
                       end).

do_merge_schema(LockTabs0) ->
    {_Mod, Tid, Ts} = get_tid_ts_and_lock(schema, write),
    LockTabs = [{T, tab_to_nodes(T)} || T <- LockTabs0],
    [get_tid_ts_and_lock(T,write) || {T,_} <- LockTabs],
    Connected = val(recover_nodes),
    Running = val({current, db_nodes}),
    Store = Ts#tidstore.store,
    %% Verify that all nodes are locked that might not be the
    %% case, if this trans where queued when new nodes where added.
    case Running -- ets:lookup_element(Store, nodes, 2) of
	[] -> ok; %% All known nodes are locked
	Miss -> %% Abort! We don't want the sideeffects below to be executed
	    mnesia:abort({bad_commit, {missing_lock, Miss}})
    end,
    case Connected -- Running of
	[Node | _] = OtherNodes ->
	    %% Time for a schema merging party!
	    mnesia_locker:wlock_no_exist(Tid, Store, schema, [Node]),
            [mnesia_locker:wlock_no_exist(
               Tid, Store, T, mnesia_lib:intersect(Ns, OtherNodes))
             || {T,Ns} <- LockTabs],
	    case fetch_cstructs(Node) of
		{cstructs, Cstructs, RemoteRunning1} ->
		    LockedAlready = Running ++ [Node],
		    {New, Old} = mnesia_recover:connect_nodes(RemoteRunning1),
		    RemoteRunning = mnesia_lib:intersect(New ++ Old, RemoteRunning1),
		    if
			RemoteRunning /= RemoteRunning1 ->
			    mnesia_lib:error("Mnesia on ~p could not connect to node(s) ~p~n",
					     [node(), RemoteRunning1 -- RemoteRunning]),
			    mnesia:abort({node_not_running, RemoteRunning1 -- RemoteRunning});
			true -> ok
		    end,
		    NeedsLock = RemoteRunning -- LockedAlready,
		    mnesia_locker:wlock_no_exist(Tid, Store, schema, NeedsLock),
                    [mnesia_locker:wlock_no_exist(Tid, Store, T,
                                                  mnesia_lib:intersect(Ns,NeedsLock))
                     || {T,Ns} <- LockTabs],

		    NeedsConversion = need_old_cstructs(NeedsLock ++ LockedAlready),
		    {value, SchemaCs} = lists:keysearch(schema, #cstruct.name, Cstructs),
		    SchemaDef = cs2list(NeedsConversion, SchemaCs),
		    %% Announce that Node is running
		    A = [{op, announce_im_running, node(), SchemaDef, Running, RemoteRunning}],
		    do_insert_schema_ops(Store, A),

		    %% Introduce remote tables to local node
		    do_insert_schema_ops(Store, make_merge_schema(Node, NeedsConversion, Cstructs)),

		    %% Introduce local tables to remote nodes
		    Tabs = val({schema, tables}),
		    Ops = [{op, merge_schema, get_create_list(T)}
			   || T <- Tabs,
			      not lists:keymember(T, #cstruct.name, Cstructs)],
		    do_insert_schema_ops(Store, Ops),

		    %% Ensure that the txn will be committed on all nodes
		    NewNodes = RemoteRunning -- Running,
		    mnesia_lib:set(prepare_op, {announce_im_running,NewNodes}),
		    announce_im_running(NewNodes, SchemaCs),
		    {merged, Running, RemoteRunning};
		{error, Reason} ->
		    {"Cannot get cstructs", Node, Reason};
		{badrpc, Reason} ->
		    {"Cannot get cstructs", Node, {badrpc, Reason}}
	    end;
	[] ->
	    %% No more nodes to merge schema with
	    not_merged
    end.

fetch_cstructs(Node) ->
    Convert = mnesia_monitor:needs_protocol_conversion(Node),
    case rpc:call(Node, mnesia_controller, get_remote_cstructs, [])  of
	{cstructs, Cs0, RemoteRunning1} when Convert ->
	    {cstructs, [list2cs(cs2list(Cs)) || Cs <- Cs0], RemoteRunning1};
	Result ->
	    Result
    end.

need_old_cstructs() ->
    need_old_cstructs(val({schema, where_to_write})).

need_old_cstructs(Nodes) ->
    Filter = fun(Node) -> mnesia_monitor:needs_protocol_conversion(Node) end,
    case lists:filter(Filter, Nodes) of
	[] -> false;
	Ns -> lists:min([element(1, ?catch_val({protocol, Node})) || Node <- Ns])
    end.

tab_to_nodes(Tab) when is_atom(Tab) ->
    Cs = val({Tab, cstruct}),
    mnesia_lib:cs_to_nodes(Cs).

make_merge_schema(Node, NeedsConv, [Cs | Cstructs]) ->
    Ops = do_make_merge_schema(Node, NeedsConv, Cs),
    Ops ++ make_merge_schema(Node, NeedsConv, Cstructs);
make_merge_schema(_Node, _, []) ->
    [].

%% Merge definitions of schema table
do_make_merge_schema(Node, NeedsConv, RemoteCs = #cstruct{name = schema}) ->
    Cs = val({schema, cstruct}),
    Masters = mnesia_recover:get_master_nodes(schema),
    HasRemoteMaster = lists:member(Node, Masters),
    HasLocalMaster = lists:member(node(), Masters),
    Force = HasLocalMaster or HasRemoteMaster,
    %% What is the storage types opinions?
    StCsLocal   = mnesia_lib:cs_to_storage_type(node(), Cs),
    StRcsLocal  = mnesia_lib:cs_to_storage_type(node(), RemoteCs),
    StCsRemote  = mnesia_lib:cs_to_storage_type(Node, Cs),
    StRcsRemote = mnesia_lib:cs_to_storage_type(Node, RemoteCs),

    if
	Cs#cstruct.cookie == RemoteCs#cstruct.cookie,
	Cs#cstruct.version == RemoteCs#cstruct.version ->
	    %% Great, we have the same cookie and version
	    %% and do not need to merge cstructs
	    [];

	Cs#cstruct.cookie /= RemoteCs#cstruct.cookie,
	Cs#cstruct.disc_copies /= [],
	RemoteCs#cstruct.disc_copies /= [] ->
	    %% Both cstructs involves disc nodes
	    %% and we cannot merge them
	    if
		HasLocalMaster == true,
		HasRemoteMaster == false ->
		    %% Choose local cstruct,
		    %% since it's the master
		    [{op, merge_schema, cs2list(NeedsConv, Cs)}];

		HasRemoteMaster == true,
		HasLocalMaster == false ->
		    %% Choose remote cstruct,
		    %% since it's the master
		    [{op, merge_schema, cs2list(NeedsConv, RemoteCs)}];

		true ->
		    Str = io_lib:format("Incompatible schema cookies. "
					"Please, restart from old backup."
					"~w = ~w, ~w = ~w~n",
					[Node, cs2list(RemoteCs), node(), cs2list(Cs)]),
		    throw(Str)
	    end;

	StCsLocal /= StRcsLocal, StRcsLocal /= unknown, StCsLocal /= ram_copies ->
	    Str = io_lib:format("Incompatible schema storage types (local). "
				"on ~w storage ~w, on ~w storage ~w~n",
				[node(), StCsLocal, Node, StRcsLocal]),
	    throw(Str);
	StCsRemote /= StRcsRemote, StCsRemote /= unknown, StRcsRemote /= ram_copies ->
	    Str = io_lib:format("Incompatible schema storage types (remote). "
				"on ~w cs ~w, on ~w rcs ~w~n",
				[node(), cs2list(Cs), Node, cs2list(RemoteCs)]),
	    throw(Str);

     	Cs#cstruct.disc_copies /= [] ->
	    %% Choose local cstruct,
	    %% since it involves disc nodes
	    MergedCs = merge_cstructs(Cs, RemoteCs, Force),
	    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}];

	RemoteCs#cstruct.disc_copies /= [] ->
	    %% Choose remote cstruct,
	    %% since it involves disc nodes
	    MergedCs = merge_cstructs(RemoteCs, Cs, Force),
	    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}];

	Cs > RemoteCs ->
	    %% Choose remote cstruct
	    MergedCs = merge_cstructs(RemoteCs, Cs, Force),
	    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}];

	true ->
	    %% Choose local cstruct
	    MergedCs = merge_cstructs(Cs, RemoteCs, Force),
	    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}]
    end;

%% Merge definitions of normal table
do_make_merge_schema(Node, NeedsConv, RemoteCs = #cstruct{}) ->
    Tab = RemoteCs#cstruct.name,
    Masters = mnesia_recover:get_master_nodes(schema),
    HasRemoteMaster = lists:member(Node, Masters),
    HasLocalMaster = lists:member(node(), Masters),
    Force = HasLocalMaster or HasRemoteMaster,
    case ?catch_val({Tab, cstruct}) of
	{'EXIT', _} ->
	    %% A completely new table, created while Node was down
	    [{op, merge_schema, cs2list(NeedsConv, RemoteCs)}];
	Cs when Cs#cstruct.cookie == RemoteCs#cstruct.cookie ->
	    if
		Cs#cstruct.version == RemoteCs#cstruct.version ->
		    %% We have exactly the same version of the
		    %% table def
		    [];

		Cs#cstruct.version > RemoteCs#cstruct.version ->
		    %% Oops, we have different versions
		    %% of the table def, lets merge them.
		    %% The only changes that may have occurred
		    %% is that new replicas may have been added.
		    MergedCs = merge_cstructs(Cs, RemoteCs, Force),
		    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}];

		Cs#cstruct.version < RemoteCs#cstruct.version ->
		    %% Oops, we have different versions
		    %% of the table def, lets merge them
		    MergedCs = merge_cstructs(RemoteCs, Cs, Force),
		    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}]
	    end;
	Cs ->
	    %% Different cookies, not possible to merge
	    if
		HasLocalMaster == true,
		HasRemoteMaster == false ->
		    %% Choose local cstruct,
		    %% since it's the master
		    [{op, merge_schema, cs2list(NeedsConv, Cs)}];

		HasRemoteMaster == true,
		HasLocalMaster == false ->
		    %% Choose remote cstruct,
		    %% since it's the master
		    [{op, merge_schema, cs2list(NeedsConv, RemoteCs)}];

		true ->
		    Str = io_lib:format("Bad cookie in table definition"
					" ~w: ~w = ~w, ~w = ~w~n",
					[Tab, node(), Cs, Node, RemoteCs]),
		    throw(Str)
	    end
    end.

%% Change of table definitions (cstructs) requires all replicas
%% of the table to be active. New replicas, db_nodes and tables
%% may however be added even if some replica is inactive. These
%% invariants must be enforced in order to allow merge of cstructs.
%%
%% Returns a new cstruct or issues a fatal error
merge_cstructs(Cs0, RemoteCs, Force) ->
    Cs = verify_cstruct(Cs0),
    try do_merge_cstructs(Cs, RemoteCs, Force) of
	MergedCs when is_record(MergedCs, cstruct) ->
	    MergedCs
    catch exit:{aborted, _Reason} when Force == true ->
	    Cs;
	  exit:Reason -> exit(Reason);
	  error:Reason -> exit(Reason)
    end.

do_merge_cstructs(Cs, RemoteCs0, Force) ->
    RemoteCs = verify_cstruct(RemoteCs0),
    Ns = mnesia_lib:uniq(mnesia_lib:cs_to_nodes(Cs) ++
			 mnesia_lib:cs_to_nodes(RemoteCs)),
    {AnythingNew, MergedCs} =
	merge_storage_type(Ns, false, Cs, RemoteCs, Force),
    verify_cstruct(
      merge_versions(AnythingNew, MergedCs, RemoteCs, Force)).


merge_storage_type([N | Ns], AnythingNew, Cs, RemoteCs, Force) ->
    Local = mnesia_lib:cs_to_storage_type(N, Cs),
    Remote = mnesia_lib:cs_to_storage_type(N, RemoteCs),
    case compare_storage_type(true, Local, Remote) of
	{same, _Storage} ->
	    merge_storage_type(Ns, AnythingNew, Cs, RemoteCs, Force);
	{diff, Storage} ->
	    Cs2 = change_storage_type(N, Storage, Cs),
	    merge_storage_type(Ns, true, Cs2, RemoteCs, Force);
	incompatible when Force == true ->
	    merge_storage_type(Ns, AnythingNew, Cs, RemoteCs, Force);
	Other ->
	    Str = io_lib:format("Cannot merge storage type for node ~w "
				"in cstruct ~w with remote cstruct ~w (~w)~n",
				[N, Cs, RemoteCs, Other]),
	    throw(Str)
    end;
merge_storage_type([], AnythingNew, MergedCs, _RemoteCs, _Force) ->
    {AnythingNew, MergedCs}.

compare_storage_type(_Retry, Any, Any) ->
    {same, Any};
compare_storage_type(_Retry, unknown, Any) ->
    {diff, Any};
compare_storage_type(_Retry, ram_copies, disc_copies) ->
    {diff, disc_copies};
compare_storage_type(_Retry, disc_copies, disc_only_copies) ->
    {diff, disc_only_copies};
compare_storage_type(true, One, Another) ->
    compare_storage_type(false, Another, One);
compare_storage_type(false, _One, _Another) ->
    incompatible.

change_storage_type(N, ram_copies, Cs) ->
    Nodes = [N | Cs#cstruct.ram_copies],
    Cs#cstruct{ram_copies = mnesia_lib:uniq(Nodes)};
change_storage_type(N, disc_copies, Cs) ->
    Nodes = [N | Cs#cstruct.disc_copies],
    Cs#cstruct{disc_copies = mnesia_lib:uniq(Nodes)};
change_storage_type(N, disc_only_copies, Cs) ->
    Nodes = [N | Cs#cstruct.disc_only_copies],
    Cs#cstruct{disc_only_copies = mnesia_lib:uniq(Nodes)}.

%% BUGBUG: Verify match of frag info; equalit demanded for all but add_node

merge_versions(AnythingNew, Cs, RemoteCs, Force) ->
    if
	Cs#cstruct.name == schema ->
	    ok;
	Cs#cstruct.name /= schema,
	Cs#cstruct.cookie == RemoteCs#cstruct.cookie ->
	    ok;
	Force == true ->
	    ok;
	true ->
	    Str = io_lib:format("Bad cookies. Cannot merge definitions of "
				"table ~w. Local = ~w, Remote = ~w~n",
				[Cs#cstruct.name, Cs, RemoteCs]),
	    throw(Str)
    end,
    if
	Cs#cstruct.name == RemoteCs#cstruct.name,
	Cs#cstruct.type == RemoteCs#cstruct.type,
	Cs#cstruct.local_content == RemoteCs#cstruct.local_content,
	Cs#cstruct.attributes == RemoteCs#cstruct.attributes,
	Cs#cstruct.index == RemoteCs#cstruct.index,
	Cs#cstruct.snmp == RemoteCs#cstruct.snmp,
	Cs#cstruct.access_mode == RemoteCs#cstruct.access_mode,
	Cs#cstruct.majority == RemoteCs#cstruct.majority,
	Cs#cstruct.load_order == RemoteCs#cstruct.load_order,
	Cs#cstruct.user_properties == RemoteCs#cstruct.user_properties ->
	    do_merge_versions(AnythingNew, Cs, RemoteCs);
	Force == true ->
	    do_merge_versions(AnythingNew, Cs, RemoteCs);
	true ->
	    Str1 = io_lib:format("Cannot merge definitions of "
				"table ~w. Local = ~w, Remote = ~w~n",
				[Cs#cstruct.name, Cs, RemoteCs]),
	    throw(Str1)
    end.

do_merge_versions(AnythingNew, MergedCs, RemoteCs) ->
    {{Major1, Minor1}, _Detail1} = MergedCs#cstruct.version,
    {{Major2, Minor2}, _Detail2} = RemoteCs#cstruct.version,
    if
	AnythingNew == false ->
	    MergedCs;
	MergedCs#cstruct.version == RemoteCs#cstruct.version ->
	    V = {{Major1, Minor1}, dummy},
	    incr_version(MergedCs#cstruct{version = V});
	Major1 == Major2 ->
	    Minor = lists:max([Minor1, Minor2]),
	    V = {{Major1, Minor}, dummy},
	    incr_version(MergedCs#cstruct{version = V});
	Major1 /= Major2 ->
	    Major = lists:max([Major1, Major2]),
	    V = {{Major, 0}, dummy},
	    incr_version(MergedCs#cstruct{version = V})
    end.

%% Verify the basics
verify_merge(RemoteCs) ->
    Tab = RemoteCs#cstruct.name,
    Masters = mnesia_recover:get_master_nodes(schema),
    HasRemoteMaster = Masters /= [],
    case ?catch_val({Tab, cstruct}) of
	{'EXIT', _} ->
	    ok;
	Cs ->
	    StCsLocal   = mnesia_lib:cs_to_storage_type(node(), Cs),
	    StRcsLocal  = mnesia_lib:cs_to_storage_type(node(), RemoteCs),
	    if
		StCsLocal  == StRcsLocal ->   ok;
		StCsLocal  == unknown ->      ok;
		(StRcsLocal == unknown), (HasRemoteMaster == false) ->
		    {merge_error, Cs, RemoteCs};
		%%  Trust the merger
		true  -> ok
	    end
    end.

announce_im_running([N | Ns], SchemaCs) ->
    {L1, L2} = mnesia_recover:connect_nodes([N]),
    case lists:member(N, L1) or lists:member(N, L2) of
	true ->
	    mnesia_lib:add({current, db_nodes}, N),
	    mnesia_controller:add_active_replica(schema, N, SchemaCs);
	false ->
	    mnesia_lib:error("Mnesia on ~p could not connect to node ~p~n",
			     [node(), N]),
	    mnesia:abort({node_not_running, N})
    end,
    announce_im_running(Ns, SchemaCs);
announce_im_running([], _) ->
    [].

unannounce_im_running([N | Ns]) ->
    mnesia_lib:del({current, db_nodes}, N),
    mnesia_controller:del_active_replica(schema, N),
    unannounce_im_running(Ns);
unannounce_im_running([]) ->
    ok.
