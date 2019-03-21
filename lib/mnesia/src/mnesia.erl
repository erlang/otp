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
%% This module exports the public interface of the Mnesia DBMS engine

-module(mnesia).
%-behaviour(mnesia_access).

-export([
	 %% Start, stop and debugging
	 start/0, start/1, stop/0,           % Not for public use
	 set_debug_level/1, lkill/0, kill/0, % Not for public use
	 ms/0,
	 change_config/2,

	 %% Activity mgt
	 abort/1, transaction/1, transaction/2, transaction/3,
	 sync_transaction/1, sync_transaction/2, sync_transaction/3,
	 async_dirty/1, async_dirty/2, sync_dirty/1, sync_dirty/2, ets/1, ets/2,
	 activity/2, activity/3, activity/4, % Not for public use
	 is_transaction/0,

	 %% Access within an activity - Lock acquisition
	 lock/2, lock/4,
	 lock_table/2,
	 read_lock_table/1,
	 write_lock_table/1,

	 %% Access within an activity - Updates
	 write/1, s_write/1, write/3, write/5,
	 delete/1, s_delete/1, delete/3, delete/5,
	 delete_object/1, s_delete_object/1, delete_object/3, delete_object/5,

	 %% Access within an activity - Reads
	 read/1, read/2, wread/1, read/3, read/5,
	 match_object/1, match_object/3, match_object/5,
	 select/1,select/2,select/3,select/4,select/5,select/6,
	 all_keys/1, all_keys/4,
	 index_match_object/2, index_match_object/4, index_match_object/6,
	 index_read/3, index_read/6,
	 first/1, next/2, last/1, prev/2,
	 first/3, next/4, last/3, prev/4,

	 %% Iterators within an activity
	 foldl/3, foldl/4, foldr/3, foldr/4,

	 %% Dirty access regardless of activities - Updates
	 dirty_write/1, dirty_write/2,
	 dirty_delete/1, dirty_delete/2,
	 dirty_delete_object/1, dirty_delete_object/2,
	 dirty_update_counter/2, dirty_update_counter/3,

	 %% Dirty access regardless of activities - Read
	 dirty_read/1, dirty_read/2,
	 dirty_select/2,
	 dirty_match_object/1, dirty_match_object/2, dirty_all_keys/1,
	 dirty_index_match_object/2, dirty_index_match_object/3,
	 dirty_index_read/3, dirty_slot/2,
	 dirty_first/1, dirty_next/2, dirty_last/1, dirty_prev/2,

	 %% Info
	 table_info/2, table_info/4, schema/0, schema/1,
	 error_description/1, info/0, system_info/1,
	 system_info/0,                      % Not for public use

	 %% Database mgt
	 create_schema/1, create_schema/2, delete_schema/1,
         add_backend_type/2,
	 backup/1, backup/2, traverse_backup/4, traverse_backup/6,
	 install_fallback/1, install_fallback/2,
	 uninstall_fallback/0, uninstall_fallback/1,
	 activate_checkpoint/1, deactivate_checkpoint/1,
	 backup_checkpoint/2, backup_checkpoint/3, restore/2,

	 %% Table mgt
	 create_table/1, create_table/2, delete_table/1,
	 add_table_copy/3, del_table_copy/2, move_table_copy/3,
	 add_table_index/2, del_table_index/2,
	 transform_table/3, transform_table/4,
	 change_table_copy_type/3, change_table_majority/2,
	 read_table_property/2, write_table_property/2, delete_table_property/2,
	 change_table_frag/2,
	 clear_table/1, clear_table/4,

	 %% Table load
	 dump_tables/1, wait_for_tables/2, force_load_table/1,
	 change_table_access_mode/2, change_table_load_order/2,
	 set_master_nodes/1, set_master_nodes/2,

	 %% Misc admin
	 dump_log/0, sync_log/0,
	 subscribe/1, unsubscribe/1, report_event/1,

	 %% Snmp
	 snmp_open_table/2, snmp_close_table/1,
	 snmp_get_row/2, snmp_get_next_index/2, snmp_get_mnesia_key/2,

	 %% Textfile access
	 load_textfile/1, dump_to_textfile/1,

	 %% QLC functions
	 table/1, table/2,

	 %% Mnemosyne exclusive
	 get_activity_id/0, put_activity_id/1, % Not for public use

	 %% Mnesia internal functions
	 dirty_rpc/4,                          % Not for public use
	 has_var/1, fun_select/7, fun_select/10, select_cont/3, dirty_sel_init/5,
	 foldl/6, foldr/6,

	 %% Module internal callback functions
	 raw_table_info/2,                      % Not for public use
	 remote_dirty_match_object/2,           % Not for public use
	 remote_dirty_select/2                  % Not for public use
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("mnesia.hrl").
-import(mnesia_lib, [verbose/2]).

-type create_option() ::
        {'access_mode', 'read_write' | 'read_only'} |
        {'attributes', [atom()]} |
        {'disc_copies', [node()]} |
        {'disc_only_copies', [node]} |
        {'index', [index_attr()]} |
        {'load_order', non_neg_integer()} |
        {'majority', boolean()} |
        {'ram_copies', [node()]} |
        {'record_name', atom()} |
        {'snmp', SnmpStruct::term()} |
        {'storage_properties', [{Backend::module(), [BackendProp::_]}]} |
        {'type', 'set' | 'ordered_set' | 'bag'} |
        {'local_content', boolean()} |
        {'user_properties', proplists:proplist()}.

-type t_result(Res) :: {'atomic', Res} | {'aborted', Reason::term()}.
-type activity() :: 'ets' | 'async_dirty' | 'sync_dirty' | 'transaction' | 'sync_transaction' |
                    {'transaction', Retries::non_neg_integer()} |
                    {'sync_transaction', Retries::non_neg_integer()}.
-type table() :: atom().
-type storage_type() :: 'ram_copies' | 'disc_copies' | 'disc_only_copies'.
-type index_attr() :: atom() | non_neg_integer() | {atom()}.
-type write_locks() :: 'write' | 'sticky_write'.
-type read_locks() :: 'read'.
-type lock_kind() :: write_locks() | read_locks().
-type select_continuation() :: term().
-type snmp_struct() :: [{atom(), snmp_type() | tuple_of(snmp_type())}].
-type snmp_type() :: 'fix_string' | 'string' | 'integer'.
-type tuple_of(_T) :: tuple().
-type config_key() :: extra_db_nodes | dc_dump_limit.
-type config_value() :: [node()] | number().
-type config_result() :: {ok, config_value()} | {error, term()}.

-define(DEFAULT_ACCESS, ?MODULE).

%% Select
-define(PATTERN_TO_OBJECT_MATCH_SPEC(Pat), [{Pat,[],['$_']}]).
-define(PATTERN_TO_BINDINGS_MATCH_SPEC(Pat), [{Pat,[],['$$']}]).

%% Local function in order to avoid external function call
val(Var) ->
    case ?catch_val_and_stack(Var) of
	{'EXIT', Stacktrace} -> mnesia_lib:other_val(Var, Stacktrace);
	Value -> Value
    end.

is_dollar_digits(Var) ->
    case atom_to_list(Var) of
	[$$ | Digs] ->
	    is_digits(Digs);
	_ ->
	    false
    end.

is_digits([Dig | Tail]) ->
    if
	$0 =< Dig, Dig =< $9 ->
	    is_digits(Tail);
	true ->
	    false
    end;
is_digits([]) ->
    true.

has_var(X) when is_atom(X) ->
    if
	X == '_' ->
	    true;
	is_atom(X) ->
	    is_dollar_digits(X);
	true  ->
	    false
    end;
has_var(X) when is_tuple(X) ->
    e_has_var(X, tuple_size(X));
has_var([H|T]) ->
    case has_var(H) of
	false -> has_var(T);
	Other -> Other
    end;
has_var(_) -> false.

e_has_var(_, 0) -> false;
e_has_var(X, Pos) ->
    case has_var(element(Pos, X))of
	false -> e_has_var(X, Pos-1);
	Other -> Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start and stop
-spec start() -> 'ok' | {'error', term()}.
start() ->
    start([]).

start_() ->
    {Time , Res} =  timer:tc(application, start, [?APPLICATION, temporary]),

    Secs = Time div 1000000,
    case Res of
	ok ->
	    verbose("Mnesia started, ~p seconds~n",[ Secs]),
	    ok;
	{error, {already_started, mnesia}} ->
	    verbose("Mnesia already started, ~p seconds~n",[ Secs]),
	    ok;
	{error, R} ->
	    verbose("Mnesia failed to start, ~p seconds: ~p~n",[ Secs, R]),
	    {error, R}
    end.

-spec start([{Option::atom(), Value::_}]) -> 'ok' | {'error', term()}.
start(ExtraEnv) when is_list(ExtraEnv) ->
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    patched_start(ExtraEnv);
	Error ->
	    Error
    end;
start(ExtraEnv) ->
    {error, {badarg, ExtraEnv}}.

patched_start([{Env, Val} | Tail]) when is_atom(Env) ->
    case mnesia_monitor:patch_env(Env, Val) of
	{error, Reason} ->
	    {error, Reason};
	_NewVal ->
	    patched_start(Tail)
    end;
patched_start([Head | _]) ->
    {error, {bad_type, Head}};
patched_start([]) ->
    start_().

-spec stop() -> 'stopped' | {'error', term()}.
stop() ->
    case application:stop(?APPLICATION) of
	ok -> stopped;
	{error, {not_started, ?APPLICATION}} -> stopped;
	Other -> Other
    end.

-spec change_config(Config::config_key(), Value::config_value()) ->
	  config_result().
change_config(extra_db_nodes, Ns) when is_list(Ns) ->
    mnesia_controller:connect_nodes(Ns);
change_config(dc_dump_limit, N) when is_number(N), N > 0 ->
    case mnesia_lib:is_running() of
	yes ->
	    mnesia_lib:set(dc_dump_limit, N),
	    {ok, N};
	_ ->
	    {error, {not_started, ?APPLICATION}}
    end;
change_config(BadKey, _BadVal) ->
    {error, {badarg, BadKey}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debugging

set_debug_level(Level) ->
    mnesia_subscr:set_debug_level(Level).

lkill() ->
    mnesia_sup:kill().

kill() ->
    rpc:multicall(mnesia_sup, kill, []).

ms() ->
    [
     mnesia,
     mnesia_app,
     mnesia_backup,
     mnesia_bup,
     mnesia_checkpoint,
     mnesia_checkpoint_sup,
     mnesia_controller,
     mnesia_dumper,
     mnesia_loader,
     mnesia_frag,
     mnesia_frag_hash,
     mnesia_index,
     mnesia_kernel_sup,
     mnesia_late_loader,
     mnesia_lib,
     mnesia_log,
     mnesia_registry,
     mnesia_schema,
     mnesia_snmp_hook,
     mnesia_snmp_sup,
     mnesia_subscr,
     mnesia_sup,
     mnesia_text,
     mnesia_tm,
     mnesia_recover,
     mnesia_locker,

     %% Keep these last in the list, so
     %% mnesia_sup kills these last
     mnesia_ext_sup,
     mnesia_monitor,
     mnesia_event
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Activity mgt

-spec abort(_) -> no_return().
abort(Reason = {aborted, _}) ->
    exit(Reason);
abort(Reason) ->
    exit({aborted, Reason}).

-spec is_transaction() -> boolean().
is_transaction() ->
    case get(mnesia_activity_state) of
	{_, Tid, _Ts} when element(1,Tid) == tid ->
	    true;
	_ ->
	    false
    end.

-spec transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
transaction(Fun) ->
    transaction(get(mnesia_activity_state), Fun, [], infinity, ?DEFAULT_ACCESS, async).

-spec transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res),
      Retries :: non_neg_integer() | 'infinity';
                 (Fun, [Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
transaction(Fun, Retries) when is_integer(Retries), Retries >= 0 ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, async);
transaction(Fun, Retries) when Retries == infinity ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, async);
transaction(Fun, Args) ->
    transaction(get(mnesia_activity_state), Fun, Args, infinity, ?DEFAULT_ACCESS, async).

-spec transaction(Fun, [Arg::_], Retries) -> t_result(Res) when
      Fun :: fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity'.
transaction(Fun, Args, Retries) ->
    transaction(get(mnesia_activity_state), Fun, Args, Retries, ?DEFAULT_ACCESS, async).

-spec sync_transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
sync_transaction(Fun) ->
    transaction(get(mnesia_activity_state), Fun, [], infinity, ?DEFAULT_ACCESS, sync).

-spec sync_transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res),
      Retries :: non_neg_integer() | 'infinity';
                      (Fun, [Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
sync_transaction(Fun, Retries) when is_integer(Retries), Retries >= 0 ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, sync);
sync_transaction(Fun, Retries) when Retries == infinity ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, sync);
sync_transaction(Fun, Args) ->
    transaction(get(mnesia_activity_state), Fun, Args, infinity, ?DEFAULT_ACCESS, sync).

-spec sync_transaction(Fun, [Arg::_], Retries) -> t_result(Res) when
      Fun :: fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity'.
sync_transaction(Fun, Args, Retries) ->
    transaction(get(mnesia_activity_state), Fun, Args, Retries, ?DEFAULT_ACCESS, sync).

transaction(State, Fun, Args, Retries, Mod, Kind)
  when is_function(Fun), is_list(Args), Retries == infinity, is_atom(Mod) ->
    mnesia_tm:transaction(State, Fun, Args, Retries, Mod, Kind);
transaction(State, Fun, Args, Retries, Mod, Kind)
  when is_function(Fun), is_list(Args), is_integer(Retries), Retries >= 0, is_atom(Mod) ->
    mnesia_tm:transaction(State, Fun, Args, Retries, Mod, Kind);
transaction(_State, Fun, Args, Retries, Mod, _Kind) ->
    {aborted, {badarg, Fun, Args, Retries, Mod}}.

non_transaction(State, Fun, Args, ActivityKind, Mod)
  when is_function(Fun), is_list(Args), is_atom(Mod) ->
    mnesia_tm:non_transaction(State, Fun, Args, ActivityKind, Mod);
non_transaction(_State, Fun, Args, _ActivityKind, _Mod) ->
    {aborted, {badarg, Fun, Args}}.

-spec async_dirty(Fun) -> Res | no_return() when
      Fun :: fun(() -> Res).
async_dirty(Fun) ->
    async_dirty(Fun, []).

-spec async_dirty(Fun, [Arg::_]) -> Res | no_return() when
      Fun :: fun((...) -> Res).
async_dirty(Fun, Args) ->
    non_transaction(get(mnesia_activity_state), Fun, Args, async_dirty, ?DEFAULT_ACCESS).

-spec sync_dirty(Fun) -> Res | no_return() when
      Fun :: fun(() -> Res).
sync_dirty(Fun) ->
    sync_dirty(Fun, []).

-spec sync_dirty(Fun, [Arg::_]) -> Res | no_return() when
      Fun :: fun((...) -> Res).
sync_dirty(Fun, Args) ->
    non_transaction(get(mnesia_activity_state), Fun, Args, sync_dirty, ?DEFAULT_ACCESS).

-spec ets(Fun) -> Res | no_return() when
      Fun :: fun(() -> Res).
ets(Fun) ->
    ets(Fun, []).

-spec ets(Fun, [Arg::_]) -> Res | no_return() when
      Fun :: fun((...) -> Res).
ets(Fun, Args) ->
    non_transaction(get(mnesia_activity_state), Fun, Args, ets, ?DEFAULT_ACCESS).

-spec activity(Kind, Fun) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun(() -> Res).
activity(Kind, Fun) ->
    activity(Kind, Fun, []).

-spec activity(Kind, Fun, [Arg::_]) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun((...) -> Res);
              (Kind, Fun, Mod) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun(() -> Res),
      Mod  :: atom().

activity(Kind, Fun, Args) when is_list(Args) ->
    activity(Kind, Fun, Args, mnesia_monitor:get_env(access_module));
activity(Kind, Fun, Mod) ->
    activity(Kind, Fun, [], Mod).

-spec activity(Kind, Fun, [Arg::_], Mod) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun((...) -> Res),
      Mod  :: atom().

activity(Kind, Fun, Args, Mod) ->
    State = get(mnesia_activity_state),
    case Kind of
	ets ->                    non_transaction(State, Fun, Args, Kind, Mod);
	async_dirty ->            non_transaction(State, Fun, Args, Kind, Mod);
	sync_dirty ->             non_transaction(State, Fun, Args, Kind, Mod);
	transaction ->            wrap_trans(State, Fun, Args, infinity, Mod, async);
	{transaction, Retries} -> wrap_trans(State, Fun, Args, Retries, Mod, async);
	sync_transaction ->            wrap_trans(State, Fun, Args, infinity, Mod, sync);
	{sync_transaction, Retries} -> wrap_trans(State, Fun, Args, Retries, Mod, sync);
	_ ->                      {aborted, {bad_type, Kind}}
    end.

wrap_trans(State, Fun, Args, Retries, Mod, Kind) ->
    case transaction(State, Fun, Args, Retries, Mod, Kind) of
	{atomic, GoodRes} -> GoodRes;
	BadRes -> exit(BadRes)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access within an activity - lock acquisition

%% Grab a lock on an item in the global lock table
%% Item may be any term. Lock may be write or read.
%% write lock is set on all the given nodes
%% read lock is only set on the first node
%% Nodes may either be a list of nodes or one node as an atom
%% Mnesia on all Nodes must be connected to each other, but
%% it is not neccessary that they are up and running.
-spec lock(LockItem, LockKind) -> list() | tuple() | no_return() when
      LockItem :: {'record', table(), Key::term()} |
                  {'table',  table()} |
                  {'global', Key::term(), MnesiaNodes::[node()]},
      LockKind :: lock_kind() | 'load'.
lock(LockItem, LockKind) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    lock(Tid, Ts, LockItem, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:lock(Tid, Ts, LockItem, LockKind);
	_ ->
	    abort(no_transaction)
    end.

-spec lock_table(Tab::table(), LockKind) -> [MnesiaNode] | no_return() when
      MnesiaNode :: node(),
      LockKind :: lock_kind() | load.
lock_table(Tab, LockKind) ->
    lock({table, Tab}, LockKind).

lock(Tid, Ts, LockItem, LockKind) ->
    case element(1, Tid) of
	tid ->
	    case LockItem of
		{record, Tab, Key} ->
		    lock_record(Tid, Ts, Tab, Key, LockKind);
		{table, Tab} ->
		    lock_table(Tid, Ts, Tab, LockKind);
		{global, GlobalKey, Nodes} ->
		    global_lock(Tid, Ts, GlobalKey, LockKind, Nodes);
		_ ->
		    abort({bad_type, LockItem})
	    end;
	_Protocol ->
	    []
    end.

%% Grab a read lock on a whole table
-spec read_lock_table(Tab::table()) -> ok.
read_lock_table(Tab) ->
    lock({table, Tab}, read),
    ok.

%% Grab a write lock on a whole table
-spec write_lock_table(Tab::table()) -> ok.
write_lock_table(Tab) ->
    lock({table, Tab}, write),
    ok.

lock_record(Tid, Ts, Tab, Key, LockKind) when is_atom(Tab) ->
    Store = Ts#tidstore.store,
    Oid =  {Tab, Key},
    case LockKind of
	read ->
	    mnesia_locker:rlock(Tid, Store, Oid);
	write ->
	    mnesia_locker:wlock(Tid, Store, Oid);
	sticky_write ->
	    mnesia_locker:sticky_wlock(Tid, Store, Oid);
	none ->
	    [];
	_ ->
	    abort({bad_type, Tab, LockKind})
    end;
lock_record(_Tid, _Ts, Tab, _Key, _LockKind) ->
    abort({bad_type, Tab}).

lock_table(Tid, Ts, Tab, LockKind) when is_atom(Tab) ->
    Store = Ts#tidstore.store,
    case LockKind of
	read ->
	    mnesia_locker:rlock_table(Tid, Store, Tab);
	write ->
	    mnesia_locker:wlock_table(Tid, Store, Tab);
	load ->
	    mnesia_locker:load_lock_table(Tid, Store, Tab);
	sticky_write ->
	    mnesia_locker:sticky_wlock_table(Tid, Store, Tab);
	none ->
	    [];
	_ ->
	    abort({bad_type, Tab, LockKind})
    end;
lock_table(_Tid, _Ts, Tab, _LockKind) ->
    abort({bad_type, Tab}).

global_lock(Tid, Ts, Item, Kind, Nodes) when is_list(Nodes) ->
    case element(1, Tid) of
	tid ->
	    Store = Ts#tidstore.store,
	    GoodNs = good_global_nodes(Nodes),
	    if
		Kind /= read, Kind /= write ->
		    abort({bad_type, Kind});
		true ->
		    mnesia_locker:global_lock(Tid, Store, Item, Kind, GoodNs)
	    end;
	_Protocol ->
	    []
    end;
global_lock(_Tid, _Ts, _Item, _Kind, Nodes) ->
    abort({bad_type, Nodes}).

good_global_nodes(Nodes) ->
    Recover = [node() | val(recover_nodes)],
    mnesia_lib:intersect(Nodes, Recover).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access within an activity - updates
-spec write(Record::tuple()) -> 'ok'.
write(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    write(Tab, Val, write);
write(Val) ->
    abort({bad_type, Val}).

-spec s_write(Record::tuple()) -> 'ok'.
s_write(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    write(Tab, Val, sticky_write).

-spec write(Tab::table(), Record::tuple(), LockKind::write_locks()) -> 'ok'.
write(Tab, Val, LockKind) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    write(Tid, Ts, Tab, Val, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:write(Tid, Ts, Tab, Val, LockKind);
	_ ->
	    abort(no_transaction)
    end.

write(Tid, Ts, Tab, Val, LockKind)
  when is_atom(Tab), Tab /= schema, is_tuple(Val), tuple_size(Val) > 2 ->
    case element(1, Tid) of
	ets ->
	    ?ets_insert(Tab, Val),
	    ok;
	tid ->
	    Store = Ts#tidstore.store,
	    Oid = {Tab, element(2, Val)},
	    case LockKind of
		write ->
		    mnesia_locker:wlock(Tid, Store, Oid);
		sticky_write ->
		    mnesia_locker:sticky_wlock(Tid, Store, Oid);
		_ ->
		    abort({bad_type, Tab, LockKind})
	    end,
	    write_to_store(Tab, Store, Oid, Val);
	Protocol ->
	    do_dirty_write(Protocol, Tab, Val)
    end;
write(_Tid, _Ts, Tab, Val, LockKind) ->
    abort({bad_type, Tab, Val, LockKind}).

write_to_store(Tab, Store, Oid, Val) ->
    {_, _, Type} = mnesia_lib:validate_record(Tab, Val),
    Oid = {Tab, element(2, Val)},
    case Type of
	bag ->
	    ?ets_insert(Store, {Oid, Val, write});
	_  ->
	    ?ets_delete(Store, Oid),
	    ?ets_insert(Store, {Oid, Val, write})
    end,
    ok.

-spec delete({Tab::table(), Key::_}) -> 'ok'.
delete({Tab, Key}) ->
    delete(Tab, Key, write);
delete(Oid) ->
    abort({bad_type, Oid}).

-spec s_delete({Tab::table(), Key::_}) -> 'ok'.
s_delete({Tab, Key}) ->
    delete(Tab, Key, sticky_write);
s_delete(Oid) ->
    abort({bad_type, Oid}).

-spec delete(Tab::table(), Key::_, LockKind::write_locks()) -> 'ok'.
delete(Tab, Key, LockKind) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    delete(Tid, Ts, Tab, Key, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:delete(Tid, Ts, Tab, Key, LockKind);
	_ ->
	    abort(no_transaction)
    end.

delete(Tid, Ts, Tab, Key, LockKind)
  when is_atom(Tab), Tab /= schema ->
      case element(1, Tid) of
	  ets ->
	      ?ets_delete(Tab, Key),
	      ok;
	  tid ->
	      Store = Ts#tidstore.store,
	      Oid = {Tab, Key},
	      case LockKind of
		  write ->
		      mnesia_locker:wlock(Tid, Store, Oid);
		  sticky_write ->
		      mnesia_locker:sticky_wlock(Tid, Store, Oid);
		  _ ->
		      abort({bad_type, Tab, LockKind})
	      end,
	      ?ets_delete(Store, Oid),
	      ?ets_insert(Store, {Oid, Oid, delete}),
	      ok;
	Protocol ->
	      do_dirty_delete(Protocol, Tab, Key)
    end;
delete(_Tid, _Ts, Tab, _Key, _LockKind) ->
    abort({bad_type, Tab}).

-spec delete_object(Rec::tuple()) -> 'ok'.
delete_object(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    delete_object(Tab, Val, write);
delete_object(Val) ->
    abort({bad_type, Val}).

-spec s_delete_object(Rec::tuple()) -> 'ok'.
s_delete_object(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    delete_object(Tab, Val, sticky_write);
s_delete_object(Val) ->
    abort({bad_type, Val}).

-spec delete_object(Tab::table(), Rec::tuple(), LockKind::write_locks()) -> 'ok'.
delete_object(Tab, Val, LockKind) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    delete_object(Tid, Ts, Tab, Val, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:delete_object(Tid, Ts, Tab, Val, LockKind);
	_ ->
	    abort(no_transaction)
    end.

delete_object(Tid, Ts, Tab, Val, LockKind)
  when is_atom(Tab), Tab /= schema, is_tuple(Val), tuple_size(Val) > 2 ->
    case has_var(Val) of
	false ->
	    do_delete_object(Tid, Ts, Tab, Val, LockKind);
	true ->
	    abort({bad_type, Tab, Val})
    end;
delete_object(_Tid, _Ts, Tab, _Key, _LockKind) ->
    abort({bad_type, Tab}).

do_delete_object(Tid, Ts, Tab, Val, LockKind) ->
      case element(1, Tid) of
	  ets ->
	      ?ets_match_delete(Tab, Val),
	      ok;
	  tid ->
	      Store = Ts#tidstore.store,
	      Oid = {Tab, element(2, Val)},
	      case LockKind of
		  write ->
		      mnesia_locker:wlock(Tid, Store, Oid);
		  sticky_write ->
		      mnesia_locker:sticky_wlock(Tid, Store, Oid);
		  _ ->
		      abort({bad_type, Tab, LockKind})
	      end,
	      case val({Tab, setorbag}) of
		  bag ->
		      ?ets_match_delete(Store, {Oid, Val, '_'}),
		      ?ets_insert(Store, {Oid, Val, delete_object});
		  _ ->
		      case ?ets_match_object(Store, {Oid, '_', write}) of
			      [] ->
			          ?ets_match_delete(Store, {Oid, Val, '_'}),
			          ?ets_insert(Store, {Oid, Val, delete_object});
			      Ops  ->
			          case lists:member({Oid, Val, write}, Ops) of
			              true ->
			                  ?ets_delete(Store, Oid),
			                  ?ets_insert(Store, {Oid, Oid, delete});
			              false -> ok
			          end
		      end
	      end,
	      ok;
	Protocol ->
	      do_dirty_delete_object(Protocol, Tab, Val)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access within an activity - read

-spec read(Tab::table(), Key::_) -> [tuple()].
read(Tab, Key) ->
    read(Tab, Key, read).

-spec read({Tab::table(), Key::_}) -> [tuple()].
read({Tab, Key}) ->
    read(Tab, Key, read);
read(Oid) ->
    abort({bad_type, Oid}).

-spec wread({Tab::table(), Key::_}) -> [tuple()].
wread({Tab, Key}) ->
    read(Tab, Key, write);
wread(Oid) ->
    abort({bad_type, Oid}).

-spec read(Tab::table(), Key::_, LockKind::lock_kind()) -> [tuple()].
read(Tab, Key, LockKind) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    read(Tid, Ts, Tab, Key, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:read(Tid, Ts, Tab, Key, LockKind);
	_ ->
	    abort(no_transaction)
    end.

read(Tid, Ts, Tab, Key, LockKind)
  when is_atom(Tab), Tab /= schema ->
    case element(1, Tid) of
	ets ->
	    ?ets_lookup(Tab, Key);
	tid ->
	    Store = Ts#tidstore.store,
	    Oid = {Tab, Key},
	    ObjsFun =
                fun() ->
                        case LockKind of
                            read ->
                                mnesia_locker:rlock(Tid, Store, Oid);
                            write ->
                                mnesia_locker:rwlock(Tid, Store, Oid);
                            sticky_write ->
                                mnesia_locker:sticky_rwlock(Tid, Store, Oid);
                            _ ->
                                abort({bad_type, Tab, LockKind})
                        end
                end,
	    add_written(?ets_lookup(Store, Oid), Tab, ObjsFun, LockKind);
	_Protocol ->
	    dirty_read(Tab, Key)
    end;
read(_Tid, _Ts, Tab, _Key, _LockKind) ->
    abort({bad_type, Tab}).

-spec first(Tab::table()) -> Key::term().
first(Tab) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    first(Tid, Ts, Tab);
	{Mod, Tid, Ts} ->
	    Mod:first(Tid, Ts, Tab);
	_ ->
	    abort(no_transaction)
    end.

first(Tid, Ts, Tab)
  when is_atom(Tab), Tab /= schema ->
    case element(1, Tid) of
	ets ->
	    ?ets_first(Tab);
	tid ->
	    lock_table(Tid, Ts, Tab, read),
	    do_fixtable(Tab,Ts),
	    Key = dirty_first(Tab),
	    stored_keys(Tab,Key,'$end_of_table',Ts,next,
			val({Tab, setorbag}));
	_Protocol ->
	    dirty_first(Tab)
    end;
first(_Tid, _Ts,Tab) ->
    abort({bad_type, Tab}).

-spec last(Tab::table()) -> Key::term().
last(Tab) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    last(Tid, Ts, Tab);
	{Mod, Tid, Ts} ->
	    Mod:last(Tid, Ts, Tab);
	_ ->
	    abort(no_transaction)
    end.

last(Tid, Ts, Tab)
  when is_atom(Tab), Tab /= schema ->
    case element(1, Tid) of
	ets ->
	    ?ets_last(Tab);
	tid ->
	    lock_table(Tid, Ts, Tab, read),
	    do_fixtable(Tab,Ts),
	    Key = dirty_last(Tab),
	    stored_keys(Tab,Key,'$end_of_table',Ts,prev,
			val({Tab, setorbag}));
	_Protocol ->
	    dirty_last(Tab)
    end;
last(_Tid, _Ts,Tab) ->
    abort({bad_type, Tab}).

-spec next(Tab::table(), Key::term()) -> NextKey::term().
next(Tab,Key) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS,Tid,Ts} ->
	    next(Tid,Ts,Tab,Key);
	{Mod,Tid,Ts} ->
	    Mod:next(Tid,Ts,Tab,Key);
	_ ->
	    abort(no_transaction)
    end.
next(Tid,Ts,Tab,Key)
  when is_atom(Tab), Tab /= schema ->
    case element(1, Tid) of
	ets ->
	    ?ets_next(Tab,Key);
	tid ->
	    lock_table(Tid, Ts, Tab, read),
	    do_fixtable(Tab,Ts),
	    New = ?CATCH(dirty_next(Tab,Key)),
	    stored_keys(Tab,New,Key,Ts,next,
			val({Tab, setorbag}));
	_Protocol ->
	    dirty_next(Tab,Key)
    end;
next(_Tid, _Ts,Tab,_) ->
    abort({bad_type, Tab}).

-spec prev(Tab::table(), Key::term()) -> PrevKey::term().
prev(Tab,Key) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS,Tid,Ts} ->
	    prev(Tid,Ts,Tab,Key);
	{Mod,Tid,Ts} ->
	    Mod:prev(Tid,Ts,Tab,Key);
	_ ->
	    abort(no_transaction)
    end.
prev(Tid,Ts,Tab,Key)
  when is_atom(Tab), Tab /= schema ->
    case element(1, Tid) of
	ets ->
	    ?ets_prev(Tab,Key);
	tid ->
	    lock_table(Tid, Ts, Tab, read),
	    do_fixtable(Tab,Ts),
	    New = ?CATCH(dirty_prev(Tab,Key)),
	    stored_keys(Tab,New,Key,Ts,prev,
			val({Tab, setorbag}));
	_Protocol ->
	    dirty_prev(Tab,Key)
    end;
prev(_Tid, _Ts,Tab,_) ->
    abort({bad_type, Tab}).

%% Compensate for transaction written and/or deleted records
stored_keys(Tab,'$end_of_table',Prev,Ts,Op,Type) ->
    case ts_keys(Ts#tidstore.store,Tab,Op,Type,[]) of
	[] -> '$end_of_table';
	Keys when Type == ordered_set->
	    get_ordered_tskey(Prev,Keys,Op);
	Keys ->
	    get_next_tskey(Prev,Keys,Tab)
    end;
stored_keys(Tab,{'EXIT',{aborted,R={badarg,[Tab,Key]}}},
	    Key,#tidstore{store=Store},Op,Type) ->
    %% Had to match on error, ouch..
    case ?ets_match(Store, {{Tab, Key}, '_', '$1'}) of
	[] ->  abort(R);
	Ops ->
	    case lists:last(Ops) of
		[delete] -> abort(R);
		_ ->
		    case ts_keys(Store,Tab,Op,Type,[]) of
			[] -> '$end_of_table';
			Keys -> get_next_tskey(Key,Keys,Tab)
		    end
	    end
    end;
stored_keys(_,{'EXIT',{aborted,R}},_,_,_,_) ->
    abort(R);
stored_keys(Tab,Key,Prev,#tidstore{store=Store},Op,ordered_set) ->
    case ?ets_match(Store, {{Tab, Key}, '_', '$1'}) of
	[] ->
	    Keys = ts_keys(Store,Tab,Op,ordered_set,[Key]),
	    get_ordered_tskey(Prev,Keys,Op);
 	Ops ->
	    case lists:last(Ops) of
		[delete] ->
	 	    mnesia:Op(Tab,Key);
		_ ->
		    Keys = ts_keys(Store,Tab,Op,ordered_set,[Key]),
		    get_ordered_tskey(Prev,Keys,Op)
	    end
    end;
stored_keys(Tab,Key,_,#tidstore{store=Store},Op,_) ->
    case ?ets_match(Store, {{Tab, Key}, '_', '$1'}) of
	[] ->  Key;
 	Ops ->
	    case lists:last(Ops) of
		[delete] -> mnesia:Op(Tab,Key);
		_ ->      Key
	    end
    end.

get_ordered_tskey('$end_of_table', [First|_],_) ->    First;
get_ordered_tskey(Prev, [First|_], next) when Prev < First -> First;
get_ordered_tskey(Prev, [First|_], prev) when Prev > First -> First;
get_ordered_tskey(Prev, [_|R],Op) ->  get_ordered_tskey(Prev,R,Op);
get_ordered_tskey(_, [],_) ->    '$end_of_table'.

get_next_tskey(Key,Keys,Tab) ->
    Next =
	if Key == '$end_of_table' -> hd(Keys);
	   true ->
		case lists:dropwhile(fun(A) -> A /= Key end, Keys) of
		    [] -> hd(Keys); %% First stored key
		    [Key] -> '$end_of_table';
		    [Key,Next2|_] -> Next2
		end
	end,
    case Next of
	'$end_of_table' -> '$end_of_table';
	_ -> %% Really slow anybody got another solution??
	    case dirty_read(Tab, Next) of
		[] -> Next;
		_ ->
		    %% Updated value we already returned this key
		    get_next_tskey(Next,Keys,Tab)
	    end
    end.

ts_keys(Store, Tab, Op, Type, Def) ->
    All = ?ets_match(Store, {{Tab,'$1'},'_','$2'}),
    Keys = ts_keys_1(All, Def),
    if
	Type == ordered_set, Op == prev ->
	    lists:reverse(lists:sort(Keys));
	Type == ordered_set ->
	    lists:sort(Keys);
	Op == next ->
	    lists:reverse(Keys);
	true ->
	    Keys
    end.

ts_keys_1([[Key, write]|R], []) ->
    ts_keys_1(R, [Key]);
ts_keys_1([[Key, write]|R], Acc=[Key|_]) ->
    ts_keys_1(R, Acc);
ts_keys_1([[Key, write]|R], Acc) ->
    ts_keys_1(R, [Key|Acc]);
ts_keys_1([[Key, delete]|R], [Key|Acc]) ->
    ts_keys_1(R, Acc);
ts_keys_1([_|R], Acc) ->
    ts_keys_1(R, Acc);
ts_keys_1([], Acc) ->
    Acc.


%%%%%%%%%%%%%%%%%%%%%
%% Iterators

-spec foldl(Fun, Acc0, Tab::table()) -> Acc when
      Fun::fun((Record::tuple(), Acc0) -> Acc).
foldl(Fun, Acc, Tab) ->
    foldl(Fun, Acc, Tab, read).

foldl(Fun, Acc, Tab, LockKind) when is_function(Fun) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    foldl(Tid, Ts, Fun, Acc, Tab, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:foldl(Tid, Ts, Fun, Acc, Tab, LockKind);
	_ ->
	    abort(no_transaction)
    end.

foldl(ActivityId, Opaque, Fun, Acc, Tab, LockKind) ->
    {Type, Prev} = init_iteration(ActivityId, Opaque, Tab, LockKind),
    Res = ?CATCH(do_foldl(ActivityId, Opaque, Tab, dirty_first(Tab), Fun, Acc, Type, Prev)),
    close_iteration(Res, Tab).

do_foldl(A, O, Tab, '$end_of_table', Fun, RAcc, _Type, Stored) ->
    lists:foldl(fun(Key, Acc) ->
			lists:foldl(Fun, Acc, read(A, O, Tab, Key, read))
		end, RAcc, Stored);
do_foldl(A, O, Tab, Key, Fun, Acc, ordered_set, [H | Stored]) when H == Key ->
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, Key, read)),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldl(Tid, Ts, Tab, dirty_next(Tab, Key), Fun, NewAcc, ordered_set, Stored);
do_foldl(A, O, Tab, Key, Fun, Acc, ordered_set, [H | Stored]) when H < Key ->
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, H, read)),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldl(Tid, Ts, Tab, Key, Fun, NewAcc, ordered_set, Stored);
do_foldl(A, O, Tab, Key, Fun, Acc, ordered_set, [H | Stored]) when H > Key ->
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, Key, read)),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldl(Tid, Ts, Tab, dirty_next(Tab, Key), Fun, NewAcc, ordered_set, [H |Stored]);
do_foldl(A, O, Tab, Key, Fun, Acc, Type, Stored) ->  %% Type is set or bag
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, Key, read)),
    NewStored = ordsets:del_element(Key, Stored),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldl(Tid, Ts, Tab, dirty_next(Tab, Key), Fun, NewAcc, Type, NewStored).

-spec foldr(Fun, Acc0, Tab::table()) -> Acc when
      Fun::fun((Record::tuple(), Acc0) -> Acc).
foldr(Fun, Acc, Tab) ->
    foldr(Fun, Acc, Tab, read).
foldr(Fun, Acc, Tab, LockKind) when is_function(Fun) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    foldr(Tid, Ts, Fun, Acc, Tab, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:foldr(Tid, Ts, Fun, Acc, Tab, LockKind);
	_ ->
	    abort(no_transaction)
    end.

foldr(ActivityId, Opaque, Fun, Acc, Tab, LockKind) ->
    {Type, TempPrev} = init_iteration(ActivityId, Opaque, Tab, LockKind),
    Prev =
	if
	    Type == ordered_set ->
		lists:reverse(TempPrev);
	    true ->      %% Order doesn't matter for set and bag
		TempPrev %% Keep the order so we can use ordsets:del_element
	end,
    Res = ?CATCH(do_foldr(ActivityId, Opaque, Tab, dirty_last(Tab), Fun, Acc, Type, Prev)),
    close_iteration(Res, Tab).

do_foldr(A, O, Tab, '$end_of_table', Fun, RAcc, _Type, Stored) ->
    lists:foldl(fun(Key, Acc) ->
			lists:foldl(Fun, Acc, read(A, O, Tab, Key, read))
		end, RAcc, Stored);
do_foldr(A, O, Tab, Key, Fun, Acc, ordered_set, [H | Stored]) when H == Key ->
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, Key, read)),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldr(Tid, Ts, Tab, dirty_prev(Tab, Key), Fun, NewAcc, ordered_set, Stored);
do_foldr(A, O, Tab, Key, Fun, Acc, ordered_set, [H | Stored]) when H > Key ->
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, H, read)),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldr(Tid, Ts, Tab, Key, Fun, NewAcc, ordered_set, Stored);
do_foldr(A, O, Tab, Key, Fun, Acc, ordered_set, [H | Stored]) when H < Key ->
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, Key, read)),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldr(Tid, Ts, Tab, dirty_prev(Tab, Key), Fun, NewAcc, ordered_set, [H |Stored]);
do_foldr(A, O, Tab, Key, Fun, Acc, Type, Stored) ->  %% Type is set or bag
    NewAcc = lists:foldl(Fun, Acc, read(A, O, Tab, Key, read)),
    NewStored = ordsets:del_element(Key, Stored),
    {_, Tid, Ts} = get(mnesia_activity_state),
    do_foldr(Tid, Ts, Tab, dirty_prev(Tab, Key), Fun, NewAcc, Type, NewStored).

init_iteration(ActivityId, Opaque, Tab, LockKind) ->
    lock(ActivityId, Opaque, {table, Tab}, LockKind),
    Type = val({Tab, setorbag}),
    Previous = add_previous(ActivityId, Opaque, Type, Tab),
    St = val({Tab, storage_type}),
    if
	St == unknown ->
	    ignore;
	true ->
	    mnesia_lib:db_fixtable(St, Tab, true)
    end,
    {Type, Previous}.

close_iteration(Res, Tab) ->
    case val({Tab, storage_type}) of
	unknown ->
	    ignore;
	St ->
	    mnesia_lib:db_fixtable(St, Tab, false)
    end,
    case Res of
	{'EXIT', {aborted, What}} ->
	   abort(What);
	{'EXIT', What} ->
	    abort(What);
	_ ->
	    Res
    end.

add_previous(_ActivityId, non_transaction, _Type, _Tab) ->
    [];
add_previous(_Tid, Ts, _Type, Tab) ->
    Previous = ?ets_match(Ts#tidstore.store, {{Tab, '$1'}, '_', write}),
    lists:sort(lists:concat(Previous)).

%% This routine fixes up the return value from read/1 so that
%% it is correct with respect to what this particular transaction
%% has already written, deleted .... etc
%% The actual read from the table is not done if not needed due to local
%% transaction context, and if so, no extra read lock is needed either.

add_written([], _Tab, ObjsFun, _LockKind) ->
    ObjsFun();  % standard normal fast case
add_written(Written, Tab, ObjsFun, LockKind) ->
    case val({Tab, setorbag}) of
	bag ->
	    add_written_to_bag(Written, ObjsFun(), []);
        _ when LockKind == read;
               LockKind == write ->
	    add_written_to_set(Written);
	_   ->
            _ = ObjsFun(),  % Fall back to request new lock and read from source
	    add_written_to_set(Written)
    end.

add_written_to_set(Ws) ->
    case lists:last(Ws) of
	{_, _, delete} -> [];
	{_, Val, write} -> [Val];
	{_, _, delete_object} -> []
    end.

add_written_to_bag([{_, Val, write} | Tail], Objs, Ack) ->
    add_written_to_bag(Tail, lists:delete(Val, Objs), [Val | Ack]);
add_written_to_bag([], Objs, Ack) ->
    Objs ++ lists:reverse(Ack); %% Oldest write first as in ets
add_written_to_bag([{_, _ , delete} | Tail], _Objs, _Ack) ->
    %% This transaction just deleted all objects
    %% with this key
    add_written_to_bag(Tail, [], []);
add_written_to_bag([{_, Val, delete_object} | Tail], Objs, Ack) ->
    add_written_to_bag(Tail, lists:delete(Val, Objs), lists:delete(Val, Ack)).

-spec match_object(Pattern::tuple()) -> [Record::tuple()].
match_object(Pat) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    match_object(Tab, Pat, read);
match_object(Pat) ->
    abort({bad_type, Pat}).

-spec match_object(Tab,Pattern,LockKind) -> [Record] when
      Tab::table(),Pattern::tuple(),LockKind::lock_kind(),Record::tuple().
match_object(Tab, Pat, LockKind) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    match_object(Tid, Ts, Tab, Pat, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:match_object(Tid, Ts, Tab, Pat, LockKind);
	_ ->
	    abort(no_transaction)
    end.

match_object(Tid, Ts, Tab, Pat, LockKind)
  when is_atom(Tab), Tab /= schema, is_tuple(Pat), tuple_size(Pat) > 2 ->
    case element(1, Tid) of
	ets ->
	    mnesia_lib:db_match_object(ram_copies, Tab, Pat);
	tid ->
	    Key = element(2, Pat),
	    case has_var(Key) of
		false -> lock_record(Tid, Ts, Tab, Key, LockKind);
		true  -> lock_table(Tid, Ts, Tab, LockKind)
	    end,
	    Objs = dirty_match_object(Tab, Pat),
	    add_written_match(Ts#tidstore.store, Pat, Tab, Objs);
	_Protocol ->
	    dirty_match_object(Tab, Pat)
    end;
match_object(_Tid, _Ts, Tab, Pat, _LockKind) ->
    abort({bad_type, Tab, Pat}).

add_written_index(Store, Pos, Tab, Key, Objs) when is_integer(Pos) ->
    Pat = setelement(Pos, val({Tab, wild_pattern}), Key),
    add_written_match(Store, Pat, Tab, Objs);
add_written_index(Store, Pos, Tab, Key, Objs) when is_tuple(Pos) ->
    IxF = mnesia_index:index_vals_f(val({Tab, storage_type}), Tab, Pos),
    Ops = find_ops(Store, Tab, '_'),
    add_ix_match(Ops, Objs, IxF, Key, val({Tab, setorbag})).

add_written_match(S, Pat, Tab, Objs) ->
    Ops = find_ops(S, Tab, Pat),
    FixedRes = add_match(Ops, Objs, val({Tab, setorbag})),
    MS = ets:match_spec_compile([{Pat, [], ['$_']}]),
    ets:match_spec_run(FixedRes, MS).

find_ops(S, Tab, Pat) ->
    GetWritten = [{{{Tab, '_'}, '_', write}, [], ['$_']},
		  {{{Tab, '_'}, '_', delete}, [], ['$_']},
		  {{{Tab, '_'}, Pat, delete_object}, [], ['$_']}],
    ets:select(S, GetWritten).

add_match([], Objs, _Type) ->
    Objs;
add_match(Written, Objs, ordered_set) ->
    %% Must use keysort which is stable
    add_ordered_match(lists:keysort(1,Written), Objs, []);
add_match([{Oid, _, delete}|R], Objs, Type) ->
    add_match(R, deloid(Oid, Objs), Type);
add_match([{_Oid, Val, delete_object}|R], Objs, Type) ->
    add_match(R, lists:delete(Val, Objs), Type);
add_match([{_Oid, Val, write}|R], Objs, bag) ->
    add_match(R, [Val | lists:delete(Val, Objs)], bag);
add_match([{Oid, Val, write}|R], Objs, set) ->
    add_match(R, [Val | deloid(Oid,Objs)],set).

add_ix_match([], Objs, _IxF, _Key, _Type) ->
    Objs;
add_ix_match(Written, Objs, IxF, Key, ordered_set) ->
    %% Must use keysort which is stable
    add_ordered_match(lists:keysort(1, ix_filter_ops(IxF, Key, Written)), Objs, []);
add_ix_match([{Oid, _, delete}|R], Objs, IxF, Key, Type) ->
    add_ix_match(R, deloid(Oid, Objs), IxF, Key, Type);
add_ix_match([{_Oid, Val, delete_object}|R], Objs, IxF, Key, Type) ->
    case ix_match(Val, IxF, Key) of
        true ->
            add_ix_match(R, lists:delete(Val, Objs), IxF, Key, Type);
        false ->
            add_ix_match(R, Objs, IxF, Key, Type)
    end;
add_ix_match([{_Oid, Val, write}|R], Objs, IxF, Key, bag) ->
    case ix_match(Val, IxF, Key) of
        true ->
            add_ix_match(R, [Val | lists:delete(Val, Objs)], IxF, Key, bag);
        false ->
            add_ix_match(R, Objs, IxF, Key, bag)
    end;
add_ix_match([{Oid, Val, write}|R], Objs, IxF, Key, set) ->
    case ix_match(Val, IxF, Key) of
        true ->
            add_ix_match(R, [Val | deloid(Oid,Objs)],IxF,Key,set);
        false ->
            add_ix_match(R, Objs, IxF, Key, set)
    end.

ix_match(Val, IxF, Key) ->
    lists:member(Key, IxF(Val)).

ix_filter_ops(IxF, Key, Ops) ->
    lists:filter(
      fun({_Oid, Obj, write}) ->
              ix_match(Obj, IxF, Key);
         (_) ->
              true
      end, Ops).

%% For ordered_set only !!
add_ordered_match(Written = [{{_, Key}, _, _}|_], [Obj|Objs], Acc)
  when Key > element(2, Obj) ->
    add_ordered_match(Written, Objs, [Obj|Acc]);
add_ordered_match([{{_, Key}, Val, write}|Rest], Objs =[Obj|_], Acc)
  when Key < element(2, Obj) ->
    add_ordered_match(Rest, [Val|Objs],Acc);
add_ordered_match([{{_, Key}, _, _DelOP}|Rest], Objs =[Obj|_], Acc)
  when Key < element(2, Obj) ->
    add_ordered_match(Rest,Objs,Acc);
%% Greater than last object
add_ordered_match([{_, Val, write}|Rest], [], Acc) ->
    add_ordered_match(Rest, [Val], Acc);
add_ordered_match([_|Rest], [], Acc) ->
    add_ordered_match(Rest, [], Acc);
%% Keys are equal from here
add_ordered_match([{_, Val, write}|Rest], [_Obj|Objs], Acc) ->
    add_ordered_match(Rest, [Val|Objs], Acc);
add_ordered_match([{_, _Val, delete}|Rest], [_Obj|Objs], Acc) ->
    add_ordered_match(Rest, Objs, Acc);
add_ordered_match([{_, Val, delete_object}|Rest], [Val|Objs], Acc) ->
    add_ordered_match(Rest, Objs, Acc);
add_ordered_match([{_, _, delete_object}|Rest], Objs, Acc) ->
    add_ordered_match(Rest, Objs, Acc);
add_ordered_match([], Objs, Acc) ->
    lists:reverse(Acc, Objs).

%% For select chunk
add_sel_match(Sorted, Objs, ordered_set) ->
    add_sel_ordered_match(Sorted, Objs, []);
add_sel_match(Written, Objs, Type) ->
    add_sel_match(Written, Objs, Type, []).

add_sel_match([], Objs, _Type, Acc) ->
    {Objs,lists:reverse(Acc)};
add_sel_match([Op={Oid, _, delete}|R], Objs, Type, Acc) ->
    case deloid(Oid, Objs) of
	Objs ->
	    add_sel_match(R, Objs, Type, [Op|Acc]);
	NewObjs when Type == set ->
	    add_sel_match(R, NewObjs, Type, Acc);
	NewObjs ->  %% If bag we may get more in next chunk
	    add_sel_match(R, NewObjs, Type, [Op|Acc])
    end;
add_sel_match([Op = {_Oid, Val, delete_object}|R], Objs, Type, Acc) ->
    case lists:delete(Val, Objs) of
	Objs ->
	    add_sel_match(R, Objs, Type, [Op|Acc]);
	NewObjs when Type == set ->
	    add_sel_match(R, NewObjs, Type, Acc);
	NewObjs ->
	    add_sel_match(R, NewObjs, Type, [Op|Acc])
    end;
add_sel_match([Op={Oid={_,Key}, Val, write}|R], Objs, bag, Acc) ->
    case lists:keymember(Key, 2, Objs) of
	true ->
	    add_sel_match(R,[Val|lists:delete(Val,Objs)],bag,
			  [{Oid,Val,delete_object}|Acc]);
	false ->
	    add_sel_match(R,Objs,bag,[Op|Acc])
    end;
add_sel_match([Op={Oid, Val, write}|R], Objs, set, Acc) ->
    case deloid(Oid,Objs) of
	Objs ->
	    add_sel_match(R, Objs,set, [Op|Acc]);
	NewObjs ->
	    add_sel_match(R, [Val | NewObjs],set, Acc)
    end.

%% For ordered_set only !!
add_sel_ordered_match(Written = [{{_, Key}, _, _}|_], [Obj|Objs],Acc)
  when Key > element(2, Obj) ->
    add_sel_ordered_match(Written, Objs, [Obj|Acc]);
add_sel_ordered_match([{{_, Key}, Val, write}|Rest], Objs =[Obj|_],Acc)
  when Key < element(2, Obj) ->
    add_sel_ordered_match(Rest,[Val|Objs],Acc);
add_sel_ordered_match([{{_, Key}, _, _DelOP}|Rest], Objs =[Obj|_], Acc)
  when Key < element(2, Obj) ->
    add_sel_ordered_match(Rest,Objs,Acc);
%% Greater than last object
add_sel_ordered_match(Ops1, [], Acc) ->
    {lists:reverse(Acc), Ops1};
%% Keys are equal from here
add_sel_ordered_match([{_, Val, write}|Rest], [_Obj|Objs], Acc) ->
    add_sel_ordered_match(Rest, [Val|Objs], Acc);
add_sel_ordered_match([{_, _Val, delete}|Rest], [_Obj|Objs], Acc) ->
    add_sel_ordered_match(Rest, Objs, Acc);
add_sel_ordered_match([{_, Val, delete_object}|Rest], [Val|Objs], Acc) ->
    add_sel_ordered_match(Rest, Objs, Acc);
add_sel_ordered_match([{_, _, delete_object}|Rest], Objs, Acc) ->
    add_sel_ordered_match(Rest, Objs, Acc);
add_sel_ordered_match([], Objs, Acc) ->
    {lists:reverse(Acc, Objs),[]}.


deloid(_Oid, []) ->
    [];
deloid({Tab, Key}, [H | T]) when element(2, H) == Key ->
    deloid({Tab, Key}, T);
deloid(Oid, [H | T]) ->
    [H | deloid(Oid, T)].

%%%%%%%%%%%%%%%%%%
% select
-spec select(Tab, Spec) -> [Match] when
      Tab::table(), Spec::ets:match_spec(), Match::term().
select(Tab, Pat) ->
    select(Tab, Pat, read).
-spec select(Tab, Spec, LockKind) -> [Match] when
      Tab::table(), Spec::ets:match_spec(),
      Match::term(),LockKind::lock_kind().
select(Tab, Pat, LockKind)
  when is_atom(Tab), Tab /= schema, is_list(Pat) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    select(Tid, Ts, Tab, Pat, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:select(Tid, Ts, Tab, Pat, LockKind);
	_ ->
	    abort(no_transaction)
    end;
select(Tab, Pat, _Lock) ->
    abort({badarg, Tab, Pat}).

select(Tid, Ts, Tab, Spec, LockKind) ->
    SelectFun = fun(FixedSpec) -> dirty_select(Tab, FixedSpec) end,
    fun_select(Tid, Ts, Tab, Spec, LockKind, Tab, SelectFun).

fun_select(Tid, Ts, Tab, Spec, LockKind, TabPat, SelectFun) ->
    case element(1, Tid) of
	ets ->
	    mnesia_lib:db_select(ram_copies, Tab, Spec);
	tid ->
	    select_lock(Tid,Ts,LockKind,Spec,Tab),
	    Store = Ts#tidstore.store,
	    Written = ?ets_match_object(Store, {{TabPat, '_'}, '_', '_'}),
	    case Written of
		[] ->
		    %% Nothing changed in the table during this transaction,
		    %% Simple case get results from [d]ets
		    SelectFun(Spec);
		_ ->
		    %% Hard (slow case) records added or deleted earlier
		    %% in the transaction, have to cope with that.
		    Type = val({Tab, setorbag}),
		    FixedSpec = get_record_pattern(Spec),
		    TabRecs = SelectFun(FixedSpec),
		    FixedRes = add_match(Written, TabRecs, Type),
		    CMS = ets:match_spec_compile(Spec),
		    ets:match_spec_run(FixedRes, CMS)
	    end;
	_Protocol ->
	    SelectFun(Spec)
    end.

select_lock(Tid,Ts,LockKind,Spec,Tab) ->
    %% Avoid table lock if possible
    case Spec of
	[{HeadPat,_, _}] when is_tuple(HeadPat), tuple_size(HeadPat) > 2 ->
	    Key = element(2, HeadPat),
	    case has_var(Key) of
		false -> lock_record(Tid, Ts, Tab, Key, LockKind);
		true  -> lock_table(Tid, Ts, Tab, LockKind)
	    end;
	_ ->
	    lock_table(Tid, Ts, Tab, LockKind)
    end.

%% Breakable Select
-spec select(Tab, Spec, N, LockKind) -> {[Match], Cont} | '$end_of_table' when
      Tab::table(), Spec::ets:match_spec(),
      Match::term(), N::non_neg_integer(),
      LockKind::lock_kind(),
      Cont::select_continuation().
select(Tab, Pat, NObjects, LockKind)
  when is_atom(Tab), Tab /= schema, is_list(Pat), is_integer(NObjects) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    select(Tid, Ts, Tab, Pat, NObjects, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:select(Tid, Ts, Tab, Pat, NObjects, LockKind);
	_ ->
	    abort(no_transaction)
    end;
select(Tab, Pat, NObjects, _Lock) ->
    abort({badarg, Tab, Pat, NObjects}).

select(Tid, Ts, Tab, Spec, NObjects, LockKind) ->
    Where = val({Tab,where_to_read}),
    Type = mnesia_lib:storage_type_at_node(Where,Tab),
    InitFun = fun(FixedSpec) -> dirty_sel_init(Where,Tab,FixedSpec,NObjects,Type) end,
    fun_select(Tid,Ts,Tab,Spec,LockKind,Tab,InitFun,NObjects,Where,Type).

-record(mnesia_select, {tab,tid,node,storage,cont,written=[],spec,type,orig}).

fun_select(Tid, Ts, Tab, Spec, LockKind, TabPat, Init, NObjects, Node, Storage) ->
    Def = #mnesia_select{tid=Tid,node=Node,storage=Storage,tab=Tab,orig=Spec},
    case element(1, Tid) of
	ets ->
	    select_state(mnesia_lib:db_select_init(ram_copies,Tab,Spec,NObjects),Def);
	tid ->
	    select_lock(Tid,Ts,LockKind,Spec,Tab),
	    Store = Ts#tidstore.store,
	    do_fixtable(Tab, Store),

	    Written0 = ?ets_match_object(Store, {{TabPat, '_'}, '_', '_'}),
	    case Written0 of
		[] ->
		    %% Nothing changed in the table during this transaction,
		    %% Simple case get results from [d]ets
		    select_state(Init(Spec),Def);
		_ ->
		    %% Hard (slow case) records added or deleted earlier
		    %% in the transaction, have to cope with that.
		    Type = val({Tab, setorbag}),
		    Written =
			if Type == ordered_set -> %% Sort stable
				lists:keysort(1,Written0);
			   true ->
				Written0
			end,
		    FixedSpec = get_record_pattern(Spec),
		    CMS = ets:match_spec_compile(Spec),
		    trans_select(Init(FixedSpec),
				 Def#mnesia_select{written=Written,spec=CMS,type=Type, orig=FixedSpec})
	    end;
	_Protocol ->
	    select_state(Init(Spec),Def)
    end.

-spec select(Cont) -> {[Match], Cont} | '$end_of_table' when
      Match::term(),
      Cont::select_continuation().
select(Cont) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    select_cont(Tid,Ts,Cont);
	{Mod, Tid, Ts} ->
	    Mod:select_cont(Tid,Ts,Cont);
	_ ->
	    abort(no_transaction)
    end.

select_cont(_Tid,_Ts,'$end_of_table') ->
    '$end_of_table';
select_cont(Tid,_Ts,State=#mnesia_select{tid=Tid,cont=Cont, orig=Ms})
  when element(1,Tid) == ets ->
    case Cont of
	'$end_of_table' -> '$end_of_table';
	_ -> select_state(mnesia_lib:db_select_cont(ram_copies,Cont,Ms),State)
    end;
select_cont(Tid,_,State=#mnesia_select{tid=Tid,written=[]}) ->
    select_state(dirty_sel_cont(State),State);
select_cont(Tid,_Ts,State=#mnesia_select{tid=Tid})  ->
    trans_select(dirty_sel_cont(State), State);
select_cont(Tid2,_,#mnesia_select{tid=_Tid1})
  when element(1,Tid2) == tid ->  % Mismatching tids
    abort(wrong_transaction);
select_cont(Tid,Ts,State=#mnesia_select{}) ->
    % Repair mismatching tids in non-transactional contexts
    RepairedState = State#mnesia_select{tid = Tid, written = [],
                                        spec = undefined, type = undefined},
    select_cont(Tid,Ts,RepairedState);
select_cont(_,_,Cont) ->
    abort({badarg, Cont}).

trans_select('$end_of_table', #mnesia_select{written=Written0,spec=CMS,type=Type}) ->
    Written = add_match(Written0, [], Type),
    {ets:match_spec_run(Written, CMS), '$end_of_table'};
trans_select({TabRecs,Cont}, State = #mnesia_select{written=Written0,spec=CMS,type=Type}) ->
    {FixedRes,Written} = add_sel_match(Written0, TabRecs, Type),
    select_state({ets:match_spec_run(FixedRes, CMS),Cont},
		 State#mnesia_select{written=Written}).

select_state({Matches, Cont}, MS) ->
    {Matches, MS#mnesia_select{cont=Cont}};
select_state('$end_of_table',_) -> '$end_of_table'.

get_record_pattern([]) ->    [];
get_record_pattern([{M,C,_B}|R]) ->
    [{M,C,['$_']} | get_record_pattern(R)].

-spec all_keys(Tab::table()) -> [Key::term()].
all_keys(Tab) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    all_keys(Tid, Ts, Tab, read);
	{Mod, Tid, Ts} ->
	    Mod:all_keys(Tid, Ts, Tab, read);
	_ ->
	    abort(no_transaction)
    end.

all_keys(Tid, Ts, Tab, LockKind)
  when is_atom(Tab), Tab /= schema ->
    Pat0 = val({Tab, wild_pattern}),
    Pat = setelement(2, Pat0, '$1'),
    Keys = select(Tid, Ts, Tab, [{Pat, [], ['$1']}], LockKind),
    case val({Tab, setorbag}) of
	bag ->
	    mnesia_lib:uniq(Keys);
	_ ->
	    Keys
    end;
all_keys(_Tid, _Ts, Tab, _LockKind) ->
    abort({bad_type, Tab}).

-spec index_match_object(Pattern, Attr) -> [Record] when
      Pattern::tuple(), Attr::index_attr(), Record::tuple().
index_match_object(Pat, Attr) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    index_match_object(Tab, Pat, Attr, read);
index_match_object(Pat, _Attr) ->
    abort({bad_type, Pat}).

-spec index_match_object(Tab, Pattern, Attr, LockKind) -> [Record] when
      Tab::table(),
      Pattern::tuple(),
      Attr::index_attr(),
      LockKind::lock_kind(),
      Record::tuple().
index_match_object(Tab, Pat, Attr, LockKind) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    index_match_object(Tid, Ts, Tab, Pat, Attr, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:index_match_object(Tid, Ts, Tab, Pat, Attr, LockKind);
	_ ->
	    abort(no_transaction)
    end.

index_match_object(Tid, Ts, Tab, Pat, Attr, LockKind)
  when is_atom(Tab), Tab /= schema, is_tuple(Pat), tuple_size(Pat) > 2 ->
    case element(1, Tid) of
	ets ->
	    dirty_index_match_object(Tab, Pat, Attr); % Should be optimized?
	tid ->
	    case mnesia_schema:attr_tab_to_pos(Tab, Attr) of
                {_} ->
                    case LockKind of
                        read ->
			    Store = Ts#tidstore.store,
			    mnesia_locker:rlock_table(Tid, Store, Tab),
			    Objs = dirty_match_object(Tab, Pat),
			    add_written_match(Store, Pat, Tab, Objs);
                        _ ->
                            abort({bad_type, Tab, LockKind})
                    end;
		Pos when Pos =< tuple_size(Pat) ->
		    case LockKind of
			read ->
			    Store = Ts#tidstore.store,
			    mnesia_locker:rlock_table(Tid, Store, Tab),
			    Objs = dirty_index_match_object(Tab, Pat, Attr),
			    add_written_match(Store, Pat, Tab, Objs);
			_ ->
			    abort({bad_type, Tab, LockKind})
		    end;
		BadPos ->
		    abort({bad_type, Tab, BadPos})
	    end;
	_Protocol ->
	    dirty_index_match_object(Tab, Pat, Attr)
    end;
index_match_object(_Tid, _Ts, Tab, Pat, _Attr, _LockKind) ->
    abort({bad_type, Tab, Pat}).

-spec index_read(Tab, Key, Attr) -> [Record] when
      Tab::table(),
      Key::term(),
      Attr::index_attr(),
      Record::tuple().
index_read(Tab, Key, Attr) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    index_read(Tid, Ts, Tab, Key, Attr, read);
	{Mod, Tid, Ts} ->
	    Mod:index_read(Tid, Ts, Tab, Key, Attr, read);
	_ ->
	    abort(no_transaction)
    end.

index_read(Tid, Ts, Tab, Key, Attr, LockKind)
  when is_atom(Tab), Tab /= schema ->
    case element(1, Tid) of
	ets ->
	    dirty_index_read(Tab, Key, Attr); % Should be optimized?
	tid ->
	    Pos = mnesia_schema:attr_tab_to_pos(Tab, Attr),
	    case LockKind of
		read ->
		    case has_var(Key) of
			false ->
			    Store = Ts#tidstore.store,
			    Objs = mnesia_index:read(Tid, Store, Tab, Key, Pos),
                            add_written_index(
                              Ts#tidstore.store, Pos, Tab, Key, Objs);
			true ->
			    abort({bad_type, Tab, Attr, Key})
		    end;
		_ ->
		    abort({bad_type, Tab, LockKind})
	    end;
	_Protocol ->
	    dirty_index_read(Tab, Key, Attr)
    end;
index_read(_Tid, _Ts, Tab, _Key, _Attr, _LockKind) ->
    abort({bad_type, Tab}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dirty access regardless of activities - updates
-spec dirty_write(Record::tuple()) -> 'ok'.
dirty_write(Val) when is_tuple(Val), tuple_size(Val) > 2  ->
    Tab = element(1, Val),
    dirty_write(Tab, Val);
dirty_write(Val) ->
    abort({bad_type, Val}).

-spec dirty_write(Tab::table(), Record::tuple()) -> 'ok'.
dirty_write(Tab, Val) ->
    do_dirty_write(async_dirty, Tab, Val).

do_dirty_write(SyncMode, Tab, Val)
  when is_atom(Tab), Tab /= schema, is_tuple(Val), tuple_size(Val) > 2 ->
    {_, _, _} = mnesia_lib:validate_record(Tab, Val),
    Oid = {Tab, element(2, Val)},
    mnesia_tm:dirty(SyncMode, {Oid, Val, write});
do_dirty_write(_SyncMode, Tab, Val) ->
    abort({bad_type, Tab, Val}).

-spec dirty_delete({Tab::table(), Key::_}) -> 'ok'.
dirty_delete({Tab, Key}) ->
    dirty_delete(Tab, Key);
dirty_delete(Oid) ->
    abort({bad_type, Oid}).

-spec dirty_delete(Tab::table(), Key::_) -> 'ok'.
dirty_delete(Tab, Key) ->
    do_dirty_delete(async_dirty, Tab, Key).

do_dirty_delete(SyncMode, Tab, Key) when is_atom(Tab), Tab /= schema  ->
    Oid = {Tab, Key},
    mnesia_tm:dirty(SyncMode, {Oid, Oid, delete});
do_dirty_delete(_SyncMode, Tab, _Key) ->
    abort({bad_type, Tab}).

-spec dirty_delete_object(Record::tuple()) -> 'ok'.
dirty_delete_object(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    dirty_delete_object(Tab, Val);
dirty_delete_object(Val) ->
    abort({bad_type, Val}).

-spec dirty_delete_object(Tab::table(), Record::tuple()) -> 'ok'.
dirty_delete_object(Tab, Val) ->
    do_dirty_delete_object(async_dirty, Tab, Val).

do_dirty_delete_object(SyncMode, Tab, Val)
    when is_atom(Tab), Tab /= schema, is_tuple(Val), tuple_size(Val) > 2 ->
    Oid = {Tab, element(2, Val)},
    case has_var(Val) of
	false ->
	    mnesia_tm:dirty(SyncMode, {Oid, Val, delete_object});
	true ->
	    abort({bad_type, Tab, Val})
    end;

do_dirty_delete_object(_SyncMode, Tab, Val) ->
    abort({bad_type, Tab, Val}).

%% A Counter is an Oid being {CounterTab, CounterName}
-spec dirty_update_counter({Tab::table(), Key::_}, Incr::integer()) ->
                                  NewVal::integer().
dirty_update_counter({Tab, Key}, Incr) ->
    dirty_update_counter(Tab, Key, Incr);
dirty_update_counter(Counter, _Incr) ->
    abort({bad_type, Counter}).

-spec dirty_update_counter(Tab::table(), Key::_, Incr::integer()) ->
                                  NewVal::integer().
dirty_update_counter(Tab, Key, Incr) ->
    do_dirty_update_counter(async_dirty, Tab, Key, Incr).

do_dirty_update_counter(SyncMode, Tab, Key, Incr)
  when is_atom(Tab), Tab /= schema, is_integer(Incr) ->
    case mnesia_lib:validate_key(Tab, Key) of
	{RecName, 3, Type} when Type == set; Type == ordered_set ->
	    Oid = {Tab, Key},
	    mnesia_tm:dirty(SyncMode, {Oid, {RecName, Incr}, update_counter});
	_ ->
	    abort({combine_error, Tab, update_counter})
    end;
do_dirty_update_counter(_SyncMode, Tab, _Key, Incr) ->
    abort({bad_type, Tab, Incr}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dirty access regardless of activities - read

-spec dirty_read({Tab::table(), Key::_}) -> [tuple()].
dirty_read({Tab, Key}) ->
    dirty_read(Tab, Key);
dirty_read(Oid) ->
    abort({bad_type, Oid}).

-spec dirty_read(Tab::table(), Key::_) -> [tuple()].
dirty_read(Tab, Key)
  when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_get, [Tab, Key]);
dirty_read(Tab, _Key) ->
    abort({bad_type, Tab}).

-spec dirty_match_object(Pattern::tuple()) -> [Record::tuple()].
dirty_match_object(Pat) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    dirty_match_object(Tab, Pat);
dirty_match_object(Pat) ->
    abort({bad_type, Pat}).

-spec dirty_match_object(Tab,Pattern) -> [Record] when
      Tab::table(), Pattern::tuple(), Record::tuple().
dirty_match_object(Tab, Pat)
  when is_atom(Tab), Tab /= schema, is_tuple(Pat), tuple_size(Pat) > 2 ->
    dirty_rpc(Tab, ?MODULE, remote_dirty_match_object, [Tab, Pat]);
dirty_match_object(Tab, Pat) ->
    abort({bad_type, Tab, Pat}).

remote_dirty_match_object(Tab, Pat) ->
    Key = element(2, Pat),
    case has_var(Key) of
	false ->
	    mnesia_lib:db_match_object(Tab, Pat);
	true ->
            PosList = regular_indexes(Tab),
	    remote_dirty_match_object(Tab, Pat, PosList)
    end.

remote_dirty_match_object(Tab, Pat, [Pos | Tail]) when Pos =< tuple_size(Pat) ->
    IxKey = element(Pos, Pat),
    case has_var(IxKey) of
	false ->
	    mnesia_index:dirty_match_object(Tab, Pat, Pos);
	true ->
	    remote_dirty_match_object(Tab, Pat, Tail)
    end;
remote_dirty_match_object(Tab, Pat, []) ->
    mnesia_lib:db_match_object(Tab, Pat);
remote_dirty_match_object(Tab, Pat, _PosList) ->
    abort({bad_type, Tab, Pat}).

-spec dirty_select(Tab, Spec) -> [Match] when
      Tab::table(), Spec::ets:match_spec(), Match::term().
dirty_select(Tab, Spec) when is_atom(Tab), Tab /= schema, is_list(Spec) ->
    dirty_rpc(Tab, ?MODULE, remote_dirty_select, [Tab, Spec]);
dirty_select(Tab, Spec) ->
    abort({bad_type, Tab, Spec}).

remote_dirty_select(Tab, Spec) ->
    case Spec of
	[{HeadPat, _, _}] when is_tuple(HeadPat), tuple_size(HeadPat) > 2 ->
	    Key = element(2, HeadPat),
	    case has_var(Key) of
		false ->
		    mnesia_lib:db_select(Tab, Spec);
		true  ->
		    PosList = regular_indexes(Tab),
		    remote_dirty_select(Tab, Spec, PosList)
	    end;
	_ ->
	    mnesia_lib:db_select(Tab, Spec)
    end.

remote_dirty_select(Tab, [{HeadPat,_, _}] = Spec, [Pos | Tail])
  when is_tuple(HeadPat), tuple_size(HeadPat) > 2, Pos =< tuple_size(HeadPat) ->
    Key = element(Pos, HeadPat),
    case has_var(Key) of
	false ->
	    Recs = mnesia_index:dirty_select(Tab, HeadPat, Pos),
	    %% Returns the records without applying the match spec
	    %% The actual filtering is handled by the caller
	    CMS = ets:match_spec_compile(Spec),
	    case val({Tab, setorbag}) of
		ordered_set ->
		    ets:match_spec_run(lists:sort(Recs), CMS);
		_ ->
		    ets:match_spec_run(Recs, CMS)
	    end;
	true  ->
	    remote_dirty_select(Tab, Spec, Tail)
    end;
remote_dirty_select(Tab, Spec, _) ->
    mnesia_lib:db_select(Tab, Spec).

dirty_sel_init(Node,Tab,Spec,NObjects,Type) ->
    do_dirty_rpc(Tab,Node,mnesia_lib,db_select_init,[Type,Tab,Spec,NObjects]).

dirty_sel_cont(#mnesia_select{cont='$end_of_table'}) -> '$end_of_table';
dirty_sel_cont(#mnesia_select{node=Node,tab=Tab,storage=Type,cont=Cont,orig=Ms}) ->
    do_dirty_rpc(Tab,Node,mnesia_lib,db_select_cont,[Type,Cont,Ms]).

-spec dirty_all_keys(Tab::table()) -> [Key::term()].
dirty_all_keys(Tab) when is_atom(Tab), Tab /= schema ->
    case ?catch_val({Tab, wild_pattern}) of
	{'EXIT', _} ->
	    abort({no_exists, Tab});
	Pat0 ->
	    Pat = setelement(2, Pat0, '$1'),
	    Keys = dirty_select(Tab, [{Pat, [], ['$1']}]),
	    case val({Tab, setorbag}) of
		bag -> mnesia_lib:uniq(Keys);
		_ -> Keys
	    end
    end;
dirty_all_keys(Tab) ->
    abort({bad_type, Tab}).

-spec dirty_index_match_object(Pattern, Attr) -> [Record] when
      Pattern::tuple(), Attr::index_attr(), Record::tuple().
dirty_index_match_object(Pat, Attr) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    dirty_index_match_object(Tab, Pat, Attr);
dirty_index_match_object(Pat, _Attr) ->
    abort({bad_type, Pat}).

-spec dirty_index_match_object(Tab, Pattern, Attr) -> [Record] when
      Tab::table(),
      Pattern::tuple(),
      Attr::index_attr(),
      Record::tuple().
dirty_index_match_object(Tab, Pat, Attr)
  when is_atom(Tab), Tab /= schema, is_tuple(Pat), tuple_size(Pat) > 2 ->
    case mnesia_schema:attr_tab_to_pos(Tab, Attr) of
        {_} ->
            dirty_match_object(Tab, Pat);
	Pos when Pos =< tuple_size(Pat) ->
	    case has_var(element(2, Pat)) of
		false ->
		    dirty_match_object(Tab, Pat);
		true ->
		    Elem = element(Pos, Pat),
		    case has_var(Elem) of
			false ->
			    dirty_rpc(Tab, mnesia_index, dirty_match_object,
				      [Tab, Pat, Pos]);
			true ->
			    abort({bad_type, Tab, Attr, Elem})
		    end
	    end;
	BadPos ->
	    abort({bad_type, Tab, BadPos})
    end;
dirty_index_match_object(Tab, Pat, _Attr) ->
    abort({bad_type, Tab, Pat}).

-spec dirty_index_read(Tab, Key, Attr) -> [Record] when
      Tab::table(),
      Key::term(),
      Attr::index_attr(),
      Record::tuple().
dirty_index_read(Tab, Key, Attr) when is_atom(Tab), Tab /= schema ->
    Pos = mnesia_schema:attr_tab_to_pos(Tab, Attr),
    case has_var(Key) of
	false ->
	    mnesia_index:dirty_read(Tab, Key, Pos);
	true ->
	    abort({bad_type, Tab, Attr, Key})
    end;
dirty_index_read(Tab, _Key, _Attr) ->
    abort({bad_type, Tab}).

%% do not use only for backwards compatibility
dirty_slot(Tab, Slot) when is_atom(Tab), Tab /= schema, is_integer(Slot)  ->
    dirty_rpc(Tab, mnesia_lib, db_slot, [Tab, Slot]);
dirty_slot(Tab, Slot) ->
    abort({bad_type, Tab, Slot}).

-spec dirty_first(Tab::table()) -> Key::term().
dirty_first(Tab) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_first, [Tab]);
dirty_first(Tab) ->
    abort({bad_type, Tab}).

-spec dirty_last(Tab::table()) -> Key::term().
dirty_last(Tab) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_last, [Tab]);
dirty_last(Tab) ->
    abort({bad_type, Tab}).

-spec dirty_next(Tab::table(), Key::_) -> NextKey::term().
dirty_next(Tab, Key) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_next_key, [Tab, Key]);
dirty_next(Tab, _Key) ->
    abort({bad_type, Tab}).

-spec dirty_prev(Tab::table(), Key::_) -> PrevKey::term().
dirty_prev(Tab, Key) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_prev_key, [Tab, Key]);
dirty_prev(Tab, _Key) ->
    abort({bad_type, Tab}).


dirty_rpc(Tab, M, F, Args) ->
    Node = val({Tab, where_to_read}),
    do_dirty_rpc(Tab, Node, M, F, Args).

do_dirty_rpc(_Tab, nowhere, _, _, Args) ->
    mnesia:abort({no_exists, Args});
do_dirty_rpc(Tab, Node, M, F, Args) ->
    case rpc:call(Node, M, F, Args) of
	{badrpc, Reason} ->
	    timer:sleep(20), %% Do not be too eager, and can't use yield on SMP
	    %% Sync with mnesia_monitor
	    _ = try sys:get_status(mnesia_monitor) catch _:_ -> ok end,
	    case mnesia_controller:call({check_w2r, Node, Tab}) of % Sync
		NewNode when NewNode =:= Node ->
		    ErrorTag = mnesia_lib:dirty_rpc_error_tag(Reason),
		    mnesia:abort({ErrorTag, Args});
		NewNode ->
		    case get(mnesia_activity_state) of
			{_Mod, Tid, _Ts} when is_record(Tid, tid) ->
			    %% In order to perform a consistent
			    %% retry of a transaction we need
			    %% to acquire the lock on the NewNode.
			    %% In this context we do neither know
			    %% the kind or granularity of the lock.
			    %% --> Abort the transaction
			    mnesia:abort({node_not_running, Node});
			{error, {node_not_running, _}} ->
			    %% Mnesia is stopping
			    mnesia:abort({no_exists, Args});
			_ ->
			    %% Splendid! A dirty retry is safe
			    %% 'Node' probably went down now
			    %% Let mnesia_controller get broken link message first
			    do_dirty_rpc(Tab, NewNode, M, F, Args)
		    end
	    end;
	Other ->
	    Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Info

%% Info about one table
-spec table_info(Tab::table(), Item::term()) -> Info::term().
table_info(Tab, Item) ->
    case get(mnesia_activity_state) of
	undefined ->
	    any_table_info(Tab, Item);
	{?DEFAULT_ACCESS, _Tid, _Ts} ->
	    any_table_info(Tab, Item);
	{Mod, Tid, Ts} ->
	    Mod:table_info(Tid, Ts, Tab, Item);
	_ ->
	    abort(no_transaction)
    end.

table_info(_Tid, _Ts, Tab, Item) ->
    any_table_info(Tab, Item).


any_table_info(Tab, Item) when is_atom(Tab) ->
    case Item of
	master_nodes ->
	    mnesia_recover:get_master_nodes(Tab);
%	checkpoints ->
%	    case ?catch_val({Tab, commit_work}) of
%		[{checkpoints, List} | _] -> List;
%		No_chk when is_list(No_chk) ->  [];
%		Else -> info_reply(Else, Tab, Item)
%	    end;
	size ->
	    raw_table_info(Tab, Item);
	memory ->
	    raw_table_info(Tab, Item);
	type ->
	    case ?catch_val({Tab, setorbag}) of
		{'EXIT', _} ->
		    abort({no_exists, Tab, Item});
		Val ->
		    Val
	    end;
	all ->
	    case mnesia_schema:get_table_properties(Tab) of
		[] ->
		    abort({no_exists, Tab, Item});
		Props ->
                    Rename = fun ({setorbag, Type}) -> {type, Type};
                                 (Prop) -> Prop
                             end,
                    lists:sort(lists:map(Rename, Props))
	    end;
	name ->
	    Tab;
	_ ->
	    case ?catch_val({Tab, Item}) of
		{'EXIT', _} ->
		    abort({no_exists, Tab, Item});
		Val ->
		    Val
	    end
    end;
any_table_info(Tab, _Item) ->
    abort({bad_type, Tab}).

raw_table_info(Tab, Item) ->
    try
	case ?ets_lookup_element(mnesia_gvar, {Tab, storage_type}, 2) of
	    ram_copies ->
		info_reply(?ets_info(Tab, Item), Tab, Item);
	    disc_copies ->
		info_reply(?ets_info(Tab, Item), Tab, Item);
	    disc_only_copies ->
		info_reply(dets:info(Tab, Item), Tab, Item);
            {ext, Alias, Mod} ->
                info_reply(catch Mod:info(Alias, Tab, Item), Tab, Item);
	    unknown ->
		bad_info_reply(Tab, Item)
	end
    catch error:_ ->
	    bad_info_reply(Tab, Item)
    end.

info_reply({error, _Reason}, Tab, Item) ->
    bad_info_reply(Tab, Item);
info_reply(Val, _Tab, _Item) ->
    Val.

bad_info_reply(_Tab, size) -> 0;
bad_info_reply(_Tab, memory) -> 0;
bad_info_reply(Tab, Item) -> abort({no_exists, Tab, Item}).

%% Raw info about all tables
-spec schema() -> ok.
schema() ->
    mnesia_schema:info().

%% Raw info about one tables
-spec schema(Tab::table()) -> ok.
schema(Tab) ->
    mnesia_schema:info(Tab).

-spec error_description(Error::term()) -> string().
error_description(Err) ->
    mnesia_lib:error_desc(Err).

-spec info() -> ok.
info() ->
    case mnesia_lib:is_running() of
	yes ->
	    TmInfo = mnesia_tm:get_info(10000),
	    Held = system_info(held_locks),
	    Queued = system_info(lock_queue),

	    io:format("---> Processes holding locks <--- ~n", []),
	    lists:foreach(fun(L) -> io:format("Lock: ~p~n", [L]) end,
			  Held),

	    io:format( "---> Processes waiting for locks <--- ~n", []),
	    lists:foreach(fun({Oid, Op, _Pid, Tid, OwnerTid}) ->
				  io:format("Tid ~p waits for ~p lock "
					    "on oid ~p owned by ~p ~n",
					    [Tid, Op, Oid, OwnerTid])
		  end, Queued),
	    mnesia_tm:display_info(group_leader(), TmInfo),

	    Pat = {'_', unclear, '_'},
	    Uncertain = ets:match_object(mnesia_decision, Pat),

	    io:format( "---> Uncertain transactions <--- ~n", []),
	    lists:foreach(fun({Tid, _, Nodes}) ->
				  io:format("Tid ~w waits for decision "
					    "from ~w~n",
					    [Tid, Nodes])
		  end, Uncertain),

	    mnesia_controller:info(),
	    display_system_info(Held, Queued, TmInfo, Uncertain);
	_ ->
	    mini_info()
    end,
    ok.

mini_info() ->
    io:format("===> System info in version ~p, debug level = ~p <===~n",
	      [system_info(version), system_info(debug)]),
    Not =
	case system_info(use_dir) of
	    true -> "";
	    false  -> "NOT "
	end,

    io:format("~w. Directory ~p is ~sused.~n",
	      [system_info(schema_location), system_info(directory), Not]),
    io:format("use fallback at restart = ~w~n",
	      [system_info(fallback_activated)]),
    Running = system_info(running_db_nodes),
    io:format("running db nodes   = ~w~n", [Running]),
    All = mnesia_lib:all_nodes(),
    io:format("stopped db nodes   = ~w ~n", [All -- Running]).

display_system_info(Held, Queued, TmInfo, Uncertain) ->
    mini_info(),
    display_tab_info(),
    S = fun(Items) -> [system_info(I) || I <- Items] end,

    io:format("~w transactions committed, ~w aborted, "
	      "~w restarted, ~w logged to disc~n",
	      S([transaction_commits, transaction_failures,
		transaction_restarts, transaction_log_writes])),

    {Active, Pending} =
	case TmInfo of
	    {timeout, _} -> {infinity, infinity};
	    {info, P, A} -> {length(A), length(P)}
	end,
    io:format("~w held locks, ~w in queue; "
	      "~w local transactions, ~w remote~n",
	      [length(Held), length(Queued), Active, Pending]),

    Ufold = fun({_, _, Ns}, {C, Old}) ->
		    New = [N || N <- Ns, not lists:member(N, Old)],
		    {C + 1, New ++ Old}
	    end,
    {Ucount, Unodes} = lists:foldl(Ufold, {0, []}, Uncertain),
    io:format("~w transactions waits for other nodes: ~p~n",
	      [Ucount, Unodes]).

display_tab_info() ->
    MasterTabs = mnesia_recover:get_master_node_tables(),
    io:format("master node tables = ~p~n", [lists:sort(MasterTabs)]),

    case get_backend_types() of
	[] -> ok;
	Ts -> list_backend_types(Ts, "backend types      = ")
    end,

    case get_index_plugins() of
	[] -> ok;
	Ps -> list_index_plugins(Ps, "index plugins      = ")
    end,

    Tabs = system_info(tables),

    {Unknown, Ram, Disc, DiscOnly, Ext} =
	lists:foldl(fun storage_count/2, {[], [], [], [], []}, Tabs),

    io:format("remote             = ~p~n", [lists:sort(Unknown)]),
    io:format("ram_copies         = ~p~n", [lists:sort(Ram)]),
    io:format("disc_copies        = ~p~n", [lists:sort(Disc)]),
    io:format("disc_only_copies   = ~p~n", [lists:sort(DiscOnly)]),
    [io:format("~-19s= ~p~n", [atom_to_list(A), Ts]) || {A,Ts} <- Ext],

    Rfoldl = fun(T, Acc) ->
		     Rpat =
			 case val({T, access_mode}) of
			     read_only ->
				 lists:sort([{A, read_only} || A <- val({T, active_replicas})]);
			     read_write ->
				 [fix_wtc(W) || W <- table_info(T, where_to_commit)]
			 end,
		     case lists:keysearch(Rpat, 1, Acc) of
			 {value, {_Rpat, Rtabs}} ->
			     lists:keyreplace(Rpat, 1, Acc, {Rpat, [T | Rtabs]});
			 false ->
			     [{Rpat, [T]} | Acc]
		     end
	     end,
    Repl = lists:foldl(Rfoldl, [], Tabs),
    Rdisp = fun({Rpat, Rtabs}) -> io:format("~p = ~p~n", [Rpat, Rtabs]) end,
    lists:foreach(Rdisp, lists:sort(Repl)).

-spec get_backend_types() -> [BackendType::term()].
get_backend_types() ->
    case ?catch_val({schema, user_property, mnesia_backend_types}) of
	{'EXIT', _} ->
	    [];
	{mnesia_backend_types, Ts} ->
	    lists:sort(Ts)
    end.

-spec get_index_plugins() -> [IndexPlugins::term()].
get_index_plugins() ->
    case ?catch_val({schema, user_property, mnesia_index_plugins}) of
	{'EXIT', _} ->
	    [];
	{mnesia_index_plugins, Ps} ->
	    lists:sort(Ps)
    end.


list_backend_types([{A,M} | T] = Ts, Legend) ->
    Indent = [$\s || _ <- Legend],
    W = integer_to_list(
	  lists:foldl(fun({Alias,_}, Wa) ->
			      erlang:max(Wa, length(atom_to_list(Alias)))
		      end, 0, Ts)),
    io:fwrite(Legend ++ "~-" ++ W ++ "s - ~s~n",
	      [atom_to_list(A), atom_to_list(M)]),
    [io:fwrite(Indent ++ "~-" ++ W ++ "s - ~s~n",
	       [atom_to_list(A1), atom_to_list(M1)])
     || {A1,M1} <- T].

list_index_plugins([{N,M,F} | T] = Ps, Legend) ->
    Indent = [$\s || _ <- Legend],
    W = integer_to_list(
	  lists:foldl(fun({N1,_,_}, Wa) ->
			      erlang:max(Wa, length(pp_ix_name(N1)))
		      end, 0, Ps)),
    io:fwrite(Legend ++ "~-" ++ W ++ "s - ~s:~ts~n",
	      [pp_ix_name(N), atom_to_list(M), atom_to_list(F)]),
    [io:fwrite(Indent ++ "~-" ++ W ++ "s - ~s:~ts~n",
	       [pp_ix_name(N1), atom_to_list(M1), atom_to_list(F1)])
     || {N1,M1,F1} <- T].

pp_ix_name(N) ->
    lists:flatten(io_lib:fwrite("~w", [N])).

fix_wtc({N, {ext,A,_}}) -> {N, A};
fix_wtc({N,A}) when is_atom(A) -> {N, A}.

storage_count(T, {U, R, D, DO, Ext}) ->
    case table_info(T, storage_type) of
	unknown -> {[T | U], R, D, DO, Ext};
	ram_copies -> {U, [T | R], D, DO, Ext};
	disc_copies -> {U, R, [T | D], DO, Ext};
	disc_only_copies -> {U, R, D, [T | DO], Ext};
        {ext, A, _} -> {U, R, D, DO, orddict:append(A, T, Ext)}
    end.

-spec system_info(Iterm::term()) -> Info::term().
system_info(Item) ->
    try system_info2(Item)
    catch _:Error -> abort(Error)
    end.

system_info2(all) ->
    Items = system_info_items(mnesia_lib:is_running()),
    [{I, system_info(I)} || I <- Items];

system_info2(db_nodes) ->
    DiscNs = ?catch_val({schema, disc_copies}),
    RamNs = ?catch_val({schema, ram_copies}),
    ExtNs = ?catch_val({schema, external_copies}),
    if
	is_list(DiscNs), is_list(RamNs), is_list(ExtNs) ->
	    DiscNs ++ RamNs ++ ExtNs;
	true ->
	    case mnesia_schema:read_nodes() of
		{ok, Nodes} -> Nodes;
		{error,Reason} -> exit(Reason)
	    end
    end;
system_info2(running_db_nodes) ->
    case ?catch_val({current, db_nodes}) of
	{'EXIT',_} ->
	    %% Ensure that we access the intended Mnesia
	    %% directory. This function may not be called
	    %% during startup since it will cause the
	    %% application_controller to get into deadlock
	    load_mnesia_or_abort(),
	    mnesia_lib:running_nodes();
	Other ->
	    Other
    end;

system_info2(extra_db_nodes) ->
    case ?catch_val(extra_db_nodes) of
	{'EXIT',_} ->
	    %% Ensure that we access the intended Mnesia
	    %% directory. This function may not be called
	    %% during startup since it will cause the
	    %% application_controller to get into deadlock
	    load_mnesia_or_abort(),
	    mnesia_monitor:get_env(extra_db_nodes);
	Other ->
	    Other
    end;

system_info2(directory) ->
    case ?catch_val(directory) of
	{'EXIT',_} ->
	    %% Ensure that we access the intended Mnesia
	    %% directory. This function may not be called
	    %% during startup since it will cause the
	    %% application_controller to get into deadlock
	    load_mnesia_or_abort(),
	    mnesia_monitor:get_env(dir);
	Other ->
	    Other
    end;

system_info2(use_dir) ->
    case ?catch_val(use_dir) of
	{'EXIT',_} ->
	    %% Ensure that we access the intended Mnesia
	    %% directory. This function may not be called
	    %% during startup since it will cause the
	    %% application_controller to get into deadlock
	    load_mnesia_or_abort(),
	    mnesia_monitor:use_dir();
	Other ->
	    Other
    end;

system_info2(schema_location) ->
    case ?catch_val(schema_location) of
	{'EXIT',_} ->
	    %% Ensure that we access the intended Mnesia
	    %% directory. This function may not be called
	    %% during startup since it will cause the
	    %% application_controller to get into deadlock
	    load_mnesia_or_abort(),
	    mnesia_monitor:get_env(schema_location);
	Other ->
	    Other
    end;

system_info2(fallback_activated) ->
    case ?catch_val(fallback_activated) of
	{'EXIT',_} ->
	    %% Ensure that we access the intended Mnesia
	    %% directory. This function may not be called
	    %% during startup since it will cause the
	    %% application_controller to get into deadlock
	    load_mnesia_or_abort(),
	    mnesia_bup:fallback_exists();
	Other ->
	    Other
    end;

system_info2(version) ->
    case ?catch_val(version) of
	{'EXIT', _} ->
	    Apps = application:loaded_applications(),
	    case lists:keysearch(?APPLICATION, 1, Apps) of
		{value, {_Name, _Desc, Version}} ->
		    Version;
		false ->
		    %% Ensure that it does not match
		    {mnesia_not_loaded, node(), erlang:timestamp()}
	    end;
	Version ->
	    Version
    end;

system_info2(access_module) -> mnesia_monitor:get_env(access_module);
system_info2(auto_repair) -> mnesia_monitor:get_env(auto_repair);
system_info2(is_running) -> mnesia_lib:is_running();
system_info2(backup_module) -> mnesia_monitor:get_env(backup_module);
system_info2(backend_types) -> mnesia_schema:backend_types();
system_info2(event_module) -> mnesia_monitor:get_env(event_module);
system_info2(debug) -> mnesia_monitor:get_env(debug);
system_info2(dump_log_load_regulation) -> mnesia_monitor:get_env(dump_log_load_regulation);
system_info2(dump_log_write_threshold) -> mnesia_monitor:get_env(dump_log_write_threshold);
system_info2(dump_log_time_threshold) -> mnesia_monitor:get_env(dump_log_time_threshold);
system_info2(dump_log_update_in_place) ->
    mnesia_monitor:get_env(dump_log_update_in_place);
system_info2(max_wait_for_decision) -> mnesia_monitor:get_env(max_wait_for_decision);
system_info2(ignore_fallback_at_startup) -> mnesia_monitor:get_env(ignore_fallback_at_startup);
system_info2(fallback_error_function) ->  mnesia_monitor:get_env(fallback_error_function);
system_info2(log_version) -> mnesia_log:version();
system_info2(protocol_version) -> mnesia_monitor:protocol_version();
system_info2(schema_version) -> mnesia_schema:version(); %backward compatibility
system_info2(tables) -> val({schema, tables});
system_info2(local_tables) -> val({schema, local_tables});
system_info2(master_node_tables) -> mnesia_recover:get_master_node_tables();
system_info2(subscribers) -> mnesia_subscr:subscribers();
system_info2(checkpoints) -> mnesia_checkpoint:checkpoints();
system_info2(held_locks) -> mnesia_locker:get_held_locks();
system_info2(lock_queue) -> mnesia_locker:get_lock_queue();
system_info2(transactions) -> mnesia_tm:get_transactions();
system_info2(transaction_failures) -> mnesia_lib:read_counter(trans_failures);
system_info2(transaction_commits) -> mnesia_lib:read_counter(trans_commits);
system_info2(transaction_restarts) -> mnesia_lib:read_counter(trans_restarts);
system_info2(transaction_log_writes) -> mnesia_dumper:get_log_writes();
system_info2(core_dir) ->  mnesia_monitor:get_env(core_dir);
system_info2(no_table_loaders) ->  mnesia_monitor:get_env(no_table_loaders);
system_info2(dc_dump_limit) ->  mnesia_monitor:get_env(dc_dump_limit);
system_info2(send_compressed) -> mnesia_monitor:get_env(send_compressed);

system_info2(Item) -> exit({badarg, Item}).

system_info_items(yes) ->
    [
     access_module,
     auto_repair,
     backend_types,
     backup_module,
     checkpoints,
     db_nodes,
     debug,
     directory,
     dump_log_load_regulation,
     dump_log_time_threshold,
     dump_log_update_in_place,
     dump_log_write_threshold,
     event_module,
     extra_db_nodes,
     fallback_activated,
     held_locks,
     ignore_fallback_at_startup,
     fallback_error_function,
     is_running,
     local_tables,
     lock_queue,
     log_version,
     master_node_tables,
     max_wait_for_decision,
     protocol_version,
     running_db_nodes,
     schema_location,
     schema_version,
     subscribers,
     tables,
     transaction_commits,
     transaction_failures,
     transaction_log_writes,
     transaction_restarts,
     transactions,
     use_dir,
     core_dir,
     no_table_loaders,
     dc_dump_limit,
     send_compressed,
     version
    ];
system_info_items(no) ->
    [
     auto_repair,
     backup_module,
     db_nodes,
     debug,
     directory,
     dump_log_load_regulation,
     dump_log_time_threshold,
     dump_log_update_in_place,
     dump_log_write_threshold,
     event_module,
     extra_db_nodes,
     ignore_fallback_at_startup,
     fallback_error_function,
     is_running,
     log_version,
     max_wait_for_decision,
     protocol_version,
     running_db_nodes,
     schema_location,
     schema_version,
     use_dir,
     core_dir,
     version
    ].

system_info() ->
    IsRunning = mnesia_lib:is_running(),
    case IsRunning of
	yes ->
	    TmInfo = mnesia_tm:get_info(10000),
	    Held = system_info(held_locks),
	    Queued = system_info(lock_queue),
	    Pat = {'_', unclear, '_'},
	    Uncertain = ets:match_object(mnesia_decision, Pat),
	    display_system_info(Held, Queued, TmInfo, Uncertain);
	_ ->
	    mini_info()
    end,
    IsRunning.

load_mnesia_or_abort() ->
    case mnesia_lib:ensure_loaded(?APPLICATION) of
	ok ->
	    ok;
	{error, Reason} ->
	    abort(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Database mgt

-spec create_schema(Ns::[node()]) -> 'ok' | {'error', Reason::term()}.
create_schema(Ns) ->
    create_schema(Ns, []).

-spec create_schema(Ns::[node()], [Prop]) -> 'ok' | {'error', Reason::term()} when
      Prop :: BackendType | IndexPlugin,
      BackendType :: {backend_types, [{Name::atom(), Module::module()}]},
      IndexPlugin :: {index_plugins, [{{Name::atom()}, Module::module(), Function::atom()}]}.
create_schema(Ns, Properties) ->
    mnesia_bup:create_schema(Ns, Properties).

-spec delete_schema(Ns::[node()]) -> 'ok' | {'error', Reason::term()}.
delete_schema(Ns) ->
    mnesia_schema:delete_schema(Ns).

-spec add_backend_type(Name::atom(), Module::module()) -> t_result('ok').
add_backend_type(Alias, Module) ->
    mnesia_schema:add_backend_type(Alias, Module).

-spec backup(Dest::term()) -> 'ok' | {'error', Reason::term()}.
backup(Opaque) ->
    mnesia_log:backup(Opaque).

-spec backup(Dest::term(), Mod::module()) ->
                    'ok' | {'error', Reason::term()}.
backup(Opaque, Mod) ->
    mnesia_log:backup(Opaque, Mod).

-spec traverse_backup(Src::term(), Dest::term(), Fun, Acc) ->
                             {'ok', Acc} | {'error', Reason::term()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
traverse_backup(S, T, Fun, Acc) ->
    mnesia_bup:traverse_backup(S, T, Fun, Acc).

-spec traverse_backup(Src::term(), SrcMod::module(),
                      Dest::term(), DestMod::module(),
                      Fun, Acc) ->
                             {'ok', Acc} | {'error', Reason::term()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
traverse_backup(S, SM, T, TM, F, A) ->
    mnesia_bup:traverse_backup(S, SM, T, TM, F, A).

-spec install_fallback(Src::term()) -> 'ok' | {'error', Reason::term()}.
install_fallback(Opaque) ->
    mnesia_bup:install_fallback(Opaque).

-spec install_fallback(Src::term(), Mod::module()|[Opt]) ->
                              'ok' | {'error', Reason::term()} when
      Opt :: Module | Scope | Dir,
      Module :: {'module', Mod::module()},
      Scope :: {'scope', 'global' | 'local'},
      Dir :: {'mnesia_dir', Dir::string()}.
install_fallback(Opaque, Mod) ->
    mnesia_bup:install_fallback(Opaque, Mod).

-spec uninstall_fallback() -> 'ok' | {'error', Reason::term()}.
uninstall_fallback() ->
    mnesia_bup:uninstall_fallback().

-spec uninstall_fallback(Args) -> 'ok' | {'error', Reason::term()} when
      Args :: [{'mnesia_dir', Dir::string()}].
uninstall_fallback(Args) ->
    mnesia_bup:uninstall_fallback(Args).

-spec activate_checkpoint([Arg]) -> {'ok', Name, [node()]} | {'error', Reason::term()} when
      Arg :: {'name', Name} | {'max', [table()]} | {'min', [table()]} |
             {'allow_remote', boolean()} | {'ram_overrides_dump', boolean()}.
activate_checkpoint(Args) ->
    mnesia_checkpoint:activate(Args).

-spec deactivate_checkpoint(Name::_) -> 'ok' | {'error', Reason::term()}.
deactivate_checkpoint(Name) ->
    mnesia_checkpoint:deactivate(Name).

-spec backup_checkpoint(Name::_, Dest::_) -> 'ok' | {'error', Reason::term()}.
backup_checkpoint(Name, Opaque) ->
    mnesia_log:backup_checkpoint(Name, Opaque).

-spec backup_checkpoint(Name::_, Dest::_, Mod::module()) ->
                               'ok' | {'error', Reason::term()}.
backup_checkpoint(Name, Opaque, Mod) ->
    mnesia_log:backup_checkpoint(Name, Opaque, Mod).

-spec restore(Src::_, [Arg]) -> t_result([table()]) when
      Op  :: 'skip_tables' | 'clear_tables' | 'keep_tables' | 'restore_tables',
      Arg :: {'module', module()} | {Op, [table()]} | {'default_op', Op}.
restore(Opaque, Args) ->
    mnesia_schema:restore(Opaque, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt
-spec create_table([Arg]) -> t_result('ok') when
      Arg :: {'name', table()} | create_option().
create_table(Arg) ->
    mnesia_schema:create_table(Arg).

-spec create_table(Name::table(), [create_option()]) -> t_result('ok').
create_table(Name, Arg) when is_list(Arg) ->
    mnesia_schema:create_table([{name, Name}| Arg]);
create_table(Name, Arg) ->
    {aborted, badarg, Name, Arg}.

-spec delete_table(Tab::table()) -> t_result('ok').
delete_table(Tab) ->
    mnesia_schema:delete_table(Tab).

-spec add_table_copy(Tab::table(), N::node(), ST::storage_type()) -> t_result(ok).
add_table_copy(Tab, N, S) ->
    mnesia_schema:add_table_copy(Tab, N, S).

-spec del_table_copy(Tab::table(), N::node()) -> t_result(ok).
del_table_copy(Tab, N) ->
    mnesia_schema:del_table_copy(Tab, N).

-spec move_table_copy(Tab::table(), From::node(), To::node()) -> t_result(ok).
move_table_copy(Tab, From, To) ->
    mnesia_schema:move_table(Tab, From, To).

-spec add_table_index(Tab::table(), I::index_attr()) -> t_result(ok).
add_table_index(Tab, Ix) ->
    mnesia_schema:add_table_index(Tab, Ix).
-spec del_table_index(Tab::table(), I::index_attr()) -> t_result(ok).
del_table_index(Tab, Ix) ->
    mnesia_schema:del_table_index(Tab, Ix).

-spec transform_table(Tab::table(), Fun, [Attr]) -> t_result(ok) when
      Attr :: atom(),
      Fun:: fun((Record::tuple()) -> Transformed::tuple()) | ignore.
transform_table(Tab, Fun, NewA) ->
    try val({Tab, record_name}) of
	OldRN -> mnesia_schema:transform_table(Tab, Fun, NewA, OldRN)
    catch exit:Reason ->
	    mnesia:abort(Reason)
    end.

-spec transform_table(Tab::table(), Fun, [Attr], RecName) -> t_result(ok) when
      RecName :: atom(),
      Attr :: atom(),
      Fun:: fun((Record::tuple()) -> Transformed::tuple()) | ignore.
transform_table(Tab, Fun, NewA, NewRN) ->
    mnesia_schema:transform_table(Tab, Fun, NewA, NewRN).

-spec change_table_copy_type(Tab::table(), Node::node(), To::storage_type()) -> t_result(ok).
change_table_copy_type(T, N, S) ->
    mnesia_schema:change_table_copy_type(T, N, S).

-spec clear_table(Tab::table()) -> t_result(ok).
clear_table(Tab) ->
    case get(mnesia_activity_state) of
	State = {Mod, Tid, _Ts} when element(1, Tid) =/= tid ->
	    transaction(State, fun() -> do_clear_table(Tab) end, [], infinity, Mod, sync);
	undefined ->
	    transaction(undefined, fun() -> do_clear_table(Tab) end, [], infinity, ?DEFAULT_ACCESS, sync);
	_ -> %% Not allowed for clear_table
	    mnesia:abort({aborted, nested_transaction})
    end.

do_clear_table(Tab) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts}  ->
	    clear_table(Tid, Ts, Tab, '_');
	{Mod, Tid, Ts} ->
	    Mod:clear_table(Tid, Ts, Tab, '_');
	_ ->
	    abort(no_transaction)
    end.

clear_table(Tid, Ts, Tab, Obj) when element(1, Tid) =:= tid ->
    Store = Ts#tidstore.store,
    mnesia_locker:wlock_table(Tid, Store, Tab),
    Oid = {Tab, '_'},
    ?ets_insert(Store, {Oid, Obj, clear_table}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt - user properties
-spec read_table_property(Tab::table(), PropKey::term()) -> Res::tuple().
read_table_property(Tab, PropKey) ->
    val({Tab, user_property, PropKey}).

-spec write_table_property(Tab::table(), Prop::tuple()) -> t_result(ok).
write_table_property(Tab, Prop) ->
    mnesia_schema:write_table_property(Tab, Prop).

-spec delete_table_property(Tab::table(), PropKey::term()) -> t_result(ok).
delete_table_property(Tab, PropKey) ->
    mnesia_schema:delete_table_property(Tab, PropKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt - user properties

-spec change_table_frag(Tab::table(), FP::term()) -> t_result(ok).
change_table_frag(Tab, FragProp) ->
    mnesia_schema:change_table_frag(Tab, FragProp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt - table load

%% Dump a ram table to disc
-spec dump_tables([Tab::table()]) -> t_result(ok).
dump_tables(Tabs) ->
    mnesia_schema:dump_tables(Tabs).

%% allow the user to wait for some tables to be loaded
-spec wait_for_tables([Tab::table()], TMO::timeout()) ->
      'ok' | {'timeout', [table()]} | {'error', Reason::term()}.
wait_for_tables(Tabs, Timeout) ->
    mnesia_controller:wait_for_tables(Tabs, Timeout).

-spec force_load_table(Tab::table()) -> 'yes' | {'error', Reason::term()}.
force_load_table(Tab) ->
    case mnesia_controller:force_load_table(Tab) of
	ok -> yes; % Backwards compatibility
	Other -> Other
    end.

-spec change_table_access_mode(Tab::table(), Mode) -> t_result(ok) when
      Mode :: 'read_only'|'read_write'.
change_table_access_mode(T, Access) ->
    mnesia_schema:change_table_access_mode(T, Access).

-spec change_table_load_order(Tab::table(), Order) -> t_result(ok) when
      Order :: non_neg_integer().
change_table_load_order(T, O) ->
    mnesia_schema:change_table_load_order(T, O).

-spec change_table_majority(Tab::table(), M::boolean()) -> t_result(ok).
change_table_majority(T, M) ->
    mnesia_schema:change_table_majority(T, M).

-spec set_master_nodes(Ns::[node()]) -> 'ok' | {'error', Reason::term()}.
set_master_nodes(Nodes) when is_list(Nodes) ->
    UseDir = system_info(use_dir),
    IsRunning = system_info(is_running),
    case IsRunning of
	yes ->
	    CsPat = {{'_', cstruct}, '_'},
	    Cstructs0 = ?ets_match_object(mnesia_gvar, CsPat),
	    Cstructs = [Cs || {_, Cs} <- Cstructs0],
	    log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning);
	_NotRunning ->
	    case UseDir of
		true ->
		    mnesia_lib:lock_table(schema),
		    Res =
			case mnesia_schema:read_cstructs_from_disc() of
			    {ok, Cstructs} ->
				log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning);
			    {error, Reason} ->
				{error, Reason}
			end,
			mnesia_lib:unlock_table(schema),
		    Res;
		false ->
		    ok
	    end
    end;
set_master_nodes(Nodes) ->
    {error, {bad_type, Nodes}}.

log_valid_master_nodes(Cstructs, Nodes, UseDir, IsRunning) ->
    Fun = fun(Cs) ->
		  Copies = mnesia_lib:copy_holders(Cs),
		  Valid = mnesia_lib:intersect(Nodes, Copies),
		  {Cs#cstruct.name, Valid}
	  end,
    Args = lists:map(Fun, Cstructs),
    mnesia_recover:log_master_nodes(Args, UseDir, IsRunning).

-spec set_master_nodes(Tab::table(), Ns::[node()]) ->
                              'ok' | {'error', Reason::term()}.
set_master_nodes(Tab, Nodes) when is_list(Nodes) ->
    UseDir = system_info(use_dir),
    IsRunning = system_info(is_running),
    case IsRunning of
	yes ->
	    case ?catch_val({Tab, cstruct}) of
		{'EXIT', _} ->
		    {error, {no_exists, Tab}};
		Cs ->
		    case Nodes -- mnesia_lib:copy_holders(Cs) of
			[] ->
			    Args = [{Tab , Nodes}],
			    mnesia_recover:log_master_nodes(Args, UseDir, IsRunning);
			BadNodes ->
			    {error, {no_exists, Tab,  BadNodes}}
		    end
	    end;
	_NotRunning ->
	    case UseDir of
		true ->
		    mnesia_lib:lock_table(schema),
		    Res =
			case mnesia_schema:read_cstructs_from_disc() of
			    {ok, Cstructs} ->
				case lists:keysearch(Tab, 2, Cstructs) of
				    {value, Cs} ->
					case Nodes -- mnesia_lib:copy_holders(Cs) of
					    [] ->
						Args = [{Tab , Nodes}],
						mnesia_recover:log_master_nodes(Args, UseDir, IsRunning);
					    BadNodes ->
						{error, {no_exists, Tab,  BadNodes}}
					end;
				    false ->
					{error, {no_exists, Tab}}
				end;
			    {error, Reason} ->
				{error, Reason}
			end,
		    mnesia_lib:unlock_table(schema),
		    Res;
		false ->
		    ok
	    end
    end;
set_master_nodes(Tab, Nodes) ->
    {error, {bad_type, Tab, Nodes}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc admin
-spec dump_log() -> 'dumped'.
dump_log() ->
    mnesia_controller:sync_dump_log(user).

-spec sync_log() -> 'ok' | {'error', Reason::term()}.
sync_log() ->
    mnesia_monitor:sync_log(latest_log).

-spec subscribe(What) -> {'ok', node()} | {'error', Reason::term()} when
      What :: 'system' | 'activity' | {'table', table(), 'simple' | 'detailed'}.
subscribe(What) ->
    mnesia_subscr:subscribe(self(), What).

-spec unsubscribe(What) -> {'ok', node()} | {'error', Reason::term()} when
      What :: 'system' | 'activity' | {'table', table(), 'simple' | 'detailed'}.
unsubscribe(What) ->
    mnesia_subscr:unsubscribe(self(), What).

-spec report_event(Event::_) -> 'ok'.
report_event(Event) ->
    mnesia_lib:report_system_event({mnesia_user, Event}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Snmp
-spec snmp_open_table(Tab::table(), Snmp::snmp_struct()) -> ok.
snmp_open_table(Tab, Us) ->
    mnesia_schema:add_snmp(Tab, Us).

-spec snmp_close_table(Tab::table()) -> ok.
snmp_close_table(Tab) ->
    mnesia_schema:del_snmp(Tab).

-spec snmp_get_row(Tab::table(), [integer()]) -> {'ok', Row::tuple()} | 'undefined'.
snmp_get_row(Tab, RowIndex) when is_atom(Tab), Tab /= schema, is_list(RowIndex) ->
    case get(mnesia_activity_state) of
 	{Mod, Tid, Ts=#tidstore{store=Store}} when element(1, Tid) =:= tid ->
	    case snmp_oid_to_mnesia_key(RowIndex, Tab) of
		unknown -> %% Arrg contains fix_string
		    Ops = find_ops(Store, Tab, val({Tab, wild_pattern})),
		    SnmpType = val({Tab,snmp}),
		    Fix = fun({{_,Key},Row,Op}, Res) ->
				  case mnesia_snmp_hook:key_to_oid(Tab,Key,SnmpType) of
				      RowIndex ->
					  case Op of
					      write -> {ok, Row};
					      _ ->
						  undefined
					  end;
				      _ ->
					  Res
				  end
			  end,
		    lists:foldl(Fix, undefined, Ops);
		Key ->
		    case Mod:read(Tid, Ts, Tab, Key, read) of
			[Row] ->
			    {ok, Row};
			_ ->
			    undefined
		    end
	    end;
	_ ->
 	    dirty_rpc(Tab, mnesia_snmp_hook, get_row, [Tab, RowIndex])
    end;
snmp_get_row(Tab, _RowIndex) ->
    abort({bad_type, Tab}).

%%%%%%%%%%%%%
-spec snmp_get_next_index(Tab::table(), [integer()]) -> {'ok', [integer()]} | 'endOfTable'.
snmp_get_next_index(Tab, RowIndex) when is_atom(Tab), Tab /= schema, is_list(RowIndex) ->
    {Next,OrigKey} = dirty_rpc(Tab, mnesia_snmp_hook, get_next_index, [Tab, RowIndex]),
    case get(mnesia_activity_state) of
	{_Mod, Tid, #tidstore{store=Store}} when element(1, Tid) =:= tid ->
	    case OrigKey of
		undefined ->
		    snmp_order_keys(Store, Tab, RowIndex, []);
		_ ->
		    case ?ets_match(Store, {{Tab,OrigKey}, '_', '$1'}) of
			[] ->  snmp_order_keys(Store,Tab,RowIndex,[OrigKey]);
			Ops ->
			    case lists:last(Ops) of
				[delete] -> snmp_get_next_index(Tab, Next);
				_ -> snmp_order_keys(Store,Tab,RowIndex,[OrigKey])
			    end
		    end
	    end;
	_ ->
	    case Next of
		endOfTable -> endOfTable;
		_ -> {ok, Next}
	    end
    end;
snmp_get_next_index(Tab, _RowIndex) ->
    abort({bad_type, Tab}).

snmp_order_keys(Store,Tab,RowIndex,Def) ->
    All = ?ets_match(Store, {{Tab,'$1'},'_','$2'}),
    SnmpType = val({Tab,snmp}),
    Keys0 = [mnesia_snmp_hook:key_to_oid(Tab,Key,SnmpType) ||
		Key <- ts_keys_1(All, Def)],
    Keys = lists:sort(Keys0),
    get_ordered_snmp_key(RowIndex,Keys).

get_ordered_snmp_key(Prev, [First|_]) when Prev < First -> {ok, First};
get_ordered_snmp_key(Prev, [_|R]) ->
    get_ordered_snmp_key(Prev, R);
get_ordered_snmp_key(_, []) ->
    endOfTable.

%%%%%%%%%%
-spec snmp_get_mnesia_key(Tab::table(), [integer()]) -> {'ok', Key::term()} | 'undefined'.
snmp_get_mnesia_key(Tab, RowIndex) when is_atom(Tab), Tab /= schema, is_list(RowIndex) ->
    case get(mnesia_activity_state) of
 	{_Mod, Tid, Ts} when element(1, Tid) =:= tid ->
	    Res = dirty_rpc(Tab,mnesia_snmp_hook,get_mnesia_key,[Tab,RowIndex]),
	    snmp_filter_key(Res, RowIndex, Tab, Ts#tidstore.store);
	_ ->
	    dirty_rpc(Tab, mnesia_snmp_hook, get_mnesia_key, [Tab, RowIndex])
    end;
snmp_get_mnesia_key(Tab, _RowIndex) ->
    abort({bad_type, Tab}).

snmp_oid_to_mnesia_key(RowIndex, Tab) ->
    case mnesia_snmp_hook:oid_to_key(RowIndex, Tab) of
	unknown ->  %% Contains fix_string needs lookup
	    case dirty_rpc(Tab,mnesia_snmp_hook,get_mnesia_key,[Tab,RowIndex]) of
		{ok, MnesiaKey} -> MnesiaKey;
		undefined -> unknown
	    end;
	MnesiaKey ->
	    MnesiaKey
    end.

snmp_filter_key(Res = {ok,Key}, _RowIndex, Tab, Store) ->
    case ?ets_lookup(Store, {Tab,Key}) of
	[] -> Res;
	Ops ->
	    case lists:last(Ops) of
		{_, _, write} -> Res;
		_ -> undefined
	    end
    end;
snmp_filter_key(undefined, RowIndex, Tab, Store) ->
    case mnesia_snmp_hook:oid_to_key(RowIndex, Tab) of
	unknown ->  %% Arrg contains fix_string
	    Ops = find_ops(Store, Tab, val({Tab, wild_pattern})),
	    SnmpType = val({Tab,snmp}),
	    Fix = fun({{_,Key},_,Op}, Res) ->
			  case mnesia_snmp_hook:key_to_oid(Tab,Key,SnmpType) of
			      RowIndex ->
				  case Op of
				      write -> {ok, Key};
				      _ ->
					  undefined
				  end;
			      _ ->
				  Res
			  end
		  end,
	    lists:foldl(Fix, undefined, Ops);
	Key ->
	    case ?ets_lookup(Store, {Tab,Key}) of
		[] ->
		    undefined;
		Ops ->
		    case lists:last(Ops) of
			{_, _, write} -> {ok, Key};
			_ -> undefined
		    end
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Textfile access
-spec load_textfile(File::file:filename()) -> t_result(ok) | {'error', term()}.
load_textfile(F) ->
    mnesia_text:load_textfile(F).

-spec dump_to_textfile(File :: file:filename()) -> 'ok' | 'error' | {'error', term()}.
dump_to_textfile(F) ->
    mnesia_text:dump_to_textfile(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QLC Handles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec table(Tab::table()) -> qlc:query_handle().
table(Tab) ->
    table(Tab, []).

-spec table(Tab::table(), Options) -> qlc:query_handle() when
      Options   :: Option | [Option],
      Option    :: MnesiaOpt | QlcOption,
      MnesiaOpt :: {'traverse', SelectOp} | {lock, lock_kind()} | {n_objects, non_neg_integer()},
      SelectOp  ::  'select' | {'select', ets:match_spec()},
      QlcOption :: {'key_equality', '==' | '=:='}.
table(Tab,Opts) ->
    {[Trav,Lock,NObjects],QlcOptions0} =
	qlc_opts(Opts,[{traverse,select},{lock,read},{n_objects,100}]),
    TF = case Trav of
	     {select,Ms} ->
		 fun() -> qlc_select(select(Tab,Ms,NObjects,Lock)) end;
	     select ->
		 fun(Ms) -> qlc_select(select(Tab,Ms,NObjects,Lock)) end;
	     _ ->
		 erlang:error({badarg, {Trav,[Tab, Opts]}})
	 end,
    Pre  = fun(Arg) -> pre_qlc(Arg, Tab) end,
    Post = fun()  -> post_qlc(Tab) end,
    Info = fun(Tag) -> qlc_info(Tab, Tag) end,
    ParentFun = fun() ->
			{mnesia_activity, mnesia:get_activity_id()}
		end,
    Lookup =
	case Trav of
	    {select, _} -> [];
	    _ ->
		LFun = fun(2, Keys) ->
			       Read = fun(Key) -> read(Tab,Key,Lock) end,
			       lists:flatmap(Read, Keys);
			  (Index,Keys) ->
			       IdxRead = fun(Key) -> index_read(Tab,Key,Index) end,
			       lists:flatmap(IdxRead, Keys)
		       end,
		[{lookup_fun, LFun}]
	end,
    MFA  = fun(Type) -> qlc_format(Type, Tab, NObjects, Lock, Opts) end,
    QlcOptions = [{pre_fun, Pre}, {post_fun, Post},
		  {info_fun, Info}, {parent_fun, ParentFun},
		  {format_fun, MFA}|Lookup] ++ QlcOptions0,
    qlc:table(TF, QlcOptions).

pre_qlc(Opts, Tab) ->
    {_,Tid,_} =
	case get(mnesia_activity_state) of
	    undefined ->
		case lists:keysearch(parent_value, 1, Opts) of
		    {value, {parent_value,{mnesia_activity,undefined}}} ->
			abort(no_transaction);
		    {value, {parent_value,{mnesia_activity,Aid}}} ->
			{value,{stop_fun,Stop}} =
			    lists:keysearch(stop_fun,1,Opts),
			put_activity_id(Aid,Stop),
			Aid;
		    _ ->
			abort(no_transaction)
		end;
	    Else ->
		Else
	end,
    case element(1,Tid) of
	tid -> ok;
	_ ->
	    case ?catch_val({Tab, setorbag}) of
		ordered_set ->   ok;
		_ ->
		    dirty_rpc(Tab, mnesia_tm, fixtable, [Tab,true,self()]),
		    ok
	    end
    end.

post_qlc(Tab) ->
    case get(mnesia_activity_state) of
	{_,#tid{},_} -> ok;
	_ ->
	    case ?catch_val({Tab, setorbag}) of
		ordered_set ->
		    ok;
		_ ->
		    dirty_rpc(Tab, mnesia_tm, fixtable, [Tab,false,self()]),
		    ok
	    end
    end.

qlc_select('$end_of_table') ->     [];
qlc_select({[], Cont}) -> qlc_select(select(Cont));
qlc_select({Objects, Cont}) ->
    Objects ++ fun() -> qlc_select(select(Cont)) end.

qlc_opts(Opts, Keys) when is_list(Opts) ->
    qlc_opts(Opts, Keys, []);
qlc_opts(Option, Keys) ->
    qlc_opts([Option], Keys, []).

qlc_opts(Opts, [{Key,Def}|Keys], Acc) ->
    Opt = case lists:keysearch(Key,1, Opts) of
	      {value, {Key,Value}} ->
		  Value;
	      false ->
		  Def
	  end,
    qlc_opts(lists:keydelete(Key,1,Opts),Keys,[Opt|Acc]);
qlc_opts(Opts,[],Acc) -> {lists:reverse(Acc),Opts}.

qlc_info(Tab, num_of_objects) ->
    dirty_rpc(Tab, ?MODULE, raw_table_info, [Tab, size]);
qlc_info(_, keypos) ->    2;
qlc_info(_, is_unique_objects) ->    true;
qlc_info(Tab, is_unique_keys) ->
    case val({Tab, type}) of
	set -> true;
	ordered_set -> true;
	_ -> false
    end;
qlc_info(Tab, is_sorted_objects) ->
    case val({Tab, type}) of
	ordered_set ->
	    case ?catch_val({Tab, frag_hash}) of
		{'EXIT', _} ->
		    ascending;
		_ ->  %% Fragmented tables are not ordered
		    no
	    end;
	_ -> no
    end;
qlc_info(Tab, indices) ->
    val({Tab,index});
qlc_info(_Tab, _) ->
    undefined.

qlc_format(all, Tab, NObjects, Lock, Opts) ->
    {?MODULE, table, [Tab,[{n_objects, NObjects}, {lock,Lock}|Opts]]};
qlc_format({match_spec, Ms}, Tab, NObjects, Lock, Opts) ->
    {?MODULE, table, [Tab,[{traverse,{select,Ms}},{n_objects, NObjects}, {lock,Lock}|Opts]]};
qlc_format({lookup, 2, Keys}, Tab, _, Lock, _) ->
    io_lib:format("lists:flatmap(fun(V) -> "
		  "~w:read(~w, V, ~w) end, ~w)",
		  [?MODULE, Tab, Lock, Keys]);
qlc_format({lookup, Index,Keys}, Tab, _, _, _) ->
    io_lib:format("lists:flatmap(fun(V) -> "
		  "~w:index_read(~w, V, ~w) end, ~w)",
		  [?MODULE, Tab, Index, Keys]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_fixtable(Tab, #tidstore{store=Store}) ->
    do_fixtable(Tab,Store);
do_fixtable(Tab, Store) ->
    case ?catch_val({Tab, setorbag}) of
	ordered_set ->
	    ok;
	_ ->
	    case ?ets_match_object(Store, {fixtable, {Tab, '_'}}) of
		[] ->
		    Node = dirty_rpc(Tab, mnesia_tm, fixtable, [Tab,true,self()]),
		    ?ets_insert(Store, {fixtable, {Tab, Node}});
		_ ->
		    ignore
	    end,
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mnemosyne exclusive

get_activity_id() ->
    get(mnesia_activity_state).

put_activity_id(Activity) ->
    mnesia_tm:put_activity_id(Activity).
put_activity_id(Activity,Fun) ->
    mnesia_tm:put_activity_id(Activity,Fun).

regular_indexes(Tab) ->
    PosList = val({Tab, index}),
    [P || P <- PosList, is_integer(P)].
