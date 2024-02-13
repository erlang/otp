%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-moduledoc """
A distributed telecommunications DBMS

The following are some of the most important and attractive capabilities
provided by Mnesia:

- A relational/object hybrid data model that is suitable for telecommunications
  applications.
- A DBMS query language, Query List Comprehension (QLC) as an add-on library.
- Persistence. Tables can be coherently kept on disc and in the main memory.
- Replication. Tables can be replicated at several nodes.
- Atomic transactions. A series of table manipulation operations can be grouped
  into a single atomic transaction.
- Location transparency. Programs can be written without knowledge of the actual
  data location.
- Extremely fast real-time data searches.
- Schema manipulation routines. The DBMS can be reconfigured at runtime without
  stopping the system.

This Reference Manual describes the Mnesia API. This includes functions that
define and manipulate Mnesia tables.

All functions in this Reference Manual can be used in any combination with
queries using the list comprehension notation. For information about the query
notation, see the `m:qlc` manual page in STDLIB.

Data in Mnesia is organized as a set of tables. Each table has a name that must
be an atom. Each table is made up of Erlang records. The user is responsible for
the record definitions. Each table also has a set of properties. The following
are some of the properties that are associated with each table:

- `type`. Each table can have `set`, `ordered_set`, or `bag` semantics. Notice
  that currently `ordered_set` is not supported for `disc_only_copies`.

  If a table is of type `set`, each key leads to either one or zero records.

  If a new item is inserted with the same key as an existing record, the old
  record is overwritten. However, if a table is of type `bag`, each key can map
  to several records. All records in type `bag` tables are unique, only the keys
  can be duplicated.

- `record_name`. All records stored in a table must have the same name. The
  records must be instances of the same record type.
- `ram_copies`. A table can be replicated on a number of Erlang nodes. Property
  `ram_copies` specifies a list of Erlang nodes where RAM copies are kept. These
  copies can be dumped to disc at regular intervals. However, updates to these
  copies are not written to disc on a transaction basis.
- `disc_copies`. This property specifies a list of Erlang nodes where the table
  is kept in RAM and on disc. All updates of the table are performed in the
  actual table and are also logged to disc. If a table is of type `disc_copies`
  at a certain node, the entire table is resident in RAM memory and on disc.
  Each transaction performed on the table is appended to a `LOG` file and
  written into the RAM table.
- `disc_only_copies`. Some, or all, table replicas can be kept on disc only.
  These replicas are considerably slower than the RAM-based replicas.
- `index`. This is a list of attribute names, or integers, which specify the
  tuple positions on which Mnesia is to build and maintain an extra index table.
- `local_content`. When an application requires tables whose contents are local
  to each node, `local_content` tables can be used. The table name is known to
  all Mnesia nodes, but its content is unique on each node. This means that
  access to such a table must be done locally. Set field `local_content` to
  `true` to enable the `local_content` behavior. Default is `false`.
- `majority`. This attribute is `true` or `false`; default is `false`. When
  `true`, a majority of the table replicas must be available for an update to
  succeed. Majority checking can be enabled on tables with mission-critical
  data, where it is vital to avoid inconsistencies because of network splits.
- `snmp`. Each (set-based) Mnesia table can be automatically turned into a
  Simple Network Management Protocol (SNMP) ordered table as well. This property
  specifies the types of the SNMP keys.
- `attributes`. The names of the attributes for the records that are inserted in
  the table.

For information about the complete set of table properties and their details,
see `mnesia:create_table/2`.

This Reference Manual uses a table of persons to illustrate various examples.
The following record definition is assumed:

```erlang
-record(person, {name,
                 age = 0,
                 address = unknown,
                 salary = 0,
                 children = []}),
```

The first record attribute is the primary key, or key for short.

The function descriptions are sorted in alphabetical order. It is recommended to
start to read about `mnesia:create_table/2`, `mnesia:lock/2`, and
`mnesia:activity/4` before you continue and learn about the rest.

Writing or deleting in transaction-context creates a local copy of each modified
record during the transaction. During iteration, that is, `mnesia:fold[lr]/4`,
`mnesia:next/2`, `mnesia:prev/2`, and `mnesia:snmp_get_next_index/2`, Mnesia
compensates for every written or deleted record, which can reduce the
performance.

If possible, avoid writing or deleting records in the same transaction before
iterating over the table.

## Configuration Parameters

[](){: #configuration_parameters }

Mnesia reads the following application configuration parameters:

- `-mnesia access_module Module`. The name of the Mnesia activity access
  callback module. Default is `mnesia`.
- `-mnesia auto_repair true | false`. This flag controls if Mnesia automatically
  tries to repair files that have not been properly closed. Default is `true`.
- `-mnesia backup_module Module`. The name of the Mnesia backup callback module.
  Default is `mnesia_backup`.
- `-mnesia debug Level`. Controls the debug level of Mnesia. The possible values
  are as follows:

  - **`none`** - No trace outputs. This is the default.

  - **`verbose`** - Activates tracing of important debug events. These events
    generate `{mnesia_info, Format, Args}` system events. Processes can
    subscribe to these events with `mnesia:subscribe/1`. The events are always
    sent to the Mnesia event handler.

  - **`debug`** - Activates all events at the verbose level plus full trace of
    all debug events. These debug events generate `{mnesia_info, Format, Args}`
    system events. Processes can subscribe to these events with
    `mnesia:subscribe/1`. The events are always sent to the Mnesia event
    handler. On this debug level, the Mnesia event handler starts subscribing to
    updates in the schema table.

  - **`trace`** - Activates all events at the debug level. On this level, the
    Mnesia event handler starts subscribing to updates on all Mnesia tables.
    This level is intended only for debugging small toy systems, as many large
    events can be generated.

  - **`false`** - An alias for none.

  - **`true`** - An alias for debug.

- `-mnesia core_dir Directory`. The name of the directory where Mnesia core
  files is stored, or false. Setting it implies that also RAM-only nodes
  generate a core file if a crash occurs.
- `-mnesia dc_dump_limit Number`. Controls how often `disc_copies` tables are
  dumped from memory. Tables are dumped when
  `filesize(Log) > (filesize(Tab)/Dc_dump_limit)`. Lower values reduce CPU
  overhead but increase disk space and startup times. Default is 4.
- `-mnesia dir Directory`. The name of the directory where all Mnesia data is
  stored. The directory name must be unique for the current node. Two nodes must
  never share the the same Mnesia directory. The results are unpredictable.
- `-mnesia dump_disc_copies_at_startup true | false`. If set to false, this
  disables the dumping of `disc_copies` tables during startup while tables are
  being loaded. The default is true.
- `-mnesia dump_log_load_regulation true | false`. Controls if log dumps are to
  be performed as fast as possible, or if the dumper is to do its own load
  regulation. Default is `false`.

  This feature is temporary and will be removed in a future release

- `-mnesia dump_log_update_in_place true | false`. Controls if log dumps are
  performed on a copy of the original data file, or if the log dump is performed
  on the original data file. Default is `true`
- [](){: #dump_log_write_threshold } `-mnesia dump_log_write_threshold Max`.
  `Max` is an integer that specifies the maximum number of writes allowed to the
  transaction log before a new dump of the log is performed. Default is `1000`
  log writes.
- [](){: #dump_log_time_threshold } `-mnesia dump_log_time_threshold Max`. `Max`
  is an integer that specifies the dump log interval in milliseconds. Default is
  3 minutes. If a dump has not been performed within `dump_log_time_threshold`
  milliseconds, a new dump is performed regardless of the number of writes
  performed.
- `-mnesia event_module Module`. The name of the Mnesia event handler callback
  module. Default is `mnesia_event`.
- `-mnesia extra_db_nodes Nodes` specifies a list of nodes, in addition to the
  ones found in the schema, with which Mnesia is also to establish contact.
  Default is `[]` (empty list).
- `-mnesia fallback_error_function {UserModule, UserFunc}`. Specifies a
  user-supplied callback function, which is called if a fallback is installed
  and Mnesia goes down on another node. Mnesia calls the function with one
  argument, the name of the dying node, for example,
  `UserModule:UserFunc(DyingNode)`. Mnesia must be restarted, otherwise the
  database can be inconsistent. The default behavior is to terminate Mnesia.
- `-mnesia max_wait_for_decision Timeout`. Specifies how long Mnesia waits for
  other nodes to share their knowledge about the outcome of an unclear
  transaction. By default, `Timeout` is set to the atom `infinity`. This implies
  that if Mnesia upon startup detects a "heavyweight transaction" whose outcome
  is unclear, the local Mnesia waits until Mnesia is started on some (in the
  worst case all) of the other nodes that were involved in the interrupted
  transaction. This is a rare situation, but if it occurs, Mnesia does not guess
  if the transaction on the other nodes was committed or terminated. Mnesia
  waits until it knows the outcome and then acts accordingly.

  If `Timeout` is set to an integer value in milliseconds, Mnesia forces
  "heavyweight transactions" to be finished, even if the outcome of the
  transaction for the moment is unclear. After `Timeout` milliseconds, Mnesia
  commits or terminates the transaction and continues with the startup. This can
  lead to a situation where the transaction is committed on some nodes and
  terminated on other nodes. If the transaction is a schema transaction, the
  inconsistency can be fatal.

- `-mnesia no_table_loaders NUMBER`. Specifies the number of parallel table
  loaders during start. More loaders can be good if the network latency is high
  or if many tables contain few records. Default is `2`.
- `-mnesia send_compressed Level`. Specifies the level of compression to be used
  when copying a table from the local node to another one. Default is `0`.

  `Level` must be an integer in the interval `[0, 9]`, where `0` means no
  compression and `9` means maximum compression. Before setting it to a non-zero
  value, ensure that the remote nodes understand this configuration.

- `-mnesia max_transfer_size Number`. Specifies the estimated size in bytes of a
  single packet of data to be used when copying a table from the local node to
  another one. Default is `64000`.
- `-mnesia schema_location Loc`. Controls where Mnesia looks for its schema.
  Parameter `Loc` can be one of the following atoms:

  - **`disc`** - Mandatory disc. The schema is assumed to be located in the
    Mnesia directory. If the schema cannot be found, Mnesia refuses to start.
    This is the old behavior.

  - **`ram`** - Mandatory RAM. The schema resides in RAM only. At startup, a
    tiny new schema is generated. This default schema only contains the
    definition of the schema table and only resides on the local node. Since no
    other nodes are found in the default schema, configuration parameter
    `extra_db_nodes` must be used to let the node share its table definitions
    with other nodes.

    Parameter `extra_db_nodes` can also be used on disc based nodes.

  - **`opt_disc`** - Optional disc. The schema can reside on disc or in RAM. If
    the schema is found on disc, Mnesia starts as a disc-based node and the
    storage type of the schema table is `disc_copies`. If no schema is found on
    disc, Mnesia starts as a disc-less node and the storage type of the schema
    table is `ram_copies`. Default value for the application parameter is
    `opt_disc`.

First, the SASL application parameters are checked, then the command-line flags
are checked, and finally, the default value is chosen.

## See Also

`m:application`, `m:dets`, `m:disk_log`, `m:ets`, `m:mnesia_registry`, `m:qlc`
""".
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
        {'disc_only_copies', [node()]} |
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
-type result() :: 'ok' | {'error', Reason::term()}.
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
-type config_key() :: 'extra_db_nodes' | 'dc_dump_limit'.
-type config_value() :: [node()] | number().
-type config_result() :: {'ok', config_value()} | {'error', term()}.
-type debug_level() :: 'none' | 'verbose' | 'debug' | 'trace'.

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

-doc false.
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
-doc """
[](){: #start }

Mnesia startup is asynchronous. The function call `mnesia:start()` returns the
atom `ok` and then starts to initialize the different tables. Depending on the
size of the database, this can take some time, and the application programmer
must wait for the tables that the application needs before they can be used.
This is achieved by using the function `mnesia:wait_for_tables/2`.

The startup procedure for a set of Mnesia nodes is a fairly complicated
operation. A Mnesia system consists of a set of nodes, with Mnesia started
locally on all participating nodes. Normally, each node has a directory where
all the Mnesia files are written. This directory is referred to as the Mnesia
directory. Mnesia can also be started on disc-less nodes. For more information
about disc-less nodes, see `mnesia:create_schema/1` and the User's Guide.

The set of nodes that makes up a Mnesia system is kept in a schema. Mnesia nodes
can be added to or removed from the schema. The initial schema is normally
created on disc with the function `mnesia:create_schema/1`. On disc-less nodes,
a tiny default schema is generated each time Mnesia is started. During the
startup procedure, Mnesia exchanges schema information between the nodes to
verify that the table definitions are compatible.

Each schema has a unique cookie, which can be regarded as a unique schema
identifier. The cookie must be the same on all nodes where Mnesia is supposed to
run. For details, see the User's Guide.

The schema file and all other files that Mnesia needs are kept in the Mnesia
directory. The command-line option `-mnesia dir Dir` can be used to specify the
location of this directory to the Mnesia system. If no such command-line option
is found, the name of the directory defaults to `Mnesia.Node`.

`application:start(mnesia)` can also be used.
""".
-spec start() -> result().
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

-doc false.
-spec start([{Option::atom(), Value::_}]) -> result().
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

-doc """
[](){: #stop }

Stops Mnesia locally on the current node.

`application:stop(mnesia)` can also be used.
""".
-spec stop() -> 'stopped' | {'error', term()}.
stop() ->
    case application:stop(?APPLICATION) of
	ok -> stopped;
	{error, {not_started, ?APPLICATION}} -> stopped;
	Other -> Other
    end.

-doc """
[](){: #change_config }

`Config` is to be an atom of the following configuration parameters:

- **`extra_db_nodes`** - `Value` is a list of nodes that Mnesia is to try to
  connect to. `ReturnValue` is those nodes in `Value` that Mnesia is connected
  to.

  Notice that this function must only be used to connect to newly started RAM
  nodes (N.D.R.S.N.) with an empty schema. If, for example, this function is
  used after the network has been partitioned, it can lead to inconsistent
  tables.

  Notice that Mnesia can be connected to other nodes than those returned in
  `ReturnValue`.

- **`dc_dump_limit`** - `Value` is a number. See the description in
  [Section Configuration Parameters](`m:mnesia#configuration_parameters`).
  `ReturnValue` is the new value. Notice that this configuration parameter is
  not persistent. It is lost when Mnesia has stopped.
""".
-spec change_config(Config, Value) -> config_result() when
      Config :: config_key(), Value :: config_value().
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


-doc """
[](){: #set_debug_level }

Changes the internal debug level of Mnesia. For details, see
[Section Configuration Parameters](`m:mnesia#configuration_parameters`).
""".
-spec set_debug_level(Level  :: debug_level()) ->
          OldLevel :: debug_level().

set_debug_level(Level) ->
    mnesia_subscr:set_debug_level(Level).

-doc false.
lkill() ->
    mnesia_sup:kill().

-doc false.
kill() ->
    rpc:multicall(mnesia_sup, kill, []).

-doc false.
ms() ->
    [
     mnesia_sup,
     mnesia_kernel_sup,
     mnesia_checkpoint_sup,
     mnesia_snmp_sup,
     mnesia_ext_sup,

     mnesia,
     mnesia_app,
     mnesia_backup,
     mnesia_bup,
     mnesia_checkpoint,
     mnesia_controller,
     mnesia_dumper,
     mnesia_loader,
     mnesia_frag,
     mnesia_frag_hash,
     mnesia_index,
     mnesia_late_loader,
     mnesia_lib,
     mnesia_log,
     mnesia_registry,
     mnesia_schema,
     mnesia_snmp_hook,
     mnesia_subscr,
     mnesia_text,
     mnesia_tm,
     mnesia_recover,
     mnesia_locker,

     %% Keep these last in the list, so
     %% mnesia_sup kills these last
     mnesia_monitor,
     mnesia_event
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Activity mgt

-doc """
Makes the transaction silently return the tuple `{aborted, Reason}`. Termination
of a Mnesia transaction means that an exception is thrown to an enclosing
`catch`. Thus, the expression `catch mnesia:abort(x)` does not terminate the
transaction.
""".
-spec abort(_) -> no_return().
abort(Reason = {aborted, _}) ->
    exit(Reason);
abort(Reason) ->
    exit({aborted, Reason}).

-doc """
[](){: #is_transaction }

When this function is executed inside a transaction-context, it returns `true`,
otherwise `false`.
""".
-spec is_transaction() -> boolean().
is_transaction() ->
    case get(mnesia_activity_state) of
	{_, Tid, _Ts} when element(1,Tid) == tid ->
	    true;
	_ ->
	    false
    end.

-doc(#{equiv => transaction/3}).
-spec transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
transaction(Fun) ->
    transaction(get(mnesia_activity_state), Fun, [], infinity, ?DEFAULT_ACCESS, async).

-doc(#{equiv => transaction/3}).
-spec transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res),
      Retries :: non_neg_integer() | 'infinity';
                 (Fun, Args::[Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
transaction(Fun, Retries) when is_integer(Retries), Retries >= 0 ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, async);
transaction(Fun, Retries) when Retries == infinity ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, async);
transaction(Fun, Args) ->
    transaction(get(mnesia_activity_state), Fun, Args, infinity, ?DEFAULT_ACCESS, async).

-doc """
[](){: #transaction }

Executes the functional object `Fun` with arguments `Args` as a transaction.

The code that executes inside the transaction can consist of a series of table
manipulation functions. If something goes wrong inside the transaction as a
result of a user error or a certain table not being available, the entire
transaction is terminated and the function [`transaction/1`](`transaction/1`)
returns the tuple `{aborted, Reason}`.

If all is going well, `{atomic, ResultOfFun}` is returned, where `ResultOfFun`
is the value of the last expression in `Fun`.

A function that adds a family to the database can be written as follows if there
is a structure `{family, Father, Mother, ChildrenList}`:

```erlang
add_family({family, F, M, Children}) ->
    ChildOids = lists:map(fun oid/1, Children),
    Trans = fun() ->
        mnesia:write(F#person{children = ChildOids}),
        mnesia:write(M#person{children = ChildOids}),
        Write = fun(Child) -> mnesia:write(Child) end,
        lists:foreach(Write, Children)
    end,
    mnesia:transaction(Trans).

oid(Rec) -> {element(1, Rec), element(2, Rec)}.
```

This code adds a set of people to the database. Running this code within one
transaction ensures that either the whole family is added to the database, or
the whole transaction terminates. For example, if the last child is badly
formatted, or the executing process terminates because of an `'EXIT'` signal
while executing the family code, the transaction terminates. Thus, the situation
where half a family is added can never occur.

It is also useful to update the database within a transaction if several
processes concurrently update the same records. For example, the function
`raise(Name, Amount)`, which adds `Amount` to the salary field of a person, is
to be implemented as follows:

```erlang
raise(Name, Amount) ->
    mnesia:transaction(fun() ->
        case mnesia:wread({person, Name}) of
            [P] ->
                Salary = Amount + P#person.salary,
                P2 = P#person{salary = Salary},
                mnesia:write(P2);
            _ ->
                mnesia:abort("No such person")
        end
    end).
```

When this function executes within a transaction, several processes running on
different nodes can concurrently execute the function `raise/2` without
interfering with each other.

Since Mnesia detects deadlocks, a transaction can be restarted any number of
times and therefore the `Fun` shall not have any side effects such as waiting
for specific messages. This function attempts a restart as many times as
specified in `Retries`. `Retries` must be an integer greater than 0 or the atom
`infinity`, default is `infinity`. Mnesia uses `exit` exceptions to signal that
a transaction needs to be restarted, thus a `Fun` must not catch `exit`
exceptions with reason `{aborted, term()}`.
""".
-spec transaction(Fun, [Arg::_], Retries) -> t_result(Res) when
      Fun :: fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity'.
transaction(Fun, Args, Retries) ->
    transaction(get(mnesia_activity_state), Fun, Args, Retries, ?DEFAULT_ACCESS, async).

-doc(#{equiv => sync_transaction/3}).
-spec sync_transaction(Fun) -> t_result(Res) when
      Fun :: fun(() -> Res).
sync_transaction(Fun) ->
    transaction(get(mnesia_activity_state), Fun, [], infinity, ?DEFAULT_ACCESS, sync).

-doc(#{equiv => sync_transaction/3}).
-spec sync_transaction(Fun, Retries) -> t_result(Res) when
      Fun :: fun(() -> Res) | fun((...) -> Res),
      Retries :: non_neg_integer() | 'infinity';
                      (Fun, Args :: [Arg::_]) -> t_result(Res) when
      Fun :: fun((...) -> Res).
sync_transaction(Fun, Retries) when is_integer(Retries), Retries >= 0 ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, sync);
sync_transaction(Fun, Retries) when Retries == infinity ->
    transaction(get(mnesia_activity_state), Fun, [], Retries, ?DEFAULT_ACCESS, sync);
sync_transaction(Fun, Args) ->
    transaction(get(mnesia_activity_state), Fun, Args, infinity, ?DEFAULT_ACCESS, sync).

-doc """
[](){: #sync_transaction }

Waits until data have been committed and logged to disk (if disk is used) on
every involved node before it returns, otherwise it behaves as
`mnesia:transaction/[1,2,3]`.

This functionality can be used to avoid that one process overloads a database on
another node.
""".
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

-doc(#{equiv => async_dirty/2}).
-spec async_dirty(Fun) -> Res | no_return() when
      Fun :: fun(() -> Res).
async_dirty(Fun) ->
    async_dirty(Fun, []).

-doc """
[](){: #async_dirty }

Calls the `Fun` in a context that is not protected by a transaction. The Mnesia
function calls performed in the `Fun` are mapped to the corresponding dirty
functions. This still involves logging, replication, and subscriptions, but
there is no locking, local transaction storage, or commit protocols involved.
Checkpoint retainers and indexes are updated, but they are updated dirty. As for
normal `mnesia:dirty_*` operations, the operations are performed
semi-asynchronously. For details, see `mnesia:activity/4` and the User's Guide.

The Mnesia tables can be manipulated without using transactions. This has some
serious disadvantages, but is considerably faster, as the transaction manager is
not involved and no locks are set. A dirty operation does, however, guarantee a
certain level of consistency, and the dirty operations cannot return garbled
records. All dirty operations provide location transparency to the programmer,
and a program does not have to be aware of the whereabouts of a certain table to
function.

Notice that it is more than ten times more efficient to read records dirty than
within a transaction.

Depending on the application, it can be a good idea to use the dirty functions
for certain operations. Almost all Mnesia functions that can be called within
transactions have a dirty equivalent, which is much more efficient.

However, notice that there is a risk that the database can be left in an
inconsistent state if dirty operations are used to update it. Dirty operations
are only to be used for performance reasons when it is absolutely necessary.

Notice that calling (nesting) `mnesia:[a]sync_dirty` inside a
transaction-context inherits the transaction semantics.
""".
-spec async_dirty(Fun, [Arg::_]) -> Res | no_return() when
      Fun :: fun((...) -> Res).
async_dirty(Fun, Args) ->
    non_transaction(get(mnesia_activity_state), Fun, Args, async_dirty, ?DEFAULT_ACCESS).

-doc(#{equiv => sync_dirty/2}).
-spec sync_dirty(Fun) -> Res | no_return() when
      Fun :: fun(() -> Res).
sync_dirty(Fun) ->
    sync_dirty(Fun, []).

-doc """
[](){: #sync_dirty }

Calls the `Fun` in a context that is not protected by a transaction. The Mnesia
function calls performed in the `Fun` are mapped to the corresponding dirty
functions. It is performed in almost the same context as
`mnesia:async_dirty/1,2`. The difference is that the operations are performed
synchronously. The caller waits for the updates to be performed on all active
replicas before the `Fun` returns. For details, see `mnesia:activity/4` and the
User's Guide.
""".
-spec sync_dirty(Fun, [Arg::_]) -> Res | no_return() when
      Fun :: fun((...) -> Res).
sync_dirty(Fun, Args) ->
    non_transaction(get(mnesia_activity_state), Fun, Args, sync_dirty, ?DEFAULT_ACCESS).

-doc(#{equiv => ets/2}).
-spec ets(Fun) -> Res | no_return() when
      Fun :: fun(() -> Res).
ets(Fun) ->
    ets(Fun, []).

-doc """
[](){: #ets }

Calls the `Fun` in a raw context that is not protected by a transaction. The
Mnesia function call is performed in the `Fun` and performed directly on the
local ETS tables on the assumption that the local storage type is `ram_copies`
and the tables are not replicated to other nodes. Subscriptions are not
triggered and checkpoints are not updated, but it is extremely fast. This
function can also be applied to `disc_copies` tables if all operations are read
only. For details, see `mnesia:activity/4` and the User's Guide.

Notice that calling (nesting) a `mnesia:ets` inside a transaction-context
inherits the transaction semantics.
""".
-spec ets(Fun, [Arg::_]) -> Res | no_return() when
      Fun :: fun((...) -> Res).
ets(Fun, Args) ->
    non_transaction(get(mnesia_activity_state), Fun, Args, ets, ?DEFAULT_ACCESS).

-doc """
[](){: #activity_2_3 }

Calls `mnesia:activity(AccessContext, Fun, Args, AccessMod)`, where `AccessMod`
is the default access callback module obtained by
`mnesia:system_info(access_module)`. `Args` defaults to `[]` (empty list).
""".
-spec activity(Kind, Fun) -> t_result(Res) | Res when
      Kind :: activity(),
      Fun  :: fun(() -> Res).
activity(Kind, Fun) ->
    activity(Kind, Fun, []).

-doc false.
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

-doc """
[](){: #activity_4 }

Executes the functional object `Fun` with argument `Args`.

The code that executes inside the activity can consist of a series of table
manipulation functions, which are performed in an `AccessContext`. Currently,
the following access contexts are supported:

- **`transaction`** - Short for `{transaction, infinity}`

- **`{transaction, Retries}`** - Calls `mnesia:transaction(Fun, Args, Retries)`.
  Notice that the result from `Fun` is returned if the transaction is successful
  (atomic), otherwise the function exits with an abort reason.

- **`sync_transaction`** - Short for `{sync_transaction, infinity}`

- **`{sync_transaction, Retries}`** - Calls
  `mnesia:sync_transaction(Fun, Args, Retries)`. Notice that the result from
  `Fun` is returned if the transaction is successful (atomic), otherwise the
  function exits with an abort reason.

- **`async_dirty`** - Calls `mnesia:async_dirty(Fun, Args)`.

- **`sync_dirty`** - Calls `mnesia:sync_dirty(Fun, Args)`.

- **`ets`** - Calls `mnesia:ets(Fun, Args)`.

This function (`mnesia:activity/4`) differs in an important way from the
functions `mnesia:transaction`, `mnesia:sync_transaction`, `mnesia:async_dirty`,
`mnesia:sync_dirty`, and `mnesia:ets`. Argument `AccessMod` is the name of a
callback module, which implements the `mnesia_access` behavior.

Mnesia forwards calls to the following functions:

- mnesia:lock/2 (read_lock_table/1, write_lock_table/1)
- mnesia:write/3 (write/1, s_write/1)
- mnesia:delete/3 (delete/1, s_delete/1)
- mnesia:delete_object/3 (delete_object/1, s_delete_object/1)
- mnesia:read/3 (read/1, wread/1)
- mnesia:match_object/3 (match_object/1)
- mnesia:all_keys/1
- mnesia:first/1
- mnesia:last/1
- mnesia:prev/2
- mnesia:next/2
- mnesia:index_match_object/4 (index_match_object/2)
- mnesia:index_read/3
- mnesia:table_info/2

to the corresponding:

- AccessMod:lock(ActivityId, Opaque, LockItem, LockKind)
- AccessMod:write(ActivityId, Opaque, Tab, Rec, LockKind)
- AccessMod:delete(ActivityId, Opaque, Tab, Key, LockKind)
- AccessMod:delete_object(ActivityId, Opaque, Tab, RecXS, LockKind)
- AccessMod:read(ActivityId, Opaque, Tab, Key, LockKind)
- AccessMod:match_object(ActivityId, Opaque, Tab, Pattern, LockKind)
- AccessMod:all_keys(ActivityId, Opaque, Tab, LockKind)
- AccessMod:first(ActivityId, Opaque, Tab)
- AccessMod:last(ActivityId, Opaque, Tab)
- AccessMod:prev(ActivityId, Opaque, Tab, Key)
- AccessMod:next(ActivityId, Opaque, Tab, Key)
- AccessMod:index_match_object(ActivityId, Opaque, Tab, Pattern, Attr, LockKind)
- AccessMod:index_read(ActivityId, Opaque, Tab, SecondaryKey, Attr, LockKind)
- AccessMod:table_info(ActivityId, Opaque, Tab, InfoItem)

`ActivityId` is a record that represents the identity of the enclosing Mnesia
activity. The first field (obtained with
[`element(1, ActivityId)`](`element/2`)) contains an atom, which can be
interpreted as the activity type: `ets`, `async_dirty`, `sync_dirty`, or `tid`.
`tid` means that the activity is a transaction. The structure of the rest of the
identity record is internal to Mnesia.

`Opaque` is an opaque data structure that is internal to Mnesia.
""".
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
%% it is not necessary that they are up and running.
-doc """
[](){: #lock }

Write locks are normally acquired on all nodes where a replica of the table
resides (and is active). Read locks are acquired on one node (the local node if
a local replica exists). Most of the context-sensitive access functions acquire
an implicit lock if they are started in a transaction-context. The granularity
of a lock can either be a single record or an entire table.

The normal use is to call the function without checking the return value, as it
exits if it fails and the transaction is restarted by the transaction manager.
It returns all the locked nodes if a write lock is acquired and `ok` if it was a
read lock.

The function `mnesia:lock/2` is intended to support explicit locking on tables,
but is also intended for situations when locks need to be acquired regardless of
how tables are replicated. Currently, two kinds of `LockKind` are supported:

- **`write`** - Write locks are exclusive. This means that if one transaction
  manages to acquire a write lock on an item, no other transaction can acquire
  any kind of lock on the same item.

- **`read`** - Read locks can be shared. This means that if one transaction
  manages to acquire a read lock on an item, other transactions can also acquire
  a read lock on the same item. However, if someone has a read lock, no one can
  acquire a write lock at the same item. If someone has a write lock, no one can
  acquire either a read lock or a write lock at the same item.

Conflicting lock requests are automatically queued if there is no risk of a
deadlock. Otherwise the transaction must be terminated and executed again.
Mnesia does this automatically as long as the upper limit of the maximum
`retries` is not reached. For details, see `mnesia:transaction/3`.

For the sake of completeness, sticky write locks are also described here even if
a sticky write lock is not supported by this function:

- **`sticky_write`** - Sticky write locks are a mechanism that can be used to
  optimize write lock acquisition. If your application uses replicated tables
  mainly for fault tolerance (as opposed to read access optimization purpose),
  sticky locks can be the best option available.

  When a sticky write lock is acquired, all nodes are informed which node is
  locked. Then, sticky lock requests from the same node are performed as a local
  operation without any communication with other nodes. The sticky lock lingers
  on the node even after the transaction ends. For details, see the User's
  Guide.

Currently, this function supports two kinds of `LockItem`:

- **`{table, Tab}`** - This acquires a lock of type `LockKind` on the entire
  table `Tab`.

- **`{global, GlobalKey, Nodes}`** - This acquires a lock of type `LockKind` on
  the global resource `GlobalKey`. The lock is acquired on all active nodes in
  the `Nodes` list.

Locks are released when the outermost transaction ends.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires locks, otherwise it
ignores the request.
""".
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

-doc false.
-spec lock_table(Tab::table(), LockKind) -> [MnesiaNode] | no_return() when
      MnesiaNode :: node(),
      LockKind :: lock_kind() | 'load'.
lock_table(Tab, LockKind) ->
    lock({table, Tab}, LockKind).

-doc false.
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
-doc """
[](){: #read_lock_table }

Calls the function `mnesia:lock({table, Tab}, read)`.
""".
-spec read_lock_table(Tab::table()) -> 'ok'.
read_lock_table(Tab) ->
    lock({table, Tab}, read),
    ok.

%% Grab a write lock on a whole table
-doc """
[](){: #write_lock_table }

Calls the function `mnesia:lock({table, Tab}, write)`.
""".
-spec write_lock_table(Tab::table()) -> 'ok'.
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
-doc """
[](){: #write_1 }

Calls the function `mnesia:write(Tab, Record, write)`, where `Tab` is
[`element(1, Record)`](`element/2`).
""".
-spec write(Record::tuple()) -> 'ok'.
write(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    write(Tab, Val, write);
write(Val) ->
    abort({bad_type, Val}).

-doc """
[](){: #s_write }

Calls the function `mnesia:write(Tab, Record, sticky_write)`, where `Tab` is
[`element(1, Record)`](`element/2`).
""".
-spec s_write(Record::tuple()) -> 'ok'.
s_write(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    write(Tab, Val, sticky_write).

-doc """
[](){: #write_3 }

Writes record `Record` to table `Tab`.

The function returns `ok`, or terminates if an error occurs. For example, the
transaction terminates if no `person` table exists.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind`. The lock types `write` and `sticky_write` are supported.
""".
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

-doc false.
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

-doc """
[](){: #delete_1 }

Calls `mnesia:delete(Tab, Key, write)`.
""".
-spec delete({Tab::table(), Key::_}) -> 'ok'.
delete({Tab, Key}) ->
    delete(Tab, Key, write);
delete(Oid) ->
    abort({bad_type, Oid}).

-doc """
[](){: #s_delete }

Calls the function `mnesia:delete(Tab, Key, sticky_write)`
""".
-spec s_delete({Tab::table(), Key::_}) -> 'ok'.
s_delete({Tab, Key}) ->
    delete(Tab, Key, sticky_write);
s_delete(Oid) ->
    abort({bad_type, Oid}).

-doc """
[](){: #delete_3 }

Deletes all records in table `Tab` with the key `Key`.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` in the record. Currently, the lock types `write` and `sticky_write`
are supported.
""".
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

-doc false.
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

-doc """
[](){: #delete_object_1 }

Calls `mnesia:delete_object(Tab, Record, write)`, where `Tab` is
[`element(1, Record)`](`element/2`).
""".
-spec delete_object(Rec::tuple()) -> 'ok'.
delete_object(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    delete_object(Tab, Val, write);
delete_object(Val) ->
    abort({bad_type, Val}).

-doc """
[](){: #s_delete_object }

Calls the function `mnesia:delete_object(Tab, Record, sticky_write)`, where
`Tab` is [`element(1, Record)`](`element/2`).
""".
-spec s_delete_object(Rec::tuple()) -> 'ok'.
s_delete_object(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    delete_object(Tab, Val, sticky_write);
s_delete_object(Val) ->
    abort({bad_type, Val}).

-doc """
[](){: #delete_object_3 }

If a table is of type `bag`, it can sometimes be needed to delete only some of
the records with a certain key. This can be done with the function
[`delete_object/3`](`delete_object/3`). A complete record must be supplied to
this function.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` on the record. Currently, the lock types `write` and `sticky_write`
are supported.
""".
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

-doc false.
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
		      case ?ets_match_object(Store, {Oid, '_', write}) ++
                          ?ets_match_object(Store, {Oid, '_', delete}) of
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

-doc """
[](){: #read_2 }

Calls function `mnesia:read(Tab, Key, read)`.
""".
-spec read(Tab::table(), Key::_) -> [tuple()].
read(Tab, Key) ->
    read(Tab, Key, read).

-doc(#{equiv => read/2}).
-spec read({Tab::table(), Key::_}) -> [tuple()].
read({Tab, Key}) ->
    read(Tab, Key, read);
read(Oid) ->
    abort({bad_type, Oid}).

-doc """
[](){: #wread }

Calls the function `mnesia:read(Tab, Key, write)`.
""".
-spec wread({Tab::table(), Key::_}) -> [tuple()].
wread({Tab, Key}) ->
    read(Tab, Key, write);
wread(Oid) ->
    abort({bad_type, Oid}).

-doc """
[](){: #read_3 }

Reads all records from table `Tab` with key `Key`. This function has the same
semantics regardless of the location of `Tab`. If the table is of type `bag`,
the function `mnesia:read(Tab, Key)` can return an arbitrarily long list. If the
table is of type `set`, the list is either of length 1, or `[]`.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind`. Currently, the lock types `read`, `write`, and `sticky_write` are
supported.

If the user wants to update the record, it is more efficient to use
`write/sticky_write` as the `LockKind`. If majority checking is active on the
table, it is checked as soon as a write lock is attempted. This can be used to
end quickly if the majority condition is not met.
""".
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

-doc false.
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

-doc """
[](){: #first }

Records in `set` or `bag` tables are not ordered. However, there is an ordering
of the records that is unknown to the user. A table can therefore be traversed
by this function with the function `mnesia:next/2`.

If there are no records in the table, this function returns the atom
`'$end_of_table'`. It is therefore highly undesirable, but not disallowed, to
use this atom as the key for any user records.
""".
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

-doc false.
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

-doc """
Works exactly like `mnesia:first/1`, but returns the last object in Erlang term
order for the `ordered_set` table type. For all other table types,
`mnesia:first/1` and `mnesia:last/1` are synonyms.
""".
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

-doc false.
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

-doc """
[](){: #next }

Traverses a table and performs operations on all records in the table. When the
end of the table is reached, the special key `'$end_of_table'` is returned.
Otherwise the function returns a key that can be used to read the actual record.
""".
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
-doc false.
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

-doc """
Works exactly like `mnesia:next/2`, but returns the previous object in Erlang
term order for the `ordered_set` table type. For all other table types,
`mnesia:next/2` and `mnesia:prev/2` are synonyms.
""".
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
-doc false.
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

-doc(#{equiv => foldl/4}).
-spec foldl(Fun, Acc0, Tab::table()) -> Acc when
      Fun::fun((Record::tuple(), Acc0) -> Acc).
foldl(Fun, Acc, Tab) ->
    foldl(Fun, Acc, Tab, read).

-doc """
[](){: #foldl }

Iterates over the table `Table` and calls `Function(Record, NewAcc)` for each
`Record` in the table. The term returned from `Function` is used as the second
argument in the next call to `Function`.

`foldl` returns the same term as the last call to `Function` returned.
""".
-spec foldl(Fun, Acc0, Tab::table(), LockKind :: lock_kind()) -> Acc when
      Fun::fun((Record::tuple(), Acc0) -> Acc).
foldl(Fun, Acc, Tab, LockKind) when is_function(Fun) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    foldl(Tid, Ts, Fun, Acc, Tab, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:foldl(Tid, Ts, Fun, Acc, Tab, LockKind);
	_ ->
	    abort(no_transaction)
    end.

-doc false.
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

-doc(#{equiv => foldr/4}).
-spec foldr(Fun, Acc0, Tab::table()) -> Acc when
      Fun::fun((Record::tuple(), Acc0) -> Acc).
foldr(Fun, Acc, Tab) ->
    foldr(Fun, Acc, Tab, read).
-doc """
[](){: #foldr }

Works exactly like [`foldl/3`](`foldl/3`) but iterates the table in the opposite
order for the `ordered_set` table type. For all other table types,
[`foldr/3`](`foldr/3`) and [`foldl/3`](`foldl/3`) are synonyms.
""".
-spec foldr(Fun, Acc0, Tab::table(), LockKind::lock_kind()) -> Acc when
      Fun::fun((Record::tuple(), Acc0) -> Acc).
foldr(Fun, Acc, Tab, LockKind) when is_function(Fun) ->
    case get(mnesia_activity_state) of
	{?DEFAULT_ACCESS, Tid, Ts} ->
	    foldr(Tid, Ts, Fun, Acc, Tab, LockKind);
	{Mod, Tid, Ts} ->
	    Mod:foldr(Tid, Ts, Fun, Acc, Tab, LockKind);
	_ ->
	    abort(no_transaction)
    end.

-doc false.
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
	    add_written_to_set(Written, ObjsFun);
	_   ->
            %% Fall back to request new lock and read from source
	    add_written_to_set(Written, ObjsFun())
    end.

add_written_to_set(Ws, ObjsOrFun) ->
    case lists:last(Ws) of
	{_, _, delete} -> [];
	{_, Val, write} -> [Val];
	{Oid, _, delete_object} ->
            %% May be several 'delete_object' in Ws; need to check if any
            %% deleted Val exists in source table; if not return whatever
            %% is/is not in the source table (ie as the Val is only deleted
            %% if matched at commit this needs to be reflected here)
            [Val || Val <- get_objs(ObjsOrFun),
                    not lists:member({Oid, Val, delete_object}, Ws)]
    end.

get_objs(ObjsFun) when is_function(ObjsFun) -> ObjsFun();
get_objs(Objs) when is_list(Objs)           -> Objs.

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

-doc """
[](){: #match_object_1 }

Calls `mnesia:match_object(Tab, Pattern, read)`, where `Tab` is
[`element(1, Pattern)`](`element/2`).
""".
-spec match_object(Pattern::tuple()) -> [Record::tuple()].
match_object(Pat) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    match_object(Tab, Pat, read);
match_object(Pat) ->
    abort({bad_type, Pat}).

-doc """
[](){: #match_object_3 }

Takes a pattern with "don't care" variables denoted as a `'_'` parameter. This
function returns a list of records that matched the pattern. Since the second
element of a record in a table is considered to be the key for the record, the
performance of this function depends on whether this key is bound or not.

For example, the call
`mnesia:match_object(person, {person, '_', 36, '_', '_'}, read)` returns a list
of all person records with an `age` field of 36.

The function `mnesia:match_object/3` automatically uses indexes if these exist.
However, no heuristics are performed to select the best index.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` on the entire table or a single record. Currently, the lock type
`read` is supported.
""".
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

-doc false.
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
-doc(#{equiv => select/3}).
-spec select(Tab, Spec) -> [Match] when
      Tab::table(), Spec::ets:match_spec(), Match::term().
select(Tab, Pat) ->
    select(Tab, Pat, read).
-doc """
[](){: #select_2_3 }

Matches the objects in table `Tab` using a `match_spec` as described in the
`ets:select/3`. Optionally a lock `read` or `write` can be given as the third
argument. Default is `read`. The return value depends on `MatchSpec`.

Notice that for best performance, `select` is to be used before any modifying
operations are done on that table in the same transaction. That is, do not use
`write` or `delete` before a `select`.

In its simplest forms, the `match_spec` look as follows:

- `MatchSpec = [MatchFunction]`
- `MatchFunction = {MatchHead, [Guard], [Result]}`
- `MatchHead = tuple() | record()`
- `Guard = {"Guardtest name", ...}`
- `Result = "Term construct"`

For a complete description of `select`, see the [ERTS](`e:erts:index.html`)
User's Guide and the `m:ets` manual page in STDLIB.

For example, to find the names of all male persons older than 30 in table `Tab`:

```erlang
MatchHead = #person{name='$1', sex=male, age='$2', _='_'},
Guard = {'>', '$2', 30},
Result = '$1',
mnesia:select(Tab,[{MatchHead, [Guard], [Result]}]),
```
""".
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

-doc false.
select(Tid, Ts, Tab, Spec, LockKind) ->
    SelectFun = fun(FixedSpec) -> dirty_select(Tab, FixedSpec) end,
    fun_select(Tid, Ts, Tab, Spec, LockKind, Tab, SelectFun).

-doc false.
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
-doc """
[](){: #select_4 }

Matches the objects in table `Tab` using a `match_spec` as described in the
[ERTS](`e:erts:index.html`) User's Guide, and returns a chunk of terms and a
continuation. The wanted number of returned terms is specified by argument
`NObjects`. The lock argument can be `read` or `write`. The continuation is to
be used as argument to `mnesia:select/1`, if more or all answers are needed.

Notice that for best performance, `select` is to be used before any modifying
operations are done on that table in the same transaction. That is, do not use
`mnesia:write` or `mnesia:delete` before a `mnesia:select`. For efficiency,
`NObjects` is a recommendation only and the result can contain anything from an
empty list to all available results.
""".
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

-doc false.
select(Tid, Ts, Tab, Spec, NObjects, LockKind) ->
    Where = val({Tab,where_to_read}),
    Type = mnesia_lib:storage_type_at_node(Where,Tab),
    InitFun = fun(FixedSpec) -> dirty_sel_init(Where,Tab,FixedSpec,NObjects,Type) end,
    fun_select(Tid,Ts,Tab,Spec,LockKind,Tab,InitFun,NObjects,Where,Type).

-record(mnesia_select, {tab,tid,node,storage,cont,written=[],spec,type,orig}).

-doc false.
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

-doc """
Selects more objects with the match specification initiated by
`mnesia:select/4`.

Notice that any modifying operations, that is, `mnesia:write` or
`mnesia:delete`, that are done between the `mnesia:select/4` and
`mnesia:select/1` calls are not visible in the result.
""".
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

-doc false.
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

-doc """
[](){: #all_keys }

Returns a list of all keys in the table named `Tab`. The semantics of this
function is context-sensitive. For more information, see `mnesia:activity/4`. In
transaction-context, it acquires a read lock on the entire table.
""".
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

-doc false.
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

-doc """
[](){: #index_match_object_2 }

Starts `mnesia:index_match_object(Tab, Pattern, Pos, read)`, where `Tab` is
[`element(1, Pattern)`](`element/2`).
""".
-spec index_match_object(Pattern, Attr) -> [Record] when
      Pattern::tuple(), Attr::index_attr(), Record::tuple().
index_match_object(Pat, Attr) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    index_match_object(Tab, Pat, Attr, read);
index_match_object(Pat, _Attr) ->
    abort({bad_type, Pat}).

-doc """
[](){: #index_match_object_4 }

In a manner similar to the function `mnesia:index_read/3`, any index information
can be used when trying to match records. This function takes a pattern that
obeys the same rules as the function `mnesia:match_object/3`, except that this
function requires the following conditions:

- The table `Tab` must have an index on position `Pos`.
- The element in position `Pos` in `Pattern` must be bound. `Pos` is an integer
  (`#record.Field`) or an attribute name.

The two index search functions described here are automatically started when
searching tables with `qlc` list comprehensions and also when using the
low-level `mnesia:[dirty_]match_object` functions.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` on the entire table or on a single record. Currently, the lock type
`read` is supported.
""".
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

-doc false.
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

-doc """
[](){: #index_read }

Assume that there is an index on position `Pos` for a certain record type. This
function can be used to read the records without knowing the actual key for the
record. For example, with an index in position 1 of table `person`, the call
`mnesia:index_read(person, 36, #person.age)` returns a list of all persons with
age 36. `Pos` can also be an attribute name (atom), but if the notation
`mnesia:index_read(person, 36, age)` is used, the field position is searched for
in runtime, for each call.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a read lock on the
entire table.
""".
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

-doc false.
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
-doc """
[](){: #dirty_write_1 }

Calls `mnesia:dirty_write(Tab, Record)`, where `Tab` is
[`element(1, Record)`](`element/2`).
""".
-spec dirty_write(Record::tuple()) -> 'ok'.
dirty_write(Val) when is_tuple(Val), tuple_size(Val) > 2  ->
    Tab = element(1, Val),
    dirty_write(Tab, Val);
dirty_write(Val) ->
    abort({bad_type, Val}).

-doc "Dirty equivalent of the function `mnesia:write/3`.".
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

-doc """
[](){: #dirty_delete }

Calls `mnesia:dirty_delete(Tab, Key)`.
""".
-spec dirty_delete({Tab::table(), Key::_}) -> 'ok'.
dirty_delete({Tab, Key}) ->
    dirty_delete(Tab, Key);
dirty_delete(Oid) ->
    abort({bad_type, Oid}).

-doc "Dirty equivalent of the function `mnesia:delete/3`.".
-spec dirty_delete(Tab::table(), Key::_) -> 'ok'.
dirty_delete(Tab, Key) ->
    do_dirty_delete(async_dirty, Tab, Key).

do_dirty_delete(SyncMode, Tab, Key) when is_atom(Tab), Tab /= schema  ->
    Oid = {Tab, Key},
    mnesia_tm:dirty(SyncMode, {Oid, Oid, delete});
do_dirty_delete(_SyncMode, Tab, _Key) ->
    abort({bad_type, Tab}).

-doc """
[](){: #dirty_delete_object_1 }

Calls `mnesia:dirty_delete_object(Tab, Record)`, where `Tab` is
[`element(1, Record)`](`element/2`).
""".
-spec dirty_delete_object(Record::tuple()) -> 'ok'.
dirty_delete_object(Val) when is_tuple(Val), tuple_size(Val) > 2 ->
    Tab = element(1, Val),
    dirty_delete_object(Tab, Val);
dirty_delete_object(Val) ->
    abort({bad_type, Val}).

-doc "Dirty equivalent of the function `mnesia:delete_object/3`.".
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
-doc """
[](){: #dirty_update_counter }

Calls `mnesia:dirty_update_counter(Tab, Key, Incr)`.
""".
-spec dirty_update_counter({Tab::table(), Key::_}, Incr::integer()) ->
                                  NewVal::integer().
dirty_update_counter({Tab, Key}, Incr) ->
    dirty_update_counter(Tab, Key, Incr);
dirty_update_counter(Counter, _Incr) ->
    abort({bad_type, Counter}).

-doc """
Mnesia has no special counter records. However, records of the form
`{Tab, Key, Integer}` can be used as (possibly disc-resident) counters when
`Tab` is a `set`. This function updates a counter with a positive or negative
number. However, counters can never become less than zero. There are two
significant differences between this function and the action of first reading
the record, performing the arithmetic, and then writing the record:

- It is much more efficient.
- `mnesia:dirty_update_counter/3` is performed as an atomic operation although
  it is not protected by a transaction.

If two processes perform `mnesia:dirty_update_counter/3` simultaneously, both
updates take effect without the risk of losing one of the updates. The new value
`NewVal` of the counter is returned.

If `Key` does not exist, a new record is created with value `Incr` if it is
larger than 0, otherwise it is set to 0.
""".
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

-doc """
[](){: #dirty_read }

Calls `mnesia:dirty_read(Tab, Key)`.
""".
-spec dirty_read({Tab::table(), Key::_}) -> [tuple()].
dirty_read({Tab, Key}) ->
    dirty_read(Tab, Key);
dirty_read(Oid) ->
    abort({bad_type, Oid}).

-doc "Dirty equivalent of the function `mnesia:read/3`.".
-spec dirty_read(Tab::table(), Key::_) -> [tuple()].
dirty_read(Tab, Key)
  when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_get, [Tab, Key]);
dirty_read(Tab, _Key) ->
    abort({bad_type, Tab}).

-doc """
[](){: #dirty_match_object_1 }

Calls `mnesia:dirty_match_object(Tab, Pattern)`, where `Tab` is
[`element(1, Pattern)`](`element/2`).
""".
-spec dirty_match_object(Pattern::tuple()) -> [Record::tuple()].
dirty_match_object(Pat) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    dirty_match_object(Tab, Pat);
dirty_match_object(Pat) ->
    abort({bad_type, Pat}).

-doc "Dirty equivalent of the function `mnesia:match_object/3`.".
-spec dirty_match_object(Tab,Pattern) -> [Record] when
      Tab::table(), Pattern::tuple(), Record::tuple().
dirty_match_object(Tab, Pat)
  when is_atom(Tab), Tab /= schema, is_tuple(Pat), tuple_size(Pat) > 2 ->
    dirty_rpc(Tab, ?MODULE, remote_dirty_match_object, [Tab, Pat]);
dirty_match_object(Tab, Pat) ->
    abort({bad_type, Tab, Pat}).

-doc false.
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

-doc """
[](){: #dirty_select }

Dirty equivalent of the function `mnesia:select/2`.
""".
-spec dirty_select(Tab, Spec) -> [Match] when
      Tab::table(), Spec::ets:match_spec(), Match::term().
dirty_select(Tab, Spec) when is_atom(Tab), Tab /= schema, is_list(Spec) ->
    dirty_rpc(Tab, ?MODULE, remote_dirty_select, [Tab, Spec]);
dirty_select(Tab, Spec) ->
    abort({bad_type, Tab, Spec}).

-doc false.
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

-doc false.
dirty_sel_init(Node,Tab,Spec,NObjects,Type) ->
    do_dirty_rpc(Tab,Node,mnesia_lib,db_select_init,[Type,Tab,Spec,NObjects]).

dirty_sel_cont(#mnesia_select{cont='$end_of_table'}) -> '$end_of_table';
dirty_sel_cont(#mnesia_select{node=Node,tab=Tab,storage=Type,cont=Cont,orig=Ms}) ->
    do_dirty_rpc(Tab,Node,mnesia_lib,db_select_cont,[Type,Cont,Ms]).

-doc """
[](){: #delete_all_keys }

Dirty equivalent of the function `mnesia:all_keys/1`.
""".
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

-doc """
[](){: #dirty_index_match_object_2 }

Starts `mnesia:dirty_index_match_object(Tab, Pattern, Pos)`, where `Tab` is
[`element(1, Pattern)`](`element/2`).
""".
-spec dirty_index_match_object(Pattern, Attr) -> [Record] when
      Pattern::tuple(), Attr::index_attr(), Record::tuple().
dirty_index_match_object(Pat, Attr) when is_tuple(Pat), tuple_size(Pat) > 2 ->
    Tab = element(1, Pat),
    dirty_index_match_object(Tab, Pat, Attr);
dirty_index_match_object(Pat, _Attr) ->
    abort({bad_type, Pat}).

-doc "Dirty equivalent of the function `mnesia:index_match_object/4`.".
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

-doc """
[](){: #dirty_index_read }

Dirty equivalent of the function `mnesia:index_read/3`.
""".
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
-doc false.
dirty_slot(Tab, Slot) when is_atom(Tab), Tab /= schema, is_integer(Slot)  ->
    dirty_rpc(Tab, mnesia_lib, db_slot, [Tab, Slot]);
dirty_slot(Tab, Slot) ->
    abort({bad_type, Tab, Slot}).

-doc """
[](){: #dirty_first }

Records in `set` or `bag` tables are not ordered. However, there is an ordering
of the records that is unknown to the user. Therefore, a table can be traversed
by this function with the function `mnesia:dirty_next/2`.

If there are no records in the table, this function returns the atom
`'$end_of_table'`. It is therefore highly undesirable, but not disallowed, to
use this atom as the key for any user records.
""".
-spec dirty_first(Tab::table()) -> Key::term().
dirty_first(Tab) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_first, [Tab]);
dirty_first(Tab) ->
    abort({bad_type, Tab}).

-doc """
[](){: #dirty_last }

Works exactly like `mnesia:dirty_first/1` but returns the last object in Erlang
term order for the `ordered_set` table type. For all other table types,
`mnesia:dirty_first/1` and `mnesia:dirty_last/1` are synonyms.
""".
-spec dirty_last(Tab::table()) -> Key::term().
dirty_last(Tab) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_last, [Tab]);
dirty_last(Tab) ->
    abort({bad_type, Tab}).

-doc """
[](){: #dirty_next }

Traverses a table and performs operations on all records in the table. When the
end of the table is reached, the special key `'$end_of_table'` is returned.
Otherwise, the function returns a key that can be used to read the actual
record. The behavior is undefined if another Erlang process performs write
operations on the table while it is being traversed with the function
`mnesia:dirty_next/2`.
""".
-spec dirty_next(Tab::table(), Key::_) -> NextKey::term().
dirty_next(Tab, Key) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_next_key, [Tab, Key]);
dirty_next(Tab, _Key) ->
    abort({bad_type, Tab}).

-doc """
[](){: #dirty_prev }

Works exactly like `mnesia:dirty_next/2` but returns the previous object in
Erlang term order for the `ordered_set` table type. For all other table types,
`mnesia:dirty_next/2` and `mnesia:dirty_prev/2` are synonyms.
""".
-spec dirty_prev(Tab::table(), Key::_) -> PrevKey::term().
dirty_prev(Tab, Key) when is_atom(Tab), Tab /= schema ->
    dirty_rpc(Tab, mnesia_lib, db_prev_key, [Tab, Key]);
dirty_prev(Tab, _Key) ->
    abort({bad_type, Tab}).


-doc false.
dirty_rpc(Tab, M, F, Args) ->
    Node = val({Tab, where_to_read}),
    do_dirty_rpc(Tab, Node, M, F, Args).

do_dirty_rpc(_Tab, nowhere, _, _, Args) ->
    mnesia:abort({no_exists, Args});
do_dirty_rpc(_Tab, Local, M, F, Args) when Local =:= node() ->
    try apply(M,F,Args)
    catch
        throw:Res -> Res;
        _:_ -> mnesia:abort({badarg, Args})
    end;
do_dirty_rpc(Tab, Node, M, F, Args) ->
    case mnesia_rpc:call(Node, M, F, Args) of
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
-doc """
[](){: #table_info }

The [`table_info/2`](`table_info/2`) function takes two arguments. The first is
the name of a Mnesia table. The second is one of the following keys:

- `all`. Returns a list of all local table information. Each element is a
  `{InfoKey, ItemVal}` tuple.

  New `InfoItem`s can be added and old undocumented `InfoItem`s can be removed
  without notice.

- `access_mode`. Returns the access mode of the table. The access mode can be
  `read_only` or `read_write`.
- `arity`. Returns the arity of records in the table as specified in the schema.
- `attributes`. Returns the table attribute names that are specified in the
  schema.
- `checkpoints`. Returns the names of the currently active checkpoints, which
  involve this table on this node.
- `cookie`. Returns a table cookie, which is a unique system-generated
  identifier for the table. The cookie is used internally to ensure that two
  different table definitions using the same table name cannot accidentally be
  intermixed. The cookie is generated when the table is created initially.
- `disc_copies`. Returns the nodes where a `disc_copy` of the table resides
  according to the schema.
- `disc_only_copies`. Returns the nodes where a `disc_only_copy` of the table
  resides according to the schema.
- `index`. Returns the list of index position integers for the table.
- `load_node`. Returns the name of the node that Mnesia loaded the table from.
  The structure of the returned value is unspecified, but can be useful for
  debugging purposes.
- `load_order`. Returns the load order priority of the table. It is an integer
  and defaults to `0` (zero).
- `load_reason`. Returns the reason of why Mnesia decided to load the table. The
  structure of the returned value is unspecified, but can be useful for
  debugging purposes.
- `local_content`. Returns `true` or `false` to indicate if the table is
  configured to have locally unique content on each node.
- `master_nodes`. Returns the master nodes of a table.
- `memory`. Returns for `ram_copies` and `disc_copies` tables the number of
  words allocated in memory to the table on this node. For `disc_only_copies`
  tables the number of bytes stored on disc is returned.
- `ram_copies`. Returns the nodes where a `ram_copy` of the table resides
  according to the schema.
- `record_name`. Returns the record name, common for all records in the table.
- `size`. Returns the number of records inserted in the table.
- `snmp`. Returns the SNMP struct. `[]` means that the table currently has no
  SNMP properties.
- `storage_type`. Returns the local storage type of the table. It can be
  `disc_copies`, `ram_copies`, `disc_only_copies`, or the atom `unknown`.
  `unknown` is returned for all tables that only reside remotely.
- `subscribers`. Returns a list of local processes currently subscribing to
  local table events that involve this table on this node.
- `type`. Returns the table type, which is `bag`, `set`, or `ordered_set`.
- `user_properties`. Returns the user-associated table properties of the table.
  It is a list of the stored property records.
- `version`. Returns the current version of the table definition. The table
  version is incremented when the table definition is changed. The table
  definition can be incremented directly when it has been changed in a schema
  transaction, or when a committed table definition is merged with table
  definitions from other nodes during startup.
- `where_to_read`. Returns the node where the table can be read. If value
  `nowhere` is returned, either the table is not loaded or it resides at a
  remote node that is not running.
- `where_to_write`. Returns a list of the nodes that currently hold an active
  replica of the table.
- `wild_pattern`. Returns a structure that can be given to the various match
  functions for a certain table. A record tuple is where all record fields have
  value `'_'`.
""".
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

-doc false.
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

-doc false.
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
                info_reply(Mod:info(Alias, Tab, Item), Tab, Item);
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
-doc "Prints information about all table definitions on the terminal.".
-spec schema() -> 'ok'.
schema() ->
    mnesia_schema:info().

%% Raw info about one tables
-doc "Prints information about one table definition on the terminal.".
-spec schema(Tab::table()) -> 'ok'.
schema(Tab) ->
    mnesia_schema:info(Tab).

-doc """
[](){: #error_description }

All Mnesia transactions, including all the schema update functions, either
return value `{atomic, Val}` or the tuple `{aborted, Reason}`. `Reason` can be
either of the atoms in the following list. The function
[`error_description/1`](`error_description/1`) returns a descriptive string that
describes the error.

- `nested_transaction`. Nested transactions are not allowed in this context.
- `badarg`. Bad or invalid argument, possibly bad type.
- `no_transaction`. Operation not allowed outside transactions.
- `combine_error`. Table options illegally combined.
- `bad_index`. Index already exists, or was out of bounds.
- `already_exists`. Schema option to be activated is already on.
- `index_exists`. Some operations cannot be performed on tables with an index.
- `no_exists`. Tried to perform operation on non-existing (not-alive) item.
- `system_limit`. A system limit was exhausted.
- `mnesia_down`. A transaction involves records on a remote node, which became
  unavailable before the transaction was completed. Records are no longer
  available elsewhere in the network.
- `not_a_db_node`. A node was mentioned that does not exist in the schema.
- `bad_type`. Bad type specified in argument.
- `node_not_running`. Node is not running.
- `truncated_binary_file`. Truncated binary in file.
- `active`. Some delete operations require that all active records are removed.
- `illegal`. Operation not supported on this record.

`Error` can be `Reason`, `{error, Reason}`, or `{aborted, Reason}`. `Reason` can
be an atom or a tuple with `Reason` as an atom in the first field.

The following examples illustrate a function that returns an error, and the
method to retrieve more detailed error information:

- The function [mnesia:create_table(bar, \[\{attributes,
  3.14\}])](`create_table/2`) returns the tuple `{aborted,Reason}`, where
  `Reason` is the tuple `{bad_type,bar,3.14000}`.
- The function [mnesia:error_description(Reason)](`error_description/1`) returns
  the term `{"Bad type on some provided arguments",bar,3.14000}`, which is an
  error description suitable for display.
""".
-spec error_description(Error::term()) -> string().
error_description(Err) ->
    mnesia_lib:error_desc(Err).

-doc """
[](){: #info }

Prints system information on the terminal. This function can be used even if
Mnesia is not started. However, more information is displayed if Mnesia is
started.
""".
-spec info() -> 'ok'.
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

-doc """
[](){: #system_info }

Returns information about the Mnesia system, such as transaction statistics,
`db_nodes`, and configuration parameters. The valid keys are as follows:

- `all`. Returns a list of all local system information. Each element is a
  `{InfoKey, InfoVal}` tuple.

  New `InfoKey`s can be added and old undocumented `InfoKey`s can be removed
  without notice.

- `access_module`. Returns the name of module that is configured to be the
  activity access callback module.
- `auto_repair`. Returns `true` or `false` to indicate if Mnesia is configured
  to start the auto-repair facility on corrupted disc files.
- `backup_module`. Returns the name of the module that is configured to be the
  backup callback module.
- `checkpoints`. Returns a list of the names of the checkpoints currently active
  on this node.
- `event_module`. Returns the name of the module that is the event handler
  callback module.
- `db_nodes`. Returns the nodes that make up the persistent database. Disc-less
  nodes are only included in the list of nodes if they explicitly have been
  added to the schema, for example, with `mnesia:add_table_copy/3`. The function
  can be started even if Mnesia is not yet running.
- `debug`. Returns the current debug level of Mnesia.
- `directory`. Returns the name of the Mnesia directory. It can be called even
  if Mnesia is not yet running.
- `dump_log_load_regulation`. Returns a boolean that tells if Mnesia is
  configured to regulate the dumper process load.

  This feature is temporary and will be removed in future releases.

- `dump_log_time_threshold`. Returns the time threshold for transaction log
  dumps in milliseconds.
- `dump_log_update_in_place`. Returns a boolean that tells if Mnesia is
  configured to perform the updates in the Dets files directly, or if the
  updates are to be performed in a copy of the Dets files.
- `dump_log_write_threshold`. Returns the write threshold for transaction log
  dumps as the number of writes to the transaction log.
- `extra_db_nodes`. Returns a list of extra `db_nodes` to be contacted at
  startup.
- `fallback_activated`. Returns `true` if a fallback is activated, otherwise
  `false`.
- `held_locks`. Returns a list of all locks held by the local Mnesia lock
  manager.
- `is_running`. Returns `yes` or `no` to indicate if Mnesia is running. It can
  also return `starting` or `stopping`. Can be called even if Mnesia is not yet
  running.
- `local_tables`. Returns a list of all tables that are configured to reside
  locally.
- `lock_queue`. Returns a list of all transactions that are queued for execution
  by the local lock manager.
- `log_version`. Returns the version number of the Mnesia transaction log
  format.
- `master_node_tables`. Returns a list of all tables with at least one master
  node.
- `protocol_version`. Returns the version number of the Mnesia inter-process
  communication protocol.
- `running_db_nodes`. Returns a list of nodes where Mnesia currently is running.
  This function can be called even if Mnesia is not yet running, but it then has
  slightly different semantics.

  If Mnesia is down on the local node, the function returns those other
  `db_nodes` and `extra_db_nodes` that for the moment are operational.

  If Mnesia is started, the function returns those nodes that Mnesia on the
  local node is fully connected to. Only those nodes that Mnesia has exchanged
  schema information with are included as `running_db_nodes`. After the merge of
  schemas, the local Mnesia system is fully operable and applications can
  perform access of remote replicas. Before the schema merge, Mnesia only
  operates locally. Sometimes there are more nodes included in the
  `running_db_nodes` list than all `db_nodes` and `extra_db_nodes` together.

- `schema_location`. Returns the initial schema location.
- `subscribers`. Returns a list of local processes currently subscribing to
  system events.
- `tables`. Returns a list of all locally known tables.
- `transactions`. Returns a list of all currently active local transactions.
- `transaction_failures`. Returns a number that indicates how many transactions
  have failed since Mnesia was started.
- `transaction_commits`. Returns a number that indicates how many transactions
  have terminated successfully since Mnesia was started.
- `transaction_restarts`. Returns a number that indicates how many transactions
  have been restarted since Mnesia was started.
- `transaction_log_writes`. Returns a number that indicates how many write
  operations that have been performed to the transaction log since startup.
- `use_dir`. Returns a boolean that indicates if the Mnesia directory is used or
  not. Can be started even if Mnesia is not yet running.
- `version`. Returns the current version number of Mnesia.
""".
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
system_info2(max_transfer_size) -> mnesia_monitor:get_env(max_transfer_size);

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
     max_transfer_size,
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

-doc false.
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

-doc """
[](){: #create_schema }

Creates a new database on disc. Various files are created in the local Mnesia
directory of each node. Notice that the directory must be unique for each node.
Two nodes must never share the same directory. If possible, use a local disc
device to improve performance.

`mnesia:create_schema/1` fails if any of the Erlang nodes given as `DiscNodes`
are not alive, if Mnesia is running on any of the nodes, or if any of the nodes
already have a schema. Use `mnesia:delete_schema/1` to get rid of old faulty
schemas.

Notice that only nodes with disc are to be included in `DiscNodes`. Disc-less
nodes, that is, nodes where all tables including the schema only resides in RAM,
must not be included.
""".
-spec create_schema(Ns::[node()]) -> result().
create_schema(Ns) ->
    create_schema(Ns, []).

-doc false.
-spec create_schema(Ns::[node()], [Prop]) -> result() when
      Prop :: BackendType | IndexPlugin,
      BackendType :: {'backend_types', [{Name::atom(), Module::module()}]},
      IndexPlugin :: {'index_plugins', [{{Name::atom()}, Module::module(), Function::atom()}]}.
create_schema(Ns, Properties) ->
    mnesia_bup:create_schema(Ns, Properties).

-doc """
[](){: #delete_schema }

Deletes a database created with `mnesia:create_schema/1`.
`mnesia:delete_schema/1` fails if any of the Erlang nodes given as `DiscNodes`
are not alive, or if Mnesia is running on any of the nodes.

After the database is deleted, it can still be possible to start Mnesia as a
disc-less node. This depends on how configuration parameter `schema_location` is
set.

> #### Warning {: .warning }
>
> Use this function with extreme caution, as it makes existing persistent data
> obsolete. Think twice before using it.
""".
-spec delete_schema(Ns::[node()]) -> result().
delete_schema(Ns) ->
    mnesia_schema:delete_schema(Ns).

-doc false.
-spec add_backend_type(Name::atom(), Module::module()) -> t_result('ok').
add_backend_type(Alias, Module) ->
    mnesia_schema:add_backend_type(Alias, Module).

-doc(#{equiv => backup/2}).
-spec backup(Dest::term()) -> result().
backup(Opaque) ->
    mnesia_log:backup(Opaque).

-doc """
[](){: #backup }

Activates a new checkpoint covering all Mnesia tables, including the schema,
with maximum degree of redundancy, and performs a backup using
`backup_checkpoint/2/3`. The default value of the backup callback module
`BackupMod` is obtained by `mnesia:system_info(backup_module)`.
""".
-spec backup(Dest::term(), Mod::module()) ->
          result().
backup(Opaque, Mod) ->
    mnesia_log:backup(Opaque, Mod).

-doc(#{equiv => traverse_backup/6}).
-spec traverse_backup(Src::term(), Dest::term(), Fun, Acc) ->
                             {'ok', Acc} | {'error', Reason::term()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
traverse_backup(S, T, Fun, Acc) ->
    mnesia_bup:traverse_backup(S, T, Fun, Acc).

-doc """
[](){: #traverse_backup }

Iterates over a backup, either to transform it into a new backup, or read it.
The arguments are explained briefly here. For details, see the User's Guide.

- `SourceMod` and `TargetMod` are the names of the modules that actually access
  the backup media.
- `Source` and `Target` are opaque data used exclusively by modules `SourceMod`
  and `TargetMod` to initialize the backup media.
- `Acc` is an initial accumulator value.
- `Fun(BackupItems, Acc)` is applied to each item in the backup. The `Fun` must
  return a tuple `{BackupItems,NewAcc}`, where `BackupItems` is a list of valid
  backup items, and `NewAcc` is a new accumulator value. The returned backup
  items are written in the target backup.
- `LastAcc` is the last accumulator value. This is the last `NewAcc` value that
  was returned by `Fun`.
""".
-spec traverse_backup(Src::term(), SrcMod::module(),
                      Dest::term(), DestMod::module(),
                      Fun, Acc) ->
                             {'ok', Acc} | {'error', Reason::term()} when
      Fun :: fun((Items, Acc) -> {Items,Acc}).
traverse_backup(S, SM, T, TM, F, A) ->
    mnesia_bup:traverse_backup(S, SM, T, TM, F, A).

-doc """
[](){: #install_fallback_1 }

Calls `mnesia:install_fallback(Opaque, Args)`, where `Args` is
`[{scope, global}]`.
""".
-spec install_fallback(Src::term()) -> result().
install_fallback(Opaque) ->
    mnesia_bup:install_fallback(Opaque).

-doc """
Installs a backup as fallback. The fallback is used to restore the database at
the next startup. Installation of fallbacks requires Erlang to be operational on
all the involved nodes, but it does not matter if Mnesia is running or not. The
installation of the fallback fails if the local node is not one of the
disc-resident nodes in the backup.

`Args` is a list of the following tuples:

- `{module, BackupMod}`. All accesses of the backup media are performed through
  a callback module named `BackupMod`. Argument `Opaque` is forwarded to the
  callback module, which can interpret it as it wishes. The default callback
  module is called `mnesia_backup` and it interprets argument `Opaque` as a
  local filename. The default for this module is also configurable through
  configuration parameter `-mnesia mnesia_backup`.
- `{scope, Scope}`. The `Scope` of a fallback is either `global` for the entire
  database or `local` for one node. By default, the installation of a fallback
  is a global operation, which either is performed on all nodes with a
  disc-resident schema or none. Which nodes that are disc-resident is determined
  from the schema information in the backup.

  If `Scope` of the operation is `local`, the fallback is only installed on the
  local node.

- `{mnesia_dir, AlternateDir}`. This argument is only valid if the scope of the
  installation is `local`. Normally the installation of a fallback is targeted
  to the Mnesia directory, as configured with configuration parameter
  `-mnesia dir`. But by explicitly supplying an `AlternateDir`, the fallback is
  installed there regardless of the Mnesia directory configuration parameter
  setting. After installation of a fallback on an alternative Mnesia directory,
  that directory is fully prepared for use as an active Mnesia directory.

  This is a dangerous feature that must be used with care. By unintentional
  mixing of directories, you can easily end up with an inconsistent database, if
  the same backup is installed on more than one directory.
""".
-spec install_fallback(Src::term(), Mod::module()|[Opt]) ->
          result() when
      Opt :: Module | Scope | Dir,
      Module :: {'module', Mod::module()},
      Scope :: {'scope', 'global' | 'local'},
      Dir :: {'mnesia_dir', Dir::string()}.
install_fallback(Opaque, Mod) ->
    mnesia_bup:install_fallback(Opaque, Mod).

-doc """
[](){: #uninstall_fallback_0 }

Calls the function `mnesia:uninstall_fallback([{scope, global}])`.
""".
-spec uninstall_fallback() -> result().
uninstall_fallback() ->
    mnesia_bup:uninstall_fallback().

-doc """
Deinstalls a fallback before it has been used to restore the database. This is
normally a distributed operation that is either performed on all nodes with disc
resident schema, or none. Uninstallation of fallbacks requires Erlang to be
operational on all involved nodes, but it does not matter if Mnesia is running
or not. Which nodes that are considered as disc-resident nodes is determined
from the schema information in the local fallback.

`Args` is a list of the following tuples:

- `{module, BackupMod}`. For semantics, see `mnesia:install_fallback/2`.
- `{scope, Scope}`. For semantics, see `mnesia:install_fallback/2`.
- `{mnesia_dir, AlternateDir}`. For semantics, see `mnesia:install_fallback/2`.
""".
-spec uninstall_fallback(Args) -> result() when
      Args :: [{'mnesia_dir', Dir::string()}].
uninstall_fallback(Args) ->
    mnesia_bup:uninstall_fallback(Args).

-doc """
[](){: #activate_checkpoint }

A checkpoint is a consistent view of the system. A checkpoint can be activated
on a set of tables. This checkpoint can then be traversed and presents a view of
the system as it existed at the time when the checkpoint was activated, even if
the tables are being or have been manipulated.

`Args` is a list of the following tuples:

- `{name,Name}`. `Name` is the checkpoint name. Each checkpoint must have a name
  that is unique to the associated nodes. The name can be reused only once the
  checkpoint has been deactivated. By default, a name that is probably unique is
  generated.
- `{max,MaxTabs}`. `MaxTabs` is a list of tables that are to be included in the
  checkpoint. Default is `[]`. For these tables, the redundancy is maximized and
  checkpoint information is retained together with all replicas. The checkpoint
  becomes more fault tolerant if the tables have several replicas. When a new
  replica is added by the schema manipulation function
  `mnesia:add_table_copy/3`, a retainer is also attached automatically.
- `{min,MinTabs}`. `MinTabs` is a list of tables that are to be included in the
  checkpoint. Default is []. For these tables, the redundancy is minimized and
  the checkpoint information is only retained with one replica, preferably on
  the local node.
- `{allow_remote,Bool}`. `false` means that all retainers must be local. The
  checkpoint cannot be activated if a table does not reside locally. `true`
  allows retainers to be allocated on any node. Default is `true`.
- `{ram_overrides_dump,Bool}`. Only applicable for `ram_copies`. `Bool` allows
  you to choose to back up the table state as it is in RAM, or as it is on disc.
  `true` means that the latest committed records in RAM are to be included in
  the checkpoint. These are the records that the application accesses. `false`
  means that the records dumped to `DAT` files are to be included in the
  checkpoint. These records are loaded at startup. Default is `false`.

Returns `{ok,Name,Nodes}` or `{error,Reason}`. `Name` is the (possibly
generated) checkpoint name. `Nodes` are the nodes that are involved in the
checkpoint. Only nodes that keep a checkpoint retainer know about the
checkpoint.
""".
-spec activate_checkpoint([Arg]) -> {'ok', Name, [node()]} | {'error', Reason::term()} when
      Arg :: {'name', Name} | {'max', [table()]} | {'min', [table()]} |
             {'allow_remote', boolean()} | {'ram_overrides_dump', boolean()}.
activate_checkpoint(Args) ->
    mnesia_checkpoint:activate(Args).

-doc """
[](){: #deactivate_checkpoint }

The checkpoint is automatically deactivated when some of the tables involved
have no retainer attached to them. This can occur when nodes go down or when a
replica is deleted. Checkpoints are also deactivated with this function. `Name`
is the name of an active checkpoint.
""".
-spec deactivate_checkpoint(Name::_) -> result().
deactivate_checkpoint(Name) ->
    mnesia_checkpoint:deactivate(Name).

-doc(#{equiv => backup_checkpoint/3}).
-spec backup_checkpoint(Name, Dest) -> result() when
      Name :: term(), Dest :: term().
backup_checkpoint(Name, Opaque) ->
    mnesia_log:backup_checkpoint(Name, Opaque).

-doc """
[](){: #backup_checkpoint }

The tables are backed up to external media using backup module `BackupMod`.
Tables with the local contents property are backed up as they exist on the
current node. `BackupMod` is the default backup callback module obtained by
`mnesia:system_info(backup_module)`. For information about the exact callback
interface (the `mnesia_backup behavior`), see the User's Guide.
""".
-spec backup_checkpoint(Name, Dest, Mod) -> result() when
      Name :: term(), Dest :: term(), Mod :: module().
backup_checkpoint(Name, Opaque, Mod) ->
    mnesia_log:backup_checkpoint(Name, Opaque, Mod).

-doc """
[](){: #restore }

With this function, tables can be restored online from a backup without
restarting Mnesia. `Opaque` is forwarded to the backup module. `Args` is a list
of the following tuples:

- `{module,BackupMod}`. The backup module `BackupMod` is used to access the
  backup media. If omitted, the default backup module is used.
- `{skip_tables, TabList}`, where `TabList` is a list of tables that is not to
  be read from the backup.
- `{clear_tables, TabList}`, where `TabList` is a list of tables that is to be
  cleared before the records from the backup are inserted. That is, all records
  in the tables are deleted before the tables are restored. Schema information
  about the tables is not cleared or read from the backup.
- `{keep_tables, TabList}`, where `TabList` is a list of tables that is not to
  be cleared before the records from the backup are inserted. That is, the
  records in the backup are added to the records in the table. Schema
  information about the tables is not cleared or read from the backup.
- `{recreate_tables, TabList}`, where `TabList` is a list of tables that is to
  be recreated before the records from the backup are inserted. The tables are
  first deleted and then created with the schema information from the backup.
  All the nodes in the backup need to be operational.
- `{default_op, Operation}`, where `Operation` is either of the operations
  `skip_tables`, `clear_tables`, `keep_tables`, or `recreate_tables`. The
  default operation specifies which operation that is to be used on tables from
  the backup that is not specified in any of the mentioned lists. If omitted,
  operation `clear_tables` is used.

The affected tables are write-locked during the restoration. However, regardless
of the lock conflicts caused by this, the applications can continue to do their
work while the restoration is being performed. The restoration is performed as
one single transaction.

If the database is huge, it it not always possible to restore it online. In such
cases, restore the old database by installing a fallback and then restart.
""".
-spec restore(Src::_, [Arg]) -> t_result([table()]) when
      Op  :: 'skip_tables' | 'clear_tables' | 'keep_tables' | 'restore_tables',
      Arg :: {'module', module()} | {Op, [table()]} | {'default_op', Op}.
restore(Opaque, Args) ->
    mnesia_schema:restore(Opaque, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt
-doc false.
-spec create_table([Arg]) -> t_result('ok') when
      Arg :: {'name', table()} | create_option().
create_table(Arg) ->
    mnesia_schema:create_table(Arg).

-doc """
[](){: #create_table }

Creates a Mnesia table called `Name` according to argument `TabDef`. This list
must be a list of `{Item, Value}` tuples, where the following values are
allowed:

- `{access_mode, Atom}`. The access mode is by default the atom `read_write` but
  it can also be set to the atom `read_only`. If `AccessMode` is set to
  `read_only`, updates to the table cannot be performed.

  At startup, Mnesia always loads `read_only` table locally regardless of when
  and if Mnesia is terminated on other nodes. This argument returns the access
  mode of the table. The access mode can be `read_only` or `read_write`.

- `{attributes, AtomList}` is a list of the attribute names for the records that
  are supposed to populate the table. Default is `[key, val]`. The table must at
  least have one extra attribute in addition to the key.

  When accessing single attributes in a record, it is not necessary, or even
  recommended, to hard code any attribute names as atoms. Use construct
  `record_info(fields, RecordName)` instead. It can be used for records of type
  `RecordName`.

- `{disc_copies, Nodelist}`, where `Nodelist` is a list of the nodes where this
  table is supposed to have disc copies. If a table replica is of type
  `disc_copies`, all write operations on this particular replica of the table
  are written to disc and to the RAM copy of the table.

  It is possible to have a replicated table of type `disc_copies` on one node
  and another type on another node. Default is `[]`.

- `{disc_only_copies, Nodelist}`, where `Nodelist` is a list of the nodes where
  this table is supposed to have `disc_only_copies`. A disc only table replica
  is kept on disc only and unlike the other replica types, the contents of the
  replica do not reside in RAM. These replicas are considerably slower than
  replicas held in RAM.
- `{index, Intlist}`, where `Intlist` is a list of attribute names (atoms) or
  record fields for which Mnesia is to build and maintain an extra index table.
  The `qlc` query compiler _may_ be able to optimize queries if there are
  indexes available.
- `{load_order, Integer}`. The load order priority is by default `0` (zero) but
  can be set to any integer. The tables with the highest load order priority are
  loaded first at startup.
- `{majority, Flag}`, where `Flag` must be a boolean. If `true`, any (non-dirty)
  update to the table is aborted, unless a majority of the table replicas are
  available for the commit. When used on a fragmented table, all fragments are
  given the same majority setting.
- `{ram_copies, Nodelist}`, where `Nodelist` is a list of the nodes where this
  table is supposed to have RAM copies. A table replica of type `ram_copies` is
  not written to disc on a per transaction basis. `ram_copies` replicas can be
  dumped to disc with the function `mnesia:dump_tables(Tabs)`. Default value for
  this attribute is `[node()]`.
- `{record_name, Name}`, where `Name` must be an atom. All records stored in the
  table must have this name as the first element. It defaults to the same name
  as the table name.
- `{snmp, SnmpStruct}`. For a description of `SnmpStruct`, see
  `mnesia:snmp_open_table/2`. If this attribute is present in `ArgList` to
  `mnesia:create_table/2`, the table is immediately accessible by SNMP.
  Therefore applications that use SNMP to manipulate and control the system can
  be designed easily, since Mnesia provides a direct mapping between the logical
  tables that make up an SNMP control application and the physical data that
  makes up a Mnesia table.
- `{storage_properties, [{Backend, Properties}]` forwards more properties to the
  back end storage. `Backend` can currently be `ets` or `dets`. `Properties` is
  a list of options sent to the back end storage during table creation.
  `Properties` cannot contain properties already used by Mnesia, such as `type`
  or `named_table`.

  For example:

  ```erlang
  mnesia:create_table(table, [{ram_copies, [node()]}, {disc_only_copies, nodes()},
         {storage_properties,
          [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}])
  ```

- `{type, Type}`, where `Type` must be either of the atoms `set`, `ordered_set`,
  or `bag`. Default is `set`. In a `set`, all records have unique keys. In a
  `bag`, several records can have the same key, but the record content is
  unique. If a non-unique record is stored, the old conflicting records are
  overwritten.

  Notice that currently `ordered_set` is not supported for `disc_only_copies`.

- `{local_content, Bool}`, where `Bool` is `true` or `false`. Default is
  `false`.

For example, the following call creates the `person` table (defined earlier) and
replicates it on two nodes:

```erlang
mnesia:create_table(person,
    [{ram_copies, [N1, N2]},
     {attributes, record_info(fields, person)}]).
```

If it is required that Mnesia must build and maintain an extra index table on
attribute `address` of all the `person` records that are inserted in the table,
the following code would be issued:

```erlang
mnesia:create_table(person,
    [{ram_copies, [N1, N2]},
     {index, [address]},
     {attributes, record_info(fields, person)}]).
```

The specification of `index` and `attributes` can be hard-coded as
`{index, [2]}` and `{attributes, [name, age, address, salary, children]}`,
respectively.

`mnesia:create_table/2` writes records into the table `schema`. This function,
and all other schema manipulation functions, are implemented with the normal
transaction management system. This guarantees that schema updates are performed
on all nodes in an atomic manner.
""".
-spec create_table(Name::table(), [create_option()]) -> t_result('ok').
create_table(Name, Arg) when is_list(Arg) ->
    mnesia_schema:create_table([{name, Name}| Arg]);
create_table(Name, Arg) ->
    {aborted, {badarg, Name, Arg}}.

-doc """
[](){: #delete_table }

Permanently deletes all replicas of table `Tab`.
""".
-spec delete_table(Tab::table()) -> t_result('ok').
delete_table(Tab) ->
    mnesia_schema:delete_table(Tab).

-doc """
[](){: #add_table_copy }

Makes another copy of a table at the node `Node`. Argument `Type` must be either
of the atoms `ram_copies`, `disc_copies`, or `disc_only_copies`. For example,
the following call ensures that a disc replica of the `person` table also exists
at node `Node`:

```text
mnesia:add_table_copy(person, Node, disc_copies)
```

This function can also be used to add a replica of the table named `schema`.
""".
-spec add_table_copy(Tab, N, ST) -> t_result('ok') when
      Tab :: table(), N::node(), ST::storage_type().
add_table_copy(Tab, N, S) ->
    mnesia_schema:add_table_copy(Tab, N, S).

-doc """
[](){: #del_table_copy }

Deletes the replica of table `Tab` at node `Node`. When the last replica is
deleted with this function, the table disappears entirely.

This function can also be used to delete a replica of the table named `schema`.
The Mnesia node is then removed. Notice that Mnesia must be stopped on the node
first.
""".
-spec del_table_copy(Tab::table(), N::node()) -> t_result('ok').
del_table_copy(Tab, N) ->
    mnesia_schema:del_table_copy(Tab, N).

-doc """
[](){: #move_table_copy }

Moves the copy of table `Tab` from node `From` to node `To`.

The storage type is preserved. For example, a RAM table moved from one node
remains a RAM on the new node. Other transactions can still read and write in
the table while it is being moved.

This function cannot be used on `local_content` tables.
""".
-spec move_table_copy(Tab::table(), From::node(), To::node()) -> t_result('ok').
move_table_copy(Tab, From, To) ->
    mnesia_schema:move_table(Tab, From, To).

-doc """
[](){: #add_table_index }

Table indexes can be used whenever the user wants to use frequently some other
field than the key field to look up records. If this other field has an
associated index, these lookups can occur in constant time and space. For
example, if your application wishes to use field `age` to find efficiently all
persons with a specific age, it can be a good idea to have an index on field
`age`. This can be done with the following call:

```text
mnesia:add_table_index(person, age)
```

Indexes do not come for free. They occupy space that is proportional to the
table size, and they cause insertions into the table to execute slightly slower.
""".
-spec add_table_index(Tab, I) -> t_result('ok') when
      Tab :: table(), I :: index_attr().
add_table_index(Tab, Ix) ->
    mnesia_schema:add_table_index(Tab, Ix).
-doc """
[](){: #del_table_index }

Deletes the index on attribute with name `AttrName` in a table.
""".
-spec del_table_index(Tab, I) -> t_result('ok') when
      Tab::table(), I::index_attr().
del_table_index(Tab, Ix) ->
    mnesia_schema:del_table_index(Tab, Ix).

-doc """
Calls `mnesia:transform_table(Tab, Fun, NewAttributeList, RecName)`, where
`RecName` is `mnesia:table_info(Tab, record_name)`.
""".
-spec transform_table(Tab::table(), Fun, [Attr]) -> t_result('ok') when
      Attr :: atom(),
      Fun:: fun((Record::tuple()) -> Transformed::tuple()) | ignore.
transform_table(Tab, Fun, NewA) ->
    try val({Tab, record_name}) of
	OldRN -> mnesia_schema:transform_table(Tab, Fun, NewA, OldRN)
    catch exit:Reason ->
	    mnesia:abort(Reason)
    end.

-doc """
[](){: #transform_table_4 }

Applies argument `Fun` to all records in the table. `Fun` is a function that
takes a record of the old type and returns a transformed record of the new type.
Argument `Fun` can also be the atom `ignore`, which indicates that only the
metadata about the table is updated. Use of `ignore` is not recommended, but
included as a possibility for the user do to an own transformation.

`NewAttributeList` and `NewRecordName` specify the attributes and the new record
type of the converted table. Table name always remains unchanged. If
`record_name` is changed, only the Mnesia functions that use table identifiers
work, for example, `mnesia:write/3` works, but not `mnesia:write/1`.
""".
-spec transform_table(Tab::table(), Fun, [Attr], RecName) -> t_result('ok') when
      RecName :: atom(),
      Attr :: atom(),
      Fun:: fun((Record::tuple()) -> Transformed::tuple()) | ignore.
transform_table(Tab, Fun, NewA, NewRN) ->
    mnesia_schema:transform_table(Tab, Fun, NewA, NewRN).

-doc """
[](){: #change_table_copy_type }

For example:

```erlang
mnesia:change_table_copy_type(person, node(), disc_copies)
```

Transforms the `person` table from a RAM table into a disc-based table at
`Node`.

This function can also be used to change the storage type of the table named
`schema`. The schema table can only have `ram_copies` or `disc_copies` as the
storage type. If the storage type of the schema is `ram_copies`, no other table
can be disc-resident on that node.
""".
-spec change_table_copy_type(Tab::table(), Node::node(), To::storage_type()) -> t_result('ok').
change_table_copy_type(T, N, S) ->
    mnesia_schema:change_table_copy_type(T, N, S).

-doc """
[](){: #clear_table }

Deletes all entries in the table `Tab`.
""".
-spec clear_table(Tab::table()) -> t_result('ok').
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

-doc false.
clear_table(Tid, Ts, Tab, Obj) when element(1, Tid) =:= tid ->
    Store = Ts#tidstore.store,
    mnesia_locker:wlock_table(Tid, Store, Tab),
    Oid = {Tab, '_'},
    ?ets_insert(Store, {Oid, Obj, clear_table}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt - user properties
-doc false.
-spec read_table_property(Tab::table(), PropKey::term()) -> Res::tuple().
read_table_property(Tab, PropKey) ->
    val({Tab, user_property, PropKey}).

-doc false.
-spec write_table_property(Tab::table(), Prop::tuple()) -> t_result('ok').
write_table_property(Tab, Prop) ->
    mnesia_schema:write_table_property(Tab, Prop).

-doc false.
-spec delete_table_property(Tab::table(), PropKey::term()) -> t_result('ok').
delete_table_property(Tab, PropKey) ->
    mnesia_schema:delete_table_property(Tab, PropKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt - user properties

-doc false.
-spec change_table_frag(Tab::table(), FP::term()) -> t_result('ok').
change_table_frag(Tab, FragProp) ->
    mnesia_schema:change_table_frag(Tab, FragProp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table mgt - table load

%% Dump a ram table to disc
-doc """
[](){: #dump_tables }

Dumps a set of `ram_copies` tables to disc. The next time the system is started,
these tables are initiated with the data found in the files that are the result
of this dump. None of the tables can have disc-resident replicas.
""".
-spec dump_tables([Tab::table()]) -> t_result('ok').
dump_tables(Tabs) ->
    mnesia_schema:dump_tables(Tabs).

%% allow the user to wait for some tables to be loaded
-doc """
[](){: #wait_for_tables }

Some applications need to wait for certain tables to be accessible to do useful
work. `mnesia:wait_for_tables/2` either hangs until all tables in `TabList` are
accessible, or until `timeout` is reached.
""".
-spec wait_for_tables([Tab::table()], TMO::timeout()) ->
      result() | {'timeout', [table()]}.
wait_for_tables(Tabs, Timeout) ->
    mnesia_controller:wait_for_tables(Tabs, Timeout).

-doc """
[](){: #force_load_table }

The Mnesia algorithm for table load can lead to a situation where a table cannot
be loaded. This situation occurs when a node is started and Mnesia concludes, or
suspects, that another copy of the table was active after this local copy became
inactive because of a system crash.

If this situation is not acceptable, this function can be used to override the
strategy of the Mnesia table load algorithm. This can lead to a situation where
some transaction effects are lost with an inconsistent database as result, but
for some applications high availability is more important than consistent data.
""".
-spec force_load_table(Tab::table()) -> 'yes' | {'error', Reason::term()}.
force_load_table(Tab) ->
    case mnesia_controller:force_load_table(Tab) of
	ok -> yes; % Backwards compatibility
	Other -> Other
    end.

-doc """
[](){: #change_table_access_mode }

`AcccessMode` is by default the atom `read_write` but it can also be set to the
atom `read_only`. If `AccessMode` is set to `read_only`, updates to the table
cannot be performed. At startup, Mnesia always loads `read_only` tables locally
regardless of when and if Mnesia is terminated on other nodes.
""".
-spec change_table_access_mode(Tab::table(), Mode) -> t_result('ok') when
      Mode :: 'read_only'|'read_write'.
change_table_access_mode(T, Access) ->
    mnesia_schema:change_table_access_mode(T, Access).

-doc """
[](){: #change_table_load_order }

The `LoadOrder` priority is by default `0` (zero) but can be set to any integer.
The tables with the highest `LoadOrder` priority are loaded first at startup.
""".
-spec change_table_load_order(Tab::table(), Order) -> t_result('ok') when
      Order :: non_neg_integer().
change_table_load_order(T, O) ->
    mnesia_schema:change_table_load_order(T, O).

-doc """
`Majority` must be a boolean. Default is `false`. When `true`, a majority of the
table replicas must be available for an update to succeed. When used on
fragmented tables, `Tab` must be the base table name. Directly changing the
majority setting on individual fragments is not allowed.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec change_table_majority(Tab::table(), M::boolean()) -> t_result('ok').
change_table_majority(T, M) ->
    mnesia_schema:change_table_majority(T, M).

-doc """
[](){: #set_master_nodes_1 }

For each table Mnesia determines its replica nodes (`TabNodes`) and starts
`mnesia:set_master_nodes(Tab, TabMasterNodes)`. where `TabMasterNodes` is the
intersection of `MasterNodes` and `TabNodes`. For semantics, see
`mnesia:set_master_nodes/2`.
""".
-spec set_master_nodes(Ns::[node()]) -> result().
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

-doc """
[](){: #set_master_nodes_2 }

If the application detects a communication failure (in a potentially partitioned
network) that can have caused an inconsistent database, it can use the function
`mnesia:set_master_nodes(Tab, MasterNodes)` to define from which nodes each
table is to be loaded. At startup, the Mnesia normal table load algorithm is
bypassed and the table is loaded from one of the master nodes defined for the
table, regardless of when and if Mnesia terminated on other nodes. `MasterNodes`
can only contain nodes where the table has a replica. If the `MasterNodes` list
is empty, the master node recovery mechanism for the particular table is reset,
and the normal load mechanism is used at the next restart.

The master node setting is always local. It can be changed regardless if Mnesia
is started or not.

The database can also become inconsistent if configuration parameter
`max_wait_for_decision` is used or if `mnesia:force_load_table/1` is used.
""".
-spec set_master_nodes(Tab::table(), Ns::[node()]) -> result().
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
-doc """
[](){: #dump_log }

Performs a user-initiated dump of the local log file. This is usually not
necessary, as Mnesia by default manages this automatically. See configuration
parameters [dump_log_time_threshold](`m:mnesia#dump_log_time_threshold`) and
[dump_log_write_threshold](`m:mnesia#dump_log_write_threshold`).
""".
-spec dump_log() -> 'dumped'.
dump_log() ->
    mnesia_controller:sync_dump_log(user).

-doc """
Ensures that the local transaction log file is synced to disk. On a single node
system, data written to disk tables since the last dump can be lost if there is
a power outage. See `dump_log/0`.
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec sync_log() -> result().
sync_log() ->
    mnesia_monitor:sync_log(latest_log).

-doc """
[](){: #subscribe }

Ensures that a copy of all events of type `EventCategory` is sent to the caller.
The available event types are described in the
[User's Guide](mnesia_chap5.md#event_handling).
""".
-spec subscribe(What) -> {'ok', node()} | {'error', Reason::term()} when
      What :: 'system' | 'activity' | {'table', table(), 'simple' | 'detailed'}.
subscribe(What) ->
    mnesia_subscr:subscribe(self(), What).

-doc """
[](){: #unsubscribe }

Stops sending events of type `EventCategory` to the caller.

`Node` is the local node.
""".
-spec unsubscribe(What) -> {'ok', node()} | {'error', Reason::term()} when
      What :: 'system' | 'activity' | {'table', table(), 'simple' | 'detailed'}.
unsubscribe(What) ->
    mnesia_subscr:unsubscribe(self(), What).

-doc """
[](){: #report_event }

When tracing a system of Mnesia applications it is useful to be able to
interleave Mnesia own events with application-related events that give
information about the application context.

Whenever the application begins a new and demanding Mnesia task, or if it enters
a new interesting phase in its execution, it can be a good idea to use
`mnesia:report_event/1`. `Event` can be any term and generates a
`{mnesia_user, Event}` event for any processes that subscribe to Mnesia system
events.
""".
-spec report_event(Event::_) -> 'ok'.
report_event(Event) ->
    mnesia_lib:report_system_event({mnesia_user, Event}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Snmp
-doc """
A direct one-to-one mapping can be established between Mnesia tables and SNMP
tables. Many telecommunication applications are controlled and monitored by the
SNMP protocol. This connection between Mnesia and SNMP makes it simple and
convenient to achieve this mapping.

Argument `SnmpStruct` is a list of SNMP information. Currently, the only
information needed is information about the key types in the table. Multiple
keys cannot be handled in Mnesia, but many SNMP tables have multiple keys.
Therefore, the following convention is used: if a table has multiple keys, these
must always be stored as a tuple of the keys. Information about the key types is
specified as a tuple of atoms describing the types. The only significant type is
`fix_string`. This means that a string has a fixed size.

For example, the following causes table `person` to be ordered as an SNMP table:

```text
mnesia:snmp_open_table(person, [{key, string}])
```

Consider the following schema for a table of company employees. Each employee is
identified by department number and name. The other table column stores the
telephone number:

```erlang
mnesia:create_table(employee,
    [{snmp, [{key, {integer, string}}]},
     {attributes, record_info(fields, employees)}]),
```

The corresponding SNMP table would have three columns: `department`, `name`, and
`telno`.

An option is to have table columns that are not visible through the SNMP
protocol. These columns must be the last columns of the table. In the previous
example, the SNMP table could have columns `department` and `name` only. The
application could then use column `telno` internally, but it would not be
visible to the SNMP managers.

In a table monitored by SNMP, all elements must be integers, strings, or lists
of integers.

When a table is SNMP ordered, modifications are more expensive than usual,
O(logN). Also, more memory is used.

Notice that only the lexicographical SNMP ordering is implemented in Mnesia, not
the actual SNMP monitoring.
""".
-spec snmp_open_table(Tab::table(), Snmp::snmp_struct()) -> 'ok'.
snmp_open_table(Tab, Us) ->
    mnesia_schema:add_snmp(Tab, Us).

-doc "Removes the possibility for SNMP to manipulate the table.".
-spec snmp_close_table(Tab::table()) -> 'ok'.
snmp_close_table(Tab) ->
    mnesia_schema:del_snmp(Tab).

-doc """
Reads a row by its SNMP index. This index is specified as an SNMP Object
Identifier, a list of integers.
""".
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
-doc """
`RowIndex` can specify a non-existing row. Specifically, it can be the empty
list. Returns the index of the next lexicographical row. If `RowIndex` is the
empty list, this function returns the index of the first row in the table.
""".
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
-doc """
Transforms an SNMP index to the corresponding Mnesia key. If the SNMP table has
multiple keys, the key is a tuple of the key columns.
""".
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
-doc """
[](){: #load_textfile }

Loads a series of definitions and data found in the text file (generated with
`mnesia:dump_to_textfile/1`) into Mnesia. This function also starts Mnesia and
possibly creates a new schema. This function is intended for educational
purposes only. It is recommended to use other functions to deal with real
backups.
""".
-spec load_textfile(File::file:filename()) -> t_result('ok') | {'error', term()}.
load_textfile(F) ->
    mnesia_text:load_textfile(F).

-doc """
[](){: #dump_to_textfile }

Dumps all local tables of a Mnesia system into a text file, which can be edited
(by a normal text editor) and then be reloaded with `mnesia:load_textfile/1`.
Only use this function for educational purposes. Use other functions to deal
with real backups.
""".
-spec dump_to_textfile(File :: file:filename()) -> result() | 'error'.
dump_to_textfile(F) ->
    mnesia_text:dump_to_textfile(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QLC Handles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-doc(#{equiv => table/2}).
-spec table(Tab::table()) -> qlc:query_handle().
table(Tab) ->
    table(Tab, []).

-doc """
[](){: #table }

Returns a Query List Comprehension (QLC) query handle, see the `m:qlc` manual
page in STDLIB. The module `qlc` implements a query language that can use Mnesia
tables as sources of data. Calling `mnesia:table/1,2` is the means to make the
`mnesia` table `Tab` usable to QLC.

`Option` can contain Mnesia options or QLC options. Mnesia recognizes the
following options (any other option is forwarded to QLC).

- `{lock, Lock}`, where `lock` can be `read` or `write`. Default is `read`.
- `{n_objects,Number}`, where `n_objects` specifies (roughly) the number of
  objects returned from Mnesia to QLC. Queries to remote tables can need a
  larger chunk to reduce network overhead. By default, `100` objects at a time
  are returned.
- `{traverse, SelectMethod}`, where `traverse` determines the method to traverse
  the whole table (if needed). The default method is `select`.

There are two alternatives for `select`:

- `select`. The table is traversed by calling `mnesia:select/4` and
  `mnesia:select/1`. The match specification (the second argument of
  [`select/3`](`select/3`)) is assembled by QLC: simple filters are translated
  into equivalent match specifications. More complicated filters need to be
  applied to all objects returned by [`select/3`](`select/3`) given a match
  specification that matches all objects.
- `{select, MatchSpec}`. As for `select`, the table is traversed by calling
  `mnesia:select/3` and `mnesia:select/1`. The difference is that the match
  specification is explicitly given. This is how to state match specifications
  that cannot easily be expressed within the syntax provided by QLC.
""".
-spec table(Tab::table(), Options) -> qlc:query_handle() when
      Options   :: Option | [Option],
      Option    :: MnesiaOpt | QlcOption,
      MnesiaOpt :: {'traverse', SelectOp} | {'lock', lock_kind()} | {'n_objects', non_neg_integer()},
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

-doc false.
get_activity_id() ->
    get(mnesia_activity_state).

-doc false.
put_activity_id(Activity) ->
    mnesia_tm:put_activity_id(Activity).
put_activity_id(Activity,Fun) ->
    mnesia_tm:put_activity_id(Activity,Fun).

regular_indexes(Tab) ->
    PosList = val({Tab, index}),
    [P || P <- PosList, is_integer(P)].
