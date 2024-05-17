<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Mnesia System Information

The following topics are included:

- Database configuration data
- Core dumps
- Dumping tables
- Checkpoints
- Startup files, log file, and data files
- Loading tables at startup
- Recovery from communication failure
- Recovery of transactions
- Backup, restore, fallback, and disaster recovery

## Database Configuration Data

The following two functions can be used to retrieve system information. For
details, see the Reference Manual.

- [`mnesia:table_info(Tab, Key) -> Info | exit({aborted, Reason})`](`mnesia:table_info/2`)
  returns information about one table, for example, the current size of the
  table and on which nodes it resides.
- [`mnesia:system_info(Key) -> Info | exit({aborted, Reason})`](`mnesia:system_info/1`)
  returns information about the `Mnesia` system, for example, transaction
  statistics, `db_nodes`, and configuration parameters.

## Core Dumps

If `Mnesia` malfunctions, system information is dumped to file
`MnesiaCore.Node.When`. The type of system information contained in this file
can also be generated with the function `mnesia_lib:coredump()`. If a `Mnesia`
system behaves strangely, it is recommended that a `Mnesia` core dump file is
included in the bug report.

## Dumping Tables

Tables of type `ram_copies` are by definition stored in memory only. However,
these tables can be dumped to disc, either at regular intervals or before the
system is shut down. The function
[`mnesia:dump_tables(TabList)`](`mnesia:dump_tables/1`) dumps all replicas of a
set of RAM tables to disc. The tables can be accessed while being dumped to
disc. To dump the tables to disc, all replicas must have the storage type
`ram_copies`.

The table content is placed in a `.DCD` file on the disc. When the `Mnesia`
system is started, the RAM table is initially loaded with data from its `.DCD`
file.

## Checkpoints

A checkpoint is a transaction consistent state that spans over one or more
tables. When a checkpoint is activated, the system remembers the current content
of the set of tables. The checkpoint retains a transaction consistent state of
the tables, allowing the tables to be read and updated while the checkpoint is
active. A checkpoint is typically used to back up tables to external media, but
they are also used internally in `Mnesia` for other purposes. Each checkpoint is
independent and a table can be involved in several checkpoints simultaneously.

Each table retains its old contents in a checkpoint retainer. For performance
critical applications, it can be important to realize the processing overhead
associated with checkpoints. In a worst case scenario, the checkpoint retainer
consumes more memory than the table itself. Also, each update becomes slightly
slower on those nodes where checkpoint retainers are attached to the tables.

For each table, it is possible to choose if there is to be one checkpoint
retainer attached to all replicas of the table, or if it is enough to have only
one checkpoint retainer attached to a single replica. With a single checkpoint
retainer per table, the checkpoint consumes less memory, but it is vulnerable to
node crashes. With several redundant checkpoint retainers, the checkpoint
survives as long as there is at least one active checkpoint retainer attached to
each table.

Checkpoints can be explicitly deactivated with the function
[`mnesia:deactivate_checkpoint(Name)`](`mnesia:deactivate_checkpoint/1`), where
`Name` is the name of an active checkpoint. This function returns `ok` if
successful or `{error, Reason}` if there is an error. All tables in a checkpoint
must be attached to at least one checkpoint retainer. The checkpoint is
automatically deactivated by `Mnesia`, when any table lacks a checkpoint
retainer. This can occur when a node goes down or when a replica is deleted. Use
arguments `min` and `max` (described in the following list) to control the
degree of checkpoint retainer redundancy.

[](){: #mnesia%3Achkpt%28Args%29 }

Checkpoints are activated with the function
[`mnesia:activate_checkpoint(Args)`](`mnesia:activate_checkpoint/1`), where `Args`
is a list of the following tuples:

- `{name, Name}`, where `Name` specifies a temporary name of the checkpoint. The
  name can be reused when the checkpoint has been deactivated. If no name is
  specified, a name is generated automatically.
- `{max, MaxTabs}`, where `MaxTabs` is a list of tables that are to be included
  in the checkpoint. Default is `[]` (empty list). For these tables, the
  redundancy is maximized. The old content of the table is retained in the
  checkpoint retainer when the main table is updated by the applications. The
  checkpoint is more fault tolerant if the tables have several replicas. When
  new replicas are added by the schema manipulation function
  `mnesia:add_table_copy/3` it also attaches a local checkpoint retainer.
- `{min, MinTabs}`, where `MinTabs` is a list of tables that are to be included
  in the checkpoint. Default is `[]`. For these tables, the redundancy is
  minimized, and there is to be single checkpoint retainer per table, preferably
  at the local node.
- `{allow_remote, Bool}`, where `false` means that all checkpoint retainers must
  be local. If a table does not reside locally, the checkpoint cannot be
  activated. `true` allows checkpoint retainers to be allocated on any node.
  Default is `true`.
- `{ram_overrides_dump, Bool}`. This argument only applies to tables of type
  `ram_copies`. `Bool` specifies if the table state in RAM is to override the
  table state on disc. `true` means that the latest committed records in RAM are
  included in the checkpoint retainer. These are the records that the
  application accesses. `false` means that the records on the disc `.DAT` file
  are included in the checkpoint retainer. These records are loaded on startup.
  Default is `false`.

The function [`mnesia:activate_checkpoint(Args)`](`mnesia:activate_checkpoint/1`)
returns one of the following values:

- `{ok, Name, Nodes}`
- `{error, Reason}`

`Name` is the checkpoint name. `Nodes` are the nodes where the checkpoint is
known.

A list of active checkpoints can be obtained with the following functions:

- [`mnesia:system_info(checkpoints)`](`mnesia:system_info/1`) returns all active
  checkpoints on the current node.
- [`mnesia:table_info(Tab, checkpoints)`](`mnesia:table_info/2`) returns active
  checkpoints on a specific table.

## Startup Files, Log File, and Data Files

This section describes the internal files that are created and maintained by the
`Mnesia` system. In particular, the workings of the `Mnesia` log are described.

### Startup Files

[Start Mnesia](mnesia_chap3.md#start_mnesia) states the following prerequisites
for starting `Mnesia`:

- An Erlang session must be started and a `Mnesia` directory must be specified
  for the database.
- A database schema must be initiated, using the function
  `mnesia:create_schema/1`.

The following example shows how these tasks are performed:

_Step 1:_ Start an Erlang session and specify a `Mnesia` directory for the
database:

```text
% erl -sname klacke -mnesia dir '"/ldisc/scratch/klacke"'
```

```erlang
Erlang (BEAM) emulator version 4.9

Eshell V4.9  (abort with ^G)
(klacke@gin)1> mnesia:create_schema([node()]).
ok
(klacke@gin)2>
^Z
Suspended
```

_Step 2:_ You can inspect the `Mnesia` directory to see what files have been
created:

```text
% ls -l /ldisc/scratch/klacke
-rw-rw-r--   1 klacke   staff       247 Aug 12 15:06 FALLBACK.BUP
```

The response shows that the file `FALLBACK.BUP` has been created. This is called
a backup file, and it contains an initial schema. If more than one node in the
function `mnesia:create_schema/1` had been specified, identical backup files
would have been created on all nodes.

_Step 3:_ Start `Mnesia`:

```erlang
(klacke@gin)3>mnesia:start( ).
ok
```

_Step 4:_ You can see the following listing in the `Mnesia` directory:

```text
-rw-rw-r--   1 klacke   staff         86 May 26 19:03 LATEST.LOG
-rw-rw-r--   1 klacke   staff      34507 May 26 19:03 schema.DAT
```

The schema in the backup file `FALLBACK.BUP` has been used to generate the file
`schema.DAT`. Since there are no other disc resident tables than the schema, no
other data files were created. The file `FALLBACK.BUP` was removed after the
successful "restoration". You also see some files that are for internal use by
`Mnesia`.

_Step 5:_ Create a table:

```erlang
(klacke@gin)4> mnesia:create_table(foo,[{disc_copies, [node()]}]).
{atomic,ok}
```

_Step 6:_ You can see the following listing in the `Mnesia` directory:

```text
% ls -l /ldisc/scratch/klacke
-rw-rw-r-- 1 klacke staff    86 May 26 19:07 LATEST.LOG
-rw-rw-r-- 1 klacke staff    94 May 26 19:07 foo.DCD
-rw-rw-r-- 1 klacke staff  6679 May 26 19:07 schema.DAT
```

The file `foo.DCD` has been created. This file will eventually store all data
that is written into the `foo` table.

### Log File

When starting `Mnesia`, a `.LOG` file called `LATEST.LOG` is created and placed
in the database directory. This file is used by `Mnesia` to log disc-based
transactions. This includes all transactions that write at least one record in a
table that is of storage type `disc_copies` or `disc_only_copies`. The file also
includes all operations that manipulate the schema itself, such as creating new
tables. The log format can vary with different implementations of `Mnesia`. The
`Mnesia` log is currently implemented in the standard library module
`m:disk_log` in `Kernel`.

The log file grows continuously and must be dumped at regular intervals.
"Dumping the log file" means that `Mnesia` performs all the operations listed in
the log and place the records in the corresponding `.DAT`, `.DCD`, and `.DCL`
data files. For example, if the operation "write record `{foo, 4, elvis, 6}`" is
listed in the log, `Mnesia` inserts the operation into the file `foo.DCL`.
Later, when `Mnesia` thinks that the `.DCL` file is too large, the data is moved
to the `.DCD` file. The dumping operation can be time consuming if the log is
large. Notice that the `Mnesia` system continues to operate during log dumps.

By default `Mnesia` either dumps the log whenever 1000 records have been written
in the log or when three minutes have passed. This is controlled by the two
application parameters `-mnesia dump_log_write_threshold WriteOperations` and
`-mnesia dump_log_time_threshold MilliSecs`.

Before the log is dumped, the file `LATEST.LOG` is renamed to `PREVIOUS.LOG`,
and a new `LATEST.LOG` file is created. Once the log has been successfully
dumped, the file `PREVIOUS.LOG` is deleted.

The log is also dumped at startup and whenever a schema operation is performed.

### Data Files

The directory listing also contains one `.DAT` file, which contains the schema
itself, contained in the `schema.DAT` file. The `DAT` files are indexed files,
and it is efficient to insert and search for records in these files with a
specific key. The `.DAT` files are used for the schema and for
`disc_only_copies` tables. The `Mnesia` data files are currently implemented in
the standard library module `m:dets` in `STDLIB`.

All operations that can be performed on `dets` files can also be performed on
the `Mnesia` data files. For example, `dets` contains the function
`dets:traverse/2`, which can be used to view the contents of a `Mnesia` `DAT`
file. However, this can only be done when `Mnesia` is not running. So, to view
the schema file, do as follows;

```erlang
{ok, N} = dets:open_file(schema, [{file, "./schema.DAT"},{repair,false},
{keypos, 2}]),
F = fun(X) -> io:format("~p~n", [X]), continue end,
dets:traverse(N, F),
dets:close(N).
```

> #### Warning {: .warning }
>
> The `DAT` files must always be opened with option `{repair, false}`. This
> ensures that these files are not automatically repaired. Without this option,
> the database can become inconsistent, because `Mnesia` can believe that the
> files were properly closed. For information about configuration parameter
> `auto_repair`, see the Reference Manual.

> #### Warning {: .warning }
>
> It is recommended that the data files are not tampered with while `Mnesia` is
> running. While not prohibited, the behavior of `Mnesia` is unpredictable.

The `disc_copies` tables are stored on disk with `.DCL` and `.DCD` files, which
are standard `disk_log` files.

## Loading Tables at Startup

At startup, `Mnesia` loads tables to make them accessible for its applications.
Sometimes `Mnesia` decides to load all tables that reside locally, and sometimes
the tables are not accessible until `Mnesia` brings a copy of the table from
another node.

To understand the behavior of `Mnesia` at startup, it is essential to understand
how `Mnesia` reacts when it loses contact with `Mnesia` on another node. At this
stage, `Mnesia` cannot distinguish between a communication failure and a
"normal" node-down. When this occurs, `Mnesia` assumes that the other node is no
longer running, whereas, in reality, the communication between the nodes has
failed.

To overcome this situation, try to restart the ongoing transactions that are
accessing tables on the failing node, and write a `mnesia_down` entry to a log
file.

At startup, notice that all tables residing on nodes without a `mnesia_down`
entry can have fresher replicas. Their replicas can have been updated after the
termination of `Mnesia` on the current node. To catch up with the latest
updates, transfer a copy of the table from one of these other "fresh" nodes. If
you are unlucky, other nodes can be down and you must wait for the table to be
loaded on one of these nodes before receiving a fresh copy of the table.

Before an application makes its first access to a table,
[`mnesia:wait_for_tables(TabList, Timeout)`](`mnesia:wait_for_tables/2`) is to be
executed to ensure that the table is accessible from the local node. If the
function times out, the application can choose to force a load of the local
replica with [`mnesia:force_load_table(Tab)`](`mnesia:force_load_table/1`) and
deliberately lose all updates that can have been performed on the other nodes
while the local node was down. If `Mnesia` has loaded the table on another node
already, or intends to do so, copy the table from that node to avoid unnecessary
inconsistency.

> #### Warning {: .warning }
>
> Only one table is loaded by
> [`mnesia:force_load_table(Tab)`](`mnesia:force_load_table/1`). Since committed
> transactions can have caused updates in several tables, the tables can become
> inconsistent because of the forced load.

The allowed `AccessMode` of a table can be defined to be `read_only` or
`read_write`. It can be toggled with the function
[`mnesia:change_table_access_mode(Tab, AccessMode)`](`mnesia:change_table_access_mode/2`)
in runtime. `read_only` tables and `local_content` tables are always loaded
locally, as there is no need for copying the table from other nodes. Other
tables are primarily loaded remotely from active replicas on other nodes if the
table has been loaded there already, or if the running `Mnesia` has decided to
load the table there already.

At startup, `Mnesia` assumes that its local replica is the most recent version
and loads the table from disc if either of the following situations is detected:

- `mnesia_down` is returned from all other nodes that hold a disc resident
  replica of the table.
- All replicas are `ram_copies`.

This is normally a wise decision, but it can be disastrous if the nodes have
been disconnected because of a communication failure, as the `Mnesia` normal
table load mechanism does not cope with communication failures.

When `Mnesia` loads many tables, the default load order is used. However, the
load order can be affected, by explicitly changing property `load_order` for the
tables, with the function
[`mnesia:change_table_load_order(Tab, LoadOrder)`](`mnesia:change_table_load_order/2`).
`LoadOrder` is by default `0` for all tables, but it can be set to any integer.
The table with the highest `load_order` is loaded first. Changing the load order
is especially useful for applications that need to ensure early availability of
fundamental tables. Large peripheral tables are to have a low load order value,
perhaps less than `0`

## Recovery from Communication Failure

There are several occasions when `Mnesia` can detect that the network has been
partitioned because of a communication failure, for example:

- `Mnesia` is operational already and the Erlang nodes gain contact again. Then
  `Mnesia` tries to contact `Mnesia` on the other node to see if it also thinks
  that the network has been partitioned for a while. If `Mnesia` on both nodes
  has logged `mnesia_down` entries from each other, `Mnesia` generates a system
  event, called `{inconsistent_database, running_partitioned_network, Node}`,
  which is sent to the `Mnesia` event handler and other possible subscribers.
  The default event handler reports an error to the error logger.
- If `Mnesia` detects at startup that both the local node and another node
  received `mnesia_down` from each other, `Mnesia` generates an
  `{inconsistent_database, starting_partitioned_network, Node}` system event and
  acts as described in the previous item.

If the application detects that there has been a communication failure that can
have caused an inconsistent database, it can use the function
[`mnesia:set_master_nodes(Tab, Nodes)`](`mnesia:set_master_nodes/2`) to pinpoint
from which nodes each table can be loaded.

At startup, the `Mnesia` normal table load algorithm is bypassed and the table
is loaded from one of the master nodes defined for the table, regardless of
potential `mnesia_down` entries in the log. `Nodes` can only contain nodes where
the table has a replica. If `Nodes` is empty, the master node recovery mechanism
for the particular table is reset and the normal load mechanism is used at the
next restart.

The function [`mnesia:set_master_nodes(Nodes)`](`mnesia:set_master_nodes/1`) sets
master nodes for all tables. For each table it determines its replica nodes and
starts [`mnesia:set_master_nodes(Tab, TabNodes)`](`mnesia:set_master_nodes/2`)
with those replica nodes that are included in the `Nodes` list (that is,
`TabNodes` is the intersection of `Nodes` and the replica nodes of the table).
If the intersection is empty, the master node recovery mechanism for the
particular table is reset and the normal load mechanism is used at the next
restart.

The functions [`mnesia:system_info(master_node_tables)`](`mnesia:system_info/1`)
and [`mnesia:table_info(Tab, master_nodes)`](`mnesia:table_info/2`) can be used to
obtain information about the potential master nodes.

Determining what data to keep after a communication failure is outside the scope
of `Mnesia`. One approach is to determine which "island" contains most of the
nodes. Using option `{majority,true}` for critical tables can be a way to ensure
that nodes that are not part of a "majority island" cannot update those tables.
Notice that this constitutes a reduction in service on the minority nodes. This
would be a tradeoff in favor of higher consistency guarantees.

The function [`mnesia:force_load_table(Tab)`](`mnesia:force_load_table/1`) can be
used to force load the table regardless of which table load mechanism that is
activated.

## Recovery of Transactions

A `Mnesia` table can reside on one or more nodes. When a table is updated,
`Mnesia` ensures that the updates are replicated to all nodes where the table
resides. If a replica is inaccessible (for example, because of a temporary
node-down), `Mnesia` performs the replication later.

On the node where the application is started, there is a transaction coordinator
process. If the transaction is distributed, there is also a transaction
participant process on all the other nodes where commit-work needs to be
performed.

Internally `Mnesia` uses several commit protocols. The selected protocol depends
on which table that has been updated in the transaction. If all the involved
tables are symmetrically replicated (that is, they all have the same
`ram_nodes`, `disc_nodes`, and `disc_only_nodes` currently accessible from the
coordinator node), a lightweight transaction commit protocol is used.

The number of messages that the transaction coordinator and its participants
need to exchange is few, as the `Mnesia` table load mechanism takes care of the
transaction recovery if the commit protocol gets interrupted. Since all involved
tables are replicated symmetrically, the transaction is automatically recovered
by loading the involved tables from the same node at startup of a failing node.
It does not matter if the transaction was committed or terminated as long as the
ACID properties can be ensured. The lightweight commit protocol is non-blocking,
that is, the surviving participants and their coordinator finish the
transaction, even if any node crashes in the middle of the commit protocol.

If a node goes down in the middle of a dirty operation, the table load mechanism
ensures that the update is performed on all replicas, or none. Both asynchronous
dirty updates and synchronous dirty updates use the same recovery principle as
lightweight transactions.

If a transaction involves updates of asymmetrically replicated tables or updates
of the schema table, a heavyweight commit protocol is used. This protocol can
finish the transaction regardless of how the tables are replicated. The typical
use of a heavyweight transaction is when a replica is to be moved from one node
to another. Then ensure that the replica either is entirely moved or left as it
was. Do never end up in a situation with replicas on both nodes, or on no node
at all. Even if a node crashes in the middle of the commit protocol, the
transaction must be guaranteed to be atomic. The heavyweight commit protocol
involves more messages between the transaction coordinator and its participants
than a lightweight protocol, and it performs recovery work at startup to finish
the terminating or commit work.

The heavyweight commit protocol is also non-blocking, which allows the surviving
participants and their coordinator to finish the transaction regardless (even if
a node crashes in the middle of the commit protocol). When a node fails at
startup, `Mnesia` determines the outcome of the transaction and recovers it.
Lightweight protocols, heavyweight protocols, and dirty updates, are dependent
on other nodes to be operational to make the correct heavyweight transaction
recovery decision.

If `Mnesia` has not started on some of the nodes that are involved in the
transaction _and_ neither the local node nor any of the already running nodes
know the outcome of the transaction, `Mnesia` waits for one, by default. In the
worst case scenario, all other involved nodes must start before `Mnesia` can
make the correct decision about the transaction and finish its startup.

Thus, `Mnesia` (on one node) can hang if a double fault occurs, that is, when
two nodes crash simultaneously and one attempts to start when the other refuses
to start, for example, because of a hardware error.

The maximum time that `Mnesia` waits for other nodes to respond with a
transaction recovery decision can be specified. The configuration parameter
`max_wait_for_decision` defaults to `infinity`, which can cause the indefinite
hanging as mentioned earlier. However, if the parameter is set to a definite
time period (for example, three minutes), `Mnesia` then enforces a transaction
recovery decision, if needed, to allow `Mnesia` to continue with its startup
procedure.

The downside of an enforced transaction recovery decision is that the decision
can be incorrect, because of insufficient information about the recovery
decisions from the other nodes. This can result in an inconsistent database
where `Mnesia` has committed the transaction on some nodes but terminated it on
others.

In fortunate cases, the inconsistency is only visible in tables belonging to a
specific application. However, if a schema transaction is inconsistently
recovered because of the enforced transaction recovery decision, the effects of
the inconsistency can be fatal. However, if the higher priority is availability
rather than consistency, it can be worth the risk.

If `Mnesia` detects an inconsistent transaction decision, an
`{inconsistent_database, bad_decision, Node}` system event is generated to give
the application a chance to install a fallback or other appropriate measures to
resolve the inconsistency. The default behavior of the `Mnesia` event handler is
the same as if the database became inconsistent as a result of partitioned
network (as described earlier).

## Backup, Restore, Fallback, and Disaster Recovery

The following functions are used to back up data, to install a backup as
fallback, and for disaster recovery:

- [`mnesia:backup_checkpoint(Name, Opaque, [Mod])`](`mnesia:backup_checkpoint/2`)
  performs a backup of the tables included in the checkpoint.
- [`mnesia:backup(Opaque, [Mod])`](`mnesia:backup/1`) activates a new checkpoint
  that covers all `Mnesia` tables and performs a backup. It is performed with
  maximum degree of redundancy (see also the function
  [`mnesia:activate_checkpoint(Args)`](mnesia_chap7.md#checkpoints),
  `{max, MaxTabs} and {min, MinTabs})`.
- [`mnesia:traverse_backup(Source, [SourceMod,] Target, [TargetMod,] Fun,
  Acc)`](`mnesia:traverse_backup/4`) can be used to read an existing backup,
  create a backup from an existing one, or to copy a backup from one type media
  to another.
- [`mnesia:uninstall_fallback()`](`mnesia:uninstall_fallback/0`) removes
  previously installed fallback files.
- [`mnesia:restore(Opaque, Args)`](`mnesia:restore/2`) restores a set of tables
  from a previous backup.
- [`mnesia:install_fallback(Opaque, [Mod])`](`mnesia:install_fallback/1`) can be
  configured to restart `Mnesia` and the reload data tables, and possibly the
  schema tables, from an existing backup. This function is typically used for
  disaster recovery purposes, when data or schema tables are corrupted.

These functions are explained in the following sections. See also
[Checkpoints](mnesia_chap7.md#checkpoints), which describes the two functions
used to activate and deactivate checkpoints.

### Backup

Backup operation are performed with the following functions:

- [`mnesia:backup_checkpoint(Name, Opaque, [Mod])`](`mnesia:backup_checkpoint/2`)
- [`mnesia:backup(Opaque, [Mod])`](`mnesia:backup/1`)
- [`mnesia:traverse_backup(Source, [SourceMod,] Target, [TargetMod,] Fun,
  Acc)`](`mnesia:traverse_backup/4`)

By default, the actual access to the backup media is performed through module
`mnesia_backup` for both read and write. Currently `mnesia_backup` is
implemented with the standard library module `disk_log`. However, you can write
your own module with the same interface as `mnesia_backup` and configure
`Mnesia` so that the alternative module performs the actual accesses to the
backup media. The user can therefore put the backup on a media that `Mnesia`
does not know about, possibly on hosts where Erlang is not running. Use
configuration parameter `-mnesia backup_module <module>` for this purpose.

The source for a backup is an activated checkpoint. The backup function
[`mnesia:backup_checkpoint(Name, Opaque,[Mod])`](`mnesia:backup_checkpoint/2`) is
most commonly used and returns `ok` or `{error,Reason}`. It has the following
arguments:

- `Name` is the name of an activated checkpoint. For details on how to include
  table names in checkpoints, see the function
  `mnesia:activate_checkpoint(ArgList)` in
  [Checkpoints](mnesia_chap7.md#checkpoints).
- `Opaque`. `Mnesia` does not interpret this argument, but it is forwarded to
  the backup module. The `Mnesia` default backup module `mnesia_backup`
  interprets this argument as a local filename.
- `Mod` is the name of an alternative backup module.

The function [`mnesia:backup(Opaque [,Mod])`](`mnesia:backup/1`) activates a new
checkpoint that covers all `Mnesia` tables with maximum degree of redundancy and
performs a backup. Maximum redundancy means that each table replica has a
checkpoint retainer. Tables with property `local_contents` are backed up as they
look on the current node.

You can iterate over a backup, either to transform it into a new backup, or only
read it. The function [`mnesia:traverse_backup(Source, [SourceMod,] Target,
[TargetMod,] Fun, Acc)`](`mnesia:traverse_backup/4`), which normally returns
`{ok, LastAcc}`, is used for both of these purposes.

Before the traversal starts, the source backup media is opened with
`SourceMod:open_read(Source)`, and the target backup media is opened with
`TargetMod:open_write(Target)`. The arguments are as follows:

- `SourceMod` and `TargetMod` are module names.
- `Source` and `Target` are opaque data used exclusively by the modules
  `SourceMod` and `TargetMod` for initializing the backup media.
- `Acc` is an initial accumulator value.
- `Fun(BackupItems, Acc)` is applied to each item in the backup. The Fun must
  return a tuple `{ValGoodBackupItems, NewAcc}`, where `ValidBackupItems` is a
  list of valid backup items. `NewAcc` is a new accumulator value. The
  `ValidBackupItems` are written to the target backup with the function
  `TargetMod:write/2`.
- `LastAcc` is the last accumulator value, that is, the last `NewAcc` value that
  was returned by `Fun`.

Also, a read-only traversal of the source backup can be performed without
updating a target backup. If `TargetMod==read_only`, no target backup is
accessed.

By setting `SourceMod` and `TargetMod` to different modules, a backup can be
copied from one backup media to another.

Valid `BackupItems` are the following tuples:

- `{schema, Tab}` specifies a table to be deleted.
- `{schema, Tab, CreateList}` specifies a table to be created. For more
  information about `CreateList`, see `mnesia:create_table/2`.
- `{Tab, Key}` specifies the full identity of a record to be deleted.
- `{Record}` specifies a record to be inserted. It can be a tuple with `Tab` as
  first field. Notice that the record name is set to the table name regardless
  of what `record_name` is set to.

The backup data is divided into two sections. The first section contains
information related to the schema. All schema-related items are tuples where the
first field equals the atom schema. The second section is the record section.
Schema records cannot be mixed with other records and all schema records must be
located first in the backup.

The schema itself is a table and is possibly included in the backup. Each node
where the schema table resides is regarded as a `db_node`.

The following example shows how
[`mnesia:traverse_backup`](`mnesia:traverse_backup/4`) can be used to rename a
`db_node` in a backup file:

```erlang
change_node_name(Mod, From, To, Source, Target) ->
    Switch =
        fun(Node) when Node == From -> To;
           (Node) when Node == To -> throw({error, already_exists});
           (Node) -> Node
        end,
    Convert =
        fun({schema, db_nodes, Nodes}, Acc) ->
                {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
           ({schema, version, Version}, Acc) ->
                {[{schema, version, Version}], Acc};
           ({schema, cookie, Cookie}, Acc) ->
                {[{schema, cookie, Cookie}], Acc};
           ({schema, Tab, CreateList}, Acc) ->
                Keys = [ram_copies, disc_copies, disc_only_copies],
                OptSwitch =
                    fun({Key, Val}) ->
                            case lists:member(Key, Keys) of
                                true -> {Key, lists:map(Switch, Val)};
                                false-> {Key, Val}
                            end
                    end,
                {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc};
           (Other, Acc) ->
                {[Other], Acc}
        end,
    mnesia:traverse_backup(Source, Mod, Target, Mod, Convert, switched).

view(Source, Mod) ->
    View = fun(Item, Acc) ->
                   io:format("~p.~n",[Item]),
                   {[Item], Acc + 1}
           end,
    mnesia:traverse_backup(Source, Mod, dummy, read_only, View, 0).
```

### Restore

Tables can be restored online from a backup without restarting `Mnesia`. A
restore is performed with the function
[`mnesia:restore(Opaque, Args)`](`mnesia:restore/2`), where `Args` can contain the
following tuples:

- `{module,Mod}`. The backup module `Mod` is used to access the backup media. If
  omitted, the default backup module is used.
- `{skip_tables, TableList}`, where `TableList` is a list of tables, which is
  not to be read from the backup.
- `{clear_tables, TableList}`, where `TableList` is a list of tables, which is
  to be cleared before the records from the backup are inserted. That is, all
  records in the tables are deleted before the tables are restored. Schema
  information about the tables is not cleared or read from the backup.
- `{keep_tables, TableList}`, where `TableList` is a list of tables, which is
  not to be cleared before the records from the backup are inserted. That is,
  the records in the backup are added to the records in the table. Schema
  information about the tables is not cleared or read from the backup.
- `{recreate_tables, TableList}`, where `TableList` is a list of tables, which
  is to be recreated before the records from the backup are inserted. The tables
  are first deleted and then created with the schema information from the
  backup. All the nodes in the backup need to be operational.
- `{default_op, Operation}`, where `Operation` is one of the operations
  `skip_tables`, `clear_tables`, `keep_tables`, or `recreate_tables`. The
  default operation specifies which operation is to be used on tables from the
  backup that are not specified in any of the previous lists. If omitted, the
  operation `clear_tables` is used.

The argument `Opaque` is forwarded to the backup module. It returns
`{atomic, TabList}` if successful, or the tuple `{aborted, Reason}` if there is
an error. `TabList` is a list of the restored tables. Tables that are restored
are write-locked during the restore operation. However, regardless of any lock
conflict caused by this, applications can continue to do their work during the
restore operation.

The restoration is performed as a single transaction. If the database is large,
it cannot always be restored online. The old database must then be restored by
installing a fallback, followed by a restart.

### Fallback

The function [`mnesia:install_fallback(Opaque,
[Mod])`](`mnesia:install_fallback/2`) installs a backup as fallback. It uses the
backup module `Mod`, or the default backup module, to access the backup media.
The function returns `ok` if successful, or `{error, Reason}` if there is an
error.

Installing a fallback is a distributed operation, which is _only_ performed on
all `db_nodes`. The fallback restores the database the next time the system is
started. If a `Mnesia` node with a fallback installed detects that `Mnesia` on
another node has died, it unconditionally terminates itself.

A fallback is typically used when a system upgrade is performed. A system
typically involves the installation of new software versions, and `Mnesia`
tables are often transformed into new layouts. If the system crashes during an
upgrade, it is highly probable that reinstallation of the old applications is
required, and restoration of the database to its previous state. This can be
done if a backup is performed and installed as a fallback before the system
upgrade begins.

If the system upgrade fails, `Mnesia` must be restarted on all `db_nodes` to
restore the old database. The fallback is automatically deinstalled after a
successful startup. The function
[`mnesia:uninstall_fallback()`](`mnesia:uninstall_fallback/0`) can also be used to
deinstall the fallback after a successful system upgrade. Again, this is a
distributed operation that is either performed on all `db_nodes` or none. Both
the installation and deinstallation of fallbacks require Erlang to be
operational on all `db_nodes`, but it does not matter if `Mnesia` is running or
not.

### Disaster Recovery

The system can become inconsistent as a result of a power failure. The UNIX
feature `fsck` can possibly repair the file system, but there is no guarantee
that the file content is consistent.

If `Mnesia` detects that a file has not been properly closed, possibly as a
result of a power failure, it tries to repair the bad file in a similar manner.
Data can be lost, but `Mnesia` can be restarted even if the data is
inconsistent. Configuration parameter `-mnesia auto_repair <bool>` can be used
to control the behavior of `Mnesia` at startup. If `<bool>` has the value
`true`, `Mnesia` tries to repair the file. If `<bool>` has the value `false`,
`Mnesia` does not restart if it detects a suspect file. This configuration
parameter affects the repair behavior of log files, `DAT` files, and the default
backup media.

Configuration parameter `-mnesia dump_log_update_in_place <bool>` controls the
safety level of the function [`mnesia:dump_log()`](`mnesia:dump_log/0`) By
default, `Mnesia` dumps the transaction log directly into the `DAT` files. If a
power failure occurs during the dump, this can cause the randomly accessed `DAT`
files to become corrupt. If the parameter is set to `false`, `Mnesia` copies the
`DAT` files and target the dump to the new temporary files. If the dump is
successful, the temporary files are renamed to their normal `DAT` suffixes. The
possibility for unrecoverable inconsistencies in the data files becomes much
smaller with this strategy. However, the actual dumping of the transaction log
becomes considerably slower. The system designer must decide whether speed or
safety is the higher priority.

Replicas of type `disc_only_copies` are only affected by this parameter during
the initial dump of the log file at startup. When designing applications with
_very_ high requirements, it can be appropriate not to use `disc_only_copies`
tables at all. The reason for this is the random access nature of normal
operating system files. If a node goes down for a reason such as a power
failure, these files can be corrupted because they are not properly closed. The
`DAT` files for `disc_only_copies` are updated on a per transaction basis.

If a disaster occurs and the `Mnesia` database is corrupted, it can be
reconstructed from a backup. Regard this as a last resort, as the backup
contains old data. The data is hopefully consistent, but data is definitely lost
when an old backup is used to restore the database.
