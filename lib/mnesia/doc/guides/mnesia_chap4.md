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
# Transactions and Other Access Contexts

This section describes the `Mnesia` transaction system and the transaction
properties that make `Mnesia` a fault-tolerant, distributed Database Management
System (DBMS).

This section also describes the locking functions, including table locks and
sticky locks, as well as alternative functions that bypass the transaction
system in favor of improved speed and reduced overhead. These functions are
called "dirty operations". The use of nested transactions is also described. The
following topics are included:

- Transaction properties, which include atomicity, consistency, isolation, and
  durability
- Locking
- Dirty operations
- Record names versus table names
- Activity concept and various access contexts
- Nested transactions
- Pattern matching
- Iteration

[](){: #trans_prop }

## Transaction Properties

Transactions are important when designing fault-tolerant, distributed systems. A
`Mnesia` transaction is a mechanism by which a series of database operations can
be executed as one functional block. The functional block that is run as a
transaction is called a Functional Object (Fun), and this code can read, write,
and delete `Mnesia` records. The Fun is evaluated as a transaction that either
commits or terminates. If a transaction succeeds in executing the Fun, it
replicates the action on all nodes involved, or terminates if an error occurs.

The following example shows a transaction that raises the salary of certain
employee numbers:

```erlang
raise(Eno, Raise) ->
    F = fun() ->
                [E] = mnesia:read(employee, Eno, write),
                Salary = E#employee.salary + Raise,
                New = E#employee{salary = Salary},
                mnesia:write(New)
        end,
    mnesia:transaction(F).
```

The function `raise/2` contains a Fun made up of four code lines. This Fun is
called by the statement `mnesia:transaction(F)` and returns a value.

The `Mnesia` transaction system facilitates the construction of reliable,
distributed systems by providing the following important properties:

- The transaction handler ensures that a Fun, which is placed inside a
  transaction, does not interfere with operations embedded in other transactions
  when it executes a series of operations on tables.
- The transaction handler ensures that either all operations in the transaction
  are performed successfully on all nodes atomically, or the transaction fails
  without permanent effect on any node.
- The `Mnesia` transactions have four important properties, called *A*tomicity,
  *C*onsistency, *I*solation, and *D*urability (ACID). These properties are
  described in the following sections.

### Atomicity

Atomicity means that database changes that are executed by a transaction take
effect on all nodes involved, or on none of the nodes. That is, the transaction
either succeeds entirely, or it fails entirely.

Atomicity is important when it is needed to write atomically more than one
record in the same transaction. The function `raise/2`, shown in the previous
example, writes one record only. The function `insert_emp/3`, shown in the
program listing in [Getting Started](mnesia_chap2.md#getting_started), writes
the record `employee` as well as employee relations, such as `at_dep` and
`in_proj`, into the database. If this latter code is run inside a transaction,
the transaction handler ensures that the transaction either succeeds completely,
or not at all.

`Mnesia` is a distributed DBMS where data can be replicated on several nodes. In
many applications, it is important that a series of write operations are
performed atomically inside a transaction. The atomicity property ensures that a
transaction takes effect on all nodes, or none.

### Consistency

The consistency property ensures that a transaction always leaves the DBMS in a
consistent state. For example, `Mnesia` ensures that no inconsistencies occur if
Erlang, `Mnesia`, or the computer crashes while a write operation is in
progress.

### Isolation

The isolation property ensures that transactions that execute on different nodes
in a network, and access and manipulate the same data records, do not interfere
with each other. The isolation property makes it possible to execute the
function `raise/2` concurrently. A classical problem in concurrency control
theory is the "lost update problem".

The isolation property is in particular useful if the following circumstances
occur where an employee (with employee number 123) and two processes (P1 and P2)
are concurrently trying to raise the salary for the employee:

- _Step 1:_ The initial value of the employees salary is, for example, 5.
  Process P1 starts to execute, reads the employee record, and adds 2 to the
  salary.
- _Step 2:_ Process P1 is for some reason pre-empted and process P2 has the
  opportunity to run.
- _Step 3:_ Process P2 reads the record, adds 3 to the salary, and finally
  writes a new employee record with the salary set to 8.
- _Step 4:_ Process P1 starts to run again and writes its employee record with
  salary set to 7, thus effectively overwriting and undoing the work performed
  by process P2. The update performed by P2 is lost.

A transaction system makes it possible to execute two or more processes
concurrently that manipulate the same record. The programmer does not need to
check that the updates are synchronous; this is overseen by the transaction
handler. All programs accessing the database through the transaction system can
be written as if they had sole access to the data.

### Durability

The durability property ensures that changes made to the DBMS by a transaction
are permanent. Once a transaction is committed, all changes made to the database
are durable, that is, they are written safely to disc and do not become
corrupted and do not disappear.

> #### Note {: .info }
>
> The described durability feature does not entirely apply to situations where
> `Mnesia` is configured as a "pure" primary memory database.

## Locking

Different transaction managers employ different strategies to satisfy the
isolation property. `Mnesia` uses the standard technique of two phase locking.
That is, locks are set on records before they are read or written. `Mnesia` uses
the following lock types:

- _Read locks_. A read lock is set on one replica of a record before it can be
  read.
- _Write locks_. Whenever a transaction writes to a record, write locks are
  first set on all replicas of that particular record.
- _Read table locks_. If a transaction traverses an entire table in search for a
  record that satisfies some particular property, it is most inefficient to set
  read locks on the records one by one. It is also memory consuming, as the read
  locks themselves can take up considerable space if the table is large.
  Therefore, `Mnesia` can set a read lock on an entire table.
- _Write table locks_. If a transaction writes many records to one table, a
  write lock can be set on the entire table.
- _Sticky locks_. These are write locks that stay in place at a node after the
  transaction that initiated the lock has terminated.

`Mnesia` employs a strategy whereby functions, such as `mnesia:read/1` acquire
the necessary locks dynamically as the transactions execute. `Mnesia`
automatically sets and releases the locks and the programmer does not need to
code these operations.

Deadlocks can occur when concurrent processes set and release locks on the same
records. `Mnesia` employs a "wait-die" strategy to resolve these situations. If
`Mnesia` suspects that a deadlock can occur when a transaction tries to set a
lock, the transaction is forced to release all its locks and sleep for a while.
The Fun in the transaction is evaluated once more.

It is therefore important that the code inside the Fun given to
`mnesia:transaction/1` is pure. Some strange results can occur if, for example,
messages are sent by the transaction Fun. The following example illustrates this
situation:

```erlang
bad_raise(Eno, Raise) ->
    F = fun() ->
                [E] = mnesia:read({employee, Eno}),
                Salary = E#employee.salary + Raise,
                New = E#employee{salary = Salary},
                io:format("Trying to write ... ~n", []),
                mnesia:write(New)
        end,
    mnesia:transaction(F).
```

This transaction can write the text `"Trying to write ... "` 1000 times to the
terminal. However, `Mnesia` guarantees that each transaction will eventually
run. As a result, `Mnesia` is not only deadlock free, but also livelock free.

The `Mnesia` programmer cannot prioritize one particular transaction to execute
before other transactions that are waiting to execute. As a result, the `Mnesia`
DBMS transaction system is not suitable for hard real-time applications.
However, `Mnesia` contains other features that have real-time properties.

`Mnesia` dynamically sets and releases locks as transactions execute. It is
therefore dangerous to execute code with transaction side-effects. In
particular, a `receive` statement inside a transaction can lead to a situation
where the transaction hangs and never returns, which in turn can cause locks not
to release. This situation can bring the whole system to a standstill, as other
transactions that execute in other processes, or on other nodes, are forced to
wait for the defective transaction.

If a transaction terminates abnormally, `Mnesia` automatically releases the
locks held by the transaction.

Up to now, examples of a number of functions that can be used inside a
transaction have been shown. The following list shows the _simplest_ `Mnesia`
functions that work with transactions. Notice that these functions must be
embedded in a transaction. If no enclosing transaction (or other enclosing
`Mnesia` activity) exists, they all fail.

- [`mnesia:transaction(Fun) -> {aborted, Reason} | {atomic, Value}`](`mnesia:transaction/1`)
  executes one transaction with the functional object `Fun` as the single
  parameter.
- [`mnesia:read({Tab, Key}) -> transaction abort | RecordList`](`mnesia:read/1`)
  reads all records with `Key` as key from table `Tab`. This function has the
  same semantics regardless of the location of `Table`. If the table is of type
  `bag`, `read({Tab, Key})` can return an arbitrarily long list. If the table is
  of type `set`, the list is either of length one or `[]`.
- [`mnesia:wread({Tab, Key}) -> transaction abort | RecordList`](`mnesia:wread/1`)
  behaves the same way as the previously listed function `read/1`, except that
  it acquires a write lock instead of a read lock. To execute a transaction that
  reads a record, modifies the record, and then writes the record, it is
  slightly more efficient to set the write lock immediately. When a
  `mnesia:read/1` is issued, followed by a `mnesia:write/1` the first read lock
  must be upgraded to a write lock when the write operation is executed.
- [`mnesia:write(Record) -> transaction abort | ok`](`mnesia:write/1`) writes a
  record into the database. Argument `Record` is an instance of a record. The
  function returns `ok`, or terminates the transaction if an error occurs.
- [`mnesia:delete({Tab, Key}) -> transaction abort | ok`](`mnesia:delete/1`)
  deletes all records with the given key.
- [`mnesia:delete_object(Record) -> transaction abort | ok`](`mnesia:delete_object/1`)
  deletes records with the OID `Record`. Use this function to delete only some
  records in a table of type `bag`.

### Sticky Locks

As previously stated, the locking strategy used by `Mnesia` is to lock one
record when reading a record, and lock all replicas of a record when writing a
record. However, some applications use `Mnesia` mainly for its fault-tolerant
qualities. These applications can be configured with one node doing all the
heavy work, and a standby node that is ready to take over if the main node
fails. Such applications can benefit from using sticky locks instead of the
normal locking scheme.

A sticky lock is a lock that stays in place at a node, after the transaction
that first acquired the lock has terminated. To illustrate this, assume that the
following transaction is executed:

```erlang
F = fun() ->
      mnesia:write(#foo{a = kalle})
    end,
mnesia:transaction(F).
```

The `foo` table is replicated on the two nodes `N1` and `N2`.

Normal locking requires the following:

- One network RPC (two messages) to acquire the write lock
- Three network messages to execute the two-phase commit protocol

If sticky locks are used, the code must first be changed as follows:

```erlang
F = fun() ->
      mnesia:s_write(#foo{a = kalle})
    end,
mnesia:transaction(F).
```

This code uses the function [`s_write/1`](`mnesia:s_write/1`) instead of the
function [`write/1`](`mnesia:write/1`) The function `s_write/1` sets a sticky lock
instead of a normal lock. If the table is not replicated, sticky locks have no
special effect. If the table is replicated, and a sticky lock is set on node
`N1`, this lock then sticks to node `N1`. The next time you try to set a sticky
lock on the same record at node `N1`, `Mnesia` detects that the lock is already
set and do no network operation to acquire the lock.

It is more efficient to set a local lock than it is to set a networked lock.
Sticky locks can therefore benefit an application that uses a replicated table
and perform most of the work on only one of the nodes.

If a record is stuck at node `N1` and you try to set a sticky lock for the
record on node `N2`, the record must be unstuck. This operation is expensive and
reduces performance. The unsticking is done automatically if you issue
`s_write/1` requests at `N2`.

### Table Locks

`Mnesia` supports read and write locks on whole tables as a complement to the
normal locks on single records. As previously stated, `Mnesia` sets and releases
locks automatically, and the programmer does not need to code these operations.
However, transactions that read and write many records in a specific table
execute more efficiently if the transaction is started by setting a table lock
on this table. This blocks other concurrent transactions from the table. The
following two functions are used to set explicit table locks for read and write
operations:

- [`mnesia:read_lock_table(Tab)`](`mnesia:read_lock_table/1`) sets a read lock on
  table `Tab`.
- [`mnesia:write_lock_table(Tab)`](`mnesia:write_lock_table/1`) sets a write lock
  on table `Tab`.

Alternative syntax for acquisition of table locks is as follows:

```erlang
mnesia:lock({table, Tab}, read)
mnesia:lock({table, Tab}, write)
```

The matching operations in `Mnesia` can either lock the entire table or only a
single record (when the key is bound in the pattern).

### Global Locks

Write locks are normally acquired on all nodes where a replica of the table
resides (and is active). Read locks are acquired on one node (the local one if a
local replica exists).

The function `mnesia:lock/2` is intended to support table locks (as mentioned
previously) but also for situations when locks need to be acquired regardless of
how tables have been replicated:

```text
mnesia:lock({global, GlobalKey, Nodes}, LockKind)

LockKind ::= read | write | ...
```

The lock is acquired on `LockItem` on all nodes in the node list.

## Dirty Operations

In many applications, the overhead of processing a transaction can result in a
loss of performance. Dirty operation are short cuts that bypass much of the
processing and increase the speed of the transaction.

Dirty operation are often useful, for example, in a datagram routing application
where `Mnesia` stores the routing table, and it is time consuming to start a
whole transaction every time a packet is received. `Mnesia` has therefore
functions that manipulate tables without using transactions. This alternative to
processing is known as a dirty operation. However, notice the trade-off in
avoiding the overhead of transaction processing:

- The atomicity and the isolation properties of `Mnesia` are lost.
- The isolation property is compromised, because other Erlang processes, which
  use transaction to manipulate the data, do not get the benefit of isolation if
  dirty operations simultaneously are used to read and write records from the
  same table.

The major advantage of dirty operations is that they execute much faster than
equivalent operations that are processed as functional objects within a
transaction.

Dirty operations are written to disc if they are performed on a table of type
`disc_copies` or type `disc_only_copies`. `Mnesia` also ensures that all
replicas of a table are updated if a dirty write operation is performed on a
table.

A dirty operation ensures a certain level of consistency. For example, dirty
operations cannot return garbled records. Hence, each individual read or write
operation is performed in an atomic manner.

All dirty functions execute a call to [`exit({aborted, Reason})`](`exit/1`) on
failure. Even if the following functions are executed inside a transaction no
locks are acquired. The following functions are available:

- [`mnesia:dirty_read({Tab, Key})`](`mnesia:dirty_read/1`) reads one or more
  records from `Mnesia`.
- [`mnesia:dirty_write(Record)`](`mnesia:dirty_write/1`) writes the record
  `Record`.
- [`mnesia:dirty_delete({Tab, Key})`](`mnesia:dirty_delete/1`) deletes one or
  more records with key `Key`.
- [`mnesia:dirty_delete_object(Record)`](`mnesia:dirty_delete_object/1`) is the
  dirty operation alternative to the function
  [`delete_object/1`](`mnesia:delete_object/1`).
- [`mnesia:dirty_first(Tab)`](`mnesia:dirty_first/1`) returns the "first" key in
  table `Tab`.

  Records in `set` or `bag` tables are not sorted. However, there is a record
  order that is unknown to the user. This means that a table can be traversed by
  this function with the function `mnesia:dirty_next/2`.

  If there are no records in the table, this function returns the atom
  `'$end_of_table'`. It is not recommended to use this atom as the key for any
  user records.

- [`mnesia:dirty_next(Tab, Key)`](`mnesia:dirty_next/2`) returns the "next" key in
  table `Tab`. This function makes it possible to traverse a table and perform
  some operation on all records in the table. When the end of the table is
  reached, the special key `'$end_of_table'` is returned. Otherwise, the
  function returns a key that can be used to read the actual record.

  The behavior is undefined if any process performs a write operation on the
  table while traversing the table with the function
  [`dirty_next/2`](`mnesia:dirty_next/2`) This is because `write` operations on a
  `Mnesia` table can lead to internal reorganizations of the table itself. This
  is an implementation detail, but remember that the dirty functions are
  low-level functions.

- [`mnesia:dirty_last(Tab)`](`mnesia:dirty_last/1`) works exactly like
  `mnesia:dirty_first/1` but returns the last object in Erlang term order for
  the table type `ordered_set`. For all other table types,
  `mnesia:dirty_first/1` and `mnesia:dirty_last/1` are synonyms.
- [`mnesia:dirty_prev(Tab, Key)`](`mnesia:dirty_prev/2`) works exactly like
  `mnesia:dirty_next/2` but returns the previous object in Erlang term order for
  the table type `ordered_set`. For all other table types, `mnesia:dirty_next/2`
  and `mnesia:dirty_prev/2` are synonyms.
- The behavior of this function is undefined if the table is written on while
  being traversed. The function
  [`mnesia:read_lock_table(Tab)`](`mnesia:read_lock_table/1`) can be used to
  ensure that no transaction-protected writes are performed during the
  iteration.
- [`mnesia:dirty_update_counter({Tab, Key}, Val)`](`mnesia:dirty_update_counter/2`).
  Counters are positive integers with a value greater than or equal to zero.
  Updating a counter adds `Val` and the counter where `Val` is a positive or
  negative integer.

  `Mnesia` has no special counter records. However, records of the form
  `{TabName, Key, Integer}` can be used as counters, and can be persistent.

  Transaction-protected updates of counter records are not possible.

  There are two significant differences when using this function instead of
  reading the record, performing the arithmetic, and writing the record:

  1. It is much more efficient.
  1. The function [`dirty_update_counter/2`](`mnesia:dirty_update_counter/2`) is
     performed as an atomic operation although it is not protected by a
     transaction. Therefore no table update is lost if two processes
     simultaneously execute the function `dirty_update_counter/2`.

- [`mnesia:dirty_match_object(Pat)`](`mnesia:dirty_match_object/2`) is the dirty
  equivalent of `mnesia:match_object/1`.
- [`mnesia:dirty_select(Tab, Pat)`](`mnesia:dirty_select/2`) is the dirty
  equivalent of `mnesia:select/2`.
- [`mnesia:dirty_index_match_object(Pat, Pos)`](`mnesia:dirty_index_match_object/2`)
  is the dirty equivalent of `mnesia:index_match_object/2`.
- [`mnesia:dirty_index_read(Tab, SecondaryKey, Pos)`](`mnesia:dirty_index_read/3`)
  is the dirty equivalent of `mnesia:index_read/3`.
- [`mnesia:dirty_all_keys(Tab)`](`mnesia:dirty_all_keys/1`) is the dirty
  equivalent of `mnesia:all_keys/1`.

[](){: #recordnames_tablenames }

## Record Names versus Table Names

In `Mnesia`, all records in a table must have the same name. All the records
must be instances of the same record type. The record name, however, does not
necessarily have to be the same as the table name, although this is the case in
most of the examples in this User's Guide. If a table is created without
property `record_name`, the following code ensures that all records in the
tables have the same name as the table:

```erlang
mnesia:create_table(subscriber, [])
```

However, if the table is created with an explicit record name as argument, as
shown in the following example, subscriber records can be stored in both of the
tables regardless of the table names:

```erlang
TabDef = [{record_name, subscriber}],
mnesia:create_table(my_subscriber, TabDef),
mnesia:create_table(your_subscriber, TabDef).
```

To access such tables, simplified access functions (as described earlier) cannot
be used. For example, writing a subscriber record into a table requires the
function `mnesia:write/3` instead of the simplified functions `mnesia:write/1`
and `mnesia:s_write/1`:

```erlang
mnesia:write(subscriber, #subscriber{}, write)
mnesia:write(my_subscriber, #subscriber{}, sticky_write)
mnesia:write(your_subscriber, #subscriber{}, write)
```

The following simple code illustrates the relationship between the simplified
access functions used in most of the examples and their more flexible
counterparts:

```erlang
mnesia:dirty_write(Record) ->
  Tab = element(1, Record),
  mnesia:dirty_write(Tab, Record).

mnesia:dirty_delete({Tab, Key}) ->
  mnesia:dirty_delete(Tab, Key).

mnesia:dirty_delete_object(Record) ->
  Tab = element(1, Record),
  mnesia:dirty_delete_object(Tab, Record)

mnesia:dirty_update_counter({Tab, Key}, Incr) ->
  mnesia:dirty_update_counter(Tab, Key, Incr).

mnesia:dirty_read({Tab, Key}) ->
  Tab = element(1, Record),
  mnesia:dirty_read(Tab, Key).

mnesia:dirty_match_object(Pattern) ->
  Tab = element(1, Pattern),
  mnesia:dirty_match_object(Tab, Pattern).

mnesia:dirty_index_match_object(Pattern, Attr)
  Tab = element(1, Pattern),
  mnesia:dirty_index_match_object(Tab, Pattern, Attr).

mnesia:write(Record) ->
  Tab = element(1, Record),
  mnesia:write(Tab, Record, write).

mnesia:s_write(Record) ->
  Tab = element(1, Record),
  mnesia:write(Tab, Record, sticky_write).

mnesia:delete({Tab, Key}) ->
  mnesia:delete(Tab, Key, write).

mnesia:s_delete({Tab, Key}) ->
  mnesia:delete(Tab, Key, sticky_write).

mnesia:delete_object(Record) ->
  Tab = element(1, Record),
  mnesia:delete_object(Tab, Record, write).

mnesia:s_delete_object(Record) ->
  Tab = element(1, Record),
  mnesia:delete_object(Tab, Record, sticky_write).

mnesia:read({Tab, Key}) ->
  mnesia:read(Tab, Key, read).

mnesia:wread({Tab, Key}) ->
  mnesia:read(Tab, Key, write).

mnesia:match_object(Pattern) ->
  Tab = element(1, Pattern),
  mnesia:match_object(Tab, Pattern, read).

mnesia:index_match_object(Pattern, Attr) ->
  Tab = element(1, Pattern),
  mnesia:index_match_object(Tab, Pattern, Attr, read).
```

## Activity Concept and Various Access Contexts

As previously described, a Functional Object (Fun) performing table access
operations, as listed here, can be passed on as arguments to the function
[`mnesia:transaction/1,2,3`](`mnesia:transaction/1`):

- [`mnesia:write/3` (`write/1`, `s_write/1`)](`mnesia:write/3`)
- `mnesia:delete/3` (`mnesia:delete/1`, `mnesia:s_delete/1`)
- `mnesia:delete_object/3` (`mnesia:delete_object/1`,
  `mnesia:s_delete_object/1`)
- `mnesia:read/3` (`mnesia:read/1`, `mnesia:wread/1`)
- [`mnesia:match_object/2`](`mnesia:match_object/3`) (`mnesia:match_object/1`)
- [`mnesia:select/3`](`mnesia:select/2`) (`mnesia:select/2`)
- `mnesia:foldl/3` (`mnesia:foldl/4`, `mnesia:foldr/3`, `mnesia:foldr/4`)
- `mnesia:all_keys/1`
- `mnesia:index_match_object/4` (`mnesia:index_match_object/2`)
- `mnesia:index_read/3`
- `mnesia:lock/2` (`mnesia:read_lock_table/1`, `mnesia:write_lock_table/1`)
- `mnesia:table_info/2`

These functions are performed in a transaction context involving mechanisms,
such as locking, logging, replication, checkpoints, subscriptions, and commit
protocols. However, the same function can also be evaluated in other activity
contexts.

The following activity access contexts are currently supported:

- `transaction`
- `sync_transaction`
- `async_dirty`
- `sync_dirty`
- `ets`

By passing the same "fun" as argument to the function
[`mnesia:sync_transaction(Fun [, Args])`](`mnesia:sync_transaction/1`) it is
performed in synced transaction context. Synced transactions wait until all
active replicas has committed the transaction (to disc) before returning from
the `mnesia:sync_transaction` call. Using `sync_transaction` is useful in the
following cases:

- When an application executes on several nodes and wants to be sure that the
  update is performed on the remote nodes before a remote process is spawned or
  a message is sent to a remote process.
- When a combining transaction writes with "dirty reads", that is, the functions
  `dirty_match_object`, `dirty_read`, `dirty_index_read`, `dirty_select`, and so
  on.
- When an application performs frequent or voluminous updates that can overload
  `Mnesia` on other nodes.

By passing the same "fun" as argument to the function [`mnesia:async_dirty(Fun
[, Args])`](`mnesia:async_dirty/1`), it is performed in dirty context. The
function calls are mapped to the corresponding dirty functions. This still
involves logging, replication, and subscriptions but no locking, local
transaction storage, or commit protocols are involved. Checkpoint retainers are
updated but updated "dirty". Thus, they are updated asynchronously. The
functions wait for the operation to be performed on one node but not the others.
If the table resides locally, no waiting occurs.

By passing the same "fun" as an argument to the function [`mnesia:sync_dirty(Fun
[, Args])`](`mnesia:sync_dirty/1`), it is performed in almost the same context
as the function [`mnesia:async_dirty/1,2`](`mnesia:async_dirty/1`). The difference
is that the operations are performed synchronously. The caller waits for the
updates to be performed on all active replicas. Using `mnesia:sync_dirty/1,2` is
useful in the following cases:

- When an application executes on several nodes and wants to be sure that the
  update is performed on the remote nodes before a remote process is spawned or
  a message is sent to a remote process.
- When an application performs frequent or voluminous updates that can overload
  `Mnesia` on the nodes.

To check if your code is executed within a transaction, use the function
`mnesia:is_transaction/0`. It returns `true` when called inside a transaction
context, otherwise `false`.

`Mnesia` tables with storage type `RAM_copies` and `disc_copies` are implemented
internally as `ets` tables. Applications can access the these tables directly.
This is only recommended if all options have been weighed and the possible
outcomes are understood. By passing the earlier mentioned "fun" to the function
[`mnesia:ets(Fun [, Args])`](`mnesia:ets/1`), it is performed but in a raw
context. The operations are performed directly on the local `ets` tables,
assuming that the local storage type is `RAM_copies` and that the table is not
replicated on other nodes.

Subscriptions are not triggered and no checkpoints are updated, but this
operation is blindingly fast. Disc resident tables are not to be updated with
the `ets` function, as the disc is not updated.

The Fun can also be passed as an argument to the function
[`mnesia:activity/2,3,4`](`mnesia:activity/2`), which enables use of customized
activity access callback modules. It can either be obtained directly by stating
the module name as argument, or implicitly by use of configuration parameter
`access_module`. A customized callback module can be used for several purposes,
such as providing triggers, integrity constraints, runtime statistics, or
virtual tables.

The callback module does not have to access real `Mnesia` tables, it is free to
do whatever it wants as long as the callback interface is fulfilled.

[Appendix B, Activity Access Callback Interface](mnesia_app_b.md) provides the
source code, `mnesia_frag.erl`, for one alternative implementation. The
context-sensitive function `mnesia:table_info/2` can be used to provide virtual
information about a table. One use of this is to perform `QLC` queries within an
activity context with a customized callback module. By providing table
information about table indexes and other `QLC` requirements, `QLC` can be used
as a generic query language to access virtual tables.

QLC queries can be performed in all these activity contexts (`transaction`,
`sync_transaction`, `async_dirty`, `sync_dirty`, and `ets`). The `ets` activity
only works if the table has no indexes.

> #### Note {: .info }
>
> The function `mnesia:dirty_*` always executes with `async_dirty` semantics
> regardless of which activity access contexts that are started. It can even
> start contexts without any enclosing activity access context.

## Nested Transactions

Transactions can be nested in an arbitrary fashion. A child transaction must run
in the same process as its parent. When a child transaction terminates, the
caller of the child transaction gets return value `{aborted, Reason}` and any
work performed by the child is erased. If a child transaction commits, the
records written by the child are propagated to the parent.

No locks are released when child transactions terminate. Locks created by a
sequence of nested transactions are kept until the topmost transaction
terminates. Furthermore, any update performed by a nested transaction is only
propagated in such a manner so that the parent of the nested transaction sees
the updates. No final commitment is done until the top-level transaction
terminates. So, although a nested transaction returns `{atomic, Val}`, if the
enclosing parent transaction terminates, the entire nested operation terminates.

The ability to have nested transaction with identical semantics as top-level
transaction makes it easier to write library functions that manipulate `Mnesia`
tables.

Consider a function that adds a subscriber to a telephony system:

```erlang
add_subscriber(S) ->
    mnesia:transaction(fun() ->
        case mnesia:read( ..........
```

This function needs to be called as a transaction. Assume that you wish to write
a function that both calls the function `add_subscriber/1` and is in itself
protected by the context of a transaction. By calling `add_subscriber/1` from
within another transaction, a nested transaction is created.

Also, different activity access contexts can be mixed while nesting. However,
the dirty ones (`async_dirty`, `sync_dirty`, and `ets`) inherit the transaction
semantics if they are called inside a transaction and thus grab locks and use
two or three phase commit.

_Example:_

```erlang
add_subscriber(S) ->
    mnesia:transaction(fun() ->
       %% Transaction context
       mnesia:read({some_tab, some_data}),
       mnesia:sync_dirty(fun() ->
           %% Still in a transaction context.
           case mnesia:read( ..) ..end), end).
add_subscriber2(S) ->
    mnesia:sync_dirty(fun() ->
       %% In dirty context
       mnesia:read({some_tab, some_data}),
       mnesia:transaction(fun() ->
           %% In a transaction context.
           case mnesia:read( ..) ..end), end).
```

## Pattern Matching

[](){: #matching }

When the function `mnesia:read/3` cannot be used, `Mnesia` provides the
programmer with several functions for matching records against a pattern. The
most useful ones are the following:

```erlang
mnesia:select(Tab, MatchSpecification, LockKind) ->
    transaction abort | [ObjectList]
mnesia:select(Tab, MatchSpecification, NObjects, Lock) ->
    transaction abort | {[Object],Continuation} | '$end_of_table'
mnesia:select(Cont) ->
    transaction abort | {[Object],Continuation} | '$end_of_table'
mnesia:match_object(Tab, Pattern, LockKind) ->
    transaction abort | RecordList
```

These functions match a `Pattern` against all records in table `Tab`. In a
[`mnesia:select`](`mnesia:select/2`) call, `Pattern` is a part of
`MatchSpecification` described in the following. It is not necessarily performed
as an exhaustive search of the entire table. By using indexes and bound values
in the key of the pattern, the actual work done by the function can be condensed
into a few hash lookups. Using `ordered_set` tables can reduce the search space
if the keys are partially bound.

The pattern provided to the functions must be a valid record, and the first
element of the provided tuple must be the `record_name` of the table. The
special element `'_'` matches any data structure in Erlang (also known as an
Erlang term). The special elements `'$<number>'` behave as Erlang variables,
that is, they match anything, bind the first occurrence, and match the coming
occurrences of that variable against the bound value.

Use function [`mnesia:table_info(Tab, wild_pattern)`](`mnesia:table_info/2`) to
obtain a basic pattern, which matches all records in a table, or use the default
value in record creation. Do not make the pattern hard-coded, as this makes the
code more vulnerable to future changes of the record definition.

_Example:_

```erlang
Wildpattern = mnesia:table_info(employee, wild_pattern),
%% Or use
Wildpattern = #employee{_ = '_'},
```

For the employee table, the wild pattern looks as follows:

```erlang
{employee, '_', '_', '_', '_', '_',' _'}.
```

To constrain the match, it is needed to replace some of the `'_'` elements. The
code for matching out all female employees looks as follows:

```erlang
Pat = #employee{sex = female, _ = '_'},
F = fun() -> mnesia:match_object(Pat) end,
Females = mnesia:transaction(F).
```

The match function can also be used to check the equality of different
attributes. For example, to find all employees with an employee number equal to
their room number:

```erlang
Pat = #employee{emp_no = '$1', room_no = '$1', _ = '_'},
F = fun() -> mnesia:match_object(Pat) end,
Odd = mnesia:transaction(F).
```

The function `mnesia:match_object/3` lacks some important features that
[`mnesia:select/3`](`mnesia:select/2`) have. For example, `mnesia:match_object/3`
can only return the matching records, and it cannot express constraints other
than equality. To find the names of the male employees on the second floor:

```erlang
MatchHead = #employee{name='$1', sex=male, room_no={'$2', '_'}, _='_'},
Guard = [{'>=', '$2', 220},{'<', '$2', 230}],
Result = '$1',
mnesia:select(employee,[{MatchHead, Guard, [Result]}])
```

The function `select` can be used to add more constraints and create output that
cannot be done with `mnesia:match_object/3`.

The second argument to `select` is a `MatchSpecification`. A
`MatchSpecification` is a list of `MatchFunction`s, where each `MatchFunction`
consists of a tuple containing `{MatchHead, MatchCondition, MatchBody}`:

- `MatchHead` is the same pattern as used in `mnesia:match_object/3` described
  earlier.
- `MatchCondition` is a list of extra constraints applied to each record.
- `MatchBody` constructs the return values.

For details about the match specifications, see "Match Specifications in Erlang"
in [ERTS](`e:erts:index.html`) User's Guide. For more information, see the
`m:ets` and `m:dets` manual pages in `STDLIB`.

The functions [`select/4`](`mnesia:select/4`) and [`select/1`](`mnesia:select/2`)
are used to get a limited number of results, where `Continuation` gets the next
chunk of results. `Mnesia` uses `NObjects` as a recommendation only. Thus, more
or less results than specified with `NObjects` can be returned in the result
list, even the empty list can be returned even if there are more results to
collect.

> #### Warning {: .warning }
>
> There is a severe performance penalty in using `mnesia:select/1,2,3,4` after
> any modifying operation is done on that table in the same transaction. That
> is, avoid using `mnesia:write/1` or `mnesia:delete/1` before `mnesia:select`
> in the same transaction.

If the key attribute is bound in a pattern, the match operation is efficient.
However, if the key attribute in a pattern is given as `'_'` or `'$1'`, the
whole `employee` table must be searched for records that match. Hence if the
table is large, this can become a time-consuming operation, but it can be
remedied with indexes (see [Indexing](mnesia_chap5.md#indexing)) if the function
[`mnesia:match_object`](`mnesia:match_object/1`) is used.

QLC queries can also be used to search `Mnesia` tables. By using the function
[`mnesia:table/1,2`](`mnesia:table/1`) as the generator inside a QLC query, you
let the query operate on a `Mnesia` table. `Mnesia`-specific options to
`mnesia:table/2` are `{lock, Lock}`, `{n_objects, Integer}`, and
`{traverse, SelMethod}`:

- `lock` specifies whether `Mnesia` is to acquire a read or write lock on the
  table.
- `n_objects` specifies how many results are to be returned in each chunk to
  QLC.
- `traverse` specifies which function `Mnesia` is to use to traverse the table.
  Default `select` is used, but by using
  `{traverse, {select, MatchSpecification}}` as an option to
  [`mnesia:table/2`](`mnesia:table/1`) the user can specify its own view of the
  table.

If no options are specified, a read lock is acquired, 100 results are returned
in each chunk, and `select` is used to traverse the table, that is:

```erlang
mnesia:table(Tab) ->
    mnesia:table(Tab, [{n_objects, 100},{lock, read}, {traverse, select}]).
```

The function [`mnesia:all_keys(Tab)`](`mnesia:all_keys/1`) returns all keys in a
table.

## Iteration

`Mnesia` provides the following functions that iterate over all the records in a
table:

```erlang
mnesia:foldl(Fun, Acc0, Tab) -> NewAcc | transaction abort
mnesia:foldr(Fun, Acc0, Tab) -> NewAcc | transaction abort
mnesia:foldl(Fun, Acc0, Tab, LockType) -> NewAcc | transaction abort
mnesia:foldr(Fun, Acc0, Tab, LockType) -> NewAcc | transaction abort
```

These functions iterate over the `Mnesia` table `Tab` and apply the function
`Fun` to each record. `Fun` takes two arguments, the first is a record from the
table, and the second is the accumulator. `Fun` returns a new accumulator.

The first time `Fun` is applied, `Acc0` is the second argument. The next time
`Fun` is called, the return value from the previous call is used as the second
argument. The term the last call to `Fun` returns is the return value of the
function `mnesia:foldl/3` or `mnesia:foldr/3`.

The difference between these functions is the order the table is accessed for
`ordered_set` tables. For other table types the functions are equivalent.

`LockType` specifies what type of lock that is to be acquired for the iteration,
default is `read`. If records are written or deleted during the iteration, a
write lock is to be acquired.

These functions can be used to find records in a table when it is impossible to
write constraints for the function `mnesia:match_object/3`, or when you want to
perform some action on certain records.

For example, finding all the employees who have a salary less than 10 can look
as follows:

```erlang
find_low_salaries() ->
  Constraint =
       fun(Emp, Acc) when Emp#employee.salary < 10 ->
              [Emp | Acc];
          (_, Acc) ->
              Acc
       end,
  Find = fun() -> mnesia:foldl(Constraint, [], employee) end,
  mnesia:transaction(Find).
```

To raise the salary to 10 for everyone with a salary less than 10 and return the
sum of all raises:

```erlang
increase_low_salaries() ->
   Increase =
       fun(Emp, Acc) when Emp#employee.salary < 10 ->
              OldS = Emp#employee.salary,
              ok = mnesia:write(Emp#employee{salary = 10}),
              Acc + 10 - OldS;
          (_, Acc) ->
              Acc
       end,
  IncLow = fun() -> mnesia:foldl(Increase, 0, employee, write) end,
  mnesia:transaction(IncLow).
```

Many nice things can be done with the iterator functions but take some caution
about performance and memory use for large tables.

Call these iteration functions on nodes that contain a replica of the table.
Each call to the function `Fun` access the table and if the table resides on
another node it generates much unnecessary network traffic.

`Mnesia` also provides some functions that make it possible for the user to
iterate over the table. The order of the iteration is unspecified if the table
is not of type `ordered_set`:

```erlang
mnesia:first(Tab) ->  Key | transaction abort
mnesia:last(Tab)  ->  Key | transaction abort
mnesia:next(Tab,Key)  ->  Key | transaction abort
mnesia:prev(Tab,Key)  ->  Key | transaction abort
mnesia:snmp_get_next_index(Tab,Index) -> {ok, NextIndex} | endOfTable
```

The order of `first`/`last` and `next`/`prev` is only valid for `ordered_set`
tables, they are synonyms for other tables. When the end of the table is
reached, the special key `'$end_of_table'` is returned.

If records are written and deleted during the traversal, use the function
`mnesia:foldl/3` or `mnesia:foldr/3` with a `write` lock. Or the function
`mnesia:write_lock_table/1` when using `first` and `next`.

Writing or deleting in transaction context creates a local copy of each modified
record. Thus, modifying each record in a large table uses much memory. `Mnesia`
compensates for every written or deleted record during the iteration in a
transaction context, which can reduce the performance. If possible, avoid
writing or deleting records in the same transaction before iterating over the
table.

In dirty context, that is, `sync_dirty` or `async_dirty`, the modified records
are not stored in a local copy; instead, each record is updated separately. This
generates much network traffic if the table has a replica on another node and
has all the other drawbacks that dirty operations have. Especially for commands
`mnesia:first/1` and `mnesia:next/2`, the same drawbacks as described previously
for `mnesia:dirty_first/1` and `mnesia:dirty_next/2` applies, that is, no
writing to the table is to be done during iteration.
