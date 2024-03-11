Built-in term storage.

This module is an interface to the Erlang built-in term storage BIFs. These
provide the ability to store very large quantities of data in an Erlang runtime
system, and to have constant access time to the data. (In the case of
`ordered_set`, see below, access time is proportional to the logarithm of the
number of stored objects.)

Data is organized as a set of dynamic tables, which can store tuples. Each table
is created by a process. When the process terminates, the table is automatically
destroyed. Every table has access rights set at creation.

Tables are divided into four different types, `set`, `ordered_set`, `bag`, and
`duplicate_bag`. A `set` or `ordered_set` table can only have one object
associated with each key. A `bag` or `duplicate_bag` table can have many objects
associated with each key.

Insert and lookup times in tables of type `set` are constant, regardless of the
table size. For table types `bag` and `duplicate_bag` time is proportional to
the number of objects with the same key. Even seemingly unrelated keys may
inflict linear search to be skipped past while looking for the key of interest
(due to hash collision).

> #### Warning {: .warning }
>
> For tables of type `bag` and `duplicate_bag`, avoid inserting an extensive
> amount of objects with the same key. It will hurt insert and lookup
> performance as well as real time characteristics of the runtime environment
> (hash bucket linear search do not yield).

The `ordered_set` table type uses a binary search tree. Insert and lookup times
are proportional to the logarithm of the number of objects in the table.

[](){: #max_ets_tables }

> #### Note {: .info }
>
> The number of tables stored at one Erlang node _used_ to be limited. This is
> no longer the case (except by memory usage). The previous default limit was
> about 1400 tables and could be increased by setting the environment variable
> `ERL_MAX_ETS_TABLES` or the command line option
> [`+e`](`e:erts:erl_cmd.md#%2Be`) before starting the Erlang runtime system.
> This hard limit has been removed, but it is currently useful to set the
> `ERL_MAX_ETS_TABLES` anyway. It should be set to an approximate of the maximum
> amount of tables used since an internal table for named tables is sized using
> this value. If large amounts of named tables are used and `ERL_MAX_ETS_TABLES`
> hasn't been increased, the performance of named table lookup will degrade.

Notice that there is no automatic garbage collection for tables. Even if there
are no references to a table from any process, it is not automatically destroyed
unless the owner process terminates. To destroy a table explicitly, use function
`delete/1`. The default owner is the process that created the table. To transfer
table ownership at process termination, use option [`heir`](`m:ets#heir`) or
call `give_away/3`.

Some implementation details:

- In the current implementation, every object insert and look-up operation
  results in a copy of the object.
- `'$end_of_table'` is not to be used as a key, as this atom is used to mark the
  end of the table when using functions `first/1` and `next/2`.

Notice the subtle difference between _matching_ and _comparing equal_, which is
demonstrated by table types `set` and `ordered_set`:

- Two Erlang terms `match` if they are of the same type and have the same value,
  so that `1` matches `1`, but not `1.0` (as `1.0` is a `t:float/0` and not an
  `t:integer/0`).
- Two Erlang terms _compare equal_ if they either are of the same type and
  value, or if both are numeric types and extend to the same value, so that `1`
  compares equal to both `1` and `1.0`.
- The `ordered_set` works on the _Erlang term order_ and no defined order exists
  between an `t:integer/0` and a `t:float/0` that extends to the same value.
  Hence the key `1` and the key `1.0` are regarded as equal in an `ordered_set`
  table.

[](){: #ets_failures }

## Failures

Functions in this module fail by raising an error exception with error reason:

- **`badarg`** - If any argument has the wrong format.

- **`badarg`** - If the table identifier is invalid.

- **`badarg`** - If the operation is denied because of table access rights
  ([protected](`m:ets#protected`) or [private](`m:ets#private`)).

- **`system_limit`** - Modification of a value causes it to not be representable
  internally in the VM. For example, incrementation of a counter past the
  largest integer representable.

- **`system_limit`** - If a match specification passed as argument has excessive
  nesting which causes scheduler stack exhaustion for the scheduler that the
  calling process is executing on.
  [Scheduler stack size](`e:erts:erl_cmd.md#sched_thread_stack_size`) can be
  configured when starting the runtime system.

[](){: #concurrency }

## Concurrency

This module provides some limited support for concurrent access. All updates to
single objects are guaranteed to be both _atomic_ and _isolated_. This means
that an updating operation to a single object either succeeds or fails
completely without any effect (atomicity) and that no intermediate results of
the update can be seen by other processes (isolation). Some functions that
update many objects state that they even guarantee atomicity and isolation for
the entire operation. In database terms the isolation level can be seen as
"serializable", as if all isolated operations are carried out serially, one
after the other in a strict order.

[](){: #traversal }

## Table traversal

There are different ways to traverse through the objects of a table.

- _Single-step_ traversal one key at at time, using `first/1`, `next/2`,
  `last/1` and `prev/2`.
- _Single-step_ traversal one key at at time, but using `first_lookup/1`,
  `next_lookup/2`, `last_lookup/1` and `prev_lookup/2`. This is more efficient
  when you also need to lookup the objects for the keys.
- Search with simple _match patterns_, using [`match/1/2/3`](`match/1`),
  `match_delete/2` and [`match_object/1/2/3`](`match_object/1`).
- Search with more powerful _match specifications_, using
  [`select/1/2/3`](`select/1`), `select_count/2`, `select_delete/2`,
  `select_replace/2` and [`select_reverse/1/2/3`](`select_reverse/1`).
- _Table conversions_, using [`tab2file/2/3`](`tab2file/2`) and `tab2list/1`.

No table traversal will guarantee a consistent snapshot of the entire table if
the table is also updated by concurrent processes during the traversal. The
result of each concurrently updated object may be seen (or not) depending on if
it has happened when the traversal visits that part of the table. The only way
to guarantee a full consistent table snapshot (if you really need that) is to
disallow concurrent updates during the entire traversal.

Moreover, traversals not done in a _safe_ way, on tables where keys are inserted
or deleted during the traversal, may yield the following undesired effects:

- Any key may be missed.
- Any key may be found more than once.
- The traversal may fail with `badarg` exception if keys are deleted.

A table traversal is _safe_ if either

- the table is of type `ordered_set`.
- the entire table traversal is done within one ETS function call.
- function `safe_fixtable/2` is used to keep the table fixated during the entire
  traversal.

> #### Note {: .info }
>
> Even though the access of a single object is always guaranteed to be
> [atomic and isolated](`m:ets#module-concurrency`), each traversal through a table to
> find the next key is not done with such guarantees. This is often not a
> problem, but may cause rare subtle "unexpected" effects if a concurrent
> process inserts objects during a traversal. For example, consider one process
> doing
>
> ```erlang
> ets:new(t, [ordered_set, named_table]),
> ets:insert(t, {1}),
> ets:insert(t, {2}),
> ets:insert(t, {3}),
> ```
>
> A concurrent call to `ets:first(t)`, done by another process, may then in rare
> cases return `2` even though `2` has never existed in the table ordered as the
> first key. In the same way, a concurrent call to `ets:next(t, 1)` may return
> `3` even though `3` never existed in the table ordered directly after `1`.
>
> Effects like this are improbable but possible. The probability will further be
> reduced (if not vanish) if table option
> [`write_concurrency`](`m:ets#new_2_write_concurrency`) is not enabled. This
> can also only be a potential concern for `ordered_set` where the traversal
> order is defined.

Traversals using `match` and `select` functions may not need to scan the entire
table depending on how the key is specified. A match pattern with a _fully bound
key_ (without any match variables) will optimize the operation to a single key
lookup without any table traversal at all. For `ordered_set` a _partially bound
key_ will limit the traversal to only scan a subset of the table based on term
order. A partially bound key is either a list or a tuple with a prefix that is
fully bound. Example:

```erlang
1> T = ets:new(t,[ordered_set]), ets:insert(T, {"555-1234", "John Smith"}).
true
2> %% Efficient search of all with area code 555
2> ets:match(T,{[$5,$5,$5,$- |'$1'],'$2'}).
[["1234","John Smith"]]
```

[](){: #match_spec }

## Match Specifications

Some of the functions use a _match specification_, `match_spec`. For a brief
explanation, see `select/2`. For a detailed description, see section
[Match Specifications in Erlang](`e:erts:match_spec.md`) in ERTS User's Guide.

A match specifications with excessive nesting will cause a
[`system_limit`](`m:ets#ets_failures`) error exception to be raised.

