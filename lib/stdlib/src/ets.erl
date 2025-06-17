%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(ets).
-moduledoc """
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
""".

-compile(nowarn_deprecated_catch).

%% Interface to the Term store BIF's
%% ets == Erlang Term Store

-export([file2tab/1,
	 file2tab/2,
	 foldl/3, foldr/3,
	 match_delete/2,
	 tab2file/2,
	 tab2file/3,
	 tabfile_info/1,
	 from_dets/2,
	 to_dets/2,
	 init_table/2,
	 test_ms/2,
	 tab2list/1,
         table/1,
         table/2,
	 fun2ms/1,
	 match_spec_run/2,
	 repair_continuation/2]).

-export([i/0, i/1, i/2, i/3]).

-export_type([table/0, table_access/0, table_type/0,
              tid/0, match_spec/0, compiled_match_spec/0, match_pattern/0]).

%%-----------------------------------------------------------------------------

-type table_access()  :: public | protected | private.
-type table()         :: atom() | tid().
-type table_type()    :: set | ordered_set | bag | duplicate_bag.
-doc """
Opaque continuation used by [`select/1,3`](`select/1`),
[`select_reverse/1,3`](`select_reverse/1`), [`match/1,3`](`match/1`), and
[`match_object/1,3`](`match_object/1`).
""".
-type continuation()  :: '$end_of_table'
                       | {table(),integer(),integer(),compiled_match_spec(),list(),integer()}
                       | {table(),_,_,integer(),compiled_match_spec(),list(),integer(),integer()}.

-doc "A table identifier, as returned by `new/2`.".
-opaque tid()         :: reference().

-type match_pattern() :: atom() | tuple().
-doc "A match specification, see [Match Specifications](`m:ets#match_spec`).".
-type match_spec()    :: [{match_pattern(), [_], [_]}].

%% Keep for backwards compatibility
-export_type([tab/0, comp_match_spec/0]).
-type tab()           :: table().

%%-----------------------------------------------------------------------------

%%% BIFs

-export([all/0, delete/1, delete/2, delete_all_objects/1,
         delete_object/2, first/1, first_lookup/1, give_away/3, info/1, info/2,
         insert/2, insert_new/2, is_compiled_ms/1, last/1, last_lookup/1, lookup/2,
         lookup_element/3, lookup_element/4, match/1, match/2, match/3, match_object/1,
         match_object/2, match_object/3, match_spec_compile/1,
         match_spec_run_r/3, member/2, new/2, next/2, next_lookup/2, prev/2, prev_lookup/2,
         rename/2, safe_fixtable/2, select/1, select/2, select/3,
         select_count/2, select_delete/2, select_replace/2, select_reverse/1,
         select_reverse/2, select_reverse/3, setopts/2, slot/2,
         take/2,
         update_counter/3, update_counter/4, update_element/3, update_element/4,
         whereis/1]).

%% internal exports
-export([internal_request_all/0,
         internal_delete_all/2,
         internal_select_delete/2]).

-doc """
Returns a list of all tables at the node. Named tables are specified by their
names, unnamed tables are specified by their table identifiers.

There is no guarantee of consistency in the returned list. Tables created or
deleted by other processes "during" the `ets:all()` call either are or are not
included in the list. Only tables created/deleted _before_ `ets:all()` is called
are guaranteed to be included/excluded.
""".
-spec all() -> [Table] when
      Table :: table().

all() ->
    receive_all(ets:internal_request_all(),
                erlang:system_info(schedulers),
                []).

receive_all(_Ref, 0, All) ->
    All;
receive_all(Ref, N, All) ->
    receive
        {Ref, SchedAll} ->
            receive_all(Ref, N-1, SchedAll ++ All)
    end.

-doc false.
-spec internal_request_all() -> reference().

internal_request_all() ->
    erlang:nif_error(undef).

-doc "Deletes the entire table `Table`.".
-spec delete(Table) -> true when
      Table :: table().

delete(_) ->
    erlang:nif_error(undef).

-doc """
Deletes all objects with key `Key` from table `Table`. This function succeeds
even if no objects with key `Key` exist.
""".
-spec delete(Table, Key) -> true when
      Table :: table(),
      Key :: term().

delete(_, _) ->
    erlang:nif_error(undef).

-doc """
Delete all objects in the ETS table `Table`. The operation is guaranteed to be
[atomic and isolated](`m:ets#module-concurrency`).
""".
-spec delete_all_objects(Table) -> true when
      Table :: table().

delete_all_objects(Table) ->
    _ = ets:internal_delete_all(Table, undefined),
    true.

-doc false.
-spec internal_delete_all(Table, undefined) -> NumDeleted when
      Table :: table(),
      NumDeleted :: non_neg_integer().

internal_delete_all(_, _) ->
    erlang:nif_error(undef).

-doc """
Delete the exact object `Object` from the ETS table, leaving objects with the
same key but other differences (useful for type `bag`). In a `duplicate_bag`
table, all instances of the object are deleted.
""".
-spec delete_object(Table, Object) -> true when
      Table :: table(),
      Object :: tuple().

delete_object(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns the first key `Key` in table `Table`. For an `ordered_set` table, the
first key in Erlang term order is returned. For other table types, the first key
according to the internal order of the table is returned. If the table is empty,
`'$end_of_table'` is returned.

To find subsequent keys in the table, use `next/2`.
""".
-spec first(Table) -> Key | '$end_of_table' when
      Table :: table(),
      Key :: term().

first(_) ->
    erlang:nif_error(undef).

-doc """
Similar to `first/1` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `first/1` followed by a
`lookup/2`. If the table is empty, `'$end_of_table'` is returned.

To find subsequent objects in the table, use `next_lookup/2`.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec first_lookup(Table) -> {Key, [Object]} | '$end_of_table' when
    Table :: table(),
    Key :: term(),
    Object :: tuple().

first_lookup(_) ->
    erlang:nif_error(undef).

-doc """
Make process `Pid` the new owner of table `Table`. If successful, message
`{'ETS-TRANSFER',Table,FromPid,GiftData}` is sent to the new owner.

The process `Pid` must be alive, local, and not already the owner of the table.
The calling process must be the table owner.

Notice that this function does not affect option [`heir`](`m:ets#heir`) of the
table. A table owner can, for example, set `heir` to itself, give the table
away, and then get it back if the receiver terminates.
""".
-spec give_away(Table, Pid, GiftData) -> true when
      Table :: table(),
      Pid :: pid(),
      GiftData :: term().

give_away(_, _, _) ->
    erlang:nif_error(undef).

-doc """
Returns information about table `Table` as a list of tuples. If `Table` has the
correct type for a table identifier, but does not refer to an existing ETS
table, `undefined` is returned. If `Table` is not of the correct type, a
`badarg` exception is raised.

- **`{compressed, boolean()}`** - Indicates if the table is compressed.

- **`{decentralized_counters, boolean()}`** - Indicates whether the table uses
  `decentralized_counters`.

- **`{heir, pid() | none}`** - The pid of the heir of the table, or `none` if no
  heir is set.

- **`{id,`[ `tid()`](`t:tid/0`)`}`** - The table identifier.

- **`{keypos, integer() >= 1}`** - The key position.

- **`{memory, integer() >= 0}`** - The number of words allocated to the table.

- **`{name, atom()}`** - The table name.

- **`{named_table, boolean()}`** - Indicates if the table is named.

- **`{node, node()}`** - The node where the table is stored. This field is no
  longer meaningful, as tables cannot be accessed from other nodes.

- **`{owner, pid()}`** - The pid of the owner of the table.

- **`{protection,` [`access()`](`t:table_access/0`)`}`** - The table access
  rights.

- **`{size, integer() >= 0}`** - The number of objects inserted in the table.

- **`{type,` [`type()`](`t:table_type/0`)`}`** - The table type.

- **`{read_concurrency, boolean()}`** - Indicates whether the table uses
  `read_concurrency` or not.

- **`{write_concurrency, WriteConcurrencyAlternative}`** - Indicates which
  `write_concurrency` option the table uses.

> #### Note {: .info }
>
> The execution time of this function is affected by the
> [`decentralized_counters`](`m:ets#new_2_decentralized_counters`) table option.
> The execution time is much longer when the `decentralized_counters` option is
> set to `true` than when the `decentralized_counters` option is set to `false`.
""".
-spec info(Table) -> InfoList | undefined when
      Table :: table(),
      InfoList :: [InfoTuple],
      InfoTuple :: {compressed, boolean()}
                 | {decentralized_counters, boolean()}
                 | {heir, pid() | none}
                 | {id, tid()}
                 | {keypos, pos_integer()}
                 | {memory, non_neg_integer()}
                 | {name, atom()}
                 | {named_table, boolean()}
                 | {node, node()}
                 | {owner, pid()}
                 | {protection, table_access()}
                 | {size, non_neg_integer()}
                 | {type, table_type()}
		 | {write_concurrency, boolean() | auto}
		 | {read_concurrency, boolean()}.

info(_) ->
    erlang:nif_error(undef).

-doc """
Returns the information associated with `Item` for table `Table`, or returns
`undefined` if `Table` does not refer an existing ETS table. If `Table` is not
of the correct type, or if `Item` is not one of the allowed values, a `badarg`
exception is raised.

In addition to the `{Item,Value}` pairs defined for `info/1`, the following
items are allowed:

- `Item=binary, Value=BinInfo`

  `BinInfo` is a list containing miscellaneous information about binaries kept
  by the table. This `Item` can be changed or removed without prior notice. In
  the current implementation `BinInfo` is a list of tuples
  `{BinaryId,BinarySize,BinaryRefcCount}`.

- `Item=fixed, Value=boolean()`

  Indicates if the table is fixed by any process.

- [](){: #info_2_safe_fixed_monotonic_time }

  `Item=safe_fixed|safe_fixed_monotonic_time, Value={FixationTime,Info}|false`

  If the table is fixed using `safe_fixtable/2`, the call returns a tuple where
  `FixationTime` is the last time when the table changed from unfixed to fixed.

  The format and value of `FixationTime` depends on `Item`:

  - **`safe_fixed`** - `FixationTime` corresponds to the result returned by
    `erlang:timestamp/0` at the time of fixation. Notice that when the system
    uses single or multi
    [time warp modes](`e:erts:time_correction.md#time-warp-modes`) this can
    produce strange results, as the use of `safe_fixed` is not
    [time warp safe](`e:erts:time_correction.md#time-warp-safe-code`). Time warp
    safe code must use `safe_fixed_monotonic_time` instead.

  - **`safe_fixed_monotonic_time`** - `FixationTime` corresponds to the result
    returned by `erlang:monotonic_time/0` at the time of fixation. The use of
    `safe_fixed_monotonic_time` is
    [time warp safe](`e:erts:time_correction.md#time-warp-safe-code`).

  `Info` is a possibly empty lists of tuples `{Pid,RefCount}`, one tuple for
  every process the table is fixed by now. `RefCount` is the value of the
  reference counter and it keeps track of how many times the table has been
  fixed by the process.

  Table fixations are not limited to `safe_fixtable/2`. Temporary fixations may
  also be done by for example [traversing functions](`m:ets#traversal`) like
  `select` and `match`. Such table fixations are automatically released before
  the corresponding functions returns, but they may be seen by a concurrent call
  to `ets:info(T,safe_fixed|safe_fixed_monotonic_time)`.

  If the table is not fixed at all, the call returns `false`.

- `Item=stats, Value=tuple()`

  Returns internal statistics about tables on an internal format used by OTP
  test suites. Not for production use.

> #### Note {: .info }
>
> The execution time of this function is affected by the
> [`decentralized_counters`](`m:ets#new_2_decentralized_counters`) table option
> when the second argument of the function is `size` or `memory`. The execution
> time is much longer when the `decentralized_counters` option is set to `true`
> than when the `decentralized_counters` option is set to `false`.
""".
-spec info(Table, Item) -> Value | undefined when
      Table :: table(),
      Item :: binary | compressed | decentralized_counters | fixed | heir | id | keypos | memory
            | name | named_table | node | owner | protection
            | safe_fixed | safe_fixed_monotonic_time | size | stats | type
	    | write_concurrency | read_concurrency,
      Value :: term().

info(_, _) ->
    erlang:nif_error(undef).

-doc """
Inserts the object or all of the objects in list `ObjectOrObjects` into table
`Table`.

- If the table type is `set` and the key of the inserted objects _matches_ the
  key of any object in the table, the old object is replaced.
- If the table type is `ordered_set` and the key of the inserted object
  _compares equal_ to the key of any object in the table, the old object is
  replaced.
- If the table type is `bag` and the object _matches_ any whole object in the
  table, the object is not inserted.
- If the list contains more than one object with _matching_ keys and the table
  type is `set`, one is inserted, which one is not defined. The same holds for
  table type `ordered_set` if the keys _compare equal_.

The entire operation is guaranteed to be
[atomic and isolated](`m:ets#module-concurrency`), even when a list of objects is
inserted.

[](){: #insert_list_order }

For `bag` and `duplicate_bag`, objects in the list with identical keys will be
inserted in list order (from head to tail). That is, a subsequent call to
[`lookup(T,Key)`](`lookup/2`) will return them in that inserted order.

> #### Note {: .info }
>
> For `bag` the insertion order of indentical keys described above was
> accidentally reverted in OTP 23.0 and later fixed in OTP 25.3. That is, from
> OTP 23.0 up until OTP 25.3 the objects in a list are inserted in reverse order
> (from tail to head).
>
> For `duplicate_bag` the same faulty reverse insertion exist from OTP 23.0
> until OTP 25.3. However, it is unpredictable and may or may not happen. A
> longer list will increase the probabiliy of the insertion being done in
> reverse.
""".
-spec insert(Table, ObjectOrObjects) -> true when
      Table :: table(),
      ObjectOrObjects :: tuple() | [tuple()].

insert(_, _) ->
    erlang:nif_error(undef).

-doc """
Same as `insert/2` except that instead of overwriting objects with the same key
(for `set` or `ordered_set`) or adding more objects with keys already existing
in the table (for `bag` and `duplicate_bag`), `false` is returned.

If `ObjectOrObjects` is a list, the function checks _every_ key before inserting
anything. Nothing is inserted unless _all_ keys present in the list are absent
from the table. Like [`insert/2`](`insert/2`), the entire operation is
guaranteed to be [atomic and isolated](`m:ets#module-concurrency`).
""".
-spec insert_new(Table, ObjectOrObjects) -> boolean() when
      Table :: table(),
      ObjectOrObjects :: tuple() | [tuple()].

insert_new(_, _) ->
    erlang:nif_error(undef).

-doc """
Checks if a term represent a valid compiled
[match specification](`m:ets#match_spec`). A compiled match specification is
only valid on the Erlang node where it was compiled by calling
`match_spec_compile/1`.

> #### Note {: .info }
>
> Before STDLIB 3.4 (OTP 20.0) compiled match specifications did not have an
> external representation. If passed through
> [`binary_to_term(term_to_binary(CMS))`](`binary_to_term/1`) or sent to another
> node and back, the result was always an empty binary `<<>>`.
>
> After STDLIB 3.4 (OTP 20.0) compiled match specifications have an external
> representation as a node specific reference to the original compiled match
> specification. If passed through
> [`binary_to_term(term_to_binary(CMS))`](`binary_to_term/1`) or sent to another
> node and back, the result _may or may not_ be a valid compiled match
> specification depending on if the original compiled match specification was
> still alive.
""".
-spec is_compiled_ms(Term) -> boolean() when
      Term :: term().

is_compiled_ms(_) ->
    erlang:nif_error(undef).

-doc """
Returns the last key `Key` according to Erlang term order in table `Table` of
type `ordered_set`. For other table types, the function is synonymous to
`first/1`. If the table is empty, `'$end_of_table'` is returned.

To find preceding keys in the table, use `prev/2`.
""".
-spec last(Table) -> Key | '$end_of_table' when
      Table :: table(),
      Key :: term().

last(_) ->
    erlang:nif_error(undef).

-doc """
Similar to `last/1` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `last/1` followed by a
`lookup/2`. If the table is empty, `'$end_of_table'` is returned.

To find preceding objects in the table, use `prev_lookup/2`.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec last_lookup(Table) -> {Key, [Object]} | '$end_of_table' when
    Table :: table(),
    Key :: term(),
    Object :: tuple().

last_lookup(_) ->
    erlang:nif_error(undef).

-doc """
Returns a list of all objects with key `Key` in table `Table`.

- For tables of type `set`, `bag`, or `duplicate_bag`, an object is returned
  only if the specified key _matches_ the key of the object in the table.
- For tables of type `ordered_set`, an object is returned if the specified key
  _compares equal_ to the key of an object in the table.

The difference is the same as between `=:=` and `==`.

As an example, one can insert an object with `t:integer/0` `1` as a key in an
`ordered_set` and get the object returned as a result of doing a
[`lookup/2`](`lookup/2`) with `t:float/0` `1.0` as the key to search for.

For tables of type `set` or `ordered_set`, the function returns either the empty
list or a list with one element, as there cannot be more than one object with
the same key. For tables of type `bag` or `duplicate_bag`, the function returns
a list of arbitrary length.

Notice that the sequential order of object insertions is preserved; the first
object inserted with the specified key is the first in the resulting list, and
so on. See also the note about
[list insertion order](`m:ets#insert_list_order`).
""".
-spec lookup(Table, Key) -> [Object] when
      Table :: table(),
      Key :: term(),
      Object :: tuple().

lookup(_, _) ->
    erlang:nif_error(undef).

-doc """
For a table `Table` of type `set` or `ordered_set`, the function returns the
`Pos`:th element of the object with key `Key`.

For tables of type `bag` or `duplicate_bag`, the functions returns a list with
the `Pos`:th element of every object with key `Key`.

If no object with key `Key` exists, the function exits with reason `badarg`.

If `Pos` is larger than the size of the tuple, the function exits with reason
`badarg`.

The difference between `set`, `bag`, and `duplicate_bag` on one hand, and
`ordered_set` on the other, regarding the fact that `ordered_set` view keys as
equal when they _compare equal_ whereas the other table types regard them equal
only when they _match_, holds for [`lookup_element/3`](`lookup_element/3`).
""".
-spec lookup_element(Table, Key, Pos) -> Elem when
      Table :: table(),
      Key :: term(),
      Pos :: pos_integer(),
      Elem :: term() | [term()].

lookup_element(_, _, _) ->
    erlang:nif_error(undef).

-doc """
For a table `Table` of type `set` or `ordered_set`, the function returns the
`Pos`:th element of the object with key `Key`.

For tables of type `bag` or `duplicate_bag`, the functions returns a list with
the `Pos`:th element of every object with key `Key`.

If no object with key `Key` exists, the function returns `Default`.

If `Pos` is larger than the size of any tuple with a matching key, the function
exits with reason `badarg`.

The difference between `set`, `bag`, and `duplicate_bag` on one hand, and
`ordered_set` on the other, regarding the fact that `ordered_set` view keys as
equal when they _compare equal_ whereas the other table types regard them equal
only when they _match_, holds for [`lookup_element/4`](`lookup_element/4`).
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec lookup_element(Table, Key, Pos, Default) -> Elem when
    Table :: table(),
    Key :: term(),
    Pos :: pos_integer(),
    Default :: term(),
    Elem :: term() | [term()].

lookup_element(_, _, _, _) ->
  erlang:nif_error(undef).

-doc """
Matches the objects in table `Table` against pattern `Pattern`.

A pattern is a term that can contain:

- Bound parts (Erlang terms)
- `'_'` that matches any Erlang term
- Pattern variables `'$N'`, where `N`=0,1,...

The function returns a list with one element for each matching object, where
each element is an ordered list of pattern variable bindings, for example:

```erlang
6> ets:match(T, '$1'). % Matches every object in table
[[{rufsen,dog,7}],[{brunte,horse,5}],[{ludde,dog,5}]]
7> ets:match(T, {'_',dog,'$1'}).
[[7],[5]]
8> ets:match(T, {'_',cow,'$1'}).
[]
```

If the key is specified in the pattern, the match is very efficient. If the key
is not specified, that is, if it is a variable or an underscore, the entire
table must be searched. The search time can be substantial if the table is very
large.

For tables of type `ordered_set`, the result is in the same order as in a
`first`/`next` traversal.
""".
-spec match(Table, Pattern) -> [Match] when
      Table :: table(),
      Pattern :: match_pattern(),
      Match :: [term()].

match(_, _) ->
    erlang:nif_error(undef).

-doc """
Works like `match/2`, but returns only a limited (`Limit`) number of matching
objects. Term `Continuation` can then be used in subsequent calls to `match/1`
to get the next chunk of matching objects. This is a space-efficient way to work
on objects in a table, which is faster than traversing the table object by
object using `first/1` and `next/2`.

If the table is empty, `'$end_of_table'` is returned.

Use `safe_fixtable/2` to guarantee [safe traversal](`m:ets#traversal`) for
subsequent calls to `match/1`.
""".
-spec match(Table, Pattern, Limit) -> {[Match], Continuation} |
                                       '$end_of_table'  when
      Table :: table(),
      Pattern :: match_pattern(),
      Limit :: pos_integer(),
      Match :: [term()],
      Continuation :: continuation().

match(_, _, _) ->
    erlang:nif_error(undef).

-doc """
Continues a match started with `match/3`. The next chunk of the size specified
in the initial [`match/3`](`match/3`) call is returned together with a new
`Continuation`, which can be used in subsequent calls to this function.

When there are no more objects in the table, `'$end_of_table'` is returned.
""".
-spec match(Continuation) -> {[Match], Continuation} |
                                '$end_of_table'  when
      Match :: [term()],
      Continuation :: continuation().

match(_) ->
    erlang:nif_error(undef).

-doc """
Matches the objects in table `Table` against pattern `Pattern`. For a
description of patterns, see `match/2`. The function returns a list of all
objects that match the pattern.

If the key is specified in the pattern, the match is very efficient. If the key
is not specified, that is, if it is a variable or an underscore, the entire
table must be searched. The search time can be substantial if the table is very
large.

For tables of type `ordered_set`, the result is in the same order as in a
`first`/`next` traversal.
""".
-spec match_object(Table, Pattern) -> [Object] when
      Table :: table(),
      Pattern :: match_pattern(),
      Object :: tuple().

match_object(_, _) ->
    erlang:nif_error(undef).

-doc """
Works like `match_object/2`, but only returns a limited (`Limit`) number of
matching objects. Term `Continuation` can then be used in subsequent calls to
`match_object/1` to get the next chunk of matching objects. This is a
space-efficient way to work on objects in a table, which is faster than
traversing the table object by object using `first/1` and `next/2`.

If the table is empty, `'$end_of_table'` is returned.

Use `safe_fixtable/2` to guarantee [safe traversal](`m:ets#traversal`) for
subsequent calls to `match_object/1`.
""".
-spec match_object(Table, Pattern, Limit) -> {[Object], Continuation} |
                                           '$end_of_table' when
      Table :: table(),
      Pattern :: match_pattern(),
      Limit :: pos_integer(),
      Object :: tuple(),
      Continuation :: continuation().

match_object(_, _, _) ->
    erlang:nif_error(undef).

-doc """
Continues a match started with `match_object/3`. The next chunk of the size
specified in the initial [`match_object/3`](`match_object/3`) call is returned
together with a new `Continuation`, which can be used in subsequent calls to
this function.

When there are no more objects in the table, `'$end_of_table'` is returned.
""".
-spec match_object(Continuation) -> {[Object], Continuation} |
                                    '$end_of_table' when
      Object :: tuple(),
      Continuation :: continuation().

match_object(_) ->
    erlang:nif_error(undef).

-doc """
Transforms a [match specification](`m:ets#match_spec`) into an internal
representation that can be used in subsequent calls to `match_spec_run/2`. The
internal representation is opaque. To check the validity of a compiled match
specification, use `is_compiled_ms/1`.

If term `MatchSpec` does not represent a valid match specification, a `badarg`
exception is raised.

> #### Note {: .info }
>
> This function has limited use in normal code. It is used by the `m:dets`
> module to perform the `dets:select/1` operations.
""".
-spec match_spec_compile(MatchSpec) -> CompiledMatchSpec when
      MatchSpec :: match_spec(),
      CompiledMatchSpec :: compiled_match_spec().

match_spec_compile(_) ->
    erlang:nif_error(undef).

-doc false.
-spec match_spec_run_r(List, CompiledMatchSpec, list()) -> list() when
      List :: [term()],
      CompiledMatchSpec :: compiled_match_spec().

match_spec_run_r(_, _, _) ->
    erlang:nif_error(undef).

-doc """
Works like `lookup/2`, but does not return the objects. Returns `true` if one or
more elements in the table has key `Key`, otherwise `false`.
""".
-spec member(Table, Key) -> boolean() when
      Table :: table(),
      Key :: term().

member(_, _) ->
    erlang:nif_error(undef).

-doc """
Creates a new table and returns a table identifier that can be used in
subsequent operations. The table identifier can be sent to other processes so
that a table can be shared between different processes within a node.

Parameter `Options` is a list of options that specifies table type, access
rights, key position, and whether the table is named. Default values are used
for omitted options. This means that not specifying any options (`[]`) is the
same as specifying
`[set, protected, {keypos,1}, {heir,none}, {write_concurrency,false}, {read_concurrency,false}, {decentralized_counters,false}]`.

- **`set`** - The table is a `set` table: one key, one object, no order among
  objects. This is the default table type.

- **`ordered_set`** - The table is a `ordered_set` table: one key, one object,
  ordered in Erlang term order, which is the order implied by the < and >
  operators. Tables of this type have a somewhat different behavior in some
  situations than tables of other types. Most notably, the `ordered_set` tables
  regard keys as equal when they _compare equal_, not only when they match. This
  means that to an `ordered_set` table, `t:integer/0` `1` and `t:float/0` `1.0`
  are regarded as equal. This also means that the key used to lookup an element
  does not necessarily _match_ the key in the returned elements, if
  `t:float/0`'s and `t:integer/0`'s are mixed in keys of a table.

- **`bag`** - The table is a `bag` table, which can have many objects, but only
  one instance of each object, per key.

- **`duplicate_bag`** - The table is a `duplicate_bag` table, which can have
  many objects, including multiple copies of the same object, per key.

- **`public`** - Any process can read or write to the table.

  [](){: #protected }

- **`protected`** - The owner process can read and write to the table. Other
  processes can only read the table. This is the default setting for the access
  rights.

  [](){: #private }

- **`private`** - Only the owner process can read or write to the table.

- **`named_table`** - If this option is present, the table is registered under
  its `Name` which can then be used instead of the table identifier in
  subsequent operations.

  The function will also return the `Name` instead of the table identifier. To
  get the table identifier of a named table, use `whereis/1`.

- **`{keypos,Pos}`** - Specifies which element in the stored tuples to use as
  key. By default, it is the first element, that is, `Pos=1`. However, this is
  not always appropriate. In particular, we do not want the first element to be
  the key if we want to store Erlang records in a table.

  Notice that any tuple stored in the table must have at least `Pos` number of
  elements.

  [](){: #heir }

- **`{heir,Pid,HeirData}  | {heir,Pid} | {heir,none}`** - Set a process as heir.
  The heir inherits the table if the owner terminates. If `HeirData` is given, a
  message `{'ETS-TRANSFER',tid(),FromPid,HeirData}` is sent to the heir when
  that occurs. If `{heir,Pid}` is given, no `'ETS-TRANSFER'` message is
  sent. The user must then make sure the heir gets notified some other way
  (through a link or monitor for example) to avoid the table being left unnoticed
  by its new owner.

  The heir must be a local process. Default heir is `none`, which
  destroys the table when the owner terminates.

  [](){: #new_2_write_concurrency }

- **`{write_concurrency,WriteConcurrencyAlternative}`** - Performance tuning.
  Defaults to `false`, in which case an operation that mutates (writes to) the
  table obtains exclusive access, blocking any concurrent access of the same
  table until finished. If set to `true`, the table is optimized for concurrent
  write access. Different objects of the same table can be mutated (and read) by
  concurrent processes. This is achieved to some degree at the expense of memory
  consumption and the performance of sequential access and concurrent reading.

  The `auto` alternative for the `write_concurrency` option is similar to the
  `true` option but automatically adjusts the synchronization granularity during
  runtime depending on how the table is used. This is the recommended
  `write_concurrency` option when using Erlang/OTP 25 and above as it performs
  well in most scenarios.

  The `write_concurrency` option can be combined with the options
  [`read_concurrency`](`m:ets#new_2_read_concurrency`) and
  [`decentralized_counters`](`m:ets#new_2_decentralized_counters`). You
  typically want to combine `write_concurrency` with `read_concurrency` when
  large concurrent read bursts and large concurrent write bursts are common; for
  more information, see option
  [`read_concurrency`](`m:ets#new_2_read_concurrency`). It is almost always a
  good idea to combine the `write_concurrency` option with the
  [`decentralized_counters`](`m:ets#new_2_decentralized_counters`) option.

  Notice that this option does not change any guarantees about
  [atomicity and isolation](`m:ets#module-concurrency`). Functions that makes such
  promises over many objects (like `insert/2`) gain less (or nothing) from this
  option.

  The memory consumption inflicted by both `write_concurrency` and
  `read_concurrency` is a constant overhead per table for `set`, `bag` and
  `duplicate_bag` when the `true` alternative for the `write_concurrency` option
  is not used. For all tables with the `auto` alternative and `ordered_set`
  tables with `true` alternative the memory overhead depends on the amount of
  actual detected concurrency during runtime. The memory overhead can be
  especially large when both `write_concurrency` and `read_concurrency` are
  combined.

  > #### Note {: .info }
  >
  > Prior to stdlib-3.7 (OTP-22.0) `write_concurrency` had no effect on
  > `ordered_set`.

  > #### Note {: .info }
  >
  > The `auto` alternative for the `write_concurrency` option is only available
  > in OTP-25.0 and above.

  [](){: #new_2_read_concurrency }

- **`{read_concurrency,boolean()}`**(Since OTP R14B)  
  Performance tuning. Defaults to `false`. When set to `true`, the table is
  optimized for concurrent read operations. When this option is enabled read
  operations become much cheaper; especially on systems with multiple physical
  processors. However, switching between read and write operations becomes more
  expensive.

  You typically want to enable this option when concurrent read operations are
  much more frequent than write operations, or when concurrent reads and writes
  comes in large read and write bursts (that is, many reads not interrupted by
  writes, and many writes not interrupted by reads).

  You typically do _not_ want to enable this option when the common access
  pattern is a few read operations interleaved with a few write operations
  repeatedly. In this case, you would get a performance degradation by enabling
  this option.

  Option `read_concurrency` can be combined with option
  [`write_concurrency`](`m:ets#new_2_write_concurrency`). You typically want to
  combine these when large concurrent read bursts and large concurrent write
  bursts are common.

  [](){: #new_2_decentralized_counters }

- **`{decentralized_counters,boolean()}`**(Since OTP 23.0)  
  Performance tuning. Defaults to `true` for all tables with the
  `write_concurrency` option set to `auto`. For tables of type `ordered_set` the
  option also defaults to true when the `write_concurrency` option is set to
  `true`. The option defaults to `false` for all other configurations. This
  option has no effect if the `write_concurrency` option is set to `false`.

  When this option is set to `true`, the table is optimized for frequent
  concurrent calls to operations that modify the tables size and/or its memory
  consumption (e.g., `insert/2` and `delete/2`). The drawback is that calls to
  `info/1` and `info/2` with `size` or `memory` as the second argument can get
  much slower when the `decentralized_counters` option is turned on.

  When this option is enabled the counters for the table size and memory
  consumption are distributed over several cache lines and the scheduling
  threads are mapped to one of those cache lines. The `erl` option
  [`+dcg`](`e:erts:erl_cmd.md#%2Bdcg`) can be used to control the number of
  cache lines that the counters are distributed over.

  [](){: #new_2_compressed }

- **`compressed`**(Since OTP R14B01)  
  If this option is present, the table data is stored in a more compact format
  to consume less memory. However, it will make table operations slower.
  Especially operations that need to inspect entire objects, such as `match` and
  `select`, get much slower. The key element is not compressed.
""".
-spec new(Name, Options) -> table() when
      Name :: atom(),
      Options :: [Option],
      Option :: Type | Access | named_table | {keypos,Pos}
              | {heir, Pid} | {heir, Pid, HeirData} | {heir, none}
              | Tweaks,
      Type :: table_type(),
      Access :: table_access(),
      WriteConcurrencyAlternative :: boolean() | auto,
      Tweaks :: {write_concurrency, WriteConcurrencyAlternative}
              | {read_concurrency, boolean()}
              | {decentralized_counters, boolean()}
              | compressed,
      Pos :: pos_integer(),
      Pid :: pid(),
      HeirData :: term().

new(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns the next key `Key2`, following key `Key1` in table `Table`. For table
type `ordered_set`, the next key in Erlang term order is returned. For other
table types, the next key according to the internal order of the table is
returned. If no next key exists, `'$end_of_table'` is returned.

To find the first key in the table, use `first/1`.

Unless a table of type `set`, `bag`, or `duplicate_bag` is fixated using
`safe_fixtable/2`, a call to [`next/2`](`next/2`) will fail if `Key1` no longer
exists in the table. For table type `ordered_set`, the function always returns
the next key after `Key1` in term order, regardless whether `Key1` ever existed
in the table.
""".
-spec next(Table, Key1) -> Key2 | '$end_of_table' when
      Table :: table(),
      Key1 :: term(),
      Key2 :: term().

next(_, _) ->
    erlang:nif_error(undef).

-doc """
Similar to `next/2` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `next/2` followed by a
`lookup/2`. If no next key exists, `'$end_of_table'` is returned.

It can be interleaved with `next/2` during traversal.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec next_lookup(Table, Key1) -> {Key2, [Object]} | '$end_of_table' when
    Table :: table(),
    Key1 :: term(),
    Key2 :: term(),
    Object :: tuple().

next_lookup(_, _) ->
  erlang:nif_error(undef).

-doc """
Returns the previous key `Key2`, preceding key `Key1` according to Erlang term
order in table `Table` of type `ordered_set`. For other table types, the
function is synonymous to `next/2`. If no previous key exists, `'$end_of_table'`
is returned.

To find the last key in an `ordered_set` table, use `last/1`.
""".
-spec prev(Table, Key1) -> Key2 | '$end_of_table' when
      Table :: table(),
      Key1 :: term(),
      Key2 :: term().

prev(_, _) ->
    erlang:nif_error(undef).

-doc """
Similar to `prev/2` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `prev/2` followed by a
`lookup/2`. If no previous key exists, `'$end_of_table'` is returned.

It can be interleaved with `prev/2` during traversal.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec prev_lookup(Table, Key1) -> {Key2, [Object]} | '$end_of_table' when
    Table :: table(),
    Key1 :: term(),
    Key2 :: term(),
    Object :: tuple().

prev_lookup(_, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: ets:rename/2
-doc """
Renames the named table `Table` to the new name `Name`. Afterwards, the old name
cannot be used to access the table. Renaming an unnamed table has no effect.
""".
-spec rename(Table, Name) -> Name when
      Table :: table(),
      Name :: atom().

rename(_, _) ->
    erlang:nif_error(undef).

-doc """
Fixes a table of type `set`, `bag`, or `duplicate_bag` for
[safe traversal](`m:ets#traversal`) using `first/1` & `next/2`, `match/3` &
`match/1`, `match_object/3` & `match_object/1`, or `select/3` & `select/1`.

A process fixes a table by calling
[`safe_fixtable(Table, true)`](`safe_fixtable/2`). The table remains fixed until
the process releases it by calling
[`safe_fixtable(Table, false)`](`safe_fixtable/2`), or until the process
terminates.

If many processes fix a table, the table remains fixed until all processes have
released it (or terminated). A reference counter is kept on a per process basis,
and N consecutive fixes requires N releases to release the table.

When a table is fixed, a sequence of `first/1` and `next/2` calls are guaranteed
to succeed even if keys are removed during the traversal. The keys for objects
inserted or deleted during a traversal may or may not be returned by
[`next/2`](`next/2`) depending on the ordering of keys within the table and if
the key exists at the time [`next/2`](`next/2`) is called.

_Example:_

```erlang
clean_all_with_value(Table,X) ->
    safe_fixtable(Table,true),
    clean_all_with_value(Table,X,ets:first(Table)),
    safe_fixtable(Table,false).

clean_all_with_value(Table,X,'$end_of_table') ->
    true;
clean_all_with_value(Table,X,Key) ->
    case ets:lookup(Table,Key) of
        [{Key,X}] ->
            ets:delete(Table,Key);
        _ ->
            true
    end,
    clean_all_with_value(Table,X,ets:next(Table,Key)).
```

Notice that deleted objects are not freed from a fixed table until it has been
released. If a process fixes a table but never releases it, the memory used by
the deleted objects is never freed. The performance of operations on the table
also degrades significantly.

To retrieve information about which processes have fixed which tables, use
[`info(Table, safe_fixed_monotonic_time)`](`m:ets#info_2_safe_fixed_monotonic_time`).
A system with many processes fixing tables can need a monitor that sends alarms
when tables have been fixed for too long.

Notice that [`safe_fixtable/2`](`safe_fixtable/2`) is not necessary for table
type `ordered_set` and for traversals done by a single ETS function call, like
`select/2`.
""".
-spec safe_fixtable(Table, Fix) -> true when
      Table :: table(),
      Fix :: boolean().

safe_fixtable(_, _) ->
    erlang:nif_error(undef).

-doc """
Matches the objects in table `Table` using a
[match specification](`m:ets#match_spec`). This is a more general call than
`match/2` and `match_object/2` calls. In its simplest form, the match
specification is as follows:

```text
MatchSpec = [MatchFunction]
MatchFunction = {MatchHead, [Guard], [Result]}
MatchHead = "Pattern as in ets:match"
Guard = {"Guardtest name", ...}
Result = "Term construct"
```

This means that the match specification is always a list of one or more tuples
(of arity 3). The first element of the tuple is to be a pattern as described in
`match/2`. The second element of the tuple is to be a list of 0 or more guard
tests (described below). The third element of the tuple is to be a list
containing a description of the value to return. In almost all normal cases, the
list contains exactly one term that fully describes the value to return for each
object.

The return value is constructed using the "match variables" bound in `MatchHead`
or using the special match variables `'$_'` (the whole matching object) and
`'$$'` (all match variables in a list), so that the following
[`match/2`](`match/2`) expression:

```erlang
ets:match(Table,{'$1','$2','$3'})
```

is exactly equivalent to:

```erlang
ets:select(Table,[{{'$1','$2','$3'},[],['$$']}])
```

And that the following [`match_object/2`](`match_object/2`) call:

```erlang
ets:match_object(Table,{'$1','$2','$1'})
```

is exactly equivalent to

```erlang
ets:select(Table,[{{'$1','$2','$1'},[],['$_']}])
```

Composite terms can be constructed in the `Result` part either by simply writing
a list, so that the following code:

```erlang
ets:select(Table,[{{'$1','$2','$3'},[],['$$']}])
```

gives the same output as:

```erlang
ets:select(Table,[{{'$1','$2','$3'},[],[['$1','$2','$3']]}])
```

That is, all the bound variables in the match head as a list. If tuples are to
be constructed, one has to write a tuple of arity 1 where the single element in
the tuple is the tuple one wants to construct (as an ordinary tuple can be
mistaken for a `Guard`).

Therefore the following call:

```erlang
ets:select(Table,[{{'$1','$2','$1'},[],['$_']}])
```

gives the same output as:

```erlang
ets:select(Table,[{{'$1','$2','$1'},[],[{{'$1','$2','$3'}}]}])
```

This syntax is equivalent to the syntax used in the trace patterns (see the
`m:dbg`) module in Runtime_Tools.

The `Guard`s are constructed as tuples, where the first element is the test name
and the remaining elements are the test parameters. To check for a specific type
(say a list) of the element bound to the match variable `'$1'`, one would write
the test as `{is_list, '$1'}`. If the test fails, the object in the table does
not match and the next `MatchFunction` (if any) is tried. Most guard tests
present in Erlang can be used, but only the new versions prefixed `is_` are
allowed (`is_float`, `is_atom`, and so on).

The `Guard` section can also contain logic and arithmetic operations, which are
written with the same syntax as the guard tests (prefix notation), so that the
following guard test written in Erlang:

```erlang
is_integer(X), is_integer(Y), X + Y < 4711
```

is expressed as follows (`X` replaced with `'$1'` and `Y` with `'$2'`):

```erlang
[{is_integer, '$1'}, {is_integer, '$2'}, {'<', {'+', '$1', '$2'}, 4711}]
```

For tables of type `ordered_set`, objects are visited in the same order as in a
`first`/`next` traversal. This means that the match specification is executed
against objects with keys in the `first`/`next` order and the corresponding
result list is in the order of that execution.
""".
-spec select(Table, MatchSpec) -> [Match] when
      Table :: table(),
      MatchSpec :: match_spec(),
      Match :: term().

select(_, _) ->
    erlang:nif_error(undef).

-doc """
Works like `select/2`, but only returns a limited (`Limit`) number of matching
objects. Term `Continuation` can then be used in subsequent calls to `select/1`
to get the next chunk of matching objects. This is a space-efficient way to work
on objects in a table, which is still faster than traversing the table object by
object using `first/1` and `next/2`.

If the table is empty, `'$end_of_table'` is returned.

Use `safe_fixtable/2` to guarantee [safe traversal](`m:ets#traversal`) for
subsequent calls to `select/1`.
""".
-spec select(Table, MatchSpec, Limit) -> {[Match],Continuation} |
                                       '$end_of_table' when
      Table :: table(),
      MatchSpec :: match_spec(),
      Limit :: pos_integer(),
      Match :: term(),
      Continuation :: continuation().

select(_, _, _) ->
    erlang:nif_error(undef).

-doc """
Continues a match started with `select/3`. The next chunk of the size specified
in the initial [`select/3`](`select/3`) call is returned together with a new
`Continuation`, which can be used in subsequent calls to this function.

When there are no more objects in the table, `'$end_of_table'` is returned.
""".
-spec select(Continuation) -> {[Match],Continuation} | '$end_of_table' when
      Match :: term(),
      Continuation :: continuation().

select(_) ->
    erlang:nif_error(undef).

-doc """
Matches the objects in table `Table` using a
[match specification](`m:ets#match_spec`). If and only if the match specification
returns `true` for an object, that object is considered a match and is counted.
For any other result from the match specification the object is not considered a
match and is therefore not counted.

The function returns the number of objects matched.
""".
-spec select_count(Table, MatchSpec) -> NumMatched when
      Table :: table(),
      MatchSpec :: match_spec(),
      NumMatched :: non_neg_integer().

select_count(_, _) ->
    erlang:nif_error(undef).

-doc """
Matches the objects in table `Table` using a
[match specification](`m:ets#match_spec`). If and only if the match
specification returns `true` for an object, that object is removed from the
table. For any other result from the match specification the object is
retained. This is a more general function than `match_delete/2`.

The function returns the number of objects deleted from the table.

> #### Note {: .info }
>
> The match specification has to return the atom `true` if the object is to be
> deleted. No other return value gets the object deleted. So one cannot use the
> same match specification for looking up elements as for deleting them.
""".
-spec select_delete(Table, MatchSpec) -> NumDeleted when
      Table :: table(),
      MatchSpec :: match_spec(),
      NumDeleted :: non_neg_integer().

select_delete(Table, [{'_',[],[true]}]) ->
    ets:internal_delete_all(Table, undefined);
select_delete(Table, MatchSpec) ->
    ets:internal_select_delete(Table, MatchSpec).

-doc false.
-spec internal_select_delete(Table, MatchSpec) -> NumDeleted when
      Table :: table(),
      MatchSpec :: match_spec(),
      NumDeleted :: non_neg_integer().

internal_select_delete(_, _) ->
    erlang:nif_error(undef).

-doc """
Matches the objects in the table `Table` using a
[match specification](`m:ets#match_spec`). For each matched object, the existing
object is replaced with the match specification result.

The match-and-replace operation for each individual object is guaranteed to be
[atomic and isolated](`m:ets#module-concurrency`). The `select_replace` table traversal
as a whole, like all other select functions, does not give such guarantees.

The match specification must be guaranteed to _retain the key_ of any matched
object. If not, `select_replace` will fail with `badarg` without updating any
objects.

For the moment, due to performance and semantic constraints, tables of type
`bag` are not yet supported.

The function returns the total number of replaced objects.

_Example_

For all 2-tuples with a list in second position, add atom `'marker'` first in
the list:

```erlang
1> T = ets:new(x,[]), ets:insert(T, {key, [1, 2, 3]}).
true
2> MS = ets:fun2ms(fun({K, L}) when is_list(L) -> {K, [marker | L]} end).
[{{'$1','$2'},[{is_list,'$2'}],[{{'$1',[marker|'$2']}}]}]
3> ets:select_replace(T, MS).
1
4> ets:tab2list(T).
[{key,[marker,1,2,3]}]
```

A generic single object compare-and-swap operation:

```erlang
[Old] = ets:lookup(T, Key),
New = update_object(Old),
Success = (1 =:= ets:select_replace(T, [{Old, [], [{const, New}]}])),
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec select_replace(Table, MatchSpec) -> NumReplaced when
      Table :: table(),
      MatchSpec :: match_spec(),
      NumReplaced :: non_neg_integer().

select_replace(_, _) ->
    erlang:nif_error(undef).

-doc """
Works like `select/2`, but returns the list in reverse order for table type
`ordered_set`. For all other table types, the return value is identical to that
of [`select/2`](`select/2`).
""".
-doc(#{since => <<"OTP R14B">>}).
-spec select_reverse(Table, MatchSpec) -> [Match] when
      Table :: table(),
      MatchSpec :: match_spec(),
      Match :: term().

select_reverse(_, _) ->
    erlang:nif_error(undef).

-doc """
Works like `select/3`, but for table type `ordered_set` traversing is done
starting at the last object in Erlang term order and moves to the first. For all
other table types, the return value is identical to that of
[`select/3`](`select/3`).

Notice that this is _not_ equivalent to reversing the result list of a
[`select/3`](`select/3`) call, as the result list is not only reversed, but also
contains the last `Limit` matching objects in the table, not the first.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec select_reverse(Table, MatchSpec, Limit) -> {[Match],Continuation} |
                                               '$end_of_table' when
      Table :: table(),
      MatchSpec :: match_spec(),
      Limit :: pos_integer(),
      Match :: term(),
      Continuation :: continuation().

select_reverse(_, _, _) ->
    erlang:nif_error(undef).

-doc """
Continues a match started with `select_reverse/3`. For tables of type
`ordered_set`, the traversal of the table continues to objects with keys earlier
in the Erlang term order. The returned list also contains objects with keys in
reverse order. For all other table types, the behavior is exactly that of
`select/1`.

_Example:_

```erlang
1> T = ets:new(x,[ordered_set]).
2> [ ets:insert(T,{N}) || N <- lists:seq(1,10) ].
...
3> {R0,C0} = ets:select_reverse(T,[{'_',[],['$_']}],4).
...
4> R0.
[{10},{9},{8},{7}]
5> {R1,C1} = ets:select_reverse(C0).
...
6> R1.
[{6},{5},{4},{3}]
7> {R2,C2} = ets:select_reverse(C1).
...
8> R2.
[{2},{1}]
9> '$end_of_table' = ets:select_reverse(C2).
...
```
""".
-doc(#{since => <<"OTP R14B">>}).
-spec select_reverse(Continuation) -> {[Match],Continuation} |
                                      '$end_of_table' when
      Continuation :: continuation(),
      Match :: term().

select_reverse(_) ->
    erlang:nif_error(undef).

-doc """
Sets table options. The only allowed option to be set after the table has been
created is [`heir`](`m:ets#heir`). The calling process must be the table owner.
""".
-spec setopts(Table, Opts) -> true when
      Table :: table(),
      Opts :: Opt | [Opt],
      Opt :: {heir, Pid} | {heir, Pid, HeirData} | {heir,none},
      Pid :: pid(),
      HeirData :: term().

setopts(_, _) ->
    erlang:nif_error(undef).

-doc """
This function is mostly for debugging purposes, normally `first`/`next` or
`last`/`prev` are to be used instead.

Returns all objects in slot `I` of table `Table`. A table can be traversed by
repeatedly calling the function, starting with the first slot `I=0` and ending
when `'$end_of_table'` is returned. If argument `I` is out of range, the
function fails with reason `badarg`.

Unless a table of type `set`, `bag`, or `duplicate_bag` is protected using
`safe_fixtable/2`, a traversal can fail if concurrent updates are made to the
table. For table type `ordered_set`, the function returns a list containing
object `I` in Erlang term order.
""".
-spec slot(Table, I) -> [Object] | '$end_of_table' when
      Table :: table(),
      I :: non_neg_integer(),
      Object :: tuple().

slot(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns and removes a list of all objects with key `Key` in table `Table`.

The specified `Key` is used to identify the object by either _comparing equal_
the key of an object in an `ordered_set` table, or _matching_ in other types of
tables (for details on the difference, see `lookup/2` and `new/2`).
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec take(Table, Key) -> [Object] when
      Table :: table(),
      Key :: term(),
      Object :: tuple().

take(_, _) ->
    erlang:nif_error(undef).

-doc(#{equiv => update_counter/4}).
-spec update_counter(Table, Key, UpdateOp | [UpdateOp] | Incr) -> Result | [Result] when
      Table :: table(),
      Key :: term(),
      UpdateOp :: {Pos, Incr} | {Pos, Incr, Threshold, SetValue},
      Pos :: integer(),
      Incr :: integer(),
      Threshold :: integer(),
      SetValue :: integer(),
      Result :: integer().

update_counter(_, _, _) ->
    erlang:nif_error(undef).

-doc """
This function provides an efficient way to update one or more counters, without
the trouble of having to look up an object, update the object by incrementing an
element, and insert the resulting object into the table again. The operation is
guaranteed to be [atomic and isolated](`m:ets#module-concurrency`).

This function destructively updates the object with key `Key` in table `Table`
by adding `Incr` to the element at position `Pos`. The new counter value is
returned. If no position is specified, the element directly following key
(`<keypos>+1`) is updated.

If a `Threshold` is specified, the counter is reset to value `SetValue` if the
following conditions occur:

- `Incr` is not negative (`>= 0`) and the result would be greater than (`>`)
  `Threshold`.
- `Incr` is negative (`< 0`) and the result would be less than (`<`)
  `Threshold`.

A list of `UpdateOp` can be supplied to do many update operations within the
object. The operations are carried out in the order specified in the list. If
the same counter position occurs more than once in the list, the corresponding
counter is thus updated many times, each time based on the previous result. The
return value is a list of the new counter values from each update operation in
the same order as in the operation list. If an empty list is specified, nothing
is updated and an empty list is returned. If the function fails, no updates are
done.

The specified `Key` is used to identify the object by either _matching_ the key
of an object in a `set` table, or _compare equal_ to the key of an object in an
`ordered_set` table (for details on the difference, see `lookup/2` and `new/2`).

If a default object `Default` is specified, it is used as the object to be
updated if the key is missing from the table. The value in place of the key is
ignored and replaced by the proper key value. The return value is as if the
default object had not been used, that is, a single updated element or a list of
them.

The function fails with reason `badarg` in the following situations:

- The table type is not `set` or `ordered_set`.
- No object with the correct key exists and no default object was supplied.
- The object has the wrong arity.
- The default object arity is smaller than `<keypos>`.
- Any field from the default object that is updated is not an integer.
- The element to update is not an integer.
- The element to update is also the key.
- Any of `Pos`, `Incr`, `Threshold`, or `SetValue` is not an integer.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec update_counter(Table, Key, UpdateOp | Incr | [UpdateOp], Default) -> Result | [Result] when
      Table :: table(),
      Key :: term(),
      UpdateOp :: {Pos, Incr}
                | {Pos, Incr, Threshold, SetValue},
      Pos :: integer(),
      Incr :: integer(),
      Threshold :: integer(),
      SetValue :: integer(),
      Result :: integer(),
      Default :: tuple().

update_counter(_, _, _, _) ->
    erlang:nif_error(undef).

-doc(#{equiv => update_element/4}).
-spec update_element(Table, Key, ElementSpec) -> boolean() when
      Table :: table(),
      Key :: term(),
      ElementSpec :: {Pos, Value} | [{Pos, Value}],
      Pos :: pos_integer(),
      Value :: term().

update_element(_, _, _) ->
    erlang:nif_error(undef).

-doc """
This function provides an efficient way to update one or more elements within an
object, without the trouble of having to look up, update, and write back the
entire object.

This function destructively updates the object with key `Key` in table `Table`.
The element at position `Pos` is given the value `Value`.

A list of `{Pos,Value}` can be supplied to update many elements within the same
object. If the same position occurs more than once in the list, the last value
in the list is written. If the list is empty or the function fails, no updates
are done. The function is also atomic in the sense that other processes can
never see any intermediate results.

Returns `true` if an object with key `Key` is found, otherwise `false`.

The specified `Key` is used to identify the object by either _matching_ the key
of an object in a `set` table, or _compare equal_ to the key of an object in an
`ordered_set` table (for details on the difference, see `lookup/2` and `new/2`).

If a default object `Default` is specified, it is used as the object to be
updated if the key is missing from the table. The value in place of the key is
ignored and replaced by the proper key value.

The function fails with reason `badarg` in the following situations:

- The table type is not `set` or `ordered_set`.
- `Pos` < 1.
- `Pos` > object arity.
- The default object arity is smaller than `<keypos>`.
- The element to update is also the key.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec update_element(Table, Key, ElementSpec, Default) -> boolean() when
      Table :: table(),
      Key :: term(),
      ElementSpec :: {Pos, Value} | [{Pos, Value}],
      Pos :: pos_integer(),
      Value :: term(),
      Default :: tuple().

update_element(_, _, _, _) ->
    erlang:nif_error(undef).

-doc """
This function returns the `t:tid/0` of the named table identified by
`TableName`, or `undefined` if no such table exists. The `t:tid/0` can be used
in place of the table name in all operations, which is slightly faster since the
name does not have to be resolved on each call.

If the table is deleted, the `t:tid/0` will be invalid even if another named
table is created with the same name.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec whereis(TableName) -> tid() | undefined when
    TableName :: atom().
whereis(_) ->
    erlang:nif_error(undef).

%%% End of BIFs

-doc "A compiled match specification.".
-opaque compiled_match_spec() :: reference().
-type comp_match_spec() :: compiled_match_spec().

-doc """
Executes the matching specified in a compiled
[match specification](`m:ets#match_spec`) on a list of terms. Term
`CompiledMatchSpec` is to be the result of a call to `match_spec_compile/1` and
is hence the internal representation of the match specification one wants to
use.

The matching is executed on each element in `List` and the function returns a
list containing all results. If an element in `List` does not match, nothing is
returned for that element. The length of the result list is therefore equal or
less than the length of parameter `List`.

_Example:_

The following two calls give the same result (but certainly not the same
execution time):

```erlang
Table = ets:new...
MatchSpec = ...
% The following call...
ets:match_spec_run(ets:tab2list(Table),
                   ets:match_spec_compile(MatchSpec)),
% ...gives the same result as the more common (and more efficient)
ets:select(Table, MatchSpec),
```

> #### Note {: .info }
>
> This function has limited use in normal code. It is used by the `m:dets`
> module to perform the `dets:select/1` operations and by Mnesia during
> transactions.
""".
-spec match_spec_run(List, CompiledMatchSpec) -> list() when
      List :: [term()],
      CompiledMatchSpec :: compiled_match_spec().

match_spec_run(List, CompiledMS) ->
    lists:reverse(ets:match_spec_run_r(List, CompiledMS, [])).

-doc """
Restores an opaque continuation returned by `select/3` or `select/1` if the
continuation has passed through external term format (been sent between nodes or
stored on disk).

The reason for this function is that continuation terms contain compiled match
specifications and may therefore be invalidated if converted to external term
format. Given that the original match specification is kept intact, the
continuation can be restored, meaning it can once again be used in subsequent
[`select/1`](`select/1`) calls even though it has been stored on disk or on
another node.

_Examples:_

The following sequence of calls may fail:

```erlang
T=ets:new(x,[]),
...
MS = ets:fun2ms(fun({N,_}=A) when (N rem 10) =:= 0 -> A end),
{_,C} = ets:select(T, MS, 10),
MaybeBroken = binary_to_term(term_to_binary(C)),
ets:select(MaybeBroken).
```

The following sequence works, as the call to
[`repair_continuation/2`](`repair_continuation/2`) reestablishes the
`MaybeBroken` continuation.

```erlang
T=ets:new(x,[]),
...
MS = ets:fun2ms(fun({N,_}=A) when (N rem 10) =:= 0 -> A end),
{_,C} = ets:select(T,MS,10),
MaybeBroken = binary_to_term(term_to_binary(C)),
ets:select(ets:repair_continuation(MaybeBroken,MS)).
```

> #### Note {: .info }
>
> This function is rarely needed in application code. It is used by Mnesia to
> provide distributed [`select/3`](`select/3`) and [`select/1`](`select/1`)
> sequences. A normal application would either use Mnesia or keep the
> continuation from being converted to external format.
>
> The actual behavior of compiled match specifications when recreated from
> external format has changed and may change in future releases, but this
> interface remains for backward compatibility. See `is_compiled_ms/1`.
""".
-spec repair_continuation(Continuation, MatchSpec) -> Continuation when
      Continuation :: continuation(),
      MatchSpec :: match_spec().

%% $end_of_table is an allowed continuation in ets...
repair_continuation('$end_of_table', _) ->
    '$end_of_table';
%% ordered_set
repair_continuation(Untouched = {Table,Lastkey,EndCondition,N2,MSRef,L2,N3,N4}, MS)
  when %% (is_atom(Table) or is_integer(Table)),
       is_integer(N2),
      %% is_reference(MSRef),
       is_list(L2),
       is_integer(N3),
       is_integer(N4) ->
    case ets:is_compiled_ms(MSRef) of
	true ->
	    Untouched;
	false ->
	    {Table,Lastkey,EndCondition,N2,ets:match_spec_compile(MS),L2,N3,N4}
    end;
%% set/bag/duplicate_bag
repair_continuation(Untouched = {Table,N1,N2,MSRef,L,N3}, MS)
  when %% (is_atom(Table) or is_integer(Table)),
       is_integer(N1),
       is_integer(N2),
      %% is_reference(MSRef),
       is_list(L),
       is_integer(N3) ->
    case ets:is_compiled_ms(MSRef) of
	true ->
	    Untouched;
	false ->
	    {Table,N1,N2,ets:match_spec_compile(MS),L,N3}
    end.

-doc """
Pseudo function that by a `parse_transform` translates `LiteralFun` typed as
parameter in the function call to a [match specification](`m:ets#match_spec`).
With "literal" is meant that the fun must textually be written as the parameter
of the function, it cannot be held in a variable that in turn is passed to the
function.

The parse transform is provided in the `ms_transform` module and the source
_must_ include file `ms_transform.hrl` in STDLIB for this pseudo function to
work. Failing to include the hrl file in the source results in a runtime error,
not a compile time error. The include file is easiest included by adding line
`-include_lib("stdlib/include/ms_transform.hrl").` to the source file.

The fun is very restricted, it can take only a single parameter (the object to
match): a sole variable or a tuple. It must use the `is_` guard tests. Language
constructs that have no representation in a match specification (`if`, `case`,
`receive`, and so on) are not allowed.

The return value is the resulting match specification.

_Example:_

```erlang
1> ets:fun2ms(fun({M,N}) when N > 3 -> M end).
[{{'$1','$2'},[{'>','$2',3}],['$1']}]
```

Variables from the environment can be imported, so that the following works:

```erlang
2> X=3.
3
3> ets:fun2ms(fun({M,N}) when N > X -> M end).
[{{'$1','$2'},[{'>','$2',{const,3}}],['$1']}]
```

The imported variables are replaced by match specification `const` expressions,
which is consistent with the static scoping for Erlang funs. However, local or
global function calls cannot be in the guard or body of the fun. Calls to
built-in match specification functions is of course allowed:

```erlang
4> ets:fun2ms(fun({M,N}) when N > X, my_fun(M) -> M end).
Error: fun containing local Erlang function calls
('my_fun' called in guard) cannot be translated into match_spec
{error,transform_error}
5> ets:fun2ms(fun({M,N}) when N > X, is_atom(M) -> M end).
[{{'$1','$2'},[{'>','$2',{const,3}},{is_atom,'$1'}],['$1']}]
```

As shown by the example, the function can be called from the shell also. The fun
must be literally in the call when used from the shell as well.

> #### Warning {: .warning }
>
> If the `parse_transform` is not applied to a module that calls this pseudo
> function, the call fails in runtime (with a `badarg`). The `ets` module
> exports a function with this name, but it is never to be called except when
> using the function in the shell. If the `parse_transform` is properly applied
> by including header file `ms_transform.hrl`, compiled code never calls the
> function, but the function call is replaced by a literal match specification.

For more information, see [`ms_transform`](`m:ms_transform`).
""".
-spec fun2ms(LiteralFun) -> MatchSpec when
      LiteralFun :: function(),
      MatchSpec :: match_spec().

fun2ms(ShellFun) when is_function(ShellFun) ->
    %% Check that this is really a shell fun...
    case erl_eval:fun_data(ShellFun) of
        {fun_data,ImportList,Clauses} ->
            case ms_transform:transform_from_shell(
                   ?MODULE,Clauses,ImportList) of
                {error,[_|_],_} ->
                    {error,transform_error};
                Else ->
                    Else
            end;
        _ ->
            exit({badarg,{?MODULE,fun2ms,
                          [function,called,with,real,'fun',
                           should,be,transformed,with,
                           parse_transform,'or',called,with,
                           a,'fun',generated,in,the,
                           shell]}})
    end.

-doc """
`Acc0` is returned if the table is empty. This function is similar to
`lists:foldl/3`. The table elements are traversed in an unspecified order,
except for `ordered_set` tables, where they are traversed first to last.

If `Function` inserts objects into the table, or another process inserts objects
into the table, those objects _can_ (depending on key ordering) be included in
the traversal.
""".
-spec foldl(Function, Acc0, Table) -> Acc1 when
      Function :: fun((Element :: term(), AccIn) -> AccOut),
      Table :: table(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

foldl(F, Accu, Tab) ->
    T = soft_whereis(Tab),
    ets:safe_fixtable(T, true),
    First = ets:first_lookup(T),
    try
        do_foldl(F, Accu, First, T)
    after
	ets:safe_fixtable(T, false)
    end.

do_foldl(_F, Accu, '$end_of_table', _T) -> Accu;
do_foldl(F, Accu0, {Key, Objects}, T) ->
    Accu = lists:foldl(F, Accu0, Objects),
    do_foldl(F, Accu, ets:next_lookup(T, Key), T).

-doc """
`Acc0` is returned if the table is empty. This function is similar to
`lists:foldr/3`. The table elements are traversed in an unspecified order,
except for `ordered_set` tables, where they are traversed last to first.

If `Function` inserts objects into the table, or another process inserts objects
into the table, those objects _can_ (depending on key ordering) be included in
the traversal.
""".
-spec foldr(Function, Acc0, Table) -> Acc1 when
      Function :: fun((Element :: term(), AccIn) -> AccOut),
      Table :: table(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

foldr(F, Accu, Tab) ->
    T = soft_whereis(Tab),
    ets:safe_fixtable(T, true),
    Last = ets:last_lookup(T),
    try
        do_foldr(F, Accu, Last, T)
    after
        ets:safe_fixtable(T, false)
    end.

do_foldr(_F, Accu, '$end_of_table', _T) -> Accu;
do_foldr(F, Accu0, {Key, Objects}, T) ->
    Accu = lists:foldr(F, Accu0, Objects),
    do_foldr(F, Accu, ets:prev_lookup(T, Key), T).

soft_whereis(Table) when is_atom(Table) ->
    case ets:whereis(Table) of
        undefined -> error(badarg, [Table], [{error_info, #{cause => id, module => erl_stdlib_errors}}]);
        Ref -> Ref
    end;
soft_whereis(Table) -> Table.

-doc """
Fills an already created ETS table with the objects in the already opened Dets
table `DetsTab`. Existing objects in the ETS table are kept unless overwritten.

If any of the tables does not exist or the Dets table is not open, a `badarg`
exception is raised.
""".
-spec from_dets(Table, DetsTab) -> 'true' when
      Table :: table(),
      DetsTab :: dets:tab_name().

from_dets(EtsTable, DetsTable) ->
    case (catch dets:to_ets(DetsTable, EtsTable)) of
	{error, Reason} ->
	    erlang:error(Reason, [EtsTable,DetsTable]);
	{'EXIT', {Reason1, _Stack1}} ->
	    erlang:error(Reason1,[EtsTable,DetsTable]);
	{'EXIT', EReason} ->
	    erlang:error(EReason,[EtsTable,DetsTable]);
	EtsTable ->
	    true;
	Unexpected -> %% Dets bug?
	    erlang:error(Unexpected,[EtsTable,DetsTable])
    end.

-doc """
Fills an already created/opened Dets table with the objects in the already
opened ETS table named `Table`. The Dets table is emptied before the objects are
inserted.
""".
-spec to_dets(Table, DetsTab) -> DetsTab when
      Table :: table(),
      DetsTab :: dets:tab_name().

to_dets(EtsTable, DetsTable) ->
    case (catch dets:from_ets(DetsTable, EtsTable)) of
	{error, Reason} ->
	    erlang:error(Reason, [EtsTable,DetsTable]);
	{'EXIT', {Reason1, _Stack1}} ->
	    erlang:error(Reason1,[EtsTable,DetsTable]);
	{'EXIT', EReason} ->
	    erlang:error(EReason,[EtsTable,DetsTable]);
	ok ->
	    DetsTable;
	Unexpected -> %% Dets bug?
	    erlang:error(Unexpected,[EtsTable,DetsTable])
    end.

-doc """
This function is a utility to test a [match specification](`m:ets#match_spec`)
used in calls to `select/2`. The function both tests `MatchSpec` for "syntactic"
correctness and runs the match specification against object `Tuple`.

If the match specification is syntactically correct, the function either returns
`{ok,Result}`, where `Result` is what would have been the result in a real
[`select/2`](`select/2`) call, or `false` if the match specification does not
match object `Tuple`.

If the match specification contains errors, tuple `{error, Errors}` is returned,
where `Errors` is a list of natural language descriptions of what was wrong with
the match specification.

This is a useful debugging and test tool, especially when writing complicated
[`select/2`](`select/2`) calls.

See also: `erlang:match_spec_test/3`.
""".
-spec test_ms(Tuple, MatchSpec) -> {'ok', Result} | {'error', Errors} when
      Tuple :: tuple(),
      MatchSpec :: match_spec(),
      Result :: term(),
      Errors :: [{'warning'|'error', string()}].

test_ms(Term, MS) ->
    case erlang:match_spec_test(Term, MS, table) of
	{ok, Result, _Flags, _Messages} ->
	    {ok, Result};
	{error, _Errors} = Error ->
	    Error
    end.

-doc """
Replaces the existing objects of table `Table` with objects created by calling
the input function `InitFun`, see below. This function is provided for
compatibility with the `dets` module, it is not more efficient than filling a
table by using `insert/2`.

When called with argument `read`, the function `InitFun` is assumed to return
`end_of_input` when there is no more input, or `{Objects, Fun}`, where `Objects`
is a list of objects and `Fun` is a new input function. Any other value `Value`
is returned as an error `{error, {init_fun, Value}}`. Each input function is
called exactly once, and if an error occur, the last function is called with
argument `close`, the reply of which is ignored.

If the table type is `set` and more than one object exists with a given key, one
of the objects is chosen. This is not necessarily the last object with the given
key in the sequence of objects returned by the input functions. This holds also
for duplicated objects stored in tables of type `bag`.
""".
-spec init_table(Table, InitFun) -> 'true' when
      Table :: table(),
      InitFun :: fun((Arg) -> Res),
      Arg :: 'read' | 'close',
      Res :: 'end_of_input' | {Objects :: [term()], InitFun} | term().

init_table(Table, Fun) ->
    ets:delete_all_objects(Table),
    init_table_continue(Table, Fun(read)).

init_table_continue(_Table, end_of_input) ->
    true;
init_table_continue(Table, {List, Fun}) when is_list(List), is_function(Fun) ->
    case (catch init_table_sub(Table, List)) of
	{'EXIT', Reason} ->
	    (catch Fun(close)),
	    exit(Reason);
	true ->
	    init_table_continue(Table, Fun(read))
    end;
init_table_continue(_Table, Error) ->
    exit(Error).

init_table_sub(_Table, []) ->
    true;
init_table_sub(Table, [H|T]) ->
    ets:insert(Table, H),
    init_table_sub(Table, T).

-doc """
Deletes all objects that match pattern `Pattern` from table `Table`. For a
description of patterns, see `match/2`.
""".
-spec match_delete(Table, Pattern) -> 'true' when
      Table :: table(),
      Pattern :: match_pattern().

match_delete(Table, Pattern) ->
    ets:select_delete(Table, [{Pattern,[],[true]}]),
    true.

%% Produce a list of tuples from a table

-doc "Returns a list of all objects in table `Table`.".
-spec tab2list(Table) -> [Object] when
      Table :: table(),
      Object :: tuple().

tab2list(T) ->
    ets:match_object(T, '_').


%% Dump a table to a file using the disk_log facility

%% Options := [Option]
%% Option := {extended_info,[ExtInfo]}
%% ExtInfo := object_count | md5sum

-define(MAJOR_F2T_VERSION,1).
-define(MINOR_F2T_VERSION,0).

-record(filetab_options,
	{
	  object_count = false :: boolean(),
	  md5sum       = false :: boolean(),
	  sync         = false :: boolean()
	 }).

-doc """
Dumps table `Table` to file `Filename`.

Equivalent to [`tab2file(Table, Filename,[])`](`tab2file/3`)
""".
-spec tab2file(Table, Filename) -> 'ok' | {'error', Reason} when
      Table :: table(),
      Filename :: file:name(),
      Reason :: term().

tab2file(Table, File) ->
    tab2file(Table, File, []).

-doc """
Dumps table `Table` to file `Filename`.

When dumping the table, some information about the table is dumped to a header
at the beginning of the dump. This information contains data about the table
type, name, protection, size, version, and if it is a named table. It also
contains notes about what extended information is added to the file, which can
be a count of the objects in the file or a MD5 sum of the header and records in
the file.

The size field in the header might not correspond to the number of records in
the file if the table is public and records are added or removed from the table
during dumping. Public tables updated during dump, and that one wants to verify
when reading, needs at least one field of extended information for the read
verification process to be reliable later.

Option `extended_info` specifies what extra information is written to the table
dump:

- **`object_count`** - The number of objects written to the file is noted in the
  file footer, so file truncation can be verified even if the file was updated
  during dump.

- **`md5sum`** - The header and objects in the file are checksummed using the
  built-in MD5 functions. The MD5 sum of all objects is written in the file
  footer, so that verification while reading detects the slightest bitflip in
  the file data. Using this costs a fair amount of CPU time.

Whenever option `extended_info` is used, it results in a file not readable by
versions of ETS before that in STDLIB 1.15.1

If option `sync` is set to `true`, it ensures that the content of the file is
written to the disk before `tab2file` returns. Defaults to `{sync, false}`.
""".
-spec tab2file(Table, Filename, Options) -> 'ok' | {'error', Reason} when
      Table :: table(),
      Filename :: file:name(),
      Options :: [Option],
      Option :: {'extended_info', [ExtInfo]} | {'sync', boolean()},
      ExtInfo :: 'md5sum' | 'object_count',
      Reason :: term().

tab2file(Table, File, Options) ->
    try
	{ok, FtOptions} = parse_ft_options(Options),
	_ = file:delete(File),
	case file:read_file_info(File) of
	    {error, enoent} -> ok;
	    _ -> throw(eaccess)
	end,
	Name = make_ref(),
	case disk_log:open([{name, Name}, {file, File},
                            {repair, truncate}]) of
	    {ok, Name} ->
		ok;
	    {error, Reason} ->
		throw(Reason)
	end,
	try
	    Info0 = case ets:info(Table) of
		       undefined ->
			   %% erlang:error(badarg, [Table, File, Options]);
			   throw(badtab);
		       I ->
			   I
	    end,
	    Info = [list_to_tuple(Info0 ++ 
				  [{major_version,?MAJOR_F2T_VERSION},
				   {minor_version,?MINOR_F2T_VERSION},
				   {extended_info, 
				    ft_options_to_list(FtOptions)}])],
	    {LogFun, InitState} = 
	    case FtOptions#filetab_options.md5sum of
		true ->
		    {fun(Oldstate,Termlist) ->
			     {NewState,BinList} = 
				 md5terms(Oldstate,Termlist),
                             case disk_log:blog_terms(Name,BinList) of
                                 ok -> NewState;
                                 {error, Reason2} -> throw(Reason2)
                             end
		     end,
		     erlang:md5_init()};
		false ->
		    {fun(_,Termlist) ->
                             case disk_log:log_terms(Name,Termlist) of
                                 ok -> true;
                                 {error, Reason2} -> throw(Reason2)
                             end
		     end, 
		     true}
	    end,
	    ets:safe_fixtable(Table,true),
	    {NewState1,Num} = try
				  NewState = LogFun(InitState,Info),
				  dump_file(
				      ets:select(Table,[{'_',[],['$_']}],100),
				      LogFun, NewState, 0)
			      after
				  (catch ets:safe_fixtable(Table,false))
			      end,
	    EndInfo = 
	    case  FtOptions#filetab_options.object_count of
		true ->
		    [{count,Num}];
		false ->
		    []
	    end ++
	    case  FtOptions#filetab_options.md5sum of
		true ->
		    [{md5,erlang:md5_final(NewState1)}];
		false ->
		    []
	    end,
	    case EndInfo of
		[] ->
		    ok;
		List ->
		    LogFun(NewState1,[['$end_of_table',List]])
	    end,
	    case FtOptions#filetab_options.sync of
	        true ->
		    case disk_log:sync(Name) of
		        ok -> ok;
			{error, Reason2} -> throw(Reason2)
		    end;
                false ->
		    ok
            end,
	    disk_log:close(Name)
	catch
	    throw:TReason ->
		_ = disk_log:close(Name),
		_ = file:delete(File),
		throw(TReason);
	    exit:ExReason ->
		_ = disk_log:close(Name),
		_ = file:delete(File),
		exit(ExReason);
	    error:ErReason:StackTrace ->
		_ = disk_log:close(Name),
		_ = file:delete(File),
	        erlang:raise(error,ErReason,StackTrace)
	end
    catch
	throw:TReason2 ->
	    {error,TReason2};
	exit:ExReason2 ->
	    {error,ExReason2}
    end.

dump_file('$end_of_table', _LogFun, State, Num) ->
    {State,Num};
dump_file({Terms, Context}, LogFun, State, Num) ->
    Count = length(Terms),
    NewState = LogFun(State, Terms),
    dump_file(ets:select(Context), LogFun, NewState, Num + Count).

ft_options_to_list(#filetab_options{md5sum = MD5, object_count = PS}) ->
    case PS of
	true ->
	    [object_count]; 
	_ ->
	    []
    end ++
	case MD5 of
	    true ->
		[md5sum]; 
	    _ ->
		[]
	end.

md5terms(State, []) ->
    {State, []};
md5terms(State, [H|T]) ->
    B = term_to_binary(H),
    NewState = erlang:md5_update(State, B),
    {FinState, TL} = md5terms(NewState, T),
    {FinState, [B|TL]}.

parse_ft_options(Options) when is_list(Options) ->
    {ok, parse_ft_options(Options, #filetab_options{}, false)}.

parse_ft_options([], FtOpt, _) ->
    FtOpt;
parse_ft_options([{sync,true} | Rest], FtOpt, EI) ->
    parse_ft_options(Rest, FtOpt#filetab_options{sync = true}, EI);
parse_ft_options([{sync,false} | Rest], FtOpt, EI) ->
    parse_ft_options(Rest, FtOpt, EI);
parse_ft_options([{extended_info,L} | Rest], FtOpt0, false) ->
    FtOpt1 = parse_ft_info_options(FtOpt0, L),
    parse_ft_options(Rest, FtOpt1, true);
parse_ft_options([Other | _], _, _) ->
    throw({unknown_option, Other});
parse_ft_options(Malformed, _, _) ->
    throw({malformed_option, Malformed}).

parse_ft_info_options(FtOpt,[]) ->
    FtOpt;
parse_ft_info_options(FtOpt,[object_count | T]) ->
    parse_ft_info_options(FtOpt#filetab_options{object_count = true}, T);
parse_ft_info_options(FtOpt,[md5sum | T]) ->
    parse_ft_info_options(FtOpt#filetab_options{md5sum = true}, T);
parse_ft_info_options(_,[Unexpected | _]) ->
    throw({unknown_option,[{extended_info,[Unexpected]}]});
parse_ft_info_options(_,Malformed) ->
    throw({malformed_option,Malformed}).
		     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read a dumped file from disk and create a corresponding table
%% Opts := [Opt]
%% Opt := {verify,boolean()}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Reads a file produced by `tab2file/2` or `tab2file/3` and creates the
corresponding table `Table`.

Equivalent to [`file2tab(Filename, [])`](`file2tab/2`).
""".
-spec file2tab(Filename) -> {'ok', Table} | {'error', Reason} when
      Filename :: file:name(),
      Table :: table(),
      Reason :: term().

file2tab(File) ->
    file2tab(File, []).

-doc """
Reads a file produced by `tab2file/2` or `tab2file/3` and creates the
corresponding table `Table`.

The only supported option is `{verify,boolean()}`. If verification is turned on
(by specifying `{verify,true}`), the function uses whatever information is
present in the file to assert that the information is not damaged. How this is
done depends on which `extended_info` was written using `tab2file/3`.

If no `extended_info` is present in the file and `{verify,true}` is specified,
the number of objects written is compared to the size of the original table when
the dump was started. This can make verification fail if the table was `public`
and objects were added or removed while the table was dumped to file. To avoid
this problem, either do not verify files dumped while updated simultaneously or
use option `{extended_info, [object_count]}` to `tab2file/3`, which extends the
information in the file with the number of objects written.

If verification is turned on and the file was written with option
`{extended_info, [md5sum]}`, reading the file is slower and consumes radically
more CPU time than otherwise.

`{verify,false}` is the default.
""".
-spec file2tab(Filename, Options) -> {'ok', Table} | {'error', Reason} when
      Filename :: file:name(),
      Table :: table(),
      Options :: [Option],
      Option :: {'verify', boolean()},
      Reason :: term().

file2tab(File, Opts) ->
    try
	{ok,Verify,TableArg} = parse_f2t_opts(Opts,false,[]),
	Name = make_ref(),
        {ok, Name} =
	    case disk_log:open([{name, Name}, 
				{file, File}, 
				{mode, read_only}]) of
		{ok, Name} ->
                    {ok, Name};
		{repaired, Name, _,_} -> %Uh? cannot happen?
		    case Verify of
			true ->
			    _ = disk_log:close(Name),
			    throw(badfile);
			false ->
                            {ok, Name}
		    end;
		{error, Other1} ->
		    throw({read_error, Other1});
		Other2 ->
		    throw(Other2)
	    end,
	{ok, Major, Minor, FtOptions, MD5State, FullHeader, DLContext} =
            try get_header_data(Name, Verify)
            catch
                badfile ->
                    _ = disk_log:close(Name),
                    throw(badfile)
            end,
	try
	    if  
		Major > ?MAJOR_F2T_VERSION -> 
		    throw({unsupported_file_version,{Major,Minor}});
		true ->
		    ok
	    end,
	    {ok, Table, HeadCount} = create_tab(FullHeader, TableArg),
	    StrippedOptions = 				   
	        case Verify of
		    true ->
			FtOptions;
		    false ->
			#filetab_options{}
		end,
	    {ReadFun,InitState} = 
	        case StrippedOptions#filetab_options.md5sum of
		    true ->
			{fun({OldMD5State,OldCount,_OL,ODLContext} = OS) ->
				 case wrap_bchunk(Name,ODLContext,100,Verify) of
				     eof ->
					 {OS,[]};
				     {NDLContext,Blist} ->
					 {Termlist, NewMD5State, 
					  NewCount,NewLast} =
					     md5_and_convert(Blist,
							     OldMD5State,
							     OldCount),
					 {{NewMD5State, NewCount,
					   NewLast,NDLContext},
					  Termlist}
				 end
			 end,
			 {MD5State,0,[],DLContext}};
		    false ->
			{fun({_,OldCount,_OL,ODLContext} = OS) ->
				 case wrap_chunk(Name,ODLContext,100,Verify) of
				     eof ->
					 {OS,[]};
				     {NDLContext,List} ->
					 {NewLast,NewCount,NewList} = 
					     scan_for_endinfo(List, OldCount),
					 {{false,NewCount,NewLast,NDLContext},
					  NewList}
				 end
			 end,
			 {false,0,[],DLContext}}
		end,
	    try
		do_read_and_verify(ReadFun,InitState,Table,
				   StrippedOptions,HeadCount,Verify)
	    catch
		throw:TReason ->
		    ets:delete(Table),    
		    throw(TReason);
		exit:ExReason ->
		    ets:delete(Table),
		    exit(ExReason);
		error:ErReason:StackTrace ->
		    ets:delete(Table),
		    erlang:raise(error,ErReason,StackTrace)
	    end
	after
	    _ = disk_log:close(Name)
	end
    catch
	throw:TReason2 ->
	    {error,TReason2};
	exit:ExReason2 ->
	    {error,ExReason2}
    end.

do_read_and_verify(ReadFun,InitState,Table,FtOptions,HeadCount,Verify) ->
    case load_table(ReadFun,InitState,Table) of
	{ok,{_,FinalCount,[],_}} ->
	    case {FtOptions#filetab_options.md5sum,
		  FtOptions#filetab_options.object_count} of
		{false,false} ->
		    case Verify of
			false ->
			    ok;
			true ->
			    case FinalCount of
				HeadCount ->
				    ok;
				_ ->
				    throw(invalid_object_count)
			    end
		    end;
		_ ->
		    throw(badfile)
	    end,
	    {ok,Table};
	{ok,{FinalMD5State,FinalCount,['$end_of_table',LastInfo],_}} ->
	    ECount = case lists:keyfind(count,1,LastInfo) of
			 {count,N} ->
			     N;
			 _ ->
			     false
		     end,
	    EMD5 = case lists:keyfind(md5,1,LastInfo) of
			 {md5,M} ->
			     M;
			 _ ->
			     false
		     end,
	    case FtOptions#filetab_options.md5sum of
		true ->
		    case erlang:md5_final(FinalMD5State) of
			EMD5 ->
			    ok;
			_MD5MisM ->
			    throw(checksum_error)
		    end;
		false ->
		    ok
	    end,
	    case FtOptions#filetab_options.object_count of
		true ->
		    case FinalCount of
			ECount ->
			    ok;
			_Other ->
			    throw(invalid_object_count)
		    end;
		false ->
		    %% Only use header count if no extended info
		    %% at all is present and verification is requested.
		    case {Verify,FtOptions#filetab_options.md5sum} of
			{true,false} ->
			    case FinalCount of
				HeadCount ->
				    ok;
				_Other2 ->
				     throw(invalid_object_count)
			    end;
			_ ->
			    ok
		    end
	    end,
	    {ok,Table}
    end.

parse_f2t_opts([],Verify,Table) ->
    {ok,Verify,Table};
parse_f2t_opts([{verify, true}|T],_OV,Table) ->
    parse_f2t_opts(T,true,Table);
parse_f2t_opts([{verify,false}|T],OV,Table) ->
    parse_f2t_opts(T,OV,Table);
parse_f2t_opts([{table,Table}|T],OV,[]) ->
    parse_f2t_opts(T,OV,Table);
parse_f2t_opts([Unexpected|_],_,_) ->
    throw({unknown_option,Unexpected});
parse_f2t_opts(Malformed,_,_) ->
    throw({malformed_option,Malformed}).
			   
count_mandatory([]) ->
    0;
count_mandatory([{Tag,_}|T]) when Tag =:= name;
				  Tag =:= type;
				  Tag =:= protection;
				  Tag =:= named_table;
				  Tag =:= keypos;
				  Tag =:= size ->
    1+count_mandatory(T);
count_mandatory([_|T]) ->
    count_mandatory(T).
				   
verify_header_mandatory(L) ->						 
    count_mandatory(L) =:= 6.

wrap_bchunk(Name,C,N,true) ->
    case disk_log:bchunk(Name,C,N) of
	{_,_,X} when X > 0 ->
	    throw(badfile);
	{NC,Bin,_} ->
	    {NC,Bin};
	Y ->
	    Y
    end;
wrap_bchunk(Name,C,N,false) ->
    case disk_log:bchunk(Name,C,N) of
	{NC,Bin,_} ->
	    {NC,Bin};
	Y ->
	    Y
    end.
					  
wrap_chunk(Name,C,N,true) ->
    case disk_log:chunk(Name,C,N) of
	{_,_,X} when X > 0 ->
	    throw(badfile);
	{NC,TL,_} ->
	    {NC,TL};
	Y ->
	    Y
    end;
wrap_chunk(Name,C,N,false) ->
    case disk_log:chunk(Name,C,N) of
	{NC,TL,_} ->
	    {NC,TL};
	Y ->
	    Y
    end.

get_header_data(Name,true) ->
    case wrap_bchunk(Name,start,1,true) of
	{C,[Bin]} when is_binary(Bin) ->
	    T = binary_to_term(Bin),
	    case T of
		Tup when is_tuple(Tup) ->
		    L = tuple_to_list(Tup),
		    case verify_header_mandatory(L) of
			false ->
			    throw(badfile);
			true ->
			    Major = case lists:keyfind(major,1,L) of
					{major,Maj} ->
					    Maj;
					_ ->
					    0
				    end,
			    Minor = case lists:keyfind(minor,1,L) of
					{minor,Min} ->
					    Min;
					_ ->
					    0
				    end,
			    FtOptions = 
				case lists:keyfind(extended_info,1,L) of
				    {extended_info,I} when is_list(I) ->
					#filetab_options
					    {
					    object_count = 
					      lists:member(object_count,I),
					    md5sum = 
					      lists:member(md5sum,I)
					    };
				    _ ->
					#filetab_options{}
				end,
			    MD5Initial = 
				case FtOptions#filetab_options.md5sum of
				    true ->
					X = erlang:md5_init(),
					erlang:md5_update(X,Bin);
				    false ->
					false
				end,
			    {ok, Major, Minor, FtOptions, MD5Initial, L, C}
		    end;
		_X ->
		    throw(badfile)
	    end;
	_Y ->
	    throw(badfile)
    end;

get_header_data(Name, false) ->
   case wrap_chunk(Name, start, 1, false) of
       {C,[Tup]} when is_tuple(Tup) ->
	   L = tuple_to_list(Tup),
	   case verify_header_mandatory(L) of
	       false ->
		   throw(badfile);
	       true ->
		   Major = case lists:keyfind(major_version, 1, L) of
			       {major_version, Maj} ->
				   Maj;
			       _ ->
				   0
			   end,
		   Minor = case lists:keyfind(minor_version, 1, L) of
			       {minor_version, Min} ->
				   Min;
			       _ ->
				   0
			   end,
		   FtOptions = 
		       case lists:keyfind(extended_info, 1, L) of
			   {extended_info, I} when is_list(I) ->
			       #filetab_options
					 {
					 object_count = 
					 lists:member(object_count,I),
					 md5sum = 
					 lists:member(md5sum,I)
					};
			   _ ->
			       #filetab_options{}
		       end,
		   {ok, Major, Minor, FtOptions, false, L, C}
	   end;
       _ ->
	   throw(badfile)
    end.

md5_and_convert([], MD5State, Count) ->
    {[],MD5State,Count,[]};
md5_and_convert([H|T], MD5State, Count) when is_binary(H) ->
    case (catch binary_to_term(H)) of
	{'EXIT', _} ->
	    md5_and_convert(T,MD5State,Count);
	['$end_of_table',_Dat] = L ->
	   {[],MD5State,Count,L};
	Term ->
	    X = erlang:md5_update(MD5State, H),
	    {Rest,NewMD5,NewCount,NewLast} = md5_and_convert(T, X, Count+1),
	    {[Term | Rest],NewMD5,NewCount,NewLast}
    end.

scan_for_endinfo([], Count) ->
    {[],Count,[]};
scan_for_endinfo([['$end_of_table',Dat]], Count) ->
    {['$end_of_table',Dat],Count,[]};
scan_for_endinfo([Term|T], Count) ->
    {NewLast,NCount,Rest} = scan_for_endinfo(T, Count+1),
    {NewLast,NCount,[Term | Rest]}.

load_table(ReadFun, State, Table) ->
    {NewState,NewData} = ReadFun(State),
    case NewData of
	[] ->
	    {ok,NewState};
	List ->
	    ets:insert(Table, List),
	    load_table(ReadFun, NewState, Table)
    end.

create_tab(I, TableArg) ->
    {name, Name} = lists:keyfind(name, 1, I),
    {type, Type} = lists:keyfind(type, 1, I),
    {protection, P} = lists:keyfind(protection, 1, I),
    {keypos, _Kp} = Keypos = lists:keyfind(keypos, 1, I),
    {size, Sz} = lists:keyfind(size, 1, I),
    L1 = [Type, P, Keypos],
    L2 = case lists:keyfind(named_table, 1, I) of
             {named_table, true} -> [named_table | L1];
	     {named_table, false} -> L1
	 end,
    L3 = case lists:keyfind(compressed, 1, I) of
	     {compressed, true} -> [compressed | L2];
	     {compressed, false} -> L2;
	     false -> L2
	 end,
    L4 = case lists:keyfind(write_concurrency, 1, I) of
	     {write_concurrency, _}=Wcc -> [Wcc | L3];
	     _ -> L3
	 end,
    L5 = case lists:keyfind(read_concurrency, 1, I) of
	     {read_concurrency, _}=Rcc -> [Rcc | L4];
	     false -> L4
	 end,
    case TableArg of
        [] ->
	    try
		Table = ets:new(Name, L5),
		{ok, Table, Sz}
	    catch _:_ ->
		throw(cannot_create_table)
            end;
        _ ->
            {ok, TableArg, Sz}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tabfile_info/1 reads the head information in an ets table dumped to
%% disk by means of file2tab and returns a list of the relevant table
%% information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Returns information about the table dumped to file by `tab2file/2` or
`tab2file/3`.

The following items are returned:

- **`name`** - The name of the dumped table. If the table was a named table, a
  table with the same name cannot exist when the table is loaded from file with
  `file2tab/2`. If the table is not saved as a named table, this field has no
  significance when loading the table from file.

- **`type`** - The ETS type of the dumped table (that is, `set`, `bag`,
  `duplicate_bag`, or `ordered_set`). This type is used when loading the table
  again.

- **`protection`** - The protection of the dumped table (that is, `private`,
  `protected`, or `public`). A table loaded from the file gets the same
  protection.

- **`named_table`** - `true` if the table was a named table when dumped to file,
  otherwise `false`. Notice that when a named table is loaded from a file, there
  cannot exist a table in the system with the same name.

- **`keypos`** - The `keypos` of the table dumped to file, which is used when
  loading the table again.

- **`size`** - The number of objects in the table when the table dump to file
  started. For a `public` table, this number does not need to correspond to the
  number of objects saved to the file, as objects can have been added or deleted
  by another process during table dump.

- **`extended_info`** - The extended information written in the file footer to
  allow stronger verification during table loading from file, as specified to
  `tab2file/3`. Notice that this function only tells _which_ information is
  present, not the values in the file footer. The value is a list containing one
  or more of the atoms `object_count` and `md5sum`.

- **`version`** - A tuple `{Major,Minor}` containing the major and minor version
  of the file format for ETS table dumps. This version field was added beginning
  with STDLIB 1.5.1. Files dumped with older versions return `{0,0}` in this
  field.

An error is returned if the file is inaccessible, badly damaged, or not produced
with `tab2file/2` or `tab2file/3`.
""".
-spec tabfile_info(Filename) -> {'ok', TableInfo} | {'error', Reason} when
      Filename :: file:name(),
      TableInfo :: [InfoItem],
      InfoItem :: {'name', atom()}
                | {'type', Type}
                | {'protection', Protection}
                | {'named_table', boolean()}
                | {'keypos', non_neg_integer()}
                | {'size', non_neg_integer()}
                | {'extended_info', [ExtInfo]}
                | {'version', {Major :: non_neg_integer(),
                               Minor :: non_neg_integer()}},
      ExtInfo :: 'md5sum' | 'object_count',
      Type :: 'bag' | 'duplicate_bag' | 'ordered_set' | 'set',
      Protection :: 'private' | 'protected' | 'public',
      Reason :: term().

tabfile_info(File) when is_list(File) ; is_atom(File) ->
    try
	Name = make_ref(),
        {ok, Name} =
	    case disk_log:open([{name, Name}, 
				{file, File}, 
				{mode, read_only}]) of
		{ok, Name} ->
                    {ok, Name};
		{repaired, Name, _,_} -> %Uh? cannot happen?
		    {ok, Name};
		{error, Other1} ->
		    throw({read_error, Other1});
		Other2 ->
		    throw(Other2)
	    end,
	{ok, Major, Minor, _FtOptions, _MD5State, FullHeader, _DLContext} =
            try get_header_data(Name, false)
            catch
                badfile ->
                    _ = disk_log:close(Name),
                    throw(badfile)
            end,
        case disk_log:close(Name) of
            ok -> ok;
            {error, Reason} -> throw(Reason)
        end,
	{value, N} = lists:keysearch(name, 1, FullHeader),
	{value, Type} = lists:keysearch(type, 1, FullHeader),
	{value, P} = lists:keysearch(protection, 1, FullHeader),
	{value, Val} = lists:keysearch(named_table, 1, FullHeader),
	{value, Kp} = lists:keysearch(keypos, 1, FullHeader),
	{value, Sz} = lists:keysearch(size, 1, FullHeader),
	Ei = case lists:keyfind(extended_info, 1, FullHeader) of
		 false -> {extended_info, []};
		 Ei0 -> Ei0
	     end,
	{ok, [N,Type,P,Val,Kp,Sz,Ei,{version,{Major,Minor}}]}
    catch
	throw:TReason ->
	    {error,TReason};
	exit:ExReason ->
	    {error,ExReason}
    end.

-doc(#{equiv => table/2}).
-spec table(Table) -> QueryHandle when
      Table :: table(),
      QueryHandle :: qlc:query_handle().

table(Table) ->
    table(Table, []).

-doc """
Returns a Query List Comprehension (QLC) query handle. The `m:qlc` module
provides a query language aimed mainly at Mnesia, but ETS tables, Dets tables,
and lists are also recognized by QLC as sources of data. Calling `table/1,2` is
the means to make the ETS table `Table` usable to QLC.

When there are only simple restrictions on the key position, QLC uses `lookup/2`
to look up the keys. When that is not possible, the whole table is traversed.
Option `traverse` determines how this is done:

- **`first_next`** - The table is traversed one key at a time by calling
  `first/1` and `next/2`.

- **`last_prev`** - The table is traversed one key at a time by calling `last/1`
  and `prev/2`.

- **`select`** - The table is traversed by calling `select/3` and `select/1`.
  Option `n_objects` determines the number of objects returned (the third
  argument of [`select/3`](`select/3`)); the default is to return `100` objects
  at a time. The [match specification](`m:ets#match_spec`) (the second argument
  of [`select/3`](`select/3`)) is assembled by QLC: simple filters are
  translated into equivalent match specifications while more complicated filters
  must be applied to all objects returned by [`select/3`](`select/3`) given a
  match specification that matches all objects.

- **`{select, MatchSpec}`** - As for `select`, the table is traversed by calling
  `select/3` and `select/1`. The difference is that the match specification is
  explicitly specified. This is how to state match specifications that cannot
  easily be expressed within the syntax provided by QLC.

_Examples:_

An explicit match specification is here used to traverse the table:

```erlang
9> true = ets:insert(Table = ets:new(t, []), [{1,a},{2,b},{3,c},{4,d}]),
MS = ets:fun2ms(fun({X,Y}) when (X > 1) or (X < 5) -> {Y} end),
QH1 = ets:table(Table, [{traverse, {select, MS}}]).
```

An example with an implicit match specification:

```erlang
10> QH2 = qlc:q([{Y} || {X,Y} <- ets:table(Table), (X > 1) or (X < 5)]).
```

The latter example is equivalent to the former, which can be verified using
function `qlc:info/1`:

```erlang
11> qlc:info(QH1) =:= qlc:info(QH2).
true
```

`qlc:info/1` returns information about a query handle, and in this case
identical information is returned for the two query handles.
""".
-spec table(Table, Options) -> QueryHandle when
      Table :: table(),
      QueryHandle :: qlc:query_handle(),
      Options :: [Option] | Option,
      Option :: {'n_objects', NObjects}
              | {'traverse', TraverseMethod},
      NObjects :: 'default' | pos_integer(),
      TraverseMethod :: 'first_next' | 'last_prev'
                      | 'select' | {'select', MatchSpec :: match_spec()}.

table(Table, Opts) ->
    case options(Opts, [traverse, n_objects]) of
        {badarg,_} ->
            erlang:error(badarg, [Table, Opts]);
        [[Traverse, NObjs], QlcOptions] ->
            TF = case Traverse of
                     first_next -> 
                         fun() -> qlc_next(Table, ets:first(Table)) end;
                     last_prev -> 
                         fun() -> qlc_prev(Table, ets:last(Table)) end;
                     select -> 
                         fun(MS) -> qlc_select(ets:select(Table, MS, NObjs)) end;
                     {select, MS} ->
                         fun() -> qlc_select(ets:select(Table, MS, NObjs)) end
                 end,
            PreFun = fun(_) -> ets:safe_fixtable(Table, true) end,
            PostFun = fun() -> ets:safe_fixtable(Table, false) end,
            InfoFun = fun(Tag) -> table_info(Table, Tag) end,
            KeyEquality = case ets:info(Table, type) of
                              ordered_set -> '==';
                              _ -> '=:='
                          end,
            LookupFun = 
                case Traverse of 
                    {select, _MS} ->
                        undefined;
                    _ -> 
                        fun(_Pos, [K]) ->
                                ets:lookup(Table, K);
                           (_Pos, Ks) -> 
                                lists:flatmap(fun(K) -> ets:lookup(Table, K) 
                                              end, Ks) 
                        end
                end,
            FormatFun = 
                fun({all, _NElements, _ElementFun}) ->
                        As = [Table | [Opts || _ <- [[]], Opts =/= []]],
                        {?MODULE, table, As};
                   ({match_spec, MS}) ->
                        {?MODULE, table, 
                         [Table, [{traverse, {select, MS}} | 
                                listify(Opts)]]};
                   ({lookup, _KeyPos, [Value], _NElements, ElementFun}) ->
                        io_lib:format("~w:lookup(~w, ~w)", 
                                      [?MODULE, Table, ElementFun(Value)]);
                   ({lookup, _KeyPos, Values, _NElements, ElementFun}) ->
                        Vals = [ElementFun(V) || V <- Values],
                        io_lib:format("lists:flatmap(fun(V) -> "
                                      "~w:lookup(~w, V) end, ~w)", 
                                      [?MODULE, Table, Vals])
                end,
            qlc:table(TF, [{pre_fun, PreFun}, {post_fun, PostFun}, 
                           {info_fun, InfoFun}, {format_fun, FormatFun},
                           {key_equality, KeyEquality},
                           {lookup_fun, LookupFun}] ++ QlcOptions)
    end.
         
table_info(Table, num_of_objects) ->
    ets:info(Table, size);
table_info(Table, keypos) ->
    ets:info(Table, keypos);
table_info(Table, is_unique_objects) ->
    ets:info(Table, type) =/= duplicate_bag;
table_info(Table, is_sorted_key) ->
    ets:info(Table, type) =:= ordered_set;
table_info(_Table, _) ->
    undefined.

qlc_next(_Table, '$end_of_table') ->
    [];
qlc_next(Table, Key) ->
    ets:lookup(Table, Key) ++ fun() -> qlc_next(Table, ets:next(Table, Key)) end.

qlc_prev(_Table, '$end_of_table') ->
    [];
qlc_prev(Table, Key) ->
    ets:lookup(Table, Key) ++ fun() -> qlc_prev(Table, ets:prev(Table, Key)) end.

qlc_select('$end_of_table') -> 
    [];
qlc_select({Objects, Cont}) -> 
    Objects ++ fun() -> qlc_select(ets:select(Cont)) end.

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options, [Key | Keys], L) when is_list(Options) ->
    V = case lists:keyfind(Key, 1, Options) of
            {n_objects, default} ->
                {ok, default_option(Key)};
            {n_objects, NObjs} when is_integer(NObjs), NObjs >= 1 ->
                {ok, NObjs};
            {traverse, select} ->
                {ok, select};
            {traverse, {select, _MS} = Select} ->
                {ok, Select};
            {traverse, first_next} ->
                {ok, first_next};
            {traverse, last_prev} ->
                {ok, last_prev};
	    {Key, _} ->
		badarg;
	    false ->
		Default = default_option(Key),
		{ok, Default}
	end,
    case V of
	badarg ->
	    {badarg, Key};
	{ok,Value} ->
	    NewOptions = lists:keydelete(Key, 1, Options),
	    options(NewOptions, Keys, [Value | L])
    end;
options(Options, [], L) ->
    [lists:reverse(L), Options].

default_option(traverse) -> select;
default_option(n_objects) -> 100.

listify(L) when is_list(L) ->
    L;
listify(T) ->
    [T].

%% End of table/2.

%% Print info about all tabs on the tty
-doc "Displays information about all ETS tables on a terminal.".
-spec i() -> 'ok'.

i() ->
    hform('id', 'name', 'type', 'size', 'mem', 'owner'),
    io:format(" -------------------------------------"
	      "---------------------------------------\n"),
    lists:foreach(fun prinfo/1, tabs()),
    ok.

tabs() ->
    lists:sort(ets:all()).

prinfo(Table) ->
    case catch prinfo2(Table) of
	{'EXIT', _} ->
	    io:format("~-10s ... unreadable \n", [to_string(Table)]);
	ok -> 
	    ok
    end.
prinfo2(Table) ->
    Name = ets:info(Table, name),
    Type = ets:info(Table, type),
    Size = ets:info(Table, size),
    Mem = ets:info(Table, memory),
    Owner = ets:info(Table, owner),
    hform(Table, Name, Type, Size, Mem, is_reg(Owner)).

is_reg(Owner) ->
    case process_info(Owner, registered_name) of
	{registered_name, Name} -> Name;
	_ -> Owner
    end.

%%% Arndt: this code used to truncate over-sized fields. Now it
%%% pushes the remaining entries to the right instead, rather than
%%% losing information.
hform(A0, B0, C0, D0, E0, F0) ->
    [A,B,C,D,E,F] = [to_string(T) || T <- [A0,B0,C0,D0,E0,F0]],
    A1 = pad_right(A, 15),
    B1 = pad_right(B, 17),
    C1 = pad_right(C, 5),
    D1 = pad_right(D, 6),
    E1 = pad_right(E, 8),
    %% no need to pad the last entry on the line
    io:format(" ~s ~s ~s ~s ~s ~s\n", [A1,B1,C1,D1,E1,F]).

pad_right(String, Len) ->
    if
	length(String) >= Len ->
	    String;
	true ->
	    [Space] = " ",
	    String ++ lists:duplicate(Len - length(String), Space)
    end.

to_string(X) ->
    lists:flatten(io_lib:format("~p", [X])).

%% view a specific table
-doc "Browses table `Table` on a terminal.".
-spec i(Table) -> 'ok' when
      Table :: table().

i(Table) ->
    i(Table, 40).

-doc false.
-spec i(table(), pos_integer()) -> 'ok'.

i(Table, Height) ->
    i(Table, Height, 80).

-doc false.
-spec i(table(), pos_integer(), pos_integer()) -> 'ok'.

i(Table, Height, Width) ->
    First = ets:first(Table),
    display_items(Height, Width, Table, First, 1, 1).

display_items(Height, Width, Table, '$end_of_table', Turn, Opos) -> 
    P = 'EOT  (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, eot, Table, '$end_of_table', Turn, Opos);
display_items(Height, Width, Table, Key, Turn, Opos) when Turn < Height ->
    do_display(Height, Width, Table, Key, Turn, Opos);
display_items(Height, Width, Table, Key, Turn, Opos) when Turn >=  Height ->
    P = '(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, normal, Table, Key, Turn, Opos).

choice(Height, Width, P, Mode, Table, Key, Turn, Opos) ->
    case get_line(P, "c\n") of
	"c\n" when Mode =:= normal ->
	    do_display(Height, Width, Table, Key, 1, Opos);
	"c\n" when is_tuple(Mode), element(1, Mode) =:= re ->
	    {re, Re} = Mode,
	    re_search(Height, Width, Table, Key, Re, 1, Opos);
	"q\n" ->
	    ok;
	"k\n" ->
	    ets:delete(Table),
	    ok;
	[$p|Digs]  ->
	    catch case catch list_to_integer(nonl(Digs)) of
		      {'EXIT', _} ->
			  io:put_chars("Bad digits\n");
		      Number when Mode =:= normal ->
			  print_number(Table, ets:first(Table), Number);
		      Number when Mode =:= eot ->
			  print_number(Table, ets:first(Table), Number);
		      Number -> %% regexp
			  {re, Re} = Mode,
			  print_re_num(Table, ets:first(Table), Number, Re)
		  end,
	    choice(Height, Width, P, Mode, Table, Key, Turn, Opos);
	[$/|Regexp]   -> %% from regexp
	    case re:compile(nonl(Regexp),[unicode]) of
		{ok,Re} ->
		    re_search(Height, Width, Table, ets:first(Table), Re, 1, 1);
		{error,{ErrorString,_Pos}} ->
		    io:format("~ts\n", [ErrorString]),
		    choice(Height, Width, P, Mode, Table, Key, Turn, Opos)
	    end;
        eof ->
            ok;
	_  ->
	    choice(Height, Width, P, Mode, Table, Key, Turn, Opos)
    end.

get_line(P, Default) ->
    case line_string(io:get_line(P)) of
	"\n" ->
	    Default;
	L ->
	    L
    end.

%% If the standard input is set to binary mode
%% convert it to a list so we can properly match.
line_string(Binary) when is_binary(Binary) -> unicode:characters_to_list(Binary);
line_string(Other) -> Other.

nonl(S) -> string:trim(S, trailing, "$\n").

print_number(Table, Key, Num) ->
    Os = ets:lookup(Table, Key),
    Len = length(Os),
    if 
	(Num - Len) < 1 ->
	    O = lists:nth(Num, Os),
	    io:format("~p~n", [O]); %% use ppterm here instead
	true ->
	    print_number(Table, ets:next(Table, Key), Num - Len)
    end.

do_display(Height, Width, Table, Key, Turn, Opos) ->
    Objs = ets:lookup(Table, Key),
    do_display_items(Height, Width, Objs, Opos),
    Len = length(Objs),
    display_items(Height, Width, Table, ets:next(Table, Key), Turn+Len, Opos+Len).

do_display_items(Height, Width, [Obj|Tail], Opos) ->
    do_display_item(Height, Width, Obj, Opos),
    do_display_items(Height, Width, Tail, Opos+1);
do_display_items(_Height, _Width, [], Opos) ->
    Opos.

do_display_item(_Height, Width, I, Opos)  ->
    L = to_string(I),
    L2 = if
	     length(L) > Width - 8 ->
                 string:slice(L, 0, Width-13) ++ "  ...";
	     true ->
		 L
	 end,
    io:format("<~-4w> ~s~n", [Opos,L2]).

re_search(Height, Width, Table, '$end_of_table', Re, Turn, Opos) ->
    P = 'EOT  (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, {re, Re}, Table, '$end_of_table', Turn, Opos);
re_search(Height, Width, Table, Key, Re, Turn, Opos) when Turn < Height ->
    re_display(Height, Width, Table, Key, ets:lookup(Table, Key), Re, Turn, Opos);
re_search(Height, Width, Table, Key, Re, Turn, Opos)  ->
    P = '(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, {re, Re}, Table, Key, Turn, Opos).

re_display(Height, Width, Table, Key, [], Re, Turn, Opos) ->
    re_search(Height, Width, Table, ets:next(Table, Key), Re, Turn, Opos);
re_display(Height, Width, Table, Key, [H|T], Re, Turn, Opos) ->
    Str = to_string(H),
    case re:run(Str, Re, [{capture,none}]) of
	match ->
	    do_display_item(Height, Width, H, Opos),
	    re_display(Height, Width, Table, Key, T, Re, Turn+1, Opos+1);
	nomatch ->
	    re_display(Height, Width, Table, Key, T, Re, Turn, Opos)
    end.

print_re_num(_,'$end_of_table',_,_) -> ok;
print_re_num(Table, Key, Num, Re) ->
    Os = re_match(ets:lookup(Table, Key), Re),
    Len = length(Os),
    if 
	(Num - Len) < 1 ->
	    O = lists:nth(Num, Os),
	    io:format("~p~n", [O]); %% use ppterm here instead
	true ->
	    print_re_num(Table, ets:next(Table, Key), Num - Len, Re)
    end.

re_match([], _) -> [];
re_match([H|T], Re) ->
    case re:run(to_string(H), Re, [{capture,none}]) of
	match -> 
	    [H|re_match(T,Re)];
	nomatch ->
	    re_match(T, Re)
    end.
