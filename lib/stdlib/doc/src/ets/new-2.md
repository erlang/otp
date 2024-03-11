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

- **`{heir,Pid,HeirData} | {heir,none}`** - Set a process as heir. The heir
  inherits the table if the owner terminates. Message
  `{'ETS-TRANSFER',tid(),FromPid,HeirData}` is sent to the heir when that
  occurs. The heir must be a local process. Default heir is `none`, which
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
