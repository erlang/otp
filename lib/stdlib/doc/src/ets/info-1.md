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
