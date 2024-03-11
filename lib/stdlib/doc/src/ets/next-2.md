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
