Returns the first key `Key` in table `Table`. For an `ordered_set` table, the
first key in Erlang term order is returned. For other table types, the first key
according to the internal order of the table is returned. If the table is empty,
`'$end_of_table'` is returned.

To find subsequent keys in the table, use `next/2`.
