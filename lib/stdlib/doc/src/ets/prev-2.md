Returns the previous key `Key2`, preceding key `Key1` according to Erlang term
order in table `Table` of type `ordered_set`. For other table types, the
function is synonymous to `next/2`. If no previous key exists, `'$end_of_table'`
is returned.

To find the last key in an `ordered_set` table, use `last/1`.
