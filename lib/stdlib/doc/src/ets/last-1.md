Returns the last key `Key` according to Erlang term order in table `Table` of
type `ordered_set`. For other table types, the function is synonymous to
`first/1`. If the table is empty, `'$end_of_table'` is returned.

To find preceding keys in the table, use `prev/2`.
