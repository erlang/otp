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
