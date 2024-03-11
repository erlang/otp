Works like `select/3`, but for table type `ordered_set` traversing is done
starting at the last object in Erlang term order and moves to the first. For all
other table types, the return value is identical to that of
[`select/3`](`select/3`).

Notice that this is _not_ equivalent to reversing the result list of a
[`select/3`](`select/3`) call, as the result list is not only reversed, but also
contains the last `Limit` matching objects in the table, not the first.
