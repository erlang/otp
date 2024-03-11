Works like `match/2`, but returns only a limited (`Limit`) number of matching
objects. Term `Continuation` can then be used in subsequent calls to `match/1`
to get the next chunk of matching objects. This is a space-efficient way to work
on objects in a table, which is faster than traversing the table object by
object using `first/1` and `next/2`.

If the table is empty, `'$end_of_table'` is returned.

Use `safe_fixtable/2` to guarantee [safe traversal](`m:ets#traversal`) for
subsequent calls to `match/1`.
