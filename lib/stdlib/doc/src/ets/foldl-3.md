`Acc0` is returned if the table is empty. This function is similar to
`lists:foldl/3`. The table elements are traversed in an unspecified order,
except for `ordered_set` tables, where they are traversed first to last.

If `Function` inserts objects into the table, or another process inserts objects
into the table, those objects _can_ (depending on key ordering) be included in
the traversal.
