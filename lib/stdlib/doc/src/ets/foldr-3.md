`Acc0` is returned if the table is empty. This function is similar to
`lists:foldr/3`. The table elements are traversed in an unspecified order,
except for `ordered_set` tables, where they are traversed last to first.

If `Function` inserts objects into the table, or another process inserts objects
into the table, those objects _can_ (depending on key ordering) be included in
the traversal.
