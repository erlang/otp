For a table `Table` of type `set` or `ordered_set`, the function returns the
`Pos`:th element of the object with key `Key`.

For tables of type `bag` or `duplicate_bag`, the functions returns a list with
the `Pos`:th element of every object with key `Key`.

If no object with key `Key` exists, the function exits with reason `badarg`.

If `Pos` is larger than the size of the tuple, the function exits with reason
`badarg`.

The difference between `set`, `bag`, and `duplicate_bag` on one hand, and
`ordered_set` on the other, regarding the fact that `ordered_set` view keys as
equal when they _compare equal_ whereas the other table types regard them equal
only when they _match_, holds for [`lookup_element/3`](`lookup_element/3`).
