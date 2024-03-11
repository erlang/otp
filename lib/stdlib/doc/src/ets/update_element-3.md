This function provides an efficient way to update one or more elements within an
object, without the trouble of having to look up, update, and write back the
entire object.

This function destructively updates the object with key `Key` in table `Table`.
The element at position `Pos` is given the value `Value`.

A list of `{Pos,Value}` can be supplied to update many elements within the same
object. If the same position occurs more than once in the list, the last value
in the list is written. If the list is empty or the function fails, no updates
are done. The function is also atomic in the sense that other processes can
never see any intermediate results.

Returns `true` if an object with key `Key` is found, otherwise `false`.

The specified `Key` is used to identify the object by either _matching_ the key
of an object in a `set` table, or _compare equal_ to the key of an object in an
`ordered_set` table (for details on the difference, see `lookup/2` and `new/2`).

The function fails with reason `badarg` in the following situations:

- The table type is not `set` or `ordered_set`.
- `Pos` < 1.
- `Pos` > object arity.
