This function provides an efficient way to update one or more counters, without
the trouble of having to look up an object, update the object by incrementing an
element, and insert the resulting object into the table again. The operation is
guaranteed to be [atomic and isolated](`m:ets#module-concurrency`).

This function destructively updates the object with key `Key` in table `Table`
by adding `Incr` to the element at position `Pos`. The new counter value is
returned. If no position is specified, the element directly following key
(`<keypos>+1`) is updated.

If a `Threshold` is specified, the counter is reset to value `SetValue` if the
following conditions occur:

- `Incr` is not negative (`>= 0`) and the result would be greater than (`>`)
  `Threshold`.
- `Incr` is negative (`< 0`) and the result would be less than (`<`)
  `Threshold`.

A list of `UpdateOp` can be supplied to do many update operations within the
object. The operations are carried out in the order specified in the list. If
the same counter position occurs more than once in the list, the corresponding
counter is thus updated many times, each time based on the previous result. The
return value is a list of the new counter values from each update operation in
the same order as in the operation list. If an empty list is specified, nothing
is updated and an empty list is returned. If the function fails, no updates are
done.

The specified `Key` is used to identify the object by either _matching_ the key
of an object in a `set` table, or _compare equal_ to the key of an object in an
`ordered_set` table (for details on the difference, see `lookup/2` and `new/2`).

The function fails with reason `badarg` in the following situations:

- The table type is not `set` or `ordered_set`.
- No object with the correct key exists.
- The object has the wrong arity.
- The element to update is not an integer.
- The element to update is also the key.
- Any of `Pos`, `Incr`, `Threshold`, or `SetValue` is not an integer.
