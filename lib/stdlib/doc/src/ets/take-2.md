Returns and removes a list of all objects with key `Key` in table `Table`.

The specified `Key` is used to identify the object by either _comparing equal_
the key of an object in an `ordered_set` table, or _matching_ in other types of
tables (for details on the difference, see `lookup/2` and `new/2`).
