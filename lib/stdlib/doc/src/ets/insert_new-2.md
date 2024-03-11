Same as `insert/2` except that instead of overwriting objects with the same key
(for `set` or `ordered_set`) or adding more objects with keys already existing
in the table (for `bag` and `duplicate_bag`), `false` is returned.

If `ObjectOrObjects` is a list, the function checks _every_ key before inserting
anything. Nothing is inserted unless _all_ keys present in the list are absent
from the table. Like [`insert/2`](`insert/2`), the entire operation is
guaranteed to be [atomic and isolated](`m:ets#module-concurrency`).
