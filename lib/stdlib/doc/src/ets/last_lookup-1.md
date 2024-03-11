Similar to `last/1` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `last/1` followed by a
`lookup/2`. If the table is empty, `'$end_of_table'` is returned.

To find preceding objects in the table, use `prev_lookup/2`.
