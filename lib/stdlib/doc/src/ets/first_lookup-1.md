Similar to `first/1` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `first/1` followed by a
`lookup/2`. If the table is empty, `'$end_of_table'` is returned.

To find subsequent objects in the table, use `next_lookup/2`.
