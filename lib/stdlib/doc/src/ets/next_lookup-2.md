Similar to `next/2` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `next/2` followed by a
`lookup/2`. If no next key exists, `'$end_of_table'` is returned.

It can be interleaved with `next/2` during traversal.
