Similar to `prev/2` except that it returns the object(s) along with the key
stored in the table. This is equivalent to doing `prev/2` followed by a
`lookup/2`. If no previous key exists, `'$end_of_table'` is returned.

It can be interleaved with `prev/2` during traversal.
