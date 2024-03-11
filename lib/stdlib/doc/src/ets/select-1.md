Continues a match started with `select/3`. The next chunk of the size specified
in the initial [`select/3`](`select/3`) call is returned together with a new
`Continuation`, which can be used in subsequent calls to this function.

When there are no more objects in the table, `'$end_of_table'` is returned.
