Continues a match started with `match/3`. The next chunk of the size specified
in the initial [`match/3`](`match/3`) call is returned together with a new
`Continuation`, which can be used in subsequent calls to this function.

When there are no more objects in the table, `'$end_of_table'` is returned.
