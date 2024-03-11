This function returns the `t:tid/0` of the named table identified by
`TableName`, or `undefined` if no such table exists. The `t:tid/0` can be used
in place of the table name in all operations, which is slightly faster since the
name does not have to be resolved on each call.

If the table is deleted, the `t:tid/0` will be invalid even if another named
table is created with the same name.
