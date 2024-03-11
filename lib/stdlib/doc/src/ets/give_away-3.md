Make process `Pid` the new owner of table `Table`. If successful, message
`{'ETS-TRANSFER',Table,FromPid,GiftData}` is sent to the new owner.

The process `Pid` must be alive, local, and not already the owner of the table.
The calling process must be the table owner.

Notice that this function does not affect option [`heir`](`m:ets#heir`) of the
table. A table owner can, for example, set `heir` to itself, give the table
away, and then get it back if the receiver terminates.
