Matches the objects in table `Table` using a
[match specification](`m:ets#match_spec`). If the match specification returns
`true` for an object, that object is removed from the table. For any other
result from the match specification the object is retained. This is a more
general call than the `match_delete/2` call.

The function returns the number of objects deleted from the table.

> #### Note {: .info }
>
> The match specification has to return the atom `true` if the object is to be
> deleted. No other return value gets the object deleted. So one cannot use the
> same match specification for looking up elements as for deleting them.
