Matches the objects in table `Table` using a
[match specification](`m:ets#match_spec`). If the match specification returns
`true` for an object, that object considered a match and is counted. For any
other result from the match specification the object is not considered a match
and is therefore not counted.

This function can be described as a `select_delete/2` function that does not
delete any elements, but only counts them.

The function returns the number of objects matched.
