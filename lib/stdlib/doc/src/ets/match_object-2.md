Matches the objects in table `Table` against pattern `Pattern`. For a
description of patterns, see `match/2`. The function returns a list of all
objects that match the pattern.

If the key is specified in the pattern, the match is very efficient. If the key
is not specified, that is, if it is a variable or an underscore, the entire
table must be searched. The search time can be substantial if the table is very
large.

For tables of type `ordered_set`, the result is in the same order as in a
`first`/`next` traversal.
