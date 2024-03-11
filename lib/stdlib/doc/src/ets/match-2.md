Matches the objects in table `Table` against pattern `Pattern`.

A pattern is a term that can contain:

- Bound parts (Erlang terms)
- `'_'` that matches any Erlang term
- Pattern variables `'$N'`, where `N`=0,1,...

The function returns a list with one element for each matching object, where
each element is an ordered list of pattern variable bindings, for example:

```erlang
6> ets:match(T, '$1'). % Matches every object in table
[[{rufsen,dog,7}],[{brunte,horse,5}],[{ludde,dog,5}]]
7> ets:match(T, {'_',dog,'$1'}).
[[7],[5]]
8> ets:match(T, {'_',cow,'$1'}).
[]
```

If the key is specified in the pattern, the match is very efficient. If the key
is not specified, that is, if it is a variable or an underscore, the entire
table must be searched. The search time can be substantial if the table is very
large.

For tables of type `ordered_set`, the result is in the same order as in a
`first`/`next` traversal.
