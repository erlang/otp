Matches the objects in the table `Table` using a
[match specification](`m:ets#match_spec`). For each matched object, the existing
object is replaced with the match specification result.

The match-and-replace operation for each individual object is guaranteed to be
[atomic and isolated](`m:ets#module-concurrency`). The `select_replace` table traversal
as a whole, like all other select functions, does not give such guarantees.

The match specification must be guaranteed to _retain the key_ of any matched
object. If not, `select_replace` will fail with `badarg` without updating any
objects.

For the moment, due to performance and semantic constraints, tables of type
`bag` are not yet supported.

The function returns the total number of replaced objects.

_Example_

For all 2-tuples with a list in second position, add atom `'marker'` first in
the list:

```erlang
1> T = ets:new(x,[]), ets:insert(T, {key, [1, 2, 3]}).
true
2> MS = ets:fun2ms(fun({K, L}) when is_list(L) -> {K, [marker | L]} end).
[{{'$1','$2'},[{is_list,'$2'}],[{{'$1',[marker|'$2']}}]}]
3> ets:select_replace(T, MS).
1
4> ets:tab2list(T).
[{key,[marker,1,2,3]}]
```

A generic single object compare-and-swap operation:

```erlang
[Old] = ets:lookup(T, Key),
New = update_object(Old),
Success = (1 =:= ets:select_replace(T, [{Old, [], [{const, New}]}])),
```
