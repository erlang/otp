Continues a match started with `select_reverse/3`. For tables of type
`ordered_set`, the traversal of the table continues to objects with keys earlier
in the Erlang term order. The returned list also contains objects with keys in
reverse order. For all other table types, the behavior is exactly that of
`select/1`.

_Example:_

```erlang
1> T = ets:new(x,[ordered_set]).
2> [ ets:insert(T,{N}) || N <- lists:seq(1,10) ].
...
3> {R0,C0} = ets:select_reverse(T,[{'_',[],['$_']}],4).
...
4> R0.
[{10},{9},{8},{7}]
5> {R1,C1} = ets:select_reverse(C0).
...
6> R1.
[{6},{5},{4},{3}]
7> {R2,C2} = ets:select_reverse(C1).
...
8> R2.
[{2},{1}]
9> '$end_of_table' = ets:select_reverse(C2).
...
```
