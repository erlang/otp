Returns a Query List Comprehension (QLC) query handle. The `m:qlc` module
provides a query language aimed mainly at Mnesia, but ETS tables, Dets tables,
and lists are also recognized by QLC as sources of data. Calling `table/1,2` is
the means to make the ETS table `Table` usable to QLC.

When there are only simple restrictions on the key position, QLC uses `lookup/2`
to look up the keys. When that is not possible, the whole table is traversed.
Option `traverse` determines how this is done:

- **`first_next`** - The table is traversed one key at a time by calling
  `first/1` and `next/2`.

- **`last_prev`** - The table is traversed one key at a time by calling `last/1`
  and `prev/2`.

- **`select`** - The table is traversed by calling `select/3` and `select/1`.
  Option `n_objects` determines the number of objects returned (the third
  argument of [`select/3`](`select/3`)); the default is to return `100` objects
  at a time. The [match specification](`m:ets#match_spec`) (the second argument
  of [`select/3`](`select/3`)) is assembled by QLC: simple filters are
  translated into equivalent match specifications while more complicated filters
  must be applied to all objects returned by [`select/3`](`select/3`) given a
  match specification that matches all objects.

- **`{select, MatchSpec}`** - As for `select`, the table is traversed by calling
  `select/3` and `select/1`. The difference is that the match specification is
  explicitly specified. This is how to state match specifications that cannot
  easily be expressed within the syntax provided by QLC.

_Examples:_

An explicit match specification is here used to traverse the table:

```erlang
9> true = ets:insert(Table = ets:new(t, []), [{1,a},{2,b},{3,c},{4,d}]),
MS = ets:fun2ms(fun({X,Y}) when (X > 1) or (X < 5) -> {Y} end),
QH1 = ets:table(Table, [{traverse, {select, MS}}]).
```

An example with an implicit match specification:

```erlang
10> QH2 = qlc:q([{Y} || {X,Y} <- ets:table(Table), (X > 1) or (X < 5)]).
```

The latter example is equivalent to the former, which can be verified using
function `qlc:info/1`:

```erlang
11> qlc:info(QH1) =:= qlc:info(QH2).
true
```

`qlc:info/1` returns information about a query handle, and in this case
identical information is returned for the two query handles.
