Fixes a table of type `set`, `bag`, or `duplicate_bag` for
[safe traversal](`m:ets#traversal`) using `first/1` & `next/2`, `match/3` &
`match/1`, `match_object/3` & `match_object/1`, or `select/3` & `select/1`.

A process fixes a table by calling
[`safe_fixtable(Table, true)`](`safe_fixtable/2`). The table remains fixed until
the process releases it by calling
[`safe_fixtable(Table, false)`](`safe_fixtable/2`), or until the process
terminates.

If many processes fix a table, the table remains fixed until all processes have
released it (or terminated). A reference counter is kept on a per process basis,
and N consecutive fixes requires N releases to release the table.

When a table is fixed, a sequence of `first/1` and `next/2` calls are guaranteed
to succeed even if keys are removed during the traversal. The keys for objects
inserted or deleted during a traversal may or may not be returned by
[`next/2`](`next/2`) depending on the ordering of keys within the table and if
the key exists at the time [`next/2`](`next/2`) is called.

_Example:_

```erlang
clean_all_with_value(Table,X) ->
    safe_fixtable(Table,true),
    clean_all_with_value(Table,X,ets:first(Table)),
    safe_fixtable(Table,false).

clean_all_with_value(Table,X,'$end_of_table') ->
    true;
clean_all_with_value(Table,X,Key) ->
    case ets:lookup(Table,Key) of
        [{Key,X}] ->
            ets:delete(Table,Key);
        _ ->
            true
    end,
    clean_all_with_value(Table,X,ets:next(Table,Key)).
```

Notice that deleted objects are not freed from a fixed table until it has been
released. If a process fixes a table but never releases it, the memory used by
the deleted objects is never freed. The performance of operations on the table
also degrades significantly.

To retrieve information about which processes have fixed which tables, use
[`info(Table, safe_fixed_monotonic_time)`](`m:ets#info_2_safe_fixed_monotonic_time`).
A system with many processes fixing tables can need a monitor that sends alarms
when tables have been fixed for too long.

Notice that [`safe_fixtable/2`](`safe_fixtable/2`) is not necessary for table
type `ordered_set` and for traversals done by a single ETS function call, like
`select/2`.
