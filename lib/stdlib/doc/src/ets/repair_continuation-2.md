Restores an opaque continuation returned by `select/3` or `select/1` if the
continuation has passed through external term format (been sent between nodes or
stored on disk).

The reason for this function is that continuation terms contain compiled match
specifications and may therefore be invalidated if converted to external term
format. Given that the original match specification is kept intact, the
continuation can be restored, meaning it can once again be used in subsequent
[`select/1`](`select/1`) calls even though it has been stored on disk or on
another node.

_Examples:_

The following sequence of calls may fail:

```erlang
T=ets:new(x,[]),
...
MS = ets:fun2ms(fun({N,_}=A) when (N rem 10) =:= 0 -> A end),
{_,C} = ets:select(T, MS, 10),
MaybeBroken = binary_to_term(term_to_binary(C)),
ets:select(MaybeBroken).
```

The following sequence works, as the call to
[`repair_continuation/2`](`repair_continuation/2`) reestablishes the
`MaybeBroken` continuation.

```erlang
T=ets:new(x,[]),
...
MS = ets:fun2ms(fun({N,_}=A) when (N rem 10) =:= 0 -> A end),
{_,C} = ets:select(T,MS,10),
MaybeBroken = binary_to_term(term_to_binary(C)),
ets:select(ets:repair_continuation(MaybeBroken,MS)).
```

> #### Note {: .info }
>
> This function is rarely needed in application code. It is used by Mnesia to
> provide distributed [`select/3`](`select/3`) and [`select/1`](`select/1`)
> sequences. A normal application would either use Mnesia or keep the
> continuation from being converted to external format.
>
> The actual behavior of compiled match specifications when recreated from
> external format has changed and may change in future releases, but this
> interface remains for backward compatibility. See `is_compiled_ms/1`.
