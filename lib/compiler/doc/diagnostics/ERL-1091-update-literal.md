# ERL-1091 - Updating literal

## Example

```erlang
-define(DEFAULT, #{a => 1}).

updated(Value) ->
    ?DEFAULT#{a => Value}.
```

```

```

## Explanation

The warning occurs when a map or a record is updated using the following
syntaxes:

```erlang
> #{a => b}#{c => d}
#{c => d,a => b}
```

```erlang
> rd(my_record, {a, b}). %% rd/2 allows you to define an Erlang record from a shell
> #my_record{a = 1}#my_record{a = 2}.
#my_record{a = 2,b = undefined}
```

While this is valid Erlang syntax, this behaviour is usually not intentional
and the result of a missing comma in a list of elements. Consider, for example:

```erlang
my_list() ->
  [ #{a => 1} %% Missing comma here!
    #{a => 2}
  ].
```

Which results in `[#{a => 2}]`.

To fix the issue, just add the missing comma. If the update is intentional,
a common (but ugly) workaround to silent the linter is to wrap the first
map/record in a `begin/end` block, which will avoid any additional runtime
cost. As an example, you could rewrite the following:

```erlang
-define(DEFAULT, #{a => 1}).

updated(Value) ->
    ?DEFAULT#{a => Value}.
```

Into:

```erlang
-define(DEFAULT, #{a => 1}).

updated(Value) ->
    begin ?DEFAULT end#{a => Value}.
```
