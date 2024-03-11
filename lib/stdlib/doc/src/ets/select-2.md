Matches the objects in table `Table` using a
[match specification](`m:ets#match_spec`). This is a more general call than
`match/2` and `match_object/2` calls. In its simplest form, the match
specification is as follows:

```text
MatchSpec = [MatchFunction]
MatchFunction = {MatchHead, [Guard], [Result]}
MatchHead = "Pattern as in ets:match"
Guard = {"Guardtest name", ...}
Result = "Term construct"
```

This means that the match specification is always a list of one or more tuples
(of arity 3). The first element of the tuple is to be a pattern as described in
`match/2`. The second element of the tuple is to be a list of 0 or more guard
tests (described below). The third element of the tuple is to be a list
containing a description of the value to return. In almost all normal cases, the
list contains exactly one term that fully describes the value to return for each
object.

The return value is constructed using the "match variables" bound in `MatchHead`
or using the special match variables `'$_'` (the whole matching object) and
`'$$'` (all match variables in a list), so that the following
[`match/2`](`match/2`) expression:

```text
ets:match(Table,{'$1','$2','$3'})
```

is exactly equivalent to:

```text
ets:select(Table,[{{'$1','$2','$3'},[],['$$']}])
```

And that the following [`match_object/2`](`match_object/2`) call:

```text
ets:match_object(Table,{'$1','$2','$1'})
```

is exactly equivalent to

```text
ets:select(Table,[{{'$1','$2','$1'},[],['$_']}])
```

Composite terms can be constructed in the `Result` part either by simply writing
a list, so that the following code:

```text
ets:select(Table,[{{'$1','$2','$3'},[],['$$']}])
```

gives the same output as:

```text
ets:select(Table,[{{'$1','$2','$3'},[],[['$1','$2','$3']]}])
```

That is, all the bound variables in the match head as a list. If tuples are to
be constructed, one has to write a tuple of arity 1 where the single element in
the tuple is the tuple one wants to construct (as an ordinary tuple can be
mistaken for a `Guard`).

Therefore the following call:

```text
ets:select(Table,[{{'$1','$2','$1'},[],['$_']}])
```

gives the same output as:

```erlang
ets:select(Table,[{{'$1','$2','$1'},[],[{{'$1','$2','$3'}}]}])
```

This syntax is equivalent to the syntax used in the trace patterns (see the
`m:dbg`) module in Runtime_Tools.

The `Guard`s are constructed as tuples, where the first element is the test name
and the remaining elements are the test parameters. To check for a specific type
(say a list) of the element bound to the match variable `'$1'`, one would write
the test as `{is_list, '$1'}`. If the test fails, the object in the table does
not match and the next `MatchFunction` (if any) is tried. Most guard tests
present in Erlang can be used, but only the new versions prefixed `is_` are
allowed (`is_float`, `is_atom`, and so on).

The `Guard` section can also contain logic and arithmetic operations, which are
written with the same syntax as the guard tests (prefix notation), so that the
following guard test written in Erlang:

```text
is_integer(X), is_integer(Y), X + Y < 4711
```

is expressed as follows (`X` replaced with `'$1'` and `Y` with `'$2'`):

```text
[{is_integer, '$1'}, {is_integer, '$2'}, {'<', {'+', '$1', '$2'}, 4711}]
```

For tables of type `ordered_set`, objects are visited in the same order as in a
`first`/`next` traversal. This means that the match specification is executed
against objects with keys in the `first`/`next` order and the corresponding
result list is in the order of that execution.
