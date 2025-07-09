# ERL-1020 - Redefined import

## Example

```erlang
-import(lists, [map/2]).
-import(lists, [map/2]).
```

```

```

## Explanation

This error occurs when you attempt to import the same function from the
same module more than once using the `-import` directive.

Each function can only be imported once from a given module. Duplicate
imports are redundant and not allowed.

To fix this error:

* Remove the duplicate import statement
* Ensure each function is imported only once per module

```erlang
%% Correct approach - single import
-import(lists, [map/2]).

foo(List) ->
    map(fun(X) -> X * 2 end, List).
```