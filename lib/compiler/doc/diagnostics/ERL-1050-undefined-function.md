# ERL-1050 - Undefined function

## Example

```erlang
main() ->
    exists(),
    not_exists().
```

```

```

## Explanation

The warning message indicates that the invoked function cannot be found.

The problem is usually due to misspelling, to the wrong number of arguments
passed to the function, or to a recent removal of the target function.

To fix the problem you should verify whether the invoked function actually
exists and has the correct arity. Remember that in Erlang a function is
identified by its name **and** the number of arguments it takes.