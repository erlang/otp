# ERL-1010 - Invalid call

## Example

```erlang
foo() ->
    123().
```

```

```

## Explanation

This error occurs when you attempt to call something that is not a function.
In Erlang, only atoms, variables containing function references, or fun
expressions can be called.

Common causes include trying to call a number, string, or other non-callable
value.

To fix this error:

* Ensure you're calling an actual function
* Check that the function name is correct
* Verify the syntax of your function call

```erlang
%% Correct function calls
foo() ->
    bar(),           % Call function bar/0
    some_module:baz(), % Call function baz/0 in some_module
    Fun = fun() -> ok end,
    Fun().           % Call a fun
```