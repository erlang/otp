# ERL-1021 - Undefined inlined function

## Example

```erlang
-compile({inline, [foo/1]}).
```

```

```

## Explanation

This error occurs when you specify a function for inlining with the
`-compile({inline, ...})` directive, but the function is not defined in the
module.

Common causes include:

- Mismatched arity (number of arguments)
- Typos in the function name
- Function doesn't exist in the module

To fix this error:

* Ensure the function name and arity match exactly
* Verify the function exists in the module
* Check for typos in the function name

```erlang
%% Correct inline directive
-compile({inline, [foo/0]}).

foo() -> ok.
```