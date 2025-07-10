# ERL-1026 - Badly formed deprecated attribute

## Example

```erlang
-deprecated([{foo, 1}]).
```

```

```

## Explanation

This error occurs when you mark a function as deprecated using the `-deprecated`
attribute, but the specified function doesn't exist in the module.

The deprecated attribute can only be applied to functions that are actually
defined in the module.

To fix this error:

* Ensure the function name and arity are correct
* Verify the function exists in the module
* Check for typos in the function name
* Remove the deprecated attribute if the function was removed

```erlang
%% Correctly deprecate an existing function
-export([foo/1]).
-deprecated([{foo, 1}]).
foo(Arg) ->
    Arg.
```