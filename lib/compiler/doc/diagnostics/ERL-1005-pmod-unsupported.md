# ERL-1005 - Unsupported parametrized module

## Example

```erlang
-module(my_module, [Param]).
```

```

```

## Explanation

This error occurs when you attempt to use parameterized modules, which were
an experimental feature in older versions of Erlang but are no longer
supported.

Parameterized modules allowed you to create modules with parameters, but
this feature has been removed from modern Erlang/OTP versions.

To fix this error:

1. Remove the parameter list from the -module directive
2. Refactor your code to use regular modules with explicit parameter passing
3. Consider using records or maps to group related data instead

```erlang
%% Instead of parameterized modules, use regular modules
-module(my_module).

%% Pass parameters explicitly to functions
init(Param) ->
    %% Initialize with parameter
    {ok, Param}.
```

See also [parametrized module](https://erlang.org/workshop/2003/paper/p29-carlsson.pdf)
for more references.
