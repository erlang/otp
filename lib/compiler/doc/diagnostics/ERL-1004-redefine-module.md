# ERL-1004 - Redefined module

## Example

```erlang
-module(my_module).
-module(my_module).
```


## Explanation

This error occurs when you attempt to define the module name more than
once using the `-module` attribute.

Each Erlang source file should contain exactly one `-module` attribute,
and it should appear at the beginning of the file.

To fix this error:

- Remove the duplicate `-module` attribute.
- Ensure only one `-module` attribute exists in your file.

```erlang
%% Correct approach - single module definition
-module(my_module).
```
