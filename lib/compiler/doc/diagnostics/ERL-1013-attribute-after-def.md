# ERL-0013 - Attribute after function definitions

## Example

```erlang
-module(my_module).
-export([foo/0]).
foo() -> ok.
-my_attribute(some_value).
```

```

```

## Explanation

Erlang attributes must be defined before any function definition.

To fix this error, move the attribute before the function definition.

```erlang
-module(my_module).
-export([foo/0]).
% Correct attribute usage
-my_attribute(some_value).
foo() -> ok.
```