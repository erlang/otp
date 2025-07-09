# ERL-1014 - Missing qlc.hrl

## Example

```erlang
foo() ->
    qlc:q([X || X <- [1,2,3]]).
```

```

```

## Explanation

This error occurs when you use `m:qlc` (Query List Comprehension) functions
without including the required header file qlc.hrl.

QLC provides a query interface to Mnesia, ETS, DETS and other data structures,
but it requires the inclusion of its header file to work properly.

To fix this error:

* Include the QLC header file at the top of your module
* Ensure you're using QLC functions correctly

```erlang
-module(my_module).
-include_lib("stdlib/include/qlc.hrl").

foo() ->
    qlc:q([X || X <- [1,2,3]]).
```