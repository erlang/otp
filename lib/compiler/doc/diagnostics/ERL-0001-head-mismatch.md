# ERL-0001 - Function head mismatch

## Example

```erlang
%% foo.erl
-module(foo).
foo(0) -> 1;
boo(1) -> 2.
```

```
$ erlc foo.erl
foo.erl:4:1: error: head mismatch: previous function foo/1 is distinct from bar/1. [ERL-0001]
%    4| bar(1) -> 2.
%     | ^
% help: call `erlc -explain ERL-0001` to see a detailed explanation
% help: Is the semicolon in foo/1 unwanted?
```

## Explanation

The error message indicates that two function clauses belonging the same function
differ in their name or in the number of arguments.

In Erlang functions are uniquely identified by the module they belong to, the
function name and the number of argument they take (known as *arity*).
Each function can be composed by multiple *clauses*, separated by a semicolon (`;`).
Therefore, all clauses belonging to the same function have to share the same name.

To fix the error you need to ensure that every function clause has the same name
and that it takes the same number of arguments.

In the above example, `boo/1` could be a second clause for the `foo/1` function,
containing a typo. In that case, the corrective action would be to fix the typo:

```erlang
foo(0) -> 1;
foo(1) -> 2.
```

It could also be that `boo/1` is intended to be a completely different function.
In that case the error can be fixed by replacing the semicolon on the previous
line with a fullstop. Leaving an empty line between the two functions would also
be a good idea, to help the reader understanding `foo/1` and `boo/1` are two
distinct functions:

```erlang
foo(0) -> 1.

boo(1) -> 2.
```

For more information about Erlang functions please refer to the
[Reference Manual](`e:system:ref_man_functions`).
