<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Functions

## Pattern Matching

Pattern matching in function head as well as in `case` and `receive` clauses are
optimized by the compiler. With a few exceptions, there is nothing to gain by
rearranging clauses.

One exception is pattern matching of binaries. The compiler does not rearrange
clauses that match binaries. Placing the clause that matches against the empty
binary _last_ is usually slightly faster than placing it _first_.

The following is a rather unnatural example to show another exception:

_DO NOT_

```erlang
atom_map1(one) -> 1;
atom_map1(two) -> 2;
atom_map1(three) -> 3;
atom_map1(Int) when is_integer(Int) -> Int;
atom_map1(four) -> 4;
atom_map1(five) -> 5;
atom_map1(six) -> 6.
```

The problem is the clause with the variable `Int`. As a variable can match
anything, including the atoms `four`, `five`, and `six`, which the following
clauses also match, the compiler must generate suboptimal code that executes as
follows:

- First, the input value is compared to `one`, `two`, and `three` (using a
  single instruction that does a binary search; thus, quite efficient even if
  there are many values) to select which one of the first three clauses to
  execute (if any).
- If none of the first three clauses match, the fourth clause match as a
  variable always matches.
- If the guard test [`is_integer(Int)`](`is_integer/1`) succeeds, the fourth
  clause is executed.
- If the guard test fails, the input value is compared to `four`, `five`, and
  `six`, and the appropriate clause is selected. (There is a `function_clause`
  exception if none of the values matched.)

Rewriting to either:

_DO_

```erlang
atom_map2(one) -> 1;
atom_map2(two) -> 2;
atom_map2(three) -> 3;
atom_map2(four) -> 4;
atom_map2(five) -> 5;
atom_map2(six) -> 6;
atom_map2(Int) when is_integer(Int) -> Int.
```

or:

_DO_

```erlang
atom_map3(Int) when is_integer(Int) -> Int;
atom_map3(one) -> 1;
atom_map3(two) -> 2;
atom_map3(three) -> 3;
atom_map3(four) -> 4;
atom_map3(five) -> 5;
atom_map3(six) -> 6.
```

gives slightly more efficient matching code.

Another example:

_DO NOT_

```erlang
map_pairs1(_Map, [], Ys) ->
    Ys;
map_pairs1(_Map, Xs, [] ) ->
    Xs;
map_pairs1(Map, [X|Xs], [Y|Ys]) ->
    [Map(X, Y)|map_pairs1(Map, Xs, Ys)].
```

The first argument is _not_ a problem. It is variable, but it is a variable in
all clauses. The problem is the variable in the second argument, `Xs`, in the
middle clause. Because the variable can match anything, the compiler is not
allowed to rearrange the clauses, but must generate code that matches them in
the order written.

If the function is rewritten as follows, the compiler is free to rearrange the
clauses:

_DO_

```erlang
map_pairs2(_Map, [], Ys) ->
    Ys;
map_pairs2(_Map, [_|_]=Xs, [] ) ->
    Xs;
map_pairs2(Map, [X|Xs], [Y|Ys]) ->
    [Map(X, Y)|map_pairs2(Map, Xs, Ys)].
```

The compiler will generate code similar to this:

_DO NOT (already done by the compiler)_

```erlang
explicit_map_pairs(Map, Xs0, Ys0) ->
    case Xs0 of
	[X|Xs] ->
	    case Ys0 of
		[Y|Ys] ->
		    [Map(X, Y)|explicit_map_pairs(Map, Xs, Ys)];
		[] ->
		    Xs0
	    end;
	[] ->
	    Ys0
    end.
```

This is slightly faster for probably the most common case that the input lists
are not empty or very short. (Another advantage is that Dialyzer can deduce a
better type for the `Xs` variable.)

## Function Calls

This is a rough hierarchy of the performance of the different types of function
calls:

- Calls to local or external functions (`foo()`, `m:foo()`) are the fastest
  calls.
- Calling or applying a fun (`Fun()`, [`apply(Fun, [])`](`apply/2`)) is just a
  little slower than external calls.
- Applying an exported function (`Mod:Name()`,
  [`apply(Mod, Name, [])`](`apply/3`)) where the number of arguments is known at
  compile time is next.
- Applying an exported function ([`apply(Mod, Name, Args)`](`apply/3`)) where
  the number of arguments is not known at compile time is the least efficient.

### Notes and Implementation Details

Calling and applying a fun does not involve any hash-table lookup. A fun
contains an (indirect) pointer to the function that implements the fun.

[`apply/3`](`apply/3`) must look up the code for the function to execute in a
hash table. It is therefore always slower than a direct call or a fun call.

Caching callback functions into funs may be more efficient in the long run than
apply calls for frequently-used callbacks.

## Memory Usage in Recursion

When writing recursive functions, it is preferable to make them tail-recursive
so that they can execute in constant memory space:

_DO_

```erlang
list_length(List) ->
    list_length(List, 0).

list_length([], AccLen) ->
    AccLen; % Base case

list_length([_|Tail], AccLen) ->
    list_length(Tail, AccLen + 1). % Tail-recursive
```

_DO NOT_

```erlang
list_length([]) ->
    0. % Base case
list_length([_ | Tail]) ->
    list_length(Tail) + 1. % Not tail-recursive
```
