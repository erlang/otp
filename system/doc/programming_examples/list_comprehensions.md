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
# List Comprehensions

## Simple Examples

This section starts with a simple example, showing a generator and a filter:

```text
> [X || X <- [1,2,a,3,4,b,5,6], X > 3].
[a,4,b,5,6]
```

This is read as follows: The list of X such that X is taken from the list
`[1,2,a,...]` and X is greater than 3.

The notation `X <- [1,2,a,...]` is a generator and the expression `X > 3` is a
filter.

An additional filter, [`is_integer(X)`](`is_integer/1`), can be added to
restrict the result to integers:

```erlang
> [X || X <- [1,2,a,3,4,b,5,6], is_integer(X), X > 3].
[4,5,6]
```

Generators can be combined. For example, the Cartesian product of two lists can
be written as follows:

```erlang
> [{X, Y} || X <- [1,2,3], Y <- [a,b]].
[{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}]
```

## Quick Sort

The well-known quick sort routine can be written as follows:

```erlang
sort([]) -> [];
sort([_] = L) -> L;
sort([Pivot|T]) ->
    sort([ X || X <- T, X < Pivot]) ++
    [Pivot] ++
    sort([ X || X <- T, X >= Pivot]).
```

The expression `[X || X <- T, X < Pivot]` is the list of all elements in `T`
that are less than `Pivot`.

`[X || X <- T, X >= Pivot]` is the list of all elements in `T` that are greater
than or equal to `Pivot`.

With the algorithm above, a list is sorted as follows:

- A list with zero or one element is trivially sorted.
- For lists with more than one element:
  1. The first element in the list is isolated as the pivot element.
  1. The remaining list is partitioned into two sublists, such that:
  - The first sublist contains all elements that are smaller than the pivot
    element.
  - The second sublist contains all elements that are greater than or equal to
    the pivot element.
  1. The sublists are recursively sorted by the same algorithm and the results
     are combined, resulting in a list consisting of:
  - All elements from the first sublist, that is all elements smaller than the
    pivot element, in sorted order.
  - The pivot element.
  - All elements from the second sublist, that is all elements greater than or
    equal to the pivot element, in sorted order.

> #### Note {: .info }
>
> While the sorting algorithm as shown above serves as a nice example to
> illustrate list comprehensions with filters, for real world use cases the
> `m:lists` module contains sorting functions that are implemented in a more
> efficient way.

## Permutations

The following example generates all permutations of the elements in a list:

```erlang
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].
```

This takes `H` from `L` in all possible ways. The result is the set of all lists
`[H|T]`, where `T` is the set of all possible permutations of `L`, with `H`
removed:

```text
> perms([b,u,g]).
[[b,u,g],[b,g,u],[u,b,g],[u,g,b],[g,b,u],[g,u,b]]
```

## Pythagorean Triplets

Pythagorean triplets are sets of integers `{A,B,C}` such that
`A**2 + B**2 = C**2`.

The function `pyth(N)` generates a list of all integers `{A,B,C}` such that
`A**2 + B**2 = C**2` and where the sum of the sides is equal to, or less than,
`N`:

```erlang
pyth(N) ->
    [ {A,B,C} ||
        A <- lists:seq(1,N),
        B <- lists:seq(1,N),
        C <- lists:seq(1,N),
        A+B+C =< N,
        A*A+B*B == C*C
    ].
```

```erlang
> pyth(3).
[].
> pyth(11).
[].
> pyth(12).
[{3,4,5},{4,3,5}]
> pyth(50).
[{3,4,5},
 {4,3,5},
 {5,12,13},
 {6,8,10},
 {8,6,10},
 {8,15,17},
 {9,12,15},
 {12,5,13},
 {12,9,15},
 {12,16,20},
 {15,8,17},
 {16,12,20}]
```

The following code reduces the search space and is more efficient:

```erlang
pyth1(N) ->
   [{A,B,C} ||
       A <- lists:seq(1,N-2),
       B <- lists:seq(A+1,N-1),
       C <- lists:seq(B+1,N),
       A+B+C =< N,
       A*A+B*B == C*C ].
```

## Simplifications With List Comprehensions

As an example, list comprehensions can be used to simplify some of the functions
in `lists.erl`:

```erlang
append(L)   ->  [X || L1 <- L, X <- L1].
map(Fun, L) -> [Fun(X) || X <- L].
filter(Pred, L) -> [X || X <- L, Pred(X)].
```

## Variable Bindings in List Comprehensions

The scope rules for variables that occur in list comprehensions are as follows:

- All variables that occur in a generator pattern are assumed to be "fresh"
  variables.
- Any variables that are defined before the list comprehension, and that are
  used in filters, have the values they had before the list comprehension.
- Variables cannot be exported from a list comprehension.

As an example of these rules, suppose you want to write the function `select`,
which selects certain elements from a list of tuples. Suppose you write
`select(X, L) -> [Y || {X, Y} <- L].` with the intention of extracting all
tuples from `L`, where the first item is `X`.

Compiling this gives the following diagnostic:

```text
./FileName.erl:Line: Warning: variable 'X' shadowed in generate
```

This diagnostic warns that the variable `X` in the pattern is not the same as
the variable `X` that occurs in the function head.

Evaluating `select` gives the following result:

```erlang
> select(b,[{a,1},{b,2},{c,3},{b,7}]).
[1,2,3,7]
```

This is not the wanted result. To achieve the desired effect, `select` must be
written as follows:

```erlang
select(X, L) ->  [Y || {X1, Y} <- L, X == X1].
```

The generator now contains unbound variables and the test has been moved into
the filter.

This now works as expected:

```erlang
> select(b,[{a,1},{b,2},{c,3},{b,7}]).
[2,7]
```

Also note that a variable in a generator pattern will shadow a variable with the
same name bound in a previous generator pattern. For example:

```erlang
> [{X,Y} || X <- [1,2,3], X=Y <- [a,b,c]].
[{a,a},{b,b},{c,c},{a,a},{b,b},{c,c},{a,a},{b,b},{c,c}]
```

A consequence of the rules for importing variables into a list comprehensions is
that certain pattern matching operations must be moved into the filters and
cannot be written directly in the generators.

To illustrate this, do _not_ write as follows:

```text
f(...) ->
    Y = ...
    [ Expression || PatternInvolving Y  <- Expr, ...]
    ...
```

Instead, write as follows:

```text
f(...) ->
    Y = ...
    [ Expression || PatternInvolving Y1  <- Expr, Y == Y1, ...]
    ...
```
