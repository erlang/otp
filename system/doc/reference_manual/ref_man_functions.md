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

[](){: #syntax }

## Function Declaration Syntax

A _function declaration_ is a sequence of function clauses separated by
semicolons, and terminated by a period (`.`).

A _function clause_ consists of a _clause head_ and a _clause body_, separated by
`->`.

A clause _head_ consists of the function name, an argument list, and an optional
guard sequence beginning with the keyword `when`:

```erlang
Name(Pattern11,...,Pattern1N) [when GuardSeq1] ->
    Body1;
...;
Name(PatternK1,...,PatternKN) [when GuardSeqK] ->
    BodyK.
```

The function name is an atom. Each argument is a pattern.

The number of arguments `N` is the _arity_ of the function. A function is
uniquely defined by the module name, function name, and arity. That is, two
functions with the same name and in the same module, but with different arities
are two different functions.

A function named `f` in module `mod` and with arity `N` is often denoted as
`mod:f/N`.

A clause _body_ consists of a sequence of expressions separated by comma (`,`):

```text
Expr1,
...,
ExprN
```

Valid Erlang expressions and guard sequences are described in
[Expressions](expressions.md).

_Example:_

```erlang
fact(N) when N > 0 ->  % first clause head
    N * fact(N-1);     % first clause body

fact(0) ->             % second clause head
    1.                 % second clause body
```

[](){: #eval }

## Function Evaluation

When a function `M:F/N` is called, first the code for the function is located.
If the function cannot be found, an `undef` runtime error occurs. Notice that
the function must be exported to be visible outside the module it is defined in.

If the function is found, the function clauses are scanned sequentially until a
clause is found that fulfills both of the following two conditions:

1. The patterns in the clause head can be successfully matched against the given
   arguments.
1. The guard sequence, if any, is true.

If such a clause cannot be found, a `function_clause` runtime error occurs.

If such a clause is found, the corresponding clause body is evaluated. That is,
the expressions in the body are evaluated sequentially and the value of the last
expression is returned.

Consider the function `fact`:

```erlang
-module(mod).
-export([fact/1]).

fact(N) when N > 0 ->
    N * fact(N - 1);
fact(0) ->
    1.
```

Assume that you want to calculate the factorial for 1:

```text
1> mod:fact(1).
```

Evaluation starts at the first clause. The pattern `N` is matched against
argument 1. The matching succeeds and the guard (`N > 0`) is true, thus `N` is
bound to 1, and the corresponding body is evaluated:

```erlang
N * fact(N-1) => (N is bound to 1)
1 * fact(0)
```

Now, `fact(0)` is called, and the function clauses are scanned
sequentially again. First, the pattern `N` is matched against 0. The
matching succeeds, but the guard (`N > 0`) is false. Second, the
pattern `0` is matched against the argument `0`. The matching succeeds
and the body is evaluated:

```text
1 * fact(0) =>
1 * 1 =>
1
```

Evaluation has succeed and `mod:fact(1)` returns 1.

If `mod:fact/1` is called with a negative number as argument, no clause head
matches. A `function_clause` runtime error occurs.

## Tail recursion

If the last expression of a function body is a function call, a
_tail-recursive call_ is done. This is to ensure that no system
resources, for example, call stack, are consumed. This means that an
infinite loop using tail-recursive calls will not exhaust the call
stack and can (in principle) run forever.

_Example:_

```erlang
loop(N) ->
    io:format("~w~n", [N]),
    loop(N+1).
```

The earlier factorial example is a counter-example. It is not
tail-recursive, since a multiplication is done on the result of the recursive
call to `fact(N-1)`.

## Built-In Functions (BIFs)

Built-In Functions (BIFs) are implemented in C code in the runtime
system. BIFs do things that are difficult or impossible to implement
in Erlang. Most of the BIFs belong to module `m:erlang`, but there
are also BIFs belonging to a few other modules, for example `m:lists`
and `m:ets`.

The most commonly used BIFs belonging to `m:erlang` are _auto-imported_. They do
not need to be prefixed with the module name. Which BIFs that are auto-imported
is specified in the `m:erlang` module in ERTS. For example, standard-type
conversion BIFs like `atom_to_list` and BIFs allowed in guards can be called
without specifying the module name.

_Examples:_

```erlang
1> tuple_size({a,b,c}).
3
2> atom_to_list('Erlang').
"Erlang"
```
