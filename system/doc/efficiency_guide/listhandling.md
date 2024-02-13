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
# List Handling

## Creating a List

Lists can only be built starting from the end and attaching list elements at the
beginning. If you use the "`++`" operator as follows, a new list is created that
is a copy of the elements in `List1`, followed by `List2`:

```erlang
List1 ++ List2
```

Looking at how `lists:append/1` or `++` would be implemented in plain Erlang,
clearly the first list is copied:

```erlang
append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.
```

When recursing and building a list, it is important to ensure that you attach
the new elements to the beginning of the list. In this way, you will build _one_
list, not hundreds or thousands of copies of the growing result list.

Let us first see how it is not to be done:

_DO NOT_

```erlang
bad_fib(N) ->
    bad_fib(N, 0, 1, []).

bad_fib(0, _Current, _Next, Fibs) ->
    Fibs;
bad_fib(N, Current, Next, Fibs) ->
    bad_fib(N - 1, Next, Current + Next, Fibs ++ [Current]).
```

Here more than one list is built. In each iteration step a new list is created
that is one element longer than the new previous list.

To avoid copying the result in each iteration, build the list in reverse order
and reverse the list when you are done:

_DO_

```erlang
tail_recursive_fib(N) ->
    tail_recursive_fib(N, 0, 1, []).

tail_recursive_fib(0, _Current, _Next, Fibs) ->
    lists:reverse(Fibs);
tail_recursive_fib(N, Current, Next, Fibs) ->
    tail_recursive_fib(N - 1, Next, Current + Next, [Current|Fibs]).
```

## List Comprehensions

Lists comprehensions still have a reputation for being slow. They used to be
implemented using funs, which used to be slow.

A list comprehension:

```erlang
[Expr(E) || E <- List]
```

is basically translated to a local function:

```erlang
'lc^0'([E|Tail], Expr) ->
    [Expr(E)|'lc^0'(Tail, Expr)];
'lc^0'([], _Expr) -> [].
```

If the result of the list comprehension will _obviously_ not be used, a list
will not be constructed. For example, in this code:

```erlang
[io:put_chars(E) || E <- List],
ok.
```

or in this code:

```erlang
...
case Var of
    ... ->
        [io:put_chars(E) || E <- List];
    ... ->
end,
some_function(...),
...
```

the value is not assigned to a variable, not passed to another function, and not
returned. This means that there is no need to construct a list and the compiler
will simplify the code for the list comprehension to:

```erlang
'lc^0'([E|Tail], Expr) ->
    Expr(E),
    'lc^0'(Tail, Expr);
'lc^0'([], _Expr) -> [].
```

The compiler also understands that assigning to '\_' means that the value will
not used. Therefore, the code in the following example will also be optimized:

```erlang
_ = [io:put_chars(E) || E <- List],
ok.
```

## Deep and Flat Lists

`lists:flatten/1` builds an entirely new list. It is therefore expensive, and
even _more_ expensive than the `++` operator (which copies its left argument,
but not its right argument).

In the following situations, you can easily avoid calling `lists:flatten/1`:

- When sending data to a port. Ports understand deep lists so there is no reason
  to flatten the list before sending it to the port.
- When calling BIFs that accept deep lists, such as
  [list_to_binary/1](`erlang:list_to_binary/1`) or
  [iolist_to_binary/1](`erlang:iolist_to_binary/1`).
- When you know that your list is only one level deep, you can use
  `lists:append/1`.

### Port Example

_DO_

```text
      ...
      port_command(Port, DeepList)
      ...
```

_DO NOT_

```erlang
      ...
      port_command(Port, lists:flatten(DeepList))
      ...
```

A common way to send a zero-terminated string to a port is the following:

_DO NOT_

```erlang
      ...
      TerminatedStr = String ++ [0], % String="foo" => [$f, $o, $o, 0]
      port_command(Port, TerminatedStr)
      ...
```

Instead:

_DO_

```erlang
      ...
      TerminatedStr = [String, 0], % String="foo" => [[$f, $o, $o], 0]
      port_command(Port, TerminatedStr)
      ...
```

### Append Example

_DO_

```erlang
      > lists:append([[1], [2], [3]]).
      [1,2,3]
      >
```

_DO NOT_

```erlang
      > lists:flatten([[1], [2], [3]]).
      [1,2,3]
      >
```

## Recursive List Functions

In section about myths, the following myth was exposed:
[Tail-Recursive Functions are Much Faster Than Recursive Functions](myths.md#tail_recursive).

There is usually not much difference between a body-recursive list function and
tail-recursive function that reverses the list at the end. Therefore,
concentrate on writing beautiful code and forget about the performance of your
list functions. In the time-critical parts of your code (and only there),
_measure_ before rewriting your code.

> #### Note {: .info }
>
> This section is about list functions that _construct_ lists. A tail-recursive
> function that does not construct a list runs in constant space, while the
> corresponding body-recursive function uses stack space proportional to the
> length of the list.

For example, a function that sums a list of integers, is _not_ to be written as
follows:

_DO NOT_

```erlang
recursive_sum([H|T]) -> H+recursive_sum(T);
recursive_sum([])    -> 0.
```

Instead:

_DO_

```erlang
sum(L) -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.
```
