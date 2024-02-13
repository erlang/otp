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
# The Seven Myths of Erlang Performance

[](){: #myths }

Some truths seem to live on well beyond their best-before date, perhaps because
"information" spreads faster from person-to-person than a single release note
that says, for example, that body-recursive calls have become faster.

This section tries to kill the old truths (or semi-truths) that have become
myths.

## Myth: Tail-Recursive Functions are Much Faster Than Recursive Functions

[](){: #tail_recursive } According to the myth, using a tail-recursive function
that builds a list in reverse followed by a call to `lists:reverse/1` is faster
than a body-recursive function that builds the list in correct order; the reason
being that body-recursive functions use more memory than tail-recursive
functions.

That was true to some extent before R12B. It was even more true before R7B.
Today, not so much. A body-recursive function generally uses the same amount of
memory as a tail-recursive function. It is generally not possible to predict
whether the tail-recursive or the body-recursive version will be faster.
Therefore, use the version that makes your code cleaner (hint: it is usually the
body-recursive version).

For a more thorough discussion about tail and body recursion, see
[Erlang's Tail Recursion is Not a Silver Bullet](http://ferd.ca/erlang-s-tail-recursion-is-not-a-silver-bullet.html).

> #### Note {: .info }
>
> A tail-recursive function that does not need to reverse the list at the end is
> faster than a body-recursive function, as are tail-recursive functions that do
> not construct any terms at all (for example, a function that sums all integers
> in a list).

## Myth: Operator "++" is Always Bad

The `++` operator has, somewhat undeservedly, got a bad reputation. It probably
has something to do with code like the following, which is the most inefficient
way there is to reverse a list:

_DO NOT_

```erlang
naive_reverse([H|T]) ->
    naive_reverse(T)++[H];
naive_reverse([]) ->
    [].
```

As the `++` operator copies its left operand, the result is copied repeatedly,
leading to quadratic complexity.

But using `++` as follows is not bad:

_OK_

```erlang
naive_but_ok_reverse([H|T], Acc) ->
    naive_but_ok_reverse(T, [H]++Acc);
naive_but_ok_reverse([], Acc) ->
    Acc.
```

Each list element is copied only once. The growing result `Acc` is the right
operand for the `++` operator, and it is _not_ copied.

Experienced Erlang programmers would write as follows:

_DO_

```erlang
vanilla_reverse([H|T], Acc) ->
    vanilla_reverse(T, [H|Acc]);
vanilla_reverse([], Acc) ->
    Acc.
```

This is slightly more efficient because here you do not build a list element
only to copy it directly. (Or it would be more efficient if the compiler did not
automatically rewrite `[H]++Acc` to `[H|Acc]`.)

## Myth: Strings are Slow

String handling can be slow if done improperly. In Erlang, you need to think a
little more about how the strings are used and choose an appropriate
representation. If you use regular expressions, use the `m:re` module in STDLIB
instead of the obsolete `regexp` module.

## Myth: Repairing a Dets File is Very Slow

The repair time is still proportional to the number of records in the file, but
Dets repairs used to be much slower in the past. Dets has been massively
rewritten and improved.

## Myth: BEAM is a Stack-Based Byte-Code Virtual Machine (and Therefore Slow)

BEAM is a register-based virtual machine. It has 1024 virtual registers that are
used for holding temporary values and for passing arguments when calling
functions. Variables that need to survive a function call are saved to the
stack.

BEAM is a threaded-code interpreter. Each instruction is word pointing directly
to executable C-code, making instruction dispatching very fast.

## Myth: Use "\_" to Speed Up Your Program When a Variable is Not Used

That was once true, but from R6B the BEAM compiler can see that a variable is
not used.

Similarly, trivial transformations on the source-code level such as converting a
`case` statement to clauses at the top-level of the function seldom makes any
difference to the generated code.

## Myth: A NIF Always Speeds Up Your Program

Rewriting Erlang code to a NIF to make it faster should be seen as a last
resort. It is only guaranteed to be dangerous, but not guaranteed to speed up
the program.

Doing too much work in each NIF call will
[degrade responsiveness of the VM](`e:erts:erl_nif.md#WARNING`). Doing too
little work may mean that the gain of the faster processing in the NIF is eaten
up by the overhead of calling the NIF and checking the arguments.

Be sure to read about [Long-running NIFs](`e:erts:erl_nif.md#lengthy_work`)
before writing a NIF.
