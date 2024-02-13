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
# Common Caveats

This section lists a few modules and BIFs to watch out for, not only from a
performance point of view.

## Timer Module

Creating timers using `erlang:send_after/3` and `erlang:start_timer/3`, is more
efficient than using the timers provided by the `m:timer` module in STDLIB.

The `timer` module uses a separate process to manage the timers. Before OTP 25,
this management overhead was substantial and increasing with the number of
timers, especially when they were short-lived, so the timer server process could
easily become overloaded and unresponsive. In OTP 25, the timer module was
improved by removing most of the management overhead and the resulting
performance penalty. Still, the timer server remains a single process, and it
may at some point become a bottleneck of an application.

The functions in the `timer` module that do not manage timers (such as
`timer:tc/3` or `timer:sleep/1`), do not call the timer-server process and are
therefore harmless.

## Accidental Copying and Loss of Sharing

When spawning a new process using a fun, one can accidentally copy more data to
the process than intended. For example:

_DO NOT_

```erlang

accidental1(State) ->
    spawn(fun() ->
                  io:format("~p\n", [State#state.info])
          end).
```

The code in the fun will extract one element from the record and print it. The
rest of the `state` record is not used. However, when the [`spawn/1`](`spawn/1`)
function is executed, the entire record is copied to the newly created process.

The same kind of problem can happen with a map:

_DO NOT_

```erlang

accidental2(State) ->
    spawn(fun() ->
                  io:format("~p\n", [map_get(info, State)])
          end).
```

In the following example (part of a module implementing the `m:gen_server`
behavior) the created fun is sent to another process:

_DO NOT_

```erlang

handle_call(give_me_a_fun, _From, State) ->
    Fun = fun() -> State#state.size =:= 42 end,
    {reply, Fun, State}.
```

How bad that unnecessary copy is depends on the contents of the record or the
map.

For example, if the `state` record is initialized like this:

```erlang

init1() ->
    #state{data=lists:seq(1, 10000)}.
```

a list with 10000 elements (or about 20000 heap words) will be copied to the
newly created process.

An unnecessary copy of 10000 element list can be bad enough, but it can get even
worse if the `state` record contains _shared subterms_. Here is a simple example
of a term with a shared subterm:

```erlang
{SubTerm, SubTerm}
```

When a term is copied to another process, sharing of subterms will be lost and
the copied term can be many times larger than the original term. For example:

```erlang

init2() ->
    SharedSubTerms = lists:foldl(fun(_, A) -> [A|A] end, [0], lists:seq(1, 15)),
    #state{data=Shared}.
```

In the process that calls `init2/0`, the size of the `data` field in the `state`
record will be 32 heap words. When the record is copied to the newly created
process, sharing will be lost and the size of the copied `data` field will be
131070 heap words. More details about
[loss off sharing](eff_guide_processes.md#loss-of-sharing) are found in a later
section.

To avoid the problem, outside of the fun extract only the fields of the record
that are actually used:

_DO_

```erlang

fixed_accidental1(State) ->
    Info = State#state.info,
    spawn(fun() ->
                  io:format("~p\n", [Info])
          end).
```

Similarly, outside of the fun extract only the map elements that are actually
used:

_DO_

```erlang

fixed_accidental2(State) ->
    Info = map_get(info, State),
    spawn(fun() ->
                  io:format("~p\n", [Info])
          end).
```

## list_to_atom/1

Atoms are not garbage-collected. Once an atom is created, it is never removed.
The emulator terminates if the limit for the number of atoms (1,048,576 by
default) is reached.

Therefore, converting arbitrary input strings to atoms can be dangerous in a
system that runs continuously. If only certain well-defined atoms are allowed as
input, [list_to_existing_atom/1](`erlang:list_to_existing_atom/1`) can be used
to guard against a denial-of-service attack. (All atoms that are allowed must
have been created earlier, for example, by simply using all of them in a module
and loading that module.)

Using [`list_to_atom/1`](`list_to_atom/1`) to construct an atom that is passed
to [`apply/3`](`apply/3`) as follows, is quite expensive and not recommended in
time-critical code:

```erlang
apply(list_to_atom("some_prefix"++Var), foo, Args)
```

## length/1

The time for calculating the length of a list is proportional to the length of
the list, as opposed to [`tuple_size/1`](`tuple_size/1`),
[`byte_size/1`](`byte_size/1`), and [`bit_size/1`](`bit_size/1`), which all
execute in constant time.

Normally, there is no need to worry about the speed of [`length/1`](`length/1`),
because it is efficiently implemented in C. In time-critical code, you might
want to avoid it if the input list could potentially be very long.

Some uses of [`length/1`](`length/1`) can be replaced by matching. For example,
the following code:

```erlang
foo(L) when length(L) >= 3 ->
    ...
```

can be rewritten to:

```erlang
foo([_,_,_|_]=L) ->
   ...
```

One slight difference is that [`length(L)`](`length/1`) fails if `L` is an
improper list, while the pattern in the second code fragment accepts an improper
list.

## setelement/3

[setelement/3](`erlang:setelement/3`) copies the tuple it modifies. Therefore,
updating a tuple in a loop using [`setelement/3`](`setelement/3`) creates a new
copy of the tuple every time.

There is one exception to the rule that the tuple is copied. If the compiler
clearly can see that destructively updating the tuple would give the same result
as if the tuple was copied, the call to [`setelement/3`](`setelement/3`) is
replaced with a special destructive `setelement` instruction. In the following
code sequence, the first [`setelement/3`](`setelement/3`) call copies the tuple
and modifies the ninth element:

```erlang
multiple_setelement(T0) ->
    T1 = setelement(9, T0, bar),
    T2 = setelement(7, T1, foobar),
    setelement(5, T2, new_value).
```

The two following [`setelement/3`](`setelement/3`) calls modify the tuple in
place.

For the optimization to be applied, _all_ the following conditions must be true:

- The indices must be integer literals, not variables or expressions.
- The indices must be given in descending order.
- There must be no calls to another function in between the calls to
  [`setelement/3`](`setelement/3`).
- The tuple returned from one [`setelement/3`](`setelement/3`) call must only be
  used in the subsequent call to [`setelement/3`](`setelement/3`).

If the code cannot be structured as in the `multiple_setelement/1` example, the
best way to modify multiple elements in a large tuple is to convert the tuple to
a list, modify the list, and convert it back to a tuple.

## size/1

[`size/1`](`size/1`) returns the size for both tuples and binaries.

Using the BIFs [`tuple_size/1`](`tuple_size/1`) and
[`byte_size/1`](`byte_size/1`) gives the compiler and the runtime system more
opportunities for optimization. Another advantage is that the BIFs give Dialyzer
more type information.

## split_binary/2

It is usually more efficient to split a binary using matching instead of calling
the [`split_binary/2`](`split_binary/2`) function. Furthermore, mixing bit
syntax matching and [`split_binary/2`](`split_binary/2`) can prevent some
optimizations of bit syntax matching.

_DO_

```text
        <<Bin1:Num/binary,Bin2/binary>> = Bin,
```

_DO NOT_

```text
        {Bin1,Bin2} = split_binary(Bin, Num)
```
