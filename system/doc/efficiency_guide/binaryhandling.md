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
# Constructing and Matching Binaries

This section gives a few examples on how to handle binaries in an efficient way.
The sections that follow take an in-depth look at how binaries are implemented
and how to best take advantages of the optimizations done by the compiler and
runtime system.

Binaries can be efficiently _built_ in the following way:

_DO_

```erlang
my_list_to_binary(List) ->
    my_list_to_binary(List, <<>>).

my_list_to_binary([H|T], Acc) ->
    my_list_to_binary(T, <<Acc/binary,H>>);
my_list_to_binary([], Acc) ->
    Acc.
```

Appending data to a binary as in the example is efficient because it is
specially optimized by the runtime system to avoid copying the `Acc` binary
every time.

Prepending data to a binary in a loop is not efficient:

_DO NOT_

```erlang
rev_list_to_binary(List) ->
    rev_list_to_binary(List, <<>>).

rev_list_to_binary([H|T], Acc) ->
    rev_list_to_binary(T, <<H,Acc/binary>>);
rev_list_to_binary([], Acc) ->
    Acc.
```

This is not efficient for long lists because the `Acc` binary is copied every
time. One way to make the function more efficient is like this:

_DO NOT_

```erlang
rev_list_to_binary(List) ->
    rev_list_to_binary(lists:reverse(List), <<>>).

rev_list_to_binary([H|T], Acc) ->
    rev_list_to_binary(T, <<Acc/binary,H>>);
rev_list_to_binary([], Acc) ->
    Acc.
```

Another way to avoid copying the binary each time is like this:

_DO_

```erlang
rev_list_to_binary([H|T]) ->
    RevTail = rev_list_to_binary(T),
    <<RevTail/binary,H>>;
rev_list_to_binary([]) ->
    <<>>.
```

Note that in each of the _DO_ examples, the binary to be appended to is always
given as the first segment.

Binaries can be efficiently _matched_ in the following way:

_DO_

```erlang
my_binary_to_list(<<H,T/binary>>) ->
    [H|my_binary_to_list(T)];
my_binary_to_list(<<>>) -> [].
```

## How Binaries are Implemented

Internally, binaries and bitstrings are implemented in the same way. In this
section, they are called _binaries_ because that is what they are called in the
emulator source code.

Four types of binary objects are available internally:

- Two are containers for binary data and are called:

  - _Refc binaries_ (short for _reference-counted binaries_)
  - _Heap binaries_

- Two are merely references to a part of a binary and are called:

  - _sub binaries_
  - _match contexts_

[](){: #refc_binary }

### Refc Binaries

Refc binaries consist of two parts:

- An object stored on the process heap, called a _ProcBin_
- The binary object itself, stored outside all process heaps

The binary object can be referenced by any number of ProcBins from any number of
processes. The object contains a reference counter to keep track of the number
of references, so that it can be removed when the last reference disappears.

All ProcBin objects in a process are part of a linked list, so that the garbage
collector can keep track of them and decrement the reference counters in the
binary when a ProcBin disappears.

[](){: #heap_binary }

### Heap Binaries

Heap binaries are small binaries, up to 64 bytes, and are stored directly on the
process heap. They are copied when the process is garbage-collected and when
they are sent as a message. They do not require any special handling by the
garbage collector.

### Sub Binaries

The reference objects _sub binaries_ and _match contexts_ can reference part of
a refc binary or heap binary.

[](){: #sub_binary } A _sub binary_ is created by
[`split_binary/2`](`split_binary/2`) and when a binary is matched out in a
binary pattern. A sub binary is a reference into a part of another binary (refc
or heap binary, but never into another sub binary). Therefore, matching out a
binary is relatively cheap because the actual binary data is never copied.

[](){: #match_context }

### Match Context

A _match context_ is similar to a sub binary, but is optimized for binary
matching. For example, it contains a direct pointer to the binary data. For each
field that is matched out of a binary, the position in the match context is
incremented.

The compiler tries to avoid generating code that creates a sub binary, only to
shortly afterwards create a new match context and discard the sub binary.
Instead of creating a sub binary, the match context is kept.

The compiler can only do this optimization if it knows that the match context
will not be shared. If it would be shared, the functional properties (also
called referential transparency) of Erlang would break.

## Constructing Binaries

Appending to a binary or bitstring in the following way is specially optimized
to avoid copying the binary:

```erlang
<<Binary/binary, ...>>
%% - OR -
<<Binary/bitstring, ...>>
```

This optimization is applied by the runtime system in a way that makes it
effective in most circumstances (for exceptions, see
[Circumstances That Force Copying](binaryhandling.md#forced_copying)). The
optimization in its basic form does not need any help from the compiler.
However, the compiler add hints to the runtime system when it is safe to apply
the optimization in a more efficient way.

> #### Change {: .info }
>
> The compiler support for making the optimization more efficient was added in
> Erlang/OTP 26.

To explain how the basic optimization works, let us examine the following code
line by line:

```erlang
Bin0 = <<0>>,                    %% 1
Bin1 = <<Bin0/binary,1,2,3>>,    %% 2
Bin2 = <<Bin1/binary,4,5,6>>,    %% 3
Bin3 = <<Bin2/binary,7,8,9>>,    %% 4
Bin4 = <<Bin1/binary,17>>,       %% 5 !!!
{Bin4,Bin3}                      %% 6
```

- Line 1 (marked with the `%% 1` comment), assigns a
  [heap binary](binaryhandling.md#heap_binary) to the `Bin0` variable.
- Line 2 is an append operation. As `Bin0` has not been involved in an append
  operation, a new [refc binary](binaryhandling.md#refc_binary) is created and
  the contents of `Bin0` is copied into it. The _ProcBin_ part of the refc
  binary has its size set to the size of the data stored in the binary, while
  the binary object has extra space allocated. The size of the binary object is
  either twice the size of `Bin1` or 256, whichever is larger. In this case it
  is 256.
- Line 3 is more interesting. `Bin1` _has_ been used in an append operation, and
  it has 252 bytes of unused storage at the end, so the 3 new bytes are stored
  there.
- Line 4. The same applies here. There are 249 bytes left, so there is no
  problem storing another 3 bytes.
- Line 5. Here something _interesting_ happens. Notice that the result is not
  appended to the previous result in `Bin3`, but to `Bin1`. It is expected that
  `Bin4` will be assigned the value `<<0,1,2,3,17>>`. It is also expected that
  `Bin3` will retain its value (`<<0,1,2,3,4,5,6,7,8,9>>`). Clearly, the runtime
  system cannot write byte `17` into the binary, because that would change the
  value of `Bin3` to `<<0,1,2,3,4,17,6,7,8,9>>`.

  To ensure that the value of `Bin3` is retained, the runtime system _copies_
  the contents of `Bin1` to a new refc binary before storing the `17` byte.

  Here is not explained how the runtime system can know that it is not allowed
  to write into `Bin1`; it is left as an exercise to the curious reader to
  figure out how it is done by reading the emulator sources, primarily
  `erl_bits.c`.

### Compiler Support For Constructing Binaries

> #### Change {: .info }
>
> The compiler support for making the optimization more efficient was added in
> Erlang/OTP 26.

In the example in the previous section, it was shown that the runtime system can
handle an append operation to a heap binary by copying it to a refc binary (line
2), and also handle an append operation to a previous version of the binary by
copying it (line 5). The support for doing that does not come for free. For
example, to make it possible to know when it is necessary to copy the binary,
for every append operation, the runtime system must create a sub binary.

When the compiler can determine that none of those situations need to be handled
and that the append operation cannot possibly fail, the compiler generates code
that causes the runtime system to apply a more efficient variant of the
optimization.

**Example:**

```erlang
-module(repack).
-export([repack/1]).

repack(Bin) when is_binary(Bin) ->
    repack(Bin, <<>>).

repack(<<C:8,T/binary>>, Result) ->
    repack(T, <<Result/binary,C:16>>);
repack(<<>>, Result) ->
    Result.
```

The `repack/2` function only keeps a single version of the binary, so there is
never any need to copy the binary. The compiler rewrites the creation of the
empty binary in `repack/1` to instead create a refc binary with 256 bytes
already reserved; thus, the append operation in `repack/2` never needs to handle
a binary not prepared for appending.

[](){: #forced_copying }

### Circumstances That Force Copying

The optimization of the binary append operation requires that there is a
_single_ ProcBin and a _single reference_ to the ProcBin for the binary. The
reason is that the binary object can be moved (reallocated) during an append
operation, and when that happens, the pointer in the ProcBin must be updated. If
there would be more than one ProcBin pointing to the binary object, it would not
be possible to find and update all of them.

Therefore, certain operations on a binary mark it so that any future append
operation will be forced to copy the binary. In most cases, the binary object
will be shrunk at the same time to reclaim the extra space allocated for
growing.

When appending to a binary as follows, only the binary returned from the latest
append operation will support further cheap append operations:

```erlang
Bin = <<Bin0,...>>
```

In the code fragment in the beginning of this section, appending to `Bin` will
be cheap, while appending to `Bin0` will force the creation of a new binary and
copying of the contents of `Bin0`.

If a binary is sent as a message to a process or port, the binary will be shrunk
and any further append operation will copy the binary data into a new binary.
For example, in the following code fragment `Bin1` will be copied in the third
line:

```erlang
Bin1 = <<Bin0,...>>,
PortOrPid ! Bin1,
Bin = <<Bin1,...>>  %% Bin1 will be COPIED
```

The same happens if you insert a binary into an Ets table, send it to a port
using `erlang:port_command/2`, or pass it to
[enif_inspect_binary](`e:erts:erl_nif.md#enif_inspect_binary`) in a NIF.

Matching a binary will also cause it to shrink and the next append operation
will copy the binary data:

```erlang
Bin1 = <<Bin0,...>>,
<<X,Y,Z,T/binary>> = Bin1,
Bin = <<Bin1,...>>  %% Bin1 will be COPIED
```

The reason is that a [match context](binaryhandling.md#match_context) contains a
direct pointer to the binary data.

If a process simply keeps binaries (either in "loop data" or in the process
dictionary), the garbage collector can eventually shrink the binaries. If only
one such binary is kept, it will not be shrunk. If the process later appends to
a binary that has been shrunk, the binary object will be reallocated to make
place for the data to be appended.

## Matching Binaries

Let us revisit the example in the beginning of the previous section:

_DO_

```erlang
my_binary_to_list(<<H,T/binary>>) ->
    [H|my_binary_to_list(T)];
my_binary_to_list(<<>>) -> [].
```

The first time `my_binary_to_list/1` is called, a
[match context](binaryhandling.md#match_context) is created. The match context
points to the first byte of the binary. 1 byte is matched out and the match
context is updated to point to the second byte in the binary.

At this point it would make sense to create a
[sub binary](binaryhandling.md#sub_binary), but in this particular example the
compiler sees that there will soon be a call to a function (in this case, to
`my_binary_to_list/1` itself) that immediately will create a new match context
and discard the sub binary.

Therefore `my_binary_to_list/1` calls itself with the match context instead of
with a sub binary. The instruction that initializes the matching operation
basically does nothing when it sees that it was passed a match context instead
of a binary.

When the end of the binary is reached and the second clause matches, the match
context will simply be discarded (removed in the next garbage collection, as
there is no longer any reference to it).

To summarize, `my_binary_to_list/1` only needs to create _one_ match context and
no sub binaries.

Notice that the match context in `my_binary_to_list/1` was discarded when the
entire binary had been traversed. What happens if the iteration stops before it
has reached the end of the binary? Will the optimization still work?

```erlang
after_zero(<<0,T/binary>>) ->
    T;
after_zero(<<_,T/binary>>) ->
    after_zero(T);
after_zero(<<>>) ->
    <<>>.
```

Yes, it will. The compiler will remove the building of the sub binary in the
second clause:

```erlang
...
after_zero(<<_,T/binary>>) ->
    after_zero(T);
...
```

But it will generate code that builds a sub binary in the first clause:

```erlang
after_zero(<<0,T/binary>>) ->
    T;
...
```

Therefore, `after_zero/1` builds one match context and one sub binary (assuming
it is passed a binary that contains a zero byte).

Code like the following will also be optimized:

```erlang
all_but_zeroes_to_list(Buffer, Acc, 0) ->
    {lists:reverse(Acc),Buffer};
all_but_zeroes_to_list(<<0,T/binary>>, Acc, Remaining) ->
    all_but_zeroes_to_list(T, Acc, Remaining-1);
all_but_zeroes_to_list(<<Byte,T/binary>>, Acc, Remaining) ->
    all_but_zeroes_to_list(T, [Byte|Acc], Remaining-1).
```

The compiler removes building of sub binaries in the second and third clauses,
and it adds an instruction to the first clause that converts `Buffer` from a
match context to a sub binary (or do nothing if `Buffer` is a binary already).

But in more complicated code, how can one know whether the optimization is
applied or not?

[](){: #bin_opt_info }

### Option bin_opt_info

Use the `bin_opt_info` option to have the compiler print a lot of information
about binary optimizations. It can be given either to the compiler or `erlc`:

```erlang
erlc +bin_opt_info Mod.erl
```

or passed through an environment variable:

```erlang
export ERL_COMPILER_OPTIONS=bin_opt_info
```

Notice that the `bin_opt_info` is not meant to be a permanent option added to
your `Makefile`s, because all messages that it generates cannot be eliminated.
Therefore, passing the option through the environment is in most cases the most
practical approach.

The warnings look as follows:

```erlang
./efficiency_guide.erl:60: Warning: NOT OPTIMIZED: binary is returned from the function
./efficiency_guide.erl:62: Warning: OPTIMIZED: match context reused
```

To make it clearer exactly what code the warnings refer to, the warnings in the
following examples are inserted as comments after the clause they refer to, for
example:

```erlang
after_zero(<<0,T/binary>>) ->
         %% BINARY CREATED: binary is returned from the function
    T;
after_zero(<<_,T/binary>>) ->
         %% OPTIMIZED: match context reused
    after_zero(T);
after_zero(<<>>) ->
    <<>>.
```

The warning for the first clause says that the creation of a sub binary cannot
be delayed, because it will be returned. The warning for the second clause says
that a sub binary will not be created (yet).

### Unused Variables

The compiler figures out if a variable is unused. The same code is generated for
each of the following functions:

```erlang
count1(<<_,T/binary>>, Count) -> count1(T, Count+1);
count1(<<>>, Count) -> Count.

count2(<<H,T/binary>>, Count) -> count2(T, Count+1);
count2(<<>>, Count) -> Count.

count3(<<_H,T/binary>>, Count) -> count3(T, Count+1);
count3(<<>>, Count) -> Count.
```

In each iteration, the first 8 bits in the binary will be skipped, not matched
out.

## Historical Note

Binary handling was significantly improved in R12B. Because code that was
efficient in R11B might not be efficient in R12B, and vice versa, earlier
revisions of this Efficiency Guide contained some information about binary
handling in R11B.
