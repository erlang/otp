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
# Processes

## Creating an Erlang Process

An Erlang process is lightweight compared to threads and processes in operating
systems.

A newly spawned Erlang process uses 327 words of memory. The size can be found
as follows:

```erlang
Erlang/OTP 27 [erts-14.2.3] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V14.2.3 (press Ctrl+G to abort, type help(). for help)
1> Fun = fun() -> receive after infinity -> ok end end.
#Fun<erl_eval.43.39164016>
2> {_,Bytes} = process_info(spawn(Fun), memory).
{memory,2616}
3> Bytes div erlang:system_info(wordsize).
327
```

The size includes 233 words for the heap area (which includes the stack). The
garbage collector increases the heap as needed.

The main (outer) loop for a process _must_ be tail-recursive. Otherwise, the
stack grows until the process terminates.

**DO NOT**

```erlang
loop() ->
  receive
     {sys, Msg} ->
         handle_sys_msg(Msg),
         loop();
     {From, Msg} ->
          Reply = handle_msg(Msg),
          From ! Reply,
          loop()
  end,
  io:format("Message is processed~n", []).
```

The call to `io:format/2` will never be executed, but a return address will
still be pushed to the stack each time `loop/0` is called recursively. The
correct tail-recursive version of the function looks as follows:

**DO**

```erlang
loop() ->
   receive
      {sys, Msg} ->
         handle_sys_msg(Msg),
         loop();
      {From, Msg} ->
         Reply = handle_msg(Msg),
         From ! Reply,
         loop()
 end.
```

### Initial Heap Size

The default initial heap size of 233 words is quite conservative to support
Erlang systems with hundreds of thousands or even millions of processes. The
garbage collector grows and shrinks the heap as needed.

In a system that use comparatively few processes, performance _might_ be
improved by increasing the minimum heap size using either the `+h` option for
[erl](`e:erts:erl_cmd.md`) or on a process-per-process basis using the
`min_heap_size` option for [spawn_opt/4](`erlang:spawn_opt/4`).

The gain is twofold:

- Although the garbage collector grows the heap, it grows it step-by-step, which
  is more costly than directly establishing a larger heap when the process is
  spawned.
- The garbage collector can also shrink the heap if it is much larger than the
  amount of data stored on it; setting the minimum heap size prevents that.

> #### Warning {: .warning }
>
> The runtime system probably uses more memory, and because garbage collections occur
> less frequently, huge binaries can be kept much longer.
>
> This optimization is not to be attempted without proper measurements.

In systems with many processes, computation tasks that run for a short time can
be spawned off into a new process with a higher minimum heap size. When the
process is done, it sends the result of the computation to another process and
terminates. If the minimum heap size is calculated properly, the process might
not have to do any garbage collections at all.

## Sending Messages

All data in messages sent between Erlang processes is copied, except for
[refc binaries](binaryhandling.md#refc_binary) and
[literals](eff_guide_processes.md#literal-pool) on the same Erlang node.

When a message is sent to a process on another Erlang node, it is
first encoded to the [Erlang External Format](`e:erts:erl_ext_dist.md`)
before being sent through a TCP/IP socket. The receiving Erlang node
decodes the message and distributes it to the correct process.

## Receiving messages

The cost of receiving messages depends on how complicated the `receive`
expression is. A simple expression that matches any message is very cheap
because it retrieves the first message in the message queue:

**DO**

```erlang
receive
    Message -> handle_msg(Message)
end.
```

However, this is not always convenient: we can receive a message that we do not
know how to handle at this point, so it is common to only match the messages we
expect:

```erlang
receive
    {Tag, Message} -> handle_msg(Message)
end.
```

While this is convenient it means that the entire message queue must be searched
until it finds a matching message. This is very expensive for processes with
long message queues, so there is an optimization for the common case of
sending a request and waiting for a response shortly after:

**DO**

```erlang
MRef = monitor(process, Process),
Process ! {self(), MRef, Request},
receive
    {MRef, Reply} ->
        erlang:demonitor(MRef, [flush]),
        handle_reply(Reply);
    {'DOWN', MRef, _, _, Reason} ->
        handle_error(Reason)
end.
```

Since the compiler knows that the reference created by
[`monitor/2`](`monitor/2`) cannot exist before the call (since it is a globally
unique identifier), and that the `receive` only matches messages that contain
said reference, it will tell the emulator to search only the messages that
arrived after the call to [`monitor/2`](`monitor/2`).

The above is a simple example where one is but guaranteed that the optimization
will take, but what about more complicated code?

[](){: #recv_opt_info }

### Option recv_opt_info

Use the `recv_opt_info` option to have the compiler print information about
receive optimizations. It can be given either to the compiler or `erlc`:

```erlang
erlc +recv_opt_info Mod.erl
```

or passed through an environment variable:

```erlang
export ERL_COMPILER_OPTIONS=recv_opt_info
```

Notice that `recv_opt_info` is not meant to be a permanent option added to your
`Makefile`s, because all messages that it generates cannot be eliminated.
Therefore, passing the option through the environment is in most cases the most
practical approach.

The warnings look as follows:

```erlang
efficiency_guide.erl:194: Warning: INFO: receive matches any message, this is always fast
efficiency_guide.erl:200: Warning: NOT OPTIMIZED: all clauses do not match a suitable reference
efficiency_guide.erl:206: Warning: OPTIMIZED: reference used to mark a message queue position
efficiency_guide.erl:208: Warning: OPTIMIZED: all clauses match reference created by monitor/2 at efficiency_guide.erl:206
efficiency_guide.erl:219: Warning: INFO: passing reference created by make_ref/0 at efficiency_guide.erl:218
efficiency_guide.erl:222: Warning: OPTIMIZED: all clauses match reference in function parameter 1
```

To make it clearer exactly what code the warnings refer to, the warnings in the
following examples are inserted as comments after the clause they refer to, for
example:

```erlang
%% DO
simple_receive() ->
%% efficiency_guide.erl:194: Warning: INFO: not a selective receive, this is always fast
receive
    Message -> handle_msg(Message)
end.

%% DO NOT, unless Tag is known to be a suitable reference: see
%% cross_function_receive/0 further down.
selective_receive(Tag, Message) ->
%% efficiency_guide.erl:200: Warning: NOT OPTIMIZED: all clauses do not match a suitable reference
receive
    {Tag, Message} -> handle_msg(Message)
end.

%% DO
optimized_receive(Process, Request) ->
%% efficiency_guide.erl:206: Warning: OPTIMIZED: reference used to mark a message queue position
    MRef = monitor(process, Process),
    Process ! {self(), MRef, Request},
    %% efficiency_guide.erl:208: Warning: OPTIMIZED: matches reference created by monitor/2 at efficiency_guide.erl:206
    receive
        {MRef, Reply} ->
        erlang:demonitor(MRef, [flush]),
        handle_reply(Reply);
    {'DOWN', MRef, _, _, Reason} ->
    handle_error(Reason)
    end.

%% DO
cross_function_receive() ->
    %% efficiency_guide.erl:218: Warning: OPTIMIZED: reference used to mark a message queue position
    Ref = make_ref(),
    %% efficiency_guide.erl:219: Warning: INFO: passing reference created by make_ref/0 at efficiency_guide.erl:218
    cross_function_receive(Ref).

cross_function_receive(Ref) ->
    %% efficiency_guide.erl:222: Warning: OPTIMIZED: all clauses match reference in function parameter 1
    receive
        {Ref, Message} -> handle_msg(Message)
    end.
```

## Literal Pool

Constant Erlang terms (hereafter called _literals_) are kept in _literal pools_;
each loaded module has its own pool. The following function does not build the
tuple every time it is called (only to have it discarded the next time the
garbage collector was run), but the tuple is located in the module's literal
pool:

**DO**

```erlang
days_in_month(M) ->
    element(M, {31,28,31,30,31,30,31,31,30,31,30,31}).
```

If a literal, or a term that contains a literal, is inserted into an Ets table,
it is _copied_. The reason is that the module containing the literal can be
unloaded in the future.

When a literal is sent to another process, it is _not_ copied. When a module
holding a literal is unloaded, the literal will be copied to the heap of all
processes that hold references to that literal.

There also exists a global literal pool that is managed by the
`m:persistent_term` module.

By default, 1 GB of virtual address space is reserved for all literal pools (in
BEAM code and persistent terms). The amount of virtual address space reserved
for literals can be changed by using the
[`+MIscs option`](`e:erts:erts_alloc.md#MIscs`) when starting the emulator.

Here is an example how the reserved virtual address space for literals can be
raised to 2 GB (2048 MB):

```text
erl +MIscs 2048
```

## Loss of Sharing

An Erlang term can have shared subterms. Here is a simple example:

```erlang
{SubTerm, SubTerm}
```

Shared subterms are _not_ preserved in the following cases:

- When a term is sent to another process
- When a term is passed as the initial process arguments in the `spawn` call
- When a term is stored in an Ets table

That is an optimization. Most applications do not send messages with shared
subterms.

The following example shows how a shared subterm can be created:

```erlang
kilo_byte() ->
    kilo_byte(10, [42]).

kilo_byte(0, Acc) ->
    Acc;
kilo_byte(N, Acc) ->
    kilo_byte(N-1, [Acc|Acc]).
```

`kilo_byte/1` creates a deep list. If [`list_to_binary/1`](`list_to_binary/1`)
is called, the deep list can be converted to a binary of 1024 bytes:

```text
1> byte_size(list_to_binary(efficiency_guide:kilo_byte())).
1024
```

Using the `erts_debug:size/1` BIF, it can be seen that the deep list only
requires 22 words of heap space:

```erlang
2> erts_debug:size(efficiency_guide:kilo_byte()).
22
```

Using the `erts_debug:flat_size/1` BIF, the size of the deep list can be
calculated if sharing is ignored. It becomes the size of the list when it has
been sent to another process or stored in an Ets table:

```erlang
3> erts_debug:flat_size(efficiency_guide:kilo_byte()).
4094
```

It can be verified that sharing will be lost if the data is inserted into an Ets
table:

```erlang
4> T = ets:new(tab, []).
#Ref<0.1662103692.2407923716.214181>
5> ets:insert(T, {key,efficiency_guide:kilo_byte()}).
true
6> erts_debug:size(element(2, hd(ets:lookup(T, key)))).
4094
7> erts_debug:flat_size(element(2, hd(ets:lookup(T, key)))).
4094
```

When the data has passed through an Ets table, `erts_debug:size/1` and
`erts_debug:flat_size/1` return the same value. Sharing has been lost.

It is possible to build an _experimental_ variant of the runtime system that
will preserve sharing when copying terms by giving the
`--enable-sharing-preserving` option to the `configure` script.

## SMP Run-Time System

The Erlang run-time system takes advantage of a multi-core or
multi-CPU computer by running several Erlang scheduler threads
(typically, the same number of threads as the number of cores).

To gain performance from a multi-core computer, your application _must have more
than one runnable Erlang process_ most of the time. Otherwise, the Erlang
emulator can still only run one Erlang process at the time.

Benchmarks that appear to be concurrent are often sequential.  For
example, the [EStone
benchmark](https://github.com/erlang/otp/blob/f164034e6fdab3316ae23c5d5bbaef258dd6d12c/erts/emulator/test/estone_SUITE.erl)
is entirely sequential. So is the most common implementation of the
"ring benchmark"; usually one process is active, while the others wait
in a `receive` statement.
