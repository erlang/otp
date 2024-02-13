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
# Overview

[](){: #otp-design-principles }

The _OTP design principles_ define how to structure Erlang code in terms of
processes, modules, and directories.

## Supervision Trees

A basic concept in Erlang/OTP is the _supervision tree_. This is a process
structuring model based on the idea of _workers_ and _supervisors_:

- Workers are processes that perform computations, that is, they do the actual
  work.
- Supervisors are processes that monitor the behaviour of workers. A supervisor
  can restart a worker if something goes wrong.
- The supervision tree is a hierarchical arrangement of code into supervisors
  and workers, which makes it possible to design and program fault-tolerant
  software.

In the following figure, square boxes represents supervisors and circles
represent workers:

[](){: #sup6 }

![Supervision Tree](assets/sup6.gif "Supervision Tree")

## Behaviours

In a supervision tree, many of the processes have similar structures, they
follow similar patterns. For example, the supervisors are similar in structure.
The only difference between them is which child processes they supervise. Many
of the workers are servers in a server-client relation, finite-state machines,
or event handlers.

_Behaviours_ are formalizations of these common patterns. The idea is to divide
the code for a process in a generic part (a behaviour module) and a specific
part (a _callback module_).

The behaviour module is part of Erlang/OTP. To implement a process such as a
supervisor, the user only has to implement the callback module which is to
export a pre-defined set of functions, the _callback functions_.

The following example illustrate how code can be divided into a generic and a
specific part. Consider the following code (written in plain Erlang) for a
simple server, which keeps track of a number of "channels". Other processes can
allocate and free the channels by calling the functions `alloc/0` and `free/1`,
respectively.

[](){: #ch1 }

```erlang
-module(ch1).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0]).

start() ->
    spawn(ch1, init, []).

alloc() ->
    ch1 ! {self(), alloc},
    receive
        {ch1, Res} ->
            Res
    end.

free(Ch) ->
    ch1 ! {free, Ch},
    ok.

init() ->
    register(ch1, self()),
    Chs = channels(),
    loop(Chs).

loop(Chs) ->
    receive
        {From, alloc} ->
            {Ch, Chs2} = alloc(Chs),
            From ! {ch1, Ch},
            loop(Chs2);
        {free, Ch} ->
            Chs2 = free(Ch, Chs),
            loop(Chs2)
    end.
```

The code for the server can be rewritten into a generic part `server.erl`:

```erlang
-module(server).
-export([start/1]).
-export([call/2, cast/2]).
-export([init/1]).

start(Mod) ->
    spawn(server, init, [Mod]).

call(Name, Req) ->
    Name ! {call, self(), Req},
    receive
        {Name, Res} ->
            Res
    end.

cast(Name, Req) ->
    Name ! {cast, Req},
    ok.

init(Mod) ->
    register(Mod, self()),
    State = Mod:init(),
    loop(Mod, State).

loop(Mod, State) ->
    receive
        {call, From, Req} ->
            {Res, State2} = Mod:handle_call(Req, State),
            From ! {Mod, Res},
            loop(Mod, State2);
        {cast, Req} ->
            State2 = Mod:handle_cast(Req, State),
            loop(Mod, State2)
    end.
```

And a callback module `ch2.erl`:

```erlang
-module(ch2).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0, handle_call/2, handle_cast/2]).

start() ->
    server:start(ch2).

alloc() ->
    server:call(ch2, alloc).

free(Ch) ->
    server:cast(ch2, {free, Ch}).

init() ->
    channels().

handle_call(alloc, Chs) ->
    alloc(Chs). % => {Ch,Chs2}

handle_cast({free, Ch}, Chs) ->
    free(Ch, Chs). % => Chs2
```

Notice the following:

- The code in `server` can be reused to build many different servers.
- The server name, in this example the atom `ch2`, is hidden from the users of
  the client functions. This means that the name can be changed without
  affecting them.
- The protocol (messages sent to and received from the server) is also hidden.
  This is good programming practice and allows one to change the protocol
  without changing the code using the interface functions.
- The functionality of `server` can be extended without having to change `ch2`
  or any other callback module.

In `ch1.erl` and `ch2.erl` above, the implementation of `channels/0`, `alloc/1`,
and `free/2` has been intentionally left out, as it is not relevant to the
example. For completeness, one way to write these functions is given below. This
is an example only, a realistic implementation must be able to handle situations
like running out of channels to allocate, and so on.

```erlang
channels() ->
   {_Allocated = [], _Free = lists:seq(1,100)}.

alloc({Allocated, [H|T] = _Free}) ->
   {H, {[H|Allocated], T}}.

free(Ch, {Alloc, Free} = Channels) ->
   case lists:member(Ch, Alloc) of
      true ->
         {lists:delete(Ch, Alloc), [Ch|Free]};
      false ->
         Channels
   end.
```

Code written without using behaviours can be more efficient, but the increased
efficiency is at the expense of generality. The ability to manage all
applications in the system in a consistent manner is important.

Using behaviours also makes it easier to read and understand code written by
other programmers. Improvised programming structures, while possibly more
efficient, are always more difficult to understand.

The `server` module corresponds, greatly simplified, to the Erlang/OTP behaviour
`gen_server`.

The standard Erlang/OTP behaviours are:

- [gen_server](gen_server_concepts.md)

  For implementing the server of a client-server relation

- [gen_statem](statem.md)

  For implementing state machines

- [gen_event](events.md)

  For implementing event handling functionality

- [supervisor](sup_princ.md)

  For implementing a supervisor in a supervision tree

The compiler understands the module attribute `-behaviour(Behaviour)` and issues
warnings about missing callback functions, for example:

```erlang
-module(chs3).
-behaviour(gen_server).
...

3> c(chs3).
./chs3.erl:10: Warning: undefined call-back function handle_call/3
{ok,chs3}
```

## Applications

Erlang/OTP comes with a number of components, each implementing some specific
functionality. Components are with Erlang/OTP terminology called _applications_.
Examples of Erlang/OTP applications are Mnesia, which has everything needed for
programming database services, and Debugger, which is used to debug Erlang
programs. The minimal system based on Erlang/OTP consists of the following two
applications:

- Kernel - Functionality necessary to run Erlang
- STDLIB - Erlang standard libraries

The application concept applies both to program structure (processes) and
directory structure (modules).

The simplest applications do not have any processes, but consist of a collection
of functional modules. Such an application is called a _library application_. An
example of a library application is STDLIB.

An application with processes is easiest implemented as a supervision tree using
the standard behaviours.

How to program applications is described in [Applications](applications.md).

## Releases

A _release_ is a complete system made out from a subset of Erlang/OTP
applications and a set of user-specific applications.

How to program releases is described in [Releases](release_structure.md).

How to install a release in a target environment is described in the section
about target systems in Section 2 System Principles.

## Release Handling

_Release handling_ is upgrading and downgrading between different versions of a
release, in a (possibly) running system. How to do this is described in
[Release Handling](release_handling.md).
