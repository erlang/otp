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
# gen_event Behaviour

[](){: #gen_event }

This section is to be read with the `m:gen_event` manual page in STDLIB, where
all interface functions and callback functions are described in detail.

## Event Handling Principles

In OTP, an _event manager_ is a named object to which events can be sent. An
_event_ can be, for example, an error, an alarm, or some information that is to
be logged.

In the event manager, zero, one, or many _event handlers_ are installed. When
the event manager is notified about an event, the event is processed by all the
installed event handlers. For example, an event manager for handling errors can
by default have a handler installed, which writes error messages to the
terminal. If the error messages during a certain period are to be saved to a
file as well, the user adds another event handler that does this. When logging
to the file is no longer necessary, this event handler is deleted.

An event manager is implemented as a process and each event handler is
implemented as a callback module.

The event manager essentially maintains a list of `{Module, State}` pairs, where
each `Module` is an event handler, and `State` is the internal state of that
event handler.

## Example

The callback module for the event handler writing error messages to the terminal
can look as follows:

```erlang
-module(terminal_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

init(_Args) ->
    {ok, []}.

handle_event(ErrorMsg, State) ->
    io:format("***Error*** ~p~n", [ErrorMsg]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
```

The callback module for the event handler writing error messages to a file can
look as follows:

```erlang
-module(file_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

init(File) ->
    {ok, Fd} = file:open(File, read),
    {ok, Fd}.

handle_event(ErrorMsg, Fd) ->
    io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
    {ok, Fd}.

terminate(_Args, Fd) ->
    file:close(Fd).
```

The code is explained in the next sections.

[](){: #mgr }

## Starting an Event Manager

To start an event manager for handling errors, as described in the previous
example, call the following function:

```text
gen_event:start_link({local, error_man})
```

This function spawns and links to a new process, an event manager.

The argument, `{local, error_man}` specifies the name. The event manager is then
locally registered as `error_man`.

If the name is omitted, the event manager is not registered. Instead its pid
must be used. The name can also be given as `{global, Name}`, in which case the
event manager is registered using `global:register_name/2`.

`gen_event:start_link` must be used if the event manager is part of a
supervision tree, that is, started by a supervisor. There is another function,
`gen_event:start`, to start a standalone event manager, that is, an event
manager that is not part of a supervision tree.

## Adding an Event Handler

The following example shows how to start an event manager and add an event
handler to it by using the shell:

```erlang
1> gen_event:start({local, error_man}).
{ok,<0.31.0>}
2> gen_event:add_handler(error_man, terminal_logger, []).
ok
```

This function sends a message to the event manager registered as `error_man`,
telling it to add the event handler `terminal_logger`. The event manager calls
the callback function `terminal_logger:init([])`, where the argument `[]` is the
third argument to `add_handler`. `init` is expected to return `{ok, State}`,
where `State` is the internal state of the event handler.

```erlang
init(_Args) ->
    {ok, []}.
```

Here, `init` does not need any input data and ignores its argument. For
`terminal_logger`, the internal state is not used. For `file_logger`, the
internal state is used to save the open file descriptor.

```erlang
init(File) ->
    {ok, Fd} = file:open(File, read),
    {ok, Fd}.
```

## Notifying about Events

```text
3> gen_event:notify(error_man, no_reply).
***Error*** no_reply
ok
```

`error_man` is the name of the event manager and `no_reply` is the event.

The event is made into a message and sent to the event manager. When the event
is received, the event manager calls `handle_event(Event, State)` for each
installed event handler, in the same order as they were added. The function is
expected to return a tuple `{ok,State1}`, where `State1` is a new value for the
state of the event handler.

In `terminal_logger`:

```erlang
handle_event(ErrorMsg, State) ->
    io:format("***Error*** ~p~n", [ErrorMsg]),
    {ok, State}.
```

In `file_logger`:

```erlang
handle_event(ErrorMsg, Fd) ->
    io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
    {ok, Fd}.
```

## Deleting an Event Handler

```erlang
4> gen_event:delete_handler(error_man, terminal_logger, []).
ok
```

This function sends a message to the event manager registered as `error_man`,
telling it to delete the event handler `terminal_logger`. The event manager
calls the callback function `terminal_logger:terminate([], State)`, where the
argument `[]` is the third argument to `delete_handler`. `terminate` is to be
the opposite of `init` and do any necessary cleaning up. Its return value is
ignored.

For `terminal_logger`, no cleaning up is necessary:

```erlang
terminate(_Args, _State) ->
    ok.
```

For `file_logger`, the file descriptor opened in `init` must be closed:

```erlang
terminate(_Args, Fd) ->
    file:close(Fd).
```

## Stopping

When an event manager is stopped, it gives each of the installed event handlers
the chance to clean up by calling `terminate/2`, the same way as when deleting a
handler.

### In a Supervision Tree

If the event manager is part of a supervision tree, no stop function is needed.
The event manager is automatically terminated by its supervisor. Exactly how
this is done is defined by a [shutdown strategy](sup_princ.md#shutdown) set in
the supervisor.

### Standalone Event Managers

An event manager can also be stopped by calling:

```text
> gen_event:stop(error_man).
ok
```

## Handling Other Messages

If the `gen_event` is to be able to receive other messages than events, the
callback function `handle_info(Info, State)` must be implemented to handle them.
Examples of other messages are exit messages, if the `gen_event` is linked to
other processes (than the supervisor, for example via `add_sup_handler`) and
trapping exit signals.

```erlang
handle_info({'EXIT', Pid, Reason}, State) ->
    ..code to handle exits here..
    {ok, NewState}.
```

The `code_change` method must also be implemented.

```erlang
code_change(OldVsn, State, Extra) ->
    ..code to convert state (and more) during code change
    {ok, NewState}
```
