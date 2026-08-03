<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2024-2025. All Rights Reserved.

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
# Creating a custom shell

This guide will show how to create a custom shell. The most common
use case for this is to support other languages running on the Erlang VM,
but it can also be used to create specialized debugging shells a system.

This guide will build on top of the built-in [Erlang line editor](`m:edlin`),
which means that the keybindings described in [tty - A Command-Line Interface](`e:erts:tty.md`)
can be used edit the input before it is passed to the custom shell. This
somewhat limits what the custom shell can do, but it also means that we do not
have to implement line editing ourselves. If you need more control over the
shell, then use [Creating a terminal application](terminal_interface.md) as
a starting-point to build your own line editor and shell.

## A process inspection shell

The custom shell that we are going to build is a process inspection shell
that supports the following commands:

* `list` - lists all processes
* `inspect pid()` - inspect a process
* `suspend pid()` - suspend a process
* `resume pid()` - resume a process

Lets get started!

## Starting with a custom shell

The custom shell will be implemented in an `m:escript`, but it could just
as well be in a regular system or as a remote shell. To start a custom shell
we first need to start Erlang in `-noinput` or `-noshell` mode. `m:escript` are
started by default in `-noshell` mode, so we don't have to do anything special here.
To start the custom shell we then call `shell:start_interactive/1`.

```
#!/usr/bin/env escript
%% pshell.es
-export([start/0]).
main(_Args) ->
    shell:start_interactive({?MODULE, start, []}),
    timer:sleep(infinity). %% Make sure the escript does not exit

-spec start() -> pid().
start() ->
    spawn(fun() ->
                  io:format(~"Starting process inspection shell~n"),
                  loop()
          end).

loop() ->
    receive _M -> loop() end.
```

If we run the above we will get this:

```
$ ./pshell.es
Erlang/OTP 28 [DEVELOPMENT] [erts-15.0.1] [source-b395339a02] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Starting process inspection shell

```

The `t:io:standard_io/0` of the created shell process will be set to the
Erlang line editor, which means that we can use the normal `m:io` functions
to read and write data to the terminal.

## Adding our first command

Let's start adding the shell interface. We will use `io:get_line/1` to read from
`t:io:standard_io/0` as this shell will be line based. However, for a more complex
shell it is better to send [`get_until` I/O requests](io_protocol.md#input-requests)
as commands read that way can span multiple lines. So we expand our `loop/0` with
a `io:get_line/1` and pass the results to our parser.

```
loop() ->
    case io:get_line("> ") of
        eof -> ok;
        {error, Reason} -> exit(Reason);
        Data -> eval(string:trim(Data))
    end,
    loop().

eval("list") ->
    Format = " ~.10ts | ~.10ts | ~.10ts~n",
    io:format(Format,["Pid", "Name", "MsgQ Len"]),
    [begin
         [{registered_name,Name},{message_queue_len,Len}]
             = erlang:process_info(Pid, [registered_name, message_queue_len]),
         io:format(Format,[to_list(Pid), to_list(Name), to_list(Len)])
     end || Pid <- processes()];
eval(Unknown) ->
    io:format("Unknown command: '~ts'~n",[Unknown]).

to_list(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_list(List) when is_list(List) ->
    List.
```

If we run the above we will get this:

```txt
$ ./pshell.es
Erlang/OTP 28 [DEVELOPMENT] [erts-15.0.1] [source-b395339a02] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Starting process inspection shell
> list
 Pid        | Name       | MsgQ Len  
 <0.0.0>    | init       | 0         
 <0.1.0>    | erts_code_ | 0         
 <0.2.0>    |            | 0         
 <0.3.0>    |            | 0         
 <0.4.0>    |            | 0         
 <0.5.0>    |            | 0         
 <0.6.0>    |            | 0         
 <0.7.0>    |            | 0         
 <0.8.0>    | socket_reg | 0         
 <0.10.0>   |            | 0         
 <0.11.0>   | erl_prim_l | 0         
 <0.43.0>   | logger     | 0         
 <0.45.0>   | applicatio | 0
...
```

With this all in place we can now easily add `inspect`, `suspend` and `resume` as well.

```
eval("inspect " ++ PidStr) ->
    case parse_pid(PidStr) of
        invalid -> ok;
        Pid ->
            [{registered_name, Name}, {memory, Memory}, {messages, Messages}, {status, Status}] =
                erlang:process_info(Pid, [registered_name, memory, messages, status]),
            io:format("Pid: ~p~nName: ~ts~nStatus: ~p~nMemory: ~p~nMessages: ~p~n",
                      [Pid, to_list(Name), Status, Memory, Messages])
    end;
eval("suspend " ++ PidStr) ->
    case parse_pid(PidStr) of
        invalid -> ok;
        Pid ->
            erlang:suspend_process(Pid),
            io:format("Suspeneded ~ts~n")
    end;
eval("resume " ++ PidStr) ->
    case parse_pid(PidStr) of
        invalid -> ok;
        Pid ->
            erlang:resumne_process(Pid),
            io:format("Resumed ~ts~n")
    end;
```

## Adding autocompletion

Wouldn't it be great if we could add some simple auto-completion for our shell? We can do that
by setting a `m:edlin_expand` fun for our shell. This is done by calling [`io:setopts([{expand_fun, Fun}])`](`io:setopts/2`). The fun that we provide is will receive the reversed current line from
`m:edlin` and is expected to return possible expansions. Let's start by adding a simple fun to
expand our commands.

```
-spec start() -> pid().
start() ->
    spawn(fun() ->
                  io:setopts([{expand_fun, fun expand_fun/1}]),
                  io:format(~"Starting process inspection shell~n"),
                  loop()
          end).

-spec expand_fun(ReverseLine :: string()) -> {yes, string(), list(string())} |
          {no, nil(), nil()}.
expand_fun("") -> %% If line is empty, we list all available commands
    {yes, "", ["list", "inspect", "suspend", "resume"]};
expand_fun(Curr) ->
    expand_fun(lists:reverse(Curr), ["list", "inspect", "suspend", "resume"]).

expand_fun(_Curr, []) ->
    {no, "", []};
expand_fun(Curr, [Cmd | T]) ->
    case lists:prefix(Curr, Cmd) of
        true ->
            %% If Curr is a prefix of Cmd we subtract Curr from Cmd to get the
            %% characters we need to complete with.
            {yes, lists:reverse(lists:reverse(Cmd) -- lists:reverse(Curr)), []};
        false ->
            expand_fun(Curr, T)
    end.
```

With the above code we will get expansions of our commands if we hit `<TAB>` in the shell.
Its possible to make very complex completion algorithms, for example the Erlang shell
has completions based on the function specifications of your code. It is important though that
the shell still feels responsive, so calling out to a LLM model for completion may or may not
be a good idea.

The complete source code for this example can be found [here](assets/pshell.es).