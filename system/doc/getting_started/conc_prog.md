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
# Concurrent Programming

## Processes

One of the main reasons for using Erlang instead of other functional languages
is Erlang's ability to handle concurrency and distributed programming. By
concurrency is meant programs that can handle several threads of execution at
the same time. For example, modern operating systems allow you to use a word
processor, a spreadsheet, a mail client, and a print job all running at the same
time. Each processor (CPU) in the system is probably only handling one thread
(or job) at a time, but it swaps between the jobs at such a rate that it gives
the illusion of running them all at the same time. It is easy to create parallel
threads of execution in an Erlang program and to allow these threads to
communicate with each other. In Erlang, each thread of execution is called a
_process_.

(Aside: the term "process" is usually used when the threads of execution share
no data with each other and the term "thread" when they share data in some way.
Threads of execution in Erlang share no data, that is why they are called
processes).

The Erlang BIF `spawn` is used to create a new process:
`spawn(Module, Exported_Function, List of Arguments)`. Consider the following
module:

```erlang
-module(tut14).

-export([start/0, say_something/2]).

say_something(What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(tut14, say_something, [hello, 3]),
    spawn(tut14, say_something, [goodbye, 3]).
```

```erlang
5> c(tut14).
{ok,tut14}
6> tut14:say_something(hello, 3).
hello
hello
hello
done
```

As shown, the function `say_something` writes its first argument the number of
times specified by second argument. The function `start` starts two Erlang
processes, one that writes "hello" three times and one that writes "goodbye"
three times. Both processes use the function `say_something`. Notice that a
function used in this way by `spawn`, to start a process, must be exported from
the module (that is, in the `-export` at the start of the module).

```erlang
9> tut14:start().
hello
goodbye
<0.63.0>
hello
goodbye
hello
goodbye
```

Notice that it did not write "hello" three times and then "goodbye" three times.
Instead, the first process wrote a "hello", the second a "goodbye", the first
another "hello" and so forth. But where did the `<0.63.0>` come from? The return
value of a function is the return value of the last "thing" in the function. The
last thing in the function `start` is

```erlang
spawn(tut14, say_something, [goodbye, 3]).
```

`spawn` returns a _process identifier_, or _pid_, which uniquely identifies the
process. So `<0.63.0>` is the pid of the `spawn` function call above. The next
example shows how to use pids.

Notice also that ~p is used instead of ~w in `io:format/2`. To quote [the manual](`m:io#tilde_p`):

> ~p Writes the data with standard syntax in the same way as ~w, but breaks terms
> whose printed representation is longer than one line into many lines and indents
> each line sensibly. It also tries to detect flat lists of printable characters and
> to output these as strings

## Message Passing

In the following example two processes are created and they send messages to
each other a number of times.

```erlang
-module(tut15).

-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    Pong_PID = spawn(tut15, pong, []),
    spawn(tut15, ping, [3, Pong_PID]).
```

```erlang
1> c(tut15).
{ok,tut15}
2> tut15: start().
<0.36.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
ping finished
Pong finished
```

The function `start` first creates a process, let us call it "pong":

```erlang
Pong_PID = spawn(tut15, pong, [])
```

This process executes `tut15:pong()`. `Pong_PID` is the process identity of the
"pong" process. The function `start` now creates another process "ping":

```erlang
spawn(tut15, ping, [3, Pong_PID]),
```

This process executes:

```erlang
tut15:ping(3, Pong_PID)
```

`<0.36.0>` is the return value from the `start` function.

The process "pong" now does:

```erlang
receive
    finished ->
        io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
        io:format("Pong received ping~n", []),
        Ping_PID ! pong,
        pong()
end.
```

The `receive` construct is used to allow processes to wait for messages from
other processes. It has the following format:

```erlang
receive
   pattern1 ->
       actions1;
   pattern2 ->
       actions2;
   ....
   patternN
       actionsN
end.
```

Notice there is no ";" before the `end`.

Messages between Erlang processes are simply valid Erlang terms. That is, they
can be lists, tuples, integers, atoms, pids, and so on.

Each process has its own input queue for messages it receives. New messages
received are put at the end of the queue. When a process executes a `receive`,
the first message in the queue is matched against the first pattern in the
`receive`. If this matches, the message is removed from the queue and the
actions corresponding to the pattern are executed.

However, if the first pattern does not match, the second pattern is tested. If
this matches, the message is removed from the queue and the actions
corresponding to the second pattern are executed. If the second pattern does not
match, the third is tried and so on until there are no more patterns to test. If
there are no more patterns to test, the first message is kept in the queue and
the second message is tried instead. If this matches any pattern, the
appropriate actions are executed and the second message is removed from the
queue (keeping the first message and any other messages in the queue). If the
second message does not match, the third message is tried, and so on, until the
end of the queue is reached. If the end of the queue is reached, the process
blocks (stops execution) and waits until a new message is received and this
procedure is repeated.

The Erlang implementation is "clever" and minimizes the number of times each
message is tested against the patterns in each `receive`.

Now back to the ping pong example.

"Pong" is waiting for messages. If the atom `finished` is received, "pong"
writes "Pong finished" to the output and, as it has nothing more to do,
terminates. If it receives a message with the format:

```erlang
{ping, Ping_PID}
```

it writes "Pong received ping" to the output and sends the atom `pong` to the
process "ping":

```erlang
Ping_PID ! pong
```

Notice how the operator "\!" is used to send messages. The syntax of "\!" is:

```erlang
Pid ! Message
```

That is, `Message` (any Erlang term) is sent to the process with identity `Pid`.

After sending the message `pong` to the process "ping", "pong" calls the `pong`
function again, which causes it to get back to the `receive` again and wait for
another message.

Now let us look at the process "ping". Recall that it was started by executing:

```erlang
tut15:ping(3, Pong_PID)
```

Looking at the function `ping/2`, the second clause of `ping/2` is executed
since the value of the first argument is 3 (not 0) (first clause head is
`ping(0,Pong_PID)`, second clause head is `ping(N,Pong_PID)`, so `N` becomes 3).

The second clause sends a message to "pong":

```erlang
Pong_PID ! {ping, self()},
```

`self/0` returns the pid of the process that executes `self/0`, in this case the
pid of "ping". (Recall the code for "pong", this lands up in the variable
`Ping_PID` in the `receive` previously explained.)

"Ping" now waits for a reply from "pong":

```erlang
receive
    pong ->
        io:format("Ping received pong~n", [])
end,
```

It writes "Ping received pong" when this reply arrives, after which "ping" calls
the `ping` function again.

```erlang
ping(N - 1, Pong_PID)
```

`N-1` causes the first argument to be decremented until it becomes 0. When this
occurs, the first clause of `ping/2` is executed:

```erlang
ping(0, Pong_PID) ->
    Pong_PID !  finished,
    io:format("ping finished~n", []);
```

The atom `finished` is sent to "pong" (causing it to terminate as described
above) and "ping finished" is written to the output. "Ping" then terminates as
it has nothing left to do.

## Registered Process Names

In the above example, "pong" was first created to be able to give the identity
of "pong" when "ping" was started. That is, in some way "ping" must be able to
know the identity of "pong" to be able to send a message to it. Sometimes
processes which need to know each other's identities are started independently
of each other. Erlang thus provides a mechanism for processes to be given names
so that these names can be used as identities instead of pids. This is done by
using the `register` BIF:

```erlang
register(some_atom, Pid)
```

Let us now rewrite the ping pong example using this and give the name `pong` to
the "pong" process:

```erlang
-module(tut16).

-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
    io:format("ping finished~n", []);

ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    register(pong, spawn(tut16, pong, [])),
    spawn(tut16, ping, [3]).
```

```erlang
2> c(tut16).
{ok, tut16}
3> tut16:start().
<0.38.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
ping finished
Pong finished
```

Here the `start/0` function,

```erlang
register(pong, spawn(tut16, pong, [])),
```

both spawns the "pong" process and gives it the name `pong`. In the "ping"
process, messages can be sent to `pong` by:

```erlang
pong ! {ping, self()},
```

`ping/2` now becomes `ping/1` as the argument `Pong_PID` is not needed.

## Distributed Programming

Let us rewrite the ping pong program with "ping" and "pong" on different
computers. First a few things are needed to set up to get this to work. The
distributed Erlang implementation provides a very basic authentication mechanism
to prevent unintentional access to an Erlang system on another computer. Erlang
systems which talk to each other must have the same _magic cookie_. The easiest
way to achieve this is by having a file called `.erlang.cookie` in your home
directory on all machines on which you are going to run Erlang systems
communicating with each other:

- On Windows systems the home directory is the directory pointed out by the
  environment variable $HOME - you may need to set this.
- On Linux or UNIX you can safely ignore this and simply create a file called
  `.erlang.cookie` in the directory you get to after executing the command `cd`
  without any argument.

The `.erlang.cookie` file is to contain a line with the same atom. For example,
on Linux or UNIX, in the OS shell:

```text
$ cd
$ cat > .erlang.cookie
this_is_very_secret
$ chmod 400 .erlang.cookie
```

The `chmod` above makes the `.erlang.cookie` file accessible only by the owner
of the file. This is a requirement.

When you start an Erlang system that is going to talk to other Erlang systems,
you must give it a name, for example:

```text
$ erl -sname my_name
```

We will see more details of this later. If you want to experiment with
distributed Erlang, but you only have one computer to work on, you can start two
separate Erlang systems on the same computer but give them different names. Each
Erlang system running on a computer is called an _Erlang node_.

(Note: `erl -sname` assumes that all nodes are in the same IP domain and we can
use only the first component of the IP address, if we want to use nodes in
different domains we use `-name` instead, but then all IP address must be given
in full.)

Here is the ping pong example modified to run on two separate nodes:

```erlang
-module(tut17).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start_pong() ->
    register(pong, spawn(tut17, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut17, ping, [3, Pong_Node]).
```

Let us assume there are two computers called gollum and kosken. First a node is
started on kosken, called ping, and then a node on gollum, called pong.

On kosken (on a Linux/UNIX system):

```text
kosken> erl -sname ping
Erlang (BEAM) emulator version 5.2.3.7 [hipe] [threads:0]

Eshell V5.2.3.7  (abort with ^G)
(ping@kosken)1>
```

On gollum:

```text
gollum> erl -sname pong
Erlang (BEAM) emulator version 5.2.3.7 [hipe] [threads:0]

Eshell V5.2.3.7  (abort with ^G)
(pong@gollum)1>
```

Now the "pong" process on gollum is started:

```erlang
(pong@gollum)1> tut17:start_pong().
true
```

And the "ping" process on kosken is started (from the code above you can see
that a parameter of the `start_ping` function is the node name of the Erlang
system where "pong" is running):

```erlang
(ping@kosken)1> tut17:start_ping(pong@gollum).
<0.37.0>
Ping received pong
Ping received pong
Ping received pong
ping finished
```

As shown, the ping pong program has run. On the "pong" side:

```erlang
(pong@gollum)2> 
Pong received ping
Pong received ping
Pong received ping
Pong finished
(pong@gollum)2> 
```

Looking at the `tut17` code, you see that the `pong` function itself is
unchanged, the following lines work in the same way irrespective of on which
node the "ping" process is executes:

```erlang
{ping, Ping_PID} ->
    io:format("Pong received ping~n", []),
    Ping_PID ! pong,
```

Thus, Erlang pids contain information about where the process executes. So if
you know the pid of a process, the `!` operator can be used to send it a
message disregarding if the process is on the same node or on a different node.

A difference is how messages are sent to a registered process on another node:

```erlang
{pong, Pong_Node} ! {ping, self()},
```

A tuple `{registered_name,node_name}` is used instead of just the
`registered_name`.

In the previous example, "ping" and "pong" were started from the shells of two
separate Erlang nodes. `spawn` can also be used to start processes in other
nodes.

The next example is the ping pong program, yet again, but this time "ping" is
started in another node:

```erlang
-module(tut18).

-export([start/1,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start(Ping_Node) ->
    register(pong, spawn(tut18, pong, [])),
    spawn(Ping_Node, tut18, ping, [3, node()]).
```

Assuming an Erlang system called ping (but not the "ping" process) has already
been started on kosken, then on gollum this is done:

```erlang
(pong@gollum)1> tut18:start(ping@kosken).
<3934.39.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong finished
ping finished
```

Notice that all the output is received on gollum. This is because the I/O system
finds out where the process is spawned from and sends all output there.

## A Larger Example

Now for a larger example with a simple "messenger". The messenger is a program
that allows users to log in on different nodes and send simple messages to each
other.

Before starting, notice the following:

- This example only shows the message passing logic - no attempt has been made
  to provide a nice graphical user interface, although this can also be done in
  Erlang.
- This sort of problem can be solved easier by use of the facilities in OTP,
  which also provide methods for updating code on the fly and so on (see
  [OTP Design Principles](`e:system:design_principles.md`)).
- The first program contains some inadequacies regarding handling of nodes which
  disappear. These are corrected in a later version of the program.

The messenger is set up by allowing "clients" to connect to a central server and
say who and where they are. That is, a user does not need to know the name of
the Erlang node where another user is located to send a message.

File `messenger.erl`:

[](){: #ex }

```erlang
%%% Message passing utility.
%%% User interface:
%%% logon(Name)
%%%     One user at a time can log in from each Erlang node in the
%%%     system messenger: and choose a suitable Name. If the Name
%%%     is already logged in at another node or if someone else is
%%%     already logged in at the same node, login will be rejected
%%%     with a suitable error message.
%%% logoff()
%%%     Logs off anybody at that node
%%% message(ToName, Message)
%%%     sends Message to ToName. Error messages if the user of this
%%%     function is not logged on or if ToName is not logged on at
%%%     any node.
%%%
%%% One node in the network of Erlang nodes runs a server which maintains
%%% data about the logged on users. The server is registered as "messenger"
%%% Each node where there is a user logged on runs a client process registered
%%% as "mess_client"
%%%
%%% Protocol between the client processes and the server
%%% ----------------------------------------------------
%%%
%%% To server: {ClientPid, logon, UserName}
%%% Reply {messenger, stop, user_exists_at_other_node} stops the client
%%% Reply {messenger, logged_on} logon was successful
%%%
%%% To server: {ClientPid, logoff}
%%% Reply: {messenger, logged_off}
%%%
%%% To server: {ClientPid, logoff}
%%% Reply: no reply
%%%
%%% To server: {ClientPid, message_to, ToName, Message} send a message
%%% Reply: {messenger, stop, you_are_not_logged_on} stops the client
%%% Reply: {messenger, receiver_not_found} no user with this name logged on
%%% Reply: {messenger, sent} Message has been sent (but no guarantee)
%%%
%%% To client: {message_from, Name, Message},
%%%
%%% Protocol between the "commands" and the client
%%% ----------------------------------------------
%%%
%%% Started: messenger:client(Server_Node, Name)
%%% To client: logoff
%%% To client: {message_to, ToName, Message}
%%%
%%% Configuration: change the server_node() function to return the
%%% name of the node where the messenger server runs

-module(messenger).
-export([start_server/0, server/1, logon/1, logoff/0, message/2, client/2]).

%%% Change the function below to return the name of the node where the
%%% messenger server runs
server_node() ->
    messenger@super.

%%% This is the server process for the "messenger"
%%% the user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {From, logoff} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List)
    end.

%%% Start the server
start_server() ->
    register(messenger, spawn(messenger, server, [[]])).


%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},  %reject logon
            User_List;
        false ->
            From ! {messenger, logged_on},
            [{From, Name} | User_List]        %add user to the list
    end.

%%% Server deletes a user from the user list
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).


%%% Server transfers a message between user
server_transfer(From, To, Message, User_List) ->
    %% check that the user is logged on and who he is
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {messenger, stop, you_are_not_logged_on};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.
%%% If the user exists, send the message
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {message_from, Name, Message},
            From ! {messenger, sent}
    end.


%%% User Commands
logon(Name) ->
    case whereis(mess_client) of
        undefined ->
            register(mess_client,
                     spawn(messenger, client, [server_node(), Name]));
        _ -> already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message},
             ok
end.


%%% The client process which runs on each server node
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

%%% wait for a response from the server
await_result() ->
    receive
        {messenger, stop, Why} -> % Stop the client
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->  % Normal response
            io:format("~p~n", [What])
    end.
```

To use this program, you need to:

- Configure the `server_node()` function.
- Copy the compiled code (`messenger.beam`) to the directory on each computer
  where you start Erlang.

In the following example using this program, nodes are started on four different
computers. If you do not have that many machines available on your network, you
can start several nodes on the same machine.

Four Erlang nodes are started up: messenger@super, c1@bilbo, c2@kosken,
c3@gollum.

First the server at messenger@super is started up:

```erlang
(messenger@super)1> messenger:start_server().
true
```

Now Peter logs on at c1@bilbo:

```erlang
(c1@bilbo)1> messenger:logon(peter).
true
logged_on
```

James logs on at c2@kosken:

```erlang
(c2@kosken)1> messenger:logon(james).
true
logged_on
```

And Fred logs on at c3@gollum:

```erlang
(c3@gollum)1> messenger:logon(fred).
true
logged_on
```

Now Peter sends Fred a message:

```erlang
(c1@bilbo)2> messenger:message(fred, "hello").
ok
sent
```

Fred receives the message and sends a message to Peter and logs off:

```erlang
Message from peter: "hello"
(c3@gollum)2> messenger:message(peter, "go away, I'm busy").
ok
sent
(c3@gollum)3> messenger:logoff().
logoff
```

James now tries to send a message to Fred:

```erlang
(c2@kosken)2> messenger:message(fred, "peter doesn't like you").
ok
receiver_not_found
```

But this fails as Fred has already logged off.

First let us look at some of the new concepts that have been introduced.

There are two versions of the `server_transfer` function: one with four
arguments (`server_transfer/4`) and one with five (`server_transfer/5`). These
are regarded by Erlang as two separate functions.

Notice how to write the `server` function so that it calls itself, through
`server(User_List)`, and thus creates a loop. The Erlang compiler is "clever"
and optimizes the code so that this really is a sort of loop and not a proper
function call. But this only works if there is no code after the call.
Otherwise, the compiler expects the call to return and make a proper function
call. This would result in the process getting bigger and bigger for every loop.

Functions in the `lists` module are used. This is a very useful module and a
study of the manual page is recommended (`erl -man lists`).
`lists:keymember(Key,Position,Lists)` looks through a list of tuples and looks
at `Position` in each tuple to see if it is the same as `Key`. The first element
is position 1. If it finds a tuple where the element at `Position` is the same
as `Key`, it returns `true`, otherwise `false`.

```erlang
3> lists:keymember(a, 2, [{x,y,z},{b,b,b},{b,a,c},{q,r,s}]).
true
4> lists:keymember(p, 2, [{x,y,z},{b,b,b},{b,a,c},{q,r,s}]).
false
```

`lists:keydelete` works in the same way but deletes the first tuple found (if
any) and returns the remaining list:

```erlang
5> lists:keydelete(a, 2, [{x,y,z},{b,b,b},{b,a,c},{q,r,s}]).
[{x,y,z},{b,b,b},{q,r,s}]
```

`lists:keysearch` is like `lists:keymember`, but it returns
`{value,Tuple_Found}` or the atom `false`.

There are many very useful functions in the `lists` module.

An Erlang process (conceptually) runs until it does a `receive` and there is no
message which it wants to receive in the message queue. "conceptually" is used
here because the Erlang system shares the CPU time between the active processes
in the system.

A process terminates when there is nothing more for it to do, that is, the last
function it calls simply returns and does not call another function. Another way
for a process to terminate is for it to call [`exit/1`](`exit/1`). The argument
to [`exit/1`](`exit/1`) has a special meaning, which is discussed later. In this
example, [`exit(normal)`](`exit/1`) is done, which has the same effect as a
process running out of functions to call.

The BIF [`whereis(RegisteredName)`](`whereis/1`) checks if a registered process
of name `RegisteredName` exists. If it exists, the pid of that process is
returned. If it does not exist, the atom `undefined` is returned.

You should by now be able to understand most of the code in the
messenger-module. Let us study one case in detail: a message is sent from one
user to another.

The first user "sends" the message in the example above by:

```erlang
messenger:message(fred, "hello")
```

After testing that the client process exists:

```erlang
whereis(mess_client)
```

And a message is sent to `mess_client`:

```erlang
mess_client ! {message_to, fred, "hello"}
```

The client sends the message to the server by:

```erlang
{messenger, messenger@super} ! {self(), message_to, fred, "hello"},
```

And waits for a reply from the server.

The server receives this message and calls:

```erlang
server_transfer(From, fred, "hello", User_List),
```

This checks that the pid `From` is in the `User_List`:

```erlang
lists:keysearch(From, 1, User_List)
```

If `keysearch` returns the atom `false`, some error has occurred and the server
sends back the message:

```erlang
From ! {messenger, stop, you_are_not_logged_on}
```

This is received by the client, which in turn does [`exit(normal)`](`exit/1`)
and terminates. If `keysearch` returns `{value,{From,Name}}` it is certain that
the user is logged on and that his name (peter) is in variable `Name`.

Let us now call:

```erlang
server_transfer(From, peter, fred, "hello", User_List)
```

Notice that as this is `server_transfer/5`, it is not the same as the previous
function `server_transfer/4`. Another `keysearch` is done on `User_List` to find
the pid of the client corresponding to fred:

```erlang
lists:keysearch(fred, 2, User_List)
```

This time argument 2 is used, which is the second element in the tuple. If this
returns the atom `false`, fred is not logged on and the following message is
sent:

```erlang
From ! {messenger, receiver_not_found};
```

This is received by the client.

If `keysearch` returns:

```erlang
{value, {ToPid, fred}}
```

The following message is sent to fred's client:

```erlang
ToPid ! {message_from, peter, "hello"},
```

The following message is sent to peter's client:

```erlang
From ! {messenger, sent}
```

Fred's client receives the message and prints it:

```erlang
{message_from, peter, "hello"} ->
    io:format("Message from ~p: ~p~n", [peter, "hello"])
```

Peter's client receives the message in the `await_result` function.
