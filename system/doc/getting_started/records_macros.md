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
# Records and Macros

Larger programs are usually written as a collection of files with a well-defined
interface between the various parts.

## The Larger Example Divided into Several Files

To illustrate this, the messenger example from the previous section is divided
into the following five files:

- `mess_config.hrl`

  Header file for configuration data

- `mess_interface.hrl`

  Interface definitions between the client and the messenger

- `user_interface.erl`

  Functions for the user interface

- `mess_client.erl`

  Functions for the client side of the messenger

- `mess_server.erl`

  Functions for the server side of the messenger

While doing this, the message passing interface between the shell, the client,
and the server is cleaned up and is defined using _records_. Also, _macros_ are
introduced:

```erlang
%%%----FILE mess_config.hrl----

%%% Configure the location of the server node,
-define(server_node, messenger@super).

%%%----END FILE----
```

```erlang
%%%----FILE mess_interface.hrl----

%%% Message interface between client and server and client shell for
%%% messenger program

%%%Messages from Client to server received in server/1 function.
-record(logon,{client_pid, username}).
-record(message,{client_pid, to_name, message}).
%%% {'EXIT', ClientPid, Reason}  (client terminated or unreachable.

%%% Messages from Server to Client, received in await_result/0 function
-record(abort_client,{message}).
%%% Messages are: user_exists_at_other_node,
%%%               you_are_not_logged_on
-record(server_reply,{message}).
%%% Messages are: logged_on
%%%               receiver_not_found
%%%               sent  (Message has been sent (no guarantee)
%%% Messages from Server to Client received in client/1 function
-record(message_from,{from_name, message}).

%%% Messages from shell to Client received in client/1 function
%%% spawn(mess_client, client, [server_node(), Name])
-record(message_to,{to_name, message}).
%%% logoff

%%%----END FILE----
```

```erlang
%%%----FILE user_interface.erl----

%%% User interface to the messenger program
%%% login(Name)
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

-module(user_interface).
-export([logon/1, logoff/0, message/2]).
-include("mess_interface.hrl").
-include("mess_config.hrl").

logon(Name) ->
    case whereis(mess_client) of
        undefined ->
            register(mess_client,
                     spawn(mess_client, client, [?server_node, Name]));
        _ -> already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> mess_client ! #message_to{to_name=ToName, message=Message},
             ok
end.

%%%----END FILE----
```

```erlang
%%%----FILE mess_client.erl----

%%% The client process which runs on each user node

-module(mess_client).
-export([client/2]).
-include("mess_interface.hrl").

client(Server_Node, Name) ->
    {messenger, Server_Node} ! #logon{client_pid=self(), username=Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            exit(normal);
        #message_to{to_name=ToName, message=Message} ->
            {messenger, Server_Node} !
                #message{client_pid=self(), to_name=ToName, message=Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

%%% wait for a response from the server
await_result() ->
    receive
        #abort_client{message=Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        #server_reply{message=What} ->
            io:format("~p~n", [What])
    after 5000 ->
            io:format("No response from server~n", []),
            exit(timeout)
    end.

%%%----END FILE---
```

```erlang
%%%----FILE mess_server.erl----

%%% This is the server process of the messenger service

-module(mess_server).
-export([start_server/0, server/0]).
-include("mess_interface.hrl").

server() ->
    process_flag(trap_exit, true),
    server([]).

%%% the user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
server(User_List) ->
    io:format("User list = ~p~n", [User_List]),
    receive
        #logon{client_pid=From, username=Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {'EXIT', From, _} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        #message{client_pid=From, to_name=To, message=Message} ->
            server_transfer(From, To, Message, User_List),
            server(User_List)
    end.

%%% Start the server
start_server() ->
    register(messenger, spawn(?MODULE, server, [])).

%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! #abort_client{message=user_exists_at_other_node},
            User_List;
        false ->
            From ! #server_reply{message=logged_on},
            link(From),
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
            From ! #abort_client{message=you_are_not_logged_on};
        {value, {_, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.
%%% If the user exists, send the message
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! #server_reply{message=receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! #message_from{from_name=Name, message=Message},
            From !  #server_reply{message=sent}
    end.

%%%----END FILE---
```

## Header Files

As shown above, some files have extension `.hrl`. These are header files that
are included in the `.erl` files by:

```erlang
-include("File_Name").
```

for example:

```erlang
-include("mess_interface.hrl").
```

In the case above the file is fetched from the same directory as all the other
files in the messenger example. (_manual_).

.hrl files can contain any valid Erlang code but are most often used for record
and macro definitions.

## Records

A record is defined as:

```text
-record(name_of_record,{field_name1, field_name2, field_name3, ......}).
```

For example:

```erlang
-record(message_to,{to_name, message}).
```

This is equivalent to:

```text
{message_to, To_Name, Message}
```

Creating a record is best illustrated by an example:

```text
#message_to{message="hello", to_name=fred)
```

This creates:

```text
{message_to, fred, "hello"}
```

Notice that you do not have to worry about the order you assign values to the
various parts of the records when you create it. The advantage of using records
is that by placing their definitions in header files you can conveniently define
interfaces that are easy to change. For example, if you want to add a new field
to the record, you only have to change the code where the new field is used and
not at every place the record is referred to. If you leave out a field when
creating a record, it gets the value of the atom `undefined`. (_manual_)

Pattern matching with records is very similar to creating records. For example,
inside a `case` or `receive`:

```text
#message_to{to_name=ToName, message=Message} ->
```

This is the same as:

```text
{message_to, ToName, Message}
```

## Macros

Another thing that has been added to the messenger is a macro. The file
`mess_config.hrl` contains the definition:

```erlang
%%% Configure the location of the server node,
-define(server_node, messenger@super).
```

This file is included in `mess_server.erl`:

```erlang
-include("mess_config.hrl").
```

Every occurrence of `?server_node` in `mess_server.erl` is now replaced by
`messenger@super`.

A macro is also used when spawning the server process:

```text
spawn(?MODULE, server, [])
```

This is a standard macro (that is, defined by the system, not by the user).
`?MODULE` is always replaced by the name of the current module (that is, the
`-module` definition near the start of the file). There are more advanced ways
of using macros with, for example, parameters (_manual_).

The three Erlang (`.erl`) files in the messenger example are individually
compiled into object code file (`.beam`). The Erlang system loads and links
these files into the system when they are referred to during execution of the
code. In this case, they are simply put in our current working directory (that
is, the place you have done "cd" to). There are ways of putting the `.beam`
files in other directories.

In the messenger example, no assumptions have been made about what the message
being sent is. It can be any valid Erlang term.
