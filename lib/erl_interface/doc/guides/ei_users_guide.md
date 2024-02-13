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
# Erl_Interface User's Guide

## Introduction

The `Erl_Interface` library contains functions that help you integrate programs
written in C and Erlang. The functions in `Erl_Interface` support the following:

- Manipulation of data represented as Erlang data types
- Conversion of data between C and Erlang formats
- Encoding and decoding of Erlang data types for transmission or storage
- Communication between C nodes and Erlang processes
- Backup and restore of C node state to and from [Mnesia](`m:mnesia`)

> #### Note {: .info }
>
> By default, the `Erl_Interface` library is only guaranteed to be compatible
> with other Erlang/OTP components from the same release as the libraries
> themselves. For information about how to communicate with Erlang/OTP
> components from earlier releases, see function
> [`ei_set_compat_rel`](ei.md#ei_set_compat_rel).

### Scope

In the following sections, these topics are described:

- Compiling your code for use with `Erl_Interface`
- Initializing `Erl_Interface`
- Encoding, decoding, and sending Erlang terms
- Building terms and patterns
- Pattern matching
- Connecting to a distributed Erlang node
- Using the Erlang Port Mapper Daemon (EPMD)
- Sending and receiving Erlang messages
- Remote procedure calls
- Using global names

### Prerequisites

It is assumed that the reader is familiar with the Erlang programming language.

## Compiling and Linking Your Code

To use any of the `Erl_Interface` functions, include the following line in your
code:

```text
#include "ei.h"
```

Determine where the top directory of your OTP installation is. To find this,
start Erlang and enter the following command at the Eshell prompt:

```text
Eshell V4.7.4  (abort with ^G)
1> code:root_dir().
/usr/local/otp
```

To compile your code, ensure that your C compiler knows where to find `ei.h` by
specifying an appropriate `-I` argument on the command line, or add it to the
`CFLAGS` definition in your `Makefile`. The correct value for this path is
`$OTPROOT/lib/erl_interface-$EIVSN/include`, where:

- `$OTPROOT` is the path reported by `code:root_dir/0` in the example above.
- `$EIVSN` is the version of the `Erl_Interface` application, for example,
  `erl_interface-3.2.3`.

Compiling the code:

```text
$ cc -c -I/usr/local/otp/lib/erl_interface-3.2.3/include myprog.c
```

When linking:

- Specify the path to `libei.a` with `-L$OTPROOT/lib/erl_interface-3.2.3/lib`.
- Specify the name of the library with `-lei`.

Do this on the command line or add the flags to the `LDFLAGS` definition in your
`Makefile`.

Linking the code:

```text
$ ld -L/usr/local/otp/lib/erl_interface-3.2.3/
                            lib myprog.o -lei -o myprog
```

On some systems it can be necessary to link with some more libraries (for
example, `libnsl.a` and `libsocket.a` on Solaris, or `wsock32.lib` on Windows)
to use the communication facilities of `Erl_Interface`.

If you use the `Erl_Interface` functions in a threaded application based on
POSIX threads or Solaris threads, then `Erl_Interface` needs access to some of
the synchronization facilities in your threads package. You must specify extra
compiler flags to indicate which of the packages you use. Define `_REENTRANT`
and either `STHREADS` or `PTHREADS`. The default is to use POSIX threads if
`_REENTRANT` is specified.

## Initializing the Library

Before calling any of the other functions in the library, initialize it by
calling `ei_init()` exactly once.

## Encoding, Decoding, and Sending Erlang Terms

Data sent between distributed Erlang nodes is encoded in the Erlang external
format. You must therefore encode and decode Erlang terms into byte streams if
you want to use the distribution protocol to communicate between a C program and
Erlang.

The `Erl_Interface` library supports this activity. It has several C functions
that create and manipulate Erlang data structures. The following example shows
how to create and encode an Erlang tuple `{tobbe,3928}`:

```text
ei_x_buff buf;
ei_x_new(&buf);
int i = 0;
ei_x_encode_tuple_header(&buf, 2);
ei_x_encode_atom(&buf, "tobbe");
ei_x_encode_long(&buf, 3928);
```

For a complete description, see the [`ei`](ei.md) module.

[](){: #building_terms_and_patterns }

## Building Terms

The previous example can be simplified by using the
[`ei_x_format_wo_ver`](ei.md#ei_x_format_wo_ver) function to create an Erlang
term:

```text
ei_x_buff buf;
ei_x_new(&buf);
ei_x_format_wo_ver(&buf, "{~a,~i}", "tobbe", 3928);
```

For a complete description of the different format directives, see the the
[`ei_x_format_wo_ver`](ei.md#ei_x_format_wo_ver) function.

The following example is more complex:

```text
ei_x_buff buf;
int i = 0;
ei_x_new(&buf);
ei_x_format_wo_ver(&buf,
                   "[{name,~a},{age,~i},{data,[{adr,~s,~i}]}]",
                   "madonna",
                   21,
                  "E-street", 42);
ei_print_term(stdout, buf.buff, &i);
ei_x_free(&buf);
```

As in the previous examples, it is your responsibility to free the memory
allocated for Erlang terms. In this example, `ei_x_free()` ensures that the data
pointed to by `buf` is released.

## Connecting to a Distributed Erlang Node

To connect to a distributed Erlang node, you must first initialize the
connection routine with one of the
[`ei_connect_init_*`](ei_connect.md#ei_connect_init) functions, which stores
information, such as the hostname, and node name for later use:

```c
int identification_number = 99;
int creation=1;
char *cookie="a secret cookie string"; /* An example */
const char* node_name = "einode@durin";
const char *cookie = NULL;
short creation = time(NULL) + 1;
ei_cnode ec;
ei_connect_init(&ec,
                node_name,
                cookie,
                creation);
```

For more information, see the [`ei_connect`](ei_connect.md) module.

After initialization, you set up the connection to the Erlang node. To specify
the Erlang node you want to connect to, use the `ei_connect_*()` family of
functions. The following example sets up the connection and is to result in a
valid socket file descriptor:

```c
int sockfd;
const char* node_name = "einode@durin"; /* An example */
if ((sockfd = ei_connect(&ec, nodename)) < 0)
  fprintf(stderr, "ERROR: ei_connect failed");
```

## Using EPMD

[`erts:epmd`](`e:erts:epmd_cmd.md`) is the Erlang Port Mapper Daemon.
Distributed Erlang nodes register with `epmd` on the local host to indicate to
other nodes that they exist and can accept connections. `epmd` maintains a
register of node and port number information, and when a node wishes to connect
to another node, it first contacts `epmd` to find the correct port number to
connect to.

When you use [`ei_connect`](ei_connect.md) to connect to an Erlang node, a
connection is first made to `epmd` and, if the node is known, a connection is
then made to the Erlang node.

C nodes can also register themselves with `epmd` if they want other nodes in the
system to be able to find and connect to them.

Before registering with `epmd`, you must first create a listen socket and bind
it to a port. Then:

```text
int pub;

pub = ei_publish(&ec, port);
```

`pub` is a file descriptor now connected to `epmd`. `epmd` monitors the other
end of the connection. If it detects that the connection has been closed, the
node becomes unregistered. So, if you explicitly close the descriptor or if your
node fails, it becomes unregistered from `epmd`.

Notice that on some systems a failed node is not detected by this mechanism, as
the operating system does not automatically close descriptors that were left
open when the node failed. If a node has failed in this way, `epmd` prevents you
from registering a new node with the old name, as it thinks that the old name is
still in use. In this case, you must close the port explicitly

## Sending and Receiving Erlang Messages

Use one of the following two functions to send messages:

- [`ei_send`](ei_connect.md#ei_send)
- [`ei_reg_send`](ei_connect.md#ei_reg_send)

As in Erlang, messages can be sent to a pid or to a registered name. It is
easier to send a message to a registered name, as it avoids the problem of
finding a suitable pid.

Use one of the following two functions to receive messages:

- [`ei_receive`](ei_connect.md#ei_receive)
- [`ei_receive_msg`](ei_connect.md#ei_receive_msg)

### Example of Sending Messages

In the following example, `{Pid, hello_world}` is sent to a registered process
`my_server`:

```text
ei_x_buff buf;
ei_x_new_with_version(&buf);

ei_x_encode_tuple_header(&buf, 2);
ei_x_encode_pid(&buf, ei_self(ec));
ei_x_encode_atom(&buf, "Hello world");

ei_reg_send(&ec, fd, "my_server", buf.buff, buf.index);
```

The first element of the tuple that is sent is your own pid. This enables
`my_server` to reply. For more information about the primitives, see the
[`ei_connect`](ei_connect.md) module.

### Example of Receiving Messages

In this example, `{Pid, Something}` is received.

```c
erlang_msg msg;
int index = 0;
int version;
int arity = 0;
erlang_pid pid;
ei_x_buff buf;
ei_x_new(&buf);
for (;;) {
  int got = ei_xreceive_msg(fd, &msg, &x);
  if (got == ERL_TICK)
    continue;
  if (got == ERL_ERROR) {
    fprintf(stderr, "ei_xreceive_msg, got==%d", got);
    exit(1);
  }
  break;
}
ei_decode_version(buf.buff, &index, &version);
ei_decode_tuple_header(buf.buff, &index, &arity);
if (arity != 2) {
  fprintf(stderr, "got wrong message");
  exit(1);
}
ei_decode_pid(buf.buff, &index, &pid);
```

To provide robustness, a distributed Erlang node occasionally polls all its
connected neighbors in an attempt to detect failed nodes or communication links.
A node that receives such a message is expected to respond immediately with an
`ERL_TICK` message. This is done automatically by `ei_xreceive_msg()`. However,
when this has occurred, `ei_xreceive_msg` returns `ERL_TICK` to the caller
without storing a message into the `erlang_msg` structure.

When a message has been received, it is the caller's responsibility to free the
received message.

For more information, see the [`ei_connect`](ei_connect.md) and [`ei`](ei.md)
modules.

## Remote Procedure Calls

An Erlang node acting as a client to another Erlang node typically sends a
request and waits for a reply. Such a request is included in a function call at
a remote node and is called a remote procedure call.

The following example checks if a specific Erlang process is alive:

```c
int index = 0, is_alive;
ei_x_buff args, result;

ei_x_new(&result);
ei_x_new(&args);
ei_x_encode_list_header(&args, 1);
ei_x_encode_pid(&args, &check_pid);
ei_x_encode_empty_list(&args);

if (ei_rpc(&ec, fd, "erlang", "is_process_alive",
           args.buff, args.index, &result) < 0)
    handle_error();

if (ei_decode_version(result.buff, &index) < 0
    || ei_decode_bool(result.buff, &index, &is_alive) < 0)
    handle_error();
```

For more information about `ei_rpc()` and its companions `ei_rpc_to()` and
`ei_rpc_from()`, see the [`ei_connect`](ei_connect.md) module.

## Using Global Names

A C node has access to names registered through the `m:global` module in Kernel.
Names can be looked up, allowing the C node to send messages to named Erlang
services. C nodes can also register global names, allowing them to provide named
services to Erlang processes or other C nodes.

`Erl_Interface` does not provide a native implementation of the global service.
Instead it uses the global services provided by a "nearby" Erlang node. To use
the services described in this section, it is necessary to first open a
connection to an Erlang node.

To see what names there are:

```c
char **names;
int count;
int i;

names = ei_global_names(&ec,fd,&count);

if (names)
  for (i=0; i<count; i++)
    printf("%s\n",names[i]);

free(names);
```

[`ei_global_names`](ei_global.md#ei_global_names) allocates and returns a buffer
containing all the names known to the `global` module in `Kernel`. `count` is
initialized to indicate the number of names in the array. The array of strings
in names is terminated by a `NULL` pointer, so it is not necessary to use
`count` to determine when the last name is reached.

It is the caller's responsibility to free the array. `ei_global_names` allocates
the array and all the strings using a single call to `malloc()`, so
`free(names)` is all that is necessary.

To look up one of the names:

```c
ETERM *pid;
char node[256];
erlang_pid the_pid;

if (ei_global_whereis(&ec,fd,"schedule",&the_pid,node) < 0)
   fprintf(stderr, "ei_global_whereis error\n");
```

If `"schedule"` is known to the `global` module in `Kernel`, an Erlang pid is
written to the_pid. This pid that can be used to send messages to the schedule
service. Also, `node` is initialized to contain the name of the node where the
service is registered, so that you can make a connection to it by simply passing
the variable to [`ei_connect`](ei_connect.md).

Before registering a name, you should already have registered your port number
with `epmd`. This is not strictly necessary, but if you neglect to do so, then
other nodes wishing to communicate with your service cannot find or connect to
your process.

Create a name that Erlang processes can use to communicate with your service:

```text
ei_global_register(fd,servicename,ei_self(ec));
```

After registering the name, use [`ei_accept`](ei_connect.md#ei_accept) to wait
for incoming connections.

> #### Note {: .info }
>
> Remember to free `pid` later with [`ei_x_free`](ei.md#ei_x_free).

To unregister a name:

```text
ei_global_unregister(&ec,fd,servicename);
```
