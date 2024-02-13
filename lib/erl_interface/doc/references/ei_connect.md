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
# ei_connect

Communicate with distributed Erlang.

## Description

This module enables C-programs to communicate with Erlang nodes, using the
Erlang distribution over TCP/IP.

A C-node appears to Erlang as a _hidden node_. That is, Erlang processes that
know the name of the C-node can communicate with it in a normal manner, but the
node name is not shown in the listing provided by `erlang:nodes/0` in `ERTS`.

The environment variable `ERL_EPMD_PORT` can be used to indicate which logical
cluster a C-node belongs to.

## Time-Out Functions

Most functions appear in a version with the suffix `_tmo` appended to the
function name. Those functions take an extra argument, a time-out in
_milliseconds_. The semantics is this: for each communication primitive involved
in the operation, if the primitive does not complete within the time specified,
the function returns an error and `erl_errno` is set to `ETIMEDOUT`. With
communication primitive is meant an operation on the socket, like `connect`,
`accept`, `recv`, or `send`.

Clearly the time-outs are for implementing fault tolerance, not to keep hard
real-time promises. The `_tmo` functions are for detecting non-responsive peers
and to avoid blocking on socket operations.

A time-out value of `0` (zero) means that time-outs are disabled. Calling a
`_tmo` function with the last argument as `0` is therefore the same thing as
calling the function without the `_tmo` suffix.

As with all other functions starting with `ei_`, you are _not_ expected to put
the socket in non-blocking mode yourself in the program. Every use of
non-blocking mode is embedded inside the time-out functions. The socket will
always be back in blocking mode after the operations are completed (regardless
of the result). To avoid problems, leave the socket options alone. `ei` handles
any socket options that need modification.

In all other senses, the `_tmo` functions inherit all the return values and the
semantics from the functions without the `_tmo` suffix.

[](){: #ussi }

## User Supplied Socket Implementation

By default `ei` supplies a TCP/IPv4 socket interface that is used when
communicating. The user can however plug in his/her own IPv4 socket
implementation. This, for example, in order to communicate over TLS. A user
supplied socket implementation is plugged in by passing a
[callback structure](ei_connect.md#ei_socket_callbacks) to either
[`ei_connect_init_ussi()`](ei_connect.md#ei_connect_init) or
[`ei_connect_xinit_ussi()`](ei_connect.md#ei_connect_init).

All callbacks in the `ei_socket_callbacks` structure _should_ return zero on
success; and a posix error code on failure.

The `addr` argument of the `listen`, `accept`, and `connect` callbacks refer to
appropriate address structure for currently used protocol. Currently `ei` only
supports IPv4. That is, at this time `addr` always points to a
`struct sockaddr_in` structure.

[](){: #ei_socket_callbacks_fields } The
[`ei_socket_callbacks`](ei_connect.md#ei_socket_callbacks) structure may be
enlarged in the future. All fields not set, _needs_ to be zeroed out. Currently
the following fields exist:

- **`flags`** - Flags informing `ei` about the behaviour of the callbacks. Flags
  should be bitwise or:ed together. If no flag, is set, the `flags` field should
  contain `0`. Currently, supported flags:

  - **`EI_SCLBK_FLG_FULL_IMPL`** - If set, the `accept()`, `connect()`,
    `writev()`, `write()`, and `read()` callbacks implements timeouts. The
    timeout is passed in the `tmo` argument and is given in milli seconds. Note
    that the `tmo` argument to these callbacks differ from the timeout arguments
    in the `ei` API. Zero means a zero timeout. That is, poll and timeout
    immediately unless the operation is successful. `EI_SCLBK_INF_TMO` (max
    `unsigned`) means infinite timeout. The file descriptor is in blocking mode
    when a callback is called, and it must be in blocking mode when the callback
    returns.

    If not set, `ei` will implement the timeout using `select()` in order to
    determine when to call the callbacks and when to time out. The `tmo`
    arguments of the `accept()`, `connect()`, `writev()`, `write()`, and
    `read()` callbacks should be ignored. The callbacks may be called in
    non-blocking mode. The callbacks are not allowed to change between blocking
    and non-blocking mode. In order for this to work, `select()` needs to
    interact with the socket primitives used the same way as it interacts with
    the ordinary socket primitives. If this is not the case, the callbacks
    _need_ to implement timeouts and this flag should be set.

  More flags may be introduced in the future.

- **`int (*socket)(void **ctx, void *setup_ctx)`** - Create a socket and a
  context for the socket.

  On success it should set `*ctx` to point to a context for the created socket.
  This context will be passed to all other socket callbacks. This function will
  be passed the same `setup_context` as passed to the preceding
  [`ei_connect_init_ussi()`](ei_connect.md#ei_connect_init) or
  [`ei_connect_xinit_ussi()`](ei_connect.md#ei_connect_init) call.

  > #### Note {: .info }
  >
  > During the lifetime of a socket, the pointer `*ctx` _has_ to remain the
  > same. That is, it cannot later be relocated.

  This callback is mandatory.

- **`int (*close)(void *ctx)`** - Close the socket identified by `ctx` and
  destroy the context.

  This callback is mandatory.

- **`int (*listen)(void *ctx, void *addr, int *len, int backlog)`** - Bind the
  socket identified by `ctx` to a local interface and then listen on it.

  The `addr` and `len` arguments are both input and output arguments. When
  called `addr` points to an address structure of length `*len` containing
  information on how to bind the socket. Upon return this callback should have
  updated the structure referred by `addr` with information on how the socket
  actually was bound. `*len` should be updated to reflect the size of `*addr`
  updated. `backlog` identifies the size of the backlog for the listen socket.

  This callback is mandatory.

- **`int (*accept)(void **ctx, void *addr, int *len, unsigned tmo)`** - Accept
  connections on the listen socket identified by `*ctx`.

  When a connection is accepted, a new context for the accepted connection
  should be created and `*ctx` should be updated to point to the new context for
  the accepted connection. When called `addr` points to an uninitialized address
  structure of length `*len`. Upon return this callback should have updated this
  structure with information about the client address. `*len` should be updated
  to reflect the size of `*addr` updated.

  If the `EI_SCLBK_FLG_FULL_IMPL` flag has been set, `tmo` contains timeout time
  in milliseconds.

  > #### Note {: .info }
  >
  > During the lifetime of a socket, the pointer `*ctx` _has_ to remain the
  > same. That is, it cannot later be relocated.

  This callback is mandatory.

- **`int (*connect)(void *ctx, void *addr, int len, unsigned tmo)`** - Connect
  the socket identified by `ctx` to the address identified by `addr`.

  When called `addr` points to an address structure of length `len` containing
  information on where to connect.

  If the `EI_SCLBK_FLG_FULL_IMPL` flag has been set, `tmo` contains timeout time
  in milliseconds.

  This callback is mandatory.

- **`int (*writev)(void *ctx, const void *iov, long iovcnt, ssize_t *len, unsigned tmo)`** -
  Write data on the connected socket identified by `ctx`.

  `iov` points to an array of `struct iovec` structures of length `iovcnt`
  containing data to write to the socket. On success, this callback should set
  `*len` to the amount of bytes successfully written on the socket.

  If the `EI_SCLBK_FLG_FULL_IMPL` flag has been set, `tmo` contains timeout time
  in milliseconds.

  This callback is optional. Set the `writev` field in the the
  `ei_socket_callbacks` structure to `NULL` if not implemented.

- **`int (*write)(void *ctx, const char *buf, ssize_t *len, unsigned tmo)`** -
  Write data on the connected socket identified by `ctx`.

  When called `buf` points to a buffer of length `*len` containing the data to
  write on the socket. On success, this callback should set `*len` to the amount
  of bytes successfully written on the socket.

  If the `EI_SCLBK_FLG_FULL_IMPL` flag has been set, `tmo` contains timeout time
  in milliseconds.

  This callback is mandatory.

- **`int (*read)(void *ctx, char *buf, ssize_t *len, unsigned tmo)`** - Read
  data on the connected socket identified by `ctx`.

  `buf` points to a buffer of length `*len` where the read data should be
  placed. On success, this callback should update `*len` to the amount of bytes
  successfully read on the socket.

  If the `EI_SCLBK_FLG_FULL_IMPL` flag has been set, `tmo` contains timeout time
  in milliseconds.

  This callback is mandatory.

- **`int (*handshake_packet_header_size)(void *ctx, int *sz)`** - Inform about
  handshake packet header size to use during the Erlang distribution handshake.

  On success, `*sz` should be set to the handshake packet header size to use.
  Valid values are `2` and `4`. Erlang TCP distribution use a handshake packet
  size of `2` and Erlang TLS distribution use a handshake packet size of `4`.

  This callback is mandatory.

- **`int (*connect_handshake_complete)(void *ctx)`** - Called when a locally
  started handshake has completed successfully.

  This callback is optional. Set the `connect_handshake_complete` field in the
  `ei_socket_callbacks` structure to `NULL` if not implemented.

- **`int (*accept_handshake_complete)(void *ctx)`** - Called when a remotely
  started handshake has completed successfully.

  This callback is optional. Set the `accept_handshake_complete` field in the
  `ei_socket_callbacks` structure to `NULL` if not implemented.

- **`int (*get_fd)(void *ctx, int *fd)`** - Inform about file descriptor used by
  the socket which is identified by `ctx`.

  > #### Note {: .info }
  >
  > During the lifetime of a socket, the file descriptor _has_ to remain the
  > same. That is, repeated calls to this callback with the same context
  > `should` always report the same file descriptor.
  >
  > The file descriptor _has_ to be a real file descriptor. That is, no other
  > operation should be able to get the same file descriptor until it has been
  > released by the `close()` callback.

  This callback is mandatory.

## Data Types

- **`ei_cnode`{: #ei_cnode }** - Opaque data type representing a C-node. A
  `ei_cnode` structure is initialized by calling
  [`ei_connect_init()`](ei_connect.md#ei_connect_init) or friends.

- **`ei_socket_callbacks`{: #ei_socket_callbacks }**

  ```c
  typedef struct {
      int flags;
      int (*socket)(void **ctx, void *setup_ctx);
      int	(*close)(void *ctx);
      int (*listen)(void *ctx, void *addr, int *len, int backlog);
      int (*accept)(void **ctx, void *addr, int *len, unsigned tmo);
      int (*connect)(void *ctx, void *addr, int len, unsigned tmo);
      int (*writev)(void *ctx, const void *iov, int iovcnt, ssize_t *len, unsigned tmo);
      int (*write)(void *ctx, const char *buf, ssize_t *len, unsigned tmo);
      int (*read)(void *ctx, char *buf, ssize_t *len, unsigned tmo);
      int (*handshake_packet_header_size)(void *ctx, int *sz);
      int (*connect_handshake_complete)(void *ctx);
      int (*accept_handshake_complete)(void *ctx);
      int (*get_fd)(void *ctx, int *fd);
  } ei_socket_callbacks;
  ```

  Callbacks functions for a
  [_User Supplied Socket Implementation_](ei_connect.md#ussi).
  [Documentation of each field](ei_connect.md#ei_socket_callbacks_fields) can be
  found in the _User Supplied Socket Implementation_ section above.

- **`ErlConnect`{: #ErlConnect }**

  ```text
  typedef struct {
      char ipadr[4]; /* Ip v4 address in network byte order */
      char nodename[MAXNODELEN];
  } ErlConnect;
  ```

  IP v4 address and nodename.

- **`Erl_IpAddr`{: #Erl_IpAddr }**

  ```text
  typedef struct {
      unsigned s_addr; /* Ip v4 address in network byte order */
  } Erl_IpAddr;
  ```

  IP v4 address.

- **`erlang_msg`{: #erlang_msg }**

  ```c
  typedef struct {
      long msgtype;
      erlang_pid from;
      erlang_pid to;
      char toname[MAXATOMLEN+1];
      char cookie[MAXATOMLEN+1];
      erlang_trace token;
  } erlang_msg;
  ```

  Information about a message received via
  [`ei_receive_msg()`](ei_connect.md#ei_receive_msg) or friends.

## ei_gethostbyaddr()

## ei_gethostbyaddr_r()

## ei_gethostbyname()

## ei_gethostbyname_r()

```c
struct hostent * ei_gethostbyaddr(const char *addr, int len, int type);
```

```c
struct hostent * ei_gethostbyaddr_r(const char *addr, int length,  int type,  struct hostent *hostp, char *buffer,   int buflen,  int *h_errnop);
```

```c
struct hostent * ei_gethostbyname(const char *name);
```

```c
struct hostent * ei_gethostbyname_r(const char *name,  struct hostent *hostp,  char *buffer,  int buflen,  int *h_errnop);
```

Convenience functions for some common name lookup functions.

## ei_accept()

```c
int ei_accept(ei_cnode *ec, int listensock, ErlConnect *conp);
```

Used by a server process to accept a connection from a client process.

- `ec` is the C-node structure.
- `listensock` is an open socket descriptor on which `listen()` has previously
  been called.
- `conp` is a pointer to an [`ErlConnect`](ei_connect.md#ErlConnect) struct.

On success, `conp` is filled in with the address and node name of the connecting
client and a file descriptor is returned. On failure, `ERL_ERROR` is returned
and `erl_errno` is set to `EIO`.

## ei_accept_tmo()

```c
int ei_accept_tmo(ei_cnode *ec, int listensock, ErlConnect *conp, unsigned timeout_ms);
```

Equivalent to `ei_accept` with an optional time-out argument, see the
description at the beginning of this manual page.

## ei_close_connection()

```c
int ei_close_connection(int fd);
```

Closes a previously opened connection or listen socket.

## ei_connect()

## ei_xconnect()

## ei_connect_host_port()

## ei_xconnect_host_port()

```c
int ei_connect(ei_cnode* ec, char *nodename);
```

```c
int ei_xconnect(ei_cnode* ec, Erl_IpAddr adr, char *alivename);
```

```c
int ei_connect_host_port(ei_cnode* ec, char *hostname, int port);
```

```c
int ei_xconnect_host_port(ei_cnode* ec, Erl_IpAddr adr, int port);
```

Sets up a connection to an Erlang node.

`ei_xconnect()` requires the IP address of the remote host and the alive name of
the remote node to be specified. `ei_connect()` provides an alternative
interface and determines the information from the node name provided. The
`ei_xconnect_host_port()` function provides yet another alternative that will
work even if there is no EPMD instance on the host where the remote node is
running. The `ei_xconnect_host_port()` function requires the IP address and port
of the remote node to be specified. The `ei_connect_host_port()` function is an
alternative to `ei_xconnect_host_port()` that lets the user specify a hostname
instead of an IP address.

- `adr` is the 32-bit IP address of the remote host.
- `alive` is the alivename of the remote node.
- `node` is the name of the remote node.
- `port` is the port number of the remote node.

These functions return an open file descriptor on success, or a negative value
indicating that an error occurred. In the latter case they set `erl_errno` to
one of the following:

- **`EHOSTUNREACH`** - The remote host `node` is unreachable.

- **`ENOMEM`** - No more memory is available.

- **`EIO`** - I/O error.

Also, `errno` values from `socket`_(2)_ and `connect`_(2)_ system calls may be
propagated into `erl_errno`.

_Example:_

```c
#define NODE   "madonna@chivas.du.etx.ericsson.se"
#define ALIVE  "madonna"
#define IP_ADDR "150.236.14.75"

/*** Variant 1 ***/
int fd = ei_connect(&ec, NODE);

/*** Variant 2 ***/
struct in_addr addr;
addr.s_addr = inet_addr(IP_ADDR);
fd = ei_xconnect(&ec, &addr, ALIVE);
```

## ei_connect_init()

## ei_connect_init_ussi()

## ei_connect_xinit()

## ei_connect_xinit_ussi()

```c
int ei_connect_init(ei_cnode* ec, const char* this_node_name, const char *cookie, unsigned creation);
```

```c
int ei_connect_init_ussi(ei_cnode* ec, const char* this_node_name, const char *cookie, unsigned creation, ei_socket_callbacks *cbs, int cbs_sz, void *setup_context);
```

```c
int ei_connect_xinit(ei_cnode* ec, const char *thishostname, const char *thisalivename, const char *thisnodename, Erl_IpAddr thisipaddr, const char *cookie, unsigned creation);
```

```c
int ei_connect_xinit_ussi(ei_cnode* ec, const char *thishostname, const char *thisalivename, const char *thisnodename, Erl_IpAddr thisipaddr, const char *cookie, unsigned creation, ei_socket_callbacks *cbs, int cbs_sz, void *setup_context);
```

Initializes the `ec` structure, to identify the node name and cookie of the
server. One of them must be called before other functions that works on the
`ei_cnode` type or a file descriptor associated with a connection to another
node is used.

- `ec` is a structure containing information about the C-node. It is used in
  other `ei` functions for connecting and receiving data.
- `this_node_name` is the name of the C-node (the name before '@' in the full
  node name).
- `cookie` is the cookie for the node.
- `creation` identifies a specific instance of a C-node. It can help prevent the
  node from receiving messages sent to an earlier process with the same
  registered name.

  > #### Note {: .info }
  >
  > The type of the `creation` argument was changed from `short` (16 bit) to
  > `unsigned int` (32 bit) in OTP 25. This should cause no practical problem
  > other than maybe a compiler warning.

- `thishostname` is the name of the machine we are running on. If long names are
  to be used, they are to be fully qualified (that is, `durin.erix.ericsson.se`
  instead of `durin`).
- `thisalivename` is the name of the local C-node (the name before '@' in the
  full node name). Can be `NULL` (from OTP 23) to get a dynamically assigned
  name from the peer node.
- `thisnodename` is the full name of the local C-node, that is, `mynode@myhost`.
  Can be `NULL` if `thisalivename` is `NULL`.
- `thispaddr` if the IP address of the host.
- `cbs` is a pointer to a
  [callback structure](ei_connect.md#ei_socket_callbacks) implementing and
  alternative socket interface.
- `cbs_sz` is the size of the structure pointed to by `cbs`.
- `setup_context` is a pointer to a structure that will be passed as second
  argument to the `socket` callback in the `cbs` structure.

A C-node acting as a server is assigned a creation number when it calls
`ei_publish()`.

A connection is closed by simply closing the socket. For information about how
to close the socket gracefully (when there are outgoing packets before close),
see the relevant system documentation.

These functions return a negative value indicating that an error occurred.

_Example 1:_

```c
unsigned n = 0;
struct in_addr addr;
ei_cnode ec;
addr.s_addr = inet_addr("150.236.14.75");
if (ei_connect_xinit(&ec,
                     "chivas",
                     "madonna",
                     "madonna@chivas.du.etx.ericsson.se",
                     &addr;
                     "cookie...",
                     n++) < 0) {
    fprintf(stderr,"ERROR when initializing: %d",erl_errno);
    exit(-1);
}
```

_Example 2:_

```c
if (ei_connect_init(&ec, "madonna", "cookie...", n++) < 0) {
    fprintf(stderr,"ERROR when initializing: %d",erl_errno);
    exit(-1);
}
```

## ei_connect_tmo()

## ei_xconnect_tmo()

## ei_connect_host_port_tmo()

## ei_xconnect_host_port_tmo()

```c
int ei_connect_tmo(ei_cnode* ec, char *nodename, unsigned timeout_ms);
```

```c
int ei_xconnect_tmo(ei_cnode* ec, Erl_IpAddr adr, char *alivename, unsigned timeout_ms);
```

```c
int ei_connect_host_port_tmo(ei_cnode* ec, char *hostname, int port, unsigned ms);
```

```c
int ei_xconnect_host_port_tmo(ei_cnode* ec, Erl_IpAddr adr, int port, unsigned ms);
```

Equivalent to `ei_connect`, `ei_xconnect`, `ei_connect_host_port` and
`ei_xconnect_host_port` with an optional time-out argument, see the description
at the beginning of this manual page.

## ei_get_tracelevel()

## ei_set_tracelevel()

```c
int ei_get_tracelevel(void);
```

```c
void ei_set_tracelevel(int level);
```

Used to set tracing on the distribution. The levels are different verbosity
levels. A higher level means more information. See also section
[Debug Information](ei_connect.md#debug_information).

These functions are not thread safe.

## ei_listen()

## ei_xlisten()

```c
int ei_listen(ei_cnode *ec, int *port, int backlog);
```

```c
int ei_xlisten(ei_cnode *ec, Erl_IpAddr adr, int *port, int backlog);
```

Used by a server process to setup a listen socket which later can be used for
accepting connections from client processes.

- `ec` is the C-node structure.
- `adr` is local interface to bind to.
- `port` is a pointer to an integer containing the port number to bind to. If
  `*port` equals `0` when calling `ei_listen()`, the socket will be bound to an
  ephemeral port. On success, `ei_listen()` will update the value of `*port` to
  the port actually bound to.
- `backlog` is maximum backlog of pending connections.

`ei_listen` will create a socket, bind to a port on the local interface
identified by `adr` (or all local interfaces if `ei_listen()` is called), and
mark the socket as a passive socket (that is, a socket that will be used for
accepting incoming connections).

On success, a file descriptor is returned which can be used in a call to
`ei_accept()`. On failure, `ERL_ERROR` is returned and `erl_errno` is set to
`EIO`.

## ei_make_pid()

```c
int ei_make_pid(ei_cnode *ec, erlang_pid *pid);
```

Creates a new process identifier in the argument `pid`. This process identifier
refers to a conseptual process residing on the C-node identified by the argument
`ec`. On success `0` is returned. On failure `ERL_ERROR` is returned and
`erl_errno` is set.

The C-node identified by `ec` must have been initialized and must have received
a name prior to the call to `ei_make_pid()`. Initialization of the C-node is
done by a call to [`ei_connect_init()`](ei_connect.md#ei_connect_init) or
friends. If the name is dynamically assigned from the peer node, the C-node also
has to be connected.

## ei_make_ref()

```c
int ei_make_ref(ei_cnode *ec, erlang_ref *ref);
```

Creates a new reference in the argument `ref`. This reference originates from
the C-node identified by the argument `ec`. On success `0` is returned. On
failure `ERL_ERROR` is returned and `erl_errno` is set.

The C-node identified by `ec` must have been initialized and must have received
a name prior to the call to `ei_make_ref()`. Initialization of the C-node is
done by a call to [`ei_connect_init()`](ei_connect.md#ei_connect_init) or
friends. If the name is dynamically assigned from the peer node, the C-node also
has to be connected.

## ei_publish()

```c
int ei_publish(ei_cnode *ec, int port);
```

Used by a server process to register with the local name server EPMD, thereby
allowing other processes to send messages by using the registered name. Before
calling either of these functions, the process should have called `bind()` and
`listen()` on an open socket.

- `ec` is the C-node structure.
- `port` is the local name to register, and is to be the same as the port number
  that was previously bound to the socket.
- `addr` is the 32-bit IP address of the local host.

To unregister with EPMD, simply close the returned descriptor. Do not use
`ei_unpublish()`, which is deprecated anyway.

On success, the function returns a descriptor connecting the calling process to
EPMD. On failure, `-1` is returned and `erl_errno` is set to `EIO`.

Also, `errno` values from `socket`_(2)_ and `connect`_(2)_ system calls may be
propagated into `erl_errno`.

## ei_publish_tmo()

```c
int ei_publish_tmo(ei_cnode *ec, int port, unsigned timeout_ms);
```

Equivalent to `ei_publish` with an optional time-out argument, see the
description at the beginning of this manual page.

## ei_receive()

```c
int ei_receive(int fd, unsigned char* bufp, int bufsize);
```

Receives a message consisting of a sequence of bytes in the Erlang external
format.

- `fd` is an open descriptor to an Erlang connection. It is obtained from a
  previous `ei_connect` or `ei_accept`.
- `bufp` is a buffer large enough to hold the expected message.
- `bufsize` indicates the size of `bufp`.

If a _tick_ occurs, that is, the Erlang node on the other end of the connection
has polled this node to see if it is still alive, the function returns
`ERL_TICK` and no message is placed in the buffer. Also, `erl_errno` is set to
`EAGAIN`.

On success, the message is placed in the specified buffer and the function
returns the number of bytes actually read. On failure, the function returns
`ERL_ERROR` and sets `erl_errno` to one of the following:

- **`EAGAIN`** - Temporary error: Try again.

- **`EMSGSIZE`** - Buffer is too small.

- **`EIO`** - I/O error.

## ei_receive_encoded()

```c
int ei_receive_encoded(int fd, char **mbufp, int *bufsz,  erlang_msg *msg, int *msglen);
```

This function is retained for compatibility with code generated by the interface
compiler and with code following examples in the same application.

In essence, the function performs the same operation as `ei_xreceive_msg`, but
instead of using an `ei_x_buff`, the function expects a pointer to a character
pointer (`mbufp`), where the character pointer is to point to a memory area
allocated by `malloc`. Argument `bufsz` is to be a pointer to an integer
containing the exact size (in bytes) of the memory area. The function may
reallocate the memory area and will in such cases put the new size in `*bufsz`
and update `*mbufp`.

Returns either `ERL_TICK` or the `msgtype` field of the `erlang_msg *msg`. The
length of the message is put in `*msglen`. On error a value `< 0` is returned.

It is recommended to use `ei_xreceive_msg` instead when possible, for the sake
of readability. However, the function will be retained in the interface for
compatibility and will _not_ be removed in future releases without prior notice.

## ei_receive_encoded_tmo()

```c
int ei_receive_encoded_tmo(int fd, char **mbufp, int *bufsz,  erlang_msg *msg, int *msglen, unsigned timeout_ms);
```

Equivalent to `ei_receive_encoded` with an optional time-out argument, see the
description at the beginning of this manual page.

## ei_receive_msg()

## ei_xreceive_msg()

```c
int ei_receive_msg(int fd, erlang_msg* msg, ei_x_buff* x);
```

```c
int ei_xreceive_msg(int fd, erlang_msg* msg, ei_x_buff* x);
```

Receives a message to the buffer in `x`. `ei_xreceive_msg` allows the buffer in
`x` to grow, but `ei_receive_msg` fails if the message is larger than the
pre-allocated buffer in `x`.

- `fd` is an open descriptor to an Erlang connection.
- `msg` is a pointer to an `erlang_msg` structure and contains information on
  the message received.
- `x` is buffer obtained from `ei_x_new`.

On success, the functions return `ERL_MSG` and the
[`msg`](ei_connect.md#erlang_msg) struct is initialized.

`msgtype` identifies the type of message, and is one of the following:

- **`ERL_SEND`** - Indicates that an ordinary send operation has occurred.
  `msg->to` contains the pid of the recipient (the C-node).

- **`ERL_REG_SEND`** - A registered send operation occurred. `msg->from`
  contains the pid of the sender.

- **`ERL_LINK` or `ERL_UNLINK`** - `msg->to` and `msg->from` contain the pids of
  the sender and recipient of the link or unlink.

- **`ERL_EXIT`** - Indicates a broken link. `msg->to` and `msg->from` contain
  the pids of the linked processes.

The return value is the same as for [`ei_receive`](ei_connect.md#ei_receive).

## ei_receive_msg_tmo()

## ei_xreceive_msg_tmo()

```c
int ei_receive_msg_tmo(int fd, erlang_msg* msg, ei_x_buff* x, unsigned imeout_ms);
```

```c
int ei_xreceive_msg_tmo(int fd, erlang_msg* msg, ei_x_buff* x, unsigned timeout_ms);
```

Equivalent to `ei_receive_msg` and `ei_xreceive_msg` with an optional time-out
argument, see the description at the beginning of this manual page.

## ei_receive_tmo()

```c
int ei_receive_tmo(int fd, unsigned char* bufp, int bufsize, unsigned timeout_ms);
```

Equivalent to `ei_receive` with an optional time-out argument, see the
description at the beginning of this manual page.

## ei_reg_send()

```c
int ei_reg_send(ei_cnode* ec, int fd, char* server_name, char* buf, int len);
```

Sends an Erlang term to a registered process.

- `fd` is an open descriptor to an Erlang connection.
- `server_name` is the registered name of the intended recipient.
- `buf` is the buffer containing the term in binary format.
- `len` is the length of the message in bytes.

Returns `0` if successful, otherwise `-1`. In the latter case it sets
`erl_errno` to `EIO`.

_Example:_

Send the atom "ok" to the process "worker":

```c
ei_x_buff x;
ei_x_new_with_version(&x);
ei_x_encode_atom(&x, "ok");
if (ei_reg_send(&ec, fd, x.buff, x.index) < 0)
    handle_error();
```

## ei_reg_send_tmo()

```c
int ei_reg_send_tmo(ei_cnode* ec, int fd, char* server_name, char* buf, int len, unsigned timeout_ms);
```

Equivalent to `ei_reg_send` with an optional time-out argument, see the
description at the beginning of this manual page.

## ei_rpc()

## ei_rpc_to()

## ei_xrpc_to()

## ei_rpc_from()

```c
int ei_rpc(ei_cnode *ec, int fd, char *mod, char *fun, const char *argbuf, int argbuflen, ei_x_buff *x);
```

```c
int ei_rpc_to(ei_cnode *ec, int fd, char *mod, char *fun, const char *argbuf, int argbuflen);
```

```c
int ei_xrpc_to(ei_cnode *ec, int fd, char *mod, char *fun, const char *argbuf, int argbuflen, int flags);
```

```c
int ei_rpc_from(ei_cnode *ec, int fd, int timeout, erlang_msg *msg, ei_x_buff *x);
```

Supports calling Erlang functions on remote nodes. `ei_rpc_to()` sends an RPC
request to a remote node and `ei_rpc_from()` receives the results of such a
call. `ei_rpc()` combines the functionality of these two functions by sending an
RPC request and waiting for the results.

The `ei_xrpc_to()` function is equivalent to `ei_rpc_to()` when its `flags`
parameter is set to `0`. When the flags parameter of `ei_xrpc_to()` is set to
`EI_RPC_FETCH_STDOUT`, stdout (standard output) data are forwarded. See the
documentation for the flags parameter for more information about the
`EI_RPC_FETCH_STDOUT` flag.

`rpc:call/4` in Kernel.

- `ec` is the C-node structure previously initiated by a call to
  `ei_connect_init()` or `ei_connect_xinit()`.
- `fd` is an open descriptor to an Erlang connection.
- `timeout` is the maximum time (in milliseconds) to wait for results. Specify
  `ERL_NO_TIMEOUT` to wait forever. `ei_rpc()` waits infinitely for the answer,
  that is, the call will never time out.
- `mod` is the name of the module containing the function to be run on the
  remote node.
- `fun` is the name of the function to run.
- `argbuf` is a pointer to a buffer with an encoded Erlang list, without a
  version magic number, containing the arguments to be passed to the function.
- `argbuflen` is the length of the buffer containing the encoded Erlang list.
- `msg` is structure of type `erlang_msg` and contains information on the
  message received. For a description of the `erlang_msg` format, see
  [`ei_receive_msg`](ei_connect.md#ei_receive_msg).
- `x` points to the dynamic buffer that receives the result. For `ei_rpc()` this
  is the result without the version magic number. For an `ei_rpc_from()` call
  the result consists of a version magic number and a 2-tuple. The 2-tuple can
  be in one of the following two forms:

  - **`{rex,Reply}`** - This response value means that the RPC has completed.
    The result value is the `Reply` term. This is the only type of response that
    one can get from an RPC triggered by a call to `ei_rpc_to()` or
    `ei_xrpc_to()` without the `EI_RPC_FETCH_STDOUT` flag. If the RPC was
    triggered by a call to `ei_xrpc_to()` with the `EI_RPC_FETCH_STDOUT` flag
    set, then all forwarded stdout data has been received.

  - **`{rex_stdout,StdOutUTF8Binary}`** - This response value can only be
    obtained if the RPC call was triggered by a call to `ei_xrpc_to()` with the
    `EI_RPC_FETCH_STDOUT` flag set. This response value means that forwarded
    stdout data has been received. The stdout data is stored in a binary and is
    UTF-8 encoded. One may need to call `ei_rpc_from()` multiple times to read
    all the stdout data. The stdout data is received in the same order as it was
    written. All forwarded stdout data have been received when a `{rex,Reply}`
    tuple has been obtained from an `ei_rpc_from()` call.

- `flags` The flag `EI_RPC_FETCH_STDOUT` is currently the only flag that is
  supported by `ei_xrpc_to()`. When `EI_RPC_FETCH_STDOUT` is set, the called
  function is executed in a new process with a
  [group leader](`erlang:group_leader/0`) that forwards all stdout data. This
  means that stdout data that are written during the execution of the called
  function, by the called function and by descendant processes, will be
  forwarded (given that the group leader has not been changed by a call to
  `erlang:group_leader/2`). The forwarded stdout data need to be collected by a
  sequence of calls to `ei_rpc_from()`. See the description of the `x` parameter
  for how `ei_rpc_from()` is used to receive stdout data. See the documentation
  of the [the I/O protocol](`e:stdlib:io_protocol.md`), for more information
  about the group leader concept.

  > #### Note {: .info }
  >
  > The flag `EI_RPC_FETCH_STDOUT` only works when interacting with a node with
  > a version greater or equal to OTP-24.

`ei_rpc()` returns the number of bytes in the result on success and `-1` on
failure. `ei_rpc_from()` returns the number of bytes, otherwise one of
`ERL_TICK`, `ERL_TIMEOUT`, and `ERL_ERROR`. The functions `ei_rpc_to()` and
`ei_xrpc_to()` returns 0 if successful, otherwise -1. When failing, all four
functions set `erl_errno` to one of the following:

- **`EIO`** - I/O error.

- **`ETIMEDOUT`** - Time-out expired.

- **`EAGAIN`** - Temporary error: Try again.

_Example:_

Check to see if an Erlang process is alive:

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

## ei_self()

```c
erlang_pid * ei_self(ei_cnode *ec);
```

Retrieves a generic pid of the C-node. Every C-node has a (pseudo) pid used in
`ei_send_reg`, `ei_rpc()`, and others. This is contained in a field in the `ec`
structure. Do _not_ modify this structure.

On success a pointer to the process identifier is returned. On failure `NULL` is
returned and `erl_errno` is set.

The C-node identified by `ec` must have been initialized and must have received
a name prior to the call to `ei_self()`. Initialization of the C-node is done by
a call to [`ei_connect_init()`](ei_connect.md#ei_connect_init) or friends. If
the name is dynamically assigned from the peer node, the C-node also has to be
connected.

## ei_send()

```c
int ei_send(int fd, erlang_pid* to, char* buf, int len);
```

Sends an Erlang term to a process.

- `fd` is an open descriptor to an Erlang connection.
- `to` is the pid of the intended recipient of the message.
- `buf` is the buffer containing the term in binary format.
- `len` is the length of the message in bytes.

Returns `0` if successful, otherwise `-1`. In the latter case it sets
`erl_errno` to `EIO`.

## ei_send_encoded()

```c
int ei_send_encoded(int fd, erlang_pid* to, char* buf, int len);
```

Works exactly as `ei_send`, the alternative name is retained for backward
compatibility. The function will _not_ be removed without prior notice.

## ei_send_encoded_tmo()

```c
int ei_send_encoded_tmo(int fd, erlang_pid* to, char* buf, int len, unsigned timeout_ms);
```

Equivalent to `ei_send_encoded` with an optional time-out argument, see the
description at the beginning of this manual page.

## ei_send_reg_encoded()

```c
int ei_send_reg_encoded(int fd, const erlang_pid *from, const char *to, const char *buf, int len);
```

This function is retained for compatibility with code generated by the interface
compiler and with code following examples in the same application.

The function works as `ei_reg_send` with one exception. Instead of taking
`ei_cnode` as first argument, it takes a second argument, an `erlang_pid`, which
is to be the process identifier of the sending process (in the Erlang
distribution protocol).

A suitable `erlang_pid` can be retrieved from the `ei_cnode` structure by
calling `ei_self(cnode_pointer)`.

## ei_send_reg_encoded_tmo()

```c
int ei_send_reg_encoded_tmo(int fd, const erlang_pid *from, const char *to, const char *buf, int len, unsigned timeout_ms);
```

Equivalent to `ei_send_reg_encoded` with an optional time-out argument, see the
description at the beginning of this manual page.

## ei_send_tmo()

```c
int ei_send_tmo(int fd, erlang_pid* to, char* buf, int len, unsigned timeout_ms);
```

Equivalent to `ei_send` with an optional time-out argument, see the description
at the beginning of this manual page.

## ei_thisnodename()

## ei_thishostname()

## ei_thisalivename()

```c
const char * ei_thisnodename(ei_cnode *ec);
```

```c
const char * ei_thishostname(ei_cnode *ec);
```

```c
const char * ei_thisalivename(ei_cnode *ec);
```

Can be used to retrieve information about the C-node. These values are initially
set with `ei_connect_init()` or `ei_connect_xinit()`.

These function simply fetch the appropriate field from the `ec` structure. Read
the field directly will probably be safe for a long time, so these functions are
not really needed.

## ei_unpublish()

```c
int ei_unpublish(ei_cnode *ec);
```

Can be called by a process to unregister a specified node from EPMD on the local
host. This is, however, usually not allowed, unless EPMD was started with flag
`-relaxed_command_check`, which it normally is not.

To unregister a node you have published, you should close the descriptor that
was returned by `ei_publish()`.

> #### Warning {: .warning }
>
> This function is deprecated and will be removed in a future release.

`ec` is the node structure of the node to unregister.

If the node was successfully unregistered from EPMD, the function returns `0`.
Otherwise, `-1` is returned and `erl_errno` is set to `EIO`.

## ei_unpublish_tmo()

```c
int ei_unpublish_tmo(ei_cnode *ec, unsigned timeout_ms);
```

Equivalent to `ei_unpublish` with an optional time-out argument, see the
description at the beginning of this manual page.

[](){: #debug_information }

## Debug Information

If a connection attempt fails, the following can be checked:

- `erl_errno`.
- That the correct cookie was used
- That EPMD is running
- That the remote Erlang node on the other side is running the same version of
  Erlang as the `ei` library
- That environment variable `ERL_EPMD_PORT` is set correctly

The connection attempt can be traced by setting a trace level by either using
`ei_set_tracelevel` or by setting environment variable `EI_TRACELEVEL`. The
trace levels have the following messages:

- 1: Verbose error messages
- 2: Above messages and verbose warning messages
- 3: Above messages and progress reports for connection handling
- 4: Above messages and progress reports for communication
- 5: Above messages and progress reports for data conversion
