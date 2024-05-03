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
# How to Implement an Alternative Carrier for the Erlang Distribution

This section describes how to implement an alternative carrier protocol for the
Erlang distribution. The distribution is normally carried by TCP/IP. Here is
explained a method for replacing TCP/IP with another protocol.

The section is a step-by-step explanation of the `uds_dist` example application
(in the Kernel application `examples` directory). The `uds_dist` application
implements distribution over Unix domain sockets and is written for the Sun
Solaris 2 operating environment. The mechanisms are however general and apply to
any operating system Erlang runs on. The reason the C code is not made portable,
is simply readability.

## Introduction

To implement a new carrier for the Erlang distribution, the main steps are as
follows.

> #### Note {: .info }
>
> As of ERTS version 10.0 support for distribution controller processes has been
> introduced. That is, the traffic over a distribution channel can be managed by
> a process instead of only by a port. This makes it possible to implement large
> parts of the logic in Erlang code, and you perhaps do not even need a new
> driver for the protocol. One example could be Erlang distribution over UDP
> using `gen_udp` (your Erlang code will of course have to take care of
> retransmissions, etc in this example). That is, depending on what you want to
> do you perhaps do not need to implement a driver at all and can then skip the
> driver related sections below. The `gen_tcp_dist` and `erl_uds_dist` examples
> described in the [Distribution Module](alt_dist.md#distribution-module)
> section utilize distribution controller processes and can be worth having a
> look at if you want to use distribution controller processes.

### Writing an Erlang Driver

First, the protocol must be available to the Erlang machine, which involves
writing an Erlang driver. A port program cannot be used, an Erlang driver is
required. Erlang drivers can be:

- Statically linked to the emulator, which can be an alternative when using the
  open source distribution of Erlang, or
- Dynamically loaded into the Erlang machines address space, which is the only
  alternative if a precompiled version of Erlang is to be used

Writing an Erlang driver is not easy. The driver is written as some callback
functions called by the Erlang emulator when data is sent to the driver, or the
driver has any data available on a file descriptor. As the driver callback
routines execute in the main thread of the Erlang machine, the callback
functions can perform no blocking activity whatsoever. The callbacks are only to
set up file descriptors for waiting and/or read/write available data. All I/O
must be non-blocking. Driver callbacks are however executed in sequence, why a
global state can safely be updated within the routines.

### Writing an Erlang Interface for the Driver

When the driver is implemented, one would preferably write an Erlang interface
for the driver to be able to test the functionality of the driver separately.
This interface can then be used by the distribution module, which will cover the
details of the protocol from the `net_kernel`.

The easiest path is to mimic the `inet` and `inet_tcp` interfaces, but not much
functionality in those modules needs to be implemented. In the example
application, only a few of the usual interfaces are implemented, and they are
much simplified.

### Writing a Distribution Module

When the protocol is available to Erlang through a driver and an Erlang
interface module, a distribution module can be written. The distribution module
is a module with well-defined callbacks, much like a `gen_server` (there is no
compiler support for checking the callbacks, though). This module implements:

- The details of finding other nodes (that is, talking to `epmd` or something
  similar)
- Creating a listen port (or similar)
- Connecting to other nodes
- Performing the handshakes/cookie verification

There is however a utility module, `dist_util`, which does most of the hard work
of handling handshakes, cookies, timers, and ticking. Using `dist_util` makes
implementing a distribution module much easier and that is done in the example
application.

### Creating Boot Scripts

The last step is to create boot scripts to make the protocol implementation
available at boot time. The implementation can be debugged by starting the
distribution when all the system is running, but in a real system the
distribution is to start very early, why a boot script and some command-line
parameters are necessary.

This step also implies that the Erlang code in the interface and distribution
modules is written in such a way that it can be run in the startup phase. In
particular, there can be no calls to the `application` module or to any modules
not loaded at boot time. That is, only `Kernel`, `STDLIB`, and the application
itself can be used.

## Distribution Module

The distribution module exposes an API that `net_kernel` calls in order to
manage connections to other nodes. The module name should have the suffix
`_dist`.

The module needs to create some kind of listening entity (process or port) and
an acceptor process that accepts incoming connections using the listening
entity. For each connection, the module at least needs to create one connection
supervisor process, which also is responsible for the handshake when setting up
the connection, and a distribution controller (process or port) responsible for
transport of data over the connection. The distribution controller and the
connection supervisor process should be linked together so both of them are
cleaned up when the connection is taken down.

Note that there need to be exactly one distribution controller per connection. A
process or port can only be distribution controller for one connection. The
registration as distribution controller cannot be undone. It will stick until
the distribution controller terminates. The distribution controller should not
ignore exit signals. It is allowed to trap exits, but it should then voluntarily
terminate when an exit signal is received.

An example implementation of a distribution module can be found in
[$ERL_TOP/lib/kernel/examples/gen_tcp_dist/src/gen_tcp_dist.erl](assets/gen_tcp_dist.erl).
It implements the distribution over TCP/IP using the `gen_tcp` API with
distribution controllers implemented by processes. This instead of using port
distribution controllers as the ordinary TCP/IP distribution uses.

Another example implementation of a distribution module can be found in
[$ERL_TOP/lib/kernel/examples/erl_uds_dist/src/erl_uds_dist.erl](assets/erl_uds_dist.erl).
It implements the distribution over Unix domain sockets using the `gen_tcp` API
with distribution controllers implemented by processes. Compared to the original
`uds_dist` example using a port driver written in C, `erl_uds_dist` is written
entirely in Erlang.

### Exported Callback Functions

The following functions are mandatory:

- ```erlang
  listen(Name) ->
    {ok, {Listen, Address, Creation}} | {error, Error}
  listen(Name,Host) ->
    {ok, {Listen, Address, Creation}} | {error, Error}
  ```
  {: #listen }
  
  `listen/2` is called once in order to listen for incoming connection requests.
  The call is made when the distribution is brought up. The argument `Name` is
  the part of the node name before the `@` sign in the full node name. It can be
  either an atom or a string. The argument `Host` is the part of the node name
  after the `@` sign in the full node name. It is always a string.

  The return value consists of a `Listen` handle (which is later passed to the
  [`accept/1`](alt_dist.md#accept) callback), `Address` which is a
  `#net_address{}` record with information about the address for the node (the
  `#net_address{}` record is defined in `kernel/include/net_address.hrl`), and
  `Creation` which (currently) is an integer `1`, `2`, or `3`.

  If [`epmd`](epmd_cmd.md) is to be used for node discovery, you typically want
  to use the `erl_epmd` module (part of the `kernel` application) in order to
  register the listen port with `epmd` and retrieve `Creation` to use.

- ```
  address() ->
    Address
  ```
  {: #address }
  
  `address/0` is called in order to get the `Address` part of the
  [`listen/2`](alt_dist.md#listen) function without creating a listen socket.
  All fields except `address` have to be set in the returned record

  Example:

  ```erlang
  address() ->
      {ok, Host} = inet:gethostname(),
      #net_address{ host = Host, protocol = tcp, family = inet6 }.
  ```

- ```erlang
  accept(Listen) ->
    AcceptorPid
  ```
  {: #accept }
  
  `accept/1` should spawn a process that accepts connections. This process
  should preferably execute on `max` priority. The process identifier of this
  process should be returned.

  The `Listen` argument will be the same as the `Listen` handle part of the
  return value of the [`listen/1`](alt_dist.md#listen) callback above.
  `accept/1` is called only once when the distribution protocol is started.

  The caller of this function is a representative for `net_kernel` (this may or
  may not be the process registered as `net_kernel`) and is in this document
  identified as `Kernel`. When a connection has been accepted by the acceptor
  process, it needs to inform `Kernel` about the accepted connection. This is
  done by passing a message on the form:

  ```erlang
  Kernel ! {accept, AcceptorPid, DistController, Family, Proto}
  ```

  `DistController` is either the process or port identifier of the distribution
  controller for the connection. The distribution controller should be created
  by the acceptor processes when a new connection is accepted. Its job is to
  dispatch traffic on the connection.

  `Kernel` responds with one of the following messages:

  - **`{Kernel, controller, SupervisorPid}`** - The request was accepted and
    `SupervisorPid` is the process identifier of the connection supervisor
    process (which is created in the
    [`accept_connection/5`](alt_dist.md#accept_connection) callback).

  - **`{Kernel, unsupported_protocol}`** - The request was rejected. This is a
    fatal error. The acceptor process should terminate.

  When an accept sequence has been completed the acceptor process is expected to
  continue accepting further requests.

- ```
  accept_connection(AcceptorPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    ConnectionSupervisorPid
  ```
  {: #accept_connection }
  
  `accept_connection/5` should spawn a process that will perform the Erlang
  distribution handshake for the connection. If the handshake successfully
  completes it should continue to function as a connection supervisor. This
  process should preferably execute on `max` priority and should be linked to
  the caller. The `dist_util:net_ticker_spawn_options()` function can be called
  to get spawn options suitable for this process which can be passed directly to
  `erlang:spawn_opt/4`. `dist_util:net_ticker_spawn_options()` will by default
  return `[link, {priority, max}]`, but allows the user to configure more
  options using the
  [`net_ticker_spawn_options`](`e:kernel:kernel_app.md#net_ticker_spawn_options`)
  kernel parameter. The process identifier of this process should be returned.

  The arguments:

  - **`AcceptorPid`** - Process identifier of the process created by the
    [`accept/1`](alt_dist.md#accept) callback.

  - **`DistCtrl`** - The identifier of the distribution controller identifier
    created by the acceptor process. To be passed along to
    `dist_util:handshake_other_started(HsData)`.

  - **`MyNode`** - Node name of this node. To be passed along to
    `dist_util:handshake_other_started(HsData)`.

  - **`Allowed`** - To be passed along to
    `dist_util:handshake_other_started(HsData)`.

  - **`SetupTime`** - Time used for creating a setup timer by a call to
    `dist_util:start_timer(SetupTime)`. The timer should be passed along to
    `dist_util:handshake_other_started(HsData)`.

  The created process should provide callbacks and other information needed for
  the handshake in a [`#hs_data{}`](alt_dist.md#hs_data_record) record and call
  `dist_util:handshake_other_started(HsData)` with this record.

  `dist_util:handshake_other_started(HsData)` will perform the handshake and if
  the handshake successfully completes this process will then continue in a
  connection supervisor loop as long as the connection is up.

- ```
  setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ConnectionSupervisorPid
  ```
  {: #setup }
  
  `setup/5` should spawn a process that connects to `Node`. When connection has
  been established it should perform the Erlang distribution handshake for the
  connection. If the handshake successfully completes it should continue to
  function as a connection supervisor. This process should preferably execute on
  `max` priority and should be linked to the caller. The
  `dist_util:net_ticker_spawn_options()` function can be called to get spawn
  options suitable for this process which can be passed directly to
  `erlang:spawn_opt/4`. `dist_util:net_ticker_spawn_options()` will by default
  return `[link, {priority, max}]`, but allows the user to configure more
  options using the
  [`net_ticker_spawn_options`](`e:kernel:kernel_app.md#net_ticker_spawn_options`)
  kernel parameter. The process identifier of this process should be returned.

  The arguments:

  - **`Node`** - Node name of remote node. To be passed along to
    `dist_util:handshake_we_started(HsData)`.

  - **`Type`** - Connection type. To be passed along to
    `dist_util:handshake_we_started(HsData)`.

  - **`MyNode`** - Node name of this node. To be passed along to
    `dist_util:handshake_we_started(HsData)`.

  - **`LongOrShortNames`** - Either the atom `longnames` or the atom
    `shortnames` indicating whether long or short names is used.

  - **`SetupTime`** - Time used for creating a setup timer by a call to
    `dist_util:start_timer(SetupTime)`. The timer should be passed along to
    `dist_util:handshake_we_started(HsData)`.

  The caller of this function is a representative for `net_kernel` (this may or
  may not be the process registered as `net_kernel`) and is in this document
  identified as `Kernel`.

  This function should, besides spawning the connection supervisor, also create
  a distribution controller. The distribution controller is either a process or
  a port which is responsible for dispatching traffic.

  The created process should provide callbacks and other information needed for
  the handshake in a [`#hs_data{}`](alt_dist.md#hs_data_record) record and call
  `dist_util:handshake_we_started(HsData)` with this record.

  `dist_util:handshake_we_started(HsData)` will perform the handshake and the
  handshake successfully completes this process will then continue in a
  connection supervisor loop as long as the connection is up.

- ```
  close(Listen) ->
    void()
  ```
  {: #close }
  
  Called in order to close the `Listen` handle that originally was passed from
  the [`listen/1`](alt_dist.md#listen) callback.

- ```
  select(NodeName) ->
    boolean()
  ```
  {: #select }
  
  Return `true` if the host name part of the `NodeName` is valid for use with
  this protocol; otherwise, `false`.

There are also two optional functions that may be exported:

- ```
  setopts(Listen, Opts) ->
    ok | {error, Error}
  ```
  {: #setopts }
  
  The argument `Listen` is the handle originally passed from the
  [`listen/1`](alt_dist.md#listen) callback. The argument `Opts` is a list of
  options to set on future connections.

- ```
  getopts(Listen, Opts) ->
    {ok, OptionValues} | {error, Error}
  ```
  {: #getopts }
  
  The argument `Listen` is the handle originally passed from the
  [`listen/1`](alt_dist.md#listen) callback. The argument `Opts` is a list of
  options to read for future connections.

[](){: #hs_data_record }

### The #hs_data\{\} Record

The `dist_util:handshake_we_started/1` and `dist_util:handshake_other_started/1`
functions takes a `#hs_data{}` record as argument. There are quite a lot of
fields in this record that you need to set. The record is defined in
`kernel/include/dist_util.hrl`. Not documented fields should not be set, i.e.,
should be left as `undefined`.

The following `#hs_data{}` record fields need to be set unless otherwise stated:

- **`kernel_pid`{: #hs_data_kernel_pid }** - Process identifier of the `Kernel`
  process. That is, the process that called either
  [`setup/5`](alt_dist.md#setup) or
  [`accept_connection/5`](alt_dist.md#accept_connection).

- **`other_node`{: #hs_data_other_node }** - Name of the other node. This field
  is only mandatory when this node initiates the connection. That is, when
  connection is set up via [`setup/5`](alt_dist.md#setup).

- **`this_node`{: #hs_data_this_node }** - The node name of this node.

- **`socket`{: #hs_data_socket }** - The identifier of the distribution
  controller.

- **`timer`{: #hs_data_timer }** - The timer created using
  `dist_util:start_timer/1`.

- **`allowed`{: #hs_data_allowed }** - Information passed as `Allowed` to
  `accept_connection/5`. This field is only mandatory when the remote node
  initiated the connection. That is, when the connection is set up via
  [`accept_connection/5`](alt_dist.md#accept_connection).

- **`f_send`{: #hs_data_f_send }** - A fun with the following signature:

  ```erlang
  fun (DistCtrlr, Data) -> ok | {error, Error}
  ```

  where `DistCtrlr` is the identifier of the distribution controller and `Data`
  is io data to pass to the other side.

  Only used during handshake phase.

- **`f_recv`{: #hs_data_f_recv }** - A fun with the following signature:

  ```erlang
  fun (DistCtrlr, Length) -> {ok, Packet} | {error, Reason}
  ```

  where `DistCtrlr` is the identifier of the distribution controller. If
  `Length` is `0`, all available bytes should be returned. If `Length > 0`,
  exactly `Length` bytes should be returned, or an error; possibly discarding
  less than `Length` bytes of data when the connection is closed from the other
  side. It is used for passive receive of data from the other end.

  Only used during handshake phase.

- **`f_setopts_pre_nodeup`{: #hs_data_f_setopts_pre_nodeup }** - A fun with the
  following signature:

  ```erlang
  fun (DistCtrlr) -> ok | {error, Error}
  ```

  where `DistCtrlr` is the identifier of the distribution controller. Called
  just before the distribution channel is taken up for normal traffic.

  Only used during handshake phase.

- **`f_setopts_post_nodeup`{: #hs_data_f_setopts_post_nodeup }** - A fun with
  the following signature:

  ```erlang
  fun (DistCtrlr) -> ok | {error, Error}
  ```

  where `DistCtrlr` is the identifier of the distribution controller. Called
  just after distribution channel has been taken up for normal traffic.

  Only used during handshake phase.

- **`f_getll`{: #hs_data_f_getll }** - A fun with the following signature:

  ```erlang
  fun (DistCtrlr) -> ID
  ```

  where `DistCtrlr` is the identifier of the distribution controller and `ID` is
  the identifier of the low level entity that handles the connection (often
  `DistCtrlr` itself).

  Only used during handshake phase.

- **`f_address`{: #hs_data_f_address }** - A fun with the following signature:

  ```erlang
  fun (DistCtrlr, Node) -> NetAddress
  ```

  where `DistCtrlr` is the identifier of the distribution controller, `Node` is
  the node name of the node on the other end, and `NetAddress` is a
  `#net_address{}` record with information about the address for the `Node` on
  the other end of the connection. The `#net_address{}` record is defined in
  `kernel/include/net_address.hrl`.

  Only used during handshake phase.

- **`mf_tick`{: #hs_data_mf_tick }** - A fun with the following signature:

  ```erlang
  fun (DistCtrlr) -> void()
  ```

  where `DistCtrlr` is the identifier of the distribution controller. This
  function should send information over the connection that is not interpreted
  by the other end while increasing the statistics of received packets on the
  other end. This is usually implemented by sending an empty packet.

  > #### Note {: .info }
  >
  > It is of vital importance that this operation does not block the caller for
  > a long time. This since it is called from the connection supervisor.

  Used when connection is up.

- **`mf_getstat`{: #hs_data_mf_getstat }** - A fun with the following signature:

  ```erlang
  fun (DistCtrlr) -> {ok, Received, Sent, PendSend}
  ```

  where `DistCtrlr` is the identifier of the distribution controller, `Received`
  is received packets, `Sent` is sent packets, and `PendSend` is amount of data
  in queue to be sent (typically in bytes, but `dist_util` only checks whether
  the value is non-zero to know there is data in queue) or a `t:boolean/0`
  indicating whether there are packets in queue to be sent.

  > #### Note {: .info }
  >
  > It is of vital importance that this operation does not block the caller for
  > a long time. This since it is called from the connection supervisor.

  Used when connection is up.

- **`request_type`{: #hs_data_request_type }** - The request `Type` as passed to
  [`setup/5`](alt_dist.md#setup). This is only mandatory when the connection has
  been initiated by this node. That is, the connection is set up via `setup/5`.

- **`mf_setopts`{: #hs_data_mf_setopts }** - A fun with the following signature:

  ```erlang
  fun (DistCtrl, Opts) -> ok | {error, Error}
  ```

  where `DistCtrlr` is the identifier of the distribution controller and `Opts`
  is a list of options to set on the connection.

  This function is optional. Used when connection is up.

- **`mf_getopts`{: #hs_data_mf_getopts }** - A fun with the following signature:

  ```erlang
  fun (DistCtrl, Opts) -> {ok, OptionValues} | {error, Error}
  ```

  where `DistCtrlr` is the identifier of the distribution controller and `Opts`
  is a list of options to read for the connection.

  This function is optional. Used when connection is up.

- **`f_handshake_complete`{: #hs_data_f_handshake_complete }** - A fun with the
  following signature:

  ```erlang
  fun (DistCtrlr, Node, DHandle) -> void()
  ```

  where `DistCtrlr` is the identifier of the distribution controller, `Node` is
  the node name of the node connected at the other end, and `DHandle` is a
  distribution handle needed by a distribution controller process when calling
  the following BIFs:

  - `erlang:dist_ctrl_get_data/1`
  - `erlang:dist_ctrl_get_data_notification/1`
  - `erlang:dist_ctrl_input_handler/2`
  - `erlang:dist_ctrl_put_data/2`

  This function is called when the handshake has completed and the distribution
  channel is up. The distribution controller can begin dispatching traffic over
  the channel. This function is optional.

  Only used during handshake phase.

- **`add_flags`{: #hs_data_add_flags }** -
  [Distribution flags](erl_dist_protocol.md#dflags) to add to the connection.
  Currently all (non obsolete) flags will automatically be enabled.

  This flag field is optional.

- **`reject_flags`{: #hs_data_reject_flags }** -
  [Distribution flags](erl_dist_protocol.md#dflags) to reject. Currently the
  following distribution flags can be rejected:

  - **`DFLAG_DIST_HDR_ATOM_CACHE`** - Do not use atom cache over this
    connection.

  - **`DFLAG_FRAGMENTS`** - Split large distribution messages into multiple
    fragments.

  This flag field is optional.

  See also [Distribution Data Delivery](alt_dist.md#distribution-data-delivery)

- **`require_flags`{: #hs_data_require_flags }** - Require these
  [distribution flags](erl_dist_protocol.md#dflags) to be used. The connection
  will be aborted during the handshake if the other end does not use them.

  This flag field is optional.

### Distribution Data Delivery

When using the default configuration, the data to pass over a connection needs
to be delivered as is to the node on the receiving end in the _exact same
order_, with no loss of data what so ever, as sent from the sending node.

The data delivery order can be relaxed by disabling features that require strict
ordering. This is done by passing the
[distribution flags](erl_dist_protocol.md#dflags) returned by
`dist_util:strict_order_flags/0` in the
[`reject_flags`](alt_dist.md#hs_data_reject_flags) field of the
[`#hs_data{}`](alt_dist.md#hs_data_record) record used when setting up the
connection. When relaxed ordering is used, only the order of signals with the
same sender/receiver pair has to be preserved. However, note that disabling the
features that require strict ordering may have a negative impact on performance,
throughput, and/or latency.

### Enable Your Distribution Module

For `net_kernel` to find out which distribution module to use, the `erl`
command-line argument `-proto_dist` is used. It is followed by one or more
distribution module names, with suffix "\_dist" removed. That is, `gen_tcp_dist`
as a distribution module is specified as `-proto_dist gen_tcp`.

If no `epmd` (TCP port mapper daemon) is used, also command-line option
`-no_epmd` is to be specified, which makes Erlang skip the `epmd` startup, both
as an OS process and as an Erlang ditto.

## The Driver

> #### Note {: .info }
>
> This section was written a long time ago. Most of it is still valid, but some
> things have changed since then. Some updates have been made to the
> documentation of the driver presented here, but more can be done and is
> planned for the future. The reader is encouraged to read the
> [`erl_driver`](erl_driver.md) and [`driver_entry`](driver_entry.md)
> documentation also.

Although Erlang drivers in general can be beyond the scope of this section, a
brief introduction seems to be in place.

### Drivers in General

An Erlang driver is a native code module written in C (or assembler), which
serves as an interface for some special operating system service. This is a
general mechanism that is used throughout the Erlang emulator for all kinds of
I/O. An Erlang driver can be dynamically linked (or loaded) to the Erlang
emulator at runtime by using the `erl_ddll` Erlang module. Some of the drivers
in OTP are however statically linked to the runtime system, but that is more an
optimization than a necessity.

The driver data types and the functions available to the driver writer are
defined in header file `erl_driver.h` seated in Erlang's include directory. See
the [erl_driver](erl_driver.md) documentation for details of which functions are
available.

When writing a driver to make a communications protocol available to Erlang, one
should know just about everything worth knowing about that particular protocol.
All operation must be non-blocking and all possible situations are to be
accounted for in the driver. A non-stable driver will affect and/or crash the
whole Erlang runtime system.

The emulator calls the driver in the following situations:

- When the driver is loaded. This callback must have a special name and inform
  the emulator of what callbacks are to be used by returning a pointer to a
  `ErlDrvEntry` struct, which is to be properly filled in (see below).
- When a port to the driver is opened (by a `open_port` call from Erlang). This
  routine is to set up internal data structures and return an opaque data entity
  of the type `ErlDrvData`, which is a data type large enough to hold a pointer.
  The pointer returned by this function is the first argument to all other
  callbacks concerning this particular port. It is usually called the port
  handle. The emulator only stores the handle and does never try to interpret
  it, why it can be virtually anything (anything not larger than a pointer that
  is) and can point to anything if it is a pointer. Usually this pointer refers
  to a structure holding information about the particular port, as it does in
  the example.
- When an Erlang process sends data to the port. The data arrives as a buffer of
  bytes, the interpretation is not defined, but is up to the implementor. This
  callback returns nothing to the caller, answers are sent to the caller as
  messages (using a routine called `driver_output` available to all drivers).
  There is also a way to talk in a synchronous way to drivers, described below.
  There can be an additional callback function for handling data that is
  fragmented (sent in a deep io-list). That interface gets the data in a form
  suitable for Unix `writev` rather than in a single buffer. There is no need
  for a distribution driver to implement such a callback, so we will not.
- When a file descriptor is signaled for input. This callback is called when the
  emulator detects input on a file descriptor that the driver has marked for
  monitoring by using the interface `driver_select`. The mechanism of driver
  select makes it possible to read non-blocking from file descriptors by calling
  `driver_select` when reading is needed, and then do the reading in this
  callback (when reading is possible). The typical scenario is that
  `driver_select` is called when an Erlang process orders a read operation, and
  that this routine sends the answer when data is available on the file
  descriptor.
- When a file descriptor is signaled for output. This callback is called in a
  similar way as the previous, but when writing to a file descriptor is
  possible. The usual scenario is that Erlang orders writing on a file
  descriptor and that the driver calls `driver_select`. When the descriptor is
  ready for output, this callback is called and the driver can try to send the
  output. Queuing can be involved in such operations, and there are convenient
  queue routines available to the driver writer to use.
- When a port is closed, either by an Erlang process or by the driver calling
  one of the `driver_failure_XXX` routines. This routine is to clean up
  everything connected to one particular port. When other callbacks call a
  `driver_failure_XXX` routine, this routine is immediately called. The callback
  routine issuing the error can make no more use of the data structures for the
  port, as this routine surely has freed all associated data and closed all file
  descriptors. If the queue utility available to driver writer is used, this
  routine is however _not_ called until the queue is empty.
- When an Erlang process calls `erlang:port_control/3`, which is a synchronous
  interface to drivers. The control interface is used to set driver options,
  change states of ports, and so on. This interface is used a lot in the
  example.
- When a timer expires. The driver can set timers with the function
  `driver_set_timer`. When such timers expire, a specific callback function is
  called. No timers are used in the example.
- When the whole driver is unloaded. Every resource allocated by the driver is
  to be freed.

### The Data Structures of the Distribution Driver

The driver used for Erlang distribution is to implement a reliable, order
maintaining, variable length packet-oriented protocol. All error correction,
resending and such need to be implemented in the driver or by the underlying
communications protocol. If the protocol is stream-oriented (as is the case with
both TCP/IP and our streamed Unix domain sockets), some mechanism for packaging
is needed. We will use the simple method of having a header of four bytes
containing the length of the package in a big-endian 32-bit integer. As Unix
domain sockets only can be used between processes on the same machine, we do not
need to code the integer in some special endianness, but we will do it anyway
because in most situation you need to do it. Unix domain sockets are reliable
and order maintaining, so we do not need to implement resends and such in the
driver.

We start writing the example Unix domain sockets driver by declaring prototypes
and filling in a static `ErlDrvEntry` structure:

```c
( 1) #include <stdio.h>
( 2) #include <stdlib.h>
( 3) #include <string.h>
( 4) #include <unistd.h>
( 5) #include <errno.h>
( 6) #include <sys/types.h>
( 7) #include <sys/stat.h>
( 8) #include <sys/socket.h>
( 9) #include <sys/un.h>
(10) #include <fcntl.h>

(11) #define HAVE_UIO_H
(12) #include "erl_driver.h"

(13) /*
(14) ** Interface routines
(15) */
(16) static ErlDrvData uds_start(ErlDrvPort port, char *buff);
(17) static void uds_stop(ErlDrvData handle);
(18) static void uds_command(ErlDrvData handle, char *buff, int bufflen);
(19) static void uds_input(ErlDrvData handle, ErlDrvEvent event);
(20) static void uds_output(ErlDrvData handle, ErlDrvEvent event);
(21) static void uds_finish(void);
(22) static int uds_control(ErlDrvData handle, unsigned int command,
(23)                        char* buf, int count, char** res, int res_size);

(24) /* The driver entry */
(25) static ErlDrvEntry uds_driver_entry = {
(26)     NULL,                            /* init, N/A */
(27)     uds_start,                       /* start, called when port is opened */
(28)     uds_stop,                        /* stop, called when port is closed */
(29)     uds_command,                     /* output, called when erlang has sent */
(30)     uds_input,                       /* ready_input, called when input
(31)                                         descriptor ready */
(32)     uds_output,                      /* ready_output, called when output
(33)                                         descriptor ready */
(34)     "uds_drv",                       /* char *driver_name, the argument
(35)                                         to open_port */
(36)     uds_finish,                      /* finish, called when unloaded */
(37)     NULL,                            /* void * that is not used (BC) */
(38)     uds_control,                     /* control, port_control callback */
(39)     NULL,                            /* timeout, called on timeouts */
(40)     NULL,                            /* outputv, vector output interface */
(41)     NULL,                            /* ready_async callback */
(42)     NULL,                            /* flush callback */
(43)     NULL,                            /* call callback */
(44)     NULL,                            /* event callback */
(45)     ERL_DRV_EXTENDED_MARKER,         /* Extended driver interface marker */
(46)     ERL_DRV_EXTENDED_MAJOR_VERSION,  /* Major version number */
(47)     ERL_DRV_EXTENDED_MINOR_VERSION,  /* Minor version number */
(48)     ERL_DRV_FLAG_SOFT_BUSY,          /* Driver flags. Soft busy flag is
(49)                                         required for distribution drivers */
(50)     NULL,                            /* Reserved for internal use */
(51)     NULL,                            /* process_exit callback */
(52)     NULL                             /* stop_select callback */
(53) };
```

On line 1-10 the OS headers needed for the driver are included. As this driver
is written for Solaris, we know that the header `uio.h` exists. So the
preprocessor variable `HAVE_UIO_H` can be defined before `erl_driver.h` is
included on line 12. The definition of `HAVE_UIO_H` will make the I/O vectors
used in Erlang's driver queues to correspond to the operating systems ditto,
which is very convenient.

On line 16-23 the different callback functions are declared ("forward
declarations").

The driver structure is similar for statically linked-in drivers and dynamically
loaded. However, some of the fields are to be left empty (that is, initialized
to NULL) in the different types of drivers. The first field (the `init` function
pointer) is always left blank in a dynamically loaded driver, see line 26.
`NULL` on line 37 is always to be there, the field is no longer used and is
retained for backward compatibility. No timers are used in this driver, why no
callback for timers is needed. The `outputv` field (line 40) can be used to
implement an interface similar to Unix `writev` for output. The Erlang runtime
system could previously not use `outputv` for the distribution, but it can as
from ERTS 5.7.2. As this driver was written before ERTS 5.7.2 it does not use
the `outputv` callback. Using the `outputv` callback is preferred, as it reduces
copying of data. (We will however use scatter/gather I/O internally in the
driver.)

As from ERTS 5.5.3 the driver interface was extended with version control and
the possibility to pass capability information. Capability flags are present on
line 48. As from ERTS 5.7.4 flag
[`ERL_DRV_FLAG_SOFT_BUSY`](driver_entry.md#driver_flags) is required for drivers
that are to be used by the distribution. The soft busy flag implies that the
driver can handle calls to the `output` and `outputv` callbacks although it has
marked itself as busy. This has always been a requirement on drivers used by the
distribution, but no capability information has been available about this
previously. For more information, see
[`erl_driver:set_busy_port()`](erl_driver.md#set_busy_port)).

This driver was written before the runtime system had SMP support. The driver
will still function in the runtime system with SMP support, but performance will
suffer from lock contention on the driver lock used for the driver. This can be
alleviated by reviewing and perhaps rewriting the code so that each instance of
the driver safely can execute in parallel. When instances safely can execute in
parallel, it is safe to enable instance-specific locking on the driver. This is
done by passing [`ERL_DRV_FLAG_USE_PORT_LOCKING`](driver_entry.md#driver_flags)
as a driver flag. This is left as an exercise for the reader.

Thus, the defined callbacks are as follows:

- **`uds_start`** - Must initiate data for a port. We do not create any sockets
  here, only initialize data structures.

- **`uds_stop`** - Called when a port is closed.

- **`uds_command`** - Handles messages from Erlang. The messages can either be
  plain data to be sent or more subtle instructions to the driver. This function
  is here mostly for data pumping.

- **`uds_input`** - Called when there is something to read from a socket.

- **`uds_output`** - Called when it is possible to write to a socket.

- **`uds_finish`** - Called when the driver is unloaded. A distribution driver
  will never be unloaded, but we include this for completeness. To be able to
  clean up after oneself is always a good thing.

- **`uds_control`** - The `erlang:port_control/3` callback, which is used a lot
  in this implementation.

The ports implemented by this driver operate in two major modes, named `command`
and `data`. In `command` mode, only passive reading and writing (like
`gen_tcp:recv`/`gen_tcp:send`) can be done. The port is in this mode during the
distribution handshake. When the connection is up, the port is switched to
`data` mode and all data is immediately read and passed further to the Erlang
emulator. In `data` mode, no data arriving to `uds_command` is interpreted, only
packaged and sent out on the socket. The `uds_control` callback does the
switching between those two modes.

While `net_kernel` informs different subsystems that the connection is coming
up, the port is to accept data to send. However, the port should not receive any
data, to avoid that data arrives from another node before every kernel subsystem
is prepared to handle it. A third mode, named `intermediate`, is used for this
intermediate stage.

An enum is defined for the different types of ports:

```c
( 1) typedef enum {
( 2)     portTypeUnknown,      /* An uninitialized port */
( 3)     portTypeListener,     /* A listening port/socket */
( 4)     portTypeAcceptor,     /* An intermediate stage when accepting
( 5)                              on a listen port */
( 6)     portTypeConnector,    /* An intermediate stage when connecting */
( 7)     portTypeCommand,      /* A connected open port in command mode */
( 8)     portTypeIntermediate, /* A connected open port in special
( 9)                              half active mode */
(10)     portTypeData          /* A connected open port in data mode */
(11) } PortType;
```

The different types are as follows:

- **`portTypeUnknown`** - The type a port has when it is opened, but not bound
  to any file descriptor.

- **`portTypeListener`** - A port that is connected to a listen socket. This
  port does not do much, no data pumping is done on this socket, but read data
  is available when one is trying to do an accept on the port.

- **`portTypeAcceptor`** - This port is to represent the result of an accept
  operation. It is created when one wants to accept from a listen socket, and it
  is converted to a `portTypeCommand` when the accept succeeds.

- **`portTypeConnector`** - Very similar to `portTypeAcceptor`, an intermediate
  stage between the request for a connect operation and that the socket is
  connected to an accepting ditto in the other end. When the sockets are
  connected, the port switches type to `portTypeCommand`.

- **`portTypeCommand`** - A connected socket (or accepted socket) in `command`
  mode mentioned earlier.

- **`portTypeIntermediate`** - The intermediate stage for a connected socket.
  There is to be no processing of input for this socket.

- **`portTypeData`** - The mode where data is pumped through the port and the
  `uds_command` routine regards every call as a call where sending is wanted. In
  this mode, all input available is read and sent to Erlang when it arrives on
  the socket, much like in the active mode of a `gen_tcp` socket.

We study the state that is needed for the ports. Notice that not all fields are
used for all types of ports. Some space could be saved by using unions, but that
would clutter the code with multiple indirections, so here is used one struct
for all types of ports, for readability:

```c
( 1) typedef unsigned char Byte;
( 2) typedef unsigned int Word;

( 3) typedef struct uds_data {
( 4)     int fd;                   /* File descriptor */
( 5)     ErlDrvPort port;          /* The port identifier */
( 6)     int lockfd;               /* The file descriptor for a lock file in
( 7)                                  case of listen sockets */
( 8)     Byte creation;            /* The creation serial derived from the
( 9)                                  lock file */
(10)     PortType type;            /* Type of port */
(11)     char *name;               /* Short name of socket for unlink */
(12)     Word sent;                /* Bytes sent */
(13)     Word received;            /* Bytes received */
(14)     struct uds_data *partner; /* The partner in an accept/listen pair */
(15)     struct uds_data *next;    /* Next structure in list */
(16)     /* The input buffer and its data */
(17)     int buffer_size;          /* The allocated size of the input buffer */
(18)     int buffer_pos;           /* Current position in input buffer */
(19)     int header_pos;           /* Where the current header is in the
(20)                                  input buffer */
(21)     Byte *buffer;             /* The actual input buffer */
(22) } UdsData;
```

This structure is used for all types of ports although some fields are useless
for some types. The least memory consuming solution would be to arrange this
structure as a union of structures. However, the multiple indirections in the
code to access a field in such a structure would clutter the code too much for
an example.

The fields in the structure are as follows:

- **`fd`** - The file descriptor of the socket associated with the port.

- **`port`** - The port identifier for the port that this structure corresponds
  to. It is needed for most `driver_XXX` calls from the driver back to the
  emulator.

- **`lockfd`** - If the socket is a listen socket, we use a separate (regular)
  file for two purposes:

  - We want a locking mechanism that gives no race conditions, to be sure if
    another Erlang node uses the listen socket name we require or if the file is
    only left there from a previous (crashed) session.
  - We store the `creation` serial number in the file. The `creation` is a
    number that is to change between different instances of different Erlang
    emulators with the same name, so that process identifiers from one emulator
    do not become valid when sent to a new emulator with the same distribution
    name. The creation can be from 0 through 3 (two bits) and is stored in every
    process identifier sent to another node.

    In a system with TCP-based distribution, this data is kept in the _Erlang
    port mapper daemon_ (`epmd`), which is contacted when a distributed node
    starts. The lock file and a convention for the UDS listen socket's name
    remove the need for `epmd` when using this distribution module. UDS is
    always restricted to one host, why avoiding a port mapper is easy.

- **`creation`** - The creation number for a listen socket, which is calculated
  as (the value found in the lock-file + 1) rem 4. This creation value is also
  written back into the lock file, so that the next invocation of the emulator
  finds our value in the file.

- **`type`** - The current type/state of the port, which can be one of the
  values declared above.

- **`name`** - The name of the socket file (the path prefix removed), which
  allows for deletion (`unlink`) when the socket is closed.

- **`sent`** - How many bytes that have been sent over the socket. This can
  wrap, but that is no problem for the distribution, as the Erlang distribution
  is only interested in if this value has changed. (The Erlang `net_kernel`
  `ticker` uses this value by calling the driver to fetch it, which is done
  through the `erlang:port_control/3` routine.)

- **`received`** - How many bytes that are read (received) from the socket, used
  in similar ways as `sent`.

- **`partner`** - A pointer to another port structure, which is either the
  listen port from which this port is accepting a connection or conversely. The
  "partner relation" is always bidirectional.

- **`next`** - Pointer to next structure in a linked list of all port
  structures. This list is used when accepting connections and when the driver
  is unloaded.

- **`buffer_size`, `buffer_pos`, `header_pos`, `buffer`** - Data for input
  buffering. For details about the input buffering, see the source code in
  directory `kernel/examples`. That certainly goes beyond the scope of this
  section.

### Selected Parts of the Distribution Driver Implementation

The implementation of the distribution driver is not completely covered here,
details about buffering and other things unrelated to driver writing are not
explained. Likewise are some peculiarities of the UDS protocol not explained in
detail. The chosen protocol is not important.

Prototypes for the driver callback routines can be found in the `erl_driver.h`
header file.

The driver initialization routine is (usually) declared with a macro to make the
driver easier to port between different operating systems (and flavors of
systems). This is the only routine that must have a well-defined name. All other
callbacks are reached through the driver structure. The macro to use is named
`DRIVER_INIT` and takes the driver name as parameter:

```c
(1) /* Beginning of linked list of ports */
(2) static UdsData *first_data;

(3) DRIVER_INIT(uds_drv)
(4) {
(5)     first_data = NULL;
(6)     return &uds_driver_entry;
(7) }
```

The routine initializes the single global data structure and returns a pointer
to the driver entry. The routine is called when `erl_ddll:load_driver` is called
from Erlang.

The `uds_start` routine is called when a port is opened from Erlang. In this
case, we only allocate a structure and initialize it. Creating the actual socket
is left to the `uds_command` routine.

```c
( 1) static ErlDrvData uds_start(ErlDrvPort port, char *buff)
( 2) {
( 3)     UdsData *ud;
( 4)
( 5)     ud = ALLOC(sizeof(UdsData));
( 6)     ud->fd = -1;
( 7)     ud->lockfd = -1;
( 8)     ud->creation = 0;
( 9)     ud->port = port;
(10)     ud->type = portTypeUnknown;
(11)     ud->name = NULL;
(12)     ud->buffer_size = 0;
(13)     ud->buffer_pos = 0;
(14)     ud->header_pos = 0;
(15)     ud->buffer = NULL;
(16)     ud->sent = 0;
(17)     ud->received = 0;
(18)     ud->partner = NULL;
(19)     ud->next = first_data;
(20)     first_data = ud;
(21)
(22)     return((ErlDrvData) ud);
(23) }
```

Every data item is initialized, so that no problems arise when a newly created
port is closed (without there being any corresponding socket). This routine is
called when [`open_port({spawn, "uds_drv"},[])`](`open_port/2`) is called from
Erlang.

The `uds_command` routine is the routine called when an Erlang process sends
data to the port. This routine handles all asynchronous commands when the port
is in `command` mode and the sending of all data when the port is in `data`
mode:

```c
( 1) static void uds_command(ErlDrvData handle, char *buff, int bufflen)
( 2) {
( 3)     UdsData *ud = (UdsData *) handle;

( 4)     if (ud->type == portTypeData || ud->type == portTypeIntermediate) {
( 5)         DEBUGF(("Passive do_send %d",bufflen));
( 6)         do_send(ud, buff + 1, bufflen - 1); /* XXX */
( 7)         return;
( 8)     }
( 9)     if (bufflen == 0) {
(10)         return;
(11)     }
(12)     switch (*buff) {
(13)     case 'L':
(14)         if (ud->type != portTypeUnknown) {
(15)             driver_failure_posix(ud->port, ENOTSUP);
(16)             return;
(17)         }
(18)         uds_command_listen(ud,buff,bufflen);
(19)         return;
(20)     case 'A':
(21)         if (ud->type != portTypeUnknown) {
(22)             driver_failure_posix(ud->port, ENOTSUP);
(23)             return;
(24)         }
(25)         uds_command_accept(ud,buff,bufflen);
(26)         return;
(27)     case 'C':
(28)         if (ud->type != portTypeUnknown) {
(29)             driver_failure_posix(ud->port, ENOTSUP);
(30)             return;
(31)         }
(32)         uds_command_connect(ud,buff,bufflen);
(33)         return;
(34)     case 'S':
(35)         if (ud->type != portTypeCommand) {
(36)             driver_failure_posix(ud->port, ENOTSUP);
(37)             return;
(38)         }
(39)         do_send(ud, buff + 1, bufflen - 1);
(40)         return;
(41)     case 'R':
(42)         if (ud->type != portTypeCommand) {
(43)             driver_failure_posix(ud->port, ENOTSUP);
(44)             return;
(45)         }
(46)         do_recv(ud);
(47)         return;
(48)     default:
(49)         return;
(50)     }
(51) }
```

The command routine takes three parameters; the handle returned for the port by
`uds_start`, which is a pointer to the internal port structure, the data buffer,
and the length of the data buffer. The buffer is the data sent from Erlang (a
list of bytes) converted to an C array (of bytes).

If Erlang sends, for example, the list `[$a,$b,$c]` to the port, the `bufflen`
variable is `3` and the `buff` variable contains `{'a','b','c'}` (no `NULL`
termination). Usually the first byte is used as an opcode, which is the case in
this driver too (at least when the port is in `command` mode). The opcodes are
defined as follows:

- **`'L'<socket name>`** - Creates and listens on socket with the specified
  name.

- **`'A'<listen number as 32-bit big-endian>`** - Accepts from the listen socket
  identified by the specified identification number. The identification number
  is retrieved with the `uds_control` routine.

- **`'C'<socket name>`** - Connects to the socket named <socket name>.

- **`'S'<data>`** - Sends the data <data> on the connected/accepted socket (in
  `command` mode). The sending is acknowledged when the data has left this
  process.

- **`'R'`** - Receives one packet of data.

"One packet of data" in command `'R'` can be explained as follows. This driver
always sends data packaged with a 4 byte header containing a big-endian 32-bit
integer that represents the length of the data in the packet. There is no need
for different packet sizes or some kind of streamed mode, as this driver is for
the distribution only. Why is the header word coded explicitly in big-endian
when a UDS socket is local to the host? It is good practice when writing a
distribution driver, as distribution in practice usually crosses the host
boundaries.

On line 4-8 is handled the case where the port is in `data` mode or
`intermediate` mode and the remaining routine handles the different commands.
The routine uses the `driver_failure_posix()` routine to report errors (see, for
example, line 15). Notice that the failure routines make a call to the
`uds_stop` routine, which will remove the internal port data. The handle (and
the casted handle `ud`) is therefore _invalid pointers_ after a `driver_failure`
call and we should _return immediately_. The runtime system will send exit
signals to all linked processes.

The `uds_input` routine is called when data is available on a file descriptor
previously passed to the `driver_select` routine. This occurs typically when a
read command is issued and no data is available. The `do_recv` routine is as
follows:

```c
( 1) static void do_recv(UdsData *ud)
( 2) {
( 3)     int res;
( 4)     char *ibuf;
( 5)     for(;;) {
( 6)         if ((res = buffered_read_package(ud,&ibuf)) < 0) {
( 7)             if (res == NORMAL_READ_FAILURE) {
( 8)                 driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_READ, 1);
( 9)             } else {
(10)                 driver_failure_eof(ud->port);
(11)             }
(12)             return;
(13)         }
(14)         /* Got a package */
(15)         if (ud->type == portTypeCommand) {
(16)             ibuf[-1] = 'R'; /* There is always room for a single byte
(17)                                opcode before the actual buffer
(18)                                (where the packet header was) */
(19)             driver_output(ud->port,ibuf - 1, res + 1);
(20)             driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_READ,0);
(21)             return;
(22)         } else {
(23)             ibuf[-1] = DIST_MAGIC_RECV_TAG; /* XXX */
(24)             driver_output(ud->port,ibuf - 1, res + 1);
(25)             driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_READ,1);
(26)         }
(27)     }
(28) }
```

The routine tries to read data until a packet is read or the
`buffered_read_package` routine returns a `NORMAL_READ_FAILURE` (an internally
defined constant for the module, which means that the read operation resulted in
an `EWOULDBLOCK`). If the port is in `command` mode, the reading stops when one
package is read. If the port is in `data` mode, the reading continues until the
socket buffer is empty (read failure). If no more data can be read and more is
wanted (which is always the case when the socket is in `data` mode),
`driver_select` is called to make the `uds_input` callback be called when more
data is available for reading.

When the port is in `data` mode, all data is sent to Erlang in a format that
suits the distribution. In fact, the raw data will never reach any Erlang
process, but will be translated/interpreted by the emulator itself and then
delivered in the correct format to the correct processes. In the current
emulator version, received data is to be tagged with a single byte of 100. That
is what the macro `DIST_MAGIC_RECV_TAG` is defined to. The tagging of data in
the distribution can be changed in the future.

The `uds_input` routine handles other input events (like non-blocking `accept`),
but most importantly handle data arriving at the socket by calling `do_recv`:

```c
( 1) static void uds_input(ErlDrvData handle, ErlDrvEvent event)
( 2) {
( 3)     UdsData *ud = (UdsData *) handle;

( 4)     if (ud->type == portTypeListener) {
( 5)         UdsData *ad = ud->partner;
( 6)         struct sockaddr_un peer;
( 7)         int pl = sizeof(struct sockaddr_un);
( 8)         int fd;

( 9)         if ((fd = accept(ud->fd, (struct sockaddr *) &peer, &pl)) < 0) {
(10)             if (errno != EWOULDBLOCK) {
(11)                 driver_failure_posix(ud->port, errno);
(12)                 return;
(13)             }
(14)             return;
(15)         }
(16)         SET_NONBLOCKING(fd);
(17)         ad->fd = fd;
(18)         ad->partner = NULL;
(19)         ad->type = portTypeCommand;
(20)         ud->partner = NULL;
(21)         driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_READ, 0);
(22)         driver_output(ad->port, "Aok",3);
(23)         return;
(24)     }
(25)     do_recv(ud);
(26) }
```

The important line is the last line in the function: the `do_read` routine is
called to handle new input. The remaining function handles input on a listen
socket, which means that it is to be possible to do an accept on the socket,
which is also recognized as a read event.

The output mechanisms are similar to the input. The `do_send` routine is as
follows:

```c
( 1) static void do_send(UdsData *ud, char *buff, int bufflen)
( 2) {
( 3)     char header[4];
( 4)     int written;
( 5)     SysIOVec iov[2];
( 6)     ErlIOVec eio;
( 7)     ErlDrvBinary *binv[] = {NULL,NULL};

( 8)     put_packet_length(header, bufflen);
( 9)     iov[0].iov_base = (char *) header;
(10)     iov[0].iov_len = 4;
(11)     iov[1].iov_base = buff;
(12)     iov[1].iov_len = bufflen;
(13)     eio.iov = iov;
(14)     eio.binv = binv;
(15)     eio.vsize = 2;
(16)     eio.size = bufflen + 4;
(17)     written = 0;
(18)     if (driver_sizeq(ud->port) == 0) {
(19)         if ((written = writev(ud->fd, iov, 2)) == eio.size) {
(20)             ud->sent += written;
(21)             if (ud->type == portTypeCommand) {
(22)                 driver_output(ud->port, "Sok", 3);
(23)             }
(24)             return;
(25)         } else if (written < 0) {
(26)             if (errno != EWOULDBLOCK) {
(27)                 driver_failure_eof(ud->port);
(28)                 return;
(29)             } else {
(30)                 written = 0;
(31)             }
(32)         } else {
(33)             ud->sent += written;
(34)         }
(35)         /* Enqueue remaining */
(36)     }
(37)     driver_enqv(ud->port, &eio, written);
(38)     send_out_queue(ud);
(39) }
```

This driver uses the `writev` system call to send data onto the socket. A
combination of `writev` and the driver output queues is very convenient. An
`ErlIOVec` structure contains a `SysIOVec` (which is equivalent to the
`struct iovec` structure defined in `uio.h`. The `ErlIOVec` also contains an
array of `ErlDrvBinary` pointers, of the same length as the number of buffers in
the I/O vector itself. One can use this to allocate the binaries for the queue
"manually" in the driver, but here the binary array is filled with `NULL` values
(line 7). The runtime system then allocates its own buffers when `driver_enqv`
is called (line 37).

The routine builds an I/O vector containing the header bytes and the buffer (the
opcode has been removed and the buffer length decreased by the output routine).
If the queue is empty, we write the data directly to the socket (or at least try
to). If any data is left, it is stored in the queue and then we try to send the
queue (line 38). An acknowledgement is sent when the message is delivered
completely (line 22). The `send_out_queue` sends acknowledgements if the sending
is completed there. If the port is in `command` mode, the Erlang code serializes
the send operations so that only one packet can be waiting for delivery at a
time. Therefore the acknowledgement can be sent whenever the queue is empty.

The `send_out_queue` routine is as follows:

```c
( 1) static int send_out_queue(UdsData *ud)
( 2) {
( 3)     for(;;) {
( 4)         int vlen;
( 5)         SysIOVec *tmp = driver_peekq(ud->port, &vlen);
( 6)         int wrote;
( 7)         if (tmp == NULL) {
( 8)             driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_WRITE, 0);
( 9)             if (ud->type == portTypeCommand) {
(10)                 driver_output(ud->port, "Sok", 3);
(11)             }
(12)             return 0;
(13)         }
(14)         if (vlen > IO_VECTOR_MAX) {
(15)             vlen = IO_VECTOR_MAX;
(16)         }
(17)         if ((wrote = writev(ud->fd, tmp, vlen)) < 0) {
(18)             if (errno == EWOULDBLOCK) {
(19)                 driver_select(ud->port, (ErlDrvEvent) ud->fd,
(20)                               DO_WRITE, 1);
(21)                 return 0;
(22)             } else {
(23)                 driver_failure_eof(ud->port);
(24)                 return -1;
(25)             }
(26)         }
(27)         driver_deq(ud->port, wrote);
(28)         ud->sent += wrote;
(29)     }
(30) }
```

We simply pick out an I/O vector from the queue (which is the whole queue as a
`SysIOVec`). If the I/O vector is too long (`IO_VECTOR_MAX` is defined to 16),
the vector length is decreased (line 15), otherwise the `writev` call (line 17)
fails. Writing is tried and anything written is dequeued (line 27). If the write
fails with `EWOULDBLOCK` (notice that all sockets are in non-blocking mode),
`driver_select` is called to make the `uds_output` routine be called when there
is space to write again.

We continue trying to write until the queue is empty or the writing blocks.

The routine above is called from the `uds_output` routine:

```c
( 1) static void uds_output(ErlDrvData handle, ErlDrvEvent event)
( 2) {
( 3)    UdsData *ud = (UdsData *) handle;
( 4)    if (ud->type == portTypeConnector) {
( 5)        ud->type = portTypeCommand;
( 6)        driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_WRITE, 0);
( 7)        driver_output(ud->port, "Cok",3);
( 8)        return;
( 9)    }
(10)    send_out_queue(ud);
(11) }
```

The routine is simple: it first handles the fact that the output select will
concern a socket in the business of connecting (and the connecting blocked). If
the socket is in a connected state, it simply sends the output queue. This
routine is called when it is possible to write to a socket where we have an
output queue, so there is no question what to do.

The driver implements a control interface, which is a synchronous interface
called when Erlang calls `erlang:port_control/3`. Only this interface can
control the driver when it is in `data` mode. It can be called with the
following opcodes:

- **`'C'`** - Sets port in `command` mode.

- **`'I'`** - Sets port in `intermediate` mode.

- **`'D'`** - Sets port in `data` mode.

- **`'N'`** - Gets identification number for listen port. This identification
  number is used in an accept command to the driver. It is returned as a
  big-endian 32-bit integer, which is the file identifier for the listen socket.

- **`'S'`** - Gets statistics, which is the number of bytes received, the number
  of bytes sent, and the number of bytes pending in the output queue. This data
  is used when the distribution checks that a connection is alive (ticking). The
  statistics is returned as three 32-bit big-endian integers.

- **`'T'`** - Sends a tick message, which is a packet of length 0. Ticking is
  done when the port is in `data` mode, so the command for sending data cannot
  be used (besides it ignores zero length packages in `command` mode). This is
  used by the ticker to send dummy data when no other traffic is present.

  _Note:_ It is important that the interface for sending ticks is not blocking.
  This implementation uses `erlang:port_control/3`, which does not block the
  caller. If `erlang:port_command` is used, use `erlang:port_command/3` and pass
  `[force]` as option list; otherwise the caller can be blocked indefinitely on
  a busy port and prevent the system from taking down a connection that is not
  functioning.

- **`'R'`** - Gets creation number of a listen socket, which is used to dig out
  the number stored in the lock file to differentiate between invocations of
  Erlang nodes with the same name.

The control interface gets a buffer to return its value in, but is free to
allocate its own buffer if the provided one is too small. The `uds_control` code
is as follows:

```c
( 1) static int uds_control(ErlDrvData handle, unsigned int command,
( 2)                        char* buf, int count, char** res, int res_size)
( 3) {
( 4) /* Local macro to ensure large enough buffer. */
( 5) #define ENSURE(N)                               \
( 6)    do {                                         \
( 7)        if (res_size < N) {                      \
( 8)            *res = ALLOC(N);                     \
( 9)        }                                        \
(10)    } while(0)

(11)    UdsData *ud = (UdsData *) handle;

(12)    switch (command) {
(13)    case 'S':
(14)        {
(15)            ENSURE(13);
(16)            **res = 0;
(17)            put_packet_length((*res) + 1, ud->received);
(18)            put_packet_length((*res) + 5, ud->sent);
(19)            put_packet_length((*res) + 9, driver_sizeq(ud->port));
(20)            return 13;
(21)        }
(22)    case 'C':
(23)        if (ud->type < portTypeCommand) {
(24)            return report_control_error(res, res_size, "einval");
(25)        }
(26)        ud->type = portTypeCommand;
(27)        driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_READ, 0);
(28)        ENSURE(1);
(29)        **res = 0;
(30)        return 1;
(31)    case 'I':
(32)        if (ud->type < portTypeCommand) {
(33)            return report_control_error(res, res_size, "einval");
(34)        }
(35)        ud->type = portTypeIntermediate;
(36)        driver_select(ud->port, (ErlDrvEvent) ud->fd, DO_READ, 0);
(37)        ENSURE(1);
(38)        **res = 0;
(39)        return 1;
(40)    case 'D':
(41)        if (ud->type < portTypeCommand) {
(42)            return report_control_error(res, res_size, "einval");
(43)        }
(44)        ud->type = portTypeData;
(45)        do_recv(ud);
(46)        ENSURE(1);
(47)        **res = 0;
(48)        return 1;
(49)    case 'N':
(50)        if (ud->type != portTypeListener) {
(51)            return report_control_error(res, res_size, "einval");
(52)        }
(53)        ENSURE(5);
(54)        (*res)[0] = 0;
(55)        put_packet_length((*res) + 1, ud->fd);
(56)        return 5;
(57)    case 'T': /* tick */
(58)        if (ud->type != portTypeData) {
(59)            return report_control_error(res, res_size, "einval");
(60)        }
(61)        do_send(ud,"",0);
(62)        ENSURE(1);
(63)        **res = 0;
(64)        return 1;
(65)    case 'R':
(66)        if (ud->type != portTypeListener) {
(67)            return report_control_error(res, res_size, "einval");
(68)        }
(69)        ENSURE(2);
(70)        (*res)[0] = 0;
(71)        (*res)[1] = ud->creation;
(72)        return 2;
(73)    default:
(74)        return report_control_error(res, res_size, "einval");
(75)    }
(76) #undef ENSURE
(77) }
```

The macro `ENSURE` (line 5-10) is used to ensure that the buffer is large enough
for the answer. We switch on the command and take actions. We always have read
select active on a port in `data` mode (achieved by calling `do_recv` on line
45), but we turn off read selection in `intermediate` and `command` modes (line
27 and 36).

The rest of the driver is more or less UDS-specific and not of general interest.

## Putting It All Together

To test the distribution, the `net_kernel:start/1` function can be used. It is
useful, as it starts the distribution on a running system, where
tracing/debugging can be performed. The `net_kernel:start/1` routine takes a
list as its single argument. The list first element in the list is to be the
node name (without the "@hostname") as an atom. The second (and last) element is
to be one of the atoms `shortnames` or `longnames`. In the example case,
`shortnames` is preferred.

For `net_kernel` to find out which distribution module to use, command-line
argument `-proto_dist` is used. It is followed by one or more distribution
module names, with suffix "\_dist" removed, that is, `uds_dist` as a
distribution module is specified as `-proto_dist uds`.

If no `epmd` (TCP port mapper daemon) is used, also command-line option
`-no_epmd` is to be specified, which makes Erlang skip the `epmd` startup, both
as an OS process and as an Erlang ditto.

The path to the directory where the distribution modules reside must be known at
boot. This can be achieved either by specifying `-pa <path>` on the command line
or by building a boot script containing the applications used for your
distribution protocol. (In the `uds_dist` protocol, only the `uds_dist`
application needs to be added to the script.)

The distribution starts at boot if all the above is specified and an
`-sname <name>` flag is present at the command line.

_Example 1:_

```text
$ erl -pa $ERL_TOP/lib/kernel/examples/uds_dist/ebin -proto_dist uds -no_epmd
Erlang (BEAM) emulator version 5.0

Eshell V5.0  (abort with ^G)
1> net_kernel:start([bing,shortnames]).
{ok,<0.30.0>}
(bing@hador)2>
```

_Example 2:_

```text
$ erl -pa $ERL_TOP/lib/kernel/examples/uds_dist/ebin -proto_dist uds \
      -no_epmd -sname bong
Erlang (BEAM) emulator version 5.0

Eshell V5.0  (abort with ^G)
(bong@hador)1>
```

The `ERL_FLAGS` environment variable can be used to store the complicated
parameters in:

```text
$ ERL_FLAGS=-pa $ERL_TOP/lib/kernel/examples/uds_dist/ebin \
      -proto_dist uds -no_epmd
$ export ERL_FLAGS
$ erl -sname bang
Erlang (BEAM) emulator version 5.0

Eshell V5.0  (abort with ^G)
(bang@hador)1>
```

`ERL_FLAGS` should not include the node name.
