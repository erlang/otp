%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(gen_sctp).

-moduledoc """
Interface to SCTP sockets.

This module provides functions for communicating over SCTP sockets.
The implementation assumes that the OS kernel supports SCTP
[(RFC 2960)](http://www.rfc-archive.org/getrfc.php?rfc=2960)
through the user-level
[Sockets API Extensions](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13).

During development, this implementation was tested on:

- Linux Fedora Core 5.0 (kernel 2.6.15-2054 or later is needed)
- Solaris 10, 11

During OTP adaptation it was tested on:

- SUSE Linux Enterprise Server 10 (x86_64) kernel 2.6.16.27-0.6-smp, with
  lksctp-tools-1.0.6
- Briefly on Solaris 10
- SUSE Linux Enterprise Server 10 Service Pack 1 (x86_64) kernel
  2.6.16.54-0.2.3-smp with lksctp-tools-1.0.7
- FreeBSD 8.2

This module was written for one-to-many style sockets (type `seqpacket`).
With the addition of `peeloff/2`, one-to-one style sockets (type `stream`)
were introduced.

Record definitions for this module can be found using:

```erlang
-include_lib("kernel/include/inet_sctp.hrl").
```

These record definitions use the "new" spelling 'adaptation',
not the deprecated 'adaption', regardless of which spelling
the underlying C API uses.

[](){: #options } SCTP Socket Options
-------------------------------------

The set of admissible SCTP socket options is by construction orthogonal
to the sets of TCP, UDP, and generic `inet` options. Here are only
options listed that are allowed for SCTP sockets.

Options can be set on the socket when calling [`open/1,2`](`open/1`),
and changed when calling [`connect/4,5`](`connect/4`) or
by calling `inet:setopts/2`.  They can be retrieved using `inet:getopts/2`.

- **`{mode, list|binary} | list | binary`** [](){: #option-binary } [](){: #option-list } -
  Determines the type of data returned from [`recv/1,2`](`recv/1`)
  or in _active mode_ data messages.

- **`{active, false|true|once|N}`** [](){: #option-active }

  - If `false` (_passive mode_, the default), the caller must do an explicit
    [`recv`](`recv/1`) call to retrieve the available data from the socket.

  - If `true|once|N` (_active modes_) received data or events are sent
    to the owning process. See [`open/0..2`](`open/0`) for the message format.

  - If `true` (fully _active mode_) there is no flow control.

    > #### Note {: .info }
    >
    > Note that this can cause the message queue to overflow
    > causing for example the virtual machine to run out of memory and crash.

  - If `once`, only one message is automatically placed in the message queue,
    and the mode resets to passive. This provides flow control
    and the possibility for the receiver to listen for incoming
    SCTP data interleaved with other inter-process messages.

  - If `active` is specified as an integer `N` in the range -32768 to 32767
    (inclusive), that number is added to the socket's data messages counter.
    If the result of the addition is negative, the counter is set to `0`.
    Once the counter reaches `0`, either through the delivery of messages
    or by being explicitly set with `inet:setopts/2`, the socket mode
    resets to passive (`{active, false}`). When a socket in `{active, N}`
    mode transitions to passive mode, the message `{sctp_passive, Socket}`
    is sent to the controlling process to notify that if it wants to receive
    more data messages from the socket, it must call `inet:setopts/2`
    to set the socket back into an active mode.

- **`{tos, integer()}`** - Sets the Type-Of-Service field on the IP datagrams
  that are sent, to the specified value. This effectively determines a
  prioritization policy for the outbound packets. The acceptable values are
  system-dependent.

- **`{priority, integer()}`** - A protocol-independent equivalent of `tos`
  above. Setting priority implies setting `tos` as well.

- **`{dontroute, true|false}`** - Defaults to `false`.
  If `true`, the kernel does not send packets through any gateway,
  but only sends them to directly connected hosts.

- **`{reuseaddr, true|false}`** - Defaults to `false`.
  If `true`, the local binding address `{IP,Port}` of the socket can be
  reused immediately. No waiting in state `CLOSE_WAIT` is performed
  (can be required for some types of servers).

- **`{sndbuf, integer()}`** - The size, in bytes, of the OS kernel send buffer
  for this socket. Sending errors would occur for datagrams larger than
  `val(sndbuf)`. Setting this option also adjusts the size of
  the driver buffer (see `buffer` above).

- **`{recbuf, integer()}`** - The size, in bytes, of the OS kernel receive
  buffer for this socket. Sending errors would occur for datagrams
  larger than `val(recbuf)`. Setting this option also adjusts the size
  of the driver buffer (see `buffer` above).

### [](){: #option_non_block_send }

- **`{non_block_send, boolean()}`** - A send call that would otherwise block (hang),
  will instead immediately return with e.g. `{error, eagain}`
  *if* this option has been set to `true`.
  Defaults to `false`.

- **`{sctp_module, module()}`** - Overrides which callback module is used.
Defaults to `inet_sctp` for IPv4 and `inet6_sctp` for IPv6.

- **`{sctp_rtoinfo, #sctp_rtoinfo{}}`**

  ```erlang
  #sctp_rtoinfo{
        assoc_id = assoc_id(),
        initial  = integer(),
        max      = integer(),
        min      = integer()
  }
  ```

  Determines retransmission time-out parameters, in milliseconds, for the
  association(s) specified by `assoc_id`.

  `assoc_id = 0` (default) indicates the whole endpoint. See
  [RFC 2960](http://www.rfc-archive.org/getrfc.php?rfc=2960) and
  [Sockets API Extensions for SCTP](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
  for the exact semantics of the field values.

- **`{sctp_associnfo, #sctp_assocparams{}}`**

  ```erlang
  #sctp_assocparams{
        assoc_id                 = assoc_id(),
        asocmaxrxt               = integer(),
        number_peer_destinations = integer(),
        peer_rwnd                = integer(),
        local_rwnd               = integer(),
        cookie_life              = integer()
  }
  ```

  Determines association parameters for the association(s) specified by
  `assoc_id`.

  `assoc_id = 0` (default) indicates the whole endpoint. See
  [Sockets API Extensions for SCTP](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
  for the discussion of their semantics. Rarely used.

- **`{sctp_initmsg, #sctp_initmsg{}}`**

  ```erlang
  #sctp_initmsg{
       num_ostreams   = integer(),
       max_instreams  = integer(),
       max_attempts   = integer(),
       max_init_timeo = integer()
  }
  ```

  Determines the default parameters that this socket tries to negotiate
  with its peer while establishing an association with it. Is to be set after
  [`open/*`](`open/1`) but before the first [`connect/*`](`connect/4`).
  `#sctp_initmsg{}` can also be used as ancillary data with the first call of
  [`send/*`](`send/3`) to a new peer (when a new association is created).

  - **`num_ostreams`** - Number of outbound streams

  - **`max_instreams`** - Maximum number of inbound streams

  - **`max_attempts`** - Maximum retransmissions while establishing an
    association

  - **`max_init_timeo`** - Time-out, in milliseconds, for establishing an
    association

- **`{sctp_autoclose, integer() >= 0}`** - Determines the time, in seconds,
  after which an idle association is automatically closed. `0` means that the
  association is never automatically closed.

- **`{sctp_nodelay, true|false}`** - Turns on|off the Nagle algorithm for
  merging small packets into larger ones. This improves throughput at the
  expense of latency.

- **`{sctp_disable_fragments, true|false}`** - If `true`, induces an error on an
  attempt to send a message larger than the current PMTU size (which would
  require fragmentation/reassembling). Notice that message fragmentation does
  not affect the logical atomicity of its delivery; this option is provided for
  performance reasons only.

- **`{sctp_i_want_mapped_v4_addr, true|false}`** - Turns on|off automatic
  mapping of IPv4 addresses into IPv6 ones (if the socket address family is
  `AF_INET6`).

- **`{sctp_maxseg, integer()}`** - Determines the maximum chunk size if message
  fragmentation is used. If `0`, the chunk size is limited by the Path MTU only.

- **`{sctp_primary_addr, #sctp_prim{}}`**

  ```erlang
  #sctp_prim{
        assoc_id = assoc_id(),
        addr     = {IP, Port}
  }
   IP = ip_address()
   Port = port_number()
  ```

  For the association specified by `assoc_id`, `{IP,Port}` must be one of the
  peer addresses. This option determines that the specified address is treated
  by the local SCTP stack as the primary address of the peer.

- **`{sctp_set_peer_primary_addr, #sctp_setpeerprim{}}`**

  ```erlang
  #sctp_setpeerprim{
        assoc_id = assoc_id(),
        addr     = {IP, Port}
  }
   IP = ip_address()
   Port = port_number()
  ```

  When set, informs the peer to use `{IP, Port}` as the primary address of the
  local endpoint for the association specified by `assoc_id`.


- **`{sctp_adaptation_layer, #sctp_setadaptation{}}`** [](){: #option-sctp_adaptation_layer }

  ```erlang
  #sctp_setadaptation{
        adaptation_ind = integer()
  }
  ```

  When set, requests that the local endpoint uses the value specified by
  `adaptation_ind` as the Adaptation Indication parameter for establishing
  new associations. For details, see
  [RFC 2960](http://www.rfc-archive.org/getrfc.php?rfc=2960) and
  [Sockets API Extensions for SCTP](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13).

- **`{sctp_peer_addr_params, #sctp_paddrparams{}}`**

  ```erlang
  #sctp_paddrparams{
        assoc_id   = assoc_id(),
        address    = {IP, Port},
        hbinterval = integer(),
        pathmaxrxt = integer(),
        pathmtu    = integer(),
        sackdelay  = integer(),
        flags      = list()
  }
  IP = ip_address()
  Port = port_number()
  ```

  Determines various per-address parameters for the association specified by
  `assoc_id` and the peer address `address` (the SCTP protocol supports
  multi-homing, so more than one address can correspond to a specified
  association).

  - **`hbinterval`** - Heartbeat interval, in milliseconds

  - **`pathmaxrxt`** - Maximum number of retransmissions before this address is
    considered unreachable (and an alternative address is selected)

  - **`pathmtu`** - Fixed Path MTU, if automatic discovery is disabled (see
    `flags` below)

  - **`sackdelay`** - Delay, in milliseconds, for SAC messages (if the delay is
    enabled, see `flags` below)

  - **`flags`** - The following flags are available:

    - **`hb_enable`** - Enables heartbeat

    - **`hb_disable`** - Disables heartbeat

    - **`hb_demand`** - Initiates heartbeat immediately

    - **`pmtud_enable`** - Enables automatic Path MTU discovery

    - **`pmtud_disable`** - Disables automatic Path MTU discovery

    - **`sackdelay_enable`** - Enables SAC delay

    - **`sackdelay_disable`** - Disables SAC delay

- **`{sctp_default_send_param, #sctp_sndrcvinfo{}}`**

  [](){: #record-sctp_sndrcvinfo }
  ```erlang
  #sctp_sndrcvinfo{
        stream     = integer(),
        ssn        = integer(),
        flags      = list(),
        ppid       = integer(),
        context    = integer(),
        timetolive = integer(),
        tsn        = integer(),
        cumtsn     = integer(),
        assoc_id   = assoc_id()
  }
  ```

  `#sctp_sndrcvinfo{}` is used both in this socket option, and as
  ancillary data while sending or receiving SCTP messages. When set
  as an option, it provides default values for subsequent
  [`send`](`send/3`) calls on the association specified by `assoc_id`.

  `assoc_id = 0` (default) indicates the whole endpoint.

  The following fields typically must be specified by the sender:

  - **`sinfo_stream`** - Stream number (0-base) within the association to send
    the messages through;

  - **`sinfo_flags`** - The following flags are recognised:

    - **`unordered`** - The message is to be sent unordered

    - **`addr_over`** - The address specified in [`send`](`send/3`) overwrites
      the primary peer address

    - **`abort`** - Aborts the current association without flushing any unsent
      data

    - **`eof`** - Gracefully shuts down the current association, with flushing
      of unsent data

    Other fields are rarely used. For complete information, see
    [RFC 2960](http://www.rfc-archive.org/getrfc.php?rfc=2960) and
    [Sockets API Extensions for SCTP](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13).

- **`{sctp_events, #sctp_event_subscribe{}}`** [](){: #option-sctp_events }

  ```erlang
  #sctp_event_subscribe{
          data_io_event          = true | false,
          association_event      = true | false,
          address_event          = true | false,
          send_failure_event     = true | false,
          peer_error_event       = true | false,
          shutdown_event         = true | false,
          partial_delivery_event = true | false,
          adaptation_layer_event = true | false
  }
  ```

  This option determines which [SCTP Events](#sctp-events) that are to be
  received (through [`recv/*`](`recv/1`)) along with the data. The only
  exception is `data_io_event`, which enables or disables receiving of
  [`#sctp_sndrcvinfo{}`](#record-sctp_sndrcvinfo) ancillary data,
  not events. By default, all flags except `adaptation_layer_event` are enabled,
  although `sctp_data_io_event` and `association_event` are used by the driver
  itself and not exported to the user level.

- **`{sctp_delayed_ack_time, #sctp_assoc_value{}}`**

  ```erlang
  #sctp_assoc_value{
        assoc_id    = assoc_id(),
        assoc_value = integer()
  }
  ```

  Rarely used. Determines the ACK time (specified by `assoc_value`, in
  milliseconds) for the specified association or the whole endpoint if
  `assoc_value = 0` (default).

- **`{sctp_status, #sctp_status{}}`**

  ```erlang
  #sctp_status{
        assoc_id            = assoc_id(),
        state               = atom(),
        rwnd                = integer(),
        unackdata           = integer(),
        penddata            = integer(),
        instrms             = integer(),
        outstrms            = integer(),
        fragmentation_point = integer(),
        primary             = #sctp_paddrinfo{}
  }
  ```

  This option is read-only. It determines the status of the SCTP association
  specified by `assoc_id`. The following are the possible values of `state`
  (the state designations are mostly self-explanatory):

  - **`sctp_state_empty`** - Default. Means that no other state is active.

  - **`sctp_state_closed`**

  - **`sctp_state_cookie_wait`**

  - **`sctp_state_cookie_echoed`**

  - **`sctp_state_established`**

  - **`sctp_state_shutdown_pending`**

  - **`sctp_state_shutdown_sent`**

  - **`sctp_state_shutdown_received`**

  - **`sctp_state_shutdown_ack_sent`**

  Semantics of the other fields:

  - **`sstat_rwnd`** - Current receiver window size of the association

  - **`sstat_unackdata`** - Number of unacked data chunks

  - **`sstat_penddata`** - Number of data chunks pending receipt

  - **`sstat_instrms`** - Number of inbound streams

  - **`sstat_outstrms`** - Number of outbound streams

  - **`sstat_fragmentation_point`** - Message size at which SCTP fragmentation
    occurs

  - **`sstat_primary`** - Information on the current primary peer address (see
    below for the format of `#sctp_paddrinfo{}`)


- **`{sctp_get_peer_addr_info, #sctp_paddrinfo{}}`** [](){: #option-sctp_get_peer_addr_info }

  ```erlang
  #sctp_paddrinfo{
        assoc_id  = assoc_id(),
        address   = {IP, Port},
        state     = inactive | active | unconfirmed,
        cwnd      = integer(),
        srtt      = integer(),
        rto       = integer(),
        mtu       = integer()
  }
  IP = ip_address()
  Port = port_number()
  ```

  This option is read-only. It determines the parameters specific to
  the peer address specified by `address` within the association specified
  by `assoc_id`.  Field `address` fmust be set by the caller; all other fields
  are filled in on return. If `assoc_id = 0` (default), the `address`
  is automatically translated into the corresponding association ID.
  This option is rarely used. For the semantics of all fields, see
  [RFC 2960](http://www.rfc-archive.org/getrfc.php?rfc=2960) and
  [Sockets API Extensions for SCTP](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13).

[](){: #examples } SCTP Examples
--------------------------------

Example of an Erlang SCTP server that receives SCTP messages
and prints them on the standard output:

```erlang
-module(sctp_server).

-export([server/0,server/1,server/2]).
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

server() ->
    server(any, 2006).

server([Host,Port]) when is_list(Host), is_list(Port) ->
    {ok, #hostent{h_addr_list = [IP|_]}} = inet:gethostbyname(Host),
    io:format("~w -> ~w~n", [Host, IP]),
    server([IP, list_to_integer(Port)]).

server(IP, Port) when is_tuple(IP) orelse IP == any orelse IP == loopback,
                      is_integer(Port) ->
    {ok,S} = gen_sctp:open(Port, [{recbuf,65536}, {ip,IP}]),
    io:format("Listening on ~w:~w. ~w~n", [IP,Port,S]),
    ok     = gen_sctp:listen(S, true),
    server_loop(S).

server_loop(S) ->
    case gen_sctp:recv(S) of
    {error, Error} ->
        io:format("SCTP RECV ERROR: ~p~n", [Error]);
    Data ->
        io:format("Received: ~p~n", [Data])
    end,
    server_loop(S).
```

Example of an Erlang SCTP client interacting with the above server.
Note that in this example the client creates an association with
the server with 5 outbound streams. Therefore, sending of `"Test 0"`
over stream 0 succeeds, but sending of `"Test 5"` over stream 5 fails.
The client then `abort`s the association, which results in that
the corresponding event is received on the server side.

```erlang
-module(sctp_client).

-export([client/0, client/1, client/2]).
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

client() ->
    client([localhost]).

client([Host]) ->
    client(Host, 2006);

client([Host, Port]) when is_list(Host), is_list(Port) ->
    client(Host,list_to_integer(Port)),
    init:stop().

client(Host, Port) when is_integer(Port) ->
    {ok,S}     = gen_sctp:open(),
    {ok,Assoc} = gen_sctp:connect
        (S, Host, Port, [{sctp_initmsg,#sctp_initmsg{num_ostreams=5}}]),
    io:format("Connection Successful, Assoc=~p~n", [Assoc]),

    io:write(gen_sctp:send(S, Assoc, 0, <<"Test 0">>)),
    io:nl(),
    timer:sleep(10000),
    io:write(gen_sctp:send(S, Assoc, 5, <<"Test 5">>)),
    io:nl(),
    timer:sleep(10000),
    io:write(gen_sctp:abort(S, Assoc)),
    io:nl(),

    timer:sleep(1000),
    gen_sctp:close(S).
```

A simple Erlang SCTP client that uses the `connect_init` API:

```erlang
-module(ex3).

-export([client/4]).
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

client(Peer1, Port1, Peer2, Port2)
  when is_tuple(Peer1), is_integer(Port1), is_tuple(Peer2), is_integer(Port2) ->
    {ok,S}     = gen_sctp:open(),
    SctpInitMsgOpt = {sctp_initmsg,#sctp_initmsg{num_ostreams=5}},
    ActiveOpt = {active, true},
    Opts = [SctpInitMsgOpt, ActiveOpt],
    ok = gen_sctp:connect(S, Peer1, Port1, Opts),
    ok = gen_sctp:connect(S, Peer2, Port2, Opts),
    io:format("Connections initiated~n", []),
    client_loop(S, Peer1, Port1, undefined, Peer2, Port2, undefined).

client_loop(S, Peer1, Port1, AssocId1, Peer2, Port2, AssocId2) ->
    receive
        {sctp, S, Peer1, Port1, {_Anc, SAC}}
          when is_record(SAC, sctp_assoc_change), AssocId1 == undefined ->
            io:format("Association 1 connect result: ~p. AssocId: ~p~n",
                      [SAC#sctp_assoc_change.state,
                       SAC#sctp_assoc_change.assoc_id]),
            client_loop(S, Peer1, Port1, SAC#sctp_assoc_change.assoc_id,
                        Peer2, Port2, AssocId2);

        {sctp, S, Peer2, Port2, {_Anc, SAC}}
          when is_record(SAC, sctp_assoc_change), AssocId2 == undefined ->
            io:format("Association 2 connect result: ~p. AssocId: ~p~n",
                      [SAC#sctp_assoc_change.state, SAC#sctp_assoc_change.assoc_id]),
            client_loop(S, Peer1, Port1, AssocId1, Peer2, Port2,
                       SAC#sctp_assoc_change.assoc_id);

        {sctp, S, Peer1, Port1, Data} ->
            io:format("Association 1: received ~p~n", [Data]),
            client_loop(S, Peer1, Port1, AssocId1,
                        Peer2, Port2, AssocId2);

        {sctp, S, Peer2, Port2, Data} ->
            io:format("Association 2: received ~p~n", [Data]),
            client_loop(S, Peer1, Port1, AssocId1,
                        Peer2, Port2, AssocId2);

        Other ->
            io:format("Other ~p~n", [Other]),
            client_loop(S, Peer1, Port1, AssocId1,
                        Peer2, Port2, AssocId2)

    after 5000 ->
            ok
    end.
```

[](){: #seealso } See Also
--------------------------

`m:gen_tcp`, `m:gen_udp`, `m:inet`,
[RFC 2960](http://www.rfc-archive.org/getrfc.php?rfc=2960) (Stream Control
Transmission Protocol),
[Sockets API Extensions for SCTP](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
""".


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This module provides functions for communicating with
%% sockets using the SCTP protocol.  The implementation assumes that
%% the OS kernel supports SCTP providing user-level SCTP Socket API:
%%     http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13

-include("inet_sctp.hrl").

-export([open/0, open/1, open/2, close/1]).
-export([listen/2, peeloff/2]).
-export([connect/3, connect/4, connect/5,
         connect_init/3, connect_init/4, connect_init/5,
         connectx_init/3, connectx_init/4,connectx_init/5]).
-export([eof/2, abort/2]).
-export([send/3, send/4, recv/1, recv/2]).
-export([error_string/1]).
-export([controlling_process/2]).

-doc """
Association ID.

An opaque term returned in, for example, `#sctp_paddr_change{}`, which
identifies an association for an SCTP socket. The term is opaque except for the
special value `0`, which has a meaning such as "the whole endpoint" or "all
future associations".
""".
-doc(#{group => <<"Exported data types">>}).
-type assoc_id() :: term().

-doc "[SCTP Socket Option](#options) name and value, to set.".
-doc(#{group => <<"Exported data types">>}).
-type option() ::
        elementary_option() |
        record_option().

-doc "[SCTP Socket Option](#options) name, to get.".
-doc(#{group => <<"Exported data types">>}).
-type option_name() ::
        elementary_option_name() |
        record_option() |
        ro_option().

-doc """
[SCTP Socket Option](#options) name and value, what you get.
""".
-doc(#{group => <<"Exported data types">>}).
-type option_value() ::
        elementary_option() |
        record_option() |
        ro_option().

-doc(#{group => <<"Internal data types">>}).
-type elementary_option() ::
        {active, true | false | once | -32768..32767} |
        {buffer, non_neg_integer()} |
        {non_block_send, boolean()} |
        {debug, boolean()} |
        {dontroute, boolean()} |
        {exclusiveaddruse, boolean()} |
        {high_msgq_watermark, pos_integer()} |
        {linger, {boolean(), non_neg_integer()}} |
        {low_msgq_watermark, pos_integer()} |
        {mode, list | binary} | list | binary |
        {priority, non_neg_integer()} |
        {recbuf, non_neg_integer()} |
        {reuseaddr, boolean()} |
        {reuseport, boolean()} |
        {reuseport_lb, boolean()} |
	{ipv6_v6only, boolean()} |
        {sndbuf, non_neg_integer()} |
        {sctp_autoclose, non_neg_integer()} |
        {sctp_disable_fragments, boolean()} |
        {sctp_i_want_mapped_v4_addr, boolean()} |
        {sctp_maxseg, non_neg_integer()} |
        {sctp_nodelay, boolean()} |
        {tos, non_neg_integer()} |
        {tclass, non_neg_integer()} |
        {ttl, non_neg_integer()} |
        {recvtos, boolean()} |
        {recvtclass, boolean()} |
        {recvttl, boolean()}.

-doc(#{group => <<"Internal data types">>}).
-type elementary_option_name() ::
        active |
        buffer |
        non_block_send |
        debug |
        dontroute |
        exclusiveaddruse |
        high_msgq_watermark |
        linger |
        low_msgq_watermark |
        mode |
        priority |
        recbuf |
        reuseaddr |
        reuseport |
        reuseport_lb |
	ipv6_v6only |
        sctp_autoclose |
        sctp_disable_fragments |
        sctp_i_want_mapped_v4_addr |
        sctp_maxseg |
        sctp_nodelay |
        sndbuf |
        tos |
        tclass |
        ttl |
        recvtos |
        recvtclass |
        recvttl.

-doc(#{group => <<"Internal data types">>}).
-type record_option() ::
        {sctp_adaptation_layer, #sctp_setadaptation{}} |
        {sctp_associnfo, #sctp_assocparams{}} |
        {sctp_default_send_param, #sctp_sndrcvinfo{}} |
        {sctp_delayed_ack_time, #sctp_assoc_value{}} |
        {sctp_events, #sctp_event_subscribe{}} |
        {sctp_initmsg, #sctp_initmsg{}} |
        {sctp_peer_addr_params, #sctp_paddrparams{}} |
        {sctp_primary_addr, #sctp_prim{}} |
        {sctp_rtoinfo, #sctp_rtoinfo{}} |
        {sctp_set_peer_primary_addr, #sctp_setpeerprim{}}.

-doc(#{group => <<"Internal data types">>}).
-type ro_option() ::
        {sctp_get_peer_addr_info, #sctp_paddrinfo{}} |
        {sctp_status, #sctp_status{}}.

-doc "Socket identifier returned from [`open/*`](`open/0`).".
-doc(#{group => <<"Exported data types">>}).
-type sctp_socket() :: port().

-export_type(
   [assoc_id/0, option/0, option_name/0,  option_value/0, sctp_socket/0]).



-doc(#{equiv => open([])}).
-spec open() -> {ok, Socket} | {error, inet:posix()} when
      Socket :: sctp_socket().
open() ->
    open([]).

-doc """
Create an SCTP socket.

With argument [`Port`](`t:inet:port_number/0`),
equivalent to`open([{port, Port}]`.

Creates an SCTP socket and binds it to the local addresses specified by all
`{ip,IP}` (or synonymously `{ifaddr,IP}`) options (this feature is called
SCTP multi-homing). The default `IP` and `Port` are `any` and `0`,
meaning bind to all local addresses on any free port.

It is also possible to use `{ifaddr, SockAddr}`, in which case it takes
precedence over the `ip` and `port` options. These options can however
be used to update the address and port of ifaddr (if they occur
after ifaddr in the options list), although this is not recommended.

Other options:

- **`inet6`** - Sets up the socket for IPv6.

- **`inet`** - Sets up the socket for IPv4. This is the default.

A default set of socket [options](#options) is used.
In particular, the socket is opened in [binary](#option-binary)
and [passive](#option-active) mode, with SockType `seqpacket`,
and with reasonably large [kernel](`m:inet#option-sndbuf`) and driver
[buffers](`m:inet#option-buffer`).

When the socket is in [passive](#option-active) mode,
data can be received through the [`recv/1,2`](`recv/1`) calls.

When the socket is in [active](#option-active) mode,
data received data is delivered to the controlling process as messages:

```erlang
{sctp, Socket, FromIP, FromPort, {AncData, Data}}
```

See [`recv/1,2`](`recv/1`) for a description of the message fields.

> #### Note {: .info }
>
> This message format unfortunately differs slightly from the
> [`gen_udp`](`gen_udp:open/1`) message format with ancillary data,
> and from the [`recv/1,2`](`recv/1`) return tuple format.
""".
-spec open(Port) -> {ok, Socket} | {error, inet:posix()} when
              Port :: inet:port_number(),
              Socket :: sctp_socket();
          (Opts) -> {ok, Socket} | {error, inet:posix()} when
              Opts :: [Opt],
              Opt :: {ifaddr, IP | SockAddr}
                   | {ip, IP}
                   | {port, Port}
                   | inet:address_family()
                   | {type, SockType}
                   | {netns, file:filename_all()}
                   | {bind_to_device, binary()}
                   | option(),
              IP       :: inet:ip_address() | any | loopback,
              SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
              Port     :: inet:port_number(),
              SockType :: seqpacket | stream,
              Socket   :: sctp_socket().
open(Opts0) when is_list(Opts0) ->
    {Mod, Opts} = inet:sctp_module(Opts0),
    case Mod:open(Opts) of
	{error,badarg} ->
	    erlang:error(badarg, [Opts]);
	{error,einval} ->
	    erlang:error(badarg, [Opts]);
	Result -> Result
    end;
open(Port) when is_integer(Port) ->
    open([{port,Port}]);
open(X) ->
    erlang:error(badarg, [X]).

-doc(#{equiv => open([{port, Port} | Opts])}).
-spec open(Port :: integer(), Opts :: [term()]) -> _.
open(Port, Opts) when is_integer(Port), is_list(Opts) ->
    open([{port,Port}|Opts]);
open(Port, Opts) ->
    erlang:error(badarg, [Port,Opts]).



-doc """
Close an SCTP socket.

Closes the socket and all associations on it. The unsent data is flushed
as for `eof/2`. The [`close/1`](`close/1`) call is blocking
depending of the value of the [`linger`](`m:inet#option-linger`)
socket [option]. If it is `false` or the linger time-out expires,
the call returns and unsent data is flushed in the background.
""".
-spec close(Socket) -> ok | {error, inet:posix()} when
      Socket :: sctp_socket().

close(S) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:close(S);
	{error,closed} -> ok
    end;
close(S) ->
    erlang:error(badarg, [S]).



-doc """
Make an SCTP socket listen to incoming associations.

The socket will listen on the IP address(es) and port number it is bound to.

For type `seqpacket`, sockets (the default), the argument`IsServer`
must be a `t:boolean/0`.  In contrast to `stream` sockets, there is
no listening queue length.  If `IsServer` is `true`, the socket accepts
new associations, that is, it becomes an SCTP server socket.

For type `stream` sockets, the argument `Backlog` sets
the backlog queue length just like for TCP.
""".
-spec listen(Socket, IsServer) -> ok | {error, Reason} when
      Socket :: sctp_socket(),
      IsServer :: boolean(),
      Reason :: term();
	    (Socket, Backlog) -> ok | {error, Reason} when
      Socket :: sctp_socket(),
      Backlog :: integer(),
      Reason :: term().

listen(S, Backlog)
  when is_port(S), is_boolean(Backlog);
       is_port(S), is_integer(Backlog) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:listen(S, Backlog);
	Error -> Error
    end;
listen(S, Flag) ->
    erlang:error(badarg, [S,Flag]).



-doc """
Branch off an association into a new socket of type `stream`.

The existing association `Assoc` in the socket `Socket` (that has to
be of type `seqpacket`; one-to-many style) is branched off into
a new socket `NewSocket` of type `stream` (one-to-one style).

The existing association argument `Assoc` can be a
[`#sctp_assoc_change{}`](#record-sctp_assoc_change) record as
returned from, for example, [`recv/*`](`recv/2`), [`connect/*`](`connect/5`),
or from a listening socket in active mode.
It can also be just the field `assoc_id` `t:integer/0` from such a record.
""".
-doc(#{since => <<"OTP R15B">>}).
-spec peeloff(Socket, Assoc) -> {ok, NewSocket} | {error, Reason} when
      Socket :: sctp_socket(),
      Assoc :: #sctp_assoc_change{} | assoc_id(),
      NewSocket :: sctp_socket(),
      Reason :: term().

peeloff(S, #sctp_assoc_change{assoc_id=AssocId}) when is_port(S) ->
    peeloff(S, AssocId);
peeloff(S, AssocId) when is_port(S), is_integer(AssocId) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:peeloff(S, AssocId);
	Error -> Error
    end.



-doc(#{since => <<"OTP 24.3">>,
       equiv => connect(Socket, SockAddr, Opts, infinity)}).
-spec connect(Socket, SockAddr, Opts) ->
                     {ok, #sctp_assoc_change{state :: 'comm_up'}} |
                     {error, #sctp_assoc_change{state :: 'cant_assoc'}} |
                     {error, inet:posix()}
                         when
      Socket   :: sctp_socket(),
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: [Opt :: option()].

connect(S, SockAddr, Opts) ->
    connect(S, SockAddr, Opts, infinity).

-doc """
Establish an association with a peer.

With arguments `Addr` and `Port`, equivalent to
[`connect(Socket, Addr, Port, Opts, infinity)`](`connect/5`).

With arguments `SockAddr` and `Opts` _(since OTP 24.3)_, equivalent to
[`connect(Socket, Addr, Port, Opts, Timeout)`](`connect/5`)
where `Addr` and `Port` are extracted from `SockAddr`.
""".
-spec connect(Socket, SockAddr, Opts, Timeout) ->
                     {ok, #sctp_assoc_change{state :: 'comm_up'}} |
                     {error, #sctp_assoc_change{state :: 'cant_assoc'}} |
                     {error, inet:posix()}
                         when
      Socket   :: sctp_socket(),
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: [Opt :: option()],
      Timeout  :: timeout();
             (Socket, Addr, Port, Opts) ->
                     {ok, #sctp_assoc_change{state :: 'comm_up'}} |
                     {error, #sctp_assoc_change{state :: 'cant_assoc'}} |
                     {error, inet:posix()}
                         when
      Socket :: sctp_socket(),
      Addr   :: inet:ip_address() | inet:hostname(),
      Port   :: inet:port_number(),
      Opts   :: [Opt :: option()].

connect(S, SockAddr, Opts, Timeout)
  when is_map(SockAddr) andalso is_list(Opts) ->
    case do_connect(S, SockAddr, Opts, Timeout, true) of
	badarg ->
	    erlang:error(badarg, [S, SockAddr, Opts, Timeout]);
	Result ->
	    Result
    end;
connect(S, Addr, Port, Opts) ->
    connect(S, Addr, Port, Opts, infinity).

-doc """
Establish an association with a peer.

Establishes a new association for socket `Socket`, with the peer
(SCTP server socket) specified by `Addr` and `Port`.
`Timeout`, is expressed in milliseconds.
A socket can be associated with multiple peers.
The socket has to be of type `seqpacket`.

> #### Warning {: .warning }
>
> Using a value of `Timeout` less than the maximum time taken by the OS to
> establish an association (around 4.5 minutes if the default values from
> [RFC 4960](https://tools.ietf.org/html/rfc4960) are used), can result in
> inconsistent or incorrect return values. This is especially relevant for
> associations sharing the same `Socket` (that is, source address and port), as
> the controlling process blocks until `connect/*` returns.
> [`connect_init/*`](`connect_init/4`) provides an alternative without this
> limitation.

### [](){: #record-sctp_assoc_change } `#sctp_assoc_change{}`

The result of `connect/*` is an `#sctp_assoc_change{}` event that contains,
in particular, the new [Association ID](`t:assoc_id/0`):
l
```erlang
#sctp_assoc_change{
      state             = atom(),
      error             = integer(),
      outbound_streams  = integer(),
      inbound_streams   = integer(),
      assoc_id          = assoc_id()
}
```

The number of outbound and inbound streams for the association
can be set by giving an `sctp_initmsg` option to `connect` as in:

```erlang
connect(Socket, Ip, Port>,
      [{sctp_initmsg,#sctp_initmsg{num_ostreams=OutStreams,
                                   max_instreams=MaxInStreams}}])
```

All options `Opt` are set on the socket before the association is attempted.
If an option record has undefined field values, the options record
is first read from the socket for those values. In effect,
`Opt` option records only need to define field values to change
before connecting.

The returned `outbound_streams` and `inbound_streams` are the number of
stream on the socket. These can be different from the requested values
(`OutStreams` and `MaxInStreams`, respectively), if the peer
requires lower values.

`state` can have the following values:

- **`comm_up`** - Association is successfully established. This indicates
  a successful completion of `connect`.

- **`cant_assoc`** - The association cannot be established
  (`connect/*` failure).

Other states do not normally occur in the output from `connect/*`.
Rather, they can occur in `#sctp_assoc_change{}` events received
instead of data from [`recv/*`](`recv/1`) calls or socket messages.
All of them indicate losing the association because of various
error conditions, and are listed here for the sake of completeness:

- **`comm_lost`**

- **`restart`**

- **`shutdown_comp`**

The field `error` can provide more detailed diagnostics.  Its value
can be converted into a string using `error_string/1`.
""".
-spec connect(Socket, Addr, Port, Opts, Timeout) ->
                     {ok, #sctp_assoc_change{state :: 'comm_up'}} |
                     {error, #sctp_assoc_change{state :: 'cant_assoc'}} |
                     {error, inet:posix()}
                         when
      Socket :: sctp_socket(),
      Addr :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Opts :: [Opt :: option()],
      Timeout :: timeout().

connect(S, Addr, Port, Opts, Timeout) ->
    case do_connect(S, Addr, Port, Opts, Timeout, true) of
	badarg ->
	    erlang:error(badarg, [S,Addr,Port,Opts,Timeout]);
	Result ->
	    Result
    end.



-doc(#{since => <<"OTP 24.3">>,
       equiv => connect_init(Socket, SockAddr, Opts, infinity)}).
-spec connect_init(Socket, SockAddr, Opts) ->
                          ok | {error, inet:posix()} when
      Socket   :: sctp_socket(),
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: [option()].

connect_init(S, SockAddr, Opts) ->
    connect_init(S, SockAddr, Opts, infinity).

-doc(#{since => <<"OTP R13B04">>}).
-doc """
Start establishing an association with a peer.

With arguments `Addr` and `Port`, equivalent to
[`connect_init(Socket, Addr, Port, Opts, infinity)`](`connect/5`).

With arguments `SockAddr` and `Opts` _(since OTP 24.3)_, equivalent to
[`connect_init(Socket, Addr, Port, Opts, Timeout)`](`connect/5`)
where `Addr` and `Port` are extracted from `SockAddr`.
""".
-spec connect_init(Socket, SockAddr, Opts, Timeout) ->
                          ok | {error, inet:posix()} when
      Socket   :: sctp_socket(),
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: [option()],
      Timeout  :: timeout();
                  (Socket, Addr, Port, Opts) ->
                          ok | {error, inet:posix()} when
      Socket :: sctp_socket(),
      Addr   :: inet:ip_address() | inet:hostname(),
      Port   :: inet:port_number(),
      Opts   :: [option()].

connect_init(S, SockAddr, Opts, Timeout)
  when is_map(SockAddr) andalso is_list(Opts) ->
    case do_connect(S, SockAddr, Opts, Timeout, false) of
	badarg ->
	    erlang:error(badarg, [S, SockAddr, Opts, Timeout]);
	Result ->
	    Result
    end;
connect_init(S, Addr, Port, Opts) ->
    connect_init(S, Addr, Port, Opts, infinity).

-doc(#{since => <<"OTP R13B04">>}).
-doc """
Start establishing an association with a peer.

Initiates a new association for socket `Socket`, with the peer
(SCTP server socket) specified by `Addr` and `Port`.

The fundamental difference between this API and `connect/*` is that
the return value is that of the underlying OS `connect(2)` system call.
If `ok` is returned, the operation has been succesfully initiated,
and the final result result of the association establishment
is sent to the socket owner (controlling process) as an
[`#sctp_assoc_change{}`](#record-sctp_assoc_change) event.
The socket owner must be prepared to receive this, the
[`recv/*`](`recv/1`) call has to be polled, depending on the value
of the [active option](#option-active).

The parameters are as described for [`connect/*`](`connect/5`),
except the `Timeout` value since for this function, the time-out only
applies to the name resolving of `Addr` when it is a `t:inet:hostname/0`.
""".
-spec connect_init(Socket, Addr, Port, Opts, Timeout) ->
                          ok | {error, inet:posix()} when
      Socket :: sctp_socket(),
      Addr :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Opts :: [option()],
      Timeout :: timeout().
connect_init(S, Addr, Port, Opts, Timeout) ->
    case do_connect(S, Addr, Port, Opts, Timeout, false) of
	badarg ->
	    erlang:error(badarg, [S,Addr,Port,Opts,Timeout]);
	Result ->
	    Result
    end.

do_connect(S, SockAddr, Opts, Timeout, ConnWait)
  when is_port(S) andalso is_list(Opts) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
            try inet:start_timer(Timeout) of
                Timer ->
                    ConnectTimer = if ConnWait == false ->
                                           nowait;
                                      true ->
                                           Timer
                                   end,
                    Mod:connect(S, inet:ensure_sockaddr(SockAddr), Opts,
                                ConnectTimer)
            catch
                error:badarg ->
                    badarg
            end;
	Error ->
            Error
    end;
do_connect(_S, _SockAddr, _Opts, _Timeout, _ConnWait) ->
    badarg.

do_connect(S, Addr, Service, Opts, Timeout, ConnWait)
  when is_port(S) andalso is_list(Opts) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    case Mod:getserv(Service) of
		{ok,Port} ->
		    try inet:start_timer(Timeout) of
			Timer ->
			    try Mod:getaddr(Addr, Timer) of
				{ok,IP} ->
				    ConnectTimer = if ConnWait == false ->
							   nowait;
						      true ->
							   Timer
						   end,
				    Mod:connect(S, IP, Port, Opts, ConnectTimer);
				Error -> Error
			    after
				_ = inet:stop_timer(Timer)
			    end
		    catch
			error:badarg ->
			    badarg
		    end;
		Error -> Error
	    end;
	Error -> Error
    end;
do_connect(_S, _Addr, _Port, _Opts, _Timeout, _ConnWait) ->
    badarg.



-doc(#{since => <<"OTP 25.0">>}).
-doc """
Start establishing an association with a peer (multiple addresses).

Similar to `connectx_init/5` except using socket addresses, and not having a
`Timeout`. Since the addresses do not need lookup and the connect is
non-blocking this call returns immediately.

The value of each socket address `port` must be the same or zero.
At least one socket address must have a non-zero `port`
""".
-spec connectx_init(Socket, SockAddrs, Opts) ->
                          {ok, assoc_id()} | {error, inet:posix()} when
      Socket   :: sctp_socket(),
      SockAddrs:: [{inet:ip_address(), inet:port_number()} |
                   inet:family_address() |
                   socket:sockaddr_in() | socket:sockaddr_in6()],
      Opts     :: [option()].
%%
connectx_init(S, SockAddrs, Opts) ->
    case do_connectx(S, SockAddrs, Opts) of
	badarg ->
	    erlang:error(badarg, [S, SockAddrs, Opts]);
	Result ->
	    Result
    end.

-doc(#{since => <<"OTP 25.0">>,
       equiv => connectx_init(Socket, Addrs, Port, Opts, infinity)}).
-spec connectx_init(Socket, Addrs, Port, Opts) ->
                          {ok, assoc_id()} | {error, inet:posix()} when
      Socket :: sctp_socket(),
      Addrs :: [inet:ip_address() | inet:hostname()],
      Port :: inet:port_number() | atom(),
      Opts :: [option()].
%%
connectx_init(S, Addrs, Port, Opts) ->
    connectx_init(S, Addrs, Port, Opts, infinity).

-doc(#{since => <<"OTP 25.0">>}).
-doc """
Start establishing an association with a peer (multiple addresses).

Initiates a new association for socket `Socket`, with the peer
(SCTP server socket) specified by `Addrs` and `Port`.

This API is similar to `connect_init/*` except the underlying OS
`sctp_connectx(3)` system call is used, that accepts multiple
destination addresses.

If successful, the association ID is returned which will be received in a
subsequent [`#sctp_assoc_change{}`](#record-sctp_assoc_change)
event.

The parameters are as described in `connect_init/5`

> #### Note {: .info }
> This API allows the OS to use all Addrs when establishing an association,
> but does not guarantee it will. Therefore, if the connection fails,
> the user may want to rotate the order of addresses for a subsequent call.
""".
-spec connectx_init(Socket, Addrs, Port, Opts, Timeout) ->
                          {ok, assoc_id()} | {error, inet:posix()} when
      Socket :: sctp_socket(),
      Addrs :: [inet:ip_address() | inet:hostname()],
      Port :: inet:port_number() | atom(),
      Opts :: [option()],
      Timeout :: timeout().
%%
connectx_init(S, Addrs, Port, Opts, Timeout) ->
    case do_connectx(S, Addrs, Port, Opts, Timeout) of
	badarg ->
	    erlang:error(badarg, [S, Addrs, Port, Opts, Timeout]);
	Result ->
	    Result
    end.


do_connectx(S, SockAddrs, Opts)
  when is_port(S), is_list(SockAddrs), is_list(Opts) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} ->
            case ensure_sockaddrs(SockAddrs) of
                {SockAddrs_1, Port} ->
                    SockAddrs_2 = set_port(SockAddrs_1, Port),
                    Mod:connectx(S, SockAddrs_2, Opts);
                Error1 ->
                    Error1
            end;
        {error, _} = Error2->
            Error2
    end;
do_connectx(_S, _SockAddrs, _Opts) ->
    badarg.

do_connectx(S, Addrs, Service, Opts, Timeout)
  when is_port(S), is_list(Addrs), is_list(Opts) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    case Mod:getserv(Service) of
		{ok,Port} ->
		    try inet:start_timer(Timeout) of
			Timer ->
                            try
                                case getaddrs(Mod, Addrs, Timer) of
                                    IPs when is_list(IPs) ->
                                        Mod:connectx(S, IPs, Port, Opts);
                                    Error1 ->
                                        Error1
                                end
                            after
                                _ = inet:stop_timer(Timer)
                            end
		    catch
			error:badarg ->
                            badarg
		    end;
		{error, _} = Error2 ->
                    Error2
	    end;
	{error, _} = Error3 ->
            Error3
    end;
do_connectx(_S, _Addrs, _Port, _Opts, _Timeout) ->
    badarg.

ensure_sockaddrs(SockAddrs) ->
    ensure_sockaddrs(SockAddrs, 0, []).
%%
ensure_sockaddrs([SockAddr | SockAddrs], Port, Acc) ->
    case SockAddr of
        {IP, P} when is_tuple(IP) ->
            ensure_sockaddrs(SockAddrs, Port, [SockAddr | Acc], P);
        {Family, {_, P}}
          when Family =:= inet;
               Family =:= inet6 ->
            ensure_sockaddrs(SockAddrs, Port, [SockAddr | Acc], P);
        #{family := Family}
          when Family =:= inet;
               Family =:= inet6 ->
            SockAddr_1 = inet:ensure_sockaddr(SockAddr),
            ensure_sockaddrs(
              SockAddrs, Port, [SockAddr_1 | Acc],
              maps:get(port, SockAddr_1, 0));
        _ -> badarg
    end;
ensure_sockaddrs([], 0, _) ->
    badarg;
ensure_sockaddrs([], Port, Acc) ->
    {lists:reverse(Acc), Port}.
%%
ensure_sockaddrs(SockAddrs, Port, Acc, P) ->
    if
        is_integer(P) ->
            if
                0 < P ->
                    ensure_sockaddrs(SockAddrs, P, Acc);
                P < 0 ->
                    badarg;
                true ->
                    ensure_sockaddrs(SockAddrs, Port, Acc)
            end;
        true ->
            badarg
    end.

set_port([SockAddr | SockAddrs], Port) ->
    case SockAddr of
        {IP, P} when is_tuple(IP) ->
            set_port(
              SockAddrs, Port, SockAddr, P,
              fun () -> {IP, Port} end);
        {Family, {Addr, P}} ->
            set_port(
              SockAddrs, Port, SockAddr, P,
              fun () -> {Family, {Addr, Port}} end);
        #{port := P} ->
            set_port(
              SockAddrs, Port, SockAddr, P,
              fun () -> SockAddr#{port := Port} end)
    end;
set_port([], _Port) ->
    [].
%%
set_port(SockAddrs, Port, SockAddr, P, NewSockAddrFun) ->
    [case P of
         Port -> SockAddr;
         _    -> NewSockAddrFun()
     end | set_port(SockAddrs, Port)].

getaddrs(Mod, Addrs, Timer) ->
    getaddrs(Mod, Addrs, Timer, []).
%%
getaddrs(Mod, [Addr | Addrs], Timer, Acc) ->
    case Mod:getaddr(Addr, Timer) of
        {ok, IP} ->
            getaddrs(Mod, Addrs, Timer, [IP | Acc]);
        {error, _} ->
            badarg
    end;
getaddrs(_Mod, [], _Timer, Acc) ->
    lists:reverse(Acc).



-doc """
Terminate an association gracefully.

Gracefully terminates the association specified by `Assoc`, flushing
all unsent data. The socket itself remains open.  Other associations
opened on this socket are still valid.  The socket can be used
in new associations.
""".
-spec eof(Socket, Assoc) -> ok | {error, Reason} when
      Socket :: sctp_socket(),
      Assoc :: #sctp_assoc_change{},
      Reason :: term().

eof(S, #sctp_assoc_change{assoc_id=AssocId}) when is_port(S) ->
    eof_or_abort(S, AssocId, eof);
eof(S, Assoc) ->
    erlang:error(badarg, [S,Assoc]).



-doc """
Abort an association.

Abnormally terminates the association specified by `Assoc`,
without flushing unsent data.  The socket itself remains open.
Other associations opened on this socket are still valid,
and the socket can be used in new associations.
""".
-spec abort(Socket, Assoc) -> ok | {error, inet:posix()} when
      Socket :: sctp_socket(),
      Assoc :: #sctp_assoc_change{}.

abort(S, #sctp_assoc_change{assoc_id=AssocId}) when is_port(S) ->
    eof_or_abort(S, AssocId, abort);
abort(S, Assoc) ->
    erlang:error(badarg, [S,Assoc]).

eof_or_abort(S, AssocId, Action) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:sendmsg(S, #sctp_sndrcvinfo{assoc_id = AssocId,
					    flags    = [Action]},
			<<>>);
	Error -> Error
    end.



-doc """
Send a `Data` message, full-featured.

Sends the `Data` message with all sending parameters from a
[`#sctp_sndrcvinfo{}`](#record-sctp_sndrcvinfo) record.
This way, the user can specify the PPID (passed to the remote end)
and context (passed to the local SCTP layer), which can be used,
for example, for error identification.  However, such a fine grained
user control is rarely required. The function [`send/4`](`send/4`)
is sufficient for most applications.

> #### Note {: .info }
>
> Send is normally blocking, but if the socket option
> [`non_block_send`](#option_non_block_send) is set to true,
> the function will return with e.g. {error, eagain}
> in the case when the function would otherwise block.
> It is then up to the user to try again later.
""".
-spec send(Socket, SndRcvInfo, Data) -> ok | {error, Reason} when
      Socket :: sctp_socket(),
      SndRcvInfo :: #sctp_sndrcvinfo{},
      Data :: binary() | iolist(),
      Reason :: term().

%% Full-featured send. Rarely needed.
send(S, #sctp_sndrcvinfo{}=SRI, Data) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:sendmsg(S, SRI, Data);
	Error -> Error
    end;
send(S, SRI, Data) ->
    erlang:error(badarg, [S,SRI,Data]).

-doc """
Send a data message.

Sends a `Data` message on the association `Assoc` and `Stream`.

`Assoc` can be specified with a
[`#sctp_assoc_change{}`](#record-sctp_assoc_change) record
from an association establishment, or as the `t:assoc_id/0`
`t:integer/0` field value.

> #### Note {: .info }
>
> Send is normally blocking, but if the socket option
> [`non_block_send`](#option_non_block_send) is set to true,
> the function will return with e.g. {error, eagain}
> in the case when the function would otherwise block.
> It is then up to the user to try again later.
""".
-spec send(Socket, Assoc, Stream, Data) -> ok | {error, Reason} when
      Socket :: sctp_socket(),
      Assoc :: #sctp_assoc_change{} | assoc_id(),
      Stream :: integer(),
      Data :: binary() | iolist(),
      Reason :: term().

send(S, #sctp_assoc_change{assoc_id=AssocId}, Stream, Data)
  when is_port(S), is_integer(Stream) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:send(S, AssocId, Stream, Data);
	Error -> Error
    end;
send(S, AssocId, Stream, Data)
  when is_port(S), is_integer(AssocId), is_integer(Stream) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:send(S, AssocId, Stream, Data);
	Error -> Error
    end;
send(S, AssocChange, Stream, Data) ->
    erlang:error(badarg, [S,AssocChange,Stream,Data]).



-doc(#{equiv => recv(Socket, infinity)}).
-spec recv(Socket) -> {ok, {FromIP, FromPort, AncData, Data}}
                          | {error, Reason} when
      Socket :: sctp_socket(),
      FromIP   :: inet:ip_address(),
      FromPort :: inet:port_number(),
      AncData  :: [#sctp_sndrcvinfo{} | inet:ancillary_data()],
      Data     :: binary() | string() | #sctp_sndrcvinfo{}
                | #sctp_assoc_change{} | #sctp_paddr_change{}
                | #sctp_adaptation_event{},
      Reason   :: inet:posix() | #sctp_send_failed{} | #sctp_paddr_change{}
                | #sctp_pdapi_event{} | #sctp_remote_error{}
                | #sctp_shutdown_event{}.

recv(S) ->
    recv(S, infinity).

-doc """
Receive a `Data` message.

Receives the `Data` message from any association of the socket.
If the receive times out, `{error,timeout}` is returned. The default
time-out is `infinity`. `FromIP` and `FromPort` indicate the address
of the sender.

`AncData` is a list of ancillary data items received with the main `Data`.
This list can be empty, or contain a single
[`#sctp_sndrcvinfo{}`](#record-sctp_sndrcvinfo) record,
if receiving ancillary data is enabled
(see option [`sctp_events`](#option-sctp_events)).
Per default, it is enabled, as such ancillary data provides an easy way
to determine the association and stream over which the message was received.
(An alternative way is to get the association ID from `FromIP` and `FromPort`
using socket option
[`sctp_get_peer_addr_info`](#option-sctp_get_peer_addr_info),
but this does still not give the stream number).

`AncData` may also contain [ancillary data](`t:inet:ancillary_data/0`)
from the socket [options](#options)
[`recvtos`](`m:inet#option-recvtos`),
[`recvtclass`](`m:inet#option-recvtclass`) or
[`recvttl`](`m:inet#option-recvttl`), if that is supported for the socket
by the platform.

The `Data` received can, depending on the socket [mode](#option-binary)
be a `t:binary/0` or a `t:list/0` of bytes (integers in the range
`0` through `255`), or it can be an SCTP event.

### [](){: #sctp-events } Possible SCTP events

- [`#sctp_sndrcvinfo{}`](#record-sctp_sndrcvinfo)
- [`#sctp_assoc_change{}`](#record-sctp_assoc_change)
- ```erlang
  #sctp_paddr_change{
        addr      = {ip_address(),port()},
        state     = atom(),
        error     = integer(),
        assoc_id  = assoc_id()
  }
  ```

  Indicates change of the status of the IP address of the peer specified by
  `addr` within association `assoc_id`. Possible values of `state` (mostly
  self-explanatory) include:

  - **`addr_unreachable`**

  - **`addr_available`**

  - **`addr_removed`**

  - **`addr_added`**

  - **`addr_made_prim`**

  - **`addr_confirmed`**

  In case of an error (for example, `addr_unreachable`), the field `error`
  provides more diagnostics. In such cases, event `#sctp_paddr_change{}`
  is automatically converted into an `error` term returned by
  [`recv`](`recv/1`). The `error` field value can be converted
  into a string using `error_string/1`.

- ```erlang
  #sctp_send_failed{
        flags     = true | false,
        error     = integer(),
        info      = #sctp_sndrcvinfo{},
        assoc_id  = assoc_id()
        data      = binary()
  }
  ```

  The sender can receive this event if a send operation fails.

  - **`flags`** - A Boolean specifying if the data has been transmitted
    over the wire.

  - **`error`** - Provides extended diagnostics, use
    [`error_string/1`.](`error_string/1`)

  - **`info`** - The original
    [`#sctp_sndrcvinfo{}`](#record-sctp_sndrcvinfo) record
    used in the failed [`send/*`.](`send/3`)

  - **`data`** - The whole original data chunk attempted to be sent.

  In the current implementation of the Erlang/SCTP binding, this event is
  internally converted into an `error` term returned by [`recv/*`](`recv/1`).

- ```erlang
  #sctp_adaptation_event{
        adaptation_ind = integer(),
        assoc_id       = assoc_id()
  }
  ```

  Delivered when a peer sends an adaptation layer indication parameter
  (configured through option
  [`sctp_adaptation_layer`](#option-sctp_adaptation_layer)).
  Notie that with the current implementation of the Erlang/SCTP binding,
  this event is disabled by default.

- ```erlang
  #sctp_pdapi_event{
        indication = sctp_partial_delivery_aborted,
        assoc_id   = assoc_id()
  }
  ```

  A partial delivery failure. In the current implementation
  of the Erlang/SCTP binding, this event is internally converted
  into an `error` term returned by [`recv/*`](`recv/1`).
""".
-spec recv(Socket, Timeout) -> {ok, {FromIP, FromPort, AncData, Data}}
                                   | {error, Reason} when
      Socket :: sctp_socket(),
      Timeout :: timeout(),
      FromIP   :: inet:ip_address(),
      FromPort :: inet:port_number(),
      AncData  :: [#sctp_sndrcvinfo{} | inet:ancillary_data()],
      Data     :: binary() | string() | #sctp_sndrcvinfo{}
                | #sctp_assoc_change{} | #sctp_paddr_change{}
                | #sctp_adaptation_event{},
      Reason   :: inet:posix() | #sctp_send_failed{} | #sctp_paddr_change{}
                | #sctp_pdapi_event{} | #sctp_remote_error{}
                | #sctp_shutdown_event{}.

recv(S, Timeout) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:recv(S, Timeout);
	Error -> Error
    end;
recv(S, Timeout) ->
    erlang:error(badarg, [S,Timeout]).


-doc """
Translate an error number into a string or atom.

Translates an SCTP error number from, for example, `#sctp_remote_error{}`
or `#sctp_send_failed{}` into an explanatory string, or into
one of the atoms `ok` for no error, or `unknown_error`
for an unrecognized integer.
""".
-spec error_string(ErrorNumber) -> ok | string() | unknown_error when
      ErrorNumber :: integer().

error_string(0) ->
    ok;
error_string(1) ->
    "Invalid Stream Identifier";
error_string(2) ->
    "Missing Mandatory Parameter";
error_string(3) ->
    "Stale Cookie Error";
error_string(4) ->
    "Out of Resource";
error_string(5) ->
    "Unresolvable Address";
error_string(6) ->
    "Unrecognized Chunk Type";
error_string(7) ->
    "Invalid Mandatory Parameter";
error_string(8) ->
    "Unrecognized Parameters";
error_string(9) ->
    "No User Data";
error_string(10) ->
    "Cookie Received While Shutting Down";
error_string(11) ->
    "Restart of an Association with New Addresses";
error_string(12) ->
    "User Initiated Abort";
error_string(13) ->
    "Protocol Violation";
%% For more info on principal SCTP error codes: phone +44 7981131933
error_string(N) when is_integer(N) ->
    unknown_error;
error_string(X) ->
    erlang:error(badarg, [X]).


-doc """
Change the controlling process (owner) of a socket.

Assigns a new controlling process `Pid` to `Socket`.
See `gen_udp:controlling_process/2`.
""".
-spec controlling_process(Socket, Pid) -> ok | {error, Reason} when
      Socket :: sctp_socket(),
      Pid :: pid(),
      Reason :: closed | not_owner | badarg | inet:posix().

controlling_process(S, Pid) when is_port(S), is_pid(Pid) ->
    inet:udp_controlling_process(S, Pid);
controlling_process(S, Pid) ->
    erlang:error(badarg, [S,Pid]).
