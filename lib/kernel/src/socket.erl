%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2024. All Rights Reserved.
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

-module(socket).
-moduledoc """
Socket interface.

This module provides an API for network sockets. Functions are provided to
create, delete and manipulate the sockets as well as sending and receiving data
on them.

The intent is that it shall be as "close as possible" to the OS level socket
interface. The only significant addition is that some of the functions, e.g.
`recv/3`, have a time-out argument.

[](){: #asynchronous-calls }

> #### Note {: .info }
>
> #### Asynchronous Calls
>
> Some functions feature _asynchronous calls_.  This is achieved by setting
> the `Timeout` argument to `nowait` or to a `Handle ::` `t:reference/0`.
> See the respective function's type specification.
>
> This module has two different implementations of asynchronous calls.
> One on the _Unix_ family of operating systems:
> `select` - based on the standard socket interface's
> `select(3)`/`poll(3)` calls, and one on _Windows_: `completion` -
> based on asynchronous I/O Completion Ports.
> The difference shows in the return values and message formats
> because they have slightly different semantics.
>
> #### The `completion` and `select` Return Values
>
> For instance, if calling `recv/3` like this;
> [`recv(Socket, 0, nowait)`](#recv-nowait), when there is
> no data available for reading, it will return one of:
>
> - `{completion, `[`CompletionInfo`](`t:completion_info/0`)`}`
> - `{select, `[`SelectInfo`](`t:select_info/0`)`}`
>
> `CompletionInfo` contains a [CompletionHandle](`t:completion_handle/0`) and
> `SelectInfo` contains a [SelectHandle](`t:select_handle/0`).
> Both are types are aliases to `t:reference/0`.
> When the operation can continue, a `completion` message containing
> the `CompletionHandle` or a `select` message containing
> the `SelectHandle` is sent to the calling process.
>
> On `select` systems some functions may also return:
>
> - `{select, {`[`SelectInfo`](`t:select_info/0`)`, _}`
>
> This may happen for sockets of type [`stream`](`t:type/0`) where
> the stream handling can split the data stream at any point.
> See the respective function's type specification's return type.
>
> #### The `completion` and `select` Messages
>
> The `completion` message has the format:
>
> - `{'$socket', `[`Socket`](`t:socket/0`)`, completion,
>   {`[`CompletionHandle`](`t:completion_handle/0`)`, CompletionStatus}}`
>
> The `select` message has the format:
>
> - `{'$socket', `[`Socket`](`t:socket/0`)`, select,
>   `[`SelectHandle`](`t:select_handle/0`)`}`
>
> When a `completion` message is received (which contains the _result_
> of the operation), it means that the operation has been _completed_ with
> `CompletionStatus :: ok | {error, Reason}`.
> See the respective function's documentation for possible values
> of `Reason`, which are the same `{error, Reason}` values
> that can be returned by the function itself.
>
> When a `select` message is received, it only means that the operation
> _may now continue_, by retrying the operation (which may return
> a new `{select, _}` value).  Some operations are retried by repeating
> the same function call, and some have a dedicated function variant
> to be used for the retry.  See the respective function's documentation.
>
> #### Operation Queuing on `select` Systems
>
> On `select` systems, all other processes are _locked out_ until the
> current process has completed the operation as in a continuation
> call has returned a value indicating success or failure
> (not a `select` return).  Other processes are queued and get
> a `select` return which makes them wait for their turn.
>
> #### Canceling an operation
>
> An operation that is in progress (not completed) may be canceled
> using `cancel/2` both on `completion` and `select` systems.
>
> Canceling an operation ensures that there is no `completion`,
> `select`, nor `abort` message in the inbox after the `cancel/2` call.
>
> #### Using a `Handle`
>
> If creating a `t:reference/0` with [`make_ref()`](`erlang:make_ref/0`)
> and using that as the `Timeout | Handle` argument, the same `Handle`
> will then be the [`SelectHandle`](`t:select_handle/0`) in the returned
> `t:select_info/0` and the received `select` message, or be
> the [`CompletionHandle`](`t:completion_handle/0`) in the returned
> `t:completion_info/0` and the received `completion` message.
>
> The compiler may then optimize a following `receive` statement
> to only scan the messages that arrive after the `t:reference/0`
> is created.  If the message queue is large this is a big optimization.
>
> The `t:reference/0` has to be unique for the call.
>
> #### Repeating an Operation on a `select` Systems
>
> On`select` systems, if a call would be repeated _before_ the `select`
> message has been received it replaces the call in progress:
>
> ```erlang
>     {select, {select_info, Handle}} = socket:accept(LSock, nowait),
>     {error, timeout} = socket:accept(LSock, 500),
>     :
> ```
> Above, `Handle` is _no longer_ valid once the second `accept/2`, call
> has been made (the first call is automatically canceled).
> After the second `accept/2` call returns `{error, timeout}`,
> the accept operation has completed.
>
> Note that there is a race here; there is no way to know if the call
> is repeated _before_ the `select` message is sent since it _may_
> have been sent just before the repeated call.  So now there
> might be a `select` message containing `Handle` in the inbox.
>
> #### The `abort` Message
>
> Another message the user must be prepared for
> (when using `nowait | Handle`) is the `abort` message:
>
> - `{'$socket', `[`Socket`](`t:socket/0`)`, abort, Info}`
>
> This message indicates that the operation in progress has been aborted.
> For instance, if the socket has been closed by another process;
> `Info` will be `{Handle, closed}`.

> #### Note {: .info }
>
> Support for IPv6 has been implemented but not _fully_ tested.
>
> SCTP has only been partly implemented (and not tested).

This module was introduced in OTP 22.0, as experimental code.
* In OTP 22.1, the `nowait` argument was added for many functions,
  and the `cancel/2` and `info/1` functions were also added.
* In OTP 22.3, the `number_of/0` function was added.
* In OTP 23.0, the functions [`is_supported/1,2`](`is_supported/1`)
  and the [`open/1,2`](`open/1`) functions with an `FD` argument were added.
* In OTP 23.1, the `use_registry/1` function was added.
* In OTP 24.0, the `t:select_handle/0` argument was added for many functions,
  the `cancel/1`, `cancel_monitor/1`, `getopt/3` with tuple options,
  `getopt_native/3`, `info/0`, `monitor/1`, `open/3` with an option list,
  many variants of the [`recv/*`](`recv/2`),
  [`recvfrom/*`](`recvfrom/1`), [`recvmsg/*`](`recvmsg/1`),
  [`send/*`](`send/2`), [`sendmsg/*`](`sendmsg/2`),
  the [`sendto/*`](`sendto/3`) functions,
  the [`sendfile/*`](`sendfile/5`) functions,
  and the `setopt/3`, `setopt_native/3` functions, were added.
* In OTP 24.1, the [`i/*`](`i/0`) functions were added.
* In OTP 24.2, the [`ioctl/*`](`ioctl/2`) functions were added.
* In OTP 26.0, the `t:completion_handle/0` argument was added for
  many functions, and the `cancel/2` function with `t:completion_info/0`
  argument was added.  That is: support for Windows asynchronous
  I/O Completion Ports was added.  The Unix-ish flavored
  ([select handle](`t:select_handle/0`)) API features could be considered
  no longer experimental.
* In OTP 27.0, the Windows flavored
  ([completion handle](`t:completion_handle/0`))
  API features could be considered no longer experimental.

## Examples

```erlang
client(SAddr, SPort) ->
   {ok, Sock} = socket:open(inet, stream, tcp),
   ok = socket:connect(Sock, #{family => inet,
                               addr   => SAddr,
                               port   => SPort}),
   Msg = <<"hello">>,
   ok = socket:send(Sock, Msg),
   ok = socket:shutdown(Sock, write),
   {ok, Msg} = socket:recv(Sock),
   ok = socket:close(Sock).

server(Addr, Port) ->
   {ok, LSock} = socket:open(inet, stream, tcp),
   ok = socket:bind(LSock, #{family => inet,
                             port   => Port,
                             addr   => Addr}),
   ok = socket:listen(LSock),
   {ok, Sock} = socket:accept(LSock),
   {ok, Msg} = socket:recv(Sock),
   ok = socket:send(Sock, Msg),
   ok = socket:close(Sock),
   ok = socket:close(LSock).
```
""".
-moduledoc(#{since => "OTP 22.0"}).

-compile({no_auto_import, [error/1, monitor/1]}).

%% Administrative and "global" utility functions
-export([
	 %% (registry) Socket functions
         number_of/0,
         which_sockets/0, which_sockets/1,

	 %% (registry) Socket monitor functions
         number_of_monitors/0, number_of_monitors/1,
         which_monitors/1,
	 monitored_by/1,

         debug/1, socket_debug/1, use_registry/1,
	 info/0, info/1,
	 i/0, i/1, i/2,
         tables/0, table/1,
         monitor/1, cancel_monitor/1,
         supports/0, supports/1, supports/2,
         is_supported/1, is_supported/2, is_supported/3,

	 to_list/1
        ]).

-export([
         open/1, open/2, open/3, open/4,
         bind/2, bind/3,
         connect/1, connect/2, connect/3,
         listen/1, listen/2,
         accept/1, accept/2,

         send/2, send/3, send/4,
         sendto/3, sendto/4, sendto/5,
         sendmsg/2, sendmsg/3, sendmsg/4,
         sendv/2, sendv/3, sendv/4,

         sendfile/2, sendfile/3, sendfile/4, sendfile/5,

         recv/1, recv/2, recv/3, recv/4,
         recvfrom/1, recvfrom/2, recvfrom/3, recvfrom/4,
         recvmsg/1, recvmsg/2, recvmsg/3, recvmsg/4, recvmsg/5,

         close/1,
         shutdown/2,

         setopt/3, setopt_native/3, setopt/4,
         getopt/2, getopt_native/3, getopt/3,

         sockname/1,
         peername/1,

         ioctl/2, ioctl/3, ioctl/4,

         cancel/2
        ]).

%% Misc utility functions
-export([
	 which_socket_kind/1,
         options/0, options/1, options/2, option/1, option/2,
         protocols/0, protocol/1
	]).

-export_type([
              socket/0,
              socket_handle/0,

              select_tag/0,
              select_handle/0,
              select_info/0,

              completion_tag/0,
              completion_handle/0,
              completion_info/0,

              invalid/0,
              eei/0,

              socket_counters/0,
              socket_info/0,

              domain/0,
              type/0,
              protocol/0,

              port_number/0,
              in_addr/0,
              in6_addr/0,
              sockaddr/0, sockaddr_recv/0,
              sockaddr_in/0,
              sockaddr_in6/0,
              sockaddr_un/0,
              sockaddr_ll/0,
              sockaddr_dl/0,
              sockaddr_unspec/0,
              sockaddr_native/0,

              msg_flag/0,

              level/0,
              otp_socket_option/0,
              socket_option/0,

              %% Option values' types
              linger/0,
              timeval/0,
              ip_mreq/0,
              ip_mreq_source/0,
              ip_msfilter/0,
              ip_pmtudisc/0,
              ip_tos/0,
              ip_pktinfo/0,

              ipv6_mreq/0,
              ipv6_pmtudisc/0,
              ipv6_hops/0,
              ipv6_pktinfo/0,

              sctp_assocparams/0,
              sctp_event_subscribe/0,
              sctp_initmsg/0,
              sctp_rtoinfo/0,

              msg/0, msg_send/0, msg_recv/0,
              cmsg/0, cmsg_send/0, cmsg_recv/0,

              ee_origin/0,
              icmp_dest_unreach/0,
              icmpv6_dest_unreach/0,
              extended_err/0,

	      info_keys/0
             ]).

%% DUMMY
-export_type([ioctl_device_flag/0, ioctl_device_map/0]).

%% We need #file_descriptor{} for sendfile/2,3,4,5
-include("file_int.hrl").

%% -define(DBG(T),
%%         erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).

%% Also in prim_socket
-define(REGISTRY, socket_registry).

-type invalid() :: {invalid, What :: term()}.

%% Extended Error Information
-doc """
Extended Error Information.

A term containing additional (error) information
_if_ the socket NIF has been configured to produce it.
""".
-type eei() :: #{info := econnreset | econnaborted |
                 netname_deleted | too_many_cmds | atom(),
                 raw_info := term()}.

-doc """
Platform dependent information items.

The value of `iov_max` is the value of the `IOV_MAX` constant
in the system headers, which is the largest allowed I/O vector.
See also `sendmsg/4` regarding the `iov` key of `t:msg_send/0`.
The smallest allowed `IOV_MAX` value according to POSIX is `16`,
but check your platform documentation to be sure.

About the `use_registry` key, see `use_registry/1`
and the `t:otp_socket_option/0` with the same name.
""".
-type info() ::
        #{counters     := #{atom() := non_neg_integer()},
          iov_max      := non_neg_integer(),
          use_registry := boolean(),
          io_backend   := #{name := atom()}}.

-doc "A `t:map/0` of `Name := Counter` associations.".
-type socket_counters() :: #{read_byte        := non_neg_integer(),
                             read_fails       := non_neg_integer(),
                             read_pkg         := non_neg_integer(),
                             read_pkg_max     := non_neg_integer(),
                             read_tries       := non_neg_integer(),
                             read_waits       := non_neg_integer(),
                             write_byte       := non_neg_integer(),
                             write_fails      := non_neg_integer(),
                             write_pkg        := non_neg_integer(),
                             write_pkg_max    := non_neg_integer(),
                             write_tries      := non_neg_integer(),
                             write_waits      := non_neg_integer(),
                             sendfile         => non_neg_integer(),
                             sendfile_byte    => non_neg_integer(),
                             sendfile_fails   => non_neg_integer(),
                             sendfile_max     => non_neg_integer(),
                             sendfile_pkg     => non_neg_integer(),
                             sendfile_pkg_max => non_neg_integer(),
                             sendfile_tries   => non_neg_integer(),
                             sendfile_waits   => non_neg_integer(),
                             acc_success      := non_neg_integer(),
                             acc_fails        := non_neg_integer(),
                             acc_tries        := non_neg_integer(),
                             acc_waits        := non_neg_integer()}.

-type socket_info() :: #{domain        := domain() | integer(),
                         type          := type() | integer(),
                         protocol      := protocol() | integer(),
                         owner         := pid(),
                         ctype         := normal | fromfd | {fromfd, integer()},
                         counters      := socket_counters(),
                         num_readers   := non_neg_integer(),
                         num_writers   := non_neg_integer(),
                         num_acceptors := non_neg_integer(),
                         writable      := boolean(),
                         readable      := boolean(),
			 rstates       := [atom()],
			 wstates       := [atom()]}.


%% We support only a subset of all domains.
-doc """
Protocol _domain_ a.k.a address _family_.

A lowercase `t:atom/0` representing a protocol _domain_
on the platform named `AF_*` (or `PF_*`).  For example
`inet` corresponds to `AF_INET`.

[`is_supported(ipv6)` ](`is_supported/1`) tells if the IPv6 protocol,
protocol domain `inet6`, is supported.

[`is_supported(local)` ](`is_supported/1`) tells if the
protocol domain `local` is supported.

`supports/0` reports both values, but also many more, with a single call.
""".
-type domain() :: inet | inet6 | local | unspec.

%% We support only a subset of all types.
%% RDM - Reliably Delivered Messages
-doc """
Protocol type.

A lowercase `t:atom/0` representing a protocol _type_
on the platform named `SOCK_*`.  For example
`stream` corresponds to `SOCK_STREAM`.
""".
-type type()   :: stream | dgram | raw | rdm | seqpacket.

%% We support all protocols enumerated by getprotoent(),
%% and all of ip | ipv6 | tcp | udp | sctp that are supported
%% by the platform, even if not enumerated by getprotoent()
-doc """
Protocol name.

A lowercase `t:atom/0` representing an OS protocol name.
To be used for example in `t:socket_option/0`
in [control messages](`t:cmsg/0`).

They have the following names in the OS header files:

- **`ip`** - `IPPROTO_IP` a.k.a `SOL_IP` with options named `IP_`\*.

- **`ipv6`** - `IPPROTO_IPV6` a.k.a `SOL_IPV6` with options named `IPV6_`\*.

- **`tcp`** - `IPPROTO_TCP` with options named `TCP_`\*.

- **`udp`** - `IPPROTO_UDP` with options named `UDP_`\*.

- **`sctp`** - `IPPROTO_SCTP` with options named `SCTP_`\*.

There are many other possible protocols, but the ones above are those for which
this socket library implements socket options and/or control messages.

All protocols known to the OS are enumerated when the Erlang VM is started,
through the `C` library call `getprotoent()`. See the OS man page for
protocols(5). Those in the list above are valid if supported by the platform,
even if they aren't enumerated.

The calls [`is_supported(ipv6)` ](`is_supported/1`)
and [`is_supported(sctp)` ](`is_supported/1`) can be used to find out
if the protocols `ipv6` and `sctp` are supported on the platform
as in appropriate header file and library exists.

The call [`is_supported(protocols, Protocol)` ](`is_supported/2`)
can only be used to find out if the platform knows the protocol number
for a named `Protocol`.

See [`open/2,3,4`](`open/3`)
""".
-type protocol() :: atom().

-type port_number() :: 0..65535.

-type in_addr() :: {0..255, 0..255, 0..255, 0..255}.

-type in6_flow_info() :: 0..16#FFFFF.
-type in6_scope_id()  :: 0..16#FFFFFFFF.

-type in6_addr() ::
           {0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535}.

-doc """
C: `struct linger`

Corresponds to the C `struct linger` for managing the
[socket option](`t:socket_option/0`) `{socket, linger}`.
""".
-type linger() ::
        #{onoff  := boolean(),
          linger := non_neg_integer()}.

-doc """
C: `struct timeval`

Corresponds to the C `struct timeval`. The field `sec` holds seconds, and `usec`
microseconds.
""".
-type timeval() ::
        #{sec  := integer(),
          usec := integer()}.

-doc """
C: `struct ip_mreq`

Corresponds to the C `struct ip_mreq` for managing multicast groups.
""".
-type ip_mreq() ::
        #{multiaddr := in_addr(),
          interface := in_addr()}.

-doc """
C: `struct ip_mreq_source`

Corresponds to the C `struct ip_mreq_source` for managing multicast groups.
""".
-type ip_mreq_source() ::
        #{multiaddr  := in_addr(),
          interface  := in_addr(),
          sourceaddr := in_addr()}.

-doc """
C: `struct ip_msfilter`

Corresponds to the C `struct ip_msfilter` for managing
multicast source filtering (RFC 3376).
""".
-type ip_msfilter() ::
        #{multiaddr := in_addr(),
          interface := in_addr(),
          mode      := 'include' | 'exclude',
          slist     := [ in_addr() ]}.

-doc """
C: `IP_PMTUDISC_*` values.

Lowercase `t:atom/0` values corresponding to the C library constants
`IP_PMTUDISC_*`. Some constant(s) may be unsupported by the platform.
""".
-type ip_pmtudisc() ::
        want | dont | do | probe.

%% If the integer value is used, its up to the caller to ensure its valid!
-doc """
C: `IPTOS_*` values.

Lowercase `t:atom/0` values corresponding to the C library constants `IPTOS_*`.
Some constant(s) may be unsupported by the platform.
""".
-type ip_tos() :: lowdelay |
                  throughput |
                  reliability |
                  mincost.

-doc "C: `struct ip_pktinfo`".
-type ip_pktinfo() ::
        #{ifindex  := non_neg_integer(), % Interface Index
          spec_dst := in_addr(),         % Local Address
          addr     := in_addr()          % Header Destination address
         }.


-doc """
C: `struct ipv6_mreq`

Corresponds to the C `struct ipv6_mreq` for managing multicast groups. See also
RFC 2553.
""".
-type ipv6_mreq() ::
        #{multiaddr := in6_addr(),
          interface := non_neg_integer()}.

-doc """
C: `IPV6_PMTUDISC_*` values

Lowercase `t:atom/0` values corresponding to the C library constants
`IPV6_PMTUDISC_*`. Some constant(s) may be unsupported by the platform.
""".
-type ipv6_pmtudisc() ::
        want | dont | do | probe.

-doc """
IPv6 hop limit value.

The value `default` is only valid to _set_ and is translated to the C value
`-1`, meaning the route default.
""".
-type ipv6_hops() ::
        default | 0..255.

-doc "C: `struct in6_pktinfo`".
-type ipv6_pktinfo() ::
        #{addr    := in6_addr(),
          ifindex := integer()
         }.

-doc "C: `struct sctp_assocparams`".
-type sctp_assocparams() ::
        #{assoc_id                := integer(),
          asocmaxrxt              := 0..16#ffff,
          numbe_peer_destinations := 0..16#ffff,
          peer_rwnd               := 0..16#ffffffff,
          local_rwnd              := 0..16#ffffffff,
          cookie_life             := 0..16#ffffffff}.

-doc """
C: `struct sctp_event_subscribe`.

Not all fields are implemented on all platforms; unimplemented fields are
ignored, but implemented fields are mandatory. Note that the '\_event' suffixes
have been stripped from the C struct field names, for convenience.
""".
-type sctp_event_subscribe() ::
        #{data_io          := boolean(),
          association      := boolean(),
          address          := boolean(),
          send_failure     := boolean(),
          peer_error       := boolean(),
          shutdown         := boolean(),
          partial_delivery := boolean(),
          adaptation_layer => boolean(),
          sender_dry       => boolean()}.

-doc "C: `struct sctp_initmsg`.".
-type sctp_initmsg() ::
        #{num_ostreams   := 0..16#ffff,
          max_instreams  := 0..16#ffff,
          max_attempts   := 0..16#ffff,
          max_init_timeo := 0..16#ffff}.

-doc "C: `struct sctp_rtoinfo`.".
-type sctp_rtoinfo() ::
        #{assoc_id := integer(),
          initial  := 0..16#ffffffff,
          max      := 0..16#ffffffff,
          min      := 0..16#ffffffff}.

-type packet_type() :: host | broadcast | multicast | otherhost |
                       outgoing | loopback | user | kernel | fastroute |
                       non_neg_integer().

-type hatype() :: netrom | eether | ether | ax25 | pronet | chaos |
                  ieee802 | arcnet | appletlk | dlci | atm | metricom |
                  ieee1394 | eui64 | infiniband |
                  tunnel | tunnel6 | loopback | localtlk |
                  none | void |
                  non_neg_integer().

-doc """
C: `struct sockaddr_un`.

A Unix Domain socket address, a.k.a local address (`AF_LOCAL`).

The `path` element will always be a `binary` when returned from this module.
When supplied to an API function in this module it may be a `t:string/0`, which
will be encoded into a binary according to the
[native file name encoding ](`file:native_name_encoding/0`)on the platform.

A terminating zero character will be appended before the address path is given
to the OS, and the terminating zero will be stripped before giving the address
path to the caller.

Linux's non-portable abstract socket address extension is handled by not doing
any terminating zero processing in either direction, if the first byte of the
address is zero.
""".
-type sockaddr_un() ::
        #{family := 'local',
          path   := binary() | string()}.

-doc """
C: `struct sockaddr_in`

[Domain `inet`](`t:domain/0`) (IPv4) address.
""".
-type sockaddr_in() ::
        #{family := 'inet',
          port   := port_number(),
          %% The 'broadcast' here is the "limited broadcast"
          addr   := 'any' | 'broadcast' | 'loopback' | in_addr()}.

-doc """
C: `struct sockaddr_in6`

[Domain `inet6`](`t:domain/0`) (IPv6) address.
""".
-type sockaddr_in6() ::
        #{family   := 'inet6',
          port     := port_number(),
          addr     := 'any' | 'loopback' | in6_addr(),
          flowinfo := in6_flow_info(),
          scope_id := in6_scope_id()}.

-doc """
C: `struct sockaddr_ll`

[Domain `packet`](`t:domain/0`), [type `raw`](`t:type/0`)
(link level) address.
""".
-type sockaddr_ll() ::
        #{family   := 'packet',
          protocol := non_neg_integer(),
          ifindex  := integer(),
          pkttype  := packet_type(),
          hatype   := hatype(),
          addr     := binary()}.

-doc """
C: `struct sockaddr_dl`

Link level address (PF_LINK) on BSD:s.
""".
-type sockaddr_dl() ::
        #{family   := 'link',
          index    := non_neg_integer(),
          type     := non_neg_integer(),
          nlen     := non_neg_integer(),
          alen     := non_neg_integer(),
          slen     := non_neg_integer(),
          data     := binary()}.

-doc """
C: `struct sockaddr` of `AF_UNSPEC`

In C, a `struct sockaddr` with `sa_family = AF_UNSPEC`
and the content of `sa_data` in the `t:map/0` key `addr`.
""".
-type sockaddr_unspec() ::
        #{family := 'unspec', addr := binary()}.

-doc """
C: `struct sockaddr`

In C, a `struct sockaddr` with the integer value of `sa_family`
in the `t:map/0` key `family`,
and the content of `sa_data` in the `t:map/0` key `addr`.
""".
-type sockaddr_native() ::
        #{family := integer(), addr := binary()}.

-type sockaddr() ::
        sockaddr_in()      |
        sockaddr_in6()     |
        sockaddr_un()      |
        sockaddr_ll()      |
        sockaddr_dl()      |
        sockaddr_unspec()  |
        sockaddr_native().

-type sockaddr_recv() ::
        sockaddr() | binary().

%% (otp)      - This option is internal to our (OTP) implementation.
%% socket     - The socket layer (SOL_SOCKET).
%% (Int)      - Raw level, sent down and used "as is".
%% protocol() - Protocol number; ip | ipv6 | tcp | udp | sctp | ...
-doc """
Protocol level.

A lowercase `t:atom/0` OS protocol level, that is:
`socket` or a `t:protocol/0` name.

`socket` is the `SOL_SOCKET` protocol level in the OS header files,
with options named `SO_`\* .
""".
-type level() ::
        %% otp | % Has got own clauses in setopt/getopt
        %% integer() % Has also got own clauses
        socket | %% Handled explicitly
        protocol().

%% There are some options that are 'read-only'.
%% Should those be included here or in a special list?
%% Should we just document it and leave it to the user?
%% Or catch it in the encode functions?
%% A setopt for a read-only option leads to {error, invalid()}?
%% Do we really need a sndbuf?

-doc """
Protocol level `otp` socket option.

Socket options for the `otp` pseudo protocol level,
that is: `{otp, Name}` options.

This protocol level is the Erlang/OTP's socket implementation layer,
hence above all OS protocol levels.

- **`debug`** - `t:boolean/0` \- Activate debug logging.

- **`iow`** - `t:boolean/0` \- Inform On Wrap of statistics counters.

- **`controlling_process`** - `t:pid/0` \- The socket "owner". Only the current
  controlling process can set this option.

- **`rcvbuf`** -
  `BufSize :: (default | integer()>0) | {N :: integer()>0, BufSize :: (default | integer()>0)} `\-
  Receive buffer size.

  The value `default` is only valid to _set_.

  `N` specifies the number of read attempts to do in a tight loop before
  assuming no more data is pending.

  This is the allocation size for the receive buffer used when calling the OS
  protocol stack's receive API, when no specific size (size 0) is requested.
  When the receive function returns the receive buffer is reallocated to the
  actually received size. If the data is copied or shrunk in place is up to
  the allocator, and can to some extent be configured in the Erlang VM.

  The similar socket option; `{socket,rcvbuf}` is a related option for the OS'
  protocol stack that on Unix corresponds to `SOL_SOCKET,SO_RCVBUF`.

- **`rcvctrlbuf`** - `BufSize :: (default | integer()>0) `\- Allocation size for
  the ancillary data buffer used when calling the OS protocol stack's receive
  API.

  The value `default` is only valid to _set_.

- **`sndctrlbuf`** - `BufSize :: (default | integer()>0) `\- Allocation size for
  the ancillary data buffer used when calling the OS protocol stack's
  [sendmsg](`sendmsg/2`) API.

  The value `default` is only valid to _set_.

  It is the user's responsibility to set a buffer size that has room for the
  encoded ancillary data in the message to send.

  See [sendmsg](`sendmsg/2`) and also the `ctrl` field of the `t:msg_send/0`
  type.

- **`fd`** - `t:integer/0` \- Only valid to _get_. The OS protocol levels'
  socket descriptor. Functions [`open/1,2`](`open/1`) can be used to create a
  socket according to this module from an existing OS socket descriptor.

- **`use_registry`** - `t:boolean/0` \- Only valid to _get_. The value is set
  when the socket is created with `open/2` or `open/4`.

Options not described here are intentionally undocumented and for Erlang/OTP
internal use only.
""".
-type otp_socket_option() ::
        debug |
        iow |
        controlling_process |
        rcvbuf | % sndbuf |
        rcvctrlbuf |
        sndctrlbuf |
        meta |
        use_registry |
        fd |
        domain.

-doc """
Socket option.

Socket options on the form `{Level, Opt}` where the OS protocol `Level` =
`t:level/0` and `Opt` is a socket option on that protocol level.

The OS name for an options is, except where otherwise noted, the `Opt` atom, in
capitals, with prefix according to `t:level/0`.

> #### Note {: .info }
>
> The `IPv6` option `pktoptions` is a special (barf) case. It is intended for
> backward compatibility usage only.
>
> Do _not_ use this option.

> #### Note {: .info }
>
> See the OS documentation for every socket option.

An option below that has the value type `t:boolean/0` will translate the value
`false` to a C `int` with value `0`, and the value `true` to `!!0` (not (not
false)).

An option with value type `t:integer/0` will be translated to a C `int` that may
have a restricted range, for example byte: `0..255`. See the OS documentation.

The calls [`supports(options)`](`supports/1`),
[`supports(options, Level)`](`supports/1`) and
[`is_supported(options, {Level, Opt})` ](`is_supported/2`)can be used to find
out which socket options that are supported by the platform.

_Options for protocol level_ [_`socket`_:](`t:level/0`)

- **`{socket, acceptconn}`** - `Value = boolean()`

- **`{socket, bindtodevice}`** - `Value = string()`

- **`{socket, broadcast}`** - `Value = boolean()`

- **`{socket, debug}`** - `Value = integer()`

- **`{socket, domain}`** - `Value =` `t:domain/0`

  Only valid to _get_.

  The socket's protocol domain. Does _not_ work on for instance FreeBSD.

- **`{socket, dontroute}`** - `Value = boolean()`

- **`{socket, keepalive}`** - `Value = boolean()`

- **`{socket, linger}`** - `Value = abort |` `t:linger/0`

  The value `abort` is shorthand for `#{onoff => true, linger => 0}`, and only
  valid to _set_.

- **`{socket, oobinline}`** - `Value = boolean()`

- **`{socket, passcred}`** - `Value = boolean()`

- **`{socket, peek_off}`** - `Value = integer()`

  Currently disabled due to a possible infinite loop when calling
  [`recv/1-4`](`recv/1`) with [`peek`](`t:msg_flag/0`) in `Flags`.

- **`{socket, priority}`** - `Value = integer()`

- **`{socket, protocol}`** - `Value =` `t:protocol/0`

  Only valid to _get_.

  The socket's protocol. Does _not_ work on for instance Darwin.

- **`{socket, rcvbuf}`** - `Value = integer()`

- **`{socket, rcvlowat}`** - `Value = integer()`

- **`{socket, rcvtimeo}`** - `Value =` `t:timeval/0`

  This option is unsupported per default; OTP has to be explicitly built with
  the `--enable-esock-rcvsndtimeo` configure option for this to be available.

  Since our implementation uses non-blocking sockets, it is unknown if and how
  this option works, or even if it may cause malfunction. Therefore, we do not
  recommend setting this option.

  Instead, use the `Timeout` argument to, for instance, the `recv/3` function.

- **`{socket, reuseaddr}`** - `Value = boolean()`

- **`{socket, reuseport}`** - `Value = boolean()`

- **`{socket, sndbuf}`** - `Value = integer()`

- **`{socket, sndlowat}`** - `Value = integer()`

- **`{socket, sndtimeo}`** - `Value =` `t:timeval/0`

  This option is unsupported per default; OTP has to be explicitly built with
  the `--enable-esock-rcvsndtimeo` configure option for this to be available.

  Since our implementation uses non-blocking sockets, it is unknown if and how
  this option works, or even if it may cause malfunction. Therefore, we do not
  recommend setting this option.

  Instead, use the `Timeout` argument to, for instance, the `send/3` function.

- **`{socket, timestamp}`** - `Value = boolean()`

- **`{socket, type}`** - `Value =` `t:type/0`

  Only valid to _get_.

  The socket's type.

_Options for protocol level_ [_`ip`_:](`t:level/0`)

- **`{ip, add_membership}`** - `Value =` `t:ip_mreq/0`

  Only valid to _set_.

- **`{ip, add_source_membership}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

- **`{ip, block_source}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

- **`{ip, drop_membership}`** - `Value =` `t:ip_mreq/0`

  Only valid to _set_.

- **`{ip, drop_source_membership}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

- **`{ip, freebind}`** - `Value = boolean()`

- **`{ip, hdrincl}`** - `Value = boolean()`

- **`{ip, minttl}`** - `Value = integer()`

- **`{ip, msfilter}`** - `Value =` `null |` `t:ip_msfilter/0`

  Only valid to _set_.

  The value `null` passes a `NULL` pointer and size `0` to the C library call.

- **`{ip, mtu}`** - `Value = integer()`

  Only valid to _get_.

- **`{ip, mtu_discover}`** - `Value =`
  [`ip_pmtudisc()` ](`t:ip_pmtudisc/0`)`| integer()`

  An `t:integer/0` value according to the platform's header files.

- **`{ip, multicast_all}`** - `Value = boolean()`

- **`{ip, multicast_if}`** - `Value =` `any |` `t:in_addr/0`

- **`{ip, multicast_loop}`** - `Value = boolean()`

- **`{ip, multicast_ttl}`** - `Value = integer()`

- **`{ip, nodefrag}`** - `Value = boolean()`

- **`{ip, pktinfo}`** - `Value = boolean()`

- **`{ip, recvdstaddr}`** - `Value = boolean()`

- **`{ip, recverr}`** - `Value = boolean()`

  Enable extended reliable error message passing.

  _Warning\!_ When this option is enabled, error messages may arrive on the
  socket's error queue, which should be read using the message flag
  [`errqueue`](`t:msg_flag/0`), and using [`recvmsg/1,2,3,4,5`](`recvmsg/1`) to
  get all error information in the [message's](`t:msg_recv/0`) `ctrl` field as a
  [control message](`t:cmsg_recv/0`) `#{level := ip, type := recverr}`.

  A working strategy should be to first poll the error queue using
  [`recvmsg/2,3,4` ](`m:socket#recvmsg-timeout`)with `Timeout =:= 0` and `Flags`
  containing `errqueue` (ignore the return value `{error, timeout}`) before
  reading the actual data to ensure that the error queue gets cleared. And read
  the data using one of the `nowait |`
  [`select_handle()` ](`t:select_handle/0`)recv functions:
  [`recv/3,4`](`m:socket#recv-nowait`),
  [`recvfrom/3,4`](`m:socket#recvfrom-nowait`) or
  [`recvmsg/3,4,5`](`m:socket#recvmsg-nowait`). Otherwise you might accidentally
  cause a busy loop in and out of 'select' for the socket.

- **`{ip, recvif}`** - `Value = boolean()`

- **`{ip, recvopts}`** - `Value = boolean()`

- **`{ip, recvorigdstaddr}`** - `Value = boolean()`

- **`{ip, recvtos}`** - `Value = boolean()`

- **`{ip, recvttl}`** - `Value = boolean()`

- **`{ip, retopts}`** - `Value = boolean()`

- **`{ip, router_alert}`** - `Value = integer()`

- **`{ip, sendsrcaddr}`** - `Value = boolean()`

- **`{ip, tos}`** - `Value =` [`ip_tos()` ](`t:ip_tos/0`)`| integer()`

  An `t:integer/0` value is according to the platform's header files.

- **`{ip, transparent}`** - `Value = boolean()`

- **`{ip, ttl}`** - `Value = integer()`

- **`{ip, unblock_source}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

_Options for protocol level_ [_`ipv6`_:](`t:level/0`)

- **`{ipv6, addrform}`** - `Value =` `t:domain/0`

  As far as we know the only valid value is `inet` and it is only allowed for an
  IPv6 socket that is connected and bound to an IPv4-mapped IPv6 address.

- **`{ipv6, add_membership}`** - `Value =` `t:ipv6_mreq/0`

  Only valid to _set_.

- **`{ipv6, authhdr}`** - `Value = boolean()`

- **`{ipv6, drop_membership}`** - `Value =` `t:ipv6_mreq/0`

  Only valid to _set_.

- **`{ipv6, dstopts}`** - `Value = boolean()`

- **`{ipv6, flowinfo}`** - `Value = boolean()`

- **`{ipv6, hoplimit}`** - `Value = boolean()`

- **`{ipv6, hopopts}`** - `Value = boolean()`

- **`{ipv6, mtu}`** - `Value = integer()`

- **`{ipv6, mtu_discover}`** - `Value =`
  [`ipv6_pmtudisc()` ](`t:ipv6_pmtudisc/0`)`| integer()`

  An `t:integer/0` value is according to the platform's header files.

- **`{ipv6, multicast_hops}`** - `Value =` `t:ipv6_hops/0`

- **`{ipv6, multicast_if}`** - `Value = integer()`

- **`{ipv6, multicast_loop}`** - `Value = boolean()`

- **`{ipv6, recverr}`** - `Value = boolean()`

  _Warning\!_ See the socket option `{ip, recverr}` regarding the socket's error
  queue. The same warning applies for this option.

- **`{ipv6, recvhoplimit}`** - `Value = boolean()`

- **`{ipv6, recvpktinfo}`** - `Value = boolean()`

- **`{ipv6, recvtclass}`** - `Value = boolean()`

- **`{ipv6, router_alert}`** - `Value = integer()`

- **`{ipv6, rthdr}`** - `Value = boolean()`

- **`{ipv6, tclass}`** - `Value = boolean()`

- **`{ipv6, unicast_hops}`** - `Value =` `t:ipv6_hops/0`

- **`{ipv6, v6only}`** - `Value = boolean()`

_Options for protocol level_ [_`sctp`_](`t:level/0`). See also RFC 6458.

- **`{sctp, associnfo}`** - `Value =` `t:sctp_assocparams/0`

- **`{sctp, autoclose}`** - `Value = integer()`

- **`{sctp, disable_fragments}`** - `Value = boolean()`

- **`{sctp, events}`** - `Value =` `t:sctp_event_subscribe/0`

  Only valid to _set_.

- **`{sctp, initmsg}`** - `Value =` `t:sctp_initmsg/0`

- **`{sctp, maxseg}`** - `Value = integer()`

- **`{sctp, nodelay}`** - `Value = boolean()`

- **`{sctp, rtoinfo}`** - `Value =` `t:sctp_rtoinfo/0`

_Options for protocol level_ [_`tcp`:_](`t:level/0`)

- **`{tcp, congestion}`** - `Value = string()`

- **`{tcp, cork}`** - `Value = boolean()`

- **`{tcp, maxseg}`** - `Value = integer()`

- **`{tcp, nodelay}`** - `Value = boolean()`

_Options for protocol level_ [_`udp`:_](`t:level/0`)

- **`{udp, cork}`** - `Value = boolean()`
""".
-type socket_option() ::
        {Level :: socket,
         Opt ::
           acceptconn |
           acceptfilter |
           bindtodevice |
           broadcast |
           bsp_state |
           busy_poll |
           debug |
           domain |
           dontroute |
           error |
           exclusiveaddruse |
           keepalive |
           linger |
           mark |
           maxdg |
           max_msg_size |
           oobinline |
           passcred |
           peek_off |
           peercred |
           priority |
           protocol |
           rcvbuf |
           rcvbufforce |
           rcvlowat |
           rcvtimeo |
           reuseaddr |
           reuseport |
           rxq_ovfl |
           setfib |
           sndbuf |
           sndbufforce |
           sndlowat |
           sndtimeo |
           timestamp |
           type} |
        {Level :: ip,
         Opt ::
           add_membership |
           add_source_membership |
           block_source |
           dontfrag |
           drop_membership |
           drop_source_membership |
           freebind |
           hdrincl |
           minttl |
           msfilter |
           mtu |
           mtu_discover |
           multicast_all |
           multicast_if |
           multicast_loop |
           multicast_ttl |
           nodefrag |
           options |
           pktinfo |
           recvdstaddr |
           recverr |
           recvif |
           recvopts |
           recvorigdstaddr |
           recvtos |
           recvttl |
           retopts |
           router_alert |
           sndsrcaddr |
           tos |
           transparent |
           ttl |
           unblock_source} |
        {Level :: ipv6,
         Opt ::
           addrform |
           add_membership |
           authhdr |
           auth_level |
           checksum |
           drop_membership |
           dstopts |
           esp_trans_level |
           esp_network_level |
           faith |
           flowinfo |
           hopopts |
           ipcomp_level |
           join_group |
           leave_group |
           mtu |
           mtu_discover |
           multicast_hops |
           multicast_if |
           multicast_loop |
           portrange |
           pktoptions |
           recverr |
           recvhoplimit | hoplimit |
           recvpktinfo | pktinfo |
           recvtclass |
           router_alert |
           rthdr |
           tclass |
           unicast_hops |
           use_min_mtu |
           v6only} |
        {Level :: tcp,
         Opt ::
           congestion |
           cork |
           info |
           keepcnt |
           keepidle |
           keepintvl |
           maxseg |
           md5sig |
           nodelay |
           noopt |
           nopush |
           syncnt |
           user_timeout} |
        {Level :: udp, Opt :: cork} |
        {Level :: sctp,
         Opt ::
           adaption_layer |
           associnfo |
           auth_active_key |
           auth_asconf |
           auth_chunk |
           auth_key |
           auth_delete_key |
           autoclose |
           context |
           default_send_params |
           delayed_ack_time |
           disable_fragments |
           hmac_ident |
           events |
           explicit_eor |
           fragment_interleave |
           get_peer_addr_info |
           initmsg |
           i_want_mapped_v4_addr |
           local_auth_chunks |
           maxseg |
           maxburst |
           nodelay |
           partial_delivery_point |
           peer_addr_params |
           peer_auth_chunks |
           primary_addr |
           reset_streams |
           rtoinfo |
           set_peer_primary_addr |
           status |
           use_ext_recvinfo}.


%% The names of these macros match the names of corresponding
%%C functions in the NIF code, so a search will match both
%%
-define(socket_tag, '$socket').
%%
%% Our socket abstract data type
-define(socket(Ref), {?socket_tag, (Ref)}).
%%
%% Messages sent from the nif-code to erlang processes:
-define(socket_msg(Socket, Tag, Info), {?socket_tag, (Socket), (Tag), (Info)}).

-doc """
A socket, according to this module.

Created and returned by [`open/1,2,3,4`](`open/1`)
and [`accept/1,2`](`accept/1`).
""".
-type socket()          :: ?socket(socket_handle()).

-doc "Opaque socket handle unique for the socket.".
-opaque socket_handle() :: reference().


%% Some flags are used for send, others for recv, and yet again
%% others are found in a cmsg().  They may occur in multiple locations..
-doc """
Platform dependent message flags.

Translates to/from message flag constants on the platform.
These flags are lowercase while the constants are uppercase
with prefix `MSG_`; for example `oob` translates to `MSG_OOB`.

Some flags are only used for sending, some only for receiving, some in received
control messages, and some for several of these. Not all flags are supported on
all platforms. See the platform's documentation,
[`supports(msg_flags)`](`supports/1`), and
[`is_supported(msg_flags, MsgFlag)`](`is_supported/2`).
""".
-type msg_flag() ::
        cmsg_cloexec |
        confirm |
        ctrunc |
        dontroute |
        eor |
        errqueue |
        more |
        oob |
        peek |
        trunc.

-doc "C: `struct msghdr`".
-type msg() :: msg_send() | msg_recv().

-doc """
Message sent by [`sendmsg/2,3,4`](`sendmsg/2`).

Corresponds to a C `struct msghdr`, see your platform documentation for
`sendmsg(2)`.

- **`addr`** - Optional peer address, used on unconnected sockets. Corresponds
  to `msg_name` and `msg_namelen` fields of a `struct msghdr`. If not used they
  are set to `NULL`, `0`.

- **`iov`** - Mandatory data as a list of binaries. The `msg_iov` and
  `msg_iovlen` fields of a `struct msghdr`.

- **`ctrl`** - Optional list of control messages (CMSG). Corresponds to the
  `msg_control` and `msg_controllen` fields of a `struct msghdr`. If not used
  they are set to `NULL`, `0`.

The `msg_flags` field of the `struct msghdr` is set to `0`.
""".
-type msg_send() ::
        #{
           %% *Optional* target address
           %% Used on an unconnected socket to specify the
           %% destination address for a message.
           addr => sockaddr(),

           iov := erlang:iovec(),

           %% *Optional* control message list (ancillary data).
           %% The maximum size of the control buffer is platform
           %% specific. It is the users responsibility to ensure
           %% that its not exceeded.
           %%
           ctrl  =>
               ([cmsg_send() |
                 #{level := level() | integer(),
                   type  := integer(),
                   data  := binary()}])
         }.

-doc """
Message returned by [`recvmsg/1,2,3,5`](`recvmsg/1`).

Corresponds to a C `struct msghdr`, see your platform documentation for
[`recvmsg(2)`](`recvmsg/1`).

- **`addr`** - Optional peer address, used on unconnected sockets. Corresponds
  to `msg_name` and `msg_namelen` fields of a `struct msghdr`. If `NULL` the map
  key is not present.

- **`iov`** - Data as a list of binaries. The `msg_iov` and `msg_iovlen` fields
  of a `struct msghdr`.

- **`ctrl`** - A possibly empty list of control messages (CMSG). Corresponds to
  the `msg_control` and `msg_controllen` fields of a `struct msghdr`.

- **`flags`** - Message flags. Corresponds to the `msg_flags` field of a
  `struct msghdr`. Unknown flags, if any, are returned in one `t:integer/0`,
  last in the containing list.
""".
-type msg_recv() ::
        #{
           %% *Optional* target address
           %% Used on an unconnected socket to return the
           %% source address for a message.
           addr => sockaddr_recv(),

           iov := erlang:iovec(),

           %% Control messages (ancillary data).
           %% The maximum size of the control buffer is platform
           %% specific. It is the users responsibility to ensure
           %% that its not exceeded.
           %%
           ctrl :=
               ([cmsg_recv() |
                 #{level := level() | integer(),
                   type  := integer(),
                   data  := binary()}]),

           %% Received message flags
           flags := [msg_flag() | integer()]
         }.


%% We are able to (completely) decode *some* control message headers.
%% Even if we are able to decode both level and type, we may not be
%% able to decode the data.  The data is always delivered as a binary()
%% and a decoded value is delivered in the 'value' field, if decoding
%% is successful.

-doc "Control messages (ancillary messages).".
-type cmsg() :: cmsg_recv() | cmsg_send().

-doc """
Control messages (ancillary messages) returned by
[`recvmsg/1,2,3,5`](`recvmsg/1`).

A control message has got a `data` field with a native (`binary`) value for the
message data, and may also have a decoded `value` field if this socket library
successfully decoded the data.
""".
-type cmsg_recv() ::
        #{level := socket,  type := timestamp,    data := binary(),
          value => timeval()}                                       |
        #{level := socket,  type := rights,       data := binary()} |
        #{level := socket,  type := credentials,  data := binary()} |
        #{level := ip,      type := tos,          data := binary(),
          value => ip_tos() | integer()}                            |
        #{level := ip,      type := recvtos,      data := binary(),
          value := ip_tos() | integer()}                            |
        #{level := ip,      type := ttl,          data := binary(),
          value => integer()}                                       |
        #{level := ip,      type := recvttl,      data := binary(),
          value := integer()}                                       |
        #{level := ip,      type := pktinfo,      data := binary(),
          value => ip_pktinfo()}                                    |
        #{level := ip,      type := origdstaddr,  data := binary(),
          value => sockaddr_recv()}                                 |
        #{level := ip,      type := recverr,      data := binary(),
          value => extended_err()}                                  |
        #{level := ipv6,    type := hoplimit,     data := binary(),
          value => integer()}                                       |
        #{level := ipv6,    type := pktinfo,      data := binary(),
          value => ipv6_pktinfo()}                                  |
        #{level := ipv6,    type := recverr,      data := binary(),
          value => extended_err()}                                  |
        #{level := ipv6,    type := tclass,       data := binary(),
          value =>        integer()}.

-type native_value() :: integer() | boolean() | binary().
%% Possible to add type tagged values a'la {uint16, 0..16#FFFF}

-doc """
Control messages (ancillary messages) accepted by
[`sendmsg/2,3,4`](`sendmsg/2`).

A control message may for some message types have a `value` field with a
symbolic value, or a `data` field with a native value, that has to be binary
compatible what is defined in the platform's header files.
""".
-type cmsg_send() ::
        #{level := socket,  type := timestamp,    data => native_value(),
          value => timeval()}                                             |
        #{level := socket,  type := rights,       data := native_value()} |
        #{level := socket,  type := credentials,  data := native_value()} |
        #{level := ip,      type := tos,          data => native_value(),
          value => ip_tos() | integer()}                                  |
        #{level := ip,      type := ttl,          data => native_value(),
          value => integer()}                                             |
        #{level := ip,      type := hoplimit,     data => native_value(),
          value => integer()}                                             |
        #{level := ipv6,    type := tclass,       data => native_value(),
          value => integer()}.

-type ee_origin() :: none | local | icmp | icmp6.
-type icmp_dest_unreach() ::
        net_unreach | host_unreach | port_unreach | frag_needed |
        net_unknown | host_unknown.
-type icmpv6_dest_unreach() ::
        noroute | adm_prohibited | not_neighbour | addr_unreach |
        port_unreach | policy_fail | reject_route.
-type extended_err() ::
        #{error    := posix(),
          origin   := icmp,
          type     := dest_unreach,
          code     := icmp_dest_unreach() | 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := icmp,
          type     := time_exceeded | 0..16#FF,
          code     := 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := icmp6,
          type     := dest_unreach,
          code     := icmpv6_dest_unreach() | 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := icmp6,
          type     := pkt_toobig | time_exceeded | 0..16#FF,
          code     := 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := ee_origin() | 0..16#FF,
          type     := 0..16#FF,
          code     := 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()}.

-doc """
Posix error codes.

Local alias for `t:inet:posix/0`, a set of `t:atom/0`s.
""".
-type posix() :: inet:posix().

-doc """
Information element designators for the  `i/1` and `i/2` functions.
""".
-type info_keys() :: [
		      'domain' | 'type' | 'protocol' |
		      'fd' | 'owner' |
		      'local_address' | 'remote_address' |
		      'recv' | 'sent' |
		      'state'
		     ].


%% Note that not all flags exist on all platforms!
-type ioctl_device_flag() :: up | broadcast | debug | loopback | pointopoint |
                             notrailers | knowsepoch | running | noarp | promisc | allmulti |
                             master | oactive | slave | simplex |
			     link0 | link1 | link2 |
			     multicast | portsel | automedia |
			     cantconfig | ppromisc |
                             dynamic |
			     monitor | staticarp | dying | renaming | nogroup |
			     lower_up | dormant | echo.

%% When reading the device map (gifmap), the resulting map will be
%% "fully" populated.
%% <DOES-THIS-WORK>
%% When writing, it is expected that only the fields that is
%% to be set is present.
%% </DOES-THIS-WORK>
-type ioctl_device_map() :: #{mem_start := non_neg_integer(),
                              mem_end   := non_neg_integer(),
                              base_addr := non_neg_integer(),
                              irq       := non_neg_integer(),
                              dma       := non_neg_integer(),
                              port      := non_neg_integer()}.


%% ===========================================================================
%%
%% Interface term formats
%%

-define(ASYNCH_DATA_TAG, (recv | recvfrom | recvmsg |
                          send | sendto | sendmsg | sendfile)).
-define(ASYNCH_TAG,      ((accept | connect) | ?ASYNCH_DATA_TAG)).

%% -type asynch_data_tag() :: send | sendto | sendmsg |
%%                            recv | recvfrom | recvmsg |
%%                            sendfile.
%% -type asynch_tag()      :: connect | accept |
%%                            asynch_data_tag().
%% -type select_tag()      :: asynch_tag() |
%%                            {asynch_data_tag(), ContData :: term()}.
%% -type completion_tag()  :: asynch_tag().
-doc """
[Select operation](#asynchronous-calls) tag.

A tag that describes the (select) operation (= function name),
contained in the returned `t:select_info/0`.
""".
-type select_tag()      :: ?ASYNCH_TAG |
                           {?ASYNCH_DATA_TAG, ContData :: term()}.

-doc(#{since => ~"OTP 26.0"}).
-doc """
[Completion operation](#asynchronous-calls) tag.


A tag that describes the ongoing (completion) operation (= function name),
contained in the returned `t:completion_info/0`.
""".
-type completion_tag()  :: ?ASYNCH_TAG.

-doc """
[Select operation](#asynchronous-calls) handle.

A `t:reference/0` that uniquely identifies the (select) operation,
contained in the returned `t:select_info/0`.
""".
-type select_handle() :: reference().

-doc(#{since => ~"OTP 26.0"}).
-doc """
[Completion operation](#asynchronous-calls) handle.

A `t:reference/0` that uniquely identifies the (completion) operation,
contained in the returned `t:completion_info/0`.
""".
-type completion_handle() :: reference().

-doc """
[Select operation](#asynchronous-calls) info.

Returned by an operation that requires the caller to wait for a
[select message](`m:socket#asynchronous-calls`) containing the
[`SelectHandle`](`t:select_handle/0`).
""".
-type select_info() ::
        {select_info,
         SelectTag :: select_tag(),
         SelectHandle :: select_handle()}.

-doc(#{since => ~"OTP 26.0"}).
-doc """
[Completion operation](#asynchronous-calls) info.

Returned by an operation that requires the caller to wait for a
[completion message](`m:socket#asynchronous-calls`) containing the
[`CompletionHandle`](`t:completion_handle/0`) _and_ the result of the operation;
the `CompletionStatus`.
""".
-type completion_info() ::
        {completion_info,
         CompletionTag :: completion_tag(),
         CompletionHandle :: completion_handle()}.

-define(SELECT_INFO(Tag, SelectHandle),
        {select_info, Tag, SelectHandle}).

-define(COMPLETION_INFO(Tag, CompletionHandle),
        {completion_info, Tag, CompletionHandle}).


%% ===========================================================================
%%
%% Defaults
%%

-define(ESOCK_LISTEN_BACKLOG_DEFAULT, 5).

-define(ESOCK_ACCEPT_TIMEOUT_DEFAULT, infinity).

-define(ESOCK_SEND_FLAGS_DEFAULT,      []).
-define(ESOCK_SEND_TIMEOUT_DEFAULT,    infinity).
-define(ESOCK_SENDTO_FLAGS_DEFAULT,    []).
-define(ESOCK_SENDTO_TIMEOUT_DEFAULT,  ?ESOCK_SEND_TIMEOUT_DEFAULT).
-define(ESOCK_SENDMSG_FLAGS_DEFAULT,   []).
-define(ESOCK_SENDMSG_TIMEOUT_DEFAULT, ?ESOCK_SEND_TIMEOUT_DEFAULT).
-define(ESOCK_SENDV_TIMEOUT_DEFAULT,   ?ESOCK_SEND_TIMEOUT_DEFAULT).

-define(ESOCK_RECV_FLAGS_DEFAULT,   []).
-define(ESOCK_RECV_TIMEOUT_DEFAULT, infinity).


%% ===========================================================================
%%
%% Administrative and utility API
%%
%% ===========================================================================

%% *** number_of ***
%%
%% Interface function to the socket registry
%% returns the number of existing (and "alive") sockets.
%%
-doc(#{since => <<"OTP 22.3">>}).
-doc "Return the number of active sockets.".
-spec number_of() -> non_neg_integer().

number_of() ->
    ?REGISTRY:number_of().


%% *** which_sockets/0,1 ***
%%
%% Interface function to the socket registry
%% Returns a list of all the sockets, according to the filter rule.
%%

-doc(#{since => <<"OTP 22.3">>}).
-doc """
Return a list of all known sockets.

Equivalent to [`which_sockets(fun (_) -> true end)`](`which_sockets/1`).
""".
-spec which_sockets() -> [socket()].

which_sockets() ->
    ?REGISTRY:which_sockets(true).

-doc(#{since => <<"OTP 22.3">>}).
-doc """
Return a filtered list of known sockets.

There are several predefined `FilterRule`s and one general:

- **`inet | inet6`** - Only the sockets with matching `t:domain/0`
  are returned.

- **`stream | dgram | seqpacket`** - Only the sockets with
  matching `t:type/0` are returned.

- **`sctp | tcp | udp`** - Only the sockets with
  matching `t:protocol/0` are returned.

- **`t:pid/0`** - Only the sockets with matching Controlling Process
  are returned. See the OTP socket option
  [`controlling_process`](`t:otp_socket_option/0`).

- **`fun((socket_info()) -> boolean())`** - The general filter rule.
  A fun that takes the socket info and returns a `t:boolean/0`
  indicating if the socket should be returned or not.
""".
-spec which_sockets(FilterRule) -> [socket()] when
	FilterRule ::
          'inet' | 'inet6' | 'local' |
          'stream' | 'dgram' | 'seqpacket' |
          'sctp' | 'tcp' | 'udp' |
          pid() |
          fun((socket_info()) -> boolean()).

which_sockets(Domain)
  when Domain =:= inet;
       Domain =:= inet6;
       Domain =:= local ->
    ?REGISTRY:which_sockets({domain, Domain});

which_sockets(Type)
  when Type =:= stream;
       Type =:= dgram;
       Type =:= seqpacket ->
    ?REGISTRY:which_sockets({type, Type});

which_sockets(Proto)
  when Proto =:= sctp;
       Proto =:= tcp;
       Proto =:= udp ->
    ?REGISTRY:which_sockets({protocol, Proto});

which_sockets(Owner)
  when is_pid(Owner) ->
    ?REGISTRY:which_sockets({owner, Owner});

which_sockets(Filter) when is_function(Filter, 1) ->
    ?REGISTRY:which_sockets(Filter);

which_sockets(Other) ->
    erlang:error(badarg, [Other]).




%% *** number_of_monitors ***
%%
%% Interface function to the socket registry
%% returns the number of existing socket monitors.
%%

-doc false.
-spec number_of_monitors() -> non_neg_integer().

number_of_monitors() ->
    ?REGISTRY:number_of_monitors().

-doc false.
-spec number_of_monitors(pid()) -> non_neg_integer().

number_of_monitors(Pid) when is_pid(Pid) ->
    ?REGISTRY:number_of_monitors(Pid).


%% *** which_monitors/1 ***
%%
%% Interface function to the socket registry
%% Returns a list of all the monitors of the process or socket.
%%

-doc false.
-spec which_monitors(Pid) -> [reference()] when
      Pid :: pid();
                    (Socket) -> [reference()] when
      Socket :: socket().

which_monitors(Pid) when is_pid(Pid) ->
    ?REGISTRY:which_monitors(Pid);
which_monitors(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    ?REGISTRY:which_monitors(Socket);
which_monitors(Socket) ->
    erlang:error(badarg, [Socket]).


%% *** monitor_by/1 ***
%%
%% Interface function to the socket registry
%% Returns a list of all the processes monitoring the socket.
%%

-doc false.
-spec monitored_by(Socket) -> [reference()] when
						Socket :: socket().

monitored_by(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    ?REGISTRY:monitored_by(Socket);
monitored_by(Socket) ->
    erlang:error(badarg, [Socket]).


%% *** to_list/1 ***
%%
%% This is intended to convert a socket() to a printable string.
%%

-doc false.
-spec to_list(Socket) -> list() when
      Socket :: socket().

to_list(?socket(SockRef)) when is_reference(SockRef) ->
    "#Ref" ++ Id = erlang:ref_to_list(SockRef),
    "#Socket" ++ Id;
to_list(Socket) ->
    erlang:error(badarg, [Socket]).


%% *** which_socket_kind/1 ***
%%
%% Utility function that returns the "kind" of socket.
%% That is, if its a "plain" socket or a compatibility socket.
%%

-doc false.
-spec which_socket_kind(Socket :: socket()) -> plain | compat.

which_socket_kind(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    case prim_socket:getopt(SockRef, {otp,meta}) of
	{ok, undefined} ->
	    plain;
	{ok, _} ->
	    compat;
	{error, _} ->
	    erlang:error(badarg, [Socket])
    end;
which_socket_kind(Socket) ->
    erlang:error(badarg, [Socket]).



%% ===========================================================================
%%
%% Debug features
%%
%% ===========================================================================


-doc false.
-spec debug(D :: boolean()) -> 'ok'.
%%
debug(D) when is_boolean(D) ->
    prim_socket:debug(D);
debug(D) ->
    erlang:error(badarg, [D]).


-doc false.
-spec socket_debug(D :: boolean()) -> 'ok'.
%%
socket_debug(D) when is_boolean(D) ->
    prim_socket:socket_debug(D);
socket_debug(D) ->
    erlang:error(badarg, [D]).



-doc(#{since => <<"OTP 23.1">>}).
-doc """
Set the global [`use_registry`](`t:otp_socket_option/0`)
option default value.

Globally change if the socket registry is to be used or not.
Note that its still possible to override this explicitly
when creating an individual sockets, see [`open/2,3,4`](`open/2`)
for more info (the [`Opts :: map/0`](`t:map/0`)).
""".
-spec use_registry(D :: boolean()) -> 'ok'.
%%
use_registry(D) when is_boolean(D) ->
    prim_socket:use_registry(D).


-doc false.
tables() ->
    #{protocols      => table(protocols),
      options        => table(options),
      ioctl_requests => table(ioctl_requests),
      ioctl_flags    => table(ioctl_flags),
      msg_flags      => table(msg_flags)}.

-doc false.
table(Table) ->
    prim_socket:p_get(Table).


%% ===========================================================================
%%
%% i/0,1,2 - List sockets
%%
%% This produces a list of "all" the sockets, and some info about each one.
%% This function is intended as a utility and debug function.
%% The sockets can be selected from domain, type or protocol.
%% The sockets are not sorted.
%%
%% ===========================================================================

-spec default_info_keys() -> info_keys().

default_info_keys() ->
    [
     domain, type, protocol, fd, owner,
     local_address, remote_address,
     recv, sent,
     state
    ].

-doc(#{since => <<"OTP 24.1">>}).
-doc """
Print information to the erlang shell in table format
for all sockets.

The information printed for each socket is specified by the default set
of `t:info_keys/0` (all keys).

The sockets that are printed are all sockets created by this
`socket` module's implementation.
""".
-spec i() -> ok.

i() ->
    do_i(which_sockets(), default_info_keys()).

-doc(#{since => <<"OTP 24.1">>}).
-doc """
Print information to the erlang shell in table format
for all sockets.

If the argument is a list of `t:info_keys/0`, print the specified
information for all sockets.  See `i/0`.

Otherwise the same as `i/2` with the same first argument
and the default information (see `i/0`).
""".
-spec i(InfoKeys :: info_keys()) -> ok;
       (Domain :: inet | inet6 | local) -> ok;
       (Proto :: sctp | tcp | udp) -> ok;
       (Type :: dgram | seqpacket | stream) -> ok.

i(InfoKeys) when is_list(InfoKeys) ->
    do_i(which_sockets(), InfoKeys);
i(Domain) when (Domain =:= inet) orelse
	       (Domain =:= inet6) orelse
	       (Domain =:= local) ->
    do_i(which_sockets(Domain), default_info_keys());
i(Proto) when (Proto =:= tcp) orelse
	      (Proto =:= udp) orelse
	      (Proto =:= sctp) ->
    do_i(which_sockets(Proto), default_info_keys());
i(Type) when (Type =:= dgram) orelse
	     (Type =:= seqpacket) orelse
	     (Type =:= stream) ->
    do_i(which_sockets(Type), default_info_keys()).

-doc(#{since => <<"OTP 24.1">>}).
-doc """
Print information to the erlang shell in table format
for a selection of sockets.

The argument [`InfoKeys`](`t:info_keys/0`) specifies which information
is printed for each socket.

If the first argument is `Domain` print information for
all sockets of that specific `t:domain/0`.

If the first argument is `Proto` print information for
all sockets of that specific `t:protocol/0`.

If the first argument is `Type` print information for
all sockets of that specific `t:type/0`.
""".
-spec i(Domain :: inet | inet6 | local, InfoKeys) -> ok
              when InfoKeys :: info_keys();
       (Proto :: sctp | tcp | udp, InfoKeys) -> ok
              when InfoKeys :: info_keys();
       (Type :: dgram | seqpacket | stream, InfoKeys) -> ok
              when InfoKeys :: info_keys().

i(Domain, InfoKeys)
  when ((Domain =:= inet) orelse
	(Domain =:= inet6) orelse
	(Domain =:= local)) andalso
       is_list(InfoKeys) ->
    do_i(which_sockets(Domain), InfoKeys);
i(Proto, InfoKeys)
  when ((Proto =:= tcp) orelse
	(Proto =:= udp) orelse
	(Proto =:= sctp)) andalso
       is_list(InfoKeys) ->
    do_i(which_sockets(Proto), InfoKeys);
i(Type, InfoKeys)
  when ((Type =:= dgram) orelse
	(Type =:= seqpacket) orelse
	(Type =:= stream)) andalso
       is_list(InfoKeys) ->
    do_i(which_sockets(Type), InfoKeys).

do_i(Sockets, InfoKeys) ->
    Lines = case i_sockets(Sockets, InfoKeys) of
		[] -> [];
		InfoLines -> [header_line(InfoKeys) | InfoLines]
	    end,
    Maxs = lists:foldl(fun(Line, Max0) -> smax(Max0, Line) end,
		       lists:duplicate(length(InfoKeys), 0), Lines),
    Fmt = lists:append(["~-" ++ integer_to_list(N) ++ "s " ||
			   N <- Maxs]) ++ "~n",
    lists:foreach(fun(Line) -> io:format(Fmt, Line) end, Lines).

header_line(Fields) ->
    [header_field(atom_to_list(F)) || F <- Fields].
header_field([C | Cs]) ->
    [string:to_upper(C) | header_field_rest(Cs)].
header_field_rest([$_, C | Cs]) ->
    [$\s, string:to_upper(C) | header_field_rest(Cs)];
header_field_rest([C|Cs]) ->
    [C | header_field_rest(Cs)];
header_field_rest([]) ->
    [].

smax([Max|Ms], [Str|Strs]) ->
    N = length(Str),
    [if N > Max -> N; true -> Max end | smax(Ms, Strs)];
smax([], []) ->
    [].

i_sockets(Sockets, InfoKeys) ->
    [i_socket(Socket, InfoKeys) || Socket <- Sockets].

i_socket(Socket, InfoKeys) ->
    %% Most of the stuff we need, is in 'socket info'
    %% so we can just as well get it now.
    Info = #{protocol := Proto} = info(Socket),
    i_socket(Proto, Socket, Info, InfoKeys).

i_socket(Proto, Socket, Info, InfoKeys) ->
    [i_socket_info(Proto, Socket, Info, InfoKey) || InfoKey <- InfoKeys].

i_socket_info(_Proto, _Socket, #{domain := Domain} = _Info, domain) ->
    atom_to_list(Domain);
i_socket_info(_Proto, _Socket, #{type := Type} = _Info, type) ->
    string:to_upper(atom_to_list(Type));
i_socket_info(Proto, _Socket, #{type := Type} = _Info, protocol) ->
    string:to_upper(atom_to_list(if
                                     (Proto =:= 0) ->
                                         case Type of
                                             stream -> tcp;
                                             dgram  -> udp;
                                             _      -> unknown
                                         end;
                                     true ->
                                         Proto
                                 end));
i_socket_info(_Proto, Socket, _Info, fd) ->
    try socket:getopt(Socket, otp, fd) of
	{ok,   FD} -> integer_to_list(FD);
	{error, _} -> " "
    catch
        _:_ -> " "
    end;
i_socket_info(_Proto, _Socket, #{owner := Pid} = _Info, owner) ->
    pid_to_list(Pid);
i_socket_info(Proto, Socket, _Info, local_address) ->
    case sockname(Socket) of
	{ok,  Addr} ->
	    fmt_sockaddr(Addr, Proto);
	{error, _} ->
	    " "
    end;
i_socket_info(Proto, Socket, _Info, remote_address) ->
    try peername(Socket) of
	{ok,  Addr} ->
	    fmt_sockaddr(Addr, Proto);
	{error, _} ->
	    " "
    catch
        _:_ ->
            " "
    end;
i_socket_info(_Proto, _Socket,
	      #{counters := #{read_byte := N}} = _Info, recv) ->
    integer_to_list(N);
i_socket_info(_Proto, _Socket,
	      #{counters := #{write_byte := N}} = _Info, sent) ->
    integer_to_list(N);
i_socket_info(_Proto, _Socket, #{rstates := RStates,
				 wstates := WStates} = _Info, state) ->
    fmt_states(RStates, WStates);
i_socket_info(_Proto, _Socket, _Info, _Key) ->
    " ".

fmt_states([], []) ->
    " ";
fmt_states(RStates, []) ->
    fmt_states(RStates) ++ ", -";
fmt_states([], WStates) ->
    " - , " ++ fmt_states(WStates);
fmt_states(RStates, WStates) ->
    fmt_states(RStates) ++ " , " ++ fmt_states(WStates).

fmt_states([H]) ->
    fmt_state(H);
fmt_states([H|T]) ->
    fmt_state(H) ++ ":"  ++ fmt_states(T).

fmt_state(accepting) ->
    "A";
fmt_state(bound) ->
    "BD";
fmt_state(busy) ->
    "BY";
fmt_state(connected) ->
    "CD";
fmt_state(connecting) ->
    "CG";
fmt_state(listen) ->
    "LN";
fmt_state(listening) ->
    "LG";
fmt_state(open) ->
    "O";
fmt_state(selected) ->
    "SD";
fmt_state(X) when is_atom(X) ->
    string:uppercase(atom_to_list(X)).


fmt_sockaddr(#{family := Fam,
	       addr   := Addr} = SockAddr, _Proto)
  when (Fam =:= inet) orelse (Fam =:= inet6) ->
    case Addr of
	{0,0,0,0}         -> "*:" ++ fmt_service(SockAddr);
	{0,0,0,0,0,0,0,0} -> "*:" ++ fmt_service(SockAddr);
	{127,0,0,1}       -> "localhost:" ++ fmt_service(SockAddr);
	{0,0,0,0,0,0,0,1} -> "localhost:" ++ fmt_service(SockAddr);
	IP                -> inet_parse:ntoa(IP) ++ ":" ++ fmt_service(SockAddr)
    end;
fmt_sockaddr(#{family := local,
	       path   := Path}, _Proto) ->
    "local:" ++
	if is_list(Path) ->
		Path;
	   is_binary(Path) ->
		binary_to_list(Path)
	end.

fmt_service(#{port := Port} = SockAddr) ->
    case net:getnameinfo(SockAddr) of
	{ok, #{service := Service}} ->
            %% Even if there is no actual service associated with 
            %% this port number, we still get a value: The port number
            try list_to_integer(Service) of
                FOO when is_integer(FOO) -> % This should be equal to Port...
                    integer_to_list(Port)
            catch
                _C:_E:_S -> %% This means that it's an actual service
                    f("~s (~w)", [Service, Port])
            end;
	_ ->
            integer_to_list(Port)
    end.


%% ===========================================================================
%%
%% info - Get miscellaneous information about a socket
%% or about the socket library.
%%
%% Generates a list of various info about the socket, such as counter values.
%%
%% Do *not* call this function often.
%%
%% ===========================================================================

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Get miscellaneous information about this `socket` library.

The function returns a map with each information item as a key-value pair.

> #### Note {: .info }
>
> In order to ensure data integrity, mutexes are taken when needed.
> So, don't call this function often.
""".
-spec info() -> info().
%%
info() ->
    try
        prim_socket:info()
    catch error:undef:ST ->
            case ST of
                %% We rewrite errors coming from prim_socket not existing
                %% to enotsup.
                [{prim_socket,info,[],_}|_] ->
                    erlang:raise(error,notsup,ST);
                _ ->
                    erlang:raise(error,undef,ST)
            end
    end.

-doc(#{since => <<"OTP 22.1">>}).
-doc """
Get miscellaneous info about a socket.

The function returns a map with each information item as a key-value pair
reflecting the "current" state of the socket.

> #### Note {: .info }
>
> In order to ensure data integrity, mutexes are taken when needed.
> So, don't call this function often.
""".
-spec info(Socket) -> socket_info() when
					Socket :: socket().
%%
info(?socket(SockRef)) when is_reference(SockRef) ->
    prim_socket:info(SockRef);
info(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% monitor - Monitor a socket
%%
%% If a socket "dies", a down message, similar to erlang:monitor, will be
%% sent to the requesting process:
%%
%%       {'DOWN', MonitorRef, socket, Socket, Info}
%%
%% ===========================================================================

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Start a socket monitor.

If the `Socket` doesn't exist or when later the monitor is triggered,
a `'DOWN'` message is sent to the process that called `monitor/1`
with the following pattern:

``` erlang
	    {'DOWN', MonitorRef, socket, Socket, Info}
```
`Info` is the termination reason of the socket or `nosock` if
`Socket` did not exist when the monitor was started.

Making several calls to `socket:monitor/1` for the same `Socket` is not an
error; each call creates an independent monitor instance.
""".
-spec monitor(Socket :: socket()) -> MonitorRef :: reference().

monitor(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    case prim_socket:setopt(SockRef, {otp, use_registry}, true) of
        ok ->
            socket_registry:monitor(Socket);
        {error, closed = SReason} ->
            MonitorRef = make_ref(),
            self() ! {'DOWN', MonitorRef, socket, Socket, SReason},
	    MonitorRef
    end;
monitor(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% cancel_monitor - Cancel a socket monitor
%%
%% If MRef is a reference that the socket obtained
%% by calling monitor/1, this monitoring is turned off.
%% If the monitoring is already turned off, nothing happens.
%%
%% ===========================================================================

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Cancel a socket monitor.

If `MRef` is a reference that the calling process obtained by calling
`monitor/1`, this monitor is removed. If there is no such monitor
for the calling process (or MRef doesn't correspond to a monitor),
nothing happens.

The returned value is one of the following:

- **`true`** - The monitor was found and removed. In this case, no `'DOWN'`
  message corresponding to this monitor has been delivered and will not be
  delivered.

- **`false`** - The monitor was not found so it couldn't be removed. This
  might be because the monitor has already triggered and there is
  a `'DOWN'` message from this monitor in the caller message queue.
""".
-spec cancel_monitor(MRef :: reference()) -> boolean().

cancel_monitor(MRef) when is_reference(MRef) ->
    case socket_registry:cancel_monitor(MRef) of
	ok ->
	    true;
	{error, Reason} ->
            case Reason of
                unknown_monitor -> false;
                not_owner       -> false;
                _ ->
                    erlang:error({invalid, Reason})
            end
    end;
cancel_monitor(MRef) ->
    erlang:error(badarg, [MRef]).


%% ===========================================================================
%%
%% supports - get information about what the platform "supports".
%%
%% Generates a list of various info about what the platform can support.
%% The most obvious case is 'options'.
%%
%% Each item in a 'supports'-list will appear only *one* time.
%%
%% ===========================================================================

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Retrieve information about what socket features
the module and the platform supports.

Returns a list of, in no particular order,
`{Key1, `[`supports(Key1)`](`supports/1`)`}` tuples
for every `Key1` described in `supports/1`,
and `{Key, boolean()}` tuples for each of the following keys:

- **`sctp`** - SCTP support

- **`ipv6`** - IPv6 support

- **`local`** - Unix Domain sockets support (`AF_UNIX | AF_LOCAL`)

- **`netns`** - Network Namespaces support (Linux, `setns(2)`)

- **`sendfile`** - Sendfile support (`sendfile(2)`)
""".
-spec supports() -> [{Key1 :: term(),
                      boolean() | [{Key2 :: term(),
                                    boolean() | [{Key3 :: term(),
                                                  boolean()}]}]}].
supports() ->
    [{Key1, supports(Key1)}
     || Key1 <- [ioctl_requests, ioctl_flags,
                 options, msg_flags, protocols]]
        ++ prim_socket:supports().

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Retrieve information about what socket features
the module and the platform supports.

If `Key1 = msg_flags` returns a list of `{Flag, boolean()}`
tuples for every `Flag` in `t:msg_flag/0` with the `t:boolean/0`
indicating if the flag is supported on this platform.

If `Key1 = protocols` returns a list of `{Name, boolean()}`
tuples for every `Name` in`t:protocol/0` with the `t:boolean/0`
indicating if the protocol is supported on this platform.

If `Key1 = options` returns a list of `{SocketOption, boolean()}`
tuples for every `SocketOption` in `t:socket_option/0` with the `t:boolean/0`
indicating if the socket option is supported on this platform.

There is no particular order of any of the returned lists.

For other values of `Key1` returns `[]`.
Note that in future versions of this module or on different platforms,
there might be more supported keys.
""".
-spec supports(Key1 :: term()) ->
                      [{Key2 :: term(),
                        boolean() | [{Key3 :: term(),
                                      boolean()}]}].
%%
supports(Key) ->
    prim_socket:supports(Key).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Retrieve information about what socket features
the module and the platform supports.

If `Key1 = options`, for a `Key2` in `t:level/0` returns
a list of `{Opt, boolean()}` tuples for all known socket options
[`Opt` on that `Level = Key2`](`t:socket_option/0`) with the `t:boolean/0`
indicating if the socket option is supported on this platform.
See `setopt/3` and `getopt/2`.

There is no particular order of any of the returned lists.

For other values of `Key1` or `Key2` returns `[]`.
Note that in future versions of this module or on different platforms,
there might be more supported keys.
""".
-spec supports(Key1 :: term(), Key2 :: term()) ->
                      [{Key3 :: term(),
                        boolean()}].
%%
supports(Key1, Key2) ->
    prim_socket:supports(Key1, Key2).


-doc(#{since => <<"OTP 23.0">>}).
-doc """
Check if a socket feature is supported.

Returns `true` if `supports/0` has a `{Key1, true}` tuple
or a `{Key1, list()}` tuple in its returned list,
otherwise `false` (also for unknown keys).

Example:
``` erlang
true = socket:is_supported(local),
```
""".
-spec is_supported(Key1 :: term()) ->
                          boolean().
is_supported(Key1) ->
    prim_socket:is_supported(Key1).

-doc(#{since => <<"OTP 23.0">>}).
-doc """
Check if a socket feature is supported.

Returns `true` if [`supports(Key1)`](`supports/1`) has a `{Key2, true}` tuple
in its returned list, otherwise `false` (also for unknown keys).

Example:
``` erlang
true = socket:is_supported(msg_flags, errqueue),
```
""".
-spec is_supported(Key1 :: term(), Key2 :: term()) ->
                          boolean().
is_supported(Key1, Key2) ->
    prim_socket:is_supported(Key1, Key2).

%% Undocumented legacy functions
%%
-doc false.
is_supported(options, Level, Opt) when is_atom(Level), is_atom(Opt) ->
    is_supported(options, {Level,Opt}).

-doc false.
options() ->
    lists:sort(supports(options)).

-doc false.
options(Level) ->
    [{Opt, Supported} || {{Lvl, Opt}, Supported} <- options(), (Lvl =:= Level)].

-doc false.
options(Level, Supported) ->
    [Opt || {Opt, Sup} <- options(Level), (Sup =:= Supported)].

-doc false.
option({Level, Opt}) ->
    lists:member(Opt, options(Level, true)).
-doc false.
option(Level, Opt) ->
    option({Level, Opt}).

-doc false.
protocols() ->
    lists:sort(supports(protocols)).

-doc false.
protocol(Proto) ->
    case lists:keysearch(Proto, 1, protocols()) of
        {value, {Proto, Supported}} ->
            Supported;
        false ->
            false
    end.



%% ===========================================================================
%%
%% The proper socket API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% <KOLLA>
%%
%% The NIF sets up a monitor to this process, and if it dies the socket
%% is closed. It is also used if someone wants to monitor the socket.
%%
%% We may therefore need monitor function(s):
%%
%%               socket:monitor(Socket)
%%               socket:demonitor(Socket)
%%
%% </KOLLA>
%%

%% ===========================================================================
%%
%% open - create an endpoint for communication
%%

-doc(#{since => <<"OTP 23.0">>}).
-doc "Equivalent to [`open(FD, #{})`](`open/2`).".
-spec open(FD :: integer()) -> _.

open(FD) when is_integer(FD) ->
    open(FD, #{});
open(FD) ->
    erlang:error(badarg, [FD]).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Create a socket.

#### With arguments `Domain` and `Type`

Equivalent to [`open(Domain, Type, default, #{})`](`open/4`).

#### With arguments `FD` and `Opts` *(since OTP 23.0)*

Creates an endpoint for communication (socket) based on
an already existing file descriptor that must be a socket.
This function attempts to retrieve the file descriptor's
`domain`, `type` and `protocol` from the system.
This is however not possible on all platforms;
in that case they should be specified in `Opts`.

The `Opts` argument can provide extra information:

- **`domain`** - The file descriptor's communication domain. See also

  [`open/2,3,4`](`open/3`).

- **`type`** - The file descriptor's socket type.

  See also [`open/2,3,4`](`open/3`).

- **`protocol`** - The file descriptor's protocol. The atom `default` is
  equivalent to the integer protocol number `0` which means the default
  protocol for a given domain and type.

  If the protocol can not be retrieved from the platform for the socket, and
  `protocol` is not specified, the default protocol is used, which may
  or may not be correct.

  See also [`open/2,3,4`](`open/3`).

- **`dup`** - If `false` don't duplicate the provided file descriptor.

  Defaults to `true`; do duplicate the file descriptor.

- **`debug`** - If `true` enable socket debug logging.

  Defaults to `false`; don't enable socket debug logging.

- **`use_registry`** - Enable or disable use of the socket registry
  for this socket. This overrides the global setting.

  Defaults to the global setting, see `use_registry/1`.

> #### Note {: .info }
>
> This function should be used with care\!
>
> On some platforms it is _necessary_ to provide `domain`, `type` and `protocol`
> since they cannot be retrieved from the platform.
>
> On some platforms it is not easy to get hold of a file descriptor
> to use in this function.
""".
-spec open(FD, Opts) -> {'ok', Socket} | {'error', Reason} when
      FD       :: integer(),
      Opts     ::
        #{'domain'       => domain() | integer(),
          'type'         => type() | integer(),
          'protocol'     => 'default' | protocol() | integer(),
          'dup'          => boolean(),
	  'debug'        => boolean(),
	  'use_registry' => boolean()},
      Socket   :: socket(),
      Reason   ::
        posix() | 'domain' | 'type' | 'protocol';

          (Domain :: term(), Type :: term()) -> _.

open(FD, Opts) when is_map(Opts) ->
    if
        is_integer(FD) ->
            case prim_socket:open(FD, Opts) of
                {ok, SockRef} ->
                    Socket = ?socket(SockRef),
                    {ok, Socket};
                {error, _} = ERROR ->
                    ERROR
            end;
        true ->
            erlang:error(badarg, [FD, Opts])
    end;
open(Domain, Type) ->
    open(Domain, Type, 0, #{}).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Create a socket.

#### With arguments `Domain`, `Type` and `Protocol`

Equivalent to [`open(Domain, Type, Protocol, #{})`](`open/4`).

#### With arguments `Domain`, `Type` and `Opts` *(since OTP 24.0)*

Equivalent to [`open(Domain, Type, default, #{})`](`open/4`).
""".
-spec open(Domain :: term(), Type :: term(), Opts :: map()) -> _;
          (Domain :: term(), Type :: term(), Protocol :: term()) -> _.

open(Domain, Type, Opts) when is_map(Opts) ->
    open(Domain, Type, 0, Opts);
open(Domain, Type, Protocol) ->
    open(Domain, Type, Protocol, #{}).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Create a socket.

Creates an endpoint for communication (socket).

`Domain` and `Type` may be `t:integer/0`s, as defined in the platform's
header files. The same goes for `Protocol` as defined in the platform's
`services(5)` database. See also the OS man page for the library call
`socket(2)`.

> #### Note {: .info }
>
> For some combinations of `Domain` and `Type` the platform has got
> a default protocol that can be selected with `Protocol = default`,
> and the platform may allow or require selecting the default protocol,
> or a specific protocol.
>
> Examples:
>
> - **`socket:open(inet, stream, tcp)`** - It is common that for
>   protocol domain and type `inet,stream` it is allowed to select
>   the `tcp` protocol although that mostly is the default.
> - **`socket:open(local, dgram)`** - It is common that for
>   the protocol domain `local` it is mandatory to not select a protocol,
>   that is; to select the default protocol.

The `Opts` argument is intended for "other" options.
The supported option(s) are described below:

- **`netns: string()`** - Used to set the network namespace during the open
  call. Only supported on Linux.

- **`debug: boolean()`** - Enable or disable debug logging.

  Defaults to `false`.

- **`use_registry: boolean()`** - Enable or disable use of the socket registry
  for this socket. This overrides the global value.

  Defaults to the global value, see `use_registry/1`.
""".
-spec open(Domain, Type, Protocol, Opts) ->
                  {'ok', Socket} | {'error', Reason} when
      Domain   :: domain() | integer(),
      Type     :: type() | integer(),
      Protocol :: 'default' | protocol() | integer(),
      Opts     ::
        #{'netns'        => string(),
	  'debug'        => boolean(),
	  'use_registry' => boolean()},
      Socket   :: socket(),
      Reason   :: posix() | 'protocol'.

open(Domain, Type, Protocol, Opts) when is_map(Opts) ->
    case prim_socket:open(Domain, Type, Protocol, Opts) of
        {ok, SockRef} ->
            Socket = ?socket(SockRef),
            {ok, Socket};
        {error, _} = ERROR ->
            ERROR
    end;
open(Domain, Type, Protocol, Opts) ->
    erlang:error(badarg, [Domain, Type, Protocol, Opts]).


%% ===========================================================================
%%
%% bind - bind a name (an address) to a socket
%%
%% Note that the short (atom) addresses only work for some domains,
%% and that the NIF will reject 'broadcast' for other domains than 'inet'
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Bind a name to a socket.

When a socket is created (with [`open`](`open/2`)), it has no address assigned
to it. `bind` assigns the address specified by the `Addr` argument.

The rules used for name binding vary between domains.

If you bind a socket to an address in for example the `inet` or `inet6`
address families, with an ephemeral port number (`0`), and want to know
which port that was chosen, you can find out using something like:
`{ok, #{port := Port}} =`[`socket:sockname(Socket)`](`sockname/1`)
""".
-spec bind(Socket, Addr) -> 'ok' | {'error', Reason} when
      Socket    :: socket(),
      Addr      :: sockaddr() | 'any' | 'broadcast' | 'loopback',
      Reason    :: posix() | 'closed' | invalid().

bind(?socket(SockRef), Addr) when is_reference(SockRef) ->
    if
        Addr =:= any;
        Addr =:= broadcast;
        Addr =:= loopback ->
            case prim_socket:getopt(SockRef, {otp, domain}) of
                {ok, Domain}
                  when Domain =:= inet;
                       Domain =:= inet6 ->
                    prim_socket:bind(
                      SockRef, #{family => Domain, addr => Addr});
                {ok, _Domain} ->
                    {error, eafnosupport};
                {error, _} = ERROR ->
                    ERROR
            end;
        is_atom(Addr) ->
            {error, {invalid, {sockaddr, Addr}}};
        true ->
            prim_socket:bind(SockRef, Addr)
    end;
bind(Socket, Addr) ->
    erlang:error(badarg, [Socket, Addr]).


%% ===========================================================================
%%
%% bind - Add or remove a bind addresses on a socket
%%
%% Calling this function is only valid if the socket is:
%%   type     = seqpacket
%%   protocol = sctp
%%
%% If the domain is inet, then all addresses *must* be IPv4.
%% If the domain is inet6, the addresses can be either IPv4 or IPv6.
%%

-doc false.
-spec bind(Socket, Addrs, Action) -> 'ok' | {'error', Reason} when
      Socket :: socket(),
      Addrs  :: [sockaddr()],
      Action :: 'add' | 'remove',
      Reason :: posix() | 'closed'.

bind(?socket(SockRef), Addrs, Action)
  when is_reference(SockRef)
       andalso is_list(Addrs)
       andalso (Action =:= add
                orelse Action =:= remove) ->
    prim_socket:bind(SockRef, Addrs, Action);
bind(Socket, Addrs, Action) ->
    erlang:error(badarg, [Socket, Addrs, Action]).


%% ===========================================================================
%%
%% connect - initiate a connection on a socket
%%

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Finalize a [`connect/3`](#connect-nowait) operation.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

On `select` systems this function finalizes a connection setup
on a socket, after receiving a `select` message
`{'$socket',` [`Socket`](`t:socket/0`)`, select,
`[`SelectHandle`](`t:select_handle/0`)`}`,
and returns whether the connection setup was successful or not.

Instead of calling this function, for backwards compatibility,
it is allowed to call [`connect/2,3`](`connect/2`) again,
but that incurs more overhead since the connect address and
time-out argument are processed in vain.

The call that completes the connect operation, the second call,
cannot return a `select` return value.
""".
-spec connect(Socket :: socket()) -> 'ok' | {'error', Reason} when
      Reason   :: posix() | 'closed' | invalid().

%% Finalize connect after connect(,, nowait | select_handle())
%% and received select message - see connect_deadline/3 as an example
%%
connect(?socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:connect(SockRef);
connect(Socket) ->
    erlang:error(badarg, [Socket]).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Equivalent to
[`connect(Socket, SockAddr, infinity)`](#connect-infinity).
""".
-spec connect(Socket :: term(), SockAddr :: term()) -> _.

connect(Socket, SockAddr) ->
    connect(Socket, SockAddr, infinity).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Connect the socket to the given address.

This function connects the socket to the address specified
by the `SockAddr` argument.

If a connection attempt is already in progress (by another process),
`{error, already}` is returned.

> #### Note {: .info }
>
> On _Windows_ the socket has to be [_bound_](`bind/2`).

[](){: #connect-infinity }

If the time-out argument (argument 3) is `infinity` it is
up to the OS implementation to decide when the connection
attempt failed and then what to return; probably `{error, etimedout}`.
The OS time-out may be very long.

[](){: #connect-timeout }

If the time-out argument (argument 3) is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if the connection hasn't been established within `Timeout` milliseconds.

> #### Note {: .info }
>
> Note that when this call has returned `{error, timeout}`
> the connection state of the socket is uncertain since the platform's
> network stack may complete the connection at any time,
> up to some platform specific time-out.
>
> Repeating a connection attempt towards the same address would be ok, but
> towards a different address could end up with a connection to either address.
>
> The safe play is to close the socket and start over.
>
> Also note that this applies to canceling a `nowait` connect call
> described below.

[](){: #connect-nowait }

If the time-out argument (argument 2) is `nowait` *(since OTP 22.1)*,
start an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the time-out argument (argument 2) is a `Handle ::` `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`Handle ::` `t:completion_handle/0` *(since OTP 26.0)*,
start an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

After receiving a `select` message call `connect/1`
to complete the operation.

If canceling the operation with `cancel/2` see the note above
about [connection time-out](#connect-timeout).
""".
-spec connect(Socket, SockAddr, Timeout :: 'infinity') ->
          'ok' |
          {'error', Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: posix() | 'closed' | invalid() | 'already' |
                  'not_bound' |
                  {add_socket,             posix()} |
                  {update_connect_context, posix()};

             (Socket, SockAddr, Timeout :: non_neg_integer()) ->
          'ok' |
          {'error', Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: posix() | 'closed' | invalid() | 'already' |
                  'not_bound' | 'timeout' |
                  {add_socket,             posix()} |
                  {update_connect_context, posix()};

             (Socket, SockAddr, 'nowait' | Handle) ->
                     'ok' |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      SockAddr       :: sockaddr(),
      Handle         :: select_handle() | completion_handle(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() | 'already' |
                        'not_bound' |
                        {add_socket,             posix()} |
                        {update_connect_context, posix()}.

%% <KOLLA>
%% Is it possible to connect with family = local for the (dest) sockaddr?
%% </KOLLA>
connect(?socket(SockRef), SockAddr, TimeoutOrHandle)
  when is_reference(SockRef) ->
    case deadline(TimeoutOrHandle) of
        invalid ->
            erlang:error({invalid, {timeout, TimeoutOrHandle}});
        nowait ->
            Handle = make_ref(),
            connect_nowait(SockRef, SockAddr, Handle);
        handle ->
            Handle = TimeoutOrHandle,
            connect_nowait(SockRef, SockAddr, Handle);
        Deadline ->
            connect_deadline(SockRef, SockAddr, Deadline)
    end;
connect(Socket, SockAddr, Timeout) ->
    erlang:error(badarg, [Socket, SockAddr, Timeout]).

connect_nowait(SockRef, SockAddr, Handle) ->
    case prim_socket:connect(SockRef, Handle, SockAddr) of
        select ->
            {select, ?SELECT_INFO(connect, Handle)};
        completion ->
            {completion, ?COMPLETION_INFO(connect, Handle)};
        Result ->
            Result
    end.

connect_deadline(SockRef, SockAddr, Deadline) ->
    Ref = make_ref(),
    case prim_socket:connect(SockRef, Ref, SockAddr) of
        select ->
            %% Connecting...
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, Ref) ->
                    prim_socket:connect(SockRef);
                ?socket_msg(_Socket, abort, {Ref, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, connect, Ref),
                    {error, timeout}
            end;
        completion ->
            %% Connecting...
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, completion, {Ref, CompletionStatus}) ->
                    CompletionStatus;
                ?socket_msg(_Socket, abort, {Ref, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, connect, Ref),
                    {error, timeout}
            end;
        Result ->
            Result
    end.


%% ===========================================================================
%%
%% listen - listen for connections on a socket
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Make a socket listen for connections.

Equivalent to [`listen(Socket, Backlog)`](`listen/2`) with a default
value for `Backlog` (currently `5`).
""".
-spec listen(Socket :: term()) -> _.

listen(Socket) ->
    listen(Socket, ?ESOCK_LISTEN_BACKLOG_DEFAULT).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Make a socket listen for connections.

The `Backlog` argument states the length of the queue for
incoming not yet accepted connections.
Exactly how that number is interpreted is up to the OS'
protocol stack, but the resulting effective queue length
will most probably be perceived as at least that long.

> #### Note {: .info }
>
> On _Windows_ the socket has to be _bound_.
""".
-spec listen(Socket, Backlog) -> 'ok' | {'error', Reason} when
      Socket  :: socket(),
      Backlog :: integer(),
      Reason  :: posix() | 'closed'.

listen(?socket(SockRef), Backlog)
  when is_reference(SockRef), is_integer(Backlog) ->
    prim_socket:listen(SockRef, Backlog);
listen(Socket, Backlog) ->
    erlang:error(badarg, [Socket, Backlog]).


%% ===========================================================================
%%
%% accept, accept4 - accept a connection on a socket
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc("Equivalent to [`accept(ListenSocket, infinity)`](`accept/2`).").
-spec accept(ListenSocket :: term()) -> _.

accept(ListenSocket) ->
    accept(ListenSocket, ?ESOCK_ACCEPT_TIMEOUT_DEFAULT).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Accept a connection on a listening socket.

`ListenSocket` has to be of a connection oriented type
(types `stream` or `seqpacket`, see `open/1`), and set to listen
(see `listen/1`).

[](){: #accept-infinity }

If the `Timeout` argument is `infinity`; accepts the first pending
incoming connection for the listen socket or wait for one to arrive,
and return the new connection socket.

[](){: #accept-timeout }

If the `Timeout` argument is a time-out value (`t:non_neg_integer/0`);
returns `{error, timeout}` if no connection has arrived
after `Timeout` milliseconds.

[](){: #accept-nowait }

If the `Handle` argument `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.
""".
-spec accept(ListenSocket, Timeout :: 'infinity') ->
          {'ok', Socket} |
          {'error', Reason} when
      ListenSocket :: socket(),
      Socket       :: socket(),
      Reason       :: posix() | 'closed' | invalid() |
                      {create_accept_socket,  posix()} |
                      {add_socket,            posix()} |
                      {update_accept_context, posix()};

            (ListenSocket, Timeout :: non_neg_integer()) ->
          {'ok', Socket} |
          {'error', Reason} when
      ListenSocket :: socket(),
      Socket       :: socket(),
      Reason       :: posix() | 'closed' | invalid() | 'timeout' |
                      {create_accept_socket,  posix()} |
                      {add_socket,            posix()} |
                      {update_accept_context, posix()};
            (ListenSocket,
             'nowait' | Handle :: select_handle() | completion_handle()) ->
          {'ok', Socket} |
          {'select', SelectInfo} |
          {'completion', CompletionInfo} |
          {'error', Reason} when
      ListenSocket    :: socket(),
      Socket          :: socket(),
      SelectInfo      :: select_info(),
      CompletionInfo  :: completion_info(),
      Reason          :: posix() | closed | invalid() |
                         {create_accept_socket,  posix()} |
                         {add_accept_socket,     posix()} |
                         {update_accept_context, posix()}.


accept(?socket(LSockRef), Timeout)
  when is_reference(LSockRef) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            accept_nowait(LSockRef, Handle);
        handle ->
            Handle = Timeout,
            accept_nowait(LSockRef, Handle);
        Deadline ->
            accept_deadline(LSockRef, Deadline)
    end;
accept(ListenSocket, Timeout) ->
    erlang:error(badarg, [ListenSocket, Timeout]).

accept_nowait(LSockRef, Handle) ->
    case prim_socket:accept(LSockRef, Handle) of
        select ->
            {select,     ?SELECT_INFO(accept, Handle)};
        completion ->
            {completion, ?COMPLETION_INFO(accept, Handle)};
        Result ->
            accept_result(LSockRef, Handle, Result)
    end.

accept_deadline(LSockRef, Deadline) ->
    AccRef = make_ref(),
    case prim_socket:accept(LSockRef, AccRef) of
        select ->
            %% Each call is non-blocking, but even then it takes
            %% *some* time, so just to be sure, recalculate before
            %% the receive.
	    Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(LSockRef), select, AccRef) ->
                    accept_deadline(LSockRef, Deadline);
                ?socket_msg(_Socket, abort, {AccRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(LSockRef, accept, AccRef),
                    {error, timeout}
            end;
        completion ->
            %% Each call is non-blocking, but even then it takes
            %% *some* time, so just to be sure, recalculate before
            %% the receive.
	    Timeout = timeout(Deadline),
            receive
                %% CompletionStatus = {ok, Socket} | {error, Reason}
                ?socket_msg(?socket(LSockRef), completion,
                            {AccRef, CompletionStatus}) ->
                    CompletionStatus;
                ?socket_msg(_Socket, abort, {AccRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(LSockRef, accept, AccRef),
                    {error, timeout}
            end;
        Result ->
            accept_result(LSockRef, AccRef, Result)
    end.

accept_result(LSockRef, AccRef, Result) ->
    case Result of
        {ok, SockRef} ->
            Socket = ?socket(SockRef),
            {ok, Socket};
        {error, _} = ERROR ->
            %% Just to be on the safe side...
            _ = cancel(LSockRef, accept, AccRef),
            ERROR
    end.


%% ===========================================================================
%%
%% send, sendto, sendmsg - send a message on a socket
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc "Equivalent to [`send(Socket, Data, [], infinity)`](`send/4`).".
-spec send(Socket :: term(), Data :: term()) -> _.

send(Socket, Data) ->
    send(Socket, Data, ?ESOCK_SEND_FLAGS_DEFAULT, ?ESOCK_SEND_TIMEOUT_DEFAULT).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Send data on a connected socket.

With argument `Timeout`; equivalent to
[`send(Socket, Data, [], Timeout)`](`send/4`).

With argument `Flags`; equivalent to
[`send(Socket, Data, Flags, infinity)`](`send/4`).

With argument `Cont`; equivalent to
[`send(Socket, Data, Cont, infinity)`](`send/4`) *(since OTP 24.0)*.
""".
-spec send(Socket :: term(), Data :: term(), Cont :: tuple()) -> _;
          (Socket :: term(), Data :: term(), Flags :: list()) -> _;
          (Socket :: term(), Data :: term(), Timeout :: timeout()) -> _.

send(Socket, Data, Flags_Cont)
  when is_list(Flags_Cont);
       is_tuple(Flags_Cont) ->
    send(Socket, Data, Flags_Cont, ?ESOCK_SEND_TIMEOUT_DEFAULT);
send(Socket, Data, Timeout) ->
    send(Socket, Data, ?ESOCK_SEND_FLAGS_DEFAULT, Timeout).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Send data on a connected socket.

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

The `Data`, if it is not a `t:binary/0`, is copied into one before
calling the platform network API, because a single buffer is required.
A returned `RestData` is a sub binary of it.

The return value indicates the result from the platform's network layer:

- **`ok`** - All data was accepted by the OS for delivery

- **`{ok, RestData}`** - Some but not all data was accepted,
  but no error was reported (partially successful send).  `RestData`
  is the tail of `Data` that wasn't accepted.

  This cannot happen for a socket of [type `stream`](`t:type/0`) where
  such a partially successful send is retried until the data is either
  accepted for delivery or there is an error.

  For a socket of [type `dgram`](`t:type/0`) this should probably
  also not happen since a message that cannot be passed atomically
  should render an error.

  It is nevertheless possible for the platform's network layer
  to return this,  surely more possible for a socket of
  [type `seqpacket`](`t:type/0`).

- **`{error, Reason}`** - An error has been reported and no data
  was accepted for delivery.  [`Reason :: posix/0`](`t:posix/0`)
  is what the platform's network layer reported.  `closed` means
  that this socket library was informed that the socket was closed,
  and `t:invalid/0` means that this socket library found
  an argument to be invalid.

- **`{error, {Reason, RestData}}`** - An error was reported but before that
  some data was accepted for delivery. `RestData` is the tail of `Data`
  that wasn't accepted. See `{error, Reason}` above.

  This can only happen for a socket of [type `stream`](`t:type/0`)
  when a partially successful send is retried until there is an error.

[](){: #send-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #send-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestData}}` if some data was sent
(accepted by the OS for delivery).  `RestData` is the tail of the data
that hasn't been sent.

[](){: #send-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #send-cont }

If the function is called with a `Cont` argument, that is;
the [`SelectInfo`](`t:select_info/0`) from the previous
[`send/3,4`](`send/3`) call; the send is continued with
preprocessed send parameters in the `SelectInfo`.
Using this argument variant avoids for example having to validate
and encode message flags in every call but the first.
""".
-spec send(Socket, Data, Flags | Cont, Timeout :: 'infinity') ->
          'ok' |
          {'ok', RestData} |
          {'error', Reason} |
          {'error', {Reason, RestData}}
              when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: [msg_flag() | integer()],
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid() |
                    netname_deleted | too_many_cmds | eei();

          (Socket, Data, Flags | Cont, Timeout :: non_neg_integer()) ->
          'ok' |
          {'ok', RestData} |
          {'error', Reason | 'timeout'} |
          {'error', {Reason | 'timeout', RestData}}
              when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: [msg_flag() | integer()],
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid() |
                    netname_deleted | too_many_cmds | eei();

          (Socket, Data, Flags | Cont, 'nowait' | Handle) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Flags          :: [msg_flag() | integer()],
      Cont           :: select_info(),
      Handle         :: select_handle() | completion_handle(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() |
                        netname_deleted | too_many_cmds | eei().

send(?socket(SockRef), Data, ?SELECT_INFO(SelectTag, _) = Cont, Timeout)
  when is_reference(SockRef), is_binary(Data) ->
    case SelectTag of
        {send, ContData} ->
            case deadline(Timeout) of
                invalid ->
                    erlang:error({invalid, {timeout, Timeout}});
                nowait ->
                    SelectHandle = make_ref(),
                    send_nowait_cont(SockRef, Data, ContData, SelectHandle);
                handle ->
                    SelectHandle = Timeout,
                    send_nowait_cont(SockRef, Data, ContData, SelectHandle);
                Deadline ->
                    HasWritten = false,
                    send_deadline_cont(
                      SockRef, Data, ContData, Deadline, HasWritten)
            end;
        _ ->
            {error, {invalid, Cont}}
    end;
send(?socket(SockRef), Data, Flags, Timeout)
  when is_reference(SockRef), is_binary(Data), is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            send_nowait(SockRef, Data, Flags, Handle);
        handle ->
            Handle = Timeout,
            send_nowait(SockRef, Data, Flags, Handle);
        Deadline ->
            send_deadline(SockRef, Data, Flags, Deadline)
     end;
send(?socket(SockRef) = Socket, [Bin], Flags, Timeout)
  when is_reference(SockRef), is_binary(Bin) ->
    send(Socket, Bin, Flags, Timeout);
send(?socket(SockRef) = Socket, Data, Flags, Timeout)
  when is_reference(SockRef), is_list(Data) ->
    try erlang:list_to_binary(Data) of
        Bin ->
            send(Socket, Bin, Flags, Timeout)
    catch
        error : badarg ->
            erlang:error({invalid, {data, Data}})
    end;
send(Socket, Data, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Data, Flags, Timeout]).

send_nowait(SockRef, Bin, Flags, Handle) ->
    send_common_nowait_result(
      Handle, send,
      prim_socket:send(SockRef, Bin, Flags, Handle)).

%% On Windows, writes either succeed directly (it their entirety),
%% they are scheduled (completion) or they fail. *No* partial success,
%% and therefor no need to handle theme here (in cont).
send_nowait_cont(SockRef, Bin, Cont, SelectHandle) ->
    send_common_nowait_result(
      SelectHandle, send,
      prim_socket:send(SockRef, Bin, Cont, SelectHandle)).

send_deadline(SockRef, Bin, Flags, Deadline) ->
    Handle = make_ref(),
    HasWritten = false,
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       send, fun send_deadline_cont/5,
       prim_socket:send(SockRef, Bin, Flags, Handle)).

send_deadline_cont(SockRef, Bin, Cont, Deadline, HasWritten) ->
    Handle = make_ref(),
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       send, fun send_deadline_cont/5,
       prim_socket:send(SockRef, Bin, Cont, Handle)).



-compile({inline, [send_common_nowait_result/3]}).
send_common_nowait_result(Handle, Op, Result) ->
    case Result of
        completion ->
            {completion, ?COMPLETION_INFO(Op, Handle)};
        {select, ContData} ->
            {select, ?SELECT_INFO({Op, ContData}, Handle)};
        {select, Data, ContData} ->
            {select, {?SELECT_INFO({Op, ContData}, Handle), Data}};
        %%
        Result ->
            Result
    end.

-compile({inline, [send_common_deadline_result/8]}).
send_common_deadline_result(
  SockRef, Data, Handle, Deadline, HasWritten,
  Op, Fun, SendResult) ->
    %%
    case SendResult of
        select ->
            %% Would block, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, Handle) ->
                    Fun(SockRef, Data, undefined, Deadline, HasWritten);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    send_common_error(Reason, Data, HasWritten)
            after Timeout ->
                    _ = cancel(SockRef, Op, Handle),
                    send_common_error(timeout, Data, HasWritten)
            end;

        {select, Cont} ->
            %% Would block, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, Handle) ->
                    Fun(SockRef, Data, Cont, Deadline, HasWritten);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    send_common_error(Reason, Data, HasWritten)
            after Timeout ->
                    _ = cancel(SockRef, Op, Handle),
                    send_common_error(timeout, Data, HasWritten)
            end;
        {select, Data_1, Cont} ->
            %% Partial send success, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, Handle) ->
                    Fun(SockRef, Data_1, Cont, Deadline, true);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    send_common_error(Reason, Data_1, true)
            after Timeout ->
                    _ = cancel(SockRef, Op, Handle),
                    send_common_error(timeout, Data_1, true)
            end;

        completion ->
            %% Would block, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, completion, {Handle, CompletionStatus}) ->
                    CompletionStatus;
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    send_common_error(Reason, Data, false)
            after Timeout ->
		    %% ?DBG(['completion send timeout - cancel']),
                    _ = cancel(SockRef, Op, Handle),
                    send_common_error(timeout, Data, false)
            end;

        %%
        {error, {_Reason, RestIOV}} = Error when is_list(RestIOV) ->
            Error;
        {error, Reason} ->
            send_common_error(Reason, Data, HasWritten);

        Result ->
            %% ?DBG([{deadline, Deadline}, {op, Op}, {result, Result}]),
            Result
    end.


send_common_error(Reason, Data, HasWritten) ->
    case HasWritten of
        false ->
            %% We have not managed to send any data;
            %% do not return what remains
            {error, Reason};
        true ->
            %% Error on subsequent send - we have sent some data;
            %% return the remaining
            case Data of
                Bin when is_binary(Bin) ->
                    {error, {Reason, Bin}};
                IOVec when is_list(IOVec) ->
                    {error, {Reason, IOVec}};
                #{iov := IOVec} = _Msg ->
                    {error, {Reason, IOVec}}
            end
    end.


%% ---------------------------------------------------------------------------
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Send data on a socket.

With argument `Dest`; equivalent to
[`sendto(Socket, Data, Dest, [], infinity)`](`sendto/5`).

With argument `Cont`; equivalent to
[`sendto(Socket, Data, Cont, infinity)`](`sendto/4`) *since OTP 24.0*.
""".
-spec sendto(Socket :: term(), Data :: term(), Cont :: select_info()) -> _;
            (Socket :: term(), Data :: term(), Dest :: term()) -> _.

sendto(Socket, Data, ?SELECT_INFO(_, _) = Cont) ->
    sendto(Socket, Data, Cont, ?ESOCK_SENDTO_TIMEOUT_DEFAULT);
sendto(Socket, Data, Dest) ->
    sendto(
      Socket, Data, Dest,
      ?ESOCK_SENDTO_FLAGS_DEFAULT, ?ESOCK_SENDTO_TIMEOUT_DEFAULT).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Send data on a socket.

With arguments `Dest` and `TimeoutOrHandle`; equivalent to
[`sendto(Socket, Data, Dest, [], TimeoutOrHandle)`](`sendto/5`).

With arguments `Dest` and `Flags`; equivalent to
[`sendto(Socket, Data, Dest, Flags, infinity)`](`sendto/5`).

With arguments `Cont` and `TimeoutOrHandle`; `Cont` must be
the [`SelectInfo`](`t:select_info/0`) from the previous
[`sendto/3,4,5`](`sendto/3`) call and the send is continued with
preprocessed send parameters in the `SelectInfo`.
Using this argument variant avoids for example having o validate
and encode message flags in every call but the first.
*(Since OTP 24.0)*

See the last argument (argument 5) of `sendto/5` for
an explanation of `TimeoutOrHandle`.
""".
-spec sendto(Socket :: term(), Data :: term(),
             Dest :: term(), Flags :: list()) -> _;
            (Socket :: term(), Data :: term(),
             Cont :: select_info(), TimeoutOrHandle :: term()) -> _;
            (Socket :: term(), Data :: term(),
             Dest :: term(), TimeoutOrHandle :: term()) -> _.

sendto(Socket, Data, Dest, Flags) when is_list(Flags) ->
    sendto(Socket, Data, Dest, Flags, ?ESOCK_SENDTO_TIMEOUT_DEFAULT);
sendto(
  ?socket(SockRef) = Socket, Data,
  ?SELECT_INFO(SelectTag, _) = Cont, Timeout)
  when is_reference(SockRef) ->
    case SelectTag of
        {sendto, ContData} ->
            case Data of
                Bin when is_binary(Bin) ->
                    sendto_timeout_cont(SockRef, Bin, ContData, Timeout);
                [Bin] when is_binary(Bin) ->
                    sendto_timeout_cont(SockRef, Bin, ContData, Timeout);
                IOV when is_list(IOV) ->
                    try erlang:list_to_binary(IOV) of
                        Bin ->
                            sendto_timeout_cont(
                              SockRef, Bin, ContData, Timeout)
                    catch
                        error : badarg ->
                            erlang:error({invalid, {data, Data}})
                    end;
                _ ->
                    erlang:error(badarg, [Socket, Data, Cont, Timeout])
            end;
        _ ->
            {error, {invalid, Cont}}
    end;
sendto(Socket, Data, Dest, Timeout) ->
    sendto(Socket, Data, Dest, ?ESOCK_SENDTO_FLAGS_DEFAULT, Timeout).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Send data on a socket.

The `To` argument is the destination address where to send the data.
For a connected socket this argument is still passed to the OS call
that may ignore the address or return an error.

See `send/4` for a description of the `Flags` and `Data` arguments,
and the return values.

[](){: #sendto-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendto-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestData}}` if some data was sent
(accepted by the OS for delivery).  `RestData` is the tail of the data
that hasn't been sent.

[](){: #sendto-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

After receiving a `select` message call [`sendto/3,4`](`sendto/3`)
with `SelectInfo` as the `Cont` argument, to complete the operation.
""".
-spec sendto(Socket, Data, Dest, Flags, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Flags, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Flags, 'nowait' | Handle) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Dest           :: sockaddr(),
      Flags          :: [msg_flag() | integer()],
      Handle         :: select_handle() | completion_handle(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid().


sendto(?socket(SockRef), Data, Dest, Flags, Timeout)
  when is_reference(SockRef), is_binary(Data), is_list(Flags) ->
    %%
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendto_nowait(SockRef, Data, Dest, Flags, SelectHandle);
        handle ->
            Handle = Timeout,
            sendto_nowait(SockRef, Data, Dest, Flags, Handle);
        Deadline ->
            HasWritten = false,
            sendto_deadline(SockRef, Data, Dest, Flags, Deadline, HasWritten)
    end;
sendto(?socket(SockRef) = Socket, [Bin], Dest, Flags, Timeout)
  when is_reference(SockRef), is_binary(Bin) ->
    sendto(Socket, Bin, Dest, Flags, Timeout);
sendto(?socket(SockRef) = Socket, Data, Dest, Flags, Timeout)
  when is_reference(SockRef), is_list(Data) ->
    try erlang:list_to_binary(Data) of
        Bin ->
            sendto(Socket, Bin, Dest, Flags, Timeout)
    catch
        error : badarg ->
            erlang:error({invalid, {data, Data}})
    end;
sendto(Socket, Data, Dest, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Data, Dest, Flags, Timeout]).

sendto_timeout_cont(SockRef, Bin, Cont, Timeout) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendto_nowait_cont(SockRef, Bin, Cont, SelectHandle);
        handle ->
            Handle = Timeout,
            sendto_nowait_cont(SockRef, Bin, Cont, Handle);
        Deadline ->
            HasWritten = false,
            sendto_deadline_cont(SockRef, Bin, Cont, Deadline, HasWritten)
    end.

sendto_nowait(SockRef, Bin, To, Flags, Handle) ->
    send_common_nowait_result(
      Handle, sendto,
      prim_socket:sendto(SockRef, Bin, To, Flags, Handle)).

sendto_nowait_cont(SockRef, Bin, Cont, Handle) ->
    send_common_nowait_result(
      Handle, sendto,
      prim_socket:sendto(SockRef, Bin, Cont, Handle)).

sendto_deadline(SockRef, Bin, To, Flags, Deadline, HasWritten) ->
    Handle = make_ref(),
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       sendto, fun sendto_deadline_cont/5,
       prim_socket:sendto(SockRef, Bin, To, Flags, Handle)).

sendto_deadline_cont(SockRef, Bin, Cont, Deadline, HasWritten) ->
    Handle = make_ref(),
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       sendto, fun sendto_deadline_cont/5,
       prim_socket:sendto(SockRef, Bin, Cont, Handle)).


%% ---------------------------------------------------------------------------
%%
%% The only part of the msg_send() that *must* exist (a connected
%% socket need not specify the addr field) is the iov.
%% The ctrl field is optional, and the addr and flags are not
%% used when sending.
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc "Equivalent to [`sendmsg(Socket, Msg, [], infinity)`](`sendmsg/4`).".
-spec sendmsg(Socket :: term(), Msg :: term()) -> _.

sendmsg(Socket, Msg) ->
    sendmsg(Socket, Msg,
            ?ESOCK_SENDMSG_FLAGS_DEFAULT, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Send data and control messages on a socket.

With arguments `Msg` and `Timeout`; equivalent to
[`sendmsg(Socket, Msg, [], Timeout)`](`sendmsg/4`).

With arguments `Msg` and `Flags`; equivalent to
[`sendmsg(Socket, Msg, Flags, infinity)`](`sendmsg/4`).

With arguments `Data` and `Cont`; equivalent to
[`sendmsg(Socket, Data, Cont, infinity)`](`sendmsg/4`) *since OTP 24.0*.
""".
-spec sendmsg(Socket :: term(), Msg :: term(), Flags :: list())
             -> _;
             (Socket :: term(), Data :: term(), Cont :: select_info())
             -> _;
             (Socket :: term(), Msg :: term(), Timeout :: term())
             -> _.

sendmsg(Socket, Msg, Flags) when is_list(Flags) ->
    sendmsg(Socket, Msg, Flags, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT);
sendmsg(Socket, Data, Cont) when is_tuple(Cont) ->
    sendmsg(Socket, Data, Cont, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT);
sendmsg(Socket, Msg, Timeout) ->
    sendmsg(Socket, Msg, ?ESOCK_SENDMSG_FLAGS_DEFAULT, Timeout).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Send data and control messages on a socket.

The argument `Msg` is a map that contains the data to be sent
under the key `iov` as an`t:erlang:iovec/0` (list of `t:binary/0`).
It may also contain the destination address under the key `addr`,
which is mandatory if the socket isn't connected.  If the socket
_is_ connected it is best to not have an `addr` key since
the platform may regard that as an error (or ignore it).
Under the key `ctrl` there may be a list of protocol and platform dependent
control messages (a.k.a ancillary data, a.k.a control information)
to send.

[](){: #sendmsg-iov }

The message data is given to the platform's network layer as an
I/O vector without copying the content. If the number of elements
in the I/O vector is larger than allowed on the platform (reported
in the [`iov_max`](`t:info/0`) field from `info/0`), on a socket of
[type `stream`](`t:type/0`) the send is iterated over all elements,
but for other socket types the call fails.

See `send/4` for a description of the `Flags` argument
and the return values.

> #### Note {: .info }
>
> On Windows, this function can only be used with datagram and raw sockets.

[](){: #sendmsg-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendmsg-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestData}}` if some data was sent
(accepted by the OS for delivery).  `RestData` is the tail of the data
that hasn't been sent.

[](){: #sendmsg-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

After receiving a `select` message call [`sendmsg/3,4`](`sendmsg/3`)
with `SelectInfo` as the `Cont` argument, to complete the operation.

[](){: #sendmsg-cont }

With the arguments `Data` and [`Cont`](`t:select_info/0`),
continues the send operation.  `Cont` should be
the [`SelectInfo`](`t:select_info/0`) returned from the previous
[`sendmsg/2,3,4`](`sendmsg/2`) call.

`Data` can be a [`Msg`](`t:msg_send/0`) `t:map/0`
where only the key `iov` is used, or an `t:erlang:iovec/0`.
""".
-spec sendmsg(Socket, Msg, Flags, Timeout :: 'infinity') ->
          'ok' |
          {'ok', RestData} |
          {'error', Reason} |
          {'error', {Reason, RestData}}
              when
      Socket     :: socket(),
      Msg        :: msg_send(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Msg, Flags, Timeout :: non_neg_integer()) ->
          'ok' |
          {'ok', RestData} |
          {'error', Reason | 'timeout'} |
          {'error', {Reason | 'timeout', RestData}}
              when
      Socket     :: socket(),
      Msg        :: msg_send(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Msg, Flags, 'nowait' | Handle) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket         :: socket(),
      Msg            :: msg_send(),
      Flags          :: [msg_flag() | integer()],
      Handle         :: select_handle() | completion_handle(),
      RestData       :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Data, Cont, Timeout :: 'infinity') ->
          'ok' |
          {'ok', RestData} |
          {'error', Reason} |
          {'error', {Reason, RestData}}
              when
      Socket     :: socket(),
      Data       :: msg_send() | erlang:iovec(),
      Cont       :: select_info(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Data, Cont, Timeout :: non_neg_integer()) ->
          'ok' |
          {'ok', RestData} |
          {'error', Reason | 'timeout'} |
          {'error', {Reason | 'timeout', RestData}}
              when
      Socket     :: socket(),
      Data       :: msg_send() | erlang:iovec(),
      Cont       :: select_info(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Data, Cont, 'nowait' | Handle) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket         :: socket(),
      Data           :: msg_send() | erlang:iovec(),
      Cont           :: select_info(),
      Handle         :: select_handle(),
      RestData       :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid().

sendmsg(
  ?socket(SockRef) = Socket, RestData,
  ?SELECT_INFO(SelectTag, _) = Cont, Timeout) ->
    %%
    case SelectTag of
        {sendmsg, ContData} ->
            case RestData of
                #{iov := IOV} ->
                    sendmsg_timeout_cont(SockRef, IOV, ContData, Timeout);
                IOV when is_list(IOV) ->
                    sendmsg_timeout_cont(SockRef, IOV, ContData, Timeout);
                _ ->
                    erlang:error(badarg, [Socket, RestData, Cont, Timeout])
            end;
        _ ->
            {error, {invalid, Cont}}
    end;
sendmsg(?socket(SockRef), #{iov := IOV} = Msg, Flags, Timeout)
  when is_reference(SockRef), is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            sendmsg_nowait(SockRef, Msg, Flags, Handle, IOV);
        handle ->
            Handle = Timeout,
            sendmsg_nowait(SockRef, Msg, Flags, Handle, IOV);
        Deadline ->
            HasWritten = false,
            sendmsg_deadline(SockRef, Msg, Flags, Deadline, HasWritten, IOV)
    end;
sendmsg(Socket, Msg, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Msg, Flags, Timeout]).

sendmsg_timeout_cont(SockRef, RestData, Cont, Timeout) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendmsg_nowait_cont(SockRef, RestData, Cont, SelectHandle);
        handle ->
            SelectHandle = Timeout,
            sendmsg_nowait_cont(SockRef, RestData, Cont, SelectHandle);
        Deadline ->
            HasWritten = false,
            sendmsg_deadline_cont(
              SockRef, RestData, Cont, Deadline, HasWritten)
    end.

sendmsg_nowait(SockRef, Msg, Flags, Handle, IOV) ->
    send_common_nowait_result(
      Handle, sendmsg,
      prim_socket:sendmsg(SockRef, Msg, Flags, Handle, IOV)).

sendmsg_nowait_cont(SockRef, RestData, Cont, SelectHandle) ->
    send_common_nowait_result(
      SelectHandle, sendmsg,
      prim_socket:sendmsg(SockRef, RestData, Cont, SelectHandle)).

sendmsg_deadline(SockRef, Msg, Flags, Deadline, HasWritten, IOV) ->
    Handle = make_ref(),
    send_common_deadline_result(
      SockRef, IOV, Handle, Deadline, HasWritten,
      sendmsg, fun sendmsg_deadline_cont/5,
      prim_socket:sendmsg(SockRef, Msg, Flags, Handle, IOV)).

sendmsg_deadline_cont(SockRef, Data, Cont, Deadline, HasWritten) ->
    SelectHandle = make_ref(),
    send_common_deadline_result(
      SockRef, Data, SelectHandle, Deadline, HasWritten,
      sendmsg, fun sendmsg_deadline_cont/5,
      prim_socket:sendmsg(SockRef, Data, Cont, SelectHandle)).


%% ---------------------------------------------------------------------------
%%
%%

-doc(#{equiv => sendv(Socket, IOV, infinity)}).
-doc(#{since => <<"OTP 27.0">>}).
-spec sendv(Socket, IOV) ->
          'ok' |
          {'ok', RestIOV} |
          {'error', Reason} |
          {'error', {Reason, RestIOV}}
              when
      Socket  :: socket(),
      IOV     :: erlang:iovec(),
      RestIOV :: erlang:iovec(),
      Reason  :: posix() | 'closed' | invalid().

sendv(Socket, IOV) ->
    sendv(Socket, IOV, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT).


-doc """
Send `t:erlang:iovec/0` data on a connected socket.

See [`sendmsg/4`](#sendmsg-iov) about how the [`IOV`](`t:erlang:iovec/0`)
data is handled towards the platform's network layer.

The return value indicates the result from the platform's network layer:

- **`ok`** - All data has been accepted by the OS for delivery.

- **`{ok, RestIOV}`** - Some but not all data was accepted,
  but no error was reported (partially successful send).  `RestIOV`
  is the tail of `IOV` that wasn't accepted.

- **`{error, Reason}`** - An error has been reported and no data
  was accepted for delivery.  [`Reason :: posix/0`](`t:posix/0`)
  is what the platform's network layer reported.  `closed` means
  that this socket library was informed that the socket was closed,
  and `t:invalid/0` means that this socket library found
  an argument to be invalid.

- **`{error, {Reason, RestIOV}}`** -  - An error was reported but before that
  some data was accepted for delivery. `RestIOV` is the tail of `IOV`
  that wasn't accepted. See `{error, Reason}` above.

[](){: #sendv-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendv-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestIOV}}` if some data was sent
(accepted by the OS for delivery).  `RestIOV` is the tail of the data
that hasn't been sent.

[](){: #sendv-nowait }

If the `Handle` argument is `nowait`,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
or on _Windows_, the equivalent `t:completion_handle/0`, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #sendv-cont }

With the argument [`Cont`](`t:select_info/0`), equivalent to
[`sendv(Socket, IOV, Cont, infinity)`](`sendv/4`).
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec sendv(Socket, IOV, Timeout :: 'infinity') ->
          'ok' |
          {'ok', RestIOV} |
          {'error', Reason} |
          {'error', {Reason, RestIOV}}
              when
      Socket     :: socket(),
      IOV        :: erlang:iovec(),
      RestIOV    :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, IOV, Timeout :: non_neg_integer()) ->
          'ok' |
          {'ok', RestIOV} |
          {'error', Reason} |
          {'error', {Reason, RestIOV}}
              when
      Socket  :: socket(),
      IOV     :: erlang:iovec(),
      RestIOV :: erlang:iovec(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout';

           (Socket, IOV, 'nowait' | Handle) ->
          'ok' |
          {'ok', RestIOV} |
          {'select', SelectInfo} |
          {'select', {SelectInfo, RestIOV}} |
          {'completion', CompletionInfo} |
          {'error', Reason} |
          {'error', {Reason, RestIOV}}
              when
      Socket         :: socket(),
      IOV            :: erlang:iovec(),
      Handle         :: select_handle() | completion_handle(),
      RestIOV        :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

           (Socket, IOV, Cont) ->
          'ok' |
          {'ok', RestIOV} |
          {'error', Reason} |
          {'error', {Reason, RestIOV}}
              when
      Socket  :: socket(),
      IOV     :: erlang:iovec(),
      Cont    :: select_info(),
      RestIOV :: erlang:iovec(),
      Reason  :: posix() | 'closed' | invalid().

sendv(?socket(SockRef) = Socket, IOV,
      ?SELECT_INFO(SelectTag, _) = Cont)
  when is_reference(SockRef) ->
    case SelectTag of
        {sendv, _} -> % We do not use ContData here
            sendv_timeout_cont(SockRef, IOV, ?ESOCK_SENDV_TIMEOUT_DEFAULT);
        _ ->
            erlang:error(badarg, [Socket, IOV, Cont])
    end;
sendv(?socket(SockRef), IOV, Timeout)
  when is_reference(SockRef) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            sendv_nowait(SockRef, IOV, Handle);
        handle ->
            Handle = Timeout,
            sendv_nowait(SockRef, IOV, Handle);
        Deadline ->
            sendv_deadline(SockRef, IOV, Deadline)
    end;
sendv(Socket, IOV, Timeout) ->
    erlang:error(badarg, [Socket, IOV, Timeout]).

-doc """
Send data on a connected socket, continuation.

Continues sending data on a connected socket.
`Cont` is the [`SelectInfo`](`t:select_info/0`) returned from
the previous [`sendv/2,3`](`sendv/2`) call.
`IOV` should be the rest data that wasn't sent.

See [asynchronous calls](#asynchronous-calls) about continuing
unfinished calls.

See `sendv/3` about the return values.
""".

-spec sendv(Socket, IOV, Cont, Timeout :: 'infinity') ->
          'ok' |
          {'ok', RestIOV} |
          {'error', Reason} |
          {'error', {Reason, RestIOV}}
              when
      Socket     :: socket(),
      IOV        :: erlang:iovec(),
      Cont       :: select_info(),
      RestIOV    :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

           (Socket, IOV, Cont, Timeout :: non_neg_integer()) ->
          'ok' |
          {'ok', RestIOV} |
          {'error', Reason} |
          {'error', {Reason | RestIOV}}
              when
      Socket     :: socket(),
      IOV        :: erlang:iovec(),
      Cont       :: select_info(),
      RestIOV    :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid() | 'timeout';

(Socket, IOV, Cont, 'nowait' | SelectHandle ) ->
          'ok' |
          {'ok', RestIOV} |
          {'select', SelectInfo} |
          {'select', {SelectInfo, RestIOV}} |
          {'error', Reason} |
          {'error', {Reason, RestIOV}}
              when
      Socket         :: socket(),
      IOV            :: erlang:iovec(),
      Cont           :: select_info(),
      SelectHandle   :: select_handle(),
      RestIOV        :: erlang:iovec(),
      SelectInfo     :: select_info(),
      Reason         :: posix() | 'closed' | invalid().

-doc(#{since => <<"OTP 27.0">>}).
sendv(?socket(SockRef) = _Socket, IOV,
      ?SELECT_INFO(SelectTag, _) = Cont, Timeout)
  when is_reference(SockRef) andalso is_list(IOV) ->
    %%
    case SelectTag of
        {sendv, _} -> % We do not use ContData here
            sendv_timeout_cont(SockRef, IOV, Timeout);
        _ ->
            {error, {invalid, Cont}}
    end;
sendv(Socket, IOV, Cont, Timeout) ->
    erlang:error(badarg, [Socket, IOV, Cont, Timeout]).


sendv_timeout_cont(SockRef, RestIOV, Timeout) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendv_nowait_cont(SockRef, RestIOV, SelectHandle);
        handle ->
            SelectHandle = Timeout,
            sendv_nowait_cont(SockRef, RestIOV, SelectHandle);
        Deadline ->
            HasWritten = false,
            sendv_deadline_cont(
              SockRef, RestIOV, undefined, Deadline, HasWritten)
    end.

sendv_nowait(SockRef, IOV, Handle) ->
    send_common_nowait_result(
      Handle, sendv,
      prim_socket:sendv(SockRef, IOV, Handle)).

sendv_nowait_cont(SockRef, RestIOV, SelectHandle) ->
    send_common_nowait_result(
      SelectHandle, sendv,
      prim_socket:sendv(SockRef, RestIOV, SelectHandle)).

sendv_deadline(SockRef, IOV, Deadline) ->
    Handle     = make_ref(),
    HasWritten = false,
    send_common_deadline_result(
      SockRef, IOV, Handle, Deadline, HasWritten,
      sendv, fun sendv_deadline_cont/5,
      prim_socket:sendv(SockRef, IOV, Handle)).

sendv_deadline_cont(SockRef, IOV, _, Deadline, HasWritten) ->
    SelectHandle = make_ref(),
    send_common_deadline_result(
      SockRef, IOV, SelectHandle, Deadline, HasWritten,
      sendv, fun sendv_deadline_cont/5,
      prim_socket:sendv(SockRef, IOV, SelectHandle)).


%% ===========================================================================
%%
%% sendfile - send a file on a socket
%%

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Send a file on a socket.

Equivalent to
[`sendfile(Socket, FileHandle_or_Continuation, 0, 0, infinity)`](`sendfile/5`).
""".
-spec sendfile(Socket :: term(), FileHandle_or_Continuation :: term()) -> _.

sendfile(Socket, FileHandle_Cont) ->
    sendfile(Socket, FileHandle_Cont, 0, 0, infinity).

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Send a file on a socket.

Equivalent to
[`sendfile(Socket, FileHandle_or_Continuation, 0, 0, Timeout_or_Handle)`](`sendfile/5`).
""".
-spec sendfile(Socket :: term(),
               FileHandle_or_Continuation :: term(),
               Timeout_or_Handle :: term()) -> _.
sendfile(Socket, FileHandle_Cont, Timeout_Handle) ->
    sendfile(Socket, FileHandle_Cont, 0, 0, Timeout_Handle).

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Send a file on a socket.

Equivalent to
[`sendfile(Socket, FileHandle_or_Continuation, Offset, Count, infinity)`](`sendfile/5`).
""".
-spec sendfile(Socket :: term(),
               FileHandle_or_Continuation :: term(),
               Offset :: term(), Count :: term()) -> _.
sendfile(Socket, FileHandle_Cont, Offset, Count) ->
    sendfile(Socket, FileHandle_Cont, Offset, Count, infinity).


-doc """
Send a file on a socket.

> #### Note {: .info }
> This function unsupported on Windows.

The `FileHandle` argument must refer to an open raw file
as described in `file:open/2`.

The `Offset` argument is the file offset to start reading from.
The default offset is `0`.

The `Count` argument is the number of bytes to transfer
from `FileHandle` to `Socket`. If `Count = 0` (the default)
the transfer stops at the end of file.

The return value indicates the result from the platform's network layer:

- **`{ok, BytesSent}`** - The transfer completed successfully after `BytesSent`
  bytes of data.

- **`{error, Reason}`** - An error has been reported and no data
  was transferred. [`Reason :: posix/0`](`t:posix/0`)
  is what the platform's network layer reported.  `closed` means
  that this socket library was informed that the socket was closed,
  and `t:invalid/0` means that this socket library found
  an argument to be invalid.

- **`{error, {Reason, BytesSent}}`** - An error has been reported
  but before that some data was transferred. See `{error, Reason}`
  and `{ok, BytesSent}` above.

[](){: #sendfile-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendfile-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, BytesSent}}` if some but not all data was sent
(accepted by the OS for delivery).

[](){: #sendfile-nowait }

If the `Handle` argument is `nowait`,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

After receiving a `select` message call [`sendfile/2,3,4,5`](`sendfile/2`)
with `SelectInfo` as the `Continuation` argument, to complete the operation.

[](){: #sendfile-cont }

If the function is called with a `Continuation` argument, that is;
the [`SelectInfo`](`t:select_info/0`) from the previous
`sendfile/5` call; the transfer is continued with
preprocessed parameters in the `SelectInfo`.

The `Offset` and maybe `Count` arguments will probably
need to be updated between continuation calls.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec sendfile(Socket, FileHandle | Continuation,
               Offset, Count, Timeout :: 'infinity') ->
          {'ok', BytesSent} |
          {'error', Reason} |
          {'error', {Reason, BytesSent}}
              when
      Socket       :: socket(),
      FileHandle   :: file:fd(),
      Continuation :: select_info(),
      Offset       :: integer(),
      Count        :: non_neg_integer(),
      BytesSent    :: non_neg_integer(),
      Reason       :: posix() | 'closed' | invalid();

              (Socket, FileHandle | Continuation, Offset, Count,
               Timeout :: non_neg_integer()) ->
          {'ok', BytesSent} |
          {'error', Reason} |
          {'error', {Reason, BytesSent}}
              when
      Socket       :: socket(),
      FileHandle   :: file:fd(),
      Continuation :: select_info(),
      Offset       :: integer(),
      Count        :: non_neg_integer(),
      BytesSent    :: non_neg_integer(),
      Reason       :: posix() | 'closed' | invalid() | 'timeout';

              (Socket, FileHandle | Continuation,
               Offset, Count, 'nowait' | SelectHandle :: select_handle()) ->
          {'ok', BytesSent} |
          {'select', SelectInfo} |
          {'select', {SelectInfo, BytesSent}} |
          {'error', Reason}
              when
      Socket       :: socket(),
      FileHandle   :: file:fd(),
      Continuation :: select_info(),
      Offset       :: integer(),
      Count        :: non_neg_integer(),
      BytesSent    :: non_neg_integer(),
      SelectInfo   :: select_info(),
      Reason       :: posix() | 'closed' | invalid().

sendfile(
  ?socket(SockRef) = Socket, FileHandle_Cont, Offset, Count, Timeout)
  when is_integer(Offset), is_integer(Count), 0 =< Count ->
    %%
    case FileHandle_Cont of
        #file_descriptor{module = Module} = FileHandle ->
            GetFRef = internal_get_nif_resource,
            try Module:GetFRef(FileHandle) of
                FRef ->
                    State = {FRef, Offset, Count},
                    sendfile_int(SockRef, State, Timeout)
            catch
                %% We could just crash here, since the caller
                %% maybe broke the API and did not provide
                %% a raw file as FileHandle, i.e GetFRef
                %% is not implemented in Module;
                %% but instead handle that nicely
                Class : Reason : Stacktrace
                  when Class =:= error, Reason =:= undef ->
                    case Stacktrace of
                        [{Module, GetFRef, Args, _} | _]
                          when Args =:= 1;        % Arity 1
                               tl(Args) =:= [] -> % Arity 1
                            erlang:error(
                              badarg,
                              [Socket, FileHandle_Cont,
                               Offset, Count, Timeout]);
                        _ -> % Re-raise
                            erlang:raise(Class, Reason, Stacktrace)
                    end
            end;
        ?SELECT_INFO(SelectTag, _) = Cont ->
            case SelectTag of
                {sendfile, FRef} ->
                    State = {FRef, Offset, Count},
                    sendfile_int(SockRef, State, Timeout);
                sendfile ->
                    State = {Offset, Count},
                    sendfile_int(SockRef, State, Timeout);
                _ ->
                    {error, {invalid, Cont}}
            end;
        _ ->
            erlang:error(
              badarg, [Socket, FileHandle_Cont, Offset, Count, Timeout])
    end;
sendfile(Socket, FileHandle_Cont, Offset, Count, Timeout) ->
    erlang:error(
      badarg, [Socket, FileHandle_Cont, Offset, Count, Timeout]).

sendfile_int(SockRef, State, Timeout) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendfile_nowait(SockRef, State, SelectHandle);
        handle ->
            SelectHandle = Timeout,
            sendfile_nowait(SockRef, State, SelectHandle);
        Deadline ->
            BytesSent = 0,
            sendfile_deadline(SockRef, State, BytesSent, Deadline)
    end.


-compile({inline, [prim_socket_sendfile/3]}).
prim_socket_sendfile(SockRef, {FRef, Offset, Count}, SelectHandle) ->
    %% Start call
    prim_socket:sendfile(SockRef, FRef, Offset, Count, SelectHandle);
prim_socket_sendfile(SockRef, {Offset, Count}, SelectHandle) ->
    %% Continuation call
    prim_socket:sendfile(SockRef, Offset, Count, SelectHandle).

sendfile_nowait(SockRef, State, SelectHandle) ->
    case prim_socket_sendfile(SockRef, State, SelectHandle) of
        select ->
            %% Can only happen when we are enqueued after
            %% a send in progress so BytesSent is 0;
            %% wait for continuation and later repeat start call
            {FRef, _Offset, _Count} = State,
            {select, ?SELECT_INFO({sendfile, FRef}, SelectHandle)};
        {select, BytesSent} ->
            {select, {?SELECT_INFO(sendfile, SelectHandle), BytesSent}};
        %%
        Result ->
            Result
    end.

sendfile_deadline(SockRef, State, BytesSent_0, Deadline) ->
    SelectHandle = make_ref(),
    case prim_socket_sendfile(SockRef, State, SelectHandle) of
        select ->
            %% Can only happen when we are enqueued after
            %% a send in progress so BytesSent is 0;
            %% wait for continuation and repeat start call
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, SelectHandle) ->
                    sendfile_deadline(
                      SockRef, State, BytesSent_0, Deadline);
                ?socket_msg(_Socket, abort, {SelectHandle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, sendfile, SelectHandle),
                    {error, timeout}
            end;
        {select, BytesSent} ->
            %% Partial send success; wait for continuation
            Timeout = timeout(Deadline),
            BytesSent_1 = BytesSent_0 + BytesSent,
            receive
                ?socket_msg(_Socket, select, SelectHandle) ->
                    sendfile_deadline(
                      SockRef,
                      sendfile_next(BytesSent, State),
                      BytesSent_1, Deadline);
                ?socket_msg(_Socket, abort, {SelectHandle, Reason}) ->
                    {error, {Reason, BytesSent_1}}
            after Timeout ->
                    _ = cancel(SockRef, sendfile, SelectHandle),
                    {error, {timeout, BytesSent_1}}
            end;
        {error, _} = Result when tuple_size(State) =:= 3 ->
            Result;
        {error, Reason} when tuple_size(State) =:= 2 ->
            {error, {Reason, BytesSent_0}};
        {ok, BytesSent} ->
            {ok, BytesSent_0 + BytesSent}
    end.

sendfile_next(BytesSent, {_FRef, Offset, Count}) ->
    sendfile_next(BytesSent, Offset, Count);
sendfile_next(BytesSent, {Offset, Count}) ->
    sendfile_next(BytesSent, Offset, Count).
%%
sendfile_next(BytesSent, Offset, Count) ->
    {Offset + BytesSent,
     if
         Count =:= 0 ->
             0;
         BytesSent < Count ->
             Count - BytesSent
     end}.

%% ===========================================================================
%%
%% recv, recvfrom, recvmsg - receive a message from a socket
%%
%% Description:
%% There is a special case for the argument Length. If its set to zero (0),
%% it means "give me everything you have".
%%
%% Returns: {ok, Binary} | {error, Reason}
%% Binary  - The received data as a binary
%% Reason  - The error reason:
%%              timeout | {timeout, AccData} |
%%              posix() | {posix(), AccData} |
%%              atom()  | {atom(), AccData}
%% AccData - The data (as a binary) that we did manage to receive
%%           before the timeout.
%%
%% Arguments:
%% Socket  - The socket to read from.
%% Length  - The number of bytes to read.
%% Flags   - A list of "options" for the read.
%% Timeout - Time-out in milliseconds.

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Equivalent to [`recv(Socket, 0, [], infinity)`](`recv/4`).
""".
-spec recv(Socket :: term()) -> _.

recv(Socket) ->
    recv(Socket, 0, ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive data on a connected socket.

With argument `Length`; equivalent to
[`recv(Socket, Length, [], infinity)`](`recv/4`).

With argument `Flags`; equivalent to
[`recv(Socket, 0, Flags, infinity)`](`recv/4`) *(since OTP 24.0)*.
""".
-spec recv(Socket :: term(), Flags :: list()) -> _;
          (Socket :: term(), Length :: non_neg_integer()) -> _.

recv(Socket, Flags) when is_list(Flags) ->
    recv(Socket, 0, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length) when is_integer(Length), 0 =< Length ->
    recv(Socket, Length,
         ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length) ->
    error(badarg, [Socket, Length]).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive data on a connected socket.

With arguments `Length` and `Flags`; equivalent to
[`recv(Socket, Length, Flags, infinity)`](`recv/4`).

With arguments `Length` and `TimeoutOrHandle`; equivalent to
[`recv(Socket, Length, [], TimeoutOrHandle)`](`recv/4`).
`TimeoutOrHandle :: nowait` has been allowed *since OTP 22.1*.
`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.

With arguments `Flags` and `TimeoutOrHandle`; equivalent to
[`recv(Socket, 0, Flags, TimeoutOrHandle)`](`recv/4`)
*(since OTP 24.0)*.
""".
-spec recv(Socket :: term(), Flags :: list(), TimeoutOrHandle :: term())
          -> _;
          (Socket :: term(), Length :: term(), Flags :: list())
          -> _;
          (Socket :: term(), Length :: term(), TimeoutOrHandle :: term())
          -> _.

recv(Socket, Flags, TimeoutOrHandle) when is_list(Flags) ->
    recv(Socket, 0, Flags, TimeoutOrHandle);
recv(Socket, Length, Flags) when is_list(Flags) ->
    recv(Socket, Length, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length, TimeoutOrHandle) ->
    recv(Socket, Length, ?ESOCK_RECV_FLAGS_DEFAULT, TimeoutOrHandle).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive data on a connected socket.

The argument `Length` specifies how many bytes to receive,
with the special case `0` meaning "all available".

When `Length` is `0`, a default buffer size is used, which can be set by
[`socket:setopt(Socket, {otp,recvbuf}, BufSz)`](`setopt/3`).

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

When there is a socket error this function returns `{error, Reason}`,
or if some data arrived before the error; `{error, {Reason, Data}}`
(can only happen for a socket of [type `stream`](`t:type/0`)).

[](){: #recv-infinity }

If the `Timeout` argument is `infinity`; waits for the data to arrive.
For a socket of [type `stream`](`t:type/0`) this call
won't return until _all_ requested data can be delivered,
or if "all available" was requested when the first data chunk arrives,
or if the OS reports an error for the operation.

[](){: #recv-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has arrived after `Timeout` milliseconds,
or `{error, {timeout, Data}}` if some but not enough data
has been received on a socket of [type `stream`](`t:type/0`).

`Timeout = 0` only polls the OS receive call and doesn't
engage the Asynchronous Calls mechanisms.  If no data
is immediately available `{error, timeout} is returned.
`On a socket of type [`stream`](`t:type/0`), `{error, {timeout, Data}}`
is returned if there is an insufficient amount of data immediately available.

[](){: #recv-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

On `select` systems, for a socket of type [`stream`](`t:type/0`),
if `Length > 0` and there isn't enough data available, this function
will return [`{select, {SelectInfo, Data}}`](`t:select_info/0`)
with partial `Data`.  A repeated call to complete the operation
will probably need an updated `Length` argument.
""".
-spec recv(Socket, Length, Flags, Timeout :: 'infinity') ->
          {'ok', Data} |
          {'error', Reason} |
          {'error', {Reason, Data}} when
      Socket  :: socket(),
      Length  :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid();

          (Socket, Length, Flags, Timeout :: non_neg_integer()) ->
          {'ok', Data} |
          {'error', Reason} |
          {'error', {Reason, Data}} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Flags  :: [msg_flag() | integer()],
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid() | 'timeout';

          (Socket, Length, Flags, 'nowait' | Handle) ->
          {'ok', Data} |
          {'select', SelectInfo} |
          {'select', {SelectInfo, Data}} |
          {'completion', CompletionInfo} |
          {'error', Reason} |
          {'error', {Reason, Data}} when
      Socket         :: socket(),
      Length         :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Handle         :: select_handle() | completion_handle(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid().


recv(?socket(SockRef), Length, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(Length), Length >= 0,
       is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            recv_nowait(SockRef, Length, Flags, Handle);
        handle ->
            Handle = Timeout,
            recv_nowait(SockRef, Length, Flags, Handle);
        zero ->
            recv_zero(SockRef, Length, Flags, []);
        Deadline ->
            recv_deadline(SockRef, Length, Flags, Deadline, [])
    end;
recv(Socket, Length, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Length, Flags, Timeout]).

%% NIF return values:
%%
%% When Timeout = zero:
%%
%% ok              -> Would block - no data immediately available
%% {ok, Bin}       -> This is all data immediately available,
%%                    and it is less than requested or less than
%%                    the default buffer size,
%%                    or it is a stream socket with request 0
%%                    (any length) that filled the default buffer size
%%                    but hit the max buffer count which in a way
%%                    also is less than requested,
%%                    or it is the requested (or default) amount of data.
%% {more, Bin}     -> This is a stream socket with request 0
%%                    (any length) that filled the default buffer size
%%                    but hasn't hit the max buffer count so there is
%%                    a good chance more data is immediately available.
%% {error, Reason} -> Error
%%
%% When Timeout = ref():
%%
%% {ok, Bin}       -> This is the requested (or default) amount of data.
%% {more, Bin}     -> This is a stream socket with request 0
%%                    (any length) that filled the default buffer size
%%                    but hasn't hit the max buffer count so there is
%%                    a good chance more data is immediately available.
%% {error, Reason} -> Error
%%
%% select          -> Would block - no data immediately available,
%%                    socket added to VM select set.
%% {select, Bin}   -> Would block - stream socket incomplete data,
%%                    socket added to VM select set.  This is the data
%%                    that was immediately available.
%% completion      -> Would block - no data immediately available,
%%                    socket added to VM select set.  The requested
%%                    amount of data will be delivered in a message.

%% deadline(Timeout = 0) -> zero
recv_zero(SockRef, Length, Flags, Buf) ->
    case prim_socket:recv(SockRef, Length, Flags, zero) of
        {more, Bin} -> % Type == stream, Length == 0, default buffer filled
            recv_zero(SockRef, Length, Flags, [Bin | Buf]);
        timeout when Buf =:= [] ->
            {error, timeout};
        timeout ->
            %% We have gotten some {more,_} before so it is
            %% a stream socket and Length =:= 0
            {ok, condense_buffer(Buf)};
        {timeout, Bin} ->
            %% Stream socket with Length > 0 and not all data
            {error, {timeout, condense_buffer([Bin | Buf])}};
        {ok, Bin} -> % All requested data
            {ok, condense_buffer([Bin | Buf])};
        {error, _} = Error when Buf =:= [] ->
            Error;
        {error, Reason} ->
            {error, {Reason, condense_buffer(Buf)}}
    end.

%% Condense buffer into a Binary
-compile({inline, [condense_buffer/1]}).
condense_buffer([]) -> <<>>;
condense_buffer([Bin]) when is_binary(Bin) -> Bin;
condense_buffer(Buffer) ->
    iolist_to_binary(lists:reverse(Buffer)).

recv_nowait(SockRef, Length, Flags, Handle) ->
    case prim_socket:recv(SockRef, Length, Flags, Handle) of
        {more, Bin} -> % Type = stream, Length = 0, default buffer filled
            recv_zero(SockRef, Length, Flags, [Bin]);
        {select, Bin} ->
            %% We got less than requested so the caller will
            %% get a select message when there might be more to read
            {select, {?SELECT_INFO(recv, Handle), Bin}};
        select ->
            %% The caller will get a select message when there
            %% might be data to read
            {select, ?SELECT_INFO(recv, Handle)};
        completion ->
            %% The caller will get a completion message (with the
            %% result) when the data arrives. *No* further action
            %% is required.
            {completion, ?COMPLETION_INFO(recv, Handle)};
        {ok, _} = OK -> % All requested data
            OK;
        {error, _} = Error ->
            Error
    end.

%% prim_socket:recv(_, AskedFor, _, zero|Handle)
%%
%% if got 0, type == STREAM                             -> {error, closed}
%% if got full buffer ->
%%     if asked for 0, type == STREAM ->
%%         if rNum =< rNumCnt                           -> {ok, Bin}
%%         else rNumCnt < rNum                          -> {more, Bin}
%%         end
%%     else asked for N; type != STREAM                 -> {ok, Bin}
%%     end
%% else got less than buffer ->
%%     if asked for N, type == STREAM ->
%%         if Timeout zero ->                           -> {timeout, Bin}
%%         else nowait Handle ->                        -> {select, Bin}
%%     else type != STREAM; asked for 0 ->              -> {ok, Bin}
%%     end
%% else got no data and would block ->
%%     if Timeout zero ->                               -> timeout
%%     else nowait Handle                               -> select
%%     end
%% else read error                                      -> {error, _}
%% end

%% We will only recurse with Length == 0 if Length is 0,
%% so Length == 0 means to return all available data also when recursing

recv_deadline(SockRef, Length, Flags, Deadline, Buf) ->
    Handle = make_ref(),
    case prim_socket:recv(SockRef, Length, Flags, Handle) of
        {more, Bin} -> % Type = stream, Length = 0, default buffer filled
            0 = Length,
            recv_zero(SockRef, Length, Flags, [Bin]);
        %%
        {select, Bin} ->
            %% We got less than requested on a stream socket
	    Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), select, Handle) ->
                    if
                        0 < Timeout ->
                            %% Recv more
                            recv_deadline(
                              SockRef, Length - byte_size(Bin), Flags,
                              Deadline, [Bin | Buf]);
                        true ->
                            {error, {timeout, condense_buffer([Bin | Buf])}}
                    end;
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, {Reason, condense_buffer([Bin | Buf])}}
            after Timeout ->
                    _ = cancel(SockRef, recv, Handle),
                    recv_error(Buf, timeout)
            end;
        %%
        select
          when 0 < Length;   % Requested a specific amount of data
               Buf =:= [] -> % or Buf empty (and requested any amount of data)
            %%
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), select, Handle) ->
                    if
                        0 < Timeout ->
                            %% Retry
                            recv_deadline(
                              SockRef, Length, Flags, Deadline, Buf);
                        true ->
                            recv_error(Buf, timeout)
                    end;
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    recv_error(Buf, Reason)
            after Timeout ->
                    _ = cancel(SockRef, recv, Handle),
                    recv_error(Buf, timeout)
            end;
        %%
        select -> % Length is 0 (request any amount of data), Buf not empty
            %%
            %% We first got some data and are then asked to wait,
            %% but what we already got will do just fine;
            %% - cancel and return what we have
            _ = cancel(SockRef, recv, Handle),
            {ok, condense_buffer(Buf)};
        %%
        completion ->
            %% There is nothing just now, but we will be notified when the
            %% data has been read (with a completion message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {ok, Bin}})
                  when Length =:= 0 ->
                    {ok, condense_buffer([Bin | Buf])};
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {ok, Bin}})
                  when Length =:= byte_size(Bin) ->
                    {ok, condense_buffer([Bin | Buf])};
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {ok, Bin}}) ->
                    if
                        0 < Timeout ->
                            %% Recv more
                            recv_deadline(
                              SockRef, Length - byte_size(Bin), Flags,
                              Deadline, [Bin | Buf]);
                        true ->
                            recv_error([Bin | Buf], timeout)
                    end;
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {error, Reason}}) ->
                    recv_error(Buf, Reason);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recv, Handle),
                    recv_error(Buf, timeout)
            end;


        %% All requested data
        {ok, Bin} when (Length =:= 0) orelse
                       (Length =:= byte_size(Bin)) -> % All requested data
            {ok, condense_buffer([Bin | Buf])};

        {ok, Bin} -> % Only part of the requested data
            Timeout = timeout(Deadline),
            if
                0 < Timeout ->
                    %% Recv more
                    recv_deadline(
                      SockRef, Length - byte_size(Bin), Flags,
                      Deadline, [Bin | Buf]);
                true ->
                    recv_error([Bin | Buf], timeout)
            end;


        %%
        {error, Reason} ->
            recv_error(Buf, Reason)
    end.

recv_error([], Reason) ->
    {error, Reason};
recv_error(Buf, Reason) when is_list(Buf) ->
    {error, {Reason, condense_buffer(Buf)}}.


%% ---------------------------------------------------------------------------
%%
%% With recvfrom we get messages, which means that regardless of how
%% much we want to read, we return when we get a message.
%% The MaxSize argument basically defines the size of our receive
%% buffer. By setting the size to zero (0), we use the configured
%% size (see setopt).
%% It may be impossible to know what (buffer) size is appropriate
%% "in advance", and in those cases it may be convenient to use the
%% (recv) 'peek' flag. When this flag is provided the message is *not*
%% "consumed" from the underlying (OS) buffers, so another recvfrom call
%% is needed, possibly with a then adjusted buffer size.
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc "Equivalent to [`recvfrom(Socket, 0, [], infinity)`](`recvfrom/4`).".
-spec recvfrom(Socket :: term()) -> _.

recvfrom(Socket) ->
    recvfrom(
      Socket, 0, ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive a message on a socket.

With argument `BufSz`; equivalent to
[`recvfrom(Socket, BufSz, [], infinity)`](`recvfrom/4`).

With argument `Flags`; equivalent to
[`recvfrom(Socket, 0, Flags, infinity)`](`recvfrom/4`) *(since OTP 24.0)*.
""".
-spec recvfrom(Socket :: term(), Flags :: list()) -> _;
              (Socket :: term(), BufSz :: non_neg_integer()) -> _.

recvfrom(Socket, Flags) when is_list(Flags) ->
    recvfrom(Socket, 0, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz) when is_integer(BufSz), 0 =< BufSz ->
    recvfrom(Socket, BufSz,
             ?ESOCK_RECV_FLAGS_DEFAULT,
             ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz) ->
    erlang:error(badarg, [Socket, BufSz]).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive a message on a socket.

With arguments `BufSz` and `Flags`; equivalent to
[`recvfrom(Socket, BufSz, Flags, infinity)`](`recvfrom/4`).

With arguments `BufSz` and `TimeoutOrHandle`; equivalent to
[`recv(Socket, BufSz, [], TimeoutOrHandle)`](`recvfrom/4`).

With arguments `Flags` and `TimeoutOrHandle`; equivalent to
[`recv(Socket, 0, Flags, TimeoutOrHandle)`](`recvfrom/4`)

`TimeoutOrHandle :: 'nowait'` has been allowed *since OTP 22.1*.

`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.
""".
-spec recvfrom(Socket :: term(), Flags :: list(),
               TimeoutOrHandle :: term())
              -> _;
              (Socket :: term(), BufSz :: term(), Flags :: list())
              -> _;
              (Socket :: term(), BufSz :: term(),
               TimeoutOrHandle :: term())
              -> _.

recvfrom(Socket, Flags, TimeoutOrHandle) when is_list(Flags) ->
    recvfrom(Socket, 0, Flags, TimeoutOrHandle);
recvfrom(Socket, BufSz, Flags) when is_list(Flags) ->
    recvfrom(Socket, BufSz, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz, TimeoutOrHandle) ->
    recvfrom(Socket, BufSz, ?ESOCK_RECV_FLAGS_DEFAULT, TimeoutOrHandle).

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive a message on a socket.

This function is intended for sockets that are not connection
oriented such as type [`dgram`](`t:type/0`) or [`seqpacket`](`t:type/0`)
where it may arrive messages from different source addresses.

Argument `BufSz` specifies the number of bytes for the receive buffer.
If the buffer size is too small, the message will be truncated.

If `BufSz` is `0`, a default buffer size is used, which can be set by
[`socket:setopt(Socket, {otp,recvbuf}, BufSz)`](`setopt/3`).

If there is no known appropriate buffer size, it may be possible
to use the receive [message flag](`t:msg_flag/0`) `peek`.
When this flag is used, the message is _not_ "consumed" from
the underlying buffers, so another `recvfrom/1,2,3,4` call
is needed, possibly with an adjusted buffer size.

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s, as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

[](){: #recvfrom-infinity }

If the `Timeout` argument is `infinity`; waits for a message
to arrive, or for a socket error.

[](){: #recvfrom-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); returns `{error, timeout}`
if no message has arrived after `Timeout` milliseconds.

`Timeout = 0` only polls the OS receive call and doesn't
engage the Asynchronous Calls mechanisms.  If no message
is immediately available `{error, timeout}` is returned.

[](){: #recvfrom-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the 'Handle' argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*,
starts an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.
""".
-spec recvfrom(Socket, BufSz, Flags, Timeout :: 'infinity') ->
          {'ok', {Source, Data}} |
          {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid();

              (Socket, BufSz, Flags, Timeout :: non_neg_integer()) ->
          {'ok', {Source, Data}} |
          {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout';

              (Socket, BufSz, Flags, 'nowait' | Handle) ->
          {'ok', {Source, Data}} |
          {'select', SelectInfo} |
          {'completion', CompletionInfo} |
          {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Handle         :: select_handle() | completion_handle(),
      Source         :: sockaddr_recv(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid().

recvfrom(?socket(SockRef), BufSz, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(BufSz), 0 =< BufSz,
       is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            recvfrom_nowait(SockRef, BufSz, Handle, Flags);
        handle ->
            Handle = Timeout,
            recvfrom_nowait(SockRef, BufSz, Handle, Flags);
        zero ->
            case prim_socket:recvfrom(SockRef, BufSz, Flags, zero) of
                timeout ->
                    {error, timeout};
                Result ->
                    recvfrom_result(Result)
            end;
        Deadline ->
            recvfrom_deadline(SockRef, BufSz, Flags, Deadline)
    end;
recvfrom(Socket, BufSz, Flags, Timeout) ->
    erlang:error(badarg, [Socket, BufSz, Flags, Timeout]).

recvfrom_nowait(SockRef, BufSz, Handle, Flags) ->
    case prim_socket:recvfrom(SockRef, BufSz, Flags, Handle) of
        select = Tag ->
            {Tag, ?SELECT_INFO(recvfrom, Handle)};
        completion = Tag ->
            {Tag, ?COMPLETION_INFO(recvfrom, Handle)};
        Result ->
            recvfrom_result(Result)
    end.

recvfrom_deadline(SockRef, BufSz, Flags, Deadline) ->
    Handle = make_ref(),
    case prim_socket:recvfrom(SockRef, BufSz, Flags, Handle) of
        select ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), select, Handle) ->
                    recvfrom_deadline(SockRef, BufSz, Flags, Deadline);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvfrom, Handle),
                    {error, timeout}
            end;

        completion ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a completion message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, CompletionStatus}) ->
                    recvfrom_result(CompletionStatus);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvfrom, Handle),
                    {error, timeout}
            end;

        Result ->
            recvfrom_result(Result)
    end.

recvfrom_result(Result) ->
    case Result of
        {ok, {_Source, _NewData}} = OK ->
            OK;
        {error, _Reason} = ERROR ->
            ERROR
    end.


%% ---------------------------------------------------------------------------
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc "Equivalent to [`recvmsg(Socket, 0, 0, [], infinity)`](`recvmsg/5`).".
-spec recvmsg(Socket :: term()) -> _.

recvmsg(Socket) ->
    recvmsg(Socket, 0, 0,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive a message on a socket.

With argument `Flags`; equivalent to
[`recvmsg(Socket, 0, 0, Flags, infinity)`](`recvmsg/5`).

With argument `TimeoutOrHandle`; equivalent to
[`recvmsg(Socket, 0, 0, [], TimeoutOrHandle)`](`recvmsg/5`).

`TimeoutOrHandle :: nowait` has been allowed *since OTP 22.1*.

`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.
""".
-spec recvmsg(Socket :: term(), Flags :: list()) -> _;
             (Socket :: term(), TimeoutOrHandle :: term()) -> _.

recvmsg(Socket, Flags) when is_list(Flags) ->
    recvmsg(Socket, 0, 0, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvmsg(Socket, TimeoutOrHandle) ->
    recvmsg(Socket, 0, 0, ?ESOCK_RECV_FLAGS_DEFAULT, TimeoutOrHandle).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive a message on a socket.

With arguments  `Flags`; equivalent to
[`recvmsg(Socket, 0, 0, Flags, infinity)`](`recvmsg/5`).

With argument `TimeoutOrHandle`; equivalent to
[`recvmsg(Socket, 0, 0, [], TimeoutOrHandle)`](`recvmsg/5`).

`TimeoutOrHandle :: nowait` has been allowed *since OTP 22.1*.

`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.
""".
-spec recvmsg(Socket :: term(), Flags :: list(), TimeoutOrHandle :: term())
             -> _;
             (Socket :: term(), BufSz :: integer(), CtrlSz :: integer())
             -> _.

recvmsg(Socket, Flags, TimeoutOrHandle) when is_list(Flags) ->
    recvmsg(Socket, 0, 0, Flags, TimeoutOrHandle);
recvmsg(Socket, BufSz, CtrlSz) when is_integer(BufSz), is_integer(CtrlSz) ->
    recvmsg(Socket, BufSz, CtrlSz,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvmsg(Socket, BufSz, CtrlSz) ->
    error(badarg, [Socket, BufSz, CtrlSz]).

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Equivalent to
[`recvmsg(Socket, BufSz, CtrlSz, [], TimeoutOrHandle)`](`recvmsg/5`).
""".
-spec recvmsg(
        Socket :: term(), BufSz :: term(), CtrlSz :: term(),
        TimeoutOrHandle :: term())
             -> _.

recvmsg(Socket, BufSz, CtrlSz, TimeoutOrHandle) ->
    recvmsg(
      Socket, BufSz, CtrlSz, ?ESOCK_RECV_FLAGS_DEFAULT, TimeoutOrHandle).


-doc(#{since => <<"OTP 22.0">>}).
-doc """
Receive a message on a socket.

This function receives both data and control messages.

Arguments `BufSz` and `CtrlSz` specifies the number of bytes for the
receive buffer and the control message buffer. If the buffer size(s)
is(are) too small, the message and/or control message list will be truncated.

If `BufSz` is `0`, a default buffer size is used, which can be set by
[`socket:setopt(Socket, {otp,recvbuf}, BufSz)`](`setopt/3`).
The same applies to `CtrlSz` and
[`socket:setopt(Socket, {otp,recvctrlbuf}, CtrlSz)`](`setopt/3`).

If there is no known appropriate buffer size, it may be possible
to use the receive [message flag](`t:msg_flag/0`) `peek`.
When this flag is used, the message is _not_ "consumed" from
the underlying buffers, so another `recvfrom/1,2,3,4` call
is needed, possibly with an adjusted buffer size.

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s, as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

[](){: #recvmsg-infinity }

If the `Timeout` argument is `infinity`; waits for the message
to arrive, or for a socket error.

[](){: #recvmsg-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no message has arrived after `Timeout` milliseconds.

`Timeout = 0` only polls the OS receive call and doesn't
engage the Asynchronous Calls mechanisms.  If no message
is immediately available `{error, timeout}` is returned.

[](){: #recvmsg-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the 'Handle' argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*,
starts an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.
""".
-spec recvmsg(Socket, BufSz, CtrlSz, Flags, Timeout :: 'infinity') ->
          {'ok', Msg} |
          {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      CtrlSz  :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid();

             (Socket, BufSz, CtrlSz, Flags, Timeout :: non_neg_integer()) ->
          {'ok', Msg} |
          {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      CtrlSz  :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout';

             (Socket, BufSz, CtrlSz, Flags, 'nowait' | Handle) ->
          {'ok', Msg} |
          {'select', SelectInfo} |
          {'completion', CompletionInfo} |
          {'error', Reason} when
      Socket        :: socket(),
      BufSz          :: non_neg_integer(),
      CtrlSz         :: non_neg_integer(),
      Handle         :: select_handle() | completion_handle(),
      Flags          :: [msg_flag() | integer()],
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid().

recvmsg(?socket(SockRef), BufSz, CtrlSz, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(BufSz), 0 =< BufSz,
       is_integer(CtrlSz), 0 =< CtrlSz,
       is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags, Handle);
        handle ->
            Handle = Timeout,
            recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags, Handle);
        zero ->
            case prim_socket:recvmsg(SockRef, BufSz, CtrlSz, Flags, zero) of
                timeout = Tag ->
                    {error, Tag};
                Result ->
                    recvmsg_result(Result)
            end;
        Deadline ->
            recvmsg_deadline(SockRef, BufSz, CtrlSz, Flags, Deadline)
    end;
recvmsg(Socket, BufSz, CtrlSz, Flags, Timeout) ->
    erlang:error(badarg, [Socket, BufSz, CtrlSz, Flags, Timeout]).

recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags, Handle)  ->
    case prim_socket:recvmsg(SockRef, BufSz, CtrlSz, Flags, Handle) of
        select = Tag ->
            {Tag, ?SELECT_INFO(recvmsg, Handle)};
        completion = Tag ->
            {Tag, ?COMPLETION_INFO(recvmsg, Handle)};
        Result ->
            recvmsg_result(Result)
    end.

recvmsg_deadline(SockRef, BufSz, CtrlSz, Flags, Deadline)  ->
    Handle = make_ref(),
    case prim_socket:recvmsg(SockRef, BufSz, CtrlSz, Flags, Handle) of
        select = Tag ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), Tag, Handle) ->
                    recvmsg_deadline(
                      SockRef, BufSz, CtrlSz, Flags, Deadline);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvmsg, Handle),
                    {error, timeout}
            end;

        completion = Tag ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a completion message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), Tag,
                            {Handle, CompletionStatus}) ->
                    recvmsg_result(CompletionStatus);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvmsg, Handle),
                    {error, timeout}
            end;

        Result ->
            recvmsg_result(Result)
    end.

recvmsg_result(Result) ->
    %% ?DBG([{result, Result}]),
    case Result of
        {ok, _Msg} = OK ->
            OK;
        {error, _Reason} = ERROR ->
            ERROR
    end.


%% ===========================================================================
%%
%% close - close a file descriptor
%%
%% Closing a socket is a two stage rocket (because of linger).
%% We need to perform the actual socket close while in BLOCKING mode.
%% But that would hang the entire VM, so what we do is divide the
%% close in two steps:
%% 1) prim_socket:nif_close + the socket_stop (nif) callback function
%%    This is for everything that can be done safely NON-BLOCKING.
%% 2) prim_socket:nif_finalize_close which is executed by a *dirty* scheduler
%%    Before we call the socket close function, we set the socket
%%    BLOCKING. Thereby linger is handled properly.

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Close a socket.

> #### Note {: .info }
>
> Note that for `Protocol = tcp` (see `open/3`), although
> TCP guarantees that when the other side sees the stream close
> all data that we sent before closing has been delivered,
> there is no way for us to know that the other side got all data
> and the stream close.  All kinds of network and OS issues
> may obliterate that.
>
> To get such a guarantee we need to implement an in-band acknowledge
> protocol on the connection, or we can use the [`shutdown`](`shutdown/2`)
> function to signal that no more data will be sent and then wait
> for the other end to close the socket.  Then we will see our read side
> getting a socket close.  In this way we implement a small
> acknowledge protocol using `shutdown/2`.  The other side cannot
> know that we ever saw the socket close, but in a client/server
> scenario that is often not relevant.
""".
-spec close(Socket) -> 'ok' | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed' | 'timeout'.

close(?socket(SockRef))
  when is_reference(SockRef) ->
    case prim_socket:close(SockRef) of
        ok ->
            prim_socket:finalize_close(SockRef);
        {ok, CloseRef} ->
            %% We must wait for the socket_stop callback function to
            %% complete its work
            receive
                ?socket_msg(?socket(SockRef), close, CloseRef) ->
                    prim_socket:finalize_close(SockRef)
            end;
        {error, _} = ERROR ->
            ERROR
    end;
close(Socket) ->
    erlang:error(badarg, [Socket]).



%% ===========================================================================
%%
%% shutdown - shut down part of a full-duplex connection
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc "Shut down all or part of a full-duplex connection.".
-spec shutdown(Socket, How) -> 'ok' | {'error', Reason} when
      Socket :: socket(),
      How    :: 'read' | 'write' | 'read_write',
      Reason :: posix() | 'closed'.

shutdown(?socket(SockRef), How)
  when is_reference(SockRef) ->
    prim_socket:shutdown(SockRef, How);
shutdown(Socket, How) ->
    erlang:error(badarg, [Socket, How]).


%% ===========================================================================
%%
%% setopt - manipulate individual properties of a socket
%%
%% What properties are valid depend on what kind of socket it is
%% (domain, type and protocol)
%% If its an "invalid" option (or value), we should not crash but return some
%% useful error...
%%
%% <KOLLA>
%%
%% WE NEED TO MAKE SURE THAT THE USER DOES NOT MAKE US BLOCKING
%% AS MUCH OF THE CODE EXPECTS TO BE NON-BLOCKING!!
%%
%% </KOLLA>

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Set a socket option.

Set an OS protocol level option, or an `otp` pseudo protocol level option.
The latter level is this module's implementation level above
the OS protocol levels.

See the type [otp_socket_option() ](`t:otp_socket_option/0`)
for a description of the `otp` protocol level.

See the type `t:socket_option/0` for which OS protocol level options
that this implementation knows about, how they are related to OS option names,
and if there are known peculiarities with any of them.

What options that are valid depends on the OS, and on the kind of socket
(`t:domain/0`,`t:type/0` and `t:protocol/0`).  See the type
`t:socket_option()` and the
[socket options ](socket_usage.md#socket_options) chapter
in the User's Guide for more info.

> #### Note {: .info }
>
> Not all options are valid, nor possible to set, on all platforms. That is,
> even if this `socket` implementation  support an option; it doesn't mean
> that the underlying OS does.
""".
-spec setopt(Socket, SocketOption, Value) ->
          ok | {'error', invalid() | 'closed'}
              when
      Socket       :: socket(),
      SocketOption :: {Level :: 'otp', Opt :: otp_socket_option()},
      Value        :: term();

            (Socket, SocketOption, Value) ->
          ok | {'error', posix() | invalid() | 'closed'}
              when
      Socket       :: socket(),
      SocketOption :: socket_option(),
      Value        :: term().

setopt(?socket(SockRef), SocketOption, Value)
  when is_reference(SockRef) ->
    prim_socket:setopt(SockRef, SocketOption, Value);
setopt(Socket, SocketOption, Value) ->
    erlang:error(badarg, [Socket, SocketOption, Value]).


%% Backwards compatibility
-doc(#{since => <<"OTP 22.0">>}).
-doc """
Set a socket option _(backwards compatibility function)_.

Equivalent to [`setopt(Socket, {Level, Opt}, Value)`](`setopt/3`),
or as a special case if `Opt = NativeOpt ::` `t:integer/0`
and `Value =` `t:binary/0` equivalent to
[`setopt_native(Socket, {Level, NativeOpt}, ValueSpec)`](`setopt_native/3`).

Use `setopt/3` or `setopt_native/3` instead to handle
the option level and name as a single term, and to make the
difference between known options and native options clear.
""".
-spec setopt(socket(), Level :: term(), Opt :: term(), Value :: term()) -> _.

setopt(Socket, Level, NativeOpt, Value)
  when is_integer(NativeOpt), is_binary(Value) ->
    setopt_native(Socket, {Level,NativeOpt}, Value);
setopt(Socket, Level, Opt, Value) ->
    setopt(Socket, {Level,Opt}, Value).


-doc(#{since => <<"OTP 24.0">>}).
-doc """
Set a "native" socket option.

Sets a socket option that may be unknown to our implementation, or that has a
type not compatible with our implementation, that is; in "native mode".

If `Value` is an `t:integer/0` it will be used as a `C` type `(int)`,
if it is a `t:boolean/0` it will be used as a `C` type `(int)`
with the `C` implementations values for `false` or `true`,
and if it is a `t:binary/0` its content and size will be used
as the option value.

The socket option may be specified with an ordinary
`t:socket_option/0` tuple, with a symbolic `Level` as
`{`[`Level :: level/0`](`t:level/0`)`, `[`NativeOpt :: integer/0`](`t:integer/0`)`}`,
or with integers for both `NativeLevel` and `NativeOpt` as
`{`[`NativeLevel :: integer/0`](`t:integer/0`)`, `[`NativeOpt :: integer/0`](`t:integer/0`)`}`.

If an option is valid depends both on the platform and on
what kind of socket it is (`t:domain/0`, `t:type/0` and `t:protocol/0`).

The integer values for `NativeLevel` and `NativeOpt` as well as the `Value`
encoding has to be deduced from the header files for the running system.
""".
-spec setopt_native(Socket, Option, Value) ->
          'ok' | {'error', posix() | invalid() | 'closed'}
              when
      Socket :: socket(),
      Option ::
        socket_option() | {Level, NativeOpt} | {NativeLevel, NativeOpt},
      Value       :: native_value(),
      Level       :: level(),
      NativeLevel :: integer(),
      NativeOpt   :: integer().

setopt_native(?socket(SockRef), SocketOption, Value)
  when is_reference(SockRef) ->
    prim_socket:setopt_native(SockRef, SocketOption, Value);
setopt_native(Socket, SocketOption, Value) ->
    erlang:error(badarg, [Socket, SocketOption, Value]).


%% ===========================================================================
%%
%% getopt - retrieve individual properties of a socket
%%
%% What properties are valid depend on what kind of socket it is
%% (domain, type and protocol).
%% If its an "invalid" option, we should not crash but return some
%% useful error...
%%
%% When specifying level as an integer, and therefore using "native mode",
%% we should make it possible to specify common types instead of the
%% value size. Example: int | bool | {string, pos_integer()} | non_neg_integer()
%%

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Get the value of a socket option.

Gets the value of an OS protocol level socket option, or from
the `otp` pseudo protocol level, which is this module's
implementation level above the OS protocol levels.

See the type [otp_socket_option() ](`t:otp_socket_option/0`)
for a description of the `otp` protocol level.

See the type `t:socket_option/0` for which OS protocol level options
that this implementation knows about, how they are related to OS option names,
and if there are known peculiarities with any of them.

What options that are valid depends on the OS, and on the kind of socket
(`t:domain/0`,`t:type/0` and `t:protocol/0`).  See the type
`t:socket_option()` and the
[socket options ](socket_usage.md#socket_options) chapter
in the User's Guide for more info.

> #### Note {: .info }
>
> Not all options are valid, nor possible to get, on all platforms. That is,
> even if this `socket` implementation  support an option; it doesn't mean
> that the underlying OS does.
""".
-spec getopt(socket(),
             SocketOption ::
               {Level :: 'otp',
                Opt :: otp_socket_option()}) ->
                    {'ok', Value :: term()} |
                    {'error', invalid() | 'closed'};
            (socket(),
             SocketOption :: socket_option()) ->
                    {'ok', Value :: term()} |
                    {'error', posix() | invalid() | 'closed'}.

getopt(?socket(SockRef), SocketOption)
  when is_reference(SockRef) ->
    prim_socket:getopt(SockRef, SocketOption).

%% Backwards compatibility
-doc(#{since => <<"OTP 22.0">>}).
-doc """
Get a socket option _(backwards compatibility function)_.

Equivalent to [`getopt(Socket, {Level, Opt})`](`getopt/2`),
or as a special case if
`Opt = {NativeOpt :: `[`integer/0`](`t:integer/0`)`, ValueSpec}`
equivalent to
[`getopt_native(Socket, {Level, NativeOpt}, ValueSpec)`](`getopt_native/3`).

Use `getopt/2` or `getopt_native/3` instead to handle
the option level and name as a single term, and to make the
difference between known options and native options clear.
""".
-spec getopt(Socket :: term(), Level :: term(), Opt :: term()) -> _.
getopt(Socket, Level, {NativeOpt, ValueSpec})
  when is_integer(NativeOpt) ->
    getopt_native(Socket, {Level,NativeOpt}, ValueSpec);
getopt(Socket, Level, Opt) ->
    getopt(Socket, {Level,Opt}).

-doc(#{since => <<"OTP 24.0">>}).
-doc """
Get a "native" socket option.

Gets a socket option that may be unknown to our implementation, or that has a
type not compatible with our implementation, that is; in "native mode".

The socket option may be specified with an ordinary
[`socket_option()` ](`t:socket_option/0`) tuple, with a known
[`Level = level()` ](`t:level/0`) and an integer `NativeOpt`,
or with both an integer `NativeLevel` and `NativeOpt`.

How to decode the option value has to be specified either with `ValueType`,
by specifying the `ValueSize` for a `t:binary/0` that will contain the fetched
option value, or by specifying a `t:binary/0` `ValueSpec` that will be copied
to a buffer for the `getsockopt()` call to write the value in which will be
returned as a new `t:binary/0`.

If `ValueType` is `integer` a `C` type `(int)` will be fetched, if it is
`boolean` a `C` type `(int)` will be fetched and converted into a `t:boolean/0`
according to the `C` implementation's notion about true and false.

If an option is valid depends both on the platform and on
what kind of socket it is (`t:domain/0`, `t:type/0` and `t:protocol/0`).

The integer values for `NativeLevel` and `NativeOpt` as well as the `Value`
encoding has to be deduced from the header files for the running system.
""".
-spec getopt_native(socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level() | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueType :: 'integer') ->
          {'ok', Value :: integer()} |
          {'error', posix() | invalid() | 'closed'};
                   (socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level() | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueType :: 'boolean') ->
          {'ok', Value :: boolean()} |
          {'error', posix() | invalid() | 'closed'};
                   (socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level() | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueSize :: non_neg_integer()) ->
          {'ok', Value :: binary()} |
          {'error', posix() | invalid() | 'closed'};
                   (socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level() | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueSpec :: binary()) ->
          {'ok', Value :: binary()} |
          {'error', posix() | invalid() | 'closed'}.
%% Compare ValueType, ValueSpec and ValueSize to native_value()
%% which are the types valid to setopt_native

getopt_native(?socket(SockRef), SocketOption, ValueSpec) ->
    prim_socket:getopt_native(SockRef, SocketOption, ValueSpec).


%% ===========================================================================
%%
%% sockname - return the current address of the socket.
%%
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Get the socket's address.

Returns the address to which the socket is currently bound.
If the bind address had the wildcard port `0`,
the address returned by this function contains the ephemeral port
selected by the OS.
""".
-spec sockname(Socket :: socket()) ->
          {'ok', SockAddr} | {'error', Reason} when
      SockAddr :: sockaddr_recv(),
      Reason   :: posix() | 'closed'.

sockname(?socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:sockname(SockRef);
sockname(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% peername - return the address of the peer *connected* to the socket.
%%
%%

-doc(#{since => <<"OTP 22.0">>}).
-doc """
Return the remote address of a socket.

Returns the address of the connected peer, that is,
the remote end of the socket.
""".
-spec peername(Socket :: socket()) ->
          {'ok', SockAddr} | {'error', Reason} when
      SockAddr :: sockaddr_recv(),
      Reason   :: posix() | 'closed'.

peername(?socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:peername(SockRef);
peername(Socket) ->
    erlang:error(badarg, [Socket]).



%% ===========================================================================
%%
%% ioctl - control device - get requests
%%
%%

-doc(#{since => <<"OTP 24.2">>}).
-doc """
Set socket (device) parameters.

This function retrieves a specific parameter, according to
the `GetRequest` argument.

- **`gifconf`** - Get a list of interface (transport layer) addresses.

  Result; a list of `t:map/0`s, one for each interface,
  with its name and address.

- **`nread`** - Get the number of bytes immediately available for reading
  *(since OTP 26.1)*.

  Result; the number of bytes, `t:integer/0`.

- **`nwrite`** - Get the number of bytes in the send queue
  *(since OTP 26.1)*.

  Result; the number of bytes, `t:integer/0`.

- **`nspace`** - Get the free space in the send queue
  *(since OTP 26.1)*.

  Result; the number of bytes, `t:integer/0`.

- **`atmark`** - Test if there is OOB (out-of-bound) data waiting to be read
  *(since OTP 26.1)*.

  Result; a `t:boolean/0`.

- **`tcp_info`** - Get miscellaneous TCP related information for a
  _connected_ socket *(since OTP 26.1)*.

  Result; a `t:map/0` with information items as key-value pairs.

> #### Note {: .info }
>
> Not all requests are supported by all platforms.
> To see if a ioctl request is supported on the current platform:
>
> ```erlang
> 	    Request = nread,
> 	    true = socket:is_supported(ioctl_requests, Request),
> 	    :
> ```
""".
-spec ioctl(Socket, GetRequest :: 'gifconf') ->
          {'ok', IFConf :: [#{name := string, addr := sockaddr()}]} |
          {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed';

           (Socket, GetRequest :: 'nread' | 'nwrite' | 'nspace') ->
          {'ok', NumBytes :: non_neg_integer()} | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed';

           (Socket, GetRequest :: 'atmark') ->
          {'ok', Available :: boolean()} | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed';

           (Socket, GetRequest :: 'tcp_info') ->
          {'ok', Info :: map()} | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed'.

%% gifconf | nread | nwrite | nspace | atmark |
%% {gifaddr, string()} | {gifindex, string()} | {gifname, integer()}
ioctl(?socket(SockRef), gifconf = GetRequest) ->
    prim_socket:ioctl(SockRef, GetRequest);
ioctl(?socket(SockRef), GetRequest) when (nread =:= GetRequest) orelse
                                         (nwrite =:= GetRequest) orelse
                                         (nspace =:= GetRequest) ->
    prim_socket:ioctl(SockRef, GetRequest);
ioctl(?socket(SockRef), GetRequest) when (atmark =:= GetRequest) ->
    prim_socket:ioctl(SockRef, GetRequest);
ioctl(Socket, GetRequest) when (tcp_info =:= GetRequest) ->
    ioctl(Socket, GetRequest, 0);
ioctl(Socket, GetRequest) ->
    erlang:error(badarg, [Socket, GetRequest]).

%% -spec ioctl(Socket, GetRequest, Index) -> {'ok', Name} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifname',
%%       Index      :: integer(),
%%       Name       :: string(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Index} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifindex',
%%       Name       :: string(),
%%       Index      :: integer(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Addr} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifaddr',
%%       Name       :: string(),
%%       Addr       :: sockaddr(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', DestAddr} | {'error', Reason} when
%%       Socket      :: socket(),
%%       GetRequest  :: 'gifdstaddr',
%%       Name        :: string(),
%%       DestAddr    :: sockaddr(),
%%       Reason      :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', BroadcastAddr} | {'error', Reason} when
%%       Socket        :: socket(),
%%       GetRequest    :: 'gifbrdaddr',
%%       Name          :: string(),
%%       BroadcastAddr :: sockaddr(),
%%       Reason        :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Netmask} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifnetmask',
%%       Name       :: string(),
%%       Netmask    :: sockaddr(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', HWAddr} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifhwaddr',
%%       Name       :: string(),
%%       HWAddr     :: sockaddr(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', GenAddr} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'genaddr',
%%       Name       :: string(),
%%       GenAddr    :: binary(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', MTU} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifmtu',
%%       Name       :: string(),
%%       MTU        :: integer(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', TransmitQLen} | {'error', Reason} when
%%       Socket       :: socket(),
%%       GetRequest   :: 'giftxqlen',
%%       Name         :: string(),
%%       TransmitQLen :: integer(),
%%       Reason       :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Flags} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifflags',
%%       Name       :: string(),
%%       Flags      :: [ioctl_device_flag() | integer()],
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', DevMap} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifmap',
%%       Name       :: string(),
%%       DevMap     :: ioctl_device_map(),
%%       Reason     :: posix() | 'closed'.

-doc(#{since => <<"OTP 24.2">>}).
-doc """
Get or set socket (device) parameters.

[](){: #ioctl-get }

This function retrieves a specific parameter, according to
one of the following `GetRequest` arguments. The third argument is
the (lookup) "key", identifying the interface, for most requests
the name of the interface as a `t:string/0`.
Also, see the note above.

- **`gifname`** - Get the name of the interface with the specified index
  (`t:integer/0`).

  Result; the name of the interface, `t:string/0`.

- **`gifindex`** - Get the index of the interface with the specified name.

  Result; the interface index, `t:integer/0`.

- **`gifaddr`** - Get the address of the interface with the specified name.

  Result; the address of the interface, `t:sockaddr/0`.

- **`gifdstaddr`** - Get the destination address of the point-to-point
  interface with the specified name.

  Result; the destination address of the interface, `t:sockaddr/0`.

- **`gifbrdaddr`** - Get the broadcast address of the interface with the
  specified name.

  Result; broadcast address of the interface, `t:sockaddr/0`.

- **`gifnetmask`** - Get the network mask of the interface with
  the specified name.

  Result; the network mask of the interface, `t:sockaddr/0`.

- **`gifhwaddr` | `genaddr`** - Get the hardware address for the interface with the
  specified name.

  Result; the hardware address of the interface, `t:sockaddr/0` | `t:binary/0`.
  The family field contains the 'ARPHRD' device type (or an integer).

- **`gifmtu`** - Get the MTU (Maximum Transfer Unit) for the interface with the
  specified name.

  Result; MTU of the interface, `t:integer/0`.

- **`giftxqlen`** - Get the transmit queue length of the interface with the
  specified name.

  Result; transmit queue length of the interface, `t:integer/0`.

- **`gifflags`** - Get the active flag word of the interface
  with the specified name.

  Result; the active flag word of the interface, is a list of
  `t:ioctl_device_flag/0` `|` `t:integer( )`.

[](){: #ioctl-set }

With the following `SetRequest` argument this function sets
the `Value` for the request parameter *(since OTP 26.1)*.

- **`rcvall`** - Enables (or disables) a socket to receive all IPv4 or IPv6
  packages passing through a network interface.

  The `Socket` has to be one of:

  - **An IPv4 socket** - Created with the address
    [domain `inet`](`t:domain/0`), socket [type `raw`](`t:type/0`)
    and [protocol `ip`](`t:protocol/0`).

  - **An IPv6 socket** - Created with the address
    [domain `inet6`](`t:domain/0`), socket [type `raw`](`t:type/0`)
    and [protocol `ipv6`](`t:protocol/0`).

  The socket must also be bound to an (explicit) local IPv4 or IPv6 interface
  (`any` isn't allowed).

  Setting this IOCTL requires elevated privileges.

With the following `SetRequest` arguments this function sets
the `Value` for the request parameter *(since OTP 26.1)*.

- **`rcvall_igmpmcall`** - Enables (or disables) a socket to receive IGMP
  multicast IP traffic, _without_ receiving any other IP traffic.

  The socket has to be created with the address
  [domain `inet`](`t:domain/0`), socket [type `raw`](`t:type/0`)
  and [protocol `igmp`](`t:protocol/0`).

  The socket must also be bound to an (explicit) local interface
  (`any` isn't allowed).

  The receive buffer must be sufficiently large.

  Setting this IOCTL requires elevated privileges.

- **`rcvall_mcall`** - Enables (or disables) a socket to receive all multicast
  IP traffic (as in; all IP packets destined for IP addresses in the range
  224.0.0.0 to 239.255.255.255).

  The socket has to be created with the address
  [domain `inet`](`t:domain/0`), socket [type `raw`](`t:type/0`)
  and [protocol `udp`](`t:protocol/0`).

  The socket must also be bound to an (explicit) local interface
  (`any` isn't allowed), And bound to port `0`.

  The receive buffer must be sufficiently large.

  Setting this IOCTL requires elevated privileges.
""".
-spec ioctl(Socket, GetRequest, NameOrIndex) -> {'ok', Result} | {'error', Reason} when
      Socket      :: socket(),
      GetRequest  :: 'gifname' | 'gifindex' |
                     'gifaddr' | 'gifdstaddr' | 'gifbrdaddr' |
                     'gifnetmask' | 'gifhwaddr' | 'genaddr' |
                     'gifmtu' | 'giftxqlen' | 'gifflags' |
		     'tcp_info',
      NameOrIndex :: string() | integer(),
      Result      :: term(),
      Reason      :: posix() | 'closed';
	   (Socket, SetRequest, Value) -> ok | {'error', Reason} when
      Socket     :: socket(),
      SetRequest :: 'rcvall',
      Value      :: off | on | iplevel,
      Reason     :: posix() | 'closed';
	   (Socket, SetRequest, Value) -> ok | {'error', Reason} when
      Socket     :: socket(),
      SetRequest :: 'rcvall_igmpmcast' | 'rcvall_mcast',
      Value      :: off | on,
      Reason     :: posix() | 'closed'.

ioctl(?socket(SockRef), gifname = GetRequest, Index)
  when is_integer(Index) ->
    prim_socket:ioctl(SockRef, GetRequest, Index);
ioctl(?socket(SockRef), gifindex = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifdstaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifbrdaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifnetmask = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifmtu = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifhwaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), genaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), giftxqlen = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifflags = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifmap = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);

ioctl(?socket(SockRef), tcp_info = GetRequest, Version)
  when (Version =:= 0) ->
    prim_socket:ioctl(SockRef, GetRequest, Version);

ioctl(?socket(SockRef), rcvall = SetRequest, Value)
  when (Value =:= off) orelse
       (Value =:= on)  orelse
       (Value =:= iplevel) ->
    prim_socket:ioctl(SockRef, SetRequest, Value);
ioctl(?socket(SockRef), SetRequest, Value)
  when ((SetRequest =:= rcvall_igmpmcast) orelse
        (SetRequest =:= rcvall_mcast)) andalso
       ((Value =:= off) orelse
        (Value =:= on)) ->
    prim_socket:ioctl(SockRef, SetRequest, Value);

ioctl(Socket, Request, Arg) ->
    erlang:error(badarg, [Socket, Request, Arg]).


-doc(#{since => <<"OTP 24.2">>}).
-doc """
Set socket (device) parameters.

This function sets a specific parameter, according to the `SetRequest`
argument. The `Name` argument is the name of the interface,
and the `Value` argument is the value to set.

These operations require elevated privileges.

- **`sifflags`** - Set the the active flag word, `#{Flag => boolean()}`, of the
  interface with the specified name.

  Each flag to be changed should be added to the value `t:map/0`,
  with the value `true` if the `Flag` should be set and `false`
  if the flag should be cleared.

- **`sifaddr`** - Set the address, `t:sockaddr/0`, of the interface with the
  specified name.

- **`sifdstaddr`** - Set the destination address, `t:sockaddr/0`, of a
  point-to-point interface with the specified name.

- **`sifbrdaddr`** - Set the broadcast address, `t:sockaddr/0`,
of the interface with the specified name.

- **`sifnetmask`** - Set the network mask, `t:sockaddr/0`, of the interface
  with the specified name.

- **`sifhwaddr`** - Set the hardware address, `t:sockaddr/0`,
of the interface with the specified name.

- **`sifmtu`** - Set the MTU (Maximum Transfer Unit), `t:integer/0`,
  for the interface with the specified name.

- **`siftxqlen`** - Set the transmit queue length, `t:integer/0`,
  of the interface with the specified name.
""".
-spec ioctl(Socket, SetRequest, Name, Value) -> 'ok' | {'error', Reason} when
      Socket     :: socket(),
      SetRequest :: 'sifflags' |
                    'sifaddr' | 'sifdstaddr' | 'sifbrdaddr' |
                    'sifnetmask' | 'sifhwaddr' |
                    'sifmtu' | 'siftxqlen',
      Name       :: string(),
      Value      :: term(),
      Reason     :: posix() | 'closed'.

ioctl(?socket(SockRef), sifflags = SetRequest, Name, Flags)
  when is_list(Name) andalso is_map(Flags) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, Flags);
ioctl(?socket(SockRef), sifaddr = SetRequest, Name, Addr)
  when is_list(Name) andalso is_map(Addr) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(Addr));
ioctl(?socket(SockRef), sifdstaddr = SetRequest, Name, DstAddr)
  when is_list(Name) andalso is_map(DstAddr) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(DstAddr));
ioctl(?socket(SockRef), sifbrdaddr = SetRequest, Name, BrdAddr)
  when is_list(Name) andalso is_map(BrdAddr) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(BrdAddr));
ioctl(?socket(SockRef), sifnetmask = SetRequest, Name, NetMask)
  when is_list(Name) andalso is_map(NetMask) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(NetMask));
ioctl(?socket(SockRef), sifhwaddr = SetRequest, Name, HWAddr)
  when is_list(Name) andalso is_map(HWAddr) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(HWAddr));
ioctl(?socket(SockRef), sifmtu = SetRequest, Name, MTU)
  when is_list(Name) andalso is_integer(MTU) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, MTU);
ioctl(?socket(SockRef), siftxqlen = SetRequest, Name, QLen)
  when is_list(Name) andalso is_integer(QLen) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, QLen);
ioctl(Socket, SetRequest, Arg1, Arg2) ->
    erlang:error(badarg, [Socket, SetRequest, Arg1, Arg2]).


%% ===========================================================================
%%
%% cancel - cancel an operation resulting in a select
%%
%% A call to accept, recv/recvfrom/recvmsg and send/sendto/sendmsg
%% can result in a select if they are called with the Timeout argument
%% set to nowait. This is indicated by the return of the select-info.
%% Such a operation can be canceled by calling this function.
%%

-doc(#{since => <<"OTP 22.1">>}).
-doc """
Cancel an asynchronous call in progress.

Call this function to cancel an [asynchronous call](#asynchronous-calls)
in progress, that is; it returned a value containing
a `t:completion_info/0` or `t:select_info/0`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

If another process tries an operation of the same basic type
(`accept/1` | `send/2` | `recv/2`) it will be enqueued and notified
through a `select` or `completion` message when the current operation
and all enqueued before it has been completed. If the current operation
is canceled by this function it is treated as a completed operation;
the process first in queue is notified.

If [`SelectInfo`](`t:select_info/0`) `|`
[`CompletionInfo`](`t:completion_info/0`) does not match
an operation in progress for the calling process, this function returns
`{error, {invalid, SelectInfo | CompletionInfo}}`.
""".
-spec cancel(Socket, SelectInfo | CompletionInfo)->
          'ok' | {'error', Reason} when
      Socket         :: socket(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: 'closed' | invalid().

cancel(?socket(SockRef), ?SELECT_INFO(SelectTag, SelectHandle) = SelectInfo)
  when is_reference(SockRef) ->
    case SelectTag of
        {Op, _} when is_atom(Op) ->
            ok;
        Op when is_atom(Op) ->
            ok
    end,
    case cancel(SockRef, Op, SelectHandle) of
        ok ->
            ok;
        invalid ->
            {error, {invalid, SelectInfo}};
        Result ->
            Result
    end;
cancel(?socket(SockRef),
       ?COMPLETION_INFO(CompletionTag, CompletionHandle) = CompletionInfo)
  when is_reference(SockRef) ->
    case CompletionTag of
        {Op, _} when is_atom(Op) ->
            ok;
        Op when is_atom(Op) ->
            ok
    end,
    case cancel(SockRef, Op, CompletionHandle) of
        ok ->
            ok;
        invalid ->
            {error, {invalid, CompletionInfo}};
        Result ->
            Result
    end;
cancel(Socket, Info) ->
    erlang:error(badarg, [Socket, Info]).


cancel(SockRef, Op, Handle) ->
    case prim_socket:cancel(SockRef, Op, Handle) of
        select_sent ->
            _ = flush_select_msg(SockRef, Handle),
            _ = flush_abort_msg(SockRef, Handle),
            ok;
        not_found ->
            _ = flush_completion_msg(SockRef, Handle),
            _ = flush_abort_msg(SockRef, Handle),
            invalid;
        Result ->
            %% Since we do not actually know if we are using
            %% select or completion here, so flush both...
            _ = flush_select_msg(SockRef, Handle),
            _ = flush_completion_msg(SockRef, Handle),
            _ = flush_abort_msg(SockRef, Handle),
	    %% ?DBG([{op, Op}, {result, Result}]),
            Result
    end.

flush_select_msg(SockRef, Ref) ->
    receive
        ?socket_msg(?socket(SockRef), select, Ref) ->
            ok
    after 0 ->
            ok
    end.

flush_completion_msg(SockRef, Ref) ->
    receive
        ?socket_msg(?socket(SockRef), completion, {Ref, Result}) ->
            Result
    after 0 ->
            ok
    end.

flush_abort_msg(SockRef, Ref) ->
    receive
        ?socket_msg(?socket(SockRef), abort, {Ref, Reason}) ->
            Reason
    after 0 ->
            ok
    end.


%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

deadline(Timeout) ->
    case Timeout of
        nowait ->
            Timeout;
        infinity ->
            Timeout;
        Handle when is_reference(Handle) ->
            handle;
        0 ->
            zero;
        _ when is_integer(Timeout), 0 < Timeout ->
            timestamp() + Timeout;
        _ ->
            invalid
    end.

timeout(Deadline) ->
    case Deadline of
        %% nowait | handle shall not be passed here
        %%
        infinity ->
            Deadline;
        zero ->
            0;
        _ ->
            Now = timestamp(),
            if
                Deadline > Now ->
                    Deadline - Now;
                true ->
                    0
            end
    end.

timestamp() ->
    erlang:monotonic_time(milli_seconds).


f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

%% mq() ->
%%     pi(messages).

%% pi(Item) ->
%%     {Item, Val} = process_info(self(), Item),
%%     Val.

%% formatted_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(Now) ->
%%     N2T = fun(N) -> calendar:now_to_local_time(N) end,
%%     format_timestamp(Now, N2T, true).

%% format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
%%     FormatExtra = ".~.2.0w",
%%     ArgsExtra   = [N3 div 10000],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra);
%% format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
%%     FormatExtra = "",
%%     ArgsExtra   = [],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra).

%% format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
%%     {Date, Time}   = N2T(N),
%%     {YYYY,MM,DD}   = Date,
%%     {Hour,Min,Sec} = Time,
%%     FormatDate =
%%         io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
%%                       [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
%%     lists:flatten(FormatDate).

%% p(F) ->
%%     p(F, []).

%% p(F, A) ->
%%     p(get(sname), F, A).

%% p(undefined, F, A) ->
%%     p("***", F, A);
%% p(SName, F, A) ->
%%     TS = formatted_timestamp(),
%%     io:format(user,"[~s][~s,~p] " ++ F ++ "~n", [TS, SName, self()|A]),
%%     io:format("[~s][~s,~p] " ++ F ++ "~n", [TS, SName, self()|A]).
