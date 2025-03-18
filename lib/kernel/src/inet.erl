%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
-module(inet).
-moduledoc """
Access to Network protocols.

This module, together with `m:gen_tcp`, `m:gen_udp` and `m:gen_sctp`
provides access to the Network protocols TCP, SCTP and UDP over IP,
as well as stream and datagram protocols over the local (unix)
address domain / protocol domain.

See also [ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`)
or more information about how to configure an Erlang runtime system
for IP communication.

The following two Kernel configuration parameters affect the behavior of all
`m:gen_tcp` sockets opened on an Erlang node:

- `inet_default_connect_options` can contain a list of
  default options used for all sockets created by
  a `gen_tcp:connect/2,3,4`](`gen_tcp:connect/2`) call.
- `inet_default_listen_options` can contain a list of default options
  used for sockets created by a `gen_tcp:listen/2` call.

For the [`gen_tcp:accept/1,2`](`gen_tcp:accept/1`) call,
the values of the listening socket options are inherited.
Therefore there is no corresponding application variable for `accept`.

Using the Kernel configuration parameters above, one can set default options
for all TCP sockets on a node, but use this with care. Options such as
`{delay_send,true}` can be specified in this way. The following is an example
of starting an Erlang node with all sockets using delayed send:

```text
$ erl -sname test -kernel \
inet_default_connect_options '[{delay_send,true}]' \
inet_default_listen_options '[{delay_send,true}]'
```

**Please note** that the default option `{active, true}` cannot be changed,
for internal implementation reasons.

Addresses as inputs to functions can be either a string or a tuple.
For example, the IP address 150.236.20.73 can be passed to
`gethostbyaddr/1`, either as a string `"150.236.20.73"`
or as a tuple `{150, 236, 20, 73}`.

_IPv4 address examples:_

```text
Address          ip_address()
-------          ------------
127.0.0.1        {127,0,0,1}
192.168.42.2     {192,168,42,2}
```

_IPv6 address examples:_

```erlang
Address          ip_address()
-------          ------------
::1             {0,0,0,0,0,0,0,1}
::192.168.42.2  {0,0,0,0,0,0,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
::FFFF:192.168.42.2
                {0,0,0,0,0,16#FFFF,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
3ffe:b80:1f8d:2:204:acff:fe17:bf38
                {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38}
fe80::204:acff:fe17:bf38
                {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38}
```

Function `parse_address/1` can be useful:

```erlang
1> inet:parse_address("192.168.42.2").
{ok,{192,168,42,2}}
2> inet:parse_address("::FFFF:192.168.42.2").
{ok,{0,0,0,0,0,65535,49320,10754}}
```

[](){: #posix-error-codes } POSIX Error Codes
---------------------------------------------

- `e2big` - Too long argument list
- `eacces` - Permission denied
- `eaddrinuse` - Address already in use
- `eaddrnotavail` - Cannot assign requested address
- `eadv` - Advertise error
- `eafnosupport` - Address family not supported by protocol family
- `eagain` - Resource temporarily unavailable
- `ealign` - EALIGN
- `ealready` - Operation already in progress
- `ebade` - Bad exchange descriptor
- `ebadf` - Bad file number
- `ebadfd` - File descriptor in bad state
- `ebadmsg` - Not a data message
- `ebadr` - Bad request descriptor
- `ebadrpc` - Bad RPC structure
- `ebadrqc` - Bad request code
- `ebadslt` - Invalid slot
- `ebfont` - Bad font file format
- `ebusy` - File busy
- `echild` - No children
- `echrng` - Channel number out of range
- `ecomm` - Communication error on send
- `econnaborted` - Software caused connection abort
- `econnrefused` - Connection refused
- `econnreset` - Connection reset by peer
- `edeadlk` - Resource deadlock avoided
- `edeadlock` - Resource deadlock avoided
- `edestaddrreq` - Destination address required
- `edirty` - Mounting a dirty fs without force
- `edom` - Math argument out of range
- `edotdot` - Cross mount point
- `edquot` - Disk quota exceeded
- `eduppkg` - Duplicate package name
- `eexist` - File already exists
- `efault` - Bad address in system call argument
- `efbig` - File too large
- `ehostdown` - Host is down
- `ehostunreach` - Host is unreachable
- `eidrm` - Identifier removed
- `einit` - Initialization error
- `einprogress` - Operation now in progress
- `eintr` - Interrupted system call
- `einval` - Invalid argument
- `eio` - I/O error
- `eisconn` - Socket is already connected
- `eisdir` - Illegal operation on a directory
- `eisnam` - Is a named file
- `el2hlt` - Level 2 halted
- `el2nsync` - Level 2 not synchronized
- `el3hlt` - Level 3 halted
- `el3rst` - Level 3 reset
- `elbin` - ELBIN
- `elibacc` - Cannot access a needed shared library
- `elibbad` - Accessing a corrupted shared library
- `elibexec` - Cannot exec a shared library directly
- `elibmax` - Attempting to link in more shared libraries than system limit
- `elibscn` - `.lib` section in `a.out` corrupted
- `elnrng` - Link number out of range
- `eloop` - Too many levels of symbolic links
- `emfile` - Too many open files
- `emlink` - Too many links
- `emsgsize` - Message too long
- `emultihop` - Multihop attempted
- `enametoolong` - Filename too long
- `enavail` - Unavailable
- `enet` - ENET
- `enetdown` - Network is down
- `enetreset` - Network dropped connection on reset
- `enetunreach` - Network is unreachable
- `enfile` - File table overflow
- `enoano` - Anode table overflow
- `enobufs` - No buffer space available
- `enocsi` - No CSI structure available
- `enodata` - No data available
- `enodev` - No such device
- `enoent` - No such file or directory
- `enoexec` - Exec format error
- `enolck` - No locks available
- `enolink` - Link has been severed
- `enomem` - Not enough memory
- `enomsg` - No message of desired type
- `enonet` - Machine is not on the network
- `enopkg` - Package not installed
- `enoprotoopt` - Bad protocol option
- `enospc` - No space left on device
- `enosr` - Out of stream resources or not a stream device
- `enosym` - Unresolved symbol name
- `enosys` - Function not implemented
- `enotblk` - Block device required
- `enotconn` - Socket is not connected
- `enotdir` - Not a directory
- `enotempty` - Directory not empty
- `enotnam` - Not a named file
- `enotsock` - Socket operation on non-socket
- `enotsup` - Operation not supported
- `enotty` - Inappropriate device for `ioctl`
- `enotuniq` - Name not unique on network
- `enxio` - No such device or address
- `eopnotsupp` - Operation not supported on socket
- `eperm` - Not owner
- `epfnosupport` - Protocol family not supported
- `epipe` - Broken pipe
- `eproclim` - Too many processes
- `eprocunavail` - Bad procedure for program
- `eprogmismatch` - Wrong program version
- `eprogunavail` - RPC program unavailable
- `eproto` - Protocol error
- `eprotonosupport` - Protocol not supported
- `eprototype` - Wrong protocol type for socket
- `erange` - Math result unrepresentable
- `erefused` - EREFUSED
- `eremchg` - Remote address changed
- `eremdev` - Remote device
- `eremote` - Pathname hit remote filesystem
- `eremoteio` - Remote I/O error
- `eremoterelease` - EREMOTERELEASE
- `erofs` - Read-only filesystem
- `erpcmismatch` - Wrong RPC version
- `erremote` - Object is remote
- `eshutdown` - Cannot send after socket shutdown
- `esocktnosupport` - Socket type not supported
- `espipe` - Invalid seek
- `esrch` - No such process
- `esrmnt` - Srmount error
- `estale` - Stale remote file handle
- `esuccess` - Error 0
- `etime` - Timer expired
- `etimedout` - Connection timed out
- `etoomanyrefs` - Too many references
- `etxtbsy` - Text file or pseudo-device busy
- `euclean` - Structure needs cleaning
- `eunatch` - Protocol driver not attached
- `eusers` - Too many users
- `eversion` - Version mismatch
- `ewouldblock` - Operation would block
- `exdev` - Cross-device link
- `exfull` - Message tables full
- `nxdomain` - Hostname or domain name cannot be found
""".

-include("inet.hrl").
-include("inet_int.hrl").
-include("inet_sctp.hrl").

%% socket
-export([peername/1, sockname/1, port/1, send/2,
	 peernames/1, peernames/2, socknames/1, socknames/2,
	 setopts/2, getopts/2,
	 getifaddrs/0, getifaddrs/1,
	 getif/1, getif/0, getiflist/0, getiflist/1,
	 ifget/3, ifget/2, ifset/3, ifset/2,
	 getstat/1, getstat/2,
         info/1, socket_to_list/1,
	 ip/1, is_ipv4_address/1, is_ipv6_address/1, is_ip_address/1,
	 stats/0, options/0,
	 pushf/3, popf/1, close/1, gethostname/0, gethostname/1,
	 parse_ipv4_address/1, parse_ipv6_address/1,
         parse_ipv4strict_address/1, parse_ipv6strict_address/1,
         parse_address/1, parse_strict_address/1,
         parse_address/2, parse_strict_address/2,
         ntoa/1, ipv4_mapped_ipv6_address/1]).

-export([connect_options/2, listen_options/2, udp_options/2, sctp_options/2]).
-export([udp_module/1, udp_module/2,
         tcp_module/1, tcp_module/2,
         sctp_module/1]).
-export([gen_tcp_module/1, gen_udp_module/1]).

-export([i/0, i/1, i/2]).

-export([getll/1, getfd/1, open/8, open_bind/8, fdopen/6]).

-export([tcp_controlling_process/2, udp_controlling_process/2,
	 tcp_close/1, udp_close/1]).

%% used by sendfile
-export([lock_socket/2]).

%% used by socks5
-export([setsockname/2, setpeername/2]).

%% resolve
-export([gethostbyname/1, gethostbyname/2, gethostbyname/3,
	 gethostbyname_tm/3]).
-export([gethostbyname_string/2, gethostbyname_self/2]).
-export([gethostbyaddr/1, gethostbyaddr/2,
	 gethostbyaddr_tm/2]).

-export([getservbyname/2, getservbyport/2]).
-export([getaddrs/2, getaddrs/3, getaddrs_tm/3,
	 getaddr/2, getaddr/3, getaddr_tm/3]).
-export([translate_ip/2]).
-export([inet_backend/0]).

-export([get_rc/0]).

%% format error
-export([format_error/1]).

%% timer interface
-export([start_timer/1, timeout/1, timeout/2, stop_timer/1]).

%% Socket monitoring
-export([monitor/1, cancel_monitor/1]).

%% Socket utility functions
-export([ensure_sockaddr/1]).

-export_type([socket_protocol/0, hostent/0, hostname/0,
              address_family/0, ip4_address/0, ip6_address/0, ip_address/0,
              port_number/0,
	      family_address/0, local_address/0, socket_address/0,
              returned_non_ip_address/0,
	      socket_setopt/0, socket_getopt/0, socket_optval/0,
              ancillary_data/0,
	      posix/0, socket/0, inet_backend/0, stat_option/0]).
%% imports
-import(lists, [append/1, duplicate/2, filter/2, foldl/3]).

-define(DEFAULT_KERNEL_INET_BACKEND, inet). % inet_backend()

%% Record Signature
-define(RS(Record),
	{Record, record_info(size, Record)}).
%% Record Signature Check (guard)
-define(RSC(Record, RS),
	element(1, Record) =:= element(1, RS),
	tuple_size(Record) =:= element(2, RS)).

%% Two kinds of debug macros (depnds on what you need to debug)
%% -define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).
%% -define(DBG(F, A), io:format("~w(~w) -> " ++ F ++ "~n", [?FUNCTION_NAME, ?LINE | A])).
%% -define(DBG(F),    ?DBG(F, [])).
-define(DBG(F, A),    ok).


%%% ---------------------------------
%%% Contract type definitions


-doc """
A record describing a host; name and address.

Corresponds to the `C`: `struct hostent` as returned by for example
`gethostbyname(3)`.

The record is defined in the Kernel include file `"inet.hrl"`.

Add the following directive to the module:

```erlang
-include_lib("kernel/include/inet.hrl").
```
""".
-doc(#{group => <<"Exported data types">>}).
-type hostent() :: #hostent{}.

-doc(#{group => <<"Exported data types">>}).
-type hostname() :: atom() | string().

-doc(#{group => <<"Exported data types">>}).
-type ip4_address() :: {0..255,0..255,0..255,0..255}.

-doc(#{group => <<"Exported data types">>}).
-type ip6_address() :: {0..65535,0..65535,0..65535,0..65535,
			0..65535,0..65535,0..65535,0..65535}.

-doc(#{group => <<"Exported data types">>}).
-type ip_address() :: ip4_address() | ip6_address().

-doc(#{group => <<"Exported data types">>}).
-type port_number() :: 0..65535.

-doc """
A general network address.

A general network address format of the form `{Family, Destination}`
where `Family` is an atom such as `local` and the format of `Destination`
depends on `Family`.  `Destination` is a complete address (for example
an IP address with port number).
""".
-doc(#{group => <<"Exported data types">>}).
-type family_address() :: inet_address() | inet6_address() | local_address().

-doc """
A network address for the `inet` family (`AF_INET`, IPv4)
> #### Warning {: .warning }
>
> This address format is currently experimental and for completeness
> to make all address families have a `{Family, Destination}` representation.
""".
-doc(#{group => <<"Internal data types">>}).
-type inet_address() ::
        {'inet', {ip4_address() | 'any' | 'loopback', port_number()}}.

-doc """
A network address for the `inet6` family (`AF_INET6`, IPv6)
> #### Warning {: .warning }
>
> This address format is currently experimental and for completeness
> to make all address families have a `{Family, Destination}` representation.
""".
-doc(#{group => <<"Internal data types">>}).
-type inet6_address() ::
        {'inet6', {ip6_address() | 'any' | 'loopback', port_number()}}.

-doc """
A network address for the `local` family (`AF_LOCAL | AF_UNIX`)

This address family, also known as "Unix domain sockets" only works
on Unix-like systems.

`File` is normally a file pathname in a local filesystem. It is limited in
length by the operating system, traditionally to 108 bytes.

A `t:binary/0` is passed as is to the operating system,
but a `t:string/0` is encoded according to the
[system filename encoding mode.](`file:native_name_encoding/0`)

Other addresses are possible, for example Linux implements
"Abstract Addresses".  See the documentation for Unix Domain Sockets
on your system, normally `unix` in manual section 7.

In most API functions where you can use this address family
the port number must be `0`.
""".
-doc(#{group => <<"Exported data types">>}).
-type local_address() :: {'local', File :: binary() | string()}.

-doc """
a non-IP network address.

Addresses besides `t:ip_address/0` ones that are returned from
socket API functions. See in particular `t:local_address/0`.
The `unspec` family corresponds to `AF_UNSPEC` and can occur
if the other side has no socket address. The `undefined`
family can only occur in the unlikely event of an address family
that the VM doesn't recognize.
""".
-doc(#{group => <<"Exported data types">>}).
-type returned_non_ip_address() ::
	{'local', binary()} |
	{'unspec', <<>>} |
	{'undefined', any()}.

-doc """
POSIX Error Code `t:atom/0`.

An atom that is named from the POSIX error codes used in Unix,
and in the runtime libraries of most C compilers.
See section [POSIX Error Codes](#posix-error-codes).
""".
-doc(#{group => <<"Exported data types">>}).
-type posix() ::
        'eaddrinuse' | 'eaddrnotavail' | 'eafnosupport' | 'ealready' |
        'econnaborted' | 'econnrefused' | 'econnreset' |
        'edestaddrreq' |
        'ehostdown' | 'ehostunreach' |
        'einprogress' | 'eisconn' |
        'emsgsize' |
        'enetdown' | 'enetunreach' |
        'enopkg' | 'enoprotoopt' | 'enotconn' | 'enotty' | 'enotsock' |
        'eproto' | 'eprotonosupport' | 'eprototype' |
        'esocktnosupport' |
        'etimedout' |
        'ewouldblock' |
        'exbadport' | 'exbadseq' | file:posix().

-type module_socket() :: {'$inet', Handler :: module(), Handle :: term()}.
-define(module_socket(Handler, Handle), {'$inet', (Handler), (Handle)}).

-doc """
A socket recognized by this module and its siblings.

See `t:gen_tcp:socket/0` and `t:gen_udp:socket/0`.
""".
-doc(#{group => <<"Exported data types">>}).
-type socket() :: port() | module_socket().

-doc """
Implementation backend selector for `t:socket/0`.

Selects the implementation backend for [sockets](`t:socket/0`).
The current default is `inet` which uses `inet_drv.c` to call
the platform's socket API. The value `socket` instead uses
the `m:socket` module and its NIF implementation.

This is a _temporary_ option that will be ignored in a future release.
""".
-doc(#{group => <<"Exported data types">>}).
-type inet_backend() :: {'inet_backend', 'inet' | 'socket'}.

-doc(#{group => <<"Exported data types">>}).
-type socket_setopt() ::
        gen_sctp:option() | gen_tcp:option() | gen_udp:option().

-doc(#{group => <<"Exported data types">>}).
-type socket_optval() ::
        gen_sctp:option_value() | gen_tcp:option() | gen_udp:option() |
        gen_tcp:pktoptions_value().

-doc(#{group => <<"Exported data types">>}).
-type socket_getopt() ::
        gen_sctp:option_name() | gen_tcp:option_name() | gen_udp:option_name().

-type ether_address() :: [0..255].

-type if_setopt() ::
      {'addr', ip_address()} |
      {'broadaddr', ip_address()} |
      {'dstaddr', ip_address()} |
      {'mtu', non_neg_integer()} |
      {'netmask', ip_address()} |
      {'flags', ['up' | 'down' | 'broadcast' | 'no_broadcast' |
		 'pointtopoint' | 'no_pointtopoint' |
		 'running' | 'multicast']} |
      {'hwaddr', ether_address()}.

-type if_getopt() ::
      'addr' | 'broadaddr' | 'dstaddr' |
      'mtu' | 'netmask' | 'flags' |'hwaddr'.

-type if_getopt_result() ::
      {'addr', ip_address()} |
      {'broadaddr', ip_address()} |
      {'dstaddr', ip_address()} |
      {'mtu', non_neg_integer()} |
      {'netmask', ip_address()} |
      {'flags', ['up' | 'down' | 'broadcast' | 'no_broadcast' |
		 'pointtopoint' | 'no_pointtopoint' |
		 'running' | 'multicast' | 'loopback']} |
      {'hwaddr', ether_address()}.

-doc """
Interface address description list.

A list returned from [`getifaddrs/0,1`](`getifaddrs/0`)
for a named interface, translated from the
returned data of the POSIX API function `getaddrinfo()`.

`Hwaddr` is hardware dependent, for example, on Ethernet interfaces it is the
6-byte Ethernet address (MAC address (EUI-48 address)).

The tuples `{addr,Addr}`, `{netmask,Netmask}`, and possibly
`{broadaddr,Broadaddr}` or `{dstaddr,Dstaddr}` are repeated in the list
if the interface has got multiple addresses.  An interface may have multiple
`{flag,_}` tuples for example if it has different flags for different
address families.

Multiple `{hwaddr,Hwaddr}` tuples is hard to say anything definite about,
though. The tuple `{flag,Flags}` is mandatory, all others are optional.

Do not rely too much on the order of `Flags` atoms or the `Ifopt` tuples.
There are however some rules:

- A `{flag,_}` tuple applies to all other tuples that follow.
- Immediately after `{addr,_}` follows `{netmask,_}`.
- Immediately thereafter may `{broadaddr,_}` follow if `broadcast`
  is member of `Flags`, or `{dstaddr,_}` if `pointtopoint`
  is member of `Flags`. Both `{dstaddr,_}` and `{broadaddr,_}` doesn't
  occur for the same `{addr,_}`.
- Any `{netmask,_}`, `{broadaddr,_}`, or `{dstaddr,_}` tuples that follow an
  `{addr,Addr}` tuple concerns the address `Addr`.

The tuple `{hwaddr,_}` is not returned on Solaris, as the hardware address
historically belongs to the link layer and it is not returned
by the Solaris API function `getaddrinfo()`.

> #### Warning {: .warning }
>
> On Windows, the data is fetched from different OS API functions, so the
> `Netmask` and `Broadaddr` values may be calculated, just as some `Flags`
> values.
""".
-doc(#{group => <<"Internal data types">>}).
-type getifaddrs_ifopts() ::
        [Ifopt :: {flags, Flags :: [up | broadcast | loopback |
                                    pointtopoint | running | multicast]} |
                  {addr, Addr :: ip_address()} |
                  {netmask, Netmask :: ip_address()} |
                  {broadaddr, Broadaddr :: ip_address()} |
                  {dstaddr, Dstaddr :: ip_address()} |
                  {hwaddr, Hwaddr :: [byte()]}].

-doc(#{group => <<"Exported data types">>}).
-type address_family() :: 'inet' | 'inet6' | 'local'.

-doc(#{group => <<"Exported data types">>}).
-type socket_protocol() :: 'tcp' | 'udp' | 'sctp'.

-type socket_type() :: 'stream' | 'dgram' | 'seqpacket'.

-doc(#{group => <<"Exported data types">>}).
-type socket_address() ::
	ip_address() | 'any' | 'loopback' | local_address().

-doc(#{group => <<"Exported data types">>}).
-type stat_option() ::
	'recv_cnt' | 'recv_max' | 'recv_avg' | 'recv_oct' | 'recv_dvi' |
	'send_cnt' | 'send_max' | 'send_avg' | 'send_oct' | 'send_pend'.

-doc """
Ancillary data / control messages.

Ancillary data received with a data packet, read with the socket option
[`pktoptions`](`t:gen_tcp:pktoptions_value/0`) from a TCP socket,
or to set in a call to [`gen_udp:send/4`](`m:gen_udp#send-4-AncData`)
or `gen_udp:send/5`.

The value(s) correspond to the currently active socket
[options](`t:socket_setopt/0`) [`recvtos`](#option-recvtos),
[`recvtclass`](#option-recvtclass) and [`recvttl`](#option-recvttl),
or for a single send operation the option(s) to override
the currently active socket option(s).
""".
-doc(#{group => <<"Exported data types">>}).
-type ancillary_data() ::
        [ {'tos', byte()} | {'tclass', byte()} | {'ttl', byte()} ].

%%% ---------------------------------

-doc """
Get the `inet` configuration.

Returns the state of the `inet` configuration database in form of
a list of recorded configuration parameters. For more information, see
[ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`).

Only actual parameters with other than default values are returned,
for example not directives that specify other sources for configuration
parameters nor directives that clear parameters.
""".
-spec get_rc() -> [{Par :: atom(), Val :: any()} |
                   {Par :: atom(), Val1 :: any(), Val2 :: any()}].

get_rc() ->
    inet_db:get_rc().

-doc "Close a socket of any type.".
-spec close(Socket) -> 'ok' when
      Socket :: socket().

close(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
close(Socket) ->
    prim_inet:close(Socket),
    receive
	{Closed, Socket} when Closed =:= tcp_closed; Closed =:= udp_closed ->
	    ok
    after 0 ->
	    ok
    end.


%% -- Socket monitor

-doc """
Start a socket monitor.

If the `Socket` to monitor doesn't exist or when the monitor is triggered,
a `'DOWN'` message is sent that has the following pattern:

```erlang
	    {'DOWN', MonitorRef, Type, Object, Info}
```

- **`MonitorRef`** - The return value from this function.

- **`Type`** - The type of socket, can be one of the following
  `t:atom/0`s: `port` or `socket`.

- **`Object`** - The monitored entity, the socket, which triggered the event.

- **`Info`** - Either the termination reason of the socket or `nosock`
  (the `Socket` did not exist when this function was called).

Making several calls to `inet:monitor/1` for the same `Socket`
is not an error; one monitor is created per call.

The monitor is triggered when the socket is closed in any way such as
an API call, remote end close, closed by signal when owner exits, ...
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec monitor(Socket) -> reference() when
      Socket :: socket().

monitor({'$inet', GenSocketMod, _} = Socket) when is_atom(GenSocketMod) ->
    MRef = GenSocketMod:?FUNCTION_NAME(Socket),
    case inet_db:put_socket_type(MRef, {socket, GenSocketMod}) of
        ok ->
            MRef;
	error ->
	    GenSocketMod:cancel_monitor(MRef),
	    erlang:error({invalid, Socket})
    end;
monitor(Socket) when is_port(Socket) ->
    MRef = erlang:monitor(port, Socket),
    case inet_db:put_socket_type(MRef, port) of
	ok ->
	    MRef;
	error ->
	    erlang:demonitor(MRef, [flush]),
	    erlang:error({invalid, Socket})
    end;
monitor(Socket) ->
    erlang:error(badarg, [Socket]).


%% -- Cancel socket monitor

-doc """
Cancel a socket monitor.

If `MRef` is a reference that the calling process obtained by calling
`monitor/1`, this monitor is removed. If the monitoring is already removed,
nothing happens.

The returned value is one of the following:

- **`true`** - The monitor was found and removed. In this case, no `'DOWN'`
  message corresponding to this monitor has been delivered and will not be
  delivered.

- **`false`** - The monitor was not found and couldn't be removed.
  Probably because the monitor has already triggered and there is
  a corresponding `'DOWN'` message in the caller message queue.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec cancel_monitor(MRef) -> boolean() when
      MRef :: reference().

cancel_monitor(MRef) when is_reference(MRef) ->
    case inet_db:take_socket_type(MRef) of
	{ok, port} ->
	    erlang:demonitor(MRef, [info]);
	{ok, {socket, GenSocketMod}} ->
	    GenSocketMod:?FUNCTION_NAME(MRef);
	error -> % Assume it has the monitor has already been cancel'ed
	    false
    end;
cancel_monitor(MRef) ->
    erlang:error(badarg, [MRef]).


%% -- Socket peername

-doc """
Return the address of the socket's remote end.

Returns the address and port for the other end of a connection.

Notice that for SCTP sockets, this function returns only one of
the peer addresses of the socket. Function [`peernames/1,2`](`peernames/1`)
returns all.
""".
-spec peername(Socket :: socket()) ->
		      {ok,
		       {ip_address(), port_number()} |
		       returned_non_ip_address()} |
		      {error, posix()}.

peername(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
peername(Socket) ->
    prim_inet:peername(Socket).

-doc false.
-spec setpeername(
	Socket :: socket(),
	Address ::
	  {ip_address() | 'any' | 'loopback',
	   port_number()} |
	  socket_address()) ->
			 'ok' | {'error', any()}.

setpeername(Socket, {IP,Port}) ->
    prim_inet:setpeername(Socket, {IP,Port});
setpeername(Socket, undefined) ->
    prim_inet:setpeername(Socket, undefined).

-doc """
Equivalent to [`peernames(Socket, 0)`](`peernames/2`).

Notice that the behavior of this function for an SCTP one-to-many style socket
is not defined by the
[SCTP Sockets API Extensions](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13).
""".
-doc(#{since => <<"OTP R16B03">>}).
-spec peernames(Socket :: socket()) ->
		       {ok,
			[{ip_address(), port_number()} |
			 returned_non_ip_address()]} |
		       {error, posix()}.

peernames(Socket) ->
    prim_inet:peernames(Socket).

-doc """
Return the addresses of all remote ends of a socket.

Returns a list of all address/port number pairs for the remote end of an
association `Assoc` of a socket.

This function can return multiple addresses for multihomed sockets,
such as SCTP sockets. For other sockets it returns a one-element list.

Notice that parameter `Assoc` is by the
[SCTP Sockets API Extensions](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
defined to be ignored for one-to-one style sockets.
What the special value `0` means, is unfortunately undefined,
and hence the behavior for one-to-many style sockets.
""".
-doc(#{since => <<"OTP R16B03">>}).
-spec peernames(Socket, Assoc) ->
		       {ok, [{Address, Port}]} | {error, posix()} when
      Socket :: socket(),
      Assoc :: #sctp_assoc_change{} | gen_sctp:assoc_id(),
      Address :: ip_address(),
      Port :: non_neg_integer().

peernames(Socket, Assoc) ->
    prim_inet:peernames(Socket, Assoc).


-doc """
Return the local address and port number for a socket.

Notice that for SCTP sockets this function returns only one of the socket
addresses. Function [`socknames/1,2`](`socknames/1`) returns all.
""".
-spec sockname(Socket :: socket()) ->
		      {ok,
		       {ip_address(), port_number()} |
		       returned_non_ip_address()} |
		      {error, posix()}.

sockname(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
sockname(Socket) ->
    prim_inet:sockname(Socket).

-doc false.
-spec setsockname(
	Socket :: socket(),
	Address ::
	  {ip_address() | 'any' | 'loopback',
	   port_number()} |
	  socket_address()) ->
	'ok' | {'error', any()}.

setsockname(Socket, {IP,Port}) ->
    prim_inet:setsockname(Socket, {IP,Port});
setsockname(Socket, undefined) ->
    prim_inet:setsockname(Socket, undefined).

-doc "Equivalent to [`socknames(Socket, 0)`](`socknames/2`).".
-doc(#{since => <<"OTP R16B03">>}).
-spec socknames(Socket :: socket()) ->
		       {ok,
			[{ip_address(), port_number()} |
			 returned_non_ip_address()]} |
		       {error, posix()}.

socknames(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
socknames(Socket) ->
    prim_inet:socknames(Socket).

-doc """
Return all localaddresses for a socket.

Returns a list of all local address/port number pairs for a socket,
for the specified association `Assoc`.

This function can return multiple addresses for multihomed sockets,
such as SCTP sockets. For other sockets it returns a one-element list.

Notice that parameter `Assoc` is by the
[SCTP Sockets API Extensions](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
defined to be ignored for one-to-one style sockets.  For one-to-many style
sockets, the special value `0` is defined to mean that the returned addresses
must be without any particular association. How different SCTP implementations
interpret this varies somewhat.
""".
-doc(#{since => <<"OTP R16B03">>}).
-spec socknames(Socket, Assoc) ->
		       {ok, [{Address, Port}]} | {error, posix()} when
      Socket :: socket(),
      Assoc :: #sctp_assoc_change{} | gen_sctp:assoc_id(),
      Address :: ip_address(),
      Port :: non_neg_integer().

socknames(Socket, Assoc) ->
    prim_inet:socknames(Socket, Assoc).


-doc "Return the local port number for a socket.".
-spec port(Socket) -> {'ok', Port} | {'error', any()} when
      Socket :: socket(),
      Port :: port_number().

port(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    case GenSocketMod:sockname(Socket) of
        {ok, {_, Port}} -> {ok, Port};
        {error, _} = Error -> Error
    end;
port(Socket) ->
    case prim_inet:sockname(Socket) of
	{ok, {_,Port}} -> {ok, Port};
	Error -> Error
    end.

-doc false.
-spec send(Socket :: socket(), Packet :: iolist()) -> % iolist()?
	'ok' | {'error', posix()}.

send(?module_socket(GenSocketMod, _) = Socket, Packet)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket, Packet);
send(Socket, Packet) ->
    prim_inet:send(Socket, Packet).

-doc """
Set one or more options for a socket.

Sets the list of `Options` on `Socket`.

The following options are available:

- **`{active, true | false | once | N}`** [](){: #option-active } -
  If the value is `true`, which is the default, everything received
  from the socket is sent as messages to the receiving process.

  If the value is `false` (passive mode), the process must explicitly receive
  incoming data by calling [`gen_tcp:recv/2,3`](`gen_tcp:recv/2`),
  [`gen_udp:recv/2,3`](`gen_udp:recv/2`), or
  [`gen_sctp:recv/1,2`](`gen_sctp:recv/1`) (depending on the type of socket).

  If the value is `once` (`{active, once}`), _one_ data message from the socket
  is sent to the process. To receive one more message,
  [`setopts/2`](`setopts/2`) must be called again with option `{active, once}`.

  If the value is an integer `N` in the range -32768 to 32767 (inclusive), the
  value is added to the socket's count of data messages sent to the controlling
  process. A socket's default message count is `0`. If a negative value is
  specified, and its magnitude is equal to or greater than the socket's current
  message count, the socket's message count is set to `0`. Once the socket's
  message count reaches `0`, either because of sending received data messages to
  the process or by being explicitly set, the process is then notified by a
  special message, specific to the type of socket, that the socket has entered
  passive mode. Once the socket enters passive mode, to receive more messages
  [`setopts/2`](`setopts/2`) must be called again to set the socket back into an
  active mode.

  When using `{active, once}` or `{active, N}`, the socket changes behavior
  automatically when data is received. This can be confusing in combination with
  connection-oriented sockets (that is, `gen_tcp`), as a socket with
  `{active, false}` behavior reports closing differently than a socket with
  `{active, true}` behavior. To simplify programming, a socket where the peer
  closed, and this is detected while in `{active, false}` mode, still generates
  message `{tcp_closed, Socket}` when set to `{active, once}`, `{active, true}`,
  or `{active, N}` mode. It is therefore safe to assume that message
  `{tcp_closed, Socket}`, possibly followed by socket port termination (depending
  on option `exit_on_close`) eventually appears when a socket changes back and
  forth between `{active, true}` and `{active, false}` mode. However, _when_
  peer closing is detected it is all up to the underlying TCP/IP stack and
  protocol.

  Notice that `{active, true}` mode provides no flow control; a fast sender can
  easily overflow the receiver with incoming messages. The same is true for
  `{active, N}` mode, while the message count is greater than zero.

  Use active mode only if your high-level protocol provides its own flow control
  (for example, acknowledging received messages) or the amount of data exchanged
  is small. Using `{active, false}` mode, `{active, once}` mode, or
  `{active, N}` mode with values of `N` appropriate for the application to
  provide flow control, ensures the other side cannot send faster than the
  receiver can read.

- **`{broadcast, Boolean}` (UDP sockets)** [](){: #option-broadcast } -
  Enables/disables permission to send broadcasts.

- **`{buffer, Size}`** [](){: #option-buffer } -
  The size of the user-level buffer used by the driver.  Not to be confused
  with options `sndbuf` and `recbuf`, which correspond to the
  Kernel socket buffers. For TCP it is recommended to have
  `val(buffer) >= val(recbuf)` to avoid performance issues because of
  unnecessary copying. For UDP the same recommendation applies, but the max
  should not be larger than the MTU of the network path. `val(buffer)` is
  automatically set to the above maximum when `recbuf` is set. However, as the
  size set for `recbuf` usually become larger, you are encouraged to use
  `getopts/2` to analyze the behavior of your operating system.

  Note that this is also the maximum amount of data that can be received from a
  single recv call. If you are using higher than normal MTU consider setting
  buffer higher.

- **`{delay_send, Boolean}`** - Normally, when an Erlang process sends to a
  socket, the driver tries to send the data immediately. If that fails, the
  driver uses any means available to queue up the message to be sent whenever
  the operating system says it can handle it. Setting `{delay_send, true}` makes
  _all_ messages queue up. The messages sent to the network are then larger but
  fewer. The option affects the scheduling of send requests versus Erlang
  processes instead of changing any real property of the socket. The option is
  implementation-specific. Defaults to `false`.

- **`{deliver, port | term}`** - When `{active, true}`, data is delivered on the
  form `port` : `{S, {data, [H1,..Hsz | Data]}}`
  or `term` : `{tcp, S, [H1..Hsz | Data]}`.

- **`{dontroute, Boolean}`** - Enables/disables routing bypass for outgoing
  messages.

- **`{exit_on_close, Boolean}`** - This option is set to `true` by default.

  The only reason to set it to `false` is if you want to continue sending data
  to the socket after a close is detected, for example, if the peer uses
  `gen_tcp:shutdown/2` to shut down the write side.

- **`{exclusiveaddruse, Boolean}`** [](){: #option-exclusiveaddruse } -
  Enables/disables exclusive address/port usage on Windows. That is, by enabling
  this option you can prevent other sockets from binding to the same
  address/port.  By default this option is disabled. That is, other sockets
  may use the same address/port by setting
  [`{reuseaddr, true}`](#option-reuseaddr) in combination with
  [`{reuseport, true}`](#option-reuseport) unless
  `{exclusiveaddruse, true}` has been set on `Socket`. On non-Windows systems
  this option is silently ignored.

  > #### Note {: .info }
  >
  > This option is _currently_ not supported for socket created with
  > `inet_backend = socket`

- **`{header, Size}`** - This option is only meaningful if option `binary` was
  specified when the socket was created. If option `header` is specified, the
  first `Size` number bytes of data received from the socket are elements of a
  list, and the remaining data is a binary specified as the tail of the same
  list. For example, if `Size == 2`, the data received matches
  `[Byte1, Byte2 | Binary]`.

- **`{high_msgq_watermark, Size}`** - The socket message queue is set to a busy
  state when the amount of data on the message queue reaches this limit. Notice
  that this limit only concerns data that has not yet reached the ERTS internal
  socket implementation. Defaults to 8 kB.

  Senders of data to the socket are suspended if either the socket message queue
  is busy or the socket itself is busy.

  For more information, see options `low_msgq_watermark`, `high_watermark`, and
  `low_watermark`.

  Notice that distribution sockets disable the use of `high_msgq_watermark` and
  `low_msgq_watermark`. Instead use the
  [distribution buffer busy limit](`m:erlang#system_info_dist_buf_busy_limit`),
  which is a similar feature.

- **`{high_watermark, Size}` (TCP/IP sockets)** - The socket is set to a busy
  state when the amount of data queued internally by the ERTS socket
  implementation reaches this limit. Defaults to 8 kB.

  Senders of data to the socket are suspended if either the socket message queue
  is busy or the socket itself is busy.

  For more information, see options `low_watermark`, `high_msgq_watermark`, and
  `low_msqg_watermark`.

- **`{ipv6_v6only, Boolean}`** - Restricts the socket to use only IPv6,
  prohibiting any IPv4 connections. This is only applicable for IPv6 sockets
  (option `inet6`).

  On most platforms this option must be set on the socket before associating it
  to an address. It is therefore only reasonable to specify it when creating the
  socket and not to use it when calling function (`setopts/2`) containing this
  description.

  The behavior of a socket with this option set to `true` is the only portable
  one. The original idea when IPv6 was new of using IPv6 for all traffic is now
  not recommended by FreeBSD (you can use `{ipv6_v6only,false}` to override the
  recommended system default value), forbidden by OpenBSD (the supported GENERIC
  kernel), and impossible on Windows (which has separate IPv4 and IPv6 protocol
  stacks). Most Linux distros still have a system default value of `false`. This
  policy shift among operating systems to separate IPv6 from IPv4 traffic has
  evolved, as it gradually proved hard and complicated to get a dual stack
  implementation correct and secure.

  On some platforms, the only allowed value for this option is `true`, for
  example, OpenBSD and Windows. Trying to set this option to `false`, when
  creating the socket, fails in this case.

  Setting this option on platforms where it does not exist is ignored. Getting
  this option with `getopts/2` returns no value, that is, the returned list does
  not contain an `{ipv6_v6only,_}` tuple. On Windows, the option does not exist,
  but it is emulated as a read-only option with value `true`.

  Therefore, setting this option to `true` when creating a socket never fails,
  except possibly on a platform where you have customized the kernel to only
  allow `false`, which can be doable (but awkward) on, for example, OpenBSD.

  If you read back the option value using `getopts/2` and get no value, the
  option does not exist in the host operating system. The behavior of both an
  IPv6 and an IPv4 socket listening on the same port, and for an IPv6 socket
  getting IPv4 traffic is then no longer predictable.

- **`{keepalive, Boolean}` (TCP/IP sockets)** - Enables/disables periodic
  transmission on a connected socket when no other data is exchanged. If the
  other end does not respond, the connection is considered broken and an error
  message is sent to the controlling process. Defaults to `false`.

- **`{linger, {true|false, Seconds}}`** [](){: #option-linger } -
  Determines the time-out, in seconds, for flushing unsent data
  in the [`close/1`](`close/1`) socket call.

  The first component is if linger is enabled, the second component is the
  flushing time-out, in seconds. There are 3 alternatives:

  - **`{false, _}`** - close/1 or shutdown/2 returns immediately, not waiting
    for data to be flushed, with closing happening in the background.

  - **`{true, 0}`** - Aborts the connection when it is closed. Discards any data
    still remaining in the send buffers and sends RST to the peer.

    This avoids TCP's TIME_WAIT state, but leaves open the possibility that
    another "incarnation" of this connection being created.

  - **`{true, Time} when Time > 0`** - close/1 or shutdown/2 will not return
    until all queued messages for the socket have been successfully sent or the
    linger timeout (Time) has been reached.

- **`{low_msgq_watermark, Size}`** - If the socket message queue is in a busy
  state, the socket message queue is set in a not busy state when the amount of
  data queued in the message queue falls below this limit. Notice that this
  limit only concerns data that has not yet reached the ERTS internal socket
  implementation. Defaults to 4 kB.

  Senders that are suspended because of either a busy message queue or a busy
  socket are resumed when the socket message queue and the socket are not busy.

  For more information, see options `high_msgq_watermark`, `high_watermark`, and
  `low_watermark`.

  Notice that distribution sockets disable the use of `high_msgq_watermark` and
  `low_msgq_watermark`. Instead they use the
  [distribution buffer busy limit](`m:erlang#system_info_dist_buf_busy_limit`),
  which is a similar feature.

- **`{low_watermark, Size}` (TCP/IP sockets)** - If the socket is in a busy
  state, the socket is set in a not busy state when the amount of data queued
  internally by the ERTS socket implementation falls below this limit. Defaults
  to 4 kB.

  Senders that are suspended because of a busy message queue or a busy socket
  are resumed when the socket message queue and the socket are not busy.

  For more information, see options `high_watermark`, `high_msgq_watermark`, and
  `low_msgq_watermark`.

- **`{mode, Mode :: binary | list}`** - Received `Packet` is delivered as
  defined by `Mode`.

- **`{netns, Namespace :: file:filename_all()}`{: #option-netns }** - Sets a
  network namespace for the socket. Parameter `Namespace` is a filename defining
  the namespace, for example, `"/var/run/netns/example"`, typically created by
  command `ip netns add example`. This option must be used in a function call
  that creates a socket, that is, [`gen_tcp:connect/3,4`](`gen_tcp:connect/3`),
  `gen_tcp:listen/2`, [`gen_udp:open/1,2`](`gen_udp:open/1`) or
  [`gen_sctp:open/0,1,2`](`gen_sctp:open/0`), and also `getifaddrs/1`.

  This option uses the Linux-specific syscall `setns()`, such as in Linux kernel
  3.0 or later, and therefore only exists when the runtime system is compiled
  for such an operating system.

  The virtual machine also needs elevated privileges, either running as
  superuser or (for Linux) having capability `CAP_SYS_ADMIN` according to the
  documentation for `setns(2)`. However, during testing also `CAP_SYS_PTRACE`
  and `CAP_DAC_READ_SEARCH` have proven to be necessary.

  _Example:_

  ```text
  setcap cap_sys_admin,cap_sys_ptrace,cap_dac_read_search+epi beam.smp
  ```

  Notice that the filesystem containing the virtual machine executable
  (`beam.smp` in the example) must be local, mounted without flag `nosetuid`,
  support extended attributes, and the kernel must support file capabilities.
  All this runs out of the box on at least Ubuntu 12.04 LTS, except that SCTP
  sockets appear to not support network namespaces.

  `Namespace` is a filename and is encoded and decoded as discussed in module
  `m:file`, with the following exceptions:

  - Emulator flag `+fnu` is ignored.
  - `getopts/2` for this option returns a binary for the filename if the stored
    filename cannot be decoded. This is only to occur if you set the option
    using a binary that cannot be decoded with the emulator's filename encoding:
    `file:native_name_encoding/0`.

- **`{bind_to_device, Ifname :: binary()}`** - Binds a socket to a specific
  network interface. This option must be used in a function call that creates a
  socket, that is, [`gen_tcp:connect/3,4`](`gen_tcp:connect/3`),
  `gen_tcp:listen/2`, [`gen_udp:open/1,2`](`gen_udp:open/1`), or
  [`gen_sctp:open/0,1,2`](`gen_sctp:open/0`).

  Unlike `getifaddrs/0`, Ifname is encoded a binary. In the unlikely case that a
  system is using non-7-bit-ASCII characters in network device names, special
  care has to be taken when encoding this argument.

  This option uses the Linux-specific socket option `SO_BINDTODEVICE`, such as
  in Linux kernel 2.0.30 or later, and therefore only exists when the runtime
  system is compiled for such an operating system.

  Before Linux 3.8, this socket option could be set, but could not retrieved
  with `getopts/2`. Since Linux 3.8, it is readable.

  The virtual machine also needs elevated privileges, either running as
  superuser or (for Linux) having capability `CAP_NET_RAW`.

  The primary use case for this option is to bind sockets into
  [Linux VRF instances](http://www.kernel.org/doc/Documentation/networking/vrf.txt).

- **`list`** - Received `Packet` is delivered as a list.

- **`binary`** - Received `Packet` is delivered as a binary.

- **`{nodelay, Boolean}` (TCP/IP sockets)** [](){: #option-nodelay } -
  If `Boolean == true`, option `TCP_NODELAY` is turned on for the socket,
  which means that also small amounts of data are sent immediately.

  This option is _not_ supported for `domain = local`, but if
  `inet_backend =/= socket` this error will be _ignored_.

- **`{nopush, Boolean}` (TCP/IP sockets)** - This translates to `TCP_NOPUSH` on
  BSD and to `TCP_CORK` on Linux.

  If `Boolean == true`, the corresponding option is turned on for the socket,
  which means that small amounts of data are accumulated until a full MSS-worth
  of data is available or this option is turned off.

  Note that while `TCP_NOPUSH` socket option is available on OSX, its semantics
  is very different (e.g., unsetting it does not cause immediate send of
  accumulated data). Hence, `nopush` option is intentionally ignored on OSX.

- **`{packet, PacketType}` (TCP/IP sockets)** [](){: #option-packet } -
  Defines the type of packets to use for a socket. Possible values:

  - **`raw | 0`** - No packaging is done.

  - **`1 | 2 | 4`** - Packets consist of a header specifying the number of bytes
    in the packet, followed by that number of bytes. The header length can be
    one, two, or four bytes, and containing an unsigned integer in big-endian
    byte order. Each send operation generates the header, and the header is
    stripped off on each receive operation.

    The 4-byte header is limited to 2Gb.

  - **`asn1 | cdr | sunrm | fcgi | tpkt | line`** - These packet types only have
    effect on receiving. When sending a packet, it is the responsibility of the
    application to supply a correct header. On receiving, however, one message
    is sent to the controlling process for each complete packet received, and,
    similarly, each call to `gen_tcp:recv/2,3` returns one complete packet. The
    header is _not_ stripped off.

    The meanings of the packet types are as follows:

    - `asn1` - ASN.1 BER
    - `sunrm` - Sun's RPC encoding
    - `cdr` - CORBA (GIOP 1.1)
    - `fcgi` - Fast CGI
    - `tpkt` - TPKT format \[RFC1006]
    - `line` - Line mode, a packet is a line-terminated with newline, lines
      longer than the receive buffer are truncated

  - **`http | http_bin`** - The Hypertext Transfer Protocol. The packets are
    returned with the format according to `HttpPacket` described in
    `erlang:decode_packet/3` in ERTS. A socket in passive mode returns
    `{ok, HttpPacket}` from `gen_tcp:recv` while an active socket sends messages
    like `{http, Socket, HttpPacket}`.

  - **`httph | httph_bin`** - These two types are often not needed, as the
    socket automatically switches from `http`/`http_bin` to `httph`/`httph_bin`
    internally after the first line is read. However, there can be occasions
    when they are useful, such as parsing trailers from chunked encoding.

- **`{packet_size, Integer}`(TCP/IP sockets)** - Sets the maximum allowed length
  of the packet body. If the packet header indicates that the length of the
  packet is longer than the maximum allowed length, the packet is considered
  invalid. The same occurs if the packet header is too large for the socket
  receive buffer.

  For line-oriented protocols (`line`, `http*`), option `packet_size` also
  guarantees that lines up to the indicated length are accepted and not
  considered invalid because of internal buffer limitations.

- **`{line_delimiter, Char}` (TCP/IP sockets)**
  [](){: #option-line_delimiter } -
  Sets the line delimiting character for line-oriented protocols (`line`).
  Defaults to `$\n`.

- **`{raw, Protocol, OptionNum, ValueBin}`** - See below.

- **`{read_ahead, Boolean}`** [](){: #option-read_ahead } -
  If set to `false` avoids reading ahead from the OS socket layer.
  The default for this option is `true` which speeds up packet header parsing.
  Setting `false` has a performance penalty because the packet header
  has to be read first, to know exactly how many bytes to read for the body,
  which roughly doubles the number of read operations.

  The use of this option is essential for example before switching to kTLS
  which activates OS socket layer encryption and decryption by setting
  special (raw) socket options.  So if the Erlang socket layer has read ahead,
  it has read bytes that was for the OS socket layer to decrypt,
  which makes packet decryption derail for the connection.

  > #### Warning {: .warning }
  >
  > For packet modes that doesn't have the packet length at a fixed location
  > in a packet header, such as `line` or `asn1`, not reading ahead
  > can become very inefficient since sometimes the only way to accomplish
  > this is to read one byte at the time until the length
  > or packet end is found.

- **`{read_packets, Integer}` (UDP sockets)** [](){: #option-read_packets } -
  Sets the maximum number of UDP packets to read without intervention
  from the socket when data is available.  When this many packets
  have been read and delivered to the destination process,
  new packets are not read until a new notification of available data
  has arrived. Defaults to `5`. If this parameter is set too high, the system
  can become unresponsive because of UDP packet flooding.

- **`{recbuf, Size}`** [](){: #option-recbuf } -
  The minimum size of the receive buffer to use for the socket.
  You are encouraged to use `getopts/2` to retrieve the size
  set by your operating system.

- **`{recvtclass, Boolean}`** [](){: #option-recvtclass } -
  If set to `true` activates returning the received `TCLASS` value
  on platforms that implements the protocol `IPPROTO_IPV6` option
  `IPV6_RECVTCLASS` or `IPV6_2292RECVTCLASS` for the socket.
  The value is returned as a `{tclass,TCLASS}` tuple regardless of if
  the platform returns an `IPV6_TCLASS` or an `IPV6_RECVTCLASS` CMSG value.

  For packet oriented sockets that supports receiving ancillary data with the
  payload data (`gen_udp` and `gen_sctp`), the `TCLASS` value is returned in an
  extended return tuple contained in an
  [ancillary data](`t:ancillary_data/0`) list. For stream oriented sockets
  (`gen_tcp`) the only way to get the `TCLASS` value is if the platform supports
  the [`pktoptions`](`t:gen_tcp:pktoptions_value/0`) option.

- **`{recvtos, Boolean}`** [](){: #option-recvtos } -
  If set to `true` activates returning the received `TOS` value
  on platforms that implements the protocol `IPPROTO_IP` option
  `IP_RECVTOS` for the socket. The value is returned as a `{tos,TOS}` tuple
  regardless of if the platform returns an `IP_TOS` or an `IP_RECVTOS` CMSG
  value.

  For packet oriented sockets that supports receiving ancillary data with the
  payload data (`gen_udp` and `gen_sctp`), the `TOS` value is returned in an
  extended return tuple contained in an
  [ancillary data](`t:ancillary_data/0`) list. For stream oriented sockets
  (`gen_tcp`) the only way to get the `TOS` value is if the platform supports
  the [`pktoptions`](`t:gen_tcp:pktoptions_value/0`) option.

- **`{recvttl, Boolean}`** [](){: #option-recvttl } -
  If set to `true` activates returning the received `TTL` value
  on platforms that implements the protocol `IPPROTO_IP` option `IP_RECVTTL`
  for the socket. The value is returned as a `{ttl,TTL}` tuple
  regardless of if the platform returns an `IP_TTL` or an `IP_RECVTTL` CMSG
  value.

  For packet oriented sockets that supports receiving ancillary data with the
  payload data (`gen_udp` and `gen_sctp`), the `TTL` value is returned in an
  extended return tuple contained in an
  [ancillary data](`t:ancillary_data/0`) list. For stream oriented sockets
  (`gen_tcp`) the only way to get the `TTL` value is if the platform supports
  the [`pktoptions`](`t:gen_tcp:pktoptions_value/0`) option.

- **`{reuseaddr, Boolean}`[](){: #option-reuseaddr }** -
   Allows or disallows reuse of local address. By default, reuse is disallowed.

  > #### Note {: .info }
  >
  > On windows `{reuseaddr, true}` will have no effect unless also
  > [`{reuseport, true}`](#option-reuseport) is set. If both are set,
  > the `SO_REUSEADDR` Windows socket option will be enabled. This since setting
  > `SO_REUSEADDR` on Windows more or less has the same behavior as setting both
  > `SO_REUSEADDR` and `SO_REUSEPORT` on BSD. This behavior was introduced as of
  > OTP 26.0.
  >
  > > #### Change {: .info }
  > >
  > > Previous behavior on Windows:
  > >
  > > - Prior to OTP 25.0, the `{reuseaddr, true}` option was silently ignored.
  > > - Between OTP 25.0 and up to the predecessor of OTP 25.2, the underlying
  > >   `SO_REUSEADDR` socket option was set if `{reuseaddr, true}` was set.
  > > - Between OTP 25.2 and up to the predecessor of OTP 26.0, the underlying
  > >   `SO_REUSEADDR` socket option was only set on UDP sockets if
  > >   `{reuseaddr, true}` was set, and silently ignored on other sockets.
  >
  > See also the [`exclusiveaddruse`](#option-exclusiveaddruse) option.

- **`{reuseport, Boolean}`[](){: #option-reuseport }** -
   Allows or disallows reuse of local port which _may or may not_
  have load balancing depending on the underlying OS. By default,
  reuse is disallowed. See also [`reuseport_lb`](#option-reuseport_lb).

  > #### Note {: .info }
  >
  > On windows `{reuseport, true}` will have no effect unless also
  > [`{reuseaddr, true}`](#option-reuseaddr) is set. If both are set,
  > the `SO_REUSEADDR` Windows socket option will be enabled. This since setting
  > `SO_REUSEADDR` on Windows more or less has the same behavior as setting both
  > `SO_REUSEADDR` and `SO_REUSEPORT` on BSD. The `reuseport` option was
  > introduced as of OTP 26.0.
  >
  > See also the [`exclusiveaddruse`](#option-exclusiveaddruse) option.

  > #### Note {: .info }
  >
  > `reuseport` _may or may not_ be the same underlying option as
  > [`reuseport_lb`](#option-reuseport_lb) depending on the underlying
  > OS. They, for example, are on Linux. When they are the same underlying
  > option, operating on both may cause them to interact in surprising ways. For
  > example, by enabling `reuseport` and then disabling `reuseport_lb` both will
  > end up being disabled.

  > #### Note {: .info }
  >
  > This option is _currently_ not supported for socket created with
  > `inet_backend = socket`

- **`{reuseport_lb, Boolean}`[](){: #option-reuseport_lb }** -
  Allows or disallows reuse of local port _with_ load balancing.
  By default, reuse is disallowed.  See also [`reuseport`](#option-reuseport).

  > #### Note {: .info }
  >
  > `reuseport_lb` _may or may not_ be the same underlying option as
  > [`reuseport`](#option-reuseport) depending on the underlying OS.
  > On Linux, for example, they are.  And when they are the same
  >  underlying option, operating on both may cause them to interact
  > in surprising ways. For example, by enabling `reuseport_lb`,
  > and then disabling `reuseport`, both will end up being disabled.

  > #### Note {: .info }
  >
  > This option is _currently_ not supported for socket created with
  > `inet_backend = socket`

- **`{send_timeout, Integer}`** - Only allowed for connection-oriented sockets.

  Specifies a longest time to wait for a send operation to be accepted by the
  underlying TCP stack. When the limit is exceeded, the send operation returns
  `{error, timeout}`. How much of a packet that got sent is unknown; the socket
  is therefore to be closed whenever a time-out has occurred (see
  `send_timeout_close` below). Defaults to `infinity`.

- **`{send_timeout_close, Boolean}`** - Only allowed for connection-oriented
  sockets.

  Used together with `send_timeout` to specify whether the socket is to be
  automatically closed when the send operation returns `{error, timeout}`. The
  recommended setting is `true`, which automatically closes the socket. Defaults
  to `false` because of backward compatibility.

- **`{show_econnreset, Boolean}` (TCP/IP sockets)**
  [](){: #option-show_econnreset } -
  When this option is set to `false`, which is default, an RST
  received from the TCP peer is treated as a normal close
  (as though an FIN was sent). A caller to `gen_tcp:recv/2` gets
  `{error, closed}`. In active mode, the controlling process receives a
  `{tcp_closed, Socket}` message, indicating that the peer has closed the
  connection.

  Setting this option to `true` allows you to distinguish between a connection
  that was closed normally, and one that was aborted (intentionally or
  unintentionally) by the TCP peer. A call to `gen_tcp:recv/2` returns
  `{error, econnreset}`. In active mode, the controlling process receives a
  `{tcp_error, Socket, econnreset}` message before the usual
  `{tcp_closed, Socket}`, as is the case for any other socket error. Calls to
  `gen_tcp:send/2` also returns `{error, econnreset}` when it is detected that a
  TCP peer has sent an RST.

  A connected socket returned from `gen_tcp:accept/1` inherits the
  `show_econnreset` setting from the listening socket.

- **`{sndbuf, Size}`** [](){: #option-sndbuf } -
  The minimum size of the send buffer to use for the socket.
  You are encouraged to use `getopts/2`, to retrieve the size
  set by your operating system.

- **`{priority, Integer}`** - Sets the `SO_PRIORITY` socket level option on
  platforms where this is implemented. The behavior and allowed range varies
  between different systems. The option is ignored on platforms where it is not
  implemented. Use with caution.

- **`{tos, Integer}`** - Sets `IP_TOS IP` level options on platforms where this
  is implemented. The behavior and allowed range varies between different
  systems. The option is ignored on platforms where it is not implemented. Use
  with caution.

- **`{tclass, Integer}`** - Sets `IPV6_TCLASS IP` level options on platforms
  where this is implemented. The behavior and allowed range varies between
  different systems. The option is ignored on platforms where it is not
  implemented. Use with caution.

In addition to these options, _raw_ option specifications can be used. The raw
options are specified as a tuple of arity four, beginning with tag `raw`,
followed by the protocol level, the option number, and the option value
specified as a binary. This corresponds to the second, third, and fourth
arguments to the `setsockopt` call in the C socket API. The option value must be
coded in the native endianness of the platform and, if a structure is required,
must follow the structure alignment conventions on the specific platform.

Using raw socket options requires detailed knowledge about the current operating
system and TCP stack.

_Example:_

This example concerns the use of raw options. Consider a Linux system where you
want to set option `TCP_LINGER2` on protocol level `IPPROTO_TCP` in the stack.
You know that on this particular system it defaults to 60 (seconds), but you
want to lower it to 30 for a particular socket. Option `TCP_LINGER2` is not
explicitly supported by `inet`, but you know that the protocol level translates
to number 6, the option number to number 8, and the value is to be specified as
a 32-bit integer. You can use this code line to set the option for the socket
named `Sock`:

```text
inet:setopts(Sock, [{raw,6,8,<<30:32/native>>}]),
```

As many options are silently discarded by the stack if they are specified out of
range; it can be a good idea to check that a raw option is accepted. The
following code places the value in variable `TcpLinger2:`

```text
{ok,[{raw,6,8,<<TcpLinger2:32/native>>}]}=inet:getopts(Sock,[{raw,6,8,4}]),
```

Code such as these examples is inherently non-portable, even different versions
of the same OS on the same platform can respond differently to this kind of
option manipulation. Use with care.

Notice that the default options for TCP/IP sockets can be changed with the
Kernel configuration parameters mentioned in the beginning of this manual page.
""".
-spec setopts(Socket, Options) -> ok | {error, posix()} when
      Socket :: socket(),
      Options :: [socket_setopt()].

setopts(?module_socket(GenSocketMod, _) = Socket, Opts) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket, Opts);
setopts(Socket, Opts) ->
    SocketOpts =
	[case Opt of
	     {netns,NS} ->
		 {netns,filename2binary(NS)};
	     _ ->
		 Opt
	 end || Opt <- Opts],
    prim_inet:setopts(Socket, SocketOpts).

-doc """
Get one or more options for a socket.

Gets all options in the list `Options` from `Socket`.
See `setopts/2` for a list of available options. See also
the descriptions of protocol specific types referenced by
[`socket_optval()` ](`t:socket_optval/0`).

The number of elements in the returned `OptionValues` list does not necessarily
correspond to the number of options asked for. If the operating system fails to
support an option, it is left out in the returned list. An error tuple is
returned only when getting options for the socket is impossible (that is, the
socket is closed or the buffer size in a raw request is too large). This
behavior is kept for backward compatibility reasons.

A raw option request `RawOptReq = {raw, Protocol, OptionNum, ValueSpec}` can be
used to get information about socket options not (explicitly) supported by the
emulator. The use of raw socket options makes the code non-portable, but allows
the Erlang programmer to take advantage of unusual features present on a
particular platform.

`RawOptReq` consists of tag `raw` followed by the protocol level, the option
number, and either a binary or the size, in bytes, of the buffer in which the
option value is to be stored. A binary is to be used when the underlying
`getsockopt` requires _input_ in the argument field. In this case, the binary
size shall correspond to the required buffer size of the return value. The
supplied values in a `RawOptReq` correspond to the second, third, and
fourth/fifth parameters to the `getsockopt` call in the C socket API. The value
stored in the buffer is returned as a binary `ValueBin`, where all values are
coded in native endianness.

Asking for and inspecting raw socket options require low-level information about
the current operating system and TCP stack.

_Example:_

Consider a Linux machine where option `TCP_INFO` can be used to collect TCP
statistics for a socket. Assume you are interested in field `tcpi_sacked` of
`struct tcp_info` filled in when asking for `TCP_INFO`. To be able to access
this information, you need to know the following:

- The numeric value of protocol level `IPPROTO_TCP`
- The numeric value of option `TCP_INFO`
- The size of `struct tcp_info`
- The size and offset of the specific field

By inspecting the headers or writing a small C program, it is found that
`IPPROTO_TCP` is 6, `TCP_INFO` is 11, the structure size is 92 (bytes), the
offset of `tcpi_sacked` is 28 bytes, and the value is a 32-bit integer. The
following code can be used to retrieve the value:

```erlang
get_tcpi_sacked(Sock) ->
    {ok,[{raw,_,_,Info}]} = inet:getopts(Sock,[{raw,6,11,92}]),
    <<_:28/binary,TcpiSacked:32/native,_/binary>> = Info,
    TcpiSacked.
```

Preferably, you would check the machine type, the operating system, and the
Kernel version before executing anything similar to this code.
""".
-spec getopts(Socket, Options) ->
	{'ok', OptionValues} | {'error', posix()} when
      Socket :: socket(),
      Options :: [socket_getopt()],
      OptionValues :: [socket_optval()].

getopts(?module_socket(GenSocketMod, _) = Socket, Opts)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket, Opts);
getopts(Socket, Opts) ->
    case prim_inet:getopts(Socket, Opts) of
	{ok,OptionValues} ->
	    {ok,
	     [case OptionValue of
		  {netns,Bin} ->
		      {netns,binary2filename(Bin)};
		  _ ->
		      OptionValue
	      end || OptionValue <- OptionValues]};
	Other ->
	    Other
    end.

%% --------------------------------------------------------------------------

-doc """
Get interface names and addresses, in a specific namespace.

Equivalent to `getifaddrs/0`, but accepts an `Option`
`{netns, Namespace}` that, on platforms that support the feature (Linux),
sets a network namespace for the OS call.
Also,
If the option 'inet_backend' is *first* in the options list,
the specified backend will be used (for 'inet', inet and
for 'socket' the equivalent net functions will be used).
                                         

See the socket option [`{netns, Namespace}`](#option-netns)
under `setopts/2`.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec getifaddrs(
        [Option :: inet_backend() | {netns, Namespace :: file:filename_all()}]
        | socket()) ->
                        {'ok', [{Ifname :: string(),
                                 Ifopts :: getifaddrs_ifopts()}]}
                            | {'error', posix()}.

getifaddrs([{inet_backend, Backend}|Opts]) ->
    do_getifaddrs(Backend, Opts);
getifaddrs(Opts) when is_list(Opts) ->
    do_getifaddrs(inet_backend(), Opts);
getifaddrs(?module_socket(GenSocketMod, _) = _Socket)
  when is_atom(GenSocketMod) ->
    do_getifaddrs('socket', []);
getifaddrs(Socket) when is_port(Socket) ->
    do_getifaddrs('inet', Socket).

do_getifaddrs('socket', []) ->
    net_getifaddrs(net:getifaddrs(all));
do_getifaddrs('socket', [{netns, Namespace}]) ->
    net_getifaddrs(net:getifaddrs(all, Namespace));
do_getifaddrs('inet', Opts) when is_list(Opts) ->
    withsocket(fun(S) -> prim_inet:getifaddrs(S) end, Opts);
do_getifaddrs('inet', Socket) when is_port(Socket) ->
    prim_inet:getifaddrs(Socket).


net_unique_if_names(Ifs) ->
    net_unique_if_names(Ifs, []).

net_unique_if_names([], IfNames) ->
    lists:reverse(IfNames);
net_unique_if_names([#{name := IfName}|Ifs], IfNames) ->
    case lists:member(IfName, IfNames) of
        true ->
            net_unique_if_names(Ifs, IfNames);
        false ->
            net_unique_if_names(Ifs, [IfName|IfNames])
    end.

net_getifaddrs({ok, AllIfs}) ->
    IfNames = net_unique_if_names(AllIfs),
    {ok, net_collect_ifopts(IfNames, AllIfs)};
net_getifaddrs({error, _} = ERROR) ->
    ERROR.


net_collect_ifopts(IfNames, AllIfs) ->
    net_collect_ifopts(IfNames, AllIfs, []).

net_collect_ifopts([], _AllIfs, AllNameAndOpts) ->
    lists:reverse(AllNameAndOpts);
net_collect_ifopts([IfName|IfNames], AllIfs, NameAndOpts) ->
    %% Get the Ifs with the name IfName
    ?DBG("entry with"
         "~n   IfName: ~p", [IfName]),
    Ifs = [If || #{name := N} = If <- AllIfs, (N =:= IfName)],
    IfOpts = net_ifs2ifopts(Ifs),
    ?DBG("collected for interface ~s:"
         "~n   ~p", [IfName, IfOpts]),
    net_collect_ifopts(IfNames, AllIfs, [{IfName, IfOpts}|NameAndOpts]).

net_ifs2ifopts(Ifs) ->
    net_ifs2ifopts(Ifs,
                   %% Family: inet
                   #{flags  => [],
                     addrs  => []},
                   %% Family: inet6
                   #{flags  => [],
                     addrs  => []},
                   %% Family: packet | link
                   #{flags  => [],
                     addr   => []}).


net_ifs2ifopts([],
               %% Family: inet
               #{flags := [],
                 addrs := []},
               %% Family: inet6
               #{flags := [],
                 addrs := []},
               %% Family: packet | link
               #{flags := [],
                 addr  := []}) ->
    [{flags, []}];
net_ifs2ifopts([],
               %% Family: inet
               #{flags := Flags4,
                 addrs := Addrs4},
               %% Family: inet6
               #{flags := Flags6,
                 addrs := Addrs6},
               %% Family: packet | link
               #{flags := FlagsHw,
                 addr  := AddrHw}) ->
    ?DBG("entry when done with"
         "~n   Flags4:  ~p"
         "~n   Addrs4:  ~p"
         "~n   Flags6:  ~p"
         "~n   Addrs6:  ~p"
         "~n   FlagsHw: ~p"
         "~n   AddrHw:  ~p",
         [Flags4, Addrs4, Flags6, Addrs6, FlagsHw, AddrHw]),
    case {Flags4, Addrs4} of
        {[], []} ->
            [];
        _ ->
            [{flags, net_flags_to_inet_flags(Flags4)}] ++
                lists:reverse(Addrs4)
    end ++
        case Addrs6 of
            [] ->
                [];
            _ ->
                case Flags6 of
                    Flags4 ->
                        lists:reverse(Addrs6);
                    [] ->
                        lists:reverse(Addrs6);
                    _ ->
                        [{flags, net_flags_to_inet_flags(Flags6)}] ++
                            lists:reverse(Addrs6)
                end
        end ++
        case {FlagsHw, AddrHw} of
            {[], []} ->
                [];
            {[], _} ->
                [{hwaddr, AddrHw}];
            {_, _} when ((FlagsHw =:= Flags4) orelse
                         (FlagsHw =:= Flags6)) andalso (AddrHw =/= []) ->
                [{hwaddr, AddrHw}];
            _ ->
                [{flags, net_flags_to_inet_flags(FlagsHw)}] ++
                    [{hwaddr, AddrHw}]
        end;
net_ifs2ifopts([If|Ifs], IfOpts4_0, IfOpts6_0, IfOptsHw_0) ->
    case If of
        %% LINK or PACKET
        %% - On some platforms LINK is used (FreeBSD for instance)
        %%   LINK does not include an explicit HW address. Instead
        %%   its part of the 'data', together with name and possibly
        %%   link layer selector (the lengths can be used to decode
        %%   the data)..
        %% - On others PACKET is used.
        #{flags := Flags,
          addr  := #{family := packet,
                     addr   := HwAddrBin}} ->
            %% This should only come once (per interface) so we
            %% do not actually check...
            ?DBG("packet entry:"
                 "~n   Flags:     ~p"
                 "~n   HwAddrBin: ~p", [Flags, HwAddrBin]),
            IfOptsHw =
                IfOptsHw_0#{flags => Flags,
                            addr  => binary_to_list(HwAddrBin)},
            net_ifs2ifopts(Ifs, IfOpts4_0, IfOpts6_0, IfOptsHw);
        #{flags := Flags,
          addr  := #{family := link,
                     nlen   := NLen,
                     alen   := ALen,
                     data   := Data}} when (ALen > 0) ->
            ?DBG("link entry:"
                 "~n   Flags: ~p"
                 "~n   NLen:  ~p"
                 "~n   ALen:  ~p"
                 "~n   Data:  ~p", [Flags, NLen, ALen, Data]),
            IfOptsHw =
                case Data of
                    <<_:NLen/binary, ABin:ALen/binary, _/binary>> ->
                        IfOptsHw_0#{flags => Flags,
                                    addr  => binary_to_list(ABin)};
                    _ ->
                        IfOptsHw_0#{flags => Flags}
                end,
            net_ifs2ifopts(Ifs, IfOpts4_0, IfOpts6_0, IfOptsHw);

        #{flags := Flags,
          addr  := #{family := Fam,
                     addr   := Addr},
          netmask := #{family := Fam,
                       addr   := Mask}} when (Fam =:= inet) orelse
                                             (Fam =:= inet6) ->
            %% We may also have broadcast or dest addr
            BroadAddr = case maps:get(broadaddr, If, undefined) of
                            undefined ->
                                [];
                            #{addr := BA} ->
                                [{broadaddr, BA}]
                        end,
            DstAddr = case maps:get(dstaddr, If, undefined) of
                          undefined ->
                              [];
                          #{addr := DA} ->
                              [{dstaddr, DA}]
                      end,
            ?DBG("~w entry:"
                 "~n   Flags:      ~p"
                 "~n   Addr:       ~p"
                 "~n   Mask:       ~p"
                 "~n   Broad Addr: ~p"
                 "~n   Dest Addr:  ~p",
                 [Fam, Flags, Addr, Mask, BroadAddr, DstAddr]),
            case Fam of
                inet ->
                    Flags4_0 = maps:get(flags, IfOpts4_0, []),
                    Flags4   = Flags4_0 ++ (Flags -- Flags4_0),
                    Addrs4_0 = maps:get(addrs, IfOpts4_0, []),
                    IfOpts4  =
                        IfOpts4_0#{flags  => Flags4,
                                   addrs  =>
                                       DstAddr ++
                                       BroadAddr ++
                                       [{netmask, Mask},
                                        {addr,    Addr}] ++
                                       Addrs4_0},
                    net_ifs2ifopts(Ifs, IfOpts4, IfOpts6_0, IfOptsHw_0);
                inet6 ->
                    Flags6_0 = maps:get(flags, IfOpts6_0, []),
                    Flags6   = Flags6_0 ++ (Flags -- Flags6_0),
                    Addrs6_0 = maps:get(addrs, IfOpts6_0, []),
                    IfOpts6 =
                        IfOpts6_0#{flags  => Flags6,
                                   addrs  =>
                                       DstAddr ++
                                       BroadAddr ++
                                       [{netmask, Mask},
                                        {addr,    Addr}] ++
                                       Addrs6_0},
                    net_ifs2ifopts(Ifs, IfOpts4_0, IfOpts6, IfOptsHw_0)
            end;

        #{flags := Flags} ->
            ?DBG("other entry => retain flags"
                 "~n   ~p", [If]),
            %% Reuse the IPv4 opts
            net_ifs2ifopts(Ifs,
                           IfOpts4_0#{flags => Flags}, IfOpts6_0, IfOptsHw_0)
    end.

net_flags_to_inet_flags(Flags) ->
    net_flags_to_inet_flags(Flags, []).

net_flags_to_inet_flags([], OutFlags) ->
    lists:reverse(net_flags_maybe_add_running(OutFlags));
net_flags_to_inet_flags([InFlag|InFlags], OutFlags) ->
    case net_flag_to_inet_flag(InFlag) of
        {value, OutFlag} ->
            net_flags_to_inet_flags(InFlags, [OutFlag | OutFlags]);
        false ->
            net_flags_to_inet_flags(InFlags, OutFlags)
    end.

net_flags_maybe_add_running(Flags) ->
    case lists:member(running, Flags) of
        true ->
            Flags;
        false ->
            case lists:member(up, Flags) of
                true ->
                    [running | Flags];
                false ->
                    Flags
            end
    end.


%% Should we do this as map instead?
%% #{up          => up,
%%   broadcast   => broadcast,
%%   loopback    => loopback,
%%   pointopoint => pointtopoint,
%%   running     => running,
%%   multicast   => multicast},
net_flag_to_inet_flag(InFlag) ->
    InetFlags = [up, broadcast, loopback,
                 {pointopoint, pointtopoint},
                 running, multicast],
    net_flag_to_inet_flag(InFlag, InetFlags).

net_flag_to_inet_flag(_InFlag, []) ->
    false;
net_flag_to_inet_flag(InFlag, [InFlag|_]) ->
    {value, InFlag};
net_flag_to_inet_flag(InFlag, [{InFlag, OutFlag}|_]) ->
    {value, OutFlag};
net_flag_to_inet_flag(InFlag, [_|InetFlags]) ->
    net_flag_to_inet_flag(InFlag, InetFlags).



-doc """
Get interface names and addresses.

Returns a list of 2-tuples containing interface names and the interfaces'
addresses. `Ifname` is a Unicode string and `Ifopts` is a list of interface
address description tuples.

The interface address description tuples are documented under
the type of the [`Ifopts`](`t:getifaddrs_ifopts/0`) value.
""".
-doc(#{since => <<"OTP R14B01">>}).
-spec getifaddrs() ->
          {'ok', [{Ifname :: string(),
                   Ifopts :: getifaddrs_ifopts()}]}
              | {'error', posix()}.

getifaddrs() ->
    do_getifaddrs(inet_backend(), []).


%% --------------------------------------------------------------------------

-doc false.
-spec getiflist(
        [Option :: inet_backend() | {netns, Namespace :: file:filename_all()}]
        | socket()) ->
                       {'ok', [string()]} | {'error', posix()}.

getiflist([{inet_backend, Backend}|Opts]) ->
    do_getiflist(Backend, Opts);
getiflist(Opts) when is_list(Opts) ->
    do_getiflist(inet_backend(), Opts);
getiflist(?module_socket(GenSocketMod, ESock) = _Socket)
  when is_atom(GenSocketMod) ->
    do_getiflist('socket', ESock);
getiflist(Socket) when is_port(Socket) ->
    do_getiflist('inet', Socket).

do_getiflist(Backend, Opts) when is_list(Opts) ->
    withsocket(fun(S) -> do_getiflist2(Backend, S) end, Backend, Opts);
do_getiflist(Backend, Socket) ->
    do_getiflist2(Backend, Socket).


do_getiflist2('socket' = _Backend, Socket) ->
    net_getiflist(Socket);
do_getiflist2('inet' = _Backend, Socket) when is_port(Socket) ->
    inet_getiflist(Socket).

net_getiflist(Socket) ->
    case socket:ioctl(Socket, gifconf) of
        {ok, Interfaces} ->
            Names = [Name || #{name := Name,
                               addr := #{family := Fam}} <-
                                 Interfaces, ((Fam =:= inet) orelse
                                              (Fam =:= inet6))],
            {ok, ensure_unique_names(Names)};
        {error, _} = ERROR ->
            ERROR
    end.

ensure_unique_names(Names) ->
    ensure_unique_names(Names, []).

ensure_unique_names([], Acc) ->
    lists:reverse(Acc);
ensure_unique_names([Name|Names], Acc) ->
    case lists:member(Name, Acc) of
        true ->
            ensure_unique_names(Names, Acc);
        false ->
            ensure_unique_names(Names, [Name|Acc])
    end.


inet_getiflist(Socket) ->
    prim_inet:getiflist(Socket).


-doc false.
-spec getiflist() -> {'ok', [string()]} | {'error', posix()}.

getiflist() ->
    do_getiflist(inet_backend(), []).


%% --------------------------------------------------------------------------

-doc false.
-spec ifget(Socket :: socket(),
            Name :: string() | atom(),
	    Opts :: [if_getopt()]) ->
	{'ok', [if_getopt_result()]} | {'error', posix()}.

ifget(?module_socket(GenSocketMod, ESock) = _Socket, Name, Opts)
  when is_atom(GenSocketMod) ->
    do_ifget('socket', ESock, Name, Opts);
ifget(Socket, Name, Opts) when is_port(Socket) ->
    do_ifget('inet', Socket, Name, Opts).


do_ifget('socket', Socket, Name, Opts) ->
    esock_ifget(Socket, Name, Opts);
do_ifget('inet',   Socket, Name, Opts) ->
    prim_inet:ifget(Socket, Name, Opts).


esock_ifget(ESock, Name, Opts) ->
    esock_ifget(ESock, Name, Opts, []).

esock_ifget(_ESock, _Name, [] = _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
esock_ifget(ESock, Name, [Opt|Opts], Acc) ->
    case do_esock_ifget(ESock, Name, Opt) of
        {ok, Value} ->
            esock_ifget(ESock, Name, Opts, [{Opt, Value}|Acc]);
        {error, _} ->
            esock_ifget(ESock, Name, Opts, Acc)
    end.

%% We should really check if these ioctl get requests are supported:
%% socket:is_supported(ioctl_requests, Req).
%% But since the error will just result in a missing result value
%% anyway (which the user needs to handle), unless there is some
%% alternative method (see hwaddr) we just return it...
do_esock_ifget(ESock, Name, 'addr') ->
    case socket:ioctl(ESock, gifaddr, Name) of
        {ok, #{addr := Addr}} ->
            {ok, Addr};
        {error, _} = ERROR ->
            ERROR
    end;
do_esock_ifget(ESock, Name, 'broadaddr') ->
    case socket:ioctl(ESock, gifbrdaddr, Name) of
        {ok, #{addr := Addr}} ->
            {ok, Addr};
        {error, _} = ERROR ->
            ERROR
    end;
do_esock_ifget(ESock, Name, 'dstaddr') ->
    case socket:ioctl(ESock, gifdstaddr, Name) of
        {ok, #{addr := Addr}} ->
            {ok, Addr};
        {error, _} = ERROR ->
            ERROR
    end;
do_esock_ifget(ESock, Name, 'mtu') ->
    socket:ioctl(ESock, gifmtu, Name);
do_esock_ifget(ESock, Name, 'netmask') ->
    case socket:ioctl(ESock, gifnetmask, Name) of
        {ok, #{addr := Addr}} ->
            {ok, Addr};
        {error, _} = ERROR ->
            ERROR
    end;
do_esock_ifget(ESock, Name, 'flags') ->
    socket:ioctl(ESock, gifflags, Name);
do_esock_ifget(ESock, Name, 'hwaddr') ->
    case use_ioctl_for_hwaddr() of
        {ok, Req} ->
            case socket:ioctl(ESock, Req, Name) of
                {ok, #{family := _Fam,
                       addr   := <<HWADDRBin:6/binary, _/binary>> = _Addr}} ->
                    {ok, binary_to_list(HWADDRBin)};
                {error, _} ->
                    %% Last effort...
                    hwaddr_from_net_getifaddrs(Name)
            end;
        error ->
            %% Try getifaddrs instead
            hwaddr_from_net_getifaddrs(Name)
    end.

use_ioctl_for_hwaddr() ->
    case socket:is_supported(ioctl_requests, gifhwaddr) of
        true ->
            {ok, gifhwaddr};
        false ->
            case socket:is_supported(ioctl_requests, genhwaddr) of
                true ->
                    {ok, genhwaddr};
                false ->
                    error
            end
    end.

hwaddr_from_net_getifaddrs(Name) ->
    %% Platforms "use" different Family.
    %% FreeBSD use 'link', Linux use 'packet'.
    Filter = fun(#{name := IF,
                   addr := #{family := Fam}})
                   when (IF =:= Name) andalso
                        ((Fam =:= link) orelse (Fam =:= packet)) ->
                     %% io:format("+ right interface and family~n", []),
                     true;
                (#{name := IF,
                   addr := #{family := _Fam}})
                   when (IF =:= Name) ->
                     %% io:format("- right interface but wrong family: "
                     %%           "~n   Fam: ~p (link|packet)"
                     %%           "~n", [_Fam]),
                     false;
                (#{name := _IF,
                   addr := #{family := Fam}})
                   when ((Fam =:= link) orelse (Fam =:= packet)) ->
                     %% io:format("- right family but wrong interface"
                     %%           "~n   IF: ~p (~p)"
                     %%           "~n", [_IF, Name]),
                     false;
                (#{name := _IF,
                   addr := #{family := _Fam}}) ->
                     %% io:format("- wrong interface and family: "
                     %%           "~n   IF:  ~p (~p)"
                     %%           "~n   Fam: ~p (link|packet)"
                     %%           "~n", [_IF, Name, _Fam]),
                     false;
                (_X) ->
                     %% io:format("- just plain wrong: "
                     %%           "~n   X: ~p"
                     %%           "~n", [_X]),
                     false
             end,
    case net:getifaddrs(Filter) of
        {ok, [#{addr := #{family := packet,
                          addr   := HwAddrBin}}|_]} ->
            {ok, binary_to_list(HwAddrBin)};
        {ok, [#{addr := #{family := link,
                          nlen   := NLen,
                          alen   := ALen,
                          data   := Data}}|_]} ->
            case Data of
                <<_:NLen/binary, ABin:ALen/binary, _/binary>> ->
                    {ok, binary_to_list(ABin)};
                _ -> %% Ouch - what is this? malformed data?
                    {error, unknown}
            end;
        {ok, _} -> %% Ouch - we got something but don't know how to decode it
            {error, unknown};
        {error, _} = ERROR ->
            ERROR
    end.
        

-doc false.
-spec ifget(
        Name :: string() | atom(),
        Opts :: [inet_backend() |
                 if_getopt() |
                 {netns, Namespace :: file:filename_all()}]) ->
	{'ok', [if_getopt_result()]} | {'error', posix()}.

ifget(Name, [{inet_backend, Backend}|Opts]) ->
    do_ifget(Backend, Name, Opts);
ifget(Name, Opts) ->
    do_ifget(inet_backend(), Name, Opts).

do_ifget(Backend, Name, Opts) ->
    {NSOpts, IFOpts} =
        lists:partition(
          fun ({netns,_}) -> true;
              (_) -> false
          end, Opts),
    withsocket(fun(S) ->
                       do_ifget(Backend, S, Name, IFOpts)
               end,
               Backend, NSOpts).


%% --------------------------------------------------------------------------

-doc false.
-spec ifset(Socket :: socket(),
            Name :: string() | atom(),
	    Opts :: [if_setopt()]) ->
	'ok' | {'error', posix()}.

ifset(?module_socket(GenSocketMod, ESock) = _Socket, Name, Opts)
  when is_atom(GenSocketMod) ->
    do_ifset('socket', ESock, Name, Opts);
ifset(Socket, Name, Opts) when is_port(Socket) ->
    do_ifset('inet', Socket, Name, Opts).

do_ifset('socket', Socket, Name, Opts) ->
    esock_ifset(Socket, Name, Opts);
do_ifset('inet', Socket, Name, Opts) ->
    prim_inet:ifset(Socket, Name, Opts).

esock_ifset(_Socket, _Name, [] = _Opts) ->
    ok;
esock_ifset(Socket, Name, [{Req, Value}|Opts]) ->
    case do_esock_ifset(Socket, Name, Req, Value) of
        ok ->
            esock_ifset(Socket, Name, Opts);
        {error, _} = ERROR ->
            ERROR
    end.

do_esock_ifset(Socket, Name, 'addr' = _Req, Addr) ->
    do_esock_ifset2(Socket, Name, sifaddr, Addr);
do_esock_ifset(Socket, Name, 'broadaddr' = _Req, Addr) ->
    do_esock_ifset2(Socket, Name, sifbrdaddr, Addr);
do_esock_ifset(Socket, Name, 'dstdaddr' = _Req, Addr) ->
    do_esock_ifset2(Socket, Name, sifdstaddr, Addr);
do_esock_ifset(Socket, Name, 'mtu' = _Req, MTU) ->
    do_esock_ifset2(Socket, Name, sifmtu, MTU);
do_esock_ifset(Socket, Name, 'netmask' = _Req, Addr) ->
    do_esock_ifset2(Socket, Name, sifnetmask, Addr);
do_esock_ifset(Socket, Name, 'flags' = _Req, Flags) ->
    do_esock_ifset2(Socket, Name, sifflags, Flags);
do_esock_ifset(Socket, Name, 'hwaddr' = _Req, Addr) ->
    do_esock_ifset2(Socket, Name, sifhwaddr, Addr).

do_esock_ifset2(Socket, Name, Req, Value) ->
    try socket:ioctl(Socket, Req, Name, Value) of
        ok ->
            ok;
        {error, _} = ERROR -> % This is "einval" stuff...
            ERROR
    catch
        error:notsup -> % These are siently ignored
            ok
    end.

    
-doc false.
-spec ifset(
        Name :: string() | atom(),
        Opts :: [inet_backend() |
                 if_setopt() |
                 {netns, Namespace :: file:filename_all()}]) ->
	'ok' | {'error', posix()}.

ifset(Name, [{inet_backend, Backend}|Opts]) ->
    do_ifset(Backend, Name, Opts);
ifset(Name, Opts) ->
    do_ifset(inet_backend(), Name, Opts).

do_ifset(Backend, Name, Opts) ->
    {NSOpts, IFOpts} =
        lists:partition(
          fun ({netns,_}) -> true;
              (_) -> false
          end, Opts),
    withsocket(fun(S) ->
                       do_ifset(Backend, S, Name, IFOpts)
               end,
               Backend, NSOpts).


%% --------------------------------------------------------------------------

-doc false.
-spec getif() ->
	{'ok', [{ip_address(), ip_address() | 'undefined', ip_address()}]} |
	{'error', posix()}.

getif() ->
    withsocket(fun(S) -> getif(S) end, inet_backend(), []).

%% backwards compatible getif
-doc false.
-spec getif(
        [Option :: inet_backend() | {netns, Namespace :: file:filename_all()}]
        | socket()) ->
	{'ok', [{ip_address(), ip_address() | 'undefined', ip_address()}]} |
	{'error', posix()}.

getif([{inet_backend, Backend}|Opts]) ->
    withsocket(fun(S) -> getif(Backend, S) end, Backend, Opts);
getif(Opts) when is_list(Opts) ->
    Backend = inet_backend(),
    withsocket(fun(S) -> getif(Backend, S) end, Backend, Opts);
getif(?module_socket(GenSocketMod, ESock) = _Socket)
  when is_atom(GenSocketMod) ->
    getif('socket', ESock);
getif(Socket) when is_port(Socket) ->
    getif('inet', Socket).


getif('socket', Socket) ->
    net_getif(Socket);
getif('inet', Socket) ->
    inet_getif(Socket).

net_getif(Socket) ->
    GetIfList = fun() -> net_getiflist(Socket) end,
    IfGet     = fun(Name) ->
                        esock_ifget(Socket, Name,
                                    [addr, broadaddr, netmask])
                end,
    do_getif(GetIfList, IfGet).

inet_getif(Socket) ->
    GetIfList = fun() -> inet_getiflist(Socket) end,
    IfGet     = fun(Name) ->
                        prim_inet:ifget(Socket, Name,
                                        [addr, broadaddr, netmask])
                end,
    do_getif(GetIfList, IfGet).

do_getif(GetIfList, IfGet) ->
    case GetIfList() of
        {ok, IfList} ->
            {ok, lists:foldl(
                   fun(Name,Acc) ->
                           case IfGet(Name) of
                               {ok, [{addr, A}, {broadaddr, B}, {netmask,M}]} ->
                                   [{A, B, M}|Acc];
                               %% Some interfaces does not have a b-addr
			       {ok,[{addr,A},{netmask,M}]} ->
				   [{A,undefined,M}|Acc];
                               _ ->
                                   Acc
                           end
                   end, [], IfList)};
        Error -> Error
    end.


%% --------------------------------------------------------------------------

withsocket(Fun) ->
    withsocket(Fun, []).
%%
withsocket(Fun, Opts) ->
    inet_withsocket(Fun, Opts).


withsocket(Fun, 'socket', Opts) ->
    esock_withsocket(Fun, Opts);
withsocket(Fun, 'inet', Opts) ->
    inet_withsocket(Fun, Opts).

esock_withsocket(Fun, Opts) ->
    EOpts =
        case Opts of
            [{netns, Namespace}] ->
                #{netns => Namespace};
            [] ->
                #{}
        end,
    case socket:open(inet, dgram, default, EOpts) of
        {ok, Socket} ->
            Res = Fun(Socket),
            _ = socket:close(Socket),
            Res;
        {error, _} = ERROR ->
            ERROR
    end.

inet_withsocket(Fun, Opts) ->
    case inet_udp:open(0, Opts) of
        {ok,Socket} ->
            Res = Fun(Socket),
            inet_udp:close(Socket),
            Res;
        Error ->
            Error
    end.



-doc false.
pushf(_Socket, Fun, _State) when is_function(Fun) ->
    {error, einval}.

-doc false.
popf(_Socket) ->
    {error, einval}.


%% --------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the hostname is not cached any more because this
% could cause troubles on at least windows with plug-and-play
% and network-cards inserted and removed in conjunction with
% use of the DHCP-protocol
% should never fail

-doc """
Get the local hostname.

Returns the local hostname. Never fails.
""".
-spec gethostname() -> {'ok', Hostname} when
      Hostname :: string().

%%% XXX gethostname() -> net:gethostname().

gethostname() ->
    do_gethostname(inet_backend()).

do_gethostname('socket') ->
    case net:gethostname() of
        {ok, Hostname} ->
            %% If its a long name (including domain), shorten to only name
            {H,_} = lists:splitwith(fun($.)->false;(_)->true end, Hostname),
            {ok, H};
        {error, _} ->
            {ok, "nohost.nodomain"}
    end;
do_gethostname('inet') ->
    case inet_udp:open(0,[]) of
	{ok,U} ->
	    {ok,Res} = gethostname(U),
	    inet_udp:close(U),
	    {Res2,_} = lists:splitwith(fun($.)->false;(_)->true end,Res),
	    {ok, Res2};
	_ ->
	    {ok, "nohost.nodomain"}
    end.

-doc false.
-spec gethostname(Socket :: socket()) ->
	{'ok', string()} | {'error', posix()}.

%% The esock version of should never really be called. Its supposed to 
%% be a utility function for gethostname/0. But just in case...
gethostname(?module_socket(GenSocketMod, _) = _Socket)
  when is_atom(GenSocketMod) ->
    %% We do not really need the socket for anything...
    net:gethostname();
gethostname(Socket) ->
    prim_inet:gethostname(Socket).


%% --------------------------------------------------------------------------

-doc(#{equiv => getstat/2}).
-spec getstat(Socket) ->
	{ok, OptionValues} | {error, posix()} when
      Socket :: socket(),
      OptionValues :: [{stat_option(), integer()}].

getstat(Socket) ->
    getstat(Socket, stats()).

-doc """
Get one or more statistics options for a socket.

[`getstat(Socket)`](`getstat/1`) is equivalent to
[`getstat(Socket, [recv_avg, recv_cnt, recv_dvi, recv_max, recv_oct, send_avg, send_cnt, send_pend, send_max, send_oct])`](`getstat/2`).

The following options are available:

- **`recv_avg`** - Average size of packets, in bytes, received by the socket.

- **`recv_cnt`** - Number of packets received by the socket.

- **`recv_dvi`** - Average packet size deviation, in bytes, received by the
  socket.

- **`recv_max`** - Size of the largest packet, in bytes, received by the socket.

- **`recv_oct`** - Number of bytes received by the socket.

- **`send_avg`** - Average size of packets, in bytes, sent from the socket.

- **`send_cnt`** - Number of packets sent from the socket.

- **`send_pend`** - Number of bytes waiting to be sent by the socket.

- **`send_max`** - Size of the largest packet, in bytes, sent from the socket.

- **`send_oct`** - Number of bytes sent from the socket.
""".
-spec getstat(Socket, Options) ->
	{ok, OptionValues} | {error, posix()} when
      Socket :: socket(),
      Options :: [stat_option()],
      OptionValues :: [{stat_option(), integer()}].

getstat(?module_socket(GenSocketMod, _) = Socket, What)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket, What);
getstat(Socket, What) ->
    prim_inet:getstat(Socket, What).


%% --------------------------------------------------------------------------

-doc """
Resolve a hostname to a [`#hostent{}`](`t:hostent/0`) record.

Returns a [`#hostent{}`](`t:hostent/0`) record for the host
with the specified `Hostname`.

This function uses the resolver, which is often the native (OS) resolver.

If resolver option `inet6` is `true`, an IPv6 address is looked up.

See [ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`) for
information about the resolver configuration.

A quirk of many resolvers is that an integer string is interpreted
as an IP address. For instance, the integer string "3232235521"
and the string "192.168.0.1" are both translated
to the IP address `{192,168,0,1}`.
""".
-spec gethostbyname(Hostname) -> {ok, Hostent} | {error, posix()} when
      Hostname :: hostname(),
      Hostent :: hostent().

gethostbyname(Name) ->
    case inet_db:res_option(inet6) of
	true ->
	    gethostbyname_tm(Name, inet6, false);
	false ->
	    gethostbyname_tm(Name, inet, false)
    end.

-doc """
Resolve a hostname to a [`#hostent{}`](`t:hostent/0`) record,
in a specific address family.

Returns a [`#hostent{}`](`t:hostent/0`) record for the host
with the specified `Hostname`, restricted to the specified address `Family`.

See also `gethostbyname/1`.
""".
-spec gethostbyname(Hostname, Family) ->
                           {ok, Hostent} | {error, posix()} when
      Hostname :: hostname(),
      Family :: address_family(),
      Hostent :: hostent().

gethostbyname(Name, Family) ->
    gethostbyname_tm(Name, Family, false).

-doc false.
-spec gethostbyname(Name :: hostname(),
	            Family :: address_family(),
	            Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', #hostent{}} | {'error', posix()}.

gethostbyname(Name, Family, Timeout) ->
    Timer = start_timer(Timeout),
    Res = gethostbyname_tm(Name,Family,Timer),
    _ = stop_timer(Timer),
    Res.

-doc false.
gethostbyname_tm(Name, Family, Timer) ->
    %% ?DBG([{name, Name}, {family, Family}, {timer, Timer}]),
    Opts0 = inet_db:res_option(lookup),
    %% ?DBG([{opts0, Opts0}]),
    Opts =
	case (lists:member(native, Opts0) orelse
	      lists:member(string, Opts0) orelse
	      lists:member(nostring, Opts0)) of
	    true ->
		Opts0;
	    false ->
		[string|Opts0]
	end,
    %% ?DBG([{opts, Opts}]),
    gethostbyname_tm(Name, Family, Timer, Opts).


-doc """
Resolve (reverse) an address to a [`#hostent{}`](`t:hostent/0`) record.

Returns a [`#hostent{}`](`t:hostent/0`) record for the host
with the specified address.
""".
-spec gethostbyaddr(Address) -> {ok, Hostent} | {error, posix()} when
      Address :: string() | ip_address(),
      Hostent :: hostent().

gethostbyaddr(Address) ->
    gethostbyaddr_tm(Address, false).

-doc false.
-spec gethostbyaddr(Address :: string() | ip_address(),
	            Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', #hostent{}} | {'error', posix()}.

gethostbyaddr(Address,Timeout) ->
    Timer = start_timer(Timeout),
    Res = gethostbyaddr_tm(Address, Timer),
    _ = stop_timer(Timer),
    Res.

-doc false.
gethostbyaddr_tm(Address,Timer) ->
    gethostbyaddr_tm(Address, Timer, inet_db:res_option(lookup)).


-doc false.
-spec socket_to_list(Socket) -> list() when
      Socket :: socket().

socket_to_list({'$inet', GenSocketMod, _} = Socket)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
socket_to_list(Socket) when is_port(Socket) ->
    erlang:port_to_list(Socket).



-doc """
Get information about a socket.

Returns a term containing miscellaneous information about a socket.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec info(Socket) -> Info when
      Socket :: socket(),
      Info :: term().

info({'$inet', GenSocketMod, _} = Socket)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
info(Socket) when is_port(Socket) ->
    case port_info(Socket) of
	#{states := _} = PortInfo ->
            case inet:getopts(Socket, [active]) of
                {ok, [{active, Active}]} ->
                    PortInfo#{active => Active};
                _ ->
                    PortInfo
            end;
	PortInfo0 ->
	    %% Its actually possible to call this function for non-socket ports,
	    %% but in that case we have no status or statistics.
	    PortInfo1 =
		case prim_inet:getstatus(Socket) of
		    {ok, State} ->
			PortInfo0#{states => State};
		    _ ->
			PortInfo0
		end,
	    case getstat(Socket) of
		{ok, Stats0} ->
		    PortInfo1#{counters => maps:from_list(Stats0)};
		_ ->
		    PortInfo1
	    end
    end.

port_info(P) when is_port(P) ->
    case erlang:port_info(P) of
	PI0 when is_list(PI0) ->
	    PI1 = port_info(PI0, [connected, links, input, output]) ++
		[erlang:port_info(P, memory), erlang:port_info(P, monitors)],
	    PI2 = pi_replace([{connected, owner}], PI1),
	    maps:from_list(PI2);
	_ ->
	    #{states => [closed]}
    end.

port_info(PI, Items) when is_list(PI) ->
    port_info(PI, Items, []).

port_info(_PI, [], Acc) ->
    Acc;
port_info(PI, [Item | Items], Acc) ->
    Val = proplists:get_value(Item, PI),
    port_info(PI, Items, [{Item, Val} | Acc]).

pi_replace([], Items) ->
    Items;
pi_replace([{Key1, Key2}|Keys], Items) ->
    case lists:keysearch(Key1, 1, Items) of
        {value, {Key1, Value}} ->
            Items2 = lists:keyreplace(Key1, 1, Items, {Key2, Value}),
            pi_replace(Keys, Items2);
        false ->
            pi_replace(Keys, Items)
    end.

-doc false.
-spec ip(Ip :: ip_address() | string() | atom()) ->
	{'ok', ip_address()} | {'error', posix()}.

ip({A,B,C,D}) when ?ip(A,B,C,D) ->
    {ok, {A,B,C,D}};
ip(Name) ->
    case gethostbyname(Name) of
	{ok, Ent} ->
	    {ok, hd(Ent#hostent.h_addr_list)};
	Error -> Error
    end.

-doc """
Test for an IPv4 address.

Tests if the argument `IPv4Address` is an `t:ip4_address/0`
and if so returns `true`, otherwise `false`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec is_ipv4_address(IPv4Address) -> boolean() when
      IPv4Address :: ip4_address() | term().
is_ipv4_address({A,B,C,D}) when ?ip(A,B,C,D) ->
    true;
is_ipv4_address(_) ->
    false.

-doc """
Test for an IPv6 address.

Tests if the argument `IPv6Address` is an `t:ip6_address/0`
and if so returns `true`, otherwise `false`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec is_ipv6_address(IPv6Address) -> boolean() when
      IPv6Address :: ip6_address() | term().
is_ipv6_address({A,B,C,D,E,F,G,H}) when ?ip6(A,B,C,D,E,F,G,H) ->
    true;
is_ipv6_address(_) ->
    false.

-doc """
Test for an IP address.

Tests if the argument `IPAddress` is an `t:ip_address/0`
and if so returns `true`, otherwise `false`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec is_ip_address(IPAddress) -> boolean() when
      IPAddress :: ip_address() | term().
is_ip_address(Address) ->
    is_ipv4_address(Address) orelse is_ipv6_address(Address).

%% This function returns the erlang port used (with inet_drv)

-doc false.
-spec getll(Socket :: socket()) -> {'ok', socket()}.

getll(Socket) when is_port(Socket) ->
    {ok, Socket}.

%%
%% Return the internal file descriptor number
%%

-doc false.
-spec getfd(Socket :: socket()) ->
	{'ok', non_neg_integer()} | {'error', posix()}.

getfd(?module_socket(GenSocketMod, ESock) = _Socket)
  when is_atom(GenSocketMod) ->
    socket:getopt(ESock, otp, fd);
getfd(Socket) ->
    prim_inet:getfd(Socket).

%%
%% Lookup an ip address
%%

-doc """
Resolve a host to an address, in a specific addresss family.

Returns the [IP address](`t:ip_address/0`) for `Host` as a tuple of integers.
`Host` can be an [IP address](`t:ip_address/0`), a single `t:hostname/0`,
or a fully qualified `t:hostname/0`.
""".
-spec getaddr(Host, Family) -> {ok, Address} | {error, posix()} when
      Host :: ip_address() | hostname(),
      Family :: address_family(),
      Address :: ip_address().

getaddr(Address, Family) ->
    getaddr(Address, Family, infinity).

-doc false.
-spec getaddr(Host :: ip_address() | hostname(),
	      Family :: address_family(),
	      Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', ip_address()} | {'error', posix()}.

getaddr(Address, Family, Timeout) ->
    %% ?DBG([{address, Address}, {family, Family}, {timeout, Timeout}]),
    Timer = start_timer(Timeout),
    Res   = getaddr_tm(Address, Family, Timer),
    %% ?DBG([{res, Res}]),
    _     = stop_timer(Timer),
    Res.

-doc false.
getaddr_tm(Address, Family, Timer) ->
    %% ?DBG([{address, Address}, {family, Family}, {timer, Timer}]),
    case getaddrs_tm(Address, Family, Timer) of
	{ok, [IP|_]} ->
	    %% ?DBG([{ip, IP}]),
	    {ok, IP};
	Error ->
	    %% ?DBG([{error, Error}]),
	    Error
    end.

-doc """
Resolve a host to a list of addresses, in a specific address family.

Returns a list of all IP addresses for `Host`.
`Host` can be an [IP address](`t:ip_address/0`),
a single `t:hostname/0`, or a fully qualified `t:hostname/0`.
""".
-spec getaddrs(Host, Family) ->
	{ok, Addresses} | {error, posix()} when
      Host :: ip_address() | hostname(),
      Family :: address_family(),
      Addresses :: [ip_address()].

getaddrs(Address, Family) ->
    getaddrs(Address, Family, infinity).

-doc false.
-spec getaddrs(Host :: ip_address() | string() | atom(),
	       Family :: address_family(),
	       Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', [ip_address()]} | {'error', posix()}.

getaddrs(Address, Family, Timeout) ->
    Timer = start_timer(Timeout),
    Res = getaddrs_tm(Address, Family, Timer),
    _ = stop_timer(Timer),
    Res.

-doc false.
-spec getservbyport(Port :: port_number(), Protocol :: atom() | string()) ->
	{'ok', string()} | {'error', posix()}.

getservbyport(Port, Protocol) ->
    case inet_backend() of
        'inet' ->
            inet_getservbyport(Port, Protocol);
        'socket' ->
            net_getservbyport(Port, Protocol)
    end.

inet_getservbyport(Port, Protocol) ->
    withsocket(fun(S) -> prim_inet:getservbyport(S, Port, Protocol) end).

net_getservbyport(Port, Protocol) when is_list(Protocol) ->
    net_getservbyport(Port, list_to_atom(Protocol));
net_getservbyport(Port, Protocol) when is_atom(Protocol) ->
    net:getservbyport(Port, Protocol).

-doc false.
-spec getservbyname(Name :: atom() | string(),
	            Protocol :: atom() | string()) ->
	{'ok', port_number()} | {'error', posix()}.

getservbyname(Name, Protocol) when is_atom(Name) ->
    getservbyname(atom_to_list(Name), Protocol);
getservbyname(Name, Protocol) when is_list(Name) ->
    case inet_backend() of
        'inet' ->
            inet_getservbyname(Name, Protocol);
        'socket' ->
            net_getservbyname(Name, Protocol)
    end.

inet_getservbyname(Name, Protocol) ->
    withsocket(fun(S) -> prim_inet:getservbyname(S, Name, Protocol) end).

net_getservbyname(Name, Protocol) when is_list(Protocol) ->
    net_getservbyname(Name, list_to_atom(Protocol));
net_getservbyname(Name, Protocol) when is_atom(Protocol) ->
    net:getservbyname(Name, Protocol).


-doc "Parse an `t:ip_address/0` to an IPv4 or IPv6 address string.".
-doc(#{since => <<"OTP R16B02">>}).
-spec ntoa(IpAddress) -> Address | {error, einval} when
      Address :: string(),
      IpAddress :: ip_address().
ntoa(Addr) ->
    inet_parse:ntoa(Addr).

-doc """
Parse (relaxed) an IPv4 address string to an `t:ip4_address/0`.

Accepts a short form IPv4 address string (less than 4 fields)
such as `"127.1"` or `"0x7f000001"`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_ipv4_address(Address) ->
	{ok, IPv4Address} | {error, einval} when
      Address :: string(),
      IPv4Address :: ip4_address().
parse_ipv4_address(Addr) ->
    inet_parse:ipv4_address(Addr).

-doc """
Parse (relaxed) an IPv6 address string to an `t:ip6_address/0`.

Also accepts a (relaxed) IPv4 address string like `parse_ipv4_address/1`
and returns an IPv4-mapped IPv6 address.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_ipv6_address(Address) ->
	{ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_ipv6_address(Addr) ->
    inet_parse:ipv6_address(Addr).

-doc """
Parse an IPv4 address string to an `t:ip4_address/0`.

Requires an IPv4 address string containing four fields,
that is; _not_ a short form address string.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_ipv4strict_address(Address) ->
	{ok, IPv4Address} | {error, einval} when
      Address :: string(),
      IPv4Address :: ip4_address().
parse_ipv4strict_address(Addr) ->
    inet_parse:ipv4strict_address(Addr).

-doc """
Parse an IPv6 address string to an `t:ip6_address/0`.

_Doesn't_ accept an IPv4 address string.  An IPv6 address string, though,
allows an IPv4 tail like this: `"::127.0.0.1"`
(which is the same as `"::7f00:0001"`).
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_ipv6strict_address(Address) ->
	{ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_ipv6strict_address(Addr) ->
    inet_parse:ipv6strict_address(Addr).

-doc """
Parse an IP address string to an `t:ip_address/0`.

Returns an `t:ip4_address/0` or an `t:ip6_address/0` depending
on which parsing that succeeds.

Accepts a short form IPv4 address string like `parse_ipv4_address/1`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_address(Address) ->
	{ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address().
parse_address(Addr) ->
    inet_parse:address(Addr).

-doc false.
-spec parse_address(Address, inet) ->
          {ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address();
                   (Address, inet6) ->
          {ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_address(Addr, inet) ->
    inet_parse:ipv4_address(Addr);
parse_address(Addr, inet6) ->
    inet_parse:ipv6_address(Addr).

-doc """
Parse an IP address string to an `t:ip_address/0`.

Like `parse_address/1` but _doesn't_ accept a short form IPv4 address string.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_strict_address(Address) ->
	{ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address().
parse_strict_address(Addr) ->
    inet_parse:strict_address(Addr).

-doc false.
-spec parse_strict_address(Address, inet) ->
          {ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address();
                   (Address, inet6) ->
          {ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_strict_address(Addr, inet) ->
    inet_parse:ipv4strict_address(Addr);
parse_strict_address(Addr, inet6) ->
    inet_parse:ipv6strict_address(Addr).

-doc """
Convert between an IPv4 address and an IPv4-mapped IPv6 address.

Convert an IPv4 address to an IPv4-mapped IPv6 address or the reverse.
When converting from an IPv6 address all but the 2 low words are ignored
so this function also works on some other types of IPv6 addresses
than IPv4-mapped.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec ipv4_mapped_ipv6_address(ip_address()) -> ip_address().
ipv4_mapped_ipv6_address({D1,D2,D3,D4})
  when (D1 bor D2 bor D3 bor D4) < 256 ->
    {0,0,0,0,0,16#ffff,(D1 bsl 8) bor D2,(D3 bsl 8) bor D4};
ipv4_mapped_ipv6_address({D1,D2,D3,D4,D5,D6,D7,D8})
  when (D1 bor D2 bor D3 bor D4 bor D5 bor D6 bor D7 bor D8) < 65536 ->
    {D7 bsr 8,D7 band 255,D8 bsr 8,D8 band 255}.

%% Return a list of available options
-doc false.
options() ->
    [
     tos, tclass, priority, reuseaddr, keepalive, dontroute, linger,
     broadcast, sndbuf, recbuf, nodelay, ipv6_v6only,
     buffer, header, active, packet, deliver, mode,
     multicast_if, multicast_ttl, multicast_loop,
     exit_on_close, high_watermark, low_watermark,
     high_msgq_watermark, low_msgq_watermark,
     send_timeout, send_timeout_close, show_econnreset
    ].

%% Return a list of statistics options

-doc false.
-spec stats() -> [stat_option(),...].

stats() ->
    [recv_oct, recv_cnt, recv_max, recv_avg, recv_dvi,
     send_oct, send_cnt, send_max, send_avg, send_pend].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:connect
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_options() ->
    [debug,
     tos, tclass, priority, reuseaddr, reuseport, reuseport_lb,
     exclusiveaddruse, keepalive,
     linger, nodelay, sndbuf, recbuf, recvtos, recvtclass, ttl, recvttl,
     header, active, packet, packet_size, buffer, mode, deliver, line_delimiter,
     exit_on_close, high_watermark, low_watermark, high_msgq_watermark,
     low_msgq_watermark, send_timeout, send_timeout_close, delay_send, raw,
     show_econnreset, bind_to_device, read_ahead].

-doc false.
connect_options(Opts, Mod) ->
    BaseOpts =
	case application:get_env(kernel, inet_default_connect_options) of
	    {ok, List} when is_list(List) ->
		NList = [{active, true} | lists:keydelete(active,1,List)],
		#connect_opts{opts = NList};
	    {ok, {active,_Bool}} ->
		#connect_opts{opts = [{active,true}]};
	    {ok, Option} ->
		#connect_opts{opts = [{active,true}, Option]};
	    _ ->
		#connect_opts{opts = [{active,true}]}
	end,
    case con_opt(Opts, BaseOpts, connect_options()) of
	{ok, R} ->
	    {ok, R#connect_opts {
		   opts = lists:reverse(R#connect_opts.opts),
		   ifaddr = Mod:translate_ip(R#connect_opts.ifaddr)
		  }};
	Error -> Error
    end.

con_opt([{raw,A,B,C}|Opts],#connect_opts{} = R,As) ->
    con_opt([{raw,{A,B,C}}|Opts],R,As);
con_opt([Opt | Opts], #connect_opts{ifaddr = IfAddr} = R, As) ->
    case Opt of
	{ifaddr, Addr} when is_map(Addr) ->
            con_opt(Opts, R#connect_opts{ ifaddr = ensure_sockaddr(Addr) }, As);
	{ifaddr, Addr} ->
            con_opt(Opts, R#connect_opts{ ifaddr = Addr }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{ip,IP} when is_map(IfAddr) ->
            con_opt(Opts, R#connect_opts{ ifaddr = IfAddr#{addr => IP} }, As);
	{ip,IP}     -> con_opt(Opts, R#connect_opts{ ifaddr = IP }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{port,P} when is_map(IfAddr) ->
            con_opt(Opts, R#connect_opts{ ifaddr = IfAddr#{port => P} }, As);
	{port,P}    -> con_opt(Opts, R#connect_opts{ port = P }, As);

	{fd,Fd}     -> con_opt(Opts, R#connect_opts{ fd = Fd }, As);

	binary      -> con_add(mode, binary, R, Opts, As);

	list        -> con_add(mode, list, R, Opts, As);

	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    con_opt(Opts, R#connect_opts { fd = [{netns,BinNS}] }, As);
		false ->
		    {error, badarg}
	    end;

        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            NOpts = lists:keydelete(active, 1, R#connect_opts.opts),
            con_opt(Opts, R#connect_opts { opts = [{active,N}|NOpts] }, As);

	{line_delimiter,C} when is_integer(C), C >= 0, C =< 255 ->
	    con_add(line_delimiter, C, R, Opts, As);

	{Name,Val} when is_atom(Name) -> con_add(Name, Val, R, Opts, As);

	_ -> {error, badarg}
    end;
con_opt([], #connect_opts{} = R, _) ->
    {ok, R}.

con_add(Name, Val, #connect_opts{} = R, Opts, AllOpts) ->
    case add_opt(Name, Val, R#connect_opts.opts, AllOpts) of
	{ok, SOpts} ->
	    con_opt(Opts, R#connect_opts { opts = SOpts }, AllOpts);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:listen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen_options() ->
    [debug,
     tos, tclass,
     priority, reuseaddr, reuseport, reuseport_lb, exclusiveaddruse, keepalive,
     linger, sndbuf, recbuf, nodelay, recvtos, recvtclass, ttl, recvttl,
     header, active, packet, buffer, mode, deliver, backlog, ipv6_v6only,
     exit_on_close, high_watermark, low_watermark, high_msgq_watermark,
     low_msgq_watermark, send_timeout, send_timeout_close, delay_send,
     packet_size, raw, show_econnreset, bind_to_device, read_ahead].

-doc false.
listen_options(Opts, Mod) ->
    BaseOpts =
	case application:get_env(kernel, inet_default_listen_options) of
	    {ok,List} when is_list(List) ->
		NList = [{active, true} | lists:keydelete(active,1,List)],
		#listen_opts{ opts = NList};
	    {ok,{active,_Bool}} ->
		#listen_opts{ opts = [{active,true}]};
	    {ok,Option} ->
		#listen_opts{ opts = [{active,true}, Option]};
	    _ ->
		#listen_opts{ opts = [{active,true}]}
	end,
    case list_opt(Opts, BaseOpts, listen_options()) of
	{ok, R} ->
	    {ok, R#listen_opts {
		   opts = lists:reverse(R#listen_opts.opts),
		   ifaddr = Mod:translate_ip(R#listen_opts.ifaddr)
		  }};
	Error -> Error
    end.

list_opt([{raw,A,B,C}|Opts], #listen_opts{} = R, As) ->
    list_opt([{raw,{A,B,C}}|Opts], R, As);
list_opt([Opt | Opts], #listen_opts{ifaddr = IfAddr} = R, As) ->
    case Opt of
	{ifaddr, Addr} when is_map(Addr) ->
            list_opt(Opts, R#listen_opts{ ifaddr = ensure_sockaddr(Addr) }, As);
	{ifaddr, Addr} ->
            list_opt(Opts, R#listen_opts{ ifaddr = Addr }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{ip,IP} when is_map(IfAddr) ->
            list_opt(Opts, R#listen_opts{ ifaddr = IfAddr#{addr => IP} }, As);
	{ip,IP}      ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{port,P} when is_map(IfAddr) ->
            list_opt(Opts, R#listen_opts{ ifaddr = IfAddr#{port => P} }, As);
	{port,P}     ->  list_opt(Opts, R#listen_opts { port = P }, As);

	{fd,Fd}      ->  list_opt(Opts, R#listen_opts { fd = Fd }, As);
	{backlog,BL} ->  list_opt(Opts, R#listen_opts { backlog = BL }, As);
	binary       ->  list_add(mode, binary, R, Opts, As);
	list         ->  list_add(mode, list, R, Opts, As);
	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    list_opt(Opts, R#listen_opts { fd = [{netns,BinNS}] }, As);
		false ->
		    {error, badarg}
	    end;
        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            NOpts = lists:keydelete(active, 1, R#listen_opts.opts),
            list_opt(Opts, R#listen_opts { opts = [{active,N}|NOpts] }, As);
	{Name,Val} when is_atom(Name) -> list_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
list_opt([], #listen_opts{} = R, _SockOpts) ->
    {ok, R}.

list_add(Name, Val, #listen_opts{} = R, Opts, As) ->
    case add_opt(Name, Val, R#listen_opts.opts, As) of
	{ok, SOpts} ->
	    list_opt(Opts, R#listen_opts { opts = SOpts }, As);
	Error -> Error
    end.

-doc false.
tcp_module(Opts) ->
    tcp_module_1(Opts, undefined).

-doc false.
tcp_module(Opts, Addr) ->
    Address = {undefined,Addr},
    %% Address has to be a 2-tuple but the first element is ignored
    tcp_module_1(Opts, Address).

tcp_module_1(Opts, Address) ->
    mod(
      Opts, tcp_module, Address,
      #{inet => inet_tcp, inet6 => inet6_tcp, local => local_tcp}).

-doc false.
gen_tcp_module([{inet_backend, Flag}|Opts]) ->
    gen_tcp_module(Opts, Flag);
gen_tcp_module(Opts) ->
    gen_tcp_module(
      Opts,
      persistent_term:get(
        {kernel, inet_backend}, ?DEFAULT_KERNEL_INET_BACKEND)).
%%
gen_tcp_module(Opts, inet) ->
    {gen_tcp, Opts};
gen_tcp_module(Opts, socket) ->
    {gen_tcp_socket, Opts}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for udp:open
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
udp_options() ->
    [
     debug,
     tos, tclass, priority,
     reuseaddr, reuseport, reuseport_lb, exclusiveaddruse,
     sndbuf, recbuf, header, active, buffer, mode,
     recvtos, recvtclass, ttl, recvttl, deliver, ipv6_v6only,
     broadcast, dontroute, multicast_if, multicast_ttl, multicast_loop,
     add_membership, drop_membership, read_packets, raw,
     high_msgq_watermark, low_msgq_watermark, bind_to_device
    ].


-doc false.
udp_options(Opts, Mod) ->
    case udp_opt(Opts, #udp_opts { }, udp_options()) of
	{ok, R} ->
	    {ok, R#udp_opts {
		   opts = lists:reverse(R#udp_opts.opts),
		   ifaddr = Mod:translate_ip(R#udp_opts.ifaddr)
		  }};
	Error -> Error
    end.

udp_opt([{raw,A,B,C}|Opts], #udp_opts{} = R, As) ->
    udp_opt([{raw,{A,B,C}}|Opts], R, As);
udp_opt([Opt | Opts], #udp_opts{ifaddr = IfAddr} = R, As) ->
    case Opt of
	{ifaddr, Addr} when is_map(Addr) ->
            udp_opt(Opts, R#udp_opts { ifaddr = ensure_sockaddr(Addr) }, As);
	{ifaddr, Addr} ->
            udp_opt(Opts, R#udp_opts { ifaddr = Addr }, As);

	{ip, IP} when is_map(IfAddr) ->
            udp_opt(Opts, R#udp_opts { ifaddr = IfAddr#{addr => IP} }, As);
	{ip, IP}                     ->
            udp_opt(Opts, R#udp_opts { ifaddr = IP }, As);

	{port, P} when is_map(IfAddr) ->
            udp_opt(Opts, R#udp_opts { ifaddr = IfAddr#{port => P} }, As);
	{port, P}                     ->
            udp_opt(Opts, R#udp_opts { port = P }, As);

	{fd,Fd}     ->  udp_opt(Opts, R#udp_opts { fd = Fd }, As);
	binary      ->  udp_add(mode, binary, R, Opts, As);
	list        ->  udp_add(mode, list, R, Opts, As);
	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    udp_opt(Opts, R#udp_opts { fd = [{netns,BinNS}] }, As);
		false ->
		    {error, badarg}
	    end;
        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            POpts = lists:keydelete(active, 1, R#udp_opts.opts),
            udp_opt(Opts, R#udp_opts { opts = [{active,N}|POpts] }, As);

        {Membership, {MAddr, If}}
          when ((Membership =:= add_membership) orelse
                (Membership =:= drop_membership)) andalso
               (tuple_size(MAddr) =:= 4) andalso
               ((If =:= any) orelse (tuple_size(If) =:= 4)) ->
            MembershipOpt = {Membership, {MAddr, If, 0}},
            POpts         = R#udp_opts.opts,
            udp_opt(Opts, R#udp_opts{opts = [MembershipOpt|POpts]}, As);

	{Name,Val} when is_atom(Name) -> udp_add(Name, Val, R, Opts, As);

	_ -> {error, badarg}
    end;
udp_opt([], #udp_opts{} = R, _SockOpts) ->
    {ok, R}.

udp_add(Name, Val, #udp_opts{} = R, Opts, As) ->
    case add_opt(Name, Val, R#udp_opts.opts, As) of
	{ok, SOpts} ->
	    udp_opt(Opts, R#udp_opts { opts = SOpts }, As);
	Error -> Error
    end.

-doc false.
udp_module(Opts) ->
    udp_module_1(Opts, undefined).

-doc false.
udp_module(Opts, Addr) ->
    Address = {undefined, Addr},
    %% Address has to be a 2-tuple but the first element is ignored
    udp_module_1(Opts, Address).

udp_module_1(Opts, Address) ->
    mod(
      Opts, udp_module, Address,
      #{inet => inet_udp, inet6 => inet6_udp, local => local_udp}).

-doc false.
gen_udp_module([{inet_backend, Flag}|Opts]) ->
    gen_udp_module(Opts, Flag);
gen_udp_module(Opts) ->
    gen_udp_module(
      Opts,
      persistent_term:get(
        {kernel, inet_backend}, ?DEFAULT_KERNEL_INET_BACKEND)).
%%
gen_udp_module(Opts, inet) ->
    {gen_udp, Opts};
gen_udp_module(Opts, socket) ->
    {gen_udp_socket, Opts}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for sctp:open
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Currently supported options include:
%  (*) {mode,   list|binary}	 or just list|binary
%  (*) {active, true|false|once|N}
%  (*) {sctp_module, inet_sctp|inet6_sctp} or just inet|inet6
%  (*) options set via setsockopt.
%      The full list is below in sctp_options/0 .
%  All other options are currently NOT supported. In particular:
%  (*) multicast on SCTP is not (yet) supported, as it may be incompatible
%      with automatic associations;
%  (*) passing of open FDs ("fdopen") is not supported.
sctp_options() ->
[   % The following are generic inet options supported for SCTP sockets:
    debug,
    mode, active, buffer, tos, tclass, ttl,
    priority, dontroute,
    reuseaddr, reuseport, reuseport_lb, exclusiveaddruse,
    linger, recvtos, recvtclass, recvttl,
    sndbuf, recbuf, ipv6_v6only, high_msgq_watermark, low_msgq_watermark,
    bind_to_device,

    % Other options are SCTP-specific (though they may be similar to their
    % TCP and UDP counter-parts):
    non_block_send,
    sctp_rtoinfo,   		 sctp_associnfo,	sctp_initmsg,
    sctp_autoclose,		 sctp_nodelay,		sctp_disable_fragments,
    sctp_i_want_mapped_v4_addr,  sctp_maxseg,		sctp_primary_addr,
    sctp_set_peer_primary_addr,  sctp_adaptation_layer,	sctp_peer_addr_params,
    sctp_default_send_param,	 sctp_events,		sctp_delayed_ack_time,
    sctp_status,	   	 sctp_get_peer_addr_info
].

-doc false.
sctp_options(Opts, Mod)  ->
    %% ?DBG([{opts, Opts}, {mod, Mod}]),
    case sctp_opt(Opts, Mod, #sctp_opts{}, sctp_options()) of
	{ok, SO} ->
	    {ok,SO#sctp_opts{opts=lists:reverse(SO#sctp_opts.opts)}};
	Error ->
            %% ?DBG([{error, Error}]),
            Error
    end.

sctp_opt([Opt|Opts], Mod, #sctp_opts{ifaddr = IfAddr} = R, As) ->
    %% ?DBG([{opt, Opt}]),
    case Opt of
        %% what if IfAddr is already a map (=sockaddr)?
        %% Shall we allow ifaddr as a list of sockaddr?
	{ifaddr, Addr} when is_map(Addr) ->
            sctp_opt(Opts, Mod, R#sctp_opts{ifaddr = ensure_sockaddr(Addr)}, As);
	{ifaddr, IP} ->
	    sctp_opt_ifaddr(Opts, Mod, R, As, IP);

	{ip, IP} when is_map(IfAddr) ->
            IP2 = Mod:translate_ip(IP),
            sctp_opt(Opts, Mod, R#sctp_opts{ifaddr = IfAddr#{addr => IP2}}, As);
	{ip, IP} ->
	    sctp_opt_ifaddr(Opts, Mod, R, As, IP);

	{port, Port} ->
	    case Mod:getserv(Port) of
		{ok, P} when is_map(IfAddr) ->
		    sctp_opt(Opts,
                             Mod,
                             R#sctp_opts{ifaddr = IfAddr#{port => P}}, As);
		{ok, P} ->
		    sctp_opt(Opts, Mod, R#sctp_opts{port = P}, As);
		Error ->
                    Error
	    end;

	{type, Type} when Type =:= seqpacket; Type =:= stream ->
            sctp_opt(Opts, Mod, R#sctp_opts{type = Type}, As);

	binary		-> sctp_opt (Opts, Mod, R, As, mode, binary);
	list		-> sctp_opt (Opts, Mod, R, As, mode, list);

	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    sctp_opt(
		      Opts, Mod,
		      R#sctp_opts { fd = [{netns,BinNS}] },
		      As);
		false ->
		    {error, badarg}
	    end;

        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            NOpts = lists:keydelete(active, 1, R#sctp_opts.opts),
            sctp_opt(Opts, Mod, R#sctp_opts { opts = [{active,N}|NOpts] }, As);

	{Name,Val}	->
            %% ?DBG([{name, Name}, {val, Val}]),
            sctp_opt(Opts, Mod, R, As, Name, Val);

	_ ->
            {error, badarg}
    end;
sctp_opt([], _Mod, #sctp_opts{ifaddr=IfAddr}=R, _SockOpts) ->
    if is_list(IfAddr) ->
	    {ok, R#sctp_opts{ifaddr=lists:reverse(IfAddr)}};
       true ->
	    {ok, R}
    end.

sctp_opt(Opts, Mod, #sctp_opts{} = R, As, Name, Val) ->
    %% ?DBG([{opts, Opts}, {mod, Mod}, {name, Name}, {val, Val}]),
    case add_opt(Name, Val, R#sctp_opts.opts, As) of
	{ok,SocketOpts} ->
	    sctp_opt(Opts, Mod, R#sctp_opts{opts=SocketOpts}, As);
	Error -> Error
    end.

sctp_opt_ifaddr(Opts, Mod, #sctp_opts{ifaddr=IfAddr}=R, As, Addr) ->
    IP = Mod:translate_ip(Addr),
    sctp_opt(Opts, Mod,
	     R#sctp_opts{
	       ifaddr=case IfAddr of
			  undefined              -> IP;
			  _ when is_list(IfAddr) -> [IP|IfAddr];
			  _                      -> [IP,IfAddr]
		      end}, As).

-doc false.
sctp_module(Opts) ->
    mod(
      Opts, sctp_module, undefined,
      #{inet => inet_sctp, inet6 => inet6_sctp}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Util to check and insert option in option list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_opt(Name, Val, Opts, As) ->
    %% ?DBG([{name, Name}, {val, Val}, {opts, Opts}, {as, As}]),
    case lists:member(Name, As) of
	true ->
            %% ?DBG(['is sockopt_val']),
	    case prim_inet:is_sockopt_val(Name, Val) of
		true when Name =:= raw ->
		    {ok, [{Name,Val} | Opts]};
		true ->
		    Opts1 = lists:keydelete(Name, 1, Opts),
		    {ok, [{Name,Val} | Opts1]};
		false ->
                    %% ?DBG(['false']),
                    {error,badarg}
	    end;
	false ->
            {error,badarg}
    end.


%% Passthrough all unknown - catch type errors later
filename2binary(List) when is_list(List) ->
    OutEncoding = file:native_name_encoding(),
    try unicode:characters_to_binary(List, unicode, OutEncoding) of
	Bin when is_binary(Bin) ->
	    Bin;
	_ ->
	    List
    catch
	error:badarg ->
	    List
    end;
filename2binary(Bin) ->
    Bin.

binary2filename(Bin) ->
    InEncoding = file:native_name_encoding(),
    case unicode:characters_to_list(Bin, InEncoding) of
	Filename when is_list(Filename) ->
	    Filename;
	_ ->
	    %% For getopt/setopt of netns this should only happen if
	    %% a binary with wrong encoding was used when setting the
	    %% option, hence the user shall eat his/her own medicine.
	    %%
	    %% I.e passthrough here too for now.
	    %% Future usecases will most probably not want this,
	    %% rather Unicode error or warning
	    %% depending on emulator flag instead.
	    Bin
    end.

%% Protocol independent, i.e common code for all
%% inet_* and inet6_* modules
%%
-doc false.
translate_ip(any,      inet)  -> {0,0,0,0};
translate_ip(loopback, inet)  -> {127,0,0,1};
translate_ip(any,      inet6) -> {0,0,0,0,0,0,0,0};
translate_ip(loopback, inet6) -> {0,0,0,0,0,0,0,1};
translate_ip(IP, _)           -> IP.  % undefined goes here

mod(Opts, Tag, Address, Map) ->
    mod(Opts, Tag, Address, Map, undefined, []).
%%
mod([{Tag, M}|Opts], Tag, Address, Map, Mod, Acc) ->
    mod(Opts, Tag, Address, Map, Mod, Acc, M);
mod([{T, _} = Opt|Opts], Tag, _Address, Map, Mod, Acc)
  when T =:= ip; T =:= ifaddr->
    mod(Opts, Tag, Opt, Map, Mod, [Opt|Acc]);
mod([Family|Opts], Tag, Address, Map, Mod, Acc) when is_atom(Family) ->
    case Map of
	#{Family := M} ->
	    mod(Opts, Tag, Address, Map, Mod, Acc, M);
	#{} ->
	    mod(Opts, Tag, Address, Map, Mod, [Family|Acc])
    end;
mod([Opt|Opts], Tag, Address, Map, Mod, Acc) ->
    mod(Opts, Tag, Address, Map, Mod, [Opt|Acc]);
mod([], Tag, Address, Map, undefined, Acc) ->
    {case Address of
	 {_, {local, _}} ->
	     case Map of
		 #{local := Mod} ->
		     Mod;
		 #{} ->
		     inet_db:Tag()
	     end;
	 {_, IP} when tuple_size(IP) =:= 8 ->
	     #{inet := IPv4Mod} = Map,
	     %% Get the mod, but IPv6 address overrides default IPv4
	     case inet_db:Tag() of
		 IPv4Mod ->
		     #{inet6 := IPv6Mod} = Map,
		     IPv6Mod;
		 Mod ->
		     Mod
	     end;
	 {_, #{family := inet6}} ->
	     #{inet := IPv4Mod} = Map,
	     %% Get the mod, but IPv6 address overrides default IPv4
	     case inet_db:Tag() of
		 IPv4Mod ->
		     #{inet6 := IPv6Mod} = Map,
		     IPv6Mod;
		 Mod ->
		     Mod
	     end;
	 _ ->
	     inet_db:Tag()
     end, lists:reverse(Acc)};
mod([], _Tag, _Address, _Map, Mod, Acc) ->
    {Mod, lists:reverse(Acc)}.
%%
mod(Opts, Tag, Address, Map, undefined, Acc, M) ->
    mod(Opts, Tag, Address, Map, M, Acc);
mod(Opts, Tag, Address, Map, Mod, Acc, _M) ->
    mod(Opts, Tag, Address, Map, Mod, Acc).

-doc false.
getaddrs_tm({A,B,C,D} = IP, Fam, _)  ->
    %% Only "syntactic" validation and check of family.
    if
	?ip(A,B,C,D) ->
	    if
		Fam =:= inet -> {ok,[IP]};
		true -> {error,eafnosupport}
	    end;
	true -> {error,einval}
    end;
getaddrs_tm({A,B,C,D,E,F,G,H} = IP, Fam, _) ->
    %% Only "syntactic" validation; we assume that the address was
    %% "semantically" validated when it was converted to a tuple.
    if
	?ip6(A,B,C,D,E,F,G,H) ->
	    if
		Fam =:= inet6 -> {ok,[IP]};
		true -> {error,eafnosupport}
	    end;
	true -> {error,einval}
    end;
getaddrs_tm(Address, Family, Timer) when is_atom(Address) ->
    getaddrs_tm(atom_to_list(Address), Family, Timer);
getaddrs_tm(Address, Family, Timer) ->
    %% ?DBG([{address, Address}, {family, Family}, {timer, Timer}]),
    case inet_parse:visible_string(Address) of
	false ->
	    {error,einval};
	true ->
	    %% Address is a host name or a valid IP address,
	    %% either way check it with the resolver.
	    case gethostbyname_tm(Address, Family, Timer) of
		{ok, Ent} ->
		    %% ?DBG([{ent, Ent}]),
		    {ok, Ent#hostent.h_addr_list};
		Error ->
		    %% ?DBG([{error, Error}]),
		    Error
	    end
    end.

%%
%% gethostbyname with option search
%%
gethostbyname_tm(Name, Type, Timer, [string|_]=Opts) ->
    %% ?DBG([string, {name, Name}, {type, Type}, {timer, Timer}]),
    Result = gethostbyname_string(Name, Type),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [dns|_]=Opts) ->
    %% ?DBG([dns, {name, Name}, {type, Type}, {timer, Timer}]),
    Result = inet_res:gethostbyname_tm(Name, Type, Timer),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [file|_]=Opts) ->
    %% ?DBG([file, {name, Name}, {type, Type}, {timer, Timer}]),
    Result = inet_hosts:gethostbyname(Name, Type),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [yp|_]=Opts) ->
    %% ?DBG([yp, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [nis|_]=Opts) ->
    %% ?DBG([nis, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [nisplus|_]=Opts) ->
    %% ?DBG([niplus, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [wins|_]=Opts) ->
    %% ?DBG([wins, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [native|_]=Opts) ->
    %% ?DBG([native, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [_|Opts]) ->
    %% ?DBG([{name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm(Name, Type, Timer, Opts);
%% Make sure we always can look up our own hostname.
gethostbyname_tm(Name, Type, Timer, []) ->
    %% ?DBG([{name, Name}, {type, Type}, {timer, Timer}]),
    Result = gethostbyname_self(Name, Type),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, [], Result).

gethostbyname_tm(Name, Type, Timer, Opts, Result) ->
    %% ?DBG([string, {name, Name}, {type, Type}, {timer, Timer},
    %% 	  {opts, Opts}, {result, Result}]),
    case Result of
	{ok,_} ->
	    Result;
	{error,formerr} ->
	    {error,einval};
	{error,_} when Opts =:= [] ->
	    {error,nxdomain};
	{error,_} ->
	    gethostbyname_tm(Name, Type, Timer, tl(Opts))
    end.

gethostbyname_tm_native(Name, Type, Timer, Opts) ->
    %% ?DBG([{name, Name}, {type, Type}, {timer, Timer}, {opts, Opts}]),
    %% Fixme: add (global) timeout to gethost_native
    Result = inet_gethost_native:gethostbyname(Name, Type),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, Opts, Result).



-doc false.
gethostbyname_self(Name, Type) when is_atom(Name) ->
    %% ?DBG([{name, Name}, {type, Type}]),
    gethostbyname_self(atom_to_list(Name), Type);
gethostbyname_self(Name, Type)
  when is_list(Name), Type =:= inet;
       is_list(Name), Type =:= inet6 ->
    %% ?DBG([{name, Name}, {type, Type}]),
    N    = inet_db:tolower(Name),
    Self = inet_db:gethostname(),
    %% ?DBG([{n, N}, {self, Self}]),
    %%
    %% This is the final fallback that pretends /etc/hosts has got
    %% a line for the hostname on the loopback address.
    %% Lookups into /etc/hosts are case insensitive and return
    %% what is in the file. Therefore the letter case may differ between
    %% the returned #hostent{} record and the hostname that was asked for.
    %%
    case inet_db:tolower(Self) of
	N ->
	    %% ?DBG([{n, N}]),
	    {ok,
	     make_hostent(
	       Self, [translate_ip(loopback, Type)], [], Type)};
	_ ->
	    case inet_db:res_option(domain) of
		"" ->
		    %% ?DBG(['res option empty domain']),
		    {error,nxdomain};
		Domain ->
		    %% ?DBG([{domain, Domain}]),
		    FQDN = lists:append([Self,".",Domain]),
		    case inet_db:tolower(FQDN) of
			N ->
			    {ok,
			     make_hostent(
			       FQDN,
			       [translate_ip(loopback, Type)], [], Type)};
			_ ->
			    %% ?DBG(['invalid domain', {fqdn, FQDN}]),
			    {error,nxdomain}
		    end
	    end
    end;
gethostbyname_self(_, _) ->
    {error,formerr}.

-doc false.
gethostbyname_string(Name, Type) when is_atom(Name) ->
    gethostbyname_string(atom_to_list(Name), Type);
gethostbyname_string(Name, Type)
  when is_list(Name), Type =:= inet;
       is_list(Name), Type =:= inet6 ->
    case
	case Type of
	    inet ->
		inet_parse:ipv4_address(Name);
	    inet6 ->
		inet_parse:ipv6strict_address(Name)
	end of
	{ok,IP} ->
	    {ok,make_hostent(Name, [IP], [], Type)};
	{error,einval} ->
	    {error,nxdomain}
    end;
gethostbyname_string(_, _) ->
    {error,formerr}.

make_hostent(Name, Addrs, Aliases, Type) ->
    #hostent{h_name = Name,
	     h_aliases = Aliases,
	     h_addrtype = Type,
	     h_length = case Type of inet -> 4; inet6 -> 16 end,
	     h_addr_list = Addrs}.

%%
%% gethostbyaddr with option search
%%
gethostbyaddr_tm(Addr, Timer, [dns | Opts]) ->
    Res = inet_res:gethostbyaddr_tm(Addr,Timer),
    case Res of
	{ok,_} -> Res;
	{error,timeout} -> Res;
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts)
    end;
gethostbyaddr_tm(Addr, Timer, [file | Opts]) ->
    case inet_hosts:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts);
	Result -> Result
    end;
gethostbyaddr_tm(Addr, Timer, [yp | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [nis | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer,  [nisplus | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [wins | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [native | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [_ | Opts]) ->
    gethostbyaddr_tm(Addr, Timer, Opts);
gethostbyaddr_tm({127,0,0,1}=IP, _Timer, []) ->
    gethostbyaddr_self(IP, inet);
gethostbyaddr_tm({0,0,0,0,0,0,0,1}=IP, _Timer, []) ->
    gethostbyaddr_self(IP, inet6);
gethostbyaddr_tm(_Addr, _Timer, []) ->
    {error, nxdomain}.

gethostbyaddr_self(IP, Type) ->
    Name = inet_db:gethostname(),
    case inet_db:res_option(domain) of
	"" ->
	    {ok,make_hostent(Name, [IP], [], Type)};
	Domain ->
	    {ok,make_hostent(Name++"."++Domain, [IP], [Name], Type)}
    end.

gethostbyaddr_tm_native(Addr, Timer, Opts) ->
    %% Fixme: user timer for timeoutvalue
    case inet_gethost_native:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts);
	Result -> Result
    end.


-doc false.
-spec open(Fd_or_OpenOpts :: integer() | list(),
	   BAddr ::
             socket:sockaddr_in6() |
	     socket_address() |
	     {ip_address() | 'any' | 'loopback', % Unofficial
	      port_number()} |
	     {inet, % Unofficial
	      {ip4_address() | 'any' | 'loopback',
	       port_number()}} |
	     {inet6, % Unofficial
	      {ip6_address() | 'any' | 'loopback',
	       port_number()}} |
	     undefined, % Internal - no bind()
	   BPort :: port_number(),
	   Opts :: [socket_setopt()],
	   Protocol :: socket_protocol(),
	   Family :: address_family(),
	   Type :: socket_type(),
	   Module :: atom()) ->
	{'ok', port()} | {'error', posix()}.

open(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module)
  when is_integer(Fd), 0 =< Fd ->
    open_fd(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module);
open(Fd_or_OpenOpts, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    open_opts(
      Fd_or_OpenOpts,
      if
          BAddr =:= undefined, BPort =/= 0 ->
              translate_ip(any, Family);
          true ->
              BAddr
      end, BPort, Opts, Protocol, Family, Type, Module).

%% The only difference between open/8 and open_bind/8 is that
%% if Fd_or_OpenOpts is not a FileDescriptor :: non_neg_integer()
%% i.e option {fd,Fd} has not been used hence we are not handling
%% an already open socket handle, and also if no bind address
%% has been specified (BAddr =:= undefined).
%%
%% Then open_bind/8 will bind to the wildcard address and the
%% specified port (BPort, 0 = wildcard port per default),
%% which is the legacy behaviour by this module when opening a socket.
%%
%% In the same situation, open/8 will bind to the wildcard address
%% and the specified port, only if BPort is not 0, i.e a bind port
%% has been specified to not be the wildcard port.
%%
%% So open/8 per default does not bind to an address, which
%% is used by TCP connect to let the OS automatically bind to
%% an address later, during TCP connect operation.  This
%% gives the OS more freedom in choosing the originating port and
%% therefore makes far more effective use of the port range.

-doc false.
-spec open_bind(Fd_or_OpenOpts :: integer() | list(),
                BAddr ::
                  socket:sockaddr_in6() |
                  socket_address() |
                  {ip_address() | 'any' | 'loopback', % Unofficial
                   port_number()} |
                  {inet, % Unofficial
                   {ip4_address() | 'any' | 'loopback',
                    port_number()}} |
                  {inet6, % Unofficial
                   {ip6_address() | 'any' | 'loopback',
                    port_number()}} |
                  undefined, % Internal - translated to 'any'
                BPort :: port_number(),
                Opts :: [socket_setopt()],
                Protocol :: socket_protocol(),
                Family :: address_family(),
                Type :: socket_type(),
                Module :: atom()) ->
                       {'ok', port()} | {'error', posix()}.

open_bind(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module)
  when is_integer(Fd), 0 =< Fd ->
    %% ?DBG([{fd, Fd},
    %%       {baddr, BAddr}, {bport, BPort},
    %%       {opts, Opts}, {proto, Protocol}, {fam, Family},
    %%       {type, Type}, {mod, Module}]),
    open_fd(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module);
open_bind(
  Fd_or_OpenOpts, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    %% ?DBG([{fd_or_openopts, Fd_or_OpenOpts},
    %%       {baddr, BAddr}, {bport, BPort},
    %%       {opts, Opts}, {proto, Protocol}, {fam, Family},
    %%       {type, Type}, {mod, Module}]),
    open_opts(
      Fd_or_OpenOpts,
      if
          BAddr =:= undefined ->
              translate_ip(any, Family);
          true ->
              BAddr
      end, BPort, Opts, Protocol, Family, Type, Module).


open_fd(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    DoNotBind =
	%% We do not do any binding if no port+addr options
	%% were given, in order to keep backwards compatibility
	%% with pre Erlang/OTP 17
        BAddr =:= undefined, % Presumably already bound
    if
        DoNotBind ->
            0 = BPort, ok; % Assertion
        true ->
            ok
    end,
    case prim_inet:fdopen(Protocol, Family, Type, Fd, DoNotBind) of
	{ok, S} ->
            open_setopts(S, BAddr, BPort, Opts, Module);
        Error ->
            Error
    end.

open_opts(Fd_or_OpenOpts, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    %% ?DBG([{fd_or_openopts, Fd_or_OpenOpts},
    %%       {baddr, BAddr}, {bport, BPort},
    %%       {opts, Opts}, {proto, Protocol}, {fam, Family},
    %%       {type, Type}, {mod, Module}]),
    OpenOpts =
	if
            is_list(Fd_or_OpenOpts) -> Fd_or_OpenOpts;
	    true -> []
	end,
    case prim_inet:open(Protocol, Family, Type, OpenOpts) of
	{ok,S} ->
            %% ?DBG(['prim_inet:open', {s, S}]),
            open_setopts(S, BAddr, BPort, Opts, Module);
        Error ->
            Error
    end.

%% If BAddr is undefined - do not bind to an address
%%
open_setopts(S, BAddr, BPort, Opts, Module) ->
    %% ?DBG([{s, S}, {baddr, BAddr}, {bport, BPort}, {opts, Opts}, {mod, Module}]),
    case prim_inet:setopts(S, Opts) of
        ok when BAddr =:= undefined ->
            %% ?DBG("ok -> register socket"),
            inet_db:register_socket(S, Module),
            {ok,S};
        ok ->
            %% ?DBG("ok -> try bind"),
            try bind(S, BAddr, BPort) of
                {ok, _} ->
                    %% ?DBG("bound"),
                    inet_db:register_socket(S, Module),
                    {ok,S};
                Error  ->
                    %% ?DBG(["bind error", {error, Error}]),
                    prim_inet:close(S),
                    Error
            catch
                BC:BE:BS ->
                    %% ?DBG(["bind failed", {class, BC}, {error, BE}, {stack, BS}]),
                    prim_inet:close(S),
                    erlang:raise(BC, BE, BS)
            end;
        Error  ->
            %% ?DBG(["error", {error, Error}]),
            prim_inet:close(S),
            Error
    end.



bind(S, Addr, Port) when is_list(Addr) ->
    bindx(S, Addr, Port);
bind(S, Addr, Port) ->
    %% ?DBG([{s, S}, {addr, Addr}, {port, Port}]),
    prim_inet:bind(S, Addr, Port).

bindx(S, [Addr], Port0) ->
    {IP, Port} = set_bindx_port(Addr, Port0),
    prim_inet:bind(S, IP, Port);
bindx(S, Addrs, Port0) ->
    [{IP, Port} | Rest] = [set_bindx_port(Addr, Port0) || Addr <- Addrs],
    case prim_inet:bind(S, IP, Port) of
	{ok, AssignedPort} when Port =:= 0 ->
	    %% On newer Linux kernels, Solaris and FreeBSD, calling
	    %% bindx with port 0 is ok, but on SuSE 10, it results in einval
	    Rest2 = [change_bindx_0_port(Addr, AssignedPort) || Addr <- Rest],
	    prim_inet:bind(S, add, Rest2);
	{ok, _} ->
	    prim_inet:bind(S, add, Rest);
	Error ->
	    Error
    end.

set_bindx_port({_IP, _Port}=Addr, _OtherPort) ->
    Addr;
set_bindx_port(IP, Port) ->
    {IP, Port}.

change_bindx_0_port({IP, 0}, AssignedPort) ->
    {IP, AssignedPort};
change_bindx_0_port({_IP, _Port}=Addr, _AssignedPort) ->
    Addr.


-doc false.
-spec fdopen(Fd :: non_neg_integer(),
	     Opts :: [socket_setopt()],
	     Protocol :: socket_protocol(),
	     Family :: address_family(),
	     Type :: socket_type(),
	     Module :: atom()) ->
	{'ok', socket()} | {'error', posix()}.

fdopen(Fd, Opts, Protocol, Family, Type, Module)
  when is_integer(Fd), 0 =< Fd ->
    open_fd(Fd, undefined, 0, Opts, Protocol, Family, Type, Module).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  socket stat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Options for selecting statistics items.

Regarding `ShowPorts`, see `show_ports` as described in the `i/2` function,
defaults to `false`.
""".
-type i_option() :: port |
                    module |
                    recv |
                    sent |
                    owner |
                    local_address |
                    {local_address, ShowPorts :: boolean()} |
                    foreign_address |
                    {foreign_address, ShowPorts :: boolean()} |
                    state |
                    type.

i_options_all() ->
    i_options_all(false).

i_options_all(ShowPorts) when is_boolean(ShowPorts) ->
    [port, module, recv, sent, owner,
     {local_address, ShowPorts}, {foreign_address, ShowPorts},
     state, type].

-doc "Equivalent to `i/1` for the protocols `tcp`, `udp`, and `sctp`".
-doc(#{since => <<"OTP 21.0">>}).
-spec i() -> ok.
i() -> i(tcp), i(udp), i(sctp).

-doc(#{equiv => i/2}).
-doc """
List network sockets.

With argument `Proto` equivalent to [`i(Proto, Options)`](`i/2`)
where `Options` is a list of all `t:atom/0`s in `t:i_option/0`.

With argument `Options`, equivalent to [`i(Proto, Options)](`i/2`)
for `Proto`: `tcp`, `udp`, and `sctp`.

With argument `show_ports` **(since OTP 27.0)** equivalent to
[`i(Proto, Options)](`i/2`) where `Option` is a list of all
options in `t:i_option/0` with `ShowPorts = true`.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec i(show_ports | socket_protocol() | [i_option()]) -> ok.
i(show_ports) ->
    i(i_options_all(true));
i(Proto) when is_atom(Proto) ->
    i(Proto, i_options_all());
i(Options) when is_list(Options) ->
    i(tcp,  Options),
    i(udp,  Options),
    i(sctp, Options).

-doc """
List network sockets.

Lists all TCP, UDP and SCTP sockets on the terminal, those created by
the Erlang runtime system as well as by the application.

The following options are available:

- **`port`** - An internal index of the port.

- **`module`** - The callback module of the socket.

- **`recv`** - Number of bytes received by the socket.

- **`sent`** - Number of bytes sent from the socket.

- **`owner`** - The socket owner process.

- **`local_address`** - The local address of the socket.

- **`foreign_address`** - The address and port of the other end of the
  connection.

- **`state`** - The connection state.

- **`type`** - STREAM or DGRAM or SEQPACKET.

The `Options` argument may also be **(since OTP 27.0)**:

- **`show_ports`** - Do *not* translate the port numbers
  (of 'local_address' and 'foreign_address') to service name(s).
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec i(socket_protocol(), show_ports | Options :: [i_option()]) -> ok.
i(Proto, show_ports) ->
    i(Proto, i_options_all(true));
i(tcp, Fs) ->
    ii(tcp_sockets(), Fs, tcp);
i(udp, Fs) ->
    ii(udp_sockets(), Fs, udp);
i(sctp, Fs) ->
    ii(sctp_sockets(), Fs, sctp).

ii(Ss, Fs, Proto) ->
    LLs =
	case info_lines(Ss, Fs, Proto) of
	    [] -> [];
	    InfoLines -> [h_line(Fs) | InfoLines]
	end,
    Maxs = foldl(
	     fun(Line,Max0) -> smax(Max0,Line) end,
	     duplicate(length(Fs),0),LLs),
    Fmt = append(["~-" ++ integer_to_list(N) ++ "s " || N <- Maxs]) ++ "\n",
    lists:foreach(fun(Line) -> io:format(Fmt, Line) end, LLs).

smax([Max|Ms], [Str|Strs]) ->
    N = length(Str),
    [if N > Max -> N; true -> Max end | smax(Ms, Strs)];
smax([], []) -> [].

info_lines(Ss, Fs, Proto) -> [i_line(S, Fs,Proto) || S <- Ss].
i_line(S, Fs, Proto)      -> [info(S, F, Proto) || F <- Fs].

h_line(Fs) -> [h_field(field2string(F)) || F <- Fs].

field2string({F, _}) when is_atom(F) ->
    field2string(F);
field2string(F) when is_atom(F) ->
    atom_to_list(F).

h_field([C|Cs]) -> [upper(C) | hh_field(Cs)].

hh_field([$_,C|Cs]) -> [$\s,upper(C) | hh_field(Cs)];
hh_field([C|Cs]) -> [C|hh_field(Cs)];
hh_field([]) -> [].

upper(C) when C >= $a, C =< $z -> (C-$a) + $A;
upper(C) -> C.


info({'$inet', GenSocketMod, _} = S, F, Proto) when is_atom(GenSocketMod) ->
    case F of
	owner ->
	    case GenSocketMod:info(S) of
		#{owner := Owner} when is_pid(Owner) -> pid_to_list(Owner);
		_ -> " "
	    end;
	port ->
	    case GenSocketMod:getopts(S, [fd]) of
		{ok, [{fd, FD}]} ->
		    "esock[" ++ integer_to_list(FD) ++ "]";
		_ ->
		    "esock"
	    end;
	sent ->
	    case GenSocketMod:getstat(S, [send_oct]) of
		{ok, [{send_oct, N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	recv ->
	    case GenSocketMod:getstat(S, [recv_oct]) of
		{ok, [{recv_oct, N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	local_address ->
	    fmt_addr(GenSocketMod:sockname(S), Proto, false);
	{local_address, ShowPort} ->
	    fmt_addr(GenSocketMod:sockname(S), Proto, ShowPort);
	foreign_address ->
	    fmt_addr(GenSocketMod:peername(S), Proto, false);
	{foreign_address, ShowPort} ->
	    fmt_addr(GenSocketMod:peername(S), Proto, ShowPort);
	state ->
	    case GenSocketMod:info(S) of
		#{rstates := RStates,
		  wstates := WStates} -> fmt_compat_status(RStates, WStates);
		_ -> " "
	    end;
	packet ->
	    case GenSocketMod:which_packet_type(S) of
		{ok, Type} -> atom_to_list(Type);
		_ -> " "
	    end;
	type ->
	    case GenSocketMod:info(S) of
		#{type := stream} -> "STREAM";
		_ -> " "
	    end;
	%% Why do we have this here? Its never called (see i/2 calling i/2).
	fd ->
	    case GenSocketMod:getopts(S, [fd]) of
		{ok, [{fd, Fd}]} -> integer_to_list(Fd);
		_ -> " "
	    end;
	module ->
	    atom_to_list(GenSocketMod)
    end;
info(S, F, Proto) ->
    case F of
	owner ->
	    case erlang:port_info(S, connected) of
		{connected, Owner} -> pid_to_list(Owner);
		_ -> " "
	    end;
	port ->
	    case erlang:port_info(S,id) of
		{id, Id}  -> integer_to_list(Id);
		undefined -> " "
	    end;
	sent ->
	    case prim_inet:getstat(S, [send_oct]) of
		{ok,[{send_oct,N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	recv ->
	    case  prim_inet:getstat(S, [recv_oct]) of
		{ok,[{recv_oct,N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	local_address ->
	    fmt_addr(prim_inet:sockname(S), Proto, false);
	{local_address, ShowPort} ->
	    fmt_addr(prim_inet:sockname(S), Proto, ShowPort);
	foreign_address ->
	    fmt_addr(prim_inet:peername(S), Proto, false);
	{foreign_address, ShowPort} ->
	    fmt_addr(prim_inet:peername(S), Proto, ShowPort);
	state ->
	    case prim_inet:getstatus(S) of
		{ok,Status} -> fmt_status(Status);
		_ -> " "
	    end;
	packet ->
	    case prim_inet:getopt(S, packet) of
		{ok,Type} when is_atom(Type) -> atom_to_list(Type);
		{ok,Type} when is_integer(Type) -> integer_to_list(Type);
		_ -> " "
	    end;
	type ->
	    case prim_inet:gettype(S) of
		{ok,{_,stream}} -> "STREAM";
		{ok,{_,dgram}}  -> "DGRAM";
		{ok,{_,seqpacket}} -> "SEQPACKET";
		_ -> " "
	    end;
	%% Why do we have this here? Its never called (see i/2 calling i/2).
	fd ->
	    case prim_inet:getfd(S) of
		{ok, Fd} -> integer_to_list(Fd);
		_ -> " "
	    end;
	module ->
	    case inet_db:lookup_socket(S) of
		{ok,Mod} -> atom_to_list(Mod);
		_ -> "prim_inet"
	    end
    end.

%% Possible flags: (sorted)
%% [accepting,bound,busy,connected,connecting,listen,listening,open]
%% Actually, we no longer gets listening...
fmt_status(Flags) ->
    case lists:sort(Flags) of
	[accepting | _]               -> "ACCEPTING";
	[bound,busy,connected|_]      -> "CONNECTED(BB)";
	[bound,connected|_]           -> "CONNECTED(B)";
	[bound,listen,listening | _]  -> "LISTENING";
	[bound,listen | _]            -> "LISTEN";
	[bound,connecting | _]        -> "CONNECTING";
	[bound,open]                  -> "BOUND";
	[connected,open]              -> "CONNECTED(O)";
	[open]                        -> "IDLE";
	[]                            -> "CLOSED";
	Sorted                        -> fmt_status2(Sorted)
    end.

fmt_compat_status(RFlags, WFlags) ->
    fmt_status(fmt_compat_status_merge(RFlags, WFlags)).

fmt_compat_status_merge(RFlags, WFlags) ->
    fmt_compat_status_merge(RFlags, WFlags, []).

fmt_compat_status_merge([], WFlags, Merged) ->
    Merged ++ WFlags;
fmt_compat_status_merge([RFlag|RFlags], WFlags, Merged) ->
    fmt_compat_status_merge(RFlags,
			    lists:delete(RFlag, WFlags),
			    [RFlag|Merged]).

fmt_status2([H]) ->
    fmt_status3(H);
fmt_status2([H|T]) ->
    fmt_status3(H) ++ ":"  ++ fmt_status2(T).

fmt_status3(accepting) ->
    "A";
fmt_status3(bound) ->
    "BD";
fmt_status3(busy) ->
    "BY";
fmt_status3(connected) ->
    "CD";
fmt_status3(connecting) ->
    "CG";
fmt_status3(listen) ->
    "LN";
fmt_status3(listening) ->
    "LG";
fmt_status3(open) ->
    "O";
fmt_status3(selected) ->
    "SD";
fmt_status3(X) when is_atom(X) ->
    string:uppercase(atom_to_list(X)).


fmt_addr({error,enotconn}, _, _) -> "*:*";
fmt_addr({error,_}, _, _)        -> " ";
fmt_addr({ok,Addr}, Proto, ShowPort) ->
    case Addr of
	%%Dialyzer {0,0}            -> "*:*";
	{{0,0,0,0}, Port} ->
            "*:" ++ fmt_port(Port, Proto, ShowPort);
	{{0,0,0,0,0,0,0,0}, Port} ->
            "*:" ++ fmt_port(Port, Proto, ShowPort);
	{{127,0,0,1}, Port} ->
            "localhost:" ++ fmt_port(Port, Proto, ShowPort);
	{{0,0,0,0,0,0,0,1}, Port} ->
            "localhost:" ++ fmt_port(Port, Proto, ShowPort);
	{local, Path} ->
            "local:" ++ binary_to_list(Path);
	{IP, Port} ->
            inet_parse:ntoa(IP) ++ ":" ++ fmt_port(Port, Proto, ShowPort)
    end.

fmt_port(N, _Proto, true = _ShowPort) ->
    integer_to_list(N);
fmt_port(N, Proto, false = _ShowPort) ->
    case inet:getservbyport(N, Proto) of
	{ok, Name} -> Name;
	_ -> integer_to_list(N)
    end.

%% Return a list of all tcp sockets
tcp_sockets()  -> port_list("tcp_inet") ++ gen_tcp_socket:which_sockets().
udp_sockets()  -> port_list("udp_inet") ++ gen_udp_socket:which_sockets().
sctp_sockets() -> port_list("sctp_inet").

%% Return all ports having the name 'Name'
port_list(Name) ->
    filter(
      fun(Port) ->
	      case erlang:port_info(Port, name) of
		  {name, Name} -> true;
		  _ -> false
	      end
      end, erlang:ports()).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Format an error code into a `t:string/0`.

Returns a diagnostic error string. For possible POSIX values
and corresponding strings, see section
[POSIX Error Codes](#posix-error-codes).
""".
-spec format_error(Reason) -> string() when
      Reason :: posix() | system_limit.

format_error(exbadport) -> "invalid port state";
format_error(exbadseq) ->  "bad command sequence";
format_error(system_limit) ->
    "a system limit was hit, probably not enough ports";
format_error(Tag) ->
    erl_posix_msg:message(Tag).

%% Close a TCP socket.
-doc false.
tcp_close(S) when is_port(S) ->
    %% if exit_on_close is set we must force a close even if remotely closed!!!
    prim_inet:close(S),
    receive {tcp_closed, S} -> ok after 0 -> ok end.

%% Close a UDP socket.
-doc false.
udp_close(S) when is_port(S) ->
    receive
	{udp_closed, S} -> ok
    after 0 ->
	    prim_inet:close(S),
	    receive {udp_closed, S} -> ok after 0 -> ok end
    end.

%% Set controlling process for TCP socket.
-doc false.
tcp_controlling_process(S, NewOwner) when is_port(S), is_pid(NewOwner) ->
    case erlang:port_info(S, connected) of
	{connected, NewOwner} ->
	    ok;
	{connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	undefined ->
	    {error, einval};
	_ ->
	    case prim_inet:getopt(S, active) of
		{ok, A0} ->
		    SetOptRes =
			case A0 of
			    false -> ok;
			    _ -> prim_inet:setopt(S, active, false)
			end,
		    case {tcp_sync_input(S, NewOwner, false), SetOptRes} of
			{true, _} ->  %% socket already closed
			    ok;
			{false, ok} ->
			    try erlang:port_connect(S, NewOwner) of
				true ->
				    unlink(S), %% unlink from port
				    case A0 of
					false -> ok;
					_ -> prim_inet:setopt(S, active, A0)
				    end
			    catch
				error:Reason ->
				    {error, Reason}
			    end;
			{false, Error} ->
			    Error
		    end;
		Error ->
		    Error
	    end
    end.

tcp_sync_input(S, Owner, Flag) ->
    receive
	{tcp, S, Data} ->
	    Owner ! {tcp, S, Data},
	    tcp_sync_input(S, Owner, Flag);
	{tcp_closed, S} ->
	    Owner ! {tcp_closed, S},
	    tcp_sync_input(S, Owner, true);
	{S, {data, Data}} ->
	    Owner ! {S, {data, Data}},
	    tcp_sync_input(S, Owner, Flag);
	{inet_async, S, Ref, Status} ->
	    Owner ! {inet_async, S, Ref, Status},
	    tcp_sync_input(S, Owner, Flag);
	{inet_reply, S, Status} ->
	    Owner ! {inet_reply, S, Status},
	    tcp_sync_input(S, Owner, Flag)
    after 0 ->
	    Flag
    end.

%% Set controlling process for UDP or SCTP socket.
-doc false.
udp_controlling_process(S, NewOwner) when is_port(S), is_pid(NewOwner) ->
    case erlang:port_info(S, connected) of
	{connected, NewOwner} ->
	    ok;
	{connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	_ ->
	    {ok, A0} = prim_inet:getopt(S, active),
	    ok = prim_inet:setopt(S, active, false),
	    udp_sync_input(S, NewOwner),
	    try erlang:port_connect(S, NewOwner) of
		true ->
		    unlink(S),
		    ok = prim_inet:setopt(S, active, A0)
	    catch
		error:Reason ->
		    {error, Reason}
	    end
    end.

udp_sync_input(S, Owner) ->
    receive
	{sctp, S, _, _, _}=Msg    -> udp_sync_input(S, Owner, Msg);
	{udp, S, _, _, _}=Msg     -> udp_sync_input(S, Owner, Msg);
	{udp_closed, S}=Msg       -> udp_sync_input(S, Owner, Msg);
	{S, {data,_}}=Msg         -> udp_sync_input(S, Owner, Msg);
	{inet_async, S, _, _}=Msg -> udp_sync_input(S, Owner, Msg);
	{inet_reply, S, _}=Msg    -> udp_sync_input(S, Owner, Msg)
    after 0 ->
	    ok
    end.

udp_sync_input(S, Owner, Msg) ->
    Owner ! Msg,
    udp_sync_input(S, Owner).

-doc false.
start_timer(infinity) -> false;
start_timer(Timeout) ->
    erlang:start_timer(Timeout, self(), inet).

-doc false.
timeout(false) -> infinity;
timeout(Timer) ->
    case erlang:read_timer(Timer) of
	false -> 0;
	Time  -> Time
    end.

-doc false.
timeout(Time, false) -> Time;
timeout(Time, Timer) ->
    TimerTime = timeout(Timer),
    if TimerTime < Time -> TimerTime;
       true -> Time
    end.

-doc false.
stop_timer(false) -> false;
stop_timer(Timer) ->
    case erlang:cancel_timer(Timer) of
	false ->
	    receive
		{timeout,Timer,_} -> false
	    after 0 ->
		    false
	    end;
	T -> T
    end.


-doc false.
lock_socket(S,Val) ->
    case erlang:port_info(S, connected) of
	{connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	undefined ->
	    {error, einval};
	_ ->
	    prim_inet:ignorefd(S,Val)
    end.


-doc false.
ensure_sockaddr(SockAddr) ->
    try prim_socket:enc_sockaddr(SockAddr)
    catch
        throw : {invalid, _} = Invalid : Stacktrace ->
            erlang:raise(error, Invalid, Stacktrace)
    end.


-doc false.
inet_backend() ->
    persistent_term:get({kernel, inet_backend}, ?DEFAULT_KERNEL_INET_BACKEND).
