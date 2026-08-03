<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# Overview

## Built-In Mechanisms

Three interoperability mechanisms are built into the Erlang runtime system,
_distributed Erlang_, _ports_, and _nifs_. A variation of ports is _linked-in drivers_.

### Distributed Erlang

An Erlang runtime system is made a distributed Erlang node by giving it a name.
A distributed Erlang node can connect to, and monitor, other nodes. It can also
spawn processes at other nodes. Message passing and error handling between
processes at different nodes are transparent. A number of useful STDLIB modules
are available in a distributed Erlang system. For example, `m:global`, which
provides global name registration. The distribution mechanism is implemented
using TCP/IP sockets.

_When to use:_ Distributed Erlang is primarily used for Erlang-Erlang
communication. It can also be used for communication between Erlang and C, if
the C program is implemented as a C node, see
[C and Java Libraries](overview.md#c-nodes).

_Where to read more:_ Distributed Erlang and some distributed programming
techniques are described in the Erlang book.

For more information, see
[Distributed Programming](`e:system:conc_prog.md#distributed-programming`).

Relevant manual pages are the following:

- `m:erlang` manual page in ERTS (describes the BIFs)
- `m:global` manual page in Kernel
- `m:net_adm` manual page in Kernel
- `m:pg` manual page in Kernel
- `m:rpc` manual page in Kernel
- `m:pool` manual page in STDLIB
- `m:slave` manual page in STDLIB

### Ports and Linked-In Drivers

Ports provide the basic mechanism for communication with the external world,
from Erlang's point of view. The ports provide a byte-oriented interface to an
external program. When a port is created, Erlang can communicate with it by
sending and receiving [lists of bytes](`t:iolist/0`) or [binaries](`t:binary/0`) (not Erlang terms).
This means that the programmer might have to invent a suitable encoding and decoding scheme.

The implementation of the port mechanism depends on the platform. For UNIX,
pipes are used and the external program is assumed to read from standard input
and write to standard output. The external program can be written in any
programming language as long as it can handle the interprocess communication
mechanism with which the port is implemented.

The external program resides in another OS process than the Erlang runtime
system. In some cases this is not acceptable. Consider, for example, drivers
with very hard time requirements. It is therefore possible to write a program in
C according to certain principles, and dynamically link it to the Erlang runtime
system. This is called a _linked-in driver_.

_When to use:_ Ports can be used for all kinds of interoperability situations
where the Erlang program and the other program runs on the same machine.
Programming is fairly straight-forward.

Linked-in drivers involves writing certain call-back functions in C. This
requires very good skills as the code is linked to the Erlang runtime system.
It is recommended to use [NIFs](#native-implemented-functions-nifs)
instead of linked-in drivers as they provide a richer feature set and can use
[dirty schedulers for lengthy work](`e:erts:erl_nif.md#dirty_nifs`).

> #### Warning {: .warning }
>
> A faulty linked-in driver causes the entire Erlang runtime system to leak
> memory, hang, or crash.

_Where to read more:_ Ports are described in section "Miscellaneous Items" of
the Erlang book. Linked-in drivers are described in Appendix E.

The BIF [`open_port/2`](`open_port/2`) is documented in the `m:erlang` manual
page in ERTS.

For linked-in drivers, the programmer needs to read the `m:erl_ddll` manual page
in Kernel.

_Examples:_ Port example in [Ports](c_port.md).

### Native implemented functions (Nifs)

NIFs provide an alternative to a port using linked-in drivers to link C code into
the Erlang runtime system. NIFs make it possible to provide C implementation of
normal Erlang functions when interacting with the OS or some other external library.

> #### Warning {: .warning }
>
> A faulty NIFs causes the entire Erlang runtime system to leak
> memory, hang, crash, or leak sensitive information.

_When to use:_ Since a faulty NIF can cause many different problems related to both
stability and security it is recommended to use an external Port if possible. If the
overhead is not acceptable then a NIF is a good solution for interacting with any
native code, be it in C, C++ or Rust.

_Where to read more:_ NIFs are described in [API functions for an Erlang NIF library](`e:erts:erl_nif.md`).

_Examples:_ Port example in [NIFs](nif.md).

## C and Java Libraries

### Erl_Interface

The program at the other side of a port is often a C program. To help the C
programmer, the Erl_Interface library has been developed

The Erlang external term format is a representation of an Erlang term as a
sequence of bytes, that is, a binary. Conversion between the two representations
is done using the following BIFs:

```text
Binary = term_to_binary(Term)
Term = binary_to_term(Binary)
```

A port can be set to use binaries instead of lists of bytes. It is then not
necessary to invent any encoding/decoding scheme. Erl_Interface functions are
used for unpacking the binary and convert it into a struct similar to an Erlang
term. Such a struct can be manipulated in different ways, be converted to the
Erlang external format, and sent to Erlang.

_When to use:_ In C code, in conjunction with Erlang binaries.

_Where to read more:_ See the Erlang Interface User's Guide, Command Reference,
and Library Reference. In Erlang/OTP R5B, and earlier versions, the information
is part of the Kernel application.

_Examples:_ Erl_Interface example in [Erl_Interface](erl_interface.md).

### C Nodes

A C program that uses the Erl_Interface functions for setting up a connection
to, and communicating with, a distributed Erlang node is called a _C node_, or a
_hidden node_. The main advantage with a C node is that the communication from
the Erlang programmer's perspective is extremely easy, as the C program behaves
as a distributed Erlang node.

_When to use:_ C nodes can typically be used on device processors (as opposed to
control processors) where C is a better choice than Erlang due to memory
limitations or application characteristics, or both.

_Where to read more:_ See the `ei_connect` part of the
[Erl_Interface](erl_interface.md) documentation. The programmer also needs to be
familiar with TCP/IP sockets, see Sockets in
[Standard Protocols](overview.md#sockets) and Distributed Erlang in
[Built-In Mechanisms](overview.md#distributed-erlang).

_Example:_ C node example in [C Nodes](cnode.md).

### Jinterface

In Erlang/OTP R6B, a library similar to Erl_Interface for Java was added called
_jinterface_. It provides a tool for Java programs to communicate with Erlang
nodes.

## Standard Protocols

Sometimes communication between an Erlang program and another program using a
standard protocol is desirable. Erlang/OTP currently supports TCP/IP and UDP
_sockets_: as follows:

- SNMP
- HTTP
- IIOP (CORBA)

Using one of the latter three requires good knowledge about the protocol and is
not covered by this tutorial. See the SNMP, Inets, and Orber applications,
respectively.

### Sockets

Simply put, connection-oriented socket communication (TCP/IP) consists of an
initiator socket ("server") started at a certain host with a certain port
number. A connector socket ("client"), which is aware of the initiator host name
and port number, can connect to it and data can be sent between them.

Connection-less socket communication (UDP) consists of an initiator socket at a
certain host with a certain port number and a connector socket sending data to
it.

For a detailed description of the socket concept, refer to a suitable book about
network programming. A suggestion is _UNIX Network Programming, Volume 1:
Networking APIs - Sockets and XTI_ by W. Richard Stevens, ISBN: 013490012X.

In Erlang/OTP, access to TCP/IP and UDP sockets is provided by the modules
`gen_tcp` and `gen_udp` in Kernel. Both are easy to use and do not require
detailed knowledge about the socket concept.

_When to use:_ For programs running on the same or on another machine than the
Erlang program.

_Where to read more:_ See the `m:gen_tcp` and the `m:gen_udp` manual pages in
Kernel.

## IC and CORBA

IC (Erlang IDL Compiler) is an interface generator that, given an IDL interface
specification, automatically generates stub code in Erlang, C, or Java. See the
IC User's Guide and IC Reference Manual.

For details, see the [corba repository](https://github.com/erlang/corba).
