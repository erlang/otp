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
# Introduction

SSH is a protocol for secure remote logon and other secure network services over
an insecure network.

## Scope and Purpose

SSH provides a single, full-duplex, and byte-oriented connection between client
and server. The protocol also provides privacy, integrity, server
authentication, and man-in-the-middle protection.

The `ssh` application is an implementation of the SSH Transport, Connection and
Authentication Layer Protocols in Erlang. It provides the following:

- API functions to write customized SSH clients and servers applications
- The Erlang shell available over SSH
- An SFTP client (`m:ssh_sftp`) and server ([ssh_sftpd](`m:ssh_sftp`))

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language,
concepts of _OTP_, and has a basic understanding of _public keys_.

## SSH Protocol Overview

Conceptually, the SSH protocol can be partitioned into four layers:

![SSH Protocol Architecture](assets/SSH_protocols.png "SSH Protocol Architecture")

### Transport Protocol

The SSH Transport Protocol is a secure, low-level transport. It provides strong
encryption, cryptographic host authentication, and integrity protection. A
minimum of Message Authentication Code (MAC) and encryption algorithms are
supported. For details, see the `m:ssh` manual page in `ssh`.

### Authentication Protocol

The SSH Authentication Protocol is a general-purpose user authentication
protocol run over the SSH Transport Layer Protocol. The `ssh` application
supports user authentication as follows:

- Using public key technology. RSA and DSA, X509-certificates are not supported.
- Using keyboard-interactive authentication. This is suitable for interactive
  authentication methods that do not need any special software support on the
  client side. Instead, all authentication data is entered from the keyboard.
- Using a pure password-based authentication scheme. Here, the plain text
  password is encrypted before sent over the network.

Several configuration options for authentication handling are available in
[ssh:connect/\[3,4]](`ssh:connect/3`) and [ssh:daemon/\[2,3]](`ssh:daemon/2`).

The public key handling can be customized by implementing the following
behaviours from `ssh`:

- Module `m:ssh_client_key_api`.
- Module `m:ssh_server_key_api`.

### Connection Protocol

The SSH Connection Protocol provides application-support services over the
transport pipe, for example, channel multiplexing, flow control, remote program
execution, signal propagation, and connection forwarding. Functions for handling
the SSH Connection Protocol can be found in the module `m:ssh_connection` in
`ssh`.

### Channels

All terminal sessions, forwarded connections, and so on, are channels. Multiple
channels are multiplexed into a single connection. All channels are
flow-controlled. This means that no data is sent to a channel peer until a
message is received to indicate that window space is available. The _initial
window size_ specifies how many bytes of channel data that can be sent to the
channel peer without adjusting the window. Typically, an SSH client opens a
channel, sends data (commands), receives data (control information), and then
closes the channel. The `m:ssh_client_channel` behaviour handles generic parts
of SSH channel management. This makes it easy to write your own SSH
client/server processes that use flow-control and thus opens for more focus on
the application logic.

Channels come in the following three flavors:

- _Subsystem_ \- Named services that can be run as part of an SSH server, such
  as SFTP [(ssh_sftpd)](`m:ssh_sftpd`), that is built into the SSH daemon
  (server) by default, but it can be disabled. The Erlang `ssh` daemon can be
  configured to run any Erlang- implemented SSH subsystem.
- _Shell_ \- Interactive shell. By default the Erlang daemon runs the Erlang
  shell. The shell can be customized by providing your own read-eval-print loop.
  You can also provide your own Command-Line Interface (CLI) implementation, but
  that is much more work.
- _Exec_ \- One-time remote execution of commands. See function
  `ssh_connection:exec/4` for more information.

## Where to Find More Information

For detailed information about the SSH protocol, refer to the following Request
for Comments(RFCs):

- [RFC 4250](http://www.ietf.org/rfc/rfc4250.txt) \- Protocol Assigned Numbers
- [RFC 4251](http://www.ietf.org/rfc/rfc4251.txt) \- Protocol Architecture
- [RFC 4252](http://www.ietf.org/rfc/rfc4252.txt) \- Authentication Protocol
- [RFC 4253](http://www.ietf.org/rfc/rfc4253.txt) \- Transport Layer Protocol
- [RFC 4254](http://www.ietf.org/rfc/rfc4254.txt) \- Connection Protocol
- [RFC 4344](http://www.ietf.org/rfc/rfc4344.txt) \- Transport Layer Encryption
  Modes
- [RFC 4716](http://www.ietf.org/rfc/rfc4716.txt) \- Public Key File Format
