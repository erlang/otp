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
# Definition of Manager Net if

![The Purpose of Manager Net if](assets/snmp_manager_netif_1.gif "The Purpose of Manager Net if")

The Network Interface (Net If) process delivers SNMP PDUs to the manager server,
and receives SNMP PDUs from the manager server. The most common behaviour of a
Net if process is that is receives request PDU from the manager server, encodes
the PDU into bytes and transmits the bytes onto the network to an agent. When
the reply from the agent is received by the Net if process, which it decodes
into an SNMP PDU, which it sends to the manager server.

However, that simple behaviour can be modified in numerous ways. For example,
the Net if process can apply some kind of encrypting/decrypting scheme on the
bytes.

The snmp application provides two different modules, `snmpm_net_if` (the
default) and `snmpm_net_if_mt`, both uses UDP as the transport protocol i.e the
transport domains `transportDomainUdpIpv4` and/or `transportDomainUdpIpv6`. The
difference between the two modules is that the latter is "multi-threaded", i.e.
for each message/request a new process is created that processes the
message/request and then exits.

There is a `server` config option,
[netif_sup](snmp_config.md#manager_server_nis) that enables "active" Net If
supervision. This is very simple mechanism. The (supervising) process simply
sends a [ping](snmp_manager_netif.md#im_ping) message and expects a
[pong](snmp_manager_netif.md#om_pong) message response (within a specific time).
The interval between each `ping/pong` exchange is user configurable. As is the
time that is allowed for the [pong](snmp_manager_netif.md#om_pong) message to
arrive. Both the NetIf module(s) provided with the app supports active
supervision. If a NetIf module/process is used which do not implement this, then
the server cannot be configured with active supervision.

It is also possible to write your own Net if process and this section describes
how to do that.

[](){: #mandatory_functions }

## Mandatory Functions

A Net If process must implement the SNMP manager
[network interface behaviour](`m:snmpm_network_interface`).

## Messages

The section _Messages_ describes mandatory (with exception for the ping/pong
messages) messages, which Net If must send to the manager server process.

In this section a `Domain` field is the transport domain i.e one of
`transportDomainUdpIpv4` or `transportDomainUdpIpv6`, and an `Addr` field is an
`{`[`IpAddr`](`t:inet:ip_address/0`)`,IpPort}` tuple.

[](){: #outgoing_messages }

### Outgoing Messages

Net if must send the following message when it receives an SNMP PDU from the
network that is aimed for the MasterAgent:

```text
Server ! {snmp_pdu, Pdu, Domain, Addr}
```

- `Pdu` is an SNMP PDU record, as defined in `snmp_types.hrl`, with the SNMP
  request.
- `Domain` is the source transport domain.
- `Addr` is the source address.

```text
Server ! {snmp_trap, Trap, Domain, Addr}
```

- `Trap` is either an SNMP pdu record or an trappdu record, as defined in
  `snmp_types.hrl`, with the SNMP request.
- `Domain` is the source transport domain.
- `Addr` is the source address.

```text
Server ! {snmp_inform, Ref, Pdu, PduMS, Domain, Addr}
```

- `Ref` is either the atom `ignore` or something that can be used to identify
  the inform-request (e.g. request-id). `ignore` is used if the response
  (acknowledgment) to the inform-request has already been sent (this means that
  the server will not make the call to the
  [inform_response](`m:snmpm_network_interface#inform_response`) function). See
  the [inform request behaviour](snmp_config.md#manager_irb) configuration
  option for more info.
- `Pdu` is an SNMP PDU record, as defined in `snmp_types.hrl`, with the SNMP
  request.
- `Domain` is the source transport domain.
- `Addr` is the source address.

```text
Server ! {snmp_report, Data, Domain, Addr}
```

- `Data` is either `{ok, Pdu}` or `{error, ReqId, ReasonInfo, Pdu}`. Which one
  is used depends on the return value from the MPD
  [process_msg](`m:snmpm_mpd#process_msg`) function. If the MsgData is `ok`, the
  first is used, and if it is `{error, ReqId, Reason}` the latter is used.
- `Pdu` is an SNMP PDU record, as defined in `snmp_types.hrl`, with the SNMP
  request.
- `ReqId` is an integer.
- `ReasonInfo` is a term().
- `Domain` is the source transport domain.
- `Addr` is the source address.

```text
Supervisor ! {pong, self()}
```

{: #om_pong }

- `Supervisor` is the process that sent the
  [ping](snmp_manager_netif.md#im_ping) message (see below).

[](){: #incoming_messages }

### Incoming Messages

This section describes the incoming messages which a Net If process may choose
to respond to.

- [](){: #im_ping } `{ping, Supervisor}`

  This message is sent to the Net If process by a process that has been
  configured to perfor "active supervision" of the Net If process. The Net If
  process should respond immediately with a
  [pong](snmp_manager_netif.md#om_pong) message.

  - `Supervisor` is a `t:pid/0`.

## Notes

Since the Net if process is responsible for encoding and decoding of SNMP
messages, it must also update the relevant counters in the SNMP group in MIB-II.
It can use the functions in the module `snmpm_mpd` for this purpose (refer to
the Reference Manual, section `snmp`, module `snmpm_mpd` for more details).

There are also some useful functions for encoding and decoding of SNMP messages
in the module `snmp_pdus`.
