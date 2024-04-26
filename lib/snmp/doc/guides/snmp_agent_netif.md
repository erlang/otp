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
# Definition of Agent Net if

![The Purpose of Agent Net if](assets/snmp_agent_netif_1.gif "The Purpose of Agent Net if")

The Network Interface (Net If) process delivers SNMP PDUs to a master agent, and
receives SNMP PDUs from the master agent. The most common behaviour of a Net if
process is that is receives bytes from a network, decodes them into an SNMP PDU,
which it sends to a master agent. When the master agent has processed the PDU,
it sends a response PDU to the Net if process, which encodes the PDU into bytes
and transmits the bytes onto the network.

However, that simple behaviour can be modified in numerous ways. For example,
the Net if process can apply some kind of encrypting/decrypting scheme on the
bytes or act as a proxy filter, which sends some packets to a proxy agent and
some packets to the master agent.

It is also possible to write your own Net if process. The default Net if process
is implemented in the module `snmpa_net_if` and it uses UDP as the transport
protocol i.e the transport domains `transportDomainUdpIpv4` and/or
`transportDomainUdpIpv6`.

This section describes how to write a Net if process.

## Mandatory Functions

A Net if process must implement the SNMP agent
[network interface behaviour](`m:snmpa_network_interface`).

## Messages

The section _Messages_ describes mandatory messages, which Net If must send and
be able to receive.

In this section an `Address` field is a `{Domain, Addr}` tuple where `Domain` is
`transportDomainUdpIpv4` or `transportDomainUdpIpv4`, and `Addr` is an
`{`[`IpAddr`](`t:inet:ip_address/0`)`,IpPort}` tuple.

[](){: #outgoing_messages }

### Outgoing Messages

Net if must send the following message when it receives an SNMP PDU from the
network that is aimed for the MasterAgent:

```text
MasterAgent ! {snmp_pdu, Vsn, Pdu, PduMS, ACMData, From, Extra}
```

{: #om_snmp_pdu }

- `Vsn` is either `'version-1'`, `'version-2'`, or `'version-3'`.
- `Pdu` is an SNMP PDU record, as defined in `snmp_types.hrl`, with the SNMP
  request.
- `PduMS` is the Maximum Size of the response Pdu allowed. Normally this is
  returned from `snmpa_mpd:process_packet` (see Reference Manual).
- `ACMData` is data used by the Access Control Module in use. Normally this is
  returned from `snmpa_mpd:process_packet` (see Reference Manual).
- `From` is the source `Address`.
- `Extra` is any term the Net if process wishes to send to the agent. This term
  can be retrieved by the instrumentation functions by calling
  `snmp:current_net_if_data()`. This data is also sent back to the Net if
  process when the agent generates a response to the request.

The following message is used to report that a response to a request has been
received. The only request an agent can send is an Inform-Request.

```text
Pid ! {snmp_response_received, Vsn, Pdu, From}
```

{: #om_snmp_response_received }

- `Pid` is the Process that waits for the response for the request. The Pid was
  specified in the `send_pdu_req` message
  [(see below)](snmp_agent_netif.md#im_send_pdu_req).
- `Vsn` is either `'version-1'`, `'version-2'`, or `'version-3'`.
- `Pdu` is the SNMP Pdu received
- `From` is the source `Address`.

[](){: #incoming_messages }

### Incoming Messages

This section describes the incoming messages which a Net if process must be able
to receive.

- [](){: #im_snmp_response }
  `{snmp_response, Vsn, Pdu, Type, ACMData, To, Extra}`

  This message is sent to the Net if process from a master agent as a response
  to a previously received request.

  - `Vsn` is either `'version-1'`, `'version-2'`, or `'version-3'`.
  - `Pdu` is an SNMP PDU record (as defined in snmp_types.hrl) with the SNMP
    response.
  - `Type` is the `#pdu.type` of the original request.
  - `ACMData` is data used by the Access Control Module in use. Normally this is
    just sent to `snmpa_mpd:generate_response_message` (see Reference Manual).
  - `To` is the destination `Address` that comes from the `From` field in the
    corresponding `snmp_pdu` message previously sent to the MasterAgent.
  - `Extra` is the term that the Net if process sent to the agent when the
    request was sent to the agent.

- [](){: #im_discarded_pdu }
  `{discarded_pdu, Vsn, ReqId, ACMData, Variable, Extra}`

  This message is sent from a master agent if it for some reason decided to
  discard the pdu.

  - `Vsn` is either `'version-1'`, `'version-2'`, or `'version-3'`.
  - `ReqId` is the request id of the original request.
  - `ACMData` is data used by the Access Control Module in use. Normally this is
    just sent to `snmpa_mpd:generate_response_message` (see Reference Manual).
  - `Variable` is the name of an snmp counter that represents the error, e.g.
    `snmpInBadCommunityUses`.
  - `Extra` is the term that the Net if process sent to the agent when the
    request was sent to the agent.

- [](){: #im_send_pdu } `{send_pdu, Vsn, Pdu, MsgData, To, Extra}`

  This message is sent from a master agent when a trap is to be sent.

  - `Vsn` is either `'version-1'`, `'version-2'`, or `'version-3'`.
  - `Pdu` is an SNMP PDU record (as defined in snmp_types.hrl) with the SNMP
    response.
  - `MsgData` is the message specific data used in the SNMP message. This value
    is normally sent to `snmpa_mpd:generate_msg/5`. In SNMPv1 and SNMPv2c,
    this message data is the community string. In SNMPv3, it is the context
    information.
  - `To` is a list of `{Address, SecData}` tuples i.e the destination addresses
    and their corresponding security parameters. This value is normally sent to
    `snmpa_mpd:generate_msg/5`.
  - `Extra` is any term that the notification sender wishes to pass to the Net
    if process when sending a notification (see
    [`send notification`](`snmpa:send_notification2/3`)for more info).

- [](){: #im_send_pdu_req } `{send_pdu_req, Vsn, Pdu, MsgData, To, Pid, Extra}`

  This message is sent from a master agent when a request is to be sent. The
  only request an agent can send is Inform-Request. The net if process needs to
  remember the request id and the Pid, and when a response is received for the
  request id, send it to Pid, using a `snmp_response_received` message.

  - `Vsn` is either `'version-1'`, `'version-2'`, or `'version-3'`.
  - `Pdu` is an SNMP PDU record (as defined in snmp_types.hrl) with the SNMP
    response.
  - `MsgData` is the message specific data used in the SNMP message. This value
    is normally sent to `snmpa_mpd:generate_msg/5`. In SNMPv1 and SNMPv2c,
    this message data is the community string. In SNMPv3, it is the context
    information.
  - `To` is a list of `{Address, SecData}` tuples i.e the destination addresses
    and their corresponding security parameters. This value is normally sent to
    `snmpa_mpd:generate_msg/5`.
  - `Pid` is a process identifier.
  - `Extra` is any term that the notification sender wishes to pass to the Net
    if process when sending a notification (see
    [`send notification`](`snmpa:send_notification2/3`)for more info).

### Notes

Since the Net if process is responsible for encoding and decoding of SNMP
messages, it must also update the relevant counters in the SNMP group in MIB-II.
It can use the functions in the module `snmpa_mpd` for this purpose (refer to
the Reference Manual, section `snmp`, module [snmpa_mpd](`m:snmp_pdus`) for more
details.)

There are also some useful functions for encoding and decoding of SNMP messages
in the module `m:snmp_pdus`.
