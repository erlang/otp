%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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
-module(snmpm_network_interface_filter).
-moduledoc """
Behaviour module for the SNMP manager network-interface filter.

This module defines the behaviour of the manager network interface filter. A
`snmpm_network_interface_filter` compliant module must export the following
functions:

- [accept_recv/2](`m:snmpm_network_interface_filter#accept_recv`)
- [accept_send/2](`m:snmpm_network_interface_filter#accept_send`)
- [accept_recv_pdu/3](`m:snmpm_network_interface_filter#accept_recv_pdu`)
- [accept_send_pdu/2](`m:snmpm_network_interface_filter#accept_send_pdu`)

The semantics of them and their exact signatures are explained below.

The purpose of the network interface filter is to allow for filtering of
messages (accept or reject) receive and send. This is done on two levels:

- The first level is at the UDP entry / exit point, i.e. immediately after the
  receipt of the message, before any message processing is done (accept_recv)
  and immediately before sending the message, after all message processing is
  done (accept_send).
- The second level is at the MPD entry / exit point, i.e. immediately after the
  basic message processing (accept_recv_pdu) / immediately before the basic
  message processing (accept_send_pdu).

Note that the network interface filter is something which is used by the network
interface implementation provided by the application (`snmpm_net_if` and
`snmpm_net_if_mt`). The default filter accepts all messages.

A network interface filter can e.g. be used during testing or for load
regulation.

Legacy network interface filter modules used arguments on the form
`(IpAddr, PortNumber,...)` instead of `(Domain, Addr, ...)`, and if the SNMP
manager is run without changing the configuration to use transport domains the
network interface filter will still get the old arguments and work as before.

## DATA TYPES

```text
port() = integer() > 0
pdu_type() = 'get-request' | 'get-next-request' | 'get-response' |
             'set-request' | trap | 'get-bulk-request' | 'inform-request' |
             report | trappdu
```

See also the [data types in `snmpa_conf`](`m:snmpa_conf#types`).

[](){: #accept_recv }
""".

-export([verify/1]).

-type transportDomain() :: snmpa_conf:transportDomain().
-type transportAddressWithPort() :: snmpa_conf:transportAddressWithPort().
-type pdu_type() :: snmpm:pdu_type().

%% accept_recv(address(), port()) -> boolean()
%% Called at the reception of a message
%% (before *any* processing has been done).
-doc """
Called at the reception of a message (before _any_ processing has been done).

For the message to be rejected, the function _must_ return _false_.

[](){: #accept_send }
""".
-callback accept_recv(Domain, Addr) -> boolean() when
                             Domain :: transportDomain(),
                             Addr :: transportAddressWithPort().
%% accept_send(address(), port()) -> boolean()
%% Called before the sending of a message
%% (after *all* processing has been done).
-doc """
Called before the sending of a message (after _all_ processing has been done).

For the message to be rejected, the function _must_ return _false_.

[](){: #accept_recv_pdu }
""".
-callback accept_send(Domain, Addr) -> boolean() when
                             Domain :: transportDomain(),
                             Addr :: transportAddressWithPort().
%% accept_recv_pdu(Addr, Port, pdu_type()) -> boolean()
%% Called after the basic message processing (MPD) has been done,
%% but before the pdu is handed over to the master-agent for
%% primary processing.
-doc """
Called after the basic message processing (MPD) has been done, but before the
pdu is handed over to the server for primary processing.

For the pdu to be rejected, the function _must_ return _false_.

[](){: #accept_send_pdu }
""".
-callback accept_recv_pdu(Domain, Addr, PduType) -> boolean() when
                                 Domain :: transportDomain(),
                                 Addr :: transportAddressWithPort(),
                                 PduType :: pdu_type().
%% accept_send_pdu(Addr, Port, pdu_type()) -> boolean()
%% Called before the basic message processing (MPD) is done, 
%% when a pdu has been received from the master-agent.
-doc """
Called before the basic message processing (MPD) is done, when a pdu has been
received from the master-agent.

For the message to be rejected, the function _must_ return _false_.
""".
-callback accept_send_pdu(Domain, Addr, PduType) -> boolean() when
                                 Domain :: transportDomain(),
                                 Addr :: transportAddressWithPort(),
                                 PduType :: pdu_type().

-doc false.
verify(Module) ->
    snmp_misc:verify_behaviour(?MODULE, Module).
