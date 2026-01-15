%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
-module(snmpa_network_interface_filter).
-moduledoc """
Behaviour module for the SNMP agent network-interface filter.

This module defines the behaviour of the agent network interface filter. A
`snmpa_network_interface_filter` compliant module must export the following
functions:

- `accept_recv/2`
- `accept_send/2`
- `accept_recv_pdu/3`
- `accept_send_pdu/2`

The semantics of them and their exact signatures are explained below.

The purpose of the network interface filter is to allow for filtering of
messages (accept or reject) receive and send. This is done on two levels:

- The first level is at the transport entry / exit point, i.e. immediately after
  the receipt of the message before any message processing is done (accept_recv)
  and immediately before sending the message after all message processing is
  done (accept_send).
- The second level is at the MPD entry / exit point, i.e. immediately after the
  basic message processing (accept_recv_pdu) / immediately before the basic
  message processing (accept_send_pdu).

Note that the network interface filter is something which is used by the network
interface implementation provided by the application (`snmpa_net_if`). The
default filter accepts all messages.

A network interface filter can e.g. be used during testing or for load
regulation. If the intended use is load regulation, see also
[req_limit](snmp_config.md#agent_ni_req_limit) and the function
[register_notification_filter](`snmpa:register_notification_filter/5`).

Legacy network interface filter modules used arguments on the form
`(IpAddr, PortNumber,...)` instead of `(Domain, Addr, ...)`, and if the SNMP
agent is run without changing the configuration to use transport domains the
network interface filter will still get the old arguments and work as before.

See also the [data types in `snmpa_conf`](`m:snmpa_conf#types`).

""".

-export([verify/1]).

-type transportDomain() :: snmpa_conf:transportDomain().
-type transportAddressWithPort() :: snmpa_conf:transportAddressWithPort().
-type pdu_type() :: snmpa:pdu_type().

%% accept_recv({domain(), address()}) -> boolean()
%% Called at the reception of a message
%% (before *any* processing has been done).
-doc """
Called at the reception of a message (before _any_ processing has been done).

For the message to be discarded, the function _must_ return _false_.

""".
-callback accept_recv(Domain, Addr) -> boolean() when
      Domain :: transportDomain(),
      Addr :: transportAddressWithPort().
%% accept_send({domain(), address()}) -> boolean()
%% Called before the sending of a message
%% (after *all* processing has been done).
-doc """
Called before the sending of a message (after _all_ processing has been done).

For the message to be discarded, the function _must_ return _false_.

""".
-callback accept_send(Domain, Addr) -> boolean() when
      Domain :: transportDomain(),
      Addr :: transportAddressWithPort().
%% accept_recv_pdu({domain(), address()}, pdu_type()) -> boolean()
%% Called after the basic message processing (MPD) has been done,
%% but before the pdu is handed over to the master-agent for
%% primary processing.
-doc """
Called after the basic message processing (MPD) has been done, but before the
pdu is handed over to the master-agent for primary processing.

For the pdu to be discarded, the function _must_ return _false_.

""".
-callback accept_recv_pdu(Domain, Addr, PduType) -> boolean() when
      Domain :: transportDomain(),
      Addr :: transportAddressWithPort(),
      PduType :: pdu_type().

-doc """
Called before the basic message processing (MPD) is done, when a pdu has been
received from the master-agent.

For the message to be discarded all together, the function _must_ return
_false_.

Note that it is possible for this function to filter out targets (but _not_ to
add its own) by returning an updated `Targets` list (`NewTargets`).
""".
-callback accept_send_pdu(Targets, PduType) -> Reply when
      Targets :: [Target],
      Target :: {Domain, Addr},
      Domain :: transportDomain(),
      Addr :: transportAddressWithPort(),
      PduType :: pdu_type(),
      Reply :: boolean() | NewTargets,
      NewTargets :: Targets.

-doc false.
verify(Module) ->
    snmp_misc:verify_behaviour(?MODULE, Module).
