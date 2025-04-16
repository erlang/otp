%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
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

-module(diameter_transport).
-moduledoc """
Diameter transport interface.

A module specified as a `transport_module` to `diameter:add_transport/2` must
implement the interface documented here. The interface consists of a function
with which diameter starts a transport process and a message interface with
which the transport process communicates with the process that starts it (aka
its parent).

## DATA TYPES

- **`message() = binary() | `[`diameter_codec:packet()`](`m:diameter_codec#packet`)**{: #message } -
  A Diameter message as passed over the transport interface.

  For an inbound message from a transport process, a
  [diameter_codec:packet()](`m:diameter_codec#packet`) must contain the received
  message in its `bin` field. In the case of an inbound request, any value set
  in the `transport_data` field will passed back to the transport module in the
  corresponding answer message, unless the sender supplies another value.

  For an outbound message to a transport process, a
  [diameter_codec:packet()](`m:diameter_codec#packet`) has a value other than
  `undefined` in its `transport_data` field and has the binary() to send in its
  `bin` field.

[](){: #MESSAGES }

## MESSAGES

All messages sent over the transport interface are of the form
`{diameter, term()}`.

A transport process can expect messages of the following types from its parent.

- **`{diameter, {send, `[`message()`](`m:diameter_transport#message`)` | false}}`** -
  An outbound Diameter message. The atom `false` can only be received when
  request acknowledgements have been requests: see the `ack` message below.

- **`{diameter, {close, Pid}}`** - A request to terminate the transport process
  after having received DPA in response to DPR. The transport process should
  exit. `Pid` is the pid() of the parent process.

- **`{diameter, {tls, Ref, Type, Bool}}`** - Indication of whether or not
  capabilities exchange has selected inband security using TLS. `Ref` is a
  reference() that must be included in the `{diameter, {tls, Ref}}` reply
  message to the transport's parent process (see below). `Type` is either
  `connect` or `accept` depending on whether the process has been started for a
  connecting or listening transport respectively. `Bool` is a boolean()
  indicating whether or not the transport connection should be upgraded to TLS.

  If TLS is requested (`Bool=true`) then a connecting process should initiate a
  TLS handshake with the peer and an accepting process should prepare to accept
  a handshake. A successful handshake should be followed by a
  `{diameter, {tls, Ref}}` message to the parent process. A failed handshake
  should cause the process to exit.

  This message is only sent to a transport process over whose
  `Inband-Security-Id` configuration has indicated support for TLS.

A transport process should send messages of the following types to its parent.

- **`{diameter, {self(), connected}}`** - Inform the parent that the transport
  process with `Type=accept` has established a connection with the peer. Not
  sent if the transport process has `Type=connect`.

- **`{diameter, {self(), connected, Remote}}`**

- **`{diameter, {self(), connected, Remote, [LocalAddr]}}`** - Inform the parent
  that the transport process with `Type=connect` has established a connection
  with a peer. Not sent if the transport process has `Type=accept`.  `Remote` is
  an arbitrary term that uniquely identifies the remote endpoint to which the
  transport has connected. A `LocalAddr` list has the same semantics as one
  returned from `c:start/3`.

- **`{diameter, ack}`** - Request acknowledgements of unanswered requests. A
  transport process should send this once before passing incoming Diameter
  messages into diameter. As a result, every Diameter request passed into
  diameter with a `recv` message (below) will be answered with a `send` message
  (above), either a [message()](`m:diameter_transport#message`) for the
  transport process to send or the atom `false` if the request has been
  discarded or otherwise not answered.

  This is to allow a transport process to keep count of the number of incoming
  request messages that have not yet been answered or discarded, to allow it to
  regulate the amount of incoming traffic. Both diameter_tcp and diameter_sctp
  request acknowledgements when a `message_cb` is configured, turning send/recv
  message into callbacks that can be used to regulate traffic.

- **`{diameter, {recv, `[`message()`](`m:diameter_transport#message`)`}}`** - An
  inbound Diameter message.

- **`{diameter, {tls, Ref}}`** - Acknowledgment of a successful TLS handshake.
  `Ref` is the reference() received in the `{diameter, {tls, Ref, Type, Bool}}`
  message in response to which the reply is sent. A transport must exit if a
  handshake is not successful.

## SEE ALSO

`m:diameter_tcp`, `m:diameter_sctp`
""".
-moduledoc(#{since => "OTP R14B03"}).

-include_lib("diameter/include/diameter.hrl").

%%
%% This module implements a transport start function that
%% evaluates its config argument.
%%

%% Transport start functions
-export([start/3,
         select/3,
         eval/3]).

-doc """
Start a transport process. Called by diameter as a consequence of a call to
`diameter:add_transport/2` in order to establish or accept a transport
connection respectively. A transport process maintains a connection with a
single remote peer.

`Type` indicates whether the transport process in question is being started for
a connecting (`Type=connect`) or listening (`Type=accept`) transport. In the
latter case, transport processes are started as required to accept connections
from multiple peers.

Ref is the value that was returned from the call to `diameter:add_transport/2`
that has lead to starting of a transport process.

`Svc` contains capabilities passed to `diameter:start_service/2` and
`diameter:add_transport/2`, values passed to the latter overriding those passed
to the former.

`Config` is as passed in `transport_config` tuple in the
[diameter:transport_opt()](`m:diameter#transport_opt`) list passed to
`diameter:add_transport/2`.

The start function should use the `Host-IP-Address` list in `Svc` and/or
`Config` to select and return an appropriate list of local IP addresses. In the
connecting case, the local address list can instead be communicated in a
`connected` message (see [MESSAGES](`m:diameter_transport#MESSAGES`) below)
following connection establishment. In either case, the local address list is
used to populate `Host-IP-Address` AVPs in outgoing capabilities exchange
messages if `Host-IP-Address` is unspecified.

A transport process must implement the message interface documented below. It
should retain the pid of its parent, monitor the parent and terminate if it
dies. It should not link to the parent. It should exit if its transport
connection with its peer is lost.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback start({Type, Ref}, Svc, Config) ->
    {ok, Pid} | {ok, Pid, LAddrs} | {error, Reason} when
      Type :: connect | accept,
      Ref :: diameter:transport_ref(),
      Svc :: #diameter_service{},
      Config :: term(),
      Pid :: pid(),
      LAddrs :: [inet:ip_address()],
      Reason :: term().

%% start/3

%% Call a start function in this module ...
-doc false.
start(T, Svc, {F,A}) ->
    start(T, Svc, {?MODULE, F, [A]});

%% ... or some other.
start(T, Svc, F) ->
    diameter_lib:eval([F, T, Svc]).

%% select/3
%%
%% A start function that whose config argument is expected to return a
%% new start function.

-doc false.
select(T, Svc, F) ->
    start(T, Svc, diameter_lib:eval([F, T, Svc])).

%% eval/3
%%
%% A start function that simply evaluates its config argument.

-doc false.
eval(_, _, F) ->
    diameter_lib:eval(F).
