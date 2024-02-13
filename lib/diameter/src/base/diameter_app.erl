%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2024. All Rights Reserved.
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

-module(diameter_app).
-moduledoc """
Callback module of a Diameter application.

A diameter service as started by `diameter:start_service/2` configures one of
more Diameter applications, each of whose configuration specifies a callback
that handles messages specific to the application. The messages and AVPs of the
application are defined in a dictionary file whose format is documented in
[diameter_dict(4)](diameter_dict.md) while the callback module is documented
here. The callback module implements the Diameter application-specific
functionality of a service.

A callback module must export all of the functions documented below. The
functions themselves are of three distinct flavours:

- `c:peer_up/3` and `c:peer_down/3` signal the attainment or loss of
  connectivity with a Diameter peer.
- `c:pick_peer/4`, `c:prepare_request/3`, `c:prepare_retransmit/3`,
  `c:handle_answer/4` and `c:handle_error/4` are (or may be) called as a
  consequence of a call to `diameter:call/4` to send an outgoing Diameter
  request message.
- `c:handle_request/3` is called in response to an incoming Diameter request
  message.

The arities for the the callback functions here assume no extra arguments. All
functions will also be passed any extra arguments configured with the callback
module itself when calling `diameter:start_service/2` and, for the call-specific
callbacks, any extra arguments passed to `diameter:call/4`.
""".
-moduledoc(#{since => "OTP R14B03"}).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include_lib("diameter/include/diameter.hrl").

-doc """
  A record containing the identities of the local Diameter node and the remote Diameter
  peer having an established transport connection, as well as the capabilities
  as determined by capabilities exchange. Each field of the record is a 2-tuple
  consisting of values for the (local) host and (remote) peer. Optional or
  possibly multiple values are encoded as lists of values, mandatory values as
  the bare value.
  """.
-type capabilities() :: #diameter_caps{}.
-doc "A term identifying a transport connection with a Diameter peer.".
-type peer_ref() :: term().
-doc "A tuple representing a Diameter peer connection.".
-type peer() :: {peer_ref(), capabilities()}.
-doc """
  A container for incoming and outgoing Diameter messages that's passed through
  encode/decode and transport. Fields should not be set in return values except
  as documented.
  """.
-type packet() :: diameter_codec:packet().
-doc """
  The state maintained by the application
  callback functions `c:peer_up/3`, `c:peer_down/3` and (optionally)
  `c:pick_peer/4`. The initial state is configured in the call to
  `diameter:start_service/2` that configures the application on a service.
  Callback functions returning a state are evaluated in a common
  service-specific process while those not returning state are evaluated in a
  request-specific process.
  """.
-type state() :: term().
-doc """
  The representation of a Diameter message as passed to `diameter:call/4` or
  returned from a `c:handle_request/3` callback.
  """.
-type message() :: diameter_codec:message().

-doc """
Invoked when a request message is received from a peer. The application in which
the callback takes place (that is, the callback module as configured with
`diameter:start_service/2`) is determined by the Application Identifier in the
header of the incoming request message, the selected module being the one whose
corresponding dictionary declares itself as defining either the application in
question or the Relay application.

The argument [packet()](`t:packet/0`) has the following signature.

```erlang
#diameter_packet{header = #diameter_header{},
                 avps   = [#diameter_avp{}],
                 msg    = record() | undefined,
                 errors = [Unsigned32() | {Unsigned32(), #diameter_avp{}}],
                 bin    = binary(),
                 transport_data = term()}
```

The `msg` field will be `undefined` in case the request has been received in the
relay application. Otherwise it contains the record representing the request as
outlined in [diameter_dict(4)](diameter_dict.md#MESSAGE_RECORDS).

The `errors` field specifies any results codes identifying errors found while
decoding the request. This is used to set Result-Code and/or Failed-AVP in a
returned answer unless the callback returns a `#diameter_packet{}` whose
`errors` field is set to either a non-empty list of its own, in which case this
list is used instead, or the atom `false` to disable any setting of Result-Code
and Failed-AVP. Note that the errors detected by diameter are of the 3xxx and
5xxx series, Protocol Errors and Permanent Failures respectively. The `errors`
list is empty if the request has been received in the relay application.

The `transport_data` field contains an arbitrary term passed into diameter from
the transport module in question, or the atom `undefined` if the transport
specified no data. The term is preserved if a
[message()](`t:message/0`) is returned but must be set explicitly in
a returned [packet()](`t:packet/0`).

The semantics of each of the possible return values are as follows.

- **`{reply, `[`packet()`](`t:packet/0`)`|`[`message()`](`t:message/0`)`}`** -
  Send the specified answer message to the peer. In the case of a
  [packet()](`t:packet/0`), the message to be sent must be set in the
  `msg` field and the `header` field can be set to a `#diameter_header{}` to
  specify values that should be preserved in the outgoing answer, appropriate
  values otherwise being set by diameter.

- **`{answer_message, 3000..3999|5000..5999}`** - Send an answer message to the
  peer containing the specified Result-Code. Equivalent to

  ```text
  {reply, ['answer-message' | Avps]
  ```

  where `Avps` sets the Origin-Host, Origin-Realm, the specified Result-Code and
  (if the request contained one) Session-Id AVPs, and possibly Failed-AVP as
  described below.

  Returning a value other than 3xxx or 5xxx will cause the request process in
  question to fail, as will returning a 5xxx value if the peer connection in
  question has been configured with the RFC 3588 common dictionary
  `diameter_gen_base_rfc3588`. (Since RFC 3588 only allows 3xxx values in an
  answer-message.)

  When returning 5xxx, Failed-AVP will be populated with the AVP of the first
  matching Result-Code/AVP pair in the `errors` field of the argument
  [packet()](`t:packet/0`), if found. If this is not appropriate then
  an answer-message should be constructed explicitly and returned in a `reply`
  tuple instead.

- **`{relay, Opts}`** - Relay a request to another peer in the role of a
  Diameter relay agent. If a routing loop is detected then the request is
  answered with 3005 (DIAMETER_LOOP_DETECTED). Otherwise a Route-Record AVP
  (containing the sending peer's Origin-Host) is added to the request and
  `c:pick_peer/4` and subsequent callbacks take place just as if
  `diameter:call/4` had been called explicitly. The End-to-End Identifier of the
  incoming request is preserved in the header of the relayed request.

  The returned `Opts` should not specify `detach`. A subsequent
  `c:handle_answer/4` callback for the relayed request must return its first
  argument, the [packet()](`t:packet/0`) containing the answer
  message. Note that the `extra` option can be specified to supply arguments
  that can distinguish the relay case from others if so desired. Any other
  return value (for example, from a `c:handle_error/4` callback) causes the
  request to be answered with 3002 (DIAMETER_UNABLE_TO_DELIVER).

- **`discard`** - Discard the request. No answer message is sent to the peer.

- **`{eval, Action, PostF}`** - Handle the request as if `Action` has been
  returned and then evaluate `PostF` in the request process. The return value is
  ignored.

- **`{eval_packet, Action, PostF}`** - Like `eval` but evaluate `PostF` on any
  encoded `#diameter_packet{}` prior to transmission, the `bin` field containing
  the encoded binary. The return value is ignored.

- **`{protocol_error, 3000..3999}`** - Equivalent to
  `{answer_message, 3000..3999}`.

> #### Note {: .info }
>
> Requests containing errors may be answered by diameter, without a callback
> taking place, depending on the value of the
> [diameter:application_opt()](`m:diameter#application_opt`) `request_errors`.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback handle_request(Packet, SvcName, Peer) -> Action when
      Packet :: packet(),
      SvcName :: term(),
      Peer :: peer(),
      Action :: Reply |
                {relay, [Opt]} |
                discard |
                {eval | eval_packet, Action, PostF},
      Reply :: {reply, packet() | message()} |
               {answer_message,
                3000..3999 | 5000..5999} |
               {protocol_error, 3000..3999},
      Opt :: diameter:call_opt(),
      PostF :: diameter:eval().

-doc """
Invoked when an error occurs before an answer message is received in response to
an outgoing request. The return value is returned from `diameter:call/4` unless
the `detach` option was specified.

Reason `timeout` indicates that an answer message has not been received within
the time specified with the corresponding
[diameter:call_opt()](`m:diameter#call_opt`). Reason `failover` indicates that
the transport connection to the peer to which the request has been sent has
become unavailable and that not alternate peer was not selected.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback handle_error(Reason, Request, SvcName, Peer) -> Result when
      Reason :: timeout | failover | term(),
      Request :: message(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Result :: term().

-doc """
Invoked when an answer message is received from a peer. The return value is
returned from `diameter:call/4` unless the `detach` option was specified.

The decoded answer record and undecoded binary are in the `msg` and `bin` fields
of the argument [packet()](`t:packet/0`) respectively. `Request` is
the outgoing request message as was returned from `c:prepare_request/3` or
`c:prepare_retransmit/3`.

For any given call to `diameter:call/4` there is at most one `c:handle_answer/4`
callback: any duplicate answer (due to retransmission or otherwise) is
discarded. Similarly, only one of `c:handle_answer/4` or `c:handle_error/4` is
called.

By default, an incoming answer message that cannot be successfully decoded
causes the request process to fail, causing `diameter:call/4` to return
`{error, failure}` unless the `detach` option was specified. In particular,
there is no `c:handle_error/4` callback in this case. The
[diameter:application_opt()](`m:diameter#application_opt`) `answer_errors` can
be set to change this behaviour.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback handle_answer(Packet, Request, SvcName, Peer) -> Result when
      Packet :: packet(),
      Request :: message(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Result :: term().

-doc """
Invoked to return a request for encoding and retransmission. Has the same role
as `c:prepare_request/3` in the case that a peer connection is lost an an
alternate peer selected but the argument [packet()](`t:packet/0`) is
as returned by the initial `c:prepare_request/3`.

Returning `{discard, Reason}` causes the request to be aborted and a
`c:handle_error/4` callback to take place with `Reason` as initial argument.
Returning `discard` is equivalent to returning `{discard, discarded}`.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback prepare_retransmit(Packet, SvcName, Peer) -> Action when
      Packet :: packet(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Action :: Send | Discard |
                {eval_packet, Action, PostF},
      Send :: {send, packet() | message()},
      Discard :: {discard, Reason :: term()} | discard,
      PostF :: diameter:eval().

-doc """
Invoked to return a request for encoding and transport. Allows the sender to use
the selected peer's capabilities to modify the outgoing request. Many
implementations may simply want to return `{send, Packet}`

A returned [packet()](`t:packet/0`) should set the request to be
encoded in its `msg` field and can set the `transport_data` field in order to
pass information to the transport process. Extra arguments passed to
`diameter:call/4` can be used to communicate transport (or any other) data to
the callback.

A returned [packet()](`t:packet/0`) can set the `header` field to a
`#diameter_header{}` to specify values that should be preserved in the outgoing
request, values otherwise being those in the header record contained in
`Packet`. A returned `length`, `cmd_code` or `application_id` is ignored.

A returned `PostF` will be evaluated on any encoded `#diameter_packet{}` prior
to transmission, the `bin` field containing the encoded binary. The return value
is ignored.

Returning `{discard, Reason}` causes the request to be aborted and the
`diameter:call/4` for which the callback has taken place to return
`{error, Reason}`. Returning `discard` is equivalent to returning
`{discard, discarded}`.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback prepare_request(Packet, SvcName, Peer) -> Action when
      Packet :: packet(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Action :: Send | Discard |
                {eval_packet, Action, PostF},
      Send :: {send, packet() | message()},
      Discard :: {discard, Reason :: term()} | discard,
      PostF :: diameter:eval().

-doc """
Invoked as a consequence of a call to `diameter:call/4` to select a destination
peer for an outgoing request. The return value indicates the selected peer.

The candidate lists contain only those peers that have advertised support for
the Diameter application in question during capabilities exchange, that have not
be excluded by a `filter` option in the call to `diameter:call/4` and whose
watchdog state machine is in the `OKAY` state. The order of the elements is
unspecified except that any peers whose Origin-Host and Origin-Realm matches
that of the outgoing request (in the sense of a `{filter, {all, [host, realm]}}`
option to `diameter:call/4`) will be placed at the head of the list.
`LocalCandidates` contains peers whose transport process resides on the local
Erlang node while `RemoteCandidates` contains peers that have been communicated
from other nodes by services of the same name.

A callback that returns a peer() will be followed by a `c:prepare_request/3`
callback and, if the latter indicates that the request should be sent, by either
`c:handle_answer/4` or `c:handle_error/4` depending on whether or not an answer
message is received from the peer. If the transport becomes unavailable after
`c:prepare_request/3` then a new `c:pick_peer/4` callback may take place to
failover to an alternate peer, after which `c:prepare_retransmit/3` takes the
place of `c:prepare_request/3` in resending the request. There is no guarantee
that a `c:pick_peer/4` callback to select an alternate peer will be followed by
any additional callbacks since a retransmission to an alternate peer is
abandoned if an answer is received from a previously selected peer.

The return values `false` and `{false, State}` (that is, `NewState = State`) are
equivalent, as are `{ok, Peer}` and `{Peer, State}`.

> #### Note {: .info }
>
> The [diameter:service_opt()](`m:diameter#service_opt`) `use_shared_peers`
> determines whether or not a service uses peers shared from other nodes. If not
> then `RemoteCandidates` is the empty list.

> #### Warning {: .warning }
>
> The return value `{Peer, NewState}` is only allowed if the Diameter
> application in question was configured with the
> [diameter:application_opt()](`m:diameter#application_opt`)
> `{call_mutates_state, true}`. Otherwise, the `State` argument is always the
> initial value as configured on the application, not any subsequent value
> returned by a `c:peer_up/3` or `c:peer_down/3` callback.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback pick_peer(LocalCandidates, RemoteCandidates, SvcName, State) ->
    Selection | false when
      LocalCandidates :: [peer()],
      RemoteCandidates :: [peer()],
      SvcName :: diameter:service_name(),
      State :: state(),
      NewState :: state(),
      Selection :: {ok, Peer} | {Peer, NewState},
      Peer :: peer() | false.

-doc """
Invoked to signal that a peer connection on the local Erlang node is no longer
available following a previous call to `c:peer_up/3`. In particular, that the
RFC 3539 watchdog state machine for the connection has left state `OKAY` and the
peer will no longer be a candidate in `c:pick_peer/4` callbacks.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback peer_down(SvcName, Peer, State) -> NewState when
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      State :: state(),
      NewState :: state().

-doc """
Invoked to signal the availability of a peer connection on the local Erlang
node. In particular, capabilities exchange with the peer has indicated support
for the application in question, the RFC 3539 watchdog state machine for the
connection has reached state `OKAY` and Diameter messages can be both sent and
received.

> #### Note {: .info }
>
> A watchdog state machine can reach state `OKAY` from state `SUSPECT` without a
> new capabilities exchange taking place. A new transport connection (and
> capabilities exchange) results in a new peer_ref().

> #### Note {: .info }
>
> There is no requirement that a callback return before incoming requests are
> received: `c:handle_request/3` callbacks must be handled independently of
> `c:peer_up/3` and `c:peer_down/3`.
""".
-doc(#{since => <<"OTP R14B03">>}).
-callback peer_up(SvcName, Peer, State) -> NewState when
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      State :: state(),
      NewState :: state().

%% start/2

-doc false.
start(_Type, _Args) ->
    diameter_sup:start_link().

%% stop/1

-doc false.
stop(_) ->
    ok.
