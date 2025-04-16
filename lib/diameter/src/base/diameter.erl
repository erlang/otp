%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

-module(diameter).
-moduledoc """
Main API of the diameter application.

This module provides the interface with which a user can implement a Diameter
node that sends and receives messages using the Diameter protocol as defined in
RFC 6733.

Basic usage consists of creating a representation of a locally implemented
Diameter node and its capabilities with `start_service/2`, adding transport
capability using `add_transport/2` and sending Diameter requests and receiving
Diameter answers with `call/4`. Incoming Diameter requests are communicated as
callbacks to a `m:diameter_app` callback modules as specified in the service
configuration.

Beware the difference between _diameter_ (not capitalized) and _Diameter_
(capitalized). The former refers to the Erlang application named diameter whose
main api is defined here, the latter to Diameter protocol in the sense of
RFC 6733.

The diameter application must be started before calling most functions in this
module.

## DATA TYPES

- **`Address()`**

- **`DiameterIdentity()`**

- **`Grouped()`**

- **`OctetString()`**

- **`Time()`**

- **`Unsigned32()`**

- **`UTF8String()`** - Types corresponding to RFC 6733 AVP Data Formats. Defined
  in [diameter_dict(4)](diameter_dict.md#DATA_TYPES).

- **`elapsed_time()`** - Elapsed time since a given time.

  [](){: #application_alias }

- **`application_alias() = term()`** - Name identifying a Diameter application
  in service configuration. Passed to `call/4` when sending requests defined by
  the application.

  [](){: #application_module }

- **`application_module() = Mod | [Mod | ExtraArgs] | #diameter_callback{}`**

  ```text
  Mod = atom()
  ExtraArgs = list()
  ```

  Module implementing the callback interface defined in `m:diameter_app`, along
  with any extra arguments to be appended to those documented. Note that extra
  arguments specific to an outgoing request can be specified to `call/4`, in
  which case those are appended to any module-specific extra arguments.

  Specifying a `#diameter_callback{}` record allows individual functions to be
  configured in place of the usual `m:diameter_app` callbacks. See
  `diameter_callback.erl` for details.

  [](){: #application_opt }

- **`application_opt()`** - Options defining a Diameter application. Has one of
  the following types.

  - **`{alias, `[`application_alias()`](`m:diameter#application_alias`)`}`** -
    Unique identifier for the application in the scope of the service. Defaults
    to the value of the `dictionary` option.

  - **`{dictionary, atom()}`** - Name of an encode/decode module for the
    Diameter messages defined by the application. These modules are generated
    from files whose format is documented in
    [diameter_dict(4)](diameter_dict.md).

  - **`{module, `[`application_module()`](`m:diameter#application_module`)`}`** -
    Callback module in which messages of the Diameter application are handled.
    See `m:diameter_app` for the required interface and semantics.

  - **`{state, term()}`** - Initial callback state. The prevailing state is
    passed to some `m:diameter_app` callbacks, which can then return a new
    state. Defaults to the value of the `alias` option.

  - **`{call_mutates_state, true|false}`** - Whether or not the
    [pick_peer/4](`c:diameter_app:pick_peer/4`) application callback can modify
    the application state. Defaults to `false`.

    > #### Warning {: .warning }
    >
    > [pick_peer/4](`c:diameter_app:pick_peer/4`) callbacks are serialized when
    > this option is `true`, which is a potential performance bottleneck. A
    > simple Diameter client may suffer no ill effects from using mutable state
    > but a server or agent that responds to incoming request should probably
    > avoid it.

  - **`{answer_errors, callback|report|discard}`** - Manner in which incoming
    answer messages containing decode errors are handled.

    If `callback` then errors result in a
    [handle_answer/4](`c:diameter_app:handle_answer/4`) callback in the same
    fashion as for [handle_request/3](`c:diameter_app:handle_request/3`), with
    errors communicated in the `errors` field of the `#diameter_packet{}` passed
    to the callback. If `report` then an answer containing errors is discarded
    without a callback and a warning report is written to the log. If `discard`
    then an answer containing errors is silently discarded without a callback.
    In both the `report` and `discard` cases the return value for the `call/4`
    invocation in question is as if a callback had taken place and returned
    `{error, failure}`.

    Defaults to `discard`.

  - **`{request_errors, answer_3xxx|answer|callback}`** - Manner in which
    incoming requests are handled when an error other than 3007
    (DIAMETER_APPLICATION_UNSUPPORTED, which cannot be associated with an
    application callback module), is detected.

    If `answer_3xxx` then requests are answered without a
    [handle_request/3](`c:diameter_app:handle_request/3`) callback taking place.
    If `answer` then even 5xxx errors are answered without a callback unless the
    connection in question has configured the RFC 3588 common dictionary as
    noted below. If `callback` then a
    [handle_request/3](`c:diameter_app:handle_request/3`) callback always takes
    place and its return value determines the answer sent to the peer, if any.

    Defaults to `answer_3xxx`.

    > #### Note {: .info }
    >
    > Answers sent by diameter set the E-bit in the Diameter Header. Since RFC
    > 3588 allows only 3xxx result codes in an `answer-message`, `answer` has
    > the same semantics as `answer_3xxx` when the transport in question has
    > been configured with `diameter_gen_base_rfc3588` as its common dictionary.
    > Since RFC 6733 allows both 3xxx and 5xxx result codes in an
    > `answer-message`, a transport with `diameter_gen_base_rfc6733` as its
    > common dictionary does distinguish between `answer_3xxx` and `answer`.

  [](){: #call_opt }

- **`call_opt()`** - Options available to `call/4` when sending an outgoing
  Diameter request. Has one of the following types.

  - **`{extra, list()}`** - Extra arguments to append to callbacks to the
    callback module in question. These are appended to any extra arguments
    configured on the callback itself. Multiple options append to the argument
    list.

  - **`{filter, `[`peer_filter()`](`m:diameter#peer_filter`)`}`** - Filter to
    apply to the list of available peers before passing it to the
    [pick_peer/4](`c:diameter_app:pick_peer/4`) callback for the application in
    question. Multiple options are equivalent a single `all` filter on the
    corresponding list of filters. Defaults to `none`.

  - **`{peer, `[`diameter_app:peer_ref()`](`t:diameter_app:peer_ref/0`)`}`** -
    Peer to which the request in question can be sent, preempting the selection
    of peers having advertised support for the Diameter application in question.
    Multiple options can be specified, and their order is respected in the
    candidate lists passed to a subsequent
    [pick_peer/4](`c:diameter_app:pick_peer/4`) callback.

  - **`{timeout, `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`** - Number of
    milliseconds after which the request should timeout. Defaults to 5000.

  - **`detach`** - Cause `call/4` to return `ok` as soon as the request in
    question has been encoded, instead of waiting for and returning the result
    from a subsequent [handle_answer/4](`c:diameter_app:handle_answer/4`) or
    [handle_error/4](`c:diameter_app:handle_error/4`) callback.

  An invalid option will cause `call/4` to fail.

  [](){: #capability }

- **`capability()`** - AVP values sent in outgoing CER or CEA messages during
  capabilities exchange. Can be configured both on a service and a transport,
  values on the latter taking precedence. Has one of the following types.

  - **`{'Origin-Host', `[`DiameterIdentity()`](diameter_dict.md#DATA_TYPES)`}`**

  - **`{'Origin-Realm', `[`DiameterIdentity()`](diameter_dict.md#DATA_TYPES)`}`**

  - **`{'Host-IP-Address', [`[`Address()`](diameter_dict.md#DATA_TYPES)`]}`** -
    An address list is available to the start function of a
    [transport module](`m:diameter_transport`), which can return a new list for
    use in the subsequent CER or CEA. Host-IP-Address need not be specified if
    the transport module in question communicates an address list as described
    in `m:diameter_transport`

  - **`{'Vendor-Id', `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`**

  - **`{'Product-Name', `[`UTF8String()`](diameter_dict.md#DATA_TYPES)`}`**

  - **`{'Origin-State-Id', `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`** -
    Origin-State-Id is optional but, if configured, will be included in outgoing
    CER/CEA and DWR/DWA messages. Setting a value of `0` (zero) is equivalent to
    not setting a value, as documented in RFC 6733. The function
    `origin_state_id/0` can be used as to retrieve a value that is computed when
    the diameter application is started.

  - **`{'Supported-Vendor-Id', [`[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`]}`**

  - **`{'Auth-Application-Id', [`[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`]}`**

  - **`{'Inband-Security-Id', [`[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`]}`** -
    Inband-Security-Id defaults to the empty list, which is equivalent to a list
    containing only 0 (NO_INBAND_SECURITY). If 1 (TLS) is specified then TLS is
    selected if the CER/CEA received from the peer offers it.

  - **`{'Acct-Application-Id', [`[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`]}`**

  - **`{'Vendor-Specific-Application-Id', [`[`Grouped()`](diameter_dict.md#DATA_TYPES)`]}`**

  - **`{'Firmware-Revision', `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`**

  Note that each tuple communicates one or more AVP values. It is an error to
  specify duplicate tuples.

  [](){: #eval }

- **`eval() = {M,F,A} | fun() | [eval() | A]`** - An expression that can be
  evaluated as a function in the following sense.

  ```erlang
  eval([{M,F,A} | T]) ->
      apply(M, F, T ++ A);
  eval([[F|A] | T]) ->
      eval([F | T ++ A]);
  eval([F|A]) ->
      apply(F, A);
  eval(F) ->
      eval([F]).
  ```

  Applying an [`eval()`](`m:diameter#eval`) `E` to an argument list `A` is meant
  in the sense of `eval([E|A])`.

  > #### Warning {: .warning }
  >
  > Beware of using fun expressions of the form `fun Name/Arity` in situations
  > in which the fun is not short-lived and code is to be upgraded at runtime
  > since any processes retaining such a fun will have a reference to old code.
  > In particular, such a value is typically inappropriate in configuration
  > passed to `start_service/2` or `add_transport/2`.

  [](){: #peer_filter }

- **`peer_filter() = term()`** - Filter passed to `call/4` in order to select
  candidate peers for a [pick_peer/4](`c:diameter_app:pick_peer/4`) callback.
  Has one of the following types.

  - **`none`** - Matches any peer. This is a convenience that provides a filter
    equivalent to no filter.

  - **`host`** - Matches only those peers whose Origin-Host has the same value
    as Destination-Host in the outgoing request in question, or any peer if the
    request does not contain a Destination-Host AVP.

  - **`realm`** - Matches only those peers whose Origin-Realm has the same value
    as Destination-Realm in the outgoing request in question, or any peer if the
    request does not contain a Destination-Realm AVP.

  - **`{host, any|`[`DiameterIdentity()`](diameter_dict.md#DATA_TYPES)`}`** -
    Matches only those peers whose Origin-Host has the specified value, or all
    peers if the atom `any`.

  - **`{realm, any|`[`DiameterIdentity()`](diameter_dict.md#DATA_TYPES)`}`** -
    Matches only those peers whose Origin-Realm has the specified value, or all
    peers if the atom `any`.

  - **`{eval, `[`eval()`](`m:diameter#eval`)`}`** - Matches only those peers for
    which the specified [`eval()`](`m:diameter#eval`) returns `true` when
    applied to the connection's `diameter_caps` record. Any other return value
    or exception is equivalent to `false`.

  - **`{neg, `[`peer_filter()`](`m:diameter#peer_filter`)`}`** - Matches only
    those peers not matched by the specified filter.

  - **`{all, [`[`peer_filter()`](`m:diameter#peer_filter`)`]}`** - Matches only
    those peers matched by each filter in the specified list.

  - **`{any, [`[`peer_filter()`](`m:diameter#peer_filter`)`]}`** - Matches only
    those peers matched by at least one filter in the specified list. The
    resulting list will be in match order, peers matching the first filter of
    the list sorting before those matched by the second, and so on.

  - **`{first, [`[`peer_filter()`](`m:diameter#peer_filter`)`]}`** - Like `any`,
    but stops at the first filter for which there are matches, which can be much
    more efficient when there are many peers. For example, the following filter
    causes only peers best matching both the host and realm filters to be
    presented.

    ```text
    {first, [{all, [host, realm]}, realm]}
    ```

  An invalid filter is equivalent to `{any,[]}`, a filter that matches no peer.

  > #### Note {: .info }
  >
  > The `host` and `realm` filters cause the Destination-Host and
  > Destination-Realm AVPs to be extracted from the outgoing request, assuming
  > it to be a record- or list-valued
  > [`diameter_codec:message()`](`m:diameter_codec#message`), and assuming at
  > most one of each AVP. If this is not the case then the
  > `{host|realm, `[`DiameterIdentity()`](diameter_dict.md#DATA_TYPES)`}`
  > filters must be used to achieve the desired result. An empty
  > [`DiameterIdentity()`](diameter_dict.md#DATA_TYPES) (which should not be
  > typical) matches all hosts/realms for the purposes of filtering.

  > #### Warning {: .warning }
  >
  > A `host` filter is not typically desirable when setting Destination-Host
  > since it will remove peer agents from the candidates list.

  [](){: #service_event }

- **`service_event() = #diameter_event{service = `[`service_name()`](`m:diameter#service_name`)`, info = `[`service_event_info()`](`m:diameter#service_event_info`)`}`** -
  An event message sent to processes that have subscribed to these using
  `subscribe/1`.

  [](){: #service_event_info }

- **`service_event_info() = term()`** - The `info` field of a
  [service_event()](`m:diameter#service_event`) record. Can have one of the
  following types.

  - **`start`**

  - **`stop`** - The service is being started or stopped. No event precedes a
    `start` event. No event follows a `stop` event, and this event implies the
    termination of all transport processes.

  - **`{up, Ref, Peer, Config, Pkt}`**

  - **`{up, Ref, Peer, Config}`**

  - **`{down, Ref, Peer, Config}`**

    ```c
    Ref    = transport_ref()
    Peer   = diameter_app:peer()
    Config = {connect|listen, [transport_opt()]}
    Pkt    = #diameter_packet{}
    ```

    The RFC 3539 watchdog state machine has transitioned into (`up`) or out of
    (`down`) the OKAY state. If a `#diameter_packet{}` is present in an `up`
    event then there has been a capabilities exchange on a newly established
    transport connection and the record contains the received CER or CEA.

    Note that a single `up` or `down` event for a given peer corresponds to
    multiple [peer_up/3](`c:diameter_app:peer_up/3`) or
    [peer_down/3](`c:diameter_app:peer_down/3`) callbacks, one for each of the
    Diameter applications negotiated during capabilities exchange. That is, the
    event communicates connectivity with the peer as a whole while the callbacks
    communicate connectivity with respect to individual Diameter applications.

  - **`{reconnect, Ref, Opts}`**

    ```text
    Ref  = transport_ref()
    Opts = [transport_opt()]
    ```

    A connecting transport is attempting to establish/reestablish a transport
    connection with a peer following [connect_timer](`m:diameter#connect_timer`)
    or [watchdog_timer](`m:diameter#watchdog_timer`) expiry.

  - **`{closed, Ref, Reason, Config}`**

    ```text
    Ref = transport_ref()
    Config = {connect|listen, [transport_opt()]}
    ```

    Capabilities exchange has failed. `Reason` can have one of the following
    types.

    - **`{'CER', Result, Caps, Pkt}`**

      ```text
      Result = ResultCode | {capabilities_cb, CB, ResultCode|discard}
      Caps = #diameter_caps{}
      Pkt  = #diameter_packet{}
      ResultCode = integer()
      CB = eval()
      ```

      An incoming CER has been answered with the indicated result code, or
      discarded. `Caps` contains pairs of values, for the local node and remote
      peer respectively. `Pkt` contains the CER in question. In the case of
      rejection by a capabilities callback, the tuple contains the rejecting
      callback.

    - **`{'CER', Caps, {ResultCode, Pkt}}`**

      ```text
      ResultCode = integer()
      Caps = #diameter_caps{}
      Pkt  = #diameter_packet{}
      ```

      An incoming CER contained errors and has been answered with the indicated
      result code. `Caps` contains values for the local node only. `Pkt`
      contains the CER in question.

    - **`{'CER', timeout}`** - An expected CER was not received within
      [capx_timeout](`m:diameter#capx_timeout`) of connection establishment.

    - **`{'CEA', Result, Caps, Pkt}`**

      ```erlang
      Result = ResultCode | atom() | {capabilities_cb, CB, ResultCode|discard}
      Caps = #diameter_caps{}
      Pkt  = #diameter_packet{}
      ResultCode = integer()
      ```

      An incoming CEA has been rejected for the indicated reason. An
      integer-valued `Result` indicates the result code sent by the peer. `Caps`
      contains pairs of values for the local node and remote peer. `Pkt`
      contains the CEA in question. In the case of rejection by a capabilities
      callback, the tuple contains the rejecting callback.

    - **`{'CEA', Caps, Pkt}`**

      ```text
      Caps = #diameter_caps{}
      Pkt  = #diameter_packet{}
      ```

      An incoming CEA contained errors and has been rejected. `Caps` contains
      only values for the local node. `Pkt` contains the CEA in question.

    - **`{'CEA', timeout}`** - An expected CEA was not received within
      [capx_timeout](`m:diameter#capx_timeout`) of connection establishment.

  - **`{watchdog, Ref, PeerRef, {From, To}, Config}`**

    ```c
    Ref = transport_ref()
    PeerRef = diameter_app:peer_ref()
    From, To = initial | okay | suspect | down | reopen
    Config = {connect|listen, [transport_opt()]}
    ```

    An RFC 3539 watchdog state machine has changed state.

  - **`t:any/0`** - For forward compatibility, a subscriber should be prepared
    to receive info fields of forms other than the above.

  [](){: #service_name }

- **`service_name() = term()`** - Name of a service as passed to
  `start_service/2` and with which the service is identified. There can be at
  most one service with a given name on a given node. Note that
  `erlang:make_ref/0` can be used to generate a service name that is somewhat
  unique.

  [](){: #service_opt }

- **`service_opt()`** - Option passed to `start_service/2`. Can be any
  [`capability()`](`m:diameter#capability`) as well as the following.

  - **`{application, [`[`application_opt()`](`m:diameter#application_opt`)`]}`** -
    A Diameter application supported by the service.

    A service must configure one tuple for each Diameter application it intends
    to support. For an outgoing request, the relevant
    [`application_alias()`](`m:diameter#application_alias`) is passed to
    `call/4`, while for an incoming request the application identifier in the
    message header determines the application, the identifier being specified in
    the application's [dictionary](diameter_dict.md) file.

    > #### Warning {: .warning }
    >
    > The capabilities advertised by a node must match its configured
    > applications. In particular, `application` configuration must be matched
    > by corresponding [capability()](`m:diameter#capability`) configuration, of
    > \*-Application-Id AVPs in particular.

  - **`{decode_format, record | list | map | none}`{: #decode_format }** - The
    format of decoded messages and grouped AVPs in the `msg` field of
    diameter_packet records and `value` field of diameter_avp records
    respectively. If `record` then a record whose definition is generated from
    the dictionary file in question. If `list` or `map` then a `[Name | Avps]`
    pair where `Avps` is a list of AVP name/values pairs or a map keyed on AVP
    names respectively. If `none` then the atom-value message name, or
    `undefined` for a Grouped AVP. See also
    [diameter_codec:message()](`m:diameter_codec#message`).

    Defaults to `record`.

    > #### Note {: .info }
    >
    > AVPs are decoded into a list of diameter_avp records in `avps` field of
    > diameter_packet records independently of `decode_format`.

  - **`{restrict_connections, false | node | nodes | [node()] | eval()}`** - The
    degree to which the service allows multiple transport connections to the
    same peer, as identified by its Origin-Host at capabilities exchange.

    If `[node()]` then a connection is rejected if another already exists on any
    of the specified nodes. Types `false`, `node`, `nodes` and
    [eval()](`m:diameter#eval`) are equivalent to `[]`, `[node()]`,
    `[node()|nodes()]` and the evaluated value respectively, evaluation of each
    expression taking place whenever a new connection is to be established. Note
    that `false` allows an unlimited number of connections to be established
    with the same peer.

    Multiple connections are independent and governed by their own peer and
    watchdog state machines.

    Defaults to `nodes`.

  - **`{sequence, {H,N} | `[`eval()`](`m:diameter#eval`)`}`** - A constant value
    `H` for the topmost `32-N` bits of of 32-bit End-to-End and Hop-by-Hop
    Identifiers generated by the service, either explicitly or as a return value
    of a function to be evaluated at `start_service/2`. In particular, an
    identifier `Id` is mapped to a new identifier as follows.

    ```text
    (H bsl N) bor (Id band ((1 bsl N) - 1))
    ```

    Note that RFC 6733 requires that End-to-End Identifiers remain unique for a
    period of at least 4 minutes and that this and the call rate places a lower
    bound on appropriate values of `N`: at a rate of `R` requests per second, an
    `N`\-bit counter traverses all of its values in `(1 bsl N) div (R*60)`
    minutes, so the bound is `4*R*60 =< 1 bsl N`.

    `N` must lie in the range `0..32` and `H` must be a non-negative integer
    less than `1 bsl (32-N)`.

    Defaults to `{0,32}`.

    > #### Warning {: .warning }
    >
    > Multiple Erlang nodes implementing the same Diameter node should be
    > configured with different sequence masks to ensure that each node uses a
    > unique range of End-to-End and Hop-by-Hop Identifiers for outgoing
    > requests.

  - **`{share_peers, boolean() | [node()] | eval()}`** - Nodes to which peer
    connections established on the local Erlang node are communicated. Shared
    peers become available in the remote candidates list passed to
    [pick_peer/4](`c:diameter_app:pick_peer/4`) callbacks on remote nodes whose
    services are configured to use them: see `use_shared_peers` below.

    If `false` then peers are not shared. If `[node()]` then peers are shared
    with the specified list of nodes. If `eval()` then peers are shared with the
    nodes returned by the specified function, evaluated whenever a peer
    connection becomes available or a remote service requests information about
    local connections. The value `true` is equivalent to `fun ``erlang:nodes/0`.
    The value `node/0` in a list is ignored, so a collection of services can all
    be configured to share with the same list of nodes.

    Defaults to `false`.

    > #### Note {: .info }
    >
    > Peers are only shared with services of the same name for the purpose of
    > sending outgoing requests. Since the value of the
    > [application_opt()](`m:diameter#application_opt`) `alias`, passed to
    > `call/4`, is the handle for identifying a peer as a suitable candidate,
    > services that share peers must use the same aliases to identify their
    > supported applications. They should typically also configure identical
    > [capabilities()](`m:diameter#capabilities`), since by sharing peer
    > connections they are distributing the implementation of a single Diameter
    > node across multiple Erlang nodes.

  - **`{strict_arities, boolean() | encode | decode}`{: #strict_arities }** -
    Whether or not to require that the number of AVPs in a message or grouped
    AVP agree with those specified in the dictionary in question when passing
    messages to `m:diameter_app` callbacks. If `true` then mismatches in an
    outgoing messages cause message encoding to fail, while mismatches in an
    incoming message are reported as 5005/5009 errors in the errors field of the
    diameter_packet record passed to
    [handle_request/3](`c:diameter_app:handle_request/3`) or
    [handle_answer/4](`c:diameter_app:handle_answer/4`) callbacks. If `false`
    then neither error is enforced/detected. If `encode` or `decode` then errors
    are only enforced/detected on outgoing or incoming messages respectively.

    Defaults to `true`.

    > #### Note {: .info }
    >
    > Disabling arity checks affects the form of messages at encode/decode. In
    > particular, decoded AVPs are represented as lists of values, regardless of
    > the AVP's arity (ie. expected number in the message/AVP grammar in
    > question), and values are expected to be supplied as lists at encode. This
    > differs from the historic decode behaviour of representing AVPs of arity 1
    > as bare values, not wrapped in a list.

  - **`{string_decode, boolean()}`{: #string_decode }** - Whether or not to
    decode AVPs of type [OctetString()](diameter_dict.md#DATA_TYPES) and its
    derived types [DiameterIdentity()](diameter_dict.md#DATA_TYPES),
    [DiameterURI()](diameter_dict.md#DATA_TYPES),
    [IPFilterRule()](diameter_dict.md#DATA_TYPES),
    [QoSFilterRule()](diameter_dict.md#DATA_TYPES), and
    [UTF8String()](diameter_dict.md#DATA_TYPES). If `true` then AVPs of these
    types are decoded to string(). If `false` then values are retained as
    binary().

    Defaults to `true`.

    > #### Warning {: .warning }
    >
    > This option should be set to `false` since a sufficiently malicious peer
    > can otherwise cause large amounts of memory to be consumed when decoded
    > Diameter messages are passed between processes. The default value is for
    > backwards compatibility.

  - **`{traffic_counters, boolean()}`{: #traffic_counters }** - Whether or not
    to count application-specific messages; those for which `m:diameter_app`
    callbacks take place. If false then only messages handled by diameter itself
    are counted: CER/CEA, DWR/DWA, DPR/DPA.

    Defaults to `true`.

    > #### Note {: .info }
    >
    > Disabling counters is a performance improvement, but means that the
    > omitted counters are not returned by `service_info/2`.

  - **`{use_shared_peers, boolean() | [node()] | eval()}`** - Nodes from which
    communicated peers are made available in the remote candidates list of
    [pick_peer/4](`c:diameter_app:pick_peer/4`) callbacks.

    If `false` then remote peers are not used. If `[node()]` then only peers
    from the specified list of nodes are used. If `eval()` then only peers
    returned by the specified function are used, evaluated whenever a remote
    service communicates information about an available peer connection. The
    value `true` is equivalent to `fun ``erlang:nodes/0`. The value `node/0` in
    a list is ignored.

    Defaults to `false`.

    > #### Note {: .info }
    >
    > A service that does not use shared peers will always pass the empty list
    > as the second argument of [pick_peer/4](`c:diameter_app:pick_peer/4`)
    > callbacks.

    > #### Warning {: .warning }
    >
    > Sending a request over a peer connection on a remote node is less
    > efficient than sending it over a local connection. It may be preferable to
    > make use of the [service_opt()](`m:diameter#service_opt`)
    > `restrict_connections` and maintain a dedicated connection on each node
    > from which requests are sent.

  - **[`transport_opt()`](`m:diameter#transport_opt`)** - Any transport option
    except `applications`, `capabilities`, `transport_config`, and
    `transport_module`. Used as defaults for transport configuration, values
    passed to `add_transport/2` overriding values configured on the service.

  [](){: #transport_opt }

- **`transport_opt()`** - Option passed to `add_transport/2`. Has one of the
  following types.

  - **`{applications, [`[`application_alias()`](`m:diameter#application_alias`)`]}`{:
    #applications }** - Diameter applications to which the transport should be
    restricted. Defaults to all applications configured on the service in
    question. Applications not configured on the service in question are
    ignored.

    > #### Warning {: .warning }
    >
    > The capabilities advertised by a node must match its configured
    > applications. In particular, setting `applications` on a transport
    > typically implies having to set matching \*-Application-Id AVPs in a
    > [capabilities()](`m:diameter#capabilities`) tuple.

  - **`{avp_dictionaries, [module()]}`{: #avp_dictionaries }** - A list of
    alternate dictionary modules with which to encode/decode AVPs that are not
    defined by the dictionary of the application in question. At decode, such
    AVPs are represented as diameter_avp records in the `'AVP'` field of a
    decoded message or Grouped AVP, the first alternate that succeeds in
    decoding the AVP setting the record's value field. At encode, values in an
    `'AVP'` list can be passed as AVP name/value 2-tuples, and it is an encode
    error for no alternate to define the AVP of such a tuple.

    Defaults to the empty list.

    > #### Note {: .info }
    >
    > The motivation for alternate dictionaries is RFC 7683, Diameter Overload
    > Indication Conveyance (DOIC), which defines AVPs to be piggybacked onto
    > existing application messages rather than defining an application of its
    > own. The DOIC dictionary is provided by the diameter application, as
    > module `diameter_gen_doic_rfc7683`, but alternate dictionaries can be used
    > to encode/decode any set of AVPs not known to an application dictionary.

  - **`{capabilities, [`[`capability()`](`m:diameter#capability`)`]}`{:
    #capabilities }** - AVPs used to construct outgoing CER/CEA messages. Values
    take precedence over any specified on the service in question.

    Specifying a capability as a transport option may be particularly
    appropriate for Inband-Security-Id, in case TLS is desired over TCP as
    implemented by `m:diameter_tcp`.

  - **`{capabilities_cb, `[`eval()`](`m:diameter#eval`)`}`{: #capabilities_cb
    }** - Callback invoked upon reception of CER/CEA during capabilities
    exchange in order to ask whether or not the connection should be accepted.
    Applied to the [`transport_ref()`](`m:diameter#transport_ref`) and
    `#diameter_caps{}` record of the connection.

    The return value can have one of the following types.

    - **`ok`** - Accept the connection.

    - **`t:integer/0`** - Causes an incoming CER to be answered with the
      specified Result-Code.

    - **`discard`** - Causes an incoming CER to be discarded without CEA being
      sent.

    - **`unknown`** - Equivalent to returning `3010`, DIAMETER_UNKNOWN_PEER.

    Returning anything but `ok` or a 2xxx series result code causes the
    transport connection to be broken. Multiple
    [capabilities_cb](`m:diameter#capabilities_cb`) options can be specified, in
    which case the corresponding callbacks are applied until either all return
    `ok` or one does not.

  - **`{capx_timeout, `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`{:
    #capx_timeout }** - Number of milliseconds after which a transport process
    having an established transport connection will be terminated if the
    expected capabilities exchange message (CER or CEA) is not received from the
    peer. For a connecting transport, the timing of connection attempts is
    governed by [connect_timer](`m:diameter#connect_timer`) or
    [watchdog_timer](`m:diameter#watchdog_timer`) expiry. For a listening
    transport, the peer determines the timing.

    Defaults to 10000.

  - **`{connect_timer, Tc}`{: #connect_timer }**

    ```text
    Tc = Unsigned32()
    ```

    For a connecting transport, the RFC 6733 Tc timer, in milliseconds. This
    timer determines the frequency with which a transport attempts to establish
    an initial connection with its peer following transport configuration. Once
    an initial connection has been established,
    [watchdog_timer](`m:diameter#watchdog_timer`) determines the frequency of
    reconnection attempts, as required by RFC 3539.

    For a listening transport, the timer specifies the time after which a
    previously connected peer will be forgotten: a connection after this time is
    regarded as an initial connection rather than reestablishment, causing the
    RFC 3539 state machine to pass to state OKAY rather than REOPEN. Note that
    these semantics are not governed by the RFC and that a listening transport's
    [connect_timer](`m:diameter#connect_timer`) should be greater than its
    peer's Tw plus jitter.

    Defaults to 30000 for a connecting transport and 60000 for a listening
    transport.

  - **`{disconnect_cb, `[`eval()`](`m:diameter#eval`)`}`{: #disconnect_cb }** -
    Callback invoked prior to terminating the transport process of a transport
    connection having watchdog state `OKAY`. Applied to
    `application|service|transport` and the
    [`transport_ref()`](`m:diameter#transport_ref`) and
    [`diameter_app:peer()`](`t:diameter_app:peer/0`) in question: `application`
    indicates that the diameter application is being stopped, `service` that the
    service in question is being stopped by `stop_service/1`, and `transport`
    that the transport in question is being removed by `remove_transport/2`.

    The return value can have one of the following types.

    - **`{dpr, [option()]}`** - Send Disconnect-Peer-Request to the peer, the
      transport process being terminated following reception of
      Disconnect-Peer-Answer or timeout. An `option()` can be one of the
      following.

      - **`{cause, 0|rebooting|1|busy|2|goaway}`** - Disconnect-Cause to send,
        `REBOOTING`, `BUSY` and `DO_NOT_WANT_TO_TALK_TO_YOU` respectively.
        Defaults to `rebooting` for `Reason=service|application` and `goaway`
        for `Reason=transport`.

      - **`{timeout, `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`** -
        Number of milliseconds after which the transport process is terminated
        if DPA has not been received. Defaults to the value of
        [dpa_timeout](`m:diameter#dpa_timeout`).

    - **`dpr`** - Equivalent to `{dpr, []}`.

    - **`close`** - Terminate the transport process without
      Disconnect-Peer-Request being sent to the peer.

    - **`ignore`** - Equivalent to not having configured the callback.

    Multiple [disconnect_cb](`m:diameter#disconnect_cb`) options can be
    specified, in which case the corresponding callbacks are applied until one
    of them returns a value other than `ignore`. All callbacks returning
    `ignore` is equivalent to not having configured them.

    Defaults to a single callback returning `dpr`.

  - **`{dpa_timeout, `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`{:
    #dpa_timeout }** - Number of milliseconds after which a transport connection
    is terminated following an outgoing DPR if DPA is not received.

    Defaults to 1000.

  - **`{dpr_timeout, `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)`}`{:
    #dpr_timeout }** - Number of milliseconds after which a transport connection
    is terminated following an incoming DPR if the peer does not close the
    connection.

    Defaults to 5000.

  - **`{incoming_maxlen, 0..16777215}`{: #incoming_maxlen }** - Bound on the
    expected size of incoming Diameter messages. Messages larger than the
    specified number of bytes are discarded.

    Defaults to `16777215`, the maximum value of the 24-bit Message Length field
    in a Diameter Header.

  - **`{length_errors, exit|handle|discard}`{: #length_errors }** - How to deal
    with errors in the Message Length field of the Diameter Header in an
    incoming message. An error in this context is that the length is not at
    least 20 bytes (the length of a Header), is not a multiple of 4 (a valid
    length) or is not the length of the message in question, as received over
    the transport interface documented in `m:diameter_transport`.

    If `exit` then the transport process in question exits. If `handle` then the
    message is processed as usual, a resulting
    [handle_request/3](`c:diameter_app:handle_request/3`) or
    [handle_answer/4](`c:diameter_app:handle_answer/4`) callback (if one takes
    place) indicating the `5015` error (DIAMETER_INVALID_MESSAGE_LENGTH). If
    `discard` then the message in question is silently discarded.

    Defaults to `exit`.

    > #### Note {: .info }
    >
    > The default value reflects the fact that a transport module for a
    > stream-oriented transport like TCP may not be able to recover from a
    > message length error since such a transport must use the Message Length
    > header to divide the incoming byte stream into individual Diameter
    > messages. An invalid length leaves it with no reliable way to rediscover
    > message boundaries, which may result in the failure of subsequent
    > messages. See `m:diameter_tcp` for the behaviour of that module.

  - **`{pool_size, pos_integer()}`** - Number of transport processes to start.
    For a listening transport, determines the size of the pool of accepting
    transport processes, a larger number being desirable for processing multiple
    concurrent peer connection attempts. For a connecting transport, determines
    the number of connections to the peer in question that will be attempted to
    be establshed: the [service_opt()](`m:diameter#service_opt`):
    `restrict_connections` should also be configured on the service in question
    to allow multiple connections to the same peer.

  - **`{spawn_opt, [term()] | {M,F,A}}`{: #spawn_opt }** - An options list
    passed to `erlang:spawn_opt/2` to spawn a handler process for an incoming
    Diameter request on the local node, or an MFA that returns the pid of a
    handler process.

    Options `monitor` and `link` are ignored in the list-valued case. An MFA is
    applied with an additional term prepended to its argument list, and should
    return either the pid of the handler process that invokes
    `diameter_traffic:request/1` on the argument in order to process the
    request, or the atom `discard`. The handler process need not be local, and
    diameter need not be started on the remote node, but diameter and relevant
    application callbacks must be on the code path.

    Defaults to the empty list.

  - **`{strict_capx, boolean()]}`{: #strict_capx }** - Whether or not to enforce
    the RFC 6733 requirement that any message before capabilities exchange
    should close the peer connection. If false then unexpected messages are
    discarded.

    Defaults to true. Changing this results in non-standard behaviour, but can
    be useful in case peers are known to be behave badly.

  - **`{strict_mbit, boolean()}`{: #strict_mbit }** - Whether or not to regard
    an AVP setting the M-bit as erroneous when the command grammar in question
    does not explicitly allow the AVP. If `true` then such AVPs are regarded as
    5001 errors, DIAMETER_AVP_UNSUPPORTED. If `false` then the M-bit is ignored
    and policing it becomes the receiver's responsibility.

    Defaults to `true`.

    > #### Warning {: .warning }
    >
    > RFC 6733 is unclear about the semantics of the M-bit. One the one hand,
    > the CCF specification in section 3.2 documents AVP in a command grammar as
    > meaning _any_ arbitrary AVP; on the other hand, 1.3.4 states that AVPs
    > setting the M-bit cannot be added to an existing command: the modified
    > command must instead be placed in a new Diameter application.
    >
    > The reason for the latter is presumably interoperability: allowing
    > arbitrary AVPs setting the M-bit in a command makes its interpretation
    > implementation-dependent, since there's no guarantee that all
    > implementations will understand the same set of arbitrary AVPs in the
    > context of a given command. However, interpreting `AVP` in a command
    > grammar as any AVP, regardless of M-bit, renders 1.3.4 meaningless, since
    > the receiver can simply ignore any AVP it thinks isn't relevant,
    > regardless of the sender's intent.
    >
    > Beware of confusing mandatory in the sense of the M-bit with mandatory in
    > the sense of the command grammar. The former is a semantic requirement:
    > that the receiver understand the semantics of the AVP in the context in
    > question. The latter is a syntactic requirement: whether or not the AVP
    > must occur in the message in question.

  - **`{transport_config, term()}`{: #transport_config }**

  - **`{transport_config, term(), `[`Unsigned32()`](diameter_dict.md#DATA_TYPES)` | infinity}`** -
    Term passed as the third argument to the
    [start/3](`c:diameter_transport:start/3`) function of the relevant
    [transport module](`m:diameter_transport`) in order to start a transport
    process. Defaults to the empty list.

    The 3-tuple form additionally specifies an interval, in milliseconds, after
    which a started transport process should be terminated if it has not yet
    established a connection. For example, the following options on a connecting
    transport request a connection with one peer over SCTP or another (typically
    the same) over TCP.

    ```erlang
    {transport_module, diameter_sctp}
    {transport_config, SctpOpts, 5000}
    {transport_module, diameter_tcp}
    {transport_config, TcpOpts}
    ```

    To listen on both SCTP and TCP, define one transport for each.

  - **`{transport_module, atom()}`{: #transport_module }** - Module implementing
    a transport process as defined in `m:diameter_transport`. Defaults to
    `diameter_tcp`.

    Multiple `transport_module` and
    [transport_config](`m:diameter#transport_config`) options are allowed. The
    order of these is significant in this case (and only in this case), a
    `transport_module` being paired with the first
    [transport_config](`m:diameter#transport_config`) following it in the
    options list, or the default value for trailing modules. Transport starts
    will be attempted with each of the modules in order until one establishes a
    connection within the corresponding timeout (see below) or all fail.

  - **`{watchdog_config, [{okay|suspect, non_neg_integer()}]}`{:
    #watchdog_config }** - Configuration that alters the behaviour of the
    watchdog state machine. On key `okay`, the non-negative number of answered
    DWR messages before transitioning from REOPEN to OKAY. On key `suspect`, the
    number of watchdog timeouts before transitioning from OKAY to SUSPECT when
    DWR is unanswered, or 0 to not make the transition.

    Defaults to `[{okay, 3}, {suspect, 1}]`. Not specifying a key is equivalent
    to specifying the default value for that key.

    > #### Warning {: .warning }
    >
    > The default value is as required by RFC 3539: changing it results in
    > non-standard behaviour that should only be used to simulate misbehaving
    > nodes during test.

  - **`{watchdog_timer, TwInit}`{: #watchdog_timer }**

    ```text
    TwInit = Unsigned32()
           | {M,F,A}
    ```

    The RFC 3539 watchdog timer. An integer value is interpreted as the RFC's
    TwInit in milliseconds, a jitter of ± 2 seconds being added at each rearming
    of the timer to compute the RFC's Tw. An MFA is expected to return the RFC's
    Tw directly, with jitter applied, allowing the jitter calculation to be
    performed by the callback.

    An integer value must be at least 6000 as required by RFC 3539. Defaults
    to 30000.

  Unrecognized options are silently ignored but are returned unmodified by
  `service_info/2` and can be referred to in predicate functions passed to
  `remove_transport/2`.

- **`transport_ref() = reference()`{: #transport_ref }** - Reference returned by
  `add_transport/2` that identifies the configuration.

## SEE ALSO

`m:diameter_app`, `m:diameter_transport`, [diameter_dict(4)](diameter_dict.md)
""".
-moduledoc(#{since => "OTP R14B03"}).

%% Configuration.
-export([start_service/2,
         stop_service/1,
         add_transport/2,
         remove_transport/2,
         which_transports/0,  which_transports/1,
         which_watchdogs/0,   which_watchdogs/1,
         which_connections/0, which_connections/1,
         subscribe/1,
         unsubscribe/1]).

%% Traffic.
-export([session_id/1,
         origin_state_id/0,
         call/3,
         call/4]).

%% Information.
-export([services/0,
         is_service/1,
         peer_info/1,
         peer_find/1,
         service_info/2]).

%% Start/stop the application. In a "real" application this should
%% typically be a consequence of a release file rather than by calling
%% start/stop explicitly.
-export([start/0,
         stop/0]).

-export_type([eval/0,
              evaluable/0,  %% deprecated
              decode_format/0,
              strict_arities/0,
              restriction/0,
              message_length/0,
              remotes/0,
              sequence/0,
              app_alias/0,
              service_name/0,
              capability/0,
              peer_filter/0,
              peer_ref/0,
              service_opt/0,
              application_opt/0,
              app_module/0,
              transport_ref/0,
              transport_opt/0,
              transport_pred/0,
              call_opt/0,
              elapsed_time/0]).

-export_type(['OctetString'/0,
              'Integer32'/0,
              'Integer64'/0,
              'Unsigned32'/0,
              'Unsigned64'/0,
              'Float32'/0,
              'Float64'/0,
              'Grouped'/0,
              'Address'/0,
              'Time'/0,
              'UTF8String'/0,
              'DiameterIdentity'/0,
              'DiameterURI'/0,
              'Enumerated'/0,
              'IPFilterRule'/0,
              'QoSFilterRule'/0]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").


%% ---------------------------------------------------------------------------
%% start/0
%% ---------------------------------------------------------------------------

-doc """
Start the diameter application.

The diameter application must be started before starting a service. In a
production system this is typically accomplished by a boot file, not by calling
`start/0` explicitly.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec start() -> ok | {error, Reason} when
      Reason :: term().

start() ->
    application:start(?APPLICATION).


%% ---------------------------------------------------------------------------
%% stop/0
%% ---------------------------------------------------------------------------

-doc """
Stop the diameter application.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec stop() -> ok | {error, Reason} when
      Reason :: term().

stop() ->
    application:stop(?APPLICATION).


%% ---------------------------------------------------------------------------
%% start_service/2
%% ---------------------------------------------------------------------------

-doc """
Start a diameter service.

A service defines a locally-implemented Diameter node, specifying the
capabilities to be advertised during capabilities exchange. Transports are added
to a service using `add_transport/2`.

> #### Note {: .info }
>
> A transport can both override its service's capabilities and restrict its
> supported Diameter applications so "service = Diameter node as identified by
> Origin-Host" is not necessarily the case.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec start_service(SvcName, Opts) -> ok | {error, Reason} when
      SvcName :: service_name(),
      Opts    :: [service_opt()],
      Reason  :: term().

start_service(SvcName, Opts)
  when is_list(Opts) ->
    diameter_config:start_service(SvcName, Opts).


%% ---------------------------------------------------------------------------
%% stop_service/1
%% ---------------------------------------------------------------------------

-doc """
Stop a diameter service.

Stopping a service causes all associated transport connections to be broken. A
DPR message will be sent as in the case of `remove_transport/2`.

> #### Note {: .info }
>
> Stopping a service does not remove any associated transports:
> `remove_transport/2` must be called to remove transport configuration.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec stop_service(SvcName) -> ok | {error, Reason} when
      SvcName :: service_name(),
      Reason  :: term().

%% To handle possible race conditions we check whois and then wait...
%% This should be simple, but just in case the function is called
%% when there is no service actually running...
stop_service(SvcName) ->
    case diameter_service:whois(SvcName) of
        undefined ->
            %% Nothing, so we just call stop to perform possible cleanup...
            diameter_config:stop_service(SvcName);
        _ ->
            %% Note that the service may die/be killed just after we checked...
            subscribe(SvcName),
            Result = do_stop_service(SvcName),
            unsubscribe(SvcName),
            Result
    end.

do_stop_service(SvcName) ->
    ok = diameter_config:stop_service(SvcName),
    %% Now wait for the stop event
    await_service_stop_event(SvcName),
    %% And finally wait for the registry to be "flushed" (ugh!)...
    diameter_service:await_service_cleanup(SvcName).
    
await_service_stop_event(SvcName) ->
    receive
        #diameter_event{service = SvcName,
                        info    = stop} ->
            ok
    after 1000 ->
            case diameter_service:whois(SvcName) of
                undefined ->
                    ok;
                _Pid ->
                    await_service_stop_event(SvcName)
            end
    end.


%% ---------------------------------------------------------------------------
%% is_service/1
%% ---------------------------------------------------------------------------

-doc false.
-spec is_service(service_name())
                -> boolean().

is_service(SvcName) ->
    (undefined =/= diameter_service:whois(SvcName)).



%% ---------------------------------------------------------------------------
%% services/0
%% ---------------------------------------------------------------------------

-doc """
Return the list of started services.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec services() -> [SvcName] when
      SvcName :: service_name().

services() ->
    [Name || {Name, _} <- diameter_service:services()].


%% ---------------------------------------------------------------------------
%% service_info/2
%% ---------------------------------------------------------------------------

-doc """
Return information about a started service. Requesting info for an unknown
service causes `undefined` to be returned. Requesting a list of items causes a
tagged list to be returned.

`Item` can be one of the following.

- **`'Origin-Host'`**

- **`'Origin-Realm'`**

- **`'Vendor-Id'`**

- **`'Product-Name'`**

- **`'Origin-State-Id'`**

- **`'Host-IP-Address'`**

- **`'Supported-Vendor'`**

- **`'Auth-Application-Id'`**

- **`'Inband-Security-Id'`**

- **`'Acct-Application-Id'`**

- **`'Vendor-Specific-Application-Id'`**

- **`'Firmware-Revision'`** - Return a capability value as configured with
  `start_service/2`.

- **`applications`** - Return the list of applications as configured with
  `start_service/2`.

- **`capabilities`** - Return a tagged list of all capabilities values as
  configured with `start_service/2`.

- **`transport`** - Return a list containing one entry for each of the service's
  transport as configured with `add_transport/2`. Each entry is a tagged list
  containing both configuration and information about established peer
  connections. An example return value with for a client service with
  Origin-Host "client.example.com" configured with a single transport connected
  to "server.example.com" might look as follows.

  ```erlang
  [[{ref,#Ref<0.0.0.93>},
    {type,connect},
    {options,[{transport_module,diameter_tcp},
              {transport_config,[{ip,{127,0,0,1}},
                                 {raddr,{127,0,0,1}},
                                 {rport,3868},
                                 {reuseaddr,true}]}]},
    {watchdog,{<0.66.0>,-576460736368485571,okay}},
    {peer,{<0.67.0>,-576460736357885808}},
    {apps,[{0,common}]},
    {caps,[{origin_host,{"client.example.com","server.example.com"}},
           {origin_realm,{"example.com","example.com"}},
           {host_ip_address,{[{127,0,0,1}],[{127,0,0,1}]}},
           {vendor_id,{0,193}},
           {product_name,{"Client","Server"}},
           {origin_state_id,{[],[]}},
           {supported_vendor_id,{[],[]}},
           {auth_application_id,{[0],[0]}},
           {inband_security_id,{[],[0]}},
           {acct_application_id,{[],[]}},
           {vendor_specific_application_id,{[],[]}},
           {firmware_revision,{[],[]}},
           {avp,{[],[]}}]},
    {port,[{owner,<0.69.0>},
           {module,diameter_tcp},
           {socket,{{127,0,0,1},48758}},
           {peer,{{127,0,0,1},3868}},
           {statistics,[{recv_oct,656},
                        {recv_cnt,6},
                        {recv_max,148},
                        {recv_avg,109},
                        {recv_dvi,19},
                        {send_oct,836},
                        {send_cnt,6},
                        {send_max,184},
                        {send_avg,139},
                        {send_pend,0}]}]},
    {statistics,[{{{0,258,0},recv},3},
                 {{{0,258,1},send},3},
                 {{{0,258,0},recv,{'Result-Code',2001}},3},
                 {{{0,257,0},recv},1},
                 {{{0,257,1},send},1},
                 {{{0,257,0},recv,{'Result-Code',2001}},1},
                 {{{0,280,1},recv},2},
                 {{{0,280,0},send},2},
                 {{{0,280,0},send,{'Result-Code',2001}},2}]}]]
  ```

  Here `ref` is a [`transport_ref()`](`m:diameter#transport_ref`) and `options`
  the corresponding [`transport_opt()`](`m:diameter#transport_opt`) list passed
  to `add_transport/2`. The `watchdog` entry shows the state of a connection's
  RFC 3539 watchdog state machine. The `peer` entry identifies the
  [`diameter_app:peer_ref()`](`t:diameter_app:peer_ref/0`) for which there will
  have been [peer_up/3](`c:diameter_app:peer_up/3`) callbacks for the Diameter
  applications identified by the `apps` entry, `common` being the
  [`application_alias()`](`m:diameter#application_alias`). The `caps` entry
  identifies the capabilities sent by the local node and received from the peer
  during capabilities exchange. The `port` entry displays socket-level
  information about the transport connection. The `statistics` entry presents
  Diameter-level counters, an entry like `{{{0,280,1},recv},2}` saying that the
  client has received 2 DWR messages:
  `{0,280,1} = {Application_Id, Command_Code, R_Flag}`.

  Note that `watchdog`, `peer`, `apps`, `caps` and `port` entries depend on
  connectivity with the peer and may not be present. Note also that the
  `statistics` entry presents values accumulated during the lifetime of the
  transport configuration.

  A listening transport presents its information slightly differently since
  there may be multiple accepted connections for the same
  [`transport_ref()`](`m:diameter#transport_ref`). The `transport` info returned
  by a server with a single client connection might look as follows.

  ```erlang
  [[{ref,#Ref<0.0.0.61>},
    {type,listen},
    {options,[{transport_module,diameter_tcp},
              {transport_config,[{reuseaddr,true},
                                 {ip,{127,0,0,1}},
                                 {port,3868}]}]},
    {accept,[[{watchdog,{<0.56.0>,-576460739249514012,okay}},
              {peer,{<0.58.0>,-576460638229179167}},
              {apps,[{0,common}]},
              {caps,[{origin_host,{"server.example.com","client.example.com"}},
                     {origin_realm,{"example.com","example.com"}},
                     {host_ip_address,{[{127,0,0,1}],[{127,0,0,1}]}},
                     {vendor_id,{193,0}},
                     {product_name,{"Server","Client"}},
                     {origin_state_id,{[],[]}},
                     {supported_vendor_id,{[],[]}},
                     {auth_application_id,{[0],[0]}},
                     {inband_security_id,{[],[]}},
                     {acct_application_id,{[],[]}},
                     {vendor_specific_application_id,{[],[]}},
                     {firmware_revision,{[],[]}},
                     {avp,{[],[]}}]},
              {port,[{owner,<0.62.0>},
                     {module,diameter_tcp},
                     {socket,{{127,0,0,1},3868}},
                     {peer,{{127,0,0,1},48758}},
                     {statistics,[{recv_oct,1576},
                                  {recv_cnt,16},
                                  {recv_max,184},
                                  {recv_avg,98},
                                  {recv_dvi,26},
                                  {send_oct,1396},
                                  {send_cnt,16},
                                  {send_max,148},
                                  {send_avg,87},
                                  {send_pend,0}]}]}],
             [{watchdog,{<0.72.0>,-576460638229717546,initial}}]]},
    {statistics,[{{{0,280,0},recv},7},
                 {{{0,280,1},send},7},
                 {{{0,280,0},recv,{'Result-Code',2001}},7},
                 {{{0,258,1},recv},3},
                 {{{0,258,0},send},3},
                 {{{0,258,0},send,{'Result-Code',2001}},3},
                 {{{0,280,1},recv},5},
                 {{{0,280,0},send},5},
                 {{{0,280,0},send,{'Result-Code',2001}},5},
                 {{{0,257,1},recv},1},
                 {{{0,257,0},send},1},
                 {{{0,257,0},send,{'Result-Code',2001}},1}]}]]
  ```

  The information presented here is as in the `connect` case except that the
  client connections are grouped under an `accept` tuple.

  Whether or not the [transport_opt()](`m:diameter#transport_opt`) `pool_size`
  has been configured affects the format of the listing in the case of a
  connecting transport, since a value greater than 1 implies multiple transport
  processes for the same [`transport_ref()`](`m:diameter#transport_ref`), as in
  the listening case. The format in this case is similar to the listening case,
  with a `pool` tuple in place of an `accept` tuple.

- **`connections`** - Return a list containing one entry for every established
  transport connection whose watchdog state machine is not in the `down` state.
  This is a flat view of `transport` info which lists only active connections
  and for which Diameter-level statistics are accumulated only for the lifetime
  of the transport connection. A return value for the server above might look as
  follows.

  ```erlang
  [[{ref,#Ref<0.0.0.61>},
    {type,accept},
    {options,[{transport_module,diameter_tcp},
              {transport_config,[{reuseaddr,true},
                                 {ip,{127,0,0,1}},
                                 {port,3868}]}]},
    {watchdog,{<0.56.0>,-576460739249514012,okay}},
    {peer,{<0.58.0>,-576460638229179167}},
    {apps,[{0,common}]},
    {caps,[{origin_host,{"server.example.com","client.example.com"}},
           {origin_realm,{"example.com","example.com"}},
           {host_ip_address,{[{127,0,0,1}],[{127,0,0,1}]}},
           {vendor_id,{193,0}},
           {product_name,{"Server","Client"}},
           {origin_state_id,{[],[]}},
           {supported_vendor_id,{[],[]}},
           {auth_application_id,{[0],[0]}},
           {inband_security_id,{[],[]}},
           {acct_application_id,{[],[]}},
           {vendor_specific_application_id,{[],[]}},
           {firmware_revision,{[],[]}},
           {avp,{[],[]}}]},
    {port,[{owner,<0.62.0>},
           {module,diameter_tcp},
           {socket,{{127,0,0,1},3868}},
           {peer,{{127,0,0,1},48758}},
           {statistics,[{recv_oct,10124},
                        {recv_cnt,132},
                        {recv_max,184},
                        {recv_avg,76},
                        {recv_dvi,9},
                        {send_oct,10016},
                        {send_cnt,132},
                        {send_max,148},
                        {send_avg,75},
                        {send_pend,0}]}]},
    {statistics,[{{{0,280,0},recv},62},
                 {{{0,280,1},send},62},
                 {{{0,280,0},recv,{'Result-Code',2001}},62},
                 {{{0,258,1},recv},3},
                 {{{0,258,0},send},3},
                 {{{0,258,0},send,{'Result-Code',2001}},3},
                 {{{0,280,1},recv},66},
                 {{{0,280,0},send},66},
                 {{{0,280,0},send,{'Result-Code',2001}},66},
                 {{{0,257,1},recv},1},
                 {{{0,257,0},send},1},
                 {{{0,257,0},send,{'Result-Code',2001}},1}]}]]
  ```

  Note that there may be multiple entries with the same `ref`, in contrast to
  `transport` info.

- **`statistics`** - Return a `{{Counter, Ref}, non_neg_integer()}` list of
  counter values. `Ref` can be either a
  [`transport_ref()`](`m:diameter#transport_ref`) or a
  [`diameter_app:peer_ref()`](`t:diameter_app:peer_ref/0`). Entries for the latter
  are folded into corresponding entries for the former as peer connections go
  down. Entries for both are removed at `remove_transport/2`. The Diameter-level
  statistics returned by `transport` and `connections` info are based upon these
  entries.

- **[`diameter_app:peer_ref()`](`t:diameter_app:peer_ref/0`)** - Return transport
  configuration associated with a single peer, as passed to `add_transport/2`.
  The returned list is empty if the peer is unknown. Otherwise it contains the
  `ref`, `type` and `options` tuples as in `transport` and `connections` info
  above. For example:

  ```erlang
  [{ref,#Ref<0.0.0.61>},
   {type,accept},
   {options,[{transport_module,diameter_tcp},
             {transport_config,[{reuseaddr,true},
                                {ip,{127,0,0,1}},
                                {port,3868}]}]}]
  ```
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec service_info(SvcName, Item | [Item]) -> term() when
      SvcName :: service_name(),
      Item    :: atom() | peer_ref().

service_info(SvcName, Option) ->
    diameter_service:info(SvcName, Option).

%% ---------------------------------------------------------------------------
%% peer_info/1
%% ---------------------------------------------------------------------------

-doc false.
-spec peer_info(peer_ref())
   -> [tuple()].

peer_info(PeerRef) ->
    diameter_service:peer_info(PeerRef).

%% ---------------------------------------------------------------------------
%% peer_find/1
%% ---------------------------------------------------------------------------

-doc false.
-spec peer_find(peer_ref() | pid())
   -> {peer_ref(), pid()}
    | false.

peer_find(Pid) ->
    diameter_peer_fsm:find(Pid).

%% ---------------------------------------------------------------------------
%% add_transport/3
%% ---------------------------------------------------------------------------

-doc """
Add transport capability to a service.

The service will start transport processes as required in order to establish a
connection with the peer, either by connecting to the peer (`connect`) or by
accepting incoming connection requests (`listen`). A connecting transport
establishes transport connections with at most one peer, an listening transport
potentially with many.

The diameter application takes responsibility for exchanging CER/CEA with the
peer. Upon successful completion of capabilities exchange the service calls each
relevant application module's [peer_up/3](`c:diameter_app:peer_up/3`) callback
after which the caller can exchange Diameter messages with the peer over the
transport. In addition to CER/CEA, the service takes responsibility for the
handling of DWR/DWA and required by RFC 3539, as well as for DPR/DPA.

The returned reference uniquely identifies the transport within the scope of the
service. Note that the function returns before a transport connection has been
established.

> #### Note {: .info }
>
> It is not an error to add a transport to a service that has not yet been
> configured: a service can be started after configuring its transports.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec add_transport(SvcName, Transport)
   -> {ok, TRef} | {error, Reason} when
      SvcName   :: service_name(),
      Transport :: {T, Opts},
      T         :: listen | connect,
      Opts      :: [transport_opt()],
      TRef      :: transport_ref(),
      Reason    :: term().

add_transport(SvcName, {T, Opts} = Cfg)
  when is_list(Opts), (T == connect orelse T == listen) ->
    diameter_config:add_transport(SvcName, Cfg).

%% ---------------------------------------------------------------------------
%% remove_transport/2
%% ---------------------------------------------------------------------------

-doc """
Remove previously added transports.

`Pred` determines which transports to remove. An arity-3-valued `Pred` removes
all transports for which `Pred(Ref, Type, Opts)` returns `true`, where `Type`
and `Opts` are as passed to `add_transport/2` and `Ref` is as returned by it.
The remaining forms are equivalent to an arity-3 fun as follows.

```erlang
Pred = fun(transport_ref(), list()):  fun(Ref, _, Opts) -> Pred(Ref, Opts) end
Pred = fun(list()):                   fun(_, _, Opts) -> Pred(Opts) end
Pred = transport_ref():               fun(Ref, _, _)  -> Pred == Ref end
Pred = list():                        fun(_, _, Opts) -> [] == Pred -- Opts end
Pred = true:                          fun(_, _, _) -> true end
Pred = false:                         fun(_, _, _) -> false end
Pred = {M,F,A}:  fun(Ref, Type, Opts) -> apply(M, F, [Ref, Type, Opts | A]) end
```

Removing a transport causes the corresponding transport processes to be
terminated. Whether or not a DPR message is sent to a peer is controlled by
value of [disconnect_cb](`m:diameter#disconnect_cb`) configured on the
transport.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec remove_transport(SvcName, Pred)
   -> ok | {error, Reason} when
      SvcName :: service_name(),
      Pred    :: transport_pred(),
      Reason  :: term().

remove_transport(SvcName, Pred) ->
    diameter_config:remove_transport(SvcName, Pred).


%% ---------------------------------------------------------------------------
%% which_transport/0, which_transport/1
%% ---------------------------------------------------------------------------
-doc """
Return a list of _all_ transports.
""".
-doc(#{since => <<"OTP 26.2.4">>}).
-spec which_transports() -> [#{ref     := reference(),
                               type    := atom(),
                               service := string()}].
which_transports() ->
    diameter_config:which_transports().


-doc """
Return a list of transports associated with the service 'SvcName'.
""".
-doc(#{since => <<"OTP 26.2.4">>}).
-spec which_transports(SvcName) -> [#{ref  := reference(),
                                      type := atom()}] when
      SvcName :: string().

which_transports(SvcName) ->
    diameter_config:which_transports(SvcName).


%% ---------------------------------------------------------------------------
%% which_watchdogs/0, which_watchdogs/1
%% ---------------------------------------------------------------------------

-doc """
Return a list of _all_ watchdogs.
""".
-doc(#{since => <<"OTP 26.2.4">>}).

-spec which_watchdogs() -> [#{ref     := reference(),
                              type    := atom(),
                              pid     := pid(),
                              state   := diameter_service:wd_state(),
                              peer    := boolean() | pid(),
                              uptime  := elapsed_time(),
                              service := SvcName}] when
      SvcName :: string().

which_watchdogs() ->
    diameter_service:which_watchdogs().


-doc """
Return a list of watchdogs associated with the service 'SvcName'.
""".
-doc(#{since => <<"OTP 26.2.4">>}).

-spec which_watchdogs(SvcName) ->
          [#{ref     := reference(),
             type    := atom(),
             pid     := pid(),
             state   := diameter_service:wd_state(),
             peer    := boolean() | pid(),
             uptime  := elapsed_time()}] when
      SvcName :: string().

which_watchdogs(SvcName) ->
    diameter_service:which_watchdogs(SvcName).


%% ---------------------------------------------------------------------------
%% which_connections/0, which_connections/1
%% ---------------------------------------------------------------------------

-doc """
Return a list of _all_ connections, grouped by the service they
are associated with.
""".
-doc(#{since => <<"OTP 26.2.4">>}).

-spec which_connections() ->
          [{SvcName,
            [#{peer     := PeerInfo,
               wd       := WDInfo,
               peername := {inet:ip_address(), inet:port_number()},
               sockname := {inet:ip_address(), inet:port_number()}}]}] when
      SvcName  :: string(),
      PeerInfo :: #{pid    := pid(),
                    uptime := elapsed_time()},
      WDInfo   :: #{ref    := reference(),
                    type   := atom(),
                    pid    := pid(),
                    state  := diameter_service:wd_state(),
                    uptime := elapsed_time()}.

which_connections() ->
    diameter_service:which_connections().


-doc """
Return a list of connections associated with the service 'SvcName'.
""".
-doc(#{since => <<"OTP 26.2.4">>}).

-spec which_connections(SvcName) ->
          [#{peer     := PeerInfo,
             wd       := WDInfo,
             peername := {inet:ip_address(), inet:port_number()},
             sockname := {inet:ip_address(), inet:port_number()}}] when
      SvcName :: string(),
      PeerInfo :: #{pid    := pid(),
                    uptime := elapsed_time()},
      WDInfo   :: #{ref    := reference(),
                    type   := atom(),
                    pid    := pid(),
                    state  := diameter_service:wd_state(),
                    uptime := elapsed_time()}.

which_connections(SvcName) ->
    diameter_service:which_connections(SvcName).


%% ---------------------------------------------------------------------------
%% subscribe/1
%% ---------------------------------------------------------------------------

-doc """
Subscribe to [`service_event()`](`m:diameter#service_event`) messages from a
service.

It is not an error to subscribe to events from a service that does not yet
exist. Doing so before adding transports is required to guarantee the reception
of all transport-related events.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec subscribe(SvcName) -> true when
      SvcName :: service_name().

subscribe(SvcName) ->
    diameter_service:subscribe(SvcName).


%% ---------------------------------------------------------------------------
%% unsubscribe/1
%% ---------------------------------------------------------------------------

-doc """
Unsubscribe to event messages from a service.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec unsubscribe(SvcName) -> true when
      SvcName :: service_name().

unsubscribe(SvcName) ->
    diameter_service:unsubscribe(SvcName).


%% ---------------------------------------------------------------------------
%% session_id/1
%% ---------------------------------------------------------------------------

-doc """
Return a value for a Session-Id AVP.

The value has the form required by section 8.8 of RFC 6733. Ident should be the
Origin-Host of the peer from which the message containing the returned value
will be sent.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec session_id(Ident) -> SessionId when
      Ident     :: 'DiameterIdentity'(),
      SessionId :: 'OctetString'().

session_id(Ident) ->
    diameter_session:session_id(Ident).


%% ---------------------------------------------------------------------------
%% origin_state_id/0
%% ---------------------------------------------------------------------------

-doc """
Return a reasonable value for use as Origin-State-Id in outgoing messages.

The value returned is the number of seconds since 19680120T031408Z, the first
value that can be encoded as a Diameter [`Time()`](diameter_dict.md#DATA_TYPES),
at the time the diameter application was started.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec origin_state_id() -> 'Unsigned32'().

origin_state_id() ->
    diameter_session:origin_state_id().


%% ---------------------------------------------------------------------------
%% call/3,4
%% ---------------------------------------------------------------------------

-doc """
Send a Diameter request message.

`App` specifies the Diameter application in which the request is defined and
callbacks to the corresponding callback module will follow as described below
and in `m:diameter_app`. Unless the `detach` option is specified, the call
returns either when an answer message is received from the peer or an error
occurs. In the answer case, the return value is as returned by a
[handle_answer/4](`c:diameter_app:handle_answer/4`) callback. In the error case,
whether or not the error is returned directly by diameter or from a
[handle_error/4](`c:diameter_app:handle_error/4`) callback depends on whether or
not the outgoing request is successfully encoded for transmission to the peer,
the cases being documented below.

If there are no suitable peers, or if
[pick_peer/4](`c:diameter_app:pick_peer/4`) rejects them by returning `false`,
then `{error,no_connection}` is returned. Otherwise
[pick_peer/4](`c:diameter_app:pick_peer/4`) is followed by a
[prepare_request/3](`c:diameter_app:prepare_request/3`) callback, the message is
encoded and then sent.

There are several error cases which may prevent an answer from being received
and passed to a [handle_answer/4](`c:diameter_app:handle_answer/4`) callback:

- If the initial encode of the outgoing request fails, then the request process
  fails and `{error,encode}` is returned.
- If the request is successfully encoded and sent but the answer times out then
  a [handle_error/4](`c:diameter_app:handle_error/4`) callback takes place with
  `Reason = timeout`.
- If the request is successfully encoded and sent but the service in question is
  stopped before an answer is received then a
  [handle_error/4](`c:diameter_app:handle_error/4`) callback takes place with
  `Reason = cancel`.
- If the transport connection with the peer goes down after the request has been
  sent but before an answer has been received then an attempt is made to resend
  the request to an alternate peer. If no such peer is available, or if the
  subsequent [pick_peer/4](`c:diameter_app:pick_peer/4`) callback rejects the
  candidates, then a [handle_error/4](`c:diameter_app:handle_error/4`) callback
  takes place with `Reason = failover`. If a peer is selected then a
  [prepare_retransmit/3](`c:diameter_app:prepare_retransmit/3`) callback takes
  place, after which the semantics are the same as following an initial
  [prepare_request/3](`c:diameter_app:prepare_request/3`) callback.
- If an encode error takes place during retransmission then the request process
  fails and `{error,failure}` is returned.
- If an application callback made in processing the request fails (pick_peer,
  prepare_request, prepare_retransmit, handle_answer or handle_error) then
  either `{error,encode}` or `{error,failure}` is returned depending on whether
  or not there has been an attempt to send the request over the transport.

Note that `{error,encode}` is the only return value which guarantees that the
request has _not_ been sent over the transport connection.
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec call(SvcName, App, Request, CallOpts)
   -> Result when
      SvcName  :: service_name(),
      App      :: app_alias(),
      Request  :: diameter_codec:message() | diameter_codec:packet(),
      CallOpts :: [call_opt()],
      Result   :: ok | {error, Reason} | Answer,
      Answer   :: term(),
      Reason   :: term().

call(SvcName, App, Message, Options) ->
    diameter_traffic:send_request(SvcName, {alias, App}, Message, Options).

-doc false.
call(SvcName, App, Message) ->
    call(SvcName, App, Message, []).

%% ===========================================================================

%% Diameter basic types

-type 'OctetString'() :: iolist().
-type 'Integer32'()   :: -2147483647..2147483647.
-type 'Integer64'()   :: -9223372036854775807..9223372036854775807.
-type 'Unsigned32'()  :: 0..4294967295.
-type 'Unsigned64'()  :: 0..18446744073709551615.
-type 'Float32'()     :: '-infinity' | float() | infinity.
-type 'Float64'()     :: '-infinity' | float() | infinity.
-type 'Grouped'()     :: list() | tuple().

%% Diameter derived types

-type 'Address'()
   :: inet:ip_address()
    | string().

-type 'Time'()             :: {{integer(), 1..12, 1..31},
                               {0..23, 0..59, 0..59}}.
-type 'UTF8String'()       :: iolist().
-type 'DiameterIdentity'() :: 'OctetString'().
-type 'DiameterURI'()      :: 'OctetString'().
-type 'Enumerated'()       :: 'Integer32'().
-type 'IPFilterRule'()     :: 'OctetString'().
-type 'QoSFilterRule'()    :: 'OctetString'().

%% The handle to a service.

-type service_name()
   :: any().

%% Capabilities options/avps on start_service/2 and/or add_transport/2

-type capability()
   :: {'Origin-Host',                    'DiameterIdentity'()}
    | {'Origin-Realm',                   'DiameterIdentity'()}
    | {'Host-IP-Address',                ['Address'()]}
    | {'Vendor-Id',                      'Unsigned32'()}
    | {'Product-Name',                   'UTF8String'()}
    | {'Supported-Vendor-Id',            ['Unsigned32'()]}
    | {'Auth-Application-Id',            ['Unsigned32'()]}
    | {'Vendor-Specific-Application-Id', ['Grouped'()]}
    | {'Firmware-Revision',              'Unsigned32'()}.

%% Filters for call/4

-type peer_filter()
   :: none
    | host
    | realm
    | {host,  any|'DiameterIdentity'()}
    | {realm, any|'DiameterIdentity'()}
    | {eval, eval()}
    | {neg, peer_filter()}
    | {first, [peer_filter()]}
    | {all, [peer_filter()]}
    | {any, [peer_filter()]}.

-opaque peer_ref()
   :: pid().

-type eval()
   :: {module(), atom(), list()}
    | fun()
    | maybe_improper_list(eval(), list()).

-type evaluable()
   :: eval().

-type sequence()
   :: {'Unsigned32'(), 0..32}.

-type restriction()
   :: false
    | node
    | nodes
    | [node()]
    | eval().

-type remotes()
   :: boolean()
    | [node()]
    | eval().

-type message_length()
   :: 0..16#FFFFFF.

-type decode_format()
   :: record
    | list
    | map
    | none
    | record_from_map.

-type strict_arities()
   :: false
    | encode
    | decode.

%% Options common to both start_service/2 and add_transport/2.

-type common_opt()
   :: {avp_dictionaries, [module()]}
    | {capabilities_cb, eval()}
    | {capx_timeout, 'Unsigned32'()}
    | {connect_timer, 'Unsigned32'()}
    | {disconnect_cb, eval()}
    | {dpa_timeout, 'Unsigned32'()}
    | {dpr_timeout, 'Unsigned32'()}
    | {incoming_maxlen, message_length()}
    | {length_errors, exit | handle | discard}
    | {pool_size, pos_integer()}
    | {spawn_opt, list() | mfa()}
    | {strict_capx, boolean()}
    | {strict_mbit, boolean()}
    | {watchdog_config, [{okay|suspect, non_neg_integer()}]}
    | {watchdog_timer, 'Unsigned32'() | {module(), atom(), list()}}.

%% Options passed to start_service/2

-type service_opt()
   :: capability()
    | {application, [application_opt()]}
    | {decode_format, decode_format()}
    | {restrict_connections, restriction()}
    | {sequence, sequence() | eval()}
    | {share_peers, remotes()}
    | {strict_arities, true | strict_arities()}
    | {string_decode, boolean()}
    | {traffic_counters, boolean()}
    | {use_shared_peers, remotes()}
    | {bins_info, boolean() | non_neg_integer()}
    | common_opt().

-type application_opt()
   :: {alias, app_alias()}
    | {answer_errors, callback|report|discard}
    | {call_mutates_state, boolean()}
    | {dictionary, module()}
    | {module, app_module()}
    | {request_errors, answer_3xxx|answer|callback}
    | {state, any()}.

-type app_alias()
   :: any().

-type app_module()
   :: module()
    | maybe_improper_list(module(), list())
    | #diameter_callback{}.

%% Identifier returned by add_transport/2

-type transport_ref()
   :: reference().

%% Options passed to add_transport/2

-type transport_opt()
   :: {applications, [app_alias()]}
    | {capabilities, [capability()]}
    | {transport_config, any()}
    | {transport_config, any(), 'Unsigned32'() | infinity}
    | {transport_module, atom()}
    | common_opt()
    | {private, any()}.

%% Predicate passed to remove_transport/2

-type transport_pred()
   :: fun((transport_ref(), connect|listen, list()) -> boolean())
    | fun((transport_ref(), list()) -> boolean())
    | fun((list()) -> boolean())
    | transport_ref()
    | boolean()
    | list()
    | {connect|listen, transport_pred()}
    | {atom(), atom(), list()}.

%% Options passed to call/4

-type call_opt()
   :: detach
    | {extra, list()}
    | {filter, peer_filter()}
    | {peer, peer_ref()}
    | {timeout, 'Unsigned32'()}.

-type elapsed_time() ::
        {Hours     :: non_neg_integer(),
         Mins      :: 0..59,
         Secs      :: 0..59,
         MicroSecs :: 0..999999}.

