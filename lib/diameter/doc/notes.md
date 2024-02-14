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
# Release Notes

Releases are listed in reverse chronological order, most recent first.

## diameter 2.3.1

### Fixed Bugs and Malfunctions

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

## diameter 2.3

### Improvements and New Features

- Replace size/1 with either tuple_size/1 or byte_size/1

  The [`size/1`](`size/1`) BIF is not optimized by the JIT, and its use can
  result in worse types for Dialyzer.

  When one knows that the value being tested must be a tuple,
  [`tuple_size/1`](`tuple_size/1`) should always be preferred.

  When one knows that the value being tested must be a binary,
  [`byte_size/1`](`byte_size/1`) should be preferred. However,
  [`byte_size/1`](`byte_size/1`) also accepts a bitstring (rounding up size to a
  whole number of bytes), so one must make sure that the call to `byte_size/` is
  preceded by a call to [`is_binary/1`](`is_binary/1`) to ensure that bitstrings
  are rejected. Note that the compiler removes redundant calls to
  [`is_binary/1`](`is_binary/1`), so if one is not sure whether previous code
  had made sure that the argument is a binary, it does not harm to add an
  [`is_binary/1`](`is_binary/1`) test immediately before the call to
  [`byte_size/1`](`byte_size/1`).

  Own Id: OTP-18405 Aux Id:
  GH-6672,PR-6702,PR-6768,PR-6700,PR-6769,PR-6812,PR-6814

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- The implementation has been fixed to use `proc_lib:init_fail/2,3` where
  appropriate, instead of `proc_lib:init_ack/1,2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18490 Aux Id: OTP-18471, GH-6339, PR-6843

## diameter 2.2.7

### Improvements and New Features

- There is a new configure option, `--enable-deterministic-build`, which will
  apply the `deterministic` compiler option when building Erlang/OTP. The
  `deterministic` option has been improved to eliminate more sources of
  non-determinism in several applications.

  Own Id: OTP-18165 Aux Id: PR-5965

## diameter 2.2.6

### Fixed Bugs and Malfunctions

- Fix decode of non-IP address types; that is, of values of the derived AVP data
  format Address whose first two octets specify an address family other than 1
  (IP) or 2 (IP6). Such values have never been decoded, and were treated as
  decode errors. They're now decoded to a 2-tuple of the integer() address
  family and binary() remaining octets, with no family-specific decode. The
  2-tuple distinguishes the decode from the 4-tuple and 8-tuple IP address
  decodes. 2-tuples are also now encoded.

  Note that even currently unassigned address families are decoded: only the
  reserved values, 0 and 65535, are treated as errors.

  Own Id: OTP-17976 Aux Id: GH-5463

## diameter 2.2.5

### Fixed Bugs and Malfunctions

- The compilation time is no longer recorded in BEAM files. There remained
  several undocumented functions that attempted to retrieve compilation times.
  Those have now been removed.

  Own Id: OTP-17962

## diameter 2.2.4

### Fixed Bugs and Malfunctions

- The unordered option was ignored on a client diameter_sctp transport, causing
  all delivery to be ordered.

  The association id was not specified to gen_sctp when requesting unordered
  delivery, causing the setting to be applied to the whole endpoint.

  Thanks to Bengt Kleberg and Andreas Schultz.

  Own Id: OTP-17366 Aux Id: GH-4775

## diameter 2.2.3

### Fixed Bugs and Malfunctions

- Add the 'first' tuple to type diameter:peer_filter/0. The filter was added in
  OTP-17.5.6.8 and OTP-18.3, but neither release updated the type specification.

  Own Id: OTP-16548 Aux Id: ERL-1191

## diameter 2.2.2

### Fixed Bugs and Malfunctions

- The possibility of choosing a handler process for an incoming Diameter request
  with a configured MFA was documented in OTP 20.0, but counters (with
  \{traffic_counters, true\}) were not incremented when this process was on a
  remote node. Counters are now incremented on the node that configures the
  transport in question.

  Introduced in OTP 21.3.

  Own Id: OTP-16457

- Transport options differing from those passed to diameter:add_transport/2 were
  used in several situations: when starting a transport process after
  connect_timer expiry after an initial connection attempt has failed, when
  starting a transport process after a connection has been accepted, when
  sending events, when returning options in diameter:service_info/2, and
  possibly more. In particular, the following configuration options to
  diameter:add_transport/2 were dropped: avp_dictionaries, incoming_maxlen,
  spawn_opt, strict_mbit.

  Moreover, any service options mistakenly passed to diameter:add_transport/2
  were interpreted as such, instead of being ignored as the documentation
  states, with the consequence that outgoing and incoming requests saw different
  values of some options, some were always taken from transport options, and
  others from service options.

  diameter:add_transport/2 must be called in new code for the fix to have
  effect.

  Introduced in OTP 20.1.

  Own Id: OTP-16459

## diameter 2.2.1

### Fixed Bugs and Malfunctions

- Fix inadvertently broad monitor that resulted in gen_server cast messages to
  hidden nodes from module diameter_dist.

  Own Id: OTP-15768

## diameter 2.2

### Fixed Bugs and Malfunctions

- Fix failure of incoming answer message with faulty Experimental-Result-Code.
  Failure to decode the AVP resulted in an uncaught exception, with no no
  handle_answer/error callback as a consequence.

  Own Id: OTP-15569 Aux Id: ERIERL-302

### Improvements and New Features

- Add spawn_opt MFA configuration to allow a callback to spawn a handler process
  for an incoming Diameter request on an an arbitrary node. Module diameter_dist
  provides a route_session/2 that can be used to distribute requests based on
  Session-Id, although this module is currently only documented in the module
  itself and may change.

  Own Id: OTP-15398

## diameter 2.1.6

### Fixed Bugs and Malfunctions

- Fix function_clause when sending an outgoing request after DPA has been sent
  in response to an incoming DPR. The caused the diameter_peer_fsm gen_server
  associated with the peer connection to fail, which could then result in the
  transport connection being reset before the peer closed it upon reception of
  DPA.

  Own Id: OTP-15198 Aux Id: ERIERL-213

## diameter 2.1.5

### Fixed Bugs and Malfunctions

- Fix documentation typos.

  Own Id: OTP-15045

## diameter 2.1.4.1

### Fixed Bugs and Malfunctions

- Fix failure of incoming answer message with faulty Experimental-Result-Code.
  Failure to decode the AVP resulted in an uncaught exception, with no no
  handle_answer/error callback as a consequence.

  Own Id: OTP-15569 Aux Id: ERIERL-302

## diameter 2.1.4

### Fixed Bugs and Malfunctions

- Fix close of diameter_tcp/sctp listening socket at
  diameter:remove_transport/2, that was broken in diameter 2.1. A reconfigured
  transport could not listen on the same endpoint as a result.

  Own Id: OTP-14839

- Fix handling of SUSPECT connections at service termination. A connection with
  this watchdog state caused diameter_service:terminate/2 to fail.

  Own Id: OTP-14947 Aux Id: ERIERL-124

## diameter 2.1.3

### Fixed Bugs and Malfunctions

- Fix documentation typo: peer_up/3 was written where peer_down/3 was intended.

  Own Id: OTP-14805

## diameter 2.1.2

### Fixed Bugs and Malfunctions

- A fault introduced in diameter 2.1 could cause decode errors to be ignored in
  AVPs following the header of a Grouped AVP.

  Own Id: OTP-14684 Aux Id: ERIERL-85

## diameter 2.1.1

### Fixed Bugs and Malfunctions

- An inadvertently removed monitor in diameter 2.1 caused the ets table
  diameter_reg to leak entries, and caused service restart and more to fail.

  Own Id: OTP-14668 Aux Id: ERIERL-83

## diameter 2.1

### Fixed Bugs and Malfunctions

- Fix handling of Proxy-Info in answer messages setting the E-bit.

  RFC 6733 requires that Proxy-Info AVPs in an incoming request be echoed in an
  outgoing answer. This was not done in answers formulated by diameter; for
  example, as a result of a handle_request callback having returned an
  'answer-message' or protocol_error tuple.

  Own Id: OTP-9869

- React to nodeup/nodedown when sharing peer connections.

  Service configuration share_peers and use_shared_peers did not respond to the
  coming and going of remote nodes.

  Own Id: OTP-14011

- Fix inappropriate message callbacks.

  An incoming CER or DPR was regarded as discarded, resulting in a corresponding
  message callback (if configured) in diameter_tcp/sctp.

  Own Id: OTP-14486

- Fix handling of 5009 errors (DIAMETER_AVP_OCCURS_TOO_MANY TIMES).

  RFC 6733 says that the first AVP that exceeds the bound should be reported,
  but the suggestions in the errors field of a diameter_packet record counted
  AVPs from the rear of the message, not the front. Additionally, diameter 2.0
  in OTP 20.0 broke the counting by accepting one more AVP than the message
  grammar in question allowed.

  Own Id: OTP-14512

- Match case insensitively in diameter_tcp/sctp accept tuple.

  Matching of remote addresses when accepting connections in a listening
  transport was case-sensitive, causing the semantics to change as a consequence
  of (kernel) OTP-13006.

  Own Id: OTP-14535 Aux Id: OTP-13006

- Fix backwards incompatibility of remote send when sharing transports.

  The sending of requests over a transport connection on a remote node running
  an older version of diameter was broken by diameter 2.0 in OTP 20.0.

  Own Id: OTP-14552

- Fix diameter_packet.avps decode of Grouped AVP errors in Failed-AVP.

  Decode didn't produce a list of diameter_avp records, so information about
  faulty component AVPs was lost.

  Own Id: OTP-14607

### Improvements and New Features

- Let unordered delivery be configured in diameter_sctp.

  With option \{unordered, boolean() | pos_integer()\}, with false the default,
  and N equivalent to OS =< N, where OS is the number of outbound streams
  negotiated on the association in question. If configured, unordered sending
  commences upon reception of a second message, outgoing messages being sent on
  stream 0 before this.

  The default false is for backwards compatibility, but false or 1 should be set
  to follow RFC 6733's recommendation on the use of unordered sending to avoid
  head-of-line blocking. There is typically no meaningful order to preserve,
  since the order in which outgoing messages are received by a transport process
  isn't known to the sender.

  Own Id: OTP-10889

- Complete/simplify Standards Compliance in User's Guide.

  Own Id: OTP-10927

- Add service option decode_format.

  To allow incoming messages to be decoded into maps or lists instead of
  records. Messages can be presented in any of the formats for encode.

  Decode performance has also been improved.

  Own Id: OTP-14511 Aux Id: OTP-14343

- Add service option traffic_counters.

  To let message-related counters be disabled, which can be a performance
  improvement in some usecases.

  Own Id: OTP-14521

- Allow loopback/any as local addresses in diameter_tcp/sctp.

  The atoms were implied by documentation, but not handled in code.

  Own Id: OTP-14544

- Add transport option strict_capx.

  To allow the RFC 6733 requirement that a transport connection be closed if a
  message is received before capabilities exchange to be relaxed.

  Own Id: OTP-14546

- Be consistent with service/transport configuration.

  For options for which it's meaningful, defaults values for transport options
  can now be configured on a service. This was previously the case only for an
  arbitrary subset of options.

  Own Id: OTP-14555

- Add service/transport option avp_dictionaries.

  To provide better support for AVPs that are not defined in the application
  dictionary: configuring additional dictionaries in an avp_dictionaries tuple
  allows their AVPs to be encoded/decoded in much the same fashion as
  application AVPs.

  The motivation is RFC 7683 Diameter Overload, Indicator Conveyance (DOIC),
  that defines AVPs intended to be piggybacked onto arbitrary messages. A DOIC
  dictionary has been included in the installation, in module
  diameter_gen_doic_rfc7683.

  Own Id: OTP-14588

- Decode application AVPs in answers setting the E-bit.

  AVPs defined in the application of the message being sent were previously not
  decoded, only those in the common application that defines the answer-message
  grammar.

  Own Id: OTP-14596

## diameter 2.0

### Improvements and New Features

- Let candidate peers be passed to diameter:call/4

  With call option peer, to allow a request to be sent to a peer that hasn't
  advertised support for the application in question.

  RFC 6733 2.4 requires a node to send the application identifiers of all
  locally supported applications at capabilities exchange, but not all nodes
  respect this for the common application, and diameter itself will send
  D\[WP]\[RA] without the common application having been explicitly advertised.
  Regarding the common application as implicit renders Result-Code 5010
  (DIAMETER_NO_COMMON_APPLICATION) meaningless however, so allow any request to
  be sent as long as there is a configured dictionary to support it.

  Own Id: OTP-14338

- Improve performance of message encode/decode and related handling.

  Dictionaries using @custom_types or @codecs will need to adapt the
  corresponding functions to accept an additional argument that is now passed
  through encode/decode, which was required to remove various process
  dictionary-based workarounds that have been used to solve problems in the
  past.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14343

- Add transport options to avoid deadlock and allow for load regulation.

  Both diameter_tcp and diameter_sctp now accept two new configuration options:
  sender and message_cb. The former causes outgoing sends to take place in a
  dedicated process, to avoid the possibility of deadlock when both the
  transport process and its peer block in send. The latter allows a callback to
  control the reading of messages on the socket, to allow for backpressure
  towards peers when the rate of incoming traffic is greater than can otherwise
  be handled.

  Neither of these options are yet documented, but are unlikely to change unless
  problems are discovered. The sender option is not the default since it should
  probably always be used in combination with message_cb, to prevent incoming
  requests from being read at a higher rate than a peer allows outgoing answers
  to be sent.

  Own Id: OTP-14455 Aux Id: ERL-332

## diameter 1.12.2

### Fixed Bugs and Malfunctions

- An improvement in the handling of peer failover in diameter 1.12.1 adversely
  affected performance when sending requests. Further, the inefficient use of a
  public table to route incoming answers has been removed.

  Own Id: OTP-14206

- Fixed xml issues in old release notes

  Own Id: OTP-14269

## diameter 1.12.1

### Fixed Bugs and Malfunctions

- Close diameter_tcp/sctp listening sockets at diameter:stop_service/1.

  Broken by OTP-13611.

  Own Id: OTP-13787 Aux Id: OTP-13611

- Update build scripts to not make assumptions about where env, cp and perl are
  located.

  Own Id: OTP-13800

## diameter 1.12

### Fixed Bugs and Malfunctions

- Ensure listening socket is closed at transport removal.

  Transport removal did not immediately close a `diameter_tcp/sctp` listening
  socket, and a subsequent peer connection caused it to remain open.

  Own Id: OTP-13611

### Improvements and New Features

- Add `diameter:peer_info/1`.

  That retrieves information in the style of `diameter:service_info/2`, but for
  a single peer connection.

  Own Id: OTP-13508

## diameter 1.11.2

### Fixed Bugs and Malfunctions

- Make peer handling more efficient.

  Inefficient lookup and manipulation of peer lists could result in poor
  performance when many outgoing requests were sent simultaneously, or when many
  peers connected simultaneously. Filtering peer lists on realm/host is now also
  more efficient in many cases.

  Own Id: OTP-13164

- Fix handling of shared peer connections in watchdog state SUSPECT.

  A peer connection shared from a remote node was regarded as being up for the
  lifetime of the connection, ignoring watchdog transitions into state SUSPECT.

  Own Id: OTP-13342

## diameter 1.11.1

### Fixed Bugs and Malfunctions

- Fix request table leaks

  The End-to-End and Hop-by-Hop identifiers of outgoing Diameter requests are
  stored in a table in order for the caller to be located when the corresponding
  answer message is received. Entries were orphaned if the handler was
  terminated by an exit signal as a consequence of actions taken by callback
  functions, or if callbacks modified identifiers in retransmission cases.

  Own Id: OTP-13137

## diameter 1.11

### Fixed Bugs and Malfunctions

- Fix relay encode of nested, Grouped AVPs.

  A fault in OTP-12475 caused encode to fail if the first AVP in a Grouped AVP
  was itself Grouped.

  Own Id: OTP-12879 Aux Id: OTP-12475

- Match acceptable peer addresses case insensitively.

  Regular expressions passed in an 'accept' tuple to diameter_tcp or
  diameter_sctp inappropriately matched case.

  Own Id: OTP-12902

- Fix diameter_watchdog function clause.

  OTP-12912 introduced an error with accepting transports setting
  `{restrict_connections, false}`, causing processes to fail when peer
  connections were terminated.

  Own Id: OTP-12969

### Improvements and New Features

- Don't report 5005 (DIAMETER_AVP_MISSING) errors unnecessarily.

  An AVP whose decode failed was reported as missing, despite having been
  reported with another error as a consequence of the failure.

  Own Id: OTP-12871

- Improve decode performance.

  The time required to decode a message increased quadratically with the number
  of AVPs in the worst case, leading to extremely long execution times.

  Own Id: OTP-12891

- Improve watchdog and statistics performance.

  Inefficient use of timers contributed to poor performance at high load, as did
  ordering of the table statistics are written to.

  Own Id: OTP-12912

- Add service_opt() strict_mbit.

  There are differing opinions on whether or not reception of an arbitrary AVP
  setting the M-bit is an error. The default interpretation is strict: if a
  command grammar doesn't explicitly allow an AVP setting the M-bit then
  reception of such an AVP is regarded as an error. Setting
  `{strict_mbit, false}` disables this check.

  Own Id: OTP-12947

## diameter 1.10

### Fixed Bugs and Malfunctions

- Fix decode of Grouped AVPs containing errors.

  RFC 6733 says this of Failed-AVP in 7.5:

  - \_\_\_\_ -
    `In the case where the offending AVP is embedded within a Grouped AVP, the Failed-AVP MAY contain the grouped AVP, which in turn contains the single offending AVP. The same method MAY be employed if the grouped AVP itself is embedded in yet another grouped AVP and so on. In this case, the Failed-AVP MAY contain the grouped AVP hierarchy up to the single offending AVP. This enables the recipient to detect the location of the offending AVP when embedded in a group.`

  It says this of DIAMETER_INVALID_AVP_LENGTH in 7.1.5:

  - \_\_\_\_ -
    `The request contained an AVP with an invalid length. A Diameter message indicating this error MUST include the offending AVPs within a Failed-AVP AVP. In cases where the erroneous AVP length value exceeds the message length or is less than the minimum AVP header length, it is sufficient to include the offending AVP header and a zero filled payload of the minimum required length for the payloads data type. If the AVP is a Grouped AVP, the Grouped AVP header with an empty payload would be sufficient to indicate the offending AVP. In the case where the offending AVP header cannot be fully decoded when the AVP length is less than the minimum AVP header length, it is sufficient to include an offending AVP header that is formulated by padding the incomplete AVP header with zero up to the minimum AVP header length.`

  The AVPs placed in the errors field of a diameter_packet record are intended
  to be appropriate for inclusion in a Failed-AVP, but neither of the above
  paragraphs has been followed in the Grouped case: the entire faulty AVP
  (non-faulty components and all) has been included. This made it difficult to
  identify the actual faulty AVP in all but simple cases.

  The decode is now adapted to the RFC, and implements the suggested single
  faulty AVP, nested in as many Grouped containers as required.

  Own Id: OTP-12721

- Fix SCTP problems on Solaris.

  The allocation of association ids in Solaris was in conflict with an
  assumption made in diameter_sctp, resulting in failures when accepting
  multiple peer connections.

  Own Id: OTP-12768

- Fix start order of alternate transports.

  A transport configured with diameter:add_transport/2 can be passed multiple
  transport_module/transport_config tuples in order to specify alternate
  configuration, modules being attempted in order until one succeeds. This is
  primarily for the connecting case; for example, to allow a transport to be
  configured to first attempt connection over SCTP, and then TCP in case SCTP
  fails. Multiple module tuples can be paired with a single config tuple, but in
  this case the start order was reversed relative to the order in which the
  modules were specified.

  Own Id: OTP-12851

### Improvements and New Features

- Change license text from Erlang Public License to Apache Public License v2.

  Own Id: OTP-12845

## diameter 1.9.2

### Fixed Bugs and Malfunctions

- Fix broken relay counters.

  OTP-12654 in OTP 17.5.3 broke counters in the case of answer messages received
  in the relay application. Counters were accumulated as unknown messages or
  no_result_code instead of as relayed messages on the intended Result-Code and
  'Experimental-Result' tuples.

  Own Id: OTP-12741

- Fix diameter_sctp listener race.

  An oversight in OTP-12428 made it possible to start a transport process that
  could not establish associations.

  Own Id: OTP-12744

## diameter 1.9.1

### Known Bugs and Problems

- Don't leave extra bit in decoded AVP data.

  OTP-12074 in OTP 17.3 missed one case: a length error on a trailing AVP
  unknown to the dictionary in question.

  Own Id: OTP-12642

- Don't confuse Result-Code and Experimental-Result.

  The errors field of a decoded diameter_packet record was populated with a
  Result-Code AVP when an Experimental-Result containing a 3xxx Result-Code was
  received in an answer not setting the E-bit. The correct AVP is now extracted
  from the incoming message.

  Own Id: OTP-12654

- Don't count on unknown Application Id.

  OTP-11721 in OTP 17.1 missed the case of an Application Id not agreeing with
  that of the dictionary in question, causing counters to be accumulated on keys
  containing the unknown id.

  Own Id: OTP-12701

## diameter 1.9

### Fixed Bugs and Malfunctions

- Don't discard outgoing answers unnecessarily.

  Answers missing a Result-Code AVP or setting an E-bit inappropriately were
  discarded even if encode was successful.

  Own Id: OTP-11492

- Increase supervision timeouts.

  At diameter application shutdown, DPR could be omitted on open peer
  connections because of short supervision timeouts.

  Own Id: OTP-12412

- Fix retransmission of messages sent as header/avps list.

  Extracting End-to-End and Hop-by-Hop Identifiers resulted in a function clause
  error, resulting in a handle_error callback.

  Own Id: OTP-12415

- Fix diameter_avp decode of Grouped AVPs having decode errors.

  Components of such an AVP were not extracted, causing it to be represented by
  a single diameter_avp record instead of the intended list.

  Dictionary files must be recompiled for the fix to have effect.

  Own Id: OTP-12475

- Fix ordering of AVPs in relayed messages.

  The order was reversed relative to the received order, with a Route-Record AVP
  prepended.

  Thanks to Andrzej Trawiński.

  Own Id: OTP-12551

- Fix issues with DiameterURI encode/decode.

  RFC 6773 changed the default port and transport, but the RFC 3588 defaults
  were used even if the RFC 6733 common dictionary was in use. The RFC 3588
  defaults are now only used when the common dictionary is
  diameter_gen_base_rfc3588.

  Both RFC 3588 and 6733 disallow transport=udp;protocol=diameter. Encode of the
  combination now fails.

  Decode of ports numbers outside the range 0-65535 and fully qualified domain
  names longer than 255 octets now fails.

  Note that RFC 3588 is obsolete, and that there is a diameter_gen_base_rfc6733.
  The change in defaults is a potential interoperability problem when moving to
  RFC 6733 with peers that do not send all URI components. The fact that 6733
  allows 5xxx result codes in answer messages setting the E-bit, which RFC 3588
  doesn't, is another.

  Own Id: OTP-12589

### Improvements and New Features

- Add service_opt() string_decode.

  To disable the decode of potentially large binaries to string. This prevents
  large strings from being copied when incoming Diameter messages are passed
  between processes, a vulnerability that can lead to memory being exhausted
  given sufficiently malicious peers.

  The value is a boolean(), true being the default for backwards compatibility.
  Setting false causes both diameter_caps records and decoded messages to
  contain binary() in relevant places that previously had string():
  diameter_app(3) callbacks need to be prepared for the change.

  The Diameter types affected are OctetString and the derived types UTF8String,
  DiameterIdentity, DiameterURI, IPFilterRule, and QoSFilterRule. Time and
  Address are unaffected.

  Own Id: OTP-11952

- Add transport_opt() pool_size.

  To allow for pools of accepting transport processes, which can better service
  multiple simultaneous peer connections. The option can also be used with
  connecting transports, to establish multiple connections to the same peer
  without having to configure multiple transports.

  Own Id: OTP-12428

- Allow DPR to be sent with diameter:call/4.

  It has been possible to send, but the answer was regarded as unsolicited and
  discarded. DPA now causes the transport process in question to be terminated,
  as for DPR that diameter itself sends.

  Own Id: OTP-12542

- Discard requests after DPR.

  RFC 6733 is imprecise, but the tone is that messages received after DPR are an
  exception to be dealt with only because of the possibility of unordered
  delivery over SCTP. As a consequence, and because a request following DPR is
  unlikely to be answered due to the impending loss of the peer connection,
  discard outgoing requests following an outgoing or incoming DPR. Incoming
  requests are also discarded, with the exception of DPR itself. Answers are
  sent and received as usual.

  Own Id: OTP-12543

- Add transport_opt() dpr_timeout.

  To cause a peer connection to be closed following an outgoing DPA when the
  peer fails to do so. It is the recipient of DPA that should close the
  connection according to RFC 6733.

  Own Id: OTP-12609

- Add service_opt() incoming_maxlen.

  To bound the expected size of incoming Diameter messages. Messages larger than
  the specified number of bytes are discarded, to prevent a malicious peer from
  generating excessive load.

  Own Id: OTP-12628

## diameter 1.8

### Fixed Bugs and Malfunctions

- Fix remote diameter_request table leak.

  An outgoing request whose pick_peer callback selected a transport on another
  node resulted in an orphaned table entry on that node.

  Own Id: OTP-12196

- Fix handling of 3xxx Result-Code without E-bit.

  OTP-12233 broke the population of the errors field of the diameter_packet
  record when an incoming request with an E-bit/Result-Code mismatch was
  detected, causing a 4-tuple to be inserted as Result-Code in a diameter_avp
  record.

  Own Id: OTP-12233

- Fix ignored connect timer.

  There are two timers governing the establishment of peer connections:
  connect_timer and watchdog_timer. The former is the RFC 6733 Tc timer, and is
  used at initial connection establishment. The latter is RFC 3539 TwInit, and
  is used for connection reestablishment. A connecting transport erroneously
  used watchdog_timer in both cases.

  Own Id: OTP-12281 Aux Id: seq12728

### Improvements and New Features

- Order candidate peers in pick_peer callbacks.

  The order of candidate peers presented to a diameter_app(3) pick_peer callback
  has previously not been documented, but there are use cases that are
  simplified by an ordering. The order is now determined by the filter.

  Own Id: OTP-12308

## diameter 1.7.1

### Fixed Bugs and Malfunctions

- Don't leave extra bit in decoded AVP data.

  An extra bit could be communicated in the data field of a diameter_avp record
  in the case of length errors. Of no consequence for code using the record
  encoding of Diameter messages, but code examining diameter_avp records would
  see this bit.

  Dictionary files must be recompiled for the fix to have effect.

  Own Id: OTP-12074

- Fix counting of outgoing requests and answers setting the E-bit.

  OTP-11721 broke these counters for all outgoing requests except DWR, and
  caused answers setting the E-bit to be counted as unknown messages.

  Own Id: OTP-12080

- Fix Failed-AVP decode.

  The best-effort decode only worked for AVPs in the common dictionary, not for
  those in the dictionary of the application identified in the Diameter Header
  of the answer message in question.

  Failed-AVP in an answer decoded with the RFC 3588 common dictionary
  (diameter_gen_base_rfc3588) was regarded as an error. The RFC 6733 dictionary
  was unaffected.

  Dictionary files must be recompiled for the fix to have effect.

  Own Id: OTP-12094

## diameter 1.7

### Fixed Bugs and Malfunctions

- Improve robustness.

  Counters returned by diameter:service_info/2 now only count messages known to
  the dictionary in question, so that an attacker cannot cause arbitrarily many
  counters to be created.

  Messages to the Erlang log have been minimized, and those related to traffic
  have been removed entirely since an attacker could cause a node to be logged
  to death. Consequently, the default answer_errors configuration has been
  changed from report to discard. A service needs to be restarted for the change
  in default to take effect.

  Own Id: OTP-11721

- Fix request table leak.

  Outgoing Diameter requests are stored in a table until an answer is received
  or times out. Calling diameter:stop_service/1 before this took place would
  orphan the entries, resulting in a memory leak.

  Own Id: OTP-11893

- Fix broken SCTP transport.

  OTP-11593 caused the sending of answer messages over SCTP to fail.

  Own Id: OTP-11901 Aux Id: OTP-11593

- Fix watchdog process leak.

  A failed capabilities exchange on a listening transport would orphan a
  process, causing a memory leak.

  Own Id: OTP-11934

- Fix incorrect handling of incoming DPR.

  In the case of a listening transport, a reconnection by a peer following DPR
  could transition the watchdog state to REOPEN instead of OKAY.

  Own Id: OTP-11938

- Fix handling of AVP length errors on unknown AVPs.

  An AVP (Header) length that pointed past the end of the message was not
  flagged as a 5014 error in this case. Moreover, encoding such an AVP in the
  Failed-AVP of an answer message as a consequence of other errors (eg. M-bit,
  resulting in 5001) failed if the AVP contained a complete header.

  Dictionary files must be recompiled for the fix to have effect.

  Own Id: OTP-11946

- Fix broken check in dictionary compilation.

  That an AVP specified in the content of a @codecs or @custom_types section was
  undefined went undetected, causing compilation to fail when attempting to
  lookup the AVP's type.

  Own Id: OTP-11958

### Improvements and New Features

- Add result code counters for CEA, DWA, and DPA.

  In addition to the existing result code counters on other answer messages.

  Own Id: OTP-11891

- Add best-effort decode of AVPs within Failed-AVP.

  OTP-11007 disabled the decode of AVPs in Failed-AVP since errors could cause
  the decode of Failed-AVP itself to fail. Component AVPs are now decoded if
  possible, otherwise not. AVPs of type Grouped are decoded as much as possible,
  as deeply as possible.

  Dictionary files must be recompiled for the fix to have effect.

  Own Id: OTP-11936 Aux Id: OTP-11007

- Add counters for encode errors in outgoing Diameter messages.

  In addition to the existing counters on decode errors. The latter now count
  independently of result codes in answer messages since decode errors do not
  preclude the presence of a result code.

  Own Id: OTP-11937

## diameter 1.6

### Fixed Bugs and Malfunctions

- Add missing check at dictionary compilation.

  In particular, that an AVP defined as having type Grouped in an @avp_types
  section has a corresponding definition in a @grouped section.

  Own Id: OTP-11561

- Correct documentation on the setting of Origin-State-Id

  It was incorrectly stated that the AVP would be set in an outgoing DPR/DPA.

  Own Id: OTP-11583

- Change interface for communicating outbound stream id to diameter_sctp

  The module uses the transport_data field of record diameter_packet to
  communicate the stream on which the an incoming message is received and on
  which an outgoing message should be sent, the previous interface being that
  both are communicated as a tuple of the form \{stream, Id\}. However, since
  diameter retains the value of an incoming request's transport_data unless the
  corresponding answer message specifies otherwise, the behaviour in this case
  is to send an answer on the outbound stream with the same identifier as the
  that of the inbound stream on which the request was received. If the inbound
  stream id is greater than or equal to the number of outbound streams then this
  is guaranteed to fail, causing the transport process in question to terminate.
  There is no relationship between inbound and outbound stream identifiers so
  diameter_sctp's imposition of one is simply wrong.

  Outbound stream ids are now communicated with a different tuple: \{outstream,
  Id\}, interpreted modulo the number of outbound streams. Thus, retention of an
  inbound request's transport_data has no effect on the selection of an outbound
  stream.

  The change in interface is not strictly backwards compatible because of the
  new atom for the outbound stream. However, as there is currently no documented
  way of obtaining the available number of outbound streams for a peer
  connection, there is no way for a client to have known the range of ids from
  which it could reliably have chosen with the previous interface, so any
  setting of the outbound stream has probably been unintentional. Not explicitly
  specifying an outbound stream now results in a round-robin selection.

  Thanks to Sharmila Pillai for reporting the problem.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11593

- Fix unicode path failure in diameter_make:codec/2.

  A dictionary path containing a unicode codepoint > 255 caused the function to
  fail.

  Own Id: OTP-11655

- Fix 'accept' config to diameter_sctp.

  OTP-10893 added support for \{accept, Match\} tuples to specify addresses or
  regexps that should be matched against peer addresses to decide whether or not
  a newly established association should be retained, but this hasn't been
  functional in the SCTP case because of missing support in inet(3).

  The display of both local and peer addresses in diameter:service_info/2 output
  has also been corrected.

  Own Id: OTP-11661 Aux Id: OTP-10229

- Be lenient with the M-bit in Grouped AVPs.

  RFC 6733 says this, in 4.4:

  - \_\_\_\_ -
    `Receivers of a Grouped AVP that does not have the 'M' (mandatory) bit set and one or more of the encapsulated AVPs within the group has the 'M' (mandatory) bit set MAY simply be ignored if the Grouped AVP itself is unrecognized. The rule applies even if the encapsulated AVP with its 'M' (mandatory) bit set is further encapsulated within other sub-groups, i.e., other Grouped AVPs embedded within the Grouped AVP.`

  The first sentence is mangled but take it to mean this:

  - \_\_\_\_ -
    `An unrecognized AVP of type Grouped that does not set the 'M' bit MAY be ignored even if one of its encapsulated AVPs sets the 'M' bit.`

  This is a bit of a non-statement since if the AVP is unrecognized then its
  type is unknown. We therefore don't know that its data bytes contain
  encapsulated AVPs, so can't but ignore any of those that set the M-bit. Doing
  anything else when the type _is_ known would be inconsistent.

  OTP-11087 (R16B03) caused the M-bit on any unrecognized AVP to be regarded as
  an error, unrecognized being taken to mean "not explicitly defined as a member
  of its container". (That is, an AVP that can't be packed into a dedicated
  record field, which is slightly stronger than "not defined".) This fixed the
  original intention for top-level AVPs but broke the required leniency for
  Grouped AVPs whose type is known. This leniency is now restored.

  Note that dictionary files need to be recompiled for the change to have
  effect.

  Thanks to Rory McKeown for reporting the problem.

  Own Id: OTP-11675 Aux Id: OTP-11087

- Fix pick_peer case clause failure.

  In the case of \{call_mutates_state, true\} configuration on the service in
  question, any peer selection that failed to select a peer resulted in a case
  clause failure. This was noticed in the case of a peer failover in which an
  alternate peer wasn't available.

  Own Id: OTP-11789

## diameter 1.5

### Improvements and New Features

- Rename reconnect_timer to connect_timer.

  The former is still accepted for backwards compatibility, but the name is
  misleading given the semantics of the timer.

  Own Id: OTP-11168

- Extend diameter_make(3).

  Dictionaries can now be compiled from strings, not just filesystem paths, and
  results can be returned instead of written to the filesystem.

  Own Id: OTP-11348

- Remove hardcoding of diameter_base as @prefix on dictionaries for application
  id 0.

  Own Id: OTP-11361

## diameter 1.4.4

### Fixed Bugs and Malfunctions

- Fix setting of End-to-End and Hop-by-Hop Identifiers in outgoing DWA.

  Broken by OTP-11184, which caused the identifiers to be set anew, discarding
  the values from the incoming DWR.

  Own Id: OTP-11367

- Fix handling of 5014, DIAMETER_INVALID_AVP_LENGTH.

  The error was detected as 5004, DIAMETER_INVALID_AVP_VALUE, for some Diameter
  types, in which case an AVP length that pointed past the end of a message
  resulted in encode failure.

  Own Id: OTP-11395

## diameter 1.4.3

### Fixed Bugs and Malfunctions

- Fix UTF8String encode.

  Encode now accepts any nested list of codepoints and binaries. A list
  containing a binary was previously misinterpreted and the documentation was
  incomplete.

  Own Id: OTP-11172

- Ensure DWR isn't sent immediately after DWA.

  This was possible if the timing was unfortunate. An incoming DWR now properly
  resets the watchdog timer.

  Own Id: OTP-11184

- Fix faulty encode of Failed-AVP

  Reception of a CER, DWR or DPR that has decode failures caused encode of the
  corresponding answer message to fail.

  Own Id: OTP-11293

- Fix broken service_opt() spawn_opt.

  The option was ignored.

  Own Id: OTP-11299

## diameter 1.4.2

### Fixed Bugs and Malfunctions

- Fix handling of 5014 (INVALID_AVP_LENGTH) errors.

  This was in some cases reported as 3009 (INVALID_AVP_BITS).

  Note that the correction is partially implemented in modules generated by
  diameterc(1): a dictionary file must be recompiled for the correction to apply
  to any messages it defines.

  Own Id: OTP-11007

- Fix faulty capitalization in release notes.

  Diameter = the protocol.  
  diameter = the Erlang application.

  Own Id: OTP-11014

- Fix watchdog memory leak.

  Entries were not removed from a service-specific ets table, causing them to be
  orphaned at connection reestablishment for listening transports, and
  diameter:remove_transport/2 for both listening and connecting transports.

  The fault was introduced by OTP-10692 in diameter-1.4.1 (R16B).

  Own Id: OTP-11019 Aux Id: OTP-10692

- Fix decode failure on AVP Length < 8.

  The failure caused the message in question to be discarded.

  Own Id: OTP-11026

- Respect Host-IP-Address configuration.

  Addresses returned from a transport module were always used to populate
  Host-IP-Address AVP's in an outgoing CER/CEA, which precluded the sending of a
  VIP address. Transport addresses are now only used if Host-IP-Address is
  unspecified.

  Own Id: OTP-11045

- Fix mkdir race.

  Install could fail if examples/code and examples/dict were created in
  parallel. Noticed on FreeBSD.

  Own Id: OTP-11051

- Fix recognition of 5001 on mandatory AVP's.

  An AVP setting the M-bit was not regarded as erroneous if it was defined in
  the dictionary in question and its container (message or Grouped AVP) had an
  'AVP' field. It's now regarded as a 5001 error (AVP_UNSUPPORTED), as in the
  case that the AVP is not defined.

  Note that the correction is partially implemented in modules generated by
  diameterc(1): a dictionary file must be recompiled for the correction to apply
  to any messages it defines.

  Own Id: OTP-11087

- Fix setting of Failed-AVP on handle_request \{answer_message, 5xxx\} return.

  Failed-AVP was never in the outgoing answer-message. It is now set with the
  AVP from the first entry with the specified Result-Code in the errors field of
  the incoming diameter_packet, if found.

  Own Id: OTP-11092

- Fix watchdog function_clause

  A listening transport on a service that allowed multiple connections to the
  same peer could result in a function_clause error in module diameter_watchdog.
  The resulting crash was harmless but unseemly.

  Thanks to Aleksander Nycz.

  Own Id: OTP-11115

- Fix population of Failed-AVP.

  In cases in which diameter populated this AVP, many values were sent instead
  of one as suggested by RFC 6733. This was partially corrected by OTP-11007.

  Own Id: OTP-11127 Aux Id: OTP-11007

- Fix list-valued Vendor-Specific-Application-Id config

  R16B (specifically, OTP-10760) broke the handling of such configuration,
  resulting in a function clause error if the list was not of length 3, and
  faulty interpretation of the list's contents otherwise. Only record-valued
  configuration was properly interpreted.

  Own Id: OTP-11165

### Improvements and New Features

- Allow peer connections to be shared between Erlang nodes for the purpose of
  sending outgoing requests.

  A diameter_app(3) pick_peer/4 callback gets a list of remote candidates as
  argument, allowing a callback on one node to select a transport connection
  established on another node. The service_opt() share_peers controls the extent
  to which local connections are shared with remote nodes. The service_opt()
  use_shared_peers controls the extent to which connections shared from remote
  nodes are utilized on the local node.

  Own Id: OTP-9610

- Allow listening diameter\_\{tcp,sctp\} transports to be configured with remote
  addresses.

  Option 'accept' allows remote addresses to be configured as tuples or regular
  expressions. Remote addresses are matched against the configured values at
  connection establishment, any non-matching address causing the connection to
  be aborted.

  Own Id: OTP-10893

- Detect more transport_opt() configuration errors at diameter:add_transport/2.

  Many errors would previously not be detected until transport start,
  diameter:add_transport/2 returning 'ok' but transport connections failing to
  be established. An error tuple is now returned.

  Own Id: OTP-10972

- Make explicit local address configuration optional in diameter_tcp:start/3.

  The default address (as determined by gen_tcp) is now used when a local
  address is not explicitly configured.

  Own Id: OTP-10986

- Improve handling of unrecognized service options.

  Such options were silently ignored by diameter:start_service/2. An error tuple
  is now returned.

  Own Id: OTP-11017

- Don't send default Inband-Security-Id in CER/CEA.

  RFC 6733 recommends against the use of Inband-Security-Id. Only send a value
  that differs from the default, NO_INBAND_SECURITY = 0.

  Own Id: OTP-11050

- Make spawn options for request processes configurable.

  Own Id: OTP-11060

## diameter 1.4.1.1

### Fixed Bugs and Malfunctions

- Fix broken Vendor-Specific-Application-Id configuration.

  RFC 6733 changed the definition of this Grouped AVP, changing the arity of
  Vendor-Id from 1\* to 1. A component Vendor-Id can now be either list- or
  integer-valued in service and transport configuration, allowing it to be used
  with both RFC 3588 and RFC 6733 dictionaries.

  Own Id: OTP-10942

### Improvements and New Features

- Add transport_opt() watchdog_config to allow non-standard behaviour of the
  watchdog state machine.

  This can be useful during test but should not be used on nodes that must
  conform to RFC 3539.

  Own Id: OTP-10898

## diameter 1.4.1

### Fixed Bugs and Malfunctions

- Fix erroneous watchdog transition from DOWN to INITIAL.

  This transition took place when a peer connection was reestablished following
  a failed capabilities exchange. RFC 3539 requires DOWN to transition into
  REOPEN.

  Own Id: OTP-10692

### Improvements and New Features

- Add application_opt() request_errors to make the handling of incoming requests
  containing decode errors configurable.

  The value 'callback' ensures that a handle_request callback takes place for
  all such requests, the default being for diameter to answer 3xxx series errors
  itself.

  Own Id: OTP-10686

- Add transport_opt() length_errors.

  The value determines how messages received over the transport interface with
  an incorrect Message Length are dealt with.

  Own Id: OTP-10687

- Add commentary on RFC 6733 to Standards Compliance chapter of the User's
  Guide.

  Own Id: OTP-10688

- Allow a 5xxx result code in an answer-message on peer connections using the
  RFC 6733 common dictionary.

  RFC 6733 allows this while RFC 3588 does not. A handle_request callback can
  return \{answer_message, 3000..3999|5000..5999\} in the simplest case.

  Own Id: OTP-10759

- Add dictionaries for RFC 6733.

  Both the common and accounting dictionaries differ from their RFC 3588
  counterparts, which is reflected in generated record definitions. Application
  configuration on a service or transport determines the dictionary that will be
  used on a given peer connection.

  Own Id: OTP-10760

- Allow a handle_request callback to control diameter's setting of Result-Code
  and Failed-AVP.

  Setting errors = false in a returned #diameter_packet\{\} disables the
  setting.

  Own Id: OTP-10761

## diameter 1.4

### Fixed Bugs and Malfunctions

- Add registered server names to the app file.

  Own Id: OTP-10442

- Fix #diameter_header\{\} handling broken by OTP-10445.

  The fault caused the the header of a \[Header | Avps] request to be ignored if
  both end_to_end_id and hop_by_hop_id were undefined.

  Own Id: OTP-10609

- Fix error handling for handle_request callback.

  A callback that returned a #diameter_packet\{\} would fail if the incoming
  request had decode errors.

  Own Id: OTP-10614

- Fix timing of service start event.

  The event did not necessarily precede other events as documented.

  Own Id: OTP-10618

- Fix setting of header T flag at peer failover.

  The flag is now set in the diameter_header record passed to a
  prepare_retransmit callback.

  Own Id: OTP-10619

- Fix sending of CER/CEA timeout event at capx_timeout.

  The event was not sent as documented.

  Own Id: OTP-10628

- Fix improper setting of Application-ID in the Diameter header of an answer
  message whose E flag is set.

  The value should be that of the request in question. The fault caused it
  always to be 0.

  Own Id: OTP-10655

- Fix faulty handling of AVP length errors.

  An incorrect AVP length but no other errors caused an incoming request to
  fail.

  Own Id: OTP-10693

## diameter 1.3.1

### Known Bugs and Problems

- Fix function clause resulting from use of an eval callback.

  Own Id: OTP-10685

## diameter 1.3

### Fixed Bugs and Malfunctions

- Fix faulty handling of Origin-State-Id and faulty config values.

  The former was expected in a list despite the documentation requiring
  (correctly) an integer. A bare value for a list-valued capability was not
  handled.

  Own Id: OTP-10440

- Fix timing of up/down events.

  Previously, a call to diameter:call/4 following a peer_up callback might
  incorrectly return \{error, no_connection\}, depending on timing. Both events
  now follow the corresponding callbacks.

  Own Id: OTP-10459

- Make diameter:service_info/2 usable in peer_up, peer_down and pick_peer
  callbacks.

  Except for in pick_peer when \{call_mutates_state, false\}, it would
  previously hang indefinitely.

  Own Id: OTP-10460

- Verify that End-to-End and Hop-by-Hop Identifiers in an incoming CEA/DPA match
  those sent in the corresponding CER/DPR.

  The values were previously ignored. Answers whose identifiers do not match are
  handled as unexpected.

  Own Id: OTP-10565

- Fix formatting problems in PDF documentation.

  In particular, text corresponding to links in HTML was omitted in preformatted
  blocks. There are still issues with indentation but this is not
  diameter-specific.

  Own Id: OTP-10583

### Improvements and New Features

- Let prepare_request, prepare_retransmit and handle_request callbacks return a
  function to be invoked on outgoing messages after encode.

  This allows encoded messages to be logged for example.

  Own Id: OTP-10441

- Add service_opt() 'restrict_connections' to allow multiple transport
  connections with the same peer.

  Own Id: OTP-10443

- Add service_opt() 'sequence' to allow the masking of a constant onto the
  topmost bits of End-to-End and Hop-by-Hop identifiers.

  This allows the same service on different nodes to use distinct values in
  outgoing request messages.

  Own Id: OTP-10445

- Add diameter:service_info(PeerRef) to return the transport_ref() and
  transport_opt() list of the corresponding transport.

  This allows easy access to these from diameter_app callbacks that only get
  peer_ref() as an argument.

  Own Id: OTP-10470

- Add reference pages diameter_codec(3) and diameter_make(3).

  Own Id: OTP-10471

- Add events for service start and stop.

  Own Id: OTP-10492

- Add transport_opt() 'disconnect_cb' to make the sending of DPR configurable.

  Whether or not DPR should be sent at application stop, service stop or
  transport removal is determined by the value returned by the callback, as is
  the Disconnect-Cause and timeout if DPA is not received.

  Own Id: OTP-10493

- Add transport_opt() 'capx_timeout' for the timeout associated with
  non-reception of CER/CEA.

  Own Id: OTP-10554

- Allow a handle_request callback to return a #diameter_packet\{\}.

  This allows an answer to set transport_data and header fields.

  Own Id: OTP-10566

- Update documentation for RFC 6733.

  RFC 3588 is now obsolete.

  Own Id: OTP-10568

## diameter 1.2

### Fixed Bugs and Malfunctions

- Fix broken Result-Code setting and Destination-Host/Realm extraction.

  Result-Code was assumed to have arity 1 when setting this value in an answer
  to a request containing AVP decode errors. Destination-Host/Realm were only
  correctly extracted from messages in the common application.

  Own Id: OTP-10202

- Handle insufficient capabilities configuration more gracefully.

  A transport that does not have sufficient capabilities configuration in order
  to encode CER/CEA will now emit an error report noting the configuration error
  and exit instead of failing. The error is not detected at
  diameter:add_transport/2 since there is no requirement that a service be
  configured before its transports.

  Own Id: OTP-10203

- Ensure a failing peer_up/down callback does not affect transport connections
  to other peers.

  Such a failure would previously have taken down all of a service's
  connections.

  Own Id: OTP-10215

### Improvements and New Features

- Statistics related to Diameter messages can be retrieved using
  diameter:service_info/2.

  Both Diameter and socket-level statistics are available, for both incoming and
  outgoing messages.

  Own Id: OTP-9608

- Allow multiple transport_module/config to diameter:add_transport/2.

  Multiple values are attempted in sequence until one results in an established
  connection. This provides a way for a connecting transport to specify
  configuration in order of preference. (For example, SCTP before TCP.)

  Own Id: OTP-9885

- Add events for state transitions in the RFC 3539 watchdog state machine.

  The watchdog state is also available through diameter:service_info/2.

  Own Id: OTP-10212

- Add diameter:service_info(SvcName, connections).

  This provides an alternative to diameter:service_info(SvcName, transport) that
  presents information per established connection instead of per transport
  reference.

  Own Id: OTP-10213

- Assorted documentation corrections/improvements.

  Own Id: OTP-10216

## diameter 1.1

### Fixed Bugs and Malfunctions

- Fix fault in sending of 'closed' events.

  The fault made it possible for the 'closed' event not to be sent following a
  failed capabilities exchange.

  Own Id: OTP-9824

- Fix faulty diameterc -name/-prefix.

  A minor blunder when introducing the new dictionary parser in diameter-1.0
  broke these options.

  Own Id: OTP-9826

## diameter 1.0

### Fixed Bugs and Malfunctions

- Fix faulty cleanup after diameter:remove_transport/2.

  Removing a transport removed the configuration but did not prevent the
  transport process from being restarted.

  Own Id: OTP-9756

### Improvements and New Features

- Add support for TLS over TCP.

  RFC 3588 requires that a Diameter server support TLS. In practice this seems
  to mean TLS over SCTP since there are limitations with running over SCTP: see
  RFC 6083 (DTLS over SCTP), which is a response to RFC 3436 (TLS over SCTP).
  The current RFC 3588 draft acknowledges this by equating TLS with TLS/TCP and
  DTLS/SCTP.

  TLS handshaking can take place either following a CER/CEA that negotiates TLS
  using the Inband-Security-Id AVP (the method documented in RFC 3588) or
  immediately following connection establishment (the method added to the
  current draft).

  Own Id: OTP-9605

- Improvements to the dictionary parser.

  The dictionary parser now emits useful error messages in case of faults in the
  input file, also identifying the line number at which the fault was detected.
  There are semantic checks that were missing in the previous parser, a fault in
  the interpretation of vendor id's in combination with @inherits has been fixed
  and @end can be used to terminate parsing explicitly instead of always parsing
  to end of file.

  Own Id: OTP-9639

- Improve dictionary reusability.

  Reusing a dictionary just to get a different generated module name or prefix
  previously required taking a copy of the source, which may consist of several
  files if inheritance is used, just to edit a couple of lines which don't
  affect the semantics of the Diameter application being defined. Options
  --name, --prefix and --inherits have been added to diameterc to allow
  corresponding values to be set at compile time.

  Own Id: OTP-9641

- Add capabilities_cb transport option.

  Its value is a function that's applied to the transport reference and
  capabilities record after capabilities exchange. If a callback returns
  anything but 'ok' then the connection is closed. In the case of an incoming
  CER, the callback can return a result code with which to answer. Multiple
  callbacks can be specified and are applied until either all return 'ok' or one
  doesn't.

  This provides a way to reject a peer connection.

  Own Id: OTP-9654

- Add @codecs to dictionary format.

  The semantics are similar to @custom_types but results in codec functions of
  the form TypeName(encode|decode, AvpName, Data) rather than
  AvpName(encode|decode, TypeName, Data). That is, the role of the AVP name and
  Diameter type name are reversed. This eliminates the need for exporting one
  function for each AVP sharing a common specialized encode/decode.

  Own Id: OTP-9708 Aux Id: OTP-9639

- Add #diameter_callback\{\} for more flexible callback configuration.

  The record allows individual functions to be configured for each of the
  diameter_app(3) callbacks, as well as a default callback.

  Own Id: OTP-9777

## diameter 0.10

### Fixed Bugs and Malfunctions

- Handle #sctp_paddr_change and #sctp_pdapi_event from gen_sctp.

  The events are enabled by default but diameter_sctp neither disabled nor dealt
  with them. Reception of such an event caused a transport process to crash.

  Own Id: OTP-9538

- Fix header folding bug.

  A prepare_request callback from diameter can return a diameter_header record
  in order to set values in the header of an outgoing request. A fault in
  diameter_lib:fold_tuple/3 caused the subsequent encode of the outgoing request
  to fail.

  Own Id: OTP-9577

- Fix bugs in sending of answer-message replies.

  3001 (DIAMETER_COMMAND_UNSUPPORTED) was not sent since the decode placed the
  AVP list in the wrong field of the diameter_packet, causing the subsequent
  encode to fail. Session-Id was also set improperly, causing encode to fail
  even in this case.

  Own Id: OTP-9578

- Fix improper use of error_logger:info_report/2.

  Function doesn't take a format string and arguments as it was called. Instead
  use error_logger:info_report/1 and use the same report format as used for
  warning and error reports.

  Own Id: OTP-9579

- Fix and clarify semantics of peer filters.

  An eval filter returning a non-true value caused the call process to fail and
  the doc was vague on how an exception was treated. Clarify that the non-tuple
  host/realm filters assume messages of a certain form.

  Own Id: OTP-9580

- Fix and clarify relay behaviour.

  Implicit filtering of the sending peer in relaying a request could cause loop
  detection to be preempted in a manner not specified by RFC3588. Reply with
  3002 (DIAMETER_UNABLE_TO_DELIVER) on anything but an answer to a relayed
  request.

  Own Id: OTP-9583

### Improvements and New Features

- @id required in dictionary files only when @messages is specified.

  @id defines an application identifier and this is used only when sending or
  receiving messages. A dictionary can define only AVP's however, to be included
  by other dictionaries using @inherits, in which case it makes no sense to
  require @id.

  Note that message definitions are not inherited with @inherits, only AVP's

  Own Id: OTP-9467

- Allow @enum when AVP is defined in an inherited dictionary.

  3GPP standards (for one) extend the values allowed for RFC 3588 AVP's of type
  Enumerated. Previously, extending an AVP was only possible by completely
  redefining the AVP.

  Own Id: OTP-9469

- Migrate testsuites to pure common test and add both suites and testcases.

  Own Id: OTP-9553

- Requests of arbitrary form.

  diameter:call/4 can be passed anything, as long as the subsequent
  prepare_request callback returns a term that can be encoded.

  Own Id: OTP-9581

## diameter 0.9

Initial release of the diameter application.

Known issues or limitations:

- Some agent-related functionality is not entirely complete. In particular,
  support for proxy agents, that advertise specific Diameter applications but
  otherwise relay messages in much the same way as relay agents (for which a
  handle_request callback can return a `relay` tuple), will be completed in an
  upcoming release. There may also be more explicit support for redirect agents,
  although redirect behaviour can be implemented with the current functionality.
- There is some asymmetry in the treatment of messages sent as
  `diameter_header/avp` records and those sent in the "normal" fashion, and not
  all of this is documented. This is related to the previous point since this
  form of sending a message was introduced specifically to handle relay agent
  behaviour using the same callback interface as for client/server behaviour.
- The User's Guide is currently quite thin. The introductory chapter followed by
  the examples (in the application `examples` subdirectory) may be sufficient
  for those having some familiarity with the Diameter protocol but the intention
  is to provide more introductory text. The reference documentation is quite
  complete, although some points could likely be expanded upon.
- The function diameter:service_info/2 can be used to retrieve information about
  a started service (statistics, information about connected peers, etc) but
  this is not yet documented and both the input and output may change in the
  next release.

See [Standards Compliance](diameter_soc.md) for standards-related issues.
