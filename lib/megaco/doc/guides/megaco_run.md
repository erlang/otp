<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Running the stack

## Starting

A user may have a number of "virtual" connections to other users. An MG is
connected to at most one MGC, while an MGC may be connected to any number of
MG's. For each connection the user selects a transport service, an encoding
scheme and a user callback module.

An MGC must initiate its transport service in order to listen to MG's trying to
connect. How the actual transport is initiated is outside the scope of this
application. However a send handle (typically a socket id or host and port) must
be provided from the transport service in order to enable us to send the message
to the correct destination. We do however not assume anything about this, from
our point of view, opaque handle. Hopefully it is rather small since it will
passed around the system between processes rather frequently.

A user may either be statically configured in a .config file according to the
application concept of Erlang/OTP or dynamically started with the configuration
settings as arguments to megaco:start_user/2. These configuration settings may
be updated later on with megaco:update_conn_info/2.

The function megaco:connect/4 is used to tell the Megaco application about which
control process it should supervise, which MID the remote user has, which
callback module it should use to send messages etc. When this "virtual"
connection is established the user may use megaco:call/3 and megaco:cast/3 in
order to send messages to the other side. Then it is up to the MG to send its
first Service Change Request message after applying some clever algorithm in
order to fight the problem with startup avalanche (as discussed in the RFC).

The originating user will wait for a reply or a timeout (defined by the
request_timer). When it receives the reply this will optionally be acknowledged
(regulated by auto_ack), and forwarded to the user. If an interim pending reply
is received, the long_request_timer will be used instead of the usual
request_timer, in order to enable avoidance of spurious re-sends of the request.

On the destination side the transport service waits for messages. Each message
is forwarded to the Megaco application via the megaco:receive_message/4 callback
function. The transport service may or may not provide means for blocking and
unblocking the reception of the incoming messages.

If a message is received before the "virtual" connection has been established,
the connection will be setup automatically. An MGC may be real open minded and
dynamically decide which encoding and transport service to use depending on how
the transport layer contact is performed. For IP transports two ports are
standardized, one for textual encoding and one for binary encoding. If for
example an UDP packet was received on the text port it would be possible to
decide encoding and transport on the fly.

After decoding a message various user callback functions are invoked in order to
allow the user to act properly. See the megaco_user module for more info about
the callback arguments.

When the user has processed a transaction request in its callback function, the
Megaco application assembles a transaction reply, encodes it using the selected
encoding module and sends the message back by invoking the callback function:

- SendMod:send_message(SendHandle, ErlangBinary)

Re-send of messages, handling pending transactions, acknowledgements etc. is
handled automatically by the Megaco application but the user is free to override
the default behaviour by the various configuration possibilities. See
megaco:update_user_info/2 and megaco:update_conn_info/2 about the possibilities.

When connections gets broken (that is explicitly by megaco:disconnect/2 or when
its controlling process dies) a user callback function is invoked in order to
allow the user to re-establish the connection. The internal state of kept
messages, re-send timers etc. is not affected by this. A few re-sends will of
course fail while the connection is down, but the automatic re-send algorithm
does not bother about this and eventually when the connection is up and running
the messages will be delivered if the timeouts are set to be long enough. The
user has the option of explicitly invoking megaco:cancel/2 to cancel all
messages for a connection.

[](){: #mgc_startup_call_flow }

## MGC startup call flow

In order to prepare the MGC for the reception of the initial message, hopefully
a Service Change Request, the following needs to be done:

- Start the Megaco application.
- Start the MGC user. This may either be done explicitly with
  megaco:start_user/2 or implicitly by providing the -megaco users configuration
  parameter.
- Initiate the transport service and provide it with a receive handle obtained
  from megaco:user_info/2.

When the initial message arrives the transport service forwards it to the
protocol engine which automatically sets up the connection and invokes
UserMod:handle_connect/2 before it invokes UserMod:handle_trans_request/3 with
the Service Change Request like this:

![MGC Startup Call Flow](assets/MGC_startup_call_flow.gif "MGC Startup Call Flow")

[](){: #mg_startup_call_flow }

## MG startup call flow

In order to prepare the MG for the sending of the initial message, hopefully a
Service Change Request, the following needs to be done:

- Start the Megaco application.
- Start the MG user. This may either be done explicitly with megaco:start_user/2
  or implicitly by providing the -megaco users configuration parameter.
- Initiate the transport service and provide it with a receive handle obtained
  from megaco:user_info/2.
- Setup a connection to the MGC with megaco:connect/4 and provide it with a
  receive handle obtained from megaco:user_info/2.

If the MG has been provisioned with the MID of the MGC it can be given as the
RemoteMid parameter to megaco:connect/4 and the call flow will look like this:

![MG Startup Call Flow](assets/MG_startup_call_flow.gif "MG Startup Call Flow")

If the MG cannot be provisioned with the MID of the MGC, the MG can use the atom
'preliminary_mid' as the RemoteMid parameter to megaco:connect/4 and the call
flow will look like this:

![MG Startup Call Flow (no MID)](assets/MG-startup_flow_noMID.gif "MG Startup Call Flow (no MID)")

[](){: #config_megaco }

## Configuring the Megaco stack

There are three kinds of configuration:

- User info - Information related to megaco users. Read/Write.

  A User is an entity identified by a MID, e.g. a MGC or a MG.

  This information can be retrieved using
  [megaco:user_info](`m:megaco#user_info`).

- Connection info - Information regarding connections. Read/Write.

  This information can be retrieved using
  [megaco:conn_info](`m:megaco#conn_info`).

- System info - System wide information. Read only.

  This information can be retrieved using
  [megaco:system_info](`m:megaco#system_info`).

[](){: #initial_config }

## Initial configuration

The initial configuration of the Megaco should be defined in the Erlang system
configuration file. The following configured parameters are defined for the
Megaco application:

- `users = [{Mid, [user_config()]}].`

  Each user is represented by a tuple with the Mid of the user and a list of
  config parameters (each parameter is in turn a tuple: `{Item, Value}`).

- `scanner = flex | {Module, Function, Arguments, Modules}`

  - `flex` will result in the start of the flex scanner with default options.
  - The MFA alternative makes it possible for Megaco to start and supervise a
    scanner written by the user (see `supervisor:start_child` for an explanation
    of the parameters).

See also
[Configuration of text encoding module(s)](megaco_encode.md#text_config) for
more info.

[](){: #changing_config }

## Changing the configuration

The configuration can be changed during runtime. This is done with the functions
[megaco:update_user_info](`m:megaco#update_user_info`) and
[megaco:update_conn_info](`m:megaco#update_conn_info`)

[](){: #transaction_sender }

## The transaction sender

The transaction sender is a process (one per connection), which handle all
transaction sending, if so configured (see
[megaco:user_info](`m:megaco#user_info`) and
[megaco:conn_info](`m:megaco#conn_info`)).

The purpose of the transaction sender is to accumulate transactions for a more
efficient message sending. The transactions that are accumulated are transaction
request and transaction ack. For transaction ack's the benefit is quite large,
since the transactions are small and it is possible to have ranges (which means
that transaction acks for transactions 1, 2, 3 and 4 can be sent as a range 1-4
in one transaction ack, instead of four separate transactions).

There are a number of configuration parameter's that control the operation of
the transaction sender. In principle, a message with everything stored (ack's
and request's) is sent from the process when:

- When `trans_timer` expires.
- When `trans_ack_maxcount` number of ack's has been received.
- When `trans_req_maxcount` number of requests's has been received.
- When the size of all received requests exceeds `trans_req_maxsize`.
- When a reply transaction is sent.
- When a pending transaction is sent.

When something is to be sent, everything is packed into one message, unless the
trigger was a reply transaction and the added size of the reply and all the
requests is greater then `trans_req_maxsize`, in which case the stored
transactions are sent first in a separate message and the reply in another
message.

When the transaction sender receives a request which is already "in storage"
(indicated by the transaction id) it is assumed to be a resend and everything
stored is sent. This could happen if the values of the `trans_timer` and the
`request_timer` is not properly chosen.

[](){: #segment_reply }

## Segmentation of transaction replies

In version 3 of the megaco standard, the concept of `segmentation package` was
introduced. Simply, this package defines a procedure to segment megaco messages
(transaction replies) when using a transport that does not automatically do this
(e.g. UDP).

Although it would be both pointless and counterproductive to use segmentation on
a transport that already does this (e.g. TCP), the megaco application does not
check this. Instead, it is up to the user to configure this properly.

- Receiving segmented messages:

  This is handled automatically by the megaco application. There is however one
  thing that need to be configured by the user, the
  [segment_recv_timer](`m:megaco#user_info`) option.

  Note that the segments are delivered to the user differently depending on
  which function is used to issue the original request. When issuing the request
  using the [megaco:cast](`m:megaco#cast`) function, the segments are delivered
  to the user via the [handle_trans_reply](`m:megaco_user#trans_reply`) callback
  function one at a time, as they arrive. But this obviously doe not work for
  the [megaco:call](`m:megaco#call`) function. In this case, the segments are
  accumulated and then delivered all at once as the function returns.

- Sending segmented messages:

  This is also handled automatically by the megaco application. First of all,
  segmentation is only attempted if so configured, see the
  [segment_send](`m:megaco#user_info`) option. Secondly, megaco relies on the
  ability of the used codec to encode action replies, which is the smallest
  component the megaco application handles when segmenting. Thirdly, the reply
  will be segmented only if the sum of the size of the action replies (plus an
  arbitrary message header size) are greater then the specified max message size
  (see the [max_pdu_size](`m:megaco#user_info`) option). Finally, if
  segmentation is decided, then each action reply will make up its own (segment)
  message.
