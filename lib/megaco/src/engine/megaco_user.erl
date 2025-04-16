%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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

%%
%%-------------------------------------------------------------------------
%% Purpose: Megaco user behaviour module
%%
%% These callback functions are the default!
%% Its possible for the user to provide a arbitrary number of "extra"
%% arguments via the user_args config option.
%% So, for instance, the handle_connect/2 could instead become
%% handle_connect/4 if the user sets the user_args option to [foo, bar].
%% This means that its impossible to define a proper behaviour.
%% So what we do here is to define a behaviour with the "default interface"
%% (the user_args option has the [] as the default value) and set them
%% all to be optional!
%%-------------------------------------------------------------------------

-module(megaco_user).
-moduledoc """
Callback module for users of the Megaco application

This module defines the callback behaviour of Megaco users. A megaco_user
compliant callback module must export the following functions:

- [handle_connect/2,3](`c:handle_connect/3`)
- [handle_disconnect/3](`c:handle_disconnect/3`)
- [handle_syntax_error/3,4](`c:handle_syntax_error/4`)
- [handle_message_error/3,4](`c:handle_message_error/4`)
- [handle_trans_request/3,4](`c:handle_trans_request/4`)
- [handle_trans_long_request/3,4](`c:handle_trans_long_request/4`)
- [handle_trans_reply/4,5](`c:handle_trans_reply/5`)
- [handle_trans_ack/4,5](`c:handle_trans_ack/5`)
- [handle_unexpected_trans/3,4](`c:handle_unexpected_trans/4`)
- [handle_trans_request_abort/4,5](`c:handle_trans_request_abort/5`)
- [handle_segment_reply/5,6](`c:handle_segment_reply/6`)

The semantics of them and their exact signatures are explained below.

The `user_args` configuration parameter which may be used to extend the argument
list of the callback functions. For example, the handle_connect function takes
by default two arguments:

```
handle_connect(Handle, Version)
```

but if the `user_args` parameter is set to a longer list, such as
`[SomePid,SomeTableRef]`, the callback function is expected to have these (in
this case two) extra arguments last in the argument list:

```erlang
handle_connect(Handle, Version, SomePid, SomeTableRef)
```

[](){: #extra_argument }

> #### Note {: .info }
>
> Must of the functions below has an optional `Extra` argument (e.g.
> [handle_unexpected_trans/4](`c:handle_unexpected_trans/4`)). The functions
> which takes this argument will be called if and only if one of the functions
> [`receive_message/5`](`megaco:receive_message/5`) or
> [`process_received_message/5`](`megaco:process_received_message/5`) was called
> with the `Extra` argument different than `ignore_extra`.

## DATA TYPES

```erlang
action_request() = #'ActionRequest'{}
action_reply() = #'ActionReply'{}
error_desc() = #'ErrorDescriptor'{}
segment_no() = integer()
```

```erlang
conn_handle() = #megaco_conn_handle{}
```

The record initially returned by `megaco:connect/4,5`. It identifies a "virtual"
connection and may be reused after a reconnect (disconnect + connect).

```text
protocol_version() = integer()
```

Is the actual protocol version. In most cases the protocol version is retrieved
from the processed message, but there are exceptions:

- When `handle_connect/2,3` is triggered by an explicit call to
  `megaco:connect/4,5`.
- [`handle_disconnect/3`](`c:handle_disconnect/3`)
- [`handle_syntax_error/3`](`c:handle_syntax_error/3`)

In these cases, the ProtocolVersion default version is obtained from the static
connection configuration:

- `megaco:conn_info(ConnHandle, protocol_version)`.

""".

-export_type([
              receive_handle/0,
              conn_handle/0,
              megaco_timer/0
             ]).

-include_lib("megaco/include/megaco.hrl").
%% -include_lib("megaco/include/megaco_message_v1.hrl").

-type receive_handle() :: #megaco_receive_handle{}.
-type conn_handle()    :: #megaco_conn_handle{}.
-type megaco_timer()   :: infinity | non_neg_integer() | #megaco_incr_timer{}.

-doc(#{equiv => handle_connect/3}).
-callback handle_connect(ConnHandle, ProtocolVersion) -> 
    ok | error | {error, ErrorDescr} when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ErrorDescr      :: megaco_encoder:error_desc().
-doc """
Invoked when a new connection is established

Connections may either be established by an explicit call to megaco:connect/4 or
implicitly at the first invocation of megaco:receive_message/3.

Normally a Media Gateway (MG) connects explicitly while a Media Gateway
Controller (MGC) connects implicitly.

At the Media Gateway Controller (MGC) side it is possible to reject a connection
request (and send a message error reply to the gateway) by returning
`{error, ErrorDescr}` or simply `error` which generates an error descriptor with
code 402 (unauthorized) and reason "Connection refused by user" (this is also
the case for all unknown results, such as exit signals or throw).

See [note](#extra_argument) above about the `Extra` argument in
[`handle_message_error/4`](`c:handle_message_error/4`).

[`handle_connect/3`](`c:handle_connect/3`) (with `Extra`) can also be called as
a result of a call to the `megaco:connect/5` function (if
that function is called with the `Extra` argument different than `ignore_extra`.
""".
-callback handle_connect(ConnHandle, ProtocolVersion, Extra) -> 
    ok | error | {error, ErrorDescr} when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Extra           :: term(),
      ErrorDescr      :: megaco_encoder:error_desc().

-doc """
Invoked when a connection is teared down

The disconnect may either be made explicitly by a call to megaco:disconnect/2 or
implicitly when the control process of the connection dies.
""".
-callback handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    megaco:void() when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Reason          :: term().

-doc(#{equiv => handle_syntax_error/4}).
-callback handle_syntax_error(ReceiveHandle, ProtocolVersion, DefaultED) -> 
    reply | {reply, ED} | no_reply | {no_reply, ED} when
      ReceiveHandle   :: receive_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      DefaultED       :: megaco_encoder:error_desc(),
      ED              :: megaco_encoder:error_desc().
-doc """
Invoked when a received message had syntax errors

Incoming messages is delivered by megaco:receive_message/4 and normally decoded
successfully. But if the decoding failed this function is called in order to
decide if the originator should get a reply message (reply) or if the reply
silently should be discarded (no_reply).

Syntax errors are detected locally on this side of the protocol and may have
many causes, e.g. a malfunctioning transport layer, wrong encoder/decoder
selected, bad configuration of the selected encoder/decoder etc.

The error descriptor defaults to `DefaultED`, but can be overridden with an
alternate one by returning `{reply,ED}` or `{no_reply,ED}` instead of `reply`
and `no_reply` respectively.

Any other return values (including exit signals or throw) and the `DefaultED`
will be used.

See [note](#extra_argument) above about the `Extra` argument in
[`handle_syntax_error/4`](`c:handle_syntax_error/4`).

""".
-callback handle_syntax_error(ReceiveHandle, ProtocolVersion, DefaultED, Extra) -> 
    reply | {reply, ED} | no_reply | {no_reply, ED} when
      ReceiveHandle   :: receive_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      DefaultED       :: megaco_encoder:error_desc(),
      ED              :: megaco_encoder:error_desc(),
      Extra           :: term().

-doc(#{equiv => handle_message_error/4}).
-callback handle_message_error(ConnHandle, ProtocolVersion, ErrorDescr) ->
    megaco:void() when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ErrorDescr      :: megaco_encoder:error_desc().
-doc """
Invoked when a received message just contains an error instead of a list of
transactions.

Incoming messages is delivered by megaco:receive_message/4 and successfully
decoded. Normally a message contains a list of transactions, but it may instead
contain an ErrorDescriptor on top level of the message.

Message errors are detected remotely on the other side of the protocol. And you
probably don't want to reply to it, but it may indicate that you have
outstanding transactions that not will get any response (request -> reply; reply
-> ack).

See [note](#extra_argument) above about the `Extra` argument in
[`handle_message_error/4`](`c:handle_message_error/4`).

""".
-callback handle_message_error(ConnHandle, ProtocolVersion, ErrorDescr, Extra) ->
    megaco:void() when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ErrorDescr      :: megaco_encoder:error_desc(),
      Extra           :: term().

-doc(#{equiv => handle_trans_request/4}).
-callback handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests) ->
    Pending | Reply | ignore_trans_request when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ActionRequests  :: [megaco_encoder:action_request()],
      Pending         :: {pending, ReqData},
      ReqData         :: term(),
      Reply           :: {AckAction, ActualReply} |
                         {AckAction, ActualReply, SendOptions},
      AckAction       :: discard_ack |
                         {handle_ack,         AckData} |
                         {handle_pending_ack, AckData} |
                         {handle_sloppy_ack,  AckData},
      ActualReply     :: [megaco_encoder:action_reply()] |
                         megaco_encoder:error_desc(),
      AckData         :: term(),
      SendOptions     :: [SendOption],
      SendOption      :: {reply_timer,      megaco_timer()} |
                         {send_handle,      term()} |
                         {protocol_version, integer()}.
-doc """
Invoked for each transaction request

Incoming messages is delivered by megaco:receive_message/4 and successfully
decoded. Normally a message contains a list of transactions and this function is
invoked for each TransactionRequest in the message.

This function takes a list of 'ActionRequest' records and has three main
options:

- **`Return ignore_trans_request`** - Decide that these action requests shall be
  ignored completely.

- **`Return pending()`** - Decide that the processing of these action requests
  will take a long time and that the originator should get an immediate
  'TransactionPending' reply as interim response. The actual processing of these
  action requests instead should be delegated to the the
  handle_trans_long_request/3 callback function with the req_data() as one of
  its arguments.

- **`Return reply()`** - Process the action requests and either return an
  error_descr() indicating some fatal error or a list of action replies
  (wildcarded or not).

  If for some reason megaco is unable to deliver the reply, the reason for this
  will be passed to the user via a call to the callback function
  [handle_trans_ack](`c:handle_trans_ack/5`), unless
  `ack_action() = discard_ack`.

  The ack_action() is either:

  - **`discard_ack`** - Meaning that you don't care if the reply is acknowledged
    or not.

  - **`{handle_ack, ack_data()} | {handle_ack, ack_data(), send_options()}`** -
    Meaning that you want an immediate acknowledgement when the other part
    receives this transaction reply. When the acknowledgement eventually is
    received, the handle_trans_ack/4 callback function will be invoked with the
    ack_data() as one of its arguments. ack_data() may be any Erlang term.

  - **`{handle_pending_ack, ack_data()} | {handle_pending_ack, ack_data(), send_options()}`** -
    This has the same effect as the above, _if and only if_ megaco has sent at
    least one pending message for this request (during the processing of the
    request). If no pending message has been sent, then immediate
    acknowledgement will _not_ be requested.

    Note that this only works as specified if the `sent_pending_limit` config
    option has been set to an integer value.

  - **`{handle_sloppy_ack, ack_data()}| {handle_sloppy_ack, ack_data(), send_options()}`** -
    Meaning that you want an acknowledgement _sometime_. When the
    acknowledgement eventually is received, the handle_trans_ack/4 callback
    function will be invoked with the ack_data() as one of its arguments.
    ack_data() may be any Erlang term.

Any other return values (including exit signals or throw) will result in an
error descriptor with code 500 (internal gateway error) and the module name (of
the callback module) as reason.

See [note](#extra_argument) above about the `Extra` argument in
[`handle_trans_request/4`](`c:handle_trans_request/4`).

""".
-callback handle_trans_request(ConnHandle,
                               ProtocolVersion,
                               ActionRequests,
                               Extra) ->
    Pending | Reply | ignore_trans_request when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ActionRequests  :: [megaco_encoder:action_request()],
      Extra           :: term(),
      Pending         :: {pending, ReqData},
      ReqData         :: term(),
      Reply           :: {AckAction, ActualReply} |
                         {AckAction, ActualReply, SendOptions},
      AckAction       :: discard_ack |
                         {handle_ack,         AckData} |
                         {handle_pending_ack, AckData} |
                         {handle_sloppy_ack,  AckData},
      ActualReply     :: [megaco_encoder:action_reply()] |
                         megaco_encoder:error_desc(),
      AckData         :: term(),
      SendOptions     :: [SendOption],
      SendOption      :: {reply_timer,      megaco_timer()} |
                         {send_handle,      term()} |
                         {protocol_version, integer()}.

-doc(#{equiv => handle_trans_long_request/4}).
-callback handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData) ->
    Reply when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ReqData         :: term(),
      Reply           :: {AckAction, ActualReply} |
                         {AckAction, ActualReply, SendOptions},
      AckAction       :: discard_ack |
                         {handle_ack, AckData} |
                         {handle_sloppy_ack, AckData},
      ActualReply     :: [megaco_encoder:action_reply()] |
                         megaco_encoder:error_desc(),
      AckData         :: term(),
      SendOptions     :: [SendOption],
      SendOption      :: {reply_timer, megaco_timer()} |
                         {send_handle, term()} |
                         {protocol_version, megaco_encoder:protocol_version()}.
-doc """
Optionally invoked for a time consuming transaction request

If this function gets invoked or not is controlled by the reply from the
preceding call to handle_trans_request/3. The handle_trans_request/3 function
may decide to process the action requests itself or to delegate the processing
to this function.

The req_data() argument to this function is the Erlang term returned by
handle_trans_request/3.

Any other return values (including exit signals or throw) will result in an
error descriptor with code 500 (internal gateway error) and the module name (of
the callback module) as reason.

See [note](#extra_argument) above about the `Extra` argument in
[`handle_trans_long_request/4`](`c:handle_trans_long_request/4`).

""".
-callback handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData, Extra) ->
    Reply when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ReqData         :: term(),
      Extra           :: term(),
      Reply           :: {AckAction, ActualReply} |
                         {AckAction, ActualReply, SendOptions},
      AckAction       :: discard_ack |
                         {handle_ack, AckData} |
                         {handle_sloppy_ack, AckData},
      ActualReply     :: [megaco_encoder:action_reply()] |
                         megaco_encoder:error_desc(),
      AckData         :: term(),
      SendOptions     :: [SendOption],
      SendOption      :: {reply_timer, megaco_timer()} |
                         {send_handle, term()} |
                         {protocol_version, megaco_encoder:protocol_version()}.

-doc(#{equiv => handle_trans_reply/5}).
-callback handle_trans_reply(ConnHandle,
                             ProtocolVersion,
                             UserReply,
                             ReplyData) ->
    ok when
      ConnHandle           :: conn_handle(),
      ProtocolVersion      :: megaco_encoder:protocol_version(),
      UserReply            :: Success | Failure,
      ReplyData            :: term(),
      Success              :: {ok, Result},
      Result               :: TransactionResult | SegmentResult,
      TransactionResult    :: [megaco_encoder:action_reply()],
      SegmentResult        :: {megaco_encoder:segment_no(),
                               LastSegment,
                               [megaco_encoder:action_reply()]},
      Failure              :: {error, Reason} |
                              {error, ReplyNo, Reason},
      Reason               :: TransactionReason |
                              SegmentReason |
                              UserCancelReason |
                              SendReason |
                              OtherReason,
      TransactionReason    :: megaco_encoder:error_desc(),
      SegmentReason        :: {megaco_encoder:segment_no(),
                               LastSegment,
                               megaco_encoder:error_desc()},
      OtherReason          :: timeout |
                              {segment_timeout, MissingSegments} |
                              exceeded_recv_pending_limit | term(),
      LastSegment          :: boolean(),
      MissingSegments      :: [megaco_encoder:segment_no()],
      UserCancelReason     :: {user_cancel, ReasonForUserCancel},
      ReasonForUserCancel  :: term(),
      SendReason           :: SendCancelledReason | SendFailedReason,
      SendCancelledReason  :: {send_message_cancelled,
                               ReasonForSendCancel},
      ReasonForSendCancel  :: term(),
      SendFailedReason     :: {send_message_failed, ReasonForSendFailure},
      ReasonForSendFailure :: term(),
      ReplyNo              :: pos_integer().
-doc """
Optionally invoked for a transaction reply

The sender of a transaction request has the option of deciding, whether the
originating Erlang process should synchronously wait (`megaco:call/3`) for a
reply or if the message should be sent asynchronously (`megaco:cast/3`) and the
processing of the reply should be delegated this callback function.

Note that if the reply is segmented (split into several smaller messages;
segments), then some extra info, segment number and an indication if all
segments of a reply has been received or not, is also included in the
`UserReply`.

The `ReplyData` defaults to `megaco:lookup(ConnHandle, reply_data)`, but may be
explicitly overridden by a `megaco:cast/3` option in order to forward info about
the calling context of the originating process.

At `success()`, the `UserReply` either contains:

- A list of 'ActionReply' records possibly containing error indications.
- A tuple of size three containing: the segment number, the
  `last segment indicator` and finally a list of 'ActionReply' records possibly
  containing error indications. This is of course only possible if the reply was
  segmented.

`failure()` indicates an local or external error and can be one of the
following:

- A `transaction_reason()`, indicates that the remote user has replied with an
  explicit transactionError.
- A `segment_reason()`, indicates that the remote user has replied with an
  explicit transactionError for this segment. This is of course only possible if
  the reply was segmented.
- A `user_cancel_reason()`, indicates that the request has been canceled by the
  user. `reason_for_user_cancel()` is the reason given in the call to the
  [cancel](`megaco:cancel/2`) function.
- A `send_reason()`, indicates that the transport module
  [send_message](`c:megaco_transport:send_message/3`) function did not send the
  message. The reason for this can be:

  - `send_cancelled_reason()` \- the message sending was deliberately cancelled.
    `reason_for_send_cancel()` is the reason given in the `cancel` return from
    the [send_message](`c:megaco_transport:send_message/3`) function.
  - `send_failed_reason()` \- an error occurred while attempting to send the
    message.

- An `other_reason()`, indicates some other error such as:

  - `timeout` \- the reply failed to arrive before the request timer expired.
  - `{segment_timeout, missing_segments()}` \- one or more segments was not
    delivered before the expire of the segment timer.
  - `exceeded_recv_pending_limit` \- the pending limit was exceeded for this
    request.

See [note](#extra_argument) above about the `Extra` argument in
[`handle_trans_reply/5`](`c:handle_trans_reply/5`).

""".
-callback handle_trans_reply(ConnHandle,
                             ProtocolVersion,
                             UserReply,
                             ReplyData,
                             Extra) ->
    ok when
      ConnHandle           :: conn_handle(),
      ProtocolVersion      :: megaco_encoder:protocol_version(),
      UserReply            :: Success | Failure,
      ReplyData            :: term(),
      Extra                :: term(),
      Success              :: {ok, Result},
      Result               :: TransactionResult | SegmentResult,
      TransactionResult    :: [megaco_encoder:action_reply()],
      SegmentResult        :: {megaco_encoder:segment_no(),
                               LastSegment,
                               [megaco_encoder:action_reply()]},
      Failure              :: {error, Reason} |
                              {error, ReplyNo, Reason},
      Reason               :: TransactionReason |
                              SegmentReason |
                              UserCancelReason |
                              SendReason |
                              OtherReason,
      TransactionReason    :: megaco_encoder:error_desc(),
      SegmentReason        :: {megaco_encoder:segment_no(),
                               LastSegment,
                               megaco_encoder:error_desc()},
      OtherReason          :: timeout |
                              {segment_timeout, MissingSegments} |
                              exceeded_recv_pending_limit | term(),
      LastSegment          :: boolean(),
      MissingSegments      :: [megaco_encoder:segment_no()],
      UserCancelReason     :: {user_cancel, ReasonForUserCancel},
      ReasonForUserCancel  :: term(),
      SendReason           :: SendCancelledReason | SendFailedReason,
      SendCancelledReason  :: {send_message_cancelled,
                               ReasonForSendCancel},
      ReasonForSendCancel  :: term(),
      SendFailedReason     :: {send_message_failed, ReasonForSendFailure},
      ReasonForSendFailure :: term(),
      ReplyNo              :: pos_integer().


-doc(#{equiv => handle_trans_ack/5}).
-callback handle_trans_ack(ConnHandle,
                           ProtocolVersion,
                           AckStatus,
                           AckData) ->
    ok when
      ConnHandle           :: conn_handle(),
      ProtocolVersion      :: megaco_encoder:protocol_version(),
      AckStatus            :: ok | {error, Reason},
      AckData              :: term(),
      Reason               :: UserCancelReason | SendReason | OtherReason,
      UserCancelReason     :: {user_cancel, ReasonForUserCancel},
      ReasonForUserCancel  :: term(),
      SendReason           :: SendCancelledReason | SendFailedReason,
      SendCancelledReason  :: {send_message_cancelled, ReasonForSendCancel},
      ReasonForSendCancel  :: term(),
      SendFailedReason     :: {send_message_failed, ReasonForSendFailure},
      ReasonForSendFailure :: term(),
      OtherReason          :: term().
-doc """
Optionally invoked for a transaction acknowledgement

If this function gets invoked or not, is controlled by the reply from the
preceding call to handle_trans_request/3. The handle_trans_request/3 function
may decide to return \{handle_ack, ack_data()\} or \{handle_sloppy_ack,
ack_data()\} meaning that you need an immediate acknowledgement of the reply and
that this function should be invoked to handle the acknowledgement.

The ack_data() argument to this function is the Erlang term returned by
handle_trans_request/3.

If the AckStatus is ok, it is indicating that this is a true acknowledgement of
the transaction reply.

If the AckStatus is \{error, Reason\}, it is an indication that the
acknowledgement or even the reply (for which this is an acknowledgement) was not
delivered, but there is no point in waiting any longer for it to arrive. This
happens when:

- **`reply_timer`** - The `reply_timer` eventually times out.

- **reply send failure** - When megaco fails to send the reply (see
  [handle_trans_reply](`c:handle_trans_reply/5`)), for whatever reason.

- **cancel** - The user has explicitly cancelled the wait (megaco:cancel/2).

See [note](#extra_argument) above about the `Extra` argument in
[`handle_trans_ack/5`](`c:handle_trans_ack/5`).

""".
-callback handle_trans_ack(ConnHandle,
                           ProtocolVersion,
                           AckStatus,
                           AckData,
                           Extra) ->
    ok when
      ConnHandle           :: conn_handle(),
      ProtocolVersion      :: megaco_encoder:protocol_version(),
      AckStatus            :: ok | {error, Reason},
      AckData              :: term(),
      Extra                :: term(),
      Reason               :: UserCancelReason | SendReason | OtherReason,
      UserCancelReason     :: {user_cancel, ReasonForUserCancel},
      ReasonForUserCancel  :: term(),
      SendReason           :: SendCancelledReason | SendFailedReason,
      SendCancelledReason  :: {send_message_cancelled, ReasonForSendCancel},
      ReasonForSendCancel  :: term(),
      SendFailedReason     :: {send_message_failed, ReasonForSendFailure},
      ReasonForSendFailure :: term(),
      OtherReason          :: term().

-doc(#{equiv => handle_unexpected_trans/4}).
-callback handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Trans           :: megaco_encoder:transaction_pending() |
                         megaco_encoder:transaction_reply() |
                         megaco_encoder:transaction_response_ack().
-doc """
Invoked when a unexpected message is received

If a reply to a request is not received in time, the megaco stack removes all
info about the request from its tables. If a reply should arrive after this has
been done the app has no way of knowing where to send this message. The message
is delivered to the "user" by calling this function on the local node (the node
which has the link).

See [note](#extra_argument) above about the `Extra` argument in
[`handle_unexpected_trans/4`](`c:handle_unexpected_trans/4`).

""".
-callback handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans, Extra) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Trans           :: megaco_encoder:transaction_pending() |
                         megaco_encoder:transaction_reply() |
                         megaco_encoder:transaction_response_ack(),
      Extra           :: term().

-doc(#{equiv => handle_trans_request_abort/5}).
-callback handle_trans_request_abort(ConnHandle,
                                     ProtocolVersion,
                                     TransNo,
                                     Pid) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      TransNo         :: integer(),
      Pid             :: undefined | pid().
-doc """
Invoked when a transaction request has been aborted

This function is invoked if the originating pending limit has been exceeded.
This usually means that a request has taken abnormally long time to complete.

See [note](#extra_argument) above about the `Extra` argument in
[`handle_trans_request_abort/5`](`c:handle_trans_request_abort/5`).

""".
-callback handle_trans_request_abort(ConnHandle,
                                     ProtocolVersion,
                                     TransNo,
                                     Pid,
                                     Extra) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      TransNo         :: integer(),
      Pid             :: undefined | pid(),
      Extra           :: term().

-doc(#{equiv => handle_segment_reply/6}).
-callback handle_segment_reply(ConnHandle,
                               ProtocolVersion,
                               TransNo,
                               SegNo,
                               SegCompl) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      TransNo         :: integer(),
      SegNo           :: integer(),
      SegCompl        :: asn1_NOVALUE | 'NULL'.
-doc """
This function is called when a segment reply has been received if the
[segment_reply_ind](`megaco:conn_info/2`) config option has been set to true.

This is in effect a progress report.

See [note](#extra_argument) above about the `Extra` argument in
[`handle_segment_reply/6`](`c:handle_segment_reply/6`).
""".
-callback handle_segment_reply(ConnHandle,
                               ProtocolVersion,
                               TransNo,
                               SegNo,
                               SegCompl,
                               Extra) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      TransNo         :: integer(),
      SegNo           :: megaco_encoder:segment_no(),
      SegCompl        :: asn1_NOVALUE | 'NULL',
      Extra           :: term().

-optional_callbacks(
   [
    %% The actual number of arguments to *all* functions,
    %% depend of the user_args config option.
    handle_connect/2,
    handle_connect/3,
    handle_disconnect/3,
    handle_syntax_error/3,
    handle_syntax_error/4,
    handle_message_error/3,
    handle_message_error/4,
    handle_trans_request/3,
    handle_trans_request/4,
    handle_trans_long_request/3,
    handle_trans_long_request/4,
    handle_trans_reply/4,
    handle_trans_reply/5,
    handle_trans_ack/4,
    handle_trans_ack/5,
    handle_unexpected_trans/3,
    handle_unexpected_trans/4,
    handle_trans_request_abort/4,
    handle_trans_request_abort/5,
    handle_segment_reply/5,
    handle_segment_reply/6
   ]).
