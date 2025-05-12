%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose : Main API for Megaco/H.248 protocol stack
%%----------------------------------------------------------------------

-module(megaco).
-moduledoc """
Main API of the Megaco application

Interface module for the Megaco application
""".

%%-----------------------------------------------------------------
%% Public interface
%%-----------------------------------------------------------------

-export([
         start/0,
         stop/0,
        
         start_user/2,
         stop_user/1,

	 info/0, 
         user_info/1, user_info/2,
         update_user_info/3,
         conn_info/1, conn_info/2,
         update_conn_info/3,
         system_info/0, system_info/1,

         connect/4, connect/5, 
         disconnect/2,

         call/3,
         cast/3,
         cancel/2,
         process_received_message/4, process_received_message/5,
         receive_message/4, receive_message/5,

	 encode_actions/3,

	 token_tag2string/1, token_tag2string/2, token_tag2string/3, 

	 parse_digit_map/1,
	 eval_digit_map/1,
	 eval_digit_map/2,
	 report_digit_event/2,
	 test_digit_event/2,

	 encode_binary_term_id/2,
	 decode_binary_term_id/2,

	 encode_sdp/1,
	 decode_sdp/1,
         get_sdp_record_from_PropertyGroup/2,

	 versions1/0, versions2/0, 
	 print_version_info/0, print_version_info/1, 
	 ms/0, nc/0, nc/1, ni/0, ni/1,

	 enable_trace/2, disable_trace/0, set_trace/1,

	 report_event/4, report_event/5,

	 test_request/5,
	 test_reply/5
        ]).

-export([
	 get_stats/0, get_stats/1, get_stats/2,
	 reset_stats/0, reset_stats/1
	]).

%% Internal
-export([
         %% These are used both for debugging (verbosity printouts)
         %% and other such "utility" operations (and testing).
         format_timestamp/1, format_timestamp/2, 
         format_short_timestamp/1, format_short_timestamp/2, 
         format_long_timestamp/1, format_long_timestamp/2,
         formated_timestamp/0, 
         formated_short_timestamp/0, 
         formated_long_timestamp/0
        ]).

-export_type([
              void/0,

              mid/0,
              receive_handle/0,
              conn_handle/0,
              action_request/0,
              action_reply/0,
              error_desc/0,
              transaction_reply/0,
              protocol_version/0,

              digit_map_value/0,
              digit_map_kind/0,
              digit_map_event/0,
              digit_map_letter/0,

              property_parm/0,
              property_group/0,
              property_groups/0,
              sdp/0,

              trace_level/0
             ]).

-doc "The type is used when a functions return is to be ignored.".
-type void() :: term().

-doc "The Megaco Identifier.".
-type mid()               :: {ip4Address, megaco_encoder:ip4Address()} |
                             {ip6Address, megaco_encoder:ip6Address()} |
                             {domainName, megaco_encoder:domainName()} |
                             {deviceName, megaco_encoder:deviceName()} |
                             {mtpAddress, megaco_encoder:mtpAddress()}.
-type megaco_message()    :: megaco_encoder:megaco_message().
-type action_request()    :: megaco_encoder:action_request().
-type action_reply()      :: megaco_encoder:action_reply().
-type error_desc()        :: megaco_encoder:error_desc().
-type transaction_reply() :: megaco_encoder:transaction_reply().
-type protocol_version()  :: megaco_encoder:protocol_version().
-type segment_no()        :: megaco_encoder:segment_no().
-type receive_handle()    :: megaco_user:receive_handle().
-type conn_handle()       :: megaco_user:conn_handle().
-type megaco_timer()      :: megaco_user:megaco_timer().
-type transaction_id()    :: pos_integer().

-type action_reqs()    :: binary() | [action_request()].
-type action_reps()    :: [action_reply()].
-doc """
This type is a basic (atom) lookup key (for info on an active connection). The
corresponding value can be of any type.

- **`control_pid`**{: #ci_control_pid } - The process identifier of the
  controlling process for a connection.

  Value type: [pid()](`t:erlang:pid/0`)

- **`send_handle`**{: #ci_send_handle } - Opaque send handle whose contents is
  internal for the send module.

  Value type: `t:send_handle/0`

  [](){: #ci_local_mid }

- **`local_mid`** - The local mid (of the connection, i.e. the own mid).

  Value type: `t:mid/0`

  [](){: #ci_remote_mid }

- **`remote_mid`** - The remote mid (of the connection).

  Value type: `t:mid/0`

  [](){: #ci_receive_handle }

- **`receive_handle`** - Construct a megaco_receive_handle record.

  Value type: `t:receive_handle/0`

  [](){: #ci_trans_id }

- **`trans_id`** - Next transaction id.

  Note that transaction id's are (currently) maintained on a per user basis so
  there is no way to be sure that the value returned will actually be used for a
  transaction sent on this connection (in case a user has several connections,
  which is not at all unlikely).

  Value type: `t:transaction_id/0` | `undefined_serial`

  [](){: #ci_max_trans_id }

- **`max_trans_id`** - Last transaction id.

  Value type: `t:transaction_id/0` | `infinity`

  [](){: #ci_request_timer }

- **`request_timer`** - Wait for reply.

  The timer is cancelled when a reply is received.

  When a pending message is received, the timer is cancelled and the
  `long_request_timer` is started instead (see below). No resends will be
  performed from this point (since we now know that the other side has received
  the request).

  When the timer reaches an intermediate expire, the request is resent and the
  timer is restarted.

  When the timer reaches the final expire, either the function `megaco:call`
  will return with `{error, timeout}` or the callback function
  `handle_trans_reply` will be called with `UserReply = {error, timeout}` (if
  `megaco:cast` was used).

  Value type: `t:megaco_timer/0`

  Defaults to `#megaco_incr_timer{}`.

  [](){: #ci_long_request_timer }

- **`long_request_timer`** - Wait for reply after having received a pending
  message.

  When the timer reaches an intermediate expire, the timer restarted.

  When a pending message is received, and the `long_request_timer` is _not_ "on
  its final leg", the timer will be restarted, and, if
  `long_request_resend = true`, the request will be re-sent.

  Value type: `t:megaco_timer/0`

  Defaults to `60 seconds`.

  [](){: #ci_request_keep_alive_timeout }

- **`request_keep_alive_timeout`** - Specifies the timeout time for the
  request-keep-alive timer.

  This timer is started when the _first_ reply to an asynchronous request
  (issued using the `megaco:cast/3` function) arrives. As long
  as this timer is running, replies will be delivered via the
  [handle_trans_reply/4,5](`c:megaco_user:handle_trans_reply/5`) callback function, with
  their "arrival number" (see `UserReply` of the
  [handle_trans_reply/4,5](`c:megaco_user:handle_trans_reply/5`) callback function).

  Replies arriving after the timer has expired, will be delivered using the
  [handle_unexpected_trans/3,4](`c:megaco_user:handle_unexpected_trans/4`) callback
  function.

  Value type: `plain` | [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  Defaults to `plain`.

  [](){: #ci_long_request_resend }

- **`long_request_resend`** - This option indicates weather the request should
  be resent until the reply is received, _even_ though a pending message has
  been received.

  Normally, after a pending message has been received, the request is not resent
  (since a pending message is an indication that the request has been received).
  But since the reply (to the request) can be lost, this behaviour has its
  values.

  It is of course pointless to set this value to _true_ unless the
  `long_request_timer` (see above) is also set to an incremental timer
  (`#megaco_incr_timer{}`).

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ci_reply_timer }

- **`reply_timer`** - Wait for an ack.

  When a request is received, some info related to the reply is store internally
  (e.g. the binary of the reply). This info will live until either an ack is
  received or this timer expires. For instance, if the same request is received
  again (e.g. a request with the same transaction id), the (stored) reply will
  be (re-) sent automatically by megaco.

  If the timer is of type `#megaco_incr_timer{}`, then for each intermediate
  timout, the reply will be resent (this is valid until the ack is received or
  the timer expires).

  Value type: `t:megaco_timer/0`

  Defaults to `30000`.

  [](){: #ci_call_proxy_gc_timeout }

- **`call_proxy_gc_timeout`** - Timeout time for the call proxy.

  When a request is sent using the `call/3` function, a proxy
  process is started to handle all replies. When the reply has been received and
  delivered to the user, the proxy process continue to exist for as long as this
  option specifies. Any received messages, is passed on to the user via the
  [handle_unexpected_trans](`c:megaco_user:handle_unexpected_trans/4`) callback
  function.

  The timeout time is in milliseconds. A value of 0 (zero) means that the proxy
  process will exit directly after the reply has been delivered.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  Defaults to `5000` (= 5 seconds).

- **`auto_ack`** - Automatic send transaction ack when the transaction reply has
  been received (see `trans_ack` below).

  This is used for _three-way-handshake_.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ci_trans_ack }

- **`trans_ack`** - Shall ack's be accumulated or not.

  This property is only valid if `auto_ack` is true.

  If `auto_ack` is true, then if `trans_ack` is `false`, ack's will be sent
  immediately. If `trans_ack` is `true`, then ack's will instead be sent to the
  transaction sender process for accumulation and later sending (see
  `trans_ack_maxcount`, `trans_req_maxcount`, `trans_req_maxsize`,
  `trans_ack_maxcount` and `trans_timer`).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ci_trans_ack_maxcount }

- **`trans_ack_maxcount`** - Maximum number of accumulated ack's. At most this
  many ack's will be accumulated by the transaction sender (if started and
  configured to accumulate ack's).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  Defaults to `10`.

  [](){: #ci_trans_req }

- **`trans_req`** - Shall requests be accumulated or not.

  If `trans_req` is `false`, then request(s) will be sent immediately (in its
  own message).

  If `trans_req` is true, then request(s) will instead be sent to the
  transaction sender process for accumulation and later sending (see
  `trans_ack_maxcount`, `trans_req_maxcount`, `trans_req_maxsize`,
  `trans_ack_maxcount` and `trans_timer`).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ci_trans_req_maxcount }

- **`trans_req_maxcount`** - Maximum number of accumulated requests. At most
  this many requests will be accumulated by the transaction sender (if started
  and configured to accumulate requests).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  Defaults to `10`.

  [](){: #ci_trans_req_maxsize }

- **`trans_req_maxsize`** - Maximum size of the accumulated requests. At most
  this much requests will be accumulated by the transaction sender (if started
  and configured to accumulate requests).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  Defaults to `2048`.

  [](){: #ci_trans_timer }

- **`trans_timer`** - Transaction sender timeout time. Has two functions. First,
  if the value is 0, then transactions will not be accumulated (e.g. the
  transaction sender process will not be started). Second, if the value is
  greater then 0 and `auto_ack` and `trans_ack` is true or if `trans_req` is
  true, then transaction sender will be started and transactions (which is
  depending on the values of `auto_ack`, `trans_ack` and `trans_req`) will be
  accumulated, for later sending.

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  Defaults to `0`.

  [](){: #ci_pending_timer }

- **`pending_timer`** - Automatic send transaction pending if the timer expires
  before a transaction reply has been sent. This timer is also called
  provisional response timer.

  Value type: `t:megaco_timer/0`

  Defaults to `30000` (= 30 seconds).

  [](){: #ci_sent_pending_limit }

- **`sent_pending_limit`** - Sent pending limit (see the
  MGOriginatedPendingLimit and the MGCOriginatedPendingLimit of the megaco root
  package). This parameter specifies how many pending messages that can be sent
  (for a given received transaction request). When the limit is exceeded, the
  transaction is aborted (see
  [handle_trans_request_abort](`c:megaco_user:handle_trans_request_abort/5`)) and an error
  message is sent to the other side.

  Note that this has no effect on the actual sending of pending transactions.
  This is either implicit (e.g. when receiving a re-sent transaction request for
  a request which is being processed) or controlled by the pending_timer, see
  above.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`) | `infinity`

  Defaults to `infinity`.

  [](){: #ci_recv_pending_limit }

- **`recv_pending_limit`** - Receive pending limit (see the
  MGOriginatedPendingLimit and the MGCOriginatedPendingLimit of the megaco root
  package). This parameter specifies how many pending messages that can be
  received (for a sent transaction request). When the limit is exceeded, the
  transaction is considered lost, and an error returned to the user (through the
  call-back function _handle_trans_reply_).

  Value type: [pos_integer()](`t:erlang:pos_integer/0`) | `infinity`

  Defaults to `infinity`.

  [](){: #ci_send_mod }

- **`send_mod`** - Send callback module which exports send_message/2. The
  function SendMod:send_message(SendHandle, Binary) is invoked when the bytes
  needs to be transmitted to the remote user.

  Value type: [module()](`t:erlang:module/0`).

  Defaults to `megaco_tcp`.

  [](){: #ci_encoding_mod }

- **`encoding_mod`** - Encoding callback module which exports encode_message/2
  and decode_message/2. The function EncodingMod:encode_message(EncodingConfig,
  MegacoMessage) is invoked whenever a 'MegacoMessage' record needs to be
  translated into an Erlang binary. The function
  EncodingMod:decode_message(EncodingConfig, Binary) is invoked whenever an
  Erlang binary needs to be translated into a 'MegacoMessage' record.

  Value type: [module()](`t:erlang:module/0`).

  Defaults to `megaco_pretty_text_encoder`.

  [](){: #ci_encoding_config }

- **`encoding_config`** - Encoding module config.

  Value type: [loist()](`t:erlang:list/0`).

  Defaults to `[]`.

  [](){: #ci_protocol_version }

- **`protocol_version`** - Actual protocol version.

  Value type: `t:protocol_version/0`

  Defaults to `1`.

  [](){: #ci_strict_version }

- **`strict_version`** - Strict version control, i.e. when a message is
  received, verify that the version is that which was negotiated.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `true`.

  [](){: #ci_reply_data }

- **`reply_data`** - Default reply data.

  Value type: [term()](`t:erlang:term/0`)

  Defaults to `undefined`.

  [](){: #ci_threaded }

- **`threaded`** - If a received message contains several transaction requests,
  this option indicates whether the requests should be handled sequentially in
  the same process (`false`), or if each request should be handled by its own
  process (`true` i.e. a separate process is spawned for each request).

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ci_resend_indication }

- **`resend_indication`** - This option indicates weather the transport module
  should be told if a message send is a resend or not.

  If _false_, megaco messages are sent using the
  [send_message/2](`c:megaco_transport:send_message/2`) function.

  If _true_, megaco message _re-sends_ are made using the
  [resend_message](`c:megaco_transport:resend_message/2`) function. The initial
  message send is still done using the
  [send_message](`c:megaco_transport:send_message/2`) function.

  The special value _flag_ instead indicates that the function
  [send_message/3](`c:megaco_transport:send_message/3`) shall be used.

  Value type: `flag` | [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ci_segment_reply_ind }

- **`segment_reply_ind`** - This option specifies if the user shall be notified
  of received segment replies or not.

  See [handle_segment_reply](`c:megaco_user:handle_segment_reply/6`) callback function
  for more information.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ci_segment_recv_timer }

- **`segment_recv_timer`** - This timer is started when the segment indicated by
  the `segmentation complete token` (e.g. the last of the segment which makes up
  the reply) is received, but all segments has not yet been received.

  When the timer finally expires, a "megaco segments not received" (459) error
  message is sent to the other side and the user is notified with a
  `segment timeout` `UserReply` in either the
  [handle_trans_reply](`c:megaco_user:handle_trans_reply/5`) callback function or the
  return value of the `call/2` function.

  Value type: `t:megaco_timer/0`

  Defaults to `10000` (= 10 seconds).

  [](){: #ci_segment_send }

- **`segment_send`** - Shall outgoing messages be segmented or not.

  Value type: `none` | [pos_integer()](`t:erlang:pos_integer/0`) | `infinity`

  - **`none`** - Do not segment outgoing reply messages. This is useful when
    either it is known that messages are never to large or that the transport
    protocol can handle such things on its own (e.g. TCP or SCTP).

  - **`t:pos_integer/0`** - Outgoing reply messages will be segmented as needed
    (see `max_pdu_size` below). This value, K, indicate the outstanding window,
    i.e. how many segments can be outstanding (not acknowledged) at any given
    time.

  - **`infinity`** - Outgoing reply messages will be segmented as needed (see
    `max_pdu_size` below). Segment messages are sent all at once (i.e. no
    acknowledgement awaited before sending the next segment).

  Defaults to `none`.

  [](){: #ci_max_pdu_size }

- **`max_pdu_size`** - Max message size. If the encoded message (PDU) exceeds
  this size, the message should be segmented, and then encoded.

  Value type: `infinity` | [pos_integer()](`t:erlang:pos_integer/0`)

  Defaults to `infinity`.
""".
-type conn_info_item() :: control_pid |
                          send_handle |
                          local_mid   |
                          remote_mid  |
                          receive_handle |
                          trans_id       |
                          max_trans_id   |
                          request_timer  |
                          long_request_timer         |
                          request_keep_alive_timeout |
                          long_request_resend        |
                          reply_timer           |
                          call_proxy_gc_timeout |
                          auto_ack  |
                          trans_ack |
                          trans_ack_maxcount |
                          trans_req          |
                          trans_req_maxcount |
                          trans_req_maxsize  |
                          trans_timer        |
                          pending_timer      |
                          sent_pending_limit |
                          recv_pending_limit |
                          send_mod           |
                          encoding_mod       |
                          encoding_config    |
                          protocol_version   |
                          strict_version     |
                          reply_data         |
                          threaded           |
                          resend_indication  |
                          segment_reply_ind  |
                          segment_recv_timer |
                          segment_send       |
                          max_pdu_size.
-doc """
- **`connections`** - Lists all active connections for this user. Returns a list
  of megaco_conn_handle records.

  Value type: [`t:conn_handle/0`]

  [](){: #ui_receive_handle }

- **`receive_handle`** - Construct a receive_handle from user config.

  Value type: `t:receive_handle/0`

  [](){: #ui_trans_id }

- **`trans_id`** - Current transaction id.

  Value type: `t:transaction_id/0` | `undefined_serial`

  [](){: #ui_min_trans_id }

- **`min_trans_id`** - First transaction id.

  Value type: `t:transaction_id/0`

  Defaults to `1`.

  [](){: #ui_max_trans_id }

- **`max_trans_id`** - Last transaction id.

  Value type: `t:transaction_id/0` | `infinity`

  Defaults to `infinity`.

  [](){: #ui_request_timer }

- **`request_timer`** - Wait for reply.

  The timer is cancelled when a reply is received.

  When a pending message is received, the timer is cancelled and the
  `long_request_timer` is started instead (see below). No resends will be
  performed from this point (since we now know that the other side has received
  the request).

  When the timer reaches an intermediate expire, the request is resent and the
  timer is restarted.

  When the timer reaches the final expire, either the function `megaco:call`
  will return with `{error, timeout}` or the callback function
  `handle_trans_reply` will be called with `UserReply = {error, timeout}` (if
  `megaco:cast` was used).

  Value type: `t:megaco_timer/0`

  Defaults to `#megaco_incr_timer{}`.

  [](){: #ui_long_request_timer }

- **`long_request_timer`** - Wait for reply after having received a pending
  message.

  When the timer reaches an intermediate expire, the timer is restarted.

  When a pending message is received, and the `long_request_timer` is _not_ "on
  its final leg", the timer will be restarted, and, if
  `long_request_resend = true`, the request will be re-sent.

  Value type: `t:megaco_timer/0`

  Defaults to `60000` (= 60 seconds).

  [](){: #ui_long_request_resend }

- **`long_request_resend`** - This option indicates weather the request should
  be resent until the reply is received, _even_ though a pending message has
  been received.

  Normally, after a pending message has been received, the request is not resent
  (since a pending message is an indication that the request has been received).
  But since the reply (to the request) can be lost, this behaviour has its
  values.

  It is of course pointless to set this value to _true_ unless the
  `long_request_timer` (see above) is also set to an incremental timer
  (`#megaco_incr_timer{}`).

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ui_reply_timer }

- **`reply_timer`** - Wait for an ack.

  When a request is received, some info related to the reply is store internally
  (e.g. the binary of the reply). This info will live until either an ack is
  received or this timer expires. For instance, if the same request is received
  again (e.g. a request with the same transaction id), the (stored) reply will
  be (re-) sent automatically by megaco.

  If the timer is of type `#megaco_incr_timer{}`, then for each intermediate
  timout, the reply will be resent (this is valid until the ack is received or
  the timer expires).

  Value type: `t:megaco_timer/0`

  Defaults to `30 seconds`.

  [](){: #ui_request_keep_alive_timeout }

- **`request_keep_alive_timeout`** - Specifies the timeout time for the
  request-keep-alive timer.

  This timer is started when the _first_ reply to an asynchronous request
  (issued using the `megaco:cast/3` function) arrives. As long
  as this timer is running, replies will be delivered via the
  [handle_trans_reply/4,5](`c:megaco_user:handle_trans_reply/5`) callback function, with
  their "arrival number" (see `UserReply` of the
  [handle_trans_reply/4,5](`c:megaco_user:handle_trans_reply/5`) callback function).

  Replies arriving after the timer has expired, will be delivered using the
  [handle_unexpected_trans/3,4](`c:megaco_user:handle_unexpected_trans/4`) callback
  function.

  Value type: `plain` | [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  Defaults to `plain`.

  [](){: #ui_call_proxy_gc_timeout }

- **`call_proxy_gc_timeout`** - Timeout time for the call proxy.

  When a request is sent using the `call/3` function, a proxy
  process is started to handle all replies. When the reply has been received and
  delivered to the user, the proxy process continue to exist for as long as this
  option specifies. Any received messages, is passed on to the user via the
  [handle_unexpected_trans](`c:megaco_user:handle_unexpected_trans/4`) callback
  function.

  The timeout time is in milliseconds. A value of 0 (zero) means that the proxy
  process will exit directly after the reply has been delivered.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`)

  Defaults to `5000` (= 5 seconds).

  [](){: #ui_auto_ack }

- **`auto_ack`** - Automatic send transaction ack when the transaction reply has
  been received (see `trans_ack` below).

  This is used for _three-way-handshake_.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ui_trans_ack }

- **`trans_ack`** - Shall ack's be accumulated or not.

  This property is only valid if `auto_ack` is true.

  If `auto_ack` is true, then if `trans_ack` is `false`, ack's will be sent
  immediately. If `trans_ack` is `true`, then ack's will instead be sent to the
  transaction sender process for accumulation and later sending (see
  `trans_ack_maxcount`, `trans_req_maxcount`, `trans_req_maxsize`,
  `trans_ack_maxcount` and `trans_timer`).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ui_trans_ack_maxcount }

- **`trans_ack_maxcount`** - Maximum number of accumulated ack's. At most this
  many ack's will be accumulated by the transaction sender (if started and
  configured to accumulate ack's).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [integer()](`t:erlang:integer/0`)

  Defaults to `10`.

  [](){: #ui_trans_req }

- **`trans_req`** - Shall requests be accumulated or not.

  If `trans_req` is `false`, then request(s) will be sent immediately (in its
  own message).

  If `trans_req` is true, then request(s) will instead be sent to the
  transaction sender process for accumulation and later sending (see
  `trans_ack_maxcount`, `trans_req_maxcount`, `trans_req_maxsize`,
  `trans_ack_maxcount` and `trans_timer`).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ui_trans_req_maxcount }

- **`trans_req_maxcount`** - Maximum number of accumulated requests. At most
  this many requests will be accumulated by the transaction sender (if started
  and configured to accumulate requests).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [integer()](`t:erlang:integer/0`)

  Defaults to `10`.

  [](){: #ui_trans_req_maxsize }

- **`trans_req_maxsize`** - Maximum size of the accumulated requests. At most
  this much requests will be accumulated by the transaction sender (if started
  and configured to accumulate requests).

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [integer()](`t:erlang:integer/0`)

  Defaults to `2048`.

  [](){: #ui_trans_timer }

- **`trans_timer`** - Transaction sender timeout time. Has two functions. First,
  if the value is 0, then transactions will not be accumulated (e.g. the
  transaction sender process will not be started). Second, if the value is
  greater then 0 and `auto_ack` and `trans_ack` are both true or if `trans_req`
  is true, then transaction sender will be started and transactions (which is
  depending on the values of `auto_ack`, `trans_ack` and `trans_req`) will be
  accumulated, for later sending.

  See also [transaction sender](megaco_run.md#transaction_sender) for more info.

  Value type: [integer()](`t:erlang:integer/0`)

  Defaults to `0`.

  [](){: #ui_pending_timer }

- **`pending_timer`** - Automatically send pending if the timer expires before a
  transaction reply has been sent. This timer is also called provisional
  response timer.

  Value type: `t:megaco_timer/0`

  Defaults to `30000`.

  [](){: #ui_sent_pending_limit }

- **`sent_pending_limit`** - Sent pending limit (see the
  MGOriginatedPendingLimit and the MGCOriginatedPendingLimit of the megaco root
  package). This parameter specifies how many pending messages that can be sent
  (for a given received transaction request). When the limit is exceeded, the
  transaction is aborted (see
  [handle_trans_request_abort](`c:megaco_user:handle_trans_request_abort/5`)) and an error
  message is sent to the other side.

  Note that this has no effect on the actual sending of pending transactions.
  This is either implicit (e.g. when receiving a re-sent transaction request for
  a request which is being processed) or controlled by the pending_timer, see
  above.

  Value type: `infinity` | [pos_integer()](`t:erlang:pos_integer/0`)

  Defaults to `infinity`.

  [](){: #ui_recv_pending_limit }

- **`recv_pending_limit`** - Receive pending limit (see the
  MGOriginatedPendingLimit and the MGCOriginatedPendingLimit of the megaco root
  package). This parameter specifies how many pending messages that can be
  received (for a sent transaction request). When the limit is exceeded, the
  transaction is considered lost, and an error returned to the user (through the
  call-back function _handle_trans_reply_).

  Value type: `infinity` | [pos_integer()](`t:erlang:pos_integer/0`)

  Defaults to `infinity`.

  [](){: #ui_send_mod }

- **`send_mod`** - Send callback module which exports send_message/2. The
  function SendMod:send_message(SendHandle, Binary) is invoked when the bytes
  needs to be transmitted to the remote user.

  Value type: [module()](`t:erlang:module/0`)

  Defaults to `megaco_tcp`.

  [](){: #ui_encoding_mod }

- **`encoding_mod`** - Encoding callback module which exports encode_message/2
  and decode_message/2. The function EncodingMod:encode_message(EncodingConfig,
  MegacoMessage) is invoked whenever a 'MegacoMessage' record needs to be
  translated into an Erlang binary. The function
  EncodingMod:decode_message(EncodingConfig, Binary) is invoked whenever an
  Erlang binary needs to be translated into a 'MegacoMessage' record.

  Value type: [module()](`t:erlang:module/0`)

  Defaults to `megaco_pretty_text_encoder`.

  [](){: #ui_encoding_config }

- **`encoding_config`** - Encoding module config.

  Value type: [list()](`t:erlang:list/0`)

  Defaults to `[]`.

  [](){: #ui_protocol_version }

- **`protocol_version`** - Actual protocol version.

  Value type: `t:protocol_version/0`

  Defaults to `1`.

  [](){: #ui_strict_version }

- **`strict_version`** - Strict version control, i.e. when a message is
  received, verify that the version is that which was negotiated.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `true`.

  [](){: #ui_reply_data }

- **`reply_data`** - Default reply data.

  Value type: [term()](`t:erlang:term/0`)

  Defaults to `undefined`.

  [](){: #ui_user_mod }

- **`user_mod`** - Name of the user callback module. See the the reference
  manual for megaco_user for more info.

  Value type: [module()](`t:erlang:module/0`)

  Has no default value since its an mandatory config option.

  [](){: #ui_user_args }

- **`user_args`** - List of extra arguments to the user callback functions. See
  the the reference manual for megaco_user for more info.

  Value type: [list()](`t:erlang:list/0`)

  [](){: #ui_threaded }

- **`threaded`** - If a received message contains several transaction requests,
  this option indicates whether the requests should be handled sequentially in
  the same process (`false`), or if each request should be handled by its own
  process (`true` i.e. a separate process is spawned for each request).

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ui_resend_indication }

- **`resend_indication`** - This option indicates weather the transport module
  should be told if a message send is a resend or not.

  If _false_, megaco messages are sent using the
  [send_message](`c:megaco_transport:send_message/2`) function.

  If _true_, megaco message _re-sends_ are made using the
  [resend_message](`c:megaco_transport:resend_message/2`) function. The initial
  message send is still done using the
  [send_message](`c:megaco_transport:send_message/2`) function.

  The special value _flag_ instead indicates that the function
  [send_message/3](`c:megaco_transport:send_message/3`) shall be used.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ui_segment_reply_ind }

- **`segment_reply_ind`** - This option specifies if the user shall be notified
  of received segment replies or not.

  See [handle_segment_reply](`c:megaco_user:handle_segment_reply/6`) callback function
  for more information.

  Value type: [boolean()](`t:erlang:boolean/0`)

  Defaults to `false`.

  [](){: #ui_segment_recv_timer }

- **`segment_recv_timer`** - This timer is started when the segment indicated by
  the `segmentation complete token` is received, but all segments has not yet
  been received.

  When the timer finally expires, a "megaco segments not received" (459) error
  message is sent to the other side and the user is notified with a
  `segment timeout` `UserReply` in either the
  [handle_trans_reply](`c:megaco_user:handle_trans_reply/5`) callback function or the
  return value of the `call/3` function.

  Value type: `t:megaco_timer/0`

  Defaults to `10000`.

  [](){: #ui_segment_send }

- **`segment_send`** - Shall outgoing messages be segmented or not.

  Value type: `none` | [pos_integer()](`t:erlang:pos_integer/0`) | `infinity`

  - **`none`** - Do not segment outgoing reply messages. This is useful when
    either it is known that messages are never to large or that the transport
    protocol can handle such things on its own (e.g. TCP or SCTP).

  - **`t:pos_integer/0`** - Outgoing reply messages will be segmented as needed
    (see `max_pdu_size` below). This value, K, indicate the outstanding window,
    i.e. how many segments can be outstanding (not acknowledged) at any given
    time.

  - **`infinity`** - Outgoing reply messages will be segmented as needed (see
    `max_pdu_size` below). Segment messages are sent all at once (i.e. no
    acknowledgement awaited before sending the next segment).

  Defaults to `none`.

  [](){: #ui_max_pdu_size }

- **`max_pdu_size`** - Max message size. If the encoded message (PDU) exceeds
  this size, the message should be segmented, and then encoded.

  Value type: `infinity` | [pos_integer()](`t:erlang:pos_integer/0`)

  Defaults to `infinity`.
""".
-type user_info_item() :: connections    |
                          receive_handle |
                          trans_id       |
                          min_trans_id   |
                          max_trans_id   |
                          request_timer  |
                          long_request_timer  |
                          long_request_resend |
                          reply_timer    |
                          request_keep_alive_timeout |
                          call_proxy_gc_timeout      |
                          auto_ack           |
                          trans_ack          |
                          trans_ack_maxcount |
                          trans_req          |
                          trans_req_maxcount |
                          trans_req_maxsize  |
                          trans_timer        |
                          pending_timer      |
                          sent_pending_limit |
                          recv_pending_limit |
                          send_mod           |
                          encoding_mod       |
                          encoding_config    |
                          protocol_version   |
                          strict_version     |
                          reply_data         |
                          user_mod           |
                          user_args          |
                          threaded           |
                          resend_indication  |
                          segment_reply_ind  |
                          segment_recv_timer | 
                          segment_send       |
                          max_pdu_size.

-doc """
Opaque send handle whose contents is internal for the send module. May be any
term.
""".
-type send_handle()          :: term().

-type digit_map_value()      :: megaco_digit_map:value().
-type digit_map_kind()       :: megaco_digit_map:kind().
-type digit_map_event()      :: megaco_digit_map:event().
-type digit_map_letter()     :: megaco_digit_map:letter().


-type property_parm()       :: megaco_sdp:property_parm().
-type property_group()      :: megaco_sdp:property_group().
-type property_groups()     :: megaco_sdp:property_groups().
-type sdp()                 :: megaco_sdp:sdp().
-type sdp_property_parm()   :: megaco_sdp:sdp_property_parm().
-type sdp_property_group()  :: megaco_sdp:sdp_property_group().
-type sdp_property_groups() :: megaco_sdp:sdp_property_groups().

-doc """
How much trace events should be produced. `min` (=`0`) means no trace events are
produced, which is the default.
""".
-type trace_level()       :: min | max | 0..100.
-doc "The trace event generated by dbg.".
-type trace_event()       :: term().
-doc """
The trace data passed to the trace handler fun (in the second argument) and
returned by same. For instance this could be a file descriptor, that the trace
handler fun can use to print the event to a file.
""".
-type trace_data()        :: term().
-doc """
The trace handler fun is used to "process" each trace event (for instance print
to file or to stdout after formating).
""".
-type trace_handler()     :: fun((trace_event(), trace_data()) -> trace_data()).

-type global_counter() :: medGwyGatewayNumErrors.
-type counter()        :: medGwyGatewayNumTimerRecovery |
                          medGwyGatewayNumErrors.
-type counter_value()  :: non_neg_integer().

-doc """
System info items.

- **`text_config`**{: #si_text_config } - The text encoding config.

  Value type: [term()](`t:erlang:term/0`)

  [](){: #si_connections }

- **`connections`** - Lists all active connections. Returns a list of
  megaco_conn_handle records.

  Value type: [`t:conn_handle/0`]

  [](){: #si_users }

- **`users`** - Lists all active users.

  Value type: [`t:mid/0`]

  [](){: #si_active_requests }

- **`n_active_requests`** - Returns number of requests that has originated from
  this Erlang node and still are active (and therefore consumes system
  resources).

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  [](){: #si_active_replies }

- **`n_active_replies`** - Returns the number of replies that has originated
  from this Erlang node and still are active (and therefore consumes system
  resources).

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  [](){: #si_active_connections }

- **`n_active_connections`** - Returns the number of active connections.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

  [](){: #si_reply_counters }

- **`reply_counters`** - Returns a list of the reply counters.

  Value type: [\{`t:conn_handle/0`, `t:transaction_id/0`,
  [non_neg_integer()](`t:erlang:non_neg_integer/0`)\}]

  [](){: #si_pending_counters }

- **`pending_counters`** - Returns a list of the receive and sent pending
  counters.

  Value type: [\{recv | sent, [\{`t:transaction_id/0`,
  [non_neg_integer()](`t:erlang:non_neg_integer/0`)\}]\}]

""".
-type system_info_item() :: text_config          |
                            connections          |
                            users                |
                            n_active_requests    |
                            n_active_replies     |
                            n_active_connections |
                            reply_counters       |
                            pending_counters.


-include("megaco_internal.hrl").


%%-----------------------------------------------------------------
%% Starts the Megaco application
%%-----------------------------------------------------------------

-doc """
Starts the Megaco application

Users may either explicitly be registered with megaco:start_user/2 and/or be
statically configured by setting the application environment variable 'users' to
a list of \{UserMid, Config\} tuples. See the function megaco:start_user/2 for
details.

""".
-spec start() -> ok | {error, Reason} when
      Reason :: term().

start() ->
    application:start(?APPLICATION).


%%-----------------------------------------------------------------
%% Stops the Megaco application
%%-----------------------------------------------------------------

-doc """
Stops the Megaco application

""".
-spec stop() -> ok | {error, Reason} when
      Reason :: term().

stop() ->
    application:stop(?APPLICATION).


%%-----------------------------------------------------------------
%% Initial configuration of a user
%%-----------------------------------------------------------------

-doc """
Initial configuration of a user

Requires the megaco application to be started. A user is either a Media Gateway
(MG) or a Media Gateway Controller (MGC). One Erlang node may host many users.

A user is identified by its UserMid, which must be a legal Megaco MID.

Config is a list of \{Item, Value\} tuples. See megaco:user_info/2 about which
items and values that are valid.

""".
-spec start_user(UserMid, Config) -> ok | {error, Reason} when
      UserMid :: mid(),
      Config  :: [{Item, Value}],
      Item    :: user_info_item(),
      Value   :: term(),
      Reason  :: term().

start_user(UserMid, Config) ->
    megaco_config:start_user(UserMid, Config).


%%-----------------------------------------------------------------
%% Delete the configuration of a user
%%-----------------------------------------------------------------

-doc """
Delete the configuration of a user

Requires that the user does not have any active connection.

""".
-spec stop_user(UserMid) -> ok | {error, Reason} when
      UserMid :: mid(),
      Reason  :: term().

stop_user(UserMid) ->
    megaco_config:stop_user(UserMid).


%%-----------------------------------------------------------------
%% Lookup user information
%%-----------------------------------------------------------------

-doc(#{equiv => user_info/2}).
-spec user_info(UserMid) -> [{Item, Value}] when
      UserMid :: mid(),
      Item    :: requests | replies | user_info_item(),
      Value   :: term().

user_info(UserMid) ->
    [{requests, user_info(UserMid, requests)},
     {replies,  user_info(UserMid, replies)} | user_info(UserMid, all)].

-doc """
Lookup user information

[](){: #user_info_21 }

Lookup user information about currently active requests.
Expected input type `Input :: Requests` with expected
output type `Result :: RequestsResult`.

[](){: #user_info_22 }

Lookup user information about currently active replies.
Expected input type `Input :: Replies` with expected
output type `Result :: RepliesResult`.

""".
-spec user_info(UserMid, Input) -> Result when
      Input   :: Requests | Replies | Item,
      Requests :: requests,
      Replies :: replies,
      Item    :: user_info_item(),
      UserMid :: mid(),
      Conn    :: conn_handle(),
      Result  :: RequestsResult | RepliesResult | ItemResult,
      RequestsResult :: [{Conn, [TransId]}],
      ItemResult :: term(),
      RepliesResult :: [{Conn, [{TransId, ReplyState, Handler}]}],
      TransId :: transaction_id(),
      ReplyState :: prepare | eval_request | waiting_for_ack | aborted,
      Handler    :: undefined | pid().

user_info(UserMid, requests) ->
    megaco_messenger:which_requests(UserMid);
user_info(UserMid, replies) ->
    megaco_messenger:which_replies(UserMid);
user_info(UserMid, Item) ->
    megaco_config:user_info(UserMid, Item).


%%-----------------------------------------------------------------
%% Update information about a user
%%-----------------------------------------------------------------

-doc """
Update information about a user

Requires that the user is started.

""".
-spec update_user_info(UserMid, Item, Value) -> ok | {error, Reason} when
      UserMid :: mid(),
      Item    :: user_info_item(),
      Value   :: term(),
      Reason  :: term().

update_user_info(UserMid, Item, Value) ->
    megaco_config:update_user_info(UserMid, Item, Value).


%%-----------------------------------------------------------------
%% Lookup information about an active connection
%%-----------------------------------------------------------------

-doc(#{equiv => conn_info/2}).
-spec conn_info(ConnHandle) -> [{Item, Value}] when
      ConnHandle :: conn_handle(),
      Item       :: requests | replies | conn_info_item(),
      Value      :: term().
      
conn_info(ConnHandle) ->
    [{requests, conn_info(ConnHandle, requests)},
     {replies,  conn_info(ConnHandle, replies)} | conn_info(ConnHandle, all)].

-doc """
Lookup information about an active connection

Requires that the connection is active.

Failure: `exit` if, for instance, `ConnHandle` refers to a connection that no
longer exists.
""".
-spec conn_info(ConnHandle, ConnInfo) -> Value when
      ConnHandle :: conn_handle(),
      ConnInfo   :: all | requests | replies | conn_info_item(),
      Value      :: term().

conn_info(ConnHandle, all = Item) ->
    megaco_config:conn_info(ConnHandle, Item);

conn_info(ConnHandle, requests = _Item) ->
    megaco_messenger:which_requests(ConnHandle);

conn_info(ConnHandle, replies) ->
    megaco_messenger:which_replies(ConnHandle);

conn_info(ConnHandle, Item) ->
    megaco_config:conn_info(ConnHandle, Item).


%%-----------------------------------------------------------------
%% Update information about an active connection
%%-----------------------------------------------------------------

-doc """
Update information about an active connection

Requires that the connection is activated. See `t:conn_info_item/0` about which
items and values that are valid.

""".
-spec update_conn_info(ConnHandle, Item, Value) -> ok | {error, Reason} when
      ConnHandle :: conn_handle(),
      Item       :: conn_info_item(),
      Value      :: term(),
      Reason     :: term().

update_conn_info(ConnHandle, Item, Value) ->
    megaco_config:update_conn_info(ConnHandle, Item, Value).


%%-----------------------------------------------------------------
%% All information for the application
%%-----------------------------------------------------------------

-doc """
This function produces a list of information about the megaco application. Such
as users and their config, connections and their config, statistics and so on.

This information can be produced by the functions
`user_info/2`, `conn_info/2`, `system_info/1` and `get_stats/2` but
this is a simple way to get it all at once.
""".
-spec info() -> Info when
      Info  :: [{Key, Value}],
      Key   :: atom(),
      Value :: term().

info() ->
    Stats = 
	case get_stats() of
	    {ok, Statistics} ->
		Statistics;
	    _ ->
		[]
	end,
    SysInfo = system_info(),
    [{statistics, Stats} | info(SysInfo)].

info(SysInfo) ->
    info(SysInfo, []).

info([], Acc) ->
    lists:reverse(Acc);
info([{connections, Conns} | SysInfo], Acc) ->
    Conns2 = extend_conns_info(Conns),
    info(SysInfo, [{connections, Conns2} | Acc]);
info([{users, Users} | SysInfo], Acc) ->
    Users2 = extend_users_info(Users),
    info(SysInfo, [{users, Users2} | Acc]);
info([Info | SysInfo], Acc) ->
    info(SysInfo, [Info | Acc]).

extend_conns_info(Conns) ->
    extend_conns_info(Conns, []).

extend_conns_info([], Acc) ->
    lists:reverse(Acc);
extend_conns_info([Conn | Conns], Acc) ->
    ConnInfo = conn_info(Conn),
    extend_conns_info(Conns, [{Conn, ConnInfo} | Acc]).

extend_users_info(Users) ->
    extend_users_info(Users, []).

extend_users_info([], Acc) ->
    lists:reverse(Acc);
extend_users_info([User | Users], Acc) ->
    UserInfo = user_info(User),
    extend_users_info(Users, [{User, UserInfo} | Acc]).


%%-----------------------------------------------------------------
%% Lookup system information
%%-----------------------------------------------------------------

-spec system_info_items() -> [Item] when
      Item :: system_info_item().

system_info_items() ->
    [
     text_config, 
     connections, 
     users, 
     n_active_requests, 
     n_active_replies, 
     n_active_connections,
     reply_counters,
     pending_counters
    ].

-doc(#{equiv => system_info/1}).
-spec system_info() -> [{Item, Value}] when
      Item  :: system_info_item(),
      Value :: term().
      
system_info() ->
    [{Item, system_info(Item)} || Item <- system_info_items()].

-doc """
Lookup system information.

""".
-spec system_info(Item) -> Value when
      Item  :: system_info_item(),
      Value :: term().
      
system_info(Item) ->
    megaco_config:system_info(Item).


%%-----------------------------------------------------------------
%% Establish a "virtual" connection
%%-----------------------------------------------------------------

-doc(#{equiv => connect/5}).
-spec connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid) ->
          {ok, ConnHandle} | {error, Reason} when
      ReceiveHandle       :: receive_handle(),
      RemoteMid           :: preliminary_mid | mid(),
      SendHandle          :: send_handle(),
      ControlPid          :: pid(),
      ConnHandle          :: conn_handle(),
      Reason              :: ConnectReason | HandleConnectReason | term(),
      ConnectReason       :: {no_such_user,      LocalMid} |
                             {already_connected, ConnHandle} | term(),
      LocalMid            :: mid(),
      HandleConnectReason :: {connection_refused, ConnData, ErrorInfo} | term(),
      ConnData            :: term(),
      ErrorInfo           :: term().      

connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid) ->
    megaco_messenger:connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid).

-doc """
Establish a "virtual" connection

Activates a connection to a remote user. When this is done the connection can be
used to send messages (with `SendMod:send_message/2`). The `ControlPid` is the
identifier of a process that controls the connection. That process will be
supervised and if it dies, this will be detected and the
`UserMod:handle_disconnect/2` callback function will be invoked. See the
megaco_user module for more info about the callback arguments. The connection
may also explicitly be deactivated by invoking `megaco:disconnect/2`.

The `ControlPid` may be the identity of a process residing on another Erlang
node. This is useful when you want to distribute a user over several Erlang
nodes. In such a case one of the nodes has the physical connection. When a user
residing on one of the other nodes needs to send a request (with `megaco:call/3`
or `megaco:cast/3`), the message will encoded on the originating Erlang node,
and then be forwarded to the node with the physical connection. When the reply
arrives, it will be forwarded back to the originator. The distributed connection
may explicitly be deactivated by a local call to `megaco:disconnect/2` or
implicitly when the physical connection is deactivated (with
`megaco:disconnect/2`, killing the controlling process, halting the other node,
...).

The call of this function will trigger the callback function
`UserMod:handle_connect/2` to be invoked. See the megaco_user module for more
info about the callback arguments.

A connection may be established in several ways:

- **`provisioned MID`** - The MG may explicitly invoke megaco:connect/4 and use
  a provisioned MID of the MGC as the RemoteMid.

- **`upgrade preliminary MID`** - The MG may explicitly invoke megaco:connect/4
  with the atom 'preliminary_mid' as a temporary MID of the MGC, send an intial
  message, the Service Change Request, to the MGC and then wait for an initial
  message, the Service Change Reply. When the reply arrives, the Megaco
  application will pick the MID of the MGC from the message header and
  automatically upgrade the connection to be a "normal" connection. By using
  this method of establishing the connection, the callback function
  `UserMod:handle_connect/2` to be invoked twice. First with a `ConnHandle` with
  the remote_mid-field set to preliminary_mid, and then when the connection
  upgrade is done with the remote_mid-field set to the actual MID of the MGC.

- **`automatic`** - When the MGC receives its first message, the Service Change
  Request, the Megaco application will automatically establish the connection by
  using the MG MID found in the message header as remote mid.

- **`distributed`** - When a user (MG/MGC) is distributed over several nodes, it
  is required that the node hosting the connection already has activated the
  connection and that it is in the "normal" state. The `RemoteMid` must be a
  real Megaco MID and not a `preliminary_mid`.

An initial megaco_receive_handle record may be obtained with
`megaco:user_info(UserMid, receive_handle)`

The send handle is provided by the preferred transport module, e.g. megaco_tcp,
megaco_udp. Read the documentation about each transport module about the
details.

The connect is done in two steps: first an internal `connection setup` and then
by calling the user [handle_connect](`c:megaco_user:handle_connect/3`) callback function.
The first step could result in an error with `Reason = ConnectReason` and the
second an error with `Reason = HandleConnectReason`:

- **`ConnectReason`** - An error with this reason is generated by the megaco
  application itself.

- **`HandleConnectReason`** - An error with this reason is caused by the user
  [handle_connect](`c:megaco_user:handle_connect/3`) callback function either returning
  an error or an invalid value.

`Extra` can be any `t:term/0` except the atom `ignore_extra`. It is passed
(back) to the user via the callback function
[handle_connect/3](`c:megaco_user:handle_connect/3`).

""".
-spec connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid, Extra) ->
          {ok, ConnHandle} | {error, Reason} when
      ReceiveHandle       :: receive_handle(),
      RemoteMid           :: preliminary_mid | mid(),
      SendHandle          :: send_handle(),
      ControlPid          :: pid(),
      Extra               :: term(),
      ConnHandle          :: conn_handle(),
      Reason              :: ConnectReason | HandleConnectReason | term(),
      ConnectReason       :: {no_such_user,      LocalMid} |
                             {already_connected, ConnHandle} | term(),
      LocalMid            :: mid(),
      HandleConnectReason :: {connection_refused, ConnData, ErrorInfo} | term(),
      ConnData            :: term(),
      ErrorInfo           :: term().      

connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid, Extra) 
  when (Extra =/= ?default_user_callback_extra) ->
    megaco_messenger:connect(ReceiveHandle, RemoteMid, SendHandle, 
			     ControlPid, Extra).


%%-----------------------------------------------------------------
%% Tear down a "virtual" connection
%%-----------------------------------------------------------------

-doc """
Tear down a "virtual" connection

Causes the `UserMod:handle_disconnect/2` callback function to be invoked. See
the megaco_user module for more info about the callback arguments.

""".
-spec disconnect(ConnHandle, DiscoReason) -> ok | {error, ErrReason} when
      ConnHandle  :: conn_handle(),
      DiscoReason :: term(),
      ErrReason   :: term().

disconnect(ConnHandle, Reason) ->
    megaco_messenger:disconnect(ConnHandle, {user_disconnect, Reason}).


%%-----------------------------------------------------------------
%% Sends a transaction request and waits for a reply
%%-----------------------------------------------------------------

-doc """
Sends one or more transaction request(s) and waits for the reply.

When sending one transaction in a message, `ActionRequests` should be
`t:action_reqs/0` (the reply will then be `UserReply`). When sending several
transactions in a message, `ActionRequests` should be `[action_reqs()]` (the
reply will then be `[UserReply]`). Each element of the list is part of one
transaction.

For some of _our_ codecs (not binary), it is also possible to pre-encode the
actions, in which case `ActionRequests` will be either a `t:binary/0` or
`[binary()]`.

The function returns when the reply arrives, when the request timer eventually
times out or when the outstanding requests are explicitly cancelled.

The default values of the send options are obtained by
`megaco:conn_info(ConnHandle, Item)`. But the send options above, may explicitly
be overridden.

The `ProtocolVersion` version is the version actually encoded in the reply
message.

At `Success`, the `UserReply` contains a list of 'ActionReply' records possibly
containing error indications.

A `Failure`, indicates that the remote user has replied with an explicit
transactionError.

A `UserCancelReason`, indicates that the request has been canceled by the user.
`ReasonForUserCancel` is the reason given in the call to the
`cancel/2` function.

A send error (`SendReason`), indicates that the send function of the megaco
transport callback module failed to send the request. There are two separate
cases: `SendCancelledReason` and `SendFailedReason`. The first is the result of
the send function returning `{cancel, Reason}` and the second is some other kind
of erroneous return value. See the
[send_message](`c:megaco_transport:send_message/3`) function for more info.

An `OtherReason`, indicates some other error such as timeout.

For more info about the 'extra' part of the result (`SuccessExtra` and
`ErrorExtra`), see the [note](`m:megaco_user#extra_argument`) in the user
callback module documentation.

""".
-spec call(ConnHandle, ActionRequests, SendOptions) ->
          {ProtocolVersion, UserReply | [UserReply]} when
      ConnHandle          :: conn_handle(),
      ActionRequests      :: action_reqs() | [action_reqs()],
      SendOptions         :: [SendOption],
      SendOption          :: {request_timer,         megaco_timer()} |
                             {long_request_timer,    megaco_timer()} |
                             {send_handle,           send_handle()} |
                             {protocol_version,      protocol_version()} |
                             {call_proxy_gc_timeout, non_neg_integer()},
      ProtocolVersion     :: protocol_version(),
      UserReply           :: Success | Failure,
      Success             :: {ok, Result} | {ok, Result, SuccessExtra},
      Result              :: MessageResult | SegmentResult,
      MessageResult       :: action_reps(),
      SegmentResult       :: SegmentsOk,
      SegmentsOk          :: [{segment_no(), action_reps()}],
      Failure             :: {error, Reason} | {error, Reason, ErrorExtra},
      Reason              :: MessageReason | SegmentReason |
                             UserCancelReason | SendReason | OtherReason,
      MessageReason       :: error_desc(),
      SegmentReason       :: {segment, SegmentsOk, SegmentsErr} |
                             {segment_timeout,
                              MissingSegments,
                              SegmentsOk,
                              SegmentsErr},
      SegmentsErr         :: {segment_no(), error_desc()},
      MissingSegments     :: [segment_no()],
      UserCancelReason    :: {user_cancel, ReasonForUserCancel},
      ReasonForUserCancel :: term(),
      SendReason          :: SendCancelledReason | SendFailedReason,
      SendCancelledReason :: {send_message_cancelled, term()},
      SendFailedReason    :: {send_message_failed,    term()},
      OtherReason         :: {wrong_mid,
                              WrongMid :: mid(),
                              RightMid :: mid(),
                              transaction_reply()} | term(),
      SuccessExtra        :: term(),
      ErrorExtra          :: term().

call(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:call(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Sends a transaction request but does NOT wait for a reply
%%-----------------------------------------------------------------

-doc """
Sends one or more transaction request(s) but does NOT wait for a reply

When sending one transaction in a message, `ActionRequests` should be
`t:action_reqs/0`. When sending several transactions in a message,
`ActionRequests` should be `[action_reqs()]`. Each element of the list is part
of one transaction.

For some of _our_ codecs (not binary), it is also possible to pre-encode the
actions, in which case `Actions` will be either a `t:binary/0` or `[binary()]`.

The default values of the send options are obtained by
megaco:conn_info(ConnHandle, Item). But the send options above, may explicitly
be overridden.

The ProtocolVersion version is the version actually encoded in the reply
message.

The callback function UserMod:handle_trans_reply/4 is invoked when the reply
arrives, when the request timer eventually times out or when the outstanding
requests are explicitly cancelled. See the megaco_user module for more info
about the callback arguments.

Given as `ReplyData` argument to UserMod:handle_trans_reply/4.

""".
-spec cast(ConnHandle, ActionRequests, SendOptions) ->
          ok | {error, Reason} when
      ConnHandle      :: conn_handle(),
      ActionRequests  :: action_reqs() | [action_reqs()],
      SendOptions     :: [SendOption],
      SendOption      :: {request_keep_alive_timeout, RequestKeepAliveTimer} |
                         {request_timer,              megaco_timer()} |
                         {long_request_timer,         megaco_timer()} |
                         {send_handle,                send_handle()} |
                         {reply_data,                 ReplyData} |
                         {protocol_version,           ProtocolVersion},
      RequestKeepAliveTimer :: plain | non_neg_integer(),
      ReplyData       :: term(),
      ProtocolVersion :: protocol_version(),
      Reason          :: term().

cast(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:cast(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Test the validity of the actions
%%-----------------------------------------------------------------
    
-doc """
Tests if the Actions argument is correctly composed.

This function is only intended for testing purposes. It's supposed to have a
same kind of interface as the `call/3` or `cast/3`
functions (with the additions of the `EncodingMod` and `EncodingConfig`
arguments). It composes a complete megaco message end attempts to encode it. The
return value, will be a tuple of the composed megaco message and the encode
result.

""".
-spec test_request(ConnHandle, Version,
                   EncodingMod, EncodingConfig,
                   ActionRequests) -> {MegaMsg, EncodeRes} when
      ConnHandle     :: conn_handle(),
      Version        :: protocol_version(),
      EncodingMod    :: module(),
      EncodingConfig :: list(),
      ActionRequests :: action_reqs() | [action_reqs()],
      MegaMsg        :: megaco_message(),
      EncodeRes      :: {ok, Bin} | {error, Reason},
      Bin            :: binary(),
      Reason         :: term().

test_request(ConnHandle, Version, EncodingMod, EncodingConfig, 
	     ActionRequests) ->
    megaco_messenger:test_request(ConnHandle, ActionRequests, 
				  Version, EncodingMod, EncodingConfig).


%% This tests the actual_reply() type of return from the 
%% handle_trans_request function.
%%

-doc """
Tests if the Reply argument is correctly composed.

This function is only intended for testing purposes. It's supposed to test the
`actual_reply()` return value of the callback functions
[handle_trans_request](`c:megaco_user:handle_trans_request/4`) and
[handle_trans_long_request](`c:megaco_user:handle_trans_long_request/4`) functions (with
the additions of the `EncodingMod` and `EncodingConfig` arguments). It composes
a complete megaco message end attempts to encode it. The return value, will be a
tuple of the composed megaco message and the encode result.
""".
-spec test_reply(ConnHandle, Version,
                 EncodingMod, EncodingConfig,
                 Reply) -> {MegaMsg, EncodeRes} when
      ConnHandle     :: conn_handle(),
      Version        :: protocol_version(),
      EncodingMod    :: module(),
      EncodingConfig :: list(),
      Reply          :: error_desc() | [action_reply()],
      MegaMsg        :: megaco_message(),
      EncodeRes      :: {ok, Bin} | {error, Reason},
      Bin            :: binary(),
      Reason         :: term().

test_reply(ConnHandle, Version, EncodingMod, EncodingConfig, 
	   Reply) ->
    megaco_messenger:test_reply(ConnHandle, Version, 
				EncodingMod, EncodingConfig, Reply).


%%-----------------------------------------------------------------
%% Func: get_stats/0, get_stats/1, get_stats/2
%% Description: Retreive statistics (counters) for TCP
%%-----------------------------------------------------------------

-doc """
Retreive all (SNMP) statistic counters maintained by the megaco application.

The global counters handle events that cannot be attributed to a single
connection (e.g. protocol errors that occur before the connection has been
properly setup).

""".
-spec get_stats() -> {ok, [TotalStats]} | {error, Reason} when
      TotalStats :: {conn_handle(),    [Stats]} |
                    {global_counter(), counter_value()},
      Stats      :: {counter(),        counter_value()},
      Reason     :: term().
      
get_stats() ->
    megaco_messenger:get_stats().

-doc """
Retreive a (SNMP) (global) statistic counter maintained by the megaco
application.

The global counters handle events that cannot be attributed to a single
connection (e.g. protocol errors that occur before the connection has been
properly setup).

[](){: #get_stats_1_2 }

Retreive all (SNMP) statistic counters maintained by the megaco application, for
a specific connection.

""".
-spec get_stats(GCounter) -> {ok, Value} | {error, Reason} when
      GCounter :: global_counter(),
      Value    :: counter_value(),
      Reason   :: term();
               (ConnHandle) -> {ok, [Stats]} | {error, Reason} when
      ConnHandle :: conn_handle(),
      Stats      :: {counter(), counter_value()},
      Reason     :: term().

get_stats(ConnHandleOrGCounter) ->
    megaco_messenger:get_stats(ConnHandleOrGCounter).

-doc """
Retreive a (SNMP) statistic counter maintained by the megaco application.

""".
-spec get_stats(ConnHandle, Counter) -> {ok, Value} | {error, Reason} when
      ConnHandle :: conn_handle(),
      Counter    :: counter(),
      Value      :: counter_value(),
      Reason     :: term().

get_stats(ConnHandle, Counter) ->
    megaco_messenger:get_stats(ConnHandle, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters) for TCP
%%-----------------------------------------------------------------

-doc """
Reset all (SNMP) statistics counters.

""".
-spec reset_stats() -> void().

reset_stats() ->
    megaco_messenger:reset_stats().

-doc """
Reset the specified (SNMP) statistics counter.

[](){: #reset_stats_1_2 }

Reset all (SNMP) statistics counters for a connection.

""".
-spec reset_stats(GCounter) -> void() when
      GCounter :: global_counter();
               (ConnHandle) -> void() when
      ConnHandle :: conn_handle().

reset_stats(ConnHandleOrGCounter) ->
    megaco_messenger:reset_stats(ConnHandleOrGCounter).


%%-----------------------------------------------------------------
%% Cancel all outstanding messages for this connection
%%-----------------------------------------------------------------

-doc """
Cancel all outstanding messages for this connection

This causes outstanding megaco:call/3 requests to return. The callback functions
UserMod:handle_reply/4 and UserMod:handle_trans_ack/4 are also invoked where it
applies. See the megaco_user module for more info about the callback arguments.

""".
-spec cancel(ConnHandle, CancelReason) -> ok | {error, Reason} when
      ConnHandle   :: conn_handle(),
      CancelReason :: term(),
      Reason       :: term().

cancel(ConnHandle, Reason) ->
    megaco_messenger:cancel(ConnHandle, {user_cancel, Reason}).


%%-----------------------------------------------------------------
%% Process a received message
%%-----------------------------------------------------------------

-doc(#{equiv => process_received_message/5}).
-spec process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary().

process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:process_received_message(ReceiveHandle, ControlPid, 
					      SendHandle, BinMsg).

-doc """
Process a received message

This function is intended to be invoked by some transport modules when get an
incoming message. Which transport that actually is used is up to the user to
choose.

The message is delivered as an Erlang binary and is decoded by the encoding
module stated in the receive handle together with its encoding config (also in
the receive handle). Depending of the outcome of the decoding various callback
functions will be invoked. See megaco_user for more info about the callback
arguments.

The argument `Extra` is just an opaque data structure passed to the user via the
callback functions in the [user callback module](`m:megaco_user`). Note however
that if `Extra` has the value `extra_undefined` the argument will be ignored
(same as if `process_received_message/4` had
been called). See the documentation for the behaviour of the callback module,
`m:megaco_user`, for more info.

Note that all processing is done in the context of the calling process. A
transport module could call this function via one of the `spawn` functions (e.g.
`spawn_opt`). See also `receive_message/4,5`.

If the message cannot be decoded the following callback function will be
invoked:

- UserMod:handle_syntax_error/3

If the decoded message instead of transactions contains a message error, the
following callback function will be invoked:

- UserMod:handle_message_error/3

If the decoded message happens to be received before the connection is
established, a new "virtual" connection is established. This is typically the
case for the Media Gateway Controller (MGC) upon the first Service Change. When
this occurs the following callback function will be invoked:

- UserMod:handle_connect/2

For each transaction request in the decoded message the following callback
function will be invoked:

- UserMod:handle_trans_request/3

For each transaction reply in the decoded message the reply is returned to the
user. Either the originating function megaco:call/3 will return. Or in case the
originating function was megaco:case/3 the following callback function will be
invoked:

- UserMod:handle_trans_reply/4

When a transaction acknowledgement is received it is possible that user has
decided not to bother about the acknowledgement. But in case the return value
from UserMod:handle_trans_request/3 indicates that the acknowledgement is
important the following callback function will be invoked:

- UserMod:handle_trans_ack/4

See the megaco_user module for more info about the callback arguments.

""".
-spec process_received_message(ReceiveHandle,
                               ControlPid, SendHandle, BinMsg, Extra) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary(),
      Extra         :: term().

process_received_message(ReceiveHandle,
                         ControlPid, SendHandle, BinMsg, Extra) ->
    megaco_messenger:process_received_message(ReceiveHandle, ControlPid, 
					      SendHandle, BinMsg, 
					      Extra).


-doc(#{equiv => receive_message/5}).
-spec receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary().

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:receive_message(ReceiveHandle, ControlPid, 
				     SendHandle, BinMsg).

-doc """
Process a received message

This is a callback function intended to be invoked by some transport modules
when get an incoming message. Which transport that actually is used is up to the
user to choose.

In principle, this function calls the [process_received_message/4,5](`process_received_message/5`)
function via a `spawn` to perform the actual processing.
""".
-spec receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
          ok when
      ReceiveHandle :: receive_handle(),
      ControlPid    :: pid(),
      SendHandle    :: send_handle(),
      BinMsg        :: binary(),
      Extra         :: term().

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
    megaco_messenger:receive_message(ReceiveHandle, ControlPid, 
				     SendHandle, BinMsg,
				     Extra).


%%-----------------------------------------------------------------
%% Encode the actions list for one or more transactions.
%%-----------------------------------------------------------------

-doc """
Encodes lists of action requests for one or more transaction request(s).

When encoding action requests for one transaction, `Actions` should be
`t:action_reqs/0`. When encoding action requests for several transactions,
`Actions` should be `[action_reqs()]`. Each element of the list is part of one
transaction.

""".
-spec encode_actions(ConnHandle, ActionRequests, Options) ->
          {ok, Result} | {error, Reason} when
      ConnHandle     :: conn_handle(),
      ActionRequests :: action_reqs() | [action_reqs()],
      Options        :: [Option],
      Option         :: {request_timer,      megaco_timer()} |
                        {long_request_timer, megaco_timer()} |
                        {send_handle,        send_handle()} |
                        {protocol_version,   protocol_version()},
      Result         :: binary() | [binary()],
      Reason         :: term().

encode_actions(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:encode_actions(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Convert the (token) tags found in a decoded message into a 
%% printable string.
%%-----------------------------------------------------------------

-doc(#{equiv => token_tag2string/3}).
-spec token_tag2string(Tag) -> Result when
      Tag    :: atom(),
      Result :: string() | {error, Reason},
      Reason :: term().

token_tag2string(Tag) ->
    token_tag2string(Tag, pretty).

-doc(#{equiv => token_tag2string/3}).
-spec token_tag2string(Tag, EncodingMod) -> Result when
      Tag         :: atom(),
      EncodingMod :: pretty | compact | module(),
      Result      :: string() | {error, Reason},
      Reason      :: term().

token_tag2string(Tag, pretty) ->
    token_tag2string(Tag, megaco_pretty_text_encoder);
token_tag2string(Tag, compact) ->
    token_tag2string(Tag, megaco_compact_text_encoder);
token_tag2string(Tag, Mod) when is_atom(Tag) and is_atom(Mod) ->
    Mod:token_tag2string(Tag).

-doc """
Convert a token tag to a string

If no encoder module is given, the default is used (which is pretty).

If no or an unknown version is given, the _best_ version is used (which is v3).

If no match is found for `Tag`, `Result` will be the empty string (`[]`).

""".
-spec token_tag2string(Tag, EncodingMod, Version) -> Result when
      Tag         :: atom(),
      EncodingMod :: pretty | compact | module(),
      Version     :: protocol_version() | v1 | v2 | v3,
      Result      :: string() | {error, Reason},
      Reason      :: term().

token_tag2string(Tag, pretty, Version) ->
    token_tag2string(Tag, megaco_pretty_text_encoder, Version);
token_tag2string(Tag, compact, Version) ->
    token_tag2string(Tag, megaco_compact_text_encoder, Version);
token_tag2string(Tag, Mod, Version) when is_atom(Tag) andalso is_atom(Mod) ->
    Mod:token_tag2string(Tag, Version).


%%-----------------------------------------------------------------
%% Parses a digit map body
%%-----------------------------------------------------------------

-doc """
Parses a digit map body

Parses a digit map body, represented as a list of characters, into a list of
state transitions suited to be evaluated by megaco:eval_digit_map/1,2.

""".
-spec parse_digit_map(DigitMapBody) ->
          {ok, ParsedDigitMap} | {error, Reason} when
      DigitMapBody   :: string(),
      ParsedDigitMap :: term(),
      Reason         :: term().

parse_digit_map(DigitMapBody) ->
    megaco_digit_map:parse(DigitMapBody).


%%-----------------------------------------------------------------
%% Collect digit map letters according to the digit map
%%-----------------------------------------------------------------

-doc(#{equiv => eval_digit_map/2}).
-spec eval_digit_map(DigitMap) -> {ok, MatchResult} | {error, Reason} when
      DigitMap       :: digit_map_value() | ParsedDigitMap,
      ParsedDigitMap :: term(),
      MatchResult    :: {Kind, Letters} | {Kind, Letters, Extra},
      Kind           :: digit_map_kind(),
      Letters        :: [digit_map_letter()],
      Extra          :: digit_map_letter(),
      Reason         :: term().

eval_digit_map(DigitMap) ->
    megaco_digit_map:eval(DigitMap).

-doc """
Collect digit map letters according to the digit map.

When evaluating a digit map, a state machine waits for timeouts and letters
reported by megaco:report_digit_event/2. The length of the various timeouts are
defined in the digit_map_value() record.

When a complete sequence of valid events has been received, the result is
returned as a list of letters.

There are two options for handling syntax errors (that is when an unexpected
event is received when the digit map evaluator is expecting some other event).
The unexpected events may either be ignored or rejected. The latter means that
the evaluation is aborted and an error is returned.

""".
-spec eval_digit_map(DigitMap, Timers) ->
          {ok, MatchResult} | {error, Reason} when
      DigitMap       :: digit_map_value() | ParsedDigitMap,
      ParsedDigitMap :: term(),
      Timers         :: Ignore | Reject,
      Ignore         :: ignore |
                        {ignore, digit_map_value()},
      Reject         :: reject |
                        {reject, digit_map_value()} |
                        digit_map_value(),
      MatchResult    :: {Kind, Letters} | {Kind, Letters, Extra},
      Kind           :: digit_map_kind(),
      Letters        :: [digit_map_letter()],
      Extra          :: digit_map_letter(),
      Reason         :: term().

eval_digit_map(DigitMap, Timers) ->
    megaco_digit_map:eval(DigitMap, Timers).


%%-----------------------------------------------------------------
%% Send one or more events to event collector process
%%-----------------------------------------------------------------

-doc """
Send one or more events to the event collector process.

Send one or more events to a process that is evaluating a digit map, that is a
process that is executing megaco:eval_digit_map/1,2.

Note that the events `$s | $S`, `l | $L` and `$z | $Z` has nothing to do with
the timers using the same characters.

""".
-spec report_digit_event(DigitMapEvalPid, Events) -> ok | {error, Reason} when
      DigitMapEvalPid :: pid(),
      Events          :: digit_map_event() | [digit_map_event()],
      Reason          :: term().

report_digit_event(DigitMapEvalPid, Event) ->
    megaco_digit_map:report(DigitMapEvalPid, Event).


%%-----------------------------------------------------------------
%% Feed digit map collector with events and return the result
%%-----------------------------------------------------------------

-doc """
Feed digit map collector with events and return the result

This function starts the evaluation of a digit map with megaco:eval_digit_map/1
and sends a sequence of events to it with megaco:report_digit_event/2 in order
to simplify testing of digit maps.

""".
-spec test_digit_event(DigitMap, Events) ->
          {ok,  Kind,  Letters}  |  {error, Reason} when
      DigitMap       :: digit_map_value() | ParsedDigitMap,
      ParsedDigitMap :: term(),
      Events         :: digit_map_event() | [digit_map_event()],
      Kind           :: digit_map_kind(),
      Letters        :: [digit_map_letter()],
      Reason         :: term().

test_digit_event(DigitMap, Events) ->
    megaco_digit_map:test(DigitMap, Events).


%%-----------------------------------------------------------------
%% encode_binary_term_id(Config, MegacoTermId) ->
%% 
%%   {ok, TerminationId} | {error, Reason}
%%
%% Encode the Megaco internal form of a termination id (a
%% megaco_term_id record) into ASN.1'1 internal form of a termination
%% id (a 'TerminationId' record).
%% %%-----------------------------------------------------------------

-doc false.
encode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:encode(Config, TermId).


%%-----------------------------------------------------------------
%% decode_binary_term_id(Config, TerminationId) ->
%% 
%%   {ok, MegacoTermId} | {error, Reason}
%%
%% Decode ASN.1's internal form of a termination id (a 'TerminationId'
%% record) into the Megaco internal form of a termination id (a
%% megaco_term_id record).
%%-----------------------------------------------------------------

-doc false.
decode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:decode(Config, TermId).


%%-----------------------------------------------------------------
%% encode_sdp(SDP) ->
%% 
%%   {ok, PP} | {error, Reason}
%%
%% Encode a SDP construct into a property parm construct
%%-----------------------------------------------------------------

-doc """
Encode (generate) an SDP construct.

If a `t:property_parm/0` is found as part of the input (`SDP`) then it is left
unchanged.

This function performs the following transformation:

- sdp() -> property_parm()
- sdp_property_group() -> property_group()
- sdp_property_groups() -> property_groups()

""".
-spec encode_sdp(SDP) -> {ok, PP} | {error, Reason} when
      SDP   :: sdp_property_parm() |
               sdp_property_group() |
               sdp_property_groups() |
               asn1_NOVALUE,
      PP    :: property_parm() |
               property_group() |
               property_groups() |
               asn1_NOVALUE,
      Reason :: term().

encode_sdp(SDP) ->
    megaco_sdp:encode(SDP).


%%-----------------------------------------------------------------
%% decode_sdp(PP) ->
%% 
%%   {ok, SDP} | {error, Reason}
%%
%% Decode a property parm construct into a SDP construct
%%-----------------------------------------------------------------

-doc """
Decode (parse) a property parameter construct.

When decoding `t:property_group/0` or `t:property_groups/0`, those property
parameter constructs that cannot be decoded (either because of decode error or
because they are unknown), will be returned as a two-tuple. The first element of
which will be the (undecoded) property parameter and the other the actual
reason. This means that the caller of this function has to expect not only
sdp-records, but also this two-tuple construct.

This function performs the following transformation:

- property_parm() -> sdp()
- property_group() -> sdp_property_group()
- property_groups() -> sdp_property_groups()

""".
-spec decode_sdp(PP) -> {ok, SDP} | {error, Reason} when
      PP                      :: property_parm() |
                                 property_group() |
                                 property_groups() |
                                 asn1_NOVALUE,
      SDP                     :: sdp() |
                                 DecodeSdpPropertyGroup |
                                 DecodeSdpPropertyGroups |
                                 asn1_NOVALUE,
      DecodeSdpPropertyGroup  :: [DecodeSDP],
      DecodeSdpPropertyGroups :: [DecodeSdpPropertyGroup],
      DecodeSDP               :: sdp() | {property_parm(), DecodeError},
      DecodeError             :: term(),
      Reason                  :: term().

decode_sdp(PP) ->
    megaco_sdp:decode(PP).


%%-----------------------------------------------------------------
%% dget_sdp_record_from_PropertyGroup(Type, PG) ->
%% 
%%   [sdp()]}
%%
%% Get all sdp records of a certain type from a property group
%%-----------------------------------------------------------------

-doc false.
get_sdp_record_from_PropertyGroup(Type, PG) ->
    megaco_sdp:get_sdp_record_from_PropertyGroup(Type, PG).


%%-----------------------------------------------------------------

-doc(#{equiv => print_version_info/1}).
-spec print_version_info() -> void().

print_version_info() ->
    {ok, Versions} = megaco:versions1(),
    print_version_info(Versions).

-doc """
Utility function to produce a formated printout of the versions info generated
by the `versions1` and `versions2` functions.

The function print_version_info/0 uses the result of function version1/0 as
`VersionInfo`.

Example:

```erlang
           {ok, V} = megaco:versions1(), megaco:format_versions(V).
```

""".
-spec print_version_info(Versions) -> void() when
      Versions    :: [VersionInfo],
      VersionInfo :: term().

print_version_info(Versions) when is_list(Versions) ->
    print_sys_info(Versions),
    print_os_info(Versions),
    print_mods_info(Versions);
print_version_info(BadVersions) ->
    {error, {bad_versions, BadVersions}}.

print_sys_info(Versions) ->
    case key1search(sys_info, Versions) of
	{value, SysInfo} when is_list(SysInfo) ->
	    {value, Arch} = key1search(arch, SysInfo, "Not found"),
	    {value, Ver}  = key1search(ver, SysInfo, "Not found"),
	    io:format("System info: "
		      "~n   Arch: ~s"
		      "~n   Ver:  ~s"
		      "~n", [Arch, Ver]),
	    ok;
	_ ->
	    io:format("System info: Not found~n", []),
	    not_found
    end.
	    
print_os_info(Versions) ->
    case key1search(os_info, Versions) of
	{value, OsInfo} when is_list(OsInfo) ->
	    Fam = 
		case key1search(fam, OsInfo, "Not found") of
		    {value, F} when is_atom(F) ->
			atom_to_list(F);
		    {value, LF} when is_list(LF) ->
			LF;
		    {value, XF} ->
			lists:flatten(io_lib:format("~p", [XF]))
		end,
	    Name = 
		case key1search(name, OsInfo) of
		    {value, N} when is_atom(N) ->
			"[" ++ atom_to_list(N) ++ "]";
		    {value, LN} when is_list(LN) ->
			"[" ++ LN ++ "]";
		    not_found -> 
			""
		end,
	    Ver = 
		case key1search(ver, OsInfo, "Not found") of
		    {value, T} when is_tuple(T) ->
			tversion(T);
		    {value, LV} when is_list(LV) ->
			LV;
		    {value, XV} ->
			lists:flatten(io_lib:format("~p", [XV]))
		end,
	    io:format("OS info: "
		      "~n   Family: ~s ~s"
		      "~n   Ver:    ~s"
		      "~n", [Fam, Name, Ver]),
	    ok;
	_ ->
	    io:format("OS info:     Not found~n", []),
	    not_found
    end.

%% tversion({A, B, C}) ->
%%     lists:flatten(io_lib:format("~w.~w.~w", [A, B, C]));
tversion(T) ->
    L = tuple_to_list(T),
    lversion(L).

lversion([]) ->
    "";
lversion([A]) ->
    integer_to_list(A);
lversion([A|R]) ->
    integer_to_list(A) ++ "." ++ lversion(R).

print_mods_info(Versions) ->
    case key1search(mod_info, Versions) of
	{value, ModsInfo} when is_list(ModsInfo) ->
	    io:format("Module info: ~n", []),
	    lists:foreach(fun print_mod_info/1, ModsInfo);
	_ ->
	    io:format("Module info: Not found~n", []),
	    not_found
    end.

print_mod_info({Module, Info}) ->
    % Maybe a asn1 generated module
    Asn1Vsn = 
	case (catch Module:info()) of
	    AI when is_list(AI) ->
		case (catch key1search(vsn, AI)) of
		    {value, V} when is_atom(V) ->
			atom_to_list(V);
		    _ ->
			"-"
		end;
	    _ ->
		"-"
	end,
    Vsn = 
	case key1search(vsn, Info) of
	    {value, I} when is_integer(I) ->
		integer_to_list(I);
	    _ ->
		"Not found"
	end,
    AppVsn = 
	case key1search(app_vsn, Info) of
	    {value, S1} when is_list(S1) ->
		S1;
	    _ ->
		"Not found"
	end,
    CompVer = 
	case key1search(compiler_version, Info) of
	    {value, S2} when is_list(S2) ->
		S2;
	    _ ->
		"Not found"
	end,
    io:format("   ~w:~n"
	      "      Vsn:          ~s~n"
	      "      App vsn:      ~s~n"
	      "      ASN.1 vsn:    ~s~n"
	      "      Compiler ver: ~s~n",
	      [Module, Vsn, AppVsn, Asn1Vsn, CompVer]),
    ok.



key1search(Key, Vals) ->
    case lists:keysearch(Key, 1, Vals) of
        {value, {Key, Val}} ->
            {value, Val};
        false ->
            not_found
    end.

key1search(Key, Vals, Def) ->
    case key1search(Key, Vals) of
	not_found ->
	    {value, Def};
	Value ->
	    Value
    end.


%%-----------------------------------------------------------------

-doc(#{equiv => versions2/0}).
-spec versions1() -> {ok, VersionInfo} | {error, Reason} when
      VersionInfo :: list(),
      Reason      :: term().

versions1() ->
    case ms1() of
	{ok, Mods} ->
	    {ok, version_info(Mods)};
	Error ->
	    Error
    end.


-doc """
Utility functions used to retrieve some system and application info.

The difference between the two functions is in how they get the modules to
check. `versions1` uses the app-file and `versions2` uses the function
`application:get_key`.

""".
-spec versions2() -> {ok, VersionInfo} | {error, Reason} when
      VersionInfo :: list(),
      Reason      :: term().

versions2() ->
    case ms2() of
	{ok, Mods} ->
	    {ok, version_info(Mods)};
	Error ->
	    Error
    end.

version_info(Mods) ->
    SysInfo = sys_info(),
    OsInfo  = os_info(),
    ModInfo = [mod_version_info(Mod) || Mod <- Mods],
    [{sys_info, SysInfo}, {os_info, OsInfo}, {mod_info, ModInfo}].
    
mod_version_info(Mod) ->
    Info = Mod:module_info(),
    {value, {attributes, Attr}}   = lists:keysearch(attributes, 1, Info),
    {value, {vsn,        [Vsn]}}  = lists:keysearch(vsn,        1, Attr),
    {value, {app_vsn,    AppVsn}} = lists:keysearch(app_vsn,    1, Attr),
    {value, {compile,    Comp}}   = lists:keysearch(compile,    1, Info),
    {value, {version,    Ver}}    = lists:keysearch(version,    1, Comp),
    {Mod, [{vsn,              Vsn}, 
	   {app_vsn,          AppVsn}, 
	   {compiler_version, Ver}]}.

sys_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    [{arch, SysArch}, {ver, SysVer}].

os_info() ->
    {OsFam, OsName} = os:type(),
    [{fam, OsFam}, {name, OsName}, {ver, os:version()}].
    
-doc false.
ms() ->    
    ms1().

ms1() ->
    App    = ?APPLICATION,
    LibDir = code:lib_dir(App),
    File   = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
        {ok, [{application, App, AppFile}]} ->
	    case lists:keysearch(modules, 1, AppFile) of
		{value, {modules, Mods}} ->
		    {ok, Mods};
		_ ->
		    {error, {invalid_format, modules}}
	    end;
        Error ->
            {error, {invalid_format, Error}}
    end.

ms2() ->
    application:get_key(?APPLICATION, modules).

-doc false.
nc() ->
    {ok, Mods} = ms(),
    nc(Mods).

-doc false.
nc(all) ->
    _ = application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    _ = application:unload(?APPLICATION),
	    nc(Mods);
	_ ->
	    {error, not_found}
    end;
nc(Mods) when is_list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, compile)].

-doc false.
ni() -> 
    case ms() of
	{ok, Mods} ->
	    ni(Mods);
	Error ->
	    Error
    end.

-doc false.
ni(all) -> 
    _ = application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    _ = application:unload(?APPLICATION),
	    ni(Mods);
	_ ->
	    {error, not_found}
    end;
ni(Mods) when is_list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, interpret)].

load(Mod, How) when is_atom(Mod) ->
    case try_load(Mod, How) of
	ok ->
	    ok;
	_ ->
	    io:format( "~n RETRY ~p FROM: ", [Mod]),
	    ModString = atom_to_list(Mod) ++ ".erl",
	    LibDir = code:lib_dir(?APPLICATION),
	    case find_file([LibDir], ModString) of
		{ok, Abs} ->
		    load(Abs, How);
		{error, Reason} ->
		    io:format( " *** ERROR *** ~p~n", [Reason]),
		    {error, Reason}
	    end
    end;
load(Abs, How) ->
    case try_load(Abs, How) of
	ok ->
	    ok;
	{error, Reason} ->
	    io:format( " *** ERROR *** ~p~n", [Reason]),
	    {error, Reason}
    end.

try_load(Mod, How) ->
    io:format( " ~p ", [Mod]),
    Flags = [{d, debug}],
    case How of
	compile ->
	    case catch c:nc(Mod, Flags) of
		{ok, _} -> ok;
		Other   -> {error, Other}
	    end;
	interpret ->
	    case catch int:ni(Mod, Flags) of
		{module, _} -> ok;
		Other       -> {error, Other}
	    end
    end.

find_file([Dir | Dirs], File) ->
    case file:list_dir(Dir) of
	{ok, List} ->
	    case lists:member(File, List) of
		true ->
		    {ok, filename:join([Dir, File])};
		false ->
		    SubDirs = [filename:join([Dir, Sub]) || Sub <- List],
		    case find_file(SubDirs, File) of
			{ok, Abs} ->
			    {ok, Abs};
			{error, _Reason} ->
			    find_file(Dirs, File)
		    end
	    end;
	{error, _Reason} ->
	    find_file(Dirs, File)
    end;
find_file([], File) ->
    {error, {no_such_file, File}}.


%%-----------------------------------------------------------------

%% -----------------------------
%% These functions can be used instead of the et tool for
%% managing trace of the megaco application.

%%-----------------------------------------------------------------
%% enable_trace(Level, Destination) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%% Destination -> File | Port | io | {io, Verbosity} | HandlerSpec
%% File -> string()
%% Port -> integer()
%% Verbosity -> true | false
%% HandlerSpec = {function(), Data}
%% Data = term()
%%
%% Description:
%% This function is used to start tracing at level Level and send
%% the result either to the file File or the port Port. Note that
%% it starts a tracer server.
%% When Destination is the atom io (or the tuple {io, Verbosity}), 
%% all (printable) megaco trace events (trace_ts events which has 
%% Severity withing Limit) will be written to stdout using io:format. 
%% 
%%-----------------------------------------------------------------

-doc """
This function is used to start megaco tracing at a given `Level` and direct
result to the given `Destination`.

It starts a tracer server and then sets the proper match spec (according to
`Level`).

In the case when `Destination` is `File`, the printable megaco trace events will
be printed to the file `File` using plain `io:format/2`.

In the case when `Destination` is `io`, the printable megaco trace events will
be printed on stdout using plain `io:format/2`.

See `dbg` for further information.

""".
-spec enable_trace(Level, Destination) -> void() when
      Level       :: trace_level(),
      Destination :: File | Port | HandlerSpec | io,
      File        :: string(),
      Port        :: integer(),
      HandlerSpec :: {HandlerFun, InitialData},
      HandlerFun  :: trace_handler(),
      InitialData :: trace_data().

enable_trace(Level, File) when is_list(File) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    HandleSpec = {fun handle_trace/2, Fd},
	    dbg:tracer(process, HandleSpec),
	    set_trace(Level);
	Err ->
	    Err
    end;
enable_trace(Level, Port) when is_integer(Port) ->
    dbg:tracer(port, dbg:trace_port(ip, Port)),
    set_trace(Level);
enable_trace(Level, io) ->
    HandleSpec = {fun handle_trace/2, standard_io},
    dbg:tracer(process, HandleSpec),
    set_trace(Level);
enable_trace(Level, {Fun, _Data} = HandleSpec) when is_function(Fun) ->
    dbg:tracer(process, HandleSpec),
    set_trace(Level).


%%-----------------------------------------------------------------
%% disable_trace() -> void()
%% 
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------

-doc """
This function is used to stop megaco tracing.

""".
-spec disable_trace() -> void().

disable_trace() ->
    %% This is to make handle_trace/2 close the output file (if the
    %% event gets there before dbg closes)
    report_event(stop_trace, stop_trace, stop_trace, stop_trace, stop_trace),
    dbg:stop().


%%-----------------------------------------------------------------
%% set_trace(Level) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%%
%% Description:
%% This function is used to change the trace level when tracing has
%% already been started. 
%%-----------------------------------------------------------------

-doc """
This function is used to change the megaco trace level.

It is assumed that tracing has already been enabled (see `enable_trace` above).

""".
-spec set_trace(Level) -> void() when
      Level :: trace_level().

set_trace(Level) ->
    Pat = et_selector:make_pattern({?MODULE, Level}),
    et_selector:change_pattern(Pat).



%%-----------------------------------------------------------------
%% report_event(DetailLevel, FromTo, Label, Contents) -> void()
%% report_event(DetailLevel, From, To, Label, Contents) -> void()
%% 
%% Description:
%% This is the function tracing is done for.
%% Trace macros used by the megaco app all call this function.
%%-----------------------------------------------------------------


-doc false.
report_event(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

-doc false.
report_event(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.
    

%% ----------------------------------------------------------------------
%% handle_trace(Event, Verbosity) -> Verbosity
%% 
%% Parameters:
%% Event -> The trace event (only megaco 'trace_ts' events are printed)
%% Verbosity -> max | min | integer() (see Level above)
%%
%% Description:
%% This function is "receive" and print the trace events. 
%% Events are printed if:
%%   - Verbosity is max
%%   - Severity is =< Verbosity (e.g. Severity = 30, and Verbosity = 40)
%% Events are not printed if:
%%   - Verbosity is min
%%   - Severity is > Verbosity
%%-----------------------------------------------------------------

handle_trace(_, closed_file = Fd) ->
    Fd;
handle_trace({trace_ts, _Who, call, 
	      {?MODULE, report_event, 
	       [stop_trace, stop_trace, stop_trace, stop_trace, stop_trace]}, 
	      _Timestamp}, 
	     standard_io = Fd) ->
    Fd;
handle_trace({trace_ts, _Who, call, 
	      {?MODULE, report_event, 
	       [stop_trace, stop_trace, stop_trace, stop_trace, stop_trace]}, 
	      Timestamp}, 
	     Fd) ->
    (catch io:format(Fd, "stop trace at ~s~n", [format_timestamp(Timestamp)])),
    (catch file:close(Fd)),
    closed_file;
handle_trace({trace_ts, Who, call, 
	      {?MODULE, report_event, 
	       [Sev, From, To, Label, Content]}, Timestamp}, 
	     Fd) ->
    (catch print_megaco_trace(Fd, Sev, Who, Timestamp, Label, From, To, Content)),
    Fd;
handle_trace(Event, Fd) ->
    (catch print_trace(Fd, Event)),
    Fd.


print_megaco_trace(Fd, Sev, Who, Timestamp, Label, From, To, Content) ->
    Ts = format_timestamp(Timestamp),
    io:format(Fd, "[megaco trace ~w ~w ~s] ~s "
	      "~n   From:     ~p"
	      "~n   To:       ~p"
	      "~n   Content:  ~p"
	      "~n", 
	      [Sev, Who, Ts, Label, From, To, Content]).
    
print_trace(Fd, {trace, Who, What, Where}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Who, What, Where]);

print_trace(Fd, {trace, Who, What, Where, Extra}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Who, What, Where, Extra]);

print_trace(Fd, {trace_ts, Who, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Ts, Who, What, Where]);

print_trace(Fd, {trace_ts, Who, What, Where, Extra, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Ts, Who, What, Where, Extra]);

print_trace(Fd, {seq_trace, What, Where}) ->
    io:format(Fd, "[seq trace]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [What, Where]);

print_trace(Fd, {seq_trace, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[seq trace ~s]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [Ts, What, Where]);

print_trace(Fd, {drop, Num}) ->
    io:format(Fd, "[drop trace] ~p~n", [Num]);

print_trace(Fd, Trace) ->
    io:format(Fd, "[trace] "
              "~n   ~p"
              "~n", [Trace]).


%% ---------------------------------------------------------------------------
%% # formated_timstamp/0,     formated_timstamp/1
%% # format_short_timstamp/0, format_short_timstamp/1
%% # format_long_timstamp/0,  format_long_timstamp/1
%% 
%% Create a formatted timestamp. Short means that it will not include 
%% the date in the formatted timestamp. Also it will only include millis.
%% ---------------------------------------------------------------------------

-doc false.
formated_timestamp() ->
    formated_long_timestamp().

-doc false.
formated_short_timestamp() ->
    format_short_timestamp(os:timestamp()).

-doc false.
formated_long_timestamp() ->
    format_long_timestamp(os:timestamp()).


%% ---------------------------------------------------------------------------
%% # format_timstamp/1, format_timstamp/2
%% # format_short_timstamp/1, format_short_timstamp/2
%% # format_long_timstamp/1, format_long_timstamp/2
%% 
%% Formats the provided timestamp. Short means that it will not include 
%% the date in the formatted timestamp.
%% ---------------------------------------------------------------------------

-doc false.
-spec format_timestamp(Now :: erlang:timestamp()) ->
    string().

format_timestamp(Now) ->
    format_long_timestamp(Now).

-doc false.
-spec format_short_timestamp(Now :: erlang:timestamp()) ->
    string().

format_short_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(short, Now, N2T).

-doc false.
-spec format_long_timestamp(Now :: erlang:timestamp()) ->
    string().

format_long_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(long, Now, N2T).

-doc false.
-spec format_timestamp(Now :: erlang:timestamp(), 
                       N2T :: function()) ->
    string().

format_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_long_timestamp(Now, N2T).

-doc false.
-spec format_short_timestamp(Now :: erlang:timestamp(), 
                             N2T :: function()) ->
    string().

format_short_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_timestamp(short, Now, N2T).

-doc false.
-spec format_long_timestamp(Now :: erlang:timestamp(), 
                            N2T :: function()) ->
    string().

format_long_timestamp(Now, N2T) when is_tuple(Now) andalso is_function(N2T) ->
    format_timestamp(long, Now, N2T).

format_timestamp(Format, {_N1, _N2, N3} = Now, N2T) ->
    {Date, Time} = N2T(Now),
    do_format_timestamp(Format, Date, Time, N3).

do_format_timestamp(short, _Date, Time, N3) ->
    do_format_short_timestamp(Time, N3);
do_format_timestamp(long, Date, Time, N3) ->
    do_format_long_timestamp(Date, Time, N3).
    
do_format_long_timestamp(Date, Time, N3) ->
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                      [YYYY, MM, DD, Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatDate).

do_format_short_timestamp(Time, N3) ->
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w", 
                      [Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatDate).

