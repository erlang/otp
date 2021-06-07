%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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
%% This callback functions are the default! Its possible for the user to
%% provide a arbitrary number of "extra" arguments via the user_args
%% config option.
%% So, for instance, the handle_connect/2 could instead become
%% handle_connect/4 if the user sets the user_args option to [foo, bar].
%% This means that its impossible to define a proper behaviour.
%% So what we do here is to define a behaviour with the "default interface"
%% (the user_args option has the [] as the default value) and set them
%% all to be optional!
%%-------------------------------------------------------------------------

-module(megaco_user).

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

-callback handle_connect(ConnHandle, ProtocolVersion) -> 
    ok | error | {error, ErrorDescr} when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ErrorDescr      :: megaco_encoder:error_desc().
-callback handle_connect(ConnHandle, ProtocolVersion, Extra) -> 
    ok | error | {error, ErrorDescr} when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Extra           :: term(),
      ErrorDescr      :: megaco_encoder:error_desc().

-callback handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    megaco:void() when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Reason          :: term().

-callback handle_syntax_error(ReceiveHandle, ProtocolVersion, DefaultED) -> 
    reply | {reply, ED} | no_reply | {no_reply, ED} when
      ReceiveHandle   :: receive_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      DefaultED       :: megaco_encoder:error_desc(),
      ED              :: megaco_encoder:error_desc().
-callback handle_syntax_error(ReceiveHandle, ProtocolVersion, DefaultED, Extra) -> 
    reply | {reply, ED} | no_reply | {no_reply, ED} when
      ReceiveHandle   :: receive_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      DefaultED       :: megaco_encoder:error_desc(),
      ED              :: megaco_encoder:error_desc(),
      Extra           :: term().

-callback handle_message_error(ConnHandle, ProtocolVersion, ErrorDescr) ->
    megaco:void() when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ErrorDescr      :: megaco_encoder:error_desc().
-callback handle_message_error(ConnHandle, ProtocolVersion, ErrorDescr, Extra) ->
    megaco:void() when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      ErrorDescr      :: megaco_encoder:error_desc(),
      Extra           :: term().

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

-callback handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Trans           :: megaco_encoder:transaction_pending() |
                         megaco_encoder:transaction_reply() |
                         megaco_encoder:transaction_response_ack().
-callback handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans, Extra) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      Trans           :: megaco_encoder:transaction_pending() |
                         megaco_encoder:transaction_reply() |
                         megaco_encoder:transaction_response_ack(),
      Extra           :: term().

-callback handle_trans_request_abort(ConnHandle,
                                     ProtocolVersion,
                                     TransNo,
                                     Pid) ->
    ok when
      ConnHandle      :: conn_handle(),
      ProtocolVersion :: megaco_encoder:protocol_version(),
      TransNo         :: integer(),
      Pid             :: undefined | pid().
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
