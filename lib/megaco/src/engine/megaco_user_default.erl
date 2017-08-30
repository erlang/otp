%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% Purpose: Default implementation of user callbacks
%%----------------------------------------------------------------------

-module(megaco_user_default).

-behaviour(megaco_user).

-export([
         handle_connect/2, handle_connect/3,
         handle_disconnect/3,
         handle_syntax_error/3,  handle_syntax_error/4, 
         handle_message_error/3, handle_message_error/4, 
         handle_trans_request/3, handle_trans_request/4, 
         handle_trans_long_request/3,  handle_trans_long_request/4, 
         handle_trans_reply/4,         handle_trans_reply/5, 
         handle_trans_ack/4,           handle_trans_ack/5, 
	 handle_unexpected_trans/3,    handle_unexpected_trans/4, 
	 handle_trans_request_abort/4, handle_trans_request_abort/5, 
         handle_segment_reply/5,       handle_segment_reply/6
        ]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").


%%----------------------------------------------------------------------
%% Invoked when a new connection is established
%%----------------------------------------------------------------------

handle_connect(_ConnHandle, _ProtocolVersion) ->
    ok.

handle_connect(_ConnHandle, _ProtocolVersion, _ConnectInfo) ->
    ok.


%%----------------------------------------------------------------------
%% Invoked when a connection is teared down
%%----------------------------------------------------------------------

handle_disconnect(ConnHandle, _ProtocolVersion, Reason) ->
    megaco:cancel(ConnHandle, Reason), % Cancel the outstanding messages
    ok.


%%----------------------------------------------------------------------
%% Invoked when  a received message had syntax errors
%%----------------------------------------------------------------------

handle_syntax_error(_ReceiveHandle, _ProtocolVersion, _ErrorDescriptor) ->
    reply.

handle_syntax_error(_ReceiveHandle, _ProtocolVersion, _ErrorDescriptor, _Extra) ->
    reply.


%%----------------------------------------------------------------------
%% Invoked when a received message contained no transactions
%%----------------------------------------------------------------------

handle_message_error(_ConnHandle, _ProtocolVersion, _ErrorDescriptor) ->
    no_reply.

handle_message_error(_ConnHandle, _ProtocolVersion, _ErrorDescriptor, _Extra) ->
    no_reply.


%%----------------------------------------------------------------------
%% Invoked for each transaction request
%%----------------------------------------------------------------------

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests) ->
    Extra = ?default_user_callback_extra, 
    handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests, Extra).

handle_trans_request(_ConnHandle, ProtocolVersion, _ActionRequests, _Extra) ->
    case ProtocolVersion of
	1 ->
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
				    errorText = "Trans requests not handled"},
	    {discard_ack, ED};
	_ ->
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_version_not_supported,
				    errorText = "Only version 1 is supported"},
	    {discard_ack, ED}
    end.


%%----------------------------------------------------------------------
%% Optionally invoked for a time consuming transaction request
%%----------------------------------------------------------------------

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData) ->
    Extra = ?default_user_callback_extra, 
    handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData, Extra).

handle_trans_long_request(_ConnHandle, _ProtocolVersion, _ReqData, _Extra) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                            errorText = "Long trans requests not handled"},
    {discard_ack,  ED}.


%%----------------------------------------------------------------------
%% Optionally invoked for a transaction reply
%%----------------------------------------------------------------------

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData) ->
    Extra = ?default_user_callback_extra, 
    handle_trans_reply(ConnHandle, ProtocolVersion, 
		       ActualReply, ReplyData, Extra).

handle_trans_reply(ConnHandle, _, {error, {send_message_failed, Reason}}, _, _Extra) ->
    megaco:disconnect(ConnHandle, {send_message_failed, Reason}),
    ok;
handle_trans_reply(_ConnHandle, _ProtocolVersion, _ActualReply, _ReplyData, _Extra) ->
    ok.


%%----------------------------------------------------------------------
%% Optionally invoked for a transaction acknowledgement
%%----------------------------------------------------------------------

handle_trans_ack(_ConnHandle, _ProtocolVersion, _AckStatus, _AckData) ->
    ok.

handle_trans_ack(_ConnHandle, _ProtocolVersion, _AckStatus, _AckData, _Extra) ->
    ok.


%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_unexpected_trans(_ConnHandle, _ProtocolVersion, _Trans) ->
    ok.

handle_unexpected_trans(_ConnHandle, _ProtocolVersion, _Trans, _Extra) ->
    ok.


%%----------------------------------------------------------------------
%% Invoked when an transaction has been aborted
%% This happens when the originating pending limit has been exceeded
%%----------------------------------------------------------------------

handle_trans_request_abort(_ConnHandle, _ProtocolVersion, _TransId, _Pid) ->
    ok.

handle_trans_request_abort(_ConnHandle, _ProtocolVersion, _TransId, _Pid, _Extra) ->
    ok.


%%----------------------------------------------------------------------
%% Invoked a segment reply has been received and the user has set
%% config option segment_reply_ind = true.
%%----------------------------------------------------------------------

handle_segment_reply(_ConnHandle, _ProtocolVersion, _TransId, _SN, _SC) ->
    ok.

handle_segment_reply(_ConnHandle, _ProtocolVersion, _TransId, _SN, _SC, _Extra) ->
    ok.

