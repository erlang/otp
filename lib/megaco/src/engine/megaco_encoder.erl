%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2019. All Rights Reserved.
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
%% Purpose: Megaco encoder behaviour module
%%----------------------------------------------------------------------

-module(megaco_encoder).

-export_type([
              protocol_version/0,
              segment_no/0,
              megaco_message/0,
              transaction/0,
              transaction_request/0,
              transaction_pending/0,
              transaction_reply/0,
              transaction_response_ack/0,
              transaction_ack/0,
              segment_reply/0,
              action_request/0,
              action_reply/0,
              command_request/0,
              error_desc/0
             ]).


-include("megaco_message_internal.hrl").

-type protocol_version() :: integer().
-type segment_no()       :: integer().
-type megaco_message() :: #'MegacoMessage'{}.
-type transaction()    :: {transactionRequest,     transaction_request()}      |
                          {transactionPending,     transaction_reply()}        |
                          {transactionReply,       transaction_pending()}      |
                          {transactionResponseAck, transaction_response_ack()} |
                          {segmentReply,           segment_reply()}.
-type transaction_request()      :: #'TransactionRequest'{}.
-type transaction_pending()      :: #'TransactionPending'{}.
%% The problem with TransactionReply is that its definition depend
%% on which version of the protocol we are using. As of version 3,
%% it has two more fields.
%% -type transaction_reply()        :: #'TransactionReply'{}.
-type transaction_reply()        :: {'TransactionReply', _, _} |
                                    {'TransactionReply', _, _, _, _}.
-type transaction_response_ack() :: [transaction_ack()].
-type transaction_ack()          :: #'TransactionAck'{}.
-type segment_reply()            :: #'SegmentReply'{}.
%% -type action_request()           :: #'ActionRequest'{}.
-type action_request()           :: {'ActionRequest', _, _, _, _}.
%% -type action_reply()             :: #'ActionReply'{}.
-type action_reply()             :: {'ActionReply', _, _, _}.
%% -type command_request()           :: #'CommandRequest'{}.
-type command_request()          :: {'CommandRequest', _, _, _}.
-type error_desc()               :: #'ErrorDescriptor'{}.

-callback encode_message(EncodingConfig,
                         Version,
                         Message) -> {ok, Bin} | Error when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      Message        :: megaco_message(),
      Bin            :: binary(),
      Error          :: term().

-callback decode_message(EncodingConfig,
                         Version,
                         Bin) -> {ok, Message} | Error when
      EncodingConfig :: list(),
      Version        :: protocol_version() | dynamic,
      Bin            :: binary(),
      Message        :: megaco_message(),
      Error          :: term().

-callback decode_mini_message(EncodingConfig,
                              Version,
                              Bin) -> {ok, Message} | Error when
      EncodingConfig :: list(),
      Version        :: protocol_version() | dynamic,
      Bin            :: binary(),
      Message        :: megaco_message(),
      Error          :: term().

-callback encode_transaction(EncodingConfig,
                             Version,
                             Transaction) -> {ok, Bin} | {error, Reason} when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      Transaction    :: transaction(),
      Bin            :: binary(),
      Reason         :: not_implemented | term().

-callback encode_action_requests(EncodingConfig,
                                 Version,
                                 ARs) -> {ok, Bin} | {error, Reason} when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      ARs            :: [action_request()],
      Bin            :: binary(),
      Reason         :: not_implemented | term().

-callback encode_action_reply(EncodingConfig,
                              Version,
                              AR) -> {ok, Bin} | {error, Reason} when
      EncodingConfig :: list(),
      Version        :: protocol_version(),
      AR             :: action_reply(),
      Bin            :: binary(),
      Reason         :: not_implemented | term().

-optional_callbacks(
   [
    encode_action_reply/3 % Only used if segementation is used
   ]).
