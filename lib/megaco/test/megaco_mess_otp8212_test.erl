%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% Purpose: A fun implementation of user callbacks
%%----------------------------------------------------------------------

-module(megaco_mess_otp8212_test).

-behaviour(megaco_user).

%% Megaco user callback exports
-export([
         handle_connect/2, handle_connect/3, 
         handle_disconnect/3,
         %% handle_syntax_error/3,        handle_syntax_error/4, 
         %% handle_message_error/3,       handle_message_error/4, 
         handle_trans_request/3,       %% handle_trans_request/4, 
         %% handle_trans_long_request/3,  handle_trans_long_request/4,
         %% handle_trans_reply/4,         handle_trans_reply/5, 
         %% handle_trans_ack/4,           handle_trans_ack/5,
	 handle_unexpected_trans/3,    handle_unexpected_trans/4 %% ,
         %% handle_trans_request_abort/4, handle_trans_request_abort/5,
         %% handle_segment_reply/5,       handle_segment_reply/6
        ]).

%% Megaco encoder callback exports
-export([
	 encode_message/3,
	 decode_message/3
	]).

%% Megaco transport callback exports
-export([
	 send_message/2
	]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").


%%----------------------------------------------------------------------
%% Megaco user callback
%%----------------------------------------------------------------------

%% -- handle_connect/2 --

handle_connect(_, _) ->
    %% i("handle_connect -> entry"),
    ok.

handle_connect(_, _, otp8212_extra) ->
    %% i("handle_connect -> entry"),
    ok;
handle_connect(_, _, {otp8212_extra, _}) ->
    %% i("handle_connect -> entry"),
    ok.

handle_disconnect(Conn, _, {user_disconnect, {otp8212_done, Pid}}) ->
    %% i("handle_disconnect -> entry"),
    Pid ! {disconnected, Conn}, 
    ok.

handle_trans_request(_, _, _) -> %% incoming SC
    %% i("handle_trans_request -> entry"),
    {discard_ack, ["sc reply"]}.

handle_unexpected_trans(_ConnHandle, _ProtocolVersion, _Trans) ->
%%     i("handle_unexpected_trans -> entry with"
%%       "~n   ConnHandle: ~p"
%%       "~n   ProtocolVersion: ~p"
%%       "~n   Trans: ~p", [ConnHandle, ProtocolVersion, Trans]),
    ok.
handle_unexpected_trans(_ConnHandle, _ProtocolVersion, _Trans, {otp8212_extra, Pid}) ->
%%     i("handle_unexpected_trans -> entry with"
%%       "~n   ConnHandle: ~p"
%%       "~n   ProtocolVersion: ~p"
%%       "~n   Trans: ~p", [ConnHandle, ProtocolVersion, Trans]),
    Pid ! {handle_unexpected_trans, otp8212_extra}, 
    ok.


%%----------------------------------------------------------------------
%% Megaco encoder callback
%%----------------------------------------------------------------------


%% Should only be encoding MGC's outgoing request, which we expect
%% has transaction id = 1.

-define(REQUEST(Id, A),
        #'MegacoMessage'
        {mess
         = #'Message'
           {version = 1,
            mId = {deviceName,"MGC"},
            messageBody
            = {transactions, [{transactionRequest,
                               #'TransactionRequest'{transactionId = Id,
                                                     actions = A}}]}}}).

-define(REPLY(A),
        #'MegacoMessage'
        {mess
         = #'Message'
           {version = 1,
            mId = {deviceName,"MGC"},
            messageBody
            = {transactions,
               [{transactionReply,
                 #'TransactionReply'{transactionResult
                                     = {actionReplies, [A]}}}]}}}).

request() ->
    list_to_binary("!/1 MGC T=1{C=-{SC=ROOT{SV{MT=RS,RE=\"901\"}}}}").

sc_reply() ->
    list_to_binary("!/1 MGC P=19731{C=-{SC=root}}").

encode_message(_, _, ?REQUEST(1, "action request")) ->
    %% i("encode_message -> entry with request"),
    {ok,  request()};

encode_message(_, _, ?REPLY("sc reply")) ->
    %% i("encode_message -> entry with reply"),
    {ok, sc_reply()}.

decode_message(_, V248, Bin) ->
    %% i("decode_message -> entry"),
    megaco_compact_text_encoder:decode_message([], V248, Bin).


%%----------------------------------------------------------------------
%% Megaco transport callback
%%----------------------------------------------------------------------

%% Outgoing SC reply.
%% send_message(otp8212_scr, _) ->
%%     i("send_message(scr) -> entry"),
%%     ok;

%% Outgoing request: fake reception of the the reply.
send_message({RH, ControlPid, _, WrongMidStr}, _) ->
    %% i("send_message -> entry"),
    spawn(fun() -> receive_reply(200, RH, ControlPid, WrongMidStr) end),
    ok.

receive_reply(After, RH, ControlPid, WrongMidStr) ->
    timer:sleep(After),
    %% i("receive_reply -> issue reply"),
    megaco:process_received_message(RH, ControlPid, 
				    otp8212_sendhandle, 
				    reply(WrongMidStr), 
				    {otp8212_extra, ControlPid}).

reply(WrongMidStr) ->  %% note "wrong" mid.
    list_to_binary("!/1 " ++ WrongMidStr ++ " P=1{C=-{SC=root}}").

%% i(F) ->
%%     i(F, []).

%% i(F, A) ->
%%     io:format(F ++ "~n", A).
