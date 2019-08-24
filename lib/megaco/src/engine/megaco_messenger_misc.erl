%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%% Purpose: Misc functions used both from the megaco_messenger module
%%          and the megaco_ack_sender module.
%% 
%%----------------------------------------------------------------------

-module(megaco_messenger_misc).

%% Application internal export
-export([encode_body/3,
	 encode_trans_request/2,
	 encode_trans_reply/2,
	 encode_actions/3,
	 send_body/3,
	 send_message/3,

	 transform_transaction_reply/2
        ]).

%% Test functions
-export([compose_message/3, encode_message/2]).


-include_lib("megaco/include/megaco.hrl").
-include("megaco_message_internal.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").

-define(MSG_HDR_SZ, 128). % This is just a guess...

-ifdef(MEGACO_TEST_CODE).
-define(SIM(Other,Where),
	fun(Afun,Bfun) ->
		Kfun = {?MODULE,Bfun},
		case (catch ets:lookup(megaco_test_data, Kfun)) of
		    [{Kfun,Cfun}] ->
			Cfun(Afun);
		    _ ->
			Afun
		end
	end(Other,Where)).
-define(TC_AWAIT_SEND_EVENT(SendFunction),
        case megaco_tc_controller:lookup(send_function) of
            {value, {Tag, Pid}} when is_pid(Pid) ->
                Pid ! {Tag, self(), SendFuncion},
                receive
                    {Tag, Pid} ->
                        ok
                end;
            _ ->
                ok
        end).
-else.
-define(SIM(Other,Where),Other).
-define(TC_AWAIT_SEND_EVENT(_),ok).
-endif.


%%----------------------------------------------------------------------
%% Encode the transaction request
%%----------------------------------------------------------------------

encode_trans_request(CD, TR) when is_record(TR, 'TransactionRequest') ->
    ?report_debug(CD, "encode trans request", [TR]),
    Trans = {transactionRequest, TR},
    encode_transaction(CD, Trans).

encode_trans_reply(#conn_data{segment_send     = SegSend, 
			      max_pdu_size     = Max,
			      protocol_version = V} = CD, Reply) 
  when (SegSend == infinity) or (is_integer(SegSend) and (SegSend > 0)) and 
       is_integer(V) and (V >= 3) and 
       is_integer(Max) and (Max >= ?MSG_HDR_SZ) ->
    (catch encode_segmented_trans_reply(CD, Reply));
encode_trans_reply(CD, TR) when is_record(TR, megaco_transaction_reply) ->
    ?report_debug(CD, "encode trans reply", [TR]),
    Trans = {transactionReply, transform_transaction_reply(CD, TR)},
    encode_transaction(CD, Trans);
encode_trans_reply(CD, TR) when is_tuple(TR) and 
				(element(1, TR) == 'TransactionReply') ->
    ?report_debug(CD, "encode trans reply", [TR]),
    Trans = {transactionReply, TR},
    encode_transaction(CD, Trans).


encode_segmented_trans_reply(#conn_data{max_pdu_size = Max} = CD, Rep) ->
    #megaco_transaction_reply{transactionResult = Res1} = Rep,
    case Res1 of
	{actionReplies, AR} when is_list(AR) andalso (length(AR) >= 1) ->
	    case encode_action_replies(CD, AR) of
		{Size, EncodedARs} when Size =< (Max - ?MSG_HDR_SZ) ->
		    ?report_debug(CD, "action replies encoded size ok", 
				  [Size, Max]),
		    %% No need to segment message: within size limit
		    Res2 = {actionReplies, EncodedARs}, 
		    TR = Rep#megaco_transaction_reply{transactionResult = Res2},
		    TR2   = transform_transaction_reply(CD, TR), 
		    Trans = {transactionReply, TR2},
		    encode_transaction(CD, Trans);

		{Size, EncodecARs} ->
		    ?report_debug(CD, 
				  "action replies encoded size to large - "
				  "segment", 
				  [Size, Max]),
		    %% Over size limit, so go segment the message
		    encode_segments(CD, Rep, EncodecARs)
	    end;
	_ ->
	    TR    = transform_transaction_reply(CD, Rep), 
	    Trans = {transactionReply, TR},
	    encode_transaction(CD, Trans)
    end.

encode_segments(CD, Reply, EncodecARs) ->
    encode_segments(CD, Reply, EncodecARs, 1, []).

encode_segments(CD, Reply, [EncodedAR], SN, EncodedSegs) ->
    Bin = encode_segment(CD, Reply, EncodedAR, SN, 'NULL'),
    {ok, lists:reverse([{SN, Bin}|EncodedSegs])};
encode_segments(CD, Reply, [EncodedAR|EncodedARs], SN, EncodedSegs) ->
    Bin = encode_segment(CD, Reply, EncodedAR, SN, asn1_NOVALUE),
    encode_segments(CD, Reply, EncodedARs, SN + 1, [{SN, Bin}|EncodedSegs]).

encode_segment(CD, Reply, EncodedAR, SN, SC) ->
    Res   = {actionReplies, [EncodedAR]},
    TR0   = Reply#megaco_transaction_reply{transactionResult    = Res,
					   segmentNumber        = SN,
					   segmentationComplete = SC},
    TR    = transform_transaction_reply(CD, TR0), 
    Trans = {transactionReply, TR},
    case encode_transaction(CD, Trans) of
	{ok, Bin} ->
	    Bin;
	Error ->
	    throw(Error)
    end.


encode_transaction(#conn_data{protocol_version = V,
			      encoding_mod     = EM,
			      encoding_config  = EC} = CD, Trans) ->
    case (catch EM:encode_transaction(EC, V, Trans)) of
	{ok, Bin} ->
	    ?SIM({ok, Bin}, encode_trans);
        {'EXIT', {undef, _}} ->
            {error, not_implemented};
	{error, not_implemented} = Error1 ->
	    Error1;
	{error, Reason} ->
	    incNumErrors(CD#conn_data.conn_handle),	    
            {error, {EM, encode_transaction, [EC, V, Trans], Reason}};
	Error2 ->
	    incNumErrors(CD#conn_data.conn_handle),	    
            {error, {EM, encode_transaction, [EC, V, Trans], Error2}}
    end.


%%----------------------------------------------------------------------
%% Encode the action request's
%%----------------------------------------------------------------------

encode_actions(#conn_data{protocol_version = V} = CD, TraceLabel, ARs) ->
    ?report_debug(CD, TraceLabel, [ARs]),

    %% Encode the actions
    EM = CD#conn_data.encoding_mod,
    EC = CD#conn_data.encoding_config,
    case (catch EM:encode_action_requests(EC, V, ARs)) of
        {ok, Bin} when is_binary(Bin) ->
            ?SIM({ok, Bin}, encode_actions);
        {'EXIT', {undef, _}} ->
            incNumErrors(CD#conn_data.conn_handle),
            Reason = not_implemented,
            {error, {EM, encode_action_requests, [EC, ARs], Reason}};
        {error, Reason} ->
	    incNumErrors(CD#conn_data.conn_handle),	    
            {error, {EM, encode_action_requests, [EC, ARs], Reason}};
        Error ->
	    incNumErrors(CD#conn_data.conn_handle),	    
            {error, {EM, encode_action_requests, [EC, ARs], Error}}
    end.


%%----------------------------------------------------------------------
%% Encode the action reply's
%%----------------------------------------------------------------------

encode_action_replies(CD, AR) ->
    encode_action_replies(CD, AR, 0, []).

encode_action_replies(_, [], Size, Acc) ->
    {Size, lists:reverse(Acc)};
encode_action_replies(#conn_data{protocol_version = V,
				 encoding_mod     = Mod,
				 encoding_config  = Conf} = CD, 
		      [AR|ARs], Size, Acc) ->
    case (catch Mod:encode_action_reply(Conf, V, AR)) of
	{ok, Bin} when is_binary(Bin) ->
	    encode_action_replies(CD, ARs, Size + size(Bin), [Bin|Acc]);
        {'EXIT', {undef, _}} ->
            throw({error, not_implemented});
	{error, not_implemented} = Error1 ->
	    throw(Error1);
	{error, Reason} ->
            incNumErrors(CD#conn_data.conn_handle),
	    throw({error, {Mod, encode_action_reply, [Conf, AR], Reason}});
	Error ->
            incNumErrors(CD#conn_data.conn_handle),
	    throw({error, {Mod, encode_action_reply, [Conf, AR], Error}})
    end.
				      

%%----------------------------------------------------------------------
%% Encode the message body
%%----------------------------------------------------------------------

encode_body(#conn_data{protocol_version = V} = ConnData, 
	    TraceLabel, Body) ->
    %% Create the message envelope
    MegaMsg = compose_message(ConnData, V, Body),

    ?report_debug(ConnData, TraceLabel, [MegaMsg]),

    %% Encode the message
    EM = ConnData#conn_data.encoding_mod,
    EC = ConnData#conn_data.encoding_config,
    case (catch EM:encode_message(EC, V, MegaMsg)) of
        {ok, Bin} when is_binary(Bin) ->
            ?SIM({ok, Bin}, encode_body);
        {error, Reason} ->
	    incNumErrors(ConnData#conn_data.conn_handle),	    
            {error, {EM, [EC, MegaMsg], Reason}};
        Error ->
	    incNumErrors(ConnData#conn_data.conn_handle),	    
            {error, {EM, [EC, MegaMsg], Error}}
    end.


%%----------------------------------------------------------------------
%% Compose and encode a message
%%----------------------------------------------------------------------
compose_message(#conn_data{conn_handle = CH,
			   auth_data   = MsgAuth}, V, Body) ->
    LocalMid = CH#megaco_conn_handle.local_mid,
    Msg      = #'Message'{version     = V,
			  mId         = LocalMid,
			  messageBody = Body},
    MegaMsg  = #'MegacoMessage'{authHeader = MsgAuth, % BUGBUG: Compute?
				mess       = Msg},
    MegaMsg.
    

encode_message(#conn_data{protocol_version = Version,
			  encoding_mod     = EncodingMod,
			  encoding_config  = EncodingConfig},  MegaMsg) ->
    (catch EncodingMod:encode_message(EncodingConfig, Version, MegaMsg)).


%%----------------------------------------------------------------------
%% Send the message body
%%----------------------------------------------------------------------

send_body(ConnData, TraceLabel, Body) ->
    case encode_body(ConnData, TraceLabel, Body) of
        {ok, Bin} ->
            send_message(ConnData, false, Bin);
        {error, Reason} ->
            {error, Reason}
    end.


%%----------------------------------------------------------------------
%% Send the (encoded) message
%%----------------------------------------------------------------------

send_message(#conn_data{resend_indication = flag} = ConnData, 
	     Resend, Bin) ->
    do_send_message(ConnData, send_message, Bin, [Resend]);

send_message(#conn_data{resend_indication = true} = ConnData, 
	     true, Bin) ->
    do_send_message(ConnData, resend_message, Bin, []);

send_message(ConnData, _Resend, Bin) ->
    do_send_message(ConnData, send_message, Bin, []).

do_send_message(ConnData, SendFunc, Bin, Extra) ->
    %% Send the message
    #conn_data{send_mod    = SendMod,
	       send_handle = SendHandle} = ConnData,

    ?TC_AWAIT_SEND_EVENT(SendFunc),

    ?report_trace(ConnData, "send bytes", [{bytes,     Bin}, 
					   {send_func, SendFunc}]),

    Args = [SendHandle, Bin | Extra], 
    case (catch apply(SendMod, SendFunc, Args)) of
        ok ->
            ?SIM({ok, Bin}, send_message);
        {cancel, Reason} ->
            ?report_trace(ConnData, "<CANCEL> send_message callback",
			  [{bytes, Bin}, {cancel, Reason}]),
            {error, {send_message_cancelled, Reason}};
        {error, Reason} ->
	    incNumErrors(ConnData#conn_data.conn_handle),
            ?report_important(ConnData, "<ERROR> send_message callback",
                              [{bytes, Bin}, {error, Reason}]),
	    error_msg("failed (error) sending message [using ~w] (~p):"
		      "~n~w", [SendFunc, SendHandle, Reason]),
            {error, {send_message_failed, Reason}};
        {'EXIT', Reason} = Error ->
	    incNumErrors(ConnData#conn_data.conn_handle),
            ?report_important(ConnData, "<ERROR> send_message callback",
                              [{bytes, Bin}, {exit, Reason}]),
	    error_msg("failed (exit) sending message [using ~w] (~p):"
		      "~n~w", [SendFunc, SendHandle, Reason]),
            {error, {send_message_failed, Error}};
        Reason ->
	    incNumErrors(ConnData#conn_data.conn_handle),
            ?report_important(ConnData, "<ERROR> send_message callback",
                              [{bytes, Bin}, {error, Reason}]),
	    error_msg("failed sending message [using ~w] on (~p): "
		      "~n~w", [SendFunc, SendHandle, Reason]),
            {error, {send_message_failed, Reason}}
    end.


%%%-----------------------------------------------------------------
%%% Misc internal util functions
%%%-----------------------------------------------------------------

transform_transaction_reply(#conn_data{protocol_version = V}, TR) 
  when is_integer(V) and (V >= 3) ->
    #megaco_transaction_reply{transactionId        = TransId, 
			      immAckRequired       = IAR, 
			      transactionResult    = TransRes,
			      segmentNumber        = SegNo,
			      segmentationComplete = SegComplete} = TR,
    {'TransactionReply', TransId, IAR, TransRes, SegNo, SegComplete};
transform_transaction_reply(_, TR) ->
    #megaco_transaction_reply{transactionId        = TransId, 
			      immAckRequired       = IAR, 
			      transactionResult    = TransRes} = TR,
    {'TransactionReply', TransId, IAR, TransRes}.


%%-----------------------------------------------------------------
%% Func: error_msg/2
%% Description: Send an error message
%%-----------------------------------------------------------------

error_msg(F, A) ->
    ?megaco_error(F, A).


%%-----------------------------------------------------------------
%% Func: incNumErrors/0, incNumErrors/1, incNumTimerRecovery/1
%% Description: SNMP counter increment functions
%%-----------------------------------------------------------------

incNumErrors(CH) ->
    incNum({CH, medGwyGatewayNumErrors}).

incNum(Cnt) ->
    case (catch ets:update_counter(megaco_stats, Cnt, 1)) of
	{'EXIT', {badarg, _R}} ->
	    ets:insert(megaco_stats, {Cnt, 1});
	Old ->
	    Old
    end.
	    
%% p(F, A) ->
%%     print(now(), F, A).

%% print(Ts, F, A) ->
%%     io:format("*** [~s] ~p ***"
%% 		 "~n   " ++ F ++ "~n", 
%% 		 [format_timestamp(Ts), self() | A]).

%% format_timestamp(Now) ->
%%     {_N1, _N2, N3}   = Now,
%%     {Date, Time}   = calendar:now_to_datetime(Now),
%%     {YYYY,MM,DD}   = Date,
%%     {Hour,Min,Sec} = Time,
%%     FormatDate = 
%% 	   io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
%% 			 [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
%%     lists:flatten(FormatDate).
