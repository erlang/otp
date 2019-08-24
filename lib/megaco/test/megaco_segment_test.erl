%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2019. All Rights Reserved.
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
%% Purpose: Test the segment package introduced in v3 of the megaco std.
%%----------------------------------------------------------------------
-module(megaco_segment_test).

-export([t/0, t/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([all/0,groups/0,init_per_group/2,end_per_group/2,

	
	 send_segmented_msg_plain1/1, 
	 send_segmented_msg_plain2/1, 
	 send_segmented_msg_plain3/1, 
	 send_segmented_msg_plain4/1,
	 send_segmented_msg_ooo1/1, 
	 send_segmented_msg_missing_seg_reply1/1, 
	 send_segmented_msg_missing_seg_reply2/1, 

	
	 recv_segmented_msg_plain/1, 
	 recv_segmented_msg_ooo_seg/1, 
	 recv_segmented_msg_missing_seg1/1, 
	 recv_segmented_msg_missing_seg2/1
	
	]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v3.hrl").

-define(TEST_VERBOSITY, debug).

-define(VERSION, 3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [{group, send}, {group, recv}].

groups() -> 
    [{send, [],
      [send_segmented_msg_plain1, send_segmented_msg_plain2,
       send_segmented_msg_plain3, send_segmented_msg_plain4,
       send_segmented_msg_ooo1,
       send_segmented_msg_missing_seg_reply1,
       send_segmented_msg_missing_seg_reply2]},
     {recv, [],
      [recv_segmented_msg_plain, recv_segmented_msg_ooo_seg,
       recv_segmented_msg_missing_seg1,
       recv_segmented_msg_missing_seg2]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                   %%%
%%%                 Segmented reply send test cases                   %%%
%%%                                                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_segmented_msg_plain1(suite) ->
    [];
send_segmented_msg_plain1(doc) ->
    "First plain test that it is possible to send segmented messages. "
	"Send window = infinity. ";
send_segmented_msg_plain1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ssmp1),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = ssmp1_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = ssmp1_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.



%%
%% MGC generator stuff
%%

ssmp1_mgc_event_sequence(text, tcp) ->
    DecodeFun = ssmp1_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ssmp1_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = ssmp1_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = ssmp1_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    CtxId1    = 1, 
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    CtxId2    = 2, 
    TransId   = 2,
    NotifyReq = ssmp1_mgc_notify_request_msg(Mid, TransId, 
					    TermId1, CtxId1, 
					    TermId2, CtxId2),
    NrVerifyFun1 = 
	ssmp1_mgc_verify_notify_reply_segment_msg_fun(1, false, TransId, 
						     TermId1, CtxId1),
    NrVerifyFun2 = 
	ssmp1_mgc_verify_notify_reply_segment_msg_fun(2, true, TransId, 
						     TermId2, CtxId2),
    SegmentRep1 = ssmp1_mgc_segment_reply_msg(Mid, TransId, 1, false),
    SegmentRep2 = ssmp1_mgc_segment_reply_msg(Mid, TransId, 2, true),
    TransAck    = ssmp1_mgc_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request",  {ScrVerifyFun, 5000}},
             {send, "service-change-reply",              ServiceChangeRep},
	     {expect_nothing, timer:seconds(1)}, 
             {send, "notify request",                    NotifyReq},
             {expect_receive, "notify reply: segment 1", {NrVerifyFun1, 2000}},
             {expect_receive, "notify reply: segment 2", {NrVerifyFun2, 1000}},
             {send, "segment reply 1",                   SegmentRep1},
	     {sleep, 100}, 
             {send, "segment reply 2",                   SegmentRep2},
	     {sleep, 100}, % {expect_nothing, 500}, 
	     {send, "transaction-ack",                   TransAck},
             {expect_closed,  timer:seconds(5)},
             disconnect
            ],
    EvSeq.

ssmp1_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

ssmp1_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

ssmp1_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch ssmp1_mgc_verify_service_change_req(Msg)) 
    end.

ssmp1_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("ssmp1_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

ssmp1_mgc_verify_notify_reply_segment_msg_fun(SN, Last, 
					     TransId, TermId, Cid) ->
    fun(Msg) -> 
	    (catch ssmp1_mgc_verify_notify_reply_segment(Msg, 
							SN, Last, 
							TransId, TermId, Cid)) 
    end.

ssmp1_mgc_verify_notify_reply_segment(#'MegacoMessage'{mess = Mess} = M,
				     SN, Last, TransId, TermId, Cid) ->
    io:format("ssmp1_mgc_verify_notify_reply_segment -> entry with"
	      "~n   M:       ~p"
	      "~n   SN:      ~p"
	      "~n   Last:    ~p"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, SN, Last, TransId, TermId, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = asn1_NOVALUE} when (Last == false) ->
		TransRes;
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = 'NULL'} when (Last == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
	    {actionReplies, [ActionReply]} ->
		ActionReply;
	    {actionReplies, ActionReplies} ->
		throw({error, {invalid_actionReplies, ActionReplies}});
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
	    #'ActionReply'{contextId    = Cid,
			   commandReply = [CommandReply]} ->
		CommandReply;
	    #'ActionReply'{contextId    = Cid,
			   commandReply = CommandReplies} ->
		throw({error, {invalid_commandReplies, CommandReplies}});
	    _ ->
		throw({error, {invalid_actionReply, AR}})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_NotifyReply, NR}}
    end;
ssmp1_mgc_verify_notify_reply_segment(Crap, 
				     _SN, _Last, _TransId, _TermId, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


ssmp1_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp1_mgc_notify_request_msg(Mid, TransId, TermId1, Cid1, TermId2, Cid2) ->
    AR1     = ssmp1_mgc_notify_request_ar(1, TermId1, Cid1),
    AR2     = ssmp1_mgc_notify_request_ar(2, TermId2, Cid2),
    TR      = cre_transReq(TransId, [AR1, AR2]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp1_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation(integer_to_list(19990720+Rid), 
			       integer_to_list(22000000+Rid)),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp1_mgc_segment_reply_msg(Mid, TransId, SN, Last) ->
    SR    = ssmp1_mgc_segment_reply(TransId, SN, Last),
    Trans = cre_transaction(SR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp1_mgc_segment_reply(TransId, SN, true) ->
    cre_segReply(TransId, SN, 'NULL');
ssmp1_mgc_segment_reply(TransId, SN, false) ->
    cre_segReply(TransId, SN, asn1_NOVALUE).

ssmp1_mgc_trans_ack_msg(Mid, TransId) ->
    TA    = cre_transAck(TransId),
    Trans = cre_transaction([TA]),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
ssmp1_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ssmp1_mg_verify_handle_connect_fun(),
    ServiceChangeReq = ssmp1_mg_service_change_request_ar(Mid, 1),
    ServiceChangeReplyVerify = ssmp1_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    NotifyReqVerify = ssmp1_mg_verify_notify_request_fun(Tid1, Tid2),
    AckVerify = ssmp1_mg_verify_ack_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, segment_send,     infinity}, 
	     {megaco_update_conn_info, max_pdu_size,     128}, 
             {sleep, 1000},
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify, 5000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


ssmp1_mg_verify_handle_connect_fun() ->
    fun(Ev) -> ssmp1_mg_verify_handle_connect(Ev) end.

ssmp1_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("ssmp1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
ssmp1_mg_verify_handle_connect(Else) ->
    io:format("ssmp1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


ssmp1_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> ssmp1_mg_verify_scr(Rep) end.

ssmp1_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch ssmp1_mg_do_verify_scr(AR));
ssmp1_mg_verify_scr(Crap) ->
    io:format("ssmp1_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

ssmp1_mg_do_verify_scr(AR) ->
    io:format("ssmp1_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

ssmp1_mg_verify_notify_request_fun(Tid1, Tid2) ->
    fun(Req) -> 
	    ssmp1_mg_verify_notify_request(Req, Tid1, Tid2) 
    end.
	     
ssmp1_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, [AR1, AR2]}, Tid1, Tid2) ->
    (catch ssmp1_mg_do_verify_notify_request(Tid1, Tid2, AR1, AR2));
ssmp1_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, _Tid1, _Tid2) ->
    {error, {invalid_action_requests, ARs}, ok};
ssmp1_mg_verify_notify_request(
  {handle_trans_request, CH, V, ARs}, _Tid1, _Tid2) ->
    {error, {invalid_trans_request, {CH, V, ARs}}, ok};
ssmp1_mg_verify_notify_request(Crap, _Tid1, _Tid2) ->
    io:format("ssmp1_mg_verify_notify_request -> unknown request"
	      "~n   Tid1: ~p"
	      "~n   Tid2: ~p"
	      "~n   Crap: ~p"
	      "~n", [_Tid1, _Tid2, Crap]),
    {error, {unexpected_event, Crap}, ok}.

ssmp1_mg_do_verify_notify_request(Tid1, Tid2, AR1, AR2) ->
    io:format("ssmp1_mg_do_verify_notify_request -> ok"
	      "~n   Tid1: ~p"
	      "~n   Tid2: ~p"
	      "~n   AR1:  ~p"
	      "~n   AR2:  ~p"
	      "~n", [Tid1, Tid2, AR1, AR2]),
    ActionReply1 = ssmp1_mg_do_verify_notify_request(Tid1, AR1),
    ActionReply2 = ssmp1_mg_do_verify_notify_request(Tid2, AR2),
    Reply = {{handle_ack, ssmp1}, [ActionReply1, ActionReply2]}, 
    {ok, [AR1, AR2], Reply}.

ssmp1_mg_do_verify_notify_request(Tid, AR) ->
    io:format("ssmp1_mg_do_verify_notify_request -> ok"
	      "~n   Tid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Tid, AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxId, 
			     commandRequests = [CmdReq]} ->
		{CtxId, CmdReq};
	    _ ->
		Reason1 = {invalid_actionRequest, AR},
		throw({error, Reason1, ok})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}, ok})
	end,
    OED = 
	case Cmd of
	    {notifyReq, 
	     #'NotifyRequest'{terminationID            = [Tid],
			      observedEventsDescriptor = ObsEvDesc,
			      errorDescriptor          = asn1_NOVALUE}} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_command, Cmd}, ok})
	end,
    OE = 
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    #'ObservedEventsDescriptor'{observedEventLst = ObsEvLst} ->
		throw({error, {invalid_observedEventLst, ObsEvLst}, ok});
	    _ ->
		throw({error, {invalid_ObservedEventsDescriptor, OED}, ok})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ssmp1_mg_notify_reply_ar(Cid, Tid);
	_ ->
	    throw({error, {invalid_ObservedEvent, OE}, ok})
    end.


ssmp1_mg_verify_ack_fun() ->
    fun(Event) -> ssmp1_mg_verify_ack(Event) end.

ssmp1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, ssmp1}) ->
    io:format("ssmp1_mg_verify_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
ssmp1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, CrapAckData}) ->
    io:format("ssmp1_mg_verify_ack -> error"
              "~n   CrapAckData: ~p"
              "~n   CH:          ~p"
              "~n", [CrapAckData, CH]),
    {error, {unknown_ack_data, CrapAckData, CH}, ok};
ssmp1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, 
		    BadAckStatus, BadAckData}) ->
    io:format("ssmp1_mg_verify_ack -> error"
              "~n   BadAckStatus: ~p"
              "~n   BadAckData: ~p"
              "~n   CH:          ~p"
              "~n", [BadAckStatus, BadAckData, CH]),
    {error, {unknown_ack_status, BadAckStatus, BadAckData, CH}, ok};
ssmp1_mg_verify_ack(BadEvent) ->
    {error, {unknown_event, BadEvent}, ok}.
    
	    
ssmp1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp1_mg_notify_reply_ar(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



send_segmented_msg_plain2(suite) ->
    [];
send_segmented_msg_plain2(doc) ->
    "Second plain test that it is possible to send segmented messages. "
	"Send window = infinity. ";
send_segmented_msg_plain2(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [linux]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ssmp2),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = ssmp2_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = ssmp2_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.



%%
%% MGC generator stuff
%%

ssmp2_mgc_event_sequence(text, tcp) ->
    DecodeFun = ssmp2_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ssmp2_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = ssmp2_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = ssmp2_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    CtxId1    = 1, 
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    CtxId2    = 2, 
    TransId   = 2,
    NotifyReq = ssmp2_mgc_notify_request_msg(Mid, TransId, 
					    TermId1, CtxId1, 
					    TermId2, CtxId2),
    NrVerifyFun1 = 
	ssmp2_mgc_verify_notify_reply_segment_msg_fun(1, false, TransId, 
						     TermId1, CtxId1),
    NrVerifyFun2 = 
	ssmp2_mgc_verify_notify_reply_segment_msg_fun(2, true, TransId, 
						     TermId2, CtxId2),
    SegmentRep1 = ssmp2_mgc_segment_reply_msg(Mid, TransId, 1, false),
    SegmentRep2 = ssmp2_mgc_segment_reply_msg(Mid, TransId, 2, true),
    TransAck    = ssmp2_mgc_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request",  {ScrVerifyFun, 5000}},
             {send, "service-change-reply",              ServiceChangeRep},
	     {expect_nothing, timer:seconds(1)}, 
             {send, "notify request",                    NotifyReq},
             {expect_receive, "notify reply: segment 1", {NrVerifyFun1, 2000}},
             {send, "segment reply 1",                   SegmentRep1},
             {expect_receive, "notify reply: segment 2", {NrVerifyFun2, 1000}},
             {send, "segment reply 2",                   SegmentRep2},
	     {sleep, 100}, % {expect_nothing, 500}, 
	     {send, "transaction-ack",                   TransAck},
             {expect_closed,  timer:seconds(5)},
             disconnect
            ],
    EvSeq.

ssmp2_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

ssmp2_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

ssmp2_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch ssmp2_mgc_verify_service_change_req(Msg)) 
    end.

ssmp2_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("ssmp2_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

ssmp2_mgc_verify_notify_reply_segment_msg_fun(SN, Last, 
					     TransId, TermId, Cid) ->
    fun(Msg) -> 
	    (catch ssmp2_mgc_verify_notify_reply_segment(Msg, 
							SN, Last, 
							TransId, TermId, Cid)) 
    end.

ssmp2_mgc_verify_notify_reply_segment(#'MegacoMessage'{mess = Mess} = M,
				     SN, Last, TransId, TermId, Cid) ->
    io:format("ssmp2_mgc_verify_notify_reply_segment -> entry with"
	      "~n   M:       ~p"
	      "~n   SN:      ~p"
	      "~n   Last:    ~p"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, SN, Last, TransId, TermId, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = asn1_NOVALUE} when (Last == false) ->
		TransRes;
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = 'NULL'} when (Last == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
	    {actionReplies, [ActionReply]} ->
		ActionReply;
	    {actionReplies, ActionReplies} ->
		throw({error, {invalid_actionReplies, ActionReplies}});
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
	    #'ActionReply'{contextId    = Cid,
			   commandReply = [CommandReply]} ->
		CommandReply;
	    #'ActionReply'{contextId    = Cid,
			   commandReply = CommandReplies} ->
		throw({error, {invalid_commandReplies, CommandReplies}});
	    _ ->
		throw({error, {invalid_actionReply, AR}})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_NotifyReply, NR}}
    end;
ssmp2_mgc_verify_notify_reply_segment(Crap, 
				     _SN, _Last, _TransId, _TermId, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


ssmp2_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp2_mgc_notify_request_msg(Mid, TransId, TermId1, Cid1, TermId2, Cid2) ->
    AR1     = ssmp2_mgc_notify_request_ar(1, TermId1, Cid1),
    AR2     = ssmp2_mgc_notify_request_ar(2, TermId2, Cid2),
    TR      = cre_transReq(TransId, [AR1, AR2]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp2_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation(integer_to_list(19990720+Rid), 
			       integer_to_list(22000000+Rid)),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp2_mgc_segment_reply_msg(Mid, TransId, SN, Last) ->
    SR    = ssmp2_mgc_segment_reply(TransId, SN, Last),
    Trans = cre_transaction(SR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp2_mgc_segment_reply(TransId, SN, true) ->
    cre_segReply(TransId, SN, 'NULL');
ssmp2_mgc_segment_reply(TransId, SN, false) ->
    cre_segReply(TransId, SN, asn1_NOVALUE).

ssmp2_mgc_trans_ack_msg(Mid, TransId) ->
    TA    = cre_transAck(TransId),
    Trans = cre_transaction([TA]),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
ssmp2_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ssmp2_mg_verify_handle_connect_fun(),
    ServiceChangeReq = ssmp2_mg_service_change_request_ar(Mid, 1),
    ServiceChangeReplyVerify = ssmp2_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    NotifyReqVerify = ssmp2_mg_verify_notify_request_fun(Tid1, Tid2),
    AckVerify = ssmp2_mg_verify_ack_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, segment_send,     infinity}, 
	     {megaco_update_conn_info, max_pdu_size,     128}, 
             {sleep, 1000},
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify, 5000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


ssmp2_mg_verify_handle_connect_fun() ->
    fun(Ev) -> ssmp2_mg_verify_handle_connect(Ev) end.

ssmp2_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("ssmp2_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
ssmp2_mg_verify_handle_connect(Else) ->
    io:format("ssmp2_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


ssmp2_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> ssmp2_mg_verify_scr(Rep) end.

ssmp2_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch ssmp2_mg_do_verify_scr(AR));
ssmp2_mg_verify_scr(Crap) ->
    io:format("ssmp2_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

ssmp2_mg_do_verify_scr(AR) ->
    io:format("ssmp2_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

ssmp2_mg_verify_notify_request_fun(Tid1, Tid2) ->
    fun(Req) -> ssmp2_mg_verify_notify_request(Req, Tid1, Tid2) end.
	     
ssmp2_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, [AR1, AR2]}, Tid1, Tid2) ->
    (catch ssmp2_mg_do_verify_notify_request(Tid1, Tid2, AR1, AR2));
ssmp2_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, _Tid1, _Tid2) ->
    {error, {invalid_action_requests, ARs}, ok};
ssmp2_mg_verify_notify_request(
  {handle_trans_request, CH, V, ARs}, _Tid1, _Tid2) ->
    {error, {invalid_trans_request, {CH, V, ARs}}, ok};
ssmp2_mg_verify_notify_request(Crap, _Tid1, _Tid2) ->
    io:format("ssmp2_mg_verify_notify_request -> unknown request"
	      "~n   Tid1: ~p"
	      "~n   Tid2: ~p"
	      "~n   Crap: ~p"
	      "~n", [_Tid1, _Tid2, Crap]),
    {error, {unexpected_event, Crap}, ok}.

ssmp2_mg_do_verify_notify_request(Tid1, Tid2, AR1, AR2) ->
    io:format("ssmp2_mg_do_verify_notify_request -> ok"
	      "~n   Tid1: ~p"
	      "~n   Tid2: ~p"
	      "~n   AR1:  ~p"
	      "~n   AR2:  ~p"
	      "~n", [Tid1, Tid2, AR1, AR2]),
    ActionReply1 = ssmp2_mg_do_verify_notify_request(Tid1, AR1),
    ActionReply2 = ssmp2_mg_do_verify_notify_request(Tid2, AR2),
    Reply = {{handle_ack, ssmp2}, [ActionReply1, ActionReply2]}, 
    {ok, [AR1, AR2], Reply}.

ssmp2_mg_do_verify_notify_request(Tid, AR) ->
    io:format("ssmp2_mg_do_verify_notify_request -> ok"
	      "~n   Tid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Tid, AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxId, 
			     commandRequests = [CmdReq]} ->
		{CtxId, CmdReq};
	    _ ->
		Reason1 = {invalid_actionRequest, AR},
		throw({error, Reason1, ok})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}, ok})
	end,
    OED = 
	case Cmd of
	    {notifyReq, 
	     #'NotifyRequest'{terminationID            = [Tid],
			      observedEventsDescriptor = ObsEvDesc,
			      errorDescriptor          = asn1_NOVALUE}} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_command, Cmd}, ok})
	end,
    OE = 
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    #'ObservedEventsDescriptor'{observedEventLst = ObsEvLst} ->
		throw({error, {invalid_observedEventLst, ObsEvLst}, ok});
	    _ ->
		throw({error, {invalid_ObservedEventsDescriptor, OED}, ok})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ssmp2_mg_notify_reply_ar(Cid, Tid);
	_ ->
	    throw({error, {invalid_ObservedEvent, OE}, ok})
    end.


ssmp2_mg_verify_ack_fun() ->
    fun(Event) -> ssmp2_mg_verify_ack(Event) end.

ssmp2_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, ssmp2}) ->
    io:format("ssmp2_mg_verify_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
ssmp2_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, CrapAckData}) ->
    {error, {unknown_ack_data, CrapAckData, CH}, ok};
ssmp2_mg_verify_ack({handle_trans_ack, CH, ?VERSION, 
		    BadAckStatus, BadAckData}) ->
    {error, {unknown_ack_status, BadAckStatus, BadAckData, CH}, ok};
ssmp2_mg_verify_ack(BadEvent) ->
    {error, {unknown_event, BadEvent}, ok}.
    
	    
ssmp2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp2_mg_notify_reply_ar(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



send_segmented_msg_plain3(suite) ->
    [];
send_segmented_msg_plain3(doc) ->
    "Third plain test that it is possible to send segmented messages. "
	"Send window = 1. ";
send_segmented_msg_plain3(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ssmp3),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = ssmp3_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = ssmp3_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.



%%
%% MGC generator stuff
%%

ssmp3_mgc_event_sequence(text, tcp) ->
    DecodeFun = ssmp3_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ssmp3_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = ssmp3_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = ssmp3_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    CtxId1    = 1, 
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    CtxId2    = 2, 
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    CtxId3    = 3, 
    TermId4   = 
	#megaco_term_id{id = ["00000000","00000000","00000004"]},
    CtxId4    = 4, 
    TermId5   = 
	#megaco_term_id{id = ["00000000","00000000","00000005"]},
    CtxId5    = 5, 
    TermId6   = 
	#megaco_term_id{id = ["00000000","00000000","00000006"]},
    CtxId6    = 6, 
    TermId7   = 
	#megaco_term_id{id = ["00000000","00000000","00000007"]},
    CtxId7    = 7, 
    TermId8   = 
	#megaco_term_id{id = ["00000000","00000000","00000008"]},
    CtxId8    = 8, 
    TransId   = 2,
    TermIDs = [TermId1, TermId2, TermId3, TermId4, 
	       TermId5, TermId6, TermId7, TermId8],
    CIDs    = [CtxId1, CtxId2, CtxId3, CtxId4, 
	       CtxId5, CtxId6, CtxId7, CtxId8], 
    NotifyReq = ssmp3_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs),
    NrVerifyFun1 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(1, false, TransId, 
						      TermId1, CtxId1),
    NrVerifyFun2 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(2, false, TransId, 
						      TermId2, CtxId2),
    NrVerifyFun3 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(3, false, TransId, 
						      TermId3, CtxId3),
    NrVerifyFun4 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(4, false, TransId, 
						      TermId4, CtxId4),
    NrVerifyFun5 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(5, false, TransId, 
						      TermId5, CtxId5),
    NrVerifyFun6 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(6, false, TransId, 
						      TermId6, CtxId6),
    NrVerifyFun7 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(7, false, TransId, 
						      TermId7, CtxId7),
    NrVerifyFun8 = 
	ssmp3_mgc_verify_notify_reply_segment_msg_fun(8, true, TransId, 
						      TermId8, CtxId8),
    SegmentRep1 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 1, false),
    SegmentRep2 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 2, false),
    SegmentRep3 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 3, false),
    SegmentRep4 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 4, false),
    SegmentRep5 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 5, false),
    SegmentRep6 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 6, false),
    SegmentRep7 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 7, false),
    SegmentRep8 = ssmp3_mgc_segment_reply_msg(Mid, TransId, 8, true),
    TransAck    = ssmp3_mgc_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request",  {ScrVerifyFun, 5000}},
             {send, "service-change-reply",              ServiceChangeRep},
	     {expect_nothing, 1000}, 
             {send, "notify request",                    NotifyReq},
             {expect_receive, "notify reply: segment 1", {NrVerifyFun1, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 1",                   SegmentRep1},

             {expect_receive, "notify reply: segment 2", {NrVerifyFun2, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 2",                   SegmentRep2},

             {expect_receive, "notify reply: segment 3", {NrVerifyFun3, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 3",                   SegmentRep3},

             {expect_receive, "notify reply: segment 4", {NrVerifyFun4, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 4",                   SegmentRep4},

             {expect_receive, "notify reply: segment 5", {NrVerifyFun5, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 5",                   SegmentRep5},

             {expect_receive, "notify reply: segment 6", {NrVerifyFun6, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 6",                   SegmentRep6},

             {expect_receive, "notify reply: segment 7", {NrVerifyFun7, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 7",                   SegmentRep7},

             {expect_receive, "notify reply: segment 8", {NrVerifyFun8, 1000}},
	     {expect_nothing, 200},
             {send, "segment reply 8",                   SegmentRep8},

	     {expect_nothing, 200}, 
	     {send, "transaction-ack",                   TransAck},
             {expect_closed,  5000},
             disconnect
            ],
    EvSeq.

ssmp3_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

ssmp3_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

ssmp3_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch ssmp3_mgc_verify_service_change_req(Msg)) 
    end.

ssmp3_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("ssmp3_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

ssmp3_mgc_verify_notify_reply_segment_msg_fun(SN, Last, 
					     TransId, TermId, Cid) ->
    fun(Msg) -> 
	    (catch ssmp3_mgc_verify_notify_reply_segment(Msg, 
							SN, Last, 
							TransId, TermId, Cid)) 
    end.

ssmp3_mgc_verify_notify_reply_segment(#'MegacoMessage'{mess = Mess} = M,
				     SN, Last, TransId, TermId, Cid) ->
    io:format("ssmp3_mgc_verify_notify_reply_segment -> entry with"
	      "~n   M:       ~p"
	      "~n   SN:      ~p"
	      "~n   Last:    ~p"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, SN, Last, TransId, TermId, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = asn1_NOVALUE} when (Last == false) ->
		TransRes;
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = 'NULL'} when (Last == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
	    {actionReplies, [ActionReply]} ->
		ActionReply;
	    {actionReplies, ActionReplies} ->
		throw({error, {invalid_actionReplies, ActionReplies}});
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
	    #'ActionReply'{contextId    = Cid,
			   commandReply = [CommandReply]} ->
		CommandReply;
	    #'ActionReply'{contextId    = Cid,
			   commandReply = CommandReplies} ->
		throw({error, {invalid_commandReplies, CommandReplies}});
	    _ ->
		throw({error, {invalid_actionReply, AR}})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_NotifyReply, NR}}
    end;
ssmp3_mgc_verify_notify_reply_segment(Crap, 
				     _SN, _Last, _TransId, _TermId, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


ssmp3_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp3_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs) ->
    ARs     = ssmp3_mgc_notify_request_ars(TermIDs, CIDs), 
    TR      = cre_transReq(TransId, ARs),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp3_mgc_notify_request_ars(TermIDs, CIDs) ->
    ssmp3_mgc_notify_request_ars(TermIDs, CIDs, []).

ssmp3_mgc_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmp3_mgc_notify_request_ars([TermID|TermIDs], [CID|CIDs], Acc) ->
    AR = ssmp3_mgc_notify_request_ar(100+CID, TermID, CID),
    ssmp3_mgc_notify_request_ars(TermIDs, CIDs, [AR|Acc]).
    
ssmp3_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation(integer_to_list(19990720+Rid), 
			       integer_to_list(22000000+Rid)),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp3_mgc_segment_reply_msg(Mid, TransId, SN, Last) ->
    SR    = ssmp3_mgc_segment_reply(TransId, SN, Last),
    Trans = cre_transaction(SR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp3_mgc_segment_reply(TransId, SN, true) ->
    cre_segReply(TransId, SN, 'NULL');
ssmp3_mgc_segment_reply(TransId, SN, false) ->
    cre_segReply(TransId, SN, asn1_NOVALUE).

ssmp3_mgc_trans_ack_msg(Mid, TransId) ->
    TA    = cre_transAck(TransId),
    Trans = cre_transaction([TA]),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
ssmp3_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ssmp3_mg_verify_handle_connect_fun(),
    ServiceChangeReq = ssmp3_mg_service_change_request_ar(Mid, 1),
    ServiceChangeReplyVerify = ssmp3_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tid4 = #megaco_term_id{id = ["00000000","00000000","00000004"]},
    Tid5 = #megaco_term_id{id = ["00000000","00000000","00000005"]},
    Tid6 = #megaco_term_id{id = ["00000000","00000000","00000006"]},
    Tid7 = #megaco_term_id{id = ["00000000","00000000","00000007"]},
    Tid8 = #megaco_term_id{id = ["00000000","00000000","00000008"]},
    Tids = [Tid1, Tid2, Tid3, Tid4, Tid5, Tid6, Tid7, Tid8], 
    NotifyReqVerify = ssmp3_mg_verify_notify_request_fun(Tids),
    AckVerify = ssmp3_mg_verify_ack_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, segment_send,     1}, 
	     {megaco_update_conn_info, max_pdu_size,     128}, 
             {sleep, 500},
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify, 5000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


ssmp3_mg_verify_handle_connect_fun() ->
    fun(Ev) -> ssmp3_mg_verify_handle_connect(Ev) end.

ssmp3_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("ssmp3_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
ssmp3_mg_verify_handle_connect(Else) ->
    io:format("ssmp3_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


ssmp3_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> ssmp3_mg_verify_scr(Rep) end.

ssmp3_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch ssmp3_mg_do_verify_scr(AR));
ssmp3_mg_verify_scr(Crap) ->
    io:format("ssmp3_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

ssmp3_mg_do_verify_scr(AR) ->
    io:format("ssmp3_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

ssmp3_mg_verify_notify_request_fun(Tids) ->
    fun(Req) -> ssmp3_mg_verify_notify_request(Req, Tids) end.
	     
ssmp3_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, Tids) 
  when length(ARs) == length(Tids) ->
    (catch ssmp3_mg_do_verify_notify_request(Tids, ARs));
ssmp3_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, _Tids) ->
    {error, {invalid_action_requests, ARs}, ok};
ssmp3_mg_verify_notify_request(
  {handle_trans_request, CH, V, ARs}, _Tids) ->
    {error, {invalid_trans_request, {CH, V, ARs}}, ok};
ssmp3_mg_verify_notify_request(Crap, _Tids) ->
    io:format("ssmp3_mg_verify_notify_request -> unknown request"
	      "~n   Crap: ~p"
	      "~n   Tids: ~p"
	      "~n", [Crap, _Tids]),
    {error, {unexpected_event, Crap}, ok}.

ssmp3_mg_do_verify_notify_request(Tids, ARs) ->
    io:format("ssmp3_mg_do_verify_notify_request -> ok"
	      "~n   Tids: ~p"
	      "~n   ARs:  ~p"
	      "~n", [Tids, ARs]),
    ActionReplies = ssmp3_mg_do_verify_notify_request_ars(Tids, ARs), 
    io:format("ssmp3_mg_do_verify_notify_request -> ok"
	      "~n   ActionReplies:  ~p"
	      "~n", [ActionReplies]),
    Reply = {{handle_ack, ssmp3}, ActionReplies}, 
    {ok, ARs, Reply}.

ssmp3_mg_do_verify_notify_request_ars(Tids, ARs) ->
    ssmp3_mg_do_verify_notify_request_ars(Tids, ARs, []).

ssmp3_mg_do_verify_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmp3_mg_do_verify_notify_request_ars([Tid|Tids], [AR|ARs], Acc) ->
    ActionReply = ssmp3_mg_do_verify_notify_request_ar(Tid, AR),
    ssmp3_mg_do_verify_notify_request_ars(Tids, ARs, [ActionReply|Acc]).

ssmp3_mg_do_verify_notify_request_ar(Tid, AR) ->
    io:format("ssmp3_mg_do_verify_notify_request_ar -> ok"
	      "~n   Tid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Tid, AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxId, 
			     commandRequests = [CmdReq]} ->
		{CtxId, CmdReq};
	    _ ->
		Reason1 = {invalid_actionRequest, AR},
		throw({error, Reason1, ok})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}, ok})
	end,
    OED = 
	case Cmd of
	    {notifyReq, 
	     #'NotifyRequest'{terminationID            = [Tid],
			      observedEventsDescriptor = ObsEvDesc,
			      errorDescriptor          = asn1_NOVALUE}} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_command, Cmd}, ok})
	end,
    OE = 
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    #'ObservedEventsDescriptor'{observedEventLst = ObsEvLst} ->
		throw({error, {invalid_observedEventLst, ObsEvLst}, ok});
	    _ ->
		throw({error, {invalid_ObservedEventsDescriptor, OED}, ok})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ssmp3_mg_notify_reply_ar(Cid, Tid);
	_ ->
	    throw({error, {invalid_ObservedEvent, OE}, ok})
    end.


ssmp3_mg_verify_ack_fun() ->
    fun(Event) -> ssmp3_mg_verify_ack(Event) end.

ssmp3_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, ssmp3}) ->
    io:format("ssmp3_mg_verify_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
ssmp3_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, CrapAckData}) ->
    {error, {unknown_ack_data, CrapAckData, CH}, ok};
ssmp3_mg_verify_ack({handle_trans_ack, CH, ?VERSION, 
		    BadAckStatus, BadAckData}) ->
    {error, {unknown_ack_status, BadAckStatus, BadAckData, CH}, ok};
ssmp3_mg_verify_ack(BadEvent) ->
    {error, {unknown_event, BadEvent}, ok}.
    
	    
ssmp3_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp3_mg_notify_reply_ar(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



send_segmented_msg_plain4(suite) ->
    [];
send_segmented_msg_plain4(doc) ->
    "Forth plain test that it is possible to send segmented messages. "
	"Send window = 3. ";
send_segmented_msg_plain4(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ssmp4),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = ssmp4_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = ssmp4_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.



%%
%% MGC generator stuff
%%

ssmp4_mgc_event_sequence(text, tcp) ->
    DecodeFun = ssmp4_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ssmp4_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = ssmp4_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = ssmp4_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    CtxId1    = 1, 
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    CtxId2    = 2, 
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    CtxId3    = 3, 
    TermId4   = 
	#megaco_term_id{id = ["00000000","00000000","00000004"]},
    CtxId4    = 4, 
    TermId5   = 
	#megaco_term_id{id = ["00000000","00000000","00000005"]},
    CtxId5    = 5, 
    TermId6   = 
	#megaco_term_id{id = ["00000000","00000000","00000006"]},
    CtxId6    = 6, 
    TermId7   = 
	#megaco_term_id{id = ["00000000","00000000","00000007"]},
    CtxId7    = 7, 
    TermId8   = 
	#megaco_term_id{id = ["00000000","00000000","00000008"]},
    CtxId8    = 8, 
    TransId   = 2,
    TermIDs = [TermId1, TermId2, TermId3, TermId4, 
	       TermId5, TermId6, TermId7, TermId8],
    CIDs    = [CtxId1, CtxId2, CtxId3, CtxId4, 
	       CtxId5, CtxId6, CtxId7, CtxId8], 
    NotifyReq = ssmp4_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs),
    NrVerifyFun1 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(1, false, TransId, 
						      TermId1, CtxId1),
    NrVerifyFun2 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(2, false, TransId, 
						      TermId2, CtxId2),
    NrVerifyFun3 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(3, false, TransId, 
						      TermId3, CtxId3),
    NrVerifyFun4 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(4, false, TransId, 
						      TermId4, CtxId4),
    NrVerifyFun5 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(5, false, TransId, 
						      TermId5, CtxId5),
    NrVerifyFun6 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(6, false, TransId, 
						      TermId6, CtxId6),
    NrVerifyFun7 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(7, false, TransId, 
						      TermId7, CtxId7),
    NrVerifyFun8 = 
	ssmp4_mgc_verify_notify_reply_segment_msg_fun(8, true, TransId, 
						      TermId8, CtxId8),
    SegmentRep1 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 1, false),
    SegmentRep2 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 2, false),
    SegmentRep3 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 3, false),
    SegmentRep4 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 4, false),
    SegmentRep5 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 5, false),
    SegmentRep6 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 6, false),
    SegmentRep7 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 7, false),
    SegmentRep8 = ssmp4_mgc_segment_reply_msg(Mid, TransId, 8, true),
    TransAck    = ssmp4_mgc_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request",  {ScrVerifyFun, 5000}},
             {send, "service-change-reply",              ServiceChangeRep},
	     {expect_nothing, 1000}, 
             {send, "notify request",                    NotifyReq},
             {expect_receive, "notify reply: segment 1", {NrVerifyFun1, 1000}},
             {expect_receive, "notify reply: segment 2", {NrVerifyFun2, 1000}},
             {expect_receive, "notify reply: segment 3", {NrVerifyFun3, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 1",                   SegmentRep1},
             {expect_receive, "notify reply: segment 4", {NrVerifyFun4, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 2",                   SegmentRep2},
             {expect_receive, "notify reply: segment 5", {NrVerifyFun5, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 3",                   SegmentRep3},
             {expect_receive, "notify reply: segment 6", {NrVerifyFun6, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 4",                   SegmentRep4},
             {expect_receive, "notify reply: segment 7", {NrVerifyFun7, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 5",                   SegmentRep5},
             {expect_receive, "notify reply: segment 8", {NrVerifyFun8, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 6",                   SegmentRep6},
	     {expect_nothing, 1000},
             {send, "segment reply 7",                   SegmentRep7},
	     {expect_nothing, 1000},
             {send, "segment reply 8",                   SegmentRep8},
	     {expect_nothing, 1000},
	     {send, "transaction-ack",                   TransAck},
             {expect_closed,  5000},
             disconnect
            ],
    EvSeq.

ssmp4_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

ssmp4_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

ssmp4_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch ssmp4_mgc_verify_service_change_req(Msg)) 
    end.

ssmp4_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("ssmp4_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

ssmp4_mgc_verify_notify_reply_segment_msg_fun(SN, Last, 
					     TransId, TermId, Cid) ->
    fun(Msg) -> 
	    (catch ssmp4_mgc_verify_notify_reply_segment(Msg, 
							SN, Last, 
							TransId, TermId, Cid)) 
    end.

ssmp4_mgc_verify_notify_reply_segment(#'MegacoMessage'{mess = Mess} = M,
				     SN, Last, TransId, TermId, Cid) ->
    io:format("ssmp4_mgc_verify_notify_reply_segment -> entry with"
	      "~n   M:       ~p"
	      "~n   SN:      ~p"
	      "~n   Last:    ~p"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, SN, Last, TransId, TermId, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = asn1_NOVALUE} when (Last == false) ->
		TransRes;
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = 'NULL'} when (Last == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
	    {actionReplies, [ActionReply]} ->
		ActionReply;
	    {actionReplies, ActionReplies} ->
		throw({error, {invalid_actionReplies, ActionReplies}});
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
	    #'ActionReply'{contextId    = Cid,
			   commandReply = [CommandReply]} ->
		CommandReply;
	    #'ActionReply'{contextId    = Cid,
			   commandReply = CommandReplies} ->
		throw({error, {invalid_commandReplies, CommandReplies}});
	    _ ->
		throw({error, {invalid_actionReply, AR}})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_NotifyReply, NR}}
    end;
ssmp4_mgc_verify_notify_reply_segment(Crap, 
				     _SN, _Last, _TransId, _TermId, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


ssmp4_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp4_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs) ->
    ARs     = ssmp4_mgc_notify_request_ars(TermIDs, CIDs), 
    TR      = cre_transReq(TransId, ARs),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp4_mgc_notify_request_ars(TermIDs, CIDs) ->
    ssmp4_mgc_notify_request_ars(TermIDs, CIDs, []).

ssmp4_mgc_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmp4_mgc_notify_request_ars([TermID|TermIDs], [CID|CIDs], Acc) ->
    AR = ssmp4_mgc_notify_request_ar(100+CID, TermID, CID),
    ssmp4_mgc_notify_request_ars(TermIDs, CIDs, [AR|Acc]).
    
ssmp4_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation(integer_to_list(19990720+Rid), 
			       integer_to_list(22000000+Rid)),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp4_mgc_segment_reply_msg(Mid, TransId, SN, Last) ->
    SR    = ssmp4_mgc_segment_reply(TransId, SN, Last),
    Trans = cre_transaction(SR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmp4_mgc_segment_reply(TransId, SN, true) ->
    cre_segReply(TransId, SN, 'NULL');
ssmp4_mgc_segment_reply(TransId, SN, false) ->
    cre_segReply(TransId, SN, asn1_NOVALUE).

ssmp4_mgc_trans_ack_msg(Mid, TransId) ->
    TA    = cre_transAck(TransId),
    Trans = cre_transaction([TA]),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
ssmp4_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ssmp4_mg_verify_handle_connect_fun(),
    ServiceChangeReq = ssmp4_mg_service_change_request_ar(Mid, 1),
    ServiceChangeReplyVerify = ssmp4_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tid4 = #megaco_term_id{id = ["00000000","00000000","00000004"]},
    Tid5 = #megaco_term_id{id = ["00000000","00000000","00000005"]},
    Tid6 = #megaco_term_id{id = ["00000000","00000000","00000006"]},
    Tid7 = #megaco_term_id{id = ["00000000","00000000","00000007"]},
    Tid8 = #megaco_term_id{id = ["00000000","00000000","00000008"]},
    Tids = [Tid1, Tid2, Tid3, Tid4, Tid5, Tid6, Tid7, Tid8], 
    NotifyReqVerify = ssmp4_mg_verify_notify_request_fun(Tids),
    AckVerify = ssmp4_mg_verify_ack_fun(), 
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, segment_send,     3}, 
	     {megaco_update_conn_info, max_pdu_size,     128}, 
             {sleep, 1000},
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify, 15000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


ssmp4_mg_verify_handle_connect_fun() ->
    fun(Ev) -> ssmp4_mg_verify_handle_connect(Ev) end.

ssmp4_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("ssmp4_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
ssmp4_mg_verify_handle_connect(Else) ->
    io:format("ssmp4_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


ssmp4_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> ssmp4_mg_verify_scr(Rep) end.

ssmp4_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch ssmp4_mg_do_verify_scr(AR));
ssmp4_mg_verify_scr(Crap) ->
    io:format("ssmp4_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

ssmp4_mg_do_verify_scr(AR) ->
    io:format("ssmp4_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

ssmp4_mg_verify_notify_request_fun(Tids) ->
    fun(Req) -> ssmp4_mg_verify_notify_request(Req, Tids) end.
	     
ssmp4_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, Tids) 
  when length(ARs) == length(Tids) ->
    (catch ssmp4_mg_do_verify_notify_request(Tids, ARs));
ssmp4_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, _Tids) ->
    {error, {invalid_action_requests, ARs}, ok};
ssmp4_mg_verify_notify_request(
  {handle_trans_request, CH, V, ARs}, _Tids) ->
    {error, {invalid_trans_request, {CH, V, ARs}}, ok};
ssmp4_mg_verify_notify_request(Crap, _Tids) ->
    io:format("ssmp4_mg_verify_notify_request -> unknown request"
	      "~n   Crap: ~p"
	      "~n   Tids: ~p"
	      "~n", [Crap, _Tids]),
    {error, {unexpected_event, Crap}, ok}.

ssmp4_mg_do_verify_notify_request(Tids, ARs) ->
    io:format("ssmp4_mg_do_verify_notify_request -> ok"
	      "~n   Tids: ~p"
	      "~n   ARs:  ~p"
	      "~n", [Tids, ARs]),
    ActionReplies = ssmp4_mg_do_verify_notify_request_ars(Tids, ARs), 
    io:format("ssmp4_mg_do_verify_notify_request -> ok"
	      "~n   ActionReplies:  ~p"
	      "~n", [ActionReplies]),
    Reply = {{handle_ack, ssmp4}, ActionReplies}, 
    {ok, ARs, Reply}.

ssmp4_mg_do_verify_notify_request_ars(Tids, ARs) ->
    ssmp4_mg_do_verify_notify_request_ars(Tids, ARs, []).

ssmp4_mg_do_verify_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmp4_mg_do_verify_notify_request_ars([Tid|Tids], [AR|ARs], Acc) ->
    ActionReply = ssmp4_mg_do_verify_notify_request_ar(Tid, AR),
    ssmp4_mg_do_verify_notify_request_ars(Tids, ARs, [ActionReply|Acc]).

ssmp4_mg_do_verify_notify_request_ar(Tid, AR) ->
    io:format("ssmp4_mg_do_verify_notify_request_ar -> ok"
	      "~n   Tid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Tid, AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxId, 
			     commandRequests = [CmdReq]} ->
		{CtxId, CmdReq};
	    _ ->
		Reason1 = {invalid_actionRequest, AR},
		throw({error, Reason1, ok})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}, ok})
	end,
    OED = 
	case Cmd of
	    {notifyReq, 
	     #'NotifyRequest'{terminationID            = [Tid],
			      observedEventsDescriptor = ObsEvDesc,
			      errorDescriptor          = asn1_NOVALUE}} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_command, Cmd}, ok})
	end,
    OE = 
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    #'ObservedEventsDescriptor'{observedEventLst = ObsEvLst} ->
		throw({error, {invalid_observedEventLst, ObsEvLst}, ok});
	    _ ->
		throw({error, {invalid_ObservedEventsDescriptor, OED}, ok})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ssmp4_mg_notify_reply_ar(Cid, Tid);
	_ ->
	    throw({error, {invalid_ObservedEvent, OE}, ok})
    end.


ssmp4_mg_verify_ack_fun() ->
    fun(Event) -> ssmp4_mg_verify_ack(Event) end.

ssmp4_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, ssmp4}) ->
    io:format("ssmp4_mg_verify_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
ssmp4_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, CrapAckData}) ->
    {error, {unknown_ack_data, CrapAckData, CH}, ok};
ssmp4_mg_verify_ack({handle_trans_ack, CH, ?VERSION, 
		    BadAckStatus, BadAckData}) ->
    {error, {unknown_ack_status, BadAckStatus, BadAckData, CH}, ok};
ssmp4_mg_verify_ack(BadEvent) ->
    {error, {unknown_event, BadEvent}, ok}.
    
	    
ssmp4_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmp4_mg_notify_reply_ar(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



send_segmented_msg_ooo1(suite) ->
    [];
send_segmented_msg_ooo1(doc) ->
    "First segment out of order test. "
	"Tests that it is possible to send segmented messages, when the "
	"segment reply is sent out-of-order. "
	"Send window = 3. ";
send_segmented_msg_ooo1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ssmo1),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = ssmo1_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = ssmo1_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.



%%
%% MGC generator stuff
%%

ssmo1_mgc_event_sequence(text, tcp) ->
    DecodeFun = ssmo1_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ssmo1_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = ssmo1_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = ssmo1_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    CtxId1    = 1, 
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    CtxId2    = 2, 
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    CtxId3    = 3, 
    TermId4   = 
	#megaco_term_id{id = ["00000000","00000000","00000004"]},
    CtxId4    = 4, 
    TermId5   = 
	#megaco_term_id{id = ["00000000","00000000","00000005"]},
    CtxId5    = 5, 
    TermId6   = 
	#megaco_term_id{id = ["00000000","00000000","00000006"]},
    CtxId6    = 6, 
    TermId7   = 
	#megaco_term_id{id = ["00000000","00000000","00000007"]},
    CtxId7    = 7, 
    TermId8   = 
	#megaco_term_id{id = ["00000000","00000000","00000008"]},
    CtxId8    = 8, 
    TransId   = 2,
    TermIDs = [TermId1, TermId2, TermId3, TermId4, 
	       TermId5, TermId6, TermId7, TermId8],
    CIDs    = [CtxId1, CtxId2, CtxId3, CtxId4, 
	       CtxId5, CtxId6, CtxId7, CtxId8], 
    NotifyReq = ssmo1_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs),
    NrVerifyFun1 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(1, false, TransId, 
						      TermId1, CtxId1),
    NrVerifyFun2 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(2, false, TransId, 
						      TermId2, CtxId2),
    NrVerifyFun3 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(3, false, TransId, 
						      TermId3, CtxId3),
    NrVerifyFun4 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(4, false, TransId, 
						      TermId4, CtxId4),
    NrVerifyFun5 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(5, false, TransId, 
						      TermId5, CtxId5),
    NrVerifyFun6 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(6, false, TransId, 
						      TermId6, CtxId6),
    NrVerifyFun7 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(7, false, TransId, 
						      TermId7, CtxId7),
    NrVerifyFun8 = 
	ssmo1_mgc_verify_notify_reply_segment_msg_fun(8, true, TransId, 
						      TermId8, CtxId8),
    SegmentRep1 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 1, false),
    SegmentRep2 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 2, false),
    SegmentRep3 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 3, false),
    SegmentRep4 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 4, false),
    SegmentRep5 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 5, false),
    SegmentRep6 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 6, false),
    SegmentRep7 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 7, false),
    SegmentRep8 = ssmo1_mgc_segment_reply_msg(Mid, TransId, 8, true),
    TransAck    = ssmo1_mgc_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request",  {ScrVerifyFun, 5000}},
             {send, "service-change-reply",              ServiceChangeRep},
	     {expect_nothing, 1000}, 
             {send, "notify request",                    NotifyReq},
             {expect_receive, "notify reply: segment 1", {NrVerifyFun1, 1000}},
             {expect_receive, "notify reply: segment 2", {NrVerifyFun2, 1000}},
             {expect_receive, "notify reply: segment 3", {NrVerifyFun3, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 2 [1]",               SegmentRep2},
             {expect_receive, "notify reply: segment 4", {NrVerifyFun4, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 1 [2]",               SegmentRep1},
             {expect_receive, "notify reply: segment 5", {NrVerifyFun5, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 3 [3]",               SegmentRep3},
             {expect_receive, "notify reply: segment 6", {NrVerifyFun6, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 5 [4]",               SegmentRep5},
             {expect_receive, "notify reply: segment 7", {NrVerifyFun7, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 4 [5]",               SegmentRep4},
             {expect_receive, "notify reply: segment 8", {NrVerifyFun8, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 6",                   SegmentRep6},
	     {expect_nothing, 1000},
             {send, "segment reply 7",                   SegmentRep7},
	     {expect_nothing, 1000},
             {send, "segment reply 8",                   SegmentRep8},
	     {expect_nothing, 1000},
	     {send, "transaction-ack",                   TransAck},
             {expect_closed,  5000},
             disconnect
            ],
    EvSeq.

ssmo1_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

ssmo1_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

ssmo1_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch ssmo1_mgc_verify_service_change_req(Msg)) 
    end.

ssmo1_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("ssmo1_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

ssmo1_mgc_verify_notify_reply_segment_msg_fun(SN, Last, 
					     TransId, TermId, Cid) ->
    fun(Msg) -> 
	    (catch ssmo1_mgc_verify_notify_reply_segment(Msg, 
							SN, Last, 
							TransId, TermId, Cid)) 
    end.

ssmo1_mgc_verify_notify_reply_segment(#'MegacoMessage'{mess = Mess} = M,
				     SN, Last, TransId, TermId, Cid) ->
    io:format("ssmo1_mgc_verify_notify_reply_segment -> entry with"
	      "~n   M:       ~p"
	      "~n   SN:      ~p"
	      "~n   Last:    ~p"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, SN, Last, TransId, TermId, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = asn1_NOVALUE} when (Last == false) ->
		TransRes;
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = 'NULL'} when (Last == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
	    {actionReplies, [ActionReply]} ->
		ActionReply;
	    {actionReplies, ActionReplies} ->
		throw({error, {invalid_actionReplies, ActionReplies}});
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
	    #'ActionReply'{contextId    = Cid,
			   commandReply = [CommandReply]} ->
		CommandReply;
	    #'ActionReply'{contextId    = Cid,
			   commandReply = CommandReplies} ->
		throw({error, {invalid_commandReplies, CommandReplies}});
	    _ ->
		throw({error, {invalid_actionReply, AR}})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_NotifyReply, NR}}
    end;
ssmo1_mgc_verify_notify_reply_segment(Crap, 
				     _SN, _Last, _TransId, _TermId, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


ssmo1_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmo1_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs) ->
    ARs     = ssmo1_mgc_notify_request_ars(TermIDs, CIDs), 
    TR      = cre_transReq(TransId, ARs),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmo1_mgc_notify_request_ars(TermIDs, CIDs) ->
    ssmo1_mgc_notify_request_ars(TermIDs, CIDs, []).

ssmo1_mgc_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmo1_mgc_notify_request_ars([TermID|TermIDs], [CID|CIDs], Acc) ->
    AR = ssmo1_mgc_notify_request_ar(100+CID, TermID, CID),
    ssmo1_mgc_notify_request_ars(TermIDs, CIDs, [AR|Acc]).
    
ssmo1_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation(integer_to_list(19990720+Rid), 
			       integer_to_list(22000000+Rid)),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmo1_mgc_segment_reply_msg(Mid, TransId, SN, Last) ->
    SR    = ssmo1_mgc_segment_reply(TransId, SN, Last),
    Trans = cre_transaction(SR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmo1_mgc_segment_reply(TransId, SN, true) ->
    cre_segReply(TransId, SN, 'NULL');
ssmo1_mgc_segment_reply(TransId, SN, false) ->
    cre_segReply(TransId, SN, asn1_NOVALUE).

ssmo1_mgc_trans_ack_msg(Mid, TransId) ->
    TA    = cre_transAck(TransId),
    Trans = cre_transaction([TA]),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
ssmo1_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ssmo1_mg_verify_handle_connect_fun(),
    ServiceChangeReq = ssmo1_mg_service_change_request_ar(Mid, 1),
    ServiceChangeReplyVerify = ssmo1_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tid4 = #megaco_term_id{id = ["00000000","00000000","00000004"]},
    Tid5 = #megaco_term_id{id = ["00000000","00000000","00000005"]},
    Tid6 = #megaco_term_id{id = ["00000000","00000000","00000006"]},
    Tid7 = #megaco_term_id{id = ["00000000","00000000","00000007"]},
    Tid8 = #megaco_term_id{id = ["00000000","00000000","00000008"]},
    Tids = [Tid1, Tid2, Tid3, Tid4, Tid5, Tid6, Tid7, Tid8], 
    NotifyReqVerify = ssmo1_mg_verify_notify_request_fun(Tids),
    AckVerify = ssmo1_mg_verify_ack_fun(), 
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, segment_send,     3}, 
	     {megaco_update_conn_info, max_pdu_size,     128}, 
             {sleep, 1000},
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify, 15000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


ssmo1_mg_verify_handle_connect_fun() ->
    fun(Ev) -> ssmo1_mg_verify_handle_connect(Ev) end.

ssmo1_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("ssmo1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
ssmo1_mg_verify_handle_connect(Else) ->
    io:format("ssmo1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


ssmo1_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> ssmo1_mg_verify_scr(Rep) end.

ssmo1_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch ssmo1_mg_do_verify_scr(AR));
ssmo1_mg_verify_scr(Crap) ->
    io:format("ssmo1_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

ssmo1_mg_do_verify_scr(AR) ->
    io:format("ssmo1_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

ssmo1_mg_verify_notify_request_fun(Tids) ->
    fun(Req) -> ssmo1_mg_verify_notify_request(Req, Tids) end.
	     
ssmo1_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, Tids) 
  when length(ARs) == length(Tids) ->
    (catch ssmo1_mg_do_verify_notify_request(Tids, ARs));
ssmo1_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, _Tids) ->
    {error, {invalid_action_requests, ARs}, ok};
ssmo1_mg_verify_notify_request(
  {handle_trans_request, CH, V, ARs}, _Tids) ->
    {error, {invalid_trans_request, {CH, V, ARs}}, ok};
ssmo1_mg_verify_notify_request(Crap, _Tids) ->
    io:format("ssmo1_mg_verify_notify_request -> unknown request"
	      "~n   Crap: ~p"
	      "~n   Tids: ~p"
	      "~n", [Crap, _Tids]),
    {error, {unexpected_event, Crap}, ok}.

ssmo1_mg_do_verify_notify_request(Tids, ARs) ->
    io:format("ssmo1_mg_do_verify_notify_request -> ok"
	      "~n   Tids: ~p"
	      "~n   ARs:  ~p"
	      "~n", [Tids, ARs]),
    ActionReplies = ssmo1_mg_do_verify_notify_request_ars(Tids, ARs), 
    io:format("ssmo1_mg_do_verify_notify_request -> ok"
	      "~n   ActionReplies:  ~p"
	      "~n", [ActionReplies]),
    Reply = {{handle_ack, ssmp4}, ActionReplies}, 
    {ok, ARs, Reply}.

ssmo1_mg_do_verify_notify_request_ars(Tids, ARs) ->
    ssmo1_mg_do_verify_notify_request_ars(Tids, ARs, []).

ssmo1_mg_do_verify_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmo1_mg_do_verify_notify_request_ars([Tid|Tids], [AR|ARs], Acc) ->
    ActionReply = ssmo1_mg_do_verify_notify_request_ar(Tid, AR),
    ssmo1_mg_do_verify_notify_request_ars(Tids, ARs, [ActionReply|Acc]).

ssmo1_mg_do_verify_notify_request_ar(Tid, AR) ->
    io:format("ssmo1_mg_do_verify_notify_request_ar -> ok"
	      "~n   Tid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Tid, AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxId, 
			     commandRequests = [CmdReq]} ->
		{CtxId, CmdReq};
	    _ ->
		Reason1 = {invalid_actionRequest, AR},
		throw({error, Reason1, ok})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}, ok})
	end,
    OED = 
	case Cmd of
	    {notifyReq, 
	     #'NotifyRequest'{terminationID            = [Tid],
			      observedEventsDescriptor = ObsEvDesc,
			      errorDescriptor          = asn1_NOVALUE}} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_command, Cmd}, ok})
	end,
    OE = 
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    #'ObservedEventsDescriptor'{observedEventLst = ObsEvLst} ->
		throw({error, {invalid_observedEventLst, ObsEvLst}, ok});
	    _ ->
		throw({error, {invalid_ObservedEventsDescriptor, OED}, ok})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ssmo1_mg_notify_reply_ar(Cid, Tid);
	_ ->
	    throw({error, {invalid_ObservedEvent, OE}, ok})
    end.


ssmo1_mg_verify_ack_fun() ->
    fun(Event) -> ssmo1_mg_verify_ack(Event) end.

ssmo1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, ssmp4}) ->
    io:format("ssmo1_mg_verify_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
ssmo1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, ok, CrapAckData}) ->
    {error, {unknown_ack_data, CrapAckData, CH}, ok};
ssmo1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, 
		    BadAckStatus, BadAckData}) ->
    {error, {unknown_ack_status, BadAckStatus, BadAckData, CH}, ok};
ssmo1_mg_verify_ack(BadEvent) ->
    {error, {unknown_event, BadEvent}, ok}.
    
	    
ssmo1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmo1_mg_notify_reply_ar(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



send_segmented_msg_missing_seg_reply1(suite) ->
    [];
send_segmented_msg_missing_seg_reply1(doc) ->
    "First missing segment test. "
	"Tests that the callbacks and error messages are delivered "
	"when a segment reply goes missing. Ack expected. "
	"Send window = 3. ";
send_segmented_msg_missing_seg_reply1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ssmmsr1),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = ssmmsr1_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = ssmmsr1_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    %% Await MGC ready for segments
    d("await MGC trigger event"),
    MgcPid = 
	receive
	    {ready_for_segments, mgc, Pid1} ->
		d("received MGC trigger event"),
		Pid1
	after 5000 ->
		d("timeout waiting for MGC trigger event: ~p", 
		  [megaco_test_lib:flush()]),
		?ERROR(timeout_MGC_trigger_event)
	end,

    %% Await MG ready for segments
    d("await MG trigger event"),
    MgPid = 
	receive
	    {ready_for_segments, mg, Pid2} ->
		d("received MG trigger event"),
		Pid2
	after 5000 ->
		d("timeout waiting for MG trigger event: ~p", 
		  [megaco_test_lib:flush()]),
		?ERROR(timeout_MG_trigger_event)
	end,

    %% Instruct the MG to continue
    d("send continue to MG"),
    MgPid ! {continue_with_segments, self()}, 

    sleep(500),

    %% Instruct the MGC to continue
    d("send continue to MGC"),
    MgcPid ! {continue_with_segments, self()}, 

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.



%%
%% MGC generator stuff
%%

ssmmsr1_mgc_event_sequence(text, tcp) ->
    DecodeFun = ssmmsr1_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ssmmsr1_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = ssmmsr1_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = ssmmsr1_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    CtxId1    = 1, 
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    CtxId2    = 2, 
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    CtxId3    = 3, 
    TermId4   = 
	#megaco_term_id{id = ["00000000","00000000","00000004"]},
    CtxId4    = 4, 
    TermId5   = 
	#megaco_term_id{id = ["00000000","00000000","00000005"]},
    CtxId5    = 5, 
    TermId6   = 
	#megaco_term_id{id = ["00000000","00000000","00000006"]},
    CtxId6    = 6, 
    TermId7   = 
	#megaco_term_id{id = ["00000000","00000000","00000007"]},
    CtxId7    = 7, 
    TermId8   = 
	#megaco_term_id{id = ["00000000","00000000","00000008"]},
    CtxId8    = 8, 
    TransId   = 2,
    TermIDs = [TermId1, TermId2, TermId3, TermId4, 
	       TermId5, TermId6, TermId7, TermId8],
    CIDs    = [CtxId1, CtxId2, CtxId3, CtxId4, 
	       CtxId5, CtxId6, CtxId7, CtxId8], 
    NotifyReq = ssmmsr1_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs),
    NrVerifyFun1 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(1, false, TransId, 
						      TermId1, CtxId1),
    NrVerifyFun2 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(2, false, TransId, 
						      TermId2, CtxId2),
    NrVerifyFun3 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(3, false, TransId, 
						      TermId3, CtxId3),
    NrVerifyFun4 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(4, false, TransId, 
						      TermId4, CtxId4),
    NrVerifyFun5 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(5, false, TransId, 
						      TermId5, CtxId5),
    NrVerifyFun6 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(6, false, TransId, 
						      TermId6, CtxId6),
    NrVerifyFun7 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(7, false, TransId, 
						      TermId7, CtxId7),
    NrVerifyFun8 = 
	ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(8, true, TransId, 
						      TermId8, CtxId8),
    SegmentRep1 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 1, false),
    SegmentRep2 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 2, false),
    %% SegmentRep3 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 3, false),
    SegmentRep4 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 4, false),
    SegmentRep5 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 5, false),
    SegmentRep6 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 6, false),
    SegmentRep7 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 7, false),
    SegmentRep8 = ssmmsr1_mgc_segment_reply_msg(Mid, TransId, 8, true),
    TransAck    = ssmmsr1_mgc_trans_ack_msg(Mid, TransId),
    ReadyForSegments = ssmmsr1_mgc_ready_for_segments_fun(), 
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request",  {ScrVerifyFun, 5000}},
             {send, "service-change-reply",              ServiceChangeRep},
	     %% {expect_nothing, 1000}, 
	     {trigger, "segment send sync trigger", ReadyForSegments}, 
             {send, "notify request",                    NotifyReq},
             {expect_receive, "notify reply: segment 1", {NrVerifyFun1, 1000}},
             {expect_receive, "notify reply: segment 2", {NrVerifyFun2, 1000}},
             {expect_receive, "notify reply: segment 3", {NrVerifyFun3, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 1 [1]",               SegmentRep1},
             {expect_receive, "notify reply: segment 4", {NrVerifyFun4, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 2 [2]",               SegmentRep2},
             {expect_receive, "notify reply: segment 5", {NrVerifyFun5, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 4 [3]",               SegmentRep4},
             {expect_receive, "notify reply: segment 6", {NrVerifyFun6, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 5 [4]",               SegmentRep5},
             {expect_receive, "notify reply: segment 7", {NrVerifyFun7, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 6 [5]",               SegmentRep6},
             {expect_receive, "notify reply: segment 8", {NrVerifyFun8, 1000}},
	     {expect_nothing, 1000},
             {send, "segment reply 7 [6]",               SegmentRep7},
	     {expect_nothing, 1000},
             {send, "segment reply 8 [7]",               SegmentRep8},
	     {expect_nothing, 1000},
	     {send, "transaction-ack",                   TransAck},
             {expect_closed,  5000},
             disconnect
            ],
    EvSeq.

ssmmsr1_mgc_ready_for_segments_fun() ->
    TC = self(),
    fun() ->
	    io:format("ssmmsr1_mgc_ready_for_segments_fun -> entry~n", []),
	    TC ! {ready_for_segments, mgc, self()},
	    receive
		{continue_with_segments, TC} ->
		    io:format("ssmmsr1_mgc_ready_for_segments_fun -> "
			      "received continue~n", []),
		    ok
	    end
    end.
		
ssmmsr1_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

ssmmsr1_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

ssmmsr1_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch ssmmsr1_mgc_verify_service_change_req(Msg)) 
    end.

ssmmsr1_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("ssmmsr1_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

ssmmsr1_mgc_verify_notify_reply_segment_msg_fun(SN, Last, 
					     TransId, TermId, Cid) ->
    fun(Msg) -> 
	    (catch ssmmsr1_mgc_verify_notify_reply_segment(Msg, 
							SN, Last, 
							TransId, TermId, Cid)) 
    end.

ssmmsr1_mgc_verify_notify_reply_segment(#'MegacoMessage'{mess = Mess} = M,
				     SN, Last, TransId, TermId, Cid) ->
    io:format("ssmmsr1_mgc_verify_notify_reply_segment -> entry with"
	      "~n   M:       ~p"
	      "~n   SN:      ~p"
	      "~n   Last:    ~p"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, SN, Last, TransId, TermId, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = asn1_NOVALUE} when (Last == false) ->
		TransRes;
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = 'NULL'} when (Last == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
	    {actionReplies, [ActionReply]} ->
		ActionReply;
	    {actionReplies, ActionReplies} ->
		throw({error, {invalid_actionReplies, ActionReplies}});
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
	    #'ActionReply'{contextId    = Cid,
			   commandReply = [CommandReply]} ->
		CommandReply;
	    #'ActionReply'{contextId    = Cid,
			   commandReply = CommandReplies} ->
		throw({error, {invalid_commandReplies, CommandReplies}});
	    _ ->
		throw({error, {invalid_actionReply, AR}})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_NotifyReply, NR}}
    end;
ssmmsr1_mgc_verify_notify_reply_segment(Crap, 
				     _SN, _Last, _TransId, _TermId, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


ssmmsr1_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmmsr1_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs) ->
    ARs     = ssmmsr1_mgc_notify_request_ars(TermIDs, CIDs), 
    TR      = cre_transReq(TransId, ARs),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmmsr1_mgc_notify_request_ars(TermIDs, CIDs) ->
    ssmmsr1_mgc_notify_request_ars(TermIDs, CIDs, []).

ssmmsr1_mgc_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmmsr1_mgc_notify_request_ars([TermID|TermIDs], [CID|CIDs], Acc) ->
    AR = ssmmsr1_mgc_notify_request_ar(100+CID, TermID, CID),
    ssmmsr1_mgc_notify_request_ars(TermIDs, CIDs, [AR|Acc]).
    
ssmmsr1_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation(integer_to_list(19990720+Rid), 
			       integer_to_list(22000000+Rid)),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmmsr1_mgc_segment_reply_msg(Mid, TransId, SN, Last) ->
    SR    = ssmmsr1_mgc_segment_reply(TransId, SN, Last),
    Trans = cre_transaction(SR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmmsr1_mgc_segment_reply(TransId, SN, true) ->
    cre_segReply(TransId, SN, 'NULL');
ssmmsr1_mgc_segment_reply(TransId, SN, false) ->
    cre_segReply(TransId, SN, asn1_NOVALUE).

ssmmsr1_mgc_trans_ack_msg(Mid, TransId) ->
    TA    = cre_transAck(TransId),
    Trans = cre_transaction([TA]),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
ssmmsr1_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ssmmsr1_mg_verify_handle_connect_fun(),
    ServiceChangeReq = ssmmsr1_mg_service_change_request_ar(Mid, 1),
    ServiceChangeReplyVerify = ssmmsr1_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tid4 = #megaco_term_id{id = ["00000000","00000000","00000004"]},
    Tid5 = #megaco_term_id{id = ["00000000","00000000","00000005"]},
    Tid6 = #megaco_term_id{id = ["00000000","00000000","00000006"]},
    Tid7 = #megaco_term_id{id = ["00000000","00000000","00000007"]},
    Tid8 = #megaco_term_id{id = ["00000000","00000000","00000008"]},
    Tids = [Tid1, Tid2, Tid3, Tid4, Tid5, Tid6, Tid7, Tid8], 
    NotifyReqVerify = ssmmsr1_mg_verify_notify_request_fun(Tids),
    AckVerify = ssmmsr1_mg_verify_ack_fun(), 
    ReadyForSegments = ssmmsr1_mg_ready_for_segments_fun(), 
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, reply_timer,      20000}, 
	     {megaco_update_conn_info, segment_send,     3}, 
	     {megaco_update_conn_info, max_pdu_size,     128}, 
	     %% {sleep, 1000},
             {trigger, ReadyForSegments}, 
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify, 15000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


ssmmsr1_mg_ready_for_segments_fun() ->
    TC = self(),
    fun() ->
	    io:format("ssmmsr1_mg_ready_for_segments_fun -> entry~n", []),
	    TC ! {ready_for_segments, mg, self()},
	    receive
		{continue_with_segments, TC} ->
		    io:format("ssmmsr1_mg_ready_for_segments_fun -> "
			      "received continue~n", []),
		    ok
	    end
    end.
		
ssmmsr1_mg_verify_handle_connect_fun() ->
    fun(Ev) -> ssmmsr1_mg_verify_handle_connect(Ev) end.

ssmmsr1_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("ssmmsr1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
ssmmsr1_mg_verify_handle_connect(Else) ->
    io:format("ssmmsr1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


ssmmsr1_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> ssmmsr1_mg_verify_scr(Rep) end.

ssmmsr1_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch ssmmsr1_mg_do_verify_scr(AR));
ssmmsr1_mg_verify_scr(Crap) ->
    io:format("ssmmsr1_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

ssmmsr1_mg_do_verify_scr(AR) ->
    io:format("ssmmsr1_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

ssmmsr1_mg_verify_notify_request_fun(Tids) ->
    fun(Req) -> ssmmsr1_mg_verify_notify_request(Req, Tids) end.
	     
ssmmsr1_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, Tids) 
  when length(ARs) == length(Tids) ->
    (catch ssmmsr1_mg_do_verify_notify_request(Tids, ARs));
ssmmsr1_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, _Tids) ->
    {error, {invalid_action_requests, ARs}, ok};
ssmmsr1_mg_verify_notify_request(
  {handle_trans_request, CH, V, ARs}, _Tids) ->
    {error, {invalid_trans_request, {CH, V, ARs}}, ok};
ssmmsr1_mg_verify_notify_request(Crap, _Tids) ->
    io:format("ssmmsr1_mg_verify_notify_request -> unknown request"
	      "~n   Crap: ~p"
	      "~n   Tids: ~p"
	      "~n", [Crap, _Tids]),
    {error, {unexpected_event, Crap}, ok}.

ssmmsr1_mg_do_verify_notify_request(Tids, ARs) ->
    io:format("ssmmsr1_mg_do_verify_notify_request -> ok"
	      "~n   Tids: ~p"
	      "~n   ARs:  ~p"
	      "~n", [Tids, ARs]),
    ActionReplies = ssmmsr1_mg_do_verify_notify_request_ars(Tids, ARs), 
    io:format("ssmmsr1_mg_do_verify_notify_request -> ok"
	      "~n   ActionReplies:  ~p"
	      "~n", [ActionReplies]),
    Reply = {{handle_ack, ssmmsr1}, ActionReplies}, 
    {ok, ARs, Reply}.

ssmmsr1_mg_do_verify_notify_request_ars(Tids, ARs) ->
    ssmmsr1_mg_do_verify_notify_request_ars(Tids, ARs, []).

ssmmsr1_mg_do_verify_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmmsr1_mg_do_verify_notify_request_ars([Tid|Tids], [AR|ARs], Acc) ->
    ActionReply = ssmmsr1_mg_do_verify_notify_request_ar(Tid, AR),
    ssmmsr1_mg_do_verify_notify_request_ars(Tids, ARs, [ActionReply|Acc]).

ssmmsr1_mg_do_verify_notify_request_ar(Tid, AR) ->
    io:format("ssmmsr1_mg_do_verify_notify_request_ar -> ok"
	      "~n   Tid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Tid, AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxId, 
			     commandRequests = [CmdReq]} ->
		{CtxId, CmdReq};
	    _ ->
		Reason1 = {invalid_actionRequest, AR},
		throw({error, Reason1, ok})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}, ok})
	end,
    OED = 
	case Cmd of
	    {notifyReq, 
	     #'NotifyRequest'{terminationID            = [Tid],
			      observedEventsDescriptor = ObsEvDesc,
			      errorDescriptor          = asn1_NOVALUE}} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_command, Cmd}, ok})
	end,
    OE = 
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    #'ObservedEventsDescriptor'{observedEventLst = ObsEvLst} ->
		throw({error, {invalid_observedEventLst, ObsEvLst}, ok});
	    _ ->
		throw({error, {invalid_ObservedEventsDescriptor, OED}, ok})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ssmmsr1_mg_notify_reply_ar(Cid, Tid);
	_ ->
	    throw({error, {invalid_ObservedEvent, OE}, ok})
    end.


ssmmsr1_mg_verify_ack_fun() ->
    fun(Event) -> ssmmsr1_mg_verify_ack(Event) end.

ssmmsr1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, AckStatus, ssmmsr1}) ->
    io:format("ssmmsr1_mg_verify_ack -> "
              "~n   AckStatus: ~p"
              "~n   CH:        ~p"
              "~n", [AckStatus, CH]),
    case AckStatus of
	{error, Reason} ->
	    case Reason of
		{segment_failure, SegInfo} when is_list(SegInfo) ->
		    case lists:keysearch(segments_not_acked, 1, SegInfo) of
			{value, {segments_not_acked, [3]}} ->
			    {ok, CH, ok};
			{value, {segments_not_acked, SNs}} ->
			    X = {unexpected_not_acked_segments, SNs},
			    {error, X, ok};
			false ->
			    X = {unexpected_seg_info, SegInfo},
			    {error, X, ok}
		    end;
		_ ->
		    X = {unexpected_reason, Reason},
		    {error, X, ok}
	    end;
	_ ->
	    X = {unexpected_ack_status, AckStatus},
	    {error, X, ok}
    end;
ssmmsr1_mg_verify_ack({handle_trans_ack, CH, ?VERSION, 
		       BadAckStatus, BadAckData}) ->
    {error, {unknown_ack_status, BadAckStatus, BadAckData, CH}, ok};
ssmmsr1_mg_verify_ack(BadEvent) ->
    {error, {unknown_event, BadEvent}, ok}.
    
	    
ssmmsr1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmmsr1_mg_notify_reply_ar(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



send_segmented_msg_missing_seg_reply2(suite) ->
    [];
send_segmented_msg_missing_seg_reply2(doc) ->
    "First missing segment test. "
	"Tests that the callbacks and error messages are delivered "
	"when a segment reply goes missing. Ack expected. "
	"Send window = 1. ";
send_segmented_msg_missing_seg_reply2(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        ssmmsr2),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = ssmmsr2_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = ssmmsr2_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    %% Await MGC ready for segments
    d("await MGC trigger event"),
    MgcPid = 
	receive
	    {ready_for_segments, mgc, Pid1} ->
		d("received MGC trigger event"),
		Pid1
	after 5000 ->
		d("timeout waiting for MGC trigger event: ~p", 
		  [megaco_test_lib:flush()]),
		?ERROR(timeout_MGC_trigger_event)
	end,

    %% Await MG ready for segments
    d("await MG trigger event"),
    MgPid = 
	receive
	    {ready_for_segments, mg, Pid2} ->
		d("received MG trigger event"),
		Pid2
	after 5000 ->
		d("timeout waiting for MG trigger event: ~p", 
		  [megaco_test_lib:flush()]),
		?ERROR(timeout_MG_trigger_event)
	end,

    %% Instruct the MG to continue
    d("send continue to MG"),
    MgPid ! {continue_with_segments, self()}, 

    sleep(500),

    %% Instruct the MGC to continue
    d("send continue to MGC"),
    MgcPid ! {continue_with_segments, self()}, 

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.



%%
%% MGC generator stuff
%%

ssmmsr2_mgc_event_sequence(text, tcp) ->
    DecodeFun = ssmmsr2_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = ssmmsr2_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = ssmmsr2_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = ssmmsr2_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    CtxId1    = 1, 
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    CtxId2    = 2, 
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    CtxId3    = 3, 
    TermId4   = 
	#megaco_term_id{id = ["00000000","00000000","00000004"]},
    CtxId4    = 4, 
    TermId5   = 
	#megaco_term_id{id = ["00000000","00000000","00000005"]},
    CtxId5    = 5, 
    TermId6   = 
	#megaco_term_id{id = ["00000000","00000000","00000006"]},
    CtxId6    = 6, 
    TermId7   = 
	#megaco_term_id{id = ["00000000","00000000","00000007"]},
    CtxId7    = 7, 
    TermId8   = 
	#megaco_term_id{id = ["00000000","00000000","00000008"]},
    CtxId8    = 8, 
    TransId   = 2,
    TermIDs = [TermId1, TermId2, TermId3, TermId4, 
	       TermId5, TermId6, TermId7, TermId8],
    CIDs    = [CtxId1, CtxId2, CtxId3, CtxId4, 
	       CtxId5, CtxId6, CtxId7, CtxId8], 
    NotifyReq = ssmmsr2_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs),
    NrVerifyFun1 = 
	ssmmsr2_mgc_verify_notify_reply_segment_msg_fun(1, false, TransId, 
						      TermId1, CtxId1),
    NrVerifyFun2 = 
    	ssmmsr2_mgc_verify_notify_reply_segment_msg_fun(2, false, TransId, 
							TermId2, CtxId2),
    SegmentRep1 = ssmmsr2_mgc_segment_reply_msg(Mid, TransId, 1, false),
    ReadyForSegments = ssmmsr2_mgc_ready_for_segments_fun(), 
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request",  {ScrVerifyFun, 5000}},
             {send, "service-change-reply",              ServiceChangeRep},
	     %% {expect_nothing, 1000}, 
	     {trigger, "segment send sync trigger", ReadyForSegments}, 
             {send, "notify request",                    NotifyReq},
             {expect_receive, "notify reply: segment 1", {NrVerifyFun1, 1000}},
             {send, "segment reply 1",               SegmentRep1},
             {expect_receive, "notify reply: segment 2", {NrVerifyFun2, 1000}},
             {expect_closed,  20000},
             disconnect
            ],
    EvSeq.

ssmmsr2_mgc_ready_for_segments_fun() ->
    TC = self(),
    fun() ->
	    io:format("ssmmsr2_mgc_ready_for_segments_fun -> entry~n", []),
	    TC ! {ready_for_segments, mgc, self()},
	    receive
		{continue_with_segments, TC} ->
		    io:format("ssmmsr2_mgc_ready_for_segments_fun -> "
			      "received continue~n", []),
		    ok
	    end
    end.
		
ssmmsr2_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

ssmmsr2_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

ssmmsr2_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch ssmmsr2_mgc_verify_service_change_req(Msg)) 
    end.

ssmmsr2_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("ssmmsr2_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

ssmmsr2_mgc_verify_notify_reply_segment_msg_fun(SN, Last, 
					     TransId, TermId, Cid) ->
    fun(Msg) -> 
	    (catch ssmmsr2_mgc_verify_notify_reply_segment(Msg, 
							SN, Last, 
							TransId, TermId, Cid)) 
    end.

ssmmsr2_mgc_verify_notify_reply_segment(#'MegacoMessage'{mess = Mess} = M,
				     SN, Last, TransId, TermId, Cid) ->
    io:format("ssmmsr2_mgc_verify_notify_reply_segment -> entry with"
	      "~n   M:       ~p"
	      "~n   SN:      ~p"
	      "~n   Last:    ~p"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, SN, Last, TransId, TermId, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = asn1_NOVALUE} when (Last == false) ->
		TransRes;
            #'TransactionReply'{transactionId        = TransId,
				transactionResult    = TransRes,
				segmentNumber        = SN,
				segmentationComplete = 'NULL'} when (Last == true) ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
	    {actionReplies, [ActionReply]} ->
		ActionReply;
	    {actionReplies, ActionReplies} ->
		throw({error, {invalid_actionReplies, ActionReplies}});
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
	    #'ActionReply'{contextId    = Cid,
			   commandReply = [CommandReply]} ->
		CommandReply;
	    #'ActionReply'{contextId    = Cid,
			   commandReply = CommandReplies} ->
		throw({error, {invalid_commandReplies, CommandReplies}});
	    _ ->
		throw({error, {invalid_actionReply, AR}})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_NotifyReply, NR}}
    end;
ssmmsr2_mgc_verify_notify_reply_segment(Crap, 
				     _SN, _Last, _TransId, _TermId, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.


ssmmsr2_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmmsr2_mgc_notify_request_msg(Mid, TransId, TermIDs, CIDs) ->
    ARs     = ssmmsr2_mgc_notify_request_ars(TermIDs, CIDs), 
    TR      = cre_transReq(TransId, ARs),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmmsr2_mgc_notify_request_ars(TermIDs, CIDs) ->
    ssmmsr2_mgc_notify_request_ars(TermIDs, CIDs, []).

ssmmsr2_mgc_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmmsr2_mgc_notify_request_ars([TermID|TermIDs], [CID|CIDs], Acc) ->
    AR = ssmmsr2_mgc_notify_request_ar(100+CID, TermID, CID),
    ssmmsr2_mgc_notify_request_ars(TermIDs, CIDs, [AR|Acc]).
    
ssmmsr2_mgc_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation(integer_to_list(19990720+Rid), 
			       integer_to_list(22000000+Rid)),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmmsr2_mgc_segment_reply_msg(Mid, TransId, SN, Last) ->
    SR    = ssmmsr2_mgc_segment_reply(TransId, SN, Last),
    Trans = cre_transaction(SR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

ssmmsr2_mgc_segment_reply(TransId, SN, true) ->
    cre_segReply(TransId, SN, 'NULL');
ssmmsr2_mgc_segment_reply(TransId, SN, false) ->
    cre_segReply(TransId, SN, asn1_NOVALUE).

%% ssmmsr2_mgc_trans_ack_msg(Mid, TransId) ->
%%     TA    = cre_transAck(TransId),
%%     Trans = cre_transaction([TA]),
%%     Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
%%     cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
ssmmsr2_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = ssmmsr2_mg_verify_handle_connect_fun(),
    ServiceChangeReq = ssmmsr2_mg_service_change_request_ar(Mid, 1),
    ServiceChangeReplyVerify = ssmmsr2_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tid4 = #megaco_term_id{id = ["00000000","00000000","00000004"]},
    Tid5 = #megaco_term_id{id = ["00000000","00000000","00000005"]},
    Tid6 = #megaco_term_id{id = ["00000000","00000000","00000006"]},
    Tid7 = #megaco_term_id{id = ["00000000","00000000","00000007"]},
    Tid8 = #megaco_term_id{id = ["00000000","00000000","00000008"]},
    Tids = [Tid1, Tid2, Tid3, Tid4, Tid5, Tid6, Tid7, Tid8], 
    NotifyReqVerify = ssmmsr2_mg_verify_notify_request_fun(Tids),
    AckVerify = ssmmsr2_mg_verify_ack_fun(), 
    ReadyForSegments = ssmmsr2_mg_ready_for_segments_fun(), 
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, reply_timer,      12000}, 
	     {megaco_update_conn_info, segment_send,     1}, 
	     {megaco_update_conn_info, max_pdu_size,     128}, 
             %% {sleep, 1000},
             {trigger, ReadyForSegments}, 
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_ack,     AckVerify, 15000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


ssmmsr2_mg_ready_for_segments_fun() ->
    TC = self(),
    fun() ->
	    io:format("ssmmsr2_mg_ready_for_segments_fun -> entry~n", []),
	    TC ! {ready_for_segments, mg, self()},
	    receive
		{continue_with_segments, TC} ->
		    io:format("ssmmsr2_mg_ready_for_segments_fun -> "
			      "received continue~n", []),
		    ok
	    end
    end.
		
ssmmsr2_mg_verify_handle_connect_fun() ->
    fun(Ev) -> ssmmsr2_mg_verify_handle_connect(Ev) end.

ssmmsr2_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("ssmmsr2_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
ssmmsr2_mg_verify_handle_connect(Else) ->
    io:format("ssmmsr2_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


ssmmsr2_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> ssmmsr2_mg_verify_scr(Rep) end.

ssmmsr2_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch ssmmsr2_mg_do_verify_scr(AR));
ssmmsr2_mg_verify_scr(Crap) ->
    io:format("ssmmsr2_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

ssmmsr2_mg_do_verify_scr(AR) ->
    io:format("ssmmsr2_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

ssmmsr2_mg_verify_notify_request_fun(Tids) ->
    fun(Req) -> ssmmsr2_mg_verify_notify_request(Req, Tids) end.
	     
ssmmsr2_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, Tids) 
  when length(ARs) == length(Tids) ->
    (catch ssmmsr2_mg_do_verify_notify_request(Tids, ARs));
ssmmsr2_mg_verify_notify_request(
  {handle_trans_request, _CH, ?VERSION, ARs}, _Tids) ->
    {error, {invalid_action_requests, ARs}, ok};
ssmmsr2_mg_verify_notify_request(
  {handle_trans_request, CH, V, ARs}, _Tids) ->
    {error, {invalid_trans_request, {CH, V, ARs}}, ok};
ssmmsr2_mg_verify_notify_request(Crap, _Tids) ->
    io:format("ssmmsr2_mg_verify_notify_request -> unknown request"
	      "~n   Crap: ~p"
	      "~n   Tids: ~p"
	      "~n", [Crap, _Tids]),
    {error, {unexpected_event, Crap}, ok}.

ssmmsr2_mg_do_verify_notify_request(Tids, ARs) ->
    io:format("ssmmsr2_mg_do_verify_notify_request -> ok"
	      "~n   Tids: ~p"
	      "~n   ARs:  ~p"
	      "~n", [Tids, ARs]),
    ActionReplies = ssmmsr2_mg_do_verify_notify_request_ars(Tids, ARs), 
    io:format("ssmmsr2_mg_do_verify_notify_request -> ok"
	      "~n   ActionReplies:  ~p"
	      "~n", [ActionReplies]),
    Reply = {{handle_ack, ssmmsr2}, ActionReplies}, 
    {ok, ARs, Reply}.

ssmmsr2_mg_do_verify_notify_request_ars(Tids, ARs) ->
    ssmmsr2_mg_do_verify_notify_request_ars(Tids, ARs, []).

ssmmsr2_mg_do_verify_notify_request_ars([], [], Acc) ->
    lists:reverse(Acc);
ssmmsr2_mg_do_verify_notify_request_ars([Tid|Tids], [AR|ARs], Acc) ->
    ActionReply = ssmmsr2_mg_do_verify_notify_request_ar(Tid, AR),
    ssmmsr2_mg_do_verify_notify_request_ars(Tids, ARs, [ActionReply|Acc]).

ssmmsr2_mg_do_verify_notify_request_ar(Tid, AR) ->
    io:format("ssmmsr2_mg_do_verify_notify_request_ar -> ok"
	      "~n   Tid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Tid, AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxId, 
			     commandRequests = [CmdReq]} ->
		{CtxId, CmdReq};
	    _ ->
		Reason1 = {invalid_actionRequest, AR},
		throw({error, Reason1, ok})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}, ok})
	end,
    OED = 
	case Cmd of
	    {notifyReq, 
	     #'NotifyRequest'{terminationID            = [Tid],
			      observedEventsDescriptor = ObsEvDesc,
			      errorDescriptor          = asn1_NOVALUE}} ->
		ObsEvDesc;
	    _ ->
		throw({error, {invalid_command, Cmd}, ok})
	end,
    OE = 
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEv]} ->
		ObsEv;
	    #'ObservedEventsDescriptor'{observedEventLst = ObsEvLst} ->
		throw({error, {invalid_observedEventLst, ObsEvLst}, ok});
	    _ ->
		throw({error, {invalid_ObservedEventsDescriptor, OED}, ok})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ssmmsr2_mg_notify_reply_ar(Cid, Tid);
	_ ->
	    throw({error, {invalid_ObservedEvent, OE}, ok})
    end.


ssmmsr2_mg_verify_ack_fun() ->
    fun(Event) -> (catch ssmmsr2_mg_verify_ack(Event)) end.

ssmmsr2_mg_verify_ack({handle_trans_ack, CH, ?VERSION, AckStatus, ssmmsr2}) ->
    io:format("ssmmsr2_mg_verify_ack -> "
              "~n   AckStatus: ~p"
              "~n   CH:        ~p"
              "~n", [AckStatus, CH]),
    case AckStatus of
	{error, Reason} ->
	    case Reason of
		{segment_failure, SegInfo} when is_list(SegInfo) ->
		    io:format("ssmmsr2_mg_verify_ack -> verify not acked"
			      "~n", []),
		    case lists:keysearch(segments_not_acked, 1, SegInfo) of
			{value, {segments_not_acked, [2]}} ->
			    ok;
			{value, {segments_not_acked, SNs}} ->
			    X = {unexpected_not_acked_segments, SNs},
			    throw({error, X, ok});
			false ->
			    X = {unexpected_seg_info, SegInfo},
			    throw({error, X, ok})
		    end,
		    io:format("ssmmsr2_mg_verify_ack -> verify not sent"
			      "~n", []),
		    case lists:keysearch(segments_not_sent, 1, SegInfo) of
			{value, {segments_not_sent, NotSent}} ->
			    case [3,4,5,6,7,8] -- NotSent of
				[] ->
				    {ok, CH, ok};
				_ ->
				    Y = {unexpected_not_sent_segments, 
					 NotSent},
				    throw({error, Y, ok})
			    end;
			false ->
			    Y = {unexpected_seg_info, SegInfo},
			    throw({error, Y, ok})
		    end;
		_ ->
		    X = {unexpected_reason, Reason},
		    {error, X, ok}
	    end;
	_ ->
	    X = {unexpected_ack_status, AckStatus},
	    {error, X, ok}
    end;
ssmmsr2_mg_verify_ack({handle_trans_ack, CH, ?VERSION, 
		       BadAckStatus, BadAckData}) ->
    {error, {unknown_ack_status, BadAckStatus, BadAckData, CH}, ok};
ssmmsr2_mg_verify_ack(BadEvent) ->
    {error, {unknown_event, BadEvent}, ok}.
    
	    
ssmmsr2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

ssmmsr2_mg_notify_reply_ar(Cid, Tid) ->
    NR = cre_notifyReply([Tid]),
    CR = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                   %%%
%%%                 Segmented reply received test cases               %%%
%%%                                                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv_segmented_msg_plain(suite) ->
    [];
recv_segmented_msg_plain(doc) ->
    "Received segmented megaco message [plain]";
recv_segmented_msg_plain(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rsmp),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rsmp_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rsmp_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

rsmp_mgc_event_sequence(text, tcp) ->
    DecodeFun = rsmp_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rsmp_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = rsmp_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = rsmp_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    TermIds   = [TermId1, TermId2, TermId3], 
    TransId   = 2,
    ReqId     = 1,
    CtxId     = 1, 
    NrVerifyFun  = 
	rsmp_mgc_verify_notify_req_msg_fun(TermIds, TransId, ReqId, CtxId),
    NotifyRep1   = rsmp_mgc_notify_reply_msg(1, Mid, TransId, CtxId, TermId1),
    NotifyRep2   = rsmp_mgc_notify_reply_msg(2, Mid, TransId, CtxId, TermId2),
    NotifyRep3   = rsmp_mgc_notify_reply_msg(3, Mid, TransId, CtxId, TermId3),
    SrVerifyFun1 = rsmp_mgc_verify_segment_reply_msg_fun(1, TransId),
    SrVerifyFun2 = rsmp_mgc_verify_segment_reply_msg_fun(2, TransId),
    SrVerifyFun3 = rsmp_mgc_verify_segment_reply_msg_fun(3, TransId),
    AckVerifyFun = rsmp_mgc_verify_trans_ack_msg_fun(TransId),
    EvSeq = [{debug,  false},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request", {ScrVerifyFun, 5000}},
             {sleep, 500},

             {send, "service-change-reply",             ServiceChangeRep},
             {expect_receive, "notify-request(1)",      {NrVerifyFun, 4000}},
             {sleep, 500},

             {send, "notify reply - segment 1",         NotifyRep1},
             {expect_receive, "segment reply 1",        {SrVerifyFun1, 2000}},
             {sleep, 500},

             {send, "notify reply - segment 2",         NotifyRep2},
             {expect_receive, "segment reply 2",        {SrVerifyFun2, 2000}},
             {sleep, 500},

             {send, "notify reply - segment 3 (last)",  NotifyRep3},
             {expect_receive, "segment reply 3 (last)", {SrVerifyFun3, 2000}},
             {expect_receive, "ack",                    {AckVerifyFun, 4000}},
             {sleep, 1000},
             disconnect
            ],
    EvSeq.

rsmp_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rsmp_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rsmp_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch rsmp_mgc_verify_service_change_req(Msg)) 
    end.

rsmp_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("rsmp_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

rsmp_mgc_verify_notify_req_msg_fun(TermIds, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rsmp_mgc_verify_notify_req(Msg, 
					      TermIds, TransId, Rid, Cid)) 
    end.

rsmp_mgc_verify_notify_req(#'MegacoMessage'{mess = Mess} = M,
			     TermIds, TransId, Rid, Cid) ->
    io:format("rsmp_mgc_verify_notify_req -> entry with"
	      "~n   M:       ~p"
	      "~n   TermIds: ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermIds, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = TransId,
				  actions       = [ActReq]} ->
		ActReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    Cmds = 
	case AR of
	    #'ActionRequest'{contextId       = Cid,
			     commandRequests = Commands} ->
		Commands;
	    _ ->
		throw({error, {invalid_actions, AR}})
	end,
    ok = rsmp_mgc_verify_notify_req_cmds(TermIds, Cmds),
    {ok, M};
rsmp_mgc_verify_notify_req(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.

rsmp_mgc_verify_notify_req_cmds([], []) ->
    ok;
rsmp_mgc_verify_notify_req_cmds([TermId|TermIds], [Cmd|Cmds]) ->
    rsmp_mgc_verify_notify_req_cmd(TermId, Cmd),
    rsmp_mgc_verify_notify_req_cmds(TermIds, Cmds);
rsmp_mgc_verify_notify_req_cmds(TermIds, Cmds) ->
     throw({error, {invalid_commands, TermIds, Cmds}}).

rsmp_mgc_verify_notify_req_cmd(TermId, #'CommandRequest'{command = Cmd}) ->
    io:format("rsmp_mgc_verify_notify_req_cmd -> entry with"
	      "~n   TermId: ~p"
	      "~n   Cmd:    ~p"
	      "~n", [TermId, Cmd]),
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_command}})
	end,
    OED = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		ObsEvsDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		throw({error, {invalid_observedEventsDescriptor, OED}})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ok;
	_ ->
	    throw({error, {invalid_observedEventLst, OE}})
    end;
rsmp_mgc_verify_notify_req_cmd(_, BadCmdReq) ->
    io:format("rsmp_mgc_verify_notify_req_cmd -> invalid"
	      "~n   BadCmdReq: ~p"
	      "~n", [BadCmdReq]),
    throw({error, {invalid_CommandRequest, BadCmdReq}}).

rsmp_mgc_verify_segment_reply_msg_fun(SN, TransId) ->
    fun(Msg) -> 
	    (catch rsmp_mgc_verify_segment_reply(Msg, SN, TransId)) 
    end.

rsmp_mgc_verify_segment_reply(#'MegacoMessage'{mess = Mess} = M, 
			      SN, TransId) ->
    io:format("rsmp_mgc_verify_segment_reply -> entry with"
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   M:       ~p"
	      "~n", [SN, TransId, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    SR = 
	case Trans of
            {segmentReply, SegmentReply} ->
		SegmentReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case SR of
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = 'NULL'} when SN == 3 ->
	    {ok, M};
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_segmentReply, SR}})
    end;
rsmp_mgc_verify_segment_reply(Crap, SN, TransId) ->
    io:format("rsmp_mgc_verify_segment_reply -> invalid: "
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   Crap:    ~p"
	      "~n", [SN, TransId, Crap]),
    {error, {invalid_MegacoMessage, Crap, SN, TransId}}.

rsmp_mgc_verify_trans_ack_msg_fun(TransId) ->
    fun(Msg) -> 
	    (catch rsmp_mgc_verify_trans_ack(Msg, TransId)) 
    end.

rsmp_mgc_verify_trans_ack(#'MegacoMessage'{mess = Mess} = M, TransId) ->
    io:format("rsmp_mgc_verify_trans_ack -> entry with"
	      "~n   TransId: ~p"
	      "~n   M:       ~p"
	      "~n", [TransId, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TA = 
	case Trans of
            {transactionResponseAck, [TransAck]} ->
		TransAck;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TA of
            #'TransactionAck'{firstAck = TransId,
			      lastAck  = asn1_NOVALUE} ->
		{ok, M};
	    _ ->
		throw({error, {invalid_transactionResponseAck, TA}})
    end;
rsmp_mgc_verify_trans_ack(Crap, _TransId) ->
    {error, {invalid_MegacoMessage, Crap}}.

rsmp_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rsmp_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

rsmp_mgc_notify_reply_msg(SN, Mid, TransId, Cid, TermId) ->
    AR    = rsmp_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR =
	if
	    SN == 3 ->
		cre_transReply(TransId, TRes, SN, 'NULL');
	    true ->
		cre_transReply(TransId, TRes, SN)
	end,
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
rsmp_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ServiceChangeReq = rsmp_mg_service_change_request_ar(Mid, 1),
    ConnectVerify = rsmp_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = rsmp_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tids = [Tid1, Tid2, Tid3],
    NotifyReq = rsmp_mg_notify_request_ar(1, Tids, 1),
    NotifyReplyVerify1 = rsmp_mg_verify_notify_reply_fun(1, Tid1),
    NotifyReplyVerify2 = rsmp_mg_verify_notify_reply_fun(2, Tid2),
    NotifyReplyVerify3 = rsmp_mg_verify_notify_reply_fun(3, Tid3),
    EvSeq = [
             {debug, true},
             %% {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_user_info, protocol_version, ?VERSION}, 
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify1},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify2},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify3},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


rsmp_mg_verify_handle_connect_fun() ->
    fun(Ev) -> rsmp_mg_verify_handle_connect(Ev) end.

rsmp_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("rsmp_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
rsmp_mg_verify_handle_connect(Else) ->
    io:format("rsmp_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


rsmp_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> rsmp_mg_verify_scr(Rep) end.

rsmp_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch rsmp_mg_do_verify_scr(AR));
rsmp_mg_verify_scr(Crap) ->
    io:format("rsmp_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

rsmp_mg_do_verify_scr(AR) ->
    io:format("rsmp_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

rsmp_mg_verify_notify_reply_fun(SN, Tid) ->
    fun(Rep) -> rsmp_mg_verify_notify_reply(Rep, SN, Tid) end.
	     
rsmp_mg_verify_notify_reply(
  {handle_trans_reply, _CH, ?VERSION, {ok, {SN, Last, [AR]}}, _}, SN, Tid) 
  when ((SN =:= 3) andalso (Last =:= true)) orelse 
       ((SN =/= 3) andalso (Last =:= false)) ->
    (catch rsmp_mg_do_verify_notify_reply(Tid, AR));
rsmp_mg_verify_notify_reply(
  {handle_trans_reply, _CH, Version, {ok, {SN1, Last, ARs}}, _}, SN2, Tid) ->
    io:format("rsmp_mg_verify_notify_reply -> unknown reply"
	      "~n   Version: ~p"
	      "~n   SN1:     ~p"
	      "~n   Last:    ~p"
	      "~n   ARs:     ~p"
	      "~n   SN2:     ~p"
	      "~n   Tid:     ~p"
	      "~n", [Version, SN1, Last, ARs, SN2, Tid]),
    Crap = {unexpected_segment_data, [SN1, Last, ARs, SN2, Tid]}, 
    {error, Crap, ok};
rsmp_mg_verify_notify_reply(Crap, SN, Tid) ->
    io:format("rsmp_mg_verify_notify_reply -> unknown reply"
	      "~n   SN:   ~p"
	      "~n   Tid:  ~p"
	      "~n   Crap: ~p"
	      "~n", [SN, Tid, Crap]),
    {error, Crap, ok}.

rsmp_mg_do_verify_notify_reply(Tid, AR) ->
    io:format("rsmp_mg_do_verify_notify_reply -> ok"
	      "~n   Tid:  ~p"
	      "~n   AR:   ~p"
	      "~n", [Tid, AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [Tid],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, AR, ok};
	_ ->
	    Reason3 = {invalid_NotifyReply, NR}, 
	    {error, Reason3, ok}
    end.


rsmp_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rsmp_mg_notify_request_ar(Rid, Tids, Cid) ->
    rsmp_mg_notify_request_ar(Rid, Tids, Cid, []).

rsmp_mg_notify_request_ar(_Rid, [], Cid, Cmds) ->
    cre_actionReq(Cid, lists:reverse(Cmds));
rsmp_mg_notify_request_ar(Rid, [Tid|Tids], Cid, Cmds) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    rsmp_mg_notify_request_ar(Rid, Tids, Cid, [CR|Cmds]).

%% rsmp_internalError(Text) ->
%%     Code = ?megaco_internal_gateway_error,
%%     cre_errorDesc(Code, Text).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv_segmented_msg_ooo_seg(suite) ->
    [];
recv_segmented_msg_ooo_seg(doc) ->
    "Received segmented megaco message [out-of-order segments]";
recv_segmented_msg_ooo_seg(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rsmos),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rsmos_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rsmos_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

rsmos_mgc_event_sequence(text, tcp) ->
    DecodeFun = rsmos_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rsmos_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = rsmos_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = rsmos_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    TermIds   = [TermId1, TermId2, TermId3], 
    TransId   = 2,
    ReqId     = 1,
    CtxId     = 1, 
    NrVerifyFun  = 
	rsmos_mgc_verify_notify_req_msg_fun(TermIds, TransId, ReqId, CtxId),
    NotifyRep1   = rsmos_mgc_notify_reply_msg(1, Mid, TransId, CtxId, TermId1),
    NotifyRep2   = rsmos_mgc_notify_reply_msg(2, Mid, TransId, CtxId, TermId2),
    NotifyRep3   = rsmos_mgc_notify_reply_msg(3, Mid, TransId, CtxId, TermId3),
    SrVerifyFun1 = rsmos_mgc_verify_segment_reply_msg_fun(1, TransId),
    SrVerifyFun2 = rsmos_mgc_verify_segment_reply_msg_fun(2, TransId),
    SrVerifyFun3 = rsmos_mgc_verify_segment_reply_msg_fun(3, TransId),
    AckVerifyFun = rsmos_mgc_verify_trans_ack_msg_fun(TransId),
    EvSeq = [{debug,  false},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request", {ScrVerifyFun, 5000}},
             {send, "service-change-reply",             ServiceChangeRep},
             {expect_receive, "notify-request",         {NrVerifyFun,  4000}},
             {send, "notify reply - segment 3",  NotifyRep3},
             {expect_receive, "segment reply 3", {SrVerifyFun3, 2000}},
             {sleep, 1000},
             {send, "notify reply - segment 2",         NotifyRep2},
             {expect_receive, "segment reply 2",        {SrVerifyFun2, 2000}},
             {sleep, 1000},
             {send, "notify reply - segment 1 (last)",  NotifyRep1},
             {expect_receive, "segment reply 1 (last)", {SrVerifyFun1, 2000}},
             {expect_receive, "ack",                    {AckVerifyFun, 4000}},
	     {expect_nothing, 10000},
             disconnect
            ],
    EvSeq.

rsmos_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rsmos_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rsmos_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch rsmos_mgc_verify_service_change_req(Msg)) 
    end.

rsmos_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("rsmos_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

rsmos_mgc_verify_notify_req_msg_fun(TermIds, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rsmos_mgc_verify_notify_req(Msg, 
					      TermIds, TransId, Rid, Cid)) 
    end.

rsmos_mgc_verify_notify_req(#'MegacoMessage'{mess = Mess} = M,
			     TermIds, TransId, Rid, Cid) ->
    io:format("rsmos_mgc_verify_notify_req -> entry with"
	      "~n   M:       ~p"
	      "~n   TermIds: ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermIds, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = TransId,
				  actions       = [ActReq]} ->
		ActReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    Cmds = 
	case AR of
	    #'ActionRequest'{contextId       = Cid,
			     commandRequests = Commands} ->
		Commands;
	    _ ->
		throw({error, {invalid_actions, AR}})
	end,
    ok = rsmos_mgc_verify_notify_req_cmds(TermIds, Cmds),
    {ok, M};
rsmos_mgc_verify_notify_req(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.

rsmos_mgc_verify_notify_req_cmds([], []) ->
    ok;
rsmos_mgc_verify_notify_req_cmds([TermId|TermIds], [Cmd|Cmds]) ->
    rsmos_mgc_verify_notify_req_cmd(TermId, Cmd),
    rsmos_mgc_verify_notify_req_cmds(TermIds, Cmds);
rsmos_mgc_verify_notify_req_cmds(TermIds, Cmds) ->
     throw({error, {invalid_commands, TermIds, Cmds}}).

rsmos_mgc_verify_notify_req_cmd(TermId, #'CommandRequest'{command = Cmd}) ->
    io:format("rsmos_mgc_verify_notify_req_cmd -> entry with"
	      "~n   TermId: ~p"
	      "~n   Cmd:    ~p"
	      "~n", [TermId, Cmd]),
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_command}})
	end,
    OED = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		ObsEvsDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		throw({error, {invalid_observedEventsDescriptor, OED}})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ok;
	_ ->
	    throw({error, {invalid_observedEventLst, OE}})
    end;
rsmos_mgc_verify_notify_req_cmd(_, BadCmdReq) ->
    io:format("rsmos_mgc_verify_notify_req_cmd -> invalid"
	      "~n   BadCmdReq: ~p"
	      "~n", [BadCmdReq]),
    throw({error, {invalid_CommandRequest, BadCmdReq}}).

rsmos_mgc_verify_segment_reply_msg_fun(SN, TransId) ->
    fun(Msg) -> 
	    (catch rsmos_mgc_verify_segment_reply(Msg, SN, TransId)) 
    end.

rsmos_mgc_verify_segment_reply(#'MegacoMessage'{mess = Mess} = M, 
			      SN, TransId) ->
    io:format("rsmos_mgc_verify_segment_reply -> entry with"
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   M:       ~p"
	      "~n", [SN, TransId, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    SR = 
	case Trans of
            {segmentReply, SegmentReply} ->
		SegmentReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case SR of
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = 'NULL'} when SN == 3 ->
	    {ok, M};
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_segmentReply, SR}})
    end;
rsmos_mgc_verify_segment_reply(Crap, SN, TransId) ->
    io:format("rsmos_mgc_verify_segment_reply -> invalid: "
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   Crap:    ~p"
	      "~n", [SN, TransId, Crap]),
    {error, {invalid_MegacoMessage, Crap, SN, TransId}}.

rsmos_mgc_verify_trans_ack_msg_fun(TransId) ->
    fun(Msg) -> 
	    (catch rsmos_mgc_verify_trans_ack(Msg, TransId)) 
    end.

rsmos_mgc_verify_trans_ack(#'MegacoMessage'{mess = Mess} = M, TransId) ->
    io:format("rsmos_mgc_verify_trans_ack -> entry with"
	      "~n   TransId: ~p"
	      "~n   M:       ~p"
	      "~n", [TransId, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TA = 
	case Trans of
            {transactionResponseAck, [TransAck]} ->
		TransAck;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TA of
            #'TransactionAck'{firstAck = TransId,
			      lastAck  = asn1_NOVALUE} ->
		{ok, M};
	    _ ->
		throw({error, {invalid_transactionResponseAck, TA}})
    end;
rsmos_mgc_verify_trans_ack(Crap, _TransId) ->
    {error, {invalid_MegacoMessage, Crap}}.

rsmos_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rsmos_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

rsmos_mgc_notify_reply_msg(SN, Mid, TransId, Cid, TermId) ->
    AR    = rsmos_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR =
	if
	    SN == 3 ->
		cre_transReply(TransId, TRes, SN, 'NULL');
	    true ->
		cre_transReply(TransId, TRes, SN)
	end,
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
rsmos_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ServiceChangeReq = rsmos_mg_service_change_request_ar(Mid, 1),
    ConnectVerify = rsmos_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = rsmos_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tids = [Tid1, Tid2, Tid3],
    NotifyReq = rsmos_mg_notify_request_ar(1, Tids, 1),
    NotifyReplyVerify1 = rsmos_mg_verify_notify_reply_fun(1, Tid1),
    NotifyReplyVerify2 = rsmos_mg_verify_notify_reply_fun(2, Tid2),
    NotifyReplyVerify3 = rsmos_mg_verify_notify_reply_fun(3, Tid3),
    DiscoVerify        = rsmos_mg_verify_handle_disco_fun(), 
    EvSeq = [
             {debug, true},
             %% {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
	     {megaco_update_user_info, segment_recv_timer, 3000}, 
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify3},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify2},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify1},
             {megaco_callback, handle_disconnect,  DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.


rsmos_mg_verify_handle_connect_fun() ->
    fun(Ev) -> rsmos_mg_verify_handle_connect(Ev) end.

rsmos_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("rsmos_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
rsmos_mg_verify_handle_connect(Else) ->
    io:format("rsmos_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


rsmos_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> rsmos_mg_verify_scr(Rep) end.

rsmos_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch rsmos_mg_do_verify_scr(AR));
rsmos_mg_verify_scr(Crap) ->
    io:format("rsmos_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

rsmos_mg_do_verify_scr(AR) ->
    io:format("rsmos_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

rsmos_mg_verify_notify_reply_fun(SN, Tid) ->
    fun(Rep) -> rsmos_mg_verify_notify_reply(Rep, SN, Tid) end.
	     
rsmos_mg_verify_notify_reply(
  {handle_trans_reply, _CH, ?VERSION, {ok, {SN, Last, [AR]}}, _}, SN, Tid) 
  when ((SN == 1) and (Last == true)) or 
       ((SN =/= 1) and (Last == false)) ->
    (catch rsmos_mg_do_verify_notify_reply(Tid, AR));
rsmos_mg_verify_notify_reply(Crap, SN, Tid) ->
    io:format("rsmos_mg_verify_notify_reply -> unknown reply"
	      "~n   SN:   ~p"
	      "~n   Tid:  ~p"
	      "~n   Crap: ~p"
	      "~n", [SN, Tid, Crap]),
    {error, Crap, ok}.

rsmos_mg_do_verify_notify_reply(Tid, AR) ->
    io:format("rsmos_mg_do_verify_notify_reply -> ok"
	      "~n   Tid:  ~p"
	      "~n   AR:   ~p"
	      "~n", [Tid, AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [Tid],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, AR, ok};
	_ ->
	    Reason3 = {invalid_NotifyReply, NR}, 
	    {error, Reason3, ok}
    end.

rsmos_mg_verify_handle_disco_fun() ->
    fun(Ev) -> rsmos_mg_verify_handle_disconnect(Ev) end.

rsmos_mg_verify_handle_disconnect({handle_disconnect, _CH, ?VERSION, R}) ->
    io:format("rsmos_mg_verify_handle_disconnect -> ok"
	      "~n   R: ~p"
	      "~n", [R]),
    case R of
	{no_controlling_process,shutdown} ->
	    {ok, R, ok};
	_ ->
	    {error, {unexpected_reason, R}, ok}
    end;
rsmos_mg_verify_handle_disconnect(Crap) ->
    io:format("rsmos_mg_verify_handle_disconnect -> invalid: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.
    
	     
rsmos_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rsmos_mg_notify_request_ar(Rid, Tids, Cid) ->
    rsmos_mg_notify_request_ar(Rid, Tids, Cid, []).

rsmos_mg_notify_request_ar(_Rid, [], Cid, Cmds) ->
    cre_actionReq(Cid, lists:reverse(Cmds));
rsmos_mg_notify_request_ar(Rid, [Tid|Tids], Cid, Cmds) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    rsmos_mg_notify_request_ar(Rid, Tids, Cid, [CR|Cmds]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv_segmented_msg_missing_seg1(suite) ->
    [];
recv_segmented_msg_missing_seg1(doc) ->
    "Received segmented megaco message with one segment missing "
	"using plain integer recv segment timer";
recv_segmented_msg_missing_seg1(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rsmms1),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rsmms1_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rsmms1_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

rsmms1_mgc_event_sequence(text, tcp) ->
    DecodeFun = rsmms1_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rsmms1_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = rsmms1_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = rsmms1_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    TermIds   = [TermId1, TermId2, TermId3], 
    TransId   = 2,
    ReqId     = 1,
    CtxId     = 1, 
    NrVerifyFun  = 
	rsmms1_mgc_verify_notify_req_msg_fun(TermIds, TransId, ReqId, CtxId),
    NotifyRep1   = 
	rsmms1_mgc_notify_reply_msg(1, Mid, TransId, CtxId, TermId1),
    NotifyRep3   = 
	rsmms1_mgc_notify_reply_msg(3, Mid, TransId, CtxId, TermId3),
    SrVerifyFun1 = rsmms1_mgc_verify_segment_reply_msg_fun(1, TransId),
    SrVerifyFun3 = rsmms1_mgc_verify_segment_reply_msg_fun(3, TransId),
    MissingSegVerifyFun = rsmms1_mgc_verify_missing_segment_fun("2"),
    EvSeq = [{debug,  false},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request", {ScrVerifyFun, 5000}},
             {send, "service-change-reply",             ServiceChangeRep},
             {expect_receive, "notify-request",         {NrVerifyFun,  4000}},
             {sleep, 1000},
             {send, "notify reply - segment 1",         NotifyRep1},
             {expect_receive, "segment reply 1",        {SrVerifyFun1, 2000}},
             {sleep, 1000},
             {send, "notify reply - segment 3",         NotifyRep3},
             {expect_receive, "segment reply 3",        {SrVerifyFun3, 2000}},
             {expect_receive, "missing segment error",  {MissingSegVerifyFun, 4000}},
	     {expect_nothing, 10000},
             disconnect
            ],
    EvSeq.

rsmms1_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rsmms1_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rsmms1_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch rsmms1_mgc_verify_service_change_req(Msg)) 
    end.

rsmms1_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("rsmms1_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

rsmms1_mgc_verify_notify_req_msg_fun(TermIds, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rsmms1_mgc_verify_notify_req(Msg, 
					      TermIds, TransId, Rid, Cid)) 
    end.

rsmms1_mgc_verify_notify_req(#'MegacoMessage'{mess = Mess} = M,
			     TermIds, TransId, Rid, Cid) ->
    io:format("rsmms1_mgc_verify_notify_req -> entry with"
	      "~n   M:       ~p"
	      "~n   TermIds: ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermIds, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = TransId,
				  actions       = [ActReq]} ->
		ActReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    Cmds = 
	case AR of
	    #'ActionRequest'{contextId       = Cid,
			     commandRequests = Commands} ->
		Commands;
	    _ ->
		throw({error, {invalid_actions, AR}})
	end,
    ok = rsmms1_mgc_verify_notify_req_cmds(TermIds, Cmds),
    {ok, M};
rsmms1_mgc_verify_notify_req(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.

rsmms1_mgc_verify_notify_req_cmds([], []) ->
    ok;
rsmms1_mgc_verify_notify_req_cmds([TermId|TermIds], [Cmd|Cmds]) ->
    rsmms1_mgc_verify_notify_req_cmd(TermId, Cmd),
    rsmms1_mgc_verify_notify_req_cmds(TermIds, Cmds);
rsmms1_mgc_verify_notify_req_cmds(TermIds, Cmds) ->
     throw({error, {invalid_commands, TermIds, Cmds}}).

rsmms1_mgc_verify_notify_req_cmd(TermId, #'CommandRequest'{command = Cmd}) ->
    io:format("rsmms1_mgc_verify_notify_req_cmd -> entry with"
	      "~n   TermId: ~p"
	      "~n   Cmd:    ~p"
	      "~n", [TermId, Cmd]),
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_command}})
	end,
    OED = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		ObsEvsDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		throw({error, {invalid_observedEventsDescriptor, OED}})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ok;
	_ ->
	    throw({error, {invalid_observedEventLst, OE}})
    end;
rsmms1_mgc_verify_notify_req_cmd(_, BadCmdReq) ->
    io:format("rsmms1_mgc_verify_notify_req_cmd -> invalid"
	      "~n   BadCmdReq: ~p"
	      "~n", [BadCmdReq]),
    throw({error, {invalid_CommandRequest, BadCmdReq}}).

rsmms1_mgc_verify_segment_reply_msg_fun(SN, TransId) ->
    fun(Msg) -> 
	    (catch rsmms1_mgc_verify_segment_reply(Msg, SN, TransId)) 
    end.

rsmms1_mgc_verify_segment_reply(#'MegacoMessage'{mess = Mess} = M, 
			      SN, TransId) ->
    io:format("rsmms1_mgc_verify_segment_reply -> entry with"
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   M:       ~p"
	      "~n", [SN, TransId, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    SR = 
	case Trans of
            {segmentReply, SegmentReply} ->
		SegmentReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case SR of
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = 'NULL'} when SN == 3 ->
	    {ok, M};
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_segmentReply, SR}})
    end;
rsmms1_mgc_verify_segment_reply(Crap, SN, TransId) ->
    io:format("rsmms1_mgc_verify_segment_reply -> invalid: "
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   Crap:    ~p"
	      "~n", [SN, TransId, Crap]),
    {error, {invalid_MegacoMessage, Crap, SN, TransId}}.


rsmms1_mgc_verify_missing_segment_fun(SN) ->
    fun(Msg) -> (catch rsmms1_mgc_verify_missing_segment(Msg, SN)) end.

rsmms1_mgc_verify_missing_segment(#'MegacoMessage'{mess = Mess} = M, Text) ->
    io:format("rsmms1_mgc_verify_missing_segment -> entry with"
	      "~n   Text: ~p"
	      "~n   M:    ~p"
	      "~n", [Text, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    case Body of
	{messageError, 
	 #'ErrorDescriptor'{errorCode = ?megaco_segments_not_received,
			    errorText = Text}} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_messageError, Body}})
    end;
rsmms1_mgc_verify_missing_segment(Crap, _SN) ->
    {error, {invalid_MegacoMessage, Crap}}.
    
rsmms1_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rsmms1_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

rsmms1_mgc_notify_reply_msg(SN, Mid, TransId, Cid, TermId) ->
    AR    = rsmms1_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR =
	if
	    SN == 3 ->
		cre_transReply(TransId, TRes, SN, 'NULL');
	    true ->
		cre_transReply(TransId, TRes, SN)
	end,
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
rsmms1_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ServiceChangeReq = rsmms1_mg_service_change_request_ar(Mid, 1),
    ConnectVerify = rsmms1_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = rsmms1_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tids = [Tid1, Tid2, Tid3],
    NotifyReq = rsmms1_mg_notify_request_ar(1, Tids, 1),
    NotifyReplyVerify1 = rsmms1_mg_verify_notify_reply_fun(1, Tid1),
    NotifyReplyVerify3 = rsmms1_mg_verify_notify_reply_fun(3, Tid3),
    SegTimeoutVerify   = rsmms1_mg_verify_segment_timeout_fun(2), 
    DiscoVerify        = rsmms1_mg_verify_handle_disco_fun(), 
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
	     {megaco_update_user_info, segment_recv_timer, 3000}, 
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify1},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify3},
             {megaco_callback, handle_trans_reply, SegTimeoutVerify},
             {megaco_callback, handle_disconnect,  DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.


rsmms1_mg_verify_handle_connect_fun() ->
    fun(Ev) -> rsmms1_mg_verify_handle_connect(Ev) end.

rsmms1_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("rsmms1_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
rsmms1_mg_verify_handle_connect(Else) ->
    io:format("rsmms1_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


rsmms1_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> rsmms1_mg_verify_scr(Rep) end.

rsmms1_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch rsmms1_mg_do_verify_scr(AR));
rsmms1_mg_verify_scr(Crap) ->
    io:format("rsmms1_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

rsmms1_mg_do_verify_scr(AR) ->
    io:format("rsmms1_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

rsmms1_mg_verify_notify_reply_fun(SN, Tid) ->
    fun(Rep) -> rsmms1_mg_verify_notify_reply(Rep, SN, Tid) end.
	     
rsmms1_mg_verify_notify_reply(
  {handle_trans_reply, _CH, ?VERSION, {ok, {SN, Last, [AR]}}, _}, SN, Tid) 
  when (Last == false) ->
    (catch rsmms1_mg_do_verify_notify_reply(Tid, AR));
rsmms1_mg_verify_notify_reply(Crap, SN, Tid) ->
    io:format("rsmms1_mg_verify_notify_reply -> unknown reply"
	      "~n   SN:   ~p"
	      "~n   Tid:  ~p"
	      "~n   Crap: ~p"
	      "~n", [SN, Tid, Crap]),
    {error, Crap, ok}.

rsmms1_mg_do_verify_notify_reply(Tid, AR) ->
    io:format("rsmms1_mg_do_verify_notify_reply -> ok"
	      "~n   Tid:  ~p"
	      "~n   AR:   ~p"
	      "~n", [Tid, AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [Tid],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, AR, ok};
	_ ->
	    Reason3 = {invalid_NotifyReply, NR}, 
	    {error, Reason3, ok}
    end.

rsmms1_mg_verify_segment_timeout_fun(SN) ->
    fun(Rep) -> rsmms1_mg_verify_segment_timeout(Rep, SN) end.
	     
rsmms1_mg_verify_segment_timeout(
  {handle_trans_reply, _CH, ?VERSION, {error, Reason}, _}, SN) ->
    case Reason of
	{segment_timeout, [SN]} ->
	    {ok, Reason, ok};
	_ ->
	    {error, {invalid_reason, Reason}, ok}
    end;
rsmms1_mg_verify_segment_timeout(Crap, SN) ->
    io:format("rsmms1_mg_verify_segment_timeout -> unknown reply"
	      "~n   SN:   ~p"
	      "~n   Crap: ~p"
	      "~n", [SN, Crap]),
    {error, Crap, ok}.

rsmms1_mg_verify_handle_disco_fun() ->
    fun(Ev) -> rsmms1_mg_verify_handle_disconnect(Ev) end.

rsmms1_mg_verify_handle_disconnect({handle_disconnect, _CH, ?VERSION, R}) ->
    io:format("rsmms1_mg_verify_handle_disconnect -> ok"
	      "~n   R: ~p"
	      "~n", [R]),
    case R of
	{no_controlling_process,shutdown} ->
	    {ok, R, ok};
	_ ->
	    {error, {unexpected_reason, R}, ok}
    end;
rsmms1_mg_verify_handle_disconnect(Crap) ->
    io:format("rsmms1_mg_verify_handle_disconnect -> invalid: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.
    
	     
rsmms1_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rsmms1_mg_notify_request_ar(Rid, Tids, Cid) ->
    rsmms1_mg_notify_request_ar(Rid, Tids, Cid, []).

rsmms1_mg_notify_request_ar(_Rid, [], Cid, Cmds) ->
    cre_actionReq(Cid, lists:reverse(Cmds));
rsmms1_mg_notify_request_ar(Rid, [Tid|Tids], Cid, Cmds) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    rsmms1_mg_notify_request_ar(Rid, Tids, Cid, [CR|Cmds]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv_segmented_msg_missing_seg2(suite) ->
    [];
recv_segmented_msg_missing_seg2(doc) ->
    "Received segmented megaco message with one segment missing "
	"using incremental recv segment timer";
recv_segmented_msg_missing_seg2(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rsmms2),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p",
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_tcp_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = rsmms2_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    {ok, MgcId} = megaco_test_tcp_generator:exec(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_megaco_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rsmms2_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    {ok, MgId} = megaco_test_megaco_generator:exec(Mg, MgEvSeq),

    d("await the generator reply(s)"),
    await_completion([MgcId, MgId]),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_tcp_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_megaco_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%%

rsmms2_mgc_event_sequence(text, tcp) ->
    DecodeFun = rsmms2_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rsmms2_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = rsmms2_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = rsmms2_mgc_service_change_reply_msg(Mid, 1),
    TermId1   = 
	#megaco_term_id{id = ["00000000","00000000","00000001"]},
    TermId2   = 
	#megaco_term_id{id = ["00000000","00000000","00000002"]},
    TermId3   = 
	#megaco_term_id{id = ["00000000","00000000","00000003"]},
    TermIds   = [TermId1, TermId2, TermId3], 
    TransId   = 2,
    ReqId     = 1,
    CtxId     = 1, 
    NrVerifyFun  = 
	rsmms2_mgc_verify_notify_req_msg_fun(TermIds, TransId, ReqId, CtxId),
    NotifyRep1   = 
	rsmms2_mgc_notify_reply_msg(1, Mid, TransId, CtxId, TermId1),
    NotifyRep3   = 
	rsmms2_mgc_notify_reply_msg(3, Mid, TransId, CtxId, TermId3),
    SrVerifyFun1 = rsmms2_mgc_verify_segment_reply_msg_fun(1, TransId),
    SrVerifyFun3 = rsmms2_mgc_verify_segment_reply_msg_fun(3, TransId),
    MissingSegVerifyFun = rsmms2_mgc_verify_missing_segment_fun("2"),
    EvSeq = [{debug,  false},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request", {ScrVerifyFun, 5000}},
             {send, "service-change-reply",             ServiceChangeRep},
             {expect_receive, "notify-request",         {NrVerifyFun,  4000}},
             {sleep, 1000},
             {send, "notify reply - segment 1",         NotifyRep1},
             {expect_receive, "segment reply 1",        {SrVerifyFun1, 2000}},
             {sleep, 1000},
             {send, "notify reply - segment 3",         NotifyRep3},
             {expect_receive, "segment reply 3",        {SrVerifyFun3, 2000}},
             {expect_receive, "missing segment error",  {MissingSegVerifyFun, 4000}},
	     {expect_nothing, 10000},
             disconnect
            ],
    EvSeq.

rsmms2_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rsmms2_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rsmms2_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch rsmms2_mgc_verify_service_change_req(Msg)) 
    end.

rsmms2_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("rsmms2_mgc_verify_service_change_req -> entry with"
	      "~n   M: ~p"
	      "~n", [M]),
    Body = 
	case Mess of 
	    #'Message'{version     = 1, 
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	%% Version 1 'ServiceChangeParm'
	{'ServiceChangeParm', 
	 restart,                            % serviceChangeMethod
	 asn1_NOVALUE,                       % serviceChangeAddress
	 ?VERSION,                           % serviceChangeVersion,
	 {'ServiceChangeProfile',"resgw",1}, % serviceChangeProfile
	 [[$9,$0,$1|_]],                     % serviceChangeReason
	 asn1_NOVALUE,                       % serviceChangeDelay
	 asn1_NOVALUE,                       % serviceChangeMgcId
	 asn1_NOVALUE,                       % timeStamp
	 asn1_NOVALUE                        % nonStandardData
	} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

rsmms2_mgc_verify_notify_req_msg_fun(TermIds, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rsmms2_mgc_verify_notify_req(Msg, 
					      TermIds, TransId, Rid, Cid)) 
    end.

rsmms2_mgc_verify_notify_req(#'MegacoMessage'{mess = Mess} = M,
			     TermIds, TransId, Rid, Cid) ->
    io:format("rsmms2_mgc_verify_notify_req -> entry with"
	      "~n   M:       ~p"
	      "~n   TermIds: ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermIds, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = TransId,
				  actions       = [ActReq]} ->
		ActReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    Cmds = 
	case AR of
	    #'ActionRequest'{contextId       = Cid,
			     commandRequests = Commands} ->
		Commands;
	    _ ->
		throw({error, {invalid_actions, AR}})
	end,
    ok = rsmms2_mgc_verify_notify_req_cmds(TermIds, Cmds),
    {ok, M};
rsmms2_mgc_verify_notify_req(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.

rsmms2_mgc_verify_notify_req_cmds([], []) ->
    ok;
rsmms2_mgc_verify_notify_req_cmds([TermId|TermIds], [Cmd|Cmds]) ->
    rsmms2_mgc_verify_notify_req_cmd(TermId, Cmd),
    rsmms2_mgc_verify_notify_req_cmds(TermIds, Cmds);
rsmms2_mgc_verify_notify_req_cmds(TermIds, Cmds) ->
     throw({error, {invalid_commands, TermIds, Cmds}}).

rsmms2_mgc_verify_notify_req_cmd(TermId, #'CommandRequest'{command = Cmd}) ->
    io:format("rsmms2_mgc_verify_notify_req_cmd -> entry with"
	      "~n   TermId: ~p"
	      "~n   Cmd:    ~p"
	      "~n", [TermId, Cmd]),
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_command}})
	end,
    OED = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		ObsEvsDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		throw({error, {invalid_observedEventsDescriptor, OED}})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    ok;
	_ ->
	    throw({error, {invalid_observedEventLst, OE}})
    end;
rsmms2_mgc_verify_notify_req_cmd(_, BadCmdReq) ->
    io:format("rsmms2_mgc_verify_notify_req_cmd -> invalid"
	      "~n   BadCmdReq: ~p"
	      "~n", [BadCmdReq]),
    throw({error, {invalid_CommandRequest, BadCmdReq}}).

rsmms2_mgc_verify_segment_reply_msg_fun(SN, TransId) ->
    fun(Msg) -> 
	    (catch rsmms2_mgc_verify_segment_reply(Msg, SN, TransId)) 
    end.

rsmms2_mgc_verify_segment_reply(#'MegacoMessage'{mess = Mess} = M, 
			      SN, TransId) ->
    io:format("rsmms2_mgc_verify_segment_reply -> entry with"
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   M:       ~p"
	      "~n", [SN, TransId, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    SR = 
	case Trans of
            {segmentReply, SegmentReply} ->
		SegmentReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case SR of
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = 'NULL'} when SN == 3 ->
	    {ok, M};
	#'SegmentReply'{transactionId        = TransId,
			segmentNumber        = SN,
			segmentationComplete = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_segmentReply, SR}})
    end;
rsmms2_mgc_verify_segment_reply(Crap, SN, TransId) ->
    io:format("rsmms2_mgc_verify_segment_reply -> invalid: "
	      "~n   SN:      ~p"
	      "~n   TransId: ~p"
	      "~n   Crap:    ~p"
	      "~n", [SN, TransId, Crap]),
    {error, {invalid_MegacoMessage, Crap, SN, TransId}}.


rsmms2_mgc_verify_missing_segment_fun(SN) ->
    fun(Msg) -> (catch rsmms2_mgc_verify_missing_segment(Msg, SN)) end.

rsmms2_mgc_verify_missing_segment(#'MegacoMessage'{mess = Mess} = M, Text) ->
    io:format("rsmms2_mgc_verify_missing_segment -> entry with"
	      "~n   Text: ~p"
	      "~n   M:    ~p"
	      "~n", [Text, M]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    case Body of
	{messageError, 
	 #'ErrorDescriptor'{errorCode = ?megaco_segments_not_received,
			    errorText = Text}} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_messageError, Body}})
    end;
rsmms2_mgc_verify_missing_segment(Crap, _SN) ->
    {error, {invalid_MegacoMessage, Crap}}.
    
rsmms2_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = {'TransactionReply', 1, asn1_NOVALUE, TRes}, 
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rsmms2_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

rsmms2_mgc_notify_reply_msg(SN, Mid, TransId, Cid, TermId) ->
    AR    = rsmms2_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR =
	if
	    SN == 3 ->
		cre_transReply(TransId, TRes, SN, 'NULL');
	    true ->
		cre_transReply(TransId, TRes, SN)
	end,
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%%
rsmms2_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    SegRecvTmr = #megaco_incr_timer{wait_for    = 1000,
				    factor      = 1, 
				    incr        = 0,
				    max_retries = 2
				   },    
    ServiceChangeReq = rsmms2_mg_service_change_request_ar(Mid, 1),
    ConnectVerify = rsmms2_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = rsmms2_mg_verify_service_change_reply_fun(),
    Tid1 = #megaco_term_id{id = ["00000000","00000000","00000001"]},
    Tid2 = #megaco_term_id{id = ["00000000","00000000","00000002"]},
    Tid3 = #megaco_term_id{id = ["00000000","00000000","00000003"]},
    Tids = [Tid1, Tid2, Tid3],
    NotifyReq = rsmms2_mg_notify_request_ar(1, Tids, 1),
    NotifyReplyVerify1 = rsmms2_mg_verify_notify_reply_fun(1, Tid1),
    NotifyReplyVerify3 = rsmms2_mg_verify_notify_reply_fun(3, Tid3),
    SegTimeoutVerify   = rsmms2_mg_verify_segment_timeout_fun(2), 
    DiscoVerify        = rsmms2_mg_verify_handle_disco_fun(), 
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             %% {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_system_info, users},
             {megaco_system_info, connections},
	     {megaco_update_user_info, segment_recv_timer,  SegRecvTmr}, 
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {megaco_update_conn_info, protocol_version, ?VERSION}, 
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify1},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify3},
             {megaco_callback, handle_trans_reply, SegTimeoutVerify},
             {megaco_callback, handle_disconnect,  DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.


rsmms2_mg_verify_handle_connect_fun() ->
    fun(Ev) -> rsmms2_mg_verify_handle_connect(Ev) end.

rsmms2_mg_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("rsmms2_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
rsmms2_mg_verify_handle_connect(Else) ->
    io:format("rsmms2_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


rsmms2_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> rsmms2_mg_verify_scr(Rep) end.

rsmms2_mg_verify_scr({handle_trans_reply, _CH, 1, {ok, [AR]}, _}) ->
    (catch rsmms2_mg_do_verify_scr(AR));
rsmms2_mg_verify_scr(Crap) ->
    io:format("rsmms2_mg_verify_scr -> error: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.

rsmms2_mg_do_verify_scr(AR) ->
    io:format("rsmms2_mg_do_verify_scr -> ok: "
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

rsmms2_mg_verify_notify_reply_fun(SN, Tid) ->
    fun(Rep) -> rsmms2_mg_verify_notify_reply(Rep, SN, Tid) end.
	     
rsmms2_mg_verify_notify_reply(
  {handle_trans_reply, _CH, ?VERSION, {ok, {SN, Last, [AR]}}, _}, SN, Tid) 
  when (Last == false) ->
    (catch rsmms2_mg_do_verify_notify_reply(Tid, AR));
rsmms2_mg_verify_notify_reply(Crap, SN, Tid) ->
    io:format("rsmms2_mg_verify_notify_reply -> unknown reply"
	      "~n   SN:   ~p"
	      "~n   Tid:  ~p"
	      "~n   Crap: ~p"
	      "~n", [SN, Tid, Crap]),
    {error, Crap, ok}.

rsmms2_mg_do_verify_notify_reply(Tid, AR) ->
    io:format("rsmms2_mg_do_verify_notify_reply -> ok"
	      "~n   Tid:  ~p"
	      "~n   AR:   ~p"
	      "~n", [Tid, AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    NR = 
	case CR of
	    {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [Tid],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, AR, ok};
	_ ->
	    Reason3 = {invalid_NotifyReply, NR}, 
	    {error, Reason3, ok}
    end.

rsmms2_mg_verify_segment_timeout_fun(SN) ->
    fun(Rep) -> rsmms2_mg_verify_segment_timeout(Rep, SN) end.
	     
rsmms2_mg_verify_segment_timeout(
  {handle_trans_reply, _CH, ?VERSION, {error, Reason}, _}, SN) ->
    case Reason of
	{segment_timeout, [SN]} ->
	    {ok, Reason, ok};
	_ ->
	    {error, {invalid_reason, Reason}, ok}
    end;
rsmms2_mg_verify_segment_timeout(Crap, SN) ->
    io:format("rsmms2_mg_verify_segment_timeout -> unknown reply"
	      "~n   SN:   ~p"
	      "~n   Crap: ~p"
	      "~n", [SN, Crap]),
    {error, Crap, ok}.

rsmms2_mg_verify_handle_disco_fun() ->
    fun(Ev) -> rsmms2_mg_verify_handle_disconnect(Ev) end.

rsmms2_mg_verify_handle_disconnect({handle_disconnect, _CH, ?VERSION, R}) ->
    io:format("rsmms2_mg_verify_handle_disconnect -> ok"
	      "~n   R: ~p"
	      "~n", [R]),
    case R of
	{no_controlling_process,shutdown} ->
	    {ok, R, ok};
	_ ->
	    {error, {unexpected_reason, R}, ok}
    end;
rsmms2_mg_verify_handle_disconnect(Crap) ->
    io:format("rsmms2_mg_verify_handle_disconnect -> invalid: "
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    {error, Crap, ok}.
    
	     
rsmms2_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rsmms2_mg_notify_request_ar(Rid, Tids, Cid) ->
    rsmms2_mg_notify_request_ar(Rid, Tids, Cid, []).

rsmms2_mg_notify_request_ar(_Rid, [], Cid, Cmds) ->
    cre_actionReq(Cid, lists:reverse(Cmds));
rsmms2_mg_notify_request_ar(Rid, [Tid|Tids], Cid, Cmds) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    rsmms2_mg_notify_request_ar(Rid, Tids, Cid, [CR|Cmds]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Common message creation functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% cre_errorDesc(Code, Text) when is_list(Text) ->
%%     #'ErrorDescriptor'{errorCode = Code, errorText = Text};
%% cre_errorDesc(Code, Text0) ->
%%     Text = lists:flatten(io_lib:format("~w",[Text0])),
%%     #'ErrorDescriptor'{errorCode = Code, errorText = Text}.

cre_segReply(TransId, SN, SC) ->
    megaco_test_msg_v3_lib:cre_SegmentReply(TransId, SN, SC).

cre_serviceChangeParm(M,R,P) ->
    %% Version 1 'ServiceChangeParm'
    {'ServiceChangeParm',
     M,            % serviceChangeMethod,
     asn1_NOVALUE, % serviceChangeAddress
     ?VERSION,     % serviceChangeVersion
     P,            % serviceChangeProfile
     R,            % serviceChangeReason
     asn1_NOVALUE, % serviceChangeDelay
     asn1_NOVALUE, % serviceChangeMgcId
     asn1_NOVALUE, % timeStamp
     asn1_NOVALUE  % nonStandardData
    }.

cre_serviceChangeReq(Tid, Parms) ->
    #'ServiceChangeRequest'{terminationID      = Tid, 
                            serviceChangeParms = Parms}.

cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

cre_obsEvent(Name, Not) ->
    #'ObservedEvent'{eventName    = Name, 
                     timeNotation = Not}.
%% cre_obsEvent(Name, Not, Par) ->
%%     #'ObservedEvent'{eventName    = Name, 
%%                      timeNotation = Not, 
%%                      eventParList = Par}.

cre_obsEvsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId        = Id, 
                                observedEventLst = EvList}.

cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID            = Tid, 
                     observedEventsDescriptor = EvsDesc}.

cre_command(R) when is_record(R, 'NotifyRequest') ->
    {notifyReq, R};
cre_command(R) when is_record(R, 'ServiceChangeRequest') ->
    {serviceChangeReq, R}.

cre_cmdReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_actionReq(CtxId, CmdReqs) when is_list(CmdReqs) ->
    #'ActionRequest'{contextId       = CtxId,
                     commandRequests = CmdReqs}.

cre_transReq(TransId, ARs) when is_list(ARs) ->
    #'TransactionRequest'{transactionId = TransId,
			  actions       = ARs}.

%% --

cre_serviceChangeResParm(Mid) ->
    cre_serviceChangeResParm(Mid, ?VERSION).

cre_serviceChangeResParm(Mid, V) ->
    #'ServiceChangeResParm'{serviceChangeMgcId   = Mid, 
			    serviceChangeVersion = V}.

cre_serviceChangeResult(SCRP) when is_record(SCRP, 'ServiceChangeResParm') ->
    {serviceChangeResParms, SCRP};
cre_serviceChangeResult(ED) when is_record(ED, 'ErrorDescriptor') ->
    {errorDescriptor, ED}.

cre_serviceChangeReply(Tid, Res) ->
    #'ServiceChangeReply'{terminationID       = Tid, 
                          serviceChangeResult = Res}.

cre_cmdReply(R) when is_record(R, 'NotifyReply') ->
    {notifyReply, R};
cre_cmdReply(R) when is_record(R, 'ServiceChangeReply') ->
    {serviceChangeReply, R}.

cre_notifyReply(Tid) ->
    #'NotifyReply'{terminationID = Tid}.

cre_actionReply(CtxId, CmdRep) ->
    #'ActionReply'{contextId    = CtxId,
                   commandReply = CmdRep}.

cre_transResult(ED) when is_record(ED, 'ErrorDescriptor') ->
    {transactionError, ED};
cre_transResult([AR|_] = ARs) when is_record(AR, 'ActionReply') ->
    {actionReplies, ARs}.

%% cre_transReply(TransId, Res) ->
%%     #'TransactionReply'{transactionId     = TransId,
%%                         transactionResult = Res}.

cre_transReply(TransId, Res, SN) ->
    #'TransactionReply'{transactionId     = TransId,
                        transactionResult = Res,
			segmentNumber     = SN}.

cre_transReply(TransId, Res, SN, SC) ->
    #'TransactionReply'{transactionId        = TransId,
                        transactionResult    = Res,
			segmentNumber        = SN,
			segmentationComplete = SC}.

cre_transAck(TransId) ->
    megaco_test_msg_v3_lib:cre_TransactionAck(TransId).


%% --

cre_serviceChangeProf(Name, Ver) when is_list(Name) andalso is_integer(Ver) ->
    #'ServiceChangeProfile'{profileName = Name, 
                            version     = Ver}.

cre_transaction(Trans) when is_record(Trans, 'TransactionRequest') ->
    {transactionRequest, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionPending') ->
    {transactionPending, Trans};
cre_transaction(Trans) 
  when is_record(Trans, 'TransactionReply') or
       (is_tuple(Trans) and (element(1, Trans) == 'TransactionReply')) ->
    {transactionReply, Trans};
cre_transaction(Trans) when is_list(Trans) ->
    {transactionResponseAck, Trans};
cre_transaction(SR) when is_record(SR, 'SegmentReply') ->
    {segmentReply, SR}.

cre_transactions(Trans) when is_list(Trans) ->
    {transactions, Trans}.

cre_message(Version, Mid, Body) ->
    #'Message'{version     = Version,
               mId         = Mid,
               messageBody = Body}.

cre_megacoMessage(Mess) ->
    #'MegacoMessage'{mess = Mess}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Transform a short timer to a long one.
%% The purpose of this is to trick the stack
%% to keep re-sending the request, even after 
%% having received the first pending (which
%% indicates that the other side _IS_ 
%% working on the request).
-ifdef(MEGACO_TEST_CODE).

init_request_timer({short, Ref}) ->
    {long, Ref};  
init_request_timer(O) ->
    O.

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

await_completion(Ids) ->
    case megaco_test_generator_lib:await_completion(Ids) of
	{ok, Reply} ->
	    d("OK => Reply: ~n~p", [Reply]),
	    ok;
	{error, {OK, ERROR}} ->
	    d("ERROR => "
	      "~n   OK:    ~p"
	      "~n   ERROR: ~p", [OK, ERROR]),
	    ?ERROR({failed, ERROR});
	{error, Reply} ->
	    d("ERROR => "
	      "~n   Reply: ~p", [Reply]),
	    ?ERROR({failed, Reply})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
     end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) -> receive after X -> ok end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), get(tc), "INF", F, A).


d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, get(verbosity), get(tc), "DBG", F, A).


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, Tc, P, F, A) ->
    print(printable(Severity, Verbosity), Tc, P, F, A).

print(true, Tc, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s:~w ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), P, self(), get(sname), Tc | A]);
print(_, _, _, _, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


