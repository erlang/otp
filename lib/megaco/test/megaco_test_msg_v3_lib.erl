%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% Purpose: Utility functions for creating the megaco types
%%----------------------------------------------------------------------

-module(megaco_test_msg_v3_lib).

%% ----

-compile({no_auto_import,[error/1]}).

%% ----

-include_lib("megaco/include/megaco_message_v3.hrl").
-include_lib("megaco/include/megaco.hrl").

%% ----

-export([chk_MegacoMessage/2,

	 cre_MegacoMessage/1, 
	 cre_MegacoMessage/2, 

	 cre_ActionReply/2, 
	 cre_ActionReply/3, 
	 cre_ActionReply/4, 
	 cre_ActionRequest/2, 
	 cre_ActionRequest/3, 
	 cre_ActionRequest/4, 
	 cre_AmmDescriptor/1, 
	 cre_AmmRequest/2, 
	 cre_AmmsReply/1, 
	 cre_AmmsReply/2, 
	 cre_AuditDescriptor/0, 
	 cre_AuditDescriptor/1, 
	 cre_AuditDescriptor/2, 
	 cre_AuditReply/1, 
	 cre_AuditRequest/2, 
	 cre_AuditRequest/3, 
	 cre_AuditResult/2, 
	 cre_AuditReturnParameter/1, 
	 cre_AuthenticationHeader/3,
	 cre_BOOLEAN/1, 
	 cre_Command/2, 
	 cre_CommandReply/2, 
	 cre_CommandRequest/1, 
	 cre_CommandRequest/2, 
	 cre_CommandRequest/3, 
	 cre_ContextAttrAuditRequest/0, 
	 cre_ContextAttrAuditRequest/3, 
	 cre_ContextAttrAuditRequest/4, 
	 cre_ContextAttrAuditRequest/5, 
	 cre_ContextAttrAuditRequest/6, 
	 cre_ContextAttrAuditRequest/7, 
	 cre_ContextAttrAuditRequest/8, 
	 cre_ContextAttrAuditRequest/9, 
	 cre_ContextID/1, 
	 cre_ContextRequest/0, 
	 cre_ContextRequest/1, 
	 cre_ContextRequest/2, 
	 cre_ContextRequest/3, 
	 cre_ContextRequest/4, 
	 cre_ContextRequest/5, 
	 cre_ContextRequest/6, 
	 cre_DigitMapDescriptor/0, 
	 cre_DigitMapDescriptor/1, 
	 cre_DigitMapDescriptor/2, 
	 cre_DigitMapName/1, 
	 cre_DigitMapValue/1, 
	 cre_DigitMapValue/4, 
	 cre_DigitMapValue/5, 
	 cre_ErrorCode/1, 
	 cre_ErrorDescriptor/1, 
	 cre_ErrorDescriptor/2, 
	 cre_ErrorText/1, 
	 cre_EventBufferControl/1, 
	 cre_EventBufferDescriptor/1, 
	 cre_EventDM/1, 
	 cre_EventName/1, 
	 cre_EventParameter/2, 
	 cre_EventParameter/4, 
	 cre_EventsDescriptor/0, 
	 cre_EventsDescriptor/2, 
	 cre_EventSpec/2, 
	 cre_EventSpec/3, 
	 %% cre_H221NonStandard/4, 
	 cre_IndAuditParameter/1, 
	 cre_IndAudLocalControlDescriptor/0, 
	 cre_IndAudLocalControlDescriptor/4, 
	 cre_IndAudLocalControlDescriptor/5, 
	 cre_IndAudLocalRemoteDescriptor/1, 
	 cre_IndAudLocalRemoteDescriptor/2, 
	 cre_IndAudMediaDescriptor/0, 
	 cre_IndAudMediaDescriptor/1, 
	 cre_IndAudMediaDescriptor/2, 
	 cre_IndAudPropertyGroup/1, 
	 cre_IndAudPropertyParm/1, 
	 cre_IndAudPropertyParm/2, 
	 cre_IndAudDigitMapDescriptor/0, 
	 cre_IndAudDigitMapDescriptor/1, 
	 cre_IndAudEventBufferDescriptor/1, 
	 cre_IndAudEventBufferDescriptor/2, 
	 cre_IndAudEventsDescriptor/1, 
	 cre_IndAudEventsDescriptor/2, 
	 cre_IndAudEventsDescriptor/3, 
	 cre_IndAudPackagesDescriptor/2, 
	 cre_IndAudSeqSigList/1, 
	 cre_IndAudSeqSigList/2, 
	 cre_IndAudSignal/1, 
	 cre_IndAudSignal/2, 
	 cre_IndAudSignalsDescriptor/1, 
	 cre_IndAudStatisticsDescriptor/1, 
	 cre_IndAudStreamDescriptor/2, 
	 cre_IndAudStreamParms/0, 
	 cre_IndAudStreamParms/1, 
	 cre_IndAudStreamParms/3, 
	 cre_IndAudStreamParms/4, 
	 cre_IndAudTerminationStateDescriptor/1, 
	 cre_IndAudTerminationStateDescriptor/3, 
	 cre_IndAudTerminationStateDescriptor/4, 
	 cre_LocalControlDescriptor/1, 
	 cre_LocalControlDescriptor/2, 
	 cre_LocalControlDescriptor/4, 
	 cre_LocalRemoteDescriptor/1, 
	 cre_MediaDescriptor/0, 
	 cre_MediaDescriptor/1, 
	 cre_MediaDescriptor/2, 
	 cre_Message/3,
	 cre_ModemDescriptor/2, 
	 %% cre_ModemDescriptor/3, 
	 cre_ModemType/1, 
	 cre_MuxDescriptor/2, 
	 %% cre_MuxDescriptor/3, 
	 cre_MuxType/1, 
	 cre_Name/1, 
	 %% cre_NonStandardData/2, 
	 %% cre_NonStandardIdentifier/1, 
	 cre_NotifyBehaviour/2, 
	 cre_NotifyCompletion/1, 
	 cre_NotifyReply/1, 
	 cre_NotifyReply/2, 
	 cre_NotifyRequest/2, 
	 cre_NotifyRequest/3, 
	 cre_ObservedEvent/2, 
	 cre_ObservedEvent/3, 
	 cre_ObservedEvent/4, 
	 cre_ObservedEventsDescriptor/2, 
	 cre_PackagesDescriptor/1, 
	 cre_PackagesItem/2, 
	 cre_PkgdName/1, 
	 cre_PkgdName/2, 
	 cre_PropertyGroup/1, 
	 cre_PropertyParm/2, 
	 cre_PropertyParm/4, 
	 cre_RegulatedEmbeddedDescriptor/0, 
	 cre_RegulatedEmbeddedDescriptor/1, 
	 cre_RegulatedEmbeddedDescriptor/2, 
	 cre_Relation/1, 
	 cre_RequestedActions/0, 
	 cre_RequestedActions/1, 
	 cre_RequestedActions/4, 
	 cre_RequestedActions/6, 
	 cre_RequestedEvent/1, 
	 cre_RequestedEvent/2, 
	 cre_RequestedEvent/3, 
	 cre_RequestedEvent/4, 
	 cre_RequestID/1, 
	 cre_SecondEventsDescriptor/1, 
	 cre_SecondEventsDescriptor/2, 
	 cre_SecondRequestedActions/0, 
	 cre_SecondRequestedActions/1, 
	 cre_SecondRequestedActions/2, 
	 cre_SecondRequestedActions/3, 
	 cre_SecondRequestedActions/5, 
	 cre_SecondRequestedEvent/2, 
	 cre_SecondRequestedEvent/3, 
	 cre_SecondRequestedEvent/4, 
	 cre_SegmentReply/2, 
	 cre_SegmentReply/3, 
	 cre_SelectLogic/1, 
	 cre_SeqSigList/2, 
	 cre_ServiceChangeAddress/2, 
	 cre_ServiceChangeRequest/2, 
	 cre_ServiceChangeMethod/1, 
	 cre_ServiceChangeParm/2, 
	 cre_ServiceChangeParm/4, 
	 cre_ServiceChangeParm/9, 
	 cre_ServiceChangeParm/10, 
	 cre_ServiceChangeProfile/1, 
	 cre_ServiceChangeProfile/2, 
	 cre_ServiceChangeReply/2, 
	 cre_ServiceChangeResParm/0, 
	 cre_ServiceChangeResParm/2, 
	 cre_ServiceChangeResParm/5, 
	 cre_ServiceChangeResult/1, 
	 cre_ServiceState/1, 
	 cre_Signal/1, 
	 cre_Signal/2, 
	 cre_Signal/7, 
	 cre_Signal/9, 
	 cre_Signal/10, 
	 cre_SignalDirection/1, 
	 cre_SignalName/1, 
	 cre_SignalRequest/1, 
	 cre_SignalsDescriptor/1, 
	 cre_SignalType/1, 
	 cre_SigParameter/2, 
	 cre_SigParameter/4, 
	 cre_StatisticsDescriptor/1, 
	 cre_StatisticsParameter/1, 
	 cre_StatisticsParameter/2, 
	 cre_StreamDescriptor/2, 
	 cre_StreamID/1, 
	 cre_StreamMode/1, 
	 cre_StreamParms/0, 
	 cre_StreamParms/1, 
	 cre_StreamParms/2, 
	 cre_StreamParms/3, 
	 cre_StreamParms/4, 
	 cre_SubtractRequest/1, 
	 cre_SubtractRequest/2, 
	 cre_TopologyRequest/3, 
	 cre_TopologyRequest/4, 
	 cre_TerminationAudit/1, 
	 cre_TerminationID/2, 
	 cre_TerminationIDList/1, 
	 cre_TerminationStateDescriptor/1, 
	 cre_TerminationStateDescriptor/2, 
	 cre_TerminationStateDescriptor/3, 
	 cre_TermListAuditResult/2, 
	 cre_TimeNotation/2, 
	 cre_Transaction/1, 
	 cre_TransactionAck/1, 
	 cre_TransactionAck/2, 
	 cre_TransactionId/1, 
	 cre_TransactionPending/1, 
	 cre_TransactionReply/2, 
	 cre_TransactionReply/3, 
	 cre_TransactionReply/4, 
	 cre_TransactionReply/5, 
	 cre_TransactionRequest/2, 
	 cre_Value/1
	 %% cre_WildcardField/1, 
	]).  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cre_MegacoMessage(M) when is_record(M, 'Message') ->
    #'MegacoMessage'{mess = M}.

cre_MegacoMessage(AH, M) 
  when is_record(AH, 'AuthenticationHeader') and
       is_record(M, 'Message') ->
    #'MegacoMessage'{authHeader = AH,
		     mess       = M}.

cre_AuthenticationHeader(SPI, SN, AD) ->
    #'AuthenticationHeader'{secParmIndex = SPI, 
			    seqNum       = SN, 
			    ad           = AD}.

cre_Message(V, Mid, ED) when is_record(ED, 'ErrorDescriptor') ->
    Body = {errorDescriptor, ED},
    #'Message'{version     = V,
	       mId         = Mid,
	       messageBody = Body};
cre_Message(V, Mid, Transactions) when is_list(Transactions) ->
    Body = {transactions, Transactions},
    #'Message'{version     = V,
	       mId         = Mid,
	       messageBody = Body};
cre_Message(V, Mid, {transactions, T} = Body) when is_list(T) ->
    #'Message'{version     = V,
	       mId         = Mid,
	       messageBody = Body};
cre_Message(V, Mid, {errorDescriptor, ED} = Body) 
  when is_record(ED, 'ErrorDescriptor') ->
    #'Message'{version     = V,
	       mId         = Mid,
	       messageBody = Body}.


cre_ErrorDescriptor(EC) when is_integer(EC) ->
    #'ErrorDescriptor'{errorCode = EC}.

cre_ErrorDescriptor(EC, ET) when is_integer(EC) and is_list(ET) ->
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.

cre_ErrorCode(C) when is_integer(C) and (0 =< C) and (C =< 65535) ->
    C;
cre_ErrorCode(C) ->
    exit({invalid_ErrorCode, C}).

cre_ErrorText(T) when is_list(T) ->
    T.

cre_ContextID(Val) when (0 =< Val) and (Val =< 4294967295) ->
    Val;
cre_ContextID(Val) ->
    exit({invalid_ContextID, Val}).

cre_Transaction(TR) when is_record(TR, 'TransactionRequest') ->
    {transactionRequest, TR};
cre_Transaction(TP) when is_record(TP, 'TransactionPending') ->
    {transactionPending, TP};
cre_Transaction(TR) when is_record(TR, 'TransactionReply') ->
    {transactionReply, TR};
cre_Transaction(TRA) when is_list(TRA) ->
    {transactionResponseAck, TRA};
cre_Transaction(SR) when is_record(SR, 'SegmentReply') ->
    {segmentReply, SR}.

cre_TransactionId(Val) when (0 =< Val) and (Val =< 4294967295) ->
    Val;
cre_TransactionId(Val) ->
    exit({invalid_TransactionId, Val}).

cre_TransactionRequest(TransID, ARs) 
  when is_integer(TransID) and is_list(ARs) -> 
    #'TransactionRequest'{transactionId = TransID, 
			  actions       = ARs}.

cre_TransactionPending(TransID) when is_integer(TransID) ->
    #'TransactionPending'{transactionId = TransID}.

cre_TransactionReply(TransID, ED) 
  when is_integer(TransID) and is_record(ED, 'ErrorDescriptor') ->
    Res = {transactionError, ED},
    #'TransactionReply'{transactionId     = TransID,
			transactionResult = Res};
cre_TransactionReply(TransID, ARs) 
  when is_integer(TransID) and is_list(ARs) ->
    Res = {actionReplies, ARs},
    #'TransactionReply'{transactionId     = TransID,
			transactionResult = Res};
cre_TransactionReply(TransID, Res) ->
    error({invalid_TransactionReply_values, {TransID, Res}}).

cre_TransactionReply(TransID, IAR, ED) 
  when is_integer(TransID) and 
       ((IAR == 'NULL') or (IAR == asn1_NOVALUE)) and 
       is_record(ED, 'ErrorDescriptor') ->
    Res = {transactionError, ED},
    #'TransactionReply'{transactionId     = TransID,
			immAckRequired    = IAR, 
			transactionResult = Res};
cre_TransactionReply(TransID, IAR, ARs) 
  when is_integer(TransID) and 
       ((IAR == 'NULL') or (IAR == asn1_NOVALUE)) and 
       is_list(ARs) ->
    Res = {actionReplies, ARs},
    #'TransactionReply'{transactionId     = TransID,
			immAckRequired    = IAR, 
			transactionResult = Res};
cre_TransactionReply(TransID, Res, SN) 
  when (is_integer(SN) and (SN > 0) and (SN =< 65535)) or 
	(SN == asn1_NOVALUE) ->
    TR  = cre_TransactionReply(TransID, Res),
    TR#'TransactionReply'{segmentNumber = SN};
cre_TransactionReply(TransID, Res, SN) ->
    error({invalid_TransactionReply_values, {TransID, Res, SN}}).

cre_TransactionReply(TransID, Res, SN, SC) 
  when is_integer(SN) and ((SC == 'NULL') or (SC == asn1_NOVALUE)) ->
    TR = cre_TransactionReply(TransID, Res),
    TR#'TransactionReply'{segmentNumber        = SN,
			  segmentationComplete = SC};
cre_TransactionReply(TransID, IAR, Res, SN) ->
    cre_TransactionReply(TransID, IAR, Res, SN, asn1_NOVALUE).

cre_TransactionReply(TransID, IAR, Res, SN, SC) 
  when (SN == asn1_NOVALUE) and (SC == asn1_NOVALUE) ->
    cre_TransactionReply(TransID, IAR, Res);
cre_TransactionReply(TransID, IAR, Res, SN, SC) 
  when (is_integer(SN) and (SN > 0) and (SN =< 65535)) and 
       ((SC == asn1_NOVALUE) or (SC == 'NULL')) ->
    TR = cre_TransactionReply(TransID, IAR, Res),
    TR#'TransactionReply'{segmentNumber        = SN,
			  segmentationComplete = SC};
cre_TransactionReply(TransID, IAR, Res, SN, SC) ->
    error({invalid_TransactionReply_values, {TransID, IAR, Res, SN, SC}}).


cre_SegmentReply(TransID, SN) 
  when is_integer(TransID) and
       is_integer(SN) and (SN > 0) and (SN =< 65535) ->
    #'SegmentReply'{transactionId = TransID,
		    segmentNumber = SN}.

cre_SegmentReply(TransID, SN, SC) 
  when is_integer(TransID) and
       is_integer(SN) and (SN > 0) and (SN =< 65535) and
       ((SC == 'NULL') or (SC == asn1_NOVALUE)) ->
    #'SegmentReply'{transactionId        = TransID,
		    segmentNumber        = SN,
		    segmentationComplete = SC}.

cre_TransactionAck(FirstAck) ->
    #'TransactionAck'{firstAck = FirstAck}.

cre_TransactionAck(FirstAck, FirstAck) ->
    #'TransactionAck'{firstAck = FirstAck};
cre_TransactionAck(FirstAck, LastAck) ->
    #'TransactionAck'{firstAck = FirstAck, 
		      lastAck  = LastAck}.

cre_ActionRequest(CtxID, CmdReqs) 
  when is_integer(CtxID) and is_list(CmdReqs) ->
    #'ActionRequest'{contextId       = CtxID,
		     commandRequests = CmdReqs}.

cre_ActionRequest(CtxID, CtxReq, CmdReqs) 
  when is_integer(CtxID) and 
       is_record(CtxReq, 'ContextRequest') and
       is_list(CmdReqs) ->
    #'ActionRequest'{contextId       = CtxID,
		     contextRequest  = CtxReq,
		     commandRequests = CmdReqs};
cre_ActionRequest(CtxID, CAAR, CmdReqs) 
  when is_integer(CtxID) and 
       is_record(CAAR, 'ContextAttrAuditRequest') and
       is_list(CmdReqs) ->
    #'ActionRequest'{contextId           = CtxID,
		     contextAttrAuditReq = CAAR,
		     commandRequests     = CmdReqs}.

cre_ActionRequest(CtxID, CtxReq, CAAR, CmdReqs) 
  when is_integer(CtxID) and 
       (is_record(CtxReq, 'ContextRequest') or 
	(CtxReq == asn1_NOVALUE)) and
       (is_record(CAAR, 'ContextAttrAuditRequest') or 
	(CAAR == asn1_NOVALUE)) and
       is_list(CmdReqs) ->
    #'ActionRequest'{contextId           = CtxID,
		     contextRequest      = CtxReq,
		     contextAttrAuditReq = CAAR,
		     commandRequests     = CmdReqs}.

cre_ActionReply(CtxID, CmdReps) 
  when is_integer(CtxID) and 
       is_list(CmdReps) ->
    #'ActionReply'{contextId    = CtxID,
		   commandReply = CmdReps}.

cre_ActionReply(CtxID, ED, CmdReps) 
  when is_integer(CtxID) and 
       is_record(ED, 'ErrorDescriptor') and 
       is_list(CmdReps) ->
    #'ActionReply'{contextId       = CtxID,
		   errorDescriptor = ED, 
		   commandReply    = CmdReps};
cre_ActionReply(CtxID, CtxReq, CmdReps) 
  when is_integer(CtxID) and
       is_record(CtxReq, 'ContextRequest') and
       is_list(CmdReps) ->
    #'ActionReply'{contextId    = CtxID,
		   contextReply = CtxReq, 
		   commandReply = CmdReps}.

cre_ActionReply(CtxID, ED, CtxReq, CmdReps) 
  when is_integer(CtxID) andalso
       (is_record(ED, 'ErrorDescriptor') orelse (ED =:= asn1_NOVALUE)) andalso
       (is_record(CtxReq, 'ContextRequest') orelse (CtxReq =:= asn1_NOVALUE)) andalso
       is_list(CmdReps) ->
    #'ActionReply'{contextId       = CtxID,
		   errorDescriptor = ED, 
		   contextReply    = CtxReq, 
		   commandReply    = CmdReps}.

cre_ContextRequest() ->
    strip_ContextRequest(#'ContextRequest'{}).

cre_ContextRequest(Prio) 
  when is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15) ->
    strip_ContextRequest(#'ContextRequest'{priority = Prio});
cre_ContextRequest(Em) 
  when (Em =:= true) orelse (Em =:= false) orelse (Em =:= asn1_NOVALUE) ->
    strip_ContextRequest(#'ContextRequest'{emergency = Em});
cre_ContextRequest(Top) when is_list(Top) ->
    strip_ContextRequest(#'ContextRequest'{topologyReq = Top}).

cre_ContextRequest(Prio, Em) 
  when (is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15)) andalso 
       ((Em =:= true) orelse (Em =:= false) orelse (Em =:= asn1_NOVALUE)) ->
    CR = #'ContextRequest'{priority  = Prio,
			   emergency = Em},
    strip_ContextRequest(CR);
cre_ContextRequest(Prio, Top) 
  when is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15) andalso is_list(Top) ->
    CR = #'ContextRequest'{priority    = Prio,
			   topologyReq = Top},
    strip_ContextRequest(CR).

cre_ContextRequest(Prio, Em, Top) 
  when (is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15)) andalso 
       ((Em =:= true) orelse (Em =:= false) orelse (Em =:= asn1_NOVALUE)) andalso
       (is_list(Top) orelse (Top == asn1_NOVALUE)) ->
    CR = #'ContextRequest'{priority    = Prio,
			   emergency   = Em,
			   topologyReq = Top},
    strip_ContextRequest(CR).

cre_ContextRequest(Prio, Em, Top, Ieps) 
  when (is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15)) andalso 
       ((Em =:= true) orelse (Em =:= false) orelse (Em =:= asn1_NOVALUE)) andalso
       (is_list(Top) orelse (Top =:= asn1_NOVALUE)) andalso 
       ((Ieps =:= true) orelse (Ieps =:= false)) ->
    CR = #'ContextRequest'{priority       = Prio,
			   emergency      = Em,
			   topologyReq    = Top,
			   iepscallind    = Ieps},
    strip_ContextRequest(CR);
cre_ContextRequest(Prio, Em, Top, Ctx) 
  when ((is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15)) or 
	(Prio =:= asn1_NOVALUE)) andalso  
       ((Em =:= true) orelse (Em =:= false) orelse (Em =:= asn1_NOVALUE)) andalso 
       (is_list(Top) orelse (Top =:= asn1_NOVALUE)) andalso 
       (is_list(Ctx)) ->
    CR = 
	case context_list_or_prop(Ctx) of
	    contextProp ->
		#'ContextRequest'{priority    = Prio,
				  emergency   = Em,
				  topologyReq = Top,
				  contextProp = Ctx};
	    contextList ->
		#'ContextRequest'{priority    = Prio,
				  emergency   = Em,
				  topologyReq = Top,
				  contextList = Ctx}
	end,
    strip_ContextRequest(CR).

cre_ContextRequest(Prio, Em, Top, Ieps, Ctx) 
  when ((is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15)) orelse 
	(Prio =:= asn1_NOVALUE)) andalso  
       ((Em =:= true) orelse (Em =:= false) orelse (Em =:= asn1_NOVALUE)) andalso 
       (is_list(Top) orelse (Top =:= asn1_NOVALUE)) andalso 
       ((Ieps =:= true) orelse (Ieps =:= false) orelse (Ieps =:= asn1_NOVALUE)) andalso 
       (is_list(Ctx)) ->
    CR = 
	case context_list_or_prop(Ctx) of
	    contextProp ->
		#'ContextRequest'{priority    = Prio,
				  emergency   = Em,
				  topologyReq = Top,
				  iepscallind = Ieps,
				  contextProp = Ctx};
	    contextList ->
		#'ContextRequest'{priority    = Prio,
				  emergency   = Em,
				  topologyReq = Top,
				  iepscallind = Ieps,
				  contextList = Ctx}
	end,
    strip_ContextRequest(CR).

cre_ContextRequest(Prio, Em, Top, Ieps, CtxProp, CtxList) 
  when ((is_integer(Prio) andalso (0 =< Prio) andalso (Prio =< 15)) orelse  
	(Prio =:= asn1_NOVALUE)) andalso 
       ((Em =:= true) orelse (Em =:= false) orelse (Em =:= asn1_NOVALUE)) andalso 
       (is_list(Top) orelse (Top =:= asn1_NOVALUE)) andalso 
       ((Ieps =:= true) orelse (Ieps =:= false) orelse (Ieps =:= asn1_NOVALUE)) andalso 
       (is_list(CtxProp) orelse (CtxProp =:= asn1_NOVALUE)) andalso 
       (is_list(CtxList) orelse (CtxList =:= asn1_NOVALUE)) ->
    CR = #'ContextRequest'{priority    = Prio,
			   emergency   = Em,
			   topologyReq = Top,
			   iepscallind = Ieps,
			   contextProp = CtxProp,
			   contextList = CtxList},
    strip_ContextRequest(CR).

context_list_or_prop(asn1_NOVALUE) ->
    contextProp;
context_list_or_prop([]) ->
    contextProp;
context_list_or_prop([H|T]) ->
    case is_ContextID(H) of
	true ->
	    context_list_or_prop(T, contextList);
	false ->
	    context_list_or_prop(T, contextProp)
    end.

context_list_or_prop([], What) ->
    What;
context_list_or_prop([H|T], What) ->
    case is_ContextID(H) of
	true when (What =:= contextList) ->
	    context_list_or_prop(T, What);
	false when (What =:= contextProp) ->
	    context_list_or_prop(T, What);
	_ ->
	    error({invalid_contextListOrProp, H, What})
    end.
    
strip_ContextRequest(#'ContextRequest'{priority    = asn1_NOVALUE,
				       emergency   = asn1_NOVALUE,
				       topologyReq = asn1_NOVALUE,
				       iepscallind = asn1_NOVALUE,
				       contextProp = asn1_NOVALUE}) ->
    asn1_NOVALUE;
strip_ContextRequest(#'ContextRequest'{priority    = asn1_NOVALUE,
				       emergency   = asn1_NOVALUE,
				       topologyReq = Top,
				       iepscallind = Ieps,
				       contextProp = Prop} = CR) ->
    case (((Top  == [])    or (Top  == asn1_NOVALUE)) and
	  ((Ieps == false) or (Ieps == asn1_NOVALUE)) and
	  ((Prop == [])    or (Prop == asn1_NOVALUE))) of
	true ->
	    asn1_NOVALUE;
	false ->
	    CR
    end;
strip_ContextRequest(CR) ->
    CR.

cre_ContextAttrAuditRequest() ->
    strip_ContextAttrAuditRequest(#'ContextAttrAuditRequest'{}).

cre_ContextAttrAuditRequest(Top, Em, Prio) 
  when ((Top  =:= 'NULL') orelse (Top  =:= asn1_NOVALUE)) andalso 
       ((Em   =:= 'NULL') orelse (Em   =:= asn1_NOVALUE)) andalso 
       ((Prio =:= 'NULL') orelse (Prio =:= asn1_NOVALUE)) ->
    CAAR = #'ContextAttrAuditRequest'{topology  = Top,
				      emergency = Em,
				      priority  = Prio},
    strip_ContextAttrAuditRequest(CAAR).

cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps) 
  when ((Top  =:= 'NULL') orelse (Top  =:= asn1_NOVALUE)) andalso
       ((Em   =:= 'NULL') orelse (Em   =:= asn1_NOVALUE)) andalso
       ((Prio =:= 'NULL') orelse (Prio =:= asn1_NOVALUE)) andalso
       ((Ieps =:= 'NULL') orelse (Ieps =:= asn1_NOVALUE)) ->
    CAAR = #'ContextAttrAuditRequest'{topology    = Top,
				      emergency   = Em,
				      priority    = Prio,
				      iepscallind = Ieps},
    strip_ContextAttrAuditRequest(CAAR).

cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx) 
  when ((Top  =:= 'NULL') orelse (Top  =:= asn1_NOVALUE)) andalso
       ((Em   =:= 'NULL') orelse (Em   =:= asn1_NOVALUE)) andalso
       ((Prio =:= 'NULL') orelse (Prio =:= asn1_NOVALUE)) andalso
       ((Ieps =:= 'NULL') orelse (Ieps =:= asn1_NOVALUE)) andalso
       (is_list(Ctx)      orelse (Ctx  =:= asn1_NOVALUE)) ->
    CAAR = #'ContextAttrAuditRequest'{topology       = Top,
				      emergency      = Em,
				      priority       = Prio,
				      iepscallind    = Ieps,
				      contextPropAud = Ctx},
    strip_ContextAttrAuditRequest(CAAR).

cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx, SelPrio) 
  when ((Top  =:= 'NULL') orelse (Top  =:= asn1_NOVALUE)) andalso
       ((Em   =:= 'NULL') orelse (Em   =:= asn1_NOVALUE)) andalso
       ((Prio =:= 'NULL') orelse (Prio =:= asn1_NOVALUE)) andalso
       ((Ieps =:= 'NULL') orelse (Ieps =:= asn1_NOVALUE)) andalso
       (is_list(Ctx)      orelse (Ctx  =:= asn1_NOVALUE)) andalso
       ((is_integer(SelPrio) andalso ((0 =< SelPrio) and (SelPrio =< 15))) orelse
	(SelPrio =:= asn1_NOVALUE)) ->
    CAAR = #'ContextAttrAuditRequest'{topology       = Top,
				      emergency      = Em,
				      priority       = Prio,
				      iepscallind    = Ieps,
				      contextPropAud = Ctx,
				      selectpriority = SelPrio},
    strip_ContextAttrAuditRequest(CAAR).

cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx, SelPrio, SelLog) 
  when ((Top  =:= 'NULL') orelse (Top  =:= asn1_NOVALUE)) andalso
       ((Em   =:= 'NULL') orelse (Em   =:= asn1_NOVALUE)) andalso
       ((Prio =:= 'NULL') orelse (Prio =:= asn1_NOVALUE)) andalso
       ((Ieps =:= 'NULL') orelse (Ieps =:= asn1_NOVALUE)) andalso
       (is_list(Ctx)      orelse (Ctx  =:= asn1_NOVALUE)) andalso
       ((is_integer(SelPrio) andalso ((0 =< SelPrio) andalso (SelPrio =< 15))) orelse 
	(SelPrio =:= asn1_NOVALUE)) ->
    case ((SelLog =:= asn1_NOVALUE) orelse is_SelectLogic(SelLog)) of
	true ->
	    CAAR = #'ContextAttrAuditRequest'{topology       = Top,
					      emergency      = Em,
					      priority       = Prio,
					      iepscallind    = Ieps,
					      contextPropAud = Ctx,
					      selectpriority = SelPrio,
					      selectLogic    = SelLog},
	    strip_ContextAttrAuditRequest(CAAR);
	false ->
	    error({invalid_SelectLogic, SelLog, 
		   ['ContextAttrAuditRequest', 
		    Top, Em, Prio, Ieps, Ctx, SelPrio]})
    end.

cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx, 
			    SelPrio, SelEm, SelIeps) 
  when ((Top  == 'NULL') or (Top  == asn1_NOVALUE)) and
       ((Em   == 'NULL') or (Em   == asn1_NOVALUE)) and
       ((Prio == 'NULL') or (Prio == asn1_NOVALUE)) and
       ((Ieps == 'NULL') or (Ieps == asn1_NOVALUE)) and
       (is_list(Ctx)     or (Ctx  == asn1_NOVALUE)) and
       ((is_integer(SelPrio) and ((0 =< SelPrio) and (SelPrio =< 15))) or
	(SelPrio == asn1_NOVALUE)) and
       ((SelEm == true)  or 
	(SelEm == false) or 
	(SelEm == asn1_NOVALUE)) and
       ((SelIeps == true)  or 
	(SelIeps == false) or 
	(SelIeps == asn1_NOVALUE)) ->
    CAAR = #'ContextAttrAuditRequest'{topology          = Top,
				      emergency         = Em,
				      priority          = Prio,
				      iepscallind       = Ieps,
				      contextPropAud    = Ctx,
				      selectpriority    = SelPrio,
				      selectemergency   = SelEm,
				      selectiepscallind = SelIeps},
    strip_ContextAttrAuditRequest(CAAR).

cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx, 
			    SelPrio, SelEm, SelIeps, SelLog) 
  when ((Top  == 'NULL') or (Top  == asn1_NOVALUE)) and
       ((Em   == 'NULL') or (Em   == asn1_NOVALUE)) and
       ((Prio == 'NULL') or (Prio == asn1_NOVALUE)) and
       ((Ieps == 'NULL') or (Ieps == asn1_NOVALUE)) and
       (is_list(Ctx)     or (Ctx  == asn1_NOVALUE)) and
       ((is_integer(SelPrio) and ((0 =< SelPrio) and (SelPrio =< 15))) or
	(SelPrio == asn1_NOVALUE)) and
       ((SelEm == true)  or 
	(SelEm == false) or 
	(SelEm == asn1_NOVALUE)) and
       ((SelIeps == true)  or 
	(SelIeps == false) or 
	(SelIeps == asn1_NOVALUE)) ->
    case ((SelLog == asn1_NOVALUE) orelse is_SelectLogic(SelLog)) of
	true ->
	    CAAR = #'ContextAttrAuditRequest'{topology          = Top,
					      emergency         = Em,
					      priority          = Prio,
					      iepscallind       = Ieps,
					      contextPropAud    = Ctx,
					      selectpriority    = SelPrio,
					      selectemergency   = SelEm,
					      selectiepscallind = SelIeps,
					      selectLogic       = SelLog},
	    strip_ContextAttrAuditRequest(CAAR);
	false ->
	    error({invalid_SelectLogic, SelLog})
    end.

strip_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{topology       = asn1_NOVALUE,
			     emergency      = asn1_NOVALUE,
			     priority       = asn1_NOVALUE,
			     iepscallind    = asn1_NOVALUE,
			     contextPropAud = asn1_NOVALUE}) ->
    asn1_NOVALUE;
strip_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{topology       = asn1_NOVALUE,
			     emergency      = asn1_NOVALUE,
			     priority       = asn1_NOVALUE,
			     iepscallind    = asn1_NOVALUE,
			     contextPropAud = []}) ->
    asn1_NOVALUE;
strip_ContextAttrAuditRequest(CAAR) ->
    CAAR.


cre_SelectLogic(andAUDITSelect = SL) ->
    {SL, 'NULL'};
cre_SelectLogic(orAUDITSelect = SL) ->
    {SL, 'NULL'};
cre_SelectLogic(asn1_NOVALUE) ->
    asn1_NOVALUE.


cre_CommandRequest(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_CommandRequest(Cmd, Opt) 
  when ((Opt == 'NULL') or (Opt == asn1_NOVALUE)) ->
    #'CommandRequest'{command  = Cmd,
		      optional = Opt}.

cre_CommandRequest(Cmd, Opt, WR) 
  when ((Opt == 'NULL') or (Opt == asn1_NOVALUE)) and 
       ((WR  == 'NULL') or (WR  == asn1_NOVALUE)) ->
    #'CommandRequest'{command        = Cmd,
		      optional       = Opt,
		      wildcardReturn = WR}.

cre_Command(addReq = Tag, Req) 
  when is_record(Req, 'AmmRequest') ->
    {Tag, Req};
cre_Command(moveReq = Tag, Req) 
  when is_record(Req, 'AmmRequest') ->
    {Tag, Req};
cre_Command(modReq = Tag, Req) 
  when is_record(Req, 'AmmRequest') ->
    {Tag, Req};
cre_Command(subtractReq = Tag, Req) 
  when is_record(Req, 'SubtractRequest') ->
    {Tag, Req};
cre_Command(auditCapRequest = Tag, Req) 
  when is_record(Req, 'AuditRequest') ->
    {Tag, Req};
cre_Command(auditValueRequest = Tag, Req) 
  when is_record(Req, 'AuditRequest') ->
    {Tag, Req};
cre_Command(notifyReq = Tag, Req) 
  when is_record(Req, 'NotifyRequest') ->
    {Tag, Req};
cre_Command(serviceChangeReq = Tag, Req) 
  when is_record(Req, 'ServiceChangeRequest') ->
    {Tag, Req}.
    
cre_CommandReply(addReply = Tag, Rep) 
  when is_record(Rep, 'AmmsReply') ->
    {Tag, Rep};
cre_CommandReply(moveReply = Tag, Rep) 
  when is_record(Rep, 'AmmsReply') ->
    {Tag, Rep};
cre_CommandReply(modReply = Tag, Rep) 
  when is_record(Rep, 'AmmsReply') ->
    {Tag, Rep};
cre_CommandReply(subtractReply = Tag, Rep) 
  when is_record(Rep, 'AmmsReply') ->
    {Tag, Rep};
cre_CommandReply(auditCapReply = Tag, Rep) 
  when is_tuple(Rep) ->
    {Tag, Rep};
cre_CommandReply(auditValueReply = Tag, Rep) 
  when is_tuple(Rep) ->
    {Tag, Rep};
cre_CommandReply(notifyReply = Tag, Rep) 
  when is_record(Rep, 'NotifyReply') ->
    {Tag, Rep};
cre_CommandReply(serviceChangeReply = Tag, Rep) 
  when is_record(Rep, 'ServiceChangeReply') ->
    {Tag, Rep}.
    

%% -- TopologyRequest --

cre_TopologyRequest(From, To, TD) 
  when (is_record(From, 'TerminationID') or
	is_record(From, megaco_term_id)) and
       (is_record(To, 'TerminationID') or
	is_record(To, megaco_term_id)) and
       ((TD == bothway) or (TD == isolate) or (TD == oneway)) ->
    #'TopologyRequest'{terminationFrom   = From,
		       terminationTo     = To,
		       topologyDirection = TD};
cre_TopologyRequest(From, To, TDE) 
  when (is_record(From, 'TerminationID') or
	is_record(From, megaco_term_id)) and
       (is_record(To, 'TerminationID') or
	is_record(To, megaco_term_id)) and
       ((TDE == onewayexternal) or (TDE == onewayboth)) ->
    #'TopologyRequest'{terminationFrom            = From,
		       terminationTo              = To,
		       topologyDirection          = oneway, 
		       topologyDirectionExtension = TDE}.

cre_TopologyRequest(From, To, TD, SID) 
  when (is_record(From, 'TerminationID') or
	is_record(From, megaco_term_id)) and
       (is_record(To, 'TerminationID') or
	is_record(To, megaco_term_id)) and
       ((TD == bothway) or (TD == isolate) or (TD == oneway)) and
       (is_integer(SID)) ->
    #'TopologyRequest'{terminationFrom   = From,
		       terminationTo     = To,
		       topologyDirection = TD,
		       streamID          = SID};
cre_TopologyRequest(From, To, SID, TDE) 
  when (is_record(From, 'TerminationID') or
	is_record(From, megaco_term_id)) and
       (is_record(To, 'TerminationID') or
	is_record(To, megaco_term_id)) and
       (is_integer(SID)) and
       ((TDE == onewayexternal) or (TDE == onewayboth)) ->
    #'TopologyRequest'{terminationFrom            = From,
		       terminationTo              = To,
		       topologyDirection          = oneway, 
		       streamID                   = SID,
		       topologyDirectionExtension = TDE}.

cre_AmmRequest(TermIDs, Descs) ->
    d("cre_AmmRequest -> entry with"
      "~n   TermIDs: ~p"
      "~n   Descs:   ~p", [TermIDs, Descs]),
    case is_TerminationIDList(TermIDs) andalso 
	is_AmmRequest_descriptors(Descs) of
	true ->
	    #'AmmRequest'{terminationID = TermIDs,
			  descriptors   = Descs};
	false ->
	    error({invalid_AmmRequest, {TermIDs, Descs}})
    end.

cre_AmmDescriptor(D) when is_record(D, 'MediaDescriptor') ->
    {mediaDescriptor, D};
cre_AmmDescriptor(D) when is_record(D, 'ModemDescriptor') ->
    {modemDescriptor, D};
cre_AmmDescriptor(D) when is_record(D, 'MuxDescriptor') ->
    {muxDescriptor, D};
cre_AmmDescriptor(D) when is_record(D, 'EventsDescriptor') ->
    {eventsDescriptor, D};
cre_AmmDescriptor(D) when is_record(D, 'DigitMapDescriptor') ->
    {digitMapDescriptor, D};
cre_AmmDescriptor(D) when is_record(D, 'AuditDescriptor') ->
    {auditDescriptor, D};
cre_AmmDescriptor(D) when is_list(D) ->
    case is_EventBufferDescriptor(D) of
	true ->
	    {eventBufferDescriptor, D};
	false ->
	    case is_SignalsDescriptor(D) of
		true ->
		    {signalsDescriptor, D};
		false ->
		    case is_StatisticsDescriptor(D) of
			true ->
			    {statisticsDescriptor, D};
			false ->
			    error({invalid_AmmDescriptor, D})
		    end
	    end
    end.

cre_AmmsReply(TermIDs) when is_list(TermIDs) ->
    #'AmmsReply'{terminationID = TermIDs}.

cre_AmmsReply(TermIDs, TAs) when is_list(TermIDs) and is_list(TAs) ->
    #'AmmsReply'{terminationID    = TermIDs, 
		 terminationAudit = TAs}.

cre_SubtractRequest(TermIDs) when is_list(TermIDs) ->
    #'SubtractRequest'{terminationID = TermIDs}.

cre_SubtractRequest(TermIDs, Audit) 
  when is_list(TermIDs) and is_record(Audit, 'AuditDescriptor') ->
    #'SubtractRequest'{terminationID   = TermIDs, 
		       auditDescriptor = Audit}.

cre_AuditRequest(TermID, Audit) 
  when is_record(TermID, megaco_term_id) and 
       is_record(Audit, 'AuditDescriptor') ->
    #'AuditRequest'{terminationID   = TermID, 
		    auditDescriptor = Audit}.

cre_AuditRequest(TID, Audit, [TID|_] = TIDs) 
  when is_record(TID, megaco_term_id) and  
       is_record(Audit, 'AuditDescriptor') ->
    #'AuditRequest'{terminationID     = TID, 
		    auditDescriptor   = Audit,
		    terminationIDList = TIDs}.

cre_AuditReply(TermIDs) when is_list(TermIDs) ->
    {contextAuditResult, TermIDs};
cre_AuditReply(ED) when is_record(ED, 'ErrorDescriptor') ->
    {error, ED};
cre_AuditReply(Audit) when is_record(Audit, 'AuditResult') ->
    {auditResult, Audit};
cre_AuditReply(ARTL) when is_record(ARTL, 'TermListAuditResult') ->
    {auditResultTermList, ARTL}.

cre_AuditResult(TID, TAs) 
  when is_record(TID, megaco_term_id) and is_list(TAs) ->
    #'AuditResult'{terminationID          = TID, 
		   terminationAuditResult = TAs}.

cre_TermListAuditResult(TIDs, TA) 
  when is_list(TIDs) and is_list(TA) ->
    #'TermListAuditResult'{terminationIDList      = TIDs,
			   terminationAuditResult = TA}.
    
cre_TerminationAudit(D) ->
    true = is_TerminationAudit(D),
    D.

cre_AuditReturnParameter(D) when is_record(D, 'ErrorDescriptor') ->
    {errorDescriptor, D};
cre_AuditReturnParameter(D) when is_record(D, 'MediaDescriptor') ->
    {mediaDescriptor, D};
cre_AuditReturnParameter(D) when is_record(D, 'ModemDescriptor') ->
    {modemDescriptor, D};
cre_AuditReturnParameter(D) when is_record(D, 'MuxDescriptor') ->
    {muxDescriptor, D};
cre_AuditReturnParameter(D) when is_record(D, 'EventsDescriptor') ->
    {eventsDescriptor, D};
cre_AuditReturnParameter([H|_] = D) when is_record(H, 'EventSpec') ->
    {eventBufferDescriptor, D};
cre_AuditReturnParameter(D) when is_record(D, 'DigitMapDescriptor') ->
    {digitMapDescriptor, D};
cre_AuditReturnParameter(D) when is_record(D, 'ObservedEventsDescriptor') ->
    {observedEventsDescriptor, D};
cre_AuditReturnParameter([H|_] = D) when is_record(H, 'StatisticsParameter') ->
    {statisticsDescriptor, D};
cre_AuditReturnParameter([H|_] = D) when is_record(H, 'PackagesItem') ->
    {packagesDescriptor, D};
cre_AuditReturnParameter(D) when is_record(D, 'AuditDescriptor') ->
    {emptyDescriptors, D};
cre_AuditReturnParameter([H|_] = D) when is_tuple(H) ->
    {signalsDescriptor, D}.

cre_AuditDescriptor() ->
    #'AuditDescriptor'{}.

cre_AuditDescriptor([H|_] = AT) when is_atom(H) ->
    #'AuditDescriptor'{auditToken = AT};
cre_AuditDescriptor(APT) ->
    #'AuditDescriptor'{auditPropertyToken = APT}.

cre_AuditDescriptor(AT, APT) ->
    #'AuditDescriptor'{auditToken         = AT, 
		       auditPropertyToken = APT}.

cre_IndAuditParameter(D) when is_record(D, 'IndAudMediaDescriptor') ->
    {indAudMediaDescriptor, D};
cre_IndAuditParameter(D) when is_record(D, 'IndAudEventsDescriptor') ->
    {indAudEventsDescriptor, D};
cre_IndAuditParameter(D) when is_record(D, 'IndAudEventBufferDescriptor') ->
    {indAudEventBufferDescriptor, D};
cre_IndAuditParameter({signal, _} = D) ->
    {indAudSignalsDescriptor, D};
cre_IndAuditParameter({seqSigList, _} = D) ->
    {indAudSignalsDescriptor, D};
cre_IndAuditParameter(D) when is_record(D, 'IndAudDigitMapDescriptor') ->
    {indAudDigitMapDescriptor, D};
cre_IndAuditParameter(D) when is_record(D, 'IndAudStatisticsDescriptor') ->
    {indAudStatisticsDescriptor, D};
cre_IndAuditParameter(D) when is_record(D, 'IndAudPackagesDescriptor') ->
    {indAudPackagesDescriptor, D}.

cre_IndAudMediaDescriptor() ->
    #'IndAudMediaDescriptor'{}.

cre_IndAudMediaDescriptor(TSD) 
  when is_record(TSD, 'IndAudTerminationStateDescriptor') ->
    #'IndAudMediaDescriptor'{termStateDescr = TSD};
cre_IndAudMediaDescriptor(Parms) when is_record(Parms, 'IndAudStreamParms') ->
    Streams = {oneStream, Parms},
    #'IndAudMediaDescriptor'{streams = Streams};
cre_IndAudMediaDescriptor(Descs) when is_list(Descs) ->
    Streams = {multiStream, Descs},
    #'IndAudMediaDescriptor'{streams = Streams}.

cre_IndAudMediaDescriptor(TSD, Parms) 
  when is_record(TSD, 'IndAudTerminationStateDescriptor') and 
       is_record(Parms, 'IndAudStreamParms') ->
    Streams = {oneStream, Parms},
    #'IndAudMediaDescriptor'{termStateDescr = TSD,
			     streams        = Streams};
cre_IndAudMediaDescriptor(TSD, Descs) 
  when is_record(TSD, 'IndAudTerminationStateDescriptor') and is_list(Descs) ->
    Streams = {multiStream, Descs},
    #'IndAudMediaDescriptor'{termStateDescr = TSD,
			     streams        = Streams}.

cre_IndAudStreamDescriptor(SID, Parms) 
  when is_integer(SID) and is_record(Parms, 'IndAudStreamParms') ->
    #'IndAudStreamDescriptor'{streamID    = SID,
			      streamParms = Parms};
cre_IndAudStreamDescriptor(SID, Parms) ->
    error({invalid_IndAudStreamDescriptor, [SID, Parms]}).

cre_IndAudStreamParms() ->
    #'IndAudStreamParms'{}.

cre_IndAudStreamParms(LCD) 
  when is_record(LCD, 'IndAudLocalControlDescriptor') ->
    #'IndAudStreamParms'{localControlDescriptor = LCD};
cre_IndAudStreamParms(SD) when is_record(SD, 'IndAudStatisticsDescriptor') ->
    #'IndAudStreamParms'{statisticsDescriptor = SD}.

cre_IndAudStreamParms(LC, L, R) 
  when is_record(LC, 'IndAudLocalControlDescriptor') and 
       is_record(L,  'IndAudLocalRemoteDescriptor')  and 
       is_record(R,  'IndAudLocalRemoteDescriptor') ->
    #'IndAudStreamParms'{localControlDescriptor = LC,
			 localDescriptor        = L,
			 remoteDescriptor       = R}.

cre_IndAudStreamParms(LC, L, R, S) 
  when is_record(LC, 'IndAudLocalControlDescriptor') and 
       is_record(L,  'IndAudLocalRemoteDescriptor')  and 
       is_record(R,  'IndAudLocalRemoteDescriptor')  and 
       is_record(S,  'IndAudStatisticsDescriptor') ->
    #'IndAudStreamParms'{localControlDescriptor = LC,
			 localDescriptor        = L,
			 remoteDescriptor       = R,
			 statisticsDescriptor   = S}.

cre_IndAudLocalControlDescriptor() ->
    #'IndAudLocalControlDescriptor'{}.

cre_IndAudLocalControlDescriptor(SM, RV, RG, PP) 
  when ((SM == 'NULL') or (SM == asn1_NOVALUE)) and
       ((RV == 'NULL') or (RV == asn1_NOVALUE)) and
       ((RG == 'NULL') or (RG == asn1_NOVALUE)) and
       (is_list(PP) or (PP == asn1_NOVALUE)) ->
    #'IndAudLocalControlDescriptor'{streamMode    = SM, 
				    reserveValue  = RV, 
				    reserveGroup  = RG, 
				    propertyParms = PP};
cre_IndAudLocalControlDescriptor(RV, RG, PP, SMS) 
  when ((RV == 'NULL') or (RV == asn1_NOVALUE)) and
       ((RG == 'NULL') or (RG == asn1_NOVALUE)) and
       (is_list(PP) or (PP == asn1_NOVALUE))    and
       is_atom(SMS) ->
    #'IndAudLocalControlDescriptor'{reserveValue  = RV, 
				    reserveGroup  = RG, 
				    propertyParms = PP,
				    streamModeSel = SMS}.

cre_IndAudLocalControlDescriptor(SM, RV, RG, PP, SMS) 
  when (SM == 'NULL') and 
       (is_atom(SMS) and (SMS =/= asn1_NOVALUE)) ->
    error({invalid_IndAudLocalControlDescriptor, [SM, RV, RG, PP, SMS]});
cre_IndAudLocalControlDescriptor(SM, RV, RG, PP, SMS) 
  when ((SM == 'NULL') or (SM == asn1_NOVALUE)) and 
       ((RV == 'NULL') or (RV == asn1_NOVALUE)) and
       ((RG == 'NULL') or (RG == asn1_NOVALUE)) and
       (is_list(PP)    or (PP == asn1_NOVALUE)) and
       is_atom(SMS) ->
    case is_StreamMode(SMS) of
	true ->
	    #'IndAudLocalControlDescriptor'{streamMode    = SM, 
					    reserveValue  = RV, 
					    reserveGroup  = RG, 
					    propertyParms = PP,
					    streamModeSel = SMS};
	false ->
	    error({invalid_IndAudLocalControlDescriptor, SMS})
    end.

cre_IndAudPropertyParm(PkgdName) when is_list(PkgdName) ->
    #'IndAudPropertyParm'{name = PkgdName}.

cre_IndAudPropertyParm(PkgdName, PP) 
  when is_list(PkgdName) and is_record(PP, 'PropertyParm') ->
    #'IndAudPropertyParm'{name = PkgdName, propertyParms = PP}.

cre_IndAudLocalRemoteDescriptor(Grps) when is_list(Grps) ->
    #'IndAudLocalRemoteDescriptor'{propGrps = Grps}.

cre_IndAudLocalRemoteDescriptor(GID, Grps) 
  when is_integer(GID) and (0 =< GID) and (GID =< 65535) and is_list(Grps) ->
    #'IndAudLocalRemoteDescriptor'{propGroupID = GID,
				   propGrps    = Grps}.

cre_IndAudPropertyGroup([]) ->
    [];
cre_IndAudPropertyGroup([H|_] = PG) when is_record(H, 'IndAudPropertyParm') ->
    PG.

cre_IndAudTerminationStateDescriptor([] = PP) ->
    #'IndAudTerminationStateDescriptor'{propertyParms = PP};
cre_IndAudTerminationStateDescriptor([H|_] = PP) 
  when is_record(H, 'IndAudPropertyParm') ->
    #'IndAudTerminationStateDescriptor'{propertyParms = PP}.

cre_IndAudTerminationStateDescriptor([] = PP, EBC, SS) 
  when ((EBC == 'NULL') or (EBC == asn1_NOVALUE)) and
       ((SS  == 'NULL') or (SS  == asn1_NOVALUE)) ->
    #'IndAudTerminationStateDescriptor'{propertyParms      = PP,
					eventBufferControl = EBC,
					serviceState       = SS};
cre_IndAudTerminationStateDescriptor([H|_] = PP, EBC, SS) 
  when is_record(H, 'IndAudPropertyParm') and
       ((EBC == 'NULL') or (EBC == asn1_NOVALUE)) and
       ((SS  == 'NULL') or (SS  == asn1_NOVALUE)) ->
    #'IndAudTerminationStateDescriptor'{propertyParms      = PP,
					eventBufferControl = EBC,
					serviceState       = SS}.

cre_IndAudTerminationStateDescriptor(PP, EBC, SS, SSS) 
  when (SS  == 'NULL') and
       ((is_atom(SSS) and (SSS =/= asn1_NOVALUE))) ->
    error({invalid_IndAudTerminationStateDescriptor, [PP, EBC, SS, SSS]});
cre_IndAudTerminationStateDescriptor([] = PP, EBC, SS, SSS) 
  when ((EBC == 'NULL') or (EBC == asn1_NOVALUE)) and
       ((SS  == 'NULL') or (SS  == asn1_NOVALUE)) and
       is_atom(SSS) ->
    #'IndAudTerminationStateDescriptor'{propertyParms      = PP,
					eventBufferControl = EBC,
					serviceState       = SS,
					serviceStateSel    = SSS};
cre_IndAudTerminationStateDescriptor([H|_] = PP, EBC, SS, SSS) 
  when is_record(H, 'IndAudPropertyParm') and
       ((EBC == 'NULL') or (EBC == asn1_NOVALUE)) and
       ((SS  == 'NULL') or (SS  == asn1_NOVALUE)) and
       is_atom(SSS) ->
    #'IndAudTerminationStateDescriptor'{propertyParms      = PP,
					eventBufferControl = EBC,
					serviceState       = SS,
					serviceStateSel    = SSS}.

cre_IndAudEventsDescriptor(PkgdName) 
  when is_list(PkgdName) ->
    #'IndAudEventsDescriptor'{pkgdName = PkgdName}.

cre_IndAudEventsDescriptor(RID, PkgdName) 
  when is_integer(RID) and is_list(PkgdName) ->
    #'IndAudEventsDescriptor'{requestID = RID, pkgdName = PkgdName};
cre_IndAudEventsDescriptor(PkgdName, SID) 
  when is_list(PkgdName) and is_integer(SID) ->
    #'IndAudEventsDescriptor'{pkgdName = PkgdName, streamID = SID}.

cre_IndAudEventsDescriptor(RID, PkgdName, SID) 
  when is_integer(RID) and is_list(PkgdName) and is_integer(SID)  ->
    #'IndAudEventsDescriptor'{requestID = RID, 
			      pkgdName  = PkgdName, 
			      streamID  = SID}.

cre_IndAudEventBufferDescriptor(EventName) when is_list(EventName) ->
    #'IndAudEventBufferDescriptor'{eventName = EventName}.

cre_IndAudEventBufferDescriptor(EventName, SID) 
  when is_list(EventName) and is_integer(SID)  ->
    #'IndAudEventBufferDescriptor'{eventName = EventName, streamID = SID}.

cre_IndAudSignalsDescriptor(S) when is_record(S, 'IndAudSignal') ->
    {signal, S};
cre_IndAudSignalsDescriptor(S) when is_record(S, 'IndAudSeqSigList') ->
    {seqSigList, S}.

cre_IndAudSeqSigList(ID) 
  when is_integer(ID) and (0=< ID) and (ID =< 65535) ->
    #'IndAudSeqSigList'{id = ID}.

cre_IndAudSeqSigList(ID, S) 
  when is_integer(ID) and (0=< ID) and (ID =< 65535) and 
       is_record(S, 'IndAudSignal') ->
    #'IndAudSeqSigList'{id = ID, signalList = S}.

cre_IndAudSignal(SigName) when is_list(SigName) ->
    #'IndAudSignal'{signalName = SigName}.

cre_IndAudSignal(SigName, RID) 
  when is_list(SigName) and is_integer(RID) ->
    #'IndAudSignal'{signalName      = SigName, 
		    signalRequestID = RID}.

cre_IndAudDigitMapDescriptor() ->
    #'IndAudDigitMapDescriptor'{}.

cre_IndAudDigitMapDescriptor(DMN) when is_list(DMN) ->
    #'IndAudDigitMapDescriptor'{digitMapName = DMN}.

cre_IndAudStatisticsDescriptor(StatName) when is_list(StatName) ->
    #'IndAudStatisticsDescriptor'{statName = StatName}.

cre_IndAudPackagesDescriptor(N, V) 
  when is_list(N) and 
       is_integer(V) and (0 =< V) and (V =< 99) ->
    #'IndAudPackagesDescriptor'{packageName    = N, 
				packageVersion = V}.

cre_NotifyRequest(TermIDs, D) 
  when is_list(TermIDs) and is_record(D, 'ObservedEventsDescriptor') ->
    #'NotifyRequest'{terminationID            = TermIDs,
		     observedEventsDescriptor = D}.

cre_NotifyRequest(TermIDs, D, ED) 
  when is_list(TermIDs) andalso 
       is_record(D, 'ObservedEventsDescriptor') andalso 
       is_record(ED, 'ErrorDescriptor') ->
    #'NotifyRequest'{terminationID            = TermIDs,
		     observedEventsDescriptor = D,
		     errorDescriptor          = ED}.
    
cre_NotifyReply(TermIDs) when is_list(TermIDs) ->
    #'NotifyReply'{terminationID = TermIDs}.

cre_NotifyReply(TermIDs, ED) 
  when is_list(TermIDs) andalso 
       is_record(ED, 'ErrorDescriptor') ->
    #'NotifyReply'{terminationID   = TermIDs,
		   errorDescriptor = ED}.

cre_ObservedEventsDescriptor(RID, [H|_] = L) 
  when is_integer(RID) andalso is_record(H, 'ObservedEvent') ->
    #'ObservedEventsDescriptor'{requestId        = RID, 
				observedEventLst = L}.

cre_ObservedEvent(EN, EPL) 
  when is_list(EN) and is_list(EPL) ->
    #'ObservedEvent'{eventName    = EN, 
		     eventParList = EPL};
cre_ObservedEvent(EN, TN) 
  when is_list(EN) and is_record(TN, 'TimeNotation') ->
    #'ObservedEvent'{eventName    = EN, 
		     timeNotation = TN}.

cre_ObservedEvent(EN, SID, EPL) 
  when is_list(EN) and is_integer(SID) and is_list(EPL) ->
    #'ObservedEvent'{eventName    = EN, 
		     streamID     = SID, 
		     eventParList = EPL};
cre_ObservedEvent(EN, EPL, TN) 
  when is_list(EN) and is_list(EPL) and is_record(TN, 'TimeNotation') ->
    #'ObservedEvent'{eventName    = EN, 
		     eventParList = EPL,
		     timeNotation = TN}.

cre_ObservedEvent(EN, SID, EPL, TN) 
  when is_list(EN)     and 
       is_integer(SID) and 
       is_list(EPL)    and
       is_record(TN, 'TimeNotation') ->
    #'ObservedEvent'{eventName    = EN, 
		     streamID     = SID, 
		     eventParList = EPL,
		     timeNotation = TN}.

cre_EventName(N) when is_list(N) ->
    N.

cre_EventParameter(N, V) when is_list(N) and is_list(V) ->
    #'EventParameter'{eventParameterName = N, 
		      value              = V}.

cre_EventParameter(N, V, relation = Tag, R) 
  when is_list(N) and is_list(V) and is_atom(R) ->
    EI = {Tag, R},
    #'EventParameter'{eventParameterName = N, 
		      value              = V,
		      extraInfo          = EI};
cre_EventParameter(N, V, range = Tag, B) 
  when is_list(N) and is_list(V) and is_atom(B) ->
    EI = {Tag, B},
    #'EventParameter'{eventParameterName = N, 
		      value              = V,
		      extraInfo          = EI};
cre_EventParameter(N, V, sublist = Tag, B) 
  when is_list(N) and is_list(V) and is_atom(B) ->
    EI = {Tag, B},
    #'EventParameter'{eventParameterName = N, 
		      value              = V,
		      extraInfo          = EI}.

cre_ServiceChangeRequest(TermIDs, SCP) 
  when is_list(TermIDs) and 
       is_record(SCP, 'ServiceChangeParm') ->
    #'ServiceChangeRequest'{terminationID      = TermIDs, 
			    serviceChangeParms = SCP}.

cre_ServiceChangeReply(TermIDs, {Tag, R} = SCR) 
  when is_list(TermIDs) and is_atom(Tag) and is_tuple(R) ->
    #'ServiceChangeReply'{terminationID       = TermIDs, 
			  serviceChangeResult = SCR}.

cre_ServiceChangeResult(ED) when is_record(ED, 'ErrorDescriptor') ->
    {errorDescriptor, ED};
cre_ServiceChangeResult(SCRP) when is_record(SCRP, 'ServiceChangeResParm') ->
    {serviceChangeResParms, SCRP}.

%% cre_WildcardField(L) when list(L), length(L) == 1 -> L.

cre_TerminationID(W, ID) 
  when is_list(W) andalso 
       is_list(ID) andalso (1 =< length(ID)) andalso (length(ID) =< 8) ->
    #'TerminationID'{wildcard = W, 
		     id       = ID}.

cre_TerminationIDList(L) when is_list(L) ->
    L.

cre_MediaDescriptor() ->
    #'MediaDescriptor'{}.

cre_MediaDescriptor(TSD) when is_record(TSD, 'TerminationStateDescriptor') ->
    #'MediaDescriptor'{termStateDescr = TSD};
cre_MediaDescriptor(SP) when is_record(SP, 'StreamParms') ->
    Streams = {oneStream, SP},
    #'MediaDescriptor'{streams = Streams};
cre_MediaDescriptor([H|_] = SDs) when is_record(H, 'StreamDescriptor') ->
    Streams = {multiStream, SDs},
    #'MediaDescriptor'{streams = Streams}.

cre_MediaDescriptor(TSD, SP) 
  when is_record(TSD, 'TerminationStateDescriptor') and 
       is_record(SP, 'StreamParms') ->
    Streams = {oneStream, SP},
    #'MediaDescriptor'{termStateDescr = TSD,
		       streams        = Streams};
cre_MediaDescriptor(TSD, [H|_] = SDs) 
  when is_record(TSD, 'TerminationStateDescriptor') and 
       is_record(H, 'StreamDescriptor') ->
    Streams = {multiStream, SDs},
    #'MediaDescriptor'{termStateDescr = TSD,
		       streams        = Streams}.

cre_StreamDescriptor(SID, SP) 
  when is_integer(SID) and is_record(SP, 'StreamParms') ->
    #'StreamDescriptor'{streamID    = SID, 
			streamParms = SP}.

cre_StreamParms() ->
    #'StreamParms'{}.

cre_StreamParms(LCD) when is_record(LCD, 'LocalControlDescriptor') ->
    #'StreamParms'{localControlDescriptor = LCD};
cre_StreamParms(LD) when is_record(LD, 'LocalRemoteDescriptor') ->
    #'StreamParms'{localDescriptor = LD};
cre_StreamParms(SD) when is_list(SD) ->
    #'StreamParms'{statisticsDescriptor = SD}.

cre_StreamParms(LCD, LD) 
  when (is_record(LCD, 'LocalControlDescriptor') or (LCD == asn1_NOVALUE)) and
       (is_record(LD,  'LocalRemoteDescriptor')  or (LD  == asn1_NOVALUE)) ->
    #'StreamParms'{localControlDescriptor = LCD,
		   localDescriptor        = LD}.

cre_StreamParms(LCD, LD, RD) 
  when (is_record(LCD, 'LocalControlDescriptor') or (LCD == asn1_NOVALUE)) and
       (is_record(LD,  'LocalRemoteDescriptor')  or (LD  == asn1_NOVALUE)) and
       (is_record(RD,  'LocalRemoteDescriptor')  or (RD  == asn1_NOVALUE)) ->
    #'StreamParms'{localControlDescriptor = LCD,
		   localDescriptor        = LD,
		   remoteDescriptor       = RD};
cre_StreamParms(LCD, LD, SD) 
  when (is_record(LCD, 'LocalControlDescriptor') or (LCD == asn1_NOVALUE)) and
       (is_record(LD,  'LocalRemoteDescriptor')  or (LD  == asn1_NOVALUE)) and
       (is_list(SD) or (SD == asn1_NOVALUE)) ->
    #'StreamParms'{localControlDescriptor = LCD,
		   localDescriptor        = LD,
		   statisticsDescriptor   = SD}.

cre_StreamParms(LCD, LD, RD, SD) 
  when (is_record(LCD, 'LocalControlDescriptor') or (LCD == asn1_NOVALUE)) and
       (is_record(LD,  'LocalRemoteDescriptor')  or (LD  == asn1_NOVALUE)) and
       (is_record(RD,  'LocalRemoteDescriptor')  or (RD  == asn1_NOVALUE)) and
       (is_list(SD) or (SD == asn1_NOVALUE)) ->
    #'StreamParms'{localControlDescriptor = LCD,
		   localDescriptor        = LD,
		   remoteDescriptor       = RD,
		   statisticsDescriptor   = SD}.

cre_LocalControlDescriptor(SM) when is_atom(SM) ->
    #'LocalControlDescriptor'{streamMode = SM, propertyParms = []};
cre_LocalControlDescriptor([H|_] = PP) when is_record(H, 'PropertyParm') ->
    #'LocalControlDescriptor'{propertyParms = PP}.

cre_LocalControlDescriptor(SM, [H|_] = PP) 
  when is_atom(SM) and is_record(H, 'PropertyParm') ->
    #'LocalControlDescriptor'{streamMode    = SM,
			      propertyParms = PP}.

cre_LocalControlDescriptor(SM, RV, RG, [H|_] = PP) 
  when is_atom(SM) and
     ((RV == true) or (RV == false) or (RV == asn1_NOVALUE)) and
     ((RG == true) or (RG == false) or (RG == asn1_NOVALUE)) and
     is_record(H, 'PropertyParm') ->
    #'LocalControlDescriptor'{streamMode    = SM, 
			      reserveValue  = RV, 
			      reserveGroup  = RG, 
			      propertyParms = PP}.
    
cre_StreamMode(sendOnly = M) ->
    M;
cre_StreamMode(recvOnly = M) ->
    M;
cre_StreamMode(sendRecv = M) ->
    M;
cre_StreamMode(inactive = M) ->
    M;
cre_StreamMode(loopBack = M) ->
    M.

cre_PropertyParm(N, [H|_] = V) when is_list(N) and is_list(H) ->
    #'PropertyParm'{name = N, value = V}.

cre_PropertyParm(N, [H|_] = V, relation = Tag, R) 
  when is_list(N) and is_list(H) and is_atom(R) ->
    EI = {Tag, R},
    #'PropertyParm'{name = N, value = V, extraInfo = EI};
cre_PropertyParm(N, [H|_] = V, range = Tag, B) 
  when is_list(N) and is_list(H) and is_atom(B) ->
    EI = {Tag, B},
    #'PropertyParm'{name = N, value = V, extraInfo = EI};
cre_PropertyParm(N, [H|_] = V, sublist = Tag, B) 
  when is_list(N) and is_list(H) and is_atom(B) ->
    EI = {Tag, B},
    #'PropertyParm'{name = N, value = V, extraInfo = EI}.


cre_Name(N) when is_list(N) and (length(N) == 2) ->
    N.

cre_PkgdName(N) when is_list(N) ->
    case string:tokens(N, [$\\]) of
	[_PkgName, _ItemID] ->
	    N;
	_ ->
	    error({invalid_PkgdName, N})
    end.
cre_PkgdName(root, root) ->
    "*/*";
cre_PkgdName(PackageName, root) 
  when is_list(PackageName) and (length(PackageName) =< 64) ->
    PackageName ++ "/*";
cre_PkgdName(PackageName, ItemID) 
  when ((is_list(PackageName) and (length(PackageName) =< 64)) and
	(is_list(ItemID)      and (length(ItemID) =< 64))) ->
    PackageName ++ "/" ++ ItemID;
cre_PkgdName(PackageName, ItemID) ->
    error({invalid_PkgdName, {PackageName, ItemID}}).

cre_Relation(greaterThan = R) ->
    R;
cre_Relation(smallerThan = R) ->
    R;
cre_Relation(unequalTo = R) ->
    R.

cre_LocalRemoteDescriptor([H|_] = PGs) when is_list(H) ->
    #'LocalRemoteDescriptor'{propGrps = PGs}.

cre_PropertyGroup([H|_] = PG) when is_record(H, 'PropertyParm') ->
    PG.
    
cre_TerminationStateDescriptor([H|_] = PPs) 
  when is_record(H, 'PropertyParm') ->
    #'TerminationStateDescriptor'{propertyParms = PPs}.

cre_TerminationStateDescriptor([H|_] = PPs, off = EBC) 
  when is_record(H, 'PropertyParm') ->
    #'TerminationStateDescriptor'{propertyParms      = PPs,
				  eventBufferControl = EBC};
cre_TerminationStateDescriptor([H|_] = PPs, lockStep = EBC) 
  when is_record(H, 'PropertyParm') ->
    #'TerminationStateDescriptor'{propertyParms      = PPs,
				  eventBufferControl = EBC};
cre_TerminationStateDescriptor([H|_] = PPs, test = SS) 
  when is_record(H, 'PropertyParm') ->
    #'TerminationStateDescriptor'{propertyParms = PPs,
				  serviceState  = SS};
cre_TerminationStateDescriptor([H|_] = PPs, outOfSvc = SS) 
  when is_record(H, 'PropertyParm') ->
    #'TerminationStateDescriptor'{propertyParms = PPs,
				  serviceState  = SS};
cre_TerminationStateDescriptor([H|_] = PPs, inSvc = SS) 
  when is_record(H, 'PropertyParm') ->
    #'TerminationStateDescriptor'{propertyParms = PPs,
				  serviceState  = SS}.

cre_TerminationStateDescriptor([H|_] = PPs, EMC, SS) 
  when is_record(H, 'PropertyParm') and 
       ((EMC == off)  or (EMC == lockStep)) and
       ((SS  == test) or (SS  == outOfSvc) or (SS == inSvc)) ->
    #'TerminationStateDescriptor'{propertyParms      = PPs,
				  eventBufferControl = EMC,
				  serviceState       = SS}.

cre_EventBufferControl(off = EBC) ->
    EBC;
cre_EventBufferControl(lockStep = EBC) ->
    EBC.

cre_ServiceState(test = SS) ->
    SS;
cre_ServiceState(outOfSvc = SS) ->
    SS;
cre_ServiceState(inSvc = SS) ->
    SS.

cre_MuxDescriptor(MT, [H|_] = TL) 
  when is_atom(MT) and is_record(H, 'TerminationID') ->
    #'MuxDescriptor'{muxType = MT, termList = TL}.

%% cre_MuxDescriptor(MT, [H|_] = TL, NSD) 
%%   when atom(MT), record(H, 'TerminationID'), record(NSD, 'NonStandardData') ->
%%     #'MuxDescriptor'{muxType = MT, termList = TL, nonStandardData = NSD}.

cre_MuxType(h221 = MT) ->
    MT;
cre_MuxType(h223 = MT) ->
    MT;
cre_MuxType(h226 = MT) ->
    MT;
cre_MuxType(v76 = MT) ->
    MT;
cre_MuxType(nx64k = MT) ->
    MT.

cre_StreamID(Val) when (0 =< Val) and (Val =< 65535) ->
    Val;
cre_StreamID(Val) ->
    exit({invalid_ContextID, Val}).

%% RequestID must be present if eventList is non empty
cre_EventsDescriptor() ->
    #'EventsDescriptor'{eventList = []}.

cre_EventsDescriptor(RID, [H|_] = EL) 
  when is_integer(RID) and is_record(H, 'RequestedEvent') ->
    #'EventsDescriptor'{requestID = RID, eventList = EL}.
    
cre_RequestedEvent(N) ->
    #'RequestedEvent'{pkgdName = N}.

cre_RequestedEvent(N, EPL) 
  when is_list(N) and is_list(EPL) ->
    #'RequestedEvent'{pkgdName  = N,
		      evParList = EPL};
cre_RequestedEvent(N, EA) 
  when is_list(N) and is_record(EA, 'RequestedActions')->
    #'RequestedEvent'{pkgdName    = N,
		      eventAction = EA}.


cre_RequestedEvent(N, SID, EPL) 
  when is_list(N) and is_integer(SID) and is_list(EPL) ->
    #'RequestedEvent'{pkgdName  = N,
		      streamID  = SID, 
		      evParList = EPL};
cre_RequestedEvent(N, EA, EPL) 
  when is_list(N) and is_record(EA, 'RequestedActions') and is_list(EPL) ->
    #'RequestedEvent'{pkgdName     = N,
		      eventAction  = EA, 
		      evParList    = EPL}.

cre_RequestedEvent(N, SID, EA, EPL) 
  when is_list(N) and 
       is_integer(SID) and
       is_record(EA, 'RequestedActions') and 
       is_list(EPL) ->
    #'RequestedEvent'{pkgdName     = N,
		      streamID     = SID, 
		      eventAction  = EA, 
		      evParList    = EPL}.

cre_RegulatedEmbeddedDescriptor() ->
    #'RegulatedEmbeddedDescriptor'{}.

cre_RegulatedEmbeddedDescriptor(D) ->
    case is_SecondEventsDescriptor(D) of
	true ->
	    #'RegulatedEmbeddedDescriptor'{secondEvent = D};
	false ->
	    case is_SignalsDescriptor(D) of
		true ->
		    #'RegulatedEmbeddedDescriptor'{signalsDescriptor = D};
		false ->
		    error({invalid_RegulatedEmbeddedDescriptor, D})
	    end
    end.

cre_RegulatedEmbeddedDescriptor(SED, SD) 
  when ((SED == asn1_NOVALUE) or is_record(SED, 'SecondEventsDescriptor')) and
       ((SD  == asn1_NOVALUE) or is_list(SD)) ->
    #'RegulatedEmbeddedDescriptor'{secondEvent       = SED,
				   signalsDescriptor = SD}.

cre_NotifyBehaviour(notifyImmediate = Tag, 'NULL' = Val) ->
    {Tag, Val};
cre_NotifyBehaviour(notifyRegulated = Tag, Val) 
  when is_record(Val, 'RegulatedEmbeddedDescriptor') ->
    {Tag, Val};
cre_NotifyBehaviour(neverNotify = Tag, 'NULL' = Val) ->
    {Tag, Val};
cre_NotifyBehaviour(Tag, Val) ->
    error({invalid_NotifyBehaviour, [Tag, Val]}).

cre_RequestedActions() ->
    #'RequestedActions'{}.

cre_RequestedActions(KA) 
  when (KA == true) or (KA == true) ->
    #'RequestedActions'{keepActive = KA};
cre_RequestedActions(SE) 
  when is_record(SE, 'SecondEventsDescriptor') ->
    #'RequestedActions'{secondEvent = SE};
cre_RequestedActions(SD) 
  when is_list(SD) ->
    #'RequestedActions'{signalsDescriptor = SD};
cre_RequestedActions({Tag, _} = EDM) 
  when is_atom(Tag) ->
    #'RequestedActions'{eventDM = EDM}.

cre_RequestedActions(KA, {Tag, _} = EDM, SE, SD) 
  when ((KA == true) or (KA == true) or (KA == asn1_NOVALUE)) and
       (is_atom(Tag) or (EDM == asn1_NOVALUE)) and
       (is_record(SE, 'SecondEventsDescriptor') or (SE == asn1_NOVALUE)) and
       (is_list(SD) or (SD == asn1_NOVALUE)) ->
    #'RequestedActions'{keepActive        = KA, 
			eventDM           = EDM, 
			secondEvent       = SE, 
			signalsDescriptor = SD}.

cre_RequestedActions(KA, {EDMTag, _} = EDM, SE, SD, {NBTag, _} = NB, RED) 
  when ((KA == true) or (KA == true) or (KA == asn1_NOVALUE)) and
       (is_atom(EDMTag) or (EDM == asn1_NOVALUE)) and
       (is_record(SE, 'SecondEventsDescriptor') or (SE == asn1_NOVALUE)) and
       (is_list(SD) or (SD == asn1_NOVALUE)) and
       (is_atom(NBTag) or (NB == asn1_NOVALUE)) and
       ((RED == 'NULL') or (RED == asn1_NOVALUE))  ->
    #'RequestedActions'{keepActive            = KA, 
			eventDM               = EDM, 
			secondEvent           = SE, 
			signalsDescriptor     = SD,
			notifyBehaviour       = NB,
			resetEventsDescriptor = RED}.

cre_EventDM(N) when is_list(N) ->
    {digitMapName, N};
cre_EventDM(V) when is_record(V, 'DigitMapValue') ->
    {digitMapValue, V}.

cre_SecondEventsDescriptor([H|_] = EL) 
  when is_record(H, 'SecondRequestedEvent') ->
    #'SecondEventsDescriptor'{eventList = EL}.
    
cre_SecondEventsDescriptor(RID, [H|_] = EL) 
  when is_integer(RID) and is_record(H, 'SecondRequestedEvent') ->
    #'SecondEventsDescriptor'{requestID = RID, eventList = EL}.
    
cre_SecondRequestedEvent(N, EPL) 
  when is_list(N) and is_list(EPL) ->
    #'SecondRequestedEvent'{pkgdName  = N,
			    evParList = EPL};
cre_SecondRequestedEvent(N, EPL) ->
    error({invalid_SecondRequestedEvent, [N, EPL]}).

cre_SecondRequestedEvent(N, SID, EPL) 
  when is_list(N) and is_integer(SID) and is_list(EPL) ->
    #'SecondRequestedEvent'{pkgdName  = N,
			    streamID  = SID, 
			    evParList = EPL};
cre_SecondRequestedEvent(N, EA, EPL) 
  when is_list(N) and 
       is_record(EA, 'SecondRequestedActions') and 
       is_list(EPL) ->
    #'SecondRequestedEvent'{pkgdName     = N,
			    eventAction  = EA, 
			    evParList    = EPL}.

cre_SecondRequestedEvent(N, SID, EA, EPL) 
  when is_list(N) and 
       is_integer(SID) and 
       is_record(EA, 'SecondRequestedActions') and 
       is_list(EPL) ->
    #'SecondRequestedEvent'{pkgdName     = N,
			    streamID     = SID, 
			    eventAction  = EA, 
			    evParList    = EPL}.

cre_SecondRequestedActions() ->
    #'SecondRequestedActions'{}.

cre_SecondRequestedActions(KA) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) ->
    #'SecondRequestedActions'{keepActive = KA};
cre_SecondRequestedActions(SD) when is_list(SD) ->
    #'SecondRequestedActions'{signalsDescriptor = SD};
cre_SecondRequestedActions({Tag, _} = Val) when is_atom(Tag) ->
    case is_EventDM(Val) of
	true ->
	    #'SecondRequestedActions'{eventDM = Val};
	false ->
	    case is_NotifyBehaviour(Val) of
		true ->
		    #'SecondRequestedActions'{notifyBehaviour = Val};
		false ->
		    error({invalid_SecondRequestedActions, Val})
	    end
    end;
cre_SecondRequestedActions('NULL' = RED) ->
    #'SecondRequestedActions'{resetEventsDescriptor = RED}.

cre_SecondRequestedActions(KA, SD) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_list(SD) ->
    #'SecondRequestedActions'{keepActive = KA, signalsDescriptor = SD};
cre_SecondRequestedActions(KA, {Tag, _} = Val) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_atom(Tag) ->
    case is_EventDM(Val) of
	true ->
	    #'SecondRequestedActions'{keepActive = KA, 
				      eventDM    = Val};
	false ->
	    case is_NotifyBehaviour(Val) of
		true ->
		    #'SecondRequestedActions'{keepActive      = KA, 
					      notifyBehaviour = Val};
		false ->
		    error({invalid_SecondRequestedActions, Val})
	    end
    end;
cre_SecondRequestedActions(KA, 'NULL' = RED) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) ->
    #'SecondRequestedActions'{keepActive            = KA, 
			      resetEventsDescriptor = RED}.

cre_SecondRequestedActions(KA, {Tag, _} = EDM, SD) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_atom(Tag) and 
       is_list(SD) ->
    #'SecondRequestedActions'{keepActive        = KA, 
			      eventDM           = EDM, 
			      signalsDescriptor = SD};
cre_SecondRequestedActions(KA, SD, {Tag, _} = NB) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_list(SD) and
       is_atom(Tag) ->
    #'SecondRequestedActions'{keepActive        = KA,
			      signalsDescriptor = SD,
			      notifyBehaviour   = NB};
cre_SecondRequestedActions(KA, SD, 'NULL' = RED) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_list(SD) ->
    #'SecondRequestedActions'{keepActive            = KA,
			      signalsDescriptor     = SD,
			      resetEventsDescriptor = RED}.

cre_SecondRequestedActions(KA, {EDMTag, _} = EDM, SD, 
			   {NBTag, _} = NB, RED) 
  when ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       (is_atom(EDMTag) or (EDM == asn1_NOVALUE)) and
       (is_list(SD) or (SD == asn1_NOVALUE)) and 
       (is_atom(NBTag) or (NB == asn1_NOVALUE)) and
       ((RED == 'NULL') or (RED == asn1_NOVALUE)) ->
    #'SecondRequestedActions'{keepActive            = KA,
			      eventDM               = EDM, 
			      signalsDescriptor     = SD,
			      notifyBehaviour       = NB,
			      resetEventsDescriptor = RED}.

cre_EventBufferDescriptor([H|_] = D) when is_record(H, 'EventSpec') ->
    D.

cre_EventSpec(N, [H|_] = EPL) 
  when is_list(N) and is_record(H, 'EventParameter') ->
    #'EventSpec'{eventName = N, eventParList = EPL}.

cre_EventSpec(N, SID, [H|_] = EPL) 
  when is_list(N) and is_integer(SID) and is_record(H, 'EventParameter') ->
    #'EventSpec'{eventName = N, streamID = SID, eventParList = EPL}.
    
cre_SignalsDescriptor(D) ->
    case is_SignalsDescriptor(D) of
	true ->
	    D;
	false ->
	    error({invalid_SignalsDescriptor, D})
    end.

cre_SignalRequest(S) when is_record(S, 'Signal') ->
    {signal, S};
cre_SignalRequest(S) when is_record(S, 'SeqSigList') ->
    {seqSigList, S}.

cre_SeqSigList(ID, [H|_] = SL) 
  when is_integer(ID) and (0 =< ID) and (ID =< 65535) and
       is_record(H, 'Signal') ->
    #'SeqSigList'{id = ID, signalList = SL}.

cre_Signal(N) when is_list(N) ->
    #'Signal'{signalName = N}.

cre_Signal(N, SPL) when is_list(N) and is_list(SPL) ->
    #'Signal'{signalName = N,
	      sigParList = SPL}.

cre_Signal(N, SID, ST, Dur, NC, KA, SPL) 
  when is_list(N) and 
       (is_integer(SID) or (SID == asn1_NOVALUE)) and 
       ((ST == brief) or (ST == onOff) or (ST == timeOut) or 
	(ST == asn1_NOVALUE)) and
       ((is_integer(Dur) and (0 =< Dur) and (Dur =< 65535)) or 
	(Dur == asn1_NOVALUE)) and
       (is_list(NC) or (NC == asn1_NOVALUE)) and
       ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_list(SPL) ->
    #'Signal'{signalName       = N,
	      streamID         = SID,
	      sigType          = ST,
	      duration         = Dur,
	      notifyCompletion = NC,
	      keepActive       = KA,
	      sigParList       = SPL}.

cre_Signal(N, SID, ST, Dur, NC, KA, SPL, Dir, RID) 
  when is_list(N) and 
       (is_integer(SID) or (SID == asn1_NOVALUE)) and 
       ((ST == brief) or (ST == onOff) or (ST == timeOut) or 
	(ST == asn1_NOVALUE)) and
       ((is_integer(Dur) and (0 =< Dur) and (Dur =< 65535)) or 
	(Dur == asn1_NOVALUE)) and
       (is_list(NC) or (NC == asn1_NOVALUE)) and
       ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_list(SPL) and
       ((Dir == internal) or (Dir == external) or (Dir == both) or 
	(Dir == asn1_NOVALUE)) and
       (is_integer(RID) or (RID == asn1_NOVALUE)) ->
    #'Signal'{signalName       = N,
	      streamID         = SID,
	      sigType          = ST,
	      duration         = Dur,
	      notifyCompletion = NC,
	      keepActive       = KA,
	      sigParList       = SPL,
	      direction        = Dir,
	      requestID        = RID}.

cre_Signal(N, SID, ST, Dur, NC, KA, SPL, Dir, RID, ISIG) 
  when is_list(N) and 
       (is_integer(SID) or (SID == asn1_NOVALUE)) and 
       ((ST == brief) or (ST == onOff) or (ST == timeOut) or 
	(ST == asn1_NOVALUE)) and
       ((is_integer(Dur) and (0 =< Dur) and (Dur =< 65535)) or 
	(Dur == asn1_NOVALUE)) and
       (is_list(NC) or (NC == asn1_NOVALUE)) and
       ((KA == true) or (KA == false) or (KA == asn1_NOVALUE)) and
       is_list(SPL) and
       ((Dir == internal) or (Dir == external) or (Dir == both) or 
	(Dir == asn1_NOVALUE)) and
       (is_integer(RID) or (RID == asn1_NOVALUE)) and
       (is_integer(ISIG) or (ISIG == asn1_NOVALUE)) ->
    #'Signal'{signalName       = N,
	      streamID         = SID,
	      sigType          = ST,
	      duration         = Dur,
	      notifyCompletion = NC,
	      keepActive       = KA,
	      sigParList       = SPL,
	      direction        = Dir,
	      requestID        = RID,
	      intersigDelay    = ISIG}.

cre_SignalType(brief = ST) ->
    ST;
cre_SignalType(onOff = ST) ->
    ST;
cre_SignalType(timeOut = ST) ->
    ST.

cre_SignalDirection(internal = SD) ->
    SD;
cre_SignalDirection(external = SD) ->
    SD;
cre_SignalDirection(both = SD) ->
    SD.

cre_SignalName(N) ->
    cre_PkgdName(N).

cre_NotifyCompletion(L) when is_list(L) ->
    Vals = [onTimeOut, onInterruptByEvent, 
	    onInterruptByNewSignalDescr, otherReason],
    F = fun(E) -> case lists:member(E, Vals) of
		      true ->
			  ok;
		      false ->
			  exit({invalid_NotifyCompletion, E})
		  end
	end,
    lists:foreach(F, L),
    L.

cre_SigParameter(N, V) when is_list(N) and is_list(V) ->
    #'SigParameter'{sigParameterName = N, value = V}.

cre_SigParameter(N, V, relation = Tag, R) 
  when is_list(N) and is_list(V) and is_atom(R) ->
    EI = {Tag, R}, 
    #'SigParameter'{sigParameterName = N, value = V, extraInfo = EI};
cre_SigParameter(N, V, range = Tag, B) 
  when is_list(N) and is_list(V) and is_atom(B) ->
    EI = {Tag, B}, 
    #'SigParameter'{sigParameterName = N, value = V, extraInfo = EI};
cre_SigParameter(N, V, sublist = Tag, B) 
  when is_list(N) and is_list(V) and is_atom(B) ->
    EI = {Tag, B}, 
    #'SigParameter'{sigParameterName = N, value = V, extraInfo = EI}.

cre_RequestID(Val) when (0 =< Val) and (Val =< 4294967295) ->
    Val;
cre_RequestID(Val) ->
    exit({invalid_RequestID, Val}).

cre_ModemDescriptor(MTL, MPL) when is_list(MTL) and is_list(MPL) ->
    #'ModemDescriptor'{mtl = MTL, mpl = MPL}.

%% cre_ModemDescriptor(MTL, MPL, NSD) 
%%   when list(MTL), list(MPL), record(NSD, 'NonStandardData') ->
%%     #'ModemDescriptor'{mtl = MTL, mpl = MPL}.

cre_ModemType(v18 = MT) ->
    MT;
cre_ModemType(v22 = MT) ->
    MT;
cre_ModemType(v22bis = MT) ->
    MT;
cre_ModemType(v32 = MT) ->
    MT;
cre_ModemType(v32bis = MT) ->
    MT;
cre_ModemType(v34 = MT) ->
    MT;
cre_ModemType(v90 = MT) ->
    MT;
cre_ModemType(v91 = MT) ->
    MT;
cre_ModemType(synchISDN = MT) ->
    MT.

cre_DigitMapDescriptor() ->
    #'DigitMapDescriptor'{}.

cre_DigitMapDescriptor(N) when is_list(N) ->
    #'DigitMapDescriptor'{digitMapName = N};
cre_DigitMapDescriptor(V) when is_record(V, 'DigitMapValue') ->
    #'DigitMapDescriptor'{digitMapValue = V}.

cre_DigitMapDescriptor(N, V) 
  when is_list(N) and is_record(V, 'DigitMapValue') ->
    #'DigitMapDescriptor'{digitMapName = N, digitMapValue = V}.

cre_DigitMapName(N) ->
    cre_Name(N).

cre_DigitMapValue(DMB) when is_list(DMB) ->
    #'DigitMapValue'{digitMapBody = DMB}.

cre_DigitMapValue(Start, Short, Long, DMB) ->
    cre_DigitMapValue(Start, Short, Long, DMB, asn1_NOVALUE).

cre_DigitMapValue(Start, Short, Long, DMB, Dur) 
  when ((is_integer(Start) and (0 =< Start) and (Start =< 99)) or
	(Start == asn1_NOVALUE)) and
       ((is_integer(Short) and (0 =< Short) and (Short =< 99)) or
	(Short == asn1_NOVALUE)) and
       ((is_integer(Long) and (0 =< Long) and (Long =< 99)) or
	(Long == asn1_NOVALUE)) and
       is_list(DMB) and
       ((is_integer(Dur) and (0 =< Dur) and (Dur =< 99)) or
	(Dur == asn1_NOVALUE)) ->
    #'DigitMapValue'{startTimer    = Start,
		     shortTimer    = Short,
		     longTimer     = Long,
		     digitMapBody  = DMB,
		     durationTimer = Dur}.

cre_ServiceChangeParm(M, R) when is_atom(M) and is_list(R) ->
    #'ServiceChangeParm'{serviceChangeMethod = M,
			 serviceChangeReason = R}.

cre_ServiceChangeParm(M, Addr, Prof, Reason) ->
    cre_ServiceChangeParm(M, Addr, asn1_NOVALUE, Prof, Reason, asn1_NOVALUE, 
			  asn1_NOVALUE, asn1_NOVALUE, 
			  asn1_NOVALUE, asn1_NOVALUE).
				
%% Addr = asn1_NOVALUE | {AddrTag, AddrVal}
cre_ServiceChangeParm(M, Addr, Ver, Prof, R, D, Mid, TS, I) ->
    cre_ServiceChangeParm(M, Addr, Ver, Prof, R, D, Mid, TS, I, asn1_NOVALUE).
		
cre_ServiceChangeParm(M, Addr, Ver, Prof, R, D, Mid, TS, I, IF) 
  when is_atom(M) and 
       ((is_integer(Ver) and (0 =< Ver) and (Ver =< 99)) or
	(Ver == asn1_NOVALUE)) and
       (is_record(Prof, 'ServiceChangeProfile') or (Prof == asn1_NOVALUE)) and
       is_list(R) and
       ((is_integer(D) and (0 =< D) and (D =< 4294967295)) or 
	(D == asn1_NOVALUE)) and
       (is_record(TS, 'TimeNotation') or (TS == asn1_NOVALUE)) and
       (is_record(I, 'AuditDescriptor') or (I == asn1_NOVALUE)) and
       ((IF == 'NULL') or (IF == asn1_NOVALUE)) ->
    F = fun(A) -> 
		(A == asn1_NOVALUE) orelse 
				      (is_tuple(A) 
				       andalso is_atom(element(1, A))) 
	end,
    case (F(Addr) andalso F(Mid)) of
	true ->
	    #'ServiceChangeParm'{serviceChangeMethod         = M,
				 serviceChangeAddress        = Addr, 
				 serviceChangeVersion        = Ver, 
				 serviceChangeProfile        = Prof,
				 serviceChangeReason         = R,
				 serviceChangeDelay          = D,
				 serviceChangeMgcId          = Mid, 
				 timeStamp                   = TS, 
				 serviceChangeInfo           = I,
				 serviceChangeIncompleteFlag = IF};
	_ ->
	    exit({invalid_ServiceChangeParm_args, {Addr, Mid}})
    end.
		
cre_ServiceChangeAddress(portNumber = Tag, P) 
  when is_integer(P) and (0 =< P) and (P =< 65535) ->
    {Tag, P};
cre_ServiceChangeAddress(ip4Address = Tag, A) 
  when is_record(A, 'IP4Address') ->
    {Tag, A};
cre_ServiceChangeAddress(ip6Address = Tag, A) 
  when is_record(A, 'IP6Address') ->
    {Tag, A};
cre_ServiceChangeAddress(domainName = Tag, N) 
  when is_record(N, 'DomainName') ->
    {Tag, N};
cre_ServiceChangeAddress(deviceName = Tag, N) when is_list(N) ->
    {Tag, N};
cre_ServiceChangeAddress(mtpAddress = Tag, A) when is_list(A) ->
    {Tag, A}.

cre_ServiceChangeResParm() ->
    #'ServiceChangeResParm'{}.
cre_ServiceChangeResParm(Addr, Prof) ->
    cre_ServiceChangeResParm(asn1_NOVALUE, Addr, asn1_NOVALUE, 
			     Prof, asn1_NOVALUE).
cre_ServiceChangeResParm(Mid, Addr, Ver, Prof, TS)
  when ((is_integer(Ver) and (0 =< Ver) and (Ver =< 99)) or 
	(Ver == asn1_NOVALUE)) and 
       (is_record(Prof, 'ServiceChangeProfile') or (Prof == asn1_NOVALUE)) and
       (is_record(TS, 'TimeNotation') or (TS == asn1_NOVALUE)) ->
    F = fun(A) -> 
		(A == asn1_NOVALUE) orelse 
				      (is_tuple(A) 
				       andalso is_atom(element(1, A))) 
	end,
    case (F(Addr) andalso F(Mid)) of
	true ->
	    #'ServiceChangeResParm'{serviceChangeMgcId   = Mid, 
				    serviceChangeAddress = Addr, 
				    serviceChangeVersion = Ver, 
				    serviceChangeProfile = Prof,
				    timeStamp            = TS};
	_ ->
	    exit({invalid_ServiceChangeResParm_args, {Addr, Mid}})
    end.

cre_ServiceChangeMethod(failover = M) ->
    M;
cre_ServiceChangeMethod(forced = M) ->
    M;
cre_ServiceChangeMethod(graceful = M) ->
    M;
cre_ServiceChangeMethod(restart = M) ->
    M;
cre_ServiceChangeMethod(disconnected = M) ->
    M;
cre_ServiceChangeMethod(handOff = M) ->
    M.

%% The version field is added to make it look more like ABNF
cre_ServiceChangeProfile(N) ->
    cre_ServiceChangeProfile(N, 1).

cre_ServiceChangeProfile(N, V) 
  when is_list(N) and is_integer(V) and (0 =< V) and (V =< 99) ->
    #'ServiceChangeProfile'{profileName = N, version = V}.
    
cre_PackagesDescriptor([H|_] = D) when is_record(H, 'PackagesItem') ->
    D.

cre_PackagesItem(N, Ver) 
  when is_list(N) and 
       is_integer(Ver) and (0 =< Ver) and (Ver =< 99) ->
    #'PackagesItem'{packageName    = N, 
		    packageVersion = Ver}.

cre_StatisticsDescriptor(D) ->
    true = is_StatisticsDescriptor(D),
    D.

cre_StatisticsParameter(N) when is_list(N) ->
    #'StatisticsParameter'{statName = N}.

cre_StatisticsParameter(N, V) when is_list(N) and is_list(V) ->
    #'StatisticsParameter'{statName = N, statValue = V}.

%% cre_NonStandardData({Tag, _} = Id, Data) when atom(Tag), list(Data) ->
%%     #'NonStandardData'{nonStandardIdentifier = Id, data = Data}.

%% cre_NonStandardIdentifier(H221) when record(H221, 'H221NonStandard') ->
%%     {h221NonStandard, H221};
%% cre_NonStandardIdentifier(Obj) when tuple(Obj) ->
%%     {object, Obj};
%% cre_NonStandardIdentifier(Exp) when list(Exp), length(Exp) == 8 ->
%%     {experimental, Exp}.

%% cre_H221NonStandard(CC1, CC2, Ext, MC) 
%%   when (is_integer(CC1) and (0 =< CC1) and (CC1 =< 255)) and
%%        (is_integer(CC2) and (0 =< CC2) and (CC2 =< 255)) and
%%        (is_integer(Ext) and (0 =< Ext) and (Ext =< 255)) and
%%        (is_integer(MC)  and (0 =< MC)  and (MC =< 255)) ->
%%     #'H221NonStandard'{t35CountryCode1  = CC1, 
%% 		       t35CountryCode2  = CC2, 
%% 		       t35Extension     = Ext, 
%% 		       manufacturerCode = MC}.
       
cre_TimeNotation(D, T) 
  when is_list(D) and (length(D) == 8) and 
       is_list(T) and (length(T) == 8) ->
    #'TimeNotation'{date = D, time = T}.
    
cre_Value([H|_] = V) when is_list(H) ->
    V.

cre_BOOLEAN(true = B) ->
    B;
cre_BOOLEAN(false = B) ->
    B.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -- MegacoMessage -- 

is_MegacoMessage(#'MegacoMessage'{authHeader = Auth,
				  mess       = Mess}) ->
    d("is_MegacoMessage -> entry"),
    is_opt_AuthenticationHeader(Auth) andalso is_Message(Mess);
is_MegacoMessage(_) ->
    false.

    
chk_MegacoMessage(M, M) ->
    d("chk_MegacoMessage -> entry (1)"),
    chk_type(fun is_MegacoMessage/1, 'MegacoMessage', M);
chk_MegacoMessage(#'MegacoMessage'{authHeader = Auth1,
				   mess       = Mess1},
		  #'MegacoMessage'{authHeader = Auth2,
				   mess       = Mess2}) ->
    d("chk_MegacoMessage -> entry (2)"),
    chk_opt_AuthenticationHeader(Auth1,Auth2),
    chk_Message(Mess1,Mess2),
    ok;
chk_MegacoMessage(M1, M2) ->
    wrong_type('MegacoMessage', M1, M2).


%% -- AuthenticationHeader -- 

is_opt_AuthenticationHeader(AH) ->
    is_OPTIONAL(fun is_AuthenticationHeader/1, AH).

is_AuthenticationHeader(#'AuthenticationHeader'{secParmIndex = SPI, 
						seqNum       = SN, 
						ad           = AD}) ->
    is_SecurityParmIndex(SPI) andalso 
	is_SequenceNum(SN) andalso 
	is_AuthData(AD);
is_AuthenticationHeader(_) ->
    false.
    
%% This stuff is not really used, so make it simple...
chk_opt_AuthenticationHeader(A1, A2) ->
    chk_OPTIONAL('AuthenticationHeader', A1, A2, 
		 fun is_AuthenticationHeader/1, 
		 fun chk_AuthenticationHeader/2).

chk_AuthenticationHeader(A, A) ->
    chk_type(fun is_AuthenticationHeader/1, 'AuthenticationHeader', A);
chk_AuthenticationHeader(A1, A2) ->
    case (is_AuthenticationHeader(A1) andalso is_AuthenticationHeader(A2)) of
	true ->
	    not_equal('AuthenticationHeader', A1, A2);
	false ->
	    wrong_type('AuthenticationHeader', A1, A2)
    end.


%% -- SecurityParmIndex --

is_SecurityParmIndex(V) -> is_OCTET_STRING(V, {exact, 4}).


%% -- SequenceNum --

is_SequenceNum(V) -> is_OCTET_STRING(V, {exact, 4}).


%% -- AuthData --

is_AuthData(V) -> is_OCTET_STRING(V, {range, 12, 32}).


%% -- Message --

is_Message(#'Message'{version     = V,
		      mId         = MID,
		      messageBody = Body}) ->
    d("is_Message -> entry"),
    is_INTEGER(V, {range, 0, 99}) andalso 
	is_MId(MID) andalso 
	is_Message_messageBody(Body);
is_Message(_) ->
    false.

chk_Message(M, M) ->
    d("chk_Message -> entry (1)"),
    chk_type(fun is_Message/1, 'Message', M);
chk_Message(#'Message'{version     = V1,
		       mId         = MID1,
		       messageBody = Body1},
	    #'Message'{version     = V2,
		       mId         = MID2,
		       messageBody = Body2}) ->
    d("chk_Message -> entry with"
      "~n   V1:    ~p"
      "~n   V2:    ~p"
      "~n   MID1:  ~p"
      "~n   MID2:  ~p"
      "~n   Body1: ~p"
      "~n   Body2: ~p",
     [V1, V2, MID1, MID2, Body1, Body2]),
    validate(fun() -> chk_INTEGER(V1, V2, {range, 0, 99}) end, 
	     'Message_version'),
    validate(fun() -> chk_MId(MID1, MID2) end, 'Message_mId'),
    chk_Message_messageBody(Body1, Body2),
    ok;
chk_Message(M1, M2) ->
    wrong_type('Message', M1, M2).


is_Message_messageBody({Tag, Val}) ->
    d("is_Message_messageBody -> entry"),
    is_Message_messageBody_tag(Tag) andalso 
	is_Message_messageBody_val(Tag, Val);
is_Message_messageBody(_) ->
    false.

is_Message_messageBody_tag(Tag) ->
    Tags = [messageError, transactions],
    lists:member(Tag, Tags).
    
is_Message_messageBody_val(messageError, Val) ->
    is_ErrorDescriptor(Val);
is_Message_messageBody_val(transactions, Val) ->
    is_Message_messageBody_transactions(Val).

is_Message_messageBody_transactions([]) ->
    d("is_Message_messageBody_transactions -> entry when done"),
    true;
is_Message_messageBody_transactions([H|T]) ->
    d("is_Message_messageBody_transactions -> entry"),
    is_Transaction(H) andalso is_Message_messageBody_transactions(T);
is_Message_messageBody_transactions(_) ->
    false.

chk_Message_messageBody(B, B) ->
    d("chk_Message_messageBody -> entry (1)"),
    chk_type(fun is_Message_messageBody/1, 'Message_messageBody', B);
chk_Message_messageBody({Tag, Val1} = B1, {Tag, Val2} = B2) ->
    d("chk_Message_messageBody -> entry (2)"),
    case (is_Message_messageBody_tag(Tag) andalso 
	  is_Message_messageBody_val(Tag, Val1) andalso 
	  is_Message_messageBody_val(Tag, Val2)) of
	true ->
	    chk_Message_messageBody_val(Tag, Val1, Val2);
	false ->
	    wrong_type('Message_messageBody', B1, B2)
    end;
chk_Message_messageBody({Tag1, Val1} = B1, {Tag2, Val2} = B2) ->
    d("chk_Message_messageBody -> entry (3)"),
    case ((is_Message_messageBody_tag(Tag1) andalso 
	   is_Message_messageBody_val(Tag1, Val1)) andalso 
	  (is_Message_messageBody_tag(Tag2) andalso 
	   is_Message_messageBody_val(Tag2, Val2))) of
	true ->
	    not_equal('Message_messageBody', B1, B2);
	false ->
	    wrong_type('Message_messageBody', B1, B2)
    end;
chk_Message_messageBody(B1, B2) ->
    wrong_type('Message_messageBody', B1, B2).

chk_Message_messageBody_val(messageError, Val1, Val2) ->
    validate(fun() -> chk_ErrorDescriptor(Val1, Val2) end, 
	     'Message_messageBody');
chk_Message_messageBody_val(transactions, Val1, Val2) ->
    chk_Message_messageBody_transactions(lists:sort(Val1), 
					 lists:sort(Val2)).

chk_Message_messageBody_transactions([], []) ->
    d("chk_Message_messageBody_transactions -> entry - ok (1)"),
    ok;
chk_Message_messageBody_transactions([] = T1, T2) ->
    d("chk_Message_messageBody_transactions -> entry - not-equal (2)"),
    not_equal('Message_messageBody_transactions', T1, T2);
chk_Message_messageBody_transactions(T1, [] = T2) ->
    d("chk_Message_messageBody_transactions -> entry - not-equal (3)"),
    not_equal('Message_messageBody_transactions', T1, T2);
chk_Message_messageBody_transactions([H|T1], [H|T2]) ->
    d("chk_Message_messageBody_transactions -> entry (4)"),
    case is_Transaction(H) of
	true ->
	    chk_Message_messageBody_transactions(T1, T2);
	false ->
	    wrong_type('Message_messageBody_transactions_val', H)
    end;
chk_Message_messageBody_transactions([H1|T1], [H2|T2]) ->
    d("chk_Message_messageBody_transactions -> entry (5)"),
    validate(fun() -> chk_Transaction(H1, H2) end, 
	     'Message_messageBody_transactions_val'),
    chk_Message_messageBody_transactions(T1, T2);
chk_Message_messageBody_transactions(T1, T2) ->
    d("chk_Message_messageBody_transactions -> entry - wrong-type (6)"),
    wrong_type('Message_messageBody_transactions', T1, T2).
 

%% -- MId --

is_opt_MId(M) ->
    is_OPTIONAL(fun is_MId/1, M).

is_MId({Tag, Val}) ->
    is_MId_tag(Tag) andalso is_MId_val(Tag, Val);
is_MId(_) ->
    false.

is_MId_tag(Tag) ->
    Tags = [ip4Address, ip6Address, domainName, deviceName, mtpAddress], 
    lists:member(Tag, Tags).

is_MId_val(ip4Address, Val) -> is_IP4Address(Val);
is_MId_val(ip6Address, Val) -> is_IP6Address(Val);
is_MId_val(domainName, Val) -> is_DomainName(Val);
is_MId_val(deviceName, Val) -> is_PathName(Val);
is_MId_val(mtpAddress, Val) -> is_OCTET_STRING(Val, {range, 2, 4}).

chk_opt_MId(M1, M2) ->
    chk_OPTIONAL('MId', M1, M2, fun is_MId/1, fun chk_MId/2).

chk_MId(M, M) ->
    chk_type(fun is_MId/1, 'MId', M);
chk_MId({Tag, Val1} = M1, {Tag, Val2} = M2) ->
    case (is_MId_tag(Tag) andalso
	  is_MId_val(Tag, Val1) andalso
	  is_MId_val(Tag, Val2)) of
	true ->
	    chk_MId_val(Tag, Val1, Val2);
	false ->
	    wrong_type('MId', M1, M2)
    end;
chk_MId({Tag1, Val1} = M1, {Tag2, Val2} = M2) ->
    case ((is_MId_tag(Tag1) andalso
	   is_MId_val(Tag1, Val1)) andalso
	  (is_MId_tag(Tag2) andalso
	   is_MId_val(Tag2, Val2))) of
	true ->
	    not_equal('MId', M1, M2);
	false ->
	    wrong_type('MId', M1, M2)
    end;
chk_MId(M1, M2) ->
    wrong_type('MId', M1, M2).

chk_MId_val(ip4Address, M1, M2) -> chk_IP4Address(M1, M2);
chk_MId_val(ip6Address, M1, M2) -> chk_IP6Address(M1, M2);
chk_MId_val(domainName, M1, M2) -> chk_DomainName(M1, M2);
chk_MId_val(deviceName, M1, M2) -> chk_PathName(M1,   M2);
chk_MId_val(mtpAddress, M1, M2) -> chk_OCTET_STRING(M1, M2, {range, 2, 4}).


%% -- DomainName --

is_DomainName(#'DomainName'{name = N, portNumber = PN}) ->
    is_IA5String(N) andalso is_opt_INTEGER(PN, {range, 0, 65535});
is_DomainName(_) ->
    false.

chk_DomainName(N, N) ->
    ok;
chk_DomainName(N1, N2) ->
    not_equal('DomainName', N1, N2).


%% -- IP4Address --

is_IP4Address(#'IP4Address'{address = A, portNumber = PN}) ->
    is_OCTET_STRING(A, {exact, 4}) andalso 
	is_opt_INTEGER(PN, {range, 0, 65535});
is_IP4Address(_) ->
    false.

chk_IP4Address(A, A) ->
    ok;
chk_IP4Address(A1, A2) ->
    not_equal('IP4Address', A1, A2).


%% -- IP6Address --

is_IP6Address(#'IP6Address'{address = A, portNumber = PN}) ->
    is_OCTET_STRING(A, {exact, 16}) andalso 
	is_opt_INTEGER(PN, {range, 0, 65535});
is_IP6Address(_) ->
    false.

chk_IP6Address(A, A) ->
    ok;
chk_IP6Address(A1, A2) ->
    not_equal('IP6Address', A1, A2).


%% -- PathName --

is_PathName(N) -> is_IA5String(N, {range, 1, 64}).
    
chk_PathName(N, N) ->
    ok;
chk_PathName(N1, N2) ->
    not_equal('PathName', N1, N2).


%% -- Transaction --

is_Transaction({Tag, Val}) ->
    d("is_Transaction -> entry"),
    is_Transaction_tag(Tag) andalso is_Transaction_val(Tag, Val);
is_Transaction(_) ->
    false.

is_Transaction_tag(Tag) ->
    Tags = [transactionRequest,
	    transactionPending,
	    transactionReply,
	    transactionResponseAck],
    lists:member(Tag, Tags).

is_Transaction_val(transactionRequest, V)     -> is_TransactionRequest(V);
is_Transaction_val(transactionPending, V)     -> is_TransactionPending(V);
is_Transaction_val(transactionReply, V)       -> is_TransactionReply(V);
is_Transaction_val(transactionResponseAck, V) -> is_TransactionResponseAck(V).
    

chk_Transaction({Tag, Val} = Trans, Trans) ->
    d("chk_Transaction -> entry (1)"),
    case (is_Transaction_tag(Tag) andalso is_Transaction_val(Tag, Val)) of
	true ->
	    ok;
	false ->
	    wrong_type('Transaction', Trans, Trans)
    end;
chk_Transaction({Tag, Val1} = Trans1, {Tag, Val2} = Trans2) ->
    d("chk_Transaction -> entry (2)"),
    case (is_Transaction_tag(Tag) and
	  is_Transaction_val(Tag, Val1) and
	  is_Transaction_val(Tag, Val2)) of
	true ->
	    chk_Transaction_val(Tag, Val1, Val2);
	false ->
	    wrong_type('Transaction', Trans1, Trans2)
    end;
chk_Transaction({Tag1, Val1} = Trans1, {Tag2, Val2} = Trans2) ->
    d("chk_Transaction -> entry (3)"),
    case ((is_Transaction_tag(Tag1) andalso 
	   is_Transaction_val(Tag1, Val1)) andalso
	  (is_Transaction_tag(Tag2) andalso 
	   is_Transaction_val(Tag2, Val2))) of
	true ->
	    not_equal('Transaction', Trans1, Trans2);
	false ->
	    wrong_type('Transaction', Trans1, Trans2)
    end;
chk_Transaction(Trans1, Trans2) ->
    d("chk_Transaction -> entry - wrong-type - (4)"),
    wrong_type('Transaction', Trans1, Trans2).

chk_Transaction_val(transactionRequest, T1, T2) ->
    chk_TransactionRequest(T1, T2);
chk_Transaction_val(transactionPending, T1, T2) ->
    chk_TransactionPending(T1, T2);
chk_Transaction_val(transactionReply, T1, T2) ->
    chk_TransactionReply(T1,T2);
chk_Transaction_val(transactionResponseAck, T1, T2) ->
    chk_TransactionResponseAck(T1, T2).
 

%% -- TransactionId --

is_opt_TransactionId(TID) -> 
    is_OPTIONAL(fun is_TransactionId/1, TID).

is_TransactionId(TID) -> 
    d("is_TransactionId -> entry"),
    is_INTEGER(TID, {range, 0, 4294967295}).

chk_opt_TransactionId(TID1, TID2) ->
    chk_OPTIONAL('TransactionId', TID1, TID2, 
		 fun is_TransactionId/1, fun chk_TransactionId/2).

chk_TransactionId(TID, TID) ->
    chk_type(fun is_TransactionId/1, 'TransactionId', TID);
chk_TransactionId(TID1, TID2) ->
    case (is_TransactionId(TID1) andalso is_TransactionId(TID2)) of
	true ->
	    not_equal('TransactionId', TID1, TID2);
	false ->
	    wrong_type('TransactionId', TID1, TID2)
    end.


%% -- TransactionRequest --

is_TransactionRequest(#'TransactionRequest'{transactionId = TID,
					    actions       = Acts}) ->
    d("is_TransactionRequest -> entry"),
    is_TransactionId(TID) andalso is_TransactionRequest_actions(Acts);
is_TransactionRequest(_) ->
    false.

chk_TransactionRequest(T, T) ->
    chk_type(fun is_TransactionRequest/1, 'TransactionRequest', T);
chk_TransactionRequest(#'TransactionRequest'{transactionId = TID1,
					     actions       = Acts1}, 
		       #'TransactionRequest'{transactionId = TID2,
					     actions       = Acts2}) ->
    validate(fun() -> chk_TransactionId(TID1, TID2) end, 'TransactionRequest'),
    chk_TransactionRequest_actions(lists:sort(Acts1), 
				   lists:sort(Acts2)),
    ok;
chk_TransactionRequest(T1, T2) ->
    wrong_type('TransactionRequest', T1, T2).

is_TransactionRequest_actions([]) ->
    d("is_TransactionRequest_actions -> entry when done"),
    true;
is_TransactionRequest_actions([H|T]) ->
    d("is_TransactionRequest_actions -> entry"),
    is_ActionRequest(H) andalso is_TransactionRequest_actions(T);
is_TransactionRequest_actions(_) ->
    false.

chk_TransactionRequest_actions([], []) ->
    ok;
chk_TransactionRequest_actions([] = Acts1, Acts2) ->
    not_equal('TransactionRequest_actions', Acts1, Acts2);
chk_TransactionRequest_actions(Acts1, [] = Acts2) ->
    not_equal('TransactionRequest_actions', Acts1, Acts2);
chk_TransactionRequest_actions([H|T1], [H|T2]) ->
    case is_ActionRequest(H) of
	true ->
	    chk_TransactionRequest_actions(T1, T2);
	false ->
	    wrong_type('TransactionRequest_actions_val', H)
    end;
chk_TransactionRequest_actions([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_ActionRequest(H1, H2) end, 
	     'TransactionRequest_actions_val'),
    chk_TransactionRequest_actions(T1, T2);
chk_TransactionRequest_actions(Acts1, Acts2) ->
    wrong_type('TransactionRequest_actions', Acts1, Acts2).


%% -- TransactionPending --

is_TransactionPending(#'TransactionPending'{transactionId = TID}) ->
    d("is_TransactionPending -> entry"),
    is_TransactionId(TID);
is_TransactionPending(_) ->
    false.

chk_TransactionPending(T, T) ->
    chk_type(fun is_TransactionPending/1, 'TransactionPending', T);
chk_TransactionPending(#'TransactionPending'{transactionId = TID1}, 
		       #'TransactionPending'{transactionId = TID2}) ->
    validate(fun() -> chk_TransactionId(TID1, TID2) end, 'TransactionPending'),
    ok;
chk_TransactionPending(T1, T2) ->
    wrong_type('TransactionPending', T1, T2).

    
%% -- TransactionReply --

is_TransactionReply(#'TransactionReply'{transactionId     = TID,
					immAckRequired    = IAR,
					transactionResult = TR}) ->
    d("is_TransactionReply -> entry"),
    is_TransactionId(TID) andalso 
	is_opt_NULL(IAR) andalso 
	is_TransactionReply_transactionResult(TR);
is_TransactionReply(_) ->
    false.

chk_TransactionReply(T, T) ->
    d("chk_TransactionReply -> entry (2)"),
    chk_type(fun is_TransactionReply/1, 'TransactionReply', T);
chk_TransactionReply(#'TransactionReply'{transactionId     = TID1,
					 immAckRequired    = IAR1,
					 transactionResult = TR1},
		     #'TransactionReply'{transactionId     = TID2,
					 immAckRequired    = IAR2,
					 transactionResult = TR2}) ->
    d("chk_TransactionReply -> entry (2)"),
    validate(fun() -> chk_TransactionId(TID1, TID2) end, 'TransactionReply'),
    validate(fun() -> chk_opt_NULL(IAR1, IAR2) end, 'TransactionReply'),
    chk_TransactionReply_transactionResult(TR1, TR2),
    ok;
chk_TransactionReply(T1, T2) ->
    d("chk_TransactionReply -> entry (3)"),
    wrong_type('TransactionReply', T1, T2).
	
is_TransactionReply_transactionResult({Tag, Val}) ->
    d("is_TransactionReply_transactionResult -> entry"),
    is_TransactionReply_transactionResult_tag(Tag) andalso
	is_TransactionReply_transactionResult_val(Tag, Val);
is_TransactionReply_transactionResult(_) ->
    false.

is_TransactionReply_transactionResult_tag(T) ->
    lists:member(T, [transactionError, actionReplies]).

is_TransactionReply_transactionResult_val(transactionError, V) ->
    is_ErrorDescriptor(V);
is_TransactionReply_transactionResult_val(actionReplies, V) ->
    is_TransactionReply_actionReplies(V).

chk_TransactionReply_transactionResult(Res, Res) ->
    chk_type(fun is_TransactionReply_transactionResult/1, 
	     'TransactionReply_transactionResult', Res);
chk_TransactionReply_transactionResult({Tag, Val1} = Res1, 
				       {Tag, Val2} = Res2) ->
    case (is_TransactionReply_transactionResult_tag(Tag) and
	  is_TransactionReply_transactionResult_val(Tag, Val1) and
	  is_TransactionReply_transactionResult_val(Tag, Val2)) of
	true ->
	    chk_TransactionReply_transactionResult_val(Tag, Val1, Val2);
	false ->
	    wrong_type('TransactionReply_transactionResult', Res1, Res2)
    end;
chk_TransactionReply_transactionResult({Tag1, Val1} = Res1, 
				       {Tag2, Val2} = Res2) ->
    case ((is_TransactionReply_transactionResult_tag(Tag1) and
	   is_TransactionReply_transactionResult_val(Tag1, Val1)) and
	  (is_TransactionReply_transactionResult_tag(Tag2) and
	   is_TransactionReply_transactionResult_val(Tag2, Val2))) of
	true ->
	    not_equal('TransactionReply_transactionResult', Res1, Res2);
	false ->
	    wrong_type('TransactionReply_transactionResult', Res1, Res2)
    end;
chk_TransactionReply_transactionResult(Res1, Res2) ->
    wrong_type('TransactionReply_transactionResult', Res1, Res2).

chk_TransactionReply_transactionResult_val(transactionError, E1, E2) ->
    validate(fun() -> chk_ErrorDescriptor(E1, E2) end, 
	     'TransactionReply_transactionResult');
chk_TransactionReply_transactionResult_val(actionReplies, R1, R2) ->
    validate(fun() -> 
		     chk_TransactionReply_actionReplies(lists:sort(R1), 
							lists:sort(R2)) 
	     end, 
	     'TransactionReply_transactionResult').

is_TransactionReply_actionReplies([]) ->
    d("is_TransactionReply_actionReplies -> entry when done"),
    true;
is_TransactionReply_actionReplies([H|T]) ->
    d("is_TransactionReply_actionReplies -> entry"),
    is_ActionReply(H) andalso is_TransactionReply_actionReplies(T);
is_TransactionReply_actionReplies(_) ->
    false.

chk_TransactionReply_actionReplies([], []) ->
    ok;
chk_TransactionReply_actionReplies([] = AR1, AR2) ->
    not_equal('TransactionReply_actionReplies', AR1, AR2);
chk_TransactionReply_actionReplies(AR1, [] = AR2) ->
    not_equal('TransactionReply_actionReplies', AR1, AR2);
chk_TransactionReply_actionReplies([H|T1], [H|T2]) ->
    case is_ActionReply(H) of
	true ->
	    chk_TransactionReply_actionReplies(T1, T2);
	false ->
	    wrong_type('TransactionReply_actionReplies_val', H)
    end;
chk_TransactionReply_actionReplies([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_ActionReply(H1, H2) end,
	     'TransactionReply_actionReplies_val'),
    chk_TransactionReply_actionReplies(T1, T2);
chk_TransactionReply_actionReplies(AR1, AR2) ->
    wrong_type('TransactionReply_actionReplies', AR1, AR2).

		     
%% -- TransactionResponseAck -- 

is_TransactionResponseAck([]) ->
    d("is_TransactionResponseAck -> entry when done"),
    true;
is_TransactionResponseAck([H|T]) ->
    d("is_TransactionResponseAck -> entry"),
    is_TransactionAck(H) andalso is_TransactionResponseAck(T);
is_TransactionResponseAck(_) ->
    false.

chk_TransactionResponseAck([], []) ->
    ok;
chk_TransactionResponseAck([] = AR1, AR2) ->
    not_equal('TransactionResponseAck', AR1, AR2);
chk_TransactionResponseAck(AR1, [] = AR2) ->
    not_equal('TransactionResponseAck', AR1, AR2);
chk_TransactionResponseAck([H|T1], [H|T2]) ->
    case is_TransactionAck(H) of
	true ->
	    chk_TransactionResponseAck(T1, T2);
	false ->
	    wrong_type('TransactionResponseAck_val', H)
    end;
chk_TransactionResponseAck([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_TransactionAck(H1, H2) end, 
	     'TransactionResponseAck'),
    chk_TransactionResponseAck(T1, T2);
chk_TransactionResponseAck(AR1, AR2) ->
    wrong_type('TransactionResponseAck', AR1, AR2).


%% -- TransactionAck --

is_TransactionAck(#'TransactionAck'{firstAck = F,
				    lastAck  = L}) ->
    d("is_TransactionAck -> entry"),
    is_TransactionId(F) andalso is_opt_TransactionId(L);
is_TransactionAck(_) ->
    false.

chk_TransactionAck(T, T) ->
    chk_type(fun is_TransactionAck/1, 'TransactionAck', T);
chk_TransactionAck(#'TransactionAck'{firstAck = F1,
				     lastAck  = L1},
		   #'TransactionAck'{firstAck = F2,
				     lastAck  = L2}) ->
    validate(fun() -> chk_TransactionId(F1, F2) end, 'TransactionAck'),
    validate(fun() -> chk_opt_TransactionId(L1, L2) end, 'TransactionAck'),
    ok;
chk_TransactionAck(T1, T2) ->
    wrong_type('TransactionAck', T1, T2).


%% -- ErrorDescriptor -- 

is_opt_ErrorDescriptor(V) ->
    is_OPTIONAL(fun is_ErrorDescriptor/1, V).

is_ErrorDescriptor(#'ErrorDescriptor'{errorCode = Code,
				      errorText = Text}) ->
    d("is_ErrorDescriptor -> entry"),
    is_ErrorCode(Code) andalso is_opt_ErrorText(Text);
is_ErrorDescriptor(_) ->
    false.

chk_opt_ErrorDescriptor(E1, E2) ->
    chk_OPTIONAL('ErrorDescriptor', E1, E2, 
		 fun is_ErrorDescriptor/1, fun chk_ErrorDescriptor/2).
 
chk_ErrorDescriptor(E, E) ->
    chk_type(fun is_ErrorDescriptor/1, 'ErrorDescriptor', E);
chk_ErrorDescriptor(#'ErrorDescriptor'{errorCode = Code1,
                                       errorText = Text1},
                    #'ErrorDescriptor'{errorCode = Code2,
                                       errorText = Text2}) ->
    chk_ErrorCode(Code1, Code2),
    chk_opt_ErrorText(Text1, Text2),
    ok;
chk_ErrorDescriptor(E1, E2) ->
    wrong_type('ErrorDescriptor', E1, E2).
 

%% -- ErrorCode --

is_ErrorCode(C) -> is_INTEGER(C, {range, 0, 65535}).

chk_ErrorCode(C, C) ->
    case is_ErrorCode(C) of
	true ->
	    ok;
	false ->
	    wrong_type(errorCode, C, C)
    end;
chk_ErrorCode(C1, C2) ->
    case (is_ErrorCode(C1) andalso is_ErrorCode(C2)) of
	true ->
	    not_equal(errorCode, C1, C2);
	false ->
	    wrong_type(errorCode, C1, C2)
    end.
 

%% -- ErrorText --

is_opt_ErrorText(V) ->
    is_OPTIONAL(fun is_ErrorText/1, V).

is_ErrorText(V) -> is_IA5String(V).

chk_opt_ErrorText(T1, T2) ->
    chk_OPTIONAL('ErrorText', T1, T2, fun is_ErrorText/1, fun chk_ErrorText/2).

chk_ErrorText(T, T) ->
    chk_type(fun is_ErrorText/1, 'ErrorText', T);
chk_ErrorText(T1, T2) ->
    case (is_ErrorText(T1) andalso is_ErrorText(T2)) of
	true ->
	    case {to_lower(T1), to_lower(T2)} of
		{T, T} ->
		    ok;
		_ ->
		    not_equal('ErrorText', T1, T2)
	    end;
	false ->
	    wrong_type('ErrorText', T1, T2)
    end.
 

%% -- ContextID --

is_ContextID(Id) -> is_INTEGER(Id, {range, 0, 4294967295}).

chk_ContextID(Id, Id) ->
    chk_type(fun is_ContextID/1, 'ContextID', Id);
chk_ContextID(Id1, Id2) ->
    case (is_ContextID(Id1) andalso is_ContextID(Id2)) of
	true ->
	    not_equal('ContextID', Id1, Id2);
	false ->
	    wrong_type('ContextID', Id1, Id2)
    end.


%% -- ActionRequest --

is_ActionRequest(#'ActionRequest'{contextId           = Id,
				  contextRequest      = CtxReq,
				  contextAttrAuditReq = AuditReq,
				  commandRequests     = CmdReqs}) ->
    d("is_ActionRequest -> entry"),
    is_ContextID(Id) andalso 
	is_opt_ContextRequest(CtxReq) andalso
	is_opt_ContextAttrAuditRequest(AuditReq) andalso
	is_ActionRequest_commandRequests(CmdReqs);
is_ActionRequest(_) ->
    false.

chk_ActionRequest(A, A) ->
    chk_type(fun is_ActionRequest/1, 'ActionRequest', A);
chk_ActionRequest(#'ActionRequest'{contextId           = Id1,
                                   contextRequest      = Req1,
                                   contextAttrAuditReq = AuditReq1,
                                   commandRequests     = CmdReqs1},
                  #'ActionRequest'{contextId           = Id2,
                                   contextRequest      = Req2,
                                   contextAttrAuditReq = AuditReq2,
                                   commandRequests     = CmdReqs2}) ->
    validate(fun() -> chk_ContextID(Id1, Id2) end, 'ActionRequest'),
    validate(fun() -> chk_opt_ContextRequest(Req1, Req2) end, 'ActionRequest'),
    validate(fun() -> 
		     chk_opt_ContextAttrAuditRequest(AuditReq1, AuditReq2) 
	     end,
	     'ActionRequest'),
    chk_ActionRequest_commandRequests(CmdReqs1, CmdReqs2),
    ok.


is_ActionRequest_commandRequests([]) ->
    d("is_ActionRequest_commandRequests -> entry when done"),
    true;
is_ActionRequest_commandRequests([H|T]) ->
    d("is_ActionRequest_commandRequests -> entry"),
    is_CommandRequest(H) andalso is_ActionRequest_commandRequests(T);
is_ActionRequest_commandRequests(_) ->
    false.

chk_ActionRequest_commandRequests([], []) ->
    ok;
chk_ActionRequest_commandRequests([] = CmdReqs1, CmdReqs2) ->
    not_equal('ActionRequest_commandRequests', CmdReqs1, CmdReqs2);
chk_ActionRequest_commandRequests(CmdReqs1, [] = CmdReqs2) ->
    not_equal('ActionRequest_commandRequests', CmdReqs1, CmdReqs2);
chk_ActionRequest_commandRequests([H|T1], [H|T2]) ->
    case is_CommandRequest(H) of
	true ->
	    chk_ActionRequest_commandRequests(T1, T2);
	false ->
	    wrong_type('ActionRequest_commandRequest_val', H)
    end;
chk_ActionRequest_commandRequests([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_CommandRequest(H1, H2) end,
	     'ActionRequest_commandRequests_val'),
    chk_ActionRequest_commandRequests(T1, T2);
chk_ActionRequest_commandRequests(R1, R2) ->
    wrong_type('ActionRequest_commandRequests', R1, R2).
    

%% -- ActionReply --

is_ActionReply(#'ActionReply'{contextId       = Id,
			      errorDescriptor = ED,
			      contextReply    = CtxRep,
			      commandReply    = CmdRep}) ->
    d("is_ActionReply -> entry"),
    is_ContextID(Id) andalso 
	is_opt_ErrorDescriptor(ED) andalso
	is_opt_ContextRequest(CtxRep) andalso
	is_ActionReply_commandReply(CmdRep);
is_ActionReply(_) ->
    false.

is_ActionReply_commandReply([]) ->
    d("is_ActionReply_commandReply -> entry when done"),
    true;
is_ActionReply_commandReply([H|T]) ->
    d("is_ActionReply_commandReply -> entry"),
    is_CommandReply(H) andalso is_ActionReply_commandReply(T);
is_ActionReply_commandReply(_) ->
    false.

chk_ActionReply(A, A) ->
    d("chk_ActionReply -> entry (1)"),
    chk_type(fun is_ActionReply/1, 'ActionReply', A);
chk_ActionReply(#'ActionReply'{contextId       = Id1,
			       errorDescriptor = ED1,
			       contextReply    = CtxRep1,
			       commandReply    = CmdRep1},
		#'ActionReply'{contextId       = Id2,
			       errorDescriptor = ED2,
			       contextReply    = CtxRep2,
			       commandReply    = CmdRep2}) ->
    d("chk_ActionReply -> entry (2)"),
    chk_ContextID(Id1, Id2),
    chk_opt_ErrorDescriptor(ED1, ED2),
    chk_opt_ContextRequest(CtxRep1, CtxRep2),
    chk_ActionReply_commandReply(CmdRep1, CmdRep2).

chk_ActionReply_commandReply([], []) ->
    ok;
chk_ActionReply_commandReply([] = Reps1, Reps2) ->
    not_equal('ActionReply_commandReply', Reps1, Reps2);
chk_ActionReply_commandReply(Reps1, [] = Reps2) ->
    not_equal('ActionReply_commandReply', Reps1, Reps2);
chk_ActionReply_commandReply([H|T1], [H|T2]) ->
    case is_CommandReply(H) of
	true ->
	    chk_ActionReply_commandReply(T1, T2);
	false ->
	    wrong_type('ActionReply_commandReply_val', H)
    end;
chk_ActionReply_commandReply([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_CommandReply(H1, H2) end, 
	     'ActionReply_commandReply_val'),
    chk_ActionReply_commandReply(T1, T2);
chk_ActionReply_commandReply(R1, R2) ->
    wrong_type('ActionReply_commandReply', R1, R2).


%% -- ContextRequest --

is_opt_ContextRequest(V) ->
    is_OPTIONAL(fun is_ContextRequest/1, V).

is_ContextRequest(#'ContextRequest'{priority    = Prio,
				    emergency   = Em,
				    topologyReq = TopReq,
				    iepscallind = Ieps,
				    contextProp = CtxProp,
				    contextList = CtxList}) ->
    d("is_ContextRequest -> entry"),
    is_ContextRequest_priority(Prio) andalso
    is_ContextRequest_emergency(Em) andalso
    is_ContextRequest_topologyReq(TopReq) andalso
    is_ContextRequest_iepscallind(Ieps) andalso
    is_ContextRequest_contextProp(CtxProp) andalso
    is_ContextRequest_contextList(CtxList);
is_ContextRequest(_) ->
    false.

is_ContextRequest_priority(asn1_NOVALUE) ->
    true;
is_ContextRequest_priority(V) ->
    is_INTEGER(V, {range, 1, 15}).

is_ContextRequest_emergency(asn1_NOVALUE) ->
    true;
is_ContextRequest_emergency(V) ->
    is_BOOLEAN(V).

is_ContextRequest_topologyReq(asn1_NOVALUE) ->
    true;
is_ContextRequest_topologyReq([]) ->
    true;
is_ContextRequest_topologyReq([H|T]) ->
    is_TopologyRequest(H) andalso is_ContextRequest_topologyReq(T);
is_ContextRequest_topologyReq(_) ->
    false.

is_ContextRequest_iepscallind(asn1_NOVALUE) ->
    true;
is_ContextRequest_iepscallind(V) ->
    is_BOOLEAN(V).

is_ContextRequest_contextProp(asn1_NOVALUE) ->
    true;
is_ContextRequest_contextProp([]) ->
    true;
is_ContextRequest_contextProp([H|T]) ->
    is_PropertyParm(H) andalso is_ContextRequest_contextProp(T);
is_ContextRequest_contextProp(_) ->
    false.

is_ContextRequest_contextList(asn1_NOVALUE) ->
    true;
is_ContextRequest_contextList([]) ->
    true;
is_ContextRequest_contextList([H|T]) ->
    is_ContextID(H) andalso is_ContextRequest_contextList(T);
is_ContextRequest_contextList(_) ->
    false.

chk_opt_ContextRequest(R1, R2) ->
    chk_OPTIONAL('ContextRequest', R1, R2, 
		 fun is_ContextRequest/1, fun chk_ContextRequest/2).

chk_ContextRequest(R, R) ->
    chk_type(fun is_ContextRequest/1, 'ContextRequest', R);
chk_ContextRequest(#'ContextRequest'{priority    = Prio1,
				     emergency   = Em1,
				     topologyReq = TopReq1,
				     iepscallind = Ieps1,
				     contextProp = CtxProp1,
				     contextList = CtxList1},
		   #'ContextRequest'{priority    = Prio2,
				     emergency   = Em2,
				     topologyReq = TopReq2,
				     iepscallind = Ieps2,
				     contextProp = CtxProp2,
				     contextList = CtxList2}) ->
    chk_ContextRequest_priority(Prio1, Prio2),
    chk_ContextRequest_emergency(Em1, Em2),
    chk_ContextRequest_topologyReq(TopReq1, TopReq2),
    chk_ContextRequest_iepscallind(Ieps1, Ieps2),
    chk_ContextRequest_contextProp(CtxProp1, CtxProp2),
    chk_ContextRequest_contextList(CtxList1, CtxList2),
    ok;
chk_ContextRequest(R1, R2) ->
    wrong_type('ContextRequest', R1, R2).


chk_ContextRequest_priority(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_ContextRequest_priority(P, P) ->
    chk_type(fun is_ContextRequest_priority/1, 'ContextRequest_priority', P);
chk_ContextRequest_priority(P1, P2) ->
    case (is_ContextRequest_priority(P1) andalso 
	  is_ContextRequest_priority(P2)) of
	true ->
	    not_equal('ContextRequest_priority', P1, P2);
	false ->
	    wrong_type(contextRequest_priority, P1, P2)
    end.


chk_ContextRequest_emergency(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextRequest_emergency(E, E) ->
    chk_type(fun is_ContextRequest_emergency/1, 'ContextRequest_emergency', E);
chk_ContextRequest_emergency(E1, E2) ->
    case (is_ContextRequest_emergency(E1) andalso 
	  is_ContextRequest_emergency(E2)) of
	true ->
	    not_equal('ContextRequest_emergency', E1, E2);
	false ->
	    wrong_type('ContextRequest_emergency', E1, E2)
    end.

chk_ContextRequest_topologyReq(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextRequest_topologyReq([], []) ->
    ok;
chk_ContextRequest_topologyReq([] = T1, T2) ->
    not_equal('ContextRequest_topologyReq', T1, T2);
chk_ContextRequest_topologyReq(T1, [] = T2) ->
    not_equal('ContextRequest_topologyReq', T1, T2);
chk_ContextRequest_topologyReq([H|T1], [H|T2]) ->
    case is_TopologyRequest(H) of
	true ->
	    chk_ContextRequest_topologyReq(T1, T2);
	false ->
	    wrong_type('ContextRequest_topologyReq_val', H)
    end;
chk_ContextRequest_topologyReq([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_TopologyRequest(H1, H2) end, 
	     'ContextRequest_topologyReq_val'),
    chk_ContextRequest_topologyReq(T1, T2);
chk_ContextRequest_topologyReq(T1, T2) ->
    wrong_type('ContextRequest_topologyReq', T1, T2).


chk_ContextRequest_iepscallind(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextRequest_iepscallind(E, E) ->
    chk_type(fun is_ContextRequest_iepscallind/1, 
	     'ContextRequest_iepscallind', E);
chk_ContextRequest_iepscallind(E1, E2) ->
    case (is_ContextRequest_iepscallind(E1) andalso 
	  is_ContextRequest_iepscallind(E2)) of
	true ->
	    case (((E1 == false)        and (E2 == asn1_NOVALUE)) or
		  ((E1 == asn1_NOVALUE) and (E2 == false))) of
		true ->
		    ok;
		false ->
		    not_equal('ContextRequest_iepscallind', E1, E2)
	    end;
		
	false ->
	    wrong_type('ContextRequest_iepscallind', E1, E2)
    end.

chk_ContextRequest_contextProp(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextRequest_contextProp([], []) ->
    ok;
chk_ContextRequest_contextProp([] = T1, T2) ->
    not_equal('ContextRequest_contextProp', T1, T2);
chk_ContextRequest_contextProp(T1, [] = T2) ->
    not_equal('ContextRequest_contextProp', T1, T2);
chk_ContextRequest_contextProp([H|T1], [H|T2]) ->
    case is_PropertyParm(H) of
	true ->
	    chk_ContextRequest_contextProp(T1, T2);
	false ->
	    wrong_type('ContextRequest_contextProp_val', H)
    end;
chk_ContextRequest_contextProp([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_PropertyParm(H1, H2) end, 
	     'ContextRequest_contextProp_val'),
    chk_ContextRequest_contextProp(T1, T2);
chk_ContextRequest_contextProp(T1, T2) ->
    wrong_type('ContextRequest_contextProp', T1, T2).

chk_ContextRequest_contextList(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextRequest_contextList([], []) ->
    ok;
chk_ContextRequest_contextList([] = T1, T2) ->
    not_equal('ContextRequest_contextList', T1, T2);
chk_ContextRequest_contextList(T1, [] = T2) ->
    not_equal('ContextRequest_contextList', T1, T2);
chk_ContextRequest_contextList([H|T1], [H|T2]) ->
    case is_ContextID(H) of
	true ->
	    chk_ContextRequest_contextList(T1, T2);
	false ->
	    wrong_type('ContextRequest_contextList_val', H)
    end;
chk_ContextRequest_contextList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_ContextID(H1, H2) end, 
	     'ContextRequest_contextList_val'),
    chk_ContextRequest_contextList(T1, T2);
chk_ContextRequest_contextList(T1, T2) ->
    wrong_type('ContextRequest_contextList', T1, T2).


%% -- ContextAttrAuditRequest --

is_opt_ContextAttrAuditRequest(asn1_NOVALUE) ->
    true;
is_opt_ContextAttrAuditRequest(V) ->
    is_ContextAttrAuditRequest(V).

is_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{topology          = T,
			     emergency         = E,
			     priority          = P,
			     iepscallind       = I,
			     contextPropAud    = A,
			     selectpriority    = SP,
			     selectemergency   = SE,
			     selectiepscallind = SI,
			     selectLogic       = SL}) ->
    d("is_ContextAttrAuditRequest -> entry"),
    is_opt_NULL(T) andalso 
	is_opt_NULL(E) andalso 
	is_opt_NULL(P) andalso 
	is_opt_NULL(I) andalso 
	is_ContextAttrAuditRequest_contextPropAud(A) andalso 
	is_ContextAttrAuditRequest_selectpriority(SP) andalso 
	is_ContextAttrAuditRequest_selectemergency(SE) andalso 
	is_ContextAttrAuditRequest_selectiepscallind(SI) andalso 
	is_opt_SelectLogic(SL);
is_ContextAttrAuditRequest(_) ->
    false.

is_ContextAttrAuditRequest_contextPropAud(asn1_NOVALUE) ->
    true;
is_ContextAttrAuditRequest_contextPropAud([]) ->
    true;
is_ContextAttrAuditRequest_contextPropAud([H|T]) ->
    d("is_ContextAttrAuditRequest_contextPropAud -> entry"),
    is_IndAudPropertyParm(H) andalso 
	is_ContextAttrAuditRequest_contextPropAud(T).

is_ContextAttrAuditRequest_selectpriority(asn1_NOVALUE) ->
    true;
is_ContextAttrAuditRequest_selectpriority(V) ->
    is_INTEGER(V, {range, 1, 15}).

is_ContextAttrAuditRequest_selectemergency(asn1_NOVALUE) ->
    true;
is_ContextAttrAuditRequest_selectemergency(V) ->
    is_BOOLEAN(V).

is_ContextAttrAuditRequest_selectiepscallind(asn1_NOVALUE) ->
    true;
is_ContextAttrAuditRequest_selectiepscallind(V) ->
    is_BOOLEAN(V).


chk_opt_ContextAttrAuditRequest(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_ContextAttrAuditRequest(R1, R2) ->
    chk_ContextAttrAuditRequest(R1, R2).

chk_ContextAttrAuditRequest(R, R) ->
    chk_type(fun is_ContextAttrAuditRequest/1, 'ContextAttrAuditRequest', R);
chk_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{topology          = T1,
			     emergency         = E1,
			     priority          = P1,
			     iepscallind       = I1,
			     contextPropAud    = A1,
			     selectpriority    = SP1,
			     selectemergency   = SE1,
			     selectiepscallind = SI1,
			     selectLogic       = SL1},
  #'ContextAttrAuditRequest'{topology          = T2,
			     emergency         = E2,
			     priority          = P2,
			     iepscallind       = I2,
			     contextPropAud    = A2,
			     selectpriority    = SP2,
			     selectemergency   = SE2,
			     selectiepscallind = SI2,
			     selectLogic       = SL2}) ->
    validate(fun() -> chk_opt_NULL(T1, T2) end, 
	     'ContextAttrAuditRequest_topology'),
    validate(fun() -> chk_opt_NULL(E1, E2) end, 
	     'ContextAttrAuditRequest_emergency'),
    validate(fun() -> chk_opt_NULL(P1, P2) end, 
	     'ContextAttrAuditRequest_priority'), 
    validate(fun() -> chk_opt_NULL(I1, I2) end, 
	     'ContextAttrAuditRequest_iepscallind'), 
    chk_ContextAttrAuditRequest_contextPropAud(lists:sort(A1), 
					       lists:sort(A2)),
    chk_ContextAttrAuditRequest_selectpriority(SP1, SP2),
    chk_ContextAttrAuditRequest_selectemergency(SE1, SE2),
    chk_ContextAttrAuditRequest_selectiepscallind(SI1, SI2),
    chk_ContextAttrAuditRequest_selectLogic(SL1, SL2),
    ok.

chk_ContextAttrAuditRequest_contextPropAud(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextAttrAuditRequest_contextPropAud(A, A) ->
    chk_type(fun is_ContextAttrAuditRequest_contextPropAud/1, 
	     'ContextAttrAuditRequest_contextPropAud', A);
chk_ContextAttrAuditRequest_contextPropAud([], []) ->
    ok;
chk_ContextAttrAuditRequest_contextPropAud([] = T1, T2) ->
    not_equal('ContextAttrAuditRequest_contextPropAud', T1, T2);
chk_ContextAttrAuditRequest_contextPropAud(T1, [] = T2) ->
    not_equal('ContextAttrAuditRequest_contextPropAud', T1, T2);
chk_ContextAttrAuditRequest_contextPropAud([H|T1], [H|T2]) ->
    case is_IndAudPropertyParm(H) of
	true ->
	    chk_ContextAttrAuditRequest_contextPropAud(T1, T2);
	false ->
	    wrong_type('ContextAttrAuditRequest_contextPropAud_val', H)
    end;
chk_ContextAttrAuditRequest_contextPropAud([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_PropertyParm(H1, H2) end, 
	     'ContextAttrAuditRequest_contextPropAud_val'),
    chk_ContextAttrAuditRequest_contextPropAud(T1, T2);
chk_ContextAttrAuditRequest_contextPropAud(T1, T2) ->
    wrong_type('ContextAttrAuditRequest_contextPropAud', T1, T2).

chk_ContextAttrAuditRequest_selectpriority(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextAttrAuditRequest_selectpriority(A, A) ->
    chk_type(fun is_ContextAttrAuditRequest_selectpriority/1, 
	     'ContextAttrAuditRequest_selectpriority', A);
chk_ContextAttrAuditRequest_selectpriority(SP1, SP2) ->
    case (is_ContextAttrAuditRequest_selectpriority(SP1) andalso
	  is_ContextAttrAuditRequest_selectpriority(SP2)) of
	true ->
	    not_equal('ContextAttrAuditRequest_selectpriority', SP1, SP2);
	false ->
	    wrong_type('ContextAttrAuditRequest_selectpriority', SP1, SP2)
    end.

chk_ContextAttrAuditRequest_selectemergency(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextAttrAuditRequest_selectemergency(A, A) ->
    chk_type(fun is_ContextAttrAuditRequest_selectemergency/1, 
	     'ContextAttrAuditRequest_selectemergency', A);
chk_ContextAttrAuditRequest_selectemergency(SE1, SE2) ->
    case (is_ContextAttrAuditRequest_selectemergency(SE1) andalso
	  is_ContextAttrAuditRequest_selectemergency(SE2)) of
	true ->
	    not_equal('ContextAttrAuditRequest_selectemergency', SE1, SE2);
	false ->
	    wrong_type('ContextAttrAuditRequest_selectemergency', SE1, SE2)
    end.

chk_ContextAttrAuditRequest_selectiepscallind(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_ContextAttrAuditRequest_selectiepscallind(A, A) ->
    chk_type(fun is_ContextAttrAuditRequest_selectiepscallind/1, 
	     'ContextAttrAuditRequest_selectiepscallind', A);
chk_ContextAttrAuditRequest_selectiepscallind(SI1, SI2) ->
    case (is_ContextAttrAuditRequest_selectiepscallind(SI1) andalso
	  is_ContextAttrAuditRequest_selectiepscallind(SI2)) of
	true ->
	    not_equal('ContextAttrAuditRequest_selectiepscallind', SI1, SI2);
	false ->
	    wrong_type('ContextAttrAuditRequest_selectiepscallind', SI1, SI2)
    end.

chk_ContextAttrAuditRequest_selectLogic(SL1, SL2) ->
    validate(fun() -> chk_opt_SelectLogic(SL1, SL2) end,
	     'ContextAttrAuditRequest_selectLogic').


%% -- SelectLogic --

is_opt_SelectLogic(asn1_NOVALUE) ->
    true;
is_opt_SelectLogic(SL) ->
    is_SelectLogic(SL).

is_SelectLogic({Tag, Val}) ->
    d("is_SelectLogic -> entry"),
    is_SelectLogic_tag(Tag) andalso is_SelectLogic_val(Tag, Val);
is_SelectLogic(_) ->
    false.

is_SelectLogic_tag(Tag) ->
    Tags = [andAUDITSelect, orAUDITSelect],
    lists:member(Tag, Tags).

is_SelectLogic_val(andAUDITSelect, 'NULL') -> true;
is_SelectLogic_val(orAUDITSelect,  'NULL') -> true;
is_SelectLogic_val(_, _) -> false.

chk_opt_SelectLogic(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_SelectLogic(SL1, SL2) ->
    chk_SelectLogic(SL1, SL2).

chk_SelectLogic(SL, SL) ->
    chk_type(fun is_SelectLogic/1, 'SelectLogic', SL);
chk_SelectLogic(asn1_NOVALUE, {andAUDITSelect, 'NULL'}) ->
    ok; % AND is default
chk_SelectLogic({andAUDITSelect, 'NULL'}, asn1_NOVALUE) ->
    ok; % AND is default
chk_SelectLogic({Tag, Val1} = SL1, {Tag, Val2} = SL2) ->
    case (is_SelectLogic_tag(Tag) andalso
	  is_SelectLogic_val(Tag, Val1) andalso
	  is_SelectLogic_val(Tag, Val2)) of
	true ->
	    chk_SelectLogic_val(Tag, Val1, Val2);
	false ->
	    wrong_type('SelectLogic', SL1, SL2)
    end;
chk_SelectLogic({Tag1, Val1} = SL1, {Tag2, Val2} = SL2) ->
    case ((is_SelectLogic_tag(Tag1) andalso 
	   is_SelectLogic_val(Tag1, Val1)) andalso
	  (is_SelectLogic_tag(Tag2) andalso 
	   is_SelectLogic_val(Tag2, Val2))) of
	true ->
	    not_equal('SelectLogic', SL1, SL2);
	false ->
	    wrong_type('SelectLogic', SL1, SL2)
    end;
chk_SelectLogic(SL1, SL2) ->
    wrong_type('SelectLogic', SL1, SL2).
    
chk_SelectLogic_val(andAUDITSelect, SL1, SL2) ->
    validate(fun() -> chk_NULL(SL1, SL2) end, 'SelectLogic_andAUDITSelect');
chk_SelectLogic_val(orAUDITSelect, SL1, SL2) ->
    validate(fun() -> chk_NULL(SL1, SL2) end, 'SelectLogic_orAUDITSelect').

		     
%% -- CommandRequest -- 

is_CommandRequest(#'CommandRequest'{command        = Cmd,
				    optional       = Opt,
				    wildcardReturn = WR}) ->
    d("is_CommandRequest -> entry with"
      "~n   Cmd: ~p"
      "~n   Opt: ~p"
      "~n   WR:  ~p", [Cmd, Opt, WR]),
    is_Command(Cmd) andalso is_opt_NULL(Opt) andalso is_opt_NULL(WR);
is_CommandRequest(_) ->
    false.

chk_CommandRequest(C, C) ->
    chk_type(fun is_CommandRequest/1, 'CommandRequest', C);
chk_CommandRequest(#'CommandRequest'{command        = Cmd1,
				     optional       = Opt1,
				     wildcardReturn = WR1}, 
		   #'CommandRequest'{command        = Cmd2,
				     optional       = Opt2,
				     wildcardReturn = WR2}) ->
    validate(fun() -> chk_Command(Cmd1, Cmd2) end, 'CommandRequest'),
    validate(fun() -> chk_opt_NULL(Opt1, Opt2) end, 'CommandRequest'),
    validate(fun() -> chk_opt_NULL(WR1, WR2) end, 'CommandRequest'),
    ok;
chk_CommandRequest(R1, R2) ->
    wrong_type('CommandRequest', R1, R2).


%% -- Command --

is_Command({Tag, Val}) ->
    d("is_Command -> entry with"
      "~n   Tag: ~p"
      "~n   Val: ~p", [Tag, Val]),
    is_Command_tag(Tag) andalso is_Command_val(Tag, Val);
is_Command(_) ->
    false.

is_Command_tag(Tag) ->
    Tags = [addReq, moveReq, modReq, subtractReq, auditCapRequest, 
	    auditValueRequest, notifyReq, serviceChangeReq],
    lists:member(Tag, Tags).

is_Command_val(addReq,  V)           -> is_AmmRequest(V);
is_Command_val(moveReq, V)           -> is_AmmRequest(V);
is_Command_val(modReq,  V)           -> is_AmmRequest(V);
is_Command_val(subtractReq, V)       -> is_SubtractRequest(V);
is_Command_val(auditCapRequest, V)   -> is_AuditRequest(V);
is_Command_val(auditValueRequest, V) -> is_AuditRequest(V);
is_Command_val(notifyReq, V)         -> is_NotifyRequest(V);
is_Command_val(serviceChangeReq, V)  -> is_ServiceChangeRequest(V).

chk_Command(Cmd, Cmd) ->
    chk_type(fun is_Command/1, 'Command', Cmd);
chk_Command({Tag, Val1} = Cmd1, {Tag, Val2} = Cmd2) ->
    case (is_Command_tag(Tag) andalso 
	  is_Command_val(Tag, Val1) andalso 
	  is_Command_val(Tag, Val2)) of
	true ->
	    chk_Command_val(Tag, Val1, Val2);
	false ->
	    wrong_type('Command', Cmd1, Cmd2)
    end;
chk_Command({Tag1, Val1} = Cmd1, {Tag2, Val2} = Cmd2) ->
    case ((is_Command_tag(Tag1) andalso is_Command_val(Tag1, Val1)) andalso
	  (is_Command_tag(Tag2) andalso is_Command_val(Tag2, Val2))) of
	true ->
	    not_equal('Command', Cmd1, Cmd2);
	false ->
	    wrong_type('Command', Cmd1, Cmd2)
    end;
chk_Command(Cmd1, Cmd2) ->
    wrong_type('Command', Cmd1, Cmd2).


chk_Command_val(addReq, R1, R2) ->
    validate(fun() -> chk_AmmRequest(R1, R2) end, 'Command_addReq');
chk_Command_val(moveReq, R1, R2) ->
    validate(fun() -> chk_AmmRequest(R1, R2) end, 'Command_moveReq');
chk_Command_val(modReq, R1, R2) ->
    validate(fun() -> chk_AmmRequest(R1, R2) end, 'Command_modReq');
chk_Command_val(subtractReq, R1, R2) ->
    validate(fun() -> chk_SubtractRequest(R1, R2) end, 'Command_subtractReq');
chk_Command_val(auditCapRequest, R1, R2) ->
    validate(fun() -> chk_AuditRequest(R1, R2) end, 'Command_auditCapRequest');
chk_Command_val(auditValueRequest, R1, R2) ->
    validate(fun() -> chk_AuditRequest(R1, R2) end, 
	     'Command_auditValueRequest');
chk_Command_val(notifyReq, R1, R2) ->
    validate(fun() -> chk_NotifyRequest(R1, R2) end, 'Command_notifyReq');
chk_Command_val(serviceChangeReq, R1, R2) ->
    validate(fun() -> chk_ServiceChangeRequest(R1, R2) end, 
	     'Command_serviceChangeReq').


%% -- CommandReply --

is_CommandReply({Tag, Val}) ->
    d("is_CommandReply -> entry"),
    is_CommandReply_tag(Tag) andalso is_CommandReply_val(Tag, Val);
is_CommandReply(_) ->
    false.

is_CommandReply_tag(Tag) ->
    Tags = [addReply, moveReply, modReply, subtractReply, 
	    auditCapReply, auditValueReply, notifyReply, serviceChangeReply],
    lists:member(Tag, Tags).
     
is_CommandReply_val(addReply, V)           -> is_AmmsReply(V);
is_CommandReply_val(moveReply, V)          -> is_AmmsReply(V);
is_CommandReply_val(modReply, V)           -> is_AmmsReply(V);
is_CommandReply_val(subtractReply, V)      -> is_AmmsReply(V);
is_CommandReply_val(auditCapReply, V)      -> is_AuditReply(V);
is_CommandReply_val(auditValueReply, V)    -> is_AuditReply(V);
is_CommandReply_val(notifyReply, V)        -> is_NotifyReply(V);
is_CommandReply_val(serviceChangeReply, V) -> is_ServiceChangeReply(V).

chk_CommandReply({Tag, Val} = Cmd, Cmd) ->
    case (is_CommandReply_tag(Tag) andalso is_CommandReply_val(Tag, Val)) of
	true ->
	    ok;
	false ->
	    wrong_type('CommandReply', Cmd)
    end;
chk_CommandReply({Tag, Val1} = Cmd1, {Tag, Val2} = Cmd2) ->
    case (is_CommandReply_tag(Tag) andalso 
	  is_CommandReply_val(Tag, Val1) andalso 
	  is_CommandReply_val(Tag, Val2)) of
	true ->
	    chk_CommandReply_val(Tag, Val1, Val2);
	false ->
	    wrong_type('CommandReply', Cmd1, Cmd2)
    end;
chk_CommandReply({Tag1, Val1} = Cmd1, {Tag2, Val2} = Cmd2) ->
    case ((is_CommandReply_tag(Tag1) andalso 
	   is_CommandReply_val(Tag1, Val1)) andalso 
	  (is_CommandReply_tag(Tag2) andalso 
	   is_CommandReply_val(Tag2, Val2))) of
	true ->
	    not_equal('CommandReply', Cmd1, Cmd2);
	false ->
	    wrong_type('CommandReply', Cmd1, Cmd2)
    end;
chk_CommandReply(Cmd1, Cmd2) ->
    wrong_type('CommandReply', Cmd1, Cmd2).

chk_CommandReply_val(addReply, V1, V2) ->
    validate(fun() -> chk_AmmsReply(V1, V2) end, 'CommandReply_addReply');
chk_CommandReply_val(moveReply, V1, V2) ->
    validate(fun() -> chk_AmmsReply(V1, V2) end, 'CommandReply_moveReply');
chk_CommandReply_val(modReply, V1, V2) ->
    validate(fun() -> chk_AmmsReply(V1, V2) end, 'CommandReply_modReply');
chk_CommandReply_val(subtractReply, V1, V2) ->
    validate(fun() -> chk_AmmsReply(V1, V2) end, 'CommandReply_subtractReply');
chk_CommandReply_val(auditCapReply, V1, V2) ->
    validate(fun() -> chk_AuditReply(V1, V2) end, 
	     'CommandReply_auditCapReply');
chk_CommandReply_val(auditValueReply, V1, V2) ->
    validate(fun() -> chk_AuditReply(V1, V2) end, 
	     'CommandReply_auditValueReply');
chk_CommandReply_val(notifyReply, V1, V2) ->
    validate(fun() -> chk_NotifyReply(V1, V2) end, 'CommandReply_notifyReply');
chk_CommandReply_val(serviceChangeReply, V1, V2) ->
    validate(fun() -> chk_ServiceChangeReply(V1, V2) end, 
	     'CommandReply_serviceChangeReply').
     

%% -- TopologyRequest --

is_TopologyRequest(#'TopologyRequest'{terminationFrom            = F,
				      terminationTo              = T,
				      topologyDirection          = TD,
				      streamID                   = S,
				      topologyDirectionExtension = TDE}) ->
    d("is_TopologyRequest -> entry"),
    is_TerminationID(F) andalso 
	is_TerminationID(T) andalso 
	is_TopologyRequest_topologyDirection(TD) andalso 
	is_opt_StreamID(S) andalso 
	is_TopologyRequest_topologyDirectionExtension(TDE);
is_TopologyRequest(_) ->
    false.
    
is_TopologyRequest_topologyDirection(D) ->
    lists:member(D, [bothway, isolate, oneway]).

is_TopologyRequest_topologyDirectionExtension(asn1_NOVALUE) ->
    true;
is_TopologyRequest_topologyDirectionExtension(D) ->
    lists:member(D, [onewayexternal, onewayboth]).


chk_TopologyRequest(T, T) when is_record(T,'TopologyRequest') ->
    ok;
chk_TopologyRequest(#'TopologyRequest'{terminationFrom            = F1,
                                       terminationTo              = T1,
                                       topologyDirection          = TD1,
				       streamID                   = S1,
				       topologyDirectionExtension = TDE1},
                    #'TopologyRequest'{terminationFrom            = F2,
                                       terminationTo              = T2,
                                       topologyDirection          = TD2,
				       streamID                   = S2,
				       topologyDirectionExtension = TDE2}) ->
    validate(fun() -> chk_TerminationID(F1, F2) end, 
	     'TopologyRequest_terminationFrom'),
    validate(fun() -> chk_TerminationID(T1, T2) end, 
	     'TopologyRequest_terminationTo'),
    chk_TopologyRequest_topologyDirection(TD1, TD2), 
    validate(fun() -> chk_opt_StreamID(S1, S2) end, 
	     'TopologyRequest_streamID'),
    chk_TopologyRequest_topologyDirectionExtension(TDE1, TDE2), 
    ok.

chk_TopologyRequest_topologyDirection(D, D) ->
    case is_TopologyRequest_topologyDirection(D) of
	true ->
	    ok;
	false ->
	    wrong_type('TopologyRequest_topologyDirection', D)
    end;
chk_TopologyRequest_topologyDirection(D1, D2) ->
    case (is_TopologyRequest_topologyDirection(D1) andalso 
	  is_TopologyRequest_topologyDirection(D1)) of
	true ->
	    not_equal('TopologyRequest_topologyDirection', D1, D2);
	false ->
	    wrong_type('TopologyRequest_topologyDirection', D1, D2)
    end.

chk_TopologyRequest_topologyDirectionExtension(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_TopologyRequest_topologyDirectionExtension(D, D) ->
    case is_TopologyRequest_topologyDirectionExtension(D) of
	true ->
	    ok;
	false ->
	    wrong_type('TopologyRequest_topologyDirectionExtension', D)
    end;
chk_TopologyRequest_topologyDirectionExtension(D1, D2) ->
    case (is_TopologyRequest_topologyDirectionExtension(D1) andalso 
	  is_TopologyRequest_topologyDirectionExtension(D1)) of
	true ->
	    not_equal('TopologyRequest_topologyDirectionExtension', D1, D2);
	false ->
	    wrong_type('TopologyRequest_topologyDirectionExtension', D1, D2)
    end.


%% -- AmmRequest --

is_AmmRequest(#'AmmRequest'{terminationID = Tids,
			    descriptors   = Descs}) ->
    d("is_AmmRequest -> entry with"
      "~n   Tids:  ~p", [Tids]),
    is_TerminationIDList(Tids) andalso is_AmmRequest_descriptors(Descs);
is_AmmRequest(_) ->
    false.

is_AmmRequest_descriptors(Descs) ->
    d("is_AmmRequest_descriptors -> entry"),
    is_AmmRequest_descriptors(Descs, []).

is_AmmRequest_descriptors([], _) ->
    true;
is_AmmRequest_descriptors([{Tag, _} = Desc|Descs], FoundDescs) ->
    d("is_AmmRequest_descriptors -> entry with"
      "~n   Tag:        ~p"
      "~n   FoundDescs: ~p", [Tag, FoundDescs]),
    case lists:member(Tag, FoundDescs) of
	true ->
	    atmost_once('AmmRequest_descriptors', Tag);
	false ->
	    case is_AmmDescriptor(Desc) of
		true ->
		    is_AmmRequest_descriptors(Descs, [Tag|FoundDescs]);
		false ->
		    wrong_type('AmmRequest_descriptors', Desc)
	    end
    end;
is_AmmRequest_descriptors(Descs, _) ->
    d("is_AmmRequest_descriptors -> entry with WRONG TYPE"
      "~n   Descs: ~p", [Descs]),
    wrong_type('AmmRequest_descriptors', Descs).

	    
chk_AmmRequest(R, R) when is_record(R, 'AmmRequest') ->
    d("chk_AmmRequest -> entry when equal"),
    chk_type(fun is_AmmRequest/1, 'AmmRequest', R);
chk_AmmRequest(#'AmmRequest'{terminationID = Tids1,
			     descriptors   = Descs1},
	       #'AmmRequest'{terminationID = Tids2,
			     descriptors   = Descs2}) ->
    d("chk_AmmRequest -> entry with not equal"
      "~n   Tids1:  ~p"
      "~n   Tids2:  ~p", [Tids1, Tids2]),
    validate(
      fun() -> chk_TerminationIDList(Tids1, Tids2) end, 
      'AmmRequest'),
    validate(
      fun() -> chk_AmmRequest_descriptors(Descs1, Descs2) end, 
      'AmmRequest'),
    ok.
		      

chk_AmmRequest_descriptors([], []) ->
    d("chk_AmmRequest_descriptors -> done when OK"),
    ok;
chk_AmmRequest_descriptors([] = Descs1, Descs2) ->
    d("chk_AmmRequest_descriptors -> done when NOT EQUAL:"
      "~n   Descs1: ~p"
      "~n   Descs1: ~p", [Descs1, Descs2]),
    not_equal('AmmRequest_descriptors', Descs1, Descs2);
chk_AmmRequest_descriptors(Descs1, [] = Descs2) ->
    d("chk_AmmRequest_descriptors -> done when NOT EQUAL:"
      "~n   Descs1: ~p"
      "~n   Descs1: ~p", [Descs1, Descs2]),
    not_equal('AmmRequest_descriptors', Descs1, Descs2);
chk_AmmRequest_descriptors([H|T1], [H|T2]) ->
    d("chk_AmmRequest_descriptors -> entry when equal"),
    case is_AmmDescriptor(H) of
	true ->
	    chk_AmmRequest_descriptors(T1, T2);
	false ->
	    wrong_type('AmmRequest_descriptors_val', H)
    end;
chk_AmmRequest_descriptors([H1|T1], [H2|T2]) ->
    d("chk_AmmRequest_descriptors -> entry when not equal"),
    validate(fun() -> chk_AmmDescriptor(H1, H2) end, 
	     'AmmRequest_descriptors_val'),
    chk_AmmRequest_descriptors(T1, T2);
chk_AmmRequest_descriptors(Descs1, Descs2) ->
    d("chk_AmmRequest_descriptors -> done when WRONG TYPE:"
      "~n   Descs1: ~p"
      "~n   Descs1: ~p", [Descs1, Descs2]),
    wrong_type('AmmRequest_descriptors', Descs1, Descs2).


%% -- AmmDescriptor --

is_AmmDescriptor({Tag, Val}) ->
    d("is_AmmDescriptor -> entry with"
      "~n   Tag: ~p"
      "~n   Val: ~p",[Tag, Val]),
    is_AmmDescriptor_tag(Tag) andalso is_AmmDescriptor_val(Tag, Val);
is_AmmDescriptor(_) ->
    false.

is_AmmDescriptor_tag(Tag) ->
    Tags = [mediaDescriptor, modemDescriptor, muxDescriptor, eventsDescriptor, 
	    eventBufferDescriptor, signalsDescriptor, digitMapDescriptor,
	    auditDescriptor, statisticsDescriptor], 
    lists:member(Tag, Tags).

is_AmmDescriptor_val(mediaDescriptor, D) ->
    is_MediaDescriptor(D);
is_AmmDescriptor_val(modemDescriptor, D) ->
    is_ModemDescriptor(D);
is_AmmDescriptor_val(muxDescriptor, D) ->
    is_MuxDescriptor(D);
is_AmmDescriptor_val(eventsDescriptor, D) ->
    is_EventsDescriptor(D);
is_AmmDescriptor_val(eventBufferDescriptor, D) ->
    is_EventBufferDescriptor(D);
is_AmmDescriptor_val(signalsDescriptor, D) ->
    is_SignalsDescriptor(D);
is_AmmDescriptor_val(digitMapDescriptor, D) ->
    is_DigitMapDescriptor(D);
is_AmmDescriptor_val(auditDescriptor, D) ->
    is_AuditDescriptor(D);
is_AmmDescriptor_val(statisticsDescriptor, D) ->
    is_StatisticsDescriptor(D).

chk_AmmDescriptor(D, D) ->
    chk_type(fun is_AmmDescriptor_tag/1, 'AmmDescriptor', D);
chk_AmmDescriptor({Tag, Val1} = Cmd1, {Tag, Val2} = Cmd2) ->
    case (is_AmmDescriptor_tag(Tag) andalso
	  is_AmmDescriptor_val(Tag, Val1) andalso
	  is_AmmDescriptor_val(Tag, Val2)) of
	true ->
	    chk_AmmDescriptor_val(Tag, Val1, Val2);
	false ->
	    wrong_type('AmmDescriptor', Cmd1, Cmd2)
    end;
chk_AmmDescriptor({Tag1, Val1} = Cmd1, {Tag2, Val2} = Cmd2) ->
    case ((is_AmmDescriptor_tag(Tag1) andalso 
	   is_AmmDescriptor_val(Tag1, Val1)) andalso
	  (is_AmmDescriptor_tag(Tag2) andalso 
	   is_AmmDescriptor_val(Tag2, Val2))) of
	true ->
	    not_equal('AmmDescriptor', Cmd1, Cmd2);
	false ->
	    wrong_type('AmmDescriptor', Cmd1, Cmd2)
    end;
chk_AmmDescriptor(Cmd1, Cmd2) ->
    wrong_type('AmmDescriptor', Cmd1, Cmd2).

chk_AmmDescriptor_val(mediaDescriptor, D1, D2) ->
    validate(fun() -> chk_MediaDescriptor(D1, D2) end, 'AmmDescriptor');
chk_AmmDescriptor_val(modemDescriptor, D1, D2) ->
    validate(fun() -> chk_ModemDescriptor(D1, D2) end, 'AmmDescriptor');
chk_AmmDescriptor_val(muxDescriptor, D1, D2) ->
    validate(fun() -> chk_MuxDescriptor(D1, D2) end, 'AmmDescriptor');
chk_AmmDescriptor_val(eventsDescriptor, D1, D2) ->
    validate(fun() -> chk_EventsDescriptor(D1, D2) end, 'AmmDescriptor');
chk_AmmDescriptor_val(eventBufferDescriptor, D1, D2) ->
    validate(fun() -> chk_EventBufferDescriptor(D1, D2) end, 'AmmDescriptor');
chk_AmmDescriptor_val(signalsDescriptor, D1, D2) ->
    validate(fun() -> chk_SignalsDescriptor(D1, D2) end, 'AmmDescriptor');
chk_AmmDescriptor_val(digitMapDescriptor, D1, D2) ->
    validate(fun() -> chk_DigitMapDescriptor(D1, D2) end, 'AmmDescriptor');
chk_AmmDescriptor_val(auditDescriptor, D1, D2) ->
    validate(fun() -> chk_AuditDescriptor(D1, D2) end, 'AmmDescriptor').


%% -- AmmsReply --

is_AmmsReply(#'AmmsReply'{terminationID    = Tids,
			  terminationAudit = TA}) ->
    is_TerminationIDList(Tids) andalso is_opt_TerminationAudit(TA);
is_AmmsReply(_) ->
    false.

chk_AmmsReply(R, R) ->
    is_AmmsReply(R);
chk_AmmsReply(#'AmmsReply'{terminationID    = TID1,
			   terminationAudit = TA1},
	      #'AmmsReply'{terminationID    = TID2,
			   terminationAudit = TA2}) ->
    validate(fun() -> chk_TerminationIDList(TID1, TID2) end, 'AmmsReply'),
    validate(fun() -> chk_opt_TerminationAudit(TA1, TA2) end, 'AmmsReply'),
    ok;
chk_AmmsReply(R1, R2) ->
    wrong_type('AmmsReply', R1, R2).
	     
		     
%% -- SubtractRequest -- 

is_SubtractRequest(#'SubtractRequest'{terminationID   = TID,
				      auditDescriptor = AD}) ->
    d("is_SubtractDescriptor -> entry with"
      "~n   TID: ~p"
      "~n   AD:  ~p",[TID, AD]),
    is_TerminationIDList(TID) andalso is_opt_AuditDescriptor(AD);
is_SubtractRequest(_) ->
    false.

chk_SubtractRequest(R, R) ->
    chk_type(fun is_SubtractRequest/1, 'SubtractRequest', R);
chk_SubtractRequest(#'SubtractRequest'{terminationID   = Tids1,
				       auditDescriptor = AD1},
		    #'SubtractRequest'{terminationID   = Tids2,
				       auditDescriptor = AD2}) ->
    validate(fun() -> chk_TerminationIDList(Tids1, Tids2) end,
	     'SubtractRequest'),
    validate(fun() -> chk_opt_AuditDescriptor(AD1, AD2) end,
	     'SubtractRequest'),
    ok;
chk_SubtractRequest(SR1, SR2) ->
    wrong_type('SubtractRequest', SR1, SR2).

		      
%% -- AuditRequest --

is_AuditRequest(#'AuditRequest'{terminationID     = TID,
				auditDescriptor   = AD,
				terminationIDList = asn1_NOVALUE}) ->
    d("is_AuditRequest -> entry with"
      "~n   TID: ~p"
      "~n   AD:  ~p",[TID, AD]),
    is_TerminationID(TID) andalso 
	is_AuditDescriptor(AD);
is_AuditRequest(#'AuditRequest'{terminationID     = TID,
				auditDescriptor   = AD,
				terminationIDList = [TID|_] = TIDs}) ->
    d("is_AuditRequest -> entry with"
      "~n   TID:   ~p"
      "~n   AD:    ~p"
      "~n   TIDs:  ~p",[TID, AD, TIDs]),
    is_TerminationID(TID) andalso 
	is_AuditDescriptor(AD) andalso 
	is_TerminationIDList(TIDs);
is_AuditRequest(_) ->
    false.

chk_AuditRequest(R, R) ->
    chk_type(fun is_AuditRequest/1, 'AuditRequest', R);
chk_AuditRequest(#'AuditRequest'{terminationID     = TID1,
				 auditDescriptor   = AD1,
				 terminationIDList = [TID1|_] = TIDs1},
		 #'AuditRequest'{terminationID     = TID2,
				 auditDescriptor   = AD2,
				 terminationIDList = [TID2|_] = TIDs2}) ->
    validate(fun() -> chk_TerminationID(TID1, TID2) end,
	     'AuditRequest'),
    validate(fun() -> chk_AuditDescriptor(AD1, AD2) end,
	     'AuditRequest'),
    validate(fun() -> chk_opt_TerminationIDList(TIDs1, TIDs2) end,
	     'AuditRequest'),
    ok;
chk_AuditRequest(AR1, AR2) ->
    wrong_type('AuditRequest', AR1, AR2).


%% -- AuditReply --

is_AuditReply({Tag, Val}) ->
    is_AuditReply_tag(Tag) andalso is_AuditReply_val(Tag, Val);
is_AuditReply(_) ->
    false.

is_AuditReply_tag(Tag) ->
    Tags = [contextAuditResult, error, auditResult, auditResultTermList],
    lists:member(Tag, Tags).

is_AuditReply_val(contextAuditResult, Val) ->
    is_TerminationIDList(Val);
is_AuditReply_val(error, Val) ->
    is_ErrorDescriptor(Val);
is_AuditReply_val(auditResult, Val) ->
    is_AuditResult(Val);
is_AuditReply_val(auditResultTermList, Val) ->
    is_TermListAuditResult(Val).

chk_AuditReply(R, R) ->
    chk_type(fun is_AuditReply/1, 'AuditReply', R);
chk_AuditReply({Tag, Val1} = R1, {Tag, Val2} = R2) ->
    case (is_AuditReply_tag(Tag) andalso
	  is_AuditReply_val(Tag, Val1)andalso
	  is_AuditReply_val(Tag, Val2)) of
	true ->
	    chk_AuditReply_val(Tag, Val1, Val2);
	false ->
	    wrong_type('AuditReply', R1, R2)
    end;
chk_AuditReply({Tag1, Val1} = R1, {Tag2, Val2} = R2) ->
    case ((is_AuditReply_tag(Tag1) andalso
	   is_AuditReply_val(Tag1, Val1)) andalso
	  (is_AuditReply_tag(Tag2) andalso
	   is_AuditReply_val(Tag2, Val2))) of
	true ->
	    not_equal('AuditReply', R1, R2);
	false ->
	    wrong_type('AuditReply', R1, R2)
    end;
chk_AuditReply(AR1, AR2) ->
    wrong_type('AuditReply', AR1, AR2).

chk_AuditReply_val(contextAuditResult, Val1, Val2) ->
    chk_TerminationIDList(Val1, Val2);
chk_AuditReply_val(error, Val1, Val2) ->
    chk_ErrorDescriptor(Val1, Val2);
chk_AuditReply_val(auditResult, Val1, Val2) ->
    chk_AuditResult(Val1, Val2);
chk_AuditReply_val(auditResultTermList, Val1, Val2) ->
    chk_TermListAuditResult(Val1, Val2).

	      
%% -- AuditResult --

is_AuditResult(#'AuditResult'{terminationID          = TID,
			      terminationAuditResult = TAR}) ->
    is_TerminationID(TID) andalso is_TerminationAudit(TAR);
is_AuditResult(_) ->
    false.

chk_AuditResult(R, R) ->
    d("chk_AuditResult -> entry (1)"),
    chk_type(fun is_AuditResult/1, 'AuditResult', R);
chk_AuditResult(#'AuditResult'{terminationID          = TID1,
			       terminationAuditResult = TAR1},
		#'AuditResult'{terminationID          = TID2,
			       terminationAuditResult = TAR2}) ->
    d("chk_AuditResult -> entry (2)"),
    validate(fun() -> chk_TerminationID(TID1, TID2) end, 'AuditResult'),
    validate(fun() -> chk_TerminationAudit(TAR1, TAR2) end, 'AuditResult'),
    ok;
chk_AuditResult(AR1, AR2) ->
    d("chk_AuditResult -> entry (3)"),
    wrong_type('AuditResult', AR1, AR2).

		      
%% -- TermListAuditResult -- 

is_TermListAuditResult(#'TermListAuditResult'{terminationIDList      = TIDs,
					      terminationAuditResult = TAR}) ->
    is_TerminationIDList(TIDs) andalso is_TerminationAudit(TAR);
is_TermListAuditResult(_) ->
    false.

chk_TermListAuditResult(TLAR, TLAR) ->
    d("chk_TermListAuditResult(1) -> entry with"
      "~n   TLAR: ~p", [TLAR]),
    chk_type(fun is_TermListAuditResult/1, 'TermListAuditResult', TLAR);
chk_TermListAuditResult(
  #'TermListAuditResult'{terminationIDList      = TIDs1,
			 terminationAuditResult = TAR1},
  #'TermListAuditResult'{terminationIDList      = TIDs2,
			 terminationAuditResult = TAR2}) ->
    d("chk_TermListAuditResult(2) -> entry with"
      "~n   TIDs1: ~p"
      "~n   TAR1:  ~p"
      "~n   TIDs2: ~p"
      "~n   TAR2:  ~p", [TIDs1, TAR1, TIDs2, TAR2]),
    validate(fun() -> chk_TerminationIDList(TIDs1, TIDs2) end,
	     'TermListAuditResult_terminationIDList'),
    validate(fun() -> chk_TerminationAudit(TAR1, TAR2) end,
	     'TermListAuditResult_terminationAuditResult'),
    ok;
chk_TermListAuditResult(TLAR1, TLAR2) ->
    d("chk_TermListAuditResult(3) -> entry with"
      "~n   TLAR1: ~p"
      "~n   TLAR2: ~p", [TLAR1, TLAR2]),
    wrong_type('TermListAuditResult', TLAR1, TLAR2).
		     

%% -- TerminationAudit --

is_opt_TerminationAudit(TA) ->
    is_OPTIONAL(fun is_TerminationAudit/1, TA).

is_TerminationAudit([]) ->
    true;
is_TerminationAudit([H|T]) ->
    is_AuditReturnParameter(H) andalso is_TerminationAudit(T);
is_TerminationAudit(_) ->
    false.

chk_opt_TerminationAudit(TA1, TA2) ->
    d("chk_opt_TerminationAudit -> entry with"
      "~n   TA1: ~p"
      "~n   TA2: ~p", [TA1, TA2]),
    chk_OPTIONAL('TerminationAudit', 
		 strip_TerminationAudit(TA1), 
		 strip_TerminationAudit(TA2), 
		 fun is_TerminationAudit/1, fun chk_TerminationAudit/2).

strip_TerminationAudit(L) when is_list(L) ->
    d("strip_TerminationAudit(1) -> entry with"
      "~n   L: ~p", [L]),
    %% Drop all empty emptyDescriptor's
    F = fun({emptyDescriptors, AD}) ->
		case AD of
		    #'AuditDescriptor'{auditToken         = asn1_NOVALUE,
				       auditPropertyToken = asn1_NOVALUE} ->
			true;
		    _ ->
			false
		end;
	   (_) ->
		false
	end,
    lists:dropwhile(F, L);
strip_TerminationAudit(asn1_NOVALUE) ->
    d("strip_TerminationAudit(2) -> entry"),
    [];
strip_TerminationAudit(X) ->
    d("strip_TerminationAudit(3) -> entry with"
      "~n   X: ~p", [X]),
    X.

chk_TerminationAudit(TA1, TA2) ->
    d("chk_TerminationAudit -> entry with"
      "~n   TA1: ~p"
      "~n   TA2: ~p", [TA1, TA2]),
    do_chk_TerminationAudit(strip_TerminationAudit(TA1), 
			    strip_TerminationAudit(TA2)).

do_chk_TerminationAudit([], []) ->
    d("do_chk_TerminationAudit(1) -> entry"),
    ok;
do_chk_TerminationAudit([], asn1_NOVALUE) ->
    d("do_chk_TerminationAudit(2) -> entry"),
    ok;
do_chk_TerminationAudit(asn1_NOVALUE, []) ->
    d("do_chk_TerminationAudit(3) -> entry"),
    ok;
do_chk_TerminationAudit([] = TA1, TA2) ->
    d("do_chk_TerminationAudit(4) -> entry with"
      "~n   TA2: ~p", [TA2]),
    not_equal('TerminationAudit', TA1, TA2);
do_chk_TerminationAudit(TA1, [] = TA2) ->
    d("do_chk_TerminationAudit(5) -> entry with"
      "~n   TA1: ~p", [TA1]),
    not_equal('TerminationAudit', TA1, TA2);
do_chk_TerminationAudit([H|T1], [H|T2]) ->
    d("do_chk_TerminationAudit(6) -> entry with"
      "~n   H: ~p", [H]),
    case is_AuditReturnParameter(H) of
	true ->
	    do_chk_TerminationAudit(T1, T2);
	false ->
	    wrong_type('TerminationAudit', H)
    end;
do_chk_TerminationAudit([H1|T1], [H2|T2]) ->
    d("do_chk_TerminationAudit(7) -> entry with"
      "~n   H1: ~p"
      "~n   H2: ~p", [H1, H2]),
    chk_AuditReturnParameter(H1, H2), 
    do_chk_TerminationAudit(T1, T2);
do_chk_TerminationAudit(TA1, TA2) ->
    d("do_chk_TerminationAudit(8) -> entry with"
      "~n   TA1: ~p"
      "~n   TA2: ~p", [TA1, TA2]),
    not_equal('TerminationAudit', TA1, TA2).


%% -- AuditReturnParameter --

is_AuditReturnParameter({Tag, Val}) ->
    is_AuditReturnParameter_tag(Tag) andalso 
	is_AuditReturnParameter_val(Tag, Val);
is_AuditReturnParameter(_) ->
    false.

is_AuditReturnParameter_tag(Tag) ->
    Tags = [errorDescriptor, 
	    mediaDescriptor, 
	    modemDescriptor, 
	    muxDescriptor, 
	    eventsDescriptor, 
	    eventBufferDescriptor, 
	    signalsDescriptor, 
	    digitMapDescriptor, 
	    observedEventsDescriptor, 
	    statisticsDescriptor, 
	    packagesDescriptor, 
	    emptyDescriptors],
    lists:member(Tag, Tags).

is_AuditReturnParameter_val(errorDescriptor, V) ->
    is_ErrorDescriptor(V); 
is_AuditReturnParameter_val(mediaDescriptor, V) ->
    is_MediaDescriptor(V);
is_AuditReturnParameter_val(modemDescriptor, V) ->
    is_ModemDescriptor(V);
is_AuditReturnParameter_val(muxDescriptor, V) ->
    is_MuxDescriptor(V);
is_AuditReturnParameter_val(eventsDescriptor, V) ->
    is_EventsDescriptor(V);
is_AuditReturnParameter_val(eventBufferDescriptor, V) ->
    is_EventBufferDescriptor(V);
is_AuditReturnParameter_val(signalsDescriptor, V) ->
    is_SignalsDescriptor(V);
is_AuditReturnParameter_val(digitMapDescriptor, V) ->
    is_DigitMapDescriptor(V);
is_AuditReturnParameter_val(observedEventsDescriptor, V) ->
    is_ObservedEventsDescriptor(V);
is_AuditReturnParameter_val(statisticsDescriptor, V) ->
    is_StatisticsDescriptor(V);
is_AuditReturnParameter_val(packagesDescriptor, V) ->
    is_PackagesDescriptor(V);
is_AuditReturnParameter_val(emptyDescriptors, V) ->
    is_AuditDescriptor(V).

chk_AuditReturnParameter(ARP, ARP) ->
    chk_type(fun is_AuditReturnParameter/1, 'AuditReturnParameter', ARP);
chk_AuditReturnParameter({Tag, Val1} = ARP1, {Tag, Val2} = ARP2) ->
    case (is_AuditReturnParameter_tag(Tag) andalso 
	  is_AuditReturnParameter_val(Tag, Val1) andalso
	  is_AuditReturnParameter_val(Tag, Val2)) of
	true ->
	    chk_AuditReturnParameter_val(Tag, Val1, Val2);
	false ->
	    wrong_type('AuditReturnParameter', ARP1, ARP2)
    end;
chk_AuditReturnParameter({Tag1, Val1} = ARP1, {Tag2, Val2} = ARP2) ->
    case ((is_AuditReturnParameter_tag(Tag1) andalso 
	   is_AuditReturnParameter_val(Tag1, Val1)) andalso 
	  (is_AuditReturnParameter_tag(Tag2) andalso 
	   is_AuditReturnParameter_val(Tag2, Val2))) of
	true ->
	    not_equal('AuditReturnParameter', ARP1, ARP2);
	false ->
	    wrong_type('AuditReturnParameter', ARP1, ARP2)
    end;
chk_AuditReturnParameter(ARP1, ARP2) ->
    wrong_type('AuditReturnParameter', ARP1, ARP2).

chk_AuditReturnParameter_val(errorDescriptor, V1, V2) ->
    validate(fun() -> chk_ErrorDescriptor(V1, V2) end, 
	     'AuditReturnParameter'); 
chk_AuditReturnParameter_val(mediaDescriptor, V1, V2) ->
    validate(fun() -> chk_MediaDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(modemDescriptor, V1, V2) ->
    validate(fun() -> chk_ModemDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(muxDescriptor, V1, V2) ->
    validate(fun() -> chk_MuxDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(eventsDescriptor, V1, V2) ->
    validate(fun() -> chk_EventsDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(eventBufferDescriptor, V1, V2) ->
    validate(fun() -> chk_EventBufferDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(signalsDescriptor, V1, V2) ->
    validate(fun() -> chk_SignalsDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(digitMapDescriptor, V1, V2) ->
    validate(fun() -> chk_DigitMapDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(observedEventsDescriptor, V1, V2) ->
    validate(fun() -> chk_ObservedEventsDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(statisticsDescriptor, V1, V2) ->
    validate(fun() -> chk_StatisticsDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(packagesDescriptor, V1, V2) ->
    validate(fun() -> chk_PackagesDescriptor(V1, V2) end, 
	     'AuditReturnParameter');
chk_AuditReturnParameter_val(emptyDescriptors, V1, V2) ->
    validate(fun() -> chk_AuditDescriptor(V1, V2) end, 
	     'AuditReturnParameter').


%% -- AuditDescriptor --

is_opt_AuditDescriptor(asn1_NOVALUE) ->
    true;
is_opt_AuditDescriptor(V) ->
    is_AuditDescriptor(V).

is_AuditDescriptor(#'AuditDescriptor'{auditToken         = AT,
				      auditPropertyToken = APT}) ->
    d("is_AuditDescriptor -> entry with"
      "~n   AT:  ~p"
      "~n   APT: ~p", [AT, APT]),
    is_AuditDescriptor_auditToken(AT) andalso
	is_AuditDescriptor_auditPropertyToken(APT);
is_AuditDescriptor(_) ->
    false.

is_AuditDescriptor_auditToken(asn1_NOVALUE) ->
    true;
is_AuditDescriptor_auditToken([]) ->
    true;
is_AuditDescriptor_auditToken([H|T]) ->
    is_AuditDescriptor_auditToken_val(H) andalso 
	is_AuditDescriptor_auditToken(T);
is_AuditDescriptor_auditToken(_) ->
    false.

is_AuditDescriptor_auditToken_val(V) ->
    Toks = [muxToken, modemToken, mediaToken, eventsToken, signalsToken, 
	    digitMapToken, statsToken, observedEventsToken, 
	    packagesToken, eventBufferToken],
    lists:member(V, Toks).

is_AuditDescriptor_auditPropertyToken(asn1_NOVALUE) ->
    true;
is_AuditDescriptor_auditPropertyToken([]) ->
    true;
is_AuditDescriptor_auditPropertyToken([H|T]) ->
    d("is_AuditDescriptor_auditPropertyToken -> entry with"
      "~n   H: ~p", [H]),
    is_IndAuditParameter(H) andalso is_AuditDescriptor_auditPropertyToken(T);
is_AuditDescriptor_auditPropertyToken(_) ->
    false.

chk_opt_AuditDescriptor(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_AuditDescriptor(AD1, AD2) ->
    chk_AuditDescriptor(AD1, AD2).

chk_AuditDescriptor(AD, AD) ->
    chk_type(fun is_AuditDescriptor/1, 'AuditDescriptor', AD);
chk_AuditDescriptor(#'AuditDescriptor'{auditToken         = AT1,
				       auditPropertyToken = APT1},
		    #'AuditDescriptor'{auditToken         = AT2,
				       auditPropertyToken = APT2}) ->
    chk_AuditDescriptor_auditToken(maybe_sort(AT1), maybe_sort(AT2)),
    chk_AuditDescriptor_auditPropertyToken(APT1, APT2),
    ok;
chk_AuditDescriptor(AD1, AD2) ->
    wrong_type('AuditDescriptor', AD1, AD2).

chk_AuditDescriptor_auditToken(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_AuditDescriptor_auditToken([], []) ->
    ok;
chk_AuditDescriptor_auditToken([] = AT1, AT2) ->
    not_equal('AuditDescriptor_auditToken', AT1, AT2);
chk_AuditDescriptor_auditToken(AT1, [] = AT2) ->
    not_equal('AuditDescriptor_auditToken', AT1, AT2);
chk_AuditDescriptor_auditToken([H|T1], [H|T2]) ->
    case is_AuditDescriptor_auditToken_val(H) of
	true ->
	    chk_AuditDescriptor_auditToken(T1, T2);
	false ->
	    wrong_type('AuditDescriptor_auditToken_val', H)
    end;
chk_AuditDescriptor_auditToken([H1|_T1], [H2|_T2]) ->
    case (is_AuditDescriptor_auditToken_val(H1) andalso 
	  is_AuditDescriptor_auditToken_val(H2)) of
	true ->
	    not_equal('AuditDescriptor_auditToken_val', H1, H2);
	false ->
	    wrong_type('AuditDescriptor_auditToken_val', H1, H2)
    end;
chk_AuditDescriptor_auditToken(AT1, AT2) ->
    wrong_type('AuditDescriptor_auditToken', AT1, AT2).

chk_AuditDescriptor_auditPropertyToken(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_AuditDescriptor_auditPropertyToken([], []) ->
    ok;
chk_AuditDescriptor_auditPropertyToken([] = AT1, AT2) ->
    not_equal('AuditDescriptor_auditPropertyToken', AT1, AT2);
chk_AuditDescriptor_auditPropertyToken(AT1, [] = AT2) ->
    not_equal('AuditDescriptor_auditPropertyToken', AT1, AT2);
chk_AuditDescriptor_auditPropertyToken([H|T1], [H|T2]) ->
    case is_IndAuditParameter(H) of
	true ->
	    chk_AuditDescriptor_auditPropertyToken(T1, T2);
	false ->
	    wrong_type('AuditDescriptor_auditPropertyToken_val', H)
    end;
chk_AuditDescriptor_auditPropertyToken([H1|_], [H2|_]) ->
    chk_IndAuditParameter(H1, H2), 
    not_equal('AuditDescriptor_auditPropertyToken_val', H1, H2);
chk_AuditDescriptor_auditPropertyToken(AT1, AT2) ->
    wrong_type('AuditDescriptor_auditPropertyToken', AT1, AT2).


%% -- IndAuditParameter --

is_IndAuditParameter({Tag, Val}) ->
    is_IndAuditParameter_tag(Tag) andalso is_IndAuditParameter_val(Tag, Val);
is_IndAuditParameter(_) ->
    false.

is_IndAuditParameter_tag(Tag) ->
    Tags = [indAudMediaDescriptor, 
	    indAudEventsDescriptor, 
	    indAudEventBufferDescriptor, 
	    indAudSignalsDescriptor, 
	    indAudDigitMapDescriptor, 
	    indAudStatisticsDescriptor, 
	    indAudPackagesDescriptor],
    lists:member(Tag, Tags).
    
is_IndAuditParameter_val(indAudMediaDescriptor, Val) ->
    is_IndAudMediaDescriptor(Val);
is_IndAuditParameter_val(indAudEventsDescriptor, Val) ->
    is_IndAudEventsDescriptor(Val);
is_IndAuditParameter_val(indAudEventBufferDescriptor, Val) ->
    is_IndAudEventBufferDescriptor(Val); 
is_IndAuditParameter_val(indAudSignalsDescriptor, Val) ->
    is_IndAudSignalsDescriptor(Val);
is_IndAuditParameter_val(indAudDigitMapDescriptor, Val) ->
    is_IndAudDigitMapDescriptor(Val);
is_IndAuditParameter_val(indAudStatisticsDescriptor, Val) ->
    is_IndAudStatisticsDescriptor(Val);
is_IndAuditParameter_val(indAudPackagesDescriptor, Val) ->
    is_IndAudPackagesDescriptor(Val).

chk_IndAuditParameter(IAP, IAP) ->
    chk_type(fun is_IndAuditParameter/1, 'IndAuditParameter', IAP);
chk_IndAuditParameter({Tag, Val1} = IAP1, {Tag, Val2} = IAP2) ->
    case (is_IndAuditParameter_tag(Tag) andalso
	  is_IndAuditParameter_val(Tag, Val1) andalso
	  is_IndAuditParameter_val(Tag, Val2)) of
	true ->
	    chk_IndAuditParameter_val(Tag, Val1, Val2);
	false ->
	    wrong_type('IndAuditParameter', IAP1, IAP2)
    end;
chk_IndAuditParameter({Tag1, Val1} = IAP1, {Tag2, Val2} = IAP2) ->
    case ((is_IndAuditParameter_tag(Tag1) andalso
	   is_IndAuditParameter_val(Tag1, Val1)) andalso
	  (is_IndAuditParameter_tag(Tag2) andalso
	   is_IndAuditParameter_val(Tag2, Val2))) of
	true ->
	    not_equal('IndAuditParameter', IAP1, IAP2);
	false ->
	    wrong_type('IndAuditParameter', IAP1, IAP2)
    end;
chk_IndAuditParameter(IAP1, IAP2) ->
    wrong_type('IndAuditParameter', IAP1, IAP2).

chk_IndAuditParameter_val(indAudMediaDescriptor, Val1, Val2) ->
    validate(fun() -> chk_IndAudMediaDescriptor(Val1, Val2) end,
	     'IndAuditParameter');
chk_IndAuditParameter_val(indAudEventsDescriptor, Val1, Val2) ->
    validate(fun() -> chk_IndAudEventsDescriptor(Val1, Val2) end,
	     'IndAuditParameter');
chk_IndAuditParameter_val(indAudEventBufferDescriptor, Val1, Val2) ->
    validate(fun() -> chk_IndAudEventBufferDescriptor(Val1, Val2) end,
	     'IndAuditParameter'); 
chk_IndAuditParameter_val(indAudSignalsDescriptor, Val1, Val2) ->
    validate(fun() -> chk_IndAudSignalsDescriptor(Val1, Val2) end,
	     'IndAuditParameter');
chk_IndAuditParameter_val(indAudDigitMapDescriptor, Val1, Val2) ->
    validate(fun() -> chk_IndAudDigitMapDescriptor(Val1, Val2) end,
	     'IndAuditParameter');
chk_IndAuditParameter_val(indAudStatisticsDescriptor, Val1, Val2) ->
    validate(fun() -> chk_IndAudStatisticsDescriptor(Val1, Val2) end,
	     'IndAuditParameter');
chk_IndAuditParameter_val(indAudPackagesDescriptor, Val1, Val2) ->
    validate(fun() -> chk_IndAudPackagesDescriptor(Val1, Val2) end,
	     'IndAuditParameter').


%% -- IndAudMediaDescriptor --

is_IndAudMediaDescriptor(#'IndAudMediaDescriptor'{termStateDescr = TSD,
						  streams        = S}) ->
    d("is_IndAudMediaDescriptor -> entry with"
      "~n   TSD: ~p"
      "~n   S:   ~p", [TSD, S]),
    is_opt_IndAudTerminationStateDescriptor(TSD) andalso
	is_IndAudMediaDescriptor_streams(S);
is_IndAudMediaDescriptor(_) ->
    false.

is_IndAudMediaDescriptor_streams(asn1_NOVALUE) ->
    true;
is_IndAudMediaDescriptor_streams({Tag, Val}) ->
    d("is_IndAudMediaDescriptor_streams -> entry with"
      "~n   Tag: ~p"
      "~n   Val: ~p", [Tag, Val]),
    is_IndAudMediaDescriptor_streams_tag(Tag) andalso
	is_IndAudMediaDescriptor_streams_val(Tag, Val);
is_IndAudMediaDescriptor_streams(_) ->
    false.

is_IndAudMediaDescriptor_streams_tag(Tag) ->
    Tags = [oneStream, multiStream],
    lists:member(Tag, Tags).

is_IndAudMediaDescriptor_streams_val(oneStream, Val) ->
    d("is_IndAudMediaDescriptor_streams_val(oneStream) -> entry with"
      "~n   Val: ~p", [Val]),
    is_IndAudStreamParms(Val);
is_IndAudMediaDescriptor_streams_val(multiStream, Val) ->
    d("is_IndAudMediaDescriptor_streams_val(multiStream) -> entry with"
      "~n   Val: ~p", [Val]),
    is_IndAudMediaDescriptor_multiStream(Val).

is_IndAudMediaDescriptor_multiStream([]) ->
    true;
is_IndAudMediaDescriptor_multiStream([H|T]) ->
    d("is_IndAudMediaDescriptor_multiStream -> entry with"
      "~n   H: ~p", [H]),
    is_IndAudStreamDescriptor(H) andalso 
	is_IndAudMediaDescriptor_multiStream(T);
is_IndAudMediaDescriptor_multiStream(_) ->
    false.

chk_IndAudMediaDescriptor(IAMD, IAMD) ->
    chk_type(fun is_IndAudMediaDescriptor/1, 'IndAudMediaDescriptor', IAMD);
chk_IndAudMediaDescriptor(#'IndAudMediaDescriptor'{termStateDescr = TSD1,
						   streams        = S1},
			  #'IndAudMediaDescriptor'{termStateDescr = TSD2,
						   streams        = S2}) ->
    validate(fun() -> chk_opt_IndAudTerminationStateDescriptor(TSD1, TSD2) end,
	     'IndAudMediaDescriptor'),
    validate(fun() -> chk_IndAudMediaDescriptor_streams(S1, S2) end,
	     'IndAudMediaDescriptor'),
    ok;
chk_IndAudMediaDescriptor(IAMD1, IAMD2) ->
    wrong_type('IndAudMediaDescriptor', IAMD1, IAMD2).
		      
chk_IndAudMediaDescriptor_streams(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_IndAudMediaDescriptor_streams({Tag, Val1} = S1, 
					     {Tag, Val2} = S2) ->
    case (is_IndAudMediaDescriptor_streams_tag(Tag) andalso
	  is_IndAudMediaDescriptor_streams_val(Tag, Val1) andalso
	  is_IndAudMediaDescriptor_streams_val(Tag, Val2)) of
	true ->
	    chk_IndAudMediaDescriptor_streams_val(Tag, Val1, Val2);
	false ->
	    wrong_type('IndAudMediaDescriptor_streams', S1, S2)
    end;
chk_IndAudMediaDescriptor_streams({Tag1, Val1} = S1, 
					     {Tag2, Val2} = S2) ->
    case ((is_IndAudMediaDescriptor_streams_tag(Tag1) andalso
	   is_IndAudMediaDescriptor_streams_val(Tag1, Val1)) andalso
	  (is_IndAudMediaDescriptor_streams_tag(Tag2) andalso
	   is_IndAudMediaDescriptor_streams_val(Tag2, Val2))) of
	true ->
	    not_equal('IndAudMediaDescriptor_streams', S1, S2);
	false ->
	    wrong_type('IndAudMediaDescriptor_streams', S1, S2)
    end;
chk_IndAudMediaDescriptor_streams(S1, S2) ->
    wrong_type('IndAudMediaDescriptor_streams', S1, S2).

chk_IndAudMediaDescriptor_streams_val(oneStream, Val1, Val2) ->
    validate(fun() -> chk_IndAudStreamParms(Val1, Val2) end,
	     'IndAudMediaDescriptor_streams');
chk_IndAudMediaDescriptor_streams_val(multiStream, Val1, Val2) ->
    validate(fun() -> chk_IndAudMediaDescriptor_multiStream(Val1, Val2) end,
	     'IndAudMediaDescriptor_streams').

chk_IndAudMediaDescriptor_multiStream([], []) ->
    ok;
chk_IndAudMediaDescriptor_multiStream([] = MS1, MS2) ->
    not_equal('IndAudMediaDescriptor_multiStream', MS1, MS2);
chk_IndAudMediaDescriptor_multiStream(MS1, [] = MS2) ->
    not_equal('IndAudMediaDescriptor_multiStream', MS1, MS2);
chk_IndAudMediaDescriptor_multiStream([H|T1], [H|T2]) ->
    case is_IndAudStreamDescriptor(H) of
	true ->
	    chk_IndAudMediaDescriptor_multiStream(T1, T2);
	false ->
	    wrong_type('IndAudMediaDescriptor_multiStream_val', H)
    end;
chk_IndAudMediaDescriptor_multiStream([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_IndAudStreamDescriptor(H1, H2) end, 
	     'IndAudMediaDescriptor_multiStream_val'),
    chk_IndAudMediaDescriptor_multiStream(T1, T2);
chk_IndAudMediaDescriptor_multiStream(MS1, MS2) ->
    wrong_type('IndAudMediaDescriptor_multiStream', MS1, MS2).


%% -- IndAudStreamDescriptor --

is_IndAudStreamDescriptor(#'IndAudStreamDescriptor'{streamID    = SID,
						    streamParms = Parms}) ->
    d("is_IndAudStreamDescriptor -> entry with"
      "~n   SID:   ~p"
      "~n   Parms: ~p", [SID, Parms]),
    is_StreamID(SID) andalso is_IndAudStreamParms(Parms);
is_IndAudStreamDescriptor(_) ->
    false.

chk_IndAudStreamDescriptor(D, D) ->
    chk_type(fun is_IndAudStreamDescriptor/1, 'IndAudStreamDescriptor', D);
chk_IndAudStreamDescriptor(#'IndAudStreamDescriptor'{streamID    = SID1,
						     streamParms = Parms1},
			   #'IndAudStreamDescriptor'{streamID    = SID2,
						     streamParms = Parms2}) ->
    validate(fun() -> chk_StreamID(SID1, SID2) end, 'IndAudStreamDescriptor'),
    validate(fun() -> chk_IndAudStreamParms(Parms1, Parms2) end, 
	     'IndAudStreamDescriptor'),
    ok;
chk_IndAudStreamDescriptor(D1, D2) ->
    wrong_type('IndAudStreamDescriptor', D1, D2).


%% -- IndAudStreamParms --

is_IndAudStreamParms(#'IndAudStreamParms'{localControlDescriptor = LCD,
					  localDescriptor        = LD,
					  remoteDescriptor       = RD}) ->
    d("is_IndAudStreamParms -> entry with"
      "~n   LCD: ~p"
      "~n   LD:  ~p"
      "~n   RD:  ~p", [LCD, LD, RD]),
    is_opt_IndAudLocalControlDescriptor(LCD) andalso 
	is_opt_IndAudLocalRemoteDescriptor(LD) andalso 
	is_opt_IndAudLocalRemoteDescriptor(RD);
is_IndAudStreamParms(_) ->
    false.

chk_IndAudStreamParms(#'IndAudStreamParms'{localControlDescriptor = LCD1,
					   localDescriptor        = LD1,
					   remoteDescriptor       = RD1},
		      #'IndAudStreamParms'{localControlDescriptor = LCD2,
					   localDescriptor        = LD2,
					   remoteDescriptor       = RD2}) ->
    validate(fun() -> chk_opt_IndAudLocalControlDescriptor(LCD1, LCD2) end,
	     'IndAudStreamParms'),
    validate(fun() -> chk_opt_IndAudLocalRemoteDescriptor(LD1, LD2) end,
	     'IndAudStreamParms'),
    validate(fun() -> chk_opt_IndAudLocalRemoteDescriptor(RD1, RD2) end,
	     'IndAudStreamParms'),
    ok;
chk_IndAudStreamParms(D1, D2) ->
    wrong_type('IndAudStreamParms', D1, D2).

		     
%% -- IndAudLocalControlDescriptor --

is_opt_IndAudLocalControlDescriptor(asn1_NOVALUE) ->
    true;
is_opt_IndAudLocalControlDescriptor(D) ->
    is_IndAudLocalControlDescriptor(D).

is_IndAudLocalControlDescriptor(
  #'IndAudLocalControlDescriptor'{streamMode    = SM,
				  reserveValue  = RV,
				  reserveGroup  = RG,
				  propertyParms = PPs,
				  streamModeSel = SMS}) ->
    d("is_IndAudLocalControlDescriptor -> entry with"
      "~n   SM:  ~p"
      "~n   RV:  ~p"
      "~n   RG:  ~p"
      "~n   PPs: ~p"
      "~n   SMS: ~p", [SM, RV, RG, PPs, SMS]),
    is_opt_NULL(SM) andalso 
	is_opt_NULL(RV) andalso 
	is_opt_NULL(RG) andalso 
	is_IndAudLocalControlDescriptor_propertyParms(PPs) andalso
	is_opt_StreamMode(SMS);
is_IndAudLocalControlDescriptor(_) ->
    false.

is_IndAudLocalControlDescriptor_propertyParms(asn1_NOVALUE) ->
    true;
is_IndAudLocalControlDescriptor_propertyParms([]) ->
    true;
is_IndAudLocalControlDescriptor_propertyParms([H|T]) ->
    is_IndAudPropertyParm(H) andalso 
	is_IndAudLocalControlDescriptor_propertyParms(T);
is_IndAudLocalControlDescriptor_propertyParms(_) ->
    false.

chk_opt_IndAudLocalControlDescriptor(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_IndAudLocalControlDescriptor(
  #'IndAudLocalControlDescriptor'{streamMode    = SM1,
				  reserveValue  = RV1,
				  reserveGroup  = RG1,
				  propertyParms = PPs1,
				  streamModeSel = SMS1},
  #'IndAudLocalControlDescriptor'{streamMode    = SM2,
				  reserveValue  = RV2,
				  reserveGroup  = RG2,
				  propertyParms = PPs2,
				  streamModeSel = SMS2}) ->
    chk_IndAudLocalControlDescriptor_streamMode(SM1, SM2),
    chk_IndAudLocalControlDescriptor_reserveValue(RV1, RV2), 
    chk_IndAudLocalControlDescriptor_reserveGroup(RG1, RG2), 
    chk_IndAudLocalControlDescriptor_propertyParms(PPs1, PPs2),
    chk_IndAudLocalControlDescriptor_streamModeSel(SMS1, SMS2), 
    ok;
chk_opt_IndAudLocalControlDescriptor(D1, D2) ->
    wrong_type('IndAudLocalControlDescriptor', D1, D2).

chk_IndAudLocalControlDescriptor_streamMode(SM1, SM2) ->
    validate(fun() -> chk_opt_NULL(SM1, SM2) end, 
	     'IndAudLocalControlDescriptor_streamMode').

chk_IndAudLocalControlDescriptor_reserveValue(RV1, RV2) ->
    validate(fun() -> chk_opt_NULL(RV1, RV2) end, 
	     'IndAudLocalControlDescriptor_reserveValue').

chk_IndAudLocalControlDescriptor_reserveGroup(RG1, RG2) ->
    validate(fun() -> chk_opt_NULL(RG1, RG2) end, 
	     'IndAudLocalControlDescriptor_reserveGroup').

chk_IndAudLocalControlDescriptor_propertyParms(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_IndAudLocalControlDescriptor_propertyParms([], []) ->
    ok;
chk_IndAudLocalControlDescriptor_propertyParms([] = PPs1, PPs2) ->
    not_equal('IndAudLocalControlDescriptor_propertyParms', PPs1, PPs2);
chk_IndAudLocalControlDescriptor_propertyParms(PPs1, [] = PPs2) ->
    not_equal('IndAudLocalControlDescriptor_propertyParms', PPs1, PPs2);
chk_IndAudLocalControlDescriptor_propertyParms([H|T1], [H|T2]) ->
    case is_IndAudPropertyParm(H) of
	true ->
	    chk_IndAudLocalControlDescriptor_propertyParms(T1, T2);
	false ->
	    wrong_type('IndAudLocalControlDescriptor_propertyParms_val', H)
    end;
chk_IndAudLocalControlDescriptor_propertyParms([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_IndAudPropertyParm(H1, H2) end,
	     'IndAudLocalControlDescriptor_propertyParms_val'),
    chk_IndAudLocalControlDescriptor_propertyParms(T1, T2);
chk_IndAudLocalControlDescriptor_propertyParms(PPs1, PPs2) ->
    wrong_type('IndAudLocalControlDescriptor_propertyParms', PPs1, PPs2).

chk_IndAudLocalControlDescriptor_streamModeSel(SMS1, SMS2) ->
    validate(fun() -> chk_opt_StreamMode(SMS1, SMS2) end, 
	     'IndAudLocalControlDescriptor_streamModeSel').


%% -- IndAudPropertyParm --

is_IndAudPropertyParm(#'IndAudPropertyParm'{name          = Name,
					    propertyParms = PP}) ->
    d("is_IndAudPropertyParm -> entry"),
    is_PkgdName(Name) andalso is_opt_PropertyParm(PP);
is_IndAudPropertyParm(_) ->
    false.

chk_IndAudPropertyParm(#'IndAudPropertyParm'{name          = Name1,
					     propertyParms = PP1},
		       #'IndAudPropertyParm'{name          = Name2,
					     propertyParms = PP2}) ->
    validate(fun() -> chk_PkgdName(Name1, Name2) end, 'IndAudPropertyParm'),
    validate(fun() -> chk_opt_PropertyParm(PP1, PP2) end, 
	     'IndAudPropertyParm'),
    ok;
chk_IndAudPropertyParm(P1, P2) ->
    wrong_type('IndAudPropertyParm', P1, P2).


%% -- IndAudLocalRemoteDescriptor --

is_opt_IndAudLocalRemoteDescriptor(asn1_NOVALUE) ->
    true;
is_opt_IndAudLocalRemoteDescriptor(D) ->
    is_IndAudLocalRemoteDescriptor(D).

is_IndAudLocalRemoteDescriptor(
  #'IndAudLocalRemoteDescriptor'{propGroupID = ID,
				 propGrps    = Grps}) ->
    is_IndAudLocalRemoteDescriptor_propGroupID(ID) andalso
	is_IndAudPropertyGroup(Grps);
is_IndAudLocalRemoteDescriptor(_) ->
    false.

is_IndAudLocalRemoteDescriptor_propGroupID(asn1_NOVALUE) ->
    true;
is_IndAudLocalRemoteDescriptor_propGroupID(V) -> 
    is_INTEGER(V, {range, 0, 65535}).

chk_opt_IndAudLocalRemoteDescriptor(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_IndAudLocalRemoteDescriptor(D1, D2) ->
    chk_IndAudLocalRemoteDescriptor(D1, D2).

chk_IndAudLocalRemoteDescriptor(D, D) ->
    chk_type(fun is_IndAudLocalRemoteDescriptor/1, 
	     'IndAudLocalRemoteDescriptor', D);
chk_IndAudLocalRemoteDescriptor(
  #'IndAudLocalRemoteDescriptor'{propGroupID = ID1,
				 propGrps    = Grps1},
  #'IndAudLocalRemoteDescriptor'{propGroupID = ID2,
				 propGrps    = Grps2}) ->
    chk_IndAudLocalRemoteDescriptor_propGroupID(ID1, ID2),
    chk_IndAudPropertyGroup(Grps1, Grps2),
    ok;
chk_IndAudLocalRemoteDescriptor(D1, D2) ->
    wrong_type('IndAudLocalRemoteDescriptor', D1, D2).

chk_IndAudLocalRemoteDescriptor_propGroupID(ID, ID) ->
    chk_type(fun is_IndAudLocalRemoteDescriptor_propGroupID/1, 
	     'IndAudLocalRemoteDescriptor_propGroupID', ID);
chk_IndAudLocalRemoteDescriptor_propGroupID(ID1, ID2) ->
    case (is_IndAudLocalRemoteDescriptor_propGroupID(ID1) andalso
	  is_IndAudLocalRemoteDescriptor_propGroupID(ID2)) of
	true ->
	    not_equal('IndAudLocalRemoteDescriptor_propGroupID', ID1, ID2);
	false ->
	    wrong_type('IndAudLocalRemoteDescriptor_propGroupID', ID1, ID2)
    end.


%% -- IndAudPropertyGroup --

is_IndAudPropertyGroup([]) ->
    true;
is_IndAudPropertyGroup([H|T]) ->
    is_IndAudPropertyParm(H) andalso is_IndAudPropertyGroup(T);
is_IndAudPropertyGroup(_) ->
    false.

chk_IndAudPropertyGroup([], []) ->
    ok;
chk_IndAudPropertyGroup([] = PG1, PG2) ->
    not_equal('IndAudPropertyGroup', PG1, PG2);
chk_IndAudPropertyGroup(PG1, [] = PG2) ->
    not_equal('IndAudPropertyGroup', PG1, PG2);
chk_IndAudPropertyGroup([H|T1], [H|T2]) ->
    case is_IndAudPropertyParm(H) of
	true ->
	    chk_IndAudPropertyGroup(T1, T2);
	false ->
	    wrong_type('IndAudPropertyGroup_val', H)
    end;
chk_IndAudPropertyGroup([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_IndAudPropertyParm(H1, H2) end,
	     'IndAudPropertyGroup_val'),
    chk_IndAudPropertyGroup(T1, T2);
chk_IndAudPropertyGroup(P1, P2) ->
    wrong_type('IndAudPropertyGroup', P1, P2).


%% -- IndAudTerminationStateDescriptor --

is_opt_IndAudTerminationStateDescriptor(asn1_NOVALUE) ->
    true;
is_opt_IndAudTerminationStateDescriptor(D) ->
    is_IndAudTerminationStateDescriptor(D).

is_IndAudTerminationStateDescriptor(
  #'IndAudTerminationStateDescriptor'{propertyParms      = Parms,
				      eventBufferControl = EBC,
				      serviceState       = SS,
				      serviceStateSel    = SSS}) ->
    is_IndAudTerminationStateDescriptor_propertyParms(Parms) andalso
	is_opt_NULL(EBC) andalso 
	is_opt_NULL(SS) andalso 
	is_opt_ServiceState(SSS);
is_IndAudTerminationStateDescriptor(_) ->
    false.

is_IndAudTerminationStateDescriptor_propertyParms([]) ->
    true;
is_IndAudTerminationStateDescriptor_propertyParms([H|T]) ->
    is_IndAudPropertyParm(H) andalso 
	is_IndAudTerminationStateDescriptor_propertyParms(T);
is_IndAudTerminationStateDescriptor_propertyParms(_) ->
    false.

chk_opt_IndAudTerminationStateDescriptor(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_IndAudTerminationStateDescriptor(D1, D2) ->
    chk_IndAudTerminationStateDescriptor(D1, D2).

chk_IndAudTerminationStateDescriptor(
  #'IndAudTerminationStateDescriptor'{propertyParms      = Parms1,
				      eventBufferControl = EBC1,
				      serviceState       = SS1,
				      serviceStateSel    = SSS1},
  #'IndAudTerminationStateDescriptor'{propertyParms      = Parms2,
				      eventBufferControl = EBC2,
				      serviceState       = SS2,
				      serviceStateSel    = SSS2}) ->
    chk_IndAudTerminationStateDescriptor_propertyParms(Parms1, Parms2),
    chk_IndAudTerminationStateDescriptor_eventBufferControl(EBC1, EBC2), 
    chk_IndAudTerminationStateDescriptor_serviceState(SS1, SS2), 
    chk_IndAudTerminationStateDescriptor_serviceStateSel(SSS1, SSS2),
    ok;
chk_IndAudTerminationStateDescriptor(D1, D2) ->
    wrong_type('IndAudTerminationStateDescriptor', D1, D2).

chk_IndAudTerminationStateDescriptor_propertyParms([], []) ->	      
    ok;
chk_IndAudTerminationStateDescriptor_propertyParms([] = PP1, PP2) ->
    not_equal('IndAudTerminationStateDescriptor_propertyParms', PP1, PP2);
chk_IndAudTerminationStateDescriptor_propertyParms(PP1, [] = PP2) ->
    not_equal('IndAudTerminationStateDescriptor_propertyParms', PP1, PP2);
chk_IndAudTerminationStateDescriptor_propertyParms([H|T1], [H|T2]) ->
    case is_IndAudPropertyParm(H) of
	true ->
	    chk_IndAudTerminationStateDescriptor_propertyParms(T1, T2);
	false ->
	    wrong_type('IndAudTerminationStateDescriptor_propertyParms', H)
    end;
chk_IndAudTerminationStateDescriptor_propertyParms([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_IndAudPropertyParm(H1, H2) end, 
	     'IndAudTerminationStateDescriptor_propertyParms'),
    chk_IndAudTerminationStateDescriptor_propertyParms(T1, T2);
chk_IndAudTerminationStateDescriptor_propertyParms(PP1, PP2) ->
    wrong_type('IndAudTerminationStateDescriptor_propertyParms', PP1, PP2).

chk_IndAudTerminationStateDescriptor_eventBufferControl(EBC1, EBC2) ->
    validate(fun() -> chk_opt_NULL(EBC1, EBC2) end, 
	     'IndAudTerminationStateDescriptor_eventBufferControl').

chk_IndAudTerminationStateDescriptor_serviceState(SS1, SS2) ->
    validate(fun() -> chk_opt_NULL(SS1, SS2) end, 
	     'IndAudTerminationStateDescriptor_serviceState').

chk_IndAudTerminationStateDescriptor_serviceStateSel(SSS1, SSS2) ->
    validate(fun() -> chk_opt_ServiceState(SSS1, SSS2) end, 
	     'IndAudTerminationStateDescriptor_serviceStateSel').

%% -- IndAudEventsDescriptor --

is_IndAudEventsDescriptor(#'IndAudEventsDescriptor'{requestID = RID,
						    pkgdName  = Name,
						    streamID  = SID}) ->
    is_opt_RequestID(RID) andalso 
	is_PkgdName(Name) andalso 
	is_opt_StreamID(SID);
is_IndAudEventsDescriptor(_) ->
    false.

chk_IndAudEventsDescriptor(#'IndAudEventsDescriptor'{requestID = RID1,
						     pkgdName  = Name1,
						     streamID  = SID1},
			   #'IndAudEventsDescriptor'{requestID = RID2,
						     pkgdName  = Name2,
						     streamID  = SID2}) ->
    chk_opt_RequestID(RID1, RID2),
    chk_PkgdName(Name1, Name2),
    chk_opt_StreamID(SID1, SID2),
    ok;
chk_IndAudEventsDescriptor(D1, D2) ->
    wrong_type('IndAudEventsDescriptor', D1, D2).


%% -- IndAudEventBufferDescriptor --

is_IndAudEventBufferDescriptor(
  #'IndAudEventBufferDescriptor'{eventName = Name,
				 streamID  = SID}) ->
    is_PkgdName(Name) andalso is_opt_StreamID(SID);
is_IndAudEventBufferDescriptor(_) ->
    false.

chk_IndAudEventBufferDescriptor(
  #'IndAudEventBufferDescriptor'{eventName = Name1,
				 streamID  = SID1},
  #'IndAudEventBufferDescriptor'{eventName = Name2,
				 streamID  = SID2}) ->
    chk_PkgdName(Name1, Name2),
    chk_opt_StreamID(SID1, SID2),
    ok;
chk_IndAudEventBufferDescriptor(D1, D2) ->
    wrong_type('IndAudEventBufferDescriptor', D1, D2).


%% -- IndAudSignalsDescriptor --

is_IndAudSignalsDescriptor({Tag, Val}) ->
    is_IndAudSignalsDescriptor_tag(Tag) andalso
	is_IndAudSignalsDescriptor_val(Tag, Val);
is_IndAudSignalsDescriptor(_) ->
    false.

is_IndAudSignalsDescriptor_tag(Tag) ->
    Tags = [signal, seqSigList],
    lists:member(Tag, Tags).

is_IndAudSignalsDescriptor_val(signal, Val) ->
    is_IndAudSignal(Val);
is_IndAudSignalsDescriptor_val(seqSigList, Val) ->
    is_IndAudSeqSigList(Val).

chk_IndAudSignalsDescriptor(D, D) ->
    chk_type(fun is_IndAudSignalsDescriptor/1, 'IndAudSignalsDescriptor', D);
chk_IndAudSignalsDescriptor({Tag, Val1} = D1, {Tag, Val2} = D2) ->
    case (is_IndAudSignalsDescriptor_tag(Tag) andalso 
	  is_IndAudSignalsDescriptor_val(Tag, Val1) andalso
	  is_IndAudSignalsDescriptor_val(Tag, Val2)) of
	true ->
	    chk_IndAudSignalsDescriptor_val(Tag, Val1, Val2);
	false ->
	    wrong_type('IndAudSignalsDescriptor', D1, D2)
    end;
chk_IndAudSignalsDescriptor({Tag1, Val1} = D1, {Tag2, Val2} = D2) ->
    case ((is_IndAudSignalsDescriptor_tag(Tag1) andalso 
	   is_IndAudSignalsDescriptor_val(Tag1, Val1)) andalso
	  (is_IndAudSignalsDescriptor_tag(Tag2) andalso 
	   is_IndAudSignalsDescriptor_val(Tag2, Val2))) of
	true ->
	    not_equal('IndAudSignalsDescriptor', D1, D2);
	false ->
	    wrong_type('IndAudSignalsDescriptor', D1, D2)
    end;
chk_IndAudSignalsDescriptor(D1, D2) ->
    wrong_type('IndAudSignalsDescriptor', D1, D2).

chk_IndAudSignalsDescriptor_val(signal, Val1, Val2) ->
    chk_IndAudSignal(Val1, Val2);
chk_IndAudSignalsDescriptor_val(seqSigList, Val1, Val2) ->
    chk_IndAudSeqSigList(Val1, Val2).


%% -- IndAudSeqSigList --

is_IndAudSeqSigList(#'IndAudSeqSigList'{id         = ID,
					signalList = SL}) ->
    is_IndAudSeqSigList_id(ID) andalso is_opt_IndAudSignal(SL);
is_IndAudSeqSigList(_) ->
    false.

is_IndAudSeqSigList_id(ID) -> is_INTEGER(ID, {range, 0, 65535}).
    
chk_IndAudSeqSigList(L, L) ->
    chk_type(fun is_IndAudSeqSigList/1, 'IndAudSeqSigList', L);
chk_IndAudSeqSigList(#'IndAudSeqSigList'{id         = ID1,
					 signalList = SL1},
		     #'IndAudSeqSigList'{id         = ID2,
					 signalList = SL2}) ->
    chk_IndAudSeqSigList_id(ID1, ID2),
    chk_opt_IndAudSignal(SL1, SL2),
    ok;
chk_IndAudSeqSigList(L1, L2) ->
    wrong_type('IndAudSeqSigList', L1, L2).

chk_IndAudSeqSigList_id(ID, ID) -> 
    chk_type(fun is_IndAudSeqSigList_id/1, 'IndAudSeqSigList_id', ID);
chk_IndAudSeqSigList_id(ID1, ID2) ->
    case (is_IndAudSeqSigList_id(ID1) andalso
	  is_IndAudSeqSigList_id(ID2)) of
	true ->
	    not_equal('IndAudSeqSigList_id', ID1, ID2);
	false ->
	    wrong_type('IndAudSeqSigList_id', ID1, ID2)
    end.


%% -- IndAudSignal -- 

is_opt_IndAudSignal(asn1_NOVALUE) ->
    true;
is_opt_IndAudSignal(V) ->
    is_IndAudSignal(V).

is_IndAudSignal(#'IndAudSignal'{signalName      = Name, 
				streamID        = SID,
				signalRequestID = SRID}) ->
    is_PkgdName(Name) andalso 
	is_opt_StreamID(SID) andalso 
	is_opt_RequestID(SRID);
is_IndAudSignal(_) ->
    false.

chk_opt_IndAudSignal(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_IndAudSignal(S1, S2) ->
    chk_IndAudSignal(S1, S2).

chk_IndAudSignal(S, S) ->
    chk_type(fun is_IndAudSignal/1, 'IndAudSignal', S);
chk_IndAudSignal(#'IndAudSignal'{signalName      = Name1, 
				 streamID        = SID1,
				 signalRequestID = SRID1},
		 #'IndAudSignal'{signalName      = Name2, 
				 streamID        = SID2,
				 signalRequestID = SRID2}) ->
    validate(fun() -> chk_PkgdName(Name1, Name2) end, 
	     'IndAudSignal_signalName'),
    validate(fun() -> chk_opt_StreamID(SID1, SID2) end, 
	     'IndAudSignal_streamID'),
    validate(fun() -> chk_opt_RequestID(SRID1, SRID2) end, 
	     'IndAudSignal_signalRequestID'),
    ok;
chk_IndAudSignal(S1, S2) ->
    wrong_type('IndAudSignal', S1, S2).


%% -- IndAudDigitMapDescriptor --

is_IndAudDigitMapDescriptor(
  #'IndAudDigitMapDescriptor'{digitMapName = Name}) ->
    is_opt_DigitMapName(Name);
is_IndAudDigitMapDescriptor(_) ->
    false.

chk_IndAudDigitMapDescriptor(D, D) ->
    chk_type(fun is_IndAudDigitMapDescriptor/1, 'IndAudDigitMapDescriptor', D);
chk_IndAudDigitMapDescriptor(
  #'IndAudDigitMapDescriptor'{digitMapName = Name1},
  #'IndAudDigitMapDescriptor'{digitMapName = Name2}) ->
    validate(fun() -> chk_opt_DigitMapName(Name1, Name2) end, 
	     'IndAudDigitMapDescriptor'),
    ok;
chk_IndAudDigitMapDescriptor(D1, D2) ->
    wrong_type('IndAudDigitMapDescriptor', D1, D2).
		      

%% -- IndAudStatisticsDescriptor --

is_IndAudStatisticsDescriptor(
  #'IndAudStatisticsDescriptor'{statName = Name}) ->
    is_PkgdName(Name);
is_IndAudStatisticsDescriptor(_) ->
    false.

chk_IndAudStatisticsDescriptor(D, D) ->
    chk_type(fun is_IndAudStatisticsDescriptor/1, 
	     'IndAudStatisticsDescriptor', D);
chk_IndAudStatisticsDescriptor(
  #'IndAudStatisticsDescriptor'{statName = Name1},
  #'IndAudStatisticsDescriptor'{statName = Name2}) ->
    validate(fun() -> chk_PkgdName(Name1, Name2) end, 
	     'IndAudStatisticsDescriptor'),
    ok;
chk_IndAudStatisticsDescriptor(D1, D2) ->
    wrong_type('IndAudStatisticsDescriptor', D1, D2).
		      

%% -- IndAudPackagesDescriptor --

is_IndAudPackagesDescriptor(
  #'IndAudPackagesDescriptor'{packageName    = Name,
			      packageVersion = Ver}) ->
    is_Name(Name) andalso is_IndAudPackagesDescriptor_packageVersion(Ver);
is_IndAudPackagesDescriptor(_) ->
    false.

is_IndAudPackagesDescriptor_packageVersion(V) ->
     is_INTEGER(V, {range, 0, 99}).

chk_IndAudPackagesDescriptor(
  #'IndAudPackagesDescriptor'{packageName    = Name1,
			      packageVersion = Ver1},
  #'IndAudPackagesDescriptor'{packageName    = Name2,
			      packageVersion = Ver2}) ->
    validate(fun() -> chk_Name(Name1, Name2) end, 'IndAudPackagesDescriptor'),
    chk_IndAudPackagesDescriptor_packageVersion(Ver1, Ver2),
    ok;
chk_IndAudPackagesDescriptor(D1, D2) ->
    wrong_type('IndAudPackagesDescriptor', D1, D2).

chk_IndAudPackagesDescriptor_packageVersion(V, V) ->
    chk_type(fun is_IndAudPackagesDescriptor_packageVersion/1, 
	     'IndAudPackagesDescriptor_packageVersion', V);
chk_IndAudPackagesDescriptor_packageVersion(V1, V2) ->
    case (is_IndAudPackagesDescriptor_packageVersion(V1) andalso
	  is_IndAudPackagesDescriptor_packageVersion(V2)) of
	true ->
	    not_equal('IndAudPackagesDescriptor_packageVersion', V1, V2);
	false ->
	    wrong_type('IndAudPackagesDescriptor_packageVersion', V1, V2)
    end.


%% -- NotifyRequest --

is_NotifyRequest(#'NotifyRequest'{terminationID            = Tids,
				  observedEventsDescriptor = OED,
				  errorDescriptor          = ED}) ->
    is_TerminationIDList(Tids) andalso 
	is_ObservedEventsDescriptor(OED) andalso
	is_opt_ErrorDescriptor(ED);
is_NotifyRequest(_) ->
    false.

chk_NotifyRequest(#'NotifyRequest'{terminationID            = Tids1,
				   observedEventsDescriptor = OED1,
				   errorDescriptor          = ED1},
		  #'NotifyRequest'{terminationID            = Tids2,
				   observedEventsDescriptor = OED2,
				   errorDescriptor          = ED2}) ->
    validate(fun() -> chk_TerminationIDList(Tids1, Tids2) end, 
	     'NotifyRequest'),
    validate(fun() -> chk_ObservedEventsDescriptor(OED1, OED2) end, 
	     'NotifyRequest'),
    validate(fun() -> chk_opt_ErrorDescriptor(ED1, ED2) end, 
	     'NotifyRequest'),
    ok;
chk_NotifyRequest(NR1, NR2) ->
    wrong_type('NotifyRequest', NR1, NR2).

    
%% -- NotifyReply --

is_NotifyReply(#'NotifyReply'{terminationID   = Tids,
			      errorDescriptor = ED}) ->
    is_TerminationIDList(Tids) andalso is_opt_ErrorDescriptor(ED);
is_NotifyReply(_) ->
    false.

chk_NotifyReply(#'NotifyReply'{terminationID   = Tids1,
			       errorDescriptor = ED1},
		#'NotifyReply'{terminationID   = Tids2,
			       errorDescriptor = ED2}) ->
    validate(fun() -> chk_TerminationIDList(Tids1, Tids2) end, 'NotifyReply'),
    validate(fun() -> chk_opt_ErrorDescriptor(ED1, ED2) end, 'NotifyReply'),
    ok;
chk_NotifyReply(NR1, NR2) ->
    wrong_type('NotifyReply', NR1, NR2).

    
%% -- ObservedEventsDescriptor --

is_ObservedEventsDescriptor(
  #'ObservedEventsDescriptor'{requestId        = RID,
			      observedEventLst = OEL}) ->
    is_RequestID(RID) andalso 
	is_ObservedEventsDescriptor_observedEventLst(OEL);
is_ObservedEventsDescriptor(_) ->
    false.

is_ObservedEventsDescriptor_observedEventLst([]) ->
    true;
is_ObservedEventsDescriptor_observedEventLst([H|T]) ->
    is_ObservedEvent(H) andalso 
	is_ObservedEventsDescriptor_observedEventLst(T);
is_ObservedEventsDescriptor_observedEventLst(_) ->
    false.

chk_ObservedEventsDescriptor(
  #'ObservedEventsDescriptor'{requestId        = RID1,
			      observedEventLst = OEL1},
  #'ObservedEventsDescriptor'{requestId        = RID2,
			      observedEventLst = OEL2}) ->
    validate(fun() -> chk_RequestID(RID1, RID2) end, 
	     'ObservedEventsDescriptor'),
    validate(
      fun() -> 
	      chk_ObservedEventsDescriptor_observedEventLst(OEL1, OEL2) 
      end, 
      'ObservedEventsDescriptor'),
    ok;
chk_ObservedEventsDescriptor(D1, D2) ->
    wrong_type('ObservedEventsDescriptor', D1, D2).
    
chk_ObservedEventsDescriptor_observedEventLst([], []) ->
    ok;
chk_ObservedEventsDescriptor_observedEventLst([] = L1, L2) ->
    not_equal('ObservedEventsDescriptor_observedEventLst', L1, L2);
chk_ObservedEventsDescriptor_observedEventLst(L1, [] = L2) ->
    not_equal('ObservedEventsDescriptor_observedEventLst', L1, L2);
chk_ObservedEventsDescriptor_observedEventLst([H|T1], [H|T2]) ->
    case is_ObservedEvent(H) of
	true ->
	    chk_ObservedEventsDescriptor_observedEventLst(T1, T2);
	false ->
	    wrong_type('ObservedEventsDescriptor_observedEventLst_val', H)
    end;
chk_ObservedEventsDescriptor_observedEventLst([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_ObservedEvent(H1, H2) end, 
	     'ObservedEventsDescriptor_observedEventLst_val'),
    chk_ObservedEventsDescriptor_observedEventLst(T1, T2);
chk_ObservedEventsDescriptor_observedEventLst(L1, L2) ->
    wrong_type('ObservedEventsDescriptor_observedEventLst', L1, L2).


%% -- ObservedEvent --

is_ObservedEvent(#'ObservedEvent'{eventName    = Name, 
				  streamID     = SID, 
				  eventParList = EPL, 
				  timeNotation = TN}) ->
    is_EventName(Name) andalso 
	is_opt_StreamID(SID) andalso 
	is_ObservedEvent_eventParList(EPL) andalso
	is_opt_TimeNotation(TN);
is_ObservedEvent(_) ->
    false.

is_ObservedEvent_eventParList([]) ->
    true;
is_ObservedEvent_eventParList([H|T]) ->
    is_EventParameter(H) andalso is_ObservedEvent_eventParList(T);
is_ObservedEvent_eventParList(_) ->
    false.

chk_ObservedEvent(E, E) ->
    chk_type(fun is_ObservedEvent/1, 'ObservedEvent', E);
chk_ObservedEvent(#'ObservedEvent'{eventName    = Name1, 
				   streamID     = SID1, 
				   eventParList = EPL1, 
				   timeNotation = TN1},
		  #'ObservedEvent'{eventName    = Name2, 
				   streamID     = SID2, 
				   eventParList = EPL2, 
				   timeNotation = TN2}) ->
    validate(fun() -> chk_EventName(Name1, Name2) end, 'ObservedEvent'),
    validate(fun() -> chk_opt_StreamID(SID1, SID2) end, 'ObservedEvent'),
    chk_ObservedEvent_eventParList(EPL1, EPL2),
    validate(fun() -> chk_opt_TimeNotation(TN1, TN2) end, 'ObservedEvent'),
    ok;
chk_ObservedEvent(E1, E2) ->
    wrong_type('ObservedEvent', E1, E2).
    
chk_ObservedEvent_eventParList([], []) ->
    ok;
chk_ObservedEvent_eventParList([] = EPL1, EPL2) ->
    not_equal('ObservedEvent_eventParList', EPL1, EPL2);
chk_ObservedEvent_eventParList(EPL1, [] = EPL2) ->
    not_equal('ObservedEvent_eventParList', EPL1, EPL2);
chk_ObservedEvent_eventParList([H|T1], [H|T2]) ->
    case is_EventParameter(H) of
	true ->
	    chk_ObservedEvent_eventParList(T1, T2);
	false ->
	    wrong_type('ObservedEvent_eventParList_val', H)
    end;
chk_ObservedEvent_eventParList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_EventParameter(H1, H2) end, 
	     'ObservedEvent_eventParList'),
    chk_ObservedEvent_eventParList(T1, T2);
chk_ObservedEvent_eventParList(L1, L2) ->
    wrong_type('ObservedEvent_eventParList', L1, L2).

    
%% -- EventName --

is_EventName(N) -> is_PkgdName(N).

chk_EventName(N, N) ->
    chk_type(fun is_EventName/1, 'EventName', N);
chk_EventName(N1, N2) ->
    case (is_EventName(N1) andalso is_EventName(N2)) of
	true ->
	    not_equal('EventName', N1, N2);
	false ->
	    wrong_type('EventName', N1, N2)
    end.
		
    
%% -- EventParameter --

is_EventParameter(#'EventParameter'{eventParameterName = Name,
				    value              = Val,
				    extraInfo          = EI}) ->
    d("is_EventParameter -> entery with"
      "~n   Name: ~p"
      "~n   Val:  ~p"
      "~n   EI:   ~p", [Name, Val, EI]),
    is_Name(Name) andalso 
	is_Value(Val) andalso 
	is_EventParameter_extraInfo(EI);
is_EventParameter(_) ->
    false.

is_EventParameter_extraInfo(asn1_NOVALUE) ->
    true;
is_EventParameter_extraInfo({Tag, Val}) ->
    is_EventParameter_extraInfo_tag(Tag) andalso
	is_EventParameter_extraInfo_val(Tag, Val);
is_EventParameter_extraInfo(_) ->
    false.

is_EventParameter_extraInfo_tag(Tag) ->
    Tags = [relation, range, sublist],
    lists:member(Tag, Tags).

is_EventParameter_extraInfo_val(relation, Val) ->
    is_Relation(Val);
is_EventParameter_extraInfo_val(range, Val) ->
    is_BOOLEAN(Val);
is_EventParameter_extraInfo_val(sublist, Val) ->
    is_BOOLEAN(Val).

chk_EventParameter(#'EventParameter'{eventParameterName = Name1,
				     value              = Val1,
				     extraInfo          = EI1},
		   #'EventParameter'{eventParameterName = Name2,
				     value              = Val2,
				     extraInfo          = EI2}) ->
    validate(fun() -> chk_Name(Name1, Name2) end, 'EventParameter'),
    validate(fun() -> chk_Value(Val1, Val2)  end, 'EventParameter'),
    chk_EventParameter_extraInfo(EI1, EI2),
    ok;
chk_EventParameter(P1, P2) ->
    wrong_type('EventParameter', P1, P2).

chk_EventParameter_extraInfo(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_EventParameter_extraInfo(EI, EI) ->
    chk_type(fun is_EventParameter_extraInfo/1, 
	     'EventParameter_extraInfo', EI);
chk_EventParameter_extraInfo({Tag, Val1} = EI1, {Tag, Val2} = EI2) ->
    case (is_EventParameter_extraInfo_tag(Tag) andalso 
	  is_EventParameter_extraInfo_val(Tag, Val1) andalso 
	  is_EventParameter_extraInfo_val(Tag, Val2)) of
	true ->
	    chk_EventParameter_extraInfo_val(Tag, Val1, Val2);
	false ->
	    wrong_type('EventParameter_extraInfo', EI1, EI2)
    end;
chk_EventParameter_extraInfo({Tag1, Val1} = EI1, {Tag2, Val2} = EI2) ->
    case ((is_EventParameter_extraInfo_tag(Tag1) andalso 
	   is_EventParameter_extraInfo_val(Tag1, Val1)) andalso 
	  (is_EventParameter_extraInfo_tag(Tag2) andalso 
	   is_EventParameter_extraInfo_val(Tag2, Val2))) of
	true ->
	    not_equal('EventParameter_extraInfo', EI1, EI2);
	false ->
	    wrong_type('EventParameter_extraInfo', EI1, EI2)
    end;
chk_EventParameter_extraInfo(EI1, EI2) ->
    wrong_type('EventParameter_extraInfo', EI1, EI2).

chk_EventParameter_extraInfo_val(relation, Val1, Val2) ->
    validate(fun() -> chk_Relation(Val1, Val2) end, 
	     'EventParameter_extraInfo_val');
chk_EventParameter_extraInfo_val(range, Val1, Val2) ->
    validate(fun() -> chk_BOOLEAN(Val1, Val2) end, 
	     'EventParameter_extraInfo_val');
chk_EventParameter_extraInfo_val(sublist, Val1, Val2) ->
    validate(fun() -> chk_BOOLEAN(Val1, Val2) end, 
	     'EventParameter_extraInfo_val').


%% -- ServiceChangeRequest --

is_ServiceChangeRequest(#'ServiceChangeRequest'{terminationID      = Tids,
						serviceChangeParms = Parms}) ->
    is_TerminationIDList(Tids) andalso is_ServiceChangeParm(Parms);
is_ServiceChangeRequest(_) ->
    false.

chk_ServiceChangeRequest(R, R) ->
    chk_type(fun is_ServiceChangeRequest/1, 'ServiceChangeRequest', R);
chk_ServiceChangeRequest(
  #'ServiceChangeRequest'{terminationID      = Tids1,
			  serviceChangeParms = Parms1},
  #'ServiceChangeRequest'{terminationID      = Tids2,
			  serviceChangeParms = Parms2}) ->
    validate(fun() -> chk_TerminationIDList(Tids1, Tids2) end,
	     'ServiceChangeRequest'),
    validate(fun() -> chk_ServiceChangeParm(Parms1, Parms2) end,
	     'ServiceChangeRequest'),
    ok;
chk_ServiceChangeRequest(R1, R2) ->
    wrong_type('ServiceChangeRequest', R1, R2).

	    
%% -- ServiceChangeReply --

is_ServiceChangeReply(#'ServiceChangeReply'{terminationID       = Tids,
					    serviceChangeResult = Res}) ->
    is_TerminationIDList(Tids) andalso is_ServiceChangeResult(Res);
is_ServiceChangeReply(_) ->
    false.

chk_ServiceChangeReply(R, R) ->
    chk_type(fun is_ServiceChangeReply/1, 'ServiceChangeReply', R);
chk_ServiceChangeReply(
  #'ServiceChangeReply'{terminationID       = Tids1,
			serviceChangeResult = Res1},
  #'ServiceChangeReply'{terminationID       = Tids2,
			serviceChangeResult = Res2}) ->
    validate(fun() -> chk_TerminationIDList(Tids1, Tids2) end,
	     'ServiceChangeReply'),
    validate(fun() -> chk_ServiceChangeResult(Res1, Res2) end,
	     'ServiceChangeReply'),
    ok;
chk_ServiceChangeReply(R1, R2) ->
    wrong_type('ServiceChangeReply', R1, R2).

	    
%% -- ServiceChangeResult --

is_ServiceChangeResult({Tag, Val}) ->
    is_ServiceChangeResult_tag(Tag) andalso 
	is_ServiceChangeResult_val(Tag, Val);
is_ServiceChangeResult(_) ->
    false.

is_ServiceChangeResult_tag(Tag) ->
    Tags = [errorDescriptor, serviceChangeResParms],
    lists:member(Tag, Tags).

is_ServiceChangeResult_val(errorDescriptor, Val) ->
    is_ErrorDescriptor(Val);
is_ServiceChangeResult_val(serviceChangeResParms, Val) ->
    is_ServiceChangeResParm(Val).

chk_ServiceChangeResult(Res, Res) ->
    chk_type(fun is_ServiceChangeResult/1, 'ServiceChangeResult', Res);
chk_ServiceChangeResult({Tag, Val1} = Res1, {Tag, Val2} = Res2) ->
    case (is_ServiceChangeResult_tag(Tag) andalso 
	  is_ServiceChangeResult_val(Tag, Val1) andalso 
	  is_ServiceChangeResult_val(Tag, Val2)) of
	true ->
	    chk_ServiceChangeResult_val(Tag, Val1, Val2);
	false ->
	    wrong_type('ServiceChangeResult', Res1, Res2)
    end;
chk_ServiceChangeResult({Tag1, Val1} = Res1, {Tag2, Val2} = Res2) ->
    case ((is_ServiceChangeResult_tag(Tag1) andalso 
	   is_ServiceChangeResult_val(Tag1, Val1)) andalso 
	  (is_ServiceChangeResult_tag(Tag2) andalso 
	   is_ServiceChangeResult_val(Tag2, Val2))) of
	true ->
	    not_equal('ServiceChangeResult', Res1, Res2);
	false ->
	    wrong_type('ServiceChangeResult', Res1, Res2)
    end;
chk_ServiceChangeResult(Res1, Res2) ->
    wrong_type('ServiceChangeResult', Res1, Res2).

chk_ServiceChangeResult_val(errorDescriptor, Val1, Val2) ->
    validate(fun() -> chk_ErrorDescriptor(Val1, Val2) end, 
	     'ServiceChangeResult');
chk_ServiceChangeResult_val(serviceChangeResParms, Val1, Val2) ->
    validate(fun() -> chk_ServiceChangeResParm(Val1, Val2) end, 
	     'ServiceChangeResult').


%% -- WildcardField --

is_WildcardField(WF) -> is_OCTET_STRING(WF, {exact, 1}).

chk_WildcardField(WF, WF) -> 
    case is_WildcardField(WF) of
	true ->
	    ok;
	false ->
	    wrong_type('WildcardField', WF)
    end;
chk_WildcardField(WF1, WF2) -> 
    case (is_WildcardField(WF1) andalso is_WildcardField(WF2)) of
	true ->
	    not_equal('WildcardField', WF1, WF2);
	false ->
	    wrong_type('WildcardField', WF1, WF2)
    end.


%% -- TerminationID --

is_TerminationID(#'TerminationID'{wildcard = W, 
				  id       = ID}) ->
    is_TerminationID_wildcard(W) andalso is_TerminationID_id(ID);
is_TerminationID(#megaco_term_id{contains_wildcards = _W,
				 id                 = _ID}) ->
    true; % What are the types?
is_TerminationID(_) ->
    false.
    
is_TerminationID_wildcard([]) ->
    true;
is_TerminationID_wildcard([H|T]) ->
    is_WildcardField(H) andalso is_TerminationID_wildcard(T);
is_TerminationID_wildcard(_) ->
    false.

is_TerminationID_id(ID) -> is_OCTET_STRING(ID, {range, 1, 8}).
			
chk_TerminationID(Id,Id) ->
    d("chk_TerminationID(1) -> entry with"
      "~n   Id: ~p", [Id]),
    chk_type(fun is_TerminationID/1, 'TerminationID', Id);
chk_TerminationID(#'TerminationID'{wildcard = W1,
                                   id       = I1},
                  #'TerminationID'{wildcard = W2,
                                   id       = I2}) ->
    d("chk_TerminationID(2) -> entry with"
      "~n   W1: ~p"
      "~n   W2: ~p"
      "~n   I1: ~p"
      "~n   I2: ~p", [W1, W2, I1, I2]),
    chk_TerminationID_wildcard(W1, W2),
    chk_TerminationID_id(I1, I2),
    ok;
chk_TerminationID(#megaco_term_id{contains_wildcards = W1,
                                  id                 = I1},
                  #megaco_term_id{contains_wildcards = W2,
                                  id                 = I2}) ->
    d("chk_TerminationID(3) -> entry with"
      "~n   W1: ~p"
      "~n   W2: ~p"
      "~n   I1: ~p"
      "~n   I2: ~p", [W1, W2, I1, I2]),
    validate(fun() -> chk_BOOLEAN(W1, W2) end, 'TerminationID_wildcard'),
    chk_TerminationID_id(I1, I2),
    ok;
chk_TerminationID(Tid1, Tid2) ->
    d("chk_TerminationID(4) -> entry with"
      "~n   Tid1: ~p"
      "~n   Tid2: ~p", [Tid1, Tid2]),
    wrong_type('TerminationID', Tid1, Tid2).
 
chk_TerminationID_wildcard([], []) ->
    ok;
chk_TerminationID_wildcard([] = WF1, WF2) ->
    not_equal('TerminationID_wildcard', WF1, WF2);
chk_TerminationID_wildcard(WF1, [] = WF2) ->
    not_equal('TerminationID_wildcard', WF1, WF2);
chk_TerminationID_wildcard([H|T1], [H|T2]) ->
    case is_WildcardField(H) of
	true ->
	    chk_TerminationID_wildcard(T1, T2);
	false ->
	    wrong_type('TerminationID_wildcard_val', H)
    end;
chk_TerminationID_wildcard([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_WildcardField(H1, H2) end, 
	     'TerminationID_wildcard_val'),
    chk_TerminationID_wildcard(T1, T2);
chk_TerminationID_wildcard(WF1,WF2) ->
    not_equal('TerminationId_wildcard', WF1, WF2).
 
chk_TerminationID_id(Id, Id) ->
    case is_OCTET_STRING(Id, {range, 1, 8}) of
	true ->
	    ok;
	false ->
	    wrong_type('TerminationID_id', Id, Id)
    end;
chk_TerminationID_id(Id1, Id2) ->
    Id12 = flatten_TerminationID_id(Id1, []),
    Id22 = flatten_TerminationID_id(Id2, []),
    case Id12 == Id22 of
	true ->
	    ok;
	false ->
	    not_equal(terminationId_id, Id1, Id2)
    end.
 
flatten_TerminationID_id([], Acc) ->
    lists:flatten(lists:reverse(Acc));
flatten_TerminationID_id([H|T], Acc) ->
    flatten_TerminationID_id(T, [string:tokens(H, [$/])|Acc]).

%% -- TerminationIDList -- 

%% is_opt_TerminationIDList(asn1_NOVALUE) ->
%%     true;
%% is_opt_TerminationIDList(TIDs) ->
%%     is_TerminationIDList(TIDs).

is_TerminationIDList([]) ->
    true;
is_TerminationIDList([H|T]) ->
    is_TerminationID(H) andalso is_TerminationIDList(T);
is_TerminationIDList(_) ->
    false.

chk_opt_TerminationIDList(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_TerminationIDList(TIDs1, TIDs2) ->
    chk_TerminationIDList(TIDs1, TIDs2).

chk_TerminationIDList([], []) ->
    ok;
chk_TerminationIDList([] = L1, L2) ->
    not_equal('TerminationIDList', L1, L2);
chk_TerminationIDList(L1, [] = L2) ->
    not_equal('TerminationIDList', L1, L2);
chk_TerminationIDList([H|T1], [H|T2]) ->
    case is_TerminationID(H) of
	true ->
	    chk_TerminationIDList(T1, T2);
	false ->
	    wrong_type('TerminationIDList', H)
    end;
chk_TerminationIDList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_TerminationID(H1, H2) end, 'TerminationIDList'),
    chk_TerminationIDList(T1, T2);
chk_TerminationIDList(L1, L2) ->
    wrong_type('TerminationIDList', L1, L2).


%% -- MediaDescriptor --

is_MediaDescriptor(#'MediaDescriptor'{termStateDescr = TSD,
				      streams        = S}) ->
    d("is_MediaDescriptor -> entry with"
      "~n   TSD: ~p"
      "~n   S:   ~p", [TSD, S]),
    is_opt_TerminationStateDescriptor(TSD) andalso
	is_MediaDescriptor_streams(S);
is_MediaDescriptor(_) ->
    false.

is_MediaDescriptor_streams(asn1_NOVALUE) ->
    true;
is_MediaDescriptor_streams({Tag, Val}) ->
    is_MediaDescriptor_streams_tag(Tag) andalso 
	is_MediaDescriptor_streams_val(Tag, Val);
is_MediaDescriptor_streams(_) ->
    false.

is_MediaDescriptor_streams_tag(Tag) ->
    Tags = [oneStream, multiStream],
    lists:member(Tag, Tags).

is_MediaDescriptor_streams_val(oneStream, SP) ->
    is_StreamParms(SP);
is_MediaDescriptor_streams_val(multiStream, SDL) ->
    is_MediaDescriptor_multiStream(SDL).

is_MediaDescriptor_multiStream([]) ->
    true;
is_MediaDescriptor_multiStream([H|T]) ->
    is_StreamDescriptor(H) andalso is_MediaDescriptor_multiStream(T);
is_MediaDescriptor_multiStream(_) ->
    false.

chk_MediaDescriptor(D, D) ->
    chk_type(fun is_MediaDescriptor/1, 'MediaDescriptor', D);
chk_MediaDescriptor(#'MediaDescriptor'{termStateDescr = TSD1,
				       streams        = S1},
		    #'MediaDescriptor'{termStateDescr = TSD2,
				       streams        = S2}) ->
    validate(
      fun() -> 
	      chk_opt_TerminationStateDescriptor(TSD1, TSD2) 
      end, 
      'MediaDescriptor'),
    validate(
      fun() ->
	      chk_MediaDescriptor_streams(S1, S2)
      end,
      'MediaDescriptor'),
    ok;
chk_MediaDescriptor(D1, D2) ->
    wrong_type('MediaDescriptor', D1, D2).


chk_MediaDescriptor_streams(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_MediaDescriptor_streams({oneStream, SP1}, {oneStream, SP2}) ->
    validate(fun() -> 
		     chk_StreamParms(SP1, SP2) 
	     end, 
	     'MediaDescriptor_streams');
chk_MediaDescriptor_streams({multiStream, SDs1}, {multiStream, SDs2}) ->
    validate(fun() ->
		     chk_MediaDescriptor_multiStream(SDs1, SDs2)
	     end,
	     'MediaDescriptor_streams');
chk_MediaDescriptor_streams(S1, S2) ->
    wrong_type('MediaDescriptor_streams', S1, S2).
    
chk_MediaDescriptor_multiStream([], []) ->
    ok;
chk_MediaDescriptor_multiStream([] = MS1, MS2) ->
    not_equal('MediaDescriptor_multiStream', MS1, MS2);
chk_MediaDescriptor_multiStream(MS1, [] = MS2) ->
    not_equal('MediaDescriptor_multiStream', MS1, MS2);
chk_MediaDescriptor_multiStream([H|T1], [H|T2]) ->
    case is_StreamDescriptor(H) of
	true ->
	    chk_MediaDescriptor_multiStream(T1, T2);
	false ->
	    wrong_type('MediaDescriptor_multiStream_val', H)
    end;
chk_MediaDescriptor_multiStream([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_StreamDescriptor(H1, H2) end, 
	     'MediaDescriptor_multiStream_val'),
    chk_MediaDescriptor_multiStream(T1, T2);
chk_MediaDescriptor_multiStream(MS1, MS2) ->
    wrong_type('MediaDescriptor_multiStream_val', MS1, MS2).


%% -- StreamDescriptor --

is_StreamDescriptor(#'StreamDescriptor'{streamID    = SID, 
					streamParms = Parms}) ->
    d("is_StreamDescriptor -> entry with"
      "~n   SID:   ~p"
      "~n   Parms: ~p", [SID, Parms]),
    is_StreamID(SID) andalso is_StreamParms(Parms);
is_StreamDescriptor(X) ->
    d("is_StreamDescriptor -> entry when ERROR with"
      "~n   X: ~p", [X]),
    false.

chk_StreamDescriptor(D, D) ->
    chk_type(fun is_StreamDescriptor/1, 'StreamDescriptor', D);
chk_StreamDescriptor(#'StreamDescriptor'{streamID    = SID1, 
					 streamParms = Parms1},
		     #'StreamDescriptor'{streamID    = SID2, 
					 streamParms = Parms2}) ->
    validate(fun() -> chk_StreamID(SID1, SID2) end, 'StreamDescriptor'),
    validate(fun() -> chk_StreamParms(Parms1, Parms2) end, 'StreamDescriptor'),
    ok;
chk_StreamDescriptor(D1, D2) ->
    wrong_type('StreamDescriptor', D1, D2).

		      
%% -- StreamParms -- 

is_StreamParms(#'StreamParms'{localControlDescriptor = LCD,
			      localDescriptor        = LD,
			      remoteDescriptor       = RD,
			      statisticsDescriptor   = SD}) ->
    d("is_StreamParms -> entry with"
      "~n   LCD: ~p"
      "~n   LD:  ~p"
      "~n   RD:  ~p"
      "~n   SD:  ~p", [LCD, LD, RD, SD]),
    is_opt_LocalControlDescriptor(LCD) andalso 
	is_opt_LocalRemoteDescriptor(LD) andalso 
	is_opt_LocalRemoteDescriptor(RD) andalso 
	is_opt_StatisticsDescriptor(SD);
is_StreamParms(X) ->
    d("is_StreamParms -> entry when ERROR with"
      "~n   X: ~p", [X]),
    false.

chk_StreamParms(SP, SP) ->
    chk_type(fun is_StreamParms/1, 'StreamParms', SP);
chk_StreamParms(#'StreamParms'{localControlDescriptor = LCD1,
			       localDescriptor        = LD1,
			       remoteDescriptor       = RD1,
			       statisticsDescriptor   = SD1},
		#'StreamParms'{localControlDescriptor = LCD2,
			       localDescriptor        = LD2,
			       remoteDescriptor       = RD2,
			       statisticsDescriptor   = SD2}) ->
    validate(fun() -> chk_opt_LocalControlDescriptor(LCD1, LCD2) end,
	     'StreamParms_localControlDescriptor'),
    validate(fun() -> chk_opt_LocalRemoteDescriptor(LD1, LD2) end, 
	     'StreamParms_localDescriptor'),
    validate(fun() -> chk_opt_LocalRemoteDescriptor(RD1, RD2) end, 
	     'StreamParms_remoteDescriptor'),
    validate(fun() -> chk_opt_StatisticsDescriptor(SD1, SD2) end, 
	     'StreamParms_statisticsDescriptor'),
    ok;
chk_StreamParms(P1, P2) ->
    wrong_type('StreamParms', P1, P2).


%% -- LocalControlDescriptor -- 

is_opt_LocalControlDescriptor(D) ->
    d("is_opt_LocalControlDescriptor -> entry"),
    is_OPTIONAL(fun is_LocalControlDescriptor/1, D).

is_LocalControlDescriptor(#'LocalControlDescriptor'{streamMode    = SM,
						    reserveValue  = RV,
						    reserveGroup  = RG, 
						    propertyParms = PP}) ->
    d("is_LocalControlDescriptor -> entry with"
      "~n   SM: ~p"
      "~n   RV: ~p"
      "~n   RG: ~p"
      "~n   PP: ~p", [SM, RV, RG, PP]),
    is_opt_StreamMode(SM) andalso 
	is_opt_BOOLEAN(RV) andalso 
	is_opt_BOOLEAN(RG) andalso 
	is_LocalControlDescriptor_propertyParms(PP);
is_LocalControlDescriptor(_) ->
    false.

is_LocalControlDescriptor_propertyParms([]) ->
    d("is_LocalControlDescriptor_propertyParms -> entry when done"),
    true;
is_LocalControlDescriptor_propertyParms([H|T]) ->
    d("is_LocalControlDescriptor_propertyParms -> entry with"
      "~n   H: ~p"
      "~n   T: ~p", [H, T]),
    is_PropertyParm(H) andalso is_LocalControlDescriptor_propertyParms(T);
is_LocalControlDescriptor_propertyParms(_) ->
    false.

chk_opt_LocalControlDescriptor(LCD1, LCD2) ->
    chk_OPTIONAL('LocalControlDescriptor', LCD1, LCD2, 
		 fun is_LocalControlDescriptor/1, 
		 fun chk_LocalControlDescriptor/2).

chk_LocalControlDescriptor(LCD, LCD) ->
    chk_type(fun is_LocalControlDescriptor/1, 'LocalControlDescriptor', LCD);
chk_LocalControlDescriptor(#'LocalControlDescriptor'{streamMode    = SM1,
						     reserveValue  = RV1,
						     reserveGroup  = RG1, 
						     propertyParms = PP1},
			   #'LocalControlDescriptor'{streamMode    = SM2,
						     reserveValue  = RV2,
						     reserveGroup  = RG2, 
						     propertyParms = PP2}) ->
    validate(
      fun() -> chk_opt_StreamMode(SM1, SM2) end, 
      'LocalControlDescriptor'),
    validate(
      fun() -> chk_opt_BOOLEAN(RV1, RV2) end, 
      'LocalControlDescriptor_reserveValue'),
    validate(
      fun() -> chk_opt_BOOLEAN(RG1, RG2) end, 
      'LocalControlDescriptor_reserveGroup'),
    chk_LocalControlDescriptor_propertyParms(PP1, PP2),
    ok;
chk_LocalControlDescriptor(LCD1, LCD2) ->
    wrong_type('LocalControlDescriptor', LCD1, LCD2).


chk_LocalControlDescriptor_propertyParms([], []) ->
    ok;
chk_LocalControlDescriptor_propertyParms([] = PP1, PP2) ->
    not_equal('LocalControlDescriptor_propertyParms', PP1, PP2);
chk_LocalControlDescriptor_propertyParms(PP1, [] = PP2) ->
    not_equal('LocalControlDescriptor_propertyParms', PP1, PP2);
chk_LocalControlDescriptor_propertyParms([H|T1], [H|T2]) ->
    case is_PropertyParm(H) of
	true ->
	    chk_LocalControlDescriptor_propertyParms(T1, T2);
	false ->
	    wrong_type('LocalControlDescriptor_propertyParms_val', H)
    end;
chk_LocalControlDescriptor_propertyParms([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_PropertyParm(H1, H2) end, 
	     'LocalControlDescriptor_propertyParms_val'),
    chk_LocalControlDescriptor_propertyParms(T1, T2);
chk_LocalControlDescriptor_propertyParms(PP1, PP2) ->
    wrong_type('LocalControlDescriptor_propertyParms', PP1, PP2).
    

%% -- StreamMode --

is_opt_StreamMode(asn1_NOVALUE) ->
    true;
is_opt_StreamMode(SM) ->
    is_StreamMode(SM).

is_StreamMode(SM) ->
    lists:member(SM, [sendOnly, recvOnly, sendRecv, inactive, loopBack]).

chk_opt_StreamMode(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_StreamMode(SM1, SM2) ->
    chk_StreamMode(SM1, SM2).

chk_StreamMode(SM, SM) ->
    chk_type(fun is_StreamMode/1, 'StreamMode', SM);
chk_StreamMode(SM1, SM2) ->
    case (is_StreamMode(SM1) andalso is_StreamMode(SM2)) of
	true ->
	    not_equal('StreamMode', SM1, SM2);
	false ->
	    wrong_type('StreamMode', SM1, SM2)
    end.


%% -- PropertyParm --

is_opt_PropertyParm(asn1_NOVALUE) ->
    true;
is_opt_PropertyParm(PP) ->
    is_PropertyParm(PP).

is_PropertyParm(#'PropertyParm'{name      = N,
				value     = V,
				extraInfo = I}) ->
    d("is_PropertyParm -> entry with"
      "~n   N: ~p"
      "~n   V: ~p"
      "~n   I: ~p", [N, V, I]),
    is_PkgdName(N) andalso 
	is_PropertyParm_value(V) andalso 
	is_PropertyParm_extraInfo(I);
is_PropertyParm(_) ->
    false.

is_PropertyParm_value([]) ->
    d("is_PropertyParm_value -> entry when done"),
    true;
is_PropertyParm_value([H|T]) ->
    d("is_PropertyParm_value -> entry with"
      "~n   H: ~p", [H]),
    is_OCTET_STRING(H) andalso is_PropertyParm_value(T);
is_PropertyParm_value(_) ->
    false.

is_PropertyParm_extraInfo(asn1_NOVALUE) ->
    true;
is_PropertyParm_extraInfo({Tag, Val}) ->
    d("is_PropertyParm_extraInfo -> entry with"
      "~n   Tag: ~p"
      "~n   Val: ~p", [Tag, Val]),
    is_PropertyParm_extraInfo_tag(Tag) andalso 
	is_PropertyParm_extraInfo_val(Tag, Val);
is_PropertyParm_extraInfo(_) ->
    false.

is_PropertyParm_extraInfo_tag(Tag) ->
    Tags = [relation, range, sublist],
    lists:member(Tag, Tags).

is_PropertyParm_extraInfo_val(relation, Val) ->
    is_Relation(Val);
is_PropertyParm_extraInfo_val(range, Val) ->
    is_BOOLEAN(Val);
is_PropertyParm_extraInfo_val(sublist, Val) ->
    is_BOOLEAN(Val).


chk_opt_PropertyParm(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_PropertyParm(P1, P2) ->
    chk_PropertyParm(P1, P2).

chk_PropertyParm(P, P) ->
    chk_type(fun is_PropertyParm/1, 'PropertyParm', P);
chk_PropertyParm(#'PropertyParm'{name      = N1,
				 value     = V1,
				 extraInfo = I1}, 
		 #'PropertyParm'{name      = N2,
				 value     = V2,
				 extraInfo = I2}) ->
    validate(fun() -> chk_PkgdName(N1, N2) end, 'PropertyParm'),
    chk_PropertyParm_value(V1, V2), 
    chk_PropertyParm_extraInfo(I1, I2), 
    ok;
chk_PropertyParm(P1, P2) ->
    wrong_type('PropertyParm', P1, P2).

chk_PropertyParm_value([], []) ->
    ok;
chk_PropertyParm_value([] = V1, V2) ->
    not_equal('PropertyParm_value', V1, V2);
chk_PropertyParm_value(V1, [] = V2) ->
    not_equal('PropertyParm_value', V1, V2);
chk_PropertyParm_value([H|T1], [H|T2]) ->
    case is_OCTET_STRING(H) of
	true ->
	    chk_PropertyParm_value(T1, T2);
	false ->
	    wrong_type('PropertyParm_value_val', H)
    end;
chk_PropertyParm_value([H1|_], [H2|_]) ->
    case (is_OCTET_STRING(H1) andalso is_OCTET_STRING(H2)) of
	true ->
	    not_equal('PropertyParm_value_val', H1, H2);
	false ->
	    wrong_type('PropertyParm_value_val', H1, H2)
    end;
chk_PropertyParm_value(V1, V2) ->
    wrong_type('PropertyParm_value', V1, V2).

chk_PropertyParm_extraInfo(EI, EI) ->
    chk_type(fun is_PropertyParm_extraInfo/1, 'PropertyParm_extraInfo', EI);
chk_PropertyParm_extraInfo({Tag, Val1} = EI1, {Tag, Val2} = EI2) ->
    case (is_PropertyParm_extraInfo_tag(Tag) and
	  is_PropertyParm_extraInfo_val(Tag, Val1) and
	  is_PropertyParm_extraInfo_val(Tag, Val2)) of
	true ->
	    chk_PropertyParm_extraInfo_val(Tag, Val1, Val2);
	false ->
	    wrong_type('PropertyParm_extraInfo', EI1, EI2)
    end;
chk_PropertyParm_extraInfo({Tag1, Val1} = EI1, {Tag2, Val2} = EI2) ->
    case ((is_PropertyParm_extraInfo_tag(Tag1) and
	   is_PropertyParm_extraInfo_val(Tag1, Val1)) and
	  (is_PropertyParm_extraInfo_tag(Tag2) and
	   is_PropertyParm_extraInfo_val(Tag2, Val2))) of
	true ->
	    not_equal('PropertyParm_extraInfo', EI1, EI2);
	false ->
	    wrong_type('PropertyParm_extraInfo', EI1, EI2)
    end;
chk_PropertyParm_extraInfo(EI1, EI2) ->
    wrong_type('PropertyParm_extraInfo', EI1, EI2).

chk_PropertyParm_extraInfo_val(relation, Val1, Val2) ->
    validate(fun() -> chk_Relation(Val1, Val2) end, 'PropertyParm_extraInfo');
chk_PropertyParm_extraInfo_val(range, Val1, Val2) ->
    validate(fun() -> chk_BOOLEAN(Val1, Val2) end, 'PropertyParm_extraInfo');
chk_PropertyParm_extraInfo_val(sublist, Val1, Val2) ->
    validate(fun() -> chk_BOOLEAN(Val1, Val2) end, 'PropertyParm_extraInfo').


%% -- Name --

is_Name(N) -> 
    %% Binary: is_OCTET_STRING(N, {exact, 2}).
    case is_OCTET_STRING(N, {range, 1, 64}) of
	true ->
	    is_NAME(N);
	false ->
	    false
    end.

is_NAME([H|T]) when (H =< $z) and ($a =< H) ->
    is_NAME2(T);
is_NAME([H|T]) when (H =< $Z) and ($A =< H) ->
    is_NAME2(T);
is_NAME(_) ->
    false.

is_NAME2([]) ->
    true;
is_NAME2([$_|T]) ->
    is_NAME2(T);
is_NAME2([H|T]) when (H =< $z) and ($a =< H) ->
    is_NAME2(T);
is_NAME2([H|T]) when (H =< $Z) and ($A =< H) ->
    is_NAME2(T);
is_NAME2([H|T]) when (H =< $9) and ($0 =< H) ->
    is_NAME2(T);
is_NAME2(_) ->
    false.
        
        
        
chk_Name(N, N) ->
    chk_type(fun is_Name/1, 'Name', N);
chk_Name(N1, N2) ->
    case (is_Name(N1) andalso is_Name(N2)) of
	true ->
	    not_equal('Name', N1, N2);
	false ->
	    wrong_type('Name', N1, N2)
    end.


%% -- PkgdName --

%% PkgdName is either "AB/CD" or just plain "ABCD"
%% Note that in ASN.1 the parts is exactly 2 char
%% each, unless you don't use the native config 
%% option. In text and in binary without the native
%% option, it is 63 + 1 chars for each.
is_PkgdName(N) -> 
    d("is_PkgdName -> entry with"
      "~n   N: ~p", [N]),
    case string:tokens(N, "/") of
	["*" = PackageName, "*" = ItemID] ->
	    d("is_PkgdName -> tokenized (0): "
	      "~n   PackageName: ~p"
	      "~n   ItemID:      ~p", [PackageName, ItemID]),
	    true; 
	[PackageName, "*" = ItemID] ->
	    d("is_PkgdName -> tokenized (1): "
	      "~n   PackageName: ~p"
	      "~n   ItemID:      ~p", [PackageName, ItemID]),
	    is_Name(PackageName);
	[PackageName, ItemID] ->
	    d("is_PkgdName -> tokenized (2): "
	      "~n   PackageName: ~p"
	      "~n   ItemID:      ~p", [PackageName, ItemID]),
	    is_Name(PackageName) andalso is_Name(ItemID);
	_ ->
	    is_Name(N)
    end.

chk_PkgdName(N, N) ->
    case is_PkgdName(N) of
	true ->
	    ok;
	false ->
	    wrong_type('PkgdName', N, N)
    end;
chk_PkgdName(N1, N2) ->
    case (is_PkgdName(N1) andalso is_PkgdName(N2)) of
	true ->
	    not_equal('PkgdName', N1, N2);
	false ->
	    wrong_type('PkgdName', N1, N2)
    end.


%% -- Relation --

is_Relation(R) ->
    lists:member(R, [greaterThan, smallerThan, unequalTo]).

chk_Relation(R, R) ->
    chk_type(fun is_Relation/1, 'Relation', R);
chk_Relation(R1, R2) ->
    case (is_Relation(R1) andalso is_Relation(R2)) of
	true ->
	    not_equal('Relation', R1, R2);
	false ->
	    wrong_type('Relation', R1, R2)
    end.


%% -- LocalRemoteDescriptor --

is_opt_LocalRemoteDescriptor(D) ->
    d("is_LocalRemoteDescriptor -> entry"),
    is_OPTIONAL(fun is_LocalRemoteDescriptor/1, D).

is_LocalRemoteDescriptor(#'LocalRemoteDescriptor'{propGrps = PGs}) ->
    d("is_LocalRemoteDescriptor -> entry with"
      "~n   PGs: ~p", [PGs]),
    is_LocalRemoteDescriptor_propGrps(PGs);
is_LocalRemoteDescriptor(_) ->
    false.

is_LocalRemoteDescriptor_propGrps([]) ->
    true;
is_LocalRemoteDescriptor_propGrps([H|T]) ->
    is_PropertyGroup(H) andalso is_LocalRemoteDescriptor_propGrps(T);
is_LocalRemoteDescriptor_propGrps(_) ->
    false.

chk_opt_LocalRemoteDescriptor(D1, D2) ->
    chk_OPTIONAL('LocalRemoteDescriptor', D1, D2, 
		 fun is_LocalRemoteDescriptor/1,
		 fun chk_LocalRemoteDescriptor/2).

chk_LocalRemoteDescriptor(LRD, LRD) ->
    chk_type(fun is_LocalRemoteDescriptor/1, 'LocalRemoteDescriptor', LRD);
chk_LocalRemoteDescriptor(#'LocalRemoteDescriptor'{propGrps = PG1},
			  #'LocalRemoteDescriptor'{propGrps = PG2}) ->
    chk_LocalRemoteDescriptor_propGrps(PG1, PG2),
    ok;
chk_LocalRemoteDescriptor(LRD1, LRD2) ->
    wrong_type('LocalRemoteDescriptor', LRD1, LRD2).

chk_LocalRemoteDescriptor_propGrps([], []) ->
    ok;
chk_LocalRemoteDescriptor_propGrps([] = PG1, PG2) ->
    not_equal('LocalRemoteDescriptor_propGrps', PG1, PG2);
chk_LocalRemoteDescriptor_propGrps(PG1, [] = PG2) ->
    not_equal('LocalRemoteDescriptor_propGrps', PG1, PG2);
chk_LocalRemoteDescriptor_propGrps([H|T1], [H|T2]) ->
    case is_PropertyGroup(H) of
	true ->
	    chk_LocalRemoteDescriptor_propGrps(T1, T2);
	false ->
	    wrong_type('LocalRemoteDescriptor_propGrps_val', H)
    end;
chk_LocalRemoteDescriptor_propGrps([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_PropertyGroup(H1, H2) end,
	     'LocalRemoteDescriptor_propGrps_val'),
    chk_LocalRemoteDescriptor_propGrps(T1, T2);
chk_LocalRemoteDescriptor_propGrps(PG1, PG2) ->
    wrong_type('LocalRemoteDescriptor_propGrps', PG1, PG2).


%% -- PropertyGroup -- 

is_PropertyGroup([]) ->
    true;
is_PropertyGroup([H|T]) ->
    is_PropertyParm(H) andalso is_PropertyGroup(T);
is_PropertyGroup(_) ->
    false.

chk_PropertyGroup([], []) ->
    ok;
chk_PropertyGroup([] = PG1, PG2) ->
    not_equal('PropertyGroup', PG1, PG2);
chk_PropertyGroup(PG1, [] = PG2) ->
    not_equal('PropertyGroup', PG1, PG2);
chk_PropertyGroup([H|T1], [H|T2]) ->
    case is_PropertyParm(H) of
	true ->
	    chk_PropertyGroup(T1, T2);
	false ->
	    wrong_type('PropertyGroup_val', H)
    end;
chk_PropertyGroup([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_PropertyParm(H1, H2) end, 'PropertyGroup_val'),
    chk_PropertyGroup(T1, T2);
chk_PropertyGroup(PG1, PG2) ->
    wrong_type('PropertyGroup', PG1, PG2).
    

%% -- TerminationStateDescriptor --

is_opt_TerminationStateDescriptor(D) ->
    is_OPTIONAL(fun is_TerminationStateDescriptor/1, D).

is_TerminationStateDescriptor(
  #'TerminationStateDescriptor'{propertyParms      = PP,
				eventBufferControl = EBC,
				serviceState       = SS}) ->
    is_TerminationStateDescriptor_propertyParms(PP) andalso 
	is_opt_EventBufferControl(EBC) andalso 
	is_opt_ServiceState(SS);
is_TerminationStateDescriptor(_) ->
    false.

is_TerminationStateDescriptor_propertyParms([]) ->
    true;
is_TerminationStateDescriptor_propertyParms([H|T]) ->
    is_PropertyParm(H) andalso is_TerminationStateDescriptor_propertyParms(T);
is_TerminationStateDescriptor_propertyParms(_) ->
    false.

chk_opt_TerminationStateDescriptor(D1, D2) ->
    chk_OPTIONAL('TerminationStateDescriptor', D1, D2, 
		 fun is_TerminationStateDescriptor/1, 
		 fun chk_TerminationStateDescriptor/2).

chk_TerminationStateDescriptor(D, D) ->
    chk_type(fun is_TerminationStateDescriptor/1, 
	     'TerminationStateDescriptor', D);
chk_TerminationStateDescriptor(
  #'TerminationStateDescriptor'{propertyParms      = PP1,
				eventBufferControl = EBC1,
				serviceState       = SS1},
  #'TerminationStateDescriptor'{propertyParms      = PP2,
				eventBufferControl = EBC2,
				serviceState       = SS2}) ->
    chk_TerminationStateDescriptor_propertyParms(PP1, PP2),
    validate(
      fun() -> 
	      chk_opt_EventBufferControl(EBC1, EBC2) 
      end, 
      'TerminationStateDescriptor'),
    validate(
      fun() -> 
	      chk_opt_ServiceState(SS1, SS2) 
      end, 
      'TerminationStateDescriptor'),
    ok;
chk_TerminationStateDescriptor(D1, D2) ->
    wrong_type('TerminationStateDescriptor', D1, D2).
		     

chk_TerminationStateDescriptor_propertyParms([], []) ->
    ok;
chk_TerminationStateDescriptor_propertyParms([] = P1, P2) ->
    not_equal('TerminationStateDescriptor_propertyParms', P1, P2);
chk_TerminationStateDescriptor_propertyParms(P1, [] = P2) ->
    not_equal('TerminationStateDescriptor_propertyParms', P1, P2);
chk_TerminationStateDescriptor_propertyParms([H|T1], [H|T2]) ->
    case is_PropertyParm(H) of
	true ->
	    chk_TerminationStateDescriptor_propertyParms(T1, T2);
	false ->
	    wrong_type('TerminationStateDescriptor_propertyParms_val', H)
    end;
chk_TerminationStateDescriptor_propertyParms([H1|_], [H2|_]) ->
    case (is_PropertyParm(H1) andalso is_PropertyParm(H2)) of
	true ->
	    not_equal('TerminationStateDescriptor_propertyParms_val', H1, H2);
	false ->
	    wrong_type('TerminationStateDescriptor_propertyParms_val', H1, H2)
    end;
chk_TerminationStateDescriptor_propertyParms(P1, P2) ->
    wrong_type('TerminationStateDescriptor_propertyParms', P1, P2).


%% -- EventBufferControl -- 

is_opt_EventBufferControl(asn1_NOVALUE) ->
    true;
is_opt_EventBufferControl(EBC) ->
    is_EventBufferControl(EBC).

is_EventBufferControl(EBC) ->
    lists:member(EBC, [off, lockStep]).

chk_opt_EventBufferControl(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_EventBufferControl(EBC1, EBC2) ->
    chk_EventBufferControl(EBC1, EBC2).

chk_EventBufferControl(EBC, EBC) ->
    chk_type(fun is_EventBufferControl/1, 'EventBufferControl', EBC);
chk_EventBufferControl(EBC1, EBC2) ->
    case (is_EventBufferControl(EBC1) andalso is_EventBufferControl(EBC2)) of
	true ->
	    not_equal('EventBufferControl', EBC1, EBC2);
	false ->
	    wrong_type('EventBufferControl', EBC1, EBC2)
    end.


%% -- ServiceState --

is_opt_ServiceState(asn1_NOVALUE) ->
    true;
is_opt_ServiceState(SS) ->
    is_ServiceState(SS).

is_ServiceState(SS) ->
    lists:member(SS, [test, outOfSvc, inSvc]).

chk_opt_ServiceState(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_ServiceState(SS1, SS2) ->
    chk_ServiceState(SS1, SS2).

chk_ServiceState(SS, SS) ->
    chk_type(fun is_ServiceState/1, 'ServiceState', SS);
chk_ServiceState(SS1, SS2) ->
    case (is_ServiceState(SS1) andalso is_ServiceState(SS2)) of
	true ->
	    not_equal('ServiceState', SS1, SS2);
	false ->
	    wrong_type('ServiceState', SS1, SS2)
    end.


%% -- MuxDescriptor --

is_MuxDescriptor(#'MuxDescriptor'{muxType         = MT,
				  termList        = TL,
				  nonStandardData = NSD}) ->
    is_MuxType(MT) andalso 
	is_MuxDescriptor_termList(TL) andalso 
	is_NonStandardData(NSD);
is_MuxDescriptor(_) ->
    false.

is_MuxDescriptor_termList([]) ->
    true;
is_MuxDescriptor_termList([H|T]) ->
    is_TerminationID(H) andalso is_MuxDescriptor_termList(T);
is_MuxDescriptor_termList(_) ->
    false.

chk_MuxDescriptor(D, D) ->
    chk_type(fun is_MuxDescriptor/1, 'MuxDescriptor', D);
chk_MuxDescriptor(#'MuxDescriptor'{muxType         = MT1,
				   termList        = TL1,
				   nonStandardData = NSD1},
		  #'MuxDescriptor'{muxType         = MT2,
				   termList        = TL2,
				   nonStandardData = NSD2}) ->
    validate(fun() -> chk_MuxType(MT1, MT2) end, 'MuxDescriptor'),
    chk_MuxDescriptor_termList(TL1, TL2),
    validate(fun() -> chk_NonStandardData(NSD1, NSD2) end, 'MuxDescriptor'),
    ok;
chk_MuxDescriptor(D1, D2) ->
    wrong_type('MuxDescriptor', D1, D2).

chk_MuxDescriptor_termList([], []) ->		     
    ok;
chk_MuxDescriptor_termList([] = TL1, TL2) ->
    not_equal('MuxDescriptor_termList', TL1, TL2);
chk_MuxDescriptor_termList(TL1, [] = TL2) ->
    not_equal('MuxDescriptor_termList', TL1, TL2);
chk_MuxDescriptor_termList([H|T1], [H|T2]) ->
    case is_TerminationID(H) of
	true ->
	    chk_MuxDescriptor_termList(T1, T2);
	false ->
	    wrong_type('MuxDescriptor_termList_val', H)
    end;
chk_MuxDescriptor_termList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_TerminationID(H1, H2) end, 
	     'MuxDescriptor_termList_val'),
    chk_MuxDescriptor_termList(T1, T2);
chk_MuxDescriptor_termList(TL1, TL2) ->
    wrong_type('MuxDescriptor_termList', TL1, TL2).


%% -- MuxType --

is_MuxType(MT) ->
    lists:member(MT, [h221, h223, h226, v76, nx64k]).

chk_MuxType(MT, MT) ->
    chk_type(fun is_MuxType/1, 'MuxType', MT);
chk_MuxType(MT1, MT2) ->
    case (is_MuxType(MT1) andalso is_MuxType(MT2)) of
	true ->
	    not_equal('MuxType', MT1, MT2);
	false ->
	    wrong_type('MuxType', MT1, MT2)
    end.


%% -- StreamID --

is_opt_StreamID(V) ->
    is_OPTIONAL(fun is_StreamID/1, V).

is_StreamID(V) -> 
    d("is_StreamID -> entry with"
      "~n   V: ~p", [V]),
    is_INTEGER(V, {range, 0, 65535}).

chk_opt_StreamID(V1, V2) ->
    chk_OPTIONAL('StreamID', V1, V2, fun is_StreamID/1, fun chk_StreamID/2).

chk_StreamID(ID, ID) ->
    chk_type(fun is_StreamID/1, 'StreamID', ID);
chk_StreamID(ID1, ID2) ->
    case (is_StreamID(ID1) andalso is_StreamID(ID2)) of
	true ->
	    not_equal('StreamID', ID1, ID2);
	false ->
	    wrong_type('StreamID', ID1, ID2)
    end.


%% -- EventsDescriptor -- 

is_EventsDescriptor(#'EventsDescriptor'{requestID = RID,
					eventList = EVL}) ->
    d("is_EventsDescriptor -> entry with"
      "~n   RID: ~p"
      "~n   EVL: ~p", [RID, EVL]),
    is_opt_RequestID(RID) andalso is_EventsDescriptor_eventList(EVL);
is_EventsDescriptor(_) ->
    false.

is_EventsDescriptor_eventList([]) ->
    true;
is_EventsDescriptor_eventList([H|T]) ->
    is_RequestedEvent(H) andalso is_EventsDescriptor_eventList(T);
is_EventsDescriptor_eventList(_) ->
    false.

chk_EventsDescriptor(D, D) ->
    chk_type(fun is_EventsDescriptor/1, 'EventsDescriptor', D);
chk_EventsDescriptor(#'EventsDescriptor'{requestID = RID1,
					 eventList = EVL1},
		     #'EventsDescriptor'{requestID = RID2,
					 eventList = EVL2}) ->
    validate(fun() -> chk_opt_RequestID(RID1, RID2) end, 'EventsDescriptor'),
    chk_EventsDescriptor_eventList(EVL1, EVL2),
    ok;
chk_EventsDescriptor(D1, D2) ->
    wrong_type('EventsDescriptor', D1, D2).

chk_EventsDescriptor_eventList([], []) ->
    ok;
chk_EventsDescriptor_eventList([] = EVL1, EVL2) ->
    not_equal('EventsDescriptor_eventList', EVL1, EVL2);
chk_EventsDescriptor_eventList(EVL1, [] = EVL2) ->
    not_equal('EventsDescriptor_eventList', EVL1, EVL2);
chk_EventsDescriptor_eventList([H|T1], [H|T2]) ->
    case is_RequestedEvent(H) of
	true ->
	    chk_EventsDescriptor_eventList(T1, T2);
	false ->
	    wrong_type('EventsDescriptor_eventList_val', H)
    end;
chk_EventsDescriptor_eventList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_RequestedEvent(H1, H2) end, 
	     'EventsDescriptor_eventList_val'),
    chk_EventsDescriptor_eventList(T1, T2);
chk_EventsDescriptor_eventList(EVL1, EVL2) ->
    wrong_type('EventsDescriptor_eventList', EVL1, EVL2).


%% -- RequestedEvent --

is_RequestedEvent(#'RequestedEvent'{pkgdName    = N,
				    streamID    = SID, 
				    eventAction = EA,
				    evParList   = EPL}) ->
    d("is_RequestedEvent -> entry with"
      "~n   N:   ~p"
      "~n   SID: ~p"
      "~n   EA:  ~p"
      "~n   EPL: ~p", [N, SID, EA, EPL]),
    is_PkgdName(N) andalso
	is_opt_StreamID(SID) andalso
	is_opt_RequestedActions(EA) andalso
	is_RequestedEvent_evParList(EPL);
is_RequestedEvent(_) ->
    false.

is_RequestedEvent_evParList([]) ->
    true;
is_RequestedEvent_evParList([H|T]) ->
    is_EventParameter(H) andalso is_RequestedEvent_evParList(T);
is_RequestedEvent_evParList(_) ->
    false.

chk_RequestedEvent(RE, RE) ->
    chk_type(fun is_RequestedEvent/1, 'RequestedEvent', RE);
chk_RequestedEvent(#'RequestedEvent'{pkgdName    = N1,
				     streamID    = SID1, 
				     eventAction = EA1,
				     evParList   = EPL1},
		   #'RequestedEvent'{pkgdName    = N2,
				     streamID    = SID2, 
				     eventAction = EA2,
				     evParList   = EPL2}) ->
    validate(fun() -> chk_PkgdName(N1, N2) end, 'RequestedEvent'),
    validate(fun() -> chk_opt_StreamID(SID1, SID2) end, 'RequestedEvent'),
    validate(fun() -> chk_opt_RequestedActions(EA1, EA2) end, 
	     'RequestedEvent'),
    chk_RequestedEvent_evParList(EPL1, EPL2),
    ok;
chk_RequestedEvent(RE1, RE2) ->
    wrong_type('RequestedEvent', RE1, RE2).
		     
chk_RequestedEvent_evParList([], []) ->
    ok;
chk_RequestedEvent_evParList([] = EPL1, EPL2) ->
    not_equal('RequestedEvent_evParList', EPL1, EPL2);
chk_RequestedEvent_evParList(EPL1, [] = EPL2) ->
    not_equal('RequestedEvent_evParList', EPL1, EPL2);
chk_RequestedEvent_evParList([H|T1], [H|T2]) ->
    case is_EventParameter(H) of
	true ->
	    chk_RequestedEvent_evParList(T1, T2);
	false ->
	    wrong_type('RequestedEvent_evParList_val', H)
    end;
chk_RequestedEvent_evParList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_EventParameter(H1, H2) end,
	     'RequestedEvent_evParList_val'),
    chk_RequestedEvent_evParList(T1, T2);
chk_RequestedEvent_evParList(EPL1, EPL2) ->
    wrong_type('RequestedEvent_evParList', EPL1, EPL2).


%% -- RegulatedEmbeddedDescriptor --

is_RegulatedEmbeddedDescriptor(
  #'RegulatedEmbeddedDescriptor'{secondEvent       = SE,
				 signalsDescriptor = SD}) ->
    is_opt_SecondEventsDescriptor(SE) andalso
	is_opt_SignalsDescriptor(SD);
is_RegulatedEmbeddedDescriptor(_) ->
    false.


chk_RegulatedEmbeddedDescriptor(
  #'RegulatedEmbeddedDescriptor'{secondEvent       = SE1,
				 signalsDescriptor = SD1},
  #'RegulatedEmbeddedDescriptor'{secondEvent       = SE2,
				 signalsDescriptor = SD2}) ->
    chk_RegulatedEmbeddedDescriptor_secondEvent(SE1, SE2),
    chk_RegulatedEmbeddedDescriptor_signalsDescriptor(SD1, SD2),
    ok;
chk_RegulatedEmbeddedDescriptor(RED1, RED2) ->
    wrong_type('RegulatedEmbeddedDescriptor', RED1, RED2).

chk_RegulatedEmbeddedDescriptor_secondEvent(SE1, SE2) ->
    validate(fun() -> chk_opt_SecondEventsDescriptor(SE1, SE2) end,
	     'RegulatedEmbeddedDescriptor_secondEvent').

chk_RegulatedEmbeddedDescriptor_signalsDescriptor(SD1, SD2) ->
    validate(fun() -> chk_opt_SignalsDescriptor(SD1, SD2) end,
	     'RegulatedEmbeddedDescriptor_signalsDescriptor').

		     
%% -- NotifyBehaviour --

tags_NotifyBehaviour() ->
    [
     {notifyImmediate, 
      fun is_NULL/1, 
      fun chk_NULL/2}, 

     {notifyRegulated, 
      fun is_RegulatedEmbeddedDescriptor/1,
      fun chk_RegulatedEmbeddedDescriptor/2}, 

     {neverNotify,
      fun is_NULL/1, 
      fun chk_NULL/2}
    ].

is_opt_NotifyBehaviour(asn1_NOVALUE) ->
    true;
is_opt_NotifyBehaviour(NB) ->
    is_NotifyBehaviour(NB).

is_NotifyBehaviour(NB) ->
    is_CHOICE(NB, tags_NotifyBehaviour()).

chk_opt_NotifyBehaviour(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_NotifyBehaviour(NB1, NB2) ->
    chk_NotifyBehaviour(NB1, NB2).

chk_NotifyBehaviour(NB1, NB2) ->
    chk_CHOICE(NB1, NB2, 'NotifyBehaviour', tags_NotifyBehaviour()).


%% -- RequestedActions --

is_opt_RequestedActions(asn1_NOVALUE) ->
    true;
is_opt_RequestedActions(RA) ->
    is_RequestedActions(RA).

is_RequestedActions(#'RequestedActions'{keepActive            = KA,
					eventDM               = EDM,
					secondEvent           = SE,
					signalsDescriptor     = SD,
					notifyBehaviour       = NB,
					resetEventsDescriptor = RED}) ->
    d("is_RequestedActions -> entry with"
      "~n   KA:  ~p"
      "~n   EDM: ~p"
      "~n   SE:  ~p"
      "~n   SD:  ~p"
      "~n   NB:  ~p"
      "~n   RED: ~p", [KA, EDM, SE, SD, NB, RED]),
    is_opt_BOOLEAN(KA) andalso
	is_opt_EventDM(EDM) andalso
	is_opt_SecondEventsDescriptor(SE) andalso
	is_opt_SignalsDescriptor(SD) andalso
	is_opt_NotifyBehaviour(SD) andalso
	is_opt_NULL(SD);
is_RequestedActions(_) ->
    false.

chk_opt_RequestedActions(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_RequestedActions(RA1, RA2) ->
    chk_RequestedActions(RA1, RA2).

chk_RequestedActions(RA, RA) ->
    chk_type(fun is_RequestedActions/1, 'RequestedActions', RA);
chk_RequestedActions(#'RequestedActions'{keepActive            = KA1,
					 eventDM               = EDM1,
					 secondEvent           = SE1,
					 signalsDescriptor     = SD1,
					 notifyBehaviour       = NB1,
					 resetEventsDescriptor = RED1},
		     #'RequestedActions'{keepActive            = KA2,
					 eventDM               = EDM2,
					 secondEvent           = SE2,
					 signalsDescriptor     = SD2,
					 notifyBehaviour       = NB2,
					 resetEventsDescriptor = RED2}) ->
    chk_RequestedActions_keepActive(KA1, KA2),
    chk_RequestedActions_eventDM(EDM1, EDM2),
    chk_RequestedActions_secondEvent(SE1, SE2),
    chk_RequestedActions_signalsDescriptor(SD1, SD2),
    chk_RequestedActions_notifyBehaviour(NB1, NB2),
    chk_RequestedActions_resetEventsDescriptor(RED1, RED2),
    ok;
chk_RequestedActions(RA1, RA2) ->
    wrong_type('RequestedActions', RA1, RA2).

chk_RequestedActions_keepActive(V1, V2) ->
    validate(fun() -> chk_opt_BOOLEAN(V1, V2) end, 
	     'RequestedActions_keepActive').

chk_RequestedActions_eventDM(V1, V2) ->
    validate(fun() -> chk_opt_EventDM(V1, V2) end, 
	     'RequestedActions_eventDM').

chk_RequestedActions_secondEvent(V1, V2) ->
    validate(fun() -> chk_opt_SecondEventsDescriptor(V1, V2) end, 
	     'RequestedActions_secondEvent').

chk_RequestedActions_signalsDescriptor(V1, V2) ->
    validate(fun() -> chk_opt_SignalsDescriptor(V1, V2) end, 
	     'RequestedActions_signalsDescriptor').

chk_RequestedActions_notifyBehaviour(V1, V2) ->
    validate(fun() -> chk_opt_NotifyBehaviour(V1, V2) end, 
	     'RequestedActions_notifyBehaviour').

chk_RequestedActions_resetEventsDescriptor(V1, V2) ->
    validate(fun() -> chk_opt_NULL(V1, V2) end, 
	     'RequestedActions_resetEventsDescriptor').


%% -- EventDM --

is_opt_EventDM(EDM) ->
    is_OPTIONAL(fun is_EventDM/1, EDM).

is_EventDM({Tag, Val}) ->
    is_EventDM_tag(Tag) andalso is_EventDM_val(Tag, Val);
is_EventDM(_) ->
    false.

is_EventDM_tag(Tag) ->
    Tags = [digitMapName, digitMapValue],
    lists:member(Tag, Tags).

is_EventDM_val(digitMapName, Val) ->
    is_DigitMapName(Val);
is_EventDM_val(digitMapValue, Val) ->
    is_DigitMapValue(Val).

chk_opt_EventDM(EDM1, EDM2) ->
    chk_OPTIONAL('EventDM', EDM1, EDM2, fun is_EventDM/1, fun chk_EventDM/2).

chk_EventDM(EDM, EDM) ->
    chk_type(fun is_EventDM/1, 'EventDM', EDM);
chk_EventDM({Tag, Val1} = EDM1, {Tag, Val2} = EDM2) ->
    case (is_EventDM_tag(Tag) andalso
	  is_EventDM_val(Tag, Val1) andalso
	  is_EventDM_val(Tag, Val2)) of
	true ->
	    chk_EventDM_val(Tag, Val1, Val2);
	false ->
	    wrong_type('EventDM', EDM1, EDM2)
    end;
chk_EventDM({Tag1, Val1} = EDM1, {Tag2, Val2} = EDM2) ->
    case ((is_EventDM_tag(Tag1) andalso
	   is_EventDM_val(Tag1, Val1)) andalso
	  (is_EventDM_tag(Tag2) andalso
	   is_EventDM_val(Tag2, Val2))) of
	true ->
	    not_equal('EventDM', EDM1, EDM2);
	false ->
	    wrong_type('EventDM', EDM1, EDM2)
    end;
chk_EventDM(EDM1, EDM2) ->
    wrong_type('EventDM', EDM1, EDM2).

chk_EventDM_val(digitMapName, Val1, Val2) ->
    validate(fun() -> chk_DigitMapName(Val1, Val2) end, 'EventDM');
chk_EventDM_val(digitMapValue, Val1, Val2) ->
    validate(fun() -> chk_DigitMapValue(Val1, Val2) end, 'EventDM').


%% -- SecondEventsDescriptor --

is_opt_SecondEventsDescriptor(asn1_NOVALUE) ->
    true;
is_opt_SecondEventsDescriptor(D) ->
    is_SecondEventsDescriptor(D).

is_SecondEventsDescriptor(#'SecondEventsDescriptor'{requestID = RID, 
						    eventList = EL}) ->
    is_opt_RequestID(RID) andalso is_SecondEventsDescriptor_eventList(EL);
is_SecondEventsDescriptor(_) ->
    false.

is_SecondEventsDescriptor_eventList([]) ->
    true;
is_SecondEventsDescriptor_eventList([H|T]) ->
    is_SecondRequestedEvent(H) andalso is_SecondEventsDescriptor_eventList(T);
is_SecondEventsDescriptor_eventList(_) ->
    false.

chk_opt_SecondEventsDescriptor(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_SecondEventsDescriptor(D1, D2) ->
    chk_SecondEventsDescriptor(D1, D2).

chk_SecondEventsDescriptor(D, D) ->
    chk_type(fun is_SecondEventsDescriptor/1, 'SecondEventsDescriptor', D);
chk_SecondEventsDescriptor(#'SecondEventsDescriptor'{requestID = RID1, 
						     eventList = EL1},
			   #'SecondEventsDescriptor'{requestID = RID2, 
						     eventList = EL2}) ->
    validate(fun() -> chk_opt_RequestID(RID1, RID2) end, 
	     'SecondEventsDescriptor'),
    chk_SecondEventsDescriptor_eventList(EL1, EL2),
    ok;
chk_SecondEventsDescriptor(D1, D2) ->
    wrong_type('SecondEventsDescriptor', D1, D2).
 
chk_SecondEventsDescriptor_eventList([], []) ->
    ok;
chk_SecondEventsDescriptor_eventList([] = EL1, EL2) ->
    not_equal('SecondEventsDescriptor_eventList', EL1, EL2);
chk_SecondEventsDescriptor_eventList(EL1, [] = EL2) ->
    not_equal('SecondEventsDescriptor_eventList', EL1, EL2);
chk_SecondEventsDescriptor_eventList([H|T1], [H|T2]) ->
    case is_SecondRequestedEvent(H) of
	true ->
	    chk_SecondEventsDescriptor_eventList(T1, T2);
	false ->
	    wrong_type('SecondEventsDescriptor_eventList_val', H)
    end;
chk_SecondEventsDescriptor_eventList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_SecondRequestedEvent(H1, H2) end,
	     'SecondEventsDescriptor_eventList_val'),
    chk_SecondEventsDescriptor_eventList(T1, T2);
chk_SecondEventsDescriptor_eventList(L1, L2) ->
    wrong_type('SecondEventsDescriptor_eventList_val', L1, L2).


%% -- SecondRequestedEvent --

is_SecondRequestedEvent(#'SecondRequestedEvent'{pkgdName    = N,
						streamID    = SID, 
						eventAction = EA,
						evParList   = EPL}) ->
    is_PkgdName(N) andalso
	is_opt_StreamID(SID) andalso
	is_opt_SecondRequestedActions(EA) andalso
	is_SecondRequestedEvent_evParList(EPL);
is_SecondRequestedEvent(_) ->
    false.

is_SecondRequestedEvent_evParList([]) ->
    true;
is_SecondRequestedEvent_evParList([H|T]) ->
    is_EventParameter(H) andalso is_SecondRequestedEvent_evParList(T);
is_SecondRequestedEvent_evParList(_) ->
    false.

chk_SecondRequestedEvent(RE, RE) ->
    chk_type(fun is_SecondRequestedEvent/1, 'SecondRequestedEvent', RE);
chk_SecondRequestedEvent(#'SecondRequestedEvent'{pkgdName    = N1,
						 streamID    = SID1, 
						 eventAction = EA1,
						 evParList   = EPL1},
			 #'SecondRequestedEvent'{pkgdName    = N2,
						 streamID    = SID2, 
						 eventAction = EA2,
						 evParList   = EPL2}) ->
    validate(fun() -> chk_PkgdName(N1, N2) end, 'SecondRequestedEvent'),
    validate(fun() -> chk_opt_StreamID(SID1, SID2) end, 
	     'SecondRequestedEvent'),
    validate(fun() -> chk_opt_SecondRequestedActions(EA1, EA2) end, 
	     'SecondRequestedEvent'),
    chk_SecondRequestedEvent_evParList(EPL1, EPL2),
    ok;
chk_SecondRequestedEvent(RE1, RE2) ->
    wrong_type('SecondRequestedEvent', RE1, RE2).
		     
chk_SecondRequestedEvent_evParList([], []) ->
    ok;
chk_SecondRequestedEvent_evParList([] = EPL1, EPL2) ->
    not_equal('SecondRequestedEvent_evParList', EPL1, EPL2);
chk_SecondRequestedEvent_evParList(EPL1, [] = EPL2) ->
    not_equal('SecondRequestedEvent_evParList', EPL1, EPL2);
chk_SecondRequestedEvent_evParList([H|T1], [H|T2]) ->
    case is_EventParameter(H) of
	true ->
	    chk_SecondRequestedEvent_evParList(T1, T2);
	false ->
	    wrong_type('SecondRequestedEvent_evParList_val', H)
    end;
chk_SecondRequestedEvent_evParList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_EventParameter(H1, H2) end, 
	     'SecondRequestedEvent_evParList_val'),
    chk_SecondRequestedEvent_evParList(T1, T2);
chk_SecondRequestedEvent_evParList(EPL1, EPL2) ->
    wrong_type('SecondRequestedEvent_evParList', EPL1, EPL2).


%% -- SecondRequestedActions --

is_opt_SecondRequestedActions(asn1_NOVALUE) ->
    true;
is_opt_SecondRequestedActions(SRA) ->
    is_SecondRequestedActions(SRA).

is_SecondRequestedActions(
  #'SecondRequestedActions'{keepActive            = KA,
			    eventDM               = EDM,
			    signalsDescriptor     = SD,
			    notifyBehaviour       = NB,
			    resetEventsDescriptor = RED}) ->
    is_opt_BOOLEAN(KA) andalso
	is_opt_EventDM(EDM) andalso
	is_opt_SignalsDescriptor(SD) andalso 
	is_opt_NotifyBehaviour(NB) andalso
	is_opt_NULL(RED);
is_SecondRequestedActions(_) ->
    false.

chk_opt_SecondRequestedActions(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_SecondRequestedActions(SRA1, SRA2) ->
    chk_SecondRequestedActions(SRA1, SRA2).

chk_SecondRequestedActions(SRA, SRA) ->
    chk_type(fun is_SecondRequestedActions/1, 'SecondRequestedActions', SRA);
chk_SecondRequestedActions(
  #'SecondRequestedActions'{keepActive            = KA1,
			    eventDM               = EDM1,
			    signalsDescriptor     = SD1,
			    notifyBehaviour       = NB1,
			    resetEventsDescriptor = RED1},
  #'SecondRequestedActions'{keepActive            = KA2,
			    eventDM               = EDM2,
			    signalsDescriptor     = SD2,
			    notifyBehaviour       = NB2,
			    resetEventsDescriptor = RED2}) ->
    chk_SecondRequestedActions_keepActive(KA1, KA2),
    chk_SecondRequestedActions_eventDM(EDM1, EDM2),
    chk_SecondRequestedActions_signalsDescriptor(SD1, SD2),
    chk_SecondRequestedActions_notifyBehaviour(NB1, NB2),
    chk_SecondRequestedActions_resetEventsDescriptor(RED1, RED2),
    ok;
chk_SecondRequestedActions(SRA1, SRA2) ->
    wrong_type('SecondRequestedActions', SRA1, SRA2).

chk_SecondRequestedActions_keepActive(V1, V2) ->
    validate(fun() -> chk_opt_BOOLEAN(V1, V2) end, 
	     'SecondRequestedActions_keepActive').

chk_SecondRequestedActions_eventDM(V1, V2) ->
    validate(fun() -> chk_opt_EventDM(V1, V2) end, 
	     'SecondRequestedActions_eventDM').

chk_SecondRequestedActions_signalsDescriptor(V1, V2) ->
    validate(fun() -> chk_opt_SignalsDescriptor(V1, V2) end, 
	     'SecondRequestedActions_signalsDescriptor').

chk_SecondRequestedActions_notifyBehaviour(V1, V2) ->
    validate(fun() -> chk_opt_NotifyBehaviour(V1, V2) end, 
	     'SecondRequestedActions_notifyBehaviour').

chk_SecondRequestedActions_resetEventsDescriptor(V1, V2) ->
    validate(fun() -> chk_opt_NULL(V1, V2) end, 
	     'SecondRequestedActions_resetEventsDescriptor').


%% -- EventBufferDescriptor --

is_EventBufferDescriptor(D) ->
    is_SEQUENCE_OF(D, fun is_EventSpec/1).

chk_EventBufferDescriptor(D1, D2) ->
    chk_SEQUENCE_OF(D1, D2, 'EventBufferDescriptor',
		    fun is_EventSpec/1, fun chk_EventSpec/2).


%% -- EventSpec --

is_EventSpec(#'EventSpec'{eventName    = N, 
			  streamID     = SID, 
			  eventParList = EPL}) ->
    is_EventName(N) andalso 
	is_opt_StreamID(SID) andalso 
	is_EventSpec_eventParList(EPL);
is_EventSpec(_) ->
    false.

is_EventSpec_eventParList([]) ->
    true;
is_EventSpec_eventParList([H|T]) ->
    is_EventParameter(H) andalso is_EventSpec_eventParList(T);
is_EventSpec_eventParList(_) ->
    false.

chk_EventSpec(ES, ES) ->
    chk_type(fun is_EventSpec/1, 'EventSpec', ES);
chk_EventSpec(#'EventSpec'{eventName    = N1, 
			   streamID     = SID1, 
			   eventParList = EPL1},
	      #'EventSpec'{eventName    = N2, 
			   streamID     = SID2, 
			   eventParList = EPL2}) ->
    validate(fun() -> chk_EventName(N1, N2) end, 'EventSpec'),
    validate(fun() -> chk_opt_StreamID(SID1, SID2) end, 'EventSpec'),
    chk_EventSpec_eventParList(EPL1, EPL2),
    ok;
chk_EventSpec(ES1, ES2) ->
    wrong_type('EventSpec', ES1, ES2).
    
chk_EventSpec_eventParList([], []) ->
    ok;
chk_EventSpec_eventParList([] = EPL1, EPL2) ->
    not_equal('EventSpec_eventParList', EPL1, EPL2);
chk_EventSpec_eventParList(EPL1, [] = EPL2) ->
    not_equal('EventSpec_eventParList', EPL1, EPL2);
chk_EventSpec_eventParList([H|T1], [H|T2]) ->
    case is_EventParameter(H) of
	true ->
	    chk_EventSpec_eventParList(T1, T2);
	false ->
	    wrong_type('EventSpec_eventParList_val', H)
    end;
chk_EventSpec_eventParList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_EventParameter(H1, H2) end, 
	     'EventSpec_eventParList_val'),
    chk_EventSpec_eventParList(T1, T2);
chk_EventSpec_eventParList(EPL1, EPL2) ->
    wrong_type('EventSpec_eventParList', EPL1, EPL2).


%% -- SignalsDescriptor -- 

is_opt_SignalsDescriptor(asn1_NOVALUE) ->
    true;
is_opt_SignalsDescriptor(D) ->
    is_SignalsDescriptor(D).

is_SignalsDescriptor(D) ->
    is_SEQUENCE_OF(D, fun is_SignalRequest/1).

chk_opt_SignalsDescriptor(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_SignalsDescriptor(D1, D2) ->
    chk_SignalsDescriptor(D1, D2).

chk_SignalsDescriptor(D1, D2) ->
    chk_SEQUENCE_OF(D1, D2, 'SignalsDescriptor',
		    fun is_SignalRequest/1, fun chk_SignalRequest/2).


%% -- SignalRequest --

tags_SignalRequest() ->
    [
     {signal,     fun is_Signal/1,     fun chk_Signal/2},
     {seqSigList, fun is_SeqSigList/1, fun chk_SeqSigList/2}
    ].

is_SignalRequest(SR) ->
    is_CHOICE(SR, tags_SignalRequest()).

chk_SignalRequest(R1, R2) ->
    chk_CHOICE(R1, R2, 'SignalRequest', tags_SignalRequest()).


%% -- SeqSigList --

%% fields_SeqSigList() ->
%%     [
%%      {id, 
%%       'INTEGER(0,65535)', 
%%       fun(ID) -> is_INTEGER(ID, {range, 0, 65535}) end, 
%%       fun(ID1, ID2) -> chk_INTEGER(ID1, ID2, {range, 0, 65535}) end},

%%      {signalList, 
%%       'SEQUENCE OF Signal', 
%%       fun(S) -> 
%% 	      is_SEQUENCE_OF(S, fun is_Signal/1) 
%%       end,
%%       fun(S1, S2) -> 
%% 	      chk_SEQUENCE_OF(S1, S2, fun is_Signal/1, fun chk_Signal/2) 
%%       end}
%%     ].

%% is_SeqSigList(V) ->
%%     is_SEQUENCE(V, 'SeqSigList', fields_SeqSigList()).


is_SeqSigList(#'SeqSigList'{id         = ID,
			    signalList = SL}) ->
    is_INTEGER(ID, {range, 0, 65535}) andalso 
	is_SeqSigList_signalList(SL);
is_SeqSigList(_) ->
    false.

is_SeqSigList_signalList([]) ->
    true;
is_SeqSigList_signalList([H|T]) ->
    is_Signal(H) andalso is_SeqSigList_signalList(T);
is_SeqSigList_signalList(_) ->
    false.

chk_SeqSigList(L, L) ->
    chk_type(fun is_SeqSigList/1, 'SeqSigList', L);
chk_SeqSigList(#'SeqSigList'{id         = ID1,
			     signalList = SL1},
	       #'SeqSigList'{id         = ID2,
			     signalList = SL2}) ->
    validate(fun() -> chk_INTEGER(ID1, ID2, {range, 0, 65535}) end, 
	     'SeqSigList'),
    chk_SeqSigList_signalList(SL1, SL2),
    ok;
chk_SeqSigList(L1, L2) ->
    wrong_type('SeqSigList', L1, L2).
		     
chk_SeqSigList_signalList([], []) ->
    ok;
chk_SeqSigList_signalList([] = L1, L2) ->
    not_equal('SeqSigList_signalList', L1, L2);
chk_SeqSigList_signalList(L1, [] = L2) ->
    not_equal('SeqSigList_signalList', L1, L2);
chk_SeqSigList_signalList([H|T1], [H|T2]) ->
    case is_Signal(H) of
	true ->
	    chk_SeqSigList_signalList(T1, T2);
	false ->
	    wrong_type('SeqSigList_signalList_val', H)
    end;
chk_SeqSigList_signalList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_Signal(H1, H2) end, 
	     'SeqSigList_signalList_val'),
    chk_SeqSigList_signalList(T1, T2);
chk_SeqSigList_signalList(L1, L2) ->
    wrong_type('SeqSigList_signalList', L1, L2).


%% -- Signal --

is_Signal(#'Signal'{signalName       = N, 
		    streamID         = SID, 
		    sigType          = ST, 
		    duration         = Dur,
		    notifyCompletion = NC, 
		    keepActive       = KA, 
		    sigParList       = SPL,
		    direction        = Dir,
		    requestID        = RID,
		    intersigDelay    = ISD}) ->
    d("is_Signal -> entry with"
      "~n   N:   ~p"
      "~n   SID: ~p"
      "~n   ST:  ~p"
      "~n   Dur: ~p"
      "~n   NC:  ~p"
      "~n   KA:  ~p"
      "~n   SPL: ~p"
      "~n   Dir: ~p"
      "~n   RID: ~p"
      "~n   ISD: ~p", [N, SID, ST, Dur, NC, KA, SPL, Dir, RID, ISD]),
    is_SignalName(N) andalso 
	is_opt_StreamID(SID) andalso
	is_opt_SignalType(ST) andalso
	is_opt_INTEGER(Dur, {range, 0, 65535}) andalso
	is_opt_NotifyCompletion(NC) andalso
	is_opt_BOOLEAN(KA) andalso
	is_Signal_sigParList(SPL) andalso
	is_opt_SignalDirection(Dir) andalso
	is_opt_RequestID(RID) andalso
	is_opt_INTEGER(ISD, {range, 0, 65535}).

is_Signal_sigParList([]) ->
    true;
is_Signal_sigParList([H|T]) ->
    is_SigParameter(H) andalso is_Signal_sigParList(T);
is_Signal_sigParList(_) ->
    false.

chk_Signal(S, S) ->
    chk_type(fun is_Signal/1, 'Signal', S);
chk_Signal(#'Signal'{signalName       = N1, 
		     streamID         = SID1, 
		     sigType          = ST1, 
		     duration         = Dur1,
		     notifyCompletion = NC1, 
		     keepActive       = KA1, 
		     sigParList       = SPL1,
		     direction        = Dir1,
		     requestID        = RID1,
		     intersigDelay    = ISD1},
	   #'Signal'{signalName       = N2, 
		     streamID         = SID2, 
		     sigType          = ST2, 
		     duration         = Dur2,
		     notifyCompletion = NC2, 
		     keepActive       = KA2, 
		     sigParList       = SPL2,
		     direction        = Dir2,
		     requestID        = RID2,
		     intersigDelay    = ISD2}) ->
    chk_Signal_signalName(N1, N2),
    chk_Signal_streamID(SID1, SID2),
    chk_Signal_sigType(ST1, ST2),
    chk_Signal_duration(Dur1, Dur2),
    chk_Signal_notifyCompletion(NC1, NC2),
    chk_Signal_keepActive(KA1, KA2),
    chk_Signal_sigParList(SPL1, SPL2),
    chk_Signal_direction(Dir1, Dir2),
    chk_Signal_requestID(RID1, RID2),
    chk_Signal_intersigDelay(ISD1, ISD2),
    ok;
chk_Signal(S1, S2) ->
    wrong_type('Signal', S1, S2).

chk_Signal_signalName(N1, N2) ->
    validate(fun() -> chk_SignalName(N1, N2) end, 'Signal_signalName').

chk_Signal_streamID(V1, V2) ->
    validate(fun() -> chk_opt_StreamID(V1, V2) end, 'Signal_streamID').

chk_Signal_sigType(V1, V2) ->
    validate(fun() -> chk_opt_SignalType(V1, V2) end, 'Signal_sigType').

chk_Signal_duration(V1, V2) ->
    validate(fun() -> 
		     chk_opt_INTEGER(V1, V2, {range, 0, 65535}) 
	     end, 
	     'Signal_duration').

chk_Signal_notifyCompletion(V1, V2) ->
    validate(fun() -> chk_opt_NotifyCompletion(V1, V2) end, 
	     'Signal_notifyCompletion').

chk_Signal_keepActive(false, asn1_NOVALUE) ->
    ok;
chk_Signal_keepActive(asn1_NOVALUE, false) ->
    ok;
chk_Signal_keepActive(V1, V2) ->
    validate(fun() -> chk_opt_BOOLEAN(V1, V2) end, 'Signal_keepActive').

chk_Signal_sigParList([], []) ->
    ok;
chk_Signal_sigParList([] = L1, L2) ->
    not_equal('Signal_sigParList', L1, L2);
chk_Signal_sigParList(L1, [] = L2) ->
    not_equal('Signal_sigParList', L1, L2);
chk_Signal_sigParList([H|T1], [H|T2]) ->
    case is_SigParameter(H) of
	true ->
	    chk_Signal_sigParList(T1, T2);
	false ->
	    wrong_type('Signal_sigParList_val', H)
    end;
chk_Signal_sigParList([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_SigParameter(H1, H2) end, 
	     'Signal_sigParList_val'),
    chk_Signal_sigParList(T1, T2);
chk_Signal_sigParList(L1, L2) ->
    wrong_type('Signal_sigParList', L1, L2).

chk_Signal_direction(V1, V2) ->
    validate(fun() -> chk_opt_SignalDirection(V1, V2) end, 'Signal_direction').

chk_Signal_requestID(V1, V2) ->
    validate(fun() -> chk_opt_RequestID(V1, V2) end, 'Signal_requestID').

chk_Signal_intersigDelay(V1, V2) ->
    validate(fun() -> chk_opt_INTEGER(V1, V2, {range, 0, 65535}) end, 
	     'Signal_intersigDelay').


%% -- SignalType --

is_opt_SignalType(T) ->
    is_OPTIONAL(fun is_SignalType/1, T).

is_SignalType(T) ->
    d("is_SignalType -> entry with"
      "~n   T: ~p", [T]),
    Types = [brief, onOff, timeOut],
    lists:member(T, Types).

chk_opt_SignalType(T1, T2) ->
    chk_OPTIONAL('SignalType', T1, T2,
		 fun is_SignalType/1, fun chk_SignalType/2).

chk_SignalType(T, T) ->
    chk_type(fun is_SignalType/1, 'SignalType', T);
chk_SignalType(T1, T2) ->
    case (is_SignalType(T1) andalso is_SignalType(T2)) of
	true ->
	    not_equal('SignalType', T1, T2);
	false ->
	    wrong_type('SignalType', T1, T2)
    end.


%% -- SignalDirection --

is_opt_SignalDirection(T) ->
    is_OPTIONAL(fun is_SignalDirection/1, T).

is_SignalDirection(Dir) ->
    d("is_SignalDirection -> entry with"
      "~n   Dir: ~p", [Dir]),
    Dirs = [internal, external, both],
    lists:member(Dir, Dirs).

chk_opt_SignalDirection(T1, T2) ->
    chk_OPTIONAL('SignalDirection', T1, T2,
		 fun is_SignalDirection/1, fun chk_SignalDirection/2).

chk_SignalDirection(Dir, Dir) ->
    chk_type(fun is_SignalDirection/1, 'SignalDirection', Dir);
chk_SignalDirection(Dir1, Dir2) ->
    case (is_SignalDirection(Dir1) andalso is_SignalDirection(Dir2)) of
	true ->
	    not_equal('SignalDirection', Dir1, Dir2);
	false ->
	    wrong_type('SignalDirection', Dir1, Dir2)
    end.


%% -- SignalName --

is_SignalName(N) -> 
    d("is_SignalName -> entry with"
      "~n   N: ~p", [N]),
    is_PkgdName(N).

chk_SignalName(N1, N2) ->
    validate(fun() -> chk_PkgdName(N1, N2) end, 'SignalName').

		     
%% -- NotifyCompletion --

is_opt_NotifyCompletion(NC) ->
    is_OPTIONAL(fun is_NotifyCompletion/1, NC).
			
is_NotifyCompletion(NC) ->
    d("is_NotifyCompletion -> entry with"
      "~n   NC: ~p", [NC]),
    Valids = [onTimeOut, 
	      onInterruptByEvent, 
	      onInterruptByNewSignalDescr, 
	      otherReason],
    lists:all(fun(X) -> lists:member(X, Valids) end, NC).

chk_opt_NotifyCompletion(NC1, NC2) ->
    chk_OPTIONAL('NotifyCompletion', NC1, NC2,
		 fun is_NotifyCompletion/1,
		 fun chk_NotifyCompletion/2).

chk_NotifyCompletion(NC, NC) ->
    chk_type(fun is_NotifyCompletion/1, 'NotifyCompletion', NC);
chk_NotifyCompletion(NC1, NC2) ->
    case (is_NotifyCompletion(NC1) andalso is_NotifyCompletion(NC2)) of
	true ->
	    not_equal('NotifyCompletion', NC1, NC2);
	false ->
	    wrong_type('NotifyCompletion', NC1, NC2)
    end.

    
%% -- SigParameter --

is_SigParameter(#'SigParameter'{sigParameterName = N,
				value            = V,
				extraInfo        = I}) ->
    d("is_SigParameter -> entry with"
      "~n   N: ~p"
      "~n   V: ~p"
      "~n   I: ~p", [N, V, I]),
    is_Name(N) andalso 
	is_Value(V) andalso 
	is_SigParameter_extraInfo(I);
is_SigParameter(_) ->
    false.

is_SigParameter_extraInfo(asn1_NOVALUE) ->
    true; 
is_SigParameter_extraInfo({Tag, Val}) ->
    is_SigParameter_extraInfo_tag(Tag) andalso 
	is_SigParameter_extraInfo_val(Tag, Val);
is_SigParameter_extraInfo(_) ->
    false.

is_SigParameter_extraInfo_tag(Tag) ->
    Tags = [relation, range, sublist],
    lists:member(Tag, Tags).

is_SigParameter_extraInfo_val(relation, Val) ->
    is_Relation(Val);
is_SigParameter_extraInfo_val(range, Val) ->
    is_BOOLEAN(Val);
is_SigParameter_extraInfo_val(sublist, Val) ->
    is_BOOLEAN(Val).

chk_SigParameter(P, P) ->
    chk_type(fun is_SigParameter/1, 'SigParameter', P);
chk_SigParameter(#'SigParameter'{sigParameterName = N1,
				 value            = V1,
				 extraInfo        = I1}, 
		 #'SigParameter'{sigParameterName = N2,
				 value            = V2,
				 extraInfo        = I2}) ->
    validate(fun() -> chk_Name(N1, N2) end, 'SigParameter'),
    validate(fun() -> chk_Value(V1, V2) end, 'SigParameter'),
    chk_SigParameter_extraInfo(I1, I2), 
    ok;
chk_SigParameter(P1, P2) ->
    wrong_type('SigParameter', P1, P2).

chk_SigParameter_extraInfo(EI, EI) ->
    chk_type(fun is_SigParameter_extraInfo/1, 'SigParameter_extraInfo', EI);
chk_SigParameter_extraInfo({Tag, Val1} = EI1, {Tag, Val2} = EI2) ->
    case (is_SigParameter_extraInfo_tag(Tag) and
	  is_SigParameter_extraInfo_val(Tag, Val1) and
	  is_SigParameter_extraInfo_val(Tag, Val2)) of
	true ->
	    chk_SigParameter_extraInfo_val(Tag, Val1, Val2);
	false ->
	    wrong_type('SigParameter_extraInfo', EI1, EI2)
    end;
chk_SigParameter_extraInfo({Tag1, Val1} = EI1, {Tag2, Val2} = EI2) ->
    case ((is_SigParameter_extraInfo_tag(Tag1) and
	   is_SigParameter_extraInfo_val(Tag1, Val1)) and
	  (is_SigParameter_extraInfo_tag(Tag2) and
	   is_SigParameter_extraInfo_val(Tag2, Val2))) of
	true ->
	    not_equal('SigParameter_extraInfo', EI1, EI2);
	false ->
	    wrong_type('SigParameter_extraInfo', EI1, EI2)
    end;
chk_SigParameter_extraInfo(EI1, EI2) ->
    wrong_type('SigParameter_extraInfo', EI1, EI2).

chk_SigParameter_extraInfo_val(relation, Val1, Val2) ->
    validate(fun() -> chk_Relation(Val1, Val2) end, 'SigParameter_extraInfo');
chk_SigParameter_extraInfo_val(range, Val1, Val2) ->
    validate(fun() -> chk_BOOLEAN(Val1, Val2) end, 'SigParameter_extraInfo');
chk_SigParameter_extraInfo_val(sublist, Val1, Val2) ->
    validate(fun() -> chk_BOOLEAN(Val1, Val2) end, 'SigParameter_extraInfo').


%% -- RequestID -- 

is_opt_RequestID(asn1_NOVALUE) -> 
    true;
is_opt_RequestID(V) ->
    is_RequestID(V).

is_RequestID(V) -> is_INTEGER(V, {range, 0, 4294967295}).

chk_opt_RequestID(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_RequestID(V1, V2) ->
    chk_RequestID(V1, V2).

chk_RequestID(ID, ID) ->
    chk_type(fun is_RequestID/1, 'RequestID', ID);
chk_RequestID(ID1, ID2) ->
    case (is_RequestID(ID1) andalso is_RequestID(ID2)) of
	true ->
	    not_equal('RequestID', ID1, ID2);
	false ->
	    wrong_type('RequestID', ID1, ID2)
    end.


%% -- ModemDescriptor -- 

is_ModemDescriptor(D) when is_record(D, 'ModemDescriptor') ->
    true;
is_ModemDescriptor(_) ->
    false.

chk_ModemDescriptor(D, D) when is_record(D, 'ModemDescriptor') ->
    ok;
chk_ModemDescriptor(#'ModemDescriptor'{mtl             = MTL1,
				       mpl             = MPL1,
				       nonStandardData = NSD1}, 
		    #'ModemDescriptor'{mtl             = MTL2,
				       mpl             = MPL2,
				       nonStandardData = NSD2}) ->
    chk_ModemDescriptor_mtl(MTL1, MTL2),
    chk_ModemDescriptor_mpl(MPL1, MPL2),
    chk_opt_NonStandardData(NSD1, NSD2),
    ok;
chk_ModemDescriptor(D1, D2) ->
    wrong_type('ModemDescriptor', D1, D2).

chk_ModemDescriptor_mtl([], []) ->
    ok;
chk_ModemDescriptor_mtl([] = MTL1, MTL2) ->
    not_equal('ModemDescriptor_mtl', MTL1, MTL2);
chk_ModemDescriptor_mtl(MTL1, [] = MTL2) ->
    not_equal('ModemDescriptor_mtl', MTL1, MTL2);
chk_ModemDescriptor_mtl([H|T1], [H|T2]) ->
    case is_ModemType(H) of 
	true ->
	    chk_ModemDescriptor_mtl(T1, T2);
	false ->
	    wrong_type('ModemDescriptor_mtl_val', H)
    end;
chk_ModemDescriptor_mtl([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_ModemType(H1, H2) end, 'ModemDescriptor_mtl_val'),
    chk_ModemDescriptor_mtl(T1, T2);
chk_ModemDescriptor_mtl(MTL1, MTL2) ->
    wrong_type('ModemDescriptor_mtl', MTL1, MTL2).


chk_ModemDescriptor_mpl([], []) ->
    ok;
chk_ModemDescriptor_mpl([] = MPL1, MPL2) ->
    not_equal('ModemDescriptor_mpl', MPL1, MPL2);
chk_ModemDescriptor_mpl(MPL1, [] = MPL2) ->
    not_equal('ModemDescriptor_mpl', MPL1, MPL2);
chk_ModemDescriptor_mpl([H|T1], [H|T2]) ->
    case is_PropertyParm(H) of
	true ->
	    chk_ModemDescriptor_mpl(T1, T2);
	false ->
	    wrong_type('ModemDescriptor_mpl_val', H)
    end;
chk_ModemDescriptor_mpl([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_PropertyParm(H1, H2) end, 'ModemDescriptor_mpl_val'),
    chk_ModemDescriptor_mpl(T1, T2);
chk_ModemDescriptor_mpl(MPL1, MPL2) ->
    wrong_type('ModemDescriptor_mpl', MPL1, MPL2).
		     

%% -- ModemType --
		     
chk_ModemType(MT, MT) ->
    case is_ModemType(MT) of
	true ->
	    ok;
	false ->
	    wrong_type('ModemType', MT, MT)
    end;
chk_ModemType(MT1, MT2) ->
    case (is_ModemType(MT1) andalso is_ModemType(MT2)) of
	true ->
	    not_equal('ModemType', MT1, MT2);
	false ->
	    wrong_type('ModemType', MT1, MT2)
    end.

is_ModemType(MT) ->
    lists:member(MT, 
		 [v18, v22, v22bis, v32, v32bis, v34, v90, v91, synchISDN]).
    

%% -- DigitMapDescriptor --

is_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = Name,
					    digitMapValue = Val}) ->
    is_opt_DigitMapName(Name) andalso is_opt_DigitMapValue(Val);
is_DigitMapDescriptor(_) ->
    false.

chk_DigitMapDescriptor(D, D) ->
    chk_type(fun is_DigitMapDescriptor/1, 'DigitMapDescriptor', D);
chk_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = Name1,
					     digitMapValue = Val1},
		       #'DigitMapDescriptor'{digitMapName  = Name2,
					     digitMapValue = Val2}) ->
    d("chk_DigitMapDescriptor -> entry with"
      "~n   Name1: ~p"
      "~n   Name2: ~p"
      "~n   Val1:  ~p"
      "~n   Val2:  ~p", [Name1, Name2, Val1, Val2]),
    validate(fun() -> chk_opt_DigitMapName(Name1, Name2) end,
		      'DigitMapDescriptor'),
    validate(fun() -> chk_opt_DigitMapValue(Val1, Val2) end,
		      'DigitMapDescriptor'),
    ok;
chk_DigitMapDescriptor(D1, D2) ->
    wrong_type('DigitMapDescriptor', D1, D2).

		      
%% -- DigitMapName --

is_opt_DigitMapName(asn1_NOVALUE) ->
    true;
is_opt_DigitMapName(N) ->
    is_DigitMapName(N).

is_DigitMapName(N) -> is_Name(N).

chk_opt_DigitMapName(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_DigitMapName(N1, N2) ->
    chk_DigitMapName(N1, N2).

chk_DigitMapName(N, N) ->
    chk_type(fun is_DigitMapName/1, 'DigitMapName', N);
chk_DigitMapName(N1, N2) ->
    case (is_DigitMapName(N1) andalso is_DigitMapName(N2)) of
	true ->
	    not_equal('DigitMapName', N1, N2);
	false ->
	    wrong_type('DigitMapName', N1, N2)
    end.

    
%% -- DigitMapValue --

is_opt_DigitMapValue(V) ->
    is_OPTIONAL(fun is_DigitMapValue/1, V).

is_DigitMapValue(#'DigitMapValue'{startTimer    = Start,
				  shortTimer    = Short,
				  longTimer     = Long, 
				  digitMapBody  = Body, 
				  durationTimer = Dur}) ->
    is_DigitMapValue_startTimer(Start) andalso 
	is_DigitMapValue_shortTimer(Short) andalso 
	is_DigitMapValue_longTimer(Long) andalso 
	is_IA5String(Body) andalso 
	is_DigitMapValue_durationTimer(Dur);
is_DigitMapValue(_) ->
    false.

is_DigitMapValue_startTimer(asn1_NOVALUE)    -> true;
is_DigitMapValue_startTimer(T)               -> is_INTEGER(T, {range, 0, 99}).
					     
is_DigitMapValue_shortTimer(asn1_NOVALUE)    -> true;
is_DigitMapValue_shortTimer(T)               -> is_INTEGER(T, {range, 0, 99}).

is_DigitMapValue_longTimer(asn1_NOVALUE)     -> true;
is_DigitMapValue_longTimer(T)                -> is_INTEGER(T, {range, 0, 99}).

is_DigitMapValue_durationTimer(asn1_NOVALUE) -> true;
is_DigitMapValue_durationTimer(T)            -> is_INTEGER(T, {range, 0, 99}).

chk_opt_DigitMapValue(V1, V2) ->
    chk_OPTIONAL('DigitMapValue', V1, V2,
		 fun is_DigitMapValue/1, fun chk_DigitMapValue/2).

chk_DigitMapValue(#'DigitMapValue'{startTimer    = Start1,
				   shortTimer    = Short1,
				   longTimer     = Long1, 
				   digitMapBody  = Body1, 
				   durationTimer = Dur1},
		  #'DigitMapValue'{startTimer    = Start2,
				   shortTimer    = Short2,
				   longTimer     = Long2, 
				   digitMapBody  = Body2, 
				   durationTimer = Dur2}) ->
    d("chk_DigitMapValue -> entry with"
      "~n   Start1: ~p"
      "~n   Start2: ~p"
      "~n   Short1: ~p"
      "~n   Short2: ~p"
      "~n   Long1:  ~p"
      "~n   Long2:  ~p"
      "~n   Body1:  ~p"
      "~n   Body2:  ~p"
      "~n   Dur1:   ~p"
      "~n   Dur2:   ~p", [Start1, Start2, 
			  Short1, Short2, 
			  Long1, Long2, 
			  Body1, Body2, 
			  Dur1, Dur2]),
    chk_DigitMapValue_startTimer(Start1, Start2),
    chk_DigitMapValue_shortTimer(Short1, Short2),
    chk_DigitMapValue_longTimer(Long1, Long2),
    chk_DigitMapValue_digitMapBody(Body1, Body2), 
    chk_DigitMapValue_durationTimer(Dur1, Dur2),
    ok;
chk_DigitMapValue(V1, V2) ->
    wrong_type('DigitMapValue', V1, V2).
    
chk_DigitMapValue_startTimer(T, T) ->
    chk_type(fun is_DigitMapValue_startTimer/1, 'DigitMapValue_startTimer', T);
chk_DigitMapValue_startTimer(T1, T2) ->
    case (is_DigitMapValue_startTimer(T1) andalso
	  is_DigitMapValue_startTimer(T2)) of
	true ->
	    not_equal('DigitMapValue_startTimer', T1, T2);
	false ->
	    wrong_type('DigitMapValue_startTimer', T1, T2)
    end.
    
chk_DigitMapValue_shortTimer(T, T) ->
    chk_type(fun is_DigitMapValue_shortTimer/1, 'DigitMapValue_shortTimer', T);
chk_DigitMapValue_shortTimer(T1, T2) ->
    case (is_DigitMapValue_shortTimer(T1) andalso
	  is_DigitMapValue_shortTimer(T2)) of
	true ->
	    not_equal('DigitMapValue_shortTimer', T1, T2);
	false ->
	    wrong_type('DigitMapValue_shortTimer', T1, T2)
    end.
    
chk_DigitMapValue_longTimer(T, T) ->
    chk_type(fun is_DigitMapValue_longTimer/1, 'DigitMapValue_longTimer', T);
chk_DigitMapValue_longTimer(T1, T2) ->
    case (is_DigitMapValue_longTimer(T1) andalso
	  is_DigitMapValue_longTimer(T2)) of
	true ->
	    not_equal('DigitMapValue_longTimer', T1, T2);
	false ->
	    wrong_type('DigitMapValue_longTimer', T1, T2)
    end.
    
chk_DigitMapValue_durationTimer(T, T) ->
    chk_type(fun is_DigitMapValue_durationTimer/1, 
	     'DigitMapValue_durationTimer', T);
chk_DigitMapValue_durationTimer(T1, T2) ->
    case (is_DigitMapValue_durationTimer(T1) andalso
	  is_DigitMapValue_durationTimer(T2)) of
	true ->
	    not_equal('DigitMapValue_durationTimer', T1, T2);
	false ->
	    wrong_type('DigitMapValue_durationTimer', T1, T2)
    end.
    
chk_DigitMapValue_digitMapBody(B, B) ->
    d("chk_DigitMapValue_digitMapBody -> entry with"
      "~n   B: ~p", [B]),
    chk_type(fun is_IA5String/1, 'DigitMapValue_digitMapBody', B);
chk_DigitMapValue_digitMapBody(B1, B2) ->
    d("chk_DigitMapValue_digitMapBody -> entry with"
      "~n   B1: ~p"
      "~n   B2: ~p", [B1, B2]),
    case (is_IA5String(B1) andalso is_IA5String(B2)) of
	true ->
	    %% If they are different it could be because
	    %% of trailing tab's and newline's.
	    case compare_strings(B1, B2) of
		{[], []} ->
		    ok;
		{Str1, []} ->
		    case strip_tab_and_newline(Str1) of
			[] ->
			    ok;
			_ ->
			    not_equal('DigitMapValue_digitMapBody', B1, B2)
		    end;
		{[], Str2} ->
		    case strip_tab_and_newline(Str2) of
			[] ->
			    ok;
			_ ->
			    not_equal('DigitMapValue_digitMapBody', B1, B2)
		    end;
		_ ->
		    not_equal('DigitMapValue_digitMapBody', B1, B2)
	    end;
	false ->
	    wrong_type('DigitMapValue_digitMapBody', B1, B2)
    end.
			    
%% -- ServiceChangeParm --

is_ServiceChangeParm(#'ServiceChangeParm'{serviceChangeMethod  = M, 
					  serviceChangeAddress = A,
					  serviceChangeVersion = V, 
					  serviceChangeProfile = P, 
					  serviceChangeReason  = R,
					  serviceChangeDelay   = D, 
					  serviceChangeMgcId   = Id, 
					  timeStamp            = TS, 
					  nonStandardData      = NSD, 
					  serviceChangeInfo    = I}) ->
    is_ServiceChangeMethod(M) andalso 
	is_opt_ServiceChangeAddress(A) andalso 
	is_opt_INTEGER(V, {range, 0, 99}) andalso 
	is_opt_ServiceChangeProfile(P) andalso 
	is_Value(R) andalso 
	is_opt_INTEGER(D, {range, 0, 4294967295}) andalso 
	is_opt_MId(Id) andalso 
	is_opt_TimeNotation(TS) andalso 
	is_opt_NonStandardData(NSD) andalso 
	is_opt_AuditDescriptor(I);
is_ServiceChangeParm(_) ->
    false.

chk_ServiceChangeParm(P, P) ->
    chk_type(fun is_ServiceChangeParm/1, 'ServiceChangeParm', P);
chk_ServiceChangeParm(#'ServiceChangeParm'{serviceChangeMethod  = M1, 
					   serviceChangeAddress = A1,
					   serviceChangeVersion = V1, 
					   serviceChangeProfile = P1, 
					   serviceChangeReason  = R1,
					   serviceChangeDelay   = D1, 
					   serviceChangeMgcId   = Id1, 
					   timeStamp            = TS1, 
					   nonStandardData      = NSD1, 
					   serviceChangeInfo    = I1},
		      #'ServiceChangeParm'{serviceChangeMethod  = M2, 
					   serviceChangeAddress = A2,
					   serviceChangeVersion = V2, 
					   serviceChangeProfile = P2, 
					   serviceChangeReason  = R2,
					   serviceChangeDelay   = D2, 
					   serviceChangeMgcId   = Id2, 
					   timeStamp            = TS2, 
					   nonStandardData      = NSD2, 
					   serviceChangeInfo    = I2}) ->
    validate(fun() -> chk_ServiceChangeMethod(M1, M2) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_ServiceChangeAddress(A1, A2) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_INTEGER(V1, V2, {range, 0, 99}) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_ServiceChangeProfile(P1, P2) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_Value(R1, R2) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_INTEGER(D1, D2, {range, 0, 4294967295}) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_MId(Id1, Id2) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_TimeNotation(TS1, TS2) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_NonStandardData(NSD1, NSD2) end, 
	     'ServiceChangeParm'),
    validate(fun() -> chk_opt_AuditDescriptor(I1, I2) end, 
	     'ServiceChangeParm'),
    ok;
chk_ServiceChangeParm(P1, P2) ->
    wrong_type('ServiceChangeParm', P1, P2).
		     
    
%% -- ServiceChangeAddress --

is_opt_ServiceChangeAddress(A) ->
    is_OPTIONAL(fun is_ServiceChangeAddress/1, A).

is_ServiceChangeAddress({Tag, Val}) ->
    is_ServiceChangeAddress_tag(Tag) andalso 
	is_ServiceChangeAddress_val(Tag, Val);
is_ServiceChangeAddress(_) ->
    false.

is_ServiceChangeAddress_tag(Tag) ->
    Tags = [portNumber, ip4Address, ip6Address, domainName, deviceName, 
	    mtpAddress],
    lists:member(Tag, Tags).

is_ServiceChangeAddress_val(portNumber, Val) ->
    is_INTEGER(Val, {range, 0, 65535});
is_ServiceChangeAddress_val(ip4Address, Val) ->
    is_IP4Address(Val);
is_ServiceChangeAddress_val(ip6Address, Val) ->
    is_IP6Address(Val);
is_ServiceChangeAddress_val(domainName, Val) ->
    is_DomainName(Val);
is_ServiceChangeAddress_val(deviceName, Val) ->
    is_PathName(Val);
is_ServiceChangeAddress_val(mtpAddress, Val) ->
    is_OCTET_STRING(Val, {range, 2, 4}).


chk_opt_ServiceChangeAddress(A1, A2) ->
    chk_OPTIONAL('ServiceChangeAddress', A1, A2, 
		 fun is_ServiceChangeAddress/1,
		 fun chk_ServiceChangeAddress/2).

chk_ServiceChangeAddress(A, A) ->
    chk_type(fun is_ServiceChangeAddress/1, 'ServiceChangeAddress', A);
chk_ServiceChangeAddress({Tag, Val1} = A1, {Tag, Val2} = A2) ->
    case (is_ServiceChangeAddress_tag(Tag) andalso 
	  is_ServiceChangeAddress_val(Tag, Val1) andalso 
	  is_ServiceChangeAddress_val(Tag, Val2)) of
	true ->
	    chk_ServiceChangeAddress_val(Tag, Val1, Val2);
	false ->
	    wrong_type('ServiceChangeAddress', A1, A2)
    end;
chk_ServiceChangeAddress({Tag1, Val1} = A1, {Tag2, Val2} = A2) ->
    case ((is_ServiceChangeAddress_tag(Tag1) andalso 
	   is_ServiceChangeAddress_val(Tag1, Val1)) andalso 
	  (is_ServiceChangeAddress_tag(Tag2) andalso 
	   is_ServiceChangeAddress_val(Tag2, Val2))) of
	true ->
	    not_equal('ServiceChangeAddress', A1, A2);
	false ->
	    wrong_type('ServiceChangeAddress', A1, A2)
    end;
chk_ServiceChangeAddress(A1, A2) ->
    wrong_type('ServiceChangeAddress', A1, A2).

chk_ServiceChangeAddress_val(portNumber, Val1, Val2) ->
    validate(fun() -> chk_INTEGER(Val1, Val2, {range, 0, 99}) end,
	     'ServiceChangeAddress');
chk_ServiceChangeAddress_val(ip4Address, Val1, Val2) ->
    validate(fun() -> chk_IP4Address(Val1, Val2) end,
	     'ServiceChangeAddress');
chk_ServiceChangeAddress_val(ip6Address, Val1, Val2) ->
    validate(fun() -> chk_IP6Address(Val1, Val2) end,
	     'ServiceChangeAddress');
chk_ServiceChangeAddress_val(domainName, Val1, Val2) ->
    validate(fun() -> chk_DomainName(Val1, Val2) end,
	     'ServiceChangeAddress');
chk_ServiceChangeAddress_val(deviceName, Val1, Val2) ->
    validate(fun() -> chk_PathName(Val1, Val2) end,
	     'ServiceChangeAddress');
chk_ServiceChangeAddress_val(mtpAddress, Val1, Val2) ->
    validate(fun() -> chk_OCTET_STRING(Val1, Val2, {range, 2, 4}) end,
	     'ServiceChangeAddress').

    
%% -- ServiceChangeResParm --

is_ServiceChangeResParm(#'ServiceChangeResParm'{serviceChangeMgcId   = Id, 
						serviceChangeAddress = A, 
						serviceChangeVersion = V, 
						serviceChangeProfile = P, 
						timeStamp            = TS}) ->
    is_opt_MId(Id) andalso 
	is_opt_ServiceChangeAddress(A) andalso 
	is_opt_INTEGER(V, {range, 0, 99}) andalso 
	is_opt_ServiceChangeProfile(P) andalso 
	is_opt_TimeNotation(TS);
is_ServiceChangeResParm(_) ->
    false.

chk_ServiceChangeResParm(P, P) ->
    chk_type(fun is_ServiceChangeResParm/1, 'ServiceChangeResParm', P);
chk_ServiceChangeResParm(
  #'ServiceChangeResParm'{serviceChangeMgcId   = Id1, 
			  serviceChangeAddress = A1, 
			  serviceChangeVersion = V1, 
			  serviceChangeProfile = P1, 
			  timeStamp            = TS1},
  #'ServiceChangeResParm'{serviceChangeMgcId   = Id2, 
			  serviceChangeAddress = A2, 
			  serviceChangeVersion = V2, 
			  serviceChangeProfile = P2, 
			  timeStamp            = TS2}) ->
    validate(fun() -> chk_opt_MId(Id1, Id2) end, 'ServiceChangeResParm'),
    validate(fun() -> chk_opt_ServiceChangeAddress(A1, A2) end, 
	     'ServiceChangeResParm'),
    validate(fun() -> chk_opt_INTEGER(V1, V2, {range, 0, 99}) end, 
	     'ServiceChangeResParm'),
    validate(fun() -> chk_opt_ServiceChangeProfile(P1, P2) end, 
	     'ServiceChangeResParm'),
    validate(fun() -> chk_opt_TimeNotation(TS1, TS2) end, 
	     'ServiceChangeResParm'),
    ok;
chk_ServiceChangeResParm(P1, P2) ->
    wrong_type('ServiceChangeResParm', P1, P2).
    
		      
%% -- ServiceChangeMethod --

is_ServiceChangeMethod(M) ->
    Methods = [failover, forced, graceful, restart, disconnected, handOff],
    lists:member(M, Methods).

chk_ServiceChangeMethod(M, M) ->
    chk_type(fun is_ServiceChangeMethod/1, 'ServiceChangeMethod', M);
chk_ServiceChangeMethod(M1, M2) ->
    case (is_ServiceChangeMethod(M1) andalso is_ServiceChangeMethod(M2)) of
	true ->
	    not_equal('ServiceChangeMethod', M1, M2);
	false ->
	    wrong_type('ServiceChangeMethod', M1, M2)
    end.


%% -- ServiceChangeProfile --

is_opt_ServiceChangeProfile(P) ->
    is_OPTIONAL(fun is_ServiceChangeProfile/1, P).

is_ServiceChangeProfile(#'ServiceChangeProfile'{profileName = N}) ->
    is_IA5String(N, {range, 1, 67});
is_ServiceChangeProfile(_) ->
    false.

chk_opt_ServiceChangeProfile(P1, P2) ->
    chk_OPTIONAL('ServiceChangeProfile', P1, P2,
		 fun is_ServiceChangeProfile/1,
		 fun chk_ServiceChangeProfile/2).

chk_ServiceChangeProfile(P, P) ->
    chk_type(fun is_ServiceChangeProfile/1, 'ServiceChangeProfile', P);
chk_ServiceChangeProfile(#'ServiceChangeProfile'{profileName = N1},
			 #'ServiceChangeProfile'{profileName = N2}) ->
    validate(fun() -> chk_IA5String(N1, N2, {range, 1, 67}) end, 
	     'ServiceChangeProfile'),
    ok;
chk_ServiceChangeProfile(P1, P2) ->
    wrong_type('ServiceChangeProfile', P1, P2).


%% -- PackagesDescriptor --

is_PackagesDescriptor([]) ->
    true;
is_PackagesDescriptor([H|T]) ->
    is_PackagesItem(H) andalso is_PackagesDescriptor(T);
is_PackagesDescriptor(_) ->
    false.

chk_PackagesDescriptor([], []) ->
    ok;
chk_PackagesDescriptor([] = D1, D2) ->
    not_equal('PackagesDescriptor', D1, D2);
chk_PackagesDescriptor(D1, [] = D2) ->
    not_equal('PackagesDescriptor', D1, D2);
chk_PackagesDescriptor([H|T1], [H|T2]) ->
    case is_PackagesItem(H) of
	true ->
	    chk_PackagesDescriptor(T1, T2);
	false ->
	    wrong_type('PackagesDescriptor_val', H)
    end;
chk_PackagesDescriptor([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_PackagesItem(H1, H2) end, 
	     'PackagesDescriptor_val'),
    chk_PackagesDescriptor(T1, T2);
chk_PackagesDescriptor(D1, D2) ->
    wrong_type('PackagesDescriptor_val', D1, D2).


%% -- PackagesItem --

is_PackagesItem(#'PackagesItem'{packageName    = N, 
				packageVersion = V}) ->
    is_Name(N) andalso is_INTEGER(V, {range, 0, 99});
is_PackagesItem(_) ->
    false.

chk_PackagesItem(I, I) ->
    chk_type(fun is_PackagesItem/1, 'PackagesItem', I);
chk_PackagesItem(#'PackagesItem'{packageName    = N1, 
				 packageVersion = V1},
		 #'PackagesItem'{packageName    = N2, 
				 packageVersion = V2}) ->
    validate(fun() -> chk_Name(N1, N2) end, 'PackagesItem'),
    validate(fun() -> chk_INTEGER(V1, V2, {range, 0, 99}) end, 'PackagesItem'),
    ok;
chk_PackagesItem(I1, I2) ->
    wrong_type('PackagesItem', I1, I2).

		     
%% -- StatisticsDescriptor --

is_opt_StatisticsDescriptor(D) ->
    is_OPTIONAL(fun is_StatisticsDescriptor/1, D).

is_StatisticsDescriptor([]) ->
    true;
is_StatisticsDescriptor([H|T]) ->
    is_StatisticsParameter(H) andalso is_StatisticsDescriptor(T);
is_StatisticsDescriptor(_) ->
    false.

chk_opt_StatisticsDescriptor(D1, D2) ->
    chk_OPTIONAL('StatisticsDescriptor', D1, D2, 
		 fun is_StatisticsDescriptor/1,
		 fun chk_StatisticsDescriptor/2).

chk_StatisticsDescriptor([], []) ->
    ok;
chk_StatisticsDescriptor([] = D1, D2) ->
    not_equal('StatisticsDescriptor', D1, D2);
chk_StatisticsDescriptor(D1, [] = D2) ->
    not_equal('StatisticsDescriptor', D1, D2);
chk_StatisticsDescriptor([H|T1], [H|T2]) ->
    case is_StatisticsParameter(H) of
	true ->
	    chk_StatisticsDescriptor(T1, T2);
	false ->
	    wrong_type('StatisticsDescriptor_val', H)
    end;
chk_StatisticsDescriptor([H1|T1], [H2|T2]) ->
    validate(fun() -> chk_StatisticsParameter(H1, H2) end, 
	     'StatisticsDescriptor_val'),
    chk_StatisticsDescriptor(T1, T2);
chk_StatisticsDescriptor(D1, D2) ->
    wrong_type('StatisticsDescriptor_val', D1, D2).

   
%% -- StatisticsParameter --

is_StatisticsParameter(#'StatisticsParameter'{statName  = N, 
					      statValue = V}) ->
    is_PkgdName(N) andalso is_opt_Value(V);
is_StatisticsParameter(_) ->
    false.

chk_StatisticsParameter(P, P) ->
    chk_type(fun is_StatisticsParameter/1, 'StatisticsParameter', P);
chk_StatisticsParameter(#'StatisticsParameter'{statName  = N1, 
					       statValue = V1},
			#'StatisticsParameter'{statName  = N2, 
					       statValue = V2}) ->
    validate(fun() -> chk_PkgdName(N1, N2) end, 'StatisticsParameter'),
    validate(fun() -> chk_opt_Value(V1, V2) end, 'StatisticsParameter'),
    ok;
chk_StatisticsParameter(P1, P2) ->
    wrong_type('StatisticsParameter', P1, P2).

		     
%% -- NonStandardData --

is_opt_NonStandardData(asn1_NOVALUE) ->
    true;
is_opt_NonStandardData(NSD) ->
    is_NonStandardData(NSD).

%% is_NonStandardData(#'NonStandardData'{nonStandardIdentifier = Id,
%% 				      data                  = D}) ->
%%     is_NonStandardIdentifier(Id) andalso is_OCTET_STRING(D);
%% is_NonStandardData(_) ->
%%     false.

is_NonStandardData(_) ->
    true.

chk_opt_NonStandardData(asn1_NOVALUE, asn1_NOVALUE) ->
    true;
chk_opt_NonStandardData(NSD1, NSD2) ->
    chk_NonStandardData(NSD1, NSD2).

chk_NonStandardData(NSD, NSD) ->
    chk_type(fun is_NonStandardData/1, 'NonStandardData', NSD);
%% chk_NonStandardData(#'NonStandardData'{nonStandardIdentifier = Id1,
%% 				       data                  = D1},
%% 		    #'NonStandardData'{nonStandardIdentifier = Id2,
%% 				       data                  = D2}) ->
%%     validate(fun() -> chk_NonStandardIdentifier(Id1, Id2) end,
%% 	     'NonStandardData'),
%%     validate(fun() -> chk_OCTET_STRING(D1, D2) end, 'NonStandardData'),
%%     ok;
%% chk_NonStandardData(NSD1, NSD2) ->
%%     wrong_type('NonStandardData', NSD1, NSD2).
chk_NonStandardData(NSD1, NSD2) ->
    not_equal('NonStandardData', NSD1, NSD2).

		     
%% -- NonStandardIdentifier --

%% is_NonStandardIdentifier({Tag, Val}) ->
%%     is_NonStandardIdentifier_tag(Tag) andalso
%% 	is_NonStandardIdentifier_val(Tag, Val);
%% is_NonStandardIdentifier(_) ->
%%     false.

%% is_NonStandardIdentifier_tag(Tag) ->
%%     Tags = [object, h221NonStandard, experimental],
%%     lists:member(Tag, Tags).

%% is_NonStandardIdentifier_val(object, Val) ->
%%     is_OBJECT_IDENTIFIER(Val);
%% is_NonStandardIdentifier_val(h221NonStandard, Val) ->
%%     is_H221NonStandard(Val);
%% is_NonStandardIdentifier_val(experimental, Val) ->
%%     is_IA5String(Val, {exact, 8}).

%% chk_NonStandardIdentifier(Id, Id) ->
%%     chk_type(fun is_NonStandardIdentifier/1, 'NonStandardIdentifier', Id);
%% chk_NonStandardIdentifier({Tag, Val1} = Id1, {Tag, Val2} = Id2) ->
%%     case (is_NonStandardIdentifier_tag(Tag) andalso
%% 	  is_NonStandardIdentifier_val(Tag, Val1) andalso
%% 	  is_NonStandardIdentifier_val(Tag, Val1)) of
%% 	true ->
%% 	    chk_NonStandardIdentifier_val(Tag, Val1, Val2);
%% 	false ->
%% 	    wrong_type('NonStandardIdentifier', Id1, Id2)
%%     end;
%% chk_NonStandardIdentifier({Tag1, Val1} = Id1, {Tag2, Val2} = Id2) ->
%%     case ((is_NonStandardIdentifier_tag(Tag1) andalso
%% 	   is_NonStandardIdentifier_val(Tag1, Val1)) andalso
%% 	  (is_NonStandardIdentifier_tag(Tag2) andalso
%% 	   is_NonStandardIdentifier_val(Tag2, Val1))) of
%% 	true ->
%% 	    not_equal('NonStandardIdentifier', Id1, Id2);
%% 	false ->
%% 	    wrong_type('NonStandardIdentifier', Id1, Id2)
%%     end;
%% chk_NonStandardIdentifier(Id1, Id2) ->
%%     wrong_type('NonStandardIdentifier', Id1, Id2).

%% chk_NonStandardIdentifier_val(object, Val1, Val2) ->
%%     chk_OBJECT_IDENTIFIER(Val1, Val2);
%% chk_NonStandardIdentifier_val(h221NonStandard, Val1, Val2) ->
%%     chk_H221NonStandard(Val1, Val2);
%% chk_NonStandardIdentifier_val(experimental, Val1, Val2) ->
%%     chk_IA5String(Val1, Val2, {exact, 8}).


%% -- H221NonStandard --

%% is_H221NonStandard(#'H221NonStandard'{t35CountryCode1  = CC1, 
%% 				      t35CountryCode2  = CC2, 
%% 				      t35Extension     = Ext, 
%% 				      manufacturerCode = MC}) ->
%%     is_INTEGER(CC1, {range, 0, 255}) andalso 
%% 	is_INTEGER(CC2, {range, 0, 255}) andalso 
%% 	is_INTEGER(Ext, {range, 0, 255}) andalso 
%% 	is_INTEGER(Ext, {range, 0, 65535});
%% is_H221NonStandard(_) ->
%%     false.

%% chk_H221NonStandard(NS, NS) ->
%%     chk_type(fun is_H221NonStandard/1, 'H221NonStandard', NS);
%% chk_H221NonStandard(#'H221NonStandard'{t35CountryCode1  = CC11, 
%% 				       t35CountryCode2  = CC21, 
%% 				       t35Extension     = Ext1, 
%% 				       manufacturerCode = MC1},
%% 		    #'H221NonStandard'{t35CountryCode1  = CC12, 
%% 				       t35CountryCode2  = CC22, 
%% 				       t35Extension     = Ext2, 
%% 				       manufacturerCode = MC2}) ->
%%     validate(fun() -> chk_INTEGER(CC11, CC12, {range, 0, 255}) end,
%% 	     'H221NonStandard'),
%%     validate(fun() -> chk_INTEGER(CC21, CC22, {range, 0, 255}) end,
%% 	     'H221NonStandard'),
%%     validate(fun() -> chk_INTEGER(Ext1, Ext2, {range, 0, 255}) end,
%% 	     'H221NonStandard'),
%%     validate(fun() -> chk_INTEGER(MC1, MC2, {range, 0, 65535}) end,
%% 	     'H221NonStandard'),
%%     ok;
%% chk_H221NonStandard(NS1, NS2) ->
%%     wrong_type('H221NonStandard', NS1, NS2).


%% -- TimeNotation --

is_opt_TimeNotation(asn1_NOVALUE) ->
    true;
is_opt_TimeNotation(TN) ->
    is_TimeNotation(TN).

is_TimeNotation(#'TimeNotation'{date = D, time = T}) ->
    is_IA5String(D, {exact, 8}) andalso is_IA5String(T, {exact, 8});
is_TimeNotation(_) ->
    false.

chk_opt_TimeNotation(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_TimeNotation(TN1, TN2) ->
    chk_TimeNotation(TN1, TN2).

chk_TimeNotation(TN, TN) ->
    chk_type(fun is_TimeNotation/1, 'TimeNotation', TN);
chk_TimeNotation(#'TimeNotation'{date = D1, time = T1},
		 #'TimeNotation'{date = D2, time = T2}) ->
    validate(fun() -> chk_IA5String(D1, D2, {exact, 8}) end, 'TimeNotation'),
    validate(fun() -> chk_IA5String(T1, T2, {exact, 8}) end, 'TimeNotation'),
    ok;
chk_TimeNotation(TN1, TN2) ->
    wrong_type('TimeNotation', TN1, TN2).
		     

%% -- Value -- 

is_opt_Value(V) ->
    is_OPTIONAL(fun is_Value/1, V).

is_Value([]) -> 
    true;
is_Value([H|T]) ->
    is_OCTET_STRING(H) andalso is_Value(T);
is_Value(_) ->
    false.

chk_opt_Value(V1, V2) ->
    chk_OPTIONAL('Value', V1, V2, fun is_Value/1, fun chk_Value/2).

chk_Value(V, V) ->
    case is_Value(V) of
	true ->
	    ok;
	false ->
	    wrong_type('Value', V, V)
    end;
chk_Value(V1, V2) ->
    case (is_Value(V1) andalso is_Value(V2)) of
	true ->
	    not_equal('Value', V1, V2);
	false ->
	    wrong_type('Value', V1, V2)
    end.
    

%% ----------------------------------------------------------------------
%% Basic type check functions
%% ----------------------------------------------------------------------


is_opt_BOOLEAN(B) ->
    is_OPTIONAL(fun is_BOOLEAN/1, B).

is_BOOLEAN(B) ->
    d("is_BOOLEAN -> entry with"
      "~n   B: ~p", [B]),
    lists:member(B, [true, false]).

chk_opt_BOOLEAN(B1, B2) ->
    chk_OPTIONAL('BOOLEAN', B1, B2, fun is_BOOLEAN/1, fun chk_BOOLEAN/2).

chk_BOOLEAN(B, B) ->
    chk_type(fun is_BOOLEAN/1, 'BOOLEAN', B);
chk_BOOLEAN(B1, B2) ->
    case (is_BOOLEAN(B1) andalso is_BOOLEAN(B2)) of
	true ->
	    not_equal('BOOLEAN', B1, B2);
	false ->
	    wrong_type('BOOLEAN', B1, B2)
    end.


is_IA5String(S) when is_list(S) ->
    true;
is_IA5String(_) ->
    false.

% chk_IA5String(S, S) ->
%     chk_type(fun is_IA5String/1, 'IA5String', S);
% chk_IA5String(S1, S2) ->
%     case (is_IA5String(S1) andalso is_IA5String(S2)) of
% 	true ->
% 	    not_equal('IA5String', S1, S2);
% 	false ->
% 	    wrong_type('IA5String', S1, S2)
%     end.

is_IA5String(S, _) when is_list(S) ->
    true;
is_IA5String(_, _) ->
    false.

chk_IA5String(S, S, R) ->
    chk_type(fun is_IA5String/2, 'IA5String', S, R);
chk_IA5String(S1, S2, R) ->
    case (is_IA5String(S1, R) andalso is_IA5String(S2, R)) of
	true ->
	    not_equal('IA5String', S1, S2);
	false ->
	    wrong_type('IA5String', S1, S2)
    end.


is_OCTET_STRING(L) -> is_OCTET_STRING(L, any).

is_OCTET_STRING(L, any) when is_list(L) ->
    true;
is_OCTET_STRING(L, {exact, Len}) when is_list(L) and (length(L) == Len) ->
    true;
is_OCTET_STRING(L, {atleast, Len}) when is_list(L) and (Len =< length(L)) ->
    true;
is_OCTET_STRING(L, {atmost, Len}) when is_list(L) and (length(L) =< Len) ->
    true;
is_OCTET_STRING(L, {range, Min, Max}) 
  when is_list(L) and (Min =< length(L)) and (length(L) =< Max) ->
    true;
is_OCTET_STRING(_, _) ->
    false.

%% chk_OCTET_STRING(L1, L2) ->
%%     chk_OCTET_STRING(L1, L2, any).

chk_OCTET_STRING(L, L, R) ->
    chk_type(fun is_OCTET_STRING/2, 'OCTET STRING', L, R);
chk_OCTET_STRING(L1, L2, R) ->
    case (is_OCTET_STRING(L1, R) andalso is_OCTET_STRING(L2, R)) of
	true ->
	    not_equal('OCTET STRING', L1, L2);
	false ->
	    wrong_type('OCTET STRING', L1, L2)
    end.


%% is_OBJECT_IDENTIFIER(_) ->
%%     true.

%% chk_OBJECT_IDENTIFIER(X, X) ->
%%     ok;
%% chk_OBJECT_IDENTIFIER(X1, X2) ->
%%     not_equal('OBJECT IDENTIFIER', X1, X2).


is_opt_NULL(N) ->
    is_OPTIONAL(fun is_NULL/1, N).

is_NULL('NULL') ->
    true;
is_NULL(_) ->
    false.

chk_opt_NULL(N1, N2) ->
    chk_OPTIONAL('NULL', N1, N2, fun is_NULL/1, fun chk_NULL/2).

chk_NULL(N, N) ->
    chk_type(fun is_NULL/1, 'NULL', N);
chk_NULL(N1, N2) ->
    case (is_NULL(N1) andalso is_NULL(N2)) of
	true ->
	    not_equal('NULL', N1, N2);
	false ->
	    wrong_type('NULL', N1, N2)
    end.


is_opt_INTEGER(I, R) ->
    d("is_opt_INTEGER -> entry with"
      "~n   I: ~p"
      "~n   R: ~p", [I, R]),
    is_OPTIONAL(fun(X) -> is_INTEGER(X, R) end, I).

is_INTEGER(I, any) when is_integer(I) ->
    true;
is_INTEGER(I, {exact, I}) when is_integer(I) ->
    true;
is_INTEGER(I, {atleast, Min}) when 
  is_integer(I) and is_integer(Min) and (Min =< I) ->
    true;
is_INTEGER(I, {atmost, Max}) 
  when is_integer(I) and is_integer(Max) and (I =< Max) ->
    true;
is_INTEGER(I, {range, Min, Max}) 
  when is_integer(I) and 
       is_integer(Min) and (Min =< I) and 
       is_integer(Max) and (I =< Max) ->
    true;
is_INTEGER(_, _) ->
    false.

chk_opt_INTEGER(I1, I2, R) ->
    chk_OPTIONAL('INTEGER', I1, I2, 
		 fun(X) -> is_INTEGER(X, R) end,
		 fun(Y1, Y2) -> chk_INTEGER(Y1, Y2, R) end).

chk_INTEGER(I, I, R) ->
    chk_type(fun is_INTEGER/2, 'INTEGER', I, R);
chk_INTEGER(I1, I2, R) ->
    case (is_INTEGER(I1, R) andalso is_INTEGER(I2, R)) of
	true ->
	    not_equal('INTEGER', I1, I2);
	false ->
	    wrong_type('INTEGER', I1, I2)
    end.


%% ----------------------------------------------------------------------
%% Various utility functions
%% ----------------------------------------------------------------------


to_lower([C|Cs]) when (C >= $A) and (C =< $Z) ->
    [C+($a-$A)|to_lower(Cs)];
to_lower([C|Cs]) ->
    [C|to_lower(Cs)];
to_lower([]) ->
    [].


validate(F, Type) when is_function(F) ->
    case (catch F()) of
	{error, Reason} ->
	    error({Type, Reason});
	ok ->
	    ok
    end.

	    
chk_type(F, T, V) when is_function(F) and is_atom(T) ->
    case F(V) of
	true ->
	    ok;
	false ->
	    wrong_type(T, V)
    end.

chk_type(F, T, V1, V2) when is_function(F) and is_atom(T) ->
    case F(V1, V2) of
	true ->
	    ok;
	false ->
	    wrong_type(T, V1)
    end.


is_OPTIONAL(_, asn1_NOVALUE) ->
    true;
is_OPTIONAL(F, Val) when is_function(F) ->
    F(Val).

chk_OPTIONAL(_, asn1_NOVALUE, asn1_NOVALUE, _, _) ->
    ok;
chk_OPTIONAL(Type, asn1_NOVALUE = V1, V2, IS, _CHK) when is_function(IS) ->
    case IS(V2) of
	true ->
	    not_equal(Type, V1, V2);
	false ->
	    wrong_type(Type, V1, V2)
    end;
chk_OPTIONAL(Type, V1, asn1_NOVALUE = V2, IS, _CHK) when is_function(IS) ->
    case IS(V1) of
	true ->
	    not_equal(Type, V1, V2);
	false ->
	    wrong_type(Type, V1, V2)
    end;
chk_OPTIONAL(_Type, V1, V2, _IS, CHK) when is_function(CHK) ->
    CHK(V1, V2).


%% -- CHOICE --

is_CHOICE({Tag, Val}, Tags) when is_list(Tags) ->
    d("is_CHOICE -> entry with"
      "~n   Tag: ~p"
      "~n   Val: ~p", [Tag, Val]),
    is_CHOICE_tag(Tag, Tags) andalso 
	is_CHOICE_val(Tag, Val, Tags);
is_CHOICE(_, _) ->
    false.
    
is_CHOICE_tag(Tag, Tags) ->
    lists:keymember(Tag, 1, Tags).

is_CHOICE_val(Tag, Val, Tags) ->
    {value, {_, IsVal, _}} = lists:keysearch(Tag, 1, Tags),
    IsVal(Val).


chk_CHOICE(V1, V2, Type, Tags) when is_atom(Type) and is_list(Tags) ->
    IsTag  = fun(Tag) -> is_CHOICE_tag(Tag, Tags) end,
    IsVal  = fun(Tag, Val) -> is_CHOICE_val(Tag, Val, Tags) end,
    ChkVal = fun(Tag, Val1, Val2) ->
		     chk_CHOICE_val(Tag, Val1, Val2, Tags)
	     end,
    chk_CHOICE(V1, V2, Type, IsTag, IsVal, ChkVal).

chk_CHOICE_val(Tag, Val1, Val2, Tags) ->
    {value, {_, _, ChkVal}} = lists:keysearch(Tag, 1, Tags),
    ChkVal(Val1, Val2).

chk_CHOICE({Tag, Val1} = V1, {Tag, Val2} = V2, Type, IsTag, IsVal, ChkVal) ->
    case (IsTag(Tag) andalso 
	  IsVal(Tag, Val1) andalso IsVal(Tag, Val2)) of
	true ->
	    ChkVal(Tag, Val1, Val2);
	false ->
	    wrong_type(Type, V1, V2)
    end;
chk_CHOICE({Tag1, Val1} = V1, {Tag2, Val2} = V2, Type, 
	   IsTag, IsVal, _ChkVal) -> 
    case ((IsTag(Tag1) andalso IsVal(Tag1, Val1)) andalso
	  (IsTag(Tag2) andalso IsVal(Tag2, Val2))) of
	true ->
	    not_equal(Type, V1, V2);
	false ->
	    wrong_type(Type, V1, V2)
    end;
chk_CHOICE(V1, V2, Type, _, _, _) ->
    wrong_type(Type, V1, V2).


%% -- SEQUENCE OF --

is_SEQUENCE_OF([], _) ->
    true;
is_SEQUENCE_OF([H|T], IS) when is_function(IS) ->
    IS(H) andalso is_SEQUENCE_OF(T, IS);
is_SEQUENCE_OF(_, _) ->
    false.

chk_SEQUENCE_OF([], [], _, _, _) ->
    ok;
chk_SEQUENCE_OF([] = V1, V2, Type, _, _) ->
    not_equal(Type, V1, V2);
chk_SEQUENCE_OF(V1, [] = V2, Type, _, _) ->
    not_equal(Type, V1, V2);
chk_SEQUENCE_OF([H|T1], [H|T2], Type, IS, CHK) ->
    case IS(H) of
	true ->
	    chk_SEQUENCE_OF(T1, T2, Type, IS, CHK);
	false ->
	    Type2 = list_to_atom(atom_to_list(Type) ++ "_val"),
	    wrong_type(Type2, H)
    end;
chk_SEQUENCE_OF([H1|T1], [H2|T2], Type, IS, CHK) ->
    Type2 = list_to_atom(atom_to_list(Type) ++ "_val"),
    validate(fun() -> CHK(H1, H2) end, Type2),
    chk_SEQUENCE_OF(T1, T2, Type, IS, CHK);
chk_SEQUENCE_OF(V1, V2, Type, _, _) ->
    wrong_type(Type, V1, V2).


%% ----------------------------------------------------------------------

compare_strings([] = L1, L2) ->
    {L1, L2};
compare_strings(L1, [] = L2) ->
    {L1, L2};
compare_strings([H|T1], [H|T2]) ->
    compare_strings(T1, T2);
compare_strings(L1, L2) ->
    {L1, L2}.

strip_tab_and_newline([]) ->
    [];
strip_tab_and_newline([$\n|T]) ->
    strip_tab_and_newline(T);
strip_tab_and_newline([$\t|T]) ->
    strip_tab_and_newline(T);
strip_tab_and_newline([H|T]) ->
    [H|strip_tab_and_newline(T)].


%% ----------------------------------------------------------------------

maybe_sort(L) when is_list(L) ->
    lists:sort(L);
maybe_sort(X) ->
    X.


%% ----------------------------------------------------------------------

atmost_once(Type, Val) ->
    error({atmost_once, {Type, Val}}).

wrong_type(Type, Val) ->
    error({wrong_type, {Type, Val}}).

wrong_type(Type, Val1, Val2) ->
    error({wrong_type, {Type, Val1, Val2}}).

not_equal(What, Val1, Val2) ->
    error({not_equal, {What, Val1, Val2}}).

error(Reason) ->
    throw({error, Reason}).


%% ----------------------------------------------------------------------

d(F) ->
    d(F, []).

d(F, A) ->
    d(get(dbg), F, A).

d(true, F, A) ->
    io:format("DBG:" ++ F ++ "~n", A);
d(_, _, _) ->
    ok.

