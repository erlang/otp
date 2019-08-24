%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%%% Purpose: Encode PRETTY Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_pretty_text_encoder_v3).

-export([encode_message/2,
	 encode_transaction/2,
	 encode_action_requests/2,
	 encode_action_request/2,
	 encode_command_request/2,
	 encode_action_reply/2]).

-export([token_tag2string/1]).


-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v3.hrl").
-include("megaco_text_tokens.hrl").


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EC, MegaMsg) 
  when is_list(EC) andalso is_record(MegaMsg, 'MegacoMessage') ->
    case (catch enc_MegacoMessage(MegaMsg)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Bin when is_binary(Bin) ->
	    {ok, Bin};
	DeepIoList ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin}
    end;
encode_message(EncodingConfig, MegaMsg) 
  when is_record(MegaMsg, 'MegacoMessage')  ->
    {error, {bad_encoding_config, EncodingConfig}};
encode_message(_EncodingConfig, _MegaMsg) ->
    {error, bad_megaco_message}.


%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%% 
%% See megaco_pretty_text_encoder:decode_message/2
%% 
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Convert a transaction record into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_transaction(_EC, Trans) ->
    case (catch enc_Transaction(Trans)) of
        {'EXIT', Reason} ->
            {error, Reason};
        Bin when is_binary(Bin) ->
            {ok, Bin};
        DeepIoList ->
            Bin = erlang:list_to_binary(DeepIoList),
            {ok, Bin}
    end.

%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests(_EC, ActReqs) when is_list(ActReqs) -> 
    case (catch enc_ActionRequests(ActReqs)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Bin when is_binary(Bin) ->
	    {ok, Bin};
	DeepIoList ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin}
    end.

%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request(_EC, ActReq) 
  when is_record(ActReq, 'ActionRequest') ->
    case (catch enc_ActionRequest(ActReq)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Bin when is_binary(Bin) ->
	    {ok, Bin};
	DeepIoList ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin}
    end.

%%----------------------------------------------------------------------
%% Convert a CommandRequest record into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_command_request(_EC, CmdReq) 
  when is_record(CmdReq, 'CommandRequest') ->
    case (catch enc_CommandRequest(CmdReq)) of
        {'EXIT', Reason} ->
            {error, Reason};
        Bin when is_binary(Bin) ->
            {ok, Bin};
        DeepIoList ->
            Bin = erlang:list_to_binary(DeepIoList),
            {ok, Bin}
    end.

%%----------------------------------------------------------------------
%% Convert a action reply into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_reply(_EC, ActRep) 
  when is_record(ActRep, 'ActionReply') ->
    case (catch enc_ActionReply(ActRep)) of
        {'EXIT', Reason} ->
            {error, Reason};
	Bin when is_binary(Bin) ->
	    {ok, Bin};
	DeepIoList ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin}
    end.


%%----------------------------------------------------------------------
%% A utility function to pretty print the tags found in a megaco message
%%----------------------------------------------------------------------

token_tag2string(addReq)                 -> ?CompactAddToken;
token_tag2string(addReply)               -> ?CompactAddToken;
%% token_tag2string(X)               -> ?CompactAndAUDITSelectToken;
token_tag2string(auditDescriptor)        -> ?CompactAuditToken;
token_tag2string(auditCapRequest)        -> ?CompactAuditCapToken;
token_tag2string(auditCapReply)          -> ?CompactAuditCapToken;
token_tag2string(auditValueRequest)      -> ?CompactAuditValueToken;
token_tag2string(auditValueReply)        -> ?CompactAuditValueToken;
%% token_tag2string(X) -> ?CompactAuthToken;
token_tag2string(both)                   -> ?CompactBothToken;
token_tag2string(bothway)                -> ?CompactBothwayToken;
token_tag2string(brief)                  -> ?CompactBriefToken;
%% token_tag2string(X) -> ?CompactBufferToken;
%% token_tag2string(X) -> ?CompactCtxToken;
%% token_tag2string(X) -> ?CompactContextAuditToken;
%% token_tag2string(X) -> ?CompactContextAttrToken;
%% token_tag2string(X) -> ?CompactContextListToken;
token_tag2string(digitMapDescriptor)     -> ?CompactDigitMapToken;
token_tag2string(digitMapToken)          -> ?CompactDigitMapToken;
%% token_tag2string(X) -> ?CompactDirectionToken;
%% token_tag2string(X) -> ?CompactDiscardToken;
%% token_tag2string(X) -> ?CompactDisconnectedToken;
%% token_tag2string(X) -> ?CompactDelayToken;
token_tag2string(duration)               -> ?CompactDurationToken;
%% token_tag2string(X) -> ?CompactEmbedToken;
token_tag2string(emergencyAudit)         -> ?CompactEmergencyToken;
%% token_tag2string(X)         -> ?CompactEmergencyOffToken;
%% token_tag2string(X)         -> ?CompactEmergencyValueToken;
token_tag2string(errorDescriptor)        -> ?CompactErrorToken;
token_tag2string(eventBufferDescriptor)  -> ?CompactEventBufferToken;
token_tag2string(eventBufferToken)       -> ?CompactEventBufferToken;
token_tag2string(eventsDescriptor)       -> ?CompactEventsToken;
token_tag2string(eventsToken)            -> ?CompactEventsToken;
token_tag2string(external)               -> ?CompactExternalToken;
%% token_tag2string(X) -> ?CompactFailoverToken;
%% token_tag2string(X) -> ?CompactForcedToken;
%% token_tag2string(X) -> ?CompactGracefulToken;
%% token_tag2string(X) -> ?CompactH221Token;
%% token_tag2string(X) -> ?CompactH223Token;
%% token_tag2string(X) -> ?CompactH226Token;
%% token_tag2string(X) -> ?CompactHandOffToken;
token_tag2string(iepsCallind)                 -> ?CompactIEPSToken;
%% token_tag2string(X) -> ?CompactImmAckRequiredToken;
token_tag2string(inactive)                    -> ?CompactInactiveToken;
token_tag2string(internal)                    -> ?CompactInternalToken;
%% token_tag2string(X)                    -> ?CompactIntsigDelayToken;
token_tag2string(onInterruptByEvent)          -> ?CompactInterruptByEventToken;
token_tag2string(onInterruptByNewSignalDescr) -> ?CompactInterruptByNewSignalsDescrToken;
token_tag2string(isolate)        	 -> ?CompactIsolateToken;
token_tag2string(inSvc)          	 -> ?CompactInSvcToken;
token_tag2string(iteration)        	 -> ?CompactIterationToken;
token_tag2string(keepActive)     	 -> ?CompactKeepActiveToken;
token_tag2string(localDescriptor)        -> ?CompactLocalToken;
token_tag2string(localControlDescriptor) -> ?CompactLocalControlToken;
token_tag2string(lockStep)          	 -> ?CompactLockStepToken;
token_tag2string(loopBack)          	 -> ?CompactLoopbackToken;
token_tag2string(mediaDescriptor)   	 -> ?CompactMediaToken;
token_tag2string(mediaToken)        	 -> ?CompactMediaToken;
%% token_tag2string(X) -> ?CompactMegacopToken;
%% token_tag2string(X) -> ?CompactMethodToken;
%% token_tag2string(X) -> ?CompactMgcIdToken;
%% token_tag2string(X) -> ?CompactModeToken;
token_tag2string(modReq)               -> ?CompactModifyToken;
token_tag2string(modReply)             -> ?CompactModifyToken;
token_tag2string(modemDescriptor)      -> ?CompactModemToken;
token_tag2string(modemToken)           -> ?CompactModemToken;
token_tag2string(moveReq)              -> ?CompactMoveToken;
token_tag2string(moveReply)            -> ?CompactMoveToken;
%% token_tag2string(X) -> ?CompactMtpToken;
token_tag2string(muxDescriptor)        -> ?CompactMuxToken;
token_tag2string(muxToken)             -> ?CompactMuxToken;
%% token_tag2string(X)            -> ?CompactNeverNotifyToken;
token_tag2string(notifyReq)            -> ?CompactNotifyToken;
%% token_tag2string(X) -> ?CompactNotifyCompletionToken;
%% token_tag2string(X) -> ?CompactNotifyImmediateToken;
%% token_tag2string(X) -> ?CompactNotifyRegulatedToken;
%% token_tag2string(X) -> ?CompactNx64kToken;
token_tag2string(observedEventsDescriptor) -> ?CompactObservedEventsToken;
token_tag2string(observedEventsToken)      -> ?CompactObservedEventsToken;
token_tag2string(false)                 -> ?CompactOffToken;
token_tag2string(off)                   -> ?CompactOffToken;
token_tag2string(oneway)                -> ?CompactOnewayToken;
token_tag2string(onewayboth)            -> ?CompactOnewayBothToken;
token_tag2string(onewayexternal)        -> ?CompactOnewayExternalToken;
token_tag2string(onOff)                 -> ?CompactOnOffToken;
%% token_tag2string(X)                  -> ?CompactOrAUDITselectToken;
token_tag2string(true)                  -> ?CompactOnToken;
token_tag2string(otherReason)           -> ?CompactOtherReasonToken;
token_tag2string(outOfSvc)              -> ?CompactOutOfSvcToken;
token_tag2string(packagesDescriptor)    -> ?CompactPackagesToken;
token_tag2string(packagesToken)         -> ?CompactPackagesToken;
%% token_tag2string(X) -> ?CompactPendingToken;
token_tag2string(priorityAudit)         -> ?CompactPriorityToken;
%% token_tag2string(X) -> ?CompactProfileToken;
%% token_tag2string(X) -> ?CompactReasonToken;
token_tag2string(recvOnly)              -> ?CompactRecvonlyToken;
%% token_tag2string(X) -> ?CompactReplyToken;
token_tag2string(resetEventsDescriptor) -> ?CompactResetEventsDescriptorToken;
%% token_tag2string(X) -> ?CompactRequestIDToken;
%% token_tag2string(X) -> ?CompactResponseAckToken;
%% token_tag2string(X) -> ?CompactRestartToken;
token_tag2string(remoteDescriptor)     -> ?CompactRemoteToken;
%% token_tag2string(X) -> ?CompactReservedGroupToken;
%% token_tag2string(X) -> ?CompactReservedValueToken;
token_tag2string(sendOnly)             -> ?CompactSendonlyToken;
token_tag2string(sendRecv)             -> ?CompactSendrecvToken;
%% token_tag2string(X) -> ?CompactServicesToken;
%% token_tag2string(X) -> ?CompactServiceStatesToken;
token_tag2string(serviceChangeReq)     -> ?CompactServiceChangeToken;
%% token_tag2string(X) -> ?CompactServiceChangeAddressToken;
token_tag2string(incomplete)           -> ?CompactServiceChangeIncompleteToken;
%% token_tag2string(X) -> ?CompactSignalListToken;
token_tag2string(signalsDescriptor)    -> ?CompactSignalsToken;
token_tag2string(signalsToken)         -> ?CompactSignalsToken; 
%% token_tag2string(X) -> ?CompactSignalTypeToken;
token_tag2string(statisticsDescriptor) -> ?CompactStatsToken;
token_tag2string(statsToken)           -> ?CompactStatsToken;
%% token_tag2string(X) -> ?CompactStreamToken;
token_tag2string(subtractReq)          -> ?CompactSubtractToken;
token_tag2string(subtractReply)        -> ?CompactSubtractToken;
%% token_tag2string(X) -> ?CompactSynchISDNToken;
%% token_tag2string(X) -> ?CompactTerminationStateToken;
token_tag2string(test)                 -> ?CompactTestToken;
token_tag2string(timeOut)              -> ?CompactTimeOutToken;
token_tag2string(onTimeOut)            -> ?CompactTimeOutToken;
token_tag2string(topologyAudit)        -> ?CompactTopologyToken;
%% token_tag2string(X) -> ?CompactTransToken;
%% token_tag2string(X) -> ?CompactV18Token;
%% token_tag2string(X) -> ?CompactV22Token;
%% token_tag2string(X) -> ?CompactV22bisToken;
%% token_tag2string(X) -> ?CompactV32Token;
%% token_tag2string(X) -> ?CompactV32bisToken;
%% token_tag2string(X) -> ?CompactV34Token;
%% token_tag2string(X) -> ?CompactV76Token;
%% token_tag2string(X) -> ?CompactV90Token;
%% token_tag2string(X) -> ?CompactV91Token;
%% token_tag2string(X) -> ?CompactVersionToken;
token_tag2string(_) -> [].


%%----------------------------------------------------------------------
%% Define various macros used by the actual generator code
%%----------------------------------------------------------------------

-define(EQUAL,  [?SpToken, ?EqualToken, ?SpToken]).
-define(COLON,  [?ColonToken]).
-define(LBRKT,  [?SpToken, ?LbrktToken, ?SpToken]).
-define(RBRKT,  [?SpToken, ?RbrktToken, ?SpToken]).
-define(LSBRKT, [?SpToken, ?LsbrktToken, ?SpToken]).
-define(RSBRKT, [?SpToken, ?RsbrktToken, ?SpToken]).
-define(COMMA,  [?CommaToken, ?SpToken]).
-define(DOT,    [?DotToken]).
-define(SLASH,  [?SlashToken]).
-define(DQUOTE, [?DoubleQuoteToken]).
-define(SP,     [?SpToken]).
-define(HTAB,   [?HtabToken]).
-define(CR,     [?CrToken]).
-define(LF,     [?LfToken]).
-define(LWSP,   []).
-define(EOL,    ?LF).
-define(WSP,    ?SP).
-define(SEP,    ?WSP).

-define(INIT_INDENT,          []).
-define(INC_INDENT(State),    [?HtabToken | State]).
-define(INDENT(State),        [?LfToken | State]).
-define(LBRKT_INDENT(State),  [?SpToken, ?LbrktToken, ?INDENT(?INC_INDENT(State))]).
-define(RBRKT_INDENT(State),  [?INDENT(State), ?RbrktToken]).
-define(LSBRKT_INDENT(State), [?SpToken, ?LsbrktToken, ?INDENT(?INC_INDENT(State))]).
-define(RSBRKT_INDENT(State), [?INDENT(State), ?RsbrktToken]).
-define(COMMA_INDENT(State),  [?CommaToken, ?INDENT(State)]).
-define(SEP_INDENT(_State),   [?LfToken]).

%%----------------------------------------------------------------------
%% Define token macros
%%----------------------------------------------------------------------

-define(AddToken                   , ?PrettyAddToken).
-define(AndAUDITSelectToken        , ?PrettytAndAUDITSelectToken).
-define(AuditToken                 , ?PrettyAuditToken).
-define(AuditCapToken              , ?PrettyAuditCapToken).
-define(AuditValueToken            , ?PrettyAuditValueToken).
-define(AuthToken                  , ?PrettyAuthToken).
-define(BothToken                  , ?PrettyBothToken).
-define(BothwayToken               , ?PrettyBothwayToken).
-define(BriefToken                 , ?PrettyBriefToken).
-define(BufferToken                , ?PrettyBufferToken).
-define(CtxToken                   , ?PrettyCtxToken).
-define(ContextAuditToken          , ?PrettyContextAuditToken).
-define(ContextAttrToken           , ?PrettyContextAttrToken).
-define(ContextListToken           , ?PrettyContextListToken).
-define(DigitMapToken              , ?PrettyDigitMapToken).
-define(DirectionToken             , ?PrettyDirectionToken).
-define(DiscardToken               , ?PrettyDiscardToken).
-define(DisconnectedToken          , ?PrettyDisconnectedToken).
-define(DelayToken                 , ?PrettyDelayToken).
-define(DeleteToken                , ?PrettyDeleteToken).
-define(DurationToken              , ?PrettyDurationToken).
-define(EmbedToken                 , ?PrettyEmbedToken).
-define(EmergencyToken             , ?PrettyEmergencyToken).
-define(EmergencyOffToken          , ?PrettyEmergencyOffToken).
-define(EmergencyValueToken        , ?PrettyEmergencyValueToken).
-define(ErrorToken                 , ?PrettyErrorToken).
-define(EventBufferToken           , ?PrettyEventBufferToken).
-define(EventsToken                , ?PrettyEventsToken).
-define(ExternalToken              , ?PrettyExternalToken).
-define(FailoverToken              , ?PrettyFailoverToken).
-define(ForcedToken                , ?PrettyForcedToken).
-define(GracefulToken              , ?PrettyGracefulToken).
-define(H221Token                  , ?PrettyH221Token).
-define(H223Token                  , ?PrettyH223Token).
-define(H226Token                  , ?PrettyH226Token).
-define(HandOffToken               , ?PrettyHandOffToken).
-define(IEPSToken                  , ?PrettyIEPSToken).
-define(ImmAckRequiredToken        , ?PrettyImmAckRequiredToken).
-define(InactiveToken              , ?PrettyInactiveToken).
-define(InternalToken              , ?PrettyInternalToken).
-define(IntsigDelayToken           , ?PrettyIntsigDelayToken).
-define(IsolateToken               , ?PrettyIsolateToken).
-define(InSvcToken                 , ?PrettyInSvcToken).
-define(InterruptByEventToken      , ?PrettyInterruptByEventToken).
-define(InterruptByNewSignalsDescrToken, ?PrettyInterruptByNewSignalsDescrToken).
-define(IterationToken             , ?PrettyIterationToken).
-define(KeepActiveToken            , ?PrettyKeepActiveToken).
-define(LocalToken                 , ?PrettyLocalToken).
-define(LocalControlToken          , ?PrettyLocalControlToken).
-define(LockStepToken              , ?PrettyLockStepToken).
-define(LoopbackToken              , ?PrettyLoopbackToken).
-define(MediaToken                 , ?PrettyMediaToken).
-define(MegacopToken               , ?PrettyMegacopToken).
-define(MessageSegmentToken        , ?PrettyMessageSegmentToken).
-define(MethodToken                , ?PrettyMethodToken).
-define(MgcIdToken                 , ?PrettyMgcIdToken).
-define(ModeToken                  , ?PrettyModeToken).
-define(ModifyToken                , ?PrettyModifyToken).
-define(ModemToken                 , ?PrettyModemToken).
-define(MoveToken                  , ?PrettyMoveToken).
-define(MtpToken                   , ?PrettyMtpToken).
-define(MuxToken                   , ?PrettyMuxToken).
-define(NeverNotifyToken           , ?PrettyNeverNotifyToken).
-define(NotifyToken                , ?PrettyNotifyToken).
-define(NotifyCompletionToken      , ?PrettyNotifyCompletionToken).
-define(NotifyImmediateToken       , ?PrettyNotifyImmediateToken).
-define(NotifyRegulatedToken       , ?PrettyNotifyRegulatedToken).
-define(Nx64kToken                 , ?PrettyNx64kToken).
-define(ObservedEventsToken        , ?PrettyObservedEventsToken).
-define(OffToken                   , ?PrettyOffToken).
-define(OnewayToken                , ?PrettyOnewayToken).
-define(OnewayBothToken            , ?PrettyOnewayBothToken).
-define(OnewayExternalToken        , ?PrettyOnewayExternalToken).
-define(OnOffToken                 , ?PrettyOnOffToken).
-define(OnToken                    , ?PrettyOnToken).
-define(OrAUDITselectToken         , ?PrettyOrAUDITselectToken).
-define(OtherReasonToken           , ?PrettyOtherReasonToken).
-define(OutOfSvcToken              , ?PrettyOutOfSvcToken).
-define(PackagesToken              , ?PrettyPackagesToken).
-define(PendingToken               , ?PrettyPendingToken).
-define(PriorityToken              , ?PrettyPriorityToken).
-define(ProfileToken               , ?PrettyProfileToken).
-define(ReasonToken                , ?PrettyReasonToken).
-define(RecvonlyToken              , ?PrettyRecvonlyToken).
-define(ReplyToken                 , ?PrettyReplyToken).
-define(ResetEventsDescriptorToken , ?PrettyResetEventsDescriptorToken).
-define(ResponseAckToken           , ?PrettyResponseAckToken).
-define(RestartToken               , ?PrettyRestartToken).
-define(RemoteToken                , ?PrettyRemoteToken).
-define(RequestIDToken             , ?PrettyRequestIDToken).
-define(ReservedGroupToken         , ?PrettyReservedGroupToken).
-define(ReservedValueToken         , ?PrettyReservedValueToken).
-define(SegmentationCompleteToken  , ?PrettySegmentationCompleteToken).
-define(SendonlyToken              , ?PrettySendonlyToken).
-define(SendrecvToken              , ?PrettySendrecvToken).
-define(ServicesToken              , ?PrettyServicesToken).
-define(ServiceStatesToken         , ?PrettyServiceStatesToken).
-define(ServiceChangeToken         , ?PrettyServiceChangeToken).
-define(ServiceChangeAddressToken  , ?PrettyServiceChangeAddressToken).
-define(ServiceChangeIncompleteToken , ?PrettyServiceChangeIncompleteToken).
-define(SignalListToken            , ?PrettySignalListToken).
-define(SignalsToken               , ?PrettySignalsToken).
-define(SignalTypeToken            , ?PrettySignalTypeToken).
-define(StatsToken                 , ?PrettyStatsToken).
-define(StreamToken                , ?PrettyStreamToken).
-define(SubtractToken              , ?PrettySubtractToken).
-define(SynchISDNToken             , ?PrettySynchISDNToken).
-define(TerminationStateToken      , ?PrettyTerminationStateToken).
-define(TestToken                  , ?PrettyTestToken).
-define(TimeOutToken               , ?PrettyTimeOutToken).
-define(TopologyToken              , ?PrettyTopologyToken).
-define(TransToken                 , ?PrettyTransToken).
-define(V18Token                   , ?PrettyV18Token).
-define(V22Token                   , ?PrettyV22Token).
-define(V22bisToken                , ?PrettyV22bisToken).
-define(V32Token                   , ?PrettyV32Token).
-define(V32bisToken                , ?PrettyV32bisToken).
-define(V34Token                   , ?PrettyV34Token).
-define(V76Token                   , ?PrettyV76Token).
-define(V90Token                   , ?PrettyV90Token).
-define(V91Token                   , ?PrettyV91Token).
-define(VersionToken               , ?PrettyVersionToken).

%%----------------------------------------------------------------------
%% Include the generator code
%%----------------------------------------------------------------------

-include("megaco_text_gen_v3.hrl").

