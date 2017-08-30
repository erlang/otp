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
%% Purpose: Encode COMPACT Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_compact_text_encoder_v2).

-export([encode_message/2, 
	 encode_transaction/2,
	 encode_action_requests/2,
	 encode_action_request/2,
	 encode_command_request/2,
	 encode_action_reply/2]).

-export([token_tag2string/1]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v2.hrl").
-define(encoder_version_pre_prev3c,true).
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
encode_action_requests(_EC, ActReqs) ->
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
token_tag2string(auditDescriptor)        -> ?CompactAuditToken;
token_tag2string(auditCapRequest)        -> ?CompactAuditCapToken;
token_tag2string(auditCapReply)          -> ?CompactAuditCapToken;
token_tag2string(auditValueRequest)      -> ?CompactAuditValueToken;
token_tag2string(auditValueReply)        -> ?CompactAuditValueToken;
%% token_tag2string(X) -> ?CompactAuthToken;
token_tag2string(bothway)                -> ?CompactBothwayToken;
token_tag2string(brief)                  -> ?CompactBriefToken;
%% token_tag2string(X) -> ?CompactBufferToken;
%% token_tag2string(X) -> ?CompactCtxToken;
%% token_tag2string(X) -> ?CompactContextAuditToken;
token_tag2string(digitMapDescriptor)     -> ?CompactDigitMapToken;
token_tag2string(digitMapToken)          -> ?CompactDigitMapToken;
%% token_tag2string(X) -> ?CompactDiscardToken;
%% token_tag2string(X) -> ?CompactDisconnectedToken;
%% token_tag2string(X) -> ?CompactDelayToken;
token_tag2string(duration)               -> ?CompactDurationToken;
%% token_tag2string(X) -> ?CompactEmbedToken;
token_tag2string(emergencyAudit)         -> ?CompactEmergencyToken;
%% token_tag2string(X)         -> ?CompactEmergencyOffToken;
token_tag2string(errorDescriptor)        -> ?CompactErrorToken;
token_tag2string(eventBufferDescriptor)  -> ?CompactEventBufferToken;
token_tag2string(eventBufferToken)       -> ?CompactEventBufferToken;
token_tag2string(eventsDescriptor)       -> ?CompactEventsToken;
token_tag2string(eventsToken)            -> ?CompactEventsToken;
%% token_tag2string(X) -> ?CompactFailoverToken;
%% token_tag2string(X) -> ?CompactForcedToken;
%% token_tag2string(X) -> ?CompactGracefulToken;
%% token_tag2string(X) -> ?CompactH221Token;
%% token_tag2string(X) -> ?CompactH223Token;
%% token_tag2string(X) -> ?CompactH226Token;
%% token_tag2string(X) -> ?CompactHandOffToken;
%% token_tag2string(X) -> ?CompactImmAckRequiredToken;
token_tag2string(inactive)                    -> ?CompactInactiveToken;
token_tag2string(onInterruptByEvent)          -> ?CompactInterruptByEventToken;
token_tag2string(onInterruptByNewSignalDescr) -> ?CompactInterruptByNewSignalsDescrToken;
token_tag2string(isolate)        	 -> ?CompactIsolateToken;
token_tag2string(inSvc)          	 -> ?CompactInSvcToken;
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
token_tag2string(notifyReq)            -> ?CompactNotifyToken;
%% token_tag2string(X) -> ?CompactNotifyCompletionToken;
%% token_tag2string(X) -> ?CompactNx64kToken;
token_tag2string(observedEventsDescriptor) -> ?CompactObservedEventsToken;
token_tag2string(observedEventsToken)      -> ?CompactObservedEventsToken;
token_tag2string(false)                -> ?CompactOffToken;
token_tag2string(off)                  -> ?CompactOffToken;
token_tag2string(oneway)               -> ?CompactOnewayToken;
token_tag2string(onOff)                -> ?CompactOnOffToken;
token_tag2string(true)                 -> ?CompactOnToken;
token_tag2string(otherReason)          -> ?CompactOtherReasonToken;
token_tag2string(outOfSvc)             -> ?CompactOutOfSvcToken;
token_tag2string(packagesDescriptor)   -> ?CompactPackagesToken;
token_tag2string(packagesToken)        -> ?CompactPackagesToken;
%% token_tag2string(X) -> ?CompactPendingToken;
token_tag2string(priorityAudit)        -> ?CompactPriorityToken;
%% token_tag2string(X) -> ?CompactProfileToken;
%% token_tag2string(X) -> ?CompactReasonToken;
token_tag2string(recvOnly)             -> ?CompactRecvonlyToken;
%% token_tag2string(X) -> ?CompactReplyToken;
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

-define(EQUAL,  [?EqualToken]).
-define(COLON,  [?ColonToken]).
-define(LBRKT,  [?LbrktToken]).
-define(RBRKT,  [?RbrktToken]).
-define(LSBRKT, [?LsbrktToken]).
-define(RSBRKT, [?RsbrktToken]).
-define(COMMA,  [?CommaToken]).
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

-define(INIT_INDENT,           []).
-define(INC_INDENT(State),     State).
-define(INDENT(State),         State).
-define(LBRKT_INDENT(_State),  [?LbrktToken]).
-define(RBRKT_INDENT(_State),  [?RbrktToken]).
-define(COMMA_INDENT(_State),  [?CommaToken]).
-define(SEP_INDENT(_State),    [?LfToken]).

%%----------------------------------------------------------------------
%% Define token macros
%%----------------------------------------------------------------------

-define(AddToken                   , ?CompactAddToken).
-define(AuditToken                 , ?CompactAuditToken).
-define(AuditCapToken              , ?CompactAuditCapToken).
-define(AuditValueToken            , ?CompactAuditValueToken).
-define(AuthToken                  , ?CompactAuthToken).
-define(BothwayToken               , ?CompactBothwayToken).
-define(BriefToken                 , ?CompactBriefToken).
-define(BufferToken                , ?CompactBufferToken).
-define(CtxToken                   , ?CompactCtxToken).
-define(ContextAuditToken          , ?CompactContextAuditToken).
-define(DigitMapToken              , ?CompactDigitMapToken).
-define(DiscardToken               , ?CompactDiscardToken).
-define(DisconnectedToken          , ?CompactDisconnectedToken).
-define(DelayToken                 , ?CompactDelayToken).
-define(DeleteToken                , ?CompactDeleteToken).
-define(DurationToken              , ?CompactDurationToken).
-define(EmbedToken                 , ?CompactEmbedToken).
-define(EmergencyToken             , ?CompactEmergencyToken).
-define(EmergencyOffToken          , ?CompactEmergencyOffToken).
-define(ErrorToken                 , ?CompactErrorToken).
-define(EventBufferToken           , ?CompactEventBufferToken).
-define(EventsToken                , ?CompactEventsToken).
-define(FailoverToken              , ?CompactFailoverToken).
-define(ForcedToken                , ?CompactForcedToken).
-define(GracefulToken              , ?CompactGracefulToken).
-define(H221Token                  , ?CompactH221Token).
-define(H223Token                  , ?CompactH223Token).
-define(H226Token                  , ?CompactH226Token).
-define(HandOffToken               , ?CompactHandOffToken).
-define(ImmAckRequiredToken        , ?CompactImmAckRequiredToken).
-define(InactiveToken              , ?CompactInactiveToken).
-define(InterruptByEventToken      , ?CompactInterruptByEventToken).
-define(InterruptByNewSignalsDescrToken, ?CompactInterruptByNewSignalsDescrToken).
-define(IsolateToken               , ?CompactIsolateToken).
-define(InSvcToken                 , ?CompactInSvcToken).
-define(KeepActiveToken            , ?CompactKeepActiveToken).
-define(LocalToken                 , ?CompactLocalToken).
-define(LocalControlToken          , ?CompactLocalControlToken).
-define(LockStepToken              , ?CompactLockStepToken).
-define(LoopbackToken              , ?CompactLoopbackToken).
-define(MediaToken                 , ?CompactMediaToken).
-define(MegacopToken               , ?CompactMegacopToken).
-define(MethodToken                , ?CompactMethodToken).
-define(MgcIdToken                 , ?CompactMgcIdToken).
-define(ModeToken                  , ?CompactModeToken).
-define(ModifyToken                , ?CompactModifyToken).
-define(ModemToken                 , ?CompactModemToken).
-define(MoveToken                  , ?CompactMoveToken).
-define(MtpToken                   , ?CompactMtpToken).
-define(MuxToken                   , ?CompactMuxToken).
-define(NotifyToken                , ?CompactNotifyToken).
-define(NotifyCompletionToken      , ?CompactNotifyCompletionToken).
-define(Nx64kToken                 , ?CompactNx64kToken).
-define(ObservedEventsToken        , ?CompactObservedEventsToken).
-define(OffToken                   , ?CompactOffToken).
-define(OnewayToken                , ?CompactOnewayToken).
-define(OnOffToken                 , ?CompactOnOffToken).
-define(OnToken                    , ?CompactOnToken).
-define(OtherReasonToken           , ?CompactOtherReasonToken).
-define(OutOfSvcToken              , ?CompactOutOfSvcToken).
-define(PackagesToken              , ?CompactPackagesToken).
-define(PendingToken               , ?CompactPendingToken).
-define(PriorityToken              , ?CompactPriorityToken).
-define(ProfileToken               , ?CompactProfileToken).
-define(ReasonToken                , ?CompactReasonToken).
-define(RecvonlyToken              , ?CompactRecvonlyToken).
-define(ReplyToken                 , ?CompactReplyToken).
-define(ResponseAckToken           , ?CompactResponseAckToken).
-define(RestartToken               , ?CompactRestartToken).
-define(RemoteToken                , ?CompactRemoteToken).
-define(ReservedGroupToken         , ?CompactReservedGroupToken).
-define(ReservedValueToken         , ?CompactReservedValueToken).
-define(SendonlyToken              , ?CompactSendonlyToken).
-define(SendrecvToken              , ?CompactSendrecvToken).
-define(ServicesToken              , ?CompactServicesToken).
-define(ServiceStatesToken         , ?CompactServiceStatesToken).
-define(ServiceChangeToken         , ?CompactServiceChangeToken).
-define(ServiceChangeAddressToken  , ?CompactServiceChangeAddressToken).
-define(SignalListToken            , ?CompactSignalListToken).
-define(SignalsToken               , ?CompactSignalsToken).
-define(SignalTypeToken            , ?CompactSignalTypeToken).
-define(StatsToken                 , ?CompactStatsToken).
-define(StreamToken                , ?CompactStreamToken).
-define(SubtractToken              , ?CompactSubtractToken).
-define(SynchISDNToken             , ?CompactSynchISDNToken).
-define(TerminationStateToken      , ?CompactTerminationStateToken).
-define(TestToken                  , ?CompactTestToken).
-define(TimeOutToken               , ?CompactTimeOutToken).
-define(TopologyToken              , ?CompactTopologyToken).
-define(TransToken                 , ?CompactTransToken).
-define(V18Token                   , ?CompactV18Token).
-define(V22Token                   , ?CompactV22Token).
-define(V22bisToken                , ?CompactV22bisToken).
-define(V32Token                   , ?CompactV32Token).
-define(V32bisToken                , ?CompactV32bisToken).
-define(V34Token                   , ?CompactV34Token).
-define(V76Token                   , ?CompactV76Token).
-define(V90Token                   , ?CompactV90Token).
-define(V91Token                   , ?CompactV91Token).
-define(VersionToken               , ?CompactVersionToken).

%%----------------------------------------------------------------------
%% Include the generator code
%%----------------------------------------------------------------------

-include("megaco_text_gen_v2.hrl").

