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

-module(megaco_pretty_text_encoder_prev3b).

-export([encode_message/2,
	 encode_transaction/2,
	 encode_action_requests/2,
	 encode_action_request/2,
	 encode_command_request/2,
	 encode_action_reply/2]).

-export([token_tag2string/1]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_prev3b.hrl").
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

token_tag2string(addReq)                 -> ?PrettyAddToken;
token_tag2string(addReply)               -> ?PrettyAddToken;
token_tag2string(auditDescriptor)        -> ?PrettyAuditToken;
token_tag2string(auditCapRequest)        -> ?PrettyAuditCapToken;
token_tag2string(auditCapReply)          -> ?PrettyAuditCapToken;
token_tag2string(auditValueRequest)      -> ?PrettyAuditValueToken;
token_tag2string(auditValueReply)        -> ?PrettyAuditValueToken;
%% token_tag2string(X) -> ?PrettyAuthToken;
token_tag2string(both)                   -> ?PrettyBothToken;
token_tag2string(bothway)                -> ?PrettyBothwayToken;
token_tag2string(brief)                  -> ?PrettyBriefToken;
%% token_tag2string(X) -> ?PrettyBufferToken;
%% token_tag2string(X) -> ?PrettyCtxToken;
%% token_tag2string(X) -> ?PrettyContextAttrToken;
%% token_tag2string(X) -> ?PrettyContextAuditToken;
%% token_tag2string(X) -> ?PrettyContextListToken;
token_tag2string(digitMapDescriptor)     -> ?PrettyDigitMapToken;
token_tag2string(digitMapToken)          -> ?PrettyDigitMapToken;
%% token_tag2string(X) -> ?PrettyDirectionToken;
%% token_tag2string(X) -> ?PrettyDiscardToken;
%% token_tag2string(X) -> ?PrettyDisconnectedToken;
%% token_tag2string(X) -> ?PrettyDelayToken;
token_tag2string(duration)               -> ?PrettyDurationToken;
%% token_tag2string(X) -> ?PrettyEmbedToken;
token_tag2string(emergencyAudit)         -> ?PrettyEmergencyToken;
%% token_tag2string(X)         -> ?PrettyEmergencyOffToken;
token_tag2string(errorDescriptor)        -> ?PrettyErrorToken;
token_tag2string(eventBufferDescriptor)  -> ?PrettyEventBufferToken;
token_tag2string(eventBufferToken)       -> ?PrettyEventBufferToken;
token_tag2string(eventsDescriptor)       -> ?PrettyEventsToken;
token_tag2string(eventsToken)            -> ?PrettyEventsToken;
token_tag2string(external)               -> ?PrettyExternalToken;
%% token_tag2string(X) -> ?PrettyFailoverToken;
%% token_tag2string(X) -> ?PrettyForcedToken;
%% token_tag2string(X) -> ?PrettyGracefulToken;
%% token_tag2string(X) -> ?PrettyH221Token;
%% token_tag2string(X) -> ?PrettyH223Token;
%% token_tag2string(X) -> ?PrettyH226Token;
%% token_tag2string(X) -> ?PrettyHandOffToken;
token_tag2string(iepsCallind)                 -> ?PrettyIEPSToken;
%% token_tag2string(X) -> ?PrettyImmAckRequiredToken;
token_tag2string(inactive)                    -> ?PrettyInactiveToken;
token_tag2string(internal)                    -> ?PrettyInternalToken;
token_tag2string(onInterruptByEvent)          -> ?PrettyInterruptByEventToken;
token_tag2string(onInterruptByNewSignalDescr) -> ?PrettyInterruptByNewSignalsDescrToken;
token_tag2string(isolate)        	 -> ?PrettyIsolateToken;
token_tag2string(inSvc)          	 -> ?PrettyInSvcToken;
token_tag2string(keepActive)     	 -> ?PrettyKeepActiveToken;
token_tag2string(localDescriptor)        -> ?PrettyLocalToken;
token_tag2string(localControlDescriptor) -> ?PrettyLocalControlToken;
token_tag2string(lockStep)          	 -> ?PrettyLockStepToken;
token_tag2string(loopBack)          	 -> ?PrettyLoopbackToken;
token_tag2string(mediaDescriptor)   	 -> ?PrettyMediaToken;
token_tag2string(mediaToken)        	 -> ?PrettyMediaToken;
%% token_tag2string(X) -> ?PrettyMegacopToken;
%% token_tag2string(X) -> ?PrettyMethodToken;
%% token_tag2string(X) -> ?PrettyMgcIdToken;
%% token_tag2string(X) -> ?PrettyModeToken;
token_tag2string(modReq)               -> ?PrettyModifyToken;
token_tag2string(modReply)             -> ?PrettyModifyToken;
token_tag2string(modemDescriptor)      -> ?PrettyModemToken;
token_tag2string(modemToken)           -> ?PrettyModemToken;
token_tag2string(moveReq)              -> ?PrettyMoveToken;
token_tag2string(moveReply)            -> ?PrettyMoveToken;
%% token_tag2string(X) -> ?PrettyMtpToken;
token_tag2string(muxDescriptor)        -> ?PrettyMuxToken;
token_tag2string(muxToken)             -> ?PrettyMuxToken;
token_tag2string(notifyReq)            -> ?PrettyNotifyToken;
%% token_tag2string(X) -> ?PrettyNotifyCompletionToken;
%% token_tag2string(X) -> ?PrettyNx64kToken;
token_tag2string(observedEventsDescriptor) -> ?PrettyObservedEventsToken;
token_tag2string(observedEventsToken)      -> ?PrettyObservedEventsToken;
token_tag2string(false)                -> ?PrettyOffToken;
token_tag2string(off)                  -> ?PrettyOffToken;
token_tag2string(oneway)               -> ?PrettyOnewayToken;
token_tag2string(onOff)                -> ?PrettyOnOffToken;
token_tag2string(true)                 -> ?PrettyOnToken;
token_tag2string(otherReason)          -> ?PrettyOtherReasonToken;
token_tag2string(outOfSvc)             -> ?PrettyOutOfSvcToken;
token_tag2string(packagesDescriptor)   -> ?PrettyPackagesToken;
token_tag2string(packagesToken)        -> ?PrettyPackagesToken;
%% token_tag2string(X) -> ?PrettyPendingToken;
token_tag2string(priorityAudit)        -> ?PrettyPriorityToken;
%% token_tag2string(X) -> ?PrettyProfileToken;
%% token_tag2string(X) -> ?PrettyReasonToken;
token_tag2string(recvOnly)             -> ?PrettyRecvonlyToken;
%% token_tag2string(X) -> ?PrettyReplyToken;
%% token_tag2string(X) -> ?PrettyRequestIDToken;
%% token_tag2string(X) -> ?PrettyResponseAckToken;
%% token_tag2string(X) -> ?PrettyRestartToken;
token_tag2string(remoteDescriptor)     -> ?PrettyRemoteToken;
%% token_tag2string(X) -> ?PrettyReservedGroupToken;
%% token_tag2string(X) -> ?PrettyReservedValueToken;
token_tag2string(sendOnly)             -> ?PrettySendonlyToken;
token_tag2string(sendRecv)             -> ?PrettySendrecvToken;
%% token_tag2string(X) -> ?PrettyServicesToken;
%% token_tag2string(X) -> ?PrettyServiceStatesToken;
token_tag2string(serviceChangeReq)     -> ?PrettyServiceChangeToken;
%% token_tag2string(X) -> ?PrettyServiceChangeAddressToken;
token_tag2string(incomplete)           -> ?PrettyServiceChangeIncompleteToken;
%% token_tag2string(X) -> ?PrettySignalListToken;
token_tag2string(signalsDescriptor)    -> ?PrettySignalsToken;
token_tag2string(signalsToken)         -> ?PrettySignalsToken; 
%% token_tag2string(X) -> ?PrettySignalTypeToken;
token_tag2string(statisticsDescriptor) -> ?PrettyStatsToken;
token_tag2string(statsToken)           -> ?PrettyStatsToken;
%% token_tag2string(X) -> ?PrettyStreamToken;
token_tag2string(subtractReq)          -> ?PrettySubtractToken;
token_tag2string(subtractReply)        -> ?PrettySubtractToken;
%% token_tag2string(X) -> ?PrettySynchISDNToken;
%% token_tag2string(X) -> ?PrettyTerminationStateToken;
token_tag2string(test)                 -> ?PrettyTestToken;
token_tag2string(timeOut)              -> ?PrettyTimeOutToken;
token_tag2string(onTimeOut)            -> ?PrettyTimeOutToken;
token_tag2string(topologyAudit)        -> ?PrettyTopologyToken;
%% token_tag2string(X) -> ?PrettyTransToken;
%% token_tag2string(X) -> ?PrettyV18Token;
%% token_tag2string(X) -> ?PrettyV22Token;
%% token_tag2string(X) -> ?PrettyV22bisToken;
%% token_tag2string(X) -> ?PrettyV32Token;
%% token_tag2string(X) -> ?PrettyV32bisToken;
%% token_tag2string(X) -> ?PrettyV34Token;
%% token_tag2string(X) -> ?PrettyV76Token;
%% token_tag2string(X) -> ?PrettyV90Token;
%% token_tag2string(X) -> ?PrettyV91Token;
%% token_tag2string(X) -> ?PrettyVersionToken;
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
-define(IsolateToken               , ?PrettyIsolateToken).
-define(InSvcToken                 , ?PrettyInSvcToken).
-define(InterruptByEventToken      , ?PrettyInterruptByEventToken).
-define(InterruptByNewSignalsDescrToken, ?PrettyInterruptByNewSignalsDescrToken).
-define(KeepActiveToken            , ?PrettyKeepActiveToken).
-define(LocalToken                 , ?PrettyLocalToken).
-define(LocalControlToken          , ?PrettyLocalControlToken).
-define(LockStepToken              , ?PrettyLockStepToken).
-define(LoopbackToken              , ?PrettyLoopbackToken).
-define(MediaToken                 , ?PrettyMediaToken).
-define(MegacopToken               , ?PrettyMegacopToken).
-define(MethodToken                , ?PrettyMethodToken).
-define(MgcIdToken                 , ?PrettyMgcIdToken).
-define(ModeToken                  , ?PrettyModeToken).
-define(ModifyToken                , ?PrettyModifyToken).
-define(ModemToken                 , ?PrettyModemToken).
-define(MoveToken                  , ?PrettyMoveToken).
-define(MtpToken                   , ?PrettyMtpToken).
-define(MuxToken                   , ?PrettyMuxToken).
-define(NotifyToken                , ?PrettyNotifyToken).
-define(NotifyCompletionToken      , ?PrettyNotifyCompletionToken).
-define(Nx64kToken                 , ?PrettyNx64kToken).
-define(ObservedEventsToken        , ?PrettyObservedEventsToken).
-define(OffToken                   , ?PrettyOffToken).
-define(OnewayToken                , ?PrettyOnewayToken).
-define(OnOffToken                 , ?PrettyOnOffToken).
-define(OnToken                    , ?PrettyOnToken).
-define(OtherReasonToken           , ?PrettyOtherReasonToken).
-define(OutOfSvcToken              , ?PrettyOutOfSvcToken).
-define(PackagesToken              , ?PrettyPackagesToken).
-define(PendingToken               , ?PrettyPendingToken).
-define(PriorityToken              , ?PrettyPriorityToken).
-define(ProfileToken               , ?PrettyProfileToken).
-define(ReasonToken                , ?PrettyReasonToken).
-define(RecvonlyToken              , ?PrettyRecvonlyToken).
-define(ReplyToken                 , ?PrettyReplyToken).
-define(RequestIDToken             , ?PrettyRequestIDToken).
-define(ResponseAckToken           , ?PrettyResponseAckToken).
-define(RestartToken               , ?PrettyRestartToken).
-define(RemoteToken                , ?PrettyRemoteToken).
-define(ReservedGroupToken         , ?PrettyReservedGroupToken).
-define(ReservedValueToken         , ?PrettyReservedValueToken).
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

-include("megaco_text_gen_prev3b.hrl").

