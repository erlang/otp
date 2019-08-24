%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(megaco_compact_text_encoder_prev3a).

-export([encode_message/2, 
	 encode_transaction/2,
	 encode_action_requests/2,
	 encode_action_request/2,
	 encode_command_request/2,
	 encode_action_reply/2]).


-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_prev3a.hrl").
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
-define(LSBRKT_INDENT(_State), [?LsbrktToken]).
-define(RSBRKT_INDENT(_State), [?RsbrktToken]).
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
-define(BothToken                  , ?CompactBothToken).
-define(BothwayToken               , ?CompactBothwayToken).
-define(BriefToken                 , ?CompactBriefToken).
-define(BufferToken                , ?CompactBufferToken).
-define(CtxToken                   , ?CompactCtxToken).
-define(ContextAuditToken          , ?CompactContextAuditToken).
-define(ContextAttrToken           , ?CompactContextAttrToken).
-define(DigitMapToken              , ?CompactDigitMapToken).
-define(DirectionToken             , ?CompactDirectionToken).
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
-define(ExternalToken              , ?CompactExternalToken).
-define(FailoverToken              , ?CompactFailoverToken).
-define(ForcedToken                , ?CompactForcedToken).
-define(GracefulToken              , ?CompactGracefulToken).
-define(H221Token                  , ?CompactH221Token).
-define(H223Token                  , ?CompactH223Token).
-define(H226Token                  , ?CompactH226Token).
-define(HandOffToken               , ?CompactHandOffToken).
-define(IEPSToken                  , ?CompactIEPSToken).
-define(ImmAckRequiredToken        , ?CompactImmAckRequiredToken).
-define(InactiveToken              , ?CompactInactiveToken).
-define(InternalToken              , ?CompactInternalToken).
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
-define(RequestIDToken             , ?CompactRequestIDToken).
-define(ReservedGroupToken         , ?CompactReservedGroupToken).
-define(ReservedValueToken         , ?CompactReservedValueToken).
-define(SendonlyToken              , ?CompactSendonlyToken).
-define(SendrecvToken              , ?CompactSendrecvToken).
-define(ServicesToken              , ?CompactServicesToken).
-define(ServiceStatesToken         , ?CompactServiceStatesToken).
-define(ServiceChangeToken         , ?CompactServiceChangeToken).
-define(ServiceChangeAddressToken  , ?CompactServiceChangeAddressToken).
-define(ServiceChangeIncompleteToken , ?CompactServiceChangeIncompleteToken).
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

-include("megaco_text_gen_prev3a.hrl").

