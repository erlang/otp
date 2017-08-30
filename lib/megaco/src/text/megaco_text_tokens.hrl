%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% Purpose: Define of tokens used in text encoding of Megaco/H.248
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Adding a new token requires changes in the following files:
%%    
%%    megaco_text_tokens.hrl
%%    megaco_text_gen.hrl
%%    megaco_compact_text_encoder.erl
%%    megaco_pretty_text_encoder.erl
%%    megaco_text_scanner.erl
%%    megaco_text_parser.yrl (safeToken rule, make_safe_token/1, actual rule)
%%
%% Plus regeneration the ASN.1 related files including
%% manual patches 
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Special records
%%----------------------------------------------------------------------

-record(property_parm,
        {
          name,
          value,
          extraInfo = asn1_NOVALUE
         }). 



%%----------------------------------------------------------------------
%% Special characters
%%----------------------------------------------------------------------

-define(EqualToken,       16#3d).
-define(ColonToken,       16#3a).
-define(LbrktToken,       16#7b).
-define(RbrktToken,       16#7d).
-define(LsbrktToken,      $[).
-define(RsbrktToken,      $]).
-define(CommaToken,       16#2c).
-define(DotToken,         16#2e).
-define(SlashToken,       16#2f).
-define(DoubleQuoteToken, 16#22).
-define(SpToken,          16#20).
-define(HtabToken,        16#09).
-define(CrToken,          16#0d).
-define(LfToken,          16#0a).

-define(SemiColonToken,   $;).
-define(NequalToken,      $#).
-define(GreaterToken,     $>).
-define(LesserToken,      $<).
-define(BackslashToken,   $\\).
-define(LparToken,        $().
-define(RparToken,        $)).
-define(VbarToken,        $|).

%%----------------------------------------------------------------------
%% Pretty version of tokens
%%----------------------------------------------------------------------

-define(PrettyAddToken                   , "Add"                   ).
-define(PrettyAndAUDITSelectToken        , "ANSLgc"                ).
-define(PrettyAuditToken                 , "Audit"                 ).
-define(PrettyAuditCapToken              , "AuditCapability"       ).
-define(PrettyAuditValueToken            , "AuditValue"            ).
-define(PrettyAuthToken                  , "Authentication"        ).
-define(PrettyBothToken                  , "Both"                  ). % v3
-define(PrettyBothwayToken               , "Bothway"               ).
-define(PrettyBriefToken                 , "Brief"                 ).
-define(PrettyBufferToken                , "Buffer"                ).
-define(PrettyCtxToken                   , "Context"               ).
-define(PrettyContextAuditToken          , "ContextAudit"          ).
-define(PrettyContextAttrToken           , "ContextAttr"           ). % v3
-define(PrettyContextListToken           , "ContextList"           ). % v3
-define(PrettyDigitMapToken              , "DigitMap"              ).
-ifdef(encoder_version_pre_prev3c).
-define(PrettyDirectionToken             , "Direction"             ). % v3 
-else.
-define(PrettyDirectionToken             , "SPADirection"          ). % v3 
-endif.
-define(PrettyDiscardToken               , "Discard"               ).
-define(PrettyDisconnectedToken          , "Disconnected"          ).
-define(PrettyDelayToken                 , "Delay"                 ).
-define(PrettyDurationToken              , "Duration"              ).
-define(PrettyEmbedToken                 , "Embed"                 ).
-define(PrettyEmergencyToken             , "Emergency"             ).
-ifdef(encoder_version_pre_prev3c).
-define(PrettyEmergencyOffToken          , "EmergencyOffToken"     ). % v2
-else.
-define(PrettyEmergencyOffToken          , "EmergencyOff"          ). % v3
-endif.
-define(PrettyEmergencyValueToken        , "EmergencyValue"        ). % v3
-define(PrettyErrorToken                 , "Error"                 ).
-define(PrettyEventBufferToken           , "EventBuffer"           ).
-define(PrettyEventsToken                , "Events"                ).
-define(PrettyExternalToken              , "External"              ). % v3
-define(PrettyFailoverToken              , "Failover"              ).
-define(PrettyForcedToken                , "Forced"                ).
-define(PrettyGracefulToken              , "Graceful"              ).
-define(PrettyH221Token                  , "H221"                  ).
-define(PrettyH223Token                  , "H223"                  ).
-define(PrettyH226Token                  , "H226"                  ).
-define(PrettyHandOffToken               , "HandOff"               ).
-define(PrettyIEPSToken                  , "IEPSCall"              ). % v3
-define(PrettyImmAckRequiredToken        , "ImmAckRequired"        ).
-define(PrettyInactiveToken              , "Inactive"              ).
-define(PrettyInternalToken              , "Internal"              ). % v3
-define(PrettyIntsigDelayToken           , "Intersignal"           ). % v3
-define(PrettyInterruptByEventToken      , "IntByEvent"            ). 
-define(PrettyInterruptByNewSignalsDescrToken, "IntBySigDescr"     ). 
-define(PrettyIsolateToken               , "Isolate"               ).
-define(PrettyInSvcToken                 , "InService"             ).
-define(PrettyIterationToken             , "Iteration"             ). % v3
-define(PrettyKeepActiveToken            , "KeepActive"            ).
-define(PrettyLocalToken                 , "Local"                 ).
-define(PrettyLocalControlToken          , "LocalControl"          ).
-define(PrettyLockStepToken              , "LockStep"              ).
-define(PrettyLoopbackToken              , "Loopback"              ).
-define(PrettyMediaToken                 , "Media"                 ).
-define(PrettyMegacopToken               , "MEGACO"                ).
-define(PrettyMessageSegmentToken        , "Segment"               ). % v3
-define(PrettyMethodToken                , "Method"                ).
-define(PrettyMgcIdToken                 , "MgcIdToTry"            ).
-define(PrettyModeToken                  , "Mode"                  ).
-define(PrettyModifyToken                , "Modify"                ).
-define(PrettyModemToken                 , "Modem"                 ).
-define(PrettyMoveToken                  , "Move"                  ).
-define(PrettyMtpToken                   , "MTP"                   ).
-define(PrettyMuxToken                   , "Mux"                   ).
-define(PrettyNeverNotifyToken           , "NeverNotify"           ). % v3
-define(PrettyNotifyToken                , "Notify"                ).
-define(PrettyNotifyCompletionToken      , "NotifyCompletion"      ).
-define(PrettyNotifyImmediateToken       , "ImmediateNotify"       ). % v3
-define(PrettyNotifyRegulatedToken       , "RegulatedNotify"       ). % v3
-define(PrettyNx64kToken                 , "Nx64Kservice"          ).
-define(PrettyObservedEventsToken        , "ObservedEvents"        ).
-define(PrettyOffToken                   , "OFF"                   ).
-define(PrettyOnewayToken                , "Oneway"                ).
-define(PrettyOnewayBothToken            , "OnewayBoth"            ). % v3
-define(PrettyOnewayExternalToken        , "OnewayExternal"        ). % v3
-define(PrettyOnOffToken                 , "OnOff"                 ).
-define(PrettyOrAUDITselectToken         , "ORLgc"                 ). % v3
-define(PrettyOnToken                    , "ON"                    ).
-define(PrettyOtherReasonToken           , "OtherReason"           ).
-define(PrettyOutOfSvcToken              , "OutOfService"          ).
-define(PrettyPackagesToken              , "Packages"              ).
-define(PrettyPendingToken               , "Pending"               ).
-define(PrettyPriorityToken              , "Priority"              ).
-define(PrettyProfileToken               , "Profile"               ).
-define(PrettyReasonToken                , "Reason"                ).
-define(PrettyRecvonlyToken              , "ReceiveOnly"           ).
-define(PrettyReplyToken                 , "Reply"                 ).
-define(PrettyResetEventsDescriptorToken , "ResetEventsDescriptor" ). % v3
-define(PrettyRestartToken               , "Restart"               ).
-define(PrettyRemoteToken                , "Remote"                ).
-ifdef(encoder_version_pre_prev3c).
-define(PrettyRequestIDToken             , "RequestID"             ). % v3
-else.
-define(PrettyRequestIDToken             , "SPARequestID"          ). % v3
-endif.
-define(PrettyReservedGroupToken         , "ReservedGroup"         ).
-define(PrettyReservedValueToken         , "ReservedValue"         ).
-define(PrettySegmentationCompleteToken  , "END"                   ). % v3
-define(PrettySendonlyToken              , "SendOnly"              ).
-define(PrettySendrecvToken              , "SendReceive"           ).
-define(PrettyServicesToken              , "Services"              ).
-define(PrettyServiceStatesToken         , "ServiceStates"         ).
-define(PrettyServiceChangeToken         , "ServiceChange"         ).
-define(PrettyServiceChangeAddressToken  , "ServiceChangeAddress"  ).
-define(PrettyServiceChangeIncompleteToken , "ServiceChangeInc"    ). % v3
-define(PrettySignalListToken            , "SignalList"            ).
-define(PrettySignalsToken               , "Signals"               ).
-define(PrettySignalTypeToken            , "SignalType"            ).
-define(PrettyStatsToken                 , "Statistics"            ).
-define(PrettyStreamToken                , "Stream"                ).
-define(PrettySubtractToken              , "Subtract"              ).
-define(PrettySynchISDNToken             , "SynchISDN"             ).
-define(PrettyTerminationStateToken      , "TerminationState"      ).
-define(PrettyTestToken                  , "Test"                  ).
-define(PrettyTimeOutToken               , "TimeOut"               ).
-define(PrettyTopologyToken              , "Topology"              ).
-define(PrettyTransToken                 , "Transaction"           ).
-define(PrettyResponseAckToken           , "TransactionResponseAck").
-define(PrettyV18Token                   , "V18"                   ).
-define(PrettyV22Token                   , "V22"                   ).
-define(PrettyV22bisToken                , "V22b"                  ).
-define(PrettyV32Token                   , "V32"                   ).
-define(PrettyV32bisToken                , "V32b"                  ).
-define(PrettyV34Token                   , "V34"                   ).
-define(PrettyV76Token                   , "V76"                   ).
-define(PrettyV90Token                   , "V90"                   ).
-define(PrettyV91Token                   , "V91"                   ).
-define(PrettyVersionToken               , "Version"               ).

%%----------------------------------------------------------------------
%% Compact version of tokens
%%----------------------------------------------------------------------

-define(CompactAddToken                   , "A"                    ).
-define(CompactAndAUDITSelectToken        , "ANSLgc"               ).
-define(CompactAuditToken                 , "AT"                   ).
-define(CompactAuditCapToken              , "AC"                   ).
-define(CompactAuditValueToken            , "AV"                   ).
-define(CompactAuthToken                  , "AU"                   ).
-define(CompactBothToken                  , "B"                    ). % v3
-define(CompactBothwayToken               , "BW"                   ).
-define(CompactBriefToken                 , "BR"                   ).
-define(CompactBufferToken                , "BF"                   ).
-define(CompactCtxToken                   , "C"                    ).
-define(CompactContextAuditToken          , "CA"                   ).
-define(CompactContextAttrToken           , "CT"                   ). % v3
-define(CompactContextListToken           , "CLT"                  ). % v3
-define(CompactDigitMapToken              , "DM"                   ).
-ifdef(encoder_version_pre_prev3c).
-define(CompactDirectionToken             , "DI"                   ). % v3 
-else.
-define(CompactDirectionToken             , "SPADI"                ). % v3 
-endif.
-define(CompactDiscardToken               , "DS"                   ).
-define(CompactDisconnectedToken          , "DC"                   ).
-define(CompactDelayToken                 , "DL"                   ).
-define(CompactDurationToken              , "DR"                   ).
-define(CompactEmbedToken                 , "EM"                   ).
-define(CompactEmergencyToken             , "EG"                   ).
-define(CompactEmergencyOffToken          , "EGO"                  ).
-define(CompactEmergencyValueToken        , "EGV"                  ). % v3
-define(CompactErrorToken                 , "ER"                   ).
-define(CompactEventBufferToken           , "EB"                   ).
-define(CompactEventsToken                , "E"                    ).
-define(CompactExternalToken              , "EX"                   ). % v3
-define(CompactFailoverToken              , "FL"                   ).
-define(CompactForcedToken                , "FO"                   ).
-define(CompactGracefulToken              , "GR"                   ).
-define(CompactH221Token                  , ?PrettyH221Token       ).
-define(CompactH223Token                  , ?PrettyH223Token       ).
-define(CompactH226Token                  , ?PrettyH226Token       ).
-define(CompactHandOffToken               , "HO"                   ).
-define(CompactIEPSToken                  , "IEPS"                 ). % v3
-define(CompactImmAckRequiredToken        , "IA"                   ).
-define(CompactInactiveToken              , "IN"                   ).
-define(CompactInternalToken              , "IT"                   ). % v3
-define(CompactIntsigDelayToken           , "SPAIS"                ). % v3
-define(CompactInterruptByEventToken      , "IBE"                  ).
-define(CompactInterruptByNewSignalsDescrToken, "IBS"              ). 
-define(CompactIsolateToken               , "IS"                   ).
-define(CompactInSvcToken                 , "IV"                   ).
-define(CompactIterationToken             , "IR"                   ). % v3
-define(CompactKeepActiveToken            , "KA"                   ).
-define(CompactLocalToken                 , "L"                    ).
-define(CompactLocalControlToken          , "O"                    ).
-define(CompactLockStepToken              , "SP"                   ).
-define(CompactLoopbackToken              , "LB"                   ).
-define(CompactMediaToken                 , "M"                    ).
-define(CompactMegacopToken               , "!"                    ).
-define(CompactMessageSegmentToken        , "SM"                   ). % v3
-define(CompactMethodToken                , "MT"                   ).
-define(CompactMgcIdToken                 , "MG"                   ).
-define(CompactModeToken                  , "MO"                   ).
-define(CompactModifyToken                , "MF"                   ).
-define(CompactModemToken                 , "MD"                   ).
-define(CompactMoveToken                  , "MV"                   ).
-define(CompactMtpToken                   , ?PrettyMtpToken        ).
-define(CompactMuxToken                   , "MX"                   ).
-define(CompactNeverNotifyToken           , "NBNN"                 ). % v3
-define(CompactNotifyToken                , "N"                    ).
-define(CompactNotifyCompletionToken      , "NC"                   ).
-define(CompactNotifyImmediateToken       , "NBIN"                 ). % v3
-define(CompactNotifyRegulatedToken       , "NBRN"                 ). % v3
-define(CompactNx64kToken                 , "N64"                  ).
-define(CompactObservedEventsToken        , "OE"                   ).
-define(CompactOffToken                   , "OFF"                  ).
-define(CompactOnewayToken                , "OW"                   ).
-define(CompactOnewayBothToken            , "OWB"                  ). % v3
-define(CompactOnewayExternalToken        , "OWE"                  ). % v3
-define(CompactOnOffToken                 , "OO"                   ).
-define(CompactOrAUDITselectToken         , "ORLgc"                ). % v3
-define(CompactOnToken                    , "ON"                   ).
-define(CompactOtherReasonToken           , "OR"                   ).
-define(CompactOutOfSvcToken              , "OS"                   ).
-define(CompactPackagesToken              , "PG"                   ).
-define(CompactPendingToken               , "PN"                   ).
-define(CompactPriorityToken              , "PR"                   ).
-define(CompactProfileToken               , "PF"                   ).
-define(CompactReasonToken                , "RE"                   ).
-define(CompactRecvonlyToken              , "RC"                   ).
-define(CompactReplyToken                 , "P"                    ).
-define(CompactResetEventsDescriptorToken , "RSE"                  ). % v3
-define(CompactRestartToken               , "RS"                   ).
-define(CompactRemoteToken                , "R"                    ).
-ifdef(encoder_version_pre_prev3c).
-define(CompactRequestIDToken             , "RQ"                   ). % v3
-else.
-define(CompactRequestIDToken             , "SPARQ"                ). % v3
-endif.
-define(CompactReservedGroupToken         , "RG"                   ).
-define(CompactReservedValueToken         , "RV"                   ).
-define(CompactSegmentationCompleteToken  , "&"                    ). % v3
-define(CompactSendonlyToken              , "SO"                   ).
-define(CompactSendrecvToken              , "SR"                   ).
-define(CompactServicesToken              , "SV"                   ).
-define(CompactServiceStatesToken         , "SI"                   ).
-define(CompactServiceChangeToken         , "SC"                   ).
-define(CompactServiceChangeAddressToken  , "AD"                   ).
-define(CompactServiceChangeIncompleteToken , "SIC"                ). % v3
-define(CompactSignalListToken            , "SL"                   ).
-define(CompactSignalsToken               , "SG"                   ).
-define(CompactSignalTypeToken            , "SY"                   ).
-define(CompactStatsToken                 , "SA"                   ).
-define(CompactStreamToken                , "ST"                   ).
-define(CompactSubtractToken              , "S"                    ).
-define(CompactSynchISDNToken             , "SN"                   ).
-define(CompactTerminationStateToken      , "TS"                   ).
-define(CompactTestToken                  , "TE"                   ).
-define(CompactTimeOutToken               , "TO"                   ).
-define(CompactTopologyToken              , "TP"                   ).
-define(CompactTransToken                 , "T"                    ).
-define(CompactResponseAckToken           , "K"                    ).
-define(CompactV18Token                   , ?PrettyV18Token        ).
-define(CompactV22Token                   , ?PrettyV22Token        ).
-define(CompactV22bisToken                , ?PrettyV22bisToken     ).
-define(CompactV32Token                   , ?PrettyV32Token        ).
-define(CompactV32bisToken                , ?PrettyV32bisToken     ).
-define(CompactV34Token                   , ?PrettyV34Token        ).
-define(CompactV76Token                   , ?PrettyV76Token        ).
-define(CompactV90Token                   , ?PrettyV90Token        ).
-define(CompactV91Token                   , ?PrettyV91Token        ).
-define(CompactVersionToken               , "V"                    ).

-define(white_space(Char),     ((Char) =:= ?SpToken) orelse ((Char) =:= ?HtabToken)).
-define(end_of_line(Char),     ((Char) =:= ?LfToken) orelse ((Char) =:= ?CrToken)).

-define(classify_char(Char),
    (case Char of
        $+                                       -> safe_char;
        $-                                       -> safe_char;
        $&                                       -> safe_char;
        $!                                       -> safe_char;
        $_                                       -> safe_char;
        $/                                       -> safe_char;
        $'                                       -> safe_char;
        $?                                       -> safe_char;
        $@                                       -> safe_char;
        $^                                       -> safe_char;
        $`                                       -> safe_char;
        $~                                       -> safe_char;
        $*                                       -> safe_char;
        $$                                       -> safe_char;
        ?BackslashToken                          -> safe_char;
        ?LparToken                               -> safe_char;
        ?RparToken                               -> safe_char;
        $%                                       -> safe_char;
        ?VbarToken                               -> safe_char;
        $.                                       -> safe_char;
        ?SemiColonToken                          -> rest_char;
        ?LsbrktToken                             -> rest_char;
        ?RsbrktToken                             -> rest_char;
        ?LbrktToken                              -> rest_char;
        ?RbrktToken                              -> rest_char;
        ?ColonToken                              -> rest_char;
        ?CommaToken                              -> rest_char;
        ?NequalToken                             -> rest_char;
        ?LesserToken                             -> rest_char;
        ?GreaterToken                            -> rest_char;
        ?EqualToken                              -> rest_char;
        ?DoubleQuoteToken                        -> double_quote;
        ?SpToken                                 -> white_space;
        ?HtabToken                               -> white_space;
        ?LfToken                                 -> end_of_line;
        ?CrToken                                 -> end_of_line;
        _ when (Char >= $0) andalso (Char =< $9) -> safe_char;
        _ when (Char >= $a) andalso (Char =< $z) -> safe_char;
        _ when (Char >= $A) andalso (Char =< $Z) -> safe_char_upper;
        _                                        -> bad_char
    end)).

-define(classify_char2(Char),
    (case Char of
        ?SemiColonToken                          -> rest_char;
        ?LsbrktToken                             -> rest_char;
        ?RsbrktToken                             -> rest_char;
        ?LbrktToken                              -> rest_char;
        ?RbrktToken                              -> rest_char;
        ?ColonToken                              -> rest_char;
        ?CommaToken                              -> rest_char;
        ?NequalToken                             -> rest_char;
        ?LesserToken                             -> rest_char;
        ?GreaterToken                            -> rest_char;
        ?EqualToken                              -> rest_char;
        ?SpToken                                 -> white_space;
        ?HtabToken                               -> white_space;
        ?LfToken                                 -> end_of_line;
        ?CrToken                                 -> end_of_line;
        _                                        -> no_skip_char
    end)).

-define(classify_char3(Char),
    (case Char of
        $+                                       -> safe_char;
        $-                                       -> safe_char;
        $&                                       -> safe_char;
        $!                                       -> safe_char;
        $_                                       -> safe_char;
        $/                                       -> safe_char;
        $'                                       -> safe_char;
        $?                                       -> safe_char;
        $@                                       -> safe_char;
        $^                                       -> safe_char;
        $`                                       -> safe_char;
        $~                                       -> safe_char;
        $*                                       -> safe_char;
        $$                                       -> safe_char;
        ?BackslashToken                          -> safe_char;
        ?LparToken                               -> safe_char;
        ?RparToken                               -> safe_char;
        $%                                       -> safe_char;
        ?VbarToken                               -> safe_char;
        $.                                       -> safe_char;
        _ when (Char >= $0) andalso (Char =< $9) -> safe_char;
        _ when (Char >= $a) andalso (Char =< $z) -> safe_char;
        _ when (Char >= $A) andalso (Char =< $Z) -> safe_char_upper;
        _                                        -> non_safe_char
    end)).

%% Only safe_char and safe_char_upper
-define(classify_char4(Char),
    (case Char of
        $+                                       -> safe_char;
        $-                                       -> safe_char;
        $&                                       -> safe_char;
        $!                                       -> safe_char;
        $_                                       -> safe_char;
        $/                                       -> safe_char;
        $'                                       -> safe_char;
        $?                                       -> safe_char;
        $@                                       -> safe_char;
        $^                                       -> safe_char;
        $`                                       -> safe_char;
        $~                                       -> safe_char;
        $*                                       -> safe_char;
        $$                                       -> safe_char;
        ?BackslashToken                          -> safe_char;
        ?LparToken                               -> safe_char;
        ?RparToken                               -> safe_char;
        $%                                       -> safe_char;
        ?VbarToken                               -> safe_char;
        $.                                       -> safe_char;
        _ when (Char >= $0) andalso (Char =< $9) -> safe_char;
        _ when (Char >= $a) andalso (Char =< $z) -> safe_char;
        _ when (Char >= $A) andalso (Char =< $Z) -> safe_char_upper;
        _                                        -> not_safe_char
    end)).

