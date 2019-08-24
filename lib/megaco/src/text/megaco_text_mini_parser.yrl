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
%% Purpose: YECC grammar for mini text decoding of Megaco/H.248
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Annex B TEXT ENCODING OF THE PROTOCOL (NORMATIVE)
%%
%% B.1 Coding of wildcards
%%
%% In a text encoding of the protocol, while TerminationIDs are
%% arbitrary, by judicious choice of names, the wildcard character, "*"
%% may be made more useful.  When the wildcard character is encountered,
%% it will "match" all TerminationIDs having the same previous and
%% following characters (if appropriate).  For example, if there were
%% TerminationIDs of R13/3/1, R13/3/2 and R13/3/3, the TerminationID
%% R13/3/* would match all of them.  There are some circumstances where
%% ALL Terminations must be referred to.  The TerminationID "*" suffices,
%% and is referred to as ALL. The CHOOSE TerminationID "$" may be used to
%% signal to the MG that it has to create an ephemeral Termination or
%% select an idle physical Termination.
%%
%% B.2 ABNF specification
%%
%% The protocol syntax is presented in ABNF according to RFC2234.  The
%% protocol is not case sensitive.  Identifiers are not case sensitive.
%%----------------------------------------------------------------------

Expect 1.


%%----------------------------------------------------------------------
%% Non-terminals
%%----------------------------------------------------------------------

Nonterminals

    authenticationHeader
    daddr
    deviceName
    domainAddress
    domainName
    mId
    megacoMessage
    message
    mtpAddress
    optSep
    pathName
    portNumber
    safeToken
    safeToken2
.

%%----------------------------------------------------------------------
%% Terminals
%%----------------------------------------------------------------------

Terminals

    %% 'AddToken'
    %% 'AndAUDITselectToken'  
    'AuditCapToken'
    'AuditToken'
    'AuditValueToken'
    'AuthToken'
    %% 'BothToken'  
    %% 'BothwayToken'
    'BriefToken'
    %% 'BufferToken'
    'COLON'
    %% 'ContextAttrToken' 
    'ContextAuditToken'
    %% 'ContextListToken' 
    'CtxToken'
    'DelayToken'
    %% 'DigitMapToken'
    %% 'DigitMapDescriptorToken'
    'DirectionToken' 
    'DiscardToken'
    'DisconnectedToken'
    'DurationToken'
    'EQUAL'
    'EmbedToken'
    %% 'EmergencyToken'
    %% 'EmergencyOffToken'
    %% 'EmergencyValueToken' 
    'ErrorToken'
    %% 'EventBufferToken'
    %% 'EventsToken'
    %% 'ExternalToken' 
    'FailoverToken'
    'ForcedToken'
    'GREATER'
    'GracefulToken'
    'H221Token'
    'H223Token'
    'H226Token'
    'HandOffToken'
    %% 'IEPSToken' 
    'ImmAckRequiredToken'
    'InSvcToken'
    'InactiveToken'
    %% 'InternalToken' 
    'InterruptByEventToken'
    'InterruptByNewSignalsDescrToken'
    %% 'IntsigDelayToken' 
    %% 'IsolateToken'
    %% 'IterationToken' 
    'KeepActiveToken'
    'LESSER'
    'LSBRKT'
    'LocalControlToken'
    %% 'LocalDescriptorToken'
    'LockStepToken'
    'LoopbackToken'
    %% 'MediaToken'
    %% 'MessageSegmentToken'
    'MethodToken'
    'MgcIdToken'
    %% 'ModeToken'
    %% 'ModemToken'
    %% 'ModifyToken'
    %% 'MoveToken'
    'MtpAddressToken'
    %% 'MuxToken'
    %% 'NeverNotifyToken' 
    'NotifyCompletionToken'
    %% 'NotifyImmediateToken' 
    %% 'NotifyRegulatedToken' 
    'NotifyToken'
    'Nx64Token' 
    %% 'ObservedEventsToken'
    'OffToken'
    'OnToken'
    'OnOffToken'
    %% 'OnewayToken'
    %% 'OnewayExternalToken' 
    %% 'OnewayBothToken' 
    %% 'OrAUDITselectToken' 
    'OtherReasonToken'
    'OutOfSvcToken'
    %% 'PackagesToken'
    'PendingToken'
    %% 'PriorityToken'
    'ProfileToken'
    %% 'QuotedChars'
    'RSBRKT'
    'ReasonToken'
    'RecvonlyToken'
    %% 'RemoteDescriptorToken'
    'ReplyToken'
    'RequestIDToken' 
    %% 'ReservedGroupToken'
    %% 'ReservedValueToken'
    %% 'ResetEventsDescriptorToken' 
    'ResponseAckToken'
    'RestartToken'
    'SEP'
    'SafeChars'
    'SendonlyToken'
    'SendrecvToken'
    'ServiceChangeAddressToken'
    'ServiceChangeToken'
    %% 'ServiceChangeIncompleteToken'
    %% 'ServiceStatesToken'
    'ServicesToken'
    'SignalListToken'
    'SignalTypeToken'
    %% 'SignalsToken'
    %% 'StatsToken'
    'StreamToken'
    %% 'SubtractToken'
    'SynchISDNToken'
    'TerminationStateToken'
    'TestToken'
    'TimeOutToken'
    %% 'TimeStampToken'
    %% 'TopologyToken'
    'TransToken'
    'V18Token'
    'V22Token'
    'V22bisToken'
    'V32Token'
    'V32bisToken'
    'V34Token'
    'V76Token'
    'V90Token'
    'V91Token'
    'VersionToken'
    endOfMessage

.

%%----------------------------------------------------------------------
%% Root symbol
%%----------------------------------------------------------------------

Rootsymbol megacoMessage.

%%----------------------------------------------------------------------
%% The grammar
%%----------------------------------------------------------------------

%% megacoMessage        = LWSP [authenticationHeader SEP ] message
%% authenticationHeader = AuthToken EQUAL SecurityParmIndex COLON
%%                        SequenceNum COLON AuthData
%%                      
%% SecurityParmIndex    = "0x" 8(HEXDIG)
%% SequenceNum          = "0x" 8(HEXDIG)
%% AuthData             = "0x" 24*64(HEXDIG)
%% message              = MegacopToken SLASH version SEP mId SEP messageBody
%% version              = 1*2(DIGIT) .

megacoMessage        -> optSep authenticationHeader message endOfMessage
                            : #'MegacoMessage'{authHeader = '$2', mess = '$3'} .

optSep               -> 'SEP'    : sep .
optSep               -> '$empty' : no_sep .

authenticationHeader -> 'AuthToken' 'EQUAL' safeToken 'COLON'
                            safeToken 'COLON' safeToken optSep
                            : ensure_auth_header('$3', '$5', '$7') .
authenticationHeader -> '$empty' : asn1_NOVALUE .

message              -> safeToken mId : ensure_message('$1', '$2') .

mId                  -> domainName               : '$1' .
mId                  -> domainAddress            : '$1' .
mId                  -> optSep mtpAddress optSep : '$2' .
mId                  -> optSep deviceName optSep : '$2' .

domainName           -> 'LESSER' safeToken 'GREATER' 'COLON' portNumber optSep
                            : ensure_domainName('$2', '$5') .
domainName           -> 'LESSER' safeToken 'GREATER'
                            : ensure_domainName('$2', asn1_NOVALUE) .

deviceName           -> pathName  : {deviceName, '$1'} .

domainAddress        -> 'LSBRKT' daddr 'RSBRKT' 'COLON' portNumber optSep
                        : ensure_domainAddress('$2', '$5') .
domainAddress        -> 'LSBRKT' daddr 'RSBRKT'
                        : ensure_domainAddress('$2', asn1_NOVALUE) .

daddr -> '$empty'        : [] .
daddr -> 'COLON' daddr   : [colon| '$2'] .
daddr -> safeToken daddr : ['$1'| '$2'] .

portNumber           -> safeToken : ensure_uint16('$1') .

mtpAddress           -> 'MtpAddressToken' : ensure_mtpAddress('$1') .

pathName             -> safeToken : ensure_pathName('$1') .

safeToken            -> safeToken2              : make_safe_token('$1') .

%% safeToken2           -> 'AddToken'              : '$1' .
safeToken2           -> 'AuditToken'            : '$1' .
safeToken2           -> 'AuditCapToken'         : '$1' .
safeToken2           -> 'AuditValueToken'       : '$1' .
safeToken2           -> 'AuthToken'             : '$1' .
%% safeToken2           -> 'BothToken'             : '$1' . % v3
%% safeToken2           -> 'BothwayToken'          : '$1' .
safeToken2           -> 'BriefToken'            : '$1' .
%% safeToken2           -> 'BufferToken'           : '$1' .
safeToken2           -> 'CtxToken'              : '$1' .
%% safeToken2           -> 'ContextAttrToken'      : '$1' . % v3
safeToken2           -> 'ContextAuditToken'     : '$1' .
%% safeToken2           -> 'ContextListToken'      : '$1' . % v3
%% safeToken2           -> 'DigitMapToken'         : '$1' .
%% safeToken2           -> 'DigitMapDescriptorToken' : '$1' .
safeToken2           -> 'DirectionToken'        : '$1' . % v3
safeToken2           -> 'DiscardToken'          : '$1' .
safeToken2           -> 'DisconnectedToken'     : '$1' .
safeToken2           -> 'DelayToken'            : '$1' .
safeToken2           -> 'DurationToken'         : '$1' .
safeToken2           -> 'EmbedToken'            : '$1' .
%% safeToken2           -> 'EmergencyToken'        : '$1' .
%% safeToken2           -> 'EmergencyOffToken'     : '$1' .
safeToken2           -> 'ErrorToken'            : '$1' .
%% safeToken2           -> 'EventBufferToken'      : '$1' .
%% safeToken2           -> 'EventsToken'           : '$1' .
%% safeToken2           -> 'ExternalToken'         : '$1' . % v3
safeToken2           -> 'FailoverToken'         : '$1' .
safeToken2           -> 'ForcedToken'           : '$1' .
safeToken2           -> 'GracefulToken'         : '$1' .
safeToken2           -> 'H221Token'             : '$1' .
safeToken2           -> 'H223Token'             : '$1' .
safeToken2           -> 'H226Token'             : '$1' .
safeToken2           -> 'HandOffToken'          : '$1' .
%% safeToken2           -> 'IEPSToken'             : '$1' . % v3
safeToken2           -> 'ImmAckRequiredToken'   : '$1' .
safeToken2           -> 'InactiveToken'         : '$1' .
%% safeToken2           -> 'InternalToken'         : '$1' . % v3
safeToken2           -> 'InterruptByEventToken' : '$1' .
safeToken2           -> 'InterruptByNewSignalsDescrToken' : '$1' .
%% safeToken2           -> 'IsolateToken'          : '$1' .
safeToken2           -> 'InSvcToken'            : '$1' .
safeToken2           -> 'KeepActiveToken'       : '$1' .
%% safeToken2        -> 'LocalToken'            : '$1' .
%% safeToken2        -> 'LocalDescriptorToken'  : '$1' .
safeToken2           -> 'LocalControlToken'     : '$1' .
safeToken2           -> 'LoopbackToken'         : '$1' .
safeToken2           -> 'LockStepToken'         : '$1' .
%% safeToken2           -> 'MediaToken'            : '$1' .
%% safeToken2           -> 'MegacopToken'          : '$1' .
safeToken2           -> 'MethodToken'           : '$1' .
safeToken2           -> 'MgcIdToken'            : '$1' .
%% safeToken2           -> 'ModeToken'             : '$1' .
%% safeToken2           -> 'ModifyToken'           : '$1' .
%% safeToken2           -> 'ModemToken'            : '$1' .
%% safeToken2           -> 'MoveToken'             : '$1' .
%% safeToken2        -> 'MtpToken'              : '$1' .
%% safeToken2        -> 'MtpAddressToken'       : '$1' .
%% safeToken2           -> 'MuxToken'              : '$1' .
safeToken2           -> 'NotifyToken'           : '$1' .
safeToken2           -> 'NotifyCompletionToken' : '$1' .
safeToken2           -> 'Nx64Token'             : '$1' .
%% safeToken2           -> 'ObservedEventsToken'   : '$1' .
%% safeToken2           -> 'OnewayToken'           : '$1' .
%% safeToken2           -> 'OnewayExternalToken'   : '$1' .
%% safeToken2           -> 'OnewayBothToken'       : '$1' .
safeToken2           -> 'OffToken'              : '$1' .
safeToken2           -> 'OnToken'               : '$1' .
safeToken2           -> 'OnOffToken'            : '$1' .
safeToken2           -> 'OutOfSvcToken'         : '$1' .
safeToken2           -> 'OtherReasonToken'      : '$1' .
%% safeToken2           -> 'PackagesToken'         : '$1' .
safeToken2           -> 'PendingToken'          : '$1' .
%% safeToken2           -> 'PriorityToken'         : '$1' .
safeToken2           -> 'ProfileToken'          : '$1' .
safeToken2           -> 'ReasonToken'           : '$1' .
safeToken2           -> 'RecvonlyToken'         : '$1' .
safeToken2           -> 'ReplyToken'            : '$1' .
safeToken2           -> 'RequestIDToken'        : '$1' . 
safeToken2           -> 'ResponseAckToken'      : '$1' .
safeToken2           -> 'SafeChars'             : '$1' .
safeToken2           -> 'RestartToken'          : '$1' .
%% safeToken2           -> 'RemoteToken'           : '$1' .
%% safeToken2           -> 'RemoteDescriptorToken' : '$1' .
%% safeToken2           -> 'ReservedGroupToken'    : '$1' .
%% safeToken2           -> 'ReservedValueToken'    : '$1' .
safeToken2           -> 'SendonlyToken'         : '$1' .
safeToken2           -> 'SendrecvToken'         : '$1' .
safeToken2           -> 'ServicesToken'         : '$1' .
%% safeToken2           -> 'ServiceStatesToken'    : '$1' .
safeToken2           -> 'ServiceChangeToken'    : '$1' .
%% safeToken2           -> 'ServiceChangeIncompleteToken' : '$1' . 
safeToken2           -> 'ServiceChangeAddressToken' : '$1' .
safeToken2           -> 'SignalListToken'       : '$1' .
%% safeToken2           -> 'SignalsToken'          : '$1' .
safeToken2           -> 'SignalTypeToken'       : '$1' .
%% safeToken2           -> 'StatsToken'            : '$1' .
safeToken2           -> 'StreamToken'           : '$1' .
%% safeToken2           -> 'SubtractToken'         : '$1' .
safeToken2           -> 'SynchISDNToken'        : '$1' .
safeToken2           -> 'TerminationStateToken' : '$1' .
safeToken2           -> 'TestToken'             : '$1' .
safeToken2           -> 'TimeOutToken'          : '$1' .
%% safeToken2           -> 'TopologyToken'         : '$1' .
safeToken2           -> 'TransToken'            : '$1' .
safeToken2           -> 'V18Token'              : '$1' .
safeToken2           -> 'V22Token'              : '$1' .
safeToken2           -> 'V22bisToken'           : '$1' .
safeToken2           -> 'V32Token'              : '$1' .
safeToken2           -> 'V32bisToken'           : '$1' .
safeToken2           -> 'V34Token'              : '$1' .
safeToken2           -> 'V76Token'              : '$1' .
safeToken2           -> 'V90Token'              : '$1' .
safeToken2           -> 'V91Token'              : '$1' .
safeToken2           -> 'VersionToken'          : '$1' .

Erlang code.

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_mini_parser.hrl").

