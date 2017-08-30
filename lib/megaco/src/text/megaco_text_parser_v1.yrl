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
%% Purpose: YECC grammar for text encoding of Megaco/H.248
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

%%----------------------------------------------------------------------
%% Number of expected shift/reduce warnings
%% This is ugly but...
%%----------------------------------------------------------------------

Expect 609.


%%----------------------------------------------------------------------
%% Non-terminals
%%----------------------------------------------------------------------

Nonterminals

    actionReply
    actionReplyBody
    actionReplyList
    actionRequest
    actionRequestItem
    actionRequestItems
    actionRequestList
    alternativeValue
    ammParameter
    ammParameters
    ammRequest
    ammRequestBody
    ammToken
    ammsReply
    ammsReplyBody
    ammsToken
    auditDescriptor
    auditDescriptorBody
    auditItem
    auditItemList
    auditOther
    auditReply
    auditRequest
    auditReturnItem
    auditReturnParameter
    auditReturnParameterList
    authenticationHeader
    commandReply
    commandReplyList
    commandRequest
    contextAudit
    contextAuditProperties
    contextAuditProperty
    contextID
    contextProperty
    contextTerminationAudit 
    daddr
    deviceName
    digitMapDescriptor
    domainAddress
    domainName
    embedFirst
    embedNoSig
    embedSig
    embedWithSig
    errorCode
    errorDescriptor
    errorText
    eventBufferControl
    eventBufferControlState
    eventBufferDescriptor
    eventDM
    eventParameter
    eventParameterName
    eventParameters
    eventSpec
    eventSpecList
    eventStreamOrOther
    eventsDescriptor
    extension
    extensionParameter
    localControlDescriptor
    localParm
    localParmList
    mId
    mediaDescriptor
    mediaParm
    mediaParmList
    megacoMessage
    message
    messageBody
    modemDescriptor
    modemType
    modemTypeList
    mtpAddress
    muxDescriptor
    muxType
    notificationReason
    notificationReasons
    notifyReply
    notifyReplyBody
    notifyRequest
    notifyRequestBody
    observedEvent
    observedEventBody
    observedEventParameter
    observedEventParameters
    observedEventTimeStamp
    observedEvents
    observedEventsDescriptor
    onOrOff
    optAuditDescriptor
    optImmAckRequired
    optPropertyParms
    optSep
    packagesDescriptor
    packagesItem
    packagesItems
    %% parmName
    parmValue
    pathName
    pkgdName
    portNumber
    priority
    propertyParm
    propertyParms
    requestID
    requestedEvent
    requestedEventBody
    requestedEvents
    safeToken
    safeToken2
    secondEventParameter
    secondEventParameters
    secondRequestedEvent
    secondRequestedEventBody
    secondRequestedEvents
    servChgReplyParm
    servChgReplyParms
    serviceChangeAddress
    serviceChangeDelay
    serviceChangeDescriptor
    serviceChangeMethod
    serviceChangeMgcId
    serviceChangeParm
    serviceChangeParms
    serviceChangeProfile 
    serviceChangeReason
    serviceChangeReply
    serviceChangeReplyBody
    serviceChangeReplyDescriptor
    serviceChangeRequest
    serviceChangeVersion
    serviceState
    serviceStates
    sigParameter
    sigParameters
    signalList
    signalListId
    signalListParm
    signalListParms
    signalName
    signalParm
    signalParms
    signalRequest
    signalsDescriptor
    signalType
    statisticsDescriptor
    statisticsParameter
    statisticsParameters
    streamDescriptor
    streamID
    streamModes
    streamParm
    streamParmList
    subtractRequest
    terminationA
    terminationAudit
    terminationB
    terminationID
    terminationIDList
    terminationIDListRepeat
    terminationStateDescriptor
    terminationStateParm
    terminationStateParms
    timeStamp
    topologyDescriptor
    topologyDirection
    topologyTriple
    topologyTripleList
    transactionAck
    transactionAckList
    transactionID
    transactionItem
    transactionList
    transactionPending
    transactionReply
    transactionReplyBody
    transactionRequest
    transactionResponseAck
    value
    valueList

.

%%----------------------------------------------------------------------
%% Terminals
%%----------------------------------------------------------------------

Terminals

    'AddToken'
    'AuditCapToken'
    'AuditToken'
    'AuditValueToken'
    'AuthToken'
    'BothwayToken'
    'BriefToken'
    'BufferToken'
    'COLON'
    'COMMA'
    'ContextAuditToken'
    'CtxToken'
    'DelayToken'
    'DigitMapToken'
    'DigitMapDescriptorToken'
    'DiscardToken'
    'DisconnectedToken'
    'DurationToken'
    'EQUAL'
    'EmbedToken'
    'EmergencyToken'
    'ErrorToken'
    'EventBufferToken'
    'EventsToken'
    'FailoverToken'
    'ForcedToken'
    'GREATER'
    'GracefulToken'
    'H221Token'
    'H223Token'
    'H226Token'
    'HandOffToken'
    'ImmAckRequiredToken'
    'InSvcToken'
    'InactiveToken'
    'IsolateToken'
    'InterruptByEventToken'
    'InterruptByNewSignalsDescrToken'
    'KeepActiveToken'
    'LBRKT'
    'LESSER'
    'LSBRKT'
    'LocalControlToken'
    'LocalDescriptorToken'
    'LockStepToken'
    'LoopbackToken'
    'MediaToken'
    %% 'MegacopToken'
    'MethodToken'
    'MgcIdToken'
    'ModeToken'
    'ModemToken'
    'ModifyToken'
    'MoveToken'
    'MtpAddressToken'
    'MuxToken'
    'NEQUAL'
    'NotifyCompletionToken'
    'NotifyToken'
    'ObservedEventsToken'
    'OffToken'
    'OnToken'
    'OnOffToken'
    'OnewayToken'
    'OtherReasonToken'
    'OutOfSvcToken'
    'PackagesToken'
    'PendingToken'
    'PriorityToken'
    'ProfileToken'
    'QuotedChars'
    'RBRKT'
    'RSBRKT'
    'ReasonToken'
    'RecvonlyToken'
    'RemoteDescriptorToken'
    'ReplyToken'
    'ReservedGroupToken'
    'ReservedValueToken'
    'ResponseAckToken'
    'RestartToken'
    'SEP'
    'SafeChars'
    'SendonlyToken'
    'SendrecvToken'
    'ServiceChangeAddressToken'
    'ServiceChangeToken'
    'ServiceStatesToken'
    'ServicesToken'
    'SignalListToken'
    'SignalTypeToken'
    'SignalsToken'
    'StatsToken'
    'StreamToken'
    'SubtractToken'
    'SynchISDNToken'
    'TerminationStateToken'
    'TestToken'
    'TimeOutToken'
    'TimeStampToken'
    'TopologyToken'
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
    'AndAUDITSelectToken'            %% OTP-7534: v3-fix
    'BothToken'                      %% OTP-7534: v3-fix
    'ContextAttrToken'               %% OTP-7534: v3-fix
    'ContextListToken'               %% OTP-7534: v3-fix
    'DirectionToken'                 %% OTP-7534: v3-fix
    'EmergencyOffToken'              %% OTP-7534: v3-fix
    'EmergencyValueToken'            %% OTP-7534: v3-fix
    'ExternalToken'                  %% OTP-7534: v3-fix
    'IEPSToken'                      %% OTP-7534: v3-fix
    'IntsigDelayToken'               %% OTP-7534: v3-fix
    'InternalToken'                  %% OTP-7534: v3-fix
    'IterationToken'                 %% OTP-7534: v3-fix
    'MessageSegmentToken'            %% OTP-7534: v3-fix
    'NeverNotifyToken'               %% OTP-7534: v3-fix
    'NotifyImmediateToken'           %% OTP-7534: v3-fix
    'NotifyRegulatedToken'           %% OTP-7534: v3-fix
    'Nx64kToken'                     %% OTP-7534: v2-fix
    'OnewayBothToken'                %% OTP-7534: v3-fix
    'OnewayExternalToken'            %% OTP-7534: v3-fix
    'OrAUDITselectToken'             %% OTP-7534: v3-fix
    'RequestIDToken'                 %% OTP-7534: v3-fix
    'ResetEventsDescriptorToken'     %% OTP-7534: v3-fix
    'SegmentationCompleteToken'      %% OTP-7534: v3-fix
    'ServiceChangeIncompleteToken'   %% OTP-7534: v3-fix
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

megacoMessage        -> optSep authenticationHeader message endOfMessage : 
                        #'MegacoMessage'{authHeader = '$2', mess = '$3'} .

optSep               -> 'SEP'    : sep .
optSep               -> '$empty' : no_sep .

authenticationHeader -> 'AuthToken' 'EQUAL' 
                        safeToken 'COLON' safeToken 'COLON' safeToken optSep : 
                        ensure_auth_header('$3', '$5', '$7') .
authenticationHeader -> '$empty' : asn1_NOVALUE .

message              -> safeToken mId messageBody : 
                        ensure_message('$1', '$2', '$3') .

messageBody          -> errorDescriptor : {messageError, '$1'} .
messageBody          -> transactionList : {transactions, '$1'} .

transactionList      -> transactionItem : ['$1'] .
transactionList      -> transactionItem transactionList : ['$1' | '$2'] .

transactionItem      -> transactionRequest      : {transactionRequest,     '$1'} .
transactionItem      -> transactionReply        : {transactionReply,       '$1'}.
transactionItem      -> transactionPending      : {transactionPending,     '$1'} .
transactionItem      -> transactionResponseAck  : {transactionResponseAck, '$1'} .

transactionResponseAck -> 'ResponseAckToken'
                          'LBRKT' transactionAck transactionAckList 'RBRKT' : 
                          ['$3' | '$4'] .

transactionAckList   -> 'COMMA' transactionAck transactionAckList : ['$2' | '$3'] .
transactionAckList   -> '$empty' : [] .

transactionAck       -> safeToken : ensure_transactionAck('$1') .

transactionPending   -> 'PendingToken' 'EQUAL' transactionID 'LBRKT' 'RBRKT' :
                        #'TransactionPending'{transactionId = ensure_transactionID('$3')} .

transactionRequest   -> 'TransToken' 
                        'LBRKT'  actionRequest actionRequestList 'RBRKT' : 
                        #'TransactionRequest'{transactionId = asn1_NOVALUE,
                                              actions = ['$3' | '$4']} .
transactionRequest   -> 'TransToken' 'EQUAL' 
                        'LBRKT'  actionRequest actionRequestList 'RBRKT' : 
                        #'TransactionRequest'{transactionId = asn1_NOVALUE,
                                              actions = ['$4' | '$5']} .
transactionRequest   -> 'TransToken' 'EQUAL' transactionID
                        'LBRKT'  actionRequest actionRequestList 'RBRKT' : 
                        #'TransactionRequest'{transactionId = ensure_transactionID('$3'),
                                              actions = ['$5' | '$6']} .

actionRequestList    -> 'COMMA' actionRequest actionRequestList : 
                        ['$2' | '$3'] .
actionRequestList    -> '$empty' : [] .

actionRequest        -> 'CtxToken' 'EQUAL' contextID
                        'LBRKT' actionRequestItem actionRequestItems 'RBRKT' :
                        merge_action_requests('$3', ['$5' | '$6']) .

actionRequestItems   -> 'COMMA' actionRequestItem actionRequestItems : ['$2' | '$3'] .
actionRequestItems   -> '$empty' : [] .

actionRequestItem    -> contextProperty : '$1'.
actionRequestItem    -> contextAudit    : '$1' .
actionRequestItem    -> commandRequest  : '$1'.

%% at-most-once
contextProperty      -> topologyDescriptor  : {topology, '$1'}.
contextProperty      -> priority            : {priority, '$1'}. 
contextProperty      -> 'EmergencyToken'    : {emergency, true}.

contextAudit         -> 'ContextAuditToken'
                        'LBRKT' contextAuditProperty 
                                contextAuditProperties 'RBRKT' : 
                        ['$3' | '$4'] .

contextAuditProperties -> 'COMMA' contextAuditProperty 
                                  contextAuditProperties  : 
                          ['$2' | '$3'] .
contextAuditProperties -> '$empty' : [] .

%% at-most-once .
contextAuditProperty -> 'TopologyToken'  : topologyAudit .
contextAuditProperty -> 'EmergencyToken' : emergencyAudit .
contextAuditProperty -> 'PriorityToken'  : priorityAudit .
                     
commandRequest       -> ammRequest             : '$1'.
commandRequest       -> subtractRequest        : '$1'.
commandRequest       -> auditRequest           : '$1'.
commandRequest       -> notifyRequest          : '$1'.
commandRequest       -> serviceChangeRequest   : '$1'.

transactionReply     -> 'ReplyToken' 'EQUAL' transactionID 
			'LBRKT' optImmAckRequired 
                                transactionReplyBody 'RBRKT' : 
                        #'TransactionReply'{transactionId     = '$3',
				            immAckRequired    = '$5',
					    transactionResult = '$6'} .

optImmAckRequired    -> 'ImmAckRequiredToken' 'COMMA' : 'NULL' .
optImmAckRequired    -> '$empty' : asn1_NOVALUE .
     
transactionReplyBody -> errorDescriptor : 
                        {transactionError, '$1'} .
transactionReplyBody -> actionReply actionReplyList  : 
                        {actionReplies, ['$1' | '$2']} .

actionReplyList      -> 'COMMA' actionReply actionReplyList  : ['$2' | '$3'] .
actionReplyList      -> '$empty' : [] .

actionReply          -> 'CtxToken' 'EQUAL' contextID 
                        'LBRKT' actionReplyBody 'RBRKT' : 
                        setelement(#'ActionReply'.contextId, '$5', '$3') .

actionReplyBody -> errorDescriptor :  
                   #'ActionReply'{errorDescriptor = '$1'} .
actionReplyBody -> commandReply commandReplyList : 
                   merge_action_reply(['$1' | '$2']) .

commandReply         -> serviceChangeReply  : {command, '$1'} .
commandReply         -> auditReply          : {command, '$1'} .
commandReply         -> ammsReply           : {command, '$1'} .
commandReply         -> notifyReply         : {command, '$1'} .
commandReply         -> contextProperty     : {context, '$1'} . 

%% OTP-5085
%% This ugly thing is to fool the parser. The errorDescriptor does not
%% realy belong here. The merge_action_reply will remove it and put it
%% in it's right place later.
commandReplyList     -> 'COMMA' errorDescriptor : 
                        ['$2'] .
commandReplyList     -> 'COMMA' commandReply commandReplyList  : 
                        ['$2' | '$3'] .
commandReplyList     -> '$empty' : [] .

%Add Move and Modify have the same request parameter
ammRequest           -> ammToken 'EQUAL' terminationID ammRequestBody : 
                        make_commandRequest('$1',
				            #'AmmRequest'{terminationID = ['$3'],
							  descriptors   = '$4'}) .

ammToken             -> 'AddToken'     : {addReq,  '$1'} .
ammToken             -> 'MoveToken'    : {moveReq, '$1'} .
ammToken             -> 'ModifyToken'  : {modReq,  '$1'} .

ammRequestBody       -> 'LBRKT' ammParameter ammParameters 'RBRKT'  : ['$2' | '$3'] .
ammRequestBody       -> '$empty' : [] .

ammParameters        -> 'COMMA' ammParameter ammParameters  :  ['$2' | '$3'] .
ammParameters        -> '$empty' : [] .

%at-most-once
ammParameter         -> mediaDescriptor        : {mediaDescriptor,       '$1'}.
ammParameter         -> modemDescriptor        : {modemDescriptor,       '$1'}.
ammParameter         -> muxDescriptor          : {muxDescriptor,         '$1'}.
ammParameter         -> eventsDescriptor       : {eventsDescriptor,      '$1'}.
ammParameter         -> eventBufferDescriptor  : {eventBufferDescriptor, '$1'}.
ammParameter         -> signalsDescriptor      : {signalsDescriptor,     '$1'}.
ammParameter         -> digitMapDescriptor     : {digitMapDescriptor,    '$1'}.
ammParameter         -> auditDescriptor        : {auditDescriptor,       '$1'}.

ammsReply            -> ammsToken 'EQUAL' terminationID ammsReplyBody :  
                        {'$1', #'AmmsReply'{terminationID    = ['$3'],
				            terminationAudit = '$4'}} .

ammsToken            -> 'AddToken'       : addReply .
ammsToken            -> 'MoveToken'      : moveReply .
ammsToken            -> 'ModifyToken'    : modReply .
ammsToken            -> 'SubtractToken'  : subtractReply .

ammsReplyBody        -> 'LBRKT' terminationAudit 'RBRKT' : '$2' .
ammsReplyBody        -> '$empty' : asn1_NOVALUE .

subtractRequest      -> 'SubtractToken' 'EQUAL' 
                        terminationID optAuditDescriptor :
                        make_commandRequest({subtractReq, '$1'},
                                            #'SubtractRequest'{terminationID = ['$3'],
                                                               auditDescriptor = '$4'}) .
  

optAuditDescriptor   -> 'LBRKT' auditDescriptor 'RBRKT'  : '$2'.
optAuditDescriptor   -> '$empty'                         : asn1_NOVALUE .

auditRequest -> 'AuditValueToken' 'EQUAL' 
                terminationID optAuditDescriptor : 
                make_commandRequest({auditValueRequest, '$1'},
                                    #'AuditRequest'{terminationID   = '$3',
                                                    auditDescriptor = '$4'}) .
auditRequest -> 'AuditCapToken' 'EQUAL' 
                terminationID optAuditDescriptor : 
                make_commandRequest({auditCapRequest, '$1'},
                                    #'AuditRequest'{terminationID   = '$3',
                                                    auditDescriptor = '$4'}) .

auditReply -> 'AuditValueToken' 'EQUAL' 'CtxToken' contextTerminationAudit
		  : {auditValueReply, '$4'} .
auditReply -> 'AuditCapToken'   'EQUAL' 'CtxToken' contextTerminationAudit
		  : {auditCapReply,   '$4'} .
auditReply -> 'AuditValueToken' 'EQUAL' auditOther
		  : {auditValueReply, '$3'} .
auditReply -> 'AuditCapToken'   'EQUAL' auditOther
		  : {auditCapReply,   '$3'} .

contextTerminationAudit -> terminationIDList               : {contextAuditResult, '$1'} .
contextTerminationAudit -> 'LBRKT' errorDescriptor 'RBRKT' : {contextAuditResult, '$2'} .

auditOther              -> terminationID : 
                           {auditResult, 
                            #'AuditResult'{terminationID          = '$1',
					   terminationAuditResult = []}} .
auditOther              -> terminationID 'LBRKT' terminationAudit 'RBRKT' : 
                           {auditResult, 
                            #'AuditResult'{terminationID          = '$1',
					   terminationAuditResult = '$3'}} .

terminationAudit     -> auditReturnParameter auditReturnParameterList : 
                        merge_terminationAudit(['$1' |'$2' ]) .

auditReturnParameterList -> 'COMMA' auditReturnParameter auditReturnParameterList : ['$2' | '$3'] .
auditReturnParameterList -> '$empty' : [] .

auditReturnParameter -> mediaDescriptor           : {mediaDescriptor, '$1'} .
auditReturnParameter -> modemDescriptor           : {modemDescriptor, '$1'} .
auditReturnParameter -> muxDescriptor             : {muxDescriptor, '$1'} .
auditReturnParameter -> eventsDescriptor          : {eventsDescriptor, '$1'} .
auditReturnParameter -> signalsDescriptor         : {signalsDescriptor, '$1'} .
auditReturnParameter -> digitMapDescriptor        : {digitMapDescriptor, '$1'} .
auditReturnParameter -> observedEventsDescriptor  : {observedEventsDescriptor, '$1'} .
auditReturnParameter -> eventBufferDescriptor     : {eventBufferDescriptor, '$1'} .
auditReturnParameter -> statisticsDescriptor      : {statisticsDescriptor, '$1'} .
auditReturnParameter -> packagesDescriptor        : {packagesDescriptor, '$1'} .
auditReturnParameter -> errorDescriptor           : {errorDescriptor, '$1'} .
auditReturnParameter -> auditReturnItem           : {auditReturnItem, '$1'} .  %% IGv11

auditDescriptor      -> 'AuditToken' 'LBRKT' auditDescriptorBody 'RBRKT'
			    : #'AuditDescriptor'{auditToken = '$3'} .

auditDescriptorBody  -> auditItem auditItemList : ['$1' | '$2'].
auditDescriptorBody  -> '$empty'                : asn1_NOVALUE .

auditItemList        -> 'COMMA' auditItem auditItemList : ['$2' | '$3'] .
auditItemList        -> '$empty'                        : [] .
 
%% IGv11 - begin
%% 
auditReturnItem      -> 'MuxToken'            : muxToken .
auditReturnItem      -> 'ModemToken'          : modemToken .
auditReturnItem      -> 'MediaToken'          : mediaToken .
auditReturnItem      -> 'DigitMapToken'       : digitMapToken .
auditReturnItem      -> 'StatsToken'          : statsToken .
auditReturnItem      -> 'ObservedEventsToken' : observedEventsToken .
auditReturnItem      -> 'PackagesToken'       : packagesToken .

%% at-most-once, and DigitMapToken and PackagesToken are not allowed 
%% in AuditCapabilities command 
%% 
auditItem            -> auditReturnItem       : '$1' .
auditItem            -> 'SignalsToken'        : signalsToken .
auditItem            -> 'EventBufferToken'    : eventBufferToken .
auditItem            -> 'EventsToken'         : eventsToken .
%% 
%% IGv11 - end

notifyRequest        -> 'NotifyToken' 'EQUAL' terminationID
                        'LBRKT' notifyRequestBody 'RBRKT'
                      : make_commandRequest({notifyReq, '$1'},
					    setelement(#'NotifyRequest'.terminationID, '$5', ['$3'])) .

notifyRequestBody    -> observedEventsDescriptor  
                            : #'NotifyRequest'{observedEventsDescriptor = '$1'}.
notifyRequestBody    -> errorDescriptor
                            : #'NotifyRequest'{errorDescriptor = '$1'}.

notifyReply          -> 'NotifyToken' 'EQUAL' terminationID notifyReplyBody
			    : {notifyReply,
			       #'NotifyReply'{terminationID = ['$3'],
					      errorDescriptor = '$4'}} .

notifyReplyBody      -> 'LBRKT' errorDescriptor 'RBRKT' : '$2'.
notifyReplyBody      -> '$empty' : asn1_NOVALUE .

serviceChangeRequest -> 'ServiceChangeToken' 'EQUAL' terminationID
                        'LBRKT' serviceChangeDescriptor 'RBRKT'
                      : make_commandRequest({serviceChangeReq, '$1'},
					    #'ServiceChangeRequest'{terminationID = ['$3'],
								    serviceChangeParms = '$5'}) .

serviceChangeReply   -> 'ServiceChangeToken' 'EQUAL' terminationID serviceChangeReplyBody
			    : {serviceChangeReply,
			       #'ServiceChangeReply'{terminationID = ['$3'],
						     serviceChangeResult = '$4'}} .

serviceChangeReplyBody -> 'LBRKT' errorDescriptor 'RBRKT'
			      : {errorDescriptor, '$2'} .
serviceChangeReplyBody -> 'LBRKT' serviceChangeReplyDescriptor 'RBRKT'
			      : {serviceChangeResParms, '$2'} .
serviceChangeReplyBody -> '$empty' : {serviceChangeResParms, #'ServiceChangeResParm'{}}.

errorDescriptor      -> 'ErrorToken' 'EQUAL' errorCode 'LBRKT' errorText 'RBRKT'
                            : #'ErrorDescriptor'{errorCode = '$3',
                                                 errorText = '$5'} .

errorCode            -> safeToken : ensure_uint('$1', 0, 999) .

errorText            -> 'QuotedChars' : value_of('$1') .
errorText            -> '$empty'      : asn1_NOVALUE .

transactionID        -> safeToken : ensure_uint32('$1') .

mId                  -> domainName               : '$1' .
mId                  -> domainAddress            : '$1' .
mId                  -> optSep mtpAddress optSep : '$2' .
mId                  -> optSep deviceName optSep : '$2' .

domainName           -> 'LESSER' safeToken 'GREATER' 'COLON' portNumber optSep
                            : ensure_domainName('$2', '$5') .
domainName           -> 'LESSER' safeToken 'GREATER'
                            : ensure_domainName('$2', asn1_NOVALUE) .

deviceName           -> pathName  : {deviceName, '$1'} .

contextID            -> safeToken : ensure_contextID('$1') .

domainAddress        -> 'LSBRKT' daddr 'RSBRKT' 'COLON' portNumber optSep
                        : ensure_domainAddress('$2', '$5') .
domainAddress        -> 'LSBRKT' daddr 'RSBRKT'
                        : ensure_domainAddress('$2', asn1_NOVALUE) .

daddr -> '$empty'        : [] .
daddr -> 'COLON' daddr   : [colon| '$2'] .
daddr -> safeToken daddr : ['$1'| '$2'] .


portNumber           -> safeToken : ensure_uint16('$1') .

mtpAddress           -> 'MtpAddressToken' : ensure_mtpAddress('$1') .

%% terminationIDList    -> LBRKT terminationID *(COMMA terminationID) RBRKT .

terminationIDList    -> 'LBRKT' terminationID terminationIDListRepeat 'RBRKT'  
                                 : ['$2' | '$3'] .

terminationIDListRepeat -> 'COMMA' terminationID terminationIDListRepeat
                                 : ['$2'| '$3'] .
terminationIDListRepeat -> '$empty' : [] .


pathName             -> safeToken : ensure_pathName('$1') . 

terminationID        -> safeToken : ensure_terminationID('$1') .

mediaDescriptor      -> 'MediaToken' 'LBRKT' mediaParm mediaParmList 'RBRKT'
			    : merge_mediaDescriptor(['$3' | '$4']) .

mediaParmList        -> 'COMMA' mediaParm mediaParmList : ['$2' | '$3'] .
mediaParmList        -> '$empty' : [] .


%% at-most-once per item
%% using either streamParms or streamDescriptors but not both
mediaParm            -> streamParm
			    : {streamParm, '$1'} .
mediaParm            -> streamDescriptor
			    : {streamDescriptor, '$1'} .
mediaParm            -> terminationStateDescriptor
			    : {termState, '$1'} .

%% at-most-onc .
%% Specially treated by the scanner.
streamParm           -> 'LocalDescriptorToken' : 
                        PGs = ensure_prop_groups('$1'), 
                        {local, #'LocalRemoteDescriptor'{propGrps = PGs} } .
streamParm           -> 'RemoteDescriptorToken' : 
                        PGs = ensure_prop_groups('$1'), 
		        {remote, #'LocalRemoteDescriptor'{propGrps = PGs}} .
streamParm           -> localControlDescriptor  : {control, '$1'} .

streamDescriptor     -> 'StreamToken' 'EQUAL' streamID
                        'LBRKT' streamParm streamParmList 'RBRKT'
		      : #'StreamDescriptor'{streamID    = '$3',
					    streamParms = merge_streamParms(['$5' | '$6'])} .

streamParmList       -> 'COMMA' streamParm streamParmList : ['$2' | '$3'] .
streamParmList       -> '$empty' : [] .

localControlDescriptor -> 'LocalControlToken' 'LBRKT' localParm localParmList 'RBRKT'
                        : ['$3' | '$4'] .

localParmList        -> 'COMMA' localParm localParmList : ['$2' | '$3'] .
localParmList        -> '$empty': [] .

terminationStateDescriptor -> 'TerminationStateToken'
				  'LBRKT' terminationStateParm terminationStateParms 'RBRKT'
		            : merge_terminationStateDescriptor(['$3' | '$4']) .

terminationStateParms -> 'COMMA' terminationStateParm terminationStateParms : ['$2' | '$3'] .
terminationStateParms -> '$empty' : [] .

%% at-most-once per item except for propertyParm
localParm            -> 'ReservedGroupToken' 'EQUAL' onOrOff : {group, '$3'} .
localParm            -> 'ReservedValueToken' 'EQUAL' onOrOff : {value, '$3'} .
localParm            -> 'ModeToken' 'EQUAL' streamModes      : {mode,  '$3'} .
localParm            -> propertyParm                         : {prop,  '$1'} .
   
onOrOff              -> 'OnToken'  : true .
onOrOff              -> 'OffToken' : false .
   
%% at-most-once    
streamModes          -> 'SendonlyToken' : sendOnly .
streamModes          -> 'RecvonlyToken' : recvOnly .
streamModes          -> 'SendrecvToken' : sendRecv .
streamModes          -> 'InactiveToken' : inactive .
streamModes          -> 'LoopbackToken' : loopBack .

propertyParm         -> pkgdName parmValue : setelement(#'PropertyParm'.name, '$2', '$1') .

parmValue            -> 'EQUAL' alternativeValue : '$2' .

parmValue            -> 'NEQUAL'  value
                            : #'PropertyParm'{value = ['$2'], extraInfo = {relation, unequalTo}} .
parmValue            -> 'LESSER'  value 
                            : #'PropertyParm'{value = ['$2'], extraInfo = {relation, smallerThan}} .
parmValue            -> 'GREATER' value
                            : #'PropertyParm'{value = ['$2'], extraInfo = {relation, greaterThan}} .

%% OTP-4013
%% alternativeValue  = ( VALUE / 
%%                       LSBRKT VALUE *(COMMA VALUE) RSBRKT  /
%%                       LSBRKT VALUE COLON VALUE RSBRKT ) /
%%                       LBRKT VALUE *(COMMA VALUE) RBRKT
alternativeValue     -> 'LBRKT' value valueList 'RBRKT'
                            : #'PropertyParm'{value     = ['$2' | '$3'],
					      extraInfo = {sublist, false}}. % OR

alternativeValue     -> 'LSBRKT' value 'COLON' value 'RSBRKT'
                            : #'PropertyParm'{value     = ['$2', '$4'],
                                              extraInfo = {range, true}}.

alternativeValue     -> 'LSBRKT' value valueList 'RSBRKT'
                            : #'PropertyParm'{value     = ['$2' | '$3'],
					      extraInfo = {sublist, true}}. % AND

alternativeValue     -> value : #'PropertyParm'{value = ['$1']} .

valueList            -> 'COMMA' value valueList : ['$2' | '$3'] .
valueList            -> '$empty' : [] .


eventBufferDescriptor -> 'EventBufferToken' : [] .
eventBufferDescriptor -> 'EventBufferToken' 'LBRKT' eventSpec eventSpecList 'RBRKT'
			 : ['$3' | '$4'] .

eventSpecList        -> 'COMMA' eventSpec eventSpecList : ['$2' | '$3'] .
eventSpecList        -> '$empty' : [] .

eventSpec            -> observedEvent : merge_eventSpec('$1') .

%% at-most-once per item except for propertyParm
terminationStateParm -> serviceStates      : {serviceState, '$1'} .
terminationStateParm -> eventBufferControl : {eventBufferControl, '$1'} .
terminationStateParm -> propertyParm       : {propertyParm, '$1'} .

serviceStates        -> 'ServiceStatesToken' 'EQUAL' serviceState : '$3' .

serviceState         -> 'TestToken'     : test . 
serviceState         -> 'OutOfSvcToken' : outOfSvc .
serviceState         -> 'InSvcToken'    : inSvc .

eventBufferControl   -> 'BufferToken' 'EQUAL' eventBufferControlState : '$3' .

eventBufferControlState -> 'OffToken'      : off .
eventBufferControlState -> 'LockStepToken' : lockStep .

muxDescriptor        -> 'MuxToken' 'EQUAL' muxType  terminationIDList
			    : #'MuxDescriptor'{muxType  = '$3',
					       termList = '$4'} .

muxType              -> safeToken : ensure_muxType('$1') .

streamID             -> safeToken : ensure_streamID('$1') .

pkgdName             -> safeToken : ensure_pkgdName('$1') .

eventsDescriptor     -> 'EventsToken' : 
                        #'EventsDescriptor'{requestID = asn1_NOVALUE,
                                            eventList = []} .
eventsDescriptor     -> 'EventsToken' 'EQUAL' requestID
                        'LBRKT' requestedEvent requestedEvents 'RBRKT' : 
                        #'EventsDescriptor'{requestID = '$3',
					    eventList = ['$5' | '$6']} .

requestedEvents      -> 'COMMA' requestedEvent requestedEvents : ['$2' | '$3']  .
requestedEvents      -> '$empty' : [] .

requestedEvent       -> pkgdName requestedEventBody : 
                        setelement(#'RequestedEvent'.pkgdName, '$2', '$1') .

requestedEventBody   -> 'LBRKT' eventParameter eventParameters 'RBRKT' : 
                        merge_eventParameters(['$2' | '$3']) .
requestedEventBody   -> '$empty' : 
                        #'RequestedEvent'{evParList = []} .

eventParameters      -> 'COMMA' eventParameter eventParameters : 
                        ['$2' | '$3'] .
eventParameters      -> '$empty' : 
                        [] .

%% at-most-once each of embedOrKeepActive , eventDM or eventStream
eventParameter       -> 'KeepActiveToken'   : keepActive .
eventParameter       -> embedWithSig        : '$1'.
eventParameter       -> embedNoSig          : '$1'.
eventParameter       -> eventDM             : '$1'.
eventParameter       -> eventStreamOrOther  : '$1'.

embedWithSig         -> 'EmbedToken' 'LBRKT' signalsDescriptor 
			    'COMMA' embedFirst 'RBRKT' : 
                         {embed, '$3', '$5'} .
embedWithSig         -> 'EmbedToken' 'LBRKT' signalsDescriptor 'RBRKT' :
			 {embed, '$3', asn1_NOVALUE} .

embedNoSig           -> 'EmbedToken' 'LBRKT' embedFirst 'RBRKT' : 
                        {embed, asn1_NOVALUE, '$3'} .
    
embedFirst           -> 'EventsToken' : 
                        #'SecondEventsDescriptor'{requestID = asn1_NOVALUE,
						  eventList = []} .
embedFirst           -> 'EventsToken' 'EQUAL' requestID
                        'LBRKT' secondRequestedEvent 
                                secondRequestedEvents 'RBRKT' :
		        #'SecondEventsDescriptor'{requestID = '$3',
						  eventList = ['$5' | '$6']} .

secondRequestedEvents -> 'COMMA' secondRequestedEvent secondRequestedEvents : 
                         ['$2' | '$3'] .
secondRequestedEvents -> '$empty' : [] .

%% at-most-once of each
secondRequestedEvent  -> pkgdName secondRequestedEventBody : 
                         setelement(#'SecondRequestedEvent'.pkgdName, 
                                    '$2', '$1') .

secondRequestedEventBody -> 'LBRKT' secondEventParameter 
                                    secondEventParameters 'RBRKT' : 
                            merge_secondEventParameters(['$2' | '$3']) .
secondRequestedEventBody -> '$empty' :  
                            #'SecondRequestedEvent'{evParList = []} .

secondEventParameters -> 'COMMA' secondEventParameter secondEventParameters : 
                         ['$2' | '$3'] .
secondEventParameters -> '$empty' : [] .

%% at-most-once each of embedOrKeepActive , eventDM or eventStream
secondEventParameter -> 'KeepActiveToken'    : keepActive .
secondEventParameter -> embedSig             : '$1' .
secondEventParameter -> eventDM              : '$1' .
secondEventParameter -> eventStreamOrOther   : '$1' .

embedSig             -> 'EmbedToken' 'LBRKT' signalsDescriptor 'RBRKT' : 
			{second_embed, '$3'} .

eventStreamOrOther   -> eventParameterName parmValue : 
                        select_stream_or_other('$1', '$2') .

eventParameterName   -> safeToken : 
                        ensure_NAME('$1') .

%% The DigitMapDescriptorToken is specially treated by the scanner
eventDM              -> 'DigitMapDescriptorToken' : ensure_eventDM('$1') .

%% H248S-IG (IGv11)
signalsDescriptor    -> 'SignalsToken' 'LBRKT' signalParm signalParms 'RBRKT' :
                        ['$3' | '$4'] .
signalsDescriptor    -> 'SignalsToken' : 
                        [] .

signalParms          -> 'COMMA' signalParm signalParms : [ '$2' | '$3'] .
signalParms          -> '$empty' : [] .

signalParm           -> signalList    : {seqSigList, '$1'} .
signalParm           -> signalRequest : {signal, '$1'} .

signalRequest        -> signalName 'LBRKT' sigParameter sigParameters 'RBRKT'
			    : merge_signalRequest('$1', ['$3' | '$4']).
signalRequest        -> signalName : merge_signalRequest('$1', []).

sigParameters        -> 'COMMA' sigParameter sigParameters : ['$2' | '$3'] .
sigParameters        -> '$empty' : [] .

%%    sigParameter    = sigStream / sigSignalType / sigDuration / sigOther 
%% 		      / notifyCompletion / KeepActiveToken 
%%    sigStream            = StreamToken EQUAL StreamID 
%%    sigOther             = sigParameterName parmValue 
%%    sigParameterName     = NAME 
%%    sigSignalType        = SignalTypeToken EQUAL signalType 
%%    signalType           = (OnOffToken / TimeOutToken / BriefToken) 
%%    sigDuration          = DurationToken EQUAL UINT16 
%%    notifyCompletion     = NotifyCompletionToken EQUAL (LBRKT 
%% 		      notificationReason *(COMMA notificationReason) RBRKT 
%%     
%%    notificationReason   = ( TimeOutToken / InterruptByEventToken 
%% 				   / InterruptByNewSignalsDescrToken 
%% 				   / OtherReasonToken ) 

sigParameter         -> 'StreamToken' 'EQUAL' streamID : {stream, '$3'}.
sigParameter         -> 'SignalTypeToken' 'EQUAL' signalType : {signal_type, '$3'} .
sigParameter         -> 'DurationToken' 'EQUAL' safeToken : {duration, ensure_uint16('$3')} .
sigParameter         -> safeToken parmValue : {other, ensure_NAME('$1'), '$2'}.
sigParameter         -> 'NotifyCompletionToken' 'EQUAL'
			    'LBRKT' notificationReason notificationReasons 'RBRKT'
			    : {notify_completion, ['$4' | '$5']} .
sigParameter         -> 'KeepActiveToken' : keepActive .

signalType           -> 'OnOffToken'   : onOff.
signalType           -> 'TimeOutToken' : timeOut.
signalType           -> 'BriefToken'   : brief.

notificationReasons  -> 'COMMA' notificationReason notificationReasons : ['$2' | '$3'] .
notificationReasons  -> '$empty' : [] .

notificationReason   -> 'TimeOutToken' : onTimeOut .
notificationReason   -> 'InterruptByEventToken' : onInterruptByEvent .
notificationReason   -> 'InterruptByNewSignalsDescrToken' : onInterruptByNewSignalDescr .
notificationReason   -> 'OtherReasonToken' : otherReason .

signalList           -> 'SignalListToken' 'EQUAL' signalListId
                        'LBRKT' signalListParm signalListParms 'RBRKT'
			    : #'SeqSigList'{id = ensure_uint16('$3'),
					    signalList = ['$5' | '$6']} .

signalListParms      -> 'COMMA' signalListParm signalListParms : ['$2' | '$3'] .
signalListParms      -> '$empty' : [] .

signalListId         -> safeToken : ensure_uint16('$1') .

%% exactly once signalType,
%% at most once duration and every signal parameter
signalListParm       -> signalRequest  : '$1'.

signalName           -> pkgdName  : '$1'.

observedEventsDescriptor -> 'ObservedEventsToken' 'EQUAL' requestID
                            'LBRKT' observedEvent observedEvents 'RBRKT'
                      : #'ObservedEventsDescriptor'{requestId        = '$3',
						    observedEventLst = ['$5' | '$6']} .

observedEvents       -> 'COMMA' observedEvent observedEvents : ['$2' | '$3'] .
observedEvents       -> '$empty' : [] .

%%time per event, because it might be buffered
observedEvent        -> observedEventTimeStamp optSep pkgdName observedEventBody : 
                        merge_observed_event('$4', '$3', '$1') .
observedEvent        -> pkgdName observedEventBody : 
                        merge_observed_event('$2', '$1', asn1_NOVALUE) .

observedEventTimeStamp -> timeStamp optSep 'COLON' : '$1' .
observedEventTimeStamp -> '$empty' : asn1_NOVALUE .

observedEventBody    -> 'LBRKT' observedEventParameter observedEventParameters 'RBRKT'
			    : ['$2' | '$3'] .
observedEventBody    -> '$empty' : [] .

observedEventParameters -> 'COMMA' observedEventParameter observedEventParameters : ['$2' | '$3'] .
observedEventParameters -> '$empty' : [] .

%%at-most-once eventStream, every eventParameterName at most once
observedEventParameter -> eventStreamOrOther : '$1' .

requestID            -> safeToken : ensure_requestID('$1') .

modemDescriptor      -> 'ModemToken' 'EQUAL' modemType optPropertyParms :
			#'ModemDescriptor'{mtl = ['$3'],
                                           mpl = '$4'} .

%% at-most-once of each modem type exept for extensionParameter
modemDescriptor      -> 'ModemToken' 'LSBRKT' modemType modemTypeList 'RSBRKT' 
                        optPropertyParms :
			#'ModemDescriptor'{mtl = ['$3'  | '$4'],
                                           mpl = '$6'} .

modemTypeList 	     -> 'COMMA' modemType modemTypeList : ['$2' | '$3'] .
modemTypeList 	     -> '$empty' : [] .
       
optPropertyParms     -> 'LBRKT' propertyParm propertyParms 'RBRKT' : ['$2' | '$3'] .
optPropertyParms     -> '$empty' : [] .
       
propertyParms  	     -> 'COMMA' propertyParm propertyParms :  ['$2' | '$3'] .
propertyParms 	     -> '$empty' : [] .

% parmName             -> safeToken : ensure_NAME('$1') .

modemType            -> safeToken : ensure_modemType('$1').

%% The DigitMapDescriptorToken is specially treated by the scanner
digitMapDescriptor   -> 'DigitMapDescriptorToken' : ensure_DMD('$1') .

%% ; at most of either serviceChangeAddress or serviceChangeMgcId but not both 
serviceChangeDescriptor -> 'ServicesToken' 
                           'LBRKT' serviceChangeParm 
                                   serviceChangeParms 'RBRKT' :
                            merge_ServiceChangeParm(['$3' | '$4']) .

serviceChangeParms   -> 'COMMA' serviceChangeParm serviceChangeParms : 
                        ['$2' | '$3'] .
serviceChangeParms   -> '$empty' : [] .

serviceChangeParm    -> serviceChangeMethod  : {method,    '$1'} .
serviceChangeParm    -> serviceChangeReason  : {reason,    '$1'} .
serviceChangeParm    -> serviceChangeDelay   : {delay,     '$1'} .
serviceChangeParm    -> serviceChangeAddress : {address,   '$1'} .
serviceChangeParm    -> serviceChangeProfile : {profile,   '$1'} .
serviceChangeParm    -> extension            : {extension, '$1'} .
serviceChangeParm    -> timeStamp            : {time_stamp,'$1'} .
serviceChangeParm    -> serviceChangeMgcId   : {mgc_id,    '$1'} .
serviceChangeParm    -> serviceChangeVersion : {version,   '$1'} .

serviceChangeMethod  -> 'MethodToken' 'EQUAL' safeToken : ensure_serviceChangeMethod('$3') .    

serviceChangeReason  -> 'ReasonToken' 'EQUAL' value : ['$3'] .

serviceChangeDelay   -> 'DelayToken'  'EQUAL' safeToken : ensure_uint32('$3').

serviceChangeAddress -> 'ServiceChangeAddressToken' 'EQUAL' mId : '$3' .
serviceChangeAddress -> 'ServiceChangeAddressToken' 'EQUAL' portNumber : {portNumber, '$3'} .

serviceChangeMgcId   -> 'MgcIdToken'   'EQUAL' mId       : '$3' .

serviceChangeProfile -> 'ProfileToken' 'EQUAL' safeToken : ensure_profile('$3').

serviceChangeVersion -> 'VersionToken' 'EQUAL' safeToken : ensure_version('$3') .

extension            ->  extensionParameter parmValue
			     : setelement(#'PropertyParm'.name, '$2', '$1') .

%% at most once. Version is REQUIRED on first ServiceChange response
%% at most of either serviceChangeAddress or serviceChangeMgcId but not both 
serviceChangeReplyDescriptor -> 'ServicesToken'
                                'LBRKT' servChgReplyParm 
                                        servChgReplyParms 'RBRKT' :
                                merge_ServiceChangeResParm(['$3' | '$4']) .

servChgReplyParms    -> 'COMMA' servChgReplyParm servChgReplyParms : 
                        ['$2' | '$3'] .
servChgReplyParms    -> '$empty' :  [] .

servChgReplyParm     -> serviceChangeAddress : {address,   '$1'} .
servChgReplyParm     -> serviceChangeMgcId   : {mgc_id,    '$1'}.
servChgReplyParm     -> serviceChangeProfile : {profile,   '$1'} .
servChgReplyParm     -> serviceChangeVersion : {version,   '$1'}.
servChgReplyParm     -> timeStamp            : {time_stamp,'$1'}.

packagesDescriptor   -> 'PackagesToken' 'LBRKT' packagesItem packagesItems 'RBRKT'
                            : ['$3' | '$4'] .

packagesItems        -> 'COMMA' packagesItem packagesItems  : ['$2' | '$3'] .
packagesItems        -> '$empty' : [] .

packagesItem         -> safeToken : ensure_packagesItem('$1') .

timeStamp            -> TimeStampToken : ensure_timeStamp('$1') .

statisticsDescriptor -> 'StatsToken'
                        'LBRKT' statisticsParameter statisticsParameters 'RBRKT'
                            :  ['$3' | '$4'] .

statisticsParameters -> 'COMMA' statisticsParameter statisticsParameters  : ['$2' | '$3'] .
statisticsParameters -> '$empty' : [] .

%%at-most-once per item
statisticsParameter  -> pkgdName 
                            : #'StatisticsParameter'{statName  = '$1',
                                                     statValue = asn1_NOVALUE} .
statisticsParameter  -> pkgdName 'EQUAL' value
                            : #'StatisticsParameter'{statName  = '$1',
                                                     statValue = ['$3']} .

topologyDescriptor   -> 'TopologyToken' 'LBRKT' topologyTriple
                        topologyTripleList 'RBRKT' : ['$3' | '$4'] .

terminationA         -> terminationID  : '$1' .

terminationB         -> terminationID  : '$1' .

topologyTriple       -> terminationA 'COMMA' terminationB 'COMMA' 
                        topologyDirection :
                          #'TopologyRequest'{terminationFrom   = '$1',
                                             terminationTo     = '$3',
                                             topologyDirection = '$5'} .

topologyTripleList   -> '$empty' : [] .
topologyTripleList   -> 'COMMA' topologyTriple topologyTripleList :
                          ['$2' | '$3'] .

topologyDirection    -> 'BothwayToken' : bothway .
topologyDirection    -> 'IsolateToken' : isolate .
topologyDirection    -> 'OnewayToken'  : oneway .

priority             -> 'PriorityToken' 'EQUAL' safeToken : ensure_uint16('$3') .

extensionParameter   -> safeToken : ensure_extensionParameter('$1') .

value                -> 'QuotedChars' : ensure_value('$1') .
value                -> safeToken     : ensure_value('$1').

safeToken            -> safeToken2              : make_safe_token('$1') .

safeToken2           -> 'SafeChars'             : '$1' .
safeToken2           -> 'AddToken'              : '$1' .
safeToken2           -> 'AuditToken'            : '$1' .
safeToken2           -> 'AuditCapToken'         : '$1' .
safeToken2           -> 'AuditValueToken'       : '$1' .
safeToken2           -> 'AuthToken'             : '$1' .
safeToken2           -> 'BothwayToken'          : '$1' .
safeToken2           -> 'BriefToken'            : '$1' .
safeToken2           -> 'BufferToken'           : '$1' .
safeToken2           -> 'CtxToken'              : '$1' .
safeToken2           -> 'ContextAuditToken'     : '$1' .
safeToken2           -> 'DigitMapToken'         : '$1' .
%% safeToken2        -> 'DigitMapDescriptorToken' : '$1' .
safeToken2           -> 'DiscardToken'          : '$1' .
safeToken2           -> 'DisconnectedToken'     : '$1' .
safeToken2           -> 'DelayToken'            : '$1' .
safeToken2           -> 'DurationToken'         : '$1' .
safeToken2           -> 'EmbedToken'            : '$1' .
safeToken2           -> 'EmergencyToken'        : '$1' .
safeToken2           -> 'ErrorToken'            : '$1' .
safeToken2           -> 'EventBufferToken'      : '$1' .
safeToken2           -> 'EventsToken'           : '$1' .
safeToken2           -> 'FailoverToken'         : '$1' .
safeToken2           -> 'ForcedToken'           : '$1' .
safeToken2           -> 'GracefulToken'         : '$1' .
safeToken2           -> 'H221Token'             : '$1' .
safeToken2           -> 'H223Token'             : '$1' .
safeToken2           -> 'H226Token'             : '$1' .
safeToken2           -> 'HandOffToken'          : '$1' .
safeToken2           -> 'ImmAckRequiredToken'   : '$1' .
safeToken2           -> 'InactiveToken'         : '$1' .
safeToken2           -> 'InterruptByEventToken' : '$1' .
safeToken2           -> 'InterruptByNewSignalsDescrToken' : '$1' .
safeToken2           -> 'IsolateToken'          : '$1' .
safeToken2           -> 'InSvcToken'            : '$1' .
safeToken2           -> 'KeepActiveToken'       : '$1' .
%% safeToken2        -> 'LocalToken'            : '$1' .
%% safeToken2        -> 'LocalDescriptorToken'  : '$1' .
safeToken2           -> 'LocalControlToken'     : '$1' .
safeToken2           -> 'LoopbackToken'         : '$1' .
safeToken2           -> 'LockStepToken'         : '$1' .
safeToken2           -> 'MediaToken'            : '$1' .
%% safeToken2        -> 'MegacopToken'          : '$1' .
safeToken2           -> 'MethodToken'           : '$1' .
safeToken2           -> 'MgcIdToken'            : '$1' .
safeToken2           -> 'ModeToken'             : '$1' .
safeToken2           -> 'ModifyToken'           : '$1' .
safeToken2           -> 'ModemToken'            : '$1' .
safeToken2           -> 'MoveToken'             : '$1' .
%% safeToken2        -> 'MtpToken'              : '$1' .
%% safeToken2        -> 'MtpAddressToken'       : '$1' .
safeToken2           -> 'MuxToken'              : '$1' .
safeToken2           -> 'NotifyToken'           : '$1' .
safeToken2           -> 'NotifyCompletionToken' : '$1' .
safeToken2           -> 'ObservedEventsToken'   : '$1' .
safeToken2           -> 'OnewayToken'           : '$1' .
safeToken2           -> 'OffToken'              : '$1' .
safeToken2           -> 'OnToken'               : '$1' .
safeToken2           -> 'OnOffToken'            : '$1' .
safeToken2           -> 'OutOfSvcToken'         : '$1' .
safeToken2           -> 'OtherReasonToken'      : '$1' .
safeToken2           -> 'PackagesToken'         : '$1' .
safeToken2           -> 'PendingToken'          : '$1' .
safeToken2           -> 'PriorityToken'         : '$1' .
safeToken2           -> 'ProfileToken'          : '$1' .
safeToken2           -> 'ReasonToken'           : '$1' .
safeToken2           -> 'RecvonlyToken'         : '$1' .
safeToken2           -> 'ReplyToken'            : '$1' .
safeToken2           -> 'ResponseAckToken'      : '$1' .
safeToken2           -> 'RestartToken'          : '$1' .
%% safeToken2        -> 'RemoteToken'           : '$1' .
%% safeToken2        -> 'RemoteDescriptorToken' : '$1' .
safeToken2           -> 'ReservedGroupToken'    : '$1' .
safeToken2           -> 'ReservedValueToken'    : '$1' .
safeToken2           -> 'SendonlyToken'         : '$1' .
safeToken2           -> 'SendrecvToken'         : '$1' .
safeToken2           -> 'ServicesToken'         : '$1' .
safeToken2           -> 'ServiceStatesToken'    : '$1' .
safeToken2           -> 'ServiceChangeToken'    : '$1' .
safeToken2           -> 'ServiceChangeAddressToken' : '$1' .
safeToken2           -> 'SignalListToken'       : '$1' .
safeToken2           -> 'SignalsToken'          : '$1' .
safeToken2           -> 'SignalTypeToken'       : '$1' .
safeToken2           -> 'StatsToken'            : '$1' .
safeToken2           -> 'StreamToken'           : '$1' .
safeToken2           -> 'SubtractToken'         : '$1' .
safeToken2           -> 'SynchISDNToken'        : '$1' .
safeToken2           -> 'TerminationStateToken' : '$1' .
safeToken2           -> 'TestToken'             : '$1' .
safeToken2           -> 'TimeOutToken'          : '$1' .
safeToken2           -> 'TopologyToken'         : '$1' .
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
%% <OTP-7534>
safeToken2           -> 'AndAUDITSelectToken'   : '$1' . % v3
safeToken2           -> 'BothToken'             : '$1' . % v3
safeToken2           -> 'ContextAttrToken'      : '$1' . % v3
safeToken2           -> 'ContextListToken'      : '$1' . % v3
safeToken2           -> 'DirectionToken'        : '$1' . % v3
safeToken2           -> 'EmergencyOffToken'     : '$1' . % v3
safeToken2           -> 'EmergencyValueToken'   : '$1' . % v3
safeToken2           -> 'ExternalToken'         : '$1' . % v3
safeToken2           -> 'IEPSToken'             : '$1' . % v3
safeToken2           -> 'InternalToken'         : '$1' . % v3
safeToken2           -> 'IntsigDelayToken'      : '$1' . % v3
safeToken2           -> 'IterationToken'        : '$1' . % v3
safeToken2           -> 'MessageSegmentToken'   : '$1' . % v3
safeToken2           -> 'NeverNotifyToken'      : '$1' . % v3
safeToken2           -> 'NotifyImmediateToken'  : '$1' . % v3
safeToken2           -> 'NotifyRegulatedToken'  : '$1' . % v3
safeToken2           -> 'Nx64kToken'            : '$1' . % v2
safeToken2           -> 'OnewayBothToken'       : '$1' . % v3
safeToken2           -> 'OnewayExternalToken'   : '$1' . % v3
safeToken2           -> 'OrAUDITselectToken'    : '$1' . % v3
safeToken2           -> 'RequestIDToken'        : '$1' . % v3
safeToken2           -> 'ResetEventsDescriptorToken'    : '$1' . % v3
safeToken2           -> 'SegmentationCompleteToken'     : '$1' . % v3
safeToken2           -> 'ServiceChangeIncompleteToken'  : '$1' . % v3
%% </OTP-7534>

Erlang code.

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_parser_v1.hrl").

%%i(F) ->
%%    i(F, []).

%%i(F, A) ->
%%    i(get(dbg), F, A).

%%i(true, F, A) ->
%%    io:format("DBG:~p:" ++ F ++ "~n", [?MODULE|A]);
%%i(_, _, _) ->
%%    ok.

