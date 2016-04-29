%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% Purpose: Transform internal form of Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_binary_transformer_v1).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-export([tr_message/3, tr_transaction/3]).

-define(DEFAULT_NAME_RESOLVER,megaco_binary_name_resolver_v1).
-define(error(R), erlang:error({error, R})).

-record(state, {mode,                 % verify | encode | decode
                resolver_module,      % 
                resolver_options}).

resolve(Type, Item, State, Constraint) ->
    case State#state.mode of
        verify ->
            Item;
        encode ->
	    %% i("resolve(encode) -> encode: ~p",[Item]),
            Mod = State#state.resolver_module,
            Opt = State#state.resolver_options,
            EncodedItem = Mod:encode_name(Opt, Type, Item),
	    %% i("resolve -> verify contraint for ~p",[EncodedItem]),
	    verify_constraint(EncodedItem, Constraint);
        decode ->
	    %% i("resolve(decode) -> verify contraint for ~p",[Item]),
	    DecodedItem = verify_constraint(Item, Constraint),
            Mod = State#state.resolver_module,
            Opt = State#state.resolver_options,
	    %% i("resolve(decode) -> decode: ~p",[DecodedItem]),
            Mod:decode_name(Opt, Type, DecodedItem)
    end.

verify_constraint(Item, valid) ->
    Item;
verify_constraint(Item, Constraint) when is_function(Constraint) ->
    Constraint(Item).

tr_message(MegaMsg, Mode, Config) ->
    case Config of
        [native] ->
            MegaMsg;
        [verify] ->
            State = #state{mode = verify},
            tr_MegacoMessage(MegaMsg, State);
        [] ->
            State = #state{mode             = Mode,
                           resolver_module  = ?DEFAULT_NAME_RESOLVER,
                           resolver_options = [8, 8, 8]},
            tr_MegacoMessage(MegaMsg, State);
        [{binary_name_resolver, {Module, Options}}] when is_atom(Module) ->
            State = #state{mode             = Mode, 
                           resolver_module  = Module, 
                           resolver_options = Options},
            tr_MegacoMessage(MegaMsg, State)
    end.

tr_transaction(Trans, Mode, Config) ->
    case Config of
        [native] ->
            Trans;
        [verify] ->
            State = #state{mode = verify},
            tr_Transaction(Trans, State);
        [] ->
            State = #state{mode             = Mode,
                           resolver_module  = ?DEFAULT_NAME_RESOLVER,
                           resolver_options = [8, 8, 8]},
            tr_Transaction(Trans, State);
        [{binary_name_resolver, {Module, Options}}] when is_atom(Module) ->
            State = #state{mode             = Mode, 
                           resolver_module  = Module, 
                           resolver_options = Options},
            tr_Transaction(Trans, State)
    end.

tr_MegacoMessage(#'MegacoMessage'{authHeader = Auth,
                                  mess       = Mess},
                 State) ->
    #'MegacoMessage'{authHeader = tr_opt_AuthenticationHeader(Auth, State),
                     mess       = tr_Message(Mess, State)}.

tr_opt_AuthenticationHeader(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_AuthenticationHeader(#'AuthenticationHeader'{secParmIndex = SPI,
                                                    seqNum       = SN,
                                                    ad           = AuthData},
                            State) ->
    #'AuthenticationHeader'{secParmIndex = tr_SecurityParmIndex(SPI, State),
                            seqNum       = tr_SequenceNum(SN, State),
                            ad           = tr_AuthData(AuthData, State)}.

tr_SecurityParmIndex(SPI, State) ->
    tr_HEXDIG(SPI, State, 4, 4). % BUGBUG: Mismatch between ASN.1 and ABNF

tr_SequenceNum(SN, State) ->
    tr_HEXDIG(SN, State, 4, 4).  % BUGBUG: Mismatch between ASN.1 and ABNF

tr_AuthData(AuthData, State) ->
    tr_HEXDIG(AuthData, State, 12, 32).  % BUGBUG: Mismatch between ASN.1 and ABNF

tr_Message(#'Message'{version     = Version,
                      mId         = MID,
                      messageBody = Body},
           State) ->
    #'Message'{version     = tr_version(Version, State),
               mId         = tr_MId(MID, State),
               messageBody = tr_Message_messageBody(Body, State)}.

tr_version(Version, State) ->
    tr_DIGIT(Version, State, 0, 99).

tr_Message_messageBody({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            messageError -> tr_ErrorDescriptor(Val, State);
            transactions when is_list(Val) -> [tr_Transaction(T, State) || T <- Val]
        end,
    {Tag, Val2}.

tr_MId({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            ip4Address -> tr_IP4Address(Val, State);
            ip6Address -> tr_IP6Address(Val, State);
            domainName -> tr_DomainName(Val, State);
            deviceName -> tr_PathName(Val, State);
            mtpAddress -> tr_mtpAddress(Val, State)
        end,
    {Tag, Val2}.

tr_mtpAddress(MtpAddr, State) ->
    tr_OCTET_STRING(MtpAddr, State, 2, 4).  % BUGBUG: Mismatch between ASN.1 and ABNF

tr_DomainName(#'DomainName'{name       = Name,
                            portNumber = Port},
              State) ->
    Domain = #'DomainName'{name       = tr_STRING(Name, State), % BUGBUG: Mismatch between ASN.1 and ABNF
                           portNumber = tr_opt_portNumber(Port, State)},
    {domainName, Domain2} = resolve(mid, {domainName, Domain}, State, valid),
    Domain2.

tr_IP4Address(#'IP4Address'{address    = [A1, A2, A3, A4],
                            portNumber = Port},
              State) ->
    #'IP4Address'{address    = [tr_V4hex(A1, State),
                                tr_V4hex(A2, State),
                                tr_V4hex(A3, State),
                                tr_V4hex(A4, State)],
                  portNumber = tr_opt_portNumber(Port, State)}.

tr_V4hex(Val, State) ->
    tr_DIGIT(Val, State, 0, 255).

tr_IP6Address(_Val, _State) ->
    ?error(ipv6_not_supported). %% BUGBUG: nyi

tr_PathName(Path, State) ->
    %% BUGBUG: ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" ) 
    %% BUGBUG: ["@" pathDomainName ]
    Constraint = fun({deviceName, Item}) -> tr_STRING(Item, State, 1, 64) end,
    resolve(mid, {deviceName, Path}, State, Constraint).

tr_Transaction({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            transactionRequest ->     tr_TransactionRequest(Val, State);
            transactionPending ->     tr_TransactionPending(Val, State);
            transactionReply ->       tr_TransactionReply(Val, State);
            transactionResponseAck -> [tr_TransactionAck(T, State) || T <- Val]
        end,
    {Tag, Val2}.

tr_TransactionAck(#'TransactionAck'{firstAck = First,
				    lastAck  = Last},
                          State) ->
    #'TransactionAck'{firstAck = tr_TransactionId(First, State),
		      lastAck  = tr_opt_TransactionId(Last, State)}.

tr_opt_TransactionId(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_TransactionId(Id, State) ->
    tr_TransactionId(Id, State).

tr_TransactionId(Id, State) ->
    tr_UINT32(Id, State).

tr_TransactionRequest(#'TransactionRequest'{transactionId = Id,
                                            actions       = Actions},
                      State) when is_list(Actions) ->

    #'TransactionRequest'{transactionId = tr_TransactionId(Id, State),
                          actions       = [tr_ActionRequest(ActReq, State) || ActReq <- Actions]}.

tr_TransactionPending(#'TransactionPending'{transactionId = Id},
                      State) ->
    #'TransactionPending'{transactionId = tr_TransactionId(Id, State)}.

tr_TransactionReply(#'TransactionReply'{transactionId     = Id,
                                        immAckRequired    = ImmAck,
                                        transactionResult = TransRes},
                    State) ->
    #'TransactionReply'{transactionId     = tr_TransactionId(Id, State),
                        immAckRequired    = tr_opt_null(ImmAck, State),
                        transactionResult = tr_TransactionReply_transactionResult(TransRes, State)}.

tr_opt_null(asn1_NOVALUE, _State) -> asn1_NOVALUE;
tr_opt_null('NULL', _State)       -> 'NULL'.

tr_TransactionReply_transactionResult({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            transactionError ->
                tr_ErrorDescriptor(Val, State);
            actionReplies when is_list(Val) andalso (Val =/= []) ->
                [tr_ActionReply(ActRep, State) || ActRep <- Val]
        end,
    {Tag, Val2}.

tr_opt_ErrorDescriptor(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_ErrorDescriptor(ErrDesc, State) ->
    tr_ErrorDescriptor(ErrDesc, State).

tr_ErrorDescriptor(#'ErrorDescriptor'{errorCode = Code,
                                      errorText = Text},
                   State) ->
    #'ErrorDescriptor'{errorCode = tr_ErrorCode(Code, State),
                       errorText = tr_opt_ErrorText(Text, State)}.

tr_ErrorCode(Code, State) ->
    tr_DIGIT(Code, State, 0, 999).

tr_opt_ErrorText(asn1_NOVALUE, _State)  ->
    asn1_NOVALUE;
tr_opt_ErrorText(Text, State)  ->
    tr_QUOTED_STRING(Text, State).

tr_ContextID(CtxId, State) ->
    case CtxId of
        ?megaco_all_context_id    -> ?megaco_all_context_id;
        ?megaco_null_context_id   -> ?megaco_null_context_id;
        ?megaco_choose_context_id -> ?megaco_choose_context_id;
        Int when is_integer(Int)  -> tr_UINT32(Int, State)
    end.

tr_ActionRequest(#'ActionRequest'{contextId           = CtxId,
                                  contextRequest      = CtxReq,
                                  contextAttrAuditReq = CtxAuditReq,
                                  commandRequests     = CmdReqList},
                 State) ->
    #'ActionRequest'{contextId           = tr_ContextID(CtxId, State),
                     contextRequest      = tr_opt_ContextRequest(CtxReq, State),
                     contextAttrAuditReq = tr_opt_ContextAttrAuditRequest(CtxAuditReq, State),
                     commandRequests     = [tr_CommandRequest(CmdReq, State) || CmdReq <- CmdReqList]}.

tr_ActionReply(#'ActionReply'{contextId       = CtxId,
                              errorDescriptor = ErrDesc,
                              contextReply    = CtxRep,
                              commandReply    = CmdRepList},
               State) ->
    CmdRepList2 = [tr_CommandReply(CmdRep, State) || CmdRep <- CmdRepList],
    #'ActionReply'{contextId       = tr_ContextID(CtxId, State),
                   errorDescriptor = tr_opt_ErrorDescriptor(ErrDesc, State),
                   contextReply    = tr_opt_ContextRequest(CtxRep, State),
                   commandReply    = CmdRepList2}.

tr_opt_ContextRequest(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_ContextRequest(#'ContextRequest'{priority    = Prio,
                                        emergency   = Em,
                                        topologyReq = TopReqList},
                      State) ->
    Prio2 = 
        case Prio of
            asn1_NOVALUE -> asn1_NOVALUE;
            _            -> tr_integer(Prio, State, 0, 15)
        end,
    Em2 = 
        case Em of
            asn1_NOVALUE -> asn1_NOVALUE;
            false        -> false;
            true         -> true
        end,
    TopReqList2 = 
        case TopReqList of
            asn1_NOVALUE -> asn1_NOVALUE;
            _            -> [tr_TopologyRequest(TopReq, State) ||
                                TopReq <- TopReqList]
        end,
    #'ContextRequest'{priority    = Prio2,
                      emergency   = Em2,
                      topologyReq = TopReqList2}.

tr_opt_ContextAttrAuditRequest(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_ContextAttrAuditRequest(#'ContextAttrAuditRequest'{topology = Top,
                                                          emergency = Em,
                                                          priority = Prio},
                               State) ->
    #'ContextAttrAuditRequest'{topology  = tr_opt_null(Top, State),
                               emergency = tr_opt_null(Em, State),
                               priority  = tr_opt_null(Prio, State)}.

tr_CommandRequest(#'CommandRequest'{command        = Cmd,
                                    optional       = Opt,
                                    wildcardReturn = Wild},
                  State) ->
    #'CommandRequest'{optional       = tr_opt_null(Opt, State),
                      wildcardReturn = tr_opt_null(Wild, State),
                      command        = tr_Command(Cmd, State)}.

tr_Command({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            addReq ->            tr_AmmRequest(Val, State);
            moveReq ->           tr_AmmRequest(Val, State);
            modReq ->            tr_AmmRequest(Val, State);
            subtractReq ->       tr_SubtractRequest(Val, State);
            auditCapRequest ->   tr_AuditRequest(Val, State);
            auditValueRequest -> tr_AuditRequest(Val, State);
            notifyReq ->         tr_NotifyRequest(Val, State);
            serviceChangeReq ->  tr_ServiceChangeRequest(Val, State)
        end,
    {Tag, Val2}.

tr_CommandReply({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            addReply ->           tr_AmmsReply(Val, State);
            moveReply ->          tr_AmmsReply(Val, State);
            modReply ->           tr_AmmsReply(Val, State);
            subtractReply ->      tr_AmmsReply(Val, State);
            auditCapReply ->      tr_AuditReply(Val, State);
            auditValueReply ->    tr_AuditReply(Val, State);
            notifyReply ->        tr_NotifyReply(Val, State);
            serviceChangeReply -> tr_ServiceChangeReply(Val, State)
        end,
    {Tag, Val2}.

tr_TopologyRequest(#'TopologyRequest'{terminationFrom   = From,
                                      terminationTo     = To,
                                      topologyDirection = Dir},
                   State) ->
    Dir2 = 
        case Dir of
            bothway -> bothway;
            isolate -> isolate;
            oneway ->  oneway
        end,
    #'TopologyRequest'{terminationFrom   = tr_TerminationID(From, State),
                       terminationTo     = tr_TerminationID(To, State),
                       topologyDirection = Dir2}.

tr_AmmRequest(#'AmmRequest'{terminationID = IdList,
                            descriptors   = DescList},
              State) ->
    #'AmmRequest'{terminationID = [tr_TerminationID(Id, State) ||
                                      Id <- IdList],
                  descriptors   = [tr_ammDescriptor(Desc, State) ||
                                      Desc <- DescList]}.

tr_ammDescriptor({Tag, Desc}, State) ->
    Desc2 = 
        case Tag of
            mediaDescriptor       -> tr_MediaDescriptor(Desc, State);
            modemDescriptor       -> tr_ModemDescriptor(Desc, State);      
            muxDescriptor         -> tr_MuxDescriptor(Desc, State);   
            eventsDescriptor      -> tr_EventsDescriptor(Desc, State);      
            eventBufferDescriptor -> tr_EventBufferDescriptor(Desc, State); 
            signalsDescriptor     -> tr_SignalsDescriptor(Desc, State);    
            digitMapDescriptor    -> tr_DigitMapDescriptor(Desc, State);    
            auditDescriptor       -> tr_AuditDescriptor(Desc, State)
        end,
    {Tag, Desc2}.

tr_AmmsReply(#'AmmsReply'{terminationID    = IdList,
                          terminationAudit = TermAudit},
             State) ->
    TermAudit2 =
        case TermAudit of
            asn1_NOVALUE -> asn1_NOVALUE;
            _            -> tr_TerminationAudit(TermAudit, State)
        end,
    #'AmmsReply'{terminationID    = [tr_TerminationID(Id, State) ||
                                        Id <- IdList],
                 terminationAudit = TermAudit2}.

tr_SubtractRequest(#'SubtractRequest'{terminationID   = IdList,
                                      auditDescriptor = Desc},
                   State) ->
    #'SubtractRequest'{terminationID   = [tr_TerminationID(Id, State) ||
                                             Id <- IdList],
                       auditDescriptor = tr_opt_AuditDescriptor(Desc, State)}.

tr_AuditRequest(#'AuditRequest'{terminationID   = Id,
                                auditDescriptor = Desc},
                State) ->
    #'AuditRequest'{terminationID = tr_TerminationID(Id, State),
                    auditDescriptor = tr_AuditDescriptor(Desc, State)}.

%% auditReply           = (AuditValueToken / AuditCapToken ) 
%%                        ( contextTerminationAudit  / auditOther)
%% auditOther           = EQUAL TerminationID LBRKT 
%%                        terminationAudit RBRKT
%% terminationAudit     = auditReturnParameter *(COMMA auditReturnParameter) 
%% 
%% contextTerminationAudit = EQUAL CtxToken ( terminationIDList / 
%%                        LBRKT errorDescriptor RBRKT )

tr_AuditReply({Tag, Val}, State) ->
    Val2 =
	case Tag of
	    contextAuditResult ->
		[tr_TerminationID(Id, State) || Id <- Val];
	    error ->
		tr_ErrorDescriptor(Val, State);
	    auditResult ->
		tr_AuditResult(Val, State)
	end,
    {Tag, Val2}.

tr_AuditResult(#'AuditResult'{terminationID          = Id,
			      terminationAuditResult = AuditRes},
              State) ->
    #'AuditResult'{terminationID          = tr_TerminationID(Id, State),
		   terminationAuditResult = tr_TerminationAudit(AuditRes, State)}.

tr_opt_AuditDescriptor(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_AuditDescriptor(Desc, State) ->
    tr_AuditDescriptor(Desc, State).

tr_AuditDescriptor(#'AuditDescriptor'{auditToken = Tokens},
                   State) ->
    Tokens2 = 
	case Tokens of
	    asn1_NOVALUE -> asn1_NOVALUE;
	    _            -> [tr_auditItem(Token, State) || Token <- Tokens]
	end,
    #'AuditDescriptor'{auditToken = Tokens2}.  

tr_auditItem(Token, _State) ->
    case Token of
        muxToken               -> muxToken;
        modemToken             -> modemToken;
        mediaToken             -> mediaToken;
        eventsToken            -> eventsToken;
        signalsToken           -> signalsToken;
        digitMapToken          -> digitMapToken;
        statsToken             -> statsToken;
        observedEventsToken    -> observedEventsToken;
        packagesToken          -> packagesToken;
        eventBufferToken       -> eventBufferToken
    end.

tr_TerminationAudit(ParmList, State) when is_list(ParmList) ->
    [tr_AuditReturnParameter(Parm, State) || Parm <- ParmList].

tr_AuditReturnParameter({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            errorDescriptor ->
                tr_ErrorDescriptor(Val, State);
            mediaDescriptor ->
                tr_MediaDescriptor(Val, State);
            modemDescriptor ->
                tr_ModemDescriptor(Val, State);
            muxDescriptor ->
                tr_MuxDescriptor(Val, State);
            eventsDescriptor ->
                tr_EventsDescriptor(Val, State);
            eventBufferDescriptor ->
                tr_EventBufferDescriptor(Val, State);
            signalsDescriptor ->
                tr_SignalsDescriptor(Val, State);
            digitMapDescriptor ->
                tr_DigitMapDescriptor(Val, State);
            observedEventsDescriptor ->
                tr_ObservedEventsDescriptor(Val, State);
            statisticsDescriptor ->
                tr_StatisticsDescriptor(Val, State);
            packagesDescriptor ->
                tr_PackagesDescriptor(Val, State);
            emptyDescriptors ->
                tr_EmptyDescriptors(Val, State)
        end,
    {Tag, Val2}.

tr_EmptyDescriptors(#'AuditDescriptor'{auditToken = Tokens},
                    State) ->
    case Tokens of
        asn1_NOVALUE -> asn1_NOVALUE;
        _            -> [tr_auditItem(Token, State) || Token <- Tokens]
    end.

tr_NotifyRequest(#'NotifyRequest'{terminationID            = IdList,
                                  observedEventsDescriptor = ObsDesc,
                                  errorDescriptor          = ErrDesc},
                 State) ->
    %% BUGBUG: Mismatch between ASN.1 and ABNF
    %% BUGBUG: The following ought to be a 'choice'
    #'NotifyRequest'{terminationID            = [tr_TerminationID(Id, State) ||
                                                    Id <- IdList],
                     observedEventsDescriptor = tr_ObservedEventsDescriptor(ObsDesc, State),
                     errorDescriptor          = tr_opt_ErrorDescriptor(ErrDesc, State)}.

tr_NotifyReply(#'NotifyReply'{terminationID   = IdList,
                              errorDescriptor = ErrDesc},
               State) ->
    #'NotifyReply'{terminationID   = [tr_TerminationID(Id, State) || Id <- IdList],
                   errorDescriptor = tr_opt_ErrorDescriptor(ErrDesc, State)}.

tr_ObservedEventsDescriptor(#'ObservedEventsDescriptor'{requestId        = Id,
                                                        observedEventLst = Events},
                            State) when is_list(Events) ->
    #'ObservedEventsDescriptor'{requestId        = tr_RequestID(Id, State),
                                observedEventLst = [tr_ObservedEvent(E, State) || E <- Events]}.

%% ;time per event, because it might be buffered
%% observedEvent        = [ TimeStamp LWSP COLON] LWSP 
%%                        pkgdName [ LBRKT observedEventParameter
%%                        *(COMMA observedEventParameter) RBRKT ]
%% 
%% ;at-most-once eventStream, every eventParameterName at most once
%% observedEventParameter = eventStream / eventOther

tr_ObservedEvent(#'ObservedEvent'{eventName    = Name,
                                  streamID     = Id,
                                  eventParList = Parms,
                                  timeNotation = Time},
                 State) ->
    #'ObservedEvent'{eventName    = tr_EventName(Name, State),
                     streamID     = tr_opt_StreamID(Id, State),
                     eventParList = [tr_EventParameter(P, Name, State) || P <- Parms],
                     timeNotation = tr_opt_TimeNotation(Time, State)}.

tr_EventName(Name, State) ->
    Constraint = fun(Item) -> tr_PkgdName(Item, State) end,
    resolve(event, Name, State, Constraint).

tr_EventParameter(#'EventParameter'{eventParameterName = ParName,
                                    value              = Value,
				    extraInfo          = Extra},
		  EventName,
                  State) ->
    %% BUGBUG: event parameter name
    Constraint = fun(Item) -> tr_Name(Item, State) end,
    N = resolve({event_parameter, EventName}, ParName, State, Constraint),
    #'EventParameter'{eventParameterName = N,
		      value              = tr_Value(Value, State),
		      extraInfo          = tr_opt_extraInfo(Extra, State)}.

tr_ServiceChangeRequest(#'ServiceChangeRequest'{terminationID      = IdList,
                                                serviceChangeParms = Parms},
                        State) ->
    #'ServiceChangeRequest'{terminationID      = [tr_TerminationID(Id, State) || Id <- IdList],
                            serviceChangeParms = tr_ServiceChangeParm(Parms, State)}.

%% serviceChangeReply   = ServiceChangeToken EQUAL TerminationID
%%                        [LBRKT (errorDescriptor / 
%%                        serviceChangeReplyDescriptor) RBRKT]
%% serviceChangeReplyDescriptor = ServicesToken LBRKT
%%                        servChgReplyParm *(COMMA servChgReplyParm) RBRKT
%% 
%% ;at-most-once. Version is REQUIRED on first ServiceChange response
%% servChgReplyParm     = (serviceChangeAddress / serviceChangeMgcId /
%%                        serviceChangeProfile / serviceChangeVersion )
tr_ServiceChangeReply(#'ServiceChangeReply'{terminationID       = IdList,
                                            serviceChangeResult = Res},
                      State) ->
    #'ServiceChangeReply'{terminationID       = [tr_TerminationID(Id, State) || Id <- IdList],
                          serviceChangeResult = tr_ServiceChangeResult(Res, State)}.

tr_ServiceChangeResult({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            errorDescriptor       -> tr_ErrorDescriptor(Val, State);
            serviceChangeResParms -> tr_ServiceChangeResParm(Val, State)
        end,
    {Tag, Val2}.

%% TerminationID        = "ROOT" / pathNAME / "$" / "*"
%% ; Total length of pathNAME must not exceed 64 chars.
%% pathNAME             = ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" ) 
%%                        ["@" pathDomainName ]

tr_TerminationID(TermId, State) when State#state.mode =/= verify ->
    resolve(term_id, TermId, State, valid);
tr_TerminationID(#'TerminationID'{wildcard = Wild,
                                  id       = Id},
                 _State) ->
    #'TerminationID'{wildcard = Wild,
                     id       = Id};
tr_TerminationID(#megaco_term_id{contains_wildcards = IsWild,
                                 id                 = Id},
                 State) ->
    #megaco_term_id{contains_wildcards = tr_bool(IsWild, State),
                    id                 = [tr_term_id_component(Sub, State) || Sub <- Id]}.

tr_opt_bool(asn1_NOVALUE, _State) -> asn1_NOVALUE;
tr_opt_bool(Bool, State)         -> tr_bool(Bool, State).

tr_bool(true, _State)  -> true;
tr_bool(false, _State) -> false.

tr_term_id_component(Sub, _State) ->
    case Sub of
        all    -> all;
        choose -> choose;
        Char when is_integer(Char) -> Char
    end.

%% mediaDescriptor      = MediaToken LBRKT mediaParm *(COMMA mediaParm) RBRKT
%% ; at-most-once per item
%% ; and either streamParm or streamDescriptor but not both
%% mediaParm            = (streamParm / streamDescriptor / 
%%                         terminationStateDescriptor)
%% ; at-most-once
%% streamParm           = ( localDescriptor / remoteDescriptor / 
%%                         localControlDescriptor )
%% streamDescriptor     = StreamToken EQUAL StreamID LBRKT streamParm 
%%                        *(COMMA streamParm) RBRKT
tr_MediaDescriptor(#'MediaDescriptor'{termStateDescr = TermState,
                                      streams        = Streams},
                   State) ->
    #'MediaDescriptor'{termStateDescr = tr_opt_TerminationStateDescriptor(TermState, State),
                       streams        = tr_opt_streams(Streams, State)}.

tr_opt_streams(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_streams({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            oneStream   -> tr_StreamParms(Val, State);
            multiStream -> [tr_StreamDescriptor(SD, State) || SD <- Val]
        end,
    {Tag, Val2}.

tr_StreamParms(#'StreamParms'{localControlDescriptor = Control,
                              localDescriptor        = Local,
                              remoteDescriptor       = Remote},
               State) ->
    #'StreamParms'{localControlDescriptor = tr_opt_LocalControlDescriptor(Control, State),
                   localDescriptor        = tr_opt_LocalRemoteDescriptor(Local, State),
                   remoteDescriptor       = tr_opt_LocalRemoteDescriptor(Remote, State)}.

tr_StreamDescriptor(#'StreamDescriptor'{streamID    = Id,
                                        streamParms = Parms},
                    State) ->
    #'StreamDescriptor'{streamID    = tr_StreamID(Id, State),
                        streamParms = tr_StreamParms(Parms, State)}.

%% localControlDescriptor = LocalControlToken LBRKT localParm 
%%                          *(COMMA localParm) RBRKT
%% 
%% ; at-most-once per item
%% localParm            = ( streamMode / propertyParm /
%%                          reservedValueMode  / reservedGroupMode ) 
%% reservedValueMode       = ReservedValueToken EQUAL ( "ON" / "OFF" ) 
%% reservedGroupMode       = ReservedGroupToken EQUAL ( "ON" / "OFF" ) 
%% 
%% reservedMode      = ReservedToken EQUAL ( "ON" / "OFF" )
%% 
%% streamMode           = ModeToken EQUAL streamModes
tr_opt_LocalControlDescriptor(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_LocalControlDescriptor(#'LocalControlDescriptor'{streamMode    = Mode,
                                                        reserveGroup  = Group,
                                                        reserveValue  = Value,
                                                        propertyParms = Props},
                              State) ->
    #'LocalControlDescriptor'{streamMode    = tr_opt_StreamMode(Mode, State),
                              reserveGroup  = tr_opt_bool(Group, State),
                              reserveValue  = tr_opt_bool(Value, State),
                              propertyParms = [tr_PropertyParm(P, State) || P <- Props]}.

tr_opt_StreamMode(Mode, _State) ->
    case Mode of
        asn1_NOVALUE -> asn1_NOVALUE;
        sendOnly     -> sendOnly;
        recvOnly     -> recvOnly;
        sendRecv     -> sendRecv;
        inactive     -> inactive;
        loopBack     -> loopBack
    end.

tr_Name(Name, State) ->
    %% BUGBUG: transform
    %% BUGBUG: NAME = ALPHA *63(ALPHA / DIGIT / "_" )
    tr_STRING(Name, State, 2, 2).

tr_PkgdName(Name, State) ->
    %% BUGBUG: transform
    %% BUGBUG:  pkgdName =  (NAME / "*")  SLASH  (ItemID / "*" )
    tr_OCTET_STRING(Name, State, 4, 4).

%% When text encoding the protocol, the descriptors consist of session
%% descriptions as defined in SDP (RFC2327), except that the "s=", "t="
%% and "o=" lines are optional. When multiple session descriptions are
%% provided in one descriptor, the "v=" lines are required as delimiters;
%% otherwise they are optional.  Implementations shall accept session
%% descriptions that are fully conformant to RFC2327. When binary
%% encoding the protocol the descriptor consists of groups of properties
%% (tag-value pairs) as specified in Annex C.  Each such group may
%% contain the parameters of a session description.
tr_opt_LocalRemoteDescriptor(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_LocalRemoteDescriptor(#'LocalRemoteDescriptor'{propGrps = Groups},
                             State) ->
    #'LocalRemoteDescriptor'{propGrps = [tr_PropertyGroup(G, State) || G <- Groups]}.

tr_PropertyGroup(Props, State) ->
    [tr_PropertyGroupParm(P, State) || P <- Props].

tr_PropertyGroupParm(#'PropertyParm'{name  = Name,
                                     value = Value},
                     State) ->
    Constraint = fun(Item) -> tr_PkgdName(Item, State) end,
    #'PropertyParm'{name  = resolve(property, Name, State, Constraint),
                    value = tr_OCTET_STRING(Value, State, 0, infinity)}.

tr_PropertyParm(#'PropertyParm'{name      = Name,
                                value     = Value,
                                extraInfo = Extra},
                State) ->
    Constraint = fun(Item) -> tr_PkgdName(Item, State) end,
    #'PropertyParm'{name      = resolve(property, Name, State, Constraint),
                    value     = tr_Value(Value, State),
                    extraInfo = tr_opt_extraInfo(Extra, State)}.

tr_opt_extraInfo(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_extraInfo({relation, Rel}, _State) ->
    Rel2 = 
        case Rel of
            greaterThan -> greaterThan;
            smallerThan -> smallerThan;
            unequalTo   -> unequalTo
        end,
    {relation, Rel2};
tr_opt_extraInfo({range, Range}, State) ->
    Range2 = tr_bool(Range, State),
    {range, Range2};
tr_opt_extraInfo({sublist, Sub}, State) ->
    Sub2 = tr_bool(Sub, State),
    {sublist, Sub2}.

tr_opt_TerminationStateDescriptor(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_TerminationStateDescriptor(#'TerminationStateDescriptor'{propertyParms      = Props,
                                                                eventBufferControl = Control,
                                                                serviceState       = Service},
                                  State) ->
    #'TerminationStateDescriptor'{propertyParms      = [tr_PropertyParm(P, State) || P <- Props],
                                  eventBufferControl = tr_opt_EventBufferControl(Control, State),
                                  serviceState       = tr_opt_ServiceState(Service, State)}.

tr_opt_EventBufferControl(Control, _State) ->
    case Control of
        asn1_NOVALUE -> asn1_NOVALUE;
        off          -> off;
        lockStep     -> lockStep
    end.

tr_opt_ServiceState(Service, _State) ->
    case Service of
        asn1_NOVALUE -> asn1_NOVALUE;
        test         -> test;
        outOfSvc     -> outOfSvc;
        inSvc        -> inSvc
    end.

tr_MuxDescriptor(#'MuxDescriptor'{muxType  = Type,
                                  termList = IdList},
                 State) ->
    #'MuxDescriptor'{muxType  = tr_MuxType(Type, State),
                     termList = [tr_TerminationID(Id, State) || Id <- IdList]}.

tr_MuxType(Type, _State) ->
    case Type of
        h221 -> h221;
        h223 -> h223;
        h226 -> h226;
        v76  -> v76
    end.

tr_opt_StreamID(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_StreamID(Id, State) ->
    tr_StreamID(Id, State).

tr_StreamID(Id, State) ->
    tr_UINT16(Id, State).

tr_EventsDescriptor(#'EventsDescriptor'{requestID = Id,
                                        eventList = Events},
                    State) ->
    #'EventsDescriptor'{requestID = tr_opt_RequestID(Id, State),
                        eventList = [tr_RequestedEvent(E, State) || E <- Events]}.

tr_RequestedEvent(#'RequestedEvent'{pkgdName    = Name,
                                    streamID    = Id,
                                    evParList   = Parms,
                                    eventAction = Actions},
                  State)  ->
    Constraint = fun(Item) -> tr_PkgdName(Item, State) end,
    #'RequestedEvent'{pkgdName    = resolve(event, Name, State, Constraint),
                      streamID    = tr_opt_StreamID(Id, State),
                      eventAction = tr_opt_RequestedActions(Actions, State),
                      evParList   = [tr_EventParameter(P, Name, State) || P <- Parms]}.

tr_opt_RequestedActions(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_RequestedActions(#'RequestedActions'{keepActive        = Keep,
                                            eventDM           = DM,
                                            secondEvent       = Event,
                                            signalsDescriptor = SigDesc},
                        State) ->
    #'RequestedActions'{keepActive        = tr_opt_keepActive(Keep, State),
                        eventDM           = tr_opt_EventDM(DM, State),
                        secondEvent       = tr_opt_SecondEventsDescriptor(Event, State),
                        signalsDescriptor = tr_opt_SignalsDescriptor(SigDesc, State)}.

tr_opt_keepActive(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_keepActive(Keep, State) ->
    tr_bool(Keep, State).

tr_opt_EventDM(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_EventDM({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            digitMapName  -> tr_DigitMapName(Val, State);
            digitMapValue -> tr_DigitMapValue(Val, State)
        end,
    {Tag, Val2}.

tr_opt_SecondEventsDescriptor(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_SecondEventsDescriptor(#'SecondEventsDescriptor'{requestID = Id,
                                                        eventList = Events},
                              State) ->
    #'SecondEventsDescriptor'{requestID = tr_RequestID(Id, State), %% IG v6 6.8 withdrawn
                              eventList = [tr_SecondRequestedEvent(E, State) || E <- Events]}.

tr_SecondRequestedEvent(#'SecondRequestedEvent'{pkgdName    = Name,
                                                streamID    = Id,
                                                evParList   = Parms,
                                                eventAction = Actions},
                        State) ->
    Constraint = fun(Item) -> tr_PkgdName(Item, State) end,
    #'SecondRequestedEvent'{pkgdName    = resolve(event, Name, State, Constraint),
                            streamID    = tr_opt_StreamID(Id, State),
                            eventAction = tr_opt_SecondRequestedActions(Actions, State),
                            evParList   = [tr_EventParameter(P, Name, State) || P <- Parms]}.


tr_opt_SecondRequestedActions(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_SecondRequestedActions(#'SecondRequestedActions'{keepActive        = Keep,
                                                        eventDM           = DM,
                                                        signalsDescriptor = SigDesc},
                              State) ->
    #'SecondRequestedActions'{keepActive        = tr_opt_keepActive(Keep, State),
                              eventDM           = tr_opt_EventDM(DM, State),
                              signalsDescriptor = tr_opt_SignalsDescriptor(SigDesc, State)}.

tr_EventBufferDescriptor(EventSpecs, State) ->
    [tr_EventSpec(ES, State) || ES <- EventSpecs].

tr_EventSpec(#'EventSpec'{eventName    = Name,
                          streamID     = Id,
                          eventParList = Parms},
             State) ->
    #'EventSpec'{eventName    = tr_EventName(Name, State),
                 streamID     = tr_opt_StreamID(Id, State),
                 eventParList = [tr_EventParameter(P, Name, State) || P <- Parms]}.

tr_opt_SignalsDescriptor(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_SignalsDescriptor(SigDesc, State) ->
    tr_SignalsDescriptor(SigDesc, State).

tr_SignalsDescriptor(SigDesc, State)  when is_list(SigDesc) ->
    [tr_SignalRequest(SigReq, State) || SigReq <- SigDesc].

tr_SignalRequest({Tag, Val}, State) ->
    Val2 =
        case Tag of
            signal     -> tr_Signal(Val, State);
            seqSigList -> tr_SeqSigList(Val, State)
        end,
    {Tag, Val2}.


tr_SeqSigList(#'SeqSigList'{id         = Id,
                            signalList = SigList},
              State) when is_list(SigList) ->
    #'SeqSigList'{id         = tr_UINT16(Id, State),
                  signalList = [tr_Signal(Sig, State) || Sig <- SigList]}.

tr_Signal(#'Signal'{signalName       = Name,
                    streamID         = Id,
                    sigType          = Type,
                    duration         = Dur,
                    notifyCompletion = Compl,
                    keepActive       = Keep,
                    sigParList       = Parms},
          State) ->
    #'Signal'{signalName       = tr_SignalName(Name, State),
              streamID         = tr_opt_StreamID(Id, State),
              sigType          = tr_opt_SignalType(Type, State),
              duration         = tr_opt_duration(Dur, State),
              notifyCompletion = tr_opt_NotifyCompletion(Compl, State),
              keepActive       = tr_opt_keepActive(Keep, State),
              sigParList       = [tr_SigParameter(P, Name, State) || P <- Parms]}.

tr_opt_duration(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_duration(Dur, State) ->
    tr_UINT16(Dur, State).

tr_opt_NotifyCompletion(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_NotifyCompletion(Items, State) when is_list(Items) ->
    [tr_notifyCompletionItem(I, State) || I <- Items].

tr_notifyCompletionItem(Item, _State) ->
    case Item of
        onTimeOut                   -> onTimeOut;
        onInterruptByEvent          -> onInterruptByEvent;
        onInterruptByNewSignalDescr -> onInterruptByNewSignalDescr;
        otherReason                 -> otherReason
    end.

tr_opt_SignalType(Type, _State) ->
    case Type of
        asn1_NOVALUE -> asn1_NOVALUE;
        brief        ->   brief;
        onOff        ->   onOff;
        timeOut      -> timeOut
    end.

tr_SignalName(Name, State) ->
    Constraint = fun(Item) -> tr_PkgdName(Item, State) end,
    resolve(signal, Name, State, Constraint).

tr_SigParameter(#'SigParameter'{sigParameterName = ParName,
                                value            = Value,
                                extraInfo        = Extra},
                SigName,
                State) ->
    Constraint = fun(Item) -> tr_Name(Item, State) end,
    N = resolve({signal_parameter, SigName}, ParName, State, Constraint),
    #'SigParameter'{sigParameterName = N,
                    value            = tr_Value(Value, State),
                    extraInfo        = tr_opt_extraInfo(Extra, State)}.

tr_opt_RequestID(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_RequestID(Id, State) ->
    tr_RequestID(Id, State).

tr_RequestID(Id, _State) when Id =:= ?megaco_all_request_id ->
    ?megaco_all_request_id;
tr_RequestID(Id, State) ->
    tr_UINT32(Id, State).

tr_ModemDescriptor(#'ModemDescriptor'{mtl = Types,
                                      mpl = Props},
                   State) when is_list(Types) andalso is_list(Props) -> 
    %% BUGBUG: Does not handle extensionParameter
    #'ModemDescriptor'{mtl = [tr_ModemType(T, State) || T <- Types],
                       mpl = [tr_PropertyParm(P, State) || P <- Props]}.

tr_ModemType(Type, _State) ->
    %% BUGBUG: Does not handle extensionParameter
    case Type of
        v18       -> v18;
        v22       -> v22;
        v22bis    -> v22bis;
        v32       -> v32;
        v32bis    -> v32bis;
        v34       -> v34;
        v90       -> v90;
        v91       -> v91;
        synchISDN -> synchISDN
    end.

tr_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = Name,
                                            digitMapValue = Value},
                      State) ->
    #'DigitMapDescriptor'{digitMapName  = tr_opt_DigitMapName(Name, State),
                          digitMapValue = tr_opt_DigitMapValue(Value, State)}.

tr_opt_DigitMapName(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_DigitMapName(Name, State) ->
    tr_DigitMapName(Name, State).

tr_DigitMapName(Name, State) ->
    Constraint = fun(Item) -> tr_Name(Item, State) end,
    resolve(dialplan, Name, State, Constraint).

tr_opt_DigitMapValue(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_DigitMapValue(Value, State) ->
    tr_DigitMapValue(Value, State).

tr_DigitMapValue(#'DigitMapValue'{digitMapBody = Body,
                                  startTimer   = Start,
                                  shortTimer   = Short,
                                  longTimer    = Long},
                 State) ->
    #'DigitMapValue'{startTimer   = tr_opt_timer(Start, State),
                     shortTimer   = tr_opt_timer(Short, State),
                     longTimer    = tr_opt_timer(Long, State),
                     digitMapBody = tr_STRING(Body, State)}. %% BUGBUG: digitMapBody not handled at all

tr_opt_timer(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_timer(Timer, State) ->
    tr_DIGIT(Timer, State, 0, 99).

tr_ServiceChangeParm(#'ServiceChangeParm'{serviceChangeMethod  = Method, 
                                          serviceChangeAddress = Addr, 
                                          serviceChangeVersion = Version, 
                                          serviceChangeProfile = Profile, 
                                          serviceChangeReason  = Reason, 
                                          serviceChangeDelay   = Delay, 
                                          serviceChangeMgcId   = MgcId, 
                                          timeStamp            = Time},
                     State) ->
    #'ServiceChangeParm'{serviceChangeMethod  = tr_ServiceChangeMethod(Method, State),
                         serviceChangeAddress = tr_opt_ServiceChangeAddress(Addr, State),
                         serviceChangeVersion = tr_opt_serviceChangeVersion(Version, State),
                         serviceChangeProfile = tr_opt_ServiceChangeProfile(Profile, State),
                         serviceChangeReason  = tr_serviceChangeReason(Reason, State),
                         serviceChangeDelay   = tr_opt_serviceChangeDelay(Delay, State),
                         serviceChangeMgcId   = tr_opt_serviceChangeMgcId(MgcId, State),
                         timeStamp            = tr_opt_TimeNotation(Time, State)}.

tr_ServiceChangeMethod(Method, _State) ->
    case Method of
        failover      -> failover;
        forced        -> forced;
        graceful      -> graceful;
        restart       -> restart;
        disconnected  -> disconnected;
        handOff       -> handOff
    end. %% BUGBUG: extension

tr_opt_ServiceChangeAddress(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_ServiceChangeAddress({Tag, Val}, State) ->
    Val2 = 
        case Tag of
            portNumber -> tr_portNumber(Val, State);
            ip4Address -> tr_IP4Address(Val, State);
            ip6Address -> tr_IP6Address(Val, State);
            domainName -> tr_DomainName(Val, State);
            deviceName -> tr_PathName(Val, State);
            mtpAddress -> tr_mtpAddress(Val, State)
        end,
    {Tag, Val2}.

tr_opt_serviceChangeVersion(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_serviceChangeVersion(Version, State) ->
    tr_version(Version, State).

tr_opt_ServiceChangeProfile(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
%% Decode
tr_opt_ServiceChangeProfile({'ServiceChangeProfile', ProfileName}, State) ->
    case string:tokens(ProfileName, "/") of
        [Name0, Version0] ->
	    Name    = tr_STRING(Name0, State, 1, 64),
	    Version = tr_version(list_to_integer(Version0), State),
	    #'ServiceChangeProfile'{profileName = Name, 
				    version     = Version}
    end;
%% Encode
tr_opt_ServiceChangeProfile(#'ServiceChangeProfile'{profileName = Name0, 
						    version     = Version0},
			    State) ->
    Name        = tr_STRING(Name0, State, 1, 64),
    Version     = tr_version(Version0, State),
    ProfileName = lists:flatten(io_lib:format("~s/~w", [Name, Version])),
    {'ServiceChangeProfile', ProfileName}.

tr_serviceChangeReason([_] = Reason, State) ->
    tr_Value(Reason, State).

tr_opt_serviceChangeDelay(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_serviceChangeDelay(Delay, State) ->
    tr_UINT32(Delay, State).

tr_opt_serviceChangeMgcId(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_serviceChangeMgcId(MgcId, State) ->
    tr_MId(MgcId, State).

tr_opt_portNumber(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_portNumber(Port, State) ->
    tr_portNumber(Port, State).

tr_portNumber(Port, State) when is_integer(Port) andalso (Port >= 0) ->
    tr_UINT16(Port, State).

tr_ServiceChangeResParm(#'ServiceChangeResParm'{serviceChangeMgcId   = MgcId, 
                                                serviceChangeAddress = Addr, 
                                                serviceChangeVersion = Version, 
                                                serviceChangeProfile = Profile,
						timeStamp            = Time}, 
                        State) ->
    #'ServiceChangeResParm'{serviceChangeMgcId   = tr_opt_serviceChangeMgcId(MgcId, State),
                            serviceChangeAddress = tr_opt_ServiceChangeAddress(Addr, State),
                            serviceChangeVersion = tr_opt_serviceChangeVersion(Version, State),
                            serviceChangeProfile = tr_opt_ServiceChangeProfile(Profile, State),
			    timeStamp            = tr_opt_TimeNotation(Time, State)}.

tr_PackagesDescriptor(Items, State) when is_list(Items) ->
    [tr_PackagesItem(I, State) || I <- Items].

tr_PackagesItem(#'PackagesItem'{packageName    = Name,
                                packageVersion = Version},
                State) ->
    Constraint = fun(Item) -> tr_Name(Item, State) end,
    #'PackagesItem'{packageName    = resolve(package, Name, State, Constraint),
                    packageVersion = tr_UINT16(Version, State)}.

tr_StatisticsDescriptor(Parms, State) when is_list(Parms) ->
    [tr_StatisticsParameter(P, State) || P <- Parms].

tr_StatisticsParameter(#'StatisticsParameter'{statName  = Name,
                                              statValue = Value},
                       State) ->
    Constraint = fun(Item) -> tr_PkgdName(Item, State) end,
    #'StatisticsParameter'{statName  = resolve(statistics, Name, State, Constraint),
                           statValue = tr_opt_Value(Value, State)}.

tr_opt_TimeNotation(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_TimeNotation(#'TimeNotation'{date = Date,
                                    time = Time},
                    State) ->
    #'TimeNotation'{date = tr_STRING(Date, State, 8, 8), % "yyyymmdd"
                    time = tr_STRING(Time, State, 8, 8)}.% "hhmmssss"

%% BUGBUG: Does not verify that string must contain at least one char
%% BUGBUG: This violation of the is required in order to comply with
%% BUGBUG: the dd/ce ds parameter that may possibly be empty.

tr_opt_Value(asn1_NOVALUE, _State) ->
    asn1_NOVALUE;
tr_opt_Value(Value, State) ->
    tr_Value(Value, State).

tr_Value(Strings, _State) when is_list(Strings) ->
    [[Char || Char <- String] || String <- Strings].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Encode an octet string, escape } by \ if necessary 
tr_OCTET_STRING(String, _State, Min, Max) when is_list(String) ->
    verify_count(length(String), Min, Max),
    String.

tr_QUOTED_STRING(String, _State) when is_list(String) ->
    verify_count(length(String), 1, infinity),
    String.

%% The internal format of hex digits is a list of octets
%% Min and Max means #hexDigits
%% Leading zeros are prepended in order to fulfill Min
tr_HEXDIG(Octets, _State, Min, Max) when is_list(Octets) ->
    verify_count(length(Octets), Min, Max),
    Octets.

tr_DIGIT(Val, State, Min, Max) ->
    tr_integer(Val, State, Min, Max).

tr_STRING(String, _State) when is_list(String) ->
    String.

tr_STRING(String, _State, Min, Max) when is_list(String) ->
    verify_count(length(String), Min, Max),
    String.

tr_UINT16(Val, State) ->
    tr_integer(Val, State, 0, 65535).

tr_UINT32(Val, State) ->
    tr_integer(Val, State, 0, 4294967295).

tr_integer(Int, _State, Min, Max) ->
    verify_count(Int, Min, Max),
    Int.

%% Verify that Count is within the range of Min and Max
verify_count(Count, Min, Max) ->
    if
        is_integer(Count) ->
            if
                is_integer(Min) andalso (Count >= Min) ->
                    if
                        is_integer(Max) andalso (Count =< Max) ->
                            Count;
                        Max =:= infinity ->
                            Count;
                        true ->
                            ?error({count_too_large, Count, Max})
                    end;
                true ->
                    ?error({count_too_small, Count, Min})
            end;
        true ->
            ?error({count_not_an_integer, Count})
    end.


% i(F,A) ->
%     %% i(true,F,A).
%     i(get(dbg),F,A).

% i(true,F,A) ->
%     S1 = io_lib:format("TRANSF-v1: " ++ F ++ "~n",A),
%     S2 = lists:flatten(S1),
%     io:format("~s",[S2]);
% i(_,_F,_A) ->
%     ok.
