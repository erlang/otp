%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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
%% Purpose : Define semantic text parser actions
%%----------------------------------------------------------------------


-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v2.hrl").
-include("megaco_text_tokens.hrl").

-define(d(F,A), io:format("DBG:"++F++"~n",A)).

make_safe_token({_TokenTag, Line, Text}) ->
    {safeToken, Line, Text}.

% ensure_value({safeToken, _Line, Text}) ->
%     ensure_value(Text);
% ensure_value({'QuotedChars', _Line, Text}) ->
%     ensure_value(Text);
% ensure_value(Text) when list(Text) ->
%     Text. %% BUGBUG: ensure length

% %% NAME       = ALPHA *63(ALPHA / DIGIT / "_" )
% ensure_NAME({_TokenTag, _Line, Text}) ->
%     Text.  %% BUGBUG: ensure length and chars

% ensure_requestID({safeToken, _Line, "*"}) ->
%     ?megaco_all_request_id;
% ensure_requestID(RequestId) ->
%     ensure_uint32(RequestId).

% ensure_streamID(StreamId) ->
%     ensure_uint16(StreamId).

ensure_auth_header(SpiToken, SnToken, AdToken) ->
    Spi = ensure_hex(SpiToken, 8,  8),
    Sn  = ensure_hex(SnToken,  8,  8), 
    Ad  = ensure_hex(AdToken, 24, 64),
    #'AuthenticationHeader'{secParmIndex = Spi, seqNum = Sn, ad = Ad}.

%% ContextID         = (UINT32 / "*" / "-" / "$")
% ensure_contextID({_TokenTag, _Line, Text}) ->
%     case Text of
%         "*"  -> ?megaco_all_context_id;
%         "-"  -> ?megaco_null_context_id;
%         "\$" -> ?megaco_choose_context_id;
%         Int  -> ensure_uint32(Int)
%     end.

ensure_domainAddress([{_T, _L, _A} = Addr0], Port) ->
    Addr = ensure_ip4addr(Addr0), 
    {ip4Address, #'IP4Address'{address = Addr, portNumber = Port}};
ensure_domainAddress([colon,colon], Port) ->
    Addr = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    {ip6Address, #'IP6Address'{address = Addr, portNumber = Port}};
ensure_domainAddress(Addr0, Port) ->
    Addr = ensure_ip6addr(Addr0),
    {ip6Address, #'IP6Address'{address = Addr, portNumber = Port}}.


ensure_ip4addr({TokenTag, Line, Addr}) ->
    case string:tokens(Addr, [$.]) of
	[T1, T2, T3, T4] ->
	    A1 = ensure_uint({TokenTag, Line, T1}, 0, 255),
	    A2 = ensure_uint({TokenTag, Line, T2}, 0, 255),
	    A3 = ensure_uint({TokenTag, Line, T3}, 0, 255),
	    A4 = ensure_uint({TokenTag, Line, T4}, 0, 255),
	    [A1, A2, A3, A4];
	_ ->
	    return_error(Line, {bad_IP4address, Addr})
    end.


ensure_ip6addr([colon,colon|T]) ->
    [H1|T1] = lists:reverse(T),
    case do_ensure_ip6addr(T1, true, [ensure_hex4_or_ip4addr(H1)], 1) of
	{true, A} when length(A) == 16 ->
	    A;
	{true, B} when length(B) < 16 ->
	    lists:duplicate(16 - length(B), 0) ++ B;
	{true, C} ->
	    throw({error, {?MODULE, {bad_mid_ip6addr_length, C}}})
    end;
ensure_ip6addr(L) ->
    case lists:reverse(L) of
	[colon, colon| T] ->
	    case do_ensure_ip6addr(T, true, [], 1) of
		{true, A} when length(A) == 16 ->
		    A;
		{true, B} when length(B) < 16 ->
		    B ++ lists:duplicate(16 - length(B), 0);
		{true, C} ->
		    throw({error, {?MODULE, {bad_mid_ip6addr_length, C}}})
	    end;
	[H|L1] -> % A (last element) could be an ip4 address
	    case do_ensure_ip6addr(L1,false,[ensure_hex4_or_ip4addr(H)],1) of
		{false, A} when length(A) == 16 -> 
		    A;
		%% allow a pad even if the address is full (i.e. 16)
		{true, B} when length(B) =< 17 -> 
		    do_ensure_ip6addr_padding(B, 0);
		{Pad, C} ->
		    throw({error, {?MODULE, {bad_mid_ip6addr_length, Pad, C}}})
	    end

    end.


do_ensure_ip6addr([], Pad, Acc, _) ->
    {Pad, lists:flatten(Acc)};
do_ensure_ip6addr([colon,colon|T], false, Acc, Line) ->
    do_ensure_ip6addr(T, true, [pad|Acc], Line);
do_ensure_ip6addr([colon,colon|T], true, Acc, Line) ->
    return_error(Line, {bad_mid_duplicate_padding, T, Acc});
do_ensure_ip6addr([colon|T], Pad, Acc, Line) ->
    do_ensure_ip6addr(T, Pad, Acc, Line);
do_ensure_ip6addr([{_, Line, _} = A|T], Pad, Acc, _) ->
    do_ensure_ip6addr(T, Pad, [ensure_hex4(A)|Acc], Line).

do_ensure_ip6addr_padding([], _) ->
    [];
do_ensure_ip6addr_padding([pad|T], N) ->
    lists:duplicate(16 - (N + length(T)), 0) ++ T;
do_ensure_ip6addr_padding([H|T], N) ->
    [H|do_ensure_ip6addr_padding(T, N+1)].

ensure_hex4_or_ip4addr({TokenTag, Line, Addr} = V) ->
    case string:tokens(Addr, [$.]) of
	[T1, T2, T3, T4] ->
	    A1 = ensure_uint({TokenTag, Line, T1}, 0, 255),
	    A2 = ensure_uint({TokenTag, Line, T2}, 0, 255),
	    A3 = ensure_uint({TokenTag, Line, T3}, 0, 255),
	    A4 = ensure_uint({TokenTag, Line, T4}, 0, 255),
	    [A1, A2, A3, A4];
	_ ->
	    ensure_hex4(V)
	    %% 	    %% BMK BMK BMK 
	    %% 	    %% Here we should test for hexseq
	    %% 	    return_error(Line, {bad_IP4address, Addr})
    end.

ensure_hex4({_TokenTag, Line, Hex4}) 
  when (length(Hex4) =< 4) andalso (length(Hex4) > 0) ->
    case (catch do_ensure_hex4(Hex4)) of
	IL when is_list(IL) andalso (length(IL) =:= 2) ->
	    IL;
	Error ->
	    return_error(Line, {bad_hex4, Hex4, Error})
    end.

do_ensure_hex4([_H1, _H2, _H3, _H4] = H) ->
    hex_to_int(H, []);
do_ensure_hex4([H2, H3, H4]) ->
    hex_to_int([$0, H2, H3, H4], []);
do_ensure_hex4([H3, H4]) ->
    hex_to_int([$0, $0, H3, H4], []);
do_ensure_hex4([H4]) ->
    hex_to_int([$0, $0, $0, H4], []).

ensure_domainName({_TokenTag, _Line, Name}, Port) ->
    %% BUGBUG: validate name
    {domainName, #'DomainName'{name = Name, portNumber = Port}}.

%% extensionParameter= "X"  ("-" / "+") 1*6(ALPHA / DIGIT)
% ensure_extensionParameter({_TokenTag, Line, Text}) ->
%     case Text of
%         [X, S | _Chars] ->
%             if
%                 X /= $X, X /= $x,
%                 S /= $+, S /= $- ->
%                     return_error(Line, {bad_extension_parameter, Text});
%                 true ->
%                     {extension_parameter, Text}
%             end;
%         _ ->
%             return_error(Line, {bad_extension_parameter, Text})
%     end.

ensure_message(MegacopToken,  MID) ->
    #'ServiceChangeProfile'{profileName = Name,
			    version     = Version} = 
	ensure_profile(MegacopToken),
    case Name of
        "megaco" ->
            #'Message'{version = Version, mId = MID};
        [$!]  ->
            #'Message'{version = Version, mId = MID}
    end.


%% modemType         = (V32bisToken / V22bisToken / V18Token / 
%%                      V22Token / V32Token / V34Token / V90Token / 
%%                      V91Token / SynchISDNToken / extensionParameter)
% ensure_modemType({_TokenTag, _Line, Text} = Token) ->
%     case Text of
%         "v32b"      -> v32bis;
%         "v22b"      -> v22bis;
%         "v18"       -> v18;
%         "v22"       -> v22;
%         "v32"       -> v32;
%         "v34"       -> v34;
%         "v90"       -> v90;
%         "v91"       -> v91;
%         "synchisdn" -> synchISDN;
%         "sn"        -> synchISDN;
%         [$x | _]               -> ensure_extensionParameter(Token)
%     end.

%% An mtp address is five octets long
ensure_mtpAddress({_TokenTag, _Line, Addr}) ->
    %% BUGBUG: validate address
    {mtpAddress, Addr}.

%% MuxType = ( H221Token / H223Token / H226Token / V76Token / extensionParameter )
% ensure_muxType({_TokenTag, _Line, Text} = Token) ->
%     case Text of
%         "h221"  -> h221;
%         "h223"  -> h223;
%         "h226"  -> h226;
%         "v76"   -> v76;
%         "nx64k" -> nx64k; % v2
%         [$x | _]          -> ensure_extensionParameter(Token)
%     end.

%% packagesItem      = NAME "-" UINT16
%% NAME              = ALPHA *63(ALPHA / DIGIT / "_" )
% ensure_packagesItem({TokenTag, Line, Text}) ->
%     case string:tokens(Text, [$-]) of
%         [Name, Version] ->
%             #'PackagesItem'{packageName    = ensure_NAME({TokenTag, Line, Name}),
%                             packageVersion = ensure_uint({TokenTag, Line, Version}, 0, 99)};
%         _ ->
%             return_error(Line, {bad_PackagesItem, Text})
%     end.

%% pkgdName          =  (PackageName / "*")  SLASH  (ItemID / "*" )
%% PackageName       = NAME
%% ItemID            = NAME
% ensure_pkgdName({TokenTag, Line, Text}) ->
%     case string:tokens(Text, [$/]) of
%         [Name, Item] ->
%             ensure_name_or_star({TokenTag, Line, Name}),
%             ensure_name_or_star({TokenTag, Line, Item}),
%             Text;
%         _ ->
%             return_error(Line, {bad_pkgdName, Text})
%     end.

% ensure_name_or_star({_, _, Name}) when Name == "*" ->
%     Name;
% ensure_name_or_star(Name) ->
%     ensure_NAME(Name).



%% v2 - start

% merge_indAudMediaDescriptor({termStateDescr, Val}) ->
%     #'IndAudMediaDescriptor'{termStateDescr = Val};
% merge_indAudMediaDescriptor({streamParm, Val}) ->
%     #'IndAudMediaDescriptor'{streams = {oneStream, Val}};
% merge_indAudMediaDescriptor({streamDescr, Val}) ->
%     #'IndAudMediaDescriptor'{streams = {multiStream, [Val]}}.

% merge_indAudLocalControlDescriptor(Parms) ->
%     do_merge_indAudLocalControlDescriptor(Parms, 
% 					  #'IndAudLocalControlDescriptor'{}).
					  
% do_merge_indAudLocalControlDescriptor([Parm | Parms], Desc) ->
%     case Parm of
% 	modeToken when Desc#'IndAudLocalControlDescriptor'.streamMode == asn1_NOVALUE ->
% 	    Desc2 = Desc#'IndAudLocalControlDescriptor'{streamMode = 'NULL'},
% 	    do_merge_indAudLocalControlDescriptor(Parms, Desc2);
% 	reservedGroupToken when Desc#'IndAudLocalControlDescriptor'.reserveGroup == asn1_NOVALUE ->
% 	    Desc2 = Desc#'IndAudLocalControlDescriptor'{reserveGroup = 'NULL'},
% 	    do_merge_indAudLocalControlDescriptor(Parms, Desc2);
% 	reservedValueToken when Desc#'IndAudLocalControlDescriptor'.reserveValue == asn1_NOVALUE ->
% 	    Desc2 = Desc#'IndAudLocalControlDescriptor'{reserveValue = 'NULL'},
% 	    do_merge_indAudLocalControlDescriptor(Parms, Desc2);
% 	{pkgdName, Val} when Desc#'IndAudLocalControlDescriptor'.propertyParms == asn1_NOVALUE ->
% 	    PropParms = [#'IndAudPropertyParm'{name = Val}],
% 	    Desc2 = Desc#'IndAudLocalControlDescriptor'{propertyParms = PropParms},
% 	    do_merge_indAudLocalControlDescriptor(Parms, Desc2);
% 	{pkgdName, Val} when list(Desc#'IndAudLocalControlDescriptor'.propertyParms) ->
% 	    PropParms = Desc#'IndAudLocalControlDescriptor'.propertyParms,
% 	    PropParms2 = [#'IndAudPropertyParm'{name = Val} | PropParms],
% 	    Desc2 = Desc#'IndAudLocalControlDescriptor'{propertyParms = PropParms2},
% 	    do_merge_indAudLocalControlDescriptor(Parms, Desc2)
%     end;
% do_merge_indAudLocalControlDescriptor([], Desc) ->
%     case Desc#'IndAudLocalControlDescriptor'.propertyParms of
% 	[_ | _] = PropParms -> % List has more then one element
% 	    PropParms2= lists:reverse(PropParms),
% 	    Desc#'IndAudLocalControlDescriptor'{propertyParms = PropParms2};
% 	_ ->
% 	    Desc
%     end.

% ensure_indAudLocalParm(Token) ->
%     case Token of
% 	{safeToken, _Line, "mode"}          -> modeToken;
% 	{safeToken, _Line, "mo"}            -> modeToken;
% 	{safeToken, _Line, "reservedgroup"} -> reservedGroupToken;
% 	{safeToken, _Line, "rg"}            -> reservedGroupToken;
% 	{safeToken, _Line, "reservedvalue"} -> reservedValueToken;
% 	{safeToken, _Line, "rv"}            -> reservedValueToken;
% 	PkgdName                            -> {pkgdName,
% 						ensure_pkgdName(PkgdName)}
%     end.

% merge_indAudTerminationStateDescriptor({pkgdName, Val}) ->
%     PropParm = #'IndAudPropertyParm'{name = Val},
%     #'IndAudTerminationStateDescriptor'{propertyParms = [PropParm]};
% merge_indAudTerminationStateDescriptor(serviceStatesToken) ->
%     #'IndAudTerminationStateDescriptor'{serviceState = 'NULL'};
% merge_indAudTerminationStateDescriptor(bufferToken) ->
%     #'IndAudTerminationStateDescriptor'{eventBufferControl = 'NULL'}.


% merge_indAudEventBufferDescriptor(EventName, SpecParams) ->
%     IAEBD = #'IndAudEventBufferDescriptor'{eventName = EventName},
%     do_merge_indAudEventBufferDescriptor(SpecParams, IAEBD).

% do_merge_indAudEventBufferDescriptor(asn1_NOVALUE, IAEBD) ->
%     IAEBD;
% do_merge_indAudEventBufferDescriptor({streamID, StreamID}, IAEBD) ->
%     IAEBD#'IndAudEventBufferDescriptor'{streamID = StreamID};
% do_merge_indAudEventBufferDescriptor({eventParameterName, _Name} = EPN, 
% 				     IAEBD) ->
%     %% BUGBUG BUGBUG BUGBUG 
%     %% This is an ugly hack to allow the eventParamName which only
%     %% exists in the text encoding...
%     IAEBD#'IndAudEventBufferDescriptor'{streamID = EPN}.


% ensure_indAudSignalListParm(SIG) when record(SIG, 'Signal') ->
%     ensure_indAudSignal(SIG).

% ensure_indAudSignal(#'Signal'{signalName       = SignalName,
% 			      streamID         = asn1_NOVALUE, 
% 			      sigType          = asn1_NOVALUE, 
% 			      duration         = asn1_NOVALUE, 
% 			      notifyCompletion = asn1_NOVALUE, 
% 			      keepActive       = asn1_NOVALUE, 
% 			      sigParList       = []}) ->
%     #'IndAudSignal'{signalName = SignalName}.
    

% ensure_IADMD({_TokenTag, _Line, 
% 	      #'DigitMapDescriptor'{digitMapName  = Name,
% 				    digitMapValue = asn1_NOVALUE}}) ->
%     #'IndAudDigitMapDescriptor'{digitMapName = Name}.


% merge_indAudPackagesDescriptor(#'PackagesItem'{packageName    = N,
% 					       packageVersion = V}) ->
%     #'IndAudPackagesDescriptor'{packageName    = N,
% 				packageVersion = V}.


% ensure_indAudTerminationStateParm(Token) ->
%     case Token of
% 	{safeToken, _Line, "servicestates"} -> serviceStatesToken;
% 	{safeToken, _Line, "si"}            -> serviceStatesToken;
% 	{safeToken, _Line, "buffer"}        -> bufferToken;
% 	{safeToken, _Line, "bf"}            -> bufferToken;
% 	PkgdName                            -> {pkgdName,
% 						ensure_pkgdName(PkgdName)}
%     end.


% %% Types modified by v2:

% merge_auditDescriptor([]) ->
%     #'AuditDescriptor'{};
% merge_auditDescriptor(Tokens) when list(Tokens) ->
%     case lists:keysearch(terminationAudit, 1, Tokens) of
% 	{value, {terminationAudit, TA}} ->
% 	    case lists:keydelete(terminationAudit, 1, Tokens) of
% 		[] ->
% 		    #'AuditDescriptor'{auditPropertyToken = TA};
% 		AuditTokens ->
% 		    #'AuditDescriptor'{auditToken         = AuditTokens,
% 				       auditPropertyToken = TA}
% 	    end;
% 	false ->
% 	    #'AuditDescriptor'{auditToken = Tokens}
%     end;
% merge_auditDescriptor(_) ->
%     #'AuditDescriptor'{}.


% %% v2 - end



% merge_ServiceChangeParm(Parms) ->
%     Required = [serviceChangeReason, serviceChangeMethod],
%     merge_ServiceChangeParm(Parms, #'ServiceChangeParm'{}, Required).

% merge_ServiceChangeParm([], SCP, []) ->
%     SCP;

% merge_ServiceChangeParm([], _SCP, Required) ->
%     exit({missing_required_serviceChangeParm, Required});

% merge_ServiceChangeParm([{address, Val}|Parms], SCP0, Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeAddress == asn1_NOVALUE,
%        SCP0#'ServiceChangeParm'.serviceChangeMgcId == asn1_NOVALUE ->
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeAddress = Val},
%     merge_ServiceChangeParm(Parms, SCP, Req);
% merge_ServiceChangeParm([{address, Val}|_Parms], SCP0, _Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeAddress == asn1_NOVALUE ->
%     MgcId = SCP0#'ServiceChangeParm'.serviceChangeMgcId,
%     exit({not_both_address_mgcid_serviceChangeParm, Val, MgcId});

% merge_ServiceChangeParm([{mgc_id, Val}|Parms], SCP0, Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeMgcId == asn1_NOVALUE,
%        SCP0#'ServiceChangeParm'.serviceChangeAddress == asn1_NOVALUE ->
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeMgcId = Val},
%     merge_ServiceChangeParm(Parms, SCP, Req);
% merge_ServiceChangeParm([{mgc_id, Val}|_Parms], SCP0, _Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeMgcId == asn1_NOVALUE ->
%     Addr = SCP0#'ServiceChangeParm'.serviceChangeAddress,
%     exit({not_both_address_mgcid_serviceChangeParm, Val, Addr});

% merge_ServiceChangeParm([{profile, Val}|Parms], SCP0, Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeProfile == asn1_NOVALUE ->
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeProfile = Val},
%     merge_ServiceChangeParm(Parms, SCP, Req);

% merge_ServiceChangeParm([{version, Val}|Parms], SCP0, Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeVersion == asn1_NOVALUE ->
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeVersion = Val},
%     merge_ServiceChangeParm(Parms, SCP, Req);

% %% REQUIRED (i.e. no default value)
% merge_ServiceChangeParm([{reason, Val}|Parms], SCP0, Req0) 
%   when SCP0#'ServiceChangeParm'.serviceChangeReason == undefined ->
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeReason = Val},
%     Req = lists:delete(serviceChangeReason, Req0),
%     merge_ServiceChangeParm(Parms, SCP, Req);

% merge_ServiceChangeParm([{delay, Val}|Parms], SCP0, Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeDelay == asn1_NOVALUE ->
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeDelay = Val},
%     merge_ServiceChangeParm(Parms, SCP, Req);

% %% REQUIRED (i.e. no default value)
% merge_ServiceChangeParm([{method, Val}|Parms], SCP0, Req0) 
%   when SCP0#'ServiceChangeParm'.serviceChangeMethod == undefined ->
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeMethod = Val},
%     Req = lists:delete(serviceChangeMethod, Req0),
%     merge_ServiceChangeParm(Parms, SCP, Req);

% merge_ServiceChangeParm([{time_stamp, Val}|Parms], SCP0, Req) 
%   when SCP0#'ServiceChangeParm'.timeStamp == asn1_NOVALUE ->
%     SCP = SCP0#'ServiceChangeParm'{timeStamp = Val},
%     merge_ServiceChangeParm(Parms, SCP, Req);

% merge_ServiceChangeParm([{extension, _Val}|Parms], SCP0, Req) ->
%     merge_ServiceChangeParm(Parms, SCP0, Req);

% merge_ServiceChangeParm([{audit_item, Val}|Parms], SCP0, Req) 
%   when SCP0#'ServiceChangeParm'.serviceChangeInfo == asn1_NOVALUE ->
%     SCI = #'AuditDescriptor'{auditToken = [Val]},
%     SCP = SCP0#'ServiceChangeParm'{serviceChangeInfo = SCI},
%     merge_ServiceChangeParm(Parms, SCP, Req);

% merge_ServiceChangeParm([{Tag, Val}|_Parms], SCP, _Req) ->
%     Val2 = 
% 	case Tag of
% 	    address -> 
% 		SCP#'ServiceChangeParm'.serviceChangeAddress;
% 	    mgc_id     -> 
% 		SCP#'ServiceChangeParm'.serviceChangeMgcId;
% 	    profile    -> 
% 		SCP#'ServiceChangeParm'.serviceChangeProfile;
% 	    version    -> 
% 		SCP#'ServiceChangeParm'.serviceChangeVersion;
% 	    reason     -> 
% 		SCP#'ServiceChangeParm'.serviceChangeReason;
% 	    delay      -> 
% 		SCP#'ServiceChangeParm'.serviceChangeDelay;
% 	    method     -> 
% 		SCP#'ServiceChangeParm'.serviceChangeMethod;
% 	    time_stamp -> 
% 		SCP#'ServiceChangeParm'.timeStamp;
% 	    audit_item -> 
% 		SCP#'ServiceChangeParm'.serviceChangeInfo
% 	end,
%     exit({at_most_once_serviceChangeParm, {Tag, Val, Val2}}).


% merge_ServiceChangeResParm(Parms) ->
%     merge_ServiceChangeResParm(Parms, #'ServiceChangeResParm'{}).

% merge_ServiceChangeResParm([], SCRP) ->
%     SCRP;
% merge_ServiceChangeResParm([{address, Val}|Parms], SCRP0) 
%   when SCRP0#'ServiceChangeResParm'.serviceChangeAddress == asn1_NOVALUE,
%        SCRP0#'ServiceChangeResParm'.serviceChangeMgcId == asn1_NOVALUE ->
%     SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeAddress = Val},
%     merge_ServiceChangeResParm(Parms, SCRP);
% merge_ServiceChangeResParm([{address, Val}|_Parms], SCRP0) 
%   when SCRP0#'ServiceChangeResParm'.serviceChangeAddress == asn1_NOVALUE ->
%     MgcId = SCRP0#'ServiceChangeResParm'.serviceChangeMgcId,
%     exit({not_both_address_mgcid_servChgReplyParm, Val, MgcId});

% merge_ServiceChangeResParm([{mgc_id, Val}|Parms], SCRP0)
%   when SCRP0#'ServiceChangeResParm'.serviceChangeMgcId == asn1_NOVALUE,
%        SCRP0#'ServiceChangeResParm'.serviceChangeAddress == asn1_NOVALUE -> 
%     SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeMgcId = Val},
%     merge_ServiceChangeResParm(Parms, SCRP);
% merge_ServiceChangeResParm([{mgc_id, Val}|_Parms], SCRP0)
%   when SCRP0#'ServiceChangeResParm'.serviceChangeMgcId == asn1_NOVALUE -> 
%     Addr = SCRP0#'ServiceChangeResParm'.serviceChangeAddress,
%     exit({not_both_address_mgcid_servChgReplyParm, Val, Addr});

% merge_ServiceChangeResParm([{profile, Val}|Parms], SCRP0)
%   when SCRP0#'ServiceChangeResParm'.serviceChangeProfile == asn1_NOVALUE ->
%     SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeProfile = Val},
%     merge_ServiceChangeResParm(Parms, SCRP);

% merge_ServiceChangeResParm([{version, Val}|Parms], SCRP0)
%   when SCRP0#'ServiceChangeResParm'.serviceChangeVersion == asn1_NOVALUE ->
%     SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeVersion = Val},
%     merge_ServiceChangeResParm(Parms, SCRP);

% merge_ServiceChangeResParm([{time_stamp, Val}|Parms], SCRP0)
%   when SCRP0#'ServiceChangeResParm'.timeStamp == asn1_NOVALUE ->
%     SCRP = SCRP0#'ServiceChangeResParm'{timeStamp = Val},
%     merge_ServiceChangeResParm(Parms, SCRP);

% merge_ServiceChangeResParm([{Tag, Val}|_Parms], SCRP) ->
%     Val2 = 
% 	case Tag of
% 	    address    -> SCRP#'ServiceChangeResParm'.serviceChangeAddress;
% 	    mgc_id     -> SCRP#'ServiceChangeResParm'.serviceChangeMgcId;
% 	    profile    -> SCRP#'ServiceChangeResParm'.serviceChangeProfile;
% 	    version    -> SCRP#'ServiceChangeResParm'.serviceChangeVersion;
% 	    time_stamp -> SCRP#'ServiceChangeResParm'.timeStamp
% 	end,
%     exit({at_most_once_servChgReplyParm, {Tag, Val, Val2}}).


% ensure_serviceChangeMethod({safeToken, _Line, "fl"}) ->
%     failover;
% ensure_serviceChangeMethod({safeToken, _Line, "failover"}) ->
%     failover;
% ensure_serviceChangeMethod({safeToken, _Line, "fo"}) -> 
%     forced;
% ensure_serviceChangeMethod({safeToken, _Line, "forced"}) ->
%     forced;
% ensure_serviceChangeMethod({safeToken, _Line, "gr"}) ->
%     graceful;
% ensure_serviceChangeMethod({safeToken, _Line, "graceful"}) ->
%     graceful;
% ensure_serviceChangeMethod({safeToken, _Line, "rs"}) ->
%     restart;
% ensure_serviceChangeMethod({safeToken, _Line, "restart"}) ->
%     restart;
% ensure_serviceChangeMethod({safeToken, _Line, "dc"}) ->
%     disconnected;
% ensure_serviceChangeMethod({safeToken, _Line, "disconnected"}) ->
%     disconnected;
% ensure_serviceChangeMethod({safeToken, _Line, "ho"}) ->
%     handOff;
% ensure_serviceChangeMethod({safeToken, _Line, "handoff"}) ->
%     handOff;
% ensure_serviceChangeMethod({safeToken, Line, Text}) ->
%     return_error(Line, {bad_serviceChangeMethod, Text}).


ensure_profile({_TokenTag, Line, Text}) ->
    case string:tokens(Text, [$/]) of
        [Name, Version] ->
            Version2 = ensure_version(Version),
            #'ServiceChangeProfile'{profileName = Name, version = Version2};
        _ ->
            return_error(Line, {bad_profile, Text})
    end.

ensure_version(Version) ->
    ensure_uint(Version, 0, 99).

% merge_signalRequest(SignalName, PropertyParms) ->
%     Sig = #'Signal'{signalName = SignalName},
%     SPL = [],
%     do_merge_signalRequest(Sig, PropertyParms, SPL).

% do_merge_signalRequest(Sig, [H | T], SPL) ->
%     case H of
%         {stream, StreamId} when Sig#'Signal'.streamID == asn1_NOVALUE ->
%             do_merge_signalRequest(Sig#'Signal'{streamID = StreamId}, T, SPL);
%         {signal_type, SigType} when Sig#'Signal'.sigType == asn1_NOVALUE ->
%             do_merge_signalRequest(Sig#'Signal'{sigType = SigType}, T, SPL);
%         {duration, Duration} when Sig#'Signal'.duration == asn1_NOVALUE ->
%             do_merge_signalRequest(Sig#'Signal'{duration = Duration}, T, SPL);
%         {notify_completion, NC} when Sig#'Signal'.notifyCompletion == asn1_NOVALUE ->
%             do_merge_signalRequest(Sig#'Signal'{notifyCompletion = NC}, T, SPL);
%         keepActive when Sig#'Signal'.keepActive == asn1_NOVALUE->
%             do_merge_signalRequest(Sig#'Signal'{keepActive = true}, T, SPL);
%         {other, Name, PP} ->
%             SP = #'SigParameter'{sigParameterName = Name, 
% 				 value            = PP#'PropertyParm'.value,
% 				 extraInfo        = PP#'PropertyParm'.extraInfo},
%             do_merge_signalRequest(Sig, T, [SP | SPL]);
%         _ ->
%             return_error(0, {bad_sigParm, H})
%     end;
% do_merge_signalRequest(Sig, [], SPL) ->
%     Sig#'Signal'{sigParList = lists:reverse(SPL)} .

% %% eventStream       = StreamToken EQUAL StreamID
% %% eventOther        = eventParameterName parmValue
% select_stream_or_other("st", #'PropertyParm'{value = [Value]}) ->
%     {stream, ensure_uint16(Value)};
% select_stream_or_other("st", Value) ->
%     {stream, ensure_uint16(Value)};
% select_stream_or_other("stream", #'PropertyParm'{value = [Value]}) ->
%     {stream, ensure_uint16(Value)};
% select_stream_or_other("stream", Value) ->
%     {stream, ensure_uint16(Value)};
% select_stream_or_other(Name, #'PropertyParm'{value = Value}) ->
%     EP = #'EventParameter'{eventParameterName = Name,
%  			   value              = Value},
%     {other, EP}.

% ensure_eventDM({_TokenTag, Line, DMD}) 
%   when record(DMD, 'DigitMapDescriptor') ->
%     Name = DMD#'DigitMapDescriptor'.digitMapName,
%     Val  = DMD#'DigitMapDescriptor'.digitMapValue,
%     if
%         Name  == asn1_NOVALUE, Val /= asn1_NOVALUE ->
% 	    {'DigitMapValue', Start, Short, Long, Duration, Body} = Val,
% 	    DMV = #'DigitMapValue'{startTimer    = Start, 
% 				   shortTimer    = Short, 
% 				   longTimer     = Long, 
% 				   digitMapBody  = Body,
% 				   durationTimer = Duration},
%             {eventDM, {digitMapValue, DMV}};
%         Name  /= asn1_NOVALUE, Val == asn1_NOVALUE ->
%             {eventDM, {digitMapName, Name}};
%         true ->
%             return_error(Line, {bad_eventDM, DMD})
%     end.
    
% ensure_DMD({_TokenTag, _Line, DMD}) 
%   when record(DMD, 'DigitMapDescriptor') ->
%     Val2 = 
% 	case DMD#'DigitMapDescriptor'.digitMapValue of
% 	    %% Note that the values of the digitMapBody and durationTimers
% 	    %% are swapped by the scanner (this is done because of a 
% 	    %% problem in the flex scanner).
% 	    #'DigitMapValue'{durationTimer = Body,
% 			     digitMapBody  = Duration} = DMV ->
% 		%% Convert to version 1 DigitMapValue
% 		DMV#'DigitMapValue'{digitMapBody  = Body,
% 				    durationTimer = Duration};
% 	    Other ->
% 		Other
% 	end,
%     DMD#'DigitMapDescriptor'{digitMapValue = Val2}.


% merge_observed_event(ObservedEvents, EventName, TimeStamp) ->
%     StreamId = asn1_NOVALUE,
%     EPL = [],
%     do_merge_observed_event(ObservedEvents, EventName, TimeStamp, StreamId, EPL).

% do_merge_observed_event([{stream, StreamID} | T], EventName, TimeStamp, asn1_NOVALUE, EPL) ->
%     do_merge_observed_event(T, EventName, TimeStamp, StreamID, EPL);
% do_merge_observed_event([{other, PP} | T], EventName, TimeStamp, StreamID, EPL) ->
%     do_merge_observed_event(T, EventName, TimeStamp, StreamID, [PP | EPL]);
% do_merge_observed_event([], EventName, TimeStamp, StreamID, EPL) ->
%     #'ObservedEvent'{eventName    = EventName,
%                      timeNotation = TimeStamp,
%                      streamID     = StreamID,
%                      eventParList = lists:reverse(EPL)}.

% merge_eventSpec(OE) when record(OE, 'ObservedEvent'),
%                          OE#'ObservedEvent'.timeNotation == asn1_NOVALUE ->
%     #'EventSpec'{eventName     = OE#'ObservedEvent'.eventName,
%                  streamID      = OE#'ObservedEvent'.streamID,
%                  eventParList  = OE#'ObservedEvent'.eventParList};
% merge_eventSpec(OE) ->
%     return_error(0, {bad_event_spec, OE}).

% merge_eventParameters(Params) ->
%     StreamId = asn1_NOVALUE,
%     EPL      = [],
%     RA       = #'RequestedActions'{},
%     HasA     = no,
%     do_merge_eventParameters(Params, StreamId, EPL, RA, HasA) .
                                   
% do_merge_eventParameters([H | T], StreamId, EPL, RA, HasA) ->
%     case H of
%         keepActive when RA#'RequestedActions'.keepActive == asn1_NOVALUE ->
%             RA2 = RA#'RequestedActions'{keepActive = true},
%             do_merge_eventParameters(T, StreamId, EPL, RA2, yes);
%         {embed, SD, SED} when RA#'RequestedActions'.signalsDescriptor == asn1_NOVALUE ->
%             RA2 = RA#'RequestedActions'{signalsDescriptor = SD,
%                                           secondEvent       = SED},
%             do_merge_eventParameters(T, StreamId, EPL, RA2, yes);
%         {eventDM, DM} when RA#'RequestedActions'.eventDM == asn1_NOVALUE ->
%             RA2 = RA#'RequestedActions'{eventDM = DM},
%             do_merge_eventParameters(T, StreamId, EPL, RA2, yes);
%         {stream, NewStreamId} when StreamId == asn1_NOVALUE ->
%             do_merge_eventParameters(T, NewStreamId, EPL, RA, HasA);
%         {other, PP} when record(PP, 'PropertyParm') ->
%             EP = #'EventParameter'{eventParameterName = PP#'PropertyParm'.name,
%                                    value              = PP#'PropertyParm'.value,
% 				   extraInfo          = PP#'PropertyParm'.extraInfo},
%             do_merge_eventParameters(T, StreamId, [EP | EPL], RA, HasA);
%         {other, EP} when record(EP, 'EventParameter') ->
%             do_merge_eventParameters(T, StreamId, [EP | EPL], RA, HasA);
%         _ ->
%             return_error(0, {bad_eventParameter, H})
%     end;
% do_merge_eventParameters([], StreamId, EPL, RA, yes) ->
%     #'RequestedEvent'{streamID    = StreamId,
%                       eventAction = RA, 
%                       evParList   = lists:reverse(EPL)};
% do_merge_eventParameters([], StreamId, EPL, _RA, no) ->
%     #'RequestedEvent'{streamID    = StreamId,
%                       eventAction = asn1_NOVALUE, 
%                       evParList   = lists:reverse(EPL)}.

% merge_secondEventParameters(Params) ->
%     StreamId = asn1_NOVALUE,
%     EPL      = [],
%     SRA      = #'SecondRequestedActions'{},
%     HasA     = no,
%     do_merge_secondEventParameters(Params, StreamId, EPL, SRA, HasA) .
                                   
% do_merge_secondEventParameters([H | T], StreamId, EPL, SRA, HasA) ->
%     case H of
%         keepActive when SRA#'SecondRequestedActions'.keepActive == asn1_NOVALUE ->
%             SRA2 = SRA#'SecondRequestedActions'{keepActive = true},
%             do_merge_secondEventParameters(T, StreamId, EPL, SRA2, yes);
%         {second_embed, SD} when SRA#'SecondRequestedActions'.signalsDescriptor == asn1_NOVALUE ->
%             SRA2 = SRA#'SecondRequestedActions'{signalsDescriptor = SD},
%             do_merge_secondEventParameters(T, StreamId, EPL, SRA2, yes);
%         {eventDM, DM} when SRA#'SecondRequestedActions'.eventDM == asn1_NOVALUE ->
%             SRA2 = SRA#'SecondRequestedActions'{eventDM = DM},
%             do_merge_secondEventParameters(T, StreamId, EPL, SRA2, yes);
%         {stream, NewStreamId} when StreamId == asn1_NOVALUE ->
%             do_merge_secondEventParameters(T, NewStreamId, EPL, SRA, HasA);
%         {other, PP} when record(PP, 'PropertyParm') ->
%             EP = #'EventParameter'{eventParameterName = PP#'PropertyParm'.name,
%                                    value              = PP#'PropertyParm'.value,
% 				   extraInfo          = PP#'PropertyParm'.extraInfo},
%             do_merge_secondEventParameters(T, StreamId, [EP | EPL], SRA, HasA);
%         {other, EP} when record(EP, 'EventParameter') ->
%             do_merge_secondEventParameters(T, StreamId, [EP | EPL], SRA, HasA);
%         _ ->
%             return_error(0, {bad_secondEventParameter, H})
%     end;
% do_merge_secondEventParameters([], StreamId, EPL, SRA, yes) ->
%     #'SecondRequestedEvent'{streamID    = StreamId,
%                             eventAction = SRA, 
%                             evParList   = lists:reverse(EPL)};
% do_merge_secondEventParameters([], StreamId, EPL, _SRA, no) ->
%     #'SecondRequestedEvent'{streamID    = StreamId,
%                             eventAction = asn1_NOVALUE, 
%                             evParList   = lists:reverse(EPL)}.

% %% terminationID     = "ROOT" / pathName / "$" / "*"
% %% Total length of pathName must not exceed 64 chars.
% %% pathName          = ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" )
% %%                     ["@" pathDomainName ]
% %% ABNF allows two or more consecutive "." although it is meaningless
% %% in a path domain name.
% %% pathDomainName    = (ALPHA / DIGIT / "*" )
% %%                        *63(ALPHA / DIGIT / "-" / "*" / ".")
% ensure_terminationID({safeToken, _Line, LowerText}) ->
%     %% terminationID     = "ROOT" / pathName / "$" / "*"
%     decode_term_id(LowerText, false, [], []).

% decode_term_id([H | T], Wild, Id, Component) ->
%     case H of
%         $/ -> decode_term_id(T, Wild, [lists:reverse(Component) | Id], []);
%         $* -> decode_term_id(T, true, Id, [?megaco_all    | Component]);
%         $$ -> decode_term_id(T, true, Id, [?megaco_choose | Component]);
%         _  -> decode_term_id(T, Wild, Id, [H | Component])
%     end;
% decode_term_id([], Wild, Id, Component) ->
%     Id2 = [lists:reverse(Component) | Id],
%     #megaco_term_id{contains_wildcards = Wild, id = lists:reverse(Id2)}.
            
ensure_pathName({_TokenTag, _Line, Text}) ->
    Text.  %% BUGBUG: ensure values

%% TimeStamp            = Date "T" Time ; per ISO 8601:1988
%% Date                 = 8(DIGIT) ; Date = yyyymmdd
%% Time                 = 8(DIGIT) ; Time = hhmmssss
% ensure_timeStamp({'TimeStampToken', Line, Text}) ->
%     case string:tokens(Text, [$T, $t]) of
%         [Date, Time] ->
%             #'TimeNotation'{date = Date, time = Time};
%         _ ->
%             return_error(Line, {bad_timeStamp, Text})
%     end.

% ensure_transactionID(TransId) ->
%     ensure_uint32(TransId).

% %% transactionAck       = transactionID / (transactionID "-" transactionID)
% ensure_transactionAck({safeToken, _Line, Text}) ->
%     case string:tokens(Text, [$-]) of
%         [Id] ->
%             #'TransactionAck'{firstAck = ensure_transactionID(Id)};
%         [Id, Id2] ->
%             #'TransactionAck'{firstAck = ensure_transactionID(Id),
% 			      lastAck  = ensure_transactionID(Id2)}
%     end.

% merge_action_requests(CtxId, Items) ->
%     CtxReq      = #'ContextRequest'{},
%     CtxAuditReq = asn1_NOVALUE, 
%     CmdReq      = [],
%     TopReq      = [],
%     do_merge_action_requests(CtxId, CtxReq, CtxAuditReq, CmdReq, TopReq, Items).

% do_merge_action_requests(CtxId, CtxReq, CtxAuditReq, CmdReq, TopReq, [H | T]) ->
%     case H of
%         _ when record(H, 'CommandRequest') ->
%             do_merge_action_requests(CtxId, CtxReq, CtxAuditReq, [H | CmdReq], TopReq, T);

%         {priority, Int} when CtxReq#'ContextRequest'.priority == asn1_NOVALUE ->
%             CtxReq2 = CtxReq#'ContextRequest'{priority = Int},
%             do_merge_action_requests(CtxId, CtxReq2, CtxAuditReq, CmdReq, 
% 				     TopReq, T);
%         {emergency, Bool} when CtxReq#'ContextRequest'.emergency == asn1_NOVALUE ->
%             CtxReq2 = CtxReq#'ContextRequest'{emergency = Bool},
%             do_merge_action_requests(CtxId, CtxReq2, CtxAuditReq, CmdReq, 
% 				     TopReq, T);
%         {topology, Desc} ->
%             TopReq2 = Desc ++ TopReq, %% OTP-4088
%             do_merge_action_requests(CtxId, CtxReq, CtxAuditReq, CmdReq, 
% 				     TopReq2, T);

% 	{contextAudit, CAs} ->
% 	    CtxAuditReq2 = merge_context_attr_audit_request(CtxAuditReq, CAs),
% 	    do_merge_action_requests(CtxId, CtxReq, CtxAuditReq2, CmdReq, 
% 				     TopReq, T)
	    
%     end;
% do_merge_action_requests(CtxId, CtxReq, CtxAuditReq, CmdReq, TopReq, []) ->
%     #'ActionRequest'{contextId           = CtxId,
%                      contextRequest      = strip_contextRequest(CtxReq, TopReq),
%                      contextAttrAuditReq = strip_contextAttrAuditRequest(CtxAuditReq),
%                      commandRequests     = lists:reverse(CmdReq)}.


% merge_context_attr_audit_request(asn1_NOVALUE, CAs) ->
%     merge_context_attr_audit_request(#'ContextAttrAuditRequest'{}, CAs);
% merge_context_attr_audit_request(CAAR, [H|T]) ->
%     CAAR2 = 
% 	case H of
% 	    priorityAudit when CAAR#'ContextAttrAuditRequest'.priority == asn1_NOVALUE ->
% 		CAAR#'ContextAttrAuditRequest'{priority = 'NULL'};

% 	    priorityAudit ->
% 		Prio = CAAR#'ContextAttrAuditRequest'.priority,
% 		exit({only_once, priorityAudit, Prio});

% 	    emergencyAudit when CAAR#'ContextAttrAuditRequest'.emergency == asn1_NOVALUE ->
% 		CAAR#'ContextAttrAuditRequest'{emergency = 'NULL'};

% 	    emergencyAudit ->
% 		Em = CAAR#'ContextAttrAuditRequest'.emergency,
% 		exit({only_once, emergencyAudit, Em});

% 	    topologyAudit when CAAR#'ContextAttrAuditRequest'.topology == asn1_NOVALUE ->
% 		CAAR#'ContextAttrAuditRequest'{topology = 'NULL'};
	    
% 	    topologyAudit ->
% 		Top = CAAR#'ContextAttrAuditRequest'.topology,
% 		exit({only_once, topologyAudit, Top})

%     end,
%     merge_context_attr_audit_request(CAAR2, T);
% merge_context_attr_audit_request(CAAR, []) ->
%     CAAR.

% %% OTP-5085: 
% %% In order to solve a problem in the parser, the error descriptor
% %% has been put last in the non-empty commandReplyList, if it is not 
% %% asn1_NOVALUE
% merge_action_reply(ReplyList) ->
%     CtxReq  = #'ContextRequest'{},
%     TopReq  = [],
%     CmdList = [],
%     case lists:reverse(ReplyList) of
% 	[ED|RL2] when record(ED, 'ErrorDescriptor') ->
% 	    AR = do_merge_action_reply(lists:reverse(RL2), 
% 				       CtxReq, TopReq, CmdList),
% 	    AR#'ActionReply'{errorDescriptor = ED};
% 	_ ->
% 	    do_merge_action_reply(ReplyList, CtxReq, TopReq, CmdList)
%     end.

% do_merge_action_reply([H | T], CtxReq, TopReq, CmdList) ->
%     case H of
%         {command, Cmd} ->
%             do_merge_action_reply(T, CtxReq, TopReq, [Cmd | CmdList]);
%         {context, Ctx} ->
%             case Ctx of
%                 {priority, Int} when CtxReq#'ContextRequest'.priority == asn1_NOVALUE ->
%                     CtxReq2 = CtxReq#'ContextRequest'{priority = Int},
%                     do_merge_action_reply(T, CtxReq2, TopReq, CmdList);
%                 {emergency, Bool} when CtxReq#'ContextRequest'.emergency == asn1_NOVALUE ->
%                     CtxReq2 = CtxReq#'ContextRequest'{emergency = Bool},
%                     do_merge_action_reply(T, CtxReq2, TopReq, CmdList);
%                 {topology, Desc} ->
%                     TopReq2 = Desc ++ TopReq, %% OTP-4088
%                     do_merge_action_reply(T, CtxReq, TopReq2, CmdList)
%             end
%     end;
% do_merge_action_reply([], CtxReq, TopReq, CmdList) ->
%     #'ActionReply'{contextReply = strip_contextRequest(CtxReq, TopReq),
%                    commandReply = lists:reverse(CmdList)}.

% strip_contextRequest(R, TopReq)
%   when R#'ContextRequest'.priority    == asn1_NOVALUE,
%        R#'ContextRequest'.emergency   == asn1_NOVALUE,
%        TopReq == [] ->
%     asn1_NOVALUE;
% strip_contextRequest(R, []) ->
%     R#'ContextRequest'{topologyReq = asn1_NOVALUE};
% strip_contextRequest(R, TopReq) ->
%     R#'ContextRequest'{topologyReq = TopReq}. %% OTP-4088


% strip_contextAttrAuditRequest(R)
%   when R#'ContextAttrAuditRequest'.priority  == asn1_NOVALUE,
%        R#'ContextAttrAuditRequest'.emergency == asn1_NOVALUE,
%        R#'ContextAttrAuditRequest'.topology  == asn1_NOVALUE ->
%     asn1_NOVALUE;
% strip_contextAttrAuditRequest(R) ->
%     R.

% make_commandRequest({CmdTag, {_TokenTag, _Line, Text}}, Cmd) ->
%     Req = #'CommandRequest'{command  = {CmdTag, Cmd}},
%     case Text of
%         [$w, $- | _] ->
%             Req#'CommandRequest'{wildcardReturn = 'NULL'};
%         [$o, $-, $w, $- | _] ->
%             Req#'CommandRequest'{optional = 'NULL', wildcardReturn = 'NULL'};
%         [$o, $- | _] ->
%             Req#'CommandRequest'{optional = 'NULL'};
%         _ -> 
%             Req
%     end.

% merge_terminationAudit(AuditReturnParameters) ->
%     lists:reverse(do_merge_terminationAudit(AuditReturnParameters, [], [])).

% do_merge_terminationAudit([H| T], ARPs, AuditItems) ->
%     case H of
% 	{auditReturnItem, AuditItem} ->
% 	    do_merge_terminationAudit(T, ARPs, [AuditItem | AuditItems]);
% 	AuditReturnParameter ->
% 	    do_merge_terminationAudit(T, [AuditReturnParameter | ARPs], AuditItems)
%     end;
% do_merge_terminationAudit([], AuditReturnParameters, []) ->
%     AuditReturnParameters;
% do_merge_terminationAudit([], AuditReturnParameters, AuditItems) ->
%     AuditDescriptor = #'AuditDescriptor'{auditToken = AuditItems},
%     AuditReturnParameter = {emptyDescriptors, AuditDescriptor},
%     [AuditReturnParameter | AuditReturnParameters].
        
% merge_mediaDescriptor(MediaParms) ->
%     do_merge_mediaDescriptor(MediaParms, asn1_NOVALUE, [], []).

% do_merge_mediaDescriptor([H | T], TS, One, Multi) ->
%     case H of
%         {streamParm, Parm} when Multi == [] ->
%             do_merge_mediaDescriptor(T, TS, [Parm | One], Multi);
%         {streamDescriptor, Desc} when One == [] ->
%             do_merge_mediaDescriptor(T, TS, One, [Desc | Multi]);
%         {termState, TS2} when TS  == asn1_NOVALUE ->
%             do_merge_mediaDescriptor(T, TS2, One, Multi);
%         _ ->
%             return_error(0, {bad_merge_mediaDescriptor, [H, TS, One, Multi]})
%     end;
% do_merge_mediaDescriptor([], TS, One, Multi) ->
%     if
% 	One == [], Multi == [] ->
% 	    #'MediaDescriptor'{streams = asn1_NOVALUE,
% 			       termStateDescr = TS};
% 	One /= [], Multi == [] ->
% 	    #'MediaDescriptor'{streams = {oneStream, merge_streamParms(One)},
% 			       termStateDescr = TS};
% 	One == [], Multi /= [] ->
% 	    #'MediaDescriptor'{streams = {multiStream, lists:reverse(Multi)},
% 			       termStateDescr = TS}
%     end.
  
% merge_streamParms(TaggedStreamParms) ->
%     SP = #'StreamParms'{},
%     do_merge_streamParms(TaggedStreamParms, SP).

% do_merge_streamParms([{Tag, D} | T] = All, SP) ->
%     case Tag of
%         local when SP#'StreamParms'.localDescriptor  == asn1_NOVALUE ->
%             do_merge_streamParms(T, SP#'StreamParms'{localDescriptor = D});
%         remote when SP#'StreamParms'.remoteDescriptor == asn1_NOVALUE ->
%             do_merge_streamParms(T, SP#'StreamParms'{remoteDescriptor = D});
%         control ->
%             LCD = 
%                 case SP#'StreamParms'.localControlDescriptor of
%                     asn1_NOVALUE ->
%                         #'LocalControlDescriptor'{propertyParms = []};
%                     PrevLCD ->
%                         PrevLCD
%                 end,
%             LCD2 = do_merge_control_streamParms(D, LCD),
%             do_merge_streamParms(T, SP#'StreamParms'{localControlDescriptor = LCD2});
%         _ ->
%             return_error(0, {do_merge_streamParms, [All, SP]})
%     end;
% do_merge_streamParms([], SP) when record(SP#'StreamParms'.localControlDescriptor, 'LocalControlDescriptor') ->
%     LCD  = SP#'StreamParms'.localControlDescriptor,
%     PP   = LCD#'LocalControlDescriptor'.propertyParms,
%     LCD2 = LCD#'LocalControlDescriptor'{propertyParms = lists:reverse(PP)},
%     SP#'StreamParms'{localControlDescriptor = LCD2};
% do_merge_streamParms([], SP) ->
%     SP.


% do_merge_control_streamParms([{SubTag, SD} | T] = All, LCD) ->
%     case SubTag of
%         group when LCD#'LocalControlDescriptor'.reserveGroup == asn1_NOVALUE ->
%             LCD2 = LCD#'LocalControlDescriptor'{reserveGroup = SD},
%             do_merge_control_streamParms(T, LCD2);
%         value when LCD#'LocalControlDescriptor'.reserveValue == asn1_NOVALUE ->
%             LCD2 = LCD#'LocalControlDescriptor'{reserveValue = SD},
%             do_merge_control_streamParms(T, LCD2);
%         mode when LCD#'LocalControlDescriptor'.streamMode == asn1_NOVALUE ->
%             LCD2 = LCD#'LocalControlDescriptor'{streamMode = SD},
%             do_merge_control_streamParms(T, LCD2);
%         prop ->
%             PP = LCD#'LocalControlDescriptor'.propertyParms,
%             LCD2 = LCD#'LocalControlDescriptor'{propertyParms = [SD | PP]},
%             do_merge_control_streamParms(T, LCD2);
%         _ ->
%             return_error(0, {do_merge_control_streamParms, [All, LCD]})
%   end;
% do_merge_control_streamParms([], LCD) ->
%     LCD.

% merge_terminationStateDescriptor(Parms) ->
%     TSD = #'TerminationStateDescriptor'{propertyParms = []},
%     do_merge_terminationStateDescriptor(Parms, TSD).

% do_merge_terminationStateDescriptor([{Tag, Val} | T], TSD) ->
%     case Tag of
%         serviceState when TSD#'TerminationStateDescriptor'.serviceState == asn1_NOVALUE ->
%             TSD2 = TSD#'TerminationStateDescriptor'{serviceState = Val},
%             do_merge_terminationStateDescriptor(T, TSD2);
%         eventBufferControl when TSD#'TerminationStateDescriptor'.eventBufferControl == asn1_NOVALUE->
%             TSD2 = TSD#'TerminationStateDescriptor'{eventBufferControl = Val},
%             do_merge_terminationStateDescriptor(T, TSD2);
%         propertyParm ->
%             PP = TSD#'TerminationStateDescriptor'.propertyParms,
%             TSD2 = TSD#'TerminationStateDescriptor'{propertyParms = [Val | PP]},
%             do_merge_terminationStateDescriptor(T, TSD2)
%     end;
% do_merge_terminationStateDescriptor([], TSD) ->
%     PP = TSD#'TerminationStateDescriptor'.propertyParms,
%     TSD#'TerminationStateDescriptor'{propertyParms = lists:reverse(PP)}.

% ensure_prop_groups({_TokenTag, _Line, Text}) ->
%     Group  = [],
%     Groups = [],
%     parse_prop_name(Text, Group, Groups).

% parse_prop_name([Char | Rest] = All, Group, Groups) ->
%     case ?classify_char(Char) of
%         white_space ->
%             parse_prop_name(Rest, Group, Groups);
%         end_of_line ->
%             parse_prop_name(Rest, Group, Groups);
%         _ ->
%             Name = [],
%             do_parse_prop_name(All, Name, Group, Groups)
%     end;
% parse_prop_name([] = All, Group, Groups) ->
%     Name = [],
%     do_parse_prop_name(All, Name, Group, Groups).

% do_parse_prop_name([Char | Rest], Name, Group, Groups) ->
%     case ?classify_char(Char) of
%         safe_char ->
%             do_parse_prop_name(Rest, [Char | Name], Group, Groups);
%         rest_char when Char == $=, Name /= [] ->
%             %% Now we have a complete name
%             if
%                 Name == "v", Group /= [] ->
%                     %% v= is a property group delimiter,
%                     %% lets create yet another property group.
%                     Groups2 = [lists:reverse(Group) | Groups],
%                     Group2 = [],
%                     parse_prop_value(Rest, Name, Group2, Groups2);
%                 true ->
%                     %% Use current property group
%                     parse_prop_value(Rest, Name, Group, Groups)
%             end;
%         _ ->
%             return_error(0, {bad_prop_name, lists:reverse(Name), Char})
%     end;
% do_parse_prop_name([], [], [], Groups) ->
%     lists:reverse(Groups);
% do_parse_prop_name([], [], Group, Groups) ->
%     Group2 = lists:reverse(Group),
%     lists:reverse([Group2 | Groups]);
% do_parse_prop_name([], Name, Group, Groups) when Name /= [] ->
%     %% Assume end of line
%     Value = [],
%     PP = make_prop_parm(Name, Value),
%     Group2 = lists:reverse([PP | Group]),
%     lists:reverse([Group2 | Groups]).
                   
% parse_prop_value(Chars, Name, Group, Groups) ->
%     Value = [],
%     do_parse_prop_value(Chars, Name, Value, Group, Groups).

% do_parse_prop_value([Char | Rest], Name, Value, Group, Groups) ->
%     case ?classify_char(Char) of
%         end_of_line ->
%             %% Now we have a complete "name=value" pair
%             PP = make_prop_parm(Name, Value),
%             parse_prop_name(Rest, [PP | Group], Groups);
%         _ ->
%             do_parse_prop_value(Rest, Name, [Char | Value], Group, Groups)
%     end;
% do_parse_prop_value([], Name, Value, Group, Groups) ->
%     %% Assume end of line
%     PP = make_prop_parm(Name, Value),
%     Group2 = lists:reverse([PP | Group]),
%     lists:reverse([Group2 | Groups]).

% make_prop_parm(Name, Value) ->
%     #'PropertyParm'{name  = lists:reverse(Name),
%                     value = [lists:reverse(Value)]}.

-dialyzer({nowarn_function, ensure_uint/3}). % Future compat
ensure_uint({_TokenTag, Line, Val}, Min, Max) when is_integer(Val) ->
    if
        is_integer(Min) andalso (Val >= Min) ->
            if
                is_integer(Max) andalso (Val =< Max) ->
                    Val;
                Max =:= infinity ->
                    Val;
                true ->
                    return_error(Line, {too_large_integer, Val, Max})
            end;
        true ->
            return_error(Line, {too_small_integer, Val, Min})
    end;
ensure_uint({TokenTag, Line, Text}, Min, Max) ->
    case catch list_to_integer(Text) of
        {'EXIT', _} ->
            return_error(Line, {not_an_integer, Text});
        Val when is_integer(Val) ->
            ensure_uint({TokenTag, Line, Val}, Min, Max)
   end;
ensure_uint(Val, Min, Max) ->
    ensure_uint({uint, 0, Val}, Min, Max).

ensure_uint16(Int) ->
    ensure_uint(Int, 0, 65535).

% ensure_uint32(Int) ->
%     ensure_uint(Int, 0, 4294967295) .

%% OTP-4710
ensure_hex({_TokenTag, _Line, [$0, $x |Chars]}, Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []);
ensure_hex({_TokenTag, _Line, [$0, $X |Chars]}, Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []);
ensure_hex([$0, $x |Chars], Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []);
ensure_hex([$0, $X |Chars], Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []).

%% OTP-4710
hex_to_int([], Acc) ->
    lists:reverse(Acc);
hex_to_int([Char1,Char2|Tail], Acc) ->
    Int1 = hchar_to_int(Char1),
    Int2 = hchar_to_int(Char2),
    Val  = Int2 bor (Int1 bsl 4),
    hex_to_int(Tail, [Val| Acc]);
hex_to_int([Char], Acc) ->
    Int = hchar_to_int(Char),
    lists:reverse([Int|Acc]).

hchar_to_int(Char) when $0 =< Char, Char =< $9 ->
    Char - $0;
hchar_to_int(Char) when $A =< Char, Char =< $F ->
    Char - $A + 10; % OTP-4710
hchar_to_int(Char) when $a =< Char, Char =< $f ->
    Char - $a + 10. % OTP-4710

% value_of({_TokenTag, _Line, Text}) ->
%     Text.


% d(F) ->
%     d(F, []).

% d(F, A) ->
%     d(get(dbg), F, A).

% d(true, F, A) ->
%     io:format("~p:" ++ F ++ "~n", [?MODULE|A]);
% d(_, _, _) ->
%     ok.

