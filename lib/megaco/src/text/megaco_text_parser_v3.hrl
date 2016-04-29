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
%% Purpose : Define semantic text parser actions
%%----------------------------------------------------------------------


-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v3.hrl").
-include("megaco_text_tokens.hrl").

-ifdef(megaco_parser_inline).
-compile({inline,[{make_safe_token,1}]}).
-endif.
make_safe_token(Token) ->
    {_TokenTag, Line, Text} = Token,
    {safeToken, Line, Text}.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_value,1}]}).
-endif.
ensure_value(Token) ->
    case Token of
	{safeToken, _Line, Text} when is_list(Text) ->
	    Text;  % We really should ensure length
	{'QuotedChars', _Line, Text} when is_list(Text) ->
	    Text;  % We really should ensure length
	Text when is_list(Text) ->
	    Text   % We really should ensure length
    end.

%% NAME       = ALPHA *63(ALPHA / DIGIT / "_" )
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_NAME,1}]}).
-endif.
ensure_NAME(Token) ->
    {_TokenTag, _Line, Text} = Token,
    Text.  %% BUGBUG: ensure length and chars

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_requestID,1}]}).
-endif.
ensure_requestID(Token) ->
    case Token of
	{safeToken, _Line, "*"} ->
	    ?megaco_all_request_id;
	_ ->
	    ensure_uint32(Token)
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_streamID,1}]}).
-endif.
ensure_streamID(StreamId) ->
    ensure_uint16(StreamId).

ensure_auth_header(SpiToken, SnToken, AdToken) ->
    Spi = ensure_hex(SpiToken, 8,  8),
    Sn  = ensure_hex(SnToken,  8,  8), 
    Ad  = ensure_hex(AdToken, 24, 64),
    #'AuthenticationHeader'{secParmIndex = Spi, seqNum = Sn, ad = Ad}.

%% ContextID = (UINT32 / "*" / "-" / "$")
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_contextID,1}]}).
-endif.
ensure_contextID(Token) ->
    {_TokenTag, Line, Text} = Token,
    case Text of
        "*"  -> ?megaco_all_context_id;
        "-"  -> ?megaco_null_context_id;
        "\$" -> ?megaco_choose_context_id;
        Int  ->
	    CID = ensure_uint32(Int),
	    if
		(CID =/= 0) andalso
		(CID =/= 16#FFFFFFFE) andalso
		(CID =/= 16#FFFFFFFF) ->
		    CID;
		true ->
		    return_error(Line, {bad_ContextID, CID})
	    end
    end.

ensure_domainAddress([{_T, _L, _A} = Addr0], Port) ->
    Addr = ensure_ip4addr(Addr0), 
    {ip4Address, #'IP4Address'{address = Addr, portNumber = Port}};
ensure_domainAddress([colon,colon], Port) ->
    Addr = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    {ip6Address, #'IP6Address'{address = Addr, portNumber = Port}};
ensure_domainAddress(Addr0, Port) ->
    Addr = ensure_ip6addr(Addr0),
    {ip6Address, #'IP6Address'{address = Addr, portNumber = Port}}.


ensure_ip4addr(Token) ->
    {_TokenTag, Line, Addr} = Token, 
    case split_ip4addr_text(Addr, []) of
	[T1, T2, T3, T4] ->
	    %% We optimize by sending only the text part (Addr) of 
	    %% the token to the function. 
	    %% If something is wrong, then we do not get a proper 
	    %% position and therefor we catch and sendissue the
	    %% the error again (with the proper line number).
	    case (catch [
			 ensure_uint(T1, 0, 255),
			 ensure_uint(T2, 0, 255),
			 ensure_uint(T3, 0, 255),
			 ensure_uint(T4, 0, 255)
			]) of
		A when is_list(A) ->
		    A;
		_ ->
		    return_error(Line, {bad_IP4address, Addr})
	    end;
	_ ->
	    return_error(Line, {bad_IP4address, Addr})
    end.

split_ip4addr_text([], Acc) ->
    [ lists:reverse(Acc) ];
split_ip4addr_text([$. | Rest], Acc) ->
    [ lists:reverse(Acc) | split_ip4addr_text(Rest, []) ];
split_ip4addr_text([H | T], Acc) ->
    split_ip4addr_text(T, [H | Acc]).


ensure_ip6addr([colon,colon|T]) ->
    [H1|T1] = lists:reverse(T),
    case do_ensure_ip6addr(T1, true, [ensure_hex4_or_ip4addr(H1)], 1) of
	{true, A} when length(A) =:= 16 ->
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
		{true, A} when length(A) =:= 16 ->
		    A;
		{true, B} when length(B) < 16 ->
		    B ++ lists:duplicate(16 - length(B), 0);
		{true, C} ->
		    throw({error, {?MODULE, {bad_mid_ip6addr_length, C}}})
	    end;
	[H|L1] -> % A (last element) could be an ip4 address
	    case do_ensure_ip6addr(L1,false,[ensure_hex4_or_ip4addr(H)],1) of
		{false, A} when length(A) =:= 16 -> 
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

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_domainName,2}]}).
-endif.
ensure_domainName(Token, Port) ->
    {_TokenTag, _Line, Name} = Token,
    %% BUGBUG: we should really validate name
    {domainName, #'DomainName'{name = Name, portNumber = Port}}.

%% extensionParameter= "X"  ("-" / "+") 1*6(ALPHA / DIGIT)
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_extensionParameter,1}]}).
-endif.
ensure_extensionParameter(Token) ->
    {_TokenTag, Line, Text} = Token,
    case Text of
        [X, S | _Chars] ->
            if
                (X =/= $X) andalso (X =/= $x) andalso
                (S =/= $+) andalso (S =/= $-) ->
                    return_error(Line, {bad_extension_parameter, Text});
                true ->
                    {extension_parameter, Text}
            end;
        _ ->
            return_error(Line, {bad_extension_parameter, Text})
    end.

ensure_message(MegacopToken,  MID, Body) ->
%%     #'ServiceChangeProfile'{profileName = Name,
%% 			    version     = Version} = 
%% 	ensure_profile(MegacopToken),
%%     case Name of
%%         "megaco" ->
%%             #'Message'{version = Version, mId = MID, messageBody = Body};
%%         [$!]  ->
%%             #'Message'{version = Version, mId = MID, messageBody = Body}
%%     end.
    {_TokenTag, Line, Text} = MegacopToken, 
    case split_Megacop(Text, []) of
	{Name, Version} ->
	    Version2 = ensure_version(Version),
	    case Name of
		"megaco" ->
		    #'Message'{version     = Version2, 
			       mId         = MID, 
			       messageBody = Body};
		[$!]  ->
		    #'Message'{version     = Version2, 
			       mId         = MID, 
			       messageBody = Body}
	    end;
	_ ->
	    return_error(Line, {bad_name_or_version, Text})
    end.

split_Megacop([], _) ->
    error;
split_Megacop([$/ | Version], Acc) ->
    {lists:reverse(Acc), Version};
split_Megacop([H | T], Acc) ->
    split_Megacop(T, [H | Acc]).


%% Corr1:
%% As of corr 1 ModemDescriptor has been deprecated.
%% and since this functon is only used when creating
%% a ModemDescriptor, iit is removed.
%% modemType         = (V32bisToken / V22bisToken / V18Token / 
%%                      V22Token / V32Token / V34Token / V90Token / 
%%                      V91Token / SynchISDNToken / extensionParameter)
%% ensure_modemType({_TokenTag, _Line, Text} = Token) ->
%%     case Text of
%%         "v32b"      -> v32bis;
%%         "v22b"      -> v22bis;
%%         "v18"       -> v18;
%%         "v22"       -> v22;
%%         "v32"       -> v32;
%%         "v34"       -> v34;
%%         "v90"       -> v90;
%%         "v91"       -> v91;
%%         "synchisdn" -> synchISDN;
%%         "sn"        -> synchISDN;
%%         [$x | _]               -> ensure_extensionParameter(Token)
%%     end.

%% An mtp address is five octets long
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_mtpAddress,1}]}).
-endif.
ensure_mtpAddress(Token) ->
    {_TokenTag, _Line, Addr} = Token, 
    %% BUGBUG: validate address
    {mtpAddress, Addr}.

%% MuxType = ( H221Token / H223Token / H226Token / V76Token / extensionParameter )
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_muxType,1}]}).
-endif.
ensure_muxType(Token) ->
    {_TokenTag, _Line, Text} = Token, 
    case Text of
        "h221"   -> h221;
        "h223"   -> h223;
        "h226"   -> h226;
        "v76"    -> v76;
        "nx64k"  -> nx64k; % v2
        [$x | _] -> ensure_extensionParameter(Token)
    end.

%% packagesItem      = NAME "-" UINT16
%% NAME              = ALPHA *63(ALPHA / DIGIT / "_" )
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_packagesItem,1}]}).
-endif.
ensure_packagesItem(Token) ->
    {_TokenTag, Line, Text} = Token, 
      case split_packagesItem(Text, []) of
	{Name, Version} ->
	    %% As we don't ensure length of the names, there is no point 
	    %% in doing the ensure_NAME thing...
            #'PackagesItem'{packageName    = Name,
                            packageVersion = ensure_uint(Version, 0, 99)};
        _ ->
            return_error(Line, {bad_PackagesItem, Text})
    end.

split_packagesItem([], _) ->
    error;
split_packagesItem([$- | Version], Acc) ->
    {lists:reverse(Acc), Version};
split_packagesItem([H|T], Acc) ->
    split_packagesItem(T, [H|Acc]).


%% pkgdName          =  (PackageName / "*")  SLASH  (ItemID / "*" )
%% PackageName       = NAME
%% ItemID            = NAME
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_pkgdName,1}]}).
-endif.
ensure_pkgdName(Token) ->
    {_TokenTag, Line, Text} = Token, 
    case ensure_pkgdName(Text, []) of
        ok ->
	    %% As we don't really do any checks on the strings 
	    %% (length or content) there is really no point in
	    %% "ensuring" the name and item part of the 
	    %% package name
            %% ensure_name_or_star(Name),
            %% ensure_name_or_star(Item),
	    Text;
        _ ->
            return_error(Line, {bad_pkgdName, Text})
    end.

ensure_pkgdName([], _) ->
    error;
ensure_pkgdName([$/ | T], Acc) 
  when ((length(T) > 0) andalso (length(Acc) > 0)) ->
    ok;
ensure_pkgdName([H | T], Acc) ->
    ensure_pkgdName(T, [H | Acc]).


%% -compile({inline,[{ensure_name_or_star,1}]}).
%% ensure_name_or_star(Val) ->
%%     %%     case Token of
%%     %% 	{_, _, Name} when Name =:= "*" ->
%%     %% 	    Name;
%%     %% 	_ ->
%%     %% 	    ensure_NAME(Token)
%%     %%     end.
%%     if
%% 	Val =:= "*" ->
%% 	    Val;
%% 	true ->
%% 	    %% as we don't really validate the text part of the token(s),
%% 	    %% we can just return the value assuming it to be correct...
%% 	    Val
%%     end.


%% v2 - start

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_indAudMediaDescriptor,1}]}).
-endif.
merge_indAudMediaDescriptor(Vals) ->
    merge_indAudMediaDescriptor(Vals, #'IndAudMediaDescriptor'{}).

merge_indAudMediaDescriptor(
  [], #'IndAudMediaDescriptor'{streams = Streams1} = D) ->
    Streams2 = 
	case Streams1 of
	    {multiStream, Descs} ->
		{multiStream, lists:reverse(Descs)};
	    _ ->
		Streams1
	end,
    D#'IndAudMediaDescriptor'{streams = Streams2};
merge_indAudMediaDescriptor([{termStateDescr, Val}|Vals], D) 
  when D#'IndAudMediaDescriptor'.termStateDescr =:= asn1_NOVALUE ->
    D2 = #'IndAudMediaDescriptor'{termStateDescr = Val},
    merge_indAudMediaDescriptor(Vals, D2);
merge_indAudMediaDescriptor([{streamParm, Val}|Vals], D)  
  when D#'IndAudMediaDescriptor'.streams =:= asn1_NOVALUE ->
    D2 = #'IndAudMediaDescriptor'{streams = {oneStream, Val}},
    merge_indAudMediaDescriptor(Vals, D2);
merge_indAudMediaDescriptor([{streamDescr, Val}|Vals], D)  
  when D#'IndAudMediaDescriptor'.streams =:= asn1_NOVALUE ->
    D2 = #'IndAudMediaDescriptor'{streams = {multiStream, [Val]}},
    merge_indAudMediaDescriptor(Vals, D2);
merge_indAudMediaDescriptor([{streamDescr, Val}|Vals], 
  #'IndAudMediaDescriptor'{streams = Streams1} = D1) ->
    Streams2 = 
	case Streams1 of
	    {multiStream, Descs} ->
		{multiStream, [Val|Descs]};
	    _ ->
		return_error(0, {bad_IndAudMediaDescriptor_streamDescr, 
				 Val, Streams1})
	end,
    D2 = D1#'IndAudMediaDescriptor'{streams = Streams2},
    merge_indAudMediaDescriptor(Vals, D2);
merge_indAudMediaDescriptor([{Tag, Val}|_], D) ->
    case Tag of
	termStateDescr ->
	    return_error(0, {bad_IndAudMediaDescriptor_termStateDescr, 
			     Val, D#'IndAudMediaDescriptor'.termStateDescr});
	streamParm ->
	    return_error(0, {bad_IndAudMediaDescriptor_streamParm, 
			     Val, D#'IndAudMediaDescriptor'.streams});
	streamDescr ->
	    return_error(0, {bad_IndAudMediaDescriptor_streamDescr, 
			     Val, D#'IndAudMediaDescriptor'.streams});
	_ ->
	    return_error(0, {bad_IndAudMediaDescriptor_tag, Tag, Val})
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_indAudLocalControlDescriptor,1}]}).
-endif.
merge_indAudLocalControlDescriptor(Parms) ->
    merge_indAudLocalControlDescriptor(Parms, 
					  #'IndAudLocalControlDescriptor'{},
					  asn1_NOVALUE).
					  
merge_indAudLocalControlDescriptor([modeToken | Parms], D, PP) 
  when (D#'IndAudLocalControlDescriptor'.streamMode    =:= asn1_NOVALUE) andalso 
       (D#'IndAudLocalControlDescriptor'.streamModeSel =:= asn1_NOVALUE) ->
    D2 = D#'IndAudLocalControlDescriptor'{streamMode = 'NULL'},
    merge_indAudLocalControlDescriptor(Parms, D2, PP);

merge_indAudLocalControlDescriptor([modeToken | Parms], D, PP) 
  when (D#'IndAudLocalControlDescriptor'.streamMode =:= asn1_NOVALUE) ->
    merge_indAudLocalControlDescriptor(Parms, D, PP);

merge_indAudLocalControlDescriptor([{mode, Val} | Parms], D, PP) 
  when (D#'IndAudLocalControlDescriptor'.streamMode    =:= asn1_NOVALUE) andalso 
       (D#'IndAudLocalControlDescriptor'.streamModeSel =:= asn1_NOVALUE) ->
    D2 = 
	case Val of
	    {equal, Val2} ->
		D#'IndAudLocalControlDescriptor'{streamModeSel = Val2};
	    {inequal, Val2} ->
		D#'IndAudLocalControlDescriptor'{streamModeSel = Val2}
	end,
    merge_indAudLocalControlDescriptor(Parms, D2, PP);

merge_indAudLocalControlDescriptor([{mode, Val} | Parms], D, PP) 
  when (D#'IndAudLocalControlDescriptor'.streamModeSel =:= asn1_NOVALUE) ->
    D2 = 
	case Val of
	    {equal, Val2} ->
		D#'IndAudLocalControlDescriptor'{streamMode    = asn1_NOVALUE,
						 streamModeSel = Val2};
	    {inequal, Val2} ->
		D#'IndAudLocalControlDescriptor'{streamMode    = asn1_NOVALUE,
						 streamModeSel = Val2}
	end,
    merge_indAudLocalControlDescriptor(Parms, D2, PP);

merge_indAudLocalControlDescriptor([reservedGroupToken | Parms], D, PP) 
  when D#'IndAudLocalControlDescriptor'.reserveGroup =:= asn1_NOVALUE ->
    D2 = D#'IndAudLocalControlDescriptor'{reserveGroup = 'NULL'},
    merge_indAudLocalControlDescriptor(Parms, D2, PP);

merge_indAudLocalControlDescriptor([reservedValueToken | Parms], D, PP) 
  when D#'IndAudLocalControlDescriptor'.reserveValue =:= asn1_NOVALUE ->
    D2 = D#'IndAudLocalControlDescriptor'{reserveValue = 'NULL'},
    merge_indAudLocalControlDescriptor(Parms, D2, PP);

%% This is really wierd in the standard, so at this point this is the
%% best I can do...  BUGBUG BUGBUG BUGBUG
%% 
merge_indAudLocalControlDescriptor([{name, Val} | Parms], D, asn1_NOVALUE) ->
    PP = #'IndAudPropertyParm'{name = Val},
    merge_indAudLocalControlDescriptor(Parms, D, PP);

merge_indAudLocalControlDescriptor([{name, Val} | Parms], D, PP) 
  when D#'IndAudLocalControlDescriptor'.propertyParms =:= asn1_NOVALUE ->
    D2  = D#'IndAudLocalControlDescriptor'{propertyParms = [PP]},
    PP2 = #'IndAudPropertyParm'{name = Val},
    merge_indAudLocalControlDescriptor(Parms, D2, PP2);

merge_indAudLocalControlDescriptor([{name, Val} | Parms], D, PP) ->
    PPs = D#'IndAudLocalControlDescriptor'.propertyParms,
    D2  = D#'IndAudLocalControlDescriptor'{propertyParms = [PP|PPs]},
    PP2 = #'IndAudPropertyParm'{name = Val},
    merge_indAudLocalControlDescriptor(Parms, D2, PP2);

%% BUGBUG BUGBUG I cannot construct a proper IndAudPropertyParm with
%% just the prop (the mandatory name part is missing), so for now I
%% assume that it this has been used, then the name part
%% (pkgdName) must precide it? 
merge_indAudLocalControlDescriptor([{prop, Val} | Parms], D, PP) 
  when (PP =/= asn1_NOVALUE) andalso 
       (D#'IndAudLocalControlDescriptor'.propertyParms =:= asn1_NOVALUE) ->
    PP2 = PP#'IndAudPropertyParm'{propertyParms = Val},
    D2  = D#'IndAudLocalControlDescriptor'{propertyParms = [PP2]},
    merge_indAudLocalControlDescriptor(Parms, D2, asn1_NOVALUE);

merge_indAudLocalControlDescriptor([{prop, Val} | Parms], D, PP) 
  when (PP =/= asn1_NOVALUE) andalso 
       is_list(D#'IndAudLocalControlDescriptor'.propertyParms) ->
    PPs = D#'IndAudLocalControlDescriptor'.propertyParms,
    PP2 = PP#'IndAudPropertyParm'{propertyParms = Val},
    D2  = D#'IndAudLocalControlDescriptor'{propertyParms = [PP2|PPs]},
    merge_indAudLocalControlDescriptor(Parms, D2, asn1_NOVALUE);

merge_indAudLocalControlDescriptor([H | _T], _D, _PP) ->
    return_error(0, {bad_indAudLocalControlDescriptor_parm, H});

merge_indAudLocalControlDescriptor([], D, asn1_NOVALUE) 
  when D#'IndAudLocalControlDescriptor'.propertyParms =:= asn1_NOVALUE ->
    D;
merge_indAudLocalControlDescriptor([], D, asn1_NOVALUE) ->
    PPs = D#'IndAudLocalControlDescriptor'.propertyParms,
    PropParms2 = lists:reverse(PPs),
    D#'IndAudLocalControlDescriptor'{propertyParms = PropParms2};
merge_indAudLocalControlDescriptor([], D, PP) 
  when D#'IndAudLocalControlDescriptor'.propertyParms =:= asn1_NOVALUE ->
    D#'IndAudLocalControlDescriptor'{propertyParms = [PP]};
merge_indAudLocalControlDescriptor([], D, PP) ->
    PPs  = D#'IndAudLocalControlDescriptor'.propertyParms,
    PPs2 = lists:reverse([PP|PPs]),
    D#'IndAudLocalControlDescriptor'{propertyParms = PPs2}.


merge_indAudTerminationStateDescriptor({name, Val}) ->
    PropParm = #'IndAudPropertyParm'{name = Val},
    #'IndAudTerminationStateDescriptor'{propertyParms = [PropParm]};
%% BUGBUG BUGBUG BUGBUG
merge_indAudTerminationStateDescriptor({prop, Val}) ->
    exit({incomplete_propertyParm_in_indAudTerminationStateDescriptor, Val});
merge_indAudTerminationStateDescriptor(serviceStatesToken) ->
    #'IndAudTerminationStateDescriptor'{serviceState = 'NULL'};
merge_indAudTerminationStateDescriptor({serviceStates, {equal, Val}}) ->
    #'IndAudTerminationStateDescriptor'{serviceStateSel = Val};
merge_indAudTerminationStateDescriptor({serviceStates, {inequal, Val}}) ->
    #'IndAudTerminationStateDescriptor'{serviceStateSel = Val};
merge_indAudTerminationStateDescriptor(bufferToken) ->
    #'IndAudTerminationStateDescriptor'{eventBufferControl = 'NULL'}.


-ifdef(megaco_parser_inline).
-compile({inline,[{merge_indAudEventBufferDescriptor,2}]}).
-endif.
merge_indAudEventBufferDescriptor(EventName, SpecParams) ->
    IAEBD = #'IndAudEventBufferDescriptor'{eventName = EventName},
    do_merge_indAudEventBufferDescriptor(SpecParams, IAEBD).

do_merge_indAudEventBufferDescriptor(asn1_NOVALUE, IAEBD) ->
    IAEBD;
do_merge_indAudEventBufferDescriptor({streamID, StreamID}, IAEBD) ->
    IAEBD#'IndAudEventBufferDescriptor'{streamID = StreamID};
do_merge_indAudEventBufferDescriptor({eventParameterName, _Name} = EPN, 
				     IAEBD) ->
    %% BUGBUG BUGBUG BUGBUG 
    %% This is an ugly hack to allow the eventParamName which only
    %% exists in the text encoding...
    IAEBD#'IndAudEventBufferDescriptor'{streamID = EPN}.


-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_indAudSignalListParm,1}]}).
-endif.
ensure_indAudSignalListParm(SIG) ->
    if
	is_record(SIG, 'Signal') ->
	    ensure_indAudSignal(SIG);
	true ->
	    return_error(0, {bad_Signal, SIG})
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_indAudSignal,1}]}).
-endif.
ensure_indAudSignal(Sig) ->
    #'Signal'{signalName       = SignalName,
	      streamID         = SID, 
	      sigType          = asn1_NOVALUE, 
	      duration         = asn1_NOVALUE, 
	      notifyCompletion = asn1_NOVALUE, 
	      keepActive       = asn1_NOVALUE, 
	      sigParList       = [],
	      requestID        = RID} = Sig,
    #'IndAudSignal'{signalName      = SignalName,
		    streamID        = SID,
		    signalRequestID = RID}.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_IADMD,1}]}).
-endif.
ensure_IADMD(Token) ->
    {_TokenTag, _Line, DMD} = Token,
    #'DigitMapDescriptor'{digitMapName  = Name,
			  digitMapValue = asn1_NOVALUE} = DMD,
    #'IndAudDigitMapDescriptor'{digitMapName = Name}.


-ifdef(megaco_parser_inline).
-compile({inline,[{merge_indAudPackagesDescriptor,1}]}).
-endif.
merge_indAudPackagesDescriptor(Pkgs) ->
    #'PackagesItem'{packageName    = N,
		    packageVersion = V} = Pkgs,
    #'IndAudPackagesDescriptor'{packageName    = N,
				packageVersion = V}.


%% ensure_indAudTerminationStateParm(Token) ->
%%     case Token of
%% 	{safeToken, _Line, "servicestates"} -> serviceStatesToken;
%% 	{safeToken, _Line, "si"}            -> serviceStatesToken;
%% 	{safeToken, _Line, "buffer"}        -> bufferToken;
%% 	{safeToken, _Line, "bf"}            -> bufferToken;
%% 	PkgdName                            -> {pkgdName,
%% 						ensure_pkgdName(PkgdName)}
%%     end.


%% Types modified by v2:

merge_auditDescriptor([]) ->
    #'AuditDescriptor'{};
merge_auditDescriptor(Tokens) when is_list(Tokens) ->
    case lists:keysearch(terminationAudit, 1, Tokens) of
	{value, {terminationAudit, TA}} ->
	    case lists:keydelete(terminationAudit, 1, Tokens) of
		[] ->
		    #'AuditDescriptor'{auditPropertyToken = TA};
		AuditTokens ->
		    #'AuditDescriptor'{auditToken         = AuditTokens,
				       auditPropertyToken = TA}
	    end;
	false ->
	    #'AuditDescriptor'{auditToken = Tokens}
    end;
merge_auditDescriptor(_) ->
    #'AuditDescriptor'{}.


%% v2 - end



-ifdef(megaco_parser_inline).
-compile({inline,[{merge_ServiceChangeParm,1}]}).
-endif.
merge_ServiceChangeParm(Parms) ->
    Required = [serviceChangeReason, serviceChangeMethod],
    merge_ServiceChangeParm(Parms, #'ServiceChangeParm'{}, Required).

merge_ServiceChangeParm([], SCP, []) ->
    SCP;

merge_ServiceChangeParm([], _SCP, Required) ->
    exit({missing_required_serviceChangeParm, Required});

merge_ServiceChangeParm([{address, Val}|Parms], SCP0, Req) 
  when (SCP0#'ServiceChangeParm'.serviceChangeAddress =:= asn1_NOVALUE) andalso 
       (SCP0#'ServiceChangeParm'.serviceChangeMgcId =:= asn1_NOVALUE) ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeAddress = Val},
    merge_ServiceChangeParm(Parms, SCP, Req);
merge_ServiceChangeParm([{address, Val}|_Parms], SCP0, _Req) 
  when SCP0#'ServiceChangeParm'.serviceChangeAddress =:= asn1_NOVALUE ->
    MgcId = SCP0#'ServiceChangeParm'.serviceChangeMgcId,
    exit({not_both_address_mgcid_serviceChangeParm, Val, MgcId});

merge_ServiceChangeParm([{mgc_id, Val}|Parms], SCP0, Req) 
  when (SCP0#'ServiceChangeParm'.serviceChangeMgcId =:= asn1_NOVALUE) andalso 
       (SCP0#'ServiceChangeParm'.serviceChangeAddress =:= asn1_NOVALUE) ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeMgcId = Val},
    merge_ServiceChangeParm(Parms, SCP, Req);
merge_ServiceChangeParm([{mgc_id, Val}|_Parms], SCP0, _Req) 
  when SCP0#'ServiceChangeParm'.serviceChangeMgcId =:= asn1_NOVALUE ->
    Addr = SCP0#'ServiceChangeParm'.serviceChangeAddress,
    exit({not_both_address_mgcid_serviceChangeParm, Val, Addr});

merge_ServiceChangeParm([{profile, Val}|Parms], SCP0, Req) 
  when SCP0#'ServiceChangeParm'.serviceChangeProfile =:= asn1_NOVALUE ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeProfile = Val},
    merge_ServiceChangeParm(Parms, SCP, Req);

merge_ServiceChangeParm([{version, Val}|Parms], SCP0, Req) 
  when SCP0#'ServiceChangeParm'.serviceChangeVersion =:= asn1_NOVALUE ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeVersion = Val},
    merge_ServiceChangeParm(Parms, SCP, Req);

%% REQUIRED (i.e. no default value)
merge_ServiceChangeParm([{reason, Val}|Parms], SCP0, Req0) 
  when SCP0#'ServiceChangeParm'.serviceChangeReason =:= undefined ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeReason = Val},
    Req = lists:delete(serviceChangeReason, Req0),
    merge_ServiceChangeParm(Parms, SCP, Req);

merge_ServiceChangeParm([{delay, Val}|Parms], SCP0, Req) 
  when SCP0#'ServiceChangeParm'.serviceChangeDelay =:= asn1_NOVALUE ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeDelay = Val},
    merge_ServiceChangeParm(Parms, SCP, Req);

%% REQUIRED (i.e. no default value)
merge_ServiceChangeParm([{method, Val}|Parms], SCP0, Req0) 
  when SCP0#'ServiceChangeParm'.serviceChangeMethod =:= undefined ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeMethod = Val},
    Req = lists:delete(serviceChangeMethod, Req0),
    merge_ServiceChangeParm(Parms, SCP, Req);

merge_ServiceChangeParm([{time_stamp, Val}|Parms], SCP0, Req) 
  when SCP0#'ServiceChangeParm'.timeStamp =:= asn1_NOVALUE ->
    SCP = SCP0#'ServiceChangeParm'{timeStamp = Val},
    merge_ServiceChangeParm(Parms, SCP, Req);

merge_ServiceChangeParm([{extension, _Val}|Parms], SCP0, Req) ->
    merge_ServiceChangeParm(Parms, SCP0, Req);

merge_ServiceChangeParm([{audit_item, Val}|Parms], SCP0, Req) 
  when (SCP0#'ServiceChangeParm'.serviceChangeInfo =:= asn1_NOVALUE) andalso 
       is_atom(Val) ->
    SCI = #'AuditDescriptor'{auditToken = [Val]},
    SCP = SCP0#'ServiceChangeParm'{serviceChangeInfo = SCI},
    merge_ServiceChangeParm(Parms, SCP, Req);
merge_ServiceChangeParm([{audit_item, Val}|Parms], SCP0, Req) 
  when (SCP0#'ServiceChangeParm'.serviceChangeInfo =:= asn1_NOVALUE) andalso 
       is_tuple(Val) ->
    SCI = #'AuditDescriptor'{auditPropertyToken = [Val]},
    SCP = SCP0#'ServiceChangeParm'{serviceChangeInfo = SCI},
    merge_ServiceChangeParm(Parms, SCP, Req);
merge_ServiceChangeParm([{audit_item, Val}|Parms], SCP0, Req) 
  when is_record(SCP0#'ServiceChangeParm'.serviceChangeInfo, 'AuditDescriptor') andalso 
       is_atom(Val) ->
    SCI0 = SCP0#'ServiceChangeParm'.serviceChangeInfo,
    L    = SCI0#'AuditDescriptor'.auditToken,
    SCI  = SCI0#'AuditDescriptor'{auditToken = [Val|L]},
    SCP  = SCP0#'ServiceChangeParm'{serviceChangeInfo = SCI},
    merge_ServiceChangeParm(Parms, SCP, Req);
merge_ServiceChangeParm([{audit_item, Val}|Parms], SCP0, Req) 
  when is_record(SCP0#'ServiceChangeParm'.serviceChangeInfo, 'AuditDescriptor') andalso 
       is_tuple(Val) ->
    SCI0 = SCP0#'ServiceChangeParm'.serviceChangeInfo,
    L    = SCI0#'AuditDescriptor'.auditPropertyToken,
    SCI  = SCI0#'AuditDescriptor'{auditPropertyToken = [Val|L]},
    SCP  = SCP0#'ServiceChangeParm'{serviceChangeInfo = SCI},
    merge_ServiceChangeParm(Parms, SCP, Req);

merge_ServiceChangeParm([incomplete|Parms], SCP0, Req) 
  when SCP0#'ServiceChangeParm'.serviceChangeIncompleteFlag =:= asn1_NOVALUE ->
    SCP = SCP0#'ServiceChangeParm'{serviceChangeIncompleteFlag = 'NULL'},
    merge_ServiceChangeParm(Parms, SCP, Req);

merge_ServiceChangeParm([{Tag, Val}|_Parms], SCP, _Req) ->
    Val2 = 
	case Tag of
	    address -> 
		SCP#'ServiceChangeParm'.serviceChangeAddress;
	    mgc_id     -> 
		SCP#'ServiceChangeParm'.serviceChangeMgcId;
	    profile    -> 
		SCP#'ServiceChangeParm'.serviceChangeProfile;
	    version    -> 
		SCP#'ServiceChangeParm'.serviceChangeVersion;
	    reason     -> 
		SCP#'ServiceChangeParm'.serviceChangeReason;
	    delay      -> 
		SCP#'ServiceChangeParm'.serviceChangeDelay;
	    method     -> 
		SCP#'ServiceChangeParm'.serviceChangeMethod;
	    time_stamp -> 
		SCP#'ServiceChangeParm'.timeStamp;
	    audit_item -> 
		SCP#'ServiceChangeParm'.serviceChangeInfo
	end,
    exit({at_most_once_serviceChangeParm, {Tag, Val, Val2}});
merge_ServiceChangeParm([Parm|_Parms], SCP, _Req) ->
    Parm2 = 
	case Parm of
	    incomplete -> 
		SCP#'ServiceChangeParm'.serviceChangeIncompleteFlag
	end,
    exit({at_most_once_serviceChangeParm, {Parm, Parm2}}).


-ifdef(megaco_parser_inline).
-compile({inline,[{merge_ServiceChangeResParm,1}]}).
-endif.
merge_ServiceChangeResParm(Parms) ->
    merge_ServiceChangeResParm(Parms, #'ServiceChangeResParm'{}).

merge_ServiceChangeResParm([], SCRP) ->
    SCRP;
merge_ServiceChangeResParm([{address, Val}|Parms], SCRP0) 
  when (SCRP0#'ServiceChangeResParm'.serviceChangeAddress =:= asn1_NOVALUE) andalso 
       (SCRP0#'ServiceChangeResParm'.serviceChangeMgcId =:= asn1_NOVALUE) ->
    SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeAddress = Val},
    merge_ServiceChangeResParm(Parms, SCRP);
merge_ServiceChangeResParm([{address, Val}|_Parms], SCRP0) 
  when SCRP0#'ServiceChangeResParm'.serviceChangeAddress =:= asn1_NOVALUE ->
    MgcId = SCRP0#'ServiceChangeResParm'.serviceChangeMgcId,
    exit({not_both_address_mgcid_servChgReplyParm, Val, MgcId});

merge_ServiceChangeResParm([{mgc_id, Val}|Parms], SCRP0)
  when (SCRP0#'ServiceChangeResParm'.serviceChangeMgcId =:= asn1_NOVALUE) andalso 
       (SCRP0#'ServiceChangeResParm'.serviceChangeAddress =:= asn1_NOVALUE) -> 
    SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeMgcId = Val},
    merge_ServiceChangeResParm(Parms, SCRP);
merge_ServiceChangeResParm([{mgc_id, Val}|_Parms], SCRP0)
  when SCRP0#'ServiceChangeResParm'.serviceChangeMgcId =:= asn1_NOVALUE -> 
    Addr = SCRP0#'ServiceChangeResParm'.serviceChangeAddress,
    exit({not_both_address_mgcid_servChgReplyParm, Val, Addr});

merge_ServiceChangeResParm([{profile, Val}|Parms], SCRP0)
  when SCRP0#'ServiceChangeResParm'.serviceChangeProfile =:= asn1_NOVALUE ->
    SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeProfile = Val},
    merge_ServiceChangeResParm(Parms, SCRP);

merge_ServiceChangeResParm([{version, Val}|Parms], SCRP0)
  when SCRP0#'ServiceChangeResParm'.serviceChangeVersion =:= asn1_NOVALUE ->
    SCRP = SCRP0#'ServiceChangeResParm'{serviceChangeVersion = Val},
    merge_ServiceChangeResParm(Parms, SCRP);

merge_ServiceChangeResParm([{time_stamp, Val}|Parms], SCRP0)
  when SCRP0#'ServiceChangeResParm'.timeStamp =:= asn1_NOVALUE ->
    SCRP = SCRP0#'ServiceChangeResParm'{timeStamp = Val},
    merge_ServiceChangeResParm(Parms, SCRP);

merge_ServiceChangeResParm([{Tag, Val}|_Parms], SCRP) ->
    Val2 = 
	case Tag of
	    address    -> SCRP#'ServiceChangeResParm'.serviceChangeAddress;
	    mgc_id     -> SCRP#'ServiceChangeResParm'.serviceChangeMgcId;
	    profile    -> SCRP#'ServiceChangeResParm'.serviceChangeProfile;
	    version    -> SCRP#'ServiceChangeResParm'.serviceChangeVersion;
	    time_stamp -> SCRP#'ServiceChangeResParm'.timeStamp
	end,
    exit({at_most_once_servChgReplyParm, {Tag, Val, Val2}}).


-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_serviceChangeMethod,1}]}).
-endif.
ensure_serviceChangeMethod(Token) ->
    case Token of
	{safeToken, _Line, "fl"} ->	
	    failover;
	{safeToken, _Line, "failover"} ->
	    failover;
	{safeToken, _Line, "fo"} -> 
	    forced;
	{safeToken, _Line, "forced"} ->
	    forced;
	{safeToken, _Line, "gr"} ->
	    graceful;
	{safeToken, _Line, "graceful"} ->
	    graceful;
	{safeToken, _Line, "rs"} ->
	    restart;
	{safeToken, _Line, "restart"} ->
	    restart;
	{safeToken, _Line, "dc"} ->
	    disconnected;
	{safeToken, _Line, "disconnected"} ->
	    disconnected;
	{safeToken, _Line, "ho"} ->
	    handOff;
	{safeToken, _Line, "handoff"} ->
	    handOff;
	{safeToken, Line, Text} ->
	    return_error(Line, {bad_serviceChangeMethod, Text})
    end.


-ifdef(megaco_parser_inline).
-compile({inline,[{merge_topologyDescriptor,1}]}).
-endif.
merge_topologyDescriptor(Components) ->
    merge_topologyDescriptor(Components, #'TopologyRequest'{}, []).

merge_topologyDescriptor([], TR, TRs) ->
    lists:reverse([ensure_TopologyRequest(TR)|TRs]);
merge_topologyDescriptor(
  [{tid, From}|Comps], 
  #'TopologyRequest'{terminationFrom = undefined} = TR1, TRs) ->
    TR2 = TR1#'TopologyRequest'{terminationFrom = From},
    merge_topologyDescriptor(Comps, TR2, TRs);
merge_topologyDescriptor(
  [{tid, To}|Comps], 
  #'TopologyRequest'{terminationTo = undefined} = TR1, 
  TRs) ->
    TR2 = TR1#'TopologyRequest'{terminationTo = To},
    merge_topologyDescriptor(Comps, TR2, TRs);
merge_topologyDescriptor([{tid, From}|Comps], TR1, TRs) ->
    TR2 = #'TopologyRequest'{terminationFrom = From},
    merge_topologyDescriptor(Comps, TR2, [TR1 | TRs]);
merge_topologyDescriptor([{direction, Dir}|Comps], TR1, TRs) ->
    TR2 = TR1#'TopologyRequest'{topologyDirection = Dir},
    merge_topologyDescriptor(Comps, TR2, TRs);
merge_topologyDescriptor([{sid, SID}|Comps], TR1, TRs) ->
    TR2 = TR1#'TopologyRequest'{streamID = SID},
    merge_topologyDescriptor(Comps, TR2, TRs);
merge_topologyDescriptor(
  [{direction_ext, EDir}|Comps], 
  #'TopologyRequest'{topologyDirection = asn1_NOVALUE} = TR1, TRs) ->
    TR2 = TR1#'TopologyRequest'{topologyDirection          = oneway,
				topologyDirectionExtension = EDir},
    merge_topologyDescriptor(Comps, TR2, TRs);
merge_topologyDescriptor([{direction_ext, EDir}|Comps], TR1, TRs) ->
    TR2 = TR1#'TopologyRequest'{topologyDirectionExtension = EDir},
    merge_topologyDescriptor(Comps, TR2, TRs);
merge_topologyDescriptor(Comps, TR, TRs) ->
    return_error(0, {bad_topologyDescriptor, Comps, TR, TRs}).


ensure_TopologyRequest(#'TopologyRequest'{terminationFrom   = From,
					  terminationTo     = To,
					  topologyDirection = Dir} = R)
  when (From =/= asn1_NOVALUE) andalso 
       (To   =/= asn1_NOVALUE) andalso 
       (Dir  =/= asn1_NOVALUE) ->
    R;
ensure_TopologyRequest(R) ->
    return_error(0, {bad_TopologyRequest, R}).

       
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_profile,1}]}).
-endif.
ensure_profile(Token) ->
    {_TokenTag, Line, Text} = Token, 
    case string:tokens(Text, [$/]) of
        [Name, Version] ->
            Version2 = ensure_version(Version),
            #'ServiceChangeProfile'{profileName = Name, version = Version2};
        _ ->
            return_error(Line, {bad_profile, Text})
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_version,1}]}).
-endif.
ensure_version(Version) ->
    ensure_uint(Version, 0, 99).

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_signalRequest,2}]}).
-endif.
merge_signalRequest(SignalName, PropertyParms) ->
    Sig = #'Signal'{signalName = SignalName},
    SPL = [],
    do_merge_signalRequest(Sig, PropertyParms, SPL).

do_merge_signalRequest(Sig, [H | T], SPL) ->
    case H of
        {stream, SID} when Sig#'Signal'.streamID =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{streamID = SID}, T, SPL);
        {signal_type, SigType} when Sig#'Signal'.sigType =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{sigType = SigType}, T, SPL);
        {duration, Duration} when Sig#'Signal'.duration =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{duration = Duration}, T, SPL);
        {notify_completion, NC} when Sig#'Signal'.notifyCompletion =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{notifyCompletion = NC}, T, SPL);
        keepActive when Sig#'Signal'.keepActive =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{keepActive = true}, T, SPL);
        {other, Name, PP} ->
            SP = #'SigParameter'{sigParameterName = Name, 
				 value            = PP#'PropertyParm'.value,
				 extraInfo        = PP#'PropertyParm'.extraInfo},
            do_merge_signalRequest(Sig, T, [SP | SPL]);
        {direction, Dir} when Sig#'Signal'.direction =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{direction = Dir}, T, SPL);
        {requestId, RID} when Sig#'Signal'.requestID =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{requestID = RID}, T, SPL);
        {intersigDelay, ISD} when Sig#'Signal'.intersigDelay =:= asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{intersigDelay = ISD}, T, SPL);
        _ ->
            return_error(0, {bad_sigParm, H})
    end;
do_merge_signalRequest(Sig, [], SPL) ->
    Sig#'Signal'{sigParList = lists:reverse(SPL)} .

%% eventStream = StreamToken EQUAL StreamID
%% eventOther  = eventParameterName parmValue
-ifdef(megaco_parser_inline).
-compile({inline,[{select_stream_or_other,2}]}).
-endif.
select_stream_or_other(EventParameterName, ParmValue) ->
    if
	(EventParameterName =:= "st") orelse 
	(EventParameterName =:= "stream") ->
	    case ParmValue of
		#'PropertyParm'{value = [Value]} ->
		    {stream, ensure_uint16(Value)};
		_ ->
		    {stream, ensure_uint16(ParmValue)}
	    end;
	true ->
	    #'PropertyParm'{value = Value} = ParmValue, 
	    EP = #'EventParameter'{eventParameterName = EventParameterName,
				   value              = Value},
	    {other, EP}
    end.

%% select_stream_or_other("st", #'PropertyParm'{value = [Value]}) ->
%%     {stream, ensure_uint16(Value)};
%% select_stream_or_other("st", Value) ->
%%     {stream, ensure_uint16(Value)};
%% select_stream_or_other("stream", #'PropertyParm'{value = [Value]}) ->
%%     {stream, ensure_uint16(Value)};
%% select_stream_or_other("stream", Value) ->
%%     {stream, ensure_uint16(Value)};
%% select_stream_or_other(Name, #'PropertyParm'{value = Value}) ->
%%     EP = #'EventParameter'{eventParameterName = Name,
%%  			   value              = Value},
%%     {other, EP}.


-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_eventDM,1}]}).
-endif.
ensure_eventDM(Token) ->
    {_TokenTag, Line, DMD} = Token,
    if 
	is_record(DMD, 'DigitMapDescriptor') ->
	    Name = DMD#'DigitMapDescriptor'.digitMapName,
	    Val  = DMD#'DigitMapDescriptor'.digitMapValue,
	    if
		(Name  =:= asn1_NOVALUE) andalso (Val =/= asn1_NOVALUE) ->
		    {'DigitMapValue', Start, Short, Long, Duration, Body} = Val,
		    DMV = #'DigitMapValue'{startTimer    = Start, 
					   shortTimer    = Short, 
					   longTimer     = Long, 
					   digitMapBody  = Body,
					   durationTimer = Duration},
		    {eventDM, {digitMapValue, DMV}};
		(Name =/= asn1_NOVALUE) andalso (Val =:= asn1_NOVALUE) ->
		    {eventDM, {digitMapName, Name}};
		true ->
		    return_error(Line, {bad_eventDM, DMD})
	    end;
	true ->
	    return_error(Line, {bad_eventDM, DMD})
    end.
	    
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_DMD,1}]}).
-endif.
ensure_DMD(Token) ->
    {_TokenTag, Line, DMD} = Token,
    if 
	is_record(DMD, 'DigitMapDescriptor') ->
	    Val2 = 
		case DMD#'DigitMapDescriptor'.digitMapValue of
		    %% Note that the values of the digitMapBody and 
		    %% durationTimers are swapped by the scanner 
		    %% (this is done because of a problem in the flex scanner).
		    #'DigitMapValue'{startTimer    = asn1_NOVALUE,
				     shortTimer    = asn1_NOVALUE,
				     longTimer     = asn1_NOVALUE,
				     durationTimer = [],
				     digitMapBody  = asn1_NOVALUE} ->
			asn1_NOVALUE;
		    #'DigitMapValue'{durationTimer = Body,
				     digitMapBody  = Duration} = DMV ->
			%% Convert to version 1 DigitMapValue
			DMV#'DigitMapValue'{digitMapBody  = Body,
					    durationTimer = Duration};
		    Other ->
			Other
		end,
	    DMD#'DigitMapDescriptor'{digitMapValue = Val2};
	true ->
	    return_error(Line, {bad_DigitMapDescriptor, DMD})
    end.


-ifdef(megaco_parser_inline).
-compile({inline,[{merge_observed_event,3}]}).
-endif.
merge_observed_event(ObservedEvents, EventName, TimeStamp) ->
    StreamId = asn1_NOVALUE,
    EPL = [],
    do_merge_observed_event(ObservedEvents, EventName, TimeStamp, StreamId, EPL).

do_merge_observed_event([{stream, StreamID} | T], EventName, TimeStamp, asn1_NOVALUE, EPL) ->
    do_merge_observed_event(T, EventName, TimeStamp, StreamID, EPL);
do_merge_observed_event([{other, PP} | T], EventName, TimeStamp, StreamID, EPL) ->
    do_merge_observed_event(T, EventName, TimeStamp, StreamID, [PP | EPL]);
do_merge_observed_event([], EventName, TimeStamp, StreamID, EPL) ->
    #'ObservedEvent'{eventName    = EventName,
                     timeNotation = TimeStamp,
                     streamID     = StreamID,
                     eventParList = lists:reverse(EPL)}.

merge_eventSpec(OE) 
  when is_record(OE, 'ObservedEvent') andalso 
       (OE#'ObservedEvent'.timeNotation =:= asn1_NOVALUE) ->
    #'EventSpec'{eventName     = OE#'ObservedEvent'.eventName,
                 streamID      = OE#'ObservedEvent'.streamID,
                 eventParList  = OE#'ObservedEvent'.eventParList};
merge_eventSpec(OE) ->
    return_error(0, {bad_event_spec, OE}).

make_RegulatedEmbeddedDescriptor({embed, SD, SED}) ->
    #'RegulatedEmbeddedDescriptor'{secondEvent       = SED,
				   signalsDescriptor = SD}.

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_eventParameters,1}]}).
-endif.
merge_eventParameters(Params) ->
    SID  = asn1_NOVALUE,
    EPL  = [],
    RA   = #'RequestedActions'{},
    HasA = no,
    do_merge_eventParameters(Params, SID, EPL, RA, HasA) .
                                   
do_merge_eventParameters([H | T], SID, EPL, RA, HasA) ->
    case H of
        keepActive when RA#'RequestedActions'.keepActive =:= asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{keepActive = true},
            do_merge_eventParameters(T, SID, EPL, RA2, yes);
        resetEventsDescriptor when RA#'RequestedActions'.resetEventsDescriptor =:= asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{resetEventsDescriptor = 'NULL'},
            do_merge_eventParameters(T, SID, EPL, RA2, yes);
        {embed, SD, SED} when RA#'RequestedActions'.signalsDescriptor =:= asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{signalsDescriptor = SD,
					secondEvent       = SED},
            do_merge_eventParameters(T, SID, EPL, RA2, yes);
        {eventDM, DM} when RA#'RequestedActions'.eventDM =:= asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{eventDM = DM},
            do_merge_eventParameters(T, SID, EPL, RA2, yes);
        {stream, NewSID} when SID =:= asn1_NOVALUE ->
            do_merge_eventParameters(T, NewSID, EPL, RA, HasA);
        {other, PP} when is_record(PP, 'PropertyParm') ->
            EP = #'EventParameter'{eventParameterName = PP#'PropertyParm'.name,
                                   value              = PP#'PropertyParm'.value,
				   extraInfo          = PP#'PropertyParm'.extraInfo},
            do_merge_eventParameters(T, SID, [EP | EPL], RA, HasA);
        {other, EP} when is_record(EP, 'EventParameter') ->
            do_merge_eventParameters(T, SID, [EP | EPL], RA, HasA);
        {notifyBehaviour, NB} when RA#'RequestedActions'.notifyBehaviour =:= asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{notifyBehaviour = NB},
            do_merge_eventParameters(T, SID, EPL, RA2, yes);
        _ ->
            return_error(0, {bad_eventParameter, H})
    end;
do_merge_eventParameters([], SID, EPL, RA, yes) ->
    #'RequestedEvent'{streamID    = SID,
                      eventAction = RA, 
                      evParList   = lists:reverse(EPL)};
do_merge_eventParameters([], SID, EPL, _RA, no) ->
    #'RequestedEvent'{streamID    = SID,
                      eventAction = asn1_NOVALUE, 
                      evParList   = lists:reverse(EPL)}.

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_secondEventParameters,1}]}).
-endif.
merge_secondEventParameters(Params) ->
    SID  = asn1_NOVALUE,
    EPL  = [],
    SRA  = #'SecondRequestedActions'{},
    HasA = no,
    do_merge_secondEventParameters(Params, SID, EPL, SRA, HasA) .
                                   
do_merge_secondEventParameters([H | T], SID, EPL, SRA, HasA) ->
    case H of
        keepActive when SRA#'SecondRequestedActions'.keepActive =:= asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{keepActive = true},
            do_merge_secondEventParameters(T, SID, EPL, SRA2, yes);
        resetEventsDescriptor when SRA#'SecondRequestedActions'.resetEventsDescriptor =:= asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{resetEventsDescriptor = 'NULL'},
            do_merge_secondEventParameters(T, SID, EPL, SRA2, yes);
        {second_embed, SD} when SRA#'SecondRequestedActions'.signalsDescriptor =:= asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{signalsDescriptor = SD},
            do_merge_secondEventParameters(T, SID, EPL, SRA2, yes);
        {eventDM, DM} when SRA#'SecondRequestedActions'.eventDM =:= asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{eventDM = DM},
            do_merge_secondEventParameters(T, SID, EPL, SRA2, yes);
        {stream, NewSID} when SID =:= asn1_NOVALUE ->
            do_merge_secondEventParameters(T, NewSID, EPL, SRA, HasA);
        {other, PP} when is_record(PP, 'PropertyParm') ->
            EP = #'EventParameter'{eventParameterName = PP#'PropertyParm'.name,
                                   value              = PP#'PropertyParm'.value,
				   extraInfo          = PP#'PropertyParm'.extraInfo},
            do_merge_secondEventParameters(T, SID, [EP | EPL], SRA, HasA);
        {other, EP} when is_record(EP, 'EventParameter') ->
            do_merge_secondEventParameters(T, SID, [EP | EPL], SRA, HasA);
        {notifyBehaviour, NB} when SRA#'SecondRequestedActions'.notifyBehaviour =:= asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{notifyBehaviour = NB},
            do_merge_secondEventParameters(T, SID, EPL, SRA2, yes);
        _ ->
            return_error(0, {bad_secondEventParameter, H})
    end;
do_merge_secondEventParameters([], SID, EPL, SRA, yes) ->
    #'SecondRequestedEvent'{streamID    = SID,
                            eventAction = SRA, 
                            evParList   = lists:reverse(EPL)};
do_merge_secondEventParameters([], SID, EPL, _SRA, no) ->
    #'SecondRequestedEvent'{streamID    = SID,
                            eventAction = asn1_NOVALUE, 
                            evParList   = lists:reverse(EPL)}.

%% terminationID     = "ROOT" / pathName / "$" / "*"
%% Total length of pathName must not exceed 64 chars.
%% pathName          = ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" )
%%                     ["@" pathDomainName ]
%% ABNF allows two or more consecutive "." although it is meaningless
%% in a path domain name.
%% pathDomainName    = (ALPHA / DIGIT / "*" )
%%                        *63(ALPHA / DIGIT / "-" / "*" / ".")
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_terminationID,1}]}).
-endif.
ensure_terminationID(Token) ->
    {safeToken, _Line, LowerText} = Token, 
    %% terminationID = "ROOT" / pathName / "$" / "*"
    decode_term_id(LowerText, false, [], []).

decode_term_id([H | T], Wild, Id, Component) ->
    case H of
        $/ -> decode_term_id(T, Wild, [lists:reverse(Component) | Id], []);
        $* -> decode_term_id(T, true, Id, [?megaco_all    | Component]);
        $$ -> decode_term_id(T, true, Id, [?megaco_choose | Component]);
        _  -> decode_term_id(T, Wild, Id, [H | Component])
    end;
decode_term_id([], Wild, Id, Component) ->
    Id2 = [lists:reverse(Component) | Id],
    #megaco_term_id{contains_wildcards = Wild, id = lists:reverse(Id2)}.
            
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_pathName,1}]}).
-endif.
ensure_pathName(Token) ->
    {_TokenTag, _Line, Text} = Token, 
    Text.  %% BUGBUG: ensure values

%% TimeStamp            = Date "T" Time ; per ISO 8601:1988
%% Date                 = 8(DIGIT) ; Date = yyyymmdd
%% Time                 = 8(DIGIT) ; Time = hhmmssss
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_timeStamp,1}]}).
-endif.
ensure_timeStamp(Token) ->
    {'TimeStampToken', Line, Text} = Token, 
    case string:tokens(Text, [$T, $t]) of
        [Date, Time] ->
            #'TimeNotation'{date = Date, time = Time};
        _ ->
            return_error(Line, {bad_timeStamp, Text})
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_transactionID,1}]}).
-endif.
ensure_transactionID(TransId) ->
    ensure_uint32(TransId).

make_transactionID_and_segment_info({TokenTag, Line, Text}) ->
    case string:tokens(Text, [$/]) of
	[TidText, SegNoText, SegComplText] ->
	    Tid   = ensure_uint32({TokenTag, Line, TidText}),
	    SegNo = ensure_uint16({TokenTag, Line, SegNoText}),
	    ensure_segmentationComplete({TokenTag, Line, SegComplText}),
	    {Tid, SegNo, 'NULL'};
	[TidText, SegNoText] ->
	    Tid   = ensure_uint32({TokenTag, Line, TidText}),
	    SegNo = ensure_uint16({TokenTag, Line, SegNoText}),
	    {Tid, SegNo};
	[TidText] ->
	    ensure_uint32({TokenTag, Line, TidText})
    end.
    
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_segmentationComplete,1}]}).
-endif.
ensure_segmentationComplete(Token) ->
    {_TokenTag, Line, Text} = Token,
    if
	(Text =:= "end") orelse (Text =:= "&") ->
	    ok;
	true ->
	    return_error(Line, {invalid_segmentationCompleteToken, Text})
    end.

make_TransactionReply({Tid, SegNo, 'NULL'}, IAR, Res) ->
    #'TransactionReply'{transactionId        = Tid,
			immAckRequired       = IAR,
			transactionResult    = Res,
			segmentNumber        = SegNo,
			segmentationComplete = 'NULL'};
make_TransactionReply({Tid, SegNo}, IAR, Res) ->
    #'TransactionReply'{transactionId        = Tid,
			immAckRequired       = IAR,
			transactionResult    = Res,
			segmentNumber        = SegNo};
make_TransactionReply(Tid, IAR, Res) ->
    #'TransactionReply'{transactionId        = Tid,
			immAckRequired       = IAR,
			transactionResult    = Res}.

make_SegmentReply({Tid, SegNo, 'NULL'}) ->
    #'SegmentReply'{transactionId        = Tid,
		    segmentNumber        = SegNo,
		    segmentationComplete = 'NULL'};
make_SegmentReply({Tid, SegNo}) ->
    #'SegmentReply'{transactionId        = Tid,
		    segmentNumber        = SegNo}.


%% transactionAck = transactionID / (transactionID "-" transactionID)
-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_transactionAck,1}]}).
-endif.
ensure_transactionAck(Tokens) ->
    {safeToken, _Line, Text} = Tokens, 
    case string:tokens(Text, [$-]) of
        [Id] ->
            #'TransactionAck'{firstAck = ensure_transactionID(Id)};
        [Id, Id2] ->
            #'TransactionAck'{firstAck = ensure_transactionID(Id),
			      lastAck  = ensure_transactionID(Id2)}
    end.

merge_context_request(asn1_NOVALUE, Prop) ->
    merge_context_request(#'ContextRequest'{}, Prop);

merge_context_request(#'ContextRequest'{priority = asn1_NOVALUE} = CR, 
                      {priority, Int}) ->
    CR#'ContextRequest'{priority = Int};

merge_context_request(#'ContextRequest'{emergency = asn1_NOVALUE} = CR, 
                      {emergency, Bool}) ->
    CR#'ContextRequest'{emergency = Bool};

merge_context_request(#'ContextRequest'{topologyReq = asn1_NOVALUE} = CR, 
                      {topology, Desc}) ->
    CR#'ContextRequest'{topologyReq = Desc};

merge_context_request(#'ContextRequest'{iepscallind = asn1_NOVALUE} = CR, 
                      {iepsCallind, Ind}) ->
    CR#'ContextRequest'{iepscallind = Ind};

merge_context_request(#'ContextRequest'{contextProp = asn1_NOVALUE} = CR,
                      {contextProp, Props}) ->
    CR#'ContextRequest'{contextProp = Props};

merge_context_request(#'ContextRequest'{contextList = asn1_NOVALUE} = CR, 
                      {contextList, IDs}) -> 
    CR#'ContextRequest'{contextList = IDs};

merge_context_request(CR, {Tag, Val}) ->
    Val2 = 
        case Tag of
            priority    -> CR#'ContextRequest'.priority;
            emergency   -> CR#'ContextRequest'.emergency;
            topology    -> CR#'ContextRequest'.topologyReq;
            iepsCallind -> CR#'ContextRequest'.iepscallind;
            contextProp -> CR#'ContextRequest'.contextProp;
	    contextList -> CR#'ContextRequest'.contextList
        end,
    exit({at_most_once_contextProperty, {Tag, Val, Val2}}).
	    

merge_context_attr_audit_request(
  #'ContextAttrAuditRequest'{contextPropAud = asn1_NOVALUE} = CAAR, []) ->
    
    CAAR;
merge_context_attr_audit_request(
  #'ContextAttrAuditRequest'{contextPropAud = CPA} = CAAR, []) ->
    
    CAAR#'ContextAttrAuditRequest'{contextPropAud = lists:reverse(CPA)};
merge_context_attr_audit_request(CAAR, [H|T]) ->
    case H of
	priorityAudit when CAAR#'ContextAttrAuditRequest'.priority =:= asn1_NOVALUE ->
            CAAR2 = CAAR#'ContextAttrAuditRequest'{priority = 'NULL'},
            merge_context_attr_audit_request(CAAR2, T);

        emergencyAudit when CAAR#'ContextAttrAuditRequest'.emergency =:= asn1_NOVALUE ->
            CAAR2 = CAAR#'ContextAttrAuditRequest'{emergency = 'NULL'},
            merge_context_attr_audit_request(CAAR2, T);

        topologyAudit when CAAR#'ContextAttrAuditRequest'.topology =:= asn1_NOVALUE ->
            CAAR2 = CAAR#'ContextAttrAuditRequest'{topology = 'NULL'},
	    merge_context_attr_audit_request(CAAR2, T);

        iepsCallind when CAAR#'ContextAttrAuditRequest'.iepscallind =:= asn1_NOVALUE ->
            CAAR2 = CAAR#'ContextAttrAuditRequest'{iepscallind = 'NULL'},
	    merge_context_attr_audit_request(CAAR2, T);

	{prop, Name} when CAAR#'ContextAttrAuditRequest'.contextPropAud =:= asn1_NOVALUE ->
	    CPA = [#'IndAudPropertyParm'{name = Name}],
	    CAAR2 = CAAR#'ContextAttrAuditRequest'{contextPropAud = CPA},
	    merge_context_attr_audit_request(CAAR2, T);

	{prop, Name} ->
	    CPA   = CAAR#'ContextAttrAuditRequest'.contextPropAud,
	    CPA2  = [#'IndAudPropertyParm'{name = Name}|CPA],
	    CAAR2 = CAAR#'ContextAttrAuditRequest'{contextPropAud = CPA2},
	    merge_context_attr_audit_request(CAAR2, T);

	{select_prio, Prio} when CAAR#'ContextAttrAuditRequest'.selectpriority =:= asn1_NOVALUE ->
	    CAAR2 = CAAR#'ContextAttrAuditRequest'{selectpriority = Prio},
	    merge_context_attr_audit_request(CAAR2, T);

	{select_emergency, EV} when CAAR#'ContextAttrAuditRequest'.selectemergency =:= asn1_NOVALUE ->
	    CAAR2 = CAAR#'ContextAttrAuditRequest'{selectemergency = EV},
	    merge_context_attr_audit_request(CAAR2, T);

	{select_ieps, IV} when CAAR#'ContextAttrAuditRequest'.selectiepscallind =:= asn1_NOVALUE ->
	    CAAR2 = CAAR#'ContextAttrAuditRequest'{selectiepscallind = IV},
	    merge_context_attr_audit_request(CAAR2, T);

	{select_logic, SL} when CAAR#'ContextAttrAuditRequest'.selectLogic =:= asn1_NOVALUE ->
	    CAAR2 = CAAR#'ContextAttrAuditRequest'{selectLogic = SL},
	    merge_context_attr_audit_request(CAAR2, T);

	%% BUGBUG BUGBUG BUGBUG BUGBUG BUGBUG BUGBUG BUGBUG BUGBUG BUGBUG
	%% 
	%% For some strange reason, contextAttrDescriptor was added
	%% to contextAuditSelector. But there is no place for this
	%% info in the ContextAttrAuditRequest. Since contextAttrDescriptor
	%% can also be found in contextProperty (which correspond to
	%% ContextRequest), the question is if this info should go there
	%% or if we shall just drop it. For now we drop it.
	%% 
	{contextProp, _PPs} ->
	    merge_context_attr_audit_request(CAAR, T);

	{contextList, _IDs} ->
	    merge_context_attr_audit_request(CAAR, T);

	_ ->
	    exit({unexpected_contextAttrAudit_item, H})
    
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_action_request,2}]}).
-endif.
merge_action_request(CtxId, Items) ->
    do_merge_action_request(Items, [], asn1_NOVALUE, asn1_NOVALUE, CtxId).

do_merge_action_request([H|T], CmdReqs, CtxReq, CtxAuditReq, CtxId) ->
    case H of
	{commandRequest, CmdReq} ->
	    do_merge_action_request(T, [CmdReq|CmdReqs], 
				    CtxReq, CtxAuditReq, CtxId);

	{contextProp, ContextProp} ->
	    do_merge_action_request(T, CmdReqs, 
				    merge_context_request(CtxReq, ContextProp),
				    CtxAuditReq, CtxId);

	{contextAudit, ContextAuditReq} when CtxAuditReq =:= asn1_NOVALUE ->
	    do_merge_action_request(T, CmdReqs, 
				    CtxReq, ContextAuditReq, CtxId)
    end;
do_merge_action_request([], CmdReqs, CtxReq, CtxAuditReq, CtxId) ->
    #'ActionRequest'{contextId           = CtxId,
                     contextRequest      = strip_ContextRequest(CtxReq),
                     contextAttrAuditReq = strip_ContextAttrAuditRequest(CtxAuditReq),
                     commandRequests     = lists:reverse(CmdReqs)}.


%% OTP-5085: 
%% In order to solve a problem in the parser, the error descriptor
%% has been put last in the non-empty commandReplyList, if it is not 
%% asn1_NOVALUE
-ifdef(megaco_parser_inline).
-compile({inline,[{merge_action_reply,1}]}).
-endif.
merge_action_reply(Items) ->
    do_merge_action_reply(Items, asn1_NOVALUE, asn1_NOVALUE, []).

do_merge_action_reply([], Err, Ctx, Cmds) ->
    #'ActionReply'{errorDescriptor = Err,
		   contextReply    = strip_ContextRequest(Ctx),
		   commandReply    = lists:reverse(Cmds)};
do_merge_action_reply([H|T], Err0, CR, Cmds) ->
    case H of
        {error, Err1} when Err0 =:= asn1_NOVALUE ->
            do_merge_action_reply(T, Err1, CR, Cmds);
        {command, Cmd} ->
            do_merge_action_reply(T, Err0, CR, [Cmd | Cmds]);
        {context, CtxProp} ->
	    do_merge_action_reply(T, Err0, 
				  merge_context_request(CR, CtxProp), Cmds)
    end.

merge_auditOther([TID], TAR) ->
    {auditResult, 
     #'AuditResult'{terminationID          = TID,
		    terminationAuditResult = TAR}};
merge_auditOther(TIDs, TAR) ->
    {auditResultTermList,
     #'TermListAuditResult'{terminationIDList      = TIDs,
			    terminationAuditResult = TAR}}.

strip_ContextRequest(#'ContextRequest'{priority    = asn1_NOVALUE,
				       emergency   = asn1_NOVALUE,
				       topologyReq = asn1_NOVALUE,
				       iepscallind = asn1_NOVALUE,
				       contextProp = asn1_NOVALUE,
				       contextList = asn1_NOVALUE}) ->
    asn1_NOVALUE;
strip_ContextRequest(#'ContextRequest'{priority    = asn1_NOVALUE,
				       emergency   = asn1_NOVALUE,
				       topologyReq = asn1_NOVALUE,
				       iepscallind = asn1_NOVALUE,
				       contextProp = [],
				       contextList = asn1_NOVALUE}) ->
    asn1_NOVALUE;
%% strip_ContextRequest(asn1_NOVALUE) ->
%%     asn1_NOVALUE;
strip_ContextRequest(R) ->
    R.

strip_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{priority          = asn1_NOVALUE,
			     emergency         = asn1_NOVALUE,
			     topology          = asn1_NOVALUE,
			     iepscallind       = asn1_NOVALUE,
			     contextPropAud    = asn1_NOVALUE,
			     selectpriority    = asn1_NOVALUE,
			     selectemergency   = asn1_NOVALUE,
			     selectiepscallind = asn1_NOVALUE, 
			     selectLogic       = asn1_NOVALUE}) ->
    asn1_NOVALUE;
strip_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{priority          = asn1_NOVALUE,
			     emergency         = asn1_NOVALUE,
			     topology          = asn1_NOVALUE,
			     iepscallind       = asn1_NOVALUE,
			     contextPropAud    = [],
			     selectpriority    = asn1_NOVALUE,
			     selectemergency   = asn1_NOVALUE,
			     selectiepscallind = asn1_NOVALUE, 
			     selectLogic       = asn1_NOVALUE}) ->
    asn1_NOVALUE;
strip_ContextAttrAuditRequest(R) ->
    R.

merge_AmmRequest_descriptors([], Acc) ->
    lists:reverse(Acc);
merge_AmmRequest_descriptors([{_, deprecated}|Descs], Acc) ->
    merge_AmmRequest_descriptors(Descs, Acc);
merge_AmmRequest_descriptors([Desc|Descs], Acc) ->
    merge_AmmRequest_descriptors(Descs, [Desc|Acc]).

make_auditRequest([TID], AD) ->
    #'AuditRequest'{terminationID   = TID,
		    auditDescriptor = AD};
make_auditRequest([TID|_] = TIDList, AD) ->
    #'AuditRequest'{terminationID     = TID,
		    auditDescriptor   = AD,
		    terminationIDList = TIDList}.
    
make_commandRequest({CmdTag, {_TokenTag, _Line, Text}}, Cmd) ->
    Req = #'CommandRequest'{command  = {CmdTag, Cmd}},
    case Text of
        [$w, $- | _] ->
            Req#'CommandRequest'{wildcardReturn = 'NULL'};
        [$o, $-, $w, $- | _] ->
            Req#'CommandRequest'{optional = 'NULL', wildcardReturn = 'NULL'};
        [$o, $- | _] ->
            Req#'CommandRequest'{optional = 'NULL'};
        _ -> 
            Req
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_terminationAudit,1}]}).
-endif.
merge_terminationAudit(AuditReturnParameters) ->
    lists:reverse(do_merge_terminationAudit(AuditReturnParameters, [], [])).

do_merge_terminationAudit([H| T], ARPs, AuditItems) ->
    case H of
	{auditReturnItem, AuditItem} ->
	    do_merge_terminationAudit(T, ARPs, [AuditItem | AuditItems]);
	AuditReturnParameter ->
	    do_merge_terminationAudit(T, [AuditReturnParameter | ARPs], AuditItems)
    end;
do_merge_terminationAudit([], AuditReturnParameters, []) ->
    AuditReturnParameters;
do_merge_terminationAudit([], AuditReturnParameters, AuditItems) ->
    AuditDescriptor = #'AuditDescriptor'{auditToken = AuditItems},
    AuditReturnParameter = {emptyDescriptors, AuditDescriptor},
    [AuditReturnParameter | AuditReturnParameters].
        
-ifdef(megaco_parser_inline).
-compile({inline,[{merge_mediaDescriptor,1}]}).
-endif.
merge_mediaDescriptor(MediaParms) ->
    do_merge_mediaDescriptor(MediaParms, asn1_NOVALUE, [], []).

do_merge_mediaDescriptor([H | T], TS, One, Multi) ->
    case H of
        {streamParm, Parm} when Multi =:= [] ->
            do_merge_mediaDescriptor(T, TS, [Parm | One], Multi);
        {streamDescriptor, Desc} when One =:= [] ->
            do_merge_mediaDescriptor(T, TS, One, [Desc | Multi]);
        {termState, TS2} when TS  =:= asn1_NOVALUE ->
            do_merge_mediaDescriptor(T, TS2, One, Multi);
        _ ->
            return_error(0, {bad_merge_mediaDescriptor, [H, TS, One, Multi]})
    end;
do_merge_mediaDescriptor([], TS, One, Multi) ->
    if
	(One =:= []) ->
	    if (Multi =:= []) ->
		    #'MediaDescriptor'{streams        = asn1_NOVALUE,
				       termStateDescr = TS};
	       true -> % (Multi =/= [])
		    Streams = {multiStream, lists:reverse(Multi)}, 
		    #'MediaDescriptor'{streams        = Streams,
				       termStateDescr = TS}
	    end;
	true -> % (One =/= [])
	    if 
		(Multi =:= []) ->
		    Streams = {oneStream, merge_streamParms(One)}, 
		    #'MediaDescriptor'{streams        = Streams,
				       termStateDescr = TS};
		true -> % (Multi =/= [])
		   return_error(0, 
				{bad_merge_mediaDescriptor, [TS, One, Multi]}) 
	    end
    end.
  
-ifdef(megaco_parser_inline).
-compile({inline,[{merge_streamParms,1}]}).
-endif.
merge_streamParms(TaggedStreamParms) ->
    SP = #'StreamParms'{},
    do_merge_streamParms(TaggedStreamParms, SP).

do_merge_streamParms([{Tag, D} | T] = All, SP) ->
    case Tag of
        local when SP#'StreamParms'.localDescriptor  =:= asn1_NOVALUE ->
            do_merge_streamParms(T, SP#'StreamParms'{localDescriptor = D});
        remote when SP#'StreamParms'.remoteDescriptor =:= asn1_NOVALUE ->
            do_merge_streamParms(T, SP#'StreamParms'{remoteDescriptor = D});
        control ->
            LCD = 
                case SP#'StreamParms'.localControlDescriptor of
                    asn1_NOVALUE ->
                        #'LocalControlDescriptor'{propertyParms = []};
                    PrevLCD ->
                        PrevLCD
                end,
            LCD2 = do_merge_control_streamParms(D, LCD),
            do_merge_streamParms(T, SP#'StreamParms'{localControlDescriptor = LCD2});
	statistics when SP#'StreamParms'.statisticsDescriptor =:= asn1_NOVALUE ->
	    do_merge_streamParms(T, SP#'StreamParms'{statisticsDescriptor = D});
        _ ->
            return_error(0, {do_merge_streamParms, [All, SP]})
    end;
do_merge_streamParms([], SP) 
  when is_record(SP#'StreamParms'.localControlDescriptor, 'LocalControlDescriptor') ->
    LCD  = SP#'StreamParms'.localControlDescriptor,
    PP   = LCD#'LocalControlDescriptor'.propertyParms,
    LCD2 = LCD#'LocalControlDescriptor'{propertyParms = lists:reverse(PP)},
    SP#'StreamParms'{localControlDescriptor = LCD2};
do_merge_streamParms([], SP) ->
    SP.


do_merge_control_streamParms([{SubTag, SD} | T] = All, LCD) ->
    case SubTag of
        group when LCD#'LocalControlDescriptor'.reserveGroup =:= asn1_NOVALUE ->
            LCD2 = LCD#'LocalControlDescriptor'{reserveGroup = SD},
            do_merge_control_streamParms(T, LCD2);
        value when LCD#'LocalControlDescriptor'.reserveValue =:= asn1_NOVALUE ->
            LCD2 = LCD#'LocalControlDescriptor'{reserveValue = SD},
            do_merge_control_streamParms(T, LCD2);
        mode when LCD#'LocalControlDescriptor'.streamMode =:= asn1_NOVALUE ->
            LCD2 = LCD#'LocalControlDescriptor'{streamMode = SD},
            do_merge_control_streamParms(T, LCD2);
        prop ->
            PP = LCD#'LocalControlDescriptor'.propertyParms,
            LCD2 = LCD#'LocalControlDescriptor'{propertyParms = [SD | PP]},
            do_merge_control_streamParms(T, LCD2);
        _ ->
            return_error(0, {do_merge_control_streamParms, [All, LCD]})
  end;
do_merge_control_streamParms([], LCD) ->
    LCD.

-ifdef(megaco_parser_inline).
-compile({inline,[{merge_terminationStateDescriptor,1}]}).
-endif.
merge_terminationStateDescriptor(Parms) ->
    TSD = #'TerminationStateDescriptor'{propertyParms = []},
    do_merge_terminationStateDescriptor(Parms, TSD).

do_merge_terminationStateDescriptor([{Tag, Val} | T], TSD) ->
    case Tag of
        serviceState when TSD#'TerminationStateDescriptor'.serviceState =:= asn1_NOVALUE ->
            TSD2 = TSD#'TerminationStateDescriptor'{serviceState = Val},
            do_merge_terminationStateDescriptor(T, TSD2);
        eventBufferControl when TSD#'TerminationStateDescriptor'.eventBufferControl =:= asn1_NOVALUE->
            TSD2 = TSD#'TerminationStateDescriptor'{eventBufferControl = Val},
            do_merge_terminationStateDescriptor(T, TSD2);
        propertyParm ->
            PP = TSD#'TerminationStateDescriptor'.propertyParms,
            TSD2 = TSD#'TerminationStateDescriptor'{propertyParms = [Val | PP]},
            do_merge_terminationStateDescriptor(T, TSD2)
    end;
do_merge_terminationStateDescriptor([], TSD) ->
    PP = TSD#'TerminationStateDescriptor'.propertyParms,
    TSD#'TerminationStateDescriptor'{propertyParms = lists:reverse(PP)}.

-ifdef(megaco_nscanner_props).

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_prop_groups,1}]}).
-endif.
ensure_prop_groups(Token) ->
    {_TokenTag, _Line, Text} = Token,
    Group  = [],
    Groups = [],
    parse_prop_name(Text, Group, Groups).

parse_prop_name([Char | Rest] = All, Group, Groups) ->
    if 
        ?white_space(Char) ->
            parse_prop_name(Rest, Group, Groups);
        ?end_of_line(Char) ->
            parse_prop_name(Rest, Group, Groups);
        true ->
            Name = [],
            do_parse_prop_name(All, Name, Group, Groups)
    end;
parse_prop_name([] = All, Group, Groups) ->
    Name = [],
    do_parse_prop_name(All, Name, Group, Groups).

do_parse_prop_name([Char | Rest], Name, Group, Groups) 
  when (Char =:= $=) andalso (Name =/= []) ->
    %% Now we have a complete name
    if
	(Name =:= "v") andalso (Group =/= []) ->
	    %% v= is a property group delimiter,
	    %% lets create yet another property group.
	    Groups2 = [lists:reverse(Group) | Groups],
	    Group2 = [],
	    parse_prop_value(Rest, Name, Group2, Groups2);
	true ->
	    %% Use current property group
	    parse_prop_value(Rest, Name, Group, Groups)
    end;
do_parse_prop_name([Char | Rest], Name, Group, Groups) ->
    case ?classify_char4(Char) of
        safe_char_upper ->
            do_parse_prop_name(Rest, [Char | Name], Group, Groups);
        safe_char ->
            do_parse_prop_name(Rest, [Char | Name], Group, Groups);
        _ ->
            return_error(0, {bad_prop_name, lists:reverse(Name), Char})
    end;
do_parse_prop_name([], [], [], Groups) ->
    lists:reverse(Groups);
do_parse_prop_name([], [], Group, Groups) ->
    Group2 = lists:reverse(Group),
    lists:reverse([Group2 | Groups]);
do_parse_prop_name([], Name, Group, Groups) when Name =/= [] ->
    %% Assume end of line
    Value = [],
    PP = make_prop_parm(Name, Value),
    Group2 = lists:reverse([PP | Group]),
    lists:reverse([Group2 | Groups]).

-ifdef(megaco_parser_inline).
-compile({inline,[{parse_prop_value,4}]}).
-endif.
parse_prop_value(Chars, Name, Group, Groups) ->
    Value = [],
    do_parse_prop_value(Chars, Name, Value, Group, Groups).

do_parse_prop_value([Char | Rest], Name, Value, Group, Groups) ->
    if
        ?end_of_line(Char) ->
            %% Now we have a complete "name=value" pair
            PP = make_prop_parm(Name, Value),
            parse_prop_name(Rest, [PP | Group], Groups);
        true ->
            do_parse_prop_value(Rest, Name, [Char | Value], Group, Groups)
    end;
do_parse_prop_value([], Name, Value, Group, Groups) ->
    %% Assume end of line
    PP = make_prop_parm(Name, Value),
    Group2 = lists:reverse([PP | Group]),
    lists:reverse([Group2 | Groups]).

-ifdef(megaco_parser_inline).
-compile({inline,[{make_prop_parm,2}]}).
-endif.
make_prop_parm(Name, Value) ->
    #'PropertyParm'{name  = lists:reverse(Name),
                    value = [lists:reverse(Value)]}.

-else. % -ifdef(megaco_nscanner_props).

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_prop_groups,1}]}).
-endif.
ensure_prop_groups(Token) ->
    {_TokenTag, _Line, Groups} = Token,
    Groups.

%% -ifdef(megaco_parser_inline).
%% -compile({inline,[{do_ensure_prop_groups,1}]}).
%% -endif.
%% do_ensure_prop_groups(Groups) when is_list(Groups) ->
%%     [ensure_prop_group(Group) || Group <- Groups];
%% do_ensure_prop_groups(BadGroups) ->
%%     throw({error, {?MODULE, {bad_property_groups, BadGroups}}}).

%% -ifdef(megaco_parser_inline).
%% -compile({inline,[{ensure_prop_group,1}]}).
%% -endif.
%% ensure_prop_group(Group) when is_list(Group) ->
%%     [ensure_prop_parm(PropParm) || PropParm <- Group];
%% ensure_prop_group(BadGroup) ->
%%     throw({error, {?MODULE, {bad_property_group, BadGroup}}}).

%% -ifdef(megaco_parser_inline).
%% -compile({inline,[{ensure_prop_parm,1}]}).
%% -endif.
%% ensure_prop_parm(#property_parm{name  = Name,
%% 				value = Value}) ->
%%     #'PropertyParm'{name  = Name,
%%                     value = Value};
%% ensure_prop_parm(PP) when is_record(PP, 'PropertyParm') ->
%%     PP;
%% ensure_prop_parm(BadPropParm) ->
%%     throw({error, {?MODULE, {bad_property_parm, BadPropParm}}}).

-endif. % -ifdef(megaco_nscanner_props).

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_uint,3}]}).
-endif.
ensure_uint(Token, Min, Max) ->
    case Token of
	{_TokenTag, Line, Val} when is_integer(Val) ->
	    ensure_uint(Val, Min, Max, Line);
	{_TokenTag, Line, Text} ->
	    case (catch list_to_integer(Text)) of
		{'EXIT', _} ->
		    return_error(Line, {not_an_integer, Text});
		Val when is_integer(Val) ->
		    ensure_uint(Val, Min, Max, Line)
	    end;
	Val when is_integer(Val) ->
	    ensure_uint(Val, Min, Max, 0);
	Text ->
	    case (catch list_to_integer(Text)) of
		{'EXIT', _} ->
		    return_error(0, {not_an_integer, Text});
		Val when is_integer(Val) ->
		    ensure_uint(Val, Min, Max, 0)
	    end
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_uint,4}]}).
-endif.
ensure_uint(Val, Min, Max, Line) ->
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
    end.

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_uint16,1}]}).
-endif.
ensure_uint16(Int) ->
    ensure_uint(Int, 0, 65535).

-ifdef(megaco_parser_inline).
-compile({inline,[{ensure_uint32,1}]}).
-endif.
ensure_uint32(Int) ->
    ensure_uint(Int, 0, 4294967295) .

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

hchar_to_int(Char) when ($0 =< Char) andalso (Char =< $9) ->
    Char - $0;
hchar_to_int(Char) when ($A =< Char) andalso (Char =< $F) ->
    Char - $A + 10; % OTP-4710
hchar_to_int(Char) when ($a =< Char) andalso (Char =< $f) ->
    Char - $a + 10. % OTP-4710

-ifdef(megaco_parser_inline).
-compile({inline,[{value_of,1}]}).
-endif.
value_of(Token) ->
    {_TokenTag, _Line, Text} = Token, 
    Text.


%% -------------------------------------------------------------------

%% d(F) ->
%%     d(F,[]).
%% d(F, A) ->
%%     %% d(true, F, A).
%%     d(get(dbg), F, A).

%% d(true, F, A) ->
%%     io:format("DBG:~w:" ++ F ++ "~n", [?MODULE | A]);
%% d(_, _, _) ->
%%     ok.

