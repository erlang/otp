%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
-module(asn1rt_ber_bin_v2).
 
%% encoding / decoding of BER  

-export([decode/1, decode/2, match_tags/2, encode/1]). 
-export([fixoptionals/2, cindex/3,
	 list_to_record/2,
	 encode_tag_val/1,
	 encode_tags/3,
	 skip_ExtensionAdditions/2]).
-export([encode_boolean/2,decode_boolean/2,
	 encode_integer/3,encode_integer/4,
	 decode_integer/3, decode_integer/4,
	 encode_enumerated/2,
	 encode_enumerated/4,decode_enumerated/4,
	 encode_real/3,decode_real/2,
	 encode_bit_string/4,decode_bit_string/4,
	 decode_compact_bit_string/4,
	 encode_octet_string/3,decode_octet_string/3,
	 encode_null/2,decode_null/2,
	 encode_relative_oid/2,decode_relative_oid/2,
	 encode_object_identifier/2,decode_object_identifier/2,
	 encode_restricted_string/4,decode_restricted_string/4,
	 encode_universal_string/3,decode_universal_string/3,
	 encode_UTF8_string/3,decode_UTF8_string/2,
	 encode_BMP_string/3,decode_BMP_string/3,
	 encode_generalized_time/3,decode_generalized_time/3,
	 encode_utc_time/3,decode_utc_time/3,
	 encode_length/1,decode_length/1,
	 decode_tag_and_length/1]).

-export([encode_open_type/1,encode_open_type/2, 
	 decode_open_type/2,decode_open_type_as_binary/2]).

-export([decode_primitive_incomplete/2,decode_selective/2]).
 
-include("asn1_records.hrl"). 
 
% the encoding of class of tag bits 8 and 7 
-define(UNIVERSAL,   0). 
-define(APPLICATION, 16#40). 
-define(CONTEXT,     16#80). 
-define(PRIVATE,     16#C0). 
 
%%% primitive or constructed encoding % bit 6 
-define(PRIMITIVE,   0). 
-define(CONSTRUCTED, 2#00100000). 

%%% The tag-number for universal types
-define(N_BOOLEAN, 1). 
-define(N_INTEGER, 2). 
-define(N_BIT_STRING, 3).
-define(N_OCTET_STRING, 4).
-define(N_NULL, 5). 
-define(N_OBJECT_IDENTIFIER, 6). 
-define(N_OBJECT_DESCRIPTOR, 7). 
-define(N_EXTERNAL, 8). 
-define(N_REAL, 9). 
-define(N_ENUMERATED, 10). 
-define(N_EMBEDDED_PDV, 11). 
-define(N_SEQUENCE, 16). 
-define(N_SET, 17). 
-define(N_NumericString, 18).
-define(N_PrintableString, 19).
-define(N_TeletexString, 20).
-define(N_VideotexString, 21).
-define(N_IA5String, 22).
-define(N_UTCTime, 23). 
-define(N_GeneralizedTime, 24). 
-define(N_GraphicString, 25).
-define(N_VisibleString, 26).
-define(N_GeneralString, 27).
-define(N_UniversalString, 28).
-define(N_BMPString, 30).

 
% the complete tag-word of built-in types 
-define(T_BOOLEAN,          ?UNIVERSAL bor ?PRIMITIVE bor 1). 
-define(T_INTEGER,          ?UNIVERSAL bor ?PRIMITIVE bor 2). 
-define(T_BIT_STRING,       ?UNIVERSAL bor ?PRIMITIVE bor 3). % can be CONSTRUCTED 
-define(T_OCTET_STRING,     ?UNIVERSAL bor ?PRIMITIVE bor 4). % can be CONSTRUCTED 
-define(T_NULL,             ?UNIVERSAL bor ?PRIMITIVE bor 5). 
-define(T_OBJECT_IDENTIFIER,?UNIVERSAL bor ?PRIMITIVE bor 6). 
-define(T_OBJECT_DESCRIPTOR,?UNIVERSAL bor ?PRIMITIVE bor 7). 
-define(T_EXTERNAL,         ?UNIVERSAL bor ?PRIMITIVE bor 8). 
-define(T_REAL,             ?UNIVERSAL bor ?PRIMITIVE bor 9). 
-define(T_ENUMERATED,       ?UNIVERSAL bor ?PRIMITIVE bor 10). 
-define(T_EMBEDDED_PDV,     ?UNIVERSAL bor ?PRIMITIVE bor 11). 
-define(T_SEQUENCE,         ?UNIVERSAL bor ?CONSTRUCTED bor 16). 
-define(T_SET,              ?UNIVERSAL bor ?CONSTRUCTED bor 17). 
-define(T_NumericString,    ?UNIVERSAL bor ?PRIMITIVE bor 18). %can be constructed 
-define(T_PrintableString,  ?UNIVERSAL bor ?PRIMITIVE bor 19). %can be constructed 
-define(T_TeletexString,    ?UNIVERSAL bor ?PRIMITIVE bor 20). %can be constructed 
-define(T_VideotexString,   ?UNIVERSAL bor ?PRIMITIVE bor 21). %can be constructed 
-define(T_IA5String,        ?UNIVERSAL bor ?PRIMITIVE bor 22). %can be constructed 
-define(T_UTCTime,          ?UNIVERSAL bor ?PRIMITIVE bor 23). 
-define(T_GeneralizedTime,  ?UNIVERSAL bor ?PRIMITIVE bor 24). 
-define(T_GraphicString,    ?UNIVERSAL bor ?PRIMITIVE bor 25). %can be constructed 
-define(T_VisibleString,    ?UNIVERSAL bor ?PRIMITIVE bor 26). %can be constructed 
-define(T_GeneralString,    ?UNIVERSAL bor ?PRIMITIVE bor 27). %can be constructed 
-define(T_UniversalString,  ?UNIVERSAL bor ?PRIMITIVE bor 28). %can be constructed 
-define(T_BMPString,        ?UNIVERSAL bor ?PRIMITIVE bor 30). %can be constructed 
 
% encode(Tlv={_Tag={?PRIMITIVE,_},_VList}) ->
%     encode_primitive(Tlv);
% encode(Tlv) ->
%     encode_constructed(Tlv).

encode([Tlv]) ->
    encode(Tlv);
encode({TlvTag,TlvVal}) when is_list(TlvVal) ->
    %% constructed form of value
    encode_tlv(TlvTag,TlvVal,?CONSTRUCTED);
encode({TlvTag,TlvVal}) ->
    encode_tlv(TlvTag,TlvVal,?PRIMITIVE);
encode(Bin) when is_binary(Bin) ->
    Bin.

encode_tlv(TlvTag,TlvVal,Form) ->
    Tag = encode_tlv_tag(TlvTag,Form),
    {Val,VLen} = encode_tlv_val(TlvVal),
    {Len,_LLen} = encode_length(VLen),
    BinLen = list_to_binary(Len),
    <<Tag/binary,BinLen/binary,Val/binary>>.

encode_tlv_tag(ClassTagNo,Form) ->
    Class = ClassTagNo bsr 16,
    encode_tag_val({Class bsl 6,Form,(ClassTagNo - (Class bsl 16))}).

encode_tlv_val(TlvL) when is_list(TlvL) ->
    encode_tlv_list(TlvL,[]);
encode_tlv_val(Bin) ->
    {Bin,size(Bin)}.

encode_tlv_list([Tlv|Tlvs],Acc) ->
    EncTlv = encode(Tlv),
    encode_tlv_list(Tlvs,[EncTlv|Acc]);
encode_tlv_list([],Acc) ->
    Bin=list_to_binary(lists:reverse(Acc)),
    {Bin,size(Bin)}.

%% asn1-1.6.8.1
%% decode(B,driver) ->
%%     case catch port_control(asn1_driver_port,2,B) of
%% 	Bin when is_binary(Bin) ->
%% 	    binary_to_term(Bin);
%% 	List when is_list(List) -> handle_error(List,B);
%% 	{'EXIT',{badarg,Reason}} ->
%% 	    asn1rt_driver_handler:load_driver(),
%% 	    receive
%% 		driver_ready ->
%% 		    case catch port_control(asn1_driver_port,2,B) of
%% 			Bin2 when is_binary(Bin2) -> binary_to_term(Bin2);
%% 			List when is_list(List) -> handle_error(List,B);
%% 			Error -> exit(Error)
%% 		    end;
%% 		{error,Error} -> % error when loading driver
%% 		    %% the driver could not be loaded
%% 		    exit(Error);
%% 		Error={port_error,Reason} ->
%% 		    exit(Error)
%% 	    end;
%% 	{'EXIT',Reason} ->
%% 	    exit(Reason)
%%     end.

%% asn1-1.6.9
decode(B,driver) ->
    case catch control(?TLV_DECODE,B) of
 	Bin when is_binary(Bin) ->
 	    binary_to_term(Bin);
 	List when is_list(List) -> handle_error(List,B);
 	{'EXIT',{badarg,_Reason}} ->
 	    case asn1rt:load_driver() of
 		ok ->
 		    case control(?TLV_DECODE,B) of
 			Bin when is_binary(Bin) -> binary_to_term(Bin);
 			List when is_list(List) -> handle_error(List,B)
 		    end;
 		Err ->
 		    Err
 	    end
    end.


handle_error([],_)->
    exit({error,{asn1,{"memory allocation problem"}}});
handle_error([$1|_],L) -> % error in driver
    exit({error,{asn1,L}});
handle_error([$2|T],L) -> % error in driver due to wrong tag
    exit({error,{asn1,{"bad tag after byte:",error_pos(T),L}}});
handle_error([$3|T],L) -> % error in driver due to length error
    exit({error,{asn1,{"bad length field after byte:",
			     error_pos(T),L}}});
handle_error([$4|T],L) -> % error in driver due to indefinite length error
    exit({error,{asn1,
		 {"indefinite length without end bytes after byte:",
		  error_pos(T),L}}});
handle_error([$5|T],L) -> % error in driver due to indefinite length error
    exit({error,{asn1,{"bad encoded value after byte:",
			     error_pos(T),L}}});
handle_error(ErrL,L) ->
    exit({error,{asn1,ErrL,L}}).

error_pos([]) ->
    "unknown position";
error_pos([B])->
    B;
error_pos([B|Bs]) ->
    BS = 8 * length(Bs),
    B bsl BS + error_pos(Bs).
%% asn1-1.6.9
control(Cmd, Data) ->
    Port = asn1rt_driver_handler:client_port(),
    erlang:port_control(Port, Cmd, Data).

decode(Bin) when is_binary(Bin) ->
    decode_primitive(Bin);
decode(Tlv) -> % assume it is a tlv
    {Tlv,<<>>}.


decode_primitive(Bin) ->
    {Form,TagNo,V,Rest} = decode_tag_and_length(Bin),
    case Form of 
	1 -> % constructed
	    {{TagNo,decode_constructed(V)},Rest};
	0 -> % primitive
	    {{TagNo,V},Rest};
	2 -> % constructed indefinite
	    {Vlist,Rest2} = decode_constructed_indefinite(V,[]),
	    {{TagNo,Vlist},Rest2}
    end.

decode_constructed(Bin) when byte_size(Bin) =:= 0 ->
    [];
decode_constructed(Bin) ->
    {Tlv,Rest} = decode_primitive(Bin),
    [Tlv|decode_constructed(Rest)].

decode_constructed_indefinite(<<0,0,Rest/binary>>,Acc) ->
    {lists:reverse(Acc),Rest};
decode_constructed_indefinite(Bin,Acc) ->
    {Tlv,Rest} = decode_primitive(Bin),
    decode_constructed_indefinite(Rest, [Tlv|Acc]). 
    
%% decode_primitive_incomplete/2 decodes an encoded message incomplete
%% by help of the pattern attribute (first argument).
decode_primitive_incomplete([[default,TagNo]],Bin) -> %default
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->
	    decode_incomplete2(Form,TagNo,V,[],Rest);
	_ ->
	    %{asn1_DEFAULT,Bin}
	    asn1_NOVALUE
    end;
decode_primitive_incomplete([[default,TagNo,Directives]],Bin) -> %default, constructed type, Directives points into this type
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->	    
	    decode_incomplete2(Form,TagNo,V,Directives,Rest);
	_ ->
	    %{asn1_DEFAULT,Bin}
	    asn1_NOVALUE
    end;
decode_primitive_incomplete([[opt,TagNo]],Bin) -> %optional
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->
	    decode_incomplete2(Form,TagNo,V,[],Rest);
	_ ->
	    %{{TagNo,asn1_NOVALUE},Bin}
	    asn1_NOVALUE
    end;
decode_primitive_incomplete([[opt,TagNo,Directives]],Bin) -> %optional
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->
	    decode_incomplete2(Form,TagNo,V,Directives,Rest);
	_ ->
	    %{{TagNo,asn1_NOVALUE},Bin}
	    asn1_NOVALUE
    end;
%% An optional that shall be undecoded
decode_primitive_incomplete([[opt_undec,Tag]],Bin) ->
    case decode_tag_and_length(Bin) of
	{_,Tag,_,_} ->
	    decode_incomplete_bin(Bin);
	_ ->
	    asn1_NOVALUE
    end;
%% A choice alternative that shall be undecoded
decode_primitive_incomplete([[alt_undec,TagNo]|RestAlts],Bin) ->
%    decode_incomplete_bin(Bin);
%    case decode_tlv(Bin) of
    case decode_tag_and_length(Bin) of
%	{{_Form,TagNo,_Len,_V},_R} ->
	{_,TagNo,_,_} ->
	    decode_incomplete_bin(Bin);
	_ ->
	    decode_primitive_incomplete(RestAlts,Bin)
    end;
decode_primitive_incomplete([[alt,TagNo]|RestAlts],Bin) ->
    case decode_tag_and_length(Bin) of
	{_Form,TagNo,V,Rest} ->
	    {{TagNo,V},Rest};
	_ ->
	    decode_primitive_incomplete(RestAlts,Bin)
    end;
decode_primitive_incomplete([[alt,TagNo,Directives]|RestAlts],Bin) ->
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->
	    decode_incomplete2(Form,TagNo,V,Directives,Rest);
	_ ->
	    decode_primitive_incomplete(RestAlts,Bin)
    end;
decode_primitive_incomplete([[alt_parts,TagNo]],Bin) ->
    case decode_tag_and_length(Bin) of
	{_Form,TagNo,V,Rest} ->
	    {{TagNo,V},Rest};
	_ ->
	    asn1_NOVALUE
    end;
decode_primitive_incomplete([[alt_parts,TagNo]|RestAlts],Bin) ->
    case decode_tag_and_length(Bin) of
	{_Form,TagNo,V,Rest} ->
	    {{TagNo,decode_parts_incomplete(V)},Rest};
	_ ->
	    decode_primitive_incomplete(RestAlts,Bin)
    end;
decode_primitive_incomplete([[undec,_TagNo]|_RestTag],Bin) -> %incomlete decode
    decode_incomplete_bin(Bin); 
decode_primitive_incomplete([[parts,TagNo]|_RestTag],Bin) ->
    case decode_tag_and_length(Bin) of
	{_Form,TagNo,V,Rest} ->
	    {{TagNo,decode_parts_incomplete(V)},Rest};
	Err ->
	    {error,{asn1,"tag failure",TagNo,Err}}
    end;
decode_primitive_incomplete([mandatory|RestTag],Bin) ->
    {Form,TagNo,V,Rest} = decode_tag_and_length(Bin),
    decode_incomplete2(Form,TagNo,V,RestTag,Rest);
%% A choice that is a toptype or a mandatory component of a
%% SEQUENCE or SET.
decode_primitive_incomplete([[mandatory|Directives]],Bin) ->
    {Form,TagNo,V,Rest} = decode_tag_and_length(Bin),
    decode_incomplete2(Form,TagNo,V,Directives,Rest);
decode_primitive_incomplete([],Bin) ->
    decode_primitive(Bin).

%% decode_parts_incomplete/1 receives a number of values encoded in
%% sequence and returns the parts as unencoded binaries
decode_parts_incomplete(<<>>) ->
    [];
decode_parts_incomplete(Bin) ->
    {ok,Rest} = skip_tag(Bin),
    {ok,Rest2} = skip_length_and_value(Rest),
    LenPart = size(Bin) - size(Rest2),
    <<Part:LenPart/binary,RestBin/binary>> = Bin,
    [Part|decode_parts_incomplete(RestBin)].
	    
    
%% decode_incomplete2 checks if V is a value of a constructed or
%% primitive type, and continues the decode propeerly.
decode_incomplete2(_Form=2,TagNo,V,TagMatch,_) ->
    %% constructed indefinite length
    {Vlist,Rest2} = decode_constr_indef_incomplete(TagMatch,V,[]),
    {{TagNo,Vlist},Rest2};
decode_incomplete2(1,TagNo,V,[TagMatch],Rest) when is_list(TagMatch) ->
    {{TagNo,decode_constructed_incomplete(TagMatch,V)},Rest};
decode_incomplete2(1,TagNo,V,TagMatch,Rest) ->
    {{TagNo,decode_constructed_incomplete(TagMatch,V)},Rest};
decode_incomplete2(0,TagNo,V,_TagMatch,Rest) ->
    {{TagNo,V},Rest}.

decode_constructed_incomplete([Tags=[Ts]],Bin) when is_list(Ts) ->
    decode_constructed_incomplete(Tags,Bin);
decode_constructed_incomplete(_TagMatch,<<>>) ->
    [];
decode_constructed_incomplete([mandatory|RestTag],Bin) ->
    {Tlv,Rest} = decode_primitive(Bin),
    [Tlv|decode_constructed_incomplete(RestTag,Rest)];
decode_constructed_incomplete(Directives=[[Alt,_]|_],Bin) 
  when Alt == alt_undec; Alt == alt; Alt == alt_parts ->
    {_Form,TagNo,V,Rest} = decode_tag_and_length(Bin),
    case incomplete_choice_alt(TagNo,Directives) of
	{alt_undec,_} ->
	    LenA = size(Bin)-size(Rest),
	    <<A:LenA/binary,Rest/binary>> = Bin,
	    A;
	{alt,InnerDirectives} ->
	    {Tlv,Rest} = decode_primitive_incomplete(InnerDirectives,V),
	    {TagNo,Tlv};
	{alt_parts,_} ->
	    [{TagNo,decode_parts_incomplete(V)}];
	no_match -> %% if a choice alternative was encoded that
	    %% was not specified in the config file,
	    %% thus decode component anonomous.
	    {Tlv,_}=decode_primitive(Bin),
	    Tlv
    end;
decode_constructed_incomplete([TagNo|RestTag],Bin) ->
%%    {Tlv,Rest} = decode_primitive_incomplete([TagNo],Bin),
    case decode_primitive_incomplete([TagNo],Bin) of
	{Tlv,Rest} ->
	    [Tlv|decode_constructed_incomplete(RestTag,Rest)];
	asn1_NOVALUE ->
	    decode_constructed_incomplete(RestTag,Bin)
    end;
decode_constructed_incomplete([],Bin) ->
    {Tlv,Rest}=decode_primitive(Bin),
    [Tlv|decode_constructed_incomplete([],Rest)].

decode_constr_indef_incomplete(_TagMatch,<<0,0,Rest/binary>>,Acc) ->
    {lists:reverse(Acc),Rest};
decode_constr_indef_incomplete([Tag|RestTags],Bin,Acc) ->
%    {Tlv,Rest} = decode_primitive_incomplete([Tag],Bin),
    case decode_primitive_incomplete([Tag],Bin) of
	{Tlv,Rest} ->
	    decode_constr_indef_incomplete(RestTags,Rest,[Tlv|Acc]);
	asn1_NOVALUE ->
	    decode_constr_indef_incomplete(RestTags,Bin,Acc)
    end.


decode_incomplete_bin(Bin) ->
    {ok,Rest} = skip_tag(Bin),
    {ok,Rest2} = skip_length_and_value(Rest),
    IncLen = size(Bin) - size(Rest2),
    <<IncBin:IncLen/binary,Ret/binary>> = Bin,
    {IncBin,Ret}.
	    
incomplete_choice_alt(TagNo,[[Alt,TagNo]|Directives]) ->
    {Alt,Directives};
incomplete_choice_alt(TagNo,[D]) when is_list(D) ->
    incomplete_choice_alt(TagNo,D);
incomplete_choice_alt(TagNo,[_H|Directives]) ->
    incomplete_choice_alt(TagNo,Directives);
incomplete_choice_alt(_,[]) ->
    no_match.




%% decode_selective(Pattern, Binary) the first argument is a pattern that tells
%% what to do with the next element the second is the BER encoded
%% message as a binary
%% Returns {ok,Value} or {error,Reason}
%% Value is a binary that in turn must be decoded to get the decoded
%% value.
decode_selective([],Binary) ->
    {ok,Binary};
decode_selective([skip|RestPattern],Binary)->
    {ok,RestBinary}=skip_tag(Binary),
    {ok,RestBinary2}=skip_length_and_value(RestBinary),
    decode_selective(RestPattern,RestBinary2);
decode_selective([[skip_optional,Tag]|RestPattern],Binary) ->
    case skip_optional_tag(Tag,Binary) of
	{ok,RestBinary} ->
	    {ok,RestBinary2}=skip_length_and_value(RestBinary),
	    decode_selective(RestPattern,RestBinary2);
	missing ->
	    decode_selective(RestPattern,Binary)
    end;
decode_selective([[choosen,Tag]],Binary) ->
    return_value(Tag,Binary);
%     case skip_optional_tag(Tag,Binary) of %may be optional/default
% 	{ok,RestBinary} ->
% 	    {ok,Value} = get_value(RestBinary);
% 	missing ->
% 	    {ok,<<>>}
%     end;
decode_selective([[choosen,Tag]|RestPattern],Binary) ->
    case skip_optional_tag(Tag,Binary) of
	{ok,RestBinary} ->
	    {ok,Value} = get_value(RestBinary),
	    decode_selective(RestPattern,Value);
	missing ->
	    {ok,<<>>}
    end;
decode_selective(P,_) ->
    {error,{asn1,{partial_decode,"bad pattern",P}}}.

return_value(Tag,Binary) ->
    {ok,{Tag,RestBinary}}=get_tag(Binary),
    {ok,{LenVal,_RestBinary2}} = get_length_and_value(RestBinary),
    {ok,<<Tag/binary,LenVal/binary>>}.


%% skip_tag and skip_length_and_value are rutines used both by
%% decode_partial_incomplete and decode_selective (decode/2).

skip_tag(<<_:3,31:5,Rest/binary>>)->
    skip_long_tag(Rest);
skip_tag(<<_:3,_Tag:5,Rest/binary>>) ->
    {ok,Rest}.

skip_long_tag(<<1:1,_:7,Rest/binary>>) ->
    skip_long_tag(Rest);
skip_long_tag(<<0:1,_:7,Rest/binary>>) ->
    {ok,Rest}.

skip_optional_tag(<<>>,Binary) ->
    {ok,Binary};
skip_optional_tag(<<Tag,RestTag/binary>>,<<Tag,Rest/binary>>) ->
    skip_optional_tag(RestTag,Rest);
skip_optional_tag(_,_) ->
    missing.




skip_length_and_value(Binary) ->
    case decode_length(Binary) of
	{indefinite,RestBinary} ->
	    skip_indefinite_value(RestBinary);
	{Length,RestBinary} ->
	    <<_:Length/unit:8,Rest/binary>> = RestBinary,
	    {ok,Rest}
    end.

skip_indefinite_value(<<0,0,Rest/binary>>) ->
    {ok,Rest};
skip_indefinite_value(Binary) ->
    {ok,RestBinary}=skip_tag(Binary),
    {ok,RestBinary2} = skip_length_and_value(RestBinary),
    skip_indefinite_value(RestBinary2).

get_value(Binary) ->
    case decode_length(Binary) of
	{indefinite,RestBinary} ->
	    get_indefinite_value(RestBinary,[]);
	{Length,RestBinary} ->
	    <<Value:Length/binary,_Rest/binary>> = RestBinary,
	    {ok,Value}
    end.

get_indefinite_value(<<0,0,_Rest/binary>>,Acc) ->
    {ok,list_to_binary(lists:reverse(Acc))};
get_indefinite_value(Binary,Acc) ->
    {ok,{Tag,RestBinary}}=get_tag(Binary),
    {ok,{LenVal,RestBinary2}} = get_length_and_value(RestBinary),
    get_indefinite_value(RestBinary2,[LenVal,Tag|Acc]).

get_tag(<<H:1/binary,Rest/binary>>) ->
    case H of
	<<_:3,31:5>> ->
	    get_long_tag(Rest,[H]);
	_ -> {ok,{H,Rest}}
    end.
get_long_tag(<<H:1/binary,Rest/binary>>,Acc) ->
    case H of
	<<0:1,_:7>> ->
	    {ok,{list_to_binary(lists:reverse([H|Acc])),Rest}};
	_ ->
	    get_long_tag(Rest,[H|Acc])
    end.

get_length_and_value(Bin = <<0:1,Length:7,_T/binary>>) ->
    <<Len,Val:Length/binary,Rest/binary>> = Bin,
    {ok,{<<Len,Val/binary>>, Rest}};
get_length_and_value(Bin = <<1:1,0:7,_T/binary>>) ->
    get_indefinite_length_and_value(Bin);
get_length_and_value(<<1:1,LL:7,T/binary>>) ->
    <<Length:LL/unit:8,Rest/binary>> = T,
    <<Value:Length/binary,Rest2/binary>> = Rest,
    {ok,{<<1:1,LL:7,Length:LL/unit:8,Value/binary>>,Rest2}}.

get_indefinite_length_and_value(<<H,T/binary>>) ->
    get_indefinite_length_and_value(T,[H]).

get_indefinite_length_and_value(<<0,0,Rest/binary>>,Acc) ->
    {ok,{list_to_binary(lists:reverse(Acc)),Rest}};
get_indefinite_length_and_value(Binary,Acc) ->
    {ok,{Tag,RestBinary}}=get_tag(Binary),
    {ok,{LenVal,RestBinary2}}=get_length_and_value(RestBinary),
    get_indefinite_length_and_value(RestBinary2,[LenVal,Tag|Acc]).
    


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% match_tags takes a Tlv (Tag, Length, Value) structure and matches
%% it with the tags in TagList. If the tags does not match the function
%% crashes otherwise it returns the remaining Tlv after that the tags have
%% been removed.
%%
%% match_tags(Tlv, TagList)
%%

match_tags({T,V},[T]) ->
    V;
match_tags({T,V}, [T|Tt]) ->
    match_tags(V,Tt);
match_tags([{T,V}],[T|Tt]) ->
    match_tags(V, Tt);
match_tags(Vlist = [{T,_V}|_], [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag,_V},[T|_Tt]) ->
    {error,{asn1,{wrong_tag,{Tag,T}}}}.

 
cindex(Ix,Val,Cname) -> 
    case element(Ix,Val) of 
	{Cname,Val2} -> Val2; 
	X -> X 
    end. 
 
%%%
%% skips components that do not match a tag in Tags
skip_ExtensionAdditions([],_Tags) ->
    [];
skip_ExtensionAdditions(TLV=[{Tag,_}|Rest],Tags) ->
    case [X||X=T<-Tags,T==Tag] of
	[] ->
	    %% skip this TLV and continue with next
	    skip_ExtensionAdditions(Rest,Tags);
	_ ->
	    TLV
    end.
    

%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
%% Optionals, preset not filled optionals with asn1_NOVALUE 
%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
 
% converts a list to a record if necessary 
list_to_record(Name,List) when is_list(List) -> 
    list_to_tuple([Name|List]); 
list_to_record(_Name,Tuple) when is_tuple(Tuple) -> 
    Tuple. 
 
 
fixoptionals(OptList,Val) when is_list(Val) -> 
    fixoptionals(OptList,Val,1,[],[]). 
 
fixoptionals([{Name,Pos}|Ot],[{Name,Val}|Vt],_Opt,Acc1,Acc2) -> 
    fixoptionals(Ot,Vt,Pos+1,[1|Acc1],[{Name,Val}|Acc2]); 
fixoptionals([{_Name,Pos}|Ot],V,Pos,Acc1,Acc2) -> 
    fixoptionals(Ot,V,Pos+1,[0|Acc1],[asn1_NOVALUE|Acc2]); 
fixoptionals(O,[Vh|Vt],Pos,Acc1,Acc2) -> 
    fixoptionals(O,Vt,Pos+1,Acc1,[Vh|Acc2]); 
fixoptionals([],[Vh|Vt],Pos,Acc1,Acc2) -> 
    fixoptionals([],Vt,Pos+1,Acc1,[Vh|Acc2]); 
fixoptionals([],[],_,_Acc1,Acc2) -> 
    % return Val as a record 
    list_to_tuple([asn1_RECORDNAME|lists:reverse(Acc2)]).
 
 
%%encode_tag(TagClass(?UNI, APP etc), Form (?PRIM etx), TagInteger) ->  
%%     8bit Int | binary 
encode_tag_val({Class, Form, TagNo}) when (TagNo =< 30) -> 
    <<(Class bsr 6):2,(Form bsr 5):1,TagNo:5>>;

encode_tag_val({Class, Form, TagNo}) -> 
    {Octets,_Len} = mk_object_val(TagNo),
    BinOct = list_to_binary(Octets),
    <<(Class bsr 6):2, (Form bsr 5):1, 31:5,BinOct/binary>>.
 

%%=============================================================================== 
%% Decode a tag 
%% 
%% decode_tag(OctetListBuffer) -> {{Form, (Class bsl 16)+ TagNo}, RestOfBuffer, RemovedBytes} 
%%=============================================================================== 

decode_tag_and_length(<<Class:2, Form:1, TagNo:5, 0:1, Length:7, V:Length/binary, RestBuffer/binary>>) when TagNo < 31 -> 
    {Form, (Class bsl 16) + TagNo, V, RestBuffer};
decode_tag_and_length(<<Class:2, 1:1, TagNo:5, 1:1, 0:7, T/binary>>) when TagNo < 31 ->
    {2, (Class bsl 16) + TagNo, T, <<>>};
decode_tag_and_length(<<Class:2, Form:1, TagNo:5, 1:1, LL:7, Length:LL/unit:8,V:Length/binary, T/binary>>) when TagNo < 31 ->
    {Form, (Class bsl 16) + TagNo, V, T};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 0:1, TagNo:7, 0:1, Length:7, V:Length/binary, RestBuffer/binary>>) -> 
    {Form, (Class bsl 16) + TagNo, V, RestBuffer};
decode_tag_and_length(<<Class:2, 1:1, 31:5, 0:1, TagNo:7, 1:1, 0:7, T/binary>>)  ->
    {2, (Class bsl 16) + TagNo, T, <<>>};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 0:1, TagNo:7, 1:1, LL:7, Length:LL/unit:8, V:Length/binary, T/binary>>)  ->
    {Form, (Class bsl 16) + TagNo, V, T};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 1:1, TagPart1:7, 0:1, TagPartLast, Buffer/binary>>) ->
    TagNo = (TagPart1 bsl 7) bor TagPartLast,
    {Length, RestBuffer} = decode_length(Buffer),
    << V:Length/binary, RestBuffer2/binary>> = RestBuffer,
    {Form, (Class bsl 16) + TagNo, V, RestBuffer2};
decode_tag_and_length(<<Class:2, Form:1, 31:5, Buffer/binary>>) ->
    {TagNo, Buffer1} = decode_tag(Buffer, 0),
    {Length, RestBuffer} = decode_length(Buffer1),
    << V:Length/binary, RestBuffer2/binary>> = RestBuffer,
    {Form, (Class bsl 16) + TagNo, V, RestBuffer2}.


     
%% last partial tag 
decode_tag(<<0:1,PartialTag:7, Buffer/binary>>, TagAck) ->
    TagNo = (TagAck bsl 7) bor PartialTag,
    %%<<TagNo>> = <<TagAck:1, PartialTag:7>>,
    {TagNo, Buffer}; 
% more tags 
decode_tag(<<_:1,PartialTag:7, Buffer/binary>>, TagAck) -> 
    TagAck1 = (TagAck bsl 7) bor PartialTag, 
    %%<<TagAck1:16>> = <<TagAck:1, PartialTag:7,0:8>>,
    decode_tag(Buffer, TagAck1). 
 

%%=======================================================================
%%
%% Encode all tags in the list Tags and return a possibly deep list of
%% bytes with tag and length encoded
%% The taglist must be in reverse order (fixed by the asn1 compiler)
%% e.g [T1,T2] will result in 
%% {[EncodedT2,EncodedT1|BytesSoFar],LenSoFar+LenT2+LenT1}
%% 

encode_tags([Tag|Trest], BytesSoFar, LenSoFar) ->
% remove    {Bytes1,L1} = encode_one_tag(Tag),
    {Bytes2,L2} = encode_length(LenSoFar),
    encode_tags(Trest, [Tag,Bytes2|BytesSoFar], 
		 LenSoFar + size(Tag) + L2);
encode_tags([], BytesSoFar, LenSoFar) ->
    {BytesSoFar,LenSoFar}.

encode_tags(TagIn, {BytesSoFar,LenSoFar}) ->
    encode_tags(TagIn, BytesSoFar, LenSoFar).

% encode_one_tag(#tag{class=Class,number=No,type=Type, form = Form}) ->			  
%     NewForm = case Type of
% 	       'EXPLICIT' ->
% 		   ?CONSTRUCTED;
% 	       _ ->
% 		   Form
% 	   end,
%     Bytes = encode_tag_val({Class,NewForm,No}),
%     {Bytes,size(Bytes)}.

 
%%=============================================================================== 
%% 
%% This comment is valid for all the encode/decode functions 
%% 
%% C = Constraint -> typically {'ValueRange',LowerBound,UpperBound} 
%%     used for PER-coding but not for BER-coding. 
%% 
%% Val = Value.  If Val is an atom then it is a symbolic integer value  
%%       (i.e the atom must be one of the names in the NamedNumberList). 
%%       The NamedNumberList is used to translate the atom to an integer value 
%%       before encoding. 
%% 
%%=============================================================================== 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Value) -> io_list (i.e nested list with integers, binaries)
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary

%%
encode_open_type(Val) when is_list(Val) -> 
%    {Val,length(Val)};
    encode_open_type(list_to_binary(Val));
encode_open_type(Val) ->
    {Val, size(Val)}. 

%% 
encode_open_type(Val, T) when is_list(Val) -> 
    encode_open_type(list_to_binary(Val),T);
encode_open_type(Val,[]) ->
    {Val, size(Val)};
encode_open_type(Val,Tag) ->
    encode_tags(Tag,Val, size(Val)). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Tlv, TagIn) -> Value
%% Tlv = {Tag,V} | V where V -> binary()
%% TagIn = [TagVal] where TagVal -> int()
%% Value = binary with decoded data (which must be decoded again as some type)
%%
decode_open_type(Tlv, TagIn) ->
    case match_tags(Tlv,TagIn) of
	Bin when is_binary(Bin) ->
	    {InnerTlv,_} = decode(Bin),
	    InnerTlv;
	TlvBytes -> TlvBytes
    end.

 
decode_open_type_as_binary(Tlv,TagIn)->
    case match_tags(Tlv,TagIn) of
	V when is_binary(V) ->
	    V;
	[Tlv2] -> encode(Tlv2);
	Tlv2 -> encode(Tlv2)
    end.
 
%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
%% Boolean, ITU_T X.690 Chapter 8.2 
%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
 
%%=============================================================================== 
%% encode_boolean(Integer, ReversedTagList) -> {[Octet],Len} 
%%=============================================================================== 
 
encode_boolean({Name, Val}, TagIn) when is_atom(Name) -> 
    encode_boolean(Val, TagIn); 
encode_boolean(true, TagIn) -> 
    encode_tags(TagIn, [16#FF],1);
encode_boolean(false, TagIn) -> 
    encode_tags(TagIn, [0],1);
encode_boolean(X,_) ->
    exit({error,{asn1, {encode_boolean, X}}}). 
 
 
%%=============================================================================== 
%% decode_boolean(BuffList, HasTag, TotalLen) -> {true, Remain, RemovedBytes} |  
%%                                               {false, Remain, RemovedBytes} 
%%=============================================================================== 
decode_boolean(Tlv,TagIn) ->
    Val = match_tags(Tlv, TagIn),
    case Val of
	<<0:8>> ->
	    false;
	<<_:8>> ->
	    true;
	_ ->
	    exit({error,{asn1, {decode_boolean, Val}}})
    end.

 
%%=========================================================================== 
%% Integer, ITU_T X.690 Chapter 8.3 

%% encode_integer(Constraint, Value, Tag) -> [octet list] 
%% encode_integer(Constraint, Name, NamedNumberList, Tag) -> [octet list] 
%%    Value = INTEGER | {Name,INTEGER}  
%%    Tag = tag | notag 
%%=========================================================================== 
 
encode_integer(C, Val, Tag) when is_integer(Val) -> 
    encode_tags(Tag, encode_integer(C, Val));
encode_integer(C,{Name,Val},Tag) when is_atom(Name) ->
    encode_integer(C,Val,Tag);
encode_integer(_C, Val, _Tag) -> 
    exit({error,{asn1, {encode_integer, Val}}}). 
 
 
 
encode_integer(C, Val, NamedNumberList, Tag) when is_atom(Val) -> 
    case lists:keysearch(Val, 1, NamedNumberList) of 
	{value,{_, NewVal}} ->  
	    encode_tags(Tag, encode_integer(C, NewVal)); 
	_ ->  
	    exit({error,{asn1, {encode_integer_namednumber, Val}}}) 
    end; 
encode_integer(C,{_Name,Val},NamedNumberList,Tag) ->
    encode_integer(C,Val,NamedNumberList,Tag); 
encode_integer(C, Val, _NamedNumberList, Tag) -> 
    encode_tags(Tag, encode_integer(C, Val)). 
 
 
encode_integer(_, Val) -> 
    Bytes =  
	if  
	    Val >= 0 -> 
		encode_integer_pos(Val, []); 
	    true -> 
		encode_integer_neg(Val, []) 
	end, 
    {Bytes,length(Bytes)}. 
 
encode_integer_pos(0, L=[B|_Acc]) when B < 128 ->
    L;
encode_integer_pos(N, Acc) ->
    encode_integer_pos((N bsr 8), [N band 16#ff| Acc]).

encode_integer_neg(-1, L=[B1|_T]) when B1 > 127 ->
    L;
encode_integer_neg(N, Acc) ->
    encode_integer_neg(N bsr 8, [N band 16#ff|Acc]).

%%=============================================================================== 
%% decode integer  
%%    (Buffer, Range, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}  
%%    (Buffer, Range, NamedNumberList, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}  
%%=============================================================================== 

decode_integer(Tlv,Range,NamedNumberList,TagIn) ->
    V = match_tags(Tlv,TagIn),
    Int = decode_integer(V),
    range_check_integer(Int,Range),
    number2name(Int,NamedNumberList).

decode_integer(Tlv,Range,TagIn) ->
    V = match_tags(Tlv, TagIn),
    Int = decode_integer(V),
    range_check_integer(Int,Range),
    Int.

%% decoding postitive integer values. 
decode_integer(Bin = <<0:1,_:7,_/binary>>) ->
    Len = size(Bin),
%    <<Int:Len/unit:8,Buffer2/binary>> = Bin,
    <<Int:Len/unit:8>> = Bin,
    Int;
%% decoding negative integer values.
decode_integer(Bin = <<1:1,B2:7,Bs/binary>>)  ->
    Len = size(Bin),
%    <<N:Len/unit:8,Buffer2/binary>> = <<B2,Bs/binary>>,
    <<N:Len/unit:8>> = <<B2,Bs/binary>>,
    Int = N - (1 bsl (8 * Len - 1)),
    Int.

range_check_integer(Int,Range) ->
    case Range of
	[] -> % No length constraint
	    Int;
	{Lb,Ub} when Int >= Lb, Ub >= Int -> % variable length constraint
	    Int;
	Int -> % fixed value constraint
	    Int;
	{_,_} -> 
	    exit({error,{asn1,{integer_range,Range,Int}}});
	SingleValue when is_integer(SingleValue) ->
	    exit({error,{asn1,{integer_range,Range,Int}}});
	_ -> % some strange constraint that we don't support yet
	    Int
    end.

number2name(Int,[]) ->
    Int;
number2name(Int,NamedNumberList) ->
    case lists:keysearch(Int, 2, NamedNumberList) of 
	{value,{NamedVal, _}} -> 
	    NamedVal; 
	_ -> 
	    Int 
    end. 

 
%%============================================================================ 
%% Enumerated value, ITU_T X.690 Chapter 8.4 

%% encode enumerated value 
%%============================================================================ 
encode_enumerated(Val, TagIn) when is_integer(Val)->
    encode_tags(TagIn, encode_integer(false,Val));
encode_enumerated({Name,Val}, TagIn) when is_atom(Name) ->
    encode_enumerated(Val, TagIn).

%% The encode_enumerated functions below this line can be removed when the
%% new code generation is stable. (the functions might have to be kept here
%% a while longer for compatibility reasons)

encode_enumerated(C, Val, {NamedNumberList,ExtList}, TagIn) when is_atom(Val) ->
    case catch encode_enumerated(C, Val, NamedNumberList, TagIn) of
	{'EXIT',_} -> encode_enumerated(C, Val, ExtList, TagIn);
	Result -> Result
    end;
 
encode_enumerated(C, Val, NamedNumberList, TagIn) when is_atom(Val) -> 
    case lists:keysearch(Val, 1, NamedNumberList) of 
	{value, {_, NewVal}} -> 
	    encode_tags(TagIn, encode_integer(C, NewVal)); 
	_ -> 
	    exit({error,{asn1, {enumerated_not_in_range, Val}}}) 
    end; 

encode_enumerated(C, {asn1_enum, Val}, {_,_}, TagIn) when is_integer(Val) ->
    encode_tags(TagIn, encode_integer(C,Val));

encode_enumerated(C, {Name,Val}, NamedNumberList, TagIn) when is_atom(Name) -> 
    encode_enumerated(C, Val, NamedNumberList, TagIn);

encode_enumerated(_C, Val, _NamedNumberList, _TagIn) -> 
    exit({error,{asn1, {enumerated_not_namednumber, Val}}}). 
 
 
 
%%============================================================================ 
%% decode enumerated value 
%%   (Buffer, Range, NamedNumberList, HasTag, TotalLen) ->  Value 
%%===========================================================================
decode_enumerated(Tlv, Range, NamedNumberList, Tags) ->
    Buffer = match_tags(Tlv,Tags),
    decode_enumerated_notag(Buffer, Range, NamedNumberList, Tags).

decode_enumerated_notag(Buffer, _Range, {NamedNumberList,ExtList}, _Tags) ->

    IVal = decode_integer2(size(Buffer), Buffer),
    case decode_enumerated1(IVal, NamedNumberList) of
	{asn1_enum,IVal} ->
	    decode_enumerated1(IVal,ExtList);
	EVal ->
	    EVal
    end;
decode_enumerated_notag(Buffer, _Range, NNList, _Tags) ->
    IVal = decode_integer2(size(Buffer), Buffer), 
    case decode_enumerated1(IVal, NNList) of
	{asn1_enum,_} ->
	    exit({error,{asn1, {illegal_enumerated, IVal}}});
	EVal ->
	    EVal
    end.

decode_enumerated1(Val, NamedNumberList) ->     
    %% it must be a named integer 
    case lists:keysearch(Val, 2, NamedNumberList) of 
	{value,{NamedVal, _}} -> 
	    NamedVal; 
	_ -> 
	    {asn1_enum,Val}
    end.
 

%%============================================================================ 
%%
%% Real value, ITU_T X.690 Chapter 8.5 
%%============================================================================ 
%%
%% encode real value 
%%============================================================================ 
 
%% only base 2 internally so far!! 
encode_real(_C,0, TagIn) ->
    encode_tags(TagIn, {[],0}); 
encode_real(_C,'PLUS-INFINITY', TagIn) ->
    encode_tags(TagIn, {[64],1}); 
encode_real(_C,'MINUS-INFINITY', TagIn) -> 
    encode_tags(TagIn, {[65],1}); 
encode_real(C,Val, TagIn) when is_tuple(Val); is_list(Val) -> 
    encode_tags(TagIn, encode_real(C,Val)). 
 


encode_real(C,Val) ->
    ?RT_COMMON:encode_real(C,Val). 
 
 
%%============================================================================ 
%% decode real value 
%% 
%% decode_real([OctetBufferList], tuple|value, tag|notag) -> 
%%  {{Mantissa, Base, Exp} | realval | PLUS-INFINITY | MINUS-INFINITY | 0, 
%%     RestBuff} 
%%  
%% only for base 2 and 10 decoding sofar!! 
%%============================================================================ 
 
decode_real(Tlv, Tags) ->
    Buffer = match_tags(Tlv,Tags),
    decode_real_notag(Buffer).

decode_real_notag(Buffer) ->
    Len =
	case Buffer of
	    Bin when is_binary(Bin) ->
		size(Bin);
	    {_T,_V} ->
		exit({error,{asn1,{real_not_in_primitive_form,Buffer}}})
	end,
    {Val,_Rest,Len} = ?RT_COMMON:decode_real(Buffer,Len),
    Val.
%%    exit({error,{asn1, {unimplemented,real}}}).
%%  decode_real2(Buffer, Form, size(Buffer)).

% decode_real2(Buffer, Form, Len) ->
%     <<First, Buffer2/binary>> = Buffer,
%     if
% 	First =:= 2#01000000 -> {'PLUS-INFINITY', Buffer2}; 
% 	First =:= 2#01000001 -> {'MINUS-INFINITY', Buffer2}; 
% 	First =:= 2#00000000 -> {0, Buffer2}; 
% 	true -> 
% 	    %% have some check here to verify only supported bases (2) 
% 	    <<B7:1,B6:1,B5_4:2,B3_2:2,B1_0:2>> = <<First>>,
% 		Sign = B6,
% 	    Base = 
% 		case B5_4 of
% 		    0 -> 2;  % base 2, only one so far 
% 		    _ -> exit({error,{asn1, {non_supported_base, First}}}) 
% 		end, 
% 	    ScalingFactor = 
% 		case B3_2 of
% 		    0 -> 0;  % no scaling so far  
% 		    _ -> exit({error,{asn1, {non_supported_scaling, First}}}) 
% 		end, 
						
% 	    {FirstLen,Exp,Buffer3} =
% 		case B1_0 of
% 		    0 ->
% 			<<_:1/unit:8,Buffer21/binary>> = Buffer2,
% 			{2, decode_integer2(1, Buffer2),Buffer21}; 
% 		    1 ->
% 			<<_:2/unit:8,Buffer21/binary>> = Buffer2,
% 			{3, decode_integer2(2, Buffer2)}; 
% 		    2 ->
% 			<<_:3/unit:8,Buffer21/binary>> = Buffer2,
% 			{4, decode_integer2(3, Buffer2)}; 
% 		    3 -> 
% 			<<ExpLen1,RestBuffer/binary>> = Buffer2,
% 			<<_:ExpLen1/unit:8,RestBuffer2/binary>> = RestBuffer,
% 			{ ExpLen1 + 2, 
% 			  decode_integer2(ExpLen1, RestBuffer, RemBytes1),
% 			  RestBuffer2} 
% 		end, 
% 	    Length = Len - FirstLen,
% 	    <<LongInt:Length/unit:8,RestBuff/binary>> = Buffer3,
% 	    {Mantissa, Buffer4} = 
% 		if Sign =:= 0 -> 
			
% 			{LongInt, RestBuff};%  sign plus, 
% 		   true -> 
			
% 			{-LongInt, RestBuff}%  sign minus 
% 		end, 
% 	    case Form of 
% 		tuple -> 
% 		    {Val,Buf,RemB} = Exp, 
% 		    {{Mantissa, Base, {Val,Buf}}, Buffer4, RemBytes2+RemBytes3};  
% 		_value -> 
% 		    comming 
% 	    end 
%     end. 
 
 
%%============================================================================ 
%% Bitstring value, ITU_T X.690 Chapter 8.6 
%%
%% encode bitstring value 
%% 
%% bitstring NamedBitList 
%% Val can be  of: 
%% - [identifiers] where only named identifers are set to one,  
%%   the Constraint must then have some information of the  
%%   bitlength. 
%% - [list of ones and zeroes] all bits  
%% - integer value representing the bitlist 
%% C is constrint Len, only valid when identifiers 
%%============================================================================ 
 
encode_bit_string(C,Bin={Unused,BinBits},NamedBitList,TagIn) when is_integer(Unused), is_binary(BinBits) ->
    encode_bin_bit_string(C,Bin,NamedBitList,TagIn);
encode_bit_string(C, [FirstVal | RestVal], NamedBitList, TagIn) when is_atom(FirstVal) -> 
    encode_bit_string_named(C, [FirstVal | RestVal], NamedBitList, TagIn); 

encode_bit_string(C, [{bit,X} | RestVal], NamedBitList, TagIn) -> 
    encode_bit_string_named(C, [{bit,X} | RestVal], NamedBitList, TagIn); 
 
encode_bit_string(C, [FirstVal| RestVal], NamedBitList, TagIn) when is_integer(FirstVal) -> 
    encode_bit_string_bits(C, [FirstVal | RestVal], NamedBitList, TagIn); 
 
encode_bit_string(_C, 0, _NamedBitList, TagIn) -> 
    encode_tags(TagIn, <<0>>,1); 

encode_bit_string(_C, [], _NamedBitList, TagIn) -> 
    encode_tags(TagIn, <<0>>,1); 

encode_bit_string(C, IntegerVal, NamedBitList, TagIn) when is_integer(IntegerVal) -> 
    BitListVal = int_to_bitlist(IntegerVal),
    encode_bit_string_bits(C, BitListVal, NamedBitList, TagIn);

encode_bit_string(C, {Name,BitList}, NamedBitList, TagIn) when is_atom(Name) ->
    encode_bit_string(C, BitList, NamedBitList, TagIn).
  
 
 
int_to_bitlist(0) ->
    [];
int_to_bitlist(Int) when is_integer(Int), Int >= 0 ->
    [Int band 1 | int_to_bitlist(Int bsr 1)].


%%================================================================= 
%% Encode BIT STRING of the form {Unused,BinBits}. 
%% Unused is the number of unused bits in the last byte in BinBits 
%% and BinBits is a binary representing the BIT STRING.
%%================================================================= 
encode_bin_bit_string(C,{Unused,BinBits},_NamedBitList,TagIn)->
    case get_constraint(C,'SizeConstraint') of
	no ->
	    remove_unused_then_dotag(TagIn, Unused, BinBits);
	{_Min,Max} ->
	    BBLen = (size(BinBits)*8)-Unused,
	    if
		BBLen > Max ->
		    exit({error,{asn1, 
				 {bitstring_length, 
				  {{was,BBLen},{maximum,Max}}}}}); 
		true ->
		    remove_unused_then_dotag(TagIn, Unused, BinBits)
	    end;
	Size ->
	    case ((size(BinBits)*8)-Unused) of
		BBSize when BBSize =< Size ->
		    remove_unused_then_dotag(TagIn, Unused, BinBits);
		BBSize  ->
		    exit({error,{asn1,  
				 {bitstring_length, 
				  {{was,BBSize},{should_be,Size}}}}})
	    end
    end.

remove_unused_then_dotag(TagIn,Unused,BinBits) ->
    case Unused of
	0 when (size(BinBits) == 0) ->
	    encode_tags(TagIn,<<0>>,1);
	0 ->
	    Bin = <<Unused,BinBits/binary>>,
	    encode_tags(TagIn,Bin,size(Bin));
	Num ->
	    N = (size(BinBits)-1),
	    <<BBits:N/binary,LastByte>> = BinBits,
	    encode_tags(TagIn,
			[Unused,binary_to_list(BBits) ++[(LastByte bsr Num) bsl Num]],
			1+size(BinBits))
    end.


%%================================================================= 
%% Encode named bits 
%%================================================================= 
 
encode_bit_string_named(C, [FirstVal | RestVal], NamedBitList, TagIn) -> 
    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []), 
    Size = 
	case get_constraint(C,'SizeConstraint') of 
	    no -> 
		lists:max(ToSetPos)+1;
	    {_Min,Max} ->
		Max;
	    TSize ->
		TSize
	end,
    BitList = make_and_set_list(Size, ToSetPos, 0), 
    {Len, Unused, OctetList} = encode_bitstring(BitList),  
    encode_tags(TagIn, [Unused|OctetList],Len+1). 

	 
%%---------------------------------------- 
%% get_all_bitposes([list of named bits to set], named_bit_db, []) -> 
%%   [sorted_list_of_bitpositions_to_set] 
%%---------------------------------------- 

get_all_bitposes([{bit,ValPos}|Rest], NamedBitList, Ack) ->
    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack ]);
get_all_bitposes([Val | Rest], NamedBitList, Ack) when is_atom(Val) -> 
    case lists:keysearch(Val, 1, NamedBitList) of 
	{value, {_ValName, ValPos}} -> 
	    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]); 
	_ -> 
	    exit({error,{asn1, {bitstring_namedbit, Val}}}) 
    end; 
get_all_bitposes([], _NamedBitList, Ack) -> 
    lists:sort(Ack). 
 
 
%%---------------------------------------- 
%% make_and_set_list(Len of list to return, [list of positions to set to 1])-> 
%% returns list of Len length, with all in SetPos set. 
%% in positioning in list the first element is 0, the second 1 etc.., but 
%% Len will make a list of length Len, not Len + 1. 
%%    BitList = make_and_set_list(C, ToSetPos, 0), 
%%---------------------------------------- 
 
make_and_set_list(0, [], _) -> []; 
make_and_set_list(0, _, _) ->  
    exit({error,{asn1,bitstring_sizeconstraint}}); 
make_and_set_list(Len, [XPos|SetPos], XPos) -> 
    [1 | make_and_set_list(Len - 1, SetPos, XPos + 1)]; 
make_and_set_list(Len, [Pos|SetPos], XPos) -> 
    [0 | make_and_set_list(Len - 1, [Pos | SetPos], XPos + 1)]; 
make_and_set_list(Len, [], XPos) -> 
    [0 | make_and_set_list(Len - 1, [], XPos + 1)]. 
 
 
 
 
 
 
%%================================================================= 
%% Encode bit string for lists of ones and zeroes 
%%================================================================= 
encode_bit_string_bits(C, BitListVal, _NamedBitList, TagIn) when is_list(BitListVal) -> 
    case get_constraint(C,'SizeConstraint') of 
	no -> 
	    {Len, Unused, OctetList} = encode_bitstring(BitListVal),  
	    %%add unused byte to the Len 
	    encode_tags(TagIn, [Unused | OctetList], Len+1);
	Constr={Min,_Max} when is_integer(Min) ->
	    %% Max may be an integer or 'MAX'
	    encode_constr_bit_str_bits(Constr,BitListVal,TagIn);
	{Constr={_,_},[]} ->%Constr={Min,Max}
	    %% constraint with extension mark
	    encode_constr_bit_str_bits(Constr,BitListVal,TagIn);
	Constr={{_,_},{_,_}} ->%{{Min1,Max1},{Min2,Max2}}
	    %% constraint with extension mark
	    encode_constr_bit_str_bits(Constr,BitListVal,TagIn);
	Size ->
	    case length(BitListVal) of
		BitSize when BitSize == Size ->
		    {Len, Unused, OctetList} = encode_bitstring(BitListVal),  
		    %%add unused byte to the Len 
		    encode_tags(TagIn, [Unused | OctetList], Len+1);  
		BitSize when BitSize < Size ->
		    PaddedList = pad_bit_list(Size-BitSize,BitListVal),
		    {Len, Unused, OctetList} = encode_bitstring(PaddedList),  
		    %%add unused byte to the Len 
		    encode_tags(TagIn, [Unused | OctetList], Len+1);
		BitSize -> 
		    exit({error,{asn1,  
			  {bitstring_length, {{was,BitSize},{should_be,Size}}}}}) 
	    end 
 
    end. 
 
encode_constr_bit_str_bits({{_Min1,Max1},{Min2,Max2}},BitListVal,TagIn) ->
    BitLen = length(BitListVal),
    case BitLen of
	Len when Len > Max2 -> 
	    exit({error,{asn1,{bitstring_length,{{was,BitLen},
						 {maximum,Max2}}}}}); 
	Len when Len > Max1, Len < Min2  ->
	    exit({error,{asn1,{bitstring_length,{{was,BitLen},
						 {not_allowed_interval,
						  Max1,Min2}}}}});
	_ ->
	    {Len, Unused, OctetList} = encode_bitstring(BitListVal),  
	    %%add unused byte to the Len 
	    encode_tags(TagIn, [Unused, OctetList], Len+1)  
    end;
encode_constr_bit_str_bits({Min,Max},BitListVal,TagIn) ->
    BitLen = length(BitListVal), 
    if  
	BitLen > Max -> 
	    exit({error,{asn1,{bitstring_length,{{was,BitLen},
						 {maximum,Max}}}}}); 
	BitLen < Min ->
	    exit({error,{asn1,{bitstring_length,{{was,BitLen},
						 {minimum,Max}}}}});
	true -> 
	    {Len, Unused, OctetList} = encode_bitstring(BitListVal),  
	    %%add unused byte to the Len 
	    encode_tags(TagIn, [Unused, OctetList], Len+1)  
    end.


%% returns a list of length Size + length(BitListVal), with BitListVal 
%% as the most significant elements followed by padded zero elements
pad_bit_list(Size,BitListVal) ->
    Tail = lists:duplicate(Size,0),
    lists:append(BitListVal,Tail).
 
%%================================================================= 
%% Do the actual encoding 
%%     ([bitlist]) -> {ListLen, UnusedBits, OctetList} 
%%================================================================= 
 
encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1 | Rest]) -> 
    Val = (B8 bsl 7) bor (B7 bsl 6) bor (B6 bsl 5) bor (B5 bsl 4) bor 
	(B4 bsl 3) bor (B3 bsl 2) bor (B2 bsl 1) bor B1, 
    encode_bitstring(Rest, [Val], 1); 
encode_bitstring(Val) -> 
    {Unused, Octet} = unused_bitlist(Val, 7, 0), 
    {1, Unused, [Octet]}. 
 
encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1 | Rest], Ack, Len) -> 
    Val = (B8 bsl 7) bor (B7 bsl 6) bor (B6 bsl 5) bor (B5 bsl 4) bor 
	(B4 bsl 3) bor (B3 bsl 2) bor (B2 bsl 1) bor B1, 
    encode_bitstring(Rest, [Ack | [Val]], Len + 1); 
%%even multiple of 8 bits.. 
encode_bitstring([], Ack, Len) -> 
    {Len, 0, Ack}; 
%% unused bits in last octet 
encode_bitstring(Rest, Ack, Len) -> 
%    io:format("uneven ~w ~w ~w~n",[Rest, Ack, Len]), 
    {Unused, Val} = unused_bitlist(Rest, 7, 0), 
    {Len + 1, Unused, [Ack | [Val]]}. 
 
%%%%%%%%%%%%%%%%%% 
%% unused_bitlist([list of ones and zeros <= 7], 7, []) -> 
%%  {Unused bits, Last octet with bits moved to right} 
unused_bitlist([], Trail, Ack) -> 
    {Trail + 1, Ack}; 
unused_bitlist([Bit | Rest], Trail, Ack) -> 
%%    io:format("trail Bit: ~w Rest: ~w Trail: ~w Ack:~w~n",[Bit, Rest, Trail, Ack]), 
    unused_bitlist(Rest, Trail - 1, (Bit bsl Trail) bor Ack). 
 
 
%%============================================================================ 
%% decode bitstring value 
%%    (Buffer, Range, NamedNumberList, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}  
%%============================================================================ 

decode_compact_bit_string(Buffer, Range, NamedNumberList, Tags) ->
%    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_BIT_STRING}),
     decode_restricted_string(Buffer, Range, ?N_BIT_STRING, Tags, 
			     NamedNumberList,bin).

decode_bit_string(Buffer, Range, NamedNumberList, Tags) -> 
%    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_BIT_STRING}),
    decode_restricted_string(Buffer, Range, ?N_BIT_STRING, Tags, 
			     NamedNumberList,old). 
 

decode_bit_string2(<<0>>,_NamedNumberList,BinOrOld) -> 
    case BinOrOld of
	bin ->
	    {0,<<>>};
	_ ->
	    []
    end;
decode_bit_string2(<<Unused,Bits/binary>>,NamedNumberList,BinOrOld) -> 
    case NamedNumberList of 
	[] -> 
	    case BinOrOld of
		bin ->
		    {Unused,Bits};
		_ ->
		    decode_bitstring2(size(Bits), Unused, Bits)
	    end;
	_ -> 
	    BitString = decode_bitstring2(size(Bits), Unused, Bits),
	    decode_bitstring_NNL(BitString,NamedNumberList) 
    end. 
 
%%---------------------------------------- 
%% Decode the in buffer to bits 
%%---------------------------------------- 
decode_bitstring2(1,Unused,<<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,_/binary>>) -> 
    lists:sublist([B7,B6,B5,B4,B3,B2,B1,B0],8-Unused); 
decode_bitstring2(Len, Unused,
		  <<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,Buffer/binary>>) -> 
    [B7, B6, B5, B4, B3, B2, B1, B0 | 
     decode_bitstring2(Len - 1, Unused, Buffer)]. 
 
%%decode_bitstring2(1, Unused, Buffer) -> 
%%    make_bits_of_int(hd(Buffer), 128, 8-Unused); 
%%decode_bitstring2(Len, Unused, [BitVal | Buffer]) -> 
%%    [B7, B6, B5, B4, B3, B2, B1, B0] = make_bits_of_int(BitVal, 128, 8), 
%%    [B7, B6, B5, B4, B3, B2, B1, B0 | 
%%     decode_bitstring2(Len - 1, Unused, Buffer)]. 
 
 
%%make_bits_of_int(_, _, 0) -> 
%%    []; 
%%make_bits_of_int(BitVal, MaskVal, Unused) when Unused > 0 -> 
%%    X = case MaskVal band BitVal of 
%%	    0 -> 0 ; 
%%	    _ -> 1 
%%	end, 
%%    [X | make_bits_of_int(BitVal, MaskVal bsr 1, Unused - 1)]. 
 
 
 
%%---------------------------------------- 
%% Decode the bitlist to names 
%%---------------------------------------- 

 
decode_bitstring_NNL(BitList,NamedNumberList) -> 
    decode_bitstring_NNL(BitList,NamedNumberList,0,[]). 
 
 
decode_bitstring_NNL([],_,_No,Result) -> 
    lists:reverse(Result); 

decode_bitstring_NNL([B|BitList],[{Name,No}|NamedNumberList],No,Result) -> 
    if 
	B == 0 -> 
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,Result); 
	true -> 
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,[Name|Result]) 
    end; 
decode_bitstring_NNL([1|BitList],NamedNumberList,No,Result) -> 
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,[{bit,No}|Result]); 
decode_bitstring_NNL([0|BitList],NamedNumberList,No,Result) ->
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,Result). 
 
 
%%============================================================================ 
%% Octet string, ITU_T X.690 Chapter 8.7 
%%
%% encode octet string 
%% The OctetList must be a flat list of integers in the range 0..255
%% the function does not check this because it takes to much time
%%============================================================================ 
encode_octet_string(_C, OctetList, TagIn) when is_binary(OctetList) -> 
    encode_tags(TagIn, OctetList, size(OctetList));
encode_octet_string(_C, OctetList, TagIn) when is_list(OctetList) -> 
    encode_tags(TagIn, OctetList, length(OctetList));
encode_octet_string(C, {Name,OctetList}, TagIn) when is_atom(Name) -> 
    encode_octet_string(C, OctetList, TagIn).
 

%%============================================================================ 
%% decode octet string  
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}  
%% 
%% Octet string is decoded as a restricted string 
%%============================================================================ 
decode_octet_string(Buffer, Range, Tags) -> 
%    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_OCTET_STRING}),
    decode_restricted_string(Buffer, Range, ?N_OCTET_STRING, 
			     Tags, [], old). 

%%============================================================================ 
%% Null value, ITU_T X.690 Chapter 8.8 
%%
%% encode NULL value 
%%============================================================================ 
 
encode_null({Name, _Val}, TagIn) when is_atom(Name) -> 
    encode_tags(TagIn, [], 0);
encode_null(_Val, TagIn) -> 
    encode_tags(TagIn, [], 0). 
 
%%============================================================================ 
%% decode NULL value 
%%    (Buffer, HasTag, TotalLen) -> {NULL, Remain, RemovedBytes}  
%%============================================================================ 

decode_null(Tlv, Tags) ->
    Val = match_tags(Tlv, Tags),
    case Val of
	<<>> ->
	    'NULL';
	_ ->
	    exit({error,{asn1,{decode_null,Val}}})
	end. 
 
%%============================================================================ 
%% Object identifier, ITU_T X.690 Chapter 8.19 
%%
%% encode Object Identifier value 
%%============================================================================ 
 
encode_object_identifier({Name,Val}, TagIn) when is_atom(Name) -> 
    encode_object_identifier(Val, TagIn);
encode_object_identifier(Val, TagIn) -> 
    encode_tags(TagIn, e_object_identifier(Val)).
 
e_object_identifier({'OBJECT IDENTIFIER', V}) -> 
    e_object_identifier(V); 
e_object_identifier({Cname, V}) when is_atom(Cname), is_tuple(V) -> 
    e_object_identifier(tuple_to_list(V)); 
e_object_identifier({Cname, V}) when is_atom(Cname), is_list(V) -> 
    e_object_identifier(V); 
e_object_identifier(V) when is_tuple(V) -> 
    e_object_identifier(tuple_to_list(V)); 
 
%%%%%%%%%%%%%%% 
%% e_object_identifier([List of Obect Identifiers]) -> 
%% {[Encoded Octetlist of ObjIds], IntLength} 
%% 
e_object_identifier([E1, E2 | Tail]) -> 
    Head = 40*E1 + E2,  % wow! 
    {H,Lh} = mk_object_val(Head),
    {R,Lr} = enc_obj_id_tail(Tail, [], 0),
    {[H|R], Lh+Lr}.

enc_obj_id_tail([], Ack, Len) ->
    {lists:reverse(Ack), Len};
enc_obj_id_tail([H|T], Ack, Len) ->
    {B, L} = mk_object_val(H),
    enc_obj_id_tail(T, [B|Ack], Len+L).


%%%%%%%%%%% 
%% mk_object_val(Value) -> {OctetList, Len} 
%% returns a Val as a list of octets, the 8 bit is allways set to one except 
%% for the last octet, where its 0 
%% 

 
mk_object_val(Val) when Val =< 127 -> 
    {[255 band Val], 1}; 
mk_object_val(Val) -> 
    mk_object_val(Val bsr 7, [Val band 127], 1).  
mk_object_val(0, Ack, Len) -> 
    {Ack, Len}; 
mk_object_val(Val, Ack, Len) -> 
    mk_object_val(Val bsr 7, [((Val band 127) bor 128) | Ack], Len + 1). 
 
 
 
%%============================================================================ 
%% decode Object Identifier value   
%%    (Buffer, HasTag, TotalLen) -> {{ObjId}, Remain, RemovedBytes}  
%%============================================================================ 
 
decode_object_identifier(Tlv, Tags) -> 
    Val = match_tags(Tlv, Tags),
    [AddedObjVal|ObjVals] = dec_subidentifiers(Val,0,[]), 
    {Val1, Val2} = if 
		       AddedObjVal < 40 -> 
			   {0, AddedObjVal}; 
		       AddedObjVal < 80 -> 
			   {1, AddedObjVal - 40}; 
		       true -> 
			   {2, AddedObjVal - 80} 
		   end, 
    list_to_tuple([Val1, Val2 | ObjVals]).

dec_subidentifiers(<<>>,_Av,Al) -> 
    lists:reverse(Al); 
dec_subidentifiers(<<1:1,H:7,T/binary>>,Av,Al) -> 
    dec_subidentifiers(T,(Av bsl 7) + H,Al); 
dec_subidentifiers(<<H,T/binary>>,Av,Al) -> 
    dec_subidentifiers(T,0,[((Av bsl 7) + H)|Al]). 

%%============================================================================ 
%% RELATIVE-OID, ITU_T X.690 Chapter 8.20 
%%
%% encode Relative Object Identifier 
%%============================================================================ 
encode_relative_oid({Name,Val},TagIn) when is_atom(Name) ->
    encode_relative_oid(Val,TagIn);
encode_relative_oid(Val,TagIn) when is_tuple(Val) ->
    encode_relative_oid(tuple_to_list(Val),TagIn);
encode_relative_oid(Val,TagIn) ->
    encode_tags(TagIn, enc_relative_oid(Val)).

enc_relative_oid(Tuple) when is_tuple(Tuple) ->
    enc_relative_oid(tuple_to_list(Tuple));
enc_relative_oid(Val) ->
    lists:mapfoldl(fun(X,AccIn) ->
			   {SO,L}=mk_object_val(X),
			   {SO,L+AccIn}
		   end
		   ,0,Val).

%%============================================================================ 
%% decode Relative Object Identifier value   
%%    (Buffer, HasTag, TotalLen) -> {{ObjId}, Remain, RemovedBytes}  
%%============================================================================ 
decode_relative_oid(Tlv, Tags) ->
    Val = match_tags(Tlv, Tags),
    ObjVals = dec_subidentifiers(Val,0,[]), 
    list_to_tuple(ObjVals).

%%============================================================================ 
%% Restricted character string types, ITU_T X.690 Chapter 8.20 
%%
%% encode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings 
%%============================================================================ 
%% The StringType arg is kept for future use but might be removed 
encode_restricted_string(_C, OctetList, _StringType, TagIn) 
  when is_binary(OctetList) -> 
    encode_tags(TagIn, OctetList, size(OctetList)); 
encode_restricted_string(_C, OctetList, _StringType, TagIn) 
  when is_list(OctetList) -> 
    encode_tags(TagIn, OctetList, length(OctetList)); 
encode_restricted_string(C,{Name,OctetL}, StringType, TagIn) when is_atom(Name)-> 
    encode_restricted_string(C, OctetL, StringType, TagIn).

%%============================================================================ 
%% decode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings 
%%    (Buffer, Range, StringType, HasTag, TotalLen) -> 
%%                                  {String, Remain, RemovedBytes}  
%%============================================================================ 

decode_restricted_string(Buffer, Range, StringType, Tags) -> 
	decode_restricted_string(Buffer, Range, StringType, Tags, [], old).


decode_restricted_string(Tlv, Range, StringType, TagsIn, 
			 NamedNumberList, BinOrOld) -> 
    Val = match_tags(Tlv, TagsIn),
    Val2 = 
	case Val of
	    PartList = [_H|_T] -> % constructed val
		Bin = collect_parts(PartList),
		decode_restricted(Bin, StringType, 
				      NamedNumberList, BinOrOld);
	    Bin ->
		decode_restricted(Bin, StringType, 
				      NamedNumberList, BinOrOld)
	end,
    check_and_convert_restricted_string(Val2,StringType,Range,NamedNumberList,BinOrOld).
     


% 	case StringType of
% 	    ?N_BIT_STRING when BinOrOld == bin ->
% 		{concat_bit_binaries(AccVal, Val), AccRb+Rb};
% 	    _ when is_binary(Val),is_binary(AccVal) ->
% 		{<<AccVal/binary,Val/binary>>,AccRb+Rb};
% 	    _ when is_binary(Val), AccVal==[] ->
% 		{Val,AccRb+Rb};				      
% 	    _ ->	
% 		{AccVal++Val, AccRb+Rb}
% 	end,

    
 
decode_restricted(Bin, StringType, NamedNumberList,BinOrOld) -> 
	case StringType of 
	    ?N_BIT_STRING -> 
		decode_bit_string2(Bin, NamedNumberList, BinOrOld); 
	    ?N_UniversalString ->
		mk_universal_string(binary_to_list(Bin)); 
	    ?N_BMPString ->  
		mk_BMP_string(binary_to_list(Bin)); 
	    _ ->  
		Bin
	end. 
     

check_and_convert_restricted_string(Val,StringType,Range,NamedNumberList,_BinOrOld) ->
    {StrLen,NewVal} = case StringType of
			  ?N_BIT_STRING when NamedNumberList /= [] ->
			      {no_check,Val};
			  ?N_BIT_STRING when is_list(Val) ->
			      {length(Val),Val};
			  ?N_BIT_STRING when is_tuple(Val) -> 
			      {(size(element(2,Val))*8) - element(1,Val),Val};
			  _ when is_binary(Val) ->
			      {size(Val),binary_to_list(Val)};
			  _ when is_list(Val) ->
			      {length(Val), Val}
		      end,
    case Range of
	_ when StrLen == no_check ->
	    NewVal;
	[] -> % No length constraint
	    NewVal;
	{Lb,Ub} when StrLen >= Lb, Ub >= StrLen -> % variable length constraint
	    NewVal;
	{{Lb,_Ub},[]} when StrLen >= Lb ->
	    NewVal;
	{{Lb,_Ub},_Ext=[Min|_]} when StrLen >= Lb; StrLen >= Min ->
	    NewVal;
	{{Lb1,Ub1},{Lb2,Ub2}} when StrLen >= Lb1, StrLen =< Ub1; 
				   StrLen =< Ub2, StrLen >= Lb2 ->
	    NewVal;
	StrLen -> % fixed length constraint
	    NewVal;
	{_,_} -> 
	    exit({error,{asn1,{length,Range,Val}}});
	_Len when is_integer(_Len) ->
	    exit({error,{asn1,{length,Range,Val}}});
	_ -> % some strange constraint that we don't support yet
	    NewVal
    end.


%%============================================================================ 
%% encode Universal string 
%%============================================================================ 

encode_universal_string(C, {Name, Universal}, TagIn) when is_atom(Name) -> 
    encode_universal_string(C, Universal, TagIn);
encode_universal_string(_C, Universal, TagIn) -> 
    OctetList = mk_uni_list(Universal), 
    encode_tags(TagIn, OctetList, length(OctetList)). 
 
mk_uni_list(In) ->  
    mk_uni_list(In,[]). 
 
mk_uni_list([],List) ->  
    lists:reverse(List); 
mk_uni_list([{A,B,C,D}|T],List) ->  
    mk_uni_list(T,[D,C,B,A|List]); 
mk_uni_list([H|T],List) ->  
    mk_uni_list(T,[H,0,0,0|List]). 
 
%%=========================================================================== 
%% decode Universal strings  
%%    (Buffer, Range, StringType, HasTag, LenIn) -> 
%%                           {String, Remain, RemovedBytes}  
%%=========================================================================== 

decode_universal_string(Buffer, Range, Tags) ->
    decode_restricted_string(Buffer, Range, ?N_UniversalString, 
			     Tags, [], old).
  
 
mk_universal_string(In) -> 
    mk_universal_string(In,[]). 
 
mk_universal_string([],Acc) -> 
    lists:reverse(Acc); 
mk_universal_string([0,0,0,D|T],Acc) -> 
    mk_universal_string(T,[D|Acc]); 
mk_universal_string([A,B,C,D|T],Acc) -> 
    mk_universal_string(T,[{A,B,C,D}|Acc]). 
 

%%============================================================================
%% encode UTF8 string 
%%============================================================================

encode_UTF8_string(_C,UTF8String,TagIn) when is_binary(UTF8String) ->
    encode_tags(TagIn, UTF8String, size(UTF8String));
encode_UTF8_string(_C,UTF8String,TagIn) ->
    encode_tags(TagIn, UTF8String, length(UTF8String)).


%%============================================================================
%% decode UTF8 string 
%%============================================================================

decode_UTF8_string(Tlv,TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
	PartList = [_H|_T] -> % constructed val
	    collect_parts(PartList);
	Bin ->
	    Bin
    end.
	    
 
%%============================================================================
%% encode BMP string 
%%============================================================================ 

encode_BMP_string(C, {Name,BMPString}, TagIn) when is_atom(Name)-> 
    encode_BMP_string(C, BMPString, TagIn);
encode_BMP_string(_C, BMPString, TagIn) -> 
    OctetList = mk_BMP_list(BMPString), 
    encode_tags(TagIn, OctetList, length(OctetList)). 
 
mk_BMP_list(In) ->  
    mk_BMP_list(In,[]). 
 
mk_BMP_list([],List) ->  
    lists:reverse(List); 
mk_BMP_list([{0,0,C,D}|T],List) ->  
    mk_BMP_list(T,[D,C|List]); 
mk_BMP_list([H|T],List) ->  
    mk_BMP_list(T,[H,0|List]). 
 
%%============================================================================ 
%% decode (OctetList, Range(ignored), tag|notag) -> {ValList, RestList} 
%%    (Buffer, Range, StringType, HasTag, TotalLen) -> 
%%                               {String, Remain, RemovedBytes}  
%%============================================================================ 
decode_BMP_string(Buffer, Range, Tags) -> 
    decode_restricted_string(Buffer, Range, ?N_BMPString, 
			     Tags, [], old). 
 
mk_BMP_string(In) -> 
    mk_BMP_string(In,[]). 
 
mk_BMP_string([],US) -> 
    lists:reverse(US); 
mk_BMP_string([0,B|T],US) -> 
    mk_BMP_string(T,[B|US]); 
mk_BMP_string([C,D|T],US) -> 
    mk_BMP_string(T,[{0,0,C,D}|US]). 
 
 
%%============================================================================ 
%% Generalized time, ITU_T X.680 Chapter 39 
%%
%% encode Generalized time 
%%============================================================================ 

encode_generalized_time(C, {Name,OctetList}, TagIn) when is_atom(Name) -> 
    encode_generalized_time(C, OctetList, TagIn);
encode_generalized_time(_C, OctetList, TagIn) -> 
    encode_tags(TagIn, OctetList, length(OctetList)). 
 
%%============================================================================ 
%% decode Generalized time  
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}  
%%============================================================================ 

decode_generalized_time(Tlv, _Range, Tags) -> 
    Val = match_tags(Tlv, Tags),
    NewVal = case Val of
		 PartList = [_H|_T] -> % constructed
		     collect_parts(PartList);
		 Bin ->
		     Bin
	     end,
    binary_to_list(NewVal).

%%============================================================================ 
%% Universal time, ITU_T X.680 Chapter 40 
%%
%% encode UTC time 
%%============================================================================ 

encode_utc_time(C, {Name,OctetList}, TagIn) when is_atom(Name) -> 
    encode_utc_time(C, OctetList, TagIn);
encode_utc_time(_C, OctetList, TagIn) -> 
    encode_tags(TagIn, OctetList, length(OctetList)).
 
%%============================================================================ 
%% decode UTC time 
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}  
%%============================================================================ 

decode_utc_time(Tlv, _Range, Tags) -> 
    Val = match_tags(Tlv, Tags),
    NewVal = case Val of
		 PartList = [_H|_T] -> % constructed
		     collect_parts(PartList);
		 Bin ->
		     Bin
	     end,
    binary_to_list(NewVal).

 
%%============================================================================ 
%% Length handling  
%%
%% Encode length 
%% 
%% encode_length(Int | indefinite) -> 
%%          [<127]| [128 + Int (<127),OctetList] | [16#80] 
%%============================================================================ 
 
encode_length(indefinite) -> 
    {[16#80],1}; % 128 
encode_length(L) when L =< 16#7F -> 
    {[L],1}; 
encode_length(L) -> 
    Oct = minimum_octets(L), 
    Len = length(Oct), 
    if 
	Len =< 126 -> 
	    {[ (16#80+Len) | Oct ],Len+1}; 
	true -> 
	    exit({error,{asn1, to_long_length_oct, Len}}) 
    end. 
 

%% Val must be >= 0
minimum_octets(Val) -> 
    minimum_octets(Val,[]). 
 
minimum_octets(0,Acc) ->
    Acc;
minimum_octets(Val, Acc) ->
    minimum_octets((Val bsr 8),[Val band 16#FF | Acc]).

 
%%=========================================================================== 
%% Decode length 
%% 
%% decode_length(OctetList) -> {{indefinite, RestOctetsL}, NoRemovedBytes} |  
%%                             {{Length, RestOctetsL}, NoRemovedBytes} 
%%=========================================================================== 

decode_length(<<1:1,0:7,T/binary>>) ->     
    {indefinite, T};
decode_length(<<0:1,Length:7,T/binary>>) ->
    {Length,T};
decode_length(<<1:1,LL:7,T/binary>>) ->
    <<Length:LL/unit:8,Rest/binary>> = T,
    {Length,Rest}.
 


%%-------------------------------------------------------------------------
%% INTERNAL HELPER FUNCTIONS (not exported)
%%-------------------------------------------------------------------------

 
%% decoding postitive integer values. 
decode_integer2(Len,Bin = <<0:1,_:7,_Bs/binary>>) ->
    <<Int:Len/unit:8>> = Bin,
    Int;
%% decoding negative integer values.
decode_integer2(Len,<<1:1,B2:7,Bs/binary>>)  ->
    <<N:Len/unit:8>> = <<B2,Bs/binary>>,
    Int = N - (1 bsl (8 * Len - 1)),
    Int.

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} -> 
	    V
    end.
 
collect_parts(TlvList) ->
    collect_parts(TlvList,[]).

collect_parts([{_,L}|Rest],Acc) when is_list(L) ->
    collect_parts(Rest,[collect_parts(L)|Acc]);
collect_parts([{?N_BIT_STRING,<<Unused,Bits/binary>>}|Rest],_Acc) ->
    collect_parts_bit(Rest,[Bits],Unused);
collect_parts([{_T,V}|Rest],Acc) ->
    collect_parts(Rest,[V|Acc]);
collect_parts([],Acc) ->
    list_to_binary(lists:reverse(Acc)).

collect_parts_bit([{?N_BIT_STRING,<<Unused,Bits/binary>>}|Rest],Acc,Uacc) ->    
    collect_parts_bit(Rest,[Bits|Acc],Unused+Uacc);
collect_parts_bit([],Acc,Uacc) ->
    list_to_binary([Uacc|lists:reverse(Acc)]).




















