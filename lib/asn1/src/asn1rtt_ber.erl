%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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

-module(asn1rtt_ber).

%% encoding / decoding of BER

-export([ber_decode_nif/1,ber_decode_erlang/1,match_tags/2,ber_encode/1]).
-export([encode_tags/3,
	 skip_ExtensionAdditions/2]).
-export([encode_boolean/2,decode_boolean/2,
	 encode_integer/2,encode_integer/3,
	 decode_integer/2,
	 number2name/2,
	 encode_unnamed_bit_string/2,encode_unnamed_bit_string/3,
	 encode_named_bit_string/3,encode_named_bit_string/4,
	 encode_bit_string/4,
	 decode_named_bit_string/3,
	 decode_compact_bit_string/2,compact_bit_string_size/1,
	 decode_native_bit_string/2,
	 native_to_legacy_bit_string/1,
	 encode_null/2,decode_null/2,
	 encode_relative_oid/2,decode_relative_oid/2,
	 encode_object_identifier/2,decode_object_identifier/2,
	 encode_restricted_string/2,
	 decode_octet_string/2,
	 decode_restricted_string/2,
	 encode_universal_string/2,decode_universal_string/2,
	 encode_UTF8_string/2,decode_UTF8_string/2,
	 encode_BMP_string/2,decode_BMP_string/2]).

-export([encode_open_type/2,decode_open_type/2,
	 decode_open_type_as_binary/2]).

-export([decode_primitive_incomplete/2,decode_selective/2]).

%% For DER.
-export([dynamicsort_SET_components/1,dynamicsort_SETOF/1]).

%% the encoding of class of tag bits 8 and 7
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


%% The complete tag-word of built-in types
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

ber_encode([Tlv]) ->
    ber_encode(Tlv);
ber_encode(Tlv) when is_binary(Tlv) ->
    Tlv;
ber_encode(Tlv) ->
    asn1rt_nif:encode_ber_tlv(Tlv).

ber_decode_nif(B) ->
    asn1rt_nif:decode_ber_tlv(B).

ber_decode_erlang(B) when is_binary(B) ->
    decode_primitive(B);
ber_decode_erlang(Tlv) ->
    {Tlv,<<>>}.

decode_primitive(Bin) ->
    {Form,TagNo,V,Rest} = decode_tag_and_length(Bin),
    case Form of
	1 ->                                    % constructed
	    {{TagNo,decode_constructed(V)},Rest};
	0 ->                                    % primitive
	    {{TagNo,V},Rest};
	2 ->                                  % constructed indefinite
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
	    asn1_NOVALUE
    end;
decode_primitive_incomplete([[default,TagNo,Directives]],Bin) ->
    %% default, constructed type, Directives points into this type
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->
	    decode_incomplete2(Form,TagNo,V,Directives,Rest);
	_ ->
	    asn1_NOVALUE
    end;
decode_primitive_incomplete([[opt,TagNo]],Bin) ->
    %% optional
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->
	    decode_incomplete2(Form,TagNo,V,[],Rest);
	_ ->
	    asn1_NOVALUE
    end;
decode_primitive_incomplete([[opt,TagNo,Directives]],Bin) ->
    %% optional
    case decode_tag_and_length(Bin) of
	{Form,TagNo,V,Rest} ->
	    decode_incomplete2(Form,TagNo,V,Directives,Rest);
	_ ->
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
    case decode_tag_and_length(Bin) of
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
decode_primitive_incomplete([[undec,_TagNo]|_RestTag],Bin) ->
    %% incomlete decode
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
    LenPart = byte_size(Bin) - byte_size(Rest2),
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
  when Alt =:= alt_undec; Alt =:= alt; Alt =:= alt_parts ->
    {_Form,TagNo,V,Rest} = decode_tag_and_length(Bin),
    case incomplete_choice_alt(TagNo, Directives) of
	{alt_undec,_} ->
	    LenA = byte_size(Bin) - byte_size(Rest),
	    <<A:LenA/binary,Rest/binary>> = Bin,
	    A;
	{alt,InnerDirectives} ->
	    {Tlv,Rest} = decode_primitive_incomplete(InnerDirectives,V),
	    {TagNo,Tlv};
	{alt_parts,_} ->
	    [{TagNo,decode_parts_incomplete(V)}];
	no_match ->
            %% if a choice alternative was encoded that
	    %% was not specified in the config file,
	    %% thus decode component anonomous.
	    {Tlv,_}=decode_primitive(Bin),
	    Tlv
    end;
decode_constructed_incomplete([TagNo|RestTag],Bin) ->
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
    case decode_primitive_incomplete([Tag],Bin) of
	{Tlv,Rest} ->
	    decode_constr_indef_incomplete(RestTags,Rest,[Tlv|Acc]);
	asn1_NOVALUE ->
	    decode_constr_indef_incomplete(RestTags,Bin,Acc)
    end.


decode_incomplete_bin(Bin) ->
    {ok,Rest} = skip_tag(Bin),
    {ok,Rest2} = skip_length_and_value(Rest),
    IncLen = byte_size(Bin) - byte_size(Rest2),
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

match_tags({T,V}, [T]) ->
    V;
match_tags({T,V}, [T|Tt]) ->
    match_tags(V,Tt);
match_tags([{T,V}], [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T,_V}|_]=Vlist, [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag,_V}=Tlv, [T|_Tt]) ->
    exit({error,{asn1,{wrong_tag,{{expected,T},{got,Tag,Tlv}}}}}).

%%%
%% skips components that do not match a tag in Tags
skip_ExtensionAdditions([], _Tags) ->
    [];
skip_ExtensionAdditions([{Tag,_}|Rest]=TLV, Tags) ->
    case [X || X=T <- Tags, T =:= Tag] of
	[] ->
	    %% skip this TLV and continue with next
	    skip_ExtensionAdditions(Rest,Tags);
	_ ->
	    TLV
    end.


%%===============================================================================
%% Decode a tag
%%
%% decode_tag(OctetListBuffer) -> {{Form, (Class bsl 16)+ TagNo}, RestOfBuffer, RemovedBytes}
%%===============================================================================

decode_tag_and_length(<<Class:2, Form:1, TagNo:5, 0:1, Length:7, V:Length/binary, RestBuffer/binary>>) when TagNo < 31 ->
    {Form, (Class bsl 16) bor TagNo, V, RestBuffer};
decode_tag_and_length(<<Class:2, 1:1, TagNo:5, 1:1, 0:7, T/binary>>) when TagNo < 31 ->
    {2, (Class bsl 16) + TagNo, T, <<>>};
decode_tag_and_length(<<Class:2, Form:1, TagNo:5, 1:1, LL:7, Length:LL/unit:8,V:Length/binary, T/binary>>) when TagNo < 31 ->
    {Form, (Class bsl 16) bor TagNo, V, T};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 0:1, TagNo:7, 0:1, Length:7, V:Length/binary, RestBuffer/binary>>) ->
    {Form, (Class bsl 16) bor TagNo, V, RestBuffer};
decode_tag_and_length(<<Class:2, 1:1, 31:5, 0:1, TagNo:7, 1:1, 0:7, T/binary>>)  ->
    {2, (Class bsl 16) bor TagNo, T, <<>>};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 0:1, TagNo:7, 1:1, LL:7, Length:LL/unit:8, V:Length/binary, T/binary>>)  ->
    {Form, (Class bsl 16) bor TagNo, V, T};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 1:1, TagPart1:7, 0:1, TagPartLast, Buffer/binary>>) ->
    TagNo = (TagPart1 bsl 7) bor TagPartLast,
    {Length, RestBuffer} = decode_length(Buffer),
    << V:Length/binary, RestBuffer2/binary>> = RestBuffer,
    {Form, (Class bsl 16) bor TagNo, V, RestBuffer2};
decode_tag_and_length(<<Class:2, Form:1, 31:5, Buffer/binary>>) ->
    {TagNo, Buffer1} = decode_tag(Buffer, 0),
    {Length, RestBuffer} = decode_length(Buffer1),
    << V:Length/binary, RestBuffer2/binary>> = RestBuffer,
    {Form, (Class bsl 16) bor TagNo, V, RestBuffer2}.



%% last partial tag
decode_tag(<<0:1,PartialTag:7, Buffer/binary>>, TagAck) ->
    TagNo = (TagAck bsl 7) bor PartialTag,
    {TagNo, Buffer};
%% more tags
decode_tag(<<_:1,PartialTag:7, Buffer/binary>>, TagAck) ->
    TagAck1 = (TagAck bsl 7) bor PartialTag,
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
    {Bytes2,L2} = encode_length(LenSoFar),
    encode_tags(Trest, [Tag,Bytes2|BytesSoFar],
		 LenSoFar + byte_size(Tag) + L2);
encode_tags([], BytesSoFar, LenSoFar) ->
    {BytesSoFar,LenSoFar}.

encode_tags(TagIn, {BytesSoFar,LenSoFar}) ->
    encode_tags(TagIn, BytesSoFar, LenSoFar).

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

encode_open_type(Val, T) when is_list(Val) ->
    encode_open_type(list_to_binary(Val), T);
encode_open_type(Val, Tag) ->
    encode_tags(Tag, Val, byte_size(Val)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Tlv, TagIn) -> Value
%% Tlv = {Tag,V} | V where V -> binary()
%% TagIn = [TagVal] where TagVal -> int()
%% Value = binary with decoded data (which must be decoded again as some type)
%%
decode_open_type(Tlv, TagIn) ->
    case match_tags(Tlv, TagIn) of
	Bin when is_binary(Bin) ->
	    {InnerTlv,_} = ber_decode_nif(Bin),
	    InnerTlv;
	TlvBytes -> TlvBytes
    end.

decode_open_type_as_binary(Tlv, TagIn)->
    ber_encode(match_tags(Tlv, TagIn)).

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

encode_integer(Val, Tag) when is_integer(Val) ->
    encode_tags(Tag, encode_integer(Val));
encode_integer(Val, _Tag) ->
    exit({error,{asn1,{encode_integer,Val}}}).


encode_integer(Val, NamedNumberList, Tag) when is_atom(Val) ->
    case lists:keyfind(Val, 1, NamedNumberList) of
	{_, NewVal} ->
	    encode_tags(Tag, encode_integer(NewVal));
	_ ->
	    exit({error,{asn1, {encode_integer_namednumber, Val}}})
    end;
encode_integer(Val, _NamedNumberList, Tag) ->
    encode_tags(Tag, encode_integer(Val)).

encode_integer(Val) ->
    Bytes =
	if
	    Val >= 0 ->
		encode_integer_pos(Val, []);
	    true ->
		encode_integer_neg(Val, [])
	end,
    {Bytes,length(Bytes)}.

encode_integer_pos(0, [B|_Acc]=L) when B < 128 ->
    L;
encode_integer_pos(N, Acc) ->
    encode_integer_pos((N bsr 8), [N band 16#ff| Acc]).

encode_integer_neg(-1, [B1|_T]=L) when B1 > 127 ->
    L;
encode_integer_neg(N, Acc) ->
    encode_integer_neg(N bsr 8, [N band 16#ff|Acc]).

%%===============================================================================
%% decode integer
%%===============================================================================

decode_integer(Tlv, TagIn) ->
    Bin = match_tags(Tlv, TagIn),
    Len = byte_size(Bin),
    <<Int:Len/signed-unit:8>> = Bin,
    Int.

number2name(Int, NamedNumberList) ->
    case lists:keyfind(Int, 2, NamedNumberList) of
	{NamedVal,_} ->
	    NamedVal;
	_ ->
	    Int
    end.

%%============================================================================
%% Bitstring value, ITU_T X.690 Chapter 8.6
%%
%% encode bitstring value
%%============================================================================

encode_unnamed_bit_string(Bits, TagIn) ->
    Unused = (8 - (bit_size(Bits) band 7)) band 7,
    Bin = <<Unused,Bits/bitstring,0:Unused>>,
    encode_tags(TagIn, Bin, byte_size(Bin)).

encode_unnamed_bit_string(MaxBits, Bits, TagIn) ->
    NumBits = bit_size(Bits),
    Unused = (8 - (NumBits band 7)) band 7,
    Bin = <<Unused,Bits/bitstring,0:Unused>>,
    if
	NumBits > MaxBits ->
	    exit({error,{asn1,
			 {bitstring_length,
			  {{was,NumBits},{maximum,MaxBits}}}}});
	true ->
	    encode_tags(TagIn, Bin, byte_size(Bin))
    end.

encode_named_bit_string([H|_]=Bits, NamedBitList, TagIn) when is_atom(H) ->
    do_encode_named_bit_string(Bits, NamedBitList, TagIn);
encode_named_bit_string([{bit,_}|_]=Bits, NamedBitList, TagIn) ->
    do_encode_named_bit_string(Bits, NamedBitList, TagIn);
encode_named_bit_string([], _NamedBitList, TagIn) ->
    encode_unnamed_bit_string(<<>>, TagIn);
encode_named_bit_string(Bits, _NamedBitList, TagIn) when is_bitstring(Bits) ->
    encode_unnamed_bit_string(Bits, TagIn).

encode_named_bit_string(C, [H|_]=Bits, NamedBitList, TagIn) when is_atom(H) ->
    do_encode_named_bit_string(C, Bits, NamedBitList, TagIn);
encode_named_bit_string(C, [{bit,_}|_]=Bits, NamedBitList, TagIn) ->
    do_encode_named_bit_string(C, Bits, NamedBitList, TagIn);
encode_named_bit_string(C, [], _NamedBitList, TagIn) ->
    encode_unnamed_bit_string(C, <<>>, TagIn);
encode_named_bit_string(C, Bits, _NamedBitList, TagIn) when is_bitstring(Bits) ->
    encode_unnamed_bit_string(C, Bits, TagIn).

do_encode_named_bit_string([FirstVal | RestVal], NamedBitList, TagIn) ->
    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []),
    Size = lists:max(ToSetPos) + 1,
    BitList = make_and_set_list(Size, ToSetPos, 0),
    {Len,Unused,OctetList} = encode_bitstring(BitList),
    encode_tags(TagIn, [Unused|OctetList],Len+1).

do_encode_named_bit_string(Size, [FirstVal | RestVal], NamedBitList, TagIn) ->
    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []),
    BitList = make_and_set_list(Size, ToSetPos, 0),
    {Len, Unused, OctetList} = encode_bitstring(BitList),
    encode_tags(TagIn, [Unused|OctetList], Len+1).

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

encode_bit_string(C, Bits, NamedBitList, TagIn) when is_bitstring(Bits) ->
    PadLen = (8 - (bit_size(Bits) band 7)) band 7,
    Compact = {PadLen,<<Bits/bitstring,0:PadLen>>},
    encode_bin_bit_string(C, Compact, NamedBitList, TagIn);
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
    encode_bit_string_bits(C, BitListVal, NamedBitList, TagIn).


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
    case C of
	[] ->
	    remove_unused_then_dotag(TagIn, Unused, BinBits);
	{_Min,Max} ->
	    BBLen = (byte_size(BinBits)*8)-Unused,
	    if
		BBLen > Max ->
		    exit({error,{asn1,
				 {bitstring_length,
				  {{was,BBLen},{maximum,Max}}}}});
		true ->
		    remove_unused_then_dotag(TagIn, Unused, BinBits)
	    end;
	Size ->
	    case ((byte_size(BinBits)*8)-Unused) of
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
	0 when byte_size(BinBits) =:= 0 ->
	    encode_tags(TagIn, <<0>>, 1);
	0 ->
	    Bin = <<Unused,BinBits/binary>>,
	    encode_tags(TagIn, Bin, byte_size(Bin));
	Num ->
	    N = byte_size(BinBits)-1,
	    <<BBits:N/binary,LastByte>> = BinBits,
	    encode_tags(TagIn,
			[Unused,binary_to_list(BBits) ++[(LastByte bsr Num) bsl Num]],
			1+byte_size(BinBits))
    end.


%%=================================================================
%% Encode named bits
%%=================================================================

encode_bit_string_named(C, [FirstVal | RestVal], NamedBitList, TagIn) ->
    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []),
    Size = case C of
	       [] ->
		   lists:max(ToSetPos) + 1;
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
    case lists:keyfind(Val, 1, NamedBitList) of
	{_ValName, ValPos} ->
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
    case C of
	[] ->
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
	Size when is_integer(Size) ->
	    case length(BitListVal) of
		BitSize when BitSize == Size ->
		    {Len, Unused, OctetList} = encode_bitstring(BitListVal),
		    %% add unused byte to the Len
		    encode_tags(TagIn, [Unused | OctetList], Len+1);
		BitSize when BitSize < Size ->
		    PaddedList = pad_bit_list(Size-BitSize,BitListVal),
		    {Len, Unused, OctetList} = encode_bitstring(PaddedList),
		    %% add unused byte to the Len
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
pad_bit_list(Size, BitListVal) ->
    Tail = lists:duplicate(Size,0),
    lists:append(BitListVal, Tail).

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
    {Unused, Val} = unused_bitlist(Rest, 7, 0),
    {Len + 1, Unused, [Ack | [Val]]}.

%%%%%%%%%%%%%%%%%%
%% unused_bitlist([list of ones and zeros <= 7], 7, []) ->
%%  {Unused bits, Last octet with bits moved to right}
unused_bitlist([], Trail, Ack) ->
    {Trail + 1, Ack};
unused_bitlist([Bit | Rest], Trail, Ack) ->
    unused_bitlist(Rest, Trail - 1, (Bit bsl Trail) bor Ack).


%%============================================================================
%% decode bitstring value
%%============================================================================

decode_compact_bit_string(Buffer, Tags) ->
    case match_and_collect(Buffer, Tags) of
	<<0>> -> {0,<<>>};
	<<Unused,Bits/binary>> -> {Unused,Bits}
    end.

compact_bit_string_size({Unused,Bits}) ->
    bit_size(Bits) - Unused.

decode_native_bit_string(Buffer, Tags) ->
    case match_and_collect(Buffer, Tags) of
	<<0>> ->
	    <<>>;
	<<Unused,Bits/binary>> ->
	    Size = bit_size(Bits) - Unused,
	    <<Val:Size/bitstring,_:Unused/bitstring>> = Bits,
	    Val
    end.

decode_named_bit_string(Buffer, NamedNumberList, Tags) ->
    case match_and_collect(Buffer, Tags) of
	<<0>> ->
	    [];
	<<Unused,Bits/binary>> ->
	    BitString = decode_bitstring2(byte_size(Bits), Unused, Bits),
	    decode_bitstring_NNL(BitString, NamedNumberList)
    end.

%%----------------------------------------
%% Decode the in buffer to bits
%%----------------------------------------
decode_bitstring2(1, Unused,
		  <<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,_/binary>>) ->
    lists:sublist([B7,B6,B5,B4,B3,B2,B1,B0], 8-Unused);
decode_bitstring2(Len, Unused,
		  <<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,Buffer/binary>>) ->
    [B7,B6,B5,B4,B3,B2,B1,B0|
     decode_bitstring2(Len - 1, Unused, Buffer)].

native_to_legacy_bit_string(Bits) ->
    [B || <<B:1>> <= Bits].

%%----------------------------------------
%% Decode the bitlist to names
%%----------------------------------------

decode_bitstring_NNL(BitList, NamedNumberList) ->
    decode_bitstring_NNL(BitList, NamedNumberList, 0, []).


decode_bitstring_NNL([],_,_No,Result) ->
    lists:reverse(Result);
decode_bitstring_NNL([B|BitList],[{Name,No}|NamedNumberList],No,Result) ->
    if
	B =:= 0 ->
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,Result);
	true ->
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,[Name|Result])
    end;
decode_bitstring_NNL([1|BitList],NamedNumberList,No,Result) ->
    decode_bitstring_NNL(BitList,NamedNumberList,No+1,[{bit,No}|Result]);
decode_bitstring_NNL([0|BitList],NamedNumberList,No,Result) ->
    decode_bitstring_NNL(BitList,NamedNumberList,No+1,Result).

%%============================================================================
%% Null value, ITU_T X.690 Chapter 8.8
%%
%% encode NULL value
%%============================================================================

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

encode_object_identifier(Val, TagIn) ->
    encode_tags(TagIn, e_object_identifier(Val)).

e_object_identifier({'OBJECT IDENTIFIER', V}) ->
    e_object_identifier(V);
e_object_identifier(V) when is_tuple(V) ->
    e_object_identifier(tuple_to_list(V));

%%%%%%%%%%%%%%%
%% e_object_identifier([List of Obect Identifiers]) ->
%% {[Encoded Octetlist of ObjIds], IntLength}
%%
e_object_identifier([E1,E2|Tail]) ->
    Head = 40*E1 + E2,  % wow!
    {H,Lh} = mk_object_val(Head),
    {R,Lr} = lists:mapfoldl(fun enc_obj_id_tail/2, 0, Tail),
    {[H|R],Lh+Lr}.

enc_obj_id_tail(H, Len) ->
    {B,L} = mk_object_val(H),
    {B,Len+L}.


%%%%%%%%%%%
%% mk_object_val(Value) -> {OctetList, Len}
%% returns a Val as a list of octets, the 8th bit is always set to one
%% except for the last octet, where it's 0
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

encode_relative_oid(Val,TagIn) when is_tuple(Val) ->
    encode_relative_oid(tuple_to_list(Val),TagIn);
encode_relative_oid(Val,TagIn) ->
    encode_tags(TagIn, enc_relative_oid(Val)).

enc_relative_oid(Tuple) when is_tuple(Tuple) ->
    enc_relative_oid(tuple_to_list(Tuple));
enc_relative_oid(Val) ->
    lists:mapfoldl(fun(X,AccIn) ->
			   {SO,L} = mk_object_val(X),
			   {SO,L+AccIn}
		   end, 0, Val).

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
encode_restricted_string(OctetList, TagIn)  when is_binary(OctetList) ->
    encode_tags(TagIn, OctetList, byte_size(OctetList));
encode_restricted_string(OctetList, TagIn) when is_list(OctetList) ->
    encode_tags(TagIn, OctetList, length(OctetList)).

%%============================================================================
%% decode OCTET STRING to binary
%%============================================================================

decode_octet_string(Tlv, TagsIn) ->
    Bin = match_and_collect(Tlv, TagsIn),
    binary:copy(Bin).

%%============================================================================
%% decode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings
%%============================================================================

decode_restricted_string(Tlv, TagsIn) ->
    match_and_collect(Tlv, TagsIn).

%%============================================================================
%% encode Universal string
%%============================================================================

encode_universal_string(Universal, TagIn) ->
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

decode_universal_string(Buffer, Tags) ->
    Bin = match_and_collect(Buffer, Tags),
    mk_universal_string(binary_to_list(Bin)).

mk_universal_string(In) ->
    mk_universal_string(In, []).

mk_universal_string([], Acc) ->
    lists:reverse(Acc);
mk_universal_string([0,0,0,D|T], Acc) ->
    mk_universal_string(T, [D|Acc]);
mk_universal_string([A,B,C,D|T], Acc) ->
    mk_universal_string(T, [{A,B,C,D}|Acc]).


%%============================================================================
%% encode UTF8 string
%%============================================================================

encode_UTF8_string(UTF8String, TagIn) when is_binary(UTF8String) ->
    encode_tags(TagIn, UTF8String, byte_size(UTF8String));
encode_UTF8_string(UTF8String, TagIn) ->
    encode_tags(TagIn, UTF8String, length(UTF8String)).


%%============================================================================
%% decode UTF8 string
%%============================================================================

decode_UTF8_string(Tlv,TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
	[_|_]=PartList ->			% constructed val
	    collect_parts(PartList);
	Bin ->
	    Bin
    end.


%%============================================================================
%% encode BMP string
%%============================================================================

encode_BMP_string(BMPString, TagIn) ->
    OctetList = mk_BMP_list(BMPString),
    encode_tags(TagIn, OctetList, length(OctetList)).

mk_BMP_list(In) ->
    mk_BMP_list(In, []).

mk_BMP_list([],List) ->
    lists:reverse(List);
mk_BMP_list([{0,0,C,D}|T], List) ->
    mk_BMP_list(T, [D,C|List]);
mk_BMP_list([H|T], List) ->
    mk_BMP_list(T, [H,0|List]).

%%============================================================================
%% decode (OctetList, Range(ignored), tag|notag) -> {ValList, RestList}
%%    (Buffer, Range, StringType, HasTag, TotalLen) ->
%%                               {String, Remain, RemovedBytes}
%%============================================================================
decode_BMP_string(Buffer, Tags) ->
    Bin = match_and_collect(Buffer, Tags),
    mk_BMP_string(binary_to_list(Bin)).

mk_BMP_string(In) ->
    mk_BMP_string(In,[]).

mk_BMP_string([], US) ->
    lists:reverse(US);
mk_BMP_string([0,B|T], US) ->
    mk_BMP_string(T, [B|US]);
mk_BMP_string([C,D|T], US) ->
    mk_BMP_string(T, [{0,0,C,D}|US]).

%%============================================================================
%% Length handling
%%
%% Encode length
%%
%% encode_length(Int) ->
%%          [<127]| [128 + Int (<127),OctetList] | [16#80]
%%============================================================================

encode_length(L) when L =< 16#7F ->
    {[L],1};
encode_length(L) ->
    Oct = minimum_octets(L),
    Len = length(Oct),
    if
	Len =< 126 ->
	    {[16#80 bor Len|Oct],Len+1};
	true ->
	    exit({error,{asn1, too_long_length_oct, Len}})
    end.

%% Val must be >= 0
minimum_octets(Val) ->
    minimum_octets(Val, []).

minimum_octets(0, Acc) ->
    Acc;
minimum_octets(Val, Acc) ->
    minimum_octets(Val bsr 8, [Val band 16#FF|Acc]).


%%===========================================================================
%% Decode length
%%
%% decode_length(OctetList) -> {{indefinite, RestOctetsL}, NoRemovedBytes} |
%%                             {{Length, RestOctetsL}, NoRemovedBytes}
%%===========================================================================

decode_length(<<1:1,0:7,T/binary>>) ->
    {indefinite,T};
decode_length(<<0:1,Length:7,T/binary>>) ->
    {Length,T};
decode_length(<<1:1,LL:7,Length:LL/unit:8,T/binary>>) ->
    {Length,T}.

%% dynamicsort_SET_components(Arg) ->
%% Res Arg -> list()
%% Res -> list()
%% Sorts the elements in Arg according to the encoded tag in
%% increasing order.
dynamicsort_SET_components(ListOfEncCs) ->
    TagBinL = [begin
		   Bin = list_to_binary(L),
		   {dynsort_decode_tag(Bin),Bin}
	       end || L <- ListOfEncCs],
    [E || {_,E} <- lists:keysort(1, TagBinL)].

%% dynamicsort_SETOF(Arg) -> Res
%% Arg -> list()
%% Res -> list()
%% Sorts the elements in Arg in increasing size
dynamicsort_SETOF(ListOfEncVal) ->
    BinL = lists:map(fun(L) when is_list(L) -> list_to_binary(L);
			(B) -> B end, ListOfEncVal),
    lists:sort(BinL).

%% multiple octet tag
dynsort_decode_tag(<<Class:2,_Form:1,31:5,Buffer/binary>>) ->
    TagNum = dynsort_decode_tag(Buffer, 0),
    {Class,TagNum};

%% single tag (< 31 tags)
dynsort_decode_tag(<<Class:2,_Form:1,TagNum:5,_/binary>>) ->
    {Class,TagNum}.

dynsort_decode_tag(<<0:1,PartialTag:7,_/binary>>, TagAcc) ->
    (TagAcc bsl 7) bor PartialTag;
dynsort_decode_tag(<<_:1,PartialTag:7,Buffer/binary>>, TagAcc0) ->
    TagAcc = (TagAcc0 bsl 7) bor PartialTag,
    dynsort_decode_tag(Buffer, TagAcc).


%%-------------------------------------------------------------------------
%% INTERNAL HELPER FUNCTIONS (not exported)
%%-------------------------------------------------------------------------

match_and_collect(Tlv, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
	[_|_]=PartList ->			% constructed val
	    collect_parts(PartList);
	Bin when is_binary(Bin) ->
	    Bin
    end.

collect_parts(TlvList) ->
    collect_parts(TlvList, []).

collect_parts([{_,L}|Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L)|Acc]);
collect_parts([{?N_BIT_STRING,<<Unused,Bits/binary>>}|Rest], _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T,V}|Rest], Acc) ->
    collect_parts(Rest, [V|Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

collect_parts_bit([{?N_BIT_STRING,<<Unused,Bits/binary>>}|Rest], Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits|Acc], Unused+Uacc);
collect_parts_bit([], Acc, Uacc) ->
    list_to_binary([Uacc|lists:reverse(Acc)]).
