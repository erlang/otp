%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
-module(asn1rt_ber_bin).

%% encoding / decoding of BER

-export([decode/1]).
-export([fixoptionals/2,split_list/2,cindex/3,restbytes2/3,
	 list_to_record/2,
	 encode_tag_val/1,decode_tag/1,peek_tag/1,
	 check_tags/3, encode_tags/3]).
-export([encode_boolean/2,decode_boolean/3,
	 encode_integer/3,encode_integer/4,
	 decode_integer/4,decode_integer/5,encode_enumerated/2,
	 encode_enumerated/4,decode_enumerated/5,
	 encode_real/2, encode_real/3,
	 decode_real/2, decode_real/4,
	 encode_bit_string/4,decode_bit_string/6,
	 decode_compact_bit_string/6,
	 encode_octet_string/3,decode_octet_string/5,
	 encode_null/2,decode_null/3,
	 encode_object_identifier/2,decode_object_identifier/3,
	 encode_relative_oid/2,decode_relative_oid/3,
	 encode_restricted_string/4,decode_restricted_string/6,
	 encode_universal_string/3,decode_universal_string/5,
	 encode_UTF8_string/3, decode_UTF8_string/3,
	 encode_BMP_string/3,decode_BMP_string/5,
	 encode_generalized_time/3,decode_generalized_time/5,
	 encode_utc_time/3,decode_utc_time/5,
	 encode_length/1,decode_length/1,
	 check_if_valid_tag/3,
	 decode_tag_and_length/1, decode_components/6,
	 decode_components/7, decode_set/6]).

-export([encode_open_type/1,encode_open_type/2,decode_open_type/1,decode_open_type/2,decode_open_type/3]).
-export([skipvalue/1, skipvalue/2,skip_ExtensionAdditions/2]).

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
-define(N_UTF8String, 12).
-define('N_RELATIVE-OID',13).
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


decode(Bin) ->
    decode_primitive(Bin).

decode_primitive(Bin) ->
    {Tlv = {Tag,Len,V},<<>>} = decode_tlv(Bin),
    case element(2,Tag) of
	?CONSTRUCTED ->
	    {Tag,Len,decode_constructed(V)};
	_ ->
	    Tlv
    end.

decode_constructed(<<>>) ->
    [];
decode_constructed(Bin) ->
    {Tlv = {Tag,Len,V},Rest} = decode_tlv(Bin),
    NewTlv =
	case element(2,Tag) of
	    ?CONSTRUCTED ->
		{Tag,Len,decode_constructed(V)};
	    _ ->
		Tlv
	end,
    [NewTlv|decode_constructed(Rest)].

decode_tlv(Bin) ->
    {Tag,Bin1,_Rb1} = decode_tag(Bin),
    {{Len,Bin2},_Rb2} = decode_length(Bin1),
    <<V:Len/binary,Bin3/binary>> = Bin2,
    {{Tag,Len,V},Bin3}.



%%%%%%%%%%%%%
% split_list(List,HeadLen) -> {HeadList,TailList}
%
% splits List into HeadList (Length=HeadLen) and TailList
% if HeadLen == indefinite -> return {List,indefinite}
split_list(List,indefinite) ->
    {List, indefinite};
split_list(Bin, Len) when is_binary(Bin) ->
    split_binary(Bin,Len);
split_list(List,Len) ->
    {lists:sublist(List,Len),lists:nthtail(Len,List)}.


%%% new function which fixes a bug regarding indefinite length decoding
restbytes2(indefinite,<<0,0,RemBytes/binary>>,_) ->
    {RemBytes,2};
restbytes2(indefinite,RemBytes,ext) ->
    skipvalue(indefinite,RemBytes);
restbytes2(RemBytes,<<>>,_) ->
    {RemBytes,0};
restbytes2(_RemBytes,Bytes,noext) ->
    exit({error,{asn1, {unexpected,Bytes}}});
restbytes2(RemBytes,Bytes,ext) ->
%%    {RemBytes,0}.
    {RemBytes,byte_size(Bytes)}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% skipvalue(Length, Bytes) -> {RemainingBytes, RemovedNumberOfBytes}
%%
%% skips the one complete (could be nested) TLV from Bytes
%% handles both definite and indefinite length encodings
%%

skipvalue(L, Bytes) ->
    skipvalue(L, Bytes, 0).

skipvalue(L, Bytes, Rb) ->
    skipvalue(L, Bytes, Rb, 0).

skipvalue(indefinite, Bytes, Rb, IndefLevel) ->
    {T,Bytes2,R2} = decode_tag(Bytes),
    {{L,Bytes3},R3} = decode_length(Bytes2),
    case {T,L} of
	{_,indefinite} ->
	    skipvalue(indefinite,Bytes3,Rb+R2+R3,IndefLevel+1);
	{{0,0,0},0} when IndefLevel =:= 0 ->
	    %% See X690 8.1.5 NOTE, end of indefinite content
	    {Bytes3,Rb+2};
	{{0,0,0},0} ->
	    skipvalue(indefinite,Bytes3,Rb+2,IndefLevel - 1);
	_ ->
	    <<_:L/binary, RestBytes/binary>> = Bytes3,
	    skipvalue(indefinite,RestBytes,Rb+R2+R3+L, IndefLevel)
	    %%{RestBytes, R2+R3+L}
    end;
%%     case Bytes4 of
%% 	<<0,0,Bytes5/binary>> ->
%% 	    {Bytes5,Rb+Rb4+2};
%% 	_  -> skipvalue(indefinite,Bytes4,Rb+Rb4)
%%     end;
skipvalue(L, Bytes, Rb, _) ->
%    <<Skip:L/binary, RestBytes/binary>> = Bytes,
    <<_:L/binary, RestBytes/binary>> = Bytes,
    {RestBytes,Rb+L}.


skipvalue(Bytes) ->
    {_T,Bytes2,R2} = decode_tag(Bytes),
    {{L,Bytes3},R3} = decode_length(Bytes2),
    skipvalue(L,Bytes3,R2+R3).


cindex(Ix,Val,Cname) ->
    case element(Ix,Val) of
	{Cname,Val2} -> Val2;
	X -> X
    end.

%%%
%% skips byte sequence of Bytes that do not match a tag in Tags
skip_ExtensionAdditions(Bytes,Tags) ->
    skip_ExtensionAdditions(Bytes,Tags,0).
skip_ExtensionAdditions(<<>>,_Tags,RmB) ->
    {<<>>,RmB};
skip_ExtensionAdditions(Bytes,Tags,RmB) ->
    case catch decode_tag(Bytes) of
	{'EXIT',_Reason} ->
	    tag_error(no_data,Tags,Bytes,'OPTIONAL');
	{_T={Class,_Form,TagNo},_Bytes2,_R2} ->
	    case [X||X=#tag{class=Cl,number=TN} <- Tags,Cl==Class,TN==TagNo] of
		[] ->
		    %% skip this TLV and continue with next
		    {Bytes3,R3} = skipvalue(Bytes),
		    skip_ExtensionAdditions(Bytes3,Tags,RmB+R3);
		_ ->
		    {Bytes,RmB}
	    end
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
    <<(Class bsr 6):2, (Form bsr 5):1, 31:5,BinOct/binary>>;

%% asumes whole correct tag bitpattern, multiple of 8
encode_tag_val(Tag) when (Tag =< 255) -> Tag;  %% används denna funktion??!!
%% asumes correct bitpattern of 0-5
encode_tag_val(Tag) -> encode_tag_val2(Tag,[]).

encode_tag_val2(Tag, OctAck) when (Tag =< 255) ->
    [Tag | OctAck];
encode_tag_val2(Tag, OctAck) ->
    encode_tag_val2(Tag bsr 8, [255 band Tag | OctAck]).


%%%encode_tag(TagClass(?UNI, APP etc), Form (?PRIM etx), TagInteger) ->
%%%     8bit Int | [list of octets]
%encode_tag_val({Class, Form, TagNo}) when (TagNo =< 30) ->
%%%    <<Class:2,Form:1,TagNo:5>>;
%    [Class bor Form bor TagNo];
%encode_tag_val({Class, Form, TagNo}) ->
%    {Octets,L} = mk_object_val(TagNo),
%    [Class bor Form bor 31 | Octets];


%%============================================================================\%% Peek on the initial tag
%% peek_tag(Bytes) -> TagBytes
%% interprets the first byte and possible  second, third and fourth byte as
%% a tag and returns all the bytes comprising the tag, the constructed/primitive bit (6:th bit of first byte) is normalised to 0
%%

peek_tag(<<B7_6:2,_:1,31:5,Buffer/binary>>) ->
    Bin = peek_tag(Buffer, <<>>),
    <<B7_6:2,31:6,Bin/binary>>;
%% single tag (tagno < 31)
peek_tag(<<B7_6:2,_:1,B4_0:5,_Buffer/binary>>) ->
    <<B7_6:2,B4_0:6>>.

peek_tag(<<0:1,PartialTag:7,_Buffer/binary>>, TagAck) ->
    <<TagAck/binary,PartialTag>>;
peek_tag(<<PartialTag,Buffer/binary>>, TagAck) ->
    peek_tag(Buffer,<<TagAck/binary,PartialTag>>);
peek_tag(_,TagAck) ->
    exit({error,{asn1, {invalid_tag,TagAck}}}).
%%peek_tag([Tag|Buffer]) when (Tag band 31) =:= 31 ->
%%    [Tag band 2#11011111 | peek_tag(Buffer,[])];
%%%% single tag (tagno < 31)
%%peek_tag([Tag|Buffer]) ->
%%    [Tag band 2#11011111].

%%peek_tag([PartialTag|Buffer], TagAck) when (PartialTag < 128 ) ->
%%    lists:reverse([PartialTag|TagAck]);
%%peek_tag([PartialTag|Buffer], TagAck) ->
%%    peek_tag(Buffer,[PartialTag|TagAck]);
%%peek_tag(Buffer,TagAck) ->
%%    exit({error,{asn1, {invalid_tag,lists:reverse(TagAck)}}}).


%%===============================================================================
%% Decode a tag
%%
%% decode_tag(OctetListBuffer) -> {{Class, Form, TagNo}, RestOfBuffer, RemovedBytes}
%%===============================================================================

%% multiple octet tag
decode_tag(<<Class:2, Form:1, 31:5, Buffer/binary>>) ->
    {TagNo, Buffer1, RemovedBytes} = decode_tag(Buffer, 0, 1),
    {{(Class bsl 6), (Form bsl 5), TagNo}, Buffer1, RemovedBytes};

%% single tag (< 31 tags)
decode_tag(<<Class:2,Form:1,TagNo:5, Buffer/binary>>) ->
    {{(Class bsl 6), (Form bsl 5), TagNo}, Buffer, 1}.

%% last partial tag
decode_tag(<<0:1,PartialTag:7, Buffer/binary>>, TagAck, RemovedBytes) ->
    TagNo = (TagAck bsl 7) bor PartialTag,
    %%<<TagNo>> = <<TagAck:1, PartialTag:7>>,
    {TagNo, Buffer, RemovedBytes+1};
% more tags
decode_tag(<<_:1,PartialTag:7, Buffer/binary>>, TagAck, RemovedBytes) ->
    TagAck1 = (TagAck bsl 7) bor PartialTag,
    %%<<TagAck1:16>> = <<TagAck:1, PartialTag:7,0:8>>,
    decode_tag(Buffer, TagAck1, RemovedBytes+1).

%%------------------------------------------------------------------
%% check_tags_i is the same as check_tags except that it stops and
%% returns the remaining tags not checked when it encounters an
%% indefinite length field
%% only called internally within this module

check_tags_i([Tag], Buffer, OptOrMand) -> % optimized very usual case
    {[],check_one_tag(Tag, Buffer, OptOrMand)};
check_tags_i(Tags, Buffer, OptOrMand) ->
    check_tags_i(Tags, Buffer, 0, OptOrMand).

check_tags_i([Tag1,Tag2|TagRest], Buffer, Rb, OptOrMand)
  when Tag1#tag.type == 'IMPLICIT' ->
    check_tags_i([Tag1#tag{type=Tag2#tag.type}|TagRest], Buffer, Rb, OptOrMand);

check_tags_i([Tag1|TagRest], Buffer, Rb, OptOrMand) ->
    {Form_Length,Buffer2,Rb1} = check_one_tag(Tag1, Buffer, OptOrMand),
    case TagRest of
	[] -> {TagRest, {Form_Length, Buffer2, Rb + Rb1}};
	_ ->
	    case Form_Length of
		{?CONSTRUCTED,_} ->
		    {TagRest, {Form_Length, Buffer2, Rb + Rb1}};
		_ ->
		    check_tags_i(TagRest, Buffer2, Rb + Rb1, mandatory)
	    end
    end;

check_tags_i([], Buffer, Rb, _) ->
    {[],{{0,0},Buffer,Rb}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This function is called from generated code

check_tags([Tag], Buffer, OptOrMand) -> % optimized very usual case
    check_one_tag(Tag, Buffer, OptOrMand);
check_tags(Tags, Buffer, OptOrMand) ->
    check_tags(Tags, Buffer, 0, OptOrMand).

check_tags([Tag1,Tag2|TagRest], Buffer, Rb, OptOrMand)
  when Tag1#tag.type == 'IMPLICIT' ->
    check_tags([Tag1#tag{type=Tag2#tag.type}|TagRest], Buffer, Rb, OptOrMand);

check_tags([Tag1|TagRest], Buffer, Rb, OptOrMand) ->
    {Form_Length,Buffer2,Rb1} = check_one_tag(Tag1, Buffer, OptOrMand),
    case TagRest of
	[] -> {Form_Length, Buffer2, Rb + Rb1};
	_ -> check_tags(TagRest, Buffer2, Rb + Rb1, mandatory)
    end;

check_tags([], Buffer, Rb, _) ->
    {{0,0},Buffer,Rb}.

check_one_tag(Tag=#tag{class=ExpectedClass,number=ExpectedNumber}, Buffer, OptOrMand) ->
    case catch decode_tag(Buffer) of
	{'EXIT',_Reason} ->
	    tag_error(no_data,Tag,Buffer,OptOrMand);
	{{ExpectedClass,Form,ExpectedNumber},Buffer2,Rb} ->
	    {{L,Buffer3},RemBytes2} = decode_length(Buffer2),
	    {{Form,L}, Buffer3, RemBytes2+Rb};
	{ErrorTag,_,_} ->
	    tag_error(ErrorTag, Tag, Buffer, OptOrMand)
    end.

tag_error(ErrorTag, Tag, Buffer, OptOrMand) ->
    case OptOrMand of
	mandatory ->
	    exit({error,{asn1, {invalid_tag,
				{ErrorTag, Tag, Buffer}}}});
	_ ->
	    exit({error,{asn1, {no_optional_tag,
				{ErrorTag, Tag, Buffer}}}})
    end.
%%=======================================================================
%%
%% Encode all tags in the list Tags and return a possibly deep list of
%% bytes with tag and length encoded
%%
%% prepend_tags(Tags, BytesSoFar, LenSoFar) -> {Bytes, Len}
encode_tags(Tags, BytesSoFar, LenSoFar) ->
    NewTags = encode_tags1(Tags, []),
    %% NewTags contains the resulting tags in reverse order
    encode_tags2(NewTags, BytesSoFar, LenSoFar).

%encode_tags2([#tag{class=?UNIVERSAL,number=No}|Trest], BytesSoFar, LenSoFar) ->
%    {Bytes2,L2} = encode_length(LenSoFar),
%    encode_tags2(Trest,[[No|Bytes2],BytesSoFar], LenSoFar + 1 + L2);
encode_tags2([Tag|Trest], BytesSoFar, LenSoFar) ->
    {Bytes1,L1} = encode_one_tag(Tag),
    {Bytes2,L2} = encode_length(LenSoFar),
    encode_tags2(Trest, [Bytes1,Bytes2|BytesSoFar],
		 LenSoFar + L1 + L2);
encode_tags2([], BytesSoFar, LenSoFar) ->
    {BytesSoFar,LenSoFar}.

encode_tags1([Tag1, Tag2| Trest], Acc) when Tag1#tag.type =:= 'IMPLICIT' ->
    encode_tags1([Tag1#tag{type=Tag2#tag.type,form=Tag2#tag.form}|Trest],Acc);
encode_tags1([Tag1 | Trest], Acc) ->
    encode_tags1(Trest, [Tag1|Acc]);
encode_tags1([], Acc) ->
    Acc. % the resulting tags are returned in reverse order

encode_one_tag(Bin) when is_binary(Bin) ->
    {Bin,byte_size(Bin)};
encode_one_tag(#tag{class=Class,number=No,type=Type, form = Form}) ->
    NewForm = case Type of
	       'EXPLICIT' ->
		   ?CONSTRUCTED;
	       _ ->
		   Form
	   end,
    Bytes = encode_tag_val({Class,NewForm,No}),
    {Bytes,size(Bytes)}.

%%===============================================================================
%% Change the tag (used when an implicit tagged type has a reference to something else)
%% The constructed bit in the tag is taken from the tag to be replaced.
%%
%% change_tag(NewTag,[Tag,Buffer]) -> [NewTag,Buffer]
%%===============================================================================

%change_tag({NewClass,NewTagNr}, Buffer) ->
%    {{OldClass, OldForm, OldTagNo}, Buffer1, RemovedBytes} = decode_tag(lists:flatten(Buffer)),
%    [encode_tag_val({NewClass, OldForm, NewTagNr}) | Buffer1].


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
%% encode_open_type(Value) -> CompleteList
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary

%% This version does not consider Explicit tagging of the open type. It
%% is only left because of backward compatibility.
encode_open_type(Val) when is_list(Val) ->
    {Val, byte_size(list_to_binary(Val))};
encode_open_type(Val) ->
    {Val, byte_size(Val)}.

%%
encode_open_type(Val, []) when is_list(Val) ->
    {Val, byte_size(list_to_binary(Val))};
encode_open_type(Val, []) ->
    {Val, byte_size(Val)};
encode_open_type(Val, Tag) when is_list(Val) ->
    encode_tags(Tag, Val, byte_size(list_to_binary(Val)));
encode_open_type(Val, Tag) ->
    encode_tags(Tag, Val, byte_size(Val)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Buffer) -> Value
%% Bytes = [byte] with BER encoded data
%% Value = [byte] with decoded data (which must be decoded again as some type)
%%
decode_open_type(Bytes) ->
%    {_Tag, Len, _RemainingBuffer, RemovedBytes} = decode_tag_and_length(Bytes),
%    N = Len + RemovedBytes,
    {_Tag, Len, RemainingBuffer, RemovedBytes} = decode_tag_and_length(Bytes),
    {_RemainingBuffer2, RemovedBytes2} = skipvalue(Len, RemainingBuffer, RemovedBytes),
    N = RemovedBytes2,
    <<Val:N/binary, RemainingBytes/binary>> = Bytes,
%    {Val, RemainingBytes, Len + RemovedBytes}.
    {Val,RemainingBytes,N}.

decode_open_type(<<>>,[]=ExplTag) -> % R9C-0.patch-40
    exit({error, {asn1,{no_optional_tag, ExplTag}}});
decode_open_type(Bytes,ExplTag) ->
    {Tag, Len, RemainingBuffer, RemovedBytes} = decode_tag_and_length(Bytes),
    case {Tag,ExplTag} of
% 	{{Class,Form,32},[#tag{class=Class,number=No,form=32}]} ->
% 	    {_Tag2, Len2, RemainingBuffer2, RemovedBytes2} = decode_tag_and_length(RemainingBuffer),
% 	    {_RemainingBuffer3, RemovedBytes3} = skipvalue(Len2, RemainingBuffer2, RemovedBytes2),
% 	    N = RemovedBytes3,
% 	    <<_:RemovedBytes/unit:8,Val:N/binary,RemainingBytes/binary>> = Bytes,
% 	    {Val, RemainingBytes, N + RemovedBytes};
	{{Class,Form,No},[#tag{class=Class,number=No,form=Form}]} ->
	    {_RemainingBuffer2, RemovedBytes2} =
		skipvalue(Len, RemainingBuffer),
	    N = RemovedBytes2,
	    <<_:RemovedBytes/unit:8,Val:N/binary,RemainingBytes/binary>> = Bytes,
	    {Val, RemainingBytes, N + RemovedBytes};
	_ ->
	    {_RemainingBuffer2, RemovedBytes2} =
		skipvalue(Len, RemainingBuffer, RemovedBytes),
	    N = RemovedBytes2,
	    <<Val:N/binary, RemainingBytes/binary>> = Bytes,
	    {Val, RemainingBytes, N}
    end.

decode_open_type(ber_bin,Bytes,ExplTag) ->
    decode_open_type(Bytes,ExplTag);
decode_open_type(ber,Bytes,ExplTag) ->
    {Val,RemBytes,Len}=decode_open_type(Bytes,ExplTag),
    {binary_to_list(Val),RemBytes,Len}.

%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Boolean, ITU_T X.690 Chapter 8.2
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% encode_boolean(Integer, tag | notag) -> [octet list]
%%===============================================================================

encode_boolean({Name, Val}, DoTag) when is_atom(Name) ->
    dotag(DoTag, ?N_BOOLEAN, encode_boolean(Val));
encode_boolean(true,[]) ->
    {[1,1,16#FF],3};
encode_boolean(false,[]) ->
    {[1,1,0],3};
encode_boolean(Val, DoTag) ->
    dotag(DoTag, ?N_BOOLEAN, encode_boolean(Val)).

%% encode_boolean(Boolean) -> [Len, Boolean] = [1, $FF | 0]
encode_boolean(true)   ->    {[16#FF],1};
encode_boolean(false)  ->    {[0],1};
encode_boolean(X) -> exit({error,{asn1, {encode_boolean, X}}}).


%%===============================================================================
%% decode_boolean(BuffList, HasTag, TotalLen) -> {true, Remain, RemovedBytes} |
%%                                               {false, Remain, RemovedBytes}
%%===============================================================================

decode_boolean(Buffer, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_BOOLEAN}),
    decode_boolean_notag(Buffer, NewTags, OptOrMand).

decode_boolean_notag(Buffer, Tags, OptOrMand) ->
    {RestTags, {FormLen,Buffer0,Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),
    case FormLen of
	{?CONSTRUCTED,Len} ->
	    {Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val,Buffer1,Rb1} = decode_boolean_notag(Buffer00, RestTags, OptOrMand),
	    {Buffer2, Rb2} = restbytes2(RestBytes,Buffer1,noext),
	    {Val, Buffer2, Rb0+Rb1+Rb2};
	{_,_} ->
	    decode_boolean2(Buffer0, Rb0)
    end.

decode_boolean2(<<0:8, Buffer/binary>>, RemovedBytes) ->
    {false, Buffer, RemovedBytes + 1};
decode_boolean2(<<_:8, Buffer/binary>>, RemovedBytes) ->
    {true, Buffer, RemovedBytes + 1};
decode_boolean2(Buffer, _) ->
    exit({error,{asn1, {decode_boolean, Buffer}}}).


%%===========================================================================
%% Integer, ITU_T X.690 Chapter 8.3

%% encode_integer(Constraint, Value, Tag) -> [octet list]
%% encode_integer(Constraint, Name, NamedNumberList, Tag) -> [octet list]
%%    Value = INTEGER | {Name,INTEGER}
%%    Tag = tag | notag
%%===========================================================================

encode_integer(C, Val, []) when is_integer(Val) ->
    {EncVal,Len} = encode_integer(C, Val),
    dotag_universal(?N_INTEGER,EncVal,Len);
encode_integer(C, Val, Tag) when is_integer(Val) ->
    dotag(Tag, ?N_INTEGER, encode_integer(C, Val));
encode_integer(C,{Name,Val},Tag) when is_atom(Name) ->
    encode_integer(C,Val,Tag);
encode_integer(_, Val, _) ->
    exit({error,{asn1, {encode_integer, Val}}}).


encode_integer(C, Val, NamedNumberList, Tag) when is_atom(Val) ->
    case lists:keyfind(Val, 1, NamedNumberList) of
	{_, NewVal} ->
	    dotag(Tag, ?N_INTEGER, encode_integer(C, NewVal));
	_ ->
	    exit({error,{asn1, {encode_integer_namednumber, Val}}})
    end;
encode_integer(C,{_,Val},NamedNumberList,Tag) ->
    encode_integer(C,Val,NamedNumberList,Tag);
encode_integer(C, Val, _NamedNumberList, Tag) ->
    dotag(Tag, ?N_INTEGER, encode_integer(C, Val)).


encode_integer(_C, Val) ->
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

decode_integer(Buffer, Range, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_INTEGER}),
    decode_integer_notag(Buffer, Range, [], NewTags, OptOrMand).

decode_integer(Buffer, Range, NamedNumberList, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_INTEGER}),
    decode_integer_notag(Buffer, Range, NamedNumberList, NewTags, OptOrMand).

decode_integer_notag(Buffer, Range, NamedNumberList, NewTags, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(NewTags, Buffer, OptOrMand),
%    Result = {Val, Buffer2, RemovedBytes} =
	case FormLen of
	    {?CONSTRUCTED,Len} ->
		{Buffer00, RestBytes} = split_list(Buffer0,Len),
		{Val01, Buffer01, Rb01} =
		    decode_integer_notag(Buffer00, Range, NamedNumberList,
					 RestTags, OptOrMand),
		{Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
		{Val01, Buffer02, Rb0+Rb01+Rb02};
	    {_, Len} ->
		Result =
		    decode_integer2(Len,Buffer0,Rb0+Len),
		Result2 = check_integer_constraint(Result,Range),
		resolve_named_value(Result2,NamedNumberList)
	end.

resolve_named_value(Result={Val,Buffer,RemBytes},NamedNumberList) ->
    case NamedNumberList of
	[] -> Result;
	_ ->
	    NewVal = case lists:keyfind(Val, 2, NamedNumberList) of
			 {NamedVal, _} ->
			     NamedVal;
			 _ ->
			     Val
		     end,
	    {NewVal, Buffer, RemBytes}
    end.

check_integer_constraint(Result={Val, _Buffer,_},Range) ->
    case Range of
	[] -> % No length constraint
	    Result;
	{Lb,Ub} when Val >= Lb, Ub >= Val -> % variable length constraint
	    Result;
	Val -> % fixed value constraint
	    Result;
	{_,_} ->
	    exit({error,{asn1,{integer_range,Range,Val}}});
	SingleValue when is_integer(SingleValue) ->
	    exit({error,{asn1,{integer_range,Range,Val}}});
	_ -> % some strange constraint that we don't support yet
	    Result
    end.

%%============================================================================
%% Enumerated value, ITU_T X.690 Chapter 8.4

%% encode enumerated value
%%============================================================================
encode_enumerated(Val, []) when is_integer(Val) ->
    {EncVal,Len} = encode_integer(false,Val),
    dotag_universal(?N_ENUMERATED,EncVal,Len);
encode_enumerated(Val, DoTag) when is_integer(Val) ->
    dotag(DoTag, ?N_ENUMERATED, encode_integer(false,Val));
encode_enumerated({Name,Val}, DoTag) when is_atom(Name) ->
    encode_enumerated(Val, DoTag).

%% The encode_enumerated functions below this line can be removed when the
%% new code generation is stable. (the functions might have to be kept here
%% a while longer for compatibility reasons)

encode_enumerated(C, Val, {NamedNumberList,ExtList}, DoTag) when is_atom(Val) ->
    case catch encode_enumerated(C, Val, NamedNumberList, DoTag) of
	{'EXIT',_} -> encode_enumerated(C, Val, ExtList, DoTag);
	Result -> Result
    end;

encode_enumerated(C, Val, NamedNumberList, DoTag) when is_atom(Val) ->
    case lists:keyfind(Val, 1, NamedNumberList) of
	{_, NewVal} when DoTag =:= [] ->
	    {EncVal,Len} = encode_integer(C,NewVal),
	    dotag_universal(?N_ENUMERATED,EncVal,Len);
	{_, NewVal} ->
	    dotag(DoTag, ?N_ENUMERATED, encode_integer(C, NewVal));
	_ ->
	    exit({error,{asn1, {enumerated_not_in_range, Val}}})
    end;

encode_enumerated(C, {asn1_enum, Val}, {_,_}, DoTag) when is_integer(Val) ->
    dotag(DoTag, ?N_ENUMERATED, encode_integer(C,Val));

encode_enumerated(C, {Name,Val}, NamedNumberList, DoTag) when is_atom(Name) ->
    encode_enumerated(C, Val, NamedNumberList, DoTag);

encode_enumerated(_, Val, _, _) ->
    exit({error,{asn1, {enumerated_not_namednumber, Val}}}).



%%============================================================================
%% decode enumerated value
%%   (Buffer, Range, NamedNumberList, HasTag, TotalLen) ->
%%                                    {Value, RemainingBuffer, RemovedBytes}
%%===========================================================================
decode_enumerated(Buffer, Range, NamedNumberList, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_ENUMERATED}),
    decode_enumerated_notag(Buffer, Range, NamedNumberList,
			    NewTags, OptOrMand).

decode_enumerated_notag(Buffer, Range, NNList = {NamedNumberList,ExtList}, Tags, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),

    case FormLen of
	{?CONSTRUCTED,Len} ->
	    {Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} =
		decode_enumerated_notag(Buffer00, Range, NNList, RestTags, OptOrMand),
	    {Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
	    {Val01, Buffer02, Rb0+Rb01+Rb02};
	{_,Len} ->
	    {Val01, Buffer01, Rb01} =
		decode_integer2(Len, Buffer0, Rb0+Len),
	    case decode_enumerated1(Val01, NamedNumberList) of
		{asn1_enum,Val01} ->
		    {decode_enumerated1(Val01,ExtList), Buffer01, Rb01};
		Result01 ->
		    {Result01, Buffer01, Rb01}
	    end
    end;

decode_enumerated_notag(Buffer, Range, NNList, Tags, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),

    case FormLen of
	{?CONSTRUCTED,Len} ->
	    {Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} =
		decode_enumerated_notag(Buffer00, Range, NNList, RestTags, OptOrMand),
	    {Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
	    {Val01, Buffer02, Rb0+Rb01+Rb02};
	{_,Len} ->
	    {Val01, Buffer02, Rb02} =
		decode_integer2(Len, Buffer0, Rb0+Len),
	    case decode_enumerated1(Val01, NNList) of
		{asn1_enum,_} ->
		    exit({error,{asn1, {illegal_enumerated, Val01}}});
		Result01 ->
		    {Result01, Buffer02, Rb02}
	    end
    end.

decode_enumerated1(Val, NamedNumberList) ->
    %% it must be a named integer
    case lists:keyfind(Val, 2, NamedNumberList) of
	{NamedVal, _} ->
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
encode_real(_C,0, DoTag) ->
    dotag(DoTag, ?N_REAL, {[],0});
encode_real(_C,'PLUS-INFINITY', DoTag) ->
    dotag(DoTag, ?N_REAL, {[64],1});
encode_real(_C,'MINUS-INFINITY', DoTag) ->
    dotag(DoTag, ?N_REAL, {[65],1});
encode_real(C,Val, DoTag) when is_tuple(Val); is_list(Val) ->
    dotag(DoTag, ?N_REAL, encode_real(C,Val)).

%%%%%%%%%%%%%%
%% only base 2 encoding!
%% binary encoding:
%% +------------+ +------------+  +-+-+-+-+---+---+
%% | (tag)9     | |  n + p + 1 |  |1|S|BB |FF |EE |
%% +------------+ +------------+  +-+-+-+-+---+---+
%%
%%	 +------------+	   +------------+
%%	 |            |	   |            |
%%	 +------------+	...+------------+
%%	     n octets for exponent
%%
%%	 +------------+	   +------------+
%%	 |            |	   |            |
%%	 +------------+	...+------------+
%%	     p octets for pos mantissa
%%
%% S is 0 for positive sign
%%      1 for negative sign
%% BB: encoding base, 00 = 2, (01 = 8, 10 = 16)
%%                             01 and 10 not used
%% FF: scale factor 00 = 0 (used in base 2 encoding)
%% EE: encoding of the exponent:
%%     00 - on the following octet
%%     01 - on the 2 following octets
%%     10 - on the 3 following octets
%%     11 - encoding of the length of the two's-complement encoding of
%%          exponent on the following octet, and two's-complement 
%%          encoding of exponent on the other octets.
%%
%% In DER and base 2 encoding the mantissa is encoded as value 0 or
%% bit shifted until it is an odd number. Thus, do this for BER as
%% well.
%% This interface also used by RT_COMMON
encode_real(_C,{Mantissa, Base, Exponent}) when Base =:= 2 ->
%%    io:format("Mantissa: ~w Base: ~w, Exp: ~w~n",[Man, Base, Exp]),
    {Man,ExpAdd} = truncate_zeros(Mantissa), %% DER adjustment
    Exp = Exponent + ExpAdd,
    OctExp = if Exp >= 0 -> list_to_binary(encode_integer_pos(Exp, []));
		true     -> list_to_binary(encode_integer_neg(Exp, []))
	     end,
%%    ok = io:format("OctExp: ~w~n",[OctExp]),
    SignBit = if  Man > 0 -> 0;  % bit 7 is pos or neg, no Zeroval
		  true -> 1
	      end,
%%    ok = io:format("SignBitMask: ~w~n",[SignBitMask]),
    SFactor = 0,
    OctExpLen = size(OctExp),
    if OctExpLen > 255 ->
	    exit({error,{asn1, {to_big_exp_in_encode_real, OctExpLen}}});
       true  -> true %% make real assert later..
    end,
    {LenCode, EOctets} = case OctExpLen of   % bit 2,1
			     1 -> {0, OctExp};
			     2 -> {1, OctExp};
			     3 -> {2, OctExp};
			     _ -> {3, <<OctExpLen, OctExp/binary>>}
			 end,
    BB = 0, %% 00 for base 2
    FirstOctet = <<1:1,SignBit:1,BB:2,SFactor:2,LenCode:2>>,
    OctMantissa = if Man > 0 -> list_to_binary(minimum_octets(Man));
		     true    -> list_to_binary(minimum_octets(-(Man))) % signbit keeps track of sign
		  end,
    %%    ok = io:format("LenMask: ~w EOctets: ~w~nFirstOctet: ~w OctMantissa: ~w OctExpLen: ~w~n", [LenMask, EOctets, FirstOctet, OctMantissa, OctExpLen]),
    Bin = <<FirstOctet/binary, EOctets/binary, OctMantissa/binary>>,
    {Bin, size(Bin)};
encode_real(C,{Mantissa,Base,Exponent}) 
  when Base =:= 10, is_integer(Mantissa), is_integer(Exponent) ->
    %% always encode as NR3 due to DER on the format
    %% mmmm.Eseeee where
    %% m := digit
    %% s := '-' | '+' | []
    %% '+' only allowed in +0
    %% e := digit
    %% ex: 1234.E-5679
%%    {Man,AddExp} = truncate_zeros(Mantissa,0),
%%    ManNum = trunc(Mantissa),
%%    {TruncatedMan,NumZeros} = truncate_zeros10(Mantissa),
    ManStr = integer_to_list(Mantissa),
    
    encode_real_as_string(C,ManStr,Exponent);
encode_real(_C,{_,Base,_}) ->
    exit({error,{asn1, {encode_real_non_supported_encodeing, Base}}});
%% base 10
encode_real(C,Real) when is_list(Real) ->
    %% The Real string may come in as a NR1, NR2 or NR3 string.
    {Mantissa, Exponent} =
	case string:tokens(Real,"Ee") of
	    [NR2] ->
		{NR2,0};
	    [NR3MB,NR3E] ->
		%% remove beginning zeros
		{NR3MB,list_to_integer(NR3E)}
	end,
    
    %% .Decimal | Number | Number.Decimal
    ZeroDecimal =
	fun("0") -> "";
	   (L) -> L
	end,
    {NewMantissa,LenDecimal} =
	case Mantissa of
	    [$.|Dec] ->
		NewMan = remove_trailing_zeros(Dec),
		{NewMan,length(ZeroDecimal(NewMan))};
	    _ ->
		case string:tokens(Mantissa,",.") of
		    [Num] -> %% No decimal-mark
			{integer_to_list(list_to_integer(Num)),0};
		    [Num,Dec] ->
			NewDec = ZeroDecimal(remove_trailing_zeros(Dec)),
			NewMan = integer_to_list(list_to_integer(Num)) ++ NewDec,
			{integer_to_list(list_to_integer(NewMan)),
			 length(NewDec)}
		end
	end,
    
%    DER_Exponent = integer_to_list(Exponent - ExpReduce),
    encode_real_as_string(C,NewMantissa,Exponent - LenDecimal).
	
encode_real_as_string(_C,Mantissa,Exponent)
  when is_list(Mantissa), is_integer(Exponent) ->
    %% Remove trailing zeros in Mantissa and add this to Exponent
    TruncMant = remove_trailing_zeros(Mantissa),

    ExpIncr = length(Mantissa) - length(TruncMant),

    ExpStr = integer_to_list(Exponent + ExpIncr),

    ExpBin =
	case ExpStr of
	    "0" ->
		<<"E+0">>;
	    _ -> 
		ExpB = list_to_binary(ExpStr),
		<<$E,ExpB/binary>>
	end,
    ManBin = list_to_binary(TruncMant),
    NR3 = 3,
    {<<NR3,ManBin/binary,$.,ExpBin/binary>>,2 + size(ManBin) + size(ExpBin)}.
    
remove_trailing_zeros(IntStr) ->
    case lists:dropwhile(fun($0)-> true;
			    (_) -> false
			 end, lists:reverse(IntStr)) of
	[] ->
	    "0";
	ReversedIntStr ->
	    lists:reverse(ReversedIntStr)
    end.

truncate_zeros(Num) ->
    truncate_zeros(Num,0).
truncate_zeros(0,Sum) ->
    {0,Sum};
truncate_zeros(M,Sum) ->
    case M band 16#f =:= M band 16#e of
	true -> truncate_zeros(M bsr 1,Sum+1);
	_ -> {M,Sum}
    end.

	    
%%============================================================================
%% decode real value
%%
%% decode_real([OctetBufferList], tuple|value, tag|notag) ->
%%  {{Mantissa, Base, Exp} | realval | PLUS-INFINITY | MINUS-INFINITY | 0,
%%     RestBuff}
%%
%% only for base 2 decoding sofar!!
%%============================================================================

decode_real(Buffer, C, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_REAL}),
    decode_real_notag(Buffer, C, NewTags, OptOrMand).

%% This interface used by RT_COMMON
decode_real(Buffer,Len) ->
    decode_real2(Buffer,[],Len,0).

decode_real_notag(Buffer, C, Tags, OptOrMand) ->
    {_RestTags, {{_,Len}, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),
    decode_real2(Buffer0, C, Len, Rb0).

decode_real2(Buffer, _C, 0, _RemBytes) ->
    {0,Buffer};
decode_real2(Buffer0, _C, Len, RemBytes1) ->
    <<First, Buffer2/binary>> = Buffer0,
    if
	First =:= 2#01000000 -> {'PLUS-INFINITY', Buffer2};
	First =:= 2#01000001 -> {'MINUS-INFINITY', Buffer2};
%%	First =:= 2#00000000 -> {0, Buffer2};
	First =:= 1 orelse First =:= 2 orelse First =:= 3 ->
	    %% charcter string encoding of base 10
	    {NRx,Rest} = split_binary(Buffer2,Len-1),
	    {binary_to_list(NRx),Rest,Len};
	true ->
	    %% have some check here to verify only supported bases (2)
	    %% not base 8 or 16
	    <<_B7:1,Sign:1,BB:2,_FF:2,EE:2>> = <<First>>,
	    Base =
		case BB of
		    0 -> 2;  % base 2, only one so far
		    _ -> exit({error,{asn1, {non_supported_base, BB}}})
		end,
	    {FirstLen, {Exp, Buffer3,_Rb2}, RemBytes2} =
		case EE of
		    0 -> {2, decode_integer2(1, Buffer2, RemBytes1), RemBytes1+1};
		    1 -> {3, decode_integer2(2, Buffer2, RemBytes1), RemBytes1+2};
		    2 -> {4, decode_integer2(3, Buffer2, RemBytes1), RemBytes1+3};
		    3 ->
			<<ExpLen1,RestBuffer/binary>> = Buffer2,
			{ ExpLen1 + 2,
			  decode_integer2(ExpLen1, RestBuffer, RemBytes1),
			  RemBytes1+ExpLen1}
		end,
	    %%	    io:format("FirstLen: ~w, Exp: ~w, Buffer3: ~w ~n",

	    Length = Len - FirstLen,
	    <<LongInt:Length/unit:8,RestBuff/binary>> = Buffer3,
	    {{Mantissa, Buffer4}, RemBytes3} =
		if Sign =:= 0 ->
			%%			io:format("sign plus~n"),
			{{LongInt, RestBuff}, 1 + Length};
		   true ->
			%%			io:format("sign minus~n"),
			{{-LongInt, RestBuff}, 1 + Length}
		end,
	    {{Mantissa, Base, Exp}, Buffer4, RemBytes2+RemBytes3}
    end.


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

encode_bit_string(C,Bin={Unused,BinBits},NamedBitList,DoTag) when is_integer(Unused), is_binary(BinBits) ->
    encode_bin_bit_string(C,Bin,NamedBitList,DoTag);
encode_bit_string(C, [FirstVal | RestVal], NamedBitList, DoTag) when is_atom(FirstVal) ->
    encode_bit_string_named(C, [FirstVal | RestVal], NamedBitList, DoTag);

encode_bit_string(C, [{bit,X} | RestVal], NamedBitList, DoTag) ->
    encode_bit_string_named(C, [{bit,X} | RestVal], NamedBitList, DoTag);

encode_bit_string(C, [FirstVal| RestVal], NamedBitList, DoTag) when is_integer(FirstVal) ->
    encode_bit_string_bits(C, [FirstVal | RestVal], NamedBitList, DoTag);

encode_bit_string(_, 0, _, []) ->
    {[?N_BIT_STRING,1,0],3};

encode_bit_string(_, 0, _, DoTag) ->
    dotag(DoTag, ?N_BIT_STRING, {<<0>>,1});

encode_bit_string(_, [], _, []) ->
    {[?N_BIT_STRING,1,0],3};

encode_bit_string(_, [], _, DoTag) ->
    dotag(DoTag, ?N_BIT_STRING, {<<0>>,1});

encode_bit_string(C, IntegerVal, NamedBitList, DoTag) when is_integer(IntegerVal) ->
    BitListVal = int_to_bitlist(IntegerVal),
    encode_bit_string_bits(C, BitListVal, NamedBitList, DoTag);

encode_bit_string(C, {Name,BitList}, NamedBitList, DoTag) when is_atom(Name) ->
    encode_bit_string(C, BitList, NamedBitList, DoTag).



int_to_bitlist(0) ->
    [];
int_to_bitlist(Int) when is_integer(Int), Int >= 0 ->
    [Int band 1 | int_to_bitlist(Int bsr 1)].


%%=================================================================
%% Encode BIT STRING of the form {Unused,BinBits}.
%% Unused is the number of unused bits in the last byte in BinBits
%% and BinBits is a binary representing the BIT STRING.
%%=================================================================
encode_bin_bit_string(C,{Unused,BinBits},_NamedBitList,DoTag)->
    case get_constraint(C,'SizeConstraint') of
	no ->
	    remove_unused_then_dotag(DoTag,?N_BIT_STRING,Unused,BinBits);
	{_Min,Max} ->
	    BBLen = (size(BinBits)*8)-Unused,
	    if
		BBLen > Max ->
		    exit({error,{asn1,
				 {bitstring_length,
				  {{was,BBLen},{maximum,Max}}}}});
		true ->
		    remove_unused_then_dotag(DoTag,?N_BIT_STRING,
					     Unused,BinBits)
	    end;
	Size ->
	    case ((size(BinBits)*8)-Unused) of
		BBSize when BBSize =< Size ->
		    remove_unused_then_dotag(DoTag,?N_BIT_STRING,
					     Unused,BinBits);
		BBSize  ->
		    exit({error,{asn1,
				 {bitstring_length,
				  {{was,BBSize},{should_be,Size}}}}})
	    end
    end.

remove_unused_then_dotag(DoTag,StringType,Unused,BinBits) ->
    case Unused of
	0 when (byte_size(BinBits) =:= 0), DoTag =:= [] ->
	    %% time optimization of next case
	   {[StringType,1,0],3};
	0 when (byte_size(BinBits) =:= 0) ->
	    dotag(DoTag,StringType,{<<0>>,1});
	0 when DoTag =:= [] -> % time optimization of next case
	    dotag_universal(StringType,[Unused|[BinBits]],size(BinBits)+1);
% 	    {LenEnc,Len} = encode_legth(size(BinBits)+1),
% 	    {[StringType,LenEnc,[Unused|BinBits]],size(BinBits)+1+Len+1};
	0 ->
	    dotag(DoTag,StringType,<<Unused,BinBits/binary>>);
	Num when DoTag =:= [] -> % time optimization of next case
	    N = byte_size(BinBits) - 1,
	    <<BBits:N/binary,LastByte>> = BinBits,
	    dotag_universal(StringType,
			    [Unused,BBits,(LastByte bsr Num) bsl Num],
			    byte_size(BinBits) + 1);
% 	    {LenEnc,Len} = encode_legth(size(BinBits)+1),
% 	    {[StringType,LenEnc,[Unused,BBits,(LastByte bsr Num) bsl Num],
% 	      1+Len+size(BinBits)+1};
	Num ->
	    N = byte_size(BinBits) - 1,
	    <<BBits:N/binary,LastByte>> = BinBits,
	    dotag(DoTag,StringType,{[Unused,binary_to_list(BBits) ++
				     [(LastByte bsr Num) bsl Num]],
				    byte_size(BinBits) + 1})
    end.


%%=================================================================
%% Encode named bits
%%=================================================================

encode_bit_string_named(C, [FirstVal | RestVal], NamedBitList, DoTag) ->
    {Len,Unused,OctetList} =
	case get_constraint(C,'SizeConstraint') of
	    no ->
		ToSetPos = get_all_bitposes([FirstVal | RestVal],
					    NamedBitList, []),
		BitList = make_and_set_list(lists:max(ToSetPos)+1,
					    ToSetPos, 0),
		encode_bitstring(BitList);
	    {_Min,Max} ->
		ToSetPos = get_all_bitposes([FirstVal | RestVal],
					    NamedBitList, []),
		BitList = make_and_set_list(Max, ToSetPos, 0),
		encode_bitstring(BitList);
	    Size ->
		ToSetPos = get_all_bitposes([FirstVal | RestVal],
					    NamedBitList, []),
		BitList = make_and_set_list(Size, ToSetPos, 0),
		encode_bitstring(BitList)
	end,
    case DoTag of
	[] ->
	    dotag_universal(?N_BIT_STRING,[Unused|OctetList],Len+1);
% 	    {EncLen,LenLen} = encode_length(Len+1),
% 	    {[?N_BIT_STRING,EncLen,Unused,OctetList],1+LenLen+Len+1};
	_ ->
	    dotag(DoTag, ?N_BIT_STRING, {[Unused|OctetList],Len+1})
    end.


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
encode_bit_string_bits(C, BitListVal, _NamedBitList, DoTag) when is_list(BitListVal) ->
    {Len,Unused,OctetList} =
	case get_constraint(C,'SizeConstraint') of
	    no ->
		encode_bitstring(BitListVal);
	    Constr={Min,_Max} when is_integer(Min) ->
		encode_constr_bit_str_bits(Constr,BitListVal,DoTag);
	    {Constr={_,_},[]} ->
		%% constraint with extension mark
		encode_constr_bit_str_bits(Constr,BitListVal,DoTag);
	    Constr={{_,_},{_,_}} ->%{{Min1,Max1},{Min2,Max2}}
		%% constraint with extension mark
		encode_constr_bit_str_bits(Constr,BitListVal,DoTag);
	    Size ->
		case length(BitListVal) of
		    BitSize when BitSize =:= Size ->
			encode_bitstring(BitListVal);
		    BitSize when BitSize < Size ->
			PaddedList =
			    pad_bit_list(Size-BitSize,BitListVal),
			encode_bitstring(PaddedList);
		    BitSize ->
			exit({error,
			      {asn1,
			       {bitstring_length,
				{{was,BitSize},
				 {should_be,Size}}}}})
		end
	end,
    %%add unused byte to the Len
    case DoTag of
	[] ->
	    dotag_universal(?N_BIT_STRING,[Unused|OctetList],Len+1);
% 	    {EncLen,LenLen}=encode_length(Len+1),
% 	    {[?N_BIT_STRING,EncLen,Unused|OctetList],1+LenLen+Len+1};
	_ ->
	    dotag(DoTag, ?N_BIT_STRING,
		  {[Unused | OctetList],Len+1})
    end.


encode_constr_bit_str_bits({{_Min1,Max1},{Min2,Max2}},BitListVal,_DoTag) ->
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
	    encode_bitstring(BitListVal)
    end;
encode_constr_bit_str_bits({Min,Max},BitListVal,_DoTag) ->
    BitLen = length(BitListVal),
    if
	BitLen > Max ->
	    exit({error,{asn1,{bitstring_length,{{was,BitLen},
						 {maximum,Max}}}}});
	BitLen < Min ->
	    exit({error,{asn1,{bitstring_length,{{was,BitLen},
						 {minimum,Min}}}}});
	true ->
	    encode_bitstring(BitListVal)
    end.


%% returns a list of length Size + length(BitListVal), with BitListVal
%% as the most significant elements followed by padded zero elements
pad_bit_list(Size,BitListVal) ->
    Tail = lists:duplicate(Size,0),
    BitListVal ++ Tail.

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

decode_compact_bit_string(Buffer, Range, NamedNumberList, Tags, LenIn, OptOrMand) ->
%    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_BIT_STRING}),
     decode_restricted_string(Buffer, Range, ?N_BIT_STRING, Tags, LenIn,
			     NamedNumberList, OptOrMand,bin).

decode_bit_string(Buffer, Range, NamedNumberList, Tags, LenIn, OptOrMand) ->
%    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_BIT_STRING}),
    decode_restricted_string(Buffer, Range, ?N_BIT_STRING, Tags, LenIn,
			     NamedNumberList, OptOrMand,old).


decode_bit_string2(1,<<0 ,Buffer/binary>>,_NamedNumberList,RemovedBytes,BinOrOld) ->
    case BinOrOld of
	bin ->
	    {{0,<<>>},Buffer,RemovedBytes};
	_ ->
	    {[], Buffer, RemovedBytes}
    end;
decode_bit_string2(Len,<<Unused,Buffer/binary>>,NamedNumberList,
		   RemovedBytes,BinOrOld) ->
    L = Len - 1,
    <<Bits:L/binary,BufferTail/binary>> = Buffer,
    case NamedNumberList of
	[] ->
	    case BinOrOld of
		bin ->
		    {{Unused,Bits},BufferTail,RemovedBytes};
		_ ->
		    BitString = decode_bitstring2(L, Unused, Buffer),
		    {BitString,BufferTail, RemovedBytes}
	    end;
	_ ->
	    BitString = decode_bitstring2(L, Unused, Buffer),
	    {decode_bitstring_NNL(BitString,NamedNumberList),
	     BufferTail,
	     RemovedBytes}
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
%% Octet string, ITU_T X.690 Chapter 8.7
%%
%% encode octet string
%% The OctetList must be a flat list of integers in the range 0..255
%% the function does not check this because it takes to much time
%%============================================================================
encode_octet_string(_C, OctetList, []) when is_binary(OctetList) ->
    dotag_universal(?N_OCTET_STRING,OctetList,byte_size(OctetList));
encode_octet_string(_C, OctetList, DoTag) when is_binary(OctetList) ->
    dotag(DoTag, ?N_OCTET_STRING, {OctetList,byte_size(OctetList)});
encode_octet_string(_C, OctetList, DoTag) when is_list(OctetList) ->
    case length(OctetList) of
	Len when DoTag =:= [] ->
	    dotag_universal(?N_OCTET_STRING,OctetList,Len);
	Len ->
	    dotag(DoTag, ?N_OCTET_STRING, {OctetList,Len})
    end;
%% encode_octet_string(C, OctetList, DoTag) when is_list(OctetList) ->
%%     dotag(DoTag, ?N_OCTET_STRING, {OctetList,length(OctetList)});
encode_octet_string(C, {Name,OctetList}, DoTag) when is_atom(Name) ->
    encode_octet_string(C, OctetList, DoTag).


%%============================================================================
%% decode octet string
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}
%%
%% Octet string is decoded as a restricted string
%%============================================================================
decode_octet_string(Buffer, Range, Tags, TotalLen, OptOrMand) ->
%%  NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_OCTET_STRING}),
    decode_restricted_string(Buffer, Range, ?N_OCTET_STRING,
			     Tags, TotalLen, [], OptOrMand,old).

%%============================================================================
%% Null value, ITU_T X.690 Chapter 8.8
%%
%% encode NULL value
%%============================================================================

encode_null(_, []) ->
    {[?N_NULL,0],2};
encode_null(_, DoTag) ->
    dotag(DoTag, ?N_NULL, {[],0}).

%%============================================================================
%% decode NULL value
%%    (Buffer, HasTag, TotalLen) -> {NULL, Remain, RemovedBytes}
%%============================================================================
decode_null(Buffer, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_NULL}),
    decode_null_notag(Buffer, NewTags, OptOrMand).

decode_null_notag(Buffer, Tags, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),

    case FormLen of
	{?CONSTRUCTED,Len} ->
	    {_Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} = decode_null_notag(Buffer0, RestTags,
							OptOrMand),
	    {Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
	    {Val01, Buffer02, Rb0+Rb01+Rb02};
	{_,0} ->
	    {'NULL', Buffer0, Rb0};
	{_,Len} ->
	    exit({error,{asn1,{invalid_length,'NULL',Len}}})
    end.


%%============================================================================
%% Object identifier, ITU_T X.690 Chapter 8.19
%%
%% encode Object Identifier value
%%============================================================================

encode_object_identifier({Name,Val}, DoTag) when is_atom(Name) ->
    encode_object_identifier(Val, DoTag);
encode_object_identifier(Val, []) ->
    {EncVal,Len} = e_object_identifier(Val),
    dotag_universal(?N_OBJECT_IDENTIFIER,EncVal,Len);
encode_object_identifier(Val, DoTag) ->
    dotag(DoTag, ?N_OBJECT_IDENTIFIER, e_object_identifier(Val)).

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

decode_object_identifier(Buffer, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,
				 number=?N_OBJECT_IDENTIFIER}),
    decode_object_identifier_notag(Buffer, NewTags, OptOrMand).

decode_object_identifier_notag(Buffer, Tags, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),

    case FormLen of
	{?CONSTRUCTED,Len} ->
	    {Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} =
		decode_object_identifier_notag(Buffer00,
					       RestTags, OptOrMand),
	    {Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
	    {Val01, Buffer02, Rb0+Rb01+Rb02};
	{_,Len} ->
	    {[AddedObjVal|ObjVals],Buffer01} =
		dec_subidentifiers(Buffer0,0,[],Len),
	    {Val1, Val2} = if
			       AddedObjVal < 40 ->
				   {0, AddedObjVal};
			       AddedObjVal < 80 ->
				   {1, AddedObjVal - 40};
			       true ->
				   {2, AddedObjVal - 80}
			   end,
	    {list_to_tuple([Val1, Val2 | ObjVals]), Buffer01,
	     Rb0+Len}
    end.

dec_subidentifiers(Buffer,_Av,Al,0) ->
    {lists:reverse(Al),Buffer};
dec_subidentifiers(<<1:1,H:7,T/binary>>,Av,Al,Len) ->
    dec_subidentifiers(T,(Av bsl 7) + H,Al,Len-1);
dec_subidentifiers(<<H,T/binary>>,Av,Al,Len) ->
    dec_subidentifiers(T,0,[((Av bsl 7) + H)|Al],Len-1).

%%============================================================================
%% RELATIVE-OID, ITU_T X.690 Chapter 8.20
%%
%% encode Relative Object Identifier
%%============================================================================
encode_relative_oid({Name,Val},TagIn) when is_atom(Name) ->
    encode_relative_oid(Val,TagIn);
encode_relative_oid(Val,TagIn) when is_tuple(Val) ->
    encode_relative_oid(tuple_to_list(Val),TagIn);
encode_relative_oid(Val,[]) ->
    {EncVal,Len} = enc_relative_oid(Val),
    dotag_universal(?'N_RELATIVE-OID',EncVal,Len);
encode_relative_oid(Val, DoTag) ->
    dotag(DoTag, ?'N_RELATIVE-OID', enc_relative_oid(Val)).

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
decode_relative_oid(Buffer, Tags, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,
				 number=?'N_RELATIVE-OID'}),
    decode_relative_oid_notag(Buffer, NewTags, OptOrMand).

decode_relative_oid_notag(Buffer, Tags, OptOrMand) ->
    {_RestTags, {_FormLen={_,Len}, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),
    {ObjVals,Buffer01} =
	dec_subidentifiers(Buffer0,0,[],Len),
    {list_to_tuple(ObjVals), Buffer01, Rb0+Len}.

%%============================================================================
%% Restricted character string types, ITU_T X.690 Chapter 8.21
%%
%% encode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings
%%============================================================================
encode_restricted_string(_C, OctetList, StringType, [])
  when is_binary(OctetList) ->
    dotag_universal(StringType, OctetList, byte_size(OctetList));
encode_restricted_string(_C, OctetList, StringType, DoTag)
  when is_binary(OctetList) ->
    dotag(DoTag, StringType, {OctetList, byte_size(OctetList)});
encode_restricted_string(_C, OctetList, StringType, [])
  when is_list(OctetList) ->
    dotag_universal(StringType, OctetList, length(OctetList));
encode_restricted_string(_C, OctetList, StringType, DoTag)
  when is_list(OctetList) ->
    dotag(DoTag, StringType, {OctetList, length(OctetList)});
encode_restricted_string(C,{Name,OctetL},StringType,DoTag) when is_atom(Name) ->
    encode_restricted_string(C, OctetL, StringType, DoTag).

%%============================================================================
%% decode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings
%%    (Buffer, Range, StringType, HasTag, TotalLen) ->
%%                                  {String, Remain, RemovedBytes}
%%============================================================================

decode_restricted_string(Buffer, Range, StringType, Tags, LenIn, OptOrMand) ->
    {Val,Buffer2,Rb} =
	decode_restricted_string_tag(Buffer, Range, StringType, Tags,
				  LenIn, [], OptOrMand,old),
    {check_and_convert_restricted_string(Val,StringType,Range,[],old),
     Buffer2,Rb}.


decode_restricted_string(Buffer, Range, StringType, Tags, LenIn, NNList, OptOrMand, BinOrOld ) ->
    {Val,Buffer2,Rb} =
	decode_restricted_string_tag(Buffer, Range, StringType, Tags,
			     LenIn, NNList, OptOrMand, BinOrOld),
    {check_and_convert_restricted_string(Val,StringType,Range,NNList,BinOrOld),
     Buffer2,Rb}.

decode_restricted_string_tag(Buffer, Range, StringType, TagsIn, LenIn, NNList, OptOrMand, BinOrOld ) ->
    NewTags = new_tags(TagsIn, #tag{class=?UNIVERSAL,number=StringType}),
    decode_restricted_string_notag(Buffer, Range, StringType, NewTags,
				   LenIn, NNList, OptOrMand, BinOrOld).


check_and_convert_restricted_string(Val,StringType,Range,NamedNumberList,_BinOrOld) ->
    {StrLen,NewVal} = case StringType of
			  ?N_BIT_STRING when NamedNumberList =/= [] ->
			      {no_check,Val};
			  ?N_BIT_STRING when is_list(Val) ->
			      {length(Val),Val};
			  ?N_BIT_STRING when is_tuple(Val) ->
			      {(size(element(2,Val))*8) - element(1,Val),Val};
			  _ when is_binary(Val) ->
			      {byte_size(Val),binary_to_list(Val)};
			  _ when is_list(Val) ->
			      {length(Val), Val}
		      end,
    case Range of
	_ when StrLen =:= no_check ->
	    NewVal;
	[] -> % No length constraint
	    NewVal;
	{Lb,Ub} when StrLen >= Lb, Ub >= StrLen -> % variable length constraint
	    NewVal;
	{{Lb,_Ub},[]} when StrLen >= Lb ->
	    NewVal;
	{{Lb,_Ub},_Ext=[MinExt|_]} when StrLen >= Lb; StrLen >= MinExt ->
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

%%=============================================================================
%% Common routines for several string types including bit string
%% handles indefinite length
%%=============================================================================


decode_restricted_string_notag(Buffer, _Range, StringType, TagsIn,
			       _, NamedNumberList, OptOrMand, BinOrOld) ->
    %%-----------------------------------------------------------
    %% Get inner (the implicit tag or no tag) and
    %%     outer (the explicit tag) lengths.
    %%-----------------------------------------------------------
    {RestTags, {FormLength={_,_Len01}, Buffer0, Rb0}} =
	check_tags_i(TagsIn, Buffer, OptOrMand),

    case FormLength of
	{?CONSTRUCTED,Len} ->
	    {Buffer00, RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} =
		decode_restricted_parts(Buffer00, RestBytes, [], StringType,
					RestTags,
					Len, NamedNumberList,
					OptOrMand,
					BinOrOld, 0, []),
	    {Val01, Buffer01, Rb0+Rb01};
	{_, Len} ->
	    {Val01, Buffer01, Rb01} =
		decode_restricted(Buffer0, Len, StringType,
				  NamedNumberList, BinOrOld),
	    {Val01, Buffer01, Rb0+Rb01}
    end.


decode_restricted_parts(Buffer, RestBytes, [], StringType, RestTags, Len, NNList,
			OptOrMand, BinOrOld, AccRb, AccVal) ->
    DecodeFun = case RestTags of
		    [] -> fun decode_restricted_string_tag/8;
		    _ -> fun decode_restricted_string_notag/8
		end,
    {Val, Buffer1, Rb} =
	DecodeFun(Buffer, [], StringType, RestTags,
		  no_length, NNList,
		  OptOrMand, BinOrOld),
    {Buffer2,More} =
	case Buffer1 of
	    <<0,0,Buffer10/binary>> when Len == indefinite ->
		{Buffer10,false};
	    <<>> ->
		{RestBytes,false};
	    _ ->
		{Buffer1,true}
	end,
    {NewVal, NewRb} =
	case StringType of
	    ?N_BIT_STRING when BinOrOld == bin ->
		{concat_bit_binaries(AccVal, Val), AccRb+Rb};
	    _ when is_binary(Val),is_binary(AccVal) ->
		{<<AccVal/binary,Val/binary>>,AccRb+Rb};
	    _ when is_binary(Val), AccVal =:= [] ->
		{Val,AccRb+Rb};
	    _ ->
		{AccVal++Val, AccRb+Rb}
	end,
    case More of
	false ->
	    {NewVal, Buffer2, NewRb};
	true ->
	    decode_restricted_parts(Buffer2, RestBytes, [], StringType, RestTags, Len, NNList,
				    OptOrMand, BinOrOld, NewRb, NewVal)
    end.



decode_restricted(Buffer, InnerLen, StringType, NamedNumberList,BinOrOld) ->

    case StringType of
	?N_BIT_STRING ->
	    decode_bit_string2(InnerLen,Buffer,NamedNumberList,InnerLen,BinOrOld);

	?N_UniversalString ->
	    <<PreBuff:InnerLen/binary,RestBuff/binary>> = Buffer,%%added for binary
	    UniString = mk_universal_string(binary_to_list(PreBuff)),
	    {UniString,RestBuff,InnerLen};
	?N_BMPString ->
	    <<PreBuff:InnerLen/binary,RestBuff/binary>> = Buffer,%%added for binary
	    BMP = mk_BMP_string(binary_to_list(PreBuff)),
	    {BMP,RestBuff,InnerLen};
	_ ->
	    <<PreBuff:InnerLen/binary,RestBuff/binary>> = Buffer,%%added for binary
	    {PreBuff, RestBuff, InnerLen}
    end.



%%============================================================================
%% encode Universal string
%%============================================================================

encode_universal_string(C, {Name, Universal}, DoTag) when is_atom(Name) ->
    encode_universal_string(C, Universal, DoTag);
encode_universal_string(_C, Universal, []) ->
    OctetList = mk_uni_list(Universal),
    dotag_universal(?N_UniversalString,OctetList,length(OctetList));
encode_universal_string(_C, Universal, DoTag) ->
    OctetList = mk_uni_list(Universal),
    dotag(DoTag, ?N_UniversalString, {OctetList,length(OctetList)}).

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

decode_universal_string(Buffer, Range, Tags, LenIn, OptOrMand) ->
%    NewTags = new_tags(HasTag, #tag{class=?UNIVERSAL,number=?N_UniversalString}),
    decode_restricted_string(Buffer, Range, ?N_UniversalString,
			     Tags, LenIn, [], OptOrMand,old).


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
encode_UTF8_string(_,UTF8String,[]) when is_binary(UTF8String) ->
    dotag_universal(?N_UTF8String,UTF8String,byte_size(UTF8String));
encode_UTF8_string(_,UTF8String,DoTag) when is_binary(UTF8String) ->
    dotag(DoTag,?N_UTF8String,{UTF8String,byte_size(UTF8String)});
encode_UTF8_string(_,UTF8String,[]) ->
    dotag_universal(?N_UTF8String,UTF8String,length(UTF8String));
encode_UTF8_string(_,UTF8String,DoTag) ->
    dotag(DoTag,?N_UTF8String,{UTF8String,length(UTF8String)}).

%%============================================================================
%% decode UTF8 string
%%============================================================================

decode_UTF8_string(Buffer, Tags, OptOrMand) ->
    NewTags = new_tags(Tags, #tag{class=?UNIVERSAL,number=?N_UTF8String}),
    decode_UTF8_string_notag(Buffer, NewTags, OptOrMand).

decode_UTF8_string_notag(Buffer, Tags, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),
    case FormLen of
	{?CONSTRUCTED,Len} ->
	    %% an UTF8String may be encoded as a constructed type
	    {Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} =
		decode_UTF8_string_notag(Buffer00,RestTags,OptOrMand),
	    {Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
	    {Val01, Buffer02, Rb0+Rb01+Rb02};
	{_,Len} ->
	    <<Result:Len/binary,RestBuff/binary>> = Buffer0,
	    {Result,RestBuff,Rb0 + Len}
    end.


%%============================================================================
%% encode BMP string
%%============================================================================

encode_BMP_string(C, {Name,BMPString}, DoTag) when is_atom(Name) ->
    encode_BMP_string(C, BMPString, DoTag);
encode_BMP_string(_C, BMPString, []) ->
    OctetList = mk_BMP_list(BMPString),
    dotag_universal(?N_BMPString,OctetList,length(OctetList));
encode_BMP_string(_C, BMPString, DoTag) ->
    OctetList = mk_BMP_list(BMPString),
    dotag(DoTag, ?N_BMPString, {OctetList,length(OctetList)}).

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
decode_BMP_string(Buffer, Range, Tags, LenIn, OptOrMand) ->
%    NewTags = new_tags(HasTag, #tag{class=?UNIVERSAL,number=?N_BMPString}),
    decode_restricted_string(Buffer, Range, ?N_BMPString,
			     Tags, LenIn, [], OptOrMand,old).

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

encode_generalized_time(C, {Name,OctetList}, DoTag) when is_atom(Name) ->
    encode_generalized_time(C, OctetList, DoTag);
encode_generalized_time(_C, OctetList, []) ->
    dotag_universal(?N_GeneralizedTime,OctetList,length(OctetList));
encode_generalized_time(_C, OctetList, DoTag) ->
    dotag(DoTag, ?N_GeneralizedTime, {OctetList,length(OctetList)}).

%%============================================================================
%% decode Generalized time
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}
%%============================================================================

decode_generalized_time(Buffer, Range, Tags, TotalLen, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,
			 number=?N_GeneralizedTime}),
    decode_generalized_time_notag(Buffer, Range, NewTags, TotalLen, OptOrMand).

decode_generalized_time_notag(Buffer, Range, Tags, TotalLen, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),

    case FormLen of
	{?CONSTRUCTED,Len} ->
	    {Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} =
		decode_generalized_time_notag(Buffer00, Range,
					      RestTags, TotalLen,
					      OptOrMand),
	    {Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
	    {Val01, Buffer02, Rb0+Rb01+Rb02};
	{_,Len} ->
	    <<PreBuff:Len/binary,RestBuff/binary>> = Buffer0,
	    {binary_to_list(PreBuff), RestBuff, Rb0+Len}
    end.

%%============================================================================
%% Universal time, ITU_T X.680 Chapter 40
%%
%% encode UTC time
%%============================================================================

encode_utc_time(C, {Name,OctetList}, DoTag) when is_atom(Name) ->
    encode_utc_time(C, OctetList, DoTag);
encode_utc_time(_C, OctetList, []) ->
    dotag_universal(?N_UTCTime, OctetList,length(OctetList));
encode_utc_time(_C, OctetList, DoTag) ->
    dotag(DoTag, ?N_UTCTime, {OctetList,length(OctetList)}).

%%============================================================================
%% decode UTC time
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}
%%============================================================================

decode_utc_time(Buffer, Range, Tags, TotalLen, OptOrMand) ->
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_UTCTime}),
    decode_utc_time_notag(Buffer, Range, NewTags, TotalLen, OptOrMand).

decode_utc_time_notag(Buffer, Range, Tags, TotalLen, OptOrMand) ->
    {RestTags, {FormLen, Buffer0, Rb0}} =
	check_tags_i(Tags, Buffer, OptOrMand),

    case FormLen of
	{?CONSTRUCTED,Len} ->
	    {Buffer00,RestBytes} = split_list(Buffer0,Len),
	    {Val01, Buffer01, Rb01} =
		decode_utc_time_notag(Buffer00, Range,
				      RestTags, TotalLen,
				      OptOrMand),
	    {Buffer02, Rb02} = restbytes2(RestBytes,Buffer01,noext),
	    {Val01, Buffer02, Rb0+Rb01+Rb02};
	{_,Len} ->
	    <<PreBuff:Len/binary,RestBuff/binary>> = Buffer0,
	    {binary_to_list(PreBuff), RestBuff, Rb0+Len}
    end.


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
    {{indefinite, T}, 1};
decode_length(<<0:1,Length:7,T/binary>>) ->
    {{Length,T},1};
decode_length(<<1:1,LL:7,T/binary>>) ->
    <<Length:LL/unit:8,Rest/binary>> = T,
    {{Length,Rest}, LL+1}.

%decode_length([128 | T]) ->
%    {{indefinite, T},1};
%decode_length([H | T]) when H =< 127 ->
%    {{H, T},1};
%decode_length([H | T]) ->
%    dec_long_length(H band 16#7F, T, 0, 1).


%%dec_long_length(0, Buffer, Acc, Len) ->
%%    {{Acc, Buffer},Len};
%%dec_long_length(Bytes, [H | T], Acc, Len) ->
%%    dec_long_length(Bytes - 1, T, (Acc bsl 8) + H, Len+1).

%%===========================================================================
%% Decode tag and length
%%
%% decode_tag_and_length(Buffer) -> {Tag, Len, RemainingBuffer, RemovedBytes}
%%
%%===========================================================================

decode_tag_and_length(Buffer) ->
    {Tag, Buffer2, RemBytesTag} = decode_tag(Buffer),
    {{Len, Buffer3}, RemBytesLen} = decode_length(Buffer2),
    {Tag, Len, Buffer3, RemBytesTag+RemBytesLen}.


%%============================================================================
%% Check if valid tag
%%
%% check_if_valid_tag(Tag, List_of_valid_tags, OptOrMand) -> name of the tag
%%===============================================================================

check_if_valid_tag(<<0,0,_/binary>>,_,_) ->
    asn1_EOC;
check_if_valid_tag(<<>>, _, OptOrMand) ->
    check_if_valid_tag2(false,[],[],OptOrMand);
check_if_valid_tag(Bytes, ListOfTags, OptOrMand) when is_binary(Bytes) ->
    {Tag, _, _} = decode_tag(Bytes),
    check_if_valid_tag(Tag, ListOfTags, OptOrMand);

%% This alternative should be removed in the near future
%% Bytes as input should be the only necessary call
check_if_valid_tag(Tag, ListOfTags, OptOrMand) ->
    {Class, _Form, TagNo} = Tag,
    C = code_class(Class),
    T = case C of
	    'UNIVERSAL' ->
		code_type(TagNo);
	    _ ->
		TagNo
	end,
    check_if_valid_tag2({C,T}, ListOfTags, Tag, OptOrMand).

check_if_valid_tag2(_Class_TagNo, [], Tag, MandOrOpt) ->
    check_if_valid_tag2_error(Tag,MandOrOpt);

check_if_valid_tag2(Class_TagNo, [{TagName,TagList}|T], Tag, OptOrMand) ->
    case check_if_valid_tag_loop(Class_TagNo, TagList) of
	true ->
	    TagName;
	false ->
	    check_if_valid_tag2(Class_TagNo, T, Tag, OptOrMand)
    end.

-spec(check_if_valid_tag2_error/2 :: (term(),atom()) -> no_return()).

check_if_valid_tag2_error(Tag,mandatory) ->
    exit({error,{asn1,{invalid_tag,Tag}}});
check_if_valid_tag2_error(Tag,_) ->
    exit({error,{asn1,{no_optional_tag,Tag}}}).

check_if_valid_tag_loop(_Class_TagNo,[]) ->
    false;
check_if_valid_tag_loop(Class_TagNo,[H|T]) ->
    %% It is not possible to distinguish between SEQUENCE OF and SEQUENCE, and
    %% between SET OF and SET because both are coded as 16 and 17, respectively.
    H_without_OF = case H of
		       {C, 'SEQUENCE OF'} ->
			   {C, 'SEQUENCE'};
		       {C, 'SET OF'} ->
			   {C, 'SET'};
		       Else ->
			   Else
		   end,

    case H_without_OF of
	Class_TagNo ->
	    true;
	{_,_} ->
	    check_if_valid_tag_loop(Class_TagNo,T);
	_ ->
	    check_if_valid_tag_loop(Class_TagNo,H),
	    check_if_valid_tag_loop(Class_TagNo,T)
    end.



code_class(0) -> 'UNIVERSAL';
code_class(16#40) -> 'APPLICATION';
code_class(16#80) -> 'CONTEXT';
code_class(16#C0) -> 'PRIVATE'.


code_type(1) -> 'BOOLEAN';
code_type(2) -> 'INTEGER';
code_type(3) -> 'BIT STRING';
code_type(4) -> 'OCTET STRING';
code_type(5) -> 'NULL';
code_type(6) -> 'OBJECT IDENTIFIER';
code_type(7) -> 'ObjectDescriptor';
code_type(8) -> 'EXTERNAL';
code_type(9) -> 'REAL';
code_type(10) -> 'ENUMERATED';
code_type(11) -> 'EMBEDDED_PDV';
code_type(16) -> 'SEQUENCE';
% code_type(16) -> 'SEQUENCE OF';
code_type(17) -> 'SET';
% code_type(17) -> 'SET OF';
code_type(18) -> 'NumericString';
code_type(19) -> 'PrintableString';
code_type(20) -> 'TeletexString';
code_type(21) -> 'VideotexString';
code_type(22) -> 'IA5String';
code_type(23) -> 'UTCTime';
code_type(24) -> 'GeneralizedTime';
code_type(25) -> 'GraphicString';
code_type(26) -> 'VisibleString';
code_type(27) -> 'GeneralString';
code_type(28) -> 'UniversalString';
code_type(30) -> 'BMPString';
code_type(Else) -> exit({error,{asn1,{unrecognized_type,Else}}}).

%%-------------------------------------------------------------------------
%% decoding of the components of a SET
%%-------------------------------------------------------------------------

decode_set(Rb, indefinite, <<0,0,Bytes/binary>>, _OptOrMand, _Fun3, Acc) ->
   {lists:reverse(Acc),Bytes,Rb+2};

decode_set(Rb, indefinite, Bytes, OptOrMand, Fun3, Acc) ->
    case Fun3(Bytes, OptOrMand) of
	{_Term, _Remain, 0} ->
	    {lists:reverse(Acc),Bytes,Rb};
	{Term, Remain, Rb1} ->
	    Fun3(Bytes, OptOrMand),
	    decode_set(Rb+Rb1, indefinite, Remain, OptOrMand, Fun3, [Term|Acc])
    end;
%%   {Term, Remain, Rb1} = Fun3(Bytes, OptOrMand),
%%   decode_set(Rb+Rb1, indefinite, Remain, OptOrMand, Fun3, [Term|Acc]);

decode_set(Rb, Num, Bytes, _OptOrMand, _Fun3, Acc) when Num == 0 ->
   {lists:reverse(Acc), Bytes, Rb};

decode_set(_, Num, _, _, _, _) when Num < 0 ->
   exit({error,{asn1,{length_error,'SET'}}});

decode_set(Rb, Num, Bytes, OptOrMand, Fun3, Acc) ->
    case Fun3(Bytes, OptOrMand) of
	{_Term, _Remain, 0} ->
	    {lists:reverse(Acc),Bytes,Rb};
	{Term, Remain, Rb1} ->
	    Fun3(Bytes, OptOrMand),
	    decode_set(Rb+Rb1, Num-Rb1, Remain, OptOrMand, Fun3, [Term|Acc])
    end.
%%    {Term, Remain, Rb1} = Fun3(Bytes, OptOrMand),
%%    decode_set(Rb+Rb1, Num-Rb1, Remain, OptOrMand, Fun3, [Term|Acc]).


%%-------------------------------------------------------------------------
%% decoding of SEQUENCE OF and SET OF
%%-------------------------------------------------------------------------

decode_components(Rb, indefinite, <<0,0,Bytes/binary>>, _Fun3, _TagIn, Acc) ->
   {lists:reverse(Acc),Bytes,Rb+2};

decode_components(Rb, indefinite, Bytes, Fun3, TagIn, Acc) ->
   {Term, Remain, Rb1} = Fun3(Bytes, mandatory, TagIn),
   decode_components(Rb+Rb1, indefinite, Remain, Fun3, TagIn, [Term|Acc]);

decode_components(Rb, Num, Bytes, _Fun3, _TagIn, Acc) when Num == 0 ->
   {lists:reverse(Acc), Bytes, Rb};

decode_components(_, Num, _, _, _, _) when Num < 0 ->
   exit({error,{asn1,{length_error,'SET/SEQUENCE OF'}}});

decode_components(Rb, Num, Bytes, Fun3, TagIn, Acc) ->
   {Term, Remain, Rb1} = Fun3(Bytes, mandatory, TagIn),
   decode_components(Rb+Rb1, Num-Rb1, Remain, Fun3, TagIn, [Term|Acc]).

%%decode_components(Rb, indefinite, [0,0|Bytes], _Fun3, _TagIn, Acc) ->
%%   {lists:reverse(Acc),Bytes,Rb+2};

decode_components(Rb, indefinite, <<0,0,Bytes/binary>>, _Fun4, _TagIn, _Fun, Acc) ->
   {lists:reverse(Acc),Bytes,Rb+2};

decode_components(Rb, indefinite, Bytes, _Fun4, TagIn, _Fun, Acc) ->
   {Term, Remain, Rb1} = _Fun4(Bytes, mandatory, TagIn, _Fun),
   decode_components(Rb+Rb1, indefinite, Remain, _Fun4, TagIn, _Fun, [Term|Acc]);

decode_components(Rb, Num, Bytes, _Fun4, _TagIn, _Fun, Acc) when Num == 0 ->
   {lists:reverse(Acc), Bytes, Rb};

decode_components(_, Num, _, _, _, _, _) when Num < 0 ->
   exit({error,{asn1,{length_error,'SET/SEQUENCE OF'}}});

decode_components(Rb, Num, Bytes, _Fun4, TagIn, _Fun, Acc) ->
   {Term, Remain, Rb1} = _Fun4(Bytes, mandatory, TagIn, _Fun),
   decode_components(Rb+Rb1, Num-Rb1, Remain, _Fun4, TagIn, _Fun, [Term|Acc]).



%%-------------------------------------------------------------------------
%% INTERNAL HELPER FUNCTIONS (not exported)
%%-------------------------------------------------------------------------


%%==========================================================================
%% Encode tag
%%
%% dotag(tag | notag, TagValpattern | TagValTuple, [Length, Value]) -> [Tag]
%% TagValPattern is a correct bitpattern for a tag
%% TagValTuple is a tuple of three bitpatterns, Class, Form and TagNo where
%%     Class = UNIVERSAL | APPLICATION | CONTEXT | PRIVATE
%%     Form  = Primitive | Constructed
%%     TagNo = Number of tag
%%==========================================================================


dotag([], Tag, {Bytes,Len}) ->
    dotag_universal(Tag,Bytes,Len);
dotag(Tags, Tag, {Bytes,Len}) ->
    encode_tags(Tags ++ [#tag{class=?UNIVERSAL,number=Tag,form=?PRIMITIVE}],
		Bytes, Len);

dotag(Tags, Tag, Bytes) ->
    encode_tags(Tags ++ [#tag{class=?UNIVERSAL,number=Tag,form=?PRIMITIVE}],
		Bytes, size(Bytes)).

dotag_universal(UniversalTag,Bytes,Len) when Len =< 16#7F->
    {[UniversalTag,Len,Bytes],2+Len};
dotag_universal(UniversalTag,Bytes,Len) ->
    {EncLen,LenLen}=encode_length(Len),
    {[UniversalTag,EncLen,Bytes],1+LenLen+Len}.

%% decoding postitive integer values.
decode_integer2(Len,Bin = <<0:1,_:7,_Bs/binary>>,RemovedBytes) ->
    <<Int:Len/unit:8,Buffer2/binary>> = Bin,
    {Int,Buffer2,RemovedBytes};
%% decoding negative integer values.
decode_integer2(Len,<<1:1,B2:7,Bs/binary>>,RemovedBytes)  ->
    <<N:Len/unit:8,Buffer2/binary>> = <<B2,Bs/binary>>,
    Int = N - (1 bsl (8 * Len - 1)),
    {Int,Buffer2,RemovedBytes}.

%%decode_integer2(Len,Buffer,Acc,RemovedBytes) when (hd(Buffer) band 16#FF) =< 16#7F ->
%%    {decode_integer_pos(Buffer, 8 * (Len - 1)),skip(Buffer,Len),RemovedBytes};
%%decode_integer2(Len,Buffer,Acc,RemovedBytes)  ->
%%    {decode_integer_neg(Buffer, 8 * (Len - 1)),skip(Buffer,Len),RemovedBytes}.

%%decode_integer_pos([Byte|Tail], Shift) ->
%%    (Byte bsl Shift) bor decode_integer_pos(Tail, Shift-8);
%%decode_integer_pos([], _) -> 0.


%%decode_integer_neg([Byte|Tail], Shift) ->
%%    (-128 + (Byte band 127) bsl Shift) bor decode_integer_pos(Tail, Shift-8).


concat_bit_binaries([],Bin={_,_}) ->
    Bin;
concat_bit_binaries({0,B1},{U2,B2}) ->
    {U2,<<B1/binary,B2/binary>>};
concat_bit_binaries({U1,B1},{U2,B2}) ->
    S1 = (size(B1) * 8) - U1,
    S2 = (size(B2) * 8) - U2,
    PadBits = 8 - ((S1+S2) rem 8),
    {PadBits, <<B1:S1/binary-unit:1,B2:S2/binary-unit:1,0:PadBits>>};
concat_bit_binaries(L1,L2) when is_list(L1), is_list(L2) ->
    %% this case occur when decoding with NNL
    L1 ++ L2.


get_constraint(C,Key) ->
    case lists:keyfind(Key,1,C) of
	false ->
	     no;
	{_, V} ->
	    V
    end.

%%skip(Buffer, 0) ->
%%    Buffer;
%%skip([H | T], Len) ->
%%    skip(T, Len-1).

new_tags([],LastTag) ->
    [LastTag];
new_tags(Tags = [#tag{type='IMPLICIT'}],_LastTag) ->
    Tags;
new_tags([T1 = #tag{type='IMPLICIT'},#tag{type=T2Type}|Rest],LastTag) ->
    new_tags([T1#tag{type=T2Type}|Rest],LastTag);
new_tags(Tags,LastTag) ->
    case lists:last(Tags) of
	#tag{type='IMPLICIT'} ->
	    Tags;
	_ ->
	    Tags ++ [LastTag]
    end.
