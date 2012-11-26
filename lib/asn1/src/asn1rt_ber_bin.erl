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

-export([decode_length/1,
	 encode_real/2, encode_real/3,
	 decode_real/2, decode_real/4,
	 decode_tag/1]).

-include("asn1_records.hrl").

%% the encoding of class of tag bits 8 and 7
-define(UNIVERSAL,   0).

%%% primitive or constructed encoding % bit 6
-define(PRIMITIVE,   0).
-define(CONSTRUCTED, 2#00100000).

%%% The tag-number for universal types
-define(N_REAL, 9).

encode_tag_val({Class, Form, TagNo}) when (TagNo =< 30) ->
    <<(Class bsr 6):2,(Form bsr 5):1,TagNo:5>>;

encode_tag_val({Class, Form, TagNo}) ->
    {Octets,_Len} = mk_object_val(TagNo),
    BinOct = list_to_binary(Octets),
    <<(Class bsr 6):2, (Form bsr 5):1, 31:5,BinOct/binary>>.

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
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This function is called from generated code

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

encode_integer_pos(0, L=[B|_Acc]) when B < 128 ->
    L;
encode_integer_pos(N, Acc) ->
    encode_integer_pos((N bsr 8), [N band 16#ff| Acc]).

encode_integer_neg(-1, L=[B1|_T]) when B1 > 127 ->
    L;
encode_integer_neg(N, Acc) ->
    encode_integer_neg(N bsr 8, [N band 16#ff|Acc]).


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
%% Length handling
%%
%% Encode length
%%
%% encode_length(Int | indefinite) ->
%%          [<127]| [128 + Int (<127),OctetList] | [16#80]
%%============================================================================

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


dotag([], Tag, {Bytes,Len}) ->
    dotag_universal(Tag,Bytes,Len);
dotag(Tags, Tag, {Bytes,Len}) ->
    encode_tags(Tags ++ [#tag{class=?UNIVERSAL,number=Tag,form=?PRIMITIVE}],
		Bytes, Len).

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
