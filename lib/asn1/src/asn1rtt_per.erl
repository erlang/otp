%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
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
-module(asn1rtt_per).

-export([setext/1, fixextensions/2,
	 skipextensions/3, getbit/1, getchoice/3,
	 set_choice/3,encode_integer/2,
	 encode_small_number/1,
	 encode_constrained_number/2,
	 encode_length/1,
	 encode_length/2,
	 encode_bit_string/3,
	 encode_object_identifier/1,
	 encode_relative_oid/1,
	 complete/1,
	 encode_open_type/1,
	 encode_GeneralString/2,
	 encode_GraphicString/2,
	 encode_TeletexString/2,
	 encode_VideotexString/2,
	 encode_ObjectDescriptor/2,
	 encode_UTF8String/1,
	 encode_octet_string/3,
	 encode_known_multiplier_string/4,
	 octets_to_complete/2]).

-define('16K',16384).
-define('32K',32768).
-define('64K',65536).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% setext(true|false) ->  CompleteList
%%

setext(false) ->
    [0];
setext(true) ->
    [1].

fixextensions({ext,ExtPos,ExtNum},Val) ->
    case fixextensions(ExtPos,ExtNum+ExtPos,Val,0) of
	0 -> [];
	ExtBits ->
	    [encode_small_length(ExtNum)|pre_complete_bits(ExtNum,ExtBits)]
    end.

fixextensions(Pos,MaxPos,_,Acc) when Pos >= MaxPos ->
    Acc;
fixextensions(Pos,ExtPos,Val,Acc) ->
    Bit = case catch(element(Pos+1,Val)) of
	      asn1_NOVALUE ->
		  0;
	      asn1_NOEXTVALUE ->
		  0;
	      {'EXIT',_} ->
		  0;
	      _ ->
		  1
	  end,
    fixextensions(Pos+1,ExtPos,Val,(Acc bsl 1)+Bit).

skipextensions(Bytes0, Nr, ExtensionBitstr) when is_bitstring(ExtensionBitstr) ->
    Prev = Nr - 1,
    case ExtensionBitstr of
	<<_:Prev,1:1,_/bitstring>> ->
	    {Len,Bytes1} = decode_length(Bytes0),
	    <<_:Len/binary,Bytes2/bitstring>> = Bytes1,
	    skipextensions(Bytes2, Nr+1, ExtensionBitstr);
	<<_:Prev,0:1,_/bitstring>> ->
	    skipextensions(Bytes0, Nr+1, ExtensionBitstr);
	_ ->
	    Bytes0
    end.


getchoice(Bytes, 1, 0) -> % only 1 alternative is not encoded
    {0,Bytes};
getchoice(Bytes, _, 1) ->
    decode_small_number(Bytes);
getchoice(Bytes, NumChoices, 0) ->
    decode_constrained_number(Bytes, {0,NumChoices-1}).


getbit(Buffer) ->
    <<B:1,Rest/bitstring>> = Buffer,
    {B,Rest}.

getbits(Buffer, Num) when is_bitstring(Buffer) ->
    <<Bs:Num,Rest/bitstring>> = Buffer,
    {Bs,Rest}.

align(Bin) when is_binary(Bin) ->
    Bin;
align(BitStr) when is_bitstring(BitStr) ->
    AlignBits = bit_size(BitStr) rem 8,
    <<_:AlignBits,Rest/binary>> = BitStr,
    Rest.


%% First align buffer, then pick the first Num octets.
%% Returns octets as an integer with bit significance as in buffer.
getoctets(Buffer, Num) when is_binary(Buffer) ->
    <<Val:Num/integer-unit:8,RestBin/binary>> = Buffer,
    {Val,RestBin};
getoctets(Buffer, Num) when is_bitstring(Buffer) ->
    AlignBits = bit_size(Buffer) rem 8,
    <<_:AlignBits,Val:Num/integer-unit:8,RestBin/binary>> = Buffer,
    {Val,RestBin}.


%% First align buffer, then pick the first Num octets.
%% Returns octets as a binary
getoctets_as_bin(Bin,Num) when is_binary(Bin) ->
    <<Octets:Num/binary,RestBin/binary>> = Bin,
    {Octets,RestBin};
getoctets_as_bin(Bin,Num) when is_bitstring(Bin) ->
    AlignBits = bit_size(Bin) rem 8,
    <<_:AlignBits,Val:Num/binary,RestBin/binary>> = Bin,
    {Val,RestBin}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set_choice(Alt,Choices,Altnum) -> ListofBitSettings
%% Alt = atom()
%% Altnum = integer() | {integer(),integer()}% number of alternatives
%% Choices = [atom()] | {[atom()],[atom()]}
%% When Choices is a tuple the first list is the Rootset and the
%% second is the Extensions and then Altnum must also be a tuple with the
%% lengths of the 2 lists
%%
set_choice(Alt,{L1,L2},{Len1,_Len2}) ->
    case set_choice_tag(Alt,L1) of
	N when is_integer(N), Len1 > 1 ->
	    [0,				% the value is in the root set
	     encode_constrained_number({0,Len1-1},N)];
	N when is_integer(N) ->
	    [0];	      % no encoding if only 0 or 1 alternative
	false ->
	    [1,					% extension value
	     case set_choice_tag(Alt, L2) of
		 N2 when is_integer(N2) ->
		     encode_small_number(N2);
		 false ->
		     unknown_choice_alt
	     end]
    end;
set_choice(Alt, L, Len) ->
    case set_choice_tag(Alt, L) of
	N when is_integer(N), Len > 1 ->
	    encode_constrained_number({0,Len-1},N);
	N when is_integer(N) ->
	    [];		      % no encoding if only 0 or 1 alternative
	false ->
	    [unknown_choice_alt]
    end.

set_choice_tag(Alt,Choices) ->
    set_choice_tag(Alt,Choices,0).

set_choice_tag(Alt,[Alt|_Rest],Tag) ->
    Tag;
set_choice_tag(Alt,[_H|Rest],Tag) ->
    set_choice_tag(Alt,Rest,Tag+1);
set_choice_tag(_Alt,[],_Tag) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Constraint, Value) -> CompleteList
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary
%% Contraint = not used in this version
%%
encode_open_type(Val) when is_list(Val) ->
    Bin = list_to_binary(Val),
    case byte_size(Bin) of
	Size when Size > 255 ->
	    [encode_length(Size),21,<<Size:16>>,Bin];
	Size ->
	    [encode_length(Size),20,Size,Bin]
    end;
encode_open_type(Val) when is_binary(Val) ->
    case byte_size(Val) of
	Size when Size > 255 ->
	    [encode_length(Size),21,<<Size:16>>,Val]; % octets implies align
	Size ->
	    [encode_length(Size),20,Size,Val]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_integer(Constraint, Value) -> CompleteList
%%
encode_integer([{Rc,_Ec}],Val) when is_tuple(Rc) ->
    try
	[0|encode_integer([Rc], Val)]
    catch
	_:{error,{asn1,_}} ->
	    [1|encode_unconstrained_number(Val)]
    end;
encode_integer([], Val) ->
    encode_unconstrained_number(Val);
%% The constraint is the effective constraint, and in this case is a number
encode_integer([{'SingleValue',V}], V) ->
    [];
encode_integer([{'ValueRange',{Lb,Ub}=VR,Range,PreEnc}],Val)
  when Val >= Lb, Ub >= Val ->
    %% this case when NamedNumberList
    encode_constrained_number(VR, Range, PreEnc, Val);
encode_integer([{'ValueRange',{Lb,'MAX'}}], Val) ->
    encode_semi_constrained_number(Lb, Val);
encode_integer([{'ValueRange',{'MIN',_}}], Val) ->
    encode_unconstrained_number(Val);
encode_integer([{'ValueRange',VR={_Lb,_Ub}}], Val) ->
    encode_constrained_number(VR, Val);
encode_integer(_,Val) ->
    exit({error,{asn1,{illegal_value,Val}}}).


%% X.691:10.6 Encoding of a normally small non-negative whole number
%% Use this for encoding of CHOICE index if there is an extension marker in
%% the CHOICE
encode_small_number(Val) when Val < 64 ->
    [10,7,Val];
encode_small_number(Val) ->
    [1|encode_semi_constrained_number(0, Val)].

decode_small_number(Bytes) ->
    {Bit,Bytes2} = getbit(Bytes),
    case Bit of
	0 ->
	    getbits(Bytes2, 6);
	1 ->
	    decode_semi_constrained_number(Bytes2)
    end.

%% X.691:10.7 Encoding of a semi-constrained whole number
encode_semi_constrained_number(Lb, Val) ->
    Val2 = Val - Lb,
    Oct = eint_positive(Val2),
    Len = length(Oct),
    if
	Len < 128 ->
	    [20,Len+1,Len|Oct];
	Len < 256 ->
	    [encode_length(Len),20,Len|Oct];
	true ->
	    [encode_length(Len),21,<<Len:16>>|Oct]
    end.

decode_semi_constrained_number(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes),
    getoctets(Bytes2, Len).

encode_constrained_number({Lb,_Ub},_Range,{bits,N},Val) ->
    Val2 = Val-Lb,
    [10,N,Val2];
encode_constrained_number({Lb,_Ub},_Range,{octets,N},Val) when N < 256->
    %% N is 8 or 16 (1 or 2 octets)
    Val2 = Val-Lb,
    [20,N,Val2];
encode_constrained_number({Lb,_Ub},_Range,{octets,N},Val) -> % N>255
    %% N is 8 or 16 (1 or 2 octets)
    Val2 = Val-Lb,
    [21,<<N:16>>,Val2];
encode_constrained_number({Lb,_Ub},Range,_,Val) ->
    Val2 = Val-Lb,
    if
	Range =< 16#1000000  -> % max 3 octets
	    Octs = eint_positive(Val2),
	    L = length(Octs),
	    [encode_length({1,3},L),[20,L,Octs]];
	Range =< 16#100000000  -> % max 4 octets
	    Octs = eint_positive(Val2),
	    L = length(Octs),
	    [encode_length({1,4},L),[20,L,Octs]];
	Range =< 16#10000000000  -> % max 5 octets
	    Octs = eint_positive(Val2),
	    L = length(Octs),
	    [encode_length({1,5},L),[20,L,Octs]];
	true  ->
	    exit({not_supported,{integer_range,Range}})
    end.

encode_constrained_number({Lb,Ub}, Val) when Val >= Lb, Ub >= Val ->
    Range = Ub - Lb + 1,
    Val2 = Val - Lb,
    if
	Range  == 1 -> [];
	Range  == 2 ->
	    [Val2];
	Range  =< 4 ->
	    [10,2,Val2];
	Range  =< 8 ->
	    [10,3,Val2];
	Range  =< 16 ->
	    [10,4,Val2];
	Range  =< 32 ->
	    [10,5,Val2];
	Range  =< 64 ->
	    [10,6,Val2];
	Range  =< 128 ->
	    [10,7,Val2];
	Range  =< 255 ->
	    [10,8,Val2];
	Range  =< 256 ->
	    [20,1,Val2];
	Range  =< 65536 ->
	    [20,2,<<Val2:16>>];
	Range =< (1 bsl (255*8))  ->
            Octs = binary:encode_unsigned(Val2),
            RangeOcts = binary:encode_unsigned(Range - 1),
            OctsLen = byte_size(Octs),
            RangeOctsLen = byte_size(RangeOcts),
            LengthBitsNeeded = minimum_bits(RangeOctsLen - 1),
            [10,LengthBitsNeeded,OctsLen-1,20,OctsLen,Octs];
	true  ->
	    exit({not_supported,{integer_range,Range}})
    end;
encode_constrained_number({_,_},Val) ->
    exit({error,{asn1,{illegal_value,Val}}}).

decode_constrained_number(Buffer,VR={Lb,Ub}) ->
    Range = Ub - Lb + 1,
    decode_constrained_number(Buffer,VR,Range).

decode_constrained_number(Buffer,{Lb,_Ub},Range) ->
						%    Val2 = Val - Lb,
    {Val,Remain} =
	if
	    Range  == 1 ->
		{0,Buffer};
	    Range  == 2 ->
		getbits(Buffer,1);
	    Range  =< 4 ->
		getbits(Buffer,2);
	    Range  =< 8 ->
		getbits(Buffer,3);
	    Range  =< 16 ->
		getbits(Buffer,4);
	    Range  =< 32 ->
		getbits(Buffer,5);
	    Range  =< 64 ->
		getbits(Buffer,6);
	    Range  =< 128 ->
		getbits(Buffer,7);
	    Range  =< 255 ->
		getbits(Buffer,8);
	    Range  =< 256 ->
		getoctets(Buffer,1);
	    Range  =< 65536 ->
		getoctets(Buffer,2);
            Range =< (1 bsl (255*8))  ->
                OList = binary:bin_to_list(binary:encode_unsigned(Range - 1)),
                RangeOctLen = length(OList),
                {Len, Bytes} = decode_length(Buffer, {1, RangeOctLen}),
                {Octs, RestBytes} = getoctets_as_bin(Bytes, Len),
                {binary:decode_unsigned(Octs), RestBytes};
	    true  ->
		exit({not_supported,{integer_range,Range}})
	end,
    {Val+Lb,Remain}.

%% For some reason the minimum bits needed in the length field in
%% the encoding of constrained whole numbers must always be at least 2?
minimum_bits(N) when N < 4 -> 2;
minimum_bits(N) when N < 8 -> 3;
minimum_bits(N) when N < 16 -> 4;
minimum_bits(N) when N < 32 -> 5;
minimum_bits(N) when N < 64 -> 6;
minimum_bits(N) when N < 128 -> 7;
minimum_bits(_N) -> 8.

%% X.691:10.8 Encoding of an unconstrained whole number

encode_unconstrained_number(Val) ->
    Oct = if
	      Val >= 0 ->
		  eint(Val, []);
	      true ->
		  enint(Val, [])
	  end,
    Len = length(Oct),
    if
        Len < 128 ->
            [20,Len + 1,Len|Oct];
        Len < 256 ->
            [20,Len + 2,<<2:2,Len:14>>|Oct];
        true ->
            [encode_length(Len),21,<<Len:16>>|Oct]
    end.

%% used for positive Values which don't need a sign bit
%% returns a list
eint_positive(Val) ->
    case eint(Val,[]) of
	[0,B1|T] ->
	    [B1|T];
	T ->
	    T
    end.


eint(0, [B|Acc]) when B < 128 ->
    [B|Acc];
eint(N, Acc) ->
    eint(N bsr 8, [N band 16#ff| Acc]).

enint(-1, [B1|T]) when B1 > 127 ->
    [B1|T];
enint(N, Acc) ->
    enint(N bsr 8, [N band 16#ff|Acc]).

%% X.691:10.9 Encoding of a length determinant
%%encode_small_length(undefined,Len) -> % null means no UpperBound
%%    encode_small_number(Len).

%% X.691:10.9.3.5
%% X.691:10.9.3.7
encode_length(Len) ->				% unconstrained
    if
	Len < 128 ->
	    [20,1,Len];
	Len < 16384 ->
	    <<20,2,2:2,Len:14>>;
	true  -> % should be able to endode length >= 16384 i.e. fragmented length
	    exit({error,{asn1,{encode_length,{nyi,above_16k}}}})
    end.

encode_length(undefined, Len) -> % un-constrained
    encode_length(Len);
encode_length({0,'MAX'},Len) ->
    encode_length(undefined,Len);
encode_length({Lb,Ub}=Vr, Len) when Ub =< 65535 ,Lb >= 0 -> % constrained
    encode_constrained_number(Vr,Len);
encode_length({Lb,_Ub}, Len) when is_integer(Lb), Lb >= 0 -> % Ub > 65535
    encode_length(Len);
encode_length({{Lb,Ub}=Vr,Ext}, Len)
  when Ub =< 65535 ,Lb >= 0,Len=<Ub, is_list(Ext) ->
    %% constrained extensible
    [0|encode_constrained_number(Vr,Len)];
encode_length({{Lb,_},Ext},Len) when is_list(Ext) ->
    [1|encode_semi_constrained_number(Lb, Len)];
encode_length(SingleValue, _Len) when is_integer(SingleValue) ->
    [].

%% X.691 10.9.3.4 (only used for length of bitmap that prefixes extension
%% additions in a sequence or set
encode_small_length(Len) when Len =< 64 ->
    [10,7,Len-1];
encode_small_length(Len) ->
    [1,encode_length(Len)].


decode_length(Buffer)  -> % un-constrained
    case align(Buffer) of
	<<0:1,Oct:7,Rest/binary>> ->
	    {Oct,Rest};
	<<2:2,Val:14,Rest/binary>> ->
	    {Val,Rest};
	<<3:2,_Val:14,_Rest/binary>> ->
	    %% this case should be fixed
	    exit({error,{asn1,{decode_length,{nyi,above_16k}}}})
    end.

decode_length(Buffer, {Lb,Ub}) when Ub =< 65535, Lb >= 0 -> % constrained
    decode_constrained_number(Buffer, {Lb,Ub});
decode_length(Buffer, {Lb,_Ub}) when is_integer(Lb), Lb >= 0 -> % Ub > 65535
    decode_length(Buffer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bitstring NamedBitList
%% Val can be  of:
%% - [identifiers] where only named identifers are set to one,
%%   the Constraint must then have some information of the
%%   bitlength.
%% - [list of ones and zeroes] all bits
%% - integer value representing the bitlist
%% C is constraint Len, only valid when identifiers


%% when the value is a list of {Unused,BinBits}, where
%% Unused = integer(),
%% BinBits = binary().

encode_bit_string(C, Bits, NamedBitList) when is_bitstring(Bits) ->
    PadLen = (8 - (bit_size(Bits) band 7)) band 7,
    Compact = {PadLen,<<Bits/bitstring,0:PadLen>>},
    encode_bin_bit_string(C, Compact, NamedBitList);
encode_bit_string(C, {Unused,BinBits}=Bin, NamedBitList)
  when is_integer(Unused), is_binary(BinBits) ->
    encode_bin_bit_string(C,Bin,NamedBitList);

%% when the value is a list of named bits

encode_bit_string(C, LoNB=[FirstVal | _RestVal], NamedBitList) when is_atom(FirstVal) ->
    ToSetPos = get_all_bitposes(LoNB, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);% consider the constraint

encode_bit_string(C, BL=[{bit,_} | _RestVal], NamedBitList) ->
    ToSetPos = get_all_bitposes(BL, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);

%% when the value is a list of ones and zeroes
encode_bit_string(Int, BitListValue, _)
  when is_list(BitListValue),is_integer(Int),Int =< 16 ->
    %% The type is constrained by a single value size constraint
    %% range_check(Int,length(BitListValue)),
    [40,Int,length(BitListValue),BitListValue];
encode_bit_string(Int, BitListValue, _)
  when is_list(BitListValue),is_integer(Int), Int =< 255 ->
    %% The type is constrained by a single value size constraint
    %% range_check(Int,length(BitListValue)),
    [2,40,Int,length(BitListValue),BitListValue];
encode_bit_string(Int, BitListValue, _)
  when is_list(BitListValue),is_integer(Int), Int < ?'64K' ->
    {Code,DesiredLength,Length} =
	case length(BitListValue) of
	    B1 when B1 > Int ->
		exit({error,{'BIT_STRING_length_greater_than_SIZE',
			     Int,BitListValue}});
	    B1 when B1 =< 255,Int =< 255 ->
		{40,Int,B1};
	    B1 when B1 =< 255 ->
		{42,<<Int:16>>,B1};
	    B1 ->
		{43,<<Int:16>>,<<B1:16>>}
	end,
    %% The type is constrained by a single value size constraint
    [2,Code,DesiredLength,Length,BitListValue];
encode_bit_string(no, BitListValue,[])
  when is_list(BitListValue) ->
    [encode_length(length(BitListValue)),
     2|BitListValue];
encode_bit_string({{Fix,Fix},Ext}, BitListValue,[])
  when is_integer(Fix), is_list(Ext) ->
    case length(BitListValue) of
	Len when Len =< Fix ->
	    [0|encode_bit_string(Fix, BitListValue, [])];
	_ ->
	    [1|encode_bit_string(no, BitListValue, [])]
    end;
encode_bit_string(C, BitListValue,[])
  when is_list(BitListValue) ->
    [encode_length(C, length(BitListValue)),
     2|BitListValue];
encode_bit_string(no, BitListValue,_NamedBitList)
  when is_list(BitListValue) ->
    %% this case with an unconstrained BIT STRING can be made more efficient
    %% if the complete driver can take a special code so the length field
    %% is encoded there.
    NewBitLVal = lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,
					    lists:reverse(BitListValue))),
    [encode_length(length(NewBitLVal)),2|NewBitLVal];
encode_bit_string({{Fix,Fix},Ext}, BitListValue, NamedBitList)
  when is_integer(Fix), is_list(Ext) ->
    case length(BitListValue) of
	Len when Len =< Fix ->
	    [0|encode_bit_string(Fix, BitListValue, NamedBitList)];
	_ ->
	    [1|encode_bit_string(no, BitListValue, NamedBitList)]
    end;
encode_bit_string(C, BitListValue, _NamedBitList)
  when is_list(BitListValue) ->			% C = {_,'MAX'}
    NewBitLVal = bit_string_trailing_zeros(BitListValue, C),
    [encode_length(C, length(NewBitLVal)),2|NewBitLVal];


%% when the value is an integer
encode_bit_string(C, IntegerVal, NamedBitList) when is_integer(IntegerVal)->
    BitList = int_to_bitlist(IntegerVal),
    encode_bit_string(C,BitList,NamedBitList).

bit_string_trailing_zeros(BitList,C) when is_integer(C) ->
    bit_string_trailing_zeros1(BitList,C,C);
bit_string_trailing_zeros(BitList,{Lb,Ub}) when is_integer(Lb) ->
    bit_string_trailing_zeros1(BitList,Lb,Ub);
bit_string_trailing_zeros(BitList,{{Lb,Ub},_}) when is_integer(Lb) ->
    bit_string_trailing_zeros1(BitList,Lb,Ub);
bit_string_trailing_zeros(BitList,_) ->
    BitList.

bit_string_trailing_zeros1(BitList,Lb,Ub) ->
    case length(BitList) of
	Lb -> BitList;
	B when B < Lb -> BitList++lists:duplicate(Lb-B, 0);
	D -> F = fun(L,LB,LB,_,_)->lists:reverse(L);
		    ([0|R],L1,LB,UB,Fun)->Fun(R,L1-1,LB,UB,Fun);
		    (L,L1,_,UB,_)when L1 =< UB -> lists:reverse(L);
		    (_,_L1,_,_,_) ->exit({error,{list_length_BIT_STRING,
						 BitList}}) end,
	     F(lists:reverse(BitList),D,Lb,Ub,F)
    end.

%% encode_bin_bit_string/3, when value is a tuple of Unused and BinBits.
%% Unused = integer(),i.e. number unused bits in least sign. byte of
%% BinBits = binary().
encode_bin_bit_string(C, {Unused,BinBits}, _NamedBitList)
  when is_integer(C),C=<16 ->
    range_check(C, bit_size(BinBits) - Unused),
    [45,C,byte_size(BinBits),BinBits];
encode_bin_bit_string(C, {Unused,BinBits}, _NamedBitList)
  when is_integer(C), C =< 255 ->
    range_check(C, bit_size(BinBits) - Unused),
    [2,45,C,byte_size(BinBits),BinBits];
encode_bin_bit_string(C, {Unused,BinBits}, _NamedBitList)
  when is_integer(C), C =< 65535 ->
    range_check(C, bit_size(BinBits) - Unused),
    case byte_size(BinBits) of
	Size when Size =< 255 ->
	    [2,46,<<C:16>>,Size,BinBits];
	Size ->
	    [2,47,<<C:16>>,<<Size:16>>,BinBits]
    end;
encode_bin_bit_string(C,UnusedAndBin={_,_},NamedBitList) ->
    {Unused1,Bin1} =
	%% removes all trailing bits if NamedBitList is not empty
	remove_trailing_bin(NamedBitList,UnusedAndBin),
    case C of
	{Lb,Ub} when is_integer(Lb),is_integer(Ub) ->
	    Size = byte_size(Bin1),
	    [encode_length({Lb,Ub}, Size*8 - Unused1),
	     2,octets_unused_to_complete(Unused1,Size,Bin1)];
	no ->
	    Size = byte_size(Bin1),
	    [encode_length(Size*8 - Unused1),
	     2|octets_unused_to_complete(Unused1, Size, Bin1)];
	{{Fix,Fix},Ext} when is_integer(Fix),is_list(Ext) ->
	    case byte_size(Bin1)*8 - Unused1 of
		Size when Size =< Fix  ->
		    [0|encode_bin_bit_string(Fix,UnusedAndBin,NamedBitList)];
		_Size ->
		    [1|encode_bin_bit_string(no,UnusedAndBin,NamedBitList)]
	    end;
	Sc ->
	    Size = byte_size(Bin1),
	    [encode_length(Sc, Size*8 - Unused1),
	     2|octets_unused_to_complete(Unused1,Size,Bin1)]
    end.

range_check(C,C) when is_integer(C) ->
    ok;
range_check(C1,C2) when is_integer(C1) ->
    exit({error,{asn1,{bit_string_out_of_range,{C1,C2}}}}).

remove_trailing_bin([], {Unused,Bin}) ->
    {Unused,Bin};
remove_trailing_bin(_NamedNumberList,{_Unused,<<>>}) ->
    {0,<<>>};
remove_trailing_bin(NamedNumberList, {_Unused,Bin}) ->
    Size = byte_size(Bin)-1,
    <<Bfront:Size/binary, LastByte:8>> = Bin,
    %% clear the Unused bits to be sure
    Unused1 = trailingZeroesInNibble(LastByte band 15),
    Unused2 =
	case Unused1 of
	    4 ->
		4 + trailingZeroesInNibble(LastByte bsr 4);
	    _ -> Unused1
	end,
    case Unused2 of
	8 ->
	    remove_trailing_bin(NamedNumberList,{0,Bfront});
	_ ->
	    {Unused2,Bin}
    end.


trailingZeroesInNibble(0) ->
    4;
trailingZeroesInNibble(1) ->
    0;
trailingZeroesInNibble(2) ->
    1;
trailingZeroesInNibble(3) ->
    0;
trailingZeroesInNibble(4) ->
    2;
trailingZeroesInNibble(5) ->
    0;
trailingZeroesInNibble(6) ->
    1;
trailingZeroesInNibble(7) ->
    0;
trailingZeroesInNibble(8) ->
    3;
trailingZeroesInNibble(9) ->
    0;
trailingZeroesInNibble(10) ->
    1;
trailingZeroesInNibble(11) ->
    0;
trailingZeroesInNibble(12) -> %#1100
    2;
trailingZeroesInNibble(13) ->
    0;
trailingZeroesInNibble(14) ->
    1;
trailingZeroesInNibble(15) ->
    0.


%%%%%%%%%%%%%%%
%%

int_to_bitlist(Int) when is_integer(Int), Int > 0 ->
    [Int band 1 | int_to_bitlist(Int bsr 1)];
int_to_bitlist(0) ->
    [].


%%%%%%%%%%%%%%%%%%
%% get_all_bitposes([list of named bits to set], named_bit_db, []) ->
%%   [sorted_list_of_bitpositions_to_set]

get_all_bitposes([{bit,ValPos}|Rest], NamedBitList, Ack) ->
    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack ]);

get_all_bitposes([Val | Rest], NamedBitList, Ack) ->
    case lists:keyfind(Val, 1, NamedBitList) of
	{_ValName, ValPos} ->
	    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]);
	false ->
	    exit({error,{asn1, {bitstring_namedbit, Val}}})
    end;
get_all_bitposes([], _NamedBitList, Ack) ->
    lists:sort(Ack).

%%%%%%%%%%%%%%%%%%
%% make_and_set_list([list of positions to set to 1])->
%% returns list with all in SetPos set.
%% in positioning in list the first element is 0, the second 1 etc.., but
%%

make_and_set_list([XPos|SetPos], XPos) ->
    [1 | make_and_set_list(SetPos, XPos + 1)];
make_and_set_list([Pos|SetPos], XPos) ->
    [0 | make_and_set_list([Pos | SetPos], XPos + 1)];
make_and_set_list([], _) ->
    [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% X.691:16
%% encode_octet_string(Constraint,ExtensionMarker,Val)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_octet_string(_C, true, _Val) ->
    exit({error,{asn1,{'not_supported',extensionmarker}}});
encode_octet_string({_,_}=SZ, false, Val) ->
    Len = length(Val),
    try
	[encode_length(SZ, Len),2|octets_to_complete(Len, Val)]
    catch
	exit:{error,{asn1,{encode_length,_}}} ->
	    encode_fragmented_octet_string(Val)
    end;
encode_octet_string(SZ, false, Val) when is_list(SZ) ->
    Len = length(Val),
    try
	[encode_length({hd(SZ),lists:max(SZ)},Len),2|
	 octets_to_complete(Len,Val)]
    catch
	exit:{error,{asn1,{encode_length,_}}} ->
	    encode_fragmented_octet_string(Val)
    end;
encode_octet_string(Sv, false, Val) when is_integer(Sv) ->
    encode_fragmented_octet_string(Val);
encode_octet_string(no, false, Val) ->
    Len = length(Val),
    try
	[encode_length(Len),2|octets_to_complete(Len, Val)]
    catch
	exit:{error,{asn1,{encode_length,_}}} ->
	    encode_fragmented_octet_string(Val)
    end;
encode_octet_string(C, _, _) ->
    exit({error,{not_implemented,C}}).

encode_fragmented_octet_string(Val) ->
    Bin = iolist_to_binary(Val),
    efos_1(Bin).

efos_1(<<B1:16#C000/binary,B2:16#4000/binary,T/binary>>) ->
    [20,1,<<3:2,4:6>>,
     octets_to_complete(16#C000, B1),
     octets_to_complete(16#4000, B2)|efos_1(T)];
efos_1(<<B:16#C000/binary,T/binary>>) ->
    [20,1,<<3:2,3:6>>,octets_to_complete(16#C000, B)|efos_1(T)];
efos_1(<<B:16#8000/binary,T/binary>>) ->
    [20,1,<<3:2,2:6>>,octets_to_complete(16#8000, B)|efos_1(T)];
efos_1(<<B:16#4000/binary,T/binary>>) ->
    [20,1,<<3:2,1:6>>,octets_to_complete(16#4000, B)|efos_1(T)];
efos_1(<<>>) ->
    [20,1,0];
efos_1(<<B/bitstring>>) ->
    Len = byte_size(B),
    [encode_length(Len)|octets_to_complete(Len, B)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Restricted char string types
%% (NumericString, PrintableString,VisibleString,IA5String,BMPString,UniversalString)
%% X.691:26 and X.680:34-36

encode_restricted_string(Val) when is_list(Val)->
    Len = length(Val),
    [encode_length(Len)|octets_to_complete(Len, Val)].

encode_known_multiplier_string(SizeC, NumBits, CharOutTab, Val) ->
    Result = chars_encode2(Val, NumBits, CharOutTab),
    case SizeC of
	Ub when is_integer(Ub), Ub*NumBits =< 16  ->
	    Result;
	Ub when is_integer(Ub), Ub =<65535 -> % fixed length
	    [2,Result];
	{Ub,Lb} ->
	    [encode_length({Ub,Lb},length(Val)),2,Result];
	no  ->
	    [encode_length(length(Val)),2,Result]
    end.

encode_GeneralString(_C,Val) ->
    encode_restricted_string(Val).

encode_GraphicString(_C,Val) ->
    encode_restricted_string(Val).

encode_ObjectDescriptor(_C,Val) ->
    encode_restricted_string(Val).

encode_TeletexString(_C,Val) -> % equivalent with T61String
    encode_restricted_string(Val).

encode_VideotexString(_C,Val) ->
    encode_restricted_string(Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% chars_encode(C,StringType,Value) -> ValueList
%%
%% encodes chars according to the per rules taking the constraint
%% PermittedAlphabet into account.
%%
%% This function only encodes the value part and NOT the length.

chars_encode2([H|T],NumBits,T1={Min,Max,notab}) when  H =< Max, H >= Min ->
    [pre_complete_bits(NumBits,H-Min)|chars_encode2(T,NumBits,T1)];
chars_encode2([H|T],NumBits,T1={Min,Max,Tab}) when H =< Max, H >= Min ->
    [pre_complete_bits(NumBits,exit_if_false(H,element(H-Min+1,Tab)))|
     chars_encode2(T,NumBits,T1)];
chars_encode2([{A,B,C,D}|T],NumBits,T1={Min,_Max,notab}) ->
    %% no value range check here (ought to be, but very expensive)
    [pre_complete_bits(NumBits,
			       ((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min)|
     chars_encode2(T,NumBits,T1)];
chars_encode2([H={A,B,C,D}|T],NumBits,{Min,Max,Tab}) ->
    %% no value range check here (ought to be, but very expensive)
    [pre_complete_bits(NumBits,exit_if_false(H,element(((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min,Tab)))|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([H|_T],_NumBits,{_Min,_Max,_Tab}) ->
    exit({error,{asn1,{illegal_char_value,H}}});
chars_encode2([],_,_) ->
    [].

exit_if_false(V,false)->
    exit({error,{asn1,{"illegal value according to Permitted alphabet constraint",V}}});
exit_if_false(_,V) ->V.

pre_complete_bits(NumBits,Val) when NumBits =< 8 ->
    [10,NumBits,Val];
pre_complete_bits(NumBits,Val) when NumBits =< 16 ->
    [10,NumBits-8,Val bsr 8,10,8,(Val band 255)];
pre_complete_bits(NumBits,Val) when NumBits =< 2040 -> % 255 * 8
    Unused = (8 - (NumBits rem 8)) rem 8,
    Len = NumBits + Unused,
    [30,Unused,Len div 8,<<(Val bsl Unused):Len>>].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_UTF8String(Val) -> CompleteList
%% Val -> <<utf8encoded binary>>
%% CompleteList -> [apropriate codes and values for driver complete]
%%
encode_UTF8String(Val) when is_binary(Val) ->
    Sz = byte_size(Val),
    [encode_length(Sz),octets_to_complete(Sz, Val)];
encode_UTF8String(Val) ->
    encode_UTF8String(list_to_binary(Val)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_object_identifier(Val) -> CompleteList
%% encode_object_identifier({Name,Val}) -> CompleteList
%% Val -> {Int1,Int2,...,IntN} % N >= 2
%% Name -> atom()
%% Int1 -> integer(0..2)
%% Int2 -> integer(0..39) when Int1 (0..1) else integer()
%% Int3-N -> integer()
%% CompleteList -> [{bits,8,Val}|{octets,Ol}|align|...]
%%
encode_object_identifier(Val) ->
    OctetList = e_object_identifier(Val),
    Octets = list_to_binary(OctetList),
    Sz = byte_size(Octets),
    [encode_length(Sz),
     octets_to_complete(Sz, Octets)].

e_object_identifier({'OBJECT IDENTIFIER',V}) ->
    e_object_identifier(V);
e_object_identifier(V) when is_tuple(V) ->
    e_object_identifier(tuple_to_list(V));

%% E1 = 0|1|2 and (E2 < 40 when E1 = 0|1)
e_object_identifier([E1,E2|Tail]) when E1 >= 0, E1 < 2, E2 < 40 ; E1==2 ->
    Head = 40*E1 + E2,  % weird
    e_object_elements([Head|Tail],[]);
e_object_identifier(Oid=[_,_|_Tail]) ->
    exit({error,{asn1,{'illegal_value',Oid}}}).

e_object_elements([],Acc) ->
    lists:reverse(Acc);
e_object_elements([H|T],Acc) ->
    e_object_elements(T,[e_object_element(H)|Acc]).

e_object_element(Num) when Num < 128 ->
    [Num];
e_object_element(Num) ->
    [e_o_e(Num bsr 7)|[Num band 2#1111111]].
e_o_e(Num) when Num < 128 ->
    Num bor 2#10000000;
e_o_e(Num) ->
    [e_o_e(Num bsr 7)|[(Num band 2#1111111) bor 2#10000000]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_relative_oid(Val) -> CompleteList
%% encode_relative_oid({Name,Val}) -> CompleteList
encode_relative_oid(Val) when is_tuple(Val) ->
    encode_relative_oid(tuple_to_list(Val));
encode_relative_oid(Val) when is_list(Val) ->
    Octets = list_to_binary([e_object_element(X)||X <- Val]),
    Sz = byte_size(Octets),
    [encode_length(Sz)|octets_to_complete(Sz, Octets)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% complete(InList) -> ByteList
%% Takes a coded list with bits and bytes and converts it to a list of bytes
%% Should be applied as the last step at encode of a complete ASN.1 type
%%

complete(L) ->
    asn1rt_nif:encode_per_complete(L).

octets_to_complete(Len,Val) when Len < 256 ->
    [20,Len,Val];
octets_to_complete(Len,Val) ->
    [21,<<Len:16>>,Val].

octets_unused_to_complete(Unused,Len,Val) when Len < 256 ->
    [30,Unused,Len,Val];
octets_unused_to_complete(Unused,Len,Val) ->
    [31,Unused,<<Len:16>>,Val].
