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
%%
-module(asn1rtt_uper).

-export([setext/1, fixoptionals/3,
	 fixextensions/2,
	 skipextensions/3, getbit/1, getchoice/3 ]).
-export([set_choice/3, encode_integer/2, encode_integer/3]).
-export([encode_small_number/1, encode_constrained_number/2,
	 encode_boolean/1,
	 encode_length/1, encode_length/2,
	 encode_bit_string/3]).
-export([encode_octet_string/1,encode_octet_string/2,
	 encode_relative_oid/1,
	 encode_object_identifier/1,
	 complete/1, complete_NFP/1]).

 -export([encode_open_type/1]).

 -export([encode_UniversalString/2,
	 encode_PrintableString/2,
	 encode_GeneralString/2,
	 encode_GraphicString/2,
	 encode_TeletexString/2,
	 encode_VideotexString/2,
	 encode_VisibleString/2,
	 encode_UTF8String/1,
	 encode_BMPString/2,
	 encode_IA5String/2,
	 encode_NumericString/2,
	 encode_ObjectDescriptor/2
	]).

-define('16K',16384).
-define('32K',32768).
-define('64K',65536).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% setext(true|false) ->  CompleteList
%%

setext(false) ->
    <<0:1>>;
setext(true) ->
    <<1:1>>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the new fixoptionals/3 which is used by the new generates
%%
fixoptionals(OptList,OptLength,Val) when is_tuple(Val) ->
    Bits = fixoptionals(OptList,Val,0),
    {Val,<<Bits:OptLength>>};

fixoptionals([],_Val,Acc) ->
    %% Optbits
    Acc;
fixoptionals([{Pos,DefVal}|Ot],Val,Acc) ->
    case element(Pos,Val) of
	asn1_DEFAULT -> fixoptionals(Ot,Val,Acc bsl 1);
	DefVal -> fixoptionals(Ot,Val,Acc bsl 1);
	_ -> fixoptionals(Ot,Val,(Acc bsl 1) + 1)
    end;
fixoptionals([Pos|Ot],Val,Acc) ->
    case element(Pos,Val) of
	asn1_NOVALUE -> fixoptionals(Ot,Val,Acc bsl 1);
	asn1_DEFAULT -> fixoptionals(Ot,Val,Acc bsl 1);
	_ -> fixoptionals(Ot,Val,(Acc bsl 1) + 1)
    end.


fixextensions({ext,ExtPos,ExtNum},Val) ->
    case fixextensions(ExtPos,ExtNum+ExtPos,Val,0) of
	0 -> [];
	ExtBits ->
	    [encode_small_length(ExtNum),<<ExtBits:ExtNum>>]
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


getchoice(Bytes,1,0) -> % only 1 alternative is not encoded
    {0,Bytes};
getchoice(Bytes,_,1) ->
    decode_small_number(Bytes);
getchoice(Bytes,NumChoices,0) ->
    decode_constrained_number(Bytes,{0,NumChoices-1}).


getbit(Buffer) ->
    <<B:1,Rest/bitstring>> = Buffer,
    {B,Rest}.

getbits(Buffer, Num) when is_bitstring(Buffer) ->
    <<Bs:Num,Rest/bitstring>> = Buffer,
    {Bs,Rest}.


%% Pick the first Num octets.
%% Returns octets as an integer with bit significance as in buffer.
getoctets(Buffer, Num) when is_bitstring(Buffer) ->
    <<Val:Num/integer-unit:8,RestBitStr/bitstring>> = Buffer,
    {Val,RestBitStr}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set_choice(Alt,Choices,Altnum) -> ListofBitSettings
%% Alt = atom()
%% Altnum = integer() | {integer(),integer()}% number of alternatives
%% Choices = [atom()] | {[atom()],[atom()]}
%% When Choices is a tuple the first list is the Rootset and the
%% second is the Extensions and then Altnum must also be a tuple with the
%% lengths of the 2 lists
%%
set_choice(Alt, {L1,L2}, {Len1,_Len2}) ->
    case set_choice_tag(Alt, L1) of
	N when is_integer(N), Len1 > 1 ->
	    [<<0:1>>, % the value is in the root set
	     encode_integer([{'ValueRange',{0,Len1-1}}],N)];
	N when is_integer(N) ->
	    <<0:1>>; % no encoding if only 0 or 1 alternative
	false ->
	    [<<1:1>>, % extension value
	     case set_choice_tag(Alt,L2) of
		 N2 when is_integer(N2) ->
		     encode_small_number(N2);
		 false ->
		     unknown_choice_alt
	     end]
    end;
set_choice(Alt,L,Len) ->
    case set_choice_tag(Alt,L) of
	N when is_integer(N), Len > 1 ->
	    encode_integer([{'ValueRange',{0,Len-1}}],N);
	N when is_integer(N) ->
	    []; % no encoding if only 0 or 1 alternative
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
    encode_open_type(list_to_binary(Val));
encode_open_type(Val) when is_binary(Val) ->
    [encode_length(byte_size(Val)),Val].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_integer(Constraint,Value,NamedNumberList) -> CompleteList
%% encode_integer(Constraint,Value) -> CompleteList
%% encode_integer(Constraint,{Name,Value}) -> CompleteList
%%
%%
encode_integer(C, V, NamedNumberList) when is_atom(V) ->
    case lists:keyfind(V, 1, NamedNumberList) of
	{_,NewV} ->
	    encode_integer(C, NewV);
	false ->
	    exit({error,{asn1,{namednumber,V}}})
    end;
encode_integer(C, V, _NamedNumberList) when is_integer(V) ->
    encode_integer(C, V).

encode_integer([{Rc,_Ec}],Val) when is_tuple(Rc) ->
    try
	[<<0:1>>,encode_integer([Rc], Val)]
    catch
	_:{error,{asn1,_}} ->
	    [<<1:1>>,encode_unconstrained_number(Val)]
    end;
encode_integer(C, Val) when is_list(C) ->
    case get_constraint(C, 'SingleValue') of
	no ->
	    encode_integer1(C,Val);
	V when is_integer(V), V =:= Val ->
	    []; % a type restricted to a single value encodes to nothing
	V when is_list(V) ->
	    case lists:member(Val,V) of
		true ->
		    encode_integer1(C,Val);
		_ ->
		    exit({error,{asn1,{illegal_value,Val}}})
	    end;
	_ ->
	    exit({error,{asn1,{illegal_value,Val}}})
    end.

encode_integer1(C, Val) ->
    case VR = get_constraint(C, 'ValueRange') of
	no ->
	    encode_unconstrained_number(Val);
	{Lb,'MAX'} ->
	    encode_semi_constrained_number(Lb, Val);
	%% positive with range
	{Lb,Ub} when Val >= Lb, Ub >= Val ->
	    encode_constrained_number(VR,Val);
	_ ->
	    exit({error,{asn1,{illegal_value,VR,Val}}})
    end.

%% X.691:10.6 Encoding of a normally small non-negative whole number
%% Use this for encoding of CHOICE index if there is an extension marker in
%% the CHOICE
encode_small_number(Val) when Val < 64 ->
    <<Val:7>>;
encode_small_number(Val) ->
    [<<1:1>>|encode_semi_constrained_number(0, Val)].

decode_small_number(Bytes) ->
    {Bit,Bytes2} = getbit(Bytes),
    case Bit of
	0 ->
	    getbits(Bytes2,6);
	1 ->
	    decode_semi_constrained_number(Bytes2)
    end.

%% X.691:10.7 Encoding of a semi-constrained whole number
encode_semi_constrained_number(Lb, Val) ->
    %% encoding in minimum number of octets preceeded by a length
    Val2 = Val - Lb,
    Bin = eint_bin_positive(Val2),
    Size = byte_size(Bin),
    if
	Size < 128 ->
	    [<<Size>>,Bin];
	Size < 16384 ->
	    [<<2:2,Size:14>>,Bin];
	true ->
	    [encode_length(Size),Bin]
    end.

decode_semi_constrained_number(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes),
    {V,Bytes3} = getoctets(Bytes2,Len),
    {V,Bytes3}.

encode_constrained_number({Lb,Ub}, Val) when Val >= Lb, Ub >= Val ->
    Range = Ub - Lb + 1,
    Val2 = Val - Lb,
    NumBits = num_bits(Range),
    <<Val2:NumBits>>;
encode_constrained_number(Range,Val) ->
    exit({error,{asn1,{integer_range,Range,value,Val}}}).


decode_constrained_number(Buffer, {Lb,Ub}) ->
    Range = Ub - Lb + 1,
    NumBits = num_bits(Range),
    {Val,Remain} = getbits(Buffer,NumBits),
    {Val+Lb,Remain}.

%% X.691:10.8 Encoding of an unconstrained whole number

encode_unconstrained_number(Val) when Val >= 0 ->
    Oct = eint_bin_2Cs(Val),
    Len = byte_size(Oct),
    if
	Len < 128 ->
	    [<<Len>>,Oct]; % equiv with encode_length(undefined,Len) but faster
	Len < 16384 ->
	    [<<2:2,Len:14>>,Oct];
	true ->
	    [encode_length(Len),<<Len:16>>,Oct]
    end;
encode_unconstrained_number(Val) -> % negative
    Oct = enint(Val,[]),
    Len = byte_size(Oct),
    if
	Len < 128 ->
	    [<<Len>>,Oct]; % equiv with encode_length(undefined,Len) but faster
	Len < 16384 ->
	    [<<2:2,Len:14>>,Oct];
	true ->
	    [encode_length(Len),Oct]
    end.


eint_bin_2Cs(Int) ->
    case eint_bin_positive(Int) of
	<<B,_/binary>> = Bin when B > 16#7f ->
	    <<0,Bin/binary>>;
	Bin -> Bin
    end.

%% returns the integer as a binary
eint_bin_positive(Val) when Val < 16#100  ->
    <<Val>>;
eint_bin_positive(Val) when Val < 16#10000 ->
    <<Val:16>>;
eint_bin_positive(Val) when Val < 16#1000000 ->
    <<Val:24>>;
eint_bin_positive(Val) when Val < 16#100000000 ->
    <<Val:32>>;
eint_bin_positive(Val) ->
    list_to_binary([eint_bin_positive2(Val bsr 32),<<Val:32>>]).

eint_bin_positive2(Val) when Val < 16#100  ->
    <<Val>>;
eint_bin_positive2(Val) when Val < 16#10000 ->
    <<Val:16>>;
eint_bin_positive2(Val) when Val < 16#1000000 ->
    <<Val:24>>;
eint_bin_positive2(Val) when Val < 16#100000000 ->
    <<Val:32>>;
eint_bin_positive2(Val) ->
    [eint_bin_positive2(Val bsr 32),<<Val:32>>].




enint(-1, [B1|T]) when B1 > 127 ->
    list_to_binary([B1|T]);
enint(N, Acc) ->
    enint(N bsr 8, [N band 16#ff|Acc]).


%% X.691:10.9 Encoding of a length determinant
%%encode_small_length(undefined,Len) -> % null means no UpperBound
%%    encode_small_number(Len).

%% X.691:10.9.3.5
%% X.691:10.9.3.7
encode_length(Len) -> % un-constrained
    if
	Len < 128 ->
	    <<Len>>;
	Len < 16384 ->
	    <<2:2,Len:14>>;
	true  -> % should be able to endode length >= 16384
	    error({error,{asn1,{encode_length,{nyi,above_16k}}}})
    end.

encode_length(undefined, Len) ->		% unconstrained
    encode_length(Len);
encode_length({0,'MAX'},Len) ->
    encode_length(undefined, Len);
encode_length({Lb,Ub}=Vr, Len) when Ub =< 65535, Lb >= 0 -> % constrained
    encode_constrained_number(Vr,Len);
encode_length({Lb,_Ub}, Len) when is_integer(Lb), Lb >= 0 -> % Ub > 65535
    encode_length(Len);
encode_length({{Lb,Ub}=Vr,Ext},Len)
  when Ub =< 65535, Lb >= 0, Len =< Ub, is_list(Ext) ->
    %% constrained extensible
    [<<0:1>>,encode_constrained_number(Vr,Len)];
encode_length({{Lb,_Ub},Ext}, Len) when is_list(Ext) ->
    [<<1:1>>,encode_semi_constrained_number(Lb, Len)];
encode_length(SingleValue, _Len) when is_integer(SingleValue) ->
    [].

%% X.691 10.9.3.4 (only used for length of bitmap that prefixes extension
%% additions in a sequence or set
encode_small_length(Len) when Len =< 64 ->
    <<(Len-1):7>>;
encode_small_length(Len) ->
    [<<1:1>>,encode_length(Len)].


%% un-constrained
decode_length(<<0:1,Oct:7,Rest/bitstring>>)  ->
    {Oct,Rest};
decode_length(<<2:2,Val:14,Rest/bitstring>>)  ->
    {Val,Rest};
decode_length(<<3:2,_:14,_Rest/bitstring>>)  ->
    exit({error,{asn1,{decode_length,{nyi,above_16k}}}}).

						% X.691:11
encode_boolean(true) ->
    <<1:1>>;
encode_boolean(false) ->
    <<0:1>>;
encode_boolean(Val) ->
    exit({error,{asn1,{encode_boolean,Val}}}).


%%============================================================================
%%============================================================================
%% Bitstring value, ITU_T X.690 Chapter 8.5
%%============================================================================
%%============================================================================

%%============================================================================
%% encode bitstring value
%%============================================================================



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bitstring NamedBitList
%% Val can be  of:
%% - [identifiers] where only named identifers are set to one,
%%   the Constraint must then have some information of the
%%   bitlength.
%% - [list of ones and zeroes] all bits
%% - integer value representing the bitlist
%% C is constraint Len, only valid when identifiers are present


%% when the value is a list of {Unused,BinBits}, where
%% Unused = integer(),
%% BinBits = binary().

encode_bit_string(C, Bits, NamedBitList) when is_bitstring(Bits) ->
    PadLen = (8 - (bit_size(Bits) band 7)) band 7,
    Compact = {PadLen,<<Bits/bitstring,0:PadLen>>},
    encode_bit_string(C, Compact, NamedBitList);
encode_bit_string(C, {Unused,BinBits}=Bin, NamedBitList)
  when is_integer(Unused), is_binary(BinBits) ->
    encode_bin_bit_string(C, Bin, NamedBitList);

encode_bit_string(C, BitListVal, NamedBitList) ->
    encode_bit_string1(C, BitListVal, NamedBitList).

%% when the value is a list of named bits
encode_bit_string1(C, [FirstVal|_RestVal]=LoNB, NamedBitList)
  when is_atom(FirstVal) ->
    ToSetPos = get_all_bitposes(LoNB, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos, 0),
    encode_bit_string1(C, BitList, NamedBitList);
encode_bit_string1(C, [{bit,_No}|_RestVal]=BL, NamedBitList) ->
    ToSetPos = get_all_bitposes(BL, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos, 0),
    encode_bit_string1(C, BitList, NamedBitList);
%% when the value is a list of ones and zeroes
encode_bit_string1(Int, BitListValue, _)
  when is_list(BitListValue), is_integer(Int) ->
    %% The type is constrained by a single value size constraint
    bit_list2bitstr(Int, BitListValue);
encode_bit_string1(no, BitListValue, [])
  when is_list(BitListValue) ->
    Len = length(BitListValue),
    [encode_length(Len),bit_list2bitstr(Len,BitListValue)];
encode_bit_string1(C, BitListValue,[])
  when is_list(BitListValue) ->
    Len = length(BitListValue),
    [encode_length(C, Len),bit_list2bitstr(Len,BitListValue)];
encode_bit_string1(no, BitListValue,_NamedBitList)
  when is_list(BitListValue) ->
    NewBitLVal = lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,
					    lists:reverse(BitListValue))),
    Len = length(NewBitLVal),
    [encode_length(Len),bit_list2bitstr(Len,NewBitLVal)];
encode_bit_string1(C, BitListValue, _NamedBitList)
  when is_list(BitListValue) ->% C = {_,'MAX'}
    NewBitStr = bitstr_trailing_zeros(BitListValue, C),
    [encode_length(C, bit_size(NewBitStr)),NewBitStr];


%% when the value is an integer
encode_bit_string1(C, IntegerVal, NamedBitList) when is_integer(IntegerVal)->
    BitList = int_to_bitlist(IntegerVal),
    encode_bit_string1(C, BitList, NamedBitList).

bit_list2bitstr(Len,BitListValue) ->
    case length(BitListValue) of
	Len ->
	    << <<B:1>> || B <- BitListValue>>;
	L when L > Len -> % truncate
	    <<(<< <<B:1>> || B <- BitListValue>>):Len/bitstring>>;
	L -> % Len > L -> pad
	    <<(<< <<B:1>> || B <- BitListValue>>)/bitstring,0:(Len-L)>>
	end.

adjust_trailing_zeros(Len, Bin) when Len =:= bit_size(Bin) ->
    Bin;
adjust_trailing_zeros(Len, Bin) when Len > bit_size(Bin) ->
    <<Bin/bitstring,0:(Len-bit_size(Bin))>>;
adjust_trailing_zeros(Len,Bin) ->
    <<Bin:Len/bitstring>>.

bitstr_trailing_zeros(BitList, C) when is_integer(C) ->
    bitstr_trailing_zeros1(BitList, C, C);
bitstr_trailing_zeros(BitList, {Lb,Ub}) when is_integer(Lb) ->
    bitstr_trailing_zeros1(BitList,Lb,Ub);
bitstr_trailing_zeros(BitList, {{Lb,Ub},_}) when is_integer(Lb) ->
    bitstr_trailing_zeros1(BitList, Lb, Ub);
bitstr_trailing_zeros(BitList, _) ->
    bit_list2bitstr(length(BitList), BitList).

bitstr_trailing_zeros1(BitList, Lb, Ub) ->
    case length(BitList) of
	Lb -> bit_list2bitstr(Lb, BitList);
	B when B < Lb -> bit_list2bitstr(Lb, BitList);
	D -> F = fun(L,LB,LB,_,_)->bit_list2bitstr(LB,lists:reverse(L));
		    ([0|R],L1,LB,UB,Fun)->Fun(R,L1-1,LB,UB,Fun);
		    (L,L1,_,UB,_)when L1 =< UB ->
			 bit_list2bitstr(L1,lists:reverse(L));
		    (_,_L1,_,_,_) ->exit({error,{list_length_BIT_STRING,
						 BitList}}) end,
	     F(lists:reverse(BitList),D,Lb,Ub,F)
    end.

%% encode_bin_bit_string/3, when value is a tuple of Unused and BinBits.
%% Unused = integer(),i.e. number unused bits in least sign. byte of
%% BinBits = binary().
encode_bin_bit_string(C, {_,BinBits}, _NamedBitList)
  when is_integer(C), C =< 16 ->
    adjust_trailing_zeros(C, BinBits);
encode_bin_bit_string(C, {_Unused,BinBits}, _NamedBitList)
  when is_integer(C) ->
    adjust_trailing_zeros(C, BinBits);
encode_bin_bit_string(C, {_,_}=UnusedAndBin, NamedBitList) ->
    %% removes all trailing bits if NamedBitList is not empty
    BitStr = remove_trailing_bin(NamedBitList, UnusedAndBin),
    case C of
	{Lb,Ub} when is_integer(Lb),is_integer(Ub) ->
	    [encode_length({Lb,Ub},bit_size(BitStr)),BitStr];
	no ->
	    [encode_length(bit_size(BitStr)),BitStr];
	Sc ->
	    [encode_length(Sc,bit_size(BitStr)),BitStr]
    end.


remove_trailing_bin([], {Unused,Bin}) ->
    BS = bit_size(Bin)-Unused,
    <<BitStr:BS/bitstring,_:Unused>> = Bin,
    BitStr;
remove_trailing_bin(_NamedNumberList, {_Unused,<<>>}) ->
    <<>>;
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
	    BS = bit_size(Bin) - Unused2,
	    <<BitStr:BS/bitstring,_:Unused2>> = Bin,
	    BitStr
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
%% encode_octet_string(Val)
%% encode_octet_string(Constraint, Val)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_octet_string(Val) ->
    try
	[encode_length(length(Val)),list_to_binary(Val)]
    catch
	error:{error,{asn1,{encode_length,_}}} ->
	    encode_fragmented_octet_string(Val)
    end.

encode_octet_string(C, Val) ->
    case C of
	1 ->
	    list_to_binary(Val);
	2 ->
	    list_to_binary(Val);
	{_,_}=VR  ->
	    try
		[encode_length(VR, length(Val)),list_to_binary(Val)]
	    catch
		error:{error,{asn1,{encode_length,_}}} ->
		    encode_fragmented_octet_string(Val)
	    end;
	Sv when is_integer(Sv), Sv =:= length(Val) -> % fixed length
	    if
		Sv =< 65535 ->
		    list_to_binary(Val);
		true ->
		    encode_fragmented_octet_string(Val)
	    end;
	Sv when is_list(Sv) ->
	    try
		[encode_length({hd(Sv),lists:max(Sv)},
			       length(Val)),list_to_binary(Val)]
	    catch
		error:{error,{asn1,{encode_length,_}}} ->
		    encode_fragmented_octet_string(Val)
	    end
    end.


encode_fragmented_octet_string(Val) ->
    Bin = list_to_binary(Val),
    efos_1(Bin).

efos_1(<<B:16#10000/binary,T/binary>>) ->
    [<<3:2,4:6>>,B|efos_1(T)];
efos_1(<<B:16#C000/binary,T/binary>>) ->
    [<<3:2,3:6>>,B|efos_1(T)];
efos_1(<<B:16#8000/binary,T/binary>>) ->
    [<<3:2,2:6>>,B|efos_1(T)];
efos_1(<<B:16#4000/binary,T/binary>>) ->
    [<<3:2,1:6>>,B|efos_1(T)];
efos_1(<<B/bitstring>>) ->
    Len = byte_size(B),
    [encode_length(Len),B].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Restricted char string types
%% (NumericString, PrintableString,VisibleString,IA5String,BMPString,UniversalString)
%% X.691:26 and X.680:34-36
%%encode_restricted_string('BMPString',Constraints,Extension,Val)


encode_restricted_string(Val) when is_list(Val)->
    [encode_length(length(Val)),list_to_binary(Val)].

encode_known_multiplier_string(StringType, C, Val) ->
    Result = chars_encode(C, StringType, Val),
    NumBits = get_NumBits(C, StringType),
    case get_constraint(C, 'SizeConstraint') of
	Ub when is_integer(Ub), Ub*NumBits =< 16  ->
	    Result;
	0 ->
	    [];
	Ub when is_integer(Ub),Ub =<65535 -> % fixed length
	    Result;
	{Ub,Lb} ->
	    [encode_length({Ub,Lb}, length(Val)),Result];
	Vl when is_list(Vl) ->
	    [encode_length({lists:min(Vl),lists:max(Vl)}, length(Val)),Result];
	no  ->
	    [encode_length(length(Val)),Result]
    end.

encode_NumericString(C,Val) ->
    encode_known_multiplier_string('NumericString',C,Val).

encode_PrintableString(C,Val) ->
    encode_known_multiplier_string('PrintableString',C,Val).

encode_VisibleString(C,Val) -> % equivalent with ISO646String
    encode_known_multiplier_string('VisibleString',C,Val).

encode_IA5String(C,Val) ->
    encode_known_multiplier_string('IA5String',C,Val).

encode_BMPString(C,Val) ->
    encode_known_multiplier_string('BMPString',C,Val).

encode_UniversalString(C,Val) ->
    encode_known_multiplier_string('UniversalString',C,Val).


%% end of known-multiplier strings for which PER visible constraints are
%% applied

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
%% encodes chars according to the per rules taking the constraint PermittedAlphabet
%% into account.
%% This function does only encode the value part and NOT the length

chars_encode(C,StringType,Value) ->
    case {StringType,get_constraint(C,'PermittedAlphabet')} of
	{'UniversalString',{_,_Sv}} ->
	    exit({error,{asn1,{'not implemented',"UniversalString with PermittedAlphabet constraint"}}});
	{'BMPString',{_,_Sv}} ->
	    exit({error,{asn1,{'not implemented',"BMPString with PermittedAlphabet constraint"}}});
	_ ->
	    {NumBits,CharOutTab} = {get_NumBits(C,StringType),get_CharOutTab(C,StringType)},
	    chars_encode2(Value,NumBits,CharOutTab)
    end.

chars_encode2([H|T],NumBits,{Min,Max,notab}) when  H =< Max, H >= Min ->
    [<<(H-Min):NumBits>>|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([H|T],NumBits,{Min,Max,Tab}) when H =< Max, H >= Min ->
    Ch = exit_if_false(H,element(H-Min+1,Tab)),
    [<<Ch:NumBits>>|chars_encode2(T,NumBits,{Min,Max,Tab})];
chars_encode2([{A,B,C,D}|T],NumBits,{Min,Max,notab}) ->
    %% no value range check here (ought to be, but very expensive)
    Ch = ((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min,
    [<<Ch:NumBits>>|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([{A,B,C,D}|T],NumBits,{Min,Max,Tab}) ->
    %% no value range check here (ought to be, but very expensive)
    Ch = exit_if_false({A,B,C,D},element(((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min,Tab)),
    [<<Ch:NumBits>>|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([H|_T],_,{_,_,_}) ->
    exit({error,{asn1,{illegal_char_value,H}}});
chars_encode2([],_,_) ->
    [].

exit_if_false(V,false)->
    exit({error,{asn1,{"illegal value according to Permitted alphabet constraint",V}}});
exit_if_false(_,V) ->V.


get_NumBits(C,StringType) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    charbits(length(Sv));
	no ->
	    case StringType of
		'IA5String' ->
		    charbits(128); % 16#00..16#7F
		'VisibleString' ->
		    charbits(95); % 16#20..16#7E
		'PrintableString' ->
		    charbits(74); % [$\s,$',$(,$),$+,$,,$-,$.,$/,"0123456789",$:,$=,$?,$A..$Z,$a..$z
		'NumericString' ->
		    charbits(11); % $ ,"0123456789"
		'UniversalString' ->
		    32;
		'BMPString' ->
		    16
	    end
    end.

get_CharOutTab(C,StringType) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    get_CharTab2(C,StringType,hd(Sv),lists:max(Sv),Sv);
	no ->
	    case StringType of
		'IA5String' ->
		    {0,16#7F,notab};
		'VisibleString' ->
		    get_CharTab2(C,StringType,16#20,16#7F,notab);
		'PrintableString' ->
		    Chars = lists:sort(
			      " '()+,-./0123456789:=?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
		    get_CharTab2(C,StringType,hd(Chars),lists:max(Chars),Chars);
		'NumericString' ->
		    get_CharTab2(C,StringType,16#20,$9," 0123456789");
		'UniversalString' ->
		    {0,16#FFFFFFFF,notab};
		'BMPString' ->
		    {0,16#FFFF,notab}
	    end
    end.

get_CharTab2(C,StringType,Min,Max,Chars) ->
    BitValMax = (1 bsl get_NumBits(C,StringType))-1,
    if
	Max =< BitValMax ->
	    {0,Max,notab};
	true ->
	    {Min,Max,create_char_tab(Min,Chars)}
    end.

create_char_tab(Min,L) ->
    list_to_tuple(create_char_tab(Min,L,0)).
create_char_tab(Min,[Min|T],V) ->
    [V|create_char_tab(Min+1,T,V+1)];
create_char_tab(_Min,[],_V) ->
    [];
create_char_tab(Min,L,V) ->
    [false|create_char_tab(Min+1,L,V)].

%% See Table 20.3 in Dubuisson
charbits(NumOfChars) when NumOfChars =< 2 -> 1;
charbits(NumOfChars) when NumOfChars =< 4 -> 2;
charbits(NumOfChars) when NumOfChars =< 8 -> 3;
charbits(NumOfChars) when NumOfChars =< 16 -> 4;
charbits(NumOfChars) when NumOfChars =< 32 -> 5;
charbits(NumOfChars) when NumOfChars =< 64 -> 6;
charbits(NumOfChars) when NumOfChars =< 128 -> 7;
charbits(NumOfChars) when NumOfChars =< 256 -> 8;
charbits(NumOfChars) when NumOfChars =< 512 -> 9;
charbits(NumOfChars) when NumOfChars =< 1024 -> 10;
charbits(NumOfChars) when NumOfChars =< 2048 -> 11;
charbits(NumOfChars) when NumOfChars =< 4096 -> 12;
charbits(NumOfChars) when NumOfChars =< 8192 -> 13;
charbits(NumOfChars) when NumOfChars =< 16384 -> 14;
charbits(NumOfChars) when NumOfChars =< 32768 -> 15;
charbits(NumOfChars) when NumOfChars =< 65536 -> 16;
charbits(NumOfChars) when is_integer(NumOfChars) ->
    16 + charbits1(NumOfChars bsr 16).

charbits1(0) ->
    0;
charbits1(NumOfChars) ->
    1 + charbits1(NumOfChars bsr 1).


%% UTF8String
encode_UTF8String(Val) when is_binary(Val) ->
    [encode_length(byte_size(Val)),Val];
encode_UTF8String(Val) ->
    Bin = list_to_binary(Val),
    encode_UTF8String(Bin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_object_identifier(Val) -> CompleteList
%% encode_object_identifier({Name,Val}) -> CompleteList
%% Val -> {Int1,Int2,...,IntN} % N >= 2
%% Name -> atom()
%% Int1 -> integer(0..2)
%% Int2 -> integer(0..39) when Int1 (0..1) else integer()
%% Int3-N -> integer()
%% CompleteList -> [binary()|bitstring()|list()]
%%
encode_object_identifier(Val) ->
    OctetList = e_object_identifier(Val),
    Octets = list_to_binary(OctetList), % performs a flatten at the same time
    [encode_length(byte_size(Octets)),Octets].

%% This code is copied from asn1_encode.erl (BER) and corrected and modified

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
    [encode_length(byte_size(Octets)),Octets].


get_constraint([{Key,V}],Key) ->
    V;
get_constraint([],_Key) ->
    no;
get_constraint(C,Key) ->
    case lists:keyfind(Key, 1, C) of
	false ->
	    no;
	{_,V} ->
	    V
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% complete(InList) -> ByteList
%% Takes a coded list with bits and bytes and converts it to a list of bytes
%% Should be applied as the last step at encode of a complete ASN.1 type
%%
complete(InList) when is_list(InList) ->
    case complete1(InList) of
	<<>> ->
	    <<0>>;
	Res ->
	    case bit_size(Res) band 7 of
		0 -> Res;
		Bits -> <<Res/bitstring,0:(8-Bits)>>
	end
    end;
complete(InList) when is_binary(InList) ->
    InList;
complete(InList) when is_bitstring(InList) ->
    PadLen = 8 - (bit_size(InList) band 7),
    <<InList/bitstring,0:PadLen>>.

complete1(L) when is_list(L) ->
    list_to_bitstring(L).

%% Special version of complete that does not align the completed message.
complete_NFP(InList) when is_list(InList) ->
    list_to_bitstring(InList);
complete_NFP(InList) when is_bitstring(InList) ->
    InList.

%% unaligned helpers

%% 10.5.6 NOTE: If "range" satisfies the inequality 2^m < "range" =<
%% 2^(m+1) then the number of bits = m + 1

num_bits(N) -> num_bits(N, 1, 0).

num_bits(N,T,B) when N =< T -> B;
num_bits(N,T,B) -> num_bits(N, T bsl 1, B+1).
