%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
-module(asn1rt_uper_bin).

%% encoding / decoding of PER unaligned

-include("asn1_records.hrl").

%%-compile(export_all).

 -export([cindex/3, list_to_record/2]).
 -export([setext/1, fixoptionals/3, 
 	 fixextensions/2, 
 	 getext/1, getextension/2, skipextensions/3, getbit/1, getchoice/3 ]).
 -export([getoptionals2/2, set_choice/3, encode_integer/2, encode_integer/3  ]).
 -export([decode_integer/2, decode_integer/3, encode_small_number/1, encode_boolean/1, 
 	 decode_boolean/1, encode_length/2, decode_length/1, decode_length/2,
 	 encode_small_length/1, decode_small_length/1,
 	 decode_compact_bit_string/3]).
 -export([decode_enumerated/3, 
 	 encode_bit_string/3, decode_bit_string/3  ]).
 -export([encode_octet_string/2, decode_octet_string/2,
	  encode_null/1, decode_null/1,
	  encode_relative_oid/1, decode_relative_oid/1,
 	 encode_object_identifier/1, decode_object_identifier/1,
	  encode_real/1, decode_real/1,
 	 complete/1, complete_NFP/1]).


 -export([encode_open_type/2, decode_open_type/2]).

 -export([encode_UniversalString/2, decode_UniversalString/2,
 	 encode_PrintableString/2, decode_PrintableString/2,
 	 encode_GeneralString/2, decode_GeneralString/2,
 	 encode_GraphicString/2, decode_GraphicString/2,
 	 encode_TeletexString/2, decode_TeletexString/2,
 	 encode_VideotexString/2, decode_VideotexString/2,
 	 encode_VisibleString/2, decode_VisibleString/2,
 	 encode_UTF8String/1, decode_UTF8String/1,
 	 encode_BMPString/2, decode_BMPString/2,
 	 encode_IA5String/2, decode_IA5String/2,
 	 encode_NumericString/2, decode_NumericString/2,
 	 encode_ObjectDescriptor/2, decode_ObjectDescriptor/1
 	]).

-define('16K',16384).
-define('32K',32768).
-define('64K',65536).


cindex(Ix,Val,Cname) ->
    case element(Ix,Val) of
	{Cname,Val2} -> Val2;
	X -> X
    end.

%% converts a list to a record if necessary
list_to_record(_Name,Tuple) when is_tuple(Tuple) ->
    Tuple;
list_to_record(Name,List) when is_list(List) ->
    list_to_tuple([Name|List]).


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


getext(Bytes) when is_bitstring(Bytes) ->
    getbit(Bytes).

getextension(0, Bytes) ->
    {{},Bytes};
getextension(1, Bytes) ->
    {Len,Bytes2} = decode_small_length(Bytes),
    {Blist, Bytes3} = getbits_as_list(Len,Bytes2),
    {list_to_tuple(Blist),Bytes3}.

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

skipextensions(Bytes,Nr,ExtensionBitPattern) -> 
    case (catch element(Nr,ExtensionBitPattern)) of
	1 ->
	    {_,Bytes2} = decode_open_type(Bytes,[]),
	    skipextensions(Bytes2, Nr+1, ExtensionBitPattern);
	0 ->
	    skipextensions(Bytes, Nr+1, ExtensionBitPattern);
	{'EXIT',_} -> % badarg, no more extensions
	    Bytes
    end.


getchoice(Bytes,1,0) -> % only 1 alternative is not encoded
    {0,Bytes};
getchoice(Bytes,_,1) ->
    decode_small_number(Bytes);
getchoice(Bytes,NumChoices,0) ->
    decode_constrained_number(Bytes,{0,NumChoices-1}).


%%%%%%%%%%%%%%%
getoptionals2(Bytes,NumOpt) ->
    getbits(Bytes,NumOpt).


%% getbits_as_binary(Num,Bytes) -> {{Unused,BinBits},RestBytes},
%% Num = integer(),
%% Bytes = list() | tuple(),
%% Unused = integer(),
%% BinBits = binary(),
%% RestBytes = tuple()
getbits_as_binary(Num,Bytes) when is_bitstring(Bytes) ->
    <<BS:Num/bitstring,Rest/bitstring>> = Bytes,
    {BS,Rest}.

getbits_as_list(Num,Bytes) when is_bitstring(Bytes) ->
    <<BitStr:Num/bitstring,Rest/bitstring>> = Bytes,
    {[ B || <<B:1>> <= BitStr],Rest}.

getbit(Buffer) ->
    <<B:1,Rest/bitstring>> = Buffer,
    {B,Rest}.


getbits(Buffer,Num) when is_bitstring(Buffer) ->
    <<Bs:Num,Rest/bitstring>> = Buffer,
    {Bs,Rest}.



%% Pick the first Num octets.
%% Returns octets as an integer with bit significance as in buffer.
getoctets(Buffer,Num) when is_bitstring(Buffer) ->
    <<Val:Num/integer-unit:8,RestBitStr/bitstring>> = Buffer,
    {Val,RestBitStr}.

%% Pick the first Num octets.
%% Returns octets as a binary
getoctets_as_bin(Bin,Num) when is_bitstring(Bin) ->
    <<Octets:Num/binary,RestBin/bitstring>> = Bin,
    {Octets,RestBin}.

%% same as above but returns octets as a List
getoctets_as_list(Buffer,Num) ->
    {Bin,Buffer2} = getoctets_as_bin(Buffer,Num),
    {binary_to_list(Bin),Buffer2}.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_fragmented_XXX; decode of values encoded fragmented according
%% to ITU-T X.691 clause 10.9.3.8. The unit (XXX) is either bits, octets,
%% characters or number of components (in a choice,sequence or similar).
%% Buffer is a buffer {Used, Bin}.
%% C is the constrained length.
%% If the buffer is not aligned, this function does that.
decode_fragmented_bits(Buffer,C) ->
    decode_fragmented_bits(Buffer,C,[]).
decode_fragmented_bits(<<3:2,Len:6,BitStr/bitstring>>,C,Acc) ->
%%    {Value,Bin2} = split_binary(Bin, Len * ?'16K'),
    FragLen = (Len*?'16K') div 8,
    <<Value:FragLen/binary,BitStr2/bitstring>> = BitStr,
    decode_fragmented_bits(BitStr2,C,[Value|Acc]);
decode_fragmented_bits(<<0:1,0:7,BitStr/bitstring>>,C,Acc) ->
    BinBits = list_to_binary(lists:reverse(Acc)),
    case C of
	Int when is_integer(Int),C == size(BinBits) ->
	    {BinBits,BitStr};
	Int when is_integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinBits}}})
    end;
decode_fragmented_bits(<<0:1,Len:7,BitStr/bitstring>>,C,Acc) ->
    <<Val:Len/bitstring,Rest/bitstring>> = BitStr,
%%    <<Value:Len/binary-unit:1,Bin2/binary>> = Bin,
    ResBitStr = list_to_bitstring(lists:reverse([Val|Acc])),
    case C of
	Int when is_integer(Int),C == bit_size(ResBitStr) ->
	    {ResBitStr,Rest};
	Int when is_integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,ResBitStr}}})
    end.


decode_fragmented_octets({0,Bin},C) ->
    decode_fragmented_octets(Bin,C,[]).

decode_fragmented_octets(<<3:2,Len:6,BitStr/bitstring>>,C,Acc) ->
    FragLen = Len * ?'16K',
    <<Value:FragLen/binary,Rest/bitstring>> = BitStr,
    decode_fragmented_octets(Rest,C,[Value|Acc]);
decode_fragmented_octets(<<0:1,0:7,Bin/bitstring>>,C,Acc) ->
    Octets = list_to_binary(lists:reverse(Acc)),
    case C of
	Int when is_integer(Int), C == size(Octets) ->
	    {Octets,Bin};
	Int when is_integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,Octets}}})
    end;
decode_fragmented_octets(<<0:1,Len:7,BitStr/bitstring>>,C,Acc) ->
    <<Value:Len/binary-unit:8,BitStr2/binary>> = BitStr,
    BinOctets = list_to_binary(lists:reverse([Value|Acc])),
    case C of
	Int when is_integer(Int),size(BinOctets) == Int ->
	    {BinOctets,BitStr2};
	Int when is_integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinOctets}}})
    end.


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Constraint, Value) -> CompleteList
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary
%% Contraint = not used in this version
%%
encode_open_type(C, Val) when is_list(Val) ->
    encode_open_type(C, list_to_binary(Val));
encode_open_type(_C, Val) when is_binary(Val) ->
    [encode_length(undefined,size(Val)),Val].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Buffer,Constraint) -> Value
%% Constraint is not used in this version
%% Buffer = [byte] with PER encoded data 
%% Value = [byte] with decoded data (which must be decoded again as some type)
%%
decode_open_type(Bytes, _C) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_bin(Bytes2,Len).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_integer(Constraint,Value,NamedNumberList) -> CompleteList
%% encode_integer(Constraint,Value) -> CompleteList
%% encode_integer(Constraint,{Name,Value}) -> CompleteList
%% 
%%
encode_integer(C,V,NamedNumberList) when is_atom(V) ->
    case lists:keysearch(V,1,NamedNumberList) of
	{value,{_,NewV}} -> 
	    encode_integer(C,NewV);
	_ -> 
	    exit({error,{asn1,{namednumber,V}}})
    end;
encode_integer(C,V,_NamedNumberList) when is_integer(V) ->
    encode_integer(C,V);
encode_integer(C,{Name,V},NamedNumberList) when is_atom(Name) ->
    encode_integer(C,V,NamedNumberList).

encode_integer(C,{Name,Val}) when is_atom(Name) ->
    encode_integer(C,Val);

encode_integer([{Rc,_Ec}],Val) when is_tuple(Rc) -> % XXX when is this invoked? First argument most often a list,...Ok this is the extension case...but it doesn't work.
    case (catch encode_integer([Rc],Val)) of
	{'EXIT',{error,{asn1,_}}} ->
	    [<<1:1>>,encode_unconstrained_number(Val)];
	Encoded ->
	    [<<0:1>>,Encoded]
    end;
encode_integer(C,Val ) when is_list(C) ->
    case get_constraint(C,'SingleValue') of
	no ->
	    encode_integer1(C,Val);
	V when is_integer(V),V == Val ->
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
    case VR = get_constraint(C,'ValueRange') of
	no ->
	    encode_unconstrained_number(Val);
	{Lb,'MAX'} ->
	    encode_semi_constrained_number(Lb,Val);
	%% positive with range
	{Lb,Ub} when Val >= Lb,
		     Ub >= Val ->
	    encode_constrained_number(VR,Val);
	_ ->
	    exit({error,{asn1,{illegal_value,VR,Val}}})
    end.

decode_integer(Buffer,Range,NamedNumberList) ->
    {Val,Buffer2} = decode_integer(Buffer,Range),
    case lists:keysearch(Val,2,NamedNumberList) of
	{value,{NewVal,_}} -> {NewVal,Buffer2};
	_ -> {Val,Buffer2}
    end.

decode_integer(Buffer,[{Rc,_Ec}]) when is_tuple(Rc) ->
    {Ext,Buffer2} = getext(Buffer),
    case Ext of
	0 -> decode_integer(Buffer2,[Rc]); %% Value in root of constraint
	1 -> decode_unconstrained_number(Buffer2)
    end;
decode_integer(Buffer,undefined) ->
    decode_unconstrained_number(Buffer);
decode_integer(Buffer,C) ->
    case get_constraint(C,'SingleValue') of
	V when is_integer(V) ->
	    {V,Buffer};
	V when is_list(V) ->
	    {Val,Buffer2} = decode_integer1(Buffer,C),
	    case lists:member(Val,V) of
		true ->
		    {Val,Buffer2};
		_ -> 
		    exit({error,{asn1,{illegal_value,Val}}})
	    end;
	_ ->
	    decode_integer1(Buffer,C)
    end.

decode_integer1(Buffer,C) ->
    case VR = get_constraint(C,'ValueRange') of
	no ->
	    decode_unconstrained_number(Buffer);
	{Lb, 'MAX'} ->
	    decode_semi_constrained_number(Buffer,Lb);
	{_,_} ->
	    decode_constrained_number(Buffer,VR)
    end.

%% X.691:10.6 Encoding of a normally small non-negative whole number
%% Use this for encoding of CHOICE index if there is an extension marker in 
%% the CHOICE
encode_small_number({Name,Val}) when is_atom(Name) ->
    encode_small_number(Val);
encode_small_number(Val) when Val =< 63 ->
    <<Val:7>>;
encode_small_number(Val) ->
    [<<1:1>>,encode_semi_constrained_number(0,Val)].

decode_small_number(Bytes) ->
    {Bit,Bytes2} = getbit(Bytes),
    case Bit of
	0 -> 
	    getbits(Bytes2,6);
	1 ->
	    decode_semi_constrained_number(Bytes2,0)
    end.

%% X.691:10.7 Encoding of a semi-constrained whole number
%% might be an optimization encode_semi_constrained_number(0,Val) ->
encode_semi_constrained_number(C,{Name,Val}) when is_atom(Name) ->
    encode_semi_constrained_number(C,Val);
encode_semi_constrained_number({Lb,'MAX'},Val) ->
    encode_semi_constrained_number(Lb,Val);
encode_semi_constrained_number(Lb,Val) ->
    %% encoding in minimum no of octets preceeded by a length
    Val2 = Val - Lb,
%%    NumBits = num_bits(Val2),
    Bin = eint_bin_positive(Val2),
    Size = size(Bin),
    if 
	Size < 128 ->
	    [<<Size>>,Bin]; % equiv with encode_length(undefined,Len) but faster
	Size < 16384 ->
	    [<<2:2,Size:14>>,Bin];
	true ->
	    [encode_length(undefined,Size),Bin]
    end.

decode_semi_constrained_number(Bytes,{Lb,_}) ->
    decode_semi_constrained_number(Bytes,Lb);
decode_semi_constrained_number(Bytes,Lb) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {V,Bytes3} = getoctets(Bytes2,Len),
    {V+Lb,Bytes3}.

encode_constrained_number(Range,{Name,Val}) when is_atom(Name) ->
    encode_constrained_number(Range,Val);
encode_constrained_number({Lb,Ub},Val) when Val >= Lb, Ub >= Val -> 
    Range = Ub - Lb + 1,
    Val2 = Val - Lb,
    NumBits = num_bits(Range),
    <<Val2:NumBits>>;
encode_constrained_number(Range,Val) -> 
    exit({error,{asn1,{integer_range,Range,value,Val}}}).


decode_constrained_number(Buffer,{Lb,Ub}) ->
    Range = Ub - Lb + 1,
    NumBits = num_bits(Range),
    {Val,Remain} = getbits(Buffer,NumBits),
    {Val+Lb,Remain}.

%% X.691:10.8 Encoding of an unconstrained whole number

encode_unconstrained_number(Val) when Val >= 0 ->
    Oct = eint_bin_2Cs(Val),
    Len = size(Oct),
    if 
	Len < 128 ->
	    [<<Len>>,Oct]; % equiv with encode_length(undefined,Len) but faster
	Len < 16384 ->
	    [<<2:2,Len:14>>,Oct];
	true ->
	    [encode_length(undefined,Len),<<Len:16>>,Oct]
    end;
encode_unconstrained_number(Val) -> % negative
    Oct = enint(Val,[]),
    Len = size(Oct),
    if 
	Len < 128 ->
	    [<<Len>>,Oct]; % equiv with encode_length(undefined,Len) but faster
	Len < 16384 ->
	    [<<2:2,Len:14>>,Oct];
	true ->
	    [encode_length(undefined,Len),Oct]
    end.


eint_bin_2Cs(Int) ->
    case eint_bin_positive(Int) of
	Bin = <<B,_/binary>> when B > 16#7f ->
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
    list_to_binary([eint_bin_positive2(Val bsr 32)|<<Val:32>>]).
eint_bin_positive2(Val) when Val < 16#100  ->
    <<Val>>;
eint_bin_positive2(Val) when Val < 16#10000 ->
    <<Val:16>>;
eint_bin_positive2(Val) when Val < 16#1000000 ->
    <<Val:24>>;
eint_bin_positive2(Val) when Val < 16#100000000 ->
    <<Val:32>>;
eint_bin_positive2(Val) ->
    [eint_bin_positive2(Val bsr 32)|<<Val:32>>].




enint(-1, [B1|T]) when B1 > 127 ->
    list_to_binary([B1|T]);
enint(N, Acc) ->
    enint(N bsr 8, [N band 16#ff|Acc]).

decode_unconstrained_number(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {Ints,Bytes3} = getoctets_as_bin(Bytes2,Len),
    {dec_integer(Ints),Bytes3}.

dec_integer(Bin = <<0:1,_:7,_/bitstring>>) ->  
    decpint(Bin);
dec_integer(<<_:1,B:7,BitStr/bitstring>>) ->
    Size = bit_size(BitStr),
    <<I:Size>> = BitStr,
    (-128 + B) bsl bit_size(BitStr) bor I.
    
decpint(Bin) ->
    Size = bit_size(Bin),
    <<Int:Size>> = Bin,
    Int.


%% X.691:10.9 Encoding of a length determinant
%%encode_small_length(undefined,Len) -> % null means no UpperBound
%%    encode_small_number(Len).

%% X.691:10.9.3.5 
%% X.691:10.9.3.7
encode_length(undefined,Len) -> % un-constrained
    if 
	Len < 128 ->
	    <<Len>>;
	Len < 16384 ->
	    <<2:2,Len:14>>;
	true  -> % should be able to endode length >= 16384
	    exit({error,{asn1,{encode_length,{nyi,above_16k}}}})
    end;

encode_length({0,'MAX'},Len) ->
    encode_length(undefined,Len);
encode_length(Vr={Lb,Ub},Len) when Ub =< 65535 ,Lb >= 0 -> % constrained
    encode_constrained_number(Vr,Len);
encode_length({Lb,_Ub},Len) when is_integer(Lb), Lb >= 0 -> % Ub > 65535
    encode_length(undefined,Len);
encode_length({Vr={Lb,Ub},Ext},Len) 
  when Ub =< 65535 ,Lb >= 0, Len=<Ub, is_list(Ext) -> 
    %% constrained extensible
    [<<0:1>>,encode_constrained_number(Vr,Len)];
encode_length({{Lb,_Ub},Ext},Len) when is_list(Ext) ->
    [<<1:1>>,encode_semi_constrained_number(Lb,Len)];
encode_length(SingleValue,_Len) when is_integer(SingleValue) ->
    [].

%% X.691 10.9.3.4 (only used for length of bitmap that prefixes extension 
%% additions in a sequence or set
encode_small_length(Len) when Len =< 64 ->
    <<(Len-1):7>>;
encode_small_length(Len) ->
    [<<1:1>>,encode_length(undefined,Len)].


decode_small_length(Buffer) ->
    case getbit(Buffer) of
	{0,Remain} -> 
	    {Bits,Remain2} = getbits(Remain,6),
	    {Bits+1,Remain2};
	{1,Remain} -> 
	    decode_length(Remain,undefined)
    end.

decode_length(Buffer) ->
    decode_length(Buffer,undefined).

%% un-constrained
decode_length(<<0:1,Oct:7,Rest/bitstring>>,undefined)  ->
    {Oct,Rest};
decode_length(<<2:2,Val:14,Rest/bitstring>>,undefined)  ->
    {Val,Rest};
decode_length(<<3:2,_:14,_Rest/bitstring>>,undefined)  ->
    exit({error,{asn1,{decode_length,{nyi,above_16k}}}});

decode_length(Buffer,{Lb,Ub}) when Ub =< 65535 ,Lb >= 0 -> % constrained
    decode_constrained_number(Buffer,{Lb,Ub});
decode_length(Buffer,{Lb,_}) when is_integer(Lb), Lb >= 0 -> % Ub > 65535
    decode_length(Buffer,undefined);
decode_length(Buffer,{VR={_Lb,_Ub},Ext}) when is_list(Ext) ->
    {0,Buffer2} = getbit(Buffer),
    decode_length(Buffer2, VR);
	

%When does this case occur with {_,_Lb,Ub} ??
% X.691:10.9.3.5 
decode_length(Bin,{_,_Lb,_Ub}) -> %when Len =< 127 -> % Unconstrained or large Ub NOTE! this case does not cover case when Ub > 65535
    case Bin of
	<<0:1,Val:7,Rest/bitstring>> -> 
	    {Val,Rest};
	<<2:2,Val:14,Rest/bitstring>> -> 
	    {Val,Rest};
	<<3:2,_:14,_Rest/bitstring>> -> 
	    exit({error,{asn1,{decode_length,{nyi,length_above_64K}}}})
    end;
decode_length(Buffer,SingleValue) when is_integer(SingleValue) ->
    {SingleValue,Buffer}.


						% X.691:11
encode_boolean(true) ->
    <<1:1>>;
encode_boolean(false) ->
    <<0:1>>;
encode_boolean({Name,Val}) when is_atom(Name) ->
    encode_boolean(Val);
encode_boolean(Val) ->
    exit({error,{asn1,{encode_boolean,Val}}}).

decode_boolean(Buffer) -> %when record(Buffer,buffer)
    case getbit(Buffer) of
	{1,Remain} -> {true,Remain};
	{0,Remain} -> {false,Remain}
    end.


%% ENUMERATED with extension marker
decode_enumerated(Buffer,C,{Ntup1,Ntup2}) when is_tuple(Ntup1), is_tuple(Ntup2) ->
    {Ext,Buffer2} = getext(Buffer),
    case Ext of
	0 -> % not an extension value
	    {Val,Buffer3} = decode_integer(Buffer2,C),
	    case catch (element(Val+1,Ntup1)) of
		NewVal when is_atom(NewVal) -> {NewVal,Buffer3};
		_Error -> exit({error,{asn1,{decode_enumerated,{Val,[Ntup1,Ntup2]}}}})
	    end;
	1 -> % this an extension value
	    {Val,Buffer3} = decode_small_number(Buffer2),
	    case catch (element(Val+1,Ntup2)) of
		NewVal when is_atom(NewVal) -> {NewVal,Buffer3};
		_ -> {{asn1_enum,Val},Buffer3}
	    end
    end;

decode_enumerated(Buffer,C,NamedNumberTup) when is_tuple(NamedNumberTup) ->
    {Val,Buffer2} = decode_integer(Buffer,C),
    case catch (element(Val+1,NamedNumberTup)) of
	NewVal when is_atom(NewVal) -> {NewVal,Buffer2};
	_Error -> exit({error,{asn1,{decode_enumerated,{Val,NamedNumberTup}}}})
    end.


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
%% C is constraint Len, only valid when identifiers


%% when the value is a list of {Unused,BinBits}, where 
%% Unused = integer(),
%% BinBits = binary().

encode_bit_string(C,Bin={Unused,BinBits},NamedBitList) when is_integer(Unused),
							    is_binary(BinBits) ->
    encode_bin_bit_string(get_constraint(C,'SizeConstraint'),Bin,NamedBitList);

encode_bit_string(C, BitListVal, NamedBitList) ->
    encode_bit_string1(get_constraint(C,'SizeConstraint'), BitListVal, NamedBitList).
%% when the value is a list of named bits
encode_bit_string1(C, LoNB=[FirstVal | _RestVal], NamedBitList) when is_atom(FirstVal) ->
    ToSetPos = get_all_bitposes(LoNB, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string1(C,BitList,NamedBitList);

encode_bit_string1(C, BL=[{bit,_No} | _RestVal], NamedBitList) ->
    ToSetPos = get_all_bitposes(BL, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string1(C,BitList,NamedBitList);
%% when the value is a list of ones and zeroes
encode_bit_string1(Int, BitListValue, _) 
  when is_list(BitListValue),is_integer(Int) ->
    %% The type is constrained by a single value size constraint
    bit_list2bitstr(Int,BitListValue);
encode_bit_string1(no, BitListValue,[]) 
  when is_list(BitListValue) ->
    Len = length(BitListValue),
    [encode_length(undefined,Len),bit_list2bitstr(Len,BitListValue)];
encode_bit_string1(C, BitListValue,[]) 
  when is_list(BitListValue) ->
    Len = length(BitListValue),
    [encode_length(C,Len),bit_list2bitstr(Len,BitListValue)];
encode_bit_string1(no, BitListValue,_NamedBitList) 
  when is_list(BitListValue) ->
    %% this case with an unconstrained BIT STRING can be made more efficient
    %% if the complete driver can take a special code so the length field
    %% is encoded there.
    NewBitLVal = lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,
					    lists:reverse(BitListValue))),
    Len = length(NewBitLVal),
    [encode_length(undefined,Len),bit_list2bitstr(Len,NewBitLVal)];
encode_bit_string1(C,BitListValue,_NamedBitList) 
  when is_list(BitListValue) ->% C = {_,'MAX'}
    NewBitStr = bitstr_trailing_zeros(BitListValue,C),
    [encode_length(C,bit_size(NewBitStr)),NewBitStr];


%% when the value is an integer
encode_bit_string1(C, IntegerVal, NamedBitList) when is_integer(IntegerVal)->
    BitList = int_to_bitlist(IntegerVal),
    encode_bit_string1(C,BitList,NamedBitList);

%% when the value is a tuple
encode_bit_string1(C,{Name,Val}, NamedBitList) when is_atom(Name) ->
    encode_bit_string1(C,Val,NamedBitList).

bit_list2bitstr(Len,BitListValue) ->
    case length(BitListValue) of
	Len ->
	    << <<B:1>> ||B <- BitListValue>>;
	L when L > Len -> % truncate
	    << << <<B:1>> ||B <- BitListValue>> :Len/bitstring>>;
	L -> % Len > L -> pad
	    << << <<B:1>> ||B <- BitListValue>>/bitstring ,0:(Len-L)>>
	end.

adjust_trailing_zeros(Len,Bin) when Len == bit_size(Bin) ->
    Bin;
adjust_trailing_zeros(Len,Bin) when Len > bit_size(Bin) ->
    <<Bin/bitstring,0:(Len-bit_size(Bin))>>;
adjust_trailing_zeros(Len,Bin) ->
    <<Bin:Len/bitstring>>.

bitstr_trailing_zeros(BitList,C) when is_integer(C) ->
    bitstr_trailing_zeros1(BitList,C,C);
bitstr_trailing_zeros(BitList,{Lb,Ub}) when is_integer(Lb) ->
    bitstr_trailing_zeros1(BitList,Lb,Ub);
bitstr_trailing_zeros(BitList,{{Lb,Ub},_}) when is_integer(Lb) ->
    bitstr_trailing_zeros1(BitList,Lb,Ub);
bitstr_trailing_zeros(BitList,_) ->
    bit_list2bitstr(length(BitList),BitList).

bitstr_trailing_zeros1(BitList,Lb,Ub) ->
    case length(BitList) of
	Lb -> bit_list2bitstr(Lb,BitList);
	B when B<Lb -> bit_list2bitstr(Lb,BitList);
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
encode_bin_bit_string(C,{_,BinBits},_NamedBitList)
  when is_integer(C),C=<16 ->
    adjust_trailing_zeros(C,BinBits);
encode_bin_bit_string(C,{_Unused,BinBits},_NamedBitList)
  when is_integer(C) ->
    adjust_trailing_zeros(C,BinBits);
encode_bin_bit_string(C,UnusedAndBin={_,_},NamedBitList) ->
    %% removes all trailing bits if NamedBitList is not empty
    BitStr = remove_trailing_bin(NamedBitList,UnusedAndBin),
    case C of
	{Lb,Ub} when is_integer(Lb),is_integer(Ub) ->
	    [encode_length({Lb,Ub},bit_size(BitStr)),BitStr];
	no ->
	    [encode_length(undefined,bit_size(BitStr)),BitStr];
	Sc -> 
	    [encode_length(Sc,bit_size(BitStr)),BitStr]
    end.


remove_trailing_bin([], {Unused,Bin}) ->
    BS = bit_size(Bin)-Unused,
    <<BitStr:BS/bitstring,_:Unused>> = Bin,
    BitStr;
remove_trailing_bin(_NamedNumberList,{_Unused,<<>>}) ->
    <<>>;
remove_trailing_bin(NamedNumberList, {_Unused,Bin}) ->
    Size = size(Bin)-1,
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
%% The result is presented as a list of named bits (if possible)
%% else as a tuple {Unused,Bits}. Unused is the number of unused
%% bits, least significant bits in the last byte of Bits. Bits is
%% the BIT STRING represented as a binary.
%% 
decode_compact_bit_string(Buffer, C, NamedNumberList) ->
    case get_constraint(C,'SizeConstraint') of
	0 -> % fixed length
	    {{8,0},Buffer};
	V when is_integer(V),V=<16 -> %fixed length 16 bits or less
	    compact_bit_string(Buffer,V,NamedNumberList);
	V when is_integer(V),V=<65536 -> %fixed length > 16 bits
	    compact_bit_string(Buffer,V,NamedNumberList);
	V when is_integer(V) -> % V > 65536 => fragmented value
	    {Bin,Buffer2} = decode_fragmented_bits(Buffer,V),
	    PadLen = (8 - (bit_size(Bin) rem 8)) rem 8,
	    {{PadLen,<<Bin/bitstring,0:PadLen>>},Buffer2};
%% 		{0,_} -> {{0,Bin},Buffer2};
%% 		{U,_} -> {{8-U,Bin},Buffer2}
	{Lb,Ub} when is_integer(Lb),is_integer(Ub) ->
	    %% This case may demand decoding of fragmented length/value
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    compact_bit_string(Bytes2,Len,NamedNumberList);
	no ->
	    %% This case may demand decoding of fragmented length/value
	    {Len,Bytes2} = decode_length(Buffer,undefined),
	    compact_bit_string(Bytes2,Len,NamedNumberList);
	Sc ->
	    {Len,Bytes2} = decode_length(Buffer,Sc),
	    compact_bit_string(Bytes2,Len,NamedNumberList)
    end.


%%%%%%%%%%%%%%%
%% The result is presented as a list of named bits (if possible)
%% else as a list of 0 and 1.
%% 
decode_bit_string(Buffer, C, NamedNumberList) ->
    case get_constraint(C,'SizeConstraint') of
	{Lb,Ub} when is_integer(Lb),is_integer(Ub) ->
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    bit_list_or_named(Bytes2,Len,NamedNumberList);
	no ->
	    {Len,Bytes2} = decode_length(Buffer,undefined),
	    bit_list_or_named(Bytes2,Len,NamedNumberList);
	0 -> % fixed length
	    {[],Buffer}; % nothing to encode
	V when is_integer(V),V=<16 -> % fixed length 16 bits or less
	    bit_list_or_named(Buffer,V,NamedNumberList);
	V when is_integer(V),V=<65536 ->
	    bit_list_or_named(Buffer,V,NamedNumberList);
	V when is_integer(V) ->
	    {BinBits,_} = decode_fragmented_bits(Buffer,V),
	    bit_list_or_named(BinBits,V,NamedNumberList);
	Sc -> % extension marker
	    {Len,Bytes2} = decode_length(Buffer,Sc),
	    bit_list_or_named(Bytes2,Len,NamedNumberList)
    end.


%% if no named bits are declared we will return a
%% {Unused,Bits}. Unused = integer(),
%% Bits = binary().
compact_bit_string(Buffer,Len,[]) ->
    {BitStr,Rest} = getbits_as_binary(Len,Buffer), % {{Unused,BinBits},NewBuffer}
    PadLen = (8 - (bit_size(BitStr) rem 8)) rem 8,
    {{PadLen,<<BitStr/bitstring,0:PadLen>>},Rest};
compact_bit_string(Buffer,Len,NamedNumberList) ->
    bit_list_or_named(Buffer,Len,NamedNumberList).


%% if no named bits are declared we will return a
%% BitList = [0 | 1]

bit_list_or_named(Buffer,Len,[]) ->
    getbits_as_list(Len,Buffer);

%% if there are named bits declared we will return a named
%% BitList where the names are atoms and unnamed bits represented
%% as {bit,Pos}
%% BitList = [atom() | {bit,Pos}]
%% Pos = integer()

bit_list_or_named(Buffer,Len,NamedNumberList) ->
    {BitList,Rest} = getbits_as_list(Len,Buffer),
    {bit_list_or_named1(0,BitList,NamedNumberList,[]), Rest}.

bit_list_or_named1(Pos,[0|Bt],Names,Acc) ->
    bit_list_or_named1(Pos+1,Bt,Names,Acc);
bit_list_or_named1(Pos,[1|Bt],Names,Acc) ->
    case lists:keysearch(Pos,2,Names) of
	{value,{Name,_}} ->
	    bit_list_or_named1(Pos+1,Bt,Names,[Name|Acc]);
	_  -> 
	    bit_list_or_named1(Pos+1,Bt,Names,[{bit,Pos}|Acc])
    end;
bit_list_or_named1(_,[],_,Acc) ->
    lists:reverse(Acc).



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
    case lists:keysearch(Val, 1, NamedBitList) of
	{value, {_ValName, ValPos}} ->
	    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]);
	_ ->
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
%% encode_octet_string(Constraint,Val)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_octet_string(C,{_Name,Val}) ->
    encode_octet_string(C,Val);
encode_octet_string(C,Val) ->
    case get_constraint(C,'SizeConstraint') of
	0 ->
	    <<>>;
	1 ->
	    list_to_binary(Val);
	2 ->
	    list_to_binary(Val);
	Sv when Sv =<65535, Sv == length(Val) -> % fixed length
	    list_to_binary(Val);
	VR = {_,_}  ->
	    [encode_length(VR,length(Val)),list_to_binary(Val)];
	Sv when is_list(Sv) ->
	    [encode_length({hd(Sv),lists:max(Sv)},length(Val)),list_to_binary(Val)];
	no  ->
	    [encode_length(undefined,length(Val)),list_to_binary(Val)]
    end.

decode_octet_string(Bytes,C) ->
    decode_octet_string1(Bytes,get_constraint(C,'SizeConstraint')).
decode_octet_string1(<<B1,Bytes/bitstring>>,1) ->
    {[B1],Bytes};
decode_octet_string1(<<B1,B2,Bytes/bitstring>>,2) ->
    {[B1,B2],Bytes};
decode_octet_string1(Bytes,Sv) when is_integer(Sv),Sv=<65535 ->
    getoctets_as_list(Bytes,Sv);
decode_octet_string1(Bytes,Sv) when is_integer(Sv) ->
    decode_fragmented_octets(Bytes,Sv);
decode_octet_string1(Bytes,{Lb,Ub}) ->
    {Len,Bytes2} = decode_length(Bytes,{Lb,Ub}),
    getoctets_as_list(Bytes2,Len);
decode_octet_string1(Bytes,Sv) when is_list(Sv) ->
    {Len,Bytes2} = decode_length(Bytes,{hd(Sv),lists:max(Sv)}),
    getoctets_as_list(Bytes2,Len);
decode_octet_string1(Bytes,no) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_list(Bytes2,Len).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Restricted char string types 
%% (NumericString, PrintableString,VisibleString,IA5String,BMPString,UniversalString)
%% X.691:26 and X.680:34-36
%%encode_restricted_string('BMPString',Constraints,Extension,Val)


encode_restricted_string({Name,Val}) when is_atom(Name) ->
    encode_restricted_string(Val);

encode_restricted_string(Val) when is_list(Val)->
    [encode_length(undefined,length(Val)),list_to_binary(Val)].

encode_known_multiplier_string(StringType,C,{Name,Val}) when is_atom(Name) ->
    encode_known_multiplier_string(StringType,C,Val);

encode_known_multiplier_string(StringType,C,Val) ->
    Result = chars_encode(C,StringType,Val),
    NumBits = get_NumBits(C,StringType),
    case get_constraint(C,'SizeConstraint') of
	Ub when is_integer(Ub), Ub*NumBits =< 16  ->
	    Result;
	0 ->
	    [];
	Ub when is_integer(Ub),Ub =<65535 -> % fixed length
	    Result;
	{Ub,Lb} ->
	    [encode_length({Ub,Lb},length(Val)),Result];
	Vl when is_list(Vl) ->
	    [encode_length({lists:min(Vl),lists:max(Vl)},length(Val)),Result];
	no  ->
	    [encode_length(undefined,length(Val)),Result]
    end.

decode_restricted_string(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_list(Bytes2,Len).

decode_known_multiplier_string(Bytes,StringType,C,_Ext) ->
    NumBits = get_NumBits(C,StringType),
    case get_constraint(C,'SizeConstraint') of
	Ub when is_integer(Ub), Ub*NumBits =< 16  ->
	    chars_decode(Bytes,NumBits,StringType,C,Ub);
	Ub when is_integer(Ub),Ub =<65535 -> % fixed length
	    chars_decode(Bytes,NumBits,StringType,C,Ub);
	0 ->
	    {[],Bytes};
	Vl when is_list(Vl) ->
	    {Len,Bytes1} = decode_length(Bytes,{hd(Vl),lists:max(Vl)}),
	    chars_decode(Bytes1,NumBits,StringType,C,Len);
	no  ->
	    {Len,Bytes1} = decode_length(Bytes,undefined),
	    chars_decode(Bytes1,NumBits,StringType,C,Len);
	{Lb,Ub}->
	    {Len,Bytes1} = decode_length(Bytes,{Lb,Ub}),
	    chars_decode(Bytes1,NumBits,StringType,C,Len)
    end.


encode_NumericString(C,Val) ->
    encode_known_multiplier_string('NumericString',C,Val).
decode_NumericString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,'NumericString',C,false).

encode_PrintableString(C,Val) ->
    encode_known_multiplier_string('PrintableString',C,Val).
decode_PrintableString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,'PrintableString',C,false).

encode_VisibleString(C,Val) -> % equivalent with ISO646String
    encode_known_multiplier_string('VisibleString',C,Val).
decode_VisibleString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,'VisibleString',C,false).

encode_IA5String(C,Val) ->
    encode_known_multiplier_string('IA5String',C,Val).
decode_IA5String(Bytes,C) ->
    decode_known_multiplier_string(Bytes,'IA5String',C,false).

encode_BMPString(C,Val) ->
    encode_known_multiplier_string('BMPString',C,Val).
decode_BMPString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,'BMPString',C,false).

encode_UniversalString(C,Val) ->
    encode_known_multiplier_string('UniversalString',C,Val).
decode_UniversalString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,'UniversalString',C,false).

    
%% end of known-multiplier strings for which PER visible constraints are
%% applied

encode_GeneralString(_C,Val) ->
    encode_restricted_string(Val).
decode_GeneralString(Bytes,_C) ->
    decode_restricted_string(Bytes).

encode_GraphicString(_C,Val) ->
    encode_restricted_string(Val).
decode_GraphicString(Bytes,_C) ->
    decode_restricted_string(Bytes).

encode_ObjectDescriptor(_C,Val) ->
    encode_restricted_string(Val).
decode_ObjectDescriptor(Bytes) ->
    decode_restricted_string(Bytes).

encode_TeletexString(_C,Val) -> % equivalent with T61String
    encode_restricted_string(Val).
decode_TeletexString(Bytes,_C) ->
    decode_restricted_string(Bytes).

encode_VideotexString(_C,Val) ->
    encode_restricted_string(Val).
decode_VideotexString(Bytes,_C) ->
    decode_restricted_string(Bytes).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getBMPChars(Bytes,Len) ->{BMPcharList,RemainingBytes}
%%
getBMPChars(Bytes,1) ->
    {O1,Bytes2} = getbits(Bytes,8),
    {O2,Bytes3} = getbits(Bytes2,8),
    if 
	O1 == 0 ->
	    {[O2],Bytes3};
	true ->
	    {[{0,0,O1,O2}],Bytes3}
    end;
getBMPChars(Bytes,Len) ->
    getBMPChars(Bytes,Len,[]).

getBMPChars(Bytes,0,Acc) ->
    {lists:reverse(Acc),Bytes};
getBMPChars(Bytes,Len,Acc) ->
    {Octs,Bytes1} = getoctets_as_list(Bytes,2),
    case Octs of
	[0,O2] ->
	    getBMPChars(Bytes1,Len-1,[O2|Acc]);
	[O1,O2]->
	    getBMPChars(Bytes1,Len-1,[{0,0,O1,O2}|Acc])
    end.


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
    %%[{bits,NumBits,H-Min}|chars_encode2(T,NumBits,{Min,Max,notab})];
    [<<(H-Min):NumBits>>|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([H|T],NumBits,{Min,Max,Tab}) when H =< Max, H >= Min ->
%%    [{bits,NumBits,exit_if_false(H,element(H-Min+1,Tab))}|chars_encode2(T,NumBits,{Min,Max,Tab})];
    Ch = exit_if_false(H,element(H-Min+1,Tab)),
    [<<Ch:NumBits>>|chars_encode2(T,NumBits,{Min,Max,Tab})];
chars_encode2([{A,B,C,D}|T],NumBits,{Min,Max,notab}) -> 
    %% no value range check here (ought to be, but very expensive)
%%    [{bits,NumBits,((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min}|chars_encode2(T,NumBits,{Min,Max,notab})];
    Ch = ((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min,
    [<<Ch:NumBits>>|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([{A,B,C,D}|T],NumBits,{Min,Max,Tab}) -> 
    %% no value range check here (ought to be, but very expensive)
%%    [{bits,NumBits,exit_if_false({A,B,C,D},element(((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min,Tab))}|chars_encode2(T,NumBits,{Min,Max,notab})];
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
    get_CharTab(C,StringType,out).

get_CharInTab(C,StringType) ->
    get_CharTab(C,StringType,in).

get_CharTab(C,StringType,InOut) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    get_CharTab2(C,StringType,hd(Sv),lists:max(Sv),Sv,InOut);
	no ->
	    case StringType of
		'IA5String' ->
		    {0,16#7F,notab};
		'VisibleString' ->
		    get_CharTab2(C,StringType,16#20,16#7F,notab,InOut);
		'PrintableString' ->
		    Chars = lists:sort(
			      " '()+,-./0123456789:=?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
		    get_CharTab2(C,StringType,hd(Chars),lists:max(Chars),Chars,InOut);
		'NumericString' ->
		    get_CharTab2(C,StringType,16#20,$9," 0123456789",InOut);
		'UniversalString' ->
		    {0,16#FFFFFFFF,notab};
		'BMPString' ->
		    {0,16#FFFF,notab}
	    end
    end.

get_CharTab2(C,StringType,Min,Max,Chars,InOut) ->
    BitValMax = (1 bsl get_NumBits(C,StringType))-1,
    if
	Max =< BitValMax ->
	    {0,Max,notab};
	true ->
	    case InOut of
		out ->
		    {Min,Max,create_char_tab(Min,Chars)};
		in  ->
		    {Min,Max,list_to_tuple(Chars)}
	    end
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


chars_decode(Bytes,_,'BMPString',C,Len) ->
    case get_constraint(C,'PermittedAlphabet') of
	no ->
	    getBMPChars(Bytes,Len);
	_ ->
	    exit({error,{asn1,
			 {'not implemented',
			  "BMPString with PermittedAlphabet constraint"}}})
    end;
chars_decode(Bytes,NumBits,StringType,C,Len) ->	
    CharInTab = get_CharInTab(C,StringType),
    chars_decode2(Bytes,CharInTab,NumBits,Len).


chars_decode2(Bytes,CharInTab,NumBits,Len) ->
    chars_decode2(Bytes,CharInTab,NumBits,Len,[]).

chars_decode2(Bytes,_CharInTab,_NumBits,0,Acc) ->
    {lists:reverse(Acc),Bytes};
chars_decode2(Bytes,{Min,Max,notab},NumBits,Len,Acc) when NumBits > 8 ->
    {Char,Bytes2} = getbits(Bytes,NumBits),
    Result = 
	if
	    Char < 256 -> Char;
	    true ->
		list_to_tuple(binary_to_list(<<Char:32>>))
	end,
    chars_decode2(Bytes2,{Min,Max,notab},NumBits,Len -1,[Result|Acc]);
chars_decode2(Bytes,{Min,Max,notab},NumBits,Len,Acc) ->
    {Char,Bytes2} = getbits(Bytes,NumBits),
    chars_decode2(Bytes2,{Min,Max,notab},NumBits,Len -1,[Char+Min|Acc]);

%% BMPString and UniversalString with PermittedAlphabet is currently not supported
chars_decode2(Bytes,{Min,Max,CharInTab},NumBits,Len,Acc) ->
    {Char,Bytes2} = getbits(Bytes,NumBits),
    chars_decode2(Bytes2,{Min,Max,CharInTab},NumBits,Len -1,[element(Char+1,CharInTab)|Acc]).


%% UTF8String
encode_UTF8String(Val) when is_binary(Val) ->
    [encode_length(undefined,size(Val)),Val];
encode_UTF8String(Val) ->
    Bin = list_to_binary(Val),
    encode_UTF8String(Bin).

decode_UTF8String(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_bin(Bytes2,Len).
    

						% X.691:17 
encode_null(_) -> []. % encodes to nothing

decode_null(Bytes) ->
    {'NULL',Bytes}.

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
encode_object_identifier({Name,Val}) when is_atom(Name) ->
    encode_object_identifier(Val);
encode_object_identifier(Val) ->
    OctetList = e_object_identifier(Val),
    Octets = list_to_binary(OctetList), % performs a flatten at the same time
    [encode_length(undefined,size(Octets)),Octets].

%% This code is copied from asn1_encode.erl (BER) and corrected and modified 

e_object_identifier({'OBJECT IDENTIFIER',V}) ->
    e_object_identifier(V);
e_object_identifier({Cname,V}) when is_atom(Cname),is_tuple(V) ->
    e_object_identifier(tuple_to_list(V));
e_object_identifier({Cname,V}) when is_atom(Cname),is_list(V) ->
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_object_identifier(Bytes) -> {ObjId,RemainingBytes}
%% ObjId -> {integer(),integer(),...} % at least 2 integers
%% RemainingBytes -> [integer()] when integer() (0..255)
decode_object_identifier(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {Octs,Bytes3} = getoctets_as_list(Bytes2,Len),
    [First|Rest] = dec_subidentifiers(Octs,0,[]),
    Idlist = if
		 First < 40 ->
		     [0,First|Rest];
		 First < 80 ->
		     [1,First - 40|Rest];
		 true ->
		     [2,First - 80|Rest]
	     end,
    {list_to_tuple(Idlist),Bytes3}.

dec_subidentifiers([H|T],Av,Al) when H >=16#80 ->
    dec_subidentifiers(T,(Av bsl 7) + (H band 16#7F),Al);
dec_subidentifiers([H|T],Av,Al) ->
    dec_subidentifiers(T,0,[(Av bsl 7) + H |Al]);
dec_subidentifiers([],_Av,Al) ->
    lists:reverse(Al).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_relative_oid(Val) -> CompleteList
%% encode_relative_oid({Name,Val}) -> CompleteList
encode_relative_oid({Name,Val}) when is_atom(Name) ->
    encode_relative_oid(Val);
encode_relative_oid(Val) when is_tuple(Val) ->
    encode_relative_oid(tuple_to_list(Val));
encode_relative_oid(Val) when is_list(Val) ->
    Octets = list_to_binary([e_object_element(X)||X <- Val]),
    [encode_length(undefined,size(Octets)),Octets].
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_relative_oid(Val) -> CompleteList
%% decode_relative_oid({Name,Val}) -> CompleteList
decode_relative_oid(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {Octs,Bytes3} = getoctets_as_list(Bytes2,Len),
    ObjVals = dec_subidentifiers(Octs,0,[]),
    {list_to_tuple(ObjVals),Bytes3}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_real(Val) -> CompleteList
%% encode_real({Name,Val}) -> CompleteList
encode_real({Name,Val}) when is_atom(Name) ->
    encode_real(Val);
encode_real(Real) ->
    {EncVal,Len} = ?RT_COMMON:encode_real([],Real),
    [encode_length(undefined,Len),EncVal].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_real(Val) -> {REALvalue,Rest}
%% decode_real({Name,Val}) -> {REALvalue,Rest}
decode_real(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    <<Bytes3:Len/binary,Rest/bitstring>> = Bytes2,
    {RealVal,Rest,Len} = ?RT_COMMON:decode_real(Bytes3,Len),
    {RealVal,Rest}.


get_constraint([{Key,V}],Key) ->
    V;
get_constraint([],_Key) ->
    no;
get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	    no;
	{value,{_,V}} -> 
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
num_bits(1) -> 0;
num_bits(2) -> 1;
num_bits(R) when R =< 4 -> 
    2;
num_bits(R) when R =< 8 ->
    3;
num_bits(R) when R =< 16 ->
    4;
num_bits(R) when R =< 32 ->
    5;	
num_bits(R) when R =< 64 ->
    6;
num_bits(R) when R =< 128 ->
    7;
num_bits(R) when R =< 256 ->
    8;
num_bits(R) when R =< 512 ->
    9;
num_bits(R) when R =< 1024 ->
    10;
num_bits(R) ->
    1+num_bits(R bsr 1).
