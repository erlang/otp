%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1rt_per_bin_rt2ct.erl,v 1.1 2008/12/17 09:53:31 mikpe Exp $
-module(asn1rt_per_bin_rt2ct).

%% encoding / decoding of PER aligned

-include("asn1_records.hrl").

-export([dec_fixup/3, cindex/3, list_to_record/2]).
-export([setchoiceext/1, setext/1, fixoptionals/3, fixextensions/2,
	 getext/1, getextension/2, skipextensions/3, getbit/1, getchoice/3 ]).
-export([getoptionals/2, getoptionals2/2,
	 set_choice/3, encode_integer/2, encode_integer/3  ]).
-export([decode_integer/2, decode_integer/3, encode_small_number/1,
	 decode_boolean/1, encode_length/2, decode_length/1, decode_length/2,
	 encode_small_length/1, decode_small_length/1,
	 decode_compact_bit_string/3]).
-export([decode_enumerated/3,
	 encode_bit_string/3, decode_bit_string/3  ]).
-export([encode_octet_string/2, decode_octet_string/2,
	 encode_null/1, decode_null/1,
	 encode_object_identifier/1, decode_object_identifier/1,
	 complete/1]).


-export([encode_open_type/2, decode_open_type/2]).

-export([%encode_UniversalString/2, decode_UniversalString/2,
	 %encode_PrintableString/2, decode_PrintableString/2,
	 encode_GeneralString/2, decode_GeneralString/2,
	 encode_GraphicString/2, decode_GraphicString/2,
	 encode_TeletexString/2, decode_TeletexString/2,
	 encode_VideotexString/2, decode_VideotexString/2,
	 %encode_VisibleString/2, decode_VisibleString/2,
	 %encode_BMPString/2, decode_BMPString/2,
	 %encode_IA5String/2, decode_IA5String/2,
	 %encode_NumericString/2, decode_NumericString/2,
	 encode_ObjectDescriptor/2, decode_ObjectDescriptor/1
	]).

-export([decode_constrained_number/2,
	 decode_constrained_number/3,
	 decode_unconstrained_number/1,
	 decode_semi_constrained_number/2,
	 encode_unconstrained_number/1,
	 decode_constrained_number/4,
	 encode_octet_string/3,
	 decode_octet_string/3,
	 encode_known_multiplier_string/5,
	 decode_known_multiplier_string/5,
	 getoctets/2, getbits/2
%	 start_drv/1,start_drv2/1,init_drv/1
	]).


-export([eint_positive/1]).
-export([pre_complete_bits/2]).

-define('16K',16384).
-define('32K',32768).
-define('64K',65536).

%%-define(nodriver,true).

dec_fixup(Terms,Cnames,RemBytes) ->
    dec_fixup(Terms,Cnames,RemBytes,[]).

dec_fixup([novalue|T],[_Hc|Tc],RemBytes,Acc) ->
    dec_fixup(T,Tc,RemBytes,Acc);
dec_fixup([{_Name,novalue}|T],[_Hc|Tc],RemBytes,Acc) ->
    dec_fixup(T,Tc,RemBytes,Acc);
dec_fixup([H|T],[Hc|Tc],RemBytes,Acc) ->
    dec_fixup(T,Tc,RemBytes,[{Hc,H}|Acc]);
dec_fixup([],_Cnames,RemBytes,Acc) ->
    {lists:reverse(Acc),RemBytes}.

cindex(Ix,Val,Cname) ->
    case element(Ix,Val) of
	{Cname,Val2} -> Val2;
	X -> X
    end.

%% converts a list to a record if necessary
list_to_record(_,Tuple) when tuple(Tuple) ->
    Tuple;
list_to_record(Name,List) when list(List) ->
    list_to_tuple([Name|List]).

%%--------------------------------------------------------
%% setchoiceext(InRootSet) -> [{bit,X}]
%% X  is set to  1 when InRootSet==false
%% X  is set to  0 when InRootSet==true
%%
setchoiceext(true) ->
%    [{debug,choiceext},{bits,1,0}];
    [0];
setchoiceext(false) ->
%    [{debug,choiceext},{bits,1,1}].
    [1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% setext(true|false) ->  CompleteList
%%

setext(false) ->
%    [{debug,ext},{bits,1,0}];
    [0];
setext(true) ->
%    [{debug,ext},{bits,1,1}];
    [1].

fixoptionals(OptList,_OptLength,Val) when tuple(Val) ->
%    Bits = fixoptionals(OptList,Val,0),
%    {Val,{bits,OptLength,Bits}};
%    {Val,[10,OptLength,Bits]};
    {Val,fixoptionals(OptList,Val,[])};

fixoptionals([],_,Acc) ->
    %% Optbits
    lists:reverse(Acc);
fixoptionals([Pos|Ot],Val,Acc) ->
    case element(Pos,Val) of
% 	asn1_NOVALUE -> fixoptionals(Ot,Val,Acc bsl 1);
% 	asn1_DEFAULT -> fixoptionals(Ot,Val,Acc bsl 1);
% 	_ -> fixoptionals(Ot,Val,(Acc bsl 1) + 1)
	asn1_NOVALUE -> fixoptionals(Ot,Val,[0|Acc]);
	asn1_DEFAULT -> fixoptionals(Ot,Val,[0|Acc]);
	_ -> fixoptionals(Ot,Val,[1|Acc])
    end.


getext(Bytes) when tuple(Bytes) ->
    getbit(Bytes);
getext(Bytes) when binary(Bytes) ->
    getbit({0,Bytes});
getext(Bytes) when list(Bytes) ->
    getbit({0,Bytes}).

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
%	    [encode_small_length(ExtNum),{bits,ExtNum,ExtBits}]
%	    [encode_small_length(ExtNum),[10,ExtNum,ExtBits]]
	    [encode_small_length(ExtNum),pre_complete_bits(ExtNum,ExtBits)]
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

%% old version kept for backward compatibility with generates from R7B01
getoptionals(Bytes,NumOpt) ->
    {Blist,Bytes1} = getbits_as_list(NumOpt,Bytes),
    {list_to_tuple(Blist),Bytes1}.

%% new version used in generates from r8b_patch/3 and later
getoptionals2(Bytes,NumOpt) ->
    {_,_} = getbits(Bytes,NumOpt).


%% getbits_as_binary(Num,Bytes) -> {{Unused,BinBits},RestBytes},
%% Num = integer(),
%% Bytes = list() | tuple(),
%% Unused = integer(),
%% BinBits = binary(),
%% RestBytes = tuple()
getbits_as_binary(Num,Bytes) when binary(Bytes) ->
    getbits_as_binary(Num,{0,Bytes});
getbits_as_binary(0,Buffer) ->
    {{0,<<>>},Buffer};
getbits_as_binary(Num,{0,Bin}) when Num > 16 ->
    Used = Num rem 8,
    Pad = (8 - Used) rem 8,
%%    Nbytes = Num div 8,
    <<Bits:Num,_:Pad,RestBin/binary>> = Bin,
    {{Pad,<<Bits:Num,0:Pad>>},RestBin};
getbits_as_binary(Num,Buffer={_Used,_Bin}) -> % Unaligned buffer
    %% Num =< 16,
    {Bits2,Buffer2} = getbits(Buffer,Num),
    Pad = (8 - (Num rem 8)) rem 8,
    {{Pad,<<Bits2:Num,0:Pad>>},Buffer2}.


% integer_from_list(Int,[],BigInt) ->
%     BigInt;
% integer_from_list(Int,[H|T],BigInt) when Int < 8 ->
%     (BigInt bsl Int) bor (H bsr (8-Int));
% integer_from_list(Int,[H|T],BigInt) ->
%     integer_from_list(Int-8,T,(BigInt bsl 8) bor H).

getbits_as_list(Num,Bytes) when binary(Bytes) ->
    getbits_as_list(Num,{0,Bytes},[]);
getbits_as_list(Num,Bytes) ->
    getbits_as_list(Num,Bytes,[]).

%% If buffer is empty and nothing more will be picked.
getbits_as_list(0, B, Acc) ->
    {lists:reverse(Acc),B};
%% If first byte in buffer is full and at least one byte will be picked,
%% then pick one byte.
getbits_as_list(N,{0,Bin},Acc) when N >= 8 ->
    <<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,Rest/binary>> = Bin,
    getbits_as_list(N-8,{0,Rest},[B0,B1,B2,B3,B4,B5,B6,B7|Acc]);
getbits_as_list(N,{Used,Bin},Acc) when N >= 4, Used =< 4 ->
    NewUsed = Used + 4,
    Rem = 8 - NewUsed,
    <<_:Used,B3:1,B2:1,B1:1,B0:1,_:Rem, Rest/binary>> = Bin,
    NewRest = case Rem of 0 -> Rest; _ -> Bin end,
    getbits_as_list(N-4,{NewUsed rem 8,NewRest},[B0,B1,B2,B3|Acc]);
getbits_as_list(N,{Used,Bin},Acc) when N >= 2, Used =< 6  ->
    NewUsed = Used + 2,
    Rem = 8 - NewUsed,
    <<_:Used,B1:1,B0:1,_:Rem, Rest/binary>> = Bin,
    NewRest = case Rem of 0 -> Rest; _ -> Bin end,
    getbits_as_list(N-2,{NewUsed rem 8,NewRest},[B0,B1|Acc]);
getbits_as_list(N,{Used,Bin},Acc) when Used =< 7 ->
    NewUsed = Used + 1,
    Rem = 8 - NewUsed,
    <<_:Used,B0:1,_:Rem, Rest/binary>> = Bin,
    NewRest = case Rem of 0 -> Rest; _ -> Bin end,
    getbits_as_list(N-1,{NewUsed rem 8,NewRest},[B0|Acc]).


getbit({7,<<_:7,B:1,Rest/binary>>}) ->
    {B,{0,Rest}};
getbit({0,Buffer = <<B:1,_:7,_/binary>>}) ->
    {B,{1,Buffer}};
getbit({Used,Buffer}) ->
    Unused = (8 - Used) - 1,
    <<_:Used,B:1,_:Unused,_/binary>> = Buffer,
    {B,{Used+1,Buffer}};
getbit(Buffer) when binary(Buffer) ->
    getbit({0,Buffer}).


getbits({0,Buffer},Num) when (Num rem 8) == 0 ->
    <<Bits:Num,Rest/binary>> = Buffer,
    {Bits,{0,Rest}};
getbits({Used,Bin},Num) ->
    NumPlusUsed = Num + Used,
    NewUsed = NumPlusUsed rem 8,
    Unused = (8-NewUsed) rem 8,
    case Unused of
	0 ->
	    <<_:Used,Bits:Num,Rest/binary>> = Bin,
	    {Bits,{0,Rest}};
	_ ->
	    Bytes = NumPlusUsed div 8,
	    <<_:Used,Bits:Num,_:Unused,_/binary>> = Bin,
	    <<_:Bytes/binary,Rest/binary>> = Bin,
	    {Bits,{NewUsed,Rest}}
    end;
getbits(Bin,Num) when binary(Bin) ->
    getbits({0,Bin},Num).



% getoctet(Bytes) when list(Bytes) ->
%     getoctet({0,Bytes});
% getoctet(Bytes) ->
%     %%    io:format("getoctet:Buffer = ~p~n",[Bytes]),
%     getoctet1(Bytes).

% getoctet1({0,[H|T]}) ->
%     {H,{0,T}};
% getoctet1({Pos,[_,H|T]}) ->
%     {H,{0,T}}.

align({0,L}) ->
    {0,L};
align({_Pos,<<_H,T/binary>>}) ->
    {0,T};
align(Bytes) ->
    {0,Bytes}.

%% First align buffer, then pick the first Num octets.
%% Returns octets as an integer with bit significance as in buffer.
getoctets({0,Buffer},Num) ->
    <<Val:Num/integer-unit:8,RestBin/binary>> = Buffer,
    {Val,{0,RestBin}};
getoctets({U,<<_Padding,Rest/binary>>},Num) when U /= 0 ->
    getoctets({0,Rest},Num);
getoctets(Buffer,Num) when binary(Buffer) ->
    getoctets({0,Buffer},Num).
% getoctets(Buffer,Num) ->
%     %%    io:format("getoctets:Buffer = ~p~nNum = ~p~n",[Buffer,Num]),
%     getoctets(Buffer,Num,0).

% getoctets(Buffer,0,Acc) ->
%     {Acc,Buffer};
% getoctets(Buffer,Num,Acc) ->
%     {Oct,NewBuffer} = getoctet(Buffer),
%     getoctets(NewBuffer,Num-1,(Acc bsl 8)+Oct).

% getoctets_as_list(Buffer,Num) ->
%     getoctets_as_list(Buffer,Num,[]).

% getoctets_as_list(Buffer,0,Acc) ->
%     {lists:reverse(Acc),Buffer};
% getoctets_as_list(Buffer,Num,Acc) ->
%     {Oct,NewBuffer} = getoctet(Buffer),
%     getoctets_as_list(NewBuffer,Num-1,[Oct|Acc]).

%% First align buffer, then pick the first Num octets.
%% Returns octets as a binary
getoctets_as_bin({0,Bin},Num)->
    <<Octets:Num/binary,RestBin/binary>> = Bin,
    {Octets,{0,RestBin}};
getoctets_as_bin({_U,Bin},Num) ->
    <<_Padding,Octets:Num/binary,RestBin/binary>> = Bin,
    {Octets,{0,RestBin}};
getoctets_as_bin(Bin,Num) when binary(Bin) ->
    getoctets_as_bin({0,Bin},Num).

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
	N when integer(N), Len1 > 1 ->
%	    [{bits,1,0}, % the value is in the root set
%	     encode_constrained_number({0,Len1-1},N)];
	    [0, % the value is in the root set
	     encode_constrained_number({0,Len1-1},N)];
	N when integer(N) ->
%	    [{bits,1,0}]; % no encoding if only 0 or 1 alternative
	    [0]; % no encoding if only 0 or 1 alternative
	false ->
%	    [{bits,1,1}, % extension value
	    [1, % extension value
	     case set_choice_tag(Alt,L2) of
		 N2 when integer(N2) ->
		     encode_small_number(N2);
		 false ->
		     unknown_choice_alt
	     end]
    end;
set_choice(Alt,L,Len) ->
    case set_choice_tag(Alt,L) of
	N when integer(N), Len > 1 ->
	    encode_constrained_number({0,Len-1},N);
	N when integer(N) ->
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
decode_fragmented_bits({0,Buffer},C) ->
    decode_fragmented_bits(Buffer,C,[]);
decode_fragmented_bits({_N,<<_B,Bs/binary>>},C) ->
    decode_fragmented_bits(Bs,C,[]).

decode_fragmented_bits(<<3:2,Len:6,Bin/binary>>,C,Acc) ->
    {Value,Bin2} = split_binary(Bin, Len * ?'16K'),
    decode_fragmented_bits(Bin2,C,[Value,Acc]);
decode_fragmented_bits(<<0:1,0:7,Bin/binary>>,C,Acc) ->
    BinBits = list_to_binary(lists:reverse(Acc)),
    case C of
	Int when integer(Int),C == size(BinBits) ->
	    {BinBits,{0,Bin}};
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinBits}}});
	_ ->
	    {BinBits,{0,Bin}}
    end;
decode_fragmented_bits(<<0:1,Len:7,Bin/binary>>,C,Acc) ->
    Result = {BinBits,{Used,_Rest}} =
	case (Len rem 8) of
	    0 ->
		<<Value:Len/binary-unit:1,Bin2/binary>> = Bin,
		{list_to_binary(lists:reverse([Value|Acc])),{0,Bin2}};
	    Rem ->
		Bytes = Len div 8,
		U = 8 - Rem,
		<<Value:Bytes/binary-unit:8,Bits1:Rem,Bits2:U,Bin2/binary>> = Bin,
		{list_to_binary(lists:reverse([Bits1 bsl U,Value|Acc])),
		 {Rem,<<Bits2,Bin2/binary>>}}
	end,
    case C of
	 Int when integer(Int),C == (size(BinBits) - ((8 - Used) rem 8)) ->
	    Result;
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinBits}}});
	_ ->
	    Result
    end.


decode_fragmented_octets({0,Bin},C) ->
    decode_fragmented_octets(Bin,C,[]);
decode_fragmented_octets({_N,<<_B,Bs/binary>>},C) ->
    decode_fragmented_octets(Bs,C,[]).

decode_fragmented_octets(<<3:2,Len:6,Bin/binary>>,C,Acc) ->
    {Value,Bin2} = split_binary(Bin,Len * ?'16K'),
    decode_fragmented_octets(Bin2,C,[Value,Acc]);
decode_fragmented_octets(<<0:1,0:7,Bin/binary>>,C,Acc) ->
    Octets = list_to_binary(lists:reverse(Acc)),
    case C of
	Int when integer(Int), C == size(Octets) ->
	    {Octets,{0,Bin}};
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,Octets}}});
	_ ->
	    {Octets,{0,Bin}}
    end;
decode_fragmented_octets(<<0:1,Len:7,Bin/binary>>,C,Acc) ->
    <<Value:Len/binary-unit:8,Bin2/binary>> = Bin,
    BinOctets = list_to_binary(lists:reverse([Value|Acc])),
    case C of
	Int when integer(Int),size(BinOctets) == Int ->
	    {BinOctets,Bin2};
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinOctets}}});
	_ ->
	    {BinOctets,Bin2}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Constraint, Value) -> CompleteList
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary
%% Contraint = not used in this version
%%
encode_open_type(_Constraint, Val) when list(Val) ->
    Bin = list_to_binary(Val),
    case size(Bin) of
	Size when Size>255 ->
	    [encode_length(undefined,Size),[21,<<Size:16>>,Bin]];
	Size ->
	    [encode_length(undefined,Size),[20,Size,Bin]]
    end;
%    [encode_length(undefined,size(Bin)),{octets,Bin}]; % octets implies align
encode_open_type(_Constraint, Val) when binary(Val) ->
%    [encode_length(undefined,size(Val)),{octets,Val}]. % octets implies align
    case size(Val) of
	Size when Size>255 ->
	    [encode_length(undefined,size(Val)),[21,<<Size:16>>,Val]]; % octets implies align
	Size ->
	    [encode_length(undefined,Size),[20,Size,Val]]
    end.
%% the binary_to_list is not optimal but compatible with the current solution

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Buffer,Constraint) -> Value
%% Constraint is not used in this version
%% Buffer = [byte] with PER encoded data
%% Value = [byte] with decoded data (which must be decoded again as some type)
%%
decode_open_type(Bytes, _Constraint) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_bin(Bytes2,Len).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_integer(Constraint,Value,NamedNumberList) -> CompleteList
%% encode_integer(Constraint,Value) -> CompleteList
%% encode_integer(Constraint,{Name,Value}) -> CompleteList
%%
%%
encode_integer(C,V,NamedNumberList) when atom(V) ->
    case lists:keysearch(V,1,NamedNumberList) of
	{value,{_,NewV}} ->
	    encode_integer(C,NewV);
	_ ->
	    exit({error,{asn1,{namednumber,V}}})
    end;
encode_integer(C,V,_NamedNumberList) when integer(V) ->
    encode_integer(C,V);
encode_integer(C,{Name,V},NamedNumberList) when atom(Name) ->
    encode_integer(C,V,NamedNumberList).

encode_integer(C,{Name,Val}) when atom(Name) ->
    encode_integer(C,Val);

encode_integer([{Rc,_Ec}],Val) when tuple(Rc) -> % XXX when is this invoked? First argument most often a list,...Ok this is the extension case...but it doesn't work.
    case (catch encode_integer([Rc],Val)) of
	{'EXIT',{error,{asn1,_}}} ->
%	    [{bits,1,1},encode_unconstrained_number(Val)];
	    [1,encode_unconstrained_number(Val)];
	Encoded ->
%	    [{bits,1,0},Encoded]
	    [0,Encoded]
    end;

encode_integer([],Val) ->
    encode_unconstrained_number(Val);
%% The constraint is the effective constraint, and in this case is a number
encode_integer([{'SingleValue',V}],V) ->
    [];
encode_integer([{'ValueRange',VR={Lb,Ub},Range,PreEnc}],Val) when Val >= Lb,
						Ub >= Val ->
    %% this case when NamedNumberList
    encode_constrained_number(VR,Range,PreEnc,Val);
encode_integer([{'ValueRange',{Lb,'MAX'}}],Val) ->
    encode_semi_constrained_number(Lb,Val);
encode_integer([{'ValueRange',{'MIN',_}}],Val) ->
    encode_unconstrained_number(Val);
encode_integer([{'ValueRange',VR={_Lb,_Ub}}],Val) ->
    encode_constrained_number(VR,Val);
encode_integer(_,Val) ->
    exit({error,{asn1,{illegal_value,Val}}}).



decode_integer(Buffer,Range,NamedNumberList) ->
    {Val,Buffer2} = decode_integer(Buffer,Range),
    case lists:keysearch(Val,2,NamedNumberList) of
	{value,{NewVal,_}} -> {NewVal,Buffer2};
	_ -> {Val,Buffer2}
    end.

decode_integer(Buffer,[{Rc,_Ec}]) when tuple(Rc) ->
    {Ext,Buffer2} = getext(Buffer),
    case Ext of
	0 -> decode_integer(Buffer2,[Rc]);
	1 -> decode_unconstrained_number(Buffer2)
    end;
decode_integer(Buffer,undefined) ->
    decode_unconstrained_number(Buffer);
decode_integer(Buffer,C) ->
    case get_constraint(C,'SingleValue') of
	V when integer(V) ->
	    {V,Buffer};
	_ ->
	    decode_integer1(Buffer,C)
    end.

decode_integer1(Buffer,C) ->
    case VR = get_constraint(C,'ValueRange') of
	no ->
	    decode_unconstrained_number(Buffer);
	{Lb, 'MAX'} ->
	    decode_semi_constrained_number(Buffer,Lb);
	{_Lb,_Ub} ->
	    decode_constrained_number(Buffer,VR)
    end.

%% X.691:10.6 Encoding of a normally small non-negative whole number
%% Use this for encoding of CHOICE index if there is an extension marker in
%% the CHOICE
encode_small_number({Name,Val}) when atom(Name) ->
    encode_small_number(Val);
encode_small_number(Val) when Val =< 63 ->
%    [{bits,1,0},{bits,6,Val}];
%    [{bits,7,Val}]; % same as above but more efficient
    [10,7,Val]; % same as above but more efficient
encode_small_number(Val) ->
%    [{bits,1,1},encode_semi_constrained_number(0,Val)].
    [1,encode_semi_constrained_number(0,Val)].

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
encode_semi_constrained_number(C,{Name,Val}) when atom(Name) ->
    encode_semi_constrained_number(C,Val);
encode_semi_constrained_number({Lb,'MAX'},Val) ->
    encode_semi_constrained_number(Lb,Val);
encode_semi_constrained_number(Lb,Val) ->
    Val2 = Val - Lb,
    Oct = eint_positive(Val2),
    Len = length(Oct),
    if
	Len < 128 ->
	    %{octets,[Len|Oct]}; % equiv with encode_length(undefined,Len) but faster
	    [20,Len+1,[Len|Oct]];
	Len < 256 ->
	    [encode_length(undefined,Len),[20,Len,Oct]];
	true ->
	    [encode_length(undefined,Len),[21,<<Len:16>>,Oct]]
    end.

decode_semi_constrained_number(Bytes,{Lb,_}) ->
    decode_semi_constrained_number(Bytes,Lb);
decode_semi_constrained_number(Bytes,Lb) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {V,Bytes3} = getoctets(Bytes2,Len),
    {V+Lb,Bytes3}.

encode_constrained_number({Lb,_Ub},_Range,{bits,N},Val) ->
    Val2 = Val-Lb,
%    {bits,N,Val2};
    [10,N,Val2];
encode_constrained_number({Lb,_Ub},_Range,{octets,N},Val) when N < 256->
    %% N is 8 or 16 (1 or 2 octets)
    Val2 = Val-Lb,
%    {octets,<<Val2:N/unit:8>>};
    [20,N,Val2];
encode_constrained_number({Lb,_Ub},_Range,{octets,N},Val) -> % N>255
    %% N is 8 or 16 (1 or 2 octets)
    Val2 = Val-Lb,
%    {octets,<<Val2:N/unit:8>>};
    [21,<<N:16>>,Val2];
encode_constrained_number({Lb,_Ub},Range,_,Val) ->
    Val2 = Val-Lb,
    if
	Range =< 16#1000000  -> % max 3 octets
	    Octs = eint_positive(Val2),
%	    [encode_length({1,3},size(Octs)),{octets,Octs}];
	    L = length(Octs),
	    [encode_length({1,3},L),[20,L,Octs]];
	Range =< 16#100000000  -> % max 4 octets
	    Octs = eint_positive(Val2),
%	    [encode_length({1,4},size(Octs)),{octets,Octs}];
	    L = length(Octs),
	    [encode_length({1,4},L),[20,L,Octs]];
	Range =< 16#10000000000  -> % max 5 octets
	    Octs = eint_positive(Val2),
%	    [encode_length({1,5},size(Octs)),{octets,Octs}];
	    L = length(Octs),
	    [encode_length({1,5},L),[20,L,Octs]];
	true  ->
	    exit({not_supported,{integer_range,Range}})
    end.

encode_constrained_number(Range,{Name,Val}) when atom(Name) ->
    encode_constrained_number(Range,Val);
encode_constrained_number({Lb,Ub},Val) when Val >= Lb, Ub >= Val ->
    Range = Ub - Lb + 1,
    Val2 = Val - Lb,
    if
	Range  == 2 ->
%	    Size = {bits,1,Val2};
	    [Val2];
	Range  =< 4 ->
%	    Size = {bits,2,Val2};
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
%	    Size = {octets,[Val2]};
	    [20,1,Val2];
	Range  =< 65536 ->
%	    Size = {octets,<<Val2:16>>};
	    [20,2,<<Val2:16>>];
	Range =< 16#1000000  ->
	    Octs = eint_positive(Val2),
%	    [{bits,2,length(Octs)-1},{octets,Octs}];
	    Len = length(Octs),
	    [10,2,Len-1,20,Len,Octs];
	Range =< 16#100000000  ->
	    Octs = eint_positive(Val2),
	    Len = length(Octs),
	    [10,2,Len-1,20,Len,Octs];
	Range =< 16#10000000000  ->
	    Octs = eint_positive(Val2),
	    Len = length(Octs),
	    [10,3,Len-1,20,Len,Octs];
	true  ->
	    exit({not_supported,{integer_range,Range}})
    end;
encode_constrained_number({_,_},Val) ->
    exit({error,{asn1,{illegal_value,Val}}}).

decode_constrained_number(Buffer,VR={Lb,Ub}) ->
    Range = Ub - Lb + 1,
    decode_constrained_number(Buffer,VR,Range).

decode_constrained_number(Buffer,{Lb,_Ub},_Range,{bits,N}) ->
    {Val,Remain} = getbits(Buffer,N),
    {Val+Lb,Remain};
decode_constrained_number(Buffer,{Lb,_Ub},_Range,{octets,N}) ->
    {Val,Remain} = getoctets(Buffer,N),
    {Val+Lb,Remain}.

decode_constrained_number(Buffer,{Lb,_Ub},Range) ->
						%    Val2 = Val - Lb,
    {Val,Remain} =
	if
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
	    Range =< 16#1000000  ->
		{Len,Bytes2} = decode_length(Buffer,{1,3}),
		{Octs,Bytes3} = getoctets_as_list(Bytes2,Len),
		{dec_pos_integer(Octs),Bytes3};
	    Range =< 16#100000000  ->
		{Len,Bytes2} = decode_length(Buffer,{1,4}),
		{Octs,Bytes3} = getoctets_as_list(Bytes2,Len),
		{dec_pos_integer(Octs),Bytes3};
	    Range =< 16#10000000000  ->
		{Len,Bytes2} = decode_length(Buffer,{1,5}),
		{Octs,Bytes3} = getoctets_as_list(Bytes2,Len),
		{dec_pos_integer(Octs),Bytes3};
	    true  ->
		exit({not_supported,{integer_range,Range}})
	end,
    {Val+Lb,Remain}.

%% X.691:10.8 Encoding of an unconstrained whole number

encode_unconstrained_number(Val) when Val >= 0 ->
    Oct = eint(Val,[]),
    Len = length(Oct),
    if
	Len < 128 ->
	    %{octets,[Len|Oct]}; % equiv with encode_length(undefined,Len) but faster
	    [20,Len+1,[Len|Oct]];
	Len < 256 ->
%	    [encode_length(undefined,Len),20,Len,Oct];
	    [20,Len+2,<<2:2,Len:14>>,Oct];% equiv with encode_length(undefined,Len) but faster
	true ->
%	    [encode_length(undefined,Len),{octets,Oct}]
	    [encode_length(undefined,Len),[21,<<Len:16>>,Oct]]
    end;
encode_unconstrained_number(Val) -> % negative
    Oct = enint(Val,[]),
    Len = length(Oct),
    if
	Len < 128 ->
%	    {octets,[Len|Oct]}; % equiv with encode_length(undefined,Len) but faster
	    [20,Len+1,[Len|Oct]];% equiv with encode_length(undefined,Len) but faster
	Len < 256 ->
%	    [encode_length(undefined,Len),20,Len,Oct];
	    [20,Len+2,<<2:2,Len:14>>,Oct];% equiv with encode_length(undefined,Len) but faster
	true ->
	    %[encode_length(undefined,Len),{octets,Oct}]
	    [encode_length(undefined,Len),[21,<<Len:16>>,Oct]]
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

decode_unconstrained_number(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {Ints,Bytes3} = getoctets_as_list(Bytes2,Len),
    {dec_integer(Ints),Bytes3}.

dec_pos_integer(Ints) ->
    decpint(Ints, 8 * (length(Ints) - 1)).
dec_integer(Ints) when hd(Ints) band 255 =< 127 -> %% Positive number
    decpint(Ints, 8 * (length(Ints) - 1));
dec_integer(Ints) ->                        %% Negative
    decnint(Ints,  8 * (length(Ints) - 1)).

decpint([Byte|Tail], Shift) ->
    (Byte bsl Shift) bor decpint(Tail, Shift-8);
decpint([], _) -> 0.

decnint([Byte|Tail], Shift) ->
    (-128 + (Byte band 127) bsl Shift) bor decpint(Tail, Shift-8).

% minimum_octets(Val) ->
%     minimum_octets(Val,[]).

% minimum_octets(Val,Acc) when Val > 0 ->
%     minimum_octets((Val bsr 8),[Val band 16#FF|Acc]);
% minimum_octets(0,Acc) ->
%     Acc.


%% X.691:10.9 Encoding of a length determinant
%%encode_small_length(undefined,Len) -> % null means no UpperBound
%%    encode_small_number(Len).

%% X.691:10.9.3.5
%% X.691:10.9.3.7
encode_length(undefined,Len) -> % un-constrained
    if
	Len < 128 ->
%	    {octets,[Len]};
	    [20,1,Len];
	Len < 16384 ->
	    %{octets,<<2:2,Len:14>>};
	    [20,2,<<2:2,Len:14>>];
	true  -> % should be able to endode length >= 16384 i.e. fragmented length
	    exit({error,{asn1,{encode_length,{nyi,above_16k}}}})
    end;

encode_length({0,'MAX'},Len) ->
    encode_length(undefined,Len);
encode_length(Vr={Lb,Ub},Len) when Ub =< 65535 ,Lb >= 0 -> % constrained
    encode_constrained_number(Vr,Len);
encode_length({Lb,_Ub},Len) when integer(Lb), Lb >= 0 -> % Ub > 65535
    encode_length(undefined,Len);
encode_length({Vr={Lb,Ub},[]},Len) when Ub =< 65535 ,Lb >= 0,Len=<Ub ->
    %% constrained extensible
%    [{bits,1,0},encode_constrained_number(Vr,Len)];
    [0,encode_constrained_number(Vr,Len)];
encode_length({{Lb,_},[]},Len) ->
    [1,encode_semi_constrained_number(Lb,Len)];
encode_length(SingleValue,_Len) when integer(SingleValue) ->
    [].

%% X.691 10.9.3.4 (only used for length of bitmap that prefixes extension
%% additions in a sequence or set
encode_small_length(Len) when Len =< 64 ->
%%    [{bits,1,0},{bits,6,Len-1}];
%    {bits,7,Len-1}; % the same as above but more efficient
    [10,7,Len-1];
encode_small_length(Len) ->
%    [{bits,1,1},encode_length(undefined,Len)].
    [1,encode_length(undefined,Len)].

% decode_small_length({Used,<<_:Used,0:1,Num:6,_:((8-Used+1) rem 8),Rest/binary>>}) ->
%     case Buffer of
% 	<<_:Used,0:1,Num:6,_:((8-Used+1) rem 8),Rest/binary>> ->
% 	    {Num,
%     case getbit(Buffer) of
% 	{0,Remain} ->
% 	    {Bits,Remain2} = getbits(Remain,6),
% 	    {Bits+1,Remain2};
% 	{1,Remain} ->
% 	    decode_length(Remain,undefined)
%     end.

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

decode_length(Buffer,undefined)  -> % un-constrained
    {0,Buffer2} = align(Buffer),
    case Buffer2 of
	<<0:1,Oct:7,Rest/binary>> ->
	    {Oct,{0,Rest}};
	<<2:2,Val:14,Rest/binary>> ->
	    {Val,{0,Rest}};
	<<3:2,_Val:14,_Rest/binary>> ->
	    %% this case should be fixed
	    exit({error,{asn1,{decode_length,{nyi,above_16k}}}})
    end;
%%    {Bits,_} = getbits(Buffer2,2),
%     case Bits of
% 	2 ->
% 	    {Val,Bytes3} = getoctets(Buffer2,2),
% 	    {(Val band 16#3FFF),Bytes3};
% 	3 ->
% 	    exit({error,{asn1,{decode_length,{nyi,above_16k}}}});
% 	_ ->
% 	    {Val,Bytes3} = getoctet(Buffer2),
% 	    {Val band 16#7F,Bytes3}
%     end;

decode_length(Buffer,{Lb,Ub}) when Ub =< 65535 ,Lb >= 0 -> % constrained
    decode_constrained_number(Buffer,{Lb,Ub});
decode_length(_Buffer,{Lb,_Ub}) when integer(Lb), Lb >= 0 -> % Ub > 65535
    exit({error,{asn1,{decode_length,{nyi,above_64K}}}});
decode_length(Buffer,{{Lb,Ub},[]}) ->
    case getbit(Buffer) of
	{0,Buffer2} ->
	    decode_length(Buffer2, {Lb,Ub})
    end;


%When does this case occur with {_,_Lb,Ub} ??
% X.691:10.9.3.5
decode_length({Used,Bin},{_,_Lb,_Ub}) -> %when Len =< 127 -> % Unconstrained or large Ub NOTE! this case does not cover case when Ub > 65535
    Unused = (8-Used) rem 8,
    case Bin of
	<<_:Used,0:1,Val:7,R:Unused,Rest/binary>> ->
	    {Val,{Used,<<R,Rest/binary>>}};
	<<_:Used,_:Unused,2:2,Val:14,Rest/binary>> ->
	    {Val, {0,Rest}};
	<<_:Used,_:Unused,3:2,_:14,_Rest/binary>> ->
	    exit({error,{asn1,{decode_length,{nyi,length_above_64K}}}})
    end;
% decode_length(Buffer,{_,_Lb,Ub}) -> %when Len =< 127 -> % Unconstrained or large Ub
%     case getbit(Buffer) of
% 	{0,Remain} ->
% 	    getbits(Remain,7);
% 	{1,Remain} ->
% 	    {Val,Remain2} = getoctets(Buffer,2),
% 	    {Val band 2#0111111111111111, Remain2}
%     end;
decode_length(Buffer,SingleValue) when integer(SingleValue) ->
    {SingleValue,Buffer}.


						% X.691:11
decode_boolean(Buffer) -> %when record(Buffer,buffer)
    case getbit(Buffer) of
	{1,Remain} -> {true,Remain};
	{0,Remain} -> {false,Remain}
    end.


%% ENUMERATED with extension marker
decode_enumerated(Buffer,C,{Ntup1,Ntup2}) when tuple(Ntup1), tuple(Ntup2) ->
    {Ext,Buffer2} = getext(Buffer),
    case Ext of
	0 -> % not an extension value
	    {Val,Buffer3} = decode_integer(Buffer2,C),
	    case catch (element(Val+1,Ntup1)) of
		NewVal when atom(NewVal) -> {NewVal,Buffer3};
		_Error -> exit({error,{asn1,{decode_enumerated,{Val,[Ntup1,Ntup2]}}}})
	    end;
	1 -> % this an extension value
	    {Val,Buffer3} = decode_small_number(Buffer2),
	    case catch (element(Val+1,Ntup2)) of
		NewVal when atom(NewVal) -> {NewVal,Buffer3};
		_ -> {{asn1_enum,Val},Buffer3}
	    end
    end;

decode_enumerated(Buffer,C,NamedNumberTup) when tuple(NamedNumberTup) ->
    {Val,Buffer2} = decode_integer(Buffer,C),
    case catch (element(Val+1,NamedNumberTup)) of
	NewVal when atom(NewVal) -> {NewVal,Buffer2};
	_Error -> exit({error,{asn1,{decode_enumerated,{Val,NamedNumberTup}}}})
    end.

%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Bitstring value, ITU_T X.690 Chapter 8.5
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% encode bitstring value
%%===============================================================================



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bitstring NamedBitList
%% Val can be  of:
%% - [identifiers] where only named identifiers are set to one,
%%   the Constraint must then have some information of the
%%   bitlength.
%% - [list of ones and zeroes] all bits
%% - integer value representing the bitlist
%% C is constraint Len, only valid when identifiers


%% when the value is a list of {Unused,BinBits}, where
%% Unused = integer(),
%% BinBits = binary().

encode_bit_string(C,Bin={Unused,BinBits},NamedBitList) when integer(Unused),
							    binary(BinBits) ->
    encode_bin_bit_string(C,Bin,NamedBitList);

%% when the value is a list of named bits

encode_bit_string(C, LoNB=[FirstVal | _RestVal], NamedBitList) when atom(FirstVal) ->
    ToSetPos = get_all_bitposes(LoNB, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);% consider the constraint

encode_bit_string(C, BL=[{bit,_} | _RestVal], NamedBitList) ->
    ToSetPos = get_all_bitposes(BL, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);

%% when the value is a list of ones and zeroes
encode_bit_string(Int, BitListValue, _)
  when list(BitListValue),integer(Int) ->
    %% The type is constrained by a single value size constraint
    [40,Int,length(BitListValue),BitListValue];
% encode_bit_string(C, BitListValue,NamedBitList)
%   when list(BitListValue) ->
%     [encode_bit_str_length(C,BitListValue),
%      2,45,BitListValue];
encode_bit_string(no, BitListValue,[])
  when list(BitListValue) ->
    [encode_length(undefined,length(BitListValue)),
     2,BitListValue];
encode_bit_string(C, BitListValue,[])
  when list(BitListValue) ->
    [encode_length(C,length(BitListValue)),
     2,BitListValue];
encode_bit_string(no, BitListValue,_NamedBitList)
  when list(BitListValue) ->
    %% this case with an unconstrained BIT STRING can be made more efficient
    %% if the complete driver can take a special code so the length field
    %% is encoded there.
    NewBitLVal = lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,
					    lists:reverse(BitListValue))),
    [encode_length(undefined,length(NewBitLVal)),
     2,NewBitLVal];
encode_bit_string(C,BitListValue,_NamedBitList)
  when list(BitListValue) ->% C = {_,'MAX'}
%     NewBitLVal = lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,
% 					    lists:reverse(BitListValue))),
    NewBitLVal = bit_string_trailing_zeros(BitListValue,C),
    [encode_length(C,length(NewBitLVal)),
     2,NewBitLVal];

% encode_bit_string(C, BitListValue, NamedBitList) when list(BitListValue) ->
%     BitListToBinary =
% 	%% fun that transforms a list of 1 and 0 to a tuple:
% 	%% {UnusedBitsInLastByte, Binary}
% 	fun([H|T],Acc,N,Fun) ->
% 		Fun(T,(Acc bsl 1)+H,N+1,Fun);
% 	   ([],Acc,N,_) -> % length fits in one byte
% 		Unused = (8 - (N rem 8)) rem 8,
% % 		case N/8 of
% % 		    _Len =< 255 ->
% % 			[30,Unused,(Unused+N)/8,<<Acc:N,0:Unused>>];
% % 		    _Len ->
% % 			Len = (Unused+N)/8,
% % 			[31,Unused,<<Len:16>>,<<Acc:N,0:Unused>>]
% % 		end
% 		{Unused,<<Acc:N,0:Unused>>}
% 	end,
%     UnusedAndBin =
% 	case NamedBitList of
% 	    [] ->  % dont remove trailing zeroes
% 		BitListToBinary(BitListValue,0,0,BitListToBinary);
% 	    _ ->
% 		BitListToBinary(lists:reverse(
% 				  lists:dropwhile(fun(0)->true;(1)->false end,
% 						  lists:reverse(BitListValue))),
% 				0,0,BitListToBinary)
% 	end,
%     encode_bin_bit_string(C,UnusedAndBin,NamedBitList);

%% when the value is an integer
encode_bit_string(C, IntegerVal, NamedBitList) when integer(IntegerVal)->
    BitList = int_to_bitlist(IntegerVal),
    encode_bit_string(C,BitList,NamedBitList);

%% when the value is a tuple
encode_bit_string(C,{Name,Val}, NamedBitList) when atom(Name) ->
    encode_bit_string(C,Val,NamedBitList).

bit_string_trailing_zeros(BitList,C) when integer(C) ->
    bit_string_trailing_zeros1(BitList,C,C);
bit_string_trailing_zeros(BitList,{Lb,Ub}) when integer(Lb) ->
    bit_string_trailing_zeros1(BitList,Lb,Ub);
bit_string_trailing_zeros(BitList,{{Lb,Ub},_}) when integer(Lb) ->
    bit_string_trailing_zeros1(BitList,Lb,Ub);
bit_string_trailing_zeros(BitList,_) ->
    BitList.

bit_string_trailing_zeros1(BitList,Lb,Ub) ->
    case length(BitList) of
	Lb -> BitList;
	B when B<Lb -> BitList++lists:duplicate(Lb-B,0);
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
encode_bin_bit_string(C,{_,BinBits},_NamedBitList)
  when integer(C),C=<16 ->
    [45,C,size(BinBits),BinBits];
encode_bin_bit_string(C,{_Unused,BinBits},_NamedBitList)
  when integer(C) ->
    [2,45,C,size(BinBits),BinBits];
encode_bin_bit_string(C,UnusedAndBin={_,_},NamedBitList) ->
%    UnusedAndBin1 = {Unused1,Bin1} =
    {Unused1,Bin1} =
	%% removes all trailing bits if NamedBitList is not empty
	remove_trailing_bin(NamedBitList,UnusedAndBin),
    case C of
%    case get_constraint(C,'SizeConstraint') of

% 	0 ->
% 	    []; % should be dont in compile time
% 	V when integer(V),V=<16 ->
% 	    {Unused2,Bin2} = pad_list(V,UnusedAndBin1),
% 	    <<BitVal:V,_:Unused2>> = Bin2,
% %	    {bits,V,BitVal};
% 	    [10,V,BitVal];
% 	V when integer(V) ->
% 	    %[align, pad_list(V, UnusedAndBin1)];
% 	    {Unused2,Bin2} = pad_list(V, UnusedAndBin1),
% 	    <<BitVal:V,_:Unused2>> = Bin2,
% 	    [2,octets_unused_to_complete(Unused2,size(Bin2),Bin2)];

	{Lb,Ub} when integer(Lb),integer(Ub) ->
%	    [encode_length({Lb,Ub},size(Bin1)*8 - Unused1),
%	     align,UnusedAndBin1];
	    Size=size(Bin1),
	    [encode_length({Lb,Ub},Size*8 - Unused1),
	     2,octets_unused_to_complete(Unused1,Size,Bin1)];
	no ->
	    Size=size(Bin1),
	    [encode_length(undefined,Size*8 - Unused1),
	     2,octets_unused_to_complete(Unused1,Size,Bin1)];
	Sc ->
	    Size=size(Bin1),
	    [encode_length(Sc,Size*8 - Unused1),
	     2,octets_unused_to_complete(Unused1,Size,Bin1)]
    end.

remove_trailing_bin([], {Unused,Bin}) ->
    {Unused,Bin};
remove_trailing_bin(NamedNumberList, {_Unused,Bin}) ->
    Size = size(Bin)-1,
    <<Bfront:Size/binary, LastByte:8>> = Bin,
    %% clear the Unused bits to be sure
%    LastByte1 = LastByte band (((1 bsl Unused) -1) bxor 255),% why this???
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
%% The result is presented as a list of named bits (if possible)
%% else as a tuple {Unused,Bits}. Unused is the number of unused
%% bits, least significant bits in the last byte of Bits. Bits is
%% the BIT STRING represented as a binary.
%%
decode_compact_bit_string(Buffer, C, NamedNumberList) ->
    case get_constraint(C,'SizeConstraint') of
	0 -> % fixed length
	    {{8,0},Buffer};
	V when integer(V),V=<16 -> %fixed length 16 bits or less
	    compact_bit_string(Buffer,V,NamedNumberList);
	V when integer(V),V=<65536 -> %fixed length > 16 bits
	    Bytes2 = align(Buffer),
	    compact_bit_string(Bytes2,V,NamedNumberList);
	V when integer(V) -> % V > 65536 => fragmented value
	    {Bin,Buffer2} = decode_fragmented_bits(Buffer,V),
	    case Buffer2 of
		{0,_} -> {{0,Bin},Buffer2};
		{U,_} -> {{8-U,Bin},Buffer2}
	    end;
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    %% This case may demand decoding of fragmented length/value
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    Bytes3 = align(Bytes2),
	    compact_bit_string(Bytes3,Len,NamedNumberList);
	no ->
	    %% This case may demand decoding of fragmented length/value
	    {Len,Bytes2} = decode_length(Buffer,undefined),
	    Bytes3 = align(Bytes2),
	    compact_bit_string(Bytes3,Len,NamedNumberList);
	Sc ->
	    {Len,Bytes2} = decode_length(Buffer,Sc),
	    Bytes3 = align(Bytes2),
	    compact_bit_string(Bytes3,Len,NamedNumberList)
    end.


%%%%%%%%%%%%%%%
%% The result is presented as a list of named bits (if possible)
%% else as a list of 0 and 1.
%%
decode_bit_string(Buffer, C, NamedNumberList) ->
    case get_constraint(C,'SizeConstraint') of
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    Bytes3 = align(Bytes2),
	    bit_list_or_named(Bytes3,Len,NamedNumberList);
	no ->
	    {Len,Bytes2} = decode_length(Buffer,undefined),
	    Bytes3 = align(Bytes2),
	    bit_list_or_named(Bytes3,Len,NamedNumberList);
	0 -> % fixed length
	    {[],Buffer}; % nothing to encode
	V when integer(V),V=<16 -> % fixed length 16 bits or less
	    bit_list_or_named(Buffer,V,NamedNumberList);
	V when integer(V),V=<65536 ->
	    Bytes2 = align(Buffer),
	    bit_list_or_named(Bytes2,V,NamedNumberList);
	V when integer(V) ->
	    Bytes2 = align(Buffer),
	    {BinBits,_Bytes3} = decode_fragmented_bits(Bytes2,V),
	    bit_list_or_named(BinBits,V,NamedNumberList);
	Sc -> % extension marker
	    {Len,Bytes2} = decode_length(Buffer,Sc),
	    Bytes3 = align(Bytes2),
	    bit_list_or_named(Bytes3,Len,NamedNumberList)
    end.


%% if no named bits are declared we will return a
%% {Unused,Bits}. Unused = integer(),
%% Bits = binary().
compact_bit_string(Buffer,Len,[]) ->
    getbits_as_binary(Len,Buffer); % {{Unused,BinBits},NewBuffer}
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
bit_list_or_named1(_Pos,[],_Names,Acc) ->
    lists:reverse(Acc).



%%%%%%%%%%%%%%%
%%

int_to_bitlist(Int) when integer(Int), Int > 0 ->
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

%%%%%%%%%%%%%%%%%
%% pad_list(N,BitList) -> PaddedList
%% returns a padded (with trailing {bit,0} elements) list of length N
%% if Bitlist contains more than N significant bits set an exit asn1_error
%% is generated

% pad_list(N,In={Unused,Bin}) ->
%     pad_list(N, size(Bin)*8 - Unused, In).

% pad_list(N,Size,In={Unused,Bin}) when N < Size ->
%     exit({error,{asn1,{range_error,{bit_string,In}}}});
% pad_list(N,Size,{Unused,Bin}) when N > Size, Unused > 0 ->
%     pad_list(N,Size+1,{Unused-1,Bin});
% pad_list(N,Size,{Unused,Bin}) when N > Size ->
%     pad_list(N,Size+1,{7,<<Bin/binary,0>>});
% pad_list(N,N,In={Unused,Bin}) ->
%     In.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% X.691:16
%% encode_octet_string(Constraint,ExtensionMarker,Val)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_octet_string(C,Val) ->
    encode_octet_string(C,false,Val).

encode_octet_string(C,Bool,{_Name,Val}) ->
    encode_octet_string(C,Bool,Val);
encode_octet_string(_C,true,_Val) ->
    exit({error,{asn1,{'not_supported',extensionmarker}}});
encode_octet_string(SZ={_,_},false,Val) ->
%    [encode_length(SZ,length(Val)),align,
%	     {octets,Val}];
    Len = length(Val),
    [encode_length(SZ,Len),2,
     octets_to_complete(Len,Val)];
encode_octet_string(SZ,false,Val) when list(SZ) ->
    Len = length(Val),
    [encode_length({hd(SZ),lists:max(SZ)},Len),2,
     octets_to_complete(Len,Val)];
encode_octet_string(no,false,Val) ->
    Len = length(Val),
    [encode_length(undefined,Len),2,
     octets_to_complete(Len,Val)];
encode_octet_string(C,_,_) ->
    exit({error,{not_implemented,C}}).


decode_octet_string(Bytes,Range) ->
    decode_octet_string(Bytes,Range,false).

decode_octet_string(Bytes,1,false) ->
    {B1,Bytes2} = getbits(Bytes,8),
    {[B1],Bytes2};
decode_octet_string(Bytes,2,false) ->
    {Bs,Bytes2}= getbits(Bytes,16),
    {binary_to_list(<<Bs:16>>),Bytes2};
decode_octet_string(Bytes,Sv,false) when integer(Sv),Sv=<65535 ->
    Bytes2 = align(Bytes),
    getoctets_as_list(Bytes2,Sv);
decode_octet_string(Bytes,Sv,false) when integer(Sv) ->
    Bytes2 = align(Bytes),
    decode_fragmented_octets(Bytes2,Sv);
decode_octet_string(Bytes,{Lb,Ub},false) ->
    {Len,Bytes2} = decode_length(Bytes,{Lb,Ub}),
    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes3,Len);
decode_octet_string(Bytes,Sv,false) when list(Sv) ->
    {Len,Bytes2} = decode_length(Bytes,{hd(Sv),lists:max(Sv)}),
    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes3,Len);
decode_octet_string(Bytes,no,false) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes3,Len).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Restricted char string types
%% (NumericString, PrintableString,VisibleString,IA5String,BMPString,UniversalString)
%% X.691:26 and X.680:34-36
%%encode_restricted_string(aligned,'BMPString',Constraints,Extension,Val)


encode_restricted_string(aligned,{Name,Val}) when atom(Name) ->
    encode_restricted_string(aligned,Val);

encode_restricted_string(aligned,Val) when list(Val)->
    Len = length(Val),
%    [encode_length(undefined,length(Val)),{octets,Val}].
    [encode_length(undefined,Len),octets_to_complete(Len,Val)].


encode_known_multiplier_string(StringType,SizeC,NumBits,CharOutTab,{Name,Val}) when atom(Name) ->
    encode_known_multiplier_string(StringType,SizeC,NumBits,CharOutTab,Val);
encode_known_multiplier_string(StringType,SizeC,NumBits,CharOutTab,Val) ->
    Result = chars_encode2(Val,NumBits,CharOutTab),
    case SizeC of
	Ub when integer(Ub), Ub*NumBits =< 16  ->
	    case {StringType,Result} of
		{'BMPString',{octets,Ol}} -> %% this case cannot happen !!??
		    [{bits,8,Oct}||Oct <- Ol];
		_ ->
		    Result
	    end;
	Ub when integer(Ub),Ub =<65535 -> % fixed length
%%	    [align,Result];
	    [2,Result];
	{Ub,Lb} ->
%	    [encode_length({Ub,Lb},length(Val)),align,Result];
	    [encode_length({Ub,Lb},length(Val)),2,Result];
	no  ->
%	    [encode_length(undefined,length(Val)),align,Result]
	    [encode_length(undefined,length(Val)),2,Result]
    end.

decode_restricted_string(Bytes,aligned) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_list(Bytes2,Len).

decode_known_multiplier_string(StringType,SizeC,NumBits,CharInTab,Bytes) ->
    case SizeC of
	Ub when integer(Ub), Ub*NumBits =< 16  ->
	    chars_decode(Bytes,NumBits,StringType,CharInTab,Ub);
	Ub when integer(Ub),Ub =<65535 -> % fixed length
	    Bytes1 = align(Bytes),
	    chars_decode(Bytes1,NumBits,StringType,CharInTab,Ub);
	Vl when list(Vl) ->
	    {Len,Bytes1} = decode_length(Bytes,{hd(Vl),lists:max(Vl)}),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,CharInTab,Len);
	no  ->
	    {Len,Bytes1} = decode_length(Bytes,undefined),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,CharInTab,Len);
	{Lb,Ub}->
	    {Len,Bytes1} = decode_length(Bytes,{Lb,Ub}),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,CharInTab,Len)
    end.

encode_GeneralString(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_GeneralString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).

encode_GraphicString(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_GraphicString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).

encode_ObjectDescriptor(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_ObjectDescriptor(Bytes) ->
    decode_restricted_string(Bytes,aligned).

encode_TeletexString(_C,Val) -> % equivalent with T61String
    encode_restricted_string(aligned,Val).
decode_TeletexString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).

encode_VideotexString(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_VideotexString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).




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

% chars_encode(C,StringType,Value) ->
%     case {StringType,get_constraint(C,'PermittedAlphabet')} of
% 	{'UniversalString',{_,Sv}} ->
% 	    exit({error,{asn1,{'not implemented',"UniversalString with PermittedAlphabet constraint"}}});
% 	{'BMPString',{_,Sv}} ->
% 	    exit({error,{asn1,{'not implemented',"BMPString with PermittedAlphabet constraint"}}});
% 	_ ->
% 	    {NumBits,CharOutTab} = {get_NumBits(C,StringType),get_CharOutTab(C,StringType)},
% 	    chars_encode2(Value,NumBits,CharOutTab)
%     end.


chars_encode2([H|T],NumBits,T1={Min,Max,notab}) when  H =< Max, H >= Min ->
%    [[10,NumBits,H-Min]|chars_encode2(T,NumBits,T1)];
    [pre_complete_bits(NumBits,H-Min)|chars_encode2(T,NumBits,T1)];
chars_encode2([H|T],NumBits,T1={Min,Max,Tab}) when H =< Max, H >= Min ->
%    [[10,NumBits,element(H-Min+1,Tab)]|chars_encode2(T,NumBits,T1)];
    [pre_complete_bits(NumBits,exit_if_false(H,element(H-Min+1,Tab)))|
     chars_encode2(T,NumBits,T1)];
chars_encode2([{A,B,C,D}|T],NumBits,T1={Min,_Max,notab}) ->
    %% no value range check here (ought to be, but very expensive)
%    [{bits,NumBits,(A*B*C*D)-Min}|chars_encode2(T,NumBits,{Min,Max,notab})];
%    [[10,NumBits,((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min]|chars_encode2(T,NumBits,T1)];
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
%     LBUsed = NumBits rem 8,
%     {Unused,Len} = case (8 - LBUsed) of
% 		       8 -> {0,NumBits div 8};
% 		       U -> {U,(NumBits div 8) + 1}
% 		   end,
%     NewVal = Val bsr LBUsed,
%     [30,Unused,Len,<<NewVal:Len/unit:8,Val:LBUsed,0:Unused>>].
    Unused = (8 - (NumBits rem 8)) rem 8,
    Len = NumBits + Unused,
    [30,Unused,Len div 8,<<(Val bsl Unused):Len>>].

% get_NumBits(C,StringType) ->
%     case get_constraint(C,'PermittedAlphabet') of
% 	{'SingleValue',Sv} ->
% 	    charbits(length(Sv),aligned);
% 	no ->
% 	    case StringType of
% 		'IA5String' ->
% 		    charbits(128,aligned); % 16#00..16#7F
% 		'VisibleString' ->
% 		    charbits(95,aligned); % 16#20..16#7E
% 		'PrintableString' ->
% 		    charbits(74,aligned); % [$\s,$',$(,$),$+,$,,$-,$.,$/,"0123456789",$:,$=,$?,$A..$Z,$a..$z
% 		'NumericString' ->
% 		    charbits(11,aligned); % $ ,"0123456789"
% 		'UniversalString' ->
% 		    32;
% 		'BMPString' ->
% 		    16
% 	    end
%     end.

%%Maybe used later
%%get_MaxChar(C,StringType) ->
%%    case get_constraint(C,'PermittedAlphabet') of
%%	{'SingleValue',Sv} ->
%%	    lists:nth(length(Sv),Sv);
%%	no ->
%%	    case StringType of
%%		'IA5String' ->
%%		    16#7F; % 16#00..16#7F
%%		'VisibleString' ->
%%		    16#7E; % 16#20..16#7E
%%		'PrintableString' ->
%%		    $z; % [$\s,$',$(,$),$+,$,,$-,$.,$/,"0123456789",$:,$=,$?,$A..$Z,$a..$z
%%		'NumericString' ->
%%		    $9; % $ ,"0123456789"
%%		'UniversalString' ->
%%		    16#ffffffff;
%%		'BMPString' ->
%%		    16#ffff
%%	    end
%%    end.

%%Maybe used later
%%get_MinChar(C,StringType) ->
%%    case get_constraint(C,'PermittedAlphabet') of
%%	{'SingleValue',Sv} ->
%%	    hd(Sv);
%%	no ->
%%	    case StringType of
%%		'IA5String' ->
%%		    16#00; % 16#00..16#7F
%%		'VisibleString' ->
%%		    16#20; % 16#20..16#7E
%%		'PrintableString' ->
%%		    $\s; % [$\s,$',$(,$),$+,$,,$-,$.,$/,"0123456789",$:,$=,$?,$A..$Z,$a..$z
%%		'NumericString' ->
%%		    $\s; % $ ,"0123456789"
%%		'UniversalString' ->
%%		    16#00;
%%		'BMPString' ->
%%		    16#00
%%	    end
%%    end.

% get_CharOutTab(C,StringType) ->
%     get_CharTab(C,StringType,out).

% get_CharInTab(C,StringType) ->
%     get_CharTab(C,StringType,in).

% get_CharTab(C,StringType,InOut) ->
%     case get_constraint(C,'PermittedAlphabet') of
% 	{'SingleValue',Sv} ->
% 	    get_CharTab2(C,StringType,hd(Sv),lists:max(Sv),Sv,InOut);
% 	no ->
% 	    case StringType of
% 		'IA5String' ->
% 		    {0,16#7F,notab};
% 		'VisibleString' ->
% 		    get_CharTab2(C,StringType,16#20,16#7F,notab,InOut);
% 		'PrintableString' ->
% 		    Chars = lists:sort(
% 			      " '()+,-./0123456789:=?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
% 		    get_CharTab2(C,StringType,hd(Chars),lists:max(Chars),Chars,InOut);
% 		'NumericString' ->
% 		    get_CharTab2(C,StringType,16#20,$9," 0123456789",InOut);
% 		'UniversalString' ->
% 		    {0,16#FFFFFFFF,notab};
% 		'BMPString' ->
% 		    {0,16#FFFF,notab}
% 	    end
%     end.

% get_CharTab2(C,StringType,Min,Max,Chars,InOut) ->
%     BitValMax = (1 bsl get_NumBits(C,StringType))-1,
%     if
% 	Max =< BitValMax ->
% 	    {0,Max,notab};
% 	true ->
% 	    case InOut of
% 		out ->
% 		    {Min,Max,create_char_tab(Min,Chars)};
% 		in  ->
% 		    {Min,Max,list_to_tuple(Chars)}
% 	    end
%     end.

% create_char_tab(Min,L) ->
%     list_to_tuple(create_char_tab(Min,L,0)).
% create_char_tab(Min,[Min|T],V) ->
%     [V|create_char_tab(Min+1,T,V+1)];
% create_char_tab(_Min,[],_V) ->
%     [];
% create_char_tab(Min,L,V) ->
%     [false|create_char_tab(Min+1,L,V)].

%% This very inefficient and should be moved to compiletime
% charbits(NumOfChars,aligned) ->
%     case charbits(NumOfChars) of
% 	1 -> 1;
% 	2 -> 2;
% 	B when B =< 4 -> 4;
% 	B when B =< 8 -> 8;
% 	B when B =< 16 -> 16;
% 	B when B =< 32 -> 32
%     end.

% charbits(NumOfChars) when NumOfChars =< 2 -> 1;
% charbits(NumOfChars) when NumOfChars =< 4 -> 2;
% charbits(NumOfChars) when NumOfChars =< 8 -> 3;
% charbits(NumOfChars) when NumOfChars =< 16 -> 4;
% charbits(NumOfChars) when NumOfChars =< 32 -> 5;
% charbits(NumOfChars) when NumOfChars =< 64 -> 6;
% charbits(NumOfChars) when NumOfChars =< 128 -> 7;
% charbits(NumOfChars) when NumOfChars =< 256 -> 8;
% charbits(NumOfChars) when NumOfChars =< 512 -> 9;
% charbits(NumOfChars) when NumOfChars =< 1024 -> 10;
% charbits(NumOfChars) when NumOfChars =< 2048 -> 11;
% charbits(NumOfChars) when NumOfChars =< 4096 -> 12;
% charbits(NumOfChars) when NumOfChars =< 8192 -> 13;
% charbits(NumOfChars) when NumOfChars =< 16384 -> 14;
% charbits(NumOfChars) when NumOfChars =< 32768 -> 15;
% charbits(NumOfChars) when NumOfChars =< 65536 -> 16;
% charbits(NumOfChars) when integer(NumOfChars) ->
%     16 + charbits1(NumOfChars bsr 16).

% charbits1(0) ->
%     0;
% charbits1(NumOfChars) ->
%     1 + charbits1(NumOfChars bsr 1).


chars_decode(Bytes,_,'BMPString',_,Len) ->
    getBMPChars(Bytes,Len);
chars_decode(Bytes,NumBits,_StringType,CharInTab,Len) ->
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


						% X.691:17
encode_null(_Val) -> []; % encodes to nothing
encode_null({Name,Val}) when atom(Name) ->
    encode_null(Val).

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
%% CompleteList -> [{bits,8,Val}|{octets,Ol}|align|...]
%%
encode_object_identifier({Name,Val}) when atom(Name) ->
    encode_object_identifier(Val);
encode_object_identifier(Val) ->
    OctetList = e_object_identifier(Val),
    Octets = list_to_binary(OctetList), % performs a flatten at the same time
%    [{debug,object_identifier},encode_length(undefined,size(Octets)),{octets,Octets}].
    [encode_length(undefined,size(Octets)),
     octets_to_complete(size(Octets),Octets)].

%% This code is copied from asn1_encode.erl (BER) and corrected and modified

e_object_identifier({'OBJECT IDENTIFIER',V}) ->
    e_object_identifier(V);
e_object_identifier({Cname,V}) when atom(Cname),tuple(V) ->
    e_object_identifier(tuple_to_list(V));
e_object_identifier({Cname,V}) when atom(Cname),list(V) ->
    e_object_identifier(V);
e_object_identifier(V) when tuple(V) ->
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
    Num;
%% must be changed to handle more than 2 octets
e_object_element(Num) ->  %% when Num < ???
    Left = ((Num band 2#11111110000000) bsr 7) bor 2#10000000,
    Right = Num band 2#1111111 ,
    [Left,Right].



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

get_constraint([{Key,V}],Key) ->
    V;
get_constraint([],_) ->
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

-ifdef(nodriver).

complete(L) ->
    case complete1(L) of
	{[],[]} ->
	    <<0>>;
	{Acc,[]} ->
	    Acc;
	{Acc,Bacc}  ->
	    [Acc|complete_bytes(Bacc)]
    end.


% this function builds the ugly form of lists [E1|E2] to avoid having to reverse it at the end.
% this is done because it is efficient and that the result always will be sent on a port or
% converted by means of list_to_binary/1
 complete1(InList) when list(InList) ->
     complete1(InList,[],[]);
 complete1(InList) ->
     complete1([InList],[],[]).

 complete1([],Acc,Bacc) ->
     {Acc,Bacc};
 complete1([H|T],Acc,Bacc) when list(H) ->
     {NewH,NewBacc} = complete1(H,Acc,Bacc),
     complete1(T,NewH,NewBacc);

 complete1([{octets,Bin}|T],Acc,[]) ->
     complete1(T,[Acc|Bin],[]);

 complete1([{octets,Bin}|T],Acc,Bacc) ->
     complete1(T,[Acc|[complete_bytes(Bacc),Bin]],[]);

 complete1([{debug,_}|T], Acc,Bacc) ->
     complete1(T,Acc,Bacc);

 complete1([{bits,N,Val}|T],Acc,Bacc) ->
     complete1(T,Acc,complete_update_byte(Bacc,Val,N));

 complete1([{bit,Val}|T],Acc,Bacc) ->
     complete1(T,Acc,complete_update_byte(Bacc,Val,1));

 complete1([align|T],Acc,[]) ->
     complete1(T,Acc,[]);
 complete1([align|T],Acc,Bacc) ->
     complete1(T,[Acc|complete_bytes(Bacc)],[]);
 complete1([{0,Bin}|T],Acc,[]) when binary(Bin) ->
     complete1(T,[Acc|Bin],[]);
 complete1([{Unused,Bin}|T],Acc,[]) when integer(Unused),binary(Bin) ->
     Size = size(Bin)-1,
     <<Bs:Size/binary,B>> = Bin,
     NumBits = 8-Unused,
     complete1(T,[Acc|Bs],[[B bsr Unused]|NumBits]);
 complete1([{Unused,Bin}|T],Acc,Bacc) when integer(Unused),binary(Bin) ->
     Size = size(Bin)-1,
     <<Bs:Size/binary,B>> = Bin,
     NumBits = 8 - Unused,
     Bf = complete_bytes(Bacc),
     complete1(T,[Acc|[Bf,Bs]],[[B bsr Unused]|NumBits]).


 complete_update_byte([],Val,Len) ->
     complete_update_byte([[0]|0],Val,Len);
 complete_update_byte([[Byte|Bacc]|NumBits],Val,Len) when NumBits + Len == 8 ->
     [[0,((Byte bsl Len) + Val) band 255|Bacc]|0];
 complete_update_byte([[Byte|Bacc]|NumBits],Val,Len) when NumBits + Len > 8  ->
     Rem = 8 - NumBits,
     Rest = Len - Rem,
     complete_update_byte([[0,((Byte bsl Rem) + (Val bsr Rest)) band 255 |Bacc]|0],Val,Rest);
 complete_update_byte([[Byte|Bacc]|NumBits],Val,Len) ->
     [[((Byte bsl Len) + Val) band 255|Bacc]|NumBits+Len].


 complete_bytes([[Byte|Bacc]|0]) ->
     lists:reverse(Bacc);
 complete_bytes([[Byte|Bacc]|NumBytes]) ->
     lists:reverse([(Byte bsl (8-NumBytes)) band 255|Bacc]);
 complete_bytes([]) ->
     [].

-else.


 complete(L) ->
    case catch port_control(drv_complete,1,L) of
	Bin when binary(Bin) ->
	    Bin;
	List when list(List) -> handle_error(List,L);
	{'EXIT',{badarg,Reason}} ->
	    asn1rt_driver_handler:load_driver(),
	    receive
		driver_ready ->
		    case catch port_control(drv_complete,1,L) of
			Bin2 when binary(Bin2) -> Bin2;
			List when list(List) -> handle_error(List,L);
			Error -> exit(Error)
		    end;
		{error,Error} -> % error when loading driver
		    %% the driver could not be loaded
		    exit(Error);
		Error={port_error,Reason} ->
		    exit(Error)
	    end;
	{'EXIT',Reason} ->
	    exit(Reason)
    end.

handle_error([],_)->
    exit({error,{"memory allocation problem"}});
handle_error("1",L) -> % error in complete in driver
    exit({error,{asn1_error,L}});
handle_error(ErrL,L) ->
    exit({error,{unknown_error,ErrL,L}}).

-endif.


octets_to_complete(Len,Val) when Len < 256 ->
    [20,Len,Val];
octets_to_complete(Len,Val) ->
    [21,<<Len:16>>,Val].

octets_unused_to_complete(Unused,Len,Val) when Len < 256 ->
    [30,Unused,Len,Val];
octets_unused_to_complete(Unused,Len,Val) ->
    [31,Unused,<<Len:16>>,Val].
