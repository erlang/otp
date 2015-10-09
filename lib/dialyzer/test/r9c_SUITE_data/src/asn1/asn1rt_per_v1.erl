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
%%     $Id: asn1rt_per_v1.erl,v 1.1 2008/12/17 09:53:31 mikpe Exp $
-module(asn1rt_per_v1).

%% encoding / decoding of PER aligned

-include("asn1_records.hrl").

-export([dec_fixup/3, cindex/3, list_to_record/2]).
-export([setchoiceext/1, setext/1, fixoptionals/2, fixextensions/2,
	 setoptionals/1, fixoptionals2/3, getext/1, getextension/2,
	 skipextensions/3, getbit/1, getchoice/3 ]).
-export([getoptionals/2, getoptionals/3, set_choice/3,
	 getoptionals2/2,
	 encode_integer/2, encode_integer/3  ]).
-export([decode_integer/2, decode_integer/3, encode_small_number/1,
	 encode_boolean/1, decode_boolean/1, encode_length/2,
	 decode_length/1, decode_length/2,
	 encode_small_length/1, decode_small_length/1,
	 decode_compact_bit_string/3]).
-export([encode_enumerated/3, decode_enumerated/3,
	 encode_bit_string/3, decode_bit_string/3  ]).
-export([encode_octet_string/2, decode_octet_string/2,
	encode_null/1, decode_null/1,
	encode_object_identifier/1, decode_object_identifier/1,
	complete/1]).

-export([encode_open_type/2, decode_open_type/2]).

-export([encode_UniversalString/2, decode_UniversalString/2,
	 encode_PrintableString/2, decode_PrintableString/2,
	 encode_GeneralString/2, decode_GeneralString/2,
	 encode_GraphicString/2, decode_GraphicString/2,
	 encode_TeletexString/2, decode_TeletexString/2,
	 encode_VideotexString/2, decode_VideotexString/2,
	 encode_VisibleString/2, decode_VisibleString/2,
	 encode_BMPString/2, decode_BMPString/2,
	 encode_IA5String/2, decode_IA5String/2,
	 encode_NumericString/2, decode_NumericString/2,
	 encode_ObjectDescriptor/2, decode_ObjectDescriptor/1
	 ]).


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

% converts a list to a record if necessary
list_to_record(Name,List) when list(List) ->
    list_to_tuple([Name|List]);
list_to_record(_Name,Tuple) when tuple(Tuple) ->
    Tuple.

%%--------------------------------------------------------
%% setchoiceext(InRootSet) -> [{bit,X}]
%% X  is set to  1 when InRootSet==false
%% X  is set to  0 when InRootSet==true
%%
setchoiceext(true) ->
    [{debug,choiceext},{bit,0}];
setchoiceext(false) ->
    [{debug,choiceext},{bit,1}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% setext(true|false) ->  CompleteList
%%

setext(true) ->
    [{debug,ext},{bit,1}];
setext(false) ->
    [{debug,ext},{bit,0}].

%%

fixoptionals2(OptList,OptLength,Val) when tuple(Val) ->
    Bits = fixoptionals2(OptList,Val,0),
    {Val,{bits,OptLength,Bits}};

fixoptionals2([],_Val,Acc) ->
    %% Optbits
    Acc;
fixoptionals2([Pos|Ot],Val,Acc) ->
    case element(Pos,Val) of
	asn1_NOVALUE -> fixoptionals2(Ot,Val,Acc bsl 1);
	asn1_DEFAULT -> fixoptionals2(Ot,Val,Acc bsl 1);
	_ -> fixoptionals2(Ot,Val,(Acc bsl 1) + 1)
    end.


%%
%% fixoptionals remains only for backward compatibility purpose
fixoptionals(OptList,Val) when tuple(Val) ->
    fixoptionals(OptList,Val,[]);

fixoptionals(OptList,Val) when list(Val) ->
    fixoptionals(OptList,Val,1,[],[]).

fixoptionals([],Val,Acc) ->
    % return {Val,Opt}
    {Val,lists:reverse(Acc)};
fixoptionals([{_,Pos}|Ot],Val,Acc) ->
    case element(Pos+1,Val) of
	asn1_NOVALUE -> fixoptionals(Ot,Val,[0|Acc]);
	asn1_DEFAULT -> fixoptionals(Ot,Val,[0|Acc]);
	_ -> fixoptionals(Ot,Val,[1|Acc])
    end.


%setoptionals(OptList,Val) ->
%    Vlist = tuple_to_list(Val),
%    setoptionals(OptList,Vlist,1,[]).

fixoptionals([{Name,Pos}|Ot],[{Name,Val}|Vt],_Opt,Acc1,Acc2) ->
    fixoptionals(Ot,Vt,Pos+1,[1|Acc1],[{Name,Val}|Acc2]);
fixoptionals([{_Name,Pos}|Ot],V,Pos,Acc1,Acc2) ->
    fixoptionals(Ot,V,Pos+1,[0|Acc1],[asn1_NOVALUE|Acc2]);
fixoptionals(O,[Vh|Vt],Pos,Acc1,Acc2) ->
    fixoptionals(O,Vt,Pos+1,Acc1,[Vh|Acc2]);
fixoptionals([],[Vh|Vt],Pos,Acc1,Acc2) ->
    fixoptionals([],Vt,Pos+1,Acc1,[Vh|Acc2]);
fixoptionals([],[],_,Acc1,Acc2) ->
    % return {Val,Opt}
    {list_to_tuple([asn1_RECORDNAME|lists:reverse(Acc2)]),lists:reverse(Acc1)}.

setoptionals([H|T]) ->
    [{bit,H}|setoptionals(T)];
setoptionals([]) ->
    [{debug,optionals}].

getext(Bytes) when tuple(Bytes) ->
    getbit(Bytes);
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
	    [encode_small_length(ExtNum),{bits,ExtNum,ExtBits}]
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
getchoice(Bytes,_NumChoices,1) ->
    decode_small_number(Bytes);
getchoice(Bytes,NumChoices,0) ->
    decode_integer(Bytes,[{'ValueRange',{0,NumChoices-1}}]).

getoptionals2(Bytes,NumOpt) ->
    getbits(Bytes,NumOpt).

%% getoptionals is kept only for bakwards compatibility
getoptionals(Bytes,NumOpt) ->
    {Blist,Bytes1} = getbits_as_list(NumOpt,Bytes),
    {list_to_tuple(Blist),Bytes1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getoptionals/3 is only here for compatibility from 1.3.2
%% the codegenerator uses getoptionals/2

getoptionals(Bytes,L,NumComp) when list(L) ->
    {Blist,Bytes1} = getbits_as_list(length(L),Bytes),
    {list_to_tuple(comptuple(Blist,L,NumComp,1)),Bytes1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% comptuple is only here for compatibility not used from 1.3.2
comptuple([Bh|Bt],[{_Name,Nr}|T],NumComp,Nr) ->
    [Bh|comptuple(Bt,T,NumComp-1,Nr+1)];
comptuple(Bl,[{Name,Tnr}|Tl],NumComp,Nr) ->
    [0|comptuple(Bl,[{Name,Tnr}|Tl],NumComp-1,Nr+1)];
comptuple(_B,_L,0,_Nr) ->
    [];
comptuple(B,O,N,Nr) ->
    [0|comptuple(B,O,N-1,Nr+1)].

%% getbits_as_binary(Num,Bytes) -> {{Unused,BinBits},RestBytes},
%% Num = integer(),
%% Bytes = list() | tuple(),
%% Unused = integer(),
%% BinBits = binary(),
%% RestBytes = tuple()
getbits_as_binary(Num,Bytes) when list(Bytes) ->
    getbits_as_binary(Num,{0,Bytes});
getbits_as_binary(_Num,{Used,[]}) ->
    {{0,<<>>},{Used,[]}};
getbits_as_binary(Num,{Used,Bits=[H|T]}) ->
    B1 = case (Num+Used) =< 8 of
	     true -> Num;
	     _ -> 8-Used
	 end,
    B2 = Num - B1,
    Pad = (8 - ((B1+B2) rem 8)) rem 8,% Pad /= 8
    RestBits = lists:nthtail((B1+B2) div 8,Bits),
    Int = integer_from_list(B2,T,0),
    NewUsed = (Used + Num) rem 8,
    {{Pad,<<(H bsr (8-(Used+B1))):B1,Int:B2,0:Pad>>},{NewUsed,RestBits}}.

integer_from_list(_Int,[],BigInt) ->
    BigInt;
integer_from_list(Int,[H|_T],BigInt) when Int < 8 ->
    (BigInt bsl Int) bor (H bsr (8-Int));
integer_from_list(Int,[H|T],BigInt) ->
    integer_from_list(Int-8,T,(BigInt bsl 8) bor H).

getbits_as_list(Num,Bytes) ->
    getbits_as_list(Num,Bytes,[]).

getbits_as_list(0,Bytes,Acc) ->
    {lists:reverse(Acc),Bytes};
getbits_as_list(Num,Bytes,Acc) ->
    {Bit,NewBytes} = getbit(Bytes),
    getbits_as_list(Num-1,NewBytes,[Bit|Acc]).

getbit(Bytes) ->
%    io:format("getbit:~p~n",[Bytes]),
    getbit1(Bytes).

getbit1({7,[H|T]}) ->
    {H band 1,{0,T}};
getbit1({Pos,[H|T]}) ->
    {(H bsr (7-Pos)) band 1,{(Pos+1) rem 8,[H|T]}};
getbit1(Bytes) when list(Bytes) ->
    getbit1({0,Bytes}).

%% This could be optimized
getbits(Buffer,Num) ->
%    io:format("getbits:Buffer = ~p~nNum=~p~n",[Buffer,Num]),
    getbits(Buffer,Num,0).

getbits(Buffer,0,Acc) ->
    {Acc,Buffer};
getbits(Buffer,Num,Acc) ->
    {B,NewBuffer} = getbit(Buffer),
    getbits(NewBuffer,Num-1,B + (Acc bsl 1)).


getoctet(Bytes) when list(Bytes) ->
    getoctet({0,Bytes});
getoctet(Bytes) ->
%    io:format("getoctet:Buffer = ~p~n",[Bytes]),
    getoctet1(Bytes).

getoctet1({0,[H|T]}) ->
    {H,{0,T}};
getoctet1({_Pos,[_,H|T]}) ->
    {H,{0,T}}.

align({0,L}) ->
    {0,L};
align({_Pos,[_H|T]}) ->
    {0,T};
align(Bytes) ->
    {0,Bytes}.

getoctets(Buffer,Num) ->
%    io:format("getoctets:Buffer = ~p~nNum = ~p~n",[Buffer,Num]),
    getoctets(Buffer,Num,0).

getoctets(Buffer,0,Acc) ->
    {Acc,Buffer};
getoctets(Buffer,Num,Acc) ->
    {Oct,NewBuffer} = getoctet(Buffer),
    getoctets(NewBuffer,Num-1,(Acc bsl 8)+Oct).

getoctets_as_list(Buffer,Num) ->
    getoctets_as_list(Buffer,Num,[]).

getoctets_as_list(Buffer,0,Acc) ->
    {lists:reverse(Acc),Buffer};
getoctets_as_list(Buffer,Num,Acc) ->
    {Oct,NewBuffer} = getoctet(Buffer),
    getoctets_as_list(NewBuffer,Num-1,[Oct|Acc]).

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
	    [{bit,0}, % the value is in the root set
	     encode_integer([{'ValueRange',{0,Len1-1}}],N)];
	N when integer(N) ->
	    [{bit,0}]; % no encoding if only 0 or 1 alternative
	false ->
	    [{bit,1}, % extension value
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
	    encode_integer([{'ValueRange',{0,Len-1}}],N);
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Constraint, Value) -> CompleteList
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary
%% Contraint = not used in this version
%%
encode_open_type(_Constraint, Val) when list(Val) ->
    [encode_length(undefined,length(Val)),align,
	     {octets,Val}];
encode_open_type(_Constraint, Val) when binary(Val) ->
    [encode_length(undefined,size(Val)),align,
	     {octets,binary_to_list(Val)}].
%% the binary_to_list is not optimal but compatible with the current solution

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Buffer,Constraint) -> Value
%% Constraint is not used in this version
%% Buffer = [byte] with PER encoded data
%% Value = [byte] with decoded data (which must be decoded again as some type)
%%
decode_open_type(Bytes, _Constraint) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes3,Len).


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
encode_integer(C,V,_) when integer(V) ->
    encode_integer(C,V);
encode_integer(C,{Name,V},NamedNumberList) when atom(Name) ->
    encode_integer(C,V,NamedNumberList).

encode_integer(C,{Name,Val}) when atom(Name) ->
    encode_integer(C,Val);

encode_integer({Rc,_Ec},Val) ->
    case (catch encode_integer(Rc,Val)) of
	{'EXIT',{error,{asn1,_}}} ->
	    [{bit,1},encode_unconstrained_number(Val)];
	Encoded ->
	    [{bit,0},Encoded]
    end;
encode_integer(C,Val ) when list(C) ->
    case get_constraint(C,'SingleValue') of
	no ->
	    encode_integer1(C,Val);
	V when integer(V),V == Val ->
	    []; % a type restricted to a single value encodes to nothing
	V when list(V) ->
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

decode_integer(Buffer,{Rc,_Ec}) ->
    {Ext,Buffer2} = getext(Buffer),
    case Ext of
	0 -> decode_integer(Buffer2,Rc);
	1 -> decode_unconstrained_number(Buffer2)
    end;
decode_integer(Buffer,undefined) ->
    decode_unconstrained_number(Buffer);
decode_integer(Buffer,C) ->
    case get_constraint(C,'SingleValue') of
	V when integer(V) ->
	    {V,Buffer};
	V when list(V) ->
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

% X.691:10.6 Encoding of a normally small non-negative whole number
% Use this for encoding of CHOICE index if there is an extension marker in
% the CHOICE
encode_small_number({Name,Val}) when atom(Name) ->
    encode_small_number(Val);
encode_small_number(Val) when Val =< 63 ->
    [{bit,0},{bits,6,Val}];
encode_small_number(Val) ->
    [{bit,1},encode_semi_constrained_number(0,Val)].

decode_small_number(Bytes) ->
    {Bit,Bytes2} = getbit(Bytes),
    case Bit of
	0 ->
	    getbits(Bytes2,6);
	1 ->
	    decode_semi_constrained_number(Bytes2,{0,'MAX'})
    end.

% X.691:10.7 Encoding of a semi-constrained whole number
%% might be an optimization encode_semi_constrained_number(0,Val) ->
encode_semi_constrained_number(C,{Name,Val}) when atom(Name) ->
    encode_semi_constrained_number(C,Val);
encode_semi_constrained_number({Lb,'MAX'},Val) ->
    encode_semi_constrained_number(Lb,Val);
encode_semi_constrained_number(Lb,Val) ->
    Val2 = Val - Lb,
    Octs = eint_positive(Val2),
    [encode_length(undefined,length(Octs)),{octets,Octs}].

decode_semi_constrained_number(Bytes,{Lb,_}) ->
    decode_semi_constrained_number(Bytes,Lb);
decode_semi_constrained_number(Bytes,Lb) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {V,Bytes3} = getoctets(Bytes2,Len),
    {V+Lb,Bytes3}.

encode_constrained_number(Range,{Name,Val}) when atom(Name) ->
    encode_constrained_number(Range,Val);
encode_constrained_number({Lb,Ub},Val) when Val >= Lb, Ub >= Val ->
    Range = Ub - Lb + 1,
    Val2 = Val - Lb,
    if
	Range  == 2 ->
	    {bits,1,Val2};
	Range  =< 4 ->
	    {bits,2,Val2};
	Range  =< 8 ->
	    {bits,3,Val2};
	Range  =< 16 ->
	    {bits,4,Val2};
	Range  =< 32 ->
	    {bits,5,Val2};
	Range  =< 64 ->
	    {bits,6,Val2};
	Range  =< 128 ->
	    {bits,7,Val2};
	Range  =< 255 ->
	    {bits,8,Val2};
	Range  =< 256 ->
	    {octets,1,Val2};
	Range  =< 65536 ->
	    {octets,2,Val2};
	Range =< 16#1000000  ->
	    Octs = eint_positive(Val2),
	    [encode_length({1,3},length(Octs)),{octets,Octs}];
	Range =< 16#100000000  ->
	    Octs = eint_positive(Val2),
	    [encode_length({1,4},length(Octs)),{octets,Octs}];
	Range =< 16#10000000000  ->
	    Octs = eint_positive(Val2),
	    [encode_length({1,5},length(Octs)),{octets,Octs}];
       true  ->
	    exit({not_supported,{integer_range,Range}})
    end.

decode_constrained_number(Buffer,{Lb,Ub}) ->
    Range = Ub - Lb + 1,
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

% X.691:10.8 Encoding of an unconstrained whole number

encode_unconstrained_number(Val) when Val >= 0 ->
    Oct = eint(Val,[]),
    [{debug,unconstrained_number},
     encode_length({0,'MAX'},length(Oct)),
     {octets,Oct}];
encode_unconstrained_number(Val) -> % negative
    Oct = enint(Val,[]),
    [{debug,unconstrained_number},
     encode_length({0,'MAX'},length(Oct)),
     {octets,Oct}].

%% used for positive Values which don't need a sign bit
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

%% used for signed positive values

%eint(Val, Ack) ->
%    X = Val band 255,
%    Next = Val bsr 8,
%    if
%	Next == 0, X >= 127 ->
%	    [0,X|Ack];
%	Next == 0 ->
%	    [X|Ack];
%	true ->
%	    eint(Next,[X|Ack])
%    end.

%%% used for signed negative values
%enint(Val, Acc) ->
%    NumOctets = if
%		    -Val < 16#80 -> 1;
%		    -Val < 16#8000 ->2;
%		    -Val < 16#800000 ->3;
%		    -Val < 16#80000000 ->4;
%		    -Val < 16#8000000000 ->5;
%		    -Val < 16#800000000000 ->6;
%		    -Val < 16#80000000000000 ->7;
%		    -Val < 16#8000000000000000 ->8;
%		    -Val < 16#800000000000000000 ->9
%		end,
%    enint(Val,Acc,NumOctets).

%enint(Val, Acc,0) ->
%    Acc;
%enint(Val, Acc,NumOctets) ->
%    enint(Val bsr 8,[Val band 255|Acc],NumOctets-1).


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

minimum_octets(Val) ->
    minimum_octets(Val,[]).

minimum_octets(Val,Acc) when Val > 0 ->
    minimum_octets((Val bsr 8),[Val band 16#FF|Acc]);
minimum_octets(0,Acc) ->
    Acc.


%% X.691:10.9 Encoding of a length determinant
%%encode_small_length(undefined,Len) -> % null means no UpperBound
%%    encode_small_number(Len).

%% X.691:10.9.3.5
%% X.691:10.9.3.7
encode_length(undefined,Len) -> % un-constrained
    if
	Len < 128 ->
	    {octet,Len band 16#7F};
	Len < 16384 ->
	    {octets,2,2#1000000000000000 bor Len};
	true  ->
	    exit({error,{asn1,{encode_length,{nyi,above_16k}}}})
    end;

encode_length({0,'MAX'},Len) ->
    encode_length(undefined,Len);
encode_length({Lb,Ub},Len) when Ub =< 65535 ,Lb >= 0 -> % constrained
    encode_constrained_number({Lb,Ub},Len);
encode_length({{Lb,Ub},[]},Len) when Ub =< 65535 ,Lb >= 0 ->
    %% constrained extensible
    [{bit,0},encode_constrained_number({Lb,Ub},Len)];
encode_length(SingleValue,_) when integer(SingleValue) ->
    [].

encode_small_length(Len) when Len =< 64 ->
    [{bit,0},{bits,6,Len-1}];
encode_small_length(Len) ->
    [{bit,1},encode_length(undefined,Len)].

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
    Buffer2 = align(Buffer),
    {Bits,_} = getbits(Buffer2,2),
    case Bits of
	2 ->
	    {Val,Bytes3} = getoctets(Buffer2,2),
	    {(Val band 16#3FFF),Bytes3};
	3 ->
	    exit({error,{asn1,{decode_length,{nyi,above_16k}}}});
	_ ->
	    {Val,Bytes3} = getoctet(Buffer2),
	    {Val band 16#7F,Bytes3}
    end;

decode_length(Buffer,{Lb,Ub}) when Ub =< 65535 ,Lb >= 0 -> % constrained
    decode_constrained_number(Buffer,{Lb,Ub});

decode_length(Buffer,{{Lb,Ub},[]}) ->
    case getbit(Buffer) of
	{0,Buffer2} ->
	    decode_length(Buffer2, {Lb,Ub})
    end;
						% X.691:10.9.3.5
decode_length(Buffer,{_,_Lb,_Ub}) -> %when Len =< 127 -> % Unconstrained or large Ub
    case getbit(Buffer) of
	{0,Remain} ->
	    getbits(Remain,7);
	{1,_Remain} ->
	    {Val,Remain2} = getoctets(Buffer,2),
	    {Val band 2#0111111111111111, Remain2}
    end;
decode_length(Buffer,SingleValue) when integer(SingleValue) ->
    {SingleValue,Buffer}.


% X.691:11
encode_boolean({Name,Val}) when atom(Name) ->
    encode_boolean(Val);
encode_boolean(true) ->
    {bit,1};
encode_boolean(false) ->
    {bit,0};
encode_boolean(Val) ->
    exit({error,{asn1,{encode_boolean,Val}}}).


decode_boolean(Buffer) -> %when record(Buffer,buffer)
    case getbit(Buffer) of
	{1,Remain} -> {true,Remain};
	{0,Remain} -> {false,Remain}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% X.691:12
%% ENUMERATED
%%
%% encode_enumerated(C,Value,NamedNumberTup) -> CompleteList
%%
%%

encode_enumerated(C,{Name,Value},NamedNumberList) when
  atom(Name),list(NamedNumberList) ->
    encode_enumerated(C,Value,NamedNumberList);

%% ENUMERATED with extension mark
encode_enumerated(_C,{asn1_enum,Value},{_Nlist1,Nlist2}) when Value >= length(Nlist2) ->
    [{bit,1},encode_small_number(Value)];
encode_enumerated(C,Value,{Nlist1,Nlist2}) ->
    case enum_search(Value,Nlist1,0) of
	NewV when integer(NewV) ->
	    [{bit,0},encode_integer(C,NewV)];
	false ->
	    case enum_search(Value,Nlist2,0) of
		ExtV when integer(ExtV) ->
		    [{bit,1},encode_small_number(ExtV)];
		false ->
		    exit({error,{asn1,{encode_enumerated,Value}}})
	    end
    end;

encode_enumerated(C,Value,NamedNumberList) when list(NamedNumberList) ->
    case enum_search(Value,NamedNumberList,0) of
	NewV when integer(NewV) ->
	    encode_integer(C,NewV);
	false ->
	    exit({error,{asn1,{encode_enumerated,Value}}})
    end.

%% returns the ordinal number from 0 ,1 ... in the list where Name is found
%% or false if not found
%%
enum_search(Name,[Name|_NamedNumberList],Acc) ->
    Acc;
enum_search(Name,[_H|T],Acc) ->
    enum_search(Name,T,Acc+1);
enum_search(_,[],_) ->
    false. % name not found !error

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
%% - [identifiers] where only named identifers are set to one,
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
encode_bit_string(C, [FirstVal | RestVal], NamedBitList) when atom(FirstVal) ->
    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);

encode_bit_string(C, [{bit,No} | RestVal], NamedBitList) ->
    ToSetPos = get_all_bitposes([{bit,No} | RestVal], NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);

%% when the value is a list of ones and zeroes

encode_bit_string(C, BitListValue, NamedBitList) when list(BitListValue) ->
    Bl1 =
	case NamedBitList of
	    [] ->  % dont remove trailing zeroes
		BitListValue;
	    _ -> % first remove any trailing zeroes
		lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,
					      lists:reverse(BitListValue)))
	end,
    BitList = [{bit,X} || X <- Bl1],
    BListLen = length(BitList),
    case get_constraint(C,'SizeConstraint') of
	0 -> % fixed length
	    []; % nothing to encode
	V when integer(V),V=<16 -> % fixed length 16 bits or less
	    pad_list(V,BitList);
	V when integer(V) -> % fixed length 16 bits or less
	    [align,pad_list(V,BitList)];
	{Lb,Ub} when integer(Lb),integer(Ub),BListLen<Lb ->
	    %% padding due to OTP-4353
	    [encode_length({Lb,Ub},Lb),align,pad_list(Lb,BitList)];
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    [encode_length({Lb,Ub},length(BitList)),align,BitList];
	no ->
	    [encode_length(undefined,length(BitList)),align,BitList];
	Sc={{Lb,Ub},_} when integer(Lb),integer(Ub),BListLen<Lb ->
	    %% padding due to OTP-4353
	    [encode_length(Sc,Lb),align,pad_list(Lb,BitList)];
	Sc -> % extension marker
	    [encode_length(Sc,length(BitList)),align,BitList]
    end;

%% when the value is an integer
encode_bit_string(C, IntegerVal, NamedBitList) when integer(IntegerVal)->
    BitList = int_to_bitlist(IntegerVal),
    encode_bit_string(C,BitList,NamedBitList);

%% when the value is a tuple
encode_bit_string(C,{Name,Val}, NamedBitList) when atom(Name) ->
    encode_bit_string(C,Val,NamedBitList).


%% encode_bin_bit_string/3, when value is a tuple of Unused and BinBits.
%% Unused = integer(),
%% BinBits = binary().

encode_bin_bit_string(C,{Unused,BinBits},NamedBitList) ->
    RemoveZerosIfNNL =
	fun({NNL,BitList}) ->
		case NNL of
		    [] -> BitList;
		    _ ->
			lists:reverse(
			  lists:dropwhile(fun(0)->true;(1)->false end,
					  lists:reverse(BitList)))
		end
	end,
    {OctetList,OLSize,LastBits} =
	case size(BinBits) of
	    N when N>1 ->
		IntList = binary_to_list(BinBits),
		[H|T] = lists:reverse(IntList),
		Bl1 = RemoveZerosIfNNL({NamedBitList,lists:reverse(int_to_bitlist(H,8-Unused))}),% lists:sublist obsolete if trailing bits are zero !
		{[{octet,X} || X <- lists:reverse(T)],size(BinBits)-1,
		 [{bit,X} || X <- Bl1]};
	    1 ->
		<<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1>> = BinBits,
		{[],0,[{bit,X} || X <- lists:sublist([B7,B6,B5,B4,B3,B2,B1,B0],8-Unused)]};
	    _ ->
		{[],0,[]}
	end,
    case get_constraint(C,'SizeConstraint') of
	0 ->
	    [];
	V when integer(V),V=<16 ->
	    [OctetList, pad_list(V,LastBits)];
	V when integer(V) ->
%	    [OctetList, align, pad_list(V rem 8,LastBits)];
	    [align,OctetList, pad_list(V rem 8,LastBits)];
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    NewLastBits = maybe_pad(Lb,length(LastBits)+(OLSize*8),LastBits,NamedBitList),
	    [encode_length({Lb,Ub},length(NewLastBits)+(OLSize*8)),
%			   OctetList,align,LastBits];
			   align,OctetList,NewLastBits];
	no ->
	    [encode_length(undefined,length(LastBits)+(OLSize*8)),
%	     OctetList,align,LastBits];
	     align,OctetList,LastBits];
	Sc={{Lb,_},_} when integer(Lb) ->
	    NewLastBits = maybe_pad(Lb,length(LastBits)+(OLSize*8),LastBits,NamedBitList),
	    [encode_length(Sc,length(NewLastBits)+(OLSize*8)),
	     align,OctetList,NewLastBits];
	Sc ->
	    [encode_length(Sc,length(LastBits)+(OLSize*8)),
%	     OctetList,align,LastBits]
	     align,OctetList,LastBits]
    end.

maybe_pad(_,_,Bits,[]) ->
    Bits;
maybe_pad(Lb,LenBits,Bits,_) when Lb>LenBits ->
    pad_list(Lb,Bits);
maybe_pad(_,_,Bits,_) ->
    Bits.

%%%%%%%%%%%%%%%
%% The result is presented as a list of named bits (if possible)
%% else as a tuple {Unused,Bits}. Unused is the number of unused
%% bits, least significant bits in the last byte of Bits. Bits is
%% the BIT STRING represented as a binary.
%%
decode_compact_bit_string(Buffer, C, NamedNumberList) ->
    case get_constraint(C,'SizeConstraint') of
	0 -> % fixed length
	    {{0,<<>>},Buffer};
	V when integer(V),V=<16 -> %fixed length 16 bits or less
	    compact_bit_string(Buffer,V,NamedNumberList);
	V when integer(V) -> %fixed length > 16 bits
	    Bytes2 = align(Buffer),
	    compact_bit_string(Bytes2,V,NamedNumberList);
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    Bytes3 = align(Bytes2),
	    compact_bit_string(Bytes3,Len,NamedNumberList);
	no ->
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
	0 -> % fixed length
	    {[],Buffer}; % nothing to encode
	V when integer(V),V=<16 -> % fixed length 16 bits or less
	    bit_list_to_named(Buffer,V,NamedNumberList);
	V when integer(V) -> % fixed length 16 bits or less
	    Bytes2 = align(Buffer),
	    bit_list_to_named(Bytes2,V,NamedNumberList);
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    Bytes3 = align(Bytes2),
	    bit_list_to_named(Bytes3,Len,NamedNumberList);
	no ->
	    {Len,Bytes2} = decode_length(Buffer,undefined),
	    Bytes3 = align(Bytes2),
	    bit_list_to_named(Bytes3,Len,NamedNumberList);
	Sc -> % extension marker
	    {Len,Bytes2} = decode_length(Buffer,Sc),
	    Bytes3 = align(Bytes2),
	    bit_list_to_named(Bytes3,Len,NamedNumberList)
    end.


%% if no named bits are declared we will return a
%% {Unused,Bits}. Unused = integer(),
%% Bits = binary().
compact_bit_string(Buffer,Len,[]) ->
    getbits_as_binary(Len,Buffer); % {{Unused,BinBits},NewBuffer}
compact_bit_string(Buffer,Len,NamedNumberList) ->
    bit_list_to_named(Buffer,Len,NamedNumberList).


%% if no named bits are declared we will return a
%% BitList = [0 | 1]

bit_list_to_named(Buffer,Len,[]) ->
    getbits_as_list(Len,Buffer);

%% if there are named bits declared we will return a named
%% BitList where the names are atoms and unnamed bits represented
%% as {bit,Pos}
%% BitList = [atom() | {bit,Pos}]
%% Pos = integer()

bit_list_to_named(Buffer,Len,NamedNumberList) ->
    {BitList,Rest} = getbits_as_list(Len,Buffer),
    {bit_list_to_named1(0,BitList,NamedNumberList,[]), Rest}.

bit_list_to_named1(Pos,[0|Bt],Names,Acc) ->
    bit_list_to_named1(Pos+1,Bt,Names,Acc);
bit_list_to_named1(Pos,[1|Bt],Names,Acc) ->
    case lists:keysearch(Pos,2,Names) of
	{value,{Name,_}} ->
	    bit_list_to_named1(Pos+1,Bt,Names,[Name|Acc]);
	_  ->
	    bit_list_to_named1(Pos+1,Bt,Names,[{bit,Pos}|Acc])
    end;
bit_list_to_named1(_Pos,[],_Names,Acc) ->
    lists:reverse(Acc).



%%%%%%%%%%%%%%%
%%

int_to_bitlist(0) ->
    [];
int_to_bitlist(Int) when integer(Int), Int >= 0 ->
    [Int band 1 | int_to_bitlist(Int bsr 1)].

int_to_bitlist(_Int,0) ->
    [];
int_to_bitlist(0,N) ->
    [0|int_to_bitlist(0,N-1)];
int_to_bitlist(Int,N) ->
    [Int band 1 | int_to_bitlist(Int bsr 1, N-1)].


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
make_and_set_list([], _XPos) ->
    [].

%%%%%%%%%%%%%%%%%
%% pad_list(N,BitList) -> PaddedList
%% returns a padded (with trailing {bit,0} elements) list of length N
%% if Bitlist contains more than N significant bits set an exit asn1_error
%% is generated

pad_list(0,BitList) ->
    case BitList of
	[] -> [];
	_ -> exit({error,{asn1,{range_error,{bit_string,BitList}}}})
    end;
pad_list(N,[Bh|Bt]) ->
    [Bh|pad_list(N-1,Bt)];
pad_list(N,[]) ->
    [{bit,0},pad_list(N-1,[])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% X.691:16
%% encode_octet_string(Constraint,ExtensionMarker,Val)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_octet_string(C,{Name,Val}) when atom(Name) ->
    encode_octet_string(C,false,Val);
encode_octet_string(C,Val) ->
    encode_octet_string(C,false,Val).

encode_octet_string(C,Bool,{_Name,Val}) ->
    encode_octet_string(C,Bool,Val);
encode_octet_string(_,true,_) ->
    exit({error,{asn1,{'not_supported',extensionmarker}}});
encode_octet_string(C,false,Val) ->
    case get_constraint(C,'SizeConstraint') of
	0 ->
	    [];
	1 ->
	    [V] = Val,
	    {bits,8,V};
	2 ->
	    [V1,V2] = Val,
	    [{bits,8,V1},{bits,8,V2}];
	Sv when Sv =<65535, Sv == length(Val) -> % fixed length
	    [align,{octets,Val}];
	{Lb,Ub}  ->
	    [encode_length({Lb,Ub},length(Val)),align,
	     {octets,Val}];
	Sv when list(Sv) ->
	    [encode_length({hd(Sv),lists:max(Sv)},length(Val)),align,
	     {octets,Val}];
	no  ->
	    [encode_length(undefined,length(Val)),align,
	     {octets,Val}]
    end.

decode_octet_string(Bytes,Range) ->
    decode_octet_string(Bytes,Range,false).

decode_octet_string(Bytes,C,false) ->
    case get_constraint(C,'SizeConstraint') of
	0 ->
	    {[],Bytes};
	1 ->
	    {B1,Bytes2} = getbits(Bytes,8),
	    {[B1],Bytes2};
	2 ->
	    {B1,Bytes2}= getbits(Bytes,8),
	    {B2,Bytes3}= getbits(Bytes2,8),
	    {[B1,B2],Bytes3};
	{_,0} ->
	    {[],Bytes};
	Sv when integer(Sv), Sv =<65535 -> % fixed length
	    Bytes2 = align(Bytes),
	    getoctets_as_list(Bytes2,Sv);
	{Lb,Ub}  ->
	    {Len,Bytes2} = decode_length(Bytes,{Lb,Ub}),
	    Bytes3 = align(Bytes2),
	    getoctets_as_list(Bytes3,Len);
	Sv when list(Sv) ->
	    {Len,Bytes2} = decode_length(Bytes,{hd(Sv),lists:max(Sv)}),
	    Bytes3 = align(Bytes2),
	    getoctets_as_list(Bytes3,Len);
	no  ->
	    {Len,Bytes2} = decode_length(Bytes,undefined),
	    Bytes3 = align(Bytes2),
	    getoctets_as_list(Bytes3,Len)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Restricted char string types
%% (NumericString, PrintableString,VisibleString,IA5String,BMPString,UniversalString)
%% X.691:26 and X.680:34-36
%%encode_restricted_string(aligned,'BMPString',Constraints,Extension,Val)


encode_restricted_string(aligned,{Name,Val}) when atom(Name) ->
    encode_restricted_string(aligned,Val);

encode_restricted_string(aligned,Val) when list(Val)->
    [encode_length(undefined,length(Val)),align,
     {octets,Val}].

encode_known_multiplier_string(aligned,StringType,C,_Ext,{Name,Val}) when atom(Name) ->
    encode_known_multiplier_string(aligned,StringType,C,false,Val);

encode_known_multiplier_string(aligned,StringType,C,_Ext,Val) ->
    Result = chars_encode(C,StringType,Val),
    NumBits = get_NumBits(C,StringType),
    case get_constraint(C,'SizeConstraint') of
	Ub when integer(Ub), Ub*NumBits =< 16  ->
	    case {StringType,Result} of
		{'BMPString',{octets,Ol}} ->
		    [{bits,8,Oct}||Oct <- Ol];
		_ ->
		    Result
	    end;
	0 ->
	    [];
	Ub when integer(Ub),Ub =<65535 -> % fixed length
	    [align,Result];
	{Ub,Lb} ->
	    [encode_length({Ub,Lb},length(Val)),align,Result];
	Vl when list(Vl) ->
	    [encode_length({lists:min(Vl),lists:max(Vl)},length(Val)),align,Result];
	no  ->
	    [encode_length(undefined,length(Val)),align,Result]
    end.

decode_restricted_string(Bytes,aligned) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes3,Len).

decode_known_multiplier_string(Bytes,aligned,StringType,C,_Ext) ->
    NumBits = get_NumBits(C,StringType),
    case get_constraint(C,'SizeConstraint') of
	Ub when integer(Ub), Ub*NumBits =< 16  ->
	    chars_decode(Bytes,NumBits,StringType,C,Ub);
	Ub when integer(Ub),Ub =<65535 -> % fixed length
	    Bytes1 = align(Bytes),
	    chars_decode(Bytes1,NumBits,StringType,C,Ub);
	0 ->
	    {[],Bytes};
	Vl when list(Vl) ->
	    {Len,Bytes1} = decode_length(Bytes,{hd(Vl),lists:max(Vl)}),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,C,Len);
	no  ->
	    {Len,Bytes1} = decode_length(Bytes,undefined),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,C,Len);
	{Lb,Ub}->
	    {Len,Bytes1} = decode_length(Bytes,{Lb,Ub}),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,C,Len)
    end.


encode_NumericString(C,Val) ->
    encode_known_multiplier_string(aligned,'NumericString',C,false,Val).
decode_NumericString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,aligned,'NumericString',C,false).

encode_PrintableString(C,Val) ->
    encode_known_multiplier_string(aligned,'PrintableString',C,false,Val).
decode_PrintableString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,aligned,'PrintableString',C,false).

encode_VisibleString(C,Val) -> % equivalent with ISO646String
    encode_known_multiplier_string(aligned,'VisibleString',C,false,Val).
decode_VisibleString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,aligned,'VisibleString',C,false).

encode_IA5String(C,Val) ->
    encode_known_multiplier_string(aligned,'IA5String',C,false,Val).
decode_IA5String(Bytes,C) ->
    decode_known_multiplier_string(Bytes,aligned,'IA5String',C,false).

encode_BMPString(C,Val) ->
    encode_known_multiplier_string(aligned,'BMPString',C,false,Val).
decode_BMPString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,aligned,'BMPString',C,false).

encode_UniversalString(C,Val) ->
    encode_known_multiplier_string(aligned,'UniversalString',C,false,Val).
decode_UniversalString(Bytes,C) ->
    decode_known_multiplier_string(Bytes,aligned,'UniversalString',C,false).

%% end of known-multiplier strings for which PER visible constraints are
%% applied

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
    [{bits,NumBits,H-Min}|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([H|T],NumBits,{Min,Max,Tab}) when H =< Max, H >= Min ->
    [{bits,NumBits,element(H-Min+1,Tab)}|chars_encode2(T,NumBits,{Min,Max,Tab})];
chars_encode2([{A,B,C,D}|T],NumBits,{Min,Max,notab}) ->
    %% no value range check here (ought to be, but very expensive)
%    [{bits,NumBits,(A*B*C*D)-Min}|chars_encode2(T,NumBits,{Min,Max,notab})];
    [{bits,NumBits,((((((A bsl 8) + B) bsl 8) + C) bsl 8) + D)-Min}|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([{A,B,C,D}|T],NumBits,{Min,Max,Tab}) ->
    %% no value range check here (ought to be, but very expensive)
%    [{bits,NumBits,element((A*B*C*D)-Min,Tab)}|chars_encode2(T,NumBits,{Min,Max,notab})];
    [{bits,NumBits,element(((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min,Tab)}|chars_encode2(T,NumBits,{Min,Max,notab})];
chars_encode2([H|_T],_,{_,_,_}) ->
    exit({error,{asn1,{illegal_char_value,H}}});
chars_encode2([],_,_) ->
    [].


get_NumBits(C,StringType) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    charbits(length(Sv),aligned);
	no ->
	    case StringType of
		'IA5String' ->
		    charbits(128,aligned); % 16#00..16#7F
		'VisibleString' ->
		    charbits(95,aligned); % 16#20..16#7E
		'PrintableString' ->
		    charbits(74,aligned); % [$\s,$',$(,$),$+,$,,$-,$.,$/,"0123456789",$:,$=,$?,$A..$Z,$a..$z
		'NumericString' ->
		    charbits(11,aligned); % $ ,"0123456789"
		'UniversalString' ->
		    32;
		'BMPString' ->
		    16
	    end
    end.

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

%% This very inefficient and should be moved to compiletime
charbits(NumOfChars,aligned) ->
    case charbits(NumOfChars) of
	1 -> 1;
	2 -> 2;
	B when B > 2, B =< 4 -> 4;
	B when B > 4, B =< 8 -> 8;
	B when B > 8, B =< 16 -> 16;
	B when B > 16, B =< 32 -> 32
    end.

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
charbits(NumOfChars) when integer(NumOfChars) ->
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
    Result = case minimum_octets(Char+Min) of
	[NewChar] -> NewChar;
	[C1,C2] -> {0,0,C1,C2};
	[C1,C2,C3] -> {0,C1,C2,C3};
	[C1,C2,C3,C4] -> {C1,C2,C3,C4}
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
encode_null({Name,Val}) when atom(Name) ->
    encode_null(Val);
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
%% CompleteList -> [{bits,8,Val}|{octets,Ol}|align|...]
%%
encode_object_identifier({Name,Val}) when atom(Name) ->
    encode_object_identifier(Val);
encode_object_identifier(Val) ->
    Octets = e_object_identifier(Val,notag),
    [{debug,object_identifier},encode_length(undefined,length(Octets)),{octets,Octets}].

%% This code is copied from asn1_encode.erl (BER) and corrected and modified

e_object_identifier({'OBJECT IDENTIFIER',V},DoTag) ->
    e_object_identifier(V,DoTag);
e_object_identifier({Cname,V},DoTag) when atom(Cname),tuple(V) ->
    e_object_identifier(tuple_to_list(V),DoTag);
e_object_identifier({Cname,V},DoTag) when atom(Cname),list(V) ->
    e_object_identifier(V,DoTag);
e_object_identifier(V,DoTag) when tuple(V) ->
    e_object_identifier(tuple_to_list(V),DoTag);

% E1 = 0|1|2 and (E2 < 40 when E1 = 0|1)
e_object_identifier([E1,E2|Tail],_DoTag) when E1 =< 2 ->
    Head = 40*E1 + E2,  % weird
    Res = e_object_elements([Head|Tail]),
%    dotag(DoTag,[6],elength(length(Res)+1),[Head|Res]),
    Res.

e_object_elements([]) ->
    [];
e_object_elements([H|T]) ->
    lists:append(e_object_element(H),e_object_elements(T)).

e_object_element(Num) when Num < 128 ->
    [Num];
% must be changed to handle more than 2 octets
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
complete(InList) when list(InList) ->
    complete(InList,[],0);
complete(InList) ->
    complete([InList],[],0).

complete([{debug,_}|T], Acc, Acclen) ->
    complete(T,Acc,Acclen);
complete([H|T],Acc,Acclen) when list(H) ->
    complete(lists:concat([H,T]),Acc,Acclen);


complete([{octets,N,Val}|T],Acc,Acclen) when N =< 4 ,integer(Val) ->
    Newval = case N of
	1 ->
	    Val4 = Val band 16#FF,
	    [Val4];
	2 ->
	    Val3 = (Val bsr 8) band 16#FF,
	    Val4 = Val band 16#FF,
	    [Val3,Val4];
	3 ->
	    Val2 = (Val bsr 16) band 16#FF,
	    Val3 = (Val bsr 8) band 16#FF,
	    Val4 = Val band 16#FF,
	    [Val2,Val3,Val4];
	    4 ->
	    Val1 = (Val bsr 24) band 16#FF,
	    Val2 = (Val bsr 16) band 16#FF,
	    Val3 = (Val bsr 8) band 16#FF,
	    Val4 = Val band 16#FF,
	    [Val1,Val2,Val3,Val4]
    end,
    complete([{octets,Newval}|T],Acc,Acclen);

complete([{octets,Oct}|T],[],_Acclen) when list(Oct) ->
    complete(T,lists:reverse(Oct),0);
complete([{octets,Oct}|T],[Hacc|Tacc],Acclen) when list(Oct) ->
    Rest = 8 - Acclen,
    if
	Rest == 8 ->
	    complete(T,lists:concat([lists:reverse(Oct),[Hacc|Tacc]]),0);
	true ->
	    complete(T,lists:concat([lists:reverse(Oct),[Hacc bsl Rest|Tacc]]),0)
    end;

complete([{bit,Val}|T], Acc, Acclen) ->
    complete([{bits,1,Val}|T],Acc,Acclen);
complete([{octet,Val}|T], Acc, Acclen) ->
    complete([{octets,1,Val}|T],Acc,Acclen);

complete([{bits,N,Val}|T], Acc, 0) when N =< 8 ->
    complete(T,[Val|Acc],N);
complete([{bits,N,Val}|T], [Hacc|Tacc], Acclen) when N =< 8 ->
    Rest = 8 - Acclen,
    if
	Rest >= N ->
	    complete(T,[(Hacc bsl N) + Val|Tacc],(Acclen+N) rem 8);
	true ->
	    Diff = N - Rest,
	    NewHacc = (Hacc bsl Rest) + (Val bsr Diff),
	    Mask = element(Diff,{1,3,7,15,31,63,127,255}),
	    complete(T,[(Val band Mask),NewHacc|Tacc],(Acclen+N) rem 8)
    end;
complete([{bits,N,Val}|T], Acc, Acclen) -> % N > 8
    complete([{bits,N-8,Val bsr 8},{bits,8,Val band 255}|T],Acc,Acclen);

complete([align|T],Acc,0) ->
    complete(T,Acc,0);
complete([align|T],[Hacc|Tacc],Acclen) ->
    Rest = 8 - Acclen,
    complete(T,[Hacc bsl Rest|Tacc],0);
complete([{octets,_N,Val}|T],Acc,Acclen) when list(Val) -> % no security check here
    complete([{octets,Val}|T],Acc,Acclen);

complete([],[],0) ->
    [0]; % a complete encoding must always be at least 1 byte
complete([],Acc,0) ->
    lists:reverse(Acc);
complete([],[Hacc|Tacc],Acclen) when Acclen > 0->
    Rest = 8 - Acclen,
    NewHacc = Hacc bsl Rest,
    lists:reverse([NewHacc|Tacc]).
