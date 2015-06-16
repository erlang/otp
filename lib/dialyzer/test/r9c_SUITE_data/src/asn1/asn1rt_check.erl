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
%%     $Id: asn1rt_check.erl,v 1.1 2008/12/17 09:53:30 mikpe Exp $
-module(asn1rt_check).

-include("asn1_records.hrl").

-export([check_bool/2,
	 check_int/3,
	 check_bitstring/3,
	 check_octetstring/2,
	 check_null/2,
	 check_objectidentifier/2,
	 check_objectdescriptor/2,
	 check_real/2,
	 check_enum/3,
	 check_restrictedstring/2]).

-export([transform_to_EXTERNAL1990/1,
	 transform_to_EXTERNAL1994/1]).


check_bool(_Bool,asn1_DEFAULT) ->
    true;
check_bool(Bool,Bool) when Bool == true; Bool == false ->
    true;
check_bool(_Bool1,Bool2) ->
    throw({error,Bool2}).

check_int(_,asn1_DEFAULT,_) ->
    true;
check_int(Value,Value,_) when integer(Value) ->
    true;
check_int(DefValue,Value,NNL) when atom(Value) ->
    case lists:keysearch(Value,1,NNL) of
	{value,{_,DefValue}} ->
	    true;
	_ ->
	    throw({error,DefValue})
    end;
check_int(DefaultValue,_Value,_) ->
    throw({error,DefaultValue}).

% check_bitstring([H|T],[H|T],_) when integer(H) ->
%     true;
% check_bitstring(V,V,_) when integer(V) ->
%     true;
%% Two equal lists or integers
check_bitstring(_,asn1_DEFAULT,_) ->
    true;
check_bitstring(V,V,_) ->
    true;
%% Default value as a list of 1 and 0 and user value as an integer
check_bitstring(L=[H|T],Int,_) when integer(Int),integer(H) ->
    case bit_list_to_int(L,length(T)) of
	Int -> true;
	_ -> throw({error,L,Int})
    end;
%% Default value as an integer, val as list
check_bitstring(Int,Val,NBL) when integer(Int),list(Val) ->
    BL = int_to_bit_list(Int,[],length(Val)),
    check_bitstring(BL,Val,NBL);
%% Default value and user value as lists of ones and zeros
check_bitstring(L1=[H1|_T1],L2=[H2|_T2],NBL=[_H|_T]) when integer(H1),integer(H2) ->
    L2new = remove_trailing_zeros(L2),
    check_bitstring(L1,L2new,NBL);
%% Default value as a list of 1 and 0 and user value as a list of atoms
check_bitstring(L1=[H1|_T1],L2=[H2|_T2],NBL) when integer(H1),atom(H2) ->
    case bit_list_to_nbl(L1,NBL,0,[]) of
	L3 -> check_bitstring(L3,L2,NBL);
	_ -> throw({error,L2})
    end;
%% Both default value and user value as a list of atoms
check_bitstring(L1=[H1|T1],L2=[H2|_T2],_) when atom(H1),atom(H2) ->
    length(L1) == length(L2),
    case lists:member(H1,L2) of
	true ->
	    check_bitstring1(T1,L2);
	false -> throw({error,L2})
    end;
%% Default value as a list of atoms and user value as a list of 1 and 0
check_bitstring(L1=[H1|_T1],L2=[H2|_T2],NBL) when atom(H1),integer(H2) ->
    case bit_list_to_nbl(L2,NBL,0,[]) of
	L3 ->
	    check_bitstring(L1,L3,NBL);
	_ -> throw({error,L2})
    end;
%% User value in compact format
check_bitstring(DefVal,CBS={_,_},NBL) ->
    NewVal = cbs_to_bit_list(CBS),
    check_bitstring(DefVal,NewVal,NBL);
check_bitstring(DV,V,_) ->
    throw({error,DV,V}).


bit_list_to_int([0|Bs],ShL)->
    bit_list_to_int(Bs,ShL-1) + 0;
bit_list_to_int([1|Bs],ShL) ->
    bit_list_to_int(Bs,ShL-1) + (1 bsl ShL);
bit_list_to_int([],_) ->
    0.

int_to_bit_list(0,Acc,0) ->
    Acc;
int_to_bit_list(Int,Acc,Len) ->
    int_to_bit_list(Int bsr 1,[Int band 1|Acc],Len - 1).

bit_list_to_nbl([0|T],NBL,Pos,Acc) ->
    bit_list_to_nbl(T,NBL,Pos+1,Acc);
bit_list_to_nbl([1|T],NBL,Pos,Acc) ->
    case lists:keysearch(Pos,2,NBL) of
	{value,{N,_}} ->
	    bit_list_to_nbl(T,NBL,Pos+1,[N|Acc]);
	_ ->
	    throw({error,{no,named,element,at,pos,Pos}})
    end;
bit_list_to_nbl([],_,_,Acc) ->
    Acc.

remove_trailing_zeros(L2) ->
    remove_trailing_zeros1(lists:reverse(L2)).
remove_trailing_zeros1(L) ->
    lists:reverse(lists:dropwhile(fun(0)->true;
				     (_) ->false
				  end,
				  L)).

check_bitstring1([H|T],NBL) ->
    case lists:member(H,NBL) of
	true ->
	    check_bitstring1(T,NBL);
	V -> throw({error,V})
    end;
check_bitstring1([],_) ->
    true.

cbs_to_bit_list({Unused,<<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,Rest/binary>>}) when size(Rest) >= 1 ->
    [B7,B6,B5,B4,B3,B2,B1,B0|cbs_to_bit_list({Unused,Rest})];
cbs_to_bit_list({0,<<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1>>}) ->
    [B7,B6,B5,B4,B3,B2,B1,B0];
cbs_to_bit_list({Unused,Bin}) when size(Bin) == 1 ->
    Used = 8-Unused,
    <<Int:Used,_:Unused>> = Bin,
    int_to_bit_list(Int,[],Used).


check_octetstring(_,asn1_DEFAULT) ->
    true;
check_octetstring(L,L) ->
    true;
check_octetstring(L,Int) when list(L),integer(Int) ->
    case integer_to_octetlist(Int) of
	L -> true;
	V -> throw({error,V})
    end;
check_octetstring(_,V) ->
    throw({error,V}).

integer_to_octetlist(Int) ->
    integer_to_octetlist(Int,[]).
integer_to_octetlist(0,Acc) ->
    Acc;
integer_to_octetlist(Int,Acc) ->
    integer_to_octetlist(Int bsr 8,[(Int band 255)|Acc]).

check_null(_,asn1_DEFAULT) ->
    true;
check_null('NULL','NULL') ->
    true;
check_null(_,V) ->
    throw({error,V}).

check_objectidentifier(_,asn1_DEFAULT) ->
    true;
check_objectidentifier(OI,OI) ->
    true;
check_objectidentifier(DOI,OI) when tuple(DOI),tuple(OI) ->
    check_objectidentifier1(tuple_to_list(DOI),tuple_to_list(OI));
check_objectidentifier(_,OI) ->
    throw({error,OI}).

check_objectidentifier1([V|Rest1],[V|Rest2]) ->
    check_objectidentifier1(Rest1,Rest2,V);
check_objectidentifier1([V1|Rest1],[V2|Rest2]) ->
    case reserved_objectid(V2,[]) of
	V1 ->
	    check_objectidentifier1(Rest1,Rest2,[V1]);
	V ->
	    throw({error,V})
    end.
check_objectidentifier1([V|Rest1],[V|Rest2],Above) ->
    check_objectidentifier1(Rest1,Rest2,[V|Above]);
check_objectidentifier1([V1|Rest1],[V2|Rest2],Above) ->
    case reserved_objectid(V2,Above) of
	V1 ->
	    check_objectidentifier1(Rest1,Rest2,[V1|Above]);
	V ->
	    throw({error,V})
    end;
check_objectidentifier1([],[],_) ->
    true;
check_objectidentifier1(_,V,_) ->
    throw({error,object,identifier,V}).

%% ITU-T Rec. X.680 Annex B - D
reserved_objectid('itu-t',[]) -> 0;
reserved_objectid('ccitt',[]) -> 0;
%% arcs below "itu-t"
reserved_objectid('recommendation',[0]) -> 0;
reserved_objectid('question',[0]) -> 1;
reserved_objectid('administration',[0]) -> 2;
reserved_objectid('network-operator',[0]) -> 3;
reserved_objectid('identified-organization',[0]) -> 4;

reserved_objectid(iso,[]) -> 1;
%% arcs below "iso", note that number 1 is not used
reserved_objectid('standard',[1]) -> 0;
reserved_objectid('member-body',[1]) -> 2;
reserved_objectid('identified-organization',[1]) -> 3;

reserved_objectid('joint-iso-itu-t',[]) -> 2;
reserved_objectid('joint-iso-ccitt',[]) -> 2;

reserved_objectid(_,_) -> false.


check_objectdescriptor(_,asn1_DEFAULT) ->
    true;
check_objectdescriptor(OD,OD) ->
    true;
check_objectdescriptor(OD,OD) ->
    throw({error,{not_implemented_yet,check_objectdescriptor}}).

check_real(_,asn1_DEFAULT) ->
    true;
check_real(R,R) ->
    true;
check_real(_,_) ->
    throw({error,{not_implemented_yet,check_real}}).

check_enum(_,asn1_DEFAULT,_) ->
    true;
check_enum(Val,Val,_) ->
    true;
check_enum(Int,Atom,Enumerations) when integer(Int),atom(Atom) ->
    case lists:keysearch(Atom,1,Enumerations) of
	{value,{_,Int}} -> true;
	_ -> throw({error,{enumerated,Int,Atom}})
    end;
check_enum(DefVal,Val,_) ->
    throw({error,{enumerated,DefVal,Val}}).


check_restrictedstring(_,asn1_DEFAULT) ->
    true;
check_restrictedstring(Val,Val) ->
    true;
check_restrictedstring([V|Rest1],[V|Rest2]) ->
    check_restrictedstring(Rest1,Rest2);
check_restrictedstring([V1|Rest1],[V2|Rest2]) ->
    check_restrictedstring(V1,V2),
    check_restrictedstring(Rest1,Rest2);
%% tuple format of value
check_restrictedstring({V1,V2},[V1,V2]) ->
    true;
check_restrictedstring([V1,V2],{V1,V2}) ->
    true;
%% quadruple format of value
check_restrictedstring({V1,V2,V3,V4},[V1,V2,V3,V4]) ->
    true;
check_restrictedstring([V1,V2,V3,V4],{V1,V2,V3,V4}) ->
    true;
%% character string list
check_restrictedstring(V1,V2) when list(V1),tuple(V2) ->
    check_restrictedstring(V1,tuple_to_list(V2));
check_restrictedstring(V1,V2) ->
    throw({error,{restricted,string,V1,V2}}).

transform_to_EXTERNAL1990(Val) when tuple(Val),size(Val) == 4 ->
    transform_to_EXTERNAL1990(tuple_to_list(Val),[]);
transform_to_EXTERNAL1990(Val) when tuple(Val) ->
    %% Data already in ASN1 1990 format
    Val.

transform_to_EXTERNAL1990(['EXTERNAL'|Rest],Acc) ->
    transform_to_EXTERNAL1990(Rest,['EXTERNAL'|Acc]);
transform_to_EXTERNAL1990([{syntax,Syntax}|Rest],Acc) ->
    transform_to_EXTERNAL1990(Rest,[asn1_NOVALUE,Syntax|Acc]);
transform_to_EXTERNAL1990([{'presentation-context-id',PCid}|Rest],Acc) ->
    transform_to_EXTERNAL1990(Rest,[PCid,asn1_NOVALUE|Acc]);
transform_to_EXTERNAL1990([{'context-negotiation',Context_negot}|Rest],Acc) ->
    {_,Presentation_Cid,Transfer_syntax} = Context_negot,
    transform_to_EXTERNAL1990(Rest,[Transfer_syntax,Presentation_Cid|Acc]);
transform_to_EXTERNAL1990([asn1_NOVALUE|Rest],Acc) ->
    transform_to_EXTERNAL1990(Rest,[asn1_NOVALUE|Acc]);
transform_to_EXTERNAL1990([Data_val_desc,Data_value],Acc) when list(Data_value)->
    list_to_tuple(lists:reverse([{'octet-aligned',Data_value},
				 Data_val_desc|Acc]));
transform_to_EXTERNAL1990([Data_value],Acc) when list(Data_value)->
    list_to_tuple(lists:reverse([{'octet-aligned',Data_value}|Acc])).


transform_to_EXTERNAL1994(V={'EXTERNAL',DRef,IndRef,Data_v_desc,Encoding}) ->
    Identification =
	case {DRef,IndRef} of
	    {DRef,asn1_NOVALUE} ->
		{syntax,DRef};
	    {asn1_NOVALUE,IndRef} ->
		{'presentation-context-id',IndRef};
	     _ ->
		{'context-negotiation',
		 {'EXTERNAL_identification_context-negotiation',IndRef,DRef}}
	end,
    case Encoding of
	{_,Val} when list(Val) ->
	    {'EXTERNAL',Identification,Data_v_desc,Val};
	_  ->
	    V
    end.
