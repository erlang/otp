%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
-module(asn1rtt_check).

-export([check_bool/2,
	 check_int/3,
	 check_bitstring/2,check_named_bitstring/3,
	 check_octetstring/2,
	 check_null/2,
	 check_objectidentifier/2,
	 check_objectdescriptor/2,
	 check_real/2,
	 check_enum/3,
	 check_restrictedstring/2]).

check_bool(_Bool, asn1_DEFAULT) ->
    true;
check_bool(Bool, Bool) when is_boolean(Bool) ->
    true;
check_bool(_Bool1, Bool2) ->
    throw({error,Bool2}).

check_int(_, asn1_DEFAULT, _) ->
    true;
check_int(Value, Value, _) when is_integer(Value) ->
    true;
check_int(DefValue, Value, NNL) when is_atom(Value) ->
    case lists:keyfind(Value, 1, NNL) of
	{_,DefValue} ->
	    true;
	_ ->
	    throw({error,DefValue})
    end;
check_int(DefaultValue, _Value, _) ->
    throw({error,DefaultValue}).

%% check_bitstring(Default, UserBitstring) -> true|false
%%  Default = bitstring()
%%  UserBitstring = integeger() | list(0|1) | {Unused,binary()} | bitstring()
check_bitstring(_, asn1_DEFAULT) ->
    true;
check_bitstring(DefVal, {Unused,Binary}) ->
    %% User value in compact format.
    Sz = bit_size(Binary) - Unused,
    <<Val:Sz/bitstring,_:Unused>> = Binary,
    check_bitstring(DefVal, Val);
check_bitstring(DefVal, Val) when is_bitstring(Val) ->
    case Val =:= DefVal of
	false -> throw(error);
	true -> true
    end;
check_bitstring(Def, Val) when is_list(Val) ->
    check_bitstring_list(Def, Val);
check_bitstring(Def, Val) when is_integer(Val) ->
    check_bitstring_integer(Def, Val).

check_bitstring_list(<<H:1,T1/bitstring>>, [H|T2]) ->
    check_bitstring_list(T1, T2);
check_bitstring_list(<<>>, []) ->
    true;
check_bitstring_list(_, _) ->
    throw(error).

check_bitstring_integer(<<H:1,T1/bitstring>>, Int) when H =:= Int band 1 ->
    check_bitstring_integer(T1, Int bsr 1);
check_bitstring_integer(<<>>, 0) ->
    true;
check_bitstring_integer(_, _) ->
    throw(error).

check_named_bitstring(_, asn1_DEFAULT, _) ->
    true;
check_named_bitstring(V, V, _) ->
    true;
%% Default value and user value as lists of ones and zeros
check_named_bitstring(L1=[H1|_T1], L2=[H2|_T2], NBL=[_H|_T]) when is_integer(H1), is_integer(H2) ->
    L2new = remove_trailing_zeros(L2),
    check_named_bitstring(L1, L2new, NBL);
%% Default value as a list of 1 and 0 and user value as a list of atoms
check_named_bitstring(L1=[H1|_T1], L2=[H2|_T2], NBL) when is_integer(H1), is_atom(H2) ->
    L3 = bit_list_to_nbl(L1, NBL, 0, []),
    check_named_bitstring(L3, L2, NBL);
%% Both default value and user value as a list of atoms
check_named_bitstring(L1=[H1|T1], L2=[H2|_T2], _)
  when is_atom(H1), is_atom(H2), length(L1) =:= length(L2) ->
    case lists:member(H1, L2) of
	true ->
	    check_bitstring1(T1, L2);
	false -> throw({error,L2})
    end;
%% Default value as a list of atoms and user value as a list of 1 and 0
check_named_bitstring(L1=[H1|_T1], L2=[H2|_T2], NBL) when is_atom(H1), is_integer(H2) ->
    L3 = bit_list_to_nbl(L2, NBL, 0, []),
    check_named_bitstring(L1, L3, NBL);
%% User value in compact format
check_named_bitstring(DefVal,CBS={_,_}, NBL) ->
    NewVal = cbs_to_bit_list(CBS),
    check_named_bitstring(DefVal, NewVal, NBL);
%% User value as a binary
check_named_bitstring(DefVal, CBS, NBL) when is_binary(CBS) ->
    NewVal = cbs_to_bit_list({0,CBS}),
    check_named_bitstring(DefVal, NewVal, NBL);
%% User value as a bitstring
check_named_bitstring(DefVal, CBS, NBL) when is_bitstring(CBS) ->
    BitSize = bit_size(CBS),
    Unused = 8 - (BitSize band 7),
    NewVal = cbs_to_bit_list({Unused,<<CBS:BitSize/bits,0:Unused>>}),
    check_named_bitstring(DefVal, NewVal, NBL);
check_named_bitstring(DV, V, _) ->
    throw({error,DV,V}).

int_to_bit_list(0, Acc, 0) ->
    Acc;
int_to_bit_list(Int, Acc, Len) when Len > 0 ->
    int_to_bit_list(Int bsr 1, [Int band 1|Acc], Len - 1).

bit_list_to_nbl([0|T], NBL, Pos, Acc) ->
    bit_list_to_nbl(T, NBL, Pos+1, Acc);
bit_list_to_nbl([1|T], NBL, Pos, Acc) ->
    case lists:keyfind(Pos, 2, NBL) of
	{N,_} ->
	    bit_list_to_nbl(T, NBL, Pos+1, [N|Acc]);
	_ ->
	    throw({error,{no,named,element,at,pos,Pos}})
    end;
bit_list_to_nbl([], _, _, Acc) ->
    Acc.

remove_trailing_zeros(L2) ->
    remove_trailing_zeros1(lists:reverse(L2)).
remove_trailing_zeros1(L) ->
    lists:reverse(lists:dropwhile(fun(0)->true;
				     (_) ->false
				  end,
				  L)).

check_bitstring1([H|T], NBL) ->
    case lists:member(H, NBL) of
	true -> check_bitstring1(T, NBL);
	V -> throw({error,V})
    end;
check_bitstring1([], _) ->
    true.

cbs_to_bit_list({Unused, <<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,Rest/binary>>}) when byte_size(Rest) >= 1 ->
    [B7,B6,B5,B4,B3,B2,B1,B0|cbs_to_bit_list({Unused,Rest})];
cbs_to_bit_list({0,<<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1>>}) ->
    [B7,B6,B5,B4,B3,B2,B1,B0];
cbs_to_bit_list({Unused,Bin}) when byte_size(Bin) =:= 1 ->
    Used = 8-Unused,
    <<Int:Used,_:Unused>> = Bin,
    int_to_bit_list(Int, [], Used).


check_octetstring(_, asn1_DEFAULT) ->
    true;
check_octetstring(L, L) ->
    true;
check_octetstring(L, Int) when is_list(L), is_integer(Int) ->
    case integer_to_octetlist(Int) of
	L -> true;
	V -> throw({error,V})
    end;
check_octetstring(_, V) ->
    throw({error,V}).

integer_to_octetlist(Int) ->
    integer_to_octetlist(Int, []).
integer_to_octetlist(0, Acc) ->
    Acc;
integer_to_octetlist(Int, Acc) ->
    integer_to_octetlist(Int bsr 8, [(Int band 255)|Acc]).

check_null(_, asn1_DEFAULT) ->
    true;
check_null('NULL', 'NULL') ->
    true;
check_null(_, V) ->
    throw({error,V}).

check_objectidentifier(_, asn1_DEFAULT) ->
    true;
check_objectidentifier(OI, OI) ->
    true;
check_objectidentifier(DOI, OI) when is_tuple(DOI), is_tuple(OI) ->
    check_objectidentifier1(tuple_to_list(DOI), tuple_to_list(OI));
check_objectidentifier(_, OI) ->
    throw({error,OI}).

check_objectidentifier1([V|Rest1], [V|Rest2]) ->
    check_objectidentifier1(Rest1, Rest2, V);
check_objectidentifier1([V1|Rest1], [V2|Rest2]) ->
    case reserved_objectid(V2, []) of
	V1 ->
	    check_objectidentifier1(Rest1, Rest2, [V1]);
	V ->
	    throw({error,V})
    end.
check_objectidentifier1([V|Rest1], [V|Rest2], Above) ->
    check_objectidentifier1(Rest1, Rest2, [V|Above]);
check_objectidentifier1([V1|Rest1], [V2|Rest2], Above) ->
    case reserved_objectid(V2, Above) of
	V1 ->
	    check_objectidentifier1(Rest1, Rest2, [V1|Above]);
	V ->
	    throw({error,V})
    end;
check_objectidentifier1([], [], _) ->
    true;
check_objectidentifier1(_, V, _) ->
    throw({error,object,identifier,V}).

%% ITU-T Rec. X.680 Annex B - D
reserved_objectid('itu-t', []) -> 0;
reserved_objectid('ccitt', []) -> 0;
%% arcs below "itu-t"
reserved_objectid('recommendation', [0]) -> 0;
reserved_objectid('question', [0]) -> 1;
reserved_objectid('administration', [0]) -> 2;
reserved_objectid('network-operator', [0]) -> 3;
reserved_objectid('identified-organization', [0]) -> 4;

reserved_objectid(iso, []) -> 1;
%% arcs below "iso", note that number 1 is not used
reserved_objectid('standard', [1]) -> 0;
reserved_objectid('member-body', [1]) -> 2;
reserved_objectid('identified-organization', [1]) -> 3;

reserved_objectid('joint-iso-itu-t', []) -> 2;
reserved_objectid('joint-iso-ccitt', []) -> 2;

reserved_objectid(_, _) -> false.


check_objectdescriptor(_, asn1_DEFAULT) ->
    true;
check_objectdescriptor(OD, OD) ->
    true;
check_objectdescriptor(OD, OD) ->
    throw({error,{not_implemented_yet,check_objectdescriptor}}).

check_real(_, asn1_DEFAULT) ->
    true;
check_real(R, R) ->
    true;
check_real(_, _) ->
    throw({error,{not_implemented_yet,check_real}}).

check_enum(_, asn1_DEFAULT, _) ->
    true;
check_enum(Val, Val, _) ->
    true;
check_enum(Int, Atom, Enumerations) when is_integer(Int), is_atom(Atom) ->
    case lists:keyfind(Atom, 1, Enumerations) of
	{_,Int} -> true;
	_ -> throw({error,{enumerated,Int,Atom}})
    end;
check_enum(DefVal, Val, _) ->
    throw({error,{enumerated,DefVal,Val}}).


check_restrictedstring(_, asn1_DEFAULT) ->
    true;
check_restrictedstring(Val, Val) ->
    true;
check_restrictedstring([V|Rest1], [V|Rest2]) ->
    check_restrictedstring(Rest1, Rest2);
check_restrictedstring([V1|Rest1], [V2|Rest2]) ->
    check_restrictedstring(V1, V2),
    check_restrictedstring(Rest1, Rest2);
%% tuple format of value
check_restrictedstring({V1,V2}, [V1,V2]) ->
    true;
check_restrictedstring([V1,V2], {V1,V2}) ->
    true;
%% quadruple format of value
check_restrictedstring({V1,V2,V3,V4}, [V1,V2,V3,V4]) ->
    true;
check_restrictedstring([V1,V2,V3,V4], {V1,V2,V3,V4}) ->
    true;
%% character string list
check_restrictedstring(V1, V2) when is_list(V1), is_tuple(V2) ->
    check_restrictedstring(V1, tuple_to_list(V2));
check_restrictedstring(V1, V2) ->
    throw({error,{restricted,string,V1,V2}}).
