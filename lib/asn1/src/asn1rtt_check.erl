%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(asn1rtt_check).

-export([check_fail/1,
	 check_int/3,
	 check_legacy_bitstring/2,
	 check_legacy_named_bitstring/3,
	 check_legacy_named_bitstring/4,
	 check_named_bitstring/3,
	 check_named_bitstring/4,
	 check_literal_sof/2,
	 check_octetstring/2,
	 check_objectidentifier/2,
	 check_objectdescriptor/2,
	 check_real/2,
	 check_restrictedstring/2]).

check_fail(_) ->
    throw(false).

check_int(Value, Value, _) when is_integer(Value) ->
    true;
check_int(Value, DefValue, NNL) when is_atom(Value) ->
    case lists:keyfind(Value, 1, NNL) of
	{_,DefValue} ->
	    true;
	_ ->
	    throw(false)
    end;
check_int(_, _, _) ->
    throw(false).

check_legacy_bitstring(Value, Default) ->
    check_bitstring(Default, Value).

%% check_bitstring(Default, UserBitstring) -> true|false
%%  Default = bitstring()
%%  UserBitstring = integeger() | list(0|1) | {Unused,binary()} | bitstring()
check_bitstring(DefVal, {Unused,Binary}) ->
    %% User value in compact format.
    Sz = bit_size(Binary) - Unused,
    <<Val:Sz/bitstring,_:Unused>> = Binary,
    check_bitstring(DefVal, Val);
check_bitstring(DefVal, Val) when is_bitstring(Val) ->
    case Val =:= DefVal of
	false -> throw(false);
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
    throw(false).

check_bitstring_integer(<<H:1,T1/bitstring>>, Int) when H =:= Int band 1 ->
    check_bitstring_integer(T1, Int bsr 1);
check_bitstring_integer(<<>>, 0) ->
    true;
check_bitstring_integer(_, _) ->
    throw(false).

check_legacy_named_bitstring([Int|_]=Val, Bs, BsSize) when is_integer(Int) ->
    check_named_bitstring(<< <<B:1>> || B <- Val >>, Bs, BsSize);
check_legacy_named_bitstring({Unused,Val0}, Bs, BsSize) ->
    Sz = bit_size(Val0) - Unused,
    <<Val:Sz/bits,_/bits>> = Val0,
    check_named_bitstring(Val, Bs, BsSize);
check_legacy_named_bitstring(Val, Bs, BsSize) when is_integer(Val) ->
    L = legacy_int_to_bitlist(Val),
    check_named_bitstring(<< <<B:1>> || B <- L >>, Bs, BsSize);
check_legacy_named_bitstring(Val, Bs, BsSize) ->
    check_named_bitstring(Val, Bs, BsSize).

check_legacy_named_bitstring([Int|_]=Val, Names, Bs, BsSize) when is_integer(Int) ->
    check_named_bitstring(<< <<B:1>> || B <- Val >>, Names, Bs, BsSize);
check_legacy_named_bitstring({Unused,Val0}, Names, Bs, BsSize) ->
    Sz = bit_size(Val0) - Unused,
    <<Val:Sz/bits,_/bits>> = Val0,
    check_named_bitstring(Val, Names, Bs, BsSize);
check_legacy_named_bitstring(Val, Names, Bs, BsSize) when is_integer(Val) ->
    L = legacy_int_to_bitlist(Val),
    check_named_bitstring(<< <<B:1>> || B <- L >>, Names, Bs, BsSize);
check_legacy_named_bitstring(Val, Names, Bs, BsSize) ->
    check_named_bitstring(Val, Names, Bs, BsSize).

legacy_int_to_bitlist(0) ->
    [];
legacy_int_to_bitlist(Int) ->
    [Int band 1|legacy_int_to_bitlist(Int bsr 1)].

check_named_bitstring(Bs, Bs, _) ->
    true;
check_named_bitstring(Val, Bs, BsSize) ->
    Rest = bit_size(Val) - BsSize,
    case Val of
	<<Bs:BsSize/bits,0:Rest>> ->
	    true;
	_ ->
	    throw(false)
    end.

check_named_bitstring([_|_]=Val, Names, _, _) ->
    case lists:sort(Val) of
	Names -> true;
	_ -> throw(false)
    end;
check_named_bitstring(Bs, _, Bs, _) ->
    true;
check_named_bitstring(Val, _, Bs, BsSize) ->
    Rest = bit_size(Val) - BsSize,
    case Val of
	<<Bs:BsSize/bits,0:Rest>> ->
	    true;
	_ ->
	    throw(false)
    end.

check_octetstring(V, V) ->
    true;
check_octetstring(V, Def) when is_list(V) ->
    case list_to_binary(V) of
	Def -> true;
	_ -> throw(false)
    end;
check_octetstring(_, _) ->
    throw(false).

check_objectidentifier(Value, {Prefix,Tail}) when is_tuple(Value) ->
    check_oid(tuple_to_list(Value), Prefix, Tail);
check_objectidentifier(_, _) ->
    throw(false).

check_oid([H|T], [K|Ks], Tail) ->
    case lists:member(H, K) of
	false -> throw(false);
	true -> check_oid(T, Ks, Tail)
    end;
check_oid(Tail, [], Tail) ->
    true;
check_oid(_, _, _) ->
    throw(false).

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
check_restrictedstring(V1, V2) when is_tuple(V1) ->
    check_restrictedstring(tuple_to_list(V1), V2);
check_restrictedstring(_, _) ->
    throw(false).

check_literal_sof(Value, Default) ->
    case lists:sort(Value) of
	Default ->
	    true;
	_ ->
	    throw(false)
    end.
