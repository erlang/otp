%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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
%%
-module(asn1rtt_per_common).

-include("asn1_records.hrl").

-export([decode_fragmented/3,
	 decode_compact_bit_string/1,
	 decode_legacy_bit_string/1,
	 decode_named_bit_string/2,
	 decode_chars/2,decode_chars/3,
	 decode_chars_16bit/1,
	 decode_big_chars/2,
	 decode_oid/1,decode_relative_oid/1,
	 encode_chars/2,encode_chars/3,
	 encode_chars_compact_map/3,
	 encode_chars_16bit/1,encode_big_chars/1,
	 encode_fragmented/2,
	 encode_oid/1,encode_relative_oid/1,
	 encode_unconstrained_number/1,
	 bitstring_from_positions/1,bitstring_from_positions/2,
	 to_bitstring/1,to_bitstring/2,
	 to_named_bitstring/1,to_named_bitstring/2,
	 bs_drop_trailing_zeroes/1,adjust_trailing_zeroes/2,
	 is_default_bitstring/3,is_default_bitstring/5,
	 extension_bitmap/3,
	 open_type_to_binary/1,legacy_open_type_to_binary/1]).

-define('16K',16384).

decode_fragmented(SegSz0, Buf0, Unit) ->
    SegSz = SegSz0 * Unit * ?'16K',
    <<Res:SegSz/bitstring,Buf/bitstring>> = Buf0,
    decode_fragmented_1(Buf, Unit, Res).

decode_fragmented_1(<<0:1,N:7,Buf0/bitstring>>, Unit, Res) ->
    Sz = N*Unit,
    <<S:Sz/bitstring,Buf/bitstring>> = Buf0,
    {<<Res/bitstring,S/bitstring>>,Buf};
decode_fragmented_1(<<1:1,0:1,N:14,Buf0/bitstring>>, Unit, Res) ->
    Sz = N*Unit,
    <<S:Sz/bitstring,Buf/bitstring>> = Buf0,
    {<<Res/bitstring,S/bitstring>>,Buf};
decode_fragmented_1(<<1:1,1:1,SegSz0:6,Buf0/bitstring>>, Unit, Res0) ->
    SegSz = SegSz0 * Unit * ?'16K',
    <<Frag:SegSz/bitstring,Buf/bitstring>> = Buf0,
    Res = <<Res0/bitstring,Frag/bitstring>>,
    decode_fragmented_1(Buf, Unit, Res).

decode_named_bit_string(Val, NNL) ->
    Bits = [B || <<B:1>> <= Val],
    decode_named_bit_string_1(0, Bits, NNL, []).

decode_legacy_bit_string(Val) ->
    [B || <<B:1>> <= Val].

decode_compact_bit_string(Val) ->
    PadLen = (8 - (bit_size(Val) band 7)) band 7,
    {PadLen,<<Val/bitstring,0:PadLen>>}.

decode_chars(Val, N) ->
    [C || <<C:N>> <= Val].

decode_chars(Val, N, Chars) ->
    [element(C+1, Chars) || <<C:N>> <= Val].

decode_chars_16bit(Val) ->
    Cs = [C || <<C:16>> <= Val],
    decode_chars_16bit_1(Cs).

decode_big_chars(Val, N) ->
    decode_big_chars_1(decode_chars(Val, N)).

decode_oid(Octets) ->
    [First|Rest] = dec_subidentifiers(Octets, 0, []),
    Idlist = if
		 First < 40 ->
		     [0,First|Rest];
		 First < 80 ->
		     [1,First - 40|Rest];
		 true ->
		     [2,First - 80|Rest]
	     end,
    list_to_tuple(Idlist).

decode_relative_oid(Octets) ->
    list_to_tuple(dec_subidentifiers(Octets, 0, [])).

encode_chars(Val, NumBits) ->
    << <<C:NumBits>> || C <- Val >>.

encode_chars(Val, NumBits, {Lb,Tab}) ->
    << <<(enc_char(C, Lb, Tab)):NumBits>> || C <- Val >>.

encode_chars_compact_map(Val, NumBits, {Lb,Limit}) ->
    << <<(enc_char_cm(C, Lb, Limit)):NumBits>> || C <- Val >>.

encode_chars_16bit(Val) ->
    L = [case C of
	     {0,0,A,B} -> [A,B];
	     C when is_integer(C) -> [0,C]
	 end || C <- Val],
    iolist_to_binary(L).

encode_big_chars(Val) ->
    L = [case C of
	     {_,_,_,_} -> tuple_to_list(C);
	     C when is_integer(C) -> [<<0,0,0>>,C]
	 end || C <- Val],
    iolist_to_binary(L).

encode_fragmented(Bin, Unit) ->
    encode_fragmented_1(Bin, Unit, 4).

encode_oid(Val) when is_tuple(Val) ->
    encode_oid(tuple_to_list(Val));
encode_oid(Val) ->
    iolist_to_binary(e_object_identifier(Val)).

encode_relative_oid(Val) when is_tuple(Val) ->
    encode_relative_oid(tuple_to_list(Val));
encode_relative_oid(Val) when is_list(Val) ->
    list_to_binary([e_object_element(X)||X <- Val]).

encode_unconstrained_number(Val) when not is_integer(Val) ->
    exit({error,{asn1,{illegal_integer,Val}}});
encode_unconstrained_number(Val) when Val >= 0 ->
    if
	Val < 16#80 ->
	    [1,Val];
	Val < 16#100 ->
	    [<<2,0>>,Val];
	true ->
	    case binary:encode_unsigned(Val) of
		<<0:1,_/bitstring>>=Bin ->
		    case byte_size(Bin) of
			Sz when Sz < 128 ->
			    [Sz,Bin];
			Sz when Sz < 16384 ->
			    [<<2:2,Sz:14>>,Bin]
		    end;
		<<1:1,_/bitstring>>=Bin ->
		    case byte_size(Bin)+1 of
			Sz when Sz < 128 ->
			    [Sz,0,Bin];
			Sz when Sz < 16384 ->
			    [<<2:2,Sz:14,0:8>>,Bin]
		    end
	    end
    end;
encode_unconstrained_number(Val) ->
    Oct = enint(Val, []),
    Len = length(Oct),
    if
        Len < 128 ->
            [Len|Oct];
        Len < 16384 ->
            [<<2:2,Len:14>>|Oct]
    end.

%% bitstring_from_positions([Position]) -> BitString
%%  Given an unsorted list of bit positions (0..MAX), construct
%%  a BIT STRING. The rightmost bit will always be a one.

bitstring_from_positions([]) -> <<>>;
bitstring_from_positions([_|_]=L0) ->
    L1 = lists:sort(L0),
    L = diff(L1, -1),
    << <<1:(N+0)>> || N <- L >>.

%% bitstring_from_positions([Position], Lb) -> BitString
%%  Given an unsorted list of bit positions (0..MAX) and a lower bound
%%  for the number of bits, construct BIT STRING (zero-padded on the
%%  right side if needed).

bitstring_from_positions(L0, Lb) ->
    L1 = lists:sort(L0),
    L = diff(L1, -1, Lb-1),
    << <<B:(N+0)>> || {B,N} <- L >>.

%% to_bitstring(Val) -> BitString
%%    Val = BitString | {Unused,Binary} | [OneOrZero] | Integer
%%  Given one of the possible representations for a BIT STRING,
%%  return a bitstring (without adding or removing any zero bits
%%  at the right end).

to_bitstring({0,Bs}) when is_binary(Bs) ->
    Bs;
to_bitstring({Unused,Bs0}) when is_binary(Bs0) ->
    Sz = bit_size(Bs0) - Unused,
    <<Bs:Sz/bits,_/bits>> = Bs0,
    Bs;
to_bitstring(Bs) when is_bitstring(Bs) ->
    Bs;
to_bitstring(Int) when is_integer(Int), Int >= 0 ->
    L = int_to_bitlist(Int),
    << <<B:1>> || B <- L >>;
to_bitstring(L) when is_list(L) ->
    << <<B:1>> || B <- L >>.

%% to_bitstring(Val, Lb) -> BitString
%%    Val = BitString | {Unused,Binary} | [OneOrZero] | Integer
%%    Lb = Integer
%%  Given one of the possible representations for a BIT STRING
%%  and the lower bound for the number of bits,
%%  return a bitstring at least Lb bits long (padded with zeroes
%%  if needed).

to_bitstring({0,Bs}, Lb) when is_binary(Bs) ->
    case bit_size(Bs) of
	Sz when Sz < Lb ->
	    <<Bs/bits,0:(Lb-Sz)>>;
	_ ->
	    Bs
    end;
to_bitstring({Unused,Bs0}, Lb) when is_binary(Bs0) ->
    Sz = bit_size(Bs0) - Unused,
    if
	Sz < Lb ->
	    <<Bs0:Sz/bits,0:(Lb-Sz)>>;
	true ->
	    <<Bs:Sz/bits,_/bits>> = Bs0,
	    Bs
    end;
to_bitstring(Bs, Lb) when is_bitstring(Bs) ->
    adjust_size(Bs, Lb);
to_bitstring(Int, Lb) when is_integer(Int), Int >= 0 ->
    L = int_to_bitlist(Int),
    Bs = << <<B:1>> || B <- L >>,
    adjust_size(Bs, Lb);
to_bitstring(L, Lb) when is_list(L) ->
    Bs = << <<B:1>> || B <- L >>,
    adjust_size(Bs, Lb).

%% to_named_bitstring(Val) -> BitString
%%    Val = BitString | {Unused,Binary} | [OneOrZero] | Integer
%%  Given one of the possible representations for a BIT STRING,
%%  return a bitstring where any trailing zeroes have been stripped.

to_named_bitstring(Val) ->
    Bs = to_bitstring(Val),
    bs_drop_trailing_zeroes(Bs).

%% to_named_bitstring(Val, Lb) -> BitString
%%    Val = BitString | {Unused,Binary} | [OneOrZero] | Integer
%%    Lb = Integer
%%  Given one of the possible representations for a BIT STRING
%%  and the lower bound for the number of bits,
%%  return a bitstring that is at least Lb bits long. There will
%%  be zeroes at the right only if needed to reach the lower bound
%%  for the number of bits.

to_named_bitstring({0,Bs}, Lb) when is_binary(Bs) ->
    adjust_trailing_zeroes(Bs, Lb);
to_named_bitstring({Unused,Bs0}, Lb) when is_binary(Bs0) ->
    Sz = bit_size(Bs0) - Unused,
    <<Bs:Sz/bits,_/bits>> = Bs0,
    adjust_trailing_zeroes(Bs, Lb);
to_named_bitstring(Bs, Lb) when is_bitstring(Bs) ->
    adjust_trailing_zeroes(Bs, Lb);
to_named_bitstring(Val, Lb) ->
    %% Obsolete representations: list or integer. Optimize
    %% for correctness, not speed.
    adjust_trailing_zeroes(to_bitstring(Val), Lb).

is_default_bitstring(asn1_DEFAULT, _, _) ->
    true;
is_default_bitstring(Named, Named, _) ->
    true;
is_default_bitstring(Bs, _, Bs) ->
    true;
is_default_bitstring(Val, _, Def) when is_bitstring(Val) ->
    Sz = bit_size(Def),
    case Val of
	<<Def:Sz/bitstring,T/bitstring>> ->
	    NumZeroes = bit_size(T),
	    case T of
		<<0:NumZeroes>> -> true;
		_ -> false
	    end;
	_ ->
	    false
    end.

is_default_bitstring(asn1_DEFAULT, _, _, _, _) ->
    true;
is_default_bitstring({Unused,Bin}, V0, V1, V2, V3) when is_integer(Unused) ->
    %% Convert compact bitstring to a bitstring.
    Sz = bit_size(Bin) - Unused,
    <<Bs:Sz/bitstring,_:Unused>> = Bin,
    is_default_bitstring(Bs, V0, V1, V2, V3);
is_default_bitstring(Named, Named, _, _, _) ->
    true;
is_default_bitstring(Bs, _, Bs, _, _) ->
    true;
is_default_bitstring(List, _, _, List, _) ->
    true;
is_default_bitstring(Int, _, _, _, Int) ->
    true;
is_default_bitstring(Val, _, Def, _, _) when is_bitstring(Val) ->
    Sz = bit_size(Def),
    case Val of
	<<Def:Sz/bitstring,T/bitstring>> ->
	    NumZeroes = bit_size(T),
	    case T of
		<<0:NumZeroes>> -> true;
		_ -> false
	    end;
	_ ->
	    false
    end;
is_default_bitstring(Val, _, _, List, _) when is_list(Val) ->
    is_default_bitstring_list(List, Val);
is_default_bitstring(_, _, _, _, _) -> false.

extension_bitmap(Val, Pos, Limit) ->
    extension_bitmap(Val, Pos, Limit, 0).

open_type_to_binary({asn1_OPENTYPE,Bin}) when is_binary(Bin) ->
    Bin.

legacy_open_type_to_binary({asn1_OPENTYPE,Bin}) when is_binary(Bin) ->
    Bin;
legacy_open_type_to_binary(Bin) when is_binary(Bin) ->
    Bin;
legacy_open_type_to_binary(List) when is_list(List) ->
    List.

%%%
%%% Internal functions.
%%%

decode_named_bit_string_1(Pos, [0|Bt], Names, Acc) ->
    decode_named_bit_string_1(Pos+1, Bt, Names, Acc);
decode_named_bit_string_1(Pos, [1|Bt], Names, Acc) ->
    case lists:keyfind(Pos, 2, Names) of
	{Name,_} ->
	    decode_named_bit_string_1(Pos+1, Bt, Names, [Name|Acc]);
	false ->
	    decode_named_bit_string_1(Pos+1, Bt, Names, [{bit,Pos}|Acc])
    end;
decode_named_bit_string_1(_Pos, [], _Names, Acc) ->
    lists:reverse(Acc).

decode_chars_16bit_1([H|T]) when H < 256 ->
    [H|decode_chars_16bit_1(T)];
decode_chars_16bit_1([H|T]) ->
    [{0,0,H bsr 8,H band 255}|decode_chars_16bit_1(T)];
decode_chars_16bit_1([]) -> [].

decode_big_chars_1([H|T]) when H < 256 ->
    [H|decode_big_chars_1(T)];
decode_big_chars_1([H|T]) ->
    [list_to_tuple(binary_to_list(<<H:32>>))|decode_big_chars_1(T)];
decode_big_chars_1([]) -> [].

dec_subidentifiers([H|T], Av, Al) when H >=16#80 ->
    dec_subidentifiers(T, (Av bsl 7) bor (H band 16#7F), Al);
dec_subidentifiers([H|T], Av, Al) ->
    dec_subidentifiers(T, 0, [(Av bsl 7) bor H|Al]);
dec_subidentifiers([], _Av, Al) ->
    lists:reverse(Al).

enc_char(C0, Lb, Tab) ->
    try	element(C0-Lb, Tab) of
	ill ->
	    illegal_char_error();
	C ->
	    C
    catch
	error:badarg ->
	    illegal_char_error()
    end.

enc_char_cm(C0, Lb, Limit) ->
    C = C0 - Lb,
    if
	0 =< C, C < Limit ->
	    C;
	true ->
	    illegal_char_error()
    end.

illegal_char_error() ->
    error({error,{asn1,"value forbidden by FROM constraint"}}).

encode_fragmented_1(Bin, Unit, N) ->
    SegSz = Unit * N * ?'16K',
    case Bin of
	<<B:SegSz/bitstring,T/bitstring>> ->
	    [<<3:2,N:6>>,B|encode_fragmented_1(T, Unit, N)];
	_ when N > 1 ->
	    encode_fragmented_1(Bin, Unit, N-1);
	_ ->
	    case bit_size(Bin) div Unit of
		Len when Len < 128 ->
		    [Len,Bin];
		Len when Len < 16384 ->
		    [<<2:2,Len:14>>,Bin]
	    end
    end.

%% E1 = 0|1|2 and (E2 < 40 when E1 = 0|1)
e_object_identifier([E1,E2|Tail]) when E1 >= 0, E1 < 2, E2 < 40; E1 =:= 2 ->
    Head = 40*E1 + E2,
    e_object_elements([Head|Tail], []);
e_object_identifier([_,_|_Tail]=Oid) ->
    exit({error,{asn1,{'illegal_value',Oid}}}).

e_object_elements([], Acc) ->
    lists:reverse(Acc);
e_object_elements([H|T], Acc) ->
    e_object_elements(T, [e_object_element(H)|Acc]).

e_object_element(Num) when Num < 128 ->
    [Num];
e_object_element(Num) ->
    [e_o_e(Num bsr 7)|[Num band 2#1111111]].

e_o_e(Num) when Num < 128 ->
    Num bor 2#10000000;
e_o_e(Num) ->
    [e_o_e(Num bsr 7)|[(Num band 2#1111111) bor 2#10000000]].

enint(-1, [B1|T]) when B1 > 127 ->
    [B1|T];
enint(N, Acc) ->
    enint(N bsr 8, [N band 16#ff|Acc]).

diff([H|T], Prev) ->
    [H-Prev|diff(T, H)];
diff([], _) -> [].

diff([H|T], Prev, Last) ->
    [{1,H-Prev}|diff(T, H, Last)];
diff([], Prev, Last) when Last >= Prev ->
    [{0,Last-Prev}];
diff([], _, _) -> [].

int_to_bitlist(0) -> [];
int_to_bitlist(Int) -> [Int band 1|int_to_bitlist(Int bsr 1)].

adjust_size(Bs, Lb) ->
    case bit_size(Bs) of
	Sz when Sz < Lb ->
	    <<Bs:Sz/bits,0:(Lb-Sz)>>;
	_ ->
	    Bs
    end.

adjust_trailing_zeroes(Bs0, Lb) ->
    case bit_size(Bs0) of
	Sz when Sz < Lb ->
	    %% Too short - pad with zeroes.
	    <<Bs0:Sz/bits,0:(Lb-Sz)>>;
	Lb ->
	    %% Exactly the right size - nothing to do.
	    Bs0;
	_ ->
	    %% Longer than the lower bound - drop trailing zeroes.
	    <<_:Lb/bits,Tail/bits>> = Bs0,
	    Sz = Lb + bit_size(bs_drop_trailing_zeroes(Tail)),
	    <<Bs:Sz/bits,_/bits>> = Bs0,
	    Bs
    end.

bs_drop_trailing_zeroes(Bs) ->
    bs_drop_trailing_zeroes(Bs, bit_size(Bs)).

bs_drop_trailing_zeroes(Bs, 0) ->
    Bs;
bs_drop_trailing_zeroes(Bs0, Sz0) when Sz0 < 8 ->
    <<Byte:Sz0>> = Bs0,
    Sz = Sz0 - ntz(Byte),
    <<Bs:Sz/bits,_/bits>> = Bs0,
    Bs;
bs_drop_trailing_zeroes(Bs0, Sz0) ->
    Sz1 = Sz0 - 8,
    <<Bs1:Sz1/bits,Byte:8>> = Bs0,
    case ntz(Byte) of
	8 ->
	    bs_drop_trailing_zeroes(Bs1, Sz1);
	Ntz ->
	    Sz = Sz0 - Ntz,
	    <<Bs:Sz/bits,_:Ntz/bits>> = Bs0,
	    Bs
    end.

%% ntz(Byte) -> Number of trailing zeroes.
ntz(Byte) ->
    %% The table was calculated like this:
    %%   NTZ = fun (B, N, NTZ) when B band 1 =:= 0 -> NTZ(B bsr 1, N+1, NTZ); (_, N, _) -> N end.
    %%   io:format("~w\n", [list_to_tuple([NTZ(B+256, 0, NTZ) || B <- lists:seq(0, 255)])]).
    T = {8,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 7,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
	 4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0},
    element(Byte+1, T).

is_default_bitstring_list([H|Def], [H|Val]) ->
    is_default_bitstring_list(Def, Val);
is_default_bitstring_list([], []) ->
    true;
is_default_bitstring_list([], [_|_]=Val) ->
    lists:all(fun(0) -> true;
		 (_) -> false
	      end, Val);
is_default_bitstring_list(_, _) -> false.

extension_bitmap(_Val, Pos, Limit, Acc) when Pos >= Limit ->
    Acc;
extension_bitmap(Val, Pos, Limit, Acc) ->
    Bit = case element(Pos, Val) of
	      asn1_NOVALUE -> 0;
	      asn1_DEFAULT -> 0;
	      _ -> 1
	  end,
    extension_bitmap(Val, Pos+1, Limit, (Acc bsl 1) bor Bit).
