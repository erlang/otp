%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
-module(asn1rtt_real_common).

-export([encode_real/1,decode_real/1,
	 ber_encode_real/1]).

%%============================================================================
%%
%% Real value, ITU_T X.690 Chapter 8.5
%%============================================================================
%%
%% encode real value
%%============================================================================

ber_encode_real(0) ->
    {[],0};
ber_encode_real('PLUS-INFINITY') ->
    {[64],1};
ber_encode_real('MINUS-INFINITY') ->
    {[65],1};
ber_encode_real(Val) when is_tuple(Val); is_list(Val) ->
    encode_real(Val).

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

encode_real(Real) ->
    encode_real([], Real).

encode_real(_C, {Mantissa, Base, Exponent}) when Base =:= 2 ->
%%    io:format("Mantissa: ~w Base: ~w, Exp: ~w~n",[Man, Base, Exp]),
    {Man,ExpAdd} = truncate_zeros(Mantissa), %% DER adjustment
    Exp = Exponent + ExpAdd,
    OctExp = if Exp >= 0 -> list_to_binary(encode_pos_integer(Exp, []));
		true     -> list_to_binary(encode_neg_integer(Exp, []))
	     end,
%%    ok = io:format("OctExp: ~w~n",[OctExp]),
    SignBit = if  Man > 0 -> 0;  % bit 7 is pos or neg, no Zeroval
		  true -> 1
	      end,
%%    ok = io:format("SignBitMask: ~w~n",[SignBitMask]),
    SFactor = 0,
    OctExpLen = byte_size(OctExp),
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
    OctMantissa = if Man > 0 -> list_to_binary(real_mininum_octets(Man));
		     true    -> list_to_binary(real_mininum_octets(-(Man))) % signbit keeps track of sign
		  end,
    %%    ok = io:format("LenMask: ~w EOctets: ~w~nFirstOctet: ~w OctMantissa: ~w OctExpLen: ~w~n", [LenMask, EOctets, FirstOctet, OctMantissa, OctExpLen]),
    <<FirstOctet/binary, EOctets/binary, OctMantissa/binary>>;
encode_real(C, {Mantissa,Base,Exponent})
  when Base =:= 10, is_integer(Mantissa), is_integer(Exponent) ->
    %% always encode as NR3 due to DER on the format
    %% mmmm.Eseeee where
    %% m := digit
    %% s := '-' | '+' | []
    %% '+' only allowed in +0
    %% e := digit
    %% ex: 1234.E-5679
    ManStr = integer_to_list(Mantissa),

    encode_real_as_string(C,ManStr,Exponent);
encode_real(_C, {_,Base,_}) ->
    exit({error,{asn1, {encode_real_non_supported_encoding, Base}}});
%% base 10
encode_real(C, Real) when is_list(Real) ->
    %% The Real string may come in as a NR1, NR2 or NR3 string.
    {Mantissa, Exponent} =
	case string:lexemes(Real,"Ee") of
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
		case string:lexemes(Mantissa,",.") of
		    [Num] -> %% No decimal-mark
			{integer_to_list(list_to_integer(Num)),0};
		    [Num,Dec] ->
			NewDec = ZeroDecimal(remove_trailing_zeros(Dec)),
			NewMan = integer_to_list(list_to_integer(Num)) ++ NewDec,
			{integer_to_list(list_to_integer(NewMan)),
			 length(NewDec)}
		end
	end,

    encode_real_as_string(C, NewMantissa, Exponent - LenDecimal).

encode_real_as_string(_C, Mantissa, Exponent)
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
    <<NR3,ManBin/binary,$.,ExpBin/binary>>.

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
    truncate_zeros(Num, 0).
truncate_zeros(0, Sum) ->
    {0,Sum};
truncate_zeros(M, Sum) ->
    case M band 16#f =:= M band 16#e of
	true -> truncate_zeros(M bsr 1, Sum+1);
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

decode_real(Buffer) ->
    Sz = byte_size(Buffer),
    {RealVal,<<>>,Sz} = decode_real2(Buffer, [], Sz, 0),
    RealVal.

decode_real2(Buffer, _C, 0, _RemBytes) ->
    {0,Buffer};
decode_real2(Buffer0, _C, Len, RemBytes1) ->
    <<First, Buffer2/binary>> = Buffer0,
    if
	First =:= 2#01000000 -> {'PLUS-INFINITY', Buffer2};
	First =:= 2#01000001 -> {'MINUS-INFINITY', Buffer2};
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

encode_pos_integer(0, [B|_Acc]=L) when B < 128 ->
    L;
encode_pos_integer(N, Acc) ->
    encode_pos_integer(N bsr 8, [N band 16#ff| Acc]).

encode_neg_integer(-1, [B1|_T]=L) when B1 > 127 ->
    L;
encode_neg_integer(N, Acc) ->
    encode_neg_integer(N bsr 8, [N band 16#ff|Acc]).


%% Val must be >= 0
real_mininum_octets(Val) ->
    real_mininum_octets(Val, []).

real_mininum_octets(0, Acc) ->
    Acc;
real_mininum_octets(Val, Acc) ->
    real_mininum_octets(Val bsr 8, [Val band 16#FF | Acc]).

%% decoding postitive integer values.
decode_integer2(Len, <<0:1,_:7,_Bs/binary>> = Bin, RemovedBytes) ->
    <<Int:Len/unit:8,Buffer2/binary>> = Bin,
    {Int,Buffer2,RemovedBytes};
%% decoding negative integer values.
decode_integer2(Len, <<1:1,B2:7,Bs/binary>>, RemovedBytes)  ->
    <<N:Len/unit:8,Buffer2/binary>> = <<B2,Bs/binary>>,
    Int = N - (1 bsl (8 * Len - 1)),
    {Int,Buffer2,RemovedBytes}.
