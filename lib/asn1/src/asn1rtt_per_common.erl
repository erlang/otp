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
%%
-module(asn1rtt_per_common).

-include("asn1_records.hrl").

-export([decode_fragmented/3,
	 decode_compact_bit_string/1,
	 decode_legacy_bit_string/1,
	 decode_named_bit_string/2]).

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
