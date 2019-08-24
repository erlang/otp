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
-module(asn1rtt_per).

-export([skipextensions/3,complete/1]).

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

align(Bin) when is_binary(Bin) ->
    Bin;
align(BitStr) when is_bitstring(BitStr) ->
    AlignBits = bit_size(BitStr) rem 8,
    <<_:AlignBits,Rest/binary>> = BitStr,
    Rest.

decode_length(Buffer)  -> % un-constrained
    case align(Buffer) of
	<<0:1,Oct:7,Rest/binary>> ->
	    {Oct,Rest};
	<<2:2,Val:14,Rest/binary>> ->
	    {Val,Rest};
	<<3:2,_Val:14,_Rest/binary>> ->
	    %% this case should be fixed
	    exit({error,{asn1,{decode_length,{nyi,above_16k}}}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% complete(InList) -> ByteList
%% Takes a coded list with bits and bytes and converts it to a list of bytes
%% Should be applied as the last step at encode of a complete ASN.1 type
%%

complete(L0) ->
    L = complete(L0, []),
    case list_to_bitstring(L) of
	<<>> -> <<0>>;
	Bin -> Bin
    end.

complete([], []) ->
    [];
complete([], [H|More]) ->
    complete(H, More);
complete([align|T], More) ->
    complete(T, More);
complete([[]|T], More) ->
    complete(T, More);
complete([[_|_]=H], More) ->
    complete(H, More);
complete([[_|_]=H|T], More) ->
    complete(H, [T|More]);
complete([H|T], More) when is_integer(H); is_binary(H) ->
    [H|complete(T, More)];
complete([H|T], More) ->
    [H|complete(T, bit_size(H), More)];
complete(Bin, More) when is_binary(Bin) ->
    [Bin|complete([], More)];
complete(Bin, More) ->
    [Bin|complete([], bit_size(Bin), More)].

complete([], Bits, []) ->
    case Bits band 7 of
	0 -> [];
	N -> [<<0:(8-N)>>]
    end;
complete([], Bits, [H|More]) ->
    complete(H, Bits, More);
complete([align|T], Bits, More) ->
    case Bits band 7 of
	0 -> complete(T, More);
	1 -> [<<0:7>>|complete(T, More)];
	2 -> [<<0:6>>|complete(T, More)];
	3 -> [<<0:5>>|complete(T, More)];
	4 -> [<<0:4>>|complete(T, More)];
	5 -> [<<0:3>>|complete(T, More)];
	6 -> [<<0:2>>|complete(T, More)];
	7 -> [<<0:1>>|complete(T, More)]
    end;
complete([[]|T], Bits, More) ->
    complete(T, Bits, More);
complete([[_|_]=H], Bits, More) ->
    complete(H, Bits, More);
complete([[_|_]=H|T], Bits, More) ->
    complete(H, Bits, [T|More]);
complete([H|T], Bits, More) when is_integer(H);
				 is_binary(H) ->
    [H|complete(T, Bits, More)];
complete([H|T], Bits, More) ->
    [H|complete(T, Bits+bit_size(H), More)];
complete(Bin, Bits, More) when is_binary(Bin) ->
    [Bin|complete([], Bits, More)];
complete(Bin, Bits, More) ->
    [Bin|complete([], Bits+bit_size(Bin), More)].
