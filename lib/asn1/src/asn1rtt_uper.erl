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
%%
-module(asn1rtt_uper).

-export([skipextensions/3]).
-export([complete/1, complete_NFP/1]).

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

%% un-constrained
decode_length(<<0:1,Oct:7,Rest/bitstring>>)  ->
    {Oct,Rest};
decode_length(<<2:2,Val:14,Rest/bitstring>>)  ->
    {Val,Rest};
decode_length(<<3:2,_:14,_Rest/bitstring>>)  ->
    exit({error,{asn1,{decode_length,{nyi,above_16k}}}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% complete(InList) -> ByteList
%% Takes a coded list with bits and bytes and converts it to a list of bytes
%% Should be applied as the last step at encode of a complete ASN.1 type
%%
complete(InList) when is_list(InList) ->
    case list_to_bitstring(InList) of
	<<>> ->
	    <<0>>;
	Res ->
	    Sz = bit_size(Res),
	    case Sz band 7 of
		0 -> Res;
		Bits -> <<Res:Sz/bitstring,0:(8-Bits)>>
	end
    end;
complete(Bin) when is_binary(Bin) ->
    case Bin of
	<<>> -> <<0>>;
	_ -> Bin
    end;
complete(InList) when is_bitstring(InList) ->
    Sz = bit_size(InList),
    PadLen = 8 - (Sz band 7),
    <<InList:Sz/bitstring,0:PadLen>>.

%% Special version of complete that does not align the completed message.
complete_NFP(InList) when is_list(InList) ->
    list_to_bitstring(InList);
complete_NFP(InList) when is_bitstring(InList) ->
    InList.
