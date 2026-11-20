%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(dets_wire).
-moduledoc false.

-export([encode_uint/1, decode_uint/1, encode/1, decode/2, encoded_size/1]).

-include("dets.hrl").

-define(ESCAPE, (1 bsl 63)).
-define(HIGH, 16#7fffffff00000000).

encode_uint(Value) when is_integer(Value), Value >= 0, Value < ?ESCAPE ->
    <<(encode_word(Value)):64>>.

decode_uint(<<Word:64>>) ->
    decode_word(Word).

encoded_size(Bytes) when is_integer(Bytes), Bytes >= 0 ->
    ((Bytes*8 + 62) div 63)*8.

encode(Binary) when is_binary(Binary) ->
    Padding = (63 - bit_size(Binary) rem 63) rem 63,
    Padded = <<Binary/binary, 0:Padding>>,
    << <<(encode_word(Value)):64>> || <<Value:63>> <= Padded >>.

decode(Binary, Bytes) when is_binary(Binary), is_integer(Bytes), Bytes >= 0 ->
    case byte_size(Binary) =:= encoded_size(Bytes) of
        true ->
            Bits = << <<(decode_word(Word)):63>> || <<Word:64>> <= Binary >>,
            Padding = bit_size(Bits) - Bytes*8,
            <<Result:Bytes/binary, Tail:Padding>> = Bits,
            case Tail of
                0 -> Result;
                _ -> erlang:error(badarg)
            end;
        false ->
            erlang:error(badarg)
    end.

%% Old v9 repair must never see ACTIVE in an upper 32-bit status word.
encode_word(Value) when Value band 16#ffffffff =:= ?ACTIVE ->
    (Value band ?HIGH) bor ?ESCAPE;
encode_word(Value) ->
    Value.

decode_word(Word) when Word band ?ESCAPE =:= 0,
                       Word band 16#ffffffff =/= ?ACTIVE ->
    Word;
decode_word(Word) when Word band ?ESCAPE =/= 0,
                       Word band 16#ffffffff =:= 0 ->
    (Word band ?HIGH) bor ?ACTIVE;
decode_word(_) ->
    erlang:error(badarg).
