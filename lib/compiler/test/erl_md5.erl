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
-module(erl_md5).
-export([md5/1]).

-define(MAX32, ((1 bsl 32)-1)).
-define(CLAMP(X), ((X) band ?MAX32)).

-define(SHIFT_1(I), element((I band 3) + 1, {7,12,17,22})).
-define(SHIFT_2(I), element((I band 3) + 1, {5, 9,14,20})).
-define(SHIFT_3(I), element((I band 3) + 1, {4,11,16,23})).
-define(SHIFT_4(I), element((I band 3) + 1, {6,10,15,21})).

-define(CONST_1(I), element((I - 0) + 1,
        {16#d76aa478, 16#e8c7b756, 16#242070db, 16#c1bdceee,
         16#f57c0faf, 16#4787c62a, 16#a8304613, 16#fd469501,
         16#698098d8, 16#8b44f7af, 16#ffff5bb1, 16#895cd7be,
         16#6b901122, 16#fd987193, 16#a679438e, 16#49b40821})).

-define(CONST_2(I), element((I - 16) + 1,
        {16#f61e2562, 16#c040b340, 16#265e5a51, 16#e9b6c7aa,
         16#d62f105d, 16#02441453, 16#d8a1e681, 16#e7d3fbc8,
         16#21e1cde6, 16#c33707d6, 16#f4d50d87, 16#455a14ed,
         16#a9e3e905, 16#fcefa3f8, 16#676f02d9, 16#8d2a4c8a})).

-define(CONST_3(I), element((I - 32) + 1,
         {16#fffa3942, 16#8771f681, 16#6d9d6122, 16#fde5380c,
         16#a4beea44, 16#4bdecfa9, 16#f6bb4b60, 16#bebfbc70,
         16#289b7ec6, 16#eaa127fa, 16#d4ef3085, 16#04881d05,
         16#d9d4d039, 16#e6db99e5, 16#1fa27cf8, 16#c4ac5665})).

-define(CONST_4(I), element((I - 48) + 1,
        {16#f4292244, 16#432aff97, 16#ab9423a7, 16#fc93a039,
         16#655b59c3, 16#8f0ccc92, 16#ffeff47d, 16#85845dd1,
         16#6fa87e4f, 16#fe2ce6e0, 16#a3014314, 16#4e0811a1,
         16#f7537e82, 16#bd3af235, 16#2ad7d2bb, 16#eb86d391})).

md5(Msg0) when is_binary(Msg0) ->
    A0 = 16#67452301,
    B0 = 16#efcdab89,
    C0 = 16#98badcfe,
    D0 = 16#10325476,

    BitSize = bit_size(Msg0),
    <<Main:(BitSize band -512)/bits,Rest/bits>> = Msg0,
    Tail = pad(Rest, BitSize),
    hash1(Main, A0, B0, C0, D0, Tail).

hash1(<<_:512/bits,Rest/binary>>=Msg, A0, B0, C0, D0, Tail) ->
    {A,B,C,D} = hash_part_1(Msg, A0, B0, C0, D0, 0),
    hash1(Rest,
          ?CLAMP(A0+A),
          ?CLAMP(B0+B),
          ?CLAMP(C0+C),
          ?CLAMP(D0+D),
          Tail);
hash1(<<>>, A, B, C, D, <<>>) ->
    <<A:32/little, B:32/little, C:32/little, D:32/little>>;
hash1(<<>>, A, B, C, D, Tail) ->
    hash1(Tail, A, B, C, D, <<>>).

hash_part_1(<<Msg/binary>> = Msg0, A, B, C, D, I) when I < 16 ->
    Const = ?CONST_1(I),
    Shift = ?SHIFT_1(I),
    G = I,
    <<_:G/unit:32, M:32/unsigned-little, _/binary>> = Msg,
    F0 = D bxor (B band (C bxor D)),
    F = ?CLAMP(F0 + A + M + Const),
    FRot = (F bsl Shift) bor (F bsr (32-Shift)),
    BUpd = ?CLAMP(B + FRot),
    %% A <= D,  B <= B + F_Rotated, D <= C, C <= B,
    hash_part_1(Msg0, D, BUpd, B, C, I+1);
hash_part_1(Msg, A, B, C, D, I) ->
    hash_part_2(Msg, A, B, C, D, I).

hash_part_2(<<Msg/binary>> = Msg0, A, B, C, D, I) when I < 32 ->
    Const = ?CONST_2(I),
    Shift = ?SHIFT_2(I),
    G = (5*I + 1) rem 16,
    <<_:G/unit:32, M:32/unsigned-little, _/binary>> = Msg,
    F0 = C bxor (D band (B bxor C)),
    F = ?CLAMP(F0 + A + M + Const),
    FRot = (F bsl Shift) bor (F bsr (32-Shift)),
    BUpd = ?CLAMP(B + FRot),
    %% A <= D,  B <= B + F_Rotated, D <= C, C <= B,
    hash_part_2(Msg0, D, BUpd, B, C, I+1);
hash_part_2(Msg, A, B, C, D, I) ->
    hash_part_3(Msg, A, B, C, D, I).

hash_part_3(<<Msg/binary>> = Msg0, A, B, C, D, I) when I < 48 ->
    Const = ?CONST_3(I),
    Shift = ?SHIFT_3(I),
    G = (3*I + 5) rem 16,
    <<_:G/unit:32, M:32/unsigned-little, _/binary>> = Msg,
    F0 = B bxor C bxor D,
    F = ?CLAMP(F0 + A + M + Const),
    FRot = (F bsl Shift) bor (F bsr (32-Shift)),
    BUpd = ?CLAMP(B + FRot),
    %% A <= D,  B <= B + F_Rotated, D <= C, C <= B,
    hash_part_3(Msg0, D, BUpd, B, C, I+1);
hash_part_3(Msg, A, B, C, D, I) ->
    hash_part_4(Msg, A, B, C, D, I).

hash_part_4(<<Msg/binary>> = Msg0, A, B, C, D, I) when I < 64 ->
    Const = ?CONST_4(I),
    Shift = ?SHIFT_4(I),
    G = (7*I) rem 16,
    <<_:G/unit:32, M:32/unsigned-little, _/binary>> = Msg,
    F0 = C bxor (B bor (bnot D)),
    F = ?CLAMP(F0 + A + M + Const),
    FRot = (F bsl Shift) bor (F bsr (32-Shift)),
    BUpd = ?CLAMP(B + FRot),
    %% A <= D,  B <= B + F_Rotated, D <= C, C <= B,
    hash_part_4(Msg0, D, BUpd, B, C, I+1);
hash_part_4(_, A, B, C, D, _) ->
    {A,B,C,D}.

pad(Msg, Size) when is_binary(Msg) ->
    Filled = ((Size + 1) rem 512),
    Zeros = (512 - 64 - Filled) band (512 - 1),
    <<Msg/bits, 1:1, 0:Zeros, Size:64/little>>.
