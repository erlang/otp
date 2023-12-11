%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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

-module(bs_match_int_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 integer/1,mixed_sizes/1,signed_integer/1,dynamic/1,more_dynamic/1,
         mml/1,match_huge_int/1,bignum/1,unaligned_32_bit/1,unit/1]).

-include_lib("common_test/include/ct.hrl").

-import(lists, [seq/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [integer, mixed_sizes, signed_integer, dynamic, more_dynamic, mml,
     match_huge_int, bignum, unaligned_32_bit, unit].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


integer(Config) when is_list(Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),

    0 = get_int(mkbin([])),
    0 = get_int(mkbin([0])),
    42 = get_int(mkbin([42])),
    255 = get_int(mkbin([255])),
    256 = get_int(mkbin([1,0])),
    257 = get_int(mkbin([1,1])),
    258 = get_int(mkbin([1,2])),
    65534 = get_int(mkbin([255,254])),
    16776455 = get_int(mkbin([255,253,7])),
    4245492555 = get_int(mkbin([253,13,19,75])),
    4294967294 = get_int(mkbin([255,255,255,254])),
    4294967295 = get_int(mkbin([255,255,255,255])),

    16#cafebeef = get_int(<<16#cafebeef:32>>),
    16#cafebeef42 = get_int(<<16#cafebeef42:40>>),
    16#cafebeeffeed = get_int(<<16#cafebeeffeed:48>>),
    16#cafebeeffeed42 = get_int(<<16#cafebeeffeed42:56>>),
    16#1cafebeeffeed42 = get_int(<<16#1cafebeeffeed42:57>>),
    16#2cafebeeffeed42 = get_int(<<16#2cafebeeffeed42:58>>),
    16#7cafebeeffeed42 = get_int(<<16#7cafebeeffeed42:59>>),

    16#beefcafefeed = get_int(<<16#beefcafefeed:60>>),
    16#beefcafefeed = get_int(<<16#beefcafefeed:61>>),
    16#beefcafefeed = get_int(<<16#beefcafefeed:62>>),
    16#beefcafefeed = get_int(<<16#beefcafefeed:63>>),

    16#cafebeeffeed42 = get_int(<<16#cafebeeffeed42:60>>),
    16#cafebeeffeed42 = get_int(<<16#cafebeeffeed42:61>>),
    16#cafebeeffeed42 = get_int(<<16#cafebeeffeed42:62>>),
    16#cafebeeffeed42 = get_int(<<16#cafebeeffeed42:63>>),

    16#acafebeeffeed42 = get_int(<<16#acafebeeffeed42:60>>),
    16#acafebeeffeed42 = get_int(<<16#acafebeeffeed42:61>>),
    16#acafebeeffeed42 = get_int(<<16#acafebeeffeed42:62>>),
    16#acafebeeffeed42 = get_int(<<16#acafebeeffeed42:63>>),

    16#cafebeeffeed = get_int(<<16#cafebeeffeed:64>>),
    16#cafebeeffeedface = get_int(<<16#cafebeeffeedface:64>>),

    get_int_roundtrip(rand:bytes(12), 0),
    get_int_roundtrip(rand:bytes(12), 0),

    Eight = [200,1,19,128,222,42,97,111],
    cmp128(Eight, uint(Eight)),
    fun_clause(catch get_int(mkbin(seq(1, 20)))),
    ok.

get_int_roundtrip(Bin0, Size) when Size =< 8*byte_size(Bin0) ->
    <<Bin:Size/bits,_/bits>> = Bin0,
    <<I:Size>> = Bin,
    I = get_int(Bin),
    get_int_roundtrip(Bin0, Size+1);
get_int_roundtrip(_, _) -> ok.

get_int(Bin0) ->
    %% Note that it has become impossible to create a byte-sized sub
    %% binary (see erts_build_sub_bitstring() in erl_bits.c) of size 64
    %% or less. Therefore, to be able to create an unaligned binary,
    %% we'll need to base it on a binary with more than 64 bytes.
    Size = bit_size(Bin0),
    Filler = rand:bytes(65),
    UnsignedBigBin = id(<<Filler/binary,Bin0/bits>>),
    I = get_unsigned_big(UnsignedBigBin),

    %% io:format("~p ~p\n", [Size,I]),
    if
        Size =< 10*8 ->
            OversizedUnsignedBig = id(<<Filler/binary,0:16,Bin0/bits>>),
            I = get_unsigned_big(OversizedUnsignedBig);
        true ->
            ok
    end,

    test_unaligned(UnsignedBigBin, I, fun get_unsigned_big/1),

    %% Test unsigned little-endian integers.

    UnsignedLittleBin = id(<<Filler/binary,I:Size/little>>),
    I = get_unsigned_little(UnsignedLittleBin),

    test_unaligned(UnsignedLittleBin, I, fun get_unsigned_little/1),

    %% Test signed big-endian integers.

    SignedBigBin1 = id(<<Filler/binary,(-I):(Size+1)/big>>),
    I = -get_signed_big(SignedBigBin1),

    SignedBigBin2 = id(<<Filler/binary,I:(Size+1)/big>>),
    I = get_signed_big(SignedBigBin2),

    %% test_unaligned(SignedBigBin1, -I, fun get_signed_big/1),
    test_unaligned(SignedBigBin2, I, fun get_signed_big/1),

    %% Test signed little-endian integers.

    SignedLittleBin1 = id(<<Filler/binary,(-I):(Size+1)/little>>),
    I = -get_signed_little(SignedLittleBin1),

    SignedLittleBin2 = id(<<Filler/binary,I:(Size+1)/little>>),
    I = get_signed_little(SignedLittleBin2),

    test_unaligned(SignedLittleBin1, -I, fun get_signed_little/1),
    test_unaligned(SignedLittleBin2, I, fun get_signed_little/1),

    I.

test_unaligned(Bin, I, Matcher) ->
    <<RandBytes1:8/binary,RandBytes2:8/binary>> = rand:bytes(16),
    Size = bit_size(Bin),
    _ = [begin
             <<_:(Offset+32),UnalignedBin:Size/bits,_/bits>> =
                 id(<<RandBytes1:(Offset+32)/bits,
                      Bin:Size/bits,
                      RandBytes2/binary>>),
             I = Matcher(UnalignedBin)
         end || Offset <- [1,2,3,4,5,6,7]],
    ok.



get_unsigned_big(Bin) ->
    Res = get_unsigned_big_plain(Bin),
    [A,B,C,D,E] = id([1,2,3,4,5]),
    {Res,_} = get_unsigned_big_memory_ctx(A, B, C, D, E, Res, Bin),
    Res.

get_unsigned_big_memory_ctx(A, B, C, D, E, Res0, Bin) ->
    %% The match context will be in {x,6}, which is not
    %% a X register backed by a CPU register on any platform.
    Res = case Bin of
              <<_:65/unit:8,I:7>> -> I;
              <<_:65/unit:8,I:8>> -> I;
              <<_:65/unit:8,I:36>> -> I;
              <<_:65/unit:8,I:59>> -> I;
              <<_:65/unit:8,I:60>> -> I;
              <<_:65/unit:8,I:61>> -> I;
              <<_:65/unit:8,I:62>> -> I;
              <<_:65/unit:8,I:63>> -> I;
              <<_:65/unit:8,I:64>> -> I;
              <<_:65/unit:8,I:65>> -> I;
              <<_:65/unit:8,I:70>> -> I;
              <<_:65/unit:8,I:80>> -> I;
              <<_:65/unit:8,I:95>> -> I;
              _ -> Res0
          end,
    {Res,{A,B,C,D,E}}.

get_unsigned_big_plain(<<_:65/unit:8,I:0>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:1>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:2>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:3>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:4>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:5>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:6>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:7>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:8>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:9>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:10>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:11>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:12>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:13>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:14>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:15>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:16>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:17>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:18>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:19>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:20>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:21>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:22>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:23>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:24>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:25>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:26>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:27>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:28>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:29>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:30>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:31>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:32>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:33>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:34>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:35>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:36>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:37>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:38>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:39>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:40>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:41>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:42>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:43>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:44>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:45>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:46>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:47>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:48>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:49>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:50>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:51>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:52>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:53>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:54>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:55>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:56>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:57>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:58>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:59>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:60>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:61>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:62>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:63>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:64>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:65>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:66>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:67>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:68>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:69>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:70>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:71>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:72>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:73>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:74>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:75>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:76>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:77>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:78>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:79>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:80>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:81>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:82>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:83>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:84>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:85>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:86>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:87>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:88>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:89>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:90>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:91>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:92>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:93>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:94>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:95>>) -> I;
get_unsigned_big_plain(<<_:65/unit:8,I:96>>) -> I.

get_unsigned_little(<<_:65/unit:8,I:0/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:1/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:2/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:3/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:4/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:5/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:6/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:7/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:8/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:9/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:10/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:11/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:12/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:13/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:14/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:15/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:16/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:17/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:18/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:19/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:20/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:21/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:22/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:23/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:24/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:25/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:26/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:27/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:28/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:29/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:30/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:31/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:32/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:33/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:34/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:35/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:36/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:37/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:38/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:39/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:40/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:41/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:42/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:43/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:44/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:45/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:46/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:47/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:48/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:49/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:50/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:51/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:52/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:53/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:54/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:55/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:56/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:57/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:58/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:59/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:60/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:61/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:62/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:63/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:64/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:65/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:66/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:67/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:68/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:69/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:70/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:71/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:72/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:73/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:74/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:75/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:76/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:77/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:78/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:79/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:80/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:81/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:82/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:83/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:84/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:85/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:86/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:87/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:88/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:89/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:90/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:91/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:92/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:93/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:94/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:95/little>>) -> I;
get_unsigned_little(<<_:65/unit:8,I:96/little>>) -> I.

get_signed_big(<<_:65/unit:8,I:0/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:1/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:2/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:3/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:4/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:5/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:6/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:7/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:8/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:9/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:10/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:11/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:12/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:13/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:14/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:15/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:16/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:17/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:18/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:19/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:20/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:21/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:22/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:23/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:24/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:25/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:26/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:27/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:28/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:29/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:30/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:31/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:32/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:33/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:34/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:35/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:36/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:37/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:38/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:39/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:40/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:41/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:42/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:43/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:44/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:45/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:46/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:47/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:48/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:49/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:50/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:51/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:52/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:53/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:54/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:55/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:56/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:57/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:58/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:59/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:60/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:61/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:62/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:63/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:64/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:65/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:66/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:67/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:68/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:69/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:70/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:71/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:72/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:73/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:74/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:75/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:76/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:77/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:78/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:79/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:80/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:81/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:82/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:83/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:84/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:85/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:86/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:87/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:88/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:89/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:90/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:91/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:92/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:93/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:94/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:95/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:96/signed-big>>) -> I;
get_signed_big(<<_:65/unit:8,I:97/signed-big>>) -> I.

get_signed_little(<<_:65/unit:8,I:0/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:1/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:2/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:3/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:4/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:5/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:6/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:7/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:8/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:9/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:10/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:11/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:12/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:13/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:14/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:15/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:16/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:17/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:18/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:19/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:20/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:21/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:22/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:23/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:24/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:25/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:26/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:27/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:28/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:29/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:30/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:31/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:32/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:33/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:34/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:35/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:36/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:37/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:38/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:39/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:40/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:41/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:42/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:43/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:44/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:45/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:46/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:47/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:48/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:49/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:50/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:51/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:52/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:53/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:54/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:55/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:56/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:57/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:58/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:59/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:60/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:61/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:62/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:63/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:64/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:65/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:66/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:67/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:68/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:69/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:70/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:71/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:72/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:73/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:74/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:75/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:76/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:77/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:78/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:79/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:80/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:81/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:82/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:83/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:84/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:85/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:86/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:87/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:88/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:89/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:90/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:91/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:92/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:93/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:94/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:95/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:96/signed-little>>) -> I;
get_signed_little(<<_:65/unit:8,I:97/signed-little>>) -> I.

cmp128(<<I:128>>, I) -> equal;
cmp128(_, _) -> not_equal.

mixed_sizes(_Config) ->
    mixed({345,42},
          fun({A,B}) ->
                  <<A:9,1:1,B:6>>;
             (<<A:9,_:1,B:6>>) ->
                  {A,B}
          end),

    mixed({27033,59991,16#c001cafe,12345,2},
          fun({A,B,C,D,E}) ->
                  <<A:16,B:16,C:32,D:22,E:2>>;
             (<<A:16,B:16,C:32,D:22,E:2>>) ->
                  {A,B,C,D,E}
          end),

    mixed({79,153,17555,50_000,777_000,36#hugebignumber,2222},
          fun({A,B,C,D,E,F,G}) ->
                  <<A:7,B:8,C:15,0:3,D:17,E:23,F:88,G:13>>;
             (<<A:7,B:8,C:15,_:3,D:17,E:23,F:88,G:13>>) ->
                  {A,B,C,D,E,F,G}
          end),

    mixed({16#123456789ABCDEF,13,36#hugenum,979},
          fun({A,B,C,D}) ->
                  <<A:60,B:4,C:50,D:10>>;
             (<<A:60,B:4,C:50,D:10>>) ->
                  {A,B,C,D}
          end),

    mixed({16#123456789ABCDEF,13,36#hugenum,979},
          fun({A,B,C,D}) ->
                  <<A:60/little,B:4/little,C:50/little,D:10/little>>;
             (<<A:60/little,B:4/little,C:50/little,D:10/little>>) ->
                  {A,B,C,D}
          end),

    mixed({15692284513449131826, 17798, 33798},
          fun({A,B,C}) ->
                  <<A:64,B:15/little,C:16/little>>;
             (<<A:64,B:15/little,C:16/little>>) ->
                  {A,B,C}
          end),

    mixed({15692344284519131826, 1779863, 13556268},
          fun({A,B,C}) ->
                  <<A:64,B:23/little,C:24/little>>;
             (<<A:64,B:23/little,C:24/little>>) ->
                  {A,B,C}
          end),

    mixed({15519169234428431825, 194086885, 2813274043},
          fun({A,B,C}) ->
                  <<A:64,B:29/little,C:32/little>>;
             (<<A:64,B:29/little,C:32/little>>) ->
                  {A,B,C}
          end),

    mixed({5,9,38759385,93},
          fun({A,B,C,D}) ->
                  <<1:3,A:4,B:5,C:47,D:7>>;
             (<<1:3,A:4,B:5,C:47,D:7>>) ->
                  {A,B,C,D}
          end),

    mixed({2022,8,22},
          fun({A,B,C}) ->
                  <<A:16/little,B,C>>;
             (<<A:16/little,B,C>>) ->
                  {A,B,C}
          end),

    mixed({2022,8,22},
          fun({A,B,C}) ->
                  <<A:16/little,B,C>>;
             (<<A:16/little,B,C>>) ->
                  _ = id(0),
                  {A,B,C}
          end),
    ok.

mixed(Data, F) ->
    Bin = F(Data),
    Data = F(Bin),
    true = is_bitstring(Bin).

signed_integer(Config) when is_list(Config) ->
    {no_match,_} = sint(mkbin([])),
    {no_match,_} = sint(mkbin([1,2,3])),
    127 = sint(mkbin([127])),
    -1 = sint(mkbin([255])),
    -128 = sint(mkbin([128])),
    42 = sint(mkbin([42,255])),
    127 = sint(mkbin([127,255])).

sint(Bin) ->
    case Bin of
	<<I:8/signed>> -> I;
	<<I:8/signed,_:3,_:5>> -> I;
	Other -> {no_match,Other}
    end.

uint(L) -> uint(L, 0).
uint([H|T], Acc) -> uint(T, Acc bsl 8 bor H);
uint([], Acc) -> Acc.

dynamic(Config) when is_list(Config) ->
    dynamic(mkbin([255]), 8),
    dynamic(mkbin([255,255]), 16),
    dynamic(mkbin([255,255,255]), 24),
    dynamic(mkbin([255,255,255,255]), 32),
    ok.

dynamic(Bin, S1) when S1 >= 0 ->
    S2 = size(Bin) * 8 - S1,
    dynamic(Bin, S1, S2, (1 bsl S1) - 1, (1 bsl S2) - 1),
    dynamic(Bin, S1-1);
dynamic(_, _) -> ok.

dynamic(Bin, S1, S2, A, B) ->
%    io:format("~p ~p ~p ~p\n", [S1,S2,A,B]),
    case Bin of
	<<A:S1,B:S2>> ->
	    io:format("~p ~p ~p ~p\n", [S1,S2,A,B]),
	    ok;
	_Other -> erlang:error(badmatch, [Bin,S1,S2,A,B])
    end.

%% Extract integers at different alignments and of different sizes.
more_dynamic(Config) when is_list(Config) ->

    %% Unsigned big-endian numbers.
    Unsigned  = fun(Bin, List, SkipBef, N) ->
			SkipAft = 8*size(Bin) - N - SkipBef,
			<<_:SkipBef,Int:N,_:SkipAft>> = Bin,
			Int = make_int(List, N, 0)
		end,
    more_dynamic1(Unsigned, erlang:md5(mkbin([42]))),

    %% Signed big-endian numbers.
    Signed  = fun(Bin, List, SkipBef, N) ->
		      SkipAft = 8*size(Bin) - N - SkipBef,
		      <<_:SkipBef,Int:N/signed,_:SkipAft>> = Bin,
		      case make_signed_int(List, N) of
			  Int -> ok;
			  Other ->
			      io:format("Bin = ~p,", [Bin]),
			      io:format("SkipBef = ~p, N = ~p", [SkipBef,N]),
			      io:format("Expected ~p, got ~p", [Int,Other]),
			      ct:fail(signed_big_endian_fail)
		      end
	      end,
    more_dynamic1(Signed, erlang:md5(mkbin([43]))),

    %% Unsigned little-endian numbers.
    UnsLittle  = fun(Bin, List, SkipBef, N) ->
			 SkipAft = 8*size(Bin) - N - SkipBef,
			 <<_:SkipBef,Int:N/little,_:SkipAft>> = Bin,
			 Int = make_int(big_to_little(List, N), N, 0)
		 end,
    more_dynamic1(UnsLittle, erlang:md5(mkbin([44]))),

    %% Signed little-endian numbers.
    SignLittle  = fun(Bin, List, SkipBef, N) ->
			  SkipAft = 8*size(Bin) - N - SkipBef,
			  <<_:SkipBef,Int:N/signed-little,_:SkipAft>> = Bin,
			  Little = big_to_little(List, N),
			  Int = make_signed_int(Little, N)
		  end,
    more_dynamic1(SignLittle, erlang:md5(mkbin([45]))),

    ok.

more_dynamic1(Action, Bin) ->
    BitList = bits_to_list(binary_to_list(Bin), 16#80),
    more_dynamic2(Action, Bin, BitList, 0).

more_dynamic2(Action, Bin, [_|T]=List, Bef) ->
    more_dynamic3(Action, Bin, List, Bef, size(Bin)*8),
    more_dynamic2(Action, Bin, T, Bef+1);
more_dynamic2(_, _, [], _) -> ok.

more_dynamic3(Action, Bin, List, Bef, Aft) when Bef =< Aft ->
%%    io:format("~p, ~p", [Bef,Aft-Bef]),
    Action(Bin, List, Bef, Aft-Bef),
    more_dynamic3(Action, Bin, List, Bef, Aft-1);
more_dynamic3(_, _, _, _, _) -> ok.

big_to_little(List, N) -> big_to_little(List, N, []).

big_to_little([B0,B1,B2,B3,B4,B5,B6,B7|T], N, Acc) when N >= 8 ->
    big_to_little(T, N-8, [B0,B1,B2,B3,B4,B5,B6,B7|Acc]);
big_to_little(List, N, Acc) -> lists:sublist(List, 1, N) ++ Acc.

make_signed_int(_List, 0) -> 0;
make_signed_int([0|_]=List, N) -> make_int(List, N, 0);
make_signed_int([1|_]=List0, N) ->
    List1 = reversed_sublist(List0, N, []),
    List2 = two_complement_and_reverse(List1, 1, []),
    -make_int(List2, length(List2), 0).

reversed_sublist(_List, 0, Acc) -> Acc;
reversed_sublist([H|T], N, Acc) -> reversed_sublist(T, N-1, [H|Acc]).
    
two_complement_and_reverse([H|T], Carry, Acc) ->
    Sum = 1-H+Carry,
    two_complement_and_reverse(T, Sum div 2, [Sum rem 2|Acc]);
two_complement_and_reverse([], Carry, Acc) -> [Carry|Acc].

make_int(_List, 0, Acc) -> Acc;
make_int([H|T], N, Acc) -> make_int(T, N-1, Acc bsl 1 bor H).
    
bits_to_list([_|T], 0) -> bits_to_list(T, 16#80);
bits_to_list([H|_]=List, Mask) ->
    [case H band Mask of
	 0 -> 0;
	 _ -> 1
     end|bits_to_list(List, Mask bsr 1)];
bits_to_list([], _) -> [].

fun_clause({'EXIT',{function_clause,_}}) -> ok.
mkbin(L) when is_list(L) -> list_to_binary(L).
    

mml(Config) when is_list(Config) ->
    single_byte_binary = mml_choose(<<42>>),
    multi_byte_binary = mml_choose(<<42,43>>).

mml_choose(<<_A:8>>) -> single_byte_binary;
mml_choose(<<_A:8,_T/binary>>) -> multi_byte_binary.

match_huge_int(Config) when is_list(Config) ->
    case ?MODULE of
        bs_match_int_no_opt_SUITE ->
            %% This test case is written with the assumption that
            %% bs_skip2 instructions are used when the value of the
            %% extracted segment will not be used. In OTP 21 and earlier, that
            %% assumption was always true, because the bs_skip optimization
            %% was included in v3_codegen and could not be disabled.
            %% In OTP 22, the bs_skip optimization is done by beam_ssa_opt
            %% and is disabled.
            %%
            %% On memory-constrained computers, using bs_get_integer2
            %% instructions may cause the runtime system to terminate
            %% because of insufficient memory.
            {skip, "unoptimized code would use too much memory"};
        bs_match_int_SUITE ->
            do_match_huge_int();
        bs_match_int_r25_SUITE ->
            do_match_huge_int();
        bs_match_int_stripped_types_SUITE ->
            do_match_huge_int()
    end.

do_match_huge_int() ->
    Sz = 1 bsl 27,
    Bin = <<0:Sz,13:8>>,
    skip_huge_int_1(Sz, Bin),
    0 = match_huge_int_1(Sz, Bin),

    %% Test overflowing the size of an integer field.
    nomatch = overflow_huge_int_skip_32(Bin),
    case erlang:system_info(wordsize) of
        4 ->
            nomatch = overflow_huge_int_32(Bin);
        8 ->
            %% An attempt will be made to allocate heap space for
            %% the bignum (which will probably fail); only if the
            %% allocation succeeds will the matching fail because
            %% the binary is too small.
            ok
    end,
    nomatch = overflow_huge_int_skip_64(Bin),
    nomatch = overflow_huge_int_64(Bin),

    %% Test overflowing the size of an integer field using
    %% variables as sizes.
    Sizes = case erlang:system_info(wordsize) of
                4 -> lists:seq(25, 32);
                8 -> []
            end ++ lists:seq(50, 64),
    ok = overflow_huge_int_unit128(Bin, Sizes),

    %% GH-6701: [vm] crash with -emu_flavor emu:
    %% "no next heap size found: 18446744072702918678, offset 0"
    {'EXIT',{function_clause,_}} =
        (catch fun(<<X:2147483647/unit:98>>) -> X end(<<>>)),
    ok.

overflow_huge_int_unit128(Bin, [Sz0|Sizes]) ->
    Sz = id(1 bsl Sz0),
    case Bin of
	<<_:Sz/unit:128,0,_/binary>> ->
	    {error,Sz};
	_ ->
	    case Bin of
		<<Var:Sz/unit:128,0,_/binary>> ->
		    {error,Sz,Var};
		_ ->
		    overflow_huge_int_unit128(Bin, Sizes)
	    end
    end;
overflow_huge_int_unit128(_, []) -> ok.

match_huge_int_1(I, Bin) ->
    <<Int:I,13>> = Bin,
    Int.

skip_huge_int_1(I, Bin) ->
    <<_:I,13>> = Bin.

overflow_huge_int_skip_32(<<_:4294967296,0,_/binary>>) -> 1; % 1 bsl 32
overflow_huge_int_skip_32(<<_:33554432/unit:128,0,_/binary>>) -> 2; % 1 bsl 25
overflow_huge_int_skip_32(<<_:67108864/unit:64,0,_/binary>>) -> 3; % 1 bsl 26
overflow_huge_int_skip_32(<<_:134217728/unit:32,0,_/binary>>) -> 4; % 1 bsl 27
overflow_huge_int_skip_32(<<_:268435456/unit:16,0,_/binary>>) -> 5; % 1 bsl 28
overflow_huge_int_skip_32(<<_:536870912/unit:8,0,_/binary>>) -> 6; % 1 bsl 29
overflow_huge_int_skip_32(<<_:1073741824/unit:8,0,_/binary>>) -> 7; % 1 bsl 30
overflow_huge_int_skip_32(<<_:2147483648/unit:8,0,_/binary>>) -> 8; % 1 bsl 31
overflow_huge_int_skip_32(_) -> nomatch.

overflow_huge_int_32(<<Int:4294967296,_/binary>>) -> {1,Int}; % 1 bsl 32
overflow_huge_int_32(<<Int:33554432/unit:128,0,_/binary>>) -> {2,Int}; % 1 bsl 25
overflow_huge_int_32(<<Int:67108864/unit:128,0,_/binary>>) -> {3,Int}; % 1 bsl 26
overflow_huge_int_32(<<Int:134217728/unit:128,0,_/binary>>) -> {4,Int}; % 1 bsl 27
overflow_huge_int_32(<<Int:268435456/unit:128,0,_/binary>>) -> {5,Int}; % 1 bsl 28
overflow_huge_int_32(<<Int:536870912/unit:128,0,_/binary>>) -> {6,Int}; % 1 bsl 29
overflow_huge_int_32(<<Int:1073741824/unit:128,0,_/binary>>) -> {7,Int}; % 1 bsl 30
overflow_huge_int_32(<<Int:2147483648/unit:128,0,_/binary>>) -> {8,Int}; % 1 bsl 31
overflow_huge_int_32(_) -> nomatch.

overflow_huge_int_skip_64(<<_:18446744073709551616,_/binary>>) -> 1; % 1 bsl 64
overflow_huge_int_skip_64(<<_:144115188075855872/unit:128,0,_/binary>>) -> 2; % 1 bsl 57
overflow_huge_int_skip_64(<<_:288230376151711744/unit:64,0,_/binary>>) -> 3; % 1 bsl 58
overflow_huge_int_skip_64(<<_:576460752303423488/unit:32,0,_/binary>>) -> 4; % 1 bsl 59
overflow_huge_int_skip_64(<<_:1152921504606846976/unit:16,0,_/binary>>) -> 5; % 1 bsl 60
overflow_huge_int_skip_64(<<_:2305843009213693952/unit:8,0,_/binary>>) -> 6; % 1 bsl 61
overflow_huge_int_skip_64(<<_:4611686018427387904/unit:8,0,_/binary>>) -> 7; % 1 bsl 62
overflow_huge_int_skip_64(<<_:9223372036854775808/unit:8,0,_/binary>>) -> 8; % 1 bsl 63
overflow_huge_int_skip_64(_) -> nomatch.

overflow_huge_int_64(<<Int:18446744073709551616,_/binary>>) -> {1,Int}; % 1 bsl 64
overflow_huge_int_64(<<Int:144115188075855872/unit:128,0,_/binary>>) -> {2,Int}; % 1 bsl 57
overflow_huge_int_64(<<Int:288230376151711744/unit:128,0,_/binary>>) -> {3,Int}; % 1 bsl 58
overflow_huge_int_64(<<Int:576460752303423488/unit:128,0,_/binary>>) -> {4,Int}; % 1 bsl 59
overflow_huge_int_64(<<Int:1152921504606846976/unit:128,0,_/binary>>) -> {5,Int}; % 1 bsl 60
overflow_huge_int_64(<<Int:2305843009213693952/unit:128,0,_/binary>>) -> {6,Int}; % 1 bsl 61
overflow_huge_int_64(<<Int:4611686018427387904/unit:128,0,_/binary>>) -> {7,Int}; % 1 bsl 62
overflow_huge_int_64(<<Int:9223372036854775808/unit:128,0,_/binary>>) -> {8,Int}; % 1 bsl 63
overflow_huge_int_64(_) -> nomatch.

bignum(Config) when is_list(Config) ->
    Bin = id(<<42,0:1024/unit:8,43>>),
    <<42:1025/little-integer-unit:8,_:8>> = Bin,
    <<_:8,43:1025/integer-unit:8>> = Bin,

    BignumBin = id(<<0:512/unit:8,258254417031933722623:9/unit:8>>),
    <<258254417031933722623:(512+9)/unit:8>> = BignumBin,
    erlang:garbage_collect(),			%Search for holes in debug-build

    %% ERL-1391
   _ =  [begin
            N = (LHS bsl RHS),
            <<N:64/integer-little-signed>> = id(<<N:64/integer-little-signed>>)
         end || LHS <- [-1, 1], RHS <- lists:seq(1, 62)],

    MPos = (1 bsl 63) - 1,
    <<MPos:64/integer-little-signed>> = id(<<MPos:64/integer-little-signed>>),
    MNeg = -1 bsl 63,
    <<MNeg:64/integer-little-signed>> = id(<<MNeg:64/integer-little-signed>>),

    ok.

unaligned_32_bit(Config) when is_list(Config) ->
    %% There used to be a risk for heap overflow (fixed in R11B-5).
    L = unaligned_32_bit_1(<<-1:(64*1024)>>),
    unaligned_32_bit_verify(L, 1638).

unaligned_32_bit_1(<<1:1,U:32,_:7,T/binary>>) ->
    [U|unaligned_32_bit_1(T)];
unaligned_32_bit_1(_) ->
    [].

unaligned_32_bit_verify([], 0) -> ok;
unaligned_32_bit_verify([4294967295|T], N) when N > 0 ->
    unaligned_32_bit_verify(T, N-1).

unit(_Config) ->
    %% GH-6732. Would fail to match with no_ssa_opt or r25.
    <<V1:5/integer-unit:8>> = id(<<16#cafebeef:5/integer-unit:8>>),
    16#cafebeef = id(V1),
    <<V2:8/integer-unit:5>> = id(<<16#cafebeef:8/integer-unit:5>>),
    16#cafebeef = id(V2),

    ok.

%%%
%%% Common utilities.
%%%

id(I) -> I.
