%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(bs_match_bin_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 byte_split_binary/1,bit_split_binary/1,match_huge_bin/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [byte_split_binary, bit_split_binary, match_huge_bin].

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


%% Tries to split a binary at all byte-aligned positions.
byte_split_binary(Config) when is_list(Config) ->
    L = lists:seq(0, 57),
    B = mkbin(L),
    byte_split(L, B, size(B)),
    Unaligned = make_unaligned_sub_binary(B),
    byte_split(L, Unaligned, size(Unaligned)).

byte_split(L, B, Pos) when Pos >= 0 ->
    Sz1 = Pos,
    Sz2 = size(B) - Pos,
    <<B1:Sz1/binary,B2:Sz2/binary>> = B,
    B1 = list_to_binary(lists:sublist(L, 1, Pos)),
    B2 = list_to_binary(lists:nthtail(Pos, L)),
    byte_split(L, B, Pos-1);
byte_split(_, _, _) -> ok.

%% Tries to split a binary at all positions.
bit_split_binary(Config) when is_list(Config) ->
    Fun = fun(Bin, List, SkipBef, N) ->
		  SkipAft = 8*size(Bin) - N - SkipBef,
		  %%io:format("~p, ~p, ~p", [SkipBef,N,SkipAft]),
		  <<_:SkipBef,OutBin:N/binary-unit:1,_:SkipAft>> = Bin,
		  OutBin = make_bin_from_list(List, N)
	  end,
    bit_split_binary1(Fun, erlang:md5(<<1,2,3>>)),
    bit_split_binary1(Fun,
			    make_unaligned_sub_binary(erlang:md5(<<1,2,3>>))),
    ok.

bit_split_binary1(Action, Bin) ->
    BitList = bits_to_list(binary_to_list(Bin), 16#80),
    bit_split_binary2(Action, Bin, BitList, 0).

bit_split_binary2(Action, Bin, [_|T]=List, Bef) ->
    bit_split_binary3(Action, Bin, List, Bef, size(Bin)*8),
    bit_split_binary2(Action, Bin, T, Bef+1);
bit_split_binary2(_, _, [], _) -> ok.

bit_split_binary3(Action, Bin, List, Bef, Aft) when Bef =< Aft ->
    Action(Bin, List, Bef, (Aft-Bef) div 8 * 8),
    bit_split_binary3(Action, Bin, List, Bef, Aft-8);
bit_split_binary3(_, _, _, _, _) -> ok.

make_bin_from_list(_, 0) -> mkbin([]);
make_bin_from_list(List, N) ->
    list_to_binary([make_int(List, 8, 0),
		    make_bin_from_list(lists:nthtail(8, List), N-8)]).


make_int(_, 0, Acc) -> Acc;
make_int([H|T], N, Acc) -> make_int(T, N-1, Acc bsl 1 bor H).
    
bits_to_list([_|T], 0) -> bits_to_list(T, 16#80);
bits_to_list([H|_]=List, Mask) ->
    [case H band Mask of
	 0 -> 0;
	 _ -> 1
     end|bits_to_list(List, Mask bsr 1)];
bits_to_list([], _) -> [].

mkbin(L) when is_list(L) -> list_to_binary(L).

make_unaligned_sub_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

id(I) -> I.

match_huge_bin(Config) when is_list(Config) ->
    Bin = <<0:(1 bsl 27),13:8>>,
    skip_huge_bin_1(1 bsl 27, Bin),
    16777216 = match_huge_bin_1(1 bsl 27, Bin),
    
    %% Test overflowing the size of a binary field.
    nomatch = overflow_huge_bin_skip_32(Bin),
    nomatch = overflow_huge_bin_32(Bin),
    nomatch = overflow_huge_bin_skip_64(Bin),
    nomatch = overflow_huge_bin_64(Bin),

    %% Size in variable
    ok = overflow_huge_bin(Bin, lists:seq(25, 32)++lists:seq(50, 64)),
    ok = overflow_huge_bin_unit128(Bin, lists:seq(25, 32)++lists:seq(50, 64)),

    ok.

overflow_huge_bin(Bin, [Sz0|Sizes]) ->
    Sz = id(1 bsl Sz0),
    case Bin of
	<<_:Sz/binary-unit:8,0,_/binary>> ->
	    {error,Sz};
	_ ->
	    case Bin of
		<<NewBin:Sz/binary-unit:8,0,_/binary>> ->
		    {error,Sz,size(NewBin)};
		_ ->
		    overflow_huge_bin(Bin, Sizes)
	    end
    end;
overflow_huge_bin(_, []) -> ok.

overflow_huge_bin_unit128(Bin, [Sz0|Sizes]) ->
    Sz = id(1 bsl Sz0),
    case Bin of
	<<_:Sz/binary-unit:128,0,_/binary>> ->
	    {error,Sz};
	_ ->
	    case Bin of
		<<NewBin:Sz/binary-unit:128,0,_/binary>> ->
		    {error,Sz,size(NewBin)};
		_ ->
		    overflow_huge_bin_unit128(Bin, Sizes)
	    end
    end;
overflow_huge_bin_unit128(_, []) -> ok.

skip_huge_bin_1(I, Bin) ->
    <<_:I/binary-unit:1,13>> = Bin,
    ok.

match_huge_bin_1(I, Bin) ->
    case Bin of
	<<Val:I/binary-unit:1,13>> -> size(Val);
	_ -> nomatch
    end.

overflow_huge_bin_skip_32(<<_:4294967296/binary,0,_/binary>>) -> 1; % 1 bsl 32
overflow_huge_bin_skip_32(<<_:33554432/binary-unit:128,0,_/binary>>) -> 2; % 1 bsl 25
overflow_huge_bin_skip_32(<<_:67108864/binary-unit:64,0,_/binary>>) -> 3; % 1 bsl 26
overflow_huge_bin_skip_32(<<_:134217728/binary-unit:32,0,_/binary>>) -> 4; % 1 bsl 27
overflow_huge_bin_skip_32(<<_:268435456/binary-unit:16,0,_/binary>>) -> 5; % 1 bsl 28
overflow_huge_bin_skip_32(<<_:536870912/binary-unit:8,0,_/binary>>) -> 6; % 1 bsl 29
overflow_huge_bin_skip_32(<<_:1073741824/binary-unit:8,0,_/binary>>) -> 7; % 1 bsl 30
overflow_huge_bin_skip_32(<<_:2147483648/binary-unit:8,0,_/binary>>) -> 8; % 1 bsl 31
overflow_huge_bin_skip_32(_) -> nomatch.

overflow_huge_bin_32(<<Bin:4294967296/binary,_/binary>>) -> {1,Bin}; % 1 bsl 32
overflow_huge_bin_32(<<Bin:33554432/binary-unit:128,0,_/binary>>) -> {2,Bin}; % 1 bsl 25
overflow_huge_bin_32(<<Bin:67108864/binary-unit:128,0,_/binary>>) -> {3,Bin}; % 1 bsl 26
overflow_huge_bin_32(<<Bin:134217728/binary-unit:128,0,_/binary>>) -> {4,Bin}; % 1 bsl 27
overflow_huge_bin_32(<<Bin:268435456/binary-unit:128,0,_/binary>>) -> {5,Bin}; % 1 bsl 28
overflow_huge_bin_32(<<Bin:536870912/binary-unit:128,0,_/binary>>) -> {6,Bin}; % 1 bsl 29
overflow_huge_bin_32(<<Bin:1073741824/binary-unit:128,0,_/binary>>) -> {7,Bin}; % 1 bsl 30
overflow_huge_bin_32(<<Bin:2147483648/binary-unit:128,0,_/binary>>) -> {8,Bin}; % 1 bsl 31
overflow_huge_bin_32(_) -> nomatch.

overflow_huge_bin_skip_64(<<_:18446744073709551616/binary,0,_/binary>>) -> 1; % 1 bsl 64
overflow_huge_bin_skip_64(<<_:144115188075855872/binary-unit:128,0,_/binary>>) -> 2; % 1 bsl 57
overflow_huge_bin_skip_64(<<_:288230376151711744/binary-unit:64,0,_/binary>>) -> 3; % 1 bsl 58
overflow_huge_bin_skip_64(<<_:576460752303423488/binary-unit:32,0,_/binary>>) -> 4; % 1 bsl 59
overflow_huge_bin_skip_64(<<_:1152921504606846976/binary-unit:16,0,_/binary>>) -> 5; % 1 bsl 60
overflow_huge_bin_skip_64(<<_:2305843009213693952/binary-unit:8,0,_/binary>>) -> 6; % 1 bsl 61
overflow_huge_bin_skip_64(<<_:4611686018427387904/binary-unit:8,0,_/binary>>) -> 7; % 1 bsl 62
overflow_huge_bin_skip_64(<<_:9223372036854775808/binary-unit:8,_/binary>>) -> 8; % 1 bsl 63
overflow_huge_bin_skip_64(_) -> nomatch.

overflow_huge_bin_64(<<Bin:18446744073709551616/binary,_/binary>>) -> {1,Bin}; % 1 bsl 64
overflow_huge_bin_64(<<Bin:144115188075855872/binary-unit:128,0,_/binary>>) -> {2,Bin}; % 1 bsl 57
overflow_huge_bin_64(<<Bin:288230376151711744/binary-unit:128,0,_/binary>>) -> {3,Bin}; % 1 bsl 58
overflow_huge_bin_64(<<Bin:576460752303423488/binary-unit:128,0,_/binary>>) -> {4,Bin}; % 1 bsl 59
overflow_huge_bin_64(<<Bin:1152921504606846976/binary-unit:128,0,_/binary>>) -> {5,Bin}; % 1 bsl 60
overflow_huge_bin_64(<<Bin:2305843009213693952/binary-unit:128,0,_/binary>>) -> {6,Bin}; % 1 bsl 61
overflow_huge_bin_64(<<Bin:4611686018427387904/binary-unit:128,0,_/binary>>) -> {7,Bin}; % 1 bsl 62
overflow_huge_bin_64(<<Bin:9223372036854775808/binary-unit:128,0,_/binary>>) -> {8,Bin}; % 1 bsl 63
overflow_huge_bin_64(_) -> nomatch.
