%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-module(bs_match_int_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 integer/1,signed_integer/1,dynamic/1,more_dynamic/1,mml/1,
	 match_huge_int/1,bignum/1,unaligned_32_bit/1]).

-include_lib("common_test/include/ct.hrl").

-import(lists, [seq/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() -> 
    [integer, signed_integer, dynamic, more_dynamic, mml,
     match_huge_int, bignum, unaligned_32_bit].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

integer(Config) when is_list(Config) ->
    0 = get_int(mkbin([])),
    0 = get_int(mkbin([0])),
    42 = get_int(mkbin([42])),
    255 = get_int(mkbin([255])),
    256 = get_int(mkbin([1,0])),
    257 = get_int(mkbin([1,1])),
    258 = get_int(mkbin([1,2])),
    258 = get_int(mkbin([1,2])),
    65534 = get_int(mkbin([255,254])),
    16776455 = get_int(mkbin([255,253,7])),
    4245492555 = get_int(mkbin([253,13,19,75])),
    4294967294 = get_int(mkbin([255,255,255,254])),
    4294967295 = get_int(mkbin([255,255,255,255])),
    Eight = [200,1,19,128,222,42,97,111],
    cmp128(Eight, uint(Eight)),
    fun_clause(catch get_int(mkbin(seq(1,5)))),
    ok.

get_int(Bin) ->
    I = get_int1(Bin),
    get_int(Bin, I).

get_int(Bin0, I) when size(Bin0) < 4 ->
    Bin = <<0,Bin0/binary>>,
    I = get_int1(Bin),
    get_int(Bin, I);
get_int(_, I) -> I.

get_int1(<<I:0>>) -> I;
get_int1(<<I:8>>) -> I;
get_int1(<<I:16>>) -> I;
get_int1(<<I:24>>) -> I;
get_int1(<<I:32>>) -> I.

cmp128(<<I:128>>, I) -> equal;
cmp128(_, _) -> not_equal.

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
    %%    io:format("~p ~p ~p ~p\n", [S1,S2,A,B]),
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
    more_dynamic1(Unsigned, funny_binary(42)),

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
			      ct:fail(failed)
		      end
	      end,
    more_dynamic1(Signed, funny_binary(43)),

    %% Unsigned little-endian numbers.
    UnsLittle  = fun(Bin, List, SkipBef, N) ->
			 SkipAft = 8*size(Bin) - N - SkipBef,
			 <<_:SkipBef,Int:N/little,_:SkipAft>> = Bin,
			 Int = make_int(big_to_little(List, N), N, 0)
		 end,
    more_dynamic1(UnsLittle, funny_binary(44)),

    %% Signed little-endian numbers.
    SignLittle  = fun(Bin, List, SkipBef, N) ->
			  SkipAft = 8*size(Bin) - N - SkipBef,
			  <<_:SkipBef,Int:N/signed-little,_:SkipAft>> = Bin,
			  Little = big_to_little(List, N),
			  Int = make_signed_int(Little, N)
		  end,
    more_dynamic1(SignLittle, funny_binary(45)),

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

funny_binary(N) ->
    B0 = erlang:md5([N]),
    {B1,_B2} = split_binary(B0, byte_size(B0) div 3),
    B1.

mml(Config) when is_list(Config) ->
    single_byte_binary = mml_choose(<<42>>),
    multi_byte_binary = mml_choose(<<42,43>>).

mml_choose(<<_A:8>>) -> single_byte_binary;
mml_choose(<<_A:8,_T/binary>>) -> multi_byte_binary.

match_huge_int(Config) when is_list(Config) ->
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
	    %% allocation succeds will the matching fail because
	    %% the binary is too small.
	    ok
    end,
    nomatch = overflow_huge_int_skip_64(Bin),
    nomatch = overflow_huge_int_64(Bin),

    %% Test overflowing the size of an integer field using variables as sizes.
    Sizes = case erlang:system_info(wordsize) of
		4 -> lists:seq(25, 32);
		8 -> []
	    end ++ lists:seq(50, 64),
    ok = overflow_huge_int_unit128(Bin, Sizes),

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
    erlang:garbage_collect(),			%Search for holes in debug-build.
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

id(I) -> I.
