%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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

-module(bs_match_int_SUITE).

-author('bjorn@erix.ericsson.se').
-export([all/1,init_per_testcase/2,fin_per_testcase/2,init_all/1,finish_all/1,
	 integer/1,signed_integer/1,dynamic/1,more_dynamic/1,mml/1]).

-include("test_server.hrl").

-import(lists, [seq/2]).

all(suite) ->
    [{conf,init_all,cases(),finish_all}].

cases() ->
    [integer,signed_integer,dynamic,more_dynamic,mml].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(4)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_all(Config) when is_list(Config) ->
    ?line test_lib:interpret(?MODULE),
    ?line true = lists:member(?MODULE, int:interpreted()),
    ok.

finish_all(Config) when is_list(Config) ->
    ok.

integer(suite) -> [];
integer(Config) when list(Config) ->
    ?line 0 = get_int(mkbin([])),
    ?line 0 = get_int(mkbin([0])),
    ?line 42 = get_int(mkbin([42])),
    ?line 255 = get_int(mkbin([255])),
    ?line 256 = get_int(mkbin([1,0])),
    ?line 257 = get_int(mkbin([1,1])),
    ?line 258 = get_int(mkbin([1,2])),
    ?line 258 = get_int(mkbin([1,2])),
    ?line 65534 = get_int(mkbin([255,254])),
    ?line 16776455 = get_int(mkbin([255,253,7])),
    ?line 4245492555 = get_int(mkbin([253,13,19,75])),
    ?line Eight = [200,1,19,128,222,42,97,111],
    ?line cmp128(Eight, uint(Eight)),
    ?line fun_clause(catch get_int(mkbin(seq(1,5)))),
    ok.

get_int(<<I:0>>) -> I;
get_int(<<I:8>>) -> I;
get_int(<<I:16>>) -> I;
get_int(<<I:24>>) -> I;
get_int(<<I:32>>) -> I.

cmp128(<<I:128>>, I) -> equal;
cmp128(_B, _I) -> not_equal.

signed_integer(suite) -> [];
signed_integer(Config) when list(Config) ->
    ?line {no_match,_} = sint(mkbin([])),
    ?line {no_match,_} = sint(mkbin([1,2,3])),
    ?line 127 = sint(mkbin([127])),
    ?line -1 = sint(mkbin([255])),
    ?line -128 = sint(mkbin([128])),
    ?line 42 = sint(mkbin([42,255])),
    ?line 127 = sint(mkbin([127,255])).

sint(Bin) ->
    case Bin of
	<<I:8/signed>> -> I;
	<<I:8/signed,_:3,_:5>> -> I;
	Other -> {no_match,Other}
    end.

uint(L) -> uint(L, 0).
uint([H|T], Acc) -> uint(T, Acc bsl 8 bor H);
uint([], Acc) -> Acc.

dynamic(Config) when list(Config) ->
    dynamic(mkbin([255]), 8),
    dynamic(mkbin([255,255]), 16),
    dynamic(mkbin([255,255,255]), 24),
    dynamic(mkbin([255,255,255,255]), 32),
    ok.

dynamic(Bin, S1) when S1 >= 0 ->
    S2 = size(Bin) * 8 - S1,
    dynamic(Bin, S1, S2, (1 bsl S1) - 1, (1 bsl S2) - 1),
    dynamic(Bin, S1-1);
dynamic(_Bin, _) -> ok.

dynamic(Bin, S1, S2, A, B) ->
%    io:format("~p ~p ~p ~p\n", [S1,S2,A,B]),
    case Bin of
	<<A:S1,B:S2>> ->
	    io:format("~p ~p ~p ~p\n", [S1,S2,A,B]),
	    ok;
	_Other ->
	    erlang:error(badmatch, [Bin,S1,S2,A,B])
    end.

more_dynamic(doc) -> "Extract integers at different alignments and of different sizes.";
more_dynamic(Config) when list(Config) ->

    % Unsigned big-endian numbers.
    Unsigned  = fun(Bin, List, SkipBef, N) ->
			SkipAft = 8*size(Bin) - N - SkipBef,
			<<_I1:SkipBef,Int:N,_I2:SkipAft>> = Bin,
			Int = make_int(List, N, 0)
		end,
    ?line more_dynamic1(Unsigned, funny_binary(42)),

    % Signed big-endian numbers.
    Signed  = fun(Bin, List, SkipBef, N) ->
		      SkipAft = 8*size(Bin) - N - SkipBef,
		      <<_I1:SkipBef,Int:N/signed,_I2:SkipAft>> = Bin,
		      case make_signed_int(List, N) of
			  Int -> ok;
			  Other ->
			      io:format("Bin = ~p,", [Bin]),
			      io:format("SkipBef = ~p, N = ~p", [SkipBef,N]),
			      io:format("Expected ~p, got ~p", [Int,Other]),
			      ?t:fail()
		      end
	      end,
    ?line more_dynamic1(Signed, funny_binary(43)),

    % Unsigned little-endian numbers.
    UnsLittle  = fun(Bin, List, SkipBef, N) ->
			 SkipAft = 8*size(Bin) - N - SkipBef,
			 <<_I1:SkipBef,Int:N/little,_I2:SkipAft>> = Bin,
			 Int = make_int(big_to_little(List, N), N, 0)
		 end,
    ?line more_dynamic1(UnsLittle, funny_binary(44)),

    % Signed little-endian numbers.
    SignLittle  = fun(Bin, List, SkipBef, N) ->
			  SkipAft = 8*size(Bin) - N - SkipBef,
			  <<_I1:SkipBef,Int:N/signed-little,_I2:SkipAft>> = Bin,
			  Little = big_to_little(List, N),
			  Int = make_signed_int(Little, N)
		  end,
    ?line more_dynamic1(SignLittle, funny_binary(45)),

    ok.

funny_binary(N) ->
    B0 = erlang:md5([N]),
    {B1,_B2} = split_binary(B0, size(B0) div 2),
    B1.

more_dynamic1(Action, Bin) ->
    BitList = bits_to_list(binary_to_list(Bin), 16#80),
    more_dynamic2(Action, Bin, BitList, 0).

more_dynamic2(Action, Bin, [_|T]=List, Bef) ->
    more_dynamic3(Action, Bin, List, Bef, size(Bin)*8),
    more_dynamic2(Action, Bin, T, Bef+1);
more_dynamic2(_Action, _Bin, [], _Bef) -> ok.

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
make_signed_int([0|_T]=List, N) -> make_int(List, N, 0);
make_signed_int([1|_T]=List0, N) ->
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

bits_to_list([_H|T], 0) -> bits_to_list(T, 16#80);
bits_to_list([H|_]=List, Mask) ->
    [case H band Mask of
	 0 -> 0;
	 _ -> 1
     end|bits_to_list(List, Mask bsr 1)];
bits_to_list([], _) -> [].

fun_clause({'EXIT',{function_clause,_}}) -> ok.
mkbin(L) when list(L) -> list_to_binary(L).

mml(Config) when list(Config) ->
    ?line single_byte_binary = mml_choose(<<42>>),
    ?line multi_byte_binary = mml_choose(<<42,43>>).

mml_choose(<<_A:8>>) -> single_byte_binary;
mml_choose(<<_A:8, _T/binary>>) -> multi_byte_binary.
