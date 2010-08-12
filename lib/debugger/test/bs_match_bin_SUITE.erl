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


-module(bs_match_bin_SUITE).

-author('bjorn@erix.ericsson.se').
-export([all/1,init_per_testcase/2,fin_per_testcase/2,init_all/1,finish_all/1,
	 byte_split_binary/1,bit_split_binary/1]).

-include("test_server.hrl").

all(suite) ->
    [{conf,init_all,cases(),finish_all}].

cases() ->
    [byte_split_binary,bit_split_binary].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(1)),
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

byte_split_binary(doc) -> "Tries to split a binary at all byte-aligned positions.";
byte_split_binary(suite) -> [];
byte_split_binary(Config) when list(Config) ->
    ?line L = lists:seq(0, 57),
    ?line B = mkbin(L),
    ?line byte_split(L, B, size(B)).

byte_split(L, B, Pos) when Pos >= 0 ->
    ?line Sz1 = Pos,
    ?line Sz2 = size(B) - Pos,
    ?line <<B1:Sz1/binary,B2:Sz2/binary>> = B,
    ?line B1 = list_to_binary(lists:sublist(L, 1, Pos)),
    ?line B2 = list_to_binary(lists:nthtail(Pos, L)),
    ?line byte_split(L, B, Pos-1);
byte_split(_L, _B, _) -> ok.

bit_split_binary(doc) -> "Tries to split a binary at all positions.";
bit_split_binary(suite) -> [];
bit_split_binary(Config) when list(Config) ->
    Fun = fun(Bin, List, SkipBef, N) ->
		  ?line SkipAft = 8*size(Bin) - N - SkipBef,
		  io:format("~p, ~p, ~p", [SkipBef,N,SkipAft]),
		  ?line <<_I1:SkipBef,OutBin:N/binary-unit:1,_I2:SkipAft>> = Bin,
		  ?line OutBin = make_bin_from_list(List, N)
	  end,
    ?line bit_split_binary1(Fun, erlang:md5(<<1,2,3>>)),
    ok.

bit_split_binary1(Action, Bin) ->
    BitList = bits_to_list(binary_to_list(Bin), 16#80),
    bit_split_binary2(Action, Bin, BitList, 0).

bit_split_binary2(Action, Bin, [_|T]=List, Bef) ->
    bit_split_binary3(Action, Bin, List, Bef, size(Bin)*8),
    bit_split_binary2(Action, Bin, T, Bef+1);
bit_split_binary2(_Action, _Bin, [], _Bef) -> ok.

bit_split_binary3(Action, Bin, List, Bef, Aft) when Bef =< Aft ->
    Action(Bin, List, Bef, (Aft-Bef) div 8 * 8),
    bit_split_binary3(Action, Bin, List, Bef, Aft-8);
bit_split_binary3(_, _, _, _, _) -> ok.

make_bin_from_list(_List, 0) ->
    mkbin([]);
make_bin_from_list(List, N) ->
    list_to_binary([make_int(List, 8, 0),
		    make_bin_from_list(lists:nthtail(8, List), N-8)]).


make_int(_List, 0, Acc) -> Acc;
make_int([H|T], N, Acc) -> make_int(T, N-1, Acc bsl 1 bor H).

bits_to_list([_H|T], 0) -> bits_to_list(T, 16#80);
bits_to_list([H|_]=List, Mask) ->
    [case H band Mask of
	 0 -> 0;
	 _ -> 1
     end|bits_to_list(List, Mask bsr 1)];
bits_to_list([], _) -> [].


mkbin(L) when list(L) -> list_to_binary(L).
