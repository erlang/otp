%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------

-module(bs_split).

-export([test/0]).

test() ->
  Funs = [fun byte_split_binary/0, fun bit_split_binary/0, fun z_split/0],
  lists:foreach(fun (F) -> ok = F() end, Funs).

%%--------------------------------------------------------------------

byte_split_binary() ->
  L = lists:seq(0, 57),
  B = mkbin(L),
  byte_split(L, B, byte_size(B)).

byte_split(L, B, Pos) when Pos >= 0 ->
  Sz1 = Pos,
  Sz2 = byte_size(B) - Pos,
  bs1(L, B, Pos, Sz1, Sz2);
byte_split(_, _, _) -> ok.

bs1(L, B, Pos, Sz1, Sz2) ->
  <<B1:Sz1/binary, B2:Sz2/binary>> = B,
  bs2(L, B, Pos, B1, B2).

bs2(L, B, Pos, B1, B2) ->
  B1 = list_to_binary(lists:sublist(L, 1, Pos)),
  bs3(L, B, Pos, B2).

bs3(L, B, Pos, B2) ->
  B2 = list_to_binary(lists:nthtail(Pos, L)),
  byte_split(L, B, Pos - 1).

%%--------------------------------------------------------------------

bit_split_binary() ->
  Fun = fun(Bin, List, SkipBef, N) ->
	    SkipAft = bit_size(Bin) - N - SkipBef,
	    %% io:format("~p, ~p, ~p", [SkipBef,N,SkipAft]),
	    <<_I1:SkipBef,OutBin:N/binary-unit:1,_I2:SkipAft>> = Bin,
	    OutBin = make_bin_from_list(List, N)
	end,
  bit_split_binary1(Fun, erlang:md5(<<1,2,3>>)).

bit_split_binary1(Action, Bin) ->
  BitList = bits_to_list(binary_to_list(Bin), 16#80),
  bit_split_binary2(Action, Bin, BitList, 0).

bit_split_binary2(Action, Bin, [_|T]=List, Bef) ->
  bit_split_binary3(Action, Bin, List, Bef, bit_size(Bin)),
  bit_split_binary2(Action, Bin, T, Bef+1);
bit_split_binary2(_Action, _Bin, [], _Bef) -> ok.

bit_split_binary3(Action, Bin, List, Bef, Aft) when Bef =< Aft ->
  Action(Bin, List, Bef, (Aft-Bef) div 8 * 8),
  bit_split_binary3(Action, Bin, List, Bef, Aft - 8);
bit_split_binary3(_, _, _, _, _) -> ok.

make_bin_from_list(_List, 0) ->
  mkbin([]);
make_bin_from_list(List, N) ->
  list_to_binary([make_int(List, 8, 0),
		  make_bin_from_list(lists:nthtail(8, List), N - 8)]).

make_int(_List, 0, Acc) -> Acc;
make_int([H|T], N, Acc) -> make_int(T, N-1, Acc bsl 1 bor H).

bits_to_list([_|T], 0) -> bits_to_list(T, 16#80);
bits_to_list([H|_]=List, Mask) ->
  [case H band Mask of
     0 -> 0;
     _ -> 1
   end|bits_to_list(List, Mask bsr 1)];
bits_to_list([], _) -> [].

mkbin(L) when is_list(L) -> list_to_binary(L).

%%--------------------------------------------------------------------
%% Splits a series of null terminated segments of a binary without
%% creating any new sub-binaries until the zero is found.

z_split() ->
  [<<61,62,63>>] = z_split(<<61,62,63>>),
  [<<61,62,63>>, <<>>] = z_split(<<61,62,63,0>>),
  [<<61,62,63>>, <<64>>] = z_split(<<61,62,63,0,64>>),
  [<<61,62,63>>, <<64,65,66>>] = z_split(<<61,62,63,0,64,65,66>>),
  [<<61,62>>, <<64>>, <<>>, <<65,66>>] = z_split(<<61,62,0,64,0,0,65,66>>),
  ok.

z_split(B) when is_binary(B) ->
  z_split(B, 0).

z_split(B, N) ->
  case B of
    <<_B1:N/binary,0,_B2/binary>> ->	% use skip_bits for B1, B2
      <<B1:N/binary,_,B2/binary>> = B,	% and postpone the matching
      [B1 | z_split(B2)];
    <<_:N/binary>> ->
      [B];
    _ ->
      z_split(B, N + 1)
  end.
