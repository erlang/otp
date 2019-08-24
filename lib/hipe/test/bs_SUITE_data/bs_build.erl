%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bs_build.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Purpose :
%%%
%%% Created : 12 Sep 2007
%%%-------------------------------------------------------------------
-module(bs_build).

-export([test/0]).

test() ->
  <<0,1,2,3,4,5,6>> = Bin = << <<X>> || X <- lists:seq(0, 6)>>,
  test(Bin).

test(Bin) ->
  <<0,1,2,3,4,5,6,0,1,2,3,4,5,6>> = RealBin = multiply(Bin, 2),
  <<6,5,4,3,2,1,0,6,5,4,3,2,1,0>> = reverse(RealBin),
  RealBin = copy(RealBin),
  RealBin = bc(RealBin),
  ok.

multiply(Bin, 1) ->
  Bin;
multiply(Bin, N) when N > 0 ->
  <<(multiply(Bin, N-1))/binary, Bin/binary>>.

bc(Bin) ->
  << <<X>> || <<X>> <= Bin >>.

reverse(<<X, Rest/binary>>) ->
  <<(reverse(Rest))/binary, X>>;
reverse(<<>>) -> <<>>.

copy(Bin) ->
  copy(Bin, <<>>).

copy(<<X, Rest/binary>>, Bin) ->
  copy(Rest, <<Bin/binary, X>>);
copy(<<>>, Bin) -> Bin.
