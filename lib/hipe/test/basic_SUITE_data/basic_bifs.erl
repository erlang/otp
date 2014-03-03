%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests for handling of BIFs in guards and body calls.
%%%-------------------------------------------------------------------
-module(basic_bifs).

-export([test/0]).

test() ->
  ok = test_element(),
  ok = test_binary_part().

%%--------------------------------------------------------------------

test_element() ->
  true  = elem({a, b}),
  false = elem({a, c}),
  other = elem(gazonk),
  ok.

elem(T) when element(1, T) == a -> element(2, T) == b;
elem(_) -> other.

%%--------------------------------------------------------------------
%% Checks that 2-ary and 3-ary BIFs can be compiled to native code.

test_binary_part() ->
  Bin = <<1,2,3,4,5,6,7,8,9,10>>,
  BinPart = bp3(Bin),
  <<7,8>> = bp2(BinPart),
  ok.

bp2(Bin) ->
  binary_part(Bin, {1, 2}).

bp3(Bin) ->
  binary_part(Bin, byte_size(Bin), -5).

%%--------------------------------------------------------------------
