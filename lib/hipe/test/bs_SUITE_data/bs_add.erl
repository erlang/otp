%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------------
%% The guard in f/3 revealed a problem in the translation of the 'bs_add'
%% BEAM instruction to Icode. The fail label was not properly translated.
%% Fixed 3/2/2011.
%%-------------------------------------------------------------------------
-module(bs_add).

-export([test/0]).

test() ->
  42 = f(<<12345:16>>, 4711, <<42>>),
  ok.

f(Bin, A, B) when <<A:9, B:7/binary>> == Bin ->
  gazonk;
f(Bin, _, _) when is_binary(Bin) ->
  42.
