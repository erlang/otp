%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------------
%% The guard in f/3 revealed a problem in the translation of the 'bs_add'
%% BEAM instruction to Icode. The fail label was not properly translated.
%% Fixed 3/2/2011. Then in 2015 we found another issue: g/2.  Also fixed.
%%-------------------------------------------------------------------------
-module(bs_add).

-export([test/0]).

test() ->
  42 = f(<<12345:16>>, 4711, <<42>>),
  true = g(<<1:13>>, 3),  %% was handled OK, but
  false = g(<<>>, gurka), %% this one leaked badarith
  ok.

f(Bin, A, B) when <<A:9, B:7/binary>> == Bin ->
  gazonk;
f(Bin, _, _) when is_binary(Bin) ->
  42.

%% Complex way of testing (bit_size(Bin) + Len) rem 8 =:= 0
g(Bin, Len) when is_binary(<<Bin/binary-unit:1, 123:Len>>) ->
    true;
g(_, _) ->
    false.
