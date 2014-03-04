%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
%% Purpose: test support for UTF datatypes in binaries - INCOMPLETE
%%-------------------------------------------------------------------

-module(bs_utf).

-export([test/0]).

test() ->
  <<65>> = b65utf8(),
  ok = m(<<65>>).

m(<<65/utf8>>) ->
  ok.

b65utf8() ->
  <<65/utf8>>.
