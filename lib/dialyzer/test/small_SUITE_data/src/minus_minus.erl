%%------------------------------------------------------------------------
%% Test file which gave a bogus warning when analyzed with Dialyzer 1.6.1.
%%------------------------------------------------------------------------
-module(minus_minus).
-export([test/0]).

test() ->
  [] -- [].
