%%%-------------------------------------------------------------------
%%% File    : compare1.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 20 Apr 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(compare1).

-export([t/0]).

t() ->
  t(42).

t(X) when X > 42 ->
  error;
t(X) when X < 42 ->
  error;
t(X) when X =/= 42 ->
  error;
t(X) -> ok.
