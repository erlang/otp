%%%-------------------------------------------------------------------
%%% File    : t2.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 26 Jul 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(t2).

-export([t1/1]).

t1(X) ->
  t1:t3(X) + t2(X).

t2(X) ->
  X + 1.
