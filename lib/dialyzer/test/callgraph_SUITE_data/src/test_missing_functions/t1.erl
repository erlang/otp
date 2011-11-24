%%%-------------------------------------------------------------------
%%% File    : t1.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 26 Jul 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(t1).

-export([t1/1, t2/1]).

t1(X) ->
  t2:t1(X).

t2(X) ->
  t2:t2(X).
