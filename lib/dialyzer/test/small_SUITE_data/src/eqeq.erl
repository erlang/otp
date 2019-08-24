%%%-------------------------------------------------------------------
%%% File    : eqeq.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 12 Nov 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(eqeq).

-export([t/0]).

t() ->
  comp(3.14, foo).

comp(X, Y) -> X =:= Y.
