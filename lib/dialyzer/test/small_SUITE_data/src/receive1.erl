%%%-------------------------------------------------------------------
%%% File    : receive1.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 27 Mar 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(receive1).

-export([t/1]).

t(X) ->
  receive
  after
    infinity -> X
  end.
