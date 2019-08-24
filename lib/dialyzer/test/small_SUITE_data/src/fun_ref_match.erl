%%%-------------------------------------------------------------------
%%% File    : fun_ref_match.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : Find that newly created funs and references cannot
%%%               match on earlier bound variables.
%%%
%%% Created : 10 Mar 2005 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(fun_ref_match).

-export([t1/1, t2/1]).

t1(X) ->
  X = fun(Y) -> Y end,
  ok.

t2(X) ->
  case make_ref() of
    X -> error;
    _ -> ok
  end.
