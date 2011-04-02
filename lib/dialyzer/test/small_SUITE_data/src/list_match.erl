%%%-------------------------------------------------------------------
%%% File    : list_match.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 12 Mar 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(list_match).

-export([t/0]).

t() ->
  t([1,2,3,4]).

t([]) ->
  ok;
t([H|T]) when is_integer(H) ->
  t(T);
t([_|T]) ->
  t(T).
