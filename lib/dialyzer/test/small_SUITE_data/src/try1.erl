%%%-------------------------------------------------------------------
%%% File    : try1.erl
%%% Author  :  <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created : 23 Aug 2005 by  <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(try1).

-export([t/1]).

t(X) ->
  case wierd_is_bool(X) of
    true -> ok;
    false -> ok
  end.

wierd_is_bool(X) ->
  try bool(X) of
      Y -> Y
  catch
    _:_  -> false
  end.

bool(true) -> true;
bool(false) -> true.
