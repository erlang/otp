%%-------------------------------------------------------------------
%% File    : unused_cases.erl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Description : Tests that Dialyzer warns whenever it finds unused
%%		 case clauses -- even those that are catch all.
%%
%% Created : 21 Jan 2007 by Kostis Sagonas <kostis@cs.ntua.gr>
%%-------------------------------------------------------------------

-module(unused_cases).
-export([test/0]).

test() -> % dummy function to avoid exporting stuff
  ok = unreachable_catchall(42),
  ok = unreachable_middle(42),
  ok = unreachable_final(42).

unreachable_catchall(X) ->
  case mk_pair(X) of
    {_,_} -> ok;
    OTHER -> {unreachable_catchall, OTHER}
  end.

unreachable_middle(X) ->
  case is_positive(X) of
    true  -> ok;
    weird -> {unreachable_middle, weird};
    false -> ok
  end.

unreachable_final(X) ->
  case is_positive(X) of
    true  -> ok;
    false -> ok;
    OTHER-> {unreachable_final, OTHER}
  end.

mk_pair(X) -> {X, X}.

is_positive(X) when is_integer(X), X > 0 -> true;
is_positive(X) when is_integer(X) -> false.
