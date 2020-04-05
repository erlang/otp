%%%-------------------------------------------------------------------
%%% File    : contract3.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : Check overloaded domains
%%%
%%% Created :  2 Nov 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(contract3).

-export([t/3]).

t(X, Y, Z) ->
  t1(X),
  t2(X, Y),
  t3(X, Y, Z).

-spec t1(atom()|integer()) -> integer();
        (atom()|list()) -> atom().

t1(X) ->
  f(X).

-spec t2(atom(), integer()) -> integer();
        (atom(), list()) -> atom().

t2(X, Y) ->
  g(X, Y).

-spec t3(atom(), integer(), list()) -> integer();
        (X, integer(), list()) -> X.

t3(X, Y, Z) ->
  X.

%% dummy functions below

f(X) -> X.

g(X, Y) when is_atom(X), is_integer(Y) -> Y;
g(X, Y) when is_atom(X), is_list(Y) -> X.
