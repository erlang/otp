%%%-------------------------------------------------------------------
%%% File    : tuple1.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : Exposed two bugs in the analysis;
%%%               one suppressed warning and one crash.
%%%
%%% Created : 13 Nov 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(tuple1).

-export([t1/2, t2/2, t3/2, bar/2]).

t1(List = [_|_], X) ->
  lists:mapfoldl(fun foo/2, X, List).

t2(List = [_|_], X) ->
  lists:mapfoldl(fun bar/2, X, List).

t3(List = [_|_], X) ->
  lists:mapfoldl(fun baz/1, X, List).


foo(1, 1) -> a;
foo(a, 1) -> b.

bar(1, 1) -> {b, b};
bar(a, 1) -> {a, a}.

baz(1) -> 1.
