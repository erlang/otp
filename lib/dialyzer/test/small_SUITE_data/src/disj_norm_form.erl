%%%-------------------------------------------------------------------
%%% File    : disj_norm_form.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : Exposes a bad behavior in expansion to
%%%               disjunctive normal form of guards.
%%%
%%% Created : 24 Aug 2007 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(disj_norm_form).

-export([t/1]).

-record(foo, {bar}).

t(R) ->
  if R#foo.bar =:= 1;
     R#foo.bar =:= 2;
     R#foo.bar =:= 3;
     R#foo.bar =:= 4;
     R#foo.bar =:= 5;
     R#foo.bar =:= 6 -> ok;
     true -> error
  end.
