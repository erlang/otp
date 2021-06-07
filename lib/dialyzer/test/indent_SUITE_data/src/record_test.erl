%%%-------------------------------------------------------------------
%%% File    : record_test.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created : 22 Oct 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(record_test).

-export([t/0]).

-record(foo, {bar}).

t() ->
  doit(foo).

doit(X) ->
  case X of
    #foo{} -> error1;
    foo -> ok;
    _ -> error2
  end.
