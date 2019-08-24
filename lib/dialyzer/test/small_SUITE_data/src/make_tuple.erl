-module(make_tuple).
-export([test/0]).

test() ->
  {_,_} = erlang:make_tuple(3, []).
