%% Analysis could enter an infinite loop when a named fun called itself.
-module(fun_hangs).
-export([bar/1, foo/0]).

-spec bar(fun((pos_integer()) -> term())) -> term().
bar(Fun) ->
  Fun.

foo() ->
    bar(fun F(_) -> F(1) end).
