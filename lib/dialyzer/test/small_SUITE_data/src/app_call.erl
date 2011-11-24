-module(app_call).
-export([test/1]).

test(m) ->
  M = get_mod(),
  M:foo();
test(f) ->
  F = get_fun(),
  mod:F();
test(_) ->
  ok.

get_mod() ->
  42.

get_fun() ->
  {gazonk, []}.
