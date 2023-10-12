-module(gencall).

-export([f/0]).

f() ->
  gen_server:call(self(),request,{not_a_timeout}),
  ets:lookup(1,2,3),
  some_mod:some_function(),
  gencall:foo(),
  gen_server:handle_cast(1,2).
