%% Error: gen_server:handle_cast/2 is not logged as an unexported func
%% but unknown function.
-module(gencall).

-export([f/0]).

f() ->
  gen_server:call(1,2,3),
  ets:lookup(1,2,3),
  some_mod:some_function(),
  gencall:foo(),
  gen_server:handle_cast(1,2).
