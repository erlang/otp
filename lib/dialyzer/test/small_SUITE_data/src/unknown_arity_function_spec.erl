-module(unknown_arity_function_spec).

-export([test/2]).

%-type t() :: 42 | fun((...) -> t()).
%-type f() :: fun((...) -> 42).

-spec test(fun((...) -> 42), list()) -> 42.
test(F, L) ->
  42 = apply(F, L).
