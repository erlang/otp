-module(specs_and_funs).

-export([my_apply/3, two/1]).

%% OTP-15519, ERL-815

-spec my_apply(Fun, Arg, fun((A) -> A)) -> Result when
      Fun :: fun((Arg) -> Result),
      Arg :: any(),
      Result :: any().

my_apply(Fun, Arg, _) ->
    Fun(Arg).

-spec two(fun((A) -> A)) -> fun((B) -> B).

two(F) ->
    F(fun(X) -> X end).
