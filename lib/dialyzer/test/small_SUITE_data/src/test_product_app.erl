-module(test_product_app).
-export([zero/0, one/1]).

-type mfa() :: integer().
-type product() :: binary().

-spec zero() -> any().
zero() ->
    ok.

-spec one(mfa()) -> mfa().
one(I) when is_integer(I) ->
    I * 42.
