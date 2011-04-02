%%---------------------------------------------------------------------------
%% Module that uses the opaque types of int_adt.
%% TODO: Should be extended with invalid contracts.
%%---------------------------------------------------------------------------
-module(int_use).

-export([test/0]).

-spec test() -> int_adt:int().
test() ->
  int_adt:new_i().
