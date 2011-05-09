-module(zoltan_kis3).

-export([f/0, gen/0]).

-opaque id() :: string().

-spec f() -> char().

%% List pattern matching issue
f() -> [H|_T] = gen(), H.

-spec gen() -> id().

gen() -> "Dummy".
