-module(zoltan_kis3).

-export([f/0, gen/0]).

%-opaque id() :: string().

-spec f() -> char().

%% List pattern matching issue
f() -> [H|_T] = gen(), H.

-spec gen() -> zoltan_adt:id().

gen() -> "Dummy".
