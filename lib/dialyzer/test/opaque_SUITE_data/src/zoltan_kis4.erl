-module(zoltan_kis4).

-export([f/0, gen/0]).

-export_type([id/0]).

-opaque id() :: string().

-spec f() -> id().
f() -> "Dummy" = gen().  %% Matching issue

-spec gen() -> id().
gen() -> "Dummy".
