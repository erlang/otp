-module(zoltan_kis6).

-export([f/0, gen/0]).

-opaque id() :: {integer(),atom()}.

%%-spec f() -> id().

%% Tuple Unification (t_unify) issue
f() -> {X,Y} = gen().

-spec gen() -> id().

gen() -> {34, leprecon}.
