-module(zoltan_kis1).

-export([f/0, gen/0]).

-opaque id() :: string().

-spec f() -> integer().

%% BIF and Unification(t_unify) issue
f() -> erlang:length(gen()).

-spec gen() -> id().

gen() -> "Dummy".
