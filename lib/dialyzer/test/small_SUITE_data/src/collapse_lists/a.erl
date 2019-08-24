-module(a).
-export([g/1]).

-export_type([a/0, t/0]).
-type a() :: integer().
-type t() :: a() | maybe_improper_list(t(), t()).

-spec g(t()) -> t().
g(X) -> X.
