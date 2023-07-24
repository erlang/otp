-module(b).
-export([mk_a/0, i/1]).
-import_type(a, [a/0,i/0]).

-spec mk_a() -> a().
mk_a() -> foo.

-spec i(i()) -> ok.
i(I) when is_atom(I) -> ok.
