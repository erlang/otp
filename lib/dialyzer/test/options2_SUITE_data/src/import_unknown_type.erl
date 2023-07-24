-module(import_unknown_type).
-export([mk_a/0]).
-import_type(a, [a/0]).

-spec mk_a() -> a().
mk_a() -> foo.
