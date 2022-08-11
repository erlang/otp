-module(m4).

-export([id/1, m/0, wrong/1]).

-ifdef(m4).
m() -> ?m4.
-else.
m() -> false.
-endif.

-spec wrong(float()) -> float().
wrong(X) when is_integer(X) -> X.

id(X) -> m5:id(X), m6:id(X).
