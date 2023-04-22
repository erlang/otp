-module(m6).

-export([id/1, m/0, wrong/1]).

-ifdef(m6).
m() -> ?m6.
-else.
m() -> false.
-endif.

-spec wrong(float()) -> float().
wrong(X) when is_integer(X) -> X.

id(X) -> X.
