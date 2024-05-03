-module(m5).

-export([id/1, m/0, wrong/1]).

-ifdef(m5).
m() -> ?m5.
-else.
m() -> false.
-endif.

-spec wrong(float()) -> float().
wrong(X) when is_integer(X) -> X.

id(X) -> X.
