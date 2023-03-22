-module(m3).

-export([id/1, m/0, wrong/1]).

-ifdef(m3).
m() -> ?m3.
-else.
m() -> false.
-endif.

-spec wrong(float()) -> float().
wrong(X) when is_integer(X) -> X.

id(X) -> m4:id(X).
