-module(m1).

-export([id/1, m/0, wrong/1]).

-ifdef(m1).
m() -> ?m1.
-else.
m() -> false.
-endif.

-spec wrong(float()) -> float().
wrong(X) when is_integer(X) -> X.

id(X) -> m2:id(X).
