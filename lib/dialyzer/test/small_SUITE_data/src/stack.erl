-module(stack).

-export([new/0, push/2, pop/1]).
-export_type([stack/1]).

-opaque stack(A) :: [A].

-spec new() -> stack(none()).
new() -> [].

-spec push(stack(A), A) -> stack(A).
push(T, H) -> [H | T].

-spec pop(stack(A)) -> {A, stack(A)}.
pop([H | T]) -> {H, T}.
