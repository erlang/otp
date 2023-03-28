-module(a).
-feature(internal_export, enable).

-export([public/1]).

-internal_export([internal/2]).

public(X) -> internal(X,1).

internal(X, Y) -> X + Y.