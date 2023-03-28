-module(b).
-feature(internal_export, enable).

-export([inc/1]).

-internal_export([dec/1]).

inc(X) -> dec(X).

dec(X) -> a:internal(X,-1).