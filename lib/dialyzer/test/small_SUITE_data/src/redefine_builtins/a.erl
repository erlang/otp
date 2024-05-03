-module(a).
-export([vi/1, sum/2, vc/1]).

-spec vi(b:integer()) -> 'ok'.
vi(I) when is_integer(I) ->
    ok.

-spec sum(b:integer(), integer()) -> integer().
sum([A], B) ->
    A + B.

-spec vc(b:collection()) -> 'ok'.
vc({Int, List}) when length(List) =:= Int ->
    ok.
