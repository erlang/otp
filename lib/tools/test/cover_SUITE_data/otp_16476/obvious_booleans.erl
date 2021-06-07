-module(obvious_booleans).
-export([?MODULE/0]).
-compile([warnings_as_errors]).

?MODULE() ->
    true = both_ok(ok, ok),
    false = both_ok(ok, nok),
    true = one_ok(ok, nok),
    true = one_ok(nok, ok),
    false = one_ok(nok, nok),
    ok.

both_ok(A, B) ->
    A =:= ok andalso B =:= ok.

one_ok(A, B) ->
    A =:= ok orelse B =:= ok.
