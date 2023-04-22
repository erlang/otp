-module(missing_return).

-export([t1/0, t2/0]).


% Should warn about only having true when also false is returned
-spec t1() -> true.
t1() ->
    case rand:uniform(2) of
        1 -> true;
        2 -> false
    end.

% Should not warn about missing return
-dialyzer({no_missing_return, t2/0}).
-spec t2() -> true.
t2() ->
    case rand:uniform(2) of
        1 -> true;
        2 -> false
    end.
