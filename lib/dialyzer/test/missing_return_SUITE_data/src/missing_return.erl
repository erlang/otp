-module(missing_return).

-export([t1/0]).


% Should warn about only having true when also false is returned
-spec t1() -> true.
t1() ->
    case rand:uniform(2) of
        1 -> true;
        2 -> false
    end.
