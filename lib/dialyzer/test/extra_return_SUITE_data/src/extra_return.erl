-module(extra_return).

-export([t1/0, t2/0]).


% Should warn about having `undefined` as return value when it is not returned by the function
-spec t1() -> true | false | 'other'.
t1() ->
    case rand:uniform(2) of
        1 -> true;
        2 -> false
    end.

% Should not warn about extra return
-dialyzer({no_extra_return, t2/0}).
-spec t2() -> true | false | 'other'.
t2() ->
    case rand:uniform(2) of
        1 -> true;
        2 -> false
    end.
