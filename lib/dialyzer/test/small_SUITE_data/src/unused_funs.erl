%% See also ERL-593.

-module(unused_funs).

-export([test/0]).

test() -> % "has no local return"
    Var = outer_scope,
    case other_error of
        error -> % "can never match"
            %% No warnings "no local return" and "_ = 1 can never match 0" (!)
            foo(fun() -> {Var, 1 = 0} end)
    end.

not_used() -> % "will never be called"
    %% No warnings "no local return" and "1 can never match 0".
    foo(fun() -> 1 = 0 end).

foo(Fun) -> % "will never be called"
    1 = 0, % No pattern match warning (foo/1 is not traversed at all).
    Fun().
