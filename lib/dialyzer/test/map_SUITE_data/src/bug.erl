-module(bug).

-export([t1/0, f1/1]).

t1() ->
    V = f1(#{a=>b}),
    case V of
	#{a := Q} -> Q; %% Must not warn here
	_ -> ok
    end,
    ok.

f1(M) -> %% Should get map() succ typing
    #{} = M.
