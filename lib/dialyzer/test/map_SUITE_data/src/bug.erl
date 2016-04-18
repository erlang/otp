-module(bug).

-export([t1/0, f1/1
	,t2/0, f2/1
	,t3/0, f3/1
	,t4/0, f4/1
	,t5/0, f5/1
	]).

t1() ->
    V = f1(#{a=>b}),
    case V of
	#{a := Q} -> Q; %% Must not warn here
	_ -> ok
    end,
    ok.

f1(M) -> %% Should get map() succ typing
    #{} = M.

t2() ->
    V = f2([#{a=>b}]),
    case V of
	[#{a := P}] -> P; %% Must not warn here
	_ -> ok
    end,
    ok.

f2(M) -> %% Should get [map(),...] succ typing
    [#{}] = M.

t3() ->
    V = f3([#{a=>b},a]),
    case V of
	[#{a := P}, _Q] -> P; %% Must not warn here
	_ -> ok
    end,
    ok.

f3(M) -> %% Should get [map()|a,...] succ typing
    [#{},a] = M.

t4() ->
    V = f4({#{a=>b},{}}),
    case V of
	{#{a := P},{}} -> P; %% Must not warn here
	_ -> ok
    end,
    ok.

f4(M) -> %% Should get {map(),{}} succ typing
    {#{},{}} = M.

t5() ->
    V = f5(#{k=>q,a=>b}),
    case V of
	#{k := q, a := P} -> P; %% Must not warn here
	_ -> ok
    end,
    ok.

f5(M) -> %% Should get #{k:=q, ...} succ typing
    #{k:=q} = M.
