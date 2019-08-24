-module(union_paren).

-compile(export_all).

-record(r0,
        {
         f1 = 4 :: atom () | integer() | pid(),
         f2 :: atom() | integer() | pid(),
         f3 :: A :: atom() | integer() | pid
        }).

-record(r1,
        {
         f1 = [4] :: [atom ()] | [integer()] | [pid()],
         f2 :: [atom()] | [integer()] | [pid()],
         f3 :: A :: [atom()] | [integer()] | [pid()],
         f8 = [u] :: X :: [A :: atom()] | [B :: integer()] | (C :: [pid()])
        }).

r1() ->
    #r1{f8 = 3.14}.

t1() ->
    t1(3.14).

-spec t1((A :: integer()) | (B :: atom())) -> integer().
t1(A) ->
    fy:bar(A).

t2() ->
    t2(3.14).

-spec t2(integer() | atom()) -> integer().
t2(A) ->
    fy:bar(A).

t3() ->
    3.14 = t3(foo).

-spec t3(_) -> (I :: integer()) | (A :: atom()).
t3(A) when is_atom(A) -> A;
t3(I) when is_integer(I) -> I.

c1() ->
    c1(#r0{f1 = a}).

-spec c1(#r0{f1 :: integer() | pid()}) -> atom().
c1(_) ->
    a.

c2() ->
    c2(#r0{f1 = a}).

-spec c2(#r0{f1 :: A :: integer() | pid()}) -> atom().
c2(_) ->
    a.

c3() ->
    c3(#r0{f1 = a}).

-spec c3(#r0{f1 :: (A :: integer()) | (B :: pid())}) -> atom().
c3(_) ->
    a.

c4() ->
    c4(#r0{f1 = a}).

-spec c4(#r0{f1 :: X :: (A :: integer()) | (B :: pid())}) -> atom().
c4(_) ->
    a.

c5() ->
    c5(#r1{f1 = [a], f2 = [1], f3 = [a]}).

-spec c5(#r1{f1 :: [integer()] | [pid()]}) -> atom().
c5(_) ->
    a.

c6() ->
    c6(#r1{f1 = [a], f2 = [1], f3 = [a]}).

-spec c6(#r1{f1 :: A :: [integer()] | [pid()]}) -> atom().
c6(_) ->
    a.

c7() ->
    c7(#r1{f1 = [a], f2 = [1], f3 = [a]}).

-spec c7(#r1{f1 :: (A :: [integer()]) | (B :: [pid()])}) -> atom().
c7(_) ->
    a.

c8() ->
    c8(#r1{f1 = [a], f2 = [1], f3 = [a]}).

-spec c8(#r1{f1 :: X :: (A :: [integer()]) | (B :: [pid()])}) -> atom().
c8(_) ->
    a.
