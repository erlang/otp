-module(union_paren).

-compile(export_all).

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
