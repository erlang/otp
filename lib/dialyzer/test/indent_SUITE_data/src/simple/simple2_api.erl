-module(simple2_api).

-export([c1/2, c2/0, c3/0, c4/1, c5/1, c6/0, c6_b/0, c7/0, c7_b/0,
         c7_c/0, c8/0, c9/0, c10/0, c11/0, c12/0, c13/0, c14/0, c15/0,
         c16/0, c17/0, c18/0, c19/0, c20/0, c21/0, c22/0, c23/0,
         c24/0, c25/0, c26/0]).

-spec c1(simple1_adt:d1(), simple1_adt:d2()) -> boolean().

c1(A, B) ->
    {A} =< {B}. % succ type of A and B is any()

c2() ->
    A = simple1_adt:d1(),
    erlang:make_tuple(1, A). % ok

c3() ->
    A = simple1_adt:d1(),
    setelement(1, {A}, A). % ok

c4(_) ->
    A = simple1_adt:d1(),
    halt(A). % ok (BIF fails...)

c5(_) ->
    A = simple1_adt:d1(),
    [A] -- [A]. % ok

c6() ->
    A = simple1_adt:d1(),
    A ! foo. % opaque term

c6_b() ->
    A = simple1_adt:d1(),
    erlang:send(A, foo). % opaque term

c7() ->
    A = simple1_adt:d1(),
    foo ! A. % ok

c7_b() ->
    A = simple1_adt:d1(),
    erlang:send(foo, A). % ok

c7_c() ->
    A = simple1_adt:d1(),
    erlang:send(foo, A, []). % ok

c8() ->
    A = simple1_adt:d1(),
    A < 3. % opaque term

c9() ->
    A = simple1_adt:d1(),
    lists:keysearch(A, 1, []). % ok

c10() ->
    A = simple1_adt:d1(),
    lists:keysearch(1, A, []). % opaque term 2

c11() ->
    A = simple1_adt:tuple(),
    lists:keysearch(key, 1, [A]). % ok

c12() ->
    A = simple1_adt:tuple(),
    lists:keysearch(key, 1, A). % opaque term 3

c13() ->
    A = simple1_adt:tuple(),
    lists:keysearch(key, 1, [{A,2}]). % ok

c14() ->
    A = simple1_adt:tuple(),
    lists:keysearch(key, 1, [{2,A}]). % ok

c15() ->
    A = simple1_adt:d1(),
    lists:keysearch(key, 1, [A]). % ok

c16() ->
    A = simple1_adt:tuple(),
    erlang:send(foo, A). % ok

c17() ->
    A = simple1_adt:tuple(),
    lists:reverse([A]). % ok

c18() ->
    A = simple1_adt:tuple(),
    lists:keyreplace(a, 1, [A], {1,2}). % ok

c19() ->
    A = simple1_adt:tuple(),
    %% Problem. The spec says argument 4 is a tuple(). Fix that!
    lists:keyreplace(a, 1, [{1,2}], A). % opaque term 4

c20() ->
    A = simple1_adt:tuple(),
    lists:flatten(A). % opaque term 1

c21() ->
    A = simple1_adt:tuple(),
    lists:flatten([[{A}]]). % ok

c22() ->
    A = simple1_adt:tuple(),
    lists:flatten([[A]]). % ok

c23() ->
    A = simple1_adt:tuple(),
    lists:flatten([A]). % ok

c24() ->
    A = simple1_adt:tuple(),
    lists:flatten({A}). % will never return

c25() ->
    A = simple1_adt:d1(),
    B = simple1_adt:tuple(),
    if {A,3} > {A,B} -> true end. % opaque 2nd argument

c26() ->
    B = simple1_adt:tuple(),
    tuple_to_list(B). % opaque term 1
