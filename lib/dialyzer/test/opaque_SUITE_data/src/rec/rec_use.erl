-module(rec_use).

-export([ok1/0, ok2/0, wrong1/0, wrong2/0, wrong3/0, wrong4/0]).

ok1() ->
    rec_adt:set_a(rec_adt:new(), foo).

ok2() ->
    R1 = rec_adt:new(),
    B1 = rec_adt:get_b(R1),
    R2 = rec_adt:set_b(R1, 42),
    B2 = rec_adt:get_b(R2),
    B1 =:= B2.

wrong1() ->
    case rec_adt:new() of
	{rec, _, 42} -> weird1;
	R when tuple_size(R) =:= 3 -> weird2
    end.

wrong2() ->
    R = list_to_tuple([rec, a, 42]),
    rec_adt:get_a(R).

wrong3() ->
    R = rec_adt:new(),
    R =:= {rec, gazonk, 42}.

wrong4() ->
    tuple_size(rec_adt:new()).
