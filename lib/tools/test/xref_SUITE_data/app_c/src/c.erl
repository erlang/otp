-module(c).

-export([do_a/1, do_b/1, do_illegal/0]).

do_a(Arg) ->
    a:public(Arg).

do_b(Arg) ->
    b:inc(Arg).

do_illegal() ->
    a:internal(1,2),
    b:dec(10).