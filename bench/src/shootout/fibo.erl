%%% -*- mode: erlang -*-
%%% $Id: fibo.erlang,v 1.5 2005-04-25 19:01:38 igouy-guest Exp $
%%% http://shootout.alioth.debian.org/

-module(fibo).
-export([main/1]).
-export([small/0,medium/0,big/0]).

small() -> 38.
medium() -> 42.
big() -> 46.

main(Arg) ->
    Num = Arg,
    io:fwrite("~w\n", [fib(Num)]),
    exit(ok).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).
