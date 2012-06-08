% The Computer Language Shootout
% http://shootout.alioth.debian.org/
% contributed by Isaac Gouy (Erlang novice)

-module(recursive).
-export([main/1]).
-export([small/0,medium/0,big/0]).

small() -> 8.
medium() -> 10.
big() -> 14.


main(Arg) ->
    N = Arg,
    io:fwrite("Ack(3,~w): ~w\n", [N, ack(3, N)]),
    io:fwrite("Fib(~.1f): ~.1f\n", [27.0+float(N), fib(27.0+float(N))]),
    M=N-1, io:fwrite("Tak(~w,~w,~w): ~w\n", [M*3,M*2,M, tak(M*3,M*2,M)]),
    io:fwrite("Fib(~w): ~w\n", [3, fib(3)]),
    io:fwrite("Tak(~.1f,~.1f,~.1f): ~.1f\n", [3.0,2.0,1.0, tak(3.0,2.0,1.0)]),
    exit(ok).

ack(0,N) -> N+1;
ack(M,0) -> ack(M-1,1);
ack(M,N) -> ack(M-1, ack(M,N-1)).

fib(N) when N < 2, float(N) -> 1.0;
fib(N) when N < 2, integer(N) -> 1;
fib(N) -> fib(N-2) + fib(N-1).

tak(X, Y, Z) when Y >= X -> Z;
tak(X, Y, Z) -> tak(tak(X-1,Y,Z), tak(Y-1,Z,X), tak(Z-1,X,Y)).
