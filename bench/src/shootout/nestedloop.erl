%%%  The Great Computer Language Shootout 
%%%  http://shootout.alioth.debian.org/
%%% 
%%%  modified by Isaac Gouy

-module(nestedloop).
-export([main/1]).
-export([small/0,medium/0,big/0]).

small() -> 10.
medium() -> 30.
big() -> 35.

main(Arg) ->
   Num = Arg,
   io:fwrite("~w\n", [loopA(Num, Num, 0)]),
   exit(ok).


loopA(0, M, N) -> N;
loopA(I, M, N) -> loopA(I - 1, M, loopB(M, M, N)).

loopB(0, M, N) -> N;
loopB(I, M, N) -> loopB(I - 1, M, loopC(M, M, N)).

loopC(0, M, N) -> N;
loopC(I, M, N) -> loopC(I - 1, M, loopD(M, M, N)).

loopD(0, M, N) -> N;
loopD(I, M, N) -> loopD(I - 1, M, loopE(M, M, N)).

loopE(0, M, N) -> N;
loopE(I, M, N) -> loopE(I - 1, M, loopF(M, N)).

loopF(0, N) -> N;
loopF(I, N) -> loopF(I - 1, 1 + N).
