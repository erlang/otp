%%% The Computer Language Shootout
%%% http://shootout.alioth.debian.org/
%%% Contributed by Isaac Gouy (Erlang novice)

-module(partialsums).
-export([main/1]).
-import(math,[pow/2,sin/1,cos/1]).
-import(lists,[foreach/2,zipwith/3]).
-export([small/0,medium/0,big/0]).

small() -> 10000.
medium() -> 1000000.
big() -> 200000000.


main(Arg) ->
   N = Arg,
   Names = ["(2/3)^k", "k^-0.5", "1/k(k+1)", "Flint Hills", "Cookson Hills",
            "Harmonic", "Riemann Zeta", "Alternating Harmonic", "Gregory"],
   Sums = loop(1, N, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
   foreach( 
      fun(L) -> io:format("~.9f\t~s~n", L) end,
      zipwith(fun(X, Y) -> [X,Y] end, Sums, Names) ),
   erlang:exit(ok).


loop(K,N,Alt,A1,A2,A3,A4,A5,A6,A7,A8,A9) when K =< N ->
   SK = sin(K),
   CK = cos(K),
   K3 = pow(K,3),

   loop(K+1,N,-Alt,
      A1 + pow(2.0/3.0,K-1),
      A2 + pow(K,-0.5),
      A3 + (1.0/(K*(K+1))),
      A4 + (1.0/(K3*SK*SK)),
      A5 + (1.0/(K3*CK*CK)),
      A6 + (1.0/K),
      A7 + (1.0/pow(K,2)),
      A8 + (Alt/K),
      A9 + (Alt/(2*K-1)));

loop(K,N,Alt,A1,A2,A3,A4,A5,A6,A7,A8,A9) -> [A1,A2,A3,A4,A5,A6,A7,A8,A9].

