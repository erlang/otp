% The Computer Language Shootout
% http://shootout.alioth.debian.org/
%
% contributed by Isaac Gouy (Erlang novice)

-module(binarytrees). 

-export([main/1]).
-export([small/0,medium/0,big/0]).

-define(Min,4).

%% Small, medium, big
small() -> 12.
medium() -> 20. % <-- default (39.84 sec)
big() -> 24.

main(Arg) ->
   N = Arg,
   Max = lists:max([?Min+2,N]),

   Stretch = Max + 1,
   io:fwrite("stretch tree of depth ~w\t check: ~w~n", 
      [ Stretch, itemCheck(bottomUp(0,Stretch)) ]),

   LongLivedTree = bottomUp(0,Max),
   depthLoop(?Min,Max),

   io:fwrite("long lived tree of depth ~w\t check: ~w~n", 
      [ Max, itemCheck(LongLivedTree) ]),

   exit(ok).


depthLoop(D,M) when D > M -> ok;
depthLoop(D,M) -> 
   N = 1 bsl (M-D + ?Min),
   io:fwrite("~w\t trees of depth ~w\t check: ~w~n", 
      [ 2*N, D, sumLoop(N,D,0) ]),
   depthLoop (D+2,M).

sumLoop(0,_,Sum) -> Sum;
sumLoop(N,D,Sum) -> 
   sumLoop(N-1,D, Sum + itemCheck(bottomUp(N,D)) + itemCheck(bottomUp(-1*N,D))).

bottomUp(I,0) -> {I, nil, nil};
bottomUp(I,D) -> {I, bottomUp(2*I-1,D-1), bottomUp(2*I,D-1)}.

itemCheck(nil) -> 0;
itemCheck({I,Left,Right}) -> 
   I + itemCheck(Left) - itemCheck(Right).
