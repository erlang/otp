%%% The Computer Language Shootout
%%% http://shootout.alioth.debian.org/
%%%
%%% Contributed by Isaac Gouy

-module(harmonic).
-export([main/1]).
-export([small/0,medium/0,big/0]).

small() -> 1600000.
medium() -> 160000000.
big() -> 200000000000.


main(Arg) ->
   Num = Arg,
   io:fwrite("~.9f~n", [harmonic(Num,0.0)]),
   erlang:exit(ok).

harmonic(0,PartialSum) -> PartialSum;
harmonic(I,PartialSum) -> harmonic(I-1,PartialSum+(1.0/I)).


 
