%%% The Computer Language Benchmarks Game
%%% http://shootout.alioth.debian.org/
%%% Contributed by Jiri Isa
%%% optimized run time options by shun shino

-module(threadring).
-export([main/1, roundtrip/2]).
-export([small/0, medium/0, big/0]).

small() -> 500000.
medium() -> 50000000. % <-- default (18.40 sec)
big() -> 100000000. % untested.

-define(RING, 503).

start(Token) ->
   H = lists:foldl(
      fun(Id, Pid) -> spawn(threadring, roundtrip, [Id, Pid]) end, 
      self(), 
      lists:seq(?RING, 2, -1)),
   H ! Token,
   roundtrip(1, H).

roundtrip(Id, Pid) ->
   receive
      suicide ->
         Pid ! suicide;
      1 ->
         io:fwrite("~b~n", [Id]),
         Pid ! suicide;
      Token ->
         Pid ! Token - 1,
         roundtrip(Id, Pid)
   end.

main(Arg) ->
   Token = Arg,
   case Token of
     X when X > 1 -> start(Token);
     _ -> ok
   end,
   erlang:exit(0).
