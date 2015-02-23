%%% The Great Computer Language Shootout 
%%% http://shootout.alioth.debian.org/
%%%
%%% Use ETS tables (Erlang's associative store).
%%%
%%% Optimizations provided by Einar Karttunen.
%%% An off-by-one error corrected by Brent Fulgham
%%% modified by Isaac Gouy

-module(hash).
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 160000.
medium() -> 1000000.
big() -> 10000000.

main() -> main(["1"]).
main(Arg) ->
   N = Arg,
   H = ets:new(i_am_a_carrot, [set]),
   doinserts(0, N + 1, H),
   Count = dolookups(N + 1, 0, H),
   io:format("~w~n", [Count]),
   exit(ok).

doinserts(N, N, H) -> ok;
doinserts(I, N, H) ->
   ets:insert(H, { erlang:integer_to_list(I, 16), I }),
   doinserts(I+1, N, H).

dolookups(0, C, H) -> C;
dolookups(I, C, H) ->
   Nx = integer_to_list(I),
   case ets:lookup(H, Nx) of
      [] ->    dolookups(I-1, C, H);
      Found -> dolookups(I-1, C+1, H)
   end.
