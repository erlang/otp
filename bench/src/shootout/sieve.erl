%% The Great Computer Language Shootout 
%% http://shootout.alioth.debian.org/

%% Based on code by Maurice Castro.
%% Originally tweaked by Doug Bagley.
%% Further tweaked and shrunk by James Hague.
%% modified by Isaac Gouy

-module(sieve).
-export([main/0, main/1, test/2]).
-export([small/0,medium/0,big/0]).

small() -> 1600.
medium() -> 1600.
big() -> 10000.

% The sieve loop is spawned into its own process with a heap size of
% 50,000 words.  With all of the list creation done by this benchmark,
% much time is spent resizing the initially tiny heap.  Starting with
% a larger heap improves execution time by ~20%.

main() -> main(["1"]).
main(Arg) ->
   Num = Arg,
   spawn_opt(sieve, test, [self(), Num], [{min_heap_size, 50000}]),
   receive Num_primes -> io:fwrite("Count: ~w\n", [Num_primes]) end,
   exit(ok).

test(From, N) -> test(From, N, math:sqrt(8192), lists:seq(2, 8192)).

test(From, N, Max, Seq) ->
   Num_primes = length(era(Max, Seq)),
   if
      N > 1 -> test(From, N-1, Max, Seq);
      true  -> From ! Num_primes
   end.

era(Max, [H|T]) when H =< Max ->
   [H | era(Max, [X || X <- T, X rem H =/= 0])];
era(Max, L) ->  L.

%%% Eratosthenes algorithm from Maurice Castro, with permission, 
%%% from his book, _Erlang in Real Time_, ISBN: 0864447434
%%% http://www.serc.rmit.edu.au/~maurice/erlbk/eg/choice/erasto.erl
