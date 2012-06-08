%% The Great Computer Language Shootout
%% http://shootout.alioth.debian.org/
%%
%% contributed by Isaac Gouy (Erlang novice)
%% 20040619: Nicolas Niclausse: use ets instead of tuples.
%% 20040721: Alexey Shchepin: use process dictionary instead of ets.
%% fixed by Isaac Gouy
%%
%% Quick and Dirty transliteration from the Mercury solution
%% with +1 adjustment for array indexes.
%% Mercury uses 0..N-1 and Erlang uses 1..N
%%
%% Usage: start from command line with
%%     erlc heapsort.erl
%%     erl -noinput -s heapsort main 10000

-module(heapsort).
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 100000.
medium() -> 700000.
big() -> 1000000.

random_heap(I, Seed, N) ->
   case I < N of
      true ->
         {NextSeed, R} = gen_random(Seed),
         up_heap(I, R),
         random_heap(I+1, NextSeed, N);
      false -> ok
   end.

up_heap(N, Y) ->
   HalfN = N div 2,
   X = get(HalfN+1), %%%% +1
   case 0 < N andalso X < Y of
      true ->
         put(N+1, X), %%%% +1
         up_heap(HalfN, Y);
      false ->
         put(N+1, Y) %%%% +1
   end.

heapsort(0) -> ok;
heapsort(N) ->
   remove_greatest(N),
   heapsort(N-1).

remove_greatest(N) ->
   X = get(0+1), %%%% +1
   Y = get(N+1), %%%% +1
   put(N+1, X), %%%% +1
   down_heap(0, N-1, Y).

down_heap(I, N, X) ->
    L = I + I + 1,
    R = L + 1,
    case N < L of
        true ->
            put(I+1, X); %%%% +1
        false ->
	    {J, Y} = if
			 R < N ->
			     RV = get(R+1),
			     LV = get(L+1),
			     if
				 RV > LV ->
				     {R, RV};
				 true ->
				     {L, LV}
			     end;
			 true ->
			     {L, get(L+1)}
		     end,
            case X > Y of
	        true -> put(I+1, X); %%%% +1
                false ->
		    put(I+1, Y), %%%% +1
		    down_heap(J, N, X)
            end
    end.

clear_ets_array(0) -> ok;
clear_ets_array(I) ->
   put(I, 0),
   clear_ets_array(I - 1).

gen_random(Seed) ->
   IM = 139968, IA = 3877, IC = 29573,
   S = ((Seed * IA) + IC) rem IM,
   {S, S/IM}.

main() -> main(["1"]).
main(Arg) ->
   N = Arg,
   clear_ets_array(N),
   random_heap(0, 42, N),
   heapsort(N-1),
   io:fwrite("~.10f~n", [get(N)]),
   exit(ok).
