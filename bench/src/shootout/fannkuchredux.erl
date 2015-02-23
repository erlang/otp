%% The Computer Language Benchmarks Game
%% http://shootout.alioth.debian.org/
%%
%% Contributed by : Alkis Gotovos and Maria Christakis, 13 Nov 2010

-module(fannkuchredux).

-export([main/1]).
-export([small/0,medium/0,big/0]).

small() -> 10.
medium() -> 12. % <-- default (6 min)
big() -> 14. % untested.

main(N) when N > 0 ->
    {MaxFlips, Checksum} = 
	case N of
	    1 -> {0, 0};
	    _Other ->
		Chunk = fact(N - 1),
		divide(0, N, lists:seq(1, N), Chunk),
		join(N, 0, 0)
	end,
    io:format("~p~nPfannkuchen(~p) = ~p~n", [Checksum, N, MaxFlips]),
    {MaxFlips, Checksum}.

divide(N, N, _L, _C) -> ok;
divide(N, MaxN, [H|T] = List, Chunk) ->
    Self = self(),
    Fun = fun() ->
	      work(N, List, N * Chunk, (N + 1) * Chunk, MaxN, 0, 0, Self)
	  end,
    spawn(Fun),
    divide(N + 1, MaxN, T ++ [H], Chunk).

join(0, MaxFlips, Checksum) -> {MaxFlips, Checksum};
join(N, MaxFlips, Checksum) ->
    receive
	{Flips, Sum} -> join(N - 1, max(MaxFlips, Flips), Checksum + Sum)
    end.

work(_P, _L, Index, Index, _R, MaxFlips, Checksum, Target) ->
    Target ! {MaxFlips, Checksum};
work(Proc, List, Index, MaxIndex, R, MaxFlips, Checksum, Target) ->
    reset(R),
    {Flips, Sum} = flip_sum(Index, List),
    NewFlips = max(Flips, MaxFlips),
    NewSum = Checksum + Sum,
    {NewList, NewR} = next(Proc, List, 1),
    work(Proc, NewList, Index + 1, MaxIndex, NewR, NewFlips, NewSum, Target).

next(Proc, List, R) ->
    NewList = next_aux(R, List),
    case put(R, get(R) - 1) of
	1 -> next(Proc, NewList, R + 1);
	_Other -> {NewList, R}
    end.

next_aux(1, [E1, E2|T]) -> [E2, E1|T];
next_aux(2, [E1, E2, E3|T]) -> [E2, E3, E1|T];
next_aux(3, [E1, E2, E3, E4|T]) -> [E2, E3, E4, E1|T];
next_aux(R, [H|T]) ->
    {Front, Back} = lists:split(R, T),
    Front ++ [H] ++ Back.    

flip_sum(Index, List) ->
    Flips = flip(List, 0),
    Sum = 
	case Index band 1 of
	    0 -> Flips;
	    1 -> -Flips
	end,
    {Flips, Sum}.

flip([1|_T], N) ->
    N;
flip([2, E1|T], N) ->
    flip([E1, 2|T], N + 1);
flip([3, E1, E2|T], N) ->
    flip([E2, E1, 3|T], N + 1);
flip([4, E1, E2, E3|T], N) ->
    flip([E3, E2, E1, 4|T], N + 1);
flip([5, E1, E2, E3, E4|T], N) ->
    flip([E4, E3, E2, E1, 5|T], N + 1);
flip([6, E1, E2, E3, E4, E5|T], N) ->
    flip([E5, E4, E3, E2, E1, 6|T], N + 1);
flip([7, E1, E2, E3, E4, E5, E6|T], N) ->
    flip([E6, E5, E4, E3, E2, E1, 7|T], N + 1);
flip([8, E1, E2, E3, E4, E5, E6, E7|T], N) ->
    flip([E7, E6, E5, E4, E3, E2, E1, 8|T], N + 1);
flip([9, E1, E2, E3, E4, E5, E6, E7, E8|T], N) ->
    flip([E8, E7, E6, E5, E4, E3, E2, E1, 9|T], N + 1);
flip([10, E1, E2, E3, E4, E5, E6, E7, E8, E9|T], N) ->
    flip([E9, E8, E7, E6, E5, E4, E3, E2, E1, 10|T], N + 1);
flip([11, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10|T], N) ->
    flip([E10, E9, E8, E7, E6, E5, E4, E3, E2, E1, 11|T], N + 1);
flip([12, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11|T], N) ->
    flip([E11, E10, E9, E8, E7, E6, E5, E4, E3, E2, E1, 12|T], N + 1);
flip([H|_T] = List, N) ->
    {First, Last} = lists:split(H, List),
    flip(lists:reverse(First) ++ Last, N + 1).

reset(1) -> ok;    
reset(N) -> put(N - 1, N), reset(N - 1).

fact(1) -> 1;
fact(N) -> N * fact(N - 1).
