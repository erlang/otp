%   The Computer Language Shootout
%   http://shootout.alioth.debian.org/
%
%   contributed by Hans Svensson

-module(fannkuch).
-export([main/1]).

-export([small/0,medium/0,big/0]).

small() -> 6.
medium() -> 10.
big() -> 14.

main(Arg) ->
    N = Arg,
    F = run(N),
    io:fwrite("Pfannkuchen(~p) = ~p~n", [N, F]),
    erlang:exit(ok).

run(N) when N > 0 ->
    L = lists:seq(1,N),
    put(pr,30),
    put(maxflip,0),
    cP(1,N,L,true),
    get(maxflip).

cP(1,N,L,_) ->
    test(L),
    cP(2,N,L,true);
cP(B,N,_,_) when B > N ->
    ok;
cP(B,N,L,Cont) ->
    Is = lists:seq(1,B-1),
    Ls = lists:map(fun(D) -> rotate(D,B,L) end,Is),
    case length(Ls) of
	1 ->
	    test(hd(Ls));
	_ ->
	    lists:map(fun(L_) -> test(L_),
				 lists:map(fun(D) -> cP(D,N,L_,false) end,
					   lists:seq(2,B-1))
		      end,Ls)
    end,
    case Cont of
	true ->
	    cP(B+1,N,L,true);
	false ->
	    ok
    end.

rotate(1,2,[H|[H2|T]]) ->
    [H2 | [ H | T]];
rotate(1,J,[H|T]) ->
    {H2,T2} = lists:split(J-1,T),
    H2 ++ [H] ++ T2;
rotate(I,J,L) ->
    {H,T} = lists:split(J,L),
    {H1,H2} = lists:split(I,H),
    H2 ++ H1 ++ T.
    
test(L) ->
    case get(pr) of 
	0 -> ok;
	N -> lists:map(fun(I) -> io:format("~p",[I]) end, L),
	     io:format("\n"),
	     put(pr,N-1)
    end,
    X = flip(L,0),
    Y = get(maxflip),
    case X > Y of
	true -> put(maxflip,X);
	false -> ok
    end.

flip([1|_], N) -> N;
flip([I|_]=L, N) ->
    {H, T} = lists:split(I, L),
    flip(lists:reverse(H)++T, N+1).
