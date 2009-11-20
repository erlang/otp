-module(t).
-author('tobbe@erix.ericsson.se').

%%-export([test/2]).
-compile(export_all).

%% Odd numbers.

%%foo(L) -> [ X || X <- L, (X > X-1) == (X /= X-1) ].

bar(L) -> [ X || X <- L, integer(X), gt(X, 3) ].

bar(L, M) -> [ Y || X <- L, integer(X), gt(X, 3),
		    Y <- M, float(Y), gt(X, Y)
		   ].

baz(L) -> [ X || X <- L, atom(X) ].

buz(L, Min) -> [ X || Min > 3, X <- L, X >= Min ].

gt(X, Y) when X > Y -> true;
gt(X, Y) -> false.

%% Turn a list into a set.
make_set([]) -> [];
make_set([H|T]) ->
    [H|[ 
     Y || Y <- make_set(T),
     Y =/= H 
     ]].

%% Return the Pythagorean triangles with sides 
%% of total length less than N
pyth(N) ->
    [ {A,B,C} ||
	A <- lists:seq(1,N),
        B <- lists:seq(1,N),
        C <- lists:seq(1,N),
        A+B+C =< N,
        A*A+B*B == C*C 
    ].

%% Cut the search space a bit..
pyth2(N) ->
    [ {A,B,C} ||
	A <- lists:seq(1,N),
        B <- lists:seq(1,N-A+1),
        C <- lists:seq(1,N-A-B+2),
        A+B+C =< N,
        A*A+B*B == C*C ].

%% Return the Cartesian product
cp() ->
    [ {X,Y} ||
	X <- a(),
        Y <- b() 
    ].

cp(A,B) when list(A),list(B) ->
    [ {X,Y} ||
	X <- A,
        Y <- B 
    ].

%a() -> 1/0.
a() -> [a,b].
b() -> [1,2,3].

%% Return all permutations of a list
perms([]) -> [[]];
perms(L)  -> [ [H|T] || H <- L, T <- perms(L--[H]) ].

%% Quick sort
sort([X|Xs]) ->
    sort([ Y || Y <- Xs, Y < X ]) ++
	[X] ++
	sort([ Y || Y <- Xs, Y >= X ]);
sort([]) -> [].

%% Vector addition
vecAdd(Xs,Ys) ->
    [ X+Y || {X,Y} <- zip(Xs,Ys) ].

zip([X|Xs],[Y|Ys]) -> [{X,Y}|zip(Xs,Ys)];
zip([],[])         -> [].

qsort([X|Xs]) ->
    qsort(lt(X,Xs))
    ++ [X] ++
    qsort(ge(X,Xs));
qsort([]) -> [].

lt(X,[H|T]) when X>H -> [H|lt(X,T)];
lt(X,[_|T]) -> lt(X,T);
lt(_,[]) -> [].

ge(X,[H|T]) when X=<H -> [H|ge(X,T)];
ge(X,[_|T]) -> ge(X,T);
ge(_,[]) -> [].
    
test(1,N) -> statistics(runtime),test1(N),statistics(runtime);
test(2,N) -> statistics(runtime),test2(N),statistics(runtime);
test(3,N) -> statistics(runtime),test3(N),statistics(runtime).

test1(0) -> true;
test1(N) ->
    sort([21,12,45,1,3,87,55,77,11,20,6,99,91,13,14,15,66,62,69,71,67,82,83,84,87,86,85]),
    test1(N-1).

test2(0) -> true;
test2(N) ->
    qsort([21,12,45,1,3,87,55,77,11,20,6,99,91,13,14,15,66,62,69,71,67,82,83,84,87,86,85]),
    test2(N-1).

test3(0) -> true;
test3(N) ->
    lists:sort([21,12,45,1,3,87,55,77,11,20,6,99,91,13,14,15,66,62,69,71,67,82,83,84,87,86,85]),
    test3(N-1).
