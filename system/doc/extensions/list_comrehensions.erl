-module(zf).

-compile(export_all).


%% Odd numbers.

%%foo(L) -> [ X || X <- L, (X > X-1) == (X /= X-1) ].

boo() -> [X||X <- [1,2,a,3,4,b,5,6], X > 3].
boo1() -> [X||X <- [1,2,a,3,4,b,5,6], integer(X),X > 3].
boo2() -> [{X,Y} || X <- [1,2,3], Y <- [a,b]].

bar(L) -> [ X || X <- L, integer(X), gt(X, 3) ].

bar(L, M) -> [ Y || X <- L, integer(X), gt(X, 3),
		    Y <- M, float(Y), gt(X, Y)
		   ].

baz(L) -> [ X || X <- L, atom(X) ].

buz(L, Min) -> [ X || Min > 3, X <- L, X >= Min ].

gt(X, Y) when X > Y -> true;
gt(X, Y) -> false.


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

cp(A,B) ->
    [ {X,Y} ||
	X <- A,
        Y <- B 
    ].

%% Return all permutations of a list
perms([]) -> [[]];
perms(L)  -> [ [H|T] || H <- L, T <- perms(L--[H]) ].

%% Quick sort
sort([X|Xs]) ->
    sort([ Y || Y <- Xs, Y < X ]) ++
	[X] ++
	sort([ Y || Y <- Xs, Y >= X ]);
sort([]) -> [].

%% append

append(L)   ->  [X||L1<-L,X<-L1].

map(Fun, L) -> [Fun(X)||X<-L].

filter(Pred, L) -> [X||X<-L,Pred(X)].

select(X, L) ->  [Y || {X1,Y} <- L, X == X1].
