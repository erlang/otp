-module(foo).

-export([f/2]).

f(X,Y) ->
    A=#{a=>#{1=>a,2=>b,3=>c},
        b=>#{1=>p,2=>q,3=>r},
        c=>#{1=>x,2=>y,3=>z}},
    A[X][Y].
