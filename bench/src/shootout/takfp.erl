%  The Great Computer Language Shootout
%   http://shootout.alioth.debian.org/ 
%  
%   contributed by Mark Scandariato
%
%   erl -noshell -noinput -run takfp main 7

-module(takfp).
-export([main/1]).
-export([small/0,medium/0,big/0]).

small() -> 8.
medium() -> 10.
big() -> 12.

main(Arg) ->
    N = Arg,
    io:fwrite("~.1f~n", [run(N)]),
    erlang:exit(ok).

run(N) when N >= 0 -> tak(N*3.0, N*2.0, N*1.0).

tak(X, Y, Z) when Y >= X -> Z;
tak(X, Y, Z) -> tak(tak(X-1.0,Y,Z), tak(Y-1.0,Z,X), tak(Z-1.0,X,Y)).

%  vim: ts=4 ft=erlang
