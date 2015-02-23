%% The Great Computer Language Shootout
%% http://shootout.alioth.debian.org/

%% contributed by Alexey Shchepin <alexey@sevcom.net>
%% modified by Isaac Gouy

-module(matrix).
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 10.
medium() -> 14.
big() -> 20.

-define(SIZE, 300).

main() -> main(["1"]).
main(Arg) ->
    N = Arg,
    M1 = mkmatrix(?SIZE, ?SIZE),
    M2 = mkmatrix(?SIZE, ?SIZE),
    mmult_n(N, M1, M2),
    exit(ok).

mmult_n(1, M1, M2) ->
    M3 = mmult(M1, M2),
    io:format("~p ~p ~p ~p~n", [element(1, element(1, M3)),
			        element(4, element(3, M3)),
			        element(3, element(4, M3)),
			        element(5, element(5, M3))]);
mmult_n(N, M1, M2) ->
    mmult(M1, M2),
    mmult_n(N-1, M1, M2).

mkmatrix(Rows, Cols) ->
    list_to_tuple(
      lists:map(fun(Row) ->
			list_to_tuple(lists:seq(Row * Cols + 1,
						Row * Cols + Cols)) 
		end, lists:seq(0, Rows - 1))).


mmult(M1, M2) ->
    mmult1(?SIZE, M1, M2, []).

mmult1(0, M1, M2, M3) ->
    list_to_tuple(M3);
mmult1(Row, M1, M2, M3) ->
    M1R = element(Row, M1),
    mmult1(Row - 1, M1, M2,
	   [mmult2(?SIZE, Row, M1R, M2, []) | M3]).

mmult2(0, Row, M1R, M2, R) ->
    list_to_tuple(R);
mmult2(Col, Row, M1R, M2, R) ->
    mmult2(Col - 1, Row, M1R, M2,
	   [inner_loop(?SIZE, Col, Row, M1R, M2, 0) | R]).

inner_loop(0, Col, Row, M1R, M2, Sum) ->
    Sum;
inner_loop(I, Col, Row, M1R, M2, Sum) ->
    inner_loop(I - 1, Col, Row, M1R, M2,
	       Sum + element(I, M1R) * element(Col, element(I, M2))).



