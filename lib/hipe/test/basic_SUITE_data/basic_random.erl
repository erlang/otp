%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% A test for list handling created using the 'random' module.
%%%-------------------------------------------------------------------
-module(basic_random).

-export([test/0]).

%% It can be used as a benchmark by playing with the following defines
-define(N, 1000).
-define(Iter, 500).

test() ->
  ok = random(?N).

random(N) ->
  random(N, ?Iter).

random(N, Iter) ->
  random:seed(1, 2, 3),
  t(ranlist(N, [], N*100), Iter).

ranlist(0, L, _N) -> L;
ranlist(N, L, N0) -> ranlist(N-1, [random:uniform(N0)+300 | L], N0).

t(_, 0) -> ok;
t(L, Iter) ->
  %% io:format("Sort starting~n"),
  sort(L),
  t(L, Iter-1).

sort([X, Y | L]) when X =< Y ->
  split_1(X, Y, L, [], []);
sort([X, Y | L]) ->
  split_2(X, Y, L, [], []);
sort(L) ->
  L.

%% Ascending.
split_1(X, Y, [Z | L], R, Rs) when Z >= Y ->
  split_1(Y, Z, L, [X | R], Rs);
split_1(X, Y, [Z | L], R, Rs) when Z >= X ->
  split_1(Z, Y, L, [X | R], Rs);
split_1(X, Y, [Z | L], [], Rs) ->
  split_1(X, Y, L, [Z], Rs);
split_1(X, Y, [Z | L], R, Rs) ->
  split_1_1(X, Y, L, R, Rs, Z);
split_1(X, Y, [], R, Rs) ->
  rmergel([[Y, X | R] | Rs], []).

%% One out-of-order element, S.
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= Y ->
  split_1_1(Y, Z, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= X ->
  split_1_1(Z, Y, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when S =< Z ->
  split_1(S, Z, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [Z | L], R, Rs, S) ->
  split_1(Z, S, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [], R, Rs, S) ->
  rmergel([[S], [Y, X | R] | Rs], []).

%% Descending.
split_2(X, Y, [Z | L], R, Rs) when Z =< Y ->
  split_2(Y, Z, L, [X | R], Rs);
split_2(X, Y, [Z | L], R, Rs) when Z =< X ->
  split_2(Z, Y, L, [X | R], Rs);
split_2(X, Y, [Z | L], [], Rs) ->
  split_2(X, Y, L, [Z], Rs);
split_2(X, Y, [Z | L], R, Rs) ->
  split_2_1(X, Y, L, R, Rs, Z);
split_2(X, Y, [], R, Rs) ->
  mergel([[Y, X | R] | Rs], []).

split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< Y ->
  split_2_1(Y, Z, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< X ->
  split_2_1(Z, Y, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when S > Z ->
  split_2(S, Z, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [Z | L], R, Rs, S) ->
  split_2(Z, S, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [], R, Rs, S) ->
  mergel([[S], [Y, X | R] | Rs], []).

mergel([[] | L], Acc) ->
  mergel(L, Acc);
mergel([A, [H2 | T2], [H3 | T3] | L], Acc) ->
  mergel(L, [merge3_1(A, [], H2, T2, H3,  T3) | Acc]);
mergel([A, [H | T]], Acc) ->
  rmergel([merge2_1(A, H, T, []) | Acc], []);
mergel([L], []) ->
  L;
mergel([L], Acc) ->
  rmergel([lists:reverse(L, []) | Acc], []);
mergel([], []) ->
  [];
mergel([], Acc) ->
  rmergel(Acc, []);
mergel([A, [] | L], Acc) ->
  mergel([A | L], Acc);
mergel([A, B, [] | L], Acc) ->
  mergel([A, B | L], Acc).

rmergel([A, [H2 | T2], [H3 | T3] | L], Acc) ->
  rmergel(L, [rmerge3_1(A, [], H2, T2, H3, T3) | Acc]);
rmergel([A, [H | T]], Acc) ->
  mergel([rmerge2_1(A, H, T, []) | Acc], []);
rmergel([L], Acc) ->
  mergel([lists:reverse(L, []) | Acc], []);
rmergel([], Acc) ->
  mergel(Acc, []).

%% Take L1 apart.
merge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
  merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_1([H1 | T1], M, H2, T2, H3, T3) ->
  merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_1(_nil, M, H2, T2, H3, T3) when H2 =< H3 ->
  merge2_1(T2, H3, T3, [H2 | M]);
merge3_1(_nil, M, H2, T2, H3, T3) ->
  merge2_1(T3, H2, T2, [H3 | M]).

%% Take L2 apart.
merge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
  merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
  merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, _nil, H3, T3) when H1 =< H3 ->
  merge2_1(T1, H3, T3, [H1 | M]);
merge3_2(T1, H1, M, _nil, H3, T3) ->
  merge2_1(T3, H1, T1, [H3 | M]).

%% H1 <= H2. Inlined.
merge3_12(T1, H1, H2, T2, H3, T3, M) when H3 < H1 ->
  merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12(T1, H1, H2, T2, H3, T3, M) ->
  merge3_1(T1, [H1 | M], H2, T2, H3, T3).

%% H1 <= H2, take L3 apart.
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H1 ->
  merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
  merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12_3(T1, H1, H2, T2, M, _nil) ->
  merge2_1(T1, H2, T2, [H1 | M]).

%% H1 > H2. Inlined.
merge3_21(T1, H1, H2, T2, H3, T3, M) when H3 < H2 ->
  merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21(T1, H1, H2, T2, H3, T3, M) ->
  merge3_2(T1, H1, [H2 | M], T2, H3, T3).

%% H1 > H2, take L3 apart.
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H2 ->
  merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
  merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21_3(T1, H1, H2, T2, M, _nil) ->
  merge2_1(T2, H1, T1, [H2 | M]).

%% Take L1 apart.
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 > H2 ->
  rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
  rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_1(_nil, M, H2, T2, H3, T3) when H2 > H3 ->
  rmerge2_1(T2, H3, T3, [H2 | M]);
rmerge3_1(_nil, M, H2, T2, H3, T3) ->
  rmerge2_1(T3, H2, T2, [H3 | M]).

%% Take L2 apart.
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 > H2 ->
  rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
  rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, _nil, H3, T3) when H1 > H3 ->
  rmerge2_1(T1, H3, T3, [H1 | M]);
rmerge3_2(T1, H1, M, _nil, H3, T3) ->
  rmerge2_1(T3, H1, T1, [H3 | M]).

%% H1 > H2. Inlined.
rmerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 >= H1 ->
  rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12(T1, H1, H2, T2, H3, T3, M) ->
  rmerge3_1(T1, [H1 | M], H2, T2, H3, T3).

%% H1 > H2, take L3 apart.
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 >= H1 ->
  rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
  rmerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rmerge3_12_3(T1, H1, H2, T2, M, _nil) ->
  rmerge2_1(T1, H2, T2, [H1 | M]).

%% H1 =< H2. Inlined.
rmerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 >= H2 ->
  rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21(T1, H1, H2, T2, H3, T3, M) ->
  rmerge3_2(T1, H1, [H2 | M], T2, H3, T3).

%% H1 =< H2, take L3 apart.
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 >= H2 ->
  rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
  rmerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rmerge3_21_3(T1, H1, H2, T2, M, _nil) ->
  rmerge2_1(T2, H1, T1, [H2 | M]).

merge2_1([H1 | T1], H2, T2, M) when H2 < H1 ->
  merge2_2(T1, H1, T2, [H2 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
  merge2_1(T1, H2, T2, [H1 | M]);
merge2_1(_nil, H2, T2, M) ->
  lists:reverse(T2, [H2 | M]).

merge2_2(T1, H1, [H2 | T2], M) when H1 < H2 ->
  merge2_1(T1, H2, T2, [H1 | M]);
merge2_2(T1, H1, [H2 | T2], M) ->
  merge2_2(T1, H1, T2, [H2 | M]);
merge2_2(T1, H1, _nil, M) ->
  lists:reverse(T1, [H1 | M]).

rmerge2_1([H1 | T1], H2, T2, M) when H2 >= H1 ->
  rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_1([H1 | T1], H2, T2, M) ->
  rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1(_nil, H2, T2, M) ->
  lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, H1, [H2 | T2], M) when H1 >= H2 ->
  rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_2(T1, H1, [H2 | T2], M) ->
  rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_2(T1, H1, _nil, M) ->
  lists:reverse(T1, [H1 | M]).
