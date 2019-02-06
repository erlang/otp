%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that manipulate and pattern match against tuples.
%%%-------------------------------------------------------------------
-module(basic_tuples).

-export([test/0]).

test() ->
  Num = 4711,
  ok = test_match({}, {1}, {1,2}, {1,2,3}, {1,2,3,4}, {1,2,3,4,5},
		  {1,2,3,4,5,6}, {1,2,3,4,5,6,7}, {1,2,3,4,5,6,7,8}),
  ok = test_size({}, {a}, {{a},{b}}, {a,{b},c}),
  ok = test_element({}, {a}, {a,b}, Num),
  ok = test_setelement({}, {1}, {1,2}, 3, [1,2]),
  ok = test_tuple_to_list({}, {a}, {a,b}, {a,b,c}, {a,b,c,d}, Num),
  ok = test_list_to_tuple([], [a], [a,b], [a,b,c], [a,b,c,d], Num),
  ok = test_tuple_with_case(),
  ok = test_tuple_in_guard({a, b}, {a, b, c}),
  ok.

%%--------------------------------------------------------------------
%% Tests matching of tuples

test_match(T0, T1, T2, T3, T4, T5, T6, T7, T8) ->
  {} = T0,
  {1} = T1,
  {1, 2} = T2,
  {1, 2, 3} = T3,
  {1, 2, 3, 4} = T4,
  {1, 2, 3, 4, 5} = T5,
  {1, 2, 3, 4, 5, 6} = T6,
  T6 = {1, 2, 3, 4, 5, 6},
  T7 = {1, 2, 3, 4, 5, 6, 7},
  {1, 2, 3, 4, 5, 6, 7, 8} = T8,
  ok.

%%--------------------------------------------------------------------
%% Tests the size/1 and tuple_size/1 BIFs.

test_size(T0, T1, T2, T3) ->
  [0, 1, 2, 3] = [size(T) || T <- [T0, T1, T2, T3]],
  [0, 1, 2, 3] = [tuple_size(T) || T <- [T0, T1, T2, T3]],
  ok.

%%--------------------------------------------------------------------
%% Tests element/2.

test_element(T0, T1, T2, N) ->
  a = element(1, T1),
  a = element(1, T2),
  %% indirect calls to element/2
  List = lists:seq(1, N),
  Tuple = list_to_tuple(List),
  ok = get_elements(List, Tuple, 1),
  %% element/2 of larger tuple with omitted bounds test
  true = lists:all(fun(I) -> I * I =:= square(I) end, lists:seq(1, 20)),
  %% some cases that throw exceptions
  {'EXIT', _} = (catch my_element(0, T2)),
  {'EXIT', _} = (catch my_element(3, T2)),
  {'EXIT', _} = (catch my_element(1, T0)),
  {'EXIT', _} = (catch my_element(1, List)),
  {'EXIT', _} = (catch my_element(1, N)),
  {'EXIT', _} = (catch my_element(1.5, T2)),
  ok.

my_element(Pos, Term) ->
  element(Pos, Term).

get_elements([Element|Rest], Tuple, Pos) ->
  Element = element(Pos, Tuple),
  get_elements(Rest, Tuple, Pos + 1);
get_elements([], _Tuple, _Pos) ->
  ok.

squares() ->
  {1*1,   2*2,   3*3,   4*4,   5*5,   6*6,   7*7,   8*8,   9*9,   10*10,
   11*11, 12*12, 13*13, 14*14, 15*15, 16*16, 17*17, 18*18, 19*19, 20*20}.

square(N) when is_integer(N), N >= 1, N =< 20 ->
  %% The guard tests lets the range analysis conclude N to be an integer in the
  %% 1..20 range. 20-1=19 is bigger than ?SET_LIMIT in erl_types.erl, and will
  %% thus be represented by an ?int_range() rather than an ?int_set().
  %% Because of the range analysis, the bounds test of this element/2 call
  %% should be omitted.
  element(N, squares()).

%%--------------------------------------------------------------------
%% Tests set_element/3.

test_setelement(T0, T1, Pair, Three, L) ->
  {x} = setelement(1, T1, x),
  {x, 2} = setelement(1, Pair, x),
  {1, x} = setelement(2, Pair, x),
  %% indirect calls to setelement/3
  Tuple = list_to_tuple(lists:duplicate(2048, x)),
  NewTuple = set_all_elements(Tuple, 1),
  NewTuple = list_to_tuple(lists:seq(1+7, 2048+7)),
  %% the following cases were rewritten to use the Three
  %% variable in this weird way so as to silence the compiler
  {'EXIT', _} = (catch setelement(Three - Three, Pair, x)),
  {'EXIT', _} = (catch setelement(Three, Pair, x)),
  {'EXIT', _} = (catch setelement(Three div Three, T0, x)),
  {'EXIT', _} = (catch setelement(Three div Three, L, x)),
  {'EXIT', _} = (catch setelement(Three / 2, Pair, x)),
  ok.

set_all_elements(Tuple, Pos) when Pos =< tuple_size(Tuple) ->
  set_all_elements(setelement(Pos, Tuple, Pos+7), Pos+1);
set_all_elements(Tuple, Pos) when Pos > tuple_size(Tuple) ->
  Tuple.

%%--------------------------------------------------------------------
%% Tests tuple_to_list/1.

test_tuple_to_list(T0, T1, T2, T3, T4, Size) ->
  [] = tuple_to_list(T0),
  [a] = tuple_to_list(T1),
  [a, b] = tuple_to_list(T2),
  [a, b, c] = tuple_to_list(T3),
  [a, b, c, d] = tuple_to_list(T4),
  [a, b, c, d] = tuple_to_list(T4),
  %% test a big tuple
  List = lists:seq(1, Size),
  Tuple = list_to_tuple(List),
  Size = tuple_size(Tuple),
  List = tuple_to_list(Tuple),
  %% some cases that should result in errors
  {'EXIT', _} = (catch my_tuple_to_list(element(2, T3))),
  {'EXIT', _} = (catch my_tuple_to_list(Size)),
  ok.

my_tuple_to_list(X) ->
  tuple_to_list(X).

%%--------------------------------------------------------------------
%% Tests list_to_tuple/1.

test_list_to_tuple(L0, L1, L2, L3, L4, Size) ->
  {} = list_to_tuple(L0),
  {a} = list_to_tuple(L1),
  {a, b} = list_to_tuple(L2),
  {a, b, c} = list_to_tuple(L3),
  {a, b, c, d} = list_to_tuple(L4),
  {a, b, c, d, e} = list_to_tuple(L4++[e]),
  %% test list_to_tuple with a big list
  Tuple = list_to_tuple(lists:seq(1, Size)),
  Size = tuple_size(Tuple),
  %% some cases that should result in errors
  {'EXIT', _} = (catch my_list_to_tuple({a,b})),
  {'EXIT', _} = (catch my_list_to_tuple([hd(L1)|hd(L2)])),
  ok.

my_list_to_tuple(X) ->
  list_to_tuple(X).

%%--------------------------------------------------------------------
%% Tests that a case nested inside a tuple is ok.
%% (This was known to crash earlier versions of BEAM.)

test_tuple_with_case() ->
  {reply, true} = tuple_with_case(),
  ok.

tuple_with_case() ->
  %% The following comments apply to the BEAM compiler.
  void(),                       % Reset var count.
  {reply,                       % Compiler will choose {x,1} for tuple.
   case void() of               % Call will reset var count.
     {'EXIT', Reason} ->        % Case will return in {x,1} (first free),
       {error, Reason};         %  but the tuple will be build in {x,1},
     _ ->                       %  so case value is lost and a circular
       true                     %  data element is built.
   end}.

void() -> ok.

%%--------------------------------------------------------------------
%% Test to build a tuple in a guard.

test_tuple_in_guard(T2, T3) ->
  %% T2 = {a, b}; T3 = {a, b, c}
  ok = if T2 == {element(1, T3), element(2, T3)} -> ok;
	  true -> other
       end,
  ok = if T3 == {element(1, T3), element(2, T3), element(3, T3)} -> ok;
	  true -> other
       end,
  ok.
