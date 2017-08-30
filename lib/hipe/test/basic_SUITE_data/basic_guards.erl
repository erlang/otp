%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests for correct handling of guards and guard BIFs.
%%%-------------------------------------------------------------------
-module(basic_guards).

-export([test/0]).
%% Prevent the inlining of the following functions
-export([bad_arith/0, bad_tuple/0, is_strange_guard/0]).

test() ->
  ok = guard0(4.2),
  ok = guard1([foo]),
  ok = test_guard2(),
  ok = test_guard3(),
  ok = test_guard4(),
  ok = test_is_boolean(),
  ok = test_bad_guards(),
  ok.

%%--------------------------------------------------------------------

guard0(X) when X /= 0, is_float(X) ->
  ok.

guard1(X) when is_atom(X) orelse is_float(X) ->
  error1;
guard1(X) when is_reference(hd(X)) ->
  error2;
guard1(X) when is_integer(hd(X)) ->
  error3;
guard1(X) when hd(X) == foo ->
  ok.

%%--------------------------------------------------------------------

test_guard2() ->
  ok1 = guard2(true),
  not_boolean = guard2(42),
  ok2 = guard2(false),
  ok.

guard2(X) when X ->  % gets transformed to:  is_boolean(X), X =:= true
  ok1;
guard2(X) when X =:= false ->
  ok2;
guard2(_) ->
  not_boolean.

%%--------------------------------------------------------------------

-define(is_foo(X), (is_atom(X) or (is_tuple(X) and (element(1, X) =:= 'foo')))).

test_guard3() ->
  no  = f('foo'),
  yes = f({'foo', 42}),
  no  = f(42),
  ok.

f(X) when ?is_foo(X) -> yes;
f(_) -> no.

%%--------------------------------------------------------------------

-define(EXT_REF, <<131,114,0,3,100,0,19,114,101,102,95,116,101,115,116,95,98,117,103,64,103,111,114,98,97,103,2,0,0,0,125,0,0,0,0,0,0,0,0>>).

test_guard4() ->
  yes = is_ref(make_ref()),
  no  = is_ref(gazonk),
  yes = is_ref(an_external_ref(?EXT_REF)),
  ok.

is_ref(Ref) when is_reference(Ref) -> yes;
is_ref(_Ref) -> no.

an_external_ref(Bin) ->
  binary_to_term(Bin).

%%--------------------------------------------------------------------

test_is_boolean() ->
  ok = is_boolean_in_if(),
  ok = is_boolean_in_guard().

is_boolean_in_if() ->
  ok1 = tif(true),
  ok2 = tif(false),
  not_bool = tif(other),
  ok.

is_boolean_in_guard() ->
  ok = tg(true),
  ok = tg(false),
  not_bool = tg(other),
  ok.

tif(V) ->
  Yes = yes(),        %% just to prevent the optimizer removing this
  if
    %% the following line generates an is_boolean instruction
    V, Yes == yes ->
      %% while the following one does not (?!)
      %% Yes == yes, V ->
      ok1;
    not(not(not(V))) ->
      ok2;
    V ->
      ok3;
    true ->
      not_bool
  end.

tg(V) when is_boolean(V) ->
  ok;
tg(_) ->
  not_bool.

yes() -> yes.

%%--------------------------------------------------------------------
%% original test by Bjorn G

test_bad_guards() ->
  ok = bad_arith(),
  ok = bad_tuple(),
  ok = is_strange_guard(),
  ok.

bad_arith() ->
  13 = bad_arith1(1, 12),
  42 = bad_arith1(1, infinity),
  42 = bad_arith1(infinity, 1),
  42 = bad_arith2(infinity, 1),
  42 = bad_arith3(inf),
  42 = bad_arith4(infinity, 1),
  ok.

bad_arith1(T1, T2) when (T1 + T2) < 17 -> T1 + T2;
bad_arith1(_, _) -> 42.

bad_arith2(T1, T2) when (T1 * T2) < 17 -> T1 * T2;
bad_arith2(_, _) -> 42.

bad_arith3(T) when (bnot T) < 17 -> T;
bad_arith3(_) -> 42.

bad_arith4(T1, T2) when (T1 bsr T2) < 10 -> T1 bsr T2;
bad_arith4(_, _) -> 42.

bad_tuple() ->
  error = bad_tuple1(a),
  error = bad_tuple1({a, b}),
  x = bad_tuple1({x, b}),
  y = bad_tuple1({a, b, y}),
  ok.

bad_tuple1(T) when element(1, T) =:= x -> x;
bad_tuple1(T) when element(3, T) =:= y -> y;
bad_tuple1(_) -> error.

is_strange_guard() when is_tuple({1, bar, length([1, 2, 3, 4]), self()}) -> ok;
is_strange_guard() -> error.
