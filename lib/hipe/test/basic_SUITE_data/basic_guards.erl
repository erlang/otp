%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests for correct handling of guards and guard BIFs.
%%%-------------------------------------------------------------------
-module(basic_guards).

-export([test/0]).

test() ->
  ok = guard0(4.2),
  ok = guard1([foo]),
  ok = test_guard2(),
  ok = test_guard3(),
  ok = test_guard4(),
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
