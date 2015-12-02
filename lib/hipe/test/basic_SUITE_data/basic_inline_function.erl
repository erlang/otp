%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that depend on the compiler inliner being turned on.
%%%-------------------------------------------------------------------
-module(basic_inline_function).

-export([test/0]).

-compile({inline, [{to_objects, 3}]}).

test() ->
  ok = test_inline_match(),
  ok.

%%--------------------------------------------------------------------

test_inline_match() ->
  bad_object = test1(a, {binary, foo, set}, c),
  bad_object = test2(a, {binary, foo, set}, c),
  bad_object = test3(a, {binary, foo, set}, c),
  ok.

%% Inlined
test1(KeysObjs, C, Ts) ->
  case catch to_objects(KeysObjs, C, Ts) of
    {'EXIT', _} ->
      bad_object;
    ok ->
      ok
  end.

%% "Inlined" by hand
test2(KeysObjs, C, _Ts) ->
  case catch (case C of
		{binary, _, set} ->
		  <<_ObjSz0:32, _T/binary>> = KeysObjs;
		_ -> ok
	      end) of
    {'EXIT', _} ->
      bad_object;
    ok ->
      ok
  end.

%% Not inlined
test3(KeysObjs, C, Ts) ->
  case catch fto_objects(KeysObjs, C, Ts) of
    {'EXIT', _} ->
      bad_object;
    ok ->
      ok
  end.

%% Inlined.
to_objects(Bin, {binary, _, set}, _Ts) ->
  <<_ObjSz0:32, _T/binary>> = Bin,
  ok;
to_objects(<<_ObjSz0:32, _T/binary>> ,_, _) ->
  ok;
to_objects(_Bin, _, _Ts) ->
  ok.

%% Not Inlined.
fto_objects(Bin, {binary, _, set}, _Ts) ->
  <<_ObjSz0:32, _T/binary>> = Bin,
  ok;
fto_objects(<<_ObjSz0:32, _T/binary>> ,_,_) ->
  ok;
fto_objects(_Bin, _, _Ts) ->
  ok.

