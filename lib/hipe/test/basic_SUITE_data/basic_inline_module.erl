%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that depend on the compiler inliner being turned on.
%%%-------------------------------------------------------------------
-module(basic_inline_module).

-export([test/0]).

-compile([inline]).     %% necessary for these tests

test() ->
  ok = test_case_end_atom(),
  ok.

%%--------------------------------------------------------------------
%% Tests whether the translation of a case_end instruction works even
%% when an exception (no matching case pattern) is to be raised.

test_case_end_atom() ->
  {'EXIT',{{case_clause,some_atom},_Trace}} = (catch test_case_stm_inlining()),
  ok.

test_case_stm_inlining() ->
  case some_atom() of
    another_atom -> strange_result
  end.

some_atom() ->
  some_atom.
