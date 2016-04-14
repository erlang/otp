%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Tests for correct translation of booleans and their primitives.
%%%-------------------------------------------------------------------
-module(basic_boolean).

-export([test/0]).

test() ->
  ok = test_boolean_ops(false, true),
  ok = test_orelse_redundant(),
  ok.

%%--------------------------------------------------------------------

test_boolean_ops(F, T) ->
  true  = T and T,
  false = T and F,
  false = F and T,
  false = F and F,
  true  = T or T,
  true  = T or F,
  true  = F or T,
  false = F or F,
  true  = T andalso T,
  false = T andalso F,
  false = F andalso T,
  false = F andalso F,
  true  = T orelse T,
  true  = T orelse F,
  true  = F orelse T,
  false = F orelse F,
  ok.

%%--------------------------------------------------------------------
%% Redundant test in BEAM code will generate type warning.

test_orelse_redundant() ->
  true = test_orelse(true, true, true),
  ok.

test_orelse(A, B, C) ->
  A andalso B orelse C.

%%--------------------------------------------------------------------
