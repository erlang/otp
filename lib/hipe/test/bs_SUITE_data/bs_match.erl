%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bs_match.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Purpose : Performs simple matching and construction of binaries
%%%    TODO : Add binary and float tests
%%% Created : 20 Feb 2004
%%%-------------------------------------------------------------------
-module(bs_match).

-export([test/0]).

test() ->
  Funs = [fun test_aligned/0, fun test_unaligned/0,
	  fun test_zero_tail/0, fun test_integer_matching/0],
  lists:foreach(fun (F) -> ok = F() end, Funs).

%%-------------------------------------------------------------------
%% Test aligned accesses

test_aligned() ->
  10 = aligned_skip_bits_all(1, <<10,11,12>>),
  ok = aligned().

aligned_skip_bits_all(N, Bin) ->
  <<X:N/integer-unit:8, _/binary>> = Bin,
  X.

aligned() ->
  Tail1 = mkbin([]),
  {258, Tail1} = al_get_tail_used(mkbin([1,2])),
  Tail2 = mkbin(lists:seq(1, 127)),
  {35091, Tail2} = al_get_tail_used(mkbin([137,19|Tail2])),
  64896 = al_get_tail_unused(mkbin([253,128])),
  64895 = al_get_tail_unused(mkbin([253,127|lists:seq(42, 255)])),
  Tail3 = mkbin(lists:seq(0, 19)),
  {0, Tail1} = get_dyn_tail_used(Tail1, 0),
  {0, Tail3} = get_dyn_tail_used(mkbin([Tail3]), 0),
  {73, Tail3} = get_dyn_tail_used(mkbin([73|Tail3]), 8),
  0 = get_dyn_tail_unused(mkbin([]), 0),
  233 = get_dyn_tail_unused(mkbin([233]), 8),
  23 = get_dyn_tail_unused(mkbin([23,22,2]), 8),
  ok.

mkbin(L) when is_list(L) -> list_to_binary(L).

al_get_tail_used(<<A:16,T/binary>>) -> {A, T}.

al_get_tail_unused(<<A:16,_/binary>>) -> A.

%%-------------------------------------------------------------------
%% Test unaligned accesses

test_unaligned() ->
  10 = unaligned_skip_bits_all(8, <<10,11,12>>),
  ok = unaligned().

unaligned_skip_bits_all(N, Bin) ->
  <<X:N, _/binary>> = Bin,
  X.

unaligned() ->
  {'EXIT', {function_clause,_}} = (catch get_tail_used(mkbin([42]))),
  {'EXIT', {{badmatch,_},_}} = (catch get_dyn_tail_used(mkbin([137]), 3)),
  {'EXIT', {function_clause,_}} = (catch get_tail_unused(mkbin([42,33]))),
  {'EXIT', {{badmatch,_},_}} = (catch get_dyn_tail_unused(mkbin([44]), 7)),
  ok.

get_tail_used(<<A:1, T/binary>>) -> {A, T}.

get_tail_unused(<<A:15, _/binary>>) -> A.

get_dyn_tail_used(Bin, Sz) ->
  <<A:Sz, T/binary>> = Bin,
  {A,T}.

get_dyn_tail_unused(Bin, Sz) ->
  <<A:Sz, _T/binary>> = Bin,
  A.

%%-------------------------------------------------------------------
%% Test zero tail

test_zero_tail() ->
  42 = zt8(mkbin([42])),
  {'EXIT', {function_clause, _}} = (catch zt8(mkbin([1,2]))),
  {'EXIT', {function_clause, _}} = (catch zt44(mkbin([1,2]))),
  ok.

zt8(<<A:8>>) -> A.

zt44(<<_:4,_:4>>) -> ok.

%%-------------------------------------------------------------------
%% Test integer matching

test_integer_matching() ->
  ok = test_static_integer_matching_1(),
  ok = test_static_integer_matching_2(),
  ok = test_static_integer_matching_3(),
  ok = test_static_integer_matching_4(),
  DynFun = fun (N) -> ok = test_dynamic_integer_matching(N) end,
  lists:foreach(DynFun, [28, 27, 9, 17, 25, 8, 16, 24, 32]).

test_static_integer_matching_1() ->
  <<0:6, -25:28/integer-signed, 0:6>> = s11(),
  <<0:6, -25:28/integer-little-signed, 0:6>> = s12(),
  <<0:6, 25:28/integer-little, 0:6>> = s13(),
  <<0:6, 25:28, 0:6>> = s14(),
  ok.

s11() ->
  <<0:6, -25:28/integer-signed, 0:6>>.
s12() ->
  <<0:6, -25:28/integer-little-signed, 0:6>>.
s13() ->
  <<0:6, 25:28/integer-little, 0:6>>.
s14() ->
  <<0:6, 25:28, 0:6>>.

test_static_integer_matching_2() ->
  <<0:6, -25:20/integer-signed, 0:6>> = s21(),
  <<0:6, -25:20/integer-little-signed, 0:6>> = s22(),
  <<0:6, 25:20/integer-little, 0:6>> = s23(),
  <<0:6, 25:20, 0:6>> = s24(),
  ok.

s21() ->
  <<0:6, -25:20/integer-signed, 0:6>>.
s22() ->
  <<0:6, -25:20/integer-little-signed, 0:6>>.
s23() ->
  <<0:6, 25:20/integer-little, 0:6>>.
s24() ->
  <<0:6, 25:20, 0:6>>.

test_static_integer_matching_3() ->
  <<0:6, -25:12/integer-signed, 0:6>> = s31(),
  <<0:6, -25:12/integer-little-signed, 0:6>> = s32(),
  <<0:6, 25:12/integer-little, 0:6>> = s33(),
  <<0:6, 25:12, 0:6>> = s34(),
  ok.

s31() ->
  <<0:6, -25:12/integer-signed, 0:6>>.
s32() ->
  <<0:6, -25:12/integer-little-signed, 0:6>>.
s33() ->
  <<0:6, 25:12/integer-little, 0:6>>.
s34() ->
  <<0:6, 25:12, 0:6>>.

test_static_integer_matching_4() ->
  <<0:6, -3:4/integer-signed, 0:6>> = s41(),
  <<0:6, -3:4/integer-little-signed, 0:6>> = s42(),
  <<0:6, 7:4/integer-little, 0:6>> = s43(),
  <<0:6, 7:4, 0:6>> = s44(),
  ok.

s41() ->
  <<0:6, -3:4/integer-signed, 0:6>>.
s42() ->
  <<0:6, -3:4/integer-little-signed, 0:6>>.
s43() ->
  <<0:6, 7:4/integer-little, 0:6>>.
s44() ->
  <<0:6, 7:4, 0:6>>.

test_dynamic_integer_matching(N) ->
  S = 32 - N,
  <<-12:N/integer-signed, 0:S>> = <<-12:N/integer-signed, 0:S>>,
  <<-12:N/integer-little-signed, 0:S>> = <<-12:N/integer-little-signed, 0:S>>,
  <<12:N/integer, 0:S>> = <<12:N/integer, 0:S>>,
  <<12:N/integer-little, 0:S>> = <<12:N/integer-little, 0:S>>,
  ok.
