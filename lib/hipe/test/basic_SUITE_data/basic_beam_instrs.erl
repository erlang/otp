%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Tests for correct translation of various BEAM instructions.
%%%-------------------------------------------------------------------
-module(basic_beam_instrs).

-export([test/0]).

test() ->
  ok = test_make_fun(),
  ok = test_switch_val(),
  ok = test_put_literal(),
  ok = test_set_tuple_element(),
  ok = test_unguarded_unsafe_element(),
  ok.

%%--------------------------------------------------------------------
%% Tests whether the translation of make_fun works.

test_make_fun() ->
  {F, G} = double_the_fun(),
  ok = F(),
  {ok, 42} = G(42),
  FV1 = {ok, free_var1},
  FV2 = {also, {free, var2}},
  {FV1, {ok, [bv]}, FV2} = contains_fun(FV1, ignored, FV2),
  ok.

double_the_fun() ->
  {fun () -> ok end, fun (V) -> {ok, V} end}.

contains_fun(X, _IGNORED_ARG, Y) ->
  calls_fun(fun(Term) -> {X, Term, Y} end).

calls_fun(F) ->
  F({ok, [bv]}).

%%--------------------------------------------------------------------
%% Tests whether the translation of switch_val works.

test_switch_val() ->
  'A' = sv(a),
  'B' = sv(b),
  'C' = sv(c),
  foo = sv(d),
  ok.

sv(a) -> 'A';
sv(b) -> 'B';
sv(c) -> 'C';
sv(_) -> foo.

%%--------------------------------------------------------------------
%% Tests correct handling of literals (statically constant terms)

-define(QUADRUPLE, {a,b,c,42}).
-define(DEEP_LIST, [42,[42,[42]]]).

test_put_literal() ->
  ?QUADRUPLE = mk_literal_quadruple(),
  ?DEEP_LIST = mk_literal_deep_list(),
  ok.

mk_literal_quadruple() ->
  ?QUADRUPLE.

mk_literal_deep_list() ->
  ?DEEP_LIST.

%%--------------------------------------------------------------------
%% Tests whether the translation of set_tuple_element works.

-record(rec, {f1, f2, f3, f4, f5}).

test_set_tuple_element() ->
  F2 = [a,b,c], F4 = {a,b},
  State0 = init_rec(F2, F4),
  State1 = simple_set(State0, 42),
  #rec{f1 = foo, f2 = F2, f3 = 42, f4 = F4, f5 = 42.0} = odd_set(State1, 21),
  ok.

init_rec(F2, F4) ->
  #rec{f1 = bar, f2 = F2, f3 = 10, f4 = F4, f5 = 3.14}.

simple_set(State, Val) ->            %% f3 = Val is the one used in set_element;
  State#rec{f3 = Val, f5 = Val*2}.   %% this checks the case of variable

odd_set(State, Val) ->               %% f3 = foo is the one used in set_element;
  State#rec{f1 = foo, f5 = Val*2.0}. %% this checks the case of constant

%%--------------------------------------------------------------------
%% Tests the handling of unguarded unsafe_element operations that BEAM
%% can sometimes construct on records (when it has enough context).

test_unguarded_unsafe_element() ->
  {badrecord, rec} = try unguarded_unsafe_element(42) catch error:E -> E end,
  ok.

unguarded_unsafe_element(X) ->
  X#rec{f1 = X#rec.f3}.
