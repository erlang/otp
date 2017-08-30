%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples that test pattern matching against terms of
%%% various types.
%%%-------------------------------------------------------------------
-module(basic_pattern_match).

-export([test/0]).

test() ->
  ok = test_hello_world(),
  ok = test_list_plus_plus_match(),
  ok.

%%--------------------------------------------------------------------
%% Trivial test to test pattern matching compilation with atoms, the
%% correct handling of all sorts of alphanumeric types in Erlang, and
%% conversions between them.

test_hello_world() ->
  String = gimme(string),
  String = atom_to_list(gimme(atom)),
  String = binary_to_list(gimme(binary)),
  true = (list_to_atom(String) =:= gimme(atom)),
  true = (list_to_binary(String) =:= gimme(binary)),
  ok.

gimme(string) ->
  "hello world";
gimme(atom) ->
  'hello world';
gimme(binary) ->
  <<"hello world">>.

%%--------------------------------------------------------------------
%% Makes sure that pattern matching expressions involving ++ work OK.
%% The third expression caused a problem in the Erlang shell of R11B-5.
%% It worked OK in both interpreted and compiled code.

test_list_plus_plus_match() ->
  ok = (fun("X" ++ _) -> ok end)("X"),
  ok = (fun([$X | _]) -> ok end)("X"),
  ok = (fun([$X] ++ _) -> ok end)("X"),
  ok.
