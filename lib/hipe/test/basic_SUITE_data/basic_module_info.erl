%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%% Date: Oct 25, 2003
%%%
%%% Tests whether calling module_info from the same module works.
%%% This seems trivial, but the problem is that the module_info/[0,1]
%%% functions that the BEAM file contains used to be dummy functions
%%% containing crap. So, these functions could not be used for
%%% compilation to native code and the functions that the BEAM loader
%%% generates should have been used instead. This was a HiPE bug
%%% reported by Dan Wallin.
%%%-------------------------------------------------------------------
-module(basic_module_info).

-export([test/0]).

test() ->
  L = test_local_mi0_call(),
  E = test_remote_mi1_call(),
  {3, 3} = {L, E},
  ok.

test_local_mi0_call() ->
  ModInfo = module_info(),
  %% io:format("ok, ModInfo=~w\n", [ModInfo]),
  {exports, FunList} = lists:keyfind(exports, 1, ModInfo),
  length(FunList).

test_remote_mi1_call() ->
  FunList = ?MODULE:module_info(exports),
  length(FunList).
