%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bs_system_limit_32.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Purpose : Checks binary system limits on 32-bit machines
%%% Created : 14 May 2008
%%%-------------------------------------------------------------------
-module(bs_system_limit_32).

-export([test/0]).

test() ->
  case erlang:system_info(wordsize) of
    4 -> system_limit_32();
    8 -> ok
  end.

system_limit_32() ->
  {'EXIT', {badarg, _}} = (catch <<42:(id(-1))>>),
  {'EXIT', {badarg, _}} = (catch <<42:(id(-389739873536870912))/unit:8>>),
  {'EXIT', {system_limit, _}} = (catch <<32:536870912/unit:8>>),
  {'EXIT', {system_limit, _}} = (catch <<42:(id(536870912))/unit:8>>),
  {'EXIT', {system_limit, _}} = (catch <<42:(id(536870912))/unit:8,1:1>>),
  ok.

id(X) -> X.
