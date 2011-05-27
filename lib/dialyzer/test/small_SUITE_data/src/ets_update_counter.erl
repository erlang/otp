-module(ets_update_counter).

-export([ti/2, tl/2, tn/2, tt/2, tu/3, tmix/3]).

ti(T, K) ->
  ets:update_counter(T, K, 42).

tl(T, K) ->
  ets:update_counter(T, K, [{2,1}, {3,2}]).

tn(T, K) ->
  ets:update_counter(T, K, []).

tt(T, K) ->
  ets:update_counter(T, K, {4,2}).

tu(T, K, Op) ->
  ets:update_counter(T, K, Op).

tmix(T, K, Choice) ->
  Op = get_op(Choice),
  ets:update_counter(T, K, Op).

get_op(i) -> 42;
get_op(t) -> {4,2}.
