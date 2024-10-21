-module(logical_processors).

-export([t0/0,t1/0,t2/0]).

t0() ->
      unknown = erlang:system_info(logical_processors),
      unknown = erlang:system_info(logical_processors_available),
      unknown = erlang:system_info(logical_processors_online),
      ok.

t1() ->
      ok = erlang:system_info(logical_processors_available).

t2() ->
      ok = erlang:system_info(logical_processors_online).

