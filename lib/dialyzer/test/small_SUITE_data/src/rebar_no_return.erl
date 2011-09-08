-module(rebar_no_return).

-export([t/0]).

-spec t() -> no_return().
t() ->
  F = log_and_halt("baz"),
  F("foo", 123).

-spec log_and_halt(string()) -> fun((string(),integer()) -> no_return()).
log_and_halt(Msg) ->
  fun(_, _) ->
      abort(Msg)
  end.

-spec abort(string()) -> no_return().
abort(Msg) ->
  io:format("~s~n", [Msg]),
  halt(1).
