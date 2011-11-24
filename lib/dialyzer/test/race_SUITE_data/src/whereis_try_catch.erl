% This tests that warnings do appear when a whereis/register combination
% is handled by try/catch.

-module(whereis_try_catch).
-export([race/1, no_race/1]).

race(Pid) ->
  case whereis(master) of
    undefined ->
      try
	io:format("exception", [])
      catch
        _ -> register(master, Pid)
      end
  end.

no_race(Pid) ->
  case whereis(master) of
    undefined ->
      try
        register(master, Pid)
      catch
        _ -> io:format("exception", [])
      end
  end.
