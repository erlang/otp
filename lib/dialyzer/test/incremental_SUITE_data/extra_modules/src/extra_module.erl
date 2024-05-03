-module(extra_module).

-export([start/2,stop/1,f/1]).

start(StartType, StartArgs) ->
  error.

stop(State) ->
  error.

% Purposely broken to generate a warning if the module is loaded and analysed
-spec f(atom()) -> string().
f(N) when is_integer(N) ->
  [N + 1|3].
