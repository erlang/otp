-module(bad_specs).
-export([f/1]).

%% There will be a warning unless specs are ignored.
-spec f(integer()) -> integer().
f(F) when is_float(F) ->
    F + 1.

