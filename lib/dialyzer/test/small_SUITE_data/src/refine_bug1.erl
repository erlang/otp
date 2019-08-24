-module(refine_bug1).
-export([f/1]).

f(gazonk = X) ->
  foo(X),        % this call is currently not considered when refining foo's
  throw(error);  % type since it appears in a clause that throws an exception
f(foo = X) ->
  foo(X).

foo(X) ->
  X.
