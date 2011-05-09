-module(areq).

-export([t/0]).

t() ->
  ar_comp(3.0, 3),
  ex_comp(3.0, 3).

ar_comp(X, Y) -> X == Y.

ex_comp(X, Y) -> X =:= Y.
