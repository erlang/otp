-module(my_filter).
-export([test/0]).

test() ->
  filter(fun mystery/1, [1,2,3,4]).

filter(Pred, List) when is_function(Pred, 1) ->
  [ E || E <- List, Pred(E) ].

mystery(X) ->
  case (X rem 3) of
    0 -> true;
    1 -> false;
    2 -> gazonk
  end.

%% mystery(_X,_Y) -> true.
