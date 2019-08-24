%%---------------------------------------------------------------------
%% A test for which the analysis gave a bogus warning due to
%% considering the function call name to be of opaque type...
%%---------------------------------------------------------------------

-module(opaque_bug2).

-export([test/0]).

-opaque o() :: 'map'.

test() ->
  lists:map(fun(X) -> X+1 end, [1,2]).
