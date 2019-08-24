%% This tests the presence of possible races due to a whereis/register
%% combination in higher order functions and inter-module calls.

-module(whereis_param_inter_module1).
-export([start/2]).

start(AnAtom, Fun) ->
  register(AnAtom, whereis_param_inter_module2:continue(AnAtom, Fun)).
