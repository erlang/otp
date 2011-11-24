%% This tests the presence of possible races due to a whereis/register
%% combination in higher order functions and inter-module calls.

-module(whereis_param_inter_module2).
-export([continue/2]).

continue(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun);
    P when is_pid(P) ->
      P
  end.
