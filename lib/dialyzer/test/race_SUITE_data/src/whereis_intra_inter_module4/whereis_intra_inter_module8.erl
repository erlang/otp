%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions and modules.

-module(whereis_intra_inter_module8).
-export([continue/2]).

continue(AnAtom, Fun) ->
  case whereis(AnAtom) of
    undefined ->
      Pid = spawn(Fun);
    P when is_pid(P) ->
      P
  end.
