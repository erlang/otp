%% This tests that the race condition detection between whereis/register
%% is robust w.r.t. having the calls in separate functions and modules.

-module(whereis_intra_inter_module7).
-export([start/2]).

start(AnAtom, Fun) ->
  register(AnAtom, continue(AnAtom, Fun)).

continue(AnAtom, Fun) ->
  whereis_intra_inter_module8:continue(AnAtom, Fun).
