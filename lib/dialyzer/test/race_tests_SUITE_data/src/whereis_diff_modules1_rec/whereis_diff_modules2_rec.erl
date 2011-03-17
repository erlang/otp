%% This tests the presence of possible races due to a register/whereis
%% combination in a recursive function.

-module(whereis_diff_modules2_rec).
-export([continue/4]).

continue(Atom, NextAtom, Fun, Id) ->
  whereis_diff_modules1_rec:start(Atom, NextAtom, Fun, Id).
