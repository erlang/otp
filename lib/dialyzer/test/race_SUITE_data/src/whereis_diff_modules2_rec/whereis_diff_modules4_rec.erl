%% This tests the presence of possible races due to a register/whereis
%% combination in a recursive function.

-module(whereis_diff_modules4_rec).
-export([continue/4]).

continue(Atom, NextAtom, Fun, Id) ->
  whereis_diff_modules3_rec:start(Atom, NextAtom, Fun, Id).
