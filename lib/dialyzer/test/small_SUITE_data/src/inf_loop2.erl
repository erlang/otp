%%---------------------------------------------------------------------
%% Module that went into an infinite loop when trying to assign types.
%%
%% What was happening is that for functions which are in an SCC but all
%% return none(), a second chance was given to them by the analysis to
%% see whether they return none() because they are involved in an loop
%% (presumably server-related) and could be assigned the type unit()
%% instead. The problem is that when the really return none() for some
%% other reason (an error such in this case) then we will again find
%% none() and try again for unit(), thereby entering an infinite loop.
%% The issue was resolved on May 17th by adding an appropriate boolean
%% parameter to dialyzer_typesig:solve_scc() function.
%%---------------------------------------------------------------------
-module(inf_loop2).

-export([test/0]).

test() ->
  lists:reverse(gazonk),
  loop().

loop() ->
  test().
