%%--------------------------------------------------------------------------
%% Module which contains direct and indirect calls to remote functions
%% which do not exist.  Their treatment should be the same.
%%--------------------------------------------------------------------------
-module(non_existing).
-export([t_call/0, t_fun/0]).

t_call() ->
  lists:non_existing_call(42).

t_fun() ->
  Fun = fun lists:non_existing_fun/1,
  Fun(42).
