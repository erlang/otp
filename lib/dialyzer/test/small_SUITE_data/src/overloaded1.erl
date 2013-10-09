%%-----------------------------------------------------------------------------
%% Test that tests overloaded contracts.
%% In December 2008 it works as far as intersection types are concerned (test1)
%% However, it does NOT work as far as type variables are concerned (test2)
%%-----------------------------------------------------------------------------
-module(overloaded1).
-export([test1/0, test2/0, foo/2]).

test1() ->
  {ok, gazonk} = foo({a,b,1}, atom_to_list(gazonk)),
  ok.

test2() ->
  {ok, gazonk} = foo(baz, []),
  ok.

-type mod() :: atom().

-spec foo(ATM, list()) -> {'ok', ATM} | {'error', _} when ATM :: mod()
       ; (MFA, list()) -> {'ok', MFA} | {'error', _} when MFA :: mfa().

foo(F, _) when is_atom(F) ->
  case atom_to_list(F) of
    [42|_] -> {ok, F};
    _Other -> {error, some_mod:some_function()}
  end;
foo({M,F,A}, _) ->
  case A =:= 0 of
    false -> {ok, {M,F,A}};
    true -> {error, M}
  end.
