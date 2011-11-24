%%-------------------------------------------------------------------
%% File    : exhaust_case.erl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Description : Tests that Dialyzer warns when it finds an unreachable
%%		 case clause (independently of whether ground vs. var).
%%
%% Created : 15 Dec 2004 by Kostis Sagonas <kostis@it.uu.se>
%%-------------------------------------------------------------------

-module(exhaust_case).
-export([t/1]).

t(X) when is_integer(X) ->
  case ret(X) of
    foo -> ok;
    bar -> ok;
    42  -> ok;
    _other -> error	%% unreachable clause (currently no warning)
    %% other -> error	%% but contrast this with this clause... hmm
  end.

ret(1) -> foo;
ret(2) -> bar.
