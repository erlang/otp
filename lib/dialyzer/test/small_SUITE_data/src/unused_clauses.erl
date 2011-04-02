%%-------------------------------------------------------------------
%% File    : unused_clauses.erl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Description : Tests that Dialyzer warns when it finds an unused
%%		 clause.
%%
%% Created : 16 Mar 2006 by Kostis Sagonas <kostis@it.uu.se>
%%-------------------------------------------------------------------

-module(unused_clauses).
-export([test/0]).

test() -> {t(atom), t({42})}.

t(X) when is_atom(X) -> X;
t(X) when is_integer(X) -> X;
t(X) when is_tuple(X) -> element(1, X);
t(X) when is_binary(X) -> X.
