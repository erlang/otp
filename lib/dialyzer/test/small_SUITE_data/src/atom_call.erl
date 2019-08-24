%%%-------------------------------------------------------------------
%%% File    : atom_call.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 10 Dec 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(atom_call).

-export([f/0,g/0]).

f() -> ok.

g() -> F = f, F().
