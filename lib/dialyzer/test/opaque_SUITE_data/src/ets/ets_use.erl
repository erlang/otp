-module(ets_use).
-export([t1/0, t2/0, t3/0, t4/0]).

t1() ->
    case n() of
	T when is_atom(T) -> atm;
	T when is_integer(T) -> int
    end.

t2() ->
    case n() of
	T when is_integer(T) -> int;
	T when is_atom(T) -> atm
    end.

t3() ->
    is_atom(n()). % no warning since atom() is possible

t4() ->
    is_integer(n()). % opaque warning since ets:tid() is opaque

n() -> ets:new(n, [named_table]). % -> atom() | ets:tid()
