-module(ets_use).
-export([t1/0, t2/0]).

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

n() -> ets:new(n, [named_table]).
