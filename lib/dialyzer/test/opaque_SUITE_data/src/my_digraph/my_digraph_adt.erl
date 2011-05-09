-module(my_digraph_adt).

-export([new/0, new/1]).

-record(my_digraph, {vtab = notable,
		     etab = notable,
		     ntab = notable,
		     cyclic = true   :: boolean()}).

-opaque my_digraph() :: #my_digraph{}.

-type d_protection() :: 'private' | 'protected'.
-type d_cyclicity()  :: 'acyclic' | 'cyclic'.
-type d_type()       :: d_cyclicity() | d_protection().

-spec new() -> my_digraph().
new() -> new([]).

-spec new([atom()]) -> my_digraph().
new(Type) ->
    try check_type(Type, protected, []) of
	{Access, Ts} ->
	    V = ets:new(vertices, [set, Access]),
	    E = ets:new(edges, [set, Access]),
	    N = ets:new(neighbours, [bag, Access]),
	    ets:insert(N, [{'$vid', 0}, {'$eid', 0}]),
	    set_type(Ts, #my_digraph{vtab=V, etab=E, ntab=N})
    catch
	throw:Error -> throw(Error)
    end.

-spec check_type([atom()], d_protection(), [{'cyclic', boolean()}]) ->
	{d_protection(), [{'cyclic', boolean()}]}.

check_type([acyclic|Ts], A, L) ->
    check_type(Ts, A,[{cyclic,false} | L]);
check_type([cyclic | Ts], A, L) ->
    check_type(Ts, A, [{cyclic,true} | L]);
check_type([protected | Ts], _, L) ->
    check_type(Ts, protected, L);
check_type([private | Ts], _, L) ->
    check_type(Ts, private, L);
check_type([T | _], _, _) ->
    throw({error, {unknown_type, T}});
check_type([], A, L) -> {A, L}.

-spec set_type([{'cyclic', boolean()}], my_digraph()) -> my_digraph().

set_type([{cyclic,V} | Ks], G) ->
    set_type(Ks, G#my_digraph{cyclic = V});
set_type([], G) -> G.
