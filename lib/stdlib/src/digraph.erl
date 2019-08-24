%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(digraph).

-export([new/0, new/1, delete/1, info/1]).

-export([add_vertex/1, add_vertex/2, add_vertex/3]).
-export([del_vertex/2, del_vertices/2]).
-export([vertex/2, no_vertices/1, vertices/1]).
-export([source_vertices/1, sink_vertices/1]).

-export([add_edge/3, add_edge/4, add_edge/5]).
-export([del_edge/2, del_edges/2, del_path/3]).
-export([edge/2, no_edges/1, edges/1]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_edges/2, in_edges/2, edges/2]).
-export([out_degree/2, in_degree/2]).
-export([get_path/3, get_cycle/2]).

-export([get_short_path/3, get_short_cycle/2]).

-export_type([graph/0, d_type/0, vertex/0, edge/0, label/0]).

-record(digraph, {vtab = notable :: ets:tab(),
		  etab = notable :: ets:tab(),
		  ntab = notable :: ets:tab(),
	          cyclic = true  :: boolean()}).

-opaque graph() :: #digraph{}.

-type edge()    :: term().
-type label()   :: term().
-type vertex()  :: term().

-type add_edge_err_rsn() :: {'bad_edge', Path :: [vertex()]}
                          | {'bad_vertex', V :: vertex()}.

%%
%% Type is a list of
%%  protected | private
%%  acyclic | cyclic
%%
%%  default is [cyclic,protected]
%%
-type d_protection() :: 'private' | 'protected'.
-type d_cyclicity()  :: 'acyclic' | 'cyclic'.
-type d_type()       :: d_cyclicity() | d_protection().

-spec new() -> graph().

new() -> new([]).

-spec new(Type) -> graph() when
      Type :: [d_type()].

new(Type) ->
    case check_type(Type, protected, []) of
	{Access, Ts} ->
	    V = ets:new(vertices, [set, Access]),
	    E = ets:new(edges, [set, Access]),
	    N = ets:new(neighbours, [bag, Access]),
	    ets:insert(N, [{'$vid', 0}, {'$eid', 0}]),
	    set_type(Ts, #digraph{vtab=V, etab=E, ntab=N});
	error ->
	    erlang:error(badarg)
    end.

%%
%% Check type of graph
%%
%-spec check_type([d_type()], d_protection(), [{'cyclic', boolean()}]) ->
%       	{d_protection(), [{'cyclic', boolean()}]}.

check_type([acyclic|Ts], A, L) ->
    check_type(Ts, A,[{cyclic,false} | L]);
check_type([cyclic | Ts], A, L) ->
    check_type(Ts, A, [{cyclic,true} | L]);
check_type([protected | Ts], _, L) ->
    check_type(Ts, protected, L);
check_type([private | Ts], _, L) ->
    check_type(Ts, private, L);
check_type([], A, L) -> {A, L};
check_type(_, _, _) -> error.

%%
%% Set graph type
%%
-spec set_type([{'cyclic', boolean()}], graph()) -> graph().

set_type([{cyclic,V} | Ks], G) ->
    set_type(Ks, G#digraph{cyclic = V});
set_type([], G) -> G.


%% Data access functions

-spec delete(G) -> 'true' when
      G :: graph().

delete(G) ->
    ets:delete(G#digraph.vtab),
    ets:delete(G#digraph.etab),
    ets:delete(G#digraph.ntab).

-spec info(G) -> InfoList when
      G :: graph(),
      InfoList :: [{'cyclicity', Cyclicity :: d_cyclicity()} |
                   {'memory', NoWords :: non_neg_integer()} |
                   {'protection', Protection :: d_protection()}].

info(G) ->
    VT = G#digraph.vtab,
    ET = G#digraph.etab,
    NT = G#digraph.ntab,
    Cyclicity = case G#digraph.cyclic of
		    true  -> cyclic;
		    false -> acyclic
		end,
    Protection = ets:info(VT, protection),
    Memory = ets:info(VT, memory) + ets:info(ET, memory) + ets:info(NT, memory),
    [{cyclicity, Cyclicity}, {memory, Memory}, {protection, Protection}].

-spec add_vertex(G) -> vertex() when
      G :: graph().

add_vertex(G) ->
    do_add_vertex({new_vertex_id(G), []}, G).

-spec add_vertex(G, V) -> vertex() when
      G :: graph(),
      V :: vertex().

add_vertex(G, V) ->
    do_add_vertex({V, []}, G).

-spec add_vertex(G, V, Label) -> vertex() when
      G :: graph(),
      V :: vertex(),
      Label :: label().

add_vertex(G, V, D) ->
    do_add_vertex({V, D}, G).

-spec del_vertex(G, V) -> 'true' when
      G :: graph(),
      V :: vertex().

del_vertex(G, V) ->
    do_del_vertex(V, G).

-spec del_vertices(G, Vertices) -> 'true' when
      G :: graph(),
      Vertices :: [vertex()].

del_vertices(G, Vs) -> 
    do_del_vertices(Vs, G).

-spec vertex(G, V) -> {V, Label} | 'false' when
      G :: graph(),
      V :: vertex(),
      Label :: label().

vertex(G, V) ->
    case ets:lookup(G#digraph.vtab, V) of
	[] -> false;
	[Vertex] -> Vertex
    end.

-spec no_vertices(G) -> non_neg_integer() when
      G :: graph().

no_vertices(G) ->
    ets:info(G#digraph.vtab, size).

-spec vertices(G) -> Vertices when
      G :: graph(),
      Vertices :: [vertex()].

vertices(G) ->
    ets:select(G#digraph.vtab, [{{'$1', '_'}, [], ['$1']}]).

-spec source_vertices(graph()) -> [vertex()].

source_vertices(G) ->
    collect_vertices(G, in).

-spec sink_vertices(graph()) -> [vertex()].

sink_vertices(G) ->
    collect_vertices(G, out).

-spec in_degree(G, V) -> non_neg_integer() when
      G :: graph(),
      V :: vertex().

in_degree(G, V) ->
    length(ets:lookup(G#digraph.ntab, {in, V})).

-spec in_neighbours(G, V) -> Vertex when
      G :: graph(),
      V :: vertex(),
      Vertex :: [vertex()].

in_neighbours(G, V) ->
    ET = G#digraph.etab,
    NT = G#digraph.ntab,
    collect_elems(ets:lookup(NT, {in, V}), ET, 2).

-spec in_edges(G, V) -> Edges when
      G :: graph(),
      V :: vertex(),
      Edges :: [edge()].

in_edges(G, V) ->
    ets:select(G#digraph.ntab, [{{{in, V}, '$1'}, [], ['$1']}]).

-spec out_degree(G, V) -> non_neg_integer() when
      G :: graph(),
      V :: vertex().

out_degree(G, V) ->
    length(ets:lookup(G#digraph.ntab, {out, V})).

-spec out_neighbours(G, V) -> Vertices when
      G :: graph(),
      V :: vertex(),
      Vertices :: [vertex()].

out_neighbours(G, V) ->
    ET = G#digraph.etab,
    NT = G#digraph.ntab,
    collect_elems(ets:lookup(NT, {out, V}), ET, 3).

-spec out_edges(G, V) -> Edges when
      G :: graph(),
      V :: vertex(),
      Edges :: [edge()].

out_edges(G, V) ->
    ets:select(G#digraph.ntab, [{{{out, V}, '$1'}, [], ['$1']}]).

-spec add_edge(G, V1, V2) -> edge() | {'error', add_edge_err_rsn()} when
      G :: graph(),
      V1 :: vertex(),
      V2 :: vertex().

add_edge(G, V1, V2) ->
    do_add_edge({new_edge_id(G), V1, V2, []}, G).

-spec add_edge(G, V1, V2, Label) -> edge() | {'error', add_edge_err_rsn()} when
      G :: graph(),
      V1 :: vertex(),
      V2 :: vertex(),
      Label :: label().

add_edge(G, V1, V2, D) ->
    do_add_edge({new_edge_id(G), V1, V2, D}, G).

-spec add_edge(G, E, V1, V2, Label) -> edge() | {'error', add_edge_err_rsn()} when
      G :: graph(),
      E :: edge(),
      V1 :: vertex(),
      V2 :: vertex(),
      Label :: label().

add_edge(G, E, V1, V2, D) ->
    do_add_edge({E, V1, V2, D}, G).

-spec del_edge(G, E) -> 'true' when
      G :: graph(),
      E :: edge().

del_edge(G, E) ->
    do_del_edges([E], G).

-spec del_edges(G, Edges) -> 'true' when
      G :: graph(),
      Edges :: [edge()].

del_edges(G, Es) ->
    do_del_edges(Es, G).

-spec no_edges(G) -> non_neg_integer() when
      G :: graph().

no_edges(G) ->
    ets:info(G#digraph.etab, size).

-spec edges(G) -> Edges when
      G :: graph(),
      Edges :: [edge()].

edges(G) ->
    ets:select(G#digraph.etab, [{{'$1', '_', '_', '_'}, [], ['$1']}]).

-spec edges(G, V) -> Edges when
      G :: graph(),
      V :: vertex(),
      Edges :: [edge()].

edges(G, V) ->
    ets:select(G#digraph.ntab, [{{{out, V},'$1'}, [], ['$1']},
				{{{in, V}, '$1'}, [], ['$1']}]).

-spec edge(G, E) -> {E, V1, V2, Label} | 'false' when
      G :: graph(),
      E :: edge(),
      V1 :: vertex(),
      V2 :: vertex(),
      Label :: label().

edge(G, E) ->
    case ets:lookup(G#digraph.etab,E) of
	[] -> false;
	[Edge] -> Edge
    end.

%%
%% Generate a "unique" edge identifier (relative to this graph)
%%
-spec new_edge_id(graph()) -> edge().

-dialyzer({no_improper_lists, new_edge_id/1}).

new_edge_id(G) ->
    NT = G#digraph.ntab,
    [{'$eid', K}] = ets:lookup(NT, '$eid'),
    true = ets:delete(NT, '$eid'),
    true = ets:insert(NT, {'$eid', K+1}),
    ['$e' | K].

%%
%% Generate a "unique" vertex identifier (relative to this graph)
%%
-spec new_vertex_id(graph()) -> vertex().

-dialyzer({no_improper_lists, new_vertex_id/1}).

new_vertex_id(G) ->
    NT = G#digraph.ntab,
    [{'$vid', K}] = ets:lookup(NT, '$vid'),
    true = ets:delete(NT, '$vid'),
    true = ets:insert(NT, {'$vid', K+1}),
    ['$v' | K].

%%
%% Collect elements for a index in a tuple
%%
collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index, []).

collect_elems([{_,Key}|Keys], Table, Index, Acc) ->
    collect_elems(Keys, Table, Index,
		  [ets:lookup_element(Table, Key, Index)|Acc]);
collect_elems([], _, _, Acc) -> Acc.

-spec do_add_vertex({vertex(), label()}, graph()) -> vertex().

do_add_vertex({V, _Label} = VL, G) ->
    ets:insert(G#digraph.vtab, VL),
    V.

%%
%% Collect either source or sink vertices.
%%
collect_vertices(G, Type) ->
    Vs = vertices(G),
    lists:foldl(fun(V, A) ->
			case ets:member(G#digraph.ntab, {Type, V}) of
			    true -> A;
			    false -> [V|A]
			end
		end, [], Vs).

%%
%% Delete vertices
%%
do_del_vertices([V | Vs], G) ->
    do_del_vertex(V, G),
    do_del_vertices(Vs, G);
do_del_vertices([], #digraph{}) -> true.

do_del_vertex(V, G) ->
    do_del_nedges(ets:lookup(G#digraph.ntab, {in, V}), G),
    do_del_nedges(ets:lookup(G#digraph.ntab, {out, V}), G),
    ets:delete(G#digraph.vtab, V).

do_del_nedges([{_, E}|Ns], G) ->
    case ets:lookup(G#digraph.etab, E) of
	[{E, V1, V2, _}] ->
	    do_del_edge(E, V1, V2, G),
	    do_del_nedges(Ns, G);
	[] -> % cannot happen
	    do_del_nedges(Ns, G)
    end;
do_del_nedges([], #digraph{}) -> true.

%%
%% Delete edges
%%
do_del_edges([E|Es], G) ->
    case ets:lookup(G#digraph.etab, E) of
	[{E,V1,V2,_}] ->
	    do_del_edge(E,V1,V2,G),
	    do_del_edges(Es, G);
	[] ->
	    do_del_edges(Es, G)
    end;
do_del_edges([], #digraph{}) -> true.

do_del_edge(E, V1, V2, G) ->
    ets:select_delete(G#digraph.ntab, [{{{in, V2}, E}, [], [true]},
				       {{{out,V1}, E}, [], [true]}]),
    ets:delete(G#digraph.etab, E).

-spec rm_edges([vertex(),...], graph()) -> 'true'.

rm_edges([V1, V2|Vs], G) ->
    rm_edge(V1, V2, G),
    rm_edges([V2|Vs], G);
rm_edges(_, _) -> true.

-spec rm_edge(vertex(), vertex(), graph()) -> 'ok'.

rm_edge(V1, V2, G) ->
    Es = out_edges(G, V1),
    rm_edge_0(Es, V1, V2, G).
    
rm_edge_0([E|Es], V1, V2, G) ->
    case ets:lookup(G#digraph.etab, E) of
	[{E, V1, V2, _}]  ->
            do_del_edge(E, V1, V2, G),
	    rm_edge_0(Es, V1, V2, G);
	_ ->
	    rm_edge_0(Es, V1, V2, G)
    end;
rm_edge_0([], _, _, #digraph{}) -> ok.
    
%%
%% Check that endpoints exist
%%
-spec do_add_edge({edge(), vertex(), vertex(), label()}, graph()) ->
	edge() | {'error', add_edge_err_rsn()}.

do_add_edge({E, V1, V2, Label}, G) ->
    case ets:member(G#digraph.vtab, V1) of
	false -> {error, {bad_vertex, V1}};
	true  ->
	    case ets:member(G#digraph.vtab, V2) of
		false -> {error, {bad_vertex, V2}};
                true ->
                    case other_edge_exists(G, E, V1, V2) of
                        true -> {error, {bad_edge, [V1, V2]}};
                        false when G#digraph.cyclic =:= false ->
                            acyclic_add_edge(E, V1, V2, Label, G);
                        false ->
                            do_insert_edge(E, V1, V2, Label, G)
                    end
	    end
    end.

other_edge_exists(#digraph{etab = ET}, E, V1, V2) ->
    case ets:lookup(ET, E) of
        [{E, Vert1, Vert2, _}] when Vert1 =/= V1; Vert2 =/= V2 ->
            true;
        _ ->
            false
    end.

-spec do_insert_edge(edge(), vertex(), vertex(), label(), graph()) -> edge().

do_insert_edge(E, V1, V2, Label, #digraph{ntab=NT, etab=ET}) ->
    ets:insert(NT, [{{out, V1}, E}, {{in, V2}, E}]),
    ets:insert(ET, {E, V1, V2, Label}),
    E.

-spec acyclic_add_edge(edge(), vertex(), vertex(), label(), graph()) ->
	edge() | {'error', {'bad_edge', [vertex()]}}.

acyclic_add_edge(_E, V1, V2, _L, _G) when V1 =:= V2 ->
    {error, {bad_edge, [V1, V2]}};
acyclic_add_edge(E, V1, V2, Label, G) ->
    case get_path(G, V2, V1) of
	false -> do_insert_edge(E, V1, V2, Label, G);
	Path -> {error, {bad_edge, Path}}
    end.

%%
%% Delete all paths from vertex V1 to vertex V2
%%

-spec del_path(G, V1, V2) -> 'true' when
      G :: graph(),
      V1 :: vertex(),
      V2 :: vertex().

del_path(G, V1, V2) ->
    case get_path(G, V1, V2) of
	false -> true;
	Path ->
	    rm_edges(Path, G),
	    del_path(G, V1, V2)
    end.

%%
%% Find a cycle through V
%% return the cycle as list of vertices [V ... V]
%% if no cycle exists false is returned
%% if only a cycle of length one exists it will be
%% returned as [V] but only after longer cycles have
%% been searched.
%%

-spec get_cycle(G, V) -> Vertices | 'false' when
      G :: graph(),
      V :: vertex(),
      Vertices :: [vertex(),...].

get_cycle(G, V) ->
    case one_path(out_neighbours(G, V), V, [], [V], [V], 2, G, 1) of
	false ->
	    case lists:member(V, out_neighbours(G, V)) of
		true -> [V];
		false -> false
	    end;
	Vs -> Vs
    end.

%%
%% Find a path from V1 to V2
%% return the path as list of vertices [V1 ... V2]
%% if no path exists false is returned
%%

-spec get_path(G, V1, V2) -> Vertices | 'false' when
      G :: graph(),
      V1 :: vertex(),
      V2 :: vertex(),
      Vertices :: [vertex(),...].

get_path(G, V1, V2) ->
    one_path(out_neighbours(G, V1), V2, [], [V1], [V1], 1, G, 1).

%%
%% prune_short_path (evaluate conditions on path)
%% short : if path is too short
%% ok    : if path is ok
%%
prune_short_path(Counter, Min) when Counter < Min ->
    short;
prune_short_path(_Counter, _Min) ->
    ok.

one_path([W|Ws], W, Cont, Xs, Ps, Prune, G, Counter) ->
    case prune_short_path(Counter, Prune) of
	short -> one_path(Ws, W, Cont, Xs, Ps, Prune, G, Counter);
	ok -> lists:reverse([W|Ps])
    end;
one_path([V|Vs], W, Cont, Xs, Ps, Prune, G, Counter) ->
    case lists:member(V, Xs) of
	true ->  one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter);
	false -> one_path(out_neighbours(G, V), W, 
			  [{Vs,Ps} | Cont], [V|Xs], [V|Ps], 
			  Prune, G, Counter+1)
    end;
one_path([], W, [{Vs,Ps}|Cont], Xs, _, Prune, G, Counter) ->
    one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter-1);
one_path([], _, [], _, _, _, _, _Counter) -> false.

%%
%% Like get_cycle/2, but a cycle of length one is preferred.
%%

-spec get_short_cycle(G, V) -> Vertices | 'false' when
      G :: graph(),
      V :: vertex(),
      Vertices :: [vertex(),...].

get_short_cycle(G, V) ->
    get_short_path(G, V, V).

%%
%% Like get_path/3, but using a breadth-first search makes it possible
%% to find a short path.
%%

-spec get_short_path(G, V1, V2) -> Vertices | 'false' when
      G :: graph(),
      V1 :: vertex(),
      V2 :: vertex(),
      Vertices :: [vertex(),...].

get_short_path(G, V1, V2) ->
    T = new(),
    add_vertex(T, V1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(V1, G, Q),
    L = spath(Q1, G, V2, T),
    delete(T),
    L.
    
spath(Q, G, Sink, T) ->
    case queue:out(Q) of
	{{value, E}, Q1} ->
	    {_E, V1, V2, _Label} = edge(G, E),
	    if 
		Sink =:= V2 ->
		    follow_path(V1, T, [V2]);
		true ->
		    case vertex(T, V2) of
			false ->
			    add_vertex(T, V2),
			    add_edge(T, V2, V1),
			    NQ = queue_out_neighbours(V2, G, Q1),
			    spath(NQ, G, Sink, T);
			_V ->
			    spath(Q1, G, Sink, T)
		    end
	    end;
	{empty, _Q1} ->
	    false
    end.

follow_path(V, T, P) ->
    P1 = [V | P],
    case out_neighbours(T, V) of
	[N] ->
	    follow_path(N, T, P1);
	[] ->
	    P1
    end.

queue_out_neighbours(V, G, Q0) ->
    lists:foldl(fun(E, Q) -> queue:in(E, Q) end, Q0, out_edges(G, V)).
