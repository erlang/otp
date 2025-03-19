%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
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
-module(digraph_utils).
-moduledoc """
This module provides algorithms based on depth-first traversal of directed
graphs.

For basic functions on directed graphs, see the `m:digraph` module.

- A _directed graph_{: #digraph } (or just "digraph") is a pair (V, E) of a
  finite set V of _vertices_{: #vertex } and a finite set E of _directed
  edges_{: #edge } (or just "edges"). The set of edges E is a subset of V × V
  (the Cartesian product of V with itself).
- Digraphs can be annotated with more information. Such information can be
  attached to the vertices and to the edges of the digraph. An annotated digraph
  is called a _labeled digraph_, and the information attached to a vertex or an
  edge is called a _label_{: #label }.
- An edge e = (v, w) is said to _emanate_{: #emanate } from vertex v and to be
  _incident_{: #incident } on vertex w.
- If an edge is emanating from v and incident on w, then w is said to be an
  _out-neighbor_{: #out_neighbour } of v, and v is said to be an _in-neighbor_{:
  #in_neighbour } of w.
- A _path_{: #path } P from v\[1] to v\[k] in a digraph (V, E) is a non-empty
  sequence v\[1], v\[2], ..., v\[k] of vertices in V such that there is an edge
  (v\[i],v\[i+1]) in E for 1 <= i < k.
- The _length_{: #length } of path P is k-1.
- Path P is a _cycle_{: #cycle } if the length of P is not zero and v\[1] =
  v\[k].
- A _loop_{: #loop } is a cycle of length one.
- An _acyclic digraph_{: #acyclic_digraph } is a digraph without cycles.
- A _depth-first traversal_{: #depth_first_traversal } of a directed digraph can
  be viewed as a process that visits all vertices of the digraph. Initially, all
  vertices are marked as unvisited. The traversal starts with an arbitrarily
  chosen vertex, which is marked as visited, and follows an edge to an unmarked
  vertex, marking that vertex. The search then proceeds from that vertex in the
  same fashion, until there is no edge leading to an unvisited vertex. At that
  point the process backtracks, and the traversal continues as long as there are
  unexamined edges. If unvisited vertices remain when all edges from the first
  vertex have been examined, some so far unvisited vertex is chosen, and the
  process is repeated.
- A _partial ordering_{: #partial_ordering } of a set S is a transitive,
  antisymmetric, and reflexive relation between the objects of S.
- The problem of [_topological sorting_](https://en.wikipedia.org/wiki/Topological_sorting) {: #topsort }
  is to find a total ordering of S that is a superset of the partial ordering.
  A digraph G = (V, E) is equivalent to a relation E on V (we neglect that
  the version of directed graphs provided by the `digraph` module allows
  multiple edges between vertices). If the digraph has no cycles of length
  two or more, the reflexive and transitive closure of E is a partial ordering.
- A _subgraph_{: #subgraph } G' of G is a digraph whose vertices and edges form
  subsets of the vertices and edges of G.
- G' is _maximal_ with respect to a property P if all other subgraphs that
  include the vertices of G' do not have property P.
- A [_strongly connected component_](https://en.wikipedia.org/wiki/Strongly_connected_component) {: #strong_components }
  is a maximal subgraph such that there is a path between each pair of vertices
- A _connected component_{: #components } is a maximal subgraph such that there
  is a path between each pair of vertices, considering all edges undirected.
- An _arborescence_{: #arborescence } is an acyclic digraph with a vertex V, the
  _root_{: #root }, such that there is a unique path from V to every other
  vertex of G.
- A _tree_{: #tree } is an acyclic non-empty digraph such that there is a unique
  path between every pair of vertices, considering all edges undirected.

## See Also

`m:digraph`
""".

%%% Operations on directed (and undirected) graphs.
%%%
%%% Implementation based on Launchbury, John: Graph Algorithms with a 
%%% Functional Flavour, in Jeuring, Johan, and Meijer, Erik (Eds.): 
%%% Advanced Functional Programming, Lecture Notes in Computer 
%%% Science 925, Springer Verlag, 1995.

-export([components/1, strong_components/1, cyclic_strong_components/1, 
	 reachable/2, reachable_neighbours/2, 
	 reaching/2, reaching_neighbours/2,
	 topsort/1, is_acyclic/1, 
         arborescence_root/1, is_arborescence/1, is_tree/1, 
         loop_vertices/1,
	 subgraph/2, subgraph/3, condensation/1,
	 preorder/1, postorder/1]).

%%
%%  A convenient type alias
%%

%%
%%  Exported functions
%%

-doc """
Returns a list of [connected components](`m:digraph_utils#components`). Each
component is represented by its vertices. The order of the vertices and the
order of the components are arbitrary. Each vertex of digraph `Digraph` occurs
in exactly one component.
""".
-spec components(Digraph) -> [Component] when
      Digraph :: digraph:graph(),
      Component :: [digraph:vertex()].

components(G) ->
    forest(G, fun inout/3).

-doc """
Returns a list of
[strongly connected components](`m:digraph_utils#strong_components`). Each
strongly component is represented by its vertices. The order of the vertices and
the order of the components are arbitrary. Each vertex of digraph `Digraph`
occurs in exactly one strong component.
""".
-spec strong_components(Digraph) -> [StrongComponent] when
      Digraph :: digraph:graph(),
      StrongComponent :: [digraph:vertex()].

strong_components(G) ->
    forest(G, fun in/3, revpostorder(G)).

-doc """
Returns a list of
[strongly connected components](`m:digraph_utils#strong_components`). Each
strongly component is represented by its vertices. The order of the vertices and
the order of the components are arbitrary. Only vertices that are included in
some [cycle](`m:digraph_utils#cycle`) in `Digraph` are returned, otherwise the
returned list is equal to that returned by `strong_components/1`.
""".
-spec cyclic_strong_components(Digraph) -> [StrongComponent] when
      Digraph :: digraph:graph(),
      StrongComponent :: [digraph:vertex()].

cyclic_strong_components(G) ->
    remove_singletons(strong_components(G), G, []).

-doc """
Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) in `Digraph` from some vertex of
`Vertices` to the vertex. In particular, as paths can have length zero, the
vertices of `Vertices` are included in the returned list.
""".
-spec reachable(Vertices, Digraph) -> Reachable when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()],
      Reachable :: [digraph:vertex()].

reachable(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun out/3, Vs, first)).

-doc """
Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) in `Digraph` of length one or
more from some vertex of `Vertices` to the vertex. As a consequence, only those
vertices of `Vertices` that are included in some
[cycle](`m:digraph_utils#cycle`) are returned.
""".
-spec reachable_neighbours(Vertices, Digraph) -> Reachable when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()],
      Reachable :: [digraph:vertex()].

reachable_neighbours(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun out/3, Vs, not_first)).

-doc """
Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) from the vertex to some vertex
of `Vertices`. In particular, as paths can have length zero, the vertices of
`Vertices` are included in the returned list.
""".
-spec reaching(Vertices, Digraph) -> Reaching when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()],
      Reaching :: [digraph:vertex()].

reaching(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun in/3, Vs, first)).

-doc """
Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) of length one or more from the
vertex to some vertex of `Vertices`. Therefore only those vertices of `Vertices`
that are included in some [cycle](`m:digraph_utils#cycle`) are returned.
""".
-spec reaching_neighbours(Vertices, Digraph) -> Reaching when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()],
      Reaching :: [digraph:vertex()].

reaching_neighbours(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun in/3, Vs, not_first)).

-doc """
Returns a [topological ordering](`m:digraph_utils#topsort`) of the vertices of
digraph `Digraph` if such an ordering exists, otherwise `false`. For each vertex
in the returned list, no [out-neighbors](`m:digraph_utils#out_neighbour`) occur
earlier in the list.
""".
-spec topsort(Digraph) -> Vertices | 'false' when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()].

topsort(G) ->
    L = revpostorder(G),
    case length(forest(G, fun in/3, L)) =:= length(digraph:vertices(G)) of
	true  -> L;
	false -> false
    end.

-doc """
Returns `true` if and only if digraph `Digraph` is
[acyclic](`m:digraph_utils#acyclic_digraph`).
""".
-spec is_acyclic(Digraph) -> boolean() when
      Digraph :: digraph:graph().

is_acyclic(G) ->
    loop_vertices(G) =:= [] andalso topsort(G) =/= false.

-doc """
Returns `{yes, Root}` if `Root` is the [root](`m:digraph_utils#root`) of the
arborescence `Digraph`, otherwise `no`.
""".
-spec arborescence_root(Digraph) -> 'no' | {'yes', Root} when
      Digraph :: digraph:graph(),
      Root :: digraph:vertex().

arborescence_root(G) ->
    case digraph:no_edges(G) =:= digraph:no_vertices(G) - 1 of
        true ->
            try
                F = fun(V, Z) ->
                            case digraph:in_degree(G, V) of
                                1 -> Z;
                                0 when Z =:= [] -> [V]
                            end
                    end,
                [Root] = lists:foldl(F, [], digraph:vertices(G)),
                {yes, Root}
            catch _:_ ->
                no
            end;
        false ->
            no
    end.

-doc """
Returns `true` if and only if digraph `Digraph` is an
[arborescence](`m:digraph_utils#arborescence`).
""".
-spec is_arborescence(Digraph) -> boolean() when
      Digraph :: digraph:graph().

is_arborescence(G) ->
    arborescence_root(G) =/= no.

-doc """
Returns `true` if and only if digraph `Digraph` is a
[tree](`m:digraph_utils#tree`).
""".
-spec is_tree(Digraph) -> boolean() when
      Digraph :: digraph:graph().

is_tree(G) ->
    (digraph:no_edges(G) =:= digraph:no_vertices(G) - 1)
    andalso (length(components(G)) =:= 1).

-doc """
Returns a list of all vertices of `Digraph` that are included in some
[loop](`m:digraph_utils#loop`).
""".
-spec loop_vertices(Digraph) -> Vertices when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()].

loop_vertices(G) ->
    [V || V <- digraph:vertices(G), is_reflexive_vertex(V, G)].

-doc(#{equiv => subgraph/3}).
-spec subgraph(Digraph, Vertices) -> SubGraph when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()],
      SubGraph :: digraph:graph().

subgraph(G, Vs) ->
    try
	subgraph_opts(G, Vs, [])
    catch
	throw:badarg ->
	    erlang:error(badarg)
    end.

-doc """
Creates a maximal [subgraph](`m:digraph_utils#subgraph`) of `Digraph` having as
vertices those vertices of `Digraph` that are mentioned in `Vertices`.

If the value of option `type` is `inherit`, which is the default, the type of
`Digraph` is used for the subgraph as well. Otherwise the option value of `type`
is used as argument to `digraph:new/1`.

If the value of option `keep_labels` is `true`, which is the default, the
[labels](`m:digraph_utils#label`) of vertices and edges of `Digraph` are used
for the subgraph as well. If the value is `false`, default label `[]` is used
for the vertices and edges of the subgroup.

[`subgraph(Digraph, Vertices)`](`subgraph/2`) is equivalent to
[`subgraph(Digraph, Vertices, [])`](`subgraph/3`).

If any of the arguments are invalid, a `badarg` exception is raised.
""".
-spec subgraph(Digraph, Vertices, Options) -> SubGraph when
      Digraph :: digraph:graph(),
      SubGraph :: digraph:graph(),
      Vertices :: [digraph:vertex()],
      Options :: [{'type', SubgraphType} | {'keep_labels', boolean()}],
      SubgraphType :: 'inherit' | [digraph:d_type()].

subgraph(G, Vs, Opts) ->
    try
	subgraph_opts(G, Vs, Opts)
    catch
	throw:badarg ->
	    erlang:error(badarg)
    end.

-doc """
Creates a digraph where the vertices are the
[strongly connected components](`m:digraph_utils#strong_components`) of
`Digraph` as returned by `strong_components/1`. If X and Y are two different
strongly connected components, and vertices x and y exist in X and Y,
respectively, such that there is an edge [emanating](`m:digraph_utils#emanate`)
from x and [incident](`m:digraph_utils#incident`) on y, then an edge emanating
from X and incident on Y is created.

The created digraph has the same type as `Digraph`. All vertices and edges have
the default [label](`m:digraph_utils#label`) `[]`.

Each [cycle](`m:digraph_utils#cycle`) is included in some strongly connected
component, which implies that a
[topological ordering](`m:digraph_utils#topsort`) of the created digraph always
exists.
""".
-spec condensation(Digraph) -> CondensedDigraph when
      Digraph :: digraph:graph(),
      CondensedDigraph :: digraph:graph().

condensation(G) ->
    SCs = strong_components(G),
    %% Each component is assigned a number.
    %% V2I: from vertex to number.
    %% I2C: from number to component.
    V2I = ets:new(condensation, []),
    I2C = ets:new(condensation, []),
    CFun = fun(SC, N) -> lists:foreach(fun(V) -> 
					  true = ets:insert(V2I, {V,N}) 
				       end, 
				       SC), 
			 true = ets:insert(I2C, {N, SC}), 
			 N + 1 
	   end,
    lists:foldl(CFun, 1, SCs),
    SCG = subgraph_opts(G, [], []),
    lists:foreach(fun(SC) -> condense(SC, G, SCG, V2I, I2C) end, SCs),
    ets:delete(V2I),
    ets:delete(I2C),
    SCG.

-doc """
Returns all vertices of digraph `Digraph`. The order is given by a
[depth-first traversal](`m:digraph_utils#depth_first_traversal`) of the digraph,
collecting visited vertices in preorder.
""".
-spec preorder(Digraph) -> Vertices when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()].

preorder(G) ->
    T = sets:new(),
    {_, Acc} = ptraverse(roots(G), fun out/3, G, T, [], []),
    lists:reverse(lists:append(Acc)).

-doc """
Returns all vertices of digraph `Digraph`. The order is given by a
[depth-first traversal](`m:digraph_utils#depth_first_traversal`) of the digraph,
collecting visited vertices in postorder. More precisely, the vertices visited
while searching from an arbitrarily chosen vertex are collected in postorder,
and all those collected vertices are placed before the subsequently visited
vertices.
""".
-spec postorder(Digraph) -> Vertices when
      Digraph :: digraph:graph(),
      Vertices :: [digraph:vertex()].

postorder(G) ->
    T = sets:new(),
    {Acc, _} = posttraverse(roots(G), G, T, []),
    lists:reverse(Acc).

%%
%%  Local functions
%%

roots(G) ->
    R1 = [V || V <- digraph:vertices(G), digraph:in_degree(G, V) =:= 0],
    R2 = [X || [X|_] <- components(G)],
    R1 ++ R2.

forest(G, SF) ->
    forest(G, SF, digraph:vertices(G)).

forest(G, SF, Vs) ->
    forest(G, SF, Vs, first).

forest(G, SF, Vs, HandleFirst) ->
    T = sets:new(),
    F = fun(V, {T0, LL}) -> pretraverse(HandleFirst, V, SF, G, T0, LL) end,
    {_, LL} = lists:foldl(F, {T, []}, Vs),
    LL.

pretraverse(first, V, SF, G, T, LL) ->
    ptraverse([V], SF, G, T, [], LL);
pretraverse(not_first, V, SF, G, T, LL) ->
    case sets:is_element(V, T) of
	false -> ptraverse(SF(G, V, []), SF, G, T, [], LL);
	true  -> {T, LL}
    end.

ptraverse([V | Vs], SF, G, T0, Rs, LL) ->
    case sets:is_element(V, T0) of
	false ->
	    T1 = sets:add_element(V, T0),
	    ptraverse(SF(G, V, Vs), SF, G, T1, [V | Rs], LL);
	true ->
	    ptraverse(Vs, SF, G, T0, Rs, LL)
    end;
ptraverse([], _SF, _G, T, [], LL) ->
    {T, LL};
ptraverse([], _SF, _G, T, Rs, LL) ->
    {T, [Rs | LL]}.

revpostorder(G) ->
    T = sets:new(),
    {L, _} = posttraverse(digraph:vertices(G), G, T, []),
    L.

posttraverse([V | Vs], G, T0, Acc0) ->
    case sets:is_element(V, T0) of
        false ->
            T1 = sets:add_element(V, T0),
            {Acc1, T2} = posttraverse(out(G, V, []), G, T1, Acc0),
            posttraverse(Vs, G, T2, [V|Acc1]);
        true ->
            posttraverse(Vs, G, T0, Acc0)
    end;
posttraverse([], _G, T, Acc) ->
    {Acc, T}.

in(G, V, Vs) ->
    digraph:in_neighbours(G, V) ++ Vs.

out(G, V, Vs) ->
    digraph:out_neighbours(G, V) ++ Vs.

inout(G, V, Vs) ->
    in(G, V, out(G, V, Vs)).

remove_singletons([C=[V] | Cs], G, L) ->
    case is_reflexive_vertex(V, G) of
	true  -> remove_singletons(Cs, G, [C | L]);
	false -> remove_singletons(Cs, G, L)
    end;
remove_singletons([C | Cs], G, L) ->
    remove_singletons(Cs, G, [C | L]);
remove_singletons([], _G, L) ->
    L.

is_reflexive_vertex(V, G) ->
    lists:member(V, digraph:out_neighbours(G, V)).

subgraph_opts(G, Vs, Opts) ->
    subgraph_opts(Opts, inherit, true, G, Vs).

subgraph_opts([{type, Type} | Opts], _Type0, Keep, G, Vs)
  when Type =:= inherit; is_list(Type) ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([{keep_labels, Keep} | Opts], Type, _Keep0, G, Vs)
  when is_boolean(Keep) ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([], inherit, Keep, G, Vs) ->
    Info = digraph:info(G),
    {_, {_, Cyclicity}} = lists:keysearch(cyclicity, 1, Info),
    {_, {_, Protection}} = lists:keysearch(protection, 1, Info),
    subgraph(G, Vs, [Cyclicity, Protection], Keep);
subgraph_opts([], Type, Keep, G, Vs) ->
    subgraph(G, Vs, Type, Keep);
subgraph_opts(_, _Type, _Keep, _G, _Vs) ->
    throw(badarg).

subgraph(G, Vs, Type, Keep) ->
    try digraph:new(Type) of
	SG ->
	    lists:foreach(fun(V) -> subgraph_vertex(V, G, SG, Keep) end, Vs),
	    EFun = fun(V) -> lists:foreach(fun(E) -> 
					       subgraph_edge(E, G, SG, Keep) 
                                           end,
                                           digraph:out_edges(G, V))
		   end,
	    lists:foreach(EFun, digraph:vertices(SG)),
	    SG
    catch
	error:badarg ->
	    throw(badarg)
    end.

subgraph_vertex(V, G, SG, Keep) ->
    case digraph:vertex(G, V) of
	false -> ok;
	_ when not Keep -> digraph:add_vertex(SG, V);
	{_V, Label} when Keep -> digraph:add_vertex(SG, V, Label)
    end.

subgraph_edge(E, G, SG, Keep) ->
    {_E, V1, V2, Label} = digraph:edge(G, E),
    case digraph:vertex(SG, V2) of
	false -> ok;
	_ when not Keep -> digraph:add_edge(SG, E, V1, V2, []);
	_ when Keep -> digraph:add_edge(SG, E, V1, V2, Label)
    end.

condense(SC, G, SCG, V2I, I2C) ->
    T = ets:new(condense, []),
    NFun = fun(Neighbour) ->
		   [{_V,I}] = ets:lookup(V2I, Neighbour),
		   ets:insert(T, {I})
	   end,
    VFun = fun(V) -> lists:foreach(NFun, digraph:out_neighbours(G, V)) end,
    lists:foreach(VFun, SC),
    digraph:add_vertex(SCG, SC),
    condense(ets:first(T), T, SC, G, SCG, I2C),
    ets:delete(T).

condense('$end_of_table', _T, _SC, _G, _SCG, _I2C) ->
    ok;
condense(I, T, SC, G, SCG, I2C) ->
    [{_,C}] = ets:lookup(I2C, I),
    digraph:add_vertex(SCG, C),
    _ = [digraph:add_edge(SCG, SC, C) || C =/= SC],
    condense(ets:next(T, I), T, SC, G, SCG, I2C).
