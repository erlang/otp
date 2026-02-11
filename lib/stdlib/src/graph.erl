%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2026. All Rights Reserved.
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
-module(graph).
-moduledoc """
A functional implementation of labeled directed graphs.

This module is closely modelled on the `digraph` and `digraph_utils`
modules, which represent graphs using mutable ETS tables. This functional
implementation is more lightweight and does not involve mutable state and
table ownership, and makes it easy to keep multiple versions of a graph, but
for large tables with a long lifetime, the ETS based implementation can be
more suitable. In the context of this module, we only use the term "digraph"
when referring to the `digraph` implementation, and not to directed graphs
in general.

When rewriting code from using `digraph` to using `graph`, keep in mind that:

- Graphs are immutable: each modifying operation returns the new graph,
  which needs to be saved in a variable and passed to the next operation,
  for example `G0 = graph:new(), G1 = graph:add_vertex(G0, v1), G2 =
  graph:add_vertex(G1, v2)`.
- Graphs are garbage collected and do not need to be explicitly deleted.
- There are no `protected` or `private` options and no `memory` info key.
- Edges are not objects with identity and state. An edge is uniquely
  identified by the triple `{From, To, Label}` where the default label is
  `[]`. There can be multiple edges with the same `From` and `To` but only
  if they have different values for `Label`.
- Vertices, however, have a unique identifier just as in `digraph`. The
  label of an existing vertex can be replaced by a new call to
  `add_vertex(Id, Label)`. The label defaults to `[]`.
- The functions in `digraph_utils` have been included directly in the
  `graph` module for simplicity.

Some graph theoretical definitions:

- A _directed graph_{: #graph } (here simply called "graph") is a pair (V, E)
  of a finite set V of _vertices_{: #vertex } and a finite set E of _directed
  edges_{: #edge } (here simply called "edges"). The set of edges E is a
  subset of V × V (the Cartesian product of V with itself).

  In this module, V is allowed to be empty. The so obtained unique graph is
  called the _empty graph_{: #empty_graph }. Each vertex has a unique Erlang
  term as identifier.

- Graphs can be annotated with more information. Such information can be
  attached to the vertices and to the edges of the graph. An annotated graph
  is called a _labeled graph_, and the information attached to a vertex or an
  edge is called a _label_{: #label }. Labels are Erlang terms.

- An edge e = (v, w) is said to _emanate_{: #emanate } from vertex v and to be
  _incident_{: #incident } on vertex w.

- The _out-degree_{: #out_degree } of a vertex is the number of edges emanating
  from that vertex.

- The _in-degree_{: #in_degree } of a vertex is the number of edges incident on
  that vertex.

- If an edge is emanating from v and incident on w, then w is said to be an
  _out-neighbor_{: #out_neighbour } of v, and v is said to be an _in-neighbor_{:
  #in_neighbour } of w.

- A _subgraph_{: #subgraph } G' of G is a graph whose vertices and edges form
  subsets of the vertices and edges of G.

- G' is _maximal_ with respect to a property P if all other subgraphs that
  include the vertices of G' do not have property P.

- A _path_{: #path } P from v\[1] to v\[k] in a graph (V, E) is a non-empty
  sequence v\[1], v\[2], ..., v\[k] of vertices in V such that there is an edge
  (v\[i], v\[i+1]) in E for 1 <= i < k.

- The _length_{: #length } of path P is k-1.

- Path P is _simple_{: #simple_path } if all vertices are distinct, except that
  the first and the last vertices can be the same.

- Path P is a _cycle_{: #cycle } if the length of P is not zero and v\[1] =
  v\[k].

- A _loop_{: #loop } is a cycle of length one.

- A _simple cycle_{: #simple_cycle } is a path that is both a cycle and simple.

- An _acyclic graph_{: #acyclic_graph } is a graph without cycles.

- A _tree_{: #tree } is an acyclic non-empty graph such that there is a unique
  path between every pair of vertices, considering all edges undirected. In an
  undirected tree, any vertex can be used as root. Informally however, "tree"
  is often used to refer to mean an _out-tree_ (arborescence), in particular
  in computer science.

- An _arborescence_{: #arborescence } or _directed rooted tree_ or _out-tree_
  is an acyclic directed graph with a vertex V, the _root_{: #root }, such that
  there is a unique path from V to every other vertex of G.

- A _forest_{: #forest } is a disjoint union of trees.

- A [_strongly connected component_](https://en.wikipedia.org/wiki/Strongly_connected_component) {: #strong_components }
  is a maximal subgraph such that there is a path between each pair of vertices.

- A _connected component_{: #components } is a maximal subgraph such that there
  is a path between each pair of vertices, considering all edges undirected.

This module also provides algorithms based on depth-first traversal of
directed graphs.

- A _depth-first traversal_{: #depth_first_traversal } of a directed graph can
  be viewed as a process that visits all vertices of the graph. Initially, all
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
  A graph G = (V, E) is equivalent to a relation E on V (we neglect that
  the version of directed graphs provided by the `digraph` module allows
  multiple edges between vertices). If the graph has no cycles of length
  two or more, the reflexive and transitive closure of E is a partial ordering.
""".
-moduledoc(#{ since => ~"OTP @OTP-19922@"}).

%% Basic functionality
-export([new/0, new/1, info/1,
         no_edges/1, source_vertices/1, sink_vertices/1,
         add_vertex/1, add_vertex/2, add_vertex/3,
         add_edge/3, add_edge/4,
         del_edge/2, del_edges/2, del_edges/3,
         del_vertex/2, del_vertices/2,
         edges/1, edges/2, edges/3,
         has_vertex/2, has_edge/2, has_edge/3,
         has_path/3, del_path/3, get_path/3, get_cycle/2,
         get_short_path/3, get_short_cycle/2,
         in_degree/2, in_edges/2, in_neighbours/2,
         no_vertices/1,
         out_degree/2, out_edges/2, out_neighbours/2,
         vertex/2, vertex/3, vertices/1, vertices_with_labels/1,
         fold_vertices/3
         ]).

%% Utilities
-export([components/1, strong_components/1, cyclic_strong_components/1,
         reachable/2, reachable_via_neighbours/2,
         reaching/2, reaching_via_neighbours/2,
         topsort/1, is_acyclic/1, roots/1,
         arborescence_root/1, is_arborescence/1, is_tree/1,
         loop_vertices/1,
         subgraph/2, subgraph/3, condensation/1,
         preorder/1, preorder/2, postorder/1, postorder/2,
         reverse_postorder/1, reverse_postorder/2]).

-export_type([graph/0, graph_type/0, graph_cyclicity/0,
              vertex/0, edge/0, label/0]).

%% Debugging.
-define(DEBUG, false).
-if(?DEBUG).
-export([dump/1,dump/2,dump/3]).
-endif.

-import(lists, [foldl/3, reverse/1]).

-type edge_map() :: #{ vertex() => ordsets:ordset(vertex()) }.
-type vertice_map() :: #{ vertex() => label() }.

-record(graph, {vs = #{} :: vertice_map(),
                in_es = #{} :: edge_map(),
                out_es = #{} :: edge_map(),
                cyclic = true :: boolean(),
                next_vid = 0 :: non_neg_integer()}).

-type graph() :: #graph{}.

-type vertex() :: term().
-type label() :: term().
-type edge() :: {vertex(), vertex(), label()}.

-type graph_cyclicity()  :: 'acyclic' | 'cyclic'.
-type graph_type()       :: graph_cyclicity().

-doc(#{ equiv => new([]) }).
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec new() -> graph().
new() ->
    new([]).

-doc """
Creates a new graph.

Returns an [empty graph](`m:graph#empty_graph`) with properties according to
the options in `Options`:

- **`cyclic`** - Allows [cycles](`m:graph#cycle`) in the graph (default).

- **`acyclic`** - The graph is to be kept [acyclic](`m:graph#acyclic_graph`).
  Attempting to add an edge that would introduce a cycle will raise an error
  `{bad_edge, {From, To}}`. Note that this slows down the adding of edges.

If an unrecognized option is specified or `Options` is not a proper list, a
`badarg` exception is raised.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec new([graph_type()]) -> graph().
new(Options) when is_list(Options) ->
    new_1(Options, #graph{}).

new_1([cyclic|Opts], G) ->
    new_1(Opts, G#graph{cyclic = true});
new_1([acyclic|Opts], G) ->
    new_1(Opts, G#graph{cyclic = false});
new_1([], G) ->
    G;
new_1(_, _) ->
    error(badarg).

-doc """
Returns a list of `{Tag, Value}` pairs describing graph `G`.

The following pairs are returned:

- `{cyclicity, Cyclicity}`, where `Cyclicity` is `cyclic` or `acyclic`,
  according to the options given to `new`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec info(graph()) -> [{'cyclicity', graph_cyclicity()}].
info(G) ->
    Cyclicity = case G#graph.cyclic of
                    true  -> cyclic;
                    false -> acyclic
                end,
    [{cyclicity, Cyclicity}].

-doc """
Adds a new vertex to graph `G`, returning the created vertex id.

The new vertex will have the empty list `[]` as [label](`m:graph#label`).

Note: Vertex ID:s are assigned as integers in increasing order starting from
zero. If you use `add_vertex/2` or `add_vertex/3` to insert vertices with
your own identifiers, this function could generate an ID that already exists
in the graph.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec add_vertex(graph()) -> {vertex(), graph()}.
add_vertex(G) ->
    {V, G1} = new_vertex_id(G),
    {V, add_vertex(G1, V)}.

new_vertex_id(#graph{next_vid=V}=G) ->
    {V, G#graph{next_vid=V+1}}.

-doc(#{equiv => add_vertex(G, V, [])}).
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec add_vertex(graph(), vertex()) -> graph().
add_vertex(G, V) ->
    add_vertex(G, V, []).

-doc """
Creates or modifies vertex `V` of graph `G`, using `L` as the (new)
[label](`m:graph#label`) of the vertex.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec add_vertex(graph(), vertex(), label()) -> graph().
add_vertex(G, V, L) ->
    #graph{vs=Vs0} = G,
    Vs = Vs0#{V=>L},
    G#graph{vs=Vs}.

-doc """
Deletes vertex `V` from graph `G`.

Any edges [emanating](`m:graph#emanate`) from `V` or
[incident](`m:graph#incident`) on `V` are also deleted.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec del_vertex(graph(), vertex()) -> graph().
del_vertex(G, V) ->
    #graph{vs=Vs0,in_es=InEsMap0,out_es=OutEsMap0} = G,
    InEs = maps:get(V, InEsMap0, []),
    OutEsMap = foldl(fun({From,_,_}=E, A) -> edge_map_del(From, E, A) end,
                     maps:remove(V, OutEsMap0), InEs),
    OutEs = maps:get(V, OutEsMap0, []),
    InEsMap = foldl(fun({_,To,_}=E, A) -> edge_map_del(To, E, A) end,
                    maps:remove(V, InEsMap0), OutEs),
    Vs = maps:remove(V, Vs0),
    G#graph{vs=Vs,in_es=InEsMap,out_es=OutEsMap}.

-doc "Deletes the vertices in list `Vs` from graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec del_vertices(G::graph(), Vs::[vertex()]) -> graph().
del_vertices(G, [V | Vs]) ->
    del_vertices(del_vertex(G, V), Vs);
del_vertices(G, []) -> G.

-doc """
Returns the [label](`m:graph#label`) of the vertex `V` of graph `G`.

An exception is raised if `V` does not exist in `G`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec vertex(G::graph(), V::vertex()) -> label().
vertex(#graph{vs=Vs}, V) ->
    map_get(V, Vs).

-doc """
Returns the [label](`m:graph#label`) of the vertex `V` of graph `G`,
or returns `Default` if `V` does not exist in `G`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec vertex(G::graph(), V::vertex(), Default::label()) -> label().
vertex(#graph{vs=Vs}, V, Default) ->
    maps:get(V, Vs, Default).

-doc "Returns the number of vertices of graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec no_vertices(G::graph()) -> non_neg_integer().
no_vertices(#graph{vs=Vs}) ->
    map_size(Vs).

-doc "Returns a list of all vertices of graph `G`, in some unspecified order.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec vertices(G::graph()) -> [vertex()].
vertices(#graph{vs=Vs}) ->
    maps:keys(Vs).

-doc """
Returns a list of all pairs `{V, L}` of vertices of graph `G` and their
respective labels, in some unspecified order.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec vertices_with_labels(G::graph()) -> [{vertex(), label()}].
vertices_with_labels(#graph{vs=Vs}) ->
    maps:to_list(Vs).

-doc """
Returns a list of all vertices of graph `G` with
[in-degree](`m:graph#in_degree`) zero.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec source_vertices(G::graph()) -> [vertex()].
source_vertices(#graph{vs=Vs, in_es=InEsMap}) ->
    maps:fold(fun(V, _, Acc) ->
                      case maps:get(V, InEsMap, []) of
                          [] -> [V | Acc];
                          _ -> Acc
                      end
              end, [], Vs).

-doc """
Returns a list of all vertices of graph `G` with
[out-degree](`m:graph#in_degree`) zero.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec sink_vertices(G::graph()) -> [vertex()].
sink_vertices(#graph{vs=Vs, out_es=OutEsMap}) ->
    maps:fold(fun(V, _, Acc) ->
                      case maps:get(V, OutEsMap, []) of
                          [] -> [V | Acc];
                          _ -> Acc
                      end
              end, [], Vs).

-doc "Returns the [in-degree](`m:graph#in_degree`) of vertex `V` of graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec in_degree(G::graph(), V::vertex()) -> non_neg_integer().
in_degree(#graph{in_es=InEsMap}, V) ->
    length(maps:get(V, InEsMap, [])).

-doc """
Returns a list of all [in-neighbors](`m:graph#in_neighbour`) of `V` of graph
`G`, in some unspecified order.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec in_neighbours(G::graph(), V::vertex()) -> [vertex()].
in_neighbours(#graph{in_es=InEsMap}, V) ->
    [From || {From,_,_} <:- maps:get(V, InEsMap, [])].

-doc """
Returns a list of all edges [incident](`m:graph#incident`) on `V` of graph
`G`, in some unspecified order.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec in_edges(G::graph(), V::vertex()) -> [edge()].
in_edges(#graph{in_es=InEsMap}, V) ->
    maps:get(V, InEsMap, []).

-doc "Returns the [out-degree](`m:graph#out_degree`) of vertex `V` of graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec out_degree(G::graph(), V::vertex()) -> non_neg_integer().
out_degree(#graph{out_es=OutEsMap}, V) ->
    length(maps:get(V, OutEsMap, [])).

-doc """
Returns a list of all [out-neighbors](`m:graph#out_neighbour`) of `V` of
graph `G`, in some unspecified order.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec out_neighbours(G::graph(), V::vertex()) -> [vertex()].
out_neighbours(#graph{out_es=OutEsMap}, V) ->
    [To || {_,To,_} <:- maps:get(V, OutEsMap, [])].

-doc """
Returns a list of all edges [emanating](`m:graph#emanate`) from `V` of graph
`G`, in some unspecified order.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec out_edges(G::graph(), V::vertex()) -> [edge()].
out_edges(#graph{out_es=OutEsMap}, V) ->
    maps:get(V, OutEsMap, []).

-doc(#{equiv => add_edge(G, V1, V2, [])}).
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec add_edge(graph(), vertex(), vertex()) -> graph().
add_edge(G, V1, V2) ->
    add_edge(G, V1, V2, []).

-doc """
Creates an edge `{V1, V2, L}` in graph `G`.

The edge is [emanating](`m:graph#emanate`) from `V1` and
[incident](`m:graph#incident`) on `V2`, and has [label](`m:graph#label`)
`L`. The edge is uniquely identified by this triple. A graph can have
multiple edges between the same vertices `V1` and `V2` but only if the
edges have different labels.

If `G` was created with option `acyclic`, then attempting to add an edge that
would introduce a cycle will raise an error `{bad_edge, {From, To}}`. Note
that checking for cyclicity slows down the adding of edges.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec add_edge(G, V1, V2, L) -> graph() when
      G :: graph(),
      V1 :: vertex(),
      V2 :: vertex(),
      L :: label().
add_edge(#graph{cyclic=true}=G, From, To, Label) ->
    do_add_edge(G, From, To, Label);
add_edge(#graph{cyclic=false}=G, From, To, Label) ->
    acyclic_add_edge(G, From, To, Label).

do_add_edge(#graph{vs=Vs}=G, From, To, Label) ->
    maps:is_key(From, Vs) orelse error({bad_vertex, From}),
    maps:is_key(To, Vs) orelse error({bad_vertex, To}),
    #graph{in_es=InEsMap0,out_es=OutEsMap0} = G,
    Name = {From,To,Label},  % note: edge tuple is shared between the maps
    InEsMap = edge_map_add(To, Name, InEsMap0),
    OutEsMap = edge_map_add(From, Name, OutEsMap0),
    G#graph{in_es=InEsMap,out_es=OutEsMap}.

edge_map_add(V, E, EsMap) ->
    Es0 = maps:get(V, EsMap, []),
    Es = ordsets:add_element(E, Es0),
    EsMap#{V=>Es}.

acyclic_add_edge(_G, From, To, _Label) when From =:= To ->
    error({bad_edge, {From,To}});
acyclic_add_edge(G, From, To, Label) ->
    case get_path(G, To, From) of
        false ->
            false = has_path(G, To, From),  % assert - remove me
            do_add_edge(G, From, To, Label);
        _ ->
            true = has_path(G, To, From),  % assert - remove me
            error({bad_edge, {From,To}})
    end.

-doc "Deletes edge `E` from graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec del_edge(G::graph(), E::edge()) -> graph().
del_edge(G, {From,To,_}=E) ->
    #graph{in_es=InEsMap0,out_es=OutEsMap0} = G,
    InEsMap = edge_map_del(To, E, InEsMap0),
    OutEsMap = edge_map_del(From, E, OutEsMap0),
    G#graph{in_es=InEsMap,out_es=OutEsMap}.

edge_map_del(V, E, EsMap) ->
    case maps:find(V, EsMap) of
        error -> EsMap;
        {ok, Es0} ->
            Es = Es0 -- [E],
            EsMap#{V:=Es}
    end.

-doc "Deletes the edges in list `Es` from graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec del_edges(graph(), [edge()]) -> graph().
del_edges(G, Es) when is_list(Es) ->
    foldl(fun(E, A) -> del_edge(A, E) end, G, Es).

-doc "Deletes all edges from vertex `V1` to vertex `V2` in graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec del_edges(graph(), vertex(), vertex()) -> graph().
del_edges(G, V1, V2) ->
    Es = out_edges(G, V1),
    del_edges(G, V1, V2, Es).

del_edges(G, V1, V2, [{Va,Vb,_}=E | Es]) when Va =:= V1, Vb =:= V2->
    del_edges(del_edge(G, E), V1, V2, Es);
del_edges(G, V1, V2, [_|Es]) ->
    del_edges(G, V1, V2, Es);
del_edges(G, _, _, []) -> G.

-doc "Returns the number of edges of graph `G`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec no_edges(G::graph()) -> non_neg_integer().
no_edges(#graph{out_es=OutEsMap}) ->
    maps:fold(fun (_, Es, Acc) -> length(Es) + Acc end,
              0, OutEsMap).

-doc "Returns a list of all edges of graph `G`, in some unspecified order.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec edges(G::graph()) -> [edge()].
edges(#graph{out_es=OutEsMap}) ->
    maps:fold(fun(_, Es, Acc) ->
                      Es ++ Acc
              end, [], OutEsMap).

-doc """
Returns a list of all edges [emanating](`m:graph#emanate`) from or
[incident](`m:graph#incident`) on `V` of graph `G`, in some unspecified
order.

Edges may occur twice in the list. Use `ordsets:from_list/1` on the
result if you need to remove duplicates.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec edges(G::graph(), V::vertex()) -> [edge()].
edges(#graph{in_es=InEsMap, out_es=OutEsMap}, V) ->
    maps:get(V, OutEsMap, [])
        ++ maps:get(V, InEsMap, []).

-doc "Returns the ordered set of edges from V1 to V2.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec edges(G::graph(), V1::vertex(), V2::vertex()) -> ordsets:ordset(edge()).
edges(#graph{out_es=OutEsMap}, V1, V2) ->
    case OutEsMap of
       #{V1 := Es} -> [E || {Va, Vb, _}=E <- Es, Va =:= V1, Vb =:= V2];
       #{} -> []
   end.

-doc """
Returns `true` if and only if `G` contains edge `E`.

Note that the identity of an edge includes its label. To check for an
arbitrary edge between two vertices, use `has_edge/3`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec has_edge(G::graph(), E::edge()) -> boolean().
has_edge(#graph{out_es=OutEsMap}, {V1, _, _}=E) ->
    case OutEsMap of
       #{V1 := Es} -> ordsets:is_element(E, Es);
       #{} -> false
   end.

-doc "Returns `true` if and only if `G` contains some edge from `V1` to `V2`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec has_edge(G::graph(), V1::vertex(), V2::vertex()) -> boolean().
has_edge(#graph{out_es=OutEsMap}, V1, V2) ->
    case OutEsMap of
       #{V1 := Es} -> has_edge_1(Es, V1, V2);
       #{} -> false
   end.

has_edge_1([{V1, V2, _L} | _], V1, V2) ->
    true;
has_edge_1([_ | Es], V1, V2) ->
    has_edge_1(Es, V1, V2);
has_edge_1([], _, _) ->
    false.

-doc "Fold `Fun` over the vertices of graph `G`, in some unspecified order.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec fold_vertices(G, Fun, Acc) -> any() when
      G :: graph(),
      Fun :: fun((vertex(), label(), any()) -> any()),
      Acc :: any().
fold_vertices(#graph{vs=Vs}, Fun, Acc) ->
    maps:fold(Fun, Acc, Vs).

-doc "Returns `true` if and only if `G` contains vertex `V`.".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec has_vertex(G::graph(), V::vertex()) -> boolean().
has_vertex(#graph{vs=Vs}, V) ->
    is_map_key(V, Vs).

-doc """
Returns `true` if and only if there is a [path](`m:graph#path`) in `G` from
vertex `V1` to vertex `V2`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec has_path(G::graph(), V1::vertex(), V2::vertex()) -> boolean().
has_path(G, From, To) ->
    Seen = sets:new(),
    try
        _ = has_path_1([From], To, G, Seen),
        false
    catch
        throw:true ->
            true
    end.

has_path_1([To|_], To, _G, _Seen) ->
    throw(true);
has_path_1([V|Vs], To, G, Seen0) ->
    case sets:is_element(V, Seen0) of
        true ->
            has_path_1(Vs, To, G, Seen0);
        false ->
            Seen1 = sets:add_element(V, Seen0),
            Successors = out_neighbours(G, V),
            Seen = has_path_1(Successors, To, G, Seen1),
            has_path_1(Vs, To, G, Seen)
    end;
has_path_1([], _To, _G, Seen) ->
    Seen.

-doc """
Deletes edges from graph `G` until there are no [paths](`m:graph#path`) from
vertex `V1` to vertex `V2`.

A sketch of the procedure employed:

- Find an arbitrary [simple path](`m:graph#simple_path`)
  v\[1], v\[2], ..., v\[k] from `V1` to `V2` in `G`.
- Remove all edges of `G` [emanating](`m:graph#emanate`) from v\[i] and
  [incident](`m:graph#incident`) to v\[i+1] for 1 <= i < k (including multiple
  edges).
- Repeat until there is no path between `V1` and `V2`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec del_path(graph(), vertex(), vertex()) -> graph().
del_path(G, V1, V2) ->
    case get_path(G, V1, V2) of
        false -> G;
        Path ->
            del_path(del_path_edges(G, Path), V1, V2)
    end.

del_path_edges(G, [V1, V2 | Vs]) ->
    del_path_edges(del_edges(G, V1, V2), [V2 | Vs]);
del_path_edges(G, _) -> G.

-doc """
Tries to find a cycle in `G` which includes vertex `V`.

If a [simple cycle](`m:graph#simple_cycle`) of length two or more exists
through vertex `V`, the cycle is returned as a list `[V, ..., V]` of vertices.
If a [loop](`m:graph#loop`) through `V` exists, the loop is returned as a list
`[V]`. If no cycles through `V` exist, `false` is returned.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec get_cycle(graph(), vertex()) -> [vertex(),...] | 'false'.
get_cycle(G, V) ->
    case one_path(out_neighbours(G, V), V, [], [V], [V], 2, G, 1) of
        false ->
            case lists:member(V, out_neighbours(G, V)) of
                true -> [V];
                false -> false
            end;
        Vs -> Vs
    end.

-doc """
Tries to find a [simple path](`m:graph#simple_path`) from vertex `V1` to
vertex `V2` of graph `G`.

Returns the path as a list `[V1, ..., V2]` of vertices, or `false` if no
simple path from `V1` to `V2` of length one or more exists.

The graph is traversed in a depth-first manner, and the first found path is
returned.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec get_path(graph(), vertex(), vertex()) -> [vertex(),...] | 'false'.
get_path(G, V1, V2) ->
    one_path(out_neighbours(G, V1), V2, [], [V1], [V1], 1, G, 1).

one_path([W|Ws], W, Cont, Xs, Ps, Prune, G, Counter) ->
    if Counter < Prune -> one_path(Ws, W, Cont, Xs, Ps, Prune, G, Counter);
        true -> reverse([W|Ps])
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

-doc """
Like `get_cycle/2`, but a cycle of length one is preferred.

Tries to find an as short as possible [simple
cycle](`m:graph#simple_cycle`) through vertex `V` of graph `G`. Returns
the cycle as a list `[V, ..., V]` of vertices, or `false` if no simple
cycle through `V` exists. Notice that a [loop](`m:graph#loop`) through
`V` is returned as list `[V, V]`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec get_short_cycle(graph(), vertex()) -> [vertex(),...] | 'false'.
get_short_cycle(G, V) ->
    get_short_path(G, V, V).

-doc """
Like `get_path/3`, but using a breadth-first search to find a short path.

Tries to find an as short as possible [simple path](`m:graph#simple_path`)
from vertex `V1` to vertex `V2` of graph `G`. Returns the path as a list
`[V1, ..., V2]` of vertices, or `false` if no simple path from `V1` to `V2` of
length one or more exists.

Graph `G` is traversed in a breadth-first manner, and the first found path is
returned.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec get_short_path(graph(), vertex(), vertex()) -> [vertex(),...] | 'false'.
get_short_path(G, V1, V2) ->
    T = new(),
    T1 = add_vertex(T, V1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(V1, G, Q),
    spath(Q1, G, V2, T1).

spath(Q, G, Sink, T) ->
    case queue:out(Q) of
        {{value, {V1, V2, _Label}}, Q1} ->
            if
                Sink =:= V2 ->
                    follow_path(V1, T, [V2]);
                true ->
                    case has_vertex(T, V2) of
                        false ->
                            T1 = add_vertex(T, V2),
                            T2 = add_edge(T1, V2, V1),
                            NQ = queue_out_neighbours(V2, G, Q1),
                            spath(NQ, G, Sink, T2);
                        true ->
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
    foldl(fun(E, Q) -> queue:in(E, Q) end, Q0, out_edges(G, V)).

-if(?DEBUG).

%%
%% Dumps the graph as a string in dot (graphviz) format.
%%
%% Use dot(1) to convert to an image:
%%
%%    dot [input] -T[format]
%%    dot graph_file -Tsvg > graph.svg

-spec dump(any()) -> any().
dump(G) ->
    Formatter = fun(Node) -> io_lib:format("~p", [Node]) end,
    io:format("~s", [dump_1(G, Formatter)]).

-spec dump(any(), any()) -> any().
dump(G, FileName) ->
    Formatter = fun(Node) -> io_lib:format("~p", [Node]) end,
    dump(G, FileName, Formatter).

-spec dump(any(), any(), any()) -> any().
dump(G, FileName, Formatter) ->
    {ok, Fd} = file:open(FileName, [write]),
    io:fwrite(Fd, "~s", [dump_1(G, Formatter)]),
    file:close(Fd).

dump_1(G, Formatter) ->
    Vs = maps:keys(G#graph.vs),

    {Map, Vertices} = dump_vertices(Vs, 0, Formatter,#{}, []),
    Edges = dump_edges(Vs, G, Map, []),

    io_lib:format("graph g {~n~s~n~s~n}~n", [Vertices, Edges]).

dump_vertices([V | Vs], Counter, Formatter, Map, Acc) ->
    VerticeSlug = io_lib:format("    ~p [label=\"~s\"]~n",
                                [Counter, Formatter(V)]),
    dump_vertices(Vs, Counter + 1, Formatter,
                  Map#{ V => Counter }, [VerticeSlug | Acc]);
dump_vertices([], _Counter, _Formatter, Map, Acc) ->
    {Map, Acc}.

dump_edges([V | Vs], G, Map, Acc) ->
    SelfId = map_get(V, Map),
    EdgeSlug = [io_lib:format("    ~p -> ~p~n", [SelfId, map_get(To, Map)]) ||
                {_, To, _} <- out_edges(G, V)],
    dump_edges(Vs, G, Map, [EdgeSlug | Acc]);
dump_edges([], _G, _Map, Acc) ->
    Acc.

-endif.


%% ------------------------------------------------------------------------
%% Graph utilities

%%% Operations on directed (and undirected) graphs.
%%%
%%% Implementation based on Launchbury, John: Graph Algorithms with a
%%% Functional Flavour, in Jeuring, Johan, and Meijer, Erik (Eds.):
%%% Advanced Functional Programming, Lecture Notes in Computer
%%% Science 925, Springer Verlag, 1995.

%%
%%  Exported functions
%%

-doc """
Returns a list of [connected components](`m:graph#components`).

Each component is represented by its vertices. The order of the vertices
and the order of the components are arbitrary. Each vertex of graph `G`
occurs in exactly one component.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec components(graph()) -> [[vertex()]].
components(G) ->
    revpreorders(G, fun inout/3).

-doc """
Returns a list of [strongly connected
components](`m:graph#strong_components`).

Each strongly component is represented by its vertices. The order of the
vertices and the order of the components are arbitrary. Each vertex of
graph `G` occurs in exactly one strong component.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec strong_components(graph()) -> [[vertex()]].
strong_components(G) ->
    revpreorders(G, fun in/3, reverse_postorder(G, vertices(G))).

-doc """
Returns a list of cyclic [strongly connected
components](`m:graph#strong_components`).

Each strongly component is represented by its vertices. The order of the
vertices and the order of the components are arbitrary. Only vertices
that are included in some [cycle](`m:graph#cycle`) in `G` are returned,
otherwise the returned list is equal to that returned by
`strong_components/1`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec cyclic_strong_components(graph()) -> [[vertex()]].
cyclic_strong_components(G) ->
    remove_singletons(strong_components(G), G, []).

-doc """
Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) in `G` from some vertex of `Vs` to
the vertex.

In particular, as paths can have length zero, the vertices of `Vs` are all
included in the returned list.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec reachable(graph(), [vertex()]) -> [vertex()].
reachable(G, Vs) when is_list(Vs) ->
    lists:append(revpreorders(G, fun out/3, Vs, first)).

-doc """
Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) in `G` of length one or more from
some vertex of `Vs` to the vertex.

Hence, vertices in `Vs` will only be included in the result if they are part
of some [cycle](`m:graph#cycle`).
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec reachable_via_neighbours(graph(), [vertex()]) -> [vertex()].
reachable_via_neighbours(G, Vs) when is_list(Vs) ->
    lists:append(revpreorders(G, fun out/3, Vs, not_first)).

-doc """
Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) from the vertex to some vertex of
`Vs`.

In particular, as paths can have length zero, the vertices of `Vs` are all
included in the returned list.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec reaching(graph(), [vertex()]) -> [vertex()].
reaching(G, Vs) when is_list(Vs) ->
    lists:append(revpreorders(G, fun in/3, Vs, first)).

-doc """
Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) of length one or more from the
vertex to some vertex of `Vs`.

Hence, vertices in `Vs` will only be included in the result if they are part
of some [cycle](`m:graph#cycle`).
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec reaching_via_neighbours(graph(), [vertex()]) -> [vertex()].
reaching_via_neighbours(G, Vs) when is_list(Vs) ->
    lists:append(revpreorders(G, fun in/3, Vs, not_first)).

-doc """
Returns a [topological ordering](`m:graph#topsort`) of the vertices of graph
`G` if such an ordering exists, otherwise `false`.

For each vertex in the returned list, no
[out-neighbors](`m:graph#out_neighbour`) occur earlier in the list.

This is currently implemented simply as `reverse_postorder(G)`, but this
detail is subject to change and should not be relied on.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec topsort(graph()) -> [vertex()].
topsort(G) ->
    reverse_postorder(G).

-doc """
Returns `true` if and only if graph `G` is
[acyclic](`m:graph#acyclic_graph`).
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec is_acyclic(graph()) -> boolean().
is_acyclic(G) ->
    cyclic_strong_components(G) =:= [].

-doc """
Returns a minimal list of vertices of `G` from which all vertices of `G` can
be reached.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec roots(graph()) -> [vertex()].
roots(G) ->
    R1 = [V || V <- vertices(G), in_degree(G, V) =:= 0],
    R2 = [X || [X|_] <- cyclic_strong_components(G)],
    R1 ++ R2.

-doc """
Returns `{yes, V}` if `G` is an [arborescence](`m:graph#arborescence`) (a
directed tree) with vertex `V` as the [root](`m:graph#root`), otherwise `no`.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec arborescence_root(graph()) -> 'no' | {'yes', vertex()}.
arborescence_root(G) ->
    case no_edges(G) =:= no_vertices(G) - 1 of
        true ->
            try
                F = fun(V, Z) ->
                            case in_degree(G, V) of
                                1 -> Z;
                                0 when Z =:= [] -> [V]
                            end
                    end,
                [Root] = foldl(F, [], vertices(G)),
                {yes, Root}
            catch _:_ ->
                no
            end;
        false ->
            no
    end.

-doc """
Returns `true` if and only if graph `G` is an
[arborescence](`m:graph#arborescence`) (a directed tree with a unique root).
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec is_arborescence(graph()) -> boolean().
is_arborescence(G) ->
    arborescence_root(G) =/= no.

-doc """
Returns `true` if and only if graph `G` is a
[tree](`m:graph#tree`), considering all edges undirected.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec is_tree(graph()) -> boolean().
is_tree(G) ->
    (no_edges(G) =:= no_vertices(G) - 1)
    andalso (length(components(G)) =:= 1).

-doc """
Returns a list of all vertices of `G` that are included in some
[loop](`m:graph#loop`).
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec loop_vertices(graph()) -> [vertex()].
loop_vertices(G) ->
    [V || V <- vertices(G), is_reflexive_vertex(G, V)].

-doc(#{equiv => subgraph(G, Vs, [])}).
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec subgraph(graph(), [vertex()]) -> graph().
subgraph(G, Vs) ->
    try
        subgraph_opts(G, Vs, [])
    catch
        throw:badarg ->
            erlang:error(badarg)
    end.

-doc """
Creates a maximal [subgraph](`m:graph#subgraph`) of `G` restricted to the
vertices listed in `Vs`.

If the value of option `type` is `inherit`, which is the default, the type
of `G` is used for the subgraph as well (for example, whether the graph
allows cycles). Otherwise the value of the `type` option is used as argument
to `new/1`.

If the value of option `keep_labels` is `true`, which is the default, the
[labels](`m:graph#label`) of vertices and edges of `G` are used for the
subgraph as well. If the value is `false`, the vertices and edges of the
subgraph will have the default labels.

If any of the arguments are invalid, a `badarg` exception is raised.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec subgraph(graph(), [vertex()], Options) -> graph() when
      Options :: [{'type', SubgraphType} | {'keep_labels', boolean()}],
      SubgraphType :: 'inherit' | [graph_type()].
subgraph(G, Vs, Options) ->
    try
        subgraph_opts(G, Vs, Options)
    catch
        throw:badarg ->
            erlang:error(badarg)
    end.

-doc """
Creates a graph where the vertices are the [strongly connected
components](`m:graph#strong_components`) of `G` as returned by
`strong_components/1`.

If X and Y are two different strongly connected components, and vertices x
and y exist in X and Y, respectively, such that there is an edge
[emanating](`m:graph#emanate`) from x and [incident](`m:graph#incident`) on
y, then an edge emanating from X and incident on Y is created.

The created graph has the same type as `G`. All vertices and edges have
the default [label](`m:graph#label`) `[]`.

Each [cycle](`m:graph#cycle`) is included in some strongly connected
component, which implies that a
[topological ordering](`m:graph#topsort`) of the created graph always
exists.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec condensation(graph()) -> graph().
condensation(G) ->
    SCs = strong_components(G),
    %% Each component is assigned a number.
    %% V2I: from vertex to number.
    %% I2C: from number to component.
    CFun = fun(SC, {N, V2I0, I2C0}) ->
                   V2I1 = foldl(fun(V, Map) -> Map#{V => N} end,
                                V2I0,
                                SC),
                   I2C1 = I2C0#{N => SC},
                   {N + 1, V2I1, I2C1}
           end,
    {_, V2I, I2C} = foldl(CFun, {1, #{}, #{}}, SCs),
    G0 = subgraph_opts(G, [], []),
    foldl(fun(SC, SCG) -> condense(SC, G, SCG, V2I, I2C) end,
          G0, SCs).

-doc(#{ equiv => preorder(G, roots(G)) }).
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec preorder(graph()) -> [vertex()].
preorder(G) ->
    preorder(G, roots(G)).

-doc """
Returns all vertices of graph `G` reachable from `Vs`, listed in pre-order.

The order is given by a [depth-first
traversal](`m:graph#depth_first_traversal`) of the graph, collecting visited
vertices in preorder.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec preorder(graph(), [vertex()]) -> [vertex()].
preorder(G, Vs) ->
    T = sets:new(),
    {_, Acc} = pretraverse_1(Vs, fun out/3, G, T, [], []),
    reverse(lists:append(Acc)).

-doc(#{ equiv => postorder(G, roots(G)) }).
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec postorder(graph()) -> [vertex()].
postorder(G) ->
    postorder(G, roots(G)).

-doc """
Returns the vertices of graph `G` reachable from `Vs`, listed in post-order.

The order is given by a [depth-first
traversal](`m:graph#depth_first_traversal`) of the graph, collecting visited
vertices in postorder. More precisely, the vertices visited while searching
from an arbitrarily chosen vertex are collected in postorder, and all those
collected vertices are placed before the subsequently visited vertices.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec postorder(graph(), [vertex()]) -> [vertex()].
postorder(G, Vs) ->
    T = sets:new(),
    {Acc, _} = posttraverse(Vs, G, T, []),
    reverse(Acc).

-doc(#{ equiv => reverse_postorder(G, roots(G)) }).
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec reverse_postorder(graph()) -> [vertex()].
reverse_postorder(G) ->
    reverse_postorder(G, roots(G)).

-doc """
Returns the vertices of graph `G` reachable from `Vs`, listed in reverse
post-order.

This effectively performs a topological sort of the reachable nodes.

The graph is traversed as for `postorder/2`, but producing the result in
reverse order.
""".
-doc(#{ since => ~"OTP @OTP-19922@"}).
-spec reverse_postorder(graph(), [vertex()]) -> [vertex()].
reverse_postorder(G, Vs) ->
    T = sets:new(),
    {L, _} = posttraverse(Vs, G, T, []),
    L.

%%
%%  Local functions
%%

revpreorders(G, SF) ->
    revpreorders(G, SF, vertices(G)).

revpreorders(G, SF, Vs) ->
    revpreorders(G, SF, Vs, first).

%% returns a list of reverse preorder traversals using the given Vs as
%% starting points (if a starting point V has already become visited by
%% a previous traversal it will not be included again)
revpreorders(G, SF, Vs, HandleFirst) ->
    T = sets:new(),
    F = fun(V, {T0, LL}) -> pretraverse(HandleFirst, V, SF, G, T0, LL) end,
    {_, LL} = foldl(F, {T, []}, Vs),
    LL.

pretraverse(first, V, SF, G, T, LL) ->
    pretraverse_1([V], SF, G, T, [], LL);
pretraverse(not_first, V, SF, G, T, LL) ->
    %% used by reachable_neighbours/2 and reaching_neighbours/2
    case sets:is_element(V, T) of
        false -> pretraverse_1(SF(G, V, []), SF, G, T, [], LL);
        true  -> {T, LL}
    end.

%% generic preorder traversal loop; given a starting set Vs of vertices
%% of G, T tracks seen vertices, the SF function queues up neighbour
%% vertexes, and the resulting list Rs of reached vertices (in reverse
%% preorder) is prepended onto LL unless it is empty
pretraverse_1([V | Vs], SF, G, T0, Rs, LL) ->
    case sets:is_element(V, T0) of
        false ->
            T1 = sets:add_element(V, T0),
            pretraverse_1(SF(G, V, Vs), SF, G, T1, [V | Rs], LL);
        true ->
            pretraverse_1(Vs, SF, G, T0, Rs, LL)
    end;
pretraverse_1([], _SF, _G, T, [], LL) ->
    {T, LL};
pretraverse_1([], _SF, _G, T, Rs, LL) ->
    {T, [Rs | LL]}.

%% similar to pretraverse_1 but accumulates onto a single list, in reverse
%% postorder, and only for out-edges
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
    in_neighbours(G, V) ++ Vs.

out(G, V, Vs) ->
    out_neighbours(G, V) ++ Vs.

inout(G, V, Vs) ->
    in(G, V, out(G, V, Vs)).

remove_singletons([C=[V] | Cs], G, L) ->
    case is_reflexive_vertex(G, V) of
        true  -> remove_singletons(Cs, G, [C | L]);
        false -> remove_singletons(Cs, G, L)
    end;
remove_singletons([C | Cs], G, L) ->
    remove_singletons(Cs, G, [C | L]);
remove_singletons([], _G, L) ->
    L.

is_reflexive_vertex(G, V) ->
    lists:member(V, out_neighbours(G, V)).

subgraph_opts(G, Vs, Opts) ->
    subgraph_opts(Opts, inherit, true, G, Vs).

subgraph_opts([{type, Type} | Opts], _Type0, Keep, G, Vs)
  when Type =:= inherit; is_list(Type) ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([{keep_labels, Keep} | Opts], Type, _Keep0, G, Vs)
  when is_boolean(Keep) ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([], inherit, Keep, G, Vs) ->
    Info = info(G),
    {_, {_, Cyclicity}} = lists:keysearch(cyclicity, 1, Info),
    subgraph(G, Vs, [Cyclicity], Keep);
subgraph_opts([], Type, Keep, G, Vs) ->
    subgraph(G, Vs, Type, Keep);
subgraph_opts(_, _Type, _Keep, _G, _Vs) ->
    throw(badarg).

subgraph(G, Vs, Type, Keep) ->
    try new(Type) of
        SG0 ->
            SG1 = foldl(fun(V, SG) -> subgraph_vertex(V, G, SG, Keep) end,
                        SG0, Vs),
            EFun = fun(V, SGv) -> foldl(fun(E, SG) ->
                                                subgraph_edge(E, SG, Keep)
                                        end,
                                        SGv,
                                        out_edges(G, V))
                   end,
            foldl(EFun, SG1, vertices(SG1))
    catch
        error:badarg ->
            throw(badarg)
    end.

subgraph_vertex(V, G, SG, Keep) ->
    case has_vertex(G, V) of
        false -> SG;
        true when not Keep -> add_vertex(SG, V);
        true when Keep -> add_vertex(SG, V, vertex(G, V))
    end.

subgraph_edge({V1, V2, Label}, SG, Keep) ->
    case has_vertex(SG, V2) of
        false -> SG;
        true when not Keep -> add_edge(SG, V1, V2);
        true when Keep -> add_edge(SG, V1, V2, Label)
    end.

condense(SC, G, SCG, V2I, I2C) ->
    NFun = fun(Neighbour, T0) ->
                   I = maps:get(Neighbour, V2I),
                   T0#{I => true}
           end,
    VFun = fun(V, T0) -> foldl(NFun, T0, out_neighbours(G, V)) end,
    T = foldl(VFun, #{}, SC),
    maps:fold(fun (I, true, SCG0) ->
                      C = maps:get(I, I2C),
                      SCG1 = add_vertex(SCG0, C),
                      if C =/= SC ->
                              add_edge(SCG1, SC, C);
                         true ->
                              SCG1
                      end
              end,
              add_vertex(SCG, SC),
              T).
