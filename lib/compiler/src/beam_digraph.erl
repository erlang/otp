
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2024. All Rights Reserved.
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
%% Digraph data type. Similar to the digraph module, but provides a
%% functional API. The functional API allows us to revert to a
%% previous version of the digraph when an optimization that may have
%% damaged the digraph has failed.
%%

-module(beam_digraph).
-moduledoc false.

-export([new/0,
         add_vertex/2, add_vertex/3, add_edge/3, add_edge/4,
         del_edge/2, del_edges/2,
         foldv/3,
         has_vertex/2,
         is_path/3,
         in_degree/2, in_edges/2, in_neighbours/2,
         no_vertices/1,
         out_degree/2, out_edges/2, out_neighbours/2,
         vertex/2, vertices/1,
         reverse_postorder/2,
         roots/1,
         topsort/1,
         strong_components/2]).

%% Debugging.
-define(DEBUG, false).
-if(?DEBUG).
-export([dump/1,dump/2,dump/3]).
-endif.

-import(lists, [foldl/3, reverse/1]).

-type edge_map() :: #{ vertex() => ordsets:ordset(vertex()) }.
-type vertice_map() :: #{ vertex() => label() }.

-record(dg, {vs = #{} :: vertice_map(),
             in_es = #{} :: edge_map(),
             out_es = #{} :: edge_map()}).

-type graph() :: #dg{}.

-type vertex() :: term().
-type label() :: term().
-type edge() :: {vertex(), vertex(), label()}.

-spec new() -> graph().
new() -> #dg{}.

-spec add_vertex(graph(), vertex()) -> graph().
add_vertex(Dg, V) ->
    add_vertex(Dg, V, vertex).

-spec add_vertex(graph(), vertex(), label()) -> graph().
add_vertex(Dg, V, Label) ->
    #dg{vs=Vs0} = Dg,
    Vs = Vs0#{V=>Label},
    Dg#dg{vs=Vs}.

-spec add_edge(graph(), vertex(), vertex()) -> graph().
add_edge(Dg, From, To) ->
    add_edge(Dg, From, To, edge).

-spec add_edge(graph(), vertex(), vertex(), label()) -> graph().
add_edge(Dg, From, To, Label) ->
    #dg{in_es=InEsMap0,out_es=OutEsMap0} = Dg,
    Name = {From,To,Label},
    InEsMap = edge_map_add(To, Name, InEsMap0),
    OutEsMap = edge_map_add(From, Name, OutEsMap0),
    Dg#dg{in_es=InEsMap,out_es=OutEsMap}.

edge_map_add(V, E, EsMap) ->
    Es0 = maps:get(V, EsMap, []),
    Es = ordsets:add_element(E, Es0),
    EsMap#{V=>Es}.

-spec del_edge(graph(), edge()) -> graph().
del_edge(Dg, {From,To,_}=E) ->
    #dg{in_es=InEsMap0,out_es=OutEsMap0} = Dg,
    InEsMap = edge_map_del(To, E, InEsMap0),
    OutEsMap = edge_map_del(From, E, OutEsMap0),
    Dg#dg{in_es=InEsMap,out_es=OutEsMap}.

edge_map_del(V, E, EsMap) ->
    Es0 = maps:get(V, EsMap, []),
    Es = Es0 -- [E],
    EsMap#{V:=Es}.

-spec del_edges(graph(), [edge()]) -> graph().
del_edges(G, Es) when is_list(Es) ->
    foldl(fun(E, A) -> del_edge(A, E) end, G, Es).

%% Fold over the vertices of the graph, the order is unspecified.
-spec foldv(graph(), fun((vertex(), label(), any()) -> any()), any()) -> any().
foldv(#dg{vs=Vs}, Fun, Acc) ->
    maps:fold(Fun, Acc, Vs).

-spec has_vertex(graph(), vertex()) -> boolean().
has_vertex(#dg{vs=Vs}, V) ->
    is_map_key(V, Vs).

-spec in_degree(graph(), vertex()) -> non_neg_integer().
in_degree(#dg{in_es=InEsMap}, V) ->
    length(maps:get(V, InEsMap, [])).

-spec in_edges(graph(), vertex()) -> [edge()].
in_edges(#dg{in_es=InEsMap}, V) ->
    maps:get(V, InEsMap, []).

-spec in_neighbours(graph(), vertex()) -> [vertex()].
in_neighbours(#dg{in_es=InEsMap}, V) ->
    [From || {From,_,_} <- maps:get(V, InEsMap, [])].

-spec is_path(graph(), vertex(), vertex()) -> boolean().
is_path(G, From, To) ->
    Seen = sets:new([{version, 2}]),
    try
        _ = is_path_1([From], To, G, Seen),
        false
    catch
        throw:true ->
            true
    end.

is_path_1([To|_], To, _G, _Seen) ->
    throw(true);
is_path_1([V|Vs], To, G, Seen0) ->
    case sets:is_element(V, Seen0) of
        true ->
            is_path_1(Vs, To, G, Seen0);
        false ->
            Seen1 = sets:add_element(V, Seen0),
            Successors = out_neighbours(G, V),
            Seen = is_path_1(Successors, To, G, Seen1),
            is_path_1(Vs, To, G, Seen)
    end;
is_path_1([], _To, _G, Seen) ->
    Seen.

-spec out_degree(graph(), vertex()) -> non_neg_integer().
out_degree(#dg{out_es=OutEsMap}, V) ->
    length(maps:get(V, OutEsMap, [])).

-spec out_edges(graph(), vertex()) -> [edge()].
out_edges(#dg{out_es=OutEsMap}, V) ->
    maps:get(V, OutEsMap, []).

-spec out_neighbours(graph(), vertex()) -> [vertex()].
out_neighbours(#dg{out_es=OutEsMap}, V) ->
    [To || {_,To,_} <- maps:get(V, OutEsMap, [])].

-spec no_vertices(graph()) -> non_neg_integer().
no_vertices(#dg{vs=Vs}) ->
    map_size(Vs).

-spec vertex(graph(), vertex()) -> label().
vertex(#dg{vs=Vs}, V) ->
    map_get(V, Vs).

-spec vertices(graph()) -> [{vertex(), label()}].
vertices(#dg{vs=Vs}) ->
    maps:to_list(Vs).

-spec reverse_postorder(graph(), [vertex()]) -> [vertex()].
reverse_postorder(G, Vs) ->
    Seen = sets:new([{version, 2}]),
    {RPO, _} = reverse_postorder_1(Vs, G, Seen, []),
    RPO.

reverse_postorder_1([V|Vs], G, Seen0, Acc0) ->
    case sets:is_element(V, Seen0) of
        true ->
            reverse_postorder_1(Vs, G, Seen0, Acc0);
        false ->
            Seen1 = sets:add_element(V, Seen0),
            Successors = out_neighbours(G, V),
            {Acc,Seen} = reverse_postorder_1(Successors, G, Seen1, Acc0),
            reverse_postorder_1(Vs, G, Seen, [V|Acc])
    end;
reverse_postorder_1([], _, Seen, Acc) ->
    {Acc, Seen}.

-spec roots(graph()) -> [vertex()].
roots(G) ->
    roots_1(vertices(G), G).

roots_1([{V,_}|Vs], G) ->
    case in_degree(G, V) of
        0 ->
            [V|roots_1(Vs, G)];
        _ ->
            roots_1(Vs, G)
    end;
roots_1([], _G) -> [].

-spec topsort(graph()) -> [vertex()].
topsort(G) ->
    Seen = roots(G),
    reverse_postorder(G, Seen).

%%
%% Kosaraju's algorithm
%%
%% Visit each node in reverse post order. If the node has not been assigned to
%% a component yet, start a new component and add all of its in-neighbors to it
%% if they don't yet belong to one. Keep going until all nodes have been
%% visited.
%%
%% https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
%%

-spec strong_components(graph(), [vertex()]) -> ComponentMap when
      %% Vertices together with their components.
      ComponentMap :: #{ vertex() => [vertex()] }.
strong_components(G, Vs) ->
    sc_1(Vs, G, #{}, #{}).

sc_1([V | Vs], G, Roots0, Components) when not is_map_key(V, Roots0) ->
    %% V has not been assigned to a component, start a new one with this one as
    %% the root.
    {Roots, Component} = sc_2([V], G, V, Roots0, []),
    sc_1(Vs, G, Roots, Components#{ V => Component });
sc_1([V | Vs], G, Roots, Components0) ->
    %% V is already part of a component, copy it over.
    Root = map_get(V, Roots),
    Components = Components0#{ V => map_get(Root, Components0) },

    sc_1(Vs, G, Roots, Components);
sc_1([], _G, _Roots, Components) ->
    Components.

sc_2([V | Vs], G, Root, Roots, Acc) when not is_map_key(V, Roots) ->
    %% V has not been assigned to a component, so assign it to the current one.
    sc_2(in_neighbours(G, V) ++ Vs, G, Root, Roots#{ V => Root }, [V | Acc]);
sc_2([_V | Vs], G, Root, Roots, Acc) ->
    %% V is already part of a component, skip it.
    sc_2(Vs, G, Root, Roots, Acc);
sc_2([], _G, _Root, Roots, Acc) ->
    {Roots, reverse(Acc)}.

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
    Vs = maps:keys(G#dg.vs),

    {Map, Vertices} = dump_vertices(Vs, 0, Formatter,#{}, []),
    Edges = dump_edges(Vs, G, Map, []),

    io_lib:format("digraph g {~n~s~n~s~n}~n", [Vertices, Edges]).

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
