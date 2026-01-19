%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2026. All Rights Reserved.
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
-module(graph_SUITE).

%%-define(STANDALONE,1).

-ifdef(STANDALONE).
-define(line, put(line, ?LINE), ).
-else.
-include_lib("common_test/include/ct.hrl").
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
         init_per_group/2,end_per_group/2]).

-export([opts/1, degree/1, path/1, cycle/1, vertices/1,
         edges/1, data/1, otp_3522/1, otp_3630/1, otp_8066/1, vertex_names/1]).

-export([simple/1, loop/1, roots/1, isolated/1, topsort/1, subgraph/1,
         condensation/1, tree/1, traversals/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [opts, degree, path, cycle, {group, misc},
     {group, tickets}, {group, utils}].

groups() ->
    [{misc, [], [vertices, edges, data, vertex_names]},
     {utils, [], [simple, loop, roots, isolated, topsort, subgraph,
                  condensation, tree, traversals]},
     {tickets, [], [otp_3522, otp_3630, otp_8066]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

opts(Config) when is_list(Config) ->
    Template = [{v1,[v2]}, {v2,[v3]}, {v3,[v4]}, {v4,[]}],
    G4 = build_graph([], Template),
    graph:add_edge(G4, v4, v1, []),
    G5 = build_graph([cyclic], Template),
    graph:add_edge(G5, v4, v1, []),
    G6 = build_graph([acyclic], Template),
    acyclic = info(G6, cyclicity),
    try graph:add_edge(G6, v4, v1) of
        _ -> error(cycle_not_detected)
    catch
        error: {bad_edge,{v4,v1}} -> ok
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

degree(Config) when is_list(Config) ->
    G = build_graph([], [{x1,[]}, {x2,[x1]}, {x3,[x1,x2]},
                         {x4,[x1,x2,x3]}, {x5,[x1,x2,x3,x4]}]),
    %% out degree
    0 = graph:out_degree(G, x1),
    1 = graph:out_degree(G, x2),
    2 = graph:out_degree(G, x3),
    3 = graph:out_degree(G, x4),
    4 = graph:out_degree(G, x5),
    %% out neighbours
    [] = check(graph:out_neighbours(G, x1), []),
    [] = check(graph:out_neighbours(G, x2), [x1]),
    [] = check(graph:out_neighbours(G, x3), [x1,x2]),
    [] = check(graph:out_neighbours(G, x4), [x1,x2,x3]),
    [] = check(graph:out_neighbours(G, x5), [x1,x2,x3,x4]),

    %% in degree
    4 = graph:in_degree(G, x1),
    3 = graph:in_degree(G, x2),
    2 = graph:in_degree(G, x3),
    1 = graph:in_degree(G, x4),
    0 = graph:in_degree(G, x5),
    %% in neighbours
    [] = check(graph:in_neighbours(G, x1), [x2,x3,x4,x5]),
    [] = check(graph:in_neighbours(G, x2), [x3,x4,x5]),
    [] = check(graph:in_neighbours(G, x3), [x4,x5]),
    [] = check(graph:in_neighbours(G, x4), [x5]),
    [] = check(graph:in_neighbours(G, x5), []),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path(Config) when is_list(Config) ->
    G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
                         {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7]}]),
    {G1, Vi} = case graph:get_path(G, x1, x7) of
             [x1,x2,x4,x5,x7] -> {graph:del_vertex(G, x5), x6};
             [x1,x2,x4,x6,x7] -> {graph:del_vertex(G, x6), x5};
             [x1,x3,x4,x5,x7] -> {graph:del_vertex(G, x5), x6};
             [x1,x3,x4,x6,x7] -> {graph:del_vertex(G, x6), x5}
         end,
    {G2, Vj} = case graph:get_path(G1, x1, x7) of
             [x1,x2,x4,Vi,x7] -> {graph:del_vertex(G1,x2), x3};
             [x1,x3,x4,Vi,x7] -> {graph:del_vertex(G1,x3), x2}
         end,
    [x1,Vj,x4,Vi,x7] = graph:get_path(G2, x1, x7),
    G3 = graph:del_vertex(G2, Vj),
    false = graph:get_path(G3, x1, x7),
    [] = check(graph:vertices(G3), [x1,x4,Vi,x7]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle(Config) when is_list(Config) ->
    G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
                         {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7,x8]},
                         {x8,[x3,x8]}]),
    false = graph:get_cycle(G, x1),
    false = graph:get_cycle(G, x2),
    false = graph:get_cycle(G, x5),
    false = graph:get_cycle(G, x7),
    [x3,x4,x6,x8,x3] = graph:get_cycle(G, x3),
    [x4,x6,x8,x3,x4] = graph:get_cycle(G, x4),
    [x6,x8,x3,x4,x6] = graph:get_cycle(G, x6),
    [x8,x3,x4,x6,x8] = graph:get_cycle(G, x8),
    G1 = graph:del_vertex(G, x4),
    [x8] = graph:get_cycle(G1, x8),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



vertices(Config) when is_list(Config) ->
    G = build_graph([], [{x,[]}, {y,[]}]),
    [] = check(graph:vertices(G), [x,y]),
    G1 = graph:del_vertices(G, [x,y]),
    [] = graph:vertices(G1),
    ok.

edges(Config) when is_list(Config) ->
    G = build_graph([], [{x, [{exy,y},{exx,x}]},
                         {y, [{eyx,x}]}
                        ]),
    [] = check(labels(graph:edges(G)), [exy, eyx, exx]),
    [] = check(labels(graph:out_edges(G, x)), [exy,exx]),
    [] = check(labels(graph:in_edges(G, x)), [eyx,exx]),
    [] = check(labels(graph:out_edges(G, y)), [eyx]),
    [] = check(labels(graph:in_edges(G, y)), [exy]),
    G1 = del_edges(G, [exy, eyx, does_not_exist]),
    [exx] = labels(graph:edges(G1)),
    [] = check(labels(graph:out_edges(G1, x)), [exx]),
    [] = check(labels(graph:in_edges(G1, x)), [exx]),
    [] = check(labels(graph:out_edges(G1, y)), []),
    [] = check(labels(graph:in_edges(G1, y)), []),
    G2 = graph:del_vertices(G1, [x,y]),
    [] = graph:edges(G2),
    [] = graph:vertices(G2),
    ok.

del_edges(G, Ls) ->
    Es = lists:usort(graph:edges(G)),
    lists:foldl(fun({_,_,L}=E, Dg) ->
                        case lists:member(L, Ls) of
                            false -> Dg;
                            true -> graph:del_edge(Dg, E)
                        end
                end,
                G, Es).

data(Config) when is_list(Config) ->
    G = build_graph([], [{x, [{exy, y}]}, {y, []}]),

    [] = graph:vertex(G, x),
    [] = graph:vertex(G, y),
    [E] = graph:edges(G),
    {x,y,exy} = E,

    G1 = graph:add_edge(G, x, y, label_1),
    G2 = graph:add_edge(G1, x, y, label_2), %E
    G3 = graph:add_vertex(G2, x, {any}),
    G4 = graph:add_vertex(G3, y, '_'), % not a wildcard

    {any} = graph:vertex(G4, x),
    '_' = graph:vertex(G4, y),
    [E,{x,y,label_1},{x,y,label_2}] = lists:sort(graph:edges(G4)),
    G5 = graph:del_edge(G4, {x,y,label_1}),
    [E,{x,y,label_2}] = lists:sort(graph:edges(G5)),
    true = sane(G5),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



otp_3522(Config) when is_list(Config) ->
    G0 = build_graph([acyclic], [{x, []}]),
    try graph:add_edge(G0, x, x) of
        _ -> error(cycle_not_detected)
    catch
        error: {bad_edge,{x,x}} -> ok
    end,

    G = graph:new(),
    0 = graph:no_vertices(G),
    0 = graph:no_edges(G),
    {V1, G1} = graph:add_vertex(G),
    G2 = graph:add_vertex(G1, '$vid'),
    {V2, G3} = graph:add_vertex(G2),
    G4 = graph:add_edge(G3, V1, V2, l1),
    G5 = graph:add_edge(G4, V1, V2),
    3 = graph:no_vertices(G5),
    2 = graph:no_edges(G5),
    cyclic = info(G5, cyclicity),

    [] = check(labels(graph:in_edges(G5, V2)), [l1, []]),
    [] = check(labels(graph:out_edges(G5, V1)), [l1, []]),
    [] = check(graph:vertices(G5), [V1,V2,'$vid']),
    [] = check(labels(graph:edges(G5)), [[], l1]),
    true = sane(G5),
    ok.

otp_3630(Config) when is_list(Config) ->
    G = build_graph([], [{x, [{exy,y},{exx,x}]},
                         {y, [{eyy,y},{eyx,x}]}
                        ]),
    [x,y] = graph:get_path(G, x, y),
    [y,x] = graph:get_path(G, y, x),

    [x,x] = graph:get_short_path(G, x, x),
    [y,y] = graph:get_short_path(G, y, y),

    G1 = build_graph([], [{1, [{12,2},{13,3},{11,1}]},
                          {2, [{23,3}]},
                          {3, [{34,4},{35,5}]},
                          {4, [{45,5}]},
                          {5, [{56,6},{57,7}]},
                          {6, [{67,7}]},
                          {7, [{71,1}]}
                         ]),

    [1,3,5,7] = graph:get_short_path(G1, 1, 7),
    [3,5,7,1,3] = graph:get_short_cycle(G1, 3),
    [1,1] = graph:get_short_cycle(G1, 1),

    F = 0.0, I = round(F),
    G2 = graph:new([acyclic]),
    G3 = graph:add_vertex(G2, F),
    G4 = graph:add_vertex(G3, I),
    G5 = graph:add_edge(G4, F, I),
    true = sane(G5),

    ok.

otp_8066(Config) when is_list(Config) ->
    fun() ->
            D = graph:new(),
            {V1, D1} = graph:add_vertex(D),
            {V2, D2} = graph:add_vertex(D1),
            D3 = graph:add_edge(D2, V1, V2),
            [V1, V2] = graph:get_path(D3, V1, V2),
            true = sane(D3),
            D4 = graph:del_path(D3, V1, V2),
            true = sane(D4),
            false = graph:get_path(D4, V1, V2),
            graph:del_path(D4, V1, V2)
    end(),

    fun() ->
            D = graph:new(),
            {V1, D1} = graph:add_vertex(D),
            {V2, D2} = graph:add_vertex(D1),
            D3 = graph:add_edge(D2, V1, V2),
            D4 = graph:add_edge(D3, V1, V2),
            D5 = graph:add_edge(D4, V1, V1),
            D6 = graph:add_edge(D5, V2, V2),
            [V1, V2] = graph:get_path(D6, V1, V2),
            true = sane(D6),
            D7 = graph:del_path(D6, V1, V2),
            false = graph:get_short_path(D7, V2, V1),

            true = sane(D7),
            false = graph:get_path(D7, V1, V2),
            graph:del_path(D7, V1, V2)
    end(),

    fun() ->
            G = graph:new(),
            {W1, G1} = graph:add_vertex(G),
            {W2, G2} = graph:add_vertex(G1),
            {_W3, G3} = graph:add_vertex(G2),
            {_W4, G4} = graph:add_vertex(G3),
            G5 = graph:add_edge(G4, W1, W2, ['$e'|0]),
            try graph:add_edge(G5, bv, W1) of
                _ -> error(bad_edge_not_detected)
            catch
                error: {bad_vertex, bv} -> ok
            end,
            try graph:add_edge(G5, W1, bv) of
                _ -> error(bad_edge_not_detected)
            catch
                error: {bad_vertex, bv} -> ok
            end,
            false = graph:get_short_cycle(G5, W1),
            true = sane(G5)
    end(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vertex_names(Config) when is_list(Config) ->
    G = graph:new([acyclic]),
    A = 'A',
    B = '_',
    G1 = graph:add_vertex(G, A),
    G2 = graph:add_vertex(G1, B),
    G3 = graph:add_edge(G2, A, B),
    AB = {A,B,[]},

    %% Link A -> B
    1 = graph:out_degree(G3, A),
    1 = graph:in_degree(G3, B),
    0 = graph:out_degree(G3, B),
    0 = graph:in_degree(G3, A),
    [B] = graph:out_neighbours(G3, A),
    [A] = graph:in_neighbours(G3, B),
    [] = graph:out_neighbours(G3, B),
    [] = graph:in_neighbours(G3, A),
    [AB] = graph:out_edges(G3, A),
    [AB] = graph:in_edges(G3, B),
    [] = graph:out_edges(G3, B),
    [] = graph:in_edges(G3, A),

    %% Reverse the edge
    G4 = graph:del_edge(G3, AB),
    G5 = graph:add_edge(G4, B, A),
    BA = {B,A,[]},

    1 = graph:out_degree(G5, B),
    1 = graph:in_degree(G5, A),
    0 = graph:out_degree(G5, A),
    0 = graph:in_degree(G5, B),
    [A] = graph:out_neighbours(G5, B),
    [B] = graph:in_neighbours(G5, A),
    [] = graph:out_neighbours(G5, A),
    [] = graph:in_neighbours(G5, B),
    [BA] = graph:out_edges(G5, B),
    [BA] = graph:in_edges(G5, A),
    [] = graph:out_edges(G5, A),
    [] = graph:in_edges(G5, B),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sane(G) ->
    sane1(G),
    erase(sane) =:= undefined.

sane1(G) ->
    Es = graph:edges(G),
    Vs = graph:vertices(G),
    VEs = lists:flatmap(fun(V) -> graph:edges(G, V) end, Vs),
    case lists:sort(Es++Es) =:= lists:sort(VEs) of
        true -> ok;
        false ->
            io:format("Bad edges~n", []), put(sane, no)
    end,

    lists:foreach(
      fun({V1, V2, _L}=Edge) ->
              case {graph:vertex(G, V1, none),
                    graph:vertex(G, V2, none)} of
                  {L1, L2} when L1 =/= none, L2 =/= none -> ok;
                  _ -> io:format("Missing vertex ~p~n", [Edge]), put(sane, no)
              end,
              In = graph:in_edges(G, V2),
              case lists:member(Edge, In) of
                  true -> ok;
                  false ->
                      io:format("Missing in-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end,
              Out = graph:out_edges(G, V1),
              case lists:member(Edge, Out) of
                  true -> ok;
                  false ->
                      io:format("Missing out-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end
      end, Es),

    lists:foreach(
      fun(V) ->
              InEs = graph:in_edges(G, V),
              %% *All* in-edges of V
              lists:foreach(
                fun(E) ->
                        case E of
                            {_, V, _} -> ok;
                            _ ->
                                io:format("Bad in-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, InEs),
              OutEs = graph:out_edges(G, V),
              lists:foreach(
                fun(E) ->
                        case E of
                            {V, _, _} -> ok;
                            _ ->
                                io:format("Bad out-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, OutEs)
      end, Vs),

    InEs = lists:flatmap(fun(V) -> graph:in_edges(G, V) end, Vs),
    OutEs = lists:flatmap(fun(V) -> graph:out_edges(G, V) end, Vs),
    lists:foreach(
      fun(E) ->
              case E of
                  {_, _, _} -> ok;
                  _ ->
                      io:format("Unknown edge (neighbour) ~p~n", [E]),
                      put(sane, no)
              end
      end, InEs++OutEs),

    N_in = length(InEs),
    N_out = length(OutEs),
    N_edges = graph:no_edges(G),
    if
        N_in =/= N_out ->
            io:format("Number of in- and out-edges differs~n", []),
            put(sane, no);
        N_in+N_out =/= N_edges+N_edges  ->
            io:format("Invalid number of edges (~p+~p =/= 2*~p)~n",
                      [N_in, N_out, N_edges]),
            put(sane, no);
        true -> ok
    end,
    EVs = lists:usort([V || {V, _, _} <- Es] ++
                          [V || {_, V, _} <- Es]),
    lists:foreach(
      fun(V) ->
              case graph:vertex(G, V, none) of
                  none ->
                      io:format("Unknown vertex in edge: ~p~n", [V]),
                      put(sane, no);
                  _ -> ok
              end
      end, EVs),

    %% sink: a vertex with no outgoing edges
    SinkVs = [V || V <- Vs, graph:out_edges(G, V) =:= [] ],
    case lists:sort(SinkVs) =:=  lists:sort(graph:sink_vertices(G)) of
        true -> ok;
        false ->
            io:format("Bad sinks~n"), put(sane, no)
    end,
    %% source: a vertex with no incoming edges
    SourceVs = [V || V <- Vs, graph:in_edges(G, V) =:= [] ],
    case lists:sort(SourceVs) =:=  lists:sort(graph:source_vertices(G)) of
        true -> ok;
        false ->
            io:format("Bad sources~n"), put(sane, no)
    end,

    true.

build_graph(Opts, Gs) ->
    G = graph:new(Opts),
    build_g(G, Gs).

build_g(G, [{V,Ns} | Gs]) ->
    G1 = graph:add_vertex(G, V),
    G2 = build_ns(G1, V, Ns),
    build_g(G2, Gs);
build_g(G, []) ->
    true = sane(G),
    G.

build_ns(G, V, [{L,W} | Ns]) ->
    G1 = graph:add_vertex(G, W),
    G2 = graph:add_edge(G1, V, W, L),
    build_ns(G2, V, Ns);
build_ns(G, V, [W | Ns]) ->
    G1 = graph:add_vertex(G, W),
    G2 = graph:add_edge(G1, V, W),
    build_ns(G2, V, Ns);
build_ns(G, _V, []) ->
    G.

info(G, What) ->
    case lists:keysearch(What, 1, graph:info(G)) of
        {value, {What, Value}} -> Value;
        false -> []
    end.

labels(List) ->
    [label(X) || X <- List].

label({_V1,_V2,L}) -> L;
label({_V, L}) -> L.

check(R0, E0) ->
    R = lists:sort(R0),
    E = lists:sort(E0),
    case R of
        E ->
            [];
        _ ->
            (R -- E) ++ (E -- R)
    end.


%% ------------------------------------------------------------------------
%% Graph utilities tests

simple(Config) when is_list(Config) ->
    G0 = graph:new(),
    G1 = add_vertices(G0, [a]),
    G = add_edges(G1, [{b,c},{b,d},{e,f},{f,g},{g,e},{h,h},
                       {i,i},{i,ij1,j},{i,ij2,j}]),
    10 = length(graph:postorder(G)),
    10 = length(graph:preorder(G)),
    ok = evall(graph:components(G),
               [[a],[b,c,d],[e,f,g],[h],[i,j]]),
    ok = evall(graph:strong_components(G),
               [[a],[b],[c],[d],[e,f,g],[h],[i],[j]]),
    ok = evall(graph:cyclic_strong_components(G),
               [[e,f,g],[h],[i]]),
    true = path(G, e, e),
    false = path(G, e, j),
    false = path(G, a, a),
    ok = eval(graph:topsort(G), [a,b,c,d,e,f,g,h,i,j]),
    false = graph:is_acyclic(G),
    true = graph:has_vertex(G, h),
    false = graph:has_vertex(G, z),
    true = graph:has_edge(G, {i, j, ij1}),
    false = graph:has_edge(G, {j, i, ij1}),
    true = graph:has_edge(G, {i, j, ij2}),
    false = graph:has_edge(G, {j, i, ij2}),
    true = graph:has_edge(G, i, j),
    false = graph:has_edge(G, j, i),
    [{i,j,ij1},{i,j,ij2}] = graph:edges(G, i, j),
    [] = graph:edges(G, j, i),
    ok = eval(graph:loop_vertices(G), [h,i]),
    ok = eval(graph:reaching(G, [e]), [e,f,g]),
    ok = eval(graph:reaching_via_neighbours(G, [e]), [e,f,g]),
    ok = eval(graph:reachable(G, [e]), [e,f,g]),
    ok = eval(graph:reachable_via_neighbours(G, [e]), [e,f,g]),
    ok = eval(graph:reaching(G, [b]), [b]),
    ok = eval(graph:reaching_via_neighbours(G, [b]), []),
    ok = eval(graph:reachable(G, [b]), [b,c,d]),
    ok = eval(graph:reachable_via_neighbours(G, [b]), [c,d]),
    ok = eval(graph:reaching(G, [h]), [h]),
    ok = eval(graph:reaching_via_neighbours(G, [h]), [h]),
    ok = eval(graph:reachable(G, [h]), [h]),
    ok = eval(graph:reachable_via_neighbours(G, [h]), [h]),
    ok = eval(graph:reachable(G, [e,f]), [e,f,g]),
    ok = eval(graph:reachable_via_neighbours(G, [e,f]), [e,f,g]),
    ok = eval(graph:reachable(G, [h,h,h]), [h]),
    ok.

roots(Config) when is_list(Config) ->
    G0 = graph:new(),
    G1 = add_vertices(G0, [a]),
    G = add_edges(G1, [{a,b},{b,c},{c,a},{c,d},{j,j},{j,k},{j,l}]),
    7 = length(graph:postorder(G)),
    7 = length(graph:preorder(G)),
    ok.

loop(Config) when is_list(Config) ->
    G0 = graph:new(),
    G1 = add_vertices(G0, [a,b]),
    G = add_edges(G1, [{a,a},{b,b}]),
    ok = evall(graph:components(G), [[a],[b]]),
    ok = evall(graph:strong_components(G), [[a],[b]]),
    ok = evall(graph:cyclic_strong_components(G), [[a],[b]]),
    [_,_] = graph:topsort(G),
    false = graph:is_acyclic(G),
    ok = eval(graph:loop_vertices(G), [a,b]),
    [_,_] = graph:preorder(G),
    [_,_] = graph:postorder(G),
    ok = eval(graph:reaching(G, [b]), [b]),
    ok = eval(graph:reaching_via_neighbours(G, [b]), [b]),
    ok = eval(graph:reachable(G, [b]), [b]),
    ok = eval(graph:reachable_via_neighbours(G, [b]), [b]),
    true = path(G, a, a),
    ok.

isolated(Config) when is_list(Config) ->
    G0 = graph:new(),
    G = add_vertices(G0, [a,b]),
    ok = evall(graph:components(G), [[a],[b]]),
    ok = evall(graph:strong_components(G), [[a],[b]]),
    ok = evall(graph:cyclic_strong_components(G), []),
    [_,_] = graph:topsort(G),
    true = graph:is_acyclic(G),
    ok = eval(graph:loop_vertices(G), []),
    [_,_] = graph:preorder(G),
    [_,_] = graph:postorder(G),
    ok = eval(graph:reaching(G, [b]), [b]),
    ok = eval(graph:reaching_via_neighbours(G, [b]), []),
    ok = eval(graph:reachable(G, [b]), [b]),
    ok = eval(graph:reachable_via_neighbours(G, [b]), []),
    false = path(G, a, a),
    ok.

topsort(Config) when is_list(Config) ->
    G0 = graph:new(),
    G = add_edges(G0, [{a,b},{b,c},{c,d},{d,e},{e,f}]),
    ok = eval(graph:topsort(G), [a,b,c,d,e,f]),
    ok.

subgraph(Config) when is_list(Config) ->
    G0 = graph:new([acyclic]),
    G1 = add_edges(G0, [{b,c},{b,d},{e,f},{f,fg,g},{f,fg2,g},{h,i},{i,j}]),
    G = add_vertices(G1, [{b,bl},{f,fl}]),
    SG = graph:subgraph(G, [u1,b,c,u2,f,g,i,u3]),
    [b,c,f,g,i] = lists:sort(graph:vertices(SG)),
    bl = graph:vertex(SG, b),
    [] = graph:vertex(SG, c),
    [{f,g,fg},{f,g,fg2}] = graph:edges(SG, f, g),
    {_, {_, acyclic}} = lists:keysearch(cyclicity, 1, graph:info(SG)),

    SG1 = graph:subgraph(G, [f, g, h],
                                 [{type, []}, {keep_labels, false}]),
    [f,g,h] = lists:sort(graph:vertices(SG1)),
    [] = graph:vertex(SG1, f),
    [{f,g,[]}] = graph:edges(SG1, f, g),
    {_, {_, cyclic}} = lists:keysearch(cyclicity, 1, graph:info(SG1)),

    SG2 = graph:subgraph(G, [f, g, h],
                                 [{type, [acyclic]},
                                  {keep_labels, true}]),
    [f,g,h] = lists:sort(graph:vertices(SG2)),
    fl = graph:vertex(SG2, f),
    [{f,g,fg},{f,g,fg2}] = graph:edges(SG2, f, g),
    {_, {_, acyclic}} = lists:keysearch(cyclicity, 1, graph:info(SG2)),

    {'EXIT',{badarg,_}} =
        (catch graph:subgraph(G, [f], [{invalid, opt}])),
    {'EXIT',{badarg,_}} =
        (catch graph:subgraph(G, [f], [{keep_labels, not_Bool}])),
    {'EXIT',{badarg,_}} =
        (catch graph:subgraph(G, [f], [{type, not_type}])),
    {'EXIT',{badarg,_}} =
        (catch graph:subgraph(G, [f], [{type, [not_type]}])),
    {'EXIT',{badarg,_}} =
        (catch graph:subgraph(G, [f], not_a_list)),

    ok.

condensation(Config) when is_list(Config) ->
    G0 = graph:new([]),
    G1 = add_edges(G0, [{b,c},{b,d},{e,f},{f,fgl,g},{f,fgl2,g},{g,e},
                        {h,h},{j,i},{i,j}]),
    G = add_vertices(G1, [q]),
    CG = graph:condensation(G),
    Vs = sort_2(graph:vertices(CG)),
    [[b],[c],[d],[e,f,g],[h],[i,j],[q]] = Vs,
    Fun = fun(E) ->
                  {V1, V2, _L} = E,
                  {lists:sort(V1), lists:sort(V2)}
          end,
    Es = lists:map(Fun, graph:edges(CG)),
    [{[b],[c]},{[b],[d]}] = lists:sort(Es),
    ok.

%% OTP-7081
tree(Config) when is_list(Config) ->
    false = is_tree([], []),
    true = is_tree([a], []),
    false = is_tree([a,b], []),
    true = is_tree([{a,b}]),
    false = is_tree([{a,b},{b,a}]),
    true = is_tree([{a,b},{a,c},{b,d},{b,e}]),
    false = is_tree([{a,b},{a,c},{b,d},{b,e},{d,e}]),
    false = is_tree([{a,b},{a,c},{b,d},{b,l1,e},{b,l2,e}]),
    true = is_tree([{a,c},{c,b}]),
    true = is_tree([{b,a},{c,a}]),
    %% Parallel edges. Acyclic and with one component
    false = is_tree([{a,l1,b},{a,l2,b}]),

    no = arborescence_root([], []),
    {yes, a} = arborescence_root([a], []),
    no = arborescence_root([a,b], []),
    {yes, a} = arborescence_root([{a,b}]),
    no = arborescence_root([{a,b},{b,a}]),
    {yes, a} = arborescence_root([{a,b},{a,c},{b,d},{b,e}]),
    no = arborescence_root([{a,b},{a,c},{b,d},{b,e},{d,e}]),
    no = arborescence_root([{a,b},{a,c},{b,d},{b,l1,e},{b,l2,e}]),
    {yes, a} = arborescence_root([{a,c},{c,b}]),
    no = arborescence_root([{b,a},{c,a}]),

    false = is_arborescence([], []),
    true = is_arborescence([a], []),
    false = is_arborescence([a,b], []),
    true = is_arborescence([{a,b}]),
    false = is_arborescence([{a,b},{b,a}]),
    true = is_arborescence([{a,b},{a,c},{b,d},{b,e}]),
    false = is_arborescence([{a,b},{a,c},{b,d},{b,e},{d,e}]),
    false = is_arborescence([{a,b},{a,c},{b,d},{b,l1,e},{b,l2,e}]),
    true = is_arborescence([{a,c},{c,b}]),
    false = is_arborescence([{b,a},{c,a}]),

    %% Parallel edges.
    false = is_arborescence([{a,l1,b},{a,l2,b}]),

    ok.

%% OTP-9040
traversals(Config) when is_list(Config) ->
    G = graph:new([]),
    [] = graph:preorder(G),
    [] = graph:postorder(G),
    G1 = add_edges(G, [{a,b},{b,c},{c,d},{d,e}]),
    [a,b,c,d,e] = graph:preorder(G1),
    [e,d,c,b,a] = graph:postorder(G1),
    G2 = add_edges(G1, [{0,1},{1,2},{2,0}]),
    [a,b,c,d,e,1,2,0] = graph:preorder(G2),
    [e,d,c,b,a,0,2,1] = graph:postorder(G2),
    G3 = add_edges(G1, [{x,0},{y,1},{z,2}]),
    [x,0,y,1,a,b,c,d,e,z,2] = graph:preorder(G3),
    [0,x,1,y,e,d,c,b,a,2,z] = graph:postorder(G3),
    ok.

is_tree(Es) ->
    is_tree([], Es).

is_tree(Vs, Es) ->
    gu(Vs, Es, fun graph:is_tree/1).

is_arborescence(Es) ->
    is_arborescence([], Es).

is_arborescence(Vs, Es) ->
    gu(Vs, Es, fun graph:is_arborescence/1).

arborescence_root(Es) ->
    arborescence_root([], Es).

arborescence_root(Vs, Es) ->
    gu(Vs, Es, fun graph:arborescence_root/1).

gu(Vs, Es, F) ->
    G = graph:new(),
    G1 = add_vertices(G, Vs),
    G2 = add_edges(G1, Es),
    F(G2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_2(L) ->
    lists:sort(lists:map(fun(V) -> lists:sort(V) end, L)).

path(G, V1, V2) ->
    graph:get_path(G, V1, V2) /= false.

add_vertices(G, Vs) ->
    lists:foldl(fun({V, Label}, G0) -> graph:add_vertex(G0, V, Label);
                   (V, G0) -> graph:add_vertex(G0, V)
                end, G, Vs).

add_edges(G, L) ->
    Fun = fun({From, To}, G0) ->
                  G1 = graph:add_vertex(G0, From),
                  G2 = graph:add_vertex(G1, To),
                  graph:add_edge(G2, From, To);
             ({From, Label, To}, G0) ->
                  G1 = graph:add_vertex(G0, From),
                  G2 = graph:add_vertex(G1, To),
                  graph:add_edge(G2, From, To, Label)
          end,
    lists:foldl(Fun, G, L).

eval(L, E) ->
    Expected = lists:sort(E),
    Got = lists:sort(L),
    if
        Expected == Got ->
            ok;
        true ->
            not_ok
    end.

evall(L, E) ->
    F = fun(L1) -> lists:sort(L1) end,
    Fun = fun(LL) -> F(lists:map(F, LL)) end,

    Expected = Fun(E),
    Got = Fun(L),
    if
        Expected == Got ->
            ok;
        true ->
            not_ok
    end.
