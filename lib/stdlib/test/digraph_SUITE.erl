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
-module(digraph_SUITE).

%%-define(STANDALONE,1).

-ifdef(STANDALONE).
-define(line, put(line, ?LINE), ).
-else.
-include_lib("common_test/include/ct.hrl").
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([opts/1, degree/1, path/1, cycle/1, vertices/1,
	 edges/1, data/1, otp_3522/1, otp_3630/1, otp_8066/1]).

-export([spawn_graph/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [opts, degree, path, cycle, {group, misc},
     {group, tickets}].

groups() -> 
    [{misc, [], [vertices, edges, data]},
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
    %% OTP-5985: the 'public' option has been removed
    {'EXIT',{badarg,_}} = (catch digraph:new([public])),
    {P2,G2} = spawn_graph([private]),
    {'EXIT',{badarg,_}} = (catch digraph:add_vertex(G2, x)),
    kill_graph(P2),
    {P3,G3} = spawn_graph([protected]),
    {'EXIT',{badarg,_}} = (catch digraph:add_vertex(G3, x)),
    kill_graph(P3),
    Template = [{v1,[v2]}, {v2,[v3]}, {v3,[v4]}, {v4,[]}],
    G4 = build_graph([], Template),
    e = digraph:add_edge(G4, e, v4, v1, []),
    digraph:delete(G4),
    G5 = build_graph([cyclic], Template),
    e = digraph:add_edge(G5, e, v4, v1, []),
    digraph:delete(G5),
    G6 = build_graph([acyclic], Template),
    acyclic = info(G6, cyclicity),
    {error, {bad_edge,_}} = digraph:add_edge(G6, v4, v1),
    digraph:delete(G6),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

degree(Config) when is_list(Config) ->
    G = build_graph([], [{x1,[]}, {x2,[x1]}, {x3,[x1,x2]},
			 {x4,[x1,x2,x3]}, {x5,[x1,x2,x3,x4]}]),
    %% out degree
    0 = digraph:out_degree(G, x1),
    1 = digraph:out_degree(G, x2),
    2 = digraph:out_degree(G, x3),
    3 = digraph:out_degree(G, x4),
    4 = digraph:out_degree(G, x5),
    %% out neighbours
    [] = check(digraph:out_neighbours(G, x1), []),
    [] = check(digraph:out_neighbours(G, x2), [x1]),
    [] = check(digraph:out_neighbours(G, x3), [x1,x2]),
    [] = check(digraph:out_neighbours(G, x4), [x1,x2,x3]),
    [] = check(digraph:out_neighbours(G, x5), [x1,x2,x3,x4]),

    %% in degree
    4 = digraph:in_degree(G, x1),
    3 = digraph:in_degree(G, x2),
    2 = digraph:in_degree(G, x3),
    1 = digraph:in_degree(G, x4),
    0 = digraph:in_degree(G, x5),
    %% in neighbours
    [] = check(digraph:in_neighbours(G, x1), [x2,x3,x4,x5]),
    [] = check(digraph:in_neighbours(G, x2), [x3,x4,x5]),
    [] = check(digraph:in_neighbours(G, x3), [x4,x5]),
    [] = check(digraph:in_neighbours(G, x4), [x5]),
    [] = check(digraph:in_neighbours(G, x5), []),
    digraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path(Config) when is_list(Config) ->
    G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
			 {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7]}]),
    Vi = case digraph:get_path(G, x1, x7) of
	     [x1,x2,x4,x5,x7] -> digraph:del_vertex(G, x5), x6;
	     [x1,x2,x4,x6,x7] -> digraph:del_vertex(G, x6), x5;
	     [x1,x3,x4,x5,x7] -> digraph:del_vertex(G, x5), x6;
	     [x1,x3,x4,x6,x7] -> digraph:del_vertex(G, x6), x5
	 end,
    Vj = case digraph:get_path(G, x1, x7) of
	     [x1,x2,x4,Vi,x7] -> digraph:del_vertex(G,x2), x3;
	     [x1,x3,x4,Vi,x7] -> digraph:del_vertex(G,x3), x2
	 end,
    [x1,Vj,x4,Vi,x7] = digraph:get_path(G, x1, x7),
    digraph:del_vertex(G, Vj),
    false = digraph:get_path(G, x1, x7),
    [] = check(digraph:vertices(G), [x1,x4,Vi,x7]),
    digraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle(Config) when is_list(Config) ->
    G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
			 {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7,x8]},
			 {x8,[x3,x8]}]),
    false = digraph:get_cycle(G, x1),
    false = digraph:get_cycle(G, x2),
    false = digraph:get_cycle(G, x5),
    false = digraph:get_cycle(G, x7),
    [x3,x4,x6,x8,x3] = digraph:get_cycle(G, x3),
    [x4,x6,x8,x3,x4] = digraph:get_cycle(G, x4),
    [x6,x8,x3,x4,x6] = digraph:get_cycle(G, x6),
    [x8,x3,x4,x6,x8] = digraph:get_cycle(G, x8),
    digraph:del_vertex(G, x4),
    [x8] = digraph:get_cycle(G, x8),
    digraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



vertices(Config) when is_list(Config) ->
    G = build_graph([], [{x,[]}, {y,[]}]),
    [] = check(digraph:vertices(G), [x,y]),
    digraph:del_vertices(G, [x,y]),
    [] = digraph:vertices(G),
    digraph:delete(G),
    ok.

edges(Config) when is_list(Config) ->
    G = build_graph([], [{x, [{exy,y},{exx,x}]},
			 {y, [{eyx,x}]}
			]),
    [] = check(digraph:edges(G), [exy, eyx, exx]),
    [] = check(digraph:out_edges(G, x), [exy,exx]),
    [] = check(digraph:in_edges(G, x), [eyx,exx]),
    [] = check(digraph:out_edges(G, y), [eyx]),
    [] = check(digraph:in_edges(G, y), [exy]),
    true = digraph:del_edges(G, [exy, eyx, does_not_exist]),
    [exx] = digraph:edges(G),
    [] = check(digraph:out_edges(G, x), [exx]),
    [] = check(digraph:in_edges(G, x), [exx]),
    [] = check(digraph:out_edges(G, y), []),
    [] = check(digraph:in_edges(G, y), []),
    digraph:del_vertices(G, [x,y]),
    [] = digraph:edges(G),
    [] = digraph:vertices(G),
    digraph:delete(G),
    ok.

data(Config) when is_list(Config) ->
    G = build_graph([], [{x, [{exy, y}]}, {y, []}]),

    {x,[]} = digraph:vertex(G, x),
    {y,[]} = digraph:vertex(G, y),
    {exy,x,y,[]} = digraph:edge(G, exy),

    digraph:add_edge(G, exy, x, y, {data,x,y}),
    E = digraph:add_edge(G, x, y, {data,y,x}),
    digraph:add_vertex(G, x, {any}),
    digraph:add_vertex(G, y, '_'),

    {x,{any}} = digraph:vertex(G, x),
    {y,'_'} = digraph:vertex(G, y),
    {exy,x,y,{data,x,y}} = digraph:edge(G, exy),
    {E,x,y,{data,y,x}} = digraph:edge(G, E),
    true = digraph:del_edge(G, E),
    false = digraph:edge(G, E),
    true = sane(G),
    digraph:delete(G),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



otp_3522(Config) when is_list(Config) ->
    G1 = build_graph([acyclic], [{x, []}]),
    {error, {bad_edge,_}} = digraph:add_edge(G1, x, x),
    true = digraph:delete(G1),

    G = digraph:new(),
    0 = digraph:no_vertices(G),
    0 = digraph:no_edges(G),
    V1 = digraph:add_vertex(G),
    '$vid' = digraph:add_vertex(G, '$vid'),
    V2 = digraph:add_vertex(G),
    '$eid' = digraph:add_edge(G, '$eid', V1, V2, []),
    E = digraph:add_edge(G, V1, V2),
    3 = digraph:no_vertices(G),
    2 = digraph:no_edges(G),
    cyclic = info(G, cyclicity),
    protected = info(G, protection),

    [] = check(digraph:in_edges(G, V2), ['$eid', E]),
    [] = check(digraph:out_edges(G, V1), ['$eid', E]),
    [] = check(digraph:vertices(G), [V1,V2,'$vid']),
    [] = check(digraph:edges(G), [E, '$eid']),
    true = sane(G),
    true = digraph:delete(G),
    ok.

otp_3630(Config) when is_list(Config) ->
    G = build_graph([], [{x, [{exy,y},{exx,x}]},
			 {y, [{eyy,y},{eyx,x}]}
			]),
    [x,y] = digraph:get_path(G, x, y),
    [y,x] = digraph:get_path(G, y, x),

    [x,x] = digraph:get_short_path(G, x, x),
    [y,y] = digraph:get_short_path(G, y, y),
    true = digraph:delete(G),

    G1 = build_graph([], [{1, [{12,2},{13,3},{11,1}]},
			  {2, [{23,3}]},
			  {3, [{34,4},{35,5}]},
			  {4, [{45,5}]},
			  {5, [{56,6},{57,7}]},
			  {6, [{67,7}]},
			  {7, [{71,1}]}
			 ]),

    [1,3,5,7] = digraph:get_short_path(G1, 1, 7),
    [3,5,7,1,3] = digraph:get_short_cycle(G1, 3),
    [1,1] = digraph:get_short_cycle(G1, 1),
    true = digraph:delete(G1),

    F = 0.0, I = round(F),
    G2 = digraph:new([acyclic]),
    digraph:add_vertex(G2, F),
    digraph:add_vertex(G2, I),
    E = digraph:add_edge(G2, F, I),
    true = not is_tuple(E),
    true = sane(G2),
    true = digraph:delete(G2),

    ok.

otp_8066(Config) when is_list(Config) ->
    fun() ->
            D = digraph:new(),
            V1 = digraph:add_vertex(D),
            V2 = digraph:add_vertex(D),
            _ = digraph:add_edge(D, V1, V2),
            [V1, V2] = digraph:get_path(D, V1, V2),
            true = sane(D),
            true = digraph:del_path(D, V1, V2),
            true = sane(D),
            false = digraph:get_path(D, V1, V2),
            true = digraph:del_path(D, V1, V2),
            true = digraph:delete(D)
    end(),

    fun() ->
            D = digraph:new(),
            V1 = digraph:add_vertex(D),
            V2 = digraph:add_vertex(D),
            _ = digraph:add_edge(D, V1, V2),
            _ = digraph:add_edge(D, V1, V2),
            _ = digraph:add_edge(D, V1, V1),
            _ = digraph:add_edge(D, V2, V2),
            [V1, V2] = digraph:get_path(D, V1, V2),
            true = sane(D),
            true = digraph:del_path(D, V1, V2),
            false = digraph:get_short_path(D, V2, V1),

            true = sane(D),
            false = digraph:get_path(D, V1, V2),
            true = digraph:del_path(D, V1, V2),
            true = digraph:delete(D)
    end(),

    fun() ->
            G = digraph:new(),
            W1 = digraph:add_vertex(G),
            W2 = digraph:add_vertex(G),
            W3 = digraph:add_vertex(G),
            W4 = digraph:add_vertex(G),
            _ = digraph:add_edge(G,['$e'|0], W1, W2, {}),
            {error,{bad_vertex, bv}} =
                digraph:add_edge(G, edge, bv, W1, {}),
            {error,{bad_vertex, bv}} =
                digraph:add_edge(G, edge, W1, bv, {}),
            false = digraph:get_short_cycle(G, W1),
            {error, {bad_edge,_}} =
                digraph:add_edge(G,['$e'|0], W3, W4, {}),
            true = sane(G),
            true = digraph:delete(G)
    end(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sane(G) ->
    sane1(G),
    erase(sane) =:= undefined.

sane1(G) ->
    %% etab: {E, V1, V2, Label}
    %% ntab: {{out,V},E} eller {{in,V},E}
    %% vtab: {V,Label}

    Es = digraph:edges(G),
    Vs = digraph:vertices(G),
    VEs = lists:flatmap(fun(V) -> digraph:edges(G, V) end, Vs),
    case lists:sort(Es++Es) =:= lists:sort(VEs) of
        true -> ok;
        false ->
            io:format("Bad edges~n", []), put(sane, no)
    end,

    lists:foreach(
      fun(E) ->
              Edge = {E, V1, V2, _L} = digraph:edge(G, E),
              case {digraph:vertex(G, V1), digraph:vertex(G, V2)} of
                  {{V1, _}, {V2, _}} -> ok;
                  _ -> io:format("Missing vertex ~p~n", [Edge]), put(sane, no)
              end,
              In = digraph:in_edges(G, V2),
              case lists:member(E, In) of 
                  true -> ok;
                  false ->
                      io:format("Missing in-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end,
              Out = digraph:out_edges(G, V1),
              case lists:member(E, Out) of 
                  true -> ok;
                  false ->
                      io:format("Missing out-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end
      end, Es),

    lists:foreach(
      fun(V) ->
              InEs = digraph:in_edges(G, V),
              %% *All* in-edoges of V
              lists:foreach(
                fun(E) ->
                        case digraph:edge(G, E) of
                            {E, _, V, _} -> ok;
                            _ -> 
                                io:format("Bad in-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, InEs),
              OutEs = digraph:out_edges(G, V),
              lists:foreach(
                fun(E) ->
                        case digraph:edge(G, E) of
                            {E, V, _, _} -> ok;
                            _ -> 
                                io:format("Bad out-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, OutEs)
      end, Vs),

    InEs = lists:flatmap(fun(V) -> digraph:in_edges(G, V) end, Vs),
    OutEs = lists:flatmap(fun(V) -> digraph:out_edges(G, V) end, Vs),
    lists:foreach(
      fun(E) ->
              case digraph:edge(G, E) of
                  {E, _, _, _} -> ok;
                  _ -> 
                      io:format("Unknown edge (neighbour) ~p~n", [E]),
                      put(sane, no)
              end
      end, InEs++OutEs),

    N_in = length(InEs),
    N_out = length(OutEs),
    N_edges = digraph:no_edges(G),
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
    Edges = [digraph:edge(G, E) || E <- Es],
    EVs = lists:usort([V || {_, V, _, _} <- Edges] ++
			  [V || {_, _, V, _} <- Edges]),
    lists:foreach(
      fun(V) ->
              case digraph:vertex(G, V) of
                  {_, _} -> ok;
                  false ->
                      io:format("Unknown vertex in edge: ~p~n", [V]),
                      put(sane, no)
              end
      end, EVs),

    %% sink_vertices and source_vertices were introduced in 2001. They
    %% are not documented.

    %% sink: a vertex with no outgoing edges
    SinkVs = [V || V <- Vs, digraph:out_edges(G, V) =:= [] ],
    case lists:sort(SinkVs) =:=  lists:sort(digraph:sink_vertices(G)) of
        true -> ok;
        false -> 
            io:format("Bad sinks~n"), put(sane, no)
    end,
    %% sink: a vertex with no incoming edges
    SourceVs = [V || V <- Vs, digraph:in_edges(G, V) =:= [] ],
    case lists:sort(SourceVs) =:=  lists:sort(digraph:source_vertices(G)) of
        true -> ok;
        false -> 
            io:format("Bad sources~n"), put(sane, no)
    end,

    true.

build_graph(Opts, Gs) ->
    G = digraph:new(Opts),
    build_g(G, Gs).

build_g(G, [{V,Ns} | Gs]) ->
    digraph:add_vertex(G, V),
    build_ns(G, V, Ns),
    build_g(G, Gs);
build_g(G, []) -> 
    true = sane(G),
    G.

build_ns(G, V, [{E,W} | Ns]) ->
    digraph:add_vertex(G, W),
    digraph:add_edge(G, E, V, W, []),
    build_ns(G, V, Ns);
build_ns(G, V, [W | Ns]) ->
    digraph:add_vertex(G, W),
    digraph:add_edge(G, V, W),
    build_ns(G, V, Ns);
build_ns(_G, _V, []) ->
    true.

%% Spawn a process that create a graph return {Pid, Graph}

spawn_graph(Opts) ->
    Pid = spawn(?MODULE, spawn_graph, [self(),Opts]),
    receive
	{Pid, G} -> {Pid,G}
    end.

%% Create a graph and wait for die message
spawn_graph(Starter, Opts) ->
    G = digraph:new(Opts),
    Starter ! {self(), G},
    receive
	die -> true
    end.

info(G, What) ->
    case lists:keysearch(What, 1, digraph:info(G)) of
	{value, {What, Value}} -> Value;
	false -> []
    end.

%% Kill process created by spawn_graph
kill_graph(Pid) ->
    Pid ! die.

check(R0, E0) ->
    R = lists:sort(R0),
    E = lists:sort(E0),
    case R of
	E ->
	    [];
	_ ->
	    (R -- E) ++ (E -- R)
    end.
