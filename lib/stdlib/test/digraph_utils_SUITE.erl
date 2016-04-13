%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
-module(digraph_utils_SUITE).

%%-define(debug, true).
-ifdef(debug).
-define(line, put(line, ?LINE), ).
-else.
-include_lib("common_test/include/ct.hrl").
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([simple/1, loop/1, isolated/1, topsort/1, subgraph/1, 
         condensation/1, tree/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [simple, loop, isolated, topsort, subgraph,
     condensation, tree].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple(Config) when is_list(Config) ->
    G = digraph:new(),
    add_vertices(G, [a]),
    add_edges(G, [{b,c},{b,d},{e,f},{f,g},{g,e},{h,h},{i,i},{i,j}]),
    10 = length(digraph_utils:postorder(G)),
    10 = length(digraph_utils:preorder(G)),
    ok = evall(digraph_utils:components(G),
	       [[a],[b,c,d],[e,f,g],[h],[i,j]]),
    ok = evall(digraph_utils:strong_components(G),
	       [[a],[b],[c],[d],[e,f,g],[h],[i],[j]]),
    ok = evall(digraph_utils:cyclic_strong_components(G),
	       [[e,f,g],[h],[i]]),
    true = path(G, e, e),
    false = path(G, e, j),
    false = path(G, a, a),
    false = digraph_utils:topsort(G),
    false = digraph_utils:is_acyclic(G),
    ok = eval(digraph_utils:loop_vertices(G), [h,i]),
    ok = eval(digraph_utils:reaching([e], G), [e,f,g]),
    ok = eval(digraph_utils:reaching_neighbours([e], G), [e,f,g]),
    ok = eval(digraph_utils:reachable([e], G), [e,f,g]),
    ok = eval(digraph_utils:reachable_neighbours([e], G), [e,f,g]),
    ok = eval(digraph_utils:reaching([b], G), [b]),
    ok = eval(digraph_utils:reaching_neighbours([b], G), []),
    ok = eval(digraph_utils:reachable([b], G), [b,c,d]),
    ok = eval(digraph_utils:reachable_neighbours([b], G), [c,d]),
    ok = eval(digraph_utils:reaching([h], G), [h]),
    ok = eval(digraph_utils:reaching_neighbours([h], G), [h]),
    ok = eval(digraph_utils:reachable([h], G), [h]),
    ok = eval(digraph_utils:reachable_neighbours([h], G), [h]),
    ok = eval(digraph_utils:reachable([e,f], G), [e,f,g]),
    ok = eval(digraph_utils:reachable_neighbours([e,f], G), [e,f,g]),
    ok = eval(digraph_utils:reachable([h,h,h], G), [h]),
    true = digraph:delete(G),
    ok.

loop(Config) when is_list(Config) ->
    G = digraph:new(),
    add_vertices(G, [a,b]),
    add_edges(G, [{a,a},{b,b}]),
    ok = evall(digraph_utils:components(G), [[a],[b]]),
    ok = evall(digraph_utils:strong_components(G), [[a],[b]]),
    ok = evall(digraph_utils:cyclic_strong_components(G), [[a],[b]]),
    [_,_] = digraph_utils:topsort(G),
    false = digraph_utils:is_acyclic(G),
    ok = eval(digraph_utils:loop_vertices(G), [a,b]),
    [_,_] = digraph_utils:preorder(G),
    [_,_] = digraph_utils:postorder(G),
    ok = eval(digraph_utils:reaching([b], G), [b]),
    ok = eval(digraph_utils:reaching_neighbours([b], G), [b]),
    ok = eval(digraph_utils:reachable([b], G), [b]),
    ok = eval(digraph_utils:reachable_neighbours([b], G), [b]),
    true = path(G, a, a),
    true = digraph:delete(G),
    ok.

isolated(Config) when is_list(Config) ->
    G = digraph:new(),
    add_vertices(G, [a,b]),
    ok = evall(digraph_utils:components(G), [[a],[b]]),
    ok = evall(digraph_utils:strong_components(G), [[a],[b]]),
    ok = evall(digraph_utils:cyclic_strong_components(G), []),
    [_,_] = digraph_utils:topsort(G),
    true = digraph_utils:is_acyclic(G),
    ok = eval(digraph_utils:loop_vertices(G), []),
    [_,_] = digraph_utils:preorder(G),
    [_,_] = digraph_utils:postorder(G),
    ok = eval(digraph_utils:reaching([b], G), [b]),
    ok = eval(digraph_utils:reaching_neighbours([b], G), []),
    ok = eval(digraph_utils:reachable([b], G), [b]),
    ok = eval(digraph_utils:reachable_neighbours([b], G), []),
    false = path(G, a, a),
    true = digraph:delete(G),
    ok.

topsort(Config) when is_list(Config) ->
    G = digraph:new(),
    add_edges(G, [{a,b},{b,c},{c,d},{d,e},{e,f}]),
    ok = eval(digraph_utils:topsort(G), [a,b,c,d,e,f]),
    true = digraph:delete(G),
    ok.

subgraph(Config) when is_list(Config) ->
    G = digraph:new([acyclic]),
    add_edges(G, [{b,c},{b,d},{e,f},{f,fg,fgl,g},{f,fg2,fgl2,g},{g,e},
		  {h,h},{i,i},{i,j}]),
    add_vertices(G, [{b,bl},{f,fl}]),
    SG = digraph_utils:subgraph(G, [u1,b,c,u2,f,g,i,u3]),
    [b,c,f,g,i] = lists:sort(digraph:vertices(SG)),
    {b,bl} = digraph:vertex(SG, b),
    {c,[]} = digraph:vertex(SG, c),
    {fg,f,g,fgl} = digraph:edge(SG, fg),
    {fg2,f,g,fgl2} = digraph:edge(SG, fg2),
    {_, {_, acyclic}} = lists:keysearch(cyclicity, 1, digraph:info(SG)),
    true = digraph:delete(SG),

    SG1 = digraph_utils:subgraph(G, [f, g, h],
				 [{type, []}, {keep_labels, false}]),
    [f,g,h] = lists:sort(digraph:vertices(SG1)),
    {f,[]} = digraph:vertex(SG1, f),
    {fg,f,g,[]} = digraph:edge(SG1, fg),
    {_, {_, cyclic}} = lists:keysearch(cyclicity, 1, digraph:info(SG1)),
    true = digraph:delete(SG1),

    SG2 = digraph_utils:subgraph(G, [f, g, h],
				 [{type, [acyclic]},
				  {keep_labels, true}]),
    [f,g,h] = lists:sort(digraph:vertices(SG2)),
    {f,fl} = digraph:vertex(SG2, f),
    {fg,f,g,fgl} = digraph:edge(SG2, fg),
    {_, {_, acyclic}} = lists:keysearch(cyclicity, 1, digraph:info(SG2)),
    true = digraph:delete(SG2),

    {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{invalid, opt}])),
    {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{keep_labels, not_Bool}])),
    {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{type, not_type}])),
    {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{type, [not_type]}])),
    {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], not_a_list)),

    true = digraph:delete(G),

    ok.

condensation(Config) when is_list(Config) ->
    G = digraph:new([]),
    add_edges(G, [{b,c},{b,d},{e,f},{f,fg,fgl,g},{f,fg2,fgl2,g},{g,e},
		  {h,h},{j,i},{i,j}]),
    add_vertices(G, [q]),
    CG = digraph_utils:condensation(G),
    Vs = sort_2(digraph:vertices(CG)),
    [[b],[c],[d],[e,f,g],[h],[i,j],[q]] = Vs,
    Fun = fun(E) ->
		  {_E, V1, V2, _L} = digraph:edge(CG, E),
		  {lists:sort(V1), lists:sort(V2)}
	  end,
    Es = lists:map(Fun, digraph:edges(CG)),
    [{[b],[c]},{[b],[d]}] = lists:sort(Es),
    true = digraph:delete(CG),
    true = digraph:delete(G),
    ok.

%% OTP-7081
tree(Config) when is_list(Config) ->
    false = is_tree([], []),
    true = is_tree([a], []),
    false = is_tree([a,b], []),
    true = is_tree([{a,b}]),
    false = is_tree([{a,b},{b,a}]),
    true = is_tree([{a,b},{a,c},{b,d},{b,e}]),
    false = is_tree([{a,b},{a,c},{b,d},{b,e}, {d,e}]),
    false = is_tree([{a,b},{a,c},{b,d},{b,e}, {b,e}]),
    true = is_tree([{a,c},{c,b}]),
    true = is_tree([{b,a},{c,a}]),
    %% Parallel edges. Acyclic and with one componets
    %% (according to the digraph module).
    false = is_tree([{a,b},{a,b}]),

    no = arborescence_root([], []),
    {yes, a} = arborescence_root([a], []),
    no = arborescence_root([a,b], []),
    {yes, a} = arborescence_root([{a,b}]),
    no = arborescence_root([{a,b},{b,a}]),
    {yes, a} = arborescence_root([{a,b},{a,c},{b,d},{b,e}]),
    no = arborescence_root([{a,b},{a,c},{b,d},{b,e}, {d,e}]),
    no = arborescence_root([{a,b},{a,c},{b,d},{b,e}, {b,e}]),
    {yes, a} = arborescence_root([{a,c},{c,b}]),
    no = arborescence_root([{b,a},{c,a}]),

    false = is_arborescence([], []),
    true = is_arborescence([a], []),
    false = is_arborescence([a,b], []),
    true = is_arborescence([{a,b}]),
    false = is_arborescence([{a,b},{b,a}]),
    true = is_arborescence([{a,b},{a,c},{b,d},{b,e}]),
    false = is_arborescence([{a,b},{a,c},{b,d},{b,e}, {d,e}]),
    false = is_arborescence([{a,b},{a,c},{b,d},{b,e}, {b,e}]),
    true = is_arborescence([{a,c},{c,b}]),
    false = is_arborescence([{b,a},{c,a}]),

    %% Parallel edges.
    false = is_arborescence([{a,b},{a,b}]),

    ok.

is_tree(Es) ->
    is_tree([], Es).

is_tree(Vs, Es) ->
    gu(Vs, Es, is_tree).

is_arborescence(Es) ->
    is_arborescence([], Es).

is_arborescence(Vs, Es) ->
    gu(Vs, Es, is_arborescence).

arborescence_root(Es) ->
    arborescence_root([], Es).

arborescence_root(Vs, Es) ->
    gu(Vs, Es, arborescence_root).

gu(Vs, Es, F) ->
    G = digraph:new(),
    add_vertices(G, Vs),
    add_edges(G, Es),
    Reply = digraph_utils:F(G),
    true = digraph:delete(G),
    Reply.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_2(L) ->
    lists:sort(lists:map(fun(V) -> lists:sort(V) end, L)).

path(G, V1, V2) ->
    digraph:get_path(G, V1, V2) /= false.

add_vertices(G, Vs) ->
    lists:foreach(fun({V, Label}) -> digraph:add_vertex(G, V, Label);
		     (V) -> digraph:add_vertex(G, V) 
		  end, Vs).

add_edges(G, L) ->
    Fun = fun({From, To}) -> 
		  digraph:add_vertex(G, From),
		  digraph:add_vertex(G, To),
		  digraph:add_edge(G, From, To);
	     ({From, Edge, Label, To}) ->
		  digraph:add_vertex(G, From),
		  digraph:add_vertex(G, To),
		  digraph:add_edge(G, Edge, From, To, Label)
	  end,
    lists:foreach(Fun, L).

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
