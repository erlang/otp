%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2012. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(digraph_utils_SUITE).

%-define(debug, true).
-ifdef(debug).
-define(line, put(line, ?LINE), ).
-else.
-include_lib("test_server/include/test_server.hrl").
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

simple(doc) -> [];
simple(suite) -> [];
simple(Config) when is_list(Config) ->
    ?line G = digraph:new(),
    ?line add_vertices(G, [a]),
    ?line add_edges(G, [{b,c},{b,d},{e,f},{f,g},{g,e},{h,h},{i,i},{i,j}]),
    ?line 10 = length(digraph_utils:postorder(G)),
    ?line 10 = length(digraph_utils:preorder(G)),
    ?line ok = evall(digraph_utils:components(G), 
		     [[a],[b,c,d],[e,f,g],[h],[i,j]]),
    ?line ok = evall(digraph_utils:strong_components(G), 
	       [[a],[b],[c],[d],[e,f,g],[h],[i],[j]]),
    ?line ok = evall(digraph_utils:cyclic_strong_components(G), 
		     [[e,f,g],[h],[i]]),
    ?line true = path(G, e, e),
    ?line false = path(G, e, j),
    ?line false = path(G, a, a),
    ?line false = digraph_utils:topsort(G),
    ?line false = digraph_utils:is_acyclic(G),
    ?line ok = eval(digraph_utils:loop_vertices(G), [h,i]),
    ?line ok = eval(digraph_utils:reaching([e], G), [e,f,g]),
    ?line ok = eval(digraph_utils:reaching_neighbours([e], G), [e,f,g]),
    ?line ok = eval(digraph_utils:reachable([e], G), [e,f,g]),
    ?line ok = eval(digraph_utils:reachable_neighbours([e], G), [e,f,g]),
    ?line ok = eval(digraph_utils:reaching([b], G), [b]),
    ?line ok = eval(digraph_utils:reaching_neighbours([b], G), []),
    ?line ok = eval(digraph_utils:reachable([b], G), [b,c,d]),
    ?line ok = eval(digraph_utils:reachable_neighbours([b], G), [c,d]),
    ?line ok = eval(digraph_utils:reaching([h], G), [h]),
    ?line ok = eval(digraph_utils:reaching_neighbours([h], G), [h]),
    ?line ok = eval(digraph_utils:reachable([h], G), [h]),
    ?line ok = eval(digraph_utils:reachable_neighbours([h], G), [h]),
    ?line ok = eval(digraph_utils:reachable([e,f], G), [e,f,g]),
    ?line ok = eval(digraph_utils:reachable_neighbours([e,f], G), [e,f,g]),
    ?line ok = eval(digraph_utils:reachable([h,h,h], G), [h]),
    ?line true = digraph:delete(G),
    ok.

loop(doc) -> [];
loop(suite) -> [];
loop(Config) when is_list(Config) ->
    ?line G = digraph:new(),
    ?line add_vertices(G, [a,b]),
    ?line add_edges(G, [{a,a},{b,b}]),
    ?line ok = evall(digraph_utils:components(G), [[a],[b]]),
    ?line ok = evall(digraph_utils:strong_components(G), [[a],[b]]),
    ?line ok = evall(digraph_utils:cyclic_strong_components(G), [[a],[b]]),
    ?line [_,_] = digraph_utils:topsort(G),
    ?line false = digraph_utils:is_acyclic(G),	
    ?line ok = eval(digraph_utils:loop_vertices(G), [a,b]),
    ?line [_,_] = digraph_utils:preorder(G),
    ?line [_,_] = digraph_utils:postorder(G),
    ?line ok = eval(digraph_utils:reaching([b], G), [b]),
    ?line ok = eval(digraph_utils:reaching_neighbours([b], G), [b]),
    ?line ok = eval(digraph_utils:reachable([b], G), [b]),
    ?line ok = eval(digraph_utils:reachable_neighbours([b], G), [b]),
    ?line true = path(G, a, a),
    ?line true = digraph:delete(G),
    ok.

isolated(doc) -> [];
isolated(suite) -> [];
isolated(Config) when is_list(Config) ->
    ?line G = digraph:new(),
    ?line add_vertices(G, [a,b]),
    ?line ok = evall(digraph_utils:components(G), [[a],[b]]),
    ?line ok = evall(digraph_utils:strong_components(G), [[a],[b]]),
    ?line ok = evall(digraph_utils:cyclic_strong_components(G), []),
    ?line [_,_] = digraph_utils:topsort(G),
    ?line true = digraph_utils:is_acyclic(G),	
    ?line ok = eval(digraph_utils:loop_vertices(G), []),
    ?line [_,_] = digraph_utils:preorder(G),
    ?line [_,_] = digraph_utils:postorder(G),
    ?line ok = eval(digraph_utils:reaching([b], G), [b]),
    ?line ok = eval(digraph_utils:reaching_neighbours([b], G), []),
    ?line ok = eval(digraph_utils:reachable([b], G), [b]),
    ?line ok = eval(digraph_utils:reachable_neighbours([b], G), []),
    ?line false = path(G, a, a),
    ?line true = digraph:delete(G),
    ok.

topsort(doc) -> [];
topsort(suite) -> [];
topsort(Config) when is_list(Config) ->
    ?line G = digraph:new(),
    ?line add_edges(G, [{a,b},{b,c},{c,d},{d,e},{e,f}]),
    ?line ok = eval(digraph_utils:topsort(G), [a,b,c,d,e,f]),
    ?line true = digraph:delete(G),
    ok.

subgraph(doc) -> [];
subgraph(suite) -> [];
subgraph(Config) when is_list(Config) ->
    ?line G = digraph:new([acyclic]),
    ?line add_edges(G, [{b,c},{b,d},{e,f},{f,fg,fgl,g},{f,fg2,fgl2,g},{g,e},
			{h,h},{i,i},{i,j}]),
    ?line add_vertices(G, [{b,bl},{f,fl}]),
    ?line SG = digraph_utils:subgraph(G, [u1,b,c,u2,f,g,i,u3]),
    ?line [b,c,f,g,i] = lists:sort(digraph:vertices(SG)),
    ?line {b,bl} = digraph:vertex(SG, b),
    ?line {c,[]} = digraph:vertex(SG, c),
    ?line {fg,f,g,fgl} = digraph:edge(SG, fg),
    ?line {fg2,f,g,fgl2} = digraph:edge(SG, fg2),
    ?line {_, {_, acyclic}} = lists:keysearch(cyclicity, 1, digraph:info(SG)),
    ?line true = digraph:delete(SG),

    ?line SG1 = digraph_utils:subgraph(G, [f, g, h], 
				       [{type, []}, {keep_labels, false}]),
    ?line [f,g,h] = lists:sort(digraph:vertices(SG1)),
    ?line {f,[]} = digraph:vertex(SG1, f),
    ?line {fg,f,g,[]} = digraph:edge(SG1, fg),
    ?line {_, {_, cyclic}} = lists:keysearch(cyclicity, 1, digraph:info(SG1)),
    ?line true = digraph:delete(SG1),
    
    ?line SG2 = digraph_utils:subgraph(G, [f, g, h], 
				       [{type, [acyclic]}, 
					{keep_labels, true}]),
    ?line [f,g,h] = lists:sort(digraph:vertices(SG2)),
    ?line {f,fl} = digraph:vertex(SG2, f),
    ?line {fg,f,g,fgl} = digraph:edge(SG2, fg),
    ?line {_, {_, acyclic}} = lists:keysearch(cyclicity, 1, digraph:info(SG2)),
    ?line true = digraph:delete(SG2),
    
    ?line {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{invalid, opt}])),
    ?line {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{keep_labels, not_Bool}])),
    ?line {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{type, not_type}])),
    ?line {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], [{type, [not_type]}])),
    ?line {'EXIT',{badarg,_}} =
	(catch digraph_utils:subgraph(G, [f], not_a_list)),

    ?line true = digraph:delete(G),

    ok.

condensation(doc) -> [];
condensation(suite) -> [];
condensation(Config) when is_list(Config) ->
    ?line G = digraph:new([]),
    ?line add_edges(G, [{b,c},{b,d},{e,f},{f,fg,fgl,g},{f,fg2,fgl2,g},{g,e},
			{h,h},{j,i},{i,j}]),
    ?line add_vertices(G, [q]),
    ?line CG = digraph_utils:condensation(G),
    ?line Vs = sort_2(digraph:vertices(CG)),
    ?line [[b],[c],[d],[e,f,g],[h],[i,j],[q]] = Vs,
    ?line Fun = fun(E) -> 
			{_E, V1, V2, _L} = digraph:edge(CG, E), 
			{lists:sort(V1), lists:sort(V2)} 
		end,
    ?line Es = lists:map(Fun, digraph:edges(CG)),
    ?line [{[b],[c]},{[b],[d]}] = lists:sort(Es),
    ?line true = digraph:delete(CG),
    ?line true = digraph:delete(G),
    ok.

tree(doc) -> ["OTP-7081"];
tree(suite) -> [];
tree(Config) when is_list(Config) ->
    ?line false = is_tree([], []),
    ?line true = is_tree([a], []),
    ?line false = is_tree([a,b], []),
    ?line true = is_tree([{a,b}]),
    ?line false = is_tree([{a,b},{b,a}]),
    ?line true = is_tree([{a,b},{a,c},{b,d},{b,e}]),
    ?line false = is_tree([{a,b},{a,c},{b,d},{b,e}, {d,e}]),
    ?line false = is_tree([{a,b},{a,c},{b,d},{b,e}, {b,e}]),
    ?line true = is_tree([{a,c},{c,b}]),
    ?line true = is_tree([{b,a},{c,a}]),
    %% Parallel edges. Acyclic and with one componets
    %% (according to the digraph module).
    ?line false = is_tree([{a,b},{a,b}]),

    ?line no = arborescence_root([], []),
    ?line {yes, a} = arborescence_root([a], []),
    ?line no = arborescence_root([a,b], []),
    ?line {yes, a} = arborescence_root([{a,b}]),
    ?line no = arborescence_root([{a,b},{b,a}]),
    ?line {yes, a} = arborescence_root([{a,b},{a,c},{b,d},{b,e}]),
    ?line no = arborescence_root([{a,b},{a,c},{b,d},{b,e}, {d,e}]),
    ?line no = arborescence_root([{a,b},{a,c},{b,d},{b,e}, {b,e}]),
    ?line {yes, a} = arborescence_root([{a,c},{c,b}]),
    ?line no = arborescence_root([{b,a},{c,a}]),

    ?line false = is_arborescence([], []),
    ?line true = is_arborescence([a], []),
    ?line false = is_arborescence([a,b], []),
    ?line true = is_arborescence([{a,b}]),
    ?line false = is_arborescence([{a,b},{b,a}]),
    ?line true = is_arborescence([{a,b},{a,c},{b,d},{b,e}]),
    ?line false = is_arborescence([{a,b},{a,c},{b,d},{b,e}, {d,e}]),
    ?line false = is_arborescence([{a,b},{a,c},{b,d},{b,e}, {b,e}]),
    ?line true = is_arborescence([{a,c},{c,b}]),
    ?line false = is_arborescence([{b,a},{c,a}]),

    %% Parallel edges.
    ?line false = is_arborescence([{a,b},{a,b}]),

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
