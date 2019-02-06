%% -*- Erlang -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% IDENTIFIES THE EXTENDED BASIC BLOCKS OF A CFG
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([cfg/1,
	 %% dag/2,
	 type/1,
	 node_label/1,
	 node_successors/1
	]).
-ifdef(DEBUG_EBB).
-export([pp/1]).
-endif.

-define(cfg, ?CFG).

%%--------------------------------------------------------------------
%% The extended basic block datatype
%%
%% An EBB is identified with the label of the root node. 
%% It's a tree
%%
%% EBB :=  {ebb_node, Label, [EBB]}
%%      |  {ebb_leaf, SuccesorLabel}
%%--------------------------------------------------------------------

-type ebb() :: ebb_node()
	     | ebb_leaf().

-record(ebb_node, {label :: icode_lbl(), successors :: [ebb()]}).
-type ebb_node() :: #ebb_node{}.

-record(ebb_leaf, {successor :: icode_lbl()}).
-type ebb_leaf() :: #ebb_leaf{}.

%%--------------------------------------------------------------------
%% Returns a list of extended basic blocks.
%%--------------------------------------------------------------------

-spec cfg(cfg()) -> [ebb()].

cfg(CFG) ->
  Start = ?cfg:start_label(CFG),
  Labels = ?cfg:reverse_postorder(CFG),
  Roots = [Start],
  Blocks = Labels -- Roots,
  Visited = new_visited(),
  build_all_ebb(Roots, Blocks, Visited, CFG).

new_visited() ->
  gb_sets:empty().
visited(L, Visited) ->
  gb_sets:is_member(L, Visited).
visit(L, Visited) ->
  gb_sets:add(L, Visited).

build_all_ebb(Roots, Blocks, Visited, CFG) ->
  build_all_ebb(Roots, Blocks, Visited, CFG, []).

build_all_ebb([], [], _, _CFG, Ebbs) ->
  lists:reverse(Ebbs);
build_all_ebb([], [BlockLeft|BlocksLeft], Visited, CFG, Ebbs) ->
  case visited(BlockLeft, Visited) of
    true -> 
      build_all_ebb([], BlocksLeft, Visited, CFG, Ebbs);
    false ->
      build_all_ebb([BlockLeft], BlocksLeft, Visited, CFG, Ebbs)
  end;
build_all_ebb([Root|Roots], Blocks, Visited, CFG, Ebbs) ->
  {Ebb, NewVisited} = build_ebb(Root, Visited, CFG),
  build_all_ebb(Roots, Blocks, NewVisited, CFG, [Ebb|Ebbs]).

%%
%% Build the extended basic block with Lbl as its root.
%%

build_ebb(Lbl, Visited, CFG) ->
  build_ebb(Lbl, Visited, 
	    fun (NodeL, NewVisited) -> {NodeL, NewVisited} end,
	    [], CFG).

build_ebb(Lbl, Visited, MkFun, EBBs, CFG) ->
  Succ = ?cfg:succ(CFG, Lbl),
  add_succ(Succ, visit(Lbl, Visited), Lbl, MkFun, EBBs, CFG).

add_succ([], Visited, Node, MkFun, EBBs, _CFG) ->
  MkFun(mk_node(Node, lists:reverse(EBBs)), Visited);
add_succ([Lbl|Lbls], Visited, Node, MkFun, EBBs, CFG) ->
  case [visited(Lbl, Visited)|?cfg:pred(CFG, Lbl)] of
    [false,_] ->
      build_ebb(Lbl, Visited, 
		fun (NewEbb, Visited0) ->
		    add_succ(Lbls, Visited0, Node, MkFun, [NewEbb|EBBs], CFG)
		end, [], CFG);
    _ ->
      add_succ(Lbls, Visited, Node, MkFun, [mk_leaf(Lbl)|EBBs], CFG)
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Generate a list of dags.
%%

%% dag(EBBs, CFG) ->
%%   Start = ?cfg:start_label(CFG),
%%   Roots = [Start],
%%   Edges = all_adges(EBBs, Roots),
%%   start_dag(Roots, Edges, []).
%% 
%% start_dag([], _Edges, _Visit) ->
%%   [];
%% start_dag([Root|Roots], Edges, Visit) ->
%%   case lists:member(Root, Visit) of
%%     true ->
%%       start_dag(Roots, Edges, Visit);
%%     false ->
%%       {Dag, Roots0, Visit0} =
%% 	fill_dag(Root, [Root], Edges, Roots, [Root|Visit]),
%%       [lists:reverse(Dag) | start_dag(Roots0, Edges, Visit0)]
%%   end.
%% 
%% fill_dag(Lbl, Dag, Edges, Roots, Visit) ->
%%   Succ = find_succ(Lbl, Edges),
%%   add_dag_succ(Succ, Dag, Edges, Roots, Visit).
%% 
%% add_dag_succ([], Dag, _Edges, Roots, Visit) ->
%%   {Dag, Roots, Visit};
%% add_dag_succ([S|Ss], Dag, Edges, Roots, Visit) ->
%%   {Dag0, Roots0, Visit0} = add_dag_succ(Ss, Dag, Edges, Roots, Visit),
%%   Pred = find_pred(S, Edges),
%%   case all_in(Pred, Dag0) of
%%     true ->
%%       fill_dag(S, [S|Dag0], Edges, Roots0, [S|Visit0]);
%%     false ->
%%       {Dag0, [S|Roots], Visit0}
%%   end.
%% 
%% find_succ(_Lbl, []) ->
%%   [];
%% find_succ(Lbl, [{Lbl, Succ}|Edges]) ->
%%   [Succ | find_succ(Lbl, Edges)];
%% find_succ(Lbl, [_|Edges]) ->
%%   find_succ(Lbl, Edges).
%% 
%% find_pred(_Lbl, []) ->
%%   [];
%% find_pred(Lbl, [{Pred, Lbl}|Edges]) ->
%%   [Pred | find_pred(Lbl, Edges)];
%% find_pred(Lbl, [_|Edges]) ->
%%   find_pred(Lbl, Edges).
%% 
%% all_edges([], _Roots) ->
%%   [];
%% all_edges([EBB|EBBs], Roots) ->
%%   succ_edges(node_label(EBB), ebb_successors(EBB), EBBs, Roots).
%% 
%% succ_edges(Lbl, [], EBBs, Roots) ->
%%   case lists:member(Lbl, Roots) of
%%     true ->
%%       [{start, Lbl} | all_edges(EBBs, Roots)];
%%     false ->
%%       all_edges(EBBs, Roots)
%%   end;
%% succ_edges(Lbl, [S|Ss], EBBs, Roots) ->
%%   [{Lbl, S} | succ_edges(Lbl, Ss, EBBs, Roots)].
%% 
%% all_in([], _List) ->
%%   true;
%% all_in([X|Xs], List) ->
%%   lists:member(X, List) andalso all_in(Xs, List).
%% 
%% find_ebb(Lbl, [EBB|EBBs]) ->
%%   case node_label(EBB) of
%%      Lbl ->
%%	 EBB;
%%      _ ->
%%	 find_ebb(Lbl, EBBs)
%%   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec mk_node(icode_lbl(), [ebb()]) -> ebb_node().
mk_node(Label, Successors) -> #ebb_node{label=Label, successors=Successors}.

-spec node_label(#ebb_node{}) -> icode_lbl().
node_label(#ebb_node{label=Label}) -> Label.

-spec node_successors(#ebb_node{}) -> [ebb()].
node_successors(#ebb_node{successors=Successors}) -> Successors.

-spec mk_leaf(icode_lbl()) -> ebb_leaf().
mk_leaf(NextEbb) -> #ebb_leaf{successor=NextEbb}.
%% leaf_next(Leaf) -> Leaf#ebb_leaf.successor.

-spec type(ebb_node()) -> 'node' ; (ebb_leaf()) -> 'leaf'.
type(#ebb_node{}) -> node;
type(#ebb_leaf{}) -> leaf.

%% ebb_successors(EBB) ->
%%   ordsets:from_list(ebb_successors0(EBB)).
%% 
%% ebb_successors0(#ebb_leaf{successor=NextEBB}) ->
%%   [NextEBB];
%% ebb_successors0(#ebb_node{successors=SuccessorNodes}) ->
%%   lists:append(lists:map(fun ebb_successors0/1, SuccessorNodes)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Prettyprint a list of extended basic blocks
%%

-ifdef(DEBUG_EBB).

pp(EBBs) ->
  lists:map(fun(E) -> pp(E, 0) end, EBBs).

pp(EBB, Indent) ->
  io:format([$~]++integer_to_list(Indent)++[$c],[$ ]),
  case type(EBB) of
    node ->
      io:format("~w~n", [node_label(EBB)]),
      lists:map(fun(E) -> pp(E, Indent+3) end, node_successors(EBB));
    leaf ->
      io:format("* -> ~w~n", [leaf_next(EBB)])
  end.

-endif.
