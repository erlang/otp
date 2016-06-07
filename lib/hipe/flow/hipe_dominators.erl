%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%------------------------------------------------------------------------
%% File    : hipe_dominators.erl
%% Author  : Christoffer Vikström <chvi3471@student.uu.se>
%%           Daniel Deogun        <dade4543@student.uu.se>
%%           Jesper Bengtsson     <jebe8371@student.uu.se>
%% Created : 18 Mar 2002
%%
%% @doc
%%   Contains utilities for creating and manipulating dominator trees
%%   and dominance frontiers from a CFG.
%% @end
%%------------------------------------------------------------------------ 
-module(hipe_dominators).

-export([domTree_create/1,
	 domTree_getChildren/2,
	 domTree_dominates/3,
	 domFrontier_create/2,
	 domFrontier_get/2]).

-export_type([domTree/0]).

-include("cfg.hrl").

%%========================================================================
%%
%% CODE FOR CREATING AND MANIPULATING DOMINATOR TREES.
%%
%%========================================================================

-record(workDataCell, {dfnum = 0       :: non_neg_integer(),
		       dfparent = none :: 'none' | cfg_lbl(),
		       semi = none     :: 'none' | cfg_lbl(),
		       ancestor = none :: 'none' | cfg_lbl(),
		       best = none     :: 'none' | cfg_lbl(),
		       samedom = none  :: 'none' | cfg_lbl(), 
		       bucket = []     :: [cfg_lbl()]}).

-record(domTree, {root                     :: cfg_lbl(),
		  size  = 0		   :: non_neg_integer(),
		  nodes = gb_trees:empty() :: gb_trees:tree()}).
-opaque domTree() :: #domTree{}.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_create/1
%% Purpose   : Creates a complete dominator tree given a CFG.
%% Arguments : CFG - a Control Flow Graph representation
%% Returns   : A dominator tree
%%>----------------------------------------------------------------------<

-spec domTree_create(cfg()) -> domTree().

domTree_create(CFG) ->
  {WorkData, DFS, N} = dfs(CFG),
  DomTree = domTree_empty(hipe_gen_cfg:start_label(CFG)),
  {DomData, WorkData2} = getIdoms(CFG, DomTree, WorkData, N, DFS),
  finalize(WorkData2, DomData, 1, N, DFS).

%%>----------------------------------------------------------------------<
%% Procedure : domTree_empty/0
%% Purpose   : Creates an empty dominator tree.
%% Arguments : The root node
%% Returns   : A dominator tree
%%>----------------------------------------------------------------------<

domTree_empty(Node) ->
  #domTree{root = Node}.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_createNode/2
%% Purpose   : Creates a new node and inserts it into the dominator tree. 
%% Arguments : Node    - The new node
%%             DomTree - The target dominator tree
%% Returns   : A dominator tree
%%>----------------------------------------------------------------------<

domTree_createNode(Node, DomTree) ->
  DomTree2 = domTree_setNodes(DomTree,
			      gb_trees:enter(Node, {none,[]},
					     domTree_getNodes(DomTree))),
  domTree_incSize(DomTree2).

%%>----------------------------------------------------------------------<
%% Procedure : domTree_getNode/2
%% Purpose   : Returns a specific node in the dominator tree.
%% Arguments : Node - The new node
%%             DomTree - The target dominator tree
%% Returns   : Node
%%>----------------------------------------------------------------------<

domTree_getNode(Node, DomTree) ->
  gb_trees:lookup(Node, domTree_getNodes(DomTree)).

%%>----------------------------------------------------------------------<
%% Procedure : domTree_getNodes/1
%% Purpose   : Retrieves the nodes from a dominator tree.
%% Arguments : DomTree - The target dominator tree
%% Returns   : A map containing the nodes of the dominator tree.
%%>----------------------------------------------------------------------<

domTree_getNodes(#domTree{nodes=Nodes}) -> Nodes.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_setNodes/2
%% Purpose   : Replaces the set of nodes in a dominator tree with a 
%%             new set of nodes.
%% Arguments : Nodes   - The new set of nodes
%%             DomTree - The target dominator tree
%% Returns   : DomTree
%%>----------------------------------------------------------------------<

domTree_setNodes(DomTree, Nodes) -> DomTree#domTree{nodes = Nodes}.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_setSize/2
%% Purpose   : Sets the size of the dominator tree, i.e. the number of 
%%             nodes in it.
%% Arguments : Size    - The new size of the target dominator tree
%%             DomTree - The target dominator tree
%% Returns   : A dominator tree
%%>----------------------------------------------------------------------<

domTree_setSize(DomTree, Size) -> DomTree#domTree{size = Size}.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_incSize/1
%% Purpose   : Increases the size of the dominator tree with one.
%% Arguments : DomTree - The target dominator tree
%% Returns   : DomTree
%%>----------------------------------------------------------------------<

domTree_incSize(DomTree) ->
  Size = domTree_getSize(DomTree),
  domTree_setSize(DomTree, Size + 1).

%%>----------------------------------------------------------------------<
%% Procedure : get IDom/2
%% Purpose   : Retrieves the immediate dominators of a node in the 
%%             dominator tree.
%% Arguments : Node    - The new node
%%             DomTree - The target dominator tree
%% Returns   : The immediate dominator
%%>----------------------------------------------------------------------<

domTree_getIDom(Node, DomTree) ->
  case domTree_getNode(Node, DomTree) of
    {value, {IDom, _}} ->
      IDom;
    none ->
      []
  end.

%%>----------------------------------------------------------------------<
%% Procedure : getChildren/2
%% Purpose   : Retrieves the children of a node in the dominator tree.
%% Arguments : Node    - The new node
%%             DomTree - The target dominator tree
%% Returns   : [children]
%%>----------------------------------------------------------------------<

-spec domTree_getChildren(cfg_lbl(), domTree()) -> [cfg_lbl()].

domTree_getChildren(Node, DomTree) ->
  case domTree_getNode(Node, DomTree) of
    {value, {_, Children}} ->
      Children;
    none ->
      []
  end.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_getSize/1
%% Purpose   : Retrieves the size of a dominator tree.
%% Arguments : DomTree - The target dominator tree
%% Returns   : A number denoting the size of the dominator tree
%%>----------------------------------------------------------------------<

domTree_getSize(#domTree{size=Size}) -> Size.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_getRoot/2
%% Purpose   : Retrieves the number of the root node in the dominator tree.
%% Arguments : DomTree - The target dominator tree
%% Returns   : Number
%%>----------------------------------------------------------------------<

domTree_getRoot(#domTree{root=Root}) -> Root.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_addChild/3
%% Purpose   : Inserts a new node as a child to another node in the
%%             dominator tree.
%% Arguments : Node    - The old node that should get a new child
%%             Child   - The new child node
%%             DomTree - The target dominator tree
%% Returns   : DomTree
%%>----------------------------------------------------------------------<

domTree_addChild(Node, Child, DomTree) ->
  {IDom, Children} = case domTree_getNode(Node, DomTree) of
		       {value, Tuple} ->
			 Tuple;
		       none ->
			 {none, []}
		     end,
  Nodes = case lists:member(Child, Children) of 
	    true ->
	      domTree_getNodes(DomTree);
	    false ->
	      gb_trees:enter(Node, {IDom, [Child|Children]},
			     domTree_getNodes(DomTree))
	  end,
  domTree_setNodes(DomTree, Nodes).

%%>----------------------------------------------------------------------<
%% Procedure : setIDom/3
%% Purpose   : Sets the immediate domminator of a node in the domminator tree.
%% Arguments : Node    - The node whose immediate domminator we are seting
%%             IDom    - The immediate domminator
%%             DomTree - The target dominator tree
%% Returns   : DomTree
%% Notes     : Is used to build the dominator tree.
%%>----------------------------------------------------------------------<

setIDom(Node, IDom, DomTree) ->
  DomTree1 = case domTree_getNode(Node, DomTree) of
	       none ->
		 domTree_createNode(Node, DomTree);
	       _ ->
		 DomTree
	     end,
  DomTree2 = domTree_addChild(IDom, Node, DomTree1),
  {value, {_, Children}} = domTree_getNode(Node, DomTree2),
  domTree_setNodes(DomTree2,
		   gb_trees:enter(Node, {IDom, Children},
				  domTree_getNodes(DomTree2))).

%%>----------------------------------------------------------------------<
%% Procedure : lookup
%% Purpose   : This function is used as a wrapper for the lookup function.
%%             The function retrieves a particular element (defined by
%%             Field) stored in a workDataCell in the table (defined by
%%             Table).
%% Arguments : Field - Value defined in the workDataCell record
%%             Key   - Value used as a key in the table
%%             Table - Table storing workDataCells
%% Returns   : A value defined in the workDataCell record
%%>----------------------------------------------------------------------<

lookup({Field, Key}, Table) when is_integer(Key) ->
  WD = lookup_table(Key, Table),
  case Field of
    ancestor -> WD#workDataCell.ancestor;
    best     -> WD#workDataCell.best;
    bucket   -> WD#workDataCell.bucket;
    dfnum    -> WD#workDataCell.dfnum; 
    dfparent -> WD#workDataCell.dfparent; 
    samedom  -> WD#workDataCell.samedom;
    semi     -> WD#workDataCell.semi    
  end.

lookup_table(Key, Table) when is_integer(Key) ->
  case gb_trees:lookup(Key, Table) of
    {value, Data} ->
      Data;
    none ->
      #workDataCell{}
  end.

%%>----------------------------------------------------------------------<
%% Procedure : update
%% Purpose   : This function is used as a wrapper for the update function 
%%             The main purpose of the update function is therefore
%%             change a particular cell in the table (Table) to the
%%             value given as an argument (Value).
%% Arguments : Key   - Value used as a key in the table
%%             Field - Value defined in the workDataCell record.
%%             Value - The new value that should replace the old in the table
%%             Table - Table storing workDataCells
%% Returns   : NewTable               
%%>----------------------------------------------------------------------<

update(Key, {Field, Value}, Table) ->
  gb_trees:enter(Key, updateCell(Value, Field, lookup_table(Key, Table)), Table);
update(Key, List, Table) ->
  gb_trees:enter(Key, update(List, lookup_table(Key, Table)), Table).

update([{Field, Value} | T], WD) -> 
  update(T, updateCell(Value, Field, WD));
update([], WD) -> WD.

updateCell(Value, Field, WD) ->
  case Field of
    dfnum    -> WD#workDataCell{dfnum   = Value}; 
    dfparent -> WD#workDataCell{dfparent= Value}; 
    semi     -> WD#workDataCell{semi    = Value}; 
    ancestor -> WD#workDataCell{ancestor= Value}; 
    best     -> WD#workDataCell{best    = Value}; 
    samedom  -> WD#workDataCell{samedom = Value}; 
    bucket   -> WD#workDataCell{bucket  = Value}
  end.

%%>----------------------------------------------------------------------<
%% Procedure : dfs/1
%% Purpose   : The main purpose of this function is to traverse the CFG in
%%             a depth first order. It is aslo used to initialize certain 
%%             elements defined in a workDataCell.
%% Arguments : CFG - a Control Flow Graph representation
%% Returns   : A table (WorkData) and the total number of elements in
%%             the CFG.
%%>----------------------------------------------------------------------<

dfs(CFG) ->
  {WorkData, DFS, N} = dfs(CFG, hipe_gen_cfg:start_label(CFG), 
			   none, 1, gb_trees:empty(), gb_trees:empty()),
  {WorkData, DFS, N-1}.

dfs(CFG, Node, Parent, N, WorkData, DFS) ->
  case lookup({dfnum, Node}, WorkData) of
    0 -> 	  
      WorkData2 = update(Node, [{dfnum, N}, {dfparent, Parent}, 
				{semi, Node}, {best, Node}], WorkData),
      DFS2 = gb_trees:enter(N, Node, DFS),
      dfsTraverse(hipe_gen_cfg:succ(CFG, Node), CFG, Node, 
		  N + 1, WorkData2, DFS2);
    _ -> {WorkData, DFS, N}
  end.

%%>----------------------------------------------------------------------<
%% Procedure : dfsTraverse/6
%% Purpose   : This function acts as a help function for the dfs algorithm
%%             in the sence that it traverses a list of nodes given by the 
%%             CFG. 
%% Arguments : Node     - The first element in the node list
%%             SuccLst  - The remainder of the node list
%%             CFG      - Control Flow Graph representation
%%             Parent   - Node representing the parent of the Node defined
%%                        above.
%%             N        - The total number of processed nodes.
%%             WorkData - Table consisting of workDataCells
%% Returns   : An updated version of the table (WorkData) and the 
%%             total number of nodes processed.
%%>----------------------------------------------------------------------<

dfsTraverse([Node|T], CFG, Parent, N, WorkData, DFS) ->
  {WorkData2, DFS2, N2} = dfs(CFG, Node, Parent, N, WorkData, DFS),
  dfsTraverse(T, CFG, Parent, N2, WorkData2, DFS2);
dfsTraverse([], _, _, N, WorkData, DFS) -> {WorkData, DFS, N}.

%%>----------------------------------------------------------------------<
%% Procedure : getIdoms/6
%% Purpose   : The purpose of this function is to compute the immediate
%%             dominators. This is accomplished by traversing the CFG nodes
%%             by their depth first number in a bottom up manner. That is, 
%%             the nodes are processed in a backward order (highest to 
%%             lowest number).
%% Arguments : CFG      - Control Flow Graph representation
%%             DomData  - Table consisting of domTree cells
%%             WorkData - Table consisting of workDataCells
%%             Index    - The index used for retrieving the node to be 
%%                        processed
%% Returns   : An updated version of the tables DomData and WorkData
%%>----------------------------------------------------------------------<

getIdoms(CFG, DomData, WorkData, Index, DFS)
     when is_integer(Index), Index > 1 ->
  Node = lookup_table(Index, DFS),
  PredLst = hipe_gen_cfg:pred(CFG, Node),
  Par = lookup({dfparent, Node}, WorkData),
  DfNumN = lookup({dfnum, Node}, WorkData),
  {S, WorkData2} = getSemiDominator(PredLst, DfNumN, Par, WorkData),
  WorkData3 = update(Node, {semi, S}, WorkData2),
  OldBucket = lookup({bucket, S}, WorkData3),
  WorkData4 = update(S, {bucket, [Node | OldBucket]}, WorkData3),
  WorkData5 = linkTrees(Par, Node, WorkData4),
  {WorkData6, DomData2} = filterBucket(lookup({bucket, Par}, WorkData5), 
				       Par, WorkData5, DomData),
  WorkData7 = update(Par, {bucket, []}, WorkData6),
  getIdoms(CFG, DomData2, WorkData7, Index - 1, DFS);
getIdoms(_, DomData, WorkData, 1, _) ->
  {DomData, WorkData}.

%%>----------------------------------------------------------------------<
%% Procedure : getSemiDominator/4
%% Purpose   : The main purpose of this algorithm is to compute the semi 
%%             dominator of the node Node based on the Semidominator Theorem
%% Arguments : Preds    - The list of predecessors of the node Node
%%             Node     - Node in the CFG
%%             S        - Parent of node Node (depth first parent)
%%             WorkData - Table consisting of workDataCells
%% Returns   : A tuple containing the semidominator and an updated version
%%             of the table WorkData.
%%>----------------------------------------------------------------------<

getSemiDominator([Pred|Preds], DfNumChild, S, WorkData) ->
  {Sp, WorkData3} = 
    case lookup({dfnum, Pred}, WorkData) =< DfNumChild of
      true  -> 
	{Pred, WorkData};
      false ->  
	{AncLowSemi, WorkData2} = getAncestorWithLowestSemi(Pred, WorkData),	
	{lookup({semi, AncLowSemi}, WorkData2), WorkData2}
    end,
  S2 = case lookup({dfnum, Sp}, WorkData3) < lookup({dfnum, S}, WorkData3) of
	 true  -> Sp;
	 false -> S
       end,
  getSemiDominator(Preds, DfNumChild, S2, WorkData3);
getSemiDominator([], _, S, WorkData) ->
  {S, WorkData}.

%%>----------------------------------------------------------------------<
%% Procedure : getAncestorWithLowestSemi/2
%% Purpose   : The main purpose of this function is to retrieve the ancestor 
%%             of a node with the lowest depth first number (semi). The
%%             function is also using path compression, i.e. it remembers the
%%             best node (the one with the lowest semi number) and hence the
%%             algorithm is only processing the minimal number of nodes.
%% Arguments : Node     - Node in the tree
%%             WorkData - Table consisting of workDataCells
%% Returns   : A node (the one with the lowest semi) and an updated version
%%             of the table WorkData.
%%>----------------------------------------------------------------------<

getAncestorWithLowestSemi(Node, WorkData) ->
  Best = lookup({best, Node}, WorkData),
  case lookup({ancestor, Node}, WorkData) of
    none -> {Best, WorkData};
    A -> 
      case lookup({ancestor, A}, WorkData) of
	none -> 
	  {Best, WorkData};
	_ -> 
	  {B, WorkData2} = getAncestorWithLowestSemi(A, WorkData),
	  AncA = lookup({ancestor, A}, WorkData2),
	  WorkData3 = update(Node, {ancestor, AncA}, WorkData2),
	  DfSemiB = lookup({dfnum, lookup({semi, B}, WorkData3)}, WorkData3),
	  BestN = lookup({best, Node}, WorkData3),
	  SemiB = lookup({semi, BestN}, WorkData3),
	  DfSemiBestN = lookup({dfnum, SemiB}, WorkData3),
	  case DfSemiB < DfSemiBestN of
	    true  ->
	      {B, update(Node, {best, B}, WorkData3)};
	    false -> 
	      {BestN, WorkData3}
	  end
      end
  end.

%%>----------------------------------------------------------------------<
%% Procedure : linkTrees/3
%% Purpose   : The main purpose of this function is to combine two trees
%%             into one (accomplished by setting the ancestor for node 
%%             Node to Parent). The algorithm is also updating the best field
%%             in the workDataCell for node Node to the value of itself.
%% Arguments : Parent   - The parent of the node Node.
%%             Node     - The node to process
%%             WorkData - Table consisting of workDataCells
%% Returns   : An updated version of table WorkData
%%>----------------------------------------------------------------------<

linkTrees(Parent, Node, WorkData) ->
  update(Node, [{ancestor, Parent}, {best, Node}], WorkData).

%%>----------------------------------------------------------------------<
%% Procedure : filterBucket/4 
%% Purpose   : The purpose of this algorith is to compute the dominator of
%%             the node Node by utilizing the first clause of the Dominator
%%             Theorem. If the first clause of the theorem doesn't apply 
%%             then the computation of that particular node is deferred to
%%             a later stage (see finalize).
%% Arguments : Nodes    - The list of CFG nodes that need to be computed.
%%             Parent   - The parent of the nodes in the list Nodes
%%             WorkData - Table consisting of workDataCells
%%             DomData  - Table consisting of domTree cells.
%% Returns   : An updated version of the tables WorkData and DomData
%%>----------------------------------------------------------------------<

filterBucket([Node|Nodes], Parent, WorkData, DomData) ->
  {Y, WorkData2} = getAncestorWithLowestSemi(Node, WorkData),
  {WorkData3, DomData2} = 
    case lookup({semi, Y}, WorkData2) =:= lookup({semi, Node}, WorkData2) of
      true  -> {WorkData2, setIDom(Node, Parent, DomData)};
      false -> {update(Node, {samedom, Y}, WorkData2), DomData}
    end,
  filterBucket(Nodes, Parent, WorkData3, DomData2);
filterBucket([], _, WorkData, DomData) ->
  {WorkData, DomData}.	     

%%>----------------------------------------------------------------------<
%% Procedure : finalize/5
%% Purpose   : This algorithm finishes up the second clause of the Dominator
%%             Theorem. Hence, the main purpose of this function is therefore
%%             to update the dominator tree with the nodes that were deferred
%%             in the filterBucket algorithm.
%% Arguments : WorkData - Table consisting of workDataCells
%%             DomData  - Table consisting of domTree cells
%%             N        - The index used for retrieving the node to be 
%%                        processed
%%             Max      - Maximum node index
%% Returns   : An updated version of the table DomData
%%>----------------------------------------------------------------------<

finalize(WorkData, DomData, N, Max, DFS) when N =< Max ->
  Node = lookup_table(N, DFS),
  case lookup({samedom, Node}, WorkData) of
    none ->
      finalize(WorkData, DomData, N + 1, Max, DFS);
    SameDomN -> 
      case domTree_getIDom(SameDomN, DomData) of
	IdomSameDomN when is_integer(IdomSameDomN) ->
	  DomData2 = setIDom(Node, IdomSameDomN, DomData),
	  finalize(WorkData, DomData2, N + 1, Max, DFS)
      end
  end;
finalize(_, DomData, _, _, _) ->
  DomData.

%%>----------------------------------------------------------------------<
%% Procedure : domTree_dominates/3
%% Purpose   : checks wheter Node1 dominates Node2 with respect to the
%%             dominator tree DomTree
%% Arguments : Node1 the possible dominator, Node2 which might be dominated 
%%             and DomTree  - the target dominator tree.
%% Notes     : Relies on lists:any to return false when the a list is empty
%%>----------------------------------------------------------------------<     

-spec domTree_dominates(cfg_lbl(), cfg_lbl(), domTree()) -> boolean().

domTree_dominates(Node1, Node1, _DomTree) ->
  true;
domTree_dominates(Node1, Node2, DomTree) ->
  Children = domTree_getChildren(Node1, DomTree),
  lists:any(fun(X) -> domTree_dominates(X, Node2, DomTree) end, Children).

%%>----------------------------------------------------------------------<
%% Procedure : pp/1
%% Purpose   : Pretty Printing a dominator tree.
%% Arguments : DomTree  - the target dominator tree.
%% Notes     : Uses pp/2 and pp_children to perform its task.
%%>----------------------------------------------------------------------<     

-ifdef(DEBUG).

domTree_pp(DomTree) ->
  io:format("Domtree:\nRoot: ~w\nSize: ~w\n", [domTree_getRoot(DomTree),
					       domTree_getSize(DomTree)]),
  domTree_pp(domTree_getRoot(DomTree), DomTree).

domTree_pp(N, DomTree) ->
  case domTree_getNode(N, DomTree) of
    {value, {IDom, Children}} ->
      io:format("Node: ~w\n\tIDom: ~w\n\tChildren: ~w\n\n",
		[N, IDom, Children]),
      domTree_pp_children(Children, DomTree);
    none ->
      failed
  end.

domTree_pp_children([Child|T], DomTree) ->
  domTree_pp(Child, DomTree),
  domTree_pp_children(T, DomTree);
domTree_pp_children([], _) ->
  ok.

-endif.	%% DEBUG

%%========================================================================
%%
%% CODE FOR CREATING AND MANIPULATING DOMINANCE FRONTIERS.
%%
%%========================================================================

-type domFrontier() :: gb_trees:tree().

%%>----------------------------------------------------------------------<
%% Procedure : domFrontier_create
%% Purpose   : This function calculates the Dominance Frontiers given
%%             a CFG and a Dominator Tree.
%% Arguments : SuccMap - The successor map of the CFG we are working with.
%%             DomTree - The dominance tree of the CFG.
%% Notes     : DomTree must actually be the dominance tree of the CFG.
%%>----------------------------------------------------------------------<

-spec domFrontier_create(cfg(), domTree()) -> domFrontier().

domFrontier_create(SuccMap, DomTree) ->
  df_create(domTree_getRoot(DomTree), SuccMap, DomTree, df__empty()).

df_create(Node, SuccMap, DomTree, DF) ->
  Children = domTree_getChildren(Node, DomTree),
  Succ = hipe_gen_cfg:succ(SuccMap, Node),
  DF1 = checkIDomList(Succ, Node, DomTree, DF),
  makeDFChildren(Children, Node, SuccMap, DomTree, DF1).

%%>----------------------------------------------------------------------<
%% Procedure : domFrontier_get
%% Purpose   : This function returns the Dominance Frontier for Node.
%% Arguments : Node - The node whose Dominance Frontier we request
%%             DF   - The Dominance Frontier structure
%% Returns   : 
%%>----------------------------------------------------------------------<

-spec domFrontier_get(cfg_lbl(), domFrontier()) -> [cfg_lbl()].

domFrontier_get(Node, DF) ->
  case gb_trees:lookup(Node, DF) of
    {value, List} -> List;
    none -> []
  end.

%%>----------------------------------------------------------------------<
%% Procedure : df__empty
%% Purpose   : This function creates an empty instance of the Dominance
%%             Frontiers (DF) structure.
%%>----------------------------------------------------------------------<

df__empty() ->
  gb_trees:empty().

%%>----------------------------------------------------------------------<
%% Procedure : df__add
%% Purpose   : This function adds Node to N in DF.
%% Arguments : N    - The value being inserted
%%             Node - The node getting the value
%%             DF   - The Dominance Frontiers
%% Returns   : DF
%% Notes     : If Node already exists at position N, it is not added again.
%%>----------------------------------------------------------------------<

df__add_to_node(N, Node, DF) ->
  case gb_trees:lookup(N, DF) of
    {value, DFList} ->
      case lists:member(Node, DFList) of
	true ->
	  DF;
	false ->
	  gb_trees:update(N, [Node|DFList], DF)
      end;
    none ->
      gb_trees:insert(N, [Node], DF)
  end.

%%>----------------------------------------------------------------------<
%% Procedure : makeDFChildren
%% Purpose   : This function calculates the dominance frontiers of the
%%             children of the parent and adds the nodes in these
%%             dominance frontiers who are not immediate dominantors of
%%             the parent to parents dominance frontier.
%% Arguments : ChildList - The list of children that the function traverses
%%             Parent - The parent of the children
%%             SuccMap - The successor map of the CFG
%%             DomTree - The dominantor tree of the CFG
%%             DF - The dominance frontiers so far
%%>----------------------------------------------------------------------<

makeDFChildren([Child|T], Parent, SuccMap, DomTree, DF) ->
  DF1 = df_create(Child, SuccMap, DomTree, DF),
  DF2 = checkIDomList(domFrontier_get(Child, DF1), Parent, DomTree, DF1),
  makeDFChildren(T, Parent, SuccMap, DomTree, DF2);
makeDFChildren([], _, _, _, DF) ->
  DF.

%%>----------------------------------------------------------------------<
%% Procedure : checIDomList
%% Purpose   : Adds all the nodes in the list to the parents dominance
%%             frontier who do not have parent as immediate dominator.
%% Arguments : NodeList - The list of nodes that the function traverses
%%             Parent - The parent of the nodes
%%             DomTree - Our dominator tree
%%             DF - The dominance frontiers so far
%%>----------------------------------------------------------------------<

checkIDomList([Node|T], Parent, DomTree, DF) ->
  DF1 = checkIDom(Node, Parent, DomTree, DF),
  checkIDomList(T, Parent, DomTree, DF1);
checkIDomList([], _, _, DF) ->
  DF.

%%>----------------------------------------------------------------------<
%% Procedure : checkIdom
%% Purpose   : Adds Node1 to Node2's dominance frontier if Node2 is not
%%             Node1's immediate dominator.
%% Arguments : Node1 - a node
%%             Node2 - another node
%%             DomTree - the dominator tree
%%             DF - the dominance frontier so far
%%>----------------------------------------------------------------------<

checkIDom(Node1, Node2, DomTree, DF) ->
  case domTree_getIDom(Node1, DomTree) of
    Node2 ->
      DF;
    none ->
      DF;
    _ ->
      df__add_to_node(Node2, Node1, DF)
  end.
