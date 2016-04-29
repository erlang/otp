%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------------
%% File    : hipe_coalescing_regalloc.erl
%% Authors : Andreas Wallin <d96awa@it.uu.se>
%%           Thorild Selén <d95ths@.it.uu.se>
%%           Ingemar Åberg <d95ina@it.uu.se>
%% Purpose : Play paintball with registers on a target machine.  We win
%%           if they are all colored.  This is an iterated coalescing
%%           register allocator.
%% Created : 4 Mar 2000
%%-----------------------------------------------------------------------

-module(hipe_coalescing_regalloc).
-export([regalloc/5]).

%%-ifndef(DEBUG).
%%-define(DEBUG,true).
%%-endif.
-include("../main/hipe.hrl").

%%-----------------------------------------------------------------------
%% Function:    regalloc
%%
%% Description: Creates a K coloring for a function.
%% Parameters:
%%   CFG         -- A control flow graph
%%   SpillIndex  -- Last index of spill variable
%%   SpillLimit  -- Temporaries with numbers higher than this have
%%                  infinite spill cost. 
%%                  Consider changing this to a set.
%%   Target      -- The module containing the target-specific functions.
%%
%% Returns:
%%   Coloring    -- A coloring for specified CFG
%%   SpillIndex0 -- A new spill index
%%-----------------------------------------------------------------------

regalloc(CFG, SpillIndex, SpillLimit, Target, _Options) ->
  %% Build interference graph
  ?debug_msg("Build IG\n", []),
  IG = hipe_ig:build(CFG, Target),
  %% io:format("IG: ~p\n", [IG]),

  ?debug_msg("Init\n", []),
  Num_Temps = Target:number_of_temporaries(CFG),
  ?debug_msg("Coalescing RA: num_temps = ~p~n", [Num_Temps]),
  Allocatable = Target:allocatable(),
  K = length(Allocatable),
  All_colors = colset_from_list(Allocatable),

  %% Add registers with their own coloring
  ?debug_msg("Moves\n", []),
  Move_sets = hipe_moves:new(IG),

  ?debug_msg("Build Worklist\n", []),
  Worklists = hipe_reg_worklists:new(IG, Target, CFG, Move_sets, K, Num_Temps),
  Alias = initAlias(Num_Temps),

  ?debug_msg("Do coloring\n~p~n", [Worklists]),
  {_IG0, Worklists0, _Moves0, Alias0} = 
    do_coloring(IG, Worklists, Move_sets, Alias, K, SpillLimit, Target),
  %% io:format("SelStk0 ~w\n",[SelStk0]),
  ?debug_msg("Init node sets\n", []),
  Node_sets = hipe_node_sets:new(),
  %% io:format("NodeSet: ~w\n NonAlloc ~w\n",[Node_sets,Target:non_alloc(CFG)]),
  ?debug_msg("Default coloring\n", []),
  {Color0,Node_sets1} = 
    defaultColoring(Target:all_precoloured(),
		    initColor(Num_Temps), Node_sets, Target),

  ?debug_msg("Assign colors\n", []),
  {Color1,Node_sets2} =
    assignColors(hipe_reg_worklists:stack(Worklists0), Node_sets1, Color0, 
		 Alias0, All_colors, Target),
  %% io:format("color0:~w\nColor1:~w\nNodes:~w\nNodes2:~w\nNum_Temps:~w\n",[Color0,Color1,Node_sets,Node_sets2,Num_Temps]),

  ?debug_msg("Build mapping ~p\n", [Node_sets2]),
  Coloring = build_namelist(Node_sets2, SpillIndex, Alias0, Color1),
  ?debug_msg("Coloring ~p\n", [Coloring]),
  Coloring.

%%----------------------------------------------------------------------
%% Function:    do_coloring
%%
%% Description: Create a coloring. That is, play paintball.
%% Parameters:
%%   IG          --  An interference graph
%%   Worklists   --  Worklists, that is simplify, spill and freeze
%%   Moves       --  Moves sets, that is coalesced, constrained 
%%                   and so on.
%%   Alias       --  Tells if two temporaries can have their value
%%                   in the same register.
%%   K           --  Want to create a K coloring.
%%   SpillLimit  --  Try not to spill nodes that are above the spill limit.
%%
%% Returns:
%%   IG          --  Updated interference graph
%%   Worklists   --  Updated Worklists structure
%%   Moves       --  Updated Moves structure 
%%   Alias       --  Updates Alias structure
%%   
%%----------------------------------------------------------------------

do_coloring(IG, Worklists, Moves, Alias, K, SpillLimit, Target) ->
  Simplify = not(hipe_reg_worklists:is_empty_simplify(Worklists)),
  Coalesce = not(hipe_moves:is_empty_worklist(Moves)),
  Freeze   = not(hipe_reg_worklists:is_empty_freeze(Worklists)),
  Spill    = not(hipe_reg_worklists:is_empty_spill(Worklists)),
  if Simplify =:= true ->
      {IG0, Worklists0, Moves0} = 
	simplify(hipe_reg_worklists:simplify(Worklists),
		 IG, 
		 Worklists, 
		 Moves, 
		 K),
      do_coloring(IG0, Worklists0, Moves0, Alias, K, SpillLimit, Target);
     Coalesce =:= true ->
      {Moves0, IG0, Worklists0, Alias0} =
	coalesce(Moves, IG, Worklists, Alias, K, Target),
      do_coloring(IG0, Worklists0, Moves0, Alias0, K, SpillLimit, Target);
     Freeze =:= true ->
      {Worklists0,Moves0} = 
	freeze(K, Worklists, Moves, IG, Alias),
      do_coloring(IG, Worklists0, Moves0, Alias, 
		  K, SpillLimit, Target);
     Spill =:= true ->
      {Worklists0, Moves0} = 
	selectSpill(Worklists, Moves, IG, K, Alias, SpillLimit),
      do_coloring(IG, Worklists0, Moves0, Alias, K, SpillLimit, Target);
     true -> % Catchall case
      {IG, Worklists, Moves, Alias}
    end.

%%----------------------------------------------------------------------
%% Function:    adjacent
%%
%% Description: Adjacent nodes that's not coalesced, on the stack or
%%               precoloured.
%% Parameters:
%%   Node        --  Node that you want to adjacents of
%%   IG          --  The interference graph
%%
%%   Returns: 
%%     A set with nodes/temporaries that are not coalesced, on the 
%%      stack or precoloured.
%%----------------------------------------------------------------------

adjacent(Node, IG, Worklists) ->
  Adjacent_edges = hipe_ig:node_adj_list(Node, IG),
  hipe_reg_worklists:non_stacked_or_coalesced_nodes(Adjacent_edges, Worklists).

%%----------------------------------------------------------------------
%% Function:    simplify
%%
%% Description: Simplify graph by removing nodes of low degree. This
%%               function simplifies all nodes it can at once.
%% Parameters:
%%   [Node|Nodes]  --  The simplify worklist
%%   IG            --  The interference graph
%%   Worklists     --  The worklists data-structure
%%   Moves         --  The moves data-structure
%%   K             --  Produce a K coloring
%%
%%   Returns: 
%%     IG          --  An updated interference graph
%%     Worklists   --  An updated worklists data-structure
%%     Moves       --  An updated moves data-structure
%%----------------------------------------------------------------------

simplify([], IG, Worklists, Moves, _K) -> 
  {IG, Worklists, Moves};
simplify([Node|Nodes], IG, Worklists, Moves, K) ->
  Worklists0 = hipe_reg_worklists:remove_simplify(Node, Worklists),
  ?debug_msg("putting ~w on stack~n",[Node]),
  Adjacent = adjacent(Node, IG, Worklists0),
  Worklists01 = hipe_reg_worklists:push_stack(Node, Adjacent, Worklists0),
  {New_ig, Worklists1, New_moves} =
    decrement_degree(Adjacent, IG, Worklists01, Moves, K),
  simplify(Nodes, New_ig, Worklists1, New_moves, K).

%%----------------------------------------------------------------------
%% Function:    decrement_degree
%%
%% Description: Decrement the degree on a number of nodes/temporaries.
%% Parameters:
%%   [Node|Nodes]  --  Decrement degree on these nodes
%%   IG            --  The interference graph
%%   Worklists     --  The Worklists data structure
%%   Moves         --  The Moves data structure.
%%   K             --  We want to create a coloring with K colors
%%
%%   Returns: 
%%     IG          --  An updated interference graph (the degrees)
%%     Worklists   --  Updated Worklists. Changed if one degree goes
%%                     down to K.
%%     Moves       --  Updated Moves. Changed if a move related temporary
%%                     gets degree K.
%%----------------------------------------------------------------------

decrement_degree([], IG, Worklists, Moves, _K) -> 
  {IG, Worklists, Moves};
decrement_degree([Node|Nodes], IG, Worklists, Moves, K) ->
  PrevDegree = hipe_ig:get_node_degree(Node, IG),
  IG0 = hipe_ig:dec_node_degree(Node, IG),
  if PrevDegree =:= K ->
      AdjList = hipe_ig:node_adj_list(Node, IG0),
      %% Ok since Node (a) is still in IG, and (b) cannot be adjacent to itself
      Moves00 = enable_moves_active_to_worklist(hipe_moves:node_movelist(Node, Moves),
						Moves),
      Moves0 = enable_moves(AdjList, Worklists, Moves00),
      Worklists0 = hipe_reg_worklists:remove_spill(Node, Worklists),
      case hipe_moves:move_related(Node, Moves0) of
	true ->
	  Worklists1 = hipe_reg_worklists:add_freeze(Node, Worklists0),
	  decrement_degree(Nodes, IG0, Worklists1, Moves0, K);
	_ ->
	  Worklists1 = hipe_reg_worklists:add_simplify(Node, Worklists0),
	  decrement_degree(Nodes, IG0, Worklists1, Moves0, K)
      end;
     true ->
      decrement_degree(Nodes, IG0, Worklists, Moves, K)
  end.
	    
%%----------------------------------------------------------------------
%% Function:    enable_moves
%%
%% Description: Make (move-related) nodes that are not yet considered for
%%               coalescing, ready for possible coalescing.
%%               
%% Parameters:
%%   [Node|Nodes]   --  A list of move nodes
%%   Moves          --  The moves data-structure
%%
%%   Returns: 
%%     An updated moves data-structure
%%----------------------------------------------------------------------

enable_moves([], _Worklists, Moves) -> Moves;
enable_moves([Node|Nodes], Worklists, Moves) ->
  case hipe_reg_worklists:member_stack_or_coalesced(Node, Worklists) of
    true -> enable_moves(Nodes, Worklists, Moves);
    _ ->
      %% moveList[n] suffices since we're checking for activeMoves membership
      Node_moves = hipe_moves:node_movelist(Node, Moves),
      New_moves = enable_moves_active_to_worklist(Node_moves, Moves),
      enable_moves(Nodes, Worklists, New_moves)
  end.

%%----------------------------------------------------------------------
%% Function:    enable_moves_active_to_worklist
%%
%% Description: Make (move-related) nodes that are not yet considered for
%%              coalescing, ready for possible coalescing.
%%               
%% Parameters:
%%   [Node|Nodes]   --  A list of move nodes
%%   Moves          --  The moves data structure
%%
%%   Returns: 
%%     An updated moves data structure
%%----------------------------------------------------------------------

enable_moves_active_to_worklist([], Moves) -> Moves;
enable_moves_active_to_worklist([Node|Nodes], Moves) ->
  NewMoves =
    case hipe_moves:member_active(Node, Moves) of
      true ->
	hipe_moves:add_worklist(Node, hipe_moves:remove_active(Node, Moves));
      _ ->
	Moves
    end,
  enable_moves_active_to_worklist(Nodes, NewMoves).

%% Build the namelists, these functions are fast hacks, they use knowledge 
%% about data representation that they shouldn't know, bad abstraction.

build_namelist(NodeSets, Index, Alias, Color) ->
  ?debug_msg("Building mapping\n",[]),
  ?debug_msg("Vector to list\n",[]),
  AliasList = build_alias_list(aliasToList(Alias),
			       0,   %% The first temporary has index 0
			       []), %% Accumulator
  ?debug_msg("Alias list:~p\n",[AliasList]),
  ?debug_msg("Coalesced\n",[]),
  NL1 = build_coalescedlist(AliasList, Color, Alias, []),
  ?debug_msg("Coalesced list:~p\n",[NL1]),
  ?debug_msg("Regs\n",[]),
  NL2 = build_reglist(hipe_node_sets:colored(NodeSets), Color, NL1),
  ?debug_msg("Regs list:~p\n",[NL2]),
  ?debug_msg("Spills\n",[]),
  build_spillist(hipe_node_sets:spilled(NodeSets), Index, NL2).

build_spillist([], Index, List) ->
  {List,Index};
build_spillist([Node|Nodes], Index, List) ->
  ?debug_msg("[~p]: Spill ~p to ~p\n", [?MODULE,Node,Index]),
  build_spillist(Nodes, Index+1, [{Node,{spill,Index}}|List]).

build_coalescedlist([], _Color, _Alias, List) ->
  List;
build_coalescedlist([Node|Ns], Color, Alias, List) when is_integer(Node) ->
  ?debug_msg("Alias of ~p is ~p~n", [Node, getAlias(Node,Alias)]),
  AC = getColor(getAlias(Node, Alias), Color),
  build_coalescedlist(Ns, Color, Alias, [{Node,{reg,AC}}|List]).

build_reglist([], _Color, List) -> 
  List;
build_reglist([Node|Ns], Color, List) ->
  build_reglist(Ns, Color, [{Node,{reg,getColor(Node,Color)}}|List]).

build_alias_list([], _I, List) ->
  List;
build_alias_list([Alias|Aliases], I, List) when is_integer(Alias) ->
  build_alias_list(Aliases, I+1, [I|List]);
build_alias_list([_Alias|Aliases], I, List) ->
  build_alias_list(Aliases, I+1, List).

%%----------------------------------------------------------------------
%% Function:    assignColors
%%
%% Description: Tries to assign colors to nodes in a stack.
%% Parameters:
%%   Stack          --  The SelectStack built by the Select function, 
%%                      this stack contains tuples in the form {Node,Edges} 
%%                      where  Node is the Node number and Edges is an ordset 
%%                      containing the numbers of all the adjacent nodes.
%%   NodeSets       --  This is a record containing all the different node 
%%                      sets that are used in the register allocator.
%%   Alias          --  This is a mapping from nodes to nodes, if a node has 
%%                      been coalesced this mapping shows the alias for that 
%%                      node.
%%   AllColors      --  This is an ordset containing all the available colors
%%
%%   Target         --  The module containing the target-specific functions.
%%
%% Returns:
%%   Color          --  A mapping from nodes to their respective color.
%%   NodeSets       --  The updated node sets.
%%----------------------------------------------------------------------

assignColors(Stack, NodeSets, Color, Alias, AllColors, Target) ->
  case Stack of
    [] ->
      {Color,NodeSets};
    [{Node,Edges}|Stack1] ->
      ?debug_msg("Coloring Node: ~p~n",[Node]),
      ?IF_DEBUG(lists:foreach(fun (_E) ->
				  ?msg("  Edge ~w-><~w>->~w~n",
				       begin A = getAlias(_E,Alias),
					     [_E,A,getColor(A,Color)]
				       end)
			      end, Edges),
		[]),
      %% When debugging, check that Node isn't precoloured.
      OkColors = findOkColors(Edges, AllColors, Color, Alias),
      case colset_is_empty(OkColors) of
	true -> % Spill case
	  NodeSets1 = hipe_node_sets:add_spilled(Node, NodeSets),
	  assignColors(Stack1, NodeSets1, Color, Alias, AllColors, Target);
	false -> % Colour case
	  Col = colset_smallest(OkColors),
	  NodeSets1 = hipe_node_sets:add_colored(Node, NodeSets),
	  Color1 = setColor(Node, Target:physical_name(Col), Color),
	  assignColors(Stack1, NodeSets1, Color1, Alias, AllColors, Target)
      end
  end.

%%---------------------------------------------------------------------
%% Function:     defaultColoring
%% 
%% Description: Make the default coloring
%% Parameters:
%%   Regs           -- The list of registers to be default colored
%%   Color          -- The color mapping that shall be changed
%%   NodeSets       -- The node sets that shall be updated
%%   Target         -- The module containing the target-specific functions.
%%
%% Returns:
%%   NewColor       -- The updated color mapping
%%   NewNodeSets    -- The updated node sets
%%---------------------------------------------------------------------

defaultColoring([], Color, NodeSets, _Target) ->
  {Color,NodeSets};
defaultColoring([Reg|Regs], Color, NodeSets, Target) ->
  Color1 = setColor(Reg,Target:physical_name(Reg), Color),
  NodeSets1 = hipe_node_sets:add_colored(Reg, NodeSets),
  defaultColoring(Regs, Color1, NodeSets1, Target).

%% Find the colors that are OK for a node with certain edges.

findOkColors(Edges, AllColors, Color, Alias) ->
  find(Edges, AllColors, Color, Alias).

%% Find all the colors of the nodes in the list [Node|Nodes] and remove them 
%% from the set OkColors, when the list is empty, return OkColors.

find([], OkColors, _Color, _Alias) ->
  OkColors;
find([Node0|Nodes], OkColors, Color, Alias) ->
  Node = getAlias(Node0, Alias),
  case getColor(Node, Color) of
    [] ->
      find(Nodes, OkColors, Color, Alias);
    Col ->
      OkColors1 = colset_del_element(Col, OkColors),
      find(Nodes, OkColors1, Color, Alias)
  end.

%%%
%%% ColSet -- ADT for the set of available colours while
%%% assigning colours.
%%%
-ifdef(notdef). % old ordsets-based implementation
colset_from_list(Allocatable) ->
  ordsets:from_list(Allocatable).

colset_del_element(Colour, ColSet) ->
  ordsets:del_element(Colour, ColSet).

colset_is_empty(ColSet) ->
  case ColSet of
    [] -> true;
    [_|_] -> false
  end.

colset_smallest([Colour|_]) ->
  Colour.
-endif.

-ifdef(notdef). % new gb_sets-based implementation
colset_from_list(Allocatable) ->
  gb_sets:from_list(Allocatable).

colset_del_element(Colour, ColSet) ->
  %% Must use gb_sets:delete_any/2 since gb_sets:del_element/2
  %% fails if the element isn't present. Bummer.
  gb_sets:delete_any(Colour, ColSet).

colset_is_empty(ColSet) ->
  gb_sets:is_empty(ColSet).

colset_smallest(ColSet) ->
  gb_sets:smallest(ColSet).
-endif.

%%-ifdef(notdef). % new bitmask-based implementation
colset_from_list(Allocatable) ->
  colset_from_list(Allocatable, 0).

colset_from_list([], ColSet) ->
  ColSet;
colset_from_list([Colour|Allocatable], ColSet) ->
  colset_from_list(Allocatable, ColSet bor (1 bsl Colour)).

colset_del_element(Colour, ColSet) ->
  ColSet band bnot(1 bsl Colour).

colset_is_empty(0) -> true;
colset_is_empty(_) -> false.

colset_smallest(ColSet) ->
  bitN_log2(ColSet band -ColSet, 0).

bitN_log2(BitN, ShiftN) ->
  if BitN > 16#ffff ->
      bitN_log2(BitN bsr 16, ShiftN + 16);
     true ->
      ShiftN + hweight16(BitN - 1)
  end.

hweight16(W) ->
  Res1 = (   W band 16#5555) + ((   W bsr 1) band 16#5555),
  Res2 = (Res1 band 16#3333) + ((Res1 bsr 2) band 16#3333),
  Res3 = (Res2 band 16#0F0F) + ((Res2 bsr 4) band 16#0F0F),
         (Res3 band 16#00FF) + ((Res3 bsr 8) band 16#00FF).
%%-endif.

%%%
%%% Colour ADT providing a partial mapping from nodes to colours.
%%%

initColor(NrNodes) ->
  {colmap, hipe_bifs:array(NrNodes, [])}.

getColor(Node, {colmap, ColMap}) ->
  hipe_bifs:array_sub(ColMap, Node).

setColor(Node, Colour, {colmap, ColMap} = Col) ->
  hipe_bifs:array_update(ColMap, Node, Colour),  
  Col.

%%%
%%% Alias ADT providing a partial mapping from nodes to nodes.
%%%

initAlias(NrNodes) ->
  {alias, hipe_bifs:array(NrNodes, [])}.

getAlias(Node, {alias, AliasMap} = Alias) ->
  case hipe_bifs:array_sub(AliasMap, Node) of
    [] ->
      Node;
    AliasNode ->
      getAlias(AliasNode, Alias)
  end.

setAlias(Node, AliasNode, {alias, AliasMap} = Alias) ->
  hipe_bifs:array_update(AliasMap, Node, AliasNode),
  Alias.

aliasToList({alias,AliasMap}) ->
  aliasToList(AliasMap, hipe_bifs:array_length(AliasMap), []).

aliasToList(AliasMap, I1, Tail) ->
  I0 = I1 - 1,
  if I0 >= 0 ->
      aliasToList(AliasMap, I0, [hipe_bifs:array_sub(AliasMap, I0)|Tail]);
     true ->
      Tail
  end.

%%----------------------------------------------------------------------
%% Function:    coalesce
%%
%% Description: Coalesces nodes in worklist
%% Parameters:
%%   Moves       -- Current move information
%%   IG          -- Interference graph
%%   Worklists   -- Current worklists
%%   Alias       -- Current aliases for temporaries
%%   K           -- Number of registers
%%   
%% Returns:
%%   {Moves, IG, Worklists, Alias}
%%         (Updated versions of above structures, after coalescing)
%%----------------------------------------------------------------------

coalesce(Moves, IG, Worklists, Alias, K, Target) ->
  case hipe_moves:worklist_get_and_remove(Moves) of
    {[],Moves0} ->
      %% Moves marked for removal from worklistMoves by FreezeMoves()
      %% are removed by worklist_get_and_remove(). This case is unlikely,
      %% but can occur if only stale moves remain in worklistMoves.
      {Moves0,IG,Worklists,Alias};
    {Move,Moves0} ->
      {Dest,Source} = hipe_moves:get_move(Move, Moves0),
      ?debug_msg("Testing nodes ~p and ~p for coalescing~n",[Dest,Source]),
      Alias_src = getAlias(Source, Alias),
      Alias_dst = getAlias(Dest, Alias),
      {U,V} = case Target:is_precoloured(Alias_dst) of
		true -> {Alias_dst, Alias_src};
		false -> {Alias_src, Alias_dst}
	      end,
      %% When debugging, check that neither V nor U is on the stack.
      if U =:= V ->
	  Moves1 = Moves0, % drop coalesced move Move
	  Worklists1 = add_worklist(Worklists, U, K, Moves1, IG, Target),
	  {Moves1, IG, Worklists1, Alias};
	 true ->
	  case (Target:is_precoloured(V) orelse
		hipe_ig:nodes_are_adjacent(U, V, IG)) of 
	    true ->
	      Moves1 = Moves0, % drop constrained move Move
	      Worklists1 = add_worklist(Worklists, U, K, Moves1, IG, Target),
	      Worklists2 = add_worklist(Worklists1, V, K, Moves1, IG, Target),
	      {Moves1, IG, Worklists2, Alias};
	    false ->
	      case (case Target:is_precoloured(U) of
		      true ->
			AdjV = hipe_ig:node_adj_list(V, IG),
			all_adjacent_ok(AdjV, U, Worklists, IG, K, Target);
		      false ->
			AdjV = hipe_ig:node_adj_list(V, IG),
			AdjU = hipe_ig:node_adj_list(U, IG),
			conservative(AdjU, AdjV, U, Worklists, IG, K)
		    end) of
		true ->
		  Moves1 = Moves0, % drop coalesced move Move
		  {IG1,Worklists1,Moves2,Alias1} =
		    combine(U, V, IG, Worklists, Moves1, Alias, K, Target),
		  Worklists2 = add_worklist(Worklists1, U, K, Moves2, IG1, Target),
		  {Moves2, IG1, Worklists2, Alias1};
		false ->
		  Moves1 = hipe_moves:add_active(Move, Moves0),
		  {Moves1, IG, Worklists, Alias}
	      end
	  end
      end
  end.

%%----------------------------------------------------------------------
%% Function:    add_worklist
%%
%% Description: Builds new worklists where U is transferred from freeze
%%              to simplify, if possible
%%
%% Parameters:
%%   Worklists     -- Current worklists
%%   U             -- Node to operate on
%%   K             -- Number of registers
%%   Moves         -- Current move information
%%   IG            -- Interference graph
%%   Target        -- The containing the target-specific functions
%%   
%% Returns:
%%   Worklists (updated)
%%----------------------------------------------------------------------

add_worklist(Worklists, U, K, Moves, IG, Target) ->
  case (not(Target:is_precoloured(U))
	andalso not(hipe_moves:move_related(U, Moves))
	andalso (hipe_ig:is_trivially_colourable(U, K, IG))) of
    true ->
      hipe_reg_worklists:transfer_freeze_simplify(U, Worklists);
    false ->
      Worklists
  end.

%%----------------------------------------------------------------------
%% Function:    combine
%%
%% Description: Combines two nodes into one (used when coalescing)
%%
%% Parameters:
%%   U          -- First node to operate on
%%   V          -- Second node to operate on
%%   IG         -- Interference graph
%%   Worklists  -- Current worklists
%%   Moves      -- Current move information
%%   Alias      -- Current aliases for temporaries
%%   K          -- Number of registers
%%
%% Returns:
%%   {IG, Worklists, Moves, Alias} (updated)
%%----------------------------------------------------------------------
       
combine(U, V, IG, Worklists, Moves, Alias, K, Target) ->
  Worklists1 = case hipe_reg_worklists:member_freeze(V, Worklists) of
		 true -> hipe_reg_worklists:remove_freeze(V, Worklists);
		 false -> hipe_reg_worklists:remove_spill(V, Worklists)
	       end,
  Worklists11 = hipe_reg_worklists:add_coalesced(V, Worklists1),
  
  ?debug_msg("Coalescing ~p and ~p to ~p~n",[V,U,U]),
  
  Alias1 = setAlias(V, U, Alias),
  
  %% Typo in published algorithm: s/nodeMoves/moveList/g to fix.
  %% XXX: moveList[u] \union moveList[v] OR NodeMoves(u) \union NodeMoves(v) ???
  %% XXX: NodeMoves() is correct, but unnecessarily strict. The ordsets:union
  %% constrains NodeMoves() to return an ordset.
  Moves1 = hipe_moves:update_movelist(U,
				      ordsets:union(hipe_moves:node_moves(U, Moves),
						    hipe_moves:node_moves(V, Moves)),
				      Moves),
  %% Missing in published algorithm. From Tiger book Errata.
  Moves2 = enable_moves_active_to_worklist(hipe_moves:node_movelist(V, Moves1), Moves1),
  AdjV = hipe_ig:node_adj_list(V, IG),
  
  {IG1, Worklists2, Moves3} =
    combine_edges(AdjV, U, IG, Worklists11, Moves2, K, Target),

  New_worklists = case (not(hipe_ig:is_trivially_colourable(U, K, IG1))
			andalso
			hipe_reg_worklists:member_freeze(U, Worklists2)) of
		    true -> 
		      hipe_reg_worklists:transfer_freeze_spill(U, Worklists2);
		    false -> Worklists2
		  end,
  {IG1, New_worklists, Moves3, Alias1}.

%%----------------------------------------------------------------------
%% Function:    combine_edges
%%
%% Description: For each node in a list, make an edge between that node
%%              and node U, and decrement its degree by 1
%%              (Used when two nodes are coalesced, to connect all nodes
%%              adjacent to one node to the other node)
%%
%% Parameters:
%%   [T|Ts]      -- List of nodes to make edges to
%%   U           -- Node to make edges from
%%   IG          -- Interference graph
%%   Worklists   -- Current worklists
%%   Moves       -- Current move information
%%   K           -- Number of registers
%%
%% Returns:
%%   {IG, Worklists, Moves} (updated)
%%----------------------------------------------------------------------

combine_edges([], _U, IG, Worklists, Moves, _K, _Target) ->
  {IG, Worklists, Moves};
combine_edges([T|Ts], U, IG, Worklists, Moves, K, Target) ->
  case hipe_reg_worklists:member_stack_or_coalesced(T, Worklists) of
    true -> combine_edges(Ts, U, IG, Worklists, Moves, K, Target);
    _ ->
      %% XXX: The issue below occurs because the T->V edge isn't removed.
      %% This causes adjList[T] to contain stale entries, to possibly grow
      %% (if T isn't already adjacent to U), and degree[T] to possibly
      %% increase (again, if T isn't already adjacent to U).
      %% The decrement_degree() call repairs degree[T] but not adjList[T].
      %% It would be better to physically replace T->V with T->U, and only
      %% decrement_degree(T) if T->U already existed.
      %%
      %% add_edge() may change a low-degree move-related node to be of
      %% significant degree. In this case the node belongs in the spill
      %% worklist, and that's where decrement_degree() expects to find it.
      %% This issue is not covered in the published algorithm.
      OldDegree = hipe_ig:get_node_degree(T, IG),
      IG1 = hipe_ig:add_edge(T, U, IG, Target),
      NewDegree = hipe_ig:get_node_degree(T, IG1),
      Worklists0 =
	if NewDegree =:= K, OldDegree =:= K-1 ->
	    %% io:format("~w:combine_edges(): repairing worklist membership for node ~w\n", [?MODULE,T]),
	    %% The node T must be on the freeze worklist:
	    %% 1. Since we're coalescing, the simplify worklist must have been
	    %%    empty when combine_edges() started.
	    %% 2. decrement_degree() may put the node T back on the simplify
	    %%    worklist, but that occurs after the worklists repair step.
	    %% 3. There are no duplicates among the edges.
	    Worklists00 = hipe_reg_worklists:remove_freeze(T, Worklists),
	    hipe_reg_worklists:add_spill(T, Worklists00);
	   true ->
	    Worklists
	end,
      {IG2, Worklists1, Moves1} =
	decrement_degree([T], IG1, Worklists0, Moves, K),
      combine_edges(Ts, U, IG2, Worklists1, Moves1, K, Target)
  end.

%%----------------------------------------------------------------------
%% Function:    ok
%%
%% Description: Checks if a node T is suitable to coalesce with R
%%
%% Parameters:
%%   T             -- Node to test
%%   R             -- Other node to test
%%   IG            -- Interference graph
%%   K             -- Number of registers
%%   Target        -- The module containing the target-specific functions
%%   
%% Returns:
%%   true iff coalescing is OK
%%----------------------------------------------------------------------

ok(T, R, IG, K, Target) ->
  ((hipe_ig:is_trivially_colourable(T, K, IG))
   orelse Target:is_precoloured(T)
   orelse hipe_ig:nodes_are_adjacent(T, R, IG)).

%%----------------------------------------------------------------------
%% Function:    all_ok
%%
%% Description: True iff, for every T in the list, OK(T,U)
%%
%% Parameters:
%%   [T|Ts]        -- Nodes to test
%%   U             -- Node to test for coalescing
%%   IG            -- Interference graph
%%   K             -- Number of registers
%%   Target        -- The module containing the target-specific functions
%%   
%% Returns:
%%   true iff coalescing is OK for all nodes in the list
%%----------------------------------------------------------------------

all_adjacent_ok([], _U, _Worklists, _IG, _K, _Target) -> true;
all_adjacent_ok([T|Ts], U, Worklists, IG, K, Target) ->
  case hipe_reg_worklists:member_stack_or_coalesced(T, Worklists) of
    true -> all_adjacent_ok(Ts, U, Worklists, IG, K, Target);
    _ ->
      %% 'andalso' does not preserve tail-recursion
      case ok(T, U, IG, K, Target) of
	true -> all_adjacent_ok(Ts, U, Worklists, IG, K, Target);
	false -> false
      end
  end.

%%----------------------------------------------------------------------
%% Function:    conservative
%%
%% Description: Checks if nodes can be safely coalesced according to
%%              the Briggs' conservative coalescing heuristic
%%
%% Parameters:
%%   Nodes         -- Adjacent nodes
%%   IG            -- Interference graph
%%   K             -- Number of registers
%%   
%% Returns:
%%   true iff coalescing is safe
%%----------------------------------------------------------------------

conservative(AdjU, AdjV, U, Worklists, IG, K) ->
  conservative_countU(AdjU, AdjV, U, Worklists, IG, K, 0).

%%----------------------------------------------------------------------
%% Function:    conservative_count
%%
%% Description: Counts degrees for conservative (Briggs' heuristic)
%%
%% Parameters:
%%   Nodes         -- (Remaining) adjacent nodes
%%   IG            -- Interference graph
%%   K             -- Number of registers
%%   Cnt           -- Accumulator for counting
%%   
%% Returns:
%%   Final value of accumulator
%%----------------------------------------------------------------------

conservative_countU([], AdjV, U, Worklists, IG, K, Cnt) ->
  conservative_countV(AdjV, U, Worklists, IG, K, Cnt);
conservative_countU([Node|AdjU], AdjV, U, Worklists, IG, K, Cnt) ->
  case hipe_reg_worklists:member_stack_or_coalesced(Node, Worklists) of
    true -> conservative_countU(AdjU, AdjV, U, Worklists, IG, K, Cnt);
    _ ->
      case hipe_ig:is_trivially_colourable(Node, K, IG) of
	true -> conservative_countU(AdjU, AdjV, U, Worklists, IG, K, Cnt);
	_ ->
	  Cnt1 = Cnt + 1,
	  if Cnt1 < K ->
	      conservative_countU(AdjU, AdjV, U, Worklists, IG, K, Cnt1);
	     true -> false
	  end
      end
  end.

conservative_countV([], _U, _Worklists, _IG, _K, _Cnt) -> true;
conservative_countV([Node|AdjV], U, Worklists, IG, K, Cnt) ->
  case hipe_reg_worklists:member_stack_or_coalesced(Node, Worklists) of
    true -> conservative_countV(AdjV, U, Worklists, IG, K, Cnt);
    _ ->
      case hipe_ig:nodes_are_adjacent(Node, U, IG) of
	true -> conservative_countV(AdjV, U, Worklists, IG, K, Cnt);
	_ ->
	  case hipe_ig:is_trivially_colourable(Node, K, IG) of
	    true -> conservative_countV(AdjV, U, Worklists, IG, K, Cnt);
	    _ ->
	      Cnt1 = Cnt + 1,
	      if Cnt1 < K ->
		  conservative_countV(AdjV, U, Worklists, IG, K, Cnt1);
		 true -> false
	      end
	  end
      end
  end.

%%---------------------------------------------------------------------
%% Function:    selectSpill
%% 
%% Description: Select the node to spill and spill it
%% Parameters:
%%   WorkLists      -- A datatype containing the different worklists
%%   Moves          -- A datatype containing the move sets
%%   IG             -- The interference graph
%%   K              -- The number of available registers
%%   Alias          -- The alias mapping
%%   SpillLimit     -- Try not to spill any nodes above the spill limit
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated moves
%%---------------------------------------------------------------------

selectSpill(WorkLists, Moves, IG, K, Alias, SpillLimit) ->
  [CAR|CDR] = hipe_reg_worklists:spill(WorkLists),
  
  SpillCost = getCost(CAR, IG, SpillLimit),
  M = findCheapest(CDR, IG, SpillCost, CAR, SpillLimit),
  
  WorkLists1 = hipe_reg_worklists:remove_spill(M, WorkLists),
  %% The published algorithm adds M to the simplify worklist
  %% before the freezeMoves() call. That breaks the worklist
  %% invariants, which is why the order is switched here.
  {WorkLists2,Moves1} = freezeMoves(M, K, WorkLists1, Moves, IG, Alias),
  WorkLists3 = hipe_reg_worklists:add_simplify(M, WorkLists2),
  {WorkLists3,Moves1}.

%% Find the node that is cheapest to spill

findCheapest([], _IG, _Cost, Cheapest, _SpillLimit) ->
  Cheapest;
findCheapest([Node|Nodes], IG, Cost, Cheapest, SpillLimit) ->
  ThisCost = getCost(Node, IG, SpillLimit),
  case ThisCost < Cost of
    true ->
      findCheapest(Nodes, IG, ThisCost, Node, SpillLimit);
    false ->
      findCheapest(Nodes, IG, Cost, Cheapest, SpillLimit)
  end.

%% Get the cost for spilling a certain node, node numbers above the spill 
%% limit are extremely expensive.

getCost(Node, IG, SpillLimit) ->
  case Node > SpillLimit of
    true -> inf;
    false -> hipe_ig:node_spill_cost(Node, IG)
  end.

%%----------------------------------------------------------------------
%% Function:    freeze
%%
%% Description: When both simplifying and coalescing is impossible we 
%%              rather freezes a node in stead of spilling, this function
%%              selects a node for freezing (it just picks the first one in
%%              the list)
%%
%% Parameters:
%%   K              -- The number of available registers
%%   WorkLists      -- A datatype containing the different worklists
%%   Moves          -- A datatype containing the different movelists
%%   IG             -- Interference graph
%%   Alias          -- An alias mapping, shows the alias of all coalesced 
%%                      nodes  
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated movelists
%%----------------------------------------------------------------------

freeze(K, WorkLists, Moves, IG, Alias) ->
  [U|_] = hipe_reg_worklists:freeze(WorkLists),         % Smarter routine?
  ?debug_msg("freezing node ~p~n", [U]),
  WorkLists0 = hipe_reg_worklists:remove_freeze(U, WorkLists),
  %% The published algorithm adds U to the simplify worklist
  %% before the freezeMoves() call. That breaks the worklist
  %% invariants, which is why the order is switched here.
  {WorkLists1,Moves1} = freezeMoves(U,K,WorkLists0,Moves,IG,Alias),
  WorkLists2 = hipe_reg_worklists:add_simplify(U, WorkLists1),
  {WorkLists2,Moves1}.

%%----------------------------------------------------------------------
%% Function:    freezeMoves
%%
%% Description: Make all move related interferences for a certain node 
%%              into ordinary interference arcs.
%%              
%% Parameters:
%%   U              -- The node we want to freeze
%%   K              -- The number of available registers
%%   WorkLists      -- A datatype containing the different worklists
%%   Moves          -- A datatype containing the different movelists
%%   IG             -- Interference graph
%%   Alias          -- An alias mapping, shows the alias of all coalesced 
%%                     nodes  
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated movelists
%%----------------------------------------------------------------------

freezeMoves(U, K, WorkLists, Moves, IG, Alias) ->
  Nodes = hipe_moves:node_moves(U, Moves),
  freezeEm(U, Nodes, K, WorkLists, Moves, IG, Alias).

%% Find what the other value in a copy instruction is, return false if 
%% the instruction isn't a move with the first argument in it.

moves(U, Move, Alias, Moves) ->
  {X,Y} = hipe_moves:get_move(Move, Moves),
  %% The old code (which followed the published algorithm) did
  %% not follow aliases before looking for "the other" node.
  %% This caused moves() to skip some moves, making some nodes
  %% still move-related after freezeMoves(). These move-related
  %% nodes were then added to the simplify worklist (by freeze()
  %% or selectSpill()), breaking the worklist invariants. Nodes
  %% already simplified appeared in coalesce(), were re-added to
  %% the simplify worklist by add_worklist(), simplified again,
  %% and coloured multiple times by assignColors(). Ouch!
  X1 = getAlias(X, Alias),
  Y1 = getAlias(Y, Alias),
  if U =:= X1 -> Y1;
     U =:= Y1 -> X1;
     true -> exit({?MODULE,moves}) % XXX: shouldn't happen
  end.

freezeEm(_U, [], _K, WorkLists, Moves, _IG, _Alias) ->
  {WorkLists,Moves};
freezeEm(U,[M|Ms], K, WorkLists, Moves, IG, Alias) ->
  V = moves(U, M, Alias, Moves),
  {WorkLists2,Moves2} = freezeEm2(U, V, M, K, WorkLists, Moves, IG, Alias),
  freezeEm(U, Ms, K, WorkLists2, Moves2, IG, Alias).

freezeEm2(U, V, M, K, WorkLists, Moves, IG, Alias) ->
  case hipe_moves:member_active(M, Moves) of
    true ->
      Moves1 = hipe_moves:remove_active(M, Moves),
      freezeEm3(U, V, M, K, WorkLists, Moves1, IG, Alias);	
    false ->
      Moves1 = hipe_moves:remove_worklist(M, Moves),
      freezeEm3(U, V, M, K, WorkLists, Moves1, IG, Alias)
  end.

freezeEm3(_U, V, _M, K, WorkLists, Moves, IG, _Alias) ->
  Moves1 = Moves, % drop frozen move M
  V1 = V, % getAlias(V,Alias),
  %% "not MoveRelated(v)" is cheaper than "NodeMoves(v) = {}"
  case ((not hipe_moves:move_related(V1, Moves1)) andalso
	hipe_ig:is_trivially_colourable(V1, K, IG)) of
    true ->
      ?debug_msg("freezing move to ~p~n", [V]),
      Worklists1 = hipe_reg_worklists:transfer_freeze_simplify(V1, WorkLists),
      {Worklists1, Moves1};
    false ->
      {WorkLists, Moves1}
  end.
