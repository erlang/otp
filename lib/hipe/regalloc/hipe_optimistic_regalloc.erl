%% -*- erlang-indent-level: 2 -*-
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
%%-----------------------------------------------------------------------
%% File    : hipe_optimistic_regalloc.erl
%% Authors : NilsOla Linnermark <nilsola@abc.se>
%%           Petter Holmberg <petter.holmberg@usa.net>
%% Purpose : Play paintball with registers on a target machine.  We win
%%           if they are all colored.  This is an optimistic coalescing
%%           register allocator.
%% Created : Spring 2005
%%-----------------------------------------------------------------------

-module(hipe_optimistic_regalloc).
-export([regalloc/7]).

-ifndef(DEBUG).
%%-define(DEBUG,true).
-else.
-ifndef(COMPARE_ITERATED_OPTIMISTIC).
%% If this macro is turned on you can easily compare 
%% each intermediate step in the iterated coalescing
%% register allocator and the optimitsitc coalescing
%% register allocator. This is useful for debugging -
%% many small erlang functions should render the same
%% register allocaton for both allocators.
-define(COMPARE_ITERATED_OPTIMISTIC, true).
-endif.
-endif.
-include("../main/hipe.hrl").
-ifdef(DEBUG_PRINTOUTS).
-define(print_adjacent(IG), hipe_ig:print_adjacent(IG)).
-define(print_degrees(IG), hipe_ig:print_degrees(IG)).
-define(print_spill_costs(IG),  hipe_ig:print_spill_costs(IG)).
-define(mov_print_memberships(MV),  hipe_moves:print_memberships(MV)).
-define(reg_print_memberships(WL), hipe_reg_worklists:print_memberships(WL)).
-define(print_alias(A), printAlias(A)).
-define(print_colors(T,C), printColors(T,C)).
-else.
-define(print_adjacent(IG), no_print).
-define(print_degrees(IG), no_print).
-define(print_spill_costs(IG), no_print).
-define(mov_print_memberships(MV), no_print).
-define(reg_print_memberships(WL), no_print).
-define(print_alias(A), no_print).
-define(print_colors(T,C), no_print).
-endif.


%%-----------------------------------------------------------------------
%% Function:    regalloc
%%
%% Description: Creates a K coloring for a function.
%% Parameters:
%%   CFG         -- A control flow graph
%%   SpillIndex  -- Last index of spill variable
%%   SpillLimit  -- Temporaris with numbers higher than this have
%%                  infinit spill cost. 
%%                  Consider changing this to a set.
%%   TgtMod      -- The module containing the target-specific functions.
%%   TgtCtx      -- Context data for TgtMod
%%
%% Returns:
%%   Coloring    -- A coloring for specified CFG
%%   SpillIndex2 -- A new spill index
%%-----------------------------------------------------------------------
-ifdef(COMPARE_ITERATED_OPTIMISTIC).
regalloc(CFG, Liveness, SpillIndex, SpillLimit, TgtMod, TgtCtx, _Options) ->
  Target = {TgtMod, TgtCtx},
  ?debug_msg("optimistic ~w\n",[TgtMod]),
  ?debug_msg("CFG: ~p\n",[CFG]),
  %% Build interference graph
  ?debug_msg("Build IG\n",[]),
  IG_O = hipe_ig:build(CFG, Liveness, TgtMod, TgtCtx),
  IG = hipe_ig:build(CFG, Liveness, TgtMod, TgtCtx),
  ?debug_msg("adjlist: ~p\n",[hipe_ig:adj_list(IG)]),
  ?debug_msg("IG:\n",[]),
  ?print_adjacent(IG),
  ?print_degrees(IG),
  ?print_spill_costs(IG),

  SavedSpillCosts = hipe_ig:spill_costs(IG),
  SavedAdjList = hipe_ig:adj_list(IG),

  ?debug_msg("Init\n",[]),
  No_temporaries = number_of_temporaries(CFG, Target),
  ?debug_msg("Coalescing RA: num_temps = ~p~n", [No_temporaries]),
  Allocatable = allocatable(Target),
  K = length(Allocatable),
  All_colors = colset_from_list(Allocatable),
  ?debug_msg("K: ~w~nAll_colors: ~p\n",[K, All_colors]), 

  %% Add registers with their own coloring
  ?debug_msg("Moves\n",[]),
  Move_sets_O = hipe_moves:new(IG_O),
  Move_sets = hipe_moves:new(IG),
  ?debug_msg("Move_sets:\n ~p\n",[Move_sets]),
  ?mov_print_memberships(Move_sets),

  ?debug_msg("Build Worklist\n",[]),
  Worklists_O = hipe_reg_worklists:new(IG_O, TgtMod, TgtCtx, CFG, Move_sets_O,
				       K, No_temporaries),
  ?debug_msg("Worklists:\n ~p\n", [Worklists_O]),
  ?reg_print_memberships(Worklists_O),

  Worklists = hipe_reg_worklists:new(IG, TgtMod, TgtCtx, CFG, K,
				     No_temporaries),
  ?debug_msg("New Worklists:\n ~p\n", [Worklists]),
  ?reg_print_memberships(Worklists),

  Alias_O = initAlias(No_temporaries),
  Alias = initAlias(No_temporaries),
  ?print_alias(Alias),

  ?debug_msg("Do coloring\n~p~n",[Worklists_O]),
  {IG0_O, Worklists0_O, Moves0_O, Alias0_O} = 
    do_coloring(IG_O, Worklists_O, Move_sets_O, Alias_O,
		K, SpillLimit, Target),
  ?debug_msg("IG_O after color:\n ~p\n",[IG0_O]),
  ?print_adjacent(IG0_O),
  ?print_degrees(IG0_O),
  ?print_spill_costs(IG0_O),
  ?debug_msg("Move_sets after color:\n ~p\n",[Moves0_O]),
  ?mov_print_memberships(Moves0_O),
  ?debug_msg("Worklists after color:\n ~p\n", [Worklists0_O]),
  ?reg_print_memberships(Worklists0_O),

  {IG0, Moves0, Alias0, Worklists0} = 
    do_coalescing(IG, Worklists, Move_sets, Alias, K, Target),
  ?debug_msg("IG after coalescing:\n",[]),
  ?print_adjacent(IG0),
  ?print_degrees(IG0),
  ?print_spill_costs(IG0),
  ?debug_msg("Move_sets after coalescing:\n ~p\n",[Moves0]),
  ?mov_print_memberships(Moves0),
  ?debug_msg("New Worklists after coalescing:\n ~p\n",
             [Worklists0]),
  ?reg_print_memberships(Worklists0),

  {IG1, Worklists1, Moves1, Alias1} = 
    do_simplify_or_spill(IG0, Worklists0, Moves0, Alias0, 
                         K, SpillLimit, Target),
  ?debug_msg("IG after simplify_or_spill:\n",[]),
  ?print_adjacent(IG1),
  ?print_degrees(IG1),
  ?print_spill_costs(IG1),
  ?debug_msg("Saved spill costs ~p~n", [SavedSpillCosts]),
  ?debug_msg("Move_sets after simplify_or_spill:\n ~p\n",[Moves1]),
  ?mov_print_memberships(Moves1),
  ?debug_msg("New Worklists after simplify_or_spill:\n ~p\n",
             [Worklists1]),
  ?reg_print_memberships(Worklists1),
  ?print_alias(Alias1),

  %% only for testing undoCoalescing and member_coalesced_to
  %test_undoCoalescing(No_temporaries, Alias1, Worklists1),

  %% only for testing fixAdj
  %?debug_msg("adj_lists_before_fixAdj ~n~p~n", [hipe_ig:adj_list(IG1)]),
  %IG2 = test_fixAdj(No_temporaries, SavedAdjList, IG1, Target),
  %?debug_msg("adj_lists__after_fixAdj ~n~p~n", [hipe_ig:adj_list(IG2)]),

  ?debug_msg("Init node sets\n",[]),
  Node_sets = hipe_node_sets:new(),
  %% ?debug_msg("NodeSet: ~w\n NonAlloc ~w\n",[Node_sets,non_alloc(CFG,Target)]),
  ?debug_msg("Default coloring\n",[]),
  {Color0,Node_sets1} = 
    defaultColoring(all_precoloured(Target),
		    initColor(No_temporaries), Node_sets, Target),
  ?debug_msg("Color0\n",[]),
  ?print_colors(No_temporaries, Color0),

  ?debug_msg("----------------------Assign colors _N\n",[]),

  Stack = hipe_reg_worklists:stack(Worklists1), 
  ?debug_msg("The stack _N ~p~n", [Stack]), 
  %SortedStack = sort_stack(Stack),
  %?debug_msg("The stack _N ~p~n", [SortedStack]), 

  %?debug_msg("Nodes _N ~w~n", [Node_sets1]),

  {Color1,Node_sets2,Alias2} =
    assignColors(Worklists1, Stack, Node_sets1, Color0, 
        	 No_temporaries, SavedAdjList, SavedSpillCosts, IG1, Alias1, All_colors, Target),
  ?print_colors(No_temporaries, Color1),
  ?debug_msg("Nodes:~w\nNodes2:~w\nNo_temporaries:~w\n",[Node_sets,Node_sets2,No_temporaries]),

  ?debug_msg("Build mapping _N ~w\n",[Node_sets2]),
  {Coloring,SpillIndex2} =
    build_namelist(Node_sets2,SpillIndex,Alias2,Color1),
  ?debug_msg("Coloring ~p\n",[Coloring]),
  SortedColoring = {sort_stack(Coloring), SpillIndex2},
  ?debug_msg("SortedColoring ~p\n",[SortedColoring]),
  %%Coloring.
  ?debug_msg("----------------------Assign colors _O\n",[]),
  {Color1_O,Node_sets2_O} =
    assignColors_O(hipe_reg_worklists:stack(Worklists0_O), Node_sets1, Color0, 
		 Alias0_O, All_colors, Target),
  ?print_colors(No_temporaries, Color1_O),
  ?debug_msg("Nodes:~w\nNodes2:~w\nNo_temporaries:~w\n",[Node_sets,Node_sets2_O,No_temporaries]),

  ?debug_msg("Build mapping ~w\n",[Node_sets2_O]),
  Coloring_O = build_namelist_O(Node_sets2_O,SpillIndex,Alias0_O,Color1_O),
  ?debug_msg("Coloring_O ~p\n",[Coloring_O]),
  SortedColoring_O = {sort_stack(element(1, Coloring_O)), element(2, Coloring_O)},
  ?debug_msg("SortedColoring_O ~p\n",[SortedColoring_O]),
  sanity_compare(SortedColoring_O, SortedColoring),
  {Coloring,SpillIndex2}.
-else.
regalloc(CFG, Liveness, SpillIndex, SpillLimit, TgtMod, TgtCtx, _Options) ->
  Target = {TgtMod, TgtCtx},
  ?debug_msg("optimistic ~w\n",[TgtMod]),
  ?debug_msg("CFG: ~p\n",[CFG]),
  %% Build interference graph
  ?debug_msg("Build IG\n",[]),
  IG = hipe_ig:build(CFG, Liveness, TgtMod, TgtCtx),
  ?debug_msg("adjlist: ~p\n",[hipe_ig:adj_list(IG)]),
  ?debug_msg("IG:\n",[]),
  ?print_adjacent(IG),
  ?print_degrees(IG),
  ?print_spill_costs(IG),

  SavedSpillCosts = hipe_ig:spill_costs(IG),
  SavedAdjList = hipe_ig:adj_list(IG),

  ?debug_msg("Init\n",[]),
  No_temporaries = number_of_temporaries(CFG, Target),
  ?debug_msg("Coalescing RA: num_temps = ~p~n", [No_temporaries]),
  Allocatable = allocatable(Target),
  K = length(Allocatable),
  All_colors = colset_from_list(Allocatable),
  ?debug_msg("K: ~w~nAll_colors: ~p\n",[K, All_colors]), 

  %% Add registers with their own coloring
  ?debug_msg("Moves\n",[]),
  Move_sets = hipe_moves:new(IG),
  ?debug_msg("Move_sets:\n ~p\n",[Move_sets]),
  ?mov_print_memberships(Move_sets),

  ?debug_msg("Build Worklist\n",[]),

  Worklists = hipe_reg_worklists:new(IG, TgtMod, TgtCtx, CFG, K,
				     No_temporaries),
  ?debug_msg("New Worklists:\n ~p\n", [Worklists]),
  ?reg_print_memberships(Worklists),

  Alias = initAlias(No_temporaries),
  ?print_alias(Alias),

  {IG0, Moves0, Alias0, Worklists0} = 
    do_coalescing(IG, Worklists, Move_sets, Alias, K, Target),
  ?debug_msg("IG after coalescing:\n",[]),
  ?print_adjacent(IG0),
  ?print_degrees(IG0),
  ?print_spill_costs(IG0),
  ?debug_msg("Move_sets after coalescing:\n ~p\n",[Moves0]),
  ?mov_print_memberships(Moves0),
  ?debug_msg("New Worklists after coalescing:\n ~p\n",
             [Worklists0]),
  ?reg_print_memberships(Worklists0),

  {IG1, Worklists1, _Moves1, Alias1} = 
    do_simplify_or_spill(IG0, Worklists0, Moves0, Alias0, 
                         K, SpillLimit, Target),
  ?debug_msg("IG after simplify_or_spill:\n",[]),
  ?print_adjacent(IG1),
  ?print_degrees(IG1),
  ?print_spill_costs(IG1),
  ?debug_msg("Saved spill costs ~p~n", [SavedSpillCosts]),
  ?debug_msg("New Worklists after simplify_or_spill:\n ~p\n",
             [Worklists1]),
  ?reg_print_memberships(Worklists1),
  ?print_alias(Alias1),

  %% only for testing undoCoalescing and member_coalesced_to
  %test_undoCoalescing(No_temporaries, Alias1, Worklists1),

  %% only for testing fixAdj
  %?debug_msg("adj_lists_before_fixAdj ~n~p~n", [hipe_ig:adj_list(IG1)]),
  %IG2 = test_fixAdj(No_temporaries, SavedAdjList, IG1, Target),
  %?debug_msg("adj_lists__after_fixAdj ~n~p~n", [hipe_ig:adj_list(IG2)]),

  ?debug_msg("Init node sets\n",[]),
  Node_sets = hipe_node_sets:new(),
  %% ?debug_msg("NodeSet: ~w\n NonAlloc ~w\n",[Node_sets,non_alloc(CFG,Target)]),
  ?debug_msg("Default coloring\n",[]),
  {Color0,Node_sets1} = 
    defaultColoring(all_precoloured(Target),
		    initColor(No_temporaries), Node_sets, Target),
  ?debug_msg("Color0\n",[]),
  ?print_colors(No_temporaries, Color0),

  ?debug_msg("----------------------Assign colors _N\n",[]),

  Stack = hipe_reg_worklists:stack(Worklists1), 
  ?debug_msg("The stack _N ~p~n", [Stack]), 
  %SortedStack = sort_stack(Stack),
  %?debug_msg("The stack _N ~p~n", [SortedStack]), 

  %?debug_msg("Nodes _N ~w~n", [Node_sets1]),

  {Color1,Node_sets2,Alias2} =
    assignColors(Worklists1, Stack, Node_sets1, Color0, 
        	 No_temporaries, SavedAdjList, SavedSpillCosts, IG1, Alias1, All_colors, Target),
  ?print_colors(No_temporaries, Color1),
  ?debug_msg("Nodes:~w\nNodes2:~w\nNo_temporaries:~w\n",[Node_sets,Node_sets2,No_temporaries]),

  ?debug_msg("Build mapping _N ~w\n",[Node_sets2]),
  {Coloring, SpillIndex2} = build_namelist(Node_sets2,SpillIndex,Alias2,Color1),
  ?debug_msg("Coloring ~p\n",[Coloring]),
  {Coloring,SpillIndex2}.
-endif.

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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
do_coloring(IG, Worklists, Moves, Alias, K, SpillLimit, Target) ->
  Simplify = not(hipe_reg_worklists:is_empty_simplify(Worklists)),
  Coalesce = not(hipe_moves:is_empty_worklist(Moves)),
  Freeze   = not(hipe_reg_worklists:is_empty_freeze(Worklists)),
  Spill    = not(hipe_reg_worklists:is_empty_spill(Worklists)),
  if Simplify =:= true ->
      {IG0, Worklists0, Moves0} = 
	simplify_O(hipe_reg_worklists:simplify(Worklists),
		   IG, 
		   Worklists, 
		   Moves, 
		   K),
      do_coloring(IG0, Worklists0, Moves0, Alias, K, SpillLimit, Target);
     Coalesce =:= true ->
      {Moves0, IG0, Worklists0, Alias0} =
	coalesce_O(Moves, IG, Worklists, Alias, K, Target),
      do_coloring(IG0, Worklists0, Moves0, Alias0, K, SpillLimit, Target);
     Freeze =:= true ->
      {Worklists0, Moves0} = 
	freeze(K, Worklists, Moves, IG, Alias),
      do_coloring(IG, Worklists0, Moves0, Alias, K, SpillLimit, Target);
     Spill =:= true ->
      {Worklists0, Moves0} = 
	selectSpill_O(Worklists, Moves, IG, K, Alias, SpillLimit),
      do_coloring(IG, Worklists0, Moves0, Alias, K, SpillLimit, Target);
     true -> % Catchall case
      {IG, Worklists, Moves, Alias}
    end.
-endif.

%%----------------------------------------------------------------------
%% Function:    do_coalescing
%%
%% Description: Try to coalesce everything (find out later if it was
%%              possible).
%% Parameters:
%%   IG          --  An interference graph
%%   Moves       --  Moves sets, that is coalesced, constrained 
%%                   and so on.
%%   Alias       --  Tells if two temporaries can have their value
%%                   in the same register.
%%
%% Returns:
%%   IG          --  Updated interference graph
%%   Moves       --  Updated Moves structure 
%%   Alias       --  Updates Alias structure
%%   
%%----------------------------------------------------------------------

do_coalescing(IG, Worklists, Moves, Alias, K, Target) ->
  case hipe_moves:is_empty_worklist(Moves) of
    true ->
      {IG, Moves, Alias, Worklists};
    _ ->
      {Moves0, IG0, Alias0, Worklists0} =
	coalesce(Moves, IG, Worklists, Alias, K, Target),
      do_coalescing(IG0, Worklists0, Moves0, Alias0, K, Target)
  end.

%%----------------------------------------------------------------------
%% Function:    do_simplify_or_spill
%%
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

do_simplify_or_spill(IG, Worklists, Moves, Alias, K, SpillLimit, Target) ->
  Simplify = not(hipe_reg_worklists:is_empty_simplify(Worklists)),
  Spill    = not(hipe_reg_worklists:is_empty_spill(Worklists)),
  if Simplify =:= true ->
      {IG0, Worklists0, Moves0} = 
	simplify(hipe_reg_worklists:simplify(Worklists),
		 IG, 
		 Worklists, 
		 Moves, 
		 K),
      do_simplify_or_spill(IG0, Worklists0, Moves0, Alias,
		  K, SpillLimit, Target);
     Spill =:= true ->
      Worklists0 = 
	selectSpill(Worklists, IG, SpillLimit),
      do_simplify_or_spill(IG, Worklists0, Moves, Alias, 
			   K, SpillLimit, Target);
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
%%               function simplify all nodes it can at once.
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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
simplify_O([], IG, Worklists, Moves, _K) -> 
  {IG, Worklists, Moves};
simplify_O([Node|Nodes], IG, Worklists, Moves, K) ->
  Worklists0 = hipe_reg_worklists:remove_simplify(Node, Worklists),
  ?debug_msg("putting ~w on stack~n",[Node]),
  Adjacent = adjacent(Node, IG, Worklists0),
  Worklists01 = hipe_reg_worklists:push_stack(Node, Adjacent, Worklists0),
  {New_ig, Worklists1, New_moves} =
    decrement_degree_O(Adjacent, IG, Worklists01, Moves, K),
  simplify_O(Nodes, New_ig, Worklists1, New_moves, K).
-endif.

%%----------------------------------------------------------------------
%% Function:    simplify
%%
%% Description: Simplify graph by removing nodes of low degree. This
%%               function simplify all nodes it can at once.
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
  {New_ig, Worklists1} = decrement_degree(Adjacent, IG, Worklists01, K),
  simplify(Nodes, New_ig, Worklists1, Moves, K).

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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
decrement_degree_O([], IG, Worklists, Moves, _K) -> 
  {IG, Worklists, Moves};
decrement_degree_O([Node|Nodes], IG, Worklists, Moves, K) ->
  PrevDegree = hipe_ig:get_node_degree(Node, IG),
  IG0 = hipe_ig:dec_node_degree(Node, IG),
  case PrevDegree =:= K of
    true ->
      AdjList = hipe_ig:node_adj_list(Node, IG0),
      %% OK since Node (a) is still in IG, and (b) cannot be adjacent to itself.
      Moves00 = enable_moves_active_to_worklist(hipe_moves:node_movelist(Node, Moves),
						Moves),
      Moves0 = enable_moves(AdjList, Worklists, Moves00),
      Worklists0 = hipe_reg_worklists:remove_spill(Node, Worklists),
      case hipe_moves:move_related(Node, Moves0) of
	true ->
	  Worklists1 = hipe_reg_worklists:add_freeze(Node, Worklists0),
	  decrement_degree_O(Nodes, IG0, Worklists1, Moves0, K);
	_ ->
	  Worklists1 = hipe_reg_worklists:add_simplify(Node, Worklists0),
	  decrement_degree_O(Nodes, IG0, Worklists1, Moves0, K)
      end;
     _ ->
      decrement_degree_O(Nodes, IG0, Worklists, Moves, K)
  end.
-endif.

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

decrement_degree([], IG, Worklists, _K) -> 
  {IG, Worklists};
decrement_degree([Node|Nodes], IG, Worklists, K) ->
  PrevDegree = hipe_ig:get_node_degree(Node, IG),
  IG0 = hipe_ig:dec_node_degree(Node, IG),
  case PrevDegree =:= K of
    true ->
      Worklists0 = hipe_reg_worklists:remove_spill(Node, Worklists),
      Worklists1 = hipe_reg_worklists:add_simplify(Node, Worklists0),
      decrement_degree(Nodes, IG0, Worklists1, K);
     _ ->
      decrement_degree(Nodes, IG0, Worklists, K)
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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
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
-endif.

%%----------------------------------------------------------------------
%% Function:    enable_moves_active_to_worklist
%%
%% Description: Make (move-related) nodes that are not yeat considered for
%%               coalescing, ready for possible coalescing.
%%               
%% Parameters:
%%   [Node|Nodes]   --  A list of move nodes
%%   Moves          --  The moves data-structure
%%
%%   Returns: 
%%     An updated moves data-structure
%%----------------------------------------------------------------------

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
enable_moves_active_to_worklist([], Moves) -> Moves;
enable_moves_active_to_worklist([Node|Nodes], Moves) ->
  case hipe_moves:member_active(Node, Moves) of
    true ->
      New_moves = 
	hipe_moves:add_worklist(Node, hipe_moves:remove_active(Node, Moves)),
      enable_moves_active_to_worklist(Nodes, New_moves);
    _ ->
      enable_moves_active_to_worklist(Nodes, Moves)
  end.
-endif.

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
sanity_compare(Coloring, Coloring_N) ->
  case compare_sanity(Coloring, Coloring_N) of
    false ->
      ?debug_msg("mismatch for coloring: ~n~p~n~p", [Coloring, Coloring_N]);
    _ -> true
  end.
compare_sanity({[], _C}, {[], _C_N}) ->
  ?debug_msg("Sanity - OK!~n", []),
  true;
compare_sanity({_Coloring_list, _C}, {[], _C_N}) ->
  ?debug_msg("Sanity - unequal numbers~n", []),
  false;
compare_sanity({[], _C}, {_Coloring_list_N, _C_N}) ->
  ?debug_msg("Sanity - unequal numbers~n", []),
  false;
compare_sanity({[Color|Coloring_list], C}, {[Color_N|Coloring_list_N], C_N}) ->
  case element(1, Color) =:= element(1, Color_N) of
    false ->
      ?debug_msg("Sanity - unequal measure~n", []),
      false;
    _ -> 
      case element(2, Color) =:= element(2, Color_N) of
        false ->  
	  ?debug_msg("Sanity - unequal color~n", []),
	  false;
	_ ->
	  case C =:= C_N of
	    false ->
	      ?debug_msg("Sanity - unequal last element~n", []),
	      false;
	    _ ->
	      compare_sanity({Coloring_list, C}, {Coloring_list_N, C_N})
	  end
      end
  end.
-endif.
  
  
%% Build the namelists, these functions are fast hacks, they use knowledge 
%% about data representation that they shouldn't know, bad abstraction.

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
build_namelist_O(NodeSets,Index,Alias,Color) ->
  ?debug_msg("NodeSets ~w~n", [NodeSets]),
  ?debug_msg("Building mapping\n",[]),
  ?debug_msg("Vector to list\n",[]),
  AliasList = 
    build_alias_list(aliasToList(Alias),
		     0, %% The first temporary has index 0
		     []), %% Accumulator
  ?debug_msg("Alias list:~p\n",[AliasList]),
  ?debug_msg("Coalesced\n",[]),
  NL1 = build_coalescedlist(AliasList,Color,Alias,[]),
  ?debug_msg("Coalesced list:~p\n",[NL1]),
  ?debug_msg("Regs\n",[]),
  NL2 = build_reglist_O(hipe_node_sets:colored(NodeSets),Color,NL1),
  ?debug_msg("Regs list:~p\n",[NL2]),
  ?debug_msg("Spills\n",[]),
  build_spillist(hipe_node_sets:spilled(NodeSets),Index,NL2).
-endif.

build_namelist(NodeSets,Index,Alias,Color) ->
  ?debug_msg("NodeSets _N ~w~n", [NodeSets]),
  ?debug_msg("Building mapping _N\n",[]),
  ?debug_msg("Vector to list _N\n",[]),
  AliasList = 
    build_alias_list(aliasToList(Alias),
		     0, %% The first temporary has index 0
		     []), %% Accumulator
  ?debug_msg("Alias list _N:~p\n",[AliasList]),
  ?debug_msg("Coalesced\n",[]),
  NL1 = build_coalescedlist(AliasList,Color,Alias,[]),
  ?debug_msg("Coalesced list:~p\n",[NL1]),
  ?debug_msg("Regs _N\n",[]),
  ColoredNodes = hipe_node_sets:colored(NodeSets),
  ?debug_msg("ColoredNodes ~p~n", [ColoredNodes]),
  NL2 = build_reglist_N(ColoredNodes,Color,NL1,NL1),
  ?debug_msg("Regs list _N:~p\n",[NL2]),
  ?debug_msg("Spills _N\n",[]),
  build_spillist(hipe_node_sets:spilled(NodeSets),Index,NL2).

build_spillist([],Index,List) ->
  {List,Index};
build_spillist([Node|Nodes],Index,List) ->
  ?debug_msg("[~p]: Spill ~p to ~p\n", [?MODULE,Node,Index]),
  build_spillist(Nodes,Index+1,[{Node,{spill,Index}}|List]).

build_coalescedlist([],_Color,_Alias,List) ->
  List;
build_coalescedlist([Node|Ns],Color,Alias,List) when is_integer(Node) ->
  ?debug_msg("Alias of ~p is ~p~n",[Node,getAlias(Node,Alias)]),
  AC = getColor(getAlias(Node,Alias),Color),
  build_coalescedlist(Ns,Color,Alias,[{Node,{reg,AC}}|List]).

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
build_reglist_O([],_Color,List) -> 
  List;
build_reglist_O([Node|Ns],Color,List) ->
  build_reglist_O(Ns,Color,[{Node,{reg,getColor(Node,Color)}}|List]).
-endif.

build_reglist_N([],_Color,List,_OrgList) -> 
  List;
build_reglist_N([Node|Ns],Color,List,OrgList) ->
  %% XXX this could be done more efficiently if both lists were sorted
  case is_already_in_list(Node, OrgList) of
    true -> build_reglist_N(Ns, Color, List, OrgList);
    _ -> build_reglist_N(Ns,Color,[{Node,{reg,getColor(Node,Color)}}|List], OrgList)
  end.

is_already_in_list(_Node, []) ->
  false;
is_already_in_list(Node, [L|List]) ->
  ?debug_msg("---test--- Node ~w  element ~w~n", [Node, element(1, L)]),
  case Node =:= element(1, L) of
    true -> true;
    _ -> is_already_in_list(Node, List)
  end.

build_alias_list([], _I, List) ->
  List;
build_alias_list([Alias|Aliases], I, List) when is_integer(Alias) ->
  build_alias_list(Aliases, I+1, [I|List]);
build_alias_list([_Alias|Aliases], I, List) ->
  build_alias_list(Aliases, I+1, List).

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
sort_stack([]) -> [];
sort_stack([Pivot|Rest]) ->
  {Smaller, Bigger} = sort_stack_split(Pivot, Rest),
  lists:append(sort_stack(Smaller), [Pivot|sort_stack(Bigger)]).

sort_stack_split(Pivot, L) ->
  sort_stack_split(Pivot, L, [], []).

sort_stack_split(_Pivot, [], Smaller, Bigger) -> 
  {Smaller, Bigger};
sort_stack_split(Pivot, [H|T], Smaller, Bigger) when element(1, H) > element(1, Pivot) ->
  sort_stack_split(Pivot, T, [H|Smaller], Bigger);
sort_stack_split(Pivot, [H|T], Smaller, Bigger) ->
  sort_stack_split(Pivot, T, Smaller, [H|Bigger]).
-endif.

%sort([]) -> [];
%sort([Pivot|Rest]) ->
%  {Smaller, Bigger} = sort_split(Pivot, Rest),
%  lists:append(sort(Smaller), [Pivot|sort(Bigger)]).
%
%sort_split(Pivot, L) ->
%  sort_split(Pivot, L, [], []).
%
%sort_split(_Pivot, [], Smaller, Bigger) -> {Smaller, Bigger};
%sort_split(Pivot, [H|T], Smaller, Bigger) when H > Pivot ->
%  sort_split(Pivot, T, [H|Smaller], Bigger);
%sort_split(Pivot, [H|T], Smaller, Bigger) ->
%  sort_split(Pivot, T, Smaller, [H|Bigger]).

%%----------------------------------------------------------------------
%% Function:    assignColors
%%
%% Description: Tries to assign colors to nodes in a stack.
%% Parameters:
%%   Worklists       --  The Worklists data structure.
%%   Stack           --  The SelectStack built by the Select function, 
%%                       this stack contains tuples in the form {Node,Edges} 
%%                       where Node is the Node number and Edges is an ordset 
%%                       containing the numbers of all the adjacent nodes.
%%   NodeSets        --  This is a record containing all the different node 
%%                       sets that are used in the register allocator.
%%   Color           --  A mapping from nodes to their respective color.
%%   No_temporaries  --  Number of temporaries.
%%   SavedAdjList    --  Saved adjacency list (from before coalescing).
%%   SavedSpillCosts --  Saved spill costs (from before coalescing).
%%   IG              --  The interference graph.
%%   Alias           --  This is a mapping from nodes to nodes. If a node has 
%%                       been coalesced, this mapping shows the alias for that 
%%                       node.
%%   AllColors       --  This is an ordset containing all the available colors
%%   Target          --  The module containing the target-specific functions,
%%                       along with its context data.
%%
%% Returns:
%%   Color          --  A mapping from nodes to their respective color.
%%   NodeSets       --  The updated node sets.
%%   Alias          --  The updated aliases.
%%----------------------------------------------------------------------

assignColors(Worklists, Stack, NodeSets, Color, No_Temporaries,
	     SavedAdjList, SavedSpillCosts, IG, Alias, AllColors, Target) ->
  case Stack of
    [] ->
      {Color,NodeSets,Alias};
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
	  case hipe_reg_worklists:member_coalesced_to(Node, Worklists) of
	    true ->
	      ?debug_msg("Alias case. Undoing coalescing.~n", []),
              {Alias1, IG1, NodeSets1, Color1, Stack2} = tryPrimitiveNodes(Node, Stack1, NodeSets, AllColors, Color, No_Temporaries, SavedAdjList, SavedSpillCosts, IG, Alias, Target),
              %{Alias1, IG1, NodeSets1, Color1, Stack2} = {Alias, IG, NodeSets, Color, Stack1},
	      assignColors(Worklists, Stack2, NodeSets1, Color1, No_Temporaries, SavedAdjList, SavedSpillCosts, IG1, Alias1, AllColors, Target);
	    false ->
	      ?debug_msg("Spill case. Spilling node.~n", []),
	      NodeSets1 = hipe_node_sets:add_spilled(Node, NodeSets),
	      assignColors(Worklists, Stack1, NodeSets1, Color, No_Temporaries, SavedAdjList, SavedSpillCosts, IG, Alias, AllColors, Target)
	  end;
	false -> % Color case
	  Col = colset_smallest(OkColors),
	  NodeSets1 = hipe_node_sets:add_colored(Node, NodeSets),
	  Color1 = setColor(Node, physical_name(Col,Target), Color),
	  ?debug_msg("Color case. Assigning color ~p to node.~n", [Col]),
	  assignColors(Worklists, Stack1, NodeSets1, Color1, No_Temporaries, SavedAdjList, SavedSpillCosts, IG, Alias, AllColors, Target)
      end
  end.

%%----------------------------------------------------------------------
%% Function:    tryPrimitiveNodes
%%
%% Description: Undoes coalescing of a non-colorable coalesced node and tries
%%              to assign colors to its primitives, such that the cheapest
%%              potential spill cost is achieved.
%% Parameters:
%%   Node            --  The representative node to undo coalescing for.
%%   Stack           --  The SelectStack built by the Select function, 
%%                       this stack contains tuples in the form {Node,Edges} 
%%                       where Node is the Node number and Edges is an ordset 
%%                       containing the numbers of all the adjacent nodes.
%%   NodeSets        --  This is a record containing all the different node 
%%                       sets that are used in the register allocator.
%%   AllColors       --  This is an ordset containing all the available colors.
%%   No_temporaries  --  Number of temporaries.
%%   SavedAdjList    --  Saved adjacency list (from before coalescing).
%%   SavedSpillCosts --  Saved spill costs (from before coalescing).
%%   IG              --  The interference graph.
%%   Alias           --  This is a mapping from nodes to nodes. If a node has 
%%                       been coalesced, this mapping shows the alias for that 
%%                       node.
%%   Target          --  The module containing the target-specific functions,
%%                       along with its context data.
%%
%% Returns:
%%   Alias           --  The restored aliases after the uncoalescing.
%%   IG              --  An updated interference graph after the uncoalescing.
%%   NodeSets        --  The updated node sets.
%%   Color           --  A mapping from nodes to their respective color.
%%   Stack           --  The updated SelectStack with non-colored primitives
%%                       placed at the bottom.
%%----------------------------------------------------------------------

tryPrimitiveNodes(Node, Stack, NodeSets, AllColors, Color, No_temporaries, SavedAdjList, SavedSpillCosts, IG, Alias, Target) ->
  ?debug_msg("Undoing coalescing of node ~p.~n", [Node]),
  {PrimitiveNodes, Alias1, IG1} = undoCoalescing(Node, No_temporaries, Alias, SavedAdjList, IG, Target),
  ?debug_msg("Spilling non-colorable primitives.~n", []),
  {ColorableNodes, NodeSets1} = spillNonColorablePrimitives([], PrimitiveNodes, NodeSets, AllColors, Color, SavedAdjList, Alias1),
  ?debug_msg("Generating splits of colorable nodes.~n", []),
  Splits = splits(ColorableNodes, SavedSpillCosts),
  {NodeSets2, Color1, Stack1} = processSplits(Splits, AllColors, IG1, Color, NodeSets1, Alias1, Target, Stack),
  {Alias1, IG1, NodeSets2, Color1, Stack1}.

%% Spill all non-colorable primitives and return the remaining set of nodes.

spillNonColorablePrimitives(ColorableNodes, [], NodeSets, _AllColors, _Color, _SavedAdjList, _Alias) ->
  {ColorableNodes, NodeSets};
spillNonColorablePrimitives(ColorableNodes, [Primitive|Primitives], NodeSets, AllColors, Color, SavedAdjList, Alias) ->
  OkColors = findOkColors(hipe_adj_list:edges(Primitive, SavedAdjList), AllColors, Color, Alias),
  case colset_is_empty(OkColors) of
    true -> % Spill case
      ?debug_msg("  Spilling primitive node ~p.~n", [Primitive]),
      NodeSets1 = hipe_node_sets:add_spilled(Primitive, NodeSets),
      spillNonColorablePrimitives(ColorableNodes, Primitives, NodeSets1, AllColors, Color, SavedAdjList, Alias);
    false -> % Colorable case
      ?debug_msg("  Primitive node ~p is colorable.~n", [Primitive]),
      spillNonColorablePrimitives([Primitive|ColorableNodes], Primitives, NodeSets, AllColors, Color, SavedAdjList, Alias)
  end.

%% Generate all splits of colorable primitives, sorted in spill cost order.

splits([], _SavedSpillCosts) ->
  [{[], [], 0}];
splits([L|Ls], SavedSpillCosts) ->
  Spl = splits(Ls, SavedSpillCosts),
  SpillCost = hipe_spillcost:spill_cost(L, SavedSpillCosts),
  Spl1 = [splits_1(S, L) || S <- Spl],
  Spl2 = [splits_2(S, L, SpillCost) || S <- Spl],
  spillCostOrderedMerge(Spl1, Spl2, []).

splits_1({Cols, NonCols, OldSpillCost}, L) ->
    {[L|Cols], NonCols, OldSpillCost}.

splits_2({Cols, NonCols, OldSpillCost}, L, SpillCost) ->
    {Cols, [L|NonCols], OldSpillCost + SpillCost}.
 
%% Merge two ordered sub-splits into one.

spillCostOrderedMerge(Spl1, [], Spl) ->
  lists:reverse(Spl, Spl1);
spillCostOrderedMerge([], Spl2, Spl) ->
  lists:reverse(Spl, Spl2);
spillCostOrderedMerge(Spl1, Spl2, Spl) ->
  {_, _, SpillCost1} = hd(Spl1),
  {_, _, SpillCost2} = hd(Spl2),
  case SpillCost1 =< SpillCost2 of
    true ->
      spillCostOrderedMerge(tl(Spl1), Spl2, [hd(Spl1)|Spl]);
    false ->
      spillCostOrderedMerge(Spl1, tl(Spl2), [hd(Spl2)|Spl])
  end.

%% Process splits, finding the one with the smallest spill cost that
%% can be assigned one color.

processSplits([], _AllColors, _IG, Color, NodeSets, _Alias, _Target, Stack) ->
  {NodeSets, Color, Stack};
processSplits([{Cols, NonCols, _SpillCost}|Splits], AllColors, IG, Color, NodeSets, Alias, Target, Stack) ->
  OkColors = findCommonColors(Cols, IG, Color, Alias, AllColors),
  case colset_is_empty(OkColors) of
    false -> % This split can be colored with one color - use it
      ?debug_msg("Found a colorable split.~n", []),
      Col = colset_smallest(OkColors),
      {NodeSets1, Color1} = colorSplit(Cols, Col, NodeSets, Color, Target),
      Stack1 = enqueueSplit(NonCols, IG, Stack),
      {NodeSets1, Color1, Stack1};
    true -> % This split cannot be colored with one color - try another
      ?debug_msg("Unable to color split.~n", []),
      processSplits(Splits, AllColors, IG, Color, NodeSets, Alias, Target, Stack)
  end.

%% Find the set of colors that can be assigned to one split.

findCommonColors([], _IG, _Color, _Alias, OkColors) ->
  OkColors;
findCommonColors([Primitive|Primitives], IG, Color, Alias, OkColors) ->
  OkColors1 = findOkColors(hipe_ig:node_adj_list(Primitive, IG), OkColors, Color, Alias),
  findCommonColors(Primitives, IG, Color, Alias, OkColors1).

%% Color nodes in a split.

colorSplit([], _Col, NodeSets, Color, _Target) ->
  {NodeSets, Color};
colorSplit([Node|Nodes], Col, NodeSets, Color, Target) ->
  ?debug_msg("  Coloring node ~p with color ~p.~n", [Node, Col]),
  NodeSets1 = hipe_node_sets:add_colored(Node, NodeSets),
  Color1 = setColor(Node, physical_name(Col,Target), Color),
  colorSplit(Nodes, Col, NodeSets1, Color1, Target).

%% Place non-colorable nodes in a split at the bottom of the SelectStack.

enqueueSplit([], _IG, Stack) ->
  Stack;
enqueueSplit([Node|Nodes], IG, Stack) ->
  ?debug_msg("  Placing node ~p at the bottom of the stack.~n", [Node]),
  Edges = hipe_ig:node_adj_list(Node, IG),
  Stack1 = Stack ++ [{Node, Edges}],
  enqueueSplit(Nodes, IG, Stack1).

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
%%   Target         --  The module containing the target-specific functions,
%%                      along with its context data.
%%
%% Returns:
%%   Color          --  A mapping from nodes to their respective color.
%%   NodeSets       --  The updated node sets.
%%----------------------------------------------------------------------

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
assignColors_O(Stack,NodeSets,Color,Alias,AllColors,Target) ->
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
	  assignColors_O(Stack1, NodeSets1, Color, Alias, AllColors, Target);
	false -> % Colour case
	  Col = colset_smallest(OkColors),
	  NodeSets1 = hipe_node_sets:add_colored(Node, NodeSets),
	  Color1 = setColor(Node, physical_name(Col,Target), Color),
	  assignColors_O(Stack1, NodeSets1, Color1, Alias, AllColors, Target)
      end
  end.
-endif.

%%---------------------------------------------------------------------
%% Function:     defaultColoring
%% 
%% Description: Make the default coloring
%% Parameters:
%%   Regs           -- The list of registers to be default colored
%%   Color          -- The color mapping that shall be changed
%%   NodeSets       -- The node sets that shall be updated
%%   Target         -- The module containing the target-specific functions,
%%                     along with its context data.
%%
%% Returns:
%%   NewColor       -- The updated color mapping
%%   NewNodeSets    -- The updated node sets
%%---------------------------------------------------------------------

defaultColoring([], Color, NodeSets, _Target) ->
  {Color,NodeSets};
defaultColoring([Reg|Regs], Color, NodeSets, Target) ->
  Color1 = setColor(Reg,physical_name(Reg,Target), Color),
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
  case BitN > 16#ffff of
    true ->
      bitN_log2(BitN bsr 16, ShiftN + 16);
    _ ->
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

setColor(Node, Color, {colmap, ColMap} = C) ->
  hipe_bifs:array_update(ColMap, Node, Color),  
  C.

-ifdef(DEBUG_PRINTOUTS).
printColors(0, _) ->
  true;
printColors(Node, {colmap, ColMap} = C) ->
  NextNode = Node - 1,
  ?debug_msg("node ~w  color ~w~n", [NextNode, hipe_bifs:array_sub(ColMap, NextNode)]),
  printColors(NextNode, C).
-endif.

%%%
%%% Alias ADT providing a partial mapping from nodes to nodes.
%%%

initAlias(NrNodes) ->
  {alias, hipe_bifs:array(NrNodes, [])}.

%% Get alias for a node. 
%% Note that non-aliased nodes could be represented in
%% two ways, either not aliased or aliased to itself. 
%% Including the latter case prevents looping bugs.
getAlias(Node, {alias, AliasMap} = Alias) ->
  case hipe_bifs:array_sub(AliasMap, Node) of
    [] ->
      Node;
    Node ->
      Node;
    AliasNode ->
      getAlias(AliasNode, Alias)
  end.

-ifdef(DEBUG_PRINTOUTS).
printAlias({alias, AliasMap} = Alias) ->
  ?debug_msg("Aliases:\n",[]),
  printAlias(hipe_bifs:array_length(AliasMap), Alias).

printAlias(0, {alias, _}) ->
  true ;
printAlias(Node, {alias, _AliasMap} = Alias) ->
  ?debug_msg("alias ~p ~p\n", [Node - 1, getAlias(Node - 1, Alias)]),
  printAlias(Node - 1, Alias).
-endif.

setAlias(Node, AliasNode, {alias, AliasMap} = Alias) ->
  hipe_bifs:array_update(AliasMap, Node, AliasNode),
  Alias.

aliasToList({alias, AliasMap}) ->
  aliasToList(AliasMap, hipe_bifs:array_length(AliasMap), []).

aliasToList(AliasMap, I1, Tail) ->
  I0 = I1 - 1,
  case I0 >= 0 of
    true ->
      aliasToList(AliasMap, I0, [hipe_bifs:array_sub(AliasMap, I0)|Tail]);
    _ ->
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
      {Moves0, IG, Alias};
    {Move,Moves0} ->
      {Dest,Source} = hipe_moves:get_move(Move, Moves0),
      ?debug_msg("Testing nodes ~p and ~p for coalescing~n",[Dest,Source]),
      Alias_src = getAlias(Source, Alias),
      Alias_dst = getAlias(Dest, Alias),
      {U,V} = case is_precoloured(Alias_dst, Target) of
		true -> {Alias_dst, Alias_src};
		false -> {Alias_src, Alias_dst}
	      end,
      %% When debugging, check that neither V nor U is on the stack.
      case U =:= V of
        true ->
	  %% drop coalesced move Move
	  {Moves0, IG, Alias, Worklists};
	_ ->
	  case (is_precoloured(V, Target) orelse
		hipe_ig:nodes_are_adjacent(U, V, IG)) of 
	    true ->
	      %% drop constrained move Move
	      {Moves0, IG, Alias, Worklists};
	    false ->
	      case (case is_precoloured(U, Target) of
		      true ->
			AdjV = hipe_ig:node_adj_list(V, IG),
			all_adjacent_ok(AdjV, U, Worklists, IG, K, Target);
		      false ->
			AdjV = hipe_ig:node_adj_list(V, IG),
			AdjU = hipe_ig:node_adj_list(U, IG),
			conservative(AdjU, AdjV, U, Worklists, IG, K)
		    end) of
		true ->
		  %% drop coalesced move Move
		  {IG1, Alias1, Worklists1} = 
		    combine(U, V, IG, Alias, Worklists, K, Target),
		  {Moves0, IG1, Alias1, Worklists1};
		false ->
		  Moves1 = hipe_moves:add_active(Move, Moves0),
		  {Moves1, IG, Alias, Worklists}
	      end
	  end
      end
  end.

%%----------------------------------------------------------------------
%% Function:    coalesce_O
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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
coalesce_O(Moves, IG, Worklists, Alias, K, Target) ->
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
      {U,V} = case is_precoloured(Alias_dst, Target) of
		true -> {Alias_dst, Alias_src};
		false -> {Alias_src, Alias_dst}
	      end,
      %% When debugging, check that neither V nor U is on the stack.
      case U =:= V of
         true ->
	  Moves1 = Moves0, % drop coalesced move Move
	  Worklists1 = add_worklist(Worklists, U, K, Moves1, IG, Target),
	  {Moves1, IG, Worklists1, Alias};
	 _ ->
	  case (is_precoloured(V, Target) orelse
		hipe_ig:nodes_are_adjacent(U, V, IG)) of 
	    true ->
	      Moves1 = Moves0, % drop constrained move Move
	      Worklists1 = add_worklist(Worklists, U, K, Moves1, IG, Target),
	      Worklists2 = add_worklist(Worklists1, V, K, Moves1, IG, Target),
	      {Moves1, IG, Worklists2, Alias};
	    false ->
	      case (case is_precoloured(U, Target) of
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
		    combine_O(U, V, IG, Worklists, Moves1, Alias, K, Target),
		  Worklists2 = add_worklist(Worklists1, U, K, Moves2, IG1, Target),
		  {Moves2, IG1, Worklists2, Alias1};
		false ->
		  Moves1 = hipe_moves:add_active(Move, Moves0),
		  {Moves1, IG, Worklists, Alias}
	      end
	  end
      end
  end.
-endif.

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
%%   Target        -- The containing the target-specific functions, along with
%%                    its context data.
%%   
%% Returns:
%%   Worklists (updated)
%%----------------------------------------------------------------------

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
add_worklist(Worklists, U, K, Moves, IG, Target) ->
  case (not(is_precoloured(U, Target))
	andalso not(hipe_moves:move_related(U, Moves))
	andalso (hipe_ig:is_trivially_colourable(U, K, IG))) of
    true ->
      hipe_reg_worklists:transfer_freeze_simplify(U, Worklists);
    false ->
      Worklists
  end.
-endif.

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
       
-ifdef(COMPARE_ITERATED_OPTIMISTIC).
combine_O(U, V, IG, Worklists, Moves, Alias, K, Target) ->
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
    combine_edges_O(AdjV, U, IG, Worklists11, Moves2, K, Target),

  New_worklists = case (not(hipe_ig:is_trivially_colourable(U, K, IG1))
			andalso hipe_reg_worklists:member_freeze(U, Worklists2)) of
		    true -> hipe_reg_worklists:transfer_freeze_spill(U, Worklists2);
		    false -> Worklists2
		  end,
  {IG1, New_worklists, Moves3, Alias1}.
-endif.

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
       
combine(U, V, IG, Alias, Worklists, K, Target) ->
  ?debug_msg("N_Coalescing ~p and ~p to ~p~n",[V,U,U]),
  Worklists1 = hipe_reg_worklists:add_coalesced(V, U, Worklists),
  Alias1 = setAlias(V, U, Alias),
  AdjV = hipe_ig:node_adj_list(V, IG),
  IG1 = combine_edges(AdjV, U, IG, Worklists1, K, Target),
  {IG1, Alias1, Worklists1}.

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

combine_edges([], _U, IG, _Worklists, _K, _Target) ->
  IG;
combine_edges([T|Ts], U, IG, Worklists, K, Target={TgtMod,TgtCtx}) ->
  case hipe_reg_worklists:member_stack_or_coalesced(T, Worklists) of
    true -> combine_edges(Ts, U, IG, Worklists, K, Target);
    _ ->
      IG1 = hipe_ig:add_edge(T, U, IG, TgtMod, TgtCtx),
      IG2 = case is_precoloured(T, Target) of
	      true -> IG1;
	      false -> hipe_ig:dec_node_degree(T, IG1)
	    end,
      combine_edges(Ts, U, IG2, Worklists, K, Target)
  end.

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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
combine_edges_O([], _U, IG, Worklists, Moves, _K, _Target) ->
  {IG, Worklists, Moves};
combine_edges_O([T|Ts], U, IG, Worklists, Moves, K, Target={TgtMod,TgtCtx}) ->
  case hipe_reg_worklists:member_stack_or_coalesced(T, Worklists) of
    true -> combine_edges_O(Ts, U, IG, Worklists, Moves, K, Target);
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
      IG1 = hipe_ig:add_edge(T, U, IG, TgtMod, TgtCtx),
      NewDegree = hipe_ig:get_node_degree(T, IG1),
      Worklists0 =
	if NewDegree =:= K, OldDegree =:= K-1 ->
	    %% ?debug_msg("~w:combine_edges_O(): repairing worklist membership for node ~w\n", [?MODULE,T]),
	    %% The node T must be on the freeze worklist:
	    %% 1. Since we're coalescing, the simplify worklist must have been
	    %%    empty when combine_edges_O() started.
	    %% 2. decrement_degree() may put the node T back on the simplify
	    %%    worklist, but that occurs after the worklists repair step.
	    %% 3. There are no duplicates among the edges.
	    Worklists00 = hipe_reg_worklists:remove_freeze(T, Worklists),
	    hipe_reg_worklists:add_spill(T, Worklists00);
	   true ->
	    Worklists
	end,
      {IG2, Worklists1, Moves1} =
	decrement_degree_O([T], IG1, Worklists0, Moves, K),
      combine_edges_O(Ts, U, IG2, Worklists1, Moves1, K, Target)
  end.
-endif.

%%----------------------------------------------------------------------
%% Function:    undoCoalescing
%%
%% Description: Returns necessary information for a coalesced node
%%
%% Parameters:
%%   N              -- The node to uncoalesce
%%   No_temporaries -- Number of temporaries
%%   Alias          -- The Alias vector before undoing
%%   SavedAdj       -- Saved adjacency list
%%   IG             -- Interference graph
%%   Target         -- The module containing the target-specific functions,
%%                     along with its context data.
%%   
%% Returns:
%%   list of primitive nodes, that is all nodes that were previously
%%           coalesced to N
%%   updated alias vector
%%   updated Interferece graph
%%----------------------------------------------------------------------
undoCoalescing(N, No_temporaries, Alias, SavedAdj, IG, Target) ->
  Primitives = findPrimitiveNodes(No_temporaries, N, Alias),
  Alias1 = restoreAliases(Primitives, Alias),
  IG1 = fixAdj(N, SavedAdj, IG, Target),
  {Primitives, Alias1, IG1}.

%% Restore aliasinfo for primitive nodes, that is 
%% unalias the node sthat were aliased to the primitive
%% nodes. Note that an unaliased node could be
%% represented in two ways, either not aliased or aliased
%% to itself. See also getAlias
restoreAliases([], Alias) ->
  Alias;
restoreAliases([Primitive|Primitives], Alias) ->
  Alias1 = setAlias(Primitive, Primitive, Alias),
  restoreAliases(Primitives, Alias1).

%% find the primitive nodes to N, that is find all
%% nodes that are aliased to N
findPrimitiveNodes(No_temporaries, N, Alias) ->
  findPrimitiveNodes(No_temporaries, N, Alias, []).

findPrimitiveNodes(0, _N, _Alias, PrimitiveNodes) ->
  PrimitiveNodes;
findPrimitiveNodes(Node, N, Alias, PrimitiveNodes) ->
  NextNode = Node - 1,
  case (getAlias(NextNode, Alias) =:= N) of
    true -> findPrimitiveNodes(NextNode, N, Alias, [NextNode | PrimitiveNodes]);
    _ -> findPrimitiveNodes(NextNode, N, Alias, PrimitiveNodes)
  end.

%test_undoCoalescing(No_temporaries, Alias, Worklists) ->
%  test_undoCoalescing(No_temporaries, No_temporaries, Alias, Worklists).
%
%test_undoCoalescing(0, _No_temporaries, _Alias, _Worklists) ->
%  true;
%test_undoCoalescing(Node, No_temporaries, Alias, Worklists) ->
%  %?debug_msg("++ the adj list: ~p~n", [SavedAdj]),
%  %?debug_msg("Node ~p~n", [Node]),
%  NextNode = Node - 1,
%  Coalesced_to = hipe_reg_worklists:member_coalesced_to(NextNode, Worklists),
%  ?debug_msg("³³-- member coalesced: ~p~n", [Coalesced_to]),
%  {Primitives, Alias1} = undoCoalescing(NextNode, No_temporaries, Alias),
%  ?debug_msg("½½-- primitivenodes ~w\n", [Primitives]),
%  case (Coalesced_to) of
%    true -> printAlias(Alias1);
%    _ -> true
%  end,
%  test_undoCoalescing(NextNode, No_temporaries, Alias, Worklists).

%%----------------------------------------------------------------------
%% Function:    fixAdj
%%
%% Description: Fixes adajency set and adjacency list when undoing coalescing
%%
%% Parameters:
%%   N             -- Node that should be uncoalesced
%%   SavedAdj      -- Saved adjacency list
%%   IG            -- Interference graph
%%   Target        -- The module containing the target-specific functions, along
%%                    with its context data.
%%   
%% Returns:
%%   updated Interferece graph
%%----------------------------------------------------------------------
fixAdj(N, SavedAdj, IG, Target) ->
  %Saved = hipe_vectors:get(SavedAdj, N),
  Saved = hipe_adj_list:edges(N, SavedAdj),
  ?debug_msg("§§--adj to ~p: ~p~n", [N, Saved]),
  Adj = hipe_ig:node_adj_list(N, IG),
  ?debug_msg("««--adj to ~p: ~p~n", [N, Adj]),
  New = findNew(Adj, Saved),
  ?debug_msg("++--new adj to ~p: ~p~n", [N, New]),
  removeAdj(New, N, IG, Target),
  %% XXX the following lines seems to make double nodes in
  %% some adj_lists, which is a bug, apart from that they
  %% don't seem to make any difference at all (even though
  %% they are in the pseudocode of "optimistic coalescing")
  %% addedge for all in the restored adj_list
  %%RestoredAdj = hipe_ig:node_adj_list(N, IG),
  %%?debug_msg("adj_lists_before_restore_o ~n~p~n", [hipe_ig:adj_list(IG)]),
  %%restoreAdj(RestoredAdj, N, IG, Alias, Target).
  IG.

removeAdj([], _N, _IG, _Target) ->
  true;
removeAdj([V| New], N, IG, Target={TgtMod,TgtCtx}) ->
  hipe_ig:remove_edge(V, N, IG, TgtMod, TgtCtx),
  removeAdj(New, N, IG, Target).

%%restoreAdj([], _N, IG, _Alias, _Target) ->
%%  %%?debug_msg("adj_lists__after_restore_o ~n~p~n", [hipe_ig:adj_list(IG)]),
%%  IG;
%%restoreAdj([V| AdjToN], N, IG, Alias, Target={TgtMod,TgtCtx}) ->
%%  AliasToV = getAlias(V, Alias),
%%  IG1 = hipe_ig:add_edge(N, AliasToV, IG, TgtMod, TgtCtx),
%%  restoreAdj(AdjToN, N, IG1, Alias, Target).

%% XXX This is probably a clumsy way of doing it
%% better to assure the lists are sorted from the beginning
%% also coalesce findNew and removeAdj should improve performance
findNew(Adj, Saved) ->
  findNew(Adj, Saved, []).

findNew([], _Saved, New) ->
  New;
findNew([A| Adj], Saved, New) ->
  case lists:member(A, Saved) of
    true -> findNew(Adj, Saved, New);
    _ -> findNew(Adj, Saved, [A| New])
  end.

%test_fixAdj(0, _SavedAdj, IG, _Target) ->
%  IG;
%test_fixAdj(Node, SavedAdj, IG, Target) ->
%  NextNode = Node - 1,
%  IG1 = fixAdj(NextNode, SavedAdj, IG, Target),
%  test_fixAdj(NextNode, SavedAdj, IG1, Target).
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
%%   Target        -- The module containing the target-specific functions, along
%%                    with its context data.
%%   
%% Returns:
%%   true iff coalescing is OK
%%----------------------------------------------------------------------

ok(T, R, IG, K, Target) ->
  ((hipe_ig:is_trivially_colourable(T, K, IG))
   orelse is_precoloured(T, Target)
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
%%   Target        -- The module containing the target-specific functions, along
%%                    with its context data.
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
%% Description: Counts degrees for conservative (Briggs' heuristics)
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
	  if Cnt1 < K -> conservative_countU(AdjU, AdjV, U, Worklists, IG, K, Cnt1);
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
	      if Cnt1 < K -> conservative_countV(AdjV, U, Worklists, IG, K, Cnt1);
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
%%   IG             -- The interference graph
%%   K              -- The number of available registers
%%   Alias          -- The alias mapping
%%   SpillLimit     -- Try not to spill any nodes above the spill limit
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%---------------------------------------------------------------------

selectSpill(WorkLists, IG, SpillLimit) ->
  [CAR|CDR] = hipe_reg_worklists:spill(WorkLists),
  SpillCost = getCost(CAR, IG, SpillLimit),
  M = findCheapest(CDR, IG, SpillCost, CAR, SpillLimit),
  WorkLists1 = hipe_reg_worklists:remove_spill(M, WorkLists),
  hipe_reg_worklists:add_simplify(M, WorkLists1).

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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
selectSpill_O(WorkLists, Moves, IG, K, Alias, SpillLimit) ->
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
-endif.

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
  case Node >= SpillLimit of
    true -> inf;
    false ->
      SpillCost = hipe_ig:node_spill_cost(Node, IG),
      ?debug_msg("Actual spillcost f node ~w is ~w~n", [Node, SpillCost]),
      SpillCost
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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
freeze(K, WorkLists, Moves, IG, Alias) ->
  [U|_] = hipe_reg_worklists:freeze(WorkLists),         % Smarter routine?
  ?debug_msg("freezing node ~p~n", [U]),
  WorkLists0 = hipe_reg_worklists:remove_freeze(U, WorkLists),
  %% The published algorithm adds U to the simplify worklist
  %% before the freezeMoves() call. That breaks the worklist
  %% invariants, which is why the order is switched here.
  {WorkLists1, Moves1} = freezeMoves(U, K, WorkLists0, Moves, IG, Alias),
  WorkLists2 = hipe_reg_worklists:add_simplify(U, WorkLists1),
  {WorkLists2, Moves1}.
-endif.

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

-ifdef(COMPARE_ITERATED_OPTIMISTIC).
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
  %% already simplified appeared in coalesce_O(), were re-added to
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
freezeEm(U, [M|Ms], K, WorkLists, Moves, IG, Alias) ->
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

freezeEm3(_U,V,_M,K,WorkLists,Moves,IG,_Alias) ->
  Moves1 = Moves, % drop frozen move M
  V1 = V, % getAlias(V,Alias),
  %% "not MoveRelated(v)" is cheaper than "NodeMoves(v) = {}"
  case ((not hipe_moves:move_related(V1,Moves1)) andalso
	hipe_ig:is_trivially_colourable(V1,K,IG)) of
    true ->
      ?debug_msg("freezing move to ~p~n", [V]),
      Worklists1 = hipe_reg_worklists:transfer_freeze_simplify(V1, WorkLists),
      {Worklists1,Moves1};
    false ->
      {WorkLists,Moves1}
  end.
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to external functions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_precoloured({TgtMod,TgtCtx}) ->
  TgtMod:all_precoloured(TgtCtx).

allocatable({TgtMod,TgtCtx}) ->
  TgtMod:allocatable(TgtCtx).

is_precoloured(R, {TgtMod,TgtCtx}) ->
  TgtMod:is_precoloured(R,TgtCtx).

number_of_temporaries(CFG, {TgtMod,TgtCtx}) ->
  TgtMod:number_of_temporaries(CFG, TgtCtx).

physical_name(R, {TgtMod,TgtCtx}) ->
  TgtMod:physical_name(R,TgtCtx).
