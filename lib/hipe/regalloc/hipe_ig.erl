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
%%----------------------------------------------------------------------
%% File    : hipe_ig.erl
%% Author  : Andreas Wallin <d96awa@csd.uu.se>
%% Purpose : Creates an interference graph that tells which temporaries
%%           interfere with each other.
%% Created : 5 Feb 2000
%%----------------------------------------------------------------------

-module(hipe_ig).

-export([build/4,
	 nodes_are_adjacent/3,
	 node_spill_cost/2,
	 node_adj_list/2,
	 get_moves/1,
	 %% degree/1,
	 %% number_of_temps/1,
	 spill_costs/1,
	 adj_list/1,
	 %% adj_set/1,
	 add_edge/5,
	 remove_edge/5,
	 %% set_adj_set/2,
	 %% set_adj_list/2,
	 %% set_ig_moves/2,
	 %% set_spill_costs/2,
	 %% set_degree/2
	 get_node_degree/2,
	 dec_node_degree/2,
	 is_trivially_colourable/3
	]).
-ifdef(DEBUG_PRINTOUTS).
-export([print_spill_costs/1,
	 print_adjacent/1,
	 print_degrees/1
	]).
-endif.

%%-ifndef(DEBUG).
%%-define(DEBUG,true).
%%-endif.

-include("../main/hipe.hrl").
-include("../flow/cfg.hrl").
-include("hipe_spillcost.hrl").

-type target_context() :: any().
-type target() :: {TargetMod :: module(), TargetContext :: target_context()}.

%%----------------------------------------------------------------------

-record(igraph, {adj_set, adj_list, ig_moves, degree,
		 spill_costs :: #spill_cost{},
		 num_temps   :: non_neg_integer()}).

%%----------------------------------------------------------------------
%% Degree: array mapping nodes to integer degrees.
%% Precoloured nodes have 'infinite' degrees: they are initialised with
%% degrees K + number_of_temporaries.
%% Operations include incrementing, decrementing, and querying a node's
%% degree, and testing for trivial colourability (degree < K).
%%----------------------------------------------------------------------

degree_new(No_temporaries, {TargetMod, TargetCtx}) ->
  Degree = hipe_bifs:array(No_temporaries, 0),
  K = length(TargetMod:allocatable(TargetCtx)),
  Inf = K + No_temporaries,
  precoloured_to_inf_degree(TargetMod:all_precoloured(TargetCtx), Inf, Degree).

precoloured_to_inf_degree([], _Inf, Degree) -> Degree;
precoloured_to_inf_degree([P|Ps], Inf, Degree) ->
  hipe_bifs:array_update(Degree, P, Inf),
  precoloured_to_inf_degree(Ps, Inf, Degree).

degree_inc(Node, Degree) ->
  hipe_bifs:array_update(Degree, Node, hipe_bifs:array_sub(Degree, Node) + 1).

degree_dec(Node, Degree) ->
  hipe_bifs:array_update(Degree, Node, hipe_bifs:array_sub(Degree, Node) - 1).

degree_get(Node, Degree) ->
  hipe_bifs:array_sub(Degree, Node).

degree_is_trivially_colourable(Node, K, Degree) ->
  hipe_bifs:array_sub(Degree, Node) < K.

%%----------------------------------------------------------------------
%% AdjSet:
%% Implements sets of adjacent nodes.
%% Symmetry implies that when (U,V) is a member, then so is (V,U).
%% Hence, only (U,V), where U<V, is actually stored.
%% Supports queries and destructive updates, but not enumeration.
%% Implemented as a bit array in an array of bytes, augmented by an
%% index vector for fast address calculations.
%%----------------------------------------------------------------------

-define(USE_NEW_BITARRAY_BIFS, true).
%%-define(EMULATE_BITARRAY_BIFS, true).

-ifdef(USE_NEW_BITARRAY_BIFS).
-define(HIPE_BIFS_BITARRAY(ArrayBits, Val), hipe_bifs:bitarray(ArrayBits, Val)).
-define(HIPE_BIFS_BITARRAY_UPDATE(Array, BitNr, Val), hipe_bifs:bitarray_update(Array, BitNr, Val)).
-define(HIPE_BIFS_BITARRAY_SUB(Array, BitNr), hipe_bifs:bitarray_sub(Array, BitNr)).
-endif.

-ifdef(EMULATE_BITARRAY_BIFS).

-define(LOG2_BITS_PER_WORD, 3).
-define(BITS_PER_WORD, (1 bsl ?LOG2_BITS_PER_WORD)).

hipe_bifs_bitarray(ArrayBits, Val) ->
  ArrayWords = (ArrayBits + (?BITS_PER_WORD - 1)) bsr ?LOG2_BITS_PER_WORD,
  Byte =
    case Val of
      true -> 16#FF;
      false -> 16#00
    end,
  hipe_bifs:bytearray(ArrayWords, Byte).

hipe_bifs_bitarray_update(Array, BitNr, Val) ->
  WordNr = BitNr bsr ?LOG2_BITS_PER_WORD,
  WordMask = 1 bsl (BitNr band (?BITS_PER_WORD - 1)),
  Word = hipe_bifs:bytearray_sub(Array, WordNr),
  NewWord =
    case Val of
      true -> Word bor WordMask;
      false -> Word band (bnot WordMask)
    end,
  hipe_bifs:bytearray_update(Array, WordNr, NewWord).

hipe_bifs_bitarray_sub(Array, BitNr) ->
  WordNr = BitNr bsr ?LOG2_BITS_PER_WORD,
  WordMask = 1 bsl (BitNr band (?BITS_PER_WORD - 1)),
  Word = hipe_bifs:bytearray_sub(Array, WordNr),
  Word band WordMask =/= 0.

-define(HIPE_BIFS_BITARRAY(ArrayBits, Val), hipe_bifs_bitarray(ArrayBits, Val)).
-define(HIPE_BIFS_BITARRAY_UPDATE(Array, BitNr, Val), hipe_bifs_bitarray_update(Array, BitNr, Val)).
-define(HIPE_BIFS_BITARRAY_SUB(Array, BitNr), hipe_bifs_bitarray_sub(Array, BitNr)).

-endif. % EMULATE_BITARRAY_BIFS

-record(adjset, {index, array}).
-record(adjset_chunked, {index, chunks}).

-spec adjset_new(non_neg_integer()) -> #adjset{} | #adjset_chunked{}.

adjset_new(NrTemps) ->
  ArrayBits = (NrTemps * (NrTemps - 1)) div 2,
  Index = adjset_mk_index(NrTemps, []),
  try ?HIPE_BIFS_BITARRAY(ArrayBits, false) of
    Array ->
      #adjset{index=Index,array=Array}
  catch
    _:_ ->
      #adjset_chunked{index=Index,chunks=adjset_mk_chunks(ArrayBits)}
  end.

-define(LOG2_CHUNK_BITS, 19).	% 2^19 bits == 64KB
-define(CHUNK_BITS, (1 bsl ?LOG2_CHUNK_BITS)).

adjset_mk_chunks(ArrayBits) ->
  Tail =
    case ArrayBits band (?CHUNK_BITS - 1) of
      0 -> [];
      LastChunkBits -> [?HIPE_BIFS_BITARRAY(LastChunkBits, false)]
    end,
  N = ArrayBits bsr ?LOG2_CHUNK_BITS,
  adjset_mk_chunks(N, Tail).

adjset_mk_chunks(0, Tail) ->
  list_to_tuple(Tail);
adjset_mk_chunks(N, Tail) ->
  adjset_mk_chunks(N-1, [?HIPE_BIFS_BITARRAY(?CHUNK_BITS, false) | Tail]).

adjset_mk_index(0, Tail) ->
  list_to_tuple(Tail);
adjset_mk_index(N, Tail) ->
  I = N - 1,
  adjset_mk_index(I, [(I * (I-1)) div 2 | Tail]).

adjset_add_edge(U0, V0, #adjset{index=Index,array=Array}) -> % PRE: U0 =/= V0
  {U,V} =
    if U0 < V0 -> {U0,V0};
       true -> {V0,U0}
    end,
  %% INV: U < V
  BitNr = element(V+1, Index) + U,
  ?HIPE_BIFS_BITARRAY_UPDATE(Array, BitNr, true);
adjset_add_edge(U0, V0, #adjset_chunked{index=Index,chunks=Chunks}) -> % PRE: U0 =/= V0
  {U,V} =
    if U0 < V0 -> {U0,V0};
       true -> {V0,U0}
    end,
  %% INV: U < V
  BitNr = element(V+1, Index) + U,
  %% here things become different
  ChunkNr = BitNr bsr ?LOG2_CHUNK_BITS,
  ChunkBit = BitNr band (?CHUNK_BITS - 1),
  Chunk = element(ChunkNr+1, Chunks),
  ?HIPE_BIFS_BITARRAY_UPDATE(Chunk, ChunkBit, true).

adjset_remove_edge(U0, V0, #adjset{index=Index,array=Array}) -> % PRE: U0 =/= V0
  {U,V} =
    if U0 < V0 -> {U0,V0};
       true -> {V0,U0}
    end,
  %% INV: U < V
  BitNr = element(V+1, Index) + U,
  ?HIPE_BIFS_BITARRAY_UPDATE(Array, BitNr, false);
adjset_remove_edge(U0, V0, #adjset_chunked{index=Index,chunks=Chunks}) -> % PRE: U0 =/= V0
  {U,V} =
    if U0 < V0 -> {U0,V0};
       true -> {V0,U0}
    end,
  %% INV: U < V
  BitNr = element(V+1, Index) + U,
  %% here things become different
  ChunkNr = BitNr bsr ?LOG2_CHUNK_BITS,
  ChunkBit = BitNr band (?CHUNK_BITS - 1),
  Chunk = element(ChunkNr+1, Chunks),
  ?HIPE_BIFS_BITARRAY_UPDATE(Chunk, ChunkBit, false).

adjset_are_adjacent(U0, V0, #adjset{index=Index,array=Array}) ->
  {U,V} =
    if U0 < V0 -> {U0,V0};
       U0 =:= V0 -> exit({?MODULE,adjacent,U0,V0}); % XXX: probably impossible
       true -> {V0,U0}
    end,
  %% INV: U < V
  BitNr = element(V+1, Index) + U,
  ?HIPE_BIFS_BITARRAY_SUB(Array, BitNr);
adjset_are_adjacent(U0, V0, #adjset_chunked{index=Index,chunks=Chunks}) ->
  {U,V} =
    if U0 < V0 -> {U0,V0};
       U0 =:= V0 -> exit({?MODULE,adjacent,U0,V0}); % XXX: probably impossible
       true -> {V0,U0}
    end,
  %% INV: U < V
  BitNr = element(V+1, Index) + U,
  %% here things become different
  ChunkNr = BitNr bsr ?LOG2_CHUNK_BITS,
  ChunkBit = BitNr band (?CHUNK_BITS - 1),
  Chunk = element(ChunkNr+1, Chunks),
  ?HIPE_BIFS_BITARRAY_SUB(Chunk, ChunkBit).

%%---------------------------------------------------------------------
%% Print functions - only used for debugging

-ifdef(DEBUG_PRINTOUTS).
print_adjacent(IG) ->
  ?debug_msg("Adjacent nodes:\n", []),
  adjset_print(number_of_temps(IG),IG).

adjset_print(2, IG) ->
  adjset_print(1, 0, IG);
adjset_print(Ntemps, IG) ->
  adjset_print(Ntemps - 1, Ntemps - 2, IG),
  adjset_print(Ntemps - 1, IG).

adjset_print(U, 0, IG) ->
  case nodes_are_adjacent(U, 0, IG) of
    true -> ?debug_msg("edge ~w ~w\n", [U, 0]);
    _ -> true
  end;
adjset_print(U, V, IG) ->
  case nodes_are_adjacent(U, V, IG) of
    true -> ?debug_msg("edge ~w ~w\n", [U, V]);
    _ -> true
  end,
  adjset_print(U, V - 1, IG).
-endif.

%%----------------------------------------------------------------------
%% Function:    adj_set, adj_list, degree, spill_costs
%%
%% Description: Selector functions. Used to get one of the encapsulated 
%%              data-structure contained in the IG structure.
%% Parameters:
%%   IG     --  An interference graph
%%
%% Returns: 
%%   One of the encapsulated data-structures.
%%----------------------------------------------------------------------
adj_set(IG)     -> IG#igraph.adj_set.
adj_list(IG)    -> IG#igraph.adj_list.
ig_moves(IG)    -> IG#igraph.ig_moves.    
degree(IG)      -> IG#igraph.degree.

-spec spill_costs(#igraph{}) -> #spill_cost{}.
spill_costs(IG) -> IG#igraph.spill_costs.

-ifdef(DEBUG_PRINTOUTS).
number_of_temps(IG) -> IG#igraph.no_temps.
-endif.

%%----------------------------------------------------------------------
%% Function:    set_adj_set, set_adj_list, set_degree, set_spill_costs
%%
%% Description: Modifier functions. Used to set one of the encapsulated 
%%              data-structure contained in the IG structure.
%% Parameters:
%%   Data-structure --  Data-structure you want to set. An adj_set 
%%                       data-structure for example.
%%   IG             --  An interference graph
%%
%% Returns: 
%%   An updated interference graph.
%%----------------------------------------------------------------------

%%set_adj_set(Adj_set, IG)       -> IG#igraph{adj_set  = Adj_set}.
set_adj_list(Adj_list, IG)       -> IG#igraph{adj_list = Adj_list}.
set_ig_moves(IG_moves, IG)       -> IG#igraph{ig_moves = IG_moves}.
%%set_degree(Degree, IG)         -> IG#igraph{degree   = Degree}.
set_spill_costs(Spill_costs, IG) -> IG#igraph{spill_costs = Spill_costs}.

%%----------------------------------------------------------------------
%% Function:    initial_ig
%%
%% Description: The initial interference record that we start with when
%%              building the interference graph.
%% Parameters:
%%   NumTemps -- Number of temporaries in the CFG we work on. This is
%%               because we have some data structures built out of vectors.
%%
%% Returns: 
%%   A new interference record
%%----------------------------------------------------------------------

-spec initial_ig(non_neg_integer(), target()) -> #igraph{}.

initial_ig(NumTemps, Target) ->
  #igraph{adj_set     = adjset_new(NumTemps),
	  adj_list    = hipe_adj_list:new(NumTemps),
	  ig_moves    = hipe_ig_moves:new(NumTemps),
	  degree      = degree_new(NumTemps, Target),
	  spill_costs = hipe_spillcost:new(NumTemps),
	  num_temps   = NumTemps
	 }.

%%----------------------------------------------------------------------
%% Function:    build
%%
%% Description: Constructs an interference graph for the specifyed CFG.
%%
%% Parameters:
%%   CFG       -- A Control Flow Graph
%%   TargetMod -- The module that contains the target-specific functions
%%   TargetCtx -- Context data to pass to TargetMod
%%
%% Returns: 
%%   An interference graph for the given CFG.
%%----------------------------------------------------------------------

-spec build(#cfg{}, Liveness::_, module(), target_context()) -> #igraph{}.

build(CFG, BBs_in_out_liveness, TargetMod, TargetCtx) ->
  Target = {TargetMod, TargetCtx},
  Labels = TargetMod:labels(CFG, TargetCtx),
  %% How many temporaries exist?
  NumTemps = TargetMod:number_of_temporaries(CFG, TargetCtx),
  IG0 = initial_ig(NumTemps, Target),
  %%?debug_msg("initial adjset: ~p\n",[element(2, IG0)]),
  %%?debug_msg("initial adjset array: ~.16b\n",[element(3, element(2, IG0))]),
  analyze_bbs(Labels, BBs_in_out_liveness, IG0, CFG, Target).

%%----------------------------------------------------------------------
%% Function:    analyze_bbs
%%
%% Description: Looks up the code that exists in all basic blocks and
%%              analyse instructions use and def's to see what 
%%              temporaries that interfere with each other.
%%
%% Parameters:
%%   L                    --  A label
%%   Ls                   --  Other labels that exits in the CFG
%%   BBs_in_out_liveness  --  The in and out liveness on all basic blocks
%%   IG                   --  The interference graph in it's current state
%%   CFG                  --  The Control Flow Graph that we constructs 
%%                            the interference graph from.
%%   Target               --  The module containing the target-specific
%%                            functions, along with its context data
%%
%% Returns: 
%%   An interference graph for the given CFG.
%%----------------------------------------------------------------------

analyze_bbs([], _, IG, _, _) -> IG;
analyze_bbs([L|Ls], BBs_in_out_liveness, IG, CFG, Target) ->
    % Get basic block associated with label L
    BB = bb(CFG, L, Target),
    % Get basic block code
    BB_code = hipe_bb:code(BB),
    % Temporaries that are live out from this basic block, only numbers
    BB_liveout_numbers = liveout(BBs_in_out_liveness, L, Target),
    % {Liveness, New Interference Graph}
    {_, New_ig, Ref} = analyze_bb_instructions(BB_code,
					       ordsets:from_list(BB_liveout_numbers),
					       IG,
					       Target),
    Newer_ig = set_spill_costs(hipe_spillcost:ref_in_bb(Ref,
							spill_costs(New_ig)),
			      New_ig),
    analyze_bbs(Ls, BBs_in_out_liveness, Newer_ig, CFG, Target).

%%----------------------------------------------------------------------
%% Function:    analyze_bb_instructions
%%
%% Description: Analyzes all instructions that is contained in a basic
%%              block in reverse order. 
%%
%% Parameters:
%%   Instruction   --  An instruction
%%   Instructions  --  The remaining instructions
%%   Live          --  All temporaries that are live at the time.
%%                     Live is a set of temporary "numbers only".
%%   IG            --  The interference graph in it's current state
%%   Target        --  The mopdule containing the target-specific functions,
%%                     along with its context data.
%%
%% Returns: 
%%   Live  --  Temporaries that are live at entery of basic block
%%              that we analyze.
%%   IG    --  Updated interference graph.
%%   Ref   --  Set of temporaries referred to in this bb.
%%----------------------------------------------------------------------

%% Ref: set of temporaries referred to in this bb
analyze_bb_instructions([], Live, IG, _) -> {Live, IG, ordsets:new()};
analyze_bb_instructions([Instruction|Instructions], Live, IG, Target) ->
  %% Analyze last instruction first.
  {Live0, IG0, Ref} = analyze_bb_instructions(Instructions, Live, 
					      IG, Target),
  %% Check for temporaries that are defined and used in instruction
  {Def, Use} = def_use(Instruction, Target),
  %% Convert to register numbers
  Def_numbers = ordsets:from_list(reg_numbers(Def, Target)),
  Use_numbers = ordsets:from_list(reg_numbers(Use, Target)),
  Ref_numbers = ordsets:union(Ref, ordsets:union(Def_numbers, Use_numbers)),
  %% Increase spill cost on all used temporaries
  IG1 = set_spill_costs(hipe_spillcost:inc_costs(Use_numbers,
						 spill_costs(IG0)),
			IG0),
  {Live1, IG2} = analyze_move(Instruction, 
			      Live0, 
			      Def_numbers, 
			      Use_numbers, 
			      IG1, 
			      Target),
  %% Adding Def to Live here has the effect of creating edges between
  %% the defined registers, which is O(N^2) for an instruction that
  %% clobbers N registers.
  %%
  %% Adding Def to Live is redundant when:
  %% 1. Def is empty, or
  %% 2. Def is a singleton, or
  %% 3. Def contains only precoloured registers, or
  %% 4. Def contains exactly one non-precoloured register, and the
  %%    remaining ones are all non-allocatable precoloured registers.
  %%
  %% HiPE's backends only create multiple-element Def sets
  %% for CALL instructions, and then all elements are precoloured.
  %%
  %% Therefore we can avoid adding Def to Live. The benefit is greatest
  %% on backends with many physical registers, since CALLs clobber all
  %% physical registers.
  Live2 = Live1, % ordsets:union(Live1, Def_numbers),
  IG3 = interfere(Def_numbers, Live2, IG2, Target),
  Live3 = ordsets:union(Use_numbers, ordsets:subtract(Live2, Def_numbers)),
  {Live3, IG3, Ref_numbers}.

%%----------------------------------------------------------------------
%% Function:    analyze_move
%%
%% Description: If a move instructions is discovered, this function is
%%              called. It is used to remember what move instructions
%%              a temporary is associated with and all moves that exists
%%              in the CFG. 
%%
%% Parameters:
%%   Instruction  --  An instruction
%%   Live         --  All temporaries that are live at the time.
%%                    Live is a set of temporary "numbers only".
%%   Def_numbers  --  Temporaries that are defined at this instruction
%%   Use_numbers  --  Temporaries that are used at this instruction
%%   IG           --  The interference graph in its current state
%%   Target       --  The module containing the target-specific functions, along
%%                    with its context data
%% Returns:
%%   Live  --  An updated live set
%%   IG    --  An updated interference graph
%%----------------------------------------------------------------------

analyze_move(Instruction, Live, Def_numbers, Use_numbers, IG, Target) ->
  case is_move(Instruction,Target) of
    true ->
      case {Def_numbers, Use_numbers} of
	{[Dst], [Src]} ->
	  New_IG = set_ig_moves(hipe_ig_moves:new_move(Dst, Src, ig_moves(IG)), IG),
	  New_live = ordsets:del_element(Src, Live),
	  {New_live, New_IG};
	_ ->
	  {Live, IG}
      end;
    _ ->
      {Live, IG}
  end.

%%----------------------------------------------------------------------
%% Function:    interfere
%%
%% Description: A number of temporaries that are defined interfere with
%%              everything in the current live set.
%%
%% Parameters:
%%   Define     --  A Define temporary
%%   Defines    --  Rest of temporaries.
%%   Live       --  Current live set
%%   IG         --  An interference graph
%%
%% Returns: 
%%   An updated interference graph.
%%----------------------------------------------------------------------

interfere([], _, IG, _) -> IG;
interfere([Define|Defines], Living, IG, Target) ->
  New_ig = interfere_with_living(Define, Living, IG, Target),
  interfere(Defines, Living, New_ig, Target).

%%----------------------------------------------------------------------
%% Function:    interfere_with_living
%%
%% Description: Let one temporary that is in the define set interfere 
%%              with all live temporaries.
%%
%% Parameters:
%%   Define     --  A Define temporary
%%   Live       --  Current live set
%%   Lives      --  Rest of living temporaries.
%%   IG         --  An interference graph
%%   Target     --  The module containing the target-specific functions, along
%%                  with its context data.
%% Returns:
%%   An updated interference graph
%%----------------------------------------------------------------------

interfere_with_living(_, [], IG, _) -> IG;
interfere_with_living(Define, [Live|Living], IG, Target) ->
  New_ig = add_edge(Define, Live, IG, Target),
  interfere_with_living(Define, Living, New_ig, Target).

%%
%% nodes_are_adjacent(U, V, IG)
%% returns true if nodes U and V are adjacent in interference graph IG
%%
-spec nodes_are_adjacent(integer(), integer(), #igraph{}) -> boolean().
nodes_are_adjacent(U, V, IG) ->
  adjset_are_adjacent(U, V, adj_set(IG)).

%%
%% node_adj_set(Node, IG)
%% returns list of Node's adjacent nodes in interference graph IG
%%
node_adj_list(Node, IG) ->
  hipe_adj_list:edges(Node, adj_list(IG)).

%%
%% node_spill_cost(Node, IG)
%% returns the Node's spill cost
%%
node_spill_cost(Node, IG) ->
  hipe_spillcost:spill_cost(Node, spill_costs(IG)).

%%----------------------------------------------------------------------
%% Print functions - only used for debugging

-ifdef(DEBUG_PRINTOUTS).
print_spill_costs(IG) ->
  ?debug_msg("Spill costs:\n", []),
  print_spill_costs(number_of_temps(IG), IG).

print_spill_costs(0, _) ->
  true;
print_spill_costs(Node, IG) ->
  NextNode = Node - 1,
  case hipe_spillcost:nr_of_use(NextNode, spill_costs(IG)) of
    0 ->
      ?debug_msg("node ~w not used\n", [NextNode]);
    _ ->
      ?debug_msg("node ~w sc ~p\n", [NextNode, node_spill_cost(NextNode, IG)])
  end,
  print_spill_costs(NextNode, IG).
-endif.

%%----------------------------------------------------------------------

get_moves(IG) ->
  hipe_ig_moves:get_moves(ig_moves(IG)).

%%----------------------------------------------------------------------
%% Function:    add_edge
%%
%% Description: Adds an edge to the adj_set data structure if it is
%%              not already a part of it and if U is not precoloured
%%              we add V to its adj_list. If V is not precoloured
%%              we add U to its adj_list.
%%
%% Parameters:
%%   U          --  A temporary number
%%   V          --  A temporary number
%%   TargetMod  --  The module containing the target-specific functions.
%%   TargetCtx  --  Context data to pass to TargetMod
%% Returns: 
%%   An updated interference graph.
%%----------------------------------------------------------------------

add_edge(U, V, IG, TargetMod, TargetCtx) ->
  add_edge(U, V, IG, {TargetMod, TargetCtx}).

add_edge(U, U, IG, _) -> IG;
add_edge(U, V, IG, Target) ->
  case nodes_are_adjacent(U, V, IG) of
    true ->
      IG;
    false ->
      _ = adjset_add_edge(U, V, adj_set(IG)),
      Degree = degree(IG),
      AdjList0 = interfere_if_uncolored(U, V, adj_list(IG), Degree, Target),
      AdjList1 = interfere_if_uncolored(V, U, AdjList0, Degree, Target),
      set_adj_list(AdjList1, IG)
  end.

%%----------------------------------------------------------------------
%% Function:    remove_edge
%%
%% Description: Removes an edge to the adj_set data-structure if it's
%%              a part of it and if U is not precoloured
%%              we remove V from it's adj_list. If V is not precoloured
%%              we remove U from it's adj_list.
%%
%% Parameters:
%%   U          --  A temporary number
%%   V          --  A temporary number
%%   TargetMod  --  The module containing the target-specific functions.
%%   TargetCtx  --  Context data for TargetMod.
%% Returns: 
%%   An updated interference graph.
%%----------------------------------------------------------------------

remove_edge(U, V, IG, TargetMod, TargetCtx) ->
  remove_edge(U, V, IG, {TargetMod, TargetCtx}).

remove_edge(U, U, IG, _) -> IG;
remove_edge(U, V, IG, Target) ->
  case nodes_are_adjacent(U, V, IG) of
    false ->
      IG;
    true ->
      _ = adjset_remove_edge(U, V, adj_set(IG)),
      Degree = degree(IG),
      AdjList0 = remove_if_uncolored(U, V, adj_list(IG), Degree, Target),
      AdjList1 = remove_if_uncolored(V, U, AdjList0, Degree, Target),
      set_adj_list(AdjList1, IG)
  end.

%%----------------------------------------------------------------------
%% Function:    remove_if_uncolored
%%
%% Description:
%%
%% Parameters:
%%   Temporary            --  A temporary that is added to the adjacent 
%%                             list if it's not precoloured.
%%   Interfere_temporary  --  Temporary will interfere with 
%%                             Interfere_temporary if temporary is not
%%                             precoloured.
%%   Adj_list             --  An adj_list
%%   Degree               --  The degree that all nodes currently have
%%   Target               --  The module containing the target-specific
%%                            functions, along with its context data.
%%
%% Returns: 
%%   Adj_list  --  An updated adj_list data structure
%%   Degree    --  An updated degree data structure (via side-effects)
%%----------------------------------------------------------------------

remove_if_uncolored(Temp, InterfereTemp, Adj_list, Degree, Target) ->
  case is_precoloured(Temp,Target) of
    false ->
      New_adj_list = hipe_adj_list:remove_edge(Temp, InterfereTemp, Adj_list),
      degree_dec(Temp, Degree),
      New_adj_list;
    true ->
      Adj_list
  end.

%%----------------------------------------------------------------------
%% Function:    interfere_if_uncolored
%%
%% Description: Let a not precoloured temporary interfere with another.
%%
%% Parameters:
%%   Temporary            --  A temporary that is added to the adjacent 
%%                             list if it's not precoloured.
%%   Interfere_temporary  --  Temporary will interfere with 
%%                             Interfere_temporary if temporary is not
%%                             precoloured.
%%   Adj_list             --  An adj_list
%%   Degree               --  The degree that all nodes currently have
%%   Target               --  The module containing the target-specific
%%                            functions, along with its context data.
%%
%% Returns: 
%%   Adj_list  --  An updated adj_list data structure
%%   Degree    --  An updated degree data structure (via side-effects)
%%----------------------------------------------------------------------

interfere_if_uncolored(Temp, InterfereTemp, Adj_list, Degree, Target) ->
  case is_precoloured(Temp, Target) of
    false ->
      New_adj_list = hipe_adj_list:add_edge(Temp, InterfereTemp, Adj_list),
      degree_inc(Temp, Degree),
      New_adj_list;
    true ->
      Adj_list
  end.

%%----------------------------------------------------------------------
%% Function:    reg_numbers
%%
%% Description: Converts a list of tuple with {something, reg_number}
%%              to a list of register numbers.
%%
%% Parameters:
%%   TRs     -- A list of temporary registers
%%   Target  -- The module containing the target-specific functions, along with
%%              its context data.
%% Returns: 
%%   A list of register numbers.
%%----------------------------------------------------------------------

reg_numbers(Regs, {TgtMod, TgtCtx}) ->
  [TgtMod:reg_nr(X,TgtCtx) || X <- Regs].

%%---------------------------------------------------------------------
%% Print functions - only used for debugging

-ifdef(DEBUG_PRINTOUTS).
print_degrees(IG) ->
  ?debug_msg("The nodes degrees:\n", []),
  print_node_degree(number_of_temps(IG), IG).

print_node_degree(0, _) ->
  true;
print_node_degree(Node, IG) ->
  NextNode = Node - 1,
  ?debug_msg("node ~w ~w\n", [NextNode, get_node_degree(NextNode, IG)]),
  print_node_degree(NextNode, IG).
-endif.

%%----------------------------------------------------------------------

get_node_degree(Node, IG) ->
  degree_get(Node, degree(IG)).

dec_node_degree(Node, IG) ->
  degree_dec(Node, degree(IG)),
  IG.

is_trivially_colourable(Node, K, IG) ->
  degree_is_trivially_colourable(Node, K, degree(IG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to external functions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bb(CFG, L, {TgtMod,TgtCtx}) ->
  TgtMod:bb(CFG,L,TgtCtx).

def_use(Instruction, {TgtMod,TgtCtx}) ->
  TgtMod:def_use(Instruction, TgtCtx).

is_move(Instruction, {TgtMod,TgtCtx}) ->
  TgtMod:is_move(Instruction, TgtCtx).

is_precoloured(R, {TgtMod,TgtCtx}) ->
  TgtMod:is_precoloured(R,TgtCtx).

liveout(Liveness,L, Target={TgtMod,TgtCtx}) ->
  reg_numbers(TgtMod:liveout(Liveness,L,TgtCtx), Target).
