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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%@doc
%%		  GRAPH COLORING REGISTER ALLOCATOR
%%
%% A simple graph coloring register allocator:
%%
%% - build interference graph + estimate spill costs
%% - simplify graph (push on stack + spill)
%% - select colors
%%
%% Emits a coloring: a list of {TempName,Location}
%%  where Location is {reg,N} or {spill,M}
%%   and {reg,N} denotes some register N
%%   and {spill,M} denotes the Mth spilled node
%% You have to figure out how to rewrite the code yourself.
%%
%% This version uses vectors rather than hash tables, and uses
%% faster algorithms since all vars are known at the start.
%% The result should be considerably quicker than earlier versions.
%%
%% Deficiencies:
%% - no renaming                 (to reduce unnecessary register pressure)
%% - spill costs are naive       (should use better; e.g., exec.estimates)
%% - no biased coloring          (which coalesces moves)
%% - no live range splitting     (possibly not critical)
%%
%% *** NOTE ***
%% Uses apply for target specific functions, takes the module name as
%% argument. This target specific module should implement all target 
%% specific functions, see the end of the file.
%% 

-module(hipe_graph_coloring_regalloc).
-export([regalloc/7]).

%%-ifndef(DO_ASSERT).
%%-define(DO_ASSERT, true).
%%-endif.

%%-ifndef(DEBUG).
%%-define(DEBUG,0).
%%-endif.
-include("../main/hipe.hrl").

%% Define these as 'ok' or 'report(X,Y)' depending on how much output you want.
-define(report0(X,Y), ?IF_DEBUG_LEVEL(0,?msg(X, Y),ok)).
-define(report(X,Y),  ?IF_DEBUG_LEVEL(1,?msg(X, Y),ok)). 
-define(report2(X,Y), ?IF_DEBUG_LEVEL(2,?msg(X, Y),ok)). 
-define(report3(X,Y), ?IF_DEBUG_LEVEL(3,?msg(X, Y),ok)).

%% Given CFG and number of colors K, produce a coloring list
%% of items {reg,N} (0 =< N =< K) and {spill,M}, where M is
%% an index denoting 'a location'.
%% (You might use it as a stack index, perhaps.)
%%
%% You can in principle delete check_coloring/2; it merely checks
%% that the coloring agrees with the interference graph (that is, that
%% no neighbors have the same register or spill location).

%% @spec regalloc(#cfg{}, liveness(), non_neg_fixnum(), non_neg_fixnum(),
%%                module(), tgt_ctx(), list()) -> {, non_neg_fixnum()}

regalloc(CFG, Live, SpillIndex, SpillLimit, TargetMod, TargetContext,
	 _Options) ->
  Target = {TargetMod, TargetContext},
  PhysRegs = allocatable(Target),
  ?report2("building IG~n", []),
  {IG, Spill} = build_ig(CFG, Live, Target),

  %% check_ig(IG),
  ?report3("graph: ~p~nphysical regs: ~p~n", [list_ig(IG), PhysRegs]),

  %% These nodes *can't* be allocated to registers. 
  NotAllocatable = non_alloc(CFG, Target),
  %% i.e. Arguments on x86
  ?report2("Nonalloc ~w~n", [NotAllocatable]),

  {Cols, NewSpillIndex} = 
    color(IG, Spill,
	  ordsets:from_list(PhysRegs), 
	  SpillIndex,
	  SpillLimit,
	  number_of_temporaries(CFG, Target),
	  Target, NotAllocatable),
  Coloring = [{X, {reg, X}} || X <- NotAllocatable] ++ Cols,
  ?ASSERT(check_coloring(Coloring, IG, Target)),

  {Coloring, NewSpillIndex}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** BUILD THE INTERFERENCE GRAPH ***
%%
%% Returns {Interference_graph, Spill_cost_dictionary}
%%

build_ig(CFG, Live, Target) ->
  NumN = number_of_temporaries(CFG, Target),  % poss. N-1?
  {IG, Spill} = build_ig_bbs(labels(CFG, Target),
			     CFG, 
			     Live,
			     empty_ig(NumN), 
			     empty_spill(NumN),
			     Target),
  {normalize_ig(IG), Spill}.

build_ig_bbs([], _CFG, _Live, IG, Spill, _Target) ->
  {IG, Spill};
build_ig_bbs([L|Ls], CFG, Live, IG, Spill, Target) ->
  Xs = bb(CFG, L, Target),
  {_, NewIG, NewSpill} =
    build_ig_bb(Xs, liveout(Live, L, Target), IG, Spill, Target),
  build_ig_bbs(Ls, CFG, Live, NewIG, NewSpill, Target).

build_ig_bb([], LiveOut, IG, Spill, _Target) ->
  {LiveOut, IG, Spill};
build_ig_bb([X|Xs], LiveOut, IG, Spill, Target) ->
  {Live,NewIG,NewSpill} = build_ig_bb(Xs, LiveOut, IG, Spill, Target),
  build_ig_instr(X, Live, NewIG, NewSpill, Target).

%% Note: We could add move-related arcs here as well.
%%
%% Note: Ideally, we would like to add all registers to the IG
%% at once rather than doing 'add_nodes' for each instruction.
%% (This is costly, since nodes that already are present are checked!)

build_ig_instr(X, Live, IG, Spill, Target) ->
  {Def, Use} = def_use(X, Target),
  ?report3("Live ~w\n~w : Def: ~w Use ~w\n", [Live, X, Def,Use]),
  DefList = ordsets:to_list(Def),
  NewSpill = inc_spill_costs(DefList, 
			     inc_spill_costs(ordsets:to_list(Use), Spill)),
  NewIG = interference_arcs(DefList, ordsets:to_list(Live), IG),
  NewLive = ordsets:union(Use, ordsets:subtract(Live, Def)),
  {NewLive, NewIG, NewSpill}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interference_arcs([], _Live, IG) -> 
  IG;
interference_arcs([X|Xs], Live, IG) ->
  interference_arcs(Xs, Live, i_arcs(X, Live, IG)).

i_arcs(_X, [], IG) -> 
  IG;
i_arcs(X, [Y|Ys], IG) ->
  i_arcs(X, Ys, add_edge(X,Y, IG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inc_spill_costs([], Spill) -> Spill;
inc_spill_costs([X|Xs], Spill) ->
  inc_spill_costs(Xs, inc_spill_cost(X, Spill)).

inc_spill_cost(X, Spill) ->
  set_spill_cost(X, get_spill_cost(X, Spill)+1, Spill).

get_spill_cost(X, Spill) ->
  spill_cost_lookup(X, Spill).

set_spill_cost(X, N, Spill) ->
  spill_cost_update(X, N, Spill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** COLORING ***
%%
%% Coloring is done straightforwardly:
%% - find the low-degree nodes, put them in low
%% - while low non-empty:
%%   * remove x from low
%%   * push x on stack
%%   * decrement degree of neighbors of x
%%   * for each neighbor y of low degree, put y on low
%% - when low empty:
%%   - if graph empty, return stack
%%   - otherwise
%%     * select a node z to spill
%%     * push z on stack
%%     * decrement degree of neighbors of z
%%     * add low-degree neighbors of z to low
%%     * restart the while-loop above

color(IG, Spill, PhysRegs, SpillIx, SpillLimit, NumNodes, Target,
      NotAllocatable) ->
  ?report("simplification of IG~n", []),
  K = ordsets:size(PhysRegs),
  Nodes = list_ig(IG),

  Low = low_degree_nodes(Nodes, K, NotAllocatable),

  %% Any nodes above the spillimit must be colored first...
  MustNotSpill = 
    if NumNodes > SpillLimit ->
	sort_on_degree(lists:seq(SpillLimit,NumNodes-1) -- Low,IG);
       true -> []
    end,
      
  ?report(" starting with low degree nodes ~p~n",[Low]),
  EmptyStk = [],
  Precolored = all_precoloured(Target),
  {Stk, NewSpillIx} = 
    simplify(Low, NumNodes, Precolored,
	     IG, Spill, K, SpillIx, EmptyStk,
	     SpillLimit, Target, NotAllocatable, MustNotSpill),
  ?report("selecting colors~n",[]),
  {select(Stk, Precolored, IG, K, PhysRegs, NumNodes, Target),
   NewSpillIx}.

sort_on_degree(Nodes, IG) ->
  [ Node3 || {_,Node3} <- 
	       lists:sort([{degree(Info),Node2} || 
			    {Info,Node2} <- [{hipe_vectors:get(IG, Node),
					      Node} || Node <-
							 Nodes]])].

%%%%%%%%%%%%%%%%%%%%
%%
%% Simplification: push all easily colored nodes on a stack;
%%  when the list of easy nodes becomes empty, see if graph is
%%  empty as well. If it is not, spill a node and continue.
%%  If it is empty, return the stack.
%%
%% Notes:
%% - We keep the set of visited nodes around for spill purposes
%%   (visited nodes are not considered for spilling)
%%
%% - At present, nodes can be pushed onto the stack even if they
%%   already are on the stack. This can be fixed by another 'Vis'
%%   dictionary that keeps track of what is on the stack.
%%   Currently, we just skip already colored nodes.
%%
%% - Arguments:
%%   Low: low-degree nodes (ready to color)
%%   NumNodes: number of remaining nodes in graph
%%   IG: interference graph
%%   Spill: spill costs of nodes
%%   K: number of colors
%%   Ix: next spill index
%%   Stk: stack of already simplified nodes
%%
%% Physical registers are marked as 'visited' prior to simplify.
%% This has the following effect:
%% - they are not considered for spilling
%% - they are not pushed on the stack
%% - since we do NOT decrement degrees of surrounding vars, the
%%   non-physreg variables must still take them into account.

simplify(Low, NumNodes, PreC, IG, Spill, K, Ix, Stk, SpillLimit,
	 Target, NotAllocatable, MustNotSpill) ->
  Vis = visit_all(PreC, none_visited(NumNodes)),
  Vis1 = visit_all(NotAllocatable, Vis),
  ActualNumNodes = (NumNodes-length(PreC))-length(NotAllocatable),
  %% Make sure that the registers that must not be spilled
  %%  get a degree less than K by spilling other regs.
  {Stk2, Ix2, Vis2, Low2} =  
    handle_non_spill(MustNotSpill, IG, Spill, K, Ix, Stk, Vis1, Low,
		     SpillLimit, Target),
  simplify_ig(Low2, ActualNumNodes-length(Stk2), IG, Spill, K, Ix2, Stk2, Vis2,
	      SpillLimit, Target).

handle_non_spill([], _IG, _Spill, _K, Ix, Stk, Vis, Low, _SpillLimit, _Target) ->
  {Stk, Ix, Vis, Low};
handle_non_spill([X|Xs] = L, IG, Spill, K, Ix, Stk, Vis, Low, SpillLimit, Target) ->
  Info = hipe_vectors:get(IG, X),
  Degree = degree(Info),
  ?report("Can't Spill ~w with degree ~w\n", [X,Degree]),
  if Degree > K ->
      ?report("  *** spill required (N<~w)***~n", [SpillLimit]),
      {Y, NewLow, NewIG} = spill(IG, Vis, Spill, K, SpillLimit, Target),
      NewVis = visit(Y,Vis),
      {NewStk, NewIx} = push_spill_node(Y, Ix, Stk),
      ?report("  node ~w spilled~n", [Y]),
      handle_non_spill(L, NewIG, Spill, K, NewIx, NewStk, NewVis,
		       Low ++ NewLow, SpillLimit, Target);
     true ->
      {NewLow, NewIG} = decrement_neighbors(X, Low, IG, Vis, K),
      ?report("  node ~w pushed\n(~w now ready)~n", [X,NewLow]),
      NewStk = push_colored(X, Stk),
      handle_non_spill(Xs, NewIG, Spill, K, Ix, NewStk, visit(X,Vis),
		       NewLow, SpillLimit, Target)
  end.

simplify_ig([], 0, _IG, _Spill, _K, Ix, Stk, _Vis, _SpillLimit, _Target) ->
  {Stk, Ix};
simplify_ig([], N, IG, Spill, K, Ix, Stk, Vis, SpillLimit, Target) 
  when N > 0 ->
  ?report3("N: ~w Stk: ~w N+Stk ~w\n", [N,length(Stk),N+length(Stk)]),
  ?report("  *** spill required (N<~w)***~n", [SpillLimit]),
  {X, Low, NewIG} = spill(IG, Vis, Spill, K, SpillLimit, Target),
  NewVis = visit(X,Vis),
  {NewStk, NewIx} = push_spill_node(X, Ix, Stk),
  ?report("  node ~w spilled\n(~w now ready)~n", [X, Low]),
  simplify_ig(Low, N-1, NewIG, Spill, K, NewIx, NewStk, NewVis,
	      SpillLimit, Target);
simplify_ig([X|Xs], N, IG, Spill, K, Ix, Stk, Vis, SpillLimit, Target) ->
  ?report3("N: ~w Stk: ~w N+Stk ~w\n", [N,length(Stk),N+length(Stk)]),
  case is_visited(X,Vis) of
    true ->
      ?report("  node ~p already visited~n",[X]),
      simplify_ig(Xs, N, IG, Spill, K, Ix, Stk, Vis, SpillLimit, Target);
    false ->
      ?report("Stack ~w\n", [Stk]),
      {NewLow, NewIG} = decrement_neighbors(X, Xs, IG, Vis, K),
      ?report("  node ~w pushed\n(~w now ready)~n", [X,NewLow]),
      NewStk = push_colored(X, Stk),
      simplify_ig(NewLow, N-1, NewIG, Spill, K, Ix, NewStk, visit(X,Vis),
		  SpillLimit, Target)
  end.

%% Returns { NowLowDegreeNeighbors, NewIG }

decrement_neighbors(X, Xs, IG, Vis, K) ->
  Ns = unvisited_neighbors(X, Vis, IG),
  ?report("  node ~p has neighbors ~w\n(unvisited ~p)~n",
	  [X, neighbors(X, IG), Ns]),
  decrement_each(Ns, Xs, IG, Vis, K).

%% For each node, decrement its degree and check if it is now
%% a low-degree node. In that case, add it to the 'low list'.

decrement_each([], Low, IG, _Vis, _K) -> 
  {Low, IG};
decrement_each([N|Ns], OldLow, IG, Vis, K) ->
  {Low, CurrIG} = Res = decrement_each(Ns, OldLow, IG, Vis, K),
  case is_visited(N, Vis) of
    true ->
      Res;
    false ->
      {D, NewIG} = decrement_degree(N, CurrIG),
      if
	D =:= K-1 ->
	  {[N|Low], NewIG};
	true ->
	  {Low, NewIG}
      end
  end.

%%%%%%%%%%%%%%%%%%%%
%%
%% The spill cost of a node is:
%%    est_spill_cost / current_degree
%%
%% For all unvisited nodes, compute spill cost and select the minimum.
%% This node is chosen to be spilled. Then decrement the degree of its
%% neighbors, and return those of low degree.
%%
%% Notes:
%% - A better method for computing spill costs is to just keep the
%%   minimum cost node. But for debugging purposes, we compute a list
%%   of {node,spillcost} pairs and select the minimum.
%%
%% Returns:
%%  {Spilled_node, Low_degree_neighbors, New_interference_graph}

spill(IG, Vis, Spill, K, SpillLimit, Target) ->
  Ns = list_ig(IG),
  Costs = spill_costs(Ns, IG, Vis, Spill, SpillLimit, Target),
  ?report3("spill costs are ~p~n", [Costs]),
  ActualCosts = lists:sort(Costs),
  ?report3("actual costs are ~p~n", [ActualCosts]),
  case ActualCosts of
    [] ->
      ?error_msg("There is no node to spill", []),
      ?EXIT('no node to spill');
    [{_Cost,N}|_] ->
      {Low, NewIG} = decrement_neighbors(N, [], IG, Vis, K),
      %% ?report("spilled node ~p at cost ~p (~p now ready)~n", [N,Cost,Low]),
      {N, Low, NewIG}
  end.

spill_costs([], _IG, _Vis, _Spill, _SpillLimit, _Target) ->
  [];
spill_costs([{N,Info}|Ns], IG, Vis, Spill, SpillLimit, Target) ->
  case degree(Info) of
    0 -> spill_costs(Ns,IG,Vis,Spill, SpillLimit, Target);
    Deg ->
      case is_visited(N,Vis) of
	true ->
	  spill_costs(Ns,IG,Vis,Spill, SpillLimit, Target);
	_ ->
	  case is_fixed(N, Target) of
	    true ->
	      spill_costs(Ns, IG, Vis, Spill, SpillLimit, Target);
	    false ->
	      if N >= SpillLimit ->
		  spill_costs(Ns, IG, Vis, Spill, SpillLimit, Target);
		 true ->
		  [{spill_cost_of(N,Spill)/Deg,N} | 
		   spill_costs(Ns,IG, Vis, Spill, SpillLimit, Target)]
	      end
	  end
      end
  end.

%%%%%%%%%%%%%%%%%%%%
%%
%% Returns a list of {Name,Location}, where Location is
%%   either {spill,M} or {reg,R}
%%
%% Note: we use pessimistic coloring here.
%% - we could use optimistic coloring: for spilled node, check if there is
%%   an unused color among the neighbors and choose that.

select(Stk, PreC, IG, K, PhysRegs, NumNodes, Target) ->
  %% NumNodes = length(Stk)+length(PreC),
  {PhysColors, Cols} = precolor(PreC, none_colored(NumNodes), Target),
  ?report("precoloring has yielded ~p~n",[list_coloring(Cols)]),
  PhysColors ++ select_colors(Stk, IG, Cols, PhysRegs, K).

select_colors([], _IG, _Cols, _PhysRegs, _K) -> 
  ?report("all nodes colored~n",[]),
  [];
select_colors([{X,colorable}|Xs], IG, Cols, PhysRegs, K) ->
  ?report("color of ~p\n",[X]),
  {Reg,NewCols} = select_color(X, IG, Cols, PhysRegs),
  ?report("~p~n",[Reg]),
  [{X,{reg,Reg}} | select_colors(Xs, IG, NewCols, PhysRegs, K)];
%%select_colors([{X,{spill,M}}|Xs], IG, Cols, PhysRegs, K) ->
%%  ?report('spilled: ~p~n',[X]),
%%  %% Check if optimistic coloring could have found a color 
%%  case catch select_color(X,IG,Cols,K) of
%%    {'EXIT',_} ->   % no color possible
%%	?report('(no optimistic color)~n',[]),
%%	[{X,{spill,M}}|select_colors(Xs, IG, Cols, PhysRegs, K)];
%%    {Reg,NewCols} ->
%%	?report('(optimistic color: ~p)~n',[Reg]),
%%	[{X,{reg,Reg}}|select_colors(Xs, IG, Cols, PhysRegs, K)]
%%  end.

%% Old code / pessimistic coloring:
select_colors([{X,{spill,M}}|Xs], IG, Cols, PhysRegs, K) ->
  ?report("spilled: ~p~n",[X]),
  %% Check if optimistic coloring could have found a color
%%    case catch select_color(X,IG,Cols,K) of
%%	{'EXIT',_} ->   % no color possible
%%	    ?report('(no optimistic color)~n',[]);
%%	{Reg,NewCols} ->
%%	    ?report('(optimistic color: ~p)~n',[Reg])
%%    end,
  [{X,{spill,M}} | select_colors(Xs, IG, Cols, PhysRegs, K)].

select_color(X, IG, Cols, PhysRegs) ->
  UsedColors = get_colors(neighbors(X, IG), Cols),
  Reg = select_unused_color(UsedColors, PhysRegs),
  {Reg, set_color(X, Reg, Cols)}.

%%%%%%%%%%%%%%%%%%%%

get_colors([], _Cols) -> [];
get_colors([X|Xs], Cols) ->
  case color_of(X, Cols) of
    uncolored ->
      get_colors(Xs, Cols);
    {color,R} ->
      [R|get_colors(Xs, Cols)]
  end.

select_unused_color(UsedColors, PhysRegs) ->
  Summary = ordsets:from_list(UsedColors),
  AvailRegs = ordsets:to_list(ordsets:subtract(PhysRegs, Summary)),
  hd(AvailRegs).
  %% select_avail_reg(AvailRegs).

%% We choose the register to use randomly from the set of available
%% registers. 
%%
%% Note: Another way of doing it is LRU-order:
%% - Have an LRU-queue of register names; when coloring, try the colors in that
%%   order (some may be occupied).
%% - When a color has been selected, put it at the end of the LRU.

%% select_avail_reg(Regs) ->
%%   case get(seeded) of
%%     undefined ->
%% 	 random:seed(),
%% 	 put(seeded,true);
%%     true ->
%% 	 ok
%%   end,
%%   NReg = length(Regs),
%%   RegNo = random:uniform(NReg),
%%   lists:nth(RegNo, Regs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

push_spill_node(X, M, Stk) ->
  {[{X,{spill,M}}|Stk], M+1}.

push_colored(X, Stk) ->
  [{X, colorable} | Stk].

%%%%%%%%%%%%%%%%%%%%

low_degree_nodes([], _K, _NotAllocatable) -> [];
low_degree_nodes([{N,Info}|Xs], K, NotAllocatable) ->
  case lists:member(N, NotAllocatable) of
    true ->
      low_degree_nodes(Xs,K, NotAllocatable);
    false ->
      ?report0("node ~p has degree ~p: ~w~n",[N,degree(Info),neighbors(Info)]),
      Deg = degree(Info),
      if
	Deg < K ->
	  [N|low_degree_nodes(Xs, K, NotAllocatable)];
	true ->
	  low_degree_nodes(Xs, K, NotAllocatable)
      end
  end.

%%%%%%%%%%%%%%%%%%%%

unvisited_neighbors(X, Vis, IG) ->
  ordsets:from_list(unvisited(neighbors(X,IG), Vis)).

unvisited([], _Vis) -> [];
unvisited([X|Xs], Vis) ->
  case is_visited(X, Vis) of
    true ->
      unvisited(Xs, Vis);
    false ->
      [X|unvisited(Xs, Vis)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** ABSTRACT DATATYPES ***



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The ig datatype:
%%
%% Note: if we know the number of temps used, we can use a VECTOR
%% instead, which will speed up things.
%%
%% Note: later on, we may wish to add 'move-related' support.

-record(ig_info, {neighbors=[], degree=0 :: integer()}).

empty_ig(NumNodes) ->
  hipe_vectors:new(NumNodes, #ig_info{neighbors=[], degree=0}).

degree(Info) ->
  Info#ig_info.degree.

neighbors(Info) ->
  Info#ig_info.neighbors.

add_edge(X, X, IG) -> IG;
add_edge(X, Y, IG) ->
  add_arc(X, Y, add_arc(Y, X, IG)).

add_arc(X, Y, IG) ->
  Info = hipe_vectors:get(IG, X),
  Old = neighbors(Info),
  New = Info#ig_info{neighbors=[Y|Old]},
  hipe_vectors:set(IG, X, New).

normalize_ig(IG) ->
  Size = hipe_vectors:size(IG),
  normalize_ig(Size-1, IG).

normalize_ig(-1, IG) ->
  IG;
normalize_ig(I, IG) ->
  Info = hipe_vectors:get(IG, I),
  N = ordsets:from_list(neighbors(Info)),
  NewIG = hipe_vectors:set(IG, I, Info#ig_info{neighbors=N, degree=length(N)}),
  normalize_ig(I-1, NewIG).

%%degree(X, IG) ->
%%  Info = hipe_vectors:get(IG, X),
%%  Info#ig_info.degree.

neighbors(X, IG) ->
  Info = hipe_vectors:get(IG, X),
  Info#ig_info.neighbors.

decrement_degree(X, IG) ->
  Info = hipe_vectors:get(IG, X),
  Degree = degree(Info),
  NewDegree = Degree-1,
  NewInfo = Info#ig_info{degree=NewDegree},
  {NewDegree, hipe_vectors:set(IG,X,NewInfo)}.

list_ig(IG) ->
  hipe_vectors:list(IG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The spill cost datatype:
%%
%% Note: if we know the number of temps used, we can use a VECTOR
%% instead, which will speed up things.

empty_spill(NumNodes) ->
  hipe_vectors:new(NumNodes, 0).

spill_cost_of(X, Spill) ->
  hipe_vectors:get(Spill, X).

spill_cost_lookup(X, Spill) ->
  spill_cost_of(X, Spill).

spill_cost_update(X, N, Spill) ->
  hipe_vectors:set(Spill, X, N).

%%list_spill_costs(Spill) ->
%%  hipe_vectors:list(Spill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The coloring datatype:

none_colored(NumNodes) ->
  hipe_vectors:new(NumNodes,uncolored).

color_of(X,Cols) ->
  hipe_vectors:get(Cols,X).

set_color(X,R,Cols) ->
  hipe_vectors:set(Cols,X,{color,R}).

-ifdef(DEBUG).
list_coloring(Cols) ->
  hipe_vectors:list(Cols).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Note: there might be a slight gain in separating the two versions
%% of visit/2 and visited/2. (So that {var,X} selects X and calls the
%% integer version.

none_visited(NumNodes) ->
  hipe_vectors:new(NumNodes, false).

visit(X,Vis) ->
  hipe_vectors:set(Vis, X, true).

is_visited(X,Vis) ->
  hipe_vectors:get(Vis, X).

visit_all([], Vis) -> Vis;
visit_all([X|Xs], Vis) ->
  visit_all(Xs, visit(X, Vis)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check that all arcs in IG are bidirectional + degree is correct

%% check_ig(IG) ->
%%   check_ig(list_ig(IG),IG).

%% check_ig([],IG) -> 
%%   ok;
%% check_ig([{N,Info}|Xs],IG) ->
%%   Ns = neighbors(Info),
%%   NumNs = length(Ns),
%%   D = degree(Info),
%%   if
%%      D =:= NumNs ->
%%        ok;
%%      true ->
%% 	 ?WARNING_MSG('node ~p has degree ~p but ~p neighbors~n',[N,D,NumNs])
%%   end,
%%   check_neighbors(N,Ns,IG),
%%   check_ig(Xs,IG).

%% check_neighbors(N,[],IG) -> 
%%   ok;
%% check_neighbors(N,[M|Ms],IG) ->
%%   Ns = neighbors(M,IG),
%%   case member(N,Ns) of
%%     true ->
%% 	 ok;
%%     true ->
%% 	 ?WARNING_MSG('node ~p should have ~p as neighbor (has ~p)~n',[M,N,Ns])
%%   end,
%%   check_neighbors(N,Ms,IG).

-ifdef(DO_ASSERT).
%%%%%%%%%%%%%%%%%%%%
%% Check that the coloring is correct (if the IG is correct):
%%

check_coloring(Coloring, IG, Target) ->
  ?report0("checking coloring ~p~n",[Coloring]),
  check_cols(list_ig(IG),init_coloring(Coloring, Target)).

init_coloring(Xs, Target) ->
  hipe_temp_map:cols2tuple(Xs, Target).

check_color_of(X, Cols) ->
%%    if
%%	is_precoloured(X) ->
%%	    phys_reg_color(X,Cols);
%%	true ->
  case hipe_temp_map:find(X, Cols) of
    unknown ->
      ?WARNING_MSG("node ~p: color not found~n", [X]),
      uncolored;
    C ->
      C
  end.

check_cols([], Cols) ->
  ?report("coloring valid~n",[]),
  true;
check_cols([{X,Info}|Xs], Cols) ->
  Cs = [{N, check_color_of(N, Cols)} || N <- neighbors(Info)],
  C = check_color_of(X, Cols),
  case valid_coloring(X, C, Cs) of
    yes ->
      check_cols(Xs, Cols);
    {no,Invalids} ->
      ?WARNING_MSG("node ~p has same color (~p) as ~p~n", [X,C,Invalids]),
      check_cols(Xs, Cols)
  end.

valid_coloring(X, C, []) ->
  yes;
valid_coloring(X, C, [{Y,C}|Ys]) ->
  case valid_coloring(X, C, Ys) of
    yes -> {no, [Y]};
    {no,Zs} -> {no, [Y|Zs]}
  end;
valid_coloring(X, C, [_|Ys]) ->
  valid_coloring(X, C, Ys).
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** INTERFACES TO OTHER MODULES ***
%%

all_precoloured({TgtMod,TgtCtx}) ->
  TgtMod:all_precoloured(TgtCtx).

allocatable({TgtMod,TgtCtx}) ->
  TgtMod:allocatable(TgtCtx).

is_fixed(Reg, {TgtMod,TgtCtx}) ->
  TgtMod:is_fixed(Reg, TgtCtx).

labels(CFG, {TgtMod,TgtCtx}) ->
  TgtMod:labels(CFG, TgtCtx).

liveout(CFG, L, Target={TgtMod,TgtCtx}) ->
  ordsets:from_list(reg_names(TgtMod:liveout(CFG, L, TgtCtx), Target)).

bb(CFG, L, {TgtMod,TgtCtx}) ->
  hipe_bb:code(TgtMod:bb(CFG, L, TgtCtx)).

def_use(X, Target={TgtMod,TgtCtx}) ->
  {ordsets:from_list(reg_names(TgtMod:defines(X,TgtCtx), Target)),
   ordsets:from_list(reg_names(TgtMod:uses(X,TgtCtx), Target))}.

non_alloc(CFG, Target={TgtMod,TgtCtx}) ->
  reg_names(TgtMod:non_alloc(CFG, TgtCtx), Target).

number_of_temporaries(CFG, {TgtMod,TgtCtx}) ->
  TgtMod:number_of_temporaries(CFG, TgtCtx).

reg_names(Regs, {TgtMod,TgtCtx}) ->
  [TgtMod:reg_nr(X,TgtCtx) || X <- Regs].

%%
%% Precoloring: use this version when a proper implementation of
%%  physical_name(X) is available!
%%

precolor(Xs, Cols, Target) ->
  ?report("precoloring ~p~n", [Xs]),
  {_Cs, _NewCol} = Res = precolor0(Xs, Cols, Target),
  ?report("    yielded ~p~n", [_Cs]),
  Res.

precolor0([], Cols, _Target) ->
  {[], Cols};
precolor0([R|Rs], Cols, Target) ->
  {Cs, Cols1} = precolor0(Rs, Cols, Target),
  {[{R, {reg, physical_name(R, Target)}}|Cs], 
   set_color(R, physical_name(R, Target), Cols1)}.

physical_name(X, {TgtMod,TgtCtx}) ->
  TgtMod:physical_name(X, TgtCtx).
