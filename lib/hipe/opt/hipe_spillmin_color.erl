%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%% ===========================================================================
%%@doc
%%		  GRAPH COLORING STACK SLOT SPILL MINIMIZER
%%
%% A simple pessimistic graph coloring stack slot spill minimizer
%%
%% - build interference graph
%% - estimate number of stack slots needed
%% - simplify graph (push on stack, abort and retry with more stack slots if spill)
%% - select colors
%%
%% Emits a coloring: a list of {TempName,Location}
%%  where Location is {spill,M}.
%% {spill,M} denotes the Mth spilled node
%%
%% This version uses ETS tables
%%
%% Deficiencies:
%% - pessimistic coloring
%%

-module(hipe_spillmin_color).

-export([stackalloc/6]).

%%-ifndef(DO_ASSERT).
%%-define(DO_ASSERT, true).
%%-endif.

%%-ifndef(DEBUG).
%%-define(DEBUG,0).
%%-endif.

%%---------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("../flow/cfg.hrl").

%% Define these as 'ok' or 'report(X,Y)' depending on how much output you want.
-define(report0(X,Y), ?IF_DEBUG_LEVEL(0,?msg(X, Y),ok)).
-define(report(X,Y),  ?IF_DEBUG_LEVEL(1,?msg(X, Y),ok)). 
-define(report2(X,Y), ?IF_DEBUG_LEVEL(2,?msg(X, Y),ok)). 
-define(report3(X,Y), ?IF_DEBUG_LEVEL(3,?msg(X, Y),ok)).

%% Emits a coloring: a list of {TempName,Location}
%%  where Location is {spill,M}.
%% {spill,M} denotes the Mth spilled node

-spec stackalloc(#cfg{}, [_], non_neg_integer(),
		 comp_options(), module(), hipe_temp_map()) ->
                                {hipe_spill_map(), non_neg_integer()}.

stackalloc(CFG, _StackSlots, SpillIndex, _Options, Target, TempMap) ->
  ?report2("building IG~n", []),
  {IG, NumNodes} = build_ig(CFG, Target, TempMap),
  {Cols, MaxColors} = 
    color_heuristic(IG, 0, NumNodes, NumNodes, NumNodes, Target, 1),
  SortedCols = lists:sort(Cols),
  {remap_temp_map(SortedCols, TempMap, SpillIndex), SpillIndex+MaxColors}.

%% Rounds a floating point value upwards
ceiling(X) ->
  T = trunc(X),
  case (X - T) of
    Neg when Neg < 0.0 -> T;
    Pos when Pos > 0.0 -> T + 1;
    _ -> T
  end.

%% Emits a coloring: an unsorted list of {Temp,Location}
%%  where Location is {spill,M}.
%% {spill,M} denotes the Mth spilled node
%%
%% Notes:
%% - Arguments:
%%   IG: The interference graph
%%   Min: The lower bound, the minimal number of colors tried.
%%   Max: The upper bound, the maximal number of colors tried.
%%   Safe: The number of colors that are guaranteed to work. This is
%%         needed, because we reuse information from color() about how
%%         many colors it used at the last try, but this is not guaranteed to
%%         be a feasible solution because color might work differently using
%%         more colors although it has successfully colored the graph with
%%         fewer colors previously. Example: color(666) colors with 23 colors,
%%                                           but color(23) fails.
%%         We use Safe inefficently, because we run color 1 additional
%%         time with the same argument if Safe is needed.
%%   MaxNodes: The number of nodes in IG.
%%   Target: Target specific information.
%%   MaxDepth: The maximum recursion depth.
color_heuristic(IG, Min, Max, Safe, MaxNodes, Target, MaxDepth) ->
  case MaxDepth of
    0 ->
      case color(IG, ordsets:from_list(init_stackslots(Max)),
		 MaxNodes, Target) of
	not_easily_colorable ->
	  color(IG, ordsets:from_list(init_stackslots(Safe)),
		MaxNodes, Target);
	Else ->
	  Else
      end;
    _ ->
      %% This can be increased from 2, and by this the heuristic can be
      %% exited earlier, but the same can be achived by decreasing the
      %% recursion depth. This should not be decreased below 2.
      case (Max - Min) < 2 of
        true ->
          case color(IG, ordsets:from_list(init_stackslots(Max)),
		     MaxNodes, Target) of
            not_easily_colorable ->
              color(IG, ordsets:from_list(init_stackslots(Safe)),
                    MaxNodes, Target);
            Else ->
              Else
          end;
        false ->
	  NumSlots = ceiling((Max - Min)/2) + Min,
	  case color(IG, ordsets:from_list(init_stackslots(NumSlots)),
		     MaxNodes, Target) of
	    not_easily_colorable ->
	      color_heuristic(IG, NumSlots, Max,
			      Safe, MaxNodes, Target, MaxDepth - 1);
	    {_TmpCols, TmpMaxColors} ->
	      color_heuristic(IG, Min, TmpMaxColors,
			      NumSlots, MaxNodes, Target, MaxDepth - 1)
	  end
      end
  end.

%% Returns a new temp map with the spilled temporaries mapped to stack slots,
%% located after SpillIndex, according to Cols.
remap_temp_map(Cols, TempMap, SpillIndex) ->
  remap_temp_map0(Cols, hipe_temp_map:to_substlist(TempMap), SpillIndex).

remap_temp_map0([], _TempMap, _SpillIndex) ->
  [];
remap_temp_map0([{_M, {spill, N}}|Xs], [{TempNr, {spill,_}}|Ys], SpillIndex) ->
  [{TempNr, {spill, SpillIndex + N-1}}|remap_temp_map0(Xs, Ys, SpillIndex)];
remap_temp_map0(Cols, [_Y|Ys], SpillIndex) ->
  remap_temp_map0(Cols, Ys, SpillIndex).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** BUILD THE INTERFERENCE GRAPH ***
%%
%% Returns {Interference_graph, Number_Of_Nodes}
%%

build_ig(CFG, Target, TempMap) ->
  try build_ig0(CFG, Target, TempMap)
  catch error:Rsn -> exit({regalloc, build_ig, Rsn})
  end.

%% Creates an ETS table consisting of the keys given in List, with the values
%% being an integer which is the position of the key in List.
%% [1,5,7] -> {1,0} {5,1} {7,2}
%% etc.
setup_ets(List) ->
  setup_ets0(List, ets:new(tempMappingTable, []), 0).

setup_ets0([], Table, _N) ->
  Table;
setup_ets0([X|Xs], Table, N) ->
  ets:insert(Table, {X, N}),
  setup_ets0(Xs, Table, N+1).

build_ig0(CFG, Target, TempMap) ->
  Live = Target:analyze(CFG),
  TempMapping = map_spilled_temporaries(TempMap),
  TempMappingTable = setup_ets(TempMapping),
  NumSpilled = length(TempMapping),
  IG = build_ig_bbs(Target:labels(CFG), CFG, Live, empty_ig(NumSpilled),
		    Target, TempMap, TempMappingTable),
  ets:delete(TempMappingTable),
  {normalize_ig(IG), NumSpilled}.

build_ig_bbs([], _CFG, _Live, IG, _Target, _TempMap, _TempMapping) ->
  IG;
build_ig_bbs([L|Ls], CFG, Live, IG, Target, TempMap, TempMapping) ->
  Xs = bb(CFG, L, Target),
  LiveOut = [X || X <- liveout(Live, L, Target), 
		  hipe_temp_map:is_spilled(X, TempMap)],
  LiveOutList = ordsets:to_list(LiveOut),
  LiveOutListMapped = list_map(LiveOutList, TempMapping, []),
  LiveOutSetMapped = ordsets:from_list(LiveOutListMapped),
  {_, NewIG} = 
    build_ig_bb(Xs, LiveOutSetMapped, IG, Target, TempMap, TempMapping),
  build_ig_bbs(Ls, CFG, Live, NewIG, Target, TempMap, TempMapping).

build_ig_bb([], LiveOut, IG, _Target, _TempMap, _TempMapping) ->
  {LiveOut, IG};
build_ig_bb([X|Xs], LiveOut, IG, Target, TempMap, TempMapping) ->
  {Live,NewIG} = 
    build_ig_bb(Xs, LiveOut, IG, Target, TempMap, TempMapping),
  build_ig_instr(X, Live, NewIG, Target, TempMap, TempMapping).

build_ig_instr(X, Live, IG, Target, TempMap, TempMapping) ->
  {Def, Use} = def_use(X, Target, TempMap),
  ?report3("Live ~w\n~w : Def: ~w Use ~w\n",[Live, X, Def,Use]),
  DefListMapped = list_map(Def, TempMapping, []),
  UseListMapped = list_map(Use, TempMapping, []),
  DefSetMapped = ordsets:from_list(DefListMapped),
  UseSetMapped = ordsets:from_list(UseListMapped),
  NewIG = interference_arcs(DefListMapped, ordsets:to_list(Live), IG),
  NewLive = ordsets:union(UseSetMapped, ordsets:subtract(Live, DefSetMapped)),
  {NewLive, NewIG}.

%% Given a list of Keys and an ets-table returns a list of the elements 
%% in Mapping corresponding to the Keys and appends Acc to this list.
list_map([], _Mapping, Acc) ->
  Acc;
list_map([X|Xs], Mapping, Acc) ->
  {_Key, Val} = hd(ets:lookup(Mapping, X)),
  list_map(Xs, Mapping, [Val | Acc]).

%% Returns an ordered list of spilled temporaries in TempMap
map_spilled_temporaries(TempMap) ->
  map_spilled_temporaries0(hipe_temp_map:to_substlist(TempMap)).

map_spilled_temporaries0([]) ->
  [];
map_spilled_temporaries0([{N, {spill, _}}|Xs]) ->
  [N | map_spilled_temporaries0(Xs)];
map_spilled_temporaries0([_X|Xs]) ->
  map_spilled_temporaries0(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interference_arcs([], _Live, IG) ->
  IG;
interference_arcs([X|Xs], Live, IG) ->
  interference_arcs(Xs, Live, i_arcs(X, Live, IG)).

i_arcs(_X, [], IG) -> 
  IG;
i_arcs(X, [Y|Ys], IG) ->
  i_arcs(X, Ys, add_edge(X, Y, IG)).

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
%%     throw an exception (the caller should retry with more stack slots)

color(IG, StackSlots, NumNodes, Target) ->
  try
    color_0(IG, StackSlots, NumNodes, Target)
  catch
    error:Rsn ->
      ?error_msg("Coloring failed with ~p~n", [Rsn]),
      ?EXIT(Rsn)
  end.

color_0(IG, StackSlots, NumNodes, Target) -> 
  ?report("simplification of IG~n", []),
  K = ordsets:size(StackSlots),
  Nodes = list_ig(IG),
  Low = low_degree_nodes(Nodes, K),
  ?report(" starting with low degree nodes ~p~n", [Low]),
  EmptyStk = [],
  case simplify(Low, NumNodes, IG, K, EmptyStk, Target) of
    non_simplifiable -> not_easily_colorable;
    Stk ->
      ?report(" selecting colors~n", []),
      select(Stk, IG, StackSlots, NumNodes)
  end.

%%%%%%%%%%%%%%%%%%%%
%%
%% Simplification: push all easily colored nodes on a stack;
%%  when the list of easy nodes becomes empty, see if graph is
%%  empty as well. If it is not, throw an exception and abort.
%%  If it is empty, return the stack.
%%
%% Notes:
%% - Arguments:
%%   Low: low-degree nodes (ready to color)
%%   NumNodes: number of remaining nodes in graph
%%   IG: interference graph
%%   K: number of colors
%%   Stk: stack of already simplified nodes
%%   Target: Machine to compile for

simplify(Low, NumNodes, IG, K, Stk, Target) ->
  Vis = none_visited(NumNodes),
  simplify_ig(Low, NumNodes, IG, K, Stk, Vis, Target).

simplify_ig([], 0, _IG, _K, Stk, _Vis, _Target) ->
  Stk;
simplify_ig([], N, _IG, _K, _Stk, _Vis, _Target) when N > 0 ->
  ?report3("N: ~w Stk: ~w N+Stk ~w\n", [N,length(Stk),N+length(Stk)]),
  non_simplifiable;
simplify_ig([X|Xs], N, IG, K, Stk, Vis, Target) ->
  ?report3("N: ~w Stk: ~w N+Stk ~w\n", [N,length(Stk),N+length(Stk)]),
  case is_visited(X, Vis) of
    true ->
      ?report("  node ~p already visited~n", [X]),
      simplify_ig(Xs, N, IG, K, Stk, Vis, Target);
    false ->
      ?report("Stack ~w\n", [Stk]),
      {NewLow, NewIG} = decrement_neighbors(X, Xs, IG, Vis, K),
      ?report("  node ~w pushed\n(~w now ready)~n", [X, NewLow]),
      NewStk = push_colored(X, Stk),
      simplify_ig(NewLow, N-1, NewIG, K, NewStk, visit(X, Vis), Target)
  end.

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
%% Returns a list of {Name,Location}, where Location is {spill,M}
%%
%% Note: we use pessimistic coloring here.
%% - we could use optimistic coloring: for spilled node, check if there is
%%   an unused color among the neighbors and choose that.

select(Stk, IG, PhysRegs, NumNodes) ->
  select_colors(Stk, IG, none_colored(NumNodes), PhysRegs).

select_colors([], _IG, _Cols, _PhysRegs) -> 
  ?report("all nodes colored~n", []),
  {[], 0};
select_colors([{X,colorable}|Xs], IG, Cols, PhysRegs) ->
  ?report("color of ~p\n", [X]),
  {Slot,NewCols} = select_color(X, IG, Cols, PhysRegs),
  ?report("~p~n", [Slot]),
  {Tail, MaxColor} = select_colors(Xs, IG, NewCols, PhysRegs),
  NewMaxColor = erlang:max(Slot, MaxColor),
  %% Since we are dealing with spills we label all our temporaries accordingly.
  {[{X,{spill,Slot}} | Tail], NewMaxColor}.

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
    {color, R} ->
      [R|get_colors(Xs, Cols)]
  end.

select_unused_color(UsedColors, PhysRegs) ->
  Summary = ordsets:from_list(UsedColors),
  AvailRegs = ordsets:to_list(ordsets:subtract(PhysRegs, Summary)),
  hd(AvailRegs).

push_colored(X, Stk) ->
  [{X, colorable} | Stk].

low_degree_nodes([], _K) -> [];
low_degree_nodes([{N,Info}|Xs], K) ->
  ?report0("node ~p has degree ~p: ~w~n", [N, degree(Info), neighbors(Info)]),
  Deg = degree(Info),
  if
    Deg < K ->
      [N|low_degree_nodes(Xs, K)];
    true ->
      low_degree_nodes(Xs, K)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unvisited_neighbors(X, Vis, IG) ->
  ordsets:from_list(unvisited(neighbors(X, IG), Vis)).

unvisited([], _Vis) -> [];
unvisited([X|Xs], Vis) ->
  case is_visited(X, Vis) of
    true ->
      unvisited(Xs, Vis);
    false ->
      [X|unvisited(Xs, Vis)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** ABSTRACT DATATYPES ***
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% The stack slot datatype
%%

init_stackslots(NumSlots) ->
  init_stackslots(NumSlots, []).

init_stackslots(0, Acc) ->
  Acc;
init_stackslots(NumSlots, Acc) ->
  init_stackslots(NumSlots - 1, [NumSlots|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The ig datatype:
%%
%% Note: if we know the number of temps used, we can use a VECTOR
%% instead, which will speed up things.
%%
%% Note: later on, we may wish to add 'move-related' support.

-record(ig_info, {neighbors = [] :: [_], degree = 0 :: non_neg_integer()}).

empty_ig(NumNodes) ->
  hipe_vectors:new(NumNodes, #ig_info{}).

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
  New = Info#ig_info{neighbors = [Y|Old]},
  hipe_vectors:set(IG,X,New).

normalize_ig(IG) ->
  Size = hipe_vectors:size(IG),
  normalize_ig(Size-1, IG).

normalize_ig(-1, IG) ->
  IG;
normalize_ig(I, IG) ->
  Info = hipe_vectors:get(IG, I),
  N = ordsets:from_list(neighbors(Info)),
  NewInfo = Info#ig_info{neighbors = N, degree = length(N)},
  NewIG = hipe_vectors:set(IG, I, NewInfo),
  normalize_ig(I-1, NewIG).

neighbors(X, IG) ->
  Info = hipe_vectors:get(IG, X),
  Info#ig_info.neighbors.

decrement_degree(X, IG) ->
  Info = hipe_vectors:get(IG, X),
  Degree = degree(Info),
  NewDegree = Degree-1,
  NewInfo = Info#ig_info{degree = NewDegree},
  {NewDegree, hipe_vectors:set(IG, X, NewInfo)}.

list_ig(IG) ->
  hipe_vectors:list(IG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The coloring datatype:

none_colored(NumNodes) ->
  hipe_vectors:new(NumNodes, uncolored).

color_of(X, Cols) ->
  hipe_vectors:get(Cols, X).

set_color(X, R, Cols) ->
  hipe_vectors:set(Cols, X, {color, R}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Note: there might be a slight gain in separating the two versions
%% of visit/2 and visited/2. (So that {var,X} selects X and calls
%% the integer version.

none_visited(NumNodes) ->
  hipe_vectors:new(NumNodes, false).

visit(X, Vis) ->
  hipe_vectors:set(Vis, X, true).

is_visited(X, Vis) ->
  hipe_vectors:get(Vis, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** INTERFACES TO OTHER MODULES ***
%%

liveout(CFG, L, Target) ->
  ordsets:from_list(reg_names(Target:liveout(CFG, L), Target)).

bb(CFG, L, Target) ->
   hipe_bb:code(Target:bb(CFG, L)).

def_use(X, Target, TempMap) ->
  Defines = [Y || Y <- reg_names(Target:defines(X), Target), 
		  hipe_temp_map:is_spilled(Y, TempMap)],
  Uses = [Z || Z <- reg_names(Target:uses(X), Target), 
	       hipe_temp_map:is_spilled(Z, TempMap)],
  {Defines, Uses}.

reg_names(Regs, Target) ->
  [Target:reg_nr(X) || X <- Regs].
