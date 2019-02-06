%% -*- Erlang -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GENERIC MODULE TO PERFORM LIVENESS ANALYSIS ON SSA FORM
%%
%% Exports:
%% ~~~~~~~
%% analyze(CFG) - returns a liveness analysis of CFG.
%% liveout(Liveness, Label) - returns the list of variables that are
%%      live at exit from basic block named Label.
%% livein(Liveness, Label) - returns the list of variables that are
%%      live on entry to the basic block named Label.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Uncomment the following if this is ever needed as an independent module
%%
-ifdef(LIVENESS_NEEDED).
-export([ssa_liveness__analyze/1,
	 ssa_liveness__livein/2]).
%%	 ssa_liveness__livein/3],
%%	 ssa_liveness__liveout/2]).
-type set(E) :: gb_sets:set(E).
-type liveness(Label, Var) ::
	#{Label => {{Gen          :: set(Var),
		     Kill         :: set(Var),
		     {TotalDirGen :: set(Var),
		      DirGen      :: gb_trees:tree(Label, set(Var))}},
		    LiveIn        :: set(Var),
		    LiveOut       :: set(Var),
		    Successors    :: [Label]}}.
-endif.
%% -ifdef(DEBUG_LIVENESS).
%% -export([pp_liveness/1]).
%% -endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface functions that MUST be implemented in the supporting files
%%
%% In the CFG file:
%% ----------------
%%  - bb(CFG, L) -> BasicBlock, extract a basic block from a cfg.
%%  - postorder(CFG) -> [Labels], the labels of the cfg in postorder
%%  - succ(CFG, L) -> [Labels], 
%%  - function(CFG) -> {M,F,A}
%%
%% In the CODE file:
%% ----------------- 
%%  - uses(Instr) ->
%%  - defines(Instr) ->
%%  - is_phi(Instr) -> Boolean
%%  - phi_arglist(Instr) -> [{Pred, Var}]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The generic liveness analysis on SSA form
%%
ssa_liveness__analyze(CFG) ->
  PO = ?CFG:postorder(CFG),
  InitLiveness = liveness_init(init(PO, CFG)),
  merry_go_around(PO, InitLiveness).

%%
%% The fixpoint iteration
%%

merry_go_around(Labels, Liveness) ->
  case doit_once(Labels, Liveness) of
    {fixpoint, NewLiveness} -> 
      NewLiveness;
    {value, NewLiveness} -> 
      merry_go_around(Labels, NewLiveness)
  end.

%%
%% One iteration
%%

doit_once(Labels, Liveness) ->
  doit_once(Labels, Liveness, true).

doit_once([], Liveness, FixPoint) ->
  if FixPoint -> {fixpoint, Liveness};
     true -> {value, Liveness}
  end;
doit_once([L|Ls], Liveness, FixPoint) ->
  LiveOut = join_livein(Liveness, L),
  NewLiveness = update_liveout(L, LiveOut, Liveness),
  Kill = set_subtract(LiveOut, kill(L, NewLiveness)),
  LiveIn = set_union(Kill, gen(L, NewLiveness)),
  case update_livein(L, LiveIn, NewLiveness) of
    fixpoint -> doit_once(Ls, NewLiveness, FixPoint);
    {value, NewLiveness1} -> doit_once(Ls, NewLiveness1, false)
  end.
      
%%
%% updates liveness for a basic block
%%

update_livein(Label, NewLiveIn, Liveness) ->
  {GKD, LiveIn, LiveOut, Succ} = liveness_lookup(Label, Liveness),
  case LiveIn of
    NewLiveIn -> 
      fixpoint;
    _ -> 
      {value, liveness_update(Label, {GKD,NewLiveIn,LiveOut,Succ}, Liveness)}
  end.

update_liveout(Label, NewLiveOut, Liveness) ->
  {GKD, LiveIn, _LiveOut, Succ} = liveness_lookup(Label, Liveness),
  liveness_update(Label, {GKD,LiveIn,NewLiveOut,Succ}, Liveness).

%%
%% Join live in to get the new live out.
%%

join_livein(Liveness, L) ->
  Succ = successors(L, Liveness),
  case Succ of
    [] ->  % special case if no successors
      gb_sets:from_list(liveout_no_succ());
    _ ->
      join_livein1(L, Succ, Liveness)
  end.

join_livein1(Pred, Labels, Liveness) ->
  join_livein1(Pred, Labels, Liveness, new_set()).

join_livein1(_Pred, [], _Liveness, Live) ->
  Live;
join_livein1(Pred, [L|Ls], Liveness, Live) ->
  OldLivein = livein_set(Liveness, L, Pred),
  NewLive = set_union(OldLivein, Live),
  join_livein1(Pred, Ls, Liveness, NewLive).


ssa_liveness__liveout(Liveness, L) ->
  {_GKD, _LiveIn, LiveOut, Successors} = liveness_lookup(L, Liveness),
  case Successors of
    [] ->  % special case if no successors
      liveout_no_succ();
    _ ->
      set_to_list(LiveOut)
  end.  

-ifdef(LIVENESS_NEEDED).
ssa_liveness__livein(Liveness, L) ->
  set_to_list(livein_set(Liveness, L)).

%% ssa_liveness__livein(Liveness, L, Pred) ->
%%   set_to_list(livein_set(Liveness, L, Pred)).

livein_set(Liveness, L) ->
  {{_Gen,_Kill,{TotalDirGen, _DirGen}}, LiveIn, _LiveOut, _Successors} = 
    liveness_lookup(L, Liveness),
  set_union(TotalDirGen, LiveIn).
-endif.

livein_set(Liveness, L, Pred) ->
  {{_Gen,_Kill,{_TotalDirGen, DirGen}}, LiveIn, _LiveOut, _Successors} = 
    liveness_lookup(L, Liveness),
  case gb_trees:lookup(Pred, DirGen) of
    none ->
      LiveIn;
    {value, LiveInFromPred} ->
      set_union(LiveInFromPred, LiveIn)
  end.

successors(L, Liveness) ->
  {_GKD, _LiveIn, _LiveOut, Successors} = liveness_lookup(L, Liveness),
  Successors.

kill(L, Liveness) ->
  {{_Gen,Kill,_DirGen},_LiveIn,_LiveOut,_Successors} = 
    liveness_lookup(L, Liveness),
  Kill.

gen(L, Liveness) ->
  {{Gen,_Kill,_DirGen},_LiveIn,_LiveOut,_Successors} = 
    liveness_lookup(L, Liveness),
  Gen.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% init returns a list of: {Label, {{Gen, Kill}, LiveIn, Successors}}
%%    - Label is the name of the basic block.
%%    - Gen is the set of varables that are used by this block.
%%    - Kill is the set of varables that are defined by this block.
%%    - LiveIn is the set of variables that are alive at entry to the
%%      block (initially empty).
%%    - Successors is a list of the successors to the block.

init([], _) ->
  [];
init([L|Ls], CFG) ->
  BB = ?CFG:bb(CFG, L),
  Code = hipe_bb:code(BB),
  Succ = ?CFG:succ(CFG, L),
  {Gen, Kill} = make_bb_transfer(Code, Succ),
  DirectedGen = get_directed_gen(Code),
  [{L, {{Gen, Kill, DirectedGen}, new_set(), new_set(), Succ}} 
   | init(Ls, CFG)].

make_bb_transfer([], _Succ) ->
  {new_set(), new_set()};   % {Gen, Kill}
make_bb_transfer([I|Is], Succ) ->
  {Gen, Kill} = make_bb_transfer(Is, Succ),
  case ?CODE:is_phi(I) of
    true ->
      InstrKill = set_from_list(?CODE:defines(I)),
      Gen1 = set_subtract(Gen, InstrKill),
      Kill1 = set_union(Kill, InstrKill),
      {Gen1, Kill1};
    false ->
      InstrGen = set_from_list(?CODE:uses(I)),
      InstrKill = set_from_list(?CODE:defines(I)),
      Gen1 = set_subtract(Gen, InstrKill),
      Gen2 = set_union(Gen1, InstrGen),
      Kill1 = set_union(Kill, InstrKill),
      Kill2 = set_subtract(Kill1, InstrGen),
      {Gen2, Kill2}
  end.

get_directed_gen(Code) ->
  Map = get_directed_gen_1(Code),
  TotalGen = lists:foldl(fun({_Pred, Gen}, Acc) ->
			     set_union(Gen, Acc)
			 end, new_set(), gb_trees:to_list(Map)),
  {TotalGen, Map}.

get_directed_gen_1([I|Left])->
  case ?CODE:is_phi(I) of
    false -> 
      gb_trees:empty();
    true -> 
      Map = get_directed_gen_1(Left),
      ArgList = ?CODE:phi_arglist(I),
      lists:foldl(fun update_directed_gen/2, Map, ArgList)
  end.

update_directed_gen({Pred, Var}, Map)->      
  case gb_trees:lookup(Pred, Map) of
    none -> gb_trees:insert(Pred, set_from_list([Var]), Map);
    {value, Set} -> gb_trees:update(Pred, set_add(Var, Set), Map)
  end.
       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% liveness
%%
-compile({inline, [liveness_lookup/2, liveness_update/3]}).

liveness_init(List) ->
  maps:from_list(List).

liveness_lookup(Label, Liveness) ->
  maps:get(Label, Liveness).
liveness_update(Label, Val, Liveness) ->
  maps:update(Label, Val, Liveness).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Sets
%%

new_set() ->
  gb_sets:empty().

set_union(S1, S2) ->
  gb_sets:union(S1, S2).

set_subtract(S1, S2) ->
  gb_sets:subtract(S1, S2).

set_from_list(List) ->
  gb_sets:from_list(List).

set_to_list(Set) ->
  gb_sets:to_list(Set).

set_add(Var, Set) ->
  gb_sets:add(Var, Set).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Pretty printer
%%

-ifdef(DEBUG_LIVENESS).

pp_liveness(CFG) ->
  io:format("Liveness for ~p:\n", [?CFG:function(CGF)]),
  Liveness = analyze(CFG),
  RevPostorder = lists:reverse(?CFG:postorder(CFG)),
  Edges = [{X, Y} || X <- RevPostorder, Y <- ?CFG:succ(CFG, X)],
  pp_liveness_edges(Edges, Liveness).

pp_liveness_edges([{From, To}|Left], Liveness)->
  LiveIn = livein(Liveness, To, From),
  io:format("Label ~w -> Label ~w: ~p\n", [From, To, LiveIn]),
  LiveOut = liveout(Liveness, From),
  io:format("Total live out from Label ~w: ~p\n", [From, LiveOut]),
  pp_liveness_edges(Left, Liveness);
pp_liveness_edges([], _Liveness) ->
  ok.

-endif.
