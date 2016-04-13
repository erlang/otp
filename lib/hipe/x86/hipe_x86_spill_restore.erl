%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% ====================================================================
%% Authors : Dogan Yazar and Erdem Aksu (KT2 project of 2008)
%% ====================================================================

-ifdef(HIPE_AMD64).
-define(HIPE_X86_SPILL_RESTORE, hipe_amd64_spill_restore).
-define(HIPE_X86_LIVENESS,      hipe_amd64_liveness).
-define(HIPE_X86_SPECIFIC,      hipe_amd64_specific).
-define(HIPE_X86_REGISTERS,	hipe_amd64_registers).
-define(X86STR, "amd64").
-else.
-define(HIPE_X86_SPILL_RESTORE, hipe_x86_spill_restore).
-define(HIPE_X86_LIVENESS,      hipe_x86_liveness).
-define(HIPE_X86_SPECIFIC,      hipe_x86_specific).
-define(HIPE_X86_REGISTERS,     hipe_x86_registers).
-define(X86STR, "x86").
-endif.

-module(?HIPE_X86_SPILL_RESTORE).

-export([spill_restore/2]).

%% controls which set library is used to keep temp variables. 
-define(SET_MODULE, ordsets).

%% Turn on instrumentation.
-define(HIPE_INSTRUMENT_COMPILER, true). 

-include("../main/hipe.hrl").
-include("../x86/hipe_x86.hrl"). % Added for the definition of #pseudo_call{}
-include("../flow/cfg.hrl").     % Added for the definition of #cfg{}

%% Main function
spill_restore(Defun, Options) ->
  CFG = ?option_time(firstPass(Defun), ?X86STR" First Pass", Options),
  CFGFinal = ?option_time(secondPass(CFG), ?X86STR" Second Pass", Options),
  hipe_x86_cfg:linearise(CFGFinal).

%% Performs the first pass of the algorithm.
%% By working bottom up, introduce the pseudo_spills.
firstPass(Defun) ->
  CFG0 = ?HIPE_X86_SPECIFIC:defun_to_cfg(Defun),
  %% get the labels bottom up
  Labels = hipe_x86_cfg:postorder(CFG0),
  Liveness = ?HIPE_X86_LIVENESS:analyse(CFG0),
  %% spill around the function will be introduced below the move
  %% formals, so get all labels except it.
  LabelsExceptMoveFormals = lists:sublist(Labels, length(Labels)-1),
  %% all work is done by the helper function firstPassHelper
  %% saveTree keeps the all newly introduced spills. Keys are the labels.
  {CFG1, SaveTree} = firstPassHelper(LabelsExceptMoveFormals, Liveness, CFG0),
  case hipe_x86_cfg:reverse_postorder(CFG0) of
    [Label1, Label2|_] ->
      SaveTreeElement = saveTreeLookup(Label2, SaveTree),
      %% FilteredSaveTreeElement is the to be spilled temps around the
      %% function call. They are spilled just before move formals.
      FilteredSaveTreeElement = [T || T <- SaveTreeElement, temp_is_pseudo(T)],
      Block = hipe_x86_cfg:bb(CFG1, Label1),
      Code = hipe_bb:code(Block),
      %% The following statements are tedious but work ok.
      %% Put spills between move formals and the jump code.
      %% This disgusting thing is done because spills should be
      %% introduced after move formals. 
      %% Another solution may be to introduce another block.
      MoveCodes = lists:sublist(Code, length(Code)-1),
      JumpCode = lists:last(Code),
      hipe_x86_cfg:bb_add(CFG1, Label1, hipe_bb:mk_bb(MoveCodes ++ [hipe_x86:mk_pseudo_spill(FilteredSaveTreeElement), JumpCode]));
    _ ->
      CFG1
  end.

%% helper function of firstPass

%% processes all labels recursively and decides the spills to be put.
%% spills are introduced before each function call (pseudo_call) as well as 
%% global spill is found
firstPassHelper(Labels, Liveness, CFG) ->
  firstPassHelper(Labels, Liveness, CFG, gb_trees:empty()).

firstPassHelper([Label|Labels], Liveness, CFG, SaveTree) ->
  LiveOut = from_list(?HIPE_X86_LIVENESS:liveout(Liveness, Label)),
  Block = hipe_x86_cfg:bb(CFG, Label),
  Code = hipe_bb:code(Block),
  Succ = hipe_x86_cfg:succ(CFG, Label),
  IntersectedSaveList = findIntersectedSaveList(Succ,SaveTree),
  %% call firstPassDoBlock which will give the updated block
  %% code(including spills) as well as Intersected Save List which
  %% should be passed above blocks
  {_,NewIntersectedList,NewCode} = 
    firstPassDoBlock(Code, LiveOut,IntersectedSaveList),
  NewBlock = hipe_bb:code_update(Block, NewCode),
  NewCFG = hipe_x86_cfg:bb_add(CFG, Label, NewBlock), 
  SizeOfSet = setSize(NewIntersectedList),
  %% if the Intersected Save List is not empty, insert it in the save tree.
  if SizeOfSet =/= 0 ->
      UpdatedSaveTree = gb_trees:insert(Label, NewIntersectedList, SaveTree),
      firstPassHelper(Labels, Liveness, NewCFG, UpdatedSaveTree);
     true ->
      firstPassHelper(Labels, Liveness, NewCFG, SaveTree)
  end;
firstPassHelper([], _, CFG, SaveTree) ->
  {CFG, SaveTree}.

%% handle each instruction in the block bottom up
firstPassDoBlock(Insts, LiveOut, IntersectedSaveList) -> 
  lists:foldr(fun firstPassDoInsn/2, {LiveOut,IntersectedSaveList,[]}, Insts).

firstPassDoInsn(I, {LiveOut,IntersectedSaveList,PrevInsts}) ->
  case I of
    #pseudo_call{} ->
      do_pseudo_call(I, {LiveOut,IntersectedSaveList,PrevInsts});
    _ -> % other instructions
      DefinedList = from_list( ?HIPE_X86_LIVENESS:defines(I)),
      UsedList = from_list(?HIPE_X86_LIVENESS:uses(I)),
      NewLiveOut = subtract(union(LiveOut, UsedList), DefinedList),
      NewIntersectedSaveList = subtract(IntersectedSaveList, DefinedList),
      {NewLiveOut, NewIntersectedSaveList, [I|PrevInsts]}
  end.

do_pseudo_call(I, {LiveOut,IntersectedSaveList,PrevInsts}) ->
  LiveTemps = [Temp || Temp <- to_list(LiveOut), temp_is_pseudo(Temp)],
  NewIntersectedSaveList = union(IntersectedSaveList, LiveOut),
  {LiveOut, NewIntersectedSaveList, [hipe_x86:mk_pseudo_spill(LiveTemps), I | PrevInsts]}.
    
findIntersectedSaveList(LabelList, SaveTree) -> 
  findIntersectedSaveList([saveTreeLookup(Label,SaveTree) || Label <- LabelList]).

findIntersectedSaveList([]) ->
  [];
findIntersectedSaveList([List1]) ->
  List1;
findIntersectedSaveList([List1,List2|Rest]) ->
  findIntersectedSaveList([intersection(List1, List2)|Rest]).

saveTreeLookup(Label, SaveTree) ->
  case gb_trees:lookup(Label, SaveTree) of
    {value, SaveList} ->
      SaveList;
    _ ->
      []
  end.

%% Performs the second pass of the algorithm.
%% It basically eliminates the unnecessary spills and introduces restores.
%% Works top down
secondPass(CFG0) ->
  Labels = hipe_x86_cfg:reverse_postorder(CFG0),
  Liveness = ?HIPE_X86_LIVENESS:analyse(CFG0),
  secondPassHelper(Labels,Liveness,CFG0).

%% helper function of secondPass.

%% recursively handle all labels given.
secondPassHelper(Labels, Liveness, CFG) ->
  secondPassHelper(Labels, Liveness, CFG, gb_trees:empty(), CFG).

%% AccumulatedCFG stands for the CFG that has restore edges incrementally.
%% UnmodifiedCFG is the CFG created after first pass.

%% AccumulatedSaveTree is used to eliminate the unnecessary saves. The
%% saves (spills) in above blocks are traversed down (if still live
%% and not redefined) and redundant saves are eliminated in the lower
%% blocks.
%% For memory efficiency, it may be better not to maintain the
%% AccumulatedSaveTree but traverse the tree recursively and pass the
%% save lists to the childs individually.
%% But current approach may be faster even though it needs bigger memory.

secondPassHelper([Label|RestOfLabels], Liveness,
		 AccumulatedCFG, AccumulatedSaveTree, UnmodifiedCFG) ->
  LiveOut = ?HIPE_X86_LIVENESS:liveout(Liveness, Label),
  Block = hipe_x86_cfg:bb(AccumulatedCFG, Label),
  Code = hipe_bb:code(Block),
  
  %% UnmodifiedCFG is needed for getting the correct predecessors.
  %% (i.e. not to get the restore edge blocks)
  PredList = hipe_x86_cfg:pred(UnmodifiedCFG, Label),
  %% find the spills coming from all the parents by intersecting 
  InitialAccumulatedSaveList = 
    findIntersectedSaveList(PredList, AccumulatedSaveTree),
  AccumulatedSaveList =
    keepLiveVarsInAccumSaveList(InitialAccumulatedSaveList, LiveOut),
  
  {NewCode, CFGUpdateWithRestores, NewAccumulatedSaveList} =
    secondPassDoBlock(Label, Code, AccumulatedCFG, AccumulatedSaveList),
  
  UpdatedAccumulatedSaveTree =
    gb_trees:insert(Label, NewAccumulatedSaveList, AccumulatedSaveTree),
  NewBlock = hipe_bb:code_update(Block, NewCode),
  NewCFG = hipe_x86_cfg:bb_add(CFGUpdateWithRestores, Label, NewBlock),
  secondPassHelper(RestOfLabels, Liveness, NewCFG,
		   UpdatedAccumulatedSaveTree, UnmodifiedCFG);
secondPassHelper([], _, AccumulatedCFG, _, _) ->
  AccumulatedCFG.

secondPassDoBlock(CurrentLabel, Insts, CFG, AccumulatedSaveList) -> 
  {NewAccumulatedSaveList,NewInsts,_,_,CFGUpdateWithRestores} = 
    lists:foldl(fun secondPassDoInsn/2, {AccumulatedSaveList,[],[],CurrentLabel,CFG}, Insts),
  {NewInsts, CFGUpdateWithRestores, NewAccumulatedSaveList}.

secondPassDoInsn(I, {AccumulatedSaveList,PrevInsts,SpillList,CurrentLabel,CFG}) -> 
  case I of
    #pseudo_spill{} ->
      %% spill variables that are not accumulated from top down
      %% (which are not already saved)
      VariablesAlreadySaved = [X || {X,_} <- to_list(AccumulatedSaveList)],
      VariablesToBeSpilled = I#pseudo_spill.args -- VariablesAlreadySaved,
      NewSpillList = [{Temp, hipe_x86:mk_new_temp(Temp#x86_temp.type)} || Temp <- VariablesToBeSpilled],
      %% update accumulated saved list by adding the newly spilled variables.
      NewAccumulatedSaveList = union(AccumulatedSaveList, from_list(NewSpillList)),
      {NewAccumulatedSaveList, PrevInsts ++ secondPassDoPseudoSpill(NewSpillList), NewSpillList, CurrentLabel, CFG};
    #pseudo_call{} ->
      {CFGUpdateWithRestores, NewPseudoCall} =
	secondPassDoPseudoCall(I, AccumulatedSaveList, CFG),
      %% spill list is emptied after use
      {AccumulatedSaveList, PrevInsts ++ [NewPseudoCall], CurrentLabel, [], CFGUpdateWithRestores};
    _ ->
      %% remove the defined variables from the accumulated save
      %% list since they need to be saved again in later occasions.
      DefinedList = from_list(?HIPE_X86_LIVENESS:defines(I)),
      NewAccumulatedSaveList = removeRedefVarsFromAccumSaveList(AccumulatedSaveList, DefinedList),
      {NewAccumulatedSaveList, PrevInsts ++ [I], SpillList, CurrentLabel, CFG}
  end.

%% remove dead vars from accumulated save list so that they are not restored.  
keepLiveVarsInAccumSaveList([], _) ->
  [];
keepLiveVarsInAccumSaveList([{Var,Temp}|Rest], DefinedList) ->
  IsDefined = is_element(Var, DefinedList),
  case IsDefined of 
    true  -> [{Var,Temp}|keepLiveVarsInAccumSaveList(Rest, DefinedList)];
    false -> keepLiveVarsInAccumSaveList(Rest, DefinedList)    
  end.

%% remove the redefined variables from accumulated save list since
%% they are changed.
removeRedefVarsFromAccumSaveList([], _) ->
  [];
removeRedefVarsFromAccumSaveList([{Var,Temp}|Rest], DefinedList) ->
  IsDefined = is_element(Var, DefinedList),
  case IsDefined of 
    true  -> removeRedefVarsFromAccumSaveList(Rest, DefinedList);
    false -> [{Var,Temp}|removeRedefVarsFromAccumSaveList(Rest, DefinedList)]
  end.
        
%% convert pseudo_spills to move instructions. 
secondPassDoPseudoSpill(SpillList) ->
  lists:foldl(fun convertPseudoSpillToMov/2, [], SpillList).

%% if there are variables to be restored, then call addRestoreBlockToEdge to 
%% place them in a new block on the edge of the blocks.
secondPassDoPseudoCall(I, RestoreList, CFG) ->
  ContLabel = I#pseudo_call.contlab,
  SizeOfSet = setSize(RestoreList),
  if SizeOfSet =/= 0 ->
      addRestoreBlockToEdge(I, ContLabel, CFG, RestoreList);
     true ->
      {CFG, I}
  end.

%% prepares the moves for the spills.    
convertPseudoSpillToMov({Temp, NewTemp}, OtherMoves) ->
  OtherMoves ++ [mkMove(Temp, NewTemp)].

%% prepares the moves for the restores.
%% Called by addRestoreBlockToEdge while introducing the restores.
convertPseudoRestoreToMov({Temp, NewTemp}, OtherMoves) ->
  OtherMoves ++ [mkMove(NewTemp, Temp)].

%% makes the move record, special care is taken for doubles.    
mkMove(NewTemp,Temp) ->
  if Temp#x86_temp.type =:= 'double' ->
      hipe_x86:mk_fmove(NewTemp, Temp);
     true ->
      hipe_x86:mk_move(NewTemp, Temp)
  end.

%% adds a new block (on the edge) that includes introduced restore moves.
addRestoreBlockToEdge(PseudoCall, ContLabel, CFG, TempArgsList) ->
  NextLabel = hipe_gensym:get_next_label(x86),
  NewCode = lists:foldl(fun convertPseudoRestoreToMov/2, [], TempArgsList) ++ [hipe_x86:mk_jmp_label(ContLabel)],
  NewBlock = hipe_bb:mk_bb(NewCode),
  NewPseudoCall = redirect_pseudo_call(PseudoCall, ContLabel, NextLabel),
  NewCFG = hipe_x86_cfg:bb_add(CFG, NextLabel, NewBlock),
  {NewCFG, NewPseudoCall}.

%% used instead of hipe_x86_cfg:redirect_jmp since it does not handle
%% pseudo_call calls.
redirect_pseudo_call(I = #pseudo_call{contlab=ContLabel}, Old, New) ->
  case Old =:= ContLabel of
    true  -> I#pseudo_call{contlab=New};
    false -> I
  end.

temp_is_pseudo(Temp) ->
  case hipe_x86:is_temp(Temp) of
    true  -> not(?HIPE_X86_REGISTERS:is_precoloured(hipe_x86:temp_reg(Temp)));
    false -> false
  end.

%%---------------------------------------------------------------------
%% Set operations where the module name is an easily changeable macro
%%---------------------------------------------------------------------

union(Set1, Set2) ->
  ?SET_MODULE:union(Set1, Set2).

setSize(Set) ->
  ?SET_MODULE:size(Set).

from_list(List) ->
  ?SET_MODULE:from_list(List).

to_list(Set) ->
  ?SET_MODULE:to_list(Set).

subtract(Set1, Set2) ->
  ?SET_MODULE:subtract(Set1, Set2).

intersection(Set1, Set2) ->
  ?SET_MODULE:intersection(Set1, Set2). 

is_element(Element, Set) ->
  ?SET_MODULE:is_element(Element, Set).
