%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : hipe_ssa.inc
%% Authors : Christoffer Vikström, Daniel Deogun, and Jesper Bengtsson
%% Created : March 2002
%% Purpose : Provides code which converts the code of a CFG into SSA
%%           (Static Single Assignment) form and back.
%%           A routine to check for SSA-ness is also provided.
%%
%% Major Modifications:
%%   * Feb 2003: Per Gustafsson - added SSA checker.
%%   * Aug 2003: Per Gustafsson - added removal of dead code.
%%   * Feb 2004: Kostis Sagonas - made it work on RTL level too.
%%   * Feb 2004: Tobias Lindahl - re-wrote the unconvert/1 function.
%%----------------------------------------------------------------------

-export([convert/1, check/1, unconvert/1, remove_dead_code/1]).

-include("../main/hipe.hrl").
-include("../flow/cfg.hrl").              %% needed for the specs
-include("../ssa/hipe_ssa_liveness.inc"). %% needed for dead code removal

%%----------------------------------------------------------------------
%%
%% NOTE! When the phi-instructions are placed, it is important that
%% the internal order is preserved. Otherwise the (correct) order:
%%
%% v1 := phi({1, v2}, {2, v11})
%% v2 := phi({1, v11}, {2, v12})
%%
%% can become (the incorrect)
%%
%% v2 := phi({1, v11}, {2, v12})
%% v1 := phi({1, v2}, {2, v11})
%%
%% that will set v1 to the _new_ value of v2 instead of the old value.
%%
%%----------------------------------------------------------------------

-spec convert(#cfg{}) -> #cfg{}.

convert(CFG) ->
  CFG1 = insertNewStartNode(CFG),

  ?opt_start_timer("Dominator Tree construction"),
  DomTree = hipe_dominators:domTree_create(CFG1),
  ?opt_stop_timer("Dominator Tree construction done"),

  ?opt_start_timer("Dominance Frontier"),
  DomFrontier = hipe_dominators:domFrontier_create(CFG1, DomTree),
  ?opt_stop_timer("Dominance Frontier done"),

  ?opt_start_timer("placement of Phi-nodes"),
  CFG2 = place_phi(CFG1, DomFrontier),
  ?opt_stop_timer("placement of Phi-nodes done"),

  ?opt_start_timer("Rename"),
  CFG3 = rename(CFG2, DomTree),
  ?opt_stop_timer("Rename done"),

  CFG3.
     
%%----------------------------------------------------------------------

insertNewStartNode(CFG) ->
  StartLabel = ?CFG:start_label(CFG),
  NewStartLabel = ?CODE:label_name(?CODE:mk_new_label()), 
  BB = hipe_bb:mk_bb([?CODE:mk_goto(StartLabel)]),
  CFG2 = ?CFG:bb_add(CFG, NewStartLabel, BB),
  ?CFG:start_label_update(CFG2, NewStartLabel).


%%======================================================================
%% PlacePhi Algorithm
%%======================================================================

%%----------------------------------------------------------------------
%% Procedure : place_phi/2 
%% Purpose   : Places phi nodes at appropriate places in the CFG.
%% Arguments : CFG - Control Flow Graph.
%%             DF  - Dominance Frontier.
%% Returns   : CFG with phi functions.
%%----------------------------------------------------------------------

place_phi(CFG, DF) ->
  AssMap = insertParams(CFG),
  AssMap2 = preProcess(CFG, AssMap),
  VarList = gb_trees:to_list(AssMap2),
  Liveness = ?LIVENESS:analyze(CFG),
  variableTraverse(CFG, DF, gb_trees:empty(), gb_trees:empty(), 
		   0, AssMap2, Liveness, VarList).
    
%%----------------------------------------------------------------------
%% Procedure : insertParams/1 
%% Purpose   : Inserts the parameters of the CFG into the AssMap.
%% Arguments : CFG - Control Flow Graph
%% Returns   : AssMap - Assignment map.
%%----------------------------------------------------------------------

insertParams(CFG) ->
  StartLabel = ?CFG:start_label(CFG),
  Params = ?CFG:params(CFG),
  insertParams(Params, StartLabel, gb_trees:empty()).

insertParams([Param|T], StartLabel, AssMap) ->
  insertParams(T, StartLabel, gb_trees:insert(Param, [StartLabel], AssMap));
insertParams([], _, AssMap) -> AssMap.

%%----------------------------------------------------------------------
%% Procedure : preProcessg/2
%% Purpose   : Creates the assignment map.
%% Arguments : CFG     - Control Flow Graph
%%             AssMap  - Assignment map
%% Returns   : AssMap.
%%----------------------------------------------------------------------

preProcess(CFG, AssMap) -> 
  traverseLabels(CFG, ?CFG:labels(CFG), AssMap).

%%----------------------------------------------------------------------
%% Procedure : traverseLabels/3
%% Purpose   : Traverses all labels and adds all assignments in the basic
%%             block to the assignment map.
%% Arguments : CFG    - Control Flow Graph
%%             AssMap - Assignment Map
%%             Label  - A label for a node
%% Returns   : AssMap. 
%%----------------------------------------------------------------------

traverseLabels(CFG, [Label|T], AssMap) ->
  Code = get_code_from_label(CFG, Label),
  NewVarList = getAssignments(Code),
  traverseLabels(CFG, T, updateAssMap(NewVarList, Label, AssMap)); 
traverseLabels(_, [], AssMap) -> AssMap. 

%%----------------------------------------------------------------------
%% Procedure : getAssignments/1
%% Purpose   : Retrieves all assigned variables in a basic block.
%% Arguments : InstrLst - A list of instructions from a basic block.
%%             VarList  - A list of variables.
%% Returns   : VarList.
%% Notes     : This function may return a list containing duplicates.
%%----------------------------------------------------------------------

getAssignments(InstrList) -> getAssignments(InstrList, []).

getAssignments([Instr|T], VarList) ->
  getAssignments(T, defs_to_rename(Instr) ++ VarList);
getAssignments([], VarList) -> VarList.

%%----------------------------------------------------------------------
%% Procedure : updateAssMap/3
%% Purpose   : Updates the assignment map with. Each variable in the AssVar
%%             list is inserted with the value Label.
%% Arguments : Label  - a label of a node
%%             AssVar - a variable that is assigned at Label
%%             AssMap - Assignment map.
%% Returns   : AssMap.
%%----------------------------------------------------------------------

updateAssMap([AssVar|T], Label, AssMap) ->
  Lst = getAssMap(AssVar, AssMap),
  updateAssMap(T, Label, gb_trees:enter(AssVar, [Label|Lst], AssMap));
updateAssMap([], _, AssMap) -> AssMap.    

getAssMap(AssVar, AssMap) ->
  case gb_trees:lookup(AssVar, AssMap) of
    {value, L} -> L;
    none -> []
  end.

%%----------------------------------------------------------------------
%% Procedure : variableTraverse/7
%% Purpose   : This function traverses all variables and adds phi functions 
%%             at appropriate nodes.
%% Arguments : CFG        - Control Flow Graph
%%             DFMap      - Dominance Frontier Map
%%             HasAlready - A map of nodes which already have phi functions
%%             Work       - 
%%             IterCount  - Counter of how many iterations have been done
%%             AssMap     - Assignment map
%%             VarLst     - Variable list that is traversed
%% Returns   : CFG.
%%----------------------------------------------------------------------

variableTraverse(CFG, DFMap, HasAlready, Work, 
		 IterCount, AssMap, Liveness, [{Var,_}|VarLst]) ->
  IterCount2 = IterCount + 1,	
  DefLst = getAssMap(Var, AssMap),
  {Work2, WorkLst2} = workListBuilder(DefLst, Work, [], IterCount2),
  {CFG2, HasAlready2, Work3} = doWork(CFG, DFMap, HasAlready, 
				      Work2, IterCount2, WorkLst2,
				      Var, Liveness),
  variableTraverse(CFG2, DFMap, HasAlready2, Work3, 
		   IterCount2, AssMap, Liveness, VarLst);
variableTraverse(CFG, _, _, _, _, _, _, []) -> CFG.

%%----------------------------------------------------------------------
%% Procedure : workListBuilder/4
%% Purpose   : Builds the worklist that the algorithm is working on.
%% Arguments : Work       - 
%%             WorkLst    - The worklist that is worked through
%%             IterCount  - Counter of how many itterations that has been done
%%             Node       - A node in the CFG
%% Returns   : 
%%----------------------------------------------------------------------

workListBuilder([Node|T], Work, WorkLst, IterCount) ->
  case getCount(Node, Work) of
    0 ->
      Work2 = gb_trees:enter(Node, IterCount, Work),
      workListBuilder(T, Work2, [Node|WorkLst], IterCount);
    _ ->
      Work2 = gb_trees:enter(Node, IterCount, Work),
      workListBuilder(T, Work2, [Node|WorkLst], IterCount)
  end;
workListBuilder([], Work, WorkLst, _IterCount) ->
  {Work, WorkLst}.

getCount(Key, Dict) ->
  case gb_trees:lookup(Key, Dict) of
    {value, V} -> V;
    none -> 0
  end.

%%----------------------------------------------------------------------
%% Procedure : doWork/7
%% Purpose   : This procedure works itself through the worklist and checks
%%             if a node needs any phi functions.
%% Arguments : CFG        - Control Flow Graph
%%             DFMap      - Dominance Frontier Map
%%             HasAlready - A map of nodes that already have phi functions
%%             Work       - 
%%             IterCount  - Counter of how many iterations have taken place
%%             WorkLst    - The worklist that is worked through
%%             Var        - Variable
%% Returns   : {CFG, HasAlready, Work}
%%----------------------------------------------------------------------   

doWork(CFG, DFMap, HasAlready, Work, IterCount,
       [Node|WorkLst], Var, Liveness) ->
  DFofX = hipe_dominators:domFrontier_get(Node, DFMap),
  {CFG2, HasAlready2, Work2, WorkLst2} =
    checkPhiNeeds(CFG, DFofX, HasAlready, Work,
		  IterCount, WorkLst, Var, Liveness),
  doWork(CFG2, DFMap, HasAlready2, Work2,
	 IterCount, WorkLst2, Var, Liveness);
doWork(CFG, _, HasAlready, Work, _, [], _, _) ->
  {CFG, HasAlready, Work}.    

%%----------------------------------------------------------------------
%% Procedure : checkPhiNeeds/7
%% Purpose   : This function checks if a node needs a phi function and adds
%%             one if its needed.
%% Arguments : CFG        - Control Flow Graph
%%             DFofX      - Dominance Frontier of a node
%%             HasAlready - A map of nodes that already have phi functions
%%             Work       - 
%%             IterCount  - Counter of how many iterations have taken place
%%             WorkLst    - The worklist that is worked through
%%             Var        - Variable
%% Returns   : {CFG, HasAlready, Work, WorkLst}
%%----------------------------------------------------------------------

checkPhiNeeds(CFG, [Node|DFofX], HasAlready, Work,
	      IterCount, WorkLst, Var, Liveness) ->
  case getCount(Node, HasAlready) < IterCount of
    true ->
      LiveIn = ?LIVENESS:livein(Liveness, Node),
      case lists:member(Var, LiveIn) of
	true ->
	  CFG2 = insertPhiCode(CFG, Node, Var),
	  HasAlready2 = gb_trees:enter(Node, IterCount, HasAlready),
	  case getCount(Node, Work) < IterCount of
	    true ->
	      Work2 = gb_trees:enter(Node, IterCount, Work),
	      WorkLst2 = [Node|WorkLst],
	      checkPhiNeeds(CFG2, DFofX, HasAlready2, Work2, 
			    IterCount, WorkLst2, Var, Liveness);
	    false ->
	      checkPhiNeeds(CFG2, DFofX, HasAlready2, Work, 
			    IterCount, WorkLst, Var, Liveness)
	  end;
	false ->
	  checkPhiNeeds(CFG, DFofX, HasAlready, Work, IterCount, 
			WorkLst, Var, Liveness)
      end;
    false ->
      checkPhiNeeds(CFG, DFofX, HasAlready, Work, IterCount, 
		    WorkLst, Var, Liveness)
  end;
checkPhiNeeds(CFG, [], HasAlready, Work, _, WorkLst, _, _) ->
  {CFG, HasAlready, Work, WorkLst}.	

%%----------------------------------------------------------------------
%% Procedure : insertPhiCode/3
%% Purpose   : 
%% Arguments : CFG     - Control Flow Graph
%%             Node    - A node
%%             Var     - A variable
%% Returns   : CFG
%%----------------------------------------------------------------------

insertPhiCode(CFG, Node, Var) ->
  BB = ?CFG:bb(CFG, Node),
  Phi = ?CODE:mk_phi(Var),
  Code = [Phi | hipe_bb:code(BB)],
  ?CFG:bb_add(CFG, Node, hipe_bb:code_update(BB, Code)).


%%======================================================================
%% SSA Renaming pass
%%======================================================================

%%----------------------------------------------------------------------
%% Procedure : rename/2
%% Purpose   : Renames all the variables in the CFG according to the SSA
%%             conversion algorithm.
%% Arguments : CFG       - The CFG being translated.
%%             DomTree   - The dominator tree of the CFG.
%% Returns   : A CFG where all variables are renamed.
%%----------------------------------------------------------------------

rename(CFG, DomTree) ->
  %% Reset the appropriate variable index so that we start from low
  %% variable numbers again
  reset_var_indx(),
  {CFG2,Current} = insertRenamedParams(CFG),
  rename(CFG2, ?CFG:start_label(CFG2), DomTree, Current).
    
rename(CFG, Node, DomTree, Current) ->
  BB = ?CFG:bb(CFG, Node),
  Statements = hipe_bb:code(BB),
  {Statements2,Current2} = renameVars(Statements, Current),
  CFG1 = ?CFG:bb_add(CFG, Node, hipe_bb:code_update(BB, Statements2)),
  Succ = ?CFG:succ(CFG1, Node),
  CFG2 = updateSuccPhi(Succ, Node, CFG1, Current2),
  Children = hipe_dominators:domTree_getChildren(Node, DomTree),
  childrenRename(Children, CFG2, DomTree, Current2).

%%----------------------------------------------------------------------
%% Procedure : childrenRename/5
%% Purpose   : Renames all the nodes in a list according to the SSA
%%	       conversion algorithm.
%% Arguments : ChildList - the list of nodes being renamed
%%             CFG       - the CFG that the children are a part of
%%             DomTree   - The dominator tree for the CFG
%%             Current   - the current index of all variables encountered
%% Returns   : CFG
%%----------------------------------------------------------------------

childrenRename([Child|Children], CFG, DomTree, Current) ->
  CFG2 = rename(CFG, Child, DomTree, Current),
  childrenRename(Children, CFG2, DomTree, Current);
childrenRename([], CFG, _, _) ->
  CFG.

%%----------------------------------------------------------------------
%% Procedure : renameVars/3
%% Purpose   : Renames the variables in basic block
%% Arguments : Statements - the basic block
%%             Current    - the current index of all variables encountered
%% Returns   : {Statements,Current}
%%----------------------------------------------------------------------

renameVars(Statements, Current) ->    
  renameVars(Statements, Current, []).

renameVars([Statement|Statements], Current, Result) ->
  Statement2 = renameUses(Statement, Current),
  {Statement3,Current2} = renameDefs(Statement2, Current),
  renameVars(Statements, Current2, [Statement3|Result]);
renameVars([], Current, Result) -> 
  {lists:reverse(Result),Current}.

%%----------------------------------------------------------------------
%% Procedure : renameUses/2
%% Purpose   : Renames all the uses of a variable in a statement.
%% Arguments : Statement - the statement being renamed.
%%             Current   - the current index of all variables encountered.
%% Returns   : Statement
%%---------------------------------------------------------------------- 

renameUses(Statement, Current) ->
  case ?CODE:is_phi(Statement) of 
    true  -> Statement;
    false -> VarList = uses_to_rename(Statement),
	     updateStatementUses(VarList, Statement, Current)
  end.

%%----------------------------------------------------------------------
%% Procedure : updateStatementUses/3
%% Purpose   : Traverses the variable list and renames all the instances
%%             of a variable in the Statement uses to its current value.
%% Arguments : VarList   - the list of variables being updated.
%%             Statement - the statement being updated.
%%             Current   - the current index of all variables encountered.
%% Returns   : An updated statement.
%%---------------------------------------------------------------------- 

updateStatementUses(Vars, Statement, Current) ->
  Substs = [{Var,gb_trees:get(Var, Current)} || Var <- Vars],
  ?CODE:subst_uses(Substs, Statement).

%%----------------------------------------------------------------------
%% Procedure : renameDefs/3
%% Purpose   : Renames all the definitons in Statement.
%% Arguments : Statement - the statement where the definitions are being
%%             renamed.               
%%             Current   - the current index of all variables encountered.
%% Returns   : Statement
%%----------------------------------------------------------------------

renameDefs(Statement, Current) ->
  VarList = defs_to_rename(Statement),
  updateStatementDefs(VarList, Statement, Current).

%%----------------------------------------------------------------------
%% Procedure : updateStatementDefs/4
%% Purpose   : traverses a variable list and exchanges all instances of
%%             the variable in the statements definitions by its current
%%             value.
%% Arguments : VariableList - the list of varibles being renamed
%%             Statement - the statement whos definitions are being changed
%%             Current - the current index of all variables encountered
%% Returns   : {Statement, Current}
%% Notes     : Per Gustafsson:
%%             I changed this function to update the statement only when
%%             all substitutions are found.
%%----------------------------------------------------------------------

updateStatementDefs(Vars, Statement, Current) ->
  updateStatementDefs(Vars, Statement, Current, []).

updateStatementDefs([Var|Vars], Statement, Current, Acc) ->
  {NewVar,Current2} = updateIndices(Current, Var),
  updateStatementDefs(Vars, Statement, Current2, [{Var,NewVar}|Acc]);
updateStatementDefs([], Statement, Current, Acc) -> 
  Statement2 = ?CODE:subst_defines(Acc, Statement),
  {Statement2,Current}.

%%----------------------------------------------------------------------
%% Procedure : updateIndices/3
%% Purpose   : This function is used for updating the Current hash table
%%             and for getting a new variable/fp variable/register.
%% Arguments : Current  - Hash table containg the current index for a 
%%                        particular variable.
%%             Variable - The variable that is used as key in the hash table.
%% Returns   : A two-tuple containing the new variable and Current.
%%----------------------------------------------------------------------

updateIndices(Current, Variable) ->
  case ?CODE:is_var(Variable) of
    true ->
      NewVar = ?CODE:mk_new_var(),
      {NewVar,gb_trees:enter(Variable, NewVar, Current)};
    false ->
      case is_fp_temp(Variable) of
	true ->
	  NewFVar = mk_new_fp_temp(),
	  {NewFVar,gb_trees:enter(Variable, NewFVar, Current)};
	false ->
	  NewReg = ?CODE:mk_new_reg(),
	  {NewReg,gb_trees:enter(Variable, NewReg, Current)}
      end
  end.

%%----------------------------------------------------------------------
%% Procedure : updateSuccPhi/4
%% Purpose   : This function is used for updating phi functions in a 
%%             particular node's successors. That is, the function 
%%             traverses the successor list of a node and updates the 
%%             arguments in the phi function calls.
%% Arguments : Succ    - A successor to the node Parent.
%%             T       - The remainder of the successor list
%%             Parent  - The parent of the node Succ
%%             CFG     - Control Flow Graph
%%             Current - Hash table containg the current index for a 
%%                       particular variable
%% Returns   : An updated version of the CFG 
%%----------------------------------------------------------------------

updateSuccPhi([Succ|T], Parent, CFG, Current) ->
  CFG2 = updatePhi(Succ, Parent, CFG, Current),
  updateSuccPhi(T, Parent, CFG2, Current);
updateSuccPhi([], _, CFG, _) ->
  CFG.

%%----------------------------------------------------------------------
%% Procedure : updatePhi/4
%% Purpose   : This function prepares for an update of a phi function call. 
%%             That is, if a statement contains a phi function call 
%%             then the number of predecessors are computed and the index 
%%             of the parent in the predecessor list is used for computing
%%             which variable in the argument list of the phi function call
%%             that need to be updated.
%% Arguments : Node    - A node in the CFG
%%             Parent  - The parent of the node Node in the dominator tree 
%%             CFG     - Control Flow Graph
%%             Current - Hash table containg the current index for a 
%%                       particular variable
%% Returns   : An updated version of the CFG
%%----------------------------------------------------------------------

updatePhi(Node, Parent, CFG, Current) ->
  BB = ?CFG:bb(CFG, Node),
  case hipe_bb:code(BB) of
    [Code|_] = Statements ->
       case ?CODE:is_phi(Code) of
	 true ->
	   Code2 = updateCode(Statements, Parent, Current),
	   ?CFG:bb_add(CFG, Node, hipe_bb:code_update(BB, Code2)); 
	 _ ->
	   CFG
       end;
    _ ->
      CFG
  end.

%%----------------------------------------------------------------------
%% Procedure : updateCode/3
%% Purpose   : This function updates a statement that contains a phi
%%             function, i.e. it changes the arguments in the phi
%%             function to their correct names.
%% Arguments : Code    - A list of code
%%             Pred    - A predecessor of the node containing the
%%                       phi-function
%%             Current - Hash table containing the current index for a 
%%                       particular variable
%% Returns   : A list of Code
%%----------------------------------------------------------------------

updateCode(Code, Pred, Current) ->
  updateCode(Code, Pred, Current, []).

updateCode([Stat|Stats] = Statements, Pred, Current, Result) ->
  case ?CODE:is_phi(Stat) of
    true ->
      Var = ?CODE:phi_id(Stat),
      Result2 = case gb_trees:lookup(Var, Current) of
		  none ->
		    [Stat|Result];
		  {value,Var2} ->
		    Stat2 = ?CODE:phi_enter_pred(Stat, Pred, Var2),
		    [Stat2|Result]
		end,
      updateCode(Stats, Pred, Current, Result2);
    _ ->
      Result ++ Statements
  end.

%%----------------------------------------------------------------------
%% Procedure : insertRenamedParams/1
%% Purpose   : Inserts the parameters of the CFG into the working hashmaps.
%% Arguments : CFG - the target control flow graph.      
%% Returns   : {CFG,Current}
%%----------------------------------------------------------------------

insertRenamedParams(CFG) ->
  Params = ?CFG:params(CFG),
  %% Current - the current variable we are working on.
  {Current,Params2} = insertRenamedParams(Params, gb_trees:empty(), []),
  CFG2 = ?CFG:params_update(CFG, Params2),
  {CFG2,Current}.
    
insertRenamedParams([Param|Params], Current, Result) ->
  {Var,Current2} = updateIndices(Current, Param),
  insertRenamedParams(Params, Current2, [Var|Result]);
insertRenamedParams([], Current, Result) ->
  {Current,lists:reverse(Result)}.


%%======================================================================
%% SSA Checker
%%======================================================================

%%
%% @doc Checks the control flow graph CFG of a function for SSA-ness.
%% More specifically, it checks that all variables in the CFG are only
%% defined once and that all uses of each variable in the function are
%% dominated by a define. If a variable does not abide by these rules,
%% a warning message will be printed on stdout.
%%
-spec check(#cfg{}) -> 'ok'.

check(CFG) ->
  Labels  = ?CFG:labels(CFG),
  VarTree = traverse_labels(Labels, CFG),
  DomTree = hipe_dominators:domTree_create(CFG),
  test_uses(Labels, VarTree, DomTree, CFG).

%%
%% @doc Traverses all the labels in a CFG.
%%
traverse_labels(Labels, CFG) ->
  VarTree = add_args(?CFG:params(CFG)),
  traverse_labels(Labels, VarTree, CFG).

traverse_labels([Label|Rest], VarTree, CFG) ->
  Code = get_code_from_label(CFG, Label),
  NewVarTree = traverse_code(Code, VarTree, Label),
  traverse_labels(Rest, NewVarTree, CFG);
traverse_labels([], VarTree, _CFG) ->
  VarTree.

%%
%% @doc Traverses the code in a basic block.
%%
traverse_code([Instr|Rest], VarTree, Label) ->
  Defined = defs_to_rename(Instr),
  NewVarTree = add_to_var_tree(Defined, VarTree, Instr, Label), 
  traverse_code(Rest, NewVarTree, Label); 
traverse_code([], VarTree, _) ->
  VarTree.

%%
%% @doc
%%   Adds a variable to the variable tree if the variable is defined.
%% The entry in the variable tree will have the variable as key and a
%% two tuple consisting of a list of Instructions and a list of labels
%% where the variable is defined. If a variable is defined a second
%% time a warning message to this effect is printed on stdout.
%%
add_to_var_tree([Var|Rest], VarTree, Instr, Label) ->
  NewVarTree =
    case gb_trees:lookup(Var, VarTree) of
      {value,{OldInstr,OldLabel}} ->
        ?WARNING_MSG("Variable: ~w defined a second time\n"++
		     "in Instr: ~w\n"++
		     "at Label: ~w\n"++
		     "variable was first defined at Label(s) ~w\n"++
		     "in Instr(s): ~w\n -> non SSA form\n",
		     [Var,Instr,Label,OldLabel,OldInstr]),
        gb_trees:update(Var, {[Instr|OldInstr],[Label|OldLabel]}, VarTree);
      none ->
        gb_trees:insert(Var, {[Instr],[Label]}, VarTree)
    end,
  add_to_var_tree(Rest, NewVarTree, Instr, Label);
add_to_var_tree([], VarTree, _, _) ->
  VarTree.

%%
%% @doc Adds the argument of a function to the VarTree.
%% They are defined at Label 0.
%%
add_args(Args) ->
  add_args(Args, gb_trees:empty()).

add_args([Arg|Rest], VarTree) ->
  add_args(Rest, gb_trees:insert(Arg, {[argument_variable],[0]}, VarTree));
add_args([], VarTree) ->
  VarTree.

%%
%% The functions below test that a use is dominated by a corresponding def.
%%

%%
%% This function is analogous to traverse_labels.
%%
test_uses([Label|Rest], VarTree, DomTree,CFG) ->
  Code = get_code_from_label(CFG, Label),
  test_code(Code, VarTree, Label, DomTree, CFG, []),
  test_uses(Rest, VarTree, DomTree, CFG);
test_uses([], _VarTree, _DomTree, _CFG) ->
  ok.

%%
%% This function is analogous to traverse_code.
%%
test_code([Instr|Instrs], VarTree, Label, DomTree, CFG, Old) ->
  case ?CODE:is_phi(Instr) of
    true ->
      ArgList = ?CODE:phi_arglist(Instr),
      case ArgList of
	[_Arg] ->
	  ?WARNING_MSG("Phi with only one source at BB with label ~w:\n",
		       [Label]),
	  %% case ?CODE of
	  %%   hipe_rtl -> ?CODE:pp_block(get_code_from_label(CFG, Label));
	  %%   _ -> ok
	  %% end,
	  ok;
	[_|_] -> ok
      end,
      lists:foreach(fun ({Pred,Var}) ->
			def_doms_use([Var], VarTree, Pred, DomTree,
				     get_code_from_label(CFG,Pred))
		    end, ArgList);
    false ->
      Uses = uses_to_rename(Instr),
      def_doms_use(Uses, VarTree, Label, DomTree, Old)
  end,
  test_code(Instrs, VarTree, Label, DomTree, CFG, [Instr|Old]);
test_code([], _VarTree, _Label, _DomTree, _CFG, _Old) ->
  ok.

get_code_from_label(CFG, Label) ->
  case ?CFG:bb(CFG,Label) of
    not_found ->
      ?error_msg("Basic block with label ~w was not found\n", [Label]);
      %% ?EXIT('Detected serious problem in SSA form');
    BB ->
      hipe_bb:code(BB)
  end.

%%
%% This function checks whether a use is dominated by a def.
%% There are five different cases:
%% 1. A use of an argument register. This use is dominated by the def.
%% 2. Use and Def in same basic block if Use comes first this will 
%%    lead to a warning message, otherwise it is ok.
%% 3. The deinition is in a basic block that dominates the basic block
%%    of the use. This is ok.
%% 4. The definition is in a basic block that does not dominate the use.
%%    This will result in a warning message being printed.
%% 5. A use without any definition. This will result in a warning message
%%    being printed.
%%
def_doms_use([Var|Vars], VarTree, Label, DomTree, Old) ->
  case gb_trees:lookup(Var, VarTree) of
    {value,{_,[DefLabel|_]}} ->
      case DefLabel of
	0 ->
	  ok;
	Label ->
	  Fun = fun(X) -> Defs = defs_to_rename(X), 
			  lists:any(fun(Y) -> Var == Y end, Defs)
		end,
	  case lists:any(Fun, Old) of
	    true ->
	      ok;
	    false ->
	      ?WARNING_MSG("Variable : ~w used before definition in bb: ~w\n",
			   [Var,Label])
	  end;
	_ ->
	  case hipe_dominators:domTree_dominates(DefLabel, Label, DomTree) of
	    true ->
	      ok;
	    false ->
	      ?WARNING_MSG("Definition does not dominate use for variable: ~w "++
			   "at label: ~w (definition label: ~w)\n", 
			   [Var, Label, DefLabel])
	  end
      end;
    none ->
      ?WARNING_MSG("Use with no definition of variable: ~w at label: ~w\n",
		   [Var, Label])
  end,
  def_doms_use(Vars, VarTree, Label, DomTree, Old);
def_doms_use([], _VarTree, _Label, _DomTree, _Old) ->
  ok.


%%======================================================================
%% SSA Un-Converter
%%======================================================================

%%----------------------------------------------------------------------
%% Procedure : unconvert/2
%% Purpose   : Removes all phi functions and propagates all
%%             assignments up to the appropriate predecessors.
%% Arguments : CFG     - Control Flow Graph
%%             Node    - A node in the CFG
%% Returns   : CFG
%% Note      : The call to remove_trivial_bbs is needed so that moves,
%%             which are introduced in new basic blocks as part of the 
%%             un-conversion, are merged with the basic blocks of their
%%             predecessors, if possible.
%%----------------------------------------------------------------------

-spec unconvert(#cfg{}) -> #cfg{}.

unconvert(CFG) ->
  ?CFG:remove_trivial_bbs(unconvert(?CFG:reverse_postorder(CFG), CFG)).

unconvert([Node|Nodes], CFG) ->
  BB = ?CFG:bb(CFG, Node),
  Code = hipe_bb:code(BB),
  {Phis,Code2} = getPhiFuncts(Code, []),
  case Phis of
    [] -> 
      unconvert(Nodes, CFG);
    _  ->
      BB2 = hipe_bb:code_update(BB, Code2),
      CFG2 = ?CFG:bb_add(CFG, Node, BB2),
      Pred = ?CFG:pred(CFG2, Node),  
      PredMoveMap = get_moves(Pred, Phis),
      CFG3 = insert_move_bbs(PredMoveMap, Node, CFG2),
      unconvert(Nodes, CFG3)
  end;
unconvert([], CFG) ->
  CFG.

%%----------------------------------------------------------------------
%% Procedure : get_moves/2 and /3
%% Purpose   : Find the moves that corresponds to  phi-instructions of
%%             a block. Try to merge incoming edges to avoid duplicate
%%             blocks.
%% Arguments : Preds - The predecessors to this block.
%%             Phis  - The phi instructions that used to start this block. 
%% Returns   : [{ListOfMoves, [Preds]}]
%%----------------------------------------------------------------------

get_moves(Preds, Phis) ->
  get_moves(Preds, Phis, gb_trees:empty()).

get_moves([Pred|Left], Phis, Map)->
  Moves = get_moves_from_phis(Pred, Phis, []),
  NewMap = 
    case gb_trees:lookup(Moves, Map) of
      none -> gb_trees:insert(Moves, [Pred], Map);
      {value,List} -> gb_trees:update(Moves, [Pred|List], Map)
    end,
  get_moves(Left, Phis, NewMap);
get_moves([], _Phis, Map) ->
  gb_trees:to_list(Map).

%%----------------------------------------------------------------------
%% Procedure : get_moves_from_phis/3
%% Purpose   : Find all the moves that should be done in the edge 
%%             coming in from Pred.
%% Arguments : Pred - The predecessor
%%             Phis - Reverse list of phi instructions. 
%% Returns   : [{Dst,Src}] representing the move instructions;
%%                    ORDERING IS SIGNIFICANT!
%%----------------------------------------------------------------------

get_moves_from_phis(Pred, [Phi|Left], Acc) ->
  Dst = ?CODE:phi_dst(Phi),
  Src = ?CODE:phi_arg(Phi, Pred),
  NewAcc = [{Dst, Src}|Acc],
  get_moves_from_phis(Pred, Left, NewAcc);
get_moves_from_phis(_Pred, [], Acc) ->
  Acc.

%%----------------------------------------------------------------------
%% Procedure : insert_move_bbs/3
%% Purpose   : Create the bbs that contains the moves.
%% Arguments : Ordset - The move instruction tuples {Dst, Src}
%%             Preds  - The predecessors that needs the moves in Ordset
%%             Label  - The original label that contained the phis.
%%             Cfg    - The current cfg
%% Returns   : The new Cfg.
%%----------------------------------------------------------------------

insert_move_bbs([{Ordset,Preds}|Left], Label, Cfg) ->
  Code = create_moves(Ordset, []) ++ [?CODE:mk_goto(Label)],
  BB = hipe_bb:mk_bb(Code),
  NewLabel = ?CODE:label_name(?CODE:mk_new_label()),
  NewCfg1 = ?CFG:bb_add(Cfg, NewLabel, BB),
  NewCfg2 = lists:foldl(fun(X, Acc) ->
				?CFG:redirect(Acc, X, Label, NewLabel)
			end,
			NewCfg1, Preds),
  insert_move_bbs(Left, Label, NewCfg2);
insert_move_bbs([], _Label, Cfg) ->
  Cfg.
		  
create_moves([{X,X}|Left], Acc) ->
  create_moves(Left, Acc);
create_moves([{Dst,Src}|Left], Acc) ->
  create_moves(Left, [makePhiMove(Dst, Src)|Acc]);
create_moves([], Acc) ->
  %% NOTE: ORDERING IS SIGNIFICANT!
  lists:reverse(Acc).

%%----------------------------------------------------------------------
%% Procedure : getPhiFuncts/2
%% Purpose   : This function returns the list of phi-functions from a
%%             list of intermediate code instructions.
%% Arguments : 
%%             List   - A list of Code
%%             Result - Accumulative parameter to store the result
%% Returns   : Reverse list of the phi instructions. ORDERING IS SIGNIFICANT!
%%----------------------------------------------------------------------

getPhiFuncts([I|T] = List, Result) ->
  case ?CODE:is_phi(I) of
    true ->
      getPhiFuncts(T, [I|Result]);
    false ->
      {Result,List}
  end;
getPhiFuncts([], Result) ->  
  {Result,[]}.


%%======================================================================
%% Dead Code Elimination on SSA form
%%======================================================================

-spec remove_dead_code(#cfg{}) -> #cfg{}.

remove_dead_code(CFG) ->
  Lbls = ?CFG:reverse_postorder(CFG),  
  Liveness = ssa_liveness__analyze(CFG),
  case do_lbls(Lbls, CFG, Liveness, false) of
    {CFG1,true} ->
      remove_dead_code(CFG1);
    {CFG1,false} ->
      CFG1
  end.

do_lbls([Lbl|Rest], CFG, Liveness, Changed) ->
  LiveOut = gb_sets:from_list(ssa_liveness__liveout(Liveness, Lbl)),
  BB = ?CFG:bb(CFG, Lbl),
  Code = hipe_bb:code(BB),
  {NewCode,NewChanged} = do_code(lists:reverse(Code), LiveOut, Changed, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCFG = ?CFG:bb_add(CFG, Lbl, NewBB),
  do_lbls(Rest, NewCFG, Liveness, NewChanged);
do_lbls([], CFG, _Liveness, Changed) ->
  {CFG,Changed}.

do_code([Instr|Instrs], LiveOut, Changed, Acc) ->
  Def = ?CODE:defines(Instr),
  Use = ?CODE:uses(Instr),
  DefSet = gb_sets:from_list(Def),
  UseSet = gb_sets:from_list(Use),
  LiveIn = gb_sets:union(gb_sets:difference(LiveOut, DefSet), UseSet),
  case gb_sets:is_empty(gb_sets:intersection(DefSet, LiveOut)) of
    false ->
      do_code(Instrs, LiveIn, Changed, [Instr|Acc]);
    true ->
      case ?CODE:is_safe(Instr) of
	true ->
	  case ?CODE:is_call(Instr) of
	    true ->
	      case ?CODE:call_continuation(Instr) of
		[] ->
		  do_code(Instrs, LiveOut, true, Acc);
		SuccLblName ->
		  NewInstr = ?CODE:mk_goto(SuccLblName),
		  do_code(Instrs, LiveOut, true, [NewInstr|Acc])
	      end;
	    false ->
	      do_code(Instrs, LiveOut, true, Acc)
	  end;
	false -> %% not a safe instruction - cannot be removed
	  case ?CODE:is_call(Instr) of
	    true ->
	      case ?CODE:call_dstlist(Instr) of
	        [] ->  %% result was not used anyway; no change
		  do_code(Instrs, LiveIn, Changed, [Instr|Acc]);
		[_Dst] -> %% remove the unused assignment to call's destination
		  NewInstr = ?CODE:call_dstlist_update(Instr, []),
		  do_code(Instrs, LiveIn, true, [NewInstr|Acc]);
		[_|_] ->  %% calls with multiple dests are left untouched
		  do_code(Instrs, LiveIn, Changed, [Instr|Acc])
	      end;
	    false ->
	      do_code(Instrs, LiveIn, Changed, [Instr|Acc])
	  end
      end
  end;
do_code([], _LiveOut, Changed, Acc) ->
  {Acc,Changed}.

