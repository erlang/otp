%% -*- Erlang -*-
%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			 CONTROL FLOW GRAPHS
%%
%% Construct and manipulate the control flow graph of a function (program?).
%%
%% Exports:
%% ~~~~~~~~
%%  init(Code) - makes a CFG out of code.
%%  bb(CFG, Label) - returns the basic block named 'Label' from the CFG.
%%  bb_add(CFG, Label, NewBB) - makes NewBB the basic block associated
%%       with Label.
%%  succ(Map, Label) - returns a list of successors of basic block 'Label'.
%%  pred(Map, Label) - returns the predecessors of basic block 'Label'.
%%  fallthrough(CFG, Label) - returns fall-through successor of basic 
%%       block 'Label' (or 'none').
%%  conditional(CFG, Label) - returns conditional successor (or 'none')
%%  start_label(CFG) - returns the label of the entry basic block.
%%  params(CFG) - returns the list of parameters to the CFG.
%%  labels(CFG) - returns a list of labels of all basic blocks in the CFG.
%%  postorder(CFG) - returns a list of labels in postorder.
%%  reverse_postorder(CFG) - returns a list of labels in reverse postorder.
%%  cfg_to_linear(CFG) - converts CFG to linearized code.
%%  remove_trivial_bbs(CFG) - removes empty BBs or BBs with only a goto.
%%  remove_unreachable_code(CFG) - removes unreachable BBs.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TODO:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=====================================================================
%% The following are ugly as hell, but what else can I do???
%%=====================================================================

-ifdef(GEN_CFG).
-define(PRED_NEEDED,true).
-endif.

-ifdef(ICODE_CFG).
-define(CLOSURE_ARITY_NEEDED,true).
-define(INFO_NEEDED,true).
-define(PARAMS_NEEDED,true).
-define(PARAMS_UPDATE_NEEDED,true).
-define(PRED_NEEDED,true).
-define(REMOVE_TRIVIAL_BBS_NEEDED,true).
-define(REMOVE_UNREACHABLE_CODE,true).
-define(START_LABEL_UPDATE_NEEDED,true).
-define(CFG_CAN_HAVE_PHI_NODES,true).
-endif.

-ifdef(RTL_CFG).
-define(PREORDER,true).
-define(FIND_NEW_LABEL_NEEDED,true).
-define(INFO_NEEDED,true).
-define(PARAMS_NEEDED,true).
-define(PARAMS_UPDATE_NEEDED,true).
-define(PRED_NEEDED,true).
-define(REMOVE_TRIVIAL_BBS_NEEDED,true).
-define(REMOVE_UNREACHABLE_CODE,true).
-define(START_LABEL_UPDATE_NEEDED,true).
-define(CFG_CAN_HAVE_PHI_NODES,true).
-endif.

-ifdef(SPARC_CFG).
-define(BREADTH_ORDER,true). % for linear scan
-define(PARAMS_NEEDED,true).
-define(START_LABEL_UPDATE_NEEDED,true).
-endif.

%%=====================================================================

-ifdef(START_LABEL_UPDATE_NEEDED).
-export([start_label_update/2]).
-endif.

-ifdef(BREADTH_ORDER).
-export([breadthorder/1]).
-endif.

-compile(inline).

%%=====================================================================
%%
%% Interface functions that MUST be implemented in the including file:
%%
%% linear_to_cfg(LinearCode) -> CFG, constructs the cfg.
%% is_label(Instr) -> boolean(), true if instruction is a label.
%% label_name(Instr) -> term(), the name of a label.
%% branch_successors(Instr) -> [term()], the successors of a branch.
%% fails_to(Instr) -> [term()], the fail-successors of an instruction.
%% is_branch(Instr) -> boolean(), true if instruction is a branch.
%% is_comment(Instr) -> boolean(),
%%       true if instruction is a comment, used by remove dead code.
%% is_goto(Instr) -> boolean(),
%%       true if instruction is a pure goto, used by remove dead code.
%% redirect_jmp(Jmp, ToOld, ToNew) -> NewJmp, 
%% redirect_ops(Labels, CFG, Map) -> CFG.
%%       Rewrite instructions with labels in operands to use
%%       the new label as given by map.
%%       Use find_new_label(OldLab,Map) to get the new label.
%%       (See hipe_sparc_cfg for example)
%% pp(CFG) -> ok, do some nifty output.
%% cfg_to_linear(CFG) -> LinearCode, linearizes the code of CFG
%% mk_goto(Label) -> instruction
%% is_phi(Instr) -> boolean(), true if the instruction is a phi-instruction.
%% phi_remove_pred(PhiInstr, Pred) -> NewPhi, 
%%       Removes the predecessor Pred from the phi instruction.
%% highest_var(Code) -> term(),
%%       Returns the highest variable used or defined in the code.
%%
%%=====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Primitives (not all of these are exported)
%%

-spec start_label(cfg()) -> cfg_lbl().
start_label(CFG) -> (CFG#cfg.info)#cfg_info.start_label.

-ifndef(GEN_CFG).
-spec start_label_update(cfg(), cfg_lbl()) -> cfg().
start_label_update(CFG, NewStartLabel) ->
  Info = CFG#cfg.info,
  CFG#cfg{info = Info#cfg_info{start_label = NewStartLabel}}.

-spec function(cfg()) -> mfa().
function(CFG) -> (CFG#cfg.info)#cfg_info.'fun'.

-spec is_closure(cfg()) -> boolean().
is_closure(CFG) -> (CFG#cfg.info)#cfg_info.is_closure.

-spec is_leaf(cfg()) -> boolean().
is_leaf(CFG) -> (CFG#cfg.info)#cfg_info.is_leaf.

mk_empty_cfg(Fun, StartLbl, Data, Closure, Leaf, Params) ->
  Info = #cfg_info{'fun' = Fun,
		   start_label = StartLbl,
		   is_closure = Closure,
		   is_leaf = Leaf,
		   params = Params},
  #cfg{table = gb_trees:empty(), data = Data, info = Info}.

data(CFG) -> CFG#cfg.data.
-endif.	% GEN_CFG

-ifdef(REMOVE_TRIVIAL_BBS_NEEDED).
-spec update_data(cfg(), cfg_data()) -> cfg().
update_data(CFG, D) ->
  CFG#cfg{data = D}.
-endif.

-ifdef(PARAMS_NEEDED).
params(CFG) -> (CFG#cfg.info)#cfg_info.params.
-endif.

-ifdef(PARAMS_UPDATE_NEEDED).
params_update(CFG, NewParams) ->
  Info = CFG#cfg.info,
  CFG#cfg{info = Info#cfg_info{params = NewParams}}.
-endif.

-ifdef(CLOSURE_ARITY_NEEDED).
-spec closure_arity(cfg()) -> arity().
closure_arity(CFG) ->
  Info = CFG#cfg.info,
  Info#cfg_info.closure_arity.

-spec closure_arity_update(cfg(), arity()) -> cfg().
closure_arity_update(CFG, Arity) ->
  Info = CFG#cfg.info,
  CFG#cfg{info = Info#cfg_info{closure_arity = Arity}}.
-endif.

%% %% Don't forget to do a start_label_update if necessary.
%% update_code(CFG, NewCode) ->
%%   take_bbs(NewCode, CFG).

-ifdef(INFO_NEEDED).
info(CFG) -> (CFG#cfg.info)#cfg_info.info.
%% info_add(CFG, A) ->
%%    As = info(CFG),
%%    Info = CFG#cfg.info,
%%    CFG#cfg{info = Info#cfg_info{info = [A|As]}}.
info_update(CFG, I) ->
  Info = CFG#cfg.info,
  CFG#cfg{info = Info#cfg_info{info = I}}.
-endif.

%%=====================================================================
-ifndef(GEN_CFG).

-spec other_entrypoints(cfg()) -> [cfg_lbl()].
%% @doc Returns a list of labels that are refered to from the data section.

other_entrypoints(CFG) ->
  hipe_consttab:referred_labels(data(CFG)).

%% is_entry(Lbl, CFG) ->
%%   Lbl =:= start_label(CFG) orelse
%% 	lists:member(Lbl, other_entrypoints(CFG)).

%% @spec bb(CFG::cfg(), Label::cfg_lbl()) -> basic_block()
%% @doc  Returns the basic block of the CFG which begins at the Label.
bb(CFG, Label) ->
  HT = CFG#cfg.table,
  case gb_trees:lookup(Label, HT) of
    {value, {Block,_Succ,_Pred}} ->
      Block;
    none ->
      not_found
  end.

%% Remove duplicates from a list. The first instance (in left-to-right
%% order) of an element is kept, remaining instances are removed.
-spec remove_duplicates([cfg_lbl()]) -> [cfg_lbl()].
remove_duplicates(List) ->
  remove_duplicates(List, []).

-spec remove_duplicates([cfg_lbl()], [cfg_lbl()]) -> [cfg_lbl()].
remove_duplicates([H|T], Acc) ->
  NewAcc =
    case lists:member(H, Acc) of
      false -> [H|Acc];
      true -> Acc
    end,
  remove_duplicates(T, NewAcc);
remove_duplicates([], Acc) ->
  lists:reverse(Acc).


-ifdef(RTL_CFG).	%% this could be CFG_CAN_HAVE_PHI_NODES
			%% if Icode also starts using this function

%% @spec bb_insert_between(CFG::cfg(),
%%                         Label::cfg_lbl(), NewBB::basic_block(),
%%                         Pred::cfg_lbl(), Succ::cfg_lbl()) -> cfg() 
%%
%% @doc Insert the new basic block with label Label in the edge from
%%      Pred to Succ

bb_insert_between(CFG, Label, NewBB, Pred, Succ) ->
  Last = hipe_bb:last(NewBB),
  %% Asserts that NewBB ends in a label
  true = is_branch(Last),
  %% Asserts that the only Successor of NewBB is Succ
  [Succ] = remove_duplicates(branch_successors(Last)),
  HT = CFG#cfg.table,
  %% Asserts that Label does not exist in the CFG
  none = gb_trees:lookup(Label, HT),
  %% Updates the predecessor of NewBB
  {value, {PBB, PSucc, PPred}} = gb_trees:lookup(Pred, HT),
  NewPSucc = [Label|lists:delete(Succ, PSucc)],
  PLast = hipe_bb:last(PBB),
  PButLast = hipe_bb:butlast(PBB),
  NewPBB = hipe_bb:code_update(PBB, PButLast++[redirect_jmp(PLast, Succ, Label)]),
  HT1 = gb_trees:update(Pred, {NewPBB,NewPSucc,PPred}, HT),
  %% Updates the successor of NewBB
  {value, {SBB, SSucc, SPred}} = gb_trees:lookup(Succ, HT1),
  NewSPred = [Label|lists:delete(Pred, SPred)],
  SCode = hipe_bb:code(SBB),
  NewSCode = redirect_phis(SCode, Pred, Label, []),
  NewSBB = hipe_bb:code_update(SBB, NewSCode),
  HT2 = gb_trees:update(Succ, {NewSBB,SSucc,NewSPred}, HT1),
  %% Enters NewBB into the CFG
  HT3 = gb_trees:insert(Label, {NewBB,[Succ],[Pred]}, HT2), 
  CFG#cfg{table = HT3}.

redirect_phis([], _OldPred, _NewPred, Acc) ->
  lists:reverse(Acc);
redirect_phis([I|Rest], OldPred, NewPred, Acc) ->
  case is_phi(I) of
    true ->
      Phi = phi_redirect_pred(I, OldPred, NewPred),
      redirect_phis(Rest, OldPred, NewPred, [Phi|Acc]);
    false ->
      redirect_phis(Rest, OldPred, NewPred, [I|Acc])
  end.

-endif.

%% @spec bb_add(CFG::cfg(), Label::cfg_lbl(), NewBB::basic_block()) -> cfg()
%% @doc  Adds a new basic block to a CFG (or updates an existing block).
bb_add(CFG, Label, NewBB) ->
  %% Asserting that the NewBB is a legal basic block
  Last = hipe_bb:last(NewBB),
  case is_branch(Last) of
    true  -> ok;
    false -> throw({?MODULE, {"Basic block ends without branch", Last}})
  end,
  %% The order of the elements from branch_successors/1 is
  %% significant. It determines the basic block order when the CFG is
  %% converted to linear form. That order may have been tuned for
  %% branch prediction purposes.
  Succ = remove_duplicates(branch_successors(Last)),
  HT = CFG#cfg.table,
  {OldSucc, OldPred} = case gb_trees:lookup(Label, HT) of
			 {value, {_Block, OSucc, OPred}} ->
			   {OSucc, OPred};
			 none ->
		           {[], []}
		       end,
  %% Change this block to contain new BB and new successors, but keep
  %% the old predecessors which will be updated in the following steps
  HT1 = gb_trees:enter(Label, {NewBB, Succ, OldPred}, HT),
  %% Add this block as predecessor to its new successors
  HT2 = lists:foldl(fun (P, HTAcc) ->
		      add_pred(HTAcc, P, Label)
		    end,
		    HT1, Succ -- OldSucc),
  %% Remove this block as predecessor of its former successors
  HT3 = lists:foldl(fun (S, HTAcc) ->
		      remove_pred(HTAcc, S, Label)
		    end,
		    HT2, OldSucc -- Succ),
  CFG#cfg{table = HT3}.

remove_pred(HT, FromL, PredL) ->
  case gb_trees:lookup(FromL, HT) of
    {value, {Block, Succ, Preds}} ->
      Code = hipe_bb:code(Block),
      NewCode = remove_pred_from_phis(Code, PredL, []),
      NewBlock = hipe_bb:code_update(Block, NewCode),      
      gb_trees:update(FromL, {NewBlock,Succ,lists:delete(PredL,Preds)}, HT);
    none ->
      HT
  end.

add_pred(HT, ToL, PredL) ->
  case gb_trees:lookup(ToL, HT) of
    {value,{Block,Succ,Preds}} ->
      gb_trees:update(ToL, {Block,Succ,[PredL|lists:delete(PredL,Preds)]}, HT);
    none ->
      gb_trees:insert(ToL, {[],[],[PredL]}, HT)
  end.

%% find_highest_label(CFG) ->
%%   Labels = labels(CFG),
%%   lists:foldl(fun(X, Acc) -> erlang:max(X, Acc) end, 0, Labels).
%% 
%% find_highest_var(CFG) ->
%%   Labels = labels(CFG),
%%   Fun = fun(X, Max) ->
%% 	    Code = hipe_bb:code(bb(CFG, X)),
%% 	    NewMax = highest_var(Code),
%% 	    erlang:max(Max, NewMax)
%% 	end,
%%   lists:foldl(Fun, 0, Labels).

-ifdef(CFG_CAN_HAVE_PHI_NODES).
%% phi-instructions in a removed block's successors must be aware of
%% the change.
remove_pred_from_phis(List = [I|Left], Label, Acc) ->
  case is_phi(I) of
    true -> 
      NewAcc = [phi_remove_pred(I, Label)|Acc],
      remove_pred_from_phis(Left, Label, NewAcc);
    false ->
      lists:reverse(Acc) ++ List
  end;
remove_pred_from_phis([], _Label, Acc) ->
  lists:reverse(Acc).
-else.
%% this is used for code representations like those of back-ends which
%% do not have phi-nodes.
remove_pred_from_phis(Code, _Label, _Acc) ->
  Code.
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Constructs a CFG from a list of instructions.
%%

take_bbs([], CFG) ->
  CFG;
take_bbs(Xs, CFG) ->
  Lbl = hd(Xs),
  case is_label(Lbl) of
    true ->
      case take_bb(tl(Xs), []) of
	{Code, Rest} ->
	  NewCFG = bb_add(CFG, label_name(Lbl), hipe_bb:mk_bb(Code)),
	  take_bbs(Rest, NewCFG)
      end;
    false ->
      erlang:error({?MODULE,"basic block doesn't start with a label",Xs})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Take_bb returns:
%%   - {Code, Rest}.
%%      * Code is a list of all the instructions.
%%      * Rest is the remainder of the instructions

take_bb([], Code) ->
  {lists:reverse(Code), []};
take_bb([X, Y|Xs], Code) ->
  case is_label(X) of
    true -> %% Empty block fallthrough
      {[mk_goto(label_name(X))], [X,Y|Xs]};
    false ->
      case is_branch(X) of
	true ->
	  case is_label(Y) of
	    true ->
	      {lists:reverse([X|Code]), [Y|Xs]};
	    false ->
	      %% This should not happen...
	      %% move the problem to the next BB.
	      {lists:reverse([X|Code]), [Y|Xs]}
	  end;
	false -> %% X not branch
	  case is_label(Y) of
	    true ->
	      {lists:reverse([mk_goto(label_name(Y)),X|Code]), [Y|Xs]};
	    false ->
	      take_bb([Y|Xs], [X|Code])
	  end
      end
  end;
take_bb([X], []) ->
  case is_label(X) of 
    true -> 
      %% We don't want the CFG to just end with a label...
      %% We loop forever instead...
      {[X,mk_goto(label_name(X))],[]};
    false ->
      {[X],[]}
  end;
take_bb([X], Code) ->
  case is_label(X) of 
    true -> 
      %% We don't want the CFG to just end with a label...
      %% We loop for ever instead...
      {lists:reverse(Code),[X,mk_goto(label_name(X))]};
    false ->
      {lists:reverse([X|Code]),[]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Functions for extracting the names of the basic blocks in various
%% orders.
%%

labels(CFG) ->
  HT = CFG#cfg.table,
  gb_trees:keys(HT).

postorder(CFG) ->
  lists:reverse(reverse_postorder(CFG)).

reverse_postorder(CFG) ->
  Start = start_label(CFG),
  {Ordering, _Visited} =
    depth_search([Start|other_entrypoints(CFG)], none_visited(), CFG, []),
  Ordering.

depth_search([N|Ns], Visited, CFG, Acc) ->
  case is_visited(N, Visited) of 
    true ->
      depth_search(Ns, Visited, CFG, Acc);
    false ->
      {Order, Vis} = depth_search(succ(CFG, N), visit(N, Visited), CFG, Acc),
      depth_search(Ns, Vis, CFG, [N|Order])
  end;
depth_search([], Visited, _, Ordering) ->
  {Ordering, Visited}.

-ifdef(PREORDER).
preorder(CFG) ->
  Start = start_label(CFG),
  {Ordering, _Visited} =
    preorder_search([Start|other_entrypoints(CFG)], none_visited(), CFG, []),
  lists:reverse(Ordering).

preorder_search([N|Ns], Visited, CFG, Acc) ->
  case is_visited(N, Visited) of 
    true ->
      preorder_search(Ns, Visited, CFG, Acc);
    false ->
      {Order, Vis} = 
	preorder_search(succ(CFG, N), visit(N, Visited), CFG, [N|Acc]),
      preorder_search(Ns, Vis, CFG, Order)
  end;
preorder_search([], Visited, _, Ordering) ->
  {Ordering,Visited}.
-endif.	% PREORDER

-ifdef(BREADTH_ORDER).
breadthorder(CFG) ->
  lists:reverse(reverse_breadthorder(CFG)).

reverse_breadthorder(CFG) ->
  Start = start_label(CFG),
  {Vis, RBO1} = breadth_list([Start], none_visited(), CFG, []),
  {_Vis1, RBO2} = breadth_list(other_entrypoints(CFG), Vis, CFG, RBO1),
  RBO2.

breadth_list([X|Xs], Vis, CFG, BO) ->
  case is_visited(X, Vis) of
    true ->
      breadth_list(Xs, Vis, CFG, BO);
    false ->
      breadth_list(Xs ++ succ(CFG, X), visit(X, Vis), CFG, [X|BO])
  end;  
breadth_list([], Vis, _CFG, BO) -> 
  {Vis, BO}.
-endif.

-spec none_visited() -> gb_sets:set().
none_visited() -> 
  gb_sets:empty().

visit(X, Vis) -> 
  gb_sets:add(X, Vis).

is_visited(X, Vis) ->
  gb_sets:is_member(X, Vis).

-endif.	% GEN_CFG

%%---------------------------------------------------------------------

succ(SuccMap, Label) ->
  HT = SuccMap#cfg.table,
  case gb_trees:lookup(Label, HT) of
    {value, {_Block,Succ,_Pred}} ->
      Succ;
    none ->
      erlang:error({"successor not found", Label, SuccMap})
  end.

-ifdef(PRED_NEEDED).
pred(Map, Label) ->
  HT = Map#cfg.table,
  case gb_trees:lookup(Label, HT) of
    {value, {_Block,_Succ,Pred}} ->
      Pred;
    none ->
      erlang:error({"predecessor not found", Label, Map})
  end.
-endif.	% PRED_NEEDED

-ifndef(GEN_CFG).
fallthrough(CFG, Label) ->
  HT = CFG#cfg.table,
  case gb_trees:lookup(Label, HT) of
    {value, {_Block, Succ, _}} ->
       case Succ of
	 [X|_] -> X;
	 _ -> none
       end;
     none ->
       erlang:error({"fallthrough label not found", Label})
  end.

conditional(CFG, Label) ->
  HT = CFG#cfg.table,
  {value,{_Block,Succ,_}} = gb_trees:lookup(Label, HT),
  case Succ of
    [] -> none;
    [_] -> none;
    [_|Labels] -> Labels
  end.
-endif.	% GEN_CFG

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Linearize the code in a CFG. Returns a list of instructions.
%%

-ifdef(GEN_CFG).
-else.
linearize_cfg(CFG) ->
  Start = start_label(CFG),
  Vis = none_visited(),
  {Vis0, NestedCode} = lin_succ(Start, CFG, Vis),
  BlocksInData = hipe_consttab:referred_labels(data(CFG)),
  AllCode = lin_other_entries(NestedCode, CFG, BlocksInData, Vis0),
  lists:flatten(AllCode).

lin_succ(none, _CFG, Vis) ->
  {Vis, []};
lin_succ([Label|Labels], CFG, Vis) ->
  {Vis1, Code1} = lin_succ(Label, CFG, Vis),
  {Vis2, Code2} = lin_succ(Labels, CFG, Vis1),
  {Vis2, [Code1,Code2]};
lin_succ([], _CFG, Vis) ->
  {Vis, []};
lin_succ(Label, CFG, Vis) ->
  case is_visited(Label, Vis) of
    true ->
      {Vis, []};      % already visited
    false ->
      Vis0 = visit(Label, Vis),
      case bb(CFG, Label) of
	not_found ->
	  erlang:error({?MODULE, "No basic block with label", Label});
        BB ->
	  Fallthrough = fallthrough(CFG, Label),
	  Cond = conditional(CFG, Label),
	  LblInstr = mk_label(Label),
	  {Vis1, Code1} = lin_succ(Fallthrough, CFG, Vis0),
	  {Vis2, Code2} = lin_succ(Cond, CFG, Vis1),
	  {Vis2, [[LblInstr|hipe_bb:code(BB)], Code1, Code2]}
      end
  end.

lin_other_entries(Code, _CFG, [], _Vis) ->
  Code;
lin_other_entries(Code, CFG, [E|Es], Vis) ->
  {Vis0, MoreCode} = lin_succ(E, CFG, Vis),
  lin_other_entries([Code, MoreCode], CFG, Es, Vis0).
-endif.

-ifdef(FIND_NEW_LABEL_NEEDED).
find_new_label(Old, Map) ->
  forward(Old, Map).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Remove empty BBs.
%% 
%% Removes basic blocks containing only a goto to another BB.
%% Branches to removed blocks are updated to the successor of the
%% removed block.
%% Loads (or other operations) on the label of the BB are also
%% updated. So are any references from the data section.
%%

-ifdef(REMOVE_TRIVIAL_BBS_NEEDED).

-spec remove_trivial_bbs(cfg()) -> cfg().
remove_trivial_bbs(CFG) ->
  ?opt_start_timer("Merge BBs"),
  CFG0 = merge_bbs(rewrite_trivial_branches(CFG)),
  ?opt_stop_timer("Merge BBs"),
  %% pp(CFG0),
  ?opt_start_timer("FindDead"),
  {NewMap, CFG1} = remap(labels(CFG0), rd_map_new(), CFG0),
  ?opt_stop_timer("FindDead"),
  ?opt_start_timer("Labels"),
  Labels = labels(CFG1),
  ?opt_stop_timer("Labels"),
  ?opt_start_timer("RedirectBranches"),
  CFG2 = redirect_branches(NewMap, CFG1),
  ?opt_stop_timer("RedirectBranches"),
  ?opt_start_timer("RedirectOps"),
  CFG3 = redirect_ops(Labels, CFG2, NewMap),
  ?opt_stop_timer("RedirectOps"),
  ?opt_start_timer("RedirectData"),
  CFG4 = redirect_data(CFG3, NewMap),
  ?opt_stop_timer("RedirectData"),
  ?opt_start_timer("RedirectStart"),
  CFG5 = redirect_start(CFG4, NewMap),
  ?opt_stop_timer("RedirectStart"),
  %% pp(CFG5),
  CFG5.

redirect_start(CFG, Map) ->
  Start = start_label(CFG),
  case forward(Start, Map) of
    Start -> CFG;
    NewStart ->
      start_label_update(CFG, NewStart)
  end.

redirect_data(CFG, Map) ->
  Data = data(CFG),
  NewData = hipe_consttab:update_referred_labels(Data, rd_succs(Map)),
  update_data(CFG, NewData).

redirect_branches(Map, CFG) ->
  lists:foldl(fun ({From,{newsuccs,Redirects}}, CFGAcc) ->
		  lists:foldl(
		    fun({ToOld,ToNew}, CFG1) ->
			case bb(CFG1, From) of
			  not_found -> 
			    CFG1;
			  _ ->
			    To = forward(ToNew, Map),
			    redirect(CFG1, From, ToOld, To)
			end
		    end,
		    CFGAcc,
		    Redirects);
		  (_, CFGAcc) -> CFGAcc
	      end,
	      CFG,
	      gb_trees:to_list(Map)).

redirect(CFG, From, ToOld, ToNew) ->
  BB = bb(CFG, From),
  LastInstr = hipe_bb:last(BB),
  NewLastInstr = redirect_jmp(LastInstr, ToOld, ToNew),
  NewBB = hipe_bb:mk_bb(hipe_bb:butlast(BB) ++ [NewLastInstr]),
  bb_add(CFG, From, NewBB).

bb_remove(CFG, Label) ->
  HT = CFG#cfg.table,
  case gb_trees:lookup(Label, HT) of
    {value, {_Block, Succ, _Preds}} ->
      %% Remove this block as a pred from all successors.
      HT1 = lists:foldl(fun (S,HTAcc) ->
			    remove_pred(HTAcc, S, Label)
			end,
			HT, Succ),
      CFG#cfg{table = gb_trees:delete(Label, HT1)};
    none -> 
      CFG
  end.

remap([L|Rest], Map, CFG) ->
  case is_empty(bb(CFG, L)) of
    true ->
      case succ(CFG, L) of
	[L] -> %% This is an empty (infinite) self loop. Leave it.
	  remap(Rest, Map, CFG);
	[SuccL] ->
	  CFG1 = bb_remove(CFG, L), 
	  NewMap = remap_to_succ(L, SuccL, Map, CFG),
	  remap(Rest, NewMap, CFG1)
      end;
    false ->
      remap(Rest, Map, CFG)
  end;
remap([], Map, CFG) ->
  {Map, CFG}.

remap_to_succ(L, SuccL, Map, PredMap) ->
  insert_remap(L, forward(SuccL,Map), pred(PredMap,L), Map).

%% Find the proxy for a BB
forward(L, Map) ->
  case gb_trees:lookup(L, Map) of
    {value, {dead, To}} ->
      forward(To, Map); %% Hope this terminates.
    _ -> L
  end.

%% A redirection map contains mappings from labels to
%%  none -> this BB is not affected by the remapping.
%%  {dead,To} -> this BB is dead, To is the new proxy.
%%  {newsuccs,[{X,Y}|...]} -> The successor X is redirected to Y. 

rd_map_new() -> gb_trees:empty().

rd_succs(M) ->
  lists:foldl(fun ({From,{dead,To}}, Acc) -> [{From,forward(To,M)}|Acc];
		  (_, Acc) -> Acc
	      end,
	      [],
	      gb_trees:to_list(M)).

add_redirectedto(L, From, To, Map) ->
  case gb_trees:lookup(L, Map) of
    {value, {newsuccs, NS}} ->
      gb_trees:update(L,{newsuccs,[{From,To}|lists:keydelete(From,1,NS)]},Map);
    {value, {dead, _}} -> Map;
    none ->
      gb_trees:insert(L, {newsuccs, [{From, To}]}, Map)
  end.

insert_remap(L, ToL, Preds, Map) ->
  Map2 = gb_trees:enter(L, {dead, ToL}, Map),
  lists:foldl(fun (Pred, AccMap) ->
		   add_redirectedto(Pred, L, ToL, AccMap)
	      end,
	      Map2,
	      Preds).

is_empty(BB) ->
  is_empty_bb(hipe_bb:code(BB)).

is_empty_bb([I]) ->
  is_goto(I); %% A BB with just a 'goto' is empty.
is_empty_bb([I|Is]) ->
  case is_comment(I) of 
    true ->
      is_empty_bb(Is);
    false ->
      false
  end;
is_empty_bb([]) ->
  true.


%% Rewrite all pure branches with one successor to goto:s

-spec rewrite_trivial_branches(cfg()) -> cfg().
rewrite_trivial_branches(CFG) ->
  rewrite_trivial_branches(postorder(CFG), CFG).

rewrite_trivial_branches([L|Left], CFG) ->
  BB = bb(CFG, L),
  Last = hipe_bb:last(BB),
  case is_goto(Last) of
    true ->
      rewrite_trivial_branches(Left, CFG);
    false ->
      case is_pure_branch(Last) of
	false ->
	  rewrite_trivial_branches(Left, CFG);
	true ->
	  case succ(CFG, L) of
	    [Successor] ->
	      Head = hipe_bb:butlast(BB),
	      NewBB = hipe_bb:mk_bb(Head ++ [mk_goto(Successor)]),
	      NewCFG = bb_add(CFG, L, NewBB),
	      rewrite_trivial_branches(Left, NewCFG);
	    _ ->
	      rewrite_trivial_branches(Left, CFG)
	  end
      end
  end;
rewrite_trivial_branches([], CFG) ->
  CFG.


%% Go through the CFG and find pairs of BBs that can be merged to one BB.
%% They are of the form:
%%
%%      L
%%      |
%%   Successor
%%
%% That is, the block L has only one successor (Successor) and that
%% successor has no other predecessors than L.
%%
%% Note: calls might end a basic block 

merge_bbs(CFG) ->
  lists:foldl(fun merge_successor/2, CFG, postorder(CFG)).

%% If L fulfills the requirements, merge it with its successor.
merge_successor(L, CFG) ->
  %% Get the BB L (If it still exists).
  case bb(CFG, L) of
    not_found -> CFG;
    BB ->
      StartLabel = start_label(CFG),
      Last = hipe_bb:last(BB),
      %% Note: Cannot use succ/2 since the instruction can have more than
      %% one successor that are the same label.
      case {branch_successors(Last), fails_to(Last)} of
	{[Successor],[Successor]} ->
	  %% The single successor is the fail-label; don't merge.
	  CFG;
	{[Successor],_} when Successor =/= StartLabel ->
	  %% Make sure the succesor only have this block as predecessor.
	  case [L] =:= pred(CFG, Successor) of
	    true ->
	      %% Remove the goto or remap fall-through in BB and merge the BBs
	      NewCode = merge(BB, bb(CFG, Successor), Successor),
	      NewBB = hipe_bb:mk_bb(NewCode),
	      bb_add(bb_remove(CFG, Successor), L, NewBB);
	    false ->
	      CFG
	  end;
	_ -> 
	  %% Not exactly one successor or tried to merge with the 
	  %% entry point
	  CFG
      end
  end.

%% Merge BB and BB2
merge(BB, BB2, BB2_Label) ->
  Head = hipe_bb:butlast(BB),
  Last = hipe_bb:last(BB),
  Tail = hipe_bb:code(BB2),
  case is_goto(Last) of
    true ->
      %% Just ignore the goto.
      Head ++ Tail;
    false -> 
      %% The last instr is not a goto,
      %%  e.g. a call with only fall-through
      %% Remove the fall-through with the []-label. 
      Head ++ [redirect_jmp(Last, BB2_Label, [])|Tail]
  end.

-endif.	% REMOVE_TRIVIAL_BBS_NEEDED


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Remove unreachable BBs.
%% 
%% A BB is unreachable if it cannot be reached by any path from the
%% start label of the function.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(REMOVE_UNREACHABLE_CODE).

-spec remove_unreachable_code(cfg()) -> cfg().

remove_unreachable_code(CFG) ->
  Start = start_label(CFG),
  Reachable = find_reachable([Start], CFG, gb_sets:from_list([Start])),
  %% Reachable is an ordset: it comes from gb_sets:to_list/1.
  %% So use ordset:subtract instead of '--' below.
  Labels = ordsets:from_list(labels(CFG)),
  case ordsets:subtract(Labels, Reachable) of
    [] ->
      CFG;
    Remove ->
      NewCFG = lists:foldl(fun(X, Acc) -> bb_remove(Acc, X) end, CFG, Remove),
      remove_unreachable_code(NewCFG)
  end.

find_reachable([Label|Left], CFG, Acc) ->
  NewAcc = gb_sets:add(Label, Acc),
  Succ = succ(CFG, Label),
  find_reachable([X || X <- Succ, not gb_sets:is_member(X, Acc)] ++ Left,
		 CFG, NewAcc);
find_reachable([], _CFG, Acc) ->
  gb_sets:to_list(Acc).

-endif.
