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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File        : hipe_rtl_lcm.erl
%% Author      : Henrik Nyman and Erik Cedheim
%% Description : Performs Lazy Code Motion on RTL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%
%% This module implements Lazy Code Motion on RTL.
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_lcm).

-export([rtl_lcm/2]).

-define(SETS, ordsets).   %% Which set implementation module to use
                          %% We have tried gb_sets, sets and ordsets and
                          %% ordsets seems to be a lot faster according to 
                          %% our test runs.

-include("../main/hipe.hrl").
-include("hipe_rtl.hrl").
-include("../flow/cfg.hrl").

%%-define(LCM_DEBUG, true). %% When defined and true, produces debug printouts

%%=============================================================================

%%
%% @doc Performs Lazy Code Motion on RTL.
%%

-spec rtl_lcm(cfg(), comp_options()) -> cfg().

rtl_lcm(CFG, Options) ->
  %% Perform pre-calculation of the data sets.
  ?opt_start_timer("RTL LCM precalc"),
  {NodeInfo, EdgeInfo, AllExpr, ExprMap, IdMap, Labels} = lcm_precalc(CFG, Options),
  ?opt_stop_timer("RTL LCM precalc"),
  %% {NodeInfo, EdgeInfo, AllExpr, ExprMap, Labels} = 
  %%   ?option_time(lcm_precalc(CFG, Options), "RTL LCM precalc", Options),
  
  pp_debug("-------------------------------------------------~n",[]),
  %% pp_debug( "~w~n", [MFA]),

  %% A check if we should pretty print the result.
  case proplists:get_bool(pp_rtl_lcm, Options) of
    true ->
      pp_debug("-------------------------------------------------~n",[]),
      %% pp_debug("AllExpr:  ~w~n", [AllExpr]),
      pp_debug("AllExpr:~n", []),
      pp_exprs(ExprMap, IdMap, ?SETS:to_list(AllExpr)),
      %% pp_sets(ExprMap, NodeInfo, EdgeInfo, AllExpr, CFG2<-ERROR!, Labels); 
      pp_sets(ExprMap, IdMap, NodeInfo, EdgeInfo, AllExpr, CFG, Labels);
    _ ->
      ok
  end,

  pp_debug("-------------------------------------------------~n",[]),
  {CFG1, MoveSet} = ?option_time(perform_lcm(CFG, NodeInfo, EdgeInfo, ExprMap,
					     IdMap, AllExpr, mk_edge_bb_map(),
					     ?SETS:new(), Labels),
				 "RTL LCM perform_lcm", Options),

  %% Scan through list of moved expressions and replace their 
  %% assignments with the new temporary created for that expression
  MoveList = ?SETS:to_list(MoveSet),
  CFG2 = ?option_time(moved_expr_replace_assignments(CFG1, ExprMap, IdMap,
						     MoveList),
		      "RTL LCM moved_expr_replace_assignments", Options),
  pp_debug("-------------------------------------------------~n~n",[]),

  CFG2.

%%=============================================================================
%% Performs lazy code motion given the pre-calculated data sets.
perform_lcm(CFG, _, _, _, _, _, _, MoveSet, []) ->
  {CFG, MoveSet};
perform_lcm(CFG0, NodeInfo, EdgeInfo, ExprMap, IdMap, AllExp, BetweenMap, 
	    MoveSet0, [Label|Labels]) ->
  Code0 = hipe_bb:code(hipe_rtl_cfg:bb(CFG0, Label)),
  DeleteSet = delete(NodeInfo, Label),
  
  %% Check if something should be deleted from this block.
  {CFG1, MoveSet1} = 
    case ?SETS:size(DeleteSet) > 0 of
      true ->
        pp_debug("Label ~w: Expressions Deleted: ~n", [Label]),
        Code1 = delete_exprs(Code0, ExprMap, IdMap, ?SETS:to_list(DeleteSet)),
        BB = hipe_bb:mk_bb(Code1),
        {hipe_rtl_cfg:bb_add(CFG0, Label, BB), 
         ?SETS:union(MoveSet0, DeleteSet)};
      false ->
        {CFG0, MoveSet0}
    end,
  
  Succs = hipe_rtl_cfg:succ(CFG1, Label),  
  
  %% Go through the list of successors and insert expression where needed.
  %% Also collect a list of expressions that are inserted somewhere
  {CFG2, NewBetweenMap, MoveSet2} = 
    lists:foldl(fun(Succ, {CFG, BtwMap, MoveSet}) ->
		    InsertSet = calc_insert_edge(NodeInfo, EdgeInfo, 
						 Label, Succ),
		    %% Check if something should be inserted on this edge.
		    case ?SETS:size(InsertSet) > 0 of
		      true ->
			pp_debug("Label ~w: Expressions Inserted for Successor: ~w~n", [Label, Succ]),
			InsertList = ?SETS:to_list(InsertSet),
			{NewCFG, NewBtwMap} =
			  insert_exprs(CFG, Label, Succ, ExprMap, IdMap, 
				       BtwMap, InsertList),
			{NewCFG, NewBtwMap, ?SETS:union(MoveSet, InsertSet)};
		      false ->
			{CFG, BtwMap, MoveSet}
		    end
		end, 
		{CFG1, BetweenMap, MoveSet1}, Succs),
  
  perform_lcm(CFG2, NodeInfo, EdgeInfo, ExprMap, IdMap, AllExp, NewBetweenMap, 
	      MoveSet2, Labels).

%%=============================================================================
%% Scan through list of moved expressions and replace their 
%% assignments with the new temporary created for that expression.
moved_expr_replace_assignments(CFG, _, _, []) ->
  CFG;
moved_expr_replace_assignments(CFG0, ExprMap, IdMap, [ExprId|Exprs]) ->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  case expr_map_lookup(ExprMap, Expr) of
    {value, {_, ReplaceList, NewReg}} -> 
      CFG1 = lists:foldl(fun({Label, Reg}, CFG) ->
                      %% Find and replace expression in block
		      pp_debug("Label ~w: Expressions Replaced:~n", [Label]),
		      Code0 = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
                      Code1 = 
                        moved_expr_do_replacement(expr_set_dst(Expr, Reg), 
                                                  Reg, NewReg, Code0),
                      hipe_rtl_cfg:bb_add(CFG, Label, hipe_bb:mk_bb(Code1))
                  end, CFG0, ReplaceList),
      moved_expr_replace_assignments(CFG1, ExprMap, IdMap, Exprs);
    none ->
      moved_expr_replace_assignments(CFG0, ExprMap, IdMap, Exprs)
  end.

moved_expr_do_replacement(_, _, _, []) ->
  [];
moved_expr_do_replacement(Expr, Reg, NewReg, [Expr|Instrs]) ->
  NewExpr = expr_set_dst(Expr, NewReg),
  Move = mk_expr_move_instr(Reg, NewReg),
  pp_debug("  Replacing:~n", []),
  pp_debug_instr(Expr),
  pp_debug("  With:~n", []),
  pp_debug_instr(NewExpr),
  pp_debug_instr(Move),
  [NewExpr, Move | moved_expr_do_replacement(Expr, Reg, NewReg, Instrs)];
moved_expr_do_replacement(Expr, Reg, NewReg, [Instr|Instrs]) ->
  [Instr | moved_expr_do_replacement(Expr, Reg, NewReg, Instrs)].

%%=============================================================================
%% Goes through the given list of expressions and deletes them from the code.
%% NOTE We do not actually delete an expression, but instead we replace it 
%%      with an assignment from the new temporary containing the result of the 
%%      expressions which is guaranteed to have been calculated earlier in 
%%      the code.
delete_exprs(Code, _, _, []) ->
  Code;
delete_exprs(Code, ExprMap, IdMap, [ExprId|Exprs]) ->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  %% Lookup expression entry.
  {value, {_, _, Defines}} = expr_map_lookup(ExprMap, Expr),
  %% Go through the code and deletes all occurences of the expression.
  NewCode = delete_expr(Code, Expr, Defines, []),
  delete_exprs(NewCode, ExprMap, IdMap, Exprs).

delete_expr([], _Expr, _Defines, Acc) -> lists:reverse(Acc);
delete_expr([CodeExpr|Code], Expr, Defines, Acc) ->
  case exp_kill_expr(CodeExpr, [Expr]) of
    [] -> % Expr was killed; deleting stops here
      pp_debug("  Stopping before:  ", []),
      pp_debug_instr(CodeExpr),
      lists:reverse(Acc, [CodeExpr|Code]);
    [Expr] ->
      NewCodeExpr =
        case is_expr(CodeExpr) of
          true ->
            case expr_clear_dst(CodeExpr) =:= Expr of
              true ->
                pp_debug("  Deleting:         ", []),
                pp_debug_instr(CodeExpr),
                MoveCode = mk_expr_move_instr(hipe_rtl:defines(CodeExpr),
                                              Defines),
                pp_debug("    Replacing with: ", []),
                pp_debug_instr(MoveCode),
                MoveCode;
              false ->
                CodeExpr
            end;
          false ->
            CodeExpr
        end,
      delete_expr(Code, Expr, Defines, [NewCodeExpr|Acc])
  end.

%%=============================================================================
%% Goes through the given list of expressions and inserts them at 
%% appropriate places in the code.
insert_exprs(CFG, _, _, _, _, BetweenMap, []) ->
  {CFG, BetweenMap};
insert_exprs(CFG, Pred, Succ, ExprMap, IdMap, BetweenMap, [ExprId|Exprs]) ->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  Instr = expr_map_get_instr(ExprMap, Expr),
  case try_insert_expr_last(CFG, Pred, Instr) of
    {ok, NewCFG} ->
      pp_debug("  Inserted last: ", []),
      pp_debug_instr(Instr),
      insert_exprs(NewCFG, Pred, Succ, ExprMap, IdMap, BetweenMap, Exprs);
    not_safe ->
      case hipe_rtl_cfg:pred(CFG, Succ) of
        [_] ->
	  pp_debug("  Inserted first: ", []),
	  pp_debug_instr(Instr),
          NewCFG = insert_expr_first(CFG, Succ, Instr),
          insert_exprs(NewCFG, Pred, Succ, ExprMap, IdMap, BetweenMap, Exprs);
        _ ->
	  pp_debug("  Inserted between: ", []),
	  pp_debug_instr(Instr),
          {NewCFG, NewBetweenMap} = 
	    insert_expr_between(CFG, BetweenMap, Pred, Succ, Instr),
          insert_exprs(NewCFG, Pred, Succ, ExprMap, IdMap, NewBetweenMap, Exprs)
      end 
  end.

%%=============================================================================
%% Recursively goes through the code in a block and returns a new block
%% with the new code inserted second to last (assuming the last expression
%% is a branch operation).
try_insert_expr_last(CFG0, Label, Instr) ->
  case hipe_rtl_cfg:succ(CFG0, Label) of
    [_] ->
      Code0 = hipe_bb:code(hipe_rtl_cfg:bb(CFG0, Label)),
      case insert_expr_last_work(Instr, Code0) of
        not_safe -> not_safe;
        Code1 ->
          {ok, hipe_rtl_cfg:bb_add(CFG0, Label, hipe_bb:mk_bb(Code1))}
      end;
    _ -> not_safe
  end.

%%=============================================================================
%% Recursively goes through the code in a block and returns a new block
%% with the new code inserted second to last (assuming the last expression
%% is a branch operation).
insert_expr_last_work(_Instr, [#call{}]) ->
  %% Call instructions clobber all expressions; we musn't insert the expression
  %% before it
  not_safe;
insert_expr_last_work(Instr, [Code1]) ->
  %% We insert the code next to last.
  [Instr, Code1];
insert_expr_last_work(Instr, [Code|Codes]) ->
  [Code|insert_expr_last_work(Instr, Codes)].

%%=============================================================================
%% Inserts expression first in the block for the given label.
insert_expr_first(CFG0, Label, Instr) ->
  %% The first instruction is always a label
  [Lbl|Code0] = hipe_bb:code(hipe_rtl_cfg:bb(CFG0, Label)),
  Code1 = [Lbl, Instr | Code0],
  hipe_rtl_cfg:bb_add(CFG0, Label, hipe_bb:mk_bb(Code1)).

%%=============================================================================
%% Inserts an expression on and edge between two existing blocks.
%% It creates a new basic block to hold the expression. 
%% Created bbs are inserted into BetweenMap to be able to reuse them for 
%% multiple inserts on the same edge.
%% NOTE Currently creates multiple blocks for identical expression with the
%%      same successor. Since the new bb usually contains very few instructions
%%      this should not be a problem.
insert_expr_between(CFG0, BetweenMap, Pred, Succ, Instr) ->
  PredSucc = {Pred, Succ},
  case edge_bb_map_lookup(BetweenMap, PredSucc) of
    none ->
      NewLabel = hipe_rtl:mk_new_label(),
      NewLabelName = hipe_rtl:label_name(NewLabel),
      pp_debug("    Creating new bb ~w~n", [NewLabel]),
      Code = [Instr, hipe_rtl:mk_goto(Succ)],
      CFG1 = hipe_rtl_cfg:bb_add(CFG0, NewLabelName, hipe_bb:mk_bb(Code)),
      CFG2 = hipe_rtl_cfg:redirect(CFG1, Pred, Succ, NewLabelName),
      NewBetweenMap = edge_bb_map_insert(BetweenMap, PredSucc, NewLabelName),
      pp_debug("    Mapping edge (~w,~w) to label ~w~n", 
	       [Pred, Succ, NewLabelName]),
      {CFG2, NewBetweenMap};
    {value, Label} ->
      pp_debug("    Using existing new bb for edge (~w,~w) with label ~w~n", 
	       [Pred, Succ, Label]),
      {ok, NewCfg} = try_insert_expr_last(CFG0, Label, Instr),
      {NewCfg, BetweenMap}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% GENERAL UTILITY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Returns true if the list of registers only contains virtual registers and
%% no machine registers. 
no_machine_regs([]) ->
  true;
no_machine_regs([Reg|Regs]) ->
  case hipe_rtl:is_reg(Reg) of
    true ->
      N = hipe_rtl:reg_index(Reg),
      (N >= hipe_rtl_arch:first_virtual_reg()) andalso no_machine_regs(Regs);
    _ ->
      case hipe_rtl:is_fpreg(Reg) of
	true ->
	  N = hipe_rtl:fpreg_index(Reg),
	  (N >= hipe_rtl_arch:first_virtual_reg()) andalso no_machine_regs(Regs);
	_ ->
	  no_machine_regs(Regs)
      end
  end.

%%=============================================================================
%% Returns true if an RTL instruction is an expression.
%%
is_expr(I) ->
  Defines = hipe_rtl:defines(I),
  Uses = hipe_rtl:uses(I),
  
  %% We don't cosider something that doesn't define anything as an expression.
  %% Also we don't consider machine registers to be expressions.
  case length(Defines) > 0 andalso no_machine_regs(Defines)
    andalso no_machine_regs(Uses) of 
    true ->
      case I of
        #alu{} -> true;
%%   	#alu{} ->
%%    	  Dst = hipe_rtl:alu_dst(I),
%%    	  Src1 = hipe_rtl:alu_src1(I),
%%    	  Src2 = hipe_rtl:alu_src2(I),

   	  %% Check if dst updates src
%%    	  case Dst =:= Src1 orelse Dst =:= Src2 of
%%    	    true ->
%%    	      false;
%%    	    false ->
%%    	      true
%%    	  end;

	  %% Check if alu expression is untagging of boxed (rX <- vX sub 2)
%% 	  case hipe_rtl:is_reg(Dst) andalso hipe_rtl:is_var(Src1) andalso 
%% 	    (hipe_rtl:alu_op(I) =:= sub) andalso hipe_rtl:is_imm(Src2) of
%% 	    true ->
%% 	      case hipe_rtl:imm_value(Src2) of
%% 		2 -> false; %% Tag for boxed. TODO: Should not be hardcoded...
%% 		_ -> true
%% 	      end;
%% 	    false ->
%% 	      true
%% 	  end;
	       
        #alub{} -> false; %% TODO: Split instruction to consider alu expression?
        #call{} -> false; %% We cannot prove that a call has no side-effects
        #comment{} -> false;
        #enter{} -> false;
        %% #fail_to{} -> false; %% Deprecated?
        #fconv{} -> true;
        #fixnumop{} -> true;
        #fload{} -> true;
        #fmove{} -> false;
        #fp{} -> true;
        #fp_unop{} -> true;
        #fstore{} -> false;
        #goto{} -> false;
        #goto_index{} -> false;
        #gctest{} -> false;
        #label{} -> false;
        #load{} -> true;
        #load_address{} ->
	  case hipe_rtl:load_address_type(I) of
	    c_const -> false;
	    closure -> false;	%% not sure whether safe to move; 
	                        %% also probably not worth it
	    constant -> true
	  end;
        #load_atom{} -> true;
        #load_word_index{} -> true;
        #move{} -> false;
        #multimove{} -> false;
        #phi{} -> false;
        #return{} -> false;
        #store{} -> false;
        #switch{} -> false
      end;
    false ->
      false
  end.

%%=============================================================================
%% Replaces destination of RTL expression with empty list.
%% 
expr_set_dst(I, [Dst|_Dsts] = DstList) ->
  case I of
    #alu{} -> hipe_rtl:alu_dst_update(I, Dst);
    #call{} -> hipe_rtl:call_dstlist_update(I, DstList);
    #fconv{} -> hipe_rtl:fconv_dst_update(I, Dst);
    #fixnumop{} -> hipe_rtl:fixnumop_dst_update(I, Dst);
    #fload{} -> hipe_rtl:fload_dst_update(I, Dst);
    %% #fmove{} -> hipe_rtl:fmove_dst_update(I, Dst);
    #fp{} -> hipe_rtl:fp_dst_update(I, Dst);
    #fp_unop{} -> hipe_rtl:fp_unop_dst_update(I, Dst);
    #load{} -> hipe_rtl:load_dst_update(I, Dst);
    #load_address{} -> hipe_rtl:load_address_dst_update(I, Dst);
    #load_atom{} -> hipe_rtl:load_atom_dst_update(I, Dst);
    #load_word_index{} -> hipe_rtl:load_word_index_dst_update(I, Dst);
    %% #move{} -> hipe_rtl:move_dst_update(I, Dst);
    _ -> exit({?MODULE, expr_set_dst, "bad expression"})
  end.

%%=============================================================================
%% Replaces destination of RTL expression with empty list.
%% 
expr_clear_dst(I) ->
  case I of
    #alu{} -> hipe_rtl:alu_dst_update(I, nil);
    #call{} -> hipe_rtl:call_dstlist_update(I, nil);
    #fconv{} -> hipe_rtl:fconv_dst_update(I, nil);
    #fixnumop{} -> hipe_rtl:fixnumop_dst_update(I, nil);
    #fload{} -> hipe_rtl:fload_dst_update(I, nil);
    %% #fmove{} -> hipe_rtl:fmove_dst_update(I, nil);
    #fp{} -> hipe_rtl:fp_dst_update(I, nil);
    #fp_unop{} -> hipe_rtl:fp_unop_dst_update(I, nil);
    #load{} -> hipe_rtl:load_dst_update(I, nil);
    #load_address{} -> hipe_rtl:load_address_dst_update(I, nil);
    #load_atom{} -> hipe_rtl:load_atom_dst_update(I, nil);
    #load_word_index{} -> hipe_rtl:load_word_index_dst_update(I, nil);
    %% #move{} -> hipe_rtl:move_dst_update(I, nil);
    _ -> exit({?MODULE, expr_clear_dst, "bad expression"})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% PRECALC FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Pre-calculates the flow analysis and puts the calculated sets in maps for 
%% easy access later.
lcm_precalc(CFG, Options) ->
  %% Calculate use map and expression map.
  {ExprMap, IdMap} = ?option_time(mk_expr_map(CFG),
				  "RTL LCM mk_expr_map", Options),
  UseMap = ?option_time(mk_use_map(CFG, ExprMap),
			"RTL LCM mk_use_map", Options),
  %% Labels = hipe_rtl_cfg:reverse_postorder(CFG),
  Labels = hipe_rtl_cfg:labels(CFG),
  %% StartLabel = hipe_rtl_cfg:start_label(CFG),
  %% AllExpr = all_exprs(CFG, Labels),
  AllExpr = ?SETS:from_list(gb_trees:keys(IdMap)),

  %% Calculate the data sets.
  NodeInfo0 = ?option_time(mk_node_info(Labels),
			   "RTL LCM mk_node_info", Options),
  %% ?option_time(EdgeInfo0 = mk_edge_info(), "RTL LCM mk_edge_info", 
  %%  	          Options),
  EdgeInfo0 = mk_edge_info(),
  NodeInfo1 = ?option_time(calc_up_exp(CFG, ExprMap, NodeInfo0, Labels),
			   "RTL LCM calc_up_exp", Options),
  NodeInfo2 = ?option_time(calc_down_exp(CFG, ExprMap, NodeInfo1, Labels),
			   "RTL LCM calc_down_exp", Options),
  NodeInfo3 = ?option_time(calc_killed_expr(CFG, NodeInfo2, UseMap, AllExpr,
					    IdMap, Labels), 
			   "RTL LCM calc_killed_exp", Options),
  NodeInfo4 = ?option_time(calc_avail(CFG, NodeInfo3),
			   "RTL LCM calc_avail", Options),
  NodeInfo5 = ?option_time(calc_antic(CFG, NodeInfo4, AllExpr),
			   "RTL LCM calc_antic", Options),
  EdgeInfo1 = ?option_time(calc_earliest(CFG, NodeInfo5, EdgeInfo0, Labels),
			   "RTL LCM calc_earliest", Options),
  {NodeInfo6, EdgeInfo2} = ?option_time(calc_later(CFG, NodeInfo5, EdgeInfo1),
					"RTL LCM calc_later", Options),
  NodeInfo7 = ?option_time(calc_delete(CFG, NodeInfo6, Labels),
			   "RTL LCM calc_delete", Options),
  {NodeInfo7, EdgeInfo2, AllExpr, ExprMap, IdMap, Labels}.

%%%%%%%%%%%%%%%%%%% AVAILABLE IN/OUT FLOW ANALYSIS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fixpoint calculation of anticipated in/out sets.
%% Uses a worklist algorithm.
%% Performs the avail in/out flow analysis.

%%=============================================================================
%% Calculates the available in/out sets, and returns an updated NodeInfo.

calc_avail(CFG, NodeInfo) ->
  StartLabel = hipe_rtl_cfg:start_label(CFG),
  Work = init_work([StartLabel]),
  %% Initialize start node
  NewNodeInfo = set_avail_in(NodeInfo, StartLabel, ?SETS:new()),
  calc_avail_fixpoint(Work, CFG, NewNodeInfo).

calc_avail_fixpoint(Work, CFG, NodeInfo) ->
  case get_work(Work) of
    fixpoint ->
      NodeInfo;
    {Label, NewWork} ->
      {NewNodeInfo, NewLabels} = calc_avail_node(Label, CFG, NodeInfo),
      NewWork2 = add_work(NewWork, NewLabels),
      calc_avail_fixpoint(NewWork2, CFG, NewNodeInfo)
  end.

calc_avail_node(Label, CFG, NodeInfo) ->
  %% Get avail in
  AvailIn = avail_in(NodeInfo, Label),

  %% Calculate avail out
  AvailOut = ?SETS:union(down_exp(NodeInfo, Label),
			 ?SETS:subtract(AvailIn,
					killed_expr(NodeInfo, Label))),
  
  {Changed, NodeInfo2} = 
    case avail_out(NodeInfo, Label) of
      none ->
	%% If there weren't any old avail out we use this one.
	{true, set_avail_out(NodeInfo, Label, AvailOut)};
      OldAvailOut ->
	%% Check if the avail outs are equal.
	case AvailOut =:= OldAvailOut of
	  true ->
	    {false, NodeInfo};
	  false ->
	    {true, set_avail_out(NodeInfo, Label, AvailOut)}
	end
    end,

  case Changed of
    true ->
      %% Update AvailIn-sets of successors and add them to worklist
      Succs = hipe_rtl_cfg:succ(CFG, Label),
      NodeInfo3 =
	lists:foldl
	  (fun(Succ, NewNodeInfo) ->
	       case avail_in(NewNodeInfo, Succ) of
		 none ->
		   %% Initialize avail in to all expressions
		   set_avail_in(NewNodeInfo, Succ, AvailOut);
		 OldAvailIn ->
		   set_avail_in(NewNodeInfo, Succ, 
				?SETS:intersection(OldAvailIn, AvailOut))
	       end
	   end,
	   NodeInfo2, Succs),
      {NodeInfo3, Succs};
    false ->
      {NodeInfo2, []}
  end.

%%%%%%%%%%%%%%%%%% ANTICIPATED IN/OUT FLOW ANALYSIS  %%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fixpoint calculation of anticipated in/out sets.
%% Uses a worklist algorithm.

%%=============================================================================
%% Calculates the anicipated in/out sets, and returns an updated NodeInfo.
calc_antic(CFG, NodeInfo, AllExpr) ->
  %% Initialize worklist with all nodes in postorder
  Labels = hipe_rtl_cfg:postorder(CFG),
  Work = init_work(Labels),
  calc_antic_fixpoint(Work, CFG, NodeInfo, AllExpr).

calc_antic_fixpoint(Work, CFG, NodeInfo, AllExpr) ->
  case get_work(Work) of
    fixpoint ->
      NodeInfo;
    {Label, NewWork} ->
      {NewNodeInfo, NewLabels} = calc_antic_node(Label, CFG, NodeInfo, AllExpr),
      NewWork2 = add_work(NewWork, NewLabels),
      calc_antic_fixpoint(NewWork2, CFG, NewNodeInfo, AllExpr)
  end.

calc_antic_node(Label, CFG, NodeInfo, AllExpr) ->
  %% Get antic out
  AnticOut = 
    case antic_out(NodeInfo, Label) of
      none -> 
	case is_exit_label(CFG, Label) of 
	  true ->
	    ?SETS:new();
	  false ->
	    AllExpr
	end;
      
      AnticOutTemp -> AnticOutTemp
    end,

  %% Calculate antic in
  AnticIn = ?SETS:union(up_exp(NodeInfo, Label),
			?SETS:subtract(AnticOut,
				       killed_expr(NodeInfo, Label))),
  {Changed, NodeInfo2} = 
    case antic_in(NodeInfo, Label) of
      %% If there weren't any old antic in we use this one.
      none ->
	{true, set_antic_in(NodeInfo, Label, AnticIn)};

      OldAnticIn ->
	%% Check if the antic in:s are equal.
	case AnticIn =:= OldAnticIn of
	  true ->
	    {false, NodeInfo};
	  false ->
	    {true, 
	     set_antic_in(NodeInfo, Label, AnticIn)}
	end
    end,

  case Changed of
    true ->
      %% Update AnticOut-sets of predecessors and add them to worklist
      Preds = hipe_rtl_cfg:pred(CFG, Label),
      NodeInfo3 =
	lists:foldl
	  (fun(Pred, NewNodeInfo) ->
	       case antic_out(NewNodeInfo, Pred) of
		 none ->
		   %% Initialize antic out to all expressions
		   set_antic_out(NewNodeInfo, Pred, AnticIn);
		 OldAnticOut ->
		   set_antic_out(NewNodeInfo, Pred, 
				 ?SETS:intersection(OldAnticOut, AnticIn))
	       end
	   end,
	   NodeInfo2, Preds),
      {NodeInfo3, Preds};
    false ->
      {NodeInfo2, []}
  end.

%%%%%%%%%%%%%%%%%%%%% LATER / LATER IN FLOW ANALYSIS %%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fixpoint calculations of Later and LaterIn sets. 
%% Uses a worklist algorithm.
%% Note that the Later set is calculated on edges.

%%=============================================================================
%% Calculates the Later and LaterIn sets, and returns updates of both 
%% NodeInfo (with LaterIn sets) and EdgeInfo (with Later sets).

calc_later(CFG, NodeInfo, EdgeInfo) ->
  StartLabel = hipe_rtl_cfg:start_label(CFG),
  Work = init_work([{node, StartLabel}]),
  %% Initialize start node
  NewNodeInfo = set_later_in(NodeInfo, StartLabel, ?SETS:new()),
  calc_later_fixpoint(Work, CFG, NewNodeInfo, EdgeInfo).

calc_later_fixpoint(Work, CFG, NodeInfo, EdgeInfo) ->
  case get_work(Work) of
    {{edge, From, To}, Work2} ->
      {NewNodeInfo, NewEdgeInfo, AddWork} = 
	calc_later_edge(From, To, CFG, NodeInfo, EdgeInfo),
      Work3 = add_work(Work2, AddWork),
      calc_later_fixpoint(Work3, CFG, NewNodeInfo, NewEdgeInfo);
    {{node, Label}, Work2} ->
      AddWork = calc_later_node(Label, CFG),
      Work3 = add_work(Work2, AddWork),
      calc_later_fixpoint(Work3, CFG, NodeInfo, EdgeInfo);
    fixpoint ->
      {NodeInfo, EdgeInfo}
  end.

calc_later_node(Label, CFG) ->
  Succs = hipe_rtl_cfg:succ(CFG, Label),
  [{edge, Label, Succ} || Succ <- Succs].

calc_later_edge(From, To, _CFG, NodeInfo, EdgeInfo) ->
  FromTo = {From, To},
  Earliest = earliest(EdgeInfo, FromTo),
  LaterIn = later_in(NodeInfo, From),
  UpExp = up_exp(NodeInfo, From),
  Later = ?SETS:union(Earliest, ?SETS:subtract(LaterIn, UpExp)),
  {Changed, EdgeInfo2} =
    case lookup_later(EdgeInfo, FromTo) of
      none ->  {true, set_later(EdgeInfo, FromTo, Later)};
      Later -> {false, EdgeInfo};
      _Old ->  {true, set_later(EdgeInfo, FromTo, Later)}
    end,
  case Changed of 
    true ->
      %% Update later in set of To-node
      case lookup_later_in(NodeInfo, To) of
	%% If the data isn't set initialize to all expressions
	none ->
 	  {set_later_in(NodeInfo, To, Later), EdgeInfo2, [{node, To}]};
	OldLaterIn ->
	  NewLaterIn = ?SETS:intersection(OldLaterIn, Later),
	  %% Check if something changed
	  %% FIXME: Implement faster equality test?
	  case NewLaterIn =:= OldLaterIn of 
	    true ->
	      {NodeInfo, EdgeInfo2, []};
	    false ->
	      {set_later_in(NodeInfo, To, NewLaterIn),
	       EdgeInfo2, [{node, To}]}
	  end
      end;
    false ->
      {NodeInfo, EdgeInfo2, []}
  end.

%%%%%%%%%%%%%%%%%% UPWARDS/DOWNWARDS EXPOSED EXPRESSIONS %%%%%%%%%%%%%%%%%%%%%%
%% Calculates upwards and downwards exposed expressions.

%%=============================================================================
%% Calculates the downwards exposed expression sets for the given labels in
%% the CFG.
calc_down_exp(_, _, NodeInfo, []) ->
  NodeInfo;
calc_down_exp(CFG, ExprMap, NodeInfo, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
  %% Data = ?SETS:from_list(lists:map(fun expr_clear_dst/1, exp_work(Code))),
  Data = ?SETS:from_list(get_expr_ids(ExprMap, exp_work(Code))),
  NewNodeInfo = set_down_exp(NodeInfo, Label, Data),
  calc_down_exp(CFG, ExprMap, NewNodeInfo, Labels).
  
%%=============================================================================
%% Calculates the upwards exposed expressions sets for the given labels in 
%% the CFG.
calc_up_exp(_, _, NodeInfo, []) ->
  NodeInfo;
calc_up_exp(CFG, ExprMap, NodeInfo, [Label|Labels]) ->
  BB = hipe_rtl_cfg:bb(CFG, Label),
  RevCode = lists:reverse(hipe_bb:code(BB)),
  Data = ?SETS:from_list(get_expr_ids(ExprMap, exp_work(RevCode))),
  NewNodeInfo = set_up_exp(NodeInfo, Label, Data),
  calc_up_exp(CFG, ExprMap, NewNodeInfo, Labels).

%%=============================================================================
%% Given a list of expression instructions, gets a list of expression ids
%% from an expression map.
get_expr_ids(ExprMap, Instrs) ->
  [expr_map_get_id(ExprMap, expr_clear_dst(I)) || I <- Instrs].

%%=============================================================================
%% Does the work of the calc_*_exp functions.
exp_work(Code) ->
  exp_work([], Code).

exp_work([], [Instr|Instrs]) ->
  case is_expr(Instr) of
    true  ->
      exp_work([Instr], Instrs);
    false ->
      exp_work([], Instrs)
  end;
exp_work(Exprs, []) ->
  Exprs;
exp_work(Exprs, [Instr|Instrs]) ->
  NewExprs = case is_expr(Instr) of
	       true ->
		 exp_kill_expr(Instr, [Instr|Exprs]);
	       false ->
		 exp_kill_expr(Instr, Exprs)
	     end,
  exp_work(NewExprs, Instrs).

%%=============================================================================
%% Checks if the given instruction redefines any operands of 
%% instructions in the instruction list. 
%% It returns the list of expressions with those instructions that has
%% operands redefined removed.
exp_kill_expr(_Instr, []) ->
  [];
exp_kill_expr(Instr, [CheckedExpr|Exprs]) ->
  %% Calls, gctests and stores potentially clobber everything
  case Instr of
    #call{} -> [];
    #gctest{} -> [];
    #store{} -> [];     %% FIXME: Only regs and vars clobbered, not fregs...
    #fstore{} -> 
      %% fstore potentially clobber float expressions
      [ExprDefine|_] = hipe_rtl:defines(CheckedExpr),
      case hipe_rtl:is_fpreg(ExprDefine) of
	true ->
	  exp_kill_expr(Instr, Exprs);
	false ->
	  [CheckedExpr | exp_kill_expr(Instr, Exprs)]
      end;
    _ ->
      InstrDefines = hipe_rtl:defines(Instr),
      ExprUses     = hipe_rtl:uses(CheckedExpr),
      Diff         = ExprUses -- InstrDefines,
      case length(Diff) < length(ExprUses) of  
	true ->
	  exp_kill_expr(Instr, Exprs);
	false ->
	  [CheckedExpr | exp_kill_expr(Instr, Exprs)]
      end
  end.
     
%%%%%%%%%%%%%%%%%%%%%%%% KILLED EXPRESSIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Calculates the killed expression sets for all given labels.
calc_killed_expr(_, NodeInfo, _, _, _, []) ->
  NodeInfo;
calc_killed_expr(CFG, NodeInfo, UseMap, AllExpr, IdMap, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
  KilledExprs = calc_killed_expr_bb(Code, UseMap, AllExpr, IdMap, ?SETS:new()),
  NewNodeInfo = set_killed_expr(NodeInfo, Label, KilledExprs),
  calc_killed_expr(CFG, NewNodeInfo, UseMap, AllExpr, IdMap, Labels).

%%=============================================================================
%% Calculates the killed expressions set for one basic block.
calc_killed_expr_bb([], _UseMap, _AllExpr, _IdMap, KilledExprs) ->
  KilledExprs;
calc_killed_expr_bb([Instr|Instrs], UseMap, AllExpr, IdMap, KilledExprs) ->
  %% Calls, gctests and stores potentially clobber everything
  case Instr of
    #call{} -> AllExpr;
    #gctest{} -> AllExpr;
    #store{} -> AllExpr;   %% FIXME: Only regs and vars clobbered, not fregs...
    #fstore{} -> 
      %% Kill all float expressions
      %% FIXME: Make separate function is_fp_expr
      ?SETS:from_list
	(lists:foldl(fun(ExprId, Fexprs) ->
			     Expr = expr_id_map_get_expr(IdMap, ExprId),
			 [Define|_] = hipe_rtl:defines(Expr),
			 case hipe_rtl:is_fpreg(Define) of
			   true ->
			     [Expr|Fexprs];
			   false ->
			     Fexprs
			 end
		     end, [], ?SETS:to_list(AllExpr)));
    _ ->
      case hipe_rtl:defines(Instr) of
	[] ->
	  calc_killed_expr_bb(Instrs, UseMap, AllExpr, IdMap, KilledExprs);
	[Define|_] ->
	  NewKilledExprs = use_map_get_expr_uses(UseMap, Define),
	  calc_killed_expr_bb(Instrs, UseMap, AllExpr, IdMap,
			      ?SETS:union(NewKilledExprs, KilledExprs))
      end
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%% EARLIEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Calculates the earliest set for all edges in the CFG.

calc_earliest(_, _, EdgeInfo, []) ->
  EdgeInfo;
calc_earliest(CFG, NodeInfo, EdgeInfo, [To|Labels]) ->
  EmptySet = ?SETS:new(),
  Preds = hipe_rtl_cfg:pred(CFG, To),
  NewEdgeInfo =
    case EmptySet =:= antic_in(NodeInfo, To) of
      true ->
	%% Earliest is empty for all edges into this block.
	lists:foldl(fun(From, EdgeInfoAcc) ->
			set_earliest(EdgeInfoAcc, {From, To}, EmptySet)
		    end, EdgeInfo, Preds);
      false ->
	lists:foldl(fun(From, EdgeInfoAcc) ->
			IsStartLabel = (From =:= hipe_rtl_cfg:start_label(CFG)),
			Earliest = 
			  calc_earliest_edge(NodeInfo, IsStartLabel, From, To),
			set_earliest(EdgeInfoAcc, {From, To}, Earliest)
		    end, EdgeInfo, Preds)
    end,
  calc_earliest(CFG, NodeInfo, NewEdgeInfo, Labels).
  
%%=============================================================================
%% Calculates the earliest set for one edge.

calc_earliest_edge(NodeInfo, IsStartLabel, From, To) ->
  AnticIn = antic_in(NodeInfo, To),
  AvailOut = avail_out(NodeInfo, From),
  
  case IsStartLabel of
    true ->
      ?SETS:subtract(AnticIn, AvailOut);
    false ->
      AnticOut = antic_out(NodeInfo, From),
      ExprKill = killed_expr(NodeInfo, From),
      ?SETS:subtract(?SETS:subtract(AnticIn, AvailOut),
		     ?SETS:subtract(AnticOut, ExprKill))
  end.
%% The above used to be:
%%
%% ?SETS:intersection(?SETS:subtract(AnticIn, AvailOut),
%%                    ?SETS:union(ExprKill, ?SETS:subtract(AllExpr, AnticOut)))
%%
%% But it is costly to use the AllExpr, so let's do some tricky set algebra.
%%
%% Let A = AnticIn, B = AvailOut, C = ExprKill, D = AnticOut, U = AllExpr
%% Let n = intersection, u = union, ' = inverse
%%
%% Then
%%    (A - B) n (C u (U - D)) =       <Remove D unless it is in C>
%%  = (A - B) n ((C u U) - (D - C)) = <But U is the whole universe>
%%  = (A - B) n (U - (D - C)) =       <We are really meaning the complement>
%%  = (A - B) n (D - C)' =            <Intersection w complement is subtraction>
%%  = (A - B) - (D - C)               <Simple enough, let's stop>
%%
%% or in other words
%%   ?SETS:subtract(?SETS:subtract(AnticIn, AvailOut),
%%                  ?SETS:subtract(AnticOut, ExprKill))



%%%%%%%%%%%%%%%%%%%%%%%% INSERT / DELETE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Calculates the insert set for one edge and returns the resulting set.
%% NOTE This does not modify the EdgeInfo set, since the resulting set is 
%%      returned and used immediately, instead of being pre-calculated as are 
%%      the other sets.
calc_insert_edge(NodeInfo, EdgeInfo, From, To) ->
  Later = later(EdgeInfo, {From, To}),
  LaterIn = later_in(NodeInfo, To),	      
  ?SETS:subtract(Later, LaterIn).

%%=============================================================================
%% Calculates the delete set for all given labels in a CFG.
calc_delete(_, NodeInfo, []) ->
  NodeInfo;
calc_delete(CFG, NodeInfo, [Label|Labels]) ->
  NewNodeInfo =
    case Label =:= hipe_rtl_cfg:start_label(CFG) of
      true ->
	set_delete(NodeInfo, Label, ?SETS:new());
      false ->
	UpExp = up_exp(NodeInfo, Label),
	LaterIn = later_in(NodeInfo, Label),
	Delete = ?SETS:subtract(UpExp, LaterIn),
	set_delete(NodeInfo, Label, Delete)
    end,
  calc_delete(CFG, NewNodeInfo, Labels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% FIXPOINT FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Worklist used by the fixpoint calculations.
%% 
%% We use gb_sets here, which is optimized for continuous inserts and
%% membership tests.

init_work(Labels) ->
  {Labels, [], gb_sets:from_list(Labels)}.

get_work({[Label|Left], List, Set}) ->
  NewWork = {Left, List, gb_sets:delete(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set}, [Label|Labels]) ->
  case gb_sets:is_member(Label, Set) of
    true ->
      add_work(Work, Labels);
    false ->
      %%io:format("Adding work: ~w\n", [Label]),
      add_work({List1, [Label|List2], gb_sets:insert(Label, Set)}, Labels)
  end;
add_work(Work, []) ->
  Work.

%%=============================================================================
%% Calculates the labels that are the exit labels.
%% FIXME We do not detect dead-end loops spanning more than one block.
%%       This could potentially cause a bug in the future...
%% exit_labels(CFG) ->
%%   Labels = hipe_rtl_cfg:labels(CFG),
%%   lists:foldl(fun(Label, ExitLabels) ->
%%                   Succs = hipe_rtl_cfg:succ(CFG, Label),
%%                   case Succs of
%% 		    [] ->
%%                       [Label|ExitLabels];
%% 		    [Label] -> %% Count single bb dead-end loops as exit labels
%%                       [Label|ExitLabels];		      
%%                     _ ->
%%                       ExitLabels
%%                   end
%%               end, [], Labels ).

%%=============================================================================
%% Return true if label is an exit label,
%% i.e. its bb has no successors or itself as only successor.
is_exit_label(CFG, Label) ->
  case hipe_rtl_cfg:succ(CFG, Label) of
    [] -> true;
    [Label] -> true;
    _ -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% DATASET FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The dataset is a collection of data about the CFG. 
%% It is divided into two parts, NodeInfo and EdgeInfo.
%% The pre-calculation step stores the calculated sets here.

-record(node_data, {up_exp      = none,
		    down_exp    = none,
		    killed_expr = none,
		    avail_in    = none,
		    avail_out   = none,
		    antic_in    = none,
		    antic_out   = none,
		    later_in    = none,
		    delete      = none}).

-record(edge_data, {earliest    = none,
		    later       = none,
		    insert      = none}).

%%=============================================================================
%% Creates a node info from a CFG (one entry for each Label).
mk_node_info(Labels) ->
  lists:foldl(fun(Label, DataTree) ->
		  gb_trees:insert(Label, #node_data{}, DataTree)
		  %%gb_trees:enter(Label, #node_data{}, DataTree)
	      end, 
	      gb_trees:empty(), Labels).

%%mk_edge_info(Labels) ->
%%  FIXME Should we traverse cfg and initialize edges?
mk_edge_info() ->
  gb_trees:empty().

%%=============================================================================
%% Get methods
up_exp(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.up_exp.

down_exp(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.down_exp.

killed_expr(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.killed_expr.

avail_in(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.avail_in.

avail_out(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.avail_out.

antic_in(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.antic_in.

antic_out(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.antic_out.

later_in(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.later_in.

lookup_later_in(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      none;
    {value, #node_data{later_in = Data}} ->
      Data
  end.

delete(NodeInfo, Label) ->
  Data = gb_trees:get(Label, NodeInfo),
  Data#node_data.delete.

earliest(EdgeInfo, Edge) ->
  Data = gb_trees:get(Edge, EdgeInfo),
  Data#edge_data.earliest.

-ifdef(LOOKUP_EARLIEST_NEEDED).
lookup_earliest(EdgeInfo, Edge) ->
  case gb_trees:lookup(Edge, EdgeInfo) of
    none -> 
      none;
    {value, #edge_data{earliest = Data}} ->
      Data
  end.
-endif.

later(EdgeInfo, Edge) ->
  Data = gb_trees:get(Edge, EdgeInfo),
  Data#edge_data.later.

lookup_later(EdgeInfo, Edge) ->
  case gb_trees:lookup(Edge, EdgeInfo) of
    none -> 
      none;
    {value, #edge_data{later = Data}} ->
      Data
  end.

%% insert(EdgeInfo, Edge) ->
%%   case gb_trees:lookup(Edge, EdgeInfo) of
%%     none -> 
%%       exit({?MODULE, insert, "edge info not found"}),
%%       none;
%%     {value, #edge_data{insert = Data}} ->
%%       Data
%%   end.

%%=============================================================================
%% Set methods
set_up_exp(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{up_exp = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{up_exp = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_down_exp(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{down_exp = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{down_exp = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_killed_expr(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{killed_expr = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{killed_expr = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_avail_in(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{avail_in = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{avail_in = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_avail_out(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{avail_out = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{avail_out = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_antic_in(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{antic_in = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{antic_in = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_antic_out(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{antic_out = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{antic_out = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_later_in(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{later_in = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{later_in = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_delete(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{delete = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{delete = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_earliest(EdgeInfo, Edge, Data) ->
  EdgeData =
    case gb_trees:lookup(Edge, EdgeInfo) of
      none -> 
	#edge_data{earliest = Data};
      {value, OldEdgeData} ->
	OldEdgeData#edge_data{earliest = Data}
    end,
  gb_trees:enter(Edge, EdgeData, EdgeInfo).

set_later(EdgeInfo, Edge, Data) ->
  EdgeData =
    case gb_trees:lookup(Edge, EdgeInfo) of
      none -> 
	#edge_data{later = Data};
      {value, OldEdgeData} ->
	OldEdgeData#edge_data{later = Data}
    end,
  gb_trees:enter(Edge, EdgeData, EdgeInfo).

%% set_insert(EdgeInfo, Edge, Data) ->
%%   EdgeData =
%%     case gb_trees:lookup(Edge, EdgeInfo) of
%%       none -> 
%% 	#edge_data{insert = Data};
%%       {value, OldEdgeData} ->
%% 	OldEdgeData#edge_data{insert = Data}
%%     end,
%%   gb_trees:enter(Edge, EdgeData, EdgeInfo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% USE MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The use map is a mapping from "use" (which is an rtl register/variable) 
%% to a set of expressions (IDs) where that register/variable is used.
%% It is used by calc_killed_expr to know what expressions are affected by
%% a definition.

%%=============================================================================
%% Creates and calculates the use map for a CFG.
%% It uses ExprMap to lookup the expression IDs.
mk_use_map(CFG, ExprMap) ->
  Labels = hipe_rtl_cfg:reverse_postorder(CFG),
  NewMap = mk_use_map(gb_trees:empty(), CFG, ExprMap, Labels),
  gb_trees:balance(NewMap).

mk_use_map(Map, _, _, []) ->
  Map;
mk_use_map(Map, CFG, ExprMap, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
  NewMap = mk_use_map_bb(Map, ExprMap, Code),
  mk_use_map(NewMap, CFG, ExprMap, Labels).

mk_use_map_bb(UseMap, _, []) ->
  UseMap;
mk_use_map_bb(UseMap, ExprMap, [Instr|Instrs]) ->
  case is_expr(Instr) of
    true ->
      Uses = hipe_rtl:uses(Instr),
      ExprId = expr_map_get_id(ExprMap, expr_clear_dst(Instr)),
      NewUseMap = mk_use_map_insert_uses(UseMap, ExprId, Uses),
      mk_use_map_bb(NewUseMap, ExprMap, Instrs);
    false ->
      mk_use_map_bb(UseMap, ExprMap, Instrs)
  end.

%%=============================================================================
%% Worker function for mk_use_map that inserts the expression id for every 
%% rtl register the expression uses in a use map.
mk_use_map_insert_uses(Map, _, []) ->
  Map;
mk_use_map_insert_uses(Map, Expr, [Use|Uses]) ->
  case gb_trees:lookup(Use, Map) of
    {value, UseSet} ->
      NewUseSet = ?SETS:add_element(Expr, UseSet),
      mk_use_map_insert_uses(gb_trees:update(Use, NewUseSet, Map), Expr, Uses);
    none ->
      UseSet = ?SETS:new(),
      NewUseSet = ?SETS:add_element(Expr, UseSet),
      mk_use_map_insert_uses(gb_trees:insert(Use, NewUseSet, Map), Expr, Uses)
  end.

%%=============================================================================
%% Gets a set of expressions where the given rtl register is used.
use_map_get_expr_uses(Map, Reg) ->
  case gb_trees:lookup(Reg, Map) of
    {value, UseSet} ->
      UseSet;
    none ->
      ?SETS:new()
  end. 

%%%%%%%%%%%%%%%%%%%%%% EXPRESSION MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The expression map is a mapping from expression to 
%% (1) Expression Id (Integer used to speed up set operations)
%% (2) List of definitions (labels where the expression is defined and the 
%%     list of registers or variables defined by an instruction in that label,
%%     represented as a tuple {Label, Defines})
%% (3) The list of replacement registers created for the expression

%%=============================================================================
%% Creates and calculates the expression map for a CFG.
mk_expr_map(CFG) ->
  init_expr_id(),
  Labels = hipe_rtl_cfg:reverse_postorder(CFG),
  {ExprMap, IdMap} = mk_expr_map(gb_trees:empty(), gb_trees:empty(), 
				 CFG, Labels),
  {gb_trees:balance(ExprMap), gb_trees:balance(IdMap)}.

mk_expr_map(ExprMap, IdMap, _, []) ->
  {ExprMap, IdMap};
mk_expr_map(ExprMap, IdMap, CFG, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
  {NewExprMap, NewIdMap} = mk_expr_map_bb(ExprMap, IdMap, Label, Code),
  mk_expr_map(NewExprMap, NewIdMap, CFG, Labels).

mk_expr_map_bb(ExprMap, IdMap, _, []) ->
  {ExprMap, IdMap};
mk_expr_map_bb(ExprMap, IdMap, Label, [Instr|Instrs]) ->
  case is_expr(Instr) of
    true ->
      Expr = expr_clear_dst(Instr),
      Defines = hipe_rtl:defines(Instr),
      case gb_trees:lookup(Expr, ExprMap) of
        {value, {ExprId, DefinesList, ReplRegs}} ->
          NewExprMap = gb_trees:update(Expr, {ExprId, 
					      [{Label, Defines}|DefinesList], 
					      ReplRegs}, ExprMap),
	  mk_expr_map_bb(NewExprMap, IdMap, Label, Instrs);
        none ->
	  NewExprId = new_expr_id(),
          NewReplRegs = mk_replacement_regs(Defines),
          NewExprMap = gb_trees:insert(Expr, {NewExprId, 
					      [{Label, Defines}], 
					      NewReplRegs}, ExprMap),
          NewIdMap = gb_trees:insert(NewExprId, Expr, IdMap),
	  mk_expr_map_bb(NewExprMap, NewIdMap, Label, Instrs)
      end;
    false ->
      mk_expr_map_bb(ExprMap, IdMap, Label, Instrs)
  end.

%%=============================================================================
%% Creates new temporaries to replace defines in moved expressions.
mk_replacement_regs([]) ->
  [];
mk_replacement_regs(Defines) ->
  mk_replacement_regs(Defines, []).

mk_replacement_regs([], NewRegs) ->
  lists:reverse(NewRegs);
mk_replacement_regs([Define|Defines], NewRegs) ->
  case hipe_rtl:is_reg(Define) of
    true ->
      NewReg =
	case hipe_rtl:reg_is_gcsafe(Define) of
	  true -> hipe_rtl:mk_new_reg_gcsafe();
	  false -> hipe_rtl:mk_new_reg()
	end,
      mk_replacement_regs(Defines, [NewReg|NewRegs]);
    false ->
      case hipe_rtl:is_var(Define) of
	true ->
	  mk_replacement_regs(Defines, [hipe_rtl:mk_new_var()|NewRegs]);
	false ->
	  true = hipe_rtl:is_fpreg(Define),
	  mk_replacement_regs(Defines, [hipe_rtl:mk_new_fpreg()|NewRegs])
      end
  end.
  
%%=============================================================================
%% Performs a lookup, which returns a tuple
%% {expression ID, list of definitions, list of replacement registers}
expr_map_lookup(Map, Expr) ->
  gb_trees:lookup(Expr, Map).

%%=============================================================================
%% Gets the actual RTL instruction to be generated for insertions of an 
%% expression.
expr_map_get_instr(Map, Expr) ->
  case gb_trees:lookup(Expr, Map) of
    {value, {_, _, Regs}} ->
      expr_set_dst(Expr, Regs);
    none ->
      exit({?MODULE, expr_map_get_instr, "expression missing"})
  end.

%%=============================================================================
%% Gets expression id.
expr_map_get_id(Map, Expr) ->
  case gb_trees:lookup(Expr, Map) of
    {value, {ExprId, _, _}} ->
      ExprId;
    none ->
      exit({?MODULE, expr_map_get_instr, "expression missing"})
  end.

%%=============================================================================
%% Creates an rtl instruction that moves a value
mk_expr_move_instr([Reg], [Define]) ->
  case hipe_rtl:is_fpreg(Reg) of
    true ->
      hipe_rtl:mk_fmove(Reg, Define);
    false ->
      %% FIXME Check is_var() orelse is_reg() ?
      hipe_rtl:mk_move(Reg, Define)
  end;
mk_expr_move_instr([_Reg|_Regs] = RegList, Defines) ->
  %% FIXME Does this really work? What about floats...
  %% (Multiple defines does not seem to be used by any of the 
  %%  instructions considered by rtl_lcm at the moment so this is pretty much
  %%  untested/unused.)
  hipe_rtl:mk_multimove(RegList, Defines);
mk_expr_move_instr(_, []) ->
  exit({?MODULE, mk_expr_move_instr, "bad match"}).

%%=============================================================================
%% Returns a set of all expressions in the code.
%% all_exprs(_CFG, []) ->
%%   ?SETS:new();
%% all_exprs(CFG, [Label|Labels]) ->
%%   BB = hipe_rtl_cfg:bb(CFG, Label),
%%   Code = hipe_bb:code(BB),
%%   ?SETS:union(all_exprs_bb(Code),
%% 	      all_exprs(CFG, Labels)).

%%=============================================================================
%% Returns a set of expressions in a basic block.
%% all_exprs_bb([]) ->
%%   ?SETS:new();
%% all_exprs_bb([Instr|Instrs]) ->
%%   case is_expr(Instr) of
%%     true ->
%%       Expr = expr_clear_dst(Instr),
%%       ExprSet = all_exprs_bb(Instrs),
%%       ?SETS:add_element(Expr, ExprSet);
%%     false ->
%%       all_exprs_bb(Instrs)
%%   end.

%%%%%%%%%%%%%%%%%% EXPRESSION ID -> EXPRESSION MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map from expression IDs to expressions.
%%=============================================================================
%% mk_expr_id_map() ->
%%   gb_trees:empty().

%% expr_id_map_insert(Map, ExprId, Expr) ->
%%   gb_trees:insert(ExprId, Expr, Map).

%% expr_id_map_lookup(Map, ExprId) ->
%%   gb_trees:lookup(ExprId, Map).

%%=============================================================================
%% Given expression id, gets expression.
expr_id_map_get_expr(Map, ExprId) ->
  case gb_trees:lookup(ExprId, Map) of
    {value, Expr} ->
      Expr;
    none ->
      exit({?MODULE, expr_id_map_get_expr, "expression id missing"})
  end.

%%=============================================================================
%% Expression ID counter
init_expr_id() ->
  put({rtl_lcm,expr_id_count}, 0),
  ok.

-spec new_expr_id() -> non_neg_integer().
new_expr_id() ->
  Obj = {rtl_lcm, expr_id_count},
  V = get(Obj),
  put(Obj, V+1),
  V.

%%%%%%%%%%%%%%%%%% EDGE BB (INSERT BETWEEN) MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map from edges to labels.
%% This is used by insert_expr_between to remember what new bbs it has created
%% for insertions on edges, and thus for multiple insertions on the same edge
%% to end up in the same bb.
%%=============================================================================
mk_edge_bb_map() ->
  gb_trees:empty().

edge_bb_map_insert(Map, Edge, Label) ->
  gb_trees:enter(Edge, Label, Map).

edge_bb_map_lookup(Map, Edge) ->
  gb_trees:lookup(Edge, Map).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRETTY-PRINTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Prints debug messages.
-ifdef(LCM_DEBUG).

pp_debug(Str, Args) ->
  case ?LCM_DEBUG of
    true ->
      io:format(standard_io, Str, Args);
    false ->
      ok
  end.

pp_debug_instr(Instr) ->
  case ?LCM_DEBUG of
    true ->
      hipe_rtl:pp_instr(standard_io, Instr);
    false ->
      ok
  end.

-else.

pp_debug(_, _) ->  
  ok.

pp_debug_instr(_) ->  
  ok.

-endif.	%% DEBUG

%%=============================================================================
%% Pretty-prints the calculated sets for the lazy code motion.
pp_sets(_, _, _, _, _, _, []) ->
  ok; 
pp_sets(ExprMap, IdMap, NodeInfo, EdgeInfo, AllExpr, CFG, [Label|Labels]) ->
  Preds = hipe_rtl_cfg:pred(CFG, Label),
  Succs = hipe_rtl_cfg:succ(CFG, Label),

  io:format(standard_io, "Label ~w~n", [Label]),
  io:format(standard_io, "  Preds:    ~w~n", [Preds]),
  io:format(standard_io, "  Succs:    ~w~n", [Succs]),

  case up_exp(NodeInfo, Label) of
    none -> ok;
    UpExp -> 
      case ?SETS:size(UpExp) =:= 0 of
	false ->
	  io:format(standard_io, "  UEExpr: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(UpExp));
	true -> ok
      end
  end,
  case down_exp(NodeInfo, Label) of
    none -> ok;
    DownExp -> 
      case ?SETS:size(DownExp) =:= 0 of
	false ->
	  io:format(standard_io, "  DEExpr: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(DownExp));
	true -> ok
      end
  end,
  case killed_expr(NodeInfo, Label) of
    none -> ok;
    KilledExpr -> 
      case ?SETS:size(KilledExpr) =:= 0 of
	false ->
	  io:format(standard_io, "  ExprKill: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(KilledExpr));
	true -> ok
      end
  end,
  case avail_in(NodeInfo, Label) of
    none -> ok;
    AvailIn ->
      case ?SETS:size(AvailIn) =:= 0 of
	false ->
	  io:format(standard_io, "  AvailIn:  ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AvailIn));
	true -> ok
      end
  end,
  case avail_out(NodeInfo, Label) of
    none -> ok;
    AvailOut ->
      case ?SETS:size(AvailOut) =:= 0 of
	false ->
	  io:format(standard_io, "  AvailOut: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AvailOut));
	true -> ok
      end
  end,
  case antic_in(NodeInfo, Label) of
    none -> ok;
    AnticIn ->
      case ?SETS:size(AnticIn) =:= 0 of
	false ->
	  io:format(standard_io, "  AnticIn:  ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AnticIn));
	true -> ok
      end
  end,
  case antic_out(NodeInfo, Label) of
    none -> ok;
    AnticOut ->
      case ?SETS:size(AnticOut) =:= 0 of
	false ->
	  io:format(standard_io, "  AnticOut: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AnticOut));
	true -> ok
      end
  end,
  case later_in(NodeInfo, Label) of
    none -> ok;
    LaterIn ->
      case ?SETS:size(LaterIn) =:= 0 of
	false ->
	  io:format(standard_io, "  LaterIn:  ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(LaterIn));
	true -> ok
      end
  end,  

  pp_earliest(ExprMap, IdMap, EdgeInfo, Label, Succs),
  pp_later(ExprMap, IdMap, EdgeInfo, Label, Succs),

  case delete(NodeInfo, Label) of
    none -> ok;
    Delete ->
      case ?SETS:size(Delete) =:= 0 of
	false ->
	  io:format(standard_io, "  Delete:   ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(Delete));
	true -> ok
      end
  end,
  pp_sets(ExprMap, IdMap, NodeInfo, EdgeInfo, AllExpr, CFG, Labels).

%%=============================================================================
%% Pretty-prints the later set.
pp_later(_, _, _, _, []) ->
  ok;
pp_later(ExprMap, IdMap, EdgeInfo, Pred, [Succ|Succs]) ->
  case later(EdgeInfo, {Pred, Succ}) of
    none -> ok;
    Later ->
      case ?SETS:size(Later) =:= 0 of
	false ->
	  io:format(standard_io, "  Later(~w->~w): ~n", [Pred,Succ]),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(Later));
	true -> ok
      end
  end,
  pp_later(ExprMap, IdMap, EdgeInfo, Pred, Succs).

%%=============================================================================
%% Pretty-prints the earliest set.
pp_earliest(_, _, _, _, []) ->
  ok;
pp_earliest(ExprMap, IdMap, EdgeInfo, Pred, [Succ|Succs]) ->
  case earliest(EdgeInfo, {Pred, Succ}) of
    none -> ok;
    Earliest ->
      case ?SETS:size(Earliest) =:= 0 of
	false ->
	  io:format(standard_io, "  Earliest(~w->~w): ~n", [Pred,Succ]),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(Earliest));
	true -> ok
      end
  end,
  pp_earliest(ExprMap, IdMap, EdgeInfo, Pred, Succs).

%%=============================================================================
%% Pretty-prints an expression
pp_expr(ExprMap, IdMap, ExprId) ->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  hipe_rtl:pp_instr(standard_io, expr_map_get_instr(ExprMap, Expr)).

pp_exprs(_, _, []) ->
  ok;
pp_exprs(ExprMap, IdMap, [E|Es]) ->
  pp_expr(ExprMap, IdMap, E),
  pp_exprs(ExprMap, IdMap, Es).
