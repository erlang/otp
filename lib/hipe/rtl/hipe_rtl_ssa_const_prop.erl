%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ============================================================================
%%  Filename :  hipe_rtl_ssa_const_prop.erl
%%  Authors  :  Bjorn Bergman, Bjarni Juliusson
%%  Purpose  :  Perform sparse conditional constant propagation on RTL.
%%  Notes    :  Works on an SSA-converted control-flow graph.
%%
%%  History  :  * 2004-03-14: Blatantly stolen from Icode (code by
%%                  Daniel Luna and Erik Andersson) and query-replaced for RTL.
%%              * 2004-04-30: Added in the repository.
%% ============================================================================
%%
%% Exports: propagate/1.
%%
%% ============================================================================
%%
%% Some things to note:
%%
%% 1. All precoloured registers are assumed to contain bottom. We can not
%%    do anything with them since they are not in SSA-form. This might be
%%    possible to resolve in some way, but we decided to not go there.
%%
%% 2. const_labels are assumed to be bottom, we can not find the address
%%    in any nice way (that I know of, maybe someone can help ?). I
%%    suppose they don't get a value until linking (or some step that
%%    resembles it). They are only affecting bignums and floats (at least
%%    as far as I can tell), which are both stored in memory and hence
%%    not handled very well by us anyway.
%%
%% 3. can v <- Constant be removed ? I think so. all uses of v will be
%%    replaced with an immediate. So why not ?
%%
%% ============================================================================
%%
%% TODO: 
%%
%% Take care of failures in call and replace operation with apropriate
%% failure.
%%
%% Handle ifs with non-binary operators
%%
%% We want multisets for easier (and faster) creation of env->ssa_edges
%% 
%% Propagation of constant arguments when some of the arguments are bottom
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(hipe_rtl_ssa_const_prop).
-export([propagate/1]).

-include("../main/hipe.hrl").
-include("hipe_rtl.hrl").
-include("../flow/cfg.hrl").

%-define(DEBUG, true).

-ifdef(DEBUG).
-define(SCCPDBG(W), W).
-define(DEBUG_TST, true).  % make sure that we can use ?DEBUG in if-cases...
-else.
-define(DEBUG_TST, false).  % make sure that we can use ?DEBUG in if-cases...
-define(SCCPDBG(W), ok).
-endif.

%%-----------------------------------------------------------------------------
%% Include stuff shared between SCCP on Icode and RTL.
%% NOTE: Needs to appear after DEBUG is possibly defined.
%%-----------------------------------------------------------------------------

-define(CODE, hipe_rtl).
-define(CFG,  hipe_rtl_cfg).
-include("../ssa/hipe_ssa_const_prop.inc").

-type bool_lattice() :: 'true' | 'false' | 'top' | 'bottom'.

%%-----------------------------------------------------------------------------
%% Procedure : visit_expression/2
%% Purpose   : do a symbolic execution of the given instruction.  This is just
%%	       a wrapper that chooses the right function to handle a particular
%%	       instruction.
%% Arguments : Instructions - the instruction
%%             Environment  - have a guess.
%% Returns   : {FlowWorkList, SSAWorkList, Environment}
%%-----------------------------------------------------------------------------
visit_expression(Instruction, Environment) ->
  case Instruction of
    #alu{} ->
      visit_alu(Instruction, Environment);
    #alub{} ->
      visit_alub(Instruction, Environment);
    #branch{} ->
      visit_branch(Instruction, Environment);
    #call{} ->
      visit_call(Instruction, Environment);
%%    #comment{} ->
%%      visit_comment(Instruction, Environment);
%%    #enter{} ->
%%      visit_enter(Instruction, Environment);
    #fconv{} ->
      visit_fconv(Instruction, Environment);
    #fixnumop{} ->
      visit_fixnumop(Instruction, Environment);
    #fload{} ->
      visit_fload(Instruction, Environment);
    #fmove{} ->
      visit_fmove(Instruction, Environment);
    #fp{} ->
      visit_fp(Instruction, Environment);
    #fp_unop{} ->
      visit_fp_unop(Instruction, Environment);
%%    #fstore{} ->
%%      visit_fstore(Instruction, Environment);
%%    #gctest{} ->
%%      visit_gctest(Instruction, Environment);
    #goto{} ->
      visit_goto(Instruction, Environment);
    #goto_index{} ->
      visit_goto_index(Instruction, Environment);
%%    #label{} ->
%%      visit_label(Instruction, Environment);
    #load{} ->
      visit_load(Instruction, Environment);
    #load_address{} ->
      visit_load_address(Instruction, Environment);
    #load_atom{} ->
      visit_load_atom(Instruction, Environment);
    #load_word_index{} ->
      visit_load_word_index(Instruction, Environment);
    #move{} ->
      visit_move(Instruction, Environment);
    #multimove{} ->
      visit_multimove(Instruction, Environment);
%% phi-nodes are handled in scc
%%    #phi{} ->
%%      visit_phi(Instruction, Environment);
%%    #return{} ->
%%      visit_return(Instruction, Environment);
%%    #store{} ->
%%      visit_store(Instruction, Environment);
    #switch{} ->
      visit_switch(Instruction, Environment);
    _ ->
      %% label, end_try, comment, return, fail, et al
      {[], [], Environment}
  end.


%%-----------------------------------------------------------------------------
%% Procedure : set_to/3
%% Purpose   : many of the visit_<inst> functions ends in a update of the 
%%             environment (and resulting SSA-edges) this function does the 
%%             update in a nice way and formats the result so that it can be
%%             imediatly returned to visit_expression
%% Arguments : Dst - the destination may be a list of destinations.
%%             Val - the new value (bottom, or some constant).
%%             Env - the environment in which the update should be done.
%% Returns   : { FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

set_to(Dst, Val, Env) ->
  {Env1, SSAWork} = update_lattice_value({Dst, Val}, Env),
  {[], SSAWork, Env1}.

%%-----------------------------------------------------------------------------
%% Procedure : visit_branch/2
%% Purpose   : do symbolic exection of branch instructions.
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : { FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_branch(Inst, Env) -> %% Titta också på exekverbarflagga
  Val1 = lookup_lattice_value(hipe_rtl:branch_src1(Inst), Env),
  Val2 = lookup_lattice_value(hipe_rtl:branch_src2(Inst), Env),
  CFGWL = case evaluate_relop(Val1, hipe_rtl:branch_cond(Inst), Val2) of
            true   -> [hipe_rtl:branch_true_label(Inst)];
            false  -> [hipe_rtl:branch_false_label(Inst)];
            bottom -> [hipe_rtl:branch_true_label(Inst), 
	               hipe_rtl:branch_false_label(Inst)];
            top    -> []
          end,
  {CFGWL, [], Env}.

%%-----------------------------------------------------------------------------
%% Procedure : evaluate_relop/3
%% Purpose   : evaluate the given relop. While taking care to handle top & 
%%             bottom in some sane way.
%% Arguments : Val1, Val2 - The operands Integers or top or bottom
%%             RelOp  - some relop atom from rtl. 
%% Returns   : bottom, top, true or false
%%-----------------------------------------------------------------------------

evaluate_relop(Val1, RelOp, Val2) ->
  if 
    (Val1==bottom) or (Val2==bottom) -> bottom ;
    (Val1==top) or (Val2==top)       ->  top;
    true ->  hipe_rtl_arch:eval_cond(RelOp, Val1, Val2)
  end.

%%-----------------------------------------------------------------------------
%% Procedure : evaluate_fixnumop/2 
%% Purpose   : try to evaluate a fixnumop.
%% Arguments : Val1 - operand (an integer, 'top' or 'bottom')
%%             Op - the operation.
%% Returns   : Result
%%              where result is an integer, 'top' or 'bottom'
%%-----------------------------------------------------------------------------

evaluate_fixnumop(Val1, Op) ->
  if Val1 =:= top ->
      top;
     Val1 =:= bottom ->
      bottom;
     is_integer(Val1) ->
      case Op of
	tag ->
	  hipe_tagscheme:mk_fixnum(Val1);
	untag ->
	  hipe_tagscheme:fixnum_val(Val1)
      end
  end.	

%%-----------------------------------------------------------------------------
%% Procedure : visit_alu/2
%% Purpose   : do symbolic exection of a alu
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : { FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_alu(Inst, Env) ->
  Val1 = lookup_lattice_value(hipe_rtl:alu_src1(Inst), Env),
  Val2 = lookup_lattice_value(hipe_rtl:alu_src2(Inst), Env),
  {NewVal, _, _, _, _} = evaluate_alu(Val1, hipe_rtl:alu_op(Inst), Val2),
  set_to(hipe_rtl:alu_dst(Inst), NewVal, Env).

%% Here follows the alu-evaluation stuff. This is the most involved part I
%% guess. The function that you may want to use is evaluate_alu/3. The 
%% evaluation functions returns 
%%  { Result, SignFlag, ZeroFlag, Overflow flag, CarryBit}
%% it uses some helpers which are explained breifly:
%% lattice_meet/2 - handles the general case of most alu-operations, called 
%%                  when at least one of the operands is nonconstant, and the
%%                  operation-specifics have been taken care of.
%% all_ones/0     - returns the value of a rtl-word set to all 1 bits.
%% partial_eval_alu - tries to catch some operation specific special cases 
%%                    when one (or both) of the operands is nonconstant.

lattice_meet(Val1, Val2) ->
  M = if (Val1 =:= top) or (Val2 =:= top) -> top;
         (Val1 =:= bottom) or (Val2 =:= bottom) -> bottom
	 % the check is realy just sanity
      end,
  {M, M, M, M, M}.

all_ones() ->
  (1 bsl ?bytes_to_bits(hipe_rtl_arch:word_size())) - 1.

%% when calling partial_eval*() we know that at least one of the Values 
%% are bottom or top. They return { Value, Sign, Zero, Overflow, Carry }. 
%% (just like hipe_rtl_arch:eval_alu)

%% logic shifts are very similar each other. Limit is the number of
%% bits in the words.
partial_eval_shift(Limit, Val1, Val2) ->
  if 
    Val2 =:= 0 -> {Val1, Val1, Val1, Val1, Val1};
    Val1 =:= 0 -> {0, false, true, false, false};
    is_integer(Val2), Val2 >= Limit -> % (Val2 =/= top) and (Val2 =/= bottom)
      {0, false, true, Val1, Val1}; % OVerflow & carry we dont know about.
    true -> lattice_meet(Val1, Val2)
  end.

%%-----------------------------------------------------------------------------
%% Procedure : partial_eval_alu/3
%% Purpose   : try to evaluate as much as possible an alu operation where at 
%%             least one of the operands is not constant.
%% Arguments : Val1, Val2 - operands (integer, top or bottom)
%%             Op  - the operation.
%% Returns   : {Result, Sign, Zero, Overflow, Carry}
%%              where Result is an integer, 'top' or 'bottom'
%%              and the others are bool, 'top' or 'bottom'.
%%-----------------------------------------------------------------------------

partial_eval_alu(Val1, add, Val2) ->
  if 
    (Val1 == 0) -> {Val2,  Val2, Val2, false, false};
    (Val2 == 0) -> {Val1,  Val1, Val1, false, false};
    true -> lattice_meet(Val1, Val2)
  end;
partial_eval_alu(Val1, sub, Val2) ->
  if 
    (Val2 == 0) -> {Val1,  Val1, Val1, false, false};
    true -> lattice_meet(Val1, Val2)
  end;
partial_eval_alu(Val1, 'or', Val2) ->
  All_ones = all_ones(),
  if 
    (Val1 == 0) -> {Val2,  Val2, Val2, false, false};
    (Val2 == 0) -> {Val1,  Val1, Val1, false, false};
    (Val1 == All_ones) or (Val2 == All_ones) -> 
      {All_ones,  true, false, false, false};
    true -> lattice_meet(Val1, Val2)
  end;
partial_eval_alu(Val1, 'and', Val2) ->
  All_ones = all_ones(),
  if 
    Val1 == All_ones -> {Val2,  Val2, Val2, false, false};
    Val2 == All_ones -> {Val1,  Val1, Val1, false, false};
    (Val1 == 0) or (Val2 == 0) -> {0,  false, true, false, false};
    true -> lattice_meet(Val1, Val2)
  end;
partial_eval_alu(Val1, 'xor', Val2) ->
  if
    (Val1 == 0) -> {Val2,  Val2, Val2, false, false};
    (Val2 == 0) -> {Val1,  Val1, Val1, false, false};
    true -> lattice_meet(Val1, Val2)
  end;
partial_eval_alu(Val1, 'xornot', Val2) ->
  All_ones = all_ones(),
  if
    Val1 == All_ones -> {Val2,  Val2, Val2, false, false};
    Val2 == All_ones -> {Val1,  Val1, Val1, false, false};
    true -> lattice_meet(Val1, Val2)
  end;
partial_eval_alu(Val1, andnot, Val2) ->
  All_ones = all_ones(),
  if 
    (Val2 == 0) -> {Val1,  Val1, Val1, false, false};
    (Val1 == 0) or (Val2 == All_ones) -> {0,  false, true, false, false};
    true -> lattice_meet(Val1, Val2)
  end;
partial_eval_alu(Val1, Op, Val2) when (Op =:= 'sll') or (Op =:= 'srl') ->
  BitSize = ?bytes_to_bits(hipe_rtl_arch:word_size()),
  partial_eval_shift(BitSize, Val1, Val2);
partial_eval_alu(Val1, Op, Val2) when (Op =:= 'sllx') or (Op =:= 'srlx') ->
  partial_eval_shift(64, Val1, Val2);
partial_eval_alu(Val1, mul, Val2) -> lattice_meet(Val1, Val2); % XXX: suboptimal

% arithmetic shifts are more tricky, shifting something unknown can
% generate all_ones() and 0 depenging on the sign of Val1.
partial_eval_alu(Val1, Op, Val2) when (Op =:= 'sra') or (Op =:= 'srax') ->
  if 
    (Val2 == 0) -> {Val1,  Val1, Val1, false, false};
    (Val1 == 0) -> {0, false, true, false, false};
    true -> lattice_meet(Val1, Val2)
  end.

%%-----------------------------------------------------------------------------
%% Procedure : evaluate_alu/3 
%% Purpose   : try to evaluate as much as possible of a alu operation.
%% Arguments : Val1, Val2 - operands (an integer, 'top' or 'bottom')
%%             Op - the operation.
%% Returns   : {Result, Sign, Zero, Overflow, Carry}
%%              where result is an integer, 'top' or 'bottom'
%%              and the others are Bool, 'top' or 'bottom'.
%%-----------------------------------------------------------------------------

evaluate_alu(Val1, Op, Val2) ->
  if 
    (Val1 =:= top) or (Val2 =:= top) or 
    (Val1 =:= bottom) or (Val2 =:= bottom) -> partial_eval_alu(Val1, Op, Val2);
    true ->
      case Op of
        sllx -> hipe_rtl_arith_64:eval_alu('sll', Val1, Val2);
        srlx -> hipe_rtl_arith_64:eval_alu('srl', Val1, Val2); 
        srax -> hipe_rtl_arith_64:eval_alu('sra', Val1, Val2);
        _    -> hipe_rtl_arch:eval_alu(Op, Val1, Val2)
      end
  end.

maybe_top_or_bottom(List) ->
  maybe_top_or_bottom(List, false).

maybe_top_or_bottom([],          TB) -> TB;
maybe_top_or_bottom([top | Rest], _) -> maybe_top_or_bottom(Rest, top);
maybe_top_or_bottom([bottom | _], _) -> bottom;
maybe_top_or_bottom([_ | Rest],  TB) -> maybe_top_or_bottom(Rest, TB).

-spec partial_eval_branch(hipe_rtl:alub_cond(), bool_lattice(), bool_lattice(),
			  bool_lattice() | 0, bool_lattice() | 0) ->
	 bool_lattice().
partial_eval_branch(Cond, N0, Z0, V0, C0) ->
  {N, Z, V, C} =
    if Cond =:= 'eq';
       Cond =:= 'ne'           -> {true, Z0,   true, true};
       Cond =:= 'gt';
       Cond =:= 'le'           -> {N0,   Z0,   V0,   true};
       Cond =:= 'gtu'          -> {true, Z0,   true, C0  };
       Cond =:= 'lt';
       Cond =:= 'ge'           -> {N0,   true, V0,   true};
       Cond =:= 'geu';
       Cond =:= 'ltu'          -> {true, true, true, C0  };
       Cond =:= 'overflow';
       Cond =:= 'not_overflow' -> {true, true, V0,   true}
    end,
  case maybe_top_or_bottom([N, Z, V, C]) of
    false  -> hipe_rtl_arch:eval_cond_bits(Cond, N, Z, V, C);
    top    -> top;
    bottom -> bottom
  end.

%%-----------------------------------------------------------------------------
%% Procedure : visit_alub/2
%% Purpose   : do symbolic exection of a alub instruction
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : { FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_alub(Inst, Env) ->
  Val1 = lookup_lattice_value(hipe_rtl:alub_src1(Inst), Env),
  Val2 = lookup_lattice_value(hipe_rtl:alub_src2(Inst), Env),
  {NewVal, N, Z, C, V} = evaluate_alu(Val1, hipe_rtl:alub_op(Inst), Val2),
  Labels = 
    case NewVal of
      bottom -> [hipe_rtl:alub_true_label(Inst), 
                 hipe_rtl:alub_false_label(Inst)];
      top    -> [];
      _      ->
        %% if the partial branch cannot be evaluated we must execute the
        %% instruction at runtime.
        case partial_eval_branch(hipe_rtl:alub_cond(Inst), N, Z, C, V) of
          bottom -> [hipe_rtl:alub_true_label(Inst), 
                     hipe_rtl:alub_false_label(Inst)];
          top    -> [];
          true   -> [hipe_rtl:alub_true_label(Inst)];
          false  -> [hipe_rtl:alub_false_label(Inst)]
        end
     end,
  {[], NewSSA, NewEnv} = set_to(hipe_rtl:alub_dst(Inst), NewVal,  Env),
  {Labels, NewSSA, NewEnv}.
      
%%-----------------------------------------------------------------------------
%% Procedure : visit_fixnumop/2
%% Purpose   : do symbolic exection of a fixnumop instruction.
%%             fixnumop is like a specialized alu.
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : { FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_fixnumop(Inst, Env) ->
  Val = lookup_lattice_value(hipe_rtl:fixnumop_src(Inst), Env),
  Res = evaluate_fixnumop(Val, hipe_rtl:fixnumop_type(Inst)),
  set_to(hipe_rtl:fixnumop_dst(Inst), Res, Env).

%%-----------------------------------------------------------------------------
%% Procedure : visit_f*
%% Purpose   : Do symbolic execution of floating point instructions.
%%             All floating-point hitngs are mapped to bottom. In order to 
%%             implement them we would have to add hipe_rtl_arch:eval_f* 
%%             instructions since floating point is no exact science.
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_fconv(Inst, Env) ->
  set_to(hipe_rtl:fconv_dst(Inst), bottom, Env).

visit_fp(Inst, Env) ->
  set_to(hipe_rtl:fp_dst(Inst), bottom, Env).

visit_fp_unop(Inst, Env) ->
  set_to(hipe_rtl:fp_unop_dst(Inst), bottom, Env).

visit_fload(Inst, Env) ->
  set_to(hipe_rtl:fload_dst(Inst), bottom, Env).

visit_fmove(Inst, Env) ->
  set_to(hipe_rtl:fmove_dst(Inst), bottom, Env).

%%-----------------------------------------------------------------------------
%% Procedure : visit_move/2
%% Purpose   : execute a register-copy
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_move(Inst, Env) ->
  Src = hipe_rtl:move_src(Inst),
  Dst = hipe_rtl:move_dst(Inst),
  set_to(Dst, lookup_lattice_value(Src, Env), Env).

%%-----------------------------------------------------------------------------
%% Procedure : visit_goto/2
%% Purpose   : execute a goto
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_goto(Instruction, Environment) ->
  GotoLabel = hipe_rtl:goto_label(Instruction),
  {[GotoLabel], [], Environment}.

%%-----------------------------------------------------------------------------
%% Procedure : visit_goto_index/2
%% Purpose   : execute a goto_index
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_goto_index(Inst, Env) ->
  Index = hipe_rtl:goto_index_index(Inst),
  case lookup_lattice_value(Index, Env) of 
    top    ->   { [], [], Env };
    bottom -> %% everything is reachable
      { hipe_rtl:goto_index_labels(Inst), [], Env };
    I   -> %% only the ith label will be taken.
      io:format("hipe_rtl_ssa_const_prop foud goto-index with constant index ~w in ~w\n",
                [I, Inst]),      
      { [ lists:nth(hipe_rtl:goto_index_labels(Inst), I) ], [], Env }
  end.

%%-----------------------------------------------------------------------------
%% Procedure : visit_load/2
%% Purpose   : do a visit_load. Its hard to track whats in memory, and it's 
%%             not in ssa form, so let's assume bottom-values !
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_load(Inst, Env) ->
  set_to(hipe_rtl:load_dst(Inst), bottom, Env).

%%-----------------------------------------------------------------------------
%% Procedure : visit_load_address/2
%% Purpose   : execute a load_address instruction, while there might be things 
%%             here that are runtime-constant they are not compile-time
%%             constant since code loading interferes with addresses.
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_load_address(Inst, Env) ->
  Dst = hipe_rtl:load_address_dst(Inst),
  Val = bottom, %% all these are probably run-time, but not
                %% compile-time constants
  set_to(Dst, Val, Env).

%%-----------------------------------------------------------------------------
%% Procedure : visit_load_atom/2
%% Purpose   : Like loadadress this one gets something that is not 
%%             compiletime-constant
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_load_atom(Inst, Env) ->
  set_to(hipe_rtl:load_atom_dst(Inst), bottom, Env).
  
%%-----------------------------------------------------------------------------
%% Procedure : visit_load_word_index/2
%% Purpose   : execute a load_word_index. Here is probably room for 
%%             improvement, we should be able to find some constants here, 
%%             since we can get the labeled values from the environment, and 
%%             then find the value with the given index.
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_load_word_index(Inst, Env) ->
  io:format(" this is load word index: ~w\n", [Inst]),
  set_to(hipe_rtl:load_word_index_dst(Inst), bottom, Env).

%%-----------------------------------------------------------------------------
%% Procedure : visit_multimove/2 & visit_multimove/4
%% Purpose   : execute a multimove instruction. 
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_multimove([Dst | Dsts], [Val | Vals], MyEnv, MySSA) ->
  {NewEnv, NewSSA} = update_lattice_value({Dst, Val}, MyEnv),
  visit_multimove(Dsts, Vals, NewEnv, MySSA ++ NewSSA);
visit_multimove([], [], MyEnv, MySSA) ->
  {MyEnv, MySSA}.

visit_multimove(Inst, Env) ->
  Srcs = [lookup_lattice_value(S, Env) || 
	   S <- hipe_rtl:multimove_srclist(Inst)],
  {NewEnv, NewSSA} = visit_multimove(hipe_rtl:multimove_dstlist(Inst),
				     Srcs, Env, []),
  {[], NewSSA, NewEnv}.
  
%%-----------------------------------------------------------------------------
%% Procedure : visit_call/2
%% Purpose   : execute a call-instruction. All calls return bottom. We make 
%%             this assumption since the icode-leel have taken care of BIF's
%%             and we belive that we are left with the things that can not be
%%             done att compile time.
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

visit_call(Inst, Env) ->
  {Env1, SSAWork} =
    update_lattice_value({hipe_rtl:call_dstlist(Inst), bottom}, Env),
    % remeber to add both continuation & failto things to the cfgwl
  Cont = case hipe_rtl:call_continuation(Inst) of
	   [] -> [];
	   C  -> [C]
         end,
  Succ = case hipe_rtl:call_fail(Inst) of
	   [] -> Cont;
	   Fail -> [Fail | Cont]
         end,
  {Succ, SSAWork, Env1}.

%%-----------------------------------------------------------------------------
%% Procedure : visit_switch/2
%% Purpose   : execute a switch-statement. 
%% Arguments : Inst - The instruction
%%             Env  - The environment
%% Returns   : {FlowWorkList, SSAWorkList, NewEnvironment}
%%-----------------------------------------------------------------------------

%% first two helpers that are used to handle the mapping from value to label.
%% why isn't there a function that does this ?

find_switch_label(Inst, Val) ->
  Labels = hipe_rtl:switch_labels(Inst),
  ?SCCPDBG(io:format("finding switch_label, ~w in ~w\n", [Val,Inst])),
  %% it seems like the index is zero based. nth uses 1-based indexing.
  lists:nth(Val + 1, Labels).

%% Switches seem tricky. the sort-order is a list of key-values to be
%% tested in order. (if elem i matches then we should jump to elem i of
%% the labels-list)
visit_switch(Inst, Env) ->
  case lookup_lattice_value(hipe_rtl:switch_src(Inst), Env) of
    top ->
      {[], [], Env};
    bottom ->
      {hipe_rtl:switch_labels(Inst), [], Env};
    Val ->
      {[find_switch_label(Inst, Val) ], [], Env}
  end.

%%-----------------------------------------------------------------------------
%% Procedure : update_instruction/2
%% Purpose   : update the given instruction using any information found in 
%%             the environment.
%% Arguments : Inst - the instruction
%%             Environment - in which everything happens.
%% Returns   : list of new instructions.
%%-----------------------------------------------------------------------------

%% idea: what to do with vi <- Constant. wouldn't it be possible to
%% remove those ? (and similarily for alu-instructions. and alub
%% instructions also ! (of course this will be done in some later step dead
%%  code elimination ? but it's a simple check.)
update_instruction(Inst, Env) ->
  case Inst of 
    #alu{} ->
      update_alu(Inst, Env);
    #alub{} ->
      update_alub(Inst, Env);
    #branch{} ->
      update_branch(Inst, Env);
    #call{} ->
      subst_all_uses(Inst, Env);
%%    #comment{} ->
%%      [Inst];
    #enter{} ->
      subst_all_uses(Inst, Env);
    #fconv{} ->
      subst_all_uses(Inst, Env);
    #fload{} ->
      subst_all_uses(Inst, Env);
    #fmove{} ->
      subst_all_uses(Inst, Env);
    #fp{} ->
      subst_all_uses(Inst, Env);
    #fp_unop{} ->
      subst_all_uses(Inst, Env);
    #fstore{} ->
      subst_all_uses(Inst, Env);
    #gctest{} ->
      subst_all_uses(Inst, Env);
%%    #goto{} ->
%%       [ Inst ];
    #goto_index{} ->
      update_goto_index(Inst, Env);
%%    #label{} ->
%%      [ Inst ];
    #load{} ->
      subst_all_uses(Inst, Env);
    #load_address{} ->
      subst_all_uses(Inst, Env);
    #load_atom{} ->
      subst_all_uses(Inst, Env);
    #load_word_index{} ->
      subst_all_uses(Inst, Env);
    #move{} ->
      subst_all_uses(Inst, Env);
    #multimove{} ->
      subst_all_uses(Inst, Env);
    #return{} ->
      subst_all_uses(Inst, Env);
    #store{} ->
      subst_all_uses(Inst, Env);
    #switch{} ->
      update_switch(Inst, Env);
    #phi{} ->
      update_phi(Inst, Env);
    _ ->  % for the others it's sufficient to just update any thing they use.
      [ Inst ]
  end.

%%-----------------------------------------------------------------------------
%% Procedure : subst_uses/2
%% Purpose   : looks up all things that an instruction uses and replaces
%%             anything that is determined to be constant.
%% Arguments : Inst - the instruction
%%             Env - in which everything happen.
%% Returns   : list of instructions to replace Inst with.
%%-----------------------------------------------------------------------------

subst_all_uses(Inst, Env) ->
  Uses = hipe_rtl_ssa:uses_to_rename(Inst),
  [ hipe_rtl:subst_uses(update_srcs(Uses, Env), Inst) ].

%%-----------------------------------------------------------------------------
%% Procedure : update_srcs/2
%% Purpose   : given the things that a instruction use return a list 
%%             {Src, NewValue} pairs that can be sent to subs_uses.
%% Arguments : Srcs - list of uses
%%             Env - in which everything happens.
%% Returns   : list of {Src, NewValue} pairs.
%%-----------------------------------------------------------------------------

update_srcs(Srcs, Env) ->
  Update = 
    fun(Src, Os) ->
      case lookup_lattice_value(Src, Env) of 
        bottom -> Os;
        top -> % this would be realy strange.
          ?EXIT({"update_src, top", Src });
        Constant ->
          [ {Src, hipe_rtl:mk_imm(Constant)} | Os]
      end
    end,
  lists:foldl(Update, [], Srcs ).
  
%%-----------------------------------------------------------------------------
%% functions for performing partial evaluation of alu-operations. They can 
%% return either an integer (the actual result), move_src1 or move_src2 in 
%% which case the alu-operation can be replace with a move, or keep_it in 
%% which case the instruction must be kept.

%%-----------------------------------------------------------------------------
%% Procedure : partial_update_shift/3
%% Purpose   : perform a shift
%% Arguments : Limit - the number of bits in the word to shift.
%%             Val1 - the shiftee
%%             Val2 - number of bits to shift
%% Returns   : Integer, move_src1, keep_it
%%-----------------------------------------------------------------------------

partial_update_shift(Limit, Val1, Val2) ->
  if
    (Val1 =:= bottom) and (Val2 =:= 0) -> move_src1;
    (Val1 =:= 0) or ((Val2 =/= bottom) and (Val2 >= Limit)) -> 0;
    true -> keep_it
  end.

%%-----------------------------------------------------------------------------
%% Procedure : partial_update_alu/3
%% Purpose   : perform as much of alu-operations where exatcly one of the
%%             operands is bottom.
%% Arguments : Val1, Val2 - operands
%%             Op - the operation.
%% Returns   : Integer, move_src1, move_src2, keep_it
%%-----------------------------------------------------------------------------

%% we know that exactly one of the operands are bottom this one
%% returns what to do with the instruction (it's either replace with
%% src1, replace src2 replace with constant or keep it.

partial_update_alu(Val1, 'add', Val2) ->
  if
    (Val1 == 0) -> move_src2;
    (Val2 == 0) -> move_src1;
    true -> keep_it
  end;
partial_update_alu(_Val1, 'sub', Val2) ->
  if
    (Val2 == 0) -> move_src1;
    true -> keep_it
  end;
partial_update_alu(Val1, 'or', Val2) ->
  All_ones = all_ones(),
  if 
    (Val1 == 0) -> move_src2;
    (Val2 == 0) -> move_src1;
    (Val1 == All_ones) or (Val2 == All_ones) -> All_ones;
    true -> keep_it
  end;
partial_update_alu(Val1, 'and', Val2) ->
  All_ones = all_ones(),
  if 
    Val1 == All_ones -> move_src2;
    Val2 == All_ones -> move_src1;
    (Val1 == 0) or (Val2 == 0) -> 0;
    true -> keep_it
  end;
partial_update_alu(Val1, 'xor', Val2) ->
  if 
    (Val1 == 0) -> move_src2;
    (Val2 == 0) -> move_src1;
    true -> keep_it
  end;
partial_update_alu(Val1, 'xornot', Val2) ->
  All_ones = all_ones(),
  if 
    (Val1 == All_ones) -> move_src2;
    (Val2 == All_ones) -> move_src1;
    true -> keep_it
  end;
partial_update_alu(Val1, andnot, Val2) ->
  All_ones = all_ones(),
  if 
    Val2 == 0 -> move_src1;
    (Val1 == 0) or (Val2 == All_ones) -> 0;
    true -> keep_it
  end;
partial_update_alu(Val1, Op, Val2) when (Op =:= 'sll') or (Op =:= 'srl') ->
  BitSize = ?bytes_to_bits(hipe_rtl_arch:word_size()),
  partial_update_shift(BitSize, Val1, Val2);
partial_update_alu(Val1, Op, Val2) when (Op =:= 'sllx') or (Op =:= 'srlx') ->
  partial_update_shift(64, Val1, Val2);
partial_update_alu(Val1, Op, Val2) when (Op =:= 'sra') or (Op =:= 'srax') ->
  if 
    Val2 == 0 -> move_src1;
    Val1 == 0 -> 0;
    true -> keep_it
  end.

%%-----------------------------------------------------------------------------
%% Procedure : update_alu/2
%% Purpose   : update an alu-instruction.
%% Arguments : Inst - the instruction.
%%             Env - in which everything happens.
%% Returns   : list of new instruction
%%-----------------------------------------------------------------------------

update_alu(Inst, Env) ->
  Val1 = lookup_lattice_value(hipe_rtl:alu_src1(Inst), Env),
  Val2 = lookup_lattice_value(hipe_rtl:alu_src2(Inst), Env),
  if 
    (Val1 =:= bottom) and (Val2 =:= bottom) ->
      [Inst];
    (Val1 =:= bottom) or (Val2 =:= bottom) ->
      NewInst =
	case partial_update_alu(Val1, hipe_rtl:alu_op(Inst), Val2) of
          move_src1 -> 
            hipe_rtl:mk_move(hipe_rtl:alu_dst(Inst), hipe_rtl:alu_src1(Inst));
          move_src2 ->
            hipe_rtl:mk_move(hipe_rtl:alu_dst(Inst), hipe_rtl:alu_src2(Inst));
          keep_it ->
            S1 = make_alub_subst_list(Val1, hipe_rtl:alu_src1(Inst), []),
            S2 = make_alub_subst_list(Val2, hipe_rtl:alu_src2(Inst), S1),
            hipe_rtl:subst_uses(S2, Inst);
          Constant ->
            hipe_rtl:mk_move(hipe_rtl:alu_dst(Inst), hipe_rtl:mk_imm(Constant))
        end,
      [NewInst];
    true ->
      {Val,_,_,_,_} = evaluate_alu(Val1, hipe_rtl:alu_op(Inst), Val2),
      [hipe_rtl:mk_move(hipe_rtl:alu_dst(Inst), hipe_rtl:mk_imm(Val))]
  end.
 
%%-----------------------------------------------------------------------------
%% Procedure : update_branch/2
%% Purpose   : update an branch-instruction
%% Arguments : Inst - the instruction.
%%             Env - in which everything happens.
%% Returns   : list of new instruction
%%-----------------------------------------------------------------------------

update_branch(Inst, Env) ->
  Src1 = hipe_rtl:branch_src1(Inst),
  Src2 = hipe_rtl:branch_src2(Inst),
  Val1 = lookup_lattice_value(Src1, Env),
  Val2 = lookup_lattice_value(Src2, Env),
  if
    (Val1 =:= bottom) and (Val2 =:= bottom) ->
      [Inst];
    Val1 =:= bottom ->
      [hipe_rtl:subst_uses([{Src2, hipe_rtl:mk_imm(Val2)}], Inst)];
    Val2 =:= bottom -> 
      [hipe_rtl:subst_uses([{Src1, hipe_rtl:mk_imm(Val1)}], Inst)];
    true ->
      case hipe_rtl_arch:eval_cond(hipe_rtl:branch_cond(Inst), Val1, Val2) of
        true  -> [hipe_rtl:mk_goto(hipe_rtl:branch_true_label(Inst))];
        false -> [hipe_rtl:mk_goto(hipe_rtl:branch_false_label(Inst))]
      end
  end.

%%-----------------------------------------------------------------------------
%% Procedure : update_alub/2
%% Purpose   : update an alub-instruction. Here are some finer points, we might
%%             be able to do the math (think b = a+0), but it's hard to replace
%%             the branch, since the mapping b/w AluOp,RelOp to BranchInstr is
%%             boring to do. (lazyness is a bliss).
%% Arguments : Inst - the instruction.
%%             Env - in which everything happens.
%% Returns   : list of new instructions
%%-----------------------------------------------------------------------------

%% some small helpers.
alub_to_move(Inst, Res, Lab) ->
  [hipe_rtl:mk_move(hipe_rtl:alub_dst(Inst), Res),
   hipe_rtl:mk_goto(Lab)].

make_alub_subst_list(bottom, _, Tail) ->  Tail;
make_alub_subst_list(top, Src, _) ->
  ?EXIT({"~w is top during update",Src });
make_alub_subst_list(Val, Src, Tail)  -> 
  case hipe_rtl:is_imm(Src) of
    true -> Tail;
    false -> [{Src, hipe_rtl:mk_imm(Val)} | Tail]
  end.

update_alub(Inst, Env) ->
  Src1 = hipe_rtl:alub_src1(Inst),
  Src2 = hipe_rtl:alub_src2(Inst),
  Val1 = lookup_lattice_value(Src1, Env),
  Val2 = lookup_lattice_value(Src2, Env),
  {ResVal, N, Z, C, V}  = evaluate_alu(Val1, hipe_rtl:alub_op(Inst), Val2),  
  CondRes = partial_eval_branch(hipe_rtl:alub_cond(Inst), N, Z, C, V),
  case CondRes of
    bottom -> 
      %% if we can't evaluate the branch, we have to keep it as a alub isnt
      %% since other optimizations might insert other instructions b/w the 
      %% move and the branch. We can however replace variable with constants:
      S1 = make_alub_subst_list(Val1, Src1, []),
      S2 = make_alub_subst_list(Val2, Src2, S1),
      [hipe_rtl:subst_uses(S2, Inst)];
    _ -> %% we know where we will be going, let's find out what Dst should be.
         %% knowing where we are going means that at most one of the values is
         %% bottom, hence we can replace the alu-instr with a move.
         %% remember, a = b + 0 can give us enough info to know what jump to
         %% do without knowing the value of a. (I wonder if this will ever
         %% actualy happen ;)
      Res = case ResVal of 
              bottom ->  % something nonconstant.
                if (Val1 =:= bottom) -> Src1;
	           (Val2 =:= bottom) -> Src2 
                end;
              _ -> hipe_rtl:mk_imm(ResVal)
            end,
      case CondRes of 
        top ->
	  io:format("oops. something VERY bad: ~w ~w V1 & 2 ~w ~w\n",
		    [Inst, {ResVal, N, Z, C, V} , Val1, Val2]),
	  [Inst];
        true  -> alub_to_move(Inst, Res, hipe_rtl:alub_true_label(Inst));
        false -> alub_to_move(Inst, Res, hipe_rtl:alub_false_label(Inst))
      end
  end.

%%-----------------------------------------------------------------------------
%% Procedure : update_goto_index/2
%% Purpose   : update a goto_index instruction.
%% Arguments : Inst - the instruction.
%%             Env  - in which everything happens.
%% Returns   : list of new instructions.
%%-----------------------------------------------------------------------------

update_goto_index(Inst, Env) ->
  Index = hipe_rtl:goto_index_index(Inst),
  case lookup_lattice_value(Index, Env) of 
    bottom -> %% everything is reachable
      [Inst];
    I -> %% only the ith label will be taken.
      [hipe_rtl:mk_goto(lists:nth(hipe_rtl:goto_index_labels(Inst), I))]
  end.

%%-----------------------------------------------------------------------------
%% Procedure : update_switch/2
%% Purpose   : update a switch instruction.
%% Arguments : Inst - the instruction.
%%             Env - in which everything happens.
%% Returns   : list of new instructions.
%%-----------------------------------------------------------------------------

update_switch(Inst, Env) ->
  case lookup_lattice_value(hipe_rtl:switch_src(Inst), Env) of
    bottom ->
      [Inst];
    Const ->
      Lab = find_switch_label(Inst, Const),
      [hipe_rtl:mk_goto(Lab)]
  end.

%%-----------------------------------------------------------------------------
%% Procedure : update_phi/3
%% Purpose   : Update a phi-function w.r.t. constants. do nothing for now.
%% Arguments : Instruction - The instruction
%%             Environment - The environment
%% Returns   : [NewInstruction]
%%-----------------------------------------------------------------------------

update_phi(Instruction, Environment) ->
  Destination = hipe_rtl:phi_dst(Instruction),
  case lookup_lattice_value(Destination, Environment) of
    bottom -> 
      [Instruction];
    top -> 
      ?WARNING_MSG("The dst of ~w is top after SCCP. Strange\n",[Instruction]),
      ?EXIT({"bang !", Instruction}),
      [Instruction];
    Value ->
      [hipe_rtl:mk_move(Destination, hipe_rtl:mk_imm(Value))]
  end.

%%-----------------------------------------------------------------------------

%% make sure that all precoloured registers are taken out of the equation.
lookup_lattice_value(X, Environment) ->
  case hipe_rtl_arch:is_precoloured(X) or hipe_rtl:is_const_label(X) of 
    true ->
      bottom;
    false ->
      lookup_lattice_value2(X, Environment)
  end.

lookup_lattice_value2(X, Environment) ->
  LatticeValues = env__lattice_values(Environment),
  case hipe_rtl:is_imm(X) of
    true ->
      hipe_rtl:imm_value(X);
    false ->
      case gb_trees:lookup(X, LatticeValues) of
        none ->
	  io:format("~w~n",[LatticeValues]),
          ?WARNING_MSG("Earlier compiler steps generated erroneous " 
                       "code for X = ~w. We are ignoring this.\n",[X]),
          bottom;
        {value, top} ->
          ?EXIT({"lookup_lattice_value, top", X}),
          top;
        {value, Y} ->
          Y
      end
  end.

%%----------------------------- End of file -----------------------------------
