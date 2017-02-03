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

-module(hipe_rtl_to_arm).
-export([translate/1]).

-include("../rtl/hipe_rtl.hrl").

translate(RTL) ->
  hipe_gensym:init(arm),
  hipe_gensym:set_var(arm, hipe_arm_registers:first_virtual()),
  hipe_gensym:set_label(arm, hipe_gensym:get_label(rtl)),
  Map0 = vmap_empty(),
  {Formals, Map1} = conv_formals(hipe_rtl:rtl_params(RTL), Map0),
  OldData = hipe_rtl:rtl_data(RTL),
  {Code0, NewData} = conv_insn_list(hipe_rtl:rtl_code(RTL), Map1, OldData),
  {RegFormals,_} = split_args(Formals),
  Code =
    case RegFormals of
      [] -> Code0;
      _ -> [hipe_arm:mk_label(hipe_gensym:get_next_label(arm)) |
	    move_formals(RegFormals, Code0)]
    end,
  IsClosure = hipe_rtl:rtl_is_closure(RTL),
  IsLeaf = hipe_rtl:rtl_is_leaf(RTL),
  hipe_arm:mk_defun(hipe_rtl:rtl_fun(RTL),
		    Formals,
		    IsClosure,
		    IsLeaf,
		    Code,
		    NewData,
		    [], 
		    []).

conv_insn_list([H|T], Map, Data) ->
  {NewH, NewMap, NewData1} = conv_insn(H, Map, Data),
  %% io:format("~w \n  ==>\n ~w\n- - - - - - - - -\n",[H,NewH]),
  {NewT, NewData2} = conv_insn_list(T, NewMap, NewData1),
  {NewH ++ NewT, NewData2};
conv_insn_list([], _, Data) ->
  {[], Data}.

conv_insn(I, Map, Data) ->
  case I of
    #alu{} -> conv_alu(I, Map, Data);
    #alub{} -> conv_alub(I, Map, Data);
    #call{} -> conv_call(I, Map, Data);
    #comment{} -> conv_comment(I, Map, Data);
    #enter{} -> conv_enter(I, Map, Data);
    #goto{} -> conv_goto(I, Map, Data);
    #label{} -> conv_label(I, Map, Data);
    #load{} -> conv_load(I, Map, Data);
    #load_address{} -> conv_load_address(I, Map, Data);
    #load_atom{} -> conv_load_atom(I, Map, Data);
    #move{} -> conv_move(I, Map, Data);
    #return{} -> conv_return(I, Map, Data);
    #store{} -> conv_store(I, Map, Data);
    #switch{} -> conv_switch(I, Map, Data);
    _ -> exit({?MODULE,conv_insn,I})
  end.

conv_alu(I, Map, Data) ->
  %% dst = src1 aluop src2
  {Dst, Map0} = conv_dst(hipe_rtl:alu_dst(I), Map),
  {Src1, Map1} = conv_src(hipe_rtl:alu_src1(I), Map0),
  {Src2, Map2} = conv_src(hipe_rtl:alu_src2(I), Map1),
  RtlAluOp = hipe_rtl:alu_op(I),
  S = false,
  I2 = mk_alu(S, Dst, Src1, RtlAluOp, Src2),
  {I2, Map2, Data}.

conv_shift(RtlShiftOp) ->
  case RtlShiftOp of
    'sll' -> 'lsl';
    'srl' -> 'lsr';
    'sra' -> 'asr'
  end.

conv_arith(RtlAluOp) -> % RtlAluOp \ RtlShiftOp -> ArmArithOp
  case RtlAluOp of
    'add' -> 'add';
    'sub' -> 'sub';
    'mul' -> 'mul';
    'or'  -> 'orr';
    'and' -> 'and';
    'xor' -> 'eor'
  end.

commute_arithop(ArithOp) ->
  case ArithOp of
    'sub' -> 'rsb';
    _ -> ArithOp
  end.

conv_cmpop('add') -> 'cmn';
conv_cmpop('sub') -> 'cmp';
conv_cmpop('and') -> 'tst';
conv_cmpop('xor') -> 'teq';
conv_cmpop(_) -> none.

cmpop_commutes('cmp') -> false;
cmpop_commutes('cmn') -> true;
cmpop_commutes('tst') -> true;
cmpop_commutes('teq') -> true.

mk_alu(S, Dst, Src1, RtlAluOp, Src2) ->
  case hipe_rtl:is_shift_op(RtlAluOp) of
    true ->
      mk_shift(S, Dst, Src1, conv_shift(RtlAluOp), Src2);
    false ->
      mk_arith(S, Dst, Src1, conv_arith(RtlAluOp), Src2)
  end.

mk_shift(S, Dst, Src1, ShiftOp, Src2) ->
  case hipe_arm:is_temp(Src1) of
    true ->
      case hipe_arm:is_temp(Src2) of
	true ->
	  mk_shift_rr(S, Dst, Src1, ShiftOp, Src2);
	_ ->
	  mk_shift_ri(S, Dst, Src1, ShiftOp, Src2)
      end;
    _ ->
      case hipe_arm:is_temp(Src2) of
	true ->
	  mk_shift_ir(S, Dst, Src1, ShiftOp, Src2);
	_ ->
	  mk_shift_ii(S, Dst, Src1, ShiftOp, Src2)
      end
  end.

mk_shift_ii(S, Dst, Src1, ShiftOp, Src2) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src1,
	mk_shift_ri(S, Dst, Tmp, ShiftOp, Src2)).

mk_shift_ir(S, Dst, Src1, ShiftOp, Src2) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src1,
	mk_shift_rr(S, Dst, Tmp, ShiftOp, Src2)).

mk_shift_ri(S, Dst, Src1, ShiftOp, 0)
  when ShiftOp =:= lsl; ShiftOp =:= lsr; ShiftOp =:= asr ->
  [hipe_arm:mk_move(S, Dst, Src1)];
mk_shift_ri(S, Dst, Src1, ShiftOp, Src2)
  when is_integer(Src2), Src2 > 0, Src2 < 32 ->
  Am1 = {Src1,ShiftOp,Src2},
  [hipe_arm:mk_move(S, Dst, Am1)].

mk_shift_rr(S, Dst, Src1, ShiftOp, Src2) ->
  Am1 = {Src1,ShiftOp,Src2},
  [hipe_arm:mk_move(S, Dst, Am1)].

mk_arith(S, Dst, Src1, ArithOp, Src2) ->
  case hipe_arm:is_temp(Src1) of
    true ->
      case hipe_arm:is_temp(Src2) of
	true ->
	  mk_arith_rr(S, Dst, Src1, ArithOp, Src2);
	_ ->
	  mk_arith_ri(S, Dst, Src1, ArithOp, Src2)
      end;
    _ ->
      case hipe_arm:is_temp(Src2) of
	true ->
	  mk_arith_ir(S, Dst, Src1, ArithOp, Src2);
	_ ->
	  mk_arith_ii(S, Dst, Src1, ArithOp, Src2)
      end
  end.

mk_arith_ii(S, Dst, Src1, ArithOp, Src2) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src1,
	mk_arith_ri(S, Dst, Tmp, ArithOp, Src2)).

mk_arith_ir(S, Dst, Src1, ArithOp, Src2) ->
  mk_arith_ri(S, Dst, Src2, commute_arithop(ArithOp), Src1).

mk_arith_ri(S, Dst, Src1, ArithOp, Src2) ->
  case ArithOp of
    'mul' -> % mul/smull only take reg/reg operands
      Tmp = new_untagged_temp(),
      mk_li(Tmp, Src2,
	    mk_arith_rr(S, Dst, Src1, ArithOp, Tmp));
    _ -> % add/sub/orr/and/eor have reg/am1 operands
      {FixAm1,NewArithOp,Am1} = fix_aluop_imm(ArithOp, Src2),
      FixAm1 ++ [hipe_arm:mk_alu(NewArithOp, S, Dst, Src1, Am1)]
  end.

mk_arith_rr(S, Dst, Src1, ArithOp, Src2) ->
  case {ArithOp,S} of
    {'mul',true} ->
      %% To check for overflow in 32x32->32 multiplication:
      %% smull Dst,TmpHi,Src1,Src2
      %% mov TmpSign,Dst,ASR #31
      %% cmp TmpSign,TmpHi
      %% [bne OverflowLabel]
      TmpHi = new_untagged_temp(),
      TmpSign = new_untagged_temp(),
      [hipe_arm:mk_smull(Dst, TmpHi, Src1, Src2),
       hipe_arm:mk_move(TmpSign, {Dst,'asr',31}),
       hipe_arm:mk_cmp('cmp', TmpSign, TmpHi)];
    _ ->
      [hipe_arm:mk_alu(ArithOp, S, Dst, Src1, Src2)]
  end.

fix_aluop_imm(AluOp, Imm) -> % {FixAm1,NewAluOp,Am1}
  case hipe_arm:try_aluop_imm(AluOp, Imm) of
    {NewAluOp,Am1} -> {[], NewAluOp, Am1};
    [] ->
      Tmp = new_untagged_temp(),
      {mk_li(Tmp, Imm), AluOp, Tmp}
  end.

conv_alub(I, Map, Data) ->
  %% dst = src1 aluop src2; if COND goto label
  {Src1, Map0} = conv_src(hipe_rtl:alub_src1(I), Map),
  {Src2, Map1} = conv_src(hipe_rtl:alub_src2(I), Map0),
  RtlAluOp = hipe_rtl:alub_op(I),
  RtlCond = hipe_rtl:alub_cond(I),
  HasDst = hipe_rtl:alub_has_dst(I),
  CmpOp = conv_cmpop(RtlAluOp),
  Cond0 = conv_alub_cond(RtlAluOp, RtlCond),
  case (not HasDst) andalso CmpOp =/= none of
    true ->
      I1 = mk_branch(Src1, CmpOp, Src2, Cond0,
		     hipe_rtl:alub_true_label(I),
		     hipe_rtl:alub_false_label(I),
		     hipe_rtl:alub_pred(I)),
      {I1, Map1, Data};
    false ->
      {Dst, Map2} =
	case HasDst of
	  false -> {new_untagged_temp(), Map1};
	  true -> conv_dst(hipe_rtl:alub_dst(I), Map1)
	end,
      Cond =
	case {RtlAluOp,Cond0} of
	  {'mul','vs'} -> 'ne';	% overflow becomes not-equal
	  {'mul','vc'} -> 'eq';	% no-overflow becomes equal
	  {'mul',_} -> exit({?MODULE,I});
	  {_,_} -> Cond0
	end,
      I2 = mk_pseudo_bc(
	     Cond,
	     hipe_rtl:alub_true_label(I),
	     hipe_rtl:alub_false_label(I),
	     hipe_rtl:alub_pred(I)),
      S = true,
      I1 = mk_alu(S, Dst, Src1, RtlAluOp, Src2),
      {I1 ++ I2, Map2, Data}
  end.

mk_branch(Src1, CmpOp, Src2, Cond, TrueLab, FalseLab, Pred) ->
  case hipe_arm:is_temp(Src1) of
    true ->
      case hipe_arm:is_temp(Src2) of
	true ->
	  mk_branch_rr(Src1, CmpOp, Src2, Cond, TrueLab, FalseLab, Pred);
	_ ->
	  mk_branch_ri(Src1, CmpOp, Src2, Cond, TrueLab, FalseLab, Pred)
      end;
    _ ->
      case hipe_arm:is_temp(Src2) of
	true ->
	  NewCond =
	    case cmpop_commutes(CmpOp) of
	      true -> Cond;
	      false ->  commute_cond(Cond)
	    end,
	  mk_branch_ri(Src2, CmpOp, Src1, NewCond, TrueLab, FalseLab, Pred);
	_ ->
	  mk_branch_ii(Src1, CmpOp, Src2, Cond, TrueLab, FalseLab, Pred)
      end
  end.

mk_branch_ii(Imm1, CmpOp, Imm2, Cond, TrueLab, FalseLab, Pred) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Imm1,
	mk_branch_ri(Tmp, CmpOp, Imm2, Cond,
		     TrueLab, FalseLab, Pred)).

mk_branch_ri(Src, CmpOp, Imm, Cond, TrueLab, FalseLab, Pred) ->
  {FixAm1,NewCmpOp,Am1} = fix_aluop_imm(CmpOp, Imm),
  FixAm1 ++ mk_branch_rr(Src, NewCmpOp, Am1, Cond, TrueLab, FalseLab, Pred).

mk_branch_rr(Src, CmpOp, Am1, Cond, TrueLab, FalseLab, Pred) ->
  [hipe_arm:mk_cmp(CmpOp, Src, Am1) |
   mk_pseudo_bc(Cond, TrueLab, FalseLab, Pred)].

conv_call(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:call_arglist(I), Map),
  {Dsts, Map1} = conv_dst_list(hipe_rtl:call_dstlist(I), Map0),
  {Fun, Map2} = conv_fun(hipe_rtl:call_fun(I), Map1),
  ContLab = hipe_rtl:call_continuation(I),
  ExnLab = hipe_rtl:call_fail(I),
  Linkage = hipe_rtl:call_type(I),
  I2 = mk_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage),
  {I2, Map2, Data}.

mk_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  case hipe_arm:is_prim(Fun) of
    true ->
      mk_primop_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage);
    false ->
      mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage)
  end.

mk_primop_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage) ->
  case hipe_arm:prim_prim(Prim) of
    %% no ARM-specific primops defined yet
    _ ->
      mk_general_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage)
  end.

mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  %% The backend does not support pseudo_calls without a
  %% continuation label, so we make sure each call has one.
  {RealContLab, Tail} =
    case mk_call_results(Dsts) of
      [] ->
	%% Avoid consing up a dummy basic block if the moves list
	%% is empty, as is typical for calls to suspend/0.
	%% This should be subsumed by a general "optimise the CFG"
	%% module, and could probably be removed.
	case ContLab of
	  [] ->
	    NewContLab = hipe_gensym:get_next_label(arm),
	    {NewContLab, [hipe_arm:mk_label(NewContLab)]};
	  _ ->
	    {ContLab, []}
	end;
      Moves ->
	%% Change the call to continue at a new basic block.
	%% In this block move the result registers to the Dsts,
	%% then continue at the call's original continuation.
	NewContLab = hipe_gensym:get_next_label(arm),
	case ContLab of
	  [] ->
	    %% This is just a fallthrough
	    %% No jump back after the moves.
	    {NewContLab,
	     [hipe_arm:mk_label(NewContLab) |
	      Moves]};
	  _ ->
	    %% The call has a continuation. Jump to it.
	    {NewContLab,
	     [hipe_arm:mk_label(NewContLab) |
	      Moves ++
	      [hipe_arm:mk_b_label(ContLab)]]}
	end
    end,
  SDesc = hipe_arm:mk_sdesc(ExnLab, 0, length(Args), {}),
  CallInsn = hipe_arm:mk_pseudo_call(Fun, SDesc, RealContLab, Linkage),
  {RegArgs,StkArgs} = split_args(Args),
  mk_push_args(StkArgs, move_actuals(RegArgs, [CallInsn | Tail])).

mk_call_results([]) ->
  [];
mk_call_results([Dst]) ->
  RV = hipe_arm:mk_temp(hipe_arm_registers:return_value(), 'tagged'),
  [hipe_arm:mk_pseudo_move(Dst, RV)];
mk_call_results(Dsts) ->
  exit({?MODULE,mk_call_results,Dsts}).

mk_push_args(StkArgs, Tail) ->
  case length(StkArgs) of
    0 ->
      Tail;
    NrStkArgs ->
      [hipe_arm:mk_pseudo_call_prepare(NrStkArgs) |
       mk_store_args(StkArgs, NrStkArgs * word_size(), Tail)]
  end.
  
mk_store_args([Arg|Args], PrevOffset, Tail) ->
  Offset = PrevOffset - word_size(),
  {Src,FixSrc} =
    case hipe_arm:is_temp(Arg) of
      true ->
	{Arg, []};
      _ ->
	Tmp = new_tagged_temp(),
	{Tmp, mk_li(Tmp, Arg)}
    end,
  NewTail = hipe_arm:mk_store('str', Src, mk_sp(), Offset, 'new', Tail),
  mk_store_args(Args, Offset, FixSrc ++ NewTail);
mk_store_args([], _, Tail) ->
  Tail.

conv_comment(I, Map, Data) ->
  I2 = [hipe_arm:mk_comment(hipe_rtl:comment_text(I))],
  {I2, Map, Data}.

conv_enter(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:enter_arglist(I), Map),
  {Fun, Map1} = conv_fun(hipe_rtl:enter_fun(I), Map0),
  I2 = mk_enter(Fun, Args, hipe_rtl:enter_type(I)),
  {I2, Map1, Data}.

mk_enter(Fun, Args, Linkage) ->
  Arity = length(Args),
  {RegArgs,StkArgs} = split_args(Args),
  move_actuals(RegArgs,
	       [hipe_arm:mk_pseudo_tailcall_prepare(),
		hipe_arm:mk_pseudo_tailcall(Fun, Arity, StkArgs, Linkage)]).

conv_goto(I, Map, Data) ->
  I2 = [hipe_arm:mk_b_label(hipe_rtl:goto_label(I))],
  {I2, Map, Data}.

conv_label(I, Map, Data) ->
  I2 = [hipe_arm:mk_label(hipe_rtl:label_name(I))],
  {I2, Map, Data}.

conv_load(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_dst(I), Map),
  {Base1, Map1} = conv_src(hipe_rtl:load_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:load_offset(I), Map1),
  LoadSize = hipe_rtl:load_size(I),
  LoadSign = hipe_rtl:load_sign(I),
  I2 = mk_load(Dst, Base1, Base2, LoadSize, LoadSign),
  {I2, Map2, Data}.

mk_load(Dst, Base1, Base2, LoadSize, LoadSign) ->
  case {LoadSize,LoadSign} of
    {byte,signed} ->
      case hipe_arm:is_temp(Base1) of
	true ->
	  case hipe_arm:is_temp(Base2) of
	    true ->
	      mk_ldrsb_rr(Dst, Base1, Base2);
	    _ ->
	      mk_ldrsb_ri(Dst, Base1, Base2)
	  end;
	_ ->
	  case hipe_arm:is_temp(Base2) of
	    true ->
	      mk_ldrsb_ri(Dst, Base2, Base1);
	    _ ->
	      mk_ldrsb_ii(Dst, Base1, Base2)
	  end
      end;
    _ ->
      LdOp =
	case LoadSize of
	  byte -> 'ldrb';
	  int32 -> 'ldr';
	  word -> 'ldr'
	end,
      case hipe_arm:is_temp(Base1) of
	true ->
	  case hipe_arm:is_temp(Base2) of
	    true ->
	      mk_load_rr(Dst, Base1, Base2, LdOp);
	    _ ->
	      mk_load_ri(Dst, Base1, Base2, LdOp)
	  end;
	_ ->
	  case hipe_arm:is_temp(Base2) of
	    true ->
	      mk_load_ri(Dst, Base2, Base1, LdOp);
	    _ ->
	      mk_load_ii(Dst, Base1, Base2, LdOp)
	  end
      end
  end.

mk_load_ii(Dst, Base1, Base2, LdOp) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Base1,
	mk_load_ri(Dst, Tmp, Base2, LdOp)).
   
mk_load_ri(Dst, Base, Offset, LdOp) ->
  hipe_arm:mk_load(LdOp, Dst, Base, Offset, 'new', []).

mk_load_rr(Dst, Base1, Base2, LdOp) ->
  Am2 = hipe_arm:mk_am2(Base1, '+', Base2),
  [hipe_arm:mk_load(LdOp, Dst, Am2)].

mk_ldrsb_ii(Dst, Base1, Base2) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Base1,
	mk_ldrsb_ri(Dst, Tmp, Base2)).
   
mk_ldrsb_ri(Dst, Base, Offset) when is_integer(Offset) ->
  {Sign,AbsOffset} =
    if Offset < 0 -> {'-', -Offset};
       true -> {'+', Offset}
    end,
  if AbsOffset =< 255 ->
      Am3 = hipe_arm:mk_am3(Base, Sign, AbsOffset),
      [hipe_arm:mk_ldrsb(Dst, Am3)];
     true ->
      Index = new_untagged_temp(),
      Am3 = hipe_arm:mk_am3(Base, Sign, Index),
      mk_li(Index, AbsOffset,
	    [hipe_arm:mk_ldrsb(Dst, Am3)])
  end.

mk_ldrsb_rr(Dst, Base1, Base2) ->
  Am3 = hipe_arm:mk_am3(Base1, '+', Base2),
  [hipe_arm:mk_ldrsb(Dst, Am3)].

conv_load_address(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_address_dst(I), Map),
  Addr = hipe_rtl:load_address_addr(I),
  Type = hipe_rtl:load_address_type(I),
  Src = {Addr,Type},
  I2 = [hipe_arm:mk_pseudo_li(Dst, Src)],
  {I2, Map0, Data}.

conv_load_atom(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_atom_dst(I), Map),
  Src = hipe_rtl:load_atom_atom(I),
  I2 = [hipe_arm:mk_pseudo_li(Dst, Src)],
  {I2, Map0, Data}.

conv_move(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:move_dst(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:move_src(I), Map0),
  I2 = mk_move(Dst, Src, []),
  {I2, Map1, Data}.

mk_move(Dst, Src, Tail) ->
  case hipe_arm:is_temp(Src) of
    true -> [hipe_arm:mk_pseudo_move(Dst, Src) | Tail];
    _ -> mk_li(Dst, Src, Tail)
  end.

conv_return(I, Map, Data) ->
  %% TODO: multiple-value returns
  {[Arg], Map0} = conv_src_list(hipe_rtl:return_varlist(I), Map),
  I2 = mk_move(mk_rv(), Arg,
	       [hipe_arm:mk_pseudo_blr()]),
  {I2, Map0, Data}.

conv_store(I, Map, Data) ->
  {Base, Map0} = conv_src(hipe_rtl:store_base(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:store_src(I), Map0),
  {Offset, Map2} = conv_src(hipe_rtl:store_offset(I), Map1),
  StoreSize = hipe_rtl:store_size(I),
  I2 = mk_store(Src, Base, Offset, StoreSize),
  {I2, Map2, Data}.

mk_store(Src, Base, Offset, StoreSize) ->
  StOp =
    case StoreSize of
      byte -> 'strb';
      int32 -> 'str';
      word -> 'str'
    end,
  case hipe_arm:is_temp(Src) of
    true ->
      mk_store2(Src, Base, Offset, StOp);
    _ ->
      Tmp = new_untagged_temp(),
      mk_li(Tmp, Src,
	    mk_store2(Tmp, Base, Offset, StOp))
  end.

mk_store2(Src, Base, Offset, StOp) ->
  case hipe_arm:is_temp(Base) of
    true ->
      case hipe_arm:is_temp(Offset) of
	true ->
	  mk_store_rr(Src, Base, Offset, StOp);
	_ ->
	  mk_store_ri(Src, Base, Offset, StOp)
      end;
    false ->
      case hipe_arm:is_temp(Offset) of
	true ->
	  mk_store_ri(Src, Offset, Base, StOp);
	_ ->
	  mk_store_ii(Src, Base, Offset, StOp)
      end
  end.

mk_store_ii(Src, Base, Offset, StOp) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Base,
	mk_store_ri(Src, Tmp, Offset, StOp)).

mk_store_ri(Src, Base, Offset, StOp) ->
  hipe_arm:mk_store(StOp, Src, Base, Offset, 'new', []).
   
mk_store_rr(Src, Base, Index, StOp) ->
  Am2 = hipe_arm:mk_am2(Base, '+', Index),
  [hipe_arm:mk_store(StOp, Src, Am2)].

conv_switch(I, Map, Data) ->
  Labels = hipe_rtl:switch_labels(I),
  LMap = [{label,L} || L <- Labels],
  {NewData, JTabLab} =
    case hipe_rtl:switch_sort_order(I) of
      [] ->
	hipe_consttab:insert_block(Data, word, LMap);
      SortOrder ->
	hipe_consttab:insert_sorted_block(
	  Data, word, LMap, SortOrder)
    end,
  %% no immediates allowed here
  {IndexR, Map1} = conv_dst(hipe_rtl:switch_src(I), Map),
  JTabR = new_untagged_temp(),
  I2 =
    [hipe_arm:mk_pseudo_li(JTabR, {JTabLab,constant}),
     hipe_arm:mk_pseudo_switch(JTabR, IndexR, Labels)],
  {I2, Map1, NewData}.

%%% Create a conditional branch.

mk_pseudo_bc(Cond, TrueLabel, FalseLabel, Pred) ->
  [hipe_arm:mk_pseudo_bc(Cond, TrueLabel, FalseLabel, Pred)].

%%% Load an integer constant into a register.

mk_li(Dst, Value) -> mk_li(Dst, Value, []).

mk_li(Dst, Value, Tail) ->
  hipe_arm:mk_li(Dst, Value, Tail).

%%% Convert an RTL condition code.

conv_alub_cond(RtlAluOp, Cond) ->	% may be unsigned, depends on aluop
  %% Note: ARM has a non-standard definition of the Carry flag:
  %% 'cmp', 'sub', and 'rsb' define Carry as the NEGATION of Borrow.
  %% This means that the mapping between C/Z combinations and
  %% conditions like "lower" and "higher" becomes non-standard.
  %% (See conv_branch_cond/1 which maps ltu to lo/carry-clear,
  %% while x86 maps ltu to b/carry-set.)
  %% Here in conv_alub_cond/2 it means that the mapping of unsigned
  %% conditions also has to consider the alu operator, since e.g.
  %% 'add' and 'sub' behave differently with regard to Carry.
  case {RtlAluOp, Cond} of	% handle allowed alub unsigned conditions
    {'add', 'ltu'} -> 'hs';	% add+ltu == unsigned overflow == carry set == hs
    %% add more cases when needed
    {'sub', _} -> conv_branch_cond(Cond);
    _ -> conv_cond(Cond)
  end.

conv_cond(Cond) ->	% only signed
  case Cond of
    eq	-> 'eq';
    ne	-> 'ne';
    gt	-> 'gt';
    ge	-> 'ge';
    lt	-> 'lt';
    le	-> 'le';
    overflow -> 'vs';
    not_overflow -> 'vc'
  end.

conv_branch_cond(Cond) -> % may be unsigned
  case Cond of
    gtu -> 'hi';
    geu -> 'hs';
    ltu -> 'lo';
    leu -> 'ls';
    _   -> conv_cond(Cond)
  end.    

%%% Commute an ARM condition code.

commute_cond(Cond) ->	% if x Cond y, then y commute_cond(Cond) x
  case Cond of
    'eq' -> 'eq';	% ==, ==
    'ne' -> 'ne';	% !=, !=
    'gt' -> 'lt';	% >, <
    'ge' -> 'le';	% >=, <=
    'lt' -> 'gt';	% <, >
    'le' -> 'ge';	% <=, >=
    'hi' -> 'lo';	% >u, <u
    'hs' -> 'ls';	% >=u, <=u
    'lo' -> 'hi';	% <u, >u
    'ls' -> 'hs';	% <=u, >=u
    %% vs/vc: n/a
    _ -> exit({?MODULE,commute_cond,Cond})
  end.

%%% Split a list of formal or actual parameters into the
%%% part passed in registers and the part passed on the stack.
%%% The parameters passed in registers are also tagged with
%%% the corresponding registers.

split_args(Args) ->
  split_args(0, hipe_arm_registers:nr_args(), Args, []).

split_args(I, N, [Arg|Args], RegArgs) when I < N ->
  Reg = hipe_arm_registers:arg(I),
  Temp = hipe_arm:mk_temp(Reg, 'tagged'),
  split_args(I+1, N, Args, [{Arg,Temp}|RegArgs]);
split_args(_, _, StkArgs, RegArgs) ->
  {RegArgs, StkArgs}.

%%% Convert a list of actual parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_actuals([{Src,Dst}|Actuals], Rest) ->
  move_actuals(Actuals, mk_move(Dst, Src, Rest));
move_actuals([], Rest) ->
  Rest.

%%% Convert a list of formal parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_formals([{Dst,Src}|Formals], Rest) ->
  move_formals(Formals, [hipe_arm:mk_pseudo_move(Dst, Src) | Rest]);
move_formals([], Rest) ->
  Rest.

%%% Convert a 'fun' operand (MFA, prim, or temp)

conv_fun(Fun, Map) ->
  case hipe_rtl:is_var(Fun) of
    true ->
      conv_dst(Fun, Map);
    false ->
      case hipe_rtl:is_reg(Fun) of
	true ->
	  conv_dst(Fun, Map);
	false ->
	  if is_atom(Fun) ->
	      {hipe_arm:mk_prim(Fun), Map};
	     true ->
	      {conv_mfa(Fun), Map}
	  end
      end
  end.

%%% Convert an MFA operand.

conv_mfa({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
  hipe_arm:mk_mfa(M, F, A).

%%% Convert an RTL source operand (imm/var/reg).
%%% Returns a temp or a naked integer.

conv_src(Opnd, Map) ->
  case hipe_rtl:is_imm(Opnd) of
    true ->
      Value = hipe_rtl:imm_value(Opnd),
      if is_integer(Value) ->
	  {Value, Map}
      end;
    false ->
      conv_dst(Opnd, Map)
  end.

conv_src_list([O|Os], Map) ->
  {V, Map1} = conv_src(O, Map),
  {Vs, Map2} = conv_src_list(Os, Map1),
  {[V|Vs], Map2};
conv_src_list([], Map) ->
  {[], Map}.

%%% Convert an RTL destination operand (var/reg).

conv_dst(Opnd, Map) ->
  {Name, Type} =
    case hipe_rtl:is_var(Opnd) of
      true ->
	{hipe_rtl:var_index(Opnd), 'tagged'};
      false ->
	case hipe_rtl:is_fpreg(Opnd) of
	  true ->
	    {hipe_rtl:fpreg_index(Opnd), 'double'};
	  false ->
	    {hipe_rtl:reg_index(Opnd), 'untagged'}
	end
    end,
  IsPrecoloured =
    case Type of
      'double' -> false; %hipe_arm_registers:is_precoloured_fpr(Name);
      _ -> hipe_arm_registers:is_precoloured_gpr(Name)
    end,
  case IsPrecoloured of
    true ->
      {hipe_arm:mk_temp(Name, Type), Map};
    false ->
      case vmap_lookup(Map, Opnd) of
	{value, NewTemp} ->
	  {NewTemp, Map};
	_ ->
	  NewTemp = hipe_arm:mk_new_temp(Type),
	  {NewTemp, vmap_bind(Map, Opnd, NewTemp)}
      end
  end.

conv_dst_list([O|Os], Map) ->
  {Dst, Map1} = conv_dst(O, Map),
  {Dsts, Map2} = conv_dst_list(Os, Map1),
  {[Dst|Dsts], Map2};
conv_dst_list([], Map) ->
  {[], Map}.

conv_formals(Os, Map) ->
  conv_formals(hipe_arm_registers:nr_args(), Os, Map, []).

conv_formals(N, [O|Os], Map, Res) ->
  Type =
    case hipe_rtl:is_var(O) of
      true -> 'tagged';
      _ -> 'untagged'
    end,
  Dst =
    if N > 0 -> hipe_arm:mk_new_temp(Type);	% allocatable
       true -> hipe_arm:mk_new_nonallocatable_temp(Type)
    end,
  Map1 = vmap_bind(Map, O, Dst),
  conv_formals(N-1, Os, Map1, [Dst|Res]);
conv_formals(_, [], Map, Res) ->
  {lists:reverse(Res), Map}.

%%% Create a temp representing the stack pointer register.

mk_sp() ->
  hipe_arm:mk_temp(hipe_arm_registers:stack_pointer(), 'untagged').

%%% Create a temp representing the return value register.

mk_rv() ->
  hipe_arm:mk_temp(hipe_arm_registers:return_value(), 'tagged').

%%% new_untagged_temp -- conjure up an untagged scratch reg

new_untagged_temp() ->
  hipe_arm:mk_new_temp('untagged').

%%% new_tagged_temp -- conjure up a tagged scratch reg

new_tagged_temp() ->
  hipe_arm:mk_new_temp('tagged').

%%% Map from RTL var/reg operands to temps.

vmap_empty() ->
  gb_trees:empty().

vmap_lookup(Map, Key) ->
  gb_trees:lookup(Key, Map).

vmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).

word_size() ->
  4.
