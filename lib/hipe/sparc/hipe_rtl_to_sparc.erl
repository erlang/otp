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

-module(hipe_rtl_to_sparc).

-export([translate/1]).

-include("../rtl/hipe_rtl.hrl").

translate(RTL) ->
  hipe_gensym:init(sparc),
  hipe_gensym:set_var(sparc, hipe_sparc_registers:first_virtual()),
  hipe_gensym:set_label(sparc, hipe_gensym:get_label(rtl)),
  Map0 = vmap_empty(),
  {Formals, Map1} = conv_formals(hipe_rtl:rtl_params(RTL), Map0),
  OldData = hipe_rtl:rtl_data(RTL),
  {Code0, NewData} = conv_insn_list(hipe_rtl:rtl_code(RTL), Map1, OldData),
  {RegFormals, _} = split_args(Formals),
  Code =
    case RegFormals of
      [] -> Code0;
      _ -> [hipe_sparc:mk_label(hipe_gensym:get_next_label(sparc)) |
	    move_formals(RegFormals, Code0)]
    end,
  IsClosure = hipe_rtl:rtl_is_closure(RTL),
  IsLeaf = hipe_rtl:rtl_is_leaf(RTL),
  hipe_sparc:mk_defun(hipe_rtl:rtl_fun(RTL),
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
    #switch{} -> conv_switch(I, Map, Data); % XXX: only switch uses/updates Data
    #fconv{} -> conv_fconv(I, Map, Data);
    #fmove{} -> conv_fmove(I, Map, Data);
    #fload{} -> conv_fload(I, Map, Data);
    #fstore{} -> conv_fstore(I, Map, Data);
    #fp{} -> conv_fp_binary(I, Map, Data);
    #fp_unop{} -> conv_fp_unary(I, Map, Data);
    _ -> exit({?MODULE,conv_insn,I})
  end.

conv_fconv(I, Map, Data) ->
  %% Dst := (double)Src, where Dst is FP reg and Src is GP reg or imm
  {Src, Map1} = conv_src(hipe_rtl:fconv_src(I), Map),
  {Dst, Map2} = conv_fpreg(hipe_rtl:fconv_dst(I), Map1),
  I2 = mk_fconv(Src, Dst),
  {I2, Map2, Data}.

mk_fconv(Src, Dst) ->
  CSP = hipe_sparc:mk_temp(14, 'untagged'), % o6
  Offset = 100,
  mk_store('stw', Src, CSP, Offset) ++
  [hipe_sparc:mk_pseudo_fload(CSP, hipe_sparc:mk_simm13(Offset), Dst, true),
   hipe_sparc:mk_fp_unary('fitod', Dst, Dst)].

conv_fmove(I, Map, Data) ->
  %% Dst := Src, where both Dst and Src are FP regs
  {Src, Map1} = conv_fpreg(hipe_rtl:fmove_src(I), Map),
  {Dst, Map2} = conv_fpreg(hipe_rtl:fmove_dst(I), Map1),
  I2 = mk_fmove(Src, Dst),
  {I2, Map2, Data}.

mk_fmove(Src, Dst) ->
  [hipe_sparc:mk_pseudo_fmove(Src, Dst)].

conv_fload(I, Map, Data) ->
  %% Dst := MEM[Base+Off], where Dst is FP reg
  {Base1, Map1} = conv_src(hipe_rtl:fload_src(I), Map),
  {Base2, Map2} = conv_src(hipe_rtl:fload_offset(I), Map1),
  {Dst, Map3} = conv_fpreg(hipe_rtl:fload_dst(I), Map2),
  I2 = mk_fload(Base1, Base2, Dst),
  {I2, Map3, Data}.

mk_fload(Base1, Base2, Dst) ->
  case hipe_sparc:is_temp(Base1) of
    true ->
      case hipe_sparc:is_temp(Base2) of
	true ->
	  mk_fload_rr(Base1, Base2, Dst);
	_ ->
	  mk_fload_ri(Base1, Base2, Dst)
      end;
    _ ->
      case hipe_sparc:is_temp(Base2) of
	true ->
	  mk_fload_ri(Base2, Base1, Dst);
	_ ->
	  mk_fload_ii(Base1, Base2, Dst)
      end
  end.

mk_fload_rr(Base1, Base2, Dst) ->
  Tmp = new_untagged_temp(),
  Disp = hipe_sparc:mk_simm13(0),
  [hipe_sparc:mk_alu('add', Base1, Base2, Tmp),
   hipe_sparc:mk_pseudo_fload(Tmp, Disp, Dst, false)].

mk_fload_ii(Base1, Base2, Dst) ->
  io:format("~w: RTL fload with two immediates\n", [?MODULE]),
  Tmp = new_untagged_temp(),
  mk_set(Base1, Tmp, mk_fload_ri(Tmp, Base2, Dst)).

mk_fload_ri(Base, Disp, Dst) ->
  hipe_sparc:mk_fload(Base, Disp, Dst, 'new').

conv_fstore(I, Map, Data) ->
  %% MEM[Base+Off] := Src, where Src is FP reg
  {Base1, Map1} = conv_dst(hipe_rtl:fstore_base(I), Map),
  {Base2, Map2} = conv_src(hipe_rtl:fstore_offset(I), Map1),
  {Src, Map3} = conv_fpreg(hipe_rtl:fstore_src(I), Map2),
  I2 = mk_fstore(Src, Base1, Base2),
  {I2, Map3, Data}.

mk_fstore(Src, Base1, Base2) ->
  case hipe_sparc:is_temp(Base2) of
    true ->
      mk_fstore_rr(Src, Base1, Base2);
    _ ->
      mk_fstore_ri(Src, Base1, Base2)
  end.

mk_fstore_rr(Src, Base1, Base2) ->
  Tmp = new_untagged_temp(),
  Disp = hipe_sparc:mk_simm13(0),
  [hipe_sparc:mk_alu('add', Base1, Base2, Tmp),
   hipe_sparc:mk_pseudo_fstore(Src, Tmp, Disp)].

mk_fstore_ri(Src, Base, Disp) ->
  hipe_sparc:mk_fstore(Src, Base, Disp, 'new').

conv_fp_binary(I, Map, Data) ->
  {Src1, Map1} = conv_fpreg(hipe_rtl:fp_src1(I), Map),
  {Src2, Map2} = conv_fpreg(hipe_rtl:fp_src2(I), Map1),
  {Dst, Map3} = conv_fpreg(hipe_rtl:fp_dst(I), Map2),
  RtlFpOp = hipe_rtl:fp_op(I),
  I2 = mk_fp_binary(RtlFpOp, Src1, Src2, Dst),
  {I2, Map3, Data}.

mk_fp_binary(RtlFpOp, Src1, Src2, Dst) ->
  FpBinOp =
    case RtlFpOp of
      'fadd' -> 'faddd';
      'fdiv' -> 'fdivd';
      'fmul' -> 'fmuld';
      'fsub' -> 'fsubd'
    end,
  [hipe_sparc:mk_fp_binary(FpBinOp, Src1, Src2, Dst)].

conv_fp_unary(I, Map, Data) ->
  {Src, Map1} = conv_fpreg(hipe_rtl:fp_unop_src(I), Map),
  {Dst, Map2} = conv_fpreg(hipe_rtl:fp_unop_dst(I), Map1),
  RtlFpUnOp = hipe_rtl:fp_unop_op(I),
  I2 = mk_fp_unary(RtlFpUnOp, Src, Dst),
  {I2, Map2, Data}.

mk_fp_unary(RtlFpUnOp, Src, Dst) ->
  FpUnOp =
    case RtlFpUnOp of
      'fchs' -> 'fnegd'
    end,
  [hipe_sparc:mk_fp_unary(FpUnOp, Src, Dst)].

conv_alu(I, Map, Data) ->
  %% dst = src1 aluop src2
  {Dst, Map0} = conv_dst(hipe_rtl:alu_dst(I), Map),
  {Src1, Map1} = conv_src(hipe_rtl:alu_src1(I), Map0),
  {Src2, Map2} = conv_src(hipe_rtl:alu_src2(I), Map1),
  AluOp = conv_aluop(hipe_rtl:alu_op(I)),
  {I2, _DidCommute} = mk_alu(AluOp, Src1, Src2, Dst),
  {I2, Map2, Data}.

mk_alu(XAluOp, Src1, Src2, Dst) ->
  case hipe_sparc:is_temp(Src1) of
    true ->
      case hipe_sparc:is_temp(Src2) of
	true ->
	  {mk_alu_rs(XAluOp, Src1, Src2, Dst),
	   false};
	_ ->
	  {mk_alu_ri(XAluOp, Src1, Src2, Dst),
	   false}
      end;
    _ ->
      case hipe_sparc:is_temp(Src2) of
	true ->
	  mk_alu_ir(XAluOp, Src1, Src2, Dst);
	_ ->
	  {mk_alu_ii(XAluOp, Src1, Src2, Dst),
	   false}
      end
  end.

mk_alu_ii(XAluOp, Src1, Src2, Dst) ->
  io:format("~w: ALU with two immediates (~w ~w ~w ~w)\n",
	    [?MODULE, XAluOp, Src1, Src2, Dst]),
  Tmp = new_untagged_temp(),
  mk_set(Src1, Tmp, mk_alu_ri(XAluOp, Tmp, Src2, Dst)).

mk_alu_ir(XAluOp, Src1, Src2, Dst) ->
  case xaluop_commutes(XAluOp) of
    true ->
      {mk_alu_ri(XAluOp, Src2, Src1, Dst),
       true};
    _ ->
      Tmp = new_untagged_temp(),
      {mk_set(Src1, Tmp, mk_alu_rs(XAluOp, Tmp, Src2, Dst)),
       false}
  end.

mk_alu_ri(XAluOp, Src1, Src2, Dst) ->
  case xaluop_is_shift(XAluOp) of
    true ->
      mk_shift_ri(XAluOp, Src1, Src2, Dst);
    false ->
      mk_arith_ri(XAluOp, Src1, Src2, Dst)
  end.

mk_shift_ri(XShiftOp, Src1, Src2, Dst) when is_integer(Src2) ->
  if Src2 >= 0, Src2 < 32 ->	% XXX: sparc64: < 64
      mk_alu_rs(XShiftOp, Src1, hipe_sparc:mk_uimm5(Src2), Dst);
     true ->
      exit({?MODULE,mk_shift_ri,Src2})	% excessive shifts are errors
  end.

mk_arith_ri(XAluOp, Src1, Src2, Dst) when is_integer(Src2) ->
  if -4096 =< Src2, Src2 < 4096 ->
      mk_alu_rs(XAluOp, Src1, hipe_sparc:mk_simm13(Src2), Dst);
     true ->
      Tmp = new_untagged_temp(),
      mk_set(Src2, Tmp, mk_alu_rs(XAluOp, Src1, Tmp, Dst))
  end.

mk_alu_rs(XAluOp, Src1, Src2, Dst) ->
  [hipe_sparc:mk_alu(xaluop_normalise(XAluOp), Src1, Src2, Dst)].

conv_alub(I, Map, Data) ->
  %% dst = src1 aluop src2; if COND goto label
  HasDst = hipe_rtl:alub_has_dst(I),
  {Dst, Map0} =
    case HasDst of
      false -> {hipe_sparc:mk_g0(), Map};
      true -> conv_dst(hipe_rtl:alub_dst(I), Map)
    end,
  {Src1, Map1} = conv_src(hipe_rtl:alub_src1(I), Map0),
  {Src2, Map2} = conv_src(hipe_rtl:alub_src2(I), Map1),
  Cond = conv_cond(hipe_rtl:alub_cond(I)),
  RtlAlubOp = hipe_rtl:alub_op(I),
  I2 =
    case RtlAlubOp of
      'mul' ->
	%% To check for overflow in 32x32->32 multiplication:
	%% smul Src1,Src2,Dst	% Dst is lo32(Res), %y is %hi32(Res)
	%% rd %y,TmpHi
	%% sra Dst,31,TmpSign	% fill TmpSign with sign of Dst
	%% subcc TmpSign,TmpHi,%g0
	%% [bne OverflowLabel]
	NewCond =
	  case Cond of
	    vs -> ne;
	    vc -> eq
	  end,
	TmpHi = hipe_sparc:mk_new_temp('untagged'),
	TmpSign = hipe_sparc:mk_new_temp('untagged'),
	G0 = hipe_sparc:mk_g0(),
	{I1, _DidCommute} = mk_alu('smul', Src1, Src2, Dst),
	I1 ++
	[hipe_sparc:mk_rdy(TmpHi),
	 hipe_sparc:mk_alu('sra', Dst, hipe_sparc:mk_uimm5(31), TmpSign) |
	 conv_alub2(G0, TmpSign, 'cmpcc', NewCond, TmpHi, I)];
      _ ->
	XAluOp =
	  case (not HasDst) andalso RtlAlubOp =:= 'sub' of
	    true -> 'cmpcc'; % == a subcc that commutes
	    false -> conv_alubop_cc(RtlAlubOp)
	  end,
	conv_alub2(Dst, Src1, XAluOp, Cond, Src2, I)
    end,
  {I2, Map2, Data}.

conv_alub2(Dst, Src1, XAluOp, Cond, Src2, I) ->
  conv_alub_bp(Dst, Src1, XAluOp, Cond, Src2, I).

conv_alub_bp(Dst, Src1, XAluOp, Cond, Src2, I) ->
  TrueLab = hipe_rtl:alub_true_label(I),
  FalseLab = hipe_rtl:alub_false_label(I),
  Pred = hipe_rtl:alub_pred(I),
  %% "Dst = Src1 AluOp Src2; if COND" becomes
  %% "Dst = Src1 AluOpCC Src22; if-COND(CC)"
  {I2, DidCommute} = mk_alu(XAluOp, Src1, Src2, Dst),
  NewCond =
    case DidCommute andalso XAluOp =:= 'cmpcc' of
      true -> commute_cond(Cond); % subcc does not commute; its conditions do
      false -> Cond
    end,
  I2 ++ mk_pseudo_bp(NewCond, TrueLab, FalseLab, Pred).

conv_alubop_cc(RtlAlubOp) ->
  case RtlAlubOp of
    'add' -> 'addcc';
    'sub' -> 'subcc';
    %% mul: handled elsewhere
    'or' -> 'orcc';
    'and' -> 'andcc';
    'xor' -> 'xorcc'
    %% no shift ops
  end.

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
  case hipe_sparc:is_prim(Fun) of
    true ->
      mk_primop_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage);
    false ->
      mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage)
  end.

mk_primop_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage) ->
  case hipe_sparc:prim_prim(Prim) of
    %% no SPARC-specific primops defined yet
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
	    NewContLab = hipe_gensym:get_next_label(sparc),
	    {NewContLab, [hipe_sparc:mk_label(NewContLab)]};
	  _ ->
	    {ContLab, []}
	end;
      Moves ->
	%% Change the call to continue at a new basic block.
	%% In this block move the result registers to the Dsts,
	%% then continue at the call's original continuation.
	NewContLab = hipe_gensym:get_next_label(sparc),
	case ContLab of
	  [] ->
	    %% This is just a fallthrough
	    %% No jump back after the moves.
	    {NewContLab,
	     [hipe_sparc:mk_label(NewContLab) |
	      Moves]};
	  _ ->
	    %% The call has a continuation. Jump to it.
	    {NewContLab,
	     [hipe_sparc:mk_label(NewContLab) |
	      Moves ++
	      [hipe_sparc:mk_b_label(ContLab)]]}
	end
    end,
  SDesc = hipe_sparc:mk_sdesc(ExnLab, 0, length(Args), {}),
  CallInsn = hipe_sparc:mk_pseudo_call(Fun, SDesc, RealContLab, Linkage),
  {RegArgs,StkArgs} = split_args(Args),
  mk_push_args(StkArgs, move_actuals(RegArgs, [CallInsn | Tail])).

mk_call_results(Dsts) ->
  case Dsts of
    [] -> [];
    [Dst] ->
      RV = hipe_sparc:mk_rv(),
      [hipe_sparc:mk_pseudo_move(RV, Dst)]
  end.

mk_push_args(StkArgs, Tail) ->
  case length(StkArgs) of
    0 ->
      Tail;
    NrStkArgs ->
      [hipe_sparc:mk_pseudo_call_prepare(NrStkArgs) |
       mk_store_args(StkArgs, NrStkArgs * word_size(), Tail)]
  end.
  
mk_store_args([Arg|Args], PrevOffset, Tail) ->
  Offset = PrevOffset - word_size(),
  {Src,FixSrc} =
    case hipe_sparc:is_temp(Arg) of
      true ->
	{Arg, []};
      _ ->
	Tmp = new_tagged_temp(),
	{Tmp, mk_set(Arg, Tmp)}
    end,
  %% XXX: sparc64: stx
  Store = hipe_sparc:mk_store('stw', Src, hipe_sparc:mk_sp(), hipe_sparc:mk_simm13(Offset)),
  mk_store_args(Args, Offset, FixSrc ++ [Store | Tail]);
mk_store_args([], _, Tail) ->
  Tail.

conv_comment(I, Map, Data) ->
  I2 = [hipe_sparc:mk_comment(hipe_rtl:comment_text(I))],
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
	       [hipe_sparc:mk_pseudo_tailcall_prepare(),
		hipe_sparc:mk_pseudo_tailcall(Fun, Arity, StkArgs, Linkage)]).

conv_goto(I, Map, Data) ->
  I2 = [hipe_sparc:mk_b_label(hipe_rtl:goto_label(I))],
  {I2, Map, Data}.

conv_label(I, Map, Data) ->
  I2 = [hipe_sparc:mk_label(hipe_rtl:label_name(I))],
  {I2, Map, Data}.

conv_load(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_dst(I), Map),
  {Base1, Map1} = conv_src(hipe_rtl:load_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:load_offset(I), Map1),
  LdOp = conv_ldop(hipe_rtl:load_size(I), hipe_rtl:load_sign(I)),
  {I2, _DidCommute} = mk_alu(LdOp, Base1, Base2, Dst),
  {I2, Map2, Data}.

conv_ldop(LoadSize, LoadSign) ->
  case LoadSize of
    word -> 'lduw';	% XXX: sparc64: ldx
    int32 -> 'lduw';	% XXX: sparc64: lduw or ldsw
    int16 ->
      case LoadSign of
	signed -> 'ldsh';
	unsigned -> 'lduh'
      end;
    byte ->
      case LoadSign of
	signed -> 'ldsb';
	unsigned -> 'ldub'
      end
  end.

conv_load_address(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_address_dst(I), Map),
  Addr = hipe_rtl:load_address_addr(I),
  Type = hipe_rtl:load_address_type(I),
  Src = {Addr,Type},
  I2 = [hipe_sparc:mk_pseudo_set(Src, Dst)],
  {I2, Map0, Data}.

conv_load_atom(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_atom_dst(I), Map),
  Src = hipe_rtl:load_atom_atom(I),
  I2 = [hipe_sparc:mk_pseudo_set(Src, Dst)],
  {I2, Map0, Data}.

conv_move(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:move_dst(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:move_src(I), Map0),
  I2 = mk_move(Src, Dst, []),
  {I2, Map1, Data}.

mk_move(Src, Dst, Tail) ->
  case hipe_sparc:is_temp(Src) of
    true -> [hipe_sparc:mk_pseudo_move(Src, Dst) | Tail];
    _ -> mk_set(Src, Dst, Tail)
  end.

conv_return(I, Map, Data) ->
  %% TODO: multiple-value returns
  {[Arg], Map0} = conv_src_list(hipe_rtl:return_varlist(I), Map),
  I2 = mk_move(Arg, hipe_sparc:mk_rv(), [hipe_sparc:mk_pseudo_ret()]),
  {I2, Map0, Data}.

conv_store(I, Map, Data) ->
  {Base1, Map0} = conv_src(hipe_rtl:store_base(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:store_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:store_offset(I), Map1),
  StOp = conv_stop(hipe_rtl:store_size(I)),
  I2 = mk_store(StOp, Src, Base1, Base2),
  {I2, Map2, Data}.

conv_stop(StoreSize) ->
  case StoreSize of
    word -> 'stw';	% XXX: sparc64: stx
    int32 -> 'stw';
    byte -> 'stb'
  end.

mk_store(StOp, Src, Base1, Base2) ->
  case hipe_sparc:is_temp(Src) of
    true ->
      mk_store2(StOp, Src, Base1, Base2);
    _ ->
      Tmp = new_untagged_temp(),
      mk_set(Src, Tmp, mk_store2(StOp, Tmp, Base1, Base2))
  end.

mk_store2(StOp, Src, Base1, Base2) ->
  case hipe_sparc:is_temp(Base1) of
    true ->
      case hipe_sparc:is_temp(Base2) of
	true ->
	  mk_store_rr(StOp, Src, Base1, Base2);
	_ ->
	  mk_store_ri(StOp, Src, Base1, Base2)
      end;
    _ ->
      case hipe_sparc:is_temp(Base2) of
	true ->
	  mk_store_ri(StOp, Src, Base2, Base1);
	_ ->
	  mk_store_ii(StOp, Src, Base1, Base2)
      end
  end.
  
mk_store_ii(StOp, Src, Base, Disp) ->
  Tmp = new_untagged_temp(),
  mk_set(Base, Tmp, mk_store_ri(StOp, Src, Tmp, Disp)).

mk_store_ri(StOp, Src, Base, Disp) ->
  hipe_sparc:mk_store(StOp, Src, Base, Disp, 'new', []).

mk_store_rr(StOp, Src, Base1, Base2) ->
  [hipe_sparc:mk_store(StOp, Src, Base1, Base2)].

conv_switch(I, Map, Data) ->
  Labels = hipe_rtl:switch_labels(I),
  LMap = [{label,L} || L <- Labels],
  {NewData, JTabLab} =
    case hipe_rtl:switch_sort_order(I) of
      [] ->
	hipe_consttab:insert_block(Data, word, LMap);
      SortOrder ->
	hipe_consttab:insert_sorted_block(Data, word, LMap, SortOrder)
    end,
  %% no immediates allowed here
  {IndexR, Map1} = conv_dst(hipe_rtl:switch_src(I), Map),
  JTabR = new_untagged_temp(),
  OffsetR = new_untagged_temp(),
  DestR = new_untagged_temp(),
  I2 =
    [hipe_sparc:mk_pseudo_set({JTabLab,constant}, JTabR),
     %% XXX: sparc64: << 3
     hipe_sparc:mk_alu('sll', IndexR, hipe_sparc:mk_uimm5(2), OffsetR),
     %% XXX: sparc64: ldx
     hipe_sparc:mk_alu('lduw', JTabR, OffsetR, DestR),
     hipe_sparc:mk_jmp(DestR, hipe_sparc:mk_simm13(0), Labels)],
  {I2, Map1, NewData}.

%%% Create a conditional branch.

mk_pseudo_bp(Cond, TrueLabel, FalseLabel, Pred) ->
  [hipe_sparc:mk_pseudo_bp(Cond, TrueLabel, FalseLabel, Pred)].

%%% Load an integer constant into a register.

mk_set(Value, Dst) -> mk_set(Value, Dst, []).

mk_set(Value, Dst, Tail) ->
  hipe_sparc:mk_set(Value, Dst, Tail).

%%% Convert an RTL ALU op.

conv_aluop(RtlAluOp) ->
  case RtlAluOp of
    'add' -> 'add';
    'sub' -> 'sub';
    'mul' -> 'mulx';
    'or' -> 'or';
    'and' -> 'and';
    'xor' -> 'xor';
    'sll' -> 'sll';	% XXX: sparc64: sllx
    'srl' -> 'srl';	% XXX: sparc64: srlx
    'sra' -> 'sra'	% XXX: sparc64: srax
  end.

%%% Check if an extended SPARC AluOp commutes.

xaluop_commutes(XAluOp) ->
  case XAluOp of
    %% 'cmp' -> true;
    'cmpcc' -> true;
    'add' -> true;
    'addcc' -> true;
    'and' -> true;
    'andcc' -> true;
    'or' -> true;
    'orcc' -> true;
    'xor' -> true;
    'xorcc' -> true;
    'sub' -> false;
    'subcc' -> false;
    'mulx' -> true;
    'smul' -> true;
    'sll' -> false;
    'srl' -> false;
    'sra' -> false;
    %% 'sllx' -> false;
    %% 'srlx' -> false;
    %% 'srax' -> false;
    'ldsb' -> true;
    'ldsh' -> true;
    %% 'ldsw' -> true;
    'ldub' -> true;
    'lduh' -> true;
    'lduw' -> true
    %% 'ldx' -> true
  end.

%%% Check if an extended SPARC AluOp is a shift.

xaluop_is_shift(XAluOp) ->
  case XAluOp of
    'add' -> false;
    'addcc' -> false;
    'and' -> false;
    'andcc' -> false;
    'cmpcc' -> false;
    'ldsb' -> false;
    'ldub' -> false;
    'lduw' -> false;
    'or' -> false;
    'sll' -> true;
    %% 'sllx' -> true;
    'smul' -> false;
    'sra' -> true;
    %% 'srax' -> true;
    'srl' -> true;
    %% 'srlx' -> true;
    'sub' -> false;
    'subcc' -> false;
    'xor' -> false
  end.

%%% Convert an extended SPARC AluOp back to a plain AluOp.
%%% This just maps cmp{,cc} to sub{,cc}.

xaluop_normalise(XAluOp) ->
  case XAluOp of
    'add' -> 'add';
    'addcc' -> 'addcc';
    'and' -> 'and';
    'andcc' -> 'andcc';
    %% 'cmp' -> 'sub';
    'cmpcc' -> 'subcc';
    'ldsb' -> 'ldsb';
    'ldub' -> 'ldub';
    'lduw' -> 'lduw';
    'or' -> 'or';
    'sll' -> 'sll';
    'smul' -> 'smul';
    'sra' -> 'sra';
    'srl' -> 'srl';
    'sub' -> 'sub';
    'subcc' -> 'subcc';
    'xor' -> 'xor'
  end.

%%% Convert an RTL condition code.

conv_cond(RtlCond) ->
  case RtlCond of
    eq	-> 'e';
    ne	-> 'ne';
    gt	-> 'g';
    gtu -> 'gu';	% >u
    ge	-> 'ge';
    geu -> 'geu';	% >=u
    lt	-> 'l';
    ltu -> 'lu';	% <u
    le	-> 'le';
    leu -> 'leu';	% <=u
    overflow -> 'vs';
    not_overflow -> 'vc'
  end.

%%% Commute a SPARC condition code.

commute_cond(Cond) ->	% if x Cond y, then y commute_cond(Cond) x
  case Cond of
    'e'   -> 'e';	% ==, ==
    'ne'  -> 'ne';	% !=, !=
    'g'   -> 'l';	% >, <
    'ge'  -> 'le';	% >=, <=
    'l'   -> 'g';	% <, >
    'le'  -> 'ge';	% <=, >=
    'gu'  -> 'lu';	% >u, <u
    'geu' -> 'leu';	% >=u, <=u
    'lu'  -> 'gu';	% <u, >u
    'leu' -> 'geu'	% <=u, >=u
    %% vs/vc: n/a
  end.

%%% Split a list of formal or actual parameters into the
%%% part passed in registers and the part passed on the stack.
%%% The parameters passed in registers are also tagged with
%%% the corresponding registers.

split_args(Args) ->
  split_args(0, hipe_sparc_registers:nr_args(), Args, []).

split_args(I, N, [Arg|Args], RegArgs) when I < N ->
  Reg = hipe_sparc_registers:arg(I),
  Temp = hipe_sparc:mk_temp(Reg, 'tagged'),
  split_args(I+1, N, Args, [{Arg,Temp}|RegArgs]);
split_args(_, _, StkArgs, RegArgs) ->
  {RegArgs, StkArgs}.

%%% Convert a list of actual parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_actuals([{Src,Dst}|Actuals], Rest) ->
  move_actuals(Actuals, mk_move(Src, Dst, Rest));
move_actuals([], Rest) ->
  Rest.

%%% Convert a list of formal parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_formals([{Dst,Src}|Formals], Rest) ->
  move_formals(Formals, [hipe_sparc:mk_pseudo_move(Src, Dst) | Rest]);
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
	      {hipe_sparc:mk_prim(Fun), Map};
	     true ->
	      {conv_mfa(Fun), Map}
	  end
      end
  end.

%%% Convert an MFA operand.

conv_mfa({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
  hipe_sparc:mk_mfa(M, F, A).

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

conv_fpreg(Opnd, Map) ->
  true = hipe_rtl:is_fpreg(Opnd),
  conv_dst(Opnd, Map).

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
      'double' -> false; %hipe_sparc_registers:is_precoloured_fpr(Name);
      _ -> hipe_sparc_registers:is_precoloured_gpr(Name)
    end,
  case IsPrecoloured of
    true ->
      {hipe_sparc:mk_temp(Name, Type), Map};
    false ->
      case vmap_lookup(Map, Opnd) of
	{value, NewTemp} ->
	  {NewTemp, Map};
	_ ->
	  NewTemp = hipe_sparc:mk_new_temp(Type),
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
  conv_formals(hipe_sparc_registers:nr_args(), Os, Map, []).

conv_formals(N, [O|Os], Map, Res) ->
  Type =
    case hipe_rtl:is_var(O) of
      true -> 'tagged';
      _ -> 'untagged'
    end,
  Dst =
    if N > 0 -> hipe_sparc:mk_new_temp(Type);	% allocatable
       true -> hipe_sparc:mk_new_nonallocatable_temp(Type)
    end,
  Map1 = vmap_bind(Map, O, Dst),
  conv_formals(N-1, Os, Map1, [Dst|Res]);
conv_formals(_, [], Map, Res) ->
  {lists:reverse(Res), Map}.

%%% new_untagged_temp -- conjure up an untagged scratch reg

new_untagged_temp() ->
  hipe_sparc:mk_new_temp('untagged').

%%% new_tagged_temp -- conjure up a tagged scratch reg

new_tagged_temp() ->
  hipe_sparc:mk_new_temp('tagged').

%%% Map from RTL var/reg operands to temps.

vmap_empty() ->
  gb_trees:empty().

vmap_lookup(Map, Key) ->
  gb_trees:lookup(Key, Map).

vmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).

word_size() ->
  hipe_rtl_arch:word_size().
