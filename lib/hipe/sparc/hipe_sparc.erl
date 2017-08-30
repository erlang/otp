%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_sparc).
-export([
	 mk_temp/2,
	 mk_new_temp/1,
	 mk_new_nonallocatable_temp/1,
	 is_temp/1,
	 temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,
	 temp_is_precoloured/1,

	 mk_g0/0,
	 mk_ra/0,
	 mk_rv/0,
	 mk_sp/0,
	 mk_temp1/0,
	 mk_temp2/0,

	 mk_simm13/1,
	 mk_uimm5/1,

	 mk_mfa/3,

	 mk_prim/1,
	 is_prim/1,
	 prim_prim/1,

	 mk_sdesc/4,

	 mk_alu/4,
	 mk_mov/2,
	 mk_load/6,

	 mk_bp/3,
	 mk_b_label/1,

	 %% mk_br/4,

	 mk_call_rec/3,

	 mk_call_tail/2,

	 mk_comment/1,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_jmp/3,
	 mk_jmpl/2,

	 mk_pseudo_bp/4,
	 negate_cond/1,

	 %% mk_pseudo_br/5,
	 %% negate_rcond/1,

	 mk_pseudo_call/4,
	 pseudo_call_contlab/1,
	 pseudo_call_funv/1,
	 pseudo_call_linkage/1,
	 pseudo_call_sdesc/1,

	 mk_pseudo_call_prepare/1,
	 pseudo_call_prepare_nrstkargs/1,

	 mk_pseudo_move/2,
	 is_pseudo_move/1,
	 pseudo_move_dst/1,
	 pseudo_move_src/1,

	 mk_pseudo_ret/0,

	 mk_pseudo_set/2,

	 mk_pseudo_tailcall/4,
	 pseudo_tailcall_funv/1,
	 pseudo_tailcall_linkage/1,
	 pseudo_tailcall_stkargs/1,

	 mk_pseudo_tailcall_prepare/0,

	 mk_rdy/1,

	 %% mk_sethi/2,
	 mk_nop/0,
	 mk_set/2,
	 mk_set/3,
	 mk_addi/4,

	 mk_store/4,
	 mk_store/6,

	 mk_fp_binary/4,

	 mk_fp_unary/3,

	 mk_pseudo_fload/4,
	 mk_fload/4,

	 mk_pseudo_fmove/2,
	 is_pseudo_fmove/1,
	 pseudo_fmove_src/1,
	 pseudo_fmove_dst/1,

	 mk_pseudo_fstore/3,
	 mk_fstore/4,

	 mk_defun/8,
	 defun_code/1,
	 defun_data/1,
	 defun_formals/1,
	 defun_is_closure/1,
	 defun_is_leaf/1,
	 defun_mfa/1,
	 defun_var_range/1
	]).

-include("hipe_sparc.hrl").

mk_temp(Reg, Type, Allocatable) ->
  #sparc_temp{reg=Reg, type=Type, allocatable=Allocatable}.
mk_temp(Reg, Type) -> mk_temp(Reg, Type, true).
mk_new_temp(Type, Allocatable) ->
  mk_temp(hipe_gensym:get_next_var(sparc), Type, Allocatable).
mk_new_temp(Type) -> mk_new_temp(Type, true).
mk_new_nonallocatable_temp(Type) -> mk_new_temp(Type, false).
is_temp(X) -> case X of #sparc_temp{} -> true; _ -> false end.
temp_reg(#sparc_temp{reg=Reg}) -> Reg.
temp_type(#sparc_temp{type=Type}) -> Type.
temp_is_allocatable(#sparc_temp{allocatable=A}) -> A.
temp_is_precoloured(#sparc_temp{reg=Reg,type=Type}) ->
  case Type of
    %% 'double' -> hipe_sparc_registers:is_precoloured_fpr(Reg);
    _ -> hipe_sparc_registers:is_precoloured_gpr(Reg)
  end.

mk_g0() -> mk_temp(hipe_sparc_registers:g0(), 'untagged').
mk_ra() -> mk_temp(hipe_sparc_registers:return_address(), 'untagged').
mk_rv() -> mk_temp(hipe_sparc_registers:return_value(), 'tagged').
mk_sp() -> mk_temp(hipe_sparc_registers:stack_pointer(), 'untagged').
mk_temp1() -> mk_temp(hipe_sparc_registers:temp1(), 'untagged').
mk_temp2() -> mk_temp(hipe_sparc_registers:temp2(), 'untagged').

mk_simm13(Value) -> #sparc_simm13{value=Value}.
mk_uimm5(Value) -> #sparc_uimm5{value=Value}.
mk_uimm22(Value) -> #sparc_uimm22{value=Value}.

mk_mfa(M, F, A) -> #sparc_mfa{m=M, f=F, a=A}.

mk_prim(Prim) -> #sparc_prim{prim=Prim}.
is_prim(X) -> case X of #sparc_prim{} -> true; _ -> false end.
prim_prim(#sparc_prim{prim=Prim}) -> Prim.

mk_sdesc(ExnLab, FSize, Arity, Live) ->
  #sparc_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

mk_alu(AluOp, Src1, Src2, Dst) ->
  #alu{aluop=AluOp, src1=Src1, src2=Src2, dst=Dst}.
mk_mov(Src, Dst) -> mk_alu('or', mk_g0(), Src, Dst).

mk_bp(Cond, Label, Pred) -> #bp{'cond'=Cond, label=Label, pred=Pred}.
mk_b_label(Label) -> mk_bp('a', Label, 1.0).

-ifdef(notdef).	% XXX: only for sparc64, alas
mk_br(RCond, Src, Label, Pred) ->
  #br{rcond=RCond, src=Src, label=Label, pred=Pred}.
-endif.

mk_call_rec(Fun, SDesc, Linkage) ->
  #call_rec{'fun'=Fun, sdesc=SDesc, linkage=Linkage}.

mk_call_tail(Fun, Linkage) -> #call_tail{'fun'=Fun, linkage=Linkage}.

mk_comment(Term) -> #comment{term=Term}.

mk_label(Label) -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

mk_jmp(Src1, Src2, Labels) -> #jmp{src1=Src1, src2=Src2, labels=Labels}.

mk_jmpl(Src, SDesc) -> #jmpl{src=Src, sdesc=SDesc}.

mk_pseudo_bp(Cond, TrueLab, FalseLab, Pred) ->
  if Pred >= 0.5 ->
      mk_pseudo_bp_simple(negate_cond(Cond), FalseLab,
			  TrueLab, 1.0-Pred);
     true ->
      mk_pseudo_bp_simple(Cond, TrueLab, FalseLab, Pred)
  end.

mk_pseudo_bp_simple(Cond, TrueLab, FalseLab, Pred) when Pred =< 0.5 ->
  #pseudo_bp{'cond'=Cond, true_label=TrueLab,
	     false_label=FalseLab, pred=Pred}.

negate_cond(Cond) ->
  case Cond of
    'l'  -> 'ge';	% <, >=
    'ge' -> 'l';	% >=, <
    'g'  -> 'le';	% >, <=
    'le' -> 'g';	% <=, >
    'e'  -> 'ne';	% ==, !=
    'ne' -> 'e';	% !=, ==
    'gu' -> 'leu';	% >u, <=u
    'leu'-> 'gu';	% <=u, >u
    'geu'-> 'lu';	% >=u, <u
    'lu' -> 'geu';	% <u, >=u
    'vs' -> 'vc';	% overflow, not_overflow
    'vc' -> 'vs'	% not_overflow, overflow
  end.

-ifdef(notdef).	% XXX: only for sparc64, alas
mk_pseudo_br(RCond, Src, TrueLab, FalseLab, Pred) ->
  if Pred >= 0.5 ->
      mk_pseudo_br_simple(negate_rcond(RCond), Src, FalseLab,
			  TrueLab, 1.0-Pred);
     true ->
      mk_pseudo_br_simple(RCond, Src, TrueLab, FalseLab, Pred)
  end.

mk_pseudo_br_simple(RCond, Src, TrueLab, FalseLab, Pred) when Pred =< 0.5 ->
  #pseudo_br{rcond=RCond, src=Src, true_label=TrueLab,
	     false_label=FalseLab, pred=Pred}.

negate_rcond(RCond) ->
  case RCond of
    'z'   -> 'nz';	% ==, !=
    'nz'  -> 'z';	% !=, ==
    'gz'  -> 'lez';	% >, <=
    'lez' -> 'gz';	% <=, >
    'gez' -> 'lz';	% >=, <
    'lz'  -> 'gez'	% <, >=
  end.
-endif.

mk_pseudo_call(FunV, SDesc, ContLab, Linkage) ->
  #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage}.
pseudo_call_funv(#pseudo_call{funv=FunV}) -> FunV.
pseudo_call_contlab(#pseudo_call{contlab=ContLab}) -> ContLab.
pseudo_call_linkage(#pseudo_call{linkage=Linkage}) -> Linkage.
pseudo_call_sdesc(#pseudo_call{sdesc=SDesc}) -> SDesc.

mk_pseudo_call_prepare(NrStkArgs) ->
  #pseudo_call_prepare{nrstkargs=NrStkArgs}.
pseudo_call_prepare_nrstkargs(#pseudo_call_prepare{nrstkargs=NrStkArgs}) ->
  NrStkArgs.

mk_pseudo_move(Src, Dst) -> #pseudo_move{src=Src, dst=Dst}.
is_pseudo_move(I) -> case I of #pseudo_move{} -> true; _ -> false end.
pseudo_move_dst(#pseudo_move{dst=Dst}) -> Dst.
pseudo_move_src(#pseudo_move{src=Src}) -> Src.

mk_pseudo_ret() -> #pseudo_ret{}.

mk_pseudo_set(Imm, Dst) -> #pseudo_set{imm=Imm, dst=Dst}.

mk_pseudo_tailcall(FunV, Arity, StkArgs, Linkage) ->
  #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage}.
pseudo_tailcall_funv(#pseudo_tailcall{funv=FunV}) -> FunV.
pseudo_tailcall_linkage(#pseudo_tailcall{linkage=Linkage}) -> Linkage.
pseudo_tailcall_stkargs(#pseudo_tailcall{stkargs=StkArgs}) -> StkArgs.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.

mk_rdy(Dst) -> #rdy{dst=Dst}.

mk_sethi(UImm22, Dst) -> #sethi{uimm22=UImm22, dst=Dst}.
mk_nop() -> mk_sethi(mk_uimm22(0), mk_g0()).

%%% Load an integer constant into a register.
mk_set(Value, Dst) -> mk_set(Value, Dst, []).

mk_set(Value, Dst, Tail) ->
  if -4096 =< Value, Value < 4096 ->
      [mk_alu('or', mk_g0(), mk_simm13(Value), Dst) | Tail];
     true ->
      Hi22 = mk_uimm22((Value bsr 10) band 16#003FFFFF),
      case (Value band 16#3FF) of
	0 ->
	  [mk_sethi(Hi22, Dst) | Tail];
	Lo10 ->
	  [mk_sethi(Hi22, Dst),
	   mk_alu('or', Dst, mk_simm13(Lo10), Dst) |
	   Tail]
      end
  end.

%%% Add an integer constant. Dst may equal Src,
%%% in which case temp2 may be clobbered.
mk_addi(Src, Value, Dst, Tail) ->
  if -4096 =< Value, Value < 4096 ->
      [mk_alu('add', Src, mk_simm13(Value), Dst) | Tail];
     true ->
      Tmp =
	begin
	  DstReg = temp_reg(Dst),
	  SrcReg = temp_reg(Src),
	  if DstReg =:= SrcReg -> mk_temp2();
	     true -> Dst
	  end
	end,
      mk_set(Value, Tmp, [mk_alu('add', Src, Tmp, Dst) | Tail])
  end.

mk_store(StOp, Src, Base, Disp) ->
  #store{stop=StOp, src=Src, base=Base, disp=Disp}.

mk_store(StOp, Src, Base, Offset, Scratch, Rest) when is_integer(Offset) ->
  if -4096 =< Offset, Offset < 4096 ->
      [mk_store(StOp, Src, Base, mk_simm13(Offset)) | Rest];
     true ->
      Index = mk_scratch(Scratch),
      mk_set(Offset, Index, [mk_store(StOp, Src, Base, Index) | Rest])
  end.

mk_load(LdOp, Base, Disp, Dst) ->
  mk_alu(LdOp, Base, Disp, Dst).

mk_load(LdOp, Base, Offset, Dst, Scratch, Rest) when is_integer(Offset) ->
  if -4096 =< Offset, Offset < 4096 ->
      [mk_load(LdOp, Base, mk_simm13(Offset), Dst) | Rest];
     true ->
      Index =
	begin
	  DstReg = temp_reg(Dst),
	  BaseReg = temp_reg(Base),
	  if DstReg =/= BaseReg -> Dst;
	     true -> mk_scratch(Scratch)
	  end
	end,
      mk_set(Offset, Index, [mk_load(LdOp, Base, Index, Dst) | Rest])
  end.

mk_scratch(Scratch) ->
  case Scratch of
    'temp2' -> mk_temp2();
    'new' -> mk_new_temp('untagged')
  end.

mk_fp_binary(FpBinOp, Src1, Src2, Dst) ->
  #fp_binary{fp_binop=FpBinOp, src1=Src1, src2=Src2, dst=Dst}.

mk_fp_unary(FpUnOp, Src, Dst) -> #fp_unary{fp_unop=FpUnOp, src=Src, dst=Dst}.

mk_pseudo_fload(Base, Disp, Dst, IsSingle) ->
  #pseudo_fload{base=Base, disp=Disp, dst=Dst, is_single=IsSingle}.

mk_fload(Base, Disp, Dst, Scratch) when is_integer(Disp) ->
  if -4096 =< Disp, Disp < (4096-4) ->
      [mk_pseudo_fload(Base, mk_simm13(Disp), Dst, false)];
     true ->
      Tmp = mk_scratch(Scratch),
      mk_set(Disp, Tmp,
	     [mk_alu('add', Tmp, Base, Tmp),
	      mk_pseudo_fload(Tmp, mk_simm13(0), Dst, false)])
  end.

mk_pseudo_fmove(Src, Dst) -> #pseudo_fmove{src=Src, dst=Dst}.
is_pseudo_fmove(I) -> case I of #pseudo_fmove{} -> true; _ -> false end.
pseudo_fmove_src(#pseudo_fmove{src=Src}) -> Src.
pseudo_fmove_dst(#pseudo_fmove{dst=Dst}) -> Dst.

mk_pseudo_fstore(Src, Base, Disp) ->
  #pseudo_fstore{src=Src, base=Base, disp=Disp}.

mk_fstore(Src, Base, Disp, Scratch) when is_integer(Disp) ->
  if -4096 =< Disp, Disp < (4096-4) ->
      [mk_pseudo_fstore(Src, Base, hipe_sparc:mk_simm13(Disp))];
     true ->
      Tmp = mk_scratch(Scratch),
      mk_set(Disp, Tmp,
	     [mk_alu('add', Tmp, Base, Tmp),
	      mk_pseudo_fstore(Src, Tmp, mk_simm13(0))])
  end.

mk_defun(MFA, Formals, IsClosure, IsLeaf, Code, Data, VarRange, LabelRange) ->
  #defun{mfa=MFA, formals=Formals, code=Code, data=Data,
	 isclosure=IsClosure, isleaf=IsLeaf,
	 var_range=VarRange, label_range=LabelRange}.
defun_code(#defun{code=Code}) -> Code.
defun_data(#defun{data=Data}) -> Data.
defun_formals(#defun{formals=Formals}) -> Formals.
defun_is_closure(#defun{isclosure=IsClosure}) -> IsClosure.
defun_is_leaf(#defun{isleaf=IsLeaf}) -> IsLeaf.
defun_mfa(#defun{mfa=MFA}) -> MFA.
defun_var_range(#defun{var_range=VarRange}) -> VarRange.
