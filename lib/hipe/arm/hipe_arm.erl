%%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_arm).
-export([
	 mk_temp/2,
	 mk_new_temp/1,
	 mk_new_nonallocatable_temp/1,
	 is_temp/1,
	 temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,
	 temp_is_precoloured/1,

	 mk_mfa/3,

	 mk_prim/1,
	 is_prim/1,
	 prim_prim/1,

	 mk_sdesc/4,

	 mk_am2/3,
	 mk_am3/3,

	 mk_alu/5,

	 mk_b_fun/2,

	 mk_b_label/2,
	 mk_b_label/1,

	 mk_bl/3,

	 mk_blx/2,

	 mk_cmp/3,

	 mk_comment/1,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_load/3,
	 mk_load/6,

	 mk_ldrsb/2,

	 mk_move/3,
	 mk_move/2,

	 mk_pseudo_bc/4,

	 mk_pseudo_call/4,
	 pseudo_call_contlab/1,
	 pseudo_call_funv/1,
	 pseudo_call_sdesc/1,
	 pseudo_call_linkage/1,

	 mk_pseudo_call_prepare/1,
	 pseudo_call_prepare_nrstkargs/1,

	 mk_pseudo_li/2,

	 mk_pseudo_move/2,
	 is_pseudo_move/1,
	 pseudo_move_dst/1,
	 pseudo_move_src/1,

	 mk_pseudo_switch/3,

	 mk_pseudo_tailcall/4,
	 pseudo_tailcall_funv/1,
	 pseudo_tailcall_stkargs/1,
	 pseudo_tailcall_linkage/1,

	 mk_pseudo_tailcall_prepare/0,

	 mk_smull/4,

	 mk_store/3,
	 mk_store/6,

	 mk_pseudo_blr/0,
	 mk_bx/1,
	 mk_mflr/1,
	 mk_mtlr/1,
	 mk_lr/0,
	 mk_pc/0,

	 mk_li/2,
	 mk_li/3,

	 mk_addi/4,

	 try_aluop_imm/2,

	 mk_defun/8,
	 defun_mfa/1,
	 defun_formals/1,
	 defun_is_closure/1,
	 defun_is_leaf/1,
	 defun_code/1,
	 defun_data/1,
	 defun_var_range/1
	]).

-include("hipe_arm.hrl").

mk_temp(Reg, Type, Allocatable) ->
  #arm_temp{reg=Reg, type=Type, allocatable=Allocatable}.
mk_temp(Reg, Type) -> mk_temp(Reg, Type, true).
mk_new_temp(Type, Allocatable) ->
  mk_temp(hipe_gensym:get_next_var(arm), Type, Allocatable).
mk_new_temp(Type) -> mk_new_temp(Type, true).
mk_new_nonallocatable_temp(Type) -> mk_new_temp(Type, false).
is_temp(X) -> case X of #arm_temp{} -> true; _ -> false end.
temp_reg(#arm_temp{reg=Reg}) -> Reg.
temp_type(#arm_temp{type=Type}) -> Type.
temp_is_allocatable(#arm_temp{allocatable=A}) -> A.
temp_is_precoloured(#arm_temp{reg=Reg}) ->
  hipe_arm_registers:is_precoloured_gpr(Reg).

mk_mfa(M, F, A) -> #arm_mfa{m=M, f=F, a=A}.

mk_prim(Prim) -> #arm_prim{prim=Prim}.
is_prim(X) -> case X of #arm_prim{} -> true; _ -> false end.
prim_prim(#arm_prim{prim=Prim}) -> Prim.

mk_am2(Src, Sign, Offset) -> #am2{src=Src, sign=Sign, offset=Offset}.
mk_am3(Src, Sign, Offset) -> #am3{src=Src, sign=Sign, offset=Offset}.

mk_alu(AluOp, S, Dst, Src, Am1) ->
  #alu{aluop=AluOp, s=S, dst=Dst, src=Src, am1=Am1}.
mk_alu(AluOp, Dst, Src, Am1) -> mk_alu(AluOp, false, Dst, Src, Am1).

mk_b_fun(Fun, Linkage) -> #b_fun{'fun'=Fun, linkage=Linkage}.

mk_b_label(Cond, Label) -> #b_label{'cond'=Cond, label=Label}.
mk_b_label(Label) -> mk_b_label('al', Label).

mk_bl(Fun, SDesc, Linkage) -> #bl{'fun'=Fun, sdesc=SDesc, linkage=Linkage}.

mk_blx(Src, SDesc) -> #blx{src=Src, sdesc=SDesc}.

mk_cmp(CmpOp, Src, Am1) -> #cmp{cmpop=CmpOp, src=Src, am1=Am1}.

mk_sdesc(ExnLab, FSize, Arity, Live) ->
  #arm_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

mk_comment(Term) -> #comment{term=Term}.

mk_label(Label) -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

mk_load(LdOp, Dst, Am2) -> #load{ldop=LdOp, dst=Dst, am2=Am2}.

mk_load(LdOp, Dst, Base, Offset, Scratch, Rest) when is_integer(Offset) ->
  {Sign,AbsOffset} =
    if Offset < 0 -> {'-', -Offset};
       true -> {'+', Offset}
    end,
  if AbsOffset =< 4095 ->
      Am2 = #am2{src=Base,sign=Sign,offset=AbsOffset},
      [mk_load(LdOp, Dst, Am2) | Rest];
     true ->
      Index =
	begin
	  DstReg = temp_reg(Dst),
	  BaseReg = temp_reg(Base),
	  if DstReg =/= BaseReg -> Dst;
	     true -> mk_scratch(Scratch)
	  end
	end,
      Am2 = #am2{src=Base,sign=Sign,offset=Index},
      mk_li(Index, AbsOffset,
	    [mk_load(LdOp, Dst, Am2) | Rest])
  end.

mk_scratch(Scratch) ->
  case Scratch of
    'temp2' -> mk_temp(hipe_arm_registers:temp2(), 'untagged');
    'new' -> mk_new_temp('untagged')
  end.

mk_ldrsb(Dst, Am3) -> #ldrsb{dst=Dst, am3=Am3}.

mk_move(MovOp, S, Dst, Am1) -> #move{movop=MovOp, s=S, dst=Dst, am1=Am1}.
mk_move(S, Dst, Am1) -> mk_move('mov', S, Dst, Am1).
mk_move(Dst, Am1) -> mk_move('mov', false, Dst, Am1).

mk_pseudo_bc(Cond, TrueLab, FalseLab, Pred) ->
  if Pred >= 0.5 ->
      mk_pseudo_bc_simple(negate_cond(Cond), FalseLab,
			  TrueLab, 1.0-Pred);
     true ->
      mk_pseudo_bc_simple(Cond, TrueLab, FalseLab, Pred)
  end.

mk_pseudo_bc_simple(Cond, TrueLab, FalseLab, Pred) when Pred =< 0.5 ->
  #pseudo_bc{'cond'=Cond, true_label=TrueLab,
	     false_label=FalseLab, pred=Pred}.

negate_cond(Cond) ->
  case Cond of
    'lt' -> 'ge';	% <, >=
    'ge' -> 'lt';	% >=, <
    'gt' -> 'le';	% >, <=
    'le' -> 'gt';	% <=, >
    'eq' -> 'ne';	% ==, !=
    'ne' -> 'eq';	% !=, ==
    'hi' -> 'ls';	% >u, <=u
    'ls' -> 'hi';	% <=u, >u
    'hs' -> 'lo';	% >=u, <u
    'lo' -> 'hs';	% <u, >=u
    'vs' -> 'vc';	% overflow, not_overflow
    'vc' -> 'vs'	% not_overflow, overflow
  end.

mk_pseudo_call(FunV, SDesc, ContLab, Linkage) ->
  #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage}.
pseudo_call_funv(#pseudo_call{funv=FunV}) -> FunV.
pseudo_call_sdesc(#pseudo_call{sdesc=SDesc}) -> SDesc.
pseudo_call_contlab(#pseudo_call{contlab=ContLab}) -> ContLab.
pseudo_call_linkage(#pseudo_call{linkage=Linkage}) -> Linkage.

mk_pseudo_call_prepare(NrStkArgs) ->
  #pseudo_call_prepare{nrstkargs=NrStkArgs}.
pseudo_call_prepare_nrstkargs(#pseudo_call_prepare{nrstkargs=NrStkArgs}) ->
  NrStkArgs.

mk_pseudo_li(Dst, Imm) ->
  #pseudo_li{dst=Dst, imm=Imm, label=hipe_gensym:get_next_label(arm)}.

mk_pseudo_move(Dst, Src) -> #pseudo_move{dst=Dst, src=Src}.
is_pseudo_move(I) -> case I of #pseudo_move{} -> true; _ -> false end.
pseudo_move_dst(#pseudo_move{dst=Dst}) -> Dst.
pseudo_move_src(#pseudo_move{src=Src}) -> Src.

mk_pseudo_switch(JTab, Index, Labels) ->
  #pseudo_switch{jtab=JTab, index=Index, labels=Labels}.

mk_pseudo_tailcall(FunV, Arity, StkArgs, Linkage) ->
  #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage}.
pseudo_tailcall_funv(#pseudo_tailcall{funv=FunV}) -> FunV.
pseudo_tailcall_stkargs(#pseudo_tailcall{stkargs=StkArgs}) -> StkArgs.
pseudo_tailcall_linkage(#pseudo_tailcall{linkage=Linkage}) -> Linkage.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.

mk_smull(DstLo, DstHi, Src1, Src2) ->
  #smull{dstlo=DstLo, dsthi=DstHi, src1=Src1, src2=Src2}.

mk_store(StOp, Src, Am2) -> #store{stop=StOp, src=Src, am2=Am2}.

mk_store(StOp, Src, Base, Offset, Scratch, Rest) when is_integer(Offset) ->
  {Sign,AbsOffset} =
    if Offset < 0 -> {'-', -Offset};
       true -> {'+', Offset}
    end,
  if AbsOffset =< 4095 ->
      Am2 = #am2{src=Base,sign=Sign,offset=AbsOffset},
      [mk_store(StOp, Src, Am2) | Rest];
     true ->
      Index = mk_scratch(Scratch),
      Am2 = #am2{src=Base,sign=Sign,offset=Index},
      mk_li(Index, AbsOffset,
	    [mk_store(StOp, Src, Am2) | Rest])
  end.

mk_pseudo_blr() -> #pseudo_blr{}.
mk_bx(Src) -> #pseudo_bx{src=Src}.
mk_mflr(Dst) -> mk_move(Dst, mk_lr()).
mk_mtlr(Src) -> mk_move(mk_lr(), Src).
mk_lr() -> mk_temp(hipe_arm_registers:lr(), 'untagged').
mk_pc() -> mk_temp(hipe_arm_registers:pc(), 'untagged').

%%% Load an integer constant into a register.
mk_li(Dst, Value) -> mk_li(Dst, Value, []).

mk_li(Dst, Value, Rest) ->
  %% XXX: expand to handle 2-instruction sequences
  case try_aluop_imm('mov', Value) of
    {NewMovOp,Am1} ->
      [mk_move(NewMovOp, false, Dst, Am1) | Rest];
    [] ->
      [mk_pseudo_li(Dst, Value) | Rest]
  end.

%%% Add an integer constant. Dst may equal Src,
%%% in which case temp2 may be clobbered.

mk_addi(Dst, Src, Value, Rest) ->
  case try_aluop_imm('add', Value) of
    {NewAluOp,Am1} ->
      [mk_alu(NewAluOp, Dst, Src, Am1) | Rest];
    [] ->
      Tmp =
	begin
	  DstReg = temp_reg(Dst),
	  SrcReg = temp_reg(Src),
	  if DstReg =:= SrcReg ->
	      mk_temp(hipe_arm_registers:temp2(), 'untagged');
	     true -> Dst
	  end
	end,
      [mk_pseudo_li(Tmp, Value), mk_alu('add', Dst, Src, Tmp) | Rest]
  end.

try_aluop_imm(AluOp, Imm) -> % -> {NewAluOp,Am1} or []
  case imm_to_am1(Imm) of
    (Am1={_Imm8,_Imm4}) -> {AluOp, Am1};
    [] ->
      case invert_aluop_imm(AluOp, Imm) of
	{NewAluOp,NewImm} ->
	  case imm_to_am1(NewImm) of
	    (Am1={_Imm8,_Imm4}) -> {NewAluOp, Am1};
	    [] -> []
	  end;
	[] -> []
      end
  end.

invert_aluop_imm(AluOp, Imm) ->
  case AluOp of
    'mov' -> {'mvn', bnot Imm};
    'mvn' -> {'mov', bnot Imm};
    'cmp' -> {'cmn', -Imm};
    'cmn' -> {'cmp', -Imm};
    'and' -> {'bic', bnot Imm};
    'bic' -> {'and', bnot Imm};
    'orr' -> {'orn', bnot Imm};
    'orn' -> {'orr', bnot Imm};
    'add' -> {'sub', -Imm};
    'sub' -> {'add', -Imm};
    _	  -> [] % no inverted form available
  end.

imm_to_am1(Imm) -> imm_to_am1(Imm band 16#FFFFFFFF, 16).
imm_to_am1(Imm, RotCnt) ->
  if Imm >= 0, Imm =< 255 -> {Imm, RotCnt band 15};
     true ->
      NewRotCnt = RotCnt - 1,
      if NewRotCnt =:= 0 -> []; % full circle, no joy
	 true ->
	  NewImm = (Imm bsr 2) bor ((Imm band 3) bsl 30),
	  imm_to_am1(NewImm, NewRotCnt)
      end
  end.      

mk_defun(MFA, Formals, IsClosure, IsLeaf, Code, Data, VarRange, LabelRange) ->
  #defun{mfa=MFA, formals=Formals, code=Code, data=Data,
	 isclosure=IsClosure, isleaf=IsLeaf,
	 var_range=VarRange, label_range=LabelRange}.
defun_mfa(#defun{mfa=MFA}) -> MFA.
defun_formals(#defun{formals=Formals}) -> Formals.
defun_is_closure(#defun{isclosure=IsClosure}) -> IsClosure.
defun_is_leaf(#defun{isleaf=IsLeaf}) -> IsLeaf.
defun_code(#defun{code=Code}) -> Code.
defun_data(#defun{data=Data}) -> Data.
defun_var_range(#defun{var_range=VarRange}) -> VarRange.
