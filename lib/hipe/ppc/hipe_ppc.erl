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

-module(hipe_ppc).
-export([
	 mk_temp/2,
	 mk_new_temp/1,
	 mk_new_nonallocatable_temp/1,
	 is_temp/1,
	 temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,
	 temp_is_precoloured/1,

	 mk_simm16/1,
	 mk_uimm16/1,

	 mk_mfa/3,

	 mk_prim/1,
	 is_prim/1,
	 prim_prim/1,

	 mk_sdesc/4,

	 mk_alu/4,

	 mk_b_fun/2,

	 mk_b_label/1,

	 mk_bc/3,

	 mk_bctr/1,

	 mk_bctrl/1,

	 mk_bl/3,

	 mk_blr/0,

	 mk_cmp/3,
	 cmpop_word/0,
	 cmpiop_word/0,
	 cmplop_word/0,
	 cmpliop_word/0,

	 mk_comment/1,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_li/2,
	 mk_li/3,
	 mk_addi/4,

	 mk_load/4,
	 mk_loadx/4,
	 mk_load/6,
	 ldop_to_ldxop/1,
	 ldop_word/0,
	 ldop_wordx/0,

	 mk_mfspr/2,

	 mk_mtcr/1,

	 mk_mtspr/2,

	 mk_pseudo_bc/4,
	 negate_bcond/1,

	 mk_pseudo_call/4,
	 pseudo_call_contlab/1,
	 pseudo_call_func/1,
	 pseudo_call_sdesc/1,
	 pseudo_call_linkage/1,

	 mk_pseudo_call_prepare/1,
	 pseudo_call_prepare_nrstkargs/1,

	 mk_pseudo_li/2,

	 mk_pseudo_move/2,
	 is_pseudo_move/1,
	 pseudo_move_dst/1,
	 pseudo_move_src/1,

	 mk_pseudo_spill_move/3,
	 is_pseudo_spill_move/1,

	 mk_pseudo_tailcall/4,
	 pseudo_tailcall_func/1,
	 pseudo_tailcall_stkargs/1,
	 pseudo_tailcall_linkage/1,

	 mk_pseudo_tailcall_prepare/0,

	 mk_store/4,
	 mk_storex/4,
	 mk_store/6,
	 stop_to_stxop/1,
	 stop_word/0,
	 stop_wordx/0,

	 mk_unary/3,

	 mk_lfd/3,
	 mk_lfdx/3,
	 mk_fload/4,

	 %% mk_stfd/3,
	 mk_stfdx/3,
	 mk_fstore/4,

	 mk_fp_binary/4,

	 mk_fp_unary/3,

	 mk_pseudo_fmove/2,
	 is_pseudo_fmove/1,
	 pseudo_fmove_dst/1,
	 pseudo_fmove_src/1,

	 mk_pseudo_spill_fmove/3,
	 is_pseudo_spill_fmove/1,

	 mk_defun/8,
	 defun_mfa/1,
	 defun_formals/1,
	 defun_is_closure/1,
	 defun_is_leaf/1,
	 defun_code/1,
	 defun_data/1,
	 defun_var_range/1]).

-include("hipe_ppc.hrl").

mk_temp(Reg, Type, Allocatable) ->
  #ppc_temp{reg=Reg, type=Type, allocatable=Allocatable}.
mk_temp(Reg, Type) -> mk_temp(Reg, Type, true).
mk_new_temp(Type, Allocatable) ->
  mk_temp(hipe_gensym:get_next_var(ppc), Type, Allocatable).
mk_new_temp(Type) -> mk_new_temp(Type, true).
mk_new_nonallocatable_temp(Type) -> mk_new_temp(Type, false).
is_temp(X) -> case X of #ppc_temp{} -> true; _ -> false end.
temp_reg(#ppc_temp{reg=Reg}) -> Reg.
temp_type(#ppc_temp{type=Type}) -> Type.
temp_is_allocatable(#ppc_temp{allocatable=A}) -> A.
temp_is_precoloured(#ppc_temp{reg=Reg,type=Type}) ->
  case Type of
    'double' -> hipe_ppc_registers:is_precoloured_fpr(Reg);
    _ -> hipe_ppc_registers:is_precoloured_gpr(Reg)
  end.

mk_simm16(Value) when Value >= -(1 bsl 15), Value < (1 bsl 15) ->
  #ppc_simm16{value=Value}.
mk_uimm16(Value) when Value >= 0, Value < (1 bsl 16) ->
  #ppc_uimm16{value=Value}.

mk_mfa(M, F, A) -> #ppc_mfa{m=M, f=F, a=A}.

mk_prim(Prim) -> #ppc_prim{prim=Prim}.
is_prim(X) -> case X of #ppc_prim{} -> true; _ -> false end.
prim_prim(#ppc_prim{prim=Prim}) -> Prim.

mk_sdesc(ExnLab, FSize, Arity, Live) ->
  #ppc_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

mk_alu(AluOp, Dst, Src1, Src2) ->
  #alu{aluop=AluOp, dst=Dst, src1=Src1, src2=Src2}.

mk_b_fun(Fun, Linkage) -> #b_fun{'fun'=Fun, linkage=Linkage}.

mk_b_label(Label) -> #b_label{label=Label}.

mk_bc(BCond, Label, Pred) -> #bc{bcond=BCond, label=Label, pred=Pred}.

mk_bctr(Labels) -> #bctr{labels=Labels}.

mk_bctrl(SDesc) -> #bctrl{sdesc=SDesc}.

mk_bl(Fun, SDesc, Linkage) -> #bl{'fun'=Fun, sdesc=SDesc, linkage=Linkage}.

mk_blr() -> #blr{}.

mk_cmp(CmpOp, Src1, Src2) -> #cmp{cmpop=CmpOp, src1=Src1, src2=Src2}.

cmpop_word() ->
  case get(hipe_target_arch) of
    powerpc -> 'cmp';
    ppc64 -> 'cmpd'
  end.

cmpiop_word() ->
  case get(hipe_target_arch) of
    powerpc -> 'cmpi';
    ppc64 -> 'cmpdi'
  end.

cmplop_word() ->
  case get(hipe_target_arch) of
    powerpc -> 'cmpl';
    ppc64 -> 'cmpld'
  end.

cmpliop_word() ->
  case get(hipe_target_arch) of
    powerpc -> 'cmpli';
    ppc64 -> 'cmpldi'
  end.


mk_comment(Term) -> #comment{term=Term}.

mk_label(Label) -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

%%% Load an integer constant into a register.
mk_li(Dst, Value) -> mk_li(Dst, Value, []).

mk_li(Dst, Value, Tail) ->   % Dst can be R0
  R0 = mk_temp(0, 'untagged'),
  %% Check if immediate can fit in the 32 bits, this is obviously a
  %% sufficient check for PPC32
  if Value >= -16#80000000,
     Value =< 16#7FFFFFFF ->
      mk_li32(Dst, R0, Value, Tail);
     true ->
      Highest = case (Value bsr 48) of       % Value@highest
		  TopBitSet when TopBitSet >= (1 bsl 15) ->
		    TopBitSet - (1 bsl 16);  % encoder needs it to be negative
		  FitsSimm16 -> FitsSimm16
		end,
      Higher = (Value bsr 32) band 16#FFFF,  % Value@higher
      High = (Value bsr 16) band 16#FFFF,    % Value@h
      Low = Value band 16#FFFF,              % Value@l
      LdLo =
	case Low of
	  0 -> Tail;
	  _ -> [mk_alu('ori', Dst, Dst, mk_uimm16(Low)) | Tail]
	end,
      Ld32bits =
	case High of
	  0 -> LdLo;
	  _ -> [mk_alu('oris', Dst, Dst, mk_uimm16(High)) | LdLo]
	end,
      [mk_alu('addis', Dst, R0, mk_simm16(Highest)),
       mk_alu('ori', Dst, Dst, mk_uimm16(Higher)),
       mk_alu('sldi', Dst, Dst, mk_uimm16(32)) |
       Ld32bits]
  end.

mk_li32(Dst, R0, Value, Tail) ->
  case at_ha(Value) of
    0 ->
      %% Value[31:16] are the sign-extension of Value[15].
      %% Use a single addi to load and sign-extend 16 bits.
      [mk_alu('addi', Dst, R0, mk_simm16(at_l(Value))) | Tail];
    _ ->
      %% Use addis to load the high 16 bits, followed by an
      %% optional ori to load non sign-extended low 16 bits.
      High = simm16sext((Value bsr 16) band 16#FFFF),
      [mk_alu('addis', Dst, R0, mk_simm16(High)) |
       case (Value band 16#FFFF) of
	 0 -> Tail;
	 Low -> [mk_alu('ori', Dst, Dst, mk_uimm16(Low)) | Tail]
       end]
  end.

mk_addi(Dst, R0, Value, Tail) ->
  Low = at_l(Value),
  High = at_ha(Value),
  case High of
    0 ->
      [mk_alu('addi', Dst, R0, mk_simm16(Low)) |
       Tail];
    _ ->
      case Low of
	0 ->
	  [mk_alu('addis', Dst, R0, mk_simm16(High)) |
	   Tail];
	_ ->
	  [mk_alu('addi', Dst, R0, mk_simm16(Low)),
	   mk_alu('addis', Dst, Dst, mk_simm16(High)) |
	   Tail]
      end
  end.

at_l(Value) ->
  simm16sext(Value band 16#FFFF).

at_ha(Value) ->
  simm16sext(((Value + 16#8000) bsr 16) band 16#FFFF).

simm16sext(Value) ->
  if Value >= 32768 -> (-1 bsl 16) bor Value;
     true -> Value
  end.

mk_load(LDop, Dst, Disp, Base) ->
  #load{ldop=LDop, dst=Dst, disp=Disp, base=Base}.

mk_loadx(LdxOp, Dst, Base1, Base2) ->
  #loadx{ldxop=LdxOp, dst=Dst, base1=Base1, base2=Base2}.

mk_load(LdOp, Dst, Offset, Base, Scratch, Rest) when is_integer(Offset) ->
  RequireAlignment =
    case LdOp of
      'ld' -> true;
      'ldx' -> true;
      _ -> false
    end,
  if Offset >= -32768, Offset =< 32767,
     not RequireAlignment orelse Offset band 3 =:= 0 ->
	  [mk_load(LdOp, Dst, Offset, Base) | Rest];
     true ->
      LdxOp = ldop_to_ldxop(LdOp),
      Index =
	begin
	  DstReg = temp_reg(Dst),
	  BaseReg = temp_reg(Base),
	  if DstReg =/= BaseReg -> Dst;
	     true -> mk_scratch(Scratch)
	  end
	end,
      mk_li(Index, Offset,
	    [mk_loadx(LdxOp, Dst, Base, Index) | Rest])
  end.

ldop_to_ldxop(LdOp) ->
  case LdOp of
    'lbz' -> 'lbzx';
    'lha' -> 'lhax';
    'lhz' -> 'lhzx';
    'lwa' -> 'lwax';
    'lwz' -> 'lwzx';
    'ld' -> 'ldx'
  end.

ldop_word() ->
  case get(hipe_target_arch) of
    powerpc -> 'lwz';
    ppc64 -> 'ld'
  end.

ldop_wordx() ->
  case get(hipe_target_arch) of
    powerpc -> 'lwzx';
    ppc64 -> 'ldx'
  end.

mk_scratch(Scratch) ->
  case Scratch of
    0 -> mk_temp(0, 'untagged');
    'new' -> mk_new_temp('untagged')
  end.

mk_mfspr(Dst, Spr) -> #mfspr{dst=Dst, spr=Spr}.

mk_mtcr(Src) -> #mtcr{src=Src}.

mk_mtspr(Spr, Src) -> #mtspr{spr=Spr, src=Src}.

mk_pseudo_bc(BCond, TrueLab, FalseLab, Pred) ->
  if Pred >= 0.5 ->
      mk_pseudo_bc_simple(negate_bcond(BCond), FalseLab,
			  TrueLab, 1.0-Pred);
     true ->
      mk_pseudo_bc_simple(BCond, TrueLab, FalseLab, Pred)
  end.

mk_pseudo_bc_simple(BCond, TrueLab, FalseLab, Pred) when Pred =< 0.5 ->
  #pseudo_bc{bcond=BCond, true_label=TrueLab,
	     false_label=FalseLab, pred=Pred}.

negate_bcond(BCond) ->
  case BCond of
    'lt' -> 'ge';
    'ge' -> 'lt';
    'gt' -> 'le';
    'le' -> 'gt';
    'eq' -> 'ne';
    'ne' -> 'eq';
    'so' -> 'ns';
    'ns' -> 'so'
  end.

mk_pseudo_call(FunC, SDesc, ContLab, Linkage) ->
  #pseudo_call{func=FunC, sdesc=SDesc, contlab=ContLab, linkage=Linkage}.
pseudo_call_func(#pseudo_call{func=FunC}) -> FunC.
pseudo_call_sdesc(#pseudo_call{sdesc=SDesc}) -> SDesc.
pseudo_call_contlab(#pseudo_call{contlab=ContLab}) -> ContLab.
pseudo_call_linkage(#pseudo_call{linkage=Linkage}) -> Linkage.

mk_pseudo_call_prepare(NrStkArgs) ->
  #pseudo_call_prepare{nrstkargs=NrStkArgs}.
pseudo_call_prepare_nrstkargs(#pseudo_call_prepare{nrstkargs=NrStkArgs}) ->
  NrStkArgs.

mk_pseudo_li(Dst, Imm) -> #pseudo_li{dst=Dst, imm=Imm}.

mk_pseudo_move(Dst, Src) -> #pseudo_move{dst=Dst, src=Src}.
is_pseudo_move(I) -> case I of #pseudo_move{} -> true; _ -> false end.
pseudo_move_dst(#pseudo_move{dst=Dst}) -> Dst.
pseudo_move_src(#pseudo_move{src=Src}) -> Src.

mk_pseudo_spill_move(Dst, Temp, Src) ->
  #pseudo_spill_move{dst=Dst, temp=Temp, src=Src}.
is_pseudo_spill_move(I) -> is_record(I, pseudo_spill_move).

mk_pseudo_tailcall(FunC, Arity, StkArgs, Linkage) ->
  #pseudo_tailcall{func=FunC, arity=Arity, stkargs=StkArgs, linkage=Linkage}.
pseudo_tailcall_func(#pseudo_tailcall{func=FunC}) -> FunC.
pseudo_tailcall_stkargs(#pseudo_tailcall{stkargs=StkArgs}) -> StkArgs.
pseudo_tailcall_linkage(#pseudo_tailcall{linkage=Linkage}) -> Linkage.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.

mk_store(STop, Src, Disp, Base) ->
  #store{stop=STop, src=Src, disp=Disp, base=Base}.

mk_storex(StxOp, Src, Base1, Base2) ->
  #storex{stxop=StxOp, src=Src, base1=Base1, base2=Base2}.

mk_store(StOp, Src, Offset, Base, Scratch, Rest)when is_integer(Offset) ->
  RequireAlignment =
    case StOp of
      'std' -> true;
      'stdx' -> true;
      _ -> false
    end,
  if Offset >= -32768, Offset =< 32767,
     not RequireAlignment orelse Offset band 3 =:= 0 ->
      [mk_store(StOp, Src, Offset, Base) | Rest];
     true ->
      StxOp = stop_to_stxop(StOp),
      Index = mk_scratch(Scratch),
      mk_li(Index, Offset,
	    [mk_storex(StxOp, Src, Base, Index) | Rest])
  end.

stop_to_stxop(StOp) ->
  case StOp of
    'stb' -> 'stbx';
    'sth' -> 'sthx';
    'stw' -> 'stwx';
    'std' -> 'stdx'
  end.

stop_word() ->
  case get(hipe_target_arch) of
    powerpc -> 'stw';
    ppc64 -> 'std'
  end.

stop_wordx() ->
  case get(hipe_target_arch) of
    powerpc -> 'stwx';
    ppc64 -> 'stdx'
  end.

mk_unary(UnOp, Dst, Src) -> #unary{unop=UnOp, dst=Dst, src=Src}.

mk_lfd(Dst, Disp, Base) -> #lfd{dst=Dst, disp=Disp, base=Base}.
mk_lfdx(Dst, Base1, Base2) -> #lfdx{dst=Dst, base1=Base1, base2=Base2}.
mk_fload(Dst, Offset, Base, Scratch) when is_integer(Offset) ->
  if Offset >= -32768, Offset =< 32767 ->
      [mk_lfd(Dst, Offset, Base)];
     true ->
      Index = mk_scratch(Scratch),
      mk_li(Index, Offset, [mk_lfdx(Dst, Base, Index)])
  end.

mk_stfd(Src, Disp, Base) -> #stfd{src=Src, disp=Disp, base=Base}.
mk_stfdx(Src, Base1, Base2) -> #stfdx{src=Src, base1=Base1, base2=Base2}.
mk_fstore(Src, Offset, Base, Scratch) when is_integer(Offset) ->
  if Offset >= -32768, Offset =< 32767 ->
      [mk_stfd(Src, Offset, Base)];
     true ->
      Index = mk_scratch(Scratch),
      mk_li(Index, Offset, [mk_stfdx(Src, Base, Index)])
  end.

mk_fp_binary(FpBinOp, Dst, Src1, Src2) ->
  #fp_binary{fp_binop=FpBinOp, dst=Dst, src1=Src1, src2=Src2}.

mk_fp_unary(FpUnOp, Dst, Src) -> #fp_unary{fp_unop=FpUnOp, dst=Dst, src=Src}.

mk_pseudo_fmove(Dst, Src) -> #pseudo_fmove{dst=Dst, src=Src}.
is_pseudo_fmove(I) -> case I of #pseudo_fmove{} -> true; _ -> false end.
pseudo_fmove_dst(#pseudo_fmove{dst=Dst}) -> Dst.
pseudo_fmove_src(#pseudo_fmove{src=Src}) -> Src.

mk_pseudo_spill_fmove(Dst, Temp, Src) ->
  #pseudo_spill_fmove{dst=Dst, temp=Temp, src=Src}.
is_pseudo_spill_fmove(I) -> is_record(I, pseudo_spill_fmove).

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
