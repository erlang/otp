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

-module(hipe_ppc_pp).
-export([pp/1, pp/2, pp_insn/1]).

-include("hipe_ppc.hrl").

pp(Defun) ->
  pp(standard_io, Defun).

pp(Dev, #defun{mfa={M,F,A}, code=Code, data=Data}) ->
  Fname = atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
  io:format(Dev, "\t.text\n", []),
  io:format(Dev, "\t.align 4\n", []),
  io:format(Dev, "\t.global ~s\n", [Fname]),
  io:format(Dev, "~s:\n", [Fname]),
  pp_insns(Dev, Code, Fname),
  io:format(Dev, "\t.rodata\n", []),
  io:format(Dev, "\t.align 4\n", []),
  hipe_data_pp:pp(Dev, Data, ppc, Fname),
  io:format(Dev, "\n", []).

pp_insns(Dev, [I|Is], Fname) ->
  pp_insn(Dev, I, Fname),
  pp_insns(Dev, Is, Fname);
pp_insns(_, [], _) ->
  [].

pp_insn(I) ->
  pp_insn(standard_io, I, "").

pp_insn(Dev, I, Pre) ->
  case I of
    #alu{aluop=AluOp, dst=Dst, src1=Src1, src2=Src2} ->
      io:format(Dev, "\t~s ", [alu_op_name(AluOp)]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src1),
      io:format(Dev, ", ", []),
      pp_src(Dev, Src2),
      io:format(Dev, "\n", []);
    #b_fun{'fun'=Fun, linkage=Linkage} ->
      io:format(Dev, "\tb ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " # ~w\n", [Linkage]);
    #b_label{label=Label} ->
      io:format(Dev, "\tb .~s_~w\n", [Pre, Label]);
    #bc{bcond=BCond, label=Label, pred=Pred} ->
      io:format(Dev, "\tb~w ~s_~w # ~.2f\n", [bcond_name(BCond), Pre, Label, Pred]);
    #bctr{labels=Labels} ->
      io:format(Dev, "\tbctr", []),
      case Labels of
	[] -> [];
	_ ->
	  io:format(Dev, " #", []),
	  pp_labels(Dev, Labels, Pre)
      end,
      io:format(Dev, "\n", []);
    #bctrl{sdesc=SDesc} ->
      io:format(Dev, "\tbctrl #", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, "\n", []);
    #bl{'fun'=Fun, sdesc=SDesc, linkage=Linkage} ->
      io:format(Dev, "\tbl ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " #", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #blr{} ->
      io:format(Dev, "\tblr\n", []);
    #comment{term=Term} ->
      io:format(Dev, "\t# ~p\n", [Term]);
    #cmp{cmpop=CmpOp, src1=Src1, src2=Src2} ->
      io:format(Dev, "\t~s ", [cmp_op_name(CmpOp)]),
      pp_temp(Dev, Src1),
      io:format(Dev, ", ", []),
      pp_src(Dev, Src2),
      io:format(Dev, "\n", []);
    #label{label=Label} ->
      io:format(Dev, ".~s_~w:~n", [Pre, Label]);
    #load{ldop=LdOp, dst=Dst, disp=Disp, base=Base} ->
      io:format(Dev, "\t~w ", [ldop_name(LdOp)]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ~s(", [to_hex(Disp)]),
      pp_temp(Dev, Base),
      io:format(Dev, ")\n", []);
    #loadx{ldxop=LdxOp, dst=Dst, base1=Base1, base2=Base2} ->
      io:format(Dev, "\t~w ", [ldxop_name(LdxOp)]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base1),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base2),
      io:format(Dev, "\n", []);
    #mfspr{dst=Dst, spr=SPR} ->
      io:format(Dev, "\tmf~w ", [spr_name(SPR)]),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #mtcr{src=Src} ->
      io:format(Dev, "\tmtcrf 0x80, ", []),
      pp_temp(Dev, Src),
      io:format(Dev, "\n", []);
    #mtspr{spr=SPR, src=Src} ->
      io:format(Dev, "\tmt~w ", [spr_name(SPR)]),
      pp_temp(Dev, Src),
      io:format(Dev, "\n", []);
    #pseudo_bc{bcond=BCond, true_label=TrueLab, false_label=FalseLab, pred=Pred} ->
      io:format(Dev, "\tpseudo_bc ~w, .~s_~w # .~s_~w ~.2f\n",
		[bcond_name(BCond), Pre, TrueLab, Pre, FalseLab, Pred]);
    #pseudo_call{func=FunC, sdesc=SDesc, contlab=ContLab, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_call ", []),
      pp_func(Dev, FunC),
      io:format(Dev, " # contlab .~s_~w", [Pre, ContLab]),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #pseudo_call_prepare{nrstkargs=NrStkArgs} ->
      SP = hipe_ppc_registers:reg_name_gpr(hipe_ppc_registers:stack_pointer()),
      io:format(Dev, "\taddi ~s, ~s, ~w # pseudo_call_prepare\n",
		[SP, SP, -(4*NrStkArgs)]);
    #pseudo_li{dst=Dst, imm=Imm} ->
      io:format(Dev, "\tpseudo_li ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_imm(Dev, Imm),
      io:format(Dev, "\n", []);
    #pseudo_move{dst=Dst, src=Src} ->
      io:format(Dev, "\tpseudo_move ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src),
      io:format(Dev, "\n", []);
    #pseudo_tailcall{func=FunC, arity=Arity, stkargs=StkArgs, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_tailcall ", []),
      pp_func(Dev, FunC),
      io:format(Dev, "/~w (", [Arity]),
      pp_args(Dev, StkArgs),
      io:format(Dev, ") ~w\n", [Linkage]);
    #pseudo_tailcall_prepare{} ->
      io:format(Dev, "\tpseudo_tailcall_prepare\n", []);
    #store{stop=StOp, src=Src, disp=Disp, base=Base} ->
      io:format(Dev, "\t~s ", [stop_name(StOp)]),
      pp_temp(Dev, Src),
      io:format(Dev, ", ~s(", [to_hex(Disp)]),
      pp_temp(Dev, Base),
      io:format(Dev, ")\n", []);
    #storex{stxop=StxOp, src=Src, base1=Base1, base2=Base2} ->
      io:format(Dev, "\t~s ", [stxop_name(StxOp)]),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base1),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base2),
      io:format(Dev, "\n", []);
    #unary{unop=UnOp, dst=Dst, src=Src} ->
      io:format(Dev, "\t~w ", [unop_name(UnOp)]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src),
      io:format(Dev, "\n", []);
    #lfd{dst=Dst, disp=Disp, base=Base} ->
      io:format(Dev, "\tlfd ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ~s(", [to_hex(Disp)]),
      pp_temp(Dev, Base),
      io:format(Dev, ")\n", []);
    #lfdx{dst=Dst, base1=Base1, base2=Base2} ->
      io:format(Dev, "\tlfdx ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base1),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base2),
      io:format(Dev, "\n", []);
    #stfd{src=Src, disp=Disp, base=Base} ->
      io:format(Dev, "\tstfd ", []),
      pp_temp(Dev, Src),
      io:format(Dev, ", ~s(", [to_hex(Disp)]),
      pp_temp(Dev, Base),
      io:format(Dev, ")\n", []);
    #stfdx{src=Src, base1=Base1, base2=Base2} ->
      io:format(Dev, "\tstfdx ", []),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base1),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Base2),
      io:format(Dev, "\n", []);
    #fp_binary{fp_binop=FpBinOp, dst=Dst, src1=Src1, src2=Src2} ->
      io:format(Dev, "\t~s ", [FpBinOp]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src1),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src2),
      io:format(Dev, "\n", []);
    #fp_unary{fp_unop=FpUnOp, dst=Dst, src=Src} ->
      io:format(Dev, "\t~s ", [FpUnOp]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src),
      io:format(Dev, "\n", []);
    #pseudo_fmove{dst=Dst, src=Src} ->
      io:format(Dev, "\tpseudo_fmove ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src),
      io:format(Dev, "\n", []);
    _ ->
      exit({?MODULE, pp_insn, I})
  end.

to_hex(N) ->
  io_lib:format("~.16x", [N, "0x"]).

pp_sdesc(Dev, Pre, #ppc_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live}) ->
  pp_sdesc_exnlab(Dev, Pre, ExnLab),
  io:format(Dev, " ~s ~w [", [to_hex(FSize), Arity]),
  pp_sdesc_live(Dev, Live),
  io:format(Dev, "]", []).

pp_sdesc_exnlab(Dev, _, []) -> io:format(Dev, " []", []);
pp_sdesc_exnlab(Dev, Pre, ExnLab) -> io:format(Dev, " .~s_~w", [Pre, ExnLab]).

pp_sdesc_live(_, {}) -> [];
pp_sdesc_live(Dev, Live) -> pp_sdesc_live(Dev, Live, 1).

pp_sdesc_live(Dev, Live, I) ->
  io:format(Dev, "~s", [to_hex(element(I, Live))]),
  if I < tuple_size(Live) ->
      io:format(Dev, ",", []),
      pp_sdesc_live(Dev, Live, I+1);
     true -> []
  end.

pp_labels(Dev, [Label|Labels], Pre) ->
  io:format(Dev, " .~s_~w", [Pre, Label]),
  pp_labels(Dev, Labels, Pre);
pp_labels(_, [], _) ->
  [].

pp_fun(Dev, Fun) ->
  case Fun of
    #ppc_mfa{m=M, f=F, a=A} ->
      io:format(Dev, "~w:~w/~w", [M, F, A]);
    #ppc_prim{prim=Prim} ->
      io:format(Dev, "~w", [Prim])
  end.

pp_func(Dev, FunC) ->
  case FunC of
    'ctr' ->
      io:format(Dev, "ctr", []);
    Fun ->
      pp_fun(Dev, Fun)
  end.

alu_op_name(Op) -> Op.

bcond_name(BCond) -> BCond.

cmp_op_name(Op) -> Op.

spr_name(SPR) -> SPR.

ldop_name(LdOp) -> LdOp.

ldxop_name(LdxOp) -> LdxOp.

stop_name(StOp) -> StOp.

stxop_name(StxOp) -> StxOp.

unop_name(UnOp) -> UnOp.

pp_temp(Dev, Temp=#ppc_temp{reg=Reg, type=Type}) ->
  case hipe_ppc:temp_is_precoloured(Temp) of
    true ->
      Name =
	case Type of
	  'double' -> hipe_ppc_registers:reg_name_fpr(Reg);
	  _ -> hipe_ppc_registers:reg_name_gpr(Reg)
	end,
      io:format(Dev, "~s", [Name]);
    false ->
      Tag =
	case Type of
	  double -> "f";
	  tagged -> "t";
	  untagged -> "u"
	end,
      io:format(Dev, "~s~w", [Tag, Reg])
  end.

pp_hex(Dev, Value) -> io:format(Dev, "~s", [to_hex(Value)]).
pp_simm16(Dev, #ppc_simm16{value=Value}) -> pp_hex(Dev, Value).
pp_uimm16(Dev, #ppc_uimm16{value=Value}) -> pp_hex(Dev, Value).

pp_imm(Dev, Value) ->
  if is_integer(Value) -> pp_hex(Dev, Value);
     true -> io:format(Dev, "~w", [Value])
  end.

pp_src(Dev, Src) ->
  case Src of
    #ppc_temp{} ->
      pp_temp(Dev, Src);
    #ppc_simm16{} ->
      pp_simm16(Dev, Src);
    #ppc_uimm16{} ->
      pp_uimm16(Dev, Src)
  end.

pp_arg(Dev, Arg) ->
  case Arg of
    #ppc_temp{} ->
      pp_temp(Dev, Arg);
    _ ->
      pp_hex(Dev, Arg)
  end.

pp_args(Dev, [A|As]) ->
  pp_arg(Dev, A),
  pp_comma_args(Dev, As);
pp_args(_, []) ->
  [].

pp_comma_args(Dev, [A|As]) ->
  io:format(Dev, ", ", []),
  pp_arg(Dev, A),
  pp_comma_args(Dev, As);
pp_comma_args(_, []) ->
  [].
