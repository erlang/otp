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

-module(hipe_sparc_pp).
-export([pp/1, pp/2, pp_insn/1]).
-include("hipe_sparc.hrl").

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
  hipe_data_pp:pp(Dev, Data, sparc, Fname),
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
      case aluop_is_ldop(AluOp) of
	true ->
	  io:format(Dev, "[", []),
	  pp_temp(Dev, Src1),
	  io:format(Dev, " + ", []),
	  pp_src(Dev, Src2),
	  io:format(Dev, "]", []);
	false ->
	  pp_temp(Dev, Src1),
	  io:format(Dev, ", ", []),
	  pp_src(Dev, Src2)
      end,
      io:format(Dev, ", ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #bp{'cond'=Cond, label=Label, pred=Pred} ->
      io:format(Dev, "\tb~w,~w .~s_~w\n",
		[cond_name(Cond), pred_name(Pred), Pre, Label]);
    %% #br{} -> pp_br(Dev, I, Pre);
    #call_rec{'fun'=Fun, sdesc=SDesc, linkage=Linkage} ->
      io:format(Dev, "\tcall ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " #", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #call_tail{'fun'=Fun, linkage=Linkage} ->
      io:format(Dev, "\tb ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " # ~w\n", [Linkage]);
    #comment{term=Term} ->
      io:format(Dev, "\t# ~p\n", [Term]);
    #jmp{src1=Src1, src2=Src2, labels=Labels} ->
      io:format(Dev, "\tjmp [", []),
      pp_temp(Dev, Src1),
      io:format(Dev, " + ", []),
      pp_src(Dev, Src2),
      io:format(Dev, "]", []),
      case Labels of
	[] -> [];
	_ ->
	  io:format(Dev, " #", []),
	  pp_labels(Dev, Labels, Pre)
      end,
      io:format(Dev, "\n", []);
    #jmpl{src=Src, sdesc=SDesc} ->
      io:format(Dev, "\tjmpl [", []),
      pp_temp(Dev, Src),
      io:format(Dev, " + 0], %o7 # ", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, "\n", []);
    #label{label=Label} ->
      io:format(Dev, ".~s_~w:~n", [Pre, Label]);
    #pseudo_bp{'cond'=Cond, true_label=TrueLab, false_label=FalseLab, pred=Pred} ->
      io:format(Dev, "\tpseudo_b~w,~w .~s_~w # .~s_~w\n",
		[cond_name(Cond), pred_name(Pred), Pre, TrueLab, Pre, FalseLab]);
    %% #pseudo_br{} -> pp_pseudo_br(Dev, I, Pre);
    #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_call ", []),
      pp_funv(Dev, FunV),
      io:format(Dev, " # contlab .~s_~w", [Pre, ContLab]),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #pseudo_call_prepare{nrstkargs=NrStkArgs} ->
      SP = hipe_sparc_registers:reg_name_gpr(hipe_sparc_registers:stack_pointer()),
      io:format(Dev, "\tsub ~s, ~w, ~s # pseudo_call_prepare\n",
		[SP, 4*NrStkArgs, SP]);
    #pseudo_move{src=Src, dst=Dst} ->
      io:format(Dev, "\tpseudo_move ", []),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #pseudo_ret{} ->
      io:format(Dev, "\tpseudo_ret\n", []);
    #pseudo_set{imm=Imm, dst=Dst} ->
      io:format(Dev, "\tpseudo_set ", []),
      pp_imm(Dev, Imm),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_tailcall ", []),
      pp_funv(Dev, FunV),
      io:format(Dev, "/~w (", [Arity]),
      pp_args(Dev, StkArgs),
      io:format(Dev, ") ~w\n", [Linkage]);
    #pseudo_tailcall_prepare{} ->
      io:format(Dev, "\tpseudo_tailcall_prepare\n", []);
    #rdy{dst=Dst} ->
      io:format(Dev, "\trd %y, ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #sethi{dst=Dst, uimm22=#sparc_uimm22{value=Value}} ->
      io:format(Dev, "\tsethi ", []),
      pp_hex(Dev, Value),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #store{stop=StOp, src=Src, base=Base, disp=Disp} ->
      io:format(Dev, "\t~s ", [stop_name(StOp)]),
      pp_temp(Dev, Src),
      io:format(Dev, ", [", []),
      pp_temp(Dev, Base),
      io:format(Dev, " + ", []),
      pp_src(Dev, Disp),
      io:format(Dev, "]\n", []);
    #fp_binary{fp_binop=FpBinOp, src1=Src1, src2=Src2, dst=Dst} ->
      io:format(Dev, "\t~s ", [FpBinOp]),
      pp_temp(Dev, Src1),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src2),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #fp_unary{fp_unop=FpUnOp, src=Src, dst=Dst} ->
      io:format(Dev, "\t~s ", [FpUnOp]),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #pseudo_fload{base=Base, disp=Disp, dst=Dst, is_single=IsSingle} ->
      io:format(Dev, "\t~s [",
		[case IsSingle of
		   true -> 'ldf';
		   _ -> 'pseudo_fload' end]),
      pp_temp(Dev, Base),
      io:format(Dev, " + ", []),
      pp_simm13(Dev, Disp),
      io:format(Dev, "], ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #pseudo_fmove{src=Src, dst=Dst} ->
      io:format(Dev, "\tpseudo_fmove ", []),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, "\n", []);
    #pseudo_fstore{src=Src, base=Base, disp=Disp} ->
      io:format(Dev, "\tpseudo_fstore ", []),
      pp_temp(Dev, Src),
      io:format(Dev, ", [", []),
      pp_temp(Dev, Base),
      io:format(Dev, " + ", []),
      pp_simm13(Dev, Disp),
      io:format(Dev, "]\n", []);
    _ ->
      exit({?MODULE, pp_insn, I})
  end.

-ifdef(notdef).	% XXX: only for sparc64, alas
pp_br(Dev, I, Pre) ->
  #br{rcond=RCond, src=Src, label=Label, pred=Pred} = I,
  io:format(Dev, "\tbr~w,~w ", [rcond_name(RCond), pred_name(Pred)]),
  pp_temp(Dev, Src),
  io:format(Dev, ", .~s_~w\n", [Pre, Label]).

pp_pseudo_br(Dev, I, Pre) ->
  #pseudo_br{rcond=RCond, src=Src, true_label=TrueLab, false_label=FalseLab, pred=Pred} = I,
  io:format(Dev, "\tpseudo_br~w,~w ", [rcond_name(RCond), pred_name(Pred)]),
  pp_src(Dev, Src),
  io:format(Dev, ", .~s_~w # .~s_~w\n", [Pre, TrueLab, Pre, FalseLab]).
-endif.

to_hex(N) ->
  io_lib:format("~.16x", [N, "0x"]).

pp_sdesc(Dev, Pre, #sparc_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live}) ->
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
    #sparc_mfa{m=M, f=F, a=A} ->
      io:format(Dev, "~w:~w/~w", [M, F, A]);
    #sparc_prim{prim=Prim} ->
      io:format(Dev, "~w", [Prim])
  end.

pp_funv(Dev, FunV) ->
  case FunV of
    #sparc_temp{} ->
      pp_temp(Dev, FunV);
    Fun ->
      pp_fun(Dev, Fun)
  end.

alu_op_name(Op) -> Op.

aluop_is_ldop(AluOp) ->
  case AluOp of
    'ldsb' -> true;
    'ldsh' -> true;
    'ldsw' -> true;
    'ldub' -> true;
    'lduh' -> true;
    'lduw' -> true;
    'ldx' -> true;
    _ -> false
  end.

cond_name(Cond) -> Cond.
%%rcond_name(RCond) -> RCond.

pred_name(Pred) ->
  if Pred >= 0.5 -> 'pt';
     true -> 'pn'
  end.

stop_name(StOp) -> StOp.

pp_temp(Dev, Temp=#sparc_temp{reg=Reg, type=Type}) ->
  case hipe_sparc:temp_is_precoloured(Temp) of
    true ->
      Name =
	case Type of
	  double -> hipe_sparc_registers:reg_name_fpr(Reg);
	  _ -> hipe_sparc_registers:reg_name_gpr(Reg)
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
pp_simm13(Dev, #sparc_simm13{value=Value}) -> pp_hex(Dev, Value).
pp_uimm5(Dev, #sparc_uimm5{value=Value}) -> pp_hex(Dev, Value).

pp_imm(Dev, Value) ->
  if is_integer(Value) -> pp_hex(Dev, Value);
     true -> io:format(Dev, "~w", [Value])
  end.

pp_src(Dev, Src) ->
  case Src of
    #sparc_temp{} ->
      pp_temp(Dev, Src);
    #sparc_simm13{} ->
      pp_simm13(Dev, Src);
    #sparc_uimm5{} ->	% XXX: sparc64: uimm6
      pp_uimm5(Dev, Src)
  end.

pp_arg(Dev, Arg) ->
  case Arg of
    #sparc_temp{} ->
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
