%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_arm_pp).
-export([pp/1, pp/2, pp_insn/1]).

-include("hipe_arm.hrl").

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
  hipe_data_pp:pp(Dev, Data, arm, Fname),
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
    #alu{aluop=AluOp, s=S, dst=Dst, src=Src, am1=Am1} ->
      io:format(Dev, "\t~s~s ", [alu_op_name(AluOp), s_name(S)]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_am1(Dev, Am1),
      io:format(Dev, "\n", []);
    #b_fun{'fun'=Fun, linkage=Linkage} ->
      io:format(Dev, "\tb ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " # ~w\n", [Linkage]);
    #b_label{'cond'=Cond, label=Label} ->
      io:format(Dev, "\tb~s .~s_~w\n", [cond_name(Cond), Pre, Label]);
    #bl{'fun'=Fun, sdesc=SDesc, linkage=Linkage} ->
      io:format(Dev, "\tbl ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " #", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #blx{src=Src, sdesc=SDesc} ->
      io:format(Dev, "\tblx ", []),
      pp_temp(Dev, Src),
      io:format(Dev, " # ", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, "\n", []);
    #cmp{cmpop=CmpOp, src=Src, am1=Am1} ->
      io:format(Dev, "\t~s ", [cmp_op_name(CmpOp)]),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_am1(Dev, Am1),
      io:format(Dev, "\n", []);
    #comment{term=Term} ->
      io:format(Dev, "\t# ~p\n", [Term]);
    #label{label=Label} ->
      io:format(Dev, ".~s_~w:~n", [Pre, Label]);
    #load{ldop=LdOp, dst=Dst, am2=Am2} ->
      io:format(Dev, "\t~w ", [ldop_name(LdOp)]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_am2(Dev, Am2),
      io:format(Dev, "\n", []);
    #ldrsb{dst=Dst, am3=Am3} ->
      io:format(Dev, "\tldrsb ", []),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_am3(Dev, Am3),
      io:format(Dev, "\n", []);
    #move{movop=MovOp, s=S, dst=Dst, am1=Am1} ->
      io:format(Dev, "\t~s~s ", [mov_op_name(MovOp), s_name(S)]),
      pp_temp(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_am1(Dev, Am1),
      io:format(Dev, "\n", []);
    #pseudo_bc{'cond'=Cond, true_label=TrueLab, false_label=FalseLab, pred=Pred} ->
      io:format(Dev, "\tpseudo_bc ~w, .~s_~w # .~s_~w ~.2f\n",
		[cond_name(Cond), Pre, TrueLab, Pre, FalseLab, Pred]);
    #pseudo_blr{} ->
      io:format(Dev, "\tpseudo_blr\n", []);
    #pseudo_bx{src=Src} ->
      io:format(Dev, "\tpseudo_bx ", []),
      pp_temp(Dev, Src),
      io:format(Dev, "\n", []);
    #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_call ", []),
      pp_funv(Dev, FunV),
      io:format(Dev, " # contlab .~s_~w", [Pre, ContLab]),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #pseudo_call_prepare{nrstkargs=NrStkArgs} ->
      SP = hipe_arm_registers:reg_name_gpr(hipe_arm_registers:stack_pointer()),
      io:format(Dev, "\tsub ~s, ~s, ~w # pseudo_call_prepare\n",
		[SP, SP, (4*NrStkArgs)]);
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
    #pseudo_switch{jtab=JTab, index=Index, labels=Labels} ->
      io:format(Dev, "\tpseudo_switch ", []),
      pp_temp(Dev, JTab),
      io:format(Dev, "[", []),
      pp_temp(Dev, Index),
      io:format(Dev, "]", []),
      case Labels of
	[] -> [];
	_ ->
	  io:format(Dev, " #", []),
	  pp_labels(Dev, Labels, Pre)
      end,
      io:format(Dev, "\n", []);
    #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_tailcall ", []),
      pp_funv(Dev, FunV),
      io:format(Dev, "/~w (", [Arity]),
      pp_args(Dev, StkArgs),
      io:format(Dev, ") ~w\n", [Linkage]);
    #pseudo_tailcall_prepare{} ->
      io:format(Dev, "\tpseudo_tailcall_prepare\n", []);
    #smull{dstlo=DstLo, dsthi=DstHi, src1=Src1, src2=Src2} ->
      io:format(Dev, "\tsmull ", []),
      pp_temp(Dev, DstLo),
      io:format(Dev, ", ", []),
      pp_temp(Dev, DstHi),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src1),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Src2),
      io:format(Dev, "\n", []);
    #store{stop=StOp, src=Src, am2=Am2} ->
      io:format(Dev, "\tstr~s ", [stop_suffix(StOp)]),
      pp_temp(Dev, Src),
      io:format(Dev, ", ", []),
      pp_am2(Dev, Am2),
      io:format(Dev, "\n", []);
    _ ->
      exit({?MODULE, pp_insn, I})
  end.

to_hex(N) ->
  io_lib:format("~.16x", [N, "0x"]).

pp_sdesc(Dev, Pre, #arm_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live}) ->
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
    #arm_mfa{m=M, f=F, a=A} ->
      io:format(Dev, "~w:~w/~w", [M, F, A]);
    #arm_prim{prim=Prim} ->
      io:format(Dev, "~w", [Prim])
  end.

pp_funv(Dev, FunV) ->
  case FunV of
    #arm_temp{} ->
      pp_temp(Dev, FunV);
    Fun ->
      pp_fun(Dev, Fun)
  end.

alu_op_name(Op) -> Op.

cond_name(Cond) ->
  case Cond of
    'al' -> "";
    _ -> Cond
  end.

s_name(S) ->
  case S of
    true -> "s";
    false -> ""
  end.

cmp_op_name(Op) -> Op.

mov_op_name(Op) -> Op.

ldop_name(LdOp) -> LdOp.

stop_suffix(StOp) ->
  case StOp of
    'str' -> "";
    'strb' -> "b"
  end.

imm8m_decode(Value, 0) ->
  Value;
imm8m_decode(Value, Rot) ->
  (Value bsr (2 * Rot)) bor (Value bsl (2 * (16 - Rot))).

pp_temp(Dev, Temp=#arm_temp{reg=Reg, type=Type}) ->
  case hipe_arm:temp_is_precoloured(Temp) of
    true ->
      Name =
%%%	case Type of
%%%	  'double' ->
%%%	    hipe_arm_registers:reg_name_fpr(Reg);
%%%	  _ -> 
	    hipe_arm_registers:reg_name_gpr(Reg)
%%%	end
	      ,
      io:format(Dev, "~s", [Name]);
    false ->
      Tag =
	case Type of
%%%	  double -> "f";
	  tagged -> "t";
	  untagged -> "u"
	end,
      io:format(Dev, "~s~w", [Tag, Reg])
  end.

pp_hex(Dev, Value) -> io:format(Dev, "~s", [to_hex(Value)]).

pp_imm(Dev, Value) ->
  if is_integer(Value) -> pp_hex(Dev, Value);
     true -> io:format(Dev, "~w", [Value])
  end.

pp_am1(Dev, Am1) ->
  case Am1 of
    #arm_temp{} ->
      pp_temp(Dev, Am1);
    {Src,rrx} ->
      pp_temp(Dev, Src),
      io:format(Dev, ", rrx", []);
    {Src,ShiftOp,ShiftArg} ->
      pp_temp(Dev, Src),
      io:format(Dev, ", ~w ", [ShiftOp]),
      case ShiftArg of
	#arm_temp{} ->
	  pp_temp(Dev, ShiftArg);
	Imm5 ->
	  io:format(Dev, "#~w", [Imm5])
      end;
    {Imm8,Imm4} ->
      io:format(Dev, "#~s", [to_hex(imm8m_decode(Imm8, Imm4))])
  end.

pp_am2(Dev, #am2{src=Src,sign=Sign,offset=Am2Offset}) ->
  io:format(Dev, "[", []),
  pp_temp(Dev, Src),
  io:format(Dev, ",~s", [sign_name(Sign)]),
  case Am2Offset of
    #arm_temp{} ->
      pp_temp(Dev, Am2Offset);
    {Src2,rrx} ->
      pp_temp(Dev, Src2),
      io:format(Dev, ", rrx", []);
    {Src2,ShiftOp,Imm5} ->
      pp_temp(Dev, Src2),
      io:format(Dev, ",~w #~w", [ShiftOp,Imm5]);
    Imm12 ->
      io:format(Dev, "#~w", [Imm12])
  end,
  io:format(Dev, "]", []).

pp_am3(Dev, #am3{src=Src,sign=Sign,offset=Am3Offset}) ->
  io:format(Dev, "[", []),
  pp_temp(Dev, Src),
  io:format(Dev, ",~s", [sign_name(Sign)]),
  case Am3Offset of
    #arm_temp{} -> pp_temp(Dev, Am3Offset);
    Imm8 -> io:format(Dev, "~w", [Imm8])
  end,
  io:format(Dev, "]", []).

sign_name(Sign) ->
  case Sign of
    '+' -> "";
    '-' -> "-"
  end.

pp_arg(Dev, Arg) ->
  case Arg of
    #arm_temp{} ->
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
