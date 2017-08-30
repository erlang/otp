%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% x86 pretty-printer

-ifdef(HIPE_AMD64).
-define(HIPE_X86_PP,        hipe_amd64_pp).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-else.
-define(HIPE_X86_PP,        hipe_x86_pp).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-endif.

-module(?HIPE_X86_PP).
-export([% pp/1, pp/2,
         pp_insn/1, optional_pp/3]).
-include("../x86/hipe_x86.hrl").

optional_pp(Defun, MFA, Options) ->
  case proplists:get_value(pp_native, Options) of
    true -> 
      pp(Defun);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA, Lst) of
	true -> pp(Defun);
	false -> ok
      end;
    {only,MFA} -> 
      pp(Defun);
    {file,FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      pp(File, Defun),
      ok = file:close(File);
    _ ->
      ok
  end.

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
  hipe_data_pp:pp(Dev, Data, x86, Fname),
  io:format(Dev, "\n", []).

pp_insns(Dev, [I|Is], Fname) ->
  pp_insn(Dev, I, Fname),
  pp_insns(Dev, Is, Fname);
pp_insns(_, [], _) ->
  ok.

pp_insn(I) ->
  pp_insn(standard_io, I, "").

pp_insn(Dev, I, Pre) ->
  case I of
    #alu{aluop=AluOp, src=Src, dst=Dst} ->
      io:format(Dev, "\t~s ", [alu_op_name(AluOp)]),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #call{'fun'=Fun, sdesc=SDesc, linkage=Linkage} ->
      io:format(Dev, "\tcall ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " #", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #cmovcc{cc=Cc, src=Src, dst=Dst} ->
      io:format(Dev, "\tcmov~s ", [cc_name(Cc)]),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #cmp{src=Src, dst=Dst} ->
      io:format(Dev, "\tcmp ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #comment{term=Term} ->
      io:format(Dev, "\t# ~p\n", [Term]);
    #imul{imm_opt=ImmOpt, src=Src, temp=Temp} ->
      io:format(Dev, "\timul ", []),
      case ImmOpt of
	[] -> ok;
	Imm ->
	  pp_imm(Dev, Imm, true),
	  io:format(Dev, ", ", [])
      end,
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Temp),
      io:format(Dev, "\n", []);
    #jcc{cc=Cc, label=Label} ->
      io:format(Dev, "\tj~s .~s_~w\n", [cc_name(Cc), Pre, Label]);
    #jmp_fun{'fun'=Fun, linkage=Linkage} ->
      io:format(Dev, "\tjmp ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " ~w\n", [Linkage]);
    #jmp_label{label=Label} ->
      io:format(Dev, "\tjmp .~s_~w\n", [Pre, Label]);
    #jmp_switch{temp=Temp, jtab=JTab, labels=Labels} ->
      io:format(Dev, "\tjmp *{constant,~w}(,", [JTab]),
      pp_temp(Dev, Temp),
      io:format(Dev, ",4) #", []),
      pp_labels(Dev, Labels, Pre),
      io:format(Dev, "\n", []);
    #label{label=Label} ->
      io:format(Dev, ".~s_~w:~n", [Pre, Label]);
    #lea{mem=Mem, temp=Temp} ->
      io:format(Dev, "\tlea ", []),
      pp_mem(Dev, Mem),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Temp),
      io:format(Dev, "\n", []);
    #move{src=Src, dst=Dst} ->
      io:format(Dev, "\tmov ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #move64{} ->
      pp_move64(Dev, I);
    #movsx{src=Src, dst=Dst} ->
      io:format(Dev, "\tmovsx ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #movzx{src=Src, dst=Dst} ->
      io:format(Dev, "\tmovzx ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #pseudo_call{'fun'=Fun, sdesc=SDesc, contlab=ContLab, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_call ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " # contlab .~s_~w", [Pre, ContLab]),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #pseudo_jcc{cc=Cc, true_label=TrueLab, false_label=FalseLab, pred=Pred} ->
      io:format(Dev, "\tpseudo_j~s ", [cc_name(Cc)]),
      io:format(Dev, ".~s_~w # .~s_~w ~.2f\n",
		[Pre, TrueLab, Pre, FalseLab, Pred]);
    #pseudo_tailcall{'fun'=Fun, arity=Arity, stkargs=StkArgs, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_tailcall ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " ~w (", [Arity]),
      pp_args(Dev, StkArgs),
      io:format(Dev, ") ~w\n", [Linkage]);
    #pseudo_tailcall_prepare{} ->
      io:format(Dev, "\tpseudo_tailcall_prepare\n", []);
    #push{src=Src} ->
      io:format(Dev, "\tpush ", []),
      pp_src(Dev, Src),
      io:format(Dev, "\n", []);
    #ret{npop=NPop} ->
      io:format(Dev, "\tret $~s\n", [to_hex(NPop)]);
    #shift{shiftop=ShiftOp, src=Src, dst=Dst} ->
      io:format(Dev, "\t~s ", [alu_op_name(ShiftOp)]),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #test{src=Src, dst=Dst} ->
      io:format(Dev, "\ttest ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #fp_binop{src=Src, dst=Dst, op=Op} ->
      io:format(Dev, "\t~s ", [Op]),
      pp_dst(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_src(Dev, Src),
      io:format(Dev, "\n", []);
    #fp_unop{arg=Arg, op=Op} ->
      io:format(Dev, "\t~s ", [Op]),
      case Arg of
	[]->
	  io:format(Dev, "\n", []);
	_ ->
	  pp_args(Dev, [Arg]),
	  io:format(Dev, "\n", [])
      end;
    #fmove{src=Src, dst=Dst} ->
      io:format(Dev, "\tfmove ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    _ ->
      exit({?MODULE, pp_insn, {"unknown x86 instruction", I}})
  end.

-ifdef(HIPE_AMD64).
pp_move64(Dev, I) ->
  #move64{imm=Src, dst=Dst} = I,
  io:format(Dev, "\tmov64 ", []),
  pp_src(Dev, Src),
  io:format(Dev, ", ", []),
  pp_dst(Dev, Dst),
  io:format(Dev, "\n", []).
-else.
pp_move64(_Dev, I) -> exit({?MODULE, I}).
-endif.

to_hex(N) ->
  io_lib:format("~.16x", [N, "0x"]).

pp_sdesc(Dev, Pre, #x86_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live}) ->
  pp_sdesc_exnlab(Dev, Pre, ExnLab),
  io:format(Dev, " ~s ~w [", [to_hex(FSize), Arity]),
  pp_sdesc_live(Dev, Live),
  io:format(Dev, "]", []).

pp_sdesc_exnlab(Dev, _, []) -> io:format(Dev, " []", []);
pp_sdesc_exnlab(Dev, Pre, ExnLab) -> io:format(Dev, " .~s_~w", [Pre, ExnLab]).

pp_sdesc_live(_, {}) -> ok;
pp_sdesc_live(Dev, Live) -> pp_sdesc_live(Dev, Live, 1).

pp_sdesc_live(Dev, Live, I) ->
  io:format(Dev, "~s", [to_hex(element(I, Live))]),
  if I < tuple_size(Live) ->
      io:format(Dev, ",", []),
      pp_sdesc_live(Dev, Live, I+1);
     true -> ok
  end.

pp_labels(Dev, [Label|Labels], Pre) ->
  io:format(Dev, " .~s_~w", [Pre, Label]),
  pp_labels(Dev, Labels, Pre);
pp_labels(_, [], _) ->
  ok.

pp_fun(Dev, Fun) ->
  case Fun of
    #x86_mfa{m=M, f=F, a=A} ->
      io:format(Dev, "~w:~w/~w", [M, F, A]);
    #x86_prim{prim=Prim} ->
      io:format(Dev, "~w", [Prim]);
    _ ->	% temp or mem
      io:format(Dev, "*", []),
      pp_dst(Dev, Fun)
  end.

alu_op_name(Op) -> Op.

cc_name(Cc) -> Cc.

pp_hard_reg(Dev, Reg) ->
  io:format(Dev, "~s", [?HIPE_X86_REGISTERS:reg_name(Reg)]).

type_tag('tagged') -> "t";
type_tag('untagged') -> "u";
type_tag('double') -> "d".

pp_temp(Dev, #x86_temp{reg=Reg, type=Type}) ->
  case Type of
    double ->
      Tag = type_tag(Type),
      io:format(Dev, "~s~w", [Tag, Reg]);
    _ ->
      case ?HIPE_X86_REGISTERS:is_precoloured(Reg) of
	true ->
	  pp_hard_reg(Dev, Reg);
	false ->
	  Tag = type_tag(Type),
	  io:format(Dev, "~s~w", [Tag, Reg])
      end
  end.

pp_fpreg(Dev, #x86_fpreg{reg=Reg, pseudo=Pseudo})->
  case Pseudo of
    true -> io:format(Dev, "pseudo_fp(~w)", [Reg]);
    _ -> io:format(Dev, "st(~w)", [Reg])
  end.

pp_imm(Dev, #x86_imm{value=Value}, Dollar) ->
  if Dollar =:= true -> io:format(Dev, [$$], []);
     true -> ok
  end,
  if is_integer(Value) -> io:format(Dev, "~s", [to_hex(Value)]);
     true -> io:format(Dev, "~w", [Value])
  end.

pp_mem(Dev, #x86_mem{base=Base, off=Off}) ->
  pp_off(Dev, Off),
  case Base of
    [] ->
      ok;
    _ ->
      io:format(Dev, "(", []),
      pp_temp(Dev, Base),
      io:format(Dev, ")", [])
  end.

pp_off(Dev, Off) ->
  pp_src(Dev, Off, false).

pp_src(Dev, Src) ->
  pp_src(Dev, Src, true).

pp_src(Dev, Src, Dollar) ->
  case Src of
    #x86_temp{} ->
      pp_temp(Dev, Src);
    #x86_imm{} ->
      pp_imm(Dev, Src, Dollar);
    #x86_mem{} ->
      pp_mem(Dev, Src);
    #x86_fpreg{} ->
      pp_fpreg(Dev, Src)
  end.

pp_dst(Dev, Dst) ->
  pp_src(Dev, Dst).

pp_args(Dev, [A|As]) ->
  pp_src(Dev, A),
  pp_comma_args(Dev, As);
pp_args(_, []) ->
  ok.

pp_comma_args(Dev, [A|As]) ->
  io:format(Dev, ", ", []),
  pp_src(Dev, A),
  pp_comma_args(Dev, As);
pp_comma_args(_, []) ->
  ok.
