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
%%% simple local x86 regalloc

-ifdef(HIPE_AMD64).
-define(HIPE_X86_RA_NAIVE, hipe_amd64_ra_naive).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_SPECIFIC_FP, hipe_amd64_specific_sse2).
-define(ECX, rcx).
-else.
-define(HIPE_X86_RA_NAIVE, hipe_x86_ra_naive).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-define(HIPE_X86_SPECIFIC_FP, hipe_x86_specific_x87).
-define(ECX, ecx).
-endif.

-module(?HIPE_X86_RA_NAIVE).
-export([ra/4]).

-include("../x86/hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true). % enable instrumentation
-include("../main/hipe.hrl").

ra(CFG0, Liveness, Coloring_fp, Options) ->
  CFG = hipe_x86_cfg:map_bbs(fun do_bb/2, CFG0),
  NofSpilledFloats = count_non_float_spills(Coloring_fp),
  NofFloats = length(Coloring_fp),
  ?add_spills(Options, hipe_gensym:get_var(x86) -
	      ?HIPE_X86_REGISTERS:first_virtual()-
	      NofSpilledFloats -
	      NofFloats),
  TempMap = [],
  {CFG, Liveness,
   TempMap}.

do_bb(_Lbl, BB) ->
  hipe_bb:code_update(BB, do_insns(hipe_bb:code(BB))).

count_non_float_spills(Coloring_fp) ->
  count_non_float_spills(Coloring_fp, 0).

count_non_float_spills([{_,To}|Tail], Num) ->
  case ?HIPE_X86_SPECIFIC_FP:is_precoloured(To, no_context) of
    true ->
      count_non_float_spills(Tail, Num);
    false ->
      count_non_float_spills(Tail, Num+1)
  end;
count_non_float_spills([], Num) ->
  Num.

do_insns([I|Insns]) ->
  do_insn(I) ++ do_insns(Insns);
do_insns([]) ->
  [].

do_insn(I) ->	% Insn -> Insn list
  case I of
    #alu{} ->
      do_alu(I);
    #cmp{} ->
      do_cmp(I);
    #imul{} ->
      do_imul(I);
    #jmp_switch{} ->
      do_jmp_switch(I);
    #lea{} ->
      do_lea(I);
    #move{} ->
      do_move(I);
    #move64{} ->
      do_move64(I);
    #movzx{} ->
      do_movx(I);
    #movsx{} ->
      do_movx(I);
    #fmove{} ->
      do_fmove(I);
    #fp_unop{} ->
      do_fp_unop(I);
    #fp_binop{} ->
      do_fp_binop(I);
    #shift{} ->
      do_shift(I);
    #test{} ->
      do_test(I);
    #label{} ->
      [I];
    #pseudo_jcc{} ->
      [I]; 
    #pseudo_call{} ->
      [I];
    #ret{} ->
      [I];
    #pseudo_tailcall_prepare{} ->
      [I];
    #pseudo_tailcall{} ->
      [I];
    #push{} ->
      [I];
    #jmp_label{} ->
      [I];
    #comment{} ->
      [I];
    _ ->
      io:format("Unknown Instruction = ~w\n", [I]),
      exit({?MODULE, unknown_instruction, I})
  end.

%%% Fix an alu op.

do_alu(I) ->
  #alu{src=Src0,dst=Dst0} = I,
  {FixSrc,Src,FixDst,Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#alu{src=Src,dst=Dst}].

%%% Fix a cmp op.

do_cmp(I) ->
  #cmp{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#cmp{src=Src,dst=Dst}].

%%% Fix an imul op.

do_imul(I) ->
  #imul{imm_opt=ImmOpt,src=Src0,temp=Temp0} = I,
  {FixSrc,Src} = fix_src_operand(Src0),	% may use temp0
  {FixTempSrc,Temp,FixTempDst} =
    case temp_is_pseudo(Temp0) of
      false ->
	{[], Temp0, []};
      true ->
	Reg = hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp1(), 'untagged'),
	{case ImmOpt of
	   [] -> [hipe_x86:mk_move(Temp0, Reg)];	% temp *= src
	   _ -> []					% temp = src * imm
	 end,
	 Reg,
	 [hipe_x86:mk_move(Reg, Temp0)]}
    end,
  FixSrc ++ FixTempSrc ++ [I#imul{src=Src,temp=Temp}] ++ FixTempDst.

%%% Fix a jmp_switch op.

-ifdef(HIPE_AMD64).
do_jmp_switch(I) ->
  #jmp_switch{temp=Temp, jtab=Tab} = I,
  case temp_is_pseudo(Temp) of
    false ->
      case temp_is_pseudo(Tab) of
	false ->
	  [I];
	true ->
	  Reg = hipe_x86:mk_temp(hipe_amd64_registers:temp0(), 'untagged'),
	  [hipe_x86:mk_move(Temp, Reg), I#jmp_switch{jtab=Reg}]
      end;
    true ->
      Reg = hipe_x86:mk_temp(hipe_amd64_registers:temp1(), 'untagged'),
      case temp_is_pseudo(Tab) of
	false ->
	  [hipe_x86:mk_move(Temp, Reg), I#jmp_switch{temp=Reg}];
	true ->
	  Reg2 = hipe_x86:mk_temp(hipe_amd64_registers:temp0(), 'untagged'),
	  [hipe_x86:mk_move(Temp, Reg),
	   hipe_x86:mk_move(Tab, Reg2),
	   I#jmp_switch{temp=Reg, jtab=Reg2}]
      end
  end.
-else.
do_jmp_switch(I) ->
  #jmp_switch{temp=Temp} = I,
  case temp_is_pseudo(Temp) of
    false ->
      [I];
    true ->
      Reg = hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp0(), 'untagged'),
      [hipe_x86:mk_move(Temp, Reg), I#jmp_switch{temp=Reg}]
  end.
-endif.

%%% Fix a lea op.

do_lea(I) ->
  #lea{temp=Temp} = I,
  case temp_is_pseudo(Temp) of
    false ->
      [I];
    true ->
      Reg = hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp0(), 'untagged'),
      [I#lea{temp=Reg}, hipe_x86:mk_move(Reg, Temp)]
  end.

%%% Fix a move op.

do_move(I) ->
  #move{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#move{src=Src,dst=Dst}].

-ifdef(HIPE_AMD64).
do_move64(I) ->
  #move64{dst=Dst} = I,
  case is_mem_opnd(Dst) of
    false ->
      [I];
    true ->     
      Reg = hipe_amd64_registers:temp1(),
      NewDst = clone(Dst, Reg),
      [I#move64{dst=NewDst}, hipe_x86:mk_move(NewDst, Dst)]
  end.
-else.
do_move64(I) -> exit({?MODULE, I}).
-endif.

do_movx(I) ->
  {{FixSrc, Src}, {FixDst, Dst}} =
    case I of
      #movsx{src=Src0,dst=Dst0} ->
	{fix_src_operand(Src0), fix_dst_operand(Dst0)};
      #movzx{src=Src0,dst=Dst0} ->
	{fix_src_operand(Src0), fix_dst_operand(Dst0)}
    end,
  Reg = ?HIPE_X86_REGISTERS:temp0(),
  Dst2 = clone(Dst, Reg),
  I2 = case is_mem_opnd(Dst) of
	 true ->
	   Reg = ?HIPE_X86_REGISTERS:temp0(),
	   Dst2 = clone(Dst, Reg),
	   case I of
		     #movsx{} ->
	       [hipe_x86:mk_movsx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)];
	     #movzx{} ->
	       [hipe_x86:mk_movzx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)]
	   end;
	 false ->
	   case I of
	     #movsx{} ->
	       [hipe_x86:mk_movsx(Src, Dst)];
	     #movzx{} ->
	       [hipe_x86:mk_movzx(Src, Dst)]
	   end
	 end,
  FixSrc ++ FixDst ++ I2.


%%% Fix a fmove op.
%% conv_to_float
do_fmove(I=#fmove{src=#x86_temp{type=untagged},
		  dst=#x86_temp{type=double}}) ->
  #fmove{src=Src0,dst=Dst0} = I,
  Src = clone(Src0, ?HIPE_X86_REGISTERS:temp0()),
  Dst = clone(Dst0, ?HIPE_X86_REGISTERS:temp1()),
  [hipe_x86:mk_move(Src0, Src),
   I#fmove{src=Src, dst=Dst},
   hipe_x86:mk_fmove(Dst, Dst0)];
%% fmove
do_fmove(I) ->
  #fmove{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#fmove{src=Src,dst=Dst}].

do_fp_unop(I) ->
  #fp_unop{arg=Arg} = I,
  case is_mem_opnd(Arg) of
    false ->
      [I];
    true ->
      Reg = ?HIPE_X86_REGISTERS:temp1(),
      NewArg = clone(Arg, Reg),
      [hipe_x86:mk_fmove(Arg, NewArg),
       I#fp_unop{arg=NewArg},
       hipe_x86:mk_fmove(NewArg, Arg)]
  end.

do_fp_binop(I) ->
  #fp_binop{src=Src0, dst=Dst0} = I,
  {FixSrc, Src} = fix_src_operand(Src0),
  {FixDst, Dst} = fix_dst_operand(Dst0),
  Reg = ?HIPE_X86_REGISTERS:temp1(),
  Dst2 = clone(Dst, Reg),
  FixSrc ++ FixDst ++ [hipe_x86:mk_fmove(Dst, Dst2),
		       I#fp_binop{src=Src, dst=Dst2},
		       hipe_x86:mk_fmove(Dst2, Dst)].

do_shift(I) ->
  #shift{src=Src0,dst=Dst0} = I,
  {FixDst, Dst} = fix_dst_operand(Dst0),
  Reg = ?HIPE_X86_REGISTERS:?ECX(),
  case Src0 of
    #x86_imm{} ->
      FixDst ++ [I#shift{dst=Dst}];
    #x86_temp{reg=Reg}  ->
      FixDst ++ [I#shift{dst=Dst}]
  end.

do_test(I) ->
  #test{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#test{src=Src,dst=Dst}].

%%% Fix the operands of a binary op.
%%% 1. remove pseudos from any explicit memory operands
%%% 2. if both operands are (implicit or explicit) memory operands,
%%%    move src to a reg and use reg as src in the original insn

do_binary(Src0, Dst0) ->
  {FixSrc, Src} = fix_src_operand(Src0),
  {FixDst, Dst} = fix_dst_operand(Dst0),
  {FixSrc3, Src3} =
    case is_mem_opnd(Src) of
      false ->
	{FixSrc, Src};
      true ->
	case is_mem_opnd(Dst) of
	  false ->
	    {FixSrc, Src};
	  true ->
	    Reg = ?HIPE_X86_REGISTERS:temp0(),
	    Src2 = clone(Src, Reg),
	    FixSrc2 = FixSrc ++ [mk_move(Src, Src2)],
	    {FixSrc2, Src2}
	end
	end,
  {FixSrc3, Src3, FixDst, Dst}.

%%% Fix any x86_mem operand to not refer to any pseudos.
%%% The fixup may use additional instructions and registers.
%%% 'src' operands may clobber '%temp0'.
%%% 'dst' operands may clobber '%temp1'.

fix_src_operand(Opnd) ->
  fix_mem_operand(Opnd, ?HIPE_X86_REGISTERS:temp0()).

fix_dst_operand(Opnd) ->
  fix_mem_operand(Opnd, ?HIPE_X86_REGISTERS:temp1()).

fix_mem_operand(Opnd, Reg) ->	% -> {[fixupcode], newop}
  case Opnd of
    #x86_mem{base=Base,off=Off} ->
      case is_mem_opnd(Base) of
	false ->
	  case src_is_pseudo(Off) of
	    false ->
	      {[], Opnd};
	    true ->		% pseudo(reg)
	      Temp = clone(Off, Reg),
	      {[hipe_x86:mk_move(Off, Temp)],
	       Opnd#x86_mem{off=Temp}}
	  end;
	true ->
	  Temp = clone(Base, Reg),
	  case src_is_pseudo(Off) of
	    false ->	% imm/reg(pseudo)
	      {[hipe_x86:mk_move(Base, Temp)],
	       Opnd#x86_mem{base=Temp}};
	    true ->		% pseudo1(pseudo0)
	      {[hipe_x86:mk_move(Base, Temp),
		hipe_x86:mk_alu('add', Off, Temp)],
	       Opnd#x86_mem{base=Temp, off=hipe_x86:mk_imm(0)}}
	  end
      end;
    _ ->
      {[], Opnd}
  end.

%%% Check if an operand denotes a memory cell (mem or pseudo).

is_mem_opnd(Opnd) ->
  case Opnd of
    #x86_mem{} -> true;
    #x86_temp{} -> temp_is_pseudo(Opnd);
    _ -> false
  end.

%%% Check if an operand is a pseudo-Temp.

src_is_pseudo(Src) ->
  case hipe_x86:is_temp(Src) of
    true -> temp_is_pseudo(Src);
    false -> false
  end.

temp_is_pseudo(Temp) ->
  not(?HIPE_X86_REGISTERS:is_precoloured(hipe_x86:temp_reg(Temp))).

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst, Reg) ->
  Type =
    case Dst of
      #x86_mem{} -> hipe_x86:mem_type(Dst);
      #x86_temp{} -> hipe_x86:temp_type(Dst)
    end,
  hipe_x86:mk_temp(Reg, Type).

mk_move(Src, Dst=#x86_temp{type=double}) ->
  hipe_x86:mk_fmove(Src, Dst);
mk_move(Src, Dst) ->
  hipe_x86:mk_move(Src, Dst).  
