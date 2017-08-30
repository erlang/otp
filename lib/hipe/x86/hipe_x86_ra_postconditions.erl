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

-ifdef(HIPE_AMD64).
-define(HIPE_X86_RA_POSTCONDITIONS,	hipe_amd64_ra_postconditions).
-define(HIPE_X86_REGISTERS,		hipe_amd64_registers).
-define(HIPE_X86_SPECIFIC,		hipe_amd64_specific).
-define(ECX,				rcx).
-else.
-define(HIPE_X86_RA_POSTCONDITIONS,	hipe_x86_ra_postconditions).
-define(HIPE_X86_REGISTERS,		hipe_x86_registers).
-define(HIPE_X86_SPECIFIC,		hipe_x86_specific).
-define(ECX,				ecx).
-endif.

-module(?HIPE_X86_RA_POSTCONDITIONS).

-export([check_and_rewrite/3]).

-include("../x86/hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true).
-include("../main/hipe.hrl").
-define(count_temp(T), ?cons_counter(counter_mfa_mem_temps, T)).

check_and_rewrite(Defun, Coloring, Strategy) ->
  %% io:format("Converting\n"),
  TempMap = hipe_temp_map:cols2tuple(Coloring, ?HIPE_X86_SPECIFIC),
  %% io:format("Rewriting\n"),
  #defun{code=Code0} = Defun,
  {Code1, DidSpill} = do_insns(Code0, TempMap, Strategy, [], false),
  {Defun#defun{code=Code1,var_range={0,hipe_gensym:get_var(x86)}},
   DidSpill}.

do_insns([I|Insns], TempMap, Strategy, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap, Strategy),
  do_insns(Insns, TempMap, Strategy, lists:reverse(NewIs, Accum), DidSpill0 or DidSpill1);
do_insns([], _TempMap, _Strategy, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap, Strategy) ->	% Insn -> {Insn list, DidSpill}
  case I of
    #alu{} ->
      do_alu(I, TempMap, Strategy);
    #cmp{} ->
      do_cmp(I, TempMap, Strategy);
    #imul{} ->
      do_imul(I, TempMap, Strategy);
    #jmp_switch{} ->
      do_jmp_switch(I, TempMap, Strategy);
    #lea{} ->
      do_lea(I, TempMap, Strategy);
    #move{} ->
      do_move(I, TempMap, Strategy);
    #move64{} ->
      do_move64(I, TempMap, Strategy);
    #movsx{} ->
      do_movx(I, TempMap, Strategy);
    #movzx{} ->
      do_movx(I, TempMap, Strategy);
    #fmove{} ->
      do_fmove(I, TempMap, Strategy);
    #shift{} ->
      do_shift(I, TempMap, Strategy);
    _ ->
      %% comment, jmp*, label, pseudo_call, pseudo_jcc, pseudo_tailcall,
      %% pseudo_tailcall_prepare, push, ret
      {[I], false}
  end.

%%% Fix an alu op.

do_alu(I, TempMap, Strategy) ->
  #alu{src=Src0,dst=Dst0} = I,
  {FixSrc,Src,FixDst,Dst,DidSpill} =
    do_binary(Src0, Dst0, TempMap, Strategy),
  {FixSrc ++ FixDst ++ [I#alu{src=Src,dst=Dst}], DidSpill}.

%%% Fix a cmp op.

do_cmp(I, TempMap, Strategy) ->
  #cmp{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst, DidSpill} =
    do_binary(Src0, Dst0, TempMap, Strategy),
  {FixSrc ++ FixDst ++ [I#cmp{src=Src,dst=Dst}], DidSpill}.

%%% Fix an imul op.

do_imul(I, TempMap, Strategy) ->
  #imul{imm_opt=ImmOpt,src=Src0,temp=Temp0} = I,
  {FixSrc,Src,DidSpill1} = fix_src_operand(Src0, TempMap, Strategy),	% temp1
  {FixTempSrc,Temp,FixTempDst,DidSpill2} =
    case is_spilled(Temp0, TempMap) of
      false ->
	{[], Temp0, [], false};
      true ->
	Reg = spill_temp0('untagged', Strategy),
	{case ImmOpt of
	   [] -> [hipe_x86:mk_move(Temp0, Reg)];	% temp *= src
	   _ -> []					% temp = src * imm
	 end,
	 Reg,
	 [hipe_x86:mk_move(Reg, Temp0)],
	 true}
    end,
  {FixSrc ++ FixTempSrc ++ [I#imul{src=Src,temp=Temp}] ++ FixTempDst,
   DidSpill1 or DidSpill2}.

%%% Fix a jmp_switch op.

-ifdef(HIPE_AMD64).
do_jmp_switch(I, TempMap, Strategy) ->
  #jmp_switch{temp=Temp, jtab=Tab} = I,
  case is_spilled(Temp, TempMap) of
    false ->
      case is_spilled(Tab, TempMap) of
        false ->
          {[I], false};
        true ->
          NewTab = spill_temp('untagged', Strategy),
          {[hipe_x86:mk_move(Tab, NewTab), I#jmp_switch{jtab=Tab}],
	   true}
      end;
    true ->
      case is_spilled(Tab, TempMap) of
        false ->
          NewTmp = spill_temp('untagged', Strategy),
          {[hipe_x86:mk_move(Temp, NewTmp), I#jmp_switch{temp=NewTmp}],
	   true};
        true ->
          NewTmp = spill_temp('untagged', Strategy),
          NewTab = spill_temp0('untagged', Strategy),
          {[hipe_x86:mk_move(Temp, NewTmp),
            hipe_x86:mk_move(Tab, NewTab),
            I#jmp_switch{temp=NewTmp, jtab=NewTab}],
	   true}
      end
  end.
-else.	% not AMD64
do_jmp_switch(I, TempMap, Strategy) ->
  #jmp_switch{temp=Temp} = I,
  case is_spilled(Temp, TempMap) of
    false ->
      {[I], false};
    true ->
      NewTmp = spill_temp('untagged', Strategy),
      {[hipe_x86:mk_move(Temp, NewTmp), I#jmp_switch{temp=NewTmp}],
       true}
  end.
-endif.	% not AMD64

%%% Fix a lea op.

do_lea(I, TempMap, Strategy) ->
  #lea{temp=Temp} = I,
  case is_spilled(Temp, TempMap) of
    false ->
      {[I], false};
    true ->
      NewTmp = spill_temp('untagged', Strategy),
      {[I#lea{temp=NewTmp}, hipe_x86:mk_move(NewTmp, Temp)],
       true}
  end.

%%% Fix a move op.

do_move(I, TempMap, Strategy) ->
  #move{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst, DidSpill} =
    do_check_byte_move(Src0, Dst0, TempMap, Strategy),
  {FixSrc ++ FixDst ++ [I#move{src=Src,dst=Dst}],
   DidSpill}.

-ifdef(HIPE_AMD64).

%%% AMD64 has no issues with byte moves.
do_check_byte_move(Src0, Dst0, TempMap, Strategy) ->
  do_binary(Src0, Dst0, TempMap, Strategy).

-else.	% not AMD64

%%% x86 can only do byte moves to a subset of the integer registers.
do_check_byte_move(Src0, Dst0, TempMap, Strategy) ->
  case Dst0 of
    #x86_mem{type=byte} ->
      do_byte_move(Src0, Dst0, TempMap, Strategy);
    _ ->
      do_binary(Src0, Dst0, TempMap, Strategy)
  end.

do_byte_move(Src0, Dst0, TempMap, Strategy) ->
  {FixSrc, Src, DidSpill1} = fix_src_operand(Src0, TempMap, Strategy),
  {FixDst, Dst, DidSpill2} = fix_dst_operand(Dst0, TempMap, Strategy),
  Reg = hipe_x86_registers:eax(),
  {FixSrc3, Src3} = % XXX: this just checks Src, the result is known!
    case Src of
      #x86_imm{} ->
	{FixSrc, Src};
      #x86_temp{reg=Reg} ->	% small moves must start from reg 1->4
	{FixSrc, Src}		% so variable sources are always put in eax
    end,
  {FixSrc3, Src3, FixDst, Dst,
   DidSpill2 or DidSpill1}.

-endif.	% not AMD64

%%% Fix a move64 op.

do_move64(I, TempMap, Strategy) ->
  #move64{dst=Dst} = I,
  case is_spilled(Dst, TempMap) of
    false ->
      {[I], false};
    true ->
      Reg = clone(Dst, Strategy),
      {[I#move64{dst=Reg}, hipe_x86:mk_move(Reg, Dst)], true}
  end.

%%% Fix a movx op.

do_movx(I, TempMap, Strategy) ->
  {{FixSrc, Src, DidSpill1}, {FixDst, Dst, DidSpill2}} =
    case I of
      #movsx{src=Src0,dst=Dst0} ->
	{fix_src_operand(Src0, TempMap, Strategy),
	 fix_dst_operand(Dst0, TempMap, Strategy)};
      #movzx{src=Src0,dst=Dst0} ->
	{fix_src_operand(Src0, TempMap, Strategy),
	 fix_dst_operand(Dst0, TempMap, Strategy)}
    end,
  {I3, DidSpill3} =
    case is_spilled(Dst, TempMap) of
      false ->
	I2 = case I of
	       #movsx{} ->
		 [hipe_x86:mk_movsx(Src, Dst)];
	       #movzx{} ->
		 [hipe_x86:mk_movzx(Src, Dst)]
	     end,
	{I2, false};
      true ->
	Dst2 = clone(Dst, Strategy),
	I2 =
	  case I of
	    #movsx{} ->
	      [hipe_x86:mk_movsx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)];
	    #movzx{} ->
	      [hipe_x86:mk_movzx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)]
	  end,
	{I2, true}
    end,
  {FixSrc++FixDst++I3,
   DidSpill3 or DidSpill2 or DidSpill1}.

%%% Fix an fmove op.

do_fmove(I, TempMap, Strategy) ->
  #fmove{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, DidSpill1} = fix_src_operand(Src0, TempMap, Strategy),
  {FixDst, Dst, DidSpill2} = fix_dst_operand(Dst0, TempMap, Strategy),
  %% fmoves from memory position to memory position is handled
  %% by the f.p. register allocator.
  {FixSrc ++ FixDst ++ [I#fmove{src=Src,dst=Dst}],
   DidSpill1 or DidSpill2}.

%%% Fix a shift operation.
%%% 1. remove pseudos from any explicit memory operands
%%% 2. if the source is a register or memory position
%%%    make sure to move it to %ecx

do_shift(I, TempMap, Strategy) ->
  #shift{src=Src0,dst=Dst0} = I,
  {FixDst, Dst, DidSpill} = fix_dst_operand(Dst0, TempMap, Strategy),
  Reg = ?HIPE_X86_REGISTERS:?ECX(),
  case Src0 of
    #x86_imm{} ->
      {FixDst ++ [I#shift{dst=Dst}], DidSpill};
    #x86_temp{reg=Reg}  ->
      {FixDst ++ [I#shift{dst=Dst}], DidSpill}
  end.

%%% Fix the operands of a binary op.
%%% 1. remove pseudos from any explicit memory operands
%%% 2. if both operands are (implicit or explicit) memory operands,
%%%    move src to a reg and use reg as src in the original insn

do_binary(Src0, Dst0, TempMap, Strategy) ->
  {FixSrc, Src, DidSpill1} = fix_src_operand(Src0, TempMap, Strategy),
  {FixDst, Dst, DidSpill2} = fix_dst_operand(Dst0, TempMap, Strategy),
  {FixSrc3, Src3, DidSpill3} =
    case is_mem_opnd(Src, TempMap) of
      false ->
	{FixSrc, Src, false};
      true ->
	case is_mem_opnd(Dst, TempMap) of
	  false ->
	    {FixSrc, Src, false};
	  true ->
	    Src2 = clone(Src, Strategy),
	    FixSrc2 = FixSrc ++ [hipe_x86:mk_move(Src, Src2)],
	    {FixSrc2, Src2, true}
	end
    end,
  {FixSrc3, Src3, FixDst, Dst,
   DidSpill3 or DidSpill2 or DidSpill1}.

%%% Fix any x86_mem operand to not refer to any spilled temps.

fix_src_operand(Opnd, TmpMap, Strategy) ->
  fix_mem_operand(Opnd, TmpMap, temp1(Strategy)).

temp1('normal') -> [];
temp1('linearscan') -> ?HIPE_X86_REGISTERS:temp1().

fix_dst_operand(Opnd, TempMap, Strategy) ->
  fix_mem_operand(Opnd, TempMap, temp0(Strategy)).

temp0('normal') -> [];
temp0('linearscan') -> ?HIPE_X86_REGISTERS:temp0().

fix_mem_operand(Opnd, TempMap, RegOpt) ->	% -> {[fixupcode], newop, DidSpill}
  case Opnd of
    #x86_mem{base=Base,off=Off} ->
      case is_mem_opnd(Base, TempMap) of
	false ->
	  case is_mem_opnd(Off, TempMap) of
	    false ->
	      {[], Opnd, false};
	    true ->
	      Temp = clone2(Off, RegOpt),
	      {[hipe_x86:mk_move(Off, Temp)],
	       Opnd#x86_mem{off=Temp},
	       true}
	  end;
	true ->
	  Temp = clone2(Base, RegOpt),
	  case is_mem_opnd(Off, TempMap) of
	    false ->		% imm/reg(pseudo)
	      {[hipe_x86:mk_move(Base, Temp)],
	       Opnd#x86_mem{base=Temp},
	       true};
	    true ->		% pseudo(pseudo)
	      {[hipe_x86:mk_move(Base, Temp),
		hipe_x86:mk_alu('add', Off, Temp)],
	       Opnd#x86_mem{base=Temp, off=hipe_x86:mk_imm(0)},
	       true}
	  end
      end;
    _ ->
      {[], Opnd, false}
  end.

%%% Check if an operand denotes a memory cell (mem or pseudo).

is_mem_opnd(Opnd, TempMap) ->
  R =
    case Opnd of
      #x86_mem{} -> true;
      #x86_temp{} ->
	Reg = hipe_x86:temp_reg(Opnd),
	case hipe_x86:temp_is_allocatable(Opnd) of
	  true ->
	    case tuple_size(TempMap) > Reg of
	      true ->
		case
		  hipe_temp_map:is_spilled(Reg, TempMap) of
		  true ->
		    ?count_temp(Reg),
		    true;
		  false -> false
		end;
	      _ ->
		%% impossible, but was true in ls post and false in normal post
		exit({?MODULE,is_mem_opnd,Reg}),
		false
	    end;
	  false -> true
	end;
      _ -> false
    end,
  %% io:format("Op ~w mem: ~w\n",[Opnd,R]),
  R.

%%% Check if an operand is a spilled Temp.

is_spilled(Temp, TempMap) ->
  case hipe_x86:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_x86:temp_reg(Temp),
      case tuple_size(TempMap) > Reg of
	true ->
	  case hipe_temp_map:is_spilled(Reg, TempMap) of
	    true ->
	      ?count_temp(Reg),
	      true;
	    false ->
	      false
	  end;
	false ->
	  false
      end;
    false -> true
  end.

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst, Strategy) ->
  Type =
    case Dst of
      #x86_mem{} -> hipe_x86:mem_type(Dst);
      #x86_temp{} -> hipe_x86:temp_type(Dst)
    end,
  spill_temp(Type, Strategy).

spill_temp0(Type, 'normal') ->
  hipe_x86:mk_new_temp(Type);
spill_temp0(Type, 'linearscan') ->
  hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp0(), Type).

spill_temp(Type, 'normal') ->
  hipe_x86:mk_new_temp(Type);
spill_temp(Type, 'linearscan') ->
  hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp1(), Type).

%%% Make a certain reg into a clone of Dst

clone2(Dst, RegOpt) ->
  Type =
    case Dst of
      #x86_mem{} -> hipe_x86:mem_type(Dst);
      #x86_temp{} -> hipe_x86:temp_type(Dst)
    end,
  case RegOpt of
    [] -> hipe_x86:mk_new_temp(Type);
    Reg -> hipe_x86:mk_temp(Reg, Type)
  end.
