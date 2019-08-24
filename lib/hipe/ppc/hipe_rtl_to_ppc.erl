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
%%% The PowerPC instruction set is quite irregular.
%%% The following quirks must be handled by the translation:
%%%
%%% - The instruction names are different for reg/reg and reg/imm
%%%   source operands. For some operations, completely different
%%%   instructions handle the reg/reg and reg/imm cases.
%%% - The name of an arithmetic instruction depends on whether any
%%%   condition codes are to be set or not. Overflow is treated
%%%   separately from other conditions.
%%% - Some combinations or RTL ALU operations, source operand shapes,
%%%   and requested conditions have no direct correspondence in the
%%%   PowerPC instruction set.
%%% - The tagging of immediate operands as simm16 or uimm16 depends
%%%   on the actual instruction.
%%% - Conditional branches have no unsigned conditions. Instead there
%%%   are signed and unsigned versions of the compare instruction.
%%% - The arithmetic overflow flag XER[SO] is sticky: once set it
%%%   remains set until explicitly cleared.

-module(hipe_rtl_to_ppc).
-export([translate/1]).

-include("../rtl/hipe_rtl.hrl").

translate(RTL) ->
  hipe_gensym:init(ppc),
  hipe_gensym:set_var(ppc, hipe_ppc_registers:first_virtual()),
  hipe_gensym:set_label(ppc, hipe_gensym:get_label(rtl)),
  Map0 = vmap_empty(),
  {Formals, Map1} = conv_formals(hipe_rtl:rtl_params(RTL), Map0),
  OldData = hipe_rtl:rtl_data(RTL),
  {Code0, NewData} = conv_insn_list(hipe_rtl:rtl_code(RTL), Map1, OldData),
  {RegFormals, _} = split_args(Formals),
  Code =
    case RegFormals of
      [] -> Code0;
      _ -> [hipe_ppc:mk_label(hipe_gensym:get_next_label(ppc)) |
	    move_formals(RegFormals, Code0)]
    end,
  IsClosure = hipe_rtl:rtl_is_closure(RTL),
  IsLeaf = hipe_rtl:rtl_is_leaf(RTL),
  hipe_ppc:mk_defun(hipe_rtl:rtl_fun(RTL),
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
    #switch{} -> conv_switch(I, Map, Data);
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
  {Dst, Map0} = conv_fpreg(hipe_rtl:fconv_dst(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:fconv_src(I), Map0),
  I2 =
    case hipe_ppc:is_temp(Src) of
      true ->
	mk_fconv(Dst, Src);
      false ->
	Tmp = new_untagged_temp(),
	mk_li(Tmp, Src,
	      mk_fconv(Dst, Tmp))
    end,
  {I2, Map1, Data}.

mk_fconv(Dst, Src) ->
  CSP = hipe_ppc:mk_temp(1, 'untagged'),
  case get(hipe_target_arch) of
    powerpc ->
      R0 = hipe_ppc:mk_temp(0, 'untagged'),
      RTmp1 = hipe_ppc:mk_new_temp('untagged'),
      RTmp2 = hipe_ppc:mk_new_temp('untagged'),
      RTmp3 = hipe_ppc:mk_new_temp('untagged'),
      FTmp1 = hipe_ppc:mk_new_temp('double'),
      FTmp2 = hipe_ppc:mk_new_temp('double'),
      [hipe_ppc:mk_pseudo_li(RTmp1, {fconv_constant,c_const}),
       hipe_ppc:mk_lfd(FTmp1, 0, RTmp1),
       hipe_ppc:mk_alu('xoris', RTmp2, Src, hipe_ppc:mk_uimm16(16#8000)),
       hipe_ppc:mk_store('stw', RTmp2, 28, CSP),
       hipe_ppc:mk_alu('addis', RTmp3, R0, hipe_ppc:mk_simm16(16#4330)),
       hipe_ppc:mk_store('stw', RTmp3, 24, CSP),
       hipe_ppc:mk_lfd(FTmp2, 24, CSP),
       hipe_ppc:mk_fp_binary('fsub', Dst, FTmp2, FTmp1)];
    ppc64 ->
      [hipe_ppc:mk_store('std', Src, 24, CSP),
       hipe_ppc:mk_lfd(Dst, 24, CSP),
       hipe_ppc:mk_fp_unary('fcfid', Dst, Dst)]
  end.

conv_fmove(I, Map, Data) ->
  %% Dst := Src, where both Dst and Src are FP regs
  {Dst, Map0} = conv_fpreg(hipe_rtl:fmove_dst(I), Map),
  {Src, Map1} = conv_fpreg(hipe_rtl:fmove_src(I), Map0),
  I2 = mk_fmove(Dst, Src),
  {I2, Map1, Data}.

mk_fmove(Dst, Src) ->
  [hipe_ppc:mk_pseudo_fmove(Dst, Src)].

conv_fload(I, Map, Data) ->
  %% Dst := MEM[Base+Off], where Dst is FP reg
  {Dst, Map0} = conv_fpreg(hipe_rtl:fload_dst(I), Map),
  {Base1, Map1} = conv_src(hipe_rtl:fload_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:fload_offset(I), Map1),
  I2 = mk_fload(Dst, Base1, Base2),
  {I2, Map2, Data}.

mk_fload(Dst, Base1, Base2) ->
  case hipe_ppc:is_temp(Base1) of
    true ->
      case hipe_ppc:is_temp(Base2) of
	true ->
	  mk_fload_rr(Dst, Base1, Base2);
	_ ->
	  mk_fload_ri(Dst, Base1, Base2)
      end;
    _ ->
      case hipe_ppc:is_temp(Base2) of
	true ->
	  mk_fload_ri(Dst, Base2, Base1);
	_ ->
	  mk_fload_ii(Dst, Base1, Base2)
      end
  end.

mk_fload_ii(Dst, Base1, Base2) ->
  io:format("~w: RTL fload with two immediates\n", [?MODULE]),
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Base1,
	mk_fload_ri(Dst, Tmp, Base2)).

mk_fload_ri(Dst, Base, Disp) ->
  hipe_ppc:mk_fload(Dst, Disp, Base, 'new').

mk_fload_rr(Dst, Base1, Base2) ->
  [hipe_ppc:mk_lfdx(Dst, Base1, Base2)].

conv_fstore(I, Map, Data) ->
  %% MEM[Base+Off] := Src, where Src is FP reg
  {Base1, Map0} = conv_dst(hipe_rtl:fstore_base(I), Map),
  {Src, Map1} = conv_fpreg(hipe_rtl:fstore_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:fstore_offset(I), Map1),
  I2 = mk_fstore(Src, Base1, Base2),
  {I2, Map2, Data}.

mk_fstore(Src, Base1, Base2) ->
  case hipe_ppc:is_temp(Base2) of
    true ->
      mk_fstore_rr(Src, Base1, Base2);
    _ ->
      mk_fstore_ri(Src, Base1, Base2)
  end.

mk_fstore_ri(Src, Base, Disp) ->
  hipe_ppc:mk_fstore(Src, Disp, Base, 'new').

mk_fstore_rr(Src, Base1, Base2) ->
  [hipe_ppc:mk_stfdx(Src, Base1, Base2)].

conv_fp_binary(I, Map, Data) ->
  {Dst, Map0} = conv_fpreg(hipe_rtl:fp_dst(I), Map),
  {Src1, Map1} = conv_fpreg(hipe_rtl:fp_src1(I), Map0),
  {Src2, Map2} = conv_fpreg(hipe_rtl:fp_src2(I), Map1),
  RtlFpOp = hipe_rtl:fp_op(I),
  I2 = mk_fp_binary(Dst, Src1, RtlFpOp, Src2),
  {I2, Map2, Data}.

mk_fp_binary(Dst, Src1, RtlFpOp, Src2) ->
  FpBinOp =
    case RtlFpOp of
      'fadd' -> 'fadd';
      'fdiv' -> 'fdiv';
      'fmul' -> 'fmul';
      'fsub' -> 'fsub'
    end,
  [hipe_ppc:mk_fp_binary(FpBinOp, Dst, Src1, Src2)].

conv_fp_unary(I, Map, Data) ->
  {Dst, Map0} = conv_fpreg(hipe_rtl:fp_unop_dst(I), Map),
  {Src, Map1} = conv_fpreg(hipe_rtl:fp_unop_src(I), Map0),
  RtlFpUnOp = hipe_rtl:fp_unop_op(I),
  I2 = mk_fp_unary(Dst, Src, RtlFpUnOp),
  {I2, Map1, Data}.

mk_fp_unary(Dst, Src, RtlFpUnOp) ->
  FpUnOp =
    case RtlFpUnOp of
      'fchs' -> 'fneg'
    end,
  [hipe_ppc:mk_fp_unary(FpUnOp, Dst, Src)].

conv_alu(I, Map, Data) ->
  %% dst = src1 aluop src2
  {Dst, Map0} = conv_dst(hipe_rtl:alu_dst(I), Map),
  {Src1, Map1} = conv_src(hipe_rtl:alu_src1(I), Map0),
  {Src2, Map2} = conv_src(hipe_rtl:alu_src2(I), Map1),
  RtlAluOp = hipe_rtl:alu_op(I),
  I2 = mk_alu(Dst, Src1, RtlAluOp, Src2),
  {I2, Map2, Data}.

mk_alu(Dst, Src1, RtlAluOp, Src2) ->
  case hipe_ppc:is_temp(Src1) of
    true ->
      case hipe_ppc:is_temp(Src2) of
	true ->
	  mk_alu_rr(Dst, Src1, RtlAluOp, Src2);
	_ ->
	  mk_alu_ri(Dst, Src1, RtlAluOp, Src2)
      end;
    _ ->
      case hipe_ppc:is_temp(Src2) of
	true ->
	  mk_alu_ir(Dst, Src1, RtlAluOp, Src2);
	_ ->
	  mk_alu_ii(Dst, Src1, RtlAluOp, Src2)
      end
  end.

mk_alu_ii(Dst, Src1, RtlAluOp, Src2) ->
  io:format("~w: RTL alu with two immediates (~w ~w ~w)\n",
	    [?MODULE, Src1, RtlAluOp, Src2]),
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src1,
	mk_alu_ri(Dst, Tmp, RtlAluOp, Src2)).

mk_alu_ir(Dst, Src1, RtlAluOp, Src2) ->
  case rtl_aluop_commutes(RtlAluOp) of
    true ->
      mk_alu_ri(Dst, Src2, RtlAluOp, Src1);
    _ ->
      Tmp = new_untagged_temp(),
      mk_li(Tmp, Src1,
	    mk_alu_rr(Dst, Tmp, RtlAluOp, Src2))
  end.

mk_alu_ri(Dst, Src1, RtlAluOp, Src2) ->
  case RtlAluOp of
    'sub' ->	% there is no 'subi'
      mk_alu_ri_addi(Dst, Src1, -Src2);
    'add' ->	% 'addi' has a 16-bit simm operand
      mk_alu_ri_addi(Dst, Src1, Src2);
    'mul' ->	% 'mulli' has a 16-bit simm operand
      mk_alu_ri_simm16(Dst, Src1, RtlAluOp, 'mulli', Src2);
    'and' ->	% 'andi.' has a 16-bit uimm operand
      if Src2 band (bnot 16#ffffffff) =:= 0 ->
	  case rlwinm_mask(Src2) of
	    {MB,ME} ->
	      [hipe_ppc:mk_unary({'rlwinm',0,MB,ME}, Dst, Src1)];
	    _ ->
	      mk_alu_ri_bitop(Dst, Src1, RtlAluOp, 'andi.', Src2)
	  end;
	 true ->
	  mk_alu_ri_bitop(Dst, Src1, RtlAluOp, 'andi.', Src2)
      end;
    'or' ->	% 'ori' has a 16-bit uimm operand
      mk_alu_ri_bitop(Dst, Src1, RtlAluOp, 'ori', Src2);
    'xor' ->	% 'xori' has a 16-bit uimm operand
      mk_alu_ri_bitop(Dst, Src1, RtlAluOp, 'xori', Src2);
    _ ->	% shift ops have 5-bit uimm operands
      mk_alu_ri_shift(Dst, Src1, RtlAluOp, Src2)
  end.

rlwinm_mask(Imm) ->
  Res1 = rlwinm_mask2(Imm),
  case Res1 of
    {_MB,_ME} -> Res1;
    [] ->
      case rlwinm_mask2(bnot Imm) of
	{MB,ME} -> {ME+1,MB-1};
	[] -> []
      end
  end.

rlwinm_mask2(Imm) ->
  case Imm band 16#ffffffff of
    0 -> [];
    Word ->
      MB = lsb_log2(Word),	% first 1 bit
      case bnot(Word bsr MB) band 16#ffffffff of
	0 -> []; % Imm was all-bits-one XXX: we should handle this
	Word1 ->
	  ME1 = lsb_log2(Word1),% first 0 bit after the 1s
	  case Word bsr (MB+ME1) of
	    0 ->
	      ME = MB+ME1-1,	% last 1 bit
	      {31-ME, 31-MB};	% convert to PPC sick and twisted bit numbers
	    _ ->
	      []
	  end
      end
  end.

lsb_log2(Word) -> % PRE: Word =/= 0
  bitN_log2(Word band -Word, 0).

bitN_log2(BitN, ShiftN) ->
  if BitN > 16#ffff ->
      bitN_log2(BitN bsr 16, ShiftN + 16);
     true ->
      ShiftN + hweight16(BitN - 1)
  end.

hweight16(Word) -> % PRE: 0 <= Word <= 16#ffff
  Res1 = (Word band 16#5555) + ((Word bsr 1) band 16#5555),
  Res2 = (Res1 band 16#3333) + ((Res1 bsr 2) band 16#3333),
  Res3 = (Res2 band 16#0F0F) + ((Res2 bsr 4) band 16#0F0F),
         (Res3 band 16#00FF) + ((Res3 bsr 8) band 16#00FF).

mk_alu_ri_addi(Dst, Src1, Src2) ->
  mk_alu_ri_simm16(Dst, Src1, 'add', 'addi', Src2).

mk_alu_ri_simm16(Dst, Src1, RtlAluOp, AluOp, Src2) ->
  if is_integer(Src2), -32768 =< Src2, Src2 < 32768 ->
      [hipe_ppc:mk_alu(AluOp, Dst, Src1,
		       hipe_ppc:mk_simm16(Src2))];
     true ->
      mk_alu_ri_rr(Dst, Src1, RtlAluOp, Src2)
  end.

mk_alu_ri_bitop(Dst, Src1, RtlAluOp, AluOp, Src2) ->
  if is_integer(Src2), 0 =< Src2, Src2 < 65536 ->
      [hipe_ppc:mk_alu(AluOp, Dst, Src1,
		       hipe_ppc:mk_uimm16(Src2))];
     true ->
      mk_alu_ri_rr(Dst, Src1, RtlAluOp, Src2)
  end.

mk_alu_ri_shift(Dst, Src1, RtlAluOp, Src2) ->
  case get(hipe_target_arch) of
    ppc64 ->
      if Src2 < 64, Src2 >= 0 ->
	  AluOp =
	    case RtlAluOp of
	      'sll' -> 'sldi'; % alias for rldimi %%% buggy
	      'srl' -> 'srdi'; % alias for rldimi %%% buggy
	      'sra' -> 'sradi' %%% buggy
	    end,
	  [hipe_ppc:mk_alu(AluOp, Dst, Src1,
			   hipe_ppc:mk_uimm16(Src2))];
	 true ->
	  mk_alu_ri_rr(Dst, Src1, RtlAluOp, Src2)
      end;
    powerpc ->
      if Src2 < 32, Src2 >= 0 ->
	  AluOp =
	    case RtlAluOp of
	      'sll' -> 'slwi'; % alias for rlwinm
	      'srl' -> 'srwi'; % alias for rlwinm
	      'sra' -> 'srawi'
	    end,
	  [hipe_ppc:mk_alu(AluOp, Dst, Src1,
			   hipe_ppc:mk_uimm16(Src2))];
	 true ->
	  mk_alu_ri_rr(Dst, Src1, RtlAluOp, Src2)
      end
  end.

mk_alu_ri_rr(Dst, Src1, RtlAluOp, Src2) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src2,
	mk_alu_rr(Dst, Src1, RtlAluOp, Tmp)).

mk_alu_rr(Dst, Src1, RtlAluOp, Src2) ->
  case RtlAluOp of
    'sub' -> % PPC weirdness
      [hipe_ppc:mk_alu('subf', Dst, Src2, Src1)];
    _ ->
      AluOp =
	case {get(hipe_target_arch), RtlAluOp} of
	  {_, 'add'} -> 'add';
	  {_, 'or'}  -> 'or';
	  {_, 'and'} -> 'and';
	  {_, 'xor'} -> 'xor';

	  {powerpc, 'mul'} -> 'mullw';
	  {powerpc, 'sll'} -> 'slw';
	  {powerpc, 'srl'} -> 'srw';
	  {powerpc, 'sra'} -> 'sraw';

	  {ppc64, 'mul'} -> 'mulld';
	  {ppc64, 'sll'} -> 'sld';
	  {ppc64, 'srl'} -> 'srd';
	  {ppc64, 'sra'} -> 'srad'
	end,
      [hipe_ppc:mk_alu(AluOp, Dst, Src1, Src2)]
  end.

conv_alub(I, Map, Data) ->
  %% dst = src1 aluop src2; if COND goto label
  HasDst = hipe_rtl:alub_has_dst(I),
  {Src1, Map0} = conv_src(hipe_rtl:alub_src1(I), Map),
  {Src2, Map1} = conv_src(hipe_rtl:alub_src2(I), Map0),
  RtlAlubOp = hipe_rtl:alub_op(I),
  RtlAlubCond = hipe_rtl:alub_cond(I),
  case {HasDst, RtlAlubOp} of
    {false, sub} ->
      {BCond,Sign} = conv_branch_cond(RtlAlubCond),
      I2 = mk_branch(Src1, BCond, Sign, Src2,
		     hipe_rtl:alub_true_label(I),
		     hipe_rtl:alub_false_label(I),
		     hipe_rtl:alub_pred(I)),
      {I2, Map1, Data};
    _ ->
      {Dst, Map2} =
	case HasDst of
	  false -> {new_untagged_temp(), Map1};
	  true -> conv_dst(hipe_rtl:alub_dst(I), Map1)
	end,
      {AluOp, BCond} =
	case {RtlAlubOp, RtlAlubCond} of
	  {'add', 'ltu'} ->
	    {'addc', 'eq'};
	  {_, _} ->
	    {conv_alub_op(RtlAlubOp), conv_alub_cond(RtlAlubCond)}
	end,
      BC = mk_pseudo_bc(BCond,
			hipe_rtl:alub_true_label(I),
			hipe_rtl:alub_false_label(I),
			hipe_rtl:alub_pred(I)),
      I2 =
	case {AluOp, BCond} of
	  {'addc', 'eq'} ->	% copy XER[CA] to CR0[EQ] before the BC
	    TmpR = new_untagged_temp(),
	    [hipe_ppc:mk_mfspr(TmpR, 'xer'),
	     hipe_ppc:mk_mtcr(TmpR) |
	     BC];
	  _ -> BC
	end,
      {NewSrc1, NewSrc2} =
	case AluOp of
	  'subf' -> {Src2, Src1};
	  _ -> {Src1, Src2}
	end,
      I1 = mk_alub(Dst, NewSrc1, AluOp, NewSrc2, BCond),
      {I1 ++ I2, Map2, Data}
  end.

conv_alub_op(RtlAluOp) ->
  case {get(hipe_target_arch), RtlAluOp} of
    {_, 'add'} -> 'add';
    {_, 'sub'} -> 'subf';	% XXX: must swap operands
    {_, 'or'}  -> 'or';
    {_, 'and'} -> 'and';
    {_, 'xor'} -> 'xor';

    {powerpc, 'mul'} -> 'mullw';
    {powerpc, 'sll'} -> 'slw';
    {powerpc, 'srl'} -> 'srw';
    {powerpc, 'sra'} -> 'sraw';

    {ppc64, 'mul'} -> 'mulld';
    {ppc64, 'sll'} -> 'sld';
    {ppc64, 'srl'} -> 'srd';
    {ppc64, 'sra'} -> 'srad'
  end.

aluop_commutes(AluOp) ->
  case AluOp of
    'add'   -> true;
    'addc'  -> true;
    'subf'  -> false;
    'mullw' -> true;
    'or'    -> true;
    'and'   -> true;
    'xor'   -> true;
    'slw'   -> false;
    'srw'   -> false;
    'sraw'  -> false;
    'mulld' -> true;	% ppc64
    'sld'   -> false;	% ppc64
    'srd'   -> false;	% ppc64
    'srad'  -> false	% ppc64
  end.

conv_alub_cond(Cond) ->	% only signed
  case Cond of
    eq	-> 'eq';
    ne	-> 'ne';
    gt	-> 'gt';
    ge	-> 'ge';
    lt	-> 'lt';
    le	-> 'le';
    overflow -> 'so';
    not_overflow -> 'ns';
    _	-> exit({?MODULE,conv_alub_cond,Cond})
  end.

mk_alub(Dst, Src1, AluOp, Src2, BCond) ->
  case hipe_ppc:is_temp(Src1) of
    true ->
      case hipe_ppc:is_temp(Src2) of
	true ->
	  mk_alub_rr(Dst, Src1, AluOp, Src2, BCond);
	_ ->
	  mk_alub_ri(Dst, Src1, AluOp, Src2, BCond)
      end;
    _ ->
      case hipe_ppc:is_temp(Src2) of
	true ->
	  mk_alub_ir(Dst, Src1, AluOp, Src2, BCond);
	_ ->
	  mk_alub_ii(Dst, Src1, AluOp, Src2, BCond)
      end
  end.

mk_alub_ii(Dst, Src1, AluOp, Src2, BCond) ->
  io:format("~w: RTL alub with two immediates\n", [?MODULE]),
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src1,
	mk_alub_ri(Dst, Tmp, AluOp, Src2, BCond)).

mk_alub_ir(Dst, Src1, AluOp, Src2, BCond) ->
  case aluop_commutes(AluOp) of
    true ->
      mk_alub_ri(Dst, Src2, AluOp, Src1, BCond);
    _ ->
      Tmp = new_untagged_temp(),
      mk_li(Tmp, Src1,
	    mk_alub_rr(Dst, Tmp, AluOp, Src2, BCond))
  end.

mk_alub_ri(Dst, Src1, AluOp, Src2, BCond) ->
  true = is_integer(Src2),
  case BCond of
    'so' -> mk_alub_ri_OE(Dst, Src1, AluOp, Src2);
    'ns' -> mk_alub_ri_OE(Dst, Src1, AluOp, Src2);
    _ -> mk_alub_ri_Rc(Dst, Src1, AluOp, Src2)
  end.

mk_alub_ri_OE(Dst, Src1, AluOp, Src2) ->
  %% Only 'add', 'subf', and 'mullw' apply here, and 'subf' becomes 'add'.
  %% 'add' and 'mullw' have no immediate+Rc+OE forms.
  %% Rewrite to reg/reg form. Sigh.
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src2,
	mk_alub_rr_OE(Dst, Src1, AluOp, Tmp)).

mk_alub_ri_Rc(Dst, Src1, AluOp, Src2) ->
  case AluOp of
    'subf' ->	% there is no 'subfi.', use 'addic.' or 'add.'
      mk_alub_ri_Rc_addi(Dst, Src1, -Src2, 'addic.', 'add.');
    'add' ->	% 'addic.' has a 16-bit simm operand
      mk_alub_ri_Rc_addi(Dst, Src1, Src2, 'addic.', 'add.');
    'addc' ->	% 'addic' has a 16-bit simm operand
      mk_alub_ri_Rc_addi(Dst, Src1, Src2, 'addic', 'addc');
    'mullw' ->  % there is no 'mulli.'
      mk_alub_ri_Rc_rr(Dst, Src1, 'mullw.', Src2);
    'mulld' ->	% there is no 'mulli.'
      mk_alub_ri_Rc_rr(Dst, Src1, 'mulld.', Src2);
    'or' ->	% there is no 'ori.'
      mk_alub_ri_Rc_rr(Dst, Src1, 'or.', Src2);
    'xor' ->	% there is no 'xori.'
      mk_alub_ri_Rc_rr(Dst, Src1, 'xor.', Src2);
    'and' ->	% 'andi.' has a 16-bit uimm operand
      if
	Src2 band (bnot 16#ffffffff) =:= 0 ->
	  case rlwinm_mask(Src2) of
	    {MB,ME} ->
	      [hipe_ppc:mk_unary({'rlwinm.',0,MB,ME}, Dst, Src1)];
	    _ ->
	      mk_alub_ri_Rc_andi(Dst, Src1, Src2)
	  end;
	true ->
	  mk_alub_ri_Rc_andi(Dst, Src1, Src2)
      end;
    _ ->	% shift ops have 5-bit uimm operands
      mk_alub_ri_Rc_shift(Dst, Src1, AluOp, Src2)
  end.

mk_alub_ri_Rc_addi(Dst, Src1, Src2, AddImmOp, AddRegOp) ->
  if is_integer(Src2), -32768 =< Src2, Src2 < 32768 ->
      [hipe_ppc:mk_alu(AddImmOp, Dst, Src1,
		       hipe_ppc:mk_simm16(Src2))];
     true ->
      mk_alub_ri_Rc_rr(Dst, Src1, AddRegOp, Src2)
  end.

mk_alub_ri_Rc_andi(Dst, Src1, Src2) ->
  if Src2 < 65536, Src2 >= 0 ->
      [hipe_ppc:mk_alu('andi.', Dst, Src1,
		       hipe_ppc:mk_uimm16(Src2))];
     true ->
      mk_alub_ri_Rc_rr(Dst, Src1, 'and.', Src2)
  end.

mk_alub_ri_Rc_shift(Dst, Src1, AluOp, Src2) ->
  {AluOpIDot, MaxIShift} =
    case AluOp of
      'slw'  -> {'slwi.', 32}; % alias for rlwinm.
      'srw'  -> {'srwi.', 32}; % alias for rlwinm.
      'sraw' -> {'srawi.', 32};
      'sld'  -> {'sldi.', 64};
      'srd'  -> {'srdi.', 64};
      'srad' -> {'sradi.', 64}
    end,
  if Src2 < MaxIShift, Src2 >= 0 ->
      [hipe_ppc:mk_alu(AluOpIDot, Dst, Src1,
		       hipe_ppc:mk_uimm16(Src2))];
     true ->
      AluOpDot =
	case AluOp of
	  'slw'  -> 'slw.';
	  'srw'  -> 'srw.';
	  'sraw' -> 'sraw.';
	  'sld'  -> 'sld.';
	  'srd'  -> 'srd.';
	  'srad' -> 'srad.'
	end,
      mk_alub_ri_Rc_rr(Dst, Src1, AluOpDot, Src2)
  end.

mk_alub_ri_Rc_rr(Dst, Src1, AluOp, Src2) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src2,
	[hipe_ppc:mk_alu(AluOp, Dst, Src1, Tmp)]).

mk_alub_rr(Dst, Src1, AluOp, Src2, BCond) ->
  case BCond of
    'so' -> mk_alub_rr_OE(Dst, Src1, AluOp, Src2);
    'ns' -> mk_alub_rr_OE(Dst, Src1, AluOp, Src2);
    _ -> mk_alub_rr_Rc(Dst, Src1, AluOp, Src2)
  end.

mk_alub_rr_OE(Dst, Src1, AluOp, Src2) ->
  AluOpODot =
    case AluOp of
      'subf'  -> 'subfo.';
      'add'   -> 'addo.';
      'mullw' -> 'mullwo.';
      'mulld' -> 'mulldo.'
		 %% fail for addc, or, and, xor, slw, srw, sraw
    end,
  [hipe_ppc:mk_alu(AluOpODot, Dst, Src1, Src2)].

mk_alub_rr_Rc(Dst, Src1, AluOp, Src2) ->
  AluOpDot =
    case AluOp of
      'subf'  -> 'subf.';
      'add'   -> 'add.';
      'addc'  -> 'addc';	% only interested in CA, no Rc needed
      'mullw' -> 'mullw.';
      'mulld' -> 'mulld.';
      'or'    -> 'or.';
      'and'   -> 'and.';
      'xor'   -> 'xor.';
      'slw'   -> 'slw.';
      'sld'   -> 'sld.';
      'srw'   -> 'srw.';
      'srd'   -> 'srd.';
      'sraw'  -> 'sraw.';
      'srad'  -> 'srad.'
    end,
  [hipe_ppc:mk_alu(AluOpDot, Dst, Src1, Src2)].

conv_branch_cond(Cond) -> % may be unsigned
  case Cond of
    gtu -> {'gt', 'unsigned'};
    geu -> {'ge', 'unsigned'};
    ltu -> {'lt', 'unsigned'};
    leu -> {'le', 'unsigned'};
    _   -> {conv_alub_cond(Cond), 'signed'}
  end.    

mk_branch(Src1, BCond, Sign, Src2, TrueLab, FalseLab, Pred) ->
  case hipe_ppc:is_temp(Src1) of
    true ->
      case hipe_ppc:is_temp(Src2) of
	true ->
	  mk_branch_rr(Src1, BCond, Sign, Src2, TrueLab, FalseLab, Pred);
	_ ->
	  mk_branch_ri(Src1, BCond, Sign, Src2, TrueLab, FalseLab, Pred)
      end;
    _ ->
      case hipe_ppc:is_temp(Src2) of
	true ->
	  NewBCond = commute_bcond(BCond),
	  mk_branch_ri(Src2, NewBCond, Sign, Src1, TrueLab, FalseLab, Pred);
	_ ->
	  mk_branch_ii(Src1, BCond, Sign, Src2, TrueLab, FalseLab, Pred)
      end
  end.

commute_bcond(BCond) ->	% if x BCond y, then y commute_bcond(BCond) x
  case BCond of
    'eq' -> 'eq';	% ==, ==
    'ne' -> 'ne';	% !=, !=
    'gt' -> 'lt';	% >, <
    'ge' -> 'le';	% >=, <=
    'lt' -> 'gt';	% <, >
    'le' -> 'ge';	% <=, >=
    %% so/ns: n/a
    _ -> exit({?MODULE,commute_bcond,BCond})
  end.

mk_branch_ii(Src1, BCond, Sign, Src2, TrueLab, FalseLab, Pred) ->
  io:format("~w: RTL branch with two immediates\n", [?MODULE]),
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Src1,
	mk_branch_ri(Tmp, BCond, Sign, Src2,
		     TrueLab, FalseLab, Pred)).

mk_branch_ri(Src1, BCond, Sign, Src2, TrueLab, FalseLab, Pred) ->
  {FixSrc2,NewSrc2,CmpOp} =
    case Sign of
      'signed' ->
	if is_integer(Src2), -32768 =< Src2, Src2 < 32768 ->
	    {[], hipe_ppc:mk_simm16(Src2), hipe_ppc:cmpiop_word()};
	   true ->
	    Tmp = new_untagged_temp(),
	    {mk_li(Tmp, Src2), Tmp, hipe_ppc:cmpop_word()}
	end;
      'unsigned' ->
	if is_integer(Src2), 0 =< Src2, Src2 < 65536 ->
	    {[], hipe_ppc:mk_uimm16(Src2), hipe_ppc:cmpliop_word()};
	   true ->
	    Tmp = new_untagged_temp(),
	    {mk_li(Tmp, Src2), Tmp, hipe_ppc:cmplop_word()}
	end
    end,
  FixSrc2 ++
    mk_cmp_bc(CmpOp, Src1, NewSrc2, BCond, TrueLab, FalseLab, Pred).

mk_branch_rr(Src1, BCond, Sign, Src2, TrueLab, FalseLab, Pred) ->
  CmpOp =
    case Sign of
      'signed' -> hipe_ppc:cmpop_word();
      'unsigned' -> hipe_ppc:cmplop_word()
    end,
  mk_cmp_bc(CmpOp, Src1, Src2, BCond, TrueLab, FalseLab, Pred).

mk_cmp_bc(CmpOp, Src1, Src2, BCond, TrueLab, FalseLab, Pred) ->
  [hipe_ppc:mk_cmp(CmpOp, Src1, Src2) |
   mk_pseudo_bc(BCond, TrueLab, FalseLab, Pred)].

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
  case hipe_ppc:is_prim(Fun) of
    true ->
      mk_primop_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage);
    false ->
      mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage)
  end.

mk_primop_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage) ->
  case hipe_ppc:prim_prim(Prim) of
    'extsh' ->
      mk_extsh_call(Dsts, Args, ContLab, ExnLab, Linkage);
    'lhbrx' ->
      mk_lhbrx_call(Dsts, Args, ContLab, ExnLab, Linkage);
    'lwbrx' ->
      mk_lwbrx_call(Dsts, Args, ContLab, ExnLab, Linkage);
    _ ->
      mk_general_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage)
  end.

mk_extsh_call([Dst], [Src], [], [], not_remote) ->
  true = hipe_ppc:is_temp(Src),
  [hipe_ppc:mk_unary('extsh', Dst, Src)].

mk_lhbrx_call(Dsts, [Base,Offset], [], [], not_remote) ->
  case Dsts of
    [Dst] -> mk_loadx('lhbrx', Dst, Base, Offset);
    [] -> [] % result unused, cancel the operation
  end.

mk_lwbrx_call([Dst], [Base,Offset], [], [], not_remote) ->
  mk_loadx('lwbrx', Dst, Base, Offset).

mk_loadx(LdxOp, Dst, Base, Offset) ->
  true = hipe_ppc:is_temp(Base),
  {FixOff,NewOff} =
    case hipe_ppc:is_temp(Offset) of
      true -> {[], Offset};
      false ->
	Tmp = new_untagged_temp(),
	{mk_li(Tmp, Offset), Tmp}
    end,
  FixOff ++ [hipe_ppc:mk_loadx(LdxOp, Dst, Base, NewOff)].

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
	    NewContLab = hipe_gensym:get_next_label(ppc),
	    {NewContLab, [hipe_ppc:mk_label(NewContLab)]};
	  _ ->
	    {ContLab, []}
	end;
      Moves ->
	%% Change the call to continue at a new basic block.
	%% In this block move the result registers to the Dsts,
	%% then continue at the call's original continuation.
	NewContLab = hipe_gensym:get_next_label(ppc),
	case ContLab of
	  [] ->
	    %% This is just a fallthrough
	    %% No jump back after the moves.
	    {NewContLab,
	     [hipe_ppc:mk_label(NewContLab) |
	      Moves]};
	  _ ->
	    %% The call has a continuation. Jump to it.
	    {NewContLab,
	     [hipe_ppc:mk_label(NewContLab) |
	      Moves ++
	      [hipe_ppc:mk_b_label(ContLab)]]}
	end
    end,
  SDesc = hipe_ppc:mk_sdesc(ExnLab, 0, length(Args), {}),
  {FixFunC,FunC} = fix_func(Fun),
  CallInsn = hipe_ppc:mk_pseudo_call(FunC, SDesc, RealContLab, Linkage),
  {RegArgs,StkArgs} = split_args(Args),
  FixFunC ++
    mk_push_args(StkArgs, move_actuals(RegArgs, [CallInsn | Tail])).

mk_call_results([]) ->
  [];
mk_call_results([Dst]) ->
  RV = hipe_ppc:mk_temp(hipe_ppc_registers:return_value(), 'tagged'),
  [hipe_ppc:mk_pseudo_move(Dst, RV)];
mk_call_results(Dsts) ->
  exit({?MODULE,mk_call_results,Dsts}).

fix_func(Fun) ->
  case hipe_ppc:is_temp(Fun) of
    true -> {[hipe_ppc:mk_mtspr('ctr', Fun)], 'ctr'};
    _ -> {[], Fun}
  end.

mk_push_args(StkArgs, Tail) ->
  case length(StkArgs) of
    0 ->
      Tail;
    NrStkArgs ->
      [hipe_ppc:mk_pseudo_call_prepare(NrStkArgs) |
       mk_store_args(StkArgs, NrStkArgs * word_size(), Tail)]
  end.
  
mk_store_args([Arg|Args], PrevOffset, Tail) ->
  Offset = PrevOffset - word_size(),
  {Src,FixSrc} =
    case hipe_ppc:is_temp(Arg) of
      true ->
	{Arg, []};
      _ ->
	Tmp = new_tagged_temp(),
	{Tmp, mk_li(Tmp, Arg)}
    end,
  Store = hipe_ppc:mk_store(hipe_ppc:stop_word(), Src, Offset, mk_sp()),
  mk_store_args(Args, Offset, FixSrc ++ [Store | Tail]);
mk_store_args([], _, Tail) ->
  Tail.

conv_comment(I, Map, Data) ->
  I2 = [hipe_ppc:mk_comment(hipe_rtl:comment_text(I))],
  {I2, Map, Data}.

conv_enter(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:enter_arglist(I), Map),
  {Fun, Map1} = conv_fun(hipe_rtl:enter_fun(I), Map0),
  I2 = mk_enter(Fun, Args, hipe_rtl:enter_type(I)),
  {I2, Map1, Data}.

mk_enter(Fun, Args, Linkage) ->
  {FixFunC,FunC} = fix_func(Fun),
  Arity = length(Args),
  {RegArgs,StkArgs} = split_args(Args),
  FixFunC ++
    move_actuals(RegArgs,
		 [hipe_ppc:mk_pseudo_tailcall_prepare(),
		  hipe_ppc:mk_pseudo_tailcall(FunC, Arity, StkArgs, Linkage)]).

conv_goto(I, Map, Data) ->
  I2 = [hipe_ppc:mk_b_label(hipe_rtl:goto_label(I))],
  {I2, Map, Data}.

conv_label(I, Map, Data) ->
  I2 = [hipe_ppc:mk_label(hipe_rtl:label_name(I))],
  {I2, Map, Data}.

conv_load(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_dst(I), Map),
  {Base1, Map1} = conv_src(hipe_rtl:load_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:load_offset(I), Map1),
  LoadSize = hipe_rtl:load_size(I),
  LoadSign = hipe_rtl:load_sign(I),
  I2 = mk_load(Dst, Base1, Base2, LoadSize, LoadSign),
  {I2, Map2, Data}.

mk_load(Dst, Base1, Base2, LoadSize, LoadSign) ->
  {LdOp, Rest} =
    case {LoadSize, LoadSign} of
      {byte, signed} -> {'lbz', [hipe_ppc:mk_unary('extsb', Dst, Dst)]};
      {byte, unsigned} -> {'lbz', []};
      {int16, signed} -> {'lha', []};
      {int16, unsigned} -> {'lhz', []};
      {int32, signed} ->
	case get(hipe_target_arch) of
	  powerpc -> {'lwz', []};
	  ppc64 -> {'lwa', []}
	end;
      {int32, unsigned} -> {'lwz', []};
      {word, _} -> {hipe_ppc:ldop_word(), []}
    end,
  case hipe_ppc:is_temp(Base1) of
    true ->
      case hipe_ppc:is_temp(Base2) of
	true ->
	  mk_load_rr(Dst, Base1, Base2, LdOp, Rest);
	_ ->
	  mk_load_ri(Dst, Base1, Base2, LdOp, Rest)
      end;
    _ ->
      case hipe_ppc:is_temp(Base2) of
	true ->
	  mk_load_ri(Dst, Base2, Base1, LdOp, Rest);
	_ ->
	  mk_load_ii(Dst, Base1, Base2, LdOp, Rest)
      end
  end.

mk_load_ii(Dst, Base1, Base2, LdOp, Rest) ->
  io:format("~w: RTL load with two immediates\n", [?MODULE]),
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Base1,
	mk_load_ri(Dst, Tmp, Base2, LdOp, Rest)).
   
mk_load_ri(Dst, Base, Disp, LdOp, Rest) ->
  hipe_ppc:mk_load(LdOp, Dst, Disp, Base, 'new', Rest).

mk_load_rr(Dst, Base1, Base2, LdOp, Rest) ->
  LdxOp = hipe_ppc:ldop_to_ldxop(LdOp),
  [hipe_ppc:mk_loadx(LdxOp, Dst, Base1, Base2) | Rest].

conv_load_address(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_address_dst(I), Map),
  Addr = hipe_rtl:load_address_addr(I),
  Type = hipe_rtl:load_address_type(I),
  Src = {Addr,Type},
  I2 = [hipe_ppc:mk_pseudo_li(Dst, Src)],
  {I2, Map0, Data}.

conv_load_atom(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_atom_dst(I), Map),
  Src = hipe_rtl:load_atom_atom(I),
  I2 = [hipe_ppc:mk_pseudo_li(Dst, Src)],
  {I2, Map0, Data}.

conv_move(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:move_dst(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:move_src(I), Map0),
  I2 = mk_move(Dst, Src, []),
  {I2, Map1, Data}.

mk_move(Dst, Src, Tail) ->
  case hipe_ppc:is_temp(Src) of
    true -> [hipe_ppc:mk_pseudo_move(Dst, Src) | Tail];
    _ -> mk_li(Dst, Src, Tail)
  end.

conv_return(I, Map, Data) ->
  %% TODO: multiple-value returns
  {[Arg], Map0} = conv_src_list(hipe_rtl:return_varlist(I), Map),
  I2 = mk_move(mk_rv(), Arg,
	       [hipe_ppc:mk_blr()]),
  {I2, Map0, Data}.

conv_store(I, Map, Data) ->
  {Base1, Map0} = conv_src(hipe_rtl:store_base(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:store_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:store_offset(I), Map1),
  StoreSize = hipe_rtl:store_size(I),
  I2 = mk_store(Src, Base1, Base2, StoreSize),
  {I2, Map2, Data}.

mk_store(Src, Base1, Base2, StoreSize) ->
  StOp =
    case StoreSize of
      byte -> 'stb';
      int16 -> 'sth';
      int32 -> 'stw';
      word -> hipe_ppc:stop_word()
    end,
  case hipe_ppc:is_temp(Src) of
    true ->
      mk_store2(Src, Base1, Base2, StOp);
    _ ->
      Tmp = new_untagged_temp(),
      mk_li(Tmp, Src,
	    mk_store2(Tmp, Base1, Base2, StOp))
  end.

mk_store2(Src, Base1, Base2, StOp) ->
  case hipe_ppc:is_temp(Base1) of
    true ->
      case hipe_ppc:is_temp(Base2) of
	true ->
	  mk_store_rr(Src, Base1, Base2, StOp);
	_ ->
	  mk_store_ri(Src, Base1, Base2, StOp)
      end;
    _ ->
      case hipe_ppc:is_temp(Base2) of
	true ->
	  mk_store_ri(Src, Base2, Base1, StOp);
	_ ->
	  mk_store_ii(Src, Base1, Base2, StOp)
      end
  end.

mk_store_ii(Src, Base, Disp, StOp) ->
  Tmp = new_untagged_temp(),
  mk_li(Tmp, Base,
	mk_store_ri(Src, Tmp, Disp, StOp)).

mk_store_ri(Src, Base, Disp, StOp) ->
  hipe_ppc:mk_store(StOp, Src, Disp, Base, 'new', []).

mk_store_rr(Src, Base1, Base2, StOp) ->
  StxOp = hipe_ppc:stop_to_stxop(StOp),
  [hipe_ppc:mk_storex(StxOp, Src, Base1, Base2)].

conv_switch(I, Map, Data) ->
  Labels = hipe_rtl:switch_labels(I),
  LMap = [{label,L} || L <- Labels],
  {NewData, JTabLab} =
    case hipe_rtl:switch_sort_order(I) of
      [] ->
	hipe_consttab:insert_block(Data, word, LMap);
      SortOrder ->
	hipe_consttab:insert_sorted_block(
	  Data, word, LMap, SortOrder)
    end,
  %% no immediates allowed here
  {IndexR, Map1} = conv_dst(hipe_rtl:switch_src(I), Map),
  JTabR = new_untagged_temp(),
  OffsetR = new_untagged_temp(),
  DestR = new_untagged_temp(),
  ShiftInstruction =
    case get(hipe_target_arch) of
      powerpc -> 'slwi';
      ppc64 -> 'sldi'
    end,
  I2 =
    [hipe_ppc:mk_pseudo_li(JTabR, {JTabLab,constant}),
     hipe_ppc:mk_alu(ShiftInstruction, OffsetR, IndexR,
		     hipe_ppc:mk_uimm16(log2_word_size())),
     hipe_ppc:mk_loadx(hipe_ppc:ldop_wordx(), DestR, JTabR, OffsetR),
     hipe_ppc:mk_mtspr('ctr', DestR),
     hipe_ppc:mk_bctr(Labels)],
  {I2, Map1, NewData}.

%%% Create a conditional branch.
%%% If the condition tests CR0[SO], rewrite the path
%%% corresponding to SO being set to clear XER[SO].

mk_pseudo_bc(BCond, TrueLabel, FalseLabel, Pred) ->
  case BCond of
    'so' ->
      NewTrueLabel = hipe_gensym:get_next_label(ppc),
      ZeroR = new_untagged_temp(),
      [hipe_ppc:mk_pseudo_bc(BCond, NewTrueLabel, FalseLabel, Pred),
       hipe_ppc:mk_label(NewTrueLabel) |
       mk_li(ZeroR, 0,
	     [hipe_ppc:mk_mtspr('xer', ZeroR),
	      hipe_ppc:mk_b_label(TrueLabel)])];
    'ns' ->
      NewFalseLabel = hipe_gensym:get_next_label(ppc),
      ZeroR = new_untagged_temp(),
      [hipe_ppc:mk_pseudo_bc(BCond, TrueLabel, NewFalseLabel, Pred),
       hipe_ppc:mk_label(NewFalseLabel) |
       mk_li(ZeroR, 0,
	     [hipe_ppc:mk_mtspr('xer', ZeroR),
	      hipe_ppc:mk_b_label(FalseLabel)])];
    _ ->
      [hipe_ppc:mk_pseudo_bc(BCond, TrueLabel, FalseLabel, Pred)]
  end.

%%% Load an integer constant into a register.

mk_li(Dst, Value) -> mk_li(Dst, Value, []).

mk_li(Dst, Value, Tail) ->
  hipe_ppc:mk_li(Dst, Value, Tail).

%%% Check if an RTL ALU or ALUB operator commutes.

rtl_aluop_commutes(RtlAluOp) ->
  case RtlAluOp of
    'add' -> true;
    'mul' -> true;
    'or'  -> true;
    'and' -> true;
    'xor' -> true;
    _	  -> false
  end.

%%% Split a list of formal or actual parameters into the
%%% part passed in registers and the part passed on the stack.
%%% The parameters passed in registers are also tagged with
%%% the corresponding registers.

split_args(Args) ->
  split_args(0, hipe_ppc_registers:nr_args(), Args, []).

split_args(I, N, [Arg|Args], RegArgs) when I < N ->
  Reg = hipe_ppc_registers:arg(I),
  Temp = hipe_ppc:mk_temp(Reg, 'tagged'),
  split_args(I+1, N, Args, [{Arg,Temp}|RegArgs]);
split_args(_, _, StkArgs, RegArgs) ->
  {RegArgs, StkArgs}.

%%% Convert a list of actual parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_actuals([{Src,Dst}|Actuals], Rest) ->
  move_actuals(Actuals, mk_move(Dst, Src, Rest));
move_actuals([], Rest) ->
  Rest.

%%% Convert a list of formal parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_formals([{Dst,Src}|Formals], Rest) ->
  move_formals(Formals, [hipe_ppc:mk_pseudo_move(Dst, Src) | Rest]);
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
	      {hipe_ppc:mk_prim(Fun), Map};
	     true ->
	      {conv_mfa(Fun), Map}
	  end
      end
  end.

%%% Convert an MFA operand.

conv_mfa({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
  hipe_ppc:mk_mfa(M, F, A).

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
  case hipe_rtl:is_fpreg(Opnd) of
    true -> conv_dst(Opnd, Map)
  end.

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
      'double' -> hipe_ppc_registers:is_precoloured_fpr(Name);
      _ -> hipe_ppc_registers:is_precoloured_gpr(Name)
    end,
  case IsPrecoloured of
    true ->
      {hipe_ppc:mk_temp(Name, Type), Map};
    false ->
      case vmap_lookup(Map, Opnd) of
	{value, NewTemp} ->
	  {NewTemp, Map};
	_ ->
	  NewTemp = hipe_ppc:mk_new_temp(Type),
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
  conv_formals(hipe_ppc_registers:nr_args(), Os, Map, []).

conv_formals(N, [O|Os], Map, Res) ->
  Type =
    case hipe_rtl:is_var(O) of
      true -> 'tagged';
      _ -> 'untagged'
    end,
  Dst =
    if N > 0 -> hipe_ppc:mk_new_temp(Type);	% allocatable
       true -> hipe_ppc:mk_new_nonallocatable_temp(Type)
    end,
  Map1 = vmap_bind(Map, O, Dst),
  conv_formals(N-1, Os, Map1, [Dst|Res]);
conv_formals(_, [], Map, Res) ->
  {lists:reverse(Res), Map}.

%%% Create a temp representing the stack pointer register.

mk_sp() ->
  hipe_ppc:mk_temp(hipe_ppc_registers:stack_pointer(), 'untagged').

%%% Create a temp representing the return value register.

mk_rv() ->
  hipe_ppc:mk_temp(hipe_ppc_registers:return_value(), 'tagged').

%%% new_untagged_temp -- conjure up an untagged scratch reg

new_untagged_temp() ->
  hipe_ppc:mk_new_temp('untagged').

%%% new_tagged_temp -- conjure up a tagged scratch reg

new_tagged_temp() ->
  hipe_ppc:mk_new_temp('tagged').

%%% Map from RTL var/reg operands to temps.

vmap_empty() ->
  gb_trees:empty().

vmap_lookup(Map, Key) ->
  gb_trees:lookup(Key, Map).

vmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).

word_size() ->
  hipe_rtl_arch:word_size().

log2_word_size() ->
  hipe_rtl_arch:log2_word_size().
