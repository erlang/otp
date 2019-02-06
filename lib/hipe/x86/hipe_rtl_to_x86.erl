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
%%% Translate 3-address RTL code to 2-address pseudo-x86 code.

-ifdef(HIPE_AMD64).
-define(HIPE_RTL_TO_X86,	hipe_rtl_to_amd64).
-define(HIPE_X86_REGISTERS,	hipe_amd64_registers).
-define(ECX,			rcx).
-define(EAX,			rax).
-else.
-define(HIPE_RTL_TO_X86,	hipe_rtl_to_x86).
-define(HIPE_X86_REGISTERS,	hipe_x86_registers).
-define(ECX,			ecx).
-define(EAX,			eax).
-endif.

-module(?HIPE_RTL_TO_X86).
-export([translate/1]).

-include("../rtl/hipe_rtl.hrl").

translate(RTL) ->	% RTL function -> x86 defun
  hipe_gensym:init(x86),
  hipe_gensym:set_var(x86, ?HIPE_X86_REGISTERS:first_virtual()),
  hipe_gensym:set_label(x86, hipe_gensym:get_label(rtl)),
  Map0 = vmap_empty(),
  {Formals, Map1} = conv_formals(hipe_rtl:rtl_params(RTL), Map0),
  OldData = hipe_rtl:rtl_data(RTL),
  {Code0, NewData} = conv_insn_list(hipe_rtl:rtl_code(RTL), Map1, OldData),
  {RegFormals,_} = split_args(Formals),
  Code =
    case RegFormals of
      [] -> Code0;
      _ -> [hipe_x86:mk_label(hipe_gensym:get_next_label(x86)) |
	    move_formals(RegFormals, Code0)]
    end,
  IsClosure = hipe_rtl:rtl_is_closure(RTL),
  IsLeaf = hipe_rtl:rtl_is_leaf(RTL),
  hipe_x86:mk_defun(hipe_rtl:rtl_fun(RTL),
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
    #alu{} ->
      %% dst = src1 binop src2
      BinOp = conv_binop(hipe_rtl:alu_op(I)),
      {Dst, Map0} = conv_dst(hipe_rtl:alu_dst(I), Map),
      {FixSrc1, Src1, Map1} = conv_src(hipe_rtl:alu_src1(I), Map0),
      {FixSrc2, Src2, Map2} = conv_src(hipe_rtl:alu_src2(I), Map1),
      I2 =
	case hipe_rtl:is_shift_op(hipe_rtl:alu_op(I)) of
	  true ->
	    conv_shift(Dst, Src1, BinOp, Src2);
	  false ->
	    conv_alu_nocc(Dst, Src1, BinOp, Src2, [])
	end,
      {FixSrc1++FixSrc2++I2, Map2, Data};
    #alub{} ->
      %% dst = src1 op src2; if COND goto label
      BinOp = conv_binop(hipe_rtl:alub_op(I)),
      {FixSrc1, Src1, Map0} = conv_src(hipe_rtl:alub_src1(I), Map),
      {FixSrc2, Src2, Map1} = conv_src(hipe_rtl:alub_src2(I), Map0),
      Cc = conv_cond(hipe_rtl:alub_cond(I)),
      BranchOp = conv_branchop(BinOp),
      HasDst = hipe_rtl:alub_has_dst(I),
      {I2, Map3} =
	case (not HasDst) andalso BranchOp =/= none of
	  true ->
	    {conv_branch(Src1, BranchOp, Src2, Cc,
			 hipe_rtl:alub_true_label(I),
			 hipe_rtl:alub_false_label(I),
			 hipe_rtl:alub_pred(I)), Map1};
	  false ->
	    {Dst, Map2} =
	      case HasDst of
		false -> {new_untagged_temp(), Map1};
		true -> conv_dst(hipe_rtl:alub_dst(I), Map1)
	      end,
	    I1 = [hipe_x86:mk_pseudo_jcc(Cc,
					 hipe_rtl:alub_true_label(I),
					 hipe_rtl:alub_false_label(I),
					 hipe_rtl:alub_pred(I))],
	    {conv_alu(Dst, Src1, BinOp, Src2, I1), Map2}
	end,
      {FixSrc1++FixSrc2++I2, Map3, Data};
    #call{} ->
      %%	push <arg1>
      %%	...
      %%	push <argn>
      %%	eax := call <Fun>; if exn goto <Fail> else goto Next
      %% Next:
      %%	<Dst> := eax
      %%	goto <Cont>
      {FixArgs, Args, Map0} = conv_src_list(hipe_rtl:call_arglist(I), Map),
      {Dsts, Map1} = conv_dst_list(hipe_rtl:call_dstlist(I), Map0),
      {Fun, Map2} = conv_fun(hipe_rtl:call_fun(I), Map1),
      I2 = conv_call(Dsts, Fun, Args,
		     hipe_rtl:call_continuation(I),
		     hipe_rtl:call_fail(I),
		     hipe_rtl:call_type(I)),
      {FixArgs++I2, Map2, Data};
    #comment{} ->
      I2 = [hipe_x86:mk_comment(hipe_rtl:comment_text(I))],
      {I2, Map, Data};
    #enter{} ->
      {FixArgs, Args, Map0} = conv_src_list(hipe_rtl:enter_arglist(I), Map),
      {Fun, Map1} = conv_fun(hipe_rtl:enter_fun(I), Map0),
      I2 = conv_tailcall(Fun, Args, hipe_rtl:enter_type(I)),
      {FixArgs++I2, Map1, Data};
    #goto{} ->
      I2 = [hipe_x86:mk_jmp_label(hipe_rtl:goto_label(I))],
      {I2, Map, Data};
    #label{} ->
      I2 = [hipe_x86:mk_label(hipe_rtl:label_name(I))],
      {I2, Map, Data};
    #load{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:load_dst(I), Map),
      {FixSrc, Src, Map1} = conv_src_noimm(hipe_rtl:load_src(I), Map0),
      {FixOff, Off, Map2} = conv_src(hipe_rtl:load_offset(I), Map1),
      I2 = case {hipe_rtl:load_size(I), hipe_rtl:load_sign(I)} of
	     {byte, signed} ->
	       [hipe_x86:mk_movsx(hipe_x86:mk_mem(Src, Off, 'byte'), Dst)];
	     {byte, unsigned} ->
	       [hipe_x86:mk_movzx(hipe_x86:mk_mem(Src, Off, 'byte'), Dst)];
	     {int16, signed} ->
	       [hipe_x86:mk_movsx(hipe_x86:mk_mem(Src, Off, 'int16'), Dst)];
	     {int16, unsigned} ->
	       [hipe_x86:mk_movzx(hipe_x86:mk_mem(Src, Off, 'int16'), Dst)];
	     {LoadSize, LoadSign} ->
	       mk_load(LoadSize, LoadSign, Src, Off, Dst)
	   end,
      {FixSrc++FixOff++I2, Map2, Data};
    #load_address{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:load_address_dst(I), Map),
      Addr = hipe_rtl:load_address_addr(I),
      Type = hipe_rtl:load_address_type(I),
      Src = hipe_x86:mk_imm_from_addr(Addr, Type),
      I2 = mk_load_address(Type, Src, Dst),
      {I2, Map0, Data};
    #load_atom{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:load_atom_dst(I), Map),
      Src = hipe_x86:mk_imm_from_atom(hipe_rtl:load_atom_atom(I)),
      I2 = [hipe_x86:mk_move(Src, Dst)],
      {I2, Map0, Data};
    #move{src=Dst, dst=Dst} -> {[], Map, Data};
    #move{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:move_dst(I), Map),
      {FixSrc, Src, Map1} = conv_src(hipe_rtl:move_src(I), Map0),
      I2 = [hipe_x86:mk_move(Src, Dst)],
      {FixSrc++I2, Map1, Data};
    #return{} ->
      {FixArgs, Args, Map0} = conv_src_list(hipe_rtl:return_varlist(I), Map),
      %% frame will fill in npop later, hence the "mk_ret(-1)"
      I2 = move_retvals(Args, [hipe_x86:mk_ret(-1)]),
      {FixArgs++I2, Map0, Data};
    #store{} ->
      {FixPtr, Ptr, Map0} = conv_src_noimm(hipe_rtl:store_base(I), Map),
      {FixSrc, Src, Map1} = conv_src(hipe_rtl:store_src(I), Map0),
      {FixOff, Off, Map2} = conv_src(hipe_rtl:store_offset(I), Map1),
      I2 = mk_store(hipe_rtl:store_size(I), Src, Ptr, Off),
      {FixPtr++FixSrc++FixOff++I2, Map2, Data};
    #switch{} ->	% this one also updates Data :-(
      %% from hipe_rtl2sparc, but we use a hairy addressing mode
      %% instead of doing the arithmetic manually
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
      {Index, Map1} = conv_dst(hipe_rtl:switch_src(I), Map),
      I2 = mk_jmp_switch(Index, JTabLab, Labels),
      {I2, Map1, NewData};
    #fload{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:fload_dst(I), Map),
      {[], Src, Map1} = conv_src_noimm(hipe_rtl:fload_src(I), Map0),
      {[], Off, Map2} = conv_src(hipe_rtl:fload_offset(I), Map1),
      I2 = [hipe_x86:mk_fmove(hipe_x86:mk_mem(Src, Off, 'double'),Dst)],
      {I2, Map2, Data};
    #fstore{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:fstore_base(I), Map),
      {[], Src, Map1} = conv_src(hipe_rtl:fstore_src(I), Map0),
      {[], Off, Map2} = conv_src(hipe_rtl:fstore_offset(I), Map1),
      I2 = [hipe_x86:mk_fmove(Src, hipe_x86:mk_mem(Dst, Off, 'double'))],
      {I2, Map2, Data};
    #fp{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:fp_dst(I), Map),
      {[], Src1, Map1} = conv_src(hipe_rtl:fp_src1(I), Map0),
      {[], Src2, Map2} = conv_src(hipe_rtl:fp_src2(I), Map1),
      FpBinOp = conv_fp_binop(hipe_rtl:fp_op(I)),
      I2 = conv_fp_binary(Dst, Src1, FpBinOp, Src2),
      {I2, Map2, Data};
    #fp_unop{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:fp_unop_dst(I), Map),
      {[], Src, Map1} = conv_src(hipe_rtl:fp_unop_src(I), Map0),
      FpUnOp = conv_fp_unop(hipe_rtl:fp_unop_op(I)),
      I2 = conv_fp_unary(Dst, Src, FpUnOp),
      {I2, Map1, Data};
    #fmove{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:fmove_dst(I), Map),
      {[], Src, Map1} = conv_src(hipe_rtl:fmove_src(I), Map0),
      I2 = [hipe_x86:mk_fmove(Src, Dst)],
      {I2, Map1, Data};
    #fconv{} ->
      {Dst, Map0} = conv_dst(hipe_rtl:fconv_dst(I), Map),
      {[], Src, Map1} = conv_src(hipe_rtl:fconv_src(I), Map0),
      I2 = conv_fconv(Dst, Src),
      {I2, Map1, Data};
    X ->
      %% gctest??
      %% jmp, jmp_link, jsr, esr, multimove,
      %% stackneed, pop_frame, restore_frame, save_frame
      throw({?MODULE, {"unknown RTL instruction", X}})
  end.

%%% Finalise the conversion of a 3-address ALU operation, taking
%%% care to not introduce more temps and moves than necessary.

conv_alu_nocc(Dst, Src1, 'add', Src2, Tail) ->
  case (not same_opnd(Dst, Src1)) andalso (not same_opnd(Dst, Src2))
    %% We could use orelse instead of xor here to generate lea T1(T2), T3, but
    %% they seem to move coalesce so well that move+add is better for them.
    andalso (hipe_x86:is_temp(Src1) xor hipe_x86:is_temp(Src2))
  of
    false -> conv_alu(Dst, Src1, 'add', Src2, Tail);
    true -> % Use LEA
      Type = typeof_dst(Dst),
      Mem = case hipe_x86:is_temp(Src1) of
	      true  -> hipe_x86:mk_mem(Src1, Src2, Type);
	      false -> hipe_x86:mk_mem(Src2, Src1, Type)
	    end,
      [hipe_x86:mk_lea(Mem, Dst) | Tail]
  end;
conv_alu_nocc(Dst, Src1, 'sub', Src2, Tail) ->
  case (not same_opnd(Dst, Src1)) andalso hipe_x86:is_temp(Src1)
    andalso (not hipe_x86:is_temp(Src2))
  of
    false -> conv_alu(Dst, Src1, 'sub', Src2, Tail);
    true -> % Use LEA
      Imm = hipe_x86:mk_imm(-hipe_x86:imm_value(Src2)),
      Mem = hipe_x86:mk_mem(Src1, Imm, typeof_dst(Dst)),
      [hipe_x86:mk_lea(Mem, Dst) | Tail]
  end;
conv_alu_nocc(Dst, Src1, BinOp, Src2, Tail) ->
  conv_alu(Dst, Src1, BinOp, Src2, Tail).

conv_alu(Dst, Src1, 'imul', Src2, Tail) ->
  mk_imul(Src1, Src2, Dst, Tail);
conv_alu(Dst, Src1, BinOp, Src2, Tail) ->
  case same_opnd(Dst, Src1) of
    true ->		% x = x op y
      [hipe_x86:mk_alu(BinOp, Src2, Dst) | Tail];	% x op= y
    false ->		% z = x op y, where z != x
      case same_opnd(Dst, Src2) of
	false ->	% z = x op y, where z != x && z != y
	  [hipe_x86:mk_move(Src1, Dst),			% z = x
	   hipe_x86:mk_alu(BinOp, Src2, Dst) | Tail];	% z op= y
	true ->		% y = x op y, where y != x
	  case binop_commutes(BinOp) of
	    true ->	% y = y op x
	      [hipe_x86:mk_alu(BinOp, Src1, Dst) | Tail]; % y op= x
	    false ->	% y = x op y, where op doesn't commute
	      Tmp = clone_dst(Dst),
	      [hipe_x86:mk_move(Src1, Tmp),		% t = x
	       hipe_x86:mk_alu(BinOp, Src2, Tmp),	% t op= y
	       hipe_x86:mk_move(Tmp, Dst) | Tail]	% y = t
	  end
      end
  end.

mk_imul(Src1, Src2, Dst, Tail) ->
  case hipe_x86:is_imm(Src1) of
    true ->
      case hipe_x86:is_imm(Src2) of
	true ->
	  mk_imul_iit(Src1, Src2, Dst, Tail);
	_ ->
	  mk_imul_itt(Src1, Src2, Dst, Tail)
      end;
    _ ->
      case hipe_x86:is_imm(Src2) of
	true ->
	  mk_imul_itt(Src2, Src1, Dst, Tail);
	_ ->
	  mk_imul_ttt(Src1, Src2, Dst, Tail)
      end
  end.

mk_imul_iit(Src1, Src2, Dst, Tail) ->
  io:format("~w: RTL mul with two immediates\n", [?MODULE]),
  Tmp2 = new_untagged_temp(),
  [hipe_x86:mk_move(Src2, Tmp2) |
   mk_imul_itt(Src1, Tmp2, Dst, Tail)].

mk_imul_itt(Src1, Src2, Dst, Tail) ->
  [hipe_x86:mk_imul(Src1, Src2, Dst) | Tail].

mk_imul_ttt(Src1, Src2, Dst, Tail) ->
  case same_opnd(Dst, Src1) of
    true ->
      [hipe_x86:mk_imul([], Src2, Dst) | Tail];
    false ->
      case same_opnd(Dst, Src2) of
	true ->
	  [hipe_x86:mk_imul([], Src1, Dst) | Tail];
	false ->
	  [hipe_x86:mk_move(Src1, Dst),
	   hipe_x86:mk_imul([], Src2, Dst) | Tail]
      end
  end.

conv_shift(Dst, Src1, BinOp, Src2) ->
  {NewSrc2,I1} =
    case hipe_x86:is_imm(Src2) of
      true ->
	{Src2, []};
      false ->
	NewSrc = hipe_x86:mk_temp(?HIPE_X86_REGISTERS:?ECX(), 'untagged'),
	{NewSrc, [hipe_x86:mk_move(Src2, NewSrc)]}
    end,
  I2 = case same_opnd(Dst, Src1) of
	 true ->	% x = x op y
	   [hipe_x86:mk_shift(BinOp, NewSrc2, Dst)];	% x op= y
	 false ->	% z = x op y, where z != x
	   case same_opnd(Dst, Src2) of
	     false ->	% z = x op y, where z != x && z != y
	       [hipe_x86:mk_move(Src1, Dst),		% z = x
		hipe_x86:mk_shift(BinOp, NewSrc2, Dst)];% z op= y
	     true ->	% y = x op y, no shift op commutes
	       Tmp = clone_dst(Dst),
	       [hipe_x86:mk_move(Src1, Tmp),		% t = x
		hipe_x86:mk_shift(BinOp, NewSrc2, Tmp),	% t op= y
		hipe_x86:mk_move(Tmp, Dst)]		% y = t
	   end
       end,
  I1 ++ I2.

%%% Finalise the conversion of a conditional branch operation, taking
%%% care to not introduce more temps and moves than necessary.

conv_branchop('sub') -> 'cmp';
conv_branchop('and') ->  'test';
conv_branchop(_) -> none.

branchop_commutes('cmp') -> false;
branchop_commutes('test') -> true.

conv_branch(Src1, Op, Src2, Cc, TrueLab, FalseLab, Pred) ->
  case hipe_x86:is_imm(Src1) of
    false ->
      mk_branch(Src1, Op, Src2, Cc, TrueLab, FalseLab, Pred);
    true ->
      case hipe_x86:is_imm(Src2) of
	false ->
	  NewCc = case branchop_commutes(Op) of
		    true -> Cc;
		    false -> commute_cc(Cc)
		  end,
	  mk_branch(Src2, Op, Src1, NewCc, TrueLab, FalseLab, Pred);
	true ->
	  %% two immediates, let the optimiser clean it up
	  Tmp = new_untagged_temp(),
	  [hipe_x86:mk_move(Src1, Tmp) |
	   mk_branch(Tmp, Op, Src2, Cc, TrueLab, FalseLab, Pred)]
      end
  end.

mk_branch(Src1, Op, Src2, Cc, TrueLab, FalseLab, Pred) ->
  %% PRE: not(is_imm(Src1))
  [mk_branchtest(Src1, Op, Src2),
   hipe_x86:mk_pseudo_jcc(Cc, TrueLab, FalseLab, Pred)].

mk_branchtest(Src1, cmp, Src2) -> hipe_x86:mk_cmp(Src2, Src1);
mk_branchtest(Src1, test, Src2) -> hipe_x86:mk_test(Src2, Src1).

%%% Convert an RTL ALU or ALUB binary operator.

conv_binop(BinOp) ->
  case BinOp of
    'add'	-> 'add';
    'sub'	-> 'sub';
    'or'	-> 'or';
    'and'	-> 'and';
    'xor'	-> 'xor';
    'sll'	-> 'shl';
    'srl'	-> 'shr';
    'sra'	-> 'sar';
    'mul'	-> 'imul';
    %% andnot ???
    _		-> exit({?MODULE, {"unknown binop", BinOp}})
  end.

binop_commutes(BinOp) ->
  case BinOp of
    'add'	-> true;
    'or'	-> true;
    'and'	-> true;
    'xor'	-> true;
    _		-> false
  end.

%%% Convert an RTL conditional operator.

conv_cond(Cond) ->
  case Cond of
    eq	-> 'e';
    ne	-> 'ne';
    gt	-> 'g';
    gtu	-> 'a';
    ge	-> 'ge';
    geu	-> 'ae';
    lt	-> 'l';
    ltu	-> 'b';
    le	-> 'le';
    leu	-> 'be';
    overflow -> 'o';
    not_overflow -> 'no';
    _	-> exit({?MODULE, {"unknown rtl cond", Cond}})
  end.

commute_cc(Cc) ->	% if x Cc y, then y commute_cc(Cc) x
  case Cc of
    'e'	-> 'e';		% ==, ==
    'ne' -> 'ne';	% !=, !=
    'g'	-> 'l';		% >, <
    'a'	-> 'b';		% >u, <u
    'ge' -> 'le';	% >=, <=
    'ae' -> 'be';	% >=u, <=u
    'l'	-> 'g';		% <, >
    'b'	-> 'a';		% <u, >u
    'le' -> 'ge';	% <=, >=
    'be' -> 'ae';	% <=u, >=u
    %% overflow/not_overflow: n/a
    _	-> exit({?MODULE, {"unknown cc", Cc}})
  end.

%%% Test if Dst and Src are the same operand.

same_opnd(Dst, Src) -> Dst =:= Src.

%%% Finalise the conversion of a tailcall instruction.

conv_tailcall(Fun, Args, Linkage) ->
  Arity = length(Args),
  {RegArgs,StkArgs} = split_args(Args),
  move_actuals(RegArgs,
	       [hipe_x86:mk_pseudo_tailcall_prepare(),
		hipe_x86:mk_pseudo_tailcall(Fun, Arity, StkArgs, Linkage)]).

split_args(Args) ->
  split_args(0, ?HIPE_X86_REGISTERS:nr_args(), Args, []).
split_args(I, N, [Arg|Args], RegArgs) when I < N ->
  Reg = ?HIPE_X86_REGISTERS:arg(I),
  Temp = hipe_x86:mk_temp(Reg, 'tagged'),
  split_args(I+1, N, Args, [{Arg,Temp}|RegArgs]);
split_args(_, _, StkArgs, RegArgs) ->
  {RegArgs, StkArgs}.

move_actuals([], Rest) -> Rest;
move_actuals([{Src,Dst}|Actuals], Rest) ->
  move_actuals(Actuals, [hipe_x86:mk_move(Src, Dst) | Rest]).

move_formals([], Rest) -> Rest;
move_formals([{Dst,Src}|Formals], Rest) ->
  move_formals(Formals, [hipe_x86:mk_move(Src, Dst) | Rest]).

%%% Finalise the conversion of a call instruction.

conv_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  case hipe_x86:is_prim(Fun) of
    true ->
      conv_primop_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage);
    false ->
      conv_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage)
  end.

conv_primop_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage) ->
  case hipe_x86:prim_prim(Prim) of
    'fwait' ->
      conv_fwait_call(Dsts, Args, ContLab, ExnLab, Linkage);
    _ ->
      conv_general_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage)
  end.

conv_fwait_call([], [], [], [], not_remote) ->
  [hipe_x86:mk_fp_unop('fwait', [])].

conv_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  %% The backend does not support pseudo_calls without a
  %% continuation label, so we make sure each call has one.
  {RealContLab, Tail} =
    case do_call_results(Dsts) of
      [] ->
	%% Avoid consing up a dummy basic block if the moves list
	%% is empty, as is typical for calls to suspend/0.
	%% This should be subsumed by a general "optimise the CFG"
	%% module, and could probably be removed.
	case ContLab of
	  [] ->
	    NewContLab = hipe_gensym:get_next_label(x86),
	    {NewContLab, [hipe_x86:mk_label(NewContLab)]};
	  _ ->
	    {ContLab, []}
	end;
      Moves ->
	%% Change the call to continue at a new basic block.
	%% In this block move the result registers to the Dsts,
	%% then continue at the call's original continuation.
	%%
	%% This should be fixed to propagate "fallthrough calls"
	%% When the rest of the backend supports them.
	NewContLab = hipe_gensym:get_next_label(x86),
	case ContLab of
	  [] ->
	    %% This is just a fallthrough
	    %% No jump back after the moves.
	    {NewContLab,
	     [hipe_x86:mk_label(NewContLab) |
	      Moves]};
	  _ ->
	    %% The call has a continuation
	    %% jump to it.
	    {NewContLab,
	     [hipe_x86:mk_label(NewContLab) |
	      Moves ++
	      [hipe_x86:mk_jmp_label(ContLab)]]}
	end
    end,
  SDesc = hipe_x86:mk_sdesc(ExnLab, 0, length(Args), {}),
  CallInsn = hipe_x86:mk_pseudo_call(Fun, SDesc, RealContLab, Linkage),
  {RegArgs,StkArgs} = split_args(Args),
  do_push_args(StkArgs, move_actuals(RegArgs, [CallInsn | Tail])).

do_push_args([Arg|Args], Tail) ->
  [hipe_x86:mk_push(Arg) | do_push_args(Args, Tail)];
do_push_args([], Tail) ->
  Tail.

%%% Move return values from the return value registers.

do_call_results(DstList) ->
  do_call_results(DstList, 0, []).

do_call_results([Dst|DstList], I, Rest) ->
  Src = hipe_x86:mk_temp(?HIPE_X86_REGISTERS:ret(I), 'tagged'),
  Move = hipe_x86:mk_move(Src, Dst),
  do_call_results(DstList, I+1, [Move|Rest]);
do_call_results([], _, Insns) -> Insns.

%%% Move return values to the return value registers.

move_retvals(SrcLst, Rest) ->
  move_retvals(SrcLst, 0, Rest).

move_retvals([Src|SrcLst], I, Rest) ->
  Dst = hipe_x86:mk_temp(?HIPE_X86_REGISTERS:ret(I), 'tagged'),
  Move = hipe_x86:mk_move(Src, Dst),
  move_retvals(SrcLst, I+1, [Move|Rest]);
move_retvals([], _, Insns) -> Insns.

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
	  case Fun of
	    Prim when is_atom(Prim) ->
	      {hipe_x86:mk_prim(Prim), Map};
	    {M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	      {hipe_x86:mk_mfa(M,F,A), Map};
	    _ ->
	      exit({?MODULE,conv_fun,Fun})
	  end
      end
  end.

conv_src_noimm(Opnd, Map) ->
  R={FixSrc0, Src, NewMap} = conv_src(Opnd, Map),
  case hipe_x86:is_imm(Src) of
    false -> R;
    true ->
      Tmp = new_untagged_temp(),
      {FixSrc0 ++ [hipe_x86:mk_move(Src, Tmp)],
       Tmp, NewMap}
  end.

%%% Convert an RTL source operand (imm/var/reg).

conv_src(Opnd, Map) ->
  case hipe_rtl:is_imm(Opnd) of
    true ->
      conv_imm(Opnd, Map);
    false ->
      {NewOpnd,NewMap} = conv_dst(Opnd, Map),
      {[], NewOpnd, NewMap}
  end.

-ifdef(HIPE_AMD64).
conv_imm(Opnd, Map) ->
  ImmVal = hipe_rtl:imm_value(Opnd),
  case is_imm64(ImmVal) of
    true ->
      Temp = hipe_x86:mk_new_temp('untagged'),
      {[hipe_x86:mk_move64(hipe_x86:mk_imm(ImmVal), Temp)], Temp, Map};
    false ->
      {[], hipe_x86:mk_imm(ImmVal), Map}
  end.

is_imm64(Value) when is_integer(Value) ->
  (Value < -(1 bsl (32 - 1))) or (Value > (1 bsl (32 - 1)) - 1);
is_imm64({_,atom})    -> false; % Atoms are 32 bits.
is_imm64({_,c_const}) -> true;  % c_consts are 64 bits.
is_imm64({_,_})       -> true . % Other relocs are 64 bits.
-else.
conv_imm(Opnd, Map) ->
  {[], hipe_x86:mk_imm(hipe_rtl:imm_value(Opnd)), Map}.
-endif.

conv_src_list([O|Os], Map) ->
  {NewInstr, V, Map1} = conv_src(O, Map),
  {Instrs, Vs, Map2} = conv_src_list(Os, Map1),
  {Instrs++NewInstr, [V|Vs], Map2};
conv_src_list([], Map) ->
  {[], [], Map}.

%%% Convert an RTL destination operand (var/reg).

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
  case ?HIPE_X86_REGISTERS:is_precoloured(Name) of
    true ->
      case ?HIPE_X86_REGISTERS:proc_offset(Name) of
	false ->
	  {hipe_x86:mk_temp(Name, Type), Map};
	Offset ->
	  Preg = ?HIPE_X86_REGISTERS:proc_pointer(),
	  Pbase = hipe_x86:mk_temp(Preg, 'untagged'),
	  Poff = hipe_x86:mk_imm(Offset),
	  {hipe_x86:mk_mem(Pbase, Poff, Type), Map}
      end;
    false ->
      case vmap_lookup(Map, Opnd) of
	{value, NewTemp} ->
	  {NewTemp, Map};
	_ ->
	  NewTemp = hipe_x86:mk_new_temp(Type),
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
  conv_formals(?HIPE_X86_REGISTERS:nr_args(), Os, Map, []).

conv_formals(N, [O|Os], Map, Res) ->
  Type =
    case hipe_rtl:is_var(O) of
      true -> 'tagged';
      false ->'untagged'
    end,
  Dst =
    if N > 0 -> hipe_x86:mk_new_temp(Type);	% allocatable
       true -> hipe_x86:mk_new_nonallocatable_temp(Type)
    end,
  Map1 = vmap_bind(Map, O, Dst),
  conv_formals(N-1, Os, Map1, [Dst|Res]);
conv_formals(_, [], Map, Res) ->
  {lists:reverse(Res), Map}.

%%% typeof_src -- what's src's type?

typeof_src(Src) ->
  case hipe_x86:is_imm(Src) of
    true ->
      'untagged';
    _ ->
      typeof_dst(Src)
  end.

%%% typeof_dst -- what's dst's type?

typeof_dst(Dst) ->
  case hipe_x86:is_temp(Dst) of
    true ->
      hipe_x86:temp_type(Dst);
    _ ->
      hipe_x86:mem_type(Dst)
  end.

%%% clone_dst -- conjure up a scratch reg with same type as dst

clone_dst(Dst) ->
  hipe_x86:mk_new_temp(typeof_dst(Dst)).

%%% new_untagged_temp -- conjure up an untagged scratch reg

new_untagged_temp() ->
  hipe_x86:mk_new_temp('untagged').

%%% Map from RTL var/reg operands to x86 temps.

vmap_empty() ->
  gb_trees:empty().

vmap_lookup(Map, Key) ->
  gb_trees:lookup(Key, Map).

vmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).

%%% Finalise the conversion of an Integer-to-Float operation.

conv_fconv(Dst, Src) ->
  case hipe_x86:is_imm(Src) of
    false ->
      [hipe_x86:mk_fmove(Src, Dst)];
    true ->
      %% cvtsi2sd does not allow src to be an immediate
      Tmp = new_untagged_temp(),
      [hipe_x86:mk_move(Src, Tmp),
       hipe_x86:mk_fmove(Tmp, Dst)]
  end.

%%% Finalise the conversion of a 2-address FP operation.

-ifdef(HIPE_AMD64).
conv_fp_unary(Dst, Src, 'fchs') ->
  Tmp = new_untagged_temp(),
  case same_opnd(Dst, Src) of
    true ->
      [];
    _ ->
      [hipe_x86:mk_fmove(Src, Dst)]
  end ++
    mk_load_address(c_const, hipe_x86:mk_imm({sse2_fnegate_mask, c_const}), Tmp) ++
    [hipe_x86:mk_fp_binop('xorpd', hipe_x86:mk_mem(Tmp, hipe_x86:mk_imm(0), double), Dst)].
-else.
conv_fp_unary(Dst, Src, FpUnOp) ->
  case same_opnd(Dst, Src) of
    true ->
      [hipe_x86:mk_fp_unop(FpUnOp, Dst)];
    _ ->
      [hipe_x86:mk_fmove(Src, Dst),
       hipe_x86:mk_fp_unop(FpUnOp, Dst)]
  end.
-endif.

conv_fp_unop(RtlFpUnOp) ->
  case RtlFpUnOp of
    'fchs' -> 'fchs'
  end.

%%% Finalise the conversion of a 3-address FP operation.

conv_fp_binary(Dst, Src1, FpBinOp, Src2) ->
  case same_opnd(Dst, Src1) of
    true ->		% x = x op y
      [hipe_x86:mk_fp_binop(FpBinOp, Src2, Dst)];		% x op= y
    false ->		% z = x op y, where z != x
      case same_opnd(Dst, Src2) of
	false ->	% z = x op y, where z != x && z != y
	  [hipe_x86:mk_fmove(Src1, Dst),			% z = x
	   hipe_x86:mk_fp_binop(FpBinOp, Src2, Dst)];		% z op= y
	true ->		% y = x op y, where y != x
	  case fp_binop_commutes(FpBinOp) of
	    true ->	% y = y op x
	      [hipe_x86:mk_fp_binop(FpBinOp, Src1, Dst)];	% y op= x
	    false ->	% y = x op y, where op doesn't commute
	      RevFpBinOp = reverse_fp_binop(FpBinOp),
	      [hipe_x86:mk_fp_binop(RevFpBinOp, Src1, Dst)]
	  end
      end
  end.

%%% Convert an RTL FP binary operator.

conv_fp_binop(RtlFpBinOp) ->
  case RtlFpBinOp of
    'fadd' -> 'fadd';
    'fdiv' -> 'fdiv';
    'fmul' -> 'fmul';
    'fsub' -> 'fsub'
  end.

fp_binop_commutes(FpBinOp) ->
  case FpBinOp of
    'fadd'	-> true;
    'fmul'	-> true;
    _		-> false
  end.

reverse_fp_binop(FpBinOp) ->
  case FpBinOp of
    'fsub' -> 'fsubr';
    'fdiv' -> 'fdivr'
  end.

%%% Create a jmp_switch instruction.

-ifdef(HIPE_AMD64).
mk_jmp_switch(Index, JTabLab, Labels) ->
  JTabReg = hipe_x86:mk_new_temp('untagged'),
  JTabImm = hipe_x86:mk_imm_from_addr(JTabLab, constant),
  [hipe_x86:mk_move64(JTabImm, JTabReg),
   hipe_x86:mk_jmp_switch(Index, JTabReg, Labels)].
-else.
mk_jmp_switch(Index, JTabLab, Labels) ->
  %% this is equivalent to "jmp *JTabLab(,Index,4)"
  %% ("r = Index; r *= 4; r += &JTab; jmp *r" isn't as nice)
  [hipe_x86:mk_jmp_switch(Index, JTabLab, Labels)].
-endif.

%%% Finalise the translation of a load_address instruction.

-ifdef(HIPE_AMD64).
mk_load_address(_Type, Src, Dst) ->
  [hipe_x86:mk_move64(Src, Dst)].
-else.
mk_load_address(_Type, Src, Dst) ->
  [hipe_x86:mk_move(Src, Dst)].
-endif.

%%% Translate 32-bit and larger loads.

-ifdef(HIPE_AMD64).
mk_load(LoadSize, LoadSign, Src, Off, Dst) ->
  case {LoadSize, LoadSign} of
    {int32, signed} ->
      [hipe_x86:mk_movsx(hipe_x86:mk_mem(Src, Off, 'int32'), Dst)];
    {int32, unsigned} ->
      %% The processor zero-extends for us. No need for 'movzx'.
      [hipe_x86:mk_move(hipe_x86:mk_mem(Src, Off, 'int32'), Dst)];
    {_, _} ->
      mk_load_word(Src, Off, Dst)
  end.
-else.
mk_load(_LoadSize, _LoadSign, Src, Off, Dst) ->
  mk_load_word(Src, Off, Dst).
-endif.

mk_load_word(Src, Off, Dst) ->
  Type = typeof_dst(Dst),
  [hipe_x86:mk_move(hipe_x86:mk_mem(Src, Off, Type), Dst)].

%%% Finalise the translation of a store instruction.

-ifdef(HIPE_AMD64).
mk_store(RtlStoreSize, Src, Ptr, Off) ->
  Type = case RtlStoreSize of
	   word ->
	     typeof_src(Src);
	   OtherType ->
	     OtherType
	 end,
  [hipe_x86:mk_move(Src, hipe_x86:mk_mem(Ptr, Off, Type))].
-else.
mk_store(RtlStoreSize, Src, Ptr, Off) ->
  case RtlStoreSize of
    word ->
      Type = typeof_src(Src),
      [hipe_x86:mk_move(Src, hipe_x86:mk_mem(Ptr, Off, Type))];
    int32 ->
      Type = typeof_src(Src),
      [hipe_x86:mk_move(Src, hipe_x86:mk_mem(Ptr, Off, Type))];
    int16 ->
      Type = 'int16',
      [hipe_x86:mk_move(Src, hipe_x86:mk_mem(Ptr, Off, Type))];
    byte ->
      Type = 'byte',
      {NewSrc, I1} = conv_small_store(Src),
      I1 ++ [hipe_x86:mk_move(NewSrc, hipe_x86:mk_mem(Ptr, Off, Type))]
  end.

conv_small_store(Src) ->
  case hipe_x86:is_imm(Src) of
    true ->
      {Src, []};
    false ->
      NewSrc = hipe_x86:mk_temp(hipe_x86_registers:eax(), 'untagged'),
      {NewSrc, [hipe_x86:mk_move(Src, NewSrc)]}
  end.
-endif.
