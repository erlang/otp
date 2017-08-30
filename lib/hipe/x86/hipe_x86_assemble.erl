%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%% %CopyrightEnd%
%%%
%%% HiPE/x86 assembler
%%%
%%% TODO:
%%% - Simplify combine_label_maps and mk_data_relocs.

-ifdef(HIPE_AMD64).
-define(HIPE_X86_ASSEMBLE,  hipe_amd64_assemble).
-define(HIPE_X86_ENCODE,    hipe_amd64_encode).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_PP,        hipe_amd64_pp).
-ifdef(AMD64_SIMULATE_NSP).
-define(X86_SIMULATE_NSP, ?AMD64_SIMULATE_NSP).
-endif.
-define(EAX, rax).
-define(REGArch, reg64).
-define(RMArch, rm64).
-define(EA_DISP32_ABSOLUTE, ea_disp32_sindex).
-else.
-define(HIPE_X86_ASSEMBLE,  hipe_x86_assemble).
-define(HIPE_X86_ENCODE,    hipe_x86_encode).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-define(HIPE_X86_PP,        hipe_x86_pp).
-define(EAX, eax).
-define(REGArch, reg32).
-define(RMArch, rm32).
-define(EA_DISP32_ABSOLUTE, ea_disp32).
-endif.

-module(?HIPE_X86_ASSEMBLE).
-export([assemble/4]).

-define(DEBUG,true).

-include("../main/hipe.hrl").
-include("../x86/hipe_x86.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-include("../misc/hipe_sdi.hrl").
-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

assemble(CompiledCode, Closures, Exports, Options) ->
  ?when_option(time, Options, ?start_timer("x86 assembler")),
  print("****************** Assembling *******************\n", [], Options),
  %%
  Code = [{MFA,
	   hipe_x86:defun_code(Defun),
	   hipe_x86:defun_data(Defun)}
	  || {MFA, Defun} <- CompiledCode],
  %%
  {ConstAlign,ConstSize,ConstMap,RefsFromConsts} =
    hipe_pack_constants:pack_constants(Code, ?HIPE_X86_REGISTERS:alignment()),
  %%
  {CodeSize,CodeBinary,AccRefs,LabelMap,ExportMap} =
    encode(translate(Code, ConstMap, Options), Options),
  print("Total num bytes=~w\n", [CodeSize], Options),
  %% put(code_size, CodeSize),
  %% put(const_size, ConstSize),
  %% ?when_option(verbose, Options,
  %%	       ?debug_msg("Constants are ~w bytes\n",[ConstSize])),
  %%
  SC = hipe_pack_constants:slim_constmap(ConstMap),
  DataRelocs = hipe_pack_constants:mk_data_relocs(RefsFromConsts, LabelMap),
  SSE = hipe_pack_constants:slim_sorted_exportmap(ExportMap,Closures,Exports),
  SlimRefs = hipe_pack_constants:slim_refs(AccRefs),
  Bin = term_to_binary([{?VERSION_STRING(),?HIPE_ERTS_CHECKSUM},
			ConstAlign, ConstSize,
			SC,
			DataRelocs, % nee LM, LabelMap
			SSE,
			CodeSize,CodeBinary,SlimRefs,
			0,[] % ColdCodeSize, SlimColdRefs
		       ]),
  %%
  %% ?when_option(time, Options, ?stop_timer("x86 assembler")),
  Bin.

%%%
%%% Assembly Pass 1.
%%% Process initial {MFA,Code,Data} list.
%%% Translate each MFA's body, choosing operand & instruction kinds.
%%%
%%% Assembly Pass 2.
%%% Perform short/long form optimisation for jumps.
%%% Build LabelMap for each MFA.
%%%
%%% Result is {MFA,NewCode,CodeSize,LabelMap} list.
%%%

translate(Code, ConstMap, Options) ->
  translate_mfas(Code, ConstMap, [], Options).

translate_mfas([{MFA,Insns,_Data}|Code], ConstMap, NewCode, Options) ->
  {NewInsns,CodeSize,LabelMap} =
    translate_insns(Insns, {MFA,ConstMap}, hipe_sdi:pass1_init(), 0, [], Options),
  translate_mfas(Code, ConstMap, [{MFA,NewInsns,CodeSize,LabelMap}|NewCode], Options);
translate_mfas([], _ConstMap, NewCode, _Options) ->
  lists:reverse(NewCode).

translate_insns([I|Insns], Context, SdiPass1, Address, NewInsns, Options) ->
  NewIs = translate_insn(I, Context, Options),
  add_insns(NewIs, Insns, Context, SdiPass1, Address, NewInsns, Options);
translate_insns([], _Context, SdiPass1, Address, NewInsns, _Options) ->
  {LabelMap,CodeSizeIncr} = hipe_sdi:pass2(SdiPass1),
  {lists:reverse(NewInsns), Address+CodeSizeIncr, LabelMap}.

add_insns([I|Is], Insns, Context, SdiPass1, Address, NewInsns, Options) ->
  NewSdiPass1 =
    case I of
      {'.label',L,_} ->
	hipe_sdi:pass1_add_label(SdiPass1, Address, L);
      {jcc_sdi,{_,{label,L}},_} ->
	SdiInfo = #sdi_info{incr=(6-2),lb=(-128)+2,ub=127+2},
	hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo);
      {jmp_sdi,{{label,L}},_} ->
	SdiInfo = #sdi_info{incr=(5-2),lb=(-128)+2,ub=127+2},
	hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo);
      _ ->
	SdiPass1
    end,
  Address1 = Address + insn_size(I),
  add_insns(Is, Insns, Context, NewSdiPass1, Address1, [I|NewInsns], Options);
add_insns([], Insns, Context, SdiPass1, Address, NewInsns, Options) ->
  translate_insns(Insns, Context, SdiPass1, Address, NewInsns, Options).

insn_size(I) ->
  case I of
    {'.label',_,_} -> 0;
    {'.sdesc',_,_} -> 0;
    {jcc_sdi,_,_} -> 2;
    {jmp_sdi,_,_} -> 2;
    {Op,Arg,_Orig} -> ?HIPE_X86_ENCODE:insn_sizeof(Op, Arg)
  end.

translate_insn(I, Context, Options) ->
  case I of
    #alu{} ->
      Arg = resolve_alu_args(hipe_x86:alu_src(I), hipe_x86:alu_dst(I), Context),
      [{hipe_x86:alu_op(I), Arg, I}];
    #call{} ->
      translate_call(I);
    #cmovcc{} ->
      {Dst,Src} = resolve_move_args(
		    hipe_x86:cmovcc_src(I), hipe_x86:cmovcc_dst(I),
		    Context),
      CC = {cc,?HIPE_X86_ENCODE:cc(hipe_x86:cmovcc_cc(I))},
      Arg = {CC,Dst,Src},
      [{cmovcc, Arg, I}];
    #cmp{} ->
      Arg = resolve_alu_args(hipe_x86:cmp_src(I), hipe_x86:cmp_dst(I), Context),
      [{cmp, Arg, I}];
    #comment{} ->
      [];
    #fmove{} ->
      {Op,Arg} = resolve_sse2_fmove_args(hipe_x86:fmove_src(I),
					 hipe_x86:fmove_dst(I)),
      [{Op, Arg, I}];
    #fp_binop{} ->
      case proplists:get_bool(x87, Options) of
	true ->  % x87
	  Arg = resolve_x87_binop_args(hipe_x86:fp_binop_src(I),
				       hipe_x86:fp_binop_dst(I)),
	  [{hipe_x86:fp_binop_op(I), Arg, I}];
	false -> % sse2
	  Arg = resolve_sse2_binop_args(hipe_x86:fp_binop_src(I),
					hipe_x86:fp_binop_dst(I)),
	  [{resolve_sse2_op(hipe_x86:fp_binop_op(I)), Arg, I}]
      end;
    #fp_unop{} ->
      case proplists:get_bool(x87, Options) of
	true ->  % x87
	  Arg = resolve_x87_unop_arg(hipe_x86:fp_unop_arg(I)),
	  [{hipe_x86:fp_unop_op(I), Arg, I}];
	false -> % sse2
	  case hipe_x86:fp_unop_op(I) of
	    'fchs' ->
	      Arg = resolve_sse2_fchs_arg(hipe_x86:fp_unop_arg(I)),
	      [{'xorpd', Arg, I}];
	    'fwait' -> % no op on sse2, magic on x87
	      []
	  end
      end;
    #imul{} ->
      translate_imul(I, Context);
    #jcc{} ->
      Cc = {cc,?HIPE_X86_ENCODE:cc(hipe_x86:jcc_cc(I))},
      Label = translate_label(hipe_x86:jcc_label(I)),
      [{jcc_sdi, {Cc,Label}, I}];
    #jmp_fun{} ->
      %% call and jmp are patched the same, so no need to distinguish
      %% call from tailcall
      PatchTypeExt =
	case hipe_x86:jmp_fun_linkage(I) of
	  remote -> ?CALL_REMOTE;
	  not_remote -> ?CALL_LOCAL
	end,
      Arg = translate_fun(hipe_x86:jmp_fun_fun(I), PatchTypeExt),
      [{jmp, {Arg}, I}];
    #jmp_label{} ->
      Arg = translate_label(hipe_x86:jmp_label_label(I)),
      [{jmp_sdi, {Arg}, I}];
    #jmp_switch{} ->
      RM32 = resolve_jmp_switch_arg(I, Context),
      [{jmp, {RM32}, I}];
    #label{} ->
      [{'.label', hipe_x86:label_label(I), I}];
    #lea{} ->
      Arg = resolve_lea_args(hipe_x86:lea_mem(I), hipe_x86:lea_temp(I)),
      [{lea, Arg, I}];
    #move{} ->
      Arg = resolve_move_args(hipe_x86:move_src(I), hipe_x86:move_dst(I),
			      Context),
      [{mov, Arg, I}];
    #move64{} ->
      translate_move64(I, Context);
    #movsx{} ->
      Arg = resolve_movx_args(hipe_x86:movsx_src(I), hipe_x86:movsx_dst(I)),
      [{movsx, Arg, I}];
    #movzx{} ->
      Arg = resolve_movx_args(hipe_x86:movzx_src(I), hipe_x86:movzx_dst(I)),
      [{movzx, Arg, I}];
    %% pseudo_call: eliminated before assembly
    %% pseudo_jcc: eliminated before assembly
    %% pseudo_tailcall: eliminated before assembly
    %% pseudo_tailcall_prepare: eliminated before assembly
    #pop{} ->
      Arg = translate_dst(hipe_x86:pop_dst(I)),
      [{pop, {Arg}, I}];
    #push{} ->
      Arg = translate_src(hipe_x86:push_src(I), Context),
      [{push, {Arg}, I}];
    #ret{} ->
      translate_ret(I);
    #shift{} ->
      Arg = resolve_shift_args(hipe_x86:shift_src(I), hipe_x86:shift_dst(I), Context),
      [{hipe_x86:shift_op(I), Arg, I}];
    #test{} ->
      Arg = resolve_test_args(hipe_x86:test_src(I), hipe_x86:test_dst(I), Context),
      [{test, Arg, I}]
  end.

-ifdef(X86_SIMULATE_NSP).
-ifdef(HIPE_AMD64).
translate_call(I) ->
  WordSize = hipe_amd64_registers:wordsize(),
  RegSP = 2#100, % esp/rsp
  TempSP = hipe_x86:mk_temp(RegSP, untagged),
  FunOrig = hipe_x86:call_fun(I),
  Fun =
    case FunOrig of
      #x86_mem{base=#x86_temp{reg=4}, off=#x86_imm{value=Off}} ->
	FunOrig#x86_mem{off=#x86_imm{value=Off+WordSize}};
      _ -> FunOrig
    end,
  RegRA =
    begin
      RegTemp0 = hipe_amd64_registers:temp0(),
      RegTemp1 = hipe_amd64_registers:temp1(),
      case Fun of
	#x86_temp{reg=RegTemp0} -> RegTemp1;
	#x86_mem{base=#x86_temp{reg=RegTemp0}} -> RegTemp1;
	_ -> RegTemp0
      end
    end,
  TempRA = hipe_x86:mk_temp(RegRA, untagged),
  PatchTypeExt =
    case hipe_x86:call_linkage(I) of
      remote -> ?CALL_REMOTE;
      not_remote -> ?CALL_LOCAL
    end,
  JmpArg = translate_fun(Fun, PatchTypeExt),
  I4 = {'.sdesc', hipe_x86:call_sdesc(I), #comment{term=sdesc}},
  I3 = {jmp, {JmpArg}, #comment{term=call}},
  Size3 = hipe_amd64_encode:insn_sizeof(jmp, {JmpArg}),
  MovArgs = {mem_to_rmArch(hipe_x86:mk_mem(TempSP,
					     hipe_x86:mk_imm(0),
					     untagged)),
	     temp_to_regArch(TempRA)},
  I2 = {mov, MovArgs, #comment{term=call}},
  Size2 = hipe_amd64_encode:insn_sizeof(mov, MovArgs),
  I1 = {lea, {temp_to_regArch(TempRA),
	      {ea, hipe_amd64_encode:ea_disp32_rip(Size2+Size3)}},
	#comment{term=call}},
  I0 = {sub, {temp_to_rmArch(TempSP), {imm8,WordSize}}, I},
  [I0,I1,I2,I3,I4].
-else.
translate_call(I) ->
  WordSize = ?HIPE_X86_REGISTERS:wordsize(),
  RegSP = 2#100, % esp/rsp
  TempSP = hipe_x86:mk_temp(RegSP, untagged),
  FunOrig = hipe_x86:call_fun(I),
  Fun =
    case FunOrig of
      #x86_mem{base=#x86_temp{reg=4}, off=#x86_imm{value=Off}} ->
	FunOrig#x86_mem{off=#x86_imm{value=Off+WordSize}};
      _ -> FunOrig
    end,
  PatchTypeExt =
    case hipe_x86:call_linkage(I) of
      remote -> ?CALL_REMOTE;
      not_remote -> ?CALL_LOCAL
    end,
  JmpArg = translate_fun(Fun, PatchTypeExt),
  I3 = {'.sdesc', hipe_x86:call_sdesc(I), #comment{term=sdesc}},
  I2 = {jmp, {JmpArg}, #comment{term=call}},
  Size2 = ?HIPE_X86_ENCODE:insn_sizeof(jmp, {JmpArg}),
  I1 = {mov, {mem_to_rmArch(hipe_x86:mk_mem(TempSP,
					  hipe_x86:mk_imm(0),
					  untagged)),
	      {imm32,{?X86ABSPCREL,4+Size2}}},
	#comment{term=call}},
  I0 = {sub, {temp_to_rmArch(TempSP), {imm8,WordSize}}, I},
  [I0,I1,I2,I3].
-endif.

translate_ret(I) ->
  NPOP = hipe_x86:ret_npop(I) + ?HIPE_X86_REGISTERS:wordsize(),
  RegSP = 2#100, % esp/rsp
  TempSP = hipe_x86:mk_temp(RegSP, untagged),
  RegRA = 2#011, % ebx/rbx
  TempRA = hipe_x86:mk_temp(RegRA, untagged),
  [{mov,
    {temp_to_regArch(TempRA),
     mem_to_rmArch(hipe_x86:mk_mem(TempSP,
				   hipe_x86:mk_imm(0),
				   untagged))},
    I},
   {add,
    {temp_to_rmArch(TempSP),
     case NPOP < 128 of
       true -> {imm8,NPOP};
       false -> {imm32,NPOP}
     end},
    #comment{term=ret}},
   {jmp,
    {temp_to_rmArch(TempRA)},
    #comment{term=ret}}].

-else. % not X86_SIMULATE_NSP

translate_call(I) ->
  %% call and jmp are patched the same, so no need to distinguish
  %% call from tailcall
  PatchTypeExt =
    case hipe_x86:call_linkage(I) of
      remote -> ?CALL_REMOTE;
      not_remote -> ?CALL_LOCAL
    end,
  Arg = translate_fun(hipe_x86:call_fun(I), PatchTypeExt),
  SDesc = hipe_x86:call_sdesc(I),
  [{call, {Arg}, I}, {'.sdesc', SDesc, #comment{term=sdesc}}].

translate_ret(I) ->
  Arg =
    case hipe_x86:ret_npop(I) of
      0 -> {};
      N -> {{imm16,N}}
    end,
  [{ret, Arg, I}].

-endif. % X86_SIMULATE_NSP

translate_imul(I, Context) ->
  Temp = temp_to_regArch(hipe_x86:imul_temp(I)),
  Src = temp_or_mem_to_rmArch(hipe_x86:imul_src(I)),
  Args =
    case hipe_x86:imul_imm_opt(I) of
      [] -> {Temp,Src};
      Imm -> {Temp,Src,translate_imm(Imm, Context, true)}
    end,
  [{'imul', Args, I}].

temp_or_mem_to_rmArch(Src) ->
  case Src of
    #x86_temp{} -> temp_to_rmArch(Src);
    #x86_mem{} -> mem_to_rmArch(Src)
  end.

translate_label(Label) when is_integer(Label) ->
  {label,Label}.	% symbolic, since offset is not yet computable

translate_fun(Arg, PatchTypeExt) ->
  case Arg of
    #x86_temp{} ->
      temp_to_rmArch(Arg);
    #x86_mem{} ->
      mem_to_rmArch(Arg);
    #x86_mfa{m=M,f=F,a=A} ->
      {rel32,{PatchTypeExt,{M,F,A}}};
    #x86_prim{prim=Prim} ->
      {rel32,{PatchTypeExt,Prim}}
  end.

translate_src(Src, Context) ->
  case Src of
    #x86_imm{} ->
      translate_imm(Src, Context, true);
    _ ->
      translate_dst(Src)
  end.

%%% MayTrunc8 controls whether negative Imm8s should be truncated
%%% to 8 bits or not. Truncation should always be done, except when
%%% the caller will widen the Imm8 to an Imm32 or Imm64.
translate_imm(#x86_imm{value=Imm}, Context, MayTrunc8) ->
  if is_atom(Imm) ->
      {imm32,{?LOAD_ATOM,Imm}};
     is_integer(Imm) ->
      case (Imm =< 127) and (Imm >= -128) of
	true ->
	  Imm8 =
	    case MayTrunc8 of
	      true -> Imm band 16#FF;
	      false -> Imm
	    end,
	  {imm8,Imm8};
	false ->
	  {imm32,Imm}
      end;
     true ->
      Val =
	case Imm of
	  {Label,constant} ->
	    {MFA,ConstMap} = Context,
	    ConstNo = hipe_pack_constants:find_const({MFA,Label}, ConstMap),
	    {constant,ConstNo};
	  {Label,closure} ->
	    {closure,Label};
	  {Label,c_const} ->
	    {c_const,Label}
	end,
      {imm32,{?LOAD_ADDRESS,Val}}
  end.

translate_dst(Dst) ->
  case Dst of
    #x86_temp{} ->
      temp_to_regArch(Dst);
    #x86_mem{type='double'} ->
      mem_to_rm64fp(Dst);
    #x86_mem{} ->
      mem_to_rmArch(Dst);
    #x86_fpreg{} ->
      fpreg_to_stack(Dst)
  end.

%%%
%%% Assembly Pass 3.
%%% Process final {MFA,Code,CodeSize,LabelMap} list from pass 2.
%%% Translate to a single binary code segment.
%%% Collect relocation patches.
%%% Build ExportMap (MFA-to-address mapping).
%%% Combine LabelMaps to a single one (for mk_data_relocs/2 compatibility).
%%% Return {CombinedCodeSize,BinaryCode,Relocs,CombinedLabelMap,ExportMap}.
%%%

encode(Code, Options) ->
  CodeSize = compute_code_size(Code, 0),
  ExportMap = build_export_map(Code, 0, []),
  {AccCode,Relocs} = encode_mfas(Code, 0, [], [], Options),
  CodeBinary = list_to_binary(lists:reverse(AccCode)),
  ?ASSERT(CodeSize =:= byte_size(CodeBinary)),
  CombinedLabelMap = combine_label_maps(Code, 0, gb_trees:empty()),
  {CodeSize,CodeBinary,Relocs,CombinedLabelMap,ExportMap}.

nr_pad_bytes(Address) -> (4 - (Address rem 4)) rem 4. % XXX: 16 or 32 instead?

align_entry(Address) -> Address + nr_pad_bytes(Address).

compute_code_size([{_MFA,_Insns,CodeSize,_LabelMap}|Code], Size) ->
  compute_code_size(Code, align_entry(Size+CodeSize));
compute_code_size([], Size) -> Size.

build_export_map([{{M,F,A},_Insns,CodeSize,_LabelMap}|Code], Address, ExportMap) ->
  build_export_map(Code, align_entry(Address+CodeSize), [{Address,M,F,A}|ExportMap]);
build_export_map([], _Address, ExportMap) -> ExportMap.

combine_label_maps([{MFA,_Insns,CodeSize,LabelMap}|Code], Address, CLM) ->
  NewCLM = merge_label_map(gb_trees:to_list(LabelMap), MFA, Address, CLM),
  combine_label_maps(Code, align_entry(Address+CodeSize), NewCLM);
combine_label_maps([], _Address, CLM) -> CLM.

merge_label_map([{Label,Offset}|Rest], MFA, Address, CLM) ->
  NewCLM = gb_trees:insert({MFA,Label}, Address+Offset, CLM),
  merge_label_map(Rest, MFA, Address, NewCLM);
merge_label_map([], _MFA, _Address, CLM) -> CLM.

encode_mfas([{MFA,Insns,CodeSize,LabelMap}|Code], Address, AccCode, Relocs, Options) ->
  print("Generating code for:~w\n", [MFA], Options),
  print("Offset   | Opcode                   | Instruction\n", [], Options),
  {Address1,Relocs1,AccCode1} =
    encode_insns(Insns, Address, Address, LabelMap, Relocs, AccCode, Options),
  ExpectedAddress = align_entry(Address + CodeSize),
  ?ASSERT(Address1 =:= ExpectedAddress),
  print("Finished.\n\n", [], Options),
  encode_mfas(Code, Address1, AccCode1, Relocs1, Options);
encode_mfas([], _Address, AccCode, Relocs, _Options) ->
  {AccCode, Relocs}.

encode_insns([I|Insns], Address, FunAddress, LabelMap, Relocs, AccCode, Options) ->
  case I of
    {'.label',L,_} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ?ASSERT(Address =:= LabelAddress),	% sanity check
      print_insn(Address, [], I, Options),
      encode_insns(Insns, Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    {'.sdesc',SDesc,_} ->
      #x86_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live} = SDesc,
      ExnRA =
	case ExnLab of
	  [] -> [];	% don't cons up a new one
	  ExnLab -> gb_trees:get(ExnLab, LabelMap) + FunAddress
	end,
      Reloc = {?SDESC, Address,
	       ?STACK_DESC(ExnRA, FSize, Arity, Live)},
      encode_insns(Insns, Address, FunAddress, LabelMap, [Reloc|Relocs], AccCode, Options);
    _ ->
      {Op,Arg,_} = fix_jumps(I, Address, FunAddress, LabelMap),
      {Bytes, NewRelocs} = ?HIPE_X86_ENCODE:insn_encode(Op, Arg, Address),
      print_insn(Address, Bytes, I, Options),
      Segment = list_to_binary(Bytes),
      Size = byte_size(Segment),
      NewAccCode = [Segment|AccCode],
      encode_insns(Insns, Address+Size, FunAddress, LabelMap, NewRelocs++Relocs, NewAccCode, Options)
  end;
encode_insns([], Address, FunAddress, LabelMap, Relocs, AccCode, Options) ->
  case nr_pad_bytes(Address) of
    0 ->
      {Address,Relocs,AccCode};
    NrPadBytes ->	% triggers at most once per function body
      Padding = lists:duplicate(NrPadBytes, {nop,{},#comment{term=padding}}),
      encode_insns(Padding, Address, FunAddress, LabelMap, Relocs, AccCode, Options)
  end.

fix_jumps(I, InsnAddress, FunAddress, LabelMap) ->
  case I of
    {jcc_sdi,{CC,{label,L}},OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ShortOffset = LabelAddress - (InsnAddress + 2),
      if is_integer(ShortOffset), ShortOffset >= -128, ShortOffset =< 127 ->
	  {jcc,{CC,{rel8,ShortOffset band 16#FF}},OrigI};
	 true ->
	  LongOffset = LabelAddress - (InsnAddress + 6),
	  {jcc,{CC,{rel32,LongOffset}},OrigI}
      end;
    {jmp_sdi,{{label,L}},OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ShortOffset = LabelAddress - (InsnAddress + 2),
      if is_integer(ShortOffset), ShortOffset >= -128, ShortOffset =< 127 ->
	  {jmp,{{rel8,ShortOffset band 16#FF}},OrigI};
	 true ->
	  LongOffset = LabelAddress - (InsnAddress + 5),
	  {jmp,{{rel32,LongOffset}},OrigI}
      end;
    _ -> I
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fpreg_to_stack(#x86_fpreg{reg=Reg}) ->
  {fpst, Reg}.

temp_to_regArch(#x86_temp{reg=Reg}) ->
  {?REGArch, Reg}.

-ifdef(HIPE_AMD64).
temp_to_reg64(#x86_temp{reg=Reg}) ->
  {reg64, Reg}.
-endif.

temp_to_reg32(#x86_temp{reg=Reg}) ->
  {reg32, Reg}.
temp_to_reg16(#x86_temp{reg=Reg}) ->
  {reg16, Reg}.
temp_to_reg8(#x86_temp{reg=Reg}) ->
  {reg8, Reg}.

temp_to_xmm(#x86_temp{reg=Reg}) ->
  {xmm, Reg}. 

-ifdef(HIPE_AMD64).
temp_to_rm64(#x86_temp{reg=Reg}) ->
  {rm64, hipe_amd64_encode:rm_reg(Reg)}.
-endif.

temp_to_rmArch(#x86_temp{reg=Reg}) ->
  {?RMArch, ?HIPE_X86_ENCODE:rm_reg(Reg)}.
temp_to_rm64fp(#x86_temp{reg=Reg}) ->
  {rm64fp, ?HIPE_X86_ENCODE:rm_reg(Reg)}.

mem_to_ea(Mem) ->
  EA = mem_to_ea_common(Mem),
  {ea, EA}.

mem_to_rm32(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm32, ?HIPE_X86_ENCODE:rm_mem(EA)}.

mem_to_rmArch(Mem) ->
  EA = mem_to_ea_common(Mem),
  {?RMArch, ?HIPE_X86_ENCODE:rm_mem(EA)}.

mem_to_rm64fp(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm64fp, ?HIPE_X86_ENCODE:rm_mem(EA)}.

%%%%%%%%%%%%%%%%%
mem_to_rm8(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm8, ?HIPE_X86_ENCODE:rm_mem(EA)}.

mem_to_rm16(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm16, ?HIPE_X86_ENCODE:rm_mem(EA)}.
%%%%%%%%%%%%%%%%%

mem_to_ea_common(#x86_mem{base=[], off=#x86_imm{value=Off}}) ->
  ?HIPE_X86_ENCODE:?EA_DISP32_ABSOLUTE(Off);
mem_to_ea_common(#x86_mem{base=#x86_temp{reg=Base}, off=#x86_temp{reg=Index}}) ->
  case Base band 2#111 of
    5 -> % ebp/rbp or r13
      case Index band 2#111 of
	5 -> % ebp/rbp or r13
	  SINDEX = ?HIPE_X86_ENCODE:sindex(0, Index),
	  SIB = ?HIPE_X86_ENCODE:sib(Base, SINDEX),
	  ?HIPE_X86_ENCODE:ea_disp8_sib(0, SIB);
	_ ->
	  SINDEX = ?HIPE_X86_ENCODE:sindex(0, Base),
	  SIB = ?HIPE_X86_ENCODE:sib(Index, SINDEX),
	  ?HIPE_X86_ENCODE:ea_sib(SIB)
      end;
    _ ->
      SINDEX = ?HIPE_X86_ENCODE:sindex(0, Index),
      SIB = ?HIPE_X86_ENCODE:sib(Base, SINDEX),
      ?HIPE_X86_ENCODE:ea_sib(SIB)
  end;
mem_to_ea_common(#x86_mem{base=#x86_temp{reg=Base}, off=#x86_imm{value=Off}}) ->
  if
    Off =:= 0 ->
      case Base of
	4 -> %esp, use SIB w/o disp8
	  SIB = ?HIPE_X86_ENCODE:sib(Base),
	  ?HIPE_X86_ENCODE:ea_sib(SIB);
	5 -> %ebp, use disp8 w/o SIB
	  ?HIPE_X86_ENCODE:ea_disp8_base(Off, Base);
        12 -> %r12, use SIB w/o disp8
	  SIB = ?HIPE_X86_ENCODE:sib(Base),
	  ?HIPE_X86_ENCODE:ea_sib(SIB);
        13 -> %r13, use disp8 w/o SIB
 	  ?HIPE_X86_ENCODE:ea_disp8_base(Off, Base);         
	_ -> %neither SIB nor disp8 needed
	  ?HIPE_X86_ENCODE:ea_base(Base)
      end;
    Off >= -128, Off =< 127 ->
      Disp8 = Off band 16#FF,
      case Base of
	4 -> %esp, must use SIB
	  SIB = ?HIPE_X86_ENCODE:sib(Base),
	  ?HIPE_X86_ENCODE:ea_disp8_sib(Disp8, SIB);
        12 -> %r12, must use SIB
	  SIB = ?HIPE_X86_ENCODE:sib(Base),
	  ?HIPE_X86_ENCODE:ea_disp8_sib(Disp8, SIB);
	_ -> %use disp8 w/o SIB
	  ?HIPE_X86_ENCODE:ea_disp8_base(Disp8, Base)
      end;
    true ->
      case Base of
	4 -> %esp, must use SIB
	  SIB = ?HIPE_X86_ENCODE:sib(Base),
	  ?HIPE_X86_ENCODE:ea_disp32_sib(Off, SIB);
	12 -> %r12, must use SIB
	  SIB = ?HIPE_X86_ENCODE:sib(Base),
	  ?HIPE_X86_ENCODE:ea_disp32_sib(Off, SIB);
	_ ->
	  ?HIPE_X86_ENCODE:ea_disp32_base(Off, Base)
      end
  end.

%% jmp_switch
-ifdef(HIPE_AMD64).
resolve_jmp_switch_arg(I, _Context) ->
  Base = hipe_x86:temp_reg(hipe_x86:jmp_switch_jtab(I)),
  Index = hipe_x86:temp_reg(hipe_x86:jmp_switch_temp(I)),
  SINDEX = hipe_amd64_encode:sindex(3, Index),
  SIB = hipe_amd64_encode:sib(Base, SINDEX),
  EA =
    if (Base =:= 5) or (Base =:= 13) ->
	hipe_amd64_encode:ea_disp8_sib(0, SIB);
       true ->
	hipe_amd64_encode:ea_sib(SIB)
    end,
  {rm64,hipe_amd64_encode:rm_mem(EA)}.
-else.
resolve_jmp_switch_arg(I, {MFA,ConstMap}) ->
  ConstNo = hipe_pack_constants:find_const({MFA,hipe_x86:jmp_switch_jtab(I)}, ConstMap),
  Disp32 = {?LOAD_ADDRESS,{constant,ConstNo}},
  SINDEX = ?HIPE_X86_ENCODE:sindex(2, hipe_x86:temp_reg(hipe_x86:jmp_switch_temp(I))),
  EA = ?HIPE_X86_ENCODE:ea_disp32_sindex(Disp32, SINDEX), % this creates a SIB implicitly
  {rm32,?HIPE_X86_ENCODE:rm_mem(EA)}.
-endif.

%% lea reg, mem
resolve_lea_args(Src=#x86_mem{}, Dst=#x86_temp{}) ->
  {temp_to_regArch(Dst),mem_to_ea(Src)}.

resolve_sse2_op(Op) ->
  case Op of
    fadd -> addsd;
    fdiv -> divsd;
    fmul -> mulsd;
    fsub -> subsd;
    _ -> exit({?MODULE, unknown_sse2_operator, Op})
  end.

%% OP xmm, mem
resolve_sse2_binop_args(Src=#x86_mem{type=double},
			Dst=#x86_temp{type=double}) ->
  {temp_to_xmm(Dst),mem_to_rm64fp(Src)};
%% movsd mem, xmm
resolve_sse2_binop_args(Src=#x86_temp{type=double},
			Dst=#x86_mem{type=double}) ->
  {mem_to_rm64fp(Dst),temp_to_xmm(Src)};
%% OP xmm, xmm
resolve_sse2_binop_args(Src=#x86_temp{type=double},
			Dst=#x86_temp{type=double}) ->
  {temp_to_xmm(Dst),temp_to_rm64fp(Src)}.

%%% fmove -> cvtsi2sd or movsd
resolve_sse2_fmove_args(Src, Dst) ->
  case {Src,Dst} of
    {#x86_temp{type=untagged}, #x86_temp{type=double}} -> % cvtsi2sd xmm, reg
      {cvtsi2sd, {temp_to_xmm(Dst),temp_to_rmArch(Src)}};
    {#x86_mem{type=untagged}, #x86_temp{type=double}} -> % cvtsi2sd xmm, mem
      {cvtsi2sd, {temp_to_xmm(Dst),mem_to_rmArch(Src)}};
    _ -> % movsd
      {movsd, resolve_sse2_binop_args(Src, Dst)}
  end.

%%% xorpd xmm, mem
resolve_sse2_fchs_arg(Dst=#x86_temp{type=double}) ->
  {temp_to_xmm(Dst),
   {rm64fp, {rm_mem, ?HIPE_X86_ENCODE:?EA_DISP32_ABSOLUTE(
		       {?LOAD_ADDRESS,
			{c_const, sse2_fnegate_mask}})}}}.

%% mov mem, imm
resolve_move_args(#x86_imm{value=ImmSrc}, Dst=#x86_mem{type=Type}, Context) ->
  case Type of   % to support byte, int16 and int32 stores
    byte ->
      ByteImm = ImmSrc band 255, %to ensure that it is a bytesized imm
      {mem_to_rm8(Dst),{imm8,ByteImm}};
    int16 ->
      {mem_to_rm16(Dst),{imm16,ImmSrc band 16#FFFF}};
    int32 ->
      {_,Imm} = translate_imm(#x86_imm{value=ImmSrc}, Context, false),
      {mem_to_rm32(Dst),{imm32,Imm}};
    _ ->
      RMArch = mem_to_rmArch(Dst),
      {_,Imm} = translate_imm(#x86_imm{value=ImmSrc}, Context, false),
      {RMArch,{imm32,Imm}}
  end;

%% mov reg,mem
resolve_move_args(Src=#x86_mem{type=Type}, Dst=#x86_temp{}, _Context) ->
  case Type of
    int32 -> % must be unsigned
      {temp_to_reg32(Dst),mem_to_rm32(Src)};
    _ ->
      {temp_to_regArch(Dst),mem_to_rmArch(Src)}
  end;

%% mov mem,reg
resolve_move_args(Src=#x86_temp{}, Dst=#x86_mem{type=Type}, _Context) ->
  case Type of   % to support byte, int16 and int32 stores
    byte ->
      {mem_to_rm8(Dst),temp_to_reg8(Src)};
    int16 ->
      {mem_to_rm16(Dst),temp_to_reg16(Src)};
    int32 ->
      {mem_to_rm32(Dst),temp_to_reg32(Src)};
    tagged -> % tagged, untagged
      {mem_to_rmArch(Dst),temp_to_regArch(Src)};
    untagged -> % tagged, untagged
      {mem_to_rmArch(Dst),temp_to_regArch(Src)}
  end;

%% mov reg,reg
resolve_move_args(Src=#x86_temp{}, Dst=#x86_temp{}, _Context) ->
  {temp_to_regArch(Dst),temp_to_rmArch(Src)};

%% mov reg,imm
resolve_move_args(Src=#x86_imm{value=_ImmSrc}, Dst=#x86_temp{}, Context) ->
  {_,Imm} = translate_imm(Src, Context, false),
  imm_move_args(Dst, Imm).

-ifdef(HIPE_AMD64).
imm_move_args(Dst, Imm) ->
  if is_number(Imm), Imm >= 0 ->
      {temp_to_reg32(Dst),{imm32,Imm}};
     true ->
      {temp_to_rm64(Dst),{imm32,Imm}}
  end.
-else.
imm_move_args(Dst, Imm) ->
  {temp_to_reg32(Dst),{imm32,Imm}}.
-endif.

-ifdef(HIPE_AMD64).
translate_move64(I, Context) ->
  Arg = resolve_move64_args(hipe_x86:move64_src(I),
			    hipe_x86:move64_dst(I),
			    Context),
  [{mov, Arg, I}].

%% mov reg,imm64
resolve_move64_args(Src=#x86_imm{}, Dst=#x86_temp{}, Context) ->
  {_,Imm} = translate_imm(Src, Context, false),
  {temp_to_reg64(Dst),{imm64,Imm}}.
-else.
translate_move64(I, _Context) -> exit({?MODULE, I}).
-endif.

%%% mov{s,z}x
resolve_movx_args(Src=#x86_mem{type=Type}, Dst=#x86_temp{}) ->
  {temp_to_regArch(Dst),
   case Type of
     byte ->
       mem_to_rm8(Src);
     int16 ->
       mem_to_rm16(Src);
     int32 ->
       mem_to_rm32(Src)
   end}.

%%% alu/cmp (_not_ test)
resolve_alu_args(Src, Dst, Context) ->
  case {Src,Dst} of
    {#x86_imm{}, #x86_mem{}} ->
      {mem_to_rmArch(Dst), translate_imm(Src, Context, true)};
    {#x86_mem{}, #x86_temp{}} ->
      {temp_to_regArch(Dst), mem_to_rmArch(Src)};
    {#x86_temp{}, #x86_mem{}} ->
      {mem_to_rmArch(Dst), temp_to_regArch(Src)};
    {#x86_temp{}, #x86_temp{}} ->
      {temp_to_regArch(Dst), temp_to_rmArch(Src)};
    {#x86_imm{}, #x86_temp{reg=0}} -> % eax,imm
      NewSrc = translate_imm(Src, Context, true),
      NewDst =
	case NewSrc of
	  {imm8,_} -> temp_to_rmArch(Dst);
	  {imm32,_} -> ?EAX
	end,
      {NewDst, NewSrc};
    {#x86_imm{}, #x86_temp{}} ->
      {temp_to_rmArch(Dst), translate_imm(Src, Context, true)}
  end.

%%% test
resolve_test_args(Src, Dst, Context) ->
  case Src of
    #x86_imm{} -> % imm8 not allowed
      {_ImmSize,ImmValue} = translate_imm(Src, Context, false),
      NewDst =
	case Dst of
	  #x86_temp{reg=0} -> ?EAX;
	  #x86_temp{} -> temp_to_rmArch(Dst);
	  #x86_mem{} -> mem_to_rmArch(Dst)
	end,
      {NewDst, {imm32,ImmValue}};
    #x86_temp{} ->
      NewDst =
	case Dst of
	  #x86_temp{} -> temp_to_rmArch(Dst);
	  #x86_mem{} -> mem_to_rmArch(Dst)
	end,
      {NewDst, temp_to_regArch(Src)}
  end.

%%% shifts
resolve_shift_args(Src, Dst, Context) ->
  RM32 =
    case Dst of
      #x86_temp{} -> temp_to_rmArch(Dst);
      #x86_mem{} -> mem_to_rmArch(Dst)
    end,
  Count =
    case Src of
      #x86_imm{value=1} -> 1;
      #x86_imm{} -> translate_imm(Src, Context, true); % must be imm8
      #x86_temp{reg=1} -> cl	% temp must be ecx
    end,
  {RM32, Count}.

%% x87_binop mem
resolve_x87_unop_arg(Arg=#x86_mem{type=Type})->
  case Type of
    'double' -> {mem_to_rm64fp(Arg)};
    'untagged' -> {mem_to_rmArch(Arg)};
    _ -> ?EXIT({fmovArgNotSupported,{Arg}})
  end;
resolve_x87_unop_arg(Arg=#x86_fpreg{}) ->
  {fpreg_to_stack(Arg)};
resolve_x87_unop_arg([]) ->
  [].

%% x87_binop mem, st(i)
resolve_x87_binop_args(Src=#x86_fpreg{}, Dst=#x86_mem{})->
  {mem_to_rm64fp(Dst),fpreg_to_stack(Src)};
%% x87_binop st(0), st(i)
resolve_x87_binop_args(Src=#x86_fpreg{}, Dst=#x86_fpreg{})->
  {fpreg_to_stack(Dst),fpreg_to_stack(Src)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% Assembly listing support (pp_asm option).
%%%

print(String, Arglist, Options) ->
  ?when_option(pp_asm, Options, io:format(String, Arglist)).

print_insn(Address, Bytes, I, Options) ->
  ?when_option(pp_asm, Options, print_insn_2(Address, Bytes, I)),
  ?when_option(pp_cxmon, Options, print_code_list_2(Bytes)).

print_code_list_2([H | Tail]) ->
  print_byte(H),
  io:format(","),
  print_code_list_2(Tail);
print_code_list_2([]) ->
  io:format("").

print_insn_2(Address, Bytes, {_,_,OrigI}) ->
  io:format("~8.16b | ", [Address]),
  print_code_list(Bytes, 0),
  ?HIPE_X86_PP:pp_insn(OrigI).

print_code_list([Byte|Rest], Len) ->
  print_byte(Byte),
  print_code_list(Rest, Len+1);
print_code_list([], Len) ->
  fill_spaces(24-(Len*2)),
  io:format(" | ").

print_byte(Byte) ->
  io:format("~2.16.0b", [Byte band 16#FF]).

fill_spaces(N) when N > 0 ->
  io:format(" "),
  fill_spaces(N-1);
fill_spaces(0) ->
  [].
