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

-module(hipe_arm_assemble).
-export([assemble/4]).

-include("../main/hipe.hrl").	% for VERSION_STRING, when_option
-include("hipe_arm.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

assemble(CompiledCode, Closures, Exports, Options) ->
  print("****************** Assembling *******************\n", [], Options),
  %%
  Code = [{MFA,
	   hipe_arm:defun_code(Defun),
	   hipe_arm:defun_data(Defun)}
	  || {MFA, Defun} <- CompiledCode],
  %%
  {ConstAlign,ConstSize,ConstMap,RefsFromConsts} =
    hipe_pack_constants:pack_constants(Code, 4),
  %%
  {CodeSize,CodeBinary,AccRefs,LabelMap,ExportMap} =
    encode(translate(Code, ConstMap), Options),
  print("Total num bytes=~w\n", [CodeSize], Options),
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
  Bin.

%%%
%%% Assembly Pass 1.
%%% Process initial {MFA,Code,Data} list.
%%% Translate each MFA's body, choosing operand & instruction kinds.
%%% Manage placement of large immediates in the code segment. (ARM-specific)
%%%
%%% Assembly Pass 2.
%%% Perform short/long form optimisation for jumps.
%%% (Trivial on ARM.)
%%%
%%% Result is {MFA,NewCode,CodeSize,LabelMap} list.
%%%

translate(Code, ConstMap) ->
  translate_mfas(Code, ConstMap, []).

translate_mfas([{MFA,Insns,_Data}|Code], ConstMap, NewCode) ->
  {NewInsns,CodeSize,LabelMap} = translate_insns(Insns, MFA, ConstMap),
  translate_mfas(Code, ConstMap, [{MFA,NewInsns,CodeSize,LabelMap}|NewCode]);
translate_mfas([], _ConstMap, NewCode) ->
  lists:reverse(NewCode).

translate_insns(Insns, MFA, ConstMap) ->
  translate_insns(Insns, MFA, ConstMap, gb_trees:empty(), 0, [],
		  previous_empty(), pending_empty()).

translate_insns([I|Is] = Insns, MFA, ConstMap, LabelMap, Address, NewInsns, PrevImms, PendImms) ->
  IsNotFallthroughInsn = is_not_fallthrough_insn(I),
  MustFlushPending = must_flush_pending(PendImms, Address),
  {NewIs,Insns1,PendImms1,DoFlushPending} =
    case {MustFlushPending,IsNotFallthroughInsn} of
      {true,false} ->
	%% To avoid having to create new symbolic labels, which is problematic
	%% in the assembler, we emit a forward branch with an offset computed
	%% from the size of the pending literals.
	N = pending_size(PendImms),	% N >= 1 since MustFlushPending is true
	BranchOffset = N - 1,		% in units of 32-bit words!
	NewIs0 = [{b, {do_cond('al'),{imm24,BranchOffset}}, #comment{term='skip'}}],
	%% io:format("~w: forced flush of pending literals in ~w at ~w\n", [?MODULE,MFA,Address]),
	{NewIs0,Insns,PendImms,true};
      {_,_} ->
	{NewIs0,PendImms0} = translate_insn(I, MFA, ConstMap, Address, PrevImms, PendImms),
	{NewIs0,Is,PendImms0,IsNotFallthroughInsn}
    end,
  add_insns(NewIs, Insns1, MFA, ConstMap, LabelMap, Address, NewInsns, PrevImms, PendImms1, DoFlushPending);
translate_insns([], _MFA, _ConstMap, LabelMap, Address, NewInsns, PrevImms, PendImms) ->
  {LabelMap1, Address1, NewInsns1, _PrevImms1} = % at end-of-function we ignore PrevImms1
    flush_pending(PendImms, LabelMap, Address, NewInsns, PrevImms),
  {lists:reverse(NewInsns1), Address1, LabelMap1}.

add_insns([I|Is], Insns, MFA, ConstMap, LabelMap, Address, NewInsns, PrevImms, PendImms, DoFlushPending) ->
  NewLabelMap =
    case I of
      {'.label',L,_} ->
	gb_trees:insert(L, Address, LabelMap);
      _ ->
	LabelMap
    end,
  Address1 = Address + insn_size(I),
  add_insns(Is, Insns, MFA, ConstMap, NewLabelMap, Address1, [I|NewInsns], PrevImms, PendImms, DoFlushPending);
add_insns([], Insns, MFA, ConstMap, LabelMap, Address, NewInsns, PrevImms, PendImms, DoFlushPending) ->
  {LabelMap1, Address1, NewInsns1, PrevImms1, PendImms1} =
    case DoFlushPending of
      true ->
	{LabelMap0,Address0,NewInsns0,PrevImms0} =
	  flush_pending(PendImms, LabelMap, Address, NewInsns, PrevImms),
	{LabelMap0,Address0,NewInsns0,PrevImms0,pending_empty()};
      false ->
	PrevImms0 = expire_previous(PrevImms, Address),
	{LabelMap,Address,NewInsns,PrevImms0,PendImms}
    end,
  translate_insns(Insns, MFA, ConstMap, LabelMap1, Address1, NewInsns1, PrevImms1, PendImms1).

must_flush_pending(PendImms, Address) ->
  case pending_firstref(PendImms) of
    [] -> false;
    LP0 ->
      Distance = Address - LP0,
      %% In "LP0: ldr R,[PC +/- imm12]", the PC value is LP0+8 so the
      %% range for the ldr is [LP0-4084, LP0+4100] (32-bit alignment!).
      %% LP0+4096 is the last point where we can emit a branch (4 bytes)
      %% followed by the pending immediates.
      %%
      %% The translation of an individual instruction must not advance
      %% . by more than 4 bytes, because that could cause us to miss
      %% the point where PendImms must be flushed.
      ?ASSERT(Distance =< 4096),
      Distance =:= 4096
  end.

flush_pending(PendImms, LabelMap, Address, Insns, PrevImms) ->
  Address1 = Address + 4*pending_size(PendImms),
  PrevImms1 = expire_previous(PrevImms, Address1),
  {LabelMap1,Address1,Insns1,PrevImms2} =
    flush_pending2(pending_to_list(PendImms), LabelMap, Address, Insns, PrevImms1),
  PrevImms3 = expire_previous(PrevImms2, Address1),
  {LabelMap1,Address1,Insns1,PrevImms3}.

flush_pending2([{Lab,RelocOrInt,Imm}|Imms], LabelMap, Address, Insns, PrevImms) ->
  PrevImms1 = previous_append(PrevImms, Address, Lab, Imm),
  LabelMap1 = gb_trees:insert(Lab, Address, LabelMap),
  {RelocOpt,LongVal} =
    if is_integer(RelocOrInt) ->
	{[],RelocOrInt};
       true ->
	{[RelocOrInt],0}
    end,
  Insns1 =
    [{'.long', LongVal, #comment{term=Imm}} |
     RelocOpt ++
     [{'.label', Lab, #comment{term=Imm}} |
      Insns]],
  flush_pending2(Imms, LabelMap1, Address+4, Insns1, PrevImms1);
flush_pending2([], LabelMap, Address, Insns, PrevImms) ->
  {LabelMap, Address, Insns, PrevImms}.

expire_previous(PrevImms, CodeAddress) ->
  case previous_findmin(PrevImms) of
    [] -> PrevImms;
    {ImmAddress,_Imm} ->
      if CodeAddress - ImmAddress > 4084 ->
	  expire_previous(previous_delmin(PrevImms), CodeAddress);
	 true ->
	  PrevImms
      end
  end.

is_not_fallthrough_insn(I) ->
  case I of
    #b_fun{} -> true;
    #b_label{'cond'='al'} -> true;
    %% bl and blx are not included since they return to ".+4"
    %% a load to PC was originally a pseudo_switch insn
    #load{dst=#arm_temp{reg=15,type=Type}} when Type =/= 'double' -> true;
    %% a move to PC was originally a pseudo_blr or pseudo_bx insn
    #move{dst=#arm_temp{reg=15,type=Type}} when Type =/= 'double' -> true;
    _ -> false
  end.

insn_size(I) ->
  case I of
    {'.label',_,_} -> 0;
    {'.reloc',_,_} -> 0;
    _ -> 4
  end.

translate_insn(I, MFA, ConstMap, Address, PrevImms, PendImms) ->
  case I of
    %% pseudo_li is the only insn using MFA, ConstMap, Address, PrevImms, or PendLits
    #pseudo_li{} -> do_pseudo_li(I, MFA, ConstMap, Address, PrevImms, PendImms);
    _ -> {translate_insn(I), PendImms}
  end.

translate_insn(I) ->	% -> [{Op,Opnd,OrigI}]
  case I of
    #alu{} -> do_alu(I);
    #b_fun{} -> do_b_fun(I);
    #b_label{} -> do_b_label(I);
    #bl{} -> do_bl(I);
    #blx{} -> do_blx(I);
    #cmp{} -> do_cmp(I);
    #comment{} -> [];
    #label{} -> do_label(I);
    #load{} -> do_load(I);
    #ldrsb{} -> do_ldrsb(I);
    #move{} -> do_move(I);
    %% pseudo_b: eliminated by finalise
    %% pseudo_blr: eliminated by finalise
    %% pseudo_call: eliminated by finalise
    %% pseudo_call_prepare: eliminated by frame
    %% pseudo_li: handled separately
    %% pseudo_move: eliminated by frame
    %% pseudo_switch: eliminated by finalise
    %% pseudo_tailcall: eliminated by frame
    %% pseudo_tailcall_prepare: eliminated by finalise
    #smull{} -> do_smull(I);
    #store{} -> do_store(I)
  end.

do_alu(I) ->
  #alu{aluop=AluOp,s=S,dst=Dst,src=Src,am1=Am1} = I,
  NewCond = do_cond('al'),
  NewS = do_s(S),
  NewDst = do_reg(Dst),
  NewSrc = do_reg(Src),
  NewAm1 = do_am1(Am1),
  {NewI,NewOpnds} = {AluOp, {NewCond,NewS,NewDst,NewSrc,NewAm1}},
  [{NewI, NewOpnds, I}].

do_b_fun(I) ->
  #b_fun{'fun'=Fun,linkage=Linkage} = I,
  [{'.reloc', {b_fun,Fun,Linkage}, #comment{term='fun'}},
   {b, {do_cond('al'),{imm24,0}}, I}].

do_b_label(I) ->
  #b_label{'cond'=Cond,label=Label} = I,
  [{b, {do_cond(Cond),do_label_ref(Label)}, I}].

do_bl(I) ->
  #bl{'fun'=Fun,sdesc=SDesc,linkage=Linkage} = I,
  [{'.reloc', {b_fun,Fun,Linkage}, #comment{term='fun'}},
   {bl, {do_cond('al'),{imm24,0}}, I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_blx(I) ->
  #blx{src=Src,sdesc=SDesc} = I,
  [{blx, {do_cond('al'),do_reg(Src)}, I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_cmp(I) ->
  #cmp{cmpop=CmpOp,src=Src,am1=Am1} = I,
  NewCond = do_cond('al'),
  NewSrc = do_reg(Src),
  NewAm1 = do_am1(Am1),
  [{CmpOp, {NewCond,NewSrc,NewAm1}, I}].

do_label(I) ->
  #label{label=Label} = I,
  [{'.label', Label, I}].

do_load(I) ->
  #load{ldop=LdOp,dst=Dst,am2=Am2} = I,
  NewCond = do_cond('al'),
  NewDst = do_reg(Dst),
  NewAm2 = do_am2(Am2),
  [{LdOp, {NewCond,NewDst,NewAm2}, I}].

do_ldrsb(I) ->
  #ldrsb{dst=Dst,am3=Am3} = I,
  NewCond = do_cond('al'),
  NewDst = do_reg(Dst),
  NewAm3 = do_am3(Am3),
  [{'ldrsb', {NewCond,NewDst,NewAm3}, I}].

do_move(I) ->
  #move{movop=MovOp,s=S,dst=Dst,am1=Am1} = I,
  NewCond = do_cond('al'),
  NewS = do_s(S),
  NewDst = do_reg(Dst),
  NewAm1 = do_am1(Am1),
  [{MovOp, {NewCond,NewS,NewDst,NewAm1}, I}].

do_pseudo_li(I, MFA, ConstMap, Address, PrevImms, PendImms) ->
  #pseudo_li{dst=Dst,imm=Imm,label=Label0} = I,
  {Label1,PendImms1} =
    case previous_lookup(PrevImms, Imm) of
      {value,Lab} -> {Lab,PendImms};
      none ->
	case pending_lookup(PendImms, Imm) of
	  {value,Lab} -> {Lab,PendImms};
	  none ->
	    RelocOrInt =
	      if is_integer(Imm) ->
		  %% This is for immediates that require too much work
		  %% to reconstruct using only arithmetic instructions.
		  Imm;
		 true ->
		  RelocData =
		    case Imm of
		      Atom when is_atom(Atom) ->
			{load_atom, Atom};
		      {Label,constant} ->
			ConstNo = hipe_pack_constants:find_const({MFA,Label}, ConstMap),
			{load_address, {constant,ConstNo}};
		      {Label,closure} ->
			{load_address, {closure,Label}};
		      {Label,c_const} ->
			{load_address, {c_const,Label}}
		    end,
		  {'.reloc', RelocData, #comment{term=reloc}}
	      end,
	    Lab = Label0, % preallocated: creating labels in the assembler doesn't work
	    {Lab, pending_append(PendImms, Address, Lab, RelocOrInt, Imm)}
	end
    end,
  NewDst = do_reg(Dst),
  {[{'.pseudo_li', {NewDst,do_label_ref(Label1)}, I}], PendImms1}.

do_smull(I) ->
  #smull{dstlo=DstLo,dsthi=DstHi,src1=Src1,src2=Src2} = I,
  NewCond = do_cond('al'),
  NewS = do_s(false),
  NewDstLo = do_reg(DstLo),
  NewDstHi = do_reg(DstHi),
  NewSrc1 = do_reg(Src1),
  NewSrc2 = do_reg(Src2),
  [{'smull', {NewCond,NewS,NewDstLo,NewDstHi,NewSrc1,NewSrc2}, I}].

do_store(I) ->
  #store{stop=StOp,src=Src,am2=Am2} = I,
  NewCond = do_cond('al'),
  NewSrc = do_reg(Src),
  NewAm2 = do_am2(Am2),
  [{StOp, {NewCond,NewSrc,NewAm2}, I}].

do_reg(#arm_temp{reg=Reg,type=Type})
  when is_integer(Reg), 0 =< Reg, Reg < 16, Type =/= 'double' ->
  {r,Reg}.
  
do_cond(Cond) -> {'cond',Cond}.

do_s(S) -> {'s', case S of false -> 0; true -> 1 end}.

do_label_ref(Label) when is_integer(Label) ->
  {label,Label}.	% symbolic, since offset is not yet computable

do_am1(Am1) ->
  case Am1 of
    #arm_temp{} -> do_reg(Am1);
    {Src1,'rrx'} -> {do_reg(Src1),'rrx'};
    {Src1,ShiftOp,Src2=#arm_temp{}} -> {do_reg(Src1),{ShiftOp,do_reg(Src2)}};
    {Src1,ShiftOp,Imm5} -> {do_reg(Src1),{ShiftOp,{imm5,Imm5}}};
    {Imm8,Imm4} -> {{imm8,Imm8},{imm4,Imm4}}
  end.

do_am2(#am2{src=Src,sign=Sign,offset=Offset}) ->
  NewSrc = do_reg(Src),
  case Offset of
    #arm_temp{} -> {'register_offset',NewSrc,Sign,do_reg(Offset)};
    {Src3,'rrx'} -> {'scaled_register_offset',NewSrc,Sign,do_reg(Src3),'rrx'};
    {Src3,ShiftOp,Imm5} -> {'scaled_register_offset',NewSrc,Sign,do_reg(Src3),{ShiftOp,{imm5,Imm5}}};
    Imm12 -> {'immediate_offset',NewSrc,Sign,{imm12,Imm12}}
  end.

do_am3(#am3{src=Src,sign=Sign,offset=Offset}) ->
  NewSrc = do_reg(Src),
  case Offset of
    #arm_temp{} -> {'register_offset',NewSrc,Sign,do_reg(Offset)};
    _ -> {'immediate_offset',NewSrc,Sign,{'imm8',Offset}}
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

compute_code_size([{_MFA,_Insns,CodeSize,_LabelMap}|Code], Size) ->
  compute_code_size(Code, Size+CodeSize);
compute_code_size([], Size) -> Size.

build_export_map([{{M,F,A},_Insns,CodeSize,_LabelMap}|Code], Address, ExportMap) ->
  build_export_map(Code, Address+CodeSize, [{Address,M,F,A}|ExportMap]);
build_export_map([], _Address, ExportMap) -> ExportMap.

combine_label_maps([{MFA,_Insns,CodeSize,LabelMap}|Code], Address, CLM) ->
  NewCLM = merge_label_map(gb_trees:to_list(LabelMap), MFA, Address, CLM),
  combine_label_maps(Code, Address+CodeSize, NewCLM);
combine_label_maps([], _Address, CLM) -> CLM.

merge_label_map([{Label,Offset}|Rest], MFA, Address, CLM) ->
  NewCLM = gb_trees:insert({MFA,Label}, Address+Offset, CLM),
  merge_label_map(Rest, MFA, Address, NewCLM);
merge_label_map([], _MFA, _Address, CLM) -> CLM.

encode_mfas([{MFA,Insns,CodeSize,LabelMap}|Code], Address, AccCode, Relocs, Options) ->
  print("Generating code for: ~w\n", [MFA], Options),
  print("Offset   | Opcode   | Instruction\n", [], Options),
  {Address1,Relocs1,AccCode1} =
    encode_insns(Insns, Address, Address, LabelMap, Relocs, AccCode, Options),
  ExpectedAddress = Address + CodeSize,
  ?ASSERT(Address1 =:= ExpectedAddress),
  print("Finished.\n", [], Options),
  encode_mfas(Code, Address1, AccCode1, Relocs1, Options);
encode_mfas([], _Address, AccCode, Relocs, _Options) ->
  {AccCode,Relocs}.

encode_insns([I|Insns], Address, FunAddress, LabelMap, Relocs, AccCode, Options) ->
  case I of
    {'.label',L,_} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ?ASSERT(Address =:= LabelAddress),	% sanity check
      print_insn(Address, [], I, Options),
      encode_insns(Insns, Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    {'.reloc',Data,_} ->
      print_insn(Address, [], I, Options),
      Reloc = encode_reloc(Data, Address, FunAddress, LabelMap),
      encode_insns(Insns, Address, FunAddress, LabelMap, [Reloc|Relocs], AccCode, Options);
    {'.long',Value,_} ->
      print_insn(Address, Value, I, Options),
      Segment = <<Value:32/integer-native>>,
      NewAccCode = [Segment|AccCode],
      encode_insns(Insns, Address+4, FunAddress, LabelMap, Relocs, NewAccCode, Options);
    _ ->
      {Op,Arg,_} = fix_pc_refs(I, Address, FunAddress, LabelMap),
      Word = hipe_arm_encode:insn_encode(Op, Arg),
      print_insn(Address, Word, I, Options),
      Segment = <<Word:32/integer-native>>,
      NewAccCode = [Segment|AccCode],
      encode_insns(Insns, Address+4, FunAddress, LabelMap, Relocs, NewAccCode, Options)
  end;
encode_insns([], Address, _FunAddress, _LabelMap, Relocs, AccCode, _Options) ->
  {Address,Relocs,AccCode}.

encode_reloc(Data, Address, FunAddress, LabelMap) ->
  case Data of
    {b_fun,MFAorPrim,Linkage} ->
      %% b and bl are patched the same, so no need to distinguish
      %% call from tailcall
      PatchTypeExt =
	case Linkage of
	  remote -> ?CALL_REMOTE;
	  not_remote -> ?CALL_LOCAL
	end,
      {PatchTypeExt, Address, untag_mfa_or_prim(MFAorPrim)};
    {load_atom,Atom} ->
      {?LOAD_ATOM, Address, Atom};
    {load_address,X} ->
      {?LOAD_ADDRESS, Address, X};
    {sdesc,SDesc} ->
      #arm_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live} = SDesc,
      ExnRA =
	case ExnLab of
	  [] -> [];	% don't cons up a new one
	  ExnLab -> gb_trees:get(ExnLab, LabelMap) + FunAddress
	end,
      {?SDESC, Address,
       ?STACK_DESC(ExnRA, FSize, Arity, Live)}
  end.

untag_mfa_or_prim(#arm_mfa{m=M,f=F,a=A}) -> {M,F,A};
untag_mfa_or_prim(#arm_prim{prim=Prim}) -> Prim.

fix_pc_refs(I, InsnAddress, FunAddress, LabelMap) ->
  case I of
    {b, {Cond,{label,L}}, OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      Imm24 = (LabelAddress - (InsnAddress+8)) div 4,
      %% ensure Imm24 fits in a 24 bit sign-extended field
      ?ASSERT(Imm24 =<   16#7FFFFF),
      ?ASSERT(Imm24 >= -(16#800000)),
      {b, {Cond,{imm24,Imm24 band 16#FFFFFF}}, OrigI};
    {'.pseudo_li', {Dst,{label,L}}, OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      Offset = LabelAddress - (InsnAddress+8),
      {Sign,Imm12} =
	if Offset < 0 -> {'-', -Offset};
	   true -> {'+', Offset}
	end,
      ?ASSERT(Imm12 =< 16#FFF),
      Am2 = {'immediate_offset',{r,15},Sign,{imm12,Imm12}},
      {ldr, {do_cond('al'),Dst,Am2}, OrigI};
    _ -> I
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% Assembly listing support (pp_asm option).
%%%

print(String, Arglist, Options) ->
  ?when_option(pp_asm, Options, io:format(String, Arglist)).

print_insn(Address, Word, I, Options) ->
  ?when_option(pp_asm, Options, print_insn_2(Address, Word, I)).

print_insn_2(Address, Word, {NewI,NewArgs,OrigI}) ->
  io:format("~8.16.0b | ", [Address]),
  print_code_list(word_to_bytes(Word), 0),
  case NewI of
    '.long' ->
      io:format("\t.long ~.16x\n", [Word, "0x"]);
    '.reloc' ->
      io:format("\t.reloc ~w\n", [NewArgs]);
    _ ->
      hipe_arm_pp:pp_insn(OrigI)
  end.

word_to_bytes(W) ->
  case W of
    [] -> [];	% label or other pseudo instruction
    _ -> [(W bsr 24) band 16#FF, (W bsr 16) band 16#FF,
	  (W bsr 8) band 16#FF, W band 16#FF]
  end.

print_code_list([Byte|Rest], Len) ->
  print_byte(Byte),
  print_code_list(Rest, Len+1);
print_code_list([], Len) ->
  fill_spaces(8-(Len*2)),
  io:format(" | ").

print_byte(Byte) ->
  io:format("~2.16.0b", [Byte band 16#FF]).

fill_spaces(N) when N > 0 ->
  io:format(" "),
  fill_spaces(N-1);
fill_spaces(0) ->
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% ADT for previous immediates.
%%% This is a queue (fifo) of the previously defined immediates,
%%% plus a mapping from these immediates to their labels.
%%%
-record(previous, {set, head, tail}). % INV: tail=[] if head=[]

previous_empty() -> #previous{set=gb_trees:empty(), head=[], tail=[]}.

previous_lookup(#previous{set=S}, Imm) -> gb_trees:lookup(Imm, S).

previous_findmin(#previous{head=H}) ->
  case H of
    [X|_] -> X;
    _ -> []
  end.

previous_delmin(#previous{set=S, head=[{_Address,Imm}|H], tail=T}) ->
  {NewH,NewT} =
    case H of
      [] -> {lists:reverse(T), []};
      _ -> {H, T}
    end,
  #previous{set=gb_trees:delete(Imm, S), head=NewH, tail=NewT}.

previous_append(#previous{set=S, head=H, tail=T}, Address, Lab, Imm) ->
  {NewH,NewT} =
    case H of
      [] -> {[{Address,Imm}], []};
      _  -> {H, [{Address,Imm}|T]}
    end,
  #previous{set=gb_trees:insert(Imm, Lab, S), head=NewH, tail=NewT}.

%%%
%%% ADT for pending immediates.
%%% This is a queue (fifo) of immediates pending definition,
%%% plus a mapping from these immediates to their labels,
%%% and a recording of the first (lowest) code address referring
%%% to a pending immediate.
%%%
-record(pending, {set, list, firstref}).

pending_empty() -> #pending{set=gb_trees:empty(), list=[], firstref=[]}.

pending_to_list(#pending{list=L}) -> lists:reverse(L).

pending_lookup(#pending{set=S}, Imm) -> gb_trees:lookup(Imm, S).

pending_firstref(#pending{firstref=F}) -> F.

pending_append(#pending{set=S, list=L, firstref=F}, Address, Lab, RelocOrInt, Imm) ->
  #pending{set=gb_trees:insert(Imm, Lab, S),
	   list=[{Lab,RelocOrInt,Imm}|L],
	   firstref=case F of [] -> Address; _ -> F end}.

pending_size(#pending{list=L}) -> length(L).
