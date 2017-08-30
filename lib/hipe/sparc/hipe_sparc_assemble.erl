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

-module(hipe_sparc_assemble).
-export([assemble/4]).

-include("../main/hipe.hrl").	% for VERSION_STRING, when_option
-include("hipe_sparc.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-include("../misc/hipe_sdi.hrl").
-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

assemble(CompiledCode, Closures, Exports, Options) ->
  print("****************** Assembling *******************\n", [], Options),
  %%
  Code = [{MFA,
	   hipe_sparc:defun_code(Defun),
	   hipe_sparc:defun_data(Defun)}
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
%%%
%%% Assembly Pass 2.
%%% Perform short/long form optimisation for jumps.
%%%
%%% Result is {MFA,NewCode,CodeSize,LabelMap} list.
%%%

translate(Code, ConstMap) ->
  translate_mfas(Code, ConstMap, []).

translate_mfas([{MFA,Insns,_Data}|Code], ConstMap, NewCode) ->
  {NewInsns,CodeSize,LabelMap} =
    translate_insns(Insns, MFA, ConstMap, hipe_sdi:pass1_init(), 0, []),
  translate_mfas(Code, ConstMap, [{MFA,NewInsns,CodeSize,LabelMap}|NewCode]);
translate_mfas([], _ConstMap, NewCode) ->
  lists:reverse(NewCode).

translate_insns([I|Insns], MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  NewIs = translate_insn(I, MFA, ConstMap),
  add_insns(NewIs, Insns, MFA, ConstMap, SdiPass1, Address, NewInsns);
translate_insns([], _MFA, _ConstMap, SdiPass1, Address, NewInsns) ->
  {LabelMap,CodeSizeIncr} = hipe_sdi:pass2(SdiPass1),
  {lists:reverse(NewInsns), Address+CodeSizeIncr, LabelMap}.

add_insns([I|Is], Insns, MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  NewSdiPass1 =
    case I of
      {'.label',L,_} ->
	hipe_sdi:pass1_add_label(SdiPass1, Address, L);
      {bp_sdi,{_,_,{label,L}},_} ->	% BP has 19-bit offset
	SdiInfo = #sdi_info{incr=(12-4),lb=-16#40000*4,ub=16#3FFFF*4},
	hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo);
      %% {br_sdi,_,_} -> add_insns_br(I, SdiPass1, Address);
      _ ->
	SdiPass1
    end,
  Address1 = Address + insn_size(I),
  add_insns(Is, Insns, MFA, ConstMap, NewSdiPass1, Address1, [I|NewInsns]);
add_insns([], Insns, MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  translate_insns(Insns, MFA, ConstMap, SdiPass1, Address, NewInsns).

-ifdef(notdef).	% XXX: only for sparc64, alas
add_insns_br(I, SdiPass1, Address) ->	% BR has 16-bit offset
  {br_sdi,{_,_,_,{label,L}},_} = I,
  SdiInfo = #sdi_info{incr=(12-4),lb=-16#8000*4,ub=16#7FFF*4},
  hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo).
-endif.

insn_size(I) ->
  case I of
    {'.label',_,_} -> 0;
    {'.reloc',_,_} -> 0;
    _ -> 4	% b{p,r}_sdi included in this case
  end.

translate_insn(I, MFA, ConstMap) ->	% -> [{Op,Opnd,OrigI}]
  case I of
    #alu{} -> do_alu(I);
    #bp{} -> do_bp(I);
    %% #br{} -> do_br(I);
    #call_rec{} -> do_call_rec(I);
    #call_tail{} -> do_call_tail(I);
    #comment{} -> [];
    #jmp{} -> do_jmp(I);
    #jmpl{} -> do_jmpl(I);
    #label{} -> do_label(I);
    %% pseudo_bp: eliminated before assembly
    %% pseudo_br: eliminated before assembly
    %% pseudo_call: eliminated before assembly
    %% pseudo_call_prepare: eliminated before assembly
    %% pseudo_move: eliminated before assembly
    %% pseudo_ret: eliminated before assembly
    #pseudo_set{} -> do_pseudo_set(I, MFA, ConstMap);
    %% pseudo_tailcall: eliminated before assembly
    %% pseudo_tailcall_prepare: eliminated before assembly
    #rdy{} -> do_rdy(I);
    #sethi{} -> do_sethi(I);
    #store{} -> do_store(I);
    #fp_binary{} -> do_fp_binary(I);
    #fp_unary{} -> do_fp_unary(I);
    #pseudo_fload{} -> do_pseudo_fload(I);
    %% #pseudo_fmove: eliminated before assembly
    #pseudo_fstore{} -> do_pseudo_fstore(I);
    _ -> exit({?MODULE,translate_insn,I})
  end.

do_alu(I) ->
  #alu{aluop=AluOp,src1=Src1,src2=Src2,dst=Dst} = I,
  NewDst = do_reg(Dst),
  NewSrc1 = do_reg(Src1),
  NewSrc2 = do_reg_or_imm(Src2),
  [{AluOp, {NewSrc1,NewSrc2,NewDst}, I}].

do_bp(I) ->
  #bp{'cond'=Cond,pred=Pred,label=Label} = I,
  NewLabel = {label,Label},
  case Cond of
    'a' ->
      [{ba, NewLabel, I}];	% 3 more offset bits
    _ ->
      NewCond = {'cond',Cond},
      NewPred = {pred,Pred},
      [{bp_sdi, {NewCond,NewPred,NewLabel}, I}]
  end.

-ifdef(notdef).	% XXX: only for sparc64, alas
do_br(I) ->
  #br{rcond=RCond,pred=Pred,src=Src,label=Label} = I,
  NewRCond = {rcond,RCond},
  NewPred = {pred,Pred},
  NewSrc = do_reg(Src),
  NewLabel = {label,Label},
  [{br_sdi, {NewRCond,NewPred,NewSrc,NewLabel}, I}].
-endif.

do_call_rec(I) ->
  #call_rec{'fun'=Fun,sdesc=SDesc,linkage=Linkage} = I,
  [{'.reloc', {call,Fun,Linkage}, #comment{term='fun'}},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}},
   {call, {disp30,0}, I}].

do_call_tail(I) ->
  #call_tail{'fun'=Fun,linkage=Linkage} = I,
  [{'.reloc', {call,Fun,Linkage}, #comment{term='fun'}},
   {call, {disp30,0}, I}].

do_jmp(I) ->
  #jmp{src1=Src1,src2=Src2} = I,
  NewSrc1 = do_reg(Src1),
  NewSrc2 = do_reg_or_imm(Src2),
  NewDst = {r,0},
  [{jmpl, {NewSrc1,NewSrc2,NewDst}, I}].

do_jmpl(I) ->
  #jmpl{src=Src,sdesc=SDesc} = I,
  NewSrc1 = do_reg(Src),
  NewSrc2 = {simm13,0},
  NewDst = {r,15},	% %o7
  [{'.reloc', {sdesc,SDesc}, #comment{term=sdesc}},
   {jmpl, {NewSrc1,NewSrc2,NewDst}, I}].

do_label(I) ->
  #label{label=Label} = I,
  [{'.label', Label, I}].

do_pseudo_set(I, MFA, ConstMap) ->
  #pseudo_set{imm=Imm,dst=Dst} = I,
  RelocData =
    case Imm of
      Atom when is_atom(Atom) ->
	{load_atom, Atom};
%%%      {mfa,MFAorPrim,Linkage} ->
%%%	Tag =
%%%	  case Linkage of
%%%	    remote -> remote_function;
%%%	    not_remote -> local_function
%%%	  end,
%%%	{load_address, {Tag,untag_mfa_or_prim(MFAorPrim)}};
      {Label,constant} ->
	ConstNo = hipe_pack_constants:find_const({MFA,Label}, ConstMap),
	{load_address, {constant,ConstNo}};
      {Label,closure} ->
	{load_address, {closure,Label}};
      {Label,c_const} ->
	{load_address, {c_const,Label}}
    end,
  NewDst = do_reg(Dst),
  [{'.reloc', RelocData, #comment{term=reloc}},
   {sethi, {{uimm22,0},NewDst}, I},
   {'or', {NewDst,{simm13,0},NewDst}, I}].

do_rdy(I) ->
  #rdy{dst=Dst} = I,
  NewDst = do_reg(Dst),
  [{rd, {y,NewDst}, I}].

do_sethi(I) ->
  #sethi{uimm22=#sparc_uimm22{value=UImm22},dst=Dst} = I,
  NewUImm22 = {uimm22,UImm22},
  NewDst = do_reg(Dst),
  [{sethi, {NewUImm22,NewDst}, I}].

do_store(I) ->
  #store{stop=StOp,src=Src,base=Base,disp=Disp} = I,
  NewSrc = do_reg(Src),
  NewBase = do_reg(Base),
  NewDisp = do_reg_or_imm(Disp),
  [{StOp, {NewSrc,NewBase,NewDisp}, I}].

do_fp_binary(I) ->
  #fp_binary{fp_binop=FpBinOp,src1=Src1,src2=Src2,dst=Dst} = I,
  NewSrc1 = do_fpreg(Src1),
  NewSrc2 = do_fpreg(Src2),
  NewDst = do_fpreg(Dst),
  [{FpBinOp, {NewSrc1,NewSrc2,NewDst}, I}].

do_fp_unary(I) ->
  #fp_unary{fp_unop=FpUnOp,src=Src,dst=Dst} = I,
  NewSrc = do_fpreg(Src),
  NewDst = do_fpreg(Dst),
  [{FpUnOp, {NewSrc,NewDst}, I}].

do_pseudo_fload(I) ->
  #pseudo_fload{base=Base,disp=Disp,dst=Dst,is_single=IsSingle} = I,
  NewBase = do_reg(Base),
  #sparc_simm13{value=RawDisp} = Disp,
  {fr,RawDst} = FrRawDst = do_fpreg(Dst),
  case IsSingle of
    true ->
      [{'ldf', {NewBase,{simm13,RawDisp},FrRawDst}, I}];
    _ ->
      [{'ldf', {NewBase,{simm13,RawDisp},FrRawDst}, I},
       {'ldf', {NewBase,{simm13,RawDisp+4},{fr,RawDst+1}}, I}]
  end.

do_pseudo_fstore(I) ->
  #pseudo_fstore{src=Src,base=Base,disp=Disp} = I,
  {fr,RawSrc} = FrRawSrc = do_fpreg(Src),
  NewBase = do_reg(Base),
  #sparc_simm13{value=RawDisp} = Disp,
  [{'stf', {FrRawSrc,NewBase,{simm13,RawDisp}}, I},
   {'stf', {{fr,RawSrc+1},NewBase,{simm13,RawDisp+4}}, I}].

%% map a virtual double-precision fp reg in [0,15] to its
%% corresponding single-precision fp reg in [0,2,4,...,28,30]
do_fpreg(#sparc_temp{reg=Reg,type='double'})
  when is_integer(Reg), 0 =< Reg, Reg < 16 ->
  {fr,2*Reg}.

do_reg(#sparc_temp{reg=Reg,type=Type})
  when is_integer(Reg), 0 =< Reg, Reg < 32, Type =/= 'double' ->
  {r,Reg}.
  
do_reg_or_imm(Src) ->
  case Src of
    #sparc_temp{} ->
      do_reg(Src);
    #sparc_simm13{value=Value} when is_integer(Value), -4096 =< Value, Value =< 4095 ->
      {simm13, Value band 16#1fff};
    #sparc_uimm5{value=Value} when is_integer(Value), 0 =< Value, Value =< 31 ->
      {uimm5, Value};
    #sparc_uimm6{value=Value} when is_integer(Value), 0 =< Value, Value =< 63 ->
      {uimm6, Value}
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
      Reloc = encode_reloc(Data, Address, FunAddress, LabelMap),
      encode_insns(Insns, Address, FunAddress, LabelMap, [Reloc|Relocs], AccCode, Options);
    {bp_sdi,_,_} ->
      encode_insns(fix_bp_sdi(I, Insns, Address, FunAddress, LabelMap),
		   Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    %% {br_sdi,_,_} ->
    %%  encode_insns(fix_br_sdi(I, Insns, Address, FunAddress, LabelMap),
    %%		   Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    _ ->
      {Op,Arg,_} = fix_jumps(I, Address, FunAddress, LabelMap),
      Word = hipe_sparc_encode:insn_encode(Op, Arg),
      print_insn(Address, Word, I, Options),
      Segment = <<Word:32/integer-big>>,
      NewAccCode = [Segment|AccCode],
      encode_insns(Insns, Address+4, FunAddress, LabelMap, Relocs, NewAccCode, Options)
  end;
encode_insns([], Address, _FunAddress, _LabelMap, Relocs, AccCode, _Options) ->
  {Address,Relocs,AccCode}.

encode_reloc(Data, Address, FunAddress, LabelMap) ->
  case Data of
    {call,MFAorPrim,Linkage} ->
      %% call_rec and call_tail are patched the same, so no need to distinguish
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
      #sparc_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live} = SDesc,
      ExnRA =
	case ExnLab of
	  [] -> [];	% don't cons up a new one
	  ExnLab -> gb_trees:get(ExnLab, LabelMap) + FunAddress
	end,
      {?SDESC, Address,
       ?STACK_DESC(ExnRA, FSize, Arity, Live)}
  end.

untag_mfa_or_prim(#sparc_mfa{m=M,f=F,a=A}) -> {M,F,A};
untag_mfa_or_prim(#sparc_prim{prim=Prim}) -> Prim.

fix_bp_sdi(I, Insns, InsnAddress, FunAddress, LabelMap) ->
  {bp_sdi,Opnds,OrigI} = I,
  {{'cond',Cond},{pred,Pred},Label} = Opnds,
  {label,L} = Label,
  LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
  BD = (LabelAddress - InsnAddress) div 4,
  if BD >= -16#40000, BD =< 16#3FFFF ->
      [{bp, Opnds, OrigI} | Insns];
     true ->
      %% bp<cond>,<pred> L; Delay
      %% -->
      %% bp<!cond>,<!pred> 1f; Delay; ba L; nop; 1:
      [Delay|Rest] = Insns,
      NewCond = hipe_sparc:negate_cond(Cond),
      NewPred = 1.0 - Pred,
      [{bp,
	{{'cond',NewCond},{pred,NewPred},'.+16'},
	#bp{'cond'=NewCond,pred=NewPred,label='.+16'}}, % pp will be ugly
       Delay,	% should be a NOP
       {ba, Label, #bp{'cond'='a',pred=1.0,label=L}},
       {sethi, {{uimm22,0},{r,0}}, #comment{term=nop}} |
       Rest]
  end.

-ifdef(notdef).	% XXX: only for sparc64, alas
fix_br_sdi(I, Insns, InsnAddress, FunAddress, LabelMap) ->
  {br_sdi,Opnds,OrigI} = I,
  {{rcond,RCond},{pred,Pred},Src,{label,L}} = Opnds,
  LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
  BD = (LabelAddress - InsnAddress) div 4,
  if BD >= -16#8000, BD =< 16#7FFF ->
      [{br, Opnds, OrigI} | Insns];
     true ->
      %% br<rcond>,<pred> reg, L; Delay
      %% -->
      %% br<!rcond>,<!pred> reg, 1f; Delay; ba L; nop; 1:
      [Delay|Rest] = Insns,
      {reg,SrcReg} = Src,
      NewRCond = hipe_sparc:negate_rcond(RCond),
      NewPred = 1.0 - Pred,
      [{br,
	{{rcond,NewRCond},{pred,NewPred},Src,'.+16'},
	#br{rcond=NewRCond,pred=NewPred,src=SrcReg,label='.+16'}}, % pp will be ugly
       Delay,	% should be a NOP
       {ba, {label,L}, #bp{'cond'='a',pred=1.0,label=L}},
       {sethi, {{uimm22,0},{r,0}}, #comment{term=nop}} |
       Rest]
  end.
-endif.

fix_jumps(I, InsnAddress, FunAddress, LabelMap) ->
  case I of
    {ba, {label,L}, OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      BD = (LabelAddress - InsnAddress) div 4,
      %% ensure BD fits in a 22 bit sign-extended field
      ?ASSERT(BD =<  16#1FFFFF),
      ?ASSERT(BD >= -16#200000),
      {ba, {disp22,BD band 16#3FFFFF}, OrigI};
    {bp, {Cond,Pred,Target}, OrigI} ->
      LabelAddress =
	case Target of
	  {label,L} -> gb_trees:get(L, LabelMap) + FunAddress;
	  '.+16' -> InsnAddress + 16
	end,
      BD = (LabelAddress - InsnAddress) div 4,
      %% ensure BD fits in a 19 bit sign-extended field
      ?ASSERT(BD =<  16#3FFFF),
      ?ASSERT(BD >= -16#40000),
      {bp, {Cond,px(Pred),{disp19,BD band 16#7FFFF}}, OrigI};
    %% {br, _, _} -> fix_br(I, InsnAddress, FunAddress, LabelMap);
    _ -> I
  end.

-ifdef(notdef).	% XXX: only for sparc64, alas
fix_br(I, InsnAddress, FunAddress, LabelMap) ->
  {br, {RCond,Pred,Src,Target}, OrigI} = I,
  LabelAddress =
    case Target of
      {label,L} -> gb_trees:get(L, LabelMap) + FunAddress;
      '.+16' -> InsnAddress + 16
    end,
  BD = (LabelAddress - InsnAddress) div 4,
  %% ensure BD fits in a 16 bit sign-extended field
  ?ASSERT(BD =<  16#7FFF),
  ?ASSERT(BD >= -16#8000),
  {br, {RCond,px(Pred),Src,{disp16,BD band 16#FFFF}}, OrigI}.
-endif.

px({pred,Pred}) ->	% XXX: use pt/pn throughout entire backend
  {pred, if Pred >= 0.5 -> 'pt'; true -> 'pn' end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% Assembly listing support (pp_asm option).
%%%

print(String, Arglist, Options) ->
  ?when_option(pp_asm, Options, io:format(String, Arglist)).

print_insn(Address, Word, I, Options) ->
  ?when_option(pp_asm, Options, print_insn_2(Address, Word, I)).

print_insn_2(Address, Word, {_,_,OrigI}) ->
  io:format("~8.16.0b | ", [Address]),
  print_code_list(word_to_bytes(Word), 0),
  hipe_sparc_pp:pp_insn(OrigI).

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
