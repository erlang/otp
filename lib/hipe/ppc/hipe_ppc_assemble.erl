%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_ppc_assemble).
-export([assemble/4]).

-include("../main/hipe.hrl").	% for VERSION_STRING, when_option
-include("hipe_ppc.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-include("../misc/hipe_sdi.hrl").
-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

assemble(CompiledCode, Closures, Exports, Options) ->
  print("****************** Assembling *******************\n", [], Options),
  %%
  Code = [{MFA,
	   hipe_ppc:defun_code(Defun),
	   hipe_ppc:defun_data(Defun)}
	  || {MFA, Defun} <- CompiledCode],
  %%
  {ConstAlign,ConstSize,ConstMap,RefsFromConsts} =
    hipe_pack_constants:pack_constants(Code),
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
      {bc_sdi,{_,{label,L},_},_} ->
	SdiInfo = #sdi_info{incr=(8-4),lb=-16#2000*4,ub=16#1FFF*4},
	hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo);
      _ ->
	SdiPass1
    end,
  Address1 = Address + insn_size(I),
  add_insns(Is, Insns, MFA, ConstMap, NewSdiPass1, Address1, [I|NewInsns]);
add_insns([], Insns, MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  translate_insns(Insns, MFA, ConstMap, SdiPass1, Address, NewInsns).

insn_size(I) ->
  case I of
    {'.label',_,_} -> 0;
    {'.reloc',_,_} -> 0;
    _ -> 4	% bc_sdi included in this case
  end.

translate_insn(I, MFA, ConstMap) ->	% -> [{Op,Opnd,OrigI}]
  case I of
    #alu{} -> do_alu(I);
    #b_fun{} -> do_b_fun(I);
    #b_label{} -> do_b_label(I);
    #bc{} -> do_bc(I);
    #bctr{} -> do_bctr(I);
    #bctrl{} -> do_bctrl(I);
    #bl{} -> do_bl(I);
    #blr{} -> do_blr(I);
    #comment{} -> [];
    #cmp{} -> do_cmp(I);
    #label{} -> do_label(I);
    #load{} -> do_load(I);
    #loadx{} -> do_loadx(I);
    #mfspr{} -> do_mfspr(I);
    #mtcr{} -> do_mtcr(I);
    #mtspr{} -> do_mtspr(I);
    %% pseudo_bc: eliminated before assembly
    %% pseudo_call: eliminated before assembly
    %% pseudo_call_prepare: eliminated before assembly
    #pseudo_li{} -> do_pseudo_li(I, MFA, ConstMap);
    %% pseudo_move: eliminated before assembly
    %% pseudo_tailcall: eliminated before assembly
    %% pseudo_tailcall_prepare: eliminated before assembly
    #store{} -> do_store(I);
    #storex{} -> do_storex(I);
    #unary{} -> do_unary(I);
    #lfd{} -> do_lfd(I);
    #stfd{} -> do_stfd(I);
    #fp_binary{} -> do_fp_binary(I);
    #fp_unary{} -> do_fp_unary(I);
    _ -> exit({?MODULE,translate_insn,I})
  end.

do_alu(I) ->
  #alu{aluop=AluOp,dst=Dst,src1=Src1,src2=Src2} = I,
  NewDst = do_reg(Dst),
  NewSrc1 = do_reg(Src1),
  NewSrc2 = do_reg_or_imm(Src2),
  {NewI,NewOpnds} =
    case AluOp of
      'slwi' -> {'rlwinm', do_slwi_opnds(NewDst, NewSrc1, NewSrc2)};
      'slwi.' -> {'rlwinm.', do_slwi_opnds(NewDst, NewSrc1, NewSrc2)};
      'srwi' -> {'rlwinm', do_srwi_opnds(NewDst, NewSrc1, NewSrc2)};
      'srwi.' -> {'rlwinm.', do_srwi_opnds(NewDst, NewSrc1, NewSrc2)};
      'srawi' -> {'srawi', {NewDst,NewSrc1,do_srawi_src2(NewSrc2)}};
      'srawi.' -> {'srawi.', {NewDst,NewSrc1,do_srawi_src2(NewSrc2)}};
      %ppc64 extension
      'sldi' -> {'rldicr', do_sldi_opnds(NewDst, NewSrc1, NewSrc2)};
      'sldi.' -> {'rldicr.', do_sldi_opnds(NewDst, NewSrc1, NewSrc2)};
      'srdi' -> {'rldicl', do_srdi_opnds(NewDst, NewSrc1, NewSrc2)};
      'srdi.' -> {'rldicl.', do_srdi_opnds(NewDst, NewSrc1, NewSrc2)};
      'sradi' -> {'sradi', {NewDst,NewSrc1,do_sradi_src2(NewSrc2)}};
      'sradi.' -> {'sradi.', {NewDst,NewSrc1,do_sradi_src2(NewSrc2)}};
      _ -> {AluOp, {NewDst,NewSrc1,NewSrc2}}
    end,
  [{NewI, NewOpnds, I}].

do_slwi_opnds(Dst, Src1, {uimm,N}) when is_integer(N), 0 =< N, N < 32 ->
  {Dst, Src1, {sh,N}, {mb,0}, {me,31-N}}.

do_srwi_opnds(Dst, Src1, {uimm,N}) when is_integer(N), 0 =< N, N < 32 ->
  %% SH should be 0 (not 32) when N is 0
  {Dst, Src1, {sh,(32-N) band 31}, {mb,N}, {me,31}}.

do_srawi_src2({uimm,N}) when  is_integer(N), 0 =< N, N < 32 -> {sh,N}.

%% ppc64 extension
do_sldi_opnds(Dst, Src1, {uimm,N}) when is_integer(N), 0 =< N, N < 64 ->
  {Dst, Src1, {sh6,N}, {me6,63-N}}.

do_srdi_opnds(Dst, Src1, {uimm,N}) when is_integer(N), 0 =< N, N < 64 ->
  %% SH should be 0 (not 64) when N is 0
  {Dst, Src1, {sh6,(64-N) band 63}, {mb6,N}}.

do_sradi_src2({uimm,N}) when is_integer(N), 0 =< N, N < 64 -> {sh6,N}.

do_b_fun(I) ->
  #b_fun{'fun'=Fun,linkage=Linkage} = I,
  [{'.reloc', {b_fun,Fun,Linkage}, #comment{term='fun'}},
   {b, {{li,0}}, I}].

do_b_label(I) ->
  #b_label{label=Label} = I,
  [{b, do_label_ref(Label), I}].

do_bc(I) ->
  #bc{bcond=BCond,label=Label,pred=Pred} = I,
  [{bc_sdi, {{bcond,BCond},do_label_ref(Label),{pred,Pred}}, I}].

do_bctr(I) ->
  [{bcctr, {{bo,2#10100},{bi,0}}, I}].

do_bctrl(I) ->
  #bctrl{sdesc=SDesc} = I,
  [{bcctrl, {{bo,2#10100},{bi,0}}, I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_bl(I) ->
  #bl{'fun'=Fun,sdesc=SDesc,linkage=Linkage} = I,
  [{'.reloc', {b_fun,Fun,Linkage}, #comment{term='fun'}},
   {bl, {{li,0}}, I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_blr(I) ->
  [{bclr, {{bo,2#10100},{bi,0}}, I}].

do_cmp(I) ->
  #cmp{cmpop=CmpOp,src1=Src1,src2=Src2} = I,
  NewSrc1 = do_reg(Src1),
  NewSrc2 = do_reg_or_imm(Src2),
  {RealOp,L} =
    case CmpOp of
      'cmpd' -> {'cmp',1};
      'cmpdi' -> {'cmpi',1};
      'cmpld' -> {'cmpl',1};
      'cmpldi' -> {'cmpli',1};
      'cmp' -> {CmpOp,0};
      'cmpi' -> {CmpOp,0};
      'cmpl' -> {CmpOp,0};
      'cmpli' -> {CmpOp,0}
    end,
  [{RealOp, {{crf,0},L,NewSrc1,NewSrc2}, I}].

do_label(I) ->
  #label{label=Label} = I,
  [{'.label', Label, I}].

do_load(I) ->
  #load{ldop=LdOp,dst=Dst,disp=Disp,base=Base} = I,
  NewDst = do_reg(Dst),
  NewDisp =
    case LdOp of
      'ld' -> do_disp_ds(Disp);
      'ldu' -> do_disp_ds(Disp);
      'lwa' -> do_disp_ds(Disp);
      _ -> do_disp(Disp)
    end,
  NewBase = do_reg(Base),
  [{LdOp, {NewDst,NewDisp,NewBase}, I}].

do_loadx(I) ->
  #loadx{ldxop=LdxOp,dst=Dst,base1=Base1,base2=Base2} = I,
  NewDst = do_reg(Dst),
  NewBase1 = do_reg(Base1),
  NewBase2 = do_reg(Base2),
  [{LdxOp, {NewDst,NewBase1,NewBase2}, I}].

do_mfspr(I) ->
  #mfspr{dst=Dst,spr=SPR} = I,
  NewDst = do_reg(Dst),
  NewSPR = do_spr(SPR),
  [{mfspr, {NewDst,NewSPR}, I}].

do_mtcr(I) ->
  #mtcr{src=Src} = I,
  NewSrc = do_reg(Src),
  [{mtcrf, {{crm,16#80},NewSrc}, I}].

do_mtspr(I) ->
  #mtspr{spr=SPR,src=Src} = I,
  NewSPR = do_spr(SPR),
  NewSrc = do_reg(Src),
  [{mtspr, {NewSPR,NewSrc}, I}].

do_pseudo_li(I, MFA, ConstMap) ->
  #pseudo_li{dst=Dst,imm=Imm} = I,
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
  Simm0 = {simm,0},
  Uimm0 = {uimm,0},
  case get(hipe_target_arch) of
    powerpc ->
      [{'.reloc', RelocData, #comment{term=reloc}},
       {addi, {NewDst,{r,0},Simm0}, I},
       {addis, {NewDst,NewDst,Simm0}, I}];
    ppc64 ->
      [{'.reloc', RelocData, #comment{term=reloc}},
       {addis, {NewDst,{r,0},Simm0}, I},   % @highest
       {ori, {NewDst,NewDst,Uimm0}, I},    % @higher
       {rldicr, {NewDst,NewDst,{sh6,32},{me6,31}}, I},
       {oris, {NewDst,NewDst,Uimm0}, I},   % @h
       {ori, {NewDst,NewDst,Uimm0}, I}]    % @l
  end.

do_store(I) ->
  #store{stop=StOp,src=Src,disp=Disp,base=Base} = I,
  NewSrc = do_reg(Src),
  NewDisp =
    case StOp of
      'std' -> do_disp_ds(Disp);
      'stdu' -> do_disp_ds(Disp);
      _ -> do_disp(Disp)
    end,
  NewBase = do_reg(Base),
  [{StOp, {NewSrc,NewDisp,NewBase}, I}].

do_storex(I) ->
  #storex{stxop=StxOp,src=Src,base1=Base1,base2=Base2} = I,
  NewSrc = do_reg(Src),
  NewBase1 = do_reg(Base1),
  NewBase2 = do_reg(Base2),
  [{StxOp, {NewSrc,NewBase1,NewBase2}, I}].

do_unary(I) ->
  #unary{unop=UnOp,dst=Dst,src=Src} = I,
  NewDst = do_reg(Dst),
  NewSrc = do_reg(Src),
  {NewI,NewOpnds} =
    case UnOp of
      {RLWINM,SH,MB,ME} -> {RLWINM, {NewDst,NewSrc,{sh,SH},{mb,MB},{me,ME}}};
      _ -> {UnOp, {NewDst,NewSrc}}
    end,
  [{NewI, NewOpnds, I}].

do_lfd(I) ->
  #lfd{dst=Dst,disp=Disp,base=Base} = I,
  NewDst = do_fpreg(Dst),
  NewDisp = do_disp(Disp),
  NewBase = do_reg(Base),
  [{lfd, {NewDst,NewDisp,NewBase}, I}].

do_stfd(I) ->
  #stfd{src=Src,disp=Disp,base=Base} = I,
  NewSrc = do_fpreg(Src),
  NewDisp = do_disp(Disp),
  NewBase = do_reg(Base),
  [{stfd, {NewSrc,NewDisp,NewBase}, I}].

do_fp_binary(I) ->
  #fp_binary{fp_binop=FpBinOp,dst=Dst,src1=Src1,src2=Src2} = I,
  NewDst = do_fpreg(Dst),
  NewSrc1 = do_fpreg(Src1),
  NewSrc2 = do_fpreg(Src2),
  [{FpBinOp, {NewDst,NewSrc1,NewSrc2}, I}].

do_fp_unary(I) ->
  #fp_unary{fp_unop=FpUnOp,dst=Dst,src=Src} = I,
  NewDst = do_fpreg(Dst),
  NewSrc = do_fpreg(Src),
  [{FpUnOp, {NewDst,NewSrc}, I}].

do_fpreg(#ppc_temp{reg=Reg,type='double'}) when is_integer(Reg), 0 =< Reg, Reg < 32 ->
  {fr,Reg}.

do_reg(#ppc_temp{reg=Reg,type=Type})
  when is_integer(Reg), 0 =< Reg, Reg < 32, Type =/= 'double' ->
  {r,Reg}.
  
do_label_ref(Label) when is_integer(Label) ->
  {label,Label}.	% symbolic, since offset is not yet computable

do_reg_or_imm(Src) ->
  case Src of
    #ppc_temp{} ->
      do_reg(Src);
    #ppc_simm16{value=Value} when is_integer(Value), -32768 =< Value, Value =< 32767 ->
      {simm, Value band 16#ffff};
    #ppc_uimm16{value=Value} when is_integer(Value), 0 =< Value, Value =< 65535 ->
      {uimm, Value}
  end.

do_disp(Disp) when is_integer(Disp), -32768 =< Disp, Disp =< 32767 ->
  {d, Disp band 16#ffff}.

do_disp_ds(Disp) when is_integer(Disp),
		      -32768 =< Disp, Disp =< 32767, Disp band 3 =:= 0 ->
  {ds, (Disp band 16#ffff) bsr 2}.

do_spr(SPR) ->
  SPR_NR =
    case SPR of
      'xer' -> 1;
      'lr' -> 8;
      'ctr' -> 9
    end,
  {spr,SPR_NR}.

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
    {bc_sdi,_,_} ->
      encode_insns(fix_bc_sdi(I, Insns, Address, FunAddress, LabelMap),
		   Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    _ ->
      {Op,Arg,_} = fix_jumps(I, Address, FunAddress, LabelMap),
      Word = hipe_ppc_encode:insn_encode(Op, Arg),
      print_insn(Address, Word, I, Options),
      Segment = <<Word:32/integer-big>>,
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
      #ppc_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live} = SDesc,
      ExnRA =
	case ExnLab of
	  [] -> [];	% don't cons up a new one
	  ExnLab -> gb_trees:get(ExnLab, LabelMap) + FunAddress
	end,
      {?SDESC, Address,
       ?STACK_DESC(ExnRA, FSize, Arity, Live)}
  end.

untag_mfa_or_prim(#ppc_mfa{m=M,f=F,a=A}) -> {M,F,A};
untag_mfa_or_prim(#ppc_prim{prim=Prim}) -> Prim.

fix_bc_sdi(I, Insns, InsnAddress, FunAddress, LabelMap) ->
  {bc_sdi,Opnds,OrigI} = I,
  {{bcond,BCond},Label,{pred,Pred}} = Opnds,
  {label,L} = Label,
  LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
  BD = (LabelAddress - InsnAddress) div 4,
  if BD >= -(16#2000), BD =< 16#1FFF ->
      [{bc, Opnds, OrigI} | Insns];
     true ->
      NewBCond = hipe_ppc:negate_bcond(BCond),
      NewPred = 1.0 - Pred,
      [{bc,
	{{bcond,NewBCond},'.+8',{pred,NewPred}},
	#bc{bcond=NewBCond,label='.+8',pred=NewPred}}, %% pp will be ugly
       {b, Label, #b_label{label=L}} |
       Insns]
  end.

fix_jumps(I, InsnAddress, FunAddress, LabelMap) ->
  case I of
    {b, {label,L}, OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      LI = (LabelAddress - InsnAddress) div 4,
      %% ensure LI fits in a 24 bit sign-extended field
      ?ASSERT(LI =<   16#7FFFFF),
      ?ASSERT(LI >= -(16#800000)),
      {b, {{li,LI band 16#FFFFFF}}, OrigI};
    {bc, {{bcond,BCond},Target,{pred,Pred}}, OrigI} ->
      LabelAddress =
	case Target of
	  {label,L} -> gb_trees:get(L, LabelMap) + FunAddress;
	  '.+8' -> InsnAddress + 8
	end,
      BD = (LabelAddress - InsnAddress) div 4,
      %% ensure BD fits in a 14 bit sign-extended field
      ?ASSERT(BD =<   16#1FFF),
      ?ASSERT(BD >= -(16#2000)),
      {BO1,BI} = split_bcond(BCond),
      BO = mk_bo(BO1, Pred, BD),
      {bc, {{bo,BO},{bi,BI},{bd,BD band 16#3FFF}}, OrigI};
    _ -> I
  end.

split_bcond(BCond) -> % {BO[1], BI for CR0}
  case BCond of
    'lt' -> {1, 2#0000};
    'ge' -> {0, 2#0000};	% not lt
    'gt' -> {1, 2#0001};
    'le' -> {0, 2#0001};	% not gt
    'eq' -> {1, 2#0010};
    'ne' -> {0, 2#0010};	% not eq
    'so' -> {1, 2#0011};
    'ns' -> {0, 2#0011}		% not so
  end.

mk_bo(BO1, Pred, BD) ->
  (BO1 bsl 3) bor 2#00100 bor mk_y(Pred, BD).

mk_y(Pred, BD) ->
  if Pred < 0.5 ->	% not taken
      if BD < 0 -> 1; true -> 0 end;
     true ->		% taken
      if BD < 0 -> 0; true -> 1 end
  end.

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
  hipe_ppc_pp:pp_insn(OrigI).

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
