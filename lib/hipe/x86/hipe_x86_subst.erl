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

-ifdef(HIPE_AMD64).
-define(HIPE_X86_SUBST, hipe_amd64_subst).
-else.
-define(HIPE_X86_SUBST, hipe_x86_subst).
-endif.

-module(?HIPE_X86_SUBST).
-export([insn_temps/2, insn_lbls/2]).
-include("../x86/hipe_x86.hrl").

%% These should be moved to hipe_x86 and exported
-type temp()   :: #x86_temp{}.
-type oper()   :: temp() | #x86_imm{} | #x86_mem{}.
-type mfarec() :: #x86_mfa{}.
-type prim()   :: #x86_prim{}.
-type funv()   :: mfarec() | prim() | temp().
-type label()  :: non_neg_integer().
-type insn()   :: tuple(). % for now

-type subst_fun() :: fun((temp()) -> temp()).

%% @doc Maps over the temporaries in an instruction
-spec insn_temps(subst_fun(), insn()) -> insn().
insn_temps(SubstTemp, I) ->
  O = fun(O) -> oper_temps(SubstTemp, O) end,
  case I of
    #alu     {src=S, dst=D}  -> I#alu     {src=O(S), dst=O(D)};
    #cmovcc  {src=S, dst=D}  -> I#cmovcc  {src=O(S), dst=O(D)};
    #cmp     {src=S, dst=D}  -> I#cmp     {src=O(S), dst=O(D)};
    #fmove   {src=S, dst=D}  -> I#fmove   {src=O(S), dst=O(D)};
    #fp_binop{src=S, dst=D}  -> I#fp_binop{src=O(S), dst=O(D)};
    #imul    {src=S, temp=T} -> I#imul    {src=O(S), temp=O(T)};
    #lea     {mem=M, temp=T} -> I#lea     {mem=O(M), temp=O(T)};
    #move    {src=S, dst=D}  -> I#move    {src=O(S), dst=O(D)};
    #movsx   {src=S, dst=D}  -> I#movsx   {src=O(S), dst=O(D)};
    #movzx   {src=S, dst=D}  -> I#movzx   {src=O(S), dst=O(D)};
    #shift   {src=S, dst=D}  -> I#shift   {src=O(S), dst=O(D)};
    #test    {src=S, dst=D}  -> I#test    {src=O(S), dst=O(D)};
    #fp_unop{arg=[]} -> I;
    #fp_unop{arg=A}  -> I#fp_unop{arg=O(A)};
    #move64 {dst=D}  -> I#move64 {dst=O(D)};
    #push   {src=S}  -> I#push   {src=O(S)};
    #pop    {dst=D}  -> I#pop    {dst=O(D)};
    #jmp_switch{temp=T, jtab=J} ->
      I#jmp_switch{temp=O(T), jtab=jtab_temps(SubstTemp, J)};
    #pseudo_call{'fun'=F} ->
      I#pseudo_call{'fun'=funv_temps(SubstTemp, F)};
    #pseudo_spill_fmove{src=S, temp=T, dst=D} ->
      I#pseudo_spill_fmove{src=O(S), temp=O(T), dst=O(D)};
    #pseudo_spill_move{src=S, temp=T, dst=D} ->
      I#pseudo_spill_move{src=O(S), temp=O(T), dst=O(D)};
    #pseudo_tailcall{'fun'=F, stkargs=Stk} ->
      I#pseudo_tailcall{'fun'=funv_temps(SubstTemp, F),
			stkargs=lists:map(O, Stk)};
    #comment{} -> I;
    #jmp_label{} -> I;
    #pseudo_tailcall_prepare{} -> I;
    #pseudo_jcc{} -> I;
    #ret{} -> I
  end.

-spec oper_temps(subst_fun(), oper()) -> oper().
oper_temps(_SubstTemp, I=#x86_imm{}) -> I;
oper_temps(SubstTemp,  T=#x86_temp{}) -> SubstTemp(T);
oper_temps(SubstTemp,  M=#x86_mem{base=Base,off=Off}) ->
  M#x86_mem{base=oper_temps(SubstTemp, Base),
	    off =oper_temps(SubstTemp, Off)}.

-spec funv_temps(subst_fun(), funv()) -> funv().
funv_temps(_SubstTemp, MFA=#x86_mfa{}) -> MFA;
funv_temps(_SubstTemp, P=#x86_prim{}) -> P;
funv_temps(SubstTemp,  T=#x86_temp{}) -> SubstTemp(T).

%% TODO: Undo this ifdeffery at the source (make jtab an #x86_imm{} on x86)
-ifdef(HIPE_AMD64).
jtab_temps(SubstTemp, T=#x86_temp{}) -> SubstTemp(T).
-else.
jtab_temps(_SubstTemp, DataLbl) when is_integer(DataLbl) -> DataLbl.
-endif.

-type lbl_subst_fun() :: fun((label()) -> label()).

%% @doc Maps over the branch targets in an instruction
-spec insn_lbls(lbl_subst_fun(), insn()) -> insn().
insn_lbls(SubstLbl, I) ->
  case I of
    #jmp_label{label=Label} ->
      I#jmp_label{label=SubstLbl(Label)};
    #pseudo_call{sdesc=Sdesc, contlab=Contlab} ->
      I#pseudo_call{sdesc=sdesc_lbls(SubstLbl, Sdesc),
		    contlab=SubstLbl(Contlab)};
    #pseudo_jcc{true_label=T, false_label=F} ->
      I#pseudo_jcc{true_label=SubstLbl(T), false_label=SubstLbl(F)}
  end.

sdesc_lbls(_SubstLbl, Sdesc=#x86_sdesc{exnlab=[]}) -> Sdesc;
sdesc_lbls(SubstLbl, Sdesc=#x86_sdesc{exnlab=Exnlab}) ->
  Sdesc#x86_sdesc{exnlab=SubstLbl(Exnlab)}.
