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

-module(hipe_arm_subst).
-export([insn_temps/2, insn_lbls/2]).
-include("hipe_arm.hrl").

%% These should be moved to hipe_arm and exported
-type temp()    :: #arm_temp{}.
-type shiftop() :: lsl | lsr | asr | ror.
-type imm4()    :: 0..15.
-type imm5()    :: 0..31.
-type imm8()    :: 0..255.
-type am1()     :: {imm8(),imm4()}
		 | temp()
		 | {temp(), rrx}
		 | {temp(), shiftop(), imm5()}
		 | {temp(), shiftop(), temp()}.
-type am2()     :: #am2{}.
-type am3()     :: #am3{}.
-type arg()     :: temp() | integer().
-type funv()    :: #arm_mfa{} | #arm_prim{} | temp().
-type label()   :: non_neg_integer().
-type insn()    :: tuple(). % for now

-type subst_fun() :: fun((temp()) -> temp()).

%% @doc Maps over the temporaries in an instruction
-spec insn_temps(subst_fun(), insn()) -> insn().
insn_temps(T, I) ->
  AM1 = fun(O) -> am1_temps(T, O) end,
  AM2 = fun(O) -> am2_temps(T, O) end,
  AM3 = fun(O) -> am3_temps(T, O) end,
  Arg = fun(O) -> arg_temps(T, O) end,
  case I of
      #alu  {dst=D,src=L,am1=R} -> I#alu{dst=T(D),src=T(L),am1=AM1(R)};
      #cmp        {src=L,am1=R} -> I#cmp         {src=T(L),am1=AM1(R)};
      #load       {dst=D,am2=S} -> I#load        {dst=T(D),am2=AM2(S)};
      #ldrsb      {dst=D,am3=S} -> I#ldrsb       {dst=T(D),am3=AM3(S)};
      #move       {dst=D,am1=S} -> I#move        {dst=T(D),am1=AM1(S)};
      #pseudo_move{dst=D,src=S} -> I#pseudo_move {dst=T(D),src=T(S)};
      #store      {src=S,am2=D} -> I#store       {src=T(S),am2=AM2(D)};
      #b_label{} -> I;
      #comment{} -> I;
      #label{} -> I;
      #pseudo_bc{} -> I;
      #pseudo_blr{} -> I;
      #pseudo_call{funv=F} -> I#pseudo_call{funv=funv_temps(T, F)};
      #pseudo_call_prepare{} -> I;
      #pseudo_li{dst=D} -> I#pseudo_li{dst=T(D)};
      #pseudo_spill_move{dst=D,temp=U,src=S} ->
	  I#pseudo_spill_move{dst=T(D),temp=T(U),src=T(S)};
      #pseudo_switch{jtab=J=#arm_temp{},index=Ix=#arm_temp{}} ->
	  I#pseudo_switch{jtab=T(J),index=T(Ix)};
      #pseudo_tailcall{funv=F,stkargs=Stk} ->
	  I#pseudo_tailcall{funv=funv_temps(T,F),stkargs=lists:map(Arg,Stk)};
      #pseudo_tailcall_prepare{} -> I;
      #smull{dstlo=DL,dsthi=DH,src1=L,src2=R} ->
	  I#smull{dstlo=T(DL),dsthi=T(DH),src1=T(L),src2=T(R)}
  end.

-spec am1_temps(subst_fun(), am1()) -> am1().
am1_temps(_SubstTemp, T={C,R}) when is_integer(C), is_integer(R) -> T;
am1_temps(SubstTemp, T=#arm_temp{}) -> SubstTemp(T);
am1_temps(SubstTemp, {T=#arm_temp{},rrx}) -> {SubstTemp(T),rrx};
am1_temps(SubstTemp, {A=#arm_temp{},Op,B=#arm_temp{}}) when is_atom(Op) ->
    {SubstTemp(A),Op,SubstTemp(B)};
am1_temps(SubstTemp, {T=#arm_temp{},Op,I}) when is_atom(Op), is_integer(I) ->
    {SubstTemp(T),Op,I}.

-spec am2_temps(subst_fun(), am2()) -> am2().
am2_temps(SubstTemp, T=#am2{src=A=#arm_temp{},offset=O0}) ->
    O = case O0 of
	    _ when is_integer(O0) -> O0;
	    #arm_temp{} -> SubstTemp(O0);
	    {B=#arm_temp{},rrx} -> {SubstTemp(B),rrx};
	    {B=#arm_temp{},Op,I} when is_atom(Op), is_integer(I) ->
		{SubstTemp(B),Op,I}
	end,
    T#am2{src=SubstTemp(A),offset=O}.

-spec am3_temps(subst_fun(), am3()) -> am3().
am3_temps(SubstTemp, T=#am3{src=A=#arm_temp{},offset=O0}) ->
    O = case O0 of
	    _ when is_integer(O0) -> O0;
	    #arm_temp{} -> SubstTemp(O0)
	end,
    T#am3{src=SubstTemp(A),offset=O}.

-spec funv_temps(subst_fun(), funv()) -> funv().
funv_temps(_SubstTemp, M=#arm_mfa{}) -> M;
funv_temps(_SubstTemp, P=#arm_prim{}) -> P;
funv_temps(SubstTemp,  T=#arm_temp{}) -> SubstTemp(T).

-spec arg_temps(subst_fun(), arg()) -> arg().
arg_temps(_SubstTemp, Imm) when is_integer(Imm) -> Imm;
arg_temps(SubstTemp,  T=#arm_temp{}) -> SubstTemp(T).

-type lbl_subst_fun() :: fun((label()) -> label()).

%% @doc Maps over the branch targets in an instruction
-spec insn_lbls(lbl_subst_fun(), insn()) -> insn().
insn_lbls(SubstLbl, I) ->
  case I of
    #b_label{label=Label} ->
      I#b_label{label=SubstLbl(Label)};
    #pseudo_bc{true_label=T, false_label=F} ->
      I#pseudo_bc{true_label=SubstLbl(T), false_label=SubstLbl(F)};
    #pseudo_call{sdesc=Sdesc, contlab=Contlab} ->
      I#pseudo_call{sdesc=sdesc_lbls(SubstLbl, Sdesc),
		    contlab=SubstLbl(Contlab)}
  end.

sdesc_lbls(_SubstLbl, Sdesc=#arm_sdesc{exnlab=[]}) -> Sdesc;
sdesc_lbls(SubstLbl, Sdesc=#arm_sdesc{exnlab=Exnlab}) ->
  Sdesc#arm_sdesc{exnlab=SubstLbl(Exnlab)}.
