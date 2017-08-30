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

-module(hipe_ppc_subst).
-export([insn_temps/2]).
-include("hipe_ppc.hrl").

%% These should be moved to hipe_ppc and exported
-type temp()    :: #ppc_temp{}.
-type oper()    :: temp() | #ppc_simm16{} | #ppc_uimm16{}.
-type arg()     :: temp() | integer().
-type insn()    :: tuple(). % for now

-type subst_fun() :: fun((temp()) -> temp()).

%% @doc Maps over the temporaries in an instruction
-spec insn_temps(subst_fun(), insn()) -> insn().
insn_temps(T, I) ->
  A = fun(O) -> arg_temps(T, O) end,
  O = fun(O) -> oper_temps(T, O) end,
  case I of
      #alu{dst=D,src1=L,src2=R} -> I#alu{dst=T(D),src1=T(L),src2=O(R)};
      #b_label{} -> I;
      %% #bc{} -> I;
      #bctr{} -> I;
      #blr{} -> I;
      #cmp{src1=L,src2=R} -> I#cmp{src1=T(L),src2=O(R)};
      #comment{} -> I;
      #label{} -> I;
      #load{dst=D,base=B} -> I#load{dst=T(D),base=T(B)};
      #loadx{dst=D,base1=L,base2=R} -> I#loadx{dst=T(D),base1=T(L),base2=T(R)};
      #mfspr{dst=D} -> I#mfspr{dst=T(D)};
      #mtcr{src=S} -> I#mtcr{src=T(S)};
      #mtspr{src=S} -> I#mtspr{src=T(S)};
      #pseudo_bc{} -> I;
      #pseudo_call{func=F} when not is_record(F, ppc_temp) -> I;
      #pseudo_call_prepare{} -> I;
      #pseudo_li{dst=D} -> I#pseudo_li{dst=T(D)};
      #pseudo_move{dst=D,src=S} -> I#pseudo_move{dst=T(D),src=T(S)};
      #pseudo_spill_move{dst=D,temp=U,src=S} ->
	  I#pseudo_spill_move{dst=T(D),temp=T(U),src=T(S)};
      #pseudo_tailcall{func=F,stkargs=Stk} when not is_record(F, ppc_temp) ->
	  I#pseudo_tailcall{stkargs=lists:map(A,Stk)};
      #pseudo_tailcall_prepare{} -> I;
      #store{src=S,base=B} -> I#store{src=T(S),base=T(B)};
      #storex{src=S,base1=L,base2=R} ->
	  I#storex{src=T(S),base1=T(L),base2=T(R)};
      #unary{dst=D,src=S} -> I#unary{dst=T(D),src=T(S)};
      #lfd{dst=D,base=B} -> I#lfd{dst=T(D),base=T(B)};
      #lfdx{dst=D,base1=L,base2=R} -> I#lfdx{dst=T(D),base1=T(L),base2=T(R)};
      #stfd{src=S,base=B} -> I#stfd{src=T(S),base=T(B)};
      #stfdx{src=S,base1=L,base2=R} -> I#stfdx{src=T(S),base1=T(L),base2=T(R)};
      #fp_binary{dst=D,src1=L,src2=R} ->
	  I#fp_binary{dst=T(D),src1=T(L),src2=T(R)};
      #fp_unary{dst=D,src=S} -> I#fp_unary{dst=T(D),src=T(S)};
      #pseudo_fmove{dst=D,src=S} -> I#pseudo_fmove{dst=T(D),src=T(S)};
      #pseudo_spill_fmove{dst=D,temp=U,src=S} ->
	  I#pseudo_spill_fmove{dst=T(D),temp=T(U),src=T(S)}
  end.

-spec oper_temps(subst_fun(), oper()) -> oper().
oper_temps(SubstTemp,  T=#ppc_temp{}) -> SubstTemp(T);
oper_temps(_SubstTemp, I=#ppc_simm16{}) -> I;
oper_temps(_SubstTemp, I=#ppc_uimm16{}) -> I.

-spec arg_temps(subst_fun(), arg()) -> arg().
arg_temps(_SubstTemp, Imm) when is_integer(Imm) -> Imm;
arg_temps(SubstTemp,  T=#ppc_temp{}) -> SubstTemp(T).
