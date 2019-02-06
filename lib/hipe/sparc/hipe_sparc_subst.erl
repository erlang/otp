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

-module(hipe_sparc_subst).
-export([insn_temps/2]).
-include("hipe_sparc.hrl").

%% These should be moved to hipe_sparc and exported
-type temp()    :: #sparc_temp{}.
-type src2()    :: temp() | #sparc_simm13{}.
-type src2b()   :: src2() | #sparc_uimm5{}.
-type funv()    :: #sparc_mfa{} | #sparc_prim{} | temp().
-type arg()     :: temp() | integer().
-type insn()    :: tuple(). % for now

-type subst_fun() :: fun((temp()) -> temp()).

%% @doc Maps over the temporaries in an instruction
-spec insn_temps(subst_fun(), insn()) -> insn().
insn_temps(T, I) ->
  S2 = fun(O) -> src2_temps(T, O) end,
  S2B = fun(O) -> src2b_temps(T, O) end,
  Arg = fun(O) -> arg_temps(T, O) end,
  case I of
      #alu{src1=L,src2=R,dst=D} -> I#alu{src1=T(L),src2=S2B(R),dst=T(D)};
      #bp{} -> I;
      #comment{} -> I;
      #jmp{src1=L,src2=R} -> I#jmp{src1=T(L),src2=S2(R)};
      #label{} -> I;
      #pseudo_bp{} -> I;
      #pseudo_call{funv=F} -> I#pseudo_call{funv=funv_temps(T,F)};
      #pseudo_call_prepare{} -> I;
      #pseudo_move{src=S,dst=D} -> I#pseudo_move{src=T(S),dst=T(D)};
      #pseudo_ret{} -> I;
      #pseudo_set{dst=D}-> I#pseudo_set{dst=T(D)};
      #pseudo_spill_move{src=S,temp=U,dst=D} ->
	  I#pseudo_spill_move{src=T(S),temp=T(U),dst=T(D)};
      #pseudo_tailcall{funv=F,stkargs=Stk} ->
	  I#pseudo_tailcall{funv=funv_temps(T,F),stkargs=lists:map(Arg,Stk)};
      #pseudo_tailcall_prepare{} -> I;
      #rdy{dst=D} -> I#rdy{dst=T(D)};
      #sethi{dst=D} -> I#sethi{dst=T(D)};
      #store{src=S,base=B,disp=D} -> I#store{src=T(S),base=T(B),disp=S2(D)};
      #fp_binary{src1=L,src2=R,dst=D} ->
	  I#fp_binary{src1=T(L),src2=T(R),dst=T(D)};
      #fp_unary{src=S,dst=D} -> I#fp_unary{src=T(S),dst=T(D)};
      #pseudo_fload{base=B,disp=Di,dst=Ds} ->
	  I#pseudo_fload{base=T(B),disp=S2(Di),dst=T(Ds)};
      #pseudo_fmove{src=S,dst=D} -> I#pseudo_fmove{src=T(S),dst=T(D)};
      #pseudo_fstore{src=S,base=B,disp=D} ->
	  I#pseudo_fstore{src=T(S),base=T(B),disp=S2(D)};
      #pseudo_spill_fmove{src=S,temp=U,dst=D} ->
	  I#pseudo_spill_fmove{src=T(S),temp=T(U),dst=T(D)}
  end.

-spec src2_temps(subst_fun(), src2()) -> src2().
src2_temps(_SubstTemp, I=#sparc_simm13{}) -> I;
src2_temps(SubstTemp,  T=#sparc_temp{}) -> SubstTemp(T).

-spec src2b_temps(subst_fun(), src2b()) -> src2b().
src2b_temps(_SubstTemp, I=#sparc_uimm5{}) -> I;
src2b_temps(SubstTemp, Op) -> src2_temps(SubstTemp, Op).

-spec funv_temps(subst_fun(), funv()) -> funv().
funv_temps(_SubstTemp, M=#sparc_mfa{}) -> M;
funv_temps(_SubstTemp, P=#sparc_prim{}) -> P;
funv_temps(SubstTemp,  T=#sparc_temp{}) -> SubstTemp(T).

-spec arg_temps(subst_fun(), arg()) -> arg().
arg_temps(_SubstTemp, Imm) when is_integer(Imm) -> Imm;
arg_temps(SubstTemp,  T=#sparc_temp{}) -> SubstTemp(T).
