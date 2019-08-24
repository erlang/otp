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

-module(hipe_ppc_defuse).
-export([insn_def_all/1, insn_use_all/1]).
-export([insn_def_gpr/1, insn_use_gpr/1]).
-export([insn_defs_all_gpr/1, insn_defs_all_fpr/1]).
-export([insn_def_fpr/1, insn_use_fpr/1]).
-include("hipe_ppc.hrl").

%%%
%%% Defs and uses for both general-purpose and floating-point registers.
%%% This is needed for the frame module, alas.
%%%
insn_def_all(I) ->
  addtemps(insn_def_fpr(I), insn_def_gpr(I)).

insn_use_all(I) ->
  addtemps(insn_use_fpr(I), insn_use_gpr(I)).

%%%
%%% Defs and uses for general-purpose (integer) registers only.
%%%
insn_def_gpr(I) ->
  case I of
    #alu{dst=Dst} -> [Dst];
    #load{dst=Dst} -> [Dst];
    #loadx{dst=Dst} -> [Dst];
    #mfspr{dst=Dst} -> [Dst];
    #pseudo_call{} -> call_clobbered_gpr();
    #pseudo_li{dst=Dst} -> [Dst];
    #pseudo_move{dst=Dst} -> [Dst];
    #pseudo_spill_move{dst=Dst,temp=Temp} -> [Dst, Temp];
    #pseudo_tailcall_prepare{} -> tailcall_clobbered_gpr();
    #unary{dst=Dst} -> [Dst];
    _ -> []
  end.

insn_defs_all_gpr(#pseudo_call{}) -> true;
insn_defs_all_gpr(_) -> false.

call_clobbered_gpr() ->
  [hipe_ppc:mk_temp(R, T)
   || {R,T} <- hipe_ppc_registers:call_clobbered() ++ all_fp_pseudos()].

all_fp_pseudos() -> [].	% XXX: for now

tailcall_clobbered_gpr() ->
  [hipe_ppc:mk_temp(R, T)
   || {R,T} <- hipe_ppc_registers:tailcall_clobbered() ++ all_fp_pseudos()].

insn_use_gpr(I) ->
  case I of
    #alu{src1=Src1,src2=Src2} -> addsrc(Src2, [Src1]);
    #blr{} ->
      [hipe_ppc:mk_temp(hipe_ppc_registers:return_value(), 'tagged')];
    #cmp{src1=Src1,src2=Src2} -> addsrc(Src2, [Src1]);
    #load{base=Base} -> [Base];
    #loadx{base1=Base1,base2=Base2} -> addtemp(Base1, [Base2]);
    #mtcr{src=Src} -> [Src];
    #mtspr{src=Src} -> [Src];
    #pseudo_call{sdesc=#ppc_sdesc{arity=Arity}} -> arity_use_gpr(Arity);
    #pseudo_move{src=Src} -> [Src];
    #pseudo_spill_move{src=Src} -> [Src];
    #pseudo_tailcall{arity=Arity,stkargs=StkArgs} ->
      addsrcs(StkArgs, addtemps(tailcall_clobbered_gpr(), arity_use_gpr(Arity)));
    #store{src=Src,base=Base} -> addtemp(Src, [Base]);
    #storex{src=Src,base1=Base1,base2=Base2} ->
      addtemp(Src, addtemp(Base1, [Base2]));
    #unary{src=Src} -> [Src];
    #lfd{base=Base} -> [Base];
    #lfdx{base1=Base1,base2=Base2} -> addtemp(Base1, [Base2]);
    #stfd{base=Base} -> [Base];
    #stfdx{base1=Base1,base2=Base2} -> addtemp(Base1, [Base2]);
    _ -> []
  end.

arity_use_gpr(Arity) ->
  [hipe_ppc:mk_temp(R, 'tagged')
   || R <- hipe_ppc_registers:args(Arity)].

addsrcs([Arg|Args], Set) ->
  addsrcs(Args, addsrc(Arg, Set));
addsrcs([], Set) ->
  Set.

addsrc(Src, Set) ->
  case Src of
    #ppc_temp{} -> addtemp(Src, Set);
    _ -> Set
  end.

%%%
%%% Defs and uses for floating-point registers only.
%%%
insn_def_fpr(I) ->
  case I of
    #pseudo_call{} -> call_clobbered_fpr();
    #lfd{dst=Dst} -> [Dst];
    #lfdx{dst=Dst} -> [Dst];
    #fp_binary{dst=Dst} -> [Dst];
    #fp_unary{dst=Dst} -> [Dst];
    #pseudo_fmove{dst=Dst} -> [Dst];
    #pseudo_spill_fmove{dst=Dst,temp=Temp} -> [Dst, Temp];
    _ -> []
  end.

insn_defs_all_fpr(#pseudo_call{}) -> true;
insn_defs_all_fpr(_) -> false.

call_clobbered_fpr() ->
  [hipe_ppc:mk_temp(R, 'double') || R <- hipe_ppc_registers:allocatable_fpr()].

insn_use_fpr(I) ->
  case I of
    #stfd{src=Src} -> [Src];
    #stfdx{src=Src} -> [Src];
    #fp_binary{src1=Src1,src2=Src2} -> addtemp(Src1, [Src2]);
    #fp_unary{src=Src} -> [Src];
    #pseudo_fmove{src=Src} -> [Src];
    #pseudo_spill_fmove{src=Src} -> [Src];
    _ -> []
  end.

%%%
%%% Auxiliary operations on sets of temps
%%% These sets are small. No point using gb_trees, right?
%%%

addtemps([Arg|Args], Set) ->
  addtemps(Args, addtemp(Arg, Set));
addtemps([], Set) ->
  Set.

addtemp(Temp, Set) ->
  case lists:member(Temp, Set) of
    false -> [Temp|Set];
    _ -> Set
  end.
