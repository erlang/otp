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

-module(hipe_sparc_defuse).
-export([insn_def_all/1, insn_use_all/1]).
-export([insn_def_gpr/1, insn_use_gpr/1]).
-export([insn_def_fpr/1, insn_use_fpr/1]).
-export([insn_defs_all_gpr/1, insn_defs_all_fpr/1]).
-include("hipe_sparc.hrl").

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
    %% #jmpl{} -> [hipe_sparc:mk_ra()]; % XXX: can jmpl occur this early?
    #pseudo_call{} -> call_clobbered_gpr();
    #pseudo_move{dst=Dst} -> [Dst];
    #pseudo_set{dst=Dst} -> [Dst];
    #pseudo_spill_move{temp=Temp, dst=Dst} -> [Temp, Dst];
    #pseudo_tailcall_prepare{} -> tailcall_clobbered_gpr();
    #rdy{dst=Dst} -> [Dst];
    #sethi{dst=Dst} -> [Dst];
    _ -> []
  end.

insn_defs_all_gpr(I) ->
  case I of
    #pseudo_call{} -> true;
    _ -> false
  end.

call_clobbered_gpr() ->
  [hipe_sparc:mk_temp(R, T)
   || {R,T} <- hipe_sparc_registers:call_clobbered() ++ all_fp_pseudos()].

all_fp_pseudos() -> [].	% XXX: for now

tailcall_clobbered_gpr() ->
  [hipe_sparc:mk_temp(R, T)
   || {R,T} <- hipe_sparc_registers:tailcall_clobbered() ++ all_fp_pseudos()].

insn_use_gpr(I) ->
  case I of
    #alu{src1=Src1,src2=Src2} -> addsrc(Src2, [Src1]);
    %% #br{src=Src} -> [Src]; % XXX: can br occur this early?
    #jmp{src1=Src1,src2=Src2} -> addsrc(Src2, [Src1]);
    %% #jmpl{src=Src} -> [Src]; % XXX: can jmpl occur this early?
    %% #pseudo_br{src=Src} -> [Src];
    #pseudo_call{funv=FunV,sdesc=#sparc_sdesc{arity=Arity}} ->
      funv_use(FunV, arity_use_gpr(Arity));
    #pseudo_move{src=Src} -> [Src];
    #pseudo_ret{} -> [hipe_sparc:mk_rv()];
    #pseudo_spill_move{src=Src} -> [Src];
    #pseudo_tailcall{funv=FunV,arity=Arity,stkargs=StkArgs} ->
      addsrcs(StkArgs, addtemps(tailcall_clobbered_gpr(), funv_use(FunV, arity_use_gpr(Arity))));
    #store{src=Src,base=Base,disp=Disp} ->
      addtemp(Src, addsrc(Disp, [Base]));
    #pseudo_fload{base=Base} -> [Base];
    #pseudo_fstore{base=Base} -> [Base];
    _ -> []
  end.

arity_use_gpr(Arity) ->
  [hipe_sparc:mk_temp(R, 'tagged')
   || R <- hipe_sparc_registers:args(Arity)].

funv_use(FunV, Set) ->
  case FunV of
    #sparc_temp{} -> addtemp(FunV, Set);
    _ -> Set
  end.

addsrcs([Arg|Args], Set) ->
  addsrcs(Args, addsrc(Arg, Set));
addsrcs([], Set) ->
  Set.

addsrc(Src, Set) ->
  case Src of
    #sparc_temp{} -> addtemp(Src, Set);
    _ -> Set
  end.

%%%
%%% Defs and uses for floating-point registers only.
%%%
insn_def_fpr(I) ->
  case I of
    #pseudo_call{} -> call_clobbered_fpr();
    #fp_binary{dst=Dst} -> [Dst];
    #fp_unary{dst=Dst} -> [Dst];
    #pseudo_fload{dst=Dst} -> [Dst];
    #pseudo_fmove{dst=Dst} -> [Dst];
    #pseudo_spill_fmove{temp=Temp, dst=Dst} -> [Temp, Dst];
    _ -> []
  end.

insn_defs_all_fpr(I) ->
  case I of
    #pseudo_call{} -> true;
    _ -> false
  end.

call_clobbered_fpr() ->
  [hipe_sparc:mk_temp(R, 'double') || R <- hipe_sparc_registers:allocatable_fpr()].

insn_use_fpr(I) ->
  case I of
    #fp_binary{src1=Src1,src2=Src2} -> addtemp(Src1, [Src2]);
    #fp_unary{src=Src} -> [Src];
    #pseudo_fmove{src=Src} -> [Src];
    #pseudo_fstore{src=Src} -> [Src];
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
