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

-module(hipe_arm_finalise).
-export([finalise/1]).
-include("hipe_arm.hrl").

finalise(Defun) ->
  #defun{code=Code0} = Defun,
  Code1 = peep(expand(Code0)),
  Defun#defun{code=Code1}.

expand(Insns) ->
  expand_list(Insns, []).

expand_list([I|Insns], Accum) ->
  expand_list(Insns, expand_insn(I, Accum));
expand_list([], Accum) ->
  lists:reverse(Accum).

expand_insn(I, Accum) ->
  case I of
    #pseudo_bc{'cond'=Cond,true_label=TrueLab,false_label=FalseLab} ->
      [hipe_arm:mk_b_label(FalseLab),
       hipe_arm:mk_b_label(Cond, TrueLab) |
       Accum];
    #pseudo_blr{} ->
      [hipe_arm:mk_move(hipe_arm:mk_pc(), hipe_arm:mk_lr()) | Accum];
    #pseudo_bx{src=Src} ->
      [hipe_arm:mk_move(hipe_arm:mk_pc(), Src) | Accum];
    #pseudo_call{funv=FunV,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
      [hipe_arm:mk_b_label(ContLab),
       case FunV of
	 #arm_temp{} -> hipe_arm:mk_blx(FunV, SDesc);
	 _ -> hipe_arm:mk_bl(FunV, SDesc, Linkage)
       end |
       Accum];
    #pseudo_switch{jtab=JTab,index=Index} ->
      PC = hipe_arm:mk_pc(),
      Am2 = hipe_arm:mk_am2(JTab, '+', {Index,'lsl',2}),
      [hipe_arm:mk_load('ldr', PC, Am2) | Accum];
    #pseudo_tailcall_prepare{} ->
      Accum;
    _ ->
      [I|Accum]
  end.

peep(Insns) ->
  peep_list(Insns, []).

peep_list([#b_label{'cond'='al',label=Label} | (Insns = [#label{label=Label}|_])], Accum) ->
  peep_list(Insns, Accum);
peep_list([I|Insns], Accum) ->
  peep_list(Insns, [I|Accum]);
peep_list([], Accum) ->
  lists:reverse(Accum).
