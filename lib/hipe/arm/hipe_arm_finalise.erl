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

-module(hipe_arm_finalise).
-export([finalise/2]).
-include("hipe_arm.hrl").

finalise(Defun, Options) ->
  #defun{code=Code0} = Defun,
  Code1Rev = expand(Code0),
  Code2 = case proplists:get_bool(peephole, Options) of
	    true -> peep(Code1Rev);
	    false -> lists:reverse(Code1Rev)
	  end,
  Defun#defun{code=Code2}.

expand(Insns) ->
  expand_list(Insns, []).

expand_list([I|Insns], Accum) ->
  expand_list(Insns, expand_insn(I, Accum));
expand_list([], Accum) ->
  Accum.

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

%% We do peephole "bottom-up" (in reverse, but applying rules to the correctly
%% ordered list). This way, we can do replacements that would take multiple
%% passes with an in-order peephole optimiser.
%%
%% N.B., if a rule wants to produce multiple instructions (even if some of them
%% are unchanged, it should push the additional instructions on the More list,
%% so that only the top instruction on Insns is new or changed, i.e. tl(Insns)
%% should have been peepholed previously.
peep(RevInsns) ->
  peep_list_skip([], RevInsns).

peep_list([#b_label{'cond'='al',label=Label}
	   | (Insns = [#label{label=Label}|_])], More) ->
  peep_list_skip(Insns, More);

peep_list([#move{movop='mov',s=false,dst=#arm_temp{reg=Dst}
		,am1=#arm_temp{reg=Dst}}|Insns], More) ->
  peep_list_skip(Insns, More);

peep_list([#move{movop='mov',s=false,dst=Dst,am1={Src,lsr,Imm}},
	   #move{movop='mov',s=false,dst=Dst,am1={Dst,lsl,Imm}}
	   |Insns], More) when Imm > 0, Imm =< 8 ->
  peep_list([#alu{aluop='bic',s=false,dst=Dst,src=Src,am1={(1 bsl Imm)-1,0}}
	    |Insns], More);
peep_list([#move{movop='mov',s=false,dst=Dst,am1={Src,lsl,Imm}},
	   #move{movop='mov',s=false,dst=Dst,am1={Dst,lsr,Imm}}
	   |Insns], More) when Imm >= 24, Imm < 32 ->
  peep_list([#alu{aluop='and',s=false,dst=Dst,src=Src
		 ,am1={(1 bsl (32-Imm))-1,0}} | Insns], More);

%% XXX: Load-after-store optimisation should also be applied to RTL, where it
%% can be more general, expose opportunities for constant propagation, etc.
peep_list([#store{stop='strb',src=Src,am2=Mem}=Str,
	   #load {ldop='ldrb',dst=Dst,am2=Mem} | Insns], More) ->
  peep_list([#alu{aluop='and',s=false,dst=Dst,src=Src,am1={16#ff,0}}|Insns],
	    [Str|More]);
peep_list([#store{stop='str',src=Src,am2=Mem}=Str,
	   #load {ldop='ldr',dst=Dst,am2=Mem} | Insns], More) ->
  peep_list([#move{movop='mov',s=false,dst=Dst,am1=Src}|Insns], [Str|More]);

peep_list([#alu{aluop='and',s=false,dst=Dst,src=Src,am1={Mask,0}},
	   #alu{aluop='bic',s=false,dst=Dst,src=Dst,am1={InvMask,0}}
	   |Insns], More) ->
  peep_list([#alu{aluop='and',s=false,dst=Dst,src=Src
		 ,am1={Mask band (bnot InvMask),0}} | Insns], More);

%% XXX: The place that generates brain-dead code like the following should be
%% fixed rather than trying to patch it over here.
peep_list([#load{ldop='ldrb',dst=Dst,am2=_Mem},
	   #alu{aluop='bic',s=false,dst=Dst,src=Dst,am1={16#ff,0}}
	   | Insns], More) ->
  peep_list([#move{movop='mov',s=false,dst=Dst,am1={0,0}}|Insns], More);

peep_list(Insns, [I|More]) ->
  peep_list([I|Insns], More);
peep_list(Accum, []) ->
  Accum.

%% Used as an optimisation instead of tailcalling peep_list/2 when Insns has
%% already been peeped or is otherwise uninteresting (such as empty).
peep_list_skip(Insns, [I|More]) ->
  peep_list([I|Insns], More);
peep_list_skip(Accum, []) ->
  Accum.
