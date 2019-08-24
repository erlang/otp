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

-module(hipe_sparc_finalise).
-export([finalise/1]).
-include("hipe_sparc.hrl").

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
    #bp{'cond'='a'} ->
      [hipe_sparc:mk_nop(),
       I |
       Accum];
    #call_rec{} ->
      [hipe_sparc:mk_nop(),
       I |
       Accum];
    #call_tail{} ->
      RA = hipe_sparc:mk_ra(),
      TempRA = hipe_sparc:mk_temp1(),
      [hipe_sparc:mk_mov(TempRA, RA),
       I,	% becomes a call, which clobbers RA
       hipe_sparc:mk_mov(RA, TempRA) |
       Accum];
    #jmp{} ->
      [hipe_sparc:mk_nop(),
       I |
       Accum];
    #pseudo_bp{'cond'=Cond,true_label=TrueLab,false_label=FalseLab, pred=Pred} ->
      [hipe_sparc:mk_nop(),
       hipe_sparc:mk_b_label(FalseLab),
       hipe_sparc:mk_nop(),
       hipe_sparc:mk_bp(Cond, TrueLab, Pred) |
       Accum];
    %% #pseudo_br{} -> expand_pseudo_br(I, Accum);
    #pseudo_call{funv=FunV,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
      [hipe_sparc:mk_nop(),
       hipe_sparc:mk_b_label(ContLab),
       hipe_sparc:mk_nop(),
       case FunV of
	 #sparc_temp{} ->
	   hipe_sparc:mk_jmpl(FunV, SDesc);
	 _ ->
	   hipe_sparc:mk_call_rec(FunV, SDesc, Linkage)
       end |
       Accum];
    #pseudo_ret{} ->
      RA = hipe_sparc:mk_ra(),
      [hipe_sparc:mk_nop(),
       hipe_sparc:mk_jmp(RA, hipe_sparc:mk_simm13(8), []) |
       Accum];
    #pseudo_tailcall_prepare{} ->
      Accum;
    _ ->
      XXX =
	case I of
	  #alu{} -> true;
	  #comment{} -> true;
	  #label{} -> true;
	  #pseudo_set{} -> true;
	  #rdy{} -> true;
	  #sethi{} -> true;
	  #store{} -> true;
	  #bp{} -> false;
	  %% #br{} -> false;
	  #call_rec{} -> false;
	  #call_tail{} -> false;
	  #jmp{} -> false;
	  #jmpl{} -> false;
	  #pseudo_bp{} -> false;
	  %% #pseudo_br{} -> false;
	  #pseudo_call{} -> false;
	  #pseudo_call_prepare{} -> false;
	  #pseudo_move{} -> false;
	  #pseudo_ret{} -> false;
	  #pseudo_tailcall{} -> false;
	  #pseudo_tailcall_prepare{} -> false;
	  #fp_binary{} -> true;
	  #fp_unary{} -> true;
	  #pseudo_fload{} -> true;
	  #pseudo_fstore{} -> true
	end,
      case XXX of
	true -> [];
	false -> exit({?MODULE,expand_insn,I})
      end,
      [I|Accum]
  end.

-ifdef(notdef).	% XXX: only for sparc64, alas
expand_pseudo_br(I, Accum) ->
  #pseudo_br{rcond=RCond,src=Src,true_label=TrueLab,false_label=FalseLab, pred=Pred} = I,
  [hipe_sparc:mk_nop(),
   hipe_sparc:mk_b_label(FalseLab),
   hipe_sparc:mk_nop(),
   hipe_sparc:mk_br(RCond, Src, TrueLab, Pred) |
   Accum].
-endif.

peep(Insns) ->
  peep_list(Insns, []).

peep_list([#bp{'cond'='a',label=Label}, #sethi{uimm22=#sparc_uimm22{value=0},dst=#sparc_temp{reg=0}} | (Insns = [#label{label=Label}|_])], Accum) ->
  peep_list(Insns, Accum);
peep_list([I|Insns], Accum) ->
  peep_list(Insns, [I|Accum]);
peep_list([], Accum) ->
  lists:reverse(Accum).
