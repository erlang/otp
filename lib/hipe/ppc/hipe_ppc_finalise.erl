%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(hipe_ppc_finalise).
-export([finalise/1]).
-include("hipe_ppc.hrl").

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
    #pseudo_bc{bcond=BCond,true_label=TrueLab,false_label=FalseLab,pred=Pred} ->
      [hipe_ppc:mk_b_label(FalseLab),
       hipe_ppc:mk_bc(BCond, TrueLab, Pred) |
       Accum];
    #pseudo_call{func=FunC,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
      [hipe_ppc:mk_b_label(ContLab),
       case FunC of
	 'ctr' -> hipe_ppc:mk_bctrl(SDesc);
	 Fun -> hipe_ppc:mk_bl(Fun, SDesc, Linkage)
       end |
       Accum];
    #pseudo_tailcall_prepare{} ->
      Accum;
    _ ->
      [I|Accum]
  end.

peep(Insns) ->
  peep_list(Insns, []).

peep_list([#b_label{label=Label} | (Insns = [#label{label=Label}|_])], Accum) ->
  peep_list(Insns, Accum);
peep_list([I|Insns], Accum) ->
  peep_list(Insns, [I|Accum]);
peep_list([], Accum) ->
  lists:reverse(Accum).
