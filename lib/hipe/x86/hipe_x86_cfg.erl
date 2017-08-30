%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(hipe_x86_cfg).

-export([init/1,
         labels/1, start_label/1,
         succ/2, pred/2,
         bb/2, bb_add/3]).
-export([postorder/1, reverse_postorder/1]).
-export([linearise/1, params/1, arity/1, redirect_jmp/3]).

%%% these tell cfg.inc what to define (ugly as hell)
-define(PRED_NEEDED,true).
-define(BREADTH_ORDER,true).
-define(PARAMS_NEEDED,true).
-define(START_LABEL_UPDATE_NEEDED,true).

-include("hipe_x86.hrl").
-include("../flow/cfg.hrl").
-include("../flow/cfg.inc").

init(Defun) ->
    %% XXX: this assumes that the code starts with a label insn.
    %% Is that guaranteed?
    Code = hipe_x86:defun_code(Defun),
    StartLab = hipe_x86:label_label(hd(Code)),
    Data = hipe_x86:defun_data(Defun),
    IsClosure = hipe_x86:defun_is_closure(Defun),
    MFA = hipe_x86:defun_mfa(Defun),
    IsLeaf = hipe_x86:defun_is_leaf(Defun),
    Formals = hipe_x86:defun_formals(Defun),
    CFG0 = mk_empty_cfg(MFA, StartLab, Data, IsClosure, IsLeaf, Formals),
    take_bbs(Code, CFG0).

is_branch(I) ->
    case I of
	#jmp_fun{} -> true;
	#jmp_label{} -> true;
	#jmp_switch{} -> true;
	#pseudo_call{} -> true;
	#pseudo_jcc{} -> true;
	#pseudo_tailcall{} -> true;
	#ret{} -> true;
	_ -> false
    end.

branch_successors(Branch) ->
    case Branch of
	#jmp_fun{} -> [];
	#jmp_label{label=Label} -> [Label];
	#jmp_switch{labels=Labels} -> Labels;
	#pseudo_call{contlab=ContLab, sdesc=#x86_sdesc{exnlab=ExnLab}} ->
	    case ExnLab of
		[] -> [ContLab];
		_ -> [ContLab,ExnLab]
	    end;
	#pseudo_jcc{true_label=TrueLab,false_label=FalseLab} -> [FalseLab,TrueLab];
	#pseudo_tailcall{} -> [];
	#ret{} -> []
    end.

-ifdef(REMOVE_TRIVIAL_BBS_NEEDED).
fails_to(_Instr) -> [].
-endif.

redirect_jmp(I, Old, New) ->
    case I of
	#jmp_label{label=Label} ->
	    if Old =:= Label -> I#jmp_label{label=New};
	       true -> I
	    end;
	#pseudo_jcc{true_label=TrueLab, false_label=FalseLab} ->
	    J0 = if Old =:= TrueLab -> I#pseudo_jcc{true_label=New};
		    true -> I
		 end,
	    if Old =:= FalseLab -> J0#pseudo_jcc{false_label=New};
	       true -> J0
	    end;
	%% handle pseudo_call too?
	_ -> I
    end.

%%% XXX: fix if labels can occur in operands
%% redirect_ops(_Labels, CFG, _Map) -> 
%%   CFG.

mk_goto(Label) ->
  hipe_x86:mk_jmp_label(Label).

is_label(I) ->
  hipe_x86:is_label(I).

label_name(Label) ->
  hipe_x86:label_label(Label).

mk_label(Name) ->
  hipe_x86:mk_label(Name).

%% is_comment(I) ->
%%   hipe_x86:is_comment(I).
%% 
%% is_goto(I) ->
%%   hipe_x86:is_jmp_label(I).

linearise(CFG) ->	% -> defun, not insn list
  MFA = function(CFG),
  Formals = params(CFG),
  Code = linearize_cfg(CFG),
  Data = data(CFG),
  VarRange = hipe_gensym:var_range(x86),
  LabelRange = hipe_gensym:label_range(x86),
  IsClosure = is_closure(CFG),
  IsLeaf = is_leaf(CFG),
  hipe_x86:mk_defun(MFA, Formals, IsClosure, IsLeaf,
		    Code, Data, VarRange, LabelRange).

arity(CFG) ->
  {_M,_F,A} = function(CFG),
  A.

%% init_gensym(CFG) ->
%%   HighestVar = find_highest_var(CFG),
%%   HighestLabel = find_highest_label(CFG),
%%   hipe_gensym:init(),
%%   hipe_gensym:set_var(x86, HighestVar),
%%   hipe_gensym:set_label(x86, HighestLabel).
%% 
%% highest_var(Code) ->
%%   hipe_x86:highest_temp(Code).
