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

-module(hipe_icode_cfg).

-export([bb/2, bb_add/3,
	 cfg_to_linear/1,
	 is_closure/1,
	 closure_arity/1,
         linear_to_cfg/1,
         labels/1, start_label/1,
	 pp/1, pp/2,
	 params/1, params_update/2,
         pred/2,
         redirect/4,
	 remove_trivial_bbs/1, remove_unreachable_code/1,
         succ/2,
	 visit/2, is_visited/2, none_visited/0
	]).
-export([postorder/1, reverse_postorder/1]).

-define(ICODE_CFG, true).	% needed by cfg.inc
%%-define(DO_ASSERT, true).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("../flow/hipe_bb.hrl").
-include("../flow/cfg.hrl").
-include("../flow/cfg.inc").

%%----------------------------------------------------------------------
%% Prototypes for exported functions which are Icode specific
%%----------------------------------------------------------------------

-spec labels(cfg()) -> [icode_lbl()].
-spec postorder(cfg()) -> [icode_lbl()].
-spec reverse_postorder(cfg()) -> [icode_lbl()].

-spec params(cfg()) -> hipe_icode:params().
-spec params_update(cfg(), hipe_icode:params()) -> cfg().

-spec is_visited(icode_lbl(), gb_sets:set()) -> boolean().
-spec visit(icode_lbl(), gb_sets:set()) -> gb_sets:set().

-spec bb(cfg(), icode_lbl()) -> 'not_found' | bb().
-spec bb_add(cfg(), icode_lbl(), bb()) -> cfg().
-spec pred(cfg(), icode_lbl()) -> [icode_lbl()].
-spec succ(cfg(), icode_lbl()) -> [icode_lbl()].
-spec redirect(cfg(), icode_lbl(), icode_lbl(), icode_lbl()) -> cfg().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to Icode
%%

-spec linear_to_cfg(#icode{}) -> cfg().

linear_to_cfg(LinearIcode) ->
  %% hipe_icode_pp:pp(Icode),
  Code = hipe_icode:icode_code(LinearIcode),
  IsClosure = hipe_icode:icode_is_closure(LinearIcode),
  StartLabel = hipe_icode:label_name(hd(Code)),
  CFG0 = mk_empty_cfg(hipe_icode:icode_fun(LinearIcode),
		      StartLabel,
		      hipe_icode:icode_data(LinearIcode),
		      IsClosure,
		      hipe_icode:icode_is_leaf(LinearIcode),
		      hipe_icode:icode_params(LinearIcode)),
  CFG1 = info_update(CFG0, hipe_icode:icode_info(LinearIcode)),
  CFG2 = case IsClosure of
	   true ->
	     closure_arity_update(CFG1,
				  hipe_icode:icode_closure_arity(LinearIcode));
	   false ->
	     CFG1
	 end,
  ?opt_start_timer("Get BBs icode"),
  FullCFG = take_bbs(Code, CFG2),
  ?opt_stop_timer("Get BBs icode"),
  FullCFG.

%% remove_blocks(CFG, []) ->
%%   CFG;
%% remove_blocks(CFG, [Lbl|Lbls]) ->
%%   remove_blocks(bb_remove(CFG, Lbl), Lbls).

-spec is_label(icode_instr()) -> boolean().
is_label(Instr) ->
  hipe_icode:is_label(Instr).

label_name(Instr) ->
  hipe_icode:label_name(Instr).

mk_label(Name) ->
  hipe_icode:mk_label(Name).

mk_goto(Name) ->
  hipe_icode:mk_goto(Name).

branch_successors(Instr) ->
  hipe_icode:successors(Instr).

fails_to(Instr) ->
  hipe_icode:fails_to(Instr).

%% True if instr has no effect.
-spec is_comment(icode_instr()) -> boolean().
is_comment(Instr) ->
  hipe_icode:is_comment(Instr).

%% True if instr is just a jump (no side-effects).
-spec is_goto(icode_instr()) -> boolean().
is_goto(Instr) ->
  hipe_icode:is_goto(Instr).

-spec is_branch(icode_instr()) -> boolean().
is_branch(Instr) ->
  hipe_icode:is_branch(Instr).

-spec is_pure_branch(icode_instr()) -> boolean().
is_pure_branch(Instr) ->
  case Instr of
    #icode_if{} -> true;
    #icode_goto{} -> true;
    #icode_switch_val{} -> true;
    #icode_switch_tuple_arity{} -> true;
    #icode_type{} -> true;
    %% false cases below -- XXX: are they correct?
    #icode_label{} -> false;
    #icode_move{} -> false;
    #icode_phi{} -> false;
    #icode_call{} -> false;
    #icode_enter{} -> false;
    #icode_return{} -> false;
    #icode_begin_try{} -> false;
    #icode_end_try{} -> false;
    #icode_begin_handler{} -> false;
    #icode_fail{} -> false;
    #icode_comment{} -> false
  end.

-spec is_phi(icode_instr()) -> boolean().
is_phi(I) ->
  hipe_icode:is_phi(I).

phi_remove_pred(I, Pred) ->
  hipe_icode:phi_remove_pred(I, Pred).

%% phi_redirect_pred(I, OldPred, NewPred) ->
%%   hipe_icode:phi_redirect_pred(I, OldPred, NewPred).

redirect_jmp(Jmp, ToOld, ToNew) ->
  hipe_icode:redirect_jmp(Jmp, ToOld, ToNew).

redirect_ops(_, CFG, _) -> %% We do not refer to labels in Icode ops.
  CFG.

%%----------------------------------------------------------------------------

-spec pp(cfg()) -> 'ok'.

pp(CFG) ->
  hipe_icode_pp:pp(cfg_to_linear(CFG)).

-spec pp(io:device(), cfg()) -> 'ok'.

pp(Dev, CFG) ->
  hipe_icode_pp:pp(Dev, cfg_to_linear(CFG)).

%%----------------------------------------------------------------------------

-spec cfg_to_linear(cfg()) -> #icode{}.
cfg_to_linear(CFG) ->
  Code = linearize_cfg(CFG),
  IsClosure = is_closure(CFG),
  Icode = hipe_icode:mk_icode(function(CFG),
			      params(CFG),
			      IsClosure,
			      is_leaf(CFG),
			      Code,
			      data(CFG),
			      hipe_gensym:var_range(icode), 
			      hipe_gensym:label_range(icode)),
  Icode1 = hipe_icode:icode_info_update(Icode, info(CFG)),
  case IsClosure of
    true -> hipe_icode:icode_closure_arity_update(Icode1, closure_arity(CFG));
    false -> Icode1
  end.
