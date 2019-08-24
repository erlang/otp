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

-module(hipe_ppc_specific_fp).

%% for hipe_coalescing_regalloc:
-export([number_of_temporaries/2
	 ,analyze/2
	 ,labels/2
	 ,all_precoloured/1
	 ,bb/3
	 ,liveout/3
	 ,reg_nr/2
	 ,def_use/2
	 ,is_move/2
	 ,is_spill_move/2
	 ,is_precoloured/2
	 ,var_range/2
	 ,allocatable/1
	 ,non_alloc/2
	 ,physical_name/2
	 ,reverse_postorder/2
	 ,livein/3
	 ,uses/2
	 ,defines/2
	 ,defines_all_alloc/2
	]).

%% for hipe_graph_coloring_regalloc:
-export([is_fixed/2]).

%% for hipe_ls_regalloc:
%%-export([args/2, is_arg/2, is_global, new_spill_index/2]).
%%-export([breadthorder/2, postorder/2]).

%% callbacks for hipe_regalloc_loop
-export([check_and_rewrite/3]).

%% callbacks for hipe_regalloc_prepass, hipe_range_split
-export([mk_move/3,
	 mk_goto/2,
	 redirect_jmp/4,
	 new_label/1,
	 new_reg_nr/1,
	 update_reg_nr/3,
	 update_bb/4,
	 subst_temps/3]).

%% callbacks for hipe_bb_weights
-export([branch_preds/2]).

check_and_rewrite(CFG, Coloring, _) ->
  hipe_ppc_ra_postconditions_fp:check_and_rewrite(CFG, Coloring).

reverse_postorder(CFG, _) ->
  hipe_ppc_cfg:reverse_postorder(CFG).

non_alloc(_CFG, _) ->
  [].

%% Liveness stuff

analyze(CFG, _) ->
  hipe_ppc_liveness_fpr:analyse(CFG).

livein(Liveness, L, _) ->
  hipe_ppc_liveness_fpr:livein(Liveness, L).

liveout(BB_in_out_liveness, Label, _) ->
  hipe_ppc_liveness_fpr:liveout(BB_in_out_liveness, Label).

%% Registers stuff

allocatable(no_context) ->
  hipe_ppc_registers:allocatable_fpr().

all_precoloured(Ctx) ->
  allocatable(Ctx).

is_precoloured(Reg, _) ->
  hipe_ppc_registers:is_precoloured_fpr(Reg).

is_fixed(_Reg, _) ->
  false.

physical_name(Reg, _) ->
  Reg.

%% CFG stuff

labels(CFG, _) ->
  hipe_ppc_cfg:labels(CFG).

var_range(_CFG, _) ->
  hipe_gensym:var_range(ppc).

number_of_temporaries(_CFG, _) ->
  Highest_temporary = hipe_gensym:get_var(ppc),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG, L, _) ->
  hipe_ppc_cfg:bb(CFG, L).

update_bb(CFG,L,BB,_) ->
  hipe_ppc_cfg:bb_add(CFG,L,BB).

branch_preds(Instr,_) ->
  hipe_ppc_cfg:branch_preds(Instr).

%% PowerPC stuff

def_use(I, Ctx) ->
  {defines(I, Ctx), uses(I, Ctx)}.

uses(I, _) ->
  hipe_ppc_defuse:insn_use_fpr(I).

defines(I, _) ->
  hipe_ppc_defuse:insn_def_fpr(I).

defines_all_alloc(I, _) ->
  hipe_ppc_defuse:insn_defs_all_fpr(I).

is_move(I, _) ->
  hipe_ppc:is_pseudo_fmove(I).
 
is_spill_move(I, _) ->
  hipe_ppc:is_pseudo_spill_fmove(I).

reg_nr(Reg, _) ->
  hipe_ppc:temp_reg(Reg).

mk_move(Src, Dst, _) ->
  hipe_ppc:mk_pseudo_fmove(Dst, Src).

mk_goto(Label, _) ->
  hipe_ppc:mk_b_label(Label).

redirect_jmp(Jmp, ToOld, ToNew, _) when is_integer(ToOld), is_integer(ToNew) ->
  hipe_ppc_cfg:redirect_jmp(Jmp, ToOld, ToNew).

new_label(_) ->
  hipe_gensym:get_next_label(ppc).

new_reg_nr(_) ->
  hipe_gensym:get_next_var(ppc).

update_reg_nr(Nr, _Temp, _) ->
  hipe_ppc:mk_temp(Nr, 'double').

subst_temps(SubstFun, Instr, _) ->
  hipe_ppc_subst:insn_temps(
    fun(Op) ->
	case hipe_ppc:temp_is_allocatable(Op)
	  andalso hipe_ppc:temp_type(Op) =:= 'double'
	of
	  true -> SubstFun(Op);
	  false -> Op
	end
    end, Instr).

-ifdef(notdef).
new_spill_index(SpillIndex, _) ->
  SpillIndex+1.

breadthorder(CFG, _) ->
  hipe_ppc_cfg:breadthorder(CFG).

postorder(CFG, _) ->
  hipe_ppc_cfg:postorder(CFG).

is_global(_R, _) ->
  false.

is_arg(_R, _) ->
  false.

args(_CFG, _) ->
  [].
-endif.
