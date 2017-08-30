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

-module(hipe_sparc_specific).

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
-export([args/2, is_arg/2, is_global/2, new_spill_index/2]).
-export([breadthorder/2, postorder/2]).

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

%% callbacks for hipe_bb_weights, hipe_range_split
-export([branch_preds/2]).

check_and_rewrite(CFG, Coloring, no_context) ->
  hipe_sparc_ra_postconditions:check_and_rewrite(CFG, Coloring, 'normal').

reverse_postorder(CFG, _) ->
  hipe_sparc_cfg:reverse_postorder(CFG).

non_alloc(CFG, no_context) ->
  non_alloc_1(hipe_sparc_registers:nr_args(), hipe_sparc_cfg:params(CFG)).

%% same as hipe_sparc_frame:fix_formals/2
non_alloc_1(0, Rest) -> Rest;
non_alloc_1(N, [_|Rest]) -> non_alloc_1(N-1, Rest);
non_alloc_1(_, []) -> [].

%% Liveness stuff

analyze(CFG, _) ->
  hipe_sparc_liveness_gpr:analyse(CFG).

livein(Liveness,L,_) ->
  [X || X <- hipe_sparc_liveness_gpr:livein(Liveness,L),
	hipe_sparc:temp_is_allocatable(X)].

liveout(BB_in_out_liveness,Label,_) ->
  [X || X <- hipe_sparc_liveness_gpr:liveout(BB_in_out_liveness,Label),
	hipe_sparc:temp_is_allocatable(X)].

%% Registers stuff

allocatable(no_context) ->
  hipe_sparc_registers:allocatable_gpr().

all_precoloured(no_context) ->
  hipe_sparc_registers:all_precoloured().

is_precoloured(Reg, _) ->
  hipe_sparc_registers:is_precoloured_gpr(Reg).

is_fixed(R, _) ->
  hipe_sparc_registers:is_fixed(R).

physical_name(Reg, _) ->
  Reg.

%% CFG stuff

labels(CFG, _) ->
  hipe_sparc_cfg:labels(CFG).

var_range(_CFG, _) ->
  hipe_gensym:var_range(sparc).

number_of_temporaries(_CFG, _) ->
  Highest_temporary = hipe_gensym:get_var(sparc),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L,_) ->
  hipe_sparc_cfg:bb(CFG,L).

update_bb(CFG,L,BB,_) ->
  hipe_sparc_cfg:bb_add(CFG,L,BB).

branch_preds(Branch,_) ->
  hipe_sparc_cfg:branch_preds(Branch).

%% SPARC stuff

def_use(Instruction, Ctx) ->
  {defines(Instruction, Ctx), uses(Instruction, Ctx)}.

uses(I, _) ->
  [X || X <- hipe_sparc_defuse:insn_use_gpr(I),
	hipe_sparc:temp_is_allocatable(X)].

defines(I, _) ->
  [X || X <- hipe_sparc_defuse:insn_def_gpr(I),
	hipe_sparc:temp_is_allocatable(X)].

defines_all_alloc(I, _) ->
  hipe_sparc_defuse:insn_defs_all_gpr(I).

is_move(Instruction, _) ->
  case hipe_sparc:is_pseudo_move(Instruction) of
    true ->
      Dst = hipe_sparc:pseudo_move_dst(Instruction),
      case hipe_sparc:temp_is_allocatable(Dst) of
	false -> false;
	_ ->
	  Src = hipe_sparc:pseudo_move_src(Instruction),
	  hipe_sparc:temp_is_allocatable(Src)
      end;
    false -> false
  end.

is_spill_move(Instruction, _) ->
  hipe_sparc:is_pseudo_spill_move(Instruction).

reg_nr(Reg, _) ->
  hipe_sparc:temp_reg(Reg).

mk_move(Src, Dst, _) ->
  hipe_sparc:mk_pseudo_move(Src, Dst).

mk_goto(Label, _) ->
  hipe_sparc:mk_b_label(Label).

redirect_jmp(Jmp, ToOld, ToNew, _) when is_integer(ToOld), is_integer(ToNew) ->
  hipe_sparc_cfg:redirect_jmp(Jmp, ToOld, ToNew).

new_label(_) ->
  hipe_gensym:get_next_label(sparc).

new_reg_nr(_) ->
  hipe_gensym:get_next_var(sparc).

update_reg_nr(Nr, Temp, _) ->
  hipe_sparc:mk_temp(Nr, hipe_sparc:temp_type(Temp)).

subst_temps(SubstFun, Instr, _) ->
  hipe_sparc_subst:insn_temps(
    fun(Op) ->
	case hipe_sparc:temp_is_allocatable(Op)
	  andalso hipe_sparc:temp_type(Op) =/= 'double'
	of
	  true -> SubstFun(Op);
	  false -> Op
	end
    end, Instr).

%%% Linear Scan stuff

new_spill_index(SpillIndex, _) when is_integer(SpillIndex) ->
  SpillIndex+1.

breadthorder(CFG, _) ->
  hipe_sparc_cfg:breadthorder(CFG).

postorder(CFG, _) ->
  hipe_sparc_cfg:postorder(CFG).

is_global(R, _) ->
  R =:= hipe_sparc_registers:temp1() orelse
  R =:= hipe_sparc_registers:temp2() orelse
  R =:= hipe_sparc_registers:temp3() orelse
  hipe_sparc_registers:is_fixed(R).

is_arg(R, _) ->
  hipe_sparc_registers:is_arg(R).

args(CFG, _) ->
  hipe_sparc_registers:args(hipe_sparc_cfg:arity(CFG)).
