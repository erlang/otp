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

-module(hipe_amd64_specific_sse2).

-export([number_of_temporaries/2]).

% The following exports are used as M:F(...) calls from other modules;
%% e.g. hipe_amd64_ra_ls.
-export([analyze/2,
	 bb/3,
	 args/2,
	 labels/2,
	 livein/3,
	 liveout/3,
	 uses/2,
	 defines/2,
	 defines_all_alloc/2,
	 def_use/2,
	 is_arg/2,	%% used by hipe_ls_regalloc
	 is_move/2,
	 is_spill_move/2,
	 is_fixed/2,	%% used by hipe_graph_coloring_regalloc
	 is_global/2,
	 is_precoloured/2,
	 reg_nr/2,
	 non_alloc/2,
	 allocatable/1,
	 allocatable/2,
	 temp0/1,
	 physical_name/2,
	 all_precoloured/1,
	 new_spill_index/2,	%% used by hipe_ls_regalloc
	 var_range/2,
	 breadthorder/2,
	 postorder/2,
	 reverse_postorder/2]).

%% callbacks for hipe_regalloc_loop
-export([check_and_rewrite/3,
	 check_and_rewrite/4]).

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

%%----------------------------------------------------------------------------

-include("../flow/cfg.hrl").

%%----------------------------------------------------------------------------

check_and_rewrite(CFG, Coloring, no_context) ->
  hipe_amd64_ra_sse2_postconditions:check_and_rewrite(CFG, Coloring).

check_and_rewrite(CFG, Coloring, Strategy, no_context) ->
  hipe_amd64_ra_sse2_postconditions:check_and_rewrite(
    CFG, Coloring, Strategy).

reverse_postorder(CFG, _) ->
  hipe_x86_cfg:reverse_postorder(CFG).

breadthorder(CFG, _) ->
  hipe_x86_cfg:breadthorder(CFG).

postorder(CFG, _) ->
  hipe_x86_cfg:postorder(CFG).

is_global(Reg, _) ->
  hipe_amd64_registers:sse2_temp0() =:= Reg.
 
is_fixed(_Reg, _) ->
  false.

is_arg(_Reg, _) ->
  false.

-spec args(#cfg{}, no_context) -> [].
args(_CFG, _) ->
  [].
 
non_alloc(_, _) ->
  [].

%% Liveness stuff

analyze(CFG, _) ->
  hipe_amd64_liveness:analyze(CFG).

livein(Liveness, L, _) ->
  [X || X <- hipe_amd64_liveness:livein(Liveness, L),
 	     hipe_x86:temp_is_allocatable(X),
	     hipe_x86:temp_type(X) =:= 'double'].

liveout(BB_in_out_liveness, Label, _) ->
  [X || X <- hipe_amd64_liveness:liveout(BB_in_out_liveness, Label),
 	     hipe_x86:temp_is_allocatable(X),
	     hipe_x86:temp_type(X) =:= 'double'].

%% Registers stuff

allocatable(Ctx) ->
  allocatable('normal', Ctx).

allocatable('normal', _) ->
  hipe_amd64_registers:allocatable_sse2();
allocatable('linearscan', _) ->
  hipe_amd64_registers:allocatable_sse2() --
    [hipe_amd64_registers:sse2_temp0()].

temp0(_) ->
  hipe_amd64_registers:sse2_temp0().

all_precoloured(Ctx) ->
  allocatable(Ctx).

is_precoloured(Reg, _) ->
  hipe_amd64_registers:is_precoloured_sse2(Reg).

physical_name(Reg, _) ->
  Reg.

%% CFG stuff

labels(CFG, _) ->
  hipe_x86_cfg:labels(CFG).

var_range(_CFG, _) ->
  hipe_gensym:var_range(x86).

-spec number_of_temporaries(#cfg{}, no_context) -> non_neg_integer().
number_of_temporaries(_CFG, _) ->
  Highest_temporary = hipe_gensym:get_var(x86),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG, L, _) ->
  hipe_x86_cfg:bb(CFG, L).

update_bb(CFG,L,BB,_) ->
  hipe_x86_cfg:bb_add(CFG,L,BB).

branch_preds(Instr,_) ->
  hipe_x86_cfg:branch_preds(Instr).

%% AMD64 stuff

def_use(Instruction, _) ->
  {[X || X <- hipe_amd64_defuse:insn_def(Instruction), 
 	   hipe_x86:temp_is_allocatable(X),
 	   hipe_x86:temp_type(X) =:= 'double'],
   [X || X <- hipe_amd64_defuse:insn_use(Instruction), 
 	   hipe_x86:temp_is_allocatable(X),
	   hipe_x86:temp_type(X) =:= 'double']
  }.

uses(I, _) ->
  [X || X <- hipe_amd64_defuse:insn_use(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     hipe_x86:temp_type(X) =:= 'double'].

defines(I, _) ->
  [X || X <- hipe_amd64_defuse:insn_def(I),
	     hipe_x86:temp_is_allocatable(X),
	     hipe_x86:temp_type(X) =:= 'double'].

defines_all_alloc(I, _) -> hipe_amd64_defuse:insn_defs_all(I).

is_move(Instruction, _) ->
  case hipe_x86:is_fmove(Instruction) of
    true ->
      Src = hipe_x86:fmove_src(Instruction),
      Dst = hipe_x86:fmove_dst(Instruction),
      hipe_x86:is_temp(Src) andalso hipe_x86:temp_is_allocatable(Src)
	andalso hipe_x86:is_temp(Dst) andalso hipe_x86:temp_is_allocatable(Dst);
    false -> false
  end.

is_spill_move(Instruction,_) ->
  hipe_x86:is_pseudo_spill_fmove(Instruction).
 
reg_nr(Reg, _) ->
  hipe_x86:temp_reg(Reg).

mk_move(Src, Dst, _) ->
  hipe_x86:mk_fmove(Src, Dst).

mk_goto(Label, _) ->
  hipe_x86:mk_jmp_label(Label).

redirect_jmp(Jmp, ToOld, ToNew, _) when is_integer(ToOld), is_integer(ToNew) ->
  Ref = make_ref(),
  put(Ref, false),
  I = hipe_x86_subst:insn_lbls(
	fun(Tgt) ->
	    if Tgt =:= ToOld -> put(Ref, true), ToNew;
	       is_integer(Tgt) -> Tgt
	    end
	end, Jmp),
  true = erase(Ref), % Assert that something was rewritten
  I.

new_label(_) ->
  hipe_gensym:get_next_label(x86).

new_reg_nr(_) ->
  hipe_gensym:get_next_var(x86).

update_reg_nr(Nr, _Temp, _) ->
  hipe_x86:mk_temp(Nr, 'double').

subst_temps(SubstFun, Instr, _) ->
  hipe_amd64_subst:insn_temps(
    fun(Op) ->
	case hipe_x86:temp_is_allocatable(Op)
	  andalso hipe_x86:temp_type(Op) =:= 'double'
	of
	  true -> SubstFun(Op);
	  false -> Op
	end
    end, Instr).

-spec new_spill_index(non_neg_integer(), no_context) -> pos_integer().
new_spill_index(SpillIndex, _) when is_integer(SpillIndex) ->
  SpillIndex + 1.
