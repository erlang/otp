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

-ifdef(HIPE_AMD64).
-define(HIPE_X86_SPECIFIC, hipe_amd64_specific).
-define(HIPE_X86_RA_POSTCONDITIONS, hipe_amd64_ra_postconditions).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_LIVENESS, hipe_amd64_liveness).
-define(HIPE_X86_DEFUSE, hipe_amd64_defuse).
-define(HIPE_X86_SUBST, hipe_amd64_subst).
-else.
-define(HIPE_X86_SPECIFIC, hipe_x86_specific).
-define(HIPE_X86_RA_POSTCONDITIONS, hipe_x86_ra_postconditions).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-define(HIPE_X86_LIVENESS, hipe_x86_liveness).
-define(HIPE_X86_DEFUSE, hipe_x86_defuse).
-define(HIPE_X86_SUBST, hipe_x86_subst).
-endif.

-module(?HIPE_X86_SPECIFIC).

-export([number_of_temporaries/2]).

%% The following exports are used as M:F(...) calls from other modules;
%% e.g. hipe_x86_ra_ls.
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
	 is_arg/2,	% used by hipe_ls_regalloc
	 is_move/2,
	 is_spill_move/2,
	 is_fixed/2,	% used by hipe_graph_coloring_regalloc
	 is_global/2,
	 is_precoloured/2,
	 reg_nr/2,
	 non_alloc/2,
	 allocatable/1,
	 physical_name/2,
	 all_precoloured/1,
	 new_spill_index/2,	% used by hipe_ls_regalloc
	 var_range/2,
	 breadthorder/2,
	 postorder/2,
	 reverse_postorder/2]).

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
  ?HIPE_X86_RA_POSTCONDITIONS:check_and_rewrite(CFG, Coloring, 'normal').

reverse_postorder(CFG, _) ->
  hipe_x86_cfg:reverse_postorder(CFG).

breadthorder(CFG, _) ->
  hipe_x86_cfg:breadthorder(CFG).

postorder(CFG, _) ->
  hipe_x86_cfg:postorder(CFG).

%% Globally defined registers for linear scan
is_global(R, _) ->
  ?HIPE_X86_REGISTERS:temp1() =:= R orelse
  ?HIPE_X86_REGISTERS:temp0() =:= R orelse
  ?HIPE_X86_REGISTERS:is_fixed(R).

is_fixed(R, _) ->
  ?HIPE_X86_REGISTERS:is_fixed(R).

is_arg(R, _) ->
  ?HIPE_X86_REGISTERS:is_arg(R).

args(CFG, _) ->
  ?HIPE_X86_REGISTERS:args(hipe_x86_cfg:arity(CFG)).

non_alloc(CFG, _) ->
  non_alloc_1(?HIPE_X86_REGISTERS:nr_args(), hipe_x86_cfg:params(CFG)).

%% same as hipe_x86_frame:fix_formals/2
non_alloc_1(0, Rest) -> Rest;
non_alloc_1(N, [_|Rest]) -> non_alloc_1(N-1, Rest);
non_alloc_1(_, []) -> [].

%% Liveness stuff

analyze(CFG, _) ->
  ?HIPE_X86_LIVENESS:analyze(CFG).

livein(Liveness,L,_) ->
  [X || X <- ?HIPE_X86_LIVENESS:livein(Liveness,L),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:fcalls(),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:heap_limit(),
	hipe_x86:temp_type(X) =/= 'double'].

liveout(BB_in_out_liveness,Label,_) ->
  [X || X <- ?HIPE_X86_LIVENESS:liveout(BB_in_out_liveness,Label),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:fcalls(),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:heap_limit(),
	hipe_x86:temp_type(X) =/= 'double'].

%% Registers stuff

allocatable(_) ->
  ?HIPE_X86_REGISTERS:allocatable().

all_precoloured(_) ->
  ?HIPE_X86_REGISTERS:all_precoloured().

is_precoloured(Reg,_) ->
  ?HIPE_X86_REGISTERS:is_precoloured(Reg).

physical_name(Reg,_) ->
  Reg.

%% CFG stuff

labels(CFG,_) ->
  hipe_x86_cfg:labels(CFG).

var_range(_CFG,_) ->
  hipe_gensym:var_range(x86).

number_of_temporaries(_CFG,_) ->
  Highest_temporary = hipe_gensym:get_var(x86),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L,_) ->
  hipe_x86_cfg:bb(CFG,L).

update_bb(CFG,L,BB,_) ->
  hipe_x86_cfg:bb_add(CFG,L,BB).

branch_preds(Instr,_) ->
  hipe_x86_cfg:branch_preds(Instr).

%% X86 stuff

def_use(Instruction,_) ->
  {[X || X <- ?HIPE_X86_DEFUSE:insn_def(Instruction),
	 hipe_x86:temp_is_allocatable(X),
	 hipe_x86:temp_type(X) =/= 'double'],
   [X || X <- ?HIPE_X86_DEFUSE:insn_use(Instruction),
	 hipe_x86:temp_is_allocatable(X),
	 hipe_x86:temp_type(X) =/= 'double']
  }.

uses(I,_) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_use(I),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_type(X) =/= 'double'].

defines(I,_) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_def(I),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_type(X) =/= 'double'].

defines_all_alloc(I,_) -> ?HIPE_X86_DEFUSE:insn_defs_all(I).

is_move(Instruction,_) ->
  case hipe_x86:is_move(Instruction) of
    true ->
      Src = hipe_x86:move_src(Instruction),
      Dst = hipe_x86:move_dst(Instruction),
      case hipe_x86:is_temp(Src) of
	true ->
	  case hipe_x86:temp_is_allocatable(Src) of
	    true ->
	      case hipe_x86:is_temp(Dst) of
		true ->
		  hipe_x86:temp_is_allocatable(Dst);
		false -> false
	      end;
	    false -> false
	  end;
	false -> false
      end;
    false -> false
  end.

is_spill_move(Instruction,_) ->
  hipe_x86:is_pseudo_spill_move(Instruction).

reg_nr(Reg,_) ->
  hipe_x86:temp_reg(Reg).

mk_move(Src, Dst, _) ->
  hipe_x86:mk_move(Src, Dst).

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

update_reg_nr(Nr, Temp, _) ->
  hipe_x86:mk_temp(Nr, hipe_x86:temp_type(Temp)).

subst_temps(SubstFun, Instr, _) ->
  ?HIPE_X86_SUBST:insn_temps(
    fun(Op) ->
	case hipe_x86:temp_is_allocatable(Op)
	  andalso hipe_x86:temp_type(Op) =/= 'double'
	of
	  true -> SubstFun(Op);
	  false -> Op
	end
    end, Instr).

new_spill_index(SpillIndex, _) when is_integer(SpillIndex) ->
  SpillIndex+1.
