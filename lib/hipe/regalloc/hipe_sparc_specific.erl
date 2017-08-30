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

-module(hipe_sparc_specific).

%% for hipe_coalescing_regalloc:
-export([number_of_temporaries/1
	 ,analyze/1
	 ,labels/1
	 ,all_precoloured/0
	 ,bb/2
	 ,liveout/2
	 ,reg_nr/1
	 ,def_use/1
	 ,is_move/1
	 ,is_precoloured/1
	 ,var_range/1
	 ,allocatable/0
	 ,non_alloc/1
	 ,physical_name/1
	 ,reverse_postorder/1
	 ,livein/2
	 ,uses/1
	 ,defines/1
	]).

%% for hipe_graph_coloring_regalloc:
-export([is_fixed/1]).

%% for hipe_ls_regalloc:
-export([args/1, is_arg/1, is_global/1, new_spill_index/1]).
-export([breadthorder/1, postorder/1]).

%% callbacks for hipe_regalloc_loop
-export([defun_to_cfg/1,
	 check_and_rewrite/2]).

defun_to_cfg(Defun) ->
  hipe_sparc_cfg:init(Defun).

check_and_rewrite(Defun, Coloring) ->
  hipe_sparc_ra_postconditions:check_and_rewrite(Defun, Coloring, 'normal').

reverse_postorder(CFG) ->
  hipe_sparc_cfg:reverse_postorder(CFG).

non_alloc(CFG) ->
  non_alloc(hipe_sparc_registers:nr_args(), hipe_sparc_cfg:params(CFG)).

%% same as hipe_sparc_frame:fix_formals/2
non_alloc(0, Rest) -> Rest;
non_alloc(N, [_|Rest]) -> non_alloc(N-1, Rest);
non_alloc(_, []) -> [].

%% Liveness stuff

analyze(CFG) ->
  hipe_sparc_liveness_gpr:analyse(CFG).

livein(Liveness,L) ->
  [X || X <- hipe_sparc_liveness_gpr:livein(Liveness,L),
	hipe_sparc:temp_is_allocatable(X)].

liveout(BB_in_out_liveness,Label) ->
  [X || X <- hipe_sparc_liveness_gpr:liveout(BB_in_out_liveness,Label),
	hipe_sparc:temp_is_allocatable(X)].

%% Registers stuff

allocatable() ->
  hipe_sparc_registers:allocatable_gpr().

all_precoloured() ->
  hipe_sparc_registers:all_precoloured().

is_precoloured(Reg) ->
  hipe_sparc_registers:is_precoloured_gpr(Reg).

is_fixed(R) ->
  hipe_sparc_registers:is_fixed(R).

physical_name(Reg) ->
  Reg.

%% CFG stuff

labels(CFG) ->
  hipe_sparc_cfg:labels(CFG).

var_range(_CFG) ->
  hipe_gensym:var_range(sparc).

number_of_temporaries(_CFG) ->
  Highest_temporary = hipe_gensym:get_var(sparc),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_sparc_cfg:bb(CFG,L).

%% SPARC stuff

def_use(Instruction) ->
  {defines(Instruction), uses(Instruction)}.

uses(I) ->
  [X || X <- hipe_sparc_defuse:insn_use_gpr(I),
	hipe_sparc:temp_is_allocatable(X)].

defines(I) ->
  [X || X <- hipe_sparc_defuse:insn_def_gpr(I),
	hipe_sparc:temp_is_allocatable(X)].

is_move(Instruction) ->
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

reg_nr(Reg) ->
  hipe_sparc:temp_reg(Reg).

%%% Linear Scan stuff

new_spill_index(SpillIndex) when is_integer(SpillIndex) ->
  SpillIndex+1.

breadthorder(CFG) ->
  hipe_sparc_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_sparc_cfg:postorder(CFG).

is_global(R) ->
  R =:= hipe_sparc_registers:temp1() orelse
  R =:= hipe_sparc_registers:temp2() orelse
  R =:= hipe_sparc_registers:temp3() orelse
  hipe_sparc_registers:is_fixed(R).

is_arg(R) ->
  hipe_sparc_registers:is_arg(R).

args(CFG) ->
  hipe_sparc_registers:args(hipe_sparc_cfg:arity(CFG)).
