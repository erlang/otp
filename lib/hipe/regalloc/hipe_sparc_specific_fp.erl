%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

-module(hipe_sparc_specific_fp).

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
%%-export([args/1, is_arg/1, is_global, new_spill_index/1]).
%%-export([breadthorder/1, postorder/1]).

%% callbacks for hipe_regalloc_loop
-export([defun_to_cfg/1,
	 check_and_rewrite/2]).

defun_to_cfg(Defun) ->
  hipe_sparc_cfg:init(Defun).

check_and_rewrite(Defun, Coloring) ->
  hipe_sparc_ra_postconditions_fp:check_and_rewrite(Defun, Coloring).

reverse_postorder(CFG) ->
  hipe_sparc_cfg:reverse_postorder(CFG).

non_alloc(_CFG) ->
  [].

%% Liveness stuff

analyze(CFG) ->
  hipe_sparc_liveness_fpr:analyse(CFG).

livein(Liveness, L) ->
  hipe_sparc_liveness_fpr:livein(Liveness, L).

liveout(BB_in_out_liveness, Label) ->
  hipe_sparc_liveness_fpr:liveout(BB_in_out_liveness, Label).

%% Registers stuff

allocatable() ->
  hipe_sparc_registers:allocatable_fpr().

all_precoloured() ->
  allocatable().

is_precoloured(Reg) ->
  hipe_sparc_registers:is_precoloured_fpr(Reg).

is_fixed(_Reg) ->
  false.

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

bb(CFG, L) ->
  hipe_sparc_cfg:bb(CFG, L).

%% SPARC stuff

def_use(I) ->
  {defines(I), uses(I)}.

uses(I) ->
  hipe_sparc_defuse:insn_use_fpr(I).

defines(I) ->
  hipe_sparc_defuse:insn_def_fpr(I).

is_move(I) ->
  hipe_sparc:is_pseudo_fmove(I).
 
reg_nr(Reg) ->
  hipe_sparc:temp_reg(Reg).

-ifdef(notdef).
new_spill_index(SpillIndex)->
  SpillIndex+1.

breadthorder(CFG) ->
  hipe_sparc_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_sparc_cfg:postorder(CFG).

is_global(_R) ->
  false.

is_arg(_R) ->
  false.

args(_CFG) ->
  [].
-endif.
