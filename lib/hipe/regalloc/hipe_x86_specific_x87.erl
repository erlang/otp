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
-define(HIPE_X86_SPECIFIC_X87, hipe_amd64_specific_x87).
-define(HIPE_X86_LIVENESS, hipe_amd64_liveness).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_DEFUSE, hipe_amd64_defuse).
-else.
-define(HIPE_X86_SPECIFIC_X87, hipe_x86_specific_x87).
-define(HIPE_X86_LIVENESS, hipe_x86_liveness).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-define(HIPE_X86_DEFUSE, hipe_x86_defuse).
-endif.

-module(?HIPE_X86_SPECIFIC_X87).
-export([allocatable/2,
	 is_precoloured/2,
	 %% var_range/2,
	 %% def_use/2,
	 %% is_fixed/2,
	 is_arg/2,
	 %% non_alloc/2,
	 new_spill_index/2,
	 number_of_temporaries/2
	]).

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
	 is_spill_move/2,
	 is_global/2,
	 reg_nr/2,
	 physical_name/2,
	 breadthorder/2,
	 postorder/2,
	 reverse_postorder/2]).

%% callbacks for hipe_x86_ra_ls
-export([check_and_rewrite/4]).

%% Rewrite happens in hipe_x86_ra_finalise:finalise/4
check_and_rewrite(CFG, _Coloring, 'linearscan', _) ->
  {CFG, false}.

breadthorder(CFG, _) ->
  hipe_x86_cfg:breadthorder(CFG).
postorder(CFG, _) ->
  hipe_x86_cfg:postorder(CFG).
reverse_postorder(CFG, _) ->
  hipe_x86_cfg:reverse_postorder(CFG).

is_global(_, _) ->
  false.

-ifdef(notdef).
is_fixed(_, _) ->
  false.
-endif.

is_arg(_, _) ->
  false.

args(_, _) ->
  [].

-ifdef(notdef).
non_alloc(_, _) ->
  [].
-endif.

%% Liveness stuff

analyze(CFG, _) ->
  ?HIPE_X86_LIVENESS:analyze(CFG).

livein(Liveness,L,_) ->
  [X || X <- ?HIPE_X86_LIVENESS:livein(Liveness,L),
 	     hipe_x86:temp_is_allocatable(X),
 	     hipe_x86:temp_type(X) =:= 'double'].

liveout(BB_in_out_liveness,Label,_) ->
  [X || X <- ?HIPE_X86_LIVENESS:liveout(BB_in_out_liveness,Label),
	     hipe_x86:temp_is_allocatable(X),
	     hipe_x86:temp_type(X) =:= 'double'].

%% Registers stuff

allocatable('linearscan', _) ->
  ?HIPE_X86_REGISTERS:allocatable_x87().

is_precoloured(Reg, _) ->
  ?HIPE_X86_REGISTERS:is_precoloured_x87(Reg).

physical_name(Reg, _) ->
  Reg.

%% CFG stuff

labels(CFG, _) ->
  hipe_x86_cfg:labels(CFG).

-ifdef(notdef).
var_range(_CFG, _) ->
  {Min,Max} = hipe_gensym:var_range(x86),
  %% io:format("Var_range: ~w\n",[{Min,Max}]),
  {Min,Max}.
-endif.

number_of_temporaries(_CFG, _) ->
  Highest_temporary = hipe_gensym:get_var(x86),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L,_) ->
  hipe_x86_cfg:bb(CFG,L).

%% X86 stuff

-ifdef(notdef).
def_use(Instruction, _) ->
  {[X || X <- ?HIPE_X86_DEFUSE:insn_def(Instruction),
	      hipe_x86:temp_is_allocatable(X),
	      temp_is_double(X)],
   [X || X <- ?HIPE_X86_DEFUSE:insn_use(Instruction),
	      hipe_x86:temp_is_allocatable(X),
	      temp_is_double(X)]
  }.
-endif.

uses(I, _) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_use(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     temp_is_double(X)].

defines(I, _) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_def(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     temp_is_double(X)].

defines_all_alloc(I, _) -> hipe_amd64_defuse:insn_defs_all(I).

is_spill_move(I, _) ->
  hipe_x86:is_pseudo_spill_fmove(I).

temp_is_double(Temp) ->
  hipe_x86:temp_type(Temp) =:= 'double'.

reg_nr(Reg, _) ->
  hipe_x86:temp_reg(Reg).

new_spill_index(SpillIndex, _) ->
  SpillIndex+1.
