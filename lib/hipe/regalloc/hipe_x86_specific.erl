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

-ifdef(HIPE_AMD64).
-define(HIPE_X86_SPECIFIC, hipe_amd64_specific).
-define(HIPE_X86_RA_POSTCONDITIONS, hipe_amd64_ra_postconditions).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_LIVENESS, hipe_amd64_liveness).
-define(HIPE_X86_DEFUSE, hipe_amd64_defuse).
-else.
-define(HIPE_X86_SPECIFIC, hipe_x86_specific).
-define(HIPE_X86_RA_POSTCONDITIONS, hipe_x86_ra_postconditions).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-define(HIPE_X86_LIVENESS, hipe_x86_liveness).
-define(HIPE_X86_DEFUSE, hipe_x86_defuse).
-endif.

-module(?HIPE_X86_SPECIFIC).

-export([number_of_temporaries/1]).

%% The following exports are used as M:F(...) calls from other modules;
%% e.g. hipe_x86_ra_ls.
-export([analyze/1,
	 bb/2,
	 args/1,
	 labels/1,
	 livein/2,
	 liveout/2,
	 uses/1,
	 defines/1,
	 def_use/1,
	 is_arg/1,	% used by hipe_ls_regalloc
	 is_move/1,
	 is_fixed/1,	% used by hipe_graph_coloring_regalloc
	 is_global/1,
	 is_precoloured/1,
	 reg_nr/1,
	 non_alloc/1,
	 allocatable/0,
	 physical_name/1,
	 all_precoloured/0,
	 new_spill_index/1,	% used by hipe_ls_regalloc
	 var_range/1,
	 breadthorder/1,
	 postorder/1,
	 reverse_postorder/1]).

%% callbacks for hipe_regalloc_loop
-export([defun_to_cfg/1,
	 check_and_rewrite/2]).

defun_to_cfg(Defun) ->
  hipe_x86_cfg:init(Defun).

check_and_rewrite(Defun, Coloring) ->
  ?HIPE_X86_RA_POSTCONDITIONS:check_and_rewrite(Defun, Coloring, 'normal').

reverse_postorder(CFG) ->
  hipe_x86_cfg:reverse_postorder(CFG).

breadthorder(CFG) ->
  hipe_x86_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_x86_cfg:postorder(CFG).

%% Globally defined registers for linear scan
is_global(R) ->
  ?HIPE_X86_REGISTERS:temp1() =:= R orelse
  ?HIPE_X86_REGISTERS:temp0() =:= R orelse
  ?HIPE_X86_REGISTERS:is_fixed(R).

is_fixed(R) ->
  ?HIPE_X86_REGISTERS:is_fixed(R).

is_arg(R) ->
  ?HIPE_X86_REGISTERS:is_arg(R).

args(CFG) ->
  ?HIPE_X86_REGISTERS:args(hipe_x86_cfg:arity(CFG)).

non_alloc(CFG) ->
  non_alloc(?HIPE_X86_REGISTERS:nr_args(), hipe_x86_cfg:params(CFG)).

%% same as hipe_x86_frame:fix_formals/2
non_alloc(0, Rest) -> Rest;
non_alloc(N, [_|Rest]) -> non_alloc(N-1, Rest);
non_alloc(_, []) -> [].

%% Liveness stuff

analyze(CFG) ->
  ?HIPE_X86_LIVENESS:analyze(CFG).

livein(Liveness,L) ->
  [X || X <- ?HIPE_X86_LIVENESS:livein(Liveness,L),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:fcalls(),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:heap_limit(),
	hipe_x86:temp_type(X) =/= 'double'].

liveout(BB_in_out_liveness,Label) ->
  [X || X <- ?HIPE_X86_LIVENESS:liveout(BB_in_out_liveness,Label),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:fcalls(),
	hipe_x86:temp_reg(X) =/= ?HIPE_X86_REGISTERS:heap_limit(),
	hipe_x86:temp_type(X) =/= 'double'].

%% Registers stuff

allocatable() ->
  ?HIPE_X86_REGISTERS:allocatable().

all_precoloured() ->
  ?HIPE_X86_REGISTERS:all_precoloured().

is_precoloured(Reg) ->
  ?HIPE_X86_REGISTERS:is_precoloured(Reg).

physical_name(Reg) ->
  Reg.

%% CFG stuff

labels(CFG) ->
  hipe_x86_cfg:labels(CFG).

var_range(_CFG) ->
  hipe_gensym:var_range(x86).

number_of_temporaries(_CFG) ->
  Highest_temporary = hipe_gensym:get_var(x86),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_x86_cfg:bb(CFG,L).

%% X86 stuff

def_use(Instruction) ->
  {[X || X <- ?HIPE_X86_DEFUSE:insn_def(Instruction),
	 hipe_x86:temp_is_allocatable(X),
	 hipe_x86:temp_type(X) =/= 'double'],
   [X || X <- ?HIPE_X86_DEFUSE:insn_use(Instruction),
	 hipe_x86:temp_is_allocatable(X),
	 hipe_x86:temp_type(X) =/= 'double']
  }.

uses(I) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_use(I),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_type(X) =/= 'double'].

defines(I) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_def(I),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_type(X) =/= 'double'].

is_move(Instruction) ->
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

reg_nr(Reg) ->
  hipe_x86:temp_reg(Reg).

new_spill_index(SpillIndex) when is_integer(SpillIndex) ->
  SpillIndex+1.
