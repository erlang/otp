%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
-export([allocatable/0,
	 is_precoloured/1,
	 %% var_range/1,
	 %% def_use/1,
	 %% is_fixed/1,
	 is_arg/1,
	 %% non_alloc/1,
	 new_spill_index/1,
	 number_of_temporaries/1
	]).

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
	 is_global/1,
	 reg_nr/1,
	 physical_name/1,
	 breadthorder/1,
	 postorder/1,
 	 reverse_postorder/1]).

breadthorder(CFG) ->
  hipe_x86_cfg:breadthorder(CFG).
postorder(CFG) ->
  hipe_x86_cfg:postorder(CFG).
reverse_postorder(CFG) ->
  hipe_x86_cfg:reverse_postorder(CFG).

is_global(_) ->
  false.

-ifdef(notdef).
is_fixed(_) ->
  false.
-endif.

is_arg(_) ->
  false.

args(_) ->
  [].

-ifdef(notdef).
non_alloc(_) ->
  [].
-endif.

%% Liveness stuff

analyze(CFG) ->
  ?HIPE_X86_LIVENESS:analyze(CFG).

livein(Liveness,L) ->
  [X || X <- ?HIPE_X86_LIVENESS:livein(Liveness,L),
 	     hipe_x86:temp_is_allocatable(X),
 	     hipe_x86:temp_type(X) =:= 'double'].

liveout(BB_in_out_liveness,Label) ->
  [X || X <- ?HIPE_X86_LIVENESS:liveout(BB_in_out_liveness,Label),
	     hipe_x86:temp_is_allocatable(X),
	     hipe_x86:temp_type(X) =:= 'double'].

%% Registers stuff

allocatable() ->
  ?HIPE_X86_REGISTERS:allocatable_x87().

is_precoloured(Reg) ->
  ?HIPE_X86_REGISTERS:is_precoloured_x87(Reg).

physical_name(Reg) ->
  Reg.

%% CFG stuff

labels(CFG) ->
  hipe_x86_cfg:labels(CFG).

-ifdef(notdef).
var_range(_CFG) ->
  {Min,Max} = hipe_gensym:var_range(x86),
  %% io:format("Var_range: ~w\n",[{Min,Max}]),
  {Min,Max}.
-endif.

number_of_temporaries(_CFG) ->
  Highest_temporary = hipe_gensym:get_var(x86),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_x86_cfg:bb(CFG,L).

%% X86 stuff

-ifdef(notdef).
def_use(Instruction) ->
  {[X || X <- ?HIPE_X86_DEFUSE:insn_def(Instruction),
	      hipe_x86:temp_is_allocatable(X),
	      temp_is_double(X)],
   [X || X <- ?HIPE_X86_DEFUSE:insn_use(Instruction),
	      hipe_x86:temp_is_allocatable(X),
	      temp_is_double(X)]
  }.
-endif.

uses(I) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_use(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     temp_is_double(X)].

defines(I) ->
  [X || X <- ?HIPE_X86_DEFUSE:insn_def(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     temp_is_double(X)].

temp_is_double(Temp) ->
  hipe_x86:temp_type(Temp) =:= 'double'.

reg_nr(Reg) ->
  hipe_x86:temp_reg(Reg).

new_spill_index(SpillIndex) ->
  SpillIndex+1.
