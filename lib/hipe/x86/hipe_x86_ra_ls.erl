%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% Linear Scan register allocator for x86

-ifdef(HIPE_AMD64).
-define(HIPE_X86_RA_LS,			hipe_amd64_ra_ls).
-define(HIPE_X86_PP,			hipe_amd64_pp).
-define(HIPE_X86_RA_POSTCONDITIONS,	hipe_amd64_ra_postconditions).
-define(HIPE_X86_REGISTERS,		hipe_amd64_registers).
-define(HIPE_X86_SPECIFIC,		hipe_amd64_specific).
-else.
-define(HIPE_X86_RA_LS,			hipe_x86_ra_ls).
-define(HIPE_X86_PP,			hipe_x86_pp).
-define(HIPE_X86_RA_POSTCONDITIONS,	hipe_x86_ra_postconditions).
-define(HIPE_X86_REGISTERS,		hipe_x86_registers).
-define(HIPE_X86_SPECIFIC,		hipe_x86_specific).
-endif.

-module(?HIPE_X86_RA_LS).
-export([ra/4,ra_fp/5]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

ra(CFG, Liveness, SpillIndex, Options) ->
  SpillLimit = ?HIPE_X86_SPECIFIC:number_of_temporaries(
		 CFG, no_context),
  ?inc_counter(bbs_counter, length(hipe_x86_cfg:labels(CFG))),
  alloc(CFG, Liveness, SpillIndex, SpillLimit, Options).

ra_fp(CFG, Liveness, Options, TargetMod, TargetCtx) ->
  ?inc_counter(ra_calls_counter,1),
  %% ?inc_counter(ra_caller_saves_counter,count_caller_saves(CFG)),
  SpillIndex = 0,
  SpillLimit = TargetMod:number_of_temporaries(CFG, TargetCtx),
  ?inc_counter(bbs_counter, length(hipe_x86_cfg:labels(CFG))),

  ?inc_counter(ra_iteration_counter,1),
  %% ?HIPE_X86_PP:pp(Defun),

  {Coloring,NewSpillIndex} =
    regalloc(CFG, Liveness,
	     TargetMod:allocatable('linearscan', TargetCtx),
	     [hipe_x86_cfg:start_label(CFG)],
	     SpillIndex, SpillLimit, Options,
	     TargetMod, TargetCtx),

  {NewCFG, _DidSpill} =
    TargetMod:check_and_rewrite(CFG, Coloring, 'linearscan', TargetCtx),
  TempMap = hipe_temp_map:cols2tuple(Coloring, TargetMod, TargetCtx),
  {TempMap2, NewSpillIndex2} =
    hipe_spillmin:stackalloc(CFG, Liveness, [], SpillIndex, Options,
			     TargetMod, TargetCtx, TempMap),
  Coloring2 =
    hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), TempMap2),
  ?add_spills(Options, NewSpillIndex),
  {NewCFG, Liveness, Coloring2, NewSpillIndex2}.

alloc(CFG, Liveness, SpillIndex, SpillLimit, Options) ->
  ?inc_counter(ra_iteration_counter,1), 
  %% ?HIPE_X86_PP:pp(Defun),	
  {Coloring, NewSpillIndex} =
    regalloc(
      CFG, Liveness,
      ?HIPE_X86_REGISTERS:allocatable()--
      [?HIPE_X86_REGISTERS:temp1(),
       ?HIPE_X86_REGISTERS:temp0()],
      [hipe_x86_cfg:start_label(CFG)],
      SpillIndex, SpillLimit, Options,
      ?HIPE_X86_SPECIFIC, no_context),
  {NewCFG, _DidSpill} =
    ?HIPE_X86_RA_POSTCONDITIONS:check_and_rewrite(
      CFG, Coloring, 'linearscan'),
  %% ?HIPE_X86_PP:pp(NewDefun),
  TempMap = hipe_temp_map:cols2tuple(Coloring, ?HIPE_X86_SPECIFIC, no_context),
  {TempMap2,NewSpillIndex2} =
    hipe_spillmin:stackalloc(CFG, Liveness, [], SpillIndex, Options,
			     ?HIPE_X86_SPECIFIC, no_context, TempMap),
  Coloring2 = 
    hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), TempMap2),
  case proplists:get_bool(verbose_spills, Options) of
    true ->
      ?msg("Stack slot size: ~p~n",[NewSpillIndex2-SpillIndex]);
    false ->
      ok
  end,
  ?add_spills(Options, NewSpillIndex),
  {NewCFG, Liveness, Coloring2}.

regalloc(CFG, Liveness, PhysRegs, Entrypoints, SpillIndex, DontSpill, Options,
	 TgtMod, TgtCtx) ->
  hipe_ls_regalloc:regalloc(CFG, Liveness, PhysRegs, Entrypoints, SpillIndex,
			    DontSpill, Options, TgtMod, TgtCtx).
