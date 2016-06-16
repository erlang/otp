%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%% %CopyrightEnd%
%%%
%%% Common wrapper for graph_coloring and coalescing regallocs.

-module(hipe_regalloc_loop).
-export([ra/5, ra_fp/4]).

%%-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

ra(CFG, SpillIndex, Options, RegAllocMod, TargetMod) ->
  {NewCFG, Coloring, _NewSpillIndex} =
    ra_common(CFG, SpillIndex, Options, RegAllocMod, TargetMod),
  {NewCFG, Coloring}.

ra_fp(CFG, Options, RegAllocMod, TargetMod) ->
  ra_common(CFG, 0, Options, RegAllocMod, TargetMod).

ra_common(CFG, SpillIndex, Options, RegAllocMod, TargetMod) ->
  ?inc_counter(ra_calls_counter, 1),
  SpillLimit = TargetMod:number_of_temporaries(CFG),
  alloc(CFG, SpillLimit, SpillIndex, Options, RegAllocMod, TargetMod).

alloc(CFG, SpillLimit, SpillIndex, Options, RegAllocMod, TargetMod) ->
  ?inc_counter(ra_iteration_counter, 1),
  {Coloring, _NewSpillIndex, Liveness} =
    case proplists:get_bool(ra_prespill, Options) of
      true ->
	hipe_regalloc_prepass:regalloc(
	  RegAllocMod, CFG, SpillIndex, SpillLimit, TargetMod, Options);
      false ->
	RegAllocMod:regalloc(CFG, SpillIndex, SpillLimit, TargetMod, Options)
    end,
  {NewCFG, DidSpill} = TargetMod:check_and_rewrite(CFG, Coloring),
  case DidSpill of
    false -> %% No new temps, we are done.
      ?add_spills(Options, _NewSpillIndex),
      TempMap = hipe_temp_map:cols2tuple(Coloring, TargetMod),
      {TempMap2, NewSpillIndex2} =
	hipe_spillmin:stackalloc(CFG, Liveness, [], SpillIndex, Options,
				 TargetMod, TempMap),
      Coloring2 = 
	hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), TempMap2),
      %% case proplists:get_bool(verbose_spills, Options) of
      %%   true ->
      %%     ?msg("Num spill slots used: ~p~n", [NewSpillIndex2-SpillIndex]);
      %%   false ->
      %%     ok
      %% end,
      {NewCFG, Coloring2, NewSpillIndex2};
    _ ->
      %% Since SpillLimit is used as a low-water-mark
      %% the list of temps not to spill is uninteresting.
      alloc(NewCFG, SpillLimit, SpillIndex, Options, RegAllocMod, TargetMod)
  end.
