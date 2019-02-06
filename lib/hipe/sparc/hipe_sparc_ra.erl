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

-module(hipe_sparc_ra).
-export([ra/2]).

ra(CFG0, Options) ->
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearise(CFG0)),
  {CFG1, _FPLiveness1, Coloring_fp, SpillIndex}
    = case proplists:get_bool(inline_fp, Options) of
	true ->
	  FPLiveness0 = hipe_sparc_specific_fp:analyze(CFG0, no_context),
	  hipe_regalloc_loop:ra_fp(CFG0, FPLiveness0, Options,
				   hipe_coalescing_regalloc,
				   hipe_sparc_specific_fp, no_context);
	false ->
	  {CFG0,undefined,[],0}
      end,
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearise(CFG1)),
  GPLiveness1 = hipe_sparc_specific:analyze(CFG1, no_context),
  {CFG2, _GPLiveness2, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(CFG1, GPLiveness1, SpillIndex, Options, hipe_coalescing_regalloc);
	optimistic ->
	  ra(CFG1, GPLiveness1, SpillIndex, Options, hipe_optimistic_regalloc);
	graph_color ->
	  ra(CFG1, GPLiveness1, SpillIndex, Options, hipe_graph_coloring_regalloc);
	linear_scan ->
	  hipe_sparc_ra_ls:ra(CFG1, GPLiveness1, SpillIndex, Options);
	naive ->
	  hipe_sparc_ra_naive:ra(CFG1, GPLiveness1, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearise(CFG2)),
  hipe_sparc_ra_finalise:finalise(CFG2, Coloring, Coloring_fp).

ra(CFG, Liveness, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(CFG, Liveness, SpillIndex, Options, RegAllocMod,
			hipe_sparc_specific, no_context).
