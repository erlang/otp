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
-define(HIPE_X86_RA, hipe_amd64_ra).
-define(HIPE_X86_PP, hipe_amd64_pp).
-define(HIPE_X86_RA_LS, hipe_amd64_ra_ls).
-define(HIPE_X86_RA_NAIVE, hipe_amd64_ra_naive).
-define(HIPE_X86_RA_FINALISE, hipe_amd64_ra_finalise).
-define(HIPE_X86_SPECIFIC, hipe_amd64_specific).
-else.
-define(HIPE_X86_RA, hipe_x86_ra).
-define(HIPE_X86_PP, hipe_x86_pp).
-define(HIPE_X86_RA_LS, hipe_x86_ra_ls).
-define(HIPE_X86_RA_NAIVE, hipe_x86_ra_naive).
-define(HIPE_X86_RA_FINALISE, hipe_x86_ra_finalise).
-define(HIPE_X86_SPECIFIC, hipe_x86_specific).
-endif.

-module(?HIPE_X86_RA).
-export([ra/2]).

%%-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

-ifdef(HIPE_INSTRUMENT_COMPILER).
code_size(CFG) ->
  hipe_x86_cfg:fold_bbs(fun(_, BB, Acc) -> Acc + length(hipe_bb:code(BB)) end,
			0, CFG).
-endif. %% ifdef(HIPE_INSTRUMENT_COMPILER)

ra(CFG0, Options) ->
  %% hipe_x86_cfg:pp(CFG0),
  Liveness0 = ?HIPE_X86_SPECIFIC:analyze(CFG0, no_context),
  {CFG1, Liveness, Coloring_fp, SpillIndex} = ra_fp(CFG0, Liveness0, Options),
  %% hipe_x86_cfg:pp(CFG1),
  ?start_ra_instrumentation(Options,
			    code_size(CFG1),
			    element(2,hipe_gensym:var_range(x86))),
  {CFG2, _, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(CFG1, Liveness, SpillIndex, Options, hipe_coalescing_regalloc);
	optimistic ->
	  ra(CFG1, Liveness, SpillIndex, Options, hipe_optimistic_regalloc);
	graph_color ->
	  ra(CFG1, Liveness, SpillIndex, Options, hipe_graph_coloring_regalloc);
	linear_scan ->
	  ?HIPE_X86_RA_LS:ra(CFG1, Liveness, SpillIndex, Options);
	naive ->
	  ?HIPE_X86_RA_NAIVE:ra(CFG1, Liveness, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  ?stop_ra_instrumentation(Options,
			   code_size(CFG2),
  			   element(2,hipe_gensym:var_range(x86))),
  %% hipe_x86_cfg:pp(CFG2),
  ?HIPE_X86_RA_FINALISE:finalise(CFG2, Coloring, Coloring_fp, Options).

ra(CFG, Liveness, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(CFG, Liveness, SpillIndex, Options, RegAllocMod,
			?HIPE_X86_SPECIFIC, no_context).

-ifdef(HIPE_AMD64).
ra_fp(CFG, Liveness, Options) ->
  Regalloc0 = proplists:get_value(regalloc, Options),
  {Regalloc, TargetMod} =
    case proplists:get_bool(inline_fp, Options) and (Regalloc0 =/= naive) of
      false -> {naive, undefined};
      true ->
	case proplists:get_bool(x87, Options) of
	  true ->  {linear_scan, hipe_amd64_specific_x87};
	  false -> {Regalloc0,   hipe_amd64_specific_sse2}
	end
    end,
  case Regalloc of
    coalescing ->
      ra_fp(CFG, Liveness, Options, hipe_coalescing_regalloc, TargetMod);
    optimistic ->
      ra_fp(CFG, Liveness, Options, hipe_optimistic_regalloc, TargetMod);
    graph_color ->
      ra_fp(CFG, Liveness, Options, hipe_graph_coloring_regalloc, TargetMod);
    linear_scan -> hipe_amd64_ra_ls:ra_fp(CFG, Liveness, Options, TargetMod,
					  no_context);
    naive -> {CFG,Liveness,[],0};
    _ ->
      exit({unknown_regalloc_compiler_option,
	    proplists:get_value(regalloc,Options)})
  end.

ra_fp(CFG, Liveness, Options, RegAllocMod, TargetMod) ->
  hipe_regalloc_loop:ra_fp(CFG, Liveness, Options, RegAllocMod, TargetMod,
			   no_context).
-else.
ra_fp(CFG, Liveness, Options) ->
  case proplists:get_bool(inline_fp, Options) of
    true ->
      hipe_x86_ra_ls:ra_fp(CFG, Liveness, Options, hipe_x86_specific_x87,
			   no_context);
    false ->
      {CFG,Liveness,[],0}
  end.
-endif.
