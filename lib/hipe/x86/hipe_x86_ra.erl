%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

ra(Defun0, Options) ->
  %% ?HIPE_X86_PP:pp(Defun0),
  {Defun1, Coloring_fp, SpillIndex} = ra_fp(Defun0, Options),
  %% ?HIPE_X86_PP:pp(Defun1),
  ?start_ra_instrumentation(Options,
			    length(hipe_x86:defun_code(Defun1)),
			    element(2,hipe_x86:defun_var_range(Defun1))),
  {Defun2, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(Defun1, SpillIndex, Options, hipe_coalescing_regalloc);
	optimistic ->
	  ra(Defun1, SpillIndex, Options, hipe_optimistic_regalloc);
	graph_color ->
	  ra(Defun1, SpillIndex, Options, hipe_graph_coloring_regalloc);
	linear_scan ->
	  ?HIPE_X86_RA_LS:ra(Defun1, SpillIndex, Options);
	naive ->
	  ?HIPE_X86_RA_NAIVE:ra(Defun1, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  ?stop_ra_instrumentation(Options,
			   length(hipe_x86:defun_code(Defun2)),
			   element(2,hipe_x86:defun_var_range(Defun2))),
  %% ?HIPE_X86_PP:pp(Defun2),
  ?HIPE_X86_RA_FINALISE:finalise(Defun2, Coloring, Coloring_fp, Options).

ra(Defun, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(Defun, SpillIndex, Options, RegAllocMod, ?HIPE_X86_SPECIFIC).

-ifdef(HIPE_AMD64).
ra_fp(Defun, Options) ->
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
    coalescing  -> ra_fp(Defun, Options, hipe_coalescing_regalloc, TargetMod);
    optimistic  -> ra_fp(Defun, Options, hipe_optimistic_regalloc, TargetMod);
    graph_color -> ra_fp(Defun, Options, hipe_graph_coloring_regalloc,
			 TargetMod);
    linear_scan -> hipe_amd64_ra_ls:ra_fp(Defun, Options, TargetMod);
    naive -> {Defun,[],0};
    _ ->
      exit({unknown_regalloc_compiler_option,
	    proplists:get_value(regalloc,Options)})
  end.

ra_fp(Defun, Options, RegAllocMod, TargetMod) ->
  hipe_regalloc_loop:ra_fp(Defun, Options, RegAllocMod, TargetMod).
-else.
ra_fp(Defun, Options) ->
  case proplists:get_bool(inline_fp, Options) of
    true ->
      hipe_x86_ra_ls:ra_fp(Defun, Options, hipe_x86_specific_x87);
    false ->
      {Defun,[],0}
  end.
-endif.
