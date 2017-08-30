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

-module(hipe_sparc_ra).
-export([ra/2]).

ra(Defun0, Options) ->
  %% hipe_sparc_pp:pp(Defun0),
  {Defun1, Coloring_fp, SpillIndex}
    = case proplists:get_bool(inline_fp, Options) of
	true ->
	  hipe_regalloc_loop:ra_fp(Defun0, Options,
				   hipe_coalescing_regalloc,
				   hipe_sparc_specific_fp);
	false ->
	  {Defun0,[],0}
      end,
  %% hipe_sparc_pp:pp(Defun1),
  {Defun2, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(Defun1, SpillIndex, Options, hipe_coalescing_regalloc);
	optimistic ->
	  ra(Defun1, SpillIndex, Options, hipe_optimistic_regalloc);
	graph_color ->
	  ra(Defun1, SpillIndex, Options, hipe_graph_coloring_regalloc);
	linear_scan ->
	  hipe_sparc_ra_ls:ra(Defun1, SpillIndex, Options);
	naive ->
	  hipe_sparc_ra_naive:ra(Defun1, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  %% hipe_sparc_pp:pp(Defun2),
  hipe_sparc_ra_finalise:finalise(Defun2, Coloring, Coloring_fp).

ra(Defun, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(Defun, SpillIndex, Options, RegAllocMod, hipe_sparc_specific).
