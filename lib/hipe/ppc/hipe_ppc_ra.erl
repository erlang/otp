%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(hipe_ppc_ra).
-export([ra/2]).

ra(Defun0, Options) ->
  %% hipe_ppc_pp:pp(Defun0),
  {Defun1, Coloring_fp, SpillIndex}
    = case proplists:get_bool(inline_fp, Options) of
	true ->
	  hipe_regalloc_loop:ra_fp(Defun0, Options,
				   hipe_coalescing_regalloc,
				   hipe_ppc_specific_fp);
	false ->
	  {Defun0,[],0}
      end,
  %% hipe_ppc_pp:pp(Defun1),
  {Defun2, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(Defun1, SpillIndex, Options, hipe_coalescing_regalloc);
	optimistic ->
	  ra(Defun1, SpillIndex, Options, hipe_optimistic_regalloc);
	graph_color ->
	  ra(Defun1, SpillIndex, Options, hipe_graph_coloring_regalloc);
	linear_scan ->
	  hipe_ppc_ra_ls:ra(Defun1, SpillIndex, Options);
	naive ->
	  hipe_ppc_ra_naive:ra(Defun1, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  %% hipe_ppc_pp:pp(Defun2),
  hipe_ppc_ra_finalise:finalise(Defun2, Coloring, Coloring_fp).

ra(Defun, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(Defun, SpillIndex, Options, RegAllocMod, hipe_ppc_specific).
