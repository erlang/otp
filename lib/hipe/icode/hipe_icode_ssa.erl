%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : hipe_icode_ssa.erl
%% Author  : 
%% Created : 
%% Purpose : Provides interface functions for converting Icode into
%%	     SSA form and back using the generic SSA converter.
%%----------------------------------------------------------------------

-module(hipe_icode_ssa).

%% The following defines are needed by the included file below
-define(CODE, hipe_icode).
-define(CFG,  hipe_icode_cfg).
-define(LIVENESS, hipe_icode_liveness).
-define(LIVENESS_NEEDED, true).

-include("hipe_icode.hrl").
-include("../ssa/hipe_ssa.inc").

%% Declarations for exported functions which are Icode-specific.
-spec ssa_liveness__analyze(#cfg{}) -> gb_tree().
-spec ssa_liveness__livein(_, icode_lbl()) -> [#icode_variable{}].
%% -spec ssa_liveness__livein(_, icode_lbl(), _) -> [#icode_var{}].

%%----------------------------------------------------------------------
%% Auxiliary operations which seriously differ between Icode and RTL.
%%----------------------------------------------------------------------

defs_to_rename(Statement) ->
  hipe_icode:defines(Statement).

uses_to_rename(Statement) ->
  hipe_icode:uses(Statement).

liveout_no_succ() ->
  [].

%%----------------------------------------------------------------------

reset_var_indx() ->
  hipe_gensym:set_var(icode, 0).

%%----------------------------------------------------------------------

is_fp_temp(Temp) ->
  hipe_icode:is_fvar(Temp).

mk_new_fp_temp() ->
  hipe_icode:mk_new_fvar().

%%----------------------------------------------------------------------
%% Procedure : makePhiMove 
%% Purpose   : Create an ICode-specific version of a move instruction
%%             depending on the type of the arguments.
%% Arguments : Dst, Src - the arguments of a Phi instruction that is
%%                        to be moved up the predecessor block as part
%%                        of the SSA unconvert phase.
%% Returns   : Code
%%----------------------------------------------------------------------

makePhiMove(Dst, Src) ->
  case hipe_icode:is_fvar(Dst) of
    false ->
      case hipe_icode:is_fvar(Src) of
	false ->
	  hipe_icode:mk_move(Dst, Src);
	true ->
	  hipe_icode:mk_primop([Dst], unsafe_tag_float, [Src])
      end;
    true ->
      case hipe_icode:is_fvar(Src) of
	true ->
	  hipe_icode:mk_move(Dst, Src);
	false ->
	  hipe_icode:mk_primop([Dst], conv_to_float, [Src])
      end
  end.

%%----------------------------------------------------------------------
