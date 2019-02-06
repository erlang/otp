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
%%
%%----------------------------------------------------------------------
%% File    : hipe_rtl_ssa.erl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Created : 30 Jan 2004
%% Purpose : Provides interface functions for converting RTL code into
%%	     SSA form and back using the generic SSA converter.
%%----------------------------------------------------------------------

-module(hipe_rtl_ssa).

-export([uses_to_rename/1]).	%% needed by hipe_rtl_ssa_const_prop

%% The following defines are needed by the included file below
-define(CODE, hipe_rtl).
-define(CFG,  hipe_rtl_cfg).
-define(LIVENESS, hipe_rtl_liveness).

-include("hipe_rtl.hrl").
-include("../ssa/hipe_ssa.inc").

%%----------------------------------------------------------------------
%% Auxiliary operations which seriously differ between Icode and RTL.
%%----------------------------------------------------------------------

defs_to_rename(Statement) ->
  Defs = hipe_rtl:defines(Statement),
  [D || D <- Defs, not hipe_rtl_arch:is_precoloured(D)].

uses_to_rename(Statement) ->
  Uses = hipe_rtl:uses(Statement),
  [U || U <- Uses, not hipe_rtl_arch:is_precoloured(U)].

liveout_no_succ() ->
  hipe_rtl_arch:live_at_return().

%-----------------------------------------------------------------------

reset_var_indx() ->
  hipe_gensym:set_var(rtl, hipe_rtl_arch:first_virtual_reg()).

%%----------------------------------------------------------------------

is_fp_temp(Temp) ->
  hipe_rtl:is_fpreg(Temp).

mk_new_fp_temp() ->
  hipe_rtl:mk_new_fpreg().

%-----------------------------------------------------------------------
%% Procedure : makePhiMove 
%% Purpose   : Create an RTL-specific version of a move instruction
%%             depending on the type of the arguments.
%% Arguments : Dst, Src - the arguments of a Phi instruction that is
%%                        to be moved up the predecessor block as part
%%                        of the SSA un-convert phase.
%% Returns   : Code
%% Note      : ?CODE here is hipe_rtl
%%----------------------------------------------------------------------

makePhiMove(Dst, Src) ->
  case hipe_rtl:is_fpreg(Dst) of
    false ->
      case hipe_rtl:is_fpreg(Src) of %% this test is just a sanity check
	false ->
	  hipe_rtl:mk_move(Dst, Src)
      end;
    true ->
      case hipe_rtl:is_fpreg(Src) of %% this test is just a sanity check
	true ->
	  hipe_rtl:mk_fmove(Dst, Src)
      end
  end.

%-----------------------------------------------------------------------
