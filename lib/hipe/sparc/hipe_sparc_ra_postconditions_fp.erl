%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(hipe_sparc_ra_postconditions_fp).

-export([check_and_rewrite/2]).

-include("hipe_sparc.hrl").

check_and_rewrite(Defun, Coloring) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_sparc_specific_fp),
  #defun{code=Code0} = Defun,
  {Code1,DidSpill} = do_insns(Code0, TempMap, [], false),
  VarRange = {0, hipe_gensym:get_var(sparc)},
  {Defun#defun{code=Code1, var_range=VarRange},
   DidSpill}.

do_insns([I|Insns], TempMap, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap),
  do_insns(Insns, TempMap, lists:reverse(NewIs, Accum), DidSpill0 or DidSpill1);
do_insns([], _TempMap, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap) ->
  case I of
    #fp_binary{} -> do_fp_binary(I, TempMap);
    #fp_unary{} -> do_fp_unary(I, TempMap);
    #pseudo_fload{} -> do_pseudo_fload(I, TempMap);
    #pseudo_fmove{} -> do_pseudo_fmove(I, TempMap);
    #pseudo_fstore{} -> do_pseudo_fstore(I, TempMap);
    _ -> {[I], false}
  end.

%%% Fix relevant instruction types.

do_fp_binary(I=#fp_binary{src1=Src1,src2=Src2,dst=Dst}, TempMap) ->
  {FixSrc1,NewSrc1,DidSpill1} = fix_src(Src1, TempMap),
  {FixSrc2,NewSrc2,DidSpill2} = fix_src(Src2, TempMap),
  {FixDst,NewDst,DidSpill3} = fix_dst(Dst, TempMap),
  NewI = I#fp_binary{src1=NewSrc1,src2=NewSrc2,dst=NewDst},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDst], DidSpill1 or DidSpill2 or DidSpill3}.

do_fp_unary(I=#fp_unary{src=Src,dst=Dst}, TempMap) ->
  {FixSrc,NewSrc,DidSpill1} = fix_src(Src, TempMap),
  {FixDst,NewDst,DidSpill2} = fix_dst(Dst, TempMap),
  NewI = I#fp_unary{src=NewSrc,dst=NewDst},
  {FixSrc ++ [NewI | FixDst], DidSpill1 or DidSpill2}.

do_pseudo_fload(I=#pseudo_fload{dst=Dst}, TempMap) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap),
  NewI = I#pseudo_fload{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_pseudo_fmove(I=#pseudo_fmove{src=Src,dst=Dst}, TempMap) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      {FixSrc,NewSrc,DidSpill} = fix_src(Src, TempMap),
      NewI = I#pseudo_fmove{src=NewSrc},
      {FixSrc ++ [NewI], DidSpill};
    _ ->
      {[I], false}
  end.

do_pseudo_fstore(I=#pseudo_fstore{src=Src}, TempMap) ->
  {FixSrc,NewSrc,DidSpill} = fix_src(Src, TempMap),
  NewI = I#pseudo_fstore{src=NewSrc},
  {FixSrc ++ [NewI], DidSpill}.

%%% Fix Dst and Src operands.

fix_src(Src, TempMap) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src),
      {[hipe_sparc:mk_pseudo_fmove(Src, NewSrc)], NewSrc, true};
    _ ->
      {[], Src, false}
  end.

fix_dst(Dst, TempMap) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst),
      {[hipe_sparc:mk_pseudo_fmove(NewDst, Dst)], NewDst, true};
    _ ->
      {[], Dst, false}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_spilled(Temp, TempMap) ->
  case hipe_sparc:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_sparc:temp_reg(Temp),
      tuple_size(TempMap) > Reg andalso hipe_temp_map:is_spilled(Reg, TempMap);
    false -> true
  end.

%%% Create a new temp with the same type as an old one.

clone(Temp) ->
  Type = hipe_sparc:temp_type(Temp),	% XXX: always double?
  hipe_sparc:mk_new_temp(Type).
