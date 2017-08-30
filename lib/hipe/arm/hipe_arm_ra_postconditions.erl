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

-module(hipe_arm_ra_postconditions).

-export([check_and_rewrite/3, check_and_rewrite2/3]).

-include("hipe_arm.hrl").

check_and_rewrite(CFG, Coloring, Allocator) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_arm_specific, no_context),
  check_and_rewrite2(CFG, TempMap, Allocator).

check_and_rewrite2(CFG, TempMap, Allocator) ->
  Strategy = strategy(Allocator),
  do_bbs(hipe_arm_cfg:labels(CFG), TempMap, Strategy, CFG, false).

strategy(Allocator) ->
  case Allocator of
    'normal' -> 'new';
    'linearscan' -> 'fixed';
    'naive' -> 'fixed'
  end.

do_bbs([], _, _, CFG, DidSpill) -> {CFG, DidSpill};
do_bbs([Lbl|Lbls], TempMap, Strategy, CFG0, DidSpill0) ->
  Code0 = hipe_bb:code(BB = hipe_arm_cfg:bb(CFG0, Lbl)),
  {Code, DidSpill} = do_insns(Code0, TempMap, Strategy, [], DidSpill0),
  CFG = hipe_arm_cfg:bb_add(CFG0, Lbl, hipe_bb:code_update(BB, Code)),
  do_bbs(Lbls, TempMap, Strategy, CFG, DidSpill).

do_insns([I|Insns], TempMap, Strategy, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap, Strategy),
  do_insns(Insns, TempMap, Strategy, lists:reverse(NewIs, Accum), DidSpill0 or DidSpill1);
do_insns([], _TempMap, _Strategy, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap, Strategy) ->
  case I of
    #alu{} -> do_alu(I, TempMap, Strategy);
    #cmp{} -> do_cmp(I, TempMap, Strategy);
    #load{} -> do_load(I, TempMap, Strategy);
    #ldrsb{} -> do_ldrsb(I, TempMap, Strategy);
    #move{} -> do_move(I, TempMap, Strategy);
    #pseudo_call{} -> do_pseudo_call(I, TempMap, Strategy);
    #pseudo_li{} -> do_pseudo_li(I, TempMap, Strategy);
    #pseudo_move{} -> do_pseudo_move(I, TempMap, Strategy);
    #pseudo_spill_move{} -> do_pseudo_spill_move(I, TempMap, Strategy);
    #pseudo_switch{} -> do_pseudo_switch(I, TempMap, Strategy);
    #pseudo_tailcall{} -> do_pseudo_tailcall(I, TempMap, Strategy);
    #smull{} -> do_smull(I, TempMap, Strategy);
    #store{} -> do_store(I, TempMap, Strategy);
    _ -> {[I], false}
  end.

%%% Fix relevant instruction types.

do_alu(I=#alu{dst=Dst,src=Src,am1=Am1}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixSrc,NewSrc,DidSpill2} = fix_src1(Src, TempMap, Strategy),
  {FixAm1,NewAm1,DidSpill3} = fix_am1(Am1, TempMap, Strategy),
  NewI = I#alu{dst=NewDst,src=NewSrc,am1=NewAm1},
  {FixSrc ++ FixAm1 ++ [NewI | FixDst], DidSpill1 or DidSpill2 or DidSpill3}.

do_cmp(I=#cmp{src=Src,am1=Am1}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill1} = fix_src1(Src, TempMap, Strategy),
  {FixAm1,NewAm1,DidSpill2} = fix_am1(Am1, TempMap, Strategy),
  NewI = I#cmp{src=NewSrc,am1=NewAm1},
  {FixSrc ++ FixAm1 ++ [NewI], DidSpill1 or DidSpill2}.

do_load(I=#load{dst=Dst,am2=Am2}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixAm2,NewAm2,DidSpill2} = fix_am2(Am2, TempMap, Strategy),
  NewI = I#load{dst=NewDst,am2=NewAm2},
  {FixAm2 ++ [NewI | FixDst], DidSpill1 or DidSpill2}.

do_ldrsb(I=#ldrsb{dst=Dst,am3=Am3}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixAm3,NewAm3,DidSpill2} = fix_am3(Am3, TempMap, Strategy),
  NewI = I#ldrsb{dst=NewDst,am3=NewAm3},
  {FixAm3 ++ [NewI | FixDst], DidSpill1 or DidSpill2}.

do_move(I=#move{dst=Dst,am1=Am1}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixAm1,NewAm1,DidSpill2} = fix_am1(Am1, TempMap, Strategy),
  NewI = I#move{dst=NewDst,am1=NewAm1},
  {FixAm1 ++ [NewI | FixDst], DidSpill1 or DidSpill2}.

do_pseudo_call(I=#pseudo_call{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_call{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

do_pseudo_li(I=#pseudo_li{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#pseudo_li{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, TempMap, Strategy) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move, pseudo_spill_move, and pseudo_tailcall
  %% are special cases: in all other instructions, all
  %% temps must be non-pseudos after register allocation.
  case temp_is_spilled(Dst, TempMap)
    andalso temp_is_spilled(Dst, TempMap)
  of
    true -> % Turn into pseudo_spill_move
      Temp = clone(Src, temp1(Strategy)),
      NewI = #pseudo_spill_move{dst=Dst, temp=Temp, src=Src},
      {[NewI], true};
    _ ->
      {[I], false}
  end.

do_pseudo_spill_move(I = #pseudo_spill_move{temp=Temp}, TempMap, _Strategy) ->
  %% Temp is above the low water mark and must not have been spilled
  false = temp_is_spilled(Temp, TempMap),
  {[I], false}. % nothing to do

do_pseudo_switch(I=#pseudo_switch{jtab=JTab,index=Index}, TempMap, Strategy) ->
  {FixJTab,NewJTab,DidSpill1} = fix_src1(JTab, TempMap, Strategy),
  {FixIndex,NewIndex,DidSpill2} = fix_src2(Index, TempMap, Strategy),
  NewI = I#pseudo_switch{jtab=NewJTab,index=NewIndex},
  {FixJTab ++ FixIndex ++ [NewI], DidSpill1 or DidSpill2}.

do_pseudo_tailcall(I=#pseudo_tailcall{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_tailcall{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

do_smull(I=#smull{dstlo=DstLo,dsthi=DstHi,src1=Src1,src2=Src2}, TempMap, Strategy) ->
  %% ARM requires that DstLo, DstHi, and Src1 are distinct.
  %% We furthermore require Src1 and Src2 to be different in the fixed strategy.
  {FixDstLo,NewDstLo,DidSpill1} = fix_dst(DstLo, TempMap, Strategy),	% temp1
  {FixDstHi,NewDstHi,DidSpill2} = fix_dst2(DstHi, TempMap, Strategy),	% temp3
  {FixSrc1,NewSrc1,DidSpill3} = fix_src2(Src1, TempMap, Strategy),	% temp2
  {FixSrc2,NewSrc2,DidSpill4} = fix_src1(Src2, TempMap, Strategy),	% temp1; temp3 would be OK
  NewI = I#smull{dstlo=NewDstLo,dsthi=NewDstHi,src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDstLo ++ FixDstHi],
   DidSpill1 or DidSpill2 or DidSpill3 or DidSpill4}.

do_store(I=#store{src=Src,am2=Am2}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill1} = fix_src1(Src, TempMap, Strategy),
  {FixAm2,NewAm2,DidSpill2} = fix_am2(Am2, TempMap, Strategy),
  NewI = I#store{src=NewSrc,am2=NewAm2},
  {FixSrc ++ FixAm2 ++ [NewI], DidSpill1 or DidSpill2}.

%%% Fix Dst and Src operands.

fix_funv(FunV, TempMap, Strategy) ->
  case FunV of
    #arm_temp{} -> fix_src3(FunV, TempMap, Strategy);
    _ -> {[], FunV, false}
  end.

fix_am1(Am1, TempMap, Strategy) ->
  case Am1 of
    #arm_temp{} ->
      fix_src2(Am1, TempMap, Strategy);
    {Src2,rrx} ->
      {Fix,New,DidSpill} = fix_src2(Src2, TempMap, Strategy),
      {Fix, {New,rrx}, DidSpill};
    {Src2,ShiftOp,ShiftArg} ->
      {FixSrc2,NewSrc2,DidSpill1} = fix_src2(Src2, TempMap, Strategy),
      {FixArg,NewArg,DidSpill2} =
	case ShiftArg of
	  #arm_temp{} -> fix_src3(ShiftArg, TempMap, Strategy);
	  _ -> {[], ShiftArg, false}
	end,
      %% order matters: FixArg may clobber temp2/Src2
      {FixArg ++ FixSrc2, {NewSrc2,ShiftOp,NewArg}, DidSpill1 or DidSpill2};
    _ -> {[], Am1, false}
  end.

fix_am2(Am2=#am2{src=Src2,offset=Offset}, TempMap, Strategy) ->
  {FixSrc2,NewSrc2,DidSpill1} = fix_src2(Src2, TempMap, Strategy),
  {FixOffset,NewOffset,DidSpill2} = fix_am2offset(Offset, TempMap, Strategy),
  NewAm2 = Am2#am2{src=NewSrc2,offset=NewOffset},
  %% order matters: FixOffset may clobber temp2/Src2
  {FixOffset ++ FixSrc2, NewAm2, DidSpill1 or DidSpill2}.

fix_am2offset(Offset, TempMap, Strategy) ->
  case Offset of
    #arm_temp{} ->
      fix_src3(Offset, TempMap, Strategy);
    {Src3,rrx} ->
      {Fix,New,DidSpill} = fix_src3(Src3, TempMap, Strategy),
      {Fix, {New,rrx}, DidSpill};
    {Src3,ShiftOp,Imm5} ->
      {Fix,New,DidSpill} = fix_src3(Src3, TempMap, Strategy),
      {Fix, {New,ShiftOp,Imm5}, DidSpill};
    _ ->
      {[], Offset, false}
  end.

fix_am3(Am3=#am3{src=Src2,offset=Offset}, TempMap, Strategy) ->
  {FixSrc2,NewSrc2,DidSpill1} = fix_src2(Src2, TempMap, Strategy),
  {FixOffset,NewOffset,DidSpill2} = fix_am3offset(Offset, TempMap, Strategy),
  NewAm3 = Am3#am3{src=NewSrc2,offset=NewOffset},
  %% order matters: FixOffset may clobber temp2/Src2
  {FixOffset ++ FixSrc2, NewAm3, DidSpill1 or DidSpill2}.

fix_am3offset(Offset, TempMap, Strategy) ->
  case Offset of
    #arm_temp{} -> fix_src3(Offset, TempMap, Strategy);
    _ -> {[], Offset, false}
  end.

fix_src1(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp1(Strategy)).

temp1('new') -> [];
temp1('fixed') -> hipe_arm_registers:temp1().

fix_src2(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp2(Strategy)).

temp2('new') -> [];
temp2('fixed') -> hipe_arm_registers:temp2().

fix_src3(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp3(Strategy)).

temp3('new') -> [];
temp3('fixed') -> hipe_arm_registers:temp3().

fix_src(Src, TempMap, RegOpt) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src, RegOpt),
      {[hipe_arm:mk_pseudo_move(NewSrc, Src)],
       NewSrc,
       true};
    _ ->
      {[], Src, false}
  end.

fix_dst(Dst, TempMap, Strategy) ->
  fix_dst_common(Dst, TempMap, temp1(Strategy)).

fix_dst2(Dst, TempMap, Strategy) -> % only used for smull's DstHi
  fix_dst_common(Dst, TempMap, temp3(Strategy)).

fix_dst_common(Dst, TempMap, RegOpt) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst, RegOpt),
      {[hipe_arm:mk_pseudo_move(Dst, NewDst)], NewDst, true};
    _ ->
      {[], Dst, false}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_spilled(Temp, []) -> % special case for naive regalloc
  not(hipe_arm:temp_is_precoloured(Temp));
temp_is_spilled(Temp, TempMap) ->
  case hipe_arm:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_arm:temp_reg(Temp),
      tuple_size(TempMap) > Reg andalso hipe_temp_map:is_spilled(Reg, TempMap);
    false -> true
  end.

%%% Make a certain reg into a clone of Temp.

clone(Temp, RegOpt) ->
  Type = hipe_arm:temp_type(Temp),
  case RegOpt of
    [] -> hipe_arm:mk_new_temp(Type);
    Reg -> hipe_arm:mk_temp(Reg, Type)
  end.
