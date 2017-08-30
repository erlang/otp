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

-module(hipe_sparc_ra_postconditions).

-export([check_and_rewrite/3, check_and_rewrite2/3]).

-include("hipe_sparc.hrl").

check_and_rewrite(CFG, Coloring, Allocator) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_sparc_specific, no_context),
  check_and_rewrite2(CFG, TempMap, Allocator).

check_and_rewrite2(CFG, TempMap, Allocator) ->
  Strategy = strategy(Allocator),
  do_bbs(hipe_sparc_cfg:labels(CFG), TempMap, Strategy, CFG, false).

strategy(Allocator) ->
  case Allocator of
    'normal' -> 'new';
    'linearscan' -> 'fixed';
    'naive' -> 'fixed'
  end.

do_bbs([], _, _, CFG, DidSpill) -> {CFG, DidSpill};
do_bbs([Lbl|Lbls], TempMap, Strategy, CFG0, DidSpill0) ->
  Code0 = hipe_bb:code(BB = hipe_sparc_cfg:bb(CFG0, Lbl)),
  {Code, DidSpill} = do_insns(Code0, TempMap, Strategy, [], DidSpill0),
  CFG = hipe_sparc_cfg:bb_add(CFG0, Lbl, hipe_bb:code_update(BB, Code)),
  do_bbs(Lbls, TempMap, Strategy, CFG, DidSpill).

do_insns([I|Insns], TempMap, Strategy, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap, Strategy),
  do_insns(Insns, TempMap, Strategy, lists:reverse(NewIs, Accum), DidSpill0 or DidSpill1);
do_insns([], _TempMap, _Strategy, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap, Strategy) ->
  case I of
    #alu{} -> do_alu(I, TempMap, Strategy);
    #jmp{} -> do_jmp(I, TempMap, Strategy);
    %% #pseudo_br{} -> do_pseudo_br(I, TempMap, Strategy);
    #pseudo_call{} -> do_pseudo_call(I, TempMap, Strategy);
    #pseudo_move{} -> do_pseudo_move(I, TempMap, Strategy);
    #pseudo_set{} -> do_pseudo_set(I, TempMap, Strategy);
    #pseudo_spill_move{} -> do_pseudo_spill_move(I, TempMap, Strategy);
    #pseudo_tailcall{} -> do_pseudo_tailcall(I, TempMap, Strategy);
    #rdy{} -> do_rdy(I, TempMap, Strategy);
    #sethi{} -> do_sethi(I, TempMap, Strategy);
    #store{} -> do_store(I, TempMap, Strategy);
    #pseudo_fload{} -> do_pseudo_fload(I, TempMap, Strategy);
    #pseudo_fstore{} -> do_pseudo_fstore(I, TempMap, Strategy);
    _ -> {[I], false}
  end.

%%% Fix relevant instruction types.

do_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixSrc1,NewSrc1,DidSpill2} = fix_src1(Src1, TempMap, Strategy),
  {FixSrc2,NewSrc2,DidSpill3} = fix_src2_or_imm(Src2, TempMap, Strategy),
  NewI = I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDst], DidSpill1 or DidSpill2 or DidSpill3}.

do_jmp(I=#jmp{src1=Src1,src2=Src2}, TempMap, Strategy) ->
  {FixSrc1,NewSrc1,DidSpill1} = fix_src1(Src1, TempMap, Strategy),
  {FixSrc2,NewSrc2,DidSpill2} = fix_src2_or_imm(Src2, TempMap, Strategy),
  NewI = I#jmp{src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI], DidSpill1 or DidSpill2}.

-ifdef(notdef).	% XXX: only for sparc64, alas
do_pseudo_br(I=#pseudo_br{src=Src}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill} = fix_src1(Src, TempMap, Strategy),
  NewI = I#pseudo_br{src=NewSrc},
  {FixSrc ++ [NewI], DidSpill}.
-endif.

do_pseudo_call(I=#pseudo_call{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_call{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

do_pseudo_move(I=#pseudo_move{src=Src,dst=Dst}, TempMap, Strategy) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move and pseudo_spill_move [XXX: not pseudo_tailcall]
  %% are special cases: in all other instructions, all temps must
  %% be non-pseudos after register allocation.
  case temp_is_spilled(Src, TempMap)
    andalso temp_is_spilled(Dst, TempMap)
  of
    true -> % Turn into pseudo_spill_move
      Temp = clone(Src, temp1(Strategy)),
      NewI = #pseudo_spill_move{src=Src,temp=Temp,dst=Dst},
      {[NewI], true};
    _ ->
      {[I], false}
  end.

do_pseudo_set(I=#pseudo_set{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#pseudo_set{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_pseudo_spill_move(I=#pseudo_spill_move{temp=Temp}, TempMap, _Strategy) ->
  %% Temp is above the low water mark and must not have been spilled
  false = temp_is_spilled(Temp, TempMap),
  {[I], false}.

do_pseudo_tailcall(I=#pseudo_tailcall{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_tailcall{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

do_rdy(I=#rdy{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#rdy{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_sethi(I=#sethi{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#sethi{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_store(I=#store{src=Src,base=Base,disp=Disp}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill1} = fix_src1(Src, TempMap, Strategy),
  {FixBase,NewBase,DidSpill2} = fix_src2(Base, TempMap, Strategy),
  {FixDisp,NewDisp,DidSpill3} = fix_src3_or_imm(Disp, TempMap, Strategy),
  NewI = I#store{src=NewSrc,base=NewBase,disp=NewDisp},
  {FixSrc ++ FixBase ++ FixDisp ++ [NewI], DidSpill1 or DidSpill2 or DidSpill3}.

do_pseudo_fload(I=#pseudo_fload{base=Base}, TempMap, Strategy) ->
  {FixBase,NewBase,DidSpill} = fix_src1(Base, TempMap, Strategy),
  NewI = I#pseudo_fload{base=NewBase},
  {FixBase ++ [NewI], DidSpill}.

do_pseudo_fstore(I=#pseudo_fstore{base=Base}, TempMap, Strategy) ->
  {FixBase,NewBase,DidSpill} = fix_src1(Base, TempMap, Strategy),
  NewI = I#pseudo_fstore{base=NewBase},
  {FixBase ++ [NewI], DidSpill}.

%%% Fix Dst and Src operands.

fix_funv(FunV, TempMap, Strategy) ->
  case FunV of
    #sparc_temp{} -> fix_src3(FunV, TempMap, Strategy);
    _ -> {[], FunV, false}
  end.

fix_src2_or_imm(Src2, TempMap, Strategy) ->
  case Src2 of
    #sparc_temp{} -> fix_src2(Src2, TempMap, Strategy);
    _ -> {[], Src2, false}
  end.

fix_src3_or_imm(Src3, TempMap, Strategy) ->
  case Src3 of
    #sparc_temp{} -> fix_src3(Src3, TempMap, Strategy);
    _ -> {[], Src3, false}
  end.

fix_src1(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp1(Strategy)).

temp1('new') -> [];
temp1('fixed') -> hipe_sparc_registers:temp1().

fix_src2(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp2(Strategy)).

temp2('new') -> [];
temp2('fixed') -> hipe_sparc_registers:temp2().

fix_src3(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp3(Strategy)).

temp3('new') -> [];
temp3('fixed') -> hipe_sparc_registers:temp3().

fix_src(Src, TempMap, RegOpt) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src, RegOpt),
      {[hipe_sparc:mk_pseudo_move(Src, NewSrc)], NewSrc, true};
    _ ->
      {[], Src, false}
  end.

fix_dst(Dst, TempMap, Strategy) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst, temp1(Strategy)),
      {[hipe_sparc:mk_pseudo_move(NewDst, Dst)], NewDst, true};
    _ ->
      {[], Dst, false}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_spilled(Temp, []) -> % special case for naive regalloc
  not(hipe_sparc:temp_is_precoloured(Temp));
temp_is_spilled(Temp, TempMap) ->
  case hipe_sparc:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_sparc:temp_reg(Temp),
      tuple_size(TempMap) > Reg andalso hipe_temp_map:is_spilled(Reg, TempMap);
    false -> true
  end.

%%% Make a certain reg into a clone of Temp.

clone(Temp, RegOpt) ->
  Type = hipe_sparc:temp_type(Temp),
  case RegOpt of
    [] -> hipe_sparc:mk_new_temp(Type);
    Reg -> hipe_sparc:mk_temp(Reg, Type)
  end.
