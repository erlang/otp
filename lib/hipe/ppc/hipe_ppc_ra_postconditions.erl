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

-module(hipe_ppc_ra_postconditions).

-export([check_and_rewrite/3, check_and_rewrite2/3]).

-include("hipe_ppc.hrl").

check_and_rewrite(Defun, Coloring, Allocator) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_ppc_specific),
  check_and_rewrite2(Defun, TempMap, Allocator).

check_and_rewrite2(Defun, TempMap, Allocator) ->
  Strategy = strategy(Allocator),
  #defun{code=Code0} = Defun,
  {Code1,DidSpill} = do_insns(Code0, TempMap, Strategy, [], false),
  VarRange = {0, hipe_gensym:get_var(ppc)},
  {Defun#defun{code=Code1, var_range=VarRange},
   DidSpill}.

strategy(Allocator) ->
  case Allocator of
    'normal' -> 'new';
    'linearscan' -> 'fixed';
    'naive' -> 'fixed'
  end.

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
    #loadx{} -> do_loadx(I, TempMap, Strategy);
    #mfspr{} -> do_mfspr(I, TempMap, Strategy);
    #mtcr{} -> do_mtcr(I, TempMap, Strategy);
    #mtspr{} -> do_mtspr(I, TempMap, Strategy);
    #pseudo_li{} -> do_pseudo_li(I, TempMap, Strategy);
    #pseudo_move{} -> do_pseudo_move(I, TempMap, Strategy);
    #store{} -> do_store(I, TempMap, Strategy);
    #storex{} -> do_storex(I, TempMap, Strategy);
    #unary{} -> do_unary(I, TempMap, Strategy);
    #lfd{} -> do_lfd(I, TempMap, Strategy);
    #lfdx{} -> do_lfdx(I, TempMap, Strategy);
    #stfd{} -> do_stfd(I, TempMap, Strategy);
    #stfdx{} -> do_stfdx(I, TempMap, Strategy);
    _ -> {[I], false}
  end.

%%% Fix relevant instruction types.

do_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixSrc1,NewSrc1,DidSpill2} = fix_src1(Src1, TempMap, Strategy),
  {FixSrc2,NewSrc2,DidSpill3} = fix_src2_or_imm(Src2, TempMap, Strategy),
  NewI = I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDst], DidSpill1 or DidSpill2 or DidSpill3}.

do_cmp(I=#cmp{src1=Src1,src2=Src2}, TempMap, Strategy) ->
  {FixSrc1,NewSrc1,DidSpill1} = fix_src1(Src1, TempMap, Strategy),
  {FixSrc2,NewSrc2,DidSpill2} = fix_src2_or_imm(Src2, TempMap, Strategy),
  NewI = I#cmp{src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI], DidSpill1 or DidSpill2}.

do_load(I=#load{dst=Dst,base=Base}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixBase,NewBase,DidSpill2} = fix_src1(Base, TempMap, Strategy),
  NewI = I#load{dst=NewDst,base=NewBase},
  {FixBase ++ [NewI | FixDst], DidSpill1 or DidSpill2}.

do_loadx(I=#loadx{dst=Dst,base1=Base1,base2=Base2}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixBase1,NewBase1,DidSpill2} = fix_src1(Base1, TempMap, Strategy),
  {FixBase2,NewBase2,DidSpill3} = fix_src2(Base2, TempMap, Strategy),
  NewI = I#loadx{dst=NewDst,base1=NewBase1,base2=NewBase2},
  {FixBase1 ++ FixBase2 ++ [NewI | FixDst], DidSpill1 or DidSpill2 or DidSpill3}.

do_mfspr(I=#mfspr{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#mfspr{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_mtcr(I=#mtcr{src=Src}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill} = fix_src1(Src, TempMap, Strategy),
  NewI = I#mtcr{src=NewSrc},
  {FixSrc ++ [NewI], DidSpill}.

do_mtspr(I=#mtspr{src=Src}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill} = fix_src1(Src, TempMap, Strategy),
  NewI = I#mtspr{src=NewSrc},
  {FixSrc ++ [NewI], DidSpill}.

do_pseudo_li(I=#pseudo_li{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#pseudo_li{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, TempMap, Strategy) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move and pseudo_tailcall are special cases: in
  %% all other instructions, all temps must be non-pseudos
  %% after register allocation.
  case temp_is_spilled(Dst, TempMap) of
    true -> % Src must not be a pseudo
      {FixSrc,NewSrc,DidSpill} = fix_src1(Src, TempMap, Strategy),
      NewI = I#pseudo_move{src=NewSrc},
      {FixSrc ++ [NewI], DidSpill};
    _ ->
      {[I], false}
  end.

do_store(I=#store{src=Src,base=Base}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill1} = fix_src1(Src, TempMap, Strategy),
  {FixBase,NewBase,DidSpill2} = fix_src2(Base, TempMap, Strategy),
  NewI = I#store{src=NewSrc,base=NewBase},
  {FixSrc ++ FixBase ++ [NewI], DidSpill1 or DidSpill2}.

do_storex(I=#storex{src=Src,base1=Base1,base2=Base2}, TempMap, Strategy) ->
  {FixSrc,NewSrc,DidSpill1} = fix_src1(Src, TempMap, Strategy),
  {FixBase1,NewBase1,DidSpill2} = fix_src2(Base1, TempMap, Strategy),
  {FixBase2,NewBase2,DidSpill3} = fix_src3(Base2, TempMap, Strategy),
  NewI = I#storex{src=NewSrc,base1=NewBase1,base2=NewBase2},
  {FixSrc ++ FixBase1 ++ FixBase2 ++ [NewI], DidSpill1 or DidSpill2 or DidSpill3}.

do_unary(I=#unary{dst=Dst,src=Src}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixSrc,NewSrc,DidSpill2} = fix_src1(Src, TempMap, Strategy),
  NewI = I#unary{dst=NewDst,src=NewSrc},
  {FixSrc ++ [NewI | FixDst], DidSpill1 or DidSpill2}.

do_lfd(I=#lfd{base=Base}, TempMap, Strategy) ->
  {FixBase,NewBase,DidSpill} = fix_src1(Base, TempMap, Strategy),
  NewI = I#lfd{base=NewBase},
  {FixBase ++ [NewI], DidSpill}.

do_lfdx(I=#lfdx{base1=Base1,base2=Base2}, TempMap, Strategy) ->
  {FixBase1,NewBase1,DidSpill1} = fix_src1(Base1, TempMap, Strategy),
  {FixBase2,NewBase2,DidSpill2} = fix_src2(Base2, TempMap, Strategy),
  NewI = I#lfdx{base1=NewBase1,base2=NewBase2},
  {FixBase1 ++ FixBase2 ++ [NewI], DidSpill1 or DidSpill2}.

do_stfd(I=#stfd{base=Base}, TempMap, Strategy) ->
  {FixBase,NewBase,DidSpill} = fix_src1(Base, TempMap, Strategy),
  NewI = I#stfd{base=NewBase},
  {FixBase ++ [NewI], DidSpill}.

do_stfdx(I=#stfdx{base1=Base1,base2=Base2}, TempMap, Strategy) ->
  {FixBase1,NewBase1,DidSpill1} = fix_src1(Base1, TempMap, Strategy),
  {FixBase2,NewBase2,DidSpill2} = fix_src2(Base2, TempMap, Strategy),
  NewI = I#stfdx{base1=NewBase1,base2=NewBase2},
  {FixBase1 ++ FixBase2 ++ [NewI], DidSpill1 or DidSpill2}.

%%% Fix Dst and Src operands.

fix_src2_or_imm(Src2, TempMap, Strategy) ->
  case Src2 of
    #ppc_temp{} -> fix_src2(Src2, TempMap, Strategy);
    _ -> {[], Src2, false}
  end.

fix_src1(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp1(Strategy)).

temp1('new') -> [];
temp1('fixed') -> hipe_ppc_registers:temp1().

fix_src2(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp2(Strategy)).

temp2('new') -> [];
temp2('fixed') -> hipe_ppc_registers:temp2().

fix_src3(Src, TempMap, Strategy) -> % storex :-(
  fix_src(Src, TempMap, temp3(Strategy)).

temp3('new') -> [];
temp3('fixed') -> hipe_ppc_registers:temp3().

fix_src(Src, TempMap, RegOpt) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src, RegOpt),
      {[hipe_ppc:mk_pseudo_move(NewSrc, Src)],
       NewSrc,
       true};
    _ ->
      {[], Src, false}
  end.

fix_dst(Dst, TempMap, Strategy) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst, temp3(Strategy)),
      {[hipe_ppc:mk_pseudo_move(Dst, NewDst)],
       NewDst,
       true};
    _ ->
      {[], Dst, false}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_spilled(Temp, []) -> % special case for naive regalloc
  not(hipe_ppc:temp_is_precoloured(Temp));
temp_is_spilled(Temp, TempMap) ->
  case hipe_ppc:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_ppc:temp_reg(Temp),
      tuple_size(TempMap) > Reg andalso hipe_temp_map:is_spilled(Reg, TempMap);
    false -> true
  end.

%%% Make a certain reg into a clone of Temp.

clone(Temp, RegOpt) ->
  Type = hipe_ppc:temp_type(Temp),
  case RegOpt of
    [] -> hipe_ppc:mk_new_temp(Type);
    Reg -> hipe_ppc:mk_temp(Reg, Type)
  end.
