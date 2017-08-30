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

-module(hipe_ppc_ra_finalise).
-export([finalise/3]).
-include("hipe_ppc.hrl").

finalise(Defun, TempMap, FPMap0) ->
  Code = hipe_ppc:defun_code(Defun),
  {_, SpillLimit} = hipe_ppc:defun_var_range(Defun),
  Map = mk_ra_map(TempMap, SpillLimit),
  FPMap1 = mk_ra_map_fp(FPMap0, SpillLimit),
  NewCode = ra_code(Code, Map, FPMap1, []),
  Defun#defun{code=NewCode}.

ra_code([I|Insns], Map, FPMap, Accum) ->
  ra_code(Insns, Map, FPMap, [ra_insn(I, Map, FPMap) | Accum]);
ra_code([], _Map, _FPMap, Accum) ->
  lists:reverse(Accum).

ra_insn(I, Map, FPMap) ->
  case I of
    #alu{} -> ra_alu(I, Map);
    #cmp{} -> ra_cmp(I, Map);
    #load{} -> ra_load(I, Map);
    #loadx{} -> ra_loadx(I, Map);
    #mfspr{} -> ra_mfspr(I, Map);
    #mtcr{} -> ra_mtcr(I, Map);
    #mtspr{} -> ra_mtspr(I, Map);
    #pseudo_li{} -> ra_pseudo_li(I, Map);
    #pseudo_move{} -> ra_pseudo_move(I, Map);
    #pseudo_tailcall{} -> ra_pseudo_tailcall(I, Map);
    #store{} -> ra_store(I, Map);
    #storex{} -> ra_storex(I, Map);
    #unary{} -> ra_unary(I, Map);
    #lfd{} -> ra_lfd(I, Map, FPMap);
    #lfdx{} -> ra_lfdx(I, Map, FPMap);
    #stfd{} -> ra_stfd(I, Map, FPMap);
    #stfdx{} -> ra_stfdx(I, Map, FPMap);
    #fp_binary{} -> ra_fp_binary(I, FPMap);
    #fp_unary{} -> ra_fp_unary(I, FPMap);
    #pseudo_fmove{} -> ra_pseudo_fmove(I, FPMap);
    _ -> I
  end.

ra_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc1 = ra_temp(Src1, Map),
  NewSrc2 = ra_temp_or_imm(Src2, Map),
  I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2}.

ra_cmp(I=#cmp{src1=Src1,src2=Src2}, Map) ->
  NewSrc1 = ra_temp(Src1, Map),
  NewSrc2 = ra_temp_or_imm(Src2, Map),
  I#cmp{src1=NewSrc1,src2=NewSrc2}.

ra_load(I=#load{dst=Dst,base=Base}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewBase = ra_temp(Base, Map),
  I#load{dst=NewDst,base=NewBase}.

ra_loadx(I=#loadx{dst=Dst,base1=Base1,base2=Base2}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewBase1 = ra_temp(Base1, Map),
  NewBase2 = ra_temp(Base2, Map),
  I#loadx{dst=NewDst,base1=NewBase1,base2=NewBase2}.

ra_mfspr(I=#mfspr{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#mfspr{dst=NewDst}.

ra_mtcr(I=#mtcr{src=Src}, Map) ->
  NewSrc = ra_temp(Src, Map),
  I#mtcr{src=NewSrc}.

ra_mtspr(I=#mtspr{src=Src}, Map) ->
  NewSrc = ra_temp(Src, Map),
  I#mtspr{src=NewSrc}.

ra_pseudo_li(I=#pseudo_li{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#pseudo_li{dst=NewDst}.

ra_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc = ra_temp(Src, Map),
  I#pseudo_move{dst=NewDst,src=NewSrc}.

ra_pseudo_tailcall(I=#pseudo_tailcall{stkargs=StkArgs}, Map) ->
  NewStkArgs = ra_args(StkArgs, Map),
  I#pseudo_tailcall{stkargs=NewStkArgs}.

ra_store(I=#store{src=Src,base=Base}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewBase = ra_temp(Base, Map),
  I#store{src=NewSrc,base=NewBase}.

ra_storex(I=#storex{src=Src,base1=Base1,base2=Base2}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewBase1 = ra_temp(Base1, Map),
  NewBase2 = ra_temp(Base2, Map),
  I#storex{src=NewSrc,base1=NewBase1,base2=NewBase2}.

ra_unary(I=#unary{dst=Dst,src=Src}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc = ra_temp(Src, Map),
  I#unary{dst=NewDst,src=NewSrc}.

ra_lfd(I=#lfd{dst=Dst,base=Base}, Map, FPMap) ->
  NewDst = ra_temp_fp(Dst, FPMap),
  NewBase = ra_temp(Base, Map),
  I#lfd{dst=NewDst,base=NewBase}.

ra_lfdx(I=#lfdx{dst=Dst,base1=Base1,base2=Base2}, Map, FPMap) ->
  NewDst = ra_temp_fp(Dst, FPMap),
  NewBase1 = ra_temp(Base1, Map),
  NewBase2 = ra_temp(Base2, Map),
  I#lfdx{dst=NewDst,base1=NewBase1,base2=NewBase2}.

ra_stfd(I=#stfd{src=Src,base=Base}, Map, FPMap) ->
  NewSrc = ra_temp_fp(Src, FPMap),
  NewBase = ra_temp(Base, Map),
  I#stfd{src=NewSrc,base=NewBase}.

ra_stfdx(I=#stfdx{src=Src,base1=Base1,base2=Base2}, Map, FPMap) ->
  NewSrc = ra_temp_fp(Src, FPMap),
  NewBase1 = ra_temp(Base1, Map),
  NewBase2 = ra_temp(Base2, Map),
  I#stfdx{src=NewSrc,base1=NewBase1,base2=NewBase2}.

ra_fp_binary(I=#fp_binary{dst=Dst,src1=Src1,src2=Src2}, FPMap) ->
  NewDst = ra_temp_fp(Dst, FPMap),
  NewSrc1 = ra_temp_fp(Src1, FPMap),
  NewSrc2 = ra_temp_fp(Src2, FPMap),
  I#fp_binary{dst=NewDst,src1=NewSrc1,src2=NewSrc2}.

ra_fp_unary(I=#fp_unary{dst=Dst,src=Src}, FPMap) ->
  NewDst = ra_temp_fp(Dst, FPMap),
  NewSrc = ra_temp_fp(Src, FPMap),
  I#fp_unary{dst=NewDst,src=NewSrc}.

ra_pseudo_fmove(I=#pseudo_fmove{dst=Dst,src=Src}, FPMap) ->
  NewDst = ra_temp_fp(Dst, FPMap),
  NewSrc = ra_temp_fp(Src, FPMap),
  I#pseudo_fmove{dst=NewDst,src=NewSrc}.

ra_args([Arg|Args], Map) ->
  [ra_temp_or_imm(Arg, Map) | ra_args(Args, Map)];
ra_args([], _) ->
  [].

ra_temp_or_imm(Arg, Map) ->
  case hipe_ppc:is_temp(Arg) of
    true ->
      ra_temp(Arg, Map);
    false ->
      Arg
  end.

ra_temp_fp(Temp, FPMap) ->
  Reg = hipe_ppc:temp_reg(Temp),
  case hipe_ppc:temp_type(Temp) of
    'double' ->
      case hipe_ppc_registers:is_precoloured_fpr(Reg) of
	true -> Temp;
	_ -> ra_temp_common(Reg, Temp, FPMap)
      end
  end.

ra_temp(Temp, Map) ->
  Reg = hipe_ppc:temp_reg(Temp),
  case hipe_ppc:temp_type(Temp) of
    'double' ->
      exit({?MODULE,ra_temp,Temp});
    _ ->
      case hipe_ppc_registers:is_precoloured_gpr(Reg) of
	true -> Temp;
	_ -> ra_temp_common(Reg, Temp, Map)
      end
  end.

ra_temp_common(Reg, Temp, Map) ->
  case gb_trees:lookup(Reg, Map) of
    {value,NewReg} -> Temp#ppc_temp{reg=NewReg};
    _ -> Temp
  end.

mk_ra_map(TempMap, SpillLimit) ->
  %% Build a partial map from pseudo to reg or spill.
  %% Spills are represented as pseudos with indices above SpillLimit.
  %% (I'd prefer to use negative indices, but that breaks
  %% hipe_ppc_registers:is_precoloured/1.)
  %% The frame mapping proper is unchanged, since spills look just like
  %% ordinary (un-allocated) pseudos.
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit, is_precoloured_gpr),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      TempMap).

conv_ra_maplet(MapLet = {From,To}, SpillLimit, IsPrecoloured) ->
  %% From should be a pseudo, or a hard reg mapped to itself.
  if is_integer(From), From =< SpillLimit ->
      case hipe_ppc_registers:IsPrecoloured(From) of
	false -> [];
	_ ->
	  case To of
	    {reg, From} -> [];
	    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
	  end
      end;
     true -> exit({?MODULE,conv_ra_maplet,MapLet})
  end,
  %% end of From check
  case To of
    {reg, NewReg} ->
      %% NewReg should be a hard reg, or a pseudo mapped
      %% to itself (formals are handled this way).
      if is_integer(NewReg) ->
	  case hipe_ppc_registers:IsPrecoloured(NewReg) of
	    true -> [];
	    _ -> if From =:= NewReg -> [];
		    true ->
		     exit({?MODULE,conv_ra_maplet,MapLet})
		 end
	  end;
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of NewReg check
      {From, NewReg};
    {spill, SpillIndex} ->
      %% SpillIndex should be >= 0.
      if is_integer(SpillIndex), SpillIndex >= 0 -> [];
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of SpillIndex check
      ToTempNum = SpillLimit+SpillIndex+1,
      MaxTempNum = hipe_gensym:get_var(ppc),
      if MaxTempNum >= ToTempNum -> ok;
	 true -> hipe_gensym:set_var(ppc, ToTempNum)
      end,
      {From, ToTempNum};
    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
  end.

mk_ra_map_fp(FPMap, SpillLimit) ->
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit,
					     is_precoloured_fpr),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      FPMap).
