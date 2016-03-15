%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(hipe_arm_ra_finalise).
-export([finalise/3]).
-include("hipe_arm.hrl").

finalise(Defun, TempMap, _FPMap0=[]) ->
  Code = hipe_arm:defun_code(Defun),
  {_, SpillLimit} = hipe_arm:defun_var_range(Defun),
  Map = mk_ra_map(TempMap, SpillLimit),
  NewCode = ra_code(Code, Map, []),
  Defun#defun{code=NewCode}.

ra_code([I|Insns], Map, Accum) ->
  ra_code(Insns, Map, [ra_insn(I, Map) | Accum]);
ra_code([], _Map, Accum) ->
  lists:reverse(Accum).

ra_insn(I, Map) ->
  case I of
    #alu{} -> ra_alu(I, Map);
    #cmp{} -> ra_cmp(I, Map);
    #load{} -> ra_load(I, Map);
    #ldrsb{} -> ra_ldrsb(I, Map);
    #move{} -> ra_move(I, Map);
    #pseudo_call{} -> ra_pseudo_call(I, Map);
    #pseudo_li{} -> ra_pseudo_li(I, Map);
    #pseudo_move{} -> ra_pseudo_move(I, Map);
    #pseudo_switch{} -> ra_pseudo_switch(I, Map);
    #pseudo_tailcall{} -> ra_pseudo_tailcall(I, Map);
    #smull{} -> ra_smull(I, Map);
    #store{} -> ra_store(I, Map);
    _ -> I
  end.

ra_alu(I=#alu{dst=Dst,src=Src,am1=Am1}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc = ra_temp(Src, Map),
  NewAm1 = ra_am1(Am1, Map),
  I#alu{dst=NewDst,src=NewSrc,am1=NewAm1}.

ra_cmp(I=#cmp{src=Src,am1=Am1}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewAm1 = ra_am1(Am1, Map),
  I#cmp{src=NewSrc,am1=NewAm1}.

ra_load(I=#load{dst=Dst,am2=Am2}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewAm2 = ra_am2(Am2, Map),
  I#load{dst=NewDst,am2=NewAm2}.

ra_ldrsb(I=#ldrsb{dst=Dst,am3=Am3}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewAm3 = ra_am3(Am3, Map),
  I#ldrsb{dst=NewDst,am3=NewAm3}.

ra_move(I=#move{dst=Dst,am1=Am1}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewAm1 = ra_am1(Am1, Map),
  I#move{dst=NewDst,am1=NewAm1}.

ra_pseudo_call(I=#pseudo_call{funv=FunV}, Map) ->
  NewFunV = ra_funv(FunV, Map),
  I#pseudo_call{funv=NewFunV}.

ra_pseudo_li(I=#pseudo_li{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#pseudo_li{dst=NewDst}.

ra_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc = ra_temp(Src, Map),
  I#pseudo_move{dst=NewDst,src=NewSrc}.

ra_pseudo_switch(I=#pseudo_switch{jtab=JTab,index=Index}, Map) ->
  NewJTab = ra_temp(JTab, Map),
  NewIndex = ra_temp(Index, Map),
  I#pseudo_switch{jtab=NewJTab,index=NewIndex}.

ra_pseudo_tailcall(I=#pseudo_tailcall{funv=FunV,stkargs=StkArgs}, Map) ->
  NewFunV = ra_funv(FunV, Map),
  NewStkArgs = ra_args(StkArgs, Map),
  I#pseudo_tailcall{funv=NewFunV,stkargs=NewStkArgs}.

ra_smull(I=#smull{dstlo=DstLo,dsthi=DstHi,src1=Src1,src2=Src2}, Map) ->
  NewDstLo = ra_temp(DstLo, Map),
  NewDstHi = ra_temp(DstHi, Map),
  NewSrc1 = ra_temp(Src1, Map),
  NewSrc2 = ra_temp(Src2, Map),
  I#smull{dstlo=NewDstLo,dsthi=NewDstHi,src1=NewSrc1,src2=NewSrc2}.

ra_store(I=#store{src=Src,am2=Am2}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewAm2 = ra_am2(Am2, Map),
  I#store{src=NewSrc,am2=NewAm2}.

%%% Tailcall stack arguments.

ra_args([Arg|Args], Map) ->
  [ra_temp_or_imm(Arg, Map) | ra_args(Args, Map)];
ra_args([], _) ->
  [].

ra_temp_or_imm(Arg, Map) ->
  case hipe_arm:is_temp(Arg) of
    true ->
      ra_temp(Arg, Map);
    false ->
      Arg
  end.

%%% FunV, Am, and Temp operands.

ra_funv(FunV, Map) ->
  case FunV of
    #arm_temp{} -> ra_temp(FunV, Map);
    _ -> FunV
  end.

ra_am1(Am1, Map) ->
  case Am1 of
    #arm_temp{} ->
      ra_temp(Am1, Map);
    {Src2,rrx} ->
      NewSrc2 = ra_temp(Src2, Map),
      {NewSrc2,rrx};
    {Src2,ShiftOp,ShiftArg} ->
      NewSrc2 = ra_temp(Src2, Map),
      NewArg =
	case ShiftArg of
	  #arm_temp{} -> ra_temp(ShiftArg, Map);
	  _ -> ShiftArg
	end,
      {NewSrc2,ShiftOp,NewArg};
    _ ->
      Am1
  end.

ra_am2(Am2=#am2{src=Src2,offset=Offset}, Map) ->
  NewSrc2 = ra_temp(Src2, Map),
  NewOffset = ra_am2offset(Offset, Map),
  Am2#am2{src=NewSrc2,offset=NewOffset}.

ra_am2offset(Offset, Map) ->
  case Offset of
    #arm_temp{} ->
      ra_temp(Offset, Map);
    {Src3,rrx} ->
      NewSrc3 = ra_temp(Src3, Map),
      {NewSrc3,rrx};
    {Src3,ShiftOp,Imm5} ->
      NewSrc3 = ra_temp(Src3, Map),
      {NewSrc3,ShiftOp,Imm5};
    _ ->
      Offset
  end.

ra_am3(Am3=#am3{src=Src2,offset=Offset}, Map) ->
  NewSrc2 = ra_temp(Src2, Map),
  NewOffset = ra_am3offset(Offset, Map),
  Am3#am3{src=NewSrc2,offset=NewOffset}.

ra_am3offset(Offset, Map) ->
  case Offset of
    #arm_temp{} -> ra_temp(Offset, Map);
    _ -> Offset
  end.

-ifdef(notdef). % for FP regalloc
ra_temp_fp(Temp, FPMap) ->
  Reg = hipe_arm:temp_reg(Temp),
  case hipe_arm:temp_type(Temp) of
    'double' ->
      case hipe_arm_registers:is_precoloured_fpr(Reg) of
	true -> Temp;
	_ -> ra_temp_common(Reg, Temp, FPMap)
      end
  end.
-endif.

ra_temp(Temp, Map) ->
  Reg = hipe_arm:temp_reg(Temp),
  case hipe_arm:temp_type(Temp) of
    'double' ->
      exit({?MODULE,ra_temp,Temp});
    _ ->
      case hipe_arm_registers:is_precoloured_gpr(Reg) of
	true -> Temp;
	_ -> ra_temp_common(Reg, Temp, Map)
      end
  end.

ra_temp_common(Reg, Temp, Map) ->
  case gb_trees:lookup(Reg, Map) of
    {value,NewReg} -> Temp#arm_temp{reg=NewReg};
    _ -> Temp
  end.

mk_ra_map(TempMap, SpillLimit) ->
  %% Build a partial map from pseudo to reg or spill.
  %% Spills are represented as pseudos with indices above SpillLimit.
  %% (I'd prefer to use negative indices, but that breaks
  %% hipe_arm_registers:is_precoloured/1.)
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
      case hipe_arm_registers:IsPrecoloured(From) of
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
	  case hipe_arm_registers:IsPrecoloured(NewReg) of
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
      MaxTempNum = hipe_gensym:get_var(arm),
      if MaxTempNum >= ToTempNum -> ok;
	 true -> hipe_gensym:set_var(arm, ToTempNum)
      end,
      {From, ToTempNum};
    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
  end.

-ifdef(notdef). % for FP regalloc
mk_ra_map_fp(FPMap, SpillLimit) ->
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit,
					     is_precoloured_fpr),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      FPMap).
-endif.
